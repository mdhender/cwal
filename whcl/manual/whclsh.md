# whcl: whclsh
##### ([&#x2b11;Table of Contents](./))
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

whclsh is the canonical whcl shell application for running whcl code
from batch scripts or an interactive REPL. It acts as client to the
whcl library interface and is not part of the library.

Some docs are still TODO.

Future topics include, but are not limited to:

- [Building whclsh](#building)
- [Invoking the Shell](#invoking)
  - [Environment Variables](#envvar) which influence it
  - [Script Access to CLI Flags](#whcl-argv)
- [APIs installed by the shell](#installed-apis)
- [Extending the shell](#extending) (without modifying it)
- [WHCL_HOME](#home)

Intro
============================================================

`whclsh` is a client application of libwhcl which serves several purposes:

- It is the primary interface for testing whcl itself via:
  - A conventional REPL ("Read, Eval, Print Loop") for interactive
  use.
  - The ability to run script files.
- It can act as a bootstrapping basis for implementing client
  applications.
- It can plug in client-provided whcl features without modifying it.


<a id='building'></a>
Building whclsh
------------------------------------------------------------

In the canonical build tree, whclsh is built as part of the normal
whcl build process.

However, the preferred distribution of whcl is [the amalgamation
build][amal], a small set of standalone C source
and header files.

In short, building the shell requires only:

```console
$ cc -c -I. cliapp.c    # utility code required by...
$ cc -c -I. whclsh.c    # the main shell app
$ cc -c -I. libwhcl.c   # the libwhcl amalgamation build
$ cc -o whclsh cliapp.o whclsh.o libwhcl.o
```

That's all there is to it, but that will create a build with no
interactive editing support, so it's not helpful for interactive
use. For interactive mode, the REPL part can use either:

- GNU Readline
- [Linenoise](https://github.com/msteveb/linenoise) (specifically,
  it's tested with Steve Bennett's linenoise fork)

To add one of those, define _either_ `CLIAPP_ENABLE_READLINE=1` _or_
`CLIAPP_ENABLE_LINENOISE=1` when compiling `cliapp.c` and link to the
appropriate libraries: `-lreadline` or `-llinenoise`, noting that
readline also requires `-lncurses` on many, if not most,
platforms. Like [the amalgamation build][amal], `cliapp.c` responds to
the `HAVE_CONFIG_H` compilation macro and will, if it's set to any
value, include `"config.h"`. Thus the line-editing macros may
optionally be set that way.
  
(Patches to add support for other interactive editing APIs are
welcomed!)

With one of those in place, the shell will start up in interactive
mode and the platform's EOF character, entered on an empty line, will
exit the shell. (On Unix systems that's ctrl-D.)


<a id='invoking'></a>
Invoking the Shell
============================================================

`whclsh -?` or `--help` provide a complete list of options for the
shell, but the more useful/common ones are listed here:

- The first non-flag argument is assumed to be a script file. The
  input file may also be specified with `-f|--file infile`.
- All flags after the first `--` flag are registered as script-side
  flags, [as described below](#whcl-argv) and do not influence the
  shell directly.
- `-e|--eval 'SCRIPT CODE'` scripts are run in the order provided, after all
  flags have been processed.
- `-v`/`--verbose` causes the shell to show the result of each line of
  input in some geeky form which includes the value's reference count,
  data type, and owning scope.
- All output generated via the cwal-level output APIs, which includes
  all script-generated output, can be redirected to a file with
  `-o|--output filename`.



<a id='envvar'></a>
Environment Variables which Influence whclsh
------------------------------------------------------------

- `WHCLSH_INIT_SCRIPT`: if set, it is assumed to be the path to a whcl
  script. It gets run after the core infrastructure is set up but a
  before any `-e` scripts are run. If it fails, initialization of the
  shell will also fail, but the `-I`/`--no-init-script` flags can be
  used to suppress its loading, as can unsetting that environment
  variable or setting it to an empty value. This file is _not_ loaded
  in "cleanroom" mode.
- `WHCLSH_HISTORY_FILE`: if set, it is assumed to be a history file
  suitable for use as the shell's line-editing history. Errors loading
  or saving this file are silently ignored.


<a id='whcl-argv'></a>
whcl[ARGV]
------------------------------------------------------------

`whcl[ARGV]` is an array holding all flags passed to whclsh _after_ a
`--` flag.  Such flags are intended for use only by scripts. The array
holds all arguments in unparsed form.

Its `flags` property is an object which holds boolean-style flags:
`-a`, `--b`, etc. It treats a single leading dash and two leading
dashes identically and strips them from the flag property's name.
Flags which have a value are required to be a single token with a `=`
between the flag and its value. The token may be quoted. Note that if
the same flag is given more than once, the last one wins, so if a
script requires such a feature it will need to process the list
directly from the `ARGV` object. Also note that property storage is
unordered, so the order of the properties in the `flags` object is
effectively arbitrary.

Its `nonFlags` property holds a list of all non-flag arguments, in the
order they are provided.

For example:

First with no flags:

```whcl
$ whclsh -e 'echo "ARGV:" [whcl.ARGV to-json 2]' \
  -e 'echo "Flags:" whcl.ARGV.flags' \
  -e 'echo "Non-flags:" whcl.ARGV.nonFlags'
ARGV: []
Flags: {}
Non-flags: []
```

And with flags:

```whcl
$ whclsh -e 'echo "ARGV:" [whcl.ARGV to-json 2]' \
  -e 'echo "Flags:" whcl.ARGV.flags.' \
  -e 'echo "Non-flags:" whcl.ARGV.nonFlags' \
 -- -a -b=c --c=d e f g --h --i="Hi there"
ARGV: [
  "-a",
  "-b=c",
  "--c=d",
  "e",
  "f",
  "g",
  "--h",
  "--i=Hi there"
]
Flags: {"c": "d", "b": "c", "a": true, "i": "Hi there", "h": true}
Non-flags: ["e", "f", "g"]
```


<a id='installed-apis'></a>
Installed APIs
============================================================

whclsh installs all of the APIs found in the [API index](api-index.md).
When running in interactive mode _or_ when passed the `--whcl.shell`
flag, it adds an object named `whcl[shell]` with the following APIs:

- **`exit`**\  
  Tells interactive mode to exit. This is different from the `exit`
  builtin command, in that that one does not end interactive mode and
  this one has no effect on non-interactive mode.
- **`history-add`** `string`\  
  Adds the given string to the interactive editing history.
- **`history-load`** `filename`\  
  Loads the interactive editing history from the given file.
- **`history-save`** `filename`\  
  Saves the interactive editing history to the given file.
- **`readline`** `[promptString]`\  
  Reads a single line of input and returns it, or returns `undefined`
  if the line was aborted with the EOF sequence (ctrl-D on most
  systems).
- **`tokenize-line`** `string`\  
  Breaks the given string into an array of tokens, supporting only the
  most basic of whcl tokens: numeric literals, quoted strings, and the
  built-in constants `true`, `false`, `null`, and `undefined`.  All
  other tokens are treated as strings. Returns `undefined` if the line
  has no tokens, else returns a non-empty array. Throws for errors
  such as unclosed quoted strings. The intent of this function is to
  facilitate dispatching of interactive input.

<a id='extending'></a>
Extending whclsh (Without Modifying It)
============================================================

The file `whclsh_extend.c` exists as a way for clients to add
functionality to whclsh without having to edit the shell
application. To build this option in, add the macro `WHCLSH_EXTEND`
when compiling `whclsh.c` and then link `whclsh_extend.o` to the
resulting application. The source tree comes with a commented copy of
that file demonstrating and explaining how to plug in new features.

As the library matures, loadable module support will also be added
(ported over from s2), allowing C-level features to be built as DLLs
and loaded at runtime.

[amal]: build.md#amalgamation


<a id='home'></a>
There's no Place Like WHCL_HOME
============================================================

This section covers a convention for how one developer installs
whcl. This convention is in no way "official" nor baked into the
language. It's just proven to be convenient over the years with whcl's
sibling languages.

In short, it's proven helpful to define an environment variable,
`WHCL_HOME`, which points to the whcl "home" directory - where all of
its utility code, loadable modules, etc., are kept.

Assuming `~/bin` is in one's path, a wrapper script such as the
following should be placed there and named `whclsh`:

```bash
#!/bin/sh
# whclsh wrapper script. Configure it for the local
# system and drop it in your PATH somewhere...
: ${WHCL_HOME:="$HOME/whcl"}
: ${WHCLSH_INIT_SCRIPT:="${WHCL_HOME}/whclsh.whcl"}
: ${WHCLSH_HISTORY_FILE:="${WHCL_HOME}/whclsh.history"}
: ${WHCL_IMPORT_PATH:=".:${WHCL_HOME}"}
: ${WHCL_MODULE_PATH:=".:${WHCL_HOME}/mod"}
: ${WHCL_REQUIRE_PATH:="."}
for rp in "${PWD}/require.d" "${WHCL_HOME}/require.d"; do
    if [ -d "${rp}" ]; then
        WHCL_REQUIRE_PATH="${WHCL_REQUIRE_PATH}:${rp}"
    fi
done
export WHCL_HOME WHCL_IMPORT_PATH \
       WHCL_MODULE_PATH WHCL_REQUIRE_PATH \
       WHCLSH_INIT_SCRIPT WHCLSH_HISTORY_FILE
exec "${WHCL_HOME}/whclsh" "$@"
```

The real whclsh binary itself is then copied to `$WHCL_HOME/whclsh`.
