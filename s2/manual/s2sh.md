# s2: s2sh - The s2 Shell
#### ([&#x2b11;Table of Contents](./))
# s2sh: The s2 Shell

See also: [s2sh2](s2sh2.md)

Jump to...

* [s2sh: The s2 Shell](#s2sh)
    * [Shell CLI Flag Conventions](#s2sh-cli-flags-shell)
    * [Script-side CLI Flags](#s2sh-cli-flags-script)
    * [Startup Wrapper Script](#s2sh-wrapper-script)
* [Global Objects & Functions](#s2sh-globals)
    * [The s2 Object](#s2sh-s2-object)
    * [Installed s2 APIs](#s2sh-installed-apis)
    * [`s2.shell` API](#s2sh-shell-api)
* [Extending s2sh](#s2sh-extend)
    * [Extending with Script Code](#s2sh-extend-with-script)
    * [Extending with Native Code](#s2sh-extend-with-native)
* [Sample Scripts](#s2sh-sample-scripts)
* [Adjusting Sweep and Vacuum Intervals](#s2sh-adjust-sweep)
* [TODOs](#s2sh-todo)

See also:

* [`S2_HOME`](s2-home.md)
* Building a static s2sh [with Docker](../../docker/README.md).

<a id="s2sh"></a>
# s2sh: The s2 Shell

s2 is developed as a library, but its primary intended usage is to be
driven from a shell-like application in scripting other libraries. The
primary application, used in driving s2's own unit tests, is the "s2
shell," a.k.a. `s2sh`.

s2sh runs s2 scripts from files, `stdin`, or (if enabled at build time) an
interactive terminal session. Run it with the `-?` or `--help` flag to see
the list of options (which may vary from commit to commit!).

The shell installs some amount of basic functionality, including all of
the prototype-level infrastructure. The following subsections describe
shell-specific features it installs, as well as how to extend it, both
from script code and C code.

s2sh sees quite a bit of use, relatively speaking. All but one of s2's
unit test scripts is driven by it, and its interactive mode is a
"must-have" during experimentation with new s2 features, creating code
snippets for these docs, and whatnot. Sometimes i start it up simply for
use as a calculator:

```bash
[stephan@host:~/cwal/s2]$ ./s2sh -v
...
s2sh> 3 + 7 * 10
result: integer@0x2628240[scope=#1@0x7ffda03cf6d0 ref#=0] ==> 73
```

<a id="s2sh-cli-flags-shell"></a>
## Shell-side CLI Flags

s2sh's handling of CLI flags has, as the Germans say, "grown
hysterically," which, in German, is a pun, on "grown historically,"
which means (in German) something like "evolved
organically/chaotically."

This section does not document the many CLI flags: run the shell
with the `-?` or `--help` flags to see the docs. It does, however,
explain the conventions:

* Flags starting with a single dash turn a feature on.
* Flags starting with two dashes turn a feature off.

If a flag is provided multiple times, the last one wins except in
rare cases where a flag increases or decreases a setting's value rather
than toggling it on or off.

A couple of examples:

* `-R` explicitly enables the Value-level recycler, whereas `--R` explicitly
  disables it.
* `-S` explicitly enables string interning, whereas `--S` explicitly
  disables it.

Flags which expect a value are in the form `-flag VALUE`, with no
equal sign between the two. This is in unfortunate contrast to
script-side flags (detailed below), but, as said above, "grown
hysterically," and many scripts and makefiles depend on these
hysterical conventions.

<a id="s2sh-cli-flags-script"></a>
## Script-side CLI Flags

When processing its CLI flags, the `--` flag tells s2sh to stop
processing flags and pass all arguments after `--` to the cwal-level
flag processor, which processes flags for script-side use, which s2sh
then exposes to script-space.

The `s2.ARGV` Array contains all flags passed to the shell after the `--` flag,
and has the following properties:

- `flags`: an Object with no prototype containing any flags passed to the script (see below for the syntax).
- `nonFlags`: an array of all passed-in non-flag values (those
  starting with neither `-` nor `+`).

Because the flags parsing happens at a deeper level in the API, and because
the parser cannot know whether any given flag requires a value or not,
the flag syntax varies from the s2sh-side flags:

- Any number of leading dashes are equivalent: `-a`, `--a`, `-----a`, etc.
- If the same flag is provided multiple times, each subsequent entry
  overwrites the previous one. i.e. the last one wins.
- A flag with a value is in the form `-flag=value`. The parser attempts to
  convert the value to one of:
    - Numeric, if it can be parsed in decimal format (hex, octal, and
      binary format are *not* supported here because this parsing happens
      at a lower level than those features (that may be changed someday,
      though)).
    - Boolean for the values `"true"` and `"false"`. A flag with no value
      is treated as a boolean with a true value.
    - `null` for the value `"null"`.
    - `undefined` for the value `"undefined"`.
    - A string for anything else.
- A flag beginning with `+` is in "inverted boolean", so `+foo` sets
`s2.ARGV.flags.foo` to `false`. If a value is passed such a flag, that
value is interpreted as described for `-`, otherwise it is treated as
the opposite of the same flag name starting with a `-`. (It "should"
error out if passed a value, but that particular code is intentionally
lax in what it accepts.)

Note that s2sh historically only defines `s2.ARGV` and its
`flags`/`nonFlags` members if at least one flag or argument is
provided. The historic reasoning behind that was so that scripts could
easily check whether any flags were provided or not, but practice has
proven that thinking to be flawed, at least from a usability point of
view, and it leads to most scripts including snippets like:

```s2
const cliFlags = (s2.ARGV ? s2.ARGV.flags : 0) ||| {prototype:null};
const files = (s2.ARGV ? s2.ARGV.nonFlags : 0) ||| [];
```

As of 2020-02-06, `s2.ARGV`, `s2.ARGV.flags`, and `s2.ARGV.nonFlags`
are always set. As of 2020-02-07, `s2.ARGV` is itself an array, rather
than an object, and contains the unparsed values of all script-side
CLI arguments.

An example:

```bash
$ s2sh -v -- --foo=hi -bar +baz a b c
...
s2sh> s2.ARGV
result: array@0x556cab209040[scope=#1 ref#=1] ==> [
  "--foo=hi",
  "+bar",
  "-baz",
  "a",
  "b",
  "c"
]
s2sh> s2.ARGV.flags
result: object@0x55e6d2146c00[scope=#1 ref#=1] ==> {
  "bar": true,
  "baz": false,
  "foo": "hi"
}
s2sh> s2.ARGV.nonFlags
result: array@0x55e6d2146c50[scope=#1 ref#=1] ==> [
  "a",
  "b",
  "c"
]
```

<a id="s2sh-wrapper-script"></a>
# Startup Wrapper Script

When installing s2sh locally (by copying the binary to one's [desired
installation location](s2-home.md)), be sure to take a look at the file
[s2sh.sh](/finfo/s2/s2sh.sh). It is intended to be installed into the
`$PATH` without the `.sh` extension, and to be edited to point to the
"real" s2sh binary (which can live anywhere, even in the same
directory as the wrapper script, but with a different extension, e.g.
the script could be named `s2sh` and the binary `s2sh.bin`). The
script's purpose is to set up various environment variables and/or
s2sh flags which may be globally useful. e.g. the [environment
variable `S2_HOME`](s2-home.md) is commonly used by scripts, and it
could be set up in that wrapper script. s2sh supports automatically
loading an "init script" when it starts up, which can be used to
automatically install new APIs or load new modules, as described
[later on in this document](#s2sh-extend-with-script).

    
<a id="s2sh-globals"></a>
# Global Objects & Functions

s2sh installs the following global symbols:

```s2-member
Function print(...)
```

A convenience reference to `s2.io.print()` (described [on the io API page](misc-io.md)). This is
not a `const`, by the way, and clients are free to overwrite it with
their own implementation. It returns itself, so calls may be chained.

```s2-member
s2
```

This object acts as the top-level namespace for all features provided
by the s2 library which s2sh plugs in.

> Sidebar: This symbol *should* have been made a language-level
*keyword* which refered to a client-defined global-scope value
instance, as opposed to being a global-scope *value* instance, but
changing that now would break probably 90% or more of my hundreds of
scripts. Oh, well.


<a id="s2sh-s2-object"></a>
## The s2 Object

The `const` global `s2` object acts as a namespace for most of the
s2sh-installed functionality. Aside from the `print()` function, which
is provided primarily because it's damned convenient to debug scripts
with (in particular with the `__FLC` keyword)[^1], the s2 object is
the only global symbol exposed by s2sh. Because client scripts
(almost) never run in the global scope, they have no way of injecting
other truly global values. For that reason, s2sh reserves the property
`s2.client` specifically for client use in storing data which needs to
be used across multiple scripts, but which has no better place to be
stored. s2sh will never set nor use the `s2.client` property.

The s2 object contains the following members, none of which require that
*s2* be their `this` object (so they can be copied for use in other
contexts):

```s2-member
integer compare(Value lhs,Value rhs [, bool typeStrict=false])
integer compare(Value rhs)
```

Compares the given values using cwal's built-in comparisons and
returns 0 if the values are equivalent (or the same instance), a
negative value if `lhs` is "less than" `rhs`, and a positive value if
`lhs` is "greater than" `rhs`. If passed only 1 argument, it uses the
current `this` as the `lhs` and the 1st argument as the `rhs`, but
that form cannot use the type-strict flag. In the 3-arg form, the 3rd
parameter controls whether type-strict comparisons are used
(equivalent to the `==` resp. `===` operators).

```s2-member
object cwalBuildInfo()
```

Returns an object which describes a variety of build-time
configuration options for libcwal (the basis off of which s2 is
built). e.g. it holds the compiler flags and "bitness" of the script
engine.

```s2-member
void dumpMetrics()
```

Dumps a great deal of cwal- and s2-level metrics to stdout and/or the
engine's defined output channel. Might output more if the -v (verbose)
s2sh flag is used once or twice.

```s2-member
void dumpVal(...)
```

A debugging-only function: runs each value through `s2_dump_val()`,
outputting the result to `stdout`[^50].

```s2-member
string|undefined getenv(string key)
```

Works like the standard C function of the same name. Returns
`undefined` if no such environment variable (case-sensitive) is
found.

```s2-member
mixed fork([bool returnFromChild=false ,] Function)
```

Only works on Unix builds (throws an exception if it is disabled).
This `fork(2)`'s the current s2sh process. The parent process gets the
child process's ID (an integer) as the return value of `fork()`.  By
default the child process exits after forking (as if s2's `exit`
keyword had been used, as opposed to C's `exit()`). If passed more
than one argument, the first is treated as a boolean, the value of
which determines whether the child process exits (pass in a falsy
value) or returns (pass in a truthy value). The second (or only)
argument is a callback function implementing any work for the child
process. If a child process returns (as opposed to using `exit`,
`fatal`, or similar), it returns to its own process, not the parent
process, and the `fork()` return value will be the value returned from
the callback function.

```s2-member
string getenv(string key)
```

Uses `getenv(3)` to search for an environment variable. Returns its
value as a string if it finds one, else `undefined`.

```s2-member
bool glob( string needle, string haystack [, int globStyle=-1] )
```

Returns `true` if the glob pattern `needle` matches `haystack`, else
returns `false`. The `globStyle` determines the type of pattern
matching used: &lt;0 = conventional Unix-like patterns (case
sensitive, the default), 0 = SQL LIKE (case insensitive), &gt;0 = SQL
LIKE (case sensitive). Throws if passed non-strings for the first two
arguments (with the note that, like most string-wielding APIs, it
treats Buffers like strings). The complete rules for the glob types
can be found in the C API docs of `s2_glob_matches_str()` [in
s2.h](/finfo/s2/s2.h).

```s2-member-deprecated
mixed import(string filename)
```

As of 20200118, this function is deprecated in favor of [the `import`
keyword](keyword-import.md). It behaves behaves exactly like that keyword
except that it does not do any path-searching for the filenames passed to it.
To make [the `import` keyword](keyword-import.md) behave equivalently to
the historical behaviour of this function, pass `false` as the first
argument to the keyword to suppress its path-search feature.


```s2-member
mixed loadModule(dllFileName[, symbolName])
mixed loadModule(dllFileName)
```

As described in [the loadable module docs](../mod/#mod-loadmodule).


```s2-member
Buffer minifyScript(string|Buffer scriptContent)
```

"Minifies" s2 script code by removing comments and most extraneous
space.  This is not yet known to be 100% reliable - use at your own
risk. That said, several of the [s2 module builds](../mod/) use it and
it "seems to work," even with the relatively complex [require.s2
module](../mod/require/).


```s2-member
mixed sealObject(...)
```

Requires that each argument be a container-type value. Flags are set
on each argument which "seals" it against future changes: assigning to
its properties, or attempting to add new ones, will trigger an
exception. It returns the last value passed to it.


<a id="s2sh-installed-apis"></a>
## Pre-installed s2 Modules/APIs

Aside from the functionality described above, s2sh installs
the following API modules by default (noting that this list
may be modified by [the build process](prologue-intro.md#download-and-build)):

* `s2.fs`: [Filesystem API](misc-fs.md)
* `s2.io`: [I/O API](misc-io.md)
* `s2.json`: [JSON API](misc-json.md)
* `s2.ob`: [Output Buffering API](misc-ob.md)
* `s2.time`: [Time API](misc-time.md)
* `s2.tmpl()`: [Text Templates API](misc-tmpl.md)


<a id="s2sh-shell-api"></a>
## `s2.shell` API

The `s2.shell` object *is only installed when running in interactive
mode* (which works only if s2sh is built with interactive editing
support) or when passed the `-s2.shell` flag (with a *single* dash).
It providing access to some of the shell's interactive features:

```s2-member
void exit()
```

Exits the interactive shell mode, similarly to the EOF sequence,
except that the exit is (A) delayed until after the complete
expression which includes this call completes and (B) does not accept
an result value like the `exit` keyword does. The `exit` keyword,
however, does not end interactive mode, which trumps the `exit`,
`fatal`, a failed `assert`, and similar, and remains in interactive
mode.


```s2-member
void historyAdd(string)
```

Adds a line to the interactive editing history. Make sure the line
contains *no* newlines, or it will be stored as multiple history
commands!

```s2-member
void historyLoad(fileaname)
void historySave(filename)
```
Load resp. save the shell command editing history.

```s2-member
string readLine([string prompt])
```

Reads a line of input from the console using whatever input mechanism
it is configured for, without a trailing newline. Returns `undefined`
if the user taps the EOF sequence (Ctrl-D on most Unix terminals,
Ctrl-Z(?) on Windows) or an empty string if the user simply taps
`<ENTER>`. The line is not automatically inserted into the history: use
`historyAdd()` for that.

Typical usage:

```s2
const x = s2.shell.readLine();
if(undefined===x) { ... leave this block/loop/function ... }
else if(""===x) { ... user just tapped ENTER ... }
else {
  ... we got something ...
}
```


```s2-member
array tokenizeLine(string)
```


Takes an a string and tokenizes it using s2's core tokenizer, returning
an array of the individual tokens in their s2-appropriate data type.

Example:

```bash
s2sh> s2.shell.tokenizeLine("'hi there' 123 4.5 true null undefined")
result: array@0x56082eccfb20[scope=#1 ref#=0] ==> [
  "hi there",
  123,
  4.5,
  true,
  null,
  null
]
```

> Sidebar: that last element is output as `null`, rather than
`undefined`, because the JSON API translates list entries (but not
object properties) with the `undefined` value to `null`. Object
properties with that value are not emitted in JSON mode (because JSON
does not support the `undefined` value and that's how JavaScript's
JSON mode works).

<a id="s2sh-extend"></a>
# Extending s2sh

s2sh is designed to be easily extensible by client-side scripts or C
code, without having to modify the [core shell.c source
file](/finfo/s2/shell.c).


<a id="s2sh-extend-with-script"></a>
## Extending s2sh with Script Code

s2sh supports auto-loading an initialization script at startup. It has
two options:

1.  First, it looks for a file named by the environment variable
    `S2SH_INIT_SCRIPT`.
2.  Secondly, it looks for a file with the same directory and base name
    as the s2sh binary, plus an ".s2" extension. That is: if your binary
    is named `/foo/bar/baz`, the autoload script should be named
    `/foo/bar/baz.s2`. Note that any ".exe" part of the name is ignored
    for this purpose, so an interpreter named *foo.exe* looks for a
    script named *foo.s2*, not *foo.exe.s2*.\  
    Bug: this 2nd approach does not canonicalize the binary's name
    (because doing so requires platform-specific code), so it does not
    work together with `$PATH` resolution (it won't find the init script,
    or will find it only incidentally).

See [s2sh.s2](/finfo/s2/s2sh.s2) for an example init script.

s2sh only uses the first one of those it finds. Not finding an init
script is not considered an error, but an error triggered by the init
script will abort the shell's startup.

Autoloading of the init script is enabled by default in the canonical
build, but may be disabled by default in custom builds. The `-a` flag
(one dash) explicitly enables autoloading and `--a` (two dashes)
explicitly disables it (which can be helpful if the file has an error
which casues s2sh startup to fail).

The init script can be used to perform arbitrary amounts of work,
e.g. loading commonly-used functions and [modules](../mod/), or
installing/extending client-written modules[^51]. Such a script may
use `s2.import()` (or equivalent) to include other files, and
[PathFinder](type-pathfinder.md) can be used to find those files.


The init script is *not* imported when s2sh is run with the
`-cleanroom` flag because in that mode the interpreter is extremely
bare-bones, with no public APIs installed (only in-language features
like keywords and operators).

<a id="s2sh-extend-with-native"></a>
## Extending s2sh with Native C/C++ Code

> What follows applies equally to `s2sh` and [`s2sh2`](s2sh2.md), the only
difference being that the former's main file is `shell.c` and the latter's
is `shell2.c`. In the instructions below, `shell2.c` may be substituted
for `shell.c`.

The absolute simplest way to add new script-visible functionality (from
C) to the shell is to use the accompanying `shell_extend.c` as a basis
and build it against s2's amalgamation build. The canonical source tree
is setup to do this already, and doing so in one's own projects requires
doing only the equivalent of:

-   Copy the following files from the s2 source distribution to your
    project:\  
    `libs2.{c,h}`, `shell.c`, `cliapp.c`, `shell_extend.c`, `shell_common.c`.\  
    Note that the "amalgamation" files are generated, not checked into
    the source tree. Run `make amal` from that source tree's `s2`
    directory to create them.
-   Edit `shell_extend.c` to include your functionality (it is commented
    quite heavily to help you along your way, includes several sample
    functions, and will compile as-is so you can get started without
    making any changes).
-   Compile `shell.c` with the `S2_SHELL_EXTEND` macro defined (any value,
    even an empty one).
    - Note that `shell_common.c` is *not* to be compiled by itself: it
    contains code common to `shell.c` and `shell2.c` (for
    [s2sh2](s2sh2.md)) and gets `#include`d directly by those files.
-   Compile `cliapp.c` (app-agnostic CLI-related utility code). No
    special flags are needed unless interactive line editing is
    desired (as demonstrated further down this page).
-   If using the amalgamation, compile `libs2.c`. It should
    need no special options, but you might want or need one or more of
    the following defines:
    -   `S2_ENABLE_MODULES=1` if your platform has loadable module
        support. That also requires *one* of:
    -   `S2_HAVE_DLOPEN=1` (for `dlopen()`) or `S2_HAVE_LTDLOPEN=1` (for
        `lt_dlopen()`) and link to the appropriate libraries (normally
        `-ldl` resp. `-lltdl`, but on BSD `dlopen()` is part of the
        system libraries amd required no extra linker flag).
-   Link the resulting objects to a binary using the name of your
    choice. Note that `shell.c` defines a `main()` routine, and
    `shell_extend.c` is only used by `shell.c` (if `S2_SHELL_EXTEND` is
    defined when `shell.c` is compiled), so neither should be linked to
    client libraries: only `libs2.*` (i.e. `libs2`) should be
    linked to clients. When building a custom (non-s2sh) application,
    clients may of course link `shell_extend.c` into their application
    and call its initialization routine themselves, just like s2sh does.

Example:

```bash
# From the s2 source directory...
#
# First, build the amalgamated sources, if needed:
#
$ make amal
#
# Then compile the parts we need:
#
$ cc -c cliapp.c
$ cc -c shell.c -DS2_SHELL_EXTEND
$ cc -c shell_extend.c
$ cc -c libs2.c
$ cc -o mysh shell.o cliapp.o shell_extend.o libs2.o
#
# Done! But... this shell is missing interactive mode:
#
$ ./mysh 
rc=305 (CWAL_RC_UNSUPPORTED): s2_engine says error #305 (CWAL_RC_UNSUPPORTED):
  Interactive mode not supported: this shell was built without line
  editing support.
shell.c:2032: rc=305 (CWAL_RC_UNSUPPORTED)
#
# Let's fix that...
```

> Pedantic sidebar: the canonical build compiles using the
highest-possible warning levels and treats *all* warnings in this
project's sources (as opposed to imported 3rd-party sources) as fatal
errors.

The above is all that's needed for the batch-mode shell. *Interactive*
shell support requires a line-editing library, and the sources currently
support two of them:

For *GNU Readline* support (with a viral GNU GPL license), do the
following:

-   Define `CLIAPP_ENABLE_READLINE=1` when compiling `cliapp.c` and make
    sure that the headers `<readline/readline.h>` and
    `<readline/history.h>` are resolvable (they are normally
    installed under `/usr/include` and need no special flags).
-   Link the resulting binary to libreadline (e.g. using `-lreadline` (and
    sometimes, depending on the platform, `-lncurses` as well)).

Alternately, use the semi-built-in
[linenoise](https://github.com/msteveb/linenoise) line editing
support, which is not as powerful as readline, but is enough for basic
work and has a user-friendly BSD license:

-   Build `linenoise/*.{c,h}` from the s2 source dir (linenoise requires
    C99). When using the amalgamation build in your source tree, it's
    easiest to just copy s2's linenoise subdirectory to the same
    directory as your copy of `shell.c`.
-   Build the (few) objects under the linenoise directory. See [the
    main s2 Makefile](/finfo/s2/Makefile) for compiler flags
    (possibly) needed to build it. It requires C99, by the way.
-   Define `CLIAPP_ENABLE_LINENOISE=1` when compiling `cliapp.c`, and
    make sure that it can resolve `"linenoise/linenoise.h"` via the
    includes path. e.g. copy the linenoise directory to wherever
    `cliapp.c` lives.

Alternately, patch `cliapp.c` for a different interactive interactive
line editing library and submit that back for inclusion in the main
tree :).

Here's a demo using `linenoise`:

```bash
# Build the linenoise object files:
#
$ CFLAGS="-std=c99 -DUSE_UTF8=1 -D_BSD_SOURCE -D_DEFAULT_SOURCE"
$ for i in linenoise/{linenoise,stringbuf,utf8}.c; do echo $i; cc $CFLAGS -c $i; done 
linenoise/linenoise.c
linenoise/stringbuf.c
linenoise/utf8.c
#
# Rebuild cliapp.c with linenoise support:
#
$ cc -c cliapp.c -DCLIAPP_ENABLE_LINENOISE=1
#
# Build the other files exactly as shown in the previous example:
#
$ cc -c shell.c -DS2_SHELL_EXTEND
$ cc -c shell_extend.c
$ cc -c libs2.c
#
# Make sure to add the linenoise object files when linking this time:
#
$ cc -o mysh shell.o shell_extend.o libs2.o linenoise/*.o
#
# Done!
#
$ ./mysh -v
s2 interactive shell. All commands run in the current scope. Use your platform's
EOF sequence on an empty line to exit. (Ctrl-D on Unix, Ctrl-Z(?) on Windows.)
Ctrl-C might work, too.

s2sh> 
```

(Tap Ctrl-D or type `s2.shell.exit()<ENTER>` to exit.)

That's all there is to it. The shell can use any functionality installed
by s2sh or via `shell_extend.c`. That file is intended to be a
boilerplate used by client code, which clients use to plugin to s2sh
when it starts up.

To upgrade the s2 core library, simply copy the following files from
the s2 source tree and (re)build your customized `shell_extend.c`
against those: `libs2.{c,h}`, `cliapp.c`, `shell.c` (or
`shell2.c`), and `shell_common.c` from the s2 source tree, . The cwal
APIs used by such extensions have been quite API-stable (only
additions, no major changes) since 2012 and has a client code base to
keep working. i.e. it is highly unlikely that upgrading your
amalgamation files will cause any breakage at the level of the cwal
API. *However*…  the s2-level API, on the other hand, is always
subject to change. *That said*... it's rare that client-side bindings
actually need access to the script language's (s2's) API - they tend
to work with the scripting *engine*'s (cwal's) API, which is fairly
stable.

Here's a sample s2sh session using functions from the
[default implementation of `shell_extend.c`](/finfo/s2/shell_extend.c):

```bash
# ./s2sh -v
…startup messages snipped…
s2sh> sampleCallback()
shell_extend.c:38: Hi, world!
result: undefined@0x6a424[scope=#0@(nil) ref#=0] ==> undefined
s2sh> sampleCallback2()
shell_extend.c:50: Hi again, world!
result: function@0x6f140[scope=#2@0xbee620e0 ref#=1] ==>
function@0x6f140
s2sh> sampleCallback2()()()
shell_extend.c:50: Hi again, world!
shell_extend.c:50: Hi again, world!
shell_extend.c:50: Hi again, world!
result: function@0x6f140[scope=#2@0xbee620e0 ref#=1] ==>
function@0x6f140
s2sh> sampleCallback3()
rc=105 (CWAL_RC_EXCEPTION)
EXCEPTION: exception@0x6f598[scope=#2@0xbee620e0 ref#=0] ==> {
 "code": 305,
 "column": 15,
 "line": 1,
 "message": "You have requested unsupported feature 'foo', so you are getting result code 'CWAL_RC_UNSUPPORTED'.",
 "script": "shell input",
 "stackTrace": [{
 "column": 15,
 "line": 1,
 "script": "shell input"
 }]
}

s2sh> sampleCallback4()
result: string@0x6f5d8[scope=#2@0xbee620e0 ref#=0] ==>
"shell_extend.c:65: Hi again, world!"
s2sh> sampleCallback4(1)
result: integer@0x6a341[scope=#0@(nil) ref#=0] ==> 1
s2sh> sampleCallback4(1,2)
result: double@0x70f58[scope=#2@0xbee620e0 ref#=0] ==> 3.0
s2sh> sampleCallback4(1,2,3)
rc=105 (CWAL_RC_EXCEPTION)
EXCEPTION: exception@0x6f598[scope=#2@0xbee620e0 ref#=0] ==> {
 "code": 303,
 "column": 15,
 "line": 1,
 "message": "Too beaucoup!",
 "script": "shell input",
 "stackTrace": [{
 "column": 15,
 "line": 1,
 "script": "shell input"
 }]
}
```

<a id="s2sh-sample-scripts"></a>
# Sample Scripts

The source code repository is rife with s2 scripts, most notably:

- [/s2/s2sh.s2](/finfo/s2/s2sh.s2) is my [s2sh init script](#s2sh-extend-with-script).
- [The unit test scripts](/dir/s2/unit?ci=trunk).
- [A collection of toys and demos](/dir/s2/toys?ci=trunk).
- `require.s2`: [loadable module](../mod/require/) and [script source](/finfo/s2/require.d/require.s2).


<a id="s2sh-adjust-sweep"></a>
# Adjusting Sweep and Vacuum Intervals

s2 "sweeps up" its current scope at regular intervals (measured in
full expressions, on a per-scope basis) in order to free up any stray
values, and once every N sweeps it goes further and vacuums (which
also catches orphaned cyclic structures). s2sh intentionally
configures the sweep and vacuum intervals to be low (i.e. sweeping up
often) because doing so tends to uncover Value memory misuse (invalid
unrefs) more quickly. It does, however, impact performance,
potentially (depending on the script) drastically. s2sh provides the
`-w` and `+w` flags to increase the sweep resp. vacuum interval, or
the intervals can be modified directly in
[shell.c](/finfo/s2/shell.c): search for "Interval" to find the
relevant code, and modify the `App` global structure to set its
`addSweepInterval` and `addVacuumInterval` members to higher
values. When using the [recommended approach of using a wrapper script
to start s2sh](#s2sh-wrapper-script), `-w` and `+w` can be passed in
there (multiple times if desired: each increases the corresponding
interval by 1).

> Tip: the sweep and vacuum intervals may be modified at runtime
using [the pragma keyword](keyword-pragma.md).

Up until December 2014, sweeping was necessary to clean up temporaries
created by expressions, as the evaluation engine tended to leave them
lying around. As of that point, all the operators do a better job of
reference handling and clean up after themselves, insofar as possible,
so temporaries generally only hang around when they're created in
standalone expressions with no operators:

```s2
new Foo();
```

In the interactive shell, s2sh would catch that and free the stray item
immediately (unless it's cyclic, in which case it will eventually be
vacuumed up), but in batch mode that happens differently: the batch
processor remembers the most recent expression's result and discards it
when the next expression completes. Thus:

```s2
new Foo();
// at ^^^^^^ this semicolon, the batch processor stashes the
// new Foo instance. This value is cleaned up when the following
// expression is finished:
10+20;
// ^^^^ at this semicolon, the batch processor stashes the result
// (30), cleaning up the previously stashed result if its refcount
// hits 0.
```

The interval-based sweeping is performed between expressions, but it
will never sweep up the batch processor's stashed pending result. If
the new `Foo()` is cyclic, its destruction will eventually be ensured by
the vacuum process, which runs once every N sweep-ups.

This stashing of the most recent expression result is necessary so that
any sweep/vacuum run while that result is pending does not
garbage-collect the value. At the end of batch processing, the
last-evaluated result is given to the caller (unless he expressed no
interest in it (by passing `NULL` for the result value handle), in which
case the batch processor does not stash anything at all).

<a id="s2sh-todo"></a>
# TODOs

Various TODOs, in no particular order...

* Try to refactor the bootstrapping bits of s2 into a client-friendly,
amalgamation-friendly "s2 bootstrap" module, so that code like s2sh
can get it up and running with less effort. The real hurdle in such an
effort is that s2sh's options allow for a good deal of engine-level
configuration which must happen before the s2 engine is
initialized.


# Footnotes

[^1]: Noting that the [s2out keyword](keywords.md) is, as of 201912,
    the "preferred" way to post output from scripts, as it's a
    keyword, so its lookup speed is *much* faster than that of a
    globally-scoped variable.

[^50]:  Not the interpreter's client-specified output channel because
    that function does not always have access to the interpreter engine
    :/.

[^51]:  When writing plugins or extending the shell in C, it is often
    easier to implement certain types of extensions in script code once
    the basic native module has been loaded, and such extensions can be
    added to (or imported by) the init script.

