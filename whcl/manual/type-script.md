# whcl: Script Type
##### ([&#x2b11;Table of Contents](./)) ([&#x2b11;API Index](api-index.md))
# The whcl_script Class
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

This API may be installed using:

`whcl install-api Script`

or calling `whcl_install_api()` from C code with a module name
of `Script`.

It adds a `whcl[Script]` constructor function and the API
described below.

Jump to...

- [Script Class](#type-script)
- [Script Members](#type-script-members)
  - [Instance Members](#type-script-instance-members)
- [Caveats and Limitations](#type-script-caveats)

<a id="type-script"></a>
# Script (whcl_script)

The `whcl_script` class, colloquially known simply as "Script,"
provides a script-side binding to the same "compiled" script code used
to evaluate whcl scripts. Its primary purpose is the creation of
script snippets which can be repeatedly `eval`'d without having to
recompile them. It's similar, in effect, to a function, but does not
have arguments.

> Design note: it's tempting to make `Script` a builtin command, as
opposed to installing this as a normal class. The down-side is that
adding the member methods to this class's prototype _requires_ loading
the core API infrastructure but the engine is intended to be usable
without the core class APIs installed (e.g. by using
[whclsh](whclsh.md)'s `--cleanroom` flag). However, making it a
builtin would have the benefit of being not only far more efficient
(because we could directly copy the parent script's token chain,
rather than having to recompile it as a string), but could also retain
the origin script and line/column values. Those are lost when the
script argument is passed to the constructor as a string.


<a id="type-script-members"></a>
# Script Members

## Constructor

```whcl
new whcl.Script [-f] [name] [line [column]] source-code
```

This constructor creates a new script with the given code and optional
name (the name can be changed later, but the code cannot). This step
does not evaluate the code given to it, it only prepares it for
evaluation.

The `name` argument is optional - some meaningless default is used if
it's not set. Tip: the [`__FLC` builtin value](builtins.md#bivs)
makes a convenient ad-hoc name:

```whcl
new whcl.Script __FLC {...}
```

The `line` and `column` are optional. If set, the line/column
information in the script is adjusted as if the first token were at
the given line/column. If `line` is provided but `column` is not then
a column of 0 is used. If `line` is 0 then both it and the `column`
are ignored. If either is out of range for the underlying numeric type
then an exception is thrown. Arguably the most useful case for the
`line` and `column` arguments is:

```whcl
new whcl.Script __FILE __LINE __COLUMN {...}
```

with the slight caveat that `__COLUMN` resolves to the column of
_that_ token. Given that slight wart, it's probably best to not pass
an explicit column unless there's a driving need for it.

The `source-code` argument may be a string or a buffer. If it is a
buffer, however, this function (on success) _takes_ its memory rather
than copying it. If `source-code` is a string then the `-f` flag tells
it to treat `source-code` as a filename instead of source code. Buffer
input is never treated as a filename. With the `-f` flag, the script's
name defaults to the given filename, but passing the `name` argument
to this function will use that name instead.

It seems likely that the most common final argument to this
constructor will be `{...}`. Keep in mind that:

- The [`new` command](builtin-new.md) _optionally_ (via its [`-post`
  flag](builtin-new.md#post-init) flag) treats such a trailing block
  argument as a post-initialization block instead of a constructor
  argument.
- The surrounding braces are elided when that block is interpreted as
  a string (before it's passed to the constructor function). That
  string format, like heredocs, does not undergo backslash
  unescaping. The "loss" of the braces does not affect how the code is
  interpreted later on, but it does affect what the `source-code`
  method returns.
- Passing the code as a heredoc is also an option, e.g. using
  `{{{...}}}`.

And...

> Design note: the name argument is first because it's generally more
  readable to have the "short" argument before the "long" one,
  considering that the code argument can be arbitrarily long and span
  any number of lines. It is, however, slightly troubling that the
  value associated with the `-f` flag is not directly adjacent to the
  flag when passing in both arguments, but flags have to preceed all
  non-flag arguments in this framework because handling them is
  otherwise a tremendous burden.

<a id="type-script-instance-methods"></a>
## Instance Methods

The following inherited methods require a script instance...

<a id='script-column'></a>
### column

Usage: `column [new-starting-column [new-starting-line]]`

If passed no arguments, this function fetches the script's current
column number relative to the source code it was originally compiled
from, noting that this value applies only to the first line of the
script. Subsequent lines always retain their initial column values.

If passed a value, the script is renumbered with the given starting
column and (optionally) line. If a column number is provided but no
line number, the current line number is retained. When used this
way, this function returns `this`.

Throws on error.

See also: [`line`](#script-line)

### destroy

Usage: `destroy`

Frees all C-level resources owned by this script. Calls to script
methods after calling this will trigger an exception. Returns
`undefined`.

<a id='script-dump'></a>
### dump

Usage: `dump [-eof] [-m] [-v]`

Dumps out this script's compiled contents in some semi-readable
debugging form very close to its internal representation. Output goes
to the script engine's output channel. Returns `this`.

The flags control the output:

- `-eof` includes so-called EOF tokens. Most scripts have many of
  these (e.g. at the end of each block construct).
- `-m` enables certain metrics at the end of the dump.
- `-v` (verbose) includes additional info, namely the strings of each
  token.

<a id='script-line'></a>
### line

Usage: `line [new-starting-line [new-starting-column]]`

If passed no arguments, this function fetches the script's current
line number relative to the source code it was originally compiled
from.

If passed a value, the script is renumbered with the given starting
line and (optionally) column. If a line number is provided but no
column number, the current column number is retained. When used this
way, this function returns `this`.

The `line` and `column` numbers are primarily used for error
reporting. Though it's not assumed that these will be useful for
client-side scripts, they exist because it's convenient to be able to
test the underlying C APIs this way.

Throws on error. Line numbering starts at 1, and if passed a value of
0 it is treated as a 1.

See also: [`column`](#script-column)

### name

Usage: `name [new-name]`

This function gets or sets the script's name. Returns the name.


### run

Usage: `run`

Evaluates the given script code. This happens within a
native function's call scope, so all `decl`s and the like
are local to this invocation of the script.

In order to be more useful for various envisioned uses, this function
[does not block symbol lookup](builtin-proc.md#-xsym), so symbols in
the script may resolve from the caller's scope.

It returns the result of the final command in the script.

#### ... or `eval`

Alternatively, the contents of a script object can be evaluated
_in the current scope_ using:

```whcl
eval -> $myScript
```

or in a new scope:

```whcl
eval -scope -> $myScript
```

The `->` modifier is required for evaluating its _contents_, as
opposed to the _value_ of `$myScript`. With that flag, `eval`
recognizes that its argument is a script and Does The Right Thing with
it.

Functional differences from the `run` method:

- By default, `eval` runs in the calling scope. The `run` method cannot
  do that. That means, for example, that vars declared via an `eval`'d
  script apply in the local scope.
- `eval` retains certain scope-level state which allows, e.g., the
  `eval`'d code to pass on `break` and `continue` results if they are
  `eval`'d within the context of a loop. The `run` method cannot do
  that.


### source-code

Usage: `source-code`

Returns a copy of the script's complete source code.


### to-jsonable

Usage: `to-jsonable`

Overridden to return a plain object with this script's core state:

```json
{ "script": "the source code", "name": "this object's name" }
```

See [the JSON API](api-json.md#json-to-jsonable) for more details.

Caveats and Limitations
======================================== 

***ACHTUNG***: the script infrastructure does not support the
execution of a single script multiple times concurrently. i.e. do not
recurse into a script object from within that object's own
code. Results are specifically undefined. (Extremely basic tests show
it to work, but there are very possibly more advanced constructs in
which it will fail unpredictably.)

Functions created in a script may be recursed into, just not the outer
script itself.
