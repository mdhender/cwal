# whcl: Builtin Commands
##### ([&#x2b11;Table of Contents](./))
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

Sections:

- [Builtin Commands](#bics)
  - **A**: [`affirm`](#bic-affirm) [`alias`](#bic-alias) [`array`](#bic-array) [`assert`](#bic-assert)
  - **B**: [`break`](#bic-break)
  - **C**: [`catch`](#bic-catch) [`concat`](#bic-concat) [`const`](#bic-const) [`continue`](#bic-continue)
  - **D**: [`__debug`](#bic-__debug) [`decl`](#bic-decl) [`decr`](#bic-decr) [`do`](#bic-do)
  - **E**: [`echo`](#bic-echo) [`eval`](#bic-eval)
    [`exception`](#bic-exception) [`exit`](#bic-exit)
    [`expr`](#bic-expr)
  - **F**: [`for`](#bic-for) [`foreach`](#bic-foreach)
  - **I**: [`if`](#bic-if) [`incr`](#bic-incr) [`info`](#bic-info)
  - **N**: [`new`](#bic-new)
  - **O**: [`object`](#bic-object)
  - **P**: [`pragma`](#bic-pragma) [`proc`](#bic-proc)
  - **R**: [`return`](#bic-return)
  - **S**: [`set`](#bic-set)
  - **T**: [`throw`](#bic-throw)
  - **U**: [`unset`](#bic-unset)
  - **W**: [`while`](#bic-while) [`with`](#bic-with)
- [Builtin Values](#bivs)
- [The Dichotomy of Builtin vs Non-builtin Commands](#builtin-vs-non)

Pages dedicated to specific builtin commands:

- [Working with Variables](variables.md) (`decl`, `set`, `incr`, `decr`)
- [Expressions: `expr`](builtin-expr.md)
- [Flow control](flow-control.md) (loops and `if/else`)
- [Functions: `proc`](builtin-proc.md)
- [Metadata queries: `info`](builtin-info.md)
- [Interpreter internals: `pragma`](builtin-pragma.md)
- [The `new` builtin](builtin-new.md)

------------------------------------------------------------

<a id='bics'></a>
Builtin Commands
============================================================

WHCL has a relatively limited set of builtin commands (colloquially
known as BICs). Though client code may freely define new commands,
builtins have a few differences:

- Their symbolic names are "reserved" (defined losely). Variables may
  not share a name with a builtin, and attempts to declare such
  variables will fail. Object properties names may be the same as a
  builtin command or value but may, depending on the context, need to
  be accessed via quoted strings to avoid a syntactic ambiguity.
  e.g., `obj.__FLC` is okay but `obj[__FLC]` expands as [the `__FLC`
  builtin value](#bivs) and would need quoting to avoid that:
  `obj['__FLC']`
- Lookup speed of builtin command names is always O(1), whereas lookup
  speed of non-builtins is "amortized O(1)" with an O(N) component, N
  being the number of scopes the engine must search through for a
  match.
- Builtins have access to the raw tokens used for evaluating the
  script, which allows them to do some things which client-defined
  commands cannot, such as access the script name, line, and column
  number for any given argument.
- They do not have a script-accessible data construct, so it is
  impossible to create symbolic references to them. See [this
  section](#builtin-vs-non) for more detail.


The following list of command names is reserved for potential future
use as builtin commands:

- import, package

The complete list of built-in commands follows in alphabetical
order...

<a id='bic-affirm'></a>
affirm
------------------------------------------------------------

Usage: `affirm EXPRESSION|{CODE}`

Works exactly like [`assert`](#bic-assert) except that it throws an
exception, instead of a fatal error, if its expression evaluates to
falsy.


<a id='bic-alias'></a>
alias
------------------------------------------------------------

Usage: `alias varName | object[property] [interceptor-function]`

This command creates a new "property alias" value (a.k.a. property
reference or propref), which exists to redirect fetching and setting
of a given variable or object property to a different value.

This command is covered in detail in [the variables
docs](variables.md#alias).


<a id='bic-array'></a>
array
------------------------------------------------------------

See also: [array data type](type-array.md)

Usage: `array [values...]`

Creates a new array instance. Its arguments may be values
and they may may optionally be contained in `(...)`,
which allows them to span lines without requiring escaping of the
newlines. e.g. `array a b c` and `array (a b c)` are equivalent.

Value tokens in the form `{...}` are treated as object literals,
not strings, and are recursively processed.
See [the `object` command](#bic-object) for more details.

Value tokens in the form `(...)` are treated as array literals,
not expressions, and are recursively processed.

> Achtung: recursively treating `(...)` as arrays is of course
incompatible with using `(...)` as a shorthand for `[expr ...]`.
Evaluating expressions for use as array values in an array body
requires the long-call form: `array (a b [expr $x + $y] ...)`.  Parens
groups inside the `expr` call are treated as subexpressions, not array
literals.


<a id='bic-assert'></a>
assert
------------------------------------------------------------

See also: [`affirm`](#bic-affirm)

Usage: `assert EXPRESSION|{CODE}`

Kills the current script if its expression evaluates to falsy. Note
that it takes an *expression* argument, not a command.

If given a single argument which is a `{...}` block, it runs that code
in a new scope, using the result of the scope's last-run command as
the test result.


<a id='bic-break'></a>
break
------------------------------------------------------------

Usage: `break [value|builtin-command args...]`

Stops execution of the inner-most loop construct, optionally giving
the loop a result value. It is an error to call this from outside of a
loop. If its first argument is a reference to a builtin command, that
command is called and its result becomes the result.

See [the flow control page](flow-control.md) for examples.

<a id='bic-catch'></a>
catch
------------------------------------------------------------

Usage: `catch [-noscope] [varname|$var[property]] {body}`

If the body throws an exception then this stops propagation of the
exception and evaluates to the exception value. If given a variable
name or `$var[property]` then the exception is also stored there. The
variable is created in the local scope and must not already exist in
that scope. `catch` will translate non-exception syntax errors (but
not other errors) to exceptions. If no exception is thrown, this
evaluates to the `undefined` value and the variable is (for
the sake of consistency and usability) still declared.

By default the body is run in its own scope. The `-noscope` flag
causes it to run in the current scope instead. That's almost never
really necessary but it was added to support whcl's own unit tests,
which needed that to support a given test case. It does, however,
provide a convenient way of catching exceptions thrown in scope-level
var initialization:

```whcl
assert ![info is-local x]
catch -noscope {
    decl x 1; # this is running in the outer scope, so...
}
assert [info is-local x]
```

<a id='bic-concat'></a>
concat
------------------------------------------------------------

Usage: `concat value1 ...`

Works identically to passing all arguments to the `concat` method of
an empty string, emitting each value in some string form and returning
a string with their concatenated values. Provided primarily as a
convenience to work around the lack of in-string var expansion. Note
that it accepts as few as one arguments, the intent of that use case
being to convert non-strings to strings.

<a id='bic-const'></a>
const
------------------------------------------------------------

Usages:

- `const varName value|builtin command...`
- `const {varName value var2 value...}`

This is a convenience form of [`decl -const`](#bic-decl).

<a id='bic-continue'></a>
continue
------------------------------------------------------------

Usage: `continue`

Skips to the next loop iteration of inner-most loop construct. It is
an error to call this from outside of a loop.


<a id='bic-__debug'></a>
__debug
------------------------------------------------------------

(Achtung: this might get renamed to `__testing` or `__testmode` or
some such.)

Usage: `__debug {...}`

These blocks function like `eval -scope {...}` except that they are
only run if the interpreter's debug block feature flag is enabled and
always evaluate to the `undefined` value. With [whclsh](whclsh.md)
they can be enabled with the `--debug` flag. In C that's done by
calling
`whcl_feature_flag_set(theWhcl,WHCL_FEATURE_F_DEBUG_BLOCK,true)`.

> Reminder to self: ideally these tokens would be stripped entirely
during the compilation process, but by the time we can handle the
`{...}` part the chain has already been allocated so we save no memory
by stripping them. TODO: restructure the chain in the post-compilation
step such that it behaves as if these blocks simply don't exist, as
opposed to skipping over them at eval time (which is not slow but it's
unnecessary).

<a id='bic-decl'></a>
decl
------------------------------------------------------------

See also: [variables][] and [`const`](#bic-const)

Usages:

- `decl: [-const] varname [value|builtin-command args...]`
- `decl: [-const] {varname value [var2 value2...]}`

Declares a symbolic name as being available for this scope and [all
subsequent sub-scopes which can see into this
one](symbol-resolution.md).

With the first usage, if the final argument is the name of a builtin
command then that command is run, passing on any remaining
arguments. That permits declarations to use (e.g.) the `object`,
`array`, and `proc` commands without having to wrap them in
`[...]`. The `{...}` usage requires matching pairs of var names and
values and does not support commands as values unless they're wrapped
in `[...]`.

The first form evaluates to the new value, or `undefined` if no value
was provided.  The second form evaluates to `undefined`.

The `-const` flag marks the variable(s) as "constant," meaning it/they
cannot be reassigned. It is illegal to use this flag without providing
a value. If the value is a container object, constness does not
protect its contents from being modified.

> Sidebar: long story short, the requirement of explicitly declaring
  variables instead of using `set` to both declare and assign to
  variables is due to significant difference between how TCL and our
  scripting engine handle scopes and resolve symbols. We cannot
  sensibly support TCL-like usage within this framework.


<a id='bic-decr'></a>
decr
------------------------------------------------------------

Usage: `decr identifier|$var[property] [value=1]`

See also: [incr](#bic-incr)

Decrements the value of the referenced variable or object property by
the given amount. Fails if given an unknown var name but will use a
default value of 0 for unknown (or non-string) object properties.
Evaluates to the new value.

<a id='bic-do'></a>
do
------------------------------------------------------------

Usage: `do {script body} while {EXPR}`

Implements a do-while loop.

See [the flow control page](flow-control.md#do-while) for more
details.


<a id='bic-echo'></a>
echo
------------------------------------------------------------

Usage: `echo [-n] [-s] [...args]`

Outputs its arguments with a space between each one unless the `-s`
flag is used. It outputs a newline at the end unless the `-n` flag is
used.


<a id='bic-eval'></a>
eval
------------------------------------------------------------

Usage: `eval [-scope] [->] one-token`

Evaluates the given code in the current scope unless the `-scope` flag
is used, in which case it runs in its own scope. It evaluates to the
value of the token. If the token is a block of script code, it
evaluates to the value of the last command. `eval` treats a `{...}` as
a script block, not a string.

The `->` flag changes its semantics: if the eval results in a string
or [Script](type-script.md) value, `->` causes the contents of that
string/Script to be eval'd. This is necessary for evaling code within
strings (possibly dynamically-constructed). Without that capability,
`eval` is of very limited use except for its ability to create a scope
(which can also be done using `if {true} {...}`).

> *ACHTUNG*: the `->` flag must come after all other flags for boring
  technical reasons.

Note that `eval` retains certain scope-level state which allows, e.g., the
`eval`'d code to pass on `break` and `continue` results if they are
`eval`'d within the context of a loop.

`eval` has one truly useful feature which may not be immediately
obvious: when evaluating a `{...}` block, the result of that
evaluation is the result of the final expression or command call
in that block. It's not always trivial to arrange the code so that
"just the right" piece is the final line of the block, though. We
can, however, use a second `eval` to get whatever result we want:

```whcl
decl x eval -scope {
    decl o object
    set o[x] proc {} {...}
    set o[y] proc {} {...}
    # We want $o to be the result, but how do we do that?
    # Answer: use another eval:
    eval $o; # This becomes the outer eval's result.
             # This operation is very cheap, too.
    # Alternately:
    # expr $o; # Has the same effect
}
```


Potential TODOs:

- ??? `-return` would cause it to catch any `return` called from
  within the block, giving the user more freedom over its structure
  in non-trivial cases.

> Reminder to self: we can probably use this for evaling files via a
`-file` flag, but an importer function would be better because it
could have a configurable search path (like s2's `import` keyword).


<a id='bic-exception'></a>
exception
------------------------------------------------------------

See also: [exception data type](type-exception.md)

Usage: `exception [integer code] message|builtin-command args...`

Creates, but does not [throw](#bic-throw), an exception object. It
passed 2+ arguments the first may be an integer *or* the string form
of a `CWAL_RC_xxx` result code, with or without the `CWAL_RC_`
prefix. e.g. `"RANGE"` and `"CWAL_RC_RANGE"` are equivalent.  If the
value is 0, not provided, or is not a number then the the default of
`CWAL_RC_EXCEPTION` is used.

The other argument may be any value type or may be a built-in with
additional arguments, in which case the result of that builtin becomes
the exception message.

The exception's `code` and `message` properties expose those
values. The `code` value need not be from the `CWAL_RC_...` family of
codes: it may be an arbitrary client-defined integer value. (If it is
not an integer, it is coerced to one.)

To throw the exception use [the `throw` command](#bic-throw).

<a id='bic-exit'></a>
exit
------------------------------------------------------------

Usage: `exit [value|builtin-command args...]`

Exits the script without an error, optionally with a value which gets
propagated back to the interpreter's caller. Script code cannot stop
propagation of `exit` but C code between the `exit` and the top-level
application can do so.


<a id='bic-expr'></a>
expr
------------------------------------------------------------

Usage: `expr args...`

Evaluates its arguments as a single expression, as opposed to a
command. This performs conventional math operations on its arguments,
which may include any of the standard math operators, parenthesized
groups, etc. Expansion of `$variables` and `[command...]` are
performed.

See [the expr page](builtin-expr.md) for more details.


<a id='bic-for'></a>
for
------------------------------------------------------------

Usage: `for {pre} {condition} {post} {body}`

Performs a conventional "for" loop.

See [the flow control page](flow-control.md#for) for more details.


<a id='bic-foreach'></a>
foreach
------------------------------------------------------------

Usage: `foreach [indexIdentifier] valueIdentifier iterable {script body}`

Iterates over list entries, object properties, or string characters.

See the [the flow control page](flow-control.md#foreach) for full
details.


<a id='bic-if'></a>
if
------------------------------------------------------------

Usage: `if {condition} {script body} [else if {condition} {body}] [else {body}]`

Implements a conventional if/else construct.

Documented in more detail in [the flow control
page](flow-control.md#if).



<a id='bic-incr'></a>
incr
------------------------------------------------------------

Usage: `incr identifier|$var[property] [value=1]`

See also: [decr](#bic-decr)

Increments the value of the referenced variable or object property or
list index entry by the given amount. Fails if given an unknown var
name but will use a default value of 0 for unknown (or non-string)
object properties and list entries. Evaluates to the new value.


<a id='bic-info'></a>
info
------------------------------------------------------------

Usage: `info sub-command [arg]`

Permits querying all sorts of information about variables and
values. Described in [the `info` command page](builtin-info.md).

<a id='bic-new'></a>
new
------------------------------------------------------------

Usage: `new [-post] Constructable ...args [{post-construction code}]`

`new` provides a mini-framework for constructing new "classes" (of a
sort). Described in detail on [the `new` page](builtin-new.md).


<a id='bic-object'></a>
object
------------------------------------------------------------

See also: [object data type](type-object.md)

Usages:
 
- `object [{key value ... keyN valueN}]`
- `object -scope {script code}`
- `object -this {script code}`

"Object" is the framework's general-purpose property storage and
hashtable type, analog to a "dict" in TCL. This command offers
several approaches to creating objects...


### object Usage #1

Usages:

- `object [key value ... keyN valueN]`
- `object [{key value ... keyN valueN}]`

Creates a new object instance. Its optional arguments may be either
_pairs_ of keys and values or a single `{...}` containing _pairs_ of
keys and values, e.g. `object {a b c d}` and `object a b c d` are
equivalent but the former may span lines without requiring that the
newlines be escaped.

Keys may be any of:

- Literal number or string (any form except a heredoc).
- Identifiers (always treated as unquoted strings).
- `[command...]` uses the result of the command as the key.
- `(expr)` uses the result of the expression as the key.
- `$var` uses the result of `$var` as the key. Note that property
  access, e.g. `obj.x` or `$obj.x` or `obj.x`, is very explicitly
  _not_ permitted as keys (because Reasons). To get the same effect,
  use `(obj.prop)`.

Note that keys retain their type, are not coerced to strings, and
property lookups are type-specific, so `1.0`, `1`, and `"1"` are three
independent keys. It is never recommended to use floating-point
numbers as keys because ["1.0 is not always
1.0"](https://floating-point-gui.de/errors/comparison/).

_Value_ tokens in the form `{...}` are treated as objects, not
strings, and are recursively processed.

_Value_ tokens in the form `(...)` are treated as arrays, not strings,
and are recursively processed, as per [the `array`
command](#bic-array). (See that command for an achtung regarding
the different-from-the-norm semantics of `(...)`.)

Keys may not currently be dynamic, e.g. var dereferences or the output
of a command, but `set $anObj[...]` does accept such things.

### object Usage #2 (`-scope`)

Usage: `object -scope {code}`

This variant behaves much differently than the one described above: it
runs the given `{...}` as a new scope and takes over the scope's
properties. The main implication of that is that all variables and
functions declared within that scope become members of the object and
retain special features like their "constness." For example:

```whcl
decl o object -scope {
  decl a 1
  decl -const b 2
  proc foo {} {...}
}
assert [info is-function $o[foo]]
assert 2 == $o[b]
assert [[catch {set $o[b] 3}].code-string] == CWAL_RC_CONST_VIOLATION
```


> Sidebar: this form was added primarily to simplify creating new
classes for use with [the `new` command](#bic-new). Interestingly, the
C-level implementation for this approach requires only about a third
as much code as the other one.

### object Usage #3 (`-this`)

Usage: `object -this {code}`

This (haha!) variant behaves identically to `-scope` but also defines,
for the duration of the object's body, `this` to be the
under-construction object (the same object which stores the
scope-level variables!). The primary use case for which to prefer
`-this` over `-scope` is when defining [new class
prototypes](builtin-new.md) and you want to change or remove the class
prototype's own prototype:

```whcl
decl Klass proc {} {...}
set Klass[__prototype] object -this {
    unset this.__prototype
    # 'this' _is_ the scope-level property storage, ergo...
    assert this == this.this
    decl x 1
    assert 1 == this.x
    ...
}
```


<a id='bic-pragma'></a>
pragma
------------------------------------------------------------

Usage: `pragma [...]`

Described in [the pragmas page](builtin-pragma.md).



<a id='bic-proc'></a>
proc
------------------------------------------------------------

Usage: `proc [-global|-anon|-local] [name] {args} {body} [using [-scope] {...}]`

Creates a function. Described in detail in
[the `proc` page](builtin-proc.md).


<a id='bic-return'></a>
return
------------------------------------------------------------

Usage: `return [value|builtin-command args...]`

Exits the current function or, if running a top-level script, that
script, optionally with a result value. If its first argument is a
reference to a builtin command, that command is called and its result
becomes the return result.


<a id='bic-set'></a>
set
------------------------------------------------------------

Usage: `set [-const] varname|$var[property] value|builtin-command args...`

Sets a variable to a new value. See [variables][] for full details.
The `-const` flag is only legal when used to set object properties,
not scope-level variables or list indexes.

It accepts a builtin command as its final argument, as described for
[the `decl` command](#bic-decl).


<a id='bic-throw'></a>
throw
------------------------------------------------------------

Usage: `throw message|builtin-command args...`

Throws an exception with the given message string.  If the argument is
a built-in command then it is called and its result becomes the
exception message, else the first argument becomes the exception
message. If such a built-in returns an exception object, it is thrown
as-is, rather than being embedded as the message of the returned
exception. For example:

```whcl
throw exception RANGE "Number is out of range."
```


<a id='bic-unset'></a>
unset
------------------------------------------------------------

Usage: `unset varname | var[property] | var.property ...`

Unsets variables from _the current scope_ or object properties. It
accepts any number of variables or properties to unset. It throws if
asked to unset an unknown (in the current scope) variable but silently
ignores missing properties.

If the _value_ of an `unset` var or property is a [propref-type
value](variables.md#ref), the propref, and not the property of the
proxied value, is removed.

See [variables][] for full details.


<a id='bic-while'></a>
while
------------------------------------------------------------

Usage: `while {EXPR} {body}`

Described in the [flow control page](flow-control.md#while).

<a id='bic-with'></a>
with
------------------------------------------------------------

Usage: `with value {body}`

This command enables a shorthand form of accessing properties of a
given object within a block of code:

```whcl
with $myObj {
    set .x 1
    set .y (.x + 1) ;# recall that (...) is (mostly) an alias for [expr ...]
}
```

In these code blocks, and only in these blocks, the property access
syntax syntax `.X`, with no LHS, is legal. It may begin a chain of
`X.Y` and `X[Y]` nested property accesses, e.g. `.foo[bar].baz`.

`with` runs within its code in its own scope. (Sidebar: it "could" run
in the current scope, but only if we give up the `..X` property access
(see below).)

The result of `with` is its object operand.

Be aware that nesting `with` blocks is legal but their semantics are
extremely simple: the innermost `with` to a `.X` property access
determines the object from which the property will be resolved. For
example:

```whcl
# Beware the limits of nested with...
decl o object {x 1}
with $o {set .y (.x + 2)}
assert 1 == o.x
assert 3 == o.y

decl x object {z $o nope 1}
with $x {
    assert 1 == .nope ;# ==> x.nope
    with .z {; # .z ==> x.z
        # In this block, . refers _only_ to x.z aka $o
        assert 1 == .x ;# ==> o.x
        assert 3 == .y ;# ==> o.y
        assert undefined == .nope ;# ==> o.nope
        # However...
        assert 1 == ..nope ; # note the extra dot
    }
}
```

One `.` may preceed such property references for each level of `with`
to look upwards, noting that it only works in the same lexical scope.
A function call boundary will block inner `with`s from accessing outer
ones.

> Sidebar: in a language where object properties are not set in stone,
  `with` cannot do anything sensible by checking outer `with` blocks
  to try to resolve a `.X` reference, in particular in a setter
  context (which object do we target?).

Complete examples of using `with` can be found in [its unit test
script](/file/whcl/unit/000-020-with.whcl).

------------------------------------------------------------

<a id='bivs'></a>
Builtin Values
==============

In addition to the above commands, certain symbols refer to builtin
values. Such symbols differ from variables primarily in that they
(like built-in commands) always have O(1) lookup speed and cannot be
modified by client code (neither script nor C code). The builtin
values are...

- **`false`**, **`true`**: conventional booleans.
- **`null`**: conventional "not a value" value, but...
- **`undefined`**: default value for anything which does not otherwise
  have one. In practice (with this scripting engine) `undefined` is
  used where `NULL` would be used in C/C++/Java/etc. One notable
  exception is when working with JSON-format data, where `null` exists
  but `undefined` does not. Whether to prefer `undefined` or `null`
  for a given context is largely a matter of personal taste. In
  practice, whcl's developer generally uses `undefined` almost
  everywhere.

All of those are constant values.

The following builtin values are "magic" in some way or other, following
rules which apply to no other values:

- **`__COLUMN`** resolves to the current line's column offset of
  this token.
- **`__FILE`** resolves to the current script's name. This might be a
  "logical name," as opposed to a file name. If no filename is set,
  it evaluates to a string describing that situation. (That can happen,
  e.g., when using the `eval-contents` methods of the string and buffer
  classes.)
- **`__FILEDIR`** resolves to the part of `__FILE` up to and including
  the final `/` or `\\` character (if any). If the name has no
  directory separators then an empty string is returned. If the
  script has no name set, `undefined` is returned.
- **`__LINE`** resolves to the current line number within the current
  script.
- **`__FLC`** resolves to a string in the form `filename:LINE,COLUMN`,
  representing the position of the `__FLC` token. This makes a convenient
  marker in debug output.
- **`this`** is a call-local variable inside function calls and some
  code-block eval contexts which refers to the "this" object of the
  current function call. Outside of a script function, or similar
  special-case blocks, it resolves to `undefined`. Declaring a var
  with this name from script code is forbidden, but C code
  has no such restriction.
- **`using`** is a pseudo-keyword [used (as it were) by
  functions](builtin-proc.md#using). `using` exists primarily for
  efficiency's sake, to avoid having to define it as a call-local
  variable in every script-side function call.
- **`whcl`** is a global-scope object which exists primarily to act as
  a namespace for storing classes and whatnot.

The lists of builtin commands and values will grow as the language
does, but the intent is to keep them brief.

For builtin values which support property access (namely `using`,
`this`, and `whcl`), properties can be accessed via
[the usual propery access syntax](grammar.md#grammar-deref-props).


<a id='builtin-vs-non'></a>
The Dichotomy of Builtin vs Non-builtin Commands
============================================================

whcl's syntax is as minimal as it gets: it accepts a "line" with a
"command," calls the command, and passes the rest of the line to the
command to do whatever it wants with it.

Easy peasy.

The devil, of course, is in the details. In order to be even remotely
useful, whcl has to take the next step and provide features for
manipulating the scripting environment, e.g. to change the current
path of code execution (a.k.a. [flow control](flow-control.md)).
Though, _in principle_, such commands could be attached to whcl the
same way that client-bound functions are, it would both severely limit
certain capability and would force some large semantic changes on the
language. One glaring example of this is that a client-installed
command is always (always) run in a new scope. If that were the case
for builtin commands, some of them could not function as-is or would
have to explicitly know exactly how far down the scope stack they are
running from the intended target scope (which, in reality, they cannot
_reliably_ know because the engine may inject scopes at any point for
garbage-collection purposes). Similarly, builtin commands have direct
access to the internal representation of the script language, allowing
them to do things which external commands cannot do (and should not be
able to, lest they hose it).

Astute whcl users will have noticed that certain built commands behave
slightly differently in the face of builtin commands vs client-side
commands, for example:

```whcl
# This is legal:
return proc {} {...}

# But these have much different semantics:
return user-defined-command ...
return $user-defined-command ...
```

The reason boils down to the latter being ambiguous:

```whcl
return user-defined-command  ;# a string or a call?
return $user-defined-command ;# reference the function's value or call it?
```

These cases require explicitly telling whcl their intent:

```whcl
return [user-definied-command ...] ;# call and return result
return $user-defined-command       ;# a reference to the command's function
return user-defined-command        ;# the identifer/string "user-defined-command"
```

That ambiguity does not arise with builtin commands for one reason
only: builtins have no script-level internal representation. They are
"pure native code." It is not possible, in whcl, to take a reference
to a builtin command and later dereference it to call that command. It
is not possible to overwrite or remove builtins. Client-bound/external
commands, on the other hand, are connected as script-accessible
entities using the features of the underlying language-agnostic
scripting engine, [cwal](/). As such, they play by different rules.

Having said all of that: the capability of certain builtin commands to
behave slightly differently when given another builtin command is
provided soley as "syntactic sugar." Ideally, this distinction would
not exist and non-builtins could be treated identically to
builtins. The hard truth, however, is that we live on an [Animal
Farm](https://en.wikipedia.org/wiki/Animal_Farm): all commands are
equal, but some are more equal than others. However, anyone bothered
by this distinction is free to not make use of it:

```whcl
return proc {} {...}   ;# can also be written like:
return [proc {} {...}] ;# same as non-builtin commands
```

Let's face it, though: the former is easier to both read and
write. However, it is _still_ not syntactically possible to pass
around a reference to a builtin command via a variable for the simple
reason that builtin commands have no state which a variable _can_
reference. That difference "could" be smoothed over via the addition
of further infrastructure, but such extra weight currently seems
unnecessary. (Also, a use case for it has yet to crop up.)


[variables]: variables.md
