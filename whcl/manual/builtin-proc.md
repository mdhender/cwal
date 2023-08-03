# whcl: proc Builtin Command
##### ([&#x2b11;Table of Contents](./)) ([builtins](builtins.md)) ([data types](type-intro.md))
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

Function doc sections:

- [Achtung: Calling Conventions](#calling-conventions)
- [Syntax](#syntax)
- [Function Names](#names)
- [Defining the Scope](#scope)
- [Parameters and Arguments](#params)
  - [The -flags Conundrum](#flag-args)
- [Closures ("using")](#using)
- [Closures Revisited with `alias`](#alias)
- [Automatic/"Magic" Variables](#automatic-vars)
- [Function-type Methods](#proc-methods)
- [Significant TODOs](#todo)

<a id='calling-conventions'></a>
Achtung: Calling Conventions
============================================================

We're getting ahead of ourselves a bit here but need to point this out
prominently early on:

Generally speaking a function is called by simply naming it as the
first token on a line and passing it arguments as subsequent tokens.
That changes, however, for "methods" - objects properties which are
themselves functions.

*Most* classes in this framework opt for a "command-like" syntax:

```whcl
theObject method-name ...args
```

That approach requires having explicit language-level support in the
form of specific property lookup and dispatch rules built into the
language: if the first element in a script line is a value which has
or inherits a function-type property named `__command`, that method is
invoked to dispatch the command and it is passed all of the command's
arguments. It is up to the `__command` method to then make sense of
the arguments, e.g. dispatching `method-name` to another function or
class method.

Such types _may_ also use a slightly more OO-looking syntax which
does _not_ rely on the `__command` dispatch approach:

```whcl
theObject[method-name] ...args
```

***However***, the Function type (that is, values created by the
`proc` builtin and callback functions bound via C code) only supports
the latter convention because the former requires installing a
prototype-level function to handle that argument order conversion,
introducing an apparently insurmountable chicken-egg situation.

Thus [methods of functions](#proc-methods), when used, have to use the
latter calling convention. Apologies for the inconsistency.

> Trivia: the `x.y` approached `x[y]` are both faster than the
  command-dispatch approach because the dispatching adds another
  level of function calls to the mix.

<a id='syntax'></a>
Creating Functions
============================================================

The `proc` command are used to create functions.

> Sidebar: once one gets used to typing "proc", typing out the
more conventional name "function" just seems like a barbaric
waste of keystrokes.

It has 3 distinct
uses:

- Declare scope-local functions.
- Declare global functions.
- Create anonymous (undeclared) functions.

Its syntax is:

```whcl
proc [-anon|-local|-global] [-xsym] [name] {parameters} {body} [using [-scope] {...}]
```

The various flags control [the scope of the function's
declaration](#scope), if any.  When called with no flags, it defaults
to `-local` or `-anon`, depending on the context:

The `-anon` flag is implied, but can be overridden with the `-local`
or `-global` flags, when `proc` is called in the following
circumstances:

- It is invoked via `[proc ...]`. 99.9 times out of 100, such cases
  want anonymous functions.
- It appears as the final argument to another builtin command which
  has special support for "trailing builtin commands." That includes
  `return`, `break`, `set`, and `decl` (and perhaps others). That is:
  this affects all other builtins which permit their final argument to
  be another builtin command.

When `proc` is not implicitly `-anon` it defaults to `-local`,
declaring the function in the current scope.

The `-xsym` flag is [described in detail below](#-xsym).

Note that the following two constructs are functionally (haha)
equivalent:

```whcl
proc f {} {}; # defaults to -local decl with the name f.
# is equivalent to:
decl f proc {} {}; # defaults to -anon but the decl command names it f.
```

The main advantage of the second formulation is that the declaration
may be made const (cannot be unset or overwritten):

```whcl
decl -const f proc {} {}
```

> Sidebar: no small amount of experience with whcl's predecessor
strongly suggests that global-scope functions and values are rarely
necessary and are a poor match for this language's [symbol resolution
rules](symbol-resolution.md). It is more efficient, both in terms of
symbol lookup speed and garbage collection, if functions are
scope-local in the deepest scope depth which is feasible for that
function. When clients _need_ global-scope functions, they should
consider instead installing them in a client-specific sub-object of
the global `whcl` object or in the `whcl.client` object (which is
reserved specifically for client-side use). The `whcl` object is a
[global-scope built-in value](builtins.md#bivs), so does not suffer
from issues with symbol lookup speed or name collisions. e.g. creating
an object named `whcl.my` or `whcl.client.my` and puting one's
functionality there is generally preferrable over sticking those
functions in the global scope.


<a id='-xsym'></a>
### The `-xsym` Flag

2022-03-28: **THIS SUPPORT IS EXPERIMENTAL AND SUBJECT TO REMOVAL OR
CHANGE.**

As described in [the symbol resolution docs](symbol-resolution.md),
symbol resolution stops looking for an answer when it reaches a function
call boundary. However, in the interest of simplifying the passing-on
of scope-local functions and `eval`'able script snippets which should,
ideally, be able to access "nearby" symbols without having to jump
through the `using` callback hoops, it is possible to tell a given
function that it should _not_ act as such a lookup boundary.

The `-xsym` flag changes how symbol resolution works: calling a
function created with this flag does not create a symbol lookup
barrier. That is, code `eval`'d, or functions called, via that
function can resolve symbols in/via the _calling_ scope, as the outer
function's call no longer acts as a lookup boundary.

This feature should be used with care, but it enables the creation of
certain types of functions which are otherwise more tedious to
construct.

In C code, a Function-type value which has the "client container flag"
named `WHCL_CONTAINER_F_XSYM` set on it has its "wall" broken down for
purposes of calling it from either script code or via the C-level
`whcl_function_call()`. Script-side functions can get that flag by
passing the `-xsym` flag to the `proc` command. However, see the
disclaimer above regarding "experimental" and "subject to change."

In practice, the only time native C functions look up script-side
symbols is when they're `eval`'ing script code or calling callback
functions on behalf of the caller. From that perspective, it would
seem feasible to always enable that flag for native functions (but not
script functions). That inconsistency, compared to script-side
functions, irks me, though. Thus only native functions which
explicitly get that flag set during installation get that behavior.



<a id='names'></a>
Regarding Function Names
============================================================

The function's name is optional only when using the `-anon` flag or
when the function is implicitly anonymous (as described above). If a
function has a name, that name gets set as a call-local variable when
that function is called, which can be used to implement recursion,
returning the function itself from a call, or similar acrobatics. WHCL
has a configurable (from C code) stack size limit and will fail loudly
if recursion goes "too deep."

Function names _may_ contain dashes, but may not start with one:

```whcl
proc foo-bar {} {} # legal
proc -bar {} {} # Not legal
```

Their names may otherwise contain any identifier-legal characters:

```whcl
proc 🙈🙉🙊 {} {}
assert [info is-local 🙈🙉🙊]
```


<a id='scope'></a>
Function Scope
==============

By default, or if the `-local` flag is used, the function is declared
in the current (local) scope:

```whcl
proc foo {} {}
assert [info is-local foo]
```

Since functions are first-class values, however, a locally-declared
function can migrate to an older scope, as in this completely
contrived example:

```whcl
decl f while {true} {
  break [proc {} {}]
}
assert [info is-function $f]
```

If the `-global` flag is used then the function is declared
in the global scope:

```whcl
proc -global foo {} {}
assert [info is-declared foo]
```

`proc` can also create so-called anonymous functions, meaning that
it does not declare them as a variable, using the `-anon` flag,
noting that the `-anon` flag is automatically implied in situations
described in the previous section.


<a id='params'></a>
Parameters and Arguments
============================================================

Function parameters may be declared in two ways:

```whcl
proc {param1 param2 param3} { ... }
proc {{param1 defaultValue} {param2 defaultValue} ...} {...}
```

WHCL _is not strict_ about how many arguments get passed to a
function, much less their type, and does not verify the the number of
declared parameters matches what the caller is attempting to pass in.
"It's possible" that that will eventually be changed to throw an
exception if the declaration does not match what's passed to it, but
at this point seems unlikely. Similarly, it is not picky about
parameters without defaults following parameters with defaults
because, frankly, that level of micromanagement feels unnecessary:

```whcl
proc {{a 1} b {alsoLegal}} {...}
```

The implicit default for any parameter is the `undefined` value.

The list of all arguments passed in to a function is available via the
call-local `argv` variable (an array). `argv[0]` maps to the first
argument, etc. The `argv` array holds only arguments _passed in_ to
the function, irrespective of the number of declared parameters or
their default values.

Default values have two significant properties which are not
immediately obvious:

1. They need not be constant values. Any one token is legal, even if
   it's a "supertoken" (e.g. `[...]`).
2. They are initialized _after_ [all of the "special" function-local
   variables](#automatic-vars) are set up. Thus default parameter
   values may be derived from other, perhaps dynamic, state. Examples
   of this can be found in a following section, after we've covered
   what that other state might be.

TODOs:

- Support a trailing `...name` arg to pack all additional args into
  their own array. OTOH, the same effect can be had by calling
  `argv.shift N` to trim `N` elements off of it. The underlying engine
  doesn't provide a way to fix or validate the number of arguments
  passed to a function, so script functions can always be passed any
  number of arguments.


<a id='flag-args'></a>
The `-flag` Conundrum
------------------------------------------------------------

In TCL it is conventional to pass optional boolean arguments to
functions in the form of flags, and WHCL offers the same for some
functions and builtins, e.g.:

```whcl
echo -n ...
echo -n -s ...
echo -s -n ...
```

(Note that the flags may be passed in arbitrary order, which
distinguishes them from normal arguments.)

_Ideally_, such flags should _not_ intrude on declared paramters, e.g.:

```whcl
proc foo {a b c} {
  echo $argv
}
foo -x 1 2 3
```

That _should_ assign `a` to `1`, `b` to `2`, etc., without the `-x`
flag interfering.

To make a very long story much shorter, WHCL has support for such
flags but requires that functions specifically declare them in order
to treat them specially. All _leading_ declared parameters which start
with `-` are considered to be flags. As soon as a single non-flag
parameter is seen, all special processing of flags stops and parameter
processing reverts to no-flag mode.

For example:

```whcl
proc foo {-flag-x -flag-y a b c} {...}
```

That function declares 2 flags: `-flag-x` and `-flag-y`. When that
function is run, its call-local `argv` value gets those flags added to
it as _properties_. They will have the value `false` if _not_
explicitly passed in to the function and `true` if they are. They will
_not_ intefere with the placement of the remaining arguments:

```whcl
foo -flag-x 1 2 3
```

That will set the call-local vas `a` to `1`, `b` to `2`, `c` to `3`,
and will set `argv[-flag-x]` to `true` and `argv[-flag-y]` to
`false`.

However, it only does this for flags which preceed non-flag
arguments. Any flag which comes after the first non-flag argument is
treated as a non-flag.

If an undeclared flag-like argument is passed to the function, it is
treated as a non-flag. "The problem" is that the function-call
infastructure cannot _reliably_ map raw script engine tokens to their
arguments after those arguments go through certain levels of
redirection, e.g. the [`call`](#method-call) and
[`apply`](#method-apply) methods. Because of that, the engine cannot,
at the point where script-side function arguments are processed,
_reliably_ distinguish between a genuine flag (`-flag`) and a
flag-like string (`"-flag"` or `'-flag'` or `{-flag}`).

Similarly, if a function declares no flags, any flag-like arguments
passed to it are treated as normal arguments. Only flags _declared_ in
the paramter list get special treatment.

Be aware that _all_ arguments explicitly passed to a function, flag or
otherwise, become part of the call-local `argv` array. That means, for
example, that:

```whcl
foo -flag-x -flag-y 1 2 3
```

will have an `argv` array with 5 values: two string values (the flags)
followed by 3 integer values. Only _explicit_ arguments are in `argv`,
not any default values specified for parameters.


<a id='using'></a>
Importing Symbols (Closures (of a sort))
========================================

It is often useful for a function to carry around some persistent
state from its declaration context. This is generically called
"closures," though in WHCL they are "lightweight semi-closures"
which need a slight helping hand in order to function. Namely,
any "imported" symbols which are to be carried around with
a function have to be manually imported into it via the `using`
option:

```whcl
decl a 1
decl o object {b 2 c 3} 
proc {...} {body} using {
  a $a obj $o
}
```

The `using` part is an object with the same syntax as that supported
by the `object` command, including the `-scope` flag, and maps keys to
arbitrary data. Those keys will be set as call-local variables on each
call to the function. An empty `using {}` block is legal for reasons
we'll get to in a moment.

Significant properties and limitations of these imported symbols:

- `using` is a builtin value with a context-dependent
  interpretation. Inside of a script function it evaluates to that
  function's `using` imports, or the `undefined` value if it has
  none. Outside of a script function it evaluates to `undefined`.
- The `using` symbols are evaluated when the function is created, not
  on each call.
- They refer to properties stored in the `using` object and are _not_
  references to the original values. (This is the aspect where they
  differ starkly from closures in most languages.)
- Because they are scope-local variables, assigning to them has no
  persistent effect. On the next call, the variables will get the
  values which were assigned in the `using` object. _However_...
- The `using` builtin value refers to the object which holds the
  `using` mapping. If the function has no `using` part then the value
  of `using` is the `undefined` value, otherwise it is an object and
  may be manipulated like one, and any changes made to it _are
  persistent_.

An example of that last point:

```whcl
proc stateful {} {
    return [incr using[x]]
} using {x 0}
assert 1 == [stateful]
assert 2 == [stateful]
assert 3 == [stateful]
```

An empty `using {}` block is seemingly useless but gives a function
a place to store persistent stuff, perhaps delaying initialization
until the first time the function is called.

Similarly, any function which has a name gets that name set as a
call-local variable of type `function`. Functions are first-class
objects and may have arbitrary properties attached to them.

For non-trivial `using` imports it may be more convenient to use
the `-scope` flag, which changes the interpretation of the `{...}`
part to run as a normal script block and the resulting object
contains any variables created in that block:

```whcl
proc stateful2 {} {
    return [incr $x[y] [get-incr]]
} using -scope {
    decl x object {y 0}
    proc get-incr {} {return 2}
}
assert undefined == using
assert 2 == [stateful2]
assert 4 == [stateful2]
assert 6 == [stateful2]
```

Skipping Declarations of `using` Vars
------------------------------------------------------------

At times it might be useful, if only from an efficiency point of view,
to tell a function to _not_ import the `using` vars as call-local
variables and instead provide access to them only via `using[name]`.

This can be achieved by prefixing the `using` part of the `proc` with
a single `-`, as in this example:

```whcl
proc foo {} {...} -using {...}
```

If used (haha) that way, it must (still) appear _after_ the function
body. Though it looks like a command flag, it's... well, it kinda is,
but it still needs to be at the end of the body. Experience with
WHCL's predecessor strongly suggests that `using` is generally more
readable when it's placed after the function body.

To demonstrate the difference between `using` and `-using`:

```whcl
proc foo {} {
  assert [info is-local a]
  assert $a == using[a]
} using {a 1}
foo

proc bar {} {
  assert ![info is-local a]
  assert 1 == using[a]
} -using {a 1}
bar
```

<a id='alias'></a>
Closures Revisited with `alias`
========================================

With the addition of [property aliasing support][propref], closures
can take on a power they do not otherwise have: they can refer
directly to variables from other scopes, such that getting/setting those
variables in the call scope will act as a proxy for the referenced var
(or object property) and act on that one instead.

A very brief example:

```whcl
decl o object x 0
decl x 1
proc f {} {
    set y 2; # y is a call-local var aliasing x
    set z 1; # z is a call-local var aliasing o.x
} using {
    /* The magic happens here... */
    y [alias x]
    z [alias o.x]
}
f
assert 2 == $x
assert 1 == o.x
```

However, be sure to read [the `alias` docs][propref] to understand
cases where that approach is possibly best avoided.

[propref]: variables.md#alias


<a href='automatic-vars'></a>
Automatic/Magic Variables
============================================================

As mentioned above, but not in a consolidated format, each call to a
script-side function (as opposed to a C-native function) has several
variables automatically defined in its call scope:

- The names of any declared parameters.
- The name of the function (if any).
- The `argv` array of all arguments _explicitly_ passed in to the
  function. This includes, e.g., any passed-in flags but does not
  include default values for parameters.
- [The `using` object](#using) is not a local var but behaves like
  one.
- The word `this` is a mixture of a call-local variable and a built-in
  value, in that it is declared as the former but is reserved like the
  latter. A function called via an object property dereference,
  e.g. `x[foo]`, gets that object set as its `this` for the duration
  of that call. A "free" function (called without a property access
  operation) has no natural/intuitive `this` and is, by long-standing
  cwal convention, its own `this` instead. Outside of a function call,
  `this` evaluates to the `undefined` value.

Native C functions bound to the interpreter to not get these, nor do
they have any access to the parameter names (there are none in C-bound
functons) nor the `using` object (which is hidden behind
interpreter-internal state). They _do_ have access to the `argv`,
`this`, and function object, though in a different form.


<a id='proc-methods'></a>
Function-type Methods
============================================================

Functions, being first-class values, may also have member functions
(a.k.a. "methods") which are themselves functions, which may also have
member methods, which are themselves functions... ad infinitum.

The function methods are...

<a id='method-apply'></a>
apply
----------------------------------------

Usage: `the-function[apply] $theThis $anArrayValue`

This calls `the-function` with `$theThis` as its `this` and
the array of arguments as its arguments.

<a id='method-call'></a>
call
----------------------------------------

Usage: `the-function[apply] $theThis ...args`

This functions (haha) exactly like `apply` but takes the
arguments without an array wrapping them up.

<a id='method-import-symbols'></a>
import-symbols
----------------------------------------

Usage: `the-function.import-symbols [-keep] [args...]`

This imports symbols into a function using the same mechanism
as [the `using` pseudo-keyword](#using).

By default this function clears _all_ imported symbols before starting
its work. The `-keep` flag tells it to retain any it currently
has. Calling this function with no arguments will clear all imported
symbols.

Each argument may be one of:

- A string, denoting the _name_ of a symbol to import. The symbol is
  resolved as if resolution took place in the scope from which
  `import-symbols` is invoked (as opposed to the call-local scope in
  which `import-symbols` is run).
- A properties-capable value, the properties of which are
  imported as-is into the function.

Returns `this`.


<a id='method-source-code'></a>
source-code
----------------------------------------

Usage: `source-code`

If this function was created from whcl code, this returns a copy
of its source code, else it returns `undefined`.


<a id='todo'></a>
Significant TODOs
=================

- It's not currently possible to create an anonymous function and call
  it in the same step, e.g.: `[proc -anon {} {...}]`. It should be
  possible to do this, but we currently lack a syntactical option for
  it.
