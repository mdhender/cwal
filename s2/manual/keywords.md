# s2: Keywords
#### ([&#x2b11;Table of Contents](./)) ([&#x2b11;grammar](grammar.md))
# Keywords

Jump to...

* [Intro](#keyword-intro)
* [Keyword List](#keyword-list)
    * [Reserved Keywords](#keyword-reserved)
* [Placement & Precedence](#keyword-placement)
* [Variables: var & const](#keyword-var-const)

The following keyword-related topics are described in detail in their
own files:

* [`eval` & Friends](keyword-eval.md), including `return`, `throw`,
  `catch`, `scope`, `break`, `exit`, and `fatal`.
* [Flow Control: `if`/`else` and loops](keyword-flow-control.md)
* [Creating functions: the `function` and `proc` keywords](type-function.md)
* [`new`](keyword-new.md)
* [`typeinfo()`](keyword-typeinfo.md)
* [`pragma()`](keyword-pragma.md)
* [`import()`](keyword-import.md)

<a id="keywords-intro"></a>
# Keywords

In s2, keywords are expression parsers which change how the tokens to
their right are interpreted. Unlike in most languages, all s2 keywords
evaluate to a value, and are generally acceptable anywhere a value is
accepted (provided other syntactic constraints do not apply, e.g. the
`break` keyword is only usable in the context of a loop). Thus, for
example, a loop or variable declaration may appear as a default value in
a function parameter list. Some few keywords evaluate like operators
(see the operator list for their precedence). The rest, when used in
expression chains, consume any RHS part after the keyword as if any
pending LHS operators did not exist, and this requires extra grouping in
some cases (described after the keyword list).

s2 does not allow (==syntax error) variable names and function parameter
names to match a keyword, as they would be interpreted as keywords
(instead of variables) when used later on by the client. Object
properties, hashtable keys, and the like, may safely collide with
keywords because context assures that they will never be treated like
one.

<a id="keyword-list"></a>
# List of Keywords

The keywords, listed alphabetically:

```s2-member
__BREAKPOINT
```

(reserved, not yet implemented) Intended to provide a way for a C
debugging breakpoint to be set at arbitrary places in script source
code, so as to allow one[^16] to more easily hone in on a given
area. It's not yet properly implemented because it requires new
semantics which the eval implementation does not currently account for
(namely, significant/non-junk tokens which have no effect on
interpretation of tokens around them).

```s2-member
__FILE
__LINE
__COLUMN
```

Each resolve to the source code location where the symbol
appears. Note that they will behave weirdly when used in
"second-round" evaluation blocks (see
[eval](keyword-eval.md#keyword-eval-second-round) for
details).  `__FILE` may or may not be an absolute path (and might be a
virtual/synthesized name), as it resolves to the file name as it was
given to the C APIs. `__FILE` is a string. `__LINE` is a 1-based
integer, but `__COLUMN` is 0-based. *Because emacs does it that way.*

```s2-member
__FILEDIR
```

resolves to the directory part of `__FILE`, up to *and including* the
final separator (so that scripts to not have to determine the
separator themselves when pasting them back together).  It may resolve
to an empty string. Note that this is distinct from the process'
current working directory, and is intended primarily so that scripts
in subdirectories may more easily find any associated resources in
that tree. Note that `__FILE.substr(__FILEDIR.length())` will return
the base filename part of the script. Note also that the path is not
necessarily absolute, as it is derived directly from `__FILE`'s
string, and that is provided by the client.

```s2-member
__FLC
```

resolves to a string in the form
`script-name:line:column`, combining the `__FILE`, `__LINE`,
`__COLUMN` values. Particularly of use with  `s2out`
for debugging purposes.

```s2-member
affirm EXPR
assert EXPR
```

Both generate an error if the expression evaluates to `false`: `affirm`
throws an exception and `assert` triggers a fatal error.  Any `assert`
error is fatal in that it ends the script (regardless of its
file-level depth), not the client application running the script. In
either case, the affirmation exception's message property resp. the
assertion's C-level error string contains the whole text of the failed
expression, including any code comments found before the end of the
expression. The intention is that `affirm` be used for, e.g. function
argument validation and `assert` be used for Truly Serious conditions
and (potentially) unit testing. Script code cannot stop a failed
assertion from propagating up the stack, but it can stop exceptions by
using the `catch` keyword.

```s2-member
break [[-> | =>] EXPR]
```

A specialized form of [eval](keyword-eval.md) for breaking out
of loops. If no expression is provided, `undefined` is
assumed. `break` causes its operand to become the result value of the
closest enclosing loop (which must be in the same lexical vicinity -
`break` does not work across function call boundaries). Triggers a
fatal syntax error if used outside of a loop, but if that error
propagates through a function call boundary then it gets downgraded to
an `exception`.

```s2-member
catch [->] EXPR | {script}
```
An [eval](keyword-eval.md) specialization which catches[^17]
exceptions thrown from its operand. Described in [the eval section](keyword-eval.md#keyword-eval).

```s2-member
const IDENTIFIER = EXPR [, IDENTIFIER =…]
```

Works exactly like var (see below) except that a value part is
required and assigning to a `const` later on triggers an exception.
Note that the "constness" applies to the *variable*, not the value
it refers to - a `const` \[reference to a\] container value can
still be modified, in that its properties can be changed through a
`const` reference.

[Described in more detail below](#keyword-var-const).


```s2-member
continue
```

Starts the next iteration of the current innermost loop construct
(`for()`, `while()`, `do/while()`, or `foreach()`), skipping any
remaining code in the loop.

<a id="keyword-define"></a>
```s2-member
define(string symbolName[, value])
```

`define` resolves to a function which can be used to "define" symbols
which behave somewhat like keywords: they are looked up in the keyword
resolution phase, bypassing all scope lookups, and all custom defines
are stored in a dedicated hashtable, so access is faster than `var`
lookups. The first argument must be an identifier-format symbol, to
which the given value is immutably mapped. `define()` will fail if
passed a symbol that is already defined or would collide with a
built-in keyword. The second argument may be any value other than
`undefined`, which is reserved to allow...

Calling it with one argument will return the matching define or the
`undefined` value. That can be used to determine if a define is
already in place. Accessing it via its symbolic name if it's not yet
defined will trigger an exception.

When called with two arguments it returns its second argument. (It
might(?) make sense(?) to change that to return itself, so that
multiple defines can be done via chaining? There's not(?) a
*compelling* argument for any specific return-value semantics.)

`define`'d values cannot be assigned to and they effectively hide any
variables *previously* declared with the same name, in that the
defined value will always be looked up before `var`s are. Trying to
define a like-named `var` later will fail because the defined symbol
is effectively upgraded to a keyword and `var`s cannot share the same
name as a keyword. Because of that, defined values "really should" use
distinct names which are unlikely to be used by downstream code. e.g.
`"a"` is a name destined to Cause Grief, but
`"ADefineWithAnInterestingName"` is *probably* safe from collisions.

There is no way to "undefine" such a value, and adding any defines at
all impacts lookup times of non-keyword/define symbols somewhat (not
human-noticeably, but measurably), so they should be used judiciously.

The lifetimes of defined values belong to global scope, and they exist
until the scripting engine is shut down.

> Potential, but unlikely, TODO: allow constants to be un-defined by
explicitly passing (appropriately enough) `undefined` as the 2nd
argument. It's not yet clear whether this can be done in such as way
as to guaranty proper value lifetime if the value is removed from a
deeply-nested scope while an older scope is still accessing it. The
*potential* hiccup there is that defined values have data other than
Values associated with them, and that does does not tie in to the
Value lifetime mechanism. Removing a bit of it while a higher-level
call is still using it would lead to illegal memory access, i.e.
Undefined Behaviour. We "could" get around that by using a
`cwal_native` instance to bind each defined value, but that would be
a comparatively heavy-weight solution.


```s2-member
do {script} while(condition)
do EXPR; while(condition)
```

Implements the conventional (yet rarely-used) do/while loop. It
evaluates to `undefined` unless `break` is used to give it a result
value. The braces around the body are optional for single-expression
bodies, but such expressions require an explicit expression
terminator. An empty body/expression is legal, but take care to avoid
endless loops. e.g. `var x = 0; do ; while(++x<=3);` Described in
[the flow control page](keyword-flow-control.md).

```s2-member
enum [OptionalTypeName] {...}
```

Evaluates to a value of type enum, described in [the enum
    section](type-enum.md).

```s2-member
eval [-> | =>] EXPR | {script}
```

Evaluates script code or captures an expression as a string.
Described in detail in [the eval page](keyword-eval.md).

```s2-member
exception(message)
exception(integer|string code, message)
exception // NOT followed by a '('
```

The first two forms create (and resolve to), but do not throw, an
[exception object](type-exception.md) and capture the current script
location information and stack trace. If this keyword is *not*
followed by a `(` then it instead resolves to the exception prototype
object (where shared exception-type methods are stored).

Calling `throw exception(oneArg)` is equivalent to calling `throw
oneArg`, namely that the value of `oneArg` will be the value of the
exception's `message` property.  The two-argument form allows one to
provide a custom error code, stored in the exception's `code` property
(and coerced to an integer). If the two-arg form is passed a string as
the first argument, that string is interpreted as the name of a
C-level `CWAL_RC_xxx` or `S2_RC_xxx` enum entry name
(e.g. `"CWAL_RC_RANGE"`) and tries to convert it to a corresponding
enum integer value (falling back to 0 if it finds no match). If passed a code
of 0, it will use the code for the C-level `CWAL_RC_EXCEPTION` (which
is what `throw` uses by default). While this looks like a function, it
is not - it is a function-like keyword.

If custom integer values are needed, clients are recommended to use
any integer value greater than or equal to the C-level
`S2_RC_CLIENT_BEGIN` enum entry (5000), as those values are
*guaranteed* not to be used by either cwal or s2. That said, there is no
inherent harm in using *any* integer code values as exception codes,
but using values which unintentionally collide with `CWAL_RC_xxx` or
`S2_RC_xxx` values may potentially cause confusion.

See [the exception type docs](type-exception.md) for more information.


```s2-member
exit [[-> | =>] EXPR]
```

A specialized form of [eval](keyword-eval.md) which
"gracefully exits" the currently running script (even when called from
a subscript), unwinding the stack as it goes so that finalizers
anywhere in the call stack get called.

```s2-member
fatal [[-> | =>] EXPR]
```

Works like `exit`. The intention is that `exit` be used for
"friendly" exits and `fatal` be used for "unfriendly" exits.
Additionally, fatal notes the script name/line/column information in
the engine's error state (accessible from C, but not script code,
as this keyword ends script execution).

```s2-member
for(pre; condition; post) {script}
for(pre; condition; post) EXPR;
```

Implements a conventional for-loop. It evaluates to `undefined` unless
`break` is used to set its result. The braces around the body are
optional for single-expression bodies, but such expressions require an
explicit expression terminator. The loop starts a new scope at the
first parenthesis and another "inner" scope immediately after the pre
part, which gets re-initialized on each iteration. An empty
body/expression is legal, but take care to avoid endless loops.
Described in [the flow-control page](keyword-flow-control.md).

```s2-member
foreach( EXPR => x, y ) EXPR | {body}
foreach( EXPR => x ) EXPR | {body}
```

Iterates over object properties, array/tuple/hash/enum entries, and
(as of 20180620) characters in a string. Described in
[the flow-control page](keyword-flow-control.md).


```s2-member
function [name] (param list) {body}
```

This is an alias for `proc` (described below). In practice,
this keyword is rarely seen (typically only in code intended to be
read by those new to the language) because once one gets used to
writing `proc`, typing out `function` just feels… *barbaric*.


```s2-member
if (condition){...} [**else** [if(...)] {...} ]
```

Works as is conventional, except that its (condition) part may contain
any number of expressions, separated by semicolons, and the
conditional value is the result of the last one. An if/else block
itself evaluates to `true` if *any* `if` part evaluates to `true`,
else it evaluates to `false`. Note that `else` is only a keyword in
this context, and is treated as a normal identifier
everywhere... *else* (as it were). The braces around the body parts
are not required for single expressions, but a *semicolon is required
after each such expression*.  e.g. `if(1) a=1; else a=2;`

Described in more detail in [the flow-control
page](keyword-flow-control.md).

```s2-member
import([bool doPathSearch=import.doPathSearch,] string filename)
import(bool doPathSearchByDefault)
```

Imports and evaluates s2 script files using a configurable lookup
path, evaluating to the result of the script. Described in detail
in [its own page](keyword-import.md).


```s2-member
EXPR inherits EXPR
EXPR !inherits EXPR
```

The first form evaluates to `true` if the LHS *is* the RHS or if RHS
is in LHS's prototype chain, else evaluates to `false`. The second
form is a more convenient syntax for `!(X inherits Y)`. This keyword
is treated like a left-associative operator with comparison
precedence.

```s2-member
nameof IDENTIFIER
```

Resolves to the string form of the identifier if the identifier is
resolvable, otherwise it triggers a *syntax error* (not an
*exception*, though that is up for reconsideration). This has only one
known "real" (non-debugging) use: see
[Function.importSymbols()](type-function.md).

```s2-member
new ContainerWithConstructor(...args…)
```

Provides a common mechanism for instantiating new instances of
"classes" (using that term loosely!). Described in [its own
file](keyword-new.md).



```s2-member
pragma(...)
```

The `pragma()` keyword allows querying and tweaking of certain
s2-level state. See [the `pragma` page](keyword-pragma.md) for more
information.


```s2-member
proc [name] (param list) {body}
proc [name] (param list) {body} using (...)|{...}
proc [name] (param list) using (...)|{...} {body}
```

An alias for `function`, this keyword creates a Function object The
name `proc` is inherited from TCL (once one gets used to typing it,
typing `function` just seems tedious). See the [function page](type-function.md)
for more details.

```s2-member
prototype
```

Is only treated specially in the context of a property lookup or as
a property name in an object literal, where it resolves to the
prototype of the LHS value (or `undefined` if it has no prototype).
In all other cases it is a normal identifier.

```s2-member-deprecated
refcount EXPR
```

Removed 20191230 (it had been deprecated since the introduction of
`typeinfo()`). Replaced by [`pragma(refcount
EXPR)`](keyword-pragma.md#pragma-refcount).


```s2-member
return [[-> | =>] EXPR]
```

A specialized form of [eval](keyword-eval.md)
for returning a value from a function or from a given
script (if called from outside of a function). If no
expression is provided, the `undefined` value is assumed.

<a id="keyword-s2out"></a>
```s2-member
s2out
```

(Added 20191210.) Resolves to a function which sends each argument to
s2's default output channel, with no additional whitespace between
arguments and no newline after the final argument. It overrides the
`operator<<()` method to do the same. It behaves like the
older/optional `s2.io.output` method except that this object is
"sealed" - attempting to assign or clear its properties will trigger
an exception. This keyword is available regardless of whether the
`s2.io` API is installed.

```s2-member
scope [->] EXPR | {script}
```

A specialization of [eval](keyword-eval.md) which evaluates
script code in a new scope.

```s2-member
throw [->] EXPR
```
A specialized form of [eval](keyword-eval.md) which creates
and throws an exception, using the given expression's value as the `message`
property of the exception. It treats a `{...}` block as an object literal, not a
script code block.

```s2-member
typeinfo(TAG EXPR)
```

Queries meta-information about an expression's type or value.
Detailed in [the typeinfo page](keyword-typeinfo.md).


```s2-member
typename EXPR
```

(Formerly deprecated, but un-deprecated on 20191230.)

This keyword is functionally equivalent to `typeinfo(name EXPR)`.

If a container has or inherits a property named `__typename`, that
property's value is used by this query, otherwise some hard-coded
default is used based on the C-level type of the expression's result.

This is one of the few contexts where referencing an unknown
identifier will *not* cause an error - it results in the string
`"undefined"` instead. So `"undefined"===typename foo` can be
used to see if foo is declared, but it will also be `"undefined"` if
foo has the `undefined` value. Type name fetching cannot tell the
difference between is-not-defined vs. is-undefined, but
[`typeinfo(isdeclared)` and `typeinfo(islocal)`](keyword-typeinfo.md)
can.


```s2-member
unset IDENTIFIER | OBJ.PROP [, …]
```

Removes vars declared in the current local scope or object
properties. Fails with a syntax error (FIXME: exception, not syntax
error) for non-local variables, unknown variables, and anything which
does not look distinctly like a variable name or object property. In
the second form it only unsets properties directly in the given
object, not prototypes. Evaluates to `undefined`.  FWIW:
`unset` is not strictly necessary in s2 and rarely seen outside of
code which tests s2. It was initially added as a way to ensure that
garbage collection and related bits were doing the right thing (and is
still used in that capacity, but not for much else).

Potential TODO: make this work like a prefix operator (limitation:
only one param)? That might allow us to get at properties more easily.
e.g. `unset "".prototype.x` currently fails because `""` is not an
identifier, but we have operator-level infrastructure for doing that.

<a id="keyword-using"></a>
```s2-member
using()
using(EXPR)
```
Provides access to symbols [imported into function
instances](type-function.md#type-function-using-keyword) via the
`using` modifier of the `function` keyword.

```s2-member
var IDENTIFIER [[= EXPR], IDENTIFIER …]
var IDENTIFIER := EXPR [, IDENTIFIER …]
```

Declares variables in the local scope. If they are not provided a
value via assignment at their declaration-point then they default to
`undefined`. Evaluates to the last-defined variable's value.

[Described in more detail below](#keyword-var-const).

```s2-member
while(EXPR) {script}
while(EXPR) EXPR;
```

Runs a conventional while loop for as long as the EXPR part evaluates
to a truthy value. See [the flow-control page](keyword-flow-control.md)
for details.


<a id="keyword-reserved"></a>
## Reserved Keywords

Like most (all?) C precompilers, s2 reserves identifiers starting with
two underscores for its own use. It also reserves symbols starting with
"s2" or "S2", with one exception: clients are free to create a
global-scope symbol named "s2" to install the language's common APIs to.
(Historical trivia: in hindsight, "s2" should have been made a keyword
which the client could direct to their "main object," but it's far too
late for that now because of how that symbol is used in client-side
scripts.)

The following keywords are reserved for *potential* future use:

-   `and`, `class`, `delete`, `echo`, `implements`, `interface`,
    `is`, `isa`, `not`, `or`, `private`, `protected`, `public`,
    `static`, `try` (in case we decide we need/want one),
    `xor`.

Reminder: identifiers which appear in the context of a property access
(the dot op or `::` overloaded operator) or keys in object/enum/hash
literals are *always* treated as property keys, not keywords, so it is
safe to use reserved names in such capacities without concerns of
collisions/breakage if/when those keywords are added.

> Reminder to self: `static` could be implemented at the function
level by storing the static vars like we do imported (closure)
symbols. It turns out it's easy to simulate static data with [the
`using` function modifier](type-function.md#type-function-using) when
the function is defined. `static` could in fact be implemented off the
same basis, becoming a convenience form of that functionality. It
would have the slight advantage of not requiring initialization until
the function is called, but would have the disadvantage of requiring
re-tokenization on each call.


<a id="keyword-placement"></a>
# Placement and Precendence

Keywords (with rare exceptions like `inherits`) are essentially treated
as values, and may (generally) be used anywhere a value is expected,
though there are some differences from plain values in how they behave
with regards to commas on their LHS. Keywords which appear in the
context of a dot operator access or keys in object literals are
recognized as normal identifiers, not keywords[^18]. Thus property
names (but not variables) may be the same names as a keyword without
causing collisions.

s2 places no *arbitrary* rules on where each type of keyword may appear
(though some have *non-arbitrary* rules), so it allows many constructs
which are technically well-defined but arguably silly or nonsensical:

```s2
return var a = 3; // same effect as return 3, but costs a var decl
throw scope { var a=1, b=2, c = a+b }; // same as throw 3, but much more costly
!foo ? throw "hi" : return; // okay, that one might actually be useful[^19]
var x = proc(a = return 1){}; // will (only incidentally) trigger an exception when called
```

For convenience and conventions conformance, some keywords which
appear *on the far LHS* of an expression *and* end in a {script}
block will treat a subsequent newline character as an implicit
semicolon. (Alas, only these constructs do that: `catch`, `eval`,
`scope`, `if`, `for`, `do/while`, `while`, `foreach`.) Note, however,
that when such a keyword is used as the RHS operand of an operator,
the pending LHS expression still expects to see a semicolon/EOX to
terminate the expression. For example:

```s2
if(...) { … } // semicolon is implicit (and optional) here!
var x = while(...){...} ; // this semicolon is required for the LHS (var decl) part!
```

In terms of operator precedence, *most* keywords effectively start a new
expression, ignoring any pending operators to the left of the keyword
until the keyword has parsed all that it can. Those keywords which work
with {script} block process only that block, and not any operators
following that block. Keywords passed a non-{script} expression
(including those, like `throw` and `return`, which interpret `{}` as an
object literal) will consume as much of that RHS as they can, stopping
at a (special case) at a colon if a ternary-if operation is in progress,
but any higher-precedence operator in the RHS will be seen as part of
the keyword's expression. These examples demonstrate the differences:

```s2
true && 1 + 2 & 3; // ⇒ true && ((1+2) & 3)
1 + eval {1+2} & 3; // ⇒ 0 ⇒ (1 + (1+2)) & 3
1 + eval 1+2 & 3; // ⇒ 4 ⇒ 1 + (1+2 & 3)
true && eval 1 + 2, 4; // ⇒ true ⇒ true && (1 + 2, 4)
true && eval {1 + 2}, 4; // ⇒ 4 ⇒ true && (1+2), 4
eval 1+2, 4; // ⇒ 4 ⇒ ((1+2), 4)
true ? eval 1+2 : 4; // ⇒ 3
true && return 1, 3; // returns ⇒ 3
```

> Sidebar: `eval 1+3,3` equals 3. JS does it that way, but it also takes
explicitly takes a string as input, so it has implicit boundaries on the
expression which s2 does not have. The result, in this case, is the same
either way, but only incidentally. Also, `1 + eval 1 + 2, 5` is 6
because the RHS part of `eval` is not aware of the `+` operator on the
LHS, and consumes the comma as a list of expressions.

This is generally only of concern when keywords are used as a
non-rightmost part in operator chains, and it is recommended (for
sanity's sake) to keep one's use of keywords on the RHS or as standalone
expressions (or in parenthesized subexpressions).

Potential (but unlikely) TODO: make keywords aware of their pending LHS
operator, if any. Not sure if this would be an improvement, and may lead
to more confusion than the current mechanism (which is predictable,
though possibly not intuitive and not quite conventional (because most
languages do not allow most keywords to appear in such contexts)).


<a id="keyword-var-const"></a>
# Variables: `var` and `const`

The `var` and `const` keywords both declare symbols in the currently
active scope[^20]. The only differences are that `const` requires that
a value be assigned when the variable is declared and any attempt to
assign to or unset a `const` later will result in an exception,
whereas a `var` can be reassigned and `unset` (though `unset` only
works on vars declared in the local scope). Examples:

```s2
var x = 1, y, z;
assert 1 === x;
assert undefined === y;
assert 'undefined' === typeinfo(name z);
const c = 42;
assert 'CWAL_RC_CONST_VIOLATION' === catch{c = 1}.codeString();
// ^^^ throws because c is const
```

Note that "constness" applies to the *variable* (i.e. the key/value
pair), not its value. Passing a const variable's value up out of its
declared scope will "lose" that constness for later assignments unless
the symbol used to hold it is declared as `const`. It is possible (and
legal) to have both const and non-const references to the same value.

Variable names may not be the same as any keywords, including
pseudo-keywords created using [`define()`](#keyword-define), and
attempting to do so triggers an exception.

Likewise, declaring a given variable name a second time in the same
scope results in an exception.

As of 2020-02-18, the `var` keyword may be used to declare const
"variables" by assigning a value with `:=` instead of `=`:

```s2
var a  = 1  /* non-const */,
    b := 2 /* const */,
    c  = 3  /* non-const */;
```

Like `const`, variables declared that way require that a value be
immediately assigned to them. The `const` keyword supports the `:=`
notation as well, for consistency with `var`, but its behaviour is
identical with both `=` and `:=`. Thus:

```s2
var x := 3;
// is functionally identical to:
const x = 3;
// and:
const x := 3;
```

# Footnotes

[^16]:  That'd be me.

[^17]:  i didn't make up these terms, just sticking to long-standing
    conventions here. But "catch" is as good a word as any, i guess.


[^18]:  That's a slight oversimplification, but not far from the truth.
    In fact, only standalone identifiers and those on the LHS of a dot
    op are tagged as identifiers. Those on the RHS of a dot op get
    re-tagged as a different type to help the relevant operators do the
    right thing.

[^19]: It turns out that trying similar tricks with `&&`
    and `||` operators require special, sometimes non-intuitive, care to get
    the desired results. e.g. `1 && return()|| throw…` parses as `1 &&
    return (() || throw…)`. Might want to fix that someday, but it
    will require special-casing for recursion via keywords in the main
    evaluation routine. Then again, "fixing" that breaks `return
    (1)+2`. Hmmm. Or we make a rule that a keyword operand starting
    with '(' is treated like a function-style call to the keyword
    (like the [exception](type-exception.md) keyword does).

[^20]:  Remember that if/else/while/for/scope bodies each start a new
    scope (two, in the case of a for-loop).

[^21]:  s2 currently has no significant (non-whitespace/comment)
    constructs which are *not* expressions (except for the EOX, i.e. a
    semicolon, the end of a block construct, and *sometimes* a newline).
