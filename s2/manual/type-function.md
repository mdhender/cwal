# s2: Functions
#### ([&#x2b11;Table of Contents](./))
# The Function Type

Jump to...

* [Functions Introduction](#type-function)
* [Function Methods](#type-function-methods)
* [apply() and call()](#type-function-apply-call)
* [Closures: `importSymbols()`](#type-function-importsymbols)
    * [Nuances of `importSymbols()`](#type-function-importsymbols-quirks)
* [Closures Part 2:  `function() using`](#type-function-using)
    * [Nuances of `function() using`](#type-function-using-quirks)
* [Closures Part 3:  The "other" `using` (*keyword*)](#type-function-using-keyword)
    * [`function() using.` with a Dot](#type-function-using-dot)
* [Tips and Tricks](#type-function-tricks)
    * [`using` with Default Parameter Values](#type-function-using-default-params)
    * [Making non-Functions Callable](#type-function-callable)

<a id="type-function"></a>
# Functions

Functions are first-class values which are able to be "call()ed".
Function call syntax uses the conventional parenthesis list:
`func(arg1,...argN)`[^1] resp. `obj.method(...)`, but the LHS may be an
expression (as opposed to being a simple identifier). It is a runtime
error (an exception, not a fatal syntax error) to have a parenthesis
group immediately after an expression which does *not* resolve to a
callable value (meaning any value which is a function or inherits one)
or is a function-like keyword
(e.g. [`typeinfo(...)`](keyword-typeinfo.md)).

Functions are created in script code using the `function` keyword or its
TCL-inspired alias `proc`[^39]. Functions are first-class values and
naming a function simply means assigning it to a variable or property:

```s2
const f = function(){...};
obj.f = proc(a,b){...};
// proc, adopted from TCL, means exactly the same thing
// as function but costs only 4 bytes and takes literally
// half as much time to type! :-D

// Define and call at the same time:
proc(...){...}( ...args… );

// A name part is optional, and can be used in recursion:
const x = function myfunc(){ … };
// The name part ^^^^^^ has a JS5-like meaning - see below.
```

A function's parameter list may include anything found in a
`var` declaration, including default values for unspecified
parameters:

```s2
var f = proc(a, b = 2, c = 7) { return a + b + c };
```

Functions do not have implicit return values - use the `return` keyword
to return a value. A function call which neither throws an exception nor
explicitly returns a value effectively returns the `undefined` value.

Parameters lists are evaluated *on each call*, so default value
expressions may arbitrarily complex and may be modified by changing
symbols they refer to. Default parameter values for which arguments are
passed by a caller get evaluated in short-circuit mode. i.e. they have
no side effects. So this weird construct can force a caller to supply
one or more arguments:

```s2
var f = proc(a=(throw 'gimme an a!'), b=1){
 // alternately: (a=(affirm argv.length()), argv.0)
 affirm 1===b; // b's default was processed
 affirm -1===a; // our argument got through
};
f(-1);
f(); // will throw
```

Inside of a script function call, the following call-local variables are
set:

-   **argv** is an array holding the passed-in arguments (irrespective
    of the declared parameters). `argv.length()` (or `argv.#`) returns
    the number of arguments passed in. Note that assigning a value to
    a named parameter does not change the value of the corresponding
    `argv` index, nor vice versa. The values in the named parameters are
    captured at the time the function is called, at which point they
    become independent of the `argv` array.
-   Its **optional name**: If the function is declared with a name
    (e.g. `function name(){...}`) then that name gets declared as a
    local variable referring to the being-called function, as if it
    had been defined in the function's body (not in the scope
    *defining* the function).  The name (if any) of a function is
    stored internally, not as a property, and is *not* cleared by the
    inherited `clearProperties()` method.
-   **this** works slightly differently than is conventional: For a
    call in the form `obj.func()`, `this` is a call-local
    variable/expression referred to here as `obj`. When a function is
    called without a property access, the function gets set as its own
    `this` value. It is thought that that approach is less confusing
    than having to struggle with the "which this is this?" question,
    as well as be more consistent (in that it's always clear what
    `this` is in a given lexical context, except perhaps in some
    hypothetical corner cases involving script code triggering native
    functions which then eval script code containing a `this`
    reference). Function-local variables can be used to alias the
    local `this` for use inside inner-function calls.
-   Any symbols imported via the `using` keyword (described later on
    in this chaper) or the `importSymbols()` method (ditto) are added
    as local variables. Assigning over them in the function's body
    *does not update the imported references*, but any modified
    *member properties* of such symbols remain modified after the
    function call completes.

Those variables are set up *before* the parameter list is processed, and
are accessible via default parameter values:

```s2
var x = function func(a = func.defaults.0, b = func.defaults.1){
 assert typeinfo(isfunction func); // just to demonstrate
 return a + b;
};
x.defaults = [1,-1];
assert 0 === x();
assert 1 === x(2);
x.defaults.0 = 2;
assert 1 === x();
```

Whether that's a feature or a bug is left for the user to judge.

Parameter lists and bodies are not parsed until a function is called, so
any syntax errors they may contain will not be triggered until then.

> Potential TODO: a syntax check on the parameter list at decl-time
would incidentally give us what we need to record the function's
declared arity, a feature i would like to experiment with in function
dispatching. This would require a minor bit of refactoring, since we
currently completely skip over the parameters when in short-circuit
mode (which is used during function declaration to "get past" the
function without evaluating it).) We'd need to add a counter to the
`var` collector which also counts in skip-mode (i.e. when
short-circuiting)

<a id="type-function-methods"></a>
# Function Methods

(Noting that each method is itself a function... with these same
methods... which are themselves functions... with these same
methods... *Gaaah!* Try not to think about it!)

The Function prototype inherits the base Object prototype and adds the
following methods:

```s2-member
mixed apply(Object [, array args])
```

Runs this function with the first argument as the 'this' value and the
second argument (empty or an array) as the call arguments.  Returns
the result of that call. Described in detail [in the next
section](type-function.md#type-function-apply-call).

```s2-member
Function bind(mixed theThis)
```

Returns a new function which wraps the being-called function but
always binds `theThis` as the call-time `this`.

> Design-note: bind() was the first-ever implemented-in-script method
introduced in the main API. (Try `print(print.bind.sourceCode())`).

```s2-member
mixed call(Object [,arg1…, argN])
```

Works identically to `apply()` except that it takes its arguments as
the 2nd and subsequent arguments instead of in an array. Passing an
array here will pass that array, not its contents, to the function
being `call()`ed. Described in detail [in the next
section](type-function.md#type-function-apply-call).

```s2-member
Function importSymbols([bool clearOldProps=true,] Object|String [, … Object|String])
```

Imports symbols into the function, such that they will be defined as
call-local variables when it is called. Described in [a following
subsection](#type-function-importsymbols). Returns this function. If
the first argument is of type bool then it is treated as a flag
specifying whether to clear any existing properties before importing
the new ones.

This function's primary role has, in the meantime, been filled by
the `using` function-creation modifier.

```s2-member
string|undefined sourceCode()
```

For script-defined functions, this returns the function's complete
source code. For functions defined in C code it returns `undefined`.
This is for information purposes only, and is not intended to be used
for storing and loading function definitions (and doing so may or may
not work for any given function).

*Special case*: empty functions (with empty params list *and* bodies
(not counting comments and spaces) and no `using` modifier) do not
save their source code (as an optimization), and for all such
instances this function returns (synthesizes) the shortest possible
string representation of an empty function: `"proc(){}"`.


<a id="type-function-apply-call"></a>
# Let's get `this` straight: `apply()` and `call()`

The `apply()` and `call()` functions, as in JavaScript, allow one to
specify the `this` value in a given function call. These functions work
identically except in how they take their arguments:

```s2-member
someFunc.call(theThis, 1, 2, 3); // equivalent to:
someFunc.apply(theThis, [1, 2, 3]);
```

Both return whatever the function would normally return. Both allow the
2nd (or subsequent) arguments to be elided for a no-argument call, so
`func.call(obj)` and `func.apply(obj)` are equivalent.

Unlike in JavaScript, it is legal to pass a non-container value as the
first argument to `call()` or `apply()`, e.g. passing an integer or
even the `undefined` value is permitted and has the same effect as
passing a container. (In JS, passing `null` uses the global-level
`this`, which, in browsers, is the `window` object.)

Note that passing a specified `this` to a native (C-side) function, as
opposed to a script-side function, might or might not behave as the
caller intends - it depends on whether/how the function uses `this`. In
particular, when going through multiple levels of function indirection,
`this` may not be the same at the deepest points in the call stack. That
said, most C-level functions behave intuitively (which might mean
throwing an exception) when used this way. Some even apply different
semantics depending on what type their `this` is. cwal's API allows such
bindings to type-safely determine if a given Value is of a given type
(even a client-supplied Native type about which cwal knows nothing), so
it "simply doesn't happen" that native C functions mistake the concrete
C type of the `this` they are given. Worst case, they trigger an
exception, as opposed to misusing/mis-casting a void pointer.


<a id="type-function-importsymbols"></a>
# Closures Part 1: `importSymbols()`

Note that the `importSymbols()` feature described in this section has
since largely been obsoleted by the newer [`using` function-creation
modifier/pseudo-keyword](#type-function-using), which does effectively
the same thing but is a language-level built-in feature. Nonetheless,
`importSymbols()` is retained (not deprecated) because:

- It has at least two obscure uses which the `using` modifier cannot
    fulfill:
    1) <s>changing the list of imported symbols on a previously
    created function.</s> (That feature was added via [the
    `using` *keyword*](#type-function-using-keyword) in Dec. 2019)
    2) importing properties from a non-literal object. (`using` can
    do that as well, but only in one specific context.)
- Its name is far more search-friendly than "using" is.

This section covers `importSymbols()` and [the next covers the `using`
clause](#type-function-using).

It is often useful for a function to have access to symbols from
arbitrary places, and yet be able to use them after those symbols are
gone (or might have been re-purposed with different semantics
altogether). This feature is generally known as "closures," and is
used in many scripting languages as well as some higher-level
languages (Java has some support for it).

s2 supports "manual closures," in that the client has to "manually"
import the symbols he wants to make persistent inside his
function. This is best shown with an example:

```s2
const f = proc(){return x};// x will be imported below...
scope {
  const x = 1;
  f.importSymbols(nameof x);
  // equivalent: f.importSymbols('x');
  // equivalent: f.importSymbols({x: x});
} // the var x is destroyed at the end of the scope, but f() still holds
  // a reference to its value!
assert 1 === f()
   /* because f holds a reference to x's value at the time
      importSymbols() was called. */;
```

Sidebar: the reason for the "manual" part of closures in s2 is: s2's
parsing model is *extremely* memory-light and does not hold enough state
at any one time to know whether a function uses symbols from other
scopes. (Even when running the function, we don't actually know *where*
any resolved symbols come from.) Thus it cannot automatically bind
symbol references. Even if it could/did, the basic var/property model it
uses could not support holding assignable references to such symbols
(JS-style).

`importSymbols()`, by default, resets the list of imported symbols on
each call, but it is (in practice) generally only ever called once per
function instance at the point where the function is defined. It
accepts any number of arguments, any of which may be:

-   A string is interpreted as the name of an identifier (but the quotes
    are required because `importSymbols()` is a function, not a
    keyword), and the *current* value of that symbol is imported. An
    exception is triggered if the symbol cannot be resolved.
-   An object: the key/value pairs of the object are imported, such that
    the object keys become the imported symbol names. Numeric keys will
    (silently) not work in this context because trying to use them as
    variables will instead cause them to be parsed as numeric literals:\  
    `proc(){print(3)}.importSymbols({3:'three'})()`\  
    outputs *3*, not *three*.
-   If the *first* argument is a boolean then it specifies whether or not
    to reset any already-imported properties. Passing `true` is equivalent
    to the default behaviour and passing `false` tells it to retain any
    existing properties.

That second option can be used to effectively rename or alias symbols,
as well as create private data with no script-visible reference outside
the function's body or parameter list:

```s2
f.importSymbols({ a: x, p: print, z: some.other.thing, mine: {...} });
```

> Sidebar: "no script-visible reference" is not quite true since the
`using` keyword was added, but one has to go actively looking for that
reference in order to see it.

`importSymbols()` returns the function it is operating on, so it can be
(and normally is) used at declaration time like this:

```s2
var f = function(){…}.importSymbols(…);
// or called directly:
function(){…}.importSymbols(…)( …args… );
```

To emulate a castrated version of JavaScript's "with" (mis)feature:

```s2
assert 3 === proc(){return a+b}.importSymbols({a: 1, b:2})();
```

Importing symbols from faraway scopes, or deeply-embedded objects, can
improve performance by reducing the distance the function has to look
for symbols it resolves, as well as avoid accidentally resolving other
like-named symbols which shadow the ones we're really interested in,
but it also has the cost/overhead of storing the imported symbol and
declaring it as a local variable on each call.

Last, but not least, `importSymbols()` can be used to set up "static"
function data, which should be initialized only once:

```s2
const f = proc(k,v){
 const argc = argv.length();
 (1===argc) && return h \# k; // or: h.search(k)
 (2===argc) && return h.insert(k,v);
 return h;
}.importSymbols({ h: new s2.Hash(117) });
```

Remember that initialization of object properties can be arbitrary
expressions, meaning that such initialization may be arbitrarily complex
and wrapped in a `scope` block (remember that keywords like `scope` are
expressions).

An alternate approach which delays instantiation of the cached data
until the first call (so it costs less if the function is never called,
but has the extra overhead of an outer wrapper object if it is):

```s2
const myCache = proc(k,v){
 const argc = argv.length();
 cache.h || (cache.h = new s2.Hash(117));
 (1===argc) && return cache.h # k; // or cache.h.search(k)
 (2===argc) && return cache.h.insert(k,v);
 return cache.h;
}.importSymbols({ cache: {} });
```

And, finally, here's another alternative which does not use
`importSymbols()` and does not cost an extra object, but has the
side-effect that the cached symbol becomes visible to the client as a
property of the function after it is called one time:

```s2
const myCache = proc cache(k,v){ // note the name added to the declaration
 const h = cache.h ||| (cache.h = new s2.Hash(117));
 const argc = argv.length();
 (1===argc) && return h # k; // or h.search(k)
 (2===argc) && return h.insert(k,v);
 return h;
};
```

<a id="type-function-importsymbols-quirks"></a>
## `importSymbols()` Nuances and quirks

-   Imported symbols get declared as local variables during
    initialization of the function call and get re-declared, using their
    `importSymbols()`-time values, each time the function is called.
    Assigning to them from within the function will have no lasting
    effect because they are (in that context) simply local variables
    which are discarded at the end of the call. Note that the
    initially-imported values themselves persist, so properties stored
    in imported *containers* will persist between calls.\  
    Design note: making them re-assignable cannot work in the tiny world
    of s2's evaluation engine. Hmmm… we "could", in the post-call()
    hook, iterate over all imported properties and copy their values
    from the scope, unsetting them if they're not longer in the scope
    (i.e. were unset there). It'd be costly for an rarely-used case, and
    would not behave like JS would. In JS, such an assignment affects
    the original, regardless of how many inner functions use the symbol.
    With the above proposition, the assignment would still be
    function-local but would also be persistent. OTOH, that is already
    possible by having a function call `importSymbols()` on itself (or,
    in the meantime, via `using.theSymbol` keyword).
-   By default the list of imported symbols is cleared/reset each time
    `importSymbols()` is called, so all symbols have to be imported at
    once. If passed `false` as its *first* argument, it amends the
    existing symbols, without clearing them first. Passing `true` as the
    first argument is the same as passing not passing a boolean first
    argument (i.e. previous imports get cleared).
-   It is possible to change imported values by calling
    `importSymbols()` on the function while it is being called, but in
    practice would be a highly usual case (except maybe to re-import
    local changes to imported symbols… hmmm…). Note, however, that
    doing so would not change the currently active symbols - the
    change would take effect on the next call. (Maybe that has
    potential uses involving recursion?)
-   Imported symbols are stored internally as part of the
    Function. Calling the inherited *clearProperties()* method on a
    Function value *will not* (as of 20160206) remove imported
    symbols. (Prior versions did, because of how the symbols were
    stored.) Prior to 201912, it was not possible to access imported
    symbols except via the call-local variable names, but the `using`
    keyword now provides access to them.

Sidebar: here's a contrived example of using `importSymbols()` in the
body of a function to retain state between calls:

```s2
const f = proc ff(a = x, b = y){
 ff.importSymbols(false,{x:a, y:b});
 return a + b + z;
}.importSymbols({x:1, y:2, z:3});
assert 6 === f();
assert 7 === f(2);
assert 7 === f();
assert 4 === f(5,-4);
assert 4 === f();
```

(Noting, however, that [the using *keyword*](#type-function-using-keyword),
as opposed to the `using` function creation modifier, which was added in 201912,
can do the same without the overhead of a script-side function call.)

Whether or not that is actually useful is debatable.

<a id="type-function-using"></a>
# Closures Part 2: `function() using(…) {}`

This feature was added on 2016-03-12 and obsoletes most (but not all)
uses of the `importSymbols()` method.

Because [`importSymbols()`](#type-function-importsymbols) is so
integral to many routines, and yet requires a client-side definition
of its *own* symbol in order to be used (e.g. it is not available in
s2sh's "cleanroom mode"), its functionality was eventually added as an
in-language construct in the form of an extension to the
`function`/`proc` keyword syntax:

```s2
proc(...) using(...) {...body...};
// or (equivalent):
proc(...) {...body...} using(...);
```

The using clause has two forms: `using()` and `using{}`. They are subtly
different but perform the same job:

-   `using()` (with parentheses) accepts a list of arguments: either
    identifiers which refer to resolvable symbol names or object
    *literals* which define key/value pairs to import. i.e. it works like
    `importSymbols()` except that where `importSymbols()` takes strings
    in place of identifier names, `using()` takes *identifiers*.
-   `using{}` (with squiggly brackets) treats its `{}` as an object
    literal. This approach is generally more flexible because it allows
    the caller to easily alias compound symbols for use in a function
    body, e.g. `{importedName: some.object.property}`.

> Sidebar: the reason for two syntaxes is more or less historical. At
the time `using()` and `using{}` were added, s2 did not yet have the
ability to create object literals with the "shorthand" syntax, e.g.
`{a,b,c}` was not legal, so `using(a,b,c)` filled that role. Since
the object literal shorthand syntax was added, the `using()` form has
mostly (but not entirely) fallen out of use.

The following are all functionally equivalent:

```s2
const a = 3;
function(...) using(a) {...};
function(...) {...} using(a);
function(...) {...}.importSymbols('a');
function(...) {...} using{a};
function(...) {...} using{a: a};
```

The end effect of all of those is identical, but `using` is parsed at
the script-evaluation level whereas `importSymbols()` requires a
function call. See the following subsection for a more detailed
description of the differences between `using()` vs. `using{}`.

The `using` clause of a function may be placed immediately before or
after the body, but not both. In practice, short lists of imported
symbols are typically placed before the body and long/complex lists
are placed after the body, but it's a matter of personal preference
and readability. The placement of `using()` after the body (its
"postfix" form) was initially intended to simplify migration of script
code from `importSymbols()` to `using()`[^40], but practice has
shown that whether the postfix or infix form is more readable may very
well depend on the code context, e.g. long inlined objects in the
`using()` parameters tend to be more readable with the postfix form,
whereas short lists are often more readable with the infix form.

The `using()` clause runs in the same scope as the function definition,
as opposed to a new scope created just for it. This is in contrast to
`importSymbols()`, whose arguments are (like all s2 script-side function
calls) evaluated in its newly-pushed call-time scope. `using{}`, on the
other hand, evaluates like an object literal, so its body is evaluated
in its own scope.

`using()` and `importSymbols()` both treat non-identifier resp.
non-string-literal arguments identically except that error reporting may
differ because they happen in different phases of evaluation:

```s2
const a = 3;
function(...) using(a, {b: a*2}) {...};
function(...) {...} using(a, {b: a*2});
function(...) {...}.importSymbols('a', {b: a*2});
function(...) {...} using{a, b: a*2};
```

> Sidebar: `using`'s contents actually get evaluated *after* the body
token is read (whether or not it appears before the body), which means a
tokenization error while reading the body may (depending on the exact
error) trigger before an error in the `using` part gets a chance to.
Unlike the function's body and parameter list, which get evaluated each
time the function is called, the `using` part is only evaluated once,
when the function is defined.

Note that `using{...}` only accepts a single *object* literal,
not a *hash* literal nor an expression which evaluates to an object. To
import symbols from a non-literal object (via an expression which
evaluates to a container), add another layer of parentheses around the
expression, e.g. `using((object expression))`. An explanation for why
the extra parentheses are needed can be found in the next subsection.


<a id="type-function-using-quirks"></a>
# Nuances of the `using` Modifier

While the above section demonstrates all one really needs to know in
order to make use of `using`, it is helpful to understand a bit more
about its syntactical limitations and syntax for working around them.
Specifically, `using` allows the following syntaxes:

First:

```s2
using {object: literal}
```

That is, in practice, the most common usage, either importing multiple
symbols or aliasing a "longer" symbol to a shorter name, e.g. `using
{func: some.obj.func}`.

Second:

```s2
using (list of identifiers and expressions starting with a non-identifier)
```

This means that `using(x)` and `using((x))` behave differently: the
first one imports the symbol `x`, regardless of its value's type, into
the function, whereas the second form evaluates the *expression* `(x)` and
expects it to resolve to a container from which `using` will import any
properties.

> Sidebar: `{...}` vs. `(...)`: the `{...}` syntax can *almost* always be used
in place of the `(...)` syntax. When `using` was first added to the
language, s2 did not yet support "shorthand" object literal syntax, e.g.
it required `{a:a, b:b}` instead of `{a,b}`. Since the addition of the
shorthand object literal syntax, the `using(...)` syntax is, for most
purposes, effectively obsolete because `{...}` can import the same
declarations just as succinctly. That said, there are some rarely-seen
(if ever seen) use cases `using(...)` allows which `using{...}` does not, as
in this next example…

Because of the parser's limitations, expressions like
`using(x.prototype)` are illegal, but that can be worked around with
`using((x.prototype))`, which will import each of the properties
contained in the resulting container-type value. The limitation here is
how s2 examines tokens: it doesn't have enough information to know (with
a reasonable amount of effort), whether the identifier `x` is a prefix
for a longer expression. Thus, if it sees an expression starting with an
identifier, it assumes that it is only an identifier and tries to import
it into the function. If it turns out that the identifier was just a
prefix of a larger expression, the next parsing step will fail because
the expression is incomplete (and, in all likelihood, syntactically
invalid). Example:

```bash
s2sh> proc(){} using(x.prototype)
rc=2009 (CWAL_SCR_SYNTAX): s2_engine says error #2009
  (CWAL_SCR_SYNTAX) @ script [shell input], line 1, column 16:
Unexpected token '.' in 'using' list.
```

What happens there is that `using` imports the identifier `x`, then
tries to move on to the next token, and chokes when it sees a `.`
starting (in its limited view) a new expression. Adding another set of
parentheses around `(x.prototype)` works around that. Note that
`using(2.prototype)` does not have that behaviour because `2` is not
an identifier, so `using` skips its "looks like an identifier" logic
and proceeds to import all symbols from that prototype into the
function!  (Again, this is a side effect of s2's parser being so
memory-light, i.e. lacking enough state to make such decisions in
advance like a Real Language would (noting that such state would cost
*at least* ten times as much memory, if not 100x, as s2 currently uses
for parsing).)


<a id="type-function-using-keyword"></a>
# Closures Part 3: Accessing Imported Symbols with the `using` *Keyword*

Prior to [2019-12-10](/info/a009b47e67e1e5db), symbols imported into
functions via the `using` function modifier or
`Function.importSymbols()` were *only* accessible as call()-time local
variables inside those functions. The [`using` standalone keyword](keywords.md#keyword-using) (as
opposed to [the `using` modifier of the `function`
keyword](#type-function-using)) provides an alternate approach:

-   In the body or parameter list of a script-defined function,
    `using()` or `using` (without parens) both resolve to the imported
    symbols for the current function. If called with no arguments, or
    without parentheses, from outside a script function, a syntax
    error is triggered.\  
    For brevity's sake, when this documentation says `using()`, it's
    referring to both equivalent forms, with and without the empty
    parenthesis.
-   Outside of a script function body/parameters, imported symbols may
    be accessed via `using(expr)`: if `expr` resolves to a script
    function, the keyword resolves to its mapping imported symbols (an
    Object), else an exception is thrown.
-   In both cases, for a script function with no imports the keyword
    resolves to the `undefined` value. Note that
    `Function.importSymbols()` can be used to install imports after
    creation of a function which initially has no imports. As fate would
    have it, `function()using{}{…}`, with an empty `using{}` clause,
    can be used to install an empty imports object, but
    `function()using(){…}`, with an empty `using()` clause, triggers
    a syntax error. The latter was an intentional design decision but
    the former is a happy accident: at the time that was implemented it
    didn't seem worth the trouble to verify that the object-literal body
    actually contained any properties (and now, it turns out, that's
    a feature, not a shortcoming).
-   Like most function-like keywords, this one processes its argument
    (if any) in the calling scope, which means, e.g. that it's possible
    to declare local variables inside the parenthesis like can be done
    with `typeinfo()`. Whether or not that's a feature or a bug is as
    yet undecided.
-   It gets its name from the `using` modifier to the `function`/`proc`
    keywords, as it provides access to the values which that modifier
    (or its older counterpart, `importSymbols()`) installs.

***Minor Achtung:*** imported symbols get defined as scope-local
variables on each call to the function. Those symbolic names refer to
to different *references* than symbol names accessed via `using()`.
Normally this is not an issue, but beware that *modifying* imports via
scope-local variables only modifies those variables, not the imported
symbols, and the changes only persist until the end of the call. To
make changes which last beyond the current call, the symbols must be
accessed via `using.SYMBOL`. For example:


```s2
const f = proc() using{a:100} {
    assert a === using.a; // same value, different references
    ++a; // modifies only the scope-local variable
    using.a += 2; // modifies the imported var directly
    assert 101 === a; // the scope-local var
    assert 102 === using.a; // the imported symbol
};
f();
assert 102 === using(f).a;
```

On the next call to that function, the scope-local variable `a` will
start out with the value... *102*. Why? Because it was set to 102 via
`using.a` on the call shown above, and that imported symbol gets
assigned as a call-local/scope-local variable on each call. The
modifications to that scope-local copy disappeared at the end of the
call, but those made to `using.a` were retained, which also means they
become the value for the scope-local `a` on the next call.

This dichotomy of symbols can be avoided altogether by making use of
the feature described in the next section...

<a id="type-function-using-dot"></a>
## `function() using.` with a Dot

In the context of adding [the `using`
keyword](#type-function-using-keyword) to the language, [`function()
using`](#type-function-using) was slightly expanded: adding a period
between the `using` and `()` or `{}` part of the clause indicates that
the function should *not* declare imported symbols as local variables
at call()-time, leaving them to be resolved *only* via the `using`
keyword instead. That modification applies to symbols imported via
`function() using` as well as to any added in the future via the
`Function.importSymbols()` method (because imported symbols are all
stored in the same way, with no internal distinction between the
source of such symbols). For example:

```s2
proc() using. /* ⇐ NOTE THAT PERIOD! */{a: 1}{
  assert !typeinfo(islocal a);
  assert 1 === using.a;
}
```

**Mnemonic:** `using.` is how the symbols will be accessed in the
function body.

Without the `.` between `using` and `{a:1}`, the symbol `a` would be
declared as a local variable each time the function is called and the
first `assert` would fail (the second `assert` would be unaffected).

The primary benefits of applying the `.` modifier are efficiency and
performance (though both gains are, admittedly, slight). With this
modifier applied, calling the function will no longer have to define
the local variables imported this way. The immediate implication is
fewer memory allocations, but a less obvious one, perhaps, is that the
lookup speed of *other* local symbols (which includes function
arguments) _may_, on average, perform a tick faster because there are
fewer scope-level symbols to search through. The `using` identifier in
a function body does not require a scope-level symbol lookup, but
instead requires a keyword lookup, which is both computationally
faster (O(1) vs. (depending on compilation options) _amortized_ O(1)
or O(N/2) (N=number of variables in the scope)) and functionally
faster because it has to go through fewer levels of
abstraction/function calls than comparing property/variable keys does.


<a id="type-function-tricks"></a>
# Tips and Tricks


<a id="type-function-using-default-params"></a>
## `using` with Default Parameter Values

When a function is called, any imported symbols are applied before the
parameter list is processed. This means that default parameter values
may refer to imported symbols. This also means that such default values
may be modified by modifying the imported symbols.

For example:

```s2
var f = proc(a=x) using{x: 1}{ s2out<<a<<'\n' };
f(); // outputs 1
using(f).x = "hi";
f(); // outputs "hi"

f = proc(a=using.x) using{x: 1}{ s2out<<a<<'\n' };
f(); // outputs 1
```

<a id="type-function-callable"></a>
## Making non-Functions Callable

It is possible to make non-functions callable as if they were
functions, though how to do it is probably not intuitive. This is
possible due to the following rules:

-   When a function is called in a non-property context, the function
    (more correctly, the called value, which might *inherit* a function)
    gets set as its own "this." (In property-access contexts, the LHS of
    the property access operator is always "this" unless
    `Function.bind()`, or similar, is used to redefine it.)
-   When a non-function value is called as a function, its prototype
    chain is searched for a function. If found, that function is called,
    but the original LHS value is retained for `this` purposes.

That allows us to do the following:

```s2
const fProto = function callee(){
 affirm this inherits callee
   /* ^^^ make sure it is being called in the right context */;
 affirm this !== callee /* do not allow it to be called standalone */;
 s2out << 'from ' << this.name <<' ' << argv << '\n';
 // note the this.name ^^^^^ property access
};

const obj1 = {
 prototype: fProto,
 name: 'object #1' // refered to the fProto function
};

const obj2 = {
 prototype: fProto,
 name: 'object #2'
};

assert obj1 inherits fProto;
assert obj2 inherits fProto;
obj1(1,2,3);
obj2(3,2,1);
```

When run, that outputs:

```bash
from object #1 [1, 2, 3]
from object #2 [3, 2, 1]
```

Note, however, that that approach breaks somewhat if the callable
objects are called via *property access*:

```s2
obj1.foo = obj2; // noting that obj2 is callable
obj1.foo(4,5,6);
```

Outputs:

```bash
from object #1 [4, 5, 6]
```

Why? Because `this` (normally) resolves to the LHS of a function
called via a property access operator (`obj1` in this case), so that's
the object being referenced via the function call operation. It is
possible to bind a specific `this` to such a call, such that a
callable object always uses a specific object inside the overridden
function, using `Function.bind()`, [the `using`
modifier](#type-function-using), or
[`Function.importSymbols()`](#type-function-importsymbols), and an
example of doing so can be found in [this
script](/finfo/s2/toys/funcbind.s2) (pun not intended).



# Footnotes

[^1]: Arguments to functions, like all other list-like constructs in
    s2, are evaluted in the order they appear in the source.

[^39]: Once one gets used to typing `proc`, typing `function` starts
    feeling tedious and barbaric.

[^40]: Simply replace `.importSymbols` with ` using`. Regex:
    `s/\.\s*importSymbols\b/ using/g`
