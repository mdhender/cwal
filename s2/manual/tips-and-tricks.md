# s2: Tips and Tricks
#### ([&#x2b11;Table of Contents](./))
# s2 Tips and Tricks

This page describes various random tips and tricks for using, and
making more effective use of, s2.

Jump to...

* [The s2 Mindset](#tips-s2-mindset)
* [Non-obvious Differences from JavaScript](#tips-s2-is-not-js)
* [Editing s2 Script Code](#tips-editing-script-code)
* [Use `importSymbols()` resp. `using` for Inner Functions](#tips-importsymbols)
* [Descriptive Assertions](#tips-descriptive-assert)
* [Relative Performance of Various Common Ops](#tips-relative-op-costs)
* [Don't Need a Prototype? Remove it!](#tips-remove-prototype)
* [Parsing Efficiency](#tips-parsing-efficiency)

<a id="tips-s2-mindset"></a>
# The s2 Mindset

While s2 superficially looks like JavaScript, and does indeed have
much in common with it, it *does* behave differently (more on that in
[a separate subsection of this chapter](#tips-s2-is-not-js)) and can
be used more effectively if one thinks not in terms of a
JavaScript-like language, but in terms of an expression evaluation
engine (with, incidentally, a JS-like syntax).

Keep in mind that keywords, like `return` and `throw` and even
`if`/`for`/`while`, are all (unlike in JS) expressions and can be used
anywhere any other expression is allowed. A variable declaration is -
you guessed it! - *an expression*, which makes it possible to optionally
declare a variable or not (which is rarely useful, but interesting
nonetheless ;), as in this example:

```s2
foo() ? const x = 3 : false /* any useless value will do here */;
// equivalent to:
foo() && (const x = 3);
// NOT equivalent to:
if( foo() ) const x = 3; // x lives in the if()'s scope!
```

In the first two examples, if `foo()` evaluates to a truthy value, `x`
gets declared in the current scope, otherwise it does not. This is
admittedly not a generally useful trick, but it demonstrates a primary
difference between s2 and most other languages it conceptually derives
from or resembles, namely that it treats keywords like expressions (the
exception is `inherits`, which behaves like an operator because it sits
between its operands (a.k.a. infix)).

This is somewhat unconventional, but once one gets used to the idea, it
allows constructs which other languages don't because of their strict
separation of, e.g. keywords vs. function calls. s2, on the other hand,
allows (because keywords are expressions) an `if`/`else` block to be a
function argument:

```s2
print(if(0) 0;else if(0) 0); // outputs "false"[^62]
print(if(0) 0;else if(1) 0); // outputs "true"
```

(Remember that an if/else block evaluates to `true` if any of its `if`
conditions evaluates to `true`, else it evaluates to `false`.)

Not that one would really want to use that particular construct, but
there [are more sensible uses](/info/4e51df52534b9ca97b36e414dd3ed8a1a5319d53?ln=61-62).


<a id="tips-s2-is-not-js"></a>
# Non-obvious Differences from JavaScript

s2 has a lot in common with JavaScript, at least cosmetically. Its
data type model is directly derived from JSON, which is a subset of
JavaScript. Nonetheless, s2 has some features which superficially
appear to be similar to counterparts in JavaScript but which behave
differently, or appear to work the same but do so very
differently. Here is an overview of things which those used to
JavaScript might potentially stumble over because of their deceptive
similarity to JS:

-   **Scoping, symbol lookup, and closures**. Many constructs with inner
    functions will appear to work just like in JS, but only
    incidentally. Namely, how the languages bind (or not) and find
    non-local variables in an inner function are ***fundamentally very
    different***. See [](symbol-resolution.md) for details.
-   **All block-level constructs in s2** (except for `eval`) **create
    new scopes**, where JS has only Function and Global scopes. Loops
    and `if`/`else` constructs push a new scope onto the stack
    regardless of whether their bodies are enclosed in braces or
    not. `[]` and `{}` also evaluate in their own scopes.  Parenthesis
    groups do not start new scopes except in the case of property
    access in the form `x.(y)` and function arguments: the arguments
    are processed in the call's (new) scope, so temporaries created in
    the arguments are cleaned up (at the latest) just before the call
    returns.
-   `this` **does not propagate into function calls** like it does in
    JavaScript. A function called without an explicit `this` gets
    *itself* set as its `this`. It is thought that this approach
    should be less confusing than guessing "which this is this?" Local
    aliases and [`importSymbols()` can provide inner functions with
    access](type-function.md#type-function-importsymbols) to an outer
    "this," but each does so differently: the former still accesses a
    higher scope's variable, whereas the latter makes a function-local
    reference to a variable's *value* (not the *variable* itself) at
    the time `importSymbols()` is called.
-   **Property keys may be of almost any type in s2**, where JS
    silently coerces all property keys to strings. Similarly to JS,
    however, both use non-type-strict comparisons for property key
    purposes, so the keys 1 (integer), '1' (string), and 1.0 (double)
    are equivalent property keys in both languages[^63]. Note that
    when generating JSON (which only supports string keys), some key
    types cannot be output (basically, any keys except strings and
    numbers will be elided, as will certain value types, and numbers
    will be converted to strings, which may cause loss of precision
    for floating point values). If s2 ever changes its core object
    type to use hashes, this type-lax comparison will disappear, so
    please don't rely on it.\  
    **As of 2021-07-09, certain types are disallowed as property and
    hashtable keys:** any type for which the *mutable* state is
    accounted for in its comparison and/or hashing is no longer legal
    as a key. That includes Buffers and Tuples. Other high-level types
    do not use their mutable state for those purposes and are thus legal
    for use as keys.
-   **Keywords are legal anywhere a value is** because they all evaluate
    to a value. This allows crazy stuff like passing loop constructs as
    function arguments or as a parameter's default value.
-   **s2 requires semicolons where JS does not**. Early versions of s2
    were more JS-like in that regard, but it led to too much
    special-case handling of newlines. Requiring explicit
    end-of-expression termination (via semicolons) allows the syntax to
    be more flexible, predictable, and robust, with less C code required
    to handle it.
-   **s2 has an exit feature**, whereas JS can only exit a script via
    running all the way through it or triggering an error. `exit` works
    similarly to its counterpart in C except that it unwinds the s2
    stack along the way so that finalizers can run[^64]. Internally,
    an `exit` is in fact treated like an error, but that error case is
    specifically handled to Do The Right Thing (the same applies for
    other flow control keywords like `return`, `break`, and `continue`).
-   `return` **is more PHP-like than JS-like.** s2 treats "top-level
    scripts" (basically any atomic script source unit, whether passed
    directly to s2sh, run via `import()`, or `Buffer.evalContents()`)
    as legal for use with the `return` keyword, similarly to PHP. It
    basically means "leave this script source code unit" or (if we don't
    mind being a bit lax with semantics) "leave this file." In s2sh's
    interactive mode, `return` simply "returns" from the current line of
    input (potentially before reaching its end) - it does not exit
    interactive mode. e.g.\  
    ```bash
    s2sh> 1 ? return 2 : throw 3;
    result: integer@0x706090[scope=#1@0x7fffa0678108 ref#=0] ==> 2
    ```
-   **s2's `new` keyword functions a bit differently**, partly so that
    it can better integrate with client-bound native types. The
    differences are mainly in how it handles the result value of a
    constructor function. See [the `new`
    section](keyword-new.md) for details. Related…
-   **The `||` operators** works differently. JS's `||` works like s2's
    `|||` operator, and s2's `||` always evaluates to a boolean
    value.
-   **s2 prototypes are not _quite_ the same as JS prototypes.**
    They're similar, but not identical.
-   In s2 **the function arguments array is really an array**. In JS
    it's an array-like Object, which sometimes requires extra work to
    convert it to an array (using
    `Array.prototype.slice.call(arguments,0)`) when passing on the
    arguments to other routines.
-   s2 (more correctly, cwal) specifies a **client-configurable output
    channel** usable by both scripts and native code, whereas JS
    (wisely) does not specify any I/O APIs (but it's generally one of
    the first things most non-browser JS engine users embed because we
    all love to have a `print()` function). s2 clients are not obligated
    to use the output channel - it is provided as a convenience for the
    common case that script- and C-side code want to share a
    well-defined output channel.


<a id="tips-editing-script-code"></a>
# Editing s2 Script Code

Most editors which support JavaScript syntax highlighting can handle s2
very well. The syntax is close enough to JavaScript that the majority of
editors will have no problems with it (or with most of it, though they
do sometimes trip up on heredocs and unusual keyword usage). For
convenience, configure your editor to use JavaScript mode for the file
extension(s) you use for your s2 scripts (i use, quite uncreatively,
".s2"). If your editor is intelligent enough to do "code inspections,"
you may want to *disable them*, as JS-specific inspections are wrong
more often than not in s2.

The only s2 construct which consistently gives me grief in JS
syntax-highlighting modes is heredocs.

See the [mindset section](#tips-s2-mindset) for suggestions regarding
the "mental side" of editing s2 code.


<a id="tips-importsymbols"></a>
# Use `importSymbols()` resp. `using` for Inner Functions

When a function defines inner functions, s2 has to (re-)create those
functions each time the outer function is run (remember, s2 is
exceedingly memory-light, and does not "compile" its code). For this
reason, it is good practice to define inner functions as imported
symbols unless the outer function will rarely be called or will only
called once or twice during the life of a given script.

For the general case, instead of this:

```s2
const f = proc(x){
 x.foo( proc() { … } );
 const aFunc = proc(){ … };
 aFunc(x);
 ...
};
```

Prefer this:

```s2
const f = proc(x){
 x.foo( xCallback );
 aFunc(x);
 ...
} using {
 aFunc: proc() { … },
 xCallback: proc() { … }
};
```

The main reason is memory usage: the second form only has to allocate
the inner functions one time (and then keeps them in memory as long as
the containing function references them), whereas the first form has to
do it on every call. (To be clear: it might not allocate anything - it
may get all its memory from the recycler. Abstractly, though, it has to
allocate a new, local instance.) Another reason is parsing effort - the
second form requires, in the aggregate, less work unless the function is
only called once or twice. Functions which rarely get called do not
benefit, or not as much, from this guideline, and may even cost more
memory: if a function is never called, the imported symbols will have
been created but will never be used.

The first form is arguably more readable, though, and if that is more
important to you than memory allocation and performance, feel free to
use it.

Such constructs can be nested - see [this script](/finfo/s2/require.d/json2.s2)
for an example of inner functions nested several levels deep
this way.

Another option is to set up inner functions only the first time they are
needed:

```s2
const f = proc callee(){
 if(!callee.inner){
   callee.inner = proc(){...};
 }
 callee.inner(...);
};
```

Here's a slightly different syntax with the same effect:

```s2
const f = proc callee(){
 callee.inner || (callee.inner = proc(){...});
 callee.inner(...);
 // or, more compactly (note the ||| operator):\
 (callee.inner ||| (callee.inner = proc(){...}))(...);
};
```

Those variants have a bit of parsing overhead on each call so that the
tokenizer can skip over the function creation on the 2nd and subsequent
calls, but it is not as expensive as instantiating the inner function on
each call. The form shown last (with the `|||` operator) is, in terms of
parser overhead, the most efficient of the initialize-on-first-use
approaches shown, but the first option (with an `if`) is arguably more
readable by humans.

All that being said, there are cases where a new function instance
must, for proper behaviour, be created anew on each use. [This
script](/finfo/s2/toys/funcbind.s2) demonstrates such a case, where
each instance of an inner function will be returned to the caller and
needs its own imported symbols, independent of all other copies of the
function.

<a id="tips-descriptive-assert"></a>
# Descriptive Assertions

When an assertion or affirmation fails via the `assert` resp. `affirm`
keywords, the text of the error contains the whole source of the
failed expression, up to the end-of-expression terminator. This means
that one can comment assertions in as much detail as they like, and
have those details bubble up to the user on error:

```s2
assert false
  /* a comment before the semicolon
     gets included in the error message */;
```

The assertion message text will include the comments because they come
before the semicolon resp. the end of the expression (colloquially
called EOX). The following
comment will *not* be included in the error message because it comes
*after* the semicolon resp. EOX:

```s2
assert false; // a comment after the semicolon is not included in the message
```

Clients can use this as a way to explain to the user what went wrong at
that point, what the assertion is really checking, why it does so, and
possibly recovery suggestions. Or maybe to annotate assertions with
corresponding ticket numbers. This is also useful for function argument
validation:

```s2
const getCityCoordinatesByName = proc(a){
 affirm typeinfo(isstring a) /* expecting a city name */;
 ...
};
```

Alternately:

```s2
const getCityCoordinatesByName = proc(a){
 typeinfo(isstring a) || throw "Expecting a city name";
 ...
};
```

> Sidebar: personally, i prefer `affirm` over `throw` because it's shorter
and comments are (A) faster to parse than strings because they require
no unescaping and (B) cost the interpreter no extra memory unless/until
they're used in an error string. That said, the above `throw` is skipped
over if the affirmation is `true`, and the string literal costs us no
extra memory in that case because skipping over an expression
necessarily tokenizes it but does not allocate any memory along the way.
(That said, tokenizing a string is ever-so-slightly slower than
tokenizing a comment.)




<a id="tips-relative-op-costs"></a>
# Relative Performance of Various Common Ops

The following list orders (*approximately!*) various types of
script-side operations from "least expensive" to "most expensive," in
terms of abstract computational costs (including memory, though not all
operations actually allocate). The ordering is based solely on my
(relatively intimate) understanding of the implementation, and not on
profiling, so interpret the list as an "educated suggestion" as opposed
to a "mathematical rule":

1.  Comments, newlines, and other noise/junk tokens get filtered out
    early on, and affect only the tokenizer level. Such tokens inside
    function bodies take up memory, though, because a function body is
    internally stored as a copy of its originally string source plus the
    location information for where the function was defined (for error
    reporting).
2.  Tokenizing operators is the cheapest of the truly "moving" parts,
    hands-down.
3.  Tokenizing identifiers. This normally requires a string allocation
    because the operator layer (the stack machine) does not have access
    to the raw token memory (they work with a higher-level form of the
    tokens).
4.  Using operators:
    - Comparisons and *instanceof* are cheapest, as their results
      require no allocation.
    - Simple operators (+, -, \*, etc.) often have to allocate a value
      for the result.
    - Compound assignment ops (+=, \*=, etc.) cost a bit more because
      they first have to do a lookup of their LHS.
    - Overloaded operators cost the same as a property lookup, a
      function call, *plus* an operator.
5.  Property lookup (implies an operator call) is approximately O(N) on
    the number of total properties (including those in prototypes), but
    the keys are (as of 20170320) kept sorted to cut average search
    time. In practice the performance has never been problematic.
    Getting notably faster lookup would require prohibitive (to me/cwal)
    memory costs. (&larr; At the time that was written, hash tables cost
    considerably more memory, on average, than Objects, but that gap has
    since been cut notably.)
6.  Keywords most often basically act as a recursive call back into the
    main eval loop, with a C function in the middle to change the
    parsing semantics a bit. Sometimes they're cheaper than operators
    (e.g. a simple *break* or *return*) but larger ones (if/while/etc.)
    may of course run an arbitrary number of expressions and scopes. The
    setup for that is not all that high (cheaper than a function call),
    but it's more costly than a simple list of values and operators.
7.  Function calls. While they appear simple, they have the highest
    relative setup and teardown cost of any core s2 operations, quite
    possibly by an order of magnitude or more (they've never been
    profiled).

Garbage collection is going on all the time, as values reach the ends of
their lives and as the core evaluation engine cleans up every now and
then (always *between* atomic expressions, though scoping might
temporarily redefine what that really means). As such, it doesn't have a
cost, per se, which we can measure in terms of the above operations. GC
costs depend 100% on the amount of stuff to clean up, which depends
largely on how much memory the previous 1 to N previous expressions *in
the current scope* allocated (where N is a "sweep interval" config
option of s2 which defaults to 1). Once the recycling bins have gotten a
few entries, further allocation sometimes drops to 0.


<a id="tips-remove-prototype"></a>
# Don't Need a Prototype? Remove it!

If you really don't want a given container value to have a prototype,
simply unset it:

```s2
unset myObj.prototype;
// or:
myObj.unset('prototype');
// or, as a special case, assignment to null or undefined:
myObj.prototype = null;
myObj.prototype = undefined;
```

(Assigning the `null` or `undefined` values to a prototype removes it,
and is also legal in the body of object literals. That is a special-case
behaviour of the `prototype` pseudo-property. For all other properties,
doing so simply assigns them that value.)

This is not allowed on non-containers because all non-containers of a
given type share (for memory usage reasons) the same prototype pointer
at the C level, and s2 does not allow clients (for reasons of sanity) to
replace the prototypes of the non-container types (it does allow those
prototypes to be modified, however).

`myObj.unset('prototype')` works even though `prototype` is not a "real"
property and `myObj.hasOwnProperty('prototype')` always returns `false`.
Interestingly, though, after calling the former, `myObj` no longer has
an `unset()` method(!) because that method is inherited from (surprise!)
its prototype or a further-up ancestor:

```bash
s2sh> g.prototype
result: object@0x1f18010[scope=#1@0x7fff825ac558 ref#=17] ==> {
}
s2sh> g.unset('prototype')
result: bool@0x684718[scope=#0@(nil) ref#=0] ==> true
s2sh> g.unset('x')
rc=105 (CWAL_RC_EXCEPTION)
EXCEPTION: exception@0x1f4aaf0[scope=#1@0x7fff825ac558 ref#=0]
==> {
 "code": 304,
 "column": 7,
 "line": 1,
 "message": "Non-function (undefined) LHS before a call() operation.",
 "script": "shell input",
 "stackTrace": [{
 "column": 7,
 "line": 1,
 "script": "shell input"
 }]
}
```


<a id="tips-parsing-efficiency"></a>
# Parsing Efficiency Tweaks

Here are some tips regarding squeezing a few extra cycles out of the
tokenizer and parser. These *are not rules*, they are just tips for
the performance-conscious.

-   **Prefer `X.Y` over `X[Y]` if possible**, as it requires less
    tokenization. This especially applies to arrays, where programmers
    are used to typing `array[3]`. That works, but `array.3` is
    *slightly* faster. The latter form cannot be used with dynamic
    indexes without parens around the index pressions (`array.(1+2)`),
    but those parens cost essentially the same as the `X[Y]` operator
    does (which also cost one less dot operator), so `X[Y]` is more
    efficient (and more conventional) than `X.(Y)`.
-   **Boolean operators are more efficient than if/else**
    `a && return a` is more efficient than: `if(a) return a`
    because it does not need to start a new scope. Whether or not an
    if/else/while/for/etc. body is contained in {squigglies} or not is
    less relevant to the overall performance, as the costs and benefits
    of single-expression bodies over {block} bodies about even out, all
    things considered.\  
    Be careful when using the `||` operator in similar contexts - the use
    case might call for `|||` instead, e.g. `return a ||| b` is
    different from `return a || b`.
-   Use block expressions to **reduce tokenization of seldom-traversed
    short-circuited expressions**. Example:\  
    `a || throw "missing a"` \  
    The throw condition presumably seldom happens, and can be
    short-circuited more quickly if it is wrapped in a block-level
    construct, e.g. parens:\  
    `a || (throw "missing a");` \  
    For a small expression it makes no difference (and parens *might*
    increase tokenization), but for a larger expression parens can save
    a few cycles. Alternately:\  
    ```s2
    affirm a && "missing a";
    // or (most efficient, but the error message might seem cryptic to non-techies!):
    affirm a /* missing a */;
    ```
-   **Comments in function bodies take up memory**. Because function
    bodies get copied, in full, to a string (for later execution),
    comments in functions are potentially a waste of memory. Suggestion:
    keep comments in function bodies to a minimum. This does not apply
    to, e.g. conventional API-doc-style comments outside of a
    function's body, except in the case of a function defined within
    another function (in which case the outer function pays the memory
    cost). (Yes, we all know that *real* programmers don't comment their
    code. But i do.)
-   **Prefer `String.concat()` over `String+String` for
    more than a few strings**. e.g. `"a".concat("b", "c")` is
    *abstractly* more efficient than `"a"+"b"+"c"`, but the function
    call overhead means that the latter is probably more efficient for
    three small strings. The `+` operator has to evaluate to a temporary
    string for each pair of operands, creating ever-more strings along
    the way to the result: `((a+b===ab) + c)===abc`, and so on for longer
    chains of `+` operators. `String.concat()` appends `this` and each
    argument to an internal buffer before creating one string at the
    end, but also has the overhead of a function call. Note that you can
    use `"".concat(...)` if you don't have a string value to start with,
    and any empty string is a constant value which does not require
    allocation.
-   **Don't declare vars until you really need them**. Since var
    declarations are themselves expressions, they can be used anywhere a
    value is expected (with the caveat that a var decl will "hijack" a
    comma list on its *LHS*, slurping up any *RHS* commas for its own
    use). This allows vars to be declared last-minute in many cases,
    though we normally need extra parenthesis to get it all right
    because of the comma hijacking.


# Footnotes

[^1]: Prior to 2021-06-24, `[]` in property access context did not
    use its own scope, which made it possible to declare variables
    which would be available outside of that access. For example:
    
```
var o = {}; o[var x = 'hi'] = 1; assert 'hi'===x;
```

    As of 2021-06-24, property access in the form `x[y]` and `x.(y)`
    now run those blocks in their own scopes.

[^62]: This is an example of how the end of a parenthesis block acts
    as an implicit EOX, which is why the "else" part does not need a
    semicolon after it.


[^63]: In hindsight possibly a poor idea, but changing this may well
    break stuff (in the sense that its docs would need fixing - it's
    unlike that any non-s2/th1ish-test script code actually relies on
    the current behaviour).

[^64]: Reminder to self: the C++ conversions can theoretically hide
    the exit in some unusual constructs, essentially stopping its
    propagation. It would require quite an unusual setup to do it,
    though. We might need an s2-internal flag for is-exiting, instead
    of relying solely on result code propagation. (Some of that has
    since been put in place, but it's currently only used for the
    optional interruption/Ctrl-C handling.)
