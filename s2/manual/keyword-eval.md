# s2: eval and Friends
#### ([&#x2b11;Table of Contents](./))
# eval & Friends

The `eval` family of keywords covers a wide range of functionality. A
large, seemingly disparate set of features follow the same basic
pattern as `eval`. Because they work "mostly" the same (enough so that
they live in the same C-side function), they are documented
together...

* [`eval` and its Many Brethren](#keyword-eval)
* [Second-round Eval: the `->` modifier](#keyword-eval-second-round)
* [Scope vs. Eval](#keyword-scope-vs-eval)
    * [Stopping Value Propagation](#keyword-eval-stop-propagation)
* [Capturing an Expression's Bytes: the `=>` modifier](#keyword-eval-capture)

<a id="keyword-eval"></a>
# `eval` and its Many Brethren

The `eval` and `scope` keywords (collectively called "eval" here,
for brevity) both do the same thing, with one difference: they evaluate
code, but the `scope` keyword does so in a new scope, whereas `eval`
uses the current scope (the one in which `eval` is called).

These keywords require that their operands be either s2 script code in
{squiggly}[^22] form or an arbitrary single expression. When given a
{squiggly}, the contents of that block is evaluated as a script, and the
result value of the `eval` expression will be that of the last
expression in the block (or `undefined` if the block evaluates to
nothing). If given any other expression, it is *basically* evaluated as
if the keyword were not there (but will use a sub-scope if the `scope`
keyword is used, and eval specializations like `catch` will still do
their things). One caveat is that most keywords start a new
subexpression, ignoring any operators on their LHS. That leads to some
potentially strange interactions when certain keywords are used in
mid-expression.

Several other keywords are implemented as specializations of
`eval` and `scope`[^23]:

-   `break` breaks out of looping constructs, optionally passing a
    value as the loop's result.
-   `catch` behaves like `scope`, in that it creates a new scope for
    evaluating the expression, but evaluates its result
    differently. If an [exception](type-exception.md) is thrown while
    evaluating the expression, catch stops its propagation and
    evaluates to that exception, otherwise it evaluates to the
    `undefined` value. In some cases it converts syntax errors to
    exceptions (more on that later on).
-   `exit` and `fatal` both end the current script, regardless of
    sub-script depth. `exit` is caught at the top-most level and treated
    as "success" at the C level (regardless of the script-specified exit
    Value), whereas `fatal` behaves similarly, but implicitly throws an
    exception which cannot be caught by script code[^24]. When that
    result exits the script, the application may (as the default shell
    does) report it as an error. Unlike in C, these methods of exiting
    a script unwind the execution stack, so finalizers get called
    along the way.
-   `return` ends the execution of a function and sets its result
    value. If used outside of a function it quits (without an error)
    the currently-running script. When running external script files
    and `return` is called from outside a function, it only returns
    from the file it was called from, and is not propagated up any
    further.
    [Buffer.evalContents()](type-buffer.md#buffer-evalcontents) is,
    for this purpose, effectively a "file", and in that context
    `return` behaves as it does for files.
-   `throw` initiates an exception condition, using the RHS
    expression's value as the `message` property of the exception unless
    the RHS is-an Exception, in which case it is thrown as-is.

The keywords (`return`, `break`, `exit`, `fatal`, `throw`) treat a {...}
RHS operand as an *object literal*. The rest treat it as a *script
block*.

For the keywords `return`, `break`, `exit`, and `fatal`, the expression
part to the right is optional - the `undefined` value is assumed if no
expression is provided. The other keywords require a non-empty
expression (an empty one triggers a syntax error).

> Reminder to self: there might still be some cleanup pending in the
handling of () in that context - `s2_eval_expr_impl()` should possibly
treat `keyword(...)` like a function call, in that the parens bind
immediately to the keyword. That, however would make `eval (1)+1` work
differently than `eval 1+1` (even though the result is the same in
that example, it's only coincidentally so). Optionally, we could do
this special case only for the few keywords which allow an empty
expression there, but that would introduce inconsistencies vis-a-vis
other keywords :/.

`catch` resolves differently than the others:

-   It resolves to `undefined` unless an exception is thrown
    from within, or propagated through, the expression/script, in which
    case it evaluates to the propagated Exception value and stops its
    propagation.
-   It converts some normally-fatal syntax errors to exceptions when it
    can safely do so, so that the script does not die outright. See
    below for details.\  
    In hindsight, that behaviour is arguable, but is mainly intended to
    allow a user to keep `eval`'d strings from killing an app outright.
    Then again, such a script can call `exit` or `fatal`, or fail an
    assertion, all of which will exit the outer script. So far (20141119
    resp. 20141217 resp. 20171205 resp. 20191207) the current mechanism
    has worked out well enough that there's been no strong argument for
    changing it.

Note that *there is no try* keyword:

```s2
var ex = catch { [,] }; // syntax error in the array literal
assert typeinfo(isexception ex);
```

Blatant syntax errors are normally fatal to the currently running
script, but `catch` is capable of converting many normally-fatal
syntax errors to exceptions when passed a {script block} or when doing
["second-round" evaluation](#keyword-eval-second-round) via the `->`
modifier (described below). It cannot do so when doing "first-round"
evaluation on an arbitrary single expression because it must presume
that a syntax error means that this branch of the script is not
syntactically valid (not traversable by the parser). A script block,
on the other hand, is known to be syntactically correct, in that we
know where the closing brace is by the time `catch` does its work. If
the *contents* trigger a syntax error, catch will convert that to an
exception because it knows the script could potentially recover from
an "inner" syntax error. The moral of the story here is that when
catching exceptions, it's possible that the exception caught was
triggered by a syntax error within the block (or in some code called
through the block), as opposed to an exception the scripter knows
might be thrown from some operation or other. s2 currently has no
mechanism for distinguishing between exception types except for their
`code` or `message` properties (or user-defined properties), but those
are of limited practical use for purposes of dispatching exceptions
based on their "type" unless one adopts the convention of storing
Objects (from a hierarchy of exception types) as the `message`
property. Bikeshedding regarding alternate solutions (by extending s2
in this regard) is actively underway.


<a id="keyword-eval-second-round"></a>
# "Second-round" Evaluation: the `->` Modifier

The question: what should the following evaluate to?

```s2
var a = 1, b = 2;
eval "a"+"+"+"b"; // "a+b" or 3?
eval "a+b"; // "a+b" or 3?
eval { "a"+"b" }; // "ab" or 3?
```

IMO, the answer is that either approach is feasible.

By default the `eval` family of keywords evaluate ("a+b") as an
expression resulting in the string "a+b". However, their behaviours can
be changed slightly by passing them a `->` symbol as their first
argument. The right-arrow modifier changes evaluation such that if the
result of the evaluation is a string, then the *contents* of that string
get re-evaluated. If the RHS is not a string value then `->` has no
effect (it is ignored). This is necessary when, e.g. programmatically
creating math expressions or variable names from parts of strings.

**Mnemonic:** the arrow "pierces into" the expression's value.

Examples:

```s2
eval 1 + 1; // ⇒ integer 2
eval {1+1}; // ⇒ integer 2
eval "1+1"; // or...
eval "1" + "+" + "1"; // ⇒ the string "1+1" unless we use ->
eval -> "1+1"; // ⇒ integer 2
```

If the expression does not evaluate to a string then the arrow
modifier has no effect.

*Achtung*: script location information, e.g. via the `__FLC` keyword
or exceptions, may be "way off" when they come from script code eval'd
this way, as they are addressing "virtual" code which is (effectively)
expanded inline into the current script. However...

The location information problem is not exhibited (or exhibits
predictably/usably) when using the [`evalContents()` method](type-buffer.md#type-buffer-evalcontents)
of the Buffer and String classes:

```s2
assert 3 === "1+2".evalContents("optionalScriptName");
assert 3 === (new s2.Buffer()<<"1+2").evalContents("optionalScriptName");
```

In such usage, script location information is relative to the content's
body, which may optionally be given a name by passing it as the first
argument to `evalContents()`, as demonstrated in this [s2sh](s2sh.md)
session:

```s2
s2> print(" __FLC".evalContents('my script'))
my script:1:3
s2> print(eval -> " __FLC")
shell input:1:16
```

The latter example is off by only a little in this case, but that
distance grows, possibly including a different script name, in
large/complex eval'd blocks, and can lead to much confusion when
following stack trace information through dynamically evaluated scripts.
The suggested approach is therefore to use `evalContents()` except
possibly for trivial use cases.

Note that the `__FLC` keyword makes a handy value for
`evalContents()`'s argument! Also, `evalContents()` can be passed
an object, the properties of which become scope-local variables for
the duration of that call.

Note that while buffers are treated as strings in many cases, the
`->` modifier explicitely does not treat them as strings because
doing so would open up a fatal case where a being-evaluated buffer
modifies its own contents, thereby invalidating the memory which is
being evaluated, leading to memory corruption. [`evalContents()`
specifically works around that eventuality](type-buffer.md#type-buffer-evalcontents).
(That said, they may be documentation references which use `->` on
buffers, from before that bug was recognized.)


<a id="keyword-scope-vs-eval"></a>
# Scope vs. Eval

Generally speaking, prefer `eval` for simple expression evaluation and
`scope` for anything which needs to do complex work and might create
many temporaries, in particular if they may contain cycles. The scope
will allow such constructs to be cleaned up more quickly (unless such a
value is the scope's final result, in which case it is moved into the
calling scope). That said, s2 uses implicit scopes in many places (e.g.
around loops, function calls (including the parameter evaluation), and
`if/else` blocks), and clients normally do not need an extra one to keep
garbage collection working optimally.


<a id="keyword-eval-stop-propagation"></a>
## Stopping Value Propagation: the double-semicolon trick

The result of a scope/eval block is the final expression in the block.
Oftentimes we want to use scopes only to ensure that value lifetimes
stay brief, and do not want a value to propagate out. This can be easily
achieved by adding either a "dummy expression" at the end of the block
or two semicolons:

```s2
scope {
 … arbitrarily long body …
 undefined; // Will be the scope's result.
 // Alternately, end the block with an extra semicolon:
 ; // a second semicolon after an expression clears any pending result
}
```

The first semicolon formally ends the previous expression (if it hadn't
ended already) but keeps the result on the result stack (because the
engine doesn't know who's going to consume it at that point). The second
semicolon evaluates to an empty expression, which causes the pending
result to be discarded and replaced by the `undefined`[^25] value.


<a id="keyword-eval-capture"></a>
# Capturing an Expression's Bytes: the `=>` Modifier


(Added 20171228.)

The `eval`, `return`, `break`, `exit`, and `fatal` keywords support
the `=>` modifier, which changes their behaviour to capture the *body*
of their expression as a string, without evaluating the contents. If
given a {block}, the result is the contents of the block, without the
braces.  If given a non-{block} expression, it captures the full text
of the expression, including any comments in or trailing it, but not
preceeding it. In both forms, leading/trailing spaces around the
expression are stripped from the result. For example:

```s2
assert '3 * 2+1; 0' === eval => { 3 * 2+1; 0 };
assert '3 * 2+1' === eval => 3 * 2+1 ;
assert 7 === eval-> eval =>3*2+1;
// Embedded and trailing comments are included:
assert '1 + 2 + 3 /* hi */' === eval => 1 + 2 + 3 /* hi */ ;
// ... but leading comments are not (for boring internal reasons):
assert '1 + 2 + 3 /* hi */' === eval => /* hi */ 1 + 2 + 3 /* hi */ ;
```

The result can be eval'd later with any member of the eval family of
keywords using the `->` modifier, as shown in the last line of the
example above. That isn't a new feature, per se - it has always been
possible to use heredocs to construct arbitrarily large code strings.
This syntactic sugar form simply formalizes that ability.

The body must be syntactically valid script code, or a parsing
exception may be triggered, but it does not get semantically evaluated
until/unless the contents are later evaluated.


> Design sidebar: the `scope` keyword does not support the `=>` modifier
because the fact that it was a scope, as opposed to an eval, would be
lost in the result. i.e. `eval=>X` and `scope=>X` would resolve to the
same thing. The idea of having 2 keywords with identical behaviour makes
me a bit ill, so `scope` does not support the `=>` modifier. The
*results* of `eval=>` can, however, be eval'd using `scope->X`.


# Footnotes


[^22]:  Remember that heredocs count as {squigglies} for *almost* any
    purpose, but this is one of the exceptions (to avoid having to
    create related exceptions in its aftermath).

[^23]:  It turns out that the internal workflow for all of these
    keywords is essentially identical, thus they are all internally the
    same function.

[^24]:  Doh - an error-checking feature added to the *C++* bindings can
    inadvertently bypass this in some (wildly hypothetical) corner
    cases, returning control to the script (in the form of an
    exception). They check for exceptions at the C level and are not
    aware of `fatal`'s special-case processing (as it happens at the s2
    level, not cwal level). This can be fixed by (internally)
    consolidating the various (internally) propagated result codes
    (return/exit/fatal/etc.) with the interruption support (which uses
    separate storage from the result value).

[^25]:  Pedantic side-note: at the C level that particular case
    translates to a NULL, so that we can differentiate between truly
    empty expressions and those evaluating to the undefined value (the
    default for any value which doesn't have a saner one).
