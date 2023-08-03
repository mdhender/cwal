# s2: Flow Control
#### ([&#x2b11;Table of Contents](./))
# The Flow-Control Keywords

Jump to...

* [if/else](#keyword-if)
* [for/while/do Loops](#keyword-for-while-do)
* [foreach Loops](#keyword-foreach)
* [break and continue](#keyword-break-continue)


<a id="keyword-if"></a>
# But what `if`...

s2 supports conventional if/else constructs:

```s2
if( condition ) { body }
else if( otherCondition ) { body }
else { body }
```

The braces are optional for single-expression bodies, but such
expressions always require a trailing semicolon:

```s2
if( condition ) a=b, c=d;
else a=d, c=b;
```

The scope created by an if/else block begins at the first parenthesis
and ends at the end of the final block in the group. This means
variables declared in a (condition) part are in scope throughout the
remaining of the if/else group:

```s2
if(!var x = getSomeValue()){ … }
else {
 s2out << 'getSomeValue() says:' << x << '\n';
}
```

Because heredocs are internally very closely related to script blocks,
they can be used as bodies for if/else (as well as loops):

```s2
if(true)<<<_FI s2out<<'if in heredoc\n' _FI
/* minor achtung: a heredoc in this context works like a
   {script block}, so a semicolon after the heredoc ends
   the 'if' block, causing a syntax error when the following
   'else' is subsequently seen outside of an if/else block: */
else <<<_ESLE s2out<<'else in heredoc\n' _ESLE;
/* OTOH, a semicolon at the end of the if/else construct
   is legal, but an EOL also works like a semicolon in
   that particular context. */
```

An if/else block, like everything else in s2, constitutes an
expression[^21]. It evaluates to `true` if *any* of the
`if(condition)` parts evaluates to a truthy value, otherwise it
evaluates to `false`. e.g. `(if(0) 1;else 2)===false`.

<a id="keyword-for-while-do"></a>
# *do*ing a loop *for* a *while*...

The `for`, `do`, and `while` keywords offer conventional loops using the
same syntax as in most other languages. Each loop starts a new scope as
follows:

-   A `while` loop starts a single new scope at its first '(' and
    re-initializes the scope on each iteration.  The braces around the
    body are optional for single-expression bodies, but a semicolon
    (or EOF) is required to terminate them. An empty body/expression
    is legal, but take care to avoid endless loops.
-   A `do/while` loop starts a new scope each time it runs the body.
    Since the condition is run *after* the body, any variables declared
    in the condition part are discarded before it iterates again.
    Variables defined in the body are visible in the `while` condition,
    however.
-   A `for` loop starts two new scopes: one at the initial '(' and one
    at the semicolon after the "pre" part, the latter getting
    re-initialized on each iteration. The reason for the first scope is
    so that variables created inside the "pre" part of the loop do not
    outlive the loop (unless propagated out). The inner scope allows
    scope-local variables to be defined with C-like semantics (i.e. the
    variables go out of scope at the end of *each iteration* of the loop
    and get created anew on each iteration). A curious side-effect of
    the eval order is that variables defined in the loop body are
    visible in the post-loop part: `for( …; …; assert x) { var x = 1; … }`

Examples:

```s2
var i;
for( i = 0; i < 10; ++i) { … } // or…
for( var i = 0; (var x = i) <10; i+=2) { … }
// ^^^ *loop-local* i, *iteration-local* x

var x = 10;
while( x > 0 ) { … x -= 1; }
while( … ) { var y = ….; } // *iteration-local* y
```

Note that the structure of `do/while` loops effectively makes any
variables defined either in the body or the `while(...)` part
iteration-local. Vars defined in the *body* of do/while loop are visible
in the `while(condition)` part, but not the other way around.

Note that the body part of the loop need not be contained in {squiggly
braces} if it is a single expression, but such expressions require a
terminating semicolon or EOF(-like construct). An empty body,
represented by either {} (an empty block) or simply a semicolon, is
legal for all three loop types, but avoiding endless loops is the
scripter's responsibility.

As an exception to the semicolons rule (made for reasons of usability
and conventions conformance), `for`/`while` loops which have a {braced
body} will *sometimes* treat a newline at the end of their body block as
an implicit semicolon. Those with an unbraced single-expression body
require a semicolon like any other expression, as do those which are the
RHS of a large expression. Some examples of when semicolons are required
and not:

```s2
for( var x = 0; x < 10; ++x );
// empty body is legal[^26] ^^^, but semicolon required

for( var x = 0; x < 10; ++x ){}
// newline acts as implicit   ^^^ optional EOX here (solely for conventions purposes)

var i = while(…) {
 … if(…) break x; …
}; // ← semicolon is required here to terminate the LHS var decl expression
```

Like the loop constructs, each `if`/`else` block has its own scope, also
starting at the first parenthesis[^27]. Thus variables declared inside
them are not visible once the scope has exited (and have, unless
propagated out, been destroyed by that point).

<a id="keyword-foreach"></a>
# `foreach()` Loops

(Added 20160227. String iteration support added 20180620.)

s2 has historically not had a for-each loop construct because (A) there
are various types of containers and it wasn't clear how/whether to
consolidate them into one construct and (B) *Object.eachProperty()*,
*Array.eachIndex()*, *Hash.eachEntry()*, and *Enum.eachEnumEntry()* all
did that job. Adding a loop construct which does those jobs simplifies
client code notably in some places, though, and is more efficient
because it doesn't require creating and calling a callback function. It
also provides an in-language approach which can be used when s2 is run
without those prototype methods.

`foreach` allows one to iterate over:

- Object-level properties
- indexed Array/Tuple entries
- Hash table entries
- Enum entries
- and (as of 20180620) characters of a string

... all with the same interface. Arrays and Hashes can hold both
properties and indexed resp. hash entries, and this keyword lets the
caller specify (in the form of a syntactical flag) which of those data
sources to iterate over.

Basic syntax:

```s2
foreach( container|string => key ) EXPR | {block}
foreach( container|string => key, val ) EXPR | {block}
```

Where `container` is the value being iterated over and `key`/`val` are
identifiers of the user's choice. It throws an exception if all of
`typeinfo(iscontainer container)`, `typeinfo(istuple container)`, and
`typeinfo(isstring container)` are false. Unlike `for/while/do` loops,
`foreach` does not permit an empty loop expression/body (because it
would be useless, whereas empty traditional loop bodies are useful on
rare occasion).

`foreach` iterates over any properties belonging to the given container
(not including hidden properties nor those inherited via its prototypes)
or (for strings) the characters of a string, defining the
iteration-local variables `key` resp. `val` to the key resp. value of
the property (or character), e.g.:

```s2
foreach( container => k, v ) print(k, '=', v);
foreach( container => k ) print(k, '=', container[k]);

// during iteration, fetching properties works, but assigning them is prohibited:
assert 'CWAL_RC_IS_VISITING'===catch{
 foreach( container => k ) container[k] = 1
}.codeString();
```

`typeinfo(mayiterate EXPR)` may be used to determine beforehand
if iteration is (currently) legal.

`foreach` throws if its operand is not legal.

The body may use the `continue` and `break` keywords, exactly as with
`for/do/while` loops. Like such loops, `foreach` evaluates to the
`undefined` value unless a value is passed to `break` within the loop, in
which case that value is the result of the `foreach` loop. Any empty
input set never iterates, and thus cannot trigger a `break`, thus its
result is always the `undefined` value.

**String iteration** iterates over each character in the input string,
with each character provided to the loop body as a length-one string.
`foreach` is much more efficient (faster) than using, e.g. `for(var i =
0, n=aString.#; i<n; ++i) {...aString[i]...}`, because that
approach has to (for non-ASCII input) re-traverse the whole string on
each indexed access, whereas `foreach` must iterate over each character
only once. Examples of iterating over strings:

```s2
foreach( "abc" => val ) assert 1 === val.#;
foreach( "abc" => index, val ) {
  assert typeinfo(isinteger index);
  assert 1 === val.#;
}
```

**Array entries**, which are distinct from its key/value properties,
can be iterated over by prepending the container expression with an
`@`-sign:

```s2
foreach( @myArray => val ) EXPR | {block}
foreach( @myArray => index, val ) EXPR | {block}
```

Note, however, that the key/val part is interpreted slightly
differently than object properties: if only one identifier is
provided, it refers to the *value* at the current index,
and if both are provided then both the current array index and the
value at that index (in that order) are exposed in the loop.

> Trivia: if the C-level Array contains any `NULL` entries, script code
receives those as the `undefined` value (which is also a legal
script-level value, i.e. `undefined` does not mean that the entry in
question *really* is a `NULL` pointer - it might have been assigned the
`undefined` value). (More trivia: prior to 20160225, `NULL` entries *were*
skipped over, but that was likely an artifact intended to keep th1ish
running (which is no longer a concern).)

Likewise, **hash entries** can be iterated over by using a `#`-sign
prefix:

```s2
foreach( #myHash => key ) EXPR | {block}
foreach( #myHash => key, val ) EXPR | {block}
```

The `@`- and `#`-forms throw if the expression does not evaluate to a
value which has an list resp. hash somewhere in its prototype chain.

**Tuples**, though similar to arrays, have no properties which can be
iterated over, so `@` is not needed to disambiguate. To simplify script
code, however, the `@` is optionally permitted:

```s2
foreach( myTuple => val ) ...; // identical to:
foreach( @myTuple => val ) ...;
foreach( myTuple => index, val ) ...; // identical to:
foreach( @myTuple => index, val ) ...;
```

**Enums are a special case:** enums internally use hashes for their
property storage but the `#` modifier is ignored. When iterating over
enum entries, only the "forward mappings" of their string keys to
unique values is iterated over:

```s2
foreach( enum {a,b,c} => k ) print(k); // outputs 3 lines: a, b, and c
foreach( enum {a,b,c} => k,v ) print(k,v); // outputs 3 lines: a <something>, b …
```

The "reverse" (unique-values-to-names) mappings are *not* iterated over.

Keep in mind that the values exposed by the loop for enums are the
*unique identities* of the enum entries, not their associated *values*
(if any). Getting at their values requires one more level of
indirection:

```s2
foreach( enum {a:1,b:2,c:3} => k,v ) print(k, v, v.value);
// Outputs something like:
a unique@0x55910999a070 1
b unique@0x559109999fd0 2
c unique@0x559109999f30 3
```

See [the enum page](type-enum.md) for more information.

Note that containers may not be modified during iteration (a limitation
of cwal's internal property storage mechanism). Doing so is legal for
list types, but the Object/Hash property model cannot support modification
during iteration (and an exception is thrown if it is attempted). In
practice this has never proven to be problematic.

<a id="keyword-break-continue"></a>
# Let's `continue` our `break`...

For each of the above-described loop types, the `continue` keyword
stops the current iteration and skips to the next iteration (if
any). The `break` keyword ends the loop, optionally with a result
value.

Examples:

```s2
assert 3 === for(var i = 0; i < 2; ++i) break 3;
assert undefined === while(true) break; // no value (===undefined)
assert 2 === do{ break 2 } while(true);
```

# Footnotes


[^26]:  Be careful to avoid infinite loops.

[^27]:  Corner case bug: we re-use one scope for all (if/else/else if)
    condition blocks and the one body/expr which gets run, so we cannot
    declare the same var in multiple conditions. Low-prio to-fix
    someday. We could simply clear the scope before each condition after
    the first, but the current semantics would seem to have more
    practical use.

