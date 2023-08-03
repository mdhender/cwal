# s2: Operators
#### ([&#x2b11;Table of Contents](./)) ([&#x2b11;grammar](grammar.md))
# Operators

Jump to...

* [Operators](#operators)
* [Short-Circuiting](#short-circuiting)
* [Property Access Operators](#prop-access-ops)
* [Hidden Properties](#hidden-props)
* [Operator Overloading](#overloading)
* [Overload-only Operators](#overload-only-ops)


<a id="operators"></a>
# Operators

s2 adopts C/C++ precedence rules, with only very minor changes in
interpretation (namely in how it evaluates parenthesis groups, though
the effect is essentially the same), and adds a few additional
operators.

Note that spaces, including newlines (in most cases), are insignificant,
so operators will span lines while looking for operands. A semicolon or
EOF[^1] ends that search. s2 does not (like C does) consider
"standalone" semicolons to be an error - they simply terminate an empty
expression (but not all contexts allow empty expressions).

## List of Operators

The operators are listed below in order of their precedence, highest to
lowest. All operators grouped under the same top-level bullet point have
the same precedence as each other unless noted otherwise.

**Primary expressions:**

-   **Dot operator:** `X.Y`\  
    Associativity: left\  
    `object.propertyName`\  
    `object.Integer|Double|String`\  
    `object.(expr)`\  
    The LHS must be a [container type](type-intro.md) or be of a type
    which has a base prototype (e.g. a number, string, or buffer). If
    the LHS is an array/tuple and the RHS an integer, it is treated as
    array indexed access. With array-index access, if the result is a
    function which is then immediately called, the array does *not*
    become the 'this' for the call, as with normal properties.\  
    As of 20171115, strings support integer property indexes to access
    individual characters (as length-1 strings), evaluating to the
    *undefined* value if it's out of range (and throwing an exception
    for negative indexes). Accessing indexes of a string is *O(1)* for
    ASCII-only strings and *O(N\[=index\])* for strings which contain
    *any* non-ASCII characters, thus it is not an efficient way to
    iterate over such strings.
-   **The property access operator** (`X[Y]`) works just like
    the dot operator except that the *Y* part is evaluated as an
    expression, the result of which is used as the property key. It
    functions identically to `X.(Y)`.

    -   **The "length" pseudo-operator:** `X.#`\  
        This is a built-in shortcut for the oft-used `length()` method
        of the following data types: Buffer, String, Array, Tuple.
        For hashtables it resolves to the number of hash entries.  For
        enums it resolves to the number of "visible" entries, not
        including its "reverse mappings." For other types which can
        hold their own properties, it resolves to the number of
        properties they currently hold (not including inherited
        properties). It is more efficient than the `length()` method
        because it avoids the script-side function call overhead. It
        is not currently overloadable but may one day be made so. This
        is only legal in read-only contexts, not assignments. For
        types not listed above, it triggers an error.

-   **The "hash-search" operator:** `X#Y`\  
    Associativity: left\  
    Similar to the dot operator but (A) requires a
    [*Hash*](type-hash.md) on the LHS, (B) does not treat RHS
    identifiers as property keys (so quote them if needed), (C) can only
    be used in read-only contexts (not assignment contexts), and (D)
    implies no association between the Hash and the result (i.e. the
    Hash is *not* `this` if the result is a function which gets called
    immediately afterwards). It is intended to bypass the function call
    overhead of `Hash.search()`, so that hashes can compete with object
    properties in terms of speed (property access does not have the
    extra function call overhead of `Hash.search()`, and that overhead
    may outweigh any speed bonus).
-   **Parenthesized and square-brace-enclosed expressions** are handled
    slightly differently than is conventional. They are not operators,
    per se, but the end effect is, because of other grammar rules, the
    same:

    -   Parsing treats these groups like atomic values, so they
        effectively have "value precedence" and no associativity. The
        most notable side-effect of this is that parenthesis groups on
        the LHS of certain operations will not work as one might
        expect, e.g. in (`(obj.prop)=3`) the RHS (the assignment), will
        fail. Also, (`(obj.func)(...)`) works but not the same as `obj.func()`,
        in that the `this` of both calls is different.\  
        Reminder to self: in both cases, the difference (vis-a-vis convention)
        is that the `this` part of the LHS is lost when the left-most `()`
        block is ended.

    -   If a `(...)` block immediately follows another value, that value
        must be a function (or otherwise be "callable"), or an
        exception is triggered. If it follows a function resp.
        callable value/keyword, it is parsed as a conventional list of
        function call arguments (from left to right), then calls the
        function with those arguments. In the context of a property
        access operators, such a call uses the property op's LHS as
        the `this` value for the call. Non-property function calls
        have the function itself as their call-time `this` value (it
        does *not* propagate `this` from another scope like JavaScript
        sometimes does).

-   The **`[...]` pseudo-operator** is parsed just like parenthesized
    groups but its interpretation is context-dependent:

    -   In a Value context, it is treated as an Array literal, similar
        to JavaScript.

    -   In an Operator context, it works like a Dot operator or (in one
        specific context) the Array-append operator (described below).
        If the \[...\] contains an expression, the contents of the
        braces become the RHS for a dot operator. i.e.
        obj\['property'\] is internally translated into obj.property,
        but allows the user more flexibility in formulation (and type)
        of the property key (which need not be a string: *X\[X\]=X* is
        perfectly legal).

-   **The `X->Y` (arrow) operator** (associativity: left) has no
    default implementation, and must be [overloaded](#overloading) if
    used. It has the same precedence as the dot operator, and is used
    like it, but (A) it does not treat RHS identifiers as property keys
    (they are expanded before being passed to the operator) and (B) does
    not do any internal setting of the current `this` like the dot up
    does[^2]. Thus it cannot be used as the target for an assignment
    nor the `unset` keyword. The return semantics are implementation-defined.
-   **The `X::Y` (double-colon) operator** (associativity: left)
    (added 20160819) is a cross between the dot and `->` operators: it
    treats RHS identifiers as property keys (as opposed to immediately
    evaluating them), like the dot does, but does not bind its LHS to
    `this` if the result is a function which gets called immediately
    after this op (so it behaves like `->` in that regard). The return
    semantics are implementation-defined. There is no default
    implementation: this operator must be [overloaded](#overloading) to be used.
    (Trivia: this operator was added in order to simplify access to
    values wrapped up behind enum entries.)

**Unary:**

-   **Unary prefix** `+X` and `-X`\  
    Associativity: right\  
    Their result type depends on their operand type. Note that s2 has no
    special support for non-numbers like NaN, Infinity, etc., so -0 is 0
    in s2[^3]. These operators are overloadable.
-   **Increment and decrement:** `X++`, `++X`, `X--`, `--X`\  
    Associativity: prefix right, postfix left\  
    The immediate LHS (for postfix) or RHS (for prefix) must be an
    identifier or an object property access operation. *The garbage
    collector works best with the prefix forms!*
-   **Logical not:** `!X`\  
    Associativity: right\  
    Evaluates to a boolean. Tip: (`!!expr`) can be used to get the boolean
    value of its RHS, but be sure to wrap the expression in parenthesis
    if it's not a single operation, e.g. `!!(a+b+c)`.
-   **Unary bitwise negation:** `~X`\  
    Associativity: right\  
    Evaluates to an integer.

**Multiplicative:**

-   `X*Y`, `X/Y`, `X%Y`\  
    Associativity: left\  
    Modulo always results in an integer (like C, not JavaScript). The
    result type of `*` and `/` is (unless overloaded) the same type as the
    LHS. e.g. (`2*2.0`) resolves to the *integer* 4, but (`2.0*2`)
    resolves to the *double* 4.0.\  
    Potential TODO? If the RHS is a double, switch to a double? In
    practice it has sometimes annoyed me that (`2*2.0===4` instead of
    `4.0`), but this approach also has potential uses/abuses.

**Additive:**

-   **Binary `X+Y` and `X-Y`**\  
    Associativity: left\  
    For the built-in (numeric) types, the result will be of the LHS
    type, so (`1+2.3===3`) but (`2.3+1===3.3`). Overloads may evaluate to
    whatever they like. For an LHS integer, each operand is converted to
    an *unsigned* integer for the operation so that overflow and underflow
    behave predictably.\  

**Bitshift:**

-   `X>>Y` **and** `X>>Y`\  
    Associativity: left\  
    Internally uses *unsigned* values during the shifting so that overflow
    and underflow are predictable. [Overloaded](#overloading)
    implementations may implement whatever semantics they prefer.

**Relational:**

-   **Comparison:** `X<Y`, `X>Y`, `X<=Y`, `X>=Y`\  
    Associativity: left

    -   **X inherits Y**\  
        Associativity: left\  
        Evaluates to true if X *is* Y or contains Y in its prototype chain.\  
        (Should this have Equivalence precedence? In JS *instanceof*
        it has relational precedence (which kinda makes sense, since
        we're checking for a parent/child "relation").)

    -   The `X=~Y` and `X!~Y` operators (dubbed "contains" and "does
        not contain," respectively) are left associative, *have no
        default implementations*, and must be
        [overloaded](#overloading) if used (else an exception is
        thrown). Their semantics are implementation-defined, and need
        not having anything to do with "containing" anything (they are
        call the "contains" operators only because they need a
        designation and we don't have a better one!).\  
        (Should these have Equivalence precedence? We have no
        precedent (as it were) for these. In Perl `=~` has a precedence
        between multiplication and unary +/-, but i don't
        \[\[currently \[, as of this writing \[(or during a later
        editing session)\],\]\] think i\] want that here. Hmmm. Then
        again, it is essentially a member function call, and could
        arguably be given Dot precedence.)

**Equivalence and Equality:**

-   `X==Y`, `X!=Y`, `X===Y` ("strict" comparison), `X!==Y` (strict not-equals)\  
    Associativity: left\  
    Strict comparisons enforce that both operands have the same type,
    so it performs no implicit conversions like some comparisons
    do. e.g.  (1=="1") but (1!=="1"). (Note that the core
    (non-overloaded) comparison operators *never* perform *implicit*
    conversions which require memory allocation, but will perform
    "small" to-string to string-to-number conversions in some contexts
    when they can be done on the stack. This basically means any types
    with a fixed memory size: numbers, booleans, `null`, and
    `undefined`.) Higher-level types *typically* never compare
    equivalent to one another using non-strict equivalence, except
    that [Buffers](type-buffer.md) compare equivalent if their
    buffered contents are equal, regardless of any member properties
    they each may have, and [Tuples](type-tuple.md) compare equal if
    all of their value entries do. (That might actually be a
    bug-in-waiting, as it means that script cannot tell the different
    between 2 Buffer or Tuple instances if they have the same
    contents, unless the script compares them, modifies one, then
    re-compares them.)

**Bitwise:**

-   `X|Y`, `X&Y`, `X^Y` (XOR)\  
    Associativity: left\  
    Always evaluate to integer values (*signed*, so be careful with overflow).

**Logical:**

-   `X&&Y` and `X||Y`. These evaluate to a boolean value, short-circuiting their RHS if they can.\  
    Associativity: left
-   `X|||Y` (a.k.a. "Or3") works like JavaScript (X||Y), evaluating to
    the same as (`X?X:Y`). Contrast with `||`, which always evaluates to a
    boolean. Has the same precedence as, and short-circuits like, the `||`
    operator.
-   `X?:Y` (a.k.a. "Elvis Operator")\  
    Associativity: left\  
    Functionally equivalent to (`(undefined===X) ? Y : X`). i.e. if
    `X` has no value assigned to it (or is assigned the `undefined`
    value) then it resolves to `Y`, otherwise
    to `X`. Has the same precedence as, and short-circuits like, the `||` operator.\  

**Assignment and Conditional:**

-   Ternary If (`IF ? THEN : ELSE`) with short-circuiting behaviour.\  
    Associativity: right\  
    The *THEN* and *ELSE* parts must each be a single expression.
-   Variable assignment: `X = Y`\  
    Associativity: right\  
    `X` must be a resolvable, non-const variable name (i.e. an identifier).
-   **Property Assignment:**

    -   `X.Y = Z`\  
        `X` must be a dereferencable value and Y must be property key
        (normally an identifier, but strings, integers, and doubles
        are also legal). If `X` is/inherits an array or string and `Y` is
        an integer then it is treated as an array resp. character
        index instead of a named property, acting on the array resp.
        string part of the value (possibly one of the value's
        prototypes, if it inherits from an array).

    -   `X[Y] = Z`\  
        Associativity: right\  
        This works just like the dot operator except that the `Y` part
        is evaluated as an expression before *its result* is used as a
        property key.

    -  `X.Y := Z`\  
       Associativity: right\  
       This works just like `X.Y=Z` except that the property is given
       the "const" flag, meaning that it cannot later be
       re-assigned. Such properties can, however, be removed by the
       `clearProperties()` object method (feature or bug?). Note that
       there is no binary form of this operator (i.e. `X:=Y`).

-   **Array-append Assignment:** `X[]=Y`\  
    Associativity: right\  
    PHP-style array-append. `X` must *be-a* array and the `[]` block must
    not contain an expressione (it may contain comments and spaces). This
    effectively resolves to (`X[X.#]=Y`) or (`X.push(Y)`), but is
    more efficient because it avoids an extra script-side function call.
-   **"Combo" assignments:**\  
    `X*=Y`, `X+=Y`, `X-=Y`, `X%=Y`, `X/=Y` , `X>>=Y`, `X<<=Y`, `X&=Y`, `X^=Y`, `X|=Y`\  
    Associativity: right\  
    `X` must an identifier or object property access expression, the value
    of which is overwritten by the result of the operation.

-   For *all* assignment operators:

    -   All evaluate to their RHS unless overloaded to do otherwise,
        but…

    -   Most overloaded implementations *must* do otherwise to support
        assignment semantics! See [the overloading
        section](#overloading) for important details regarding
        overloading of assignment combo operators.

    -   The LHS part of assignments must be in the same expression group
        as the assignment operator and the RHS must either be in the
        same subexpression or start a new one. e.g. `((x)=3)` will not
        work[^4], but `(x=3)` and `(x=(3))` will.

**Commas:**

-   Commas generally run a series of expressions left-to-right,
    evaluating to the last one in the list, with an effect similar, but
    not identical, to semicolons. However, comma interpretation is often
    handled in a context-specific manner. e.g. function arguments, var
    declarations, and literal arrays and objects each collect such lists
    differently. Comma-separated lists are never traversed/evaluated in
    any direction other than their byte order in the source code.

<a id="short-circuiting"></a>
## Short Circuiting

The logical operators and ternary-if support short-circuiting of their
RHS. When appropriate, they will short-circuit a single expression to
the right. When short-circuiting, blatant syntax errors are still caught
(and cause script failure), but the code being "skipped" is not "really"
evaluated, meaning that no semantics are applied to identifiers and
there are no side effects such as function calls or creation of new
values. e.g. unknown identifiers and illegal property accesses will not
trigger errors when being skipped, but two consecutive non-keyword
identifiers will (that's one of those "blatant" syntax errors mentioned
above). Some examples:

```s2
assert 'hi' === (true ? 'hi' : this+error * is / skipped);
assert 1 === (false ? obj.invalidProp.x.y.z() : 1);
assert !(false && eval another, error, skipped[^5]);
```

Remember that comparison and logical operators have a higher precedence
than ternary if, thus the extra parenthesis are needed on the first two
lines. Same goes for the `!` operator on the last line.

<a id="prop-access-ops"></a>
## Property Access Operators

There are several options for accessing properties of values:

Most simply:

```s2
obj.propName
obj.'propName'
obj['propName']
```

But more properly:

```s2
obj.identifier|String|Integer|Double[^6]
obj.(expr)
obj[expr]
```

Examples:

```s2
assert obj.x === obj.('x');
assert obj.x === obj.'x';
assert obj.x === obj['x'];
assert 6 === [0,2,4].1*3; // theArray[1] * 3 ⇒ 2 * 3 ⇒ 6
```

These operators can of course be chained:

```s2
obj.prop['sub'].subsub
```

Except when short-circuiting, an exception is thrown when attempting to
apply these operators to values/expressions which do not support them.
When short-circuiting, all property access is considered legal (but
always resolves to the *undefined* value internally).

Arrays treat integer property keys as array indexes and all others as
normal (object-level) properties. Arrays throw exceptions if given
negative indexes. In s2, any value type can be a property key, but there
are some caveats regarding how equivalence is checked. Namely, a
non-strict comparison is used, meaning (e.g.) that the integer property
1 and double property 1.0 are equivalent for *most* purposes, except for
array's special-casing of index properties (where it only recognizes
true integers). This can hypothetically (but in practice never has) lead
to a minor inconsistency: an array can have an integer property 1 (array
index) and a double property 1.0 (stored as an object property), whereas
an in any other non-[hashtable](type-hash.md) object those are equivalent property keys.

> Sidebar: "arrays", in the context of integer-type property lookups,
means any value which is of type Array or has such a type in its
prototype chain. The above-described behaviour applies to the first
Array value found in that Value's prototype chain, whether it's the
first one (as will be the case for "real" Arrays) or 5 levels up. There
has been some debate on whether this should only apply to Values which
are themselves really arrays, but (A) there are uses (admittedly few
good ones, though) for the current behaviour and (B) so far the current
behaviour hasn't caused any backfires. Anyone inheriting arrays is
assumed to know what they're doing.

<a id="hidden-props"></a>
## Hidden Properties

From the C level (not script level) it is possible to make certain
properties "hidden." Such properties will normally be invisible, in
that they do not show up in certain uses (e.g. property iteration or
JSON output), but they can be addressed by name if the name is known.
(Because cwal supports using any value type for property keys, it is
possible to create keys which neither script code nor external C code
can know (or even be able to formulate).) Note that hidden properties
are *not* exempt from being removed from client code, e.g. via direct
assignment over them, the `unset` keyword, `Object.unset()`, or
`Object.clearProperties()`.

<a id="overloading"></a>
# Operator Overloading

**This support is optional - feel free to skip it.** Internally we use
operator overloading to implement the `+` and `+=` operators for strings
(and, in the meantime, several other operators), so this feature is
unlikely to outright disappear, but its conventions are up for tweaking.

ACHTUNG: overloaded operators used to be passed their `this` value as
their own first argument and their operand, if any, as the second
argument. Operators no longer explicitly pass in their own `this` as
an argument. While explicitly passing on `this` initially sounded like
The Right Thing To Do, it turned out to just muddle up script code
with unused (but required) parameters.

Why support operator overloading at all, when many programmers (not me!)
despise operator overloading? Because it fits easily within the overall
framework and is interesting to experiment with. It's certainly not a
required feature in order to use s2 effectively, and should be
considered a "syntactic sugar" feature (except that string concatenation
via the binary + operator is implemented via an operator overload in the
string prototype).

s2 has support for overloading the majority of the basic operators for
arbitrary [container values](type-intro.md) using script-side
functions. s2 will in fact throw an error if the LHS of an overloadable
operator (resp. the RHS for prefix operators) is not a "simple" type and
does not implement that operator, under the assumption that the
operation has no useful semantics. In that sense, it *requires*
overloading if one wants to use a higher-level data type with most
operators.

When and where operator overloads are checked:

-   Only for the LHS of a binary operator or the RHS of a prefix
    operator. It *never* looks for an overload in the RHS of a binary
    op, regardless of commutativity of that operator with regards to
    math rules.
-   Any operator overloads defined for the numeric types (via their
    prototypes), booleans, `null`, and `undefined`[^7] are
    *ignored*, to avoid that clients "change the physics" of the script
    via replacing the core operators. Those types always use the
    built-in operators (for the sake of sanity, not necessity… unless
    one considers sanity a necessity).
-   The string type allows only a small subset of operators to be
    overridden. It implements the binary `operator+` by default and may
    optionally be extended with `operator*`. Whether to allow other
    (non-assignment) operators solely for overloading use is under
    consideration. Strings fall back to numeric interpretation for other
    numeric operators (0 if it is not syntactically a number). Note that
    the prefix unary minus and plus can be used to coerce a string into
    a numeric value, e.g. `+"1" === 1` and `-"-1" === 1`.
-   Most other types implement no overloads by default and will trigger
    an error if used in a context which requires an operator
    implementation.

The generic process for overloading is the same for all operators:
assign a function to a property with the operator's name (all are listed
below). Operators which can be overloaded may use that proxy under the
conditions described above. The return result of an overload operator
becomes the result of the operation.

Notes and caveats:

-   Save memory by adding overloads to prototypes, instead of individual
    instances, where it makes sense to do so (e.g. for custom classes or
    singleton/one-shot objects).
-   When an assignment operator, e.g. `X+=Y`, is overloaded, it is *almost
    always important that the overload return its `this` value*, instead
    of the new value. Failing to do so will lead to the assigned-to
    variable or property getting overwritten with the result of the
    operator.
-   There is currently no support for automatically making use of
    simpler operators to resolve missing compound operators. e.g. s2
    currently requires `<` and `<=` to be implemented separately (if
    both are needed), though in principle it should be able to use the
    `<` and `==` operators (if available) to automatically infer that
    operator. (An attempt was made to do so, but different return
    semantics requirements made it unfeasible.) The implication is
    that if a value participates in both (e.g.) `<` and `<=`
    operations, both must be overloaded (but such overloads may in
    turn use the other overloaded operators).
-   Be careful to avoid operator overload infinite recursion, in
    particular when evaluating arbitrary foreign values which might or
    might not be of the same type, or use operators which eventually
    trigger the one currently running. It's hypothetical so far, but
    certainly possible.
-   See the following subsection for operators which exist *only* to be
    overloaded.

The list of overloadable operators follows. The name, up to the first
parenthesis, is the operator's property name. The parameter list shows
what (if anything) gets passed to the operator:

-   "rhs" refers to the right-hand side of a binary operator.
-   "self" is the value which overloaded the operator. Note that 'this',
    in the context of an overloaded operator call, is the same as the
    'self' value (if any) passed to it (i.e. this===self), and script
    code may refer to either one. This parameter is historical - it is
    not used since 20190804.

The operators:

-   `operator->(rhs)`
-   `operator::(rhs)`
-   `operator+(rhs)`
-   `operator+=(rhs)`
-   `operator-(rhs)`
-   `operator-=(rhs)`
-   +`operator()` (unary)
-   -`operator()` (unary)\  
    (Design note: unary +/- are separate from binary +/- only as an
    optimization, to avoid clients having to check for both cases, as it
    is thought unary ops will only rarely be overloaded.)
-   `operator++()` (postfix) (ACHTUNG: prior to 20190804, this was
    *operator++(self)*)
-   `++operator()` (prefix) (ACHTUNG: prior to 20190804, this was
    *operator++()*)
-   `operator--()` (postfix) (ACHTUNG: prior to 20190804, this was
    *operator--(self)*)
-   `--operator()` (prefix) (ACHTUNG: prior to 20190804, this was
    *operator--()*)
    -   Note that the distinction between overloaded prefix and postfix
        ++/-- is not terribly useful in s2 because the operators
        *must*, when overloaded, returns their `this` object for
        proper semantics.
-   `operator*(rhs)`
-   `operator*=(rhs)`
-   `operator/(rhs)`
-   `operator/=(rhs)`
-   `operator%(rhs)`
-   `operator%=(rhs)`
-   `operator<<(rhs)`
-   `operator<<=(rhs)`
-   `operator>>(rhs)`
-   `operator>>=(rhs)`
-   `operator<(rhs)`
-   `operator<=(rhs)`
-   `operator>(rhs)`
-   `operator>=(rhs)`
-   `operator==(rhs)`
-   `operator!=(rhs)`
-   `operator&(rhs)`
-   `operator&=(rhs)`
-   `operator|(rhs)`
-   `operator|=(rhs)`
-   `operator^(rhs)`
-   `operator^=(rhs)`

We can use overloading to emulate, for example, C++ stream-style output
stream operators. The following example uses one more layer of
indirection than is strictly needed, but only for demonstration
purposes:

```s2
// Some arbitrary output-streaming function...
var f = proc me(){
 return me.b.append(argv.0);
};
// A buffer for our demo:
f.b = new s2.Buffer(100);
// The operator:
f.'operator<<' = proc(arg){
 this(arg); // extra indirection only for demo purposes.
 return this; // for chaining to work
};
// C++-like output streams:
f << "a" << "bc" << "def";
assert "abcdef" === f.b.toString();
f.b.reset();
f << 1 << 2+3;
assert "15" === f.b.toString();
// Alternate (simpler) approach:
var o = {
  buf: new s2.Buffer(20),
    'operator<<': proc(arg){
    this.buf.append(arg);
    return this;
  },
  reset: proc(){
    this.buf.reset();
    return this;
 }
};
```

(Note that in the meantime, the Buffer class overrides
`operator<<` to append its argument to the buffer.)

This incidentally allows simplified output of huge heredocs:

```s2
f << <<<EOF ...arbitrarily long heredoc spanning tens of
lines... EOF;
```

Note that a call made in that form needs no closing parenthesis at the
(far-away) other end. On the other hand, it's likely only half as fast
because it's got twice the function call overhead due to the extra level
of indirection the overload adds.

<a id="overload-only-ops"></a>
## Overload-only Operators

s2 supports the concept of "overload-only" operators, meaning that it
has a slot for them in the operator table, but it does not implement
them by default. Using them will cause an exception to be thrown unless
the LHS (for binary) resp. RHS (for unary prefix) implements the given
operator.

The following operators are currently implemented as overload-only,
denoted using the same naming/parameters conventions as in the full
operator list above. See the [operator overview](#operators) for
precedence and associativity information.

-   `operator->(rhs)`
-   `operator::(rhs)`
-   `operator=~(rhs)`
-   `operator!~(rhs)`

s2 applies no specific semantics to these operators - those are defined
by the implementor. It makes one exception for the `::`, in that an
identifier on the RHS of the `::` operator is treated just like an RHS
identifier for the dot operator. Namely, the identifier is not resolved
when evaluated, but is instead passed on to the dot/`::` ops as the lookup
key. Contrast with the `->` operator, where s2 evaluates the RHS
identifier before passing its result value to the operator. In other
words `foo.bar`, `foo->'bar'`, and `foo::bar` (note that the latter
has no quotes) all end up passing 'bar' to the associated operator.



# Footnotes

[^1]: all `{}`, `[]`, and `()` in s2 have, because of how they are
tokenized, have a *virtual EOF* just before the closing character,
and thus explicit end-of-expression tokens are not necessary in any
such contexts (and are errors in some contexts).

[^2]:  It "could" be made to, but making it so is rather more intrusive
than i would like. It affects all of the combo assignment ops as
well and a nice solution for that particular case is not immediately
clear.

[^3]:  C99 has such support, but cwal/s2 are, so far, strictly C89.


[^4]:  Because the result of `(x)` is the value `x` refers to, instead of
the identifier `x`, and assignments cannot work with that.

[^5]: Careful - [eval](keyword-eval.md) and friends consume commas on
    the RHS!


[^6]: There is a minor ambiguity with literal *doubles*: (`foo.1.2.3`)
will be recognized as a series of 5 tokens (`foo . (1.2) . 3`) by the
tokenizer. Such usage is expected to be rare, and one can use
`foo[1][2][3]` or (depending on intent) `foo[1.2][3]` instead.

[^7]: Currently booleans, null, and undefined have no prototype but
    one may eventually be added, if only to add a toString() method for
    consistency with other types.*
