# s2: typeinfo()
#### ([&#x2b11;Table of Contents](./))
# `typeinfo()`: Value Meta-info Queries

* [Introduction to `typeinfo()`](#keyword-typeinfo)
* [`typeinfo()` Tags](#keyword-typeinfo-tags)

<a id="keyword-typeinfo"></a>
# `typeinfo()`

It is often useful to be able to ask questions about the *type* of a
value, e.g. to figure out if it's safe to call it like a function or
see if it's a string or a number. This capability is provided by the
`typeinfo` keyword, which is capable of querying all sorts of
information about types and some non-type-related metadata (`typeinfo`
is a slight misnomer - it perhaps ought to have been called `valinfo`
or `valueinfo`).

It is used like this:

```s2
const answer = typeinfo(TAG EXPR);
```

Where `TAG` is a pseudo-keyword (listed below) used only in the context
of `typeinfo` (they are normal identifiers in all other contexts). The
operand is *normally* an arbitrary expression, but some tags expect a
specific type of token, e.g. an identifier, or no operand at all. Each
tag tells `typeinfo` which tidbit of metadata to resolve to regarding
the value being queried, and most resolve to a boolean. While this is
splitting hairs, note that the query is applied to the `result` of its
expression operand, not to the expression itself. i.e. its operand does
get evaluated (except when short-circuiting is in effect).

`typeinfo` is a function-like keyword, but does *not* start a new
scope in its argument list like a function call does. It is most often
used for function parameter validation and/or dispatching. For
example, basic argument validation might look like:

```s2
function(a,b){
 affirm typeinfo(isstring a);
 affirm typeinfo(isnumeric b);
 …
};
```

> Tip: such uses should prefer `affirm` over `assert`, so that failure throws
an exception instead of a fatal error.

> Design note: this keyword originally did not use parenthesis, but
experimentation showed that that could lead to some potentially
ambiguous interpretations, e.g. `typeinfo xyz def || abc` would (for
consistency with other keywords) need to parse the `||` as part of
`typeinfo`'s RHS, but most real uses would probably intend `(typeinfo
xyz def) || abc`. The decision to leave out a comma between the tag and
expression was made after experimentation showed the comma to just add
noise. The parenthesis, while adding noise, also serve to disambiguate
the operand from any other RHS part.

<a id="keyword-typeinfo-tags"></a>
# `typeinfo()` Tags

The current tags are listed in full below but first a description of
their naming conventions is in order. The tags are all lower-case.
**Most start with "is" or "has", and both of those have similar meanings
best explained with an example:** Consider the *isarray* and *hasarray*
tags: an Array value (created via an array literal or the C-level
`cwal_new_array()`) "is" an array, whereas a non-array value which
includes an Array in its prototype chain "has" an array (and can be
treated like one for many purposes). In s2, the "integer property is an
array index" behaviour of property lookup applies to both types of
arrays: "real" ones and values which inherit an array (in which case the
inherited array is the one the index operations are applied to!). The
relation between "is" and "has" is subtly different from the `inherits`
operator, which checks whether a value contains a certain exact value
within its prototype chain, whereas these tests (mostly) check for
type-level attributes, not attributes of concrete instances. e.g. what
makes an array an Array is *really* the fact that it has the C-level
type ID `CWAL_TYPE_ARRAY`. Whether or not it behaves like an array in
s2 depends largely on its prototype, where most script-visible behavior
is implemented (modulo the array-append operator and
integer-property-key case, which are handled in one of s2's lowest
abstraction-layer levels and use the same logic the "hasarray" tag
uses).

On 20200111, `typeinfo` tag names were extended to offer hyphenated
forms to take advantage of a new feature which allows identifiers, in
limited contexts, to contain hyphens. These are functionally identical
to their older variants but may be more readable, especially for the
longer tags. e.g. `typeinfo(cannew)` and `typeinfo(can-new)` are
equivalent. The list below shows both forms because the older form
is prevalent in client-side scripts and is not going anywhere anytime
soon (it's not deprecated).

The tags and what they resolve to are listed here in alphabetical order
below. <u>***All of them expect an expression after the tag unless noted otherwise***</u>
(some require an identifier):

```s2-member
cannew
can-new
```
`true` if the operand is legal for use as an operand for the `new` keyword, else `false`.

```s2-member
hasarray
has-array
```

`true` if the operand is an array or has an array in its prototype chain.

```s2-member
hasbuffer
has-buffer
```

`true` if the operand is a buffer or has a buffer in its prototype chain.

```s2-member
hasenum
has-enum
```

`true` if the operand is an enum or has an enum in its prototype chain.

```s2-member
hasexception
has-exception
```

`true` if the operand is an exception or has an exception in its prototype chain.

```s2-member
hashash
has-hash
```

`true` if the operand is a hash or has a hash in its prototype chain.

*Achtung:* though [enums](type-enum.md) *may* internally be hashes,
this predicate and `is-hash` evaluate to `false` for enums because
enums are not treated as hashes in all contexts.


```s2-member
hasnative
has-native
```

`true` if the operand is a native or has a native in its prototype chain.

```s2-member
hasobject
has-object
```

`true` if the operand is an object or has an Object in its prototype chain.

```s2-member
hasprototype
has-prototype
```

`true` if the operand has a prototype. Booleans, `null`, and `undefined`
have no prototypes, nor do the top-most prototypes in any chain, and
prototypes may be (and often are) removed from various objects.

```s2-member
isarray
is-array
```

`true` if the operand is an array, i.e. created via an array literal or
the C-level `cwal_new_array()`.

```s2-member
isbool
is-bool
```

`true` if the operand is a bool.

```s2-member
isbuffer
is-buffer
```

`true` if the operand is a buffer.

```s2-member
iscallable
is-callable
```

`true` if the operand is "callable" like a function.
i.e. if it is a function or has a function in its prototype chain.

```s2-member
iscontainer
is-container
```

`true` if the operand is a container, i.e. can hold its own properties.

```s2-member
isdeclared IDENTIFIER
is-declared IDENTIFIER
```

`true` if the given identifier names a declared (in-scope)
variable/const. Note that this tag accepts only a single identifier,
not an arbitrary expression.

```s2-member
isderefable
is-derefable
```

`true` if the operand is legal for use with the property access
operators (`X.Y` and `X[Y]`). Note that numbers and strings are
derefable but are not containers - property lookups resolve through
their prototype(s).

```s2-member
isdouble
is-double
```

`true` if the operand is (strictly) a double.

```s2-member
isenum
is-enum
```

`true` if the operand is an enum.

```s2-member
isexception
is-exception
```

`true` if the operand is an exception.

```s2-member
isfunction
is-function
```

`true` if the operand is a function. (Note: there is
no *hasfunction* tag, but `iscallable` does what that tag would do.)

```s2-member
ishash
is-hash
```

`true` if the operand is a hash, i.e. created via a hash
literal, `new s2.Hash()` (or equivalent), or the C-level
`cwal_new_hash()`.

*Achtung:* though [enums](type-enum.md) *may* internally be hashes,
this predicate and `has-hash` evaluate to `false` for enums because
enums are not treated as hashes in all contexts.


```s2-member
isinteger
is-integer
```

`true` if the operand is (strictly) an integer.


```s2-member
isiterating
is-iterating
```

Equivalent to
`typeinfo(isiteratinglist EXPR) || typeinfo(isiteratingprops EXPR)`.

```s2-member
isiteratinglist
is-iterating-list
```

`true` if the operand is a data type which has a
list and is currently iterating over it. This includes arrays,
tuples, and hash tables when iterating over their hash properties
(as opposed to their object properties).

```s2-member
isiteratingprops
is-iterating-props
```

`true` if the operand is a data type which can
contain object-level properties and is currently iterating over
those properties (as opposed to hashtable entries).


```s2-member
islist
is-list
```

`true` if the operand is an array or tuple. It is
equivalent to `typeinfo(isarray X) || typeinfo(istuple X)`. Hashes
also manage a list, but they are not considered lists for this
purpose (though they are for `isiteratinglist` and
`mayiteratelist`).

```s2-member
islocal IDENTIFIER
is-local IDENTIFIER
```

`true` if the given identifier is the name of a var/const declared *in
the current (local) scope*. Be aware that `if(typeinfo(islocal x))` is
not generally useful because `if()` starts a new scope at its opening
parenthesis. Similarly, `for(var x…; …; …) {typeinfo(islocal x)...}`
is not useful because `for()` loops start a new scope after the first
`;` *and* on each iteration. Because of that, the expected usages of
this query are something like:

```s2
typeinfo(islocal foo) || (var foo = ...);
typeinfo(islocal bar) && (foo = bar);
// Emulating a longer if(...):
typeinfo(islocal bar) && scope {...};
```

```s2-member
isnative
is-native
```

`true` if the operand is a Native. Natives are the
generic type used to bind things for which cwal has no built-in type
equivalent. e.g. instances of s2's [PathFinder class](type-pathfinder.md)
are Natives and several of the [loadable modules](../mod/) make use of
Natives.


```s2-member
isnewing [EXPR = this]
is-newing [EXPR = this]
```

`true` only if the expression's value (default is the current "this"
value) was created by the [`new` keyword](keyword-new.md) for a
constructor call and that value's constructor is currently
running. i.e. its intended use is `typeinfo(isnewing)` from inside
constructor methods intended for use with the `new` keyword. If this
evaluates to `false`, the constructor was not called via `new`, which
means its call-time `this` is likely different than a constructor
expects. Constructors can use this test to change behaviours (or throw
an error) depending on whether they are being `new`'d or not.

Semantics changed on 20171206: prior to that, `isnewing` did not work
in a subscope of a constructor. e.g. `if(typeinfo(isnewing)){...}`
would previously never trigger because the `X` in `if(X)` is evaluated
in a subscope.

```s2-member
isnumber
is-number
```

`true` if the operand is (strictly) an integer or a double. Contrast
with...

```s2-member
isnumeric
is-numeric
```

`true` if the operand is an integer, a double, a boolean, or a
numeric-format string (one parseable by
[`parseNumber()`](type-number.md)).


```s2-member
isobject
is-object
```

`true` if the operand is an Object, i.e. created via an object literal
or the C-level `cwal_new_object()`.

```s2-member
isstring
is-string
```

`true` if the operand is a string.

```s2-member
istuple
is-tuple
```

`true` if the operand is a Tuple value.

```s2-member
isunique
is-unique
```

`true` if operand is a Unique-type value. In s2, currently the only
way to create these is via enums (their entries are are of this type)
or from C code (using `cwal_new_unique()`).

```s2-member
name
```

Evaluates to the type's name, just like the deprecated *typename*
keyword (which this tag replaces).

If an object has or inherits a property named `__typename`,
that property's value is used by this query.

This is one of the few contexts where referencing an unknown
identifier will *not* cause an error - it results in the string
`"undefined"` instead. So `"undefined"===typeinfo(name foo)` can be
used to see if `foo` is declared, but it will also be `"undefined"` if
`foo` has the `undefined` value. Type name fetching cannot tell the
difference between is-not-defined vs. is-undefined, but
`isdeclared` and `islocal` can.


```s2-member
mayiterate
may-iterate
```

Equivalent to `typeinfo(mayiterateprops EXPR) || typeinfo(mayiteratelist EXPR)`
except that the `EXPR` is only evaluated once.


```s2-member
mayiteratelist
may-iterate-list
```

`true` if its operand is a value type which manages a list and that
list is currently capable of being iterated over, else `false`. This
includes arrays, tuples, and hashes (which are a list, of sorts). Note
that the length-0 tuple, a shared constant, is reported as being
iterable but it has no entries, so
iteration on it (with [`foreach`](keyword-loops.md#keyword-loops-foreach)) is a no-op.

Currently (201912) the only operation which blocks list iteration is
[Array.sort()](type-array.md#array-methods).

```s2-member
mayiterateprops
may-iterate-props
```

`true` if its operand is a value type which is capable of holding
object-level properties and iteration over those properties is
currently legal, else `false`.

The semantics of when it is legal to iterate changed (for the better)
on 20191211, in that it is now possible to iterate concurrently
multiple times over the same value's properties, so long as a specific
operation is not blocking that. As of 20191212, there are no such
blocking operations, but any operations which would modify a
container's property list during iteration are (still) illegal (the
data model does not support modification during iteration). Part of
that change was the requirement for different `typeinfo` tags to check
for property-vs-list iteration capabilities/status. Previously there
was an internal ambiguity which caused the underlying flag for this
query to apply to both object properties and list operations.

```s2-member
refcount
```

Has (as of 20191230) been deprecated [in favor of
`pragma(refcount)`](keyword-pragma.md#pragma-refcount).

---

[This test script](/finfo/s2/unit/099-000-typeinfo.s2) demonstrates
most of the above, and [this one demonstrates cannew and
issnewing](/finfo/s2/unit/400-000-new.s2).

*Potential* TODO tags include:

-   `isconst IDENT | prop access`: true if var/property is const. The
    problem with properties is that this would currently require two
    lookups for property access, due to how the EXPR is processed.\  
    Reminder to self: i think that can be optimized by adding
    `s2_engine::dotOpKvp`, pointing to the KVP the lookup was for, BUT
    that would have to be done in `s2_get_v()`, which is not specific to
    the dot operator. Hmm. Maybe add `s2_engine::getKVP`, holding only
    the most recent KVP (from objects, hashes, or scopes). Fixing that
    could potentially also allow the `name` query tag to distinguish
    between undeclared props and those the undefined value. Holding that
    pointer is inherently dangerous, though, as any change to its
    container may invalidate it. (Oh, we just need an `s2_get()` variant
    which returns the KVP, and the underlying cwal-level APIs already
    support that for all our use cases. Maybe someday we'll experiment
    with that.)
