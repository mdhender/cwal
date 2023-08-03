# s2: enums
#### ([&#x2b11;Table of Contents](./))
# The Enum Type

Jump to...

* [Enums](#type-enum)
* [Enum Members](#type-enum-members)
* [Enum Tricks](#type-enum-tricks)

<a id="type-enum"></a>
# Enums

Enums were added to the trunk on 20160107.

In s2, "enums" are a special-case type of object intended to denote an
immutable set of globally unique keys, optionally with values associated
to them. They are notably different from C/C++ enums, being more like
(but not identical to) Java enums. Unlike conventional enums, s2 enums
do not have an inherent ordinal/integer value, but each may optionally
have a single value (of any type) associated with it. An enum can be
thought of as a special-case object with an immutable set of keys which
triggers an error if any unknown key is referenced.

Each enum entry is of a cwal type called (for lack of a better name)
"unique." A "unique" value is intended primarily to act as a unique
key which will never compare as equivalent to *any* other value. Each
entry may optionally wrap a single value of any type. Referencing an
enum entry via the property access operators, e.g. `myEnum.X`,
resolves to the entry itself (its *unique identity*), and accessing
the `value` pseudo-property of that entry resolves to its wrapped
value (if any). (Examples are below.)

Enums are different from other types of containers in <s>several</s> a
number of notable ways:

-   Each entry in the enum is of *an opaque type*, with a single
    property, and guaranteed to never compare equivalent to any value
    other than itself. Unlike enum entries in C and Java, **s2 enum
    entries themselves have no intrinsic value other than their
    identity**. Each is guaranteed to evaluate to `true` in a boolean
    context (even though it will never *compare as equivalent* to
    `true`!).
-   Each enum entry may optionally **wrap a single value**.
-   **The association with a value is immutable** (specified only when
    the enum is created), but that value's contents may be modified if
    it is a mutable type.
-   The enum's typename can optionally be set by passing an identifier
    (the type name) right after the `enum` keyword. The identifier's
    string form will be the result of passing the enum to the
    `typeinfo(name …)` keyword:\  
    ```s2
    const myEnum = enum こんにちは { ☺, Æ, ©, … };
    assert 'こんにちは' === typeinfo(name myEnum);
    ```
-   Each enum contains a reverse mapping of its (opaque) values to their
    names: **`myEnum[myEnum.EntryName]` resolves to the entry's
    (string) name** (e.g. "EntryName" in that example).
-   <s>**Whether an enum uses an Object or Hash as storage is
    undefined**.</s> No longer true: As of 2020-02-21, enums are
    always implemented as hashes. Their entries are accessed using the
    dot operator (or equivalent). However, the `typeinfo(ishash)` and
    `typeinfo(hashash)` type queries explicitly evaluate to `false`
    for enums, even if the enum is internally a hash.
-   **Accessing an unknown property with the dot op triggers an
    exception**, making it impossible to use an invalid enum entry.
-   **Trying to set or remove any property on an enum triggers an
    exception**, *except* that...
-   **The prototype may be swapped out after enum creation, but...**
    Whether or not that's a feature or bug (in the form of an
    inconsistency) is as yet undecided and *this aspect may change in
    the future*.
-   **The `->` operator is overloaded** to provide a non-throwing 
    lookup alternative, as explained in the following section.
-   **The `::` operator is overloaded** to return the value wrapped by
    a given enum entry. e.g. `myEnum::X` is equivalent to
    `myEnum.X.value`. Like the dot operator, it throws for unknown
    properties.
-   **The `.#` operator** evaluates to the number of user-defined
    entries in the enum, not counting the internal reverse
    identity-to-name mappings. i.e. `enum {a,b,c}.# === 3`.
-   The hash search operator (`#`) *explicitly does not work on
    enums*, even though they are internally implemented as hashes. Use
    the `.`, `[]`, `->`, and `::` operators to access their elements.
-   Unlike in most languages, **the order in which enum entries appear
    is irrelevant** except for determining their initialization order
    (they are initialized from in the same relative order they appear in
    the input source byte stream). Their order in the enum does not
    guaranty whether one or the other compares as less than or greater
    than another (they never compare as equal/equivalent). Their
    comparison ordering is based (internally) on their pointer address
    or some other internal value, and there is no guaranty whatsoever
    that subsequent elements have ascending values.

A brief example:

```s2
const e = enum OptionalTypeName {
    a, // its own name (a string) as its value
    b: 2, // any value type is fine
    f: proc(){return this}
};
assert 3 === e.#;
assert 'a' === e.a.value;
assert 'a' === e::a; // equivalent
assert 2 === e.b.value;
assert 2 === e::b; // equivalent unless the entry.value is a Function call:
assert e.f === e.f.value(); // e.f is bound as 'this'
assert e::f === e::f(); // no 'this', so e.f.value is its own 'this'
assert 'b' === e[e.b]; // get the string-form name of an entry
assert e.a === e['a']; // note that e['a'] is functionally identical to e.a.
assert 'OptionalTypeName' === typeinfo(name e);
assert undefined === e->'c'; //-> op does not throw for unknown properties
assert catch {e#'c'}.codeString()==='CWAL_RC_TYPE'; // hash op not allowed
assert catch {e::c}.codeString()==='CWAL_RC_NOT_FOUND'; // unknown property
assert catch {e.c}.codeString()==='CWAL_RC_NOT_FOUND'; // unknown property
assert catch {e.a = 1}.codeString()==='CWAL_RC_DISALLOW_PROP_SET';
```

Enums treat standalone keys (without values) differently than objects
do: an entry entry with no value gets (as of 20171204) the string form
of its identifier as its value. e.g. `enum{x}` has a single entry,
`x`, with a string value of `x`. (This change was made after practice
showed that them defaulting to `undefined`, as they did before, was
pretty useless.)

A feature-complete example can be found [in the s2 unit
tests](/finfo/s2/unit/070-000-enum.s2).

Note that enum entries never compare equivalent to any other value, and
yet still evaluate to `true` in a boolean context:

```s2
assert true === !!e.a; // will pass, but…
assert true == e.a; // will fail!
assert false == e.a; // will also fail!
```

Open points:

-   We need(???) a way to do inheritance which keeps the
    throw-on-unknown-props behaviour. Subclassing an enum hides that
    behaviour unless the enum part of the prototype chain is used
    directly. Inheritance of enums "loses" their throw-on-unknown-props
    and throw-on-write when the access goes through the derived part.
    This is a corner case, but how significant of one?


<a id="type-enum-members"></a>
# Enum Members

Enums all share a common prototype (which itself has no prototype) with
the methods listed below. The prototype may be modified further by
clients.

```s2-member
Enum eachEnumEntry( Function(entryName, entryIdentity) )
```

For each distinct entry in the enum, this routine calls the given
callback, passing it the enum entry name and its unique identity (in
that order). The 2nd argument is an enum entry, so to get its wrapped
value (if any), fetch its `value` property. Propagates any
exceptions. If the callback returns a literal `false` (not another
falsy value) then iteration stops without an error. Note that while
each enum holds a reverse mapping of its entries to its names, the
callback is not called twice by this method - it is only passed they
name/identity mappings, not the identity/name mappings. In the context
of the callback, the enum itself is bound to `this` and
`eachEnumEntry()` returns its `this`. Remember that the order of
enum entries is indeterminate, meaning the callback will *not* be
passed the entries in a well-defined order.

Note that `foreach(anEnum=>name,identity)` is much more efficient than
using this function (but this method predates that feature).

> Potential TODO(???): in the context of the callback, make `this` the enum
entry (the Unique-type value).

```s2-member
Array getEnumKeys()
```

Returns an array containing the names of all entries in the enum (in
an unspecified order, since they come from storage with unspecified
ordering).

```s2-member
bool hasEnumEntry(key)
```

Returns `true` if the enum part of this value (not including
prototypes or derived parts) contains the given key, else `false`.
Note that this will resolve both the "forward" and "reverse" enum
entries, so the key may be either an entry's name or its identity.

```s2-member
mixed operator->(key)
```

Overloaded to perform a *non-throwing* enum entry lookup, only
checking for enum properties (only those listed in the body of the
enum). Note that this will resolve both the "forward" and "reverse"
enum entries, so the key may be either an entry's name or its identity.
It will return the entry it finds, or the `undefined` value for
unknown entries. Notes:

- *The `->` operator does not treat its RHS like a property key*, so
  the RHS needs to be quoted unless it is an identifier which resolves
  to an enum entry name or value. e.g. use `myEnum->'EntryName'`
  instead of `myEnum->EntryName`, unless `EntryName` is a variable
  which resolves to a string or enum entry value.
- *The enum is not bound as the `this` of the result* (which doesn't
  *really* matter because it will never resolve to a method, only an
  enum entry).
- This will resolve both the "forward" and "reverse" enum entries, so
  the key may be either its name or its identity (and its counterpart
  will be returned).
- This operator (or the `hasEnumEntry()` method) can be used to
  distinguish between derived properties resolved via the dot operator
  and pure enum entries (the only thing this operator will resolve).


```s2-member
mixed operator::key
```

Overloaded to resolve to the value wrapped by the given key in the
enum, rather than resolving to the entry itself. The RHS of the this
operator may be an identifier literal or an expression which evaluates
to a string value. e.g. given `enum e{a:1}`, then: `e.a.value ===
e::a` and `e::a === e::'a'` and `e::a === e::(e[e.a])`. Like the dot
operator, it throws if the given property is not in this enum.


<a id="type-enum-tricks"></a>
# Enum Tips and Tricks

## Enum Entries without Enums

Currently, the only mechanism s2 provides for creating values of the
type used for enum *entries* is constructing an enum. Enum entries
need not be left tied to their initial enum, though. They can be
copied around just like any other properties, and can be used as both
keys and values:

```s2
var e = enum { a, b, c };
var x = {};
var a = [];
foreach(e=>k,v) { x[k] = v; x[v] = k; a.push(k, v) }
// or: e.eachEnumEntry(proc(k,v){x[k]=v; x[v]=k; a[]=k;a[]=v});
assert x.a === e.a; // === enum entry
assert x[e.a] === e[e.a]; // === reverse mapping, i.e. "a"
assert a.indexOf(e.a) >= 0;
unset e; // x/a both still hold refs to the enum entries
assert a.indexOf('b') >= 0;
```

If script code needs some guaranteed-unique key, enum entries can be
useful even if one doesn't really need an enum (simply discard the enum
after extracting the keys).

## "Sealed" Objects

An enum is immutable, making it potentially suitable for use as a
"sealed" configuration object, namespace, or similar. The notable wart
there is the requirement to use `enumEntry.value` to access the
underlying values:

```s2
const e = enum {
 f: function(){...}
};
e.f.value(); // kinda ugly
```

Hmm. Necessity being the mother of invention, that led to the the `::`
operator being introduced. See the next section…

## The `::` Operator and Binding of `this` in `enum.entry.value()` Calls

To simplify access to `enum.entry.value`, enums overload the `::`
operator so that we can do...

```s2
const e = enum {
 a: 1,
 f: function(){...}
};
assert e.a.value === e::a; // equivalent operations
e::f(); // *almost* equivalent to e.f.value()
```

The minor difference between `e.f.value` and `e::f` comes when they
resolve to a Function which is called in the next operation. The
`e.f.value()` form will bind the enum entry `f` as the `this` of the
function call, where `::` does not (which means that `f.value` becomes its
own `this` in the context of a call). A demonstration of that
difference:

```bash
[stephan@host:~/fossil/cwal/s2]$ ./s2sh ...
s2sh> var e = enum {f:proc(){print(this)}}
s2sh> e.f.value()
unique@2211050
s2sh> print(e.f)
unique@2211050
s2sh> e::f()
function@22332B0
s2sh> print(e::f)
function@22332B0
```

In practice that makes effectively no difference, as such functions
are unlikely to be able to do anything interesting with `this` in
either case. If the embedded functions need access to the outer enum,
it can be provided after initialization of the enum, like so:

```s2
const e = enum {
 f1: proc(){assert 'enum' === typeinfo(name E)},
 f2: proc(){assert 'unique' === typeinfo(name E.f1)}
};
const imports = {E:e};
foreach(e=>k,v) v.value.importSymbols(imports)
/**
  note: we don't inline the imports Object in importSymbols argument
  list b/c that would create a new wrapper Object and "E" key for each
  iteration!

  Note that importSymbols() will overwrite any symbols which were
  installed in such functions via the "using" modifier.
*/;
```
