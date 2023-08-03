# s2: Tuples
#### ([&#x2b11;Table of Contents](./))
# The Tuple Type

Jump to...

* [Tuple](#type-tuple)
* [Tuple Methods](#type-tuple-methods)


> Sidebar: the usage of the term "tuple" in this API almost certainly
does not align perfectly with [the formal definition/description](https://en.wikipedia.org/wiki/Tuple).
Oh, well.

<a id="type-tuple"></a>
# Tuple

(Added 20160227.)

Tuples are basically lightweight, fixed-length arrays:

-   The length of any given tuple is **fixed at construction**.
-   They are not full-fledged containers and **may not hold their own
    properties**. This also means they cannot be used as prototypes for
    other values[^47].
-   They overload the **comparison operators** to compare tuples in
    conventional ways.
-   Two tuples **compare equivalent** only if all elements in each tuple
    compare equivalent (in the same order). A shorter tuple is always
    (in cwal) "less than" a longer tuple.
    - ***ACHTUNG***: because the contents of a tuple may change and
      their comparison depends on their contents, *tuples are not
      suitable for use as property keys*. The API does not disallow
      their use as keys, but *results are undefined* if the contents
      of a tuple are modified while it is being used as a key for an
      object property or hashtable entry.
    - ***ACHTUNG***: the type-strict `===` comparison applies the
      same-type semantics only to the tuples being compared, not their
      values.
    - Potential TODO: explicitly disallow tuples as property keys.
-   Unlike arrays, using an **out-of-bounds index** will result in an
    exception, not the undefined value.
-   The **empty (length-0) tuple** is a built-in constant value, like
    the empty string is. [`typeinfo(mayiterate
    [#])`](keyword-typeinfo.md) will report it as iterable, but
    iteration is a no-op because there is nothing to iterate over.
-   **Length is limited** to 16 bits worth of entries (64k).

The primary use case for tuples is APIs which may create many lists
which won't ever need to grow or shrink and don't need the full range
of [Array](type-array.md) functionality. Tuples can save a bit of
memory, compared to arrays, for such cases (as of this writing 32
bytes per instance on 64-bit builds).

Like full-fledged container, tuples may participate in cycles.

Examples:

```s2
const t = new s2.Tuple(1); // size of the tuple (may be 0)
assert 1 === t.length();
t.0 = 'hi';
// Or, more simply, via a tuple literal:
// var t = [# 'hi'];
// Mnemonic: a tuple is an array with a fixed # of entries.

foreach(t=>v) print(v); // ⇒ hi
foreach(t=>i,v) print(i,v); // ⇒ 0 hi
const t2 = new s2.Tuple([1, 2]); // copies elements from an array
assert 2 === t2[1];
assert 2 === t2.length();
t2[0] = t; // they may of course be cyclic
t.0 = t2;
const t3 = new s2.Tuple(t2); // clones the given tuple
assert t3.length() === t2.length();
assert t2.1 === t3.1;
```

Due to an ambiguity between the tokenizer and evaluation engine,
accessing more than one level of nested tuple or array requires
using the `[N]` operator instead of the dot operator:

```bash
s2sh> t2.1.0 = t2; // thinks that t2.(1.0) is intended, so:
rc=105 (CWAL_RC_EXCEPTION)
EXCEPTION: exception@0x25adfb0[scope=#1@0x7fff6883fd50 ref#=0]
==> {
 "code": 304,
 "column": 9,
 "line": 1,
 "message": "Cannot set properties on non-container type 'tuple'.",
 "script": "shell input"
}

// Use the [...] operator instead of the dot to disambiguate:
s2sh> t2[1][0] = t2
result: tuple@0x25ade90[scope=#1@0x7fff6883fd50 ref#=2] ==>
tuple@0x25ade90
```

<a id="type-tuple-methods"></a>
# Tuple Methods

The Tuple prototype does *not* inherit the base Object prototype but
brings with it the following methods:

```s2-member
new Tuple(integer length)
```

Must be 0 or greater. There is only one length-0 Tuple, and it is a
built-in constant value.

```s2-member
new Tuple(array)
```
Copies the length and entries from the given array.

```s2-member
new Tuple(otherTuple)
```
Clones the given Tuple.

Tuples can also be instantiated as literals, similarly to
arrays:

```
// [# …elements]
// e.g.:
assert typeinfo(istuple [# 1,2,3]);
```

**Mnemonic:** a tuple is (basically) an array witha fixed "#" of
elements.

```s2-member
int length()
```

The *immutable* length of the tuple. The build-in short form
`theTuple.#` works as well, and is more efficient because it avoids
the overhead of a script-bound function call.

```s2-member
int compare(Value [, Value])
```

Either compares itself to its single argument or compares its two
arguments. Unlike the overloaded comparison ops, this function does
not trigger an error if the virtual RHS is not a tuple.

```s2-member
string join([string glue])
```

Works just like [Array.join()](type-array.md#type-array-methods).

```s2-member
string toJSONString([int indentation=0]
```
Works just like [Array.toJSONString()](type-array.md#type-array-methods).

```s2-member
string toString()
```

Works just like [Array.toString()](type-array.md#type-array-methods).

---

-   TODO(?):
    -   `array toArray()`\  
        This can be achieved using [@rray expansion](type-array.md#type-array-expansion):
        `[@myTuple]`
    -   `tuple slice([startPos=0[, howMany=0]])`

Tuples overload all of the basic comparison operators (&lt;, &gt;,
&lt;=, &gt;=, ==, !=) to compare RHS tuples in conventional ways. The
comparisons ops currently throw if the RHS is not a tuple. This should
possibly be relaxed for the sake of script-side `sort()`
implementations. Then again, no other high-level container types
currently implement such ops and therefore also cannot be used with
comparison ops in such routines.

For iteration over the entries, use `foreach()`. If truly need be, we'll
add an `eachIndex()` method which works like *Array.eachIndex()*.



# Footnotes

[^47]:  Reminder to self: cwal-internal changes in early 2016 would
    potentially make it possible to have non-containers as prototypes,
    but there's no pressing need for it (and wouldn't work for the
    built-in empty Tuple).
