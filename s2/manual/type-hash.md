# s2: Hashtables
#### ([&#x2b11;Table of Contents](./))
# The Hash (a.k.a. Hashtable) Type

Jump to...

* [Hashes a.k.a. Hashtables](#type-hash)
* [Hash Methods](#type-hash-methods)

<a id="type-hash"></a>
# Hashes

Hashes, also known as hash tables, work similarly to Objects except
that they support both "normal" properties (as for containers) and
"fast" properties (a hash table). Normal properties are set and
fetched as for other objects, and accessing the hashtable properties
uses separate routines. Hashes, due to the nature of hash tables, take
up notably more memory than objects (approximately hash-size times
`sizeof(void*)` more), and should only be used when speed of access to
properties is of the utmost importance or when storing large mappings
of keys to values, as searching through large sets is much faster with
hashes than objects. Hashed property access is amortized
O(1) and does not traverse prototypes if an entry is not found.

> As of 2021-07-12, [the object class](./type-object.md) supports a
build-time operation which enables internal use of a hashtable for
property access. With that option enabled, normal property access is
actually a tick faster than hashtable entries (because it lacks the
script-side function-call overhead of hashtable entries) but the two
types of storage have one notable difference: lookups for object-level
properties will, when a property is not found in an object, be
searched for in the prototype(s) containers. Conversely, prototype
lookups are never performed when looking for plain hashtable entries.


To create a hash table:

```s2
var h = new s2.Hash(713);
assert h inherits s2.Hash;
assert 'hash' === typeinfo(name h);
assert typeinfo(ishash h);
```

Or use the "hash literal" syntax, which identical to object literals
except for the addition of the `#` character as the first character
of the literal's body:

```s2
{# // <== denotes a hash literal
  a: 1,
  b:= 'hi', // := sets the entry as const (cannot be re-set later)
  ...
}
```

The (optional) first parameter to the constructor defines the
(approximate) size of the hash table (s2 might round it to some
"nearby" prime value). The default value is relatively small but is
fine for simple uses. A value of 0, or larger than some internally-set
limit, causes some unspecified default to be used. A negative value
triggers an exception. A hash's table can be resized using its
`resize()` method. One simple way to get a prime is to call
`0.nthPrime(20)`, which returns the Nth prime (where N is its
argument), limited to the [first 1000 prime
numbers](https://en.wikipedia.org/wiki/List_of_prime_numbers#The_first_1000_prime_numbers)
(up to and including 7919).

Tip: the inherited `withThis()` method can be used to initialize a hash
table at its construction point:

```s2
const h = new s2.Hash(0.nthPrime(20)).withThis(proc(){
 this.insert('a', 'aaaa');
 this.insert('b', 'bbbb');
 ….
});
```

Alternately (and newer):

```s2
const h = new s2.Hash(0.nthPrime(20)) {
 this.insert('a', 'aaaa');
 this.insert('b', 'bbbb');
 ….
};
```

Hashed values (as opposed to "normal" properties) are added to the
object using the `insert()` method, searched for using `search()` or (much more efficiently!)
the `#` operator, and are removed using `remove()`. The full list of member
functions is in [the next subsection](#type-hash-methods).

> ***ACHTUNG***, ***ACHTUNG***, ***ACHTUNG***: As with object
properties, hashes can contain any Value type, both as keys and
values. ***However***, unlike object properties, *hashed keys are
type-strict*, meaning that the keys integer 1, string "1", and double
1.0 are *not* equivalent hash keys as they are for "normal"
properties. This is a side-effect of how hashing works (type-lax,
hashcode-based comparisons of keys cannot work (can it?)).  Similarly,
Tuples do not hash based on their contents (even though they compare
using their contents) because hashing requires stable values (and
tuples are mutable). Because of this, Tuples as keys will never hash
as equivalent to each other except via some blind twist of fate (an
internal hash collision with a different Tuple instance *and*
equivalent content). Summary: do *not* use Tuples as hash keys. The
engine does not currently disallow it, but it is not recommended and
may be disabled in the future.


<a id="type-hash-methods"></a>
# Hash Methods

Hashes add the following methods to their class hierarchy (in
alphabetical order):

```s2-member
Hash clearEntries([bool clearProperties=false])
```

Clears all entries from the hash table. If the optional parameter is
true then non-hash properties (those belonging to "the object part"
of the Hash) are also cleared (only those set in this object, not
properties inherited from the prototype). This does not change the
size of the hashtable. Returns this object.

```s2-member
bool containsEntry(Key)
```

Returns true if the hash contains the given key.

```s2-member
Hash eachEntry(Function)
```

Calls the given function one time for each key/value in the hash
table, passing it the (key,value). Note that the order of the keys
is non-deterministic. It is illegal to modify the hash table while
it is being iterated over, and doing so will lead to an exception.
If the callback returns a literal false value, then looping over the
callback stops immediately without an error. If the callback throws,
the exception is propagated. Returns this object.

Note that
[`foreach(#aHash)`](keyword-flow-control.md#keyword-foreach) is much
more efficient, but was added to the language long after this method.

```s2-member
Hash eachEntry(Hash, Function)
```

Works like `eachEntry(Function)`, but uses the first parameter as
the "this" for the call.

```s2-member
Hash eachEntry(Hash)
```

Copies all properties from this object to the given hash. Equivalent
to: `myHash.eachEntry(targetHash, targetHash.insert)`

```s2-member
integer entryCount()
```

Returns the number of entries in the hash table (an O(1) operation).
Note that this is a function, not a value property (because s2 lacks
so-called property interceptor functions)!

```s2-member
Array entryKeys()
```

Returns a new array of all keys in the hash table, in an indeterminate
order. (Note that hashes inherit the `Object.propertyKeys()` method as
well.)

```s2-member
Array entryValues()
```

Returns a new array of all values in the hash table. Their ordering
is the same as their corresponding keys (but *that* ordering is
indeterminate). It is guaranteed that the arrays returned from this
function and `entryKeys()` will have their entries aligned such that a
given index in the key list properly goes with the value at that
same index in the values list, provided the hash table is not
modified between the calls to `entryKeys()` and `entryValues()`.

```s2-member
Hash growIfLoaded([double loadFactor=approx. 0.8])
```

If this hash's load factor is over the given value, it is resized to
have (roughly) the given load factor. Returns this hash. Unusually
small or large numbers might be trimmed to within "reasonable"
limits. This routine never shrinks the table.

```s2-member
bool hasEntries()
```

Returns `true` if this hash contains any hash table entries, else
returns `false`.
    
Design note: in order to avoid confusion wrt the inherited
`Object.isEmpty()`, we don't override it, but instead provide this
function, which has the opposite return semantics.

```s2-member
integer hashSize()
```

Returns the size of the hash table (not the number of entries).
Ideally (for search speed) this value is a prime number larger than
the number of entries. This value is set at creation time or later
on via the `resize()` method.

```s2-member
Value insert(Key, Value)
```

Inserts the given key/value pair, overwriting any existing entry
with an equivalent key. Returns the value it sets.

> Potential TODO: add a 3rd (boolean) param specifying whether to
overwrite or fail if an entry already exists.

```s2-member
new Hash(integer tableSize)
```

Evaluates to a new hashtable with the given table size (which really, really
should be a prime number). The constructor might
(and might not) adjusted the provided size to fit within some certain range and/or
make it a prime number. The `nthPrime()` method of the integer prototype can be
used to fetch any of the first 1000 prime numbers, e.g. `0.nthPrime(20)` fetches
the 20th prime.


```s2-member
bool remove(Key)
```

Removes the given key. Returns true if it found an entry, else
false.

```s2-member
Hash resize(integer newSizePleaseUseAPrimeNumber)
```

Resizes the internal lookup table, retaining all existing entries.
Its argument must be an integer value greater than 0 and it really, really
should be a prime number. The library reserves the right to cap the
size at some internal limit. Tip: the expression `0.nthPrime(N)`,
where `N` is an integer between 1 and 1000, will return the Nth prime
number.

```s2-member
Value search(Key)
```

Searches for the given key and returns its value, or `undefined` if no
entry was found (but note that `undefined` is also a legal value!).
The `#` ("hash") operator provides this same feature but without the
function call overhead (i.e. it's faster and needs less memory). e.g.
`hashInstance # key` is equivalent to, but slightly faster than,
`hashInstance.search(key)`.


```s2-member
Hash takeProperties(Container src [, integer overwritePolicy=1])
```

> **Warning:** this member is not implemented (will throw an exception) if
the library is compiled with the `CWAL_OBASE_ISA_HASH` option. The
underlying algorithm has to be reimplemented for that mode of
operation.

Transfers (*moves*) all properties from the given container, inserting
them as hash table entries in such a way that does not require
allocating any new memory. The second parameter determines how
overwriting is handled: 0 means throw an exception if a key collision
occurs. Greater than 0 means to overwrite any entries in the hash with
the entry from the source container. Less than 0 means to not
overwrite entries, keeping any existing values (and treat it as
success). If the passed-in container contains any properties after
this method returns, it means those properties were not transferred
because the `overwritePolicy` did not allow for it.  Returns this
object.

If the 2nd argument is of type "unique" (i.e. is an enum entry) then
its wrapped value is used in its place. (Potential TODO: add enum
entries to this method which correspond to the 3 legal values of the
policy.)
