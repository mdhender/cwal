# s2: Script Snippets
#### ([&#x2b11;Table of Contents](./))
# Script Snippets

Jump to...

* [Listing Object Properties](#snippet-list-properties)
  * [This Time Recursively](#snippet-list-properties-recursive)

<a id="snippet-list-properties"></a>
# Listing Object Properties

(TODO: re-do this example to use `foreach` instead of `eachProperty()`,
as that's far more efficient (but didn't exist when this section was
written).)

It is common that, while in s2sh, i quickly want to get a list of
inherited methods for a value. Here's one solution:

```s2
s2sh> var x = {a: 1, b: 2.2, c: true}
s2sh> x.eachProperty(proc(k,v){print(typeinfo(name v),k)})[^68]
bool c
double b
integer a

s2sh> x.prototype.eachProperty(proc(k,v){print(typeinfo(name v),k)})[^69]
function withThis
function unset
function toString
function toJSONString
function sortProperties
function set
function propertyCount
function propertyKeys
function isEmpty
function mayIterate
function hasOwnProperty
function get
function eachProperty
function compare
function copyPropertiesTo
function clearProperties
```

If you simply want the names, not the types, they can be fetched via
`value.prototype.propertyKeys()`.

However, there are cases where that doesn't work: the number and string
types have prototypes which are Objects but do not derive from the base
Object prototype (because most of its methods cannot work on
non-container types). e.g. they have no `eachProperty()` method. Such
"non-object Objects" can still be traversed via one level of
indirection:

```s2
s2sh> {}.eachProperty.call(0.prototype, proc(k,v){print(typeinfo(name v),k)})
function rand
function toUtf8Char
function toChar
integer INT_MAX
integer INT_MIN
s2sh> {}.eachProperty.call(0.prototype.prototype,
proc(k,v){print(typeinfo(name v),k)})
function toString
function toJSONString
function toInt
function toDouble
function parseNumber
function parseInt
function parseDouble
function compare
```

What we did there was get access to `Object.eachProperty` via a
temporary object (any object would have sufficed), then `call()` that
method with a first argument of the object we want to traverse (the
`this` for the `eachProperty()` call). Integers have a prototype and
doubles have a different one, but both of those prototypes derive from
yet another prototype, which is demonstrated in the 2nd call shown
above, using `0.prototype.prototype`.


<a id="snippet-list-properties-recursive"></a>
## Listing Properties Recursively through the Prototype Chain

Search [/s2/s2sh.s2](/finfo/s2/s2sh.s2) for the text "vtree" to see a
complete implementation.


# Footnotes

[^68]:  The (newer) foreach loop would be more efficient.

[^69]:  Same here.
