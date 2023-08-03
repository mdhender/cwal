# whcl: Object
##### ([&#x2b11;Table of Contents](./)) ([data types](type-intro.md))
# Objects (a.k.a. Dictionaries, Hash tables, and Associative Arrays)
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

See also: [the `object` builtin command](builtins.md#bic-object)

Jump to...

* [Introduction](#type-object)
* [Object Methods](#type-object-methods)

<a id="type-object"></a>
Objects
============================================================

Object's are the framework's general-purpose container type, capable
of holding arbitrary key-value pairs in _unordered_ storage with
amortized O(1) lookup speed. The Object class is the base class
of almost every other class in the framework.



<a id="type-object-methods"></a>
Object Methods
============================================================

The methods provided by this class are inherited by most
other classes in the library. They are listed in alphabetical
order below.

<a id='method-clear-properties'></a>
clear-properties
------------------------------------------------------------

Usage: `clear-properties`

Clears all properties from _this_ object (not including prototypes)
and returns this object.


<a id='method-copy-properties-to'></a>
copy-properties-to
------------------------------------------------------------

Usage: `copy-properties-to target [...targetN]`

Copies all properties from this object to one or more target
containers and returns the last target argument.


<a id='method-get'></a>
get
------------------------------------------------------------

Usage: `get key`

Works just like `$obj[key]`, returning the value for the given key or
the `undefined` value. If passed an integer key and "this" is an
array, it operates on an array index instead of a property key.


<a id='method-has-own-propery'></a>
has-own-propery
------------------------------------------------------------

Usage: `has-own-propery key`

Returns true if this object has its own copy of the given property,
not considering any copies which a prototype might have.


<a id='method-is-empty'></a>
is-empty
------------------------------------------------------------

Usage: `is-empty`

Returns true if this object has no properties.


<a id='method-property-keys'></a>
property-keys
------------------------------------------------------------

Usage: `property-keys`

Returns an array of all property keys, or an empty array if
the object has no properties.


<a id='method-set'></a>
set
------------------------------------------------------------

Usage: `set`

Works just like `set $obj[key] value`.


<a id='method-property-count'></a>
property-count
------------------------------------------------------------

Usage: `property-count`

Returns the number of properties in this object.

<a id='method-to-json'></a>
to-json
------------------------------------------------------------

Usage: `to-json [integer indentAmount [bool cyclesAsStrings=false]]`

Returns a JSON-format string representation of this object. The first
argument may be an integer or string. A positive integer is a number
of spaces to indent per level, negative is a number of hard tabs.

With throw if the object contains any cycles unless passed a truthy
second argument, in which case cycles are rendered in some unspecified
debug-like form.

Non-JSON-able keys and values are elided.


<a id='method-to-string'></a>
to-string
------------------------------------------------------------

Usage: `to-string`

Works just like `to-json` but does not accept any formatting options.


<a id='method-unset'></a>
unset
------------------------------------------------------------

Usage: `unset key [...keyN]`

Works just like `unset $obj[key]` but returns this object.


<a id='method-with-this'></a>
with-this
------------------------------------------------------------

Usage: `with-this aFunction`

Calls the given function with this object as its `this`. Returns the
result of that function unless the function returns `undefined` or
does not explicitly return, in which case it returns `this`.




