# whcl: Arrays
##### ([&#x2b11;Table of Contents](./)) ([data types](type-intro.md))
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

See also: [the `array` builtin command](builtins.md#bic-array)

Jump to...

* [Introduction](#type-array)
* [Array Methods](#type-array-methods)

<a id="type-array"></a>
Arrays and Tuples (Lists)
============================================================

Arrays are the framework's general-purpose list-style container.  They
derive from [the Object class](type-object.md), so may hold arbitrary
key-value pairs and inherits its methods. What differentiates
them is that they can also hold an ordered list of values.

Tuples are (in this framework) a close cousin of arrays. Tuples are
fixed-length arrays which do _not_ derive from the core object class,
making them considerably memory-lighter than arrays (as of this
writing `sizeof(cwal_array)` is 72 and `sizeof(cwal_tuple)` is a mere
16).  This also means, however, that they cannot have per-instance
properties and their prototype may not be swapped out from script
code - all instances of non-object types share an identical
prototype. Tuples support only a subset of array options and their
lengths cannot be modified after construction. Tuples are otherwise
treated like arrays for many API purposes, including [`foreach`
loops](flow-control.md#foreach) and [the `set`
command](builtins.md#bic-set). Tuples are rarely used directly from
script code but are sometimes exposed by C-level bindings as a memory
use optimization.


<a id="type-array-methods"></a>
Array and Tuple Methods
============================================================

Arrays and Tuples are sometimes interchangeable, but tuples support
only a small subset of list operations:

- `compare`
- `join`
- `length` (read-only)
- `to-json`
- `to-string`
- `filter`

Construction
------------------------------------------------------------

Arrays are created using the [`array` builtin
command](builtins.md#bic-array). Tuples are created with
a constructor function:

```
decl t new whcl.Tuple X
```

where `X` is a number of elements for the tuple _or_ an array or tuple
to shallowly clone the contents of. If passed a number, it must be
equal to or greater than 0 but it is limited to 16 bits (64k).


<a id="method-clear"></a>
clear
------------------------------------------------------------

Usage: `clear-list [bool alsoClearProperties=false]`

Removes all entries from the array and optionally also its
object-level properties. Returns this object.


<a id="method-filter"></a>
filter
------------------------------------------------------------

Usage: `filter [-v] [-tuple] function`

Returns a new array consisting only of values from this array for
which the given function returns a truthy value. The function is
passed a single argument: the value in each slot in this array. If the
`-v` flag is used, the results of the filter are inverted: only items
for which the function returns a falsy value are included in the
results.

If `this` is a tuple or the `-tuple` flag is provided, it returns a
tuple instead of an array.

> Trivia: this method is implemented in whcl code.

<a id="method-get-index"></a>
get-index
------------------------------------------------------------

Usage: `get-index integer`

Functions identically to `$anArray[an-integer]`, returning
the value at the given index or `undefined` if no value is
at that index or the index is out of range.


<a id="method-index-of"></a>
index-of
------------------------------------------------------------

Usage: `index-of value [bool type-strict-compare=true]`

Searches the array for a value which compares equivalent to the given
value and returns its index, or an unspecified negative value if no
match is found.


<a id="method-is-empty"></a>
is-empty
------------------------------------------------------------

Usage: `is-empty`

Returns true if the array's `length` is 0.


<a id="method-join"></a>
join
------------------------------------------------------------

Usage: `join joinerString`

Concatenates all entries in the array into a string, separated by the
given string, and returns that string.


<a id="method-length"></a>
length
------------------------------------------------------------

Usage: `length [integer newLength]`

Either returns the array's current length (if passed no arguments) or
sets its length and returns the array object. If passed a length,
any elements beyond the new index will be lost to the array (but this does
not invalidate other references to them). If the new length is longer,
new slots have C-level `NULL` values, which is translated as `undefined`
in script code.


<a id="method-pop"></a>
pop
------------------------------------------------------------

Usage: `pop`

Removes the last element from the array, decreases its length by
1, and returns that value. If the array is empty it returns the
`undefined` value.


<a id="method-push"></a>
push
------------------------------------------------------------

Usage: `push value [...value]`

Appends one or more values to the end of the array, increasing its
length as necessary, and returns the final value passed to it.


<a id="method-reserve"></a>
reserve
------------------------------------------------------------

Usage: `reserve integer`

Ensures that the array has at least enough space for the given number
of entries and returns this array. It never shrinks the array, nor
changes its length, but may pre-allocate space for future array entries.


<a id="method-reverse"></a>
reverse
------------------------------------------------------------

Usage: `reverse`

Reverses the order of the array's entries and returns the array.


<a id="method-set-index"></a>
set-index
------------------------------------------------------------

Usage: `set-index integerIndex value`

Works like `set $anArray[an-integer] value`, including that it returns
the value passed ot it.


<a id="method-shift"></a>
shift
------------------------------------------------------------

Usage: `shift [integer count=1]`

Removes one or more elements from the start of the array, decreases
its length by that number, and returns the last value which was
removed. If the array is empty it returns the `undefined` value.


<a id="method-slice"></a>
slice
------------------------------------------------------------

Usage: `slice [offset=0 [count=-1]]`

Returns a new array containing the first `count` elements starting at
the given zero-based offset. If count is negative then it means "until
the end." Attempts to copy outside of the array's bounds will be
silently ignored and the resulting array will be truncated or empty. A
negative offset counts from the end of the array and if the negative
offset is larger than the array length then it is treated as 0.

Note that `slice 0 -1` (or the equivalent, passing no arguments) can
be used to *shallowly* clone an array without any problems vis-a-vis
cycles - the new array will hold references to the same values as the
initial array (and in the same order). i.e. modifying one array does
*not* modify the other, but changes made to (e.g.) an object in one of
the arrays will be visible via the other via that object reference.

*ACHTUNG:* note that the semantics of the 2nd argument to `slice`
differ from its JavaScript counterpart.


<a id="method-sort"></a>
sort
------------------------------------------------------------

Usage: `sort [a-comparison-function]`

Sorts this array using either the built-in comparison routines or the
given comparison function, which gets passed two arguments and must
return a value with "memcmp semantics": less than 0 if the first arg
is "less than" the second, 0 if they are equivalent, and greater than
0 if the first is "greater than" the second. Returns
`this`. Propagates any exceptions the sorting function throws.

Certain array operations are disallowed while sorting: resizing or
changing element values. Because *That Way Lies Madness*. Likewise, an
array which is currently being iterated over cannot be sorted, and
calling `sort` while iteration is running will trigger an exception.

> Sidebar: there's an ugly corner case involving [property
  aliases](variables.md#alias): an array entry may be an alias, and it
  will resolve for sorting purposes. Depending on where that alias
  points to, it's potentially possible that a misbehaved comparison
  function can swap out the value the alias resolves to in
  mid-sort. In such cases, sorting behavior is unspecified: it may
  sort in any order in that case. One potential FIXME in the
  underlying sorting is to resolve all proprefs up front into a temp
  list and sort on _that_. That would eliminate the mid-sort
  unspecified behavior but the end effect would be the same: after the
  sort, the alias would resolve to its replaced value, potentially
  leaving it out of sorted order.

<a id="method-unset-index"></a>
unset-index
------------------------------------------------------------

Usage: `unset-index integerIndex`

Removes one an entry from the array, shifts all right-side
elements one slot to the left, and reduces its length by 1.
Returns true if it removes an element, false if the given index
is out of range.


<a id="method-unshift"></a>
unshift
------------------------------------------------------------

Usage: `unshift value [...value]`

The counterpart of `shift`, this function prepends one or more values to
the start of an array, increasing its length as needed. They are added
such that their order in the array is the same as the order they are
passed to this function. Returns this array.
