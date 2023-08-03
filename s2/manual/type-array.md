# s2: arrays
#### ([&#x2b11;Table of Contents](./))
# The array Type

Jump to...

* [Arrays](#type-array)
* [Array Methods](#type-array-methods)
* [@rray Expansion](#type-array-expansion)

<a id="type-array"></a>
# Arrays

Arrays are generic heterogeneous lists of values.

Arrays are created using a "literal" syntax identical to JavaScript's:

```s2
const a = []; // empty array
const b = [1, 2, 3]; // array with 3 entries
```

Though an array's indexed entries can be accessed using
`anArray[index]` or `anArray.index` (where index is an integer value),
note that function calls made through such access do not bind the LHS to
the array as the `this` to the call. For example:

```s2
const ar = [0, proc f(){assert f===this; return this}, 2];
assert ar.1 === ar.1() /* 'this' is not the array */;
```

That applies only to integer properties (i.e. indexed array entries),
*not* to "normal" (object-level) properties, which bind the LHS as
`this` just like Objects do (because Arrays derive from Objects).


<a id="type-array-methods"></a>
# Array Methods

The base Array prototype inherits the Object prototype and brings the
following methods with it:

**Constructor:**

```s2-member
new [].prototype(…)
```

This exists for completeness, but is functionally identically to (and
less efficient than) an array literal. All arguments are added as
elements of the new array in their given order.

```s2-member
Array clear([bool clearPropertiesToo=false])
```

Clears the array list's contents and optionally (if passed a truthy
value) any properties set on this object (not those in prototypes).
It does not change the reserved number of list slots (i.e. does not
reallocate the list memory). Returns this array.

```s2-member
Array eachIndex( Function(value,index) )
```

For each index entry in the array, it calls the given function,
passing it the value and its index in the array (in that order).
Returns this array. If the
callback returns a literal `false` then looping stops without an
error.

Trivia: the argument order for the callback (value,index) is the
opposite of `Object.eachProperty()` (key,value) because in practice
the 2nd argument to the `eachIndex()` callback is normally ignored
(and often not specified in the callback's signature).

Note that `foreach(@anArray=>...)` is normally a much more efficient
approach to iteration, but `eachIndex()` predates `foreach()` by a
couple of years.

```s2-member
Array filter( Function(value) [, invert=false] )
```

Each element in this array is passed, one at a time, to the given
callback function and a new array is returned which contains only the
elements for which the callback returns a truthy value. If the 2nd
parameter is truthy then the filter is inverted, returning only
elements for which the callback returns a falsy value.

TODO: if passed a non-function value, simply compare each array entry
to that value, resulting in a list of equivalent values.


```s2-member
mixed getIndex(integer index)
```

Works identically to `this[index]`.

```s2-member
integer indexOf(Value [, bool typeStrictCompare=true])
```

Searches for a value in the array and returns the index of the first
match. It returns a negative value if no entry is found. If the 2nd
parameter is true (the default) then the search will only compare
values which have the same type (but note that Object/Array instances
never compare as equivalent in s2 except when comparing to themselves,
so it has no real effect on such types). If the 2nd parameter is
false, a non-strict comparison is performed. e.g. the string '1'
matches the integer 1 in non-strict mode, but not in strict mode.

```s2-member
bool isEmpty()
```

Returns true if the array's length is 0. This is more efficient than
comparing `0===array.length()` because it does not have to allocate an
integer value for the result.

TODO(?): rename this to not collide with the inherited Object method
of the same name, as that one has different semantics!

```s2-member
string join(string glue)
```

Joins each element of the array into a string, using the given "glue"
string between neighboring entries. An empty array returns an empty
string and a length-1 array will not apply the "glue" part.

```s2-member
integer length()
Array length(integer length)
```

The first (getter) form returns the array's effective length.  That's
normally equal to the highest index at which a value has been stored,
plus 1 (0 for an empty array), but the setter form of this method can
be used to set an arbitrary length. The array's current internal
capacity might be arbitrarily much higher than its current effective
length, but is never less.

Note that `anArray.#` is a shorthand form of `anArray.length()`.

The second (setter) form sets the array's length (distinct from its
*capacity*: see `reserve()`) and returns this array. It trims entries
if needed and (effectively) pads new empty slots with the `undefined`
value (it's actually a C-level NULL, but s2 translates that to
`undefined`).


```s2-member
Array operator+=(rhs)
```

Works like `push(rhs)` except that it returns this object.

```s2-member
Value push(Value...)
```

Pushes the given value(s) to the end of the array, in the order given,
and returns the last one added. For single values, the `[]=operator`
is more efficient/faster. e.g. `myArray[]=1` is functionally
equivalent to `myArray.push(1)`, but is more efficient.

```s2-member
mixed pop()
```

Removes the last item from the array and returns it, changing the
array's length. Returns `undefined` if the array is empty or if the
element had no value. Note that `undefined` is the value any
unassigned element will hold, so it is not a reliable indicator of
end-of-list.

```s2-member
bool removeIndex(integer index)
```

Removes the given array index, shifting all elements in higher indexes
one to the left and reducing its size by one. Returns `true` if the
index is in its range of slots and it removes the slot (whether or not
it contained a value), else `false` (if passed an index out of its
range).

```s2-member
Array reserve(integer howMany)
```

Reserves enough memory for the given number of entries and returns
this array. This is solely an allocation optimization. Reserving many
very large arrays might effectively "leak" them (or the upper, unused
parts of them) when they go through the list recycling. The memory is
*not* lost, but it might go unused when given to a smaller array via
recycling.

> Sidebar: arrays are implemented as simple C arrays of Values,
meaning that if a script array has a length of 100000, then it is
(regardless of how many elements it actually contains) taking up a
linearly-proportional amount of memory to do so (specifically,
`sizeof(void*)` times the number of value slots reserved). cwal
internally recycles list memory amongst arrays, tuples, buffers, and
hash tables, so in practice arrays are normally very cheap to
allocate, provided they don't get too large (and therefore won't
recycle as easily, except possibly with equivalently large buffers).

> Trivia: certain uses/patterns of `reserve()` bypass
internal memory-use optimizations and/or memory recycling and *might*
end up allocating more memory than if *not* using `reserve()`. Been
there, debugged that. That's ironic, however, considering that the
purpose of `reserve()` is to reduce memory allocation counts. Thus
`reserve()` is only recommended for relatively large values (e.g.
20+).


```s2-member
Array reverse()
```

Reverses the order of the entries in the array and returns this array.

```s2-member
mixed setIndex(integer index, value)
```

Works similarly to `this[index]=value`, but will coerce the index into
an integer (0 if it's not really an integer) instead of
using a non-integer key as a normal property like the `array[x]` operator
does.

```s2-member
mixed shift([integer howMany=1])
```

Removes the first `howMany` items from the array and returns the last
of those, changing the array's length (e.g. passing 2 reduces the
length by 2 and returns the (formerly) 2nd element). Returns
`undefined` if the array has fewer than `howMany` elements, but
`undefined` is also a legal array entry value (representing either an
unused slot or one which explicitly holds the `undefined` value).

```s2-member
Array slice([offset=0 [,count=0]])
```

Returns a new array containing the first count elements starting at
the given zero-based offset. If count is 0 then it means "until the
end." Attempts to copy outside of the array's bounds will be silently
ignored and the resulting array will be truncated or empty.  Note that
slice(0,0) (or the equivalent, passing no arguments) can be used to
*shallowly* clone an array without any problems vis-a-vis cycles - the
new array will hold references to the same values as the initial array
(and in the same order). i.e. modifying one array does *not* modify
the other, but changes made to (e.g.) an object in one of the arrays
will be visible via the other via that object reference. A more
efficient way to clone an array is [@rray
expansion](#type-array-expansion): `var clone = [@theArrayToClone]`.

*ACHTUNG:* note that the semantics of the 2nd argument to `slice()`
differ from its JavaScript counterpart.

*FIXME:* use `cwal_array_copy_range2()` instead of
`cwal_array_copy_range()` so that we have saniter offset
semantics. Fixing this is trivial easy, but finding script references
which rely on the current semantics is not!


```s2-member
Array sort([Function(lhs,rhs)])
```

Sorts this array using either the built-in comparison routines or the
given comparison function, which gets passed two arguments and must
return a value with "memcmp semantics": less than 0 if the first arg
is "less than" the second, 0 if they are equivalent, and greater than
0 if the first is "greater than" the second. Returns
`this`. Propagates any exceptions the sorting function throws.

Certain array operations are disallowed while sorting: resizing or
changing element values. Because *That Way Lies Madness*. Likewise, an
array which is currently being iterated over cannot be sorted, and
calling `sort()` while iteration is running will trigger an exception.

> Trivia: the exact sorting algorithm is technically unspecified, but is
currently `qsort()`.

```s2-member
string toString()
```

Returns a JSON-ized form of its contents. Note that this class also
inherits `Object.toJSONString()`.

```s2-member
Array unshift(value [, … value])
```

Prepends each argument to the array, in *reverse* order, such that
they are appear at the front of the array in the same order they are
provided to this function. Returns this array.

Trivia: this is not a terribly time-efficient way to splice lots of
values to the start of an array, as it re-shifts the array for each
element added. It only re-allocates the array (at most) a single time,
but it may re-shift the array elements a number of times equal to the
argument count.


<a id="type-array-expansion"></a>
# @rray Expansion

(Added 2016-02-27.)

The "@rray expansion" feature (as it's locally known) allows arrays and
tuples, in *very limited contexts*, to be expanded as if they were a list
of *individual expressions*. It provides at least one unique feature
(generically forwarding arguments to constructors) and a couple other
interesting features which aren't otherwise easy to achieve in s2
scripts.

This syntax simply prepends a `@` to an array/tuple expression and
causes the list's entries to be used as inputs to the evaluation engine
as if they had come from separate expressions, as demonstrated here via
an [s2sh](s2sh.md) session:

```bash
s2sh> print( 1,2,3 ) // A basis for comparison: 3 arguments
1 2 3
s2sh> print( [1,2,3] ) // basis for comparison: 1 arg (3-element array)
[1, 2, 3]
s2sh> print( @[1,2,3]) // Compare the inputs and outputs to those above
1 2 3
s2sh> print( [1,2,[3,4,5]] ) // Another plain array
[1, 2, [3, 4, 5]]
s2sh> print( [1,2,@[3,4,5]] ) // An expansion inside an array literal
[1, 2, 3, 4, 5]
s2sh> print( @[1,2,@[3,4,5]] ) // Inner/outer expansions (expanded left to right)
1 2 3 4 5
s2sh> print( 1,@[2,3,4],5 ) // In-between other arguments
1 2 3 4 5
s2sh> print( 1,@[],2 ) // An empty list behaves like *no* argument, not an *empty* arg
1 2
s2sh> print(@"abc".split('')) // works on all arrays, not just array literals
a b c
```

This feature is only available in these contexts:

- Function call arguments
- `new Constructor()` arguments
- In the body of an array/tuple literal (expanding an inner list into
  the containing list).

Everywhere else it will trigger a syntax error. Even in cases where it
is allowed, it's only allowed in a limited syntactical form: expansions
must appear "by themselves," not as an operand to an operator or keyword
(other than the comma operator which separates them from sibling
arguments). e.g. `print(1 + @[1])` will cause a syntax error.

This pseudo-operator, for precedence purposes, acts like a Value, not
an operator, and is interpreted as an integral part of its own RHS
operand. It evaluates as if the `@` is not there, and then (after the
RHS completes), remembers that it had an `@` and treats its RHS
appropriately (as opposed to passing it up the eval chain as a single
Array/Tuple value).

> Sidebar: `@` is not a true operator, as s2's stack machine isn't
> geared for operators to feed arbitrarily many values back via the
> stack machine. While it does have a mechanism for doing so (because
> i thought, at the start, that it would be needed for function
> arguments), it's been unused since s2's earliest days and there's no
> driving need to change that. The `@` construct's ability to make an
> empty list appear as if nothing at all was there (as opposed to an
> empty expression), including the comma before or after an empty
> list, is unique in the evaluation engine.


One completely new feature this enables, which currently requires
creating a proxy closure to accomplish, is generically proxying
arguments to constructors. e.g. a function's arguments can be passed
as-is to a constructor with `new MyType(@argv)`. Normally that's not
syntactically possible because constructors cannot be used \[*as*
`new` constructors\] with
[Function.apply()](type-function.md#type-function-apply-call) like
most functions can (that's a potential TODO, though). Expansions can
also be used to concatenate multiple arrays:

```bash
s2sh> print([@[1,2],@[3,4],5,6])
[1, 2, 3, 4, 5, 6]
```

Nested `@` constructs are, in effect, expanded from left-to-right.

> Sidebar: @array expansion is essentially the same thing as a
> list-type-only version of [JS's more generic "spread"
> syntax](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Spread_syntax),
> but that is coincidental. This feature was implemented a full 4
> years before i learned about JS spread syntax.
