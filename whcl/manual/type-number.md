# whcl: Numbers
##### ([&#x2b11;Table of Contents](./)) ([data types](type-intro.md))
# Numeric Types (Integer and Double)
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

See also: [type introduction](type-intro.md#type-intro) for supported numeric
literal syntax.

Jump to...

* [Numbers](#numbers)
    * [Shared Number-type Methods](#number-methods)
    * [Customizing Numeric Prototypes](#customizing-numeric-prototypes)
* [Double Methods](#double-methods)
* [Integer Methods & Properties](#integer-methods)

<a id="numbers"></a>
# Numbers (Integer and Double)

whcl (or, rather, cwal) supports *signed* integer values and
double-precision floating point values, both with build-time-specified
limits. By default the C-standard double type is used for doubles and
either 32- or 64-bit integers are used for integers, depending on the
platform. In C they are typedef'd as `cwal_double_t` resp.
`cwal_int_t`. It is possible (and usual, but not required) to build
with 64-bit integer support on 32-bit platforms.

*ACHTUNG*: for floating-point values, cwal does not guaranty numeric
precision beyond what the system's local standard C library does. Do
not expect to be able to use it with 20-digit precision. It also does
no locale-specific conversions, so floating point numbers always (when
using any built-in conversions) use a dot (not a comma) as the
fractional separator.

While numbers are not objects and may not have custom properties, they
have methods inherited from their prototype(s), listed in the following
subsections.


<a id="number-methods"></a>
## Shared Numeric-type Methods

The following methods are shared by both the integer and double types...

<a id="method-compare"></a>
### compare

Usage: `compare otherValue [otherOtherValue]`

Compares this value to the first argument using "memcmp semantics."
If passed two values it compares those instead.

<a id="method-parse-double"></a>
### parse-double

Usage: `parse-double value`

If value is a numeric value or (special case) boolean, it is converted
to a double and returned. If it is a string which strictly looks like
a complete number (in decimal, octal, binary, or hex format), it is
parsed as such and its double value is returned. Returns `undefined`
if it does not think the value represents a double or an integer
(which it promotes to a double). Note that this will parse hex or
octal integer values, but will coerce the result to a double.

<a id="method-parse-int"></a>
### parse-int

Usage: `parse-int value`

If value is a numeric value or boolean, it is converted to an
integer and returned. If it is a string which strictly looks like a
complete number (in decimal, octal, or hex format), it is parsed as
such and its integer value is returned. If passed a double, it
returns that value truncated to an integer. Returns `undefined` if
it does not think the value represents an integer.

<a id="method-parse-number"></a>
### parse-number

Usage: `parse-number value`

Works like a combination of `parse-int` and `parse-double`. It returns
either an integer or double value on success and `undefined` for
non-numeric values. Boolean true/false are treated as the integers 1
resp. 0.

<a id="method-to-double"></a>
### to-double

Usage: `to-double`

Returns the double value of this number. This is a no-op for doubles
and coerces integers into their double counterpart. Note that 64-bit
integers have a larger integral range than normal doubles (but not
*long doubles*), so this may lead to incorrect results with large
integer values (more than 48 bits).


<a id="method-to-int"></a>
### to-int

Usage: `to-int`

Returns the integer form of this number. This is a no-op for
integers. Doubles get *truncated*, not rounded.

<a id="method-to-json"></a>
### to-json

Usage: `to-json`

This is equivalent to the `to-string` method, but is provided for
symmetry with other JSON-capable classes.


<a id="method-to-string"></a>
### to-string

Usage: `to-string [string format=undefined]`

Returns the number in string form. If passed a string, the string is
expected to be the "trailing" part of a formatting flag compatible
with [buffer appendf](type-buffer.md#type-buffer-appendf), namely the
part after ``$1%`. e.g. 12 `to-string "04x"` is `"000c"`.  Throws if
passed an argument which is either not a string or which causes
`cwal_buffer_format()` to complain (return non-0, in which case the
exception message normally explains exactly which part of the
formatting string is broken).

Reminder: `buffer appendf`'s `%s` format only supports types which
have a "native" string representation (namely strings and buffers),
so numbers do not work with the `%s` formatting flag. One can,
however, use the `%d` (integer) formatting flag to coerce a double or
boolean to integer form, e.g. 2.3 `to-string 'd'` is `"2"`.

<a id="customizing-numeric-prototypes"></a>
## Customizing Numeric Prototypes

While numbers are not containers and may not have properties, they do
have a prototype which can be customized (in fact, the integer and
double prototype inherits a common one for operations common to both
types):

```whcl
decl d 1
set $d[__prototype][twice] proc {} { return [expr $this * 2]}
assert 2 == [d twice]
incr d 2
assert 6 == [d twice]
```

(Yes, `$this` refers to the number in that function body.)

Achtung: high-precision truncation/rounding errors are always a
possibility with such comparisons because the input script is a string,
so "8.0" might end up with a different literal double value than
`4.0*2.0`. For more information than one can possibly want to know
regarding the reasons for this, feed your favourite search engine the
phrase "why does 1.0 not equal 1.0".

Achtung again: Double and Integer each have their own prototypes which
derive from a common prototype, so to add functionality to both integers
and doubles, clients need to add it to the prototype of the prototype:

```whcl
decl d 0
set $d[__prototype][__prototype] ... # common double/int prototype
```

Note that all numeric values are immutable and cannot be changed by,
e.g. re-assigning to `this` in a custom method. (`this` is just a
scope-level variable which disappears when the function returns.)


<a id="double-methods"></a>
# Double Methods

<a id="method-ceil"></a>
## ceil

Usage: `ceil`

Returns the smallest integral value not less than itself. i.e. it
works like C's `ceil(3)`, within some "reasonable" level of precision.

<a id="method-floor"></a>
## floor

Usage: `floor`

Returns the largest integral value not greater than itself. i.e. it
works like C's `floor(3)`.

<a id="integer-methods"></a>
# Integer Methods and Properties

<a id="method-rand"></a>
## rand

Usage: `rand`

Returns the same as the C-standard `rand(3)`: the next pseudo-random
number from the system's own RNG, possibly trimmed to fit within
cwal's compile-time numeric range (if that's smaller than the
platform's *int* type). Note that the RNG is not automatically
seeded by the script engine: call `srand` to seed it. If it is not
seeded, it *may* (depending on its runtime environment) return the
same sequence of numbers across all executions of a script.

*THAT SAID*: the exact RNG used by this function is unspecified. It
may, in the future, be modified to use a different random number
source.

<a id="method-srand int randomSeed"></a>
## srand

Usage: `srand int randomSeed`

Sets the random seed for future calls to the `rand` method. This
affects all C-level APIs which use `rand(3)`. FWIW, the conventional
approach to seed the RNG, if security is not an issue, is to pass it
the current time. Throws if not passed exactly 1 argument.

<a id="method-to-char"></a>
## to-char

Usage: `to-char`

Returns the UTF-8 character for this integer as a length-1 string,
`undefined` if it is not a UTF-8 character, or an empty string if it
is 0 (the NUL byte).

> Design notes: `to-char()` should arguably return a length-1 string
(`"\0"`) for the NUL-byte case, but that would likely just lead to
confusion. While the vast majority of cwal- and whcl-level APIs use
the "proper" length of a String value, many C APIs do not (they
depend on a NUL byte to mark the end of a string), and would
consider a string of `"\0"` to have a length of 0. Thus we return an
empty string (which has an implicit NUL byte, as cwal always
NUL-pads strings which it creates (X- and Z-strings may come from
client code, and *might not* be NUL-padded (but, in practice, always
are unless someone specifically aims to misbehave)).

**Prototype Properties**

- `INT_MIN` and `INT_MAX` each hold the library's minimum resp.
maximum integer values. Note that underflow and overflow results are
*unspecified* in C for *signed* integer types, so don't necessarily
expect numbers to "wrap around" in a predictable manner when doing
"large math." (It's just a mere scripting language, after all!) In
practice, they do indeed seem to wrap around nicely, but that's not
currently guaranteed (it depends on the API being used, and whether
that API takes care to internally use unsigned values to get
well-defined over/underflow behaviour).

