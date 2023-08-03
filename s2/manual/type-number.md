# s2: Numbers
#### ([&#x2b11;Table of Contents](./))
# Numeric Types (Integer and Double)

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

s2 (or, rather, cwal) supports *signed* integer values and
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
subsections. Note that it is legal to call a method on a numeric literal
- no reference via a variable or property is required:

```s2
1.1.ceil(); // ⇒ 2
1.3.floor(); // ⇒ 1
42.toDouble(); // ⇒ 42.0
0.prototype.twice = proc(){ return this * 2 };
2.twice(); // ⇒ 4
```


<a id="number-methods"></a>
## Shared Numeric-type Methods

The following methods are shared by both the integer and double types...

```s2-member
integer compare(Value[, Value])
```
Compares this value to the first argument using "memcmp semantics."
If passed two values it compares those instead.

```s2-member
integer nthPrime(integer between 1 and 1000 (inclusive))
```
Returns the Nth prime number out of the first 1000 primes. Throws if
its argument is out of range. This is primarily intended to assist
in getting numbers to pass to the *Hash* constructor.

```s2-member
mixed parseDouble(value)
```
If value is a numeric value or (special case) boolean, it is
converted to a double and returned. If it is a string which strictly
looks like a complete number (in decimal, octal, binary, or hex format), it
is parsed as such and its double value is returned. Returns
`undefined` if it does not think the value represents a double or an
integer (which it promotes to a double). Note that this will parse
hex or octal integer values, but will coerce the result to a double.

```s2-member
mixed parseInt(value)
```
If value is a numeric value or boolean, it is converted to an
integer and returned. If it is a string which strictly looks like a
complete number (in decimal, octal, or hex format), it is parsed as
such and its integer value is returned. If passed a double, it
returns that value truncated to an integer. Returns `undefined` if
it does not think the value represents an integer.

```s2-member
mixed parseNumber(value)
```
Works like a combination of `parseInt()` and `parseDouble()`. It
returns either an integer or double value on success and `undefined`
for non-numeric values. Boolean true/false are treated as the
integers 1 resp. 0.

```s2-member
double toDouble()
```
Returns the double value of this number. This is a no-op for doubles
and coerces integers into their double counterpart. Note that 64-bit
integers have a larger integral range than normal doubles (but not
*long doubles*), so this may lead to incorrect results with large
integer values (more than 48 bits).

```s2-member
integer toInt()
```
Returns the integer form of this number. This is a no-op for
integers. Doubles get *truncated*, not rounded (because `round(3)` is
C99 and s2 is otherwise C89 compliant).

```s2-member
string toJSONString()
```
This is equivalent to the `toString()` method, but is provided for
symmetry with other JSON-capable classes.

```s2-member
string toString([string format=undefined])
```
Returns the number in string form (potentially truncated, for
doubles). e.g. `"3" === 3.toString()`. If passed a string, the
string is expected to be the "trailing" part of a formatting flag
compatible with [Buffer.appendf()](type-buffer.md#type-buffer-appendf), namely the
part after ``$1%`. e.g. `12.toString("04x")==="000c"` and
`1.2.toString("2.2f")==="1.20"`[^46]. Throws if passed an argument
which is either not a string or which causes
`cwal_buffer_format()` to complain (return non-0, in which case
the exception message normally explains exactly which part of the
formatting string is broken).

Reminder: `Buffer.appendf()`'s `%s` format only supports types which
have a "native" string representation (namely strings and buffers),
so numbers do not work with the `%s` formatting flag. One can,
however, use the `%d` (integer) formatting flag to coerce a double or
boolean to integer form, e.g. `2.3.toString('d')==="2"`, which is
functionally equivalent to, but more efficient than,
`2.3.toInt().toString()`.

<a id="customizing-numeric-prototypes"></a>
## Customizing Numeric Prototypes

While numbers are not containers and may not have properties, they do
have a prototype which can be customized (in fact, the integer and
double prototype inherits a common one for operations common to both
types):

```s2
var dbl = 0.0;
dbl.prototype.twice = proc(){ return this * 2 };
assert 6.2 === 3.1.twice();
assert 8.0 === (2.0*2).twice();
```

Achtung: high-precision truncation/rounding errors are always a
possibility with such comparisons because the input script is a string,
so "8.0" might end up with a different literal double value than
`4.0*2.0`. For more information than one can possibly want to know
regarding the reasons for this, feed your favourite search engine the
phrase "why does 1.0 not equal 1.0".

Achtung again: Double and Integer each have their own prototypes which
derive from a common prototype, so to add functionality to both integers
and doubles, clients need to add it to `0.prototype.prototype`.
In other words, `0.prototype.prototype === 0.0.prototype.prototype`!

Note that all numeric values are immutable and cannot be changed by,
e.g. re-assigning to `this` in a custom method. (`this` is just a
scope-level variable which disappears when the function returns.)

Installing [operator overloads](operators.md#overloading) for
the core operators will (by design!) not work for numeric types - the
engine always uses the built-in operators for them, ignoring any
client-installed ones, simply for sanity's sake. s2 does, however,
allow "overloadable-only" operators to be used on these prototypes, as
those have no default semantics which it could otherwise apply.


<a id="double-methods"></a>
# Double Methods

```s2-member
integer ceil()
```
Returns the smallest integral value not less than itself. i.e. it
works like C's `ceil(3)`, within some "reasonable" level of precision.

```s2-member
double floor()
```
Returns the largest integral value not greater than itself. i.e. it
works like C's `floor(3)`.

Potential TODO: add some numeric constants as properties, like PI (but
we could use the UTF-8 Pi symbol, if i could ever find it in this
drop-down menu).

<a id="integer-methods"></a>
# Integer Methods and Properties

```s2-member
int rand()
```
Returns the same as the C-standard `rand(3)`: the next pseudo-random
number from the system's own RNG, possibly trimmed to fit within
cwal's compile-time numeric range (if that's smaller than the
platform's *int* type). Note that the RNG is not automatically
seeded by the script engine: call `srand()` to seed it. If it is not
seeded, it *may* (depending on its runtime environment) return the
same sequence of numbers across all executions of a script.

*THAT SAID*: the exact RNG used by this function is unspecified. It
may, in the future, be modified to use a different random number
source.

```s2-member
void srand(int randomSeed)
```
Sets the random seed for future calls to the `rand()` method. This
affects all C-level APIs which use `rand(3)`. FWIW, the conventional
approach to seed the RNG, if security is not an issue, is to pass it
the current time, e.g. `0.srand(s2.time.time())`, but keep in mind that
a script which is run repeatedly within the same clock second would
seed the RNG with the same value each time. Throws if not passed
exactly 1 argument.

```s2-member
mixed toChar()
```
Returns the UTF-8 character for this integer as a length-1 string,
`undefined` if it is not a UTF-8 character, or an empty string if it
is 0 (the NUL byte).

> Design notes: `toChar()` should arguably return a length-1 string
(`"\0"`) for the NUL-byte case, but that would likely just lead to
confusion. While the vast majority of cwal- and s2-level APIs use
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



# Footnotes

[^46]:  Reminder to self: why does `%f` not support a 0 prefix padding
like `%d` does?
