# s2: string
#### ([&#x2b11;Table of Contents](./))
# string

See also: [String syntaxes](grammar.md#grammar-strings)

Jump to...

* [Introduction](#type-string)
* [String Methods](#type-string-methods)
* [Strings as Numbers](#type-string-as-numbers)
* [Limits](#type-string-limits)

<a id="type-string"></a>
# Strings

Strings are probably the single most-used data type in many scripts, in
terms of how many the user types and how many the interpreter implicitly
processes (non-keyword identifiers are internally strings). In s2
strings may contain any UTF-8 input and are immutable. (The
[Buffer](type-buffer.md) class offers what amounts to a mutable
string.) Strings have a size limit, as described in the [](#type-string-limits) section.

See [String syntaxes](grammar.md#grammar-strings) for the various ways to
construct strings.


<a id="type-string-methods"></a>
# String Methods

While strings are not objects and may not have custom properties, they
do have a prototype.



The shared methods may be extended by modifying this type's prototype.
For example:

```
"".prototype.firstChar = proc(asInteger=false){
 return this.charAt(0, asInteger);
};
assert "a" === "abc".firstChar();
```

The protype includes, by default, the following methods, listed in
alphabetical order:

```s2-member
string applyFormat(...)
```
Treats this string as if it is formatted using the
[Buffer.appendf()](type-buffer.md#type-buffer-appendf) rules and treats any
arguments as values to apply those rules to. i.e. it's functionally
equivalent to `aBuffer.reset().appendf(thisString,...).takeString()`.
Returns a new, formatted string.

```s2-member
integer byteAt(integer byteOffset)
```
Similar to `charAt()` but returns the integer value of the byte at the
given byte offset (not *character* offset), or `undefined` if out of
range. If `(str.byteAt(X)!==str.charAt(X,true))` then the returned
byte is part of a multibyte character or `X` is out of range for
`charAt()` (remember that the byte length of a string will never be
less, and may be more, than the character length (assuming valid
UTF-8)).

```s2-member
mixed charAt(integer charIndex [,asInteger=false])
```
Returns the character at the given character position (not *byte*
position), or undefined if out of range. If the boolean parameter is
false (the default) then the result is a length-1 string, else it is
the integer value (the Unicode code point). Note that this is an
O(N) operation, not O(1), due to the UTF-8 character-counting
required.

> Sidebar: As of 20171115, strings support indexed access just like arrays,
being functionally equivalent to `charAt()` but being more
efficient because it happens in the operator layer and doesn't
require a function call. e.g. `('abc'[2] === 'c')` and
`('abc'[1][0][0][0] === 'b')`. Now that strings
support indexed access, it might make sense to change the 2nd
parameter's default to true.

```s2-member
integer compare(Value [,Value])
```

Compares this value to another one using "memcmp semantics". If
passed two values it compares those instead.

```s2-member
string concat(Value...)
```
Returns a new string comprised of this string plus the stringified
form of all arguments. When concatenating more than one string, this
is *much* more efficient than chaining more than one + operation
because it avoids extra temporary strings which that operator
necessarily invokes.

```s2-member
mixed evalContents(… various …)
```
Works identically to [*Buffer.evalContents()*](type-buffer.md#buffer-evalcontents).

```s2-member
integer indexOf(string [,offset=0])
```

Returns the index of the first instance of the given string in this
string. If the 2nd parameter is provided it is the starting
*character* (not byte) offset to start the search at. A negative
offset means to start searching that many characters from the end of
the string, but it does not change the order of the search (*it does
not search backwards!*). Returns an unspecified negative value if no
match is found, if the given string is empty, or its length is
greater than this string's.

```s2-member
bool isAscii()
```
Returns true if this string contains only ASCII bytes (those with
values in the range 0 to 127, inclusive). While that really makes no
difference in scripts, some of the C-native string algorithms can
run much more quickly if they know that they don't have to parse
non-ASCII UTF8 characters.

```s2-member
integer length()
```
Alias for lengthUtf8().

```s2-member
integer lengthBytes()
```
Returns the length of the string in *bytes*.

```s2-member
integer lengthUtf8()
```
Returns the length of the string in UTF-8 characters.

```s2-member
string operator+()
```
Overloads the binary `+` operator when a string
is on the left-hand side of an addition operation. Returns a new
string compounded of this string plus the stringified form of its
right-hand argument. e.g. `"3"+7+"!" === "37!"`. Note, however, that
calling `concat()` is more efficient than chaining more than one `+`
operator because the `+` operator (when chained) evaluates to several
temporary strings along the way to its result, whereas `concat()` does
not (it operates on a memory buffer, ideally with a single allocation).

```s2-member
string replace(needle, replacement [, limit = 0])
```
Returns a new string, a copy of this string with instances of
*needle* replaced by *replacement*. If a limit is supplied, only the
first *limit* instances are replaced, else all instances are
replaced. If no changes are made, the original string is returned.

```s2-member
array split(string separator [,limit = 0])
```
Splits this string on the given separator string. Returns an array
of entries, containing the whole input string as a single entry if
no separators are found. Neighboring separators, or separators at
the beginning or ending, act as if they had an empty entry on their
side(s), and empty entries are populated with length-0
strings[^43]. If the second parameter is specified (and is greater
than 0) then it stops splitting after tokenizing that many elements.
e.g. splitting `"a:b:c"` with a delimiter of `":"` and limit of 2 will
result in `["a","b"]`. (Side-note: those semantics changed to match
JavaScript's behaviour on 20160213[^44].)

Special case: If the separator is an empty string, the input string is
split into individual characters (up to the given limit, if any).

```s2-member
string substr([offset=0 [,length=-1]])
```
Returns a copy of a substring of the current string, starting at the
given *character* (not byte) offset (negative values count from the
end of the string). If the length is negative (the default) then the
range from the offset to the end is returned. If the offset is
larger than the string's length an empty string is returned. If a
negative offset has an absolute value greater than the string's
length then it is treated as 0.

Achtung: prior to 20171115, a length of 0 meant to copy until the
end of the string. That led to some quirky corner cases, so the
semantics were changed.

```s2-member
string toJSONString()
```

Returns a JSON-escaped form of this string, including surrounding
double quotes.

```s2-member
string toLower()
```
Returns the lower-cased form of this string. Supports *all* of the
one-to-one case conversions specified by Unicode, and *none* of the
n-to-one/one-to-n special cases. If there is no valid conversion for
a character, it is kept as-is.

```s2-member
string toUpper()
```

Returns the upper-cased form of the string. See toLower() for
Unicode details.

```s2-member
string trim()
```

Returns a copy of this string trimmed of leading and trailing
whitespace. Note that only ASCII whitespace is considered, not
exotic UTF-8 spaces.

```s2-member
string trimLeft/trimRight()
```
Returns a copy of this string trimmed of leading resp. trailing
whitespace.

```s2-member
string unescape()
```
Returns a copy of this string with conventional C-style
backslash-escape sequences unescaped. Also unescapes `\uXXXX` and
`\UXXXXXXXX` Unicode sequences. Generally only of use with heredocs
or strings read from files or user input, since quoted strings in
scripts do this automatically (using the same algorithm) when
evaluated. Unknown backslash sequences are left intact (as this
simplifies(?) escaping data for certain types of script-bound C APIs
(might want to rethink that)).

TODO: list all the sequences it supports.

<a id="type-string-as-numbers"></a>
# Strings as Numbers

The API has limited built-in support for converting decimal-format
strings into numbers. Any math operation with a number on the LHS and
a string on the RHS will automatically try to convert the string to a
number, resulting in a value of 0 if the value is-not-a number. Unary
plus and minus can also coerce a string into a number. Examples:

```
assert "3.00.1" === "3.0"+0.1; // string on the left
assert 3.7 === 0.7 + "3"; // string on the right
assert 3.74 === 0.7 + "3" + "0.04";
assert 3.1 === 3.1 + "abc"; // "abc"==0
assert 5 === +"5";
assert -5 === -"5";
assert -5 === -"+5"; // + sign inside the string is supported
assert -5 === +"-5"; // as is a - sign.
assert 1.2 === -"-1.2"; // but beware of floating-point precision changes on such conversions!
```

Alternately, the `parseInt()`, `parseDouble()`, and `parseNumber()`
members of the [numeric prototypes](type-number.md) provide more complete numeric
conversions (including hex and octal notations) and report invalid
conversions by returning `undefined`:

```
const pn = 0.parseNumber, pInt = 0.parseInt;
assert 1.0 === pn('1.0');
assert 1 === pInt(1.0);
assert 1.0 === pn(1.0);
assert -1 === pn('-1');
assert 'double' === typeinfo(name pn("1.3")); // parseNumber() keeps the numeric type
assert 'integer' === typeinfo(name pn("1"));
assert 'integer' === typeinfo(name pInt("1.3")); // parseInt() truncates to an integer
assert undefined === pn("not a number");
assert undefined === pn("1-1");
assert undefined === pInt("1.blah");
```

If you are not concerned about whether you get an integer or double
result, use `parseNumber()`, as it may return either one, whereas
`parseInt()` and `parseDouble()` always coerce their result (if not
`undefined`) to integer and double, respectfully.


<a id="type-string-limits"></a>
# Length Limits

The maximum *byte* length of any given string is
`2^(CWAL_SIZE_T_BITS-3)`, e.g. 30 bits (1GB) in a 32-bit
build[^45], and function bodies count as strings for this purpose
(so no billion-byte Functions, okay?). The remaining bits are used for
internal state flags. (Design note: it was either that or add a flag
field to strings, which would have increased their size by 4-8 bytes
once the compiler padded the structure.) In a 16-bit build the maximum
string length is only 8kb, but for most of s2's envisioned uses even
that "should be" sufficient. On 64-bit, that limit is functionally
unreachable. While Buffers have a limit of just under
`2^CWAL_SIZE_T_BITS` bytes, such large buffers cannot be converted to
strings. The `cwal_build_info()` C function (and its [s2sh](s2sh.md)
counterpart, `s2.cwalBuildInfo()`) can be used to find out the maximum
length of a string at runtime.

Note that the [Buffer class](type-buffer.md) does not have this limitation
- it may use (essentially) the whole bit range (64kb in a 16-bit
build, 4GB in a 32-bit build, and gazillions of bytes in 64-bit).


# Footnotes

[^43]:  These semantics changed on 2019-12-08 to match JavaScript and
    reduce insanity in client-side code. Prior to that, empty slots used
    to be filled the *undefined* value instead of empty strings.

[^44]:  20191110 LOL. i was just expecting the old semantics in some
    client code and came to look at the docs, expecting to see the old
    semantics described. Thought i had discovered a bug.

[^45]:  This is set independently of the architecture's bitness. A
    64-bit machine can build a 16-bit libcwal and a 32-bit machine can
    build a 64-bit-capable libcwal, provided the underlying platform is
    capable of it.
