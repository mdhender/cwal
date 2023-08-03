# whcl: Strings
##### ([&#x2b11;Table of Contents](./)) ([data types](type-intro.md))
# Strings
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

Jump to...

* [Introduction](#type-string)
* [String Methods](#type-string-methods)
* [Strings as Numbers](#type-string-as-numbers)
* [Limits](#type-string-limits)

<a id="type-string"></a>
# Strings

Strings are probably the single most-used data type in many scripts, in
terms of how many the user types and how many the interpreter implicitly
processes (most identifiers are internally strings). In whcl
strings may contain any UTF-8 input and are immutable. (The
[Buffer](type-buffer.md) class offers what amounts to a mutable
string.) Strings have a size limit, as described in the
[limits](#type-string-limits) section.

See [String syntaxes](grammar.md#grammar-strings) for the various ways to
construct strings.


<a id="type-string-methods"></a>
# String Methods
========================================

While strings are not objects and may not have custom properties, they
do have a prototype, accessible as via `whcl[prototypes][String]`. The
protype can be used to invoke certain string functionality without a
string instance, namely the `concat` method:

```whcl
dec -const cc whcl[prototypes][String][concat]
decl x [cc "a=" $a ", b=" $b]
```

(In the meantime, `concat` is a builtin command which works the same
way.)

The prototype includes, by default, the following methods, listed in
alphabetical order...


<a id='method-apply-format'></a>
apply-format
------------------------------------------------------------

Usage: `string apply-format ...args`


Treats this string as if it is formatted using the
[Buffer appendf method](type-buffer.md#type-buffer-appendf) rules and
treats any arguments as values to apply those rules to. Returns a new,
formatted string.


<a id='method-byte-at'></a>
byte-at
------------------------------------------------------------

Usage: `integer byte-at integer byteIndex`

Similar to `char-at` but returns the integer value of the byte at the
given byte offset (not *character* offset), or `undefined` if out of
range.



<a id='method-char-at'></a>
char-at
------------------------------------------------------------

Usage: `mixed char-at integer charIndex [asInteger=false]`

Returns the character at the given character position (not *byte*
position), or undefined if out of range. If the boolean parameter is
false (the default) then the result is a length-1 string, else it is
the integer value (the Unicode code point). Note that this is an
O(N) operation, not O(1), due to the UTF-8 character-counting
required.

Note that strings support indexed access just like arrays,
being functionally equivalent to `char-at` but being more
efficient because it happens in the operator layer and doesn't
require a function call.



<a id='method-compare'></a>
compare
------------------------------------------------------------

Usage: `compare otherValue`


Compares this value to another one using "memcmp semantics". If
passed two values it compares those instead. Note that this does
a type-loose comparison and will compare a number-like string
as equivalent to a similar numeric value.


<a id='method-concat'></a>
concat
------------------------------------------------------------

Usage: `concat value...`

Returns a new string comprised of this string plus the stringified
form of each argument. It performs no extra formatting such as spaces
between the elements or a newline at the end.

This function may be called with a non-string as its `this`, in which
case it returns the concatenation of all of the arguments:

```whcl
decl -const concat $whcl[String][concat]
echo [concat "ABC" "DEF"]
```


<a id='method-eval-contents'></a>
eval-contents
------------------------------------------------------------

Usages:

- `eval-contents [pseudoFilename [varsObject]]`
- `eval-contents [varsObject [pseudoFilename]]`

Works identically to [`Buffer eval-contents`](type-buffer.md#method-eval-contents).

<a id='method-glob'></a>
glob-matches and matches-glob
------------------------------------------------------------

The functions `glob-matches` and `matches-glob` perform the same function
but have different argument semantics.

`glob-matches` has two distinct usages:

1. `glob-matches [-like|-ilike|-glob|-iglob] haystack`\  
   If its `this` is a string then it returns true if the string
   argument matches the glob pattern defined by `this`.
2. `glob-matches [-like|-ilike|-glob|-iglob] glob-pattern haystack`\  
   If its `this` is _not_ a string then it returns true if
   the second argument matches the glob pattern specified by
   the first argument.

`matches-glob` works identically except that the argument order for
the glob and `haystack` are swapped:

1. `matches-glob [-like|-ilike|-glob|-iglob] glob-pattern`\  
   If its `this` is a string then it returns true if `this` matches
   the glob pattern defined by the first argument.
2. `matches-glob [-like|-ilike|-glob|-iglob] hashstack glob-pattern`\  
   If its `this` is _not_ a string then it returns true if the string
   in the first argument matches the glob in the second argument.

One optional flag may be specified:

- `-glob`: use case-sensitive conventional Unix-style globs. This is
  the default if no flag is specified.
- `-iglob` as for `-glob`, but case-insensitive.
- `-like` use case-sensitive SQL LIKE-style matching.
- `-ilike` use case-insensitive SQL LIKE-style matching.

Matching is UTF-8 aware but case-sensitivity only applies to those
case for which case folding is well-defined in cwal. Namely, it
supports all characters which have 1-to-1, reversible case-folding
mappings. It specifically does not support any special-case
case-folding cases (haha).

In SQL LIKE mode, `%` matches any number of characters and `_` matches
a single character. For conventional wildcard mode:

- `*` Matches any sequence of zero or more characters.
- `?` Matches exactly one character.
- `[...]` Matches one character from the enclosed list of characters.
- `[^...]` Matches one character not in the enclosed list.


<a id='method-index-of'></a>
index-of
------------------------------------------------------------

Usage: `index-of string needle [charOffset=0]`


Returns the index of the first instance of the given string in this
string. If the 2nd parameter is provided it is the starting
*character* (not byte) offset to start the search at. A negative
offset means to start searching that many characters from the end of
the string, but it does not change the order of the search (*it does
not search backwards!*). Returns an unspecified negative value if no
match is found, if the given string is empty, or its length is
greater than this string's.



<a id='method-is-ascii'></a>
is-ascii
------------------------------------------------------------

Usage: `is-ascii `


Returns true if this string contains only ASCII bytes (those with
values in the range 0 to 127, inclusive). While that really makes no
difference in scripts, some of the C-native string algorithms can run
much more quickly if they know that they don't have to parse non-ASCII
UTF8 characters. This is an O(1) operation, determined at the time the
string is created.


<a id='method-length'></a>
length
------------------------------------------------------------

Usage: `length `


Returns the number of _characters_ in the string, which may be
lower than...

<a id='method-length-bytes'></a>
length-bytes
------------------------------------------------------------

Usage: `length-bytes `


Returns the length of the string in *bytes*.


<a id='method-replace'></a>
replace
------------------------------------------------------------

Usage: `replace needle replacement [limit = 0])`

Returns a new string, a copy of this string with instances of
*needle* replaced by *replacement*. If a limit is supplied, only the
first *limit* instances are replaced, else all instances are
replaced. If no changes are made, the original string is returned.


<a id='method-split'></a>
split
------------------------------------------------------------

Usage: `split separator [limit = 0])`

Splits this string on the given separator string. Returns an array
of entries, containing the whole input string as a single entry if
no separators are found. Neighboring separators, or separators at
the beginning or ending, act as if they had an empty entry on their
side(s), and empty entries are populated with length-0
strings. If the second parameter is specified (and is greater
than 0) then it stops splitting after tokenizing that many elements.
e.g. splitting `"a:b:c"` with a delimiter of `":"` and limit of 2 will
result in `["a","b"]`.

Special case: If the separator is an empty string, the input string is
split into individual characters (up to the given limit, if any).


<a id='method-substr'></a>
substr
------------------------------------------------------------

Usage: `substr offset [length=-1]`


Returns a copy of a substring of the current string, starting at the
given *character* (not byte) offset (negative values count from the
end of the string). If the length is negative (the default) then the
range from the offset to the end is returned. If the offset is
larger than the string's length an empty string is returned. If a
negative offset has an absolute value greater than the string's
length then it is treated as 0.


<a id='method-to-json'></a>
to-json
------------------------------------------------------------

Usage: `to-json `

Returns a JSON-escaped form of this string, including surrounding
double quotes.


<a id='method-to-lower'></a>
to-lower
------------------------------------------------------------

Usage: `to-lower `

Returns the lower-cased form of this string. Supports *all* of the
one-to-one case conversions specified by Unicode, and *none* of the
n-to-one/one-to-n special cases. If there is no valid conversion for
a character, it is kept as-is.



<a id='method-to-upper'></a>
to-upper
------------------------------------------------------------

Usage: `to-upper `


Returns the upper-cased form of the string. See `to-lower` for
Unicode details.



<a id='method-trim'></a>
trim
------------------------------------------------------------

Usage: `trim [-left] [-right]`


Returns a copy of this string trimmed of leading and trailing
whitespace. Note that only ASCII whitespace is considered, not
exotic UTF-8 spaces. The `-left` and/or `-right` flags may be
specified to restrict trimming to one side. The default is as if
both flags are passed in.


<a id="type-string-as-numbers"></a>
Strings as Numbers
============================================================

The API has limited built-in support for converting decimal-format
strings into numbers. Any math operation with a number on the LHS and
a string on the RHS will automatically try to convert the string to a
number, resulting in a value of 0 if the value is-not-a number. Unary
plus and minus can also coerce a string into a number.

```whcl
assert 3.7 == 0.7 + "3"; # string on the right
assert 3.74 == 0.7 + "3" + "0.04";
assert 3.1 == 3.1 + "abc"; # "abc"==0
assert 5 == +"5";
assert -5 == -"5";
assert -5 == -"+5"; # + sign inside the string is supported
assert -5 == +"-5"; # as is a - sign.
assert 1.2 == -"-1.2"; # but beware of floating-point precision changes on such conversions!
```

<!--
Alternately, the `parseInt()`, `parseDouble()`, and `parseNumber()`
members of the [numeric prototypes](type-number.md) provide more complete numeric
conversions (including hex and octal notations) and report invalid
conversions by returning `undefined`:

```whcl
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
-->


<a id="type-string-limits"></a>
# Length Limits
========================================

The maximum *byte* length of any given string is
`2^(CWAL_SIZE_T_BITS-3)`, e.g. 30 bits (1GB) in a 32-bit build[^45],
and function bodies count as strings for this purpose (so no
billion-byte Functions, okay?). The remaining bits are used for
internal state flags. (Design note: it was either that or add a flag
field to strings, which would have increased their size by 4-8 bytes
once the compiler padded the structure.) On 64-bit, that limit is
functionally unreachable. While Buffers have a limit of just under
`2^CWAL_SIZE_T_BITS` bytes, such large buffers cannot be converted to
strings. The `cwal_build_info()` C function can be used to find out
the maximum length of a string at runtime.


# Footnotes
========================================

[^45]:  This is set independently of the architecture's bitness. A
    64-bit machine can build a 16-bit libcwal and a 32-bit machine can
    build a 64-bit-capable libcwal, provided the underlying platform is
    capable of it.
