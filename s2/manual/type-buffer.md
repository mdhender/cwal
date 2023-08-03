# s2: Buffers
#### ([&#x2b11;Table of Contents](./))
# The Buffer Type

Jump to...

* [Buffers](#type-buffer)
* [Buffer Members](#type-buffer-members)
    * [Running code with `evalContents()`](#type-buffer-evalcontents)
* [Formatting with `appendf()`](#type-buffer-appendf)
* [Compression Support](#type-buffer-compression)

<a id="section-1"></a>
# Buffers (the Memory Slayer)

Buffers are general-purpose byte arrays, most often used for reading
file content and creating dynamic strings. This is more efficient than
using string concatenation[^41], and the [`appendf()`](#type-buffer-appendf)
method gives us more formatting flexibility.

Buffers are (as of 20141217) containers, so they may have custom
properties and act as a prototype for another value.

To create a buffer:

```s2
var b = new s2.Buffer();
assert b inherits s2.Buffer;
assert 'buffer' === typeinfo(name b);
assert typeinfo(isbuffer b);
```

The constructor optionally takes an integer argument - an initial amount
of memory to reserve.

Buffers are, at the cwal level, inherently "stringable," meaning they
can be used in most (not all) contexts which expect a string argument.
The majority of APIs which take a string will also accept a buffer,
treating its contents like a string and its "used" length
(`Buffer.length()`) as its string's length in bytes. That will,
however, have undefined results if one feeds invalid string data
(non-UTF-8) through such conversions.


<a id="type-buffer-members"></a>
# Buffer Members

The Buffer prototype inherits the base Object prototype and brings with
it the following methods:

```s2-member
Buffer append(values...)
```

Appends each value to the buffer in "some string form," except for
Buffer values, which are copied as-is in full. If a Buffer is appended
to itself, it doubles its size and appends its entire current contents
to itself. Returns this buffer object.

```s2-member
Buffer appendf(string format, values...)
```

Uses the C-level `cwal_buffer_format()` to append a formatted string
to the buffer. It works similarly to `java.lang.String.format()`, and
*not* like C's `printf()`. Returns this buffer object. Described in
more detail in [its own subsection](#type-buffer-appendf). See also:
[`String.applyFormat()`](type-string.md#type-string-applyformat),
which is a proxy for this function. TODO: ensure that this variant
behaves properly when appending a buffer to itself.

```s2-member
Buffer appendJSON(Value v [, int indent=0])
```

Appends the JSON form of the given value to this buffer. If the value
contains cycles, they will be output in some functionally useless but
slightly informative string form (TODO: reconsider that decision,
maybe preferring an exception). The indent argument may be a positive
value to add that many spaces of indentation per JSON level, or a
negative value to use that many hard tabs per level.  Returns this
buffer.

```s2-member
integer|Buffer byteAt(offset[, value])
```

Gets or sets the byte value at the given byte index. An assigned value
is masked against 0xFF. In setter mode it extends the "used" length if
the given offset is past the end, then returns this buffer
instance. In getter mode it returns an integer value between 0 and
255, or `undefined` if the offset is out of range.

```s2-member
integer|Buffer capacity([integer])
```

If passed no arguments, it returns this buffer's current capacity, in
bytes. Otherwise it reserves at least the given amount of memory in
the buffer (possibly more) and returns this buffer. It never *reduces*
the memory amount unless the value is 0, in which case it immediately
frees the current internal memory buffer (but it may be re-created by
adding data to the buffer). It also never changes the length() (only
the reserved capacity) unless passed 0.

```s2-member
Buffer compress([Buffer])
```

Compresses the buffer's contents in-place using a zlib-compatible
compression algorithm (plus a 4-byte size header stored in big-endian
order). Silently ignores requests to compress data which appear to be
compressed already. If built without compression support, it throws an
exception. If passed an argument, it evaluates that buffer, else it
evaluates this buffer. Returns the buffer it evaluates.

```s2-member
compression
```

If s2 is built with compression support, this is a string property
holding the name of the underlying implementation ("zlib" or "miniz"),
else it is a falsy value. See the [compression
section](#type-buffer-compression) for more details.

<a id="type-buffer-evalcontents"></a>
```s2-member
mixed evalContents([string name])
mixed evalContents(object symbols)
mixed evalContents([string name ,] object symbols)
mixed evalContents(object symbols [,string name])
```
Evaluates the contents of this buffer as s2 code, as if it were in a
file of its own, using an optional "file name" for error reporting
purposes (defaults to `typeinfo(name this)`). Script location
information generated by the eval'd code is (unlike when using
[second-round eval expansion](keyword-eval.md#keyword-eval-second-round))
correct, i.e. relative to the buffer's
contents. Note that `this` may be either a Buffer or a String.

If passed an object, all symbols in that object are imported into the
eval'd scope as local variables. This works just like the ["using"
function modifier](type-function.md#type-function-using), so assigning
over those variables from inside the eval'd script will not replace
their original values, only their local copies. This function accepts
its arguments in any combination: (string,obj), (obj,string), (obj),
(string), or (/\*no args\*/).

Tip: the `__FLC` keyword makes a useful ad-hoc value for the script
name argument. The eval'd script's name does not default to that value
only because normal callback functions (as opposed to in-language
features) don't have access to that level of script information.

> *ACHTUNG*: as of 20171013, the contents of the buffer are
temporarily moved out of the way during evaluation to avoid undefined
behaviour if/when the buffer's contents are modified during
evaluation. Thus, while `evalContents()` is running, the buffer will
initially appear empty and may be modified. After `evalContents()`
completes, the original contents are restored, discarding any changes
made to this buffer instance while `evalContents()` was running. If
you need to modify a buffer from within its own `evalContents()` (or
some nested construct), use `theBuffer.toString().evalContents()` (or
`takeString()`) instead, which will eval a *copy* of the buffer,
allowing it to be safely modified. Prior versions had undefined
behaviour in this corner case. This buffer-moving does not apply when
a String is the "this" of `evalContents()` because Strings are
immutable.


```s2-member
Buffer fill(byte [, startAtOffset=0 [,length=up to length()]])
```

Fills a range of bytes in the buffer with the given byte value. The
byte may be an integer or a string (in which case the first *byte*
(not UTF8 character) is used), and that value is masked against
0xFF. This function never writes beyond the buffer's `length()` (as
opposed to its `capacity()`), and will truncate larger requests to the
current length. Returns this buffer object.

> Potential TODO: if given a string, fill the whole buffer with that
string. The question then becomes whether or not to expand the buffer
if this routine would need to do so to apply the full string to the
final block.


```s2-member
bool isCompressed([Buffer])
```

Returns `true` if the buffer's contents appear to have been compressed
using the `compress()` method (or its C-level equivalent). Works
whether or not compression support is enabled. If passed an
argument, it evaluates that buffer, else it evaluates this buffer.

```s2-member
bool isEmpty()
```

Returns `true` if `0===length()`, else `false`, but this function is
more efficient than a size comparison because it does not have to
allocate an integer for the return.

```s2-member
integer|Buffer length([integer newLength])
```

If called with no arguments, returns the "effective length" of the
buffer, in bytes. The "length" is generally interpreted as the
"used" space - the space where memory has been filled by the client,
but that does not necessarily hold if the client sets the length or
chooses to follow other conventions for determining how much of the
buffer is "used" by his script. If called with an argument, it sets
the length and returns this buffer.

```s2-member
integer lengthUtf8()
```

Assumes the buffer is either empty or contains only valid UTF-8 and
returns the number of UTF-8 characters in the buffer. This is
equivalent to, but notably cheaper than, calling
`aBuffer.toString().length()`.

```s2-member-deprecated
Buffer new([initialCapacity=0])
```
Deprecated: prefer `new s2.Buffer()` instead.

```s2-member
new Buffer([initialCapacity=0])
```

Evaluates to a new buffer with the given initial capacity (in bytes).
Will trigger an OOM error if the requested capacity cannot be
allocated, and such errors are invariably(?) fatal to a script.

```s2-member
Buffer operator<<(arg)
```

Passes its argument to the `append()` method and returns this object.

```s2-member
Buffer readFile(filename)
```

*Appends* the contents of the given file to this buffer. Throws on
error. Returns this buffer instance. If this function is called
without a Buffer instance as its this, then it behaves differently: it
creates a new Buffer instance, populates the buffer from the given
file, and returns the new buffer. Thus it can be called "statically"
or its reference can be copied and called without a `this`. Empty
files result in an empty buffer, not `null`.

```s2-member
Buffer replace(string needle, string replacement [, limit =0])
Buffer replace(int needle, int replacement [, limit = 0])
```

Replaces instances of a string or byte value (in the range 0 to 255)
in this buffer and returns this buffer. If `limit` is greater than 0
then only the first that-many instances will be replaced, else all of
them will be.

```s2-member
Buffer reset()
```

Is equivalent to setting the length (not capacity) to 0, but also
zeroes out (does not free) the buffer's memory. Returns the buffer
object.

```s2-member
Buffer resize(integer n)
```

Similar to `capacity(n)`, but (A) forces the buffer's size to the
given value (a value of 0 acts similarly to `reset()`) and (B) does
not free the memory like `capacity(0)` does (but instead sets its
contents to an empty string, like `reset()` does). Returns the buffer
object.

```s2-member
Buffer slice([byteOffset = 0 [, byteCount -1]])
```

Creates a new buffer from a byte range of this buffer, starting at the
given offset (default=0) and copying the given number of bytes (a
negative value means "until the end"). Returns a new buffer.

Potential TODO: if passed a Buffer as its first argument, append the
result there (and return that buffer) instead of creating a new one.

```s2-member
string substr(int characterOffset [, int numberOfChars=-1])
```

Works just like
[`String.substr()`](type-string.md#type-string-methods), counting
offsets in terms of full characters (as opposed to bytes, like the
`toString()` method). If the buffer has no (used) memory associated
with it, this function behaves as if the buffer contained an empty
string.

```s2-member
string takeString()
```

Transfers the entire memory of the buffer into a new string value
which takes over ownership of that (*now immutable*) memory. This is
an optimization to use in place of `toString()` when the buffer will
otherwise be discarded, to avoid creating an unneeded copy of its
memory. After calling this, the buffer will be empty but can still be
used.

```s2-member
string toString([offsetInBytes = 0 [,lengthInBytes = this.length()]])
```

Creates a new string value from a copy of the contents of the buffer,
or `null` if the buffer has no memory (which is different from a buffer
with an empty string as content!). If passed no arguments it returns
the whole buffer as a String value. If passed 1 argument it returns
the first N *bytes* of the buffer as a String. If passed 2 arguments
it returns a string from the memory range (offset,length), truncating
the length to fit within the current length of the buffer. Be very
aware that when passing 1 or 2 arguments the substring might have a
truncated multi-byte character, leading to an invalid string! The
`substr()` method can be used to work with character-counting ranges.

```s2-member
Buffer uncompress([Buffer])
```

If `isCompressed()` is true, this decompresses the buffer's contents
in-place, else it has no effect. It silently ignores requests to
uncompress data which does not appear to be compressed. If built
without compression support, this throws an exception (see the
[compression section](#type-buffer-compression) for more details). If
passed an argument, it evaluates that buffer, else it evaluates this
buffer. Returns the buffer it evaluates.

```s2-member
undefined|integer uncompressedSize([Buffer])
```

If `isCompressed([Buffer])` is true, returns the uncompressed size of
the buffer, else returns `undefined`. Works whether or not compression
support is enabled (see the [compression section](#type-buffer-compression) for more details). If passed an
argument, it evaluates that buffer, else it evaluates this buffer.

```s2-member
Buffer writeFile(string filename [, append=false])
```

Writes the entire contents of the buffer (the "used" space) to the
given file. Throws on error. If the second argument is truthy then the
file is appended to, else it is truncated. In either case, it is
created if needed. Throws on error. Returns this buffer object.


<a id="type-buffer-appendf"></a>
# Formatting with `appendf()`

`Buffer.appendf()` provides a
[java.lang.String.format()](http://docs.oracle.com/javase/1.5.0/docs/api/java/lang/String.html#format(java.lang.String,%20java.lang.Object...)-like
mechanism for formatting strings. It resembles, but differs
significantly from, C's `printf(3)` conventions. Its first argument is
a string describing the format. Its second and subsequent arguments
are values for the formatting specifiers. It does not support explicit
locales and currently provides no format specifiers which could make
use of them (except doubles, which are output basically as if the "C"
locale is in effect, with an unspecified level of default (or maximum)
precision).

A format specifier has this syntax:

```s2
%N$[flags][[-]width][.precision][type]
```

The `N` part of `%N$` specifies the 1-based argument index which
should be formatted. It is 1-based because that is how
`java.lang.String.format()` does it (and it incidentally lines up with
the argument numbers after the format string). However, unlike in
Java, the index number is not optional[^43]. The value at that
argument index is expected to be of the type(s) specified by the
format specifier, or implicitly convertible to that type. Any given
argument may be referenced any number of times. Unreferenced arguments
are ignored but an exception is thrown if a requested argument index
is out of bounds or if the format string appears malformed.

The flags field may currently only be a `+`, which forces numeric
conversions to include a sign character even if they are positive.

Anything which is not a format specifier is appended as-is to the
buffer. `%%` is interpreted as a single `%` character, not a format
specifier.

How the width and precision are handled varies depending on the
`type` flag, which must one of the following single letters:

-   `b`: treat the value as a boolean, evaluate to `"true"` or `"false"`.
    Width and precision are ignored.
-   `B`: treat the value, which must be a String or Buffer, as a
    "blob", and encode it as a series of hex values, two hex characters
    per byte of length. The precision specifies the maximum number of
    hex byte *pairs* to output (so the formatted length will be *twice*
    the precision).
-   `c`: interprets the argument as a UTF8 character, either as the
    first character of a string/buffer argument or as an integer value.
    The width is interpreted as for the `%s` conversion. The precision, if
    positive, means to repeat the character that many times. e.g.
    `%1$10.5c` means to output the character argument 5 times and
    right-pad it out to a width of 10 characters (a negative width
    results in left-hand padding).
-   `d`, `o`, `x`, `X`: means format the argument as an integer
    in decimal, octal, hex (lowercase), or hex (uppercase),
    respectively. If a width is specified and starts with a `0` (zero)
    then `0` (instead of a space) is used for left-side padding if the
    number is shorter than the specified width. Precision is not
    supported. Note that the sign, if any, counts against the total
    string length for purposes of zero padding (that might be a bug -
    need to compare it to printf and Java).
-   `f`: double value. Width and precision work like `cwal_outputf()`
    and friends. Trailing zeroes are elided if no precision is
    specified, except that a zero immediately after the dot is retained.
-   `J`: runs the value through `cwal_json_output()` to convert it to
    a JSON string. The width can be used to specify indentation.
    Positive values indent by that many spaces per level, negative
    values indicate that many hard tabs per level. The precision is
    ignored. Will throw an exception if the value contains cycles (we
    cannot output cyclic JSON!) unless the JSON bits are built (by
    default) to output cycles in "some debugging-only string form."
    TODO: formatting is handled as the cwal level, not s2 level, and
    we still need a way of letting s2 take over (or supplement (or
    replace)) this conversion, e.g. by clients providing a `toJSON()`
    method which returns a JSON-able value (as opposed to the value's
    string representation).
-   `N`: format the value as null, regardless of its real value.
-   `p`: similar to `printf(3)`'s `%p`, but evaluates to a string in
    the form `TYPE_NAME@ADDRESS`, using the value's `typeinfo(name…)`
    and the hex notation form of the value's C-level memory
    address. Width and precision are ignored. Intended primarily for
    debugging.
-   `q`: expects a String/Buffer or null value. Converts all single
    quotes to two single quotes and interprets null as the string
    `"(NULL)"` (without the quotes)[^42]. Width and precision are
    ignored.
-   `Q`: similar to `q` but encloses the whole result in outer single
    quotes and interprets `null` values as the string `"NULL"`
    (without the quotes).
-   `r` and `R`: these perform URL encoding (`r`) and decoding (`R`),
    require a String argument, and support neither precision nor width.
    They only work on ASCII data[^43].
-   `s`: String or Buffer value. The precision determines the
    *maximum* length (in characters). The width determines the *minimum*
    length (in characters). If the string is shorter (in characters)
    than the absolute value of the width then a positive width will
    left-pad the string with spaces (i.e. right-justify it) and a
    negative width will right-pad the string with spaces. Achtung: for
    non-string/buffer types, this currently evaluates to `(nil)`. That is
    arguably a bug, but most other types can be coerced to strings with
    `''+theValue` or a type-specific method.
-   `U`: format the value as `undefined`, regardless of its real
    value.  Not quite sure what it's useful for, other than
    potentially as a placeholder for in-development code.
-   `y`: evaluates to the *type name* of the argument, and respects
    the value of the `__typename` magic property *if it is a string*.
    Width and precision are ignored. TODO: support width/precision as
    for the `s`' format.

TODOs(?):

-   Possibly: a mechanism for adding custom formats. This would allow s2
    to override the `J` (JSON) modifier and maybe plug in `strftime()`.
-   Possibly: non-decimal floating point (e.g. `e` notation). In my
    whole life i've never needed it (outside of high school chemistry,
    all of which i have long since forgotten).

Note that the buffer is *appended to* by this function, so when re-using
a buffer one may need to set its length to 0 (or call its `reset()`
method) before calling this to ensure a clean string value. Similarly,
when converting the buffer's contents to a string after appending,
`Buffer.takeString()` might be preferable over `Buffer.toString()`, as
it transfers the memory over to a new string, rather than allocating a
copy (it allocates (possibly recycles) only a small wrapper, internally
called a *z-string*).

When formatting fails an exception is thrown and the buffer's length is
reset such that looks like no appending had happened (its end-of-buffer
cursor points back to the pre-call location), but in fact its memory
might still contain the exception's error string somewhere between the
'used' position and the 'capacity' (don't rely on this behaviour, but
don't be surprised by it, either - it's a side-effect of the underlying
error reporting mechanism reusing the buffer to avoid another
allocation).

Here's an example script which wraps this functionality into a function
which returns a formatted string:

```s2
const formatString = proc callee(/*fmt, …*/){
 const b = callee.buffer ||| (callee.buffer = new s2.Buffer());
 return b.appendf(@argv).takeString();
};
```

Or, similarly:

```s2
const formatString = proc(/*fmt, …*/){
 return b.appendf(@argv).takeString();
} using {b: new s2.Buffer()};
```

Either form can be used like this:

```s2
print(formatString('0x%1$06x', 0x1234));
// outputs: 0x001234
```

Or simply use `String.applyFormat()`:

```s2
print('0x%1$06x'.applyFormat(0x1234))
```

Or, more simply (and more recently):

```s2
print(0x1234.toString('06x'))
```


<a id="type-buffer-compression"></a>
# Compression Support

If enabled at build-time, the `Buffer.compress()` and `uncompress()`
methods can be used to (un)compress buffered data. It uses a
zlib-compatible compression prefixed by a 4-byte header storing the
(unsigned) uncompressed size of the buffer in big-endian order.

To enable compression support when building s2, do *one* of the
following at build-time:

-   Define the precompiler macro `S2_INTERNAL_MINIZ` to a non-0
    integer value to use s2's internal copy of the
    [miniz](https://github.com/richgel999/miniz) library (a
    single-file 3rd-party implementation of zlib compression, with
    drop-in zlib compatibility APIs). No extra link-time parameters are
    needed, but note that if the app *also* links to libz (perhaps
    indirectly, e.g. via libssl) then there will be symbol collisions.
    In such a case, simply tell s2 to use zlib...
-   Define the precompiler macro `S2_ENABLE_ZLIB` to a non-0 integer
    value to use zlib (the header `<zlib.h>` must be in the
    includes path) and link the resulting app to *libz*.

Only one of those options may be enabled: if both are, a compilation
error will be triggered.

Minor pedantic caveat: the compression libraries use their own
allocators, and are not subject to any memory management/limits in s2.
i.e. even if s2 is configured to use, at most, 100kb of memory, the
underlying APIs might allocate a gazillion bytes. Likewise, in my
experience valgrind likes to complain about things in both zlib and
miniz now and then, so don't be surprised if new valgrind warnings
turn up when compression is enabled.

See [the buffer unit test script](/finfo/s2/unit/020-050-buffer.s2)
for examples of using the compression API in script code (search that
file for "compression").


# Footnotes

[^41]: i like to say that, but it's only abstractly true. The total
    overhead of calling a script-bound function is notably higher than
    appending a few strings together (but a function call might not
    need to allocate - they often get by using only values from the
    recycling bins).


[^42]:  That behaviour is up for debate. That was inherited from
    `sqlite3_printf()`'s %q modifier.

[^43]: Patches to improve that would be welcomed :);