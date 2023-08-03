# whcl: Buffers
##### ([&#x2b11;Table of Contents](./)) ([data types](type-intro.md))
# Memory Buffers 
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

Jump to...

* [Introduction](#type-buffer)
* [Buffer Methods](#type-buffer-methods)

<a id="type-buffer"></a>
Buffer
============================================================

The Buffer class offers a byte array and is most commonly used for
creating strings dynamically. It inherits [the object
class](type-object.md), so includes all of those methods,
plus those listed in the following sections.


<a id="type-buffer-methods"></a>
Buffer Methods
============================================================

<a id="method-ctor"></a>
Constructor
------------------------------------------------------------

Buffers are often returned via various APIs and can be created using:

```whcl
new whcl[Buffer]
```

The constructor optionally accepts an amount of memory to reserve.


<a id="method-append"></a>
append
------------------------------------------------------------

Usage: `append value [...value]`

Appends any number of values, in "some string form" to the buffer.



<a id="method-append-json"></a>
append-json
------------------------------------------------------------

Usage: `append-json value [indentation [cyclesAsStrings=false]]`

Appends a value in its JSON form to the buffer. The optional
second argument may be an integer (positive means a number
of spaces to indent and negative means a number of tabs)
or a string to use for each level of indentation. If the final
argument is false (the default) then any cycles in the value
will trigger an exception, as JSON cannot handle cycles.
If that argument is true then cycles are rendered as strings
in some "debug-like" form.

Returns this object.

<a id="method-appendf"></a>
appendf
------------------------------------------------------------

Usage: `appendf format-string ...values`


`appendf()` provides a
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

>
    %N$[flags][[-]width][.precision][type]


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
    cannot output cyclic JSON!).
-   `N`: format the value as `null`, regardless of its real value.
-   `p`: similar to `printf(3)`'s `%p`, but evaluates to a string in
    the form `TYPE_NAME@ADDRESS`, using the value's type name and the
    hex notation form of the value's C-level memory address. Width and
    precision are ignored. Intended primarily for debugging.
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
    a type-specific method.
-   `U`: format the value as `undefined`, regardless of its real
    value.  Not quite sure what it's useful for, other than
    potentially as a placeholder for in-development code.
-   `y`: evaluates to the *type name* of the argument.
    Width and precision are ignored.

Note that the buffer is *appended to* by this function, so when
re-using a buffer one may need to set its length to 0 (or call its
`reset` method) before calling this to ensure a clean string
value. Similarly, when converting the buffer's contents to a string
after appending, `take-string` might be preferable over `to-string`,
as it transfers the memory over to a new string, rather than
allocating a copy (it allocates (possibly recycles) only a small
wrapper, internally called a *z-string*).

When formatting fails an exception is thrown and the buffer's length is
reset such that looks like no appending had happened (its end-of-buffer
cursor points back to the pre-call location), but in fact its memory
might still contain the exception's error string somewhere between the
'used' position and the 'capacity' (don't rely on this behaviour, but
don't be surprised by it, either - it's a side-effect of the underlying
error reporting mechanism reusing the buffer to avoid another
allocation).



<a id="method-byte-at"></a>
byte-at
------------------------------------------------------------

Usage: `byte-at [offset [value]]`

Gets or sets the byte value at the given byte index. An assigned value
is masked against 0xFF. In setter mode it extends the "used" length if
the given offset is past the end, then returns this buffer
instance. In getter mode it returns an integer value between 0 and
255, or `undefined` if the offset is out of range.


<a id="method-capacity"></a>
capacity
------------------------------------------------------------

Usage: `capacity [integer newCapacity]`

If passed no arguments, it returns this buffer's current capacity, in
bytes. Otherwise it reserves at least the given amount of memory in
the buffer (possibly more) and returns this buffer. It never *reduces*
the memory amount unless the value is 0, in which case it immediately
frees the current internal memory buffer (but it may be re-created by
adding data to the buffer). It also never changes the `length` (only
the reserved capacity) unless passed 0.

<a id='method-eval-contents'></a>
eval-contents
------------------------------------------------------------

Usages:

- `eval-contents [pseudoFilename [varsObject]]`
- `eval-contents [varsObject [pseudoFilename]]`

Evaluates the contents of this buffer as whcl code, as if it were in a
file of its own, using an optional "file name" for error reporting
purposes (defaults to `info type-name this`). Script location
information generated by the eval'd code is. Note that `this` may be
either a Buffer or a String.

If passed an object, all symbols in that object are imported into the
eval'd scope as local variables. Assigning over those variables from
inside the eval'd script will _not_ replace their original values,
only their local copies. This function accepts its arguments in any
combination: (string,obj), (obj,string), (obj), (string), or no
arguments.

The result value of this call is that of evaling the string, i.e. the
result of the final command in the code. If the eval'd code invokes
the `return` builtin, that result is captured by this function and the
value of the return becomes the result of this function.

Tip: the `__FLC` keyword makes a useful ad-hoc value for the script
name argument. The eval'd script's name does not default to that value
only because normal callback functions (as opposed to in-language
features) don't have access to that level of script information.

Tip: this is a relatively expensive approach to evaluating code, as
the buffer has to be recompiled each time this function is called. It
is far more efficient to pack re-used code into a function, as those
only have to be compiled a single time.

> *ACHTUNG*: the contents of the buffer are temporarily moved out of
the way during evaluation to avoid undefined behaviour if/when the
buffer's contents are modified during evaluation. Thus, while
`eval-contents` is running, the buffer will initially appear empty and
may be modified. After `eval-contents` completes, the original
contents are restored, discarding any changes made to this buffer
instance while `eval-contents` was running. If you need to modify a
buffer from within its own `eval-contents` (or some nested construct),
use `theBuffer[to-string]` to get a _copy_ of its contents as a
string, then call `thatString eval-contents`. This buffer-moving does
not apply when a String is the "this" of `eval-contents` because
Strings are immutable.


<a id="method-fill"></a>
fill
------------------------------------------------------------

Usage: `fill`

Fills a range of bytes in the buffer with the given byte value. The
byte may be an integer or a string (in which case the first *byte*
(not UTF8 character) is used), and that value is masked against
0xFF. This function never writes beyond the buffer's `length` (as
opposed to its `capacity`), and will truncate larger requests to the
current length. Returns this buffer object.

> Potential TODO: if given a string, fill the whole buffer with that
string. The question then becomes whether or not to expand the buffer
if this routine would need to do so to apply the full string to the
final block.


<a id="method-is-empty"></a>
is-empty
------------------------------------------------------------

Usage: `is-empty`


Returns `true` if `length` is 0, else `false`, but this function is
more efficient than a size comparison because it does not have to
allocate an integer for the return.


<a id="method-length"></a>
length
------------------------------------------------------------

Usage: `length [integer new-size]`

If called with no arguments, returns the "effective length" of the
buffer, in bytes. The "length" is generally interpreted as the "used"
space - the space where memory has been filled by the client, but that
does not necessarily hold if the client sets the length or chooses to
follow other conventions for determining how much of the buffer is
"used" by his script. If called with an argument, it sets the length
and returns this buffer.


<a id="method-length-utf8"></a>
length-utf8
------------------------------------------------------------

Usage: `length-utf8`

_Assumes_ the buffer is either empty or contains only valid UTF-8 and
returns the number of UTF-8 characters in the buffer.


<!--

<a id="method-new"></a>
new
------------------------------------------------------------

Usage: `new`

-->

<a id="method-read-file"></a>
read-file
------------------------------------------------------------

Usage: `read-file`

*Appends* the contents of the given file to this buffer. Throws on
error. Returns this buffer instance. If this function is called
without a Buffer instance as its `this`, then it behaves differently:
it creates a new Buffer instance, populates the buffer from the given
file, and returns the new buffer. Thus it can be called "statically"
or its reference can be copied and called without a `this`. Empty
files result in an empty buffer, not `null`.


<a id="method-replace"></a>
replace
------------------------------------------------------------

Usages:

- `replace string needle string replacement [limit = 0]`
- `replace int needle int replacement [limit = 0]`


Replaces instances of a string or byte value (in the range 0 to 255)
in this buffer and returns this buffer. If `limit` is greater than 0
then only the first that-many instances will be replaced, else all of
them will be. Returns this object. If passed integers, they are masked
against 0xFF.

<a id="method-reset"></a>
reset
------------------------------------------------------------

Usage: `reset`

Is equivalent to setting the length (not capacity) to 0, but also
zeroes out (does not free) the buffer's memory. Returns the buffer
object.


<a id="method-resize"></a>
resize
------------------------------------------------------------

Usage: `resize newSize`

Similar to `capacity`, but (A) forces the buffer's size to the
given value (a value of 0 acts similarly to `reset`) and (B) does
not free the memory like `capacity 0` does (but instead sets its
contents to an empty string, like `reset` does). Returns the buffer
object.


<a id="method-slice"></a>
slice
------------------------------------------------------------

Usage: `slice [byteOffset = 0 [byteCount -1]]`

Creates a new buffer from a byte range of this buffer, starting at the
given offset (default=0) and copying the given number of bytes (a
negative value means "until the end"). Returns a new buffer.

(FIXME? The byteCount semantics differ from the `array slice` method!
Since a slice of 0 bytes is not useful, we may want to adapt this
impl, rather than array's.)

Potential TODO: if passed a Buffer as its first argument, append the
result there (and return that buffer) instead of creating a new one.


<a id="method-substr"></a>
substr
------------------------------------------------------------

Usage: `substr offset [length=-1]`

Works just like [`string substr`](type-string.md#method-substr),
counting offsets in terms of full characters (as opposed to bytes,
like the `to-string` method). If the buffer has no (used) memory
associated with it, this function behaves as if the buffer contained
an empty string.


<a id="method-take-string"></a>
take-string
------------------------------------------------------------

Usage: `take-string`

Transfers the entire memory of the buffer into a new string value
which takes over ownership of that (*now immutable*) memory. This is
an optimization to use in place of `to-string` when the buffer will
otherwise be discarded, to avoid creating an unneeded copy of its
memory. After calling this, the buffer will be empty but can still be
used.


<a id="method-to-string"></a>
to-string
------------------------------------------------------------

Usage: `to-string [byteCount] | [byteOffset byteLength]`

Achtung: this method might be removed or become an alias for
`substr`.

Creates a new string value from a copy of the contents of the buffer,
or `null` if the buffer has no memory (which is different from a buffer
with an empty string as content!). If passed no arguments it returns
the whole buffer as a String value. If passed 1 argument it returns
the first N *bytes* of the buffer as a String. If passed 2 arguments
it returns a string from the memory range (offset,length), truncating
the length to fit within the current length of the buffer. Be very
aware that when passing 1 or 2 arguments the substring might have a
truncated multi-byte character, leading to an invalid string! The
`substr` method can be used to work with character-counting ranges.



<a id="method-write-file"></a>
write-file
------------------------------------------------------------

Usage: `write-file filename [append=false]`

Writes the entire contents of the buffer (the "used" space) to the
given file. Throws on error. If the second argument is truthy then the
file is appended to, else it is truncated. In either case, it is
created if needed. Throws on error. Returns this buffer object.



Footnotes
============================================================

[^42]:  That behaviour is up for debate. That was inherited from
    `sqlite3_printf()`'s %q modifier.

[^43]: Patches to improve that would be welcomed :);
