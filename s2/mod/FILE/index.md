# s2 FILE wrapper
#### ([&#x2b11;Main Module Docs](../))
# FILE Module

- Sources code: [](/dir/s2/mod/FILE?ci=trunk)
- Test/demo code: [](/finfo/s2/mod/FILE/mod_FILE.s2)

The `FILE` module approximates the C89-specific FILE API plus a few
utilities.

In practice, general-purpose `FILE` handles are not all that useful in
s2. The built-in Buffer API provides methods for reading a file's
contents and writing a buffer to a file, and those operations
adequately cover the overwhelmingly vast majority of
file-content-handling cases required by s2 client code. This module is
provided primarily to fill a *perceived* functionality gap, rather
than to fill a real/practical functionality gap. "Because we can," as
opposed to "because we should."

Caveat: this module currently does not support "large files", and some
APIs may misbehave when used on files of a size larger than will fit
the platform's signed long int type (2GB for a 32-bit long). That
said, it is not expected that s2 is the best tool for the job when
working with such files.

# Module Methods

These methods may be used directly on the module object:

```s2-member
new FILE(string filename [, openmode="rb"])
```
This constructor Works identically to the `open()` method, described
below.

```s2-member
FILE open(string filename [, openmode="rb"])
```

Analogous to C's `fopen(3)` function, taking the same arguments. The
returned `FILE` handle gets a property called "name" with the same value
passed to this function. It supports 3 special names which refer to
the system's standard streams: `:stdin:`, `:stdout:`, and `:stderr:`. They
behave like other streams except that their `openmode` is ignored and
`close()` does not really close the stream (it simply disconnects the
script-space `FILE` handle from the stream). Note that output to
`:stdout:` or `:stderr:` bypasses the script engine's output layer, which
means that s2/cwal-level output buffers/proxies *are not honored* when
writing to these streams.

```s2-member
bool unlink(string filename [, throwOnError=true])
```

Analogous to C's `unlink(2)` function. If passed a falsy value for the
2nd argument then it will suppress any error, e.g. the file is not
found, otherwise it throws on error. If the 2nd argument is falsy then
it returns a boolean indicating success or failure, rather than
throwing. Note that not all platforms support `unlink()`, nor do all
support `unlink()`ing an opened file. This does not work on directories.

# `FILE` Instance Methods

`FILE` instances are creates using the use the module-level `open()`
method or constructor (they're functionally equivalent). Each FILE
instance inherits a prototype with the following methods:

```s2-member
FILE clearError()
```
Clears any EOF or error flag and returns this object.

```s2-member
void close()
```
Closes the file handle and cleans up any C-level resources.

```s2-member
bool eof()
```
Returns true if the file's cursor is at EOF, else false.  Note that it
returns false for a newly-opened, empty file (because that's how
`feof(3)` works).

```s2-member
FILE flush()
```
Flushes any pending buffered output and returns this object.

```s2-member
bool hasError()
```
Returns true if the file handle has any error flag set.

```s2-member
FILE read([byteCount=-1,] Buffer target)
Buffer read([byteCount=-1])
```

Reads up to `byteCount` *bytes* (not UTF8 *characters*) from the file
stream and appends them to the given target buffer or (if no buffer is
passed in) returns them in a new buffer. If `byteCount` is -1 then it
reads until EOF. `byteCount` values of 0, or less than -1, are
illegal. Because it reads raw bytes, rather than UTF characters, any
given read snippet may contain partial UTF characters when reading UTF
input, thus using `toString()` or `takeString()` on the resulting buffer
might not be legal.


```s2-member
FILE rewind()
```
Sets the file's cursor to the start of the file.

```s2-member
FILE seek(int offset [, int whence = seek.SEEK_SET])
```
Moves the file cursor exactly as per `fseek(2)`. The C-level constants
`SEEK_SET`, `SEEK_CUR`, and `SEEK_EOF` are defined as member properties
of this method. Seeking clears any EOF flag on the file. Note that
`seek()` only works on random-access `FILE` handles, not those opened in
"append" mode, nor for `stdin` (which, interestingly, has an effective
mode not represented by a corresponding `open()` flag).

```s2-member
integer size()
```
Returns the current size of the file. Currently limited to sizes which
fit in a C-native long int value. It uses `seek()` and `tell()` to figure
out the size, which clears any EOF flag.

```s2-member
integer tell()
```
Returns the file's current cursor position, in bytes from its starting
position.

```s2-member
FILE unlink()
```
Tries to `unlink(2)` this opened file. Throws on error.  Returns this
object. See the module-level method of the same name for more details.

```s2-member
FILE write(string | Buffer)
```
Writes all bytes of the given string or buffer to this file, at the
file's current cursor position. Returns this object. Note that it does
not support writing of numbers and booleans and such because doing so
would require writing platform-dependent amounts of bytes. To write
binary data with this method it is necessary to encode the data (using
a client-defined encoding) into a Buffer, then `write()` that Buffer.
