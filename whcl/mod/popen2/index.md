# whcl popen2
#### ([&#x2b11;Main Module Docs](../))
# whcl: Pipe Data to and from Sub-processes
<style>@import url(../../../doc/fossil-doc.css)</style>
<script src="../../../doc/highlightjs/highlight-cwal.min.js"></script>

- Source code: [](/dir/whcl/mod/popen2?ci=trunk)
- Test/demo code: [](/finfo/whcl/mod/popen2/test.whcl)

This module, ported in from a utility API in the [Fossil
SCM](https://fossil-scm.org) source tree, provides the ability to
start external processes and pipe data to and from them. It's
conceptually similar to the `popen()` C API except that it provides a
two-way pipe.

Construction and Methods
========================================

This module's only function acts as a factory function:

```whcl
decl p [whcl.popen2 '/path/to/command -flags']
affirm [info type-name $p] == 'popen2'
```

(TODO: accept an array of flags and change internal processing to
store an array instead of a single string.)

Each instance inherits the following methods...

close
----------------------------------------

Usage: `close`

Closes the stream and frees any C-level resources. Further method
calls after this will trigger an exception. Returns `undefined`.

flush
----------------------------------------

Usage: `flush`

Flushes the outbound stream and returns this object. Normally this would
be needed before each `read` call, to ensure that the read does not block while
waiting on the outbound stream, but the internals automatically call this
if a `read` is called after a non-flushed `write`. Returns `this`.

read
----------------------------------------

Usage: `read buffer [int N=-1]`

It reads, at most, `N` bytes from the process's output stream, or all
of it if `N` is negative. This is a blocking operation, ending when
the specified number of bytes have been read or EOF is reached.

When told to read only part of the input (N>=0 or not specified), it
returns the number of bytes read, else returns `this`.


write
----------------------------------------

Usages:

- `write [-close] buffer [offset=0 [N=-1]]`
- `write [-close] string`

The first form writes the given buffer to the input sink on the this
object. The offset specifies which byte (not character) to start at in
the buffer and N specifies how many bytes to write, a negative value
meaning "all of it."

The second form writes the given string to the input sink.

Both forms return the number of bytes written.

The `-close` flag means to close the outbound stream at the end of the
write, in which case future calls to this method or `flush` will
trigger an exception. If `-close` is provided, an input value argument
is optional.
