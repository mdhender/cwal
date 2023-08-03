# s2 Felta - Content Delta Generator
#### ([&#x2b11;Main Module Docs](../))
# Felta - Content Delta Generator

- Source code: [](/dir/s2/mod/felta?ci=trunk)
- Test/demo code: [](/finfo/s2/mod/felta/test.s2)

This loadable module contains bindings to a C API for generating
content deltas using the delta algorithm from the [Fossil
SCM](https://fossil-scm.org). This code was ported directly from
Fossil, refactored into a standalone library. It creates space-efficient
deltas between two versions of arbitrary content, be it text or binary.

***UTF8 Achtung:*** though this API accepts both strings and buffers
for most inputs, if the data are non-UTF8 they must be provided via
Buffers, not strings. A delta can hypothetically split up multi-byte
UTF8 characters, and therefore may contain "binary" data even if the
inputs are UTF8, so converting deltas from buffers (the form this API
generates them in) to strings may (depending on the data) have
undefined results.

# Felta Methods

This module implements the following functions, listed in alphabetical order:

```s2-member
int appliedLength(Buffer delta [, throwIfNotDelta=true])
```

Given a buffer of delta content (created with the `create()`
function), this function returns its "applied" length, in bytes. That
is, when the delta is applied to its original input source, the value
returned by this function is the size of the buffer needed to hold the
result. By default it throws if its argument is not a delta, but if
the second argument is truthy, it instead returns `undefined` for that
case.

Note that `isDelta()` is more efficient if a caller simply wants to know
if a piece of data is a delta, as it doesn't have to allocate an integer
for the return result.


```s2-member
Buffer apply(Buffer|string version1, Buffer delta)
```

Given a "version 1" of a piece of content and a delta which was
created against that content using the `create()` method, this
function applies the delta to "version 1" to recreate
and return "version 2" of that content.


```s2-member
Buffer create(Buffer|string version1, Buffer|string version2)
```

Given content in the form of two buffers or strings, this function
creates a delta which, when applied to the first argument, will
create the content of the second argument.


```s2-member
boolean isDelta(mixed arg)
```

Returns `true` if its argument is a delta-format string or buffer,
else returns `false`. Throws if passed no arguments or more than 1 argument.
