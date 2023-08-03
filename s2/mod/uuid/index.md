# s2 UUIDs
#### ([&#x2b11;Main Module Docs](../))
# UUID Module

- Sources code: [](/dir/s2/mod/uuid?ci=trunk)
- Test/demo code: [](/finfo/s2/mod/uuid/test.s2), [](/finfo/s2/mod/uuid/lots.s2)

This module consists of a single function which takes no arguments and
returns a UUID in the form of a 36-character string (sans terminating
NUL byte). They are syntactically identical to [v4 UUIDs](https://en.wikipedia.org/wiki/Universally_unique_identifier)
except that they *do not reserve any static bits* - they are filled
entirely with random data.

The RNG source is currently static, but has yet, in tests of many
millions of UUIDs, to generate a duplicate. The underlying library
supports user-defined RNG sources, but there's currently not an
interesting way for script code to supply any because there's no good
way for script code to feed it random bytes unless they read from an
external random source, which would be unwieldy and would not perform
all that well. To that end, it generates an initial random seed based
on hashes of the state of certain volatile s2/cwal internals. This
isn't cryptographically secure, but (seriously) don't use s2 for
anything which needs to be cryptographically secure. (s2 is a toy,
remember?)

Potential TODO: allow it to take a Buffer or Array to append the
generated UUID to. The Buffer approach would allow it work without
allocating any memory, provided the target buffer is big enough.
