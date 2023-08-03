# s2 Hashing
#### ([&#x2b11;Main Module Docs](../))
# Hashing Module

- Source code: [](/dir/s2/mod/hashing?ci=trunk)
- Test/demo code: [](/finfo/s2/mod/hashing/test.s2)

This small module provides methods for hashing data of type String and
Buffer. As of this writing, it supports SHA1 and SHA3 hashes. This
modules provides the following methods:

```s2-member
string sha1(stringOrBuffer1 [... stringOrBufferN])
```

Returns a 40-byte lower-case hexadecimal hash of its inputs, all of
which must be of string or buffer type. If given more than one input,
it treats them as a single data set for hashing purposes, as if they
were concatenated. Note that this is the "classic" SHA1, not the newer
"hardened" one.

```s2-member
string sha3([int hashSize=256,] stringOrBuffer1 [... stringOrBufferN])
```

Returns a lower-case hexadecimal hash of its inputs, all of which must
be of string or buffer type. If given more than one input, it treats
them as a single data set for hashing purposes, as if they were
concatenated. The first argument may be an optional integer
representing the hash size, which must be an increment of 32 in the
inclusive range (128..512), defaulting to 256. The length of the
returned string is the hash's size divided by 4. An exception is
thrown if the hash size is not valid.
