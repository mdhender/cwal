#if !defined(WANDERINGHORSE_NET_SHA3_H)
#define WANDERINGHORSE_NET_SHA3_H 1

#include <stdint.h> /* uint64_t */

/** @page whsha3

The SHA3 API adapted by Stephan Beal
(https://wanderinghorse.net/home/stephan/) from the SHA3
implementation in the Fossil SCM (https://fossil-scm.rg) source tree,
with the following attribution and license:

************************************************************************
** Copyright (c) 2017 D. Richard Hipp
**
** This program is free software; you can redistribute it and/or
** modify it under the terms of the Simplified BSD License (also
** known as the "2-Clause License" or "FreeBSD License".)
**
** This program is distributed in the hope that it will be useful,
** but without any warranty; without even the implied warranty of
** merchantability or fitness for a particular purpose.
**
** Author contact information:
**   drh@hwaci.com
**   http://www.hwaci.com/drh/
************************************************************************
*/

#if defined(__cplusplus)
extern "C" {
#endif


/**
   Legal values for SHA3 hash sizes, in bits: an increment of 32 bits
   in the inclusive range (128..512).

   The hexidecimal-code size, in bytes, of any given bit size in this
   enum is the bit size/4.
*/
enum whsha3_hash_size {
/** Sentinel value. Must be 0. */
WHSHA3_INVALID = 0,
WHSHA3_128 = 128, WHSHA3_160 = 160, WHSHA3_192 = 192,
WHSHA3_224 = 224, WHSHA3_256 = 256, WHSHA3_288 = 288,
WHSHA3_320 = 320, WHSHA3_352 = 352, WHSHA3_384 = 384,
WHSHA3_416 = 416, WHSHA3_448 = 448, WHSHA3_480 = 480,
WHSHA3_512 = 512
};

/**
   Type for holding SHA3 processing state. Each instance must be
   initialized with whsha3_init(), populated with whsha3_update(),
   and "sealed" with whsha3_end().

   Sample usage:

   @code
   whsha3_cx cx;
   whsha3_init(&cx, WHSHA3_256);
   whsha3_update(&cx, memory, lengthOfMemory);
   whsha3_end(&cx);
   printf("Hash = %s\n", (char const *)cx.hex);
   @endcode

   After whsha3_end() is called cx.hex contains the hex-string forms
   of the digest. Note that whsha3_update() may be called an arbitrary
   number of times to feed in chunks of memory (e.g. to stream in
   arbitrarily large data).
*/
struct whsha3_cx {
    union {
        uint64_t s[25];         /* Keccak state. 5x5 lines of 64 bits each */
        unsigned char x[1600];  /* ... or 1600 bytes */
    } u;
    unsigned nRate;        /* Bytes of input accepted per Keccak iteration */
    unsigned nLoaded;      /* Input bytes loaded into u.x[] so far this cycle */
    unsigned ixMask;       /* Insert next input into u.x[nLoaded^ixMask]. */
    enum whsha3_hash_size size; /* Size of the hash, in bits. */
    unsigned char hex[132]; /* Hex form of final digest: 56-128 bytes
                               plus terminating NUL. */
};
/** Convenience typedef. */
typedef struct whsha3_cx whsha3_cx;

/**
   If the given number is a valid whsha3_hash_size value, its enum
   entry is returned, else WHSHA3_INVALID is returned.

   @see whsha3_init()
*/
enum whsha3_hash_size whsha3_hash_size_for_int(int);

/**
   Initialize a new hash. The second argument specifies the size of the hash
   in bits. Results are undefined if cx is NULL or sz is not a valid value.

   After calling this, use whsha3_update() to hash data and
   whsha3_end() to finalize the hashing process and generate a digest.
*/
void whsha3_init(whsha3_cx *cx, enum whsha3_hash_size sz);

/**
   Updates cx's state to include the first len bytes of data.

   If cx is NULL results are undefined (segfault!). If mem is not
   NULL then it must be at least n bytes long. If n is 0 then this
   function has no side-effects.

   @see whsha3_init()
   @see whsha3_end()
*/
void whsha3_update( whsha3_cx *cx, void const *data, unsigned int len);

/**
   To be called when hashing is complete: finishes the hash
   calculation and populates cx->hex with the final hash code in
   hexidecimal-string form. Returns the binary-form digest value,
   which refers to cx->size/8 bytes of memory which lives in the cx
   object. After this call cx->hex will be populated with cx->size/4
   bytes of lower-case ASCII hex codes plus a terminating NUL byte.

   @see whsha3_init()
   @see whsha3_update()
*/
unsigned char const * whsha3_end(whsha3_cx *cx);
    
#if defined(__cplusplus)
} /* extern "C" */
#endif
    
#endif /* WANDERINGHORSE_NET_SHA3_H */
