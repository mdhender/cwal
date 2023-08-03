#if !defined(WANDERINGHORSE_NET_SHA1_H)
#define WANDERINGHORSE_NET_SHA1_H 1

#include <stdarg.h>

/** @page whsha1

The SHA1 implementation adapted from a Public Domain implementation
by Steve Reid <steve@edmweb.com>.

Maintainer: Stephan Beal (http://wanderinghorse.net/home/stephan/)

License: Public Domain
*/

#if defined(__cplusplus)
extern "C" {
#endif

    /**
       Type for holding SHA1 processing state.  Each instance must be
       initialized with whsha1_init(), populated with whsha1_update(),
       and "sealed" with whsha1_end().

       Sample usage:

       @code
       whsha1_cx cx;
       whsha1_init(&cx);
       whsha1_update(&cx, memory, lengthOfMemory);
       whsha1_end(&cx);
       @endcode

       After whsha1_end() is called cx.digest and cx.hex contain the
       binary and hex-string forms of the digest. Note that whsha1_update()
       may be called an arbitrary number of times to feed in chunks
       of memory (e.g. to stream in arbitrarily large data).
    */
    struct whsha1_cx {
        unsigned int state[5];
        unsigned int count[2];
        unsigned char buffer[64];
        /**
           Only valid after whsha1_end() has been called.
           Holds the 20-byte SHA1 digest/hash value.
         */
        unsigned char digest [20];

        /**
           Only valid after whsha1_end() has been called.
           Holds the 40-byte hex-string form of the digest
           value plus a NUL terminating byte.
        */
        char hex[41];
    };
    typedef struct whsha1_cx whsha1_cx;
    /**
       Empty-initialized whsha1_cx object.
     */
#define whsha1_cx_empty_m {                                             \
        {/*state*/0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0},\
        {/*count*/0,0},\
        {/*buffer*/0},\
        {/*digest*/0},\
        {/*hex*/0}\
    }

    /**
       Empty-initialized whsha1_cx object.
     */
    extern const whsha1_cx whsha1_cx_empty;

    /**
       Initializes cx with well-defined SHA1 starting state.
       
    */
    void whsha1_init(whsha1_cx *cx);

    /**
       Updates cx's state to include the first len bytes of data.

       If cx is NULL results are undefined (segfault!). If mem is not
       NULL then it must be at least n bytes long. If n is 0 then this
       function has no side-effects.
    */
    void whsha1_update( whsha1_cx *cx, void const *data, unsigned int len);

    /**
       "Finalizes" cx's state, adding padding as needed and populating
       cx->digest and cx->hex with the final hash code (in binary and
       hexidecimal-string forms, respectively).
    */
    void whsha1_end(whsha1_cx *cx);
    
    /**
       Convenience equivalent to:

       @code
       whsha1_init(cx);
       whsha1_update(cx, mem, n);
       whsha1_end(cx);
       @endcode
    */
    void whsha1_hash( whsha1_cx *cx, void const * mem, unsigned int n );
    
    /**
       Creates an SHA1 hash using the first n bytes of mem.

       dest must be at least 41 bytes long. 40 bytes of hex codes and
       a terminating NUL byte are written to it.
    */
    void whsha1_hash_hex( char * dest, void const * mem, unsigned int n );

    /**
       Creates an SHA1 hash using a series of n inputs.  Each variadic
       argument MUST be a NUL-terminated (char const *) and there must
       be no fewer than n of them. A hash is generated for the
       concatenated form of all of the strings.

       dest must be at least 41 bytes long. 40 bytes of hex codes and
       a terminating NUL byte are written to it.
    */
    void whsha1_hash_words_hex( char * dest, unsigned int n, ... );

    /**
       Identical to whsha1_hash_words_hex() but takes a va_list.
    */
    void whsha1_hash_words_hex_v( char * dest, unsigned int n, va_list vargs );
    

#if defined(__cplusplus)
} /* extern "C" */
#endif
    
#endif /* WANDERINGHORSE_NET_SHA1_H */
