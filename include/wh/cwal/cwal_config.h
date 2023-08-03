/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=4 et sw=2 tw=80: */
#if !defined(WANDERINGHORSE_NET_CWAL_CONFIG_H_INCLUDED)
#define WANDERINGHORSE_NET_CWAL_CONFIG_H_INCLUDED 1

/**
   Optionally #include a user-defined header, whereby compilation
   options may be set prior to where they take effect, but after
   platform setup. If CWAL_CUSTOM_CONFIG_H=? is defined, its value
   names the #include file (without quotes, which will be added
   automatically).
*/
#ifdef CWAL_CUSTOM_CONFIG_H
# define INC_STRINGIFY_(f) #f
# define INC_STRINGIFY(f) INC_STRINGIFY_(f)
# include INC_STRINGIFY(CWAL_CUSTOM_CONFIG_H)
# undef INC_STRINGIFY
# undef INC_STRINGIFY_
#endif

#if defined(HAVE_CONFIG_H)
#  include "config.h"
#endif
#if defined(HAVE_AUTOCONFIG_H)
#  include "autoconfig.h"
#endif

#if !defined(CWAL_VERSION_STRING)
#define CWAL_VERSION_STRING "?CWAL_VERSION_STRING?"
#endif

#if !defined(CWAL_CPPFLAGS)
#define CWAL_CPPFLAGS "?CWAL_CPPFLAGS?"
#endif

#if !defined(CWAL_CFLAGS)
#define CWAL_CFLAGS "?CWAL_CFLAGS?"
#endif

#if !defined(CWAL_CXXFLAGS)
#define CWAL_CXXFLAGS "?CWAL_CXXFLAGS?"
#endif

#if !defined(CWAL_SWITCH_FALL_THROUGH)
#  if defined(__GNUC__) && !defined(__clang__) && (__GNUC__ >= 7)
/*
  #define CWAL_USING_GCC

  gcc v7+ treats implicit 'switch' fallthrough as a warning
  (i.e. error because we always build with -Wall -Werror -Wextra
  -pedantic). Because now it's apparently considered modern to warn
  for using perfectly valid features of the language. Holy cow, guys,
  what the hell were you thinking!?!?!?

  Similarly braindead, clang #defines __GNUC__.

  So now we need this ugliness throughout the source tree:

  #if defined(CWAL_USING_GCC)
  __attribute__ ((fallthrough));
  #endif

  It turns out that one can write "fall through", case sensitive (or
  not, depending on the warning level), as a _standalone C-style
  comment_ to (possibly) achieve the same result (depending on the
  -Wimplicit-fallthrough=N warning level, which can be set high enough
  to disable that workaround or change its case-sensitivity).

  Facepalm! FacePalm!! FACEPALM!!!

  PS: i wanted to strip comments from one large piece of generated
  code to reduce its distribution size, but then gcc fails to compile
  it because of these damned "fall through" comments. gcc devs, i hate
  you for this.
*/
#    define CWAL_SWITCH_FALL_THROUGH __attribute__ ((fallthrough))
#  else
#    define CWAL_SWITCH_FALL_THROUGH
#  endif
#endif
/* /CWAL_SWITCH_FALL_THROUGH

   TODO: add support for the C++ attributes for doing this.
*/

#if defined(__arm__) || defined(__thumb__) \
    || defined(__TARGET_ARCH_ARM) \
    || defined(__TARGET_ARCH_THUMB) \
    || defined(_ARM) \
    || defined(_M_ARM) \
    || defined(_M_ARMT)
/* adapted from http://sourceforge.net/p/predef/wiki/Architectures/ */
#define CWAL_PLATFORM_ARM 1
#endif

#if defined(__cplusplus) && !defined(__STDC_FORMAT_MACROS)
/* inttypes.h needs this for the PRI* and SCN* macros in C++ mode. */
#  define __STDC_FORMAT_MACROS
#endif
#include <inttypes.h> /* C99 PRIuXX macros and fixed-size
                         integers. */
/*#define CWAL_VOID_PTR_IS_BIG 1*/

#if defined(CWAL_SIZE_T_BITS)
# error "CWAL_SIZE_T_BITS must not be defined before including this file! Edit this file instead!"
#endif

/**
   A workaround for late-2015 gcc versions adding __func__ warnings
   to -pedantic mode.
*/
#if !defined(__cplusplus) && !defined(__func__)
#  if !defined(__STDC_VERSION__) || (__STDC_VERSION__ < 199901L)
#    define __func__ "__func__"
#  endif
#endif


/** @def CWAL_ENABLE_JSON_PARSER

   If CWAL_ENABLE_JSON_PARSER is set to 0 then the cwal_json_parse()
   family of functions get disabled (they still exist, but will return
   errors). It has no effect on the JSON output routines.

   The reason for this is only one of licensing: the (3rd-party)
   parser uses a BSD-style license with a "do no evil" clause, and
   that clause prohibits the code from being used in certain package
   repositories.

   Note that setting this macro might not be enough to please package
   maintainers - they may also require physical removal of the code
   blocked off by this macro. It can all be found in cwal_json.c.
*/
#if !defined(CWAL_ENABLE_JSON_PARSER)
#define CWAL_ENABLE_JSON_PARSER 1
#endif

/** @def CWAL_DISABLE_FLOATING_POINT

    "The plan is" to allow compiling cwal with no support for doubles, but
    that is not yet in place.
*/
#if !defined(CWAL_DISABLE_FLOATING_POINT)
#define CWAL_DISABLE_FLOATING_POINT 0
#endif

/** @def CWAL_VOID_PTR_IS_BIG

    ONLY define this to a true value if you know that

    (sizeof(cwal_int_t) <= sizeof(void*))

    If that is the case, cwal does not need to dynamically allocate
    integers. ALL CLIENTS must use the same value for this macro.

    This value is (or was?) also used to
    optimize/modify/make-more-portable a couple other internal bits.

    FIXME: figure out exactly how this plays together with
    CWAL_INT_T_BITS and document what combinations are portable across
    c89/c99 and 32/64 bits.
    */
#if !defined(CWAL_VOID_PTR_IS_BIG)

    /* Largely taken from http://predef.sourceforge.net/prearch.html

    See also: http://poshlib.hookatooka.com/poshlib/trac.cgi/browser/posh.h
    */
#  if defined(_WIN64) || defined(__LP64__)/*gcc*/                       \
    || defined(_M_X64) || defined(__amd64__) || defined(__amd64)        \
    ||  defined(__x86_64__) || defined(__x86_64)                        \
    || defined(__ia64__) || defined(__ia64) || defined(_IA64) || defined(__IA64__) \
    || defined(_M_IA64)                                                 \
    || defined(__sparc_v9__) || defined(__sparcv9) || defined(_ADDR64)  \
    || defined(__64BIT__)
#    define CWAL_VOID_PTR_IS_BIG 1
#  else
#    define CWAL_VOID_PTR_IS_BIG 0
#  endif
#endif

/** @def CWAL_SIZE_T_BITS

    CWAL_SIZE_T_BITS defines the number of bits used by cwal's primary
    cwal_size_t. This is used so that cwal can guaranty and enforce
    certain number ranges.

    This value must be one of (16,32,64), though values lower than 32
    may or may not work well with any given component of the library
    (e.g. string interning can use relatively much compared to the
    rest of the lib).
*/

/** @def CWAL_INT_T_BITS
    
   CWAL_INT_T_BITS is the cwal_int_t counterpart of
   CWAL_SIZE_T_BITS. cwal_int_t is independent of cwal_size_t, and may
   have a different size.

*/

#if 0
/**
   For testing purposes, or if a client wants to override this
   globally...

   Reminder to self: a value of 64 does not work on ODroid ARM/Linux.
   Problems are mostly related to hash seeds and bitmasks being too
   large for unsigned long on that platform.
*/
#  define CWAL_SIZE_T_BITS 16
#endif

#if !defined(CWAL_SIZE_T_BITS)
/**
  CWAL_SIZE_T_BITS "really should" not be higher than the pointer size
  (in bits), since it would be impossible to allocate anywhere near
  that number of items and this value is largely used as a length- and
  reference counter. It is NOT anticipated that cwal will be used in
  any environments where an unsigned 32-bit limit could ever be
  reached for the things it uses cwal_size_t for (discounting
  artifical/malicious inflation of reference counts and such).

  A value of 16 is perfectly reasonable for small use cases. It
  doesn't save _much_ memory, but it does save some.
*/
#  if CWAL_VOID_PTR_IS_BIG
#    define CWAL_SIZE_T_BITS 64
#  else
#    define CWAL_SIZE_T_BITS 32
#  endif
#endif

#if !defined(CWAL_INT_T_BITS)
/**
  The ONLY reason we fall back to 32 bits here is because C89 lacks a
  portable printf format string for the equivalent of PRIi64
  :/. Other than that 64-bits will (should!) work find on 32-bit
  platforms as long as CWAL_VOID_PTR_IS_BIG is false. In C99 mode
  64-bit int compiles fine on 32-bit (because the result of PRIi64 is
  well-defined there).
*/
#  if CWAL_VOID_PTR_IS_BIG || (defined(__STDC_VERSION__) && (__STDC_VERSION__>=199901L))
#    define CWAL_INT_T_BITS CWAL_SIZE_T_BITS
#  else
#    define CWAL_INT_T_BITS CWAL_SIZE_T_BITS
#  endif
#endif


/** @def CWAL_SIZE_T_PFMT

    Is is a printf-style specifier, minus the '%' prefix, for
    use with cwal_size_t arguments. It can be used like this:

    @code
    cwal_size_t x = 42;
    printf("The value of x is %"CWAL_SIZE_T_PFMT".", x );
    @endcode

    Using this constant ensures that the printf-style commands
    work when cwal_size_t is of varying sizes.

    @see CWAL_SIZE_T_SFMT
*/

/** @def CWAL_SIZE_T_SFMT

CWAL_SIZE_T_SFMT is the scanf counterpart of CWAL_SIZE_T_PFMT.

@see CWAL_SIZE_T_PFMT
@see CWAL_SIZE_T_SFMT
*/

/** @def CWAL_SIZE_T_PFMTX

CWAL_SIZE_T_PFMTX is the upper-case hexidecimal counterpart of
CWAL_SIZE_T_PFMT.

@see CWAL_SIZE_T_PFMT
*/

/** @def CWAL_SIZE_T_PFMTx

CWAL_SIZE_T_PFMTX is the lower-case hexidecimal counterpart of
CWAL_SIZE_T_PFMT.

@see CWAL_SIZE_T_PFMT
*/


/** @def CWAL_SIZE_T_PFMTo

CWAL_SIZE_T_PFMTo is the octal counterpart of CWAL_SIZE_T_PFMT.

@see CWAL_INT_T_SFMT
*/

    
/** @def CWAL_SIZE_T_SFMTX

CWAL_SIZE_T_SFMTX is the hexidecimal counterpart to CWAL_SIZE_T_SFMT.

@see CWAL_SIZE_T_PFMT
@see CWAL_SIZE_T_SFMT
*/

/** @def CWAL_INT_T_PFMT

CWAL_INT_T_PFMT is the cwal_int_t counterpart of CWAL_SIZE_T_PFMT.

@see CWAL_SIZE_T_PFMT
@see CWAL_INT_T_SFMT
*/

/** @def CWAL_INT_T_SFMT

CWAL_INT_T_SFMT is the scanf counterpart of CWAL_INT_T_PFMT.

@see CWAL_INT_T_PFMT
*/

/** @def CWAL_INT_T_PFMTX

CWAL_INT_T_PFMTX is the upper-case hexidecimal counterpart of
CWAL_INT_T_PFMT.

@see CWAL_INT_T_SFMT
*/

/** @def CWAL_INT_T_PFMTx

CWAL_INT_T_PFMTX is the lower-case hexidecimal counterpart of
CWAL_INT_T_PFMT.

@see CWAL_INT_T_SFMT
*/

/** @def CWAL_INT_T_PFMTo

CWAL_INT_T_PFMTo is the octal counterpart of CWAL_INT_T_PFMT.

@see CWAL_INT_T_SFMT
*/

    
/** @def CWAL_INT_T_SFMTX

CWAL_INT_T_SFMTX is the hexidecimal counterpart to CWAL_INT_T_SFMT.

@see CWAL_INT_T_PFMT
@see CWAL_INT_T_SFMT
*/


/** @def CWAL_INT_T_MIN

CWAL_INT_T_MIN is the minimum value of the data type cwal_int_t.

@see CWAL_INT_T_MAX
*/

/** @def CWAL_INT_T_MAX

CWAL_INT_T_MAX is the maximum value of the data type cwal_int_t.

@see CWAL_INT_T_MAX
*/



/** typedef some_unsigned_int_type_which_is_CWAL_SIZE_T_BITS_long cwal_size_t

cwal_size_t is a configurable unsigned integer type specifying the
ranges used by this library. Its exact type depends on the value of
CWAL_SIZE_T_BITS: it will be uintXX_t, where XX is the value of
CWAL_SIZE_T_BITS (16, 32, or 64).

We use a fixed-size numeric type, instead of relying on a standard
type with an unspecified size (e.g. size_t) to help avoid nasty
surprises when porting to machines with different size_t
sizes.

For cwal's intended purposes uint16_t is "almost certainly" fine, but
those who are concerned about 64kb limitations on certain contexts
might want to set this to uint32_t.

*/

/** typedef some_unsigned_int_type cwal_midsize_t

A cwal_size_t counterpart which is intended to be capped at 32 bits
and used in contexts for which 64 bits is simply a massive waste (e.g.
arrays, strings, and hashtables).
*/

/** @typedef some_signed_integer cwal_int_t

    This is the type of integer value used by the library for its
    "script-visible" integers.
*/

/** @typedef some_unsigned_integer cwal_refcount_t

    This is the type of integer value used by the library to keep
    track of both reference counts and their embedded flags (which
    reduce the useful range of the reference count).
*/

/** @def CWAL_REFCOUNT_T_BITS

    @internal

    MUST be equal to (sizeof(cwal_refcount_t) * 8), but must be a
    constant usable by the preprocessor.
*/


/* Set up CWAL_SIZE_T... */
#if CWAL_SIZE_T_BITS == 16
#  define CWAL_SIZE_T_PFMT PRIu16
#  define CWAL_SIZE_T_PFMTx PRIx16
#  define CWAL_SIZE_T_PFMTX PRIX16
#  define CWAL_SIZE_T_PFMTo PRIo16
#  define CWAL_SIZE_T_SFMT SCNu16
#  define CWAL_SIZE_T_SFMTX SCNx16
#  define CWAL_SIZE_T_MAX 65535U
    typedef uint16_t cwal_size_t;
    typedef uint32_t cwal_refcount_t /* yes, 32, because we store flags in cwal_value refcounts */;
#  define CWAL_REFCOUNT_T_BITS 32
#elif CWAL_SIZE_T_BITS == 32
#  define CWAL_SIZE_T_PFMT PRIu32
#  define CWAL_SIZE_T_PFMTx PRIx32
#  define CWAL_SIZE_T_PFMTX PRIX32
#  define CWAL_SIZE_T_PFMTo PRIo32
#  define CWAL_SIZE_T_SFMT SCNu32
#  define CWAL_SIZE_T_SFMTX SCNx32
#  define CWAL_SIZE_T_MAX 4294967295U
    typedef uint32_t cwal_size_t;
    typedef uint32_t cwal_refcount_t;
#  define CWAL_REFCOUNT_T_BITS 32
#elif CWAL_SIZE_T_BITS == 64
#  define CWAL_SIZE_T_PFMT PRIu64
#  define CWAL_SIZE_T_PFMTx PRIx64
#  define CWAL_SIZE_T_PFMTX PRIX64
#  define CWAL_SIZE_T_PFMTo PRIo64
#  define CWAL_SIZE_T_SFMT SCNu64
#  define CWAL_SIZE_T_SFMTX SCNx64
#  define CWAL_SIZE_T_MAX 18446744073709551615U
    typedef uint64_t cwal_size_t;
    typedef uint64_t cwal_refcount_t /*32 bits "should be" fine, but using
                                       64 doesn't (because of padding) actually change
                                       the sizeof of cwal_value */;
#  define CWAL_REFCOUNT_T_BITS 64
#else
#  error "CWAL_SIZE_T_BITS must be one of: 16, 32, 64"
#endif

/* Set up CWAL_MIDSIZE_... */
#if CWAL_SIZE_T_BITS == 16
    typedef uint16_t cwal_midsize_t;
#  define CWAL_MIDSIZE_T_PFMT CWAL_SIZE_T_PFMT
#  define CWAL_MIDSIZE_T_PFMTx CWAL_SIZE_T_PFMTx
#  define CWAL_MIDSIZE_T_PFMTX CWAL_SIZE_T_PFMTX
#  define CWAL_MIDSIZE_T_PFMTo CWAL_SIZE_T_PFMTo
#  define CWAL_MIDSIZE_T_SFMT CWAL_SIZE_T_SFMT
#  define CWAL_MIDSIZE_T_SFMTX CWAL_SIZE_T_SFMTX
#  define CWAL_MIDSIZE_T_MAX CWAL_SIZE_T_MAX
#else
    typedef uint32_t cwal_midsize_t;
#  define CWAL_MIDSIZE_T_PFMT PRIu32
#  define CWAL_MIDSIZE_T_PFMTx PRIx32
#  define CWAL_MIDSIZE_T_PFMTX PRIX32
#  define CWAL_MIDSIZE_T_PFMTo PRIo32
#  define CWAL_MIDSIZE_T_SFMT SCNu32
#  define CWAL_MIDSIZE_T_SFMTX SCNx32
#  define CWAL_MIDSIZE_T_MAX 4294967295U
#endif

/* Set up CWAL_INT_... */
#if CWAL_INT_T_BITS == 16
#  define CWAL_INT_T_PFMT PRIi16
#  define CWAL_INT_T_PFMTx PRIx16
#  define CWAL_INT_T_PFMTX PRIX16
#  define CWAL_INT_T_PFMTo PRIo16
#  define CWAL_INT_T_SFMT SCNi16
#  define CWAL_INT_T_SFMTX SCNX16
#  define CWAL_INT_T_MAX 32767
    typedef int16_t cwal_int_t;
    typedef uint16_t cwal_uint_t;
#elif CWAL_INT_T_BITS == 32
#  define CWAL_INT_T_PFMT PRIi32
#  define CWAL_INT_T_PFMTx PRIx32
#  define CWAL_INT_T_PFMTX PRIX32
#  define CWAL_INT_T_PFMTo PRIo32
#  define CWAL_INT_T_SFMT SCNi32
#  define CWAL_INT_T_SFMTX SCNx32
#  define CWAL_INT_T_MAX 2147483647
    typedef int32_t cwal_int_t;
    typedef uint32_t cwal_uint_t;
#elif CWAL_INT_T_BITS == 64
#  define CWAL_INT_T_PFMT PRIi64
#  define CWAL_INT_T_PFMTx PRIx64
#  define CWAL_INT_T_PFMTX PRIX64
#  define CWAL_INT_T_PFMTo PRIo64
#  define CWAL_INT_T_SFMT SCNi64
#  define CWAL_INT_T_SFMTX SCNx64
#  define CWAL_INT_T_MAX 9223372036854775807
    typedef int64_t cwal_int_t;
    typedef uint64_t cwal_uint_t;
#else
#  error "CWAL_INT_T_BITS must be one of: 16, 32, 64"
#endif
#define CWAL_INT_T_MIN ((-CWAL_INT_T_MAX)-1)
/*
   Reminder: the definition ((-CWAL_INT_T_MAX)-1) was gleaned from the
   clang headers, but AFAIK C does not actually define what happens
   for under/overflow for _signed_ types. Trying to use the literal
   value in 64-bit mode gives me a compile error on gcc ("constant
   value is only signed in C99", or some such).
*/

/** @typedef some_unsigned_int_type cwal_hash_t

   Hash value type used by the library. It must be an unsigned integer
   type.
*/
#if 16 == CWAL_INT_T_BITS
typedef uint32_t cwal_hash_t /* need 32-bit for some hash seeds */;
#elif 32 == CWAL_INT_T_BITS
typedef uint32_t cwal_hash_t;
#elif 64 == CWAL_INT_T_BITS
typedef uint64_t cwal_hash_t;
#endif

/** @typedef double_or_long_double cwal_double_t

    This is the type of double value used by the library.  It is only
    lightly tested with long double, and when using long double the
    memory requirements for such values goes up (of course).

    Note that by default cwal uses C-API defaults for
    numeric precision. To use a custom precision throughout
    the library, one needs to define the macros CWAL_DOUBLE_T_SFMT
    and/or CWAL_DOUBLE_T_PFMT macros to include their desired
    precision, and must build BOTH cwal AND the client using
    these same values. For example:

    @code
    #define CWAL_DOUBLE_T_PFMT ".8Lf"
    #define HAVE_LONG_DOUBLE
    #include "cwal_amalgamation.h"
    @endcode
*/
#if CWAL_DISABLE_FLOATING_POINT
   /* No doubles support: use integers instead so we don't have to
      block out portions of the API.
   */
   typedef cwal_int_t cwal_double_t;
#  define CWAL_DOUBLE_T_SFMT CWAL_INT_T_SFMT
#  define CWAL_DOUBLE_T_PFMT CWAL_INT_T_PFMT
#else
#  if defined(HAVE_LONG_DOUBLE)
     typedef long double cwal_double_t;
#  ifndef CWAL_DOUBLE_T_SFMT
#    define CWAL_DOUBLE_T_SFMT "Lf"
#  endif
#  ifndef CWAL_DOUBLE_T_PFMT
#    define CWAL_DOUBLE_T_PFMT "Lf"
#  endif
#  else
     typedef double cwal_double_t;
#  ifndef CWAL_DOUBLE_T_SFMT
#    define CWAL_DOUBLE_T_SFMT "f"
#endif
#  ifndef CWAL_DOUBLE_T_PFMT
#    define CWAL_DOUBLE_T_PFMT "f"
#endif
#  endif
#endif

#if !defined(CWAL_ENABLE_BUILTIN_LEN1_ASCII_STRINGS)
/**
   If true when cwal is compiled (as opposed to later on, when
   including this header via client code), cwal includes all 128
   length-1 ASCII strings in its list of static builtin values. This
   list includes a length-1 cwal_string instance (builtin/constant)
   for all ASCII values (0 to 127, inclusive). This has a static
   memory cost of ~7.5k on a 64-bit build but has the potential to
   save scads of allocations, especially if client code consciously
   takes advantage of it (mine does!). Here's an out-take from the
   cwal metrics dump after running the s2 unit test suit (20181128):

   @code
   Length-1 ASCII string optimization savings...
      strings:   2681 allocation(s), 150136 bytes.
      x-strings: 1 allocation(s), 56 bytes.
      z-strings: 1 allocation(s), 56 bytes.
   Total savings: 2683, allocation(s), 150248 bytes.
   @endcode
*/
#  define CWAL_ENABLE_BUILTIN_LEN1_ASCII_STRINGS 1
#endif

#if !defined(CWAL_ENABLE_TRACE)
/**
   Setting CWAL_ENABLE_TRACE to a true value enables
   cwal-internal tracing. Profiling shows it to be very
   expensive, and its level of detail is too great for most
   users to be able to do anything with, so it is recommended
   that it be left off unless needed.
 */
#  define CWAL_ENABLE_TRACE 0
#endif


#if !defined(CWAL_OBASE_ISA_HASH)
/** @def CWAL_OBASE_ISA_HASH

  If CWAL_OBASE_ISA_HASH is true then cwal_obase will use a hashtable,
  instead of a sorted list, for its property management. That works
  the same as the "legacy" mode (sorted doubly-linked property lists)
  except that (1) it's computationally faster, (2) requires more
  memory, and (3) has type-strict property keys. e.g. in legacy mode
  the property keys "1" (string) and 1 (integer) are equivalent, but
  they are distinct keys in hashtable mode.

  Because this flag changes the sizeof() of the cwal container types,
  it is critical that all compilation units (including loadable
  modules and any downstream client code) use the same value! The
  easiest(?) way to ensure that that happens when using the cwal
  amalgamation build (the preferred approach) from a client tree is to
  define `HAVE_CONFIG_H` for the whole build, which will cause this file
  to `#include "config.h"`, where this macro can be set to a default
  value.

  As of 2021-08-01, this flag defaults to true. It can be set to false
  to save a small amount of client-side memory at the cost of object
  property lookup speed.
*/
#  define CWAL_OBASE_ISA_HASH 1
#endif

#endif
/* WANDERINGHORSE_NET_CWAL_CONFIG_H_INCLUDED */
