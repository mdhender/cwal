/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=4 et sw=2 tw=80: */
#include <assert.h>
#include <stdlib.h> /* malloc(), free(), qsort() */
#include <string.h>
#include <errno.h>
#include <stdarg.h>
#include "wh/cwal/cwal.h"
#include "wh/cwal/cwal_printf.h"
#include "cwal_internal.h"

/**
   Tells the internals whether to keep Object properties sorted or
   not. Sorting speeds up searches. It is/should be enabled as of
   20170320.

   202107: this has no effect when CWAL_OBASE_ISA_HASH is true.
*/
#define CWAL_KVP_TRY_SORTING 1

#if 1
#include <stdio.h>
#define MARKER(pfexp)                                               \
  do{ printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__);   \
    printf pfexp;                                                   \
  } while(0)
#else
#define MARKER(exp) if(0) printf
#endif

#if defined(__cplusplus)
extern "C" {
#endif

/** Convenience typedef.
    typedef enum cwal_e_options cwal_e_options; */
    

/*
  Public-API constant/shared objects.
*/
const cwal_array cwal_array_empty = cwal_array_empty_m;
const cwal_buffer cwal_buffer_empty = cwal_buffer_empty_m;
const cwal_callback_args cwal_callback_args_empty = cwal_callback_args_empty_m;
const cwal_callback_hook cwal_callback_hook_empty = cwal_callback_hook_empty_m;
const cwal_engine cwal_engine_empty = cwal_engine_empty_m;
const cwal_engine_tracer cwal_engine_tracer_empty = cwal_engine_tracer_empty_m;
const cwal_engine_vtab cwal_engine_vtab_empty = cwal_engine_vtab_empty_m;
const cwal_error cwal_error_empty = cwal_error_empty_m;
const cwal_exception cwal_exception_empty = cwal_exception_empty_m;
const cwal_exception_info cwal_exception_info_empty = cwal_exception_info_empty_m;
const cwal_function cwal_function_empty = cwal_function_empty_m;
const cwal_hash cwal_hash_empty = cwal_hash_empty_m;
const cwal_kvp cwal_kvp_empty = cwal_kvp_empty_m;
const cwal_list cwal_list_empty = cwal_list_empty_m;
const cwal_native cwal_native_empty = cwal_native_empty_m;
const cwal_object cwal_object_empty = cwal_object_empty_m;
const cwal_output_buffer_state cwal_output_buffer_state_empty = {NULL/*e*/, NULL/*b*/};
const cwal_ptr_table cwal_ptr_table_empty = cwal_ptr_table_empty_m;
const cwal_recycler cwal_recycler_empty = cwal_recycler_empty_m;
const cwal_scope cwal_scope_empty = cwal_scope_empty_m;
const cwal_state cwal_state_empty = cwal_state_empty_m;
const cwal_trace_state cwal_trace_state_empty = cwal_trace_state_empty_m;
const cwal_value_vtab cwal_value_vtab_empty = cwal_value_vtab_empty_m;
const cwal_memchunk_config cwal_memchunk_config_empty = cwal_memchunk_config_empty_m;
static const cwal_memchunk_overlay cwal_memchunk_overlay_empty = {0,0};
const cwal_memcap_config cwal_memcap_config_empty = cwal_memcap_config_empty_m;
const cwal_buffer_obj cwal_buffer_obj_empty = cwal_buffer_obj_empty_m;
const cwal_weakref cwal_weakref_empty = cwal_weakref_empty_m;
static const cwal_tuple cwal_tuple_empty = cwal_tuple_empty_m;
//static const cwal_propref cwal_propref_empty = cwal_propref_empty_m;

#define E_IS_DEAD(E) ((E)->fatalCode)

/**
   An experiment in padding all allocations up to a pointer size so
   that the recycler (which might (or might not)) can optimize a
   bit. Similar to the string padding, but it's not clear whether we
   will get as much benefit here.

   Initial tests show padding to take up notably more memory (~2k in
   s2's amalgamated unit tests) and not saving any (or trivially few)
   allocations.

   20160206: enabling this gives, in the s2 unit tests, a microscopic
   reduction (<500b) in peak mem (via list and buffer memory) but
   shows promise in chunk recycling hits (roughly 4%
   improvement). This depends entirely on other options, though: if
   value/chunk recycling and string interning are enabled, plus
   cwal_memcap_config::forceAllocSizeTracking is enabled, alloc counts
   are completely unaffected and the byte difference between using
   this and not is negligible.
*/
#define CWAL_ALLOC_DO_PAD 1
#if 0
/* Round N to next ptr size, even if it is on a ptr size boundary. */
/* ==> valgrind errors (uninvestigated) */
#define CWAL_MEMSZ_PAD(N)                           \
  ((CWAL_ALLOC_DO_PAD)                              \
   ? ((N) + sizeof(void*) - ((N) % sizeof(void*)))  \
   : (N))
#else
/* Round to next ptr size if N is not on a ptr size boundary. */
#define CWAL_MEMSZ_PAD(N)                               \
  ((CWAL_ALLOC_DO_PAD)                                  \
   ? (((N)%sizeof(void*))                               \
      ? ((N) + sizeof(void*) - ((N) % sizeof(void*)))   \
      : (N))                                            \
   : (N))
#endif

/**
   CWAL_RCFLAGS_BITS = Number of bits to steal from
   cwal_value::refcount for use in per-value-instance flags.

   i could possibly be convinced to go up to 8 bits (16M on 32-bit,
   noting that cwal_config.h uses 32-bit int for cwal_refcount_t on
   16-bit builds). If we go any higher than 10 bits (~4M refcount max
   on 32-bit) it would make sense to switch to 64-bit
   cwal_value::refcount on 32-bit platforms, in which case we can
   comfortably use as many of these flags as we need (at the cost of
   bit-flipping performance on 32-bit platforms).
*/
#define CWAL_RCFLAGS_BITS 8
/** CWAL_RCFLAGS_BITS_MASK = lower-byte mask of CWAL_RCFLAGS_BITS bits. */
#define CWAL_RCFLAGS_BITS_MASK 0xFF

/**
   CWAL_RCFLAG_MAXRC is a mask of bits of cwal_value::refcount
   which are usable for reference counting. We actually use the bottom
   bits for flags, not the top, just to help provoke any internal
   misuse faster. This mask is used to check for overflow when
   incrementing the refcount.
*/
#define CWAL_RCFLAG_MAXRC (((cwal_refcount_t)-1)>>CWAL_RCFLAGS_BITS)

/**
   CWAL_REFCOUNT(V) requires V to be a non-null
   (cwal_value*). Evaluates to the (V)->rcflags normalized as a
   refcount counter, stripped of the mask bits.

   Warning: the compiler likes to complain about expressions with no
   effect, and it's easy to use this macro in such a context (when
   combined with other macros from its family). It's also easy to
   reformulate such uses so that they don't cause that.
*/
#define CWAL_REFCOUNT(V) ((V)->rcflags >> CWAL_RCFLAGS_BITS)

/**
   CWAL_REFCOUNT_SHIFTED(V) requires V to be a non-null
   (cwal_value*). Evaluates to the (V)->rcflags, shifted and/or masked
   into position so that it can be masked with flags.

   e.g. if the reference count is currently 1, then this will be (1
   <<CWAL_RCFLAGS_BITS).
*/
#define CWAL_REFCOUNT_SHIFTED(V) ((V)->rcflags & ~CWAL_RCFLAGS_BITS_MASK)

/**
   CWAL_RCFLAGS(V) requires V to be a non-null (cwal_value*). Evaluates to the
   flag bits of (V)->rcflags, stripped of the refcount part.
*/
#define CWAL_RCFLAGS(V) ((V)->rcflags & CWAL_RCFLAGS_BITS_MASK)

/** Adjusts (cwal_value*) V's refcount by N, retaining flags stashed there. Evals
    to the new refcount, INCLUDING the flags bits. */
#define CWAL_RCADJ(V,N) ((V)->rcflags = CWAL_RCFLAGS(V) | ((CWAL_REFCOUNT(V)+(N)) << CWAL_RCFLAGS_BITS))

/** Decrements (cwal_value*) V's refcount by 1, retaining flags stashed there. Evals to
    the new refcount value, sans flags. */
#define CWAL_RCDECR(V) (CWAL_RCADJ(V,-1), CWAL_REFCOUNT(V))

/** Increments (cwal_value*) V's refcount by 1, retaining flags stashed there. Evals to
    the new refcount value, sans flags. */
#define CWAL_RCINCR(V) (CWAL_RCADJ(V,1), CWAL_REFCOUNT(V))

/*#define CWAL_RCFLAGS_SET(V,F) ((V)->rcflags = CWAL_REFCOUNT_SHIFTED(V) | (CWAL_RCFLAGS_BITS_MASK & (F)))*/
#define CWAL_RCFLAG_ON(V,F) ((V)->rcflags = CWAL_REFCOUNT_SHIFTED(V) | (CWAL_RCFLAGS_BITS_MASK & (CWAL_RCFLAGS(V) | (F))))

/*Disable the given flag in the given (cwal_value*). */
#define CWAL_RCFLAG_OFF(V,F) ((V)->rcflags = CWAL_REFCOUNT_SHIFTED(V) | (CWAL_RCFLAGS_BITS_MASK & (CWAL_RCFLAGS(V) & ~(F))))

/*True if the given flag is on in the given (cwal_value*), else false. */
#define CWAL_RCFLAG_HAS(V,F) ((CWAL_RCFLAGS(V) & (F)) ? 1 : 0)

/*True if the (cwal_value*) V is not NULL and has any rc-flags
  set which indicate that it is in the process of being cleaned
  up, e.g. during its own destruction or its scope's cleanup. */
#define CWAL_V_IS_IN_CLEANUP(V)                                     \
  ((V) && (CWAL_RCFLAG_HAS((V),CWAL_RCF_IS_DESTRUCTING)             \
           || CWAL_RCFLAG_HAS((V), CWAL_RCF_IS_GC_QUEUED)           \
           /* || CWAL_RCFLAG_HAS((V), CWAL_RCF_IS_RECYCLED)) */))

#define CWAL_V_IS_RESCOPING(V) \
  ((V) && CWAL_RCFLAG_HAS((V),CWAL_RCF_IS_RESCOPING))

/**
   Evaluates to true if the non-NULL (cwal_value*) V is suitable for storage
   in cwal_scope::mine::headObj (else it goes in headPod, which has finalization
   repercussions).
*/
#if 0
#define CWAL_V_GOES_IN_HEADOBJ(V) (CWAL_VOBASE(V)                       \
                                   || CWAL_TYPE_TUPLE==(V)->vtab->typeID \
                                   || CWAL_TYPE_UNIQUE==(V)->vtab->typeID)
#else
#  define CWAL_V_GOES_IN_HEADOBJ(V) (!!CWAL_VOBASE(V))
#endif

/**
   Flags for use with CWAL_REFCOUNT() and friends. Reminder: every bit
   we steal halves the maximum refcount value!

   See also: CWAL_RCFLAGS_BITS
*/
  enum {
  CWAL_RCF_NONE = 0x0,
  /**
     Set on values which are being destroyed, so that finalization can
     DTRT when encounting it multiple times along the way (cycles).
  */
  CWAL_RCF_IS_DESTRUCTING = 0x1,
  /**
     Sanity-checking flag which tells us that a given Value is in the
     delayed-gc queue.
  */
  CWAL_RCF_IS_GC_QUEUED = 0x2,
  /**
     Sanity-checking flag which tells us that a given Value is in a
     recycling bin.
  */
  CWAL_RCF_IS_RECYCLED = 0x4,
  /**
     Flag set only when a value is in the process of rescoping, so that
     cyclic rescoping can break cycles.
  */
  CWAL_RCF_IS_RESCOPING = 0x8,
  /**
     Flags an object that its properties are being iterated over or are
     otherwise locked from being modified.
  */
  CWAL_RCF_IS_VISITING = 0x10,
  /**
     Flags an object that one of the following parts is being iterated
     over or is otherwise locked from being modified: array list, tuple
     list, hashtable (also a list).
  */
  CWAL_RCF_IS_VISITING_LIST = 0x20,

  /**
     A variant of the visiting-related flags intended to be able to
     restrict recursion by catching cycles. Specifically added to
     re-enable cycle detection in the JSON bits. This flag is
     specifically not intended to be used recursively (it's intended to
     catch recursion) and it explicitly is not specific to iteration
     over either object-level properties or list entries: it's intended
     to be used for both. In the contexts it's used for (JSON), those
     two uses are never combined in the same operation, so there is no
     semantic ambiguity.
  */
  CWAL_RCF_IS_VISITING_ACYCLIC = 0x40,

  /**
     This flag tells the engine whether or not a given value is
     "vacuum-proof" (immune to cwal_engine_vacuum()). This has to be in
     cwal_value::rcflags instead of cwal_obase::flags so that
     CWAL_TYPE_UNIQUE and CWAL_TYPE_TUPLE (and similar
     not-full-fledged-container types) can be made vacuum-proof.
  */
  CWAL_RCF_IS_VACUUM_PROOF = 0x80
  /* CWAL_RCF_IS_LOCKED = 0x100 - we'll need this if we want to lock
     tuples the same way as arrays and hashtables, but we can also
     justify not giving tuples full-fledged superpowers. */
  };

#define cwal_string_empty_m {0U/*length*/}
static const cwal_string cwal_string_empty = cwal_string_empty_m;

#if 0
const cwal_var cwal_var_empty = cwal_var_empty_m;
#endif
const cwal_engine_tracer cwal_engine_tracer_FILE = {
cwal_engine_tracer_f_FILE,
cwal_engine_tracer_close_FILE,
0
};
const cwal_allocator cwal_allocator_empty =
  cwal_allocator_empty_m;
const cwal_allocator cwal_allocator_std = {
cwal_realloc_f_std,
cwal_state_empty_m
};
const cwal_outputer cwal_outputer_empty =
  cwal_outputer_empty_m;
const cwal_outputer cwal_outputer_FILE = {
cwal_output_f_FILE,
cwal_output_flush_f_FILE,
cwal_state_empty_m
};
#if 0
const cwal_outputer cwal_outputer_buffer = {
cwal_output_f_buffer,
NULL,
cwal_state_empty_m
};
#endif

const cwal_engine_vtab cwal_engine_vtab_basic = {
{ /*allocator*/
cwal_realloc_f_std,
cwal_state_empty_m
},
{/* outputer */
cwal_output_f_FILE,
cwal_output_flush_f_FILE,
{/*state (cwal_state) */
NULL/*data*/,
NULL/*typeID*/,
cwal_finalizer_f_fclose/*finalize*/
}
},
cwal_engine_tracer_empty_m,
cwal_state_empty_m/*state*/,
{/*hook*/
NULL/*on_init()*/,
NULL/*init_state*/,
NULL/*scope_push()*/,
NULL/*scope_pop()*/,
NULL/*scope_state*/
},
{/*interning*/
cwal_cstr_internable_predicate_f_default/*is_internable()*/,
NULL/*state*/
},
cwal_memcap_config_empty_m
};

/**
   CwalConsts holds some library-level constants and default values
   which have no better home.
*/
static const struct {
  /** The minimum/default hash size used by cwal_ptr_table. */
  const uint16_t MinimumHashSize;
  /** The minimum step/span used by cwal_ptr_table. */
  const uint16_t MinimumStep;
  /** Default length of some arrays on the first memory reservation
      op.
  */
  const uint16_t InitialArrayLength;
  /**
     Used as the "allocStamp" value for values which the library
     allocates, in order to differentiate them from those allocated
     via stack or being embedded in another object. Should point to
     some library-internal static/constant pointer (any one will
     do).
  */
  void const * const AllocStamp;
  /**
     Largest allowable size for cwal_size_t values in certain
     contexts (e.g. cwal_value::refcount).  If it gets this high
     then something is probably very wrong or the client is trying
     too hard to push the boundaries.
  */
  const cwal_size_t MaxSizeTCounter;
  /**
     If true then newly-create cwal_string instances will be
     auto-interned. All strings with the same bytes will be shared
     across a single cwal_string instance.
  */
  char AutoInternStrings;
  /**
     Property name used to store the message part of an Exception
     value.
  */
  char const * ExceptionMessageKey;

  /**
     Property name used to store the code part of an Exception
     value.
  */
  char const * ExceptionCodeKey;

  /**
     If >0, this sets the maximum size for interning strings. Larger
     strings will not be interned.
  */
  cwal_size_t MaxInternedStringSize;

  /**
     The maximum length of string to recycle. Should be relatively
     small unless we implement a close-fit strategy for
     recycling. Currently we only recycle strings for use with
     "close" size matches (within 1 increment of
     CwalConsts.StringPadSize).
  */
  cwal_size_t MaxRecycledStringLen;

  /**
     If StringPadSize is not 0...

     When strings are allocated, their sizes are padded to be evenly
     divisible by this many bytes. When recycling strings, we use
     this padding to allow the strings to be re-used for any similar
     length which is within 1 of these increments.

     Expected to be an even value, relatively small (under 32, in any
     case).

     Tests have shown 4-8 to be good values, saving anywhere from a
     few percent to 36%(!!!) of the total allocations in the th1ish
     test scripts (compared to a value of 0). Switching from 0 to 4
     gives a notable improvement on the current test scripts. 8 or
     12 don't reduce allocations all that much compared to 4, and
     _normally_ (not always) cost more total memory.
  */
  cwal_size_t StringPadSize;

  /**
     Initial size for cwal_scope::props hashtables.
  */
  cwal_size_t DefaultHashtableSize;
  /**
     "Preferred" load for hashtables before they'll automatically
     resize.
  */
  double PreferredHashLoad;
} CwalConsts = {
13 /* MinimumHashSize. Chosen almost arbitrarily, weighted towards
      small memory footprint per table. */,
sizeof(void*) /* MinimumStep. */,
6 /* InitialArrayLength, starting length of various arrays. */,
&cwal_engine_vtab_empty/*AllocStamp - any internal ptr will do.*/,
#if 16 == CWAL_SIZE_T_BITS
(cwal_size_t)0xCFFF/*MaxSizeTCounter*/,
#else
(cwal_size_t)0xCFFFFFFF/*MaxSizeTCounter*/,
#endif
0/*AutoInternStrings*/,
"message"/*ExceptionMessageKey*/,
"code"/*ExceptionCodeKey*/,
32/*MaxInternedStringSize*/,
#if 0
32/*MaxRecycledStringLen*/,
#elif 16 == CWAL_SIZE_T_BITS
32/*MaxRecycledStringLen*/,
#elif 1
64/*MaxRecycledStringLen*/,
#elif 32==CWAL_SIZE_T_BITS || 16==CWAL_SIZE_T_BITS
2*CWAL_SIZE_T_BITS/*MaxRecycledStringLen*/,
/* In basic tests using s2, 64 gives overall better results (tiny
   bit of peak, marginal number of mallocs saved), than 32 or 48.
*/
#else
CWAL_SIZE_T_BITS/*MaxRecycledStringLen*/,
#endif
8/*StringPadSize*/,
11/*DefaultHashtableSize*/,
0.75/*PreferredHashLoad*/
};

/**
   CWAL_BUILTIN_INT_FIRST is the lowest-numbered built-in integer
   value, and it must have a value of 0 or
   lower. CWAL_BUILTIN_INT_LAST is the highest, and it must have a
   value of 0 or higher. CWAL_BUILTIN_INT_COUNT is how many of those
   values there are.

   The library currently won't build if CWAL_BUILTIN_INT_COUNT is 0,
   but both FIRST and LAST may be 0, in which case the COUNT will be 1
   (the number 0 will be built in).

   The real limit to how many values we "could" build in is the
   sizeof(CWAL_BUILTIN_VALS): we don't want it to be 64k+. 100
   built-in integers take up approximately 4k of space on 64-bit.

   Historical tests showed the single biggest gains to be had (in
   terms of malloc saving) when then inclusive range [-1,1] is built
   in. A range of [-1,10] provides marginally higher results. Above
   that tends to make little difference, at least in generic test
   scripts. The values lower than -1 are not used nearly as often, and
   thus don't benefit as much from building them in.
*/
#define CWAL_BUILTIN_INT_FIRST (-10)
#define CWAL_BUILTIN_INT_LAST (20)
#define CWAL_BUILTIN_INT_COUNT (CWAL_BUILTIN_INT_LAST + -(CWAL_BUILTIN_INT_FIRST) + 1/*zero*/)

char const * cwal_version_string(cwal_size_t * length){
  if(length) *length = (cwal_size_t)(sizeof(CWAL_VERSION_STRING)
                                     -sizeof(CWAL_VERSION_STRING[0]
                                             /*NUL byte*/));
  return CWAL_VERSION_STRING;
}

char const * cwal_cppflags(cwal_size_t * length){
  if(length) *length = (cwal_size_t)(sizeof(CWAL_CPPFLAGS)
                                     -sizeof(CWAL_CPPFLAGS[0]
                                             /*NUL byte*/));
  return CWAL_CPPFLAGS;
}

char const * cwal_cflags(cwal_size_t * length){
  if(length) *length = (cwal_size_t)(sizeof(CWAL_CFLAGS)
                                     -sizeof(CWAL_CFLAGS[0]
                                             /*NUL byte*/));
  return CWAL_CFLAGS;
}

char const * cwal_cxxflags(cwal_size_t * length){
  if(length) *length = (cwal_size_t)(sizeof(CWAL_CXXFLAGS)
                                     -sizeof(CWAL_CXXFLAGS[0]
                                             /*NUL byte*/));
  return CWAL_CXXFLAGS;
}

/** @internal

   Emits a message to stderr and calls abort(). This is only intended
   for use in parts of the library where "cannot happen" situations
   somehow happen. It uses abort() instead of exit so we can catch it
   in gdb and do a backtrace. i think we can, anyway. As already said,
   this type of failure "cannot happen."
*/
static void cwal__fatalv( char const * file, int line,
                   char const * func,
                   int code, char const * fmt, va_list vargs ){
  fprintf(stderr, "%s():%s:%d FATAL ERROR: code=%d (%s)\n",
          func, file, line, code, cwal_rc_cstr(code));
  if(fmt){
    vfprintf(stderr, fmt, vargs);
    fwrite("\n", 1, 1, stderr);
  }
  abort();
}

static void cwal__fatal2( char const * file, int line,
                          char const * func,
                          int code, char const * fmt, ... ){
  va_list args;
  va_start(args,fmt);
  cwal__fatalv(file, line, func, code, fmt, args);
  va_end(args);
}
/** @internal Convenience form of cwal__fatal2(): pass it
    (code, printfFormatString ...). */
#define cwal__fatal(code,...)/* ==> code, fmt[, args] */    \
  cwal__fatal2(__FILE__,__LINE__,__func__,code,__VA_ARGS__)


/**
   Internal impl for cwal_value_unref(). Neither e nor v may be NULL
   (may trigger an assert()).  This is a no-op if v is a builtin.
*/
static int cwal_value_unref2(cwal_engine * e, cwal_value *v );

/**
   Was intended to be an internal impl for cwal_value_ref(), but it
   didn't quite work out that way.
*/
static int cwal_value_ref2( cwal_engine *e, cwal_value * cv );

/**
   Does nothing. Intended as a cwal_value_vtab::cleanup handler
   for types which need not cleanup.

   TODO: re-add the zeroing of such values (lost in porting from
   cson).
*/
static void cwal_value_cleanup_noop( cwal_engine * e, void * self );
/**
   Requires that self is-a CWAL_TYPE_INTEGER cwal_value. This function
   zeros its numeric value but goes not free() self.
*/
static void cwal_value_cleanup_integer( cwal_engine * e, void * self );
/**
   Requires that self is-a CWAL_TYPE_DOUBLE cwal_value. This function
   zeros its numeric value but goes not free() self.
*/
static void cwal_value_cleanup_double( cwal_engine * e, void * self );
/**
   Requires that self is-a cwal_array. This function destroys its
   contents but goes not free() self.
*/
static void cwal_value_cleanup_array( cwal_engine * e, void * self );
/**
   Requires that self is-a cwal_object. This function destroys its
   contents but goes not free() self.
*/
static void cwal_value_cleanup_object( cwal_engine * e, void * self );
/**
   Requires that self is-a cwal_native. This function destroys its
   contents but goes not free() self.
*/
static void cwal_value_cleanup_native( cwal_engine * e, void * self );
/**
   Requires that self is-a cwal_string. This function removes the
   string from e's intering table.
*/
static void cwal_value_cleanup_string( cwal_engine * e, void * self );

/**
   Requires that self is-a cwal_buffer. This function calls
   cwal_buffer_reserve() to clear the memory owned by the buffer.
*/
static void cwal_value_cleanup_buffer( cwal_engine * e, void * self );
/**
   Requires that self is-a cwal_function. This function destroys its
   contents but goes not free() self.
*/
static void cwal_value_cleanup_function( cwal_engine * e, void * self );
/**
   Requires that self is-a cwal_exception. This function destroys its
   contents but goes not free() self.
*/
static void cwal_value_cleanup_exception( cwal_engine * e, void * self );

/**
   Requires that self is-a cwal_hash. This function destroys its
   contents but goes not free() self.
*/
static void cwal_value_cleanup_hash( cwal_engine * e, void * self );

/**
   Requires that self is-a cwal_unique. This function destroys its
   contents but goes not free() self.
*/
static void cwal_value_cleanup_unique( cwal_engine * e, void * self );

/**
   Requires that self is-a cwal_tuple. This function destroys its
   contents but goes not free() self.
*/
static void cwal_value_cleanup_tuple( cwal_engine * e, void * self );

/**
   Requires that self is-a cwal_propref. This function unrefs its
   contents but goes not free() self.
*/
static void cwal_value_cleanup_propref( cwal_engine * e, void * self );

/**
   Fetches v's string value as a non-const string.

   cwal_strings are supposed to be immutable, but this form provides
   access to the immutable bits, which are v->length bytes long. A
   length-0 string is returned as NULL from here, as opposed to
   "". (This is a side-effect of the string allocation mechanism.)
   Returns NULL if !v or if v is the internal empty-string singleton.
*/
static char * cwal_string_str_rw(cwal_string *v);

/*
  Hash/compare routines for cwal_value_vtab.
*/
static cwal_hash_t cwal_value_hash_null_undef( cwal_value const * v );
static cwal_hash_t cwal_value_hash_bool( cwal_value const * v );
static cwal_hash_t cwal_value_hash_int( cwal_value const * v );
static cwal_hash_t cwal_value_hash_double( cwal_value const * v );
static cwal_hash_t cwal_value_hash_string( cwal_value const * v );
/* For all types without a sensible O(1) hash, we just use their
   pointers as their hash values. */
static cwal_hash_t cwal_value_hash_ptr( cwal_value const * v );
static cwal_hash_t cwal_value_hash_tuple( cwal_value const * v );
static int cwal_value_cmp_bool( cwal_value const * lhs, cwal_value const * rhs );
static int cwal_value_cmp_int( cwal_value const * lhs, cwal_value const * rhs );
static int cwal_value_cmp_double( cwal_value const * lhs, cwal_value const * rhs );
static int cwal_value_cmp_string( cwal_value const * lhs, cwal_value const * rhs );
/* static int cwal_value_cmp_type_only( cwal_value const * lhs, cwal_value const * rhs ); */
/* For all types without a sensible comparison, we just compare their
   pointers and document that these types have undefined and
   unpredictable ordering which may change between sessions. */
static int cwal_value_cmp_ptr_only( cwal_value const * lhs, cwal_value const * rhs );
static int cwal_value_cmp_nullundef( cwal_value const * lhs, cwal_value const * rhs );
static int cwal_value_cmp_func( cwal_value const * lhs, cwal_value const * rhs );
static int cwal_value_cmp_buffer( cwal_value const * lhs, cwal_value const * rhs );
static int cwal_value_cmp_tuple( cwal_value const * lhs, cwal_value const * rhs );

/*
  Rescope-children routines for container(esque) types.
*/
static int cwal_rescope_children_obase( cwal_value * v );
static int cwal_rescope_children_array( cwal_value * v );
static int cwal_rescope_children_function( cwal_value * v );
static int cwal_rescope_children_native( cwal_value * v );
static int cwal_rescope_children_hash( cwal_value * v );
static int cwal_rescope_children_unique( cwal_value * v );
static int cwal_rescope_children_tuple( cwal_value * v );
static int cwal_rescope_children_propref( cwal_value * v );


static const cwal_value_vtab cwal_value_vtab_bool =
  { CWAL_TYPE_BOOL, "bool",
    CWAL_F_NONE,
    cwal_value_cleanup_noop,
    cwal_value_hash_bool,
    cwal_value_cmp_bool,
    NULL/*rescope_children()*/
  };

static const cwal_value_vtab cwal_value_vtab_double =
  { CWAL_TYPE_DOUBLE, "double",
    CWAL_F_NONE,
    cwal_value_cleanup_double,
    cwal_value_hash_double,
    cwal_value_cmp_double,
    NULL/*rescope_children()*/
  };

static const cwal_value_vtab cwal_value_vtab_integer =
  { CWAL_TYPE_INTEGER, "integer",
    CWAL_F_NONE,
    cwal_value_cleanup_integer,
    cwal_value_hash_int,
    cwal_value_cmp_int,
    NULL/*rescope_children()*/
  };

static const cwal_value_vtab cwal_value_vtab_null =
  { CWAL_TYPE_NULL, "null",
    CWAL_F_NONE,
    cwal_value_cleanup_noop,
    cwal_value_hash_null_undef,
    cwal_value_cmp_nullundef,
    NULL/*rescope_children()*/
  };

static const cwal_value_vtab cwal_value_vtab_string =
  { CWAL_TYPE_STRING, "string",
    CWAL_F_NONE,
    cwal_value_cleanup_string,
    cwal_value_hash_string,
    cwal_value_cmp_string,
    NULL/*rescope_children()*/
  };

static const cwal_value_vtab cwal_value_vtab_tuple =
  { CWAL_TYPE_TUPLE, "tuple",
    CWAL_F_NONE,
    cwal_value_cleanup_tuple,
    cwal_value_hash_tuple,
    cwal_value_cmp_tuple,
    cwal_rescope_children_tuple
  };

static const cwal_value_vtab cwal_value_vtab_undef =
  { CWAL_TYPE_UNDEF, "undefined",
    CWAL_F_NONE,
    cwal_value_cleanup_noop,
    cwal_value_hash_null_undef,
    cwal_value_cmp_nullundef,
    NULL/*rescope_children()*/
  };

/**
   Internal constant cwal_values for each type which can have builtin
   constant values. Used for copy-initialization of new Value
   instances.
*/
static const cwal_value cwal_value_integer_empty = { &cwal_value_vtab_integer, NULL, NULL, NULL, 0 };
static const cwal_value cwal_value_double_empty = { &cwal_value_vtab_double, NULL, NULL, NULL, 0 };
static const cwal_value cwal_value_string_empty = { &cwal_value_vtab_string, NULL, NULL, NULL, 0 };
static const cwal_value cwal_value_tuple_empty = { &cwal_value_vtab_tuple, NULL, NULL, NULL, 0 };

/**
   Returns the vtab for the given Value type, or NULL if id does not
   represent a Value type.

   Maintenance reminder: vtabs for types which have any builtin
   constant values must be defined file-static so that the init of the
   builtin constants can work.
 */
static cwal_value_vtab const * cwal__value_vtab( cwal_type_id t ){
  switch(t){
    case CWAL_TYPE_ARRAY: {
      static const cwal_value_vtab vtab =
        { CWAL_TYPE_ARRAY, "array",
          CWAL_F_ISA_OBASE,
          cwal_value_cleanup_array,
          cwal_value_hash_ptr,
          cwal_value_cmp_ptr_only,
          cwal_rescope_children_array
        };
      return &vtab;
    }
    case CWAL_TYPE_BOOL: return &cwal_value_vtab_bool;
    case CWAL_TYPE_BUFFER: {
      static const cwal_value_vtab vtab =
        { CWAL_TYPE_BUFFER, "buffer",
          CWAL_F_ISA_OBASE,
          cwal_value_cleanup_buffer,
          cwal_value_hash_ptr,
          cwal_value_cmp_buffer,
          cwal_rescope_children_obase
        };
      return &vtab;
    }
    case CWAL_TYPE_DOUBLE: return &cwal_value_vtab_double;
    case CWAL_TYPE_EXCEPTION: {
      static const cwal_value_vtab vtab =
        { CWAL_TYPE_EXCEPTION, "exception",
          CWAL_F_ISA_OBASE,
          cwal_value_cleanup_exception,
          cwal_value_hash_ptr,
          cwal_value_cmp_ptr_only,
          cwal_rescope_children_obase
        };
      return &vtab;
    }
    case CWAL_TYPE_FUNCTION: {
      static const cwal_value_vtab vtab =
        { CWAL_TYPE_FUNCTION, "function",
          CWAL_F_ISA_OBASE,
          cwal_value_cleanup_function,
          cwal_value_hash_ptr,
          cwal_value_cmp_func,
          cwal_rescope_children_function
        };
      return &vtab;
    }
    case CWAL_TYPE_HASH: {
      static const cwal_value_vtab vtab =
        { CWAL_TYPE_HASH, "hash",
          CWAL_F_ISA_OBASE,
          cwal_value_cleanup_hash,
          cwal_value_hash_ptr,
          cwal_value_cmp_ptr_only,
          cwal_rescope_children_hash
        };
      return &vtab;
    }
    case CWAL_TYPE_INTEGER: return &cwal_value_vtab_integer;
    case CWAL_TYPE_NATIVE: {
      static const cwal_value_vtab vtab =
        { CWAL_TYPE_NATIVE, "native",
          CWAL_F_ISA_OBASE,
          cwal_value_cleanup_native,
          cwal_value_hash_ptr,
          cwal_value_cmp_ptr_only,
          cwal_rescope_children_native
        };
      return &vtab;
    }
    case CWAL_TYPE_NULL: return &cwal_value_vtab_null;
    case CWAL_TYPE_OBJECT: {
      static const cwal_value_vtab vtab =
        { CWAL_TYPE_OBJECT, "object",
          CWAL_F_ISA_OBASE,
          cwal_value_cleanup_object,
          cwal_value_hash_ptr,
          cwal_value_cmp_ptr_only,
          cwal_rescope_children_obase
        };
      return &vtab;
    }
    case CWAL_TYPE_PROPREF: {
      static cwal_value_vtab vtab =
        { CWAL_TYPE_PROPREF, "propref",
          CWAL_F_NONE,
          cwal_value_cleanup_propref,
          cwal_value_hash_ptr,
          cwal_value_cmp_ptr_only,
          cwal_rescope_children_propref
        };
      return &vtab;
    }
    case CWAL_TYPE_STRING:
    case CWAL_TYPE_XSTRING:
    case CWAL_TYPE_ZSTRING: return &cwal_value_vtab_string;
    case CWAL_TYPE_TUPLE: return &cwal_value_vtab_tuple;
    case CWAL_TYPE_UNDEF: return &cwal_value_vtab_undef;
    case CWAL_TYPE_UNIQUE: {
      static const cwal_value_vtab vtab = {
      CWAL_TYPE_UNIQUE, "unique",
      CWAL_F_NONE,
      cwal_value_cleanup_unique,
      cwal_value_hash_ptr,
      cwal_value_cmp_ptr_only,
      cwal_rescope_children_unique
      };
      return &vtab;
    }
    default: return NULL;
  }
}

/*
  Internal convenience macros...
*/

/**
   Cast ((cwal_value*)V) to (T*). It assumes the caller knows WTF he
   is doing. Evaluates to 0 if !(V).

   This op relies on the fact that memory (V+1) contains a (T)
   value. i.e. it relies on the allocation mechanism used by
   cwal_value_new() and that V was allocated by that function, is a
   builtin/shared value instance, or is NULL.
*/
#define CWAL_VVPCAST(T,V) ((T*)((V) ? ((T*)((V)+1)) : (T*)NULL))

/**
   CWAL_VALPART(CONCRETE)

   Cast (concrete_value_type*) CONCRETE to a
   (cwal_value*). CWAL_VALPART() relies on the fact that ALL cwal_value
   types which use it are allocated in a single memory chunk with
   (cwal_value,concrete_type), in that order. Do not use this macro
   for types which are not allocated that way.

   AFAIK, it is also legal for CONCRETE to be (cwal_obase*), provided
   that pointer was allocated as part of one of the container types
   (object, array, etc.). This relies on them having their (cwal_obase
   base) member as the first member of the struct.

   e.g. assuming an Object value:

   cwal_obase * base = CWAL_VOBASE(val);
   cwal_object * obj = cwal_value_get_object(val);
   assert( CWAL_VALPART(base) == val );
   assert( base == &obj->base );
*/

#if 1
#define CWAL_VALPART(CONCRETE) \
  ((CONCRETE) ? ((cwal_value *)(((unsigned char *)(CONCRETE)) \
                                -sizeof(cwal_value))) : NULL)
#else
/* i'm not convinced that this one is standards-conformant.
   But it looks nicer.
*/
#define CWAL_VALPART(CONCRETE) ((CONCRETE) ? \
                                (((cwal_value *)(CONCRETE))-1) : NULL)
#endif

/**
   CWAL_INT(V) casts CWAL_TYPE_INTEGER (cwal_value*) V to a
   (cwal_int_t*).
*/
#define CWAL_INT(V) (CWAL_VVPCAST(cwal_int_t,(V)))

/**
   Requires that V be one of the special cwal_values TRUE or FALSE.
   Evaluates to 1 if it is the TRUE value, else false.
*/
#define CWAL_BOOL(V) ((&CWAL_BUILTIN_VALS.vTrue==(V)) ? 1 : 0)

/**
   CWAL_DBL(V) casts CWAL_TYPE_DOUBLE (cwal_value*) V to a (cwal_double_t*).
*/
#define CWAL_DBL(V) CWAL_VVPCAST(cwal_double_t,(V))

/*
  workarounds for false gcc warning:
  
  https://gcc.gnu.org/bugzilla/show_bug.cgi?id=47214

  Summary: gcc's nonnull warning triggers incorrectly
  for cases like somefunc( X ? Y : NULL ) because it cannot
  know that X will never be false in that context.

  The _NONULL variants work like the non-_NONULL variants
  but MUST NOT be passed a NULL value.
*/
#define CWAL_VVPCAST_NONULL(T,V) ((T*)((V)+1))
#define CWAL_DBL_NONULL(V) CWAL_VVPCAST_NONULL(cwal_double_t,(V))
#define CWAL_INT_NONULL(V) (CWAL_VVPCAST_NONULL(cwal_int_t,(V)))

/**
   For cwal_string we store flags in the cwal_string::length member,
   rather than add a flag to them (for memory reasons). We reserve the
   top three bits and encode the length in the remaining ones. So the
   maximum length of a given String value is 2^(CWAL_SIZE_T_BITS-3).
*/
#if 16 == CWAL_SIZE_T_BITS
/* Max string length: 8k */
#  define CWAL_STRLEN_MASK    ((cwal_size_t)0x1FFFU)
#  define CWAL_STR_XMASK      ((cwal_size_t)0x8000U)
#  define CWAL_STR_ZMASK      ((cwal_size_t)0x4000U)
#  define CWAL_STR_ASCII_MASK ((cwal_size_t)0x2000U)
#elif 32 == CWAL_SIZE_T_BITS
/* Max string length: 0.5GB */
#  define CWAL_STRLEN_MASK    ((cwal_size_t)0x1FFFFFFFU)
#  define CWAL_STR_XMASK      ((cwal_size_t)0x80000000U)
#  define CWAL_STR_ZMASK      ((cwal_size_t)0x40000000U)
#  define CWAL_STR_ASCII_MASK ((cwal_size_t)0x20000000U)
#elif 64 == CWAL_SIZE_T_BITS
#if 0
/* Portability: stick with 32-bit lengths. */
#  define CWAL_STRLEN_MASK    ((cwal_size_t)0x1FFFFFFFU)
#  define CWAL_STR_XMASK      ((cwal_size_t)0x80000000U)
#  define CWAL_STR_ZMASK      ((cwal_size_t)0x40000000U)
#  define CWAL_STR_ASCII_MASK ((cwal_size_t)0x20000000U)
#else
/* Max string length: 32-bits so that we can use cwal_midsize_t
   for all string lengths. */
#  define CWAL_STRLEN_MASK    ((cwal_size_t)0x1FFFFFFFU)
#  define CWAL_STR_XMASK      ((cwal_size_t)0x8000000000000000U)
#  define CWAL_STR_ZMASK      ((cwal_size_t)0x4000000000000000U)
#  define CWAL_STR_ASCII_MASK ((cwal_size_t)0x2000000000000000U)
#endif
#endif

/*
  CWAL_STR_ASCII_MASK is a tag for strings which are pure ASCII (all
  bytes are in the inclusive range [0,127]), in which case we can
  speed up many operations which currently always have to read UTF8
  char-by-char. To disable this feature, set CWAL_STR_ASCII_MASK to 0
  and make sure all test code which assert()s that a given string is
  ASCII is disabled.
*/

/* #define CWAL_STRLEN_MASK ((cwal_size_t)~(CWAL_STR_XMASK \
   | CWAL_STR_ZMASK | CWAL_STR_ASCII_MASK)) */


/**
   CWAL_STR(V) casts CWAL_TYPE_STRING (cwal_value*) V to a (cwal_string*).
   If V is NULL or not-a String it evals to 0.
*/
#define CWAL_STR(V) (((V) && (V)->vtab && (CWAL_TYPE_STRING==(V)->vtab->typeID)) ? CWAL_VVPCAST(cwal_string,(V)) : 0)

/**
   Evaluates to true if S (which must be a valid (cwal_string*)) is an
   x-string, else false.
*/
#define CWAL_STR_ISX(S) ((CWAL_STR_XMASK & (S)->length) ? 1 : 0)
/**
   Evaluates to true if S (which must be a valid (cwal_string*)) is a
   z-string, else false.
*/
#define CWAL_STR_ISZ(S) ((CWAL_STR_ZMASK & (S)->length) ? 1 : 0)
/**
   Evaluates to true if S (which must be a valid (cwal_string*)) is a
   either an x-string or z-string, else false.
*/
#define CWAL_STR_ISXZ(S) (CWAL_STR_ISX(S) || CWAL_STR_ISZ(S))

/**
   Evaluates to true if S (which must be a valid (cwal_string*)) has been
   marked as being ASCII.
*/
#define CWAL_STR_ISASCII(S) ((CWAL_STR_ASCII_MASK & (S)->length) ? 1 : 0)

/**
   Evaluates to the absolute value of S->length, in bytes, where S
   must be a non-NULL (cwal_string [const]*). This is required instead
   direct access to S->length because we encode non-size info in the
   length field for X- and Z-strings, plus the is-ASCII flag.
*/
#define CWAL_STRLEN(S) ((cwal_midsize_t)((S)->length & CWAL_STRLEN_MASK))

/** Evaluates to non-0 if the cwal_size_t value LEN is too long for a string length. */
#define CWAL_STRLEN_TOO_LONG(LEN) (((cwal_size_t)(LEN)) & ~CWAL_STRLEN_MASK)

/**
   CWAL_OBJ(V) casts CWAL_TYPE_OBJECT (cwal_value*) V to a (cwal_object*).
*/
#define CWAL_OBJ(V) (((V) && (V)->vtab && (CWAL_TYPE_OBJECT==(V)->vtab->typeID)) ? CWAL_VVPCAST(cwal_object,(V)) : 0)

/**
   CWAL_ARRAY(V) casts CWAL_TYPE_ARRAY (cwal_value*) V to a (cwal_array*).
*/
#define CWAL_ARRAY(V) (((V) && (V)->vtab && (CWAL_TYPE_ARRAY==(V)->vtab->typeID)) ? CWAL_VVPCAST(cwal_array,(V)) : 0)

/**
   CWAL_HASH(V) casts CWAL_TYPE_HASH (cwal_value*) V to a (cwal_hash*).
*/
#define CWAL_HASH(V) (((V) && (V)->vtab && (CWAL_TYPE_HASH==(V)->vtab->typeID)) ? CWAL_VVPCAST(cwal_hash,(V)) : 0)

/**
   CWAL_OBASE(O) casts the OBase-type pointer OB to a (cwal_obase*).
   This relies on OB being an obase-compliant type, with a cwal_obase
   member being the first struct member of OB.
*/
#define CWAL_OBASE(OB) ((cwal_obase*)(OB))

/**
   Evaluates to true if ((cwal_value*) V)->vtab is not 0 and has the
   CWAL_F_ISA_OBASE flag, else false.
*/
#define CWAL_V_IS_OBASE(V) (((V) && (V)->vtab && (CWAL_F_ISA_OBASE & (V)->vtab->flags)) ? 1 : 0)

/**
   If CWAL_V_IS_OBASE(V), evaluates to V's (cwal_obase*) part, else it
   evaluates to 0. For this to work all "object base" types must have
   a cwal_obase member named 'base' as their VERY FIRST structure
   member because we rely on a C's rule/allowance that a struct can be
   cast to a pointer to its first member.
*/
#define CWAL_VOBASE(V) (CWAL_V_IS_OBASE(V) ? CWAL_VVPCAST(cwal_obase,(V)) : 0)

/**
   Casts CWAL_TYPE_NATIVE value V to (cwal_native*).
*/
#define CWAL_V2NATIVE(V) CWAL_VVPCAST(cwal_native,(V))
/**
   CWAL_BUFOBJ(V) casts CWAL_TYPE_BUFFER (cwal_value*) V to a (cwal_buffer_obj*).
*/
#define CWAL_BUFOBJ(V) (((V) && (V)->vtab && (CWAL_TYPE_BUFFER==(V)->vtab->typeID)) ? CWAL_VVPCAST(cwal_buffer_obj,(V)) : 0)

/**
   CWAL_UNIQUE_VALPP(V) casts gets the wrapped (cwal_value**) part of
   CWAL_TYPE_UNIQUE (cwal_value*) V. If V is-not-a Unique, it evals
   to 0.
*/
#define CWAL_UNIQUE_VALPP(V) (((V) && (V)->vtab && (CWAL_TYPE_UNIQUE==(V)->vtab->typeID)) ? (CWAL_VVPCAST(cwal_value*,(V))) : 0)

/**
   CWAL_TUPLE(V) casts (cwal_value*) V to (cwal_tuple*).
   If V is-not-a Tuple, it evals to 0.
*/
#define CWAL_TUPLE(V) (((V) && (V)->vtab && (CWAL_TYPE_TUPLE==(V)->vtab->typeID)) ? (CWAL_VVPCAST(cwal_tuple,(V))) : 0)

/**
   CWAL__V2PROPREF(V) casts (cwal_value*) V to (cwal_propref*).
   If V is-not-a Propref, it evals to 0.
*/
#define CWAL__V2PROPREF(V) (((V) && (V)->vtab && (CWAL_TYPE_PROPREF==(V)->vtab->typeID)) ? (CWAL_VVPCAST(cwal_propref,(V))) : 0)
enum {
CWAL__PROPREF_MASK_WEAK = 0x1000,
CWAL__PROPREF_MASK_RO   = 0x2000,
CWAL__PROPREF_MASK_TYPE = 0x0FFF
};

/**
   If (cwal_value*) V is not NULL and has a scope, this evaluates to
   its (cwal_engine*), else to 0. Note that built-in values have no
   scope and are not specific to a cwal_engine instance.
*/
#define CWAL_VENGINE(V) ((V) ? ((V)->scope ? (V)->scope->e : 0) : 0)

/**
   Evaluates to true if (cwal_value [const] *)V is currently flagged
   as being visited (object-level properties).
*/
#define CWAL_V_IS_VISITING(V) CWAL_RCFLAG_HAS((V),CWAL_RCF_IS_VISITING)
/**
   Evaluates to true if (cwal_value [const] *)V is currently flagged
   as being list-visited (arrays and hashtable entries).
*/
#define CWAL_V_IS_VISITING_LIST(V) CWAL_RCFLAG_HAS((V),CWAL_RCF_IS_VISITING_LIST)

/**
   CWAL_OB_xxx(OBTYPE) all require OBTYPE to be a pointer to a
   cwal_obase-deriving type, typically an array or hashtable.

   Semantic ambiguity here: this flag is currently only used to lock
   array/list parts. If we also want to lock properties, this flag
   becomes ambiguous for list-using types. It would be conceivable to
   have the list part locked while the properties are not, and vice
   versa.
*/
#define CWAL_OB_LOCK(OBTYPE) (OBTYPE)->base.flags |= CWAL_F_LOCKED
#define CWAL_OB_UNLOCK(OBTYPE) (OBTYPE)->base.flags &= ~CWAL_F_LOCKED
#define CWAL_OB_IS_LOCKED(OBTYPE) (CWAL_F_LOCKED & (OBTYPE)->base.flags)

#define METRICS_REQ_INCR(E,TYPE) ++(E)->metrics.requested[TYPE]

#define CWAL_V_IS_VACUUM_SAFE(V)                    \
  (CWAL_RCFLAG_HAS((V), CWAL_RCF_IS_VACUUM_PROOF))

/**
   Must only be used when (cwal_engine::flags &
   CWAL_F_TRACK_MEM_SIZE). This returns an address sizeof(void*)
   bytes before void pointer M, cast to a (cwal_memsize_t*).
*/
#define MEMSZ_PTR_FROM_MEM(M) (cwal_memsize_t*)((unsigned char *)(M) - sizeof(void*))

/**
   Returns a (cwal_value*) to the given index from
   CWAL_BUILTIN_VALS.memInt.
*/
#define CWAL_BUILTIN_INT_VAL(NDX) ((cwal_value*)&CWAL_BUILTIN_VALS.memInt[NDX])

/**
   Some "special" shared cwal_value instances.

   Note that they are not const because they are used as
   shared-allocation objects in non-const contexts. However, the public
   API provides no way of modifying them, and clients who modify values
   directly are subject to The Wrath of Undefined Behaviour.
*/
static struct CWAL_BUILTIN_VALS_ {
  /**
     Gets set to a true value when this struct gets initialized by
     cwal_init_builtin_values(), to ensure that we only initialize
     this once. Pedantic side-note: it's potentially possible that
     two engine instances in different threads, being initialized at
     the same time, try to initialize this data concurrently. That's
     okay, as each initialization will set the data to the exact
     same state and same addresses, so there's no real harm done. We
     intentionally don't update the 'inited' member until the end up
     of the init process, as its harmless if it's inited multiple
     times concurrently but not harmless if we update this flag at
     the start of the process and another thread tries to use this
     data before it's completely initialized by the thread which set
     that flag.
  */
  int inited;
  /**
     Each of the memXXX entries holds the raw block of memory
     intended for (cwal_value + concrete_type). These are
     initialized by cwal_init_builtin_values().
  */
#define sz_int sizeof(cwal_value)+sizeof(cwal_int_t)
  unsigned char memInt[CWAL_BUILTIN_INT_COUNT
                       ? CWAL_BUILTIN_INT_COUNT
                       : 1/*dummy build placeholder*/][sz_int];
  unsigned char memDblM1[sizeof(cwal_value)+sizeof(cwal_double_t)];
  unsigned char memDbl0[sizeof(cwal_value)+sizeof(cwal_double_t)];
  unsigned char memDbl1[sizeof(cwal_value)+sizeof(cwal_double_t)];
  unsigned char memEmptyString[sizeof(cwal_value)+sizeof(cwal_string)+1/*NUL byte*/];
#if CWAL_ENABLE_BUILTIN_LEN1_ASCII_STRINGS
  unsigned char memAsciiPrintable[128/* one entry each for ASCII [0,127] */]
  [(sizeof(cwal_value)+sizeof(cwal_string)+2)
   /* ==> "X\0", where X is an ASCII char 0..127. */];
#else
  unsigned char memAsciiPrintable[1][1/*dummy placeholder*/];
#endif
  unsigned char memTuple0[sizeof(cwal_value)+sizeof(cwal_tuple)];
#undef sz_int
  /**
     Each of the vXXX pointers points to memory held in the
     similarly-named memXXX member.
  */
  cwal_value * vDblM1 /* TODO: eliminate these, as was done for integers. */;
  cwal_value * vDbl0;
  cwal_value * vDbl1;
  cwal_value * vEmptyString;

  /**
     Points to the cwal_string part of this->memEmptyString.
  */
  cwal_string * sEmptyString;
  cwal_value * vTuple0;
  cwal_value vTrue;
  cwal_value vFalse;
  cwal_value vNull;
  cwal_value vUndef;

  /**
     Double values -1.0, 0.0, and 1.0.
  */
  struct {
    cwal_double_t mOne;
    cwal_double_t zero;
    cwal_double_t one;
  } dbls;
    
  /**
     Each of the wref.wXXX entries is a shared cwal_weakref
     instance pointing to a similarly-named vXXX entry.  We do not
     allocate new cwal_weakref instances if they wrap a Value which
     itself is a built-in value. It's hard to imagine a use case for
     someone trying to weak-ref a boolean value, but the generic
     nature of the Value system makes it conceivably possible, so
     here it is...
  */
  struct {
    cwal_weakref wTrue;
    cwal_weakref wFalse;
    cwal_weakref wNull;
    cwal_weakref wUndef;
    cwal_weakref wStrEmpty
    /* Reminder to self: we don't currently have entries here for
       the built-in length-1 strings (memAsciiPrintable). Nor the
       length-0 Tuple, it seems. Oh, well. We removed the integer
       weak refs on 20171202 when that numeric range was made
       built-time configurable. */;
    cwal_weakref wDblM1;
    cwal_weakref wDbl0;
    cwal_weakref wDbl1;
  } wref;

} CWAL_BUILTIN_VALS = {
0/*inited*/,
{/*memInt*/ {0}},
{/*memDblM1*/ 0},
{/*memDbl0*/ 0},
{/*memDbl1*/ 0},
{/*memEmptyString*/ 0},
{/*memAsciiPrintable*/{0}},
{/*memTuple0*/0},
NULL/*vDblM1*/,
NULL/*vDbl0*/,
NULL/*vDbl1*/,
NULL/*vEmptyString*/,
NULL/*sEmptyString*/,
NULL/*vTuple0*/,
{/*vTrue*/ &cwal_value_vtab_bool, NULL, NULL, NULL, 0 },
{/*vFalse*/ &cwal_value_vtab_bool, NULL, NULL, NULL, 0 },
{/*vNull*/ &cwal_value_vtab_null, NULL, NULL, NULL, 0 },
{/*vUndef*/ &cwal_value_vtab_undef, NULL, NULL, NULL, 0 },
#if CWAL_DISABLE_FLOATING_POINT
{/*dbls*/-1,0,1},
#else
{/*dbls*/-1.0,0.0,1.0},
#endif
{/*wref*/
cwal_weakref_empty_m/* wTrue */,
cwal_weakref_empty_m/* wFalse */,
cwal_weakref_empty_m/* wNull */,
cwal_weakref_empty_m/* wUndef */,
cwal_weakref_empty_m/* wStrEmpty */,
cwal_weakref_empty_m/* wDblM1 */,
cwal_weakref_empty_m/* wDbl0 */,
cwal_weakref_empty_m/* wDbl1 */
}
};

static void cwal_init_builtin_values(){
  struct CWAL_BUILTIN_VALS_ * h = &CWAL_BUILTIN_VALS;
  cwal_value * v;

  {/* Set up empty string */
    memset(h->memEmptyString, 0, sizeof(h->memEmptyString))
      /* ensure that the NUL terminator is set */;
    v = h->vEmptyString = (cwal_value*)h->memEmptyString;
    *v = cwal_value_string_empty;
    h->sEmptyString = CWAL_STR(v);
    assert(h->sEmptyString);
    assert(0==CWAL_STRLEN(h->sEmptyString));
    assert(0==*cwal_string_cstr(h->sEmptyString));
  }

#if CWAL_ENABLE_BUILTIN_LEN1_ASCII_STRINGS
  {/* set up length-1 ASCII strings */
    int i = 0;
    cwal_string * s;
    char * cp;
    memset(h->memAsciiPrintable, 0, sizeof(h->memAsciiPrintable));
    for( ; i <= 127; ++i ){
      v = (cwal_value *)(h->memAsciiPrintable[i]);
      *v = cwal_value_string_empty;
      s = CWAL_STR(v);
      *s = cwal_string_empty;
      s->length = CWAL_STR_ASCII_MASK | 1;
      assert(1 == CWAL_STRLEN(s));
      cp = cwal_string_str_rw(s);
      cp[0] = (char)i;
      cp[1] = 0;
      assert(1 == CWAL_STRLEN(s));
      assert(cp == cwal_string_cstr(s));
    }
    v = 0;
  }
#endif

  {
    /* Set up integers [CWAL_BUILTIN_INT_FIRST .. CWAL_BUILTIN_INT_LAST].
       Maintenance reminder: the memcpy() is needed, rather than direct
       copy, to avoid a "type punning" error in certain compilation
       environments. */
    cwal_int_t v = CWAL_BUILTIN_INT_FIRST, ndx = 0;
    cwal_int_t const last = CWAL_BUILTIN_INT_LAST;
    assert(CWAL_BUILTIN_INT_FIRST <= 0);
    assert(CWAL_BUILTIN_INT_LAST >= 0);
    assert(CWAL_BUILTIN_INT_COUNT > 0);
    assert(sizeof(h->memInt)/sizeof(h->memInt[0]) ==
           (CWAL_BUILTIN_INT_COUNT ? CWAL_BUILTIN_INT_COUNT : 1));
    for( ; v <= last; ++v, ++ndx ){
      cwal_value * cv = CWAL_BUILTIN_INT_VAL(ndx);
      *cv = cwal_value_integer_empty;
      memcpy(CWAL_INT_NONULL(cv), &v, sizeof(cwal_int_t));
      assert( v == cwal_value_get_integer(cv) );
    }
  }

  /* Set up doubles (-1, 0, 1) */
#if CWAL_DISABLE_FLOATING_POINT
#define dbl_m1 -1
#define dbl_0 0
#define dbl_1 1
#else
#define dbl_m1 -1.0
#define dbl_0 0.0
#define dbl_1 1.0
#endif
#define NUM(N,V) {                                          \
    const cwal_double_t dv = V;                             \
    h->vDbl##N = v = (cwal_value*)h->memDbl##N;             \
    *v = cwal_value_double_empty;                           \
    memcpy(CWAL_DBL_NONULL(v), &dv, sizeof(cwal_double_t)); \
  } (void)0
  NUM(M1,dbl_m1);
  NUM(0,dbl_0);
  NUM(1,dbl_1);
#undef NUM
#undef dbl_m1
#undef dbl_0
#undef dbl_1

  {
    /** Sets up shared weak refs */
    cwal_weakref * r;
#define REF(N,V) r = &h->wref.N; *r = cwal_weakref_empty;  \
    r->value = V; r->typeID = (V)->vtab->typeID
    REF(wTrue,&h->vTrue);
    REF(wFalse,&h->vFalse);
    REF(wNull,&h->vNull);
    REF(wUndef,&h->vUndef);
    REF(wStrEmpty,h->vEmptyString);
    REF(wDblM1,h->vDblM1);
    REF(wDbl0,h->vDbl0);
    REF(wDbl1,h->vDbl1);
  }

  { /* Empty Tuple... */
    memset(h->memTuple0, 0, sizeof(h->memTuple0));
    v = h->vTuple0 = (cwal_value*)h->memTuple0;
    *v = cwal_value_tuple_empty;
    assert(CWAL_TUPLE(v));
    assert(0==CWAL_TUPLE(h->vTuple0)->n);
  }

  h->inited = 1
    /* We do this at the end, instead of the start, to cover a
       specific threading corner case: If two (or more) threads
       end up triggering this routine concurrently, we let all of
       them modify the memory.  Because they all set it to the
       same values, we really don't care which one finishes
       first. Once any of them have set h->inited=1,
       CWAL_BUILTIN_VALS contains the memory we want. Even if a
       slower thread is still re-initializing it after a faster
       thread has returned, it is overwriting the contents with
       the same values, so writing them while the faster thread is
       (potentially) using them "should" be harmless.

       Note that this race can only potentially happen once during
       the life of the application, during the initialization of
       the first cwal_engine instance(s), and only if they are
       initialized in concurrent threads.
    */;

}

/**
   CWAL_MEM_IS_BUILTIN(V) determines if the (void const *) V is one of
   the special built-in/constant values. It does so by simply checking
   if its address lies within range of the stack-allocated
   special-value holders, so it's about as fast as it can be.
   

   Maintenance reminders:

   - Profiling shows that cwal_value_is_builtin() is by far the
   most-called routine in the library and accounts for about 1% of the
   total calls in my test app. That is the only reason it has become a
   macro. Client-side code never uses (never really has a need for,
   other than basic curiosity) cwal_value_is_builtin().
*/
#define CWAL_MEM_IS_BUILTIN(V)                                      \
  ((((void const *)(V) >= (void const *)&CWAL_BUILTIN_VALS)         \
    && ( (void const *)(V) < (void const *)(&CWAL_BUILTIN_VALS+1))  \
    ) ? 1 : 0)


/**
   Intended for use in assertions to ensure that (cwal_value const *)
   V is either a builtin value or has an owning scope.
*/
#define V_SEEMS_OK(V) ((V)->scope || CWAL_MEM_IS_BUILTIN(V))

/**
   Type used for counting memory chunk sizes.
*/
typedef uint32_t cwal_memsize_t;

    
bool cwal_value_is_builtin( void const * m ){
  return CWAL_MEM_IS_BUILTIN(m);
}

/**
   Static state for the recycling mechanism. Here we only
   store some calculated values which apply to all engines.

   Gets populated by cwal_setup_recycler_indexes().
*/
static struct {
  /**
     A map of cwal_type_id to integer array indexes
     for cwal_engine::recycler[]. It gets populated
     once during engine initialization and holds values
     valid for all engine instances.
  */
  int indexes[CWAL_TYPE_end];
  /**
     The total number of recycling bins calculated by
     cwal_setup_recycler_indexes(). cwal_engine::recycler
     must have at least this many elements.
  */
  cwal_size_t recyclerCount;
} cwalRecyclerInfo = {
{-1,-1,-1,-1, -1,-1,-1,-1,
 -1,-1,-1,-1, -1,-1,-1,-1,
 -1,-1,-1,-1, -1,-1 },
0
};

/**
   Calculates an optimized value recycling bin layout for
   cwal_engine::recycler and stores the result in cwalRecyclerInfo.indexes.
   All non-recyclable types get a -1 put in
   cwalRecyclerInfo.indexes[thatTypeId].  All others get a value
   representing an index into cwal_engine::recycler.  All types with
   the same base Value size are grouped together in the same index.
*/
static void cwal_setup_recycler_indexes(){
  if(cwalRecyclerInfo.recyclerCount) return;
  else{
    static const cwal_type_id tlist[] = {
    /*
      Reminders to self: only set up VALUE TYPES this way because
      we cannot mix Value and non-Value types in one recycler
      because usage of their linked list members
      breaks. CWAL_TYPE_STRING must not be in this list, but
      CWAL_TYPE_XSTRING and ZSTRING must be (both have the same
      size and will be grouped together).
    */
    CWAL_TYPE_INTEGER,
    CWAL_TYPE_DOUBLE,
    CWAL_TYPE_ARRAY,
    CWAL_TYPE_OBJECT,
    CWAL_TYPE_NATIVE,
    CWAL_TYPE_BUFFER,
    CWAL_TYPE_FUNCTION,
    CWAL_TYPE_EXCEPTION,
    CWAL_TYPE_HASH,
    CWAL_TYPE_XSTRING,
    CWAL_TYPE_ZSTRING,
    CWAL_TYPE_UNIQUE,
    CWAL_TYPE_TUPLE,
    CWAL_TYPE_PROPREF,
    CWAL_TYPE_UNDEF /* list sentinel - gets excluded below */
    };
#define ASIZE(A) (sizeof(A)/sizeof(A[0]))
    cwal_size_t xlist[ASIZE(tlist)];
    cwal_size_t zlist[ASIZE(tlist)];
    cwal_size_t i, x, zCount = 0;
    memset(zlist, 0, sizeof(zlist));
#if !defined(NDEBUG)
    /* Make sure the inlined intialization is as expected... */
    for(i = 0; i < CWAL_TYPE_end; ++i){
      assert(-1==cwalRecyclerInfo.indexes[i]);
      /* cwalRecyclerInfo.indexes[i] = -1; */
      /* cwalRecyclerSizes[i] = cwal_type_id_sizeof(i); */
    }
#endif
    /* Collect the sizes of each type... */
    for(i = 0; CWAL_TYPE_UNDEF != tlist[i] ; ++i){
      xlist[i] = cwal_type_id_sizeof(tlist[i]);
      assert(xlist[i]);
      /*MARKER(("xlist[#%d/%s] = %d\n", (int)i,
        cwal_type_id_name(tlist[i]), (int)xlist[i]));*/
    };
    xlist[i] = 0;
    /* Bubblesort the sizes... */
    for(i = 0; i < ASIZE(xlist)-1; ++i){
      for( x = 1; x < ASIZE(xlist)-1; ++x){
        if(xlist[x] < xlist[x-1]){
          cwal_size_t const tmp = xlist[x-1];
          xlist[x-1] = xlist[x];
          xlist[x] = tmp;
        }
      }
    }
#if 0
    for(i = 0; xlist[i] && i < CWAL_TYPE_end; ++i){
      MARKER(("Sorted index %d %d\n",
              (int)i, (int)xlist[i]));
    }
#endif
    /* Remove dupes... */
    for( x = 0; x < ASIZE(xlist)-1; ++x ){
      int gotIt = 0;
      for(i = 0; zlist[i] && i < ASIZE(zlist)-1; ++i){
        if( zlist[i] == xlist[x] ){
          gotIt = 1;
          break;
        }
      }
      if(!gotIt) zlist[zCount++] = xlist[x];
    }
#if 0
    for(i = 0; i < CWAL_TYPE_end && zlist[i]; ++i){
      MARKER(("Index #%d sizeof=%d\n",
              (int)i, (int)zlist[i]));
    }
#endif
    /* Match up sizes to types, to group recycle bins... */
    for(i = 0; zlist[i]; ++i){
      /* MARKER(("#%d = %d\n", (int)i, (int)zlist[i])); */
      for(x = 0; CWAL_TYPE_UNDEF != tlist[x]; ++x){
        cwal_size_t const sz = cwal_type_id_sizeof(tlist[x]);
        if(sz == zlist[i]){
          /*MARKER(("Size match (%d): %s\n", (int)sz,
            cwal_type_id_name(tlist[x])));*/
          cwalRecyclerInfo.indexes[tlist[x]] = (int)i;
        }
      }
    }
    assert(cwalRecyclerInfo.indexes[CWAL_TYPE_XSTRING]
           ==cwalRecyclerInfo.indexes[CWAL_TYPE_ZSTRING]);
    /* assert(cwalRecyclerSizes[CWAL_TYPE_OBJECT]); */

    cwalRecyclerInfo.indexes[CWAL_TYPE_KVP] = zCount++;
    cwalRecyclerInfo.indexes[CWAL_TYPE_WEAKREF] = zCount++;
    cwalRecyclerInfo.indexes[CWAL_TYPE_SCOPE] = zCount++;

    cwalRecyclerInfo.recyclerCount = zCount;
#if !defined(NDEBUG)
    {
      /* Make sure cwal_engine::recycler is sized big enough. */
      cwal_engine * e = 0;
      assert(zCount <= sizeof(e->recycler)/sizeof(e->recycler[0]));
    }
#endif
#if 0
    for(i = 0; i < CWAL_TYPE_end; ++i){
      MARKER(("Recycler: %d %s index=%d\n",
              (int)i,
              cwal_type_id_name((cwal_type_id)i),
              (int)cwalRecyclerInfo.indexes[i]
              ));
    }
#endif
  }
#undef ASIZE
}

/**
   Returns the index in cwal_engine::recycler[] for the given
   type ID, or -1 if there is no recycler for that type.

   Special case: for CWAL_TYPE_STRING it returns the x-/z-string
   index. Cases which refer to the real string recycler need to be
   careful NOT to use this routine, but cwal_recycler_get()
   instead.
*/
static int cwal_recycler_index( cwal_type_id typeID ){
  static int once = 0;
  if(!once){
    cwal_setup_recycler_indexes();
    once=1;
    /*
      Pedantic side note: if two threads init two cwal_engine
      instances at the same time, they will both init the recycler
      indexes, but will set the same memory to the same values, so
      it makes no difference. We set once=1 last to allow two
      threads to do that, rather than to have the second thread
      continue before the first is filling up cwalRecyclerInfo.
    */
  }
  switch( typeID ){
    case CWAL_TYPE_STRING:
      return cwalRecyclerInfo.indexes[CWAL_TYPE_XSTRING]
        /* Special case because x/z-strings get tagged
           with this type after construction. */
        ;
    case CWAL_TYPE_INTEGER:
    case CWAL_TYPE_DOUBLE:
    case CWAL_TYPE_XSTRING:
    case CWAL_TYPE_ZSTRING:
      /* we only handle x/z-strings via e->recycler,
         and STRING via e->reString.  */
    case CWAL_TYPE_ARRAY:
    case CWAL_TYPE_OBJECT:
    case CWAL_TYPE_HASH:
    case CWAL_TYPE_NATIVE:
    case CWAL_TYPE_BUFFER:
    case CWAL_TYPE_FUNCTION:
    case CWAL_TYPE_EXCEPTION:
    case CWAL_TYPE_UNIQUE:
    case CWAL_TYPE_TUPLE:

    case CWAL_TYPE_KVP:
    case CWAL_TYPE_SCOPE:
    case CWAL_TYPE_WEAKREF:
    case CWAL_TYPE_PROPREF:

      assert(cwalRecyclerInfo.indexes[typeID]>=0);
      return cwalRecyclerInfo.indexes[typeID];
    default:
      return -1;
  }
}

/**
   Overwrites all state in buf with defaults (zeroes) but
   retains the buf->self member.
*/
static void cwal_buffer_wipe_keep_self( cwal_buffer * buf ){
  void * self = buf->self;
  *buf = cwal_buffer_empty;
  buf->self = self;
}

int cwal_is_dead(cwal_engine const * e){
  return e ? e->fatalCode : CWAL_RC_MISUSE;
}

/** @internal

    Checks whether child refers to a newer scope than parent, and if
    it does then it moves child to par. If par is 0 this function
    sets *res (if not NULL) to 1 and returns.

    Returns 0 on success and the only error case is if popping child
    from its parent or pushing to its new parent fails (which since the
    transition from arrays to linked lists for child values, has no
    failure cases other than memory corruption or similar internal
    mismanagement of cwal's bits). It sets res (if not NULL) to one of:

    -1: means that par takes over child, adding it to its list setting
    child->scope to par.

    0: no action is necessary. Child already belongs to par.

    1: child is a built-in, belongs to par, or belongs to a scope with
    a lower scope->level (in which case that scope is its owner). In any
    such case, this function does not modify child's scope.

    Note that this does NOT change child's reference count in any case.

    It may set e->fatalCode, in which case it returns that.
*/
static int cwal_value_xscope( cwal_engine * const e, cwal_scope * const par,
                              cwal_value * child, int * res );

    
/**
   Internal debuggering routine which dumps out info about the value v
   (which must not be NULL) to stdout or stderr. If msg is provided it
   is appended to the output.
*/
void cwal_dump_value( char const * File, int Line, cwal_value const * v,
                      char const * msg ){
  FILE * out = stdout;
  cwal_obase const * b = CWAL_VOBASE(v);
  if(File && *File && (Line>0)){
    fprintf(out, "%s():%d: ", File, Line );
  }
  if(!v){
    fprintf(out, "(cwal_value *)NULL\n");
    return;
  }
  fprintf(out, "%s@%p (scope=#%d) refCount=%u",
          v->vtab->typeName,
          (void*)v,
          v->scope
          ? (int)v->scope->level
          : (int)(CWAL_MEM_IS_BUILTIN(v) ? 0 : -666),
          (unsigned)CWAL_REFCOUNT(v));
  if(b){
    fprintf( out, " flags=%02x", b->flags );
  }
  if( cwal_value_is_string(v) ){
    cwal_string const * s = cwal_value_get_string(v);
    if(s && CWAL_STRLEN(s)){
      const cwal_size_t len = CWAL_STRLEN(s);
      fprintf( out, " strlen=%u", (unsigned)len);
      if( len <= 30 ){
        fprintf( out, " bytes=[%.*s]", (int)len, cwal_string_cstr(s) );
      }
    }else{
      fprintf( out, " (STILL INITIALIZING?)" );
    }
  }
  else if(cwal_value_is_integer(v)){
    fprintf( out, " int=%"CWAL_INT_T_PFMT,
             cwal_value_get_integer(v));
  }
  else if(cwal_value_is_double(v)){
    fprintf( out, " double=%"CWAL_DOUBLE_T_PFMT,
             cwal_value_get_double(v));
  }
  else if(cwal_value_is_bool(v)){
    fprintf( out, " bool=%s",
             cwal_value_get_double(v) ? "true" : "false");
  }else if(cwal_value_is_tuple(v)){
    cwal_tuple const * p = CWAL_TUPLE(v);
    assert(p);
    fprintf( out, " n=%d", (int)p->n );
  }
  if(msg && *msg) fprintf( out, "\t%s\n", msg );
  else fputc( '\n', out );
  fflush(out);
}

#define dump_val(V,M) cwal_dump_value(__FILE__,__LINE__,(V),(M))

/**
   Returns the recycler for the given type, distinguishing between
   CWAL_TYPE_STRING (==>e->reString) and CWAL_TYPE_XSTRING/ZSTRING
   (==>e->recycler[something]). BE CAREFUL: do not put x/z-strings
   in the other string bin, or vice versa!
*/
static cwal_recycler * cwal_recycler_get( cwal_engine * e, cwal_type_id t ){
  if(CWAL_TYPE_STRING == t){
    return &e->reString;
  }
  else {
    const int ndx = cwal_recycler_index(t);
    /* assert(ndx>=0); */
    return (ndx >= 0) ? &e->recycler[ndx] : 0;
  }
}


/**
   Either adds mem to e->reChunk or (if recycling is disabled or capacity/quota
   is reached) frees mem using cwal_free().
*/    
static void cwal_memchunk_add(cwal_engine * e, void * mem, cwal_size_t size ){
  cwal_memchunk_recycler * re = &e->reChunk;
  assert(mem);
  assert(size>0);
  assert((cwal_size_t)-1 != size);
  size = CWAL_MEMSZ_PAD(size);
  if(CWAL_F_TRACK_MEM_SIZE & e->flags){
    /* If we have the size info in the memory block then use that.
       This allows us to make up for list and string allocations truncating
       sizes, effectively "losing" bytes (making them unusable, but still
       part of a tracked block) each time they do that. */
    cwal_memsize_t * sz = MEMSZ_PTR_FROM_MEM(mem);
    /* static int counter1 = 0; ++counter1; */
    if(*sz != size+sizeof(void*)){
      /* static int counter2 = 0; */
      assert(*sz);
      assert(e->memcap.currentMem >= *sz);
      assert(size < *sz-sizeof(void*));
      /*MARKER(("#%d of %d Replacing size %d with %d from memory stamp.\n",
        ++counter2, counter1, (int)size, (int)(*sz - sizeof(void*))));*/
      e->metrics.recoveredSlackBytes += *sz - size - sizeof(void*);
      ++e->metrics.recoveredSlackCount;
      size = *sz - sizeof(void*)
        /* -sizeof is needed to avoid a semantic conflict in
           cwal_memchunk_overlay::size
           vs. *MEMSZ_PTR_FROM_MEM(mem). If we don't account
           for it here, we end up corrupting malloc()-internal
           state!
        */;
    }
  }
  if(!re->config.maxChunkCount
     || !e->current /* i.e. during finalization */
     || size < sizeof(cwal_memchunk_overlay)
     || (re->config.maxTotalSize < (size + re->currentTotal))
     || (re->config.maxChunkSize < size)
     || (re->headCount >= re->config.maxChunkCount)
     ){
    /* MARKER(("Freeing chunk @%p of size %d\n", mem, (int)size)); */
    cwal_free(e,mem);
  }else{
    cwal_memchunk_overlay * ovl;
    /* MARKER(("Adding mem chunk @%p of size %d\n", mem, (int)size)); */
    assert(re->headCount < re->config.maxChunkCount);
    /* memset(mem, 0, size); */
    ovl = (cwal_memchunk_overlay *)mem;
    *ovl = cwal_memchunk_overlay_empty;
    ovl->size = size;
    if(!re->metrics.smallestChunkSize
       || (size < re->metrics.smallestChunkSize)){
      re->metrics.smallestChunkSize = size;
    }
    if(size > re->metrics.largestChunkSize){
      re->metrics.largestChunkSize = size;
    }
    if( (re->currentTotal += size) > 
        re->metrics.peakTotalSize ){
      re->metrics.peakTotalSize = re->currentTotal;
    }
    if( ++re->headCount > re->metrics.peakChunkCount ){
      re->metrics.peakChunkCount = re->headCount;
    }
    re->metrics.runningAverageSize = (re->metrics.runningAverageSize + size)/2;
    /* Insert ovl into the linked list, sorted by size... */
    if(!re->head){
      assert(1==re->headCount) /* 1 b/c we increment it above */;
      re->head = ovl;
    }else{
      cwal_memchunk_overlay * h = re->head;
      cwal_memchunk_overlay * prev = 0;
      char gotIt = 0;
      for( ; h; prev = h, h = h->next ){
        if(h->size>=ovl->size){
          gotIt = 1;
          if(prev){
            prev->next = ovl;
          }else{
            assert(h==re->head);
            re->head = ovl;
          }
          ovl->next = h;
          break;
        }
      }
      if(!gotIt){
        /* End of list: ovl is biggest */
        assert(prev);
        assert(!prev->next);
        assert(prev->size < ovl->size);
        prev->next = ovl;
      }
    }
    /* MARKER(("Added mem chunk @%p #%d of size %d\n", mem, (int)re->headCount, (int)size)); */
  }
  return;
}

/**
   Requests a chunk of memory from the memchunk cache. *size must be
   the size of the block we would "like" to have. A value of zero
   means to give back the first chunk (an O(1) op).

   deltaPolicy specifies a size tolerance. Legal values are:

   0: no tolerance - only an exact size match qualifies.

   N>100: a percent larger the buffer may be. If a larger buffer is
   found within that that size, it is returned. It is ignored if
   !*size. If deltaPolicy>999 then any chunk at least as large as *size
   will be accepted.

   N<100: at least *size, but no bigger than *size+N.

   N<0: the first buffer in the list will match, the same as if
   *size==0.

   If non-NULL is returned, *size is updated to the size of the chunk.

   Potential TODO: change sig to include a param which specifies how
   to interpret deltaPolicy (e.g. absolute vs percent vs
   smaller-or-larger).

   Potential TODO: if !*size, interpret deltaPolicy as a preferred value
*/
static void * cwal_memchunk_request(cwal_engine * e, cwal_size_t * size,
                                    int deltaPolicy,
                                    char const * debugDescription){
  cwal_memchunk_recycler * re = &e->reChunk;
  cwal_memchunk_overlay * ovl = 0;
  cwal_memchunk_overlay * left = 0;
  void * rc = 0;
  ++re->metrics.requests;
  for( ovl = re->head; ovl; left = ovl, ovl = ovl->next ){
    ++re->metrics.searchComparisons;
    if(deltaPolicy<0
       || !*size
       || *size == ovl->size) break;
    else if( deltaPolicy>100 ){
      /* Chunk size within (*size, *size+(deltaPolicy/100)) */
      cwal_size_t const max = (*size * (cwal_size_t)(deltaPolicy/100.0));
      assert(ovl->size!=*size && "Is caught above.");
      if( ovl->size>*size
          && (deltaPolicy>999 || ovl->size <= max)
          ){
        break;
      }else if(ovl->size > max){
        /* We won't find a match above this point */
        ovl = 0;
        break;
      }
    }else if(deltaPolicy>0){
      /* Interpret as absolute byte count */
      if(ovl->size>*size
         && ovl->size <= *size + deltaPolicy ){
        break;
      }else if(ovl->size > *size * deltaPolicy){
        /* We won't find a match above this point */
        ovl = 0;
        break;
      }
    }
  }
  if(!ovl) ++re->metrics.searchMisses;
  else{
    /*MARKER(("Donating chunk of size %d for request of size %d. Context=%s\n",
      (int)ch->size, (int)*size, debugDescription));*/
    if(debugDescription){/*avoid unused param warning*/}
    assert(re->headCount>0);
    if(left){
      assert(re->headCount>1);
      assert(left->next == ovl);
      left->next = ovl->next;
    }
    if(ovl->next){
      assert(re->headCount > (left ? 2 : 1));
    }
    if(re->head == ovl){
      re->head = ovl->next;
    }
    *size = ovl->size;
    memset(ovl, 0, *size);
    rc = ovl;
    assert(re->currentTotal >= *size);
    re->currentTotal -= *size;
    --re->headCount;
    if(!re->headCount){
      assert(!re->head);
    }
    ++re->metrics.totalChunksServed;
    re->metrics.totalBytesServed += *size;
    re->metrics.runningAverageResponseSize =
      (re->metrics.runningAverageResponseSize + *size)/2;
  }
  return rc;
}


static void cwal_memchunk_freeit( cwal_engine * e, cwal_memchunk_overlay * ovl ){
  cwal_memchunk_recycler * re = &e->reChunk;
  assert(!ovl->next);
  assert(re->headCount>0);
  assert(re->currentTotal >= ovl->size);
  --re->headCount;
  re->currentTotal -= ovl->size;
  *ovl = cwal_memchunk_overlay_empty;
  cwal_free(e, ovl);
}

/**
   Frees up all entries in e->reChunk.
*/
static void cwal_memchunks_free( cwal_engine * e ){
  cwal_memchunk_recycler * re = &e->reChunk;
  cwal_memchunk_overlay * ovl;
  for( ; (ovl = re->head);  ){
    re->head = ovl->next;
    ovl->next = 0;
    cwal_memchunk_freeit(e, ovl);
  }
  assert(0==re->headCount);
  assert(0==re->currentTotal);
  re->head = 0;
}


int cwal_engine_memchunk_config( cwal_engine * e,
                                 cwal_memchunk_config const * conf){
  if(!e || !conf) return CWAL_RC_MISUSE;
  else{
    /* Adjust size... */
    cwal_memchunk_recycler * re = &e->reChunk;
    cwal_memchunk_overlay * ovl;
    if(!conf->maxChunkCount
       || !conf->maxTotalSize
       || !conf->maxChunkSize){
      /* The simplest case: disable the recycler. */
      cwal_memchunks_free( e );
      re->config = *conf;
      return 0;
    }

    if(re->config.maxChunkSize > conf->maxChunkSize){
      /* Trim now-too-large chunks. */
      cwal_memchunk_overlay * prev = 0;
      cwal_memchunk_overlay * next = 0;
      ovl = re->head;
      assert(ovl);
      for( ; ovl; ovl = next){
        next = ovl->next;
        if(ovl->size > conf->maxChunkSize){
          if(prev) prev->next = next;
          ovl->next = 0;
          cwal_memchunk_freeit(e, ovl);
        }else{
          prev = ovl;
        }
      }
    }

    if(re->currentTotal > conf->maxTotalSize){
      /* Trim chunks until we're under the limit. */
      cwal_memchunk_overlay * prev = 0;
      cwal_memchunk_overlay * next = 0;
      ovl = re->head;
      assert(ovl);
      for( ; ovl && (re->currentTotal > conf->maxTotalSize);
           ovl = next ){
        next = ovl->next;
        if(ovl->size > conf->maxChunkSize){
          if(prev) prev->next = next;
          ovl->next = 0;
          cwal_memchunk_freeit(e, ovl);
        }else{
          prev = ovl;
        }
      }
    }
    while(conf->maxChunkCount
          ? (re->headCount > conf->maxChunkCount)
          : !!re->headCount){
      /* Too many chunks. Lop off the smallest ones. */
      assert(re->head);
      ovl = re->head;
      re->head = ovl->next;
      ovl->next = 0;
      cwal_memchunk_freeit(e, ovl);
    }
    re->config = *conf;
    return 0;
  }
}

cwal_kvp * cwal_kvp_alloc(cwal_engine *e){
  cwal_kvp * kvp;
  cwal_recycler * re;
  ++e->metrics.requested[CWAL_TYPE_KVP];
  re = cwal_recycler_get(e, CWAL_TYPE_KVP);
  assert(re);
  if(re->list){
    ++re->hits;
    ++e->metrics.valuesRecycled;
    kvp = (cwal_kvp*)re->list;
    re->list = kvp->right;
    kvp->right = 0;
    --re->count;
  }
  else{
    ++re->misses;
    ++e->metrics.valuesRecycleMisses;
    kvp = (cwal_kvp*)cwal_malloc(e, sizeof(cwal_kvp));
    if(kvp){
      ++e->metrics.allocated[CWAL_TYPE_KVP];
      e->metrics.bytes[CWAL_TYPE_KVP] += sizeof(cwal_kvp);
    }
  }
  if( kvp ) {
    *kvp = cwal_kvp_empty;
  }
  return kvp;
}

void cwal_kvp_clean( cwal_engine * e, /* cwal_scope * fromScope, */
                     cwal_kvp * kvp ){
  if( kvp ){
    cwal_value * key = kvp->key;
    cwal_value * value = kvp->value;
    *kvp = cwal_kvp_empty;
    if(key) cwal_value_unref2(e, key);
    if(value) cwal_value_unref2(e, value);
  }
}

void cwal_kvp_free( cwal_engine * e, /* cwal_scope * fromScope, */
                    cwal_kvp * kvp, char allowRecycle ){
  if( kvp ){
    cwal_kvp_clean(e/* , fromScope */, kvp);
    if(allowRecycle){
      cwal_recycler * re = cwal_recycler_get(e, CWAL_TYPE_KVP);
      assert(re);
      if(re->count < re->maxLength){
        assert(!kvp->right);
        kvp->right = (cwal_kvp*)re->list;
        re->list = kvp;
        ++re->count;
      }else{
        cwal_free(e, kvp);
      }
    }
    else{
      cwal_free(e, kvp);
    }
  }
}

cwal_value * cwal_kvp_key( cwal_kvp const * kvp ){
  return kvp ? kvp->key : NULL;
}

cwal_value * cwal_kvp_value( cwal_kvp const * kvp ){
  return kvp ? kvp->value : NULL;
}

int cwal_kvp_value_set( cwal_kvp * const kvp,
                        cwal_value * const v ){
  if(!kvp || !v) return CWAL_RC_MISUSE;
  else if(kvp->value == v) return 0;
  else{
    assert(kvp->key);
    assert(kvp->value);
    cwal_value_ref(v);
    cwal_value_unref(kvp->value);
    kvp->value = v;
    return 0;
  }
}

int cwal_kvp_value_set2( cwal_kvp * const kvp,
                         cwal_value * const v ){
  if(!kvp || !v) return CWAL_RC_MISUSE;
  else if(CWAL_VAR_F_CONST & kvp->flags){
    return CWAL_RC_CONST_VIOLATION;
  }
  return cwal_kvp_value_set(kvp, v);
}

cwal_flags16_t cwal_kvp_flags( cwal_kvp const * kvp ){
  return kvp ? (cwal_flags16_t)(CWAL_VAR_F_PRESERVE & kvp->flags) : 0;
}

cwal_flags16_t cwal_kvp_flags_set( cwal_kvp * kvp, cwal_flags16_t flags ){
  cwal_flags16_t const rc = (cwal_flags16_t)(CWAL_VAR_F_PRESERVE & kvp->flags);
  if(CWAL_VAR_F_PRESERVE != flags){
    kvp->flags = flags;
  }
  return rc;
}

#if !CWAL_OBASE_ISA_HASH
/** @internal

    Searches for the given key in the given kvp list.

    Returns the found item if a match is found, NULL if not. If prev is
    not NULL then *prev is set to the returned value's left-hand-side
    kvp from the linked list (or the end of the list, if no match is
    found). A *prev value of NULL and return value of non-NULL
    indicates that the result kvp was found at the head of the list.
*/
static cwal_kvp * cwal_kvp_search( cwal_kvp * kvp, char const * key,
                                   cwal_midsize_t keyLen, cwal_kvp ** prev){
  if(!kvp || !key ) return NULL;
  else{
    cwal_kvp * left = 0;
    char const * cKey;
    cwal_size_t cLen;
    int cmp;
    for( left = 0; kvp; left = kvp, kvp = kvp->right ){
      assert( kvp->key );
      assert(kvp->right != kvp);
      cKey = cwal_value_get_cstr(kvp->key, &cLen);
      if(prev) *prev=left;
      if(!cKey) continue /* we don't know where non-strings sort in this world! */;
      else if(0 == (cmp = cwal_compare_cstr( key, keyLen, cKey, cLen ))){
        return kvp;
      }
#if CWAL_KVP_TRY_SORTING
      if(cmp<0) break;
#endif
    }
    if(prev) *prev = CWAL_KVP_TRY_SORTING ? left : NULL;
    return 0;
  }
}

/**
   Variant of cwal_kvp_search() which takes a cwal_value key and uses
   key->vtab->compare() to check for equivalence.
*/
static cwal_kvp * cwal_kvp_search_v( cwal_kvp * kvp,
                                     cwal_value const * key,
                                     cwal_kvp ** prev){
  if(!kvp || !key ) return NULL;
  else{
    cwal_kvp * left;
    int cmp;
    const int keyIsBool = CWAL_TYPE_BOOL==key->vtab->typeID;
    for( left = 0; kvp; left = kvp, kvp = kvp->right ){
      const int kvpIsBool = CWAL_TYPE_BOOL==kvp->key->vtab->typeID;
      assert(kvp->key);
      assert(kvp->right != kvp);
      if(key==kvp->key){
        cmp = 0;
      }else if(keyIsBool || kvpIsBool){
        /*
          20190706: it was *finally* discovered (by accident)
          that:

          var o = {};
          o[true];

          Matches the first truthy property key! In our case,
          it was the s2 object prototype's constructor method.

          Likewise:

          o[true] = 3; // bool-typed property key
          o['x']; // ==> 3

          Because this lookup explicitly does type-loose
          comparisons and most things are equivalent to true
          in that context. So... now we special-case a
          type-strict lookup if either of key or kvp->key is-a
          boolean.
        */
        if(keyIsBool && kvpIsBool){
          cmp = key->vtab->compare(key, kvp->key);
          /* And fall through. */
        }else{
          /* Do not allow a match if either key is a bool
             but the other one is not. */
          continue;
        }
      }else{
        cmp = key->vtab->compare(key, kvp->key);
      }
      if(prev) *prev=left;
      if(0==cmp) return kvp;
#if CWAL_KVP_TRY_SORTING
      else if(cmp<0) break;
#endif
    }
    if(prev) *prev = CWAL_KVP_TRY_SORTING ? left : 0;
    return 0;
  }
}

/**
   Functionally identical to cwal_kvp_unset_v() but takes
   its key in C-string form.
*/
static int cwal_kvp_unset( cwal_engine * e, /* cwal_scope * fromScope, */
                           cwal_kvp ** list,
                           char const * key, cwal_midsize_t keyLen ) {
  /* assert(fromScope); */
  assert(e);
  assert(key);
  if( !e || !key || !list ) return CWAL_RC_MISUSE;
  else if(!*list) return CWAL_RC_NOT_FOUND;
  else {
    cwal_kvp * left = 0;
    cwal_kvp * kvp;
    kvp = cwal_kvp_search( *list, key, keyLen, &left );
    if( !kvp ) return CWAL_RC_NOT_FOUND;
    else if(left) left->right = kvp->right;
    else {
      assert(*list == kvp);
      *list = kvp->right;
    }
    kvp->right = NULL;
    cwal_kvp_free( e/* , fromScope */, kvp, 1 );
    return 0;
  }
}

/**
   Unsets a the given key in the given kvp list. list must point to
   the head of a cwal_kvp list, and if the head is the unset kvp then
   *list is updated to point to kvp->right.

   Returns 0 on successs, CWAL_RC_NOT_FOUND if
   no match is found.
*/
static int cwal_kvp_unset_v( cwal_engine * e, cwal_kvp ** list,
                             cwal_value * key ) {
  assert(e);
  assert(key);
  if( !e || !key || !list ) return CWAL_RC_MISUSE;
  else if(!*list) return CWAL_RC_NOT_FOUND;
  else {
    cwal_kvp * left = 0;
    cwal_kvp * kvp;
    kvp = cwal_kvp_search_v( *list, key, &left );
    if( ! kvp ) return CWAL_RC_NOT_FOUND;
    if(left) left->right = kvp->right;
    else {
      assert(*list==kvp);
      *list = kvp->right;
    }
    kvp->right = NULL;
    cwal_kvp_free( e, kvp, 1 );
    return 0;
  }
}
#endif /* !CWAL_OBASE_ISA_HASH */
    
/**
   Returns the client-supplied hash table size "trimmed" to some
   "convenient" prime number (more or less arbitrarily chosen by this
   developer - feel free to change/extend this range).
*/
static uint16_t cwal_trim_hash_size( uint16_t hashSize ){
  if(hashSize < CwalConsts.MinimumHashSize) return CwalConsts.MinimumHashSize;
#define P(N) else if( hashSize <= N ) return N
  /* TODO? add more granularity here. */
  P(17); P(37); P(53); P(71);
  P(151); P(211); P(281); P(311);
  P(379); P(433); P(503); P(547);
  P(587); P(613); P(683); P(719);
  P(751); P(1033);
  P(1549); P(2153);
  else return 3163;
#undef P
}

/**
   Returns the cwal_ptr_table step size "trimmed" to some "acceptable"
   range.
*/
static uint16_t cwal_trim_step( uint16_t step ){
  if(step < CwalConsts.MinimumStep) step = CwalConsts.MinimumStep;
  return step;
}


/**
   Allocates a new cwal_ptr_page object, owned by e. Returned NULL on
   error. On success the returned value is empty-initialized.

   The page has n entries (hash table size) and the given step size.

   n MUST currently be the same as the owning table's hash size, but
   there are plans to potentially change that, allowing new page sizes
   to grow if collision counts become too high.
*/
static cwal_ptr_page * cwal_ptr_page_create( cwal_engine * e, uint16_t n ){
  const uint32_t asz = (sizeof(void*) * n);
  const uint32_t psz = asz + sizeof(cwal_ptr_page);
  cwal_ptr_page * p = (cwal_ptr_page*)cwal_malloc(e, psz);
  if(p){
    memset( p, 0, psz );
    p->list = (void**)(p+1);
    p->entryCount = 0;
  }
  return p;
}

/**
   Allocates a new page for the given table.

   Returns CWAL_RC_OK on success. The only conceivable non-bug error
   case here is CWAL_RC_OOM.
*/
static int cwal_ptr_table_add_page( cwal_engine * e, cwal_ptr_table * t ){
  cwal_ptr_page * p = cwal_ptr_page_create( e, t->hashSize );
  if(!p) return CWAL_RC_OOM;
  else {
    cwal_ptr_page * tail = t->pg.tail;
    if(!tail){
      assert(!t->pg.head);
      t->pg.head = t->pg.tail = p;
    }else{
      assert(t->pg.head);
      assert(!tail->next);
      t->pg.tail = tail->next = p;
    }
    assert(t->pg.tail->next != t->pg.head);
    return 0;
  }
}

int cwal_ptr_table_create( cwal_engine * e, cwal_ptr_table ** T,
                           uint16_t hashSize,
                           uint16_t step){
  int rc;
  cwal_ptr_table * t;
  if(!e || !T) return CWAL_RC_MISUSE;
  hashSize = cwal_trim_hash_size(hashSize);
  step = cwal_trim_step(step);
  t = *T;
  if(t){
    *t = cwal_ptr_table_empty;
  }else {
    cwal_size_t reqSize = sizeof(cwal_ptr_table);
    t = (cwal_ptr_table*)cwal_memchunk_request(e, &reqSize, 0,
                                               "cwal_ptr_table_create()");
    if(!t){
      t = (cwal_ptr_table*)cwal_malloc(e, reqSize);
    }else{
      assert(reqSize==sizeof(cwal_ptr_table));
    }
    if(!t) {
      rc = CWAL_RC_OOM;
      goto error;
    }
    *t = cwal_ptr_table_empty;
    t->allocStamp = CwalConsts.AllocStamp;
  }

  t->hashSize = hashSize;
  t->step = step;
  rc = cwal_ptr_table_add_page( e, t );
  if(rc) goto error;
  assert( t->pg.head );
  if(!*T) *T = t;
  return CWAL_RC_OK;

  error:
  assert(0 != rc && "You seem to have failed to set an error code!");
  if(t && (t!=*T)){
    cwal_memchunk_add(e, t, sizeof(cwal_ptr_table));
    /* cwal_free(e, t); */
  }
  return rc;
}

int cwal_ptr_table_destroy( cwal_engine * e, cwal_ptr_table * t ){
  if(!e || !t) return CWAL_RC_MISUSE;
  else{
    cwal_size_t const psz = sizeof(cwal_ptr_page) +
      (sizeof(void*) * t->hashSize);
    void const * stamp = t->allocStamp;
    cwal_ptr_page * p = t->pg.head;
    assert(t->hashSize || !p);
    t->pg.head = t->pg.tail = NULL;
    while( p ){
      cwal_ptr_page * n = p->next;
      /* cwal_free(e, p); */
      cwal_memchunk_add(e, p, psz);
      p = n;
    }
    *t = cwal_ptr_table_empty;
    if( CwalConsts.AllocStamp == stamp ){
      cwal_free( e, t );
      /* cwal_memchunk_add(e, t, sizeof(cwal_ptr_table)); */
    }else{
      /* Assume t was stack allocated or is part of another
         object.*/
      t->allocStamp = stamp;
    }
    return CWAL_RC_OK;
  }
}

int cwal_ptr_table_visit( cwal_ptr_table * t, cwal_ptr_table_visitor_f f, void * state ){
  if(!t || !f) return CWAL_RC_MISUSE;
  else{
    cwal_size_t i;
    cwal_ptr_page * page = t->pg.head;
    cwal_value ** val;
    cwal_size_t seen;
    int rc = 0;
    for( ; page; page = page->next ){
      seen = 0;
      for( i = 0;
           (0==rc)
             && (i < t->hashSize)
             && (seen < page->entryCount); ++i ){
        val = (cwal_value**)&page->list[i];
        if(!*val) continue;
        ++seen;
        rc = f( val, state );
        page->list[i] = *val;
      }

    }
    return rc;
  }
}


int cwal_ptr_table_mem_cost( cwal_ptr_table const * t,
                             uint32_t * mallocs,
                             uint32_t * memory ){
  enum { SV = sizeof(void*) };
  uint32_t a = 1;
  uint32_t m = sizeof(cwal_ptr_table);
  if(!t) return CWAL_RC_MISUSE;
  else{
    cwal_ptr_page const * p = t->pg.head;
    for( ; p; p = p->next ){
      ++a;
      m += sizeof(cwal_ptr_page)
        + (t->hashSize*SV);
    }
  }
  if(mallocs) *mallocs = a;
  if(memory) *memory = m;
  return CWAL_RC_OK;
}

static uint16_t cwal_ptr_table_hash( void const * key,
                                     uint16_t step,
                                     uint16_t hashSize){
#if CWAL_VOID_PTR_IS_BIG
  /* IF THE DEBUGGER LEADS YOU NEAR HERE...
     try changing ptr_int_t back to uint64_t.
  */
  typedef uint64_t ptr_int_t;
#else
  typedef uint32_t ptr_int_t;
#endif
#if 1
  const ptr_int_t k = ((ptr_int_t)key);
  const uint32_t v1 = (uint32_t)(k / step);
  const uint16_t v2 = v1 % hashSize;
  /*
    MARKER("key=%p step=%u hashSize=%u k=%lu v1=%u v2=%u\n",
    key,   step,   hashSize,   k,    v1,   v2);
  */
  return v2;
#else
  return ((ptr_int_t)key) / step % hashSize;
#endif
}


int cwal_ptr_table_op( cwal_engine * e,
                       cwal_ptr_table * t,
                       void * key,
                       cwal_ptr_table_ops op ){
  cwal_ptr_page * pPage;
  void * pRet = NULL;
  uint16_t iKey;
  int rc = 0;
  if(!e || !t || !key) return CWAL_RC_MISUSE;
  iKey = cwal_ptr_table_hash( key, t->step, t->hashSize );
  assert( iKey < t->hashSize );
  assert( t->pg.head );
  /*
    TODO?: honor page-specific hashSize values, re-calculating the
    hash value.  if the step value changes while iterating.
  */
  switch( op ){
    /* FIXME: refactor the 3 loops below into one loop at the start.
     */
    case CWAL_PTR_TABLE_OP_SEARCH:
      rc = CWAL_RC_NOT_FOUND;
      for(pPage = t->pg.head; pPage; pPage = pPage->next ){
        pRet = pPage->list[iKey];
        if(!pRet) break;
        else if(pRet == key){
          rc = CWAL_RC_OK;
          break;
        }
      }
      break;
    case CWAL_PTR_TABLE_OP_INSERT:{
      assert(t->pg.head);
      rc = CWAL_RC_OK;
      for(pPage = t->pg.head; pPage; pPage = pPage->next ){
        pRet = pPage->list[iKey];
#if 0
        MARKER("COMPARING STASHED %p AGAINST KEY %p\n", (void*)pRet, (void*)key);
#endif
        if(!pRet) goto insert;
        else if(pRet == key){
          rc = CWAL_RC_ALREADY_EXISTS;
          break;
        }
      }
#if 0
      MARKER("INSERT NO AVAILABLE SLOT CONTAINS %p. "
             "ADDING PAGE for hash %u rc=%d=%s\n",
             (void *)key, iKey, rc, cwal_rc_cstr(rc));
#endif
      if(rc) break;
      /* We reached the end of the tables and found
         no empty slot. Add a page and insert it there. */
      rc = cwal_ptr_table_add_page( e, t );
      if(rc) break;
      pPage = t->pg.tail;
        insert:
#if 0
      MARKER("INSERTING %p (hash=%u) in list %p\n", (void*)key, iKey, (void *)pPage);
#endif
      rc = CWAL_RC_OK;
      assert(NULL != pPage);
      pPage->list[iKey] = key;
      ++pPage->entryCount;
    } break;
    case CWAL_PTR_TABLE_OP_REMOVE:
      rc = CWAL_RC_NOT_FOUND;
      for(pPage = t->pg.head; pPage; pPage = pPage->next ){
        pRet = pPage->list[iKey];
        if(!pRet) break;
        else if(pRet == key){
          cwal_ptr_page * prevPage = pPage;
          assert(pPage->entryCount>0);
          /* hijack the loop to shift all other entries
             down... */
          pPage->list[iKey] = NULL;
          if(!pPage->next){
            --pPage->entryCount;
          }else{
            /* Potential TODO: move empty pages
               to the back of the list. */
            for( pPage = pPage->next; pPage; 
                 prevPage = pPage, pPage = pPage->next ){
              if(!(prevPage->list[iKey] = pPage->list[iKey])){
                --prevPage->entryCount;
                break;
              }
              else if(!pPage->next){
                pPage->list[iKey] = 0;
                --pPage->entryCount;
              }
            }
          }
          rc = CWAL_RC_OK;
#if 0
          MARKER("REMOVING ENTRY %p for hash %u FROM PAGE #%u\n", (void*)pRet, iKey, x+1);
#endif
          break;
        }
      }
      break;
  }
    
  return rc;
}

int cwal_ptr_table_search( cwal_engine * e,
                           cwal_ptr_table * t,
                           cwal_value * key ){
  return cwal_ptr_table_op( e, t, key, CWAL_PTR_TABLE_OP_SEARCH );
}
int cwal_ptr_table_insert( cwal_engine * e,
                           cwal_ptr_table * t,
                           cwal_value * key ){
  return cwal_ptr_table_op( e, t, key, CWAL_PTR_TABLE_OP_INSERT );
}
int cwal_ptr_table_remove( cwal_engine * e,
                           cwal_ptr_table * t,
                           cwal_value * key ){
  return cwal_ptr_table_op( e, t, key, CWAL_PTR_TABLE_OP_REMOVE );
}

/**
   Adds an entry in e->weakp for p, initializing e->weakp
   if needed.
*/
static int cwal_weak_annotate( cwal_engine * e, void * p ){
  int rc;
  cwal_ptr_table * pt = &e->weakp;
  if(!pt->pg.head){
    cwal_size_t const hashSize = 151
      /* We don't expect many weak refs, so we'll try
         a relatively small hash size. */;
    rc = cwal_ptr_table_create(e, &pt, hashSize, sizeof(void *) );
    if(rc) return rc;
    assert(pt->pg.head);
  }
  rc = cwal_ptr_table_op( e, pt, p, CWAL_PTR_TABLE_OP_INSERT );
  /* MARKER(("Annotating %s weak ptr @%p insert rc=%d\n",cwal_type_id_name(tid),p,rc)); */
  switch(rc){
    case 0:
    case CWAL_RC_ALREADY_EXISTS:
      return 0;
    default:
      return rc;
  }
}

/**
   If p is found in e->weakr[tid] then its (shared) cwal_weakref is
   returned, otherwise 0 is returned.
*/
static cwal_weakref * cwal_weakref_search( cwal_engine * e, void *p, cwal_type_id tid ){
  assert(e && p && (tid>=CWAL_TYPE_UNDEF && tid<CWAL_TYPE_end));
  cwal_weakref * r = e->weakr[tid];
  for( ; r && (r->value != p); r = r->next ){}
  return r;
}

/**
   Removes the entry for p added by cwal_weak_annotate() and
   unsets the value field of each cwal_weakref in
   e->weakr[t] where value==p.
*/
static int cwal_weak_unregister( cwal_engine * e, void * p,
                                 cwal_type_id t ){
  assert(p);
  assert(t>=CWAL_TYPE_UNDEF && t<CWAL_TYPE_end);
  if(!e->weakp.pg.head) return 0;
  else{
        
    int const rc = cwal_ptr_table_op( e, &e->weakp, p,
                                      CWAL_PTR_TABLE_OP_REMOVE );
    switch(rc){
      case 0:{
        /* MARKER(("Invalidating %s weak ptr to %p\n",cwal_type_id_name(t),p)); */
        cwal_weakref * r = cwal_weakref_search( e, p, t );
        if(r) r->value = NULL
                /* because we share instances for any given (p,t)
                   combination, there can be no more matches in the
                   list. */
                ;
        return 0;
      }
      case CWAL_RC_NOT_FOUND:
        return 0;
      default:
        return rc;
    }
  }
}

bool cwal_is_weak_referenced( cwal_engine * e, void * p ){
  return (e && p)
    ? (0==cwal_ptr_table_op( e, &e->weakp, p,
                             CWAL_PTR_TABLE_OP_SEARCH ))
    : 0;
}

/**
   Makes wr the head of the cwal_weakref chain starting at
   e->weakr[wr->typeID]. Returns 0 on success, and the only non-bug
   error cases are CWAL_RC_OOM.
*/
static int cwal_weakref_insert( cwal_engine * e, cwal_weakref * wr ){
  int rc;
  assert(wr && (wr->typeID>=CWAL_TYPE_UNDEF && wr->typeID<CWAL_TYPE_end));
  rc = cwal_weak_annotate(e, wr->value);
  if(rc) return rc;
  else{
    cwal_weakref * list;
    assert(!wr->next);
    list = e->weakr[wr->typeID];
    wr->next = list;
    e->weakr[wr->typeID] = wr;
    return 0;
  }
}

/**
   Removes wr from e->weakr[wr->typeID] but does not free wr.
*/
static void cwal_weakref_remove( cwal_engine * e, cwal_weakref * wr ){
  cwal_weakref * list;
  cwal_weakref * prev = NULL;
  assert(wr && (wr->typeID>=CWAL_TYPE_UNDEF && wr->typeID<CWAL_TYPE_end));
  list = e->weakr[wr->typeID];
  for( ; list; prev = list, list = list->next ){
    if(wr != list) continue;
    else if(prev) prev->next = wr->next;
    else e->weakr[wr->typeID] = wr->next;
    wr->next = NULL;
    break;
  }
  assert(wr == list);
}

/**
   Zeroes out r and either adds r to e->recycler (if there is space) or
   frees it (if no recycling space is available).

   Preconditions:

   - neither argument may be 0.
   - r->next must be 0.

   Postconditions: r must be treated as if this function freed it
   (because semantically it does).

*/
static void cwal_weakref_free2( cwal_engine * e, cwal_weakref * r ){
  cwal_recycler * re = cwal_recycler_get(e, CWAL_TYPE_WEAKREF);
  assert(e && r && !r->next);
  assert(!CWAL_MEM_IS_BUILTIN(r));
  if(re->maxLength>0
     && re->count < re->maxLength){
    *r = cwal_weakref_empty;
    r->next = (cwal_weakref*)re->list;
    re->list = r;
    ++re->count;
  }
  else {
    *r = cwal_weakref_empty;
    cwal_free2( e, r, sizeof(cwal_weakref) );
    /* cwal_memchunk_add(e, r, sizeof(cwal_weakref)); */
  }
}

void cwal_weakref_free( cwal_engine * e, cwal_weakref * r ){
  assert(e && r);
  if(!e || !r || CWAL_MEM_IS_BUILTIN(r)) return;
  else if(0==r->refcount || 0==--r->refcount){
    cwal_weakref_remove( e, r );
    cwal_weakref_free2(e, r);
  }         
}

cwal_value * cwal_weakref_value( cwal_weakref * r ){
  if(!r||!r->value) return NULL;
  else switch(r->typeID){
      case CWAL_TYPE_BOOL:
      case CWAL_TYPE_NULL:
      case CWAL_TYPE_UNDEF:
        assert(r->value)
          /* Because of how we allocate/share these, this will
             always be true if r was validly allocated. */;
        CWAL_SWITCH_FALL_THROUGH;
      case CWAL_TYPE_ARRAY:
      case CWAL_TYPE_BUFFER:
      case CWAL_TYPE_DOUBLE:
      case CWAL_TYPE_EXCEPTION:
      case CWAL_TYPE_FUNCTION:
      case CWAL_TYPE_HASH:
      case CWAL_TYPE_INTEGER:
      case CWAL_TYPE_NATIVE:
      case CWAL_TYPE_OBJECT:
      case CWAL_TYPE_STRING:
      case CWAL_TYPE_UNIQUE:
      case CWAL_TYPE_TUPLE:
      case CWAL_TYPE_PROPREF:
        return (cwal_value*)r->value;
      case CWAL_TYPE_KVP:
      case CWAL_TYPE_SCOPE:
      case CWAL_TYPE_WEAKREF:
        return NULL;
      case CWAL_TYPE_XSTRING:
      case CWAL_TYPE_ZSTRING:
        assert(!"Not possible");
        CWAL_SWITCH_FALL_THROUGH;
      default:
        assert(!"Unhandled type!");
        return NULL;          
    }
}


/**
   Allocates a new cwal_weakref. If possible, it takes the head of
   the recycler list, else is cwal_malloc()s it.  The returned value must
   eventually be passed to cwal_weakref_free() or
   cwal_weakref_free2(), depending on its state at cleanup time.

   If cwal_weakref_search(e,ptr,tid) returns a value then this
   function returns the same one.

   It is up to the caller to increment the return'd object's refcount.
*/
static cwal_weakref * cwal_weakref_alloc( cwal_engine * e, void * ptr,
                                            cwal_type_id tid ){
  cwal_weakref * r;
  ++e->metrics.requested[CWAL_TYPE_WEAKREF];
  r = cwal_weakref_search(e, ptr, tid);
  if(!r){
    cwal_recycler * re = cwal_recycler_get(e, CWAL_TYPE_WEAKREF);
    r = (cwal_weakref *)re->list;
    if(r){
      assert(re->count);
      ++re->hits;
      re->list = r->next;
      --re->count;
    }else{
      ++re->misses;
      r = cwal_malloc2(e, sizeof(cwal_weakref));
      if(r){
        ++e->metrics.allocated[CWAL_TYPE_WEAKREF];
        e->metrics.bytes[CWAL_TYPE_WEAKREF] += sizeof(cwal_weakref);
      }
    }
    if(r){
      *r = cwal_weakref_empty;
      r->value = ptr;
      r->typeID = tid;
    }
    if(cwal_weakref_insert(e, r)){
      cwal_weakref_free2(e, r);
      r = NULL;
    }
  }
  return r;
}

cwal_weakref * cwal_weakref_new( cwal_value * v ){
  cwal_engine * e = v ? ((v && v->scope) ? v->scope->e : NULL) : NULL;
  if(!e && !CWAL_MEM_IS_BUILTIN(v)) return NULL;
  else{
    cwal_type_id const tid = v->vtab->typeID;
    cwal_weakref * r = NULL;
    switch(tid){
      case CWAL_TYPE_UNDEF:
        r = &CWAL_BUILTIN_VALS.wref.wUndef;
        break;
      case CWAL_TYPE_NULL:
        r = &CWAL_BUILTIN_VALS.wref.wNull;
        break;
      case CWAL_TYPE_BOOL:
        r = (&CWAL_BUILTIN_VALS.vTrue == v)
          ? &CWAL_BUILTIN_VALS.wref.wTrue
          : &CWAL_BUILTIN_VALS.wref.wFalse;
        break;
      case CWAL_TYPE_STRING:
        if(CWAL_BUILTIN_VALS.vEmptyString==v) r = &CWAL_BUILTIN_VALS.wref.wStrEmpty;
        else{/* It's one of the length-1 strings */}
        break;
      case CWAL_TYPE_INTEGER:{
        /*
          Shared weak-ref-to-int removed 20171202 because (A)
          the built-in int range is now variable, (B) adding
          static weak refs to all of them would be a huge waste
          (weak refs to ints are NEVER used), and (C) ... i
          forgot (C) while reformatting this comment :/.
        */
        break;
      }
      case CWAL_TYPE_DOUBLE:
        if(CWAL_MEM_IS_BUILTIN(v)){
          if(CWAL_BUILTIN_VALS.vDblM1==v) r = &CWAL_BUILTIN_VALS.wref.wDblM1;
          else if(CWAL_BUILTIN_VALS.vDbl0==v) r = &CWAL_BUILTIN_VALS.wref.wDbl0;
          else if(CWAL_BUILTIN_VALS.vDbl1==v) r = &CWAL_BUILTIN_VALS.wref.wDbl1;
          else{assert(!"Impossible!");}
        }
        break;
      default:
        break;
    }
    if(!r){
      r = cwal_weakref_alloc(e, v, v->vtab->typeID);
      if(r){
        assert(r->value == v);
        assert(r->typeID == v->vtab->typeID);
        ++r->refcount;
      }
    }
    return r;
  }
}

cwal_weakref * cwal_weakref_custom_new( cwal_engine * e, void * p ){
  if(!e || CWAL_MEM_IS_BUILTIN(p)) return NULL;
  else{
    cwal_weakref * r = cwal_weakref_alloc(e, p, CWAL_TYPE_WEAKREF);
    if(r){
      assert(r->value == p);
      assert(r->typeID == CWAL_TYPE_WEAKREF);
      ++r->refcount;
    }
    return r;
  }
}

void * cwal_weakref_custom_ptr( cwal_weakref * r ){
  return (r && (CWAL_TYPE_WEAKREF==r->typeID))
    ? r->value
    : NULL;
}

void * cwal_weakref_custom_check( cwal_engine * e, void * p ){
  if(!e || !p) return NULL;
  else{
    cwal_weakref * r = cwal_weakref_search(e, p, CWAL_TYPE_WEAKREF);
    if(r){
      assert(r->value==p);
    }
    return r ? r->value : NULL;
  }
}

bool cwal_weakref_custom_invalidate( cwal_engine * e, void * p ){
  if(!e || !p) return 0;
  else{
    cwal_weakref * r = cwal_weakref_search(e, p, CWAL_TYPE_WEAKREF);
    if(r) r->value = NULL;
    return r ? 1 : 0;
  }
}


static int cwal_engine_init_interning(cwal_engine * e){
  cwal_ptr_table * t = &e->interned;
  /* notes to self regarding table size...

     The following gives me a really good fill rate for
     the low page (over 90%) for 500 random strings:
    
     entries= 3*1024/(2*sizeof(void*))
    
     hashSize= trim_hash(entriesPerPage*33/50)
    
     But lots of tables (9 on average, it seems)
    
  */
  uint16_t const entriesPerPage =
#if 0
    /* testing a leak of interned strings. */
    3
#elif 0
    1024 * 4 / sizeof(void*)
#elif 0
    281 /* mediocre */
#elif 1
    379 /* reasonable */
#elif 0
    503
#else
    /*cwal_trim_hash_size(200)*/
    /* not bad 240 */
    /* 281 seems to give a good size for 500-string tests. */
    512 * 5 / (sizeof(void*)) /* also not too bad, about 2.5kb/page (64-bit), low page count. */
    /* One test: with a table size of 3163 we got
       over 90% in the first hit and 99% in the
       second hit with 478 strings.
           
       With 433 entries/page we got about 58%
       in the 1st hit, 85 in the 2nd hit.

       After much experimentation, a starting page size of about
       550 seems to provide a good basis here. Increasing the size
       by 8 (later: 8 what?) doesn't give us many fewer tables and
       does cost a lot more memory.
    */
#endif
    ;
  uint16_t const hashSize =
    cwal_trim_hash_size(entriesPerPage);
  uint16_t const stepSize = 0 /* not actually used by this particular table */;
  /*MARKER("INTERNED STRING TABLE hashSize=%u stepSize=%u\n", hashSize, stepSize);*/
  return cwal_ptr_table_create(e, &t, hashSize, stepSize );
}



cwal_hash_t cwal_hash_bytes( void const * _zKey, cwal_size_t nKey ){
  unsigned char const * zKey = (unsigned char const *)_zKey;
#if 0
  /*
    FNV-xx. These both well for our typical inputs.  Marginally more
    collisions when using the 32-bit seeds on 64-bit arch.
  */
#  if CWAL_SIZE_T_BITS < 64
  cwal_hash_t const prime = 16777619U;
  cwal_hash_t const offset = 2166136261U;
#  else
  cwal_hash_t const prime = 1099511628211U;
  cwal_hash_t const offset = 14695981039346656037U;
#  endif
  cwal_size_t i;
  cwal_hash_t hash = offset;
  for( i = 0; i < nKey; ++i ){
#  if 0
    /* FNV-1 */
    hash = hash * prime;
    hash = hash ^ zKey[i];
#  else
    /* FNV-1a */
    /* Works a tick better than FNV-1 in my tests */
    hash = hash ^ zKey[i];
    hash = hash * prime;
#  endif
  }
  return hash;
  /* end FNV-1/1a */
#elif 0
  /*
    CRC32a: http://www.hackersdelight.org/hdcodetxt/crc.c.txt
  */
#  define reverse(x)                                        \
  x = ((x & 0x55555555) <<  1) | ((x >>  1) & 0x55555555);  \
  x = ((x & 0x33333333) <<  2) | ((x >>  2) & 0x33333333);  \
  x = ((x & 0x0F0F0F0F) <<  4) | ((x >>  4) & 0x0F0F0F0F);  \
  x = (x << 24) | ((x & 0xFF00) << 8) |                     \
    ((x >> 8) & 0xFF00) | (x >> 24)
  int i, j;
  cwal_hash_t byte, crc;
  cwal_size_t n;
  i = 0;
  crc = 0xFFFFFFFF;
  for( n = 0; n < nKey; ++n ){
    byte = (unsigned char)zKey[i];
    reverse(byte);
    for (j = 0; j <= 7; j++) {
      if ((int)(crc ^ byte) < 0)
        crc = (crc << 1) ^ 0x04C11DB7;
      else crc = crc << 1;
      byte = byte << 1;
    }
    i = i + 1;
  }
  crc = ~crc;
  reverse(crc);
  return crc;
#  undef reverse
#elif 0
  /*
    CRC32b: http://www.hackersdelight.org/hdcodetxt/crc.c.txt
  */
  int j;
  unsigned char byte;
  cwal_hash_t crc, mask;
  cwal_size_t n = 0;
  crc = 0xFFFFFFFF;
  for ( ; n < nKey; ++n) {
    byte = (unsigned char)zKey[n];
    crc = crc ^ byte;
    for (j = 7; j >= 0; j--) {
      mask = -(crc & 1);
      crc = (crc >> 1) ^ (0xEDB88320 & mask);
    }
  }
  return ~crc;
#elif 0
  /*
    CRC32e: http://www.hackersdelight.org/hdcodetxt/crc.c.txt

    HOLY COW: this collides about every 2nd string!

    Worst. Hash. Ever.

    Seriously, in the s2 unit tests: interning tables with space for
    379 entries each, holdling a total of 264 strings: 32 tables are
    needed with this hash!
  */
  int j;
  cwal_hash_t byte, c;
  cwal_int_t crc;
  cwal_size_t i;
  const unsigned int g0 = 0xEDB88320, g1 = g0 >> 1,
    g2 = g0 >> 2,    g3 = g0 >> 3;
#  if CWAL_SIZE_T_BITS < 64
  crc = 0xFFFFFFFF;
#  else
  crc = 0xFFFFFFFFFFFFFFFF;
#  endif
  for(i = 0; i < nKey; ++i ){
    byte = (unsigned char)zKey[i];
    crc = crc ^ byte;
    for (j = 1; j >= 0; j--) {
      c = ((crc<<31>>31) & g3) ^ ((crc<<30>>31) & g2) ^
        ((crc<<29>>31) & g1) ^ ((crc<<28>>31) & g0);
      crc = ((unsigned)crc >> 4) ^ c;
    }
    i = i + 1;
  }
  return (cwal_hash_t)~crc;
#elif 1
  /* another experiment... */
  /* 20181122: this one seems to perform the best for the s2 unit
     test suite, measured in terms of how many string interning
     tables get allocated and how tightly packed they get. */
  cwal_hash_t h = 0;
  unsigned char const * p = (unsigned char const *)zKey;
  cwal_size_t i = 0;
  /* This one performs a tick better than FNV-1a in my brief tests */
  for( ; i<nKey; ++i, ++p)
    h = 31 * h + (*p * 307); /* stackoverflow says 31 or 37 */
  return h;
#else
  /* several alternate implementations... */
  if(0 && (1==nKey)){
    return *zKey;
  }else if(0){
    /* This one isn't that bad for our purposes... */
    cwal_hash_t hash = 0 /*2166136261U*/;
    cwal_size_t i;
    for(i=0; i<nKey; ++i){
      hash = ((hash<<3) ^ hash) - (zKey[i]*117);
    }
    return hash;
  }else{
#  if 0
    /* FVN-xx. These both well for our typical inputs. */
#    if CWAL_SIZE_T_BITS < 64
    cwal_hash_t const prime = 16777619U;
    cwal_hash_t const offset = 2166136261U;
#    else
    cwal_hash_t const prime = 1099511628211U;
    cwal_hash_t const offset = 14695981039346656037U;
#    endif
    cwal_size_t i;
    cwal_hash_t hash = offset;
    for( i = 0; i < nKey; ++i ){
#    if 0
      /* FNV-1 */
      hash = hash * prime;
      hash = hash ^ zKey[i];
#    else
      /* FNV-1a */
      /* Works a tick better than FNV-1 in my tests */
      hash = hash ^ zKey[i];
      hash = hash * prime;
#    endif
    }
    return hash;
#  elif 0
    /* just a tiny tick faster than option #3. */
    /* this one (taken from th1) provides fairly unpredictable results
       in my tests, with +/-2 tables on any given run.
    */
    cwal_hash_t hash = 0;
    cwal_size_t i;
    for(i=0; i<nKey; i++){
      hash = (hash<<3) ^ hash ^ zKey[i];
    }
    return hash;
#  elif 0
    /* slow compared to options 1 and 3, even without the shift. */
    /* http://home.comcast.net/~bretm/hash/6.html */
    static const cwal_hash_t shift = 14 /* 0..31

14, 8 seem to work well here.

<8 is especially poor
                                        */;
    cwal_hash_t hash = 2166136261U /* this is only an unsigned 32-bit const in C90? */;
    cwal_size_t i = 0;
    for( ; (i<nKey); ++zKey, ++i ){
      hash = (hash * 16777619) ^ (unsigned char)*zKey;
    }
    if( ! shift ) return hash;
    else {
      cwal_hash_t mask =  (cwal_hash_t)((1U << shift) - 1U);
      return (hash ^ (hash >> shift)) & mask;
    }
#  elif 0
    /* This one seems to perform (hash-wise) pretty well, by just a tick,
       based on simple tests with the string interning table.
       It's just a tiny tick slower than option #1.

       This hash in combination with a table size of 547 performs
       quite well for my pseudo-scientific random-strings tests.

       Longer-term tests show it not to perform as well as FNV
       for s2's unit tests.
    */
    /* "djb2" algo code taken from: http://www.cse.yorku.ca/~oz/hash.html */
    static const cwal_hash_t seed = 5381;
    char const * vstr = (char const *)zKey;
    cwal_hash_t hash = seed;
    cwal_size_t i = 0;
    for( ; i<nKey; ++vstr, ++i )
    {
      if(0) hash = ((cwal_hash_t)(hash << 5) + hash) + *vstr;
      else hash = hash * 33 + *vstr;
    }
    return hash ? hash : seed;
#  elif 0
    /* "Modified Bernstein" algo, taken from:
       http://eternallyconfuzzled.com/tuts/algorithms/jsw_tut_hashing.aspx

       Seems to lead to marginally more collisions than the others.
    */
    unsigned char const * p = (unsigned char const *)zKey;
    cwal_hash_t h = 0;
    cwal_size_t i = 0;
    for( ; i < nKey; ++i )
    {
      h = 33 * h ^ p[i];
    }
    return h;
#  else
    pick_a_hash_algo;
    /*assert(!"Pick a hash algo!");*/
#  endif    
  }
#endif
  /* endless hash [algo] experimentation. */
}


int cwal_interned_search( cwal_engine * e,
                          char const * zKey,
                          cwal_size_t nKey,
                          cwal_value ** out,
                          cwal_ptr_page ** pageIndex,
                          uint16_t * itemIndex ){
  cwal_ptr_page * pPage;
  cwal_value * pRet = NULL;
  uint16_t iKey = 0;
  cwal_ptr_table *t;
  if(!e || !zKey) return CWAL_RC_MISUSE;
  t = &e->interned;
  if( !t->pg.head ) return CWAL_RC_NOT_FOUND;
  iKey = (uint16_t)(cwal_hash_bytes( zKey, nKey ) % t->hashSize);
  if(itemIndex) *itemIndex = iKey;
  for(pPage = t->pg.head; pPage; pPage = pPage->next ){
    pRet = (cwal_value*)pPage->list[iKey];
    if(!pRet) break /* end of the table list */;
    else if(pRet){
      cwal_string const * sRet = CWAL_STR(pRet);
      assert(sRet);
      if(sRet &&
         (CWAL_STRLEN(sRet) == nKey)
         && (0 == memcmp( cwal_string_cstr(sRet),
                          zKey, nKey ))){
        if(out) *out = pRet;
        if(pageIndex) *pageIndex = pPage;
        return CWAL_RC_OK;
      }
    }
  }
  return CWAL_RC_NOT_FOUND;
}

int cwal_interned_search_val( cwal_engine * e,
                              cwal_value const * v,
                              cwal_value ** out,
                              cwal_ptr_page ** pageIndex,
                              uint16_t * itemIndex ){
  cwal_ptr_page * pPage = NULL;
  cwal_value * pRet = NULL;
  uint16_t iKey = 0;
  cwal_ptr_table *t;
  if(!e || !v) return CWAL_RC_MISUSE;
  t = &e->interned;
  if( !t->pg.head ) return CWAL_RC_NOT_FOUND;
  iKey = (uint16_t)(v->vtab->hash(v) % t->hashSize);
  if(itemIndex) *itemIndex = iKey;
  for(pPage = t->pg.head; pPage; pPage = pPage->next ){
    pRet = (cwal_value*)pPage->list[iKey];
    if(!pRet) break /* end of the table list for this hash */;
    else if(pRet){
      if((v==pRet)
         || ((v->vtab == pRet->vtab /* enforce strict type comparisons */)
             && (0 == v->vtab->compare(v, pRet)))){
        if(out) *out = pRet;
        if(pageIndex) *pageIndex = pPage;
        return CWAL_RC_OK;
      }
    }
  }
  return CWAL_RC_NOT_FOUND;
}

    
int cwal_interned_insert( cwal_engine * e, cwal_value * v ){
  cwal_ptr_page * pPage = NULL;
  cwal_ptr_page * pageIndex = NULL;
  cwal_value * pRet = NULL;
  uint16_t iKey = 0;
  int rc;
  cwal_ptr_table * t;
  if(!(CWAL_FEATURE_INTERN_STRINGS & e->flags)) return CWAL_RC_UNSUPPORTED;
  else if(!e || !v) return CWAL_RC_MISUSE;
  t = &e->interned;
  if(!t->pg.head){
    rc = cwal_engine_init_interning( e );
    if(rc) return rc;
    assert(t->pg.head);
  }

  rc = cwal_interned_search_val( e, v, &pRet, &pageIndex, &iKey );
  if( pRet ) {
    assert(pageIndex);
    assert( (pRet == v) && "i'm fairly sure this holds with the current code.");
    return CWAL_RC_ALREADY_EXISTS;
  }
  assert(!pageIndex);
    
  /**
     Search just failed, so we need to add an entry.  Check if one
     of our existing pages has a slot...
  */
  for(pPage = t->pg.head; pPage; pPage = pPage->next ){
    pRet = pPage->list[iKey];
    if(!pRet) goto insert;
  }
  if(!pPage){
    /* We reached the end of the table and found no empty slot
       (maybe had a collision). Add a page and insert the value
       there. */
    rc = cwal_ptr_table_add_page( e, t );
    if(rc) return rc;
    pPage = t->pg.tail;
  }
  insert:
  rc = CWAL_RC_OK;
  assert(NULL != pPage);
  assert(!pPage->list[iKey]);
  pPage->list[iKey] = v;
  CWAL_TR_VCS(e,v);
  CWAL_TR2(e,CWAL_TRACE_VALUE_INTERNED);
  ++pPage->entryCount;
  return rc;
}

int cwal_interned_remove( cwal_engine * e,
                          cwal_value const * v,
                          cwal_value ** out ){
  cwal_ptr_page * pPage = 0;
  cwal_value * pRet = NULL;
  cwal_ptr_table * t;
  uint16_t iKey = 0;
  cwal_ptr_page * pageIndex = NULL;
  int rc;
  if(!e || !v) return CWAL_RC_MISUSE;
  t = &e->interned;
  if(!t->pg.head) return CWAL_RC_NOT_FOUND;

  rc = cwal_interned_search_val( e, v, &pRet, &pageIndex, &iKey );
  if( !pRet ) {
    assert( CWAL_RC_NOT_FOUND == rc );
    return rc;
  }
  else if(rc){
    assert(!"Cannot happen");
    return rc;
  }
  if( out ) *out = pRet;
  CWAL_TR_VCS(e,pRet);
  CWAL_TR2(e,CWAL_TRACE_VALUE_UNINTERNED);
  assert( pageIndex );
  for( pPage = pageIndex; pPage; pPage = pPage->next ){
    /* Remove the entry. If any higher-level pages contain that
       key, move it into this page (up through to the top-most
       page which has a matching key).
    */
    pPage->list[iKey] = pPage->next
      ? pPage->next->list[iKey]
      : 0;
    if( !pPage->list[iKey] ) {
      --pPage->entryCount;
      break;
    }
  }
  assert( pPage && (0 == pPage->list[iKey]) );
  return CWAL_RC_OK;
}

cwal_obase * cwal_value_obase( cwal_value * const v ){
  return CWAL_VOBASE(v);
}


void cwal_value_cleanup_noop( cwal_engine * e, void * v ){
  if(e || v){/*avoid unused param warning*/}
}

void cwal_value_cleanup_integer( cwal_engine * e, void * self ){
#if 1
  /* Only disable this block when chasing bugs. */
  cwal_value * v = (cwal_value *)self;
  *CWAL_INT(v) = 0;
#endif
  if(e){/*avoid unused param warning*/}
}

void cwal_value_cleanup_double( cwal_engine * e, void * self ){
  static const cwal_double_t zero = 0.0;
  cwal_value * v = (cwal_value *)self;
  memcpy(CWAL_DBL_NONULL(v), &zero, sizeof(cwal_double_t));
  if(e){/*avoid unused param warning*/}
}


void cwal_value_cleanup_unique( cwal_engine * e, void * self ){
  cwal_value * v = (cwal_value *)self;
  cwal_value ** wrapped = CWAL_UNIQUE_VALPP(v);
  if(e){/*avoid unused param warning*/}
  assert(wrapped);
  if(*wrapped){
    cwal_value * w = *wrapped;
    *wrapped = 0;
    if(!CWAL_V_IS_IN_CLEANUP(w)){
      /*
        Without this is-in-cleanup check, code like the
        following (s2) crashes here during scope cleanup:

        var e = enum {a:{x:1},b:{x:2}};
        e.a.value.b = e.b;
        e.b.value.a = e.a;

        20180105: a string key and its Unique-type wrapper were
        being cleaned up during scope cleanup and the string got
        recycled (as one does) during that process before the
        Unique wrapper did. That ended up triggering the
        assert() below. As of 20180105, the cleanup process
        gc-queues all values, not just containers, which
        resolves this case. It's just amazing that it didn't
        trigger sooner.

        Addenda: the root cause, it turns out, is that Uniques
        and Tuples live in the cwal_scope::mine::headPod list,
        not headObj, and can therefore be destroyed after their
        values are.
      */
      assert( CWAL_REFCOUNT(w) || CWAL_MEM_IS_BUILTIN(w) );
      cwal_value_unref(w);
    }
  }
}

void cwal_value_cleanup_tuple( cwal_engine * e, void * self ){
  cwal_value * v = (cwal_value *)self;
  cwal_tuple * p = CWAL_TUPLE(v);
  cwal_size_t i;
  assert(p && p->n);
  for( i = 0; i < p->n; ++i ){
    /* hashtag CWAL_V_GOES_IN_HEADOBJ */
    if(!CWAL_V_IS_IN_CLEANUP(p->list[i])){
      /* ^^^ cleanup check is hypothetically needed for certain
         constellations because Tuples are managed via the
         cwal_scope::headPod list, not headObj. That said, it's
         never been seen to trigger a problem before (probably
         because tuples are seldom used?).*/
      cwal_value_unref(p->list[i]);
    }
    p->list[i] = 0;
  }
  cwal_free2(e, p->list, sizeof(cwal_value*) * p->n);
  *p = cwal_tuple_empty;
}

void cwal_value_cleanup_propref( cwal_engine * e, void * V ){
  cwal_value * const v = (cwal_value *)V;
  cwal_propref * const r = CWAL__V2PROPREF(v);
  if(r->key) {
    cwal_value_unref2(e, r->key);
    r->key = NULL;
  }
  if(CWAL__PROPREF_MASK_WEAK & r->flags){
    if(r->target.w) {
      cwal_weakref_free(e, r->target.w);
      r->target.w = NULL;
    }
  }else if(r->target.c) {
    cwal_value_unref2(e, r->target.c);
    r->target.c = NULL;
  }
  if(r->xtra) {
    cwal_value_unref2(e, r->xtra);
    r->xtra = NULL;
  }
}

int cwal_rescope_children_propref( cwal_value * v ){
  int rc = 0;
  cwal_propref * const r = CWAL__V2PROPREF(v);
  assert(r);
  assert(v->scope);
  if(r->key){
    rc = cwal_value_xscope( v->scope->e, v->scope, r->key, 0 );
  }
  if(0==rc){
    // weak refs must not affect target lifetime.
    if(0==(CWAL__PROPREF_MASK_WEAK & r->flags) && r->target.c){
      rc = cwal_value_xscope(v->scope->e, v->scope, r->target.c, NULL);
    }
  }
  if(0==rc && r->xtra){
    rc = cwal_value_xscope( v->scope->e, v->scope, r->xtra, NULL );
  }
  return rc;
}


cwal_type_id cwal_value_type_id( cwal_value const * v ){
  return (v && v->vtab) ? v->vtab->typeID : CWAL_TYPE_UNDEF;
}

cwal_value_type_name_proxy_f cwal_engine_type_name_proxy( cwal_engine * e,
                                                          cwal_value_type_name_proxy_f f ){
  cwal_value_type_name_proxy_f rc = e ? e->type_name_proxy : 0;
  if(e) e->type_name_proxy = f;
  return rc;
}

char const * cwal_value_type_name2( cwal_value const * v,
                                    cwal_size_t * len){
  if(!v || !v->vtab) return NULL;
  else{
    cwal_engine const * e = v->scope ? v->scope->e : NULL;
    char const * rc = NULL;
    if(e && e->type_name_proxy){
      rc = e->type_name_proxy(v, len);
    }
    if(!rc){
      rc = v->vtab->typeName;
      if(rc && len) *len = cwal_strlen(rc);
    }
    return rc;
  }
}

char const * cwal_value_type_name( cwal_value const * v ){
  return v ? cwal_value_type_name2(v, NULL) : NULL;
}

char const * cwal_type_id_name( cwal_type_id id ){
  switch(id){
    default: break;
    case CWAL_TYPE_SCOPE: return "cwal_scope";
    case CWAL_TYPE_KVP: return "cwal_kvp";
    case CWAL_TYPE_WEAKREF: return "weakref";
    case CWAL_TYPE_XSTRING: return "x-string";
    case CWAL_TYPE_ZSTRING: return "z-string";
    case CWAL_TYPE_LISTMEM: return "cwal_list";
  }
  cwal_value_vtab const * t = cwal__value_vtab(id);
  return t ? t->typeName : NULL;
}

bool cwal_value_is_undef( cwal_value const * v ){
  return ( !v || !v->vtab || (v->vtab==&cwal_value_vtab_undef))
    ? 1 : 0;
}

#define ISA(T,TID) bool cwal_value_is_##T( cwal_value const * v ) {     \
    /*return (v && v->vtab) ? cwal_value_is_a(v,CWAL_TYPE_##TID) : 0;*/ \
  return v ? v->vtab == cwal__value_vtab(TID) : false;            \
  }

  ISA(null,CWAL_TYPE_NULL)
  ISA(bool,CWAL_TYPE_BOOL)
  ISA(integer,CWAL_TYPE_INTEGER)
  ISA(double,CWAL_TYPE_DOUBLE)
  ISA(string,CWAL_TYPE_STRING)
  ISA(array,CWAL_TYPE_ARRAY)
  ISA(object,CWAL_TYPE_OBJECT)
  ISA(native,CWAL_TYPE_NATIVE)
/* ISA(buffer,BUFFER) */
  ISA(function,CWAL_TYPE_FUNCTION)
  ISA(exception,CWAL_TYPE_EXCEPTION)
  ISA(hash,CWAL_TYPE_HASH)
  ISA(propref,CWAL_TYPE_PROPREF)
  ISA(unique,CWAL_TYPE_UNIQUE)
  ISA(tuple,CWAL_TYPE_TUPLE)
#undef ISA


  bool cwal_value_is_buffer( cwal_value const * v ){
  cwal_buffer_obj const * bo = CWAL_BUFOBJ(v);
  assert(bo ? (bo==bo->buf.self) : 1);
  return (bo && bo == bo->buf.self) ? 1 : 0;
}

bool cwal_value_is_number( cwal_value const * v ){
  if(!v) return 0;
  else switch(v->vtab->typeID){
      case CWAL_TYPE_INTEGER:
      case CWAL_TYPE_DOUBLE:
      case CWAL_TYPE_BOOL:
        return 1;
      default:
        return 0;
    }
}


void cwal_finalizer_f_fclose( cwal_engine * e, void * m ){
  if(e){/*avoid unused param warning*/}
  if(m && (m != stdout) && (m != stderr) && (m != stdin)){
    fclose( (FILE*)m );
  }
}

void * cwal_realloc_f_std( void * state, void * m, cwal_size_t n ){
  if( 0 == n ){
    free( m );
    return NULL;
  }else if( !m ){
    return malloc( n );
  }else{
    if(state){/*avoid unused param warning*/}
    return realloc( m, n );
  }
}

int cwal_output_f_FILE( void * state, void const * src, cwal_size_t n ){
  if( !state || !src || !n ) return 0;
  return (1 == fwrite( src, n, 1, state ? (FILE*)state : stdout ))
    ? CWAL_RC_OK
    : CWAL_RC_IO;
}

int cwal_output_flush_f_FILE( void * f ){
  return fflush(f ? (FILE*)f : stdout)
    ? CWAL_RC_IO
    : 0
    ;
}

int cwal_output_f_buffer( void * state, void const * src, cwal_size_t n ){
  cwal_output_buffer_state * ob = (cwal_output_buffer_state*) state;
#if 1
  return cwal_buffer_append(ob->e, ob->b, src, n);
#else
  int rc = 0;
  cwal_output_buffer_state * ob = (cwal_output_buffer_state *)state_;
  cwal_size_t sz = ob->b->used+n+1;
  if(sz > ob->b->capacity){
    /* TODO? expand by some factor */
    /* sz = sz * 13 / 10; */
    rc = cwal_buffer_reserve(ob->e, ob->b, sz);
  }
  if(!rc){
    rc = cwal_buffer_append( ob->e, ob->b, n );
  }
  return rc;
#endif
}

void cwal_output_buffer_finalizer( cwal_engine * e, void * m ){
  cwal_output_buffer_state * ob = (cwal_output_buffer_state *)m;
  assert(ob->e == e);
  cwal_buffer_reserve(e, ob->b, 0);
  ob->e = NULL;
  *ob = cwal_output_buffer_state_empty;
}

void * cwal_malloc( cwal_engine * e, cwal_size_t n ){
  unsigned char * rc = 0;
  cwal_size_t const origN = n;
  if(!e || !n || !e->vtab) return 0;
  n = CWAL_MEMSZ_PAD(n);
  if(CWAL_F_TRACK_MEM_SIZE & e->flags){
    n += sizeof(void*)
      /* To store the size in. Must be generically aligned, thus
         we don't use sizeof(uint32_t). */;
#if 64==CWAL_SIZE_T_BITS
    if(n > 0xFFFFFFFF){
      /* b/c our size marker is explicitly uint32_t. */
      return NULL;
    }
#endif
  }
  CWAL_TR_MEM(e,rc,n);
  if(n < origN) return NULL /* overflow after adjustment */;
  else if(/* Check cap constraints... */
          /* too-large single alloc */
          (e->vtab->memcap.maxSingleAllocSize
           && (n > e->vtab->memcap.maxSingleAllocSize)) 
          || /* too many concurrent allocs */
          (e->vtab->memcap.maxConcurrentAllocCount
           && (e->memcap.currentAllocCount
               >= e->vtab->memcap.maxConcurrentAllocCount))
          || /* too much concurrent memory */
          (e->vtab->memcap.maxConcurrentMem
           && (e->memcap.currentMem + n
               > e->vtab->memcap.maxConcurrentMem))
          || /* too many total allocs */
          (e->vtab->memcap.maxTotalAllocCount
           && (e->memcap.totalAllocCount
               >= e->vtab->memcap.maxTotalAllocCount))
          || /* Too much total memory... */
          (e->vtab->memcap.maxTotalMem
           && (e->memcap.currentMem + n
               > e->vtab->memcap.maxTotalMem))
          ){
    return 0;
  }
  rc = (unsigned char *)
    e->vtab->allocator.realloc( e->vtab->allocator.state.data, NULL, n );
  if(rc){
    if( ++e->memcap.currentAllocCount > e->memcap.peakAllocCount ){
      e->memcap.peakAllocCount =
        e->memcap.currentAllocCount;
    }
    e->memcap.totalMem += n;
    ++e->memcap.totalAllocCount;
    if(CWAL_F_TRACK_MEM_SIZE & e->flags){
      /* Stamp the size and adjust rc. */
      cwal_memsize_t * sz = (cwal_memsize_t*)rc;
      *sz = (cwal_memsize_t)n;
      rc += sizeof(void*);
      e->memcap.currentMem += n
        /* We can only decrement this if overallocating, so
           only increment if we overallocate. */;
      if(e->memcap.peakMem < e->memcap.currentMem){
        e->memcap.peakMem = e->memcap.currentMem;
      }
    }
  }
  CWAL_TR2(e,CWAL_TRACE_MEM_MALLOC);
  return rc;
}

void * cwal_malloc2( cwal_engine * e, cwal_size_t n ){
  cwal_size_t newN = n;
  void * rc = cwal_memchunk_request(e, &newN,
                                    (CWAL_F_TRACK_MEM_SIZE & e->flags)
                                    ? 150 : 125
                                    /* If over-allocation/tracking
                                       is on, we can recover all
                                       slack bytes when the mem is
                                       passed to cwal_free2(), so
                                       allow more leeway in the size
                                       of the recycled chunk. */,
                                    "cwal_malloc2()");
  if(!rc){
    rc = cwal_malloc(e, n);
  }
  return rc;
}

        
void cwal_free( cwal_engine * e, void * m ){
  if(e && m){
    assert(e != m /* check for this corner case: e is allocated
                     before cwal_malloc() and friends are
                     configured, so e was allocated on the stack or
                     before CWAL_F_TRACK_MEM_SIZE could take
                     effect. */);
    if(CWAL_F_TRACK_MEM_SIZE & e->flags){
      cwal_memsize_t * sz = MEMSZ_PTR_FROM_MEM(m);
      assert(e->memcap.currentMem >= *sz);
      e->memcap.currentMem -= *sz;
      m = sz;
    }
    CWAL_TR_MEM(e,m,0);
    CWAL_TR2(e,CWAL_TRACE_MEM_FREE);
    e->vtab->allocator.realloc( e->vtab->allocator.state.data, m, 0 );
    --e->memcap.currentAllocCount;
  }
}

void cwal_free2(cwal_engine * e, void * mem, cwal_size_t size ){
  assert(e);
  if(mem){
    assert(e != mem /* check for this corner case: e is allocated
                       before cwal_malloc() and friends are
                       configured, so e was allocated on the stack or
                       before CWAL_F_TRACK_MEM_SIZE could take
                       effect. */);
    if(size) cwal_memchunk_add(e, mem, size);
    else cwal_free(e, mem);
  }
}

void * cwal_realloc( cwal_engine * e, void * m, cwal_size_t n ){
  if(!e || !e->vtab) return 0;
  else if( 0 == n ){
    cwal_free( e, m );
    return NULL;
  }else if( !m ){
    return cwal_malloc( e, n );
  }else{
    unsigned char * rc;
    uint32_t oldSize = 0;
    cwal_size_t const origN = n;
    n = CWAL_MEMSZ_PAD(n);
    if(CWAL_F_TRACK_MEM_SIZE & e->flags){
      cwal_memsize_t * sz = MEMSZ_PTR_FROM_MEM(m);
      oldSize = *sz;
      assert(oldSize>0);
      assert(e->memcap.currentMem >= oldSize);
      m = sz;
      n += sizeof(void*);
#if 64==CWAL_SIZE_T_BITS
      if(n > 0xFFFFFFFF){
        /* b/c our size marker is explicitly uint32_t. */
        return NULL;
      }
#endif
    }
    CWAL_TR_MEM(e,m,n);
    CWAL_TR2(e,CWAL_TRACE_MEM_REALLOC);
    if(n < origN) return NULL /* overflow after adjustment */;
    else if(/* Allocation is too big... */
            (e->vtab->memcap.maxSingleAllocSize
             && (n > e->vtab->memcap.maxSingleAllocSize))
            || /* Too much concurrent memory... */
            (e->vtab->memcap.maxConcurrentMem
             && (e->memcap.currentMem - oldSize + n
                 > e->vtab->memcap.maxConcurrentMem))
            || /* Too much total memory... */
            (e->vtab->memcap.maxTotalMem
             && (e->memcap.currentMem + n
                 > e->vtab->memcap.maxTotalMem))
            ){
      rc = NULL;
    }else{
      rc = (unsigned char*)
        e->vtab->allocator.realloc( e->vtab->allocator.state.data,
                                    m, n );
      if(rc && (CWAL_F_TRACK_MEM_SIZE & e->flags)){
        /* update e->memcap, re-stamp memory size */
        cwal_memsize_t * sz = (cwal_memsize_t*)rc;
        *sz = n;
        rc += sizeof(void*);
        e->memcap.currentMem -= oldSize;
        e->memcap.currentMem += n;
      }
    }
    return rc;
  }
}

/**
   Sweeps up any values owned by s which have a refcount of 0.
   Returns the number of values swept up.
*/
static cwal_midsize_t cwal_scope_sweep_r0( cwal_scope * s ){
  cwal_engine * const e = s->e;
  cwal_value * v;
  cwal_midsize_t rc = 0;
  assert(e);
  CWAL_TR_MSG(e,"s->mine.r0:");
  CWAL_TR_SV(e,s,s->mine.r0);
  CWAL_TR2(e,CWAL_TRACE_SCOPE_MASK);
  while( (v = s->mine.r0) ){
    if(v->scope!=s) {
      dump_val(v,"Check for scope mismatch");
      assert(!v->left);
      if(v->right) dump_val(v->right,"v->right");
    }
    assert(v->scope==s);
    assert(0==CWAL_REFCOUNT(v) && "Else it wouldn't be in s->mine.r0.");
    /* The "special" propagating values always get a reference,
       and therefore cannot be in the r0 list. */
    assert(e->values.exception != v);
    assert(e->values.propagating != v);
    cwal_value_unref2(e,v);
    ++rc;
  }
  return rc;
}

cwal_midsize_t cwal_engine_sweep2( cwal_engine * e, bool allScopes ){
  if(!e) return 0;
  else if(!allScopes){
    return cwal_scope_sweep_r0(e->current);
  }
  else {
    cwal_scope * s = e->current;
    cwal_midsize_t rc = 0;
    for( ; s; s = s->parent ){
      rc += cwal_scope_sweep_r0(s);
    }
    return rc;
  }
}

cwal_midsize_t cwal_engine_sweep( cwal_engine * e ){
  return (e && e->current)
    ? cwal_scope_sweep_r0(e->current)
    : 0;
}

cwal_midsize_t cwal_scope_sweep( cwal_scope * s ){
  return (s && s->e)
    ? cwal_scope_sweep_r0(s)
    : 0;
}

/**
   calls cwal_value_snip() to snip v from any chain and moves v to the
   head of one of s->mine's members (depending on a couple of
   factors). Does not modify refcount.

   Reminder to self: this is also used to move v around within
   s->mine's various lists, so v->scope may be s.

   It only returns non-0 if it sets s->e->fatalCode.
*/
static int cwal_scope_insert( cwal_scope * const s, cwal_value * const v );

/**
   Removes v from its owning scope and places it in s->mine.r0,
   putting it back in the probationary state.  v MUST have a refcount
   of 0 and CWAL_MEM_IS_BUILTIN(v) MUST be false. s MUST be the scope to
   reprobate v to.
*/
static void cwal_value_reprobate( cwal_scope * const s, cwal_value * const v){
  /* int rc; */
  assert(v);
  assert(!CWAL_REFCOUNT(v));
  assert(!CWAL_MEM_IS_BUILTIN(v));
  assert(s);
  assert(s->e);
  /* dump_val(v,"Re-probating value"); */
  cwal_scope_insert(s, v);
  /* dump_val(v,"Re-probated value"); */
}

/**
   "Snips" v from its left/right neighbors.  If v->scope and v is the
   head of one of v->scope->mine's ownership lists then the list is
   adjusted to point to v->left (if set) or v->right. Also sets
   v->scope to 0.

   Returns the right-hand neighbor of v, or 0 if it has no neighbor.
*/
static cwal_value * cwal_value_snip( cwal_value * v );

/**
   Internal helper to move a refcount==0 value from the r0 list to one
   of the "longer-lived" lists. Returns 0 on success.

   CWAL_REFCOUNT(v) must be 1 (not 0) when this is called, and must have
   just gone from 0 to 1, as opposed to going from 2 to 1.
*/
static int cwal_scope_from_r0( cwal_value * v ){
  cwal_scope * s = v->scope;
  assert(1==CWAL_REFCOUNT(v));
  if(!s->mine.r0) return CWAL_RC_NOT_FOUND;
  else if(1!=CWAL_REFCOUNT(v)) return CWAL_RC_RANGE
                                 /* Only to ensure that caller ++'s it before calling this, so
                                    that the cwal_scope_insert() call below can DTRT. */
                                 ;
  cwal_scope_insert( s, v ) /* does the list shuffling */;
  if(E_IS_DEAD(s->e)) return s->e->fatalCode;
  assert(v->scope == s);
  return 0;
}

/**
   Returns the number of values in s->mine's various lists. An O(N)
   operation, N being the number returned (not incidentally).
*/
static cwal_size_t cwal__scope_value_count( cwal_scope const * s ){
  cwal_size_t n = 0;
  cwal_value const * v;
#  define VCOUNT(WHO) v = WHO; while(v){++n; v=v->right;} (void)0
  VCOUNT(s->mine.headPod);
  VCOUNT(s->mine.headObj);
  VCOUNT(s->mine.headSafe);
  VCOUNT(s->mine.r0);
#undef VCOUNT
  return n;
}

/**
   This function frees the internal state of s but does not free s.
   If any of the specially-propagating values live in s, they are
   re-scoped/moved to s's parent unless s is the top-most scope. in
   which case they get cleaned up.
*/
int cwal_scope_clean( cwal_engine * e, cwal_scope * s ){
  int rc = 0;
  cwal_value * v;
  bool iInitedGc = false;
  if(!e->gcInitiator) {
    iInitedGc = true;
    e->gcInitiator = s;
    CWAL_TR_S(e,s);
    CWAL_TR3(e,CWAL_TRACE_MESSAGE,"Initializing gc capture.");
  }

  CWAL_TR_S(e,s);
  CWAL_TR3(e,CWAL_TRACE_SCOPE_CLEAN_START,"Scope cleanup starting.");
  /* prohibits vacuum: assert(e->current != s); */
  /* Special treatment of e->values.exception and e->values.propagating. */
  {
    cwal_value * vPropagate;
    int phase = 1;
    propagate_next:
    vPropagate = (1==phase ? e->values.exception : e->values.propagating);
    if(vPropagate && vPropagate->scope == s){
      cwal_scope * const parent = (s==e->current /* vacuum op */) ? s->parent : e->current;
      CWAL_TR_SV(e,s,vPropagate);
      CWAL_TR3(e,CWAL_TRACE_SCOPE_MASK,"Relocating vPropagate...");
      if(!parent){
        if(1==phase) cwal_exception_set(e, NULL);
        else cwal_propagating_set(e, NULL);
        assert((1==phase) ? NULL==e->values.exception : NULL==e->values.propagating);
      }else{
        rc = cwal_value_xscope( e, parent, vPropagate, NULL );
        /* we really should soldier on and clean up, but
           we'd be doing so on possibly corrupt memory!

           20200111: that said, this operation has never
           failed in practice. */
        if(rc){
          assert(rc == e->fatalCode);
          cwal__fatal(rc, "The xscope operation is no longer supposed "
                      "to be able to fail. rc=%s", cwal_rc_cstr(rc));
          return e->fatalCode/*not reached*/;
        }
        assert(!rc && "Cannot fail anymore?");
        assert(vPropagate && (vPropagate->scope!=s));
        CWAL_TR_SV(e,vPropagate->scope,vPropagate);
        CWAL_TR3(e,CWAL_TRACE_SCOPE_MASK,"Moved EXCEPTION/PROPAGATING to next scope up.");
      }
    }
    if(2==++phase) goto propagate_next;
  }

  if(s->props){
    cwal_value * const pv = s->props;
    cwal_obase * const obase = CWAL_VOBASE(pv);
    assert(pv);
    assert(obase);
    assert(pv->scope && "It's not possible to have no scope at this point.");
    assert((pv->scope==s || (pv->scope->level<s->level))
           && "Scoping/lifetime precondition violation.");
    assert((CWAL_REFCOUNT(pv)>0) && "Who stole my properties ref?");
    /*
      If CWAL_REFCOUNT(pv)>1 then we know that "someone" still
      holds a reference.  During/after scope cleanup, using such a
      reference is illegal, so we don't really care about
      that. What we do care about is if...

      If we set the vacuum-safe flag on pv, we also need to turn
      off the flag, but...  If a client gets a handle to pv and
      explicitely sets it to vacuum-safe then we can no longer
      know (without a new flag) that we are the ones who set the
      vacuum-proofing flag, so unsetting it here "could be bad."
      One solution would be to use another flag bit in cwal_obase
      to mark property storage objects as being so, and unmark
      them if they are ever removed from their initial scope. It
      is possible/legal that an older scope holds a reference to
      pv, and in that case pv->scope!=s. So we can use that info
      to determine whether this scope really owns pv or not, which
      may help us do...  something or other useful...

      So CWAL_F_IS_PROP_STORAGE is born...

      2022-02-26 reminder to self: we genuinely do need the
      CWAL_F_IS_PROP_STORAGE flag in order to keep s->props
      vacuum-safe in the case that s->props is upscoped while s is
      still alive and that older scope ever gets vacuumed while s is
      still alive. Without this flag, or something equivalent, the
      older scope would vacuum up s->props.
    */
    assert(obase->flags & CWAL_F_IS_PROP_STORAGE);
    obase->flags &= ~CWAL_F_IS_PROP_STORAGE;
    s->props = 0 /* if pv->scope==s, pv is in s->mine,
                    otherwise pv is in pv->scope->mine. Either way,
                    it's where it needs to be right now. */;
    cwal_value_unref(pv);
  }

  if(e->values.prototypes && (s == CWAL_VALPART(e->values.prototypes)->scope)){
    /* cwal_value_unhand(CWAL_VALPART(e->values.prototypes)); */
    e->values.prototypes = 0 /* it's in s->mine somewhere,
                                and will be cleaned up
                                momentarily. */;
  }

  cwal_scope_sweep_r0( s );

  /**
     Reminder: we HAVE to clean up the containers first to ensure
     that cleanup of PODs during container cleanup does not step
     on dead POD refs we already cleaned. We could get around this
     ordering if we included PODs in the gc queue, but we do not
     need to, so we don't. (20180105: we gc-queue PODs now.)

     Algorith: keep reducing each value's refcount by 1 until it's
     dead. This weeds out cycles one step at a time.

     Notes:

     For PODs we REALLY want to unref them only once here, BUT
     internalized strings screw that up for us (but are too cool to
     outright give up, yet i also don't want to special-case them).

     The destruction order is not QUITE what i want (i would prefer
     reverse-allocation order, but we cannot guaranty that ordering
     once objects move between scopes, anyway). What we're doing
     here is unref'ing the head (last-added) item. If that item
     still has references (it was not destroyed) then we proceed to
     unref subsequent items in the list until one is destroyed.
     Value destruction modifies the list we are traversing, forcing
     a re-start of the traversal if any item is actually finalized
     by the unref. As values are cleaned up they remove themselves
     from s->mine, so we can simply walk the list until it's
     empty. For "normal use" the destruction order will be the
     referse of allocation, but once references are held that
     doesn't...  well, hold.

     Remember that this ONLY works because of our scoping rules:

     - All values in a scope when it is cleaned up must not (cannot)
     reference values in higher (newer) scopes because performing
     such a reference transfers the being-referenced value
     (recursively for containers) into the referencing value's
     owning scope.
  */
  while((v = s->mine.headObj
         ? s->mine.headObj
         : (s->mine.headSafe
            ? s->mine.headSafe
            : s->mine.headPod)
         )){
    CWAL_TR_SV(e,s,v);
    CWAL_TR_MSG(e,"Scope is about to unref value");
    CWAL_TR2(e,CWAL_TRACE_SCOPE_MASK);
    assert(!CWAL_MEM_IS_BUILTIN(v));
    assert(v->scope);
    assert(v->scope==s);
    assert(v->right ? v->right->scope==s : 1);
    while(v){
      cwal_value * n = v->right;
      assert(n != v);
      assert(!n || CWAL_REFCOUNT(n)>0);
      assert(!n || n->scope == s);
      if( CWAL_RC_HAS_REFERENCES == cwal_value_unref2(e, v) ) {
        /*
          It still has references. Let's try again.

          This is part of the reason the gc queue is so
          important/helpful. Consider what happens when we
          clean up a Prototype value (which may have hundreds
          or thousands of references to it).  We may clean it
          up before some of the objects which reference it
          because of this "repeat if it survives" behaviour.
        */
        assert(CWAL_REFCOUNT(v)>0);
        v = n;
        assert((!n || s == n->scope) && "unexpected. Quite.");
        continue;
        /* break; */
      }
      else if(n && n->scope){
        assert((s == n->scope) && "This is still true in a vacuum, right?");
        /* n is still in THIS list, else n is in the gc
           queue and we need to re-start traversal.

           The destruction of v can only affect n if v is a
           container. a POD cannot form a reference to anyone
           else, so if we're here then we know that either:

           - v was a POD before unref'ing it.

           - OR v was a container which did not (even
           indirectly) reference n. Had it cleaned up n,
           n->scope would be 0.
        */
        v = n;
        continue;
      }
      else{
        /*
          Need to restart traversal due to either:

          - v being finalized (which modifies this list).

          - n being in the gc queue or recycle bin (having
          been put there when we unref'd v, which held the
          only reference(s) (possibly indirectly) to n).
          We detect this case by checking whether n has a scope.
          If it has no scope, it's in the gc queue. Because
          PODs don't gc-queue (they don't have to because they
          cannot reference anything) all this funkiness only
          applies to containers.
        */
        break;
      }
    }
  }

  assert(0 == s->mine.headPod);
  assert(0 == s->mine.headObj);
  assert(0 == s->mine.headSafe);
  assert(0 == s->mine.r0);
  CWAL_TR3(e,CWAL_TRACE_SCOPE_CLEAN_END,"Scope cleanup finished.");
  /*MARKER("ALLOCS LIST SIZES: compound=%u simple=%u\n", s->mine.compound.count, s->mine.simple.count );*/

  if(iInitedGc){
    assert(s == e->gcInitiator);
    if(s == e->gcInitiator) {
      e->gcInitiator = 0;
      cwal_gc_flush( e );
    }
  }
  return rc;
}

static void cwal_scope_free( cwal_engine * e, cwal_scope * s, char allowRecycle ){
  void const * stamp;
  assert( e && s );
  stamp =  s->allocStamp;
  s->flags |= CWAL_F_IS_DESTRUCTING;
  cwal_scope_clean(e, s);
  *s = cwal_scope_empty;
  if(CwalConsts.AllocStamp == stamp){
    /* This API allocated the scope - recycle or free it. */
    cwal_recycler * re = cwal_recycler_get(e, CWAL_TYPE_SCOPE);
    assert(re);
    if( allowRecycle && (re->count < re->maxLength) ){
      s->parent = (cwal_scope*)re->list;
      re->list = s;
      ++re->count;
    }
    else cwal_free(e, s);
  }else{
    /* it was allocated elsewhere */
    s->allocStamp = stamp;
  }
}

int cwal_exception_info_clear( cwal_engine * e, cwal_exception_info * err ){
  if(!e || !err) return CWAL_RC_MISUSE;
  else{
    int rc = CWAL_RC_OK;
    if( err->zMsg ) cwal_free( e, err->zMsg );
    if( err->value ) rc = cwal_value_unref2( e, err->value );
    if(CWAL_RC_DESTRUCTION_RUNNING == rc) {
      assert( 0 && "i don't _think_ this can happen." );
      rc = 0;
    }
    assert(0 == rc);
    *err = cwal_exception_info_empty;
    return CWAL_RC_OK;
  }
}

/**
   Passes all values in the given linked value list to cwal_free().
   The values MUST have been properly cleaned up via the
   cwal_unref() mechanism.
*/       
static void cwal_value_list_free( cwal_engine * e, cwal_value * list){
  cwal_value * v, * next;
  for( v = list; v; v = next ){
    assert(0==v->scope);
    assert(0==CWAL_REFCOUNT(v));
    next = v->right;
    cwal_free(e, v);
  }
}
    

/**
   If s->finalize is not NULL then s->finalize(e, s->state) is called
   (if finalize is not 0) and s's state is cleared, else this is a
   no-op.
*/
static void cwal_state_cleanup( cwal_engine * e, cwal_state * s );

static int cwal_engine_destroy_impl( cwal_engine * e, cwal_engine_vtab * vtab ){
  if(!e || !vtab) return CWAL_RC_MISUSE;
  else{
    void const * stamp = e->allocStamp;
    if(!e->vtab ) e->vtab = vtab /* only happens during on-init errors. */ ;
    e->gcInitiator = 0;
    e->flags |= CWAL_F_IS_DESTRUCTING;
    CWAL_TR2(e,CWAL_TRACE_ENGINE_SHUTDOWN_START);

    /*
      Maintenance reminder: if we ever have an Values to clean up,
      they need to be cleaned up first (right after e->client).
    */

    cwal_recycler_get(e, CWAL_TYPE_WEAKREF)->maxLength = 0;

    e->values.exception = 0 /* its scope will clean it up */;
    e->values.propagating = 0 /* its scope will clean it up */;
    e->values.prototypes = 0 /* its scope will clean it up */;
    while( e->current ){
      cwal_scope_pop(e);
    }
    cwal_state_cleanup( e, &e->client );
       
    {/* Cleanup recyclers (AFTER scopes have been popped)... */
      cwal_size_t i;
      cwal_kvp * kvp, * next;
      cwal_recycler * re;
      cwal_scope * s, * snext;
      /* This "should" be a loop, but our use of mixed types
         screws that up.
      */
#define RE(T) re = cwal_recycler_get(e, T); assert(re && #T)
#define UNCYCLE(T) RE(T); cwal_value_list_free(e, (cwal_value*)re->list); \
      re->list = 0; re->count = 0
      UNCYCLE(CWAL_TYPE_INTEGER);
      UNCYCLE(CWAL_TYPE_DOUBLE);
      UNCYCLE(CWAL_TYPE_OBJECT);
      UNCYCLE(CWAL_TYPE_HASH);
      UNCYCLE(CWAL_TYPE_ARRAY);
      UNCYCLE(CWAL_TYPE_NATIVE);
      UNCYCLE(CWAL_TYPE_BUFFER);
      UNCYCLE(CWAL_TYPE_FUNCTION);
      UNCYCLE(CWAL_TYPE_EXCEPTION);
      UNCYCLE(CWAL_TYPE_XSTRING /* actually x/z-strings! */);
      UNCYCLE(CWAL_TYPE_UNIQUE);
      UNCYCLE(CWAL_TYPE_TUPLE);
      UNCYCLE(CWAL_TYPE_PROPREF);
      /* WEAK_REF, KVP and SCOPE are handled below... */
#undef UNCYCLE
      cwal_value_list_free(e, e->reString.list);
      e->reString.list = 0;
      e->reString.count = 0;
            
      RE(CWAL_TYPE_KVP);
      kvp = (cwal_kvp*) re->list;
      re->list = 0;
      for( ; kvp; kvp = next ){
        next = kvp->right;
        assert(!kvp->key);
        assert(!kvp->value);
        kvp->right = NULL;
        cwal_kvp_free( e/* , NULL */, kvp, 0 );
        --re->count;
      }
      assert(0==re->count);
            
      RE(CWAL_TYPE_SCOPE);
      s = (cwal_scope*) re->list;
      re->list = 0;
      for( ; s; s = snext ){
        snext = s->parent;
        s->parent = 0;
        cwal_free( e, s );
        --re->count;
      }
      assert(0==re->count);

      { /* Clean up weak ref recycler... */
        cwal_weakref * wr;
        cwal_weakref * wnext;
        RE(CWAL_TYPE_WEAKREF);
        wr = (cwal_weakref*) re->list;
        re->list = 0;
        for( ; wr; wr = wnext ){
          wnext = wr->next;
          assert(!wr->value);
          wr->next = NULL;
          cwal_free(e, wr);
          --re->count;
        }
        assert(0==re->count);
      }
#undef RE
            
      /* sanity-check to make sure we didn't leave any
         new additions out of the e-recycler cleanup...

         This has saved me grief twice now. Three times.
      */
      for( i = 0; i < (sizeof(e->recycler)/sizeof(e->recycler[0])); ++i ){
        re = &e->recycler[i];
        if(re->list){
          MARKER(("Recycler index #%d has a list? count=%d\n", (int)i, (int)re->count));
        }
#if 0
        assert(0==re->list && "Steve seems to have forgotten "
               "to account for cwal_engine::recycler-related changes.");
        assert(0==re->count);
#endif
      }
    }        

    { /* Clean up any dangling cwal_weakrefs if the client
         failed to do so... */
      cwal_size_t i = 0;
      for( i = 0; i < sizeof(e->weakr)/sizeof(e->weakr[0]); ++i ){
        cwal_weakref * list = e->weakr[i];
        e->weakr[i] = NULL;
        while( list ){
          cwal_weakref * next = list->next;
          list->next = NULL;
          cwal_weakref_free2(e, list);
          list = next;
        }
      }
    }
    cwal_ptr_table_destroy(e, &e->weakp);
    cwal_ptr_table_destroy( e, &e->interned );
    cwal_buffer_reserve(e, &e->buffer, 0);
    cwal_gc_flush( e );

    if(vtab->tracer.close){
      vtab->tracer.close( vtab->tracer.state );
      vtab->tracer.state = 0;
    }
    if(vtab->outputer.state.finalize){
      vtab->outputer.state.finalize( e, vtab->outputer.state.data );
      vtab->outputer.state.data = 0;
    }

    cwal_memchunks_free(e);
    cwal_error_clear(e, &e->err);

    CWAL_TR2(e,CWAL_TRACE_ENGINE_SHUTDOWN_END);
    *e = cwal_engine_empty;

    if( stamp == CwalConsts.AllocStamp ){
      vtab->allocator.realloc( vtab->state.data, e, 0 );
    }else{
      /* client allocated it or it was part of another object. */
      e->allocStamp = stamp;
    }

    if(vtab->state.finalize){
      vtab->state.finalize( NULL, vtab->state.data );
    }

    /**
       TODO: call vtab->shutdown() once that's added to the
       vtab interface.
    */
        
    return CWAL_RC_OK;
  }
}

/**
   Lazily allocates and initializes e->values.prototypes, if it is not
   0, then returns it. Returns 0 only on OOM errors during initial
   allocation/intialization.
*/
static cwal_array * cwal_engine_prototypes(cwal_engine * e){
  if(!e->values.prototypes && (e->values.prototypes = cwal_new_array(e)) ){
    if(cwal_array_reserve(e->values.prototypes, (cwal_size_t)CWAL_TYPE_end-1)){
      cwal_value_unref(CWAL_VALPART(e->values.prototypes));
      e->values.prototypes = 0;
    }else{
      cwal_value_ref2(e, CWAL_VALPART(e->values.prototypes))
        /* So that it cannot get sweep()'d up. */
        ;
      cwal_value_make_vacuum_proof(CWAL_VALPART(e->values.prototypes),1);
    }
  }
  return e->values.prototypes;
}

int cwal_engine_destroy( cwal_engine * e ){
  if( NULL == e->vtab ) return 0 /* special case: assume not yet inited */;
  return e ? cwal_engine_destroy_impl(e, e->vtab) : CWAL_RC_MISUSE;
}

int cwal_engine_init( cwal_engine ** E, cwal_engine_vtab * vtab ){
  unsigned const sz = sizeof(cwal_engine);
  cwal_engine * e;
  int rc;
  static int once = 0;
  if(!once){
    /* Just making sure some assumptions are right... */
#if CWAL_VOID_PTR_IS_BIG
    assert(sizeof(void*)>4);
#else
    assert(sizeof(void*)<8);
#endif
    assert(sizeof(cwal_memsize_t) <= sizeof(void*))
      /* or we'll overwrite important stuff */;
    once = 1;
  }

  if(!E || !vtab){
    return CWAL_RC_MISUSE;
  }
  if(!CWAL_BUILTIN_VALS.inited) cwal_init_builtin_values();
  assert(CWAL_BUILTIN_VALS.inited);
  e = *E;
  if(!e){
    e = (cwal_engine*)vtab->allocator
      .realloc( vtab->allocator.state.data, NULL, sz )
      /* reminder: this does not take into account memory
         over-allocation. */;
    if(!e){
      return CWAL_RC_OOM;
    }
  }
  *e = cwal_engine_empty;
  if(!*E){
    e->allocStamp = CwalConsts.AllocStamp
      /* we use this later to recognize that we allocated (and
         need to free()) e. */;
    *E = e;
  }

  e->vtab = vtab;
  if(CwalConsts.AutoInternStrings){
    e->flags |= CWAL_FEATURE_INTERN_STRINGS;
  }
  if(e->vtab->memcap.forceAllocSizeTracking
     || e->vtab->memcap.maxTotalMem
     || e->vtab->memcap.maxConcurrentMem){
    e->flags |= CWAL_F_TRACK_MEM_SIZE;
  }

  /* Tag e->recycler[*].id, just for sanity checking and to simplify
     metrics reporting. We store the virtual size of the Value type(s!)
     stored in that bin.
  */
  {
    int i = CWAL_TYPE_UNDEF;
    for( ; i < CWAL_TYPE_end; ++i ){
      cwal_recycler * re = cwal_recycler_get(e, (cwal_type_id)i);
      if(re){
        re->id = (int)cwal_type_id_sizeof((cwal_type_id)i);
      }
    }
    e->reString.id = (int)cwal_type_id_sizeof(CWAL_TYPE_STRING);
  }

#if CWAL_ENABLE_TRACE
  e->trace.e = e;
#endif
  {
    cwal_scope * sc = &e->topScope;
    rc = cwal_scope_push( e, &sc );
    if(rc) goto end;
    assert(sc->e == e);
  }
#if 0
  /* defer interned strings init until needed. */
  if(CWAL_FEATURE_INTERN_STRINGS & e->flags) {
    rc = cwal_engine_init_interning(e);
    if(rc) goto end;
  }
#endif

  if(! cwal_engine_prototypes(e) ){
    rc = CWAL_RC_OOM;
  }else if(vtab->hook.on_init){
    rc = vtab->hook.on_init( e, vtab );
  }
  end:
  return rc;
}


/*static int cwal_list_visit_FreeFunction( void * S, void * E ){
  cwal_engine * e = (cwal_engine *)E;
  cwal_function_info * f = (cwal_function_info *)S;
  cwal_state_cleanup( e, &f->state );
  cwal_string_unref( e, f->name );
  cwal_free( e, S );
  return CWAL_RC_OK;
  }
*/

void cwal_state_cleanup( cwal_engine * e, cwal_state * s ){
  if( s ){
    if( s->finalize ) s->finalize( e, s->data );
    *s = cwal_state_empty;
  }
}

int cwal_engine_client_state_set( cwal_engine * e,
                                  void * state, void const * typeId,
                                  cwal_finalizer_f dtor){
  if(!e || !state) return CWAL_RC_MISUSE;
  else if(e->client.data) return CWAL_RC_ACCESS;
  else{
    e->client.data = state;
    e->client.typeID = typeId;
    e->client.finalize = dtor;
    return 0;
  }
}

void * cwal_engine_client_state_get( cwal_engine * e, void const * typeId ){
  return (e && (typeId == e->client.typeID))
    ? e->client.data
    : 0;
}

    
int cwal_output( cwal_engine * e, void const * src, cwal_size_t n ){
  return (e && src && n)
    ? (e->vtab->outputer.output
       ? e->vtab->outputer.output( e->vtab->outputer.state.data, src, n )
       : CWAL_RC_OK)
    : CWAL_RC_MISUSE;
}

int cwal_output_flush( cwal_engine * e ){
  return (e && e->vtab)
    ? (e->vtab->outputer.flush
       ? e->vtab->outputer.flush( e->vtab->outputer.state.data )
       : CWAL_RC_OK)
    : CWAL_RC_MISUSE;
}

static int cwal_printfv_appender_cwal_output( void * S, char const * s,
                                              unsigned n ){
  return cwal_output( (cwal_engine *)S, s, (cwal_size_t)n );
}

int cwal_outputfv( cwal_engine * e, char const * fmt, va_list args ){
  if(!e || !fmt) return CWAL_RC_MISUSE;
  else{
    return cwal_printfv( cwal_printfv_appender_cwal_output, e, fmt, args );
  }
}

    
int cwal_outputf( cwal_engine * e, char const * fmt, ... ){
  if(!e || !fmt) return CWAL_RC_MISUSE;
  else{
    int rc;
    va_list args;
    va_start(args,fmt);
    rc = cwal_outputfv( e, fmt, args );
    va_end(args);
    return rc;
  }
}

typedef struct BufferAppender {
  cwal_engine * e;
  cwal_buffer * b;
  int rc;
} BufferAppender;

static int cwal_printfv_appender_buffer( void * arg, char const * data,
                                         unsigned n ){
  BufferAppender * const ba = (BufferAppender*)arg;
  cwal_buffer * const sb = ba->b;
  if( !sb ) return CWAL_RC_MISUSE;
  else if( ! n ) return 0;
  else{
    int rc;
    unsigned N;
    size_t npos = sb->used + n;
    if( npos >= sb->capacity ){
      const size_t asz = npos ? ((3 * npos / 2) + 1) : 32;
      if( asz < npos /* overflow */ ) {
        return ba->rc = CWAL_RC_RANGE;
      } else {
        rc = cwal_buffer_reserve( ba->e, sb, asz );
        if(rc) {
          return ba->rc = rc;
        }
      }
    }
    N = 0;
    for( ; N < n; ++N, ++sb->used ) sb->mem[sb->used] = data[N];
    sb->mem[sb->used] = 0;
    return 0;
  }
}

int cwal_buffer_append( cwal_engine * e,
                        cwal_buffer * b,
                        void const * data,
                        cwal_size_t len ){
  cwal_size_t sz;
  int rc;
  if(!b || !data) return CWAL_RC_MISUSE;
  sz = b->used + len + 1/*NUL*/;
  rc = cwal_buffer_reserve( e, b, sz );
  if(rc) return rc;
  memcpy( b->mem+b->used, data, len );
  b->used += len;
  b->mem[b->used] = 0;
  return 0;
}

int cwal_buffer_printfv( cwal_engine * e, cwal_buffer * b, char const * fmt, va_list args){
  if(!e || !b || !fmt) return CWAL_RC_MISUSE;
  else{
    BufferAppender ba;
    cwal_size_t const oldUsed = b->used;
    ba.b = b;
    ba.e = e;
    ba.rc = 0;
    cwal_printfv( cwal_printfv_appender_buffer, &ba, fmt, args );
    if(ba.rc){
      b->used = oldUsed;
      if(b->capacity>oldUsed){
        b->mem[oldUsed] = 0;
      }
    }
    return ba.rc;
  }
}

int cwal_buffer_printf( cwal_engine * e, cwal_buffer * b, char const * fmt, ... ){
  if(!e || !b || !fmt) return CWAL_RC_MISUSE;
  else{
    int rc;
    va_list args;
    va_start(args,fmt);
    rc = cwal_buffer_printfv( e, b, fmt, args );
    va_end(args);
    return rc;
  }
}

static int cwal_scope_alloc( cwal_engine * e, cwal_scope ** S ){
  cwal_scope * s;
  assert( S && "Invalid NULL ptr.");
  s = *S;
  ++e->metrics.requested[CWAL_TYPE_SCOPE];
  if(!s){
    cwal_recycler * re = cwal_recycler_get(e, CWAL_TYPE_SCOPE);
    assert(re);
    if(re->count){
      s = (cwal_scope*)re->list;
      assert(s);
      if(s->parent){
        re->list = s->parent;
        s->parent = 0;
      }
      else re->list = 0;
      --re->count;
    }
    else{
      s = (cwal_scope *)cwal_malloc( e, sizeof(cwal_scope) );
      if(s){
        ++e->metrics.allocated[CWAL_TYPE_SCOPE];
        e->metrics.bytes[CWAL_TYPE_SCOPE] += sizeof(cwal_scope);
      }
    }
  }
  if(s){
    *s = cwal_scope_empty;
    s->e = e;
    if(*S != s) {
      s->allocStamp = CwalConsts.AllocStamp
        /* we use this later to know whether or not we need to
           free() s. */;
      /* Potential TODO: use e as the allocStamp for any
         resources e allocates. */
      *S = s;
    }
  }
  return s ? CWAL_RC_OK : CWAL_RC_OOM;
}

/**
   Pops the e->current scope from the scope stack, cleaning it up and
   possibly cwal_free()ing it. If callHook is true then the
   cwal_engine_vtab::hook::scope_pop hook, if not NULL, will be called
   before the scope state is changed. The only error conditions are
   invalid arguments:

   !e = CWAL_RC_MISUSE

   !e->current = CWAL_RC_RANGE
*/
static int cwal_scope_pop_impl( cwal_engine * e, char callHook ){
  if(!e) return CWAL_RC_MISUSE;
  else if( 0 == e->current ) return CWAL_RC_RANGE;
  else{
    cwal_scope * p;
    cwal_scope * s = e->current;
    assert(s->e == e);
    if(callHook && e->vtab->hook.scope_pop){
      e->vtab->hook.scope_pop( s, e->vtab->hook.scope_state );
    }
    p = s->parent;
    assert(p || (e->top==s));
    e->current = p;
    if(e->top==s) e->top = 0;
    cwal_scope_free( e, s, 1 );
    return 0;
  }
}

int cwal_scope_pop( cwal_engine * e ){
  return cwal_scope_pop_impl(e, 1);
}

int cwal_scope_push( cwal_engine * e, cwal_scope ** S ){
  cwal_scope * s = S ? *S : NULL;
  int rc;
  if(!e) return CWAL_RC_MISUSE;
  rc = cwal_scope_alloc(e, &s);
  if(rc){
    assert(NULL == s);
    return rc;
  }
  assert(NULL != s);
  s->level = e->current ? (1 + e->current->level) : 1;
  s->parent = e->current;
  if(!e->top){
    assert(NULL == e->current);
    e->top = s;
  }
  e->current = s;
  if(e->vtab->hook.scope_push){
    rc = e->vtab->hook.scope_push( s, e->vtab->hook.scope_state );
    if(rc) cwal_scope_pop_impl(e, 0);
  }
  if(!rc && S) *S = s;
  return rc;
}

int cwal_scope_push2( cwal_engine * e, cwal_scope * s ){
  return (!e || !s || memcmp(s, &cwal_scope_empty, sizeof(cwal_scope)))
    ? CWAL_RC_MISUSE
    : cwal_scope_push( e, &s );
}

int cwal_scope_pop2( cwal_engine * e, cwal_value * resultVal ){
  if(!e) return CWAL_RC_MISUSE;
  else if(!e->current
          || (resultVal && !e->current->parent)) return CWAL_RC_RANGE;
  else{
    int rc = 0;
#if !defined(NDEBUG)
    bool const isMyProps = resultVal==e->current->props;
#endif
    if(resultVal){
      cwal_value_ref(resultVal);
      cwal_value_rescope(e->current->parent, resultVal);
    }
    rc = cwal_scope_pop_impl(e, 1);
    if(resultVal){
#if !defined(NDEBUG)
      if(isMyProps){
        cwal_obase * const obase = CWAL_VOBASE(resultVal);
        assert(obase);
        assert(!(obase->flags & ~CWAL_F_IS_PROP_STORAGE));
      }
#endif
      cwal_value_unhand(resultVal);
    }
    return rc;
  }        
}

cwal_value * cwal_value_snip( cwal_value * v ){
  cwal_scope * p = v->scope;
  cwal_value * l = v->left;
  cwal_value * r = v->right;
  v->scope = 0;
  v->right = v->left = 0;
  assert(r != v);
  assert(l != v);
  if(l) l->right = r;
  if(r) r->left = l;
  if(p){
    /* Adjust value lifetime lists if v is the head of one of them.
       If it is not the head, then removal from its linked list
       is sufficient.
    */
    l = l ? l : r;
    if(l){
      while(l->left){
        if(l == l->left){
          cwal_dump_value( "cwal.c", __LINE__, l, "Internal cwal_value list misuse.");
          assert(l->left != l && "Internal cwal_value list misuse.");
          E_IS_DEAD(p->e) = CWAL_RC_ASSERT;
          return NULL;
          /* abort(); */
        }
        l=l->left;
      }
    }
    if(p->mine.headObj==v){
      p->mine.headObj = l;
      CWAL_TR_SV(p->e,p,l);
      CWAL_TR3(p->e,CWAL_TRACE_SCOPE_MASK,"Scope replaced mine->headObj.");
      if(p->mine.headObj) assert(0==p->mine.headObj->left);
    }else if(p->mine.headPod==v){
      p->mine.headPod = l;
      CWAL_TR_SV(p->e,p,l);
      CWAL_TR3(p->e,CWAL_TRACE_SCOPE_MASK,"Scope replaced mine->headPod.");
      if(p->mine.headPod) assert(0==p->mine.headPod->left);
    }else if(p->mine.headSafe==v){
      p->mine.headSafe = l;
      CWAL_TR_SV(p->e,p,l);
      CWAL_TR3(p->e,CWAL_TRACE_SCOPE_MASK,"Scope replaced mine->headSafe.");
      if(p->mine.headSafe) assert(0==p->mine.headSafe->left);
    }else if(p->mine.r0==v){
      p->mine.r0 = l;
      CWAL_TR_SV(p->e,p,l);
      CWAL_TR3(p->e,CWAL_TRACE_SCOPE_MASK,"Scope replaced mine->r0.");
      if(p->mine.r0) assert(0==p->mine.r0->left);
    }
  }
  return r;
}

/**
   Inserts v so that v->right is now l, adjusting v and l as necessary.
   v->right and v->left must be 0 before calling this (it assert()s so).
*/
static void cwal_value_insert_before( cwal_value * l, cwal_value * v ){
  assert(0 == v->right);
  assert(0 == v->left);
  assert((l != v) && "Unexpected duplicate items for value list. "
         "Possibly caused by an unwarranted unref.");
  /* if(l != v){ */
  if( l->left ){
    l->left->right = v;
  }
  l->left = v;
  v->right = l;
  /* } */
}

cwal_value * cwal_string_from_recycler( cwal_engine * e, cwal_size_t len ){
  cwal_value * li = (cwal_value *)e->reString.list;
  cwal_string * s;
  cwal_value * prev = 0;
  cwal_size_t slen;
  cwal_size_t paddedLen;
  for( ; li; prev = li, li = li->right ){
    s = CWAL_STR(li) /*cwal_value_get_string(li)*/;
    assert(s);
    assert(0 < CWAL_STRLEN(s));
    assert(!CWAL_STR_ISXZ(s));
    assert(prev != li);
    slen = CWAL_STRLEN(s);
    if(!CwalConsts.StringPadSize){
      if(len!=slen) continue;
      /* Else fall through */
    }else if(len!=slen){
      /**
         If s's "padded length" is large enough for the request,
         but not "too large" (within 1 increment of
         CwalConsts.StringPadSize) then we will re-use it.
      */
      cwal_size_t const mod = (slen % CwalConsts.StringPadSize);
      paddedLen = mod
        ? (slen + (CwalConsts.StringPadSize - mod))
        : slen;
      if(paddedLen < len) continue;
      else if(paddedLen > len){
        if((paddedLen - CwalConsts.StringPadSize) > len) continue;
      }
    }
    if(prev){
      prev->right = li->right;
    }
    else {
      assert(e->reString.list == li);
      e->reString.list = li->right;
    }
    li->right = 0;
    --e->reString.count;
    if(CwalConsts.StringPadSize){
      s->length = CWAL_STRLEN_MASK & len;
    }
    /* MARKER("Pulling string of len %u from recycle bin.\n", (unsigned)len); */
    CWAL_TR_V(e,li);
    CWAL_TR3(e,CWAL_TRACE_MEM_FROM_RECYCLER,
             "Pulled string from recycle bin.");
    ++e->metrics.valuesRecycled;
    ++e->reString.hits;
    return li;
  }
  ++e->reString.misses;
  ++e->metrics.valuesRecycleMisses;
#if 0
  /* this is causing a valgrind warning via memset() via cwal_value_new() */
  if(e->reChunk.head
     && !e->reString.list /* see comments below */){
    /* Look in the chunk recycler as a last resort. Testing with
       the s2 amalgamation shows that this very rarely hits if
       recycling is on (only once in the whole test suite from
       20141201). If recycling is off, it hits surprisingly often
       (1224 times in the test suite). So we only do this O(N)
       lookup when string recycling is off or its recycling bin is
       empty.

       If both value recyling and string interning are off, this
       block hits 3798 times in the above-mentioned test suite.
    */
    cwal_size_t reqSize = sizeof(cwal_value)
      + sizeof(cwal_string)
      + len + 1 /*NUL*/;
    cwal_value * v = (cwal_value *)
      cwal_memchunk_request(e, &reqSize,
                            len<CwalConsts.MaxRecycledStringLen
                            /* TODO: tweak these values */
                            ? 150 /* ==> up to 1.5x */
                            : 125 /* ==> up to 1.25x. Wasting
                                     memory? They should be glad
                                     we're recycling it at
                                     all! */,
                            "cwal_string_from_recycler()");
    if(v){
      /* MARKER(("Got string fallback!\n")); */
      *v = cwal_value_string_empty;
      s = CWAL_STR(v);
      assert(!(~CWAL_STRLEN_MASK & len));
      s->length = (cwal_size_t)(CWAL_STRLEN_MASK & len);
      return v;
    }
  }
#endif
  return NULL;
}

    
static bool cwal_string_recycle( cwal_engine * e, cwal_value * v ){
  cwal_string * s = cwal_value_get_string(v);
  cwal_size_t const slen = CWAL_STRLEN(s);
  char const * freeMsg = 0;
  cwal_value * li;
  assert(s);
  if( slen > CwalConsts.MaxRecycledStringLen ){
    freeMsg = "String too long to recycle - freeing.";
    goto freeit;
  }
  else if(CWAL_STR_ISXZ(s)){
    assert(!"Cannot happen anymore - x/z-string recycled elsewhere/elsehow.");
    freeMsg = "Cannot recycle x/z-strings.";
    goto freeit;
  }
  else if( 0 == e->reString.maxLength ){
    freeMsg = "String recycling disabled - freeing.";
    goto freeit;
  }else if(freeMsg){
    /* avoiding an unused var in non-debug build */
    assert(!"impossible");
  }
  li = (cwal_value *)e->reString.list;
  assert(v != li);
  if(e->reString.count>=e->reString.maxLength){
    /*
      Remove the oldest entries from the list.
      They have not been recycled in a while,
      and are not likely needed any more.

      To avoid unduly high N on the O(N) linked list traversal,
      when trimming the list we trim some percentage of it, as
      opposed to only the last (oldest) element. Otherwise this
      algo is remarkably slow on large recycle lists or when
      rapidly freeing many strings.

      TODO?: sort the strings by size(?) in order to optimize the
      from-recycle-bin size lookup?
    */
    cwal_size_t keep = e->reString.maxLength * 3 / 4;
    cwal_size_t i;
    cwal_value * x = li;
    cwal_value * prev = 0;
    cwal_value * cut = 0;
    for( i = 0; x;
         prev = x, x = x->right, ++i ){
      if(i==keep){
        assert(!x->left);
        cut = x;
        break;
      }
    }
    if(prev) prev->right = 0;
    else e->reString.list = 0;

#if 0
    MARKER("Trimming list to %u of %u entries cut=%c.\n",
           (unsigned)i, (unsigned) e->reString.maxLength, cut?'y':'n');
#endif

    if(cut){
#if 0
      e->reString.count = i;
      CWAL_TR_V(e,x);
      CWAL_TR3(e,CWAL_TRACE_MEM_TO_RECYCLER,
               "Popping stale string(s) from recycler.");
      cwal_value_list_free(e, cut);
#else
      /* We "could" just use cwal_value_list_free(),
         but i want more tracing info. */
      for( x = cut; x ; x = cut ){
        cut = x->right;
        x->right = 0;
        --e->reString.count;
        CWAL_TR_V(e,x);
        CWAL_TR3(e,CWAL_TRACE_MEM_TO_RECYCLER,
                 "Popping stale string from recycler.");
        cwal_free(e,x);
      }
#endif
    }
    assert(e->reString.count < e->reString.maxLength);
  }

  /* MARKER("String to recyler...\n"); */
  assert(!v->right);
  assert(!v->scope);
  assert(!CWAL_REFCOUNT(v));
  assert(CWAL_TYPE_STRING==v->vtab->typeID);
  li = (cwal_value*)e->reString.list;
  assert(!li || (CWAL_TYPE_STRING==li->vtab->typeID));
  assert(v != li);
  v->right = li;
  e->reString.list = v;
  ++e->reString.count;
  CWAL_TR_V(e,v);
  CWAL_TR3(e, CWAL_TRACE_MEM_TO_RECYCLER,
           "Placed string in recycling bin.");
  return 1;
  freeit:
#if !CWAL_ENABLE_TRACE
  if(0){ assert(freeMsg && "Avoid unused var warning in non-trace builds."); }
#endif
  CWAL_TR_SV(e,e->current,v);
  CWAL_TR3(e,CWAL_TRACE_MEM_TO_RECYCLER, freeMsg);
#if 1
  {
    /**
       Pushing this memory to the chunk recycler can lower both allocs
       and peak marginally, but can also increase peak while lowering
       allocs, depending on the usage. Compare s2's UNIT.s2 vs
       UNIT-import.s2. They both run the same tests, but the former
       loads them as a single script and the latter imports one script
       at a time.
    */
    cwal_size_t sz = sizeof(cwal_value)+sizeof(cwal_string) + slen + 1 /*NUL byte*/;
    /* Account for string size padding */
    if(CwalConsts.StringPadSize){
      if(slen<CwalConsts.StringPadSize) sz = sz - slen + CwalConsts.StringPadSize;
      else if(slen>CwalConsts.StringPadSize){
        cwal_size_t const mod = (slen % CwalConsts.StringPadSize);
        if(mod) sz = sz + (CwalConsts.StringPadSize - mod);
      }
    }
    cwal_memchunk_add(e, v, sz);
  }
#else
  cwal_free( e, v );
#endif
  return 0;
    
}

int cwal_value_recycle( cwal_engine * e, cwal_value * v ){
  int ndx;
  cwal_recycler * re;    
  cwal_size_t max;
#if CWAL_ENABLE_TRACE
  char const * freeMsg = "==> CANNOT RECYCLE. FREEING.";
#  define MSG(X) freeMsg = X
#else
#  define MSG(X)
#endif
  assert(e && v && v->vtab);
  assert(0==CWAL_REFCOUNT(v));
  CWAL_RCFLAG_OFF(v, CWAL_RCF_IS_VACUUM_PROOF);
  if(e->gcInitiator
     /*&& (CWAL_V_IS_OBASE(v)
       || (CWAL_TYPE_UNIQUE==v->vtab->typeID)
       || (CWAL_TYPE_TUPLE==v->vtab->typeID)
       //^^^^ We also need to put "not-quite-containers" here,
       // namely CWAL_TYPE_UNIQUE and any potentially similar
       // ones (CWAL_TYPE_TUPLE).
       )*/){
    /**
       20180105: historically we only put certain types in the GC
       queue, but we came across a situation where a Unique value
       wrapping a string choked at destruction because its wrapped
       string had already been sent to the string recycler
       (strings, at the time, did not get gc-queued)
    */
    /**
       This means a scope cleanup is running and deallocation must
       be delayed until the cleanup is finished. Move the value
       into the GC queue. We hypothetically only need this for
       types which can participate in cycles, as it's easy to step
       on a destroyed value via a cycle during value
       destruction. Practice has (finally, 20180105) shown that
       certain non-cyclic relationships require that their values
       (in particular code constellations) must also be gc-queued.
    */
    CWAL_TR_V(e,v);
    CWAL_TR3(e,CWAL_TRACE_MEM_TO_GC_QUEUE,
             "Redirecting value to gc queue");
    ndx = cwal_gc_push( e, v );
    assert(0==ndx && "cwal_gc_push() can (now) only fail if !e->gcInitiator.");
    return (0==ndx)
      ? -1
      : 0;
  }
  else if(CWAL_TYPE_STRING == v->vtab->typeID
          && !CWAL_STR_ISXZ(CWAL_STR(v))){
    return cwal_string_recycle( e, v );
  }

  assert( 0 == CWAL_REFCOUNT(v) );
  assert( 0 == v->right );
  assert( 0 == v->left );
  assert( 0 == v->scope );
  ndx = cwal_recycler_index( v->vtab->typeID );
  if( ndx < 0 ) {
    /* Non-recylable type */
    MSG("==> Unrecyclable type. FREEING.");
    goto freeit;
  }
  re = &e->recycler[ndx];
  max = re->maxLength;
  if(!max) {
    /* recycling disabled */
    MSG("==> Recyling disabled for this type. FREEING.");
    goto freeit;
  }
  else if( re->count >= max ){
    /* bin is full */
    MSG("==> Recyling bin for this type is full. FREEING.");
    goto freeit;
  }

  assert(!CWAL_RCFLAG_HAS(v, CWAL_RCF_IS_RECYCLED));
  CWAL_RCFLAG_ON(v, CWAL_RCF_IS_RECYCLED);
  assert(CWAL_RCFLAG_HAS(v, CWAL_RCF_IS_RECYCLED));
  if(re->list){
    cwal_value_insert_before( re->list, v );
  }
  ++re->count;
  re->list = v;
  if(re->count > 1){
    assert(((cwal_value*)re->list)->right);
  }
  CWAL_TR_V(e,v);
  CWAL_TR3(e, CWAL_TRACE_MEM_TO_RECYCLER,
           "Placed in recycling bin.");
  return 1;
  freeit:
  CWAL_TR_V(e,v);
  CWAL_TR3(e,CWAL_TRACE_MEM_TO_RECYCLER,freeMsg);
  *v = *cwal_value_undefined();
  cwal_free(e, v);
  return 0;
#undef MSG
}


int cwal_gc_push( cwal_engine * e, cwal_value * v ){
  assert( e->gcInitiator );
  assert(0 == v->scope);
  assert(0 == v->right);
  if(!e->gcInitiator) return CWAL_RC_MISUSE;
  if(e->values.gcList){
    cwal_value_insert_before( e->values.gcList, v );
  }
  assert(!CWAL_RCFLAG_HAS(v,CWAL_RCF_IS_GC_QUEUED));
  CWAL_RCFLAG_ON(v,CWAL_RCF_IS_GC_QUEUED);
  assert(CWAL_RCFLAG_HAS(v,CWAL_RCF_IS_GC_QUEUED));
  e->values.gcList = v;
  assert(0==e->values.gcList->left);
  return CWAL_RC_OK;
}

int cwal_gc_flush( cwal_engine * e ){
  int rc = 0;
  cwal_value * v;
  cwal_value * n;
  assert( 0 == e->gcInitiator
          && "Otherwise we might have a loop b/t this and cwal_value_recycle()");
  for( v = e->values.gcList; v; v = n ){
    n = v->right;
    cwal_value_snip(v);
    assert(!CWAL_REFCOUNT(v));
    assert(CWAL_RCFLAG_HAS(v,CWAL_RCF_IS_GC_QUEUED));
    CWAL_RCFLAG_OFF(v,CWAL_RCF_IS_GC_QUEUED);
    assert(!CWAL_RCFLAG_HAS(v,CWAL_RCF_IS_GC_QUEUED));
    /* if( (base = CWAL_VOBASE(v)) ) base->flags &= ~CWAL_F_IS_GC_QUEUED; */
    /* dump_val(v,"refcount?"); */
    assert(!CWAL_REFCOUNT(v));
    rc = cwal_value_recycle(e, v);
    assert(-1!=rc && "Impossible loop!");
  }
  e->values.gcList = 0;
  return rc;
}

int cwal_scope_insert( cwal_scope * const s, cwal_value * const v ){
  /* cwal_scope * p = v->scope; */
  cwal_value * list;
  cwal_value ** listpp = 0;
  cwal_obase * const b = CWAL_VOBASE(v);
  assert(s && v);
  assert(!CWAL_MEM_IS_BUILTIN(v));
  /*
    Reminder to self: v->scope == s when we're moving items
    around from s->mine.{r0,headSafe}.
  */
  cwal_value_snip( v );
  assert(0==v->right);
  assert(0==v->left);
  assert(!CWAL_RCFLAG_HAS(v, CWAL_RCF_IS_RECYCLED));
  if(0==CWAL_REFCOUNT(v)){
    listpp = &s->mine.r0;
  }else if(CWAL_V_IS_VACUUM_SAFE(v)
           || (b && (b->flags & CWAL_F_IS_PROP_STORAGE))){
    /* ------------- keeping in ^^^^^ mind that b might be the
       cwal_scope::props of a _different_ (newer) scope. We need the
       CWAL_F_IS_PROP_STORAGE flag primarily to protect against
       vacuuming in that unusual (but not unrealistic) case. */
    listpp = &s->mine.headSafe;
  }else if(CWAL_V_GOES_IN_HEADOBJ(v)){
    /* 20180105 reminder: this doesn't include Uniques and Tuples.
       TODO?: add those types to headObj once we're certain that
       doing so won't cause other Grief. We could undo the
       special-case destruction checks in
       cwal_value_cleanup_unique() and cwal_value_cleanup_tuple()
       if we do that, and revert the GC queue to only holding
       containers (plus Unique and Tuple). Initial tests in that
       regard didn't alleviate them (or Unique, at least) needing
       their extra cleanup checks :/.
    */
    listpp = &s->mine.headObj; 
  }else {
    listpp = &s->mine.headPod;
  }
  list = *listpp;
  if(list == v){
    cwal_dump_value( __FILE__, __LINE__, v,
                     "FATAL: inserting item we already have! "
                     "String interning backfire?");
    assert(list != v && "Insertion failed: this item is "
           "the head of one of the lists! String interning backfire?");
    E_IS_DEAD(s->e) = CWAL_RC_ASSERT;
#if 1
    return s->e->fatalCode;
#else
    abort(/* this is the only solution, as this error is indicative
             or memory corruption within cwal and we cannot report
             it to the user from this level. */);
#endif
  }
  else if(list) {
    v->right = list;
    assert(0==list->left);
    list->left = v;
  }
  *listpp = v;
  v->scope = s;
  assert(0==v->left);
  CWAL_TR_SV(s->e,s,v);
  CWAL_TR3(s->e,CWAL_TRACE_VALUE_SCOPED,"Value moved to scope.");
  return 0;
}


int cwal_value_take( cwal_engine * e, cwal_value * v ){
  cwal_scope * s = v ? v->scope : 0;
  if(!e || !v) return CWAL_RC_MISUSE;
  else if( CWAL_MEM_IS_BUILTIN( v ) ) return CWAL_RC_OK;
  else if(!s) return CWAL_RC_RANGE;
  else{
    cwal_value_snip( v );
    CWAL_TR_SV(e,s,v);
    CWAL_TR3(e,CWAL_TRACE_VALUE_UNSCOPED,"Removed from parent scope.");
    assert(!v->scope);
    assert(0==v->right);
    assert(0==v->left);
    assert(s->mine.headObj != v);
    assert(s->mine.headPod != v);
    assert(s->mine.headSafe != v);
    assert(s->mine.r0 != v);
    return CWAL_RC_OK;
  }
}

int cwal_value_unref(cwal_value *v ){
  if(!v) return CWAL_RC_MISUSE;
  else if( CWAL_MEM_IS_BUILTIN( v ) ) return CWAL_RC_OK;
  else if(!v->scope){
    if(CWAL_V_IS_IN_CLEANUP(v)) return CWAL_RC_DESTRUCTION_RUNNING;
    assert(!"Cannot unref a Value with no scope: serious misuse or Value corruption.");
    return CWAL_RC_MISUSE;
  }
  else return cwal_value_unref2(v->scope->e, v);
}

int cwal_value_unref2(cwal_engine * e, cwal_value *v ){
  assert( e && v );
  if(NULL == e || NULL == v) return CWAL_RC_MISUSE;
  CWAL_TR_SV(e,v->scope,v);
  if(CWAL_MEM_IS_BUILTIN(v)) return 0;
  else {
    cwal_obase * b;
    b = CWAL_VOBASE(v);
    CWAL_TR3(e,CWAL_TRACE_VALUE_REFCOUNT,"Unref'ing");
    if(!CWAL_REFCOUNT(v) || !CWAL_RCDECR(v)){
      cwal_scope * const vScope = v->scope;
      CWAL_TR_V(e,v);
      if(CWAL_RCFLAG_HAS(v, CWAL_RCF_IS_GC_QUEUED)){
        assert(!v->scope);
        CWAL_TR_S(e,vScope);
        CWAL_TR3(e,CWAL_TRACE_VALUE_CYCLE,
                 "DESTRUCTION OF A GC-QUEUED OBJECT: SKIPPING");
        /* Possible again since starting refcount at 0:
           assert(!"This is no longer possible."); */
        return CWAL_RC_DESTRUCTION_RUNNING;
      }
      else if(CWAL_RCFLAG_HAS(v, CWAL_RCF_IS_RECYCLED)){
        assert(!v->scope);
        CWAL_TR_S(e,vScope);
        CWAL_TR3(e,CWAL_TRACE_VALUE_CYCLE,
                 "DESTRUCTION OF A RECYCLED OBJECT: SKIPPING");
        /* Possible again since starting refcount at 0:
           assert(!"This is no longer possible."); */
        return CWAL_RC_DESTRUCTION_RUNNING;
      }
      else if(CWAL_RCFLAG_HAS(v, CWAL_RCF_IS_DESTRUCTING)) {
        CWAL_TR_S(e,vScope);
        CWAL_TR3(e,CWAL_TRACE_VALUE_CYCLE,
                 "ENTERING DESTRUCTION A 2ND TIME (or more): SKIPPING");
        /* Possible again since starting refcount at 0:
           assert(!"This is no longer possible."); */
        return CWAL_RC_DESTRUCTION_RUNNING;
      }
      CWAL_TR_S(e,vScope);
      CWAL_TR3(e,CWAL_TRACE_VALUE_CLEAN_START,
               "Starting finalization...");

      if(e->values.exception == v){
        e->values.exception = 0;
      }
      if(e->values.propagating == v){
        e->values.propagating = 0;
      }

      if(!v->scope){
        dump_val(v,"Value with NULL scope???");
        assert(v->scope && "this is always true now. "
               "That was not always the case.");
        return e->fatalCode = CWAL_RC_ASSERT;
      }
      cwal_value_take(e, v)
        /*ignoring rc! take() cannot fail any more under these
          conditions.*/;
      if(b && (CWAL_F_IS_PROP_STORAGE & b->flags)){
        /* reminder to self: it's potentially possible for
           clients to move this value into a place where
           it's used for purposes other than property
           storage, via cwal_scope_properties().
        */
        cwal_scope * sc = e->current;
        assert(CWAL_TYPE_OBJECT==v->vtab->typeID);
        /*
          Special case: if v is the cwal_scope::props handle
          of a scope, we need to clear that to make sure it
          doesn't get stale. This can hypothetically happen if
          client code exposes cwal_scope_properties() to
          script-space, causes it to get upscoped (for
          ownership purposes) but still being pointed to by
          cwal_scope::props, and then unrefs it. If that
          happens, though...  hmmm... the scope storage is
          always treated as vacuum-safe, which means that they
          could become impossible to vacuum up if clients
          introduced cycles and then abandoned all
          script-visible references. Oh, well. They shouldn't
          be exposing these to script code, probably.
        */
        for( ; sc ; sc = sc->parent ){
          if(sc->props == v){
            sc->props = 0;
            break;
          }
        }
      }
      /* The left/right assertions help ensure we're not now
         traversing through the recycle list, which is an easy thing
         to do when memory has been mismanaged.
      */
      if(v->left || v->right){
        /*
          This is case checked above in a different manner.
          If this check fails, then memory corruption is
          what's going on and we need to stop that... the only
          way we reasonably can. Alternately, we may at some
          point add a flag to the engine telling it it's in an
          unrecoverable/unusable state. That would require
          adding that check to a great many routines, though.
        */
        dump_val(v,"Item from recycler???");
        if(v->left) dump_val(v->left,"v->left");
        if(v->right) dump_val(v->right,"v->right");
        cwal__fatal(CWAL_RC_CORRUPTION,
                    "Trying to clean up item from recycler(?)!");
        return e->fatalCode = CWAL_RC_ASSERT;
        /* abort(); */
      }

      assert(!CWAL_RCFLAG_HAS(v, CWAL_RCF_IS_DESTRUCTING));
      CWAL_RCFLAG_ON(v, CWAL_RCF_IS_DESTRUCTING);
      assert(CWAL_RCFLAG_HAS(v, CWAL_RCF_IS_DESTRUCTING));
      assert(0 == v->right);
      assert(0 == v->left);

      cwal_weak_unregister( e, v, v->vtab->typeID );
      v->scope = vScope
        /* Workaround to help support the ancient behaviour
           which may or may not still be relevant.
        */;
      v->vtab->cleanup(e, v);
      v->scope = 0 /* END KLUDGE! */;
      assert(CWAL_RCFLAG_HAS(v, CWAL_RCF_IS_DESTRUCTING));
      CWAL_RCFLAG_OFF(v, CWAL_RCF_IS_DESTRUCTING);
      assert(!CWAL_RCFLAG_HAS(v, CWAL_RCF_IS_DESTRUCTING));
      CWAL_TR_V(e,v);
      CWAL_TR3(e,CWAL_TRACE_VALUE_CLEAN_END,
               "Cleanup complete. Sending to recycler...");
      assert(0 == CWAL_REFCOUNT(v));
      cwal_value_recycle(e, v);
      return CWAL_RC_FINALIZED;
    }
    else{
      CWAL_TR_V(e,v);
      CWAL_TR3(e,CWAL_TRACE_VALUE_REFCOUNT,
               "It continues to live");
      return CWAL_RC_HAS_REFERENCES;
    }
  }
}

cwal_value * cwal_value_unhand( cwal_value * v ){
  if(!v || CWAL_MEM_IS_BUILTIN(v)) return v;
  else if(!v->scope){
    /* should we do this check, like we do in cwal_value_unref()?
       if(CWAL_V_IS_IN_CLEANUP(v)) return NULL;
    */
    assert(!"Cannot unhand a Value with no scope: serious misuse or Value corruption.");
    return NULL;
  }
  else if(CWAL_REFCOUNT(v) && 0==CWAL_RCDECR(v)){
    cwal_value_reprobate(v->scope, v);
  }
  return v;
}

/**
   Increments cv's reference count by 1. Asserts that !CWAL_MEM_IS_BUILTIN(v)
   and (NULL!=cv). Code which might be dealing with those value should
   call the public-API-equivalent of this, cwal_value_ref().
*/
static int cwal_refcount_incr( cwal_engine *e, cwal_value * cv )
{
  assert( NULL != cv );
  /* assert(!CWAL_MEM_IS_BUILTIN(cv)); */
  assert(cv->scope) /* also catches builtins */;
  if( CWAL_RCFLAG_MAXRC <= CWAL_REFCOUNT(cv) ){
    /* Overflow! */
    cwal_dump_value( __FILE__, __LINE__, cv,
                     "FATAL: refcount overflow! How?!?");
    assert(!"Refcount overflow! Undefined behaviour!");
    return e->fatalCode = CWAL_RC_RANGE;
  }
  else if(1 == CWAL_RCINCR(cv)){
    if(cwal_scope_from_r0(cv)){
      assert(!"If this is failing then internal "
             "preconditions/assumptions have not been met.");
#if !defined(NDEBUG)
      abort();
#endif
      return e->fatalCode = CWAL_RC_ASSERT;
    }
  }
  if(CWAL_REFCOUNT(cv) > e->metrics.highestRefcount){
    e->metrics.highestRefcount = CWAL_REFCOUNT(cv);
    e->metrics.highestRefcountType = cv->vtab->typeID;
  }
  CWAL_TR_V(e,cv);
  CWAL_TR3(e,CWAL_TRACE_VALUE_REFCOUNT,"++Refcount");
  return 0;
}

int cwal_value_ref2( cwal_engine *e, cwal_value * cv ){
  assert(e && cv);
  if( !e || !cv ) return CWAL_RC_MISUSE;
  else if( CWAL_MEM_IS_BUILTIN(cv) ) return CWAL_RC_OK;
  else if( !cv->scope ){
    assert(!"Apparent attempt to ref() an "
           "invalid (cleaned up?) value.");
    return CWAL_RC_MISUSE;
  }
  else return CWAL_RCFLAG_MAXRC <= CWAL_REFCOUNT(cv)
         ? CWAL_RC_RANGE
         : cwal_refcount_incr( e, cv );
}
    
int cwal_value_ref( cwal_value * cv ){
  if( NULL == cv ) return CWAL_RC_MISUSE;
  else if( CWAL_MEM_IS_BUILTIN(cv) ) return CWAL_RC_OK;
  else if( !cv->scope ){
    assert(!"Apparent attempt to ref() an "
           "invalid (cleaned up?) value.");
    return CWAL_RC_MISUSE;
  }
  else {
    assert( cv->scope->e );
    return cwal_refcount_incr( cv->scope->e, cv );
  }
}

bool cwal_refunref( cwal_value * v ){
  char rc = 0;
  if(v && v->scope && !CWAL_REFCOUNT(v)){
    /* This is a temp. Or, it turns out, it's an interned string
       which is used in 2+ places and our nuking it has disastrous
       side-effects. So... there is no obvious internal workaround
       because string interning does not count how many instances
       are interned (which we could use to "fudge" the refcount
       check). So we're going to call that a usage error and leave
       it at that. i'm suddenly very glad i resisted, at the time,
       the urge to intern integers/doubles as well.

       One potential workaround, but not a good one:

       if v is-a string AND (v->scope->level <
       v->scope->e->current->level) then don't nuke it.

       That would only hide the problem some (most?) of the time,
       though.
    */
    cwal_value_unref(v);
    rc = 1;
  }
  return rc;
}

cwal_refcount_t cwal_value_refcount( cwal_value const * v ){
  return v ? CWAL_REFCOUNT(v) : 0;
}

/** @internal
   DO NOT USE THIS! It was a design decision which is in conflict with
   the overall lifetime model.

   On success, *s is assigned to the current scope and 0 is returned.
   On error *s is not modified and one of the following are returned:

   CWAL_RC_MISUSE: one of the arguments is NULL.

   CWAL_RC_RANGE: e currently has no scope.
*/
int cwal_scope_current( cwal_engine * e, cwal_scope ** s );
int cwal_scope_current( cwal_engine * e, cwal_scope ** s ){
  if(!e || !s) return CWAL_RC_MISUSE;
  else if(!e->current) return CWAL_RC_RANGE;
  else{
    *s = e->current;
    return CWAL_RC_OK;
  }
}

cwal_scope * cwal_scope_current_get( cwal_engine * e ){
  return e ? e->current : 0;
}



cwal_size_t cwal_type_id_sizeof( cwal_type_id id ){
  switch(id){
    case CWAL_TYPE_BOOL:
    case CWAL_TYPE_UNDEF:
    case CWAL_TYPE_NULL:
      return 0;
#define SCV sizeof(cwal_value)
    case CWAL_TYPE_STRING: return SCV+sizeof(cwal_string);
    case CWAL_TYPE_UNIQUE: return SCV+sizeof(cwal_value*);
    case CWAL_TYPE_TUPLE: return SCV+sizeof(cwal_tuple);
    case CWAL_TYPE_INTEGER: return SCV+sizeof(cwal_int_t);
    case CWAL_TYPE_DOUBLE: return SCV+sizeof(cwal_double_t);
    case CWAL_TYPE_ARRAY: return SCV+sizeof(cwal_array);
    case CWAL_TYPE_OBJECT: return SCV+sizeof(cwal_object);
    case CWAL_TYPE_NATIVE: return SCV+sizeof(cwal_native);
    case CWAL_TYPE_BUFFER: return SCV+sizeof(cwal_buffer_obj);
    case CWAL_TYPE_FUNCTION: return SCV+sizeof(cwal_function);
    case CWAL_TYPE_EXCEPTION: return SCV+sizeof(cwal_exception);
    case CWAL_TYPE_HASH: return SCV+sizeof(cwal_hash);
    case CWAL_TYPE_SCOPE: return sizeof(cwal_scope);
    case CWAL_TYPE_KVP: return sizeof(cwal_kvp);
    case CWAL_TYPE_WEAKREF: return sizeof(cwal_weakref);
    case CWAL_TYPE_PROPREF: return SCV+sizeof(cwal_propref);;
    case CWAL_TYPE_XSTRING:
    case CWAL_TYPE_ZSTRING:
      return SCV + sizeof(cwal_string) + sizeof(char **);
    case CWAL_TYPE_LISTMEM: return 0;
#undef SCV
    default:
      return 0;
  }
}

/**
   Allocates a new value of the specified type. The value is pushed
   onto e->current, effectively transfering ownership to that scope.

   extra is a number of extra bytes to allocate after the "concrete
   type part" of the allocation. It is only valid for type
   CWAL_TYPE_STRING, and must be the length of the string to allocate
   (NOT included the terminating NUL byte - this function adds that
   byte itself).

   The returned value->vtab member will be set appropriately and.  Use
   the internal CWAL_VVPCAST() family of macros to convert the
   cwal_values to their corresponding native representation.

   Returns NULL on allocation error or if adding the new value
   to s fails.

   @see cwal_new_array()
   @see cwal_new_object()
   @see cwal_new_string()
   @see cwal_new_integer()
   @see cwal_new_double()
   @see cwal_new_function()
   @see cwal_new_native()
   @see cwal_new_buffer()
   @see cwal_new_hash()
   @see cwal_new_unique()
   @see cwal_new_tuple()
   @see cwal_new_propref()
   @see cwal_value_unref()
*/
static cwal_value * cwal_value_new(cwal_engine * e,
                                   cwal_type_id t, cwal_size_t extra){
  enum { vsz = sizeof(cwal_value) };
  const cwal_size_t sz = vsz + extra /* base amount of memory to allocate */;
  cwal_size_t tx = 0 /* number of extra bytes to allocate for the
                        concrete value type */;
  cwal_value * v = NULL;
  cwal_value_vtab const * const vtab = cwal__value_vtab(t);
  assert( vtab && vtab->typeID != CWAL_TYPE_UNDEF );
  switch(t){
    case CWAL_TYPE_DOUBLE:
      assert( 0 == extra );
      tx = sizeof(cwal_double_t);
      break;
    case CWAL_TYPE_INTEGER:
      assert( 0 == extra );
      tx = sizeof(cwal_int_t);
      break;
    case CWAL_TYPE_STRING:{
      assert( (extra > 0) && "empty strings are handled elsewhere" );
      tx = sizeof(cwal_string) + 1 /*NUL byte*/;
      if(CwalConsts.StringPadSize){
        int const pad = extra % CwalConsts.StringPadSize;
        if(pad) tx += CwalConsts.StringPadSize - pad;
      }
      break;
    }
    case CWAL_TYPE_XSTRING:
    case CWAL_TYPE_ZSTRING:
      assert( !extra && "x/z-string length is handled elsewhere" );
      tx = sizeof(cwal_string) + sizeof(unsigned char **)
        /* x/z-strings are stored like
           (cwa_value+cwal_string+(cstring-ptr)), and we stuff
           the external string pointer in the cstring-ptr part.
        */;
      break;
    case CWAL_TYPE_ARRAY:
      assert( 0 == extra );
      tx = sizeof(cwal_array);
      break;
    case CWAL_TYPE_OBJECT:
      assert( 0 == extra );
      tx = sizeof(cwal_object);
      break;
    case CWAL_TYPE_FUNCTION:
      assert( 0 == extra );
      tx = sizeof(cwal_function);
      break;
    case CWAL_TYPE_NATIVE:
      assert( 0 == extra );
      tx = sizeof(cwal_native);
      break;
    case CWAL_TYPE_EXCEPTION:
      assert( 0 == extra );
      tx = sizeof(cwal_exception);
      break;
    case CWAL_TYPE_BUFFER:
      assert( 0 == extra );
      tx = sizeof(cwal_buffer_obj);
      break;
    case CWAL_TYPE_HASH:
      assert( 0 == extra );
      tx = sizeof(cwal_hash);
      break;
    case CWAL_TYPE_UNIQUE:
      assert( 0 == extra );
      tx = sizeof(cwal_value*);
      break;
    case CWAL_TYPE_TUPLE:
      assert( 0 == extra );
      tx = sizeof(cwal_tuple);
      break;
    case CWAL_TYPE_PROPREF:
      assert( 0 == extra );
      tx = sizeof(cwal_propref);
      break;
    default:
      assert(0 && "Unhandled type in cwal_value_new()!");
      /* FIXME: set e error state here. */
      return NULL;
  }
  /* See if one of the recycle bins can serve the request... */
  if(CWAL_TYPE_STRING == t){
    v = cwal_string_from_recycler( e, extra );
  }
  else{
    cwal_recycler * re =
      cwal_recycler_get( e, t /*vtab->typeID (wrong for x/z-strings) */ );
    /* MARKER(("BIN #%d(%s)\n", recycleListIndex, vtab->typeName)); */
    if(re){
      if(!re->count){
        ++e->metrics.valuesRecycleMisses;
        ++re->misses;
      }else{
        /* Recycle (take) the first entry from the list. */
        ++re->hits;
        ++e->metrics.valuesRecycled;
        /* MARKER(("BIN #%d(%s) LENGTH=%u\n", recycleListIndex, cwal_type_id_name(t), (unsigned)re->count)); */
        v = (cwal_value*)re->list;
        assert(NULL != v);
        re->list = cwal_value_snip( v );
        --re->count;
        CWAL_TR_V(e,v);
        CWAL_TR3(e,CWAL_TRACE_MEM_FROM_RECYCLER,
                 "RECYCLED FROM BIN.");
        assert(CWAL_RCFLAG_HAS(v, CWAL_RCF_IS_RECYCLED));
        CWAL_RCFLAG_OFF(v,CWAL_RCF_IS_RECYCLED);
        assert(!CWAL_RCFLAG_HAS(v, CWAL_RCF_IS_RECYCLED));
        *v = *cwal_value_undefined()/*needed for types which share a recycling bin*/;
        v->vtab = vtab;
      }
    }
  }
  ++e->metrics.requested[t];
  if(!v){
    /* Check the memchunk for an exact-fit match.  This saves a
       small handful of allocs and total memory, but increases the
       search/miss ratio notably, going from ~5% misses to ~20% misses
       in the s2 amalgamated unit tests.
    */
    cwal_size_t reqSize = sz+tx;
    v = (e->reChunk.config.useForValues
         && e->reChunk.head
         && e->reChunk.head->size<=reqSize)
      ? (cwal_value*)cwal_memchunk_request(e, &reqSize, 0,
                                           "cwal_value_new()")
      : 0;
    if(v){
      assert(reqSize==sz+tx);
    }else{
      v = (cwal_value *)cwal_malloc(e, sz+tx);
      if(v){
        ++e->metrics.allocated[t];
        e->metrics.bytes[t] += sz+tx;
      }
    }
  }
  if( v ) {
    int rc = 0;
    *v = *cwal_value_undefined();
    v->vtab = vtab;
    if(tx || extra){
      memset(v+1, 0, tx + extra);
    }
    assert(0 == v->scope);
    assert(0 == CWAL_REFCOUNT(v));

    CWAL_TR_V(e, v);
    CWAL_TR_S(e, s);
    CWAL_TR2(e, CWAL_TRACE_VALUE_CREATED);
    int check = 0;
    rc = cwal_value_xscope( e, e->current, v, &check )
      /* reminder: xscope "cannot fail" in this case except
         on inputs which are corrupt in "just the right way"
         such that they appear valid by this point. */
      ;
    assert(0 == CWAL_REFCOUNT(v));
    assert(e->current == v->scope);
    assert(-1==check);
    assert(0==rc);
    if(rc){
      /* Reminder to self: since the port to linked lists for
         values, this scenario essentially cannot happen for a
         new value. */
      v->vtab->cleanup( e, v );
      cwal_free(e, v);
      v = NULL;
    }
    else if(e->values.prototypes
            /* Will only be false once, while creating
               e->values.prototypes! This also means e->values.prototypes will
               not get the built-in prototype for arrays unless we
               special-case that, but clients do not have access
               to e->values.prototypes, anyway, so that shouldn't be a
               problem/limitation.
            */){
      cwal_obase const * base = CWAL_VOBASE(v);
      if(base){
        cwal_value * proto = cwal_prototype_base_get(e, t);
        if(proto){
          /*MARKER(("Setting %s prototype from base: @%p\n",
            cwal_type_id_name(t), (void const*)proto));*/
          cwal_value_prototype_set(v, proto);
          assert(proto == cwal_value_prototype_get(e, v));
        }
      }
    }
#if !defined(NDEBUG)
    if(v){ /* Sanity-check that the CWAL_VOBASE() and CWAL_VALPART()
              can perform round-trip conversions. If not, much of cwal
              is broken!
           */
      cwal_obase const * baseCheck = CWAL_VOBASE(v);
      if(baseCheck){
        /*MARKER("v=@%p, baseCheck=@%p\n", (void const *)v, (void const *)baseCheck);*/
        assert( CWAL_VALPART(baseCheck) == v );
      }
    }
#endif
  }
  return v;
}

cwal_value * cwal_new_bool( int v ){
  return v ? &CWAL_BUILTIN_VALS.vTrue : &CWAL_BUILTIN_VALS.vFalse;
}

cwal_value * cwal_value_true(){
  return &CWAL_BUILTIN_VALS.vTrue;
}

cwal_value * cwal_value_false(){
  return &CWAL_BUILTIN_VALS.vFalse;
}

cwal_value * cwal_value_null(){
  return &CWAL_BUILTIN_VALS.vNull;
}

cwal_value * cwal_value_undefined(){
  return &CWAL_BUILTIN_VALS.vUndef;
}

cwal_value * cwal_new_integer( cwal_engine * e, cwal_int_t v ){
  if(!e) return NULL;
  if(v>=CWAL_BUILTIN_INT_FIRST &&
     v<= CWAL_BUILTIN_INT_LAST){
    METRICS_REQ_INCR(e,CWAL_TYPE_INTEGER);
    return (cwal_value*) &CWAL_BUILTIN_VALS.memInt[v + -CWAL_BUILTIN_INT_FIRST];
  }
  else{
    cwal_value * c = cwal_value_new(e, CWAL_TYPE_INTEGER,0);
    if( c ){
      *CWAL_INT(c) = v;
    }
    return c;
  }
}

cwal_value * cwal_new_double( cwal_engine * e, cwal_double_t v )
{
  if( CWAL_BUILTIN_VALS.dbls.zero == v ){
    METRICS_REQ_INCR(e,CWAL_TYPE_DOUBLE);
    return CWAL_BUILTIN_VALS.vDbl0;
  }
  else if( CWAL_BUILTIN_VALS.dbls.one == v ){
    METRICS_REQ_INCR(e,CWAL_TYPE_DOUBLE);
    return CWAL_BUILTIN_VALS.vDbl1;
  }
  else if( CWAL_BUILTIN_VALS.dbls.mOne == v ){
    METRICS_REQ_INCR(e,CWAL_TYPE_DOUBLE);
    return CWAL_BUILTIN_VALS.vDblM1;
  }else{
    cwal_value * c = cwal_value_new(e, CWAL_TYPE_DOUBLE, 0);
    if( c ){
      memcpy(CWAL_DBL_NONULL(c), &v, sizeof(cwal_double_t));
    }
    return c;
  }
}

cwal_value * cwal_new_unique( cwal_engine * e, cwal_value * wrapped ){
  cwal_value * v = e ? cwal_value_new(e, CWAL_TYPE_UNIQUE, 0) : 0;
  if(v){
    *CWAL_UNIQUE_VALPP(v) = wrapped;
    if(wrapped){
      cwal_value_ref(wrapped);
      cwal_value_rescope(v->scope, wrapped)
        /* that's just me being overly pedantic. It's not
           possible that wrapped->scope is newer if v->scope
           resp. e->current is running. */;
    }
  }
  return v;
}

cwal_value * cwal_unique_wrapped_get( cwal_value const * v ){
  cwal_value ** rc = CWAL_UNIQUE_VALPP(v);
  return rc ? *rc : 0;
}

int cwal_unique_wrapped_set( cwal_value * v, cwal_value * w ){
  cwal_value ** ch = CWAL_UNIQUE_VALPP(v);
  if(!ch) return CWAL_RC_TYPE;
  else if(v == w) return CWAL_RC_CYCLES_DETECTED;
  else{
    cwal_value * prev = *ch;
    if(prev == w) return 0;
    else{
      *ch = w;
      if(w){
        cwal_value_ref(w);
        cwal_value_rescope(v->scope, w);
      }
      if(prev){
        assert( CWAL_REFCOUNT(prev) || CWAL_MEM_IS_BUILTIN(prev) );
        cwal_value_unref(prev);
      }
      return 0;
    }
  }
}

uint16_t cwal_tuple_length(cwal_tuple const * p){
  return p ? p->n : 0;
}

/**
   Internal helper which sets e's error state to
   CWAL_RC_CYCLES_DETECTED with a message describing the problem. func
   is required to be __func__.
*/
static inline void cwal__propref_cycles(cwal_engine * const e, char const * func){
  cwal_error_set(e, NULL, CWAL_RC_CYCLES_DETECTED,
                 "Loop detected in resolving proprefs via %s().", func);
  assert(e->err.code);
}

/**
   Internal macro to reduce code needed to properly catch cyclic
   propref resolution. It requires a (cwal_engine*) e symbol and
   a (cwal_propref*) p symbol.

   All code which uses these blocks must protect against the condition
   covered by the cwal__propref_is_cyclic condition, which indicates
   cyclic propref resolution. If that condition is true, this macro
   will set e's error state to code CWAL_RC_CYCLES_DETECTED. The
   cyclic case handling is otherwise necessarily call-site-specific.
   After invoking this, checking cwal__propref_is_cyclic is required,
   as is invoking cwal__propref_end.
*/
#define cwal__propref_begin_p \
  bool const _pIsCyclic = CWAL_RCFLAG_HAS(CWAL_VALPART(p), CWAL_RCF_IS_VISITING_ACYCLIC); \
  if(_pIsCyclic) cwal__propref_cycles(e, __func__); \
  else CWAL_RCFLAG_ON(CWAL_VALPART(p), CWAL_RCF_IS_VISITING_ACYCLIC)
/**
   True the propref check started by cwal__propref_begin_p indicates
   that a cyclic property resolution has been discovered.
*/
#define cwal__propref_is_cyclic _pIsCyclic
/**
   Declares a local (cwal_propref * const) p
   in the local scope from CWAL__V2PROPREF(VALUE), which MUST
   resolve to non-NULL. Then invokes cwal__propref_begin_p.
*/
#define cwal__propref_begin_v(VALUE)                       \
  cwal_propref * const p = CWAL__V2PROPREF(VALUE);       \
  assert(p);                                             \
  cwal__propref_begin_p
/**
   Must be invoked to close a cwal__propref_begin_p/begin_v block, from
   the same or a deeper scope.
*/
#define cwal__propref_end \
  if(!cwal__propref_is_cyclic){ \
    CWAL_RCFLAG_OFF(CWAL_VALPART(p), CWAL_RCF_IS_VISITING_ACYCLIC); \
  } (void)0

/** "Optimized alternative to cwal_error_reset(&(e->err)).

    Reminder: just etting err.code to 0 is not sufficient:
    err.msg.used must also be set to 0 or s2 unit tests mysteriously
    fail, and i've not yet figured out where.
*/
#define cwal__err_reset(e) \
  (e)->err.code = 0; (e)->err.msg.used = 0; \
  (e)->err.line = (e)->err.col = 0

static inline int cwal__propref_err_ro(cwal_propref const * const p){
  return cwal_error_set(CWAL_VALPART(p)->scope->e, NULL,
                        CWAL_RC_ACCESS,
                        "This property reference is read-only.");
}
static inline int cwal__propref_err_weak(cwal_propref const * const p){
  return cwal_error_set(CWAL_VALPART(p)->scope->e, NULL,
                        CWAL_RC_NOT_FOUND,
                        "This property reference's target "
                        "has been destroyed.");
}

static inline bool cwal__propref_is_weak(cwal_propref const * const p){
  return !!(CWAL__PROPREF_MASK_WEAK & p->flags);
}

/**
   Returns the result of the setter operation appropriate for
   p->target, depending on p->flags. The kvpFlags arg is ignored
   unless  p->flags indicates CWAL_PROPREF_PROPERTY.

   If bypassCycleCheck is false then the check for is-this-cyclic is
   skipped. Only do that when the calling code has already performed
   that check, e.g. using cwal__propref_begin/cwal__propref_end.

   If bypassCycleCheck is false and resolving p would introduce cyclic
   lookups, CWAL_RC_CYCLES_DETECTED and v is not set.
*/
static int cwal__propref_set(cwal_propref const * const p,
                             bool const bypassCycleCheck,
                             cwal_value * const v,
                             cwal_flags16_t const kvpFlags ){
  int rc = 0;
  cwal_engine * const e = CWAL_VALPART(p)->scope->e;
  assert(e);
  cwal__err_reset(e);
  cwal_value * const c = cwal_propref_container(p);
  if(!c){
    assert(cwal__propref_is_weak(p));
    return cwal__propref_err_weak(p);
  }
  bool cyclic = false;
  if(bypassCycleCheck) goto gogo_one;
  cwal__propref_begin_p;
  cyclic = cwal__propref_is_cyclic;
  gogo_one:
  if(!cyclic && (CWAL__PROPREF_MASK_RO & p->flags)){
    /* ^^^^ we want the cyclic error to have precedence and it will be
       checked below */
    rc = cwal__propref_err_ro(p);
  }
  switch((rc || cyclic) ? -1 : (CWAL__PROPREF_MASK_TYPE & p->flags)){
    case -1:
      if(!rc){
        rc = e->err.code;
        if(!rc){
          cwal__fatal(CWAL_RC_CANNOT_HAPPEN,
                      "We have no error code but should. bypassCycleCheck=%d",
                      (int)bypassCycleCheck);
        }
      }
      break;
    case CWAL_PROPREF_PROPERTY:
      rc = cwal_prop_set_with_flags_v(c, p->key, v, kvpFlags);
      break;
    case CWAL_PROPREF_LIST: {
      cwal_array * const a = CWAL_ARRAY(c);
      assert(a);
      cwal_int_t const i = cwal_value_get_integer(p->key);
      if(i<0){
        rc = cwal_error_set(e, NULL, CWAL_RC_RANGE,
                            "Property reference to an array may "
                            "not have a negative index.");
      }else{
        rc = cwal_array_set(a, (cwal_midsize_t)i, v);
      }
      break;
    }
    case CWAL_PROPREF_INTERCEPTOR: {
      cwal_function * const f = cwal_value_get_function(p->xtra);
      assert(f);
      cwal_value * arg = v ? v : cwal_value_undefined();
      cwal_ref(arg);
      rc = cwal_function_call2(f, CWAL_VALPART(p), c, NULL, 1, &arg);
      cwal_unhand(arg);
      break;
    }
    default:
      cwal__fatal(CWAL_RC_ASSERT,
                  "Cannot happen: unhandled propref flags 0x%04x.",
                  (int)p->flags);
      /* not reached */
  }
  if(!bypassCycleCheck){
    /* It seems likely that one of these days some compiler or other
       is going to complain about cwal__propref_end accessing a var
       which "might not have been initialized," because it will
       mis-diagnose that case because of the gogo_one jump up
       above. */
    cwal__propref_end;
  }
  return rc;
}

int cwal_propref_set(cwal_propref const * const p, cwal_value * const v){
  return cwal__propref_set(p, false, v, CWAL_VAR_F_PRESERVE);
}

int cwal_propref_resolve(cwal_propref const * const p,
                         cwal_value * const propertyHolder, cwal_value **rv){
  cwal_value * v = NULL;
  assert(p);
  cwal_value * const c = cwal_propref_container(p);
  if(!c){
    assert(cwal__propref_is_weak(p));
    return cwal__propref_err_weak(p);
  }
  cwal_engine * const e = CWAL_VALPART(p)->scope->e;
  assert(e);
  cwal__err_reset(e);
  cwal__propref_begin_p;
  int rc = 0;
  switch(cwal__propref_is_cyclic
         ? -1
         : (CWAL__PROPREF_MASK_TYPE & p->flags)){
    case -1:
      assert(CWAL_RC_CYCLES_DETECTED==e->err.code);
      break;
    case CWAL_PROPREF_PROPERTY:
      v = cwal_prop_get_v(c, p->key);
      break;
    case CWAL_PROPREF_LIST: {
      cwal_array * const a = CWAL_ARRAY(c);
      assert(a);
      cwal_int_t const i = cwal_value_get_integer(p->key);
      v = i<0 ? NULL : cwal_array_get(a, (cwal_midsize_t)i);
      break;
    }
    case CWAL_PROPREF_INTERCEPTOR: {
      cwal_function * const f = cwal_value_get_function(p->xtra);
      assert(f);
      rc = cwal_function_call2(f, propertyHolder ? propertyHolder : CWAL_VALPART(p),
                               c, &v, 0, NULL);
      break;
    }
    default:
      cwal__fatal(CWAL_RC_ASSERT,
                  "Cannot happen: unhandled propref flags 0x%04x.",
                  (int)p->flags);
      /* not reached */
  }
  cwal__propref_end;
  if(!rc) rc = e->err.code;
  if(rc){
    if(v) cwal_refunref(v);
  }else{
    *rv = v;
  }
  return rc;
}

int cwal_tuple_set_v2(cwal_tuple * const t, uint16_t n, cwal_value * const v,
                      bool resolvePropref){
  if(n>=t->n) return CWAL_RC_RANGE;
  else{
    cwal_value * const self = CWAL_VALPART(t);
    cwal_value * const ch = t->list[n];
    if(ch && resolvePropref && CWAL_TYPE_PROPREF==ch->vtab->typeID){
      cwal_propref * const p = CWAL__V2PROPREF(ch);
      return cwal__propref_set(p, false, v, CWAL_VAR_F_PRESERVE);
    }
    if(v) cwal_value_ref(v);
    if(ch) cwal_value_unref(ch);
    t->list[n] = v;
    if(v) cwal_value_rescope(self->scope, v);
    return 0;
  }
}

int cwal_tuple_set(cwal_tuple * const t, uint16_t n, cwal_value * const v){
  return cwal_tuple_set_v2(t, n, v, true);
}

cwal_value * cwal_tuple_get_v2(cwal_tuple const * t, uint16_t n,
                               bool resolvePropref){
  if(n>=t->n) return NULL;
  cwal_value * v = n<t->n ? t->list[n] : NULL;
  if(v && resolvePropref && CWAL_TYPE_PROPREF==v->vtab->typeID){
    cwal_propref * const p = CWAL__V2PROPREF(v);
    v = NULL;
    cwal_propref_resolve(p, CWAL_VALPART(t), &v);
  }
  return v;
}

cwal_value * cwal_tuple_get(cwal_tuple const * p, uint16_t n){
  return cwal_tuple_get_v2(p, n, true);
}

cwal_tuple * cwal_new_tuple( cwal_engine * e, uint16_t n ){
  if(!n){
    assert(CWAL_MEM_IS_BUILTIN(CWAL_BUILTIN_VALS.vTuple0));
    return CWAL_TUPLE(CWAL_BUILTIN_VALS.vTuple0);
  }else{
    cwal_tuple * p = 0;
    cwal_value * v = e
      ? cwal_value_new(e, CWAL_TYPE_TUPLE, 0)
      : 0;
    if(v){
      cwal_size_t const reqSize = sizeof(cwal_value*) * n;
      p = CWAL_TUPLE(v);
      assert(p);
      p->list = (cwal_value**)cwal_malloc2(e, reqSize);
      if(!p->list){
        cwal_value_unref(v);
        p = 0;
      }else{
        memset(p->list, 0, reqSize);
        p->n = n;
      }
    }
    return p;
  }
}

cwal_value * cwal_new_tuple_value( cwal_engine * e, uint16_t n ){
  cwal_tuple * p = cwal_new_tuple(e, n);
  return CWAL_VALPART(p);
}

cwal_value * cwal_tuple_value( cwal_tuple const * p ){
  return CWAL_VALPART(p);
}

cwal_tuple * cwal_value_get_tuple( cwal_value * v ){
  return CWAL_TUPLE(v);
}

#if 0
/* Not needed because tuples cannot be prototypes. Maybe
   someday. */
cwal_tuple * cwal_tuple_part( cwal_engine * e,
                              cwal_value * v ){
  cwal_tuple * p;
  do{
    if( (p = CWAL_TUPLE(v)) ) return p;
    else v = cwal_value_prototype_get(e, v);
  }while(v);
  return NULL;
}
#endif

int cwal_tuple_visit( cwal_tuple * const o, cwal_value_visitor_f f,
                      void * const state ){
  cwal_value * const tv = CWAL_VALPART(o);
  if(!tv || !f) return CWAL_RC_MISUSE;
  /*else if(CWAL_OB_IS_LOCKED(o)) return CWAL_RC_LOCKED;*/
  else if(!o->n) return 0;
  else {
    uint16_t i;
    cwal_value * v;
    int rc = CWAL_RC_OK;
    int opaque = 0;
    cwal_visit_list_begin(tv, &opaque);
    cwal_value_ref(tv);
    for( i = 0; i < o->n && !rc; ++i ){
      if((v = o->list[i])){
        cwal_value_ref(v);
        if(CWAL_TYPE_PROPREF==v->vtab->typeID){
          cwal_propref * const p = CWAL__V2PROPREF(v);
          cwal_value * vr = NULL;
          rc = cwal_propref_resolve(p, CWAL_VALPART(o), &vr);
          if(0==rc && vr) cwal_ref(vr);
          cwal_unref(v);
          if(rc) break;
          v = vr;
        }
      }
      rc = f( v, state );
      if(v) cwal_value_unref(v);
    }
    cwal_value_unhand(tv);
    cwal_visit_list_end(tv, opaque);
    return rc;
  }
}

cwal_value * cwal_new_array_value(cwal_engine *e){
  cwal_value * v = (e && e->current)
    ? cwal_value_new(e, CWAL_TYPE_ARRAY,0)
    : 0;
  if( NULL != v ){
    cwal_array * ar = CWAL_ARRAY(v);
    cwal_value * proto = ar->base.prototype;
    *ar = cwal_array_empty;
    ar->base.prototype = proto;
  }
  return v;
}

cwal_array * cwal_new_array(cwal_engine *e){
  return cwal_value_get_array(cwal_new_array_value(e));
}

cwal_value * cwal_new_object_value(cwal_engine *e){
  cwal_value * v = (e && e->current)
    ? cwal_value_new(e, CWAL_TYPE_OBJECT,0)
    : 0;
  if( NULL != v )
  {
    cwal_object * ar = CWAL_OBJ(v);
    cwal_value * proto = ar->base.prototype;
    *ar = cwal_object_empty;
    ar->base.prototype = proto;
  }
  return v;
}

cwal_object * cwal_new_object(cwal_engine *e){
  return cwal_value_get_object(cwal_new_object_value(e));
}

/**
   If li->list is not 0 and e has space in its list memory recycler,
   the contents of li are moved into the recycler and *li is reset to a
   clean state. If li->list but the recycler has no slots, the memory
   is freed instead.
*/
static void cwal_list_to_recycler( cwal_engine * e, cwal_list * li );

/**
   Frees all cwal_kvp entries in the given htable. If freeList
   is true then it also frees up htable->list.list.
*/
static void cwal_cleanup_htable( cwal_engine * const e,
                                 cwal_htable * const htable,
                                 bool freeList){
  cwal_kvp * kvp = 0;
  cwal_kvp * next = 0;
  for( cwal_midsize_t i = 0;
       htable->list.count && (i < htable->hashSize);
       ++i ){
    kvp = (cwal_kvp*)htable->list.list[i];
    htable->list.list[i] = next = NULL;
    for(; kvp; kvp = next){
      assert(htable->list.count>0);
      next = kvp->right;
      kvp->right = 0;
      cwal_kvp_free( e, kvp, 1 );
      --htable->list.count;
    }
  }
  assert(0==htable->list.count);
  if(freeList){
    cwal_list_to_recycler(e, &htable->list);
    assert(0==htable->list.count);
    assert(0==htable->list.alloced);
    assert(0==htable->list.list);
  }
}

/**
   Cleanup routine for the cwal_obase part of classes which use that.
   Cleans up the contents of b->kvp and sets b->kvp to NULL.  Also, if
   isFinalCleanup is true, it assumes final cleanup is underway and it
   frees the property-storage memory and, clears the
   CWAL_CONTAINER_DISALLOW_PROTOTYPE_SET flag from b->flags, and
   unrefs/NULLs b->prototype.
*/
static void cwal_cleanup_obase( cwal_engine * e, cwal_obase * b, bool isFinalCleanup ){
#if CWAL_OBASE_ISA_HASH
  cwal_cleanup_htable(e, &b->hprops, isFinalCleanup);
#else
  while(b->kvp){
    cwal_kvp * kvp = b->kvp;
    cwal_kvp * next = 0;
    b->kvp = 0;
    /* In theory the is-visiting flag is not needed here because
       b->kvp=0 means we cannot possibly traverse the property
       list as a side-effect of this cleanup. Well, we cannot
       travse THIS property list. We can only hope that cleanup
       does not then add properties to b->kvp, but that's why we
       while(b->kvp).
    */
    /* b->flags |= CWAL_F_IS_VISITING; */
    for( ; kvp; kvp = next ){
      next = kvp->right;
      kvp->right = 0;
      /* if(kvp->key==bv) kvp->key = 0; */
      /* if(kvp->value==bv) kvp->value = 0; */
      cwal_kvp_free( e/* , bv->scope */, kvp, 1 );
    }
    /* b->flags &= ~CWAL_F_IS_VISITING; */
  }
#endif
  if( isFinalCleanup ){
    b->flags &= ~CWAL_CONTAINER_DISALLOW_PROTOTYPE_SET;
    if( b->prototype ){
      cwal_value_prototype_set( CWAL_VALPART(b), NULL );
    }
  }
}

/**
   If table->hashSize is 0, initialize the table for hashSize
   elements, or CwalConsts.ScopePropsHashSize if hashSize==0. Use this
   only to initialize an empty table. Returns 0 on success,
   CWAL_RC_OOM on error.
*/
static int cwal_htable_alloc( cwal_engine * const e, cwal_htable * const table,
                              cwal_midsize_t hashSize ){
  if(table->hashSize && table->list.alloced){
    assert(table->list.alloced >= table->hashSize);
    assert(table->hashSize >= hashSize);
    return 0;
  }else{
    assert(!table->list.alloced);
    if(!hashSize) hashSize = CwalConsts.DefaultHashtableSize;
    int rc = 0;
    if(hashSize > cwal_list_reserve(e, &table->list, hashSize)){
      rc = CWAL_RC_OOM;
    }else{
      table->hashSize = hashSize;
    }
    return rc;
  }
}

/** @internal

    Internal impl for cwal_list-of-(cwal_kvp*) hashtable search. If
    tableIndex is not NULL then *tableIndex is assigned to the hash
    table index for the given key, even on search failure. `*htable`
    must initially be set to the hashtable we want to search for.

    If a match is found then its kvp entry is returned and if left is
    not NULL then *left will point to the kvp immediately to the left
    of the returned kvp in the hash table (required for re-linking
    collisions).

    This function is intollerant of NULLs for (htable,key).

    If resolvePropref is true then if a match is found and its value
    is a propref, this function returns the result of resolving that
    propref, updating `*container`, `*htable`, `*tableIndex` (if not
    NULL), and `*left` (if not NULL) to refer to the the kvp from the
    updated `*htable` (that kvp's owner). Be very aware that those
    values return to the container the resolved prop was actually
    found in, and _not_ the proxied hashtable. If a loop is detected
    in propref processing then it will return NULL but will update the
    engine's error state with the code CWAL_RC_CYCLES_DETECTED.

    In other words: if resolvePropref is true, this function might
    return NULL even if it finds something (a propref which resolves
    to NULL). If resolvePropref is false, `*container`, `*htable` and
    `*left` will always refer to the call-time hashtable.

    The container argument may be NULL. If resolvePropref is false
    then it is ignored. If it is NOT NULL then it must initially point
    to the owning container of htable. If propref resolution proxies a
    value then `*container` (if not NULL) gets set to the container in
    which the value was actually resolved from.

    ACHTUNG: the returned kvp is owned by `*htable`, which _may_ (if
    resolvePropref is true) differ from the call-time `*htable` value.

    Sidebar: the first parameter "should" be a pointer to a const
    htable, as this function never modifies the table, but we need
    non-const in a case or two.
*/
static cwal_kvp * cwal_htable_search_impl_v( cwal_value ** container,
                                             cwal_htable const ** htable,
                                             bool resolvePropref,
                                             cwal_value const * key,
                                             cwal_midsize_t * tableIndex,
                                             cwal_kvp ** left ){
  cwal_htable const * ht = *htable;
  if(!ht->hashSize) return NULL;
  cwal_midsize_t const ndx = cwal_value_hash( key ) % ht->hashSize;
  if(!ht->list.count){
    if(left) *left = 0;
    if(tableIndex) *tableIndex = ndx;
    return NULL;
  }
  cwal_type_id const kType = key->vtab->typeID /*cwal_value_type_id(key)*/;
  cwal_kvp * kvp = (cwal_kvp*)ht->list.list[ndx];
  //t2 = args->argv[ndx++];
  //dump_val(key, "cwal_htable_search_impl_v() key");
  //MARKER(("ndx=%d, list->count=%d, kvp=%p\n", (int)ndx, (int)ht->list.count, (void const*)kvp));
  if(tableIndex) *tableIndex = ndx;
  for( ; kvp; (left?(*left=kvp):NULL), kvp = kvp->right ){
    assert(kvp->key);
#if 0
    if(kvp){
      dump_val(kvp->key,"cwal_htable_search_impl_v() found key");
      dump_val(kvp->value,"cwal_htable_search_impl_v() found value");
    }
#endif
    if(kvp->key==key) goto got_one;
    else if(kType != kvp->key->vtab->typeID
            /*cwal_value_type_id(kvp->key)*/){
      //dump_val(kvp->key,"cwal_htable_search_impl_v() type mismatch. Skipping");
      continue;
    }
    else if(0==key->vtab->compare(key, kvp->key)){
      //MARKER(("returning ndx=%d, kvp=%p\n", (int)ndx, (void const*)kvp));
      goto got_one;
    }
  }
  return NULL;
  got_one:
  assert(kvp);
  switch(kvp->value->vtab->typeID){
    default: break;
    case CWAL_TYPE_PROPREF:
      if(resolvePropref){
        cwal_engine * const e = kvp->value->scope->e;
        cwal__propref_begin_v(kvp->value);
        if(cwal__propref_is_cyclic){
          kvp = NULL;
        }else{
          //dump_val(c, "p->container"); dump_val(p->key, "p->key");
          cwal_value * const pc = cwal_propref_container(p);
          if(!pc){
            assert(cwal__propref_is_weak(p));
            cwal__propref_err_weak(p);
            return NULL;
          }
          cwal_obase * const b = CWAL_VOBASE(pc);
          assert(b);
          cwal_htable const * pht = &b->hprops;
          if(container) *container = pc;
          kvp = cwal_htable_search_impl_v(container, &pht, true, p->key,
                                          tableIndex, left);
          if(kvp) *htable = pht;
        }
        cwal__propref_end;
      }
  }
  return kvp;
}

/**
   C-string conterpart of cwal_htable_search_impl_v().
*/
static cwal_kvp * cwal_htable_search_impl_cstr( cwal_value ** container,
                                                cwal_htable const ** htable,
                                                bool resolvePropref,
                                                char const * key,
                                                cwal_midsize_t keyLen,
                                                cwal_midsize_t * tableIndex,
                                                cwal_kvp ** left ){
  cwal_htable const * const ht = *htable;
  if(!ht->hashSize) return NULL;
  cwal_hash_t const ndx =
    cwal_hash_bytes( key, keyLen ) % ht->hashSize;
  if(!ht->list.count){
    if(left) *left = 0;
    if(tableIndex) *tableIndex = ndx;
    return NULL;
  }
  cwal_kvp * kvp = (cwal_kvp*)ht->list.list[ndx];
  cwal_string const * sk;
#if 0
  MARKER(("%s() hash entries=%d, ndx=%d, key=%.*s, hash=%08x, kvp=%p\n", __func__,
          (int)ht->list.count, (int)ndx, (int)keyLen, key,
          (unsigned)cwal_hash_bytes( key, keyLen ),
          (void *)kvp));
#endif
  if(tableIndex) *tableIndex = ndx;
  for( ; kvp; (left?(*left=kvp):NULL), kvp = kvp->right ){
    assert(kvp->key);
    //dump_val(kvp->key, "Comparing to this key.");
    if(CWAL_TYPE_STRING != cwal_value_type_id(kvp->key)) continue;
    sk = cwal_value_get_string(kvp->key);
    if(keyLen != CWAL_STRLEN(sk)) continue;
    else if(0!=cwal_compare_str_cstr(sk, key, keyLen)) continue;
    switch(kvp->value->vtab->typeID){
      default: break;
      case CWAL_TYPE_PROPREF:
        if(resolvePropref){
          //cwal_engine * const e = kvp->value->scope->e;
          cwal_engine * const e = kvp->value->scope->e;
          cwal__propref_begin_v(kvp->value);
          //dump_val(p->container, "p->container"); dump_val(p->key, "p->key");
          if(cwal__propref_is_cyclic){
            kvp = NULL;
          }else{
            //dump_val(p->container, "p->container"); dump_val(p->key, "p->key");
            cwal_value * const pc = cwal_propref_container(p);
            if(!pc){
              assert(cwal__propref_is_weak(p));
              cwal__propref_err_weak(p);
              return NULL;
            }   
            cwal_obase * const b = CWAL_VOBASE(pc);
            assert(b);
            cwal_htable const * pht = &b->hprops;
            if(container) *container = pc;
            kvp = cwal_htable_search_impl_cstr(container, &pht, true, key,
                                               keyLen, tableIndex, left);
            if(kvp) *htable = pht;
          }
          cwal__propref_end;
        }
        break;
    }
    return kvp;
  }
  return NULL;
}

static cwal_kvp * cwal__prop_get_kvp( bool resolvePropref,
                                      cwal_value * c, char const * key,
                                      cwal_midsize_t keyLen, bool searchProtos,
                                      cwal_value ** foundIn ){
  cwal_obase * b = CWAL_VOBASE(c);
  if(!key || !*key || !b) return 0;
  else{
    cwal_kvp * kvp = 0;
    while(b){
#if CWAL_OBASE_ISA_HASH
      cwal_htable const * pht = &b->hprops;
      cwal_value * vOwner = c;
      kvp = cwal_htable_search_impl_cstr(&vOwner, &pht, resolvePropref,
                                         key, keyLen, NULL, NULL);
      if(kvp){
        if(foundIn) *foundIn = vOwner;
        break;
      }
#else
      if(resolvePropref){/*unused*/}
      kvp = cwal_kvp_search(b->kvp, key, keyLen, NULL);
      if(kvp){
        if(foundIn) *foundIn = c;
        break;
      }        
#endif
      else if(searchProtos
              && (c = b->prototype)
              && (b = CWAL_VOBASE(c))){
        continue;
      }
      b = 0;
    }while(0);
    return kvp;
  }
}

static cwal_kvp * cwal__prop_get_kvp_v( bool resolvePropref,
                                        cwal_value * c, cwal_value const * key,
                                        bool searchProtos,
                                        cwal_value ** foundIn ){
  cwal_obase * b = CWAL_VOBASE(c);
  if(!key || !b) return 0;
  else{
    cwal_kvp * kvp = 0;
    while(b){
#if CWAL_OBASE_ISA_HASH
      cwal_htable const * pht = &b->hprops;
      cwal_value * vOwner = c;
      kvp = cwal_htable_search_impl_v(&vOwner, &pht, resolvePropref,
                                      key, NULL, NULL);
      assert(resolvePropref ? 1 : pht == &b->hprops);
      if(kvp){
        if(foundIn) *foundIn = vOwner;
        break;
      }
#else
      if(resolvePropref){/*unused*/}
      kvp = cwal_kvp_search_v(b->kvp, key, NULL);
      if(kvp){
        if(foundIn) *foundIn = c;
        break;
      }
#endif
      else if(searchProtos
               && (c = b->prototype)
               && (b = CWAL_VOBASE(c))){
        continue;
      }
      b = 0;
    }
    return kvp;
  }
}

/**
   The cwal_htable counterpart of the public API's
   cwal_hash_insert_with_flags_v(). The isResizing flag must only be
   true when this insert is called during the course of
   cwal_htable_resize(). That parameter is not strictly needed: it's
   used as a sanity-checking measure for a case which could, if
   internally used incorrectly, lead to infinite recursion (stack
   overflow).

   If resolvePropref is true then if a match is found and its value is
   a propref, this function returns the result of setting that
   propref's proxy property. In that case, it may return
   CWAL_RC_CYCLES_DETECTED if propref resolution detects a loop.
*/
static int cwal_htable_insert_impl_v( cwal_value * const container,
                                      cwal_htable * const table,
                                      bool resolvePropref,
                                      cwal_value * const key, cwal_value * const v,
                                      bool allowOverwrite, uint16_t kvpFlags,
                                      bool isResizing );
/**
   The cwal_htable counterpart of the public API's
   cwal_hash_grow_if_loaded(). If load<=0 then
   CwalConsts.PreferredHashLoad is used. htable is assumed to be
   a component of the given container.
*/
static int cwal_htable_grow_if_loaded( cwal_value * const container,
                                       cwal_htable * htable, double load );

/**
   Assumes table is used as a hashtable for the value hv. The table
   is, if needed, resized to toSize. Returns 0 on success, CWAL_RC_OOM
   on OOM.
*/
static int cwal_htable_resize( cwal_value * const hv,
                               cwal_htable * const table,
                               cwal_midsize_t toSize ){
  if(!hv) return CWAL_RC_MISUSE;
  else if(!toSize) return CWAL_RC_RANGE;
  //else if(CWAL_V_IS_VISITING_LIST(hv)) return CWAL_RC_IS_VISITING_LIST;
  else if(toSize==table->hashSize) return 0;
  /* highly arguable: toSize = cwal_trim_hash_size( toSize ); */
  cwal_list li = cwal_list_empty;
  int rc = 0;
  cwal_engine * e = hv->scope->e;
  cwal_midsize_t i;
  //cwal_midsize_t const oldSize = table->hashSize;
#if !defined(NDEBUG)
  cwal_midsize_t const oldCount = table->list.count;
#endif
  //MARKER(("Resizing htable @%p from %d to %d\n", (void *)table, (int)table->hashSize, (int)toSize));
  //dump_val(hv, "hv htable holder");
  if(toSize > cwal_list_reserve(e, &li, toSize)){
    assert(!li.list);
    return CWAL_RC_OOM;
  }
  {
    /* Swap the table memory */
    cwal_list const tmp = table->list;
    table->list = li;
    li = tmp;
  }
  /* Iterate over li.list and move all entries into table->list,
     assigning them to a new index, as appropriate. */
  table->hashSize = toSize;
  assert(toSize <= table->list.alloced);
  assert(!table->list.count);
  assert(li.alloced ? !!li.list : !li.list);
  for( i = 0; li.count; ++i ){
    cwal_kvp * kvp = (cwal_kvp*)li.list[i];
    cwal_kvp * next = 0;
    li.list[i] = 0;
    for( ; !rc && kvp; kvp = next ){
      cwal_value * const k = kvp->key;
      cwal_value * const v = kvp->value;
      assert(V_SEEMS_OK(k));
      assert(V_SEEMS_OK(v));
      next = kvp->right;
      /* kvp holds refs to k/v, but we're going to steal them... */
      kvp->right = 0;
      kvp->key = 0;
      kvp->value = 0;
      /* *kvp = cwal_kvp_empty; must retain flags */
      e->values.hashXfer = kvp;
#if !defined(NDEBUG)
      cwal_midsize_t const cCheck = table->list.count;
#endif
      rc = cwal_htable_insert_impl_v( hv, table, false, k, v,
                                      false, kvp->flags, true );
      assert(!rc) /* cannot fail under these conditions - no allocation */;
      assert(0==e->values.hashXfer);
      assert(kvp->key == k);
      assert(kvp->value == v);
      assert(table->list.count==cCheck+1);
      /* We got an extra ref on k/v up there, so... */
      assert(CWAL_REFCOUNT(k)>1 || CWAL_MEM_IS_BUILTIN(k));
      assert(CWAL_REFCOUNT(v)>1 || CWAL_MEM_IS_BUILTIN(v));
      cwal_value_unref(k);
      cwal_value_unref(v);
      assert(CWAL_MEM_IS_BUILTIN(k) || k->scope);
      assert(CWAL_MEM_IS_BUILTIN(v) || v->scope);
      assert(V_SEEMS_OK(k));
      assert(V_SEEMS_OK(v));
      --li.count;
    }
  }
  assert(0==li.count);
#if defined(DEBUG)
  assert(table->list.count == oldCount);
#endif
  cwal_list_reserve(e, &li, 0);
  return rc;
}

static int cwal_htable_grow_if_loaded( cwal_value * const container,
                                       cwal_htable * htable, double load ){
  if(!htable->list.count) return 0;
  else if(load<=0) load = CwalConsts.PreferredHashLoad;
  else if(load<0.5) load = 0.5/*kinda arbitrary*/;
  double const hashSize = (double)htable->hashSize;
  double const entryCount = (double)(htable->list.count
                                     ? htable->list.count
                                     : CwalConsts.DefaultHashtableSize);
  int rc = 0;
  assert(hashSize);
#if 0
  /* arguable. If someone wants loads of 5.0, let him. OTOH, values
     approaching or surpassing 1.0 break an upcoming calculation... */
  if(load >= 0.95) load = 0.95;
#endif
  if((entryCount / hashSize) > load){
    cwal_midsize_t const newSize =
      (cwal_midsize_t)cwal_next_prime((cwal_midsize_t)(entryCount * 3 / 2));
    assert(newSize > entryCount);
#if 0
    MARKER(("Resizing hash: container@%p old size=%f, count=%f, Going for load %f, size=%d\n",
            (void const *)container,
            hashSize, entryCount, load, (int)newSize));
#endif
    rc = cwal_htable_resize(container, htable, newSize);
  }
  return rc;
}

int cwal_htable_insert_impl_v( cwal_value * const container,
                               cwal_htable * const htable,
                               bool resolvePropref,
                               cwal_value * const key, cwal_value * const v,
                               bool allowOverwrite, uint16_t kvpFlags,
                               bool isResizing ){
  cwal_midsize_t ndx = 0;
  cwal_kvp * left = 0;
  cwal_kvp * kvp;
  cwal_obase * const base = CWAL_VOBASE(container);
  cwal_scope * const sc = container->scope;
  if(!key || !v) return CWAL_RC_MISUSE;
  else if(CWAL_V_IS_IN_CLEANUP(container)) return CWAL_RC_DESTRUCTION_RUNNING;
#if 0
  // These need to be checked in the higher-level containers which
  // call this
  else if((h = CWAL_HASH(container)) &&
          table==&h->htable
          && CWAL_V_IS_VISITING_LIST(container)){
    /* Genuine hashtable entries */
    return CWAL_RC_IS_VISITING_LIST;
  }
  else if(CWAL_V_IS_VISITING(container)){
    /* base->hprops properties */
    return CWAL_RC_IS_VISITING;
  }
#endif
  else if(!cwal_prop_key_can(key)) return CWAL_RC_TYPE;
  assert(!(CWAL_CONTAINER_DISALLOW_PROP_SET & base->flags)
         && "Expecting this to be checked before this is called.");
  int rc = 0;
  /* Maintenance reminder: we need to init/resize the table before
     searching so that the calculated hash index is valid. There's a
     potential corner case here where we'll grow even if the entry is
     already in the hashtable (so no new space would have been
     needed), but that's not too tragic. Working around that would
     require(?) searching, then resizing if needed, then searching
     _again_ so that we pick up the proper (new) kvp->right and ndx
     values. Not worth it. */
  if(!htable->list.alloced){
    assert(!isResizing && "Table must have already been allocated in this case.");
    rc = cwal_htable_alloc(CWAL_VENGINE(container), htable, 0);
  }else if(!isResizing){
    rc = cwal_htable_grow_if_loaded(container, htable, -1.0);
  }
  if(rc) return rc;
  assert(htable->list.alloced>=htable->hashSize);
  cwal_htable const * pht = htable;
  kvp = cwal_htable_search_impl_v( NULL, &pht, false, key, &ndx, &left );
  assert(pht == htable);
  if(kvp){
    assert(!isResizing);
    if(!allowOverwrite) return CWAL_RC_ALREADY_EXISTS;
    else if(CWAL_VAR_F_CONST & kvp->flags){
      return CWAL_RC_CONST_VIOLATION;
    }
    switch(kvp->value->vtab->typeID){
      default: break;
      case CWAL_TYPE_PROPREF:
        if(resolvePropref){
          /* If a propref is found, simply recurse into this func, passing
             on ref->container and ref->key, and return its result. */
          cwal_engine * const e = kvp->value->scope->e;
          cwal__propref_begin_v(kvp->value);
          if(!cwal__propref_is_cyclic &&
             /* ^^^ is-cyclic check preempts the read-only check */
             (CWAL__PROPREF_MASK_RO & p->flags)){
            rc = cwal__propref_err_ro(p);
          }
          switch((rc || cwal__propref_is_cyclic)
                 ? -1 : (CWAL__PROPREF_MASK_TYPE & p->flags)){
            case -1:
              if(!rc){
                rc = e->err.code;
                if(!rc){
                  cwal__fatal(CWAL_RC_CANNOT_HAPPEN,
                              "We have no error code but should.");
                }
                assert(CWAL_RC_CYCLES_DETECTED==rc);
              }
              break;
            case CWAL_PROPREF_PROPERTY: {
              cwal_value * const pc = cwal_propref_container(p);
              if(!pc){
                assert(cwal__propref_is_weak(p));
                rc = cwal__propref_err_weak(p);
                break;
              }          
              cwal_obase * const b = CWAL_VOBASE(pc);
              assert(b);
              //dump_val(p->container, "p->container"); dump_val(p->key, "p->key");
              //dump_val(v, "v");
              rc = cwal_htable_insert_impl_v(pc, &b->hprops, true,
                                             p->key, v, allowOverwrite, kvpFlags,
                                             false);
              break;
            }
            case CWAL_PROPREF_INTERCEPTOR:
            case CWAL_PROPREF_LIST:
              //dump_val(p->container, "p->container"); dump_val(p->key, "p->key");
              rc = cwal__propref_set(p, true, v, kvpFlags);
              break;
            default:
              cwal__fatal(CWAL_RC_ASSERT,
                          "Cannot happen: unhandled propref flags 0x%04x.",
                          p->flags);
              rc = CWAL_RC_ERROR /*not reached*/;
          }
          cwal__propref_end;
          return rc;
        }/*if resolvePropref*/
        break;
    }
    if(kvp->key != key){
      cwal_value * const old = kvp->key;
      cwal_value_xscope(sc->e, sc, key, 0);
      kvp->key = key;
      cwal_value_ref(key);
      cwal_value_unref(old);
    }
    if(kvp->value != v){
      cwal_value * const old = kvp->value;
      cwal_value_xscope(sc->e, sc, v, 0);
      kvp->value = v;
      cwal_value_ref(v);
      cwal_value_unref(old);
    }
    if(CWAL_VAR_F_PRESERVE!=kvpFlags) kvp->flags = kvpFlags;
  }else{/* Not found: add it. */
    if(CWAL_CONTAINER_DISALLOW_NEW_PROPERTIES & base->flags){
      return CWAL_RC_DISALLOW_NEW_PROPERTIES;
    }
    if(sc->e->values.hashXfer){
      kvp = sc->e->values.hashXfer;
      sc->e->values.hashXfer = 0;
    }else{
      kvp = cwal_kvp_alloc(sc->e);
      if(!kvp) return CWAL_RC_OOM;
    }
    assert(!kvp->key);
    assert(!kvp->value);
    assert(!kvp->right);
    if(left){
      kvp->right = left->right;
      left->right = kvp;
    }else{
      kvp->right = (cwal_kvp*)htable->list.list[ndx];
      htable->list.list[ndx] = kvp;
    }
    cwal_value_xscope(sc->e, sc, key, 0);
    cwal_value_xscope(sc->e, sc, v, 0);
    cwal_value_ref(key);
    cwal_value_ref(v);
    kvp->key = key;
    kvp->value = v;
    if(CWAL_VAR_F_PRESERVE!=kvpFlags) kvp->flags = kvpFlags;
    assert(CWAL_MEM_IS_BUILTIN(key) || key->scope);
    assert(CWAL_MEM_IS_BUILTIN(v) || v->scope);
#if 0 && CWAL_OBASE_ISA_HASH
    dump_val(kvp->key,"inserted hash key");
    dump_val(kvp->value,"inserted hash value");
    dump_val(container,"inserted into container");
    MARKER(("hash ndx=%d, hashSize=%d\n", (int)ndx, (int)htable->hashSize));
#endif
    ++htable->list.count;
  }
  return 0;
}

static int cwal_htable_remove_impl_v( cwal_value * const vSelf,
                                      cwal_htable * const htable,
                                      cwal_value * const key ){
  if(!htable || !key) return CWAL_RC_MISUSE;
  else if(!htable->hashSize || !htable->list.count) return CWAL_RC_NOT_FOUND;
  assert(!(CWAL_RCFLAG_HAS(vSelf,CWAL_RCF_IS_DESTRUCTING))
         && "Expecting this to be checked upstream.");
#if 0
  // To be checked by higher-level containers...
  if(CWAL_RCFLAG_HAS(vSelf,CWAL_RCF_IS_DESTRUCTING)) return CWAL_RC_DESTRUCTION_RUNNING;
  else if(CWAL_CONTAINER_DISALLOW_PROP_SET & CWAL_VOBASE(vSelf)->flags) return CWAL_RC_DISALLOW_PROP_SET;
  else if(CWAL_V_IS_VISITING_LIST(vSelf)) return CWAL_RC_IS_VISITING_LIST;
#endif
  cwal_midsize_t ndx = 0;
  cwal_kvp * left = 0;
  cwal_htable const * pht = htable;
  cwal_kvp * const kvp =
    cwal_htable_search_impl_v( NULL, &pht, false, key, &ndx, &left );
  assert(pht == htable);
  if(!kvp) return CWAL_RC_NOT_FOUND;
  else{
    //dump_val(kvp->key, "removing key");
    //dump_val(vSelf, "removing from container");
    assert(htable->list.count>0);
    if(left){
      left->right = kvp->right;
    }else{
      htable->list.list[ndx] = kvp->right;
    }
    kvp->right = NULL;
    --htable->list.count;
    cwal_kvp_free(vSelf->scope->e, kvp, 1);
    return 0;
  }
}

static int cwal_htable_remove_impl_cstr( cwal_value * const vSelf,
                                         cwal_htable * const htable,
                                         char const * const key,
                                         cwal_midsize_t keyLen ){
  if(!vSelf || !key) return CWAL_RC_MISUSE;
  else if(!htable->hashSize || !htable->list.count) return CWAL_RC_NOT_FOUND;
  assert(!(CWAL_RCFLAG_HAS(vSelf,CWAL_RCF_IS_DESTRUCTING))
         && "Expecting this to be checked upstream.");
#if 0
  // To be checked by higher-level containers...
  else if(CWAL_RCFLAG_HAS(vSelf,CWAL_RCF_IS_DESTRUCTING)) return CWAL_RC_DESTRUCTION_RUNNING;
  else if(CWAL_CONTAINER_DISALLOW_PROP_SET & CWAL_VOBASE(vSelf)->flags) return CWAL_RC_DISALLOW_PROP_SET;
  else if(CWAL_V_IS_VISITING_LIST(vSelf)) return CWAL_RC_IS_VISITING_LIST;
#endif
  cwal_midsize_t ndx = 0;
  cwal_kvp * left = 0;
  cwal_htable const * pht = htable;
  cwal_kvp * kvp =
    cwal_htable_search_impl_cstr( NULL, &pht, false, key, keyLen, &ndx, &left );
  assert(pht == htable);
  if(!kvp) return CWAL_RC_NOT_FOUND;
  else{
    assert(htable->list.count>0);
    if(left){
      left->right = kvp->right;
    }else{
      htable->list.list[ndx] = kvp->right;
    }
    kvp->right = NULL;
    --htable->list.count;
    cwal_kvp_free(vSelf->scope->e, kvp, 1);
    return 0;
  }
}

void cwal_list_to_recycler( cwal_engine * e, cwal_list * li ){
  if(li->list){
    cwal_memchunk_add(e, li->list, li->alloced * sizeof(void*));
    *li = cwal_list_empty;
  }else{
    assert(!li->count);
    assert(!li->alloced);
    li->isVisiting = false;
  }
}    

/**
   Cleans up various parts of an array:

   self must be a (ceal_value*).

   Cleans up all list entries Then...

   freeList: if true, frees all list memory

   freeProps: if true, clears all properties.

   isFinalCleanup: if true, clears the
   CWAL_CONTAINER_DISALLOW_PROTOTYPE_SET flag from the cwal_obase part
   and unsets/unrefs the prototype.
*/
static void cwal_value_cleanup_array_impl( cwal_engine * e, void * self,
                                           bool freeList, bool freeProps,
                                           bool isFinalCleanup ){
  cwal_value * const vSelf = (cwal_value *)self;
  cwal_array * const ar = cwal_value_get_array(vSelf);
  int opaque = 0;
  assert(NULL!=ar);
  cwal_visit_list_begin(vSelf, &opaque);
  if( ar->list.count ){
    cwal_value * v;
    cwal_size_t i = 0, x = ar->list.count -1;
    for( ; i < ar->list.count; ++i, --x ){
      v = (cwal_value*)ar->list.list[x];
      if(v){
        ar->list.list[x] = NULL;
        if(!CWAL_V_IS_IN_CLEANUP(v)){
          cwal_value_unref(v);
        }
      }
    }
    ar->list.count = 0;
  }
  cwal_visit_list_end(vSelf, opaque);
  if((isFinalCleanup || freeList) && ar->list.list){
    cwal_list_to_recycler(e, &ar->list);
  }
  if(isFinalCleanup || freeProps) {
    cwal_cleanup_obase( e, &ar->base, isFinalCleanup );
  }
  if(isFinalCleanup){
    ar->base.flags &= ~CWAL_CONTAINER_DISALLOW_PROTOTYPE_SET;
    if(ar->base.prototype){
      assert(vSelf->scope && "We can't have a prototype and yet have no scope.");
      /* dump_val(vSelf,"Unref'ing this one's prototype"); */
      /* dump_val(ar->base.prototype,"Unref'ing prototype"); */
      cwal_value_prototype_set( vSelf, NULL );
    }
  }
}


/**
   cwal_value_vtab::cleanup() impl for Array values. Cleans up
   self-owned memory, but does not free self.
*/
void cwal_value_cleanup_array( cwal_engine * e, void * self ){
  cwal_value * const vSelf = (cwal_value *)self;
  cwal_array * const ar = cwal_value_get_array(vSelf);
  cwal_value_cleanup_array_impl( e, self, true, true, true );
  *ar = cwal_array_empty;
}

void cwal_array_clear( cwal_array * ar, bool freeList, bool freeProps ){
  cwal_scope * s = ar ? CWAL_VALPART(ar)->scope : 0;
  cwal_engine * e = s ? s->e : 0;
  if(e){
    cwal_value_cleanup_array_impl( e, CWAL_VALPART(ar),
                                   freeList, freeProps, false );
  }
}
    
void cwal_value_cleanup_object( cwal_engine * e, void * self ){
  cwal_value * const vSelf = (cwal_value *)self;
  cwal_object * const obj = cwal_value_get_object(vSelf);
  assert(vSelf && obj);
  cwal_cleanup_obase( e, &obj->base, true );
  *obj = cwal_object_empty;
}

void cwal_value_cleanup_function( cwal_engine * e, void * self ){
  cwal_value * const v = (cwal_value*)self;
  cwal_function * const f = CWAL_VVPCAST(cwal_function,v);
  assert(v && f);
  if(f->state.finalize){
    f->state.finalize( e, f->state.data );
  }
  cwal_cleanup_obase( e, &f->base, true );
  *f = cwal_function_empty;
}

int cwal_value_fetch_function( cwal_value const * val, cwal_function ** x){
  if( ! val ) return CWAL_RC_MISUSE;
  else if( CWAL_TYPE_FUNCTION != val->vtab->typeID ) return CWAL_RC_TYPE;
  else{
    if(x) *x = CWAL_VVPCAST(cwal_function,val);
    return 0;
  }
}
    

cwal_value * cwal_new_function_value(cwal_engine * e,
                                     cwal_callback_f callback,
                                     void * state,
                                     cwal_finalizer_f stateDtor,
                                     void const * stateTypeID ){
  if(!e || !callback) return NULL;
  else{
    cwal_value * v = cwal_value_new(e, CWAL_TYPE_FUNCTION, 0);
    if( NULL != v ) {
      cwal_function * f = CWAL_VVPCAST(cwal_function,v);
      cwal_value * proto = f->base.prototype;
      *f = cwal_function_empty;
      f->base.prototype = proto;
      f->state.data = state;
      f->state.finalize = stateDtor;
      f->state.typeID = stateTypeID;
      f->callback = callback;
    }
    return v;
  }
}

cwal_function * cwal_new_function(cwal_engine * e,
                                  cwal_callback_f callback,
                                  void * state,
                                  cwal_finalizer_f stateDtor,
                                  void const * stateTypeID ){
  cwal_value * v = cwal_new_function_value(e, callback, state,
                                           stateDtor, stateTypeID);
  return v ? cwal_value_get_function(v) : NULL;
}

int cwal_function_unref(cwal_function *fv){
  cwal_value * v = CWAL_VALPART(fv);
  if(!v){
    assert(!fv);
    return CWAL_RC_MISUSE;
  }
  assert(v->scope);
  assert(v->scope->e);
  return cwal_value_unref2( v->scope->e, v );
}

cwal_engine * cwal_scope_engine(cwal_scope const * s){
  return s ? s->e : NULL;
}


int cwal_function_call_array( cwal_scope * s,
                              cwal_function * f,
                              cwal_value * self,
                              cwal_value ** rv,
                              cwal_array * args){
  cwal_value ** argv = args ?
    (args->list.count ? (cwal_value **)args->list.list : 0)
    : 0;
  int const argc = argv ? (int)args->list.count : 0;
  int rc;
  cwal_value * vargs = argc ? CWAL_VALPART(args) : 0;
  char const aWasVacSafe = vargs
    ? CWAL_V_IS_VACUUM_SAFE(vargs)
    : 1;
  if(argc && !aWasVacSafe){
    cwal_value_make_vacuum_proof(vargs,1);
  }
  cwal_value_ref(vargs);
  rc = s
    ? cwal_function_call_in_scope( s, f, self, rv, argc, argv )
    : cwal_function_call( f, self, rv, argc, argv );
  cwal_value_unhand(vargs);
  if(!aWasVacSafe){
    cwal_value_make_vacuum_proof(vargs,0);
  }
  return rc;
}

int cwal_function_call_in_scope2( cwal_scope * s,
                                  cwal_function * const f,
                                  cwal_value * const propertyHolder,
                                  cwal_value * const self,
                                  cwal_value ** _rv,
                                  uint16_t argc,
                                  cwal_value * const * argv){
  if(!s ||!s->e || !f) return CWAL_RC_MISUSE;
  cwal_engine * const e = s->e /* typing saver */;
  cwal_scope * old = e->current /* previous scope */;
  cwal_scope * check = 0 /* sanity check */;
  int rc = 0;
  cwal_callback_args args = cwal_callback_args_empty /* callback state */;
  cwal_value * rv = NULL /* result value for f() */;
  cwal_value * const fv = CWAL_VALPART(f);
  cwal_callback_hook const hook = e->cbHook /* why (again) do we take/need a copy? */;
  char const fWasVacuumProof = CWAL_V_IS_VACUUM_SAFE(fv);
  cwal_obase * const selfBase = CWAL_VOBASE(self);
  bool const selfWasVacSafe = selfBase
    ? CWAL_V_IS_VACUUM_SAFE(self)
    : 0;
  /* uint32_t const oldScopeFlags = s->flags; */
  assert(fv->scope);
  args.engine = e;
  args.scope = s;
  args.self = self;
  args.callee = f;
  args.state = f->state.data;
  args.stateTypeID = f->state.typeID;
  args.argc = argc;
  args.argv = argv;
  args.propertyHolder = propertyHolder;
  /* s->flags |= CWAL_F_IS_CALL_SCOPE; */

  /* We can't just fiddle with the refcount here:
     we have to make sure fv is removed from
     fv->scope->mine.r0 so it's sweep-safe. */
  rc = cwal_refcount_incr(e, fv);
  if(rc) return rc /* game over, man */;

  /*
    We set the vacuum-proofing flag and an artificial
    reference on f to proactively cover this hypothetical
    case:

    function(){...}()

    where the anonymous function triggers a recursive sweep or
    vacuum.

    Reminder to self:

    (proc(){...})()

    in th1ish, that can theoretically kill that proc before the
    call op can be applied if auto-sweeping is running at an
    interval of 1! In s2, the eval engine disables
    sweeping/vacuuming during any given single expression, so
    it's not a problem there unless/until we add recursive
    sweeping/vacuuming.
  */
  if(!fWasVacuumProof){
    cwal_value_make_vacuum_proof(fv, 1);
    /* why does this trigger in th1ish? */
    assert(fv == (CWAL_REFCOUNT(fv) ? fv->scope->mine.headSafe : fv->scope->mine.r0));
    /*
      Reminder: in th1ish we have refcount-0 funcs being
      called. That's potentially unsafe (we've learned in the
      meantime).
    */
  }
  if(self){
    cwal_value_ref(self);
    if(selfBase && !selfWasVacSafe){
      cwal_value_make_vacuum_proof(self,1);
    }
  }
  cwal_value_ref(propertyHolder);
  if(hook.pre){
    rc = hook.pre(&args, hook.state);
  }
  if(!rc){
    uint16_t i;
    for(i = 0; i < argc; ++i ) cwal_value_ref(argv[i]);
    rc = f->callback( &args, &rv );
    if(hook.post){
      /* ALWAYS call post() if pre() succeeds. */
      int const rc2 = hook.post(&args, hook.state,
                                rc, rc ? NULL : rv);
      if(rc2 && !rc) rc = rc2;
    }
    for(i = 0; i < argc; ++i ) cwal_value_unhand(argv[i]);
  }
  /* assert(CWAL_REFCOUNT(fv)>0 && "Someone took my ref!"); */
  cwal_value_unhand(propertyHolder);
  cwal_value_unhand(fv);
  if(!fWasVacuumProof){
    cwal_value_make_vacuum_proof(fv, 0);
    /*20181122: removed because this is triggering and i'm not 100% sure
      whether this assertion is truly valid (i've been away from the
      intricaces of this level too long :/)

      assert(fv == (CWAL_REFCOUNT(fv)
      ? fv->scope->mine.headSafe : fv->scope->mine.r0));

      For the time being we'll replace it with something less
      intrusive...
    */
    assert(fv->scope);
    assert(!CWAL_RCFLAG_HAS(fv,CWAL_RCF_IS_GC_QUEUED));
  }
  if(self){
    cwal_value_unhand(self);
    if(selfBase && !selfWasVacSafe){
      cwal_value_make_vacuum_proof(self,0);
    }
  }
  /* s->flags = oldScopeFlags; */
  check = e->current;
  if(old != check){
    /* i've never seen this happen - it's intended as a
       sanity check for higher-level implementors. */
    assert(!"The callback implementor or the engine "
           "around it violated scope creation rules.");
    MARKER(("WARNING: callback created scopes without popping them "
            "(or popped too many!)."
            " Cleaning them up now!\n"));
    while(e->current && (e->current!=old)){
      cwal_scope_pop(e);
    }
    if(e->current!=old){
      assert(!"Serious scope mis-management during callback.");
      rc = CWAL_RC_FATAL;
      old = NULL /* assume it was lost along the way */;
    }
  }
  e->current = old;
  assert(e->current);
  if(rc){
    if(rv) cwal_refunref(rv);
  }else{
#if 0
    /* Historical: disabled 20141124. The docs do not imply
       that we do this, and no C code (aside from a stray
       assertion) assumes it, either. And we need to leave
       rv==0 in order to distinguish between "no return" and
       "return undefined" (if we ever really want/need to).
       FWIW, my callbacks almost always explicitly set it to
       undefined, but that's more of a style thing than a
       requirement. */
    if(!rv) rv = cwal_value_undefined();
#endif
    if(_rv) *_rv = rv;
    else if(rv) cwal_refunref(rv);
  }
  return rc;
}

int cwal_function_call_in_scope( cwal_scope * const s,
                                 cwal_function * const f,
                                 cwal_value * const self,
                                 cwal_value ** rv,
                                 uint16_t argc,
                                 cwal_value * const * argv){
  return cwal_function_call_in_scope2( s, f, 0, self, rv, argc, argv );
}

int cwal_function_forward( cwal_function * const f,
                           uint16_t trimArgCount,
                           cwal_callback_args const * args,
                           cwal_value ** rv ){
  cwal_callback_args ax = *args;
  if(ax.argc<=trimArgCount){
    ax.argc = 0;
    ax.argv = args->argv;
  }else{
    ax.argc -= trimArgCount;
    ax.argv += trimArgCount;
  }
  ax.callee = f;
  return f->callback(&ax, rv);
}

bool cwal_function_is( cwal_function * const f,
                       cwal_callback_f cb ){
  return f->callback == cb;
}



void * cwal_args_state( cwal_callback_args const * args,
                        void const * stateTypeID ){
  return (args && (args->stateTypeID==stateTypeID || !args->stateTypeID))
    ? args->state
    : NULL;
}

void * cwal_function_state_get( cwal_function * f,
                                void const * stateTypeID ){
  return (f && (f->state.typeID==stateTypeID || !stateTypeID))
    ? f->state.data
    : NULL;

}

int cwal_function_set_rescoper( cwal_function * f,
                                cwal_value_rescoper_f rescoper){
  if(!f) return CWAL_RC_MISUSE;
  else {
    f->rescoper = rescoper;
    return 0;
  }
}

int cwal_function_call_in_scopef( cwal_scope * s,
                                  cwal_function * f,
                                  cwal_value * self,
                                  cwal_value ** rv, ... ){
  if(!s || !s->e || !f) return CWAL_RC_MISUSE;
  else {
    int rc = CWAL_RC_OK;
    cwal_value * argv[CWAL_OPT_MAX_FUNC_CALL_ARGS+1] = {0,};
    cwal_value * v;
    uint16_t argc = 0;
    va_list args;
    memset( argv, 0, sizeof(argv) );
    va_start(args, rv);
    while( (v=va_arg(args,cwal_value*)) ){
      if(argc>CWAL_OPT_MAX_FUNC_CALL_ARGS){
        rc = CWAL_RC_RANGE;
        break;
      }
      else argv[argc++] = v;
    }
    va_end(args);
    if(CWAL_RC_OK==rc){
      argv[argc] = 0;
      rc = cwal_function_call_in_scope( s, f, self, rv, argc, argv );
    }
    return rc;
  }
}

int cwal_function_call2( cwal_function * f,
                         cwal_value * propertyHolder,
                         cwal_value * self,
                         cwal_value ** rv,
                         uint16_t argc,
                         cwal_value * const * argv ){
  cwal_value * const fv = f ? cwal_function_value(f) : NULL;
  cwal_engine * const e = (fv && fv->scope) ? fv->scope->e : NULL;
  if(!e) return CWAL_RC_MISUSE;
  else{
    int rc, rc2 = 0;
    cwal_scope _sub = cwal_scope_empty;
    cwal_scope * s = &_sub;
    rc = cwal_scope_push(e, &s);
    if(!rc){
      rc = cwal_function_call_in_scope2( s, f, propertyHolder,
                                         self, rv, argc, argv );
      rc2 = cwal_scope_pop2(e, rc ? 0 : (rv ? *rv : 0));
    }
    return rc2 ? rc2 : rc;
  }
}

int cwal_function_call( cwal_function * f,
                        cwal_value * self,
                        cwal_value ** rv,
                        uint16_t argc,
                        cwal_value * const * argv ){
  return cwal_function_call2( f, NULL, self, rv, argc, argv );
}

int cwal_function_callv( cwal_function * f, cwal_value * self,
                         cwal_value ** rv, va_list args ){
  cwal_value * fv = f ? cwal_function_value(f) : NULL;
  cwal_engine * e = (fv && fv->scope) ? fv->scope->e : NULL;
  if(!e) return CWAL_RC_MISUSE;
  else {
    cwal_value * argv[CWAL_OPT_MAX_FUNC_CALL_ARGS+1] = {0,};
    cwal_value * v;
    uint16_t argc = 0;
    int rc = 0;
    memset( argv, 0, sizeof(argv) );
    while( (v=va_arg(args,cwal_value*)) ){
      if(argc>CWAL_OPT_MAX_FUNC_CALL_ARGS){
        rc = CWAL_RC_RANGE;
        break;
      }
      else argv[argc++] = v;
    }
    if(rc) return rc;
    else{
      argv[argc] = 0;
      return cwal_function_call( f, self, rv, argc, argv );
    }
  }
}

    
int cwal_function_callf( cwal_function * f,
                         cwal_value * self,
                         cwal_value ** rv,
                         ... ){
  int rc = 0;
  va_list args;
  va_start(args, rv);
  rc = cwal_function_callv( f, self, rv, args );
  va_end(args);
  return rc;
}

cwal_function * cwal_value_get_function( cwal_value const * v ) {
  cwal_function * ar = NULL;
  cwal_value_fetch_function( v, &ar );
  return ar;
}

cwal_value * cwal_function_value(cwal_function const * s){
  return CWAL_VALPART(s);
}

cwal_value * cwal_new_buffer_value(cwal_engine *e, cwal_size_t startingSize){
  cwal_value * v = cwal_value_new(e, CWAL_TYPE_BUFFER,0);
  if( NULL != v )
  {
    cwal_buffer_obj * bo = CWAL_BUFOBJ(v);
    cwal_buffer * b;
    assert(NULL != bo);
    b = &bo->buf;
    b->self = bo;
    cwal_buffer_wipe_keep_self(b);
    assert(bo == b->self);
    if(startingSize &&
       cwal_buffer_reserve(e, b, startingSize)){
      cwal_value_unref2(e, v);
      v = NULL;
    }
  }
  return v;
}

int cwal_buffer_unref(cwal_engine *e, cwal_buffer *v){
  return (e&&v)
    ? cwal_value_unref2( e, cwal_buffer_value(v) )
    : CWAL_RC_MISUSE;
}

int cwal_value_fetch_buffer( cwal_value const * val, cwal_buffer ** x){
  cwal_buffer_obj * bo;
  if( ! val ) return CWAL_RC_MISUSE;
  else if( !(bo = CWAL_BUFOBJ(val)) ) return CWAL_RC_TYPE;
  else{
    if(x) *x = &bo->buf;
    return 0;
  }
}
    
cwal_buffer * cwal_value_get_buffer( cwal_value const * v ) {
  cwal_buffer * b = NULL;
  cwal_value_fetch_buffer( v, &b );
  return b;
}

cwal_buffer * cwal_new_buffer(cwal_engine *e, cwal_size_t startingSize){
  return cwal_value_get_buffer(cwal_new_buffer_value(e, startingSize));
}

char const * cwal_buffer_cstr(cwal_buffer const * const b, cwal_size_t * n){
  if(b->mem) if(n) *n = b->used;
  return (char const *)b->mem;
}

cwal_value * cwal_buffer_value(cwal_buffer const * s){
  if(!s || !s->self) return 0;
  else{
    cwal_buffer_obj const * bo = (cwal_buffer_obj const *)s->self;
    cwal_value * v = CWAL_VALPART(bo);
    if(s->self != CWAL_BUFOBJ(v)){
      assert(!"It seems that that the 'self' member of a buffer got screwed up.");
      return 0;
    }
    if(v && v->vtab && (CWAL_TYPE_BUFFER==v->vtab->typeID)){
      return v;
    }else{
      assert(!"It seems that we were passed a non-Value cwal_buffer.");
      return NULL;
    }
  }
}

cwal_string * cwal_buffer_to_zstring(cwal_engine * e, cwal_buffer * b){
  if(!e || !e->current || !b) return 0;
  else if((b->used+1) & ~((cwal_size_t)CWAL_STRLEN_MASK)) return 0 /* too big */;
  else{
    cwal_string * s = cwal_new_zstring(e, (char *)b->mem, b->used)
      /* reminder: that might cwal_free(e, b->mem) */;
    if(!s) return NULL;
    else if(s && !CWAL_MEM_IS_BUILTIN(s)){
      /* Re-tweak the metrics which the z-string ctor just
         counted. Those bytes were already counted by wherever
         buf->mem came from (it might have initially been from,
         e.g. cwal_list::list).

         Because b->mem's source is unclear, we cannot subtract
         the metric from its origin entry in e->metrics. The
         best we can do here is _subtract_ the b->used+1 which
         the z-string ctor just added to its metrics, to avoid
         double-counting.

         Not perfect, but there it is.
      */
      assert(e->metrics.bytes[CWAL_TYPE_ZSTRING] >= b->used+1);
      e->metrics.bytes[CWAL_TYPE_ZSTRING] -= b->used+1;
    }
    cwal_buffer_wipe_keep_self(b);
    return s;
  }
}

cwal_value * cwal_buffer_to_zstring_value(cwal_engine * e, cwal_buffer * b){
  return cwal_string_value(cwal_buffer_to_zstring(e,b));
}
    
/**
   cwal_value_vtab::destroy_value() impl for Buffer
   values. Cleans up self-owned memory, but does not
   free self.
*/
void cwal_value_cleanup_buffer( cwal_engine * e, void * self ){
  cwal_value * const v = (cwal_value*)self;
  cwal_buffer_obj * const bo = CWAL_BUFOBJ(v);
  cwal_buffer_reserve(e, &bo->buf, 0);
  cwal_cleanup_obase(e, &bo->base, true);
  *bo = cwal_buffer_obj_empty;
}

void cwal_value_cleanup_exception( cwal_engine * e, void * self ){
  cwal_value * const v = (cwal_value*)self;
  cwal_exception * const f = CWAL_VVPCAST(cwal_exception,v);
  cwal_cleanup_obase(e, &f->base, true);
  *f = cwal_exception_empty;
}

cwal_value * cwal_new_exception_value( cwal_engine * e, int code, cwal_value * msg ){
  cwal_value * v = e
    ? cwal_value_new(e, CWAL_TYPE_EXCEPTION, 0 )
    : NULL;
  if(v){
    cwal_value * proto;
    cwal_exception * r;
    static cwal_size_t codeKeyLen = 0;
    static cwal_size_t msgKeyLen = 0;
    int rc;
    if(!codeKeyLen){
      msgKeyLen = cwal_strlen(CwalConsts.ExceptionMessageKey);
      codeKeyLen = cwal_strlen(CwalConsts.ExceptionCodeKey);
    }
    /*
      Reminder:

      i would prefer to have a cwal_exception::message member, but
      lifetime of it gets problematic. One solution would be to
      move the xscope() operation into cwal_value_vtab, so that we
      can generically xscope Exception values without having to
      know that they have a free-floating member (not in a
      cwal_obase::kvp list).

      (That feature has since been added, by the way.)
    */
    r = cwal_value_get_exception(v);
    assert(r);
    proto = r->base.prototype;
    *r = cwal_exception_empty;
    r->base.prototype = proto;
    r->code = code;
    rc = cwal_prop_set(v, CwalConsts.ExceptionCodeKey,
                       codeKeyLen,
                       cwal_new_integer(e, (cwal_int_t)code));
    if(!rc && msg) rc = cwal_prop_set(v,
                                      CwalConsts.ExceptionMessageKey,
                                      msgKeyLen,
                                      msg);
    if(0!=rc){
      cwal_value_unref2(e, v);
      v = 0;
    }
  }
  return v;
}

cwal_exception * cwal_new_exceptionfv(cwal_engine * e, int code, char const * fmt, va_list args ){
  if(!e) return 0;
  else if(!fmt || !*fmt) return cwal_new_exception(e,code, NULL);
  else{
    cwal_string * s = cwal_new_stringfv( e, fmt, args);
    cwal_exception * x;
    if(!s) return NULL;
    x = cwal_new_exception(e, code, cwal_string_value(s));
    if(!x) cwal_string_unref(s);
    return x;
  }
}

cwal_exception * cwal_new_exceptionf(cwal_engine * e, int code, char const * fmt, ...){
  if(!e) return 0;
  else if(!fmt || !*fmt) return cwal_new_exception(e,code, NULL);
  else{
    cwal_exception * x;
    va_list args;
    va_start(args,fmt);
    x = cwal_new_exceptionfv(e, code, fmt, args);
    va_end(args);
    return x;
  }
}


    
int cwal_exception_unref(cwal_engine *e, cwal_exception *v){
  return (e&&v)
    ? cwal_value_unref2( e, cwal_exception_value(v) )
    : CWAL_RC_MISUSE;
}

    
int cwal_value_fetch_exception( cwal_value const * val, cwal_exception ** x){
  if( ! val ) return CWAL_RC_MISUSE;
  else if( !cwal_value_is_exception(val) ) return CWAL_RC_TYPE;
  else{
    if(x) *x = CWAL_VVPCAST(cwal_exception,val);
    return 0;
  }
}

cwal_exception * cwal_value_get_exception( cwal_value const * v ){
  cwal_exception * r = 0;
  cwal_value_fetch_exception( v, &r );
  return r;
}
    
cwal_exception * cwal_new_exception( cwal_engine * e, int code, cwal_value * msg ){
  cwal_value * v = cwal_new_exception_value(e, code, msg);
  return v ? cwal_value_get_exception(v) : NULL;
}
    
cwal_value * cwal_exception_value(cwal_exception const * s){
  return s
    ? CWAL_VALPART(s)
    : NULL;
}

int cwal_exception_code_get( cwal_exception const * r ){
  return r ? r->code : cwal_exception_empty.code;
}
int cwal_exception_code_set( cwal_exception * r, int code ){
  return r
    ? (r->code=code, 0)
    : CWAL_RC_MISUSE;
}

cwal_value * cwal_exception_message_get( cwal_exception const * r ){
  static cwal_midsize_t len = 0;
  if(0==len){
    len = cwal_strlen(CwalConsts.ExceptionMessageKey);
  }
  cwal_kvp * const kvp =
    cwal__prop_get_kvp( true,
                        CWAL_VALPART(r),
                        CwalConsts.ExceptionMessageKey, len,
                        false, NULL);
  return kvp ? kvp->value : NULL;
}

int cwal_exception_message_set( cwal_engine * e, cwal_exception * r, cwal_value * msg ){
  if(!e || !r) return CWAL_RC_MISUSE;
  else return cwal_prop_set( cwal_exception_value(r),
                             CwalConsts.ExceptionMessageKey,
                             cwal_strlen(CwalConsts.ExceptionMessageKey),
                             msg );
}
    
    
char * cwal_string_str_rw(cwal_string *v){
  /*
    See http://groups.google.com/group/comp.lang.c.moderated/browse_thread/thread/2e0c0df5e8a0cd6a
  */
  assert(v &&
         !CWAL_STR_ISX(v)/* not allowed for x-strings */);
  return CWAL_STR_ISZ(v)
    ? (char *)*((unsigned char **)(v+1))
    : (CWAL_STRLEN(v)
       ? (char *)((unsigned char *)( v+1 ))
       : NULL
       );
}

/**
   Intended to be called immediately after initialization of s and
   assignment of its string content, and it assert()'s that the
   is-ascii flag is no set on s. If the byte length of s equals its
   UTF8 length, the is-ascii flag is encoded in s->length, else this
   has no side effects.
*/
static void cwal_string_check_for_ascii( cwal_string * s ){
  unsigned char const * c = (unsigned char const *) cwal_string_cstr(s);
  unsigned char const * end = c + CWAL_STRLEN(s);
  assert(!CWAL_STR_ISASCII(s));
  assert(c);
  assert(c < end);
  for( ; c < end; ++c ){
    if(*c & 0x80) return;
  }
  s->length |= CWAL_STR_ASCII_MASK;
}

char const * cwal_string_cstr(cwal_string const *v){
#if 1
  return (NULL == v)
    ? NULL
    : (CWAL_STR_ISXZ(v)
       ? (char const *) *((unsigned char const **)(v+1))
       : (char const *) ((unsigned char const *)(v+1)))
    ;
#else
  /*
    See https://groups.google.com/group/comp.lang.c.moderated/browse_thread/thread/2e0c0df5e8a0cd6a
  */
  return (NULL == v)
    ? NULL
    : (CWAL_STRLEN(v)
       ? (CWAL_STR_ISXZ(v)
          ? (char const *) *((unsigned char const **)(v+1))
          : (char const *) ((unsigned char const *)(v+1)))
       : "");
#endif
}


char const * cwal_string_cstr2(cwal_string const *v, cwal_midsize_t * len){
  if(v && len) *len = CWAL_STRLEN(v);
  return cwal_string_cstr(v);
}

void cwal_value_cleanup_string( cwal_engine * e, void * V ){
  cwal_value * v = (cwal_value*)V;
  cwal_string * s = cwal_value_get_string(v);
  assert(s);
  assert(CWAL_STRLEN(s) && "Empty string cannot be cleaned up - it doesn't refcount.");
  if(CWAL_MEM_IS_BUILTIN(v)) return;
  else if(CWAL_STR_ISZ(s)){
    unsigned char ** pos = (unsigned char **)(s+1);
    char * cs = cwal_string_str_rw(s);
    cwal_size_t const slen = CWAL_STRLEN(s);
    assert(cs == (char *)*pos);
    *pos = NULL;
    if(e->flags & CWAL_FEATURE_ZERO_STRINGS_AT_CLEANUP){
      memset(cs, 0, slen+1/*NUL*/);
    }
    cwal_memchunk_add(e, cs, slen+1/*NUL*/);
    /* cwal_free(e, cs); */
  }else if(CWAL_STR_ISX(s)){
    unsigned char const ** pos = (unsigned char const **)(s+1);
#if !defined(NDEBUG)
    char const * cs = cwal_string_cstr(s);
    assert(cs == (char *)*pos);
#endif
    *pos = NULL;
    /* Nothing to free - the bytes are external */
  }else{/* Is a normal string, not an X/Y-string */
    cwal_interned_remove( e, v, 0 );
    if(e->flags & CWAL_FEATURE_ZERO_STRINGS_AT_CLEANUP){
      char * cs = cwal_string_str_rw(s);
      memset(cs, 0, CWAL_STRLEN(s));
    }
  }
}

int cwal_string_unref(cwal_string * s){
  cwal_value * v = cwal_string_value(s);
  return v
    ? cwal_value_unref2( cwal_value_engine(v), v )
    : CWAL_RC_MISUSE;
}

cwal_midsize_t cwal_string_length_bytes( cwal_string const * str ){
  return str
    ? CWAL_STRLEN(str)
    : 0U;
}


cwal_midsize_t cwal_string_length_utf8( cwal_string const * str ){
  return str
    ? (CWAL_STR_ISASCII(str)
       ? CWAL_STRLEN(str)
       : cwal_strlen_utf8( cwal_string_cstr(str),
                           CWAL_STRLEN(str) )
       )
    : 0U;
}

bool cwal_string_is_ascii( cwal_string const * str ){
  return str ? CWAL_STR_ISASCII(str) : 0;
}

cwal_value * cwal_new_string_value(cwal_engine * e, char const * str, cwal_midsize_t len){
  return cwal_string_value( cwal_new_string(e, str, len) );
}

cwal_value * cwal_new_string_value2(cwal_engine * const e,
                                    char const * const str,
                                    cwal_int_t len){
  return cwal_string_value(cwal_new_string(e, str, len<0
                                           ? (cwal_midsize_t)cwal_strlen(str)
                                           : (cwal_midsize_t)len) );
}

bool cwal_cstr_internable_predicate_f_default( void * state, char const * str, cwal_size_t len ){
  if(state || str){/*avoid unused param warning*/}
  return !CwalConsts.MaxInternedStringSize
    || (len <= CwalConsts.MaxInternedStringSize);
}

#if CWAL_ENABLE_BUILTIN_LEN1_ASCII_STRINGS
/**
   Expects (asserts) char to be in the range [0,127]. Gets the shared
   length-1 string for that character and returns it. If it fails, ndx
   is of an unexpected value.

   Intended ONLY to be called from cwal_new_string/xstring/zstring()
   and ONLY after they have verified that ndx is in range.
*/
static cwal_string * cwal_len1_ascii_string(int ndx){
  cwal_value * v;
  assert(ndx>=0 && ndx<=127);
  v = (cwal_value *)CWAL_BUILTIN_VALS.memAsciiPrintable[ndx];
  assert(CWAL_STR(v));
  return CWAL_STR(v);
}
#endif

cwal_string * cwal_new_string(cwal_engine * e, char const * str, cwal_midsize_t len){
  if(!e || CWAL_STRLEN_TOO_LONG(len)){
    return NULL ;
  }
  else if( !str || !len ){
    METRICS_REQ_INCR(e,CWAL_TYPE_STRING);
    return CWAL_BUILTIN_VALS.sEmptyString;
  }
#if CWAL_ENABLE_BUILTIN_LEN1_ASCII_STRINGS
  else if( 1==len
           && ((unsigned char)*str)<=127 ){
    METRICS_REQ_INCR(e,CWAL_TYPE_STRING);
    ++e->metrics.len1StringsSaved[0];
    return cwal_len1_ascii_string((signed char)*str);
  }
#endif
  else{
    cwal_value * c = 0;
    cwal_string * s = 0;
    assert(len);
    if(CWAL_FEATURE_INTERN_STRINGS & e->flags){
      cwal_interned_search( e, str, len, &c, 0, 0 );
    }
    if(c/* Got an interned string... */){ 
      s = cwal_value_get_string(c);
      assert(0 != s);
      METRICS_REQ_INCR(e,CWAL_TYPE_STRING);
      CWAL_TR_V(e,c);
      CWAL_TR3(e,CWAL_TRACE_VALUE_INTERNED,
               "re-using interned string");
      assert(c->scope->level <= e->current->level);
    }
    else{ /* Create new string... */
      c = cwal_value_new(e, CWAL_TYPE_STRING, len);
      if( c ){
        char * dest = NULL;
        s = CWAL_STR(c);
        assert( s );
        *s = cwal_string_empty;
        s->length = ((cwal_size_t)CWAL_STRLEN_MASK) & len;
        assert(CWAL_STRLEN(s) == len);
        dest = cwal_string_str_rw(s)
          /* maintenance note: this is the only place in the
             library where _writing_ to a normal
             (non-X/Z-string) cwal_string is allowed.
          */;
        assert( (NULL != dest)
                && "Empty string should have been caught earlier!");
        {
          unsigned char isAscii = 0;
          unsigned char const * usrc = (unsigned char const *)str;
          unsigned char * udest = (unsigned char *)dest;
          unsigned char const * end = udest + len;
          for( ; udest < end; ++udest, ++usrc ){
            isAscii |= (*udest = *usrc);
          }
          *udest = 0;
          if(!(isAscii & 0x80)){
            s->length |= CWAL_STR_ASCII_MASK;
          }    
        }
        if((CWAL_FEATURE_INTERN_STRINGS & e->flags)
           && (e->vtab->interning.is_internable
               && e->vtab->interning.is_internable( e->vtab->interning.state, str, len )
               )
           ){
          cwal_interned_insert( e, c )
            /* This insertion effectively controls whether
               or not interning of strings is on. If it
               fails, the string is effectively not
               interned, but otherwise no harm is
               done. Allocation of a new interning table
               could fail, but that's about the only
               conceivable error condition here (and we
               can ignore it by not interning).
            */;
          /* MARKER(("INTERNING rc=%d: %.*s\n", rc, (int)len, str)); */
        }
      }
    }
    return s;
  }
}

cwal_string * cwal_new_xstring(cwal_engine * e, char const * str,
                               cwal_midsize_t len){
  if(!e || (len & ~((cwal_size_t)CWAL_STRLEN_MASK) /* too big */)){
    return NULL;
  }else if( !len ){
    METRICS_REQ_INCR(e,CWAL_TYPE_XSTRING);
    return CWAL_BUILTIN_VALS.sEmptyString;
  }
#if CWAL_ENABLE_BUILTIN_LEN1_ASCII_STRINGS
  else if( 1==len
           && ((unsigned char)*str)<=127 ){
    METRICS_REQ_INCR(e,CWAL_TYPE_XSTRING);
    ++e->metrics.len1StringsSaved[1];
    return cwal_len1_ascii_string((signed char)*str);
  }
#endif
  else{
    cwal_value * c = NULL;
    cwal_string * s = NULL;
    c = cwal_value_new(e, CWAL_TYPE_XSTRING, 0);
    if( c ){
      unsigned char  const ** dest;
      s = CWAL_STR(c);
      assert( NULL != s );
      *s = cwal_string_empty;
      s->length = CWAL_STR_XMASK | len;
      assert(s->length > len);
      assert(CWAL_STRLEN(s) == len);
      assert(CWAL_STR_ISX(s));
      assert(CWAL_STR_ISXZ(s));
      dest = (unsigned char const **)(s+1);
      *dest = (unsigned char const *)str;
      cwal_string_check_for_ascii( s );
    }
    return s;
  }
}

cwal_value * cwal_new_xstring_value(cwal_engine * e, char const * str,
                                    cwal_midsize_t len){
  cwal_string * s = cwal_new_xstring(e, str, len);
  return s ? cwal_string_value(s) : NULL;
}

cwal_string * cwal_new_zstring(cwal_engine * e, char * str, cwal_midsize_t len){
  if(!e || (len & ~((cwal_size_t)CWAL_STRLEN_MASK) /* too big */)){
    return NULL;
  }else if(!str){
    METRICS_REQ_INCR(e,CWAL_TYPE_ZSTRING);
    return CWAL_BUILTIN_VALS.sEmptyString;
  }
  else if(!len){
    /* Special case: free source memory immediately. */
    METRICS_REQ_INCR(e,CWAL_TYPE_ZSTRING);
    cwal_free(e, str);
    return CWAL_BUILTIN_VALS.sEmptyString;
  }
#if CWAL_ENABLE_BUILTIN_LEN1_ASCII_STRINGS
  else if( 1==len
           && ((unsigned char)*str)<=127 ){
    /* Special case: free source memory immediately. */
    cwal_string * rc = cwal_len1_ascii_string((signed char)*str);
    assert(rc && 1==CWAL_STRLEN(rc));
    METRICS_REQ_INCR(e,CWAL_TYPE_ZSTRING);
    cwal_free(e, str);
    ++e->metrics.len1StringsSaved[2];
    return rc;
  }
#endif
  else{
    cwal_value * c = NULL;
    cwal_string * s = NULL;
    assert(len>0);
    c = cwal_value_new(e, CWAL_TYPE_ZSTRING, 0);
    if( c ){
      unsigned char  ** dest;
      s = CWAL_STR(c);
      assert( NULL != s );
      *s = cwal_string_empty;
      s->length = CWAL_STR_ZMASK | len;
      e->metrics.bytes[CWAL_TYPE_ZSTRING] += len +1
        /* we're going to assume a NUL byte for
           metrics purposes, because there essentially
           always is one for z-strings. */;
      assert(s->length > len);
      assert(CWAL_STRLEN(s) == len);
      assert(CWAL_STR_ISZ(s));
      assert(CWAL_STR_ISXZ(s));
      dest = (unsigned char **)(s+1);
      *dest = (unsigned char *)str;
      cwal_string_check_for_ascii( s );
    }else{
      /* See the API docs for why we do this. */
      cwal_free2( e, str, len/*+1 would be safe, until it wasn't.*/ );
    }
    return s;
  }
}

cwal_value * cwal_new_zstring_value(cwal_engine * e, char * str, cwal_midsize_t len){
  cwal_string * s = cwal_new_zstring(e, str, len);
  return s ? cwal_string_value(s) : NULL;
}


void cwal_buffer_swap_mem( cwal_buffer * const left, cwal_buffer * const right ){
  void * const self1 = left->self, * const self2 = right->self
    /* cwal_value parts of script-side buffers */;
  cwal_buffer const tmp = *left;
  assert(left && right);
  assert(left != right);
  *left = *right;
  *right = tmp;
  left->self = self1;
  right->self = self2;
}

cwal_buffer * cwal_buffer_reuse( cwal_buffer * const b ){
  if(b->capacity){
    assert(b->mem);
    b->mem[0] = 0;
  }
  b->used = 0;
  return b;
}

int cwal_buffer_resize( cwal_engine * e, cwal_buffer * buf, cwal_size_t n ){
  if( !buf ) return CWAL_RC_MISUSE;
  else if(n && (buf->capacity == n+1)){
    buf->used = n;
    buf->mem[n] = 0;
    return 0;
  }else{
    unsigned char * x = (unsigned char *)cwal_realloc( e, buf->mem,
                                                       n+1/*NUL*/ );
    if( ! x ) return CWAL_RC_OOM;
    if(n > buf->capacity){
      /* zero-fill new parts */
      memset( x + buf->capacity, 0, n - buf->capacity +1/*NUL*/ );
    }
    /* reminder to self: e->metrics.bytes[CWAL_TYPE_BUFFER] might be
       0 here because buf->mem might have come from a recycler. That
       means we're not byte-counting buffer resize(), which is a bit
       disturbing. We could measure it if over-allocation is on, but
       we don't know which pool (if any) to modify the count in. e.g.
       buf->mem might have come from the recycler after having been
       allocated as a cwal_list::list. So... hmmm.
    */
    /* assert(e->metrics.bytes[CWAL_TYPE_BUFFER]) >= buf->capacity; */
    /* e->metrics.bytes[CWAL_TYPE_BUFFER] -= buf->capacity; */
    buf->capacity = n + 1 /*NUL*/;
    /* e->metrics.bytes[CWAL_TYPE_BUFFER] += buf->capacity; */
    buf->used = n;
    buf->mem = x;
    buf->mem[buf->used] = 0;
    return 0;
  }
}


cwal_string * cwal_new_stringfv(cwal_engine * e, char const * fmt, va_list args ){
  if(!e || !fmt) return 0;
  else if(!*fmt) return cwal_new_string(e,"",0);
  else{
    int rc;
    cwal_size_t const oldUsed = e->buffer.used;
    cwal_size_t slen;
    rc = cwal_buffer_printfv(e, &e->buffer, fmt, args);
    slen = e->buffer.used - oldUsed;
    e->buffer.used = oldUsed;
    return rc
      ? NULL
      : cwal_new_string(e, (char const*)(e->buffer.mem+oldUsed), slen);
    ;
  }
}

cwal_string * cwal_new_stringf(cwal_engine * e, char const * fmt, ...){
  if(!e || !fmt) return 0;
  else if(!*fmt) return cwal_new_string(e,NULL,0);
  else{
    cwal_string * str;
    va_list args;
    va_start(args,fmt);
    str = cwal_new_stringfv(e, fmt, args);
    va_end(args);
    return str;
  }
}


cwal_value * cwal_string_value(cwal_string const * s){
  return s
    ? (CWAL_STRLEN(s)
       ? CWAL_VALPART(s)
       : CWAL_BUILTIN_VALS.vEmptyString)
    : NULL;
}

cwal_engine * cwal_value_engine( cwal_value const * v ){
  return (v && v->scope)
    ? v->scope->e
    : 0;
}

cwal_scope * cwal_value_scope( cwal_value const * v ){
  return v ? v->scope : NULL;
}

cwal_value * cwal_string_concat( cwal_string const * s1, cwal_string const * s2 ){
  if(!s1 || !s2) return NULL;
  else {
    cwal_size_t newLen;
    int rc;
    cwal_engine * e = cwal_value_engine(cwal_string_value(s1));
    assert(e);
    newLen = CWAL_STRLEN(s1) + CWAL_STRLEN(s2) + 1/*NUL byte*/;
    if( CWAL_STRLEN_TOO_LONG(newLen) ) return NULL;
    rc = cwal_buffer_reserve( e, &e->buffer, newLen );
    if(rc) return NULL;
    e->buffer.used = 0;
    rc = cwal_buffer_append( e, &e->buffer, cwal_string_cstr(s1), CWAL_STRLEN(s1) );
    if(rc) return NULL;
    rc = cwal_buffer_append( e, &e->buffer, cwal_string_cstr(s2), CWAL_STRLEN(s2) );
    if(rc) return NULL;
    e->buffer.mem[e->buffer.used] = 0;
    return cwal_new_string_value( e, (char const *)e->buffer.mem, e->buffer.used );
  }
}
    

int cwal_value_fetch_bool( cwal_value const * val, char * v )
{
  /**
     FIXME: move the to-bool operation into cwal_value_vtab, like we
     do in the C++ API.
  */
  if( ! val || !val->vtab ) return CWAL_RC_MISUSE;
  else
  {
    int rc = 0;
    char b = NULL != CWAL_VOBASE(val);
    if(!b) switch( val->vtab->typeID ){
        case CWAL_TYPE_BUFFER:
          b = 1;
          break;
        case CWAL_TYPE_STRING: {
          char const * str = cwal_string_cstr(cwal_value_get_string(val));
          b = (str && *str) ? 1 : 0;
          break;
        }
        case CWAL_TYPE_UNDEF:
        case CWAL_TYPE_NULL:
          break;
        case CWAL_TYPE_BOOL:
          b = CWAL_BOOL(val);
          break;
        case CWAL_TYPE_INTEGER: {
          cwal_int_t i = 0;
          cwal_value_fetch_integer( val, &i );
          b = i ? 1 : 0;
          break;
        }
        case CWAL_TYPE_DOUBLE: {
          cwal_double_t d = 0.0;
          cwal_value_fetch_double( val, &d );
          b = (0.0==d) ? 0 : 1;
          break;
        }
        case CWAL_TYPE_UNIQUE:
          b = 1;
          break;
        case CWAL_TYPE_TUPLE:
          b = CWAL_TUPLE(val)->n ? 1 : 0;
          break;
        default:
          rc = CWAL_RC_TYPE;
          break;
      }
    if( !rc && v ) *v = b;
    return rc;
  }
}

bool cwal_value_get_bool( cwal_value const * val )
{
  char i = 0;
  cwal_value_fetch_bool( val, &i );
  return i;
}

int cwal_value_fetch_integer( cwal_value const * val, cwal_int_t * v )
{
  if( ! val || !val->vtab ) return CWAL_RC_MISUSE;
  else {
    cwal_int_t i = 0;
    int rc = 0;
    switch(val->vtab->typeID){
      case CWAL_TYPE_UNIQUE:
      case CWAL_TYPE_TUPLE:
      case CWAL_TYPE_UNDEF: 
      case CWAL_TYPE_NULL:
        i = 0;
        break;
      case CWAL_TYPE_BOOL:{
        char b = 0;
        cwal_value_fetch_bool( val, &b );
        i = b;
        break;
      }
      case CWAL_TYPE_INTEGER: {
        i = *CWAL_INT(val);
        break;
      }
      case CWAL_TYPE_DOUBLE:{
        cwal_double_t d = 0.0;
        cwal_value_fetch_double( val, &d );
        i = (cwal_int_t)d;
        break;
      }
      case CWAL_TYPE_STRING:
        rc = cwal_string_to_int( cwal_value_get_string(val),
                                 &i );
        break;
      default:
        rc = CWAL_RC_TYPE;
        break;
    }
    if(!rc && v) *v = i;
    return rc;
  }
}

cwal_int_t cwal_value_get_integer( cwal_value const * val )
{
  cwal_int_t i = 0;
  cwal_value_fetch_integer( val, &i );
  return i;
}

int cwal_value_fetch_double( cwal_value const * val, cwal_double_t * v )
{
  if( ! val || !val->vtab ) return CWAL_RC_MISUSE;
  else
  {
    cwal_double_t d = 0.0;
    int rc = 0;
    switch(val->vtab->typeID)
    {
      case CWAL_TYPE_UNIQUE:
      case CWAL_TYPE_TUPLE:
      case CWAL_TYPE_UNDEF:
      case CWAL_TYPE_NULL:
        d = 0;
        break;
      case CWAL_TYPE_BOOL: {
        char b = 0;
        cwal_value_fetch_bool( val, &b );
        d = b ? 1.0 : 0.0;
        break;
      }
      case CWAL_TYPE_INTEGER: {
        cwal_int_t i = 0;
        cwal_value_fetch_integer( val, &i );
        d = i;
        break;
      }
      case CWAL_TYPE_DOUBLE:
        memcpy(&d, CWAL_DBL_NONULL(val), sizeof(cwal_double_t));
        break;
      case CWAL_TYPE_STRING:
        rc = cwal_string_to_double( cwal_value_get_string(val),
                                    &d );
        break;
      default:
        rc = CWAL_RC_TYPE;
        break;
    }
    if(!rc && v) *v = d;
    return rc;
  }
}

cwal_double_t cwal_value_get_double( cwal_value const * val )
{
  cwal_double_t i = 0.0;
  cwal_value_fetch_double( val, &i );
  return i;
}

int cwal_value_fetch_string( cwal_value const * val, cwal_string ** dest )
{
  if( ! val || ! dest ) return CWAL_RC_MISUSE;
  else if( ! cwal_value_is_string(val) ) return CWAL_RC_TYPE;
  else
  {
    if( dest ) *dest = CWAL_STR(val);
    return CWAL_RC_OK;
  }
}

cwal_string * cwal_value_get_string( cwal_value const * val )
{
  cwal_string * rc = NULL;
  cwal_value_fetch_string( val, &rc );
  return rc;
}

char const * cwal_value_get_cstr( cwal_value const * val, cwal_size_t * len )
{
  switch(val ? val->vtab->typeID : 0){
    case CWAL_TYPE_STRING:{
      cwal_string const * s = cwal_value_get_string(val);
      if(len) *len = CWAL_STRLEN(s);
      return cwal_string_cstr(s);
    }
    case CWAL_TYPE_BUFFER:{
      cwal_buffer const * b = cwal_value_get_buffer(val);
      if(len) *len = b->used;
      return (char const *)b->mem;
    }
    default:
      return NULL;
  }
}

int cwal_value_fetch_array( cwal_value const * val, cwal_array ** ar)
{
  if( ! val ) return CWAL_RC_MISUSE;
  else if( !cwal_value_is_array(val) ) return CWAL_RC_TYPE;
  else
  {
    if(ar) *ar = CWAL_ARRAY(val);
    return 0;
  }
}

cwal_array * cwal_value_get_array( cwal_value const * v )
{
  cwal_array * ar = NULL;
  cwal_value_fetch_array( v, &ar );
  return ar;
}

cwal_value * cwal_array_value(cwal_array const * s)
{
  return s
    ? CWAL_VALPART(s)
    : NULL;
}

int cwal_array_unref(cwal_array *x)
{
  cwal_value * v = CWAL_VALPART(x);
  return (v && v->scope)
    ? cwal_value_unref2(v->scope->e, v)
    : CWAL_RC_MISUSE;
}


int cwal_array_fetch_v2( cwal_array const * ar, cwal_midsize_t pos,
                         cwal_value ** v, bool resolvePropref ){
  if( pos >= ar->list.count ) return CWAL_RC_RANGE;
  else{
    int rc = 0;
    if(v){
      cwal_value * rv = (cwal_value*)ar->list.list[pos];
      if(v && rv && resolvePropref && CWAL_TYPE_PROPREF==rv->vtab->typeID){
        cwal_propref * const p = CWAL__V2PROPREF(rv);
        rc = cwal_propref_resolve(p, CWAL_VALPART(ar), &rv);
      }
      if(0==rc && v) *v = rv;
    }
    return rc;
  }
}

int cwal_array_fetch( cwal_array const * ar, cwal_midsize_t pos,
                            cwal_value ** v ){
  return cwal_array_fetch_v2(ar, pos, v, true);
}

cwal_value * cwal_array_get( cwal_array const * ar, cwal_midsize_t pos )
{
  cwal_value *v = NULL;
  cwal_array_fetch_v2(ar, pos, &v, true);
  return v;
}

cwal_value * cwal_array_get_v2( cwal_array const * ar, cwal_midsize_t pos,
                                bool resolvePropref )
{
  cwal_value *v = NULL;
  cwal_array_fetch_v2(ar, pos, &v, resolvePropref);
  return v;
}

/**
   Internal helper macro for array-centric functions.
*/
#define SETUP__ARRAY_ARGS                               \
  cwal_scope * const s = ar ? CWAL_VALPART(ar)->scope : 0;    \
  cwal_engine * const e = s ? s->e : 0;                       \
  if(!s || !e) return CWAL_RC_MISUSE

/**
   Internal impl for cwal_array_set(), the difference from that
   function's docs being that if resolvePropref is true then if ar[ndx]
   is a cwal_propref, v gets set in its target, else it gets set
   directly in the array. proprefs are ignored if v is NULL.
*/
int cwal_array_set_v2( cwal_array * const ar, 
                       cwal_midsize_t ndx, cwal_value * v,
                       bool resolvePropref ){
  SETUP__ARRAY_ARGS;
  if( !ar ) return CWAL_RC_MISUSE;
  else if(CWAL_OB_IS_LOCKED(ar)) return CWAL_RC_LOCKED;
  else if( (ndx+1) > CwalConsts.MaxSizeTCounter) /* overflow */return CWAL_RC_RANGE;
  else{
    cwal_size_t len;
    len = ar->list.alloced;
    if(len <= ndx){
      len = cwal_list_reserve( e, &ar->list,
                               (ndx<CwalConsts.InitialArrayLength)
                               ? CwalConsts.InitialArrayLength
                               : ndx+1 );
      if( len <= ndx ) return CWAL_RC_OOM;
    }
    int rc;
    cwal_value * const arV = cwal_array_value( ar );
    cwal_value * const old = (cwal_value*)ar->list.list[ndx];
    assert( arV && (CWAL_TYPE_ARRAY==arV->vtab->typeID) );
    if( old ){
      if(old == v) return 0;
      else if(old && v && resolvePropref
              && CWAL_TYPE_PROPREF==old->vtab->typeID){
        cwal_propref * const p = CWAL__V2PROPREF(old);
        return cwal__propref_set(p, false, v, CWAL_VAR_F_PRESERVE);
      }
    }
    if(v){
      rc = cwal_value_xscope( e, arV->scope, v, 0 );
      if(rc){
        cwal__fatal(CWAL_RC_ASSERT, "xscope failed! "
                    "THIS IS SUPPOSED TO BE IMPOSSIBLE :(\n");
        return rc;
      }
      cwal_value_ref2( e, v );
    }
    if(old) cwal_value_unref2(e, old);
    ar->list.list[ndx] = v;
    if( ndx >= ar->list.count ){
      ar->list.count = ndx+1;
    }
    return 0;
  }
}

int cwal_array_set( cwal_array * const ar, cwal_midsize_t ndx,
                    cwal_value * const v ){
  return cwal_array_set_v2( ar, ndx, v, true);
}

cwal_value * cwal_array_take( cwal_array * const ar, cwal_midsize_t pos ){
  cwal_value *v = NULL;
  cwal_array_fetch_v2(ar, pos, &v, false);
  if(v){
    if(CWAL_MEM_IS_BUILTIN(v)){
      cwal_array_set_v2(ar, pos, NULL, false);
    }else{
      cwal_value_ref(v);
      cwal_array_set_v2(ar, pos, NULL, false);
      cwal_value_unhand(v);
      assert(v->scope && "Already dead?");
    }
  }
  return v;
}

cwal_midsize_t cwal_array_capacity_get( cwal_array const * ar ){
  return ar->list.alloced;
}

int cwal_array_length_fetch( cwal_array const * ar, cwal_midsize_t * v )
{
  if( ! ar || !v ) return CWAL_RC_MISUSE;
  else{
    if(v) *v = ar->list.count;
    return 0;
  }
}

cwal_midsize_t cwal_array_length_get( cwal_array const * ar )
{
  cwal_midsize_t i = 0;
  cwal_array_length_fetch(ar, &i);
  return i;
}

int cwal_array_length_set( cwal_array * ar, cwal_midsize_t newSize ){
  cwal_midsize_t i;
  if(!ar) return CWAL_RC_MISUSE;
  else if(CWAL_V_IS_VISITING_LIST(CWAL_VALPART(ar))) return CWAL_RC_IS_VISITING_LIST;
  else if(CWAL_OB_IS_LOCKED(ar)) return CWAL_RC_LOCKED;
  else if(ar->list.count == newSize) return 0;
  if( newSize < ar->list.count ){
    int rc = 0;
    for( i = newSize; !rc && (i < ar->list.count); ++i ){
      rc = cwal_array_set_v2( ar, i, NULL, false );
    }
    ar->list.count = newSize;
    return rc;
  }
  else { /* grow */
    int const rc = cwal_array_reserve( ar, newSize );
    if(!rc){
      ar->list.count = newSize;
    }
    return rc;
  }
}


/**
   Internal helper for recycling array list memory. li must be a new,
   clean list with no memory (that might get assert()ed).  If the
   recyling list has an entry then that entry's memory is transfered
   into li. If no entry is capable of holding it, li is left
   unmolested. There are no error conditions except for precondition
   violations (assertions). If minCount is not 0 then only a recycled
   chunk with enough space for at least that many entries will serve
   the request.
*/
static void cwal_list_from_recycler( cwal_engine * e, cwal_list * li,
                                     cwal_size_t minCount );

int cwal_array_reserve( cwal_array * ar, cwal_midsize_t size )
{
  SETUP__ARRAY_ARGS;
  if( ! ar ) return CWAL_RC_MISUSE;
  else if( size <= ar->list.alloced )
  {
    /* We don't want to introduce a can of worms by trying to
       handle the cleanup from here.
    */
    return 0;
  }
#if 0
  else if(!ar->list.list){
    cwal_list_from_recycler(e, &ar->list, size);
    if(ar->list.list){
      assert(ar->list.alloced>=size);
      assert(NULL == ar->list.list[0]);
      assert(size ? (NULL == ar->list.list[size-1]) : 1);
      return 0;
    }
  }
#endif
  else{
    CWAL_UNUSED_VAR cwal_size_t const oldLen = ar->list.alloced;
    cwal_size_t rrc;
    rrc = cwal_list_reserve( e, &ar->list, size );
    if(rrc < size) return CWAL_RC_OOM;
    else{
      assert(rrc > oldLen);
      return 0;
    }
  }
}


/** @internal

    cwal_list_visitor_f which expects V to be a (cwal_value*) and
    and VParent to be its (cwal_value*) scoping parent.
    This makes sure that (sub)child are properly up-scoped
    if needed. Returns 0 on success.
*/
static int cwal_xscope_visitor_children_array( void * V, void * VParent ){
  cwal_value * par = (cwal_value*)VParent;
  cwal_value * child = (cwal_value*)V;
  assert(par && par->scope);
  return cwal_value_xscope( par->scope->e, par->scope, child, 0 );
}

static void cwal_htable_rescope(cwal_scope * const sc,
                                cwal_htable * const h){
  /* cwal_dump_v(nv,"Re-scoping cwal_htable children..."); */
  int rc = 0;
  cwal_midsize_t const max = h->list.alloced>=h->hashSize
    ? h->hashSize : h->list.alloced;
  for( cwal_midsize_t i = 0; !rc && (i < max); ++i ){
    cwal_kvp * kvp = (cwal_kvp*)h->list.list[i];
    if(!kvp) continue;
    cwal_kvp * next = NULL;
    for( ; !rc && kvp; kvp = next){
      next = kvp->right;
      assert(kvp->key);
      assert(kvp->value);
      rc = cwal_value_xscope(sc->e, sc, kvp->key, 0);
      if(!rc && kvp->key != kvp->value){
        rc = cwal_value_xscope(sc->e, sc, kvp->value, 0);
      }
      /* cwal_dump_v(kvp->key,"Re-scoped key"); */
      /* cwal_dump_v(kvp->value,"Re-scoped value"); */
    }
    assert(!rc && "Rescoping failure is no longer be possible "
           "except in the case of memory corruption.");
  }
}

int cwal_rescope_children_obase( cwal_value * v ){
  cwal_obase * const b = CWAL_VOBASE(v);
  int rc = CWAL_RC_OK;
  assert(b);
  assert(CWAL_V_IS_RESCOPING(v));
  assert(v->scope);
#if CWAL_OBASE_ISA_HASH
  cwal_htable_rescope(v->scope, &b->hprops);
#else
  cwal_obase_kvp_iter iter;
  cwal_kvp const * kvp = cwal_obase_kvp_iter_init(v, &iter);
  for( ; kvp && (0==rc); kvp = cwal_obase_kvp_iter_next(&iter) ){
    if(kvp->key){
      rc = cwal_value_xscope( v->scope->e, v->scope, kvp->key, 0 );
    }
    if((0==rc) && kvp->value){
      rc = cwal_value_xscope( v->scope->e, v->scope, kvp->value, 0 );
    }
  }
  if(rc){
    assert(!"Rescoping failure should no longer be possible.");
  }
#endif
  return rc;
}

int cwal_rescope_children_native( cwal_value * v ){
  int rc;
  cwal_native * n = cwal_value_get_native(v);
  assert(v->scope);
  assert(n);
  rc = cwal_rescope_children_obase(v);
  if(!rc && n->rescoper){
    rc = n->rescoper( v->scope, v );
  }
  return rc;
}

int cwal_rescope_children_function( cwal_value * v ){
  int rc;
  cwal_function * f = cwal_value_get_function(v);
  assert(v->scope);
  assert(f);
  assert(CWAL_V_IS_RESCOPING(v));
  rc = cwal_rescope_children_obase(v);
  if(!rc && f->rescoper){
    rc = f->rescoper( v->scope, v );
  }
  return rc;
}

int cwal_rescope_children_unique( cwal_value * v ){
  cwal_value * ch = *CWAL_UNIQUE_VALPP(v);
  int rc = 0;
  assert(v->scope);
  if(ch){
    *CWAL_UNIQUE_VALPP(v) = 0
      /* a poor man's recursion-prevention scheme. */;
    rc = cwal_value_rescope(v->scope, ch);

    *CWAL_UNIQUE_VALPP(v) = ch;
  }
  return rc;
}

int cwal_rescope_children_tuple( cwal_value * v ){
  cwal_tuple * p = CWAL_TUPLE(v);
  cwal_size_t i;
  cwal_value * ch;
  int rc = 0;
  assert(!CWAL_MEM_IS_BUILTIN(v));
  assert(p->n || !p->list /* this gets called once from cwal_value_new() */);
  assert(v->scope);
  for( i = 0; !rc && i < p->n; ++i ){
    if( (ch = p->list[i]) ){
      rc = cwal_value_xscope(v->scope->e, v->scope, ch, 0);
    }
  }
  return rc;
}

int cwal_rescope_children_array( cwal_value * v ){
  int rc;
  cwal_array * ar = cwal_value_get_array(v);
  assert(ar);
  assert(CWAL_V_IS_RESCOPING(v));
  rc = cwal_rescope_children_obase( v );
  if(rc) return rc;
  rc = cwal_list_visit( &ar->list,
                        -1, cwal_xscope_visitor_children_array, v );
  return rc;
}

int cwal_value_rescope( cwal_scope * s, cwal_value * v ){
  return (!s || !s->e)
    ? CWAL_RC_MISUSE
    : ((v && CWAL_MEM_IS_BUILTIN(v))
       ? 0
       : cwal_value_xscope( s->e, s, v, NULL ) );
}

int cwal_value_xscope( cwal_engine * const e, cwal_scope * const par,
                       cwal_value * child, int * res ){
  cwal_obase * chb;
  int RC = res ? *res : 0;
  if(!res) res = &RC/*simplifies some code below*/;
  if(!par) {
    *res = 1;
    return 0;/*par = e->current;*/
  }
  assert( e && par && child );
  start:
  if( CWAL_MEM_IS_BUILTIN(child) ) {
    *res = 1;
    return CWAL_RC_OK;
  }
  else if(child->scope == par) {
    *res = 0;
    return CWAL_RC_OK;
  }
  chb = CWAL_VOBASE(child);
  assert(chb ? !CWAL_RCFLAG_HAS(child,CWAL_RCF_IS_DESTRUCTING) : 1);
  if(CWAL_V_IS_RESCOPING(child)){
    *res = 0;
    assert(child->scope->level <= par->level);
    /* MARKER(("Skipping re-rescoping.\n")); */
    /* haven't yet seen this happen! */
    return 0;
  }
  else
#if 0
    /*
      20160206: what was this for, way back when?  This block IS being
      triggered via s2 unit tests, but i'm curious what this is
      supposed to accomplish. Seems to work fine without it, but
      probably only because child->scope->level is always < par->level
      in this case. And yet i'm not certain why!

      Was it for breaking cycles? The children-rescopers don't
      set the CWAL_F_IS_VISITING flag. Maybe they used to?

      i think i see now... we've got a complex call chain which is
      trying to upscope a child, but that child is currently being
      visited. Failing to upscope would be an error, but continuing
      from here would lead to an assertion later (and we'd be unable
      to catch cycles). We need a flag for "is rescoping."
    */
    if( chb && ( CWAL_F_IS_VISITING & chb->flags ) ){
      /* dump_val(child,"is visiting?"); */
      assert(child->scope->level <= par->level)
        /* This assertion is triggered in at least 1 s2 unit test
           script. Troubling.
        */;
      *res = 0;
      return 0
        /* Normally we would return CWAL_RC_CYCLES_DETECTED,
           but for this special case we need to return 0
           to keep list iteration from aborting. */;
    }
    else
#endif
    {
      int rc = CWAL_RC_OK;
      if( child->scope ){
        CWAL_TR_V(e,child);
        if( child->scope->level <= par->level ){
          CWAL_TR3(e,CWAL_TRACE_MESSAGE,
                   "Keeping value in its current scope.");
          *res = 1;
          return 0;
        }
        else{
          CWAL_TR3(e,CWAL_TRACE_MESSAGE,
                   "Migrating value to older scope.");
          rc = cwal_value_take( e, child );
          if(rc){
            CWAL_TR_SV(e,child->scope,child);
            CWAL_TR3(e,CWAL_TRACE_ERROR,
                     "POPPING FROM ITS PARENT SCOPE IS NO "
                     "LONGER SUPPOSED TO BE ABLE TO FAIL "
                     "IN THESE CONDITIONS.");
            assert(!"Pop child from its scope failed.");
            return e->fatalCode = rc;
          }
        }
      }
      assert(!child->scope);
      *res = -1;
      if( cwal_scope_insert( par, child ) ){
        assert(e->fatalCode);
        return e->fatalCode;
      }
      if(child->vtab->rescope_children){
        /* For containers we now, for the sake of cross-scope
           cycles, we need to ensure that any sub-(sub...)children
           are up-scoped.
        */
        if(!chb){
          assert(CWAL_TYPE_UNIQUE==child->vtab->typeID
                 || CWAL_TYPE_TUPLE==child->vtab->typeID
                 || CWAL_TYPE_PROPREF==child->vtab->typeID
                 );
          /*
            Doh... we have a potentially problem: we can
            potentially endlessly cycle on cwal_uniques. They
            have no flags which will let us stop recursion!
            Does that matter, since the value being wrapped has
            them (if a container, else it's moot)? In the
            non-container case, the 2nd rescope would not
            recurse because the scope level will have already
            been adjusted. Whew.
          */
          /* dump_val(child,"Has rescope_children but no obase!?!?!?"); */
        }
        assert(!CWAL_V_IS_RESCOPING(child));
        CWAL_RCFLAG_ON(child,CWAL_RCF_IS_RESCOPING);
        assert(CWAL_V_IS_RESCOPING(child));
        rc = child->vtab->rescope_children(child);
        assert(CWAL_V_IS_RESCOPING(child));
        CWAL_RCFLAG_OFF(child,CWAL_RCF_IS_RESCOPING);
        assert(!CWAL_V_IS_RESCOPING(child));
        if(0!=rc){
          /* Reminder: now that values are tracked in linked
             lists, xscope can only fail if some assertion
             fails. There is no longer the potential for an OOM
             case.
          */
          MARKER(("xscope returned %d/%s\n", rc, cwal_rc_cstr(rc)));
          cwal__fatal(CWAL_RC_CANNOT_HAPPEN,
                      "NO RECOVERY STRATEGY HERE!");
          return e->fatalCode = rc;
        }
      }
      assert(par == child->scope);
      if(chb && chb->prototype && chb->prototype->scope
         && chb->prototype->scope->level > par->level){
        /*
          Added 20141205 when it suddenly occurred to me that we
          do not otherwise make prototypes vacuum-safe (not that i
          could find, anyway). Seems to have never been necessary
          before.
        */
        /* MARKER(("Rescoping my prototype! Why does this never need to happen?\n")); */
        child = chb->prototype;
        res = &RC /* keep original res result in place */;
        goto start;
      }
      return rc;
    }
}

int cwal_value_upscope( cwal_value * v ){
  cwal_engine * e = CWAL_VENGINE(v);
  if(!e || !e->current || !v->scope) return CWAL_MEM_IS_BUILTIN(v) ? 0 : CWAL_RC_MISUSE;
  else if(!e->current->parent){
    assert(1==e->current->level);
    assert(e->current == v->scope);
    return 0;
  }
  else if(e->current->parent == v->scope) return 0;
  else {
    int rc, dir = 0;
    rc = cwal_value_xscope( e, e->current->parent, v, &dir);
    assert(0==rc);
    return rc;
  }    
}


cwal_value * cwal_propagating_get( cwal_engine * e ){
  return e->values.propagating;
}

cwal_value * cwal_propagating_set( cwal_engine * e, cwal_value * v ){
  if(v != e->values.propagating){
    if(v) cwal_value_ref(v);
    if(e->values.propagating){
      cwal_value_unref(e->values.propagating);
    }
    e->values.propagating = v;
  }
  return v;
}

cwal_value * cwal_propagating_take( cwal_engine * e ){
  cwal_value * rv = e->values.propagating;
  if(rv){
    cwal_value_ref(rv);
    cwal_propagating_set(e, 0);
    cwal_value_unhand(rv);
  }
  return rv;
}

cwal_value * cwal_exception_take( cwal_engine * const e ){
  cwal_value * const rv = e->values.exception;
  if(rv){
    cwal_value_ref(rv);
    cwal_exception_set(e, 0);
    cwal_value_unhand(rv);
  }
  return rv;
}

int cwal_exception_set( cwal_engine * const e, cwal_value * const v ){
  if(v == e->values.exception) return v ? CWAL_RC_EXCEPTION : 0;
  else if(!v) {
    if(e->values.exception) cwal_value_unref(e->values.exception);
    e->values.exception = 0 /* its scope owns it */;
    return 0;
  }
  else{
    cwal_value_ref(v);
    if(e->values.exception) cwal_value_unref(e->values.exception);
    e->values.exception = v;
    /* cwal_value_ref(v); */
    return CWAL_RC_EXCEPTION;
  }
}

int cwal_exception_setfv(cwal_engine * const e, int code,
                         char const * fmt, va_list args){
  int rc;
  assert(e->current && e->current->level);
  switch(code){
    case CWAL_RC_OOM: rc = code;
      break;
    default: {
      cwal_exception * x;
      x = (fmt && *fmt)
        ? cwal_new_exceptionfv(e, code, fmt, args)
        : cwal_new_exception(e, code, NULL);
      if(!x) rc = CWAL_RC_OOM;
      else{
        cwal_value * xv = cwal_exception_value(x);
        cwal_value_ref(xv);
        rc = cwal_exception_set( e, xv );
        cwal_value_unref(xv);
        assert(0!=rc);
      }
      break;
    }
  }
  return rc;
}
int cwal_exception_setf(cwal_engine * const e, int code, char const * fmt, ...){
  int rc;
  va_list args;
  va_start(args,fmt);
  rc = cwal_exception_setfv(e, code, fmt, args);
  va_end(args);
  return rc;
}

int cwal_cb_throw( cwal_callback_args const * args, int code, char const * fmt, ... ){
  int rc;
  va_list vargs;
  va_start(vargs,fmt);
  rc = cwal_exception_setfv(args->engine, code, fmt, vargs);
  va_end(vargs);
  return rc;
}

cwal_value * cwal_exception_get( cwal_engine * const e ){
  return e ? e->values.exception : 0;
}

#if 0
/* keeping around for a look at its heurists later on. */
/**
   Internal helper for recycling buffer memory. dest must be a new,
   clean buffer with no memory (that might get assert()ed).  If the
   recyling list has an entry which can serve at least forAllocSize
   bytes, that entry's memory is transfered into dest. If no entry is
   capable of holding it, dest->mem will still be 0 after
   returning. There are no error conditions except for precondition
   violations (assertions).

*/
/* static */ void cwal_buffer_steal_mem( cwal_engine * e, cwal_buffer * dest,
                                         cwal_size_t forAllocSize){
#if 0
  /*Enable this section to effectively disable buffer->mem recycling
    for memory cost/savings comparisons. */
  return;
#else
  assert(!dest->mem);
  assert(!dest->used);
  if(dest->mem || e->reBuf.cursor<0) return;
  else{
    int i = e->reBuf.cursor;
    /* Potential TODO: find the closest-(but >=)-fit entry */
    for( ; i < (int)(sizeof(e->reBuf.buffers)/sizeof(e->reBuf.buffers[0]));
         ++i ){
      cwal_buffer * br = &e->reBuf.buffers[i];
      if(br->mem
         /* Try an approximate fit... */
         && br->capacity>=forAllocSize
#if 1
         /* This heuristic is very basic, of course. */
         && (br->capacity <= 64 * CWAL_SIZE_T_BITS /* 1k, 2k, 4k */
             || br->capacity<= 2 * forAllocSize
             )
#endif
         ){
        *dest = *br;
        /* MARKER(("Re-using buffer memory (%"CWAL_SIZE_T_PFMT") from slot #%d\n", br->capacity, e->reBuf.cursor)); */
        if(e->reBuf.cursor != i){
          /* Move the final buffer in the list to this slot,
             so that our list is always contiguous. */
          *br = e->reBuf.buffers[e->reBuf.cursor];
          e->reBuf.buffers[e->reBuf.cursor] = cwal_buffer_empty;
        }else{
          *br = cwal_buffer_empty;
        }
        --e->reBuf.cursor;
        break;
      }
    }
  }
#endif
}
#endif

void cwal_list_from_recycler( cwal_engine * e, cwal_list * list,
                              cwal_size_t minCount ){
#if 0
  /*Enable this section to effectively disable array->list recycling
    for memory cost/savings comparisons. */
  return;
#else
  if(list->list) return;
  else{
    void * mem;
    cwal_size_t reqSize = minCount * sizeof(void*);
    assert(!list->alloced);
    if( (mem = cwal_memchunk_request(e, &reqSize, 1000,
                                     minCount
                                     ? "cwal_list_from_recycler(min)"
                                     : "cwal_list_from_recycler(0)"
                                     ))){
      assert(reqSize>= minCount * sizeof(void*));
      list->list = (void **)mem;
      list->alloced = reqSize/sizeof(void*)
        /* Yes, we might be losing a few bytes here. The alternatives
           include:

           (A) Put it back and try for another (aligned) one
           (or give up).
                   
           (B) Add a flag to cwal_memchunk_request() which specifies
           we need it aligned.

           (C) Make cwal_re/alloc() always align up and rely on that
           in the recycler? The culprit is really cwal_buffer_reserve(),
           so that would be the one to patch.

           (D) Something different?
        */
        ;
      assert(list->alloced >= minCount);
      /*MARKER(("Reused array list memory: %u entries from %u bytes\n",
        (unsigned)list->alloced, (unsigned)reqSize));*/
    }        
  }
#endif
}

int cwal_array_append( cwal_array * const ar, cwal_value * const v ){
  SETUP__ARRAY_ARGS;
  if( !ar ) return CWAL_RC_MISUSE;
  else if(CWAL_OB_IS_LOCKED(ar)) return CWAL_RC_LOCKED;
  else if( (ar->list.count+1) < ar->list.count ) return CWAL_RC_RANGE;
  else{
    if(!ar->list.list) cwal_list_from_recycler(e, &ar->list, 0);
    if( !ar->list.alloced
        || (ar->list.count == ar->list.alloced-1)){
      unsigned const int n = ar->list.count
        ? ar->list.alloced * 2
        : CwalConsts.InitialArrayLength;
      if( n > cwal_list_reserve( e, &ar->list, n ) ){
        return CWAL_RC_OOM;
      }
    }
    return cwal_array_set_v2( ar, ar->list.count, v, false );
  }
}

int cwal_array_prepend( cwal_array * const ar, cwal_value * const v ){
  SETUP__ARRAY_ARGS;
  if( !ar || !v ) return CWAL_RC_MISUSE;
  else if(CWAL_OB_IS_LOCKED(ar)) return CWAL_RC_LOCKED;
  else{
    int rc;
    cwal_value ** vlist;
    if(!ar->list.list) cwal_list_from_recycler(e, &ar->list, 1);
    if( !ar->list.alloced
        || (ar->list.count == ar->list.alloced-1)){
      unsigned const int n = ar->list.count
        ? ar->list.alloced * 2
        : CwalConsts.InitialArrayLength;
      if( n > cwal_list_reserve( e, &ar->list, n ) ){
        return CWAL_RC_OOM;
      }
    }
    if(ar->list.count){
      unsigned char * mem =
        (unsigned char *)ar->list.list;
      memmove( mem+sizeof(cwal_value*), mem,
               sizeof(cwal_value*)*ar->list.count);
    }
    vlist = (cwal_value **)ar->list.list;
    vlist[0] = NULL;
    ++ar->list.count;
    rc = cwal_array_set_v2( ar, 0, v, false );
    /* A recovery here on error would be messy, but the
       set() can only fail on allocation error and we've
       already done the allocation.
    */
    assert(!rc);
    assert(v == *((cwal_value**)ar->list.list));
    return rc;
  }
}

int cwal_array_shift( cwal_array * ar, cwal_value **rv ){
  SETUP__ARRAY_ARGS;
  if( !ar ) return CWAL_RC_MISUSE;
  else{
    cwal_value ** vlist;
    unsigned char * mem;
    cwal_value * v;
    if(!ar->list.count) return CWAL_RC_RANGE;
    vlist = (cwal_value **)ar->list.list;
    v = vlist[0];
    if(rv) *rv = v;
    mem = (unsigned char *)ar->list.list;
    memmove( mem, mem+sizeof(cwal_value*),
             sizeof(cwal_value*)*(ar->list.count-1));
    vlist[--ar->list.count] = NULL;
    if(v){
      if(rv) cwal_value_unhand(v);
      else cwal_value_unref(v);
    }
    return 0;
  }
}

int cwal_array_index_of( cwal_array const * ar, cwal_value const * v,
                         cwal_size_t * index, bool strictComparison ){
  if(!ar) return CWAL_RC_MISUSE;
  else if(!ar->list.count) return CWAL_RC_NOT_FOUND;
  else{
    cwal_size_t i;
    cwal_value * const arv = CWAL_VALPART(ar);
    int opaque = 0;
    cwal_visit_list_begin(arv, &opaque);
#if 0
    /* Non-propref-handling impl. */
    for( i = 0; i < ar->list.count; ++i ){
      cwal_value const * rhs = (cwal_value const *)ar->list.list[i];
      if(v==rhs) break /* also match on NULL */;
      else if((v && !rhs) || (!v && rhs)) continue;
      else if(strictComparison){
        if(v->vtab->typeID == rhs->vtab->typeID
           && 0 == cwal_value_compare(v, rhs)) break;
      }else if(0 == cwal_value_compare(v, rhs)) break;
    }
#else
    /* Propref-handling impl. */
    cwal_engine * const e = arv->scope->e;
    cwal__err_reset(e);
    for( i = 0; i < ar->list.count; ++i ){
      cwal_value * rhs = (cwal_value *)ar->list.list[i];
      if(v==rhs) break /* also match on NULL */;
      else if((v && !rhs) || (!v && rhs)) continue;
      else if(CWAL_TYPE_PROPREF == rhs->vtab->typeID){
        rhs = cwal_array_get(ar, i);
        if(e->err.code) return e->err.code;
      }
      if(strictComparison && v->vtab->typeID != rhs->vtab->typeID) continue;
      else 
      if(0 == cwal_value_compare(v, rhs)) break;
    }
#endif
    cwal_visit_list_end(arv, opaque);
    if(i < ar->list.count){
      if(index) *index = i;
      return 0;
    }else{
      return CWAL_RC_NOT_FOUND;
    }
  }
}

int cwal_array_copy_range( cwal_array * ar, cwal_size_t offset,
                           cwal_size_t count,
                           cwal_array **dest ){
  SETUP__ARRAY_ARGS;
  if( !ar || !dest || (ar==*dest) ) return CWAL_RC_MISUSE;
  else if(CWAL_OB_IS_LOCKED(ar) || (*dest && CWAL_OB_IS_LOCKED(*dest))) return CWAL_RC_LOCKED;
  else{
    int rc = 0;
    cwal_size_t i;
    cwal_array * tgt = *dest ? *dest : cwal_new_array(e);
    cwal_value ** vlist;
    cwal_value * const arv = CWAL_VALPART(ar);
    int opaque = 0;
    cwal_visit_list_begin(arv, &opaque);
    vlist = (cwal_value **)ar->list.list;
    if(offset<ar->list.count){
      cwal_size_t to;
      if(!count) count = ar->list.count - offset;
      to = offset + count;
      if(to > ar->list.count) to = ar->list.count;
      rc = cwal_array_reserve( tgt, count );
      for( i = offset;
           !rc && (i<to);
           ++i ){
        cwal_value * v = vlist[i];
        rc = cwal_array_append( tgt, v );
      }
    }
    cwal_visit_list_end(arv, opaque);
    if(!rc) *dest = tgt;
    else if(rc && (*dest != tgt)) cwal_array_unref(tgt);
    return rc;
  }
}

int cwal_array_copy_range2( cwal_array * ar, cwal_int_t offset,
                            cwal_int_t count, cwal_array **dest ){
  SETUP__ARRAY_ARGS;
  if( !ar || !dest || (ar==*dest) ) return CWAL_RC_MISUSE;
  else if(CWAL_OB_IS_LOCKED(ar) || (*dest && CWAL_OB_IS_LOCKED(*dest))) return CWAL_RC_LOCKED;
  else{
    int rc = 0;
    cwal_size_t i, off, n;
    cwal_array * tgt = *dest ? *dest : cwal_new_array(e);
    cwal_value ** vlist;
    cwal_value * const arv = CWAL_VALPART(ar);
    int opaque = 0;
    if(offset<0){
      if( -offset > (cwal_int_t)ar->list.count ) off = 0;
      else off = ar->list.count + offset;
    }else{
      off = (cwal_size_t)offset;
    }
    if(count<0) n = ar->list.count;
    else n = (cwal_size_t)count;
    cwal_visit_list_begin(arv, &opaque);
    vlist = (cwal_value **)ar->list.list;
    if(off<ar->list.count){
      cwal_size_t to = off + n;
      if(to > ar->list.count) to = ar->list.count;
      assert(to>=off);
      rc = cwal_array_reserve( tgt, to-off );
      for( i = off; !rc && i<to; ++i ){
        cwal_value * v = vlist[i];
        rc = cwal_array_append( tgt, v );
      }
    }
    cwal_visit_list_end(arv, opaque);
    if(!rc) *dest = tgt;
    else if(rc && (*dest != tgt)) cwal_array_unref(tgt);
    return rc;
  }
}



int cwal_value_fetch_object( cwal_value const * val, cwal_object ** ar){
  if( ! val ) return CWAL_RC_MISUSE;
  else if( !cwal_value_is_object(val) ) return CWAL_RC_TYPE;
  else{
    if(ar) *ar = CWAL_OBJ(val);
    return 0;
  }
}

cwal_object * cwal_value_get_object( cwal_value const * v ) {
  cwal_object * ar = NULL;
  cwal_value_fetch_object( v, &ar );
  return ar;
}

cwal_value * cwal_object_value(cwal_object const * s){
  return s
    ? CWAL_VALPART(s)
    : NULL;
}

int cwal_object_unref(cwal_object *x){
  cwal_value * v = CWAL_VALPART(x);
  return (v && v->scope)
    ? cwal_value_unref2(v->scope->e, v)
    : CWAL_RC_MISUSE;
}

/**
   The C-string equivalent of cwal_obase_search_v().
*/
static cwal_value * cwal_obase_search( cwal_obase const * base,
                                       bool searchProto,
                                       char const * const key,
                                       cwal_midsize_t keyLen){
  if(!base || !key) return NULL;
  else {
    cwal_kvp * kvp;
    cwal_value * rc = NULL;
    while(base){
      if(CWAL_F_LOCKED & base->flags) break;
#if CWAL_OBASE_ISA_HASH
      cwal_htable const * pht = &base->hprops;
      kvp = cwal_htable_search_impl_cstr(NULL, &pht, true,
                                         key, keyLen, NULL, NULL);
      assert(pht == &base->hprops);
#else
      kvp = cwal_kvp_search( base->kvp, key, keyLen, prev );
#endif
      if(kvp) {
        rc = kvp->value;
        break;
      }
      else base = searchProto ? CWAL_VOBASE(base->prototype) : 0;
    }
    return rc;
  }
}

/**
   Counterpart of cwal_kvp_search_v(). Searches for key in base->kvp.
   If not found and searchProto is true, it searches base->prototype
   (recursively). If it encounters a locked object (base or one of its
   prototypes) while searching, NULL is returned. It does not simply
   skip over the locked member because the search results could then
   arguably be inconsistent, depending on whether the locked member
   actually overrides the property from a prototype further up the
   chain.
*/
static cwal_value * cwal_obase_search_v( cwal_obase const * base,
                                         bool searchProto,
                                         cwal_value const * key){
  if(!base || !key) return NULL;
  else {
    cwal_kvp * kvp;
    cwal_value * rc = NULL;
    while(base){
      if(CWAL_F_LOCKED & base->flags) break;
#if CWAL_OBASE_ISA_HASH
      cwal_htable const * pht = &base->hprops;
      kvp = cwal_htable_search_impl_v(NULL, &pht, true, key, NULL, NULL);
#else
      kvp = cwal_kvp_search_v( base->kvp, key, prev );
#endif
      if(kvp) {
        rc = kvp->value;
        break;
      }
      else {
        base = searchProto ? CWAL_VOBASE(base->prototype) : 0;
      }
    }
    return rc;
  }
}


bool cwal_prop_has( cwal_value const * v,
                    char const * key, cwal_midsize_t keyLen,
                    bool searchPrototype ) {
  cwal_obase const * const base = CWAL_VOBASE(v);
  return (base && key)
    ? !!cwal_obase_search(base, searchPrototype, key, keyLen)
    : false;
}

bool cwal_prop_has_v( cwal_value const * v,
                      cwal_value const * key,
                      bool searchPrototype ){
  cwal_obase const * base = CWAL_VOBASE(v);
  return (base && key)
    ? !!cwal_obase_search_v(base, searchPrototype, key )
    : false;
}

cwal_value * cwal_prop_get( cwal_value const * v,
                            char const * key, cwal_midsize_t keyLen ) {
  cwal_obase const * const base = CWAL_VOBASE(v);
  return (base && key)
    ? cwal_obase_search( base, true, key, keyLen )
    : NULL;
}


cwal_value * cwal_prop_get_v( cwal_value const * c,
                              cwal_value const * key ) {
  cwal_obase const * base = CWAL_VOBASE(c);
  return (base && key)
    ? cwal_obase_search_v( base, true, key )
    : NULL;
}

cwal_kvp * cwal_prop_get_kvp( cwal_value * c, char const * key,
                              cwal_midsize_t keyLen, bool searchProtos,
                              cwal_value ** foundIn ){
  return cwal__prop_get_kvp( false, c, key, keyLen, searchProtos, foundIn );
}


cwal_kvp * cwal_prop_get_kvp_v( cwal_value * c, cwal_value const * key,
                                bool searchProtos,
                                cwal_value ** foundIn ){
  return cwal__prop_get_kvp_v( false, c, key, searchProtos, foundIn);
}

int cwal_prop_unset( cwal_value * const c,
                     char const * key, cwal_midsize_t keyLen ) {
  cwal_obase * const b = CWAL_VOBASE(c);
  cwal_engine * const e = b ? CWAL_VENGINE(c) : NULL;
  if(!e) return CWAL_RC_MISUSE;
  else if(CWAL_RCFLAG_HAS(c,CWAL_RCF_IS_DESTRUCTING)) return CWAL_RC_DESTRUCTION_RUNNING;
  else if(CWAL_V_IS_VISITING(c)) return CWAL_RC_IS_VISITING;
  else if(CWAL_CONTAINER_DISALLOW_PROP_SET & b->flags){
    return CWAL_RC_DISALLOW_PROP_SET;
  }
#if CWAL_OBASE_ISA_HASH
  return cwal_htable_remove_impl_cstr(c, &b->hprops, key, keyLen);
#else
  return cwal_kvp_unset( e, &b->kvp, key, keyLen );
#endif
}

int cwal_prop_unset_v( cwal_value * c, cwal_value * key ) {
  cwal_obase * b = CWAL_VOBASE(c);
  cwal_engine * e = b ? CWAL_VENGINE(c) : NULL;
  if(!e) return CWAL_RC_MISUSE;
  else if(CWAL_RCFLAG_HAS(c,CWAL_RCF_IS_DESTRUCTING)) return CWAL_RC_DESTRUCTION_RUNNING;
  else if(CWAL_V_IS_VISITING(c)) return CWAL_RC_IS_VISITING;
  else if(CWAL_CONTAINER_DISALLOW_PROP_SET & b->flags) return CWAL_RC_DISALLOW_PROP_SET;
#if CWAL_OBASE_ISA_HASH
  return cwal_htable_remove_impl_v(c, &b->hprops, key);
#else
  return cwal_kvp_unset_v( e, &b->kvp, key );
#endif
}

#if !CWAL_OBASE_ISA_HASH
/**
   Adds an entry or updates an existing one in the given kvp list.
   listOwner must be the value which owns/manages list. key is the key
   to search for and value is the value to set it
   to. key->vtab->compare() is used to test for equivalence.

   On success listOwner->kvp may be modified.

   Returns 0 on success, CWAL_RC_MISUSE if any arguments are NULL, and
   CWAL_RC_OOM on allocation error.

   Currently returns CWAL_RC_NOT_FOUND if the key is not found, but
   that's highly arguable to do from this level. We do it here because
   it would otherwise require a search-then-set (second lookup) in
   some oft-called client code.

   Returns CWAL_RC_CONST_VIOLATION if key refers to an existing value
   which is flagged as CWAL_VAR_F_CONST _unless_ v==theExistingValue
   (in which case it is a silent no-op which reports success). The
   (v==existing) workaround is admittedly to support operator
   overloading in th1ish (and subsequently s2), where (x+=1) is
   logically assigning to x (which may be const) but (because of the
   overload) is actually doing something quite different. TODO: see if
   we can remove this workaround - i don't think s2 needs it, but
   th1ish might rely on it.

   Returns CWAL_RC_DISALLOW_PROP_SET if listOwner has the
   CWAL_CONTAINER_DISALLOW_PROP_SET flag.

   Returns CWAL_RC_DISALLOW_NEW_PROPERTIES if listOwner is flagged
   with the CWAL_CONTAINER_DISALLOW_NEW_PROPERTIES flag _AND_
   key refers to a new property.

   If flags!=CWAL_VAR_F_PRESERVE then the flags of the kvp are set to
   the given value (but the CONST check trumps this).
*/
static int cwal_kvp_set_v( cwal_engine * e, cwal_value * listOwner,
                           cwal_value * key, cwal_value * v, uint32_t flags ){
  if( !e || !key || !listOwner || !v ) return CWAL_RC_MISUSE;
  else {
    int rc;
    cwal_kvp * kvp = 0;
    cwal_kvp * left = 0;
    bool iAlloccedKvp = 0;
    cwal_obase * const ob = CWAL_VOBASE(listOwner);
    assert(ob);
    if(CWAL_CONTAINER_DISALLOW_PROP_SET & ob->flags){
      return CWAL_RC_DISALLOW_PROP_SET;
    }
    kvp = cwal_kvp_search_v( ob->kvp, key, &left );
    if( !kvp ){
      if(CWAL_CONTAINER_DISALLOW_NEW_PROPERTIES & ob->flags){
        return CWAL_RC_DISALLOW_NEW_PROPERTIES;
      }
      else if(!v){
        return CWAL_RC_NOT_FOUND;
      }                 
      kvp = cwal_kvp_alloc( e );
      if( !kvp ) return CWAL_RC_OOM;
      iAlloccedKvp = 1;
      kvp->flags = (CWAL_VAR_F_PRESERVE==flags)
        ? CWAL_VAR_F_NONE : flags;
    }
    /*
      Here is where we would check if kvp->value is
      a function with the CWAL_CONTAINER_PROP_SETTER
      container flag. If so, we would return the call
      to that function, passing it (key, value).
    */
    else if(CWAL_VAR_F_CONST & kvp->flags){
      return CWAL_RC_CONST_VIOLATION;
    }
    if(CWAL_VAR_F_PRESERVE!=flags) kvp->flags = flags;

    /* Reminder: we have to increase the refcount of the key
       even if it's the same pointer as a key we have or the same
       as v. Remember that interning guarantees that we'll
       eventually see the same key instances.
    */
    for( int i = 0; i < 2; ++i ){ /* 0==key, 1==value */
      cwal_value * vv = 0==i ? key : v;
      rc = cwal_value_xscope( e, listOwner->scope, vv, 0 );
      if(rc){
        if(iAlloccedKvp){
          cwal_kvp_free( e/* , listOwner->scope */, kvp, 1 );
        }
        assert(!"This cannot fail since the transition from arrays to linked lists.");
        return rc;
      }
      /* The ref/unref order is important in case key==kvp->key or...
         in case v==kvp->value, key, or listOwner. */
      cwal_value_ref2( e, vv );
      if(0==i) { /* KEY part... */
        if(kvp->key){
          cwal_value_unref2( e, kvp->key );
        }
        kvp->key = vv;
      }else{ /* VALUE part... */
        assert(1 == i);
        if(kvp->value){
          cwal_value_unref2( e, kvp->value );
        }
        kvp->value = vv;
        break;
      }
    }
    assert(kvp->key != 0);
    assert(kvp->value != 0);
    if(!iAlloccedKvp){
      /* kvp is already in *list */
      assert(left || ob->kvp==kvp);
    }else if(left){
      /* kvp compares > left, so insert it here. */
      cwal_kvp * const r = left->right;
      assert(!kvp->right);
      left->right = kvp;
      kvp->right = r;
    }else{
      /* Make kvp the head of the list */
      assert(!left);
      assert(ob->kvp != kvp);
      assert(!kvp->right);
      kvp->right = ob->kvp;
      ob->kvp = kvp;
    }
    return CWAL_RC_OK;
  }
}
#endif /* !CWAL_OBASE_ISA_HASH */

static int cwal__prop_setx_v( cwal_value * c,
                              CWAL_UNUSED_VAR bool resolvePropref,
                              cwal_value * key, cwal_value * v,
                              uint16_t flags ) {
  cwal_obase * const b = CWAL_VOBASE(c);
  cwal_engine * const e = CWAL_VENGINE(c);
  assert(v ? (!!e || CWAL_MEM_IS_BUILTIN(v)) : 1);
  if(!e || !key) return CWAL_RC_MISUSE;
  else if(!b || !cwal_prop_key_can(key)) return CWAL_RC_TYPE;
  else if(CWAL_RCFLAG_HAS(c,CWAL_RCF_IS_DESTRUCTING)) return CWAL_RC_DESTRUCTION_RUNNING;
  else if(CWAL_V_IS_VISITING(c)) return CWAL_RC_IS_VISITING;
  else if(CWAL_CONTAINER_DISALLOW_PROP_SET & b->flags){
    return CWAL_RC_DISALLOW_PROP_SET;
  }
  if(v){
#if CWAL_OBASE_ISA_HASH
    return cwal_htable_insert_impl_v(c, &b->hprops, resolvePropref, key, v,
                                     true, flags, false); 
#else
    if(resolvePropref){/*unused*/}
    return cwal_kvp_set_v( e, c, key, v, flags );
#endif
  }
  return cwal_prop_unset_v( c, key );
}

static int cwal__prop_setx( cwal_value * c,
                            bool resolvePropref,
                            char const * key, cwal_midsize_t keyLen, cwal_value * v,
                            uint16_t flags ) {
  cwal_obase * const b = CWAL_VOBASE(c);
  cwal_engine * const e = b ? CWAL_VENGINE(c) : 0;
  if( !e || !key ) return CWAL_RC_MISUSE;
  else if(!b) return CWAL_RC_TYPE;
  else if(CWAL_V_IS_VISITING(c)) return CWAL_RC_IS_VISITING;
  else if(CWAL_CONTAINER_DISALLOW_PROP_SET & b->flags){
    return CWAL_RC_DISALLOW_PROP_SET;
  } else if(CWAL_RCFLAG_HAS(c,CWAL_RCF_IS_DESTRUCTING)){
    return CWAL_RC_DESTRUCTION_RUNNING;
  }
  else if( NULL == v ) return cwal_prop_unset( c, key, keyLen );
  cwal_value * const vkey = cwal_new_string_value(e, key, keyLen);
  if(!vkey) return CWAL_RC_OOM;
  int rc;
  cwal_value_ref(vkey);
  rc = cwal__prop_setx_v( c, resolvePropref, vkey, v, flags);
  cwal_value_unref2(e, vkey);
  return rc;
}

int cwal_prop_set_with_flags_v( cwal_value * c, cwal_value * key, cwal_value * v,
                                uint16_t flags ) {
  return cwal__prop_setx_v( c, true, key, v, flags );
}

int cwal_prop_set_v( cwal_value * c, cwal_value * key, cwal_value * v ) {
  return cwal__prop_setx_v( c, true, key, v, CWAL_VAR_F_PRESERVE );
}

int cwal_prop_set_with_flags( cwal_value * c,
                              char const * key, cwal_midsize_t keyLen, cwal_value * v,
                              uint16_t flags ) {
  return cwal__prop_setx( c, true, key, keyLen, v, flags );
}

int cwal_prop_set( cwal_value * c,
                   char const * key, cwal_midsize_t keyLen, cwal_value * v ) {
  return cwal__prop_setx( c, true, key, keyLen, v, CWAL_VAR_F_PRESERVE );
}

cwal_value * cwal_prop_take( cwal_value * c, char const * key ){
  cwal_obase * const b = CWAL_VOBASE(c);
  cwal_engine * const e = CWAL_VENGINE(c);
  assert(c ? !!e : 1);
  if( !e || !b || !key || CWAL_V_IS_VISITING(c)) return NULL;
#if CWAL_OBASE_ISA_HASH
  else if(!b->hprops.list.count) return NULL;
#else
  else if(!b->kvp) return NULL;
#endif
  else {
    /* FIXME: this is 90% identical to cwal_kvp_unset(),
       only with different refcount handling.
       Consolidate them.

       FIXME #2: instead of keeping our reference,
       drop the reference and reprobate the value
       if needed.
    */
    cwal_kvp * kvp;
    cwal_kvp * left = 0;
    cwal_value * rv = NULL;
#if CWAL_OBASE_ISA_HASH
    cwal_midsize_t ndx = 0;
    cwal_htable const * pht = &b->hprops;
    kvp = cwal_htable_search_impl_cstr(NULL, &pht, false,
                                       key, cwal_strlen(key),
                                       &ndx, &left);
    assert(pht == &b->hprops);
#else
    kvp = cwal_kvp_search( b->kvp, key,
                           cwal_strlen(key), &left );
#endif
    if( !kvp ) return NULL;
    rv = kvp->value;
    if(left){
      assert(kvp==left->right);
      left->right = kvp->right;
    }else{
#if CWAL_OBASE_ISA_HASH
      /* kvp is the only entry at ndx or the left-most entry in a hash
         collision */
      assert(b->hprops.list.list[ndx] == kvp);
      b->hprops.list.list[ndx] = kvp->right;
#else
      assert(b->kvp == kvp && "kvp is the head of b->kvp");
      b->kvp = kvp->right;
#endif
    }
#if CWAL_OBASE_ISA_HASH
    --b->hprops.list.count;
#endif
    kvp->right = NULL;
    assert(rv);
    cwal_value_unref2(e, kvp->key);
    kvp->key = 0;
    kvp->value = NULL/*steal its ref point*/;
    cwal_kvp_free( e, kvp, 1 );
    if(!CWAL_MEM_IS_BUILTIN(rv)){
      assert( (CWAL_REFCOUNT(rv) > 0) && "Should still have kvp's reference!" );
      cwal_value_unhand(rv);
    }
    return rv;
  }
}

cwal_value * cwal_prop_take_v( cwal_value * c, cwal_value * key,
                               cwal_value ** takeKeyAsWell ){
  cwal_obase * const b = CWAL_VOBASE(c);
  cwal_engine * const e = CWAL_VENGINE(c);
  assert(c ? !!e : 1);
  if( !e || !b || !key) return NULL;
#if CWAL_OBASE_ISA_HASH
  else if(!b->hprops.list.count) return NULL;
#else
  else if(!b->kvp) return NULL;
#endif
  else if(CWAL_V_IS_VISITING(c)) return NULL;
  else{
    cwal_kvp * left = 0;
    cwal_value * rv = 0;
    cwal_kvp * kvp;
#if CWAL_OBASE_ISA_HASH
    cwal_midsize_t ndx = 0;
    cwal_htable const * pht = &b->hprops;
    kvp = cwal_htable_search_impl_v(NULL, &pht, false, key, &ndx, &left);
    assert(pht == &b->hprops);
#else
    kvp = cwal_kvp_search_v( b->kvp, key, &left );
#endif
    if(!kvp) return NULL;
    rv = cwal_kvp_value(kvp);
    if(left){
      assert(kvp==left->right);
      left->right = kvp->right;
    }else{
#if CWAL_OBASE_ISA_HASH
      /* kvp is only entry at ndx or the left-most entry
         in a hash collision */
      assert(b->hprops.list.list[ndx] == kvp);
      b->hprops.list.list[ndx] = kvp->right;
#else
      assert(b->kvp == kvp && "kvp is the head b->kvp");
      b->kvp = kvp->right /* ? kvp->right : NULL */;
#endif
    }
#if CWAL_OBASE_ISA_HASH
    --b->hprops.list.count;
#endif
    kvp->right = NULL;
    kvp->value = NULL/*steal its ref point*/;
    if(takeKeyAsWell){
      *takeKeyAsWell = kvp->key;
      kvp->key = NULL;
      cwal_value_unhand(*takeKeyAsWell);
    }
    cwal_kvp_free(e, kvp, 1);
    if(!CWAL_MEM_IS_BUILTIN(rv)){
      assert( (CWAL_REFCOUNT(rv) > 0) && "Should still have kvp's reference!" );
      cwal_value_unhand(rv);
    }
    return rv;
  }
}

/**
   "Visits" one member of a key/value pair. If mode is 0 then the key
   is visited, and 1 means to visit the value. Any other value is
   illegal.

   Returns the result of func( pairPair, fState ).
*/
static int cwal_obase_visit_impl( cwal_kvp const *kvp, char mode,
                                  cwal_value_visitor_f func, void * fState  ){
  cwal_value * v = 0;
  int rc;
  switch( mode ){
    case 0:
      v = kvp->key;
      break;
    case 1:
      v = kvp->value;
      assert(0 != v && "i am pretty sure we don't currently allow this.");
      if(!v) return 0;
      break;
    default:
      assert(!"Invalid visit mode.");
      return CWAL_RC_CANNOT_HAPPEN;
  }
  assert(0 != v);
  cwal_value_ref(v);
  rc = func( v, fState );
  cwal_value_unhand(v);
  return rc;
}

bool cwal_props_can( cwal_value const * c ){
  return !!CWAL_VOBASE(c);
}

bool cwal_prop_key_can( cwal_value const * c ){
  switch(c->vtab->typeID){
    case CWAL_TYPE_ARRAY:
    case CWAL_TYPE_BOOL:
    case CWAL_TYPE_DOUBLE:
    case CWAL_TYPE_EXCEPTION:
    case CWAL_TYPE_FUNCTION:
    case CWAL_TYPE_HASH:
    case CWAL_TYPE_INTEGER:
    case CWAL_TYPE_NATIVE:
    case CWAL_TYPE_OBJECT:
    case CWAL_TYPE_STRING:
    case CWAL_TYPE_UNIQUE:
    case CWAL_TYPE_XSTRING:
    case CWAL_TYPE_ZSTRING:
    case CWAL_TYPE_PROPREF:
      return true;
    default:
      return false;
  }
}

int cwal_props_clear( cwal_value * c ){
  cwal_obase * b = CWAL_VOBASE(c);
  cwal_engine * e = CWAL_VENGINE(c);
  /* dump_val(c,"props_clear"); */
  if(!b) return CWAL_RC_TYPE;
  else if(CWAL_CONTAINER_DISALLOW_PROP_SET & b->flags){
    return CWAL_RC_DISALLOW_PROP_SET;
  }else{
    cwal_cleanup_obase( e, b, false );
    return CWAL_RC_OK;
  }
}

static int cwal_kvp_visitor_props_copy( cwal_kvp const * kvp,
                                        void * state ){
  cwal_value * const dest = (cwal_value*)state;
  cwal_obase * const b = CWAL_VOBASE(dest);
  cwal_engine * const e = CWAL_VENGINE(dest);
  if( !e || !b ) return CWAL_RC_MISUSE;
  /* Reminder: we have to keep the property flags intact. */
#if CWAL_OBASE_ISA_HASH
  return cwal_htable_insert_impl_v(dest, &b->hprops, false/*???*/,
                                   kvp->key, kvp->value, true,
                                   kvp->flags, false);
#else
  return cwal_kvp_set_v( e, dest, kvp->key, kvp->value, kvp->flags );
#endif
}

int cwal_props_copy( cwal_value * src, cwal_value * dest ){
  cwal_obase const * const bSrc = CWAL_VOBASE(src);
  cwal_obase const * const bDest = CWAL_VOBASE(dest);
  if(!src || !dest) return CWAL_RC_MISUSE;
  else if(!bSrc || !bDest) return CWAL_RC_TYPE;
  else if(CWAL_V_IS_VISITING(dest)) return CWAL_RC_IS_VISITING;
  /*else if(CWAL_OB_IS_LOCKED(bSrc) || CWAL_OB_IS_LOCKED(bDest)) return CWAL_RC_LOCKED;*/
  return cwal_props_visit_kvp( src, cwal_kvp_visitor_props_copy, dest );
}

bool cwal_props_has_any( cwal_value const * c ){
  cwal_obase const * b = CWAL_VOBASE(c);
#if CWAL_OBASE_ISA_HASH
  return 0 < b->hprops.list.count;
#else
  return b && b->kvp ? 1 : 0;
#endif
}

cwal_midsize_t cwal_props_count( cwal_value const * c ){
  cwal_obase const * b = CWAL_VOBASE(c);
#if CWAL_OBASE_ISA_HASH
  return b->hprops.list.count;
#else
  cwal_size_t rc = 0;
  cwal_kvp const * kvp = b->kvp;
  for( ; kvp ; ++rc, kvp = kvp->right ){}
  return rc;
#endif
}

int cwal_props_visit_values( cwal_value * c, cwal_value_visitor_f f, void * state ){
  cwal_obase * b = CWAL_VOBASE(c);
  if(!c || !f) return CWAL_RC_MISUSE;
  else if(!b) return CWAL_RC_TYPE;
  else {
    int rc = 0;
    int opaque = 0;
    cwal_obase_kvp_iter iter;
    cwal_kvp const * kvp;
    cwal_value_ref(c);
    cwal_visit_props_begin(c, &opaque);
    kvp = cwal_obase_kvp_iter_init(c, &iter);
    for( ; kvp && (0==rc); kvp = cwal_obase_kvp_iter_next(&iter)){
      rc = (CWAL_VAR_F_HIDDEN & kvp->flags)
        ? 0
        : cwal_obase_visit_impl( kvp, 1, f, state );
    }
    cwal_visit_props_end(c, opaque);
    cwal_value_unhand(c);
    return rc;
  }
}

int cwal_props_visit_keys( cwal_value * c, cwal_value_visitor_f f, void * state ){
  cwal_obase * b = CWAL_VOBASE(c);
  if(!c || !f) return CWAL_RC_MISUSE;
  else if(!b) return CWAL_RC_TYPE;
  else{
    int rc = 0;
    int opaque = 0;
    cwal_obase_kvp_iter iter;
    cwal_kvp const * kvp;
    cwal_visit_props_begin(c, &opaque);
    kvp = cwal_obase_kvp_iter_init(c, &iter);
    for( ; kvp && (0==rc); kvp = cwal_obase_kvp_iter_next(&iter)){
      rc = (CWAL_VAR_F_HIDDEN & kvp->flags)
        ? 0
        : cwal_obase_visit_impl( kvp, 0, f, state );
    }
    cwal_visit_props_end(c, opaque);
    return rc;
  }
}

bool cwal_value_may_iterate( cwal_value const * const c ){
  if(!c) return 0;
  else if(CWAL_MEM_IS_BUILTIN(c)){
    return 0;
  }
  else{
#if 0
    cwal_obase const * const ob = CWAL_VOBASE(c);
    /* No... this will disable iteration even when array indexes
       (but not properties) are locked... */
    return ob ? ((CWAL_F_LOCKED & ob->flags) ? 0 : 1) : 0;
#else
    return !!CWAL_VOBASE(c);
#endif
  }
}

bool cwal_value_is_iterating_list( cwal_value const * const c ){
  return c ? CWAL_V_IS_VISITING_LIST(c) : 0;
}

bool cwal_value_is_iterating_props( cwal_value const * const c ){
  return c ? CWAL_V_IS_VISITING(c) : 0;
}

bool cwal_value_may_iterate_list( cwal_value const * const c ){
  if(!c) return 0;
  switch(c->vtab->typeID){
    case CWAL_TYPE_TUPLE:
      return 1 /* nothing can currently lock a tuple */;
    case CWAL_TYPE_ARRAY:
    case CWAL_TYPE_HASH:
      return (CWAL_F_LOCKED & CWAL_VOBASE(c)->flags) ? 0 : 1;
    default:
      return 0;
  }
}

int cwal_props_visit_kvp( cwal_value * c, cwal_kvp_visitor_f f, void * state ){
  cwal_obase * b = CWAL_VOBASE(c);
  if(!c || !f) return CWAL_RC_MISUSE;
  else if(!b) return CWAL_RC_TYPE;
  else {
    int rc = CWAL_RC_OK;
    int opaque = 0;
    cwal_obase_kvp_iter iter;
    cwal_kvp const * kvp;
    cwal_visit_props_begin(c, &opaque);
    kvp = cwal_obase_kvp_iter_init(c, &iter);
    assert( CWAL_RCFLAG_HAS(c, CWAL_RCF_IS_VISITING) );
    cwal_value_ref(c);
    for( ; kvp && (0==rc);
         kvp = cwal_obase_kvp_iter_next(&iter) ){
      if(!(CWAL_VAR_F_HIDDEN & kvp->flags)){
        /* In case the callback does something untowards with
           the kvp, we'll hold a couple new refs... */
        cwal_value * const lhs = kvp->key;
        cwal_value * const rhs = kvp->value;
        cwal_value_ref(lhs);
        cwal_value_ref(rhs);
        rc = f( kvp, state );
        cwal_value_unref(lhs);
        cwal_value_unref(rhs);
      }
    }
    cwal_visit_props_end(c, opaque);
    cwal_value_unhand(c);
    return rc;
  }
}

int cwal_array_visit_v2( cwal_array * const o, bool resolvePropref,
                         cwal_value_visitor_f const f, void * const state ){
  /* Maintenance reminder: identical to cwal_array_visit2_v2() except
     for the callback type. */
  if(!o || !f) return CWAL_RC_MISUSE;
  else if(CWAL_OB_IS_LOCKED(o)) return CWAL_RC_LOCKED;
  else {
    cwal_size_t i;
    cwal_value * v;
    cwal_value * const vSelf = cwal_array_value(o);
    int rc = CWAL_RC_OK;
    cwal_list * const li = &o->list;
    int opaque = 0;
    cwal_visit_list_begin(vSelf, &opaque);
    cwal_value_ref(vSelf);
    for( i = 0;
         (CWAL_RC_OK==rc)
           && (i < li->count);
         ++i ){
      v = (cwal_value*)li->list[i];
      if(v){
        /* to support modification during traversal, we need
           another ref... */
        cwal_value_ref(v);
        if(resolvePropref && CWAL_TYPE_PROPREF==v->vtab->typeID){
          cwal_propref * const p = CWAL__V2PROPREF(v);
          cwal_value * vr = NULL;
          rc = cwal_propref_resolve(p, CWAL_VALPART(o), &vr);
          if(0==rc && vr) cwal_ref(vr);
          cwal_unref(v);
          if(rc) break;
          v = vr;
        }
      }
      rc = f( v, state );
      if(v) cwal_value_unref(v);
    }
    cwal_value_unhand(vSelf);
    cwal_visit_list_end(vSelf, opaque);
    return rc;
  }
}

int cwal_array_visit2_v2( cwal_array * const o, bool resolvePropref,
                          cwal_array_visitor_f const f, void * const state ){
  /* Maintenance reminder: identical to cwal_array_visit_v2() except
     for the callback type. */
  if(!o || !f) return CWAL_RC_MISUSE;
  else if(CWAL_OB_IS_LOCKED(o)) return CWAL_RC_LOCKED;
  else{
    cwal_size_t i;
    cwal_value * v;
    cwal_value * vSelf = cwal_array_value(o);
    int rc = CWAL_RC_OK;
    cwal_list * li = &o->list;
    int opaque = 0;
    cwal_visit_list_begin(vSelf, &opaque);
    cwal_value_ref(vSelf);
    for( i = 0;
         (CWAL_RC_OK==rc)
           && (i < li->count);
         ++i ){
      v = (cwal_value*)li->list[i];
      if(v){
        /* to support modification during traversal, we need
           another ref... */
        cwal_value_ref(v);
        if(resolvePropref && CWAL_TYPE_PROPREF==v->vtab->typeID){
          cwal_propref * const p = CWAL__V2PROPREF(v);
          cwal_value * vr = NULL;
          rc = cwal_propref_resolve(p, CWAL_VALPART(o), &vr);
          if(0==rc && vr) cwal_ref(vr);
          cwal_unref(v);
          if(rc) break;
          v = vr;
        }
      }
      rc = f( o, v, i, state );
      if(v) cwal_value_unref(v);
    }
    cwal_value_unhand(vSelf);
    cwal_visit_list_end(vSelf, opaque);
    return rc;
  }
}

int cwal_array_visit( cwal_array * const a, cwal_value_visitor_f const f, void * const state ){
  return cwal_array_visit_v2(a, false, f, state);
}

int cwal_array_visit2( cwal_array * const a, cwal_array_visitor_f const f, void * const state ){
  return cwal_array_visit2_v2(a, false, f, state);
}


/**
   qsort implementation adapted from:

   http://en.wikibooks.org/wiki/Algorithm_Implementation/Sorting/Quicksort#Iterative_Quicksort

   Based on the "iterative quicksort" C implementation.
   
   li is the cwal_list to sort. cmp is the comparison function to use
   and the state parameter is passed as-is to cmp().

   Returns 0 on success or non-0 if any call to cmp() returns, via
   cmp()'s final parameter, a non-0 code (in which case that code is
   returned).
*/
static int cwal_value_list_sort_impl( cwal_engine * const e,
                                      cwal_list * const li,
                                      cwal_value_stateful_compare_f const cmp,
                                      void * const state ){
  enum { MAX = 64 /* stack size for max 2^(64/2) array elements  */};
  cwal_size_t left = 0, right, pos = 0,
    seed = /*(cwal_size_t)rand()*/
    li->count/2
    /* reminder to self (20180620): using a random pivot requires
       that we seed the RNG (call srand()), and it seems a bit
       rude for the cwal core to do so, as the best we can do for
       a seed is to call time(). Checkin f40e1afaede9a6fd (which
       removed an srand() call in s2) uncovered an error
       propagation problem when we use an unseeded random
       pivot. Funnily enough, the checkin comment said:

       <<<Added (integer prototype).srand() and rand() no longer
       automatically calls srand() the first time rand() is
       called. It seems that rand() was never documented, so this
       should not break anything.>>>

       Haha. Running the s2 unit tests before checkin would have
       revealed this problem then, rather than a month later
       :|. */,
    stack[MAX];
  cwal_size_t len = li->count;
  int errc = 0, cmprc;
  void * pivot = 0;
  void * tmp;
  assert(len > 1 && "this is never called for an empty- or length-1 array");
#define CV (cwal_value*)
  for( ;; ){
    for (; left+1 < len; ++len) {              /* sort left to len-1 */
      if (pos == MAX) len = stack[pos = 0];  /* stack overflow, reset */
      pivot = li->list[left+seed%(len-left)];/* pick [possibly random] pivot */
      seed = seed*69069+1;                   /* next pseudorandom number */
      stack[pos++] = len;                    /* sort right part later */
      for(right = left-1; ; ) {              /* inner loop: partitioning */
        while(1){
          /* look for greater element */
          cmprc = cmp( CV li->list[++right], CV pivot, state, &errc );
          if(errc) return errc;
          else if(e->err.code) return e->err.code;
          else if(cmprc>=0) break;
        }
        while(1){
          /* look for smaller element */
          cmprc = cmp( CV pivot, CV li->list[--len], state, &errc );
          if(errc) return errc;
          else if(e->err.code) return e->err.code;
          else if(cmprc>=0) break;
        }
        if(right >= len) break;           /* partition point found? */
        tmp = li->list[right];
        li->list[right] = li->list[len]; /* the only swap */
        li->list[len] = tmp;
      } /* partitioned, continue left part */
    }
    if (pos == 0) break;     /* stack empty? */
    left = len;              /* left to right is sorted */
    len = stack[--pos];      /* get next range to sort */
  }
#undef CV
  assert(!errc);
  if(e){/*avoid unused param warning*/}
  return 0;
}

/** Short-lived code consolidation for the cwal_array_sort
    routines. */
#define cwal__array_sort_prologue(AR) \
  if(CWAL_V_IS_VISITING_LIST(CWAL_VALPART(AR))) return CWAL_RC_IS_VISITING_LIST; \
  else if(CWAL_OB_IS_LOCKED(AR)) return CWAL_RC_LOCKED; \
  else if(AR->list.count<2) return 0

int cwal_array_sort_stateful( cwal_array * const ar,
                              cwal_value_stateful_compare_f cmp,
                              void * state ){
  cwal_value * const arv = CWAL_VALPART(ar);
  if(!ar || !cmp || !arv) return CWAL_RC_MISUSE;
  else cwal__array_sort_prologue(ar);
  else {
    cwal_engine * e;
    int rc = 0;
    int opaque = 0;
    cwal_visit_list_begin(arv, &opaque);
    assert(arv->scope);
    e = arv->scope->e;
    CWAL_OB_LOCK(ar);
    assert(CWAL_OB_IS_LOCKED(ar));
    rc = cwal_value_list_sort_impl( e, &ar->list, cmp, state );
    CWAL_OB_UNLOCK(ar);
    assert(!CWAL_OB_IS_LOCKED(ar));
    cwal_visit_list_end(arv, opaque);
    return rc ? rc : (e->values.exception ? CWAL_RC_EXCEPTION : 0);
  }
}

/**
   Internal state for cwal_array_sort_func() and
   cwal_array_sort_func_cb().
*/
struct ArrayFuncState {
  cwal_function * f;
  cwal_value * self;
  bool resolvePropref;
};
typedef struct ArrayFuncState ArrayFuncState;

static int cwal__array_sort_func( cwal_value * lhs, cwal_value * rhs, void * state,
                                  int * errCode ){
  ArrayFuncState * const st = (ArrayFuncState *)state;
  cwal_value * rv = 0;
  int rc = 0;
  cwal_value * rL, * rR /* propref-resolved values */;
  short toggleVacOff = 0 /* mask: 0x01=toggle vacuum-safe off for rL,
                            0x02=toggle off for rR */;
  if(st->resolvePropref){
    for(int i = 0; i < 2; ++i){
      /* Check lhs/rhs for propref resolution... */
      cwal_value * const arg = i ? rhs : lhs;
      cwal_value ** const pArg = i ? &rR : &rL;
      if(arg && CWAL_TYPE_PROPREF==arg->vtab->typeID){
        *pArg = NULL;
        rc = cwal_propref_resolve(CWAL__V2PROPREF(arg), arg, pArg);
        if(rc){
          assert(!rR);
          if(lhs == rL) cwal_unhand(rL);
          else cwal_unref(rL);
          *errCode = rc;
          return 0;
        }
      }else{
        *pArg = arg;
      }
      if(*pArg){
        cwal_ref(*pArg);
        if(!CWAL_V_IS_VACUUM_SAFE(*pArg)){
          toggleVacOff |= 1 << i;
          cwal_value_make_vacuum_proof(*pArg, true);
        }
      }else{
        *pArg = cwal_value_undefined();
      }
    }
  }else{
    rL = lhs; rR = rhs;
  }
  cwal_value * argv[2] = {NULL, NULL};
  argv[0] = rL ? rL : cwal_value_undefined();
  argv[1] = rR ? rR : cwal_value_undefined();
  *errCode = cwal_function_call(st->f, st->self, &rv, 2, argv);
  if(0x01 & toggleVacOff) cwal_value_make_vacuum_proof(rL, false);
  if(0x02 & toggleVacOff) cwal_value_make_vacuum_proof(rR, false);
  if(0==*errCode && rv){
    /* We have to resolve this using doubles, not integers. See
       https://github.com/ccxvii/mujs/issues/122 for why. */
    cwal_double_t const d = cwal_value_get_double(rv);
    rc = (0.0==d) ? 0 : ((d<0) ? -1 : 1);
    /* An alternate, more clever, formulation (via the above
       ticket link), but i find it less readable:
       rc = (d > 0) - (d < 0) */
    /* There's a corner-case bug here, though: if rv is an
       integer value larger than cwal_double_t can represent, the
       results will very possibly be wrong. */           
  }
  if(st->resolvePropref){
    if(lhs == rL) cwal_unhand(rL);
    else cwal_unref(rL);
    if(rhs == rR) cwal_unhand(rR);
    else cwal_unref(rR);
  }
  cwal_refunref(rv);
  return rc;
}

int cwal_array_sort_func( cwal_array * const ar, cwal_value * const self,
                          cwal_function * const cmp, bool resolvePropref){
  if(!ar || !cmp) return CWAL_RC_MISUSE;
  else cwal__array_sort_prologue(ar);
  else {
    ArrayFuncState st;
    st.f = cmp;
    st.self = self ? self : cwal_function_value(cmp);
    st.resolvePropref = resolvePropref;
    return cwal_array_sort_stateful( ar, cwal__array_sort_func, &st );
  }
}

int cwal_array_stateful_cmp_v2( cwal_value * lhs, cwal_value * rhs, void * state,
                                int * errCode ){
  bool const resolvePropref = !!state;
  return cwal_value_compare_v2(lhs, rhs, resolvePropref, errCode);
}

int cwal_array_sort_v2( cwal_array * ar, bool resolvePropref ){
  if(!ar) return CWAL_RC_MISUSE;
  else cwal__array_sort_prologue(ar);
  else return cwal_array_sort_stateful( ar, cwal_array_stateful_cmp_v2,
                                        resolvePropref ? &resolvePropref : NULL );
}

int cwal_array_sort( cwal_array * ar, int(*cmp)(void const *, void const *) ){
  if(!ar || !cmp) return CWAL_RC_MISUSE;
  else cwal__array_sort_prologue(ar);
  else {
    cwal_value * arv = CWAL_VALPART(ar);
    int opaque = 0;
    cwal_visit_list_begin(arv, &opaque);
    /* We don't know what cmp may do, thus we need to flag the visiting-related bits. */
    CWAL_OB_LOCK(ar);
    qsort( ar->list.list, ar->list.count, sizeof(void*), cmp );
    CWAL_OB_UNLOCK(ar);
    cwal_visit_list_end(arv, opaque);
    return 0;
  }
}
#undef cwal__array_sort_prologue

int cwal_array_reverse( cwal_array * ar ){
  if(!ar) return CWAL_RC_MISUSE;
#if 0
  /* reversal is just a special case of get/set, so there seems to
     be little harm in disallowing it during list
     traversal. (Sorting is kinda also just a special case of
     get/set, but it's a lot more involved.) */
  else if(CWAL_V_IS_VISITING_LIST(CWAL_VALPART(ar))) return CWAL_RC_IS_VISITING_LIST;
#endif
  else if(CWAL_OB_IS_LOCKED(ar)) return CWAL_RC_LOCKED;
  else if(ar->list.count < 2) return 0;
  else{
    cwal_size_t b = 0, e = ar->list.count-1;
    void ** li = ar->list.list;
    void * tmp;
    for( ; b<e; ++b, --e ){
      tmp = li[b];
      li[b] = li[e];
      li[e] = tmp;
    }
    return 0;
  }
}

int cwal_compare_value_void( void const * lhs, void const * rhs ){
  return cwal_value_compare( *((cwal_value const**)lhs),
                             *((cwal_value const**)rhs) );
}
int cwal_compare_value_reverse_void( void const * lhs, void const * rhs ){
  int const rc = cwal_value_compare( *((cwal_value const**)lhs),
                                     *((cwal_value const**)rhs) );
  return rc ? -rc : 0;
}

int cwal_int_to_cstr( cwal_int_t iVal, char * dest, cwal_size_t * nDest ){
  enum { BufLen = CWAL_SIZE_T_BITS * 2 };
  /* int isNegative = 0; */
  char zBuf[BufLen];
  char *z = zBuf;
  cwal_size_t zLen;
  if( !dest || !nDest ) return CWAL_RC_MISUSE;
#if 1
  zLen = sprintf(z, "%"CWAL_INT_T_PFMT, iVal)
    /* Portability problem with 64-bit CWAL_SIZE_T_BITS on
       32-bit platforms: format specifier not portable. */
    ;
  assert(zLen>0);
  if( *nDest < zLen ) return CWAL_RC_RANGE;
  *nDest = (cwal_size_t)zLen;
  memcpy( dest, z, zLen + 1/*NUL byte*/ );
  return 0;
#else
  /*Implementation taken from th1 sources. Breaks on INT_MIN,
    resulting in garbage. */
  if( iVal<0 ){
    isNegative = 1;
    iVal = iVal * -1;
  }
  *(--z) = '\0';
  *(--z) = (char)(48+(iVal%10));
  while( (iVal = (iVal/10))>0 ){
    *(--z) = (char)(48+(iVal%10));
    assert(z>zBuf);
  }
  if( isNegative ){
    *(--z) = '-';
  }
  zLen = zBuf+BufLen-z - 1/*NUL byte*/;
  if( *nDest <= zLen ) return CWAL_RC_RANGE;
  else{
    *nDest = zLen;
    memcpy( dest, z, zLen + 1/*NUL byte*/ );
    MARKER("int %"CWAL_INT_T_PFMT"==>string: <<<%s>>>\n", iVal, dest );
    return CWAL_RC_OK;
  }
#endif   
}

#if 0 /* cwal_double_to_cstr() ... */
/* This conversion is easy to implement but fails some comparison
   tests in cwal's test.c because 42.242 converts to 42.2419999... */
int cwal_double_to_cstr( cwal_double_t fVal, char * dest, cwal_size_t * nDest ){
  enum { BufLen = 256 };
  char zBuf[BufLen];   /* Output buffer */
  int n;
  n = sprintf(zBuf, "%.17f", fVal);
  if(n<1) return CWAL_RC_ERROR;
  if(1==n){/*kludge!*/
    zBuf[1] = '.';
    zBuf[2] = '0';
    n += 2;
  }
  zBuf[n] = 0;
  MARKER(("double zBuf=%s\n", zBuf));
  if(*nDest <= (cwal_size_t)n) return CWAL_RC_RANGE;
  *nDest = (cwal_size_t)n;
  memcpy(dest, zBuf, (size_t)n);
  return 0;
}
#else
/**
   Internal helper to proxy double-to-string conversions through
   cwal_printf() (because it has far better precision handling
   than i know how to implement properly).
*/
typedef struct FixedBufferAppender {
  char const * begin;
  char const * end;
  char * pos;
  char const * dotPos;
  int rc;
} FixedBufferAppender;
/** Internal cwal_printfv_appender() impl to append data to
    a FixedBufferAppender.
*/
static int cwal_printfv_appender_double( void * arg, char const * data,
                                         unsigned n ){
  unsigned i;
  FixedBufferAppender * ba = (FixedBufferAppender*)arg;
  for( i = 0; i < n; ++i ){
    if(ba->pos==ba->end){
      return ba->rc = CWAL_RC_RANGE;
    }
    if('.' == (*ba->pos = data[i])) ba->dotPos = ba->pos;
    else if('e' == data[i] || 'E' == data[i]){
      assert(!"We're expecting no exponent notation!");
      return ba->rc = CWAL_RC_CANNOT_HAPPEN;
    }
    ++ba->pos;
    assert(ba->pos<ba->end);
  }
  return 0;
}

int cwal_double_to_cstr( cwal_double_t fVal, char * dest, cwal_size_t * nDest ){
  enum { BufLen = 120 };
  char zBuf[BufLen] = {0,0,0,0,0,0};
  FixedBufferAppender dba;
  cwal_size_t dLen;
  dba.begin = dba.pos = zBuf;
  dba.end = zBuf + BufLen;
  dba.dotPos = 0;
  dba.rc = 0;
  cwal_printf( cwal_printfv_appender_double, &dba, "%lf", fVal );
  if(!dba.rc){
    if('N'==*zBuf || 'I'==*zBuf || 'I'==zBuf[1]){
      /* Assume NaN/[+-]Inf. */
    }else{
      assert(dba.dotPos
             && "We're expecting the output to _always_ contain a decimal point!");
      if(!dba.dotPos) return CWAL_RC_CANNOT_HAPPEN;
    }
    *dba.pos = 0;
    dLen = (cwal_size_t)(dba.pos - dba.begin);
    --dba.pos;
    /* The obligatory kludge: trim zeroes... */
    for( ; dba.dotPos && dba.pos > dba.dotPos+1
           && '0'==*dba.pos
           /*&& '.' != *(dba.pos-1)*/; --dba.pos){
      *dba.pos = 0;
      --dLen;
    }
    if(*nDest<=dLen+1/*NUL*/) return CWAL_RC_RANGE;
    *nDest = dLen;
    memcpy(dest, zBuf, dLen+1/*NUL*/);
  }
  return dba.rc;
}
#endif /* cwal_double_to_cstr() */

int cwal_cstr_to_int( char const * cstr, cwal_size_t slen,
                      cwal_int_t * dest ){
  cwal_int_t v = 0;
  cwal_int_t oflow = 0;
  char const * p = cstr+slen-1;
  cwal_int_t mult = 0;
  int digitCount = 0;
  if(!cstr || !slen || !*cstr) return CWAL_RC_MISUSE;
  for( ; p >= cstr;
       --p, oflow = v ){
    if( (*p<'0') || (*p>'9') ) {
      if(cstr == p){
        if('-'==*p){
          v = -v;
          break;
        }
        else if('+'==*p){
          break;
        }
      }
      return CWAL_RC_TYPE;
    }
    ++digitCount;
    v += mult
      ? (mult*(*p-'0'))
      : (*p-'0');
    if(v < oflow) return CWAL_RC_RANGE;
    mult = mult ? (mult*10) : 10;
  }
  if(!digitCount) return CWAL_RC_TYPE;
  else if(dest) *dest = v;
  return 0;
}

int cwal_string_to_int( cwal_string const * s, cwal_int_t * dest ){
  return s
    ? cwal_cstr_to_int( cwal_string_cstr(s), CWAL_STRLEN(s), dest )
    : CWAL_RC_MISUSE;
}

int cwal_cstr_to_double( char const * cstr, cwal_size_t slen,
                         cwal_double_t * dest ){
  int rc;
  cwal_int_t rhs = 0;
  cwal_int_t lhs = 0;
  cwal_double_t rmult = 0.0;
  int sign = 1;
  if(!cstr || !slen || !*cstr) return CWAL_RC_MISUSE;
  switch(*cstr){
    case '-': sign = -1;
      CWAL_SWITCH_FALL_THROUGH;
    case '+': ++cstr; --slen; break;
  }
  char const * p = cstr+slen-1;
  for( ; (p > cstr) && ('.' != *p); --p ){
    rmult = rmult ? (10*rmult) : 10;
  }
  if(p==cstr){
    /* Maybe it's an integer */
    if('.'==*p) return CWAL_RC_TYPE /* .1234 */;
    else {
      /* Try it as an integer */
      rc = cwal_cstr_to_int( p, slen, &lhs );
      if(!rc && dest) *dest = (cwal_double_t)(lhs * sign);
      return rc;
    }
  }
    
  assert('.' == *p);
  rc  = cwal_cstr_to_int( p+1, cstr+slen-p-1, &rhs );
  if(rc) return rc;
  else if((p>cstr) && (rhs<0)) return CWAL_RC_TYPE /* 123.-456 */;

  rc = cwal_cstr_to_int( cstr, p - cstr, &lhs );
  if(!rc && dest){
    if(lhs>=0){
      *dest = ((cwal_double_t)lhs
        + (rmult
           ? (((cwal_double_t)rhs) / rmult)
           : 0)) * sign;
    }else{
      *dest = ((cwal_double_t)lhs
        - (rmult
           ? (((cwal_double_t)rhs) / rmult)
           : 0)) * sign;
    }
  }
  return rc;
}

int cwal_string_to_double( cwal_string const * s, cwal_double_t * dest ){
  return s
    ? cwal_cstr_to_double( cwal_string_cstr(s), CWAL_STRLEN(s), dest )
    : CWAL_RC_MISUSE;
}

void cwal_hash_clear( cwal_hash * h, bool freeProps ){
  cwal_scope * const sc = h ? CWAL_VALPART(h)->scope : 0;
  cwal_engine * const e = sc ? sc->e : 0;
  assert(e && "Else invalid cwal_hash reference.");
  cwal_cleanup_htable(e, &h->htable, false);
  if(freeProps){
    cwal_cleanup_obase(e, &h->base, false);
  }
}

void cwal_value_cleanup_hash( cwal_engine * e, void * V ){
  cwal_value * const vSelf = (cwal_value *)V;
  cwal_hash * const h = CWAL_HASH(vSelf);
  /* MARKER("Freeing hash @%p\n", (void const *)h); */
  cwal_cleanup_htable(e, &h->htable, true);
  cwal_cleanup_obase(e, &h->base, true);
  *h = cwal_hash_empty;
}

int cwal_rescope_children_hash( cwal_value * v ){
  cwal_hash * h = CWAL_HASH(v);
  cwal_scope * sc = v->scope;
  assert(sc && h && v);
  assert(CWAL_V_IS_RESCOPING(v));
  cwal_rescope_children_obase(v);
  /* cwal_dump_v(nv,"Re-scoping hashtable children..."); */
  cwal_htable_rescope(sc, &h->htable);
  return 0;
}

cwal_value * cwal_new_hash_value( cwal_engine * e, cwal_size_t hashSize){
  if(!e || !hashSize) return NULL;
  else {
    cwal_value * v = cwal_value_new(e, CWAL_TYPE_HASH, 0);
    if(v){
      cwal_hash * h = CWAL_HASH(v);
      cwal_value * proto = h->base.prototype;
      assert(v->scope);
      *h = cwal_hash_empty;
      h->base.prototype = proto;
      if( 0 != cwal_hash_resize(h, hashSize) ){
        cwal_value_unref(v);
        v = 0;
      }else{
        assert(h->htable.list.alloced >= hashSize);
        assert(!h->htable.list.count);
        assert(hashSize == h->htable.hashSize);
      }
    }
    return v;
  }
}

cwal_hash * cwal_value_get_hash( cwal_value * v ){
  return CWAL_HASH(v);
}

cwal_hash * cwal_new_hash( cwal_engine * e, cwal_size_t hashSize ){
  cwal_value * v = cwal_new_hash_value(e, hashSize);
  return v ? CWAL_HASH(v) : NULL;
}

cwal_value * cwal_hash_value( cwal_hash * h ){
  return CWAL_VALPART(h);
}

cwal_kvp * cwal_hash_search_kvp( cwal_hash * h, char const * key,
                                 cwal_midsize_t keyLen ){
  cwal_htable const * pht = &h->htable;
  return cwal_htable_search_impl_cstr(NULL, &pht, false, key, keyLen,
                                      NULL, NULL);
}

cwal_value * cwal_hash_search( cwal_hash * h, char const * key,
                               cwal_midsize_t keyLen ){
  cwal_htable const * pht = &h->htable;
  cwal_kvp const * const kvp =
    cwal_htable_search_impl_cstr(NULL, &pht, false, key, keyLen,
                                 NULL, NULL);
  return kvp ? kvp->value : 0;
}
    
cwal_value * cwal_hash_search_v( cwal_hash * h, cwal_value const * key ){
  cwal_htable const * pht = &h->htable;
  cwal_kvp const * const kvp =
    cwal_htable_search_impl_v(NULL, &pht, false, key, NULL, NULL);
  return kvp ? kvp->value : NULL;
}

cwal_kvp * cwal_hash_search_kvp_v( cwal_hash * h, cwal_value const * key ){
  cwal_htable const * pht = &h->htable;
  return cwal_htable_search_impl_v(NULL, &pht, false, key, NULL, NULL);
}

int cwal_hash_insert_with_flags_v( cwal_hash * h, cwal_value * key, cwal_value * v,
                                   bool allowOverwrite, cwal_flags16_t kvpFlags ){
  cwal_value * const hv = CWAL_VALPART(h);
  if(!cwal_prop_key_can(key)) return CWAL_RC_TYPE;
  else if(CWAL_V_IS_IN_CLEANUP(hv)) return CWAL_RC_DESTRUCTION_RUNNING;
  else if(CWAL_V_IS_VISITING_LIST(hv)) return CWAL_RC_IS_VISITING_LIST;
  return cwal_htable_insert_impl_v(hv, &h->htable, false, key, v,
                                   allowOverwrite, kvpFlags, false);
}

int cwal_hash_insert_v( cwal_hash * h, cwal_value * key, cwal_value * v,
                        bool allowOverwrite ){
  return cwal_hash_insert_with_flags_v(h, key, v, allowOverwrite,
                                       CWAL_VAR_F_PRESERVE);
}

int cwal_hash_insert_with_flags( cwal_hash * h, char const * key, cwal_midsize_t keyLen,
                                 cwal_value * v, bool allowOverwrite,
                                 cwal_flags16_t kvpFlags ){
  cwal_value * const hv = CWAL_VALPART(h);
  cwal_engine * const e = hv->scope->e;
  cwal_value * const vKey = cwal_new_string_value(e, key, keyLen);
  if(!vKey) return CWAL_RC_OOM;
  int rc;
  cwal_value_ref(vKey);
  rc = cwal_hash_insert_with_flags_v(h, vKey, v,
                                     allowOverwrite, kvpFlags);
  cwal_value_unref(vKey);
  return rc;
}

int cwal_hash_insert( cwal_hash * h, char const * key, cwal_midsize_t keyLen,
                      cwal_value * v, bool allowOverwrite ){
  return cwal_hash_insert_with_flags( h, key, keyLen, v, allowOverwrite, CWAL_VAR_F_PRESERVE );
}

int cwal_hash_remove_v( cwal_hash * h, cwal_value * key ){
  cwal_value * const vSelf = CWAL_VALPART(h);
  if(!h || !vSelf || !key) return CWAL_RC_MISUSE;
  else if(CWAL_RCFLAG_HAS(vSelf,CWAL_RCF_IS_DESTRUCTING)) return CWAL_RC_DESTRUCTION_RUNNING;
  //else if(CWAL_CONTAINER_DISALLOW_PROP_SET & h->base.flags) return CWAL_RC_DISALLOW_PROP_SET;
  else if(CWAL_V_IS_VISITING_LIST(vSelf)) return CWAL_RC_IS_VISITING_LIST;
  return cwal_htable_remove_impl_v(vSelf, &h->htable, key);
}

int cwal_hash_remove( cwal_hash * h, char const * key, cwal_midsize_t keyLen ){
  cwal_value * const vSelf = CWAL_VALPART(h);
  if(!h || !vSelf || !key) return CWAL_RC_MISUSE;
  else if(CWAL_RCFLAG_HAS(vSelf,CWAL_RCF_IS_DESTRUCTING)) return CWAL_RC_DESTRUCTION_RUNNING;
  //else if(CWAL_CONTAINER_DISALLOW_PROP_SET & h->base.flags) return CWAL_RC_DISALLOW_PROP_SET;
  else if(CWAL_V_IS_VISITING_LIST(vSelf)) return CWAL_RC_IS_VISITING_LIST;
  return cwal_htable_remove_impl_cstr(vSelf, &h->htable, key, keyLen);
}

cwal_midsize_t cwal_hash_entry_count(cwal_hash const *h){
  return h->htable.list.count;
}

cwal_midsize_t cwal_hash_size( cwal_hash const * h ){
  return h->htable.hashSize;
}

int cwal_hash_visit_kvp( cwal_hash * h, cwal_kvp_visitor_f f, void * state ){
  cwal_value * const hv = (h && f) ? CWAL_VALPART(h) : NULL;
  if(!hv || !f) return CWAL_RC_MISUSE;
  else {
    cwal_kvp * kvp;
    cwal_size_t i;
    int rc = CWAL_RC_OK;
    int opaque = 0;
    cwal_visit_list_begin(hv, &opaque);
    for( i = 0; !rc && (i < h->htable.hashSize); ++i ){
      kvp = (cwal_kvp*)h->htable.list.list[i];
      if(!kvp) continue;
      else{
        cwal_kvp * next = 0;
        for( ; !rc && kvp; kvp = next ){
          next = kvp->right;
          rc = (CWAL_VAR_F_HIDDEN & kvp->flags)
            ? 0
            : f( kvp, state );
        }
      }
    }
    cwal_visit_list_end(hv, opaque);
    return rc;
  }
}
/**
   mode: 1==visit the keys, 0==the values.
*/
static int cwal_hash_visit_value_impl( cwal_hash * h, char mode,
                                       cwal_value_visitor_f f, void * state ){
  cwal_value * hv = (h && f) ? CWAL_VALPART(h) : NULL;
  cwal_obase * b = CWAL_VOBASE(hv);
  if(!hv || !f) return CWAL_RC_MISUSE;
  else if(!b) return CWAL_RC_TYPE;
  else {
    cwal_kvp * kvp;
    cwal_size_t i;
    int rc = CWAL_RC_OK;
    int opaque = 0;
    cwal_visit_list_begin(hv, &opaque);
    for( i = 0; !rc && (i < h->htable.hashSize); ++i ){
      kvp = (cwal_kvp*)h->htable.list.list[i];
      if(!kvp) continue;
      else{
        cwal_kvp * next = 0;
        for( ; !rc && kvp; kvp = next ){
          next = kvp->right;
          rc = (CWAL_VAR_F_HIDDEN & kvp->flags)
            ? 0
            : f( mode ? kvp->value : kvp->key,
                 state );
        }
      }
    }
    cwal_visit_list_end(hv, opaque);
    return rc;
  }
}

int cwal_hash_visit_keys( cwal_hash * h, cwal_value_visitor_f f,
                          void * state ){
  return cwal_hash_visit_value_impl(h, 0, f, state);
}

int cwal_hash_visit_values( cwal_hash * h, cwal_value_visitor_f f,
                            void * state ){
  return cwal_hash_visit_value_impl(h, 1, f, state);
}

int cwal_hash_resize( cwal_hash * h, cwal_size_t newSize ){
  cwal_value * const hv = CWAL_VALPART(h);
  if(!h || !hv) return CWAL_RC_MISUSE;
  else if(CWAL_V_IS_VISITING_LIST(hv)) return CWAL_RC_IS_VISITING_LIST;
  /* highly arguable: newSize = cwal_trim_hash_size( newSize ); */
  return cwal_htable_resize(hv, &h->htable, newSize);
}

cwal_midsize_t cwal_next_prime( cwal_midsize_t n ){
#if 1
  int const * p = cwal_first_1000_primes();
  int const last = 999;
  int i = 0;
  if((int)n >= p[last]) return (cwal_hash_t)p[last];
  for( ; i < last; ++i){
    if(p[i] > (int)n) return (cwal_hash_t)p[i];
  }
  return p[i];
#else
  /**
     This list was more or less arbitrarily chosen by starting at
     some relatively small prime and roughly doubling it for each
     increment, then filling out some numbers in the middle.
  */
  static const cwal_hash_t list[] = {
  5, 7, 11, 17, 19, 27, 41, 59, 71, 97, 109, 137,
  167, 199, 239, 373, 457, 613, 983, 1319, 2617,
  5801, 7919, 9973, 14563, 20011,
  30241, 40637,
#if CWAL_INT_T_BITS <= 16
  /* max for 16-bit */
  49999,
#else
  60913, 80897,
  104729, 160969,
#endif
  0/*sentinel*/};
  cwal_hash_t const * i;
  for( i = list; *i && *i<n; ++i );
#if CWAL_INT_T_BITS == 16
  return *i ? *i : 49999;
#else
  return *i ? *i : 160969;
#endif
#endif
}

int cwal_hash_grow_if_loaded( cwal_hash * h, double load ){
  cwal_value * const hv = CWAL_VALPART(h);
  if(CWAL_V_IS_VISITING_LIST(hv)) return CWAL_RC_IS_VISITING_LIST;
  return cwal_htable_grow_if_loaded(hv, &h->htable, load);
}

int cwal_hash_take_props( cwal_hash * const h, cwal_value * const src,
                          int overwritePolicy ){
  cwal_obase * srcBase;
  cwal_value * const hv = CWAL_VALPART(h);
  if(!src || !h || !hv) return CWAL_RC_MISUSE;
  else if(CWAL_V_IS_VISITING_LIST(hv)) return CWAL_RC_IS_VISITING_LIST;
  else if(CWAL_V_IS_VISITING(src)) return CWAL_RC_IS_VISITING;
  else if(!(srcBase = CWAL_VOBASE(src))) return CWAL_RC_TYPE;
#if CWAL_OBASE_ISA_HASH
  /** FIXME: what follows is a relatively inefficient implementation
      compared to the !CWAL_OBASE_ISA_HASH variant in the following
      #else block.  It would seem that implementing an equivalent
      no-alloc impl for the CWAL_OBASE_ISA_HASH case is far more
      trouble (or else i'm simply getting old and it just *feels* like
      far more trouble). */
  int rc = 0;
  cwal_obase_kvp_iter iter;
  cwal_kvp const * kvp;
  cwal_array * keysToRm = 0;
  int opaque = 0;
  cwal_visit_props_begin(src, &opaque);
  if(overwritePolicy<0) overwritePolicy=-1 /* keep existing props */;
  else if(overwritePolicy>0) overwritePolicy=1 /* overwrite */;
  else overwritePolicy=0 /* error on collision */;
  assert(overwritePolicy>=-1 && overwritePolicy<=1);
  kvp = cwal_obase_kvp_iter_init(src, &iter);
  cwal_htable const * ht = &h->htable;
  for( ; kvp && (0==rc); kvp = cwal_obase_kvp_iter_next(&iter) ){
    cwal_kvp * const hKvp =
      cwal_htable_search_impl_v(NULL, &ht, false, kvp->key, NULL, NULL);
    assert(ht == &h->htable);
    assert(&h->htable == ht);
    while(hKvp){
      switch(overwritePolicy){
        case -1: /* Keep existing keys */
          kvp = 0;
          break;
        case 0: /* Collision == error */
          rc = CWAL_RC_ALREADY_EXISTS;
          kvp = 0;
          break;
        case 1: /* Overwrite... */
          break;
        default:
          cwal__fatal(CWAL_RC_ASSERT, "Cannot happen: "
                      "unhandled overwritePolicy mapping.");
          break /*not reached*/;
      }
      break;
    }
    if(!kvp) continue;
    else if(!keysToRm){
      keysToRm = cwal_new_array(CWAL_VENGINE(hv));
      if(!keysToRm){
        rc = CWAL_RC_OOM;
        break;
      }
    }
    rc = cwal_htable_insert_impl_v(hv, &h->htable, false,
                                   kvp->key, kvp->value,
                                   true, CWAL_VAR_F_PRESERVE, false);
    if(!rc) rc = cwal_array_append(keysToRm, kvp->key);
  }
  cwal_visit_props_end(src, opaque);
  if(!rc && keysToRm){
    cwal_midsize_t const nRm = cwal_array_length_get(keysToRm);
    for(cwal_midsize_t i = 0; i < nRm; ++i){
      cwal_value * const k = cwal_array_get(keysToRm, i);
      cwal_prop_unset_v(src, k);
    }
  }
  cwal_value_unref(CWAL_VALPART(keysToRm));
  return rc;
#else
  cwal_kvp * kvp;
  cwal_kvp * keepThese = 0;
  cwal_kvp * toKeepTail = 0;
  int rc = 0;
  cwal_engine * const e = hv->scope->e;
  /*nope - will break following set ops: cwal_value_set_visiting_list(hv, 1);*/
  while( (kvp = srcBase->kvp) ){
    cwal_value * k = kvp->key;
    cwal_value * v = kvp->value;
    assert(CWAL_REFCOUNT(k) || CWAL_MEM_IS_BUILTIN(k));
    assert(CWAL_REFCOUNT(v) || CWAL_MEM_IS_BUILTIN(v));
    srcBase->kvp = kvp->right;
    *kvp = cwal_kvp_empty;
    e->values.hashXfer = kvp;
    rc = cwal_hash_insert_v( h, k, v, overwritePolicy>0 ? 1 : 0 );
    switch(rc){
      case 0:
        if(e->values.hashXfer){
          assert(0!=overwritePolicy);
          cwal_kvp_free(e, e->values.hashXfer, 1);
          e->values.hashXfer = 0;
        }else{
          /* e->values.hashXfer was taken by insert() */
          assert(CWAL_REFCOUNT(k)>1 || CWAL_MEM_IS_BUILTIN(k));
          assert(CWAL_REFCOUNT(v)>1 || CWAL_MEM_IS_BUILTIN(v));
          assert(kvp->key == k);
          assert(kvp->value == v);
        }
        /* account for src container refs */
        cwal_value_unref(k);
        cwal_value_unref(v);
        continue;
      case CWAL_RC_ALREADY_EXISTS:
        assert(kvp == e->values.hashXfer);
        e->values.hashXfer = 0;
        assert(overwritePolicy<=0);
        if(overwritePolicy<0){
          rc = 0;
          assert(!kvp->right);
          assert(!kvp->key);
          assert(!kvp->value);
          kvp->right = keepThese;
          kvp->key = k;
          kvp->value = v;
          keepThese = kvp;
          if(!toKeepTail) toKeepTail = keepThese;
          continue;
        }else{
          goto fixkvp;
        }
      default:
        assert(kvp == e->values.hashXfer);
        e->values.hashXfer = 0;
        goto fixkvp;
    }
    assert(!"not reached");
    fixkvp:
    assert(CWAL_REFCOUNT(k) || CWAL_MEM_IS_BUILTIN(k));
    assert(CWAL_REFCOUNT(v) || CWAL_MEM_IS_BUILTIN(v));
    assert(0!=rc);
    /* Put kvp back in srcBase */
    assert(!kvp->right);
    kvp->right = srcBase->kvp;
    srcBase->kvp = kvp;
    kvp->key = k;
    kvp->value = v;
    break;
  }
  if(toKeepTail){
    assert(keepThese);
    assert(toKeepTail->key);
    assert(toKeepTail->value);
    assert(CWAL_REFCOUNT(toKeepTail->key) || CWAL_MEM_IS_BUILTIN(toKeepTail->key));
    assert(CWAL_REFCOUNT(toKeepTail->value) || CWAL_MEM_IS_BUILTIN(toKeepTail->value));
    toKeepTail->right = srcBase->kvp;
    srcBase->kvp = keepThese;
  }
  /* nope cwal_value_set_visiting_list(hv, 0); */
  return rc;
#endif
}

    
void cwal_value_cleanup_native( cwal_engine * e, void * V ){
  cwal_value * v = (cwal_value *)V;
  cwal_native * n = v ? CWAL_V2NATIVE(v) : 0;
  assert(v && n);
  if(n->finalize){
    n->finalize( e, n->native );
    n->finalize = NULL;
    n->native = NULL;
  }
  cwal_cleanup_obase( e, &n->base, true )
    /* Do this first in case any properties use the
       native data. No... do it last in case
       any of the properties are used in the finalizer.
       i broke linenoiseish's auto-save-at-finalize
       when i swapped this. */;
  *n = cwal_native_empty;
}

cwal_value * cwal_new_native_value( cwal_engine * e, void * N,
                                    cwal_finalizer_f dtor,
                                    void const * typeID ){
  if(!e || !N || !typeID) return NULL;
  else{
    cwal_value * v = cwal_value_new(e, CWAL_TYPE_NATIVE, 0);
    if( NULL != v ){
      cwal_native * n = CWAL_V2NATIVE(v);
      cwal_value * proto = n->base.prototype;
#if 0
      cwal_weak_annotate(e, N);
#endif
      *n = cwal_native_empty;
      n->base.prototype = proto;
      n->native = N;
      n->finalize = dtor;
      n->typeID = typeID;
    }
    return v;
  }
}

void cwal_native_set_rescoper( cwal_native * const nv,
                               cwal_value_rescoper_f const rescoper){
  nv->rescoper = rescoper;
}

int cwal_value_fetch_native( cwal_value const * val, cwal_native ** ar){
  if( ! val ) return CWAL_RC_MISUSE;
  else if( CWAL_TYPE_NATIVE != val->vtab->typeID ) return CWAL_RC_TYPE;
  else{
    if(ar) *ar = CWAL_V2NATIVE(val);
    return 0;
  }
}

int cwal_native_fetch( cwal_native const * n,
                       void const * typeID, void ** dest){
  if( !n) return CWAL_RC_MISUSE;
  else if(!typeID || n->typeID==typeID){
    if(dest) *dest = n->native;
    return 0;
  }else return CWAL_RC_TYPE;
}

void * cwal_native_get( cwal_native const * n, void const * typeID){
  void * x = NULL;
  cwal_native_fetch( n, typeID, &x );
  return x;
}

void cwal_native_clear( cwal_native * const n, bool callFinalizer ){
  if(n->native){
    if(callFinalizer && n->finalize){
      cwal_value * const nv = CWAL_VALPART(n);
      assert(nv->scope && nv->scope->e);
      n->finalize( nv->scope->e, n->native );
    }            
    n->native = NULL;
    n->finalize = NULL;
    n->typeID = NULL;
    n->rescoper = NULL;
  }
}

cwal_value * cwal_native_value( cwal_native const * n ){
  return n ? CWAL_VALPART(n) : 0;
}

cwal_native * cwal_new_native( cwal_engine * e, void * n,
                               cwal_finalizer_f dtor,
                               void const * typeID ){
  cwal_value * v = cwal_new_native_value( e, n, dtor, typeID );
  return v ? CWAL_V2NATIVE(v) : 0;
}

cwal_native * cwal_value_get_native( cwal_value const * v ) {
  cwal_native * ar = NULL;
  cwal_value_fetch_native( v, &ar );
  return ar;
}


cwal_size_t cwal_engine_recycle_max_get( cwal_engine * e, cwal_type_id type ){
  if(!e) return 0;
  else switch(type){
      case CWAL_TYPE_STRING:
        return e->reString.maxLength;
      default:{
        cwal_recycler const * re = cwal_recycler_get( e, type );
        return re ? re->maxLength : 0;
      }
    }
}

int cwal_engine_recycle_max( cwal_engine * e, cwal_type_id typeID,
                             cwal_size_t max ){
  if(!e) return CWAL_RC_MISUSE;
  else if( CWAL_TYPE_UNDEF == typeID ){
    int rc = 0;
#define DO(T) rc = cwal_engine_recycle_max(e, CWAL_TYPE_##T, max ); if(rc) return rc
    DO(ARRAY);
    DO(BUFFER);
    DO(DOUBLE);
    DO(EXCEPTION);
    DO(FUNCTION);
    DO(INTEGER);
    DO(KVP);
    DO(NATIVE);
    DO(OBJECT);
    DO(HASH);
    DO(SCOPE);
    DO(STRING);
    DO(XSTRING/* also Z-strings*/);
    DO(UNIQUE);
    DO(TUPLE);
    DO(WEAKREF);
    DO(PROPREF);
#undef DO
    return rc;
  }
  else {
    cwal_recycler * li;
    void * mem;
    li = cwal_recycler_get( e, typeID );
    assert(li && "Else a bogus type ID.");
    if( !li ) return CWAL_RC_TYPE;
    li->maxLength = max;
    while( li->count > max ){
      /* If we're over our limit, give them back... */
      assert(li->list);
      switch(typeID){
        case CWAL_TYPE_WEAKREF:{
          cwal_weakref * r = (cwal_weakref *)li->list;
          mem = r;
          li->list = r->next;
          r->next = NULL;
          break;
        }
        case CWAL_TYPE_KVP:{
          cwal_kvp * kvp = (cwal_kvp *)li->list;
          mem = kvp;
          li->list = kvp->right;
          kvp->right = 0;
          break;
        }
        default:
          mem = (cwal_value *)li->list;
          li->list = cwal_value_snip((cwal_value *)li->list);
          break;
      }
      --li->count;
      cwal_free( e, mem );
    }
    return CWAL_RC_OK;
  }
}

/**
   Ensures that s->props is initialized. Returns 0 on success, non-0
   on error (which is serious and must not be ignored). After a
   successful call for a given scope call it "cannot fail" on
   subsequent calls unless the scope somehow loses its properties, in
   which case this routine would create a new s->props object/hash.
*/
static int cwal_scope_init_props(cwal_scope *s){
  if(s->props) return 0;
  assert( s->e );
  s->props = cwal_new_object_value(s->e);
  /* reminder: its owning scope might be a different one!  We'll
     fix that below. */
  if(s->props){
    cwal_value * const pv = s->props;
    cwal_obase * const obase = CWAL_VOBASE(s->props);
    assert(obase);
    cwal_value_ref2(s->e, pv)
      /* we do this so that sweep() cannot wipe it out.
         Yes, i've actually seen this object get swept
         up before. */
      ;
    obase->flags |= CWAL_F_IS_PROP_STORAGE;
    if(pv->scope->level > s->level){
      /* This can happen if a scope creates no vars but one is
         added after a subscope is active. Our newly-created pv
         is owned by the newer scope initially, and we need to
         back-door it into the scope on whose behalf it stores
         properties (at least initially - it can move out later
         on).
      */
      cwal_value_xscope(s->e, s, pv, NULL);
      assert( obase->flags & CWAL_F_IS_PROP_STORAGE );
    }else{
      assert(s == pv->scope);
      if(cwal_scope_insert(pv->scope, pv)){
        /* We do ^^^^ this to move pv into the vacuum-safe list (on
           account of its CWAL_F_IS_PROP_STORAGE flag). */
        assert(s->e->fatalCode);
        return s->e->fatalCode;
      }
      assert( obase->flags & CWAL_F_IS_PROP_STORAGE );
    }
    assert(pv->scope->mine.headSafe == pv);
#if 1
    if(obase->prototype){
      /**
         If we don't do this then cwal_scope_search_v()
         and friends will resolve prototype members installed
         for the default prototype for props->proto's type by
         the client. It might be interesting to install our own
         prototype for these objects, but i'm not sure what we
         might do with it.

         i.e. if we don't do this then (using th1ish as an example):

         scope {
         get(xyz) // resolves to Object.get() inherited method
         }

         20181128: i still can't shake the feeling that there
         might be interesting uses for that in s2. Could we
         possibly implement a JS-like "with" feature using that,
         by setting the prototype to the "with'd" value?

         Nevermind: cwal_scope_search_v() and friends, via
         cwal_prop_get_kvp_v(), explicitly do not search
         prototypes for values. Maybe they should? Enabling that
         "shouldn't" break anything because scope properties
         have explicitely (via this upcoming call) had no
         prototypes since our earliest ancestors wrote code.
         Adding that feature would require adding support for it
         in cwal_hash_search_impl_v() and
         cwal_hash_search_impl_cstr(), as those don't currently
         support prototype traversal. Enabling prototype lookups
         might also have negative side effects if we "with'd"
         one scope properties with another and both scopes used
         different storage types (hash vs object). All routines
         involved would have to know to potentially switch
         types/modes, and that quickly turns into a rat's nest
         of special cases.
      */
      cwal_value_prototype_set(pv, NULL);
    }
#endif
  }
  return s->props
    ? 0
    : CWAL_RC_OOM;
}

static cwal_kvp * cwal__scope_search_kvp_v( bool resolvePropref,
                                            cwal_scope * s,
                                            int upToDepth,
                                            cwal_value const * key,
                                            cwal_scope ** foundIn ){
  if(!s || !key) return NULL;
  else if(!s->props && !upToDepth) return NULL;
  else {
    cwal_scope * os;
    cwal_kvp * kvp = 0;
    if( upToDepth<0 ){
      assert(s->level);
      upToDepth = (int)(s->level-1);
    }
    for( os = s;
         os && (upToDepth>=0);
         --upToDepth ){
      if(os->props){
        cwal_value * p = NULL;
        kvp = cwal__prop_get_kvp_v( resolvePropref, os->props, key,
                                    false, &p );
        if(kvp){
          if(foundIn){
            assert(p);
            while(os && os->props != p) os = os->parent;
            assert(os);
            *foundIn = os;
          }
          break;
        }
      }
      os = os->parent;
    }
#if 0
    if(kvp){
      dump_val(os ? os->props : 0,"cwal_scope_search_kvp_v() storage");
      MARKER(("kvp->flags=0x%04x\n", kvp->flags));
    }
#endif
    return kvp;
  }
}

static cwal_kvp * cwal__scope_search_kvp( bool resolvePropref,
                                          cwal_scope * s,
                                          int upToDepth,
                                          char const * key,
                                          cwal_midsize_t keyLen,
                                          cwal_scope ** foundIn ){
  if(!s || !key) return NULL;
  else if(!s->props && !upToDepth) return NULL;
  else {
    cwal_scope * os;
    cwal_kvp * kvp = 0;
    if( upToDepth<0 ){
      assert(s->level);
      upToDepth = (int)(s->level-1);
    }
    for( os = s;
         os && (upToDepth>=0);
         --upToDepth ){
      if(os->props){
        cwal_value * p = os->props;
        kvp = cwal__prop_get_kvp( resolvePropref, os->props, key,
                                  keyLen, false, &p );
        if(kvp){
          if(foundIn){
            assert(p);
            while(os && os->props != p) os = os->parent;
            assert(os);
            *foundIn = os;
          }
          break;
        }
      }
      os = os->parent;
    }
    return kvp;
  }
}

cwal_kvp * cwal_scope_search_kvp_v( cwal_scope * s,
                                    int upToDepth,
                                    cwal_value const * key,
                                    cwal_scope ** foundIn ){
  return cwal__scope_search_kvp_v( false, s, upToDepth, key, foundIn);
}

cwal_kvp * cwal_scope_search_kvp( cwal_scope * s,
                                  int upToDepth,
                                  char const * key,
                                  cwal_midsize_t keyLen,
                                  cwal_scope ** foundIn ){
  return cwal__scope_search_kvp(false, s, upToDepth,
                                key, keyLen, foundIn);
}


/**
   Temporary internal macro used to set up the engine/scope
   arguments for the cwal_var_xxx() family of functions.
*/
#define SETUP_VAR_E_S if(!e) e=s?s->e:0; if(!s)s=e?e->current:0;    \
  if(!s && !e) return CWAL_RC_MISUSE

static int cwal_var_set_v_impl( cwal_engine * e, cwal_scope * s,
                                cwal_value * key, cwal_value * v,
                                bool searchParents,
                                uint16_t flags ){
  if(!e || !key) return CWAL_RC_MISUSE;
  SETUP_VAR_E_S;
  else{
    /* We need to take parent scopes into account when assigning a
       variable. This causes duplicate lookup of the var via
       cwal_kvp_set_v() (which also has to search).
    */
    cwal_scope * foundIn = 0;
    int rc = 0;
    /* cwal_value * got = 0; */
    cwal_scope * origin = s;
    cwal_kvp * kvp = 0;
    assert(!(s->flags & CWAL_F_IS_DESTRUCTING));
    if(!v) searchParents = 0 /* do not remove props from parens this way! */;
    while(s && !foundIn){
      kvp = cwal__scope_search_kvp_v( true, s, 0, key, &foundIn );
      if(kvp) break;
      s = searchParents ? s->parent : 0;
    }
#if 0
    dump_val(key,"setting this key");
    MARKER(("var set v impl new kvp flags=0x%04x\n", flags));
    if(kvp){
      MARKER(("var set v impl kvp flags=0x%04x, new flags=0x%04x\n", (int)kvp->flags, flags));
      dump_val(kvp->key,"key");
      dump_val(kvp->value,"val");
    }
#endif
    if(kvp){
      /* Found a match. */
      /*MARKER(("kvp found. flags=0x%04d\n", (int)kvp->flags));*/
      /* dump_val(kvp->key,"key"); */
      assert(s == foundIn);
      assert(foundIn->props);
      if(CWAL_VAR_F_CONST & kvp->flags){
        return CWAL_RC_CONST_VIOLATION;
      }
      else if(v){
        rc = cwal_kvp_value_set2(kvp, v);
        if(!rc) cwal_value_rescope(foundIn, v);
        return rc;
      }
    }
    /* Below here, we did not find a match, so we need to insert one,
       OR we found a match but are about to do an UNSET.

       FIXME (2021-07-110: This is currently less efficient than it
       could be. We now have enough infrastructure to be able to do
       this without a second property lookup imposed by the upcoming
       set/unset operations.
    */
    if(!foundIn){
      if(!v) return CWAL_RC_NOT_FOUND;
      foundIn = origin;
    }
    if(!foundIn->props){
      rc = cwal_scope_init_props(foundIn);
    }else if(CWAL_V_IS_VISITING(foundIn->props)){
      rc = CWAL_RC_IS_VISITING;
    }
#if 0
    /* 2021-08-01: hypothetically not possible since removal of
       CWAL_FEATURE_SCOPE_STORAGE_HASH. */
    else if(CWAL_V_IS_VISITING_LIST(foundIn->props)){
      rc = CWAL_RC_IS_VISITING_LIST;
    }
#endif
    if(rc) return rc;
    assert(foundIn && foundIn->props);
#if 0
    MARKER(("Setting [opaque key type] in scope#%d via scope#%d.\n",
            (int)foundIn->level, (int)origin->level));
#endif

    cwal_obase * const b = CWAL_VOBASE(foundIn->props);
    assert(b);
    assert(foundIn->props);
#if CWAL_OBASE_ISA_HASH
    rc = v
      ? cwal_htable_insert_impl_v(foundIn->props, &b->hprops, true,
                                  key, v, true, flags, false)
      : cwal_htable_remove_impl_v(foundIn->props, &b->hprops,
                                  key);
#else
    rc = v
      ? cwal_kvp_set_v( e, foundIn->props, key, v, flags )
      : cwal_kvp_unset_v( e, &b->kvp, key );
#endif
    return rc;
  }
}
    
static int cwal_kvp_visitor_scope_import_props( cwal_kvp const * kvp,
                                                void * state ){
  cwal_scope * dest = (cwal_scope*)state;
  return cwal_var_set_v_impl( dest->e, dest, kvp->key, kvp->value, 0, kvp->flags );
}

int cwal_scope_import_props( cwal_scope * dest, cwal_value * src ){
  if(!src || !dest || src==dest->props) return CWAL_RC_MISUSE;
  else if(CWAL_F_IS_DESTRUCTING & dest->flags) return CWAL_RC_DESTRUCTION_RUNNING;
  else if(!cwal_props_can(src)) return CWAL_RC_TYPE;
  else{
    int rc = cwal_scope_init_props(dest);
    if(rc) return rc;
    assert(dest->props);
    if(CWAL_V_IS_VISITING(dest->props)) return CWAL_RC_IS_VISITING;
#if 0
    /* 2021-08-01: hypothetically not possible since removal of
       CWAL_FEATURE_SCOPE_STORAGE_HASH. */
    else if(CWAL_V_IS_VISITING_LIST(dest->props)) return CWAL_RC_IS_VISITING_LIST;
#endif
    if(dest->props && CWAL_V_IS_IN_CLEANUP(dest->props)){
      assert(!"This really shouldn't be possible unless this "
             "scope is destructing, which it's not.");
      return CWAL_RC_DESTRUCTION_RUNNING;
    }
    rc = cwal_props_visit_kvp( src, cwal_kvp_visitor_scope_import_props, dest );
#if CWAL_OBASE_ISA_HASH
    assert(CWAL_VOBASE(dest->props)->hprops.list.count >= CWAL_VOBASE(src)->hprops.list.count);
#endif
    return rc;
  }
}


cwal_scope * cwal_scope_parent( cwal_scope * s ){
  return s ? s->parent : 0;
}

cwal_scope * cwal_scope_top( cwal_scope * s ){
  for( ; s && s->parent; s = s->parent ){}
  return s;
}

cwal_value * cwal_scope_properties( cwal_scope * s ){
  if(!s) return NULL;
  else if(!s->props) cwal_scope_init_props(s);
  return s->props;
}

cwal_value * cwal_scope_search( cwal_scope * s, int upToDepth,
                                char const * key,
                                cwal_midsize_t keyLen,
                                cwal_scope ** foundIn ){
  cwal_kvp * const kvp =
    cwal__scope_search_kvp(true, s, upToDepth, key, keyLen, foundIn);
  return kvp ? kvp->value : NULL;
}

cwal_value * cwal_scope_search_v( cwal_scope * s, int upToDepth,
                                  cwal_value const * const key,
                                  cwal_scope ** foundIn ){
  cwal_kvp * const kvp =
    cwal__scope_search_kvp_v(true, s, upToDepth, key, foundIn);
  return kvp ? kvp->value : NULL;
}

int cwal_scope_chain_set_with_flags_v( cwal_scope * s, int upToDepth,
                                       cwal_value * k, cwal_value * v,
                                       uint16_t flags ){
  if(!s || !k) return CWAL_RC_MISUSE;
  else if(CWAL_F_IS_DESTRUCTING & s->flags) return CWAL_RC_DESTRUCTION_RUNNING;
  else {
    int rc;
    cwal_scope * os = NULL;
    cwal_kvp * const kvp = cwal__scope_search_kvp_v( true, s, upToDepth,
                                                     k, &os );
    cwal_obase * ob;
    assert(!(s->flags & CWAL_F_IS_DESTRUCTING));
    if(!os) os = s;
    if(os->props && CWAL_RCFLAG_HAS(os->props,CWAL_RCF_IS_DESTRUCTING)){
      return CWAL_RC_DESTRUCTION_RUNNING;
    }else if(os->props && CWAL_V_IS_VISITING(os->props)){
      return CWAL_RC_IS_VISITING;
    }
#if 0
    /* 2021-08-01: hypothetically not possible since removal of
       CWAL_FEATURE_SCOPE_STORAGE_HASH. */
    else if(CWAL_V_IS_VISITING_LIST(os->props)){
      return CWAL_RC_IS_VISITING_LIST;
    }
#endif
    ob = CWAL_VOBASE(os->props);
    if(ob){
      if(CWAL_CONTAINER_DISALLOW_PROP_SET & ob->flags){
        return CWAL_RC_DISALLOW_PROP_SET;
      }else if(!kvp && (CWAL_CONTAINER_DISALLOW_NEW_PROPERTIES & ob->flags)){
        return CWAL_RC_DISALLOW_NEW_PROPERTIES;
      }
    }
    if(kvp && (CWAL_VAR_F_CONST & kvp->flags)){
      return CWAL_RC_CONST_VIOLATION;
    }
#if 0
    if(kvp){
      MARKER(("kvp flags=0x%08x, new flags=0x%08x\n", (int)kvp->flags, flags));
      dump_val(kvp->key,"key");
      dump_val(kvp->value,"val");
    }
#endif
    if(v && kvp){
      /* avoid a second lookup for the property... */
      assert(os->props);
      rc = cwal_kvp_value_set2( kvp, v );
      if(!rc) cwal_value_rescope(os->props->scope, v);
      return rc;
    }else if(!os->props){
      rc = cwal_scope_init_props( os );
      if(rc) return rc;
      assert(os->props);
    }
    rc = cwal_prop_set_with_flags_v( os->props, k, v, flags );
    return rc;
  }
}

int cwal_scope_chain_set_v( cwal_scope * s, int upToDepth,
                            cwal_value * k, cwal_value * v ){
  return cwal_scope_chain_set_with_flags_v(s, upToDepth, k, v, CWAL_VAR_F_PRESERVE);
}

int cwal_scope_chain_set_with_flags( cwal_scope * s, int upToDepth,
                                     char const * k, cwal_midsize_t keyLen,
                                     cwal_value * v, uint16_t flags ){
  if(!s || !k) return CWAL_RC_MISUSE;
  else {
    int rc;
    cwal_value * kv = cwal_new_string_value(s->e, k, keyLen);
    if(!v) return CWAL_RC_OOM;
    cwal_value_ref(kv);
    rc = cwal_scope_chain_set_with_flags_v( s, upToDepth, kv, v, flags );
    cwal_value_unref(kv);
    return rc;
  }
}

int cwal_scope_chain_set( cwal_scope * s, int upToDepth,
                          char const * k, cwal_midsize_t keyLen,
                          cwal_value * v ){
  return cwal_scope_chain_set_with_flags( s, upToDepth, k, keyLen,
                                          v, CWAL_VAR_F_PRESERVE );
}

int cwal_var_decl_v( cwal_engine * e, cwal_scope * s,
                     cwal_value * key, cwal_value * v,
                     uint16_t flags ){
  if(!e || !key) return CWAL_RC_MISUSE;
  SETUP_VAR_E_S;
  else{
    int const rc = cwal_scope_init_props(s);
    if(rc) return rc;
    assert(s->props);
    if(CWAL_V_IS_VISITING(s->props)) return CWAL_RC_IS_VISITING;
#if 0
    /* 2021-08-01: hypothetically not possible since removal of
       CWAL_FEATURE_SCOPE_STORAGE_HASH. */
    else if(CWAL_V_IS_VISITING_LIST(s->props)) return CWAL_RC_IS_VISITING_LIST;
#endif
    return (cwal_scope_search_v(s, 0, key, NULL)
            ? CWAL_RC_ALREADY_EXISTS
            : cwal_var_set_v_impl( e, s, key,
                                   v ? v : cwal_value_undefined(),
                                   0, flags));
  }
}

int cwal_var_decl( cwal_engine * e, cwal_scope * s, char const * key,
                   cwal_midsize_t keyLen, cwal_value * v, uint16_t flags ){
  if(!e || !key || !*key) return CWAL_RC_MISUSE;
  SETUP_VAR_E_S;
  else {
    int rc;
    cwal_value * k = cwal_new_string_value(e, key, keyLen);
    if(!k) return CWAL_RC_OOM;
    cwal_value_ref(k);
    rc = cwal_var_decl_v(e, s, k, v, flags);
    cwal_value_unref(k);
    return rc;
  }
}

#undef SETUP_VAR_E_S

cwal_hash_t cwal_value_hash_null_undef( cwal_value const * v ){
  return (cwal_hash_t)
    ((CWAL_TYPE_NULL==v->vtab->typeID)
     ? -1 : -2);
}

cwal_hash_t cwal_value_hash_bool( cwal_value const * v ){
  return (cwal_hash_t) (CWAL_BOOL(v) ? -4 : -3);
}

cwal_hash_t cwal_value_hash_int( cwal_value const * v ){
  return (cwal_hash_t) *CWAL_INT(v);
}

cwal_hash_t cwal_value_hash_double( cwal_value const * v ){
#if CWAL_PLATFORM_ARM
  return (cwal_hash_t) cwal_value_get_double(v)
    /* See comments below. This formulation does not
       bus fault on my ARM, but is slower. */
    ;
#else
  return (cwal_hash_t) *CWAL_DBL(v)
    /* On my ARM box this is causing a bus fault for the builtin
       double 1.0, but not for a dynamically-allocated double! */
    ;
#endif
}

cwal_hash_t cwal_value_hash_string( cwal_value const * v ){
  cwal_string const * s = CWAL_STR(v);
  assert(s);
  return cwal_hash_bytes( cwal_string_cstr(s), CWAL_STRLEN(s) );
}

cwal_hash_t cwal_value_hash_ptr( cwal_value const * v ){
#if CWAL_VOID_PTR_IS_BIG
  /* IF THE DEBUGGER LEADS YOU NEAR HERE...
     try changing ptr_int_t back to uint64_t.
  */
  typedef uint64_t ptr_int_t;
#else
  typedef uint32_t ptr_int_t;
#endif
  return (cwal_hash_t)(((ptr_int_t)v / (ptr_int_t)sizeof(void*))
                       + ((ptr_int_t)v % (ptr_int_t)sizeof(void*)));
}

cwal_hash_t cwal_value_hash_tuple( cwal_value const * v ){
#if 1
  return cwal_value_hash_ptr(v);
#else
  /* needed? If so, do we want to start with the ptr hash or 0?
     No - then we cannot use tuples as hash keys (hash value must
     be constant).*/
  cwal_hash_t h = cwal_value_hash_ptr(v);
  cwal_tuple const * p = CWAL_TUPLE(v);
  uint16_t i = 0;
  for( ; i < p->n; ++i ) h += cwal_value_hash(p->list[i]);
  return h;
#endif
}

cwal_hash_t cwal_value_hash( cwal_value const * const v ){
  return (v && v->vtab && v->vtab->hash)
    ? v->vtab->hash(v)
    : 0;
}

int cwal_value_compare( cwal_value const * lhs, cwal_value const * rhs ){
  if(lhs == rhs) return 0;
  else if(!lhs) return -1;
  else if(!rhs) return 1;
  else return lhs->vtab->compare( lhs, rhs );
}

#define COMPARE_TYPE_IDS(L,R) ((L)->vtab->typeID - (R)->vtab->typeID)
/* (((L)->vtab->typeID < (R)->vtab->typeID) ? -1 : 1) */

int cwal_value_cmp_ptr_only( cwal_value const * lhs, cwal_value const * rhs ){
  if(lhs==rhs) return 0;
  else if(lhs->vtab->typeID==rhs->vtab->typeID) return
                                                  ((void const*)lhs < (void const *)rhs)
                                                  ? -1 : 1 /*
                                                             Why do this? Because it at least provides
                                                             consistent ordering for two different instances
                                                             within a given value's lifetime. i can't think of
                                                             any saner alternative :/.
                                                           */
                                                  ;
  else return COMPARE_TYPE_IDS(lhs,rhs);
}

int cwal_value_cmp_func( cwal_value const * lhs, cwal_value const * rhs ){
  if(lhs==rhs) return 0;
  else if(lhs->vtab->typeID!=rhs->vtab->typeID) return COMPARE_TYPE_IDS(lhs,rhs);
  else {
    cwal_function const * l  = cwal_value_get_function(lhs);
    cwal_function const * r  = cwal_value_get_function(rhs);
    assert(l);
    if(!l) return r ? -1 : 0;
    else if(!r) return 1;
    else if((l->callback == r->callback) && (l->state.data==r->state.data)) return 0;
    else return
           ((void const*)lhs < (void const *)rhs)
           ? -1 : 1 /* see comments in cwal_value_cmp_ptr_only() */;
  }
}

int cwal_value_cmp_tuple( cwal_value const * lhs, cwal_value const * rhs ){
  if(lhs==rhs) return 0;
  else if(lhs->vtab->typeID!=rhs->vtab->typeID) return COMPARE_TYPE_IDS(lhs,rhs);
  else {
    /*
      Compare the cwal_tuple::list parts using normal value
      compare semantics... Only compare equivalent if all
      entries compare equivalent.
    */
    cwal_tuple const * l  = CWAL_TUPLE(lhs);
    cwal_tuple const * r  = CWAL_TUPLE(rhs);
    assert(l);
    assert(r);
    assert(l != r);
    if(l->n != r->n) return (int)l->n - (int)r->n;
    else{
      cwal_size_t i;
      int cmp = 0;
      assert(l->n && r->n && "We can't have made it this far with the 0-tuple.");
      for( i = 0; i < l->n && !cmp; ++i ){
        cmp = cwal_value_compare(l->list[i], r->list[i]);
      }
      return cmp;
    }
  }
}

int cwal_value_cmp_buffer( cwal_value const * lhs, cwal_value const * rhs ){
  if(lhs==rhs) return 0;
  else if(lhs->vtab->typeID!=rhs->vtab->typeID) return COMPARE_TYPE_IDS(lhs,rhs);
  else {
    cwal_buffer const * l  = cwal_value_get_buffer(lhs);
    cwal_buffer const * r  = cwal_value_get_buffer(rhs);
    assert(l);
    if(!l) return r ? -1 : 0;
    else if(!r) return 1;
    else if(l->used == r->used){
      return l->used ? memcmp(l->mem, r->mem, l->used) : 0;
    }else{
      cwal_size_t const min = (l->used < r->used) ? l->used : r->used;
      int const cmp = min
        ? memcmp(l->mem, r->mem, min)
        : ((l->used==min) ? -1 : 1);
      return cmp ? cmp : ((l->used==min) ? -1 : 1);
    }
  }
}
    
int cwal_compare_cstr( char const * s1, cwal_size_t len1,
                       char const * s2, cwal_size_t len2 ){
  if(!len1) return len2 ? -1 : 0;
  else if(!len2) return 1;
  else if(!s1) return s2 ? -1 : 0;
  else if(!s2) return 1;
  else {
    cwal_size_t const max = (len1<len2) ? len1 : len2;
    int const rc = memcmp( s1, s2, max );
    /* When the first max bytes match, the shorter string compares less
       than the longer one. */
    return (0!=rc)
      ? rc
      : (len1==len2 ? 0 :
         (len1<len2 ? -1 : 1));
  }
}

int cwal_compare_str_cstr( cwal_string const * s1,
                           char const * s2, cwal_size_t len2 ){
  return cwal_compare_cstr(s1? cwal_string_cstr(s1) : 0,
                           s1 ? CWAL_STRLEN(s1) : 0,
                           s2, len2);
}


#if 0
int cwal_value_cmp_type_only( cwal_value const * lhs, cwal_value const * rhs ){
  return(lhs->vtab->typeID==rhs->vtab->typeID)
    ? 0
    : COMPARE_TYPE_IDS(lhs,rhs);
}
#endif

int cwal_value_cmp_nullundef( CWAL_UNUSED_VAR cwal_value const * lhs, cwal_value const * rhs ){
#if !defined(NDEBUG)
  char const isNull = (CWAL_TYPE_NULL==lhs->vtab->typeID) ? 1 : 0;
  assert(isNull || CWAL_TYPE_UNDEF==lhs->vtab->typeID);
#endif
  switch( rhs->vtab->typeID ){
    case CWAL_TYPE_NULL:
    case CWAL_TYPE_UNDEF:
      return 0;
    default:
      return cwal_value_get_bool(rhs) ? -1 : 0;
  }
}

int cwal_value_cmp_string( cwal_value const * lhs, cwal_value const * rhs ){
  cwal_string const * s = CWAL_STR(lhs);
  assert(s && "lhs is not-a string");
  switch(rhs->vtab->typeID){
#if 0
    case CWAL_TYPE_OBJECT:
    case CWAL_TYPE_ARRAY:
    case CWAL_TYPE_FUNCTION:
    case CWAL_TYPE_NATIVE:
    case CWAL_TYPE_BUFFER:
    case CWAL_TYPE_EXCEPTION:
      /* Seems to be how JS does it. */
      return CWAL_STRLEN(s) ? -1 : 1;
#endif
    case CWAL_TYPE_STRING:{
      cwal_string const * r = CWAL_STR(rhs);
      return cwal_compare_cstr( cwal_string_cstr(s), CWAL_STRLEN(s),
                                cwal_string_cstr(r), CWAL_STRLEN(r) );
    }
    case CWAL_TYPE_BOOL: /* BOOL here is sooo arguable. */
      return CWAL_STRLEN(s)
        ? (cwal_value_get_bool(rhs) ? 0 : 1)
        : (cwal_value_get_bool(rhs) ? -1 : 0);
#if 1
    case CWAL_TYPE_INTEGER:
    case CWAL_TYPE_DOUBLE:{
      /* FIXME: for number-string conversions it would make
         more sense to do it the other way around: convert
         the string to a number, then compare. That way we
         have no problems with trailing 0's and whatnot.

         20181127: tried that (see #if'd-out block below)
         and it introduce this Damned Weird behaviour:

         var o = {2:'two'};
         assert o.hasOwnProperty(2); // okay
         assert o.hasOwnProperty('2'); // FAILS

         And yet:

         var o = {a:1, 2: 'two'}
         assert o.hasOwnProperty(2); // okay
         assert o.hasOwnProperty('2'); // okay

         That's a bonafide bug but i'm far too tired to chase
         it down, so we'll go back to this comparison for
         the time being.
      */
      enum { BufSize = 100 };
      char buf[BufSize] = {0,};
      cwal_size_t sz = BufSize;
      switch(rhs->vtab->typeID){
        case CWAL_TYPE_INTEGER:
          cwal_int_to_cstr( cwal_value_get_integer(rhs), buf, &sz);
          break;
        case CWAL_TYPE_DOUBLE:
          cwal_double_to_cstr( cwal_value_get_double(rhs), buf, &sz);
          break;
        case CWAL_TYPE_BOOL:
          /* FIXME? Use "true" and "false" instead of 0 and 1? */
        default:
          cwal__fatal(CWAL_RC_ASSERT, "Cannot happen: unhandled value type %s.",
                      rhs->vtab->typeName);
          break /* not reached */;
      }
      return cwal_compare_cstr( cwal_string_cstr(s), CWAL_STRLEN(s),
                                buf, sz );
    }
#else
      /* This breaks unit tests */
    case CWAL_TYPE_INTEGER:{
      cwal_int_t i = 0, iR = cwal_value_get_integer(rhs);
      cwal_string_to_int( s, &i );
      return i==iR ? 0 : i<iR ? -1 : 1;
    }
    case CWAL_TYPE_DOUBLE:{
      cwal_double_t i = 0, iR = cwal_value_get_double(rhs);
      cwal_string_to_double( s, &i );
      return i==iR ? 0 : i<iR ? -1 : 1;
    }
#endif
    default:
      return COMPARE_TYPE_IDS(lhs,rhs);
  }
}

int cwal_value_cmp_bool( cwal_value const * lhs, cwal_value const * rhs ){
  assert(CWAL_TYPE_BOOL==lhs->vtab->typeID);
  switch( rhs->vtab->typeID ){
    case CWAL_TYPE_BOOL:
      /* reminder: all booleans point either to the same value
         (for true) or NULL (for false). */
      return (lhs==rhs)
        ? 0
        : (CWAL_BOOL(lhs) ? 1 : -1);
    case CWAL_TYPE_INTEGER:
    case CWAL_TYPE_DOUBLE:{
      char const l = CWAL_BOOL(lhs);
      char const r = cwal_value_get_bool(rhs);
      return l==r
        ? 0 : (l<r ? -1 : 1);
    }
    case CWAL_TYPE_STRING:
      return -(rhs->vtab->compare(rhs, lhs));
#if 0
    case CWAL_TYPE_UNDEF:
    case CWAL_TYPE_NULL:
      return -1;
#endif
    default:
      return COMPARE_TYPE_IDS(lhs,rhs);
  };
}

int cwal_value_cmp_int( cwal_value const * lhs, cwal_value const * rhs ){
  assert(CWAL_TYPE_INTEGER==lhs->vtab->typeID);
  switch( rhs->vtab->typeID ){
    case CWAL_TYPE_DOUBLE:{
      cwal_int_t const l = *CWAL_INT(lhs);
      cwal_double_t const r = cwal_value_get_double(rhs);
      return l==r ? 0 : (l<r ? -1 : 1);
    }
    case CWAL_TYPE_BOOL:
    case CWAL_TYPE_INTEGER:{
      cwal_int_t const l = *CWAL_INT(lhs);
      cwal_int_t const r = cwal_value_get_integer(rhs);
      return l==r ? 0 : (l<r ? -1 : 1);
    }
    case CWAL_TYPE_STRING:
      return -(rhs->vtab->compare(rhs, lhs));
#if 0
    case CWAL_TYPE_UNDEF:
    case CWAL_TYPE_NULL:{
      cwal_int_t const l = *CWAL_INT(lhs);
      return (l==0) ? 0 : (l<0 ? -1 : 1)
        /* in xemacs's font that looks like
           one's instead of ell's.
        */
        ;
    }
#endif
    default:
      return COMPARE_TYPE_IDS(lhs,rhs);
  };
}

int cwal_value_cmp_double( cwal_value const * lhs, cwal_value const * rhs ){
  assert(CWAL_TYPE_DOUBLE==lhs->vtab->typeID);
  switch( rhs->vtab->typeID ){
    case CWAL_TYPE_BOOL:
    case CWAL_TYPE_INTEGER:{
      cwal_double_t const l = cwal_value_get_double(lhs);
      cwal_int_t const r = cwal_value_get_integer(rhs);
      return l==r ? 0 : (l<r ? -1 : 1);
    }
    case CWAL_TYPE_DOUBLE:{
      cwal_double_t const l = cwal_value_get_double(lhs);
      cwal_double_t const r = cwal_value_get_double(rhs);
      return l==r
        ? 0 : (l<r ? -1 : 1);
    }
    case CWAL_TYPE_STRING:
      return -(rhs->vtab->compare(rhs, lhs));
#if 0
    case CWAL_TYPE_UNDEF:
    case CWAL_TYPE_NULL:{
      cwal_double_t const l = *CWAL_DBL(lhs);
      return (l==0.0) ? 0 : (l<0.0 ? -1 : 1);
    }
#endif
    default:
      return COMPARE_TYPE_IDS(lhs,rhs);
  };
}

int cwal_value_compare_v2( cwal_value * const lhs, cwal_value * const rhs,
                           bool resolvePropref, int * errCode ){
  assert(lhs && rhs);
  if(!resolvePropref){
    if(errCode) *errCode = 0;
    return cwal_value_compare(lhs, rhs);
  }else if(lhs==rhs){
    if(errCode) *errCode = 0;
    return 0;
  }
  int rc = 0;
  cwal_value * rL = NULL /* propref-resolved values */;
  cwal_value * rR = NULL;
  cwal_engine * e = NULL;
  bool toggleVacOff = false;
  for(int i = 0; i < 2; ++i){
    /* Check lhs/rhs for propref resolution... */
    cwal_value * const arg = i ? rhs : lhs;
    cwal_value * * pArg = i ? &rR : &rL;
    if(arg && arg->scope && !e) e = arg->scope->e;
    if(arg && CWAL_TYPE_PROPREF==arg->vtab->typeID){
      cwal_propref * const p = CWAL__V2PROPREF(arg);
      *pArg = NULL;
      rc = cwal_propref_resolve(p, arg, pArg);
      if(rc){
        assert(!rR);
        if(lhs == rL) cwal_unhand(rL);
        else cwal_unref(rL);
        if(errCode) *errCode = rc;
        return 0;
      }
    }else{
      *pArg = arg;
    }
    if(*pArg){
      cwal_ref(*pArg);
      if(0==i && !CWAL_V_IS_VACUUM_SAFE(*pArg)){
        /* Reminder: resolution of rhs can trigger arbitrary script
           code and lead to sweeps and vacuums. The sweeps are
           harmless for us here but a vacuum would be fatal if the
           first of two resolved values is not made vacuum-safe and
           the resolution of the second triggers a vacuum. Thus we
           toggle the vacuum-safe flag on rL. */
        cwal_value_make_vacuum_proof(*pArg, true);
        toggleVacOff = true;
      }
    }else{
      *pArg = cwal_value_undefined();
    }
  }
  assert(rL && rR);
  if(toggleVacOff) cwal_value_make_vacuum_proof(rL, false);
  rc = cwal_value_compare(rL, rR);
  if(errCode) *errCode = e ? e->err.code : 0;
  if(lhs == rL) cwal_unhand(rL);
  else cwal_unref(rL);
  if(rhs == rR) cwal_unhand(rR);
  else cwal_unref(rR);
  return rc;
}


int cwal_stream( cwal_input_f inF, void * inState,
                 cwal_output_f outF, void * outState ){
  if(!inF || !outF) return CWAL_RC_MISUSE;
  else{
    int rc = 0;
    enum { BufSize = 1024 * 4 };
    unsigned char buf[BufSize];
    cwal_size_t rn = BufSize;
    for( ; !rc &&
           (rn==BufSize)
           && (0==(rc=inF(inState, buf, &rn)));
         rn = BufSize){
      if(rn) rc = outF(outState, buf, rn);
      else break;
    }
    return rc;
  }
}

int cwal_output_f_cwal_engine( void * state, void const * src, cwal_size_t n ){
  return cwal_output( (cwal_engine *)state, src, n );
}

int cwal_output_f_cwal_outputer( void * state, void const * src, cwal_size_t n ){
  cwal_outputer * out = (cwal_outputer *)state;
  if(!src || !n) return 0;
  else return out->output ? out->output( out->state.data, src, n ) : 0;
}

cwal_outputer const * cwal_engine_outputer_get( cwal_engine const * e ){
  return e ? &e->vtab->outputer : 0;
}

void cwal_engine_outputer_set( cwal_engine * e,
                               cwal_outputer const * replacement,
                               cwal_outputer * tgt ){
  assert(e && replacement);
  if(tgt) *tgt = e->vtab->outputer;
  e->vtab->outputer = *replacement;
}


int cwal_buffer_fill_from( cwal_engine * e, cwal_buffer * dest, cwal_input_f src, void * state )
{
  int rc;
  enum { BufSize = 512 * 8 };
  char rbuf[BufSize];
  cwal_size_t total = 0;
  cwal_size_t rlen = 0;
  if( ! dest || ! src ) return CWAL_RC_MISUSE;
  dest->used = 0;
  while(1)
  {
    rlen = BufSize;
    rc = src( state, rbuf, &rlen );
    if( rc ) break;
    total += rlen;
    if(total<rlen){
      /* Overflow! */
      rc = CWAL_RC_RANGE;
      break;
    }
    if( dest->capacity < (total+1) )
    {
      rc = cwal_buffer_reserve( e, dest,
                                total + ((rlen<BufSize) ? 1 : BufSize)
                                /* Probably unnecessarily clever :/ */
                                );
      if( 0 != rc ) break;
    }
    memcpy( dest->mem + dest->used, rbuf, rlen );
    dest->used += rlen;
    if( rlen < BufSize ) break;
  }
  if( !rc && dest->used )
  {
    assert( dest->used < dest->capacity );
    dest->mem[dest->used] = 0;
  }
  return rc;
}

int cwal_input_f_FILE( void * state, void * dest, cwal_size_t * n ){
  FILE * f = (FILE*) state;
  if( ! state || ! n || !dest ) return CWAL_RC_MISUSE;
  else if( !*n ) return CWAL_RC_RANGE;
  *n = (cwal_size_t)fread( dest, 1, *n, f );
  return !*n
    ? (feof(f) ? 0 : CWAL_RC_IO)
    : 0;
}

int cwal_buffer_fill_from_FILE( cwal_engine * e, cwal_buffer * dest, FILE * src ){
  if(!e || !dest || !src) return CWAL_RC_MISUSE;
  else{
    long pos = ftell(src);
    int rc = (pos>=0) ? fseek(src, 0, SEEK_END) : -1;
    long epos = rc ? 0 : ftell(src);
    /* Ignore any tell/fseek-related errors */
    if(!rc){
      rc = fseek(src, pos, SEEK_SET);
      if(!rc){
        assert(epos>=pos);
        if(epos>pos){
          rc = cwal_buffer_reserve(e, dest,
                                   dest->used +
                                   (cwal_size_t)(epos-pos) + 1);
          if(rc) return rc;
        }
      }
    }
    if(rc) errno = 0;
    return cwal_buffer_fill_from( e, dest, cwal_input_f_FILE, src );
  }
}



int cwal_buffer_fill_from_filename( cwal_engine * e, cwal_buffer * dest, char const * filename ){
  if(!e || !dest || !filename || !*filename) return CWAL_RC_MISUSE;
  else{
    int rc;
    FILE * src = (('-'==*filename)&&!*(filename+1)) ? stdin : fopen(filename,"rb");
    if(!src) return CWAL_RC_IO;
    rc = (stdin==src)
      ? cwal_buffer_fill_from( e, dest, cwal_input_f_FILE, src )
      : cwal_buffer_fill_from_FILE( e, dest, src) /* to get pre-allocating behaviour */
      ;
    if(stdin!=src) fclose(src);
    return rc;
  }
}

int cwal_buffer_fill_from_filename2( cwal_engine * e, cwal_buffer * dest, char const * filename,
                                     cwal_size_t nameLen){
  enum { BufSize = 2048 };
  if(!e || !dest || !filename || !*filename) return CWAL_RC_MISUSE;
  else if(!nameLen || nameLen>=(cwal_size_t)BufSize) return CWAL_RC_RANGE;
  else{
    char nameBuf[BufSize] = {0};
    memcpy( nameBuf, filename, (size_t)nameLen );
    nameBuf[nameLen] = 0;
    return cwal_buffer_fill_from_filename(e, dest, filename);
  }
}



int cwal_buffer_clear( cwal_engine * e, cwal_buffer * b ){
  return cwal_buffer_reserve(e, b, 0);
}

int cwal_buffer_reserve( cwal_engine * e, cwal_buffer * buf, cwal_size_t n ){
  if( !e || !buf ) return CWAL_RC_MISUSE;
  else if( 0 == n ){
    if(buf->mem){
      assert(buf->capacity);
      assert((cwal_size_t)-1 != buf->capacity);
      cwal_memchunk_add(e, buf->mem, buf->capacity);
      cwal_buffer_wipe_keep_self(buf);
      assert(0==buf->mem);
      assert(0==buf->capacity);
      assert(0==buf->used);
    }
    return 0;
  }
  else if( buf->capacity >= n ){
    return 0;
  }
    
  if(!buf->mem){
    cwal_size_t reqSize = n;
    void * mem = cwal_memchunk_request(e, &reqSize, 1000,
                                       "cwal_buffer_reserve()");
    if(mem){
      assert(reqSize>=n);
      buf->mem = (unsigned char *)mem;
      buf->capacity = reqSize;
      buf->mem[0] = 0;
      return 0;
    }
    /* else fall through */
  }

  {
    unsigned char * x;
#if 0
    /* A theoretical micro-optimization for the memchunk
       recycler. In s2's unit tests (20141206) it gains no
       recycling and costs ~80 bytes. */
    if(n<sizeof(cwal_memchunk_overlay)) n = sizeof(cwal_memchunk_overlay);
#endif
    x = (unsigned char *)cwal_realloc( e, buf->mem, n );
    if( ! x ) return CWAL_RC_OOM;
    e->metrics.bytes[CWAL_TYPE_BUFFER] += (n - buf->capacity);
    memset( x + buf->used, 0, n - buf->used );
    buf->mem = x;
    buf->capacity = n;
    return 0;
  }
}

cwal_size_t cwal_buffer_fill( cwal_buffer * buf, unsigned char c ){
  if( !buf || !buf->capacity || !buf->mem ) return 0;
  else{
    memset( buf->mem, c, buf->capacity );
    return buf->capacity;
  }
}

int cwal_buffer_replace_str( cwal_engine * e, cwal_buffer * self,
                             unsigned char const * needle, cwal_size_t needleLen,
                             unsigned char const * repl, cwal_size_t replLen,
                             cwal_size_t limit,
                             cwal_size_t * changeCount){
  int rc = 0;
  if(!e || !self || !needle || (!repl && replLen>0)) rc = CWAL_RC_MISUSE;
  else if(!needleLen) rc = CWAL_RC_RANGE;
  if(rc || !self->used || needleLen>self->used){
    /* nothing to do */
    if(changeCount) *changeCount = 0;
    return rc;
  }else{
    cwal_size_t matchCount = 0;
    cwal_size_t slen = self->used;
    unsigned char const * pos = self->mem;
    unsigned char const * eof = pos + slen;
    unsigned char const * start = pos;
    cwal_buffer * buf;
    cwal_buffer bufLocal = cwal_buffer_empty;
    buf = &bufLocal;
    while( 1 ){
      if( (pos>=eof)
          || (0==memcmp(needle, pos, needleLen))
          ){
        cwal_size_t sz;
        char last = (pos>=eof) ? 1 : 0;
        if(!last && matchCount++==limit && limit>0){
          pos = eof;
          last = 1;
        }
        else if(pos>eof) pos=eof;
        sz = pos-start;
        if(sz){
          /* Append pending unmatched part... */
          rc = cwal_buffer_append(e, buf, start, sz);
        }
        if(!rc && pos<eof && replLen){
          /* Append replacement... */
          /* assert(pos <= (eof-replLen)); */
          rc = cwal_buffer_append(e, buf, repl, replLen);
        }
        if(rc || last) break;
        pos += needleLen;
        start = pos;
      }else{
        cwal_utf8_read_char( pos, eof, &pos );
      }
    }
    if(!rc){
      if(matchCount){
        cwal_buffer btmp = *self;
        void * const vself = self->self;
        btmp.self = 0;
        *self = *buf;
        self->self = vself;
        *buf = btmp;        
      }/*else self->buf is still okay*/
    }
    if(changeCount) *changeCount = matchCount;
    cwal_buffer_clear(e, buf);
    assert(self->capacity > self->used);
    self->mem[self->used] = 0;
    return rc;
  }
}

int cwal_buffer_replace_byte( cwal_engine * e, cwal_buffer * buf,
                              unsigned char needle, unsigned char repl,
                              cwal_size_t limit,
                              cwal_size_t * changeCount){
  cwal_size_t matchCount = 0;
  int rc = 0;
  if(!e || !buf){
    rc = CWAL_RC_MISUSE;
  }else if(!buf->used || needle==repl){
    /* nothing to do! */
    rc = 0;
  }else{
    cwal_size_t i = 0;
    for( ; i < buf->used; ++i ){
      if(buf->mem[i] == needle){
        buf->mem[i] = repl;
        if(++matchCount && limit && matchCount==limit) break;
      }
    }
  }
  if(changeCount) *changeCount = matchCount;
  return rc;
}



void cwal_engine_adjust_client_mem( cwal_engine * e, cwal_int_t amount ){
  assert(e);
  if(!amount) return;
  else if(amount<0){
    cwal_size_t const x = (cwal_size_t)(-amount);
    if(x >= e->metrics.clientMemCurrent) e->metrics.clientMemCurrent = 0;
    else e->metrics.clientMemCurrent -= x;
  }else{
    e->metrics.clientMemCurrent += amount;
    e->metrics.clientMemTotal += amount;
  }
}

void cwal_dump_allocation_metrics( cwal_engine * const e ){
  cwal_size_t totalRequests = 0;
  cwal_size_t totalAlloced = 0;
  cwal_size_t totalSizeof = 0;
  cwal_size_t grandTotal = 0;
  typedef struct {
    cwal_type_id tid;
    char const * note;
    cwal_size_t size;
  } TheTypes;
  TheTypes typeInfo[] = {
  /* MUST be in the same order as the cwal_type_id enum entries */
  {CWAL_TYPE_UNDEF, 0, 0},
  {CWAL_TYPE_NULL, 0, 0},
  {CWAL_TYPE_BOOL, 0, 0},
  {CWAL_TYPE_INTEGER, 0, 0},
  {CWAL_TYPE_DOUBLE, 0,0},
  {CWAL_TYPE_STRING, "Incl. string bytes plus NULs",0},
  {CWAL_TYPE_ARRAY, "Not incl. cwal_list memory",0},
  {CWAL_TYPE_OBJECT, 0,0},
  {CWAL_TYPE_FUNCTION, 0,0},
  {CWAL_TYPE_EXCEPTION, 0,0},
  {CWAL_TYPE_NATIVE, "Not including (opaque/unknown) native memory",0},
  {CWAL_TYPE_BUFFER, "[2] Incl. total allocated buffer memory and non-Value buffers",0},
  {CWAL_TYPE_HASH, "Not incl. cwal_list table memory.",0},
  {CWAL_TYPE_SCOPE, 0, 0},
  {CWAL_TYPE_KVP, 0, 0},
  {CWAL_TYPE_WEAKREF, 0, 0},
  {CWAL_TYPE_XSTRING, "Not incl. external string bytes",0},
  {CWAL_TYPE_ZSTRING, "Incl. string bytes",0},
  {CWAL_TYPE_UNIQUE, 0,0},
  {CWAL_TYPE_TUPLE, "[4]",0},
  {CWAL_TYPE_PROPREF, 0,0},
  {CWAL_TYPE_LISTMEM, "[5] Total alloced cwal_list::list memory used by arrays, hashtables, ...", 0}
  };
  assert(e);

        
  cwal_outputf(e, "%-18s   %-16s"
               "Actually allocated count * Value-type sizeof() "
               "==> total bytes from allocator\n\n",
               "TypeId/Bin[1]/Name",
               "AllocRequests"
               );

  for( size_t i = 0; i < (sizeof(typeInfo)/sizeof(typeInfo[0])); ++i ){
    TheTypes * const tinfo = &typeInfo[i];
    cwal_type_id const tid = tinfo->tid;
    if(!e->metrics.bytes[tid] && !e->metrics.requested[tid]) continue;
    int reIndex;
    reIndex = cwalRecyclerInfo.indexes[tid]
      /* because the index cwal_recycler_index()
         would give for strings is misleading. */;
    switch(i){
      case CWAL_TYPE_SCOPE:
        /* The stack-allocatedness of these skews the metrics.
           Don't count them for grand total statistics. */
        totalRequests += e->metrics.allocated[tid];
        break;
      default:
        totalRequests += e->metrics.requested[tid];
        break;
    }
    tinfo->size = cwal_type_id_sizeof(tinfo->tid);
    totalAlloced += e->metrics.allocated[tid];
    totalSizeof += tinfo->size * e->metrics.allocated[tid];
    grandTotal += e->metrics.bytes[tid];
    cwal_outputf(e, "%-3d%2d %-15s"
                 "%-16"CWAL_SIZE_T_PFMT
                 "%-7"CWAL_SIZE_T_PFMT
                 "(%06.2f%%) " /* reminder to self: the 06 applies to the _whole_ number! */
                 " * %2d "
                 "==> %-8"CWAL_SIZE_T_PFMT" %s"
                 "\n",
                 i,
                 reIndex,
                 cwal_type_id_name(tinfo->tid),
                 (cwal_size_t)e->metrics.requested[tid],
                 (cwal_size_t)e->metrics.allocated[tid],
                 e->metrics.requested[tid]
                 ? (double)e->metrics.allocated[tid]/e->metrics.requested[tid]*100.0
                 : 0,
                 tinfo->size,
                 (cwal_size_t)e->metrics.bytes[tid],
                 tinfo->note ? tinfo->note : ""
                 );
  }

  if(totalRequests){
    cwal_outputf(e,
                 "\nTotals:              "
                 "%-16"CWAL_SIZE_T_PFMT
                 "%-7"CWAL_SIZE_T_PFMT
                 "(%06.2f%%)"
                 "[1]    ==> %-8"CWAL_SIZE_T_PFMT
                 "\n",
                 (cwal_size_t)totalRequests,
                 (cwal_size_t)totalAlloced,
                 (double)totalAlloced/totalRequests*100.0,
                 (cwal_size_t)grandTotal);
    cwal_outputf(e,"\nNotes:\n");
    cwal_outputf(e,"\nThe recycler moves some bits of memory around, "
                 "counting them only once, so do not search for an Absolute "
                 "Truth in these metrics!\n");
    cwal_outputf(e,"\n  [1] = Types with the same 'bin' value share a "
                 "recycling bin. A value of -1 indicates either no "
                 "recycling or a separate mechanism.\n");
    cwal_outputf(e,"\n  [2] = "
                 "%% applies to allocation counts, not sizes. "
                 "The total alloc count does not account for "
                 "cwal_buffer::mem (re)allocations, but the total size "
                 "does. A request/allocation count of 0 and memory >0 "
                 "means that only non-Value buffers allocated memory.\n");
    cwal_outputf(e, "\n  [3] = Scopes are always stack allocated "
                 "and skew the statistics, so only allocated scopes "
                 "are counted for totals purposes.\n");
    cwal_outputf(e, "\n  [4] = Tuples of all lengths>0, excluding list "
                 "memory (managed via the chunk recycler).\n");
    cwal_outputf(e, "\n  [5] = cwal_list memory is internally a special case: "
                 "1) 'sizeof()' is meaningless and 2) the allocation count "
                 "includes re-allocations to larger sizes, but the total includes "
                 "only the maximum (re)alloc'd size of each list. List memory "
                 "recycled from other places (e.g. the chunk recycler) is not "
                 "counted against this type's memory total, but do count as alloc "
                 "requests.\n");
  }

  if(CWAL_TYPE_UNDEF != e->metrics.highestRefcountType){
    cwal_outputf(e, "\nHighest individual refcount=%d on a value of type '%s'.\n"
                 "(FYI, that's essentially guaranteed to be one of the "
                 "base-most prototypes.)\n",
                 (unsigned)e->metrics.highestRefcount,
                 cwal_type_id_name( e->metrics.highestRefcountType ));
  }
    
  { /* Recycling bin sizes... */
    int i, head = 0;
    unsigned rtotal = 0, binCount = 0, itemCount = 0;
    cwal_recycler * re;
    cwal_outputf(e, "\nValue/KVP Recycling: %u recycled, "
                 "%u recycler misses.\n",
                 (unsigned)e->metrics.valuesRecycled,
                 (unsigned)e->metrics.valuesRecycleMisses);
    for( i = 0; i < (int)cwalRecyclerInfo.recyclerCount; ++i){
      char const * specialCaseLabel = 0;
      re = e->recycler + i;
      if(!re->id) continue;
      if(re->hits || re->misses){
        if(!head){
          ++head;
          cwal_outputf(e, "Bin#   SlotSize  "
                       "Hits     Misses  Current#  Capacity "
                       "(recyclable for type(s))\n");
        }
        if(re->count){
          rtotal += re->id * re->count;
          itemCount += re->count;
          ++binCount;
        }
        cwal_outputf(e, "%3d    %-9d %-8d %-7d %-9d %d ",
                     i, re->id,
                     (int)re->hits, (int)re->misses,
                     re->count, re->maxLength);
        if(i==cwalRecyclerInfo.indexes[CWAL_TYPE_KVP]){
          specialCaseLabel = cwal_type_id_name(CWAL_TYPE_KVP);
        }else if(i==cwalRecyclerInfo.indexes[CWAL_TYPE_WEAKREF]){
          specialCaseLabel = cwal_type_id_name(CWAL_TYPE_WEAKREF);
        }else if(i==cwalRecyclerInfo.indexes[CWAL_TYPE_SCOPE]){
          specialCaseLabel = cwal_type_id_name(CWAL_TYPE_SCOPE);
        }
        if(specialCaseLabel){
          cwal_outputf(e, "\t(%s)\n", specialCaseLabel);
        }else{
          int x, x2;
          for( x2=0, x = CWAL_TYPE_UNDEF; x < CWAL_TYPE_end; ++x ){
            switch((cwal_type_id)x){
              case CWAL_TYPE_KVP:
              case CWAL_TYPE_WEAKREF:
              case CWAL_TYPE_SCOPE:
              case CWAL_TYPE_STRING:
                continue;
              default:
                if(re->id==(int)cwal_type_id_sizeof((cwal_type_id)x)){
                  cwal_outputf(e, "%s%s", (x2 ? ", " : "\t("),
                               cwal_type_id_name((cwal_type_id)x));
                  ++x2;
                }
            }
          }
          cwal_outputf(e, "%s\n", x2 ? ")" : "");
        }
      }
    }
    re = &e->reString;
    if(re->count){
      ++binCount;
      cwal_outputf(e, "strings:         %-9d%-8d%-9d %d\n",
                   (int)re->hits, (int)re->misses, re->count, re->maxLength);
      rtotal += cwal_type_id_sizeof(CWAL_TYPE_STRING) * re->count;
    }
    if(rtotal){
      cwal_outputf(e, "Currently holding %u bytes%s in "
                   "%u slot(s) in %u bin(s).\n",
                   rtotal,
                   re->count ? " (sans raw string bytes)" : "",
                   itemCount, binCount);
    }
  }

  { /* cwal_ptr_table memory... */
    int x;
    for(x = 0; x < 2; ++x ){ /* 0==string interning table, 1==weak ref table */
      char const * label = 0;
      cwal_ptr_table const * ptbl = 0;
      switch(x){
        case 0: label = "String interning tables:"; ptbl = &e->interned; break;
        case 1: label = "Weak ref tables:"; ptbl = &e->weakp; break;
      }
      assert(label && ptbl);
      if(ptbl->pg.head){
        unsigned i = 0;
        unsigned entryCount = 0;
        cwal_ptr_page const * h = ptbl->pg.head;
        unsigned const pgSize = sizeof(cwal_ptr_page) +
          (ptbl->hashSize * sizeof(void*));
        for( ; h ; h = h->next, ++i ){
          entryCount += h->entryCount;
        }
        grandTotal += i * pgSize;
        cwal_outputf(e, "\n%-25s %u page(s) of size %u (%u bytes) ==> %u bytes "
                     "holding %u entry(ies).\n",
                     label,
                     i, (unsigned)ptbl->hashSize, pgSize,
                     i * pgSize,
                     entryCount);
      }
      if(0==x && ptbl->pg.head){
        unsigned totalRefCount = 0;
        cwal_ptr_page const * pPage;
        cwal_value const * sv;
        cwal_string const * str;
        uint64_t size1 = 0, size2 = 0;
        for( pPage = ptbl->pg.head; pPage; pPage = pPage->next ){
          for(cwal_size_t i = 0; i < ptbl->hashSize; ++i ){
            sv = (cwal_value const *)pPage->list[i];
            str = CWAL_STR(sv);
            if(str){
              cwal_size_t const len = CWAL_STRLEN(str) + 1/*NUL*/;
              size1 += len;
              size2 += len * CWAL_REFCOUNT(sv);
              totalRefCount += CWAL_REFCOUNT(sv);
            }
          }
        }
        cwal_outputf(e, "Total size of all interned strings: %"PRIu64" bytes",
                     size1);
        cwal_outputf(e, ", adjusted for %u refcounts = %"PRIu64" bytes\n",
                     totalRefCount, size2);

      }
    }
  }

  if(e->metrics.clientMemTotal){
    cwal_outputf(e,"\nClient-reported memory: currently %u of %u total reported bytes\n",
                 (unsigned)e->metrics.clientMemCurrent,
                 (unsigned)e->metrics.clientMemTotal);
    grandTotal += e->metrics.clientMemTotal;
  }

  cwal_outputf(e, "\nAll built-in Values (sizeof(CWAL_BUILTIN_VALS)): "
               "%u static bytes\n",
               sizeof(CWAL_BUILTIN_VALS));
   
  if(e->buffer.capacity){
    cwal_outputf(e,"\nGeneral-purpose buffer capacity: %u bytes\n",
                 (unsigned)e->buffer.capacity);
    grandTotal += e->buffer.capacity;
  }
  if(!e->reChunk.metrics.peakChunkCount){
    cwal_outputf(e,"\nChunk recycler went unused.\n");
  }else{
    cwal_memchunk_recycler * re = &e->reChunk;
    cwal_outputf(e,"\nChunk recycler capacity=%u chunks, %u bytes.\n",
                 (unsigned)re->config.maxChunkCount,
                 (unsigned)re->config.maxTotalSize
                 );
    cwal_outputf(e,"Chunk requests: %u, Comparisons: %u, Misses: %u, "
                 "Smallest chunk: %u bytes, Largest: %u bytes\n",
                 (unsigned)re->metrics.requests,
                 (unsigned)re->metrics.searchComparisons,
                 (unsigned)re->metrics.searchMisses,
                 (unsigned)re->metrics.smallestChunkSize,
                 (unsigned)re->metrics.largestChunkSize
                 );
    cwal_outputf(e,"Reused %u chunk(s) totaling %u byte(s), "
                 "with peaks of %u chunk(s) and %u byte(s).\n",
                 (unsigned)re->metrics.totalChunksServed,
                 (unsigned)re->metrics.totalBytesServed,
                 (unsigned)re->metrics.peakChunkCount,
                 (unsigned)re->metrics.peakTotalSize
                 );
    cwal_outputf(e,"Running averages: chunk size: %u, "
                 "response size: %u\n",
                 (unsigned)re->metrics.runningAverageSize,
                 (unsigned)re->metrics.runningAverageResponseSize
                 );
    if(e->metrics.recoveredSlackCount){
      cwal_outputf(e, "Recovered %u byte(s) of \"slack\" memory "
                   "from %u block(s) via memory-capping metadata.\n",
                   (unsigned)e->metrics.recoveredSlackBytes,
                   (unsigned)e->metrics.recoveredSlackCount);
    }
    if(re->headCount){
      /* Output list of chunks... */
      cwal_memchunk_overlay * c;
      cwal_memchunk_overlay * prev = 0;
      int colCount = -1, sameSizeCount = 0, entriesPerLine = 8;
      cwal_outputf(e,"Currently contains %u chunk(s) "
                   "totaling %u byte(s).\n",
                   (unsigned)re->headCount, (unsigned)re->currentTotal
                   );
      cwal_outputf(e,"Current chunks, by size:\n");
      for( c = re->head; c; prev = c, c = c->next) {
        if(prev){
          assert(prev->size<=c->size);
        }
        if(prev && prev->size==c->size){
          ++sameSizeCount;
        }else{
          if(sameSizeCount){
            cwal_outputf( e, " (x%d)", sameSizeCount+1);
            sameSizeCount = 0;
          }
          if(++colCount == entriesPerLine){
            colCount = 0;
            cwal_output( e, ",\n\t", 3);
            prev = 0;
          }else if(!prev){
            cwal_output( e, "\t", 1);
          }
          cwal_outputf( e, "%s%u", prev ? ", " : "",
                        (unsigned)c->size);
        }
      }
      if(sameSizeCount){
        cwal_outputf( e, " (x%d)", sameSizeCount+1);
      }
      cwal_outputf(e,"\n");
    }
  }
  if(CWAL_ENABLE_BUILTIN_LEN1_ASCII_STRINGS)
  { /* e->metrics.len1StringsSaved[...] */
    int i, showTotals = 0;
    cwal_size_t len1TotalCount = 0;
    cwal_size_t len1TotalMem = 0;
    cwal_size_t const sizeOfStringBase
      = sizeof(cwal_value) + sizeof(cwal_string)
      /**
         The real alloced size for normal stings depends on
         CwalConsts.StringPadSize.
      */;
    cwal_outputf(e, "\nLength-1 ASCII string optimization "
                 "savings...");
    for( i = 0; i < 3; ++i ){
      cwal_size_t len1This;
      cwal_size_t sz;
      len1This = e->metrics.len1StringsSaved[i];
      if(len1This) ++showTotals;
      len1TotalCount += len1This;
      sz = (unsigned)len1This
        * (sizeOfStringBase
           + (i
              ? (unsigned)sizeof(unsigned char **)/*x-/z-strings*/
              : (unsigned)CwalConsts.StringPadSize/*normal strings*/));
      len1TotalMem += sz;
      cwal_outputf(e,"\n    %13s:\t%u alloc(s), %u bytes",
                   (0==i ? "plain strings"
                    : (1==i ? "x-strings" : "z-strings")),
                   (unsigned)len1This,
                   (unsigned)sz);
    }
    if(showTotals>1){
      if(len1TotalMem){
        cwal_outputf(e,"\nTotal savings: %u alloc(s), %u bytes\n",
                     (unsigned)len1TotalCount,
                     (unsigned)len1TotalMem);
      }else{
        cwal_outputf(e,"\n\tnone :`(\n");
      }
    }else{
      cwal_outputf(e,"\n");
    }
  }
  cwal_outputf(e,"\ncwal_engine instance ");
  if(e->allocStamp){
    unsigned int const sz = (unsigned)sizeof(cwal_engine);
    cwal_outputf(e,"is malloced: sizeof=%u\n", sz);
    grandTotal += sz;
  }else{
    cwal_outputf(e,"was not allocated by cwal_engine_init(). "
                 "sizeof(cwal_engine)=%u\n",
                 (unsigned)sizeof(cwal_engine));
  }


  if(CWAL_F_TRACK_MEM_SIZE & e->flags){
    cwal_outputf(e,"\nMemory size tracking/capping enabled:\n");
#define OUT(OPT) cwal_outputf(e,"\tvtab->memcap.%s = %u\n", #OPT, (unsigned)e->vtab->memcap.OPT)
    OUT(maxTotalAllocCount);
    OUT(maxTotalMem);
    OUT(maxConcurrentAllocCount);
    OUT(maxConcurrentMem);
    OUT(maxSingleAllocSize);
#undef OUT
#define OUT(OPT) cwal_outputf(e,"\tengine->memcap.%s = %u\n", #OPT, (unsigned)e->memcap.OPT)
    OUT(currentMem);
    OUT(currentAllocCount);
    OUT(peakMem);
    OUT(peakAllocCount);
    OUT(totalAllocCount);
    OUT(totalMem);
#undef OUT
    cwal_outputf(e,
                 "\tOver-allocation overhead =\n"
                 "\t\tengine->memcap.totalAllocCount (%u) "
                 "* sizeof(void*) (%u) =\n"
                 "\t\t%u bytes\n",
                 (unsigned)e->memcap.totalAllocCount,
                 (unsigned)sizeof(void*),
                 (unsigned)(e->memcap.totalAllocCount * sizeof(void*)));
    grandTotal += e->memcap.totalAllocCount * sizeof(void*);
  }


  if(grandTotal){
    cwal_outputf(e, "\nTotal bytes allocated for metrics-tracked resources: %"CWAL_SIZE_T_PFMT"\n",
                 (cwal_size_t)grandTotal);
  }

}

void cwal_dump_interned_strings_table( cwal_engine * const e,
                                       bool showEntries,
                                       cwal_size_t includeStrings ){
  enum { BufSize = 1024 };
  char buf[BufSize] = {0,};
  cwal_ptr_table * t = &e->interned;
  uint16_t i, x;
  int rc;
  cwal_output_f f = e->vtab->outputer.output;
  void * outState = e->vtab->outputer.state.data;
  uint32_t total = 0;
  unsigned int pageSize = sizeof(cwal_ptr_page)+(t->hashSize*sizeof(void*));
  unsigned int pageCount = 0;
  unsigned totalRefCount = 0;
  cwal_ptr_page const * p;
  assert( NULL != f );
  assert(0==buf[5]);
  assert(e && f);
  for( p = t->pg.head; p; p = p->next ) ++pageCount;
        
  rc = sprintf( buf, "Interned value table for engine@%p: ", (void*)e);
#define RC (cwal_size_t)rc
  f( outState, buf, RC );
  rc = sprintf( buf, " Page size=%"PRIu16" (%u bytes) "
                "count=%u (%u bytes)\n",
                t->hashSize, (unsigned)pageSize,
                (unsigned)pageCount,
                (unsigned)(pageSize * pageCount));
  f( outState, buf, RC );
  for( i = 0, p = t->pg.head; p; p = p->next, ++i ){
    uint16_t seen = 0;
    rc = sprintf( buf, "  Page #%d: entry count=%"PRIu16"\n",
                  (int)i+1, p->entryCount );
    f( outState, buf, RC );
    if(!showEntries){
      total += p->entryCount;
      continue;
    }
    for( x = 0;
         (x < t->hashSize); /*&& (seen<p->entryCount);*/
         ++x ){
      cwal_value * v = (cwal_value *)p->list[x];
      if(!v) continue;
      ++seen;
      rc = sprintf( buf, "    #%"PRIu16":\t %s"
                    "@%p [scope=%d] "
                    "refcount=%u",
                    x, v->vtab->typeName,
                    (void const *)v, (int)v->scope->level,
                    (unsigned)CWAL_REFCOUNT(v));
      f( outState, buf, RC );
      totalRefCount += CWAL_REFCOUNT(v);
      if(includeStrings){
        switch(v->vtab->typeID){
          case CWAL_TYPE_STRING:{
            cwal_string const * s = cwal_value_get_string(v);
            rc = sprintf(buf, " len=%u bytes=[", (unsigned)CWAL_STRLEN(s));
            f( outState, buf, RC );
            if(includeStrings >= CWAL_STRLEN(s)){
              f(outState, cwal_string_cstr(s), CWAL_STRLEN(s));
            }else{
              f(outState, cwal_string_cstr(s), includeStrings);
              f(outState, ">>>", 3);
            }
            f( outState, "]", 1 );
            break;
          }
          case CWAL_TYPE_INTEGER:
            rc = sprintf(buf, " value=%"CWAL_INT_T_PFMT,
                         cwal_value_get_integer(v));
            f( outState, buf, RC );
            break;
          case CWAL_TYPE_DOUBLE:
            rc = sprintf(buf, " value=%"CWAL_DOUBLE_T_PFMT,
                         cwal_value_get_double(v));
            f( outState, buf, RC );
            break;
          default:
            break;
                      
        }
      }
      f( outState, "\n", 1 );
    }
    assert( (seen == p->entryCount) && "Mis-management of p->entryCount detected." );
    if(seen){
      total += seen;
      rc = sprintf( buf, "  End Page #%d (%"PRIu16" entry(ies))\n",
                    (int)i+1, p->entryCount );
      f( outState, buf, RC );
    }
  }
  rc = sprintf(buf, "  Total entry count=%"PRIu32", with %u refcount point(s).\n",
               total, totalRefCount);
  f(outState, buf, RC );
#undef RC

}

void cwal_engine_tracer_close_FILE( void * filePtr ){
  FILE * f = (FILE*)filePtr;
  if(f && (f!=stdout) && (f!=stderr) && (f!=stdin)){
    fclose(f);
  }
}

#if CWAL_ENABLE_TRACE
static char const * cwal_tr_cstr( cwal_trace_flags_e ev ){
  switch(ev){
#define CASE(X) case CWAL_TRACE_##X: return #X
    CASE(NONE);
    CASE(ALL);

    CASE(GROUP_MASK);
        
    CASE(MEM_MASK);
    CASE(MEM_MALLOC);
    CASE(MEM_REALLOC);
    CASE(MEM_FREE);
    CASE(MEM_TO_RECYCLER);
    CASE(MEM_FROM_RECYCLER);
    CASE(MEM_TO_GC_QUEUE);

    CASE(VALUE_MASK);
    CASE(VALUE_CREATED);
    CASE(VALUE_SCOPED);
    CASE(VALUE_UNSCOPED);
    CASE(VALUE_CLEAN_START);
    CASE(VALUE_CLEAN_END);
    CASE(VALUE_CYCLE);
    CASE(VALUE_INTERNED);
    CASE(VALUE_UNINTERNED);
    CASE(VALUE_VISIT_START);
    CASE(VALUE_VISIT_END);
    CASE(VALUE_REFCOUNT);

    CASE(SCOPE_MASK);
    CASE(SCOPE_PUSHED);
    CASE(SCOPE_CLEAN_START);
    CASE(SCOPE_CLEAN_END);
    CASE(SCOPE_SWEEP_START);
    CASE(SCOPE_SWEEP_END);

    CASE(ENGINE_MASK);
    CASE(ENGINE_STARTUP);
    CASE(ENGINE_SHUTDOWN_START);
    CASE(ENGINE_SHUTDOWN_END);

    CASE(FYI_MASK);
    CASE(MESSAGE);
    CASE(ERROR_MASK);
    CASE(ERROR);

  };
  assert(!"MISSING cwal_tr_cstr() ENTRY!");
  return NULL;
#undef CASE
}
#endif
/*CWAL_ENABLE_TRACE*/

void cwal_engine_tracer_f_FILE( void * filePtr,
                                cwal_trace_state const * ev ){
#if CWAL_ENABLE_TRACE
  FILE * f = (FILE*)filePtr;
  if(!f) f = stdout;
  fprintf(f, "%s\tengine@%p scope#%"CWAL_SIZE_T_PFMT"@%p",
          cwal_tr_cstr(ev->event),
          (void const *)ev->e,
          ev->scope ? ev->scope->level : 0,
          (void const *)ev->scope );
  fprintf(f, "\t%s():%d",
          ev->cFunc, ev->cLine );

  if(ev->msg && *ev->msg) fprintf( f, "\n\t%s", ev->msg );
  fputc( '\n', f );

  if(ev->value){
    cwal_obase const * b = CWAL_VOBASE(ev->value);
    cwal_value const * v = ev->value;
        
    fprintf(f, "\t%s@%p", v->vtab->typeName, (void*)v);
    if(v->scope){
      fprintf(f, "(->scope#%"CWAL_SIZE_T_PFMT"@%p)",
              v->scope->level, (void*)v->scope);
    }
    fprintf(f, " refCount=%"CWAL_SIZE_T_PFMT, CWAL_REFCOUNT(v));
    if(b){
      fprintf( f, " flags=%02x", b->flags );
#if 0
      fprintf( f, " childCount=%"CWAL_SIZE_T_PFMT,
               b->list.count );
#endif
    }
    if( cwal_value_is_string(v) ){
      const cwal_size_t truncLen = 16;
      cwal_string const * s = cwal_value_get_string(v);
      if(s && CWAL_STRLEN(s)){
        fprintf( f, " strlen=%"CWAL_MIDSIZE_T_PFMT, CWAL_STRLEN(s) );
        if( CWAL_STRLEN(s) <= truncLen ){
          fprintf( f, " bytes=[%s]", cwal_string_cstr(s) );
        }else{
          fprintf( f, " bytes=[%.*s...] (truncated)", (int)truncLen, cwal_string_cstr(s) );
          /*fprintf( f, " bytes=[%s]", cwal_string_cstr(s) );*/
        }
      }else{
        fprintf( f, " (STILL INITIALIZING!)" );
      }
    }else if(cwal_value_is_integer(v)){
      fprintf( f, " value=[%"CWAL_INT_T_PFMT"]",cwal_value_get_integer(v));
    }else if(cwal_value_is_double(v)){
      fprintf( f, " value=[%"CWAL_DOUBLE_T_PFMT"]", cwal_value_get_double(v));
    }else if(cwal_value_is_bool(v)){
      fprintf( f, " value=[%s]", cwal_value_get_bool(v) ? "true" : "false" );
    }
    fputc( '\n', f );
  }
  if(ev->memory||ev->memorySize){
    fputc('\t',f);
    if( ev->memory) fprintf(f, "memory=%p ", ev->memory);
    if( ev->memorySize) fprintf(f, "(%"CWAL_SIZE_T_PFMT" bytes)",
                                ev->memorySize);
    fputc('\n',f);
  }

  fflush(f);
#else
  if(ev || filePtr){/*avoid unused param warning*/}
#endif
}

uint32_t cwal_engine_feature_flags( cwal_engine * e, int32_t mask ){
  if(!e) return -1;
  else{
    uint32_t const rc = e->flags & CWAL_FEATURE_MASK;
    if(mask>0){
      e->flags &= ~CWAL_FEATURE_MASK;
      e->flags |= CWAL_FEATURE_MASK & mask;
    }
    /** If auto-interning was disabled, free up the
        table memory.
    */
    if((CWAL_FEATURE_INTERN_STRINGS & rc)
       && !(CWAL_FEATURE_INTERN_STRINGS & e->flags)){
      cwal_ptr_table_destroy( e, &e->interned );
    }
    return rc;
  }
}

int32_t cwal_engine_trace_flags( cwal_engine * e, int32_t mask ){
#if !CWAL_ENABLE_TRACE
  if(e || mask){/*avoid unused param warning*/}
  return -1;
#else
  if(!e) return -1;
  else if(-1==mask) return e->trace.mask;
  else {
    int32_t const rc = e->trace.mask;
    e->trace.mask = mask;
    return rc;
  }
#endif
}
void cwal_trace( cwal_engine * e ){
#if CWAL_ENABLE_TRACE
  int32_t const m = e->trace.mask;
  /*MARKER("TRACE()? ev=%08x mask=%08x\n", e->trace.event, e->trace.mask);*/
    
  if(!
     ((m & (CWAL_TRACE_GROUP_MASK&e->trace.event))
      /*&& (e->trace.mask & (~CWAL_TRACE_GROUP_MASK&e->trace.event))*/
      )){
    goto end;
  }
  else if(!e->vtab->tracer.trace) goto end;
  else {
    if( e->trace.msg && !e->trace.msgLen && *e->trace.msg ){
      e->trace.msgLen = cwal_strlen(e->trace.msg);
    }
    e->vtab->tracer.trace( e->vtab->tracer.state, &e->trace );
    /* fall through */
  }
  end:
  e->trace = cwal_trace_state_empty;
  e->trace.e = e;
  e->trace.mask = m;
  assert(0==e->trace.msg);
  assert(0==e->trace.msgLen);
#else
  if(e){/*avoid unused param warning*/}
#endif
}

#define CwalRcFallbackCount 8
static struct {
  /* List of fallback handlers for cwal_rc_cstr() */
  unsigned short count;
  cwal_rc_cstr_f entries[CwalRcFallbackCount];
} CwalRcFallbacks = {
0, {0}
};

void cwal_rc_cstr_fallback(cwal_rc_cstr_f f){
  if(CwalRcFallbacks.count==CwalRcFallbackCount){
    assert(!"Too many cwal_rc_cstr_fallback() registrations.");
    fprintf(stderr,"%s:%d: "
            "Too many cwal_rc_cstr_fallback() registrations!\n",
            __FILE__, __LINE__);
    abort();
  }
  CwalRcFallbacks.entries[CwalRcFallbacks.count++] = f;
}
#undef CwalRcFallbackCount


char const * cwal_rc_cstr(int rc)
{
  char const * s = cwal_rc_cstr2(rc);
  return s ? s : "Unknown result code";
}

char const * cwal_rc_cstr2(int rc)
{
  switch(rc){
#define C(N) case N: return #N
    C(CWAL_RC_OK);
    C(CWAL_RC_ERROR);
    C(CWAL_RC_OOM);
    C(CWAL_RC_FATAL);

    C(CWAL_RC_CONTINUE);
    C(CWAL_RC_BREAK);
    C(CWAL_RC_RETURN);
    C(CWAL_RC_EXIT);
    C(CWAL_RC_EXCEPTION);
    C(CWAL_RC_ASSERT);

    C(CWAL_RC_MISUSE);
    C(CWAL_RC_NOT_FOUND);
    C(CWAL_RC_ALREADY_EXISTS);
    C(CWAL_RC_RANGE);
    C(CWAL_RC_TYPE);
    C(CWAL_RC_UNSUPPORTED);
    C(CWAL_RC_ACCESS);

    C(CWAL_RC_IS_VISITING);
    C(CWAL_RC_IS_VISITING_LIST);
    C(CWAL_RC_DISALLOW_NEW_PROPERTIES);
    C(CWAL_RC_DISALLOW_PROP_SET);
    C(CWAL_RC_DISALLOW_PROTOTYPE_SET);
    C(CWAL_RC_CONST_VIOLATION);
    C(CWAL_RC_LOCKED);
        
    C(CWAL_RC_CYCLES_DETECTED);
    C(CWAL_RC_DESTRUCTION_RUNNING);
    C(CWAL_RC_FINALIZED);
    C(CWAL_RC_HAS_REFERENCES);
    C(CWAL_RC_INTERRUPTED);
    C(CWAL_RC_CANCELLED);
    C(CWAL_RC_IO);
    C(CWAL_RC_CANNOT_HAPPEN);

    C(CWAL_RC_JSON_INVALID_CHAR);
    C(CWAL_RC_JSON_INVALID_KEYWORD);
    C(CWAL_RC_JSON_INVALID_ESCAPE_SEQUENCE);
    C(CWAL_RC_JSON_INVALID_UNICODE_SEQUENCE);
    C(CWAL_RC_JSON_INVALID_NUMBER);
    C(CWAL_RC_JSON_NESTING_DEPTH_REACHED);
    C(CWAL_RC_JSON_UNBALANCED_COLLECTION);
    C(CWAL_RC_JSON_EXPECTED_KEY);
    C(CWAL_RC_JSON_EXPECTED_COLON);
        
    C(CWAL_SCR_CANNOT_CONSUME);
    C(CWAL_SCR_INVALID_OP);
    C(CWAL_SCR_UNKNOWN_IDENTIFIER);
    C(CWAL_SCR_CALL_OF_NON_FUNCTION);
    C(CWAL_SCR_MISMATCHED_BRACE);
    C(CWAL_SCR_MISSING_SEPARATOR);
    C(CWAL_SCR_UNEXPECTED_TOKEN);
    C(CWAL_SCR_UNEXPECTED_EOF);
    C(CWAL_SCR_DIV_BY_ZERO);
    C(CWAL_SCR_SYNTAX);
    C(CWAL_SCR_EOF);
    C(CWAL_SCR_TOO_MANY_ARGUMENTS);
    C(CWAL_SCR_EXPECTING_IDENTIFIER);

    C(CWAL_RC_CLIENT_BEGIN1);
    C(CWAL_RC_CLIENT_BEGIN2);
    default:
      if(CwalRcFallbacks.count){
        unsigned short i = CwalRcFallbacks.count;
        char const * str = 0;
        for(; i>0 ; --i){
          str = CwalRcFallbacks.entries[i-1](rc);
          if(str) return str;
        }}
      return 0;
  }
#undef C
}

int cwal_prototype_base_set( cwal_engine * e, cwal_type_id t, cwal_value * proto ){
  if(!e) return CWAL_RC_MISUSE;
  else if(!cwal_engine_prototypes(e)) return CWAL_RC_OOM;
  else{
    assert((t>=0) && (t<CWAL_TYPE_end));
    switch(t){
      case CWAL_TYPE_XSTRING:
      case CWAL_TYPE_ZSTRING:
        t = CWAL_TYPE_STRING;
      default:
        break;
    }
    return cwal_array_set_v2(e->values.prototypes,
                             (cwal_midsize_t)t, proto, false );
  }
}

cwal_value * cwal_prototype_base_get( cwal_engine * e, cwal_type_id t ){
  if(!e || !e->values.prototypes) return NULL;
  else{
    assert((t>=0) && (t<CWAL_TYPE_end));
    switch(t){
      case CWAL_TYPE_XSTRING:
      case CWAL_TYPE_ZSTRING:
        t = CWAL_TYPE_STRING;
      default:
        break;
    }
    return cwal_array_get(e->values.prototypes, (cwal_size_t)t);
  }
}


int cwal_value_prototype_set( cwal_value * v, cwal_value * prototype ){
  cwal_obase * child = CWAL_VOBASE(v);
  cwal_obase * chPro = child ? CWAL_VOBASE(prototype) : NULL;
  cwal_obase * pBase;
  cwal_value * pCheck;
#if 0
  cwal_engine * e;
#endif
  if((prototype && !chPro) || CWAL_MEM_IS_BUILTIN(v)){
    return CWAL_RC_TYPE;
  }else if(!child || (child == chPro)){
    return CWAL_RC_MISUSE;
  }
  if(CWAL_CONTAINER_DISALLOW_PROTOTYPE_SET & child->containerFlags){
    return CWAL_RC_DISALLOW_PROTOTYPE_SET;
  }
  if( prototype == child->prototype ){
    return 0;
  }
#if 0
  e = v->scope ? v->scope->e : (prototype->scope ? prototype->scope->e : 0);
  assert(e);
#endif
  if(prototype){
    /* Ensure that v is nowhere in prototype's chain. */
    /*
      TODO: Strictly speaking, there's little reason to disallow
      non-containers as prototypes.

      Maybe refactor to use cwal_prototype_base_get() for
      non-containers and allow non-container prototypes.
    */
    for( pCheck = prototype; pCheck ; ){
      if(pCheck == v) return CWAL_RC_CYCLES_DETECTED;
      pBase = CWAL_VOBASE(pCheck);
      pCheck = (pBase?pBase->prototype:NULL);
    }
    cwal_value_ref2( v->scope->e, prototype );
    cwal_value_rescope( v->scope, prototype );
  }
  if(child->prototype){
    cwal_value_unref/* _from */(/* child->scope, */child->prototype );
  }
  child->prototype = prototype;
  return 0;
}

cwal_value * cwal_value_prototype_get(cwal_engine * e, cwal_value const * v){
  if(!v) return NULL;
  else{
    cwal_obase const * b = CWAL_VOBASE(v);
    if(b) return b->prototype;
    else{
      if(!e && v->scope) e = v->scope->e;
      return cwal_prototype_base_get(e, v->vtab->typeID);
    }
  }
}

bool cwal_value_derives_from( cwal_engine * e,
                              cwal_value const * v,
                              cwal_value const * p ){
  if(!e || !v || !p) return 0;
  else if(v==p) return 1;
  else {
    cwal_type_id const tid = v->vtab->typeID;
    while(v){
      cwal_obase const * base = CWAL_VOBASE(v);
      cwal_value const * theP =
        base ? base->prototype : cwal_prototype_base_get(e, tid);
      if(p==theP) return 1;
      else v = theP;
    }
    return 0;
  }
}


cwal_object * cwal_value_object_part( cwal_engine * e,
                                      cwal_value * v ){
  cwal_object * obj;
  do{
    if( (obj = CWAL_OBJ(v)) ) return obj;
    else v = cwal_value_prototype_get(e, v);
  }while(v);
  return NULL;
}

cwal_buffer * cwal_value_buffer_part( cwal_engine * e,
                                      cwal_value * v ){
  cwal_buffer_obj * f;
  do{
    if( (f = CWAL_BUFOBJ(v)) ) return &f->buf;
    else v = cwal_value_prototype_get(e, v);
  }while(v);
  return NULL;
}

cwal_native * cwal_value_native_part( cwal_engine * e,
                                      cwal_value * v,
                                      void const * typeID){
  cwal_native * n;
  do{
    if( (n = cwal_value_get_native(v))
        && (!typeID || typeID==n->typeID) ){
      return n;
    }
    v = cwal_value_prototype_get(e, v);
  }while(v);
  return NULL;
}

cwal_function * cwal_value_function_part( cwal_engine * e,
                                          cwal_value * v ){
  cwal_function * f;
  do{
    if( (f = cwal_value_get_function(v)) ) return f;
    else v = cwal_value_prototype_get(e, v);
  }while(v);
  return NULL;
}

cwal_array * cwal_value_array_part( cwal_engine * e,
                                    cwal_value * v ){
  cwal_array * a;
  do{
    if( (a = CWAL_ARRAY(v)) ) return a;
    else v = cwal_value_prototype_get(e, v);
  }while(v);
  return NULL;
}

cwal_exception * cwal_value_exception_part( cwal_engine * e,
                                            cwal_value * v ){
  cwal_exception * f;
  do{
    if( (f = cwal_value_get_exception(v)) ) return f;
    else v = cwal_value_prototype_get(e, v);
  }while(v);
  return NULL;
}

cwal_hash * cwal_value_hash_part( cwal_engine * e,
                                  cwal_value * v ){
  cwal_hash * f;
  do{
    if( (f = CWAL_HASH(v)) ) return f;
    else v = cwal_value_prototype_get(e, v);
  }while(v);
  return NULL;
}

cwal_string * cwal_value_string_part( cwal_engine * e,
                                      cwal_value * v ){
  cwal_string * f;
  do{
    if( (f = CWAL_STR(v)) ) return f;
    else v = cwal_value_prototype_get(e, v);
  }while(v);
  return NULL;
}

cwal_value * cwal_value_container_part( cwal_engine * e, cwal_value * v ){
  while(v){
    if(cwal_props_can(v)) return v;
    v = cwal_value_prototype_get(e, v);
  }
  return 0;
}

cwal_size_t cwal_list_reserve( cwal_engine * e, cwal_list * self, cwal_size_t n )
{
  assert(e);
  assert(self);
  if( !e || !self ) return 0;
  else if(0 == n)
  {
    if(0 == self->alloced) return 0;
    cwal_memchunk_add(e, self->list, self->alloced * sizeof(void*));
    self->list = NULL;
    self->alloced = self->count = 0;
    return 0;
  }
  if( self->alloced >= n ){
    return self->alloced;
  }
  METRICS_REQ_INCR(e,CWAL_TYPE_LISTMEM);
  if(!self->list){
    /* Check the e->reChunk recycler... */
    cwal_size_t reqSize = n * sizeof(void*);
    void** m = (void**)cwal_memchunk_request( e, &reqSize, 1000,
                                              "cwal_list_reserve(n)");
    if(m){
      self->alloced = reqSize/sizeof(void*);
      self->list = m;
      assert(self->alloced >= n);
      /*MARKER(("list recycler got: n=%d, reqSize=%d, self->alloced=%d\n",
        (int)n, (int)reqSize, (int)self->alloced));*/
      return self->alloced;
    }
    /* else fall through... */
  }
  {
    size_t const sz = sizeof(void*) * n;
    cwal_size_t i;
    void ** m = (void**)cwal_realloc( e, self->list, sz );
    if( ! m ) return self->alloced;
#if 0
    memset( m + self->alloced, 0, (sizeof(void*)*(n-self->alloced)));
#else
    for( i = self->count; i < n; ++i ) m[i] = NULL;
#endif
    ++e->metrics.allocated[CWAL_TYPE_LISTMEM]
      /* This isn't _quite_ pedantically true, as it counts
         reallocs as allocs, and we don't _really_ know if the
         system actually had to realloc the memory, but it'll
         do. We "could" only increment this is self->alloced==0,
         but that would also not be quite right. Better to err
         on the side of reporting more allocs (via realloc)
         here. */;
    e->metrics.bytes[CWAL_TYPE_LISTMEM] += (n - self->alloced) * sizeof(void*);
    self->alloced = n;
    self->list = m;
    return n;
  }
}

int cwal_list_append( cwal_engine * e, cwal_list * self, void* cp )
{
  if( !e || !self || !cp ) return CWAL_RC_MISUSE;
  else if( self->alloced > cwal_list_reserve(e, self, self->count+1) )
  {
    return CWAL_RC_OOM;
  }
  else
  {
    self->list[self->count++] = cp;

    if(self->count< self->alloced-1) self->list[self->count]=0;

    return 0;
  }
}

int cwal_list_visit( cwal_list * self, int order,
                     cwal_list_visitor_f visitor, void * visitorState )
{
  int rc = CWAL_RC_OK;
  if( self && self->count && visitor )
  {
    cwal_size_t i = 0;
    cwal_size_t pos = (order<0) ? self->count-1 : 0;
    int step = (order<0) ? -1 : 1;
    for( rc = 0; (i < self->count) && (0 == rc); ++i, pos+=step )
    {

      void* obj = self->list[pos];
      if(obj) rc = visitor( obj, visitorState );
      if( obj != self->list[pos] ){
        --i;
        if(order>=0) pos -= step;
      }
    }
  }
  return rc;
}


int cwal_list_visit_p( cwal_list * self, int order,
                       char shiftIfNulled,
                       cwal_list_visitor_f visitor, void * visitorState )
{
  int rc = CWAL_RC_OK;
  if( self && self->count && visitor )
  {
    int i = 0;
    int pos = (order<0) ? self->count-1 : 0;
    int step = (order<0) ? -1 : 1;
    for( rc = 0; (i < (int)self->count) && (0 == rc); ++i, pos+=step )
    {
      void* obj = self->list[pos];
      if(obj) {
        assert((order<0) && "TEST THAT THIS WORKS WITH IN-ORDER!");
        rc = visitor( &self->list[pos], visitorState );
        if( shiftIfNulled && !self->list[pos]){
          int x = pos;
          int const to = self->count-pos;

          assert( to < (int) self->alloced );
          for( ; x < to; ++x ) self->list[x] = self->list[x+1];
          if( x < (int)self->alloced ) self->list[x] = 0;
          --i;
          --self->count;
          if(order>=0) pos -= step;
        }
      }
    }
  }
  return rc;
}


cwal_value * cwal_value_from_arg(cwal_engine * e,
                                 char const *arg){
  cwal_size_t sLen;
  if(!e) return NULL;
  else if(!arg) return cwal_value_null();
  sLen = cwal_strlen(arg);
  if((('0'>*arg) || ('9'<*arg)) && ('+'!=*arg) && ('-'!=*arg)){
    goto do_string;
  }
  else{ /* try numbers... */
    cwal_int_t nI = 0;
    cwal_double_t nD = 0.0;
    if(0==cwal_cstr_to_int(arg, sLen, &nI)){
      return cwal_new_integer(e, (cwal_int_t)nI);
    }
#if 1
    /* cwal_cstr_to_double() might have some overflow-related
       problems (not certain - we've never made heavy use of
       doubles in client code!). If so, we can fall back
       to strtod(). */
    else if(0==cwal_cstr_to_double(arg, sLen, &nD)){
      return cwal_new_double(e, nD);
    }
#else
    else {
      char const * end = 0;
      double const val = strtod(arg, &end);
      if(!*end){
        return cwal_new_double(e, val);
      }
    }
#endif
    /* fall through and treat it like a string... */
  }
  do_string:
  if(4==sLen){
    if(0==strcmp("true",arg)) return cwal_value_true();
    if(0==strcmp("null",arg)) return cwal_value_null();
  }
  else if(5==sLen && 0==strcmp("false",arg)) return cwal_value_false();
  else if(9==sLen && 'u'==*arg && 0==strcmp("undefined",arg)) return cwal_value_undefined();
  else if(sLen>1 && (('\''==*arg && '\''==arg[sLen-1])
                     ||('"'==*arg && '"'==arg[sLen-1]))){
    /* trim quote marks */
    ++arg;
    sLen -= 2;
  }
  return cwal_new_string_value(e, arg, sLen);
}


int cwal_parse_argv_flags( cwal_engine * e,
                           int argc, char const * const * argv,
                           cwal_value ** tgt ){
  cwal_value * o = NULL;
  cwal_value * flags = NULL;
  cwal_array * nonFlags = NULL;
  cwal_array * tgtArray = 0;
  char const allocateTgt = (tgt && *tgt) ? 0 : 1;
  int rc = 0;
  int i = 0;
  if(!e || !tgt) return CWAL_RC_MISUSE;
  else if(*tgt && !cwal_props_can(*tgt)) return CWAL_RC_MISUSE;
  else if(argc<0) return CWAL_RC_RANGE;

  flags = cwal_new_object_value(e);
  if(!flags) return CWAL_RC_OOM;
  cwal_value_ref(flags);
  cwal_value_prototype_set(flags, 0);
  nonFlags = cwal_new_array(e);
  if(!nonFlags) {
    cwal_value_unref(flags);
    return CWAL_RC_OOM;
  }
  cwal_value_ref(cwal_array_value(nonFlags));
  o = allocateTgt ? cwal_new_object_value(e) : *tgt;
  if(!o){
    cwal_value_unref(flags);
    cwal_value_unref(cwal_array_value(nonFlags));
    return CWAL_RC_OOM;
  }
  if(allocateTgt) cwal_value_ref(o);
  else{
    tgtArray = cwal_value_get_array(o);
    if(tgtArray){
      cwal_array_reserve( tgtArray,
                          cwal_array_length_get(tgtArray)
                          + (unsigned)argc );
    }
  }
  rc = cwal_prop_set(o, "flags", 5, flags);
  if(rc){
    goto end;
  }
  rc = cwal_prop_set(o, "nonFlags", 8, cwal_array_value(nonFlags));
  if(rc){
    goto end;
  }
  for( i = 0; i < argc; ++i ){
    char const * arg = argv[i];
    char const * key = arg;
    char const * pos;
    cwal_value * k = NULL;
    cwal_value * v = NULL;
    char invertFlag = 0;
    if(!arg) continue;
    else if(tgtArray){
      v = cwal_new_string_value(e, arg, cwal_strlen(arg));
      if(v){
        cwal_value_ref(v);
        rc = cwal_array_append(tgtArray, v);
        cwal_value_unref(v);
      }else{
        rc = CWAL_RC_OOM;
      }
      if(rc) break;
    }        
    if('+' == *arg){
      invertFlag = 1;
      ++key;
    }
    else if('-' != *arg){
      v = cwal_new_string_value(e, arg, cwal_strlen(arg));
      if(!v){
        rc = CWAL_RC_OOM;
        break;
      }
      cwal_value_ref(v);
      rc = cwal_array_append(nonFlags, v);
      cwal_value_unref(v);
      if(rc) break;
      continue;
    }
    while('-'==*key) ++key;
    if(!*key) continue;
    pos = key;
    while( *pos && ('=' != *pos)) ++pos;
    k = cwal_new_string_value(e, key, pos-key);
    if(!k){
      rc = CWAL_RC_OOM;
      break;
    }
    cwal_value_ref(k);
    if(!*pos){ /** --key or +key */
      v = invertFlag
        ? cwal_value_false()
        : cwal_value_true();
    }else{ /** --key=...*/
      assert('=' == *pos);
      ++pos /*skip '='*/;
      v = cwal_value_from_arg(e, pos);
      if(!v){
        cwal_value_unref(k);
        rc = CWAL_RC_OOM;
        break;
      }
    }
    cwal_value_ref(v);
    rc=cwal_prop_set_v(flags, k, v);
    cwal_value_unref(k);
    cwal_value_unref(v);
    if(rc) break;
  }
  end:
  /* On success, o holds refs to flags/nonFlags via
     its properties. */
  cwal_value_unref(cwal_array_value(nonFlags));
  cwal_value_unref(flags);
  if(rc){
    if(allocateTgt) cwal_value_unref(o);
    else assert(o == *tgt);
  }else{
    if(allocateTgt){
      *tgt = o;
      cwal_value_unhand(o);
    }else{
      assert(o == *tgt);
    }
  }
  return rc;
}

bool cwal_strtok( char const ** inp, char separator,
                  char const ** end ){
  char const * pos = NULL;
  assert( inp && end && *inp );
  if( ! inp || !end ) return 0;
  else if( *inp == *end ) return 0;
  pos = *inp;
  if( !*pos )
  {
    *end = pos;
    return 0;
  }
  for( ; *pos && (*pos == separator); ++pos) { /* skip preceeding splitters */ }
  *inp = pos;
  for( ; *pos && (*pos != separator); ++pos) { /* find next splitter */ }
  *end = pos;
  return (pos > *inp) ? 1 : 0;
}

int cwal_prop_fetch_sub( cwal_value * obj, cwal_value ** tgt, char const * path, char sep )
{
  if( ! obj || !path ) return CWAL_RC_MISUSE;
  else if( !*path || !sep ) return CWAL_RC_RANGE;
  else{
    char const * beg = path;
    char const * end = NULL;
    unsigned int i, len;
    unsigned int tokenCount = 0;
    cwal_value * cv = NULL;
    cwal_value * curObj = obj;
    enum { BufSize = 128 };
    char buf[BufSize];
    memset( buf, 0, BufSize );

    while( cwal_strtok( &beg, sep, &end ) ){
      if( beg == end ) break;
      else{
        ++tokenCount;
        beg = end;
        end = NULL;
      }
    }
    if( 0 == tokenCount ) return CWAL_RC_RANGE;
    beg = path;
    end = NULL;
    for( i = 0; i < tokenCount; ++i, beg=end, end=NULL ){
      CWAL_UNUSED_VAR char const rc = cwal_strtok( &beg, sep, &end );
      assert( 1 == rc );
      assert( beg != end );
      assert( end > beg );
      len = end - beg;
      if( len > (BufSize-1) ) return CWAL_RC_RANGE;
      /* memset( buf, 0, len + 1 ); */
      memcpy( buf, beg, len );
      buf[len] = 0;
      cv = cwal_prop_get( curObj, buf, len );
      if( NULL == cv ) return CWAL_RC_NOT_FOUND;
      else if( i == (tokenCount-1) ){
        if(tgt) *tgt = cv;
        return 0;
      }
      else if( cwal_props_can(cv) ){
        curObj = cv;
      }
      /* TODO: arrays. Requires numeric parsing for the index. */
      else{
        return CWAL_RC_NOT_FOUND;
      }
    }
    assert( i == tokenCount );
    return CWAL_RC_NOT_FOUND;
  }
}

int cwal_prop_fetch_sub2( cwal_value * obj, cwal_value ** tgt, char const * path )
{
  if( ! obj || !path ) return CWAL_RC_MISUSE;
  else if( !*path || !*(1+path) ) return CWAL_RC_RANGE;
  else return cwal_prop_fetch_sub(obj, tgt, path+1, *path);
}


cwal_value * cwal_prop_get_sub( cwal_value * obj, char const * path, char sep )
{
  cwal_value * v = NULL;
  cwal_prop_fetch_sub( obj, &v, path, sep );
  return v;
}

cwal_value * cwal_prop_get_sub2( cwal_value * obj, char const * path )
{
  cwal_value * v = NULL;
  cwal_prop_fetch_sub2( obj, &v, path );
  return v;
}

int cwal_callback_hook_set(cwal_engine * e, cwal_callback_hook const * h ){
  if(!e) return CWAL_RC_MISUSE;
  e->cbHook = h ? *h : cwal_callback_hook_empty;
  return 0;
}

cwal_midsize_t cwal_strlen( char const * str ){
  return str ? (cwal_midsize_t)strlen(str) : 0U;
}

bool cwal_value_is_vacuum_proof( cwal_value const * v ){
  return v->scope
    ? CWAL_V_IS_VACUUM_SAFE(v)
    : CWAL_MEM_IS_BUILTIN(v);
}

int cwal_value_make_vacuum_proof( cwal_value * v, bool yes ){
  if(!v) return CWAL_RC_MISUSE;
  else if(CWAL_MEM_IS_BUILTIN(v)) return 0;
  else if(yes && CWAL_RCFLAG_HAS(v, CWAL_RCF_IS_VACUUM_PROOF)){
    return 0;
  }else if(!yes && !CWAL_RCFLAG_HAS(v, CWAL_RCF_IS_VACUUM_PROOF)){
    return 0;
  }else{
    cwal_scope * const s = v->scope;
    assert(s);
    if(yes) CWAL_RCFLAG_ON(v, CWAL_RCF_IS_VACUUM_PROOF);
    else CWAL_RCFLAG_OFF(v, CWAL_RCF_IS_VACUUM_PROOF);
    return cwal_scope_insert(s, v);
  }
}

#if 0
/* Though this fundamentally seems like a workable idea, and it
   initially seemed to work, it doesn't work. This needs to be
   revisited with fresh eyes at some point. Unfortunately, this
   is nigh impossible to debug because each step potentially
   changes tons of state.

   The desired algorithm here is essentially:

   - Fake a new top-most scope, F.

   - For each scope S, starting at the newest and working up, rescope
     all of S->mine.headSafe into F then clean S.

   - At the end, rescope everything from F to e->top and clean up F.

   However, even if all we do here is rescope values, without actually
   cleaning any, we break whcl unit tests. That's as-yet unexplained
   and "shouldn't" (i think) happen. It would seem to hint at a
   rescoping bug but if we had a genuine rescoping bug, it seems
   _highly_ likely that we'd have long since tripped over it.
*/
void cwal__vac(cwal_scope * const to, cwal_scope * const from){
  assert(from->level > to->level);
  //cwal_scope_sweep_r0(from);
  while(from->mine.headSafe) cwal_value_rescope(to, from->mine.headSafe);
  //while(from->mine.headObj) cwal_value_rescope(to, from->mine.headObj);
  //while(from->mine.headPod) cwal_value_rescope(to, from->mine.headPod);
  //while(from->mine.r0) cwal_value_rescope(to, from->mine.r0);
  /* TODO: figure out how we need to do to->props */
#if 0
  /* Reminder to self: cwal_scope_clean() handles the
     specially-propagating values specially (surprise). */
  //cwal_scope * const s = from->e->current;
  //from->e->current = to->parent;
  cwal_scope_clean(to->e, from);
  //from->e->current = s;
#endif
}

void cwal_engine_vacuum_v2( cwal_engine * const e ){
  cwal_scope * s;
  cwal_scope * const top = e->top;
  cwal_scope tmp = cwal_scope_empty;
  tmp.e = e;
  top->parent = &tmp;
  e->top = &tmp;
  cwal_scope * const tgt = top;
  //MARKER(("Don't use %s()!\n", __func__));
#if 1
  /* Put the specially-propagating values aside... */
  cwal_value * const pvP = e->values.propagating;
  cwal_scope * const psP = pvP ? pvP->scope : NULL;
  cwal_value * const pvE = e->values.exception;
  cwal_scope * const psE = pvE ? pvE->scope : NULL;
  e->values.propagating = e->values.exception = NULL;
  /* What follows is actually bad because one of these values
     may refer to state from another scope and take it with it,
     and then effectively down-scope it when we rescope it
     in a moment. Down-scoping is an absolute no-no in this
     engine. */
  if(pvP) cwal_value_rescope(&tmp, pvP);
  if(pvE) cwal_value_rescope(&tmp, pvE);
#endif
  for(s = e->current; s != top; s = s->parent){
    /* Move s's vacuum-safe values... */
    assert(s->parent);
    assert(!s->props && "Handling scope props is TODO.");
    cwal__vac(tgt, s);
  }
  e->top = s = top;
  top->parent = NULL;
  tmp.level = s->level + 1;
  tmp.parent = s;
  //MARKER(("Moving tmp scope stuff back into top.\n"));
#if 0
#if 0
  while(tmp.mine.headSafe){
#if !defined(NDEBUG)
    cwal_value const * vcheck = tmp.mine.headSafe;
#endif
    cwal_value_rescope( s, tmp.mine.headSafe );
#if !defined(NDEBUG)
    assert(tmp.mine.headSafe != vcheck);
    //assert(s->mine.headSafe == vcheck);
#endif
  }
#else
  cwal__vac(top, &tmp);
  assert(!tmp.mine.headSafe);
  assert(!tmp.mine.headObj);
  assert(!tmp.mine.headPod);
  assert(!tmp.mine.r0);
  //cwal_scope_sweep_r0(&tmp);
#endif
  //cwal_scope_clean(e, &tmp);
#endif
#if 1
  /* Restore the specially-propagating values... */
  if(pvP){
    MARKER(("Rescoping e->values.propagating.\n"));
    tmp.level = psP->level + 1; tmp.parent = psP;
    cwal_value_rescope(psP, pvP);
    e->values.propagating = pvP;
  }
  if(pvE){
    MARKER(("Rescoping e->values.exception.\n"));
    tmp.level = psE->level + 1; tmp.parent = psE;
    cwal_value_rescope(psE, pvE);
    e->values.exception = pvE;
  }
#endif
}
#endif /* cwal_engine_vacuum_v2() */

int cwal_scope_vacuum( cwal_scope * s, int * sweepCount ){
  cwal_engine * e = s->e;
  int rc = 0;
  cwal_scope s2 = cwal_scope_empty;
  cwal_size_t origCount = 0;
#define SHOWCOUNTS 0
#if SHOWCOUNTS
  cwal_value * check = 0;
#endif
  if(!e || !s) return CWAL_RC_MISUSE;
  assert(s->level>0);
  assert(s->parent || (1==s->level/*top scope*/));
  if(!s->props && !s->mine.headSafe){
    /* s has no safe properties, so we clean up everything... */
    if(sweepCount){
      *sweepCount = (int)cwal__scope_value_count(s);
    }
    cwal_scope_clean(e, s);
    return 0;
  }

#  define VLIST_COUNT(WHO) check = WHO; rc = 0; while(check){++rc; check=check->right;}(void)0
#  define VCOUNT(WHO) VLIST_COUNT(WHO); cwal_outputf(e, #WHO" count: %d\n", rc); rc = 0
  if(sweepCount){
    /* Count starting number of Values in the scope ... */
    origCount = cwal__scope_value_count(s);
  }
#if SHOWCOUNTS
  VCOUNT(s->mine.headPod);
  VCOUNT(s->mine.headObj);
  VCOUNT(s->mine.headSafe);
  VCOUNT(s->mine.r0);
#endif

  s2.level = s->level - 1;
  s2.parent = s->parent;
  s->parent = &s2;
  s2.e = e;
  if(s->props){
    cwal_value * const pv = s->props;
#if !defined(NDEBUG)
    cwal_obase * const obase = CWAL_VOBASE(pv);
    assert(obase->flags & CWAL_F_IS_PROP_STORAGE);
#endif
    cwal_value_rescope( &s2, pv );
    assert(pv->scope == &s2);
    s2.props = s->props;
    s->props = 0 /* transfer ref point */;
  }
  while(s->mine.headSafe){
#if !defined(NDEBUG)
    cwal_value const * vcheck = s->mine.headSafe;
#endif
    cwal_value_rescope( &s2, s->mine.headSafe );
#if !defined(NDEBUG)
    assert(s->mine.headSafe != vcheck);
#endif
  }
  /* Clean up, fake a lower/newer scope level,
     and move the vars back... */
  cwal_scope_clean( e, s )
    /* This eliminates non-vacuum-safe vars. This is also why a
       recursive vacuum _cannot_ reliably work. It's possible for
       newer scopes to hold refs/contain values which live in older
       scopes, but not the other way around. A vacuum on any scope
       other than the current (newest) one can pull values out from
       under any scope newer than the one being vacuumed.  Whether or
       not that's resolvable is unclear, but (1) the prognosis is not
       good and (2) at least it's finally (2022-04-08) clear _why_ it
       cannot currently work reliably except in a single-scope client.
    */;

#if SHOWCOUNTS
  VCOUNT(s->mine.headPod);
  VCOUNT(s->mine.headObj);
  VCOUNT(s->mine.headSafe);
  VCOUNT(s->mine.r0);

  VCOUNT(s2.mine.headPod);
  VCOUNT(s2.mine.headObj);
  VCOUNT(s2.mine.headSafe);
  VCOUNT(s2.mine.r0);
#endif

  /* Make s2 be s's child */
  s->parent = s2.parent;
  s2.parent = s;
  s2.level = s->level + 1;
  /* Move properties and vacuum-proof values back to s */
  if(s2.props){
    cwal_value_rescope( s, s2.props );
    assert(s2.props->scope == s);
    s->props = s2.props;
    s2.props = 0 /* transfer ref point */;
  }
  while(s2.mine.headSafe){
#if !defined(NDEBUG)
    cwal_value const * vcheck = s2.mine.headSafe;
#endif
    cwal_value_rescope( s, s2.mine.headSafe );
#if !defined(NDEBUG)
    assert(s2.mine.headSafe != vcheck);
#endif
  }

#if 0
  if(e->values.propagating && e->values.propagating->scope==&s2){
    cwal_value_rescope(s, e->values.propagating);
  }
  if(e->values.exception && e->values.exception->scope==&s2){
    cwal_value_rescope(s, e->values.exception);
  }
#else
  cwal_scope_clean(e, &s2) /* move specially-propagating values */;
#endif
  assert(0==s2.mine.r0);
  assert(0==s2.mine.headPod);
  assert(0==s2.mine.headObj);
  assert(0==s2.mine.headSafe);
  assert(0==s2.props);
  if(sweepCount){
    cwal_size_t newCount = cwal__scope_value_count(s);
#if SHOWCOUNTS
    VCOUNT(s->mine.headPod);
    VCOUNT(s->mine.headObj);
    VCOUNT(s->mine.headSafe);
    VCOUNT(s->mine.r0);

    VCOUNT(s2.mine.headPod);
    VCOUNT(s2.mine.headObj);
    VCOUNT(s2.mine.headSafe);
    VCOUNT(s2.mine.r0);
    MARKER(("origCount=%d, newCount=%d\n", (int)origCount, (int)newCount));
#endif
    /*
      Having more items than before is a sign that we imported
      more than we should have.
    */
    assert(newCount <= origCount);
    *sweepCount = (int)origCount - (int)newCount;
  }
#if 0
  cwal_scope_clean(e, &s2) /* not strictly necessary - s2 must
                              be empty by now or our universe's
                              physics are all wrong. */;
#endif
#undef SHOWCOUNTS
#undef VCOUNT
#undef VLIST_COUNT
  return rc;
}

int cwal_engine_vacuum( cwal_engine * e, int * sweepCount ){
  cwal_scope * s = e ? e->current : 0;
  if(!e || !s) return CWAL_RC_MISUSE;
  return cwal_scope_vacuum( s, sweepCount );
}

cwal_flags16_t cwal_container_flags_set( cwal_value * v, cwal_flags16_t flags ){
  cwal_obase * b = CWAL_VOBASE(v);
  if(!b) return 0;
  else{
    cwal_flags16_t const rc = b->containerFlags;
    b->containerFlags = flags;
    return rc;
  }
}

cwal_flags16_t cwal_container_flags_get( cwal_value const * v ){
  cwal_obase const * b = CWAL_VOBASE(v);
  return b ? b->containerFlags : 0;
}

cwal_flags16_t cwal_container_client_flags_set( cwal_value * v, cwal_flags16_t flags ){
  cwal_obase * b = CWAL_VOBASE(v);
  if(!b) return 0;
  else{
    cwal_flags16_t const rc = b->clientFlags;
    b->clientFlags = flags;
    return rc;
  }
}

cwal_flags16_t cwal_container_client_flags_get( cwal_value const * v ){
  cwal_obase const * b = CWAL_VOBASE(v);
  return b ? b->clientFlags : 0;
}


char * cwal_printfv_cstr( cwal_engine * e, char const * fmt, va_list vargs ){
  if( ! fmt ) return 0;
  else{
    cwal_buffer b = cwal_buffer_empty;
    if(cwal_buffer_printfv( e, &b, fmt, vargs )){
      cwal_buffer_clear(e, &b);
    }
    return (char*)b.mem;
  }
}

char * cwal_printf_cstr( cwal_engine * e, char const * fmt, ... ){
  va_list vargs;
  char * ret;
  va_start( vargs, fmt );
  ret = cwal_printfv_cstr( e, fmt, vargs );
  va_end( vargs );
  return ret;
}


int const * cwal_first_1000_primes(){
  static const int first1000Primes[1001] = {
  2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47,
  53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113,
  127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197,
  199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281,
  283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379,
  383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463,
  467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571,
  577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659,
  661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761,
  769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863,
  877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977,
  983, 991, 997, 1009, 1013, 1019, 1021, 1031, 1033, 1039, 1049, 1051, 1061, 1063, 1069,
  1087, 1091, 1093, 1097, 1103, 1109, 1117, 1123, 1129, 1151, 1153, 1163, 1171, 1181, 1187,
  1193, 1201, 1213, 1217, 1223, 1229, 1231, 1237, 1249, 1259, 1277, 1279, 1283, 1289, 1291,
  1297, 1301, 1303, 1307, 1319, 1321, 1327, 1361, 1367, 1373, 1381, 1399, 1409, 1423, 1427,
  1429, 1433, 1439, 1447, 1451, 1453, 1459, 1471, 1481, 1483, 1487, 1489, 1493, 1499, 1511,
  1523, 1531, 1543, 1549, 1553, 1559, 1567, 1571, 1579, 1583, 1597, 1601, 1607, 1609, 1613,
  1619, 1621, 1627, 1637, 1657, 1663, 1667, 1669, 1693, 1697, 1699, 1709, 1721, 1723, 1733,
  1741, 1747, 1753, 1759, 1777, 1783, 1787, 1789, 1801, 1811, 1823, 1831, 1847, 1861, 1867,
  1871, 1873, 1877, 1879, 1889, 1901, 1907, 1913, 1931, 1933, 1949, 1951, 1973, 1979, 1987,
  1993, 1997, 1999, 2003, 2011, 2017, 2027, 2029, 2039, 2053, 2063, 2069, 2081, 2083, 2087,
  2089, 2099, 2111, 2113, 2129, 2131, 2137, 2141, 2143, 2153, 2161, 2179, 2203, 2207, 2213,
  2221, 2237, 2239, 2243, 2251, 2267, 2269, 2273, 2281, 2287, 2293, 2297, 2309, 2311, 2333,
  2339, 2341, 2347, 2351, 2357, 2371, 2377, 2381, 2383, 2389, 2393, 2399, 2411, 2417, 2423,
  2437, 2441, 2447, 2459, 2467, 2473, 2477, 2503, 2521, 2531, 2539, 2543, 2549, 2551, 2557,
  2579, 2591, 2593, 2609, 2617, 2621, 2633, 2647, 2657, 2659, 2663, 2671, 2677, 2683, 2687,
  2689, 2693, 2699, 2707, 2711, 2713, 2719, 2729, 2731, 2741, 2749, 2753, 2767, 2777, 2789,
  2791, 2797, 2801, 2803, 2819, 2833, 2837, 2843, 2851, 2857, 2861, 2879, 2887, 2897, 2903,
  2909, 2917, 2927, 2939, 2953, 2957, 2963, 2969, 2971, 2999, 3001, 3011, 3019, 3023, 3037,
  3041, 3049, 3061, 3067, 3079, 3083, 3089, 3109, 3119, 3121, 3137, 3163, 3167, 3169, 3181,
  3187, 3191, 3203, 3209, 3217, 3221, 3229, 3251, 3253, 3257, 3259, 3271, 3299, 3301, 3307,
  3313, 3319, 3323, 3329, 3331, 3343, 3347, 3359, 3361, 3371, 3373, 3389, 3391, 3407, 3413,
  3433, 3449, 3457, 3461, 3463, 3467, 3469, 3491, 3499, 3511, 3517, 3527, 3529, 3533, 3539,
  3541, 3547, 3557, 3559, 3571, 3581, 3583, 3593, 3607, 3613, 3617, 3623, 3631, 3637, 3643,
  3659, 3671, 3673, 3677, 3691, 3697, 3701, 3709, 3719, 3727, 3733, 3739, 3761, 3767, 3769,
  3779, 3793, 3797, 3803, 3821, 3823, 3833, 3847, 3851, 3853, 3863, 3877, 3881, 3889, 3907,
  3911, 3917, 3919, 3923, 3929, 3931, 3943, 3947, 3967, 3989, 4001, 4003, 4007, 4013, 4019,
  4021, 4027, 4049, 4051, 4057, 4073, 4079, 4091, 4093, 4099, 4111, 4127, 4129, 4133, 4139,
  4153, 4157, 4159, 4177, 4201, 4211, 4217, 4219, 4229, 4231, 4241, 4243, 4253, 4259, 4261,
  4271, 4273, 4283, 4289, 4297, 4327, 4337, 4339, 4349, 4357, 4363, 4373, 4391, 4397, 4409,
  4421, 4423, 4441, 4447, 4451, 4457, 4463, 4481, 4483, 4493, 4507, 4513, 4517, 4519, 4523,
  4547, 4549, 4561, 4567, 4583, 4591, 4597, 4603, 4621, 4637, 4639, 4643, 4649, 4651, 4657,
  4663, 4673, 4679, 4691, 4703, 4721, 4723, 4729, 4733, 4751, 4759, 4783, 4787, 4789, 4793,
  4799, 4801, 4813, 4817, 4831, 4861, 4871, 4877, 4889, 4903, 4909, 4919, 4931, 4933, 4937,
  4943, 4951, 4957, 4967, 4969, 4973, 4987, 4993, 4999, 5003, 5009, 5011, 5021, 5023, 5039,
  5051, 5059, 5077, 5081, 5087, 5099, 5101, 5107, 5113, 5119, 5147, 5153, 5167, 5171, 5179,
  5189, 5197, 5209, 5227, 5231, 5233, 5237, 5261, 5273, 5279, 5281, 5297, 5303, 5309, 5323,
  5333, 5347, 5351, 5381, 5387, 5393, 5399, 5407, 5413, 5417, 5419, 5431, 5437, 5441, 5443,
  5449, 5471, 5477, 5479, 5483, 5501, 5503, 5507, 5519, 5521, 5527, 5531, 5557, 5563, 5569,
  5573, 5581, 5591, 5623, 5639, 5641, 5647, 5651, 5653, 5657, 5659, 5669, 5683, 5689, 5693,
  5701, 5711, 5717, 5737, 5741, 5743, 5749, 5779, 5783, 5791, 5801, 5807, 5813, 5821, 5827,
  5839, 5843, 5849, 5851, 5857, 5861, 5867, 5869, 5879, 5881, 5897, 5903, 5923, 5927, 5939,
  5953, 5981, 5987, 6007, 6011, 6029, 6037, 6043, 6047, 6053, 6067, 6073, 6079, 6089, 6091,
  6101, 6113, 6121, 6131, 6133, 6143, 6151, 6163, 6173, 6197, 6199, 6203, 6211, 6217, 6221,
  6229, 6247, 6257, 6263, 6269, 6271, 6277, 6287, 6299, 6301, 6311, 6317, 6323, 6329, 6337,
  6343, 6353, 6359, 6361, 6367, 6373, 6379, 6389, 6397, 6421, 6427, 6449, 6451, 6469, 6473,
  6481, 6491, 6521, 6529, 6547, 6551, 6553, 6563, 6569, 6571, 6577, 6581, 6599, 6607, 6619,
  6637, 6653, 6659, 6661, 6673, 6679, 6689, 6691, 6701, 6703, 6709, 6719, 6733, 6737, 6761,
  6763, 6779, 6781, 6791, 6793, 6803, 6823, 6827, 6829, 6833, 6841, 6857, 6863, 6869, 6871,
  6883, 6899, 6907, 6911, 6917, 6947, 6949, 6959, 6961, 6967, 6971, 6977, 6983, 6991, 6997,
  7001, 7013, 7019, 7027, 7039, 7043, 7057, 7069, 7079, 7103, 7109, 7121, 7127, 7129, 7151,
  7159, 7177, 7187, 7193, 7207, 7211, 7213, 7219, 7229, 7237, 7243, 7247, 7253, 7283, 7297,
  7307, 7309, 7321, 7331, 7333, 7349, 7351, 7369, 7393, 7411, 7417, 7433, 7451, 7457, 7459,
  7477, 7481, 7487, 7489, 7499, 7507, 7517, 7523, 7529, 7537, 7541, 7547, 7549, 7559, 7561,
  7573, 7577, 7583, 7589, 7591, 7603, 7607, 7621, 7639, 7643, 7649, 7669, 7673, 7681, 7687,
  7691, 7699, 7703, 7717, 7723, 7727, 7741, 7753, 7757, 7759, 7789, 7793, 7817, 7823, 7829,
  7841, 7853, 7867, 7873, 7877, 7879, 7883, 7901, 7907, 7919,
  0
#if 0
  , 0 /* causes an "excess elements in initializer" error if
         we've counted properly above. */
#endif
  };
  return first1000Primes;
}

cwal_build_info_t const * cwal_build_info(){
  static const cwal_build_info_t info = {

  /* cwal_size_t's */
  CWAL_SIZE_T_BITS,
  CWAL_INT_T_BITS,
  CWAL_STRLEN_MASK /* maxStringLength */,

  /* strings... */
  CWAL_VERSION_STRING,
  CWAL_CPPFLAGS,
  CWAL_CFLAGS,
  CWAL_CXXFLAGS,

  /* booleans... */

  CWAL_ENABLE_JSON_PARSER/* isJsonParserEnabled */,
#if defined(DEBUG)
  /* isDebug */
  1,
#else
  0,
#endif
  {/* sizeofs... */
  sizeof(CWAL_BUILTIN_VALS),
  sizeof(cwal_value),
  sizeof(void*)
  }
  };
  return &info;
}

cwal_value * cwal_build_info_object(cwal_engine * e){
  cwal_value * obj;
  cwal_value * setter;
  cwal_value * v;
  int rc;
  cwal_build_info_t const * const bi = cwal_build_info();
  char const * key;
  if(!e || !e->current) return 0;
  assert(e);
  obj = cwal_new_object_value(e);
  if(!obj) goto fail;
  cwal_value_ref(obj);
  setter = obj;
    
#define SET1                                            \
  if(!v) goto fail;                                     \
  cwal_value_ref(v);                                    \
  rc = cwal_prop_set(setter, key, cwal_strlen(key), v); \
  cwal_value_unref(v);                                  \
  v = 0;                                                \
  if(rc) goto fail

  /*************************************************************
      Numeric members...
  */
#define SET_INT(NAME,MEMBER)                                \
  v = cwal_new_integer(e, (cwal_int_t) bi->MEMBER);         \
  key = (char const *)NAME ? (char const *)NAME : #MEMBER;  \
  SET1

  SET_INT(0,size_t_bits);
  SET_INT(0,int_t_bits);
  SET_INT(0,maxStringLength);

  /*************************************************************
       Boolean members...
  */
#define SET_BOOL(NAME,MEMBER)                               \
  v = bi->MEMBER ? cwal_value_true() : cwal_value_false();  \
  key = NAME ? NAME : #MEMBER;                              \
  SET1

  SET_BOOL(0,isJsonParserEnabled);
  SET_BOOL(0,isDebug);

  /*************************************************************
      String members...
  */
#define SET_STR(NAME,MEMBER)                                            \
  v = cwal_new_string_value(e, bi->MEMBER, cwal_strlen(bi->MEMBER));    \
  key = NAME ? NAME : #MEMBER;                                          \
  SET1

  SET_STR(0,versionString);
  SET_STR(0,cppFlags);
  SET_STR(0,cFlags);
  SET_STR(0,cxxFlags);

  /*************************************************************
      sizeofs...
  */
  {
    cwal_value * so = cwal_new_object_value(e);
    if(!so) goto fail;
    cwal_value_ref(so);
    rc = cwal_prop_set(obj, "sizeofs", 7, so);
    cwal_value_unref(so);
    if(rc) goto fail;
    setter = so;
#define SO(X) SET_INT(#X, sizeofs.X)
    SO(voidPointer);
    SO(builtinValues);
    SO(cwalValue);
#undef SO
  }
    
#undef SET1
#undef SET_INT
#undef SET_BOOL
#undef SET_STR
  cwal_value_unhand(obj);
  return obj;
  fail:
  cwal_value_unref(obj);
  return 0;
}

int cwal_callback_f_build_info(cwal_callback_args const * args, cwal_value ** rv){
  return (*rv = cwal_build_info_object(args->engine))
    ? 0
    : CWAL_RC_OOM;
}

enum {
/**
   Indicates that one of the cwal_visit_XXX_begin() routines
   set a flag on its operand value.
*/
visitOpaqueMarkerYes = 0x01234567,
/**
   Indicates that one of the cwal_visit_XXX_begin() routines
   did not set a flag on its operand value.
*/
visitOpaqueMarkerNo =  0x76543210
};

void cwal_visit_props_begin( cwal_value * const v, int * const opaque ){
#if CWAL_OBASE_ISA_HASH
  cwal_obase * const ob = CWAL_VOBASE(v);
#endif
#if !defined(NDEBUG)
  assert(CWAL_VOBASE(v) && "Invalid use of cwal_visit_props_begin()");
#endif
  assert(v);
  assert(opaque);
  if(CWAL_V_IS_VISITING(v)){
    *opaque = visitOpaqueMarkerNo;
  }else{
    *opaque = visitOpaqueMarkerYes;
    CWAL_RCFLAG_ON(v,CWAL_RCF_IS_VISITING);
#if CWAL_OBASE_ISA_HASH
    ob->hprops.list.isVisiting = true;
#endif
  }
}

void cwal_visit_props_end( cwal_value * const v, int opaque ){
#if CWAL_OBASE_ISA_HASH
  cwal_obase * const ob = CWAL_VOBASE(v);
  assert(ob->hprops.list.isVisiting && "Else internal API misuse.");
#elif defined(DEBUG)
  assert(CWAL_VOBASE(v) && "Invalid use of cwal_visit_props_end()");
#endif
  assert(v);
  assert(visitOpaqueMarkerYes==opaque || visitOpaqueMarkerNo==opaque);
  if(visitOpaqueMarkerYes==opaque){
#if CWAL_OBASE_ISA_HASH
    ob->hprops.list.isVisiting = false;
#endif
    CWAL_RCFLAG_OFF(v,CWAL_RCF_IS_VISITING);
  }
}

void cwal_visit_list_begin( cwal_value * const v, int * const opaque ){
  cwal_hash * const h = CWAL_HASH(v);
  cwal_array * const a = h ? NULL : CWAL_ARRAY(v);
  assert(v);
  assert(CWAL_TYPE_TUPLE==v->vtab->typeID
         || CWAL_TYPE_ARRAY==v->vtab->typeID
         || CWAL_TYPE_HASH==v->vtab->typeID);
  assert(opaque);
  if(CWAL_V_IS_VISITING_LIST(v)){
    *opaque = visitOpaqueMarkerNo;
    if(h) assert(h->htable.list.isVisiting);
    if(a) assert(a->list.isVisiting);
  }else{
    *opaque = visitOpaqueMarkerYes;
    CWAL_RCFLAG_ON(v,CWAL_RCF_IS_VISITING_LIST);
    if(h){
      assert(!h->htable.list.isVisiting);
      h->htable.list.isVisiting = true;
    }
    else if(a){
      assert(!a->list.isVisiting);
      a->list.isVisiting = true;
    }
  }
}

void cwal_visit_list_end( cwal_value * const v, int opaque ){
  assert(v);
  assert(CWAL_TYPE_TUPLE==v->vtab->typeID
         || CWAL_TYPE_ARRAY==v->vtab->typeID
         || CWAL_TYPE_HASH==v->vtab->typeID);
  assert(visitOpaqueMarkerYes==opaque || visitOpaqueMarkerNo==opaque);
  if(visitOpaqueMarkerYes==opaque){
    cwal_hash * const h = CWAL_HASH(v);
    cwal_array * const a = h ? NULL : CWAL_ARRAY(v);
    CWAL_RCFLAG_OFF(v,CWAL_RCF_IS_VISITING_LIST);
    if(h){
      assert(h->htable.list.isVisiting);
      h->htable.list.isVisiting = false;
    }
    else if(a){
      assert(a->list.isVisiting);
      a->list.isVisiting = false;
    }
  }
}

int cwal_visit_acyclic_begin( cwal_value * v, int * opaque ){
  assert(v);
  assert(opaque);
  switch(v->vtab->typeID){
    case CWAL_TYPE_ARRAY:
    case CWAL_TYPE_OBJECT:
    case CWAL_TYPE_EXCEPTION:
    case CWAL_TYPE_TUPLE:
      if(CWAL_RCFLAG_HAS(v, CWAL_RCF_IS_VISITING_ACYCLIC)){
        *opaque = visitOpaqueMarkerNo;
        return CWAL_RC_CYCLES_DETECTED;
      }else{
        CWAL_RCFLAG_ON(v, CWAL_RCF_IS_VISITING_ACYCLIC);
        *opaque = visitOpaqueMarkerYes;
        return 0;
      }
      break /* not reached */;
    default:
      *opaque = visitOpaqueMarkerNo;
      return 0;
  }
}

void cwal_visit_acyclic_end( cwal_value * v, int opaque ){
  assert(v);
  assert(visitOpaqueMarkerYes==opaque || visitOpaqueMarkerNo==opaque);
  if(visitOpaqueMarkerYes==opaque){
    CWAL_RCFLAG_OFF(v,CWAL_RCF_IS_VISITING_ACYCLIC);
  }
}


int cwal_error_setv( cwal_engine * const e, cwal_error * const err_, int code,
                     char const * fmt, va_list args){
  int rc = 0;
  cwal_error * const err = err_ ? err_ : &e->err;
  err->msg.used = 0;
  err->code = code;
  if(CWAL_RC_OOM != (err->code = code)){
    if(!fmt || !*fmt) cwal_buffer_reuse(&err->msg);
    else rc = cwal_buffer_printfv(e, &err->msg, fmt, args);
  }else if(err->msg.capacity){
    /* make sure it's nul-terminated :/ */
    err->msg.mem[0] = 0;
  }
  return rc ? rc : code;
}

int cwal_error_set( cwal_engine * const e, cwal_error * const err,
                    int code, char const * fmt, ... ){
  int rc;
  va_list args;
  va_start(args,fmt);
  rc = cwal_error_setv(e, err, code, fmt, args);
  va_end(args);
  return rc;
}

int cwal_error_copy( cwal_engine * e, cwal_error const * src, cwal_error * dest ){
  int rc = 0;
  if(!src) src = &e->err;
  else if(!dest) dest = &e->err;
  if(src == dest) return CWAL_RC_MISUSE;
  cwal_error_reset( dest );
  dest->line = src->line;
  dest->col = src->col;
  dest->code = src->code;
  if(src->msg.used){
    rc = cwal_buffer_append(e, &dest->msg, src->msg.mem,
                            src->msg.used);
  }
  if(!rc && src->script.used){
    rc = cwal_buffer_append(e, &dest->script,
                            src->script.mem, src->script.used);
  }
  return rc;
}

void cwal_error_reset( cwal_error * const err ){
  cwal_buffer_reuse(&err->msg);
  cwal_buffer_reuse(&err->script);
  err->code = err->line = err->col = 0;
}

void cwal_engine_error_reset( cwal_engine * const e ){
  cwal__err_reset(e);
}

void cwal_error_uplift( cwal_error * const lower, cwal_error * const higher ){
  cwal_error const err = *lower;
  *lower = *higher;
  lower->line = lower->col = lower->code = 0;
  lower->msg.used = lower->script.used = 0U;
  *higher = err;
}

void cwal_error_clear( cwal_engine * e, cwal_error * err ){
  if(!err) err = &e->err;
  cwal_buffer_clear(e, &err->msg);
  cwal_buffer_clear(e, &err->script);
  *err = cwal_error_empty;
}

int cwal_error_get( cwal_error const * err, char const ** msg, cwal_size_t * msgLen ){
  if(err->code){
    if(msg) *msg = err->msg.used ? (char const *)err->msg.mem : 0;
    if(msgLen) *msgLen = err->msg.used;
  }
  return err->code;
}
int cwal_engine_error_get( cwal_engine const * e, char const ** msg, cwal_size_t * msgLen ){
  return cwal_error_get(&e->err, msg, msgLen);
}
static int cwal_error_add_script_props( cwal_engine * e,
                                        cwal_value * ex,
                                        char const * scriptName,
                                        int line, int col){
  int rc = 0;
  if(scriptName && *scriptName){
    cwal_value * snv = cwal_new_string_value(e,
                                             scriptName,
                                             cwal_strlen(scriptName));
    cwal_value_ref(snv);
    rc = snv
      ? cwal_prop_set(ex, "script", 6, snv)
      : CWAL_RC_OOM;
    cwal_value_unref(snv);
  }
  if(!rc && line>0){
    cwal_value * v = cwal_new_integer(e, line);
    if(!v) rc = CWAL_RC_OOM;
    else{
      cwal_value_ref(v);
      rc = cwal_prop_set(ex, "line", 4, v);
      cwal_value_unref(v);
      v = 0;
    }
    if(!rc){
      v = cwal_new_integer(e, col);
      if(!v) rc = CWAL_RC_OOM;
      else{
        cwal_value_ref(v);
        rc = cwal_prop_set(ex, "column", 6, v);
        cwal_value_unref(v);
        v = 0;
      }
    }
  }
  return rc;
}


cwal_value * cwal_error_exception( cwal_engine * e,
                                   cwal_error * err,
                                   char const * scriptName,
                                   int line, int col ){
  int rc = 0;
  cwal_value * msg;
  cwal_value * ex;
  if(!err) err = &e->err;
  if(line<=0){
    line = err->line;
    col = err->col;
  }
  if(err->msg.used){
    msg = cwal_string_value(cwal_buffer_to_zstring(e, &err->msg));
  }else{
    msg = cwal_string_value(cwal_new_stringf(e, "Error #%d (%s).",
                                             err->code,
                                             cwal_rc_cstr(err->code)));
  }
  if(msg) cwal_value_ref(msg);
  ex = msg
    ? cwal_new_exception_value(e, err->code, msg)
    : 0;
  if(msg){
    cwal_value_unref(msg);
    msg = 0;
  }
  if(!ex){
    rc = CWAL_RC_OOM;
  }else{
    cwal_value_ref(ex);
    assert(cwal_value_is_exception(ex));
    if(!scriptName && err->script.used) scriptName = (char const *)err->script.mem;
    rc = cwal_error_add_script_props(e, ex, scriptName, line, col);
    if(rc) {
      cwal_value_unref(ex);
      ex = 0;
    }else{
      cwal_value_unhand(ex);
    }
  }
  cwal_error_reset(err);
  return ex;
}

int cwal_error_throw( cwal_engine * e, cwal_error * err,
                      char const * script,
                      int line, int col ){
  cwal_value * const ex = cwal_error_exception(e, err, script, line, col);
  if(!ex) return CWAL_RC_OOM;
  else{
    int rc;
    /* cwal_dump_val(ex, "exception"); */
    rc = cwal_exception_set(e, ex);
    assert(CWAL_RC_EXCEPTION==rc);
    assert(1==cwal_value_refcount(ex));
    return rc;
  }
}

cwal_error * cwal_engine_error( cwal_engine * const e ){
  return &e->err;
}
#if 0
cwal_error const * cwal_engine_error_c( cwal_engine const * e ){
  return &e->err;
}
#endif

cwal_kvp const * cwal_obase_kvp_iter_init( cwal_value * const v,
                                           cwal_obase_kvp_iter * const oks ){
  cwal_obase * const base = cwal_value_obase(v);
  assert(CWAL_V_IS_VISITING(v)
         || CWAL_V_IS_VISITING_LIST(v)
         || CWAL_V_IS_RESCOPING(v));
  oks->v = v;
#if CWAL_OBASE_ISA_HASH
  oks->base = base;
  oks->li = &base->hprops.list;
  oks->ndx = 0;
  oks->current = NULL;
  return cwal_obase_kvp_iter_next(oks);
#else
  return oks->current = base->kvp;
#endif
}

cwal_kvp const * cwal_obase_kvp_iter_next( cwal_obase_kvp_iter * const oks ){
  assert(CWAL_V_IS_VISITING(oks->v)
         || CWAL_V_IS_VISITING_LIST(oks->v)
         || CWAL_V_IS_RESCOPING(oks->v));
#if CWAL_OBASE_ISA_HASH
  //assert(CWAL_V_IS_VISITING(CWAL_VALPART(oks->base)));
  assert(oks->base->hprops.list.isVisiting);
  if(oks->current && oks->current->right){
    return oks->current = oks->current->right;
  }else if(oks->ndx>=oks->base->hprops.hashSize){
    return NULL;
  }
  for(cwal_midsize_t i = oks->ndx; i < oks->base->hprops.hashSize; ++i){
    if(oks->li->list[i]){
      oks->current = (cwal_kvp const *)oks->li->list[i];
      oks->ndx = i+1;
      return oks->current;
    }
  }
  oks->ndx = oks->base->hprops.hashSize;
  return NULL;
#else
  return oks->current ? (oks->current = oks->current->right) : NULL;
#endif
}

int cwal_errno_to_cwal_rc(int errNo, int dflt){
  switch(errNo ? errNo : errno){
    case 0: return CWAL_RC_OK;
    case EACCES: return CWAL_RC_ACCESS;
    case EEXIST: return CWAL_RC_ALREADY_EXISTS;
    case EFBIG: return CWAL_RC_RANGE;
    case EINTR: return CWAL_RC_INTERRUPTED
      /* potential semantic conflict with
         s2_engine::flags::interrupted */
      ;
    case EINVAL: return CWAL_RC_MISUSE;
    case EIO: return CWAL_RC_IO;
    case EISDIR: return CWAL_RC_TYPE;
    case ELOOP: return CWAL_RC_CYCLES_DETECTED;
    case EMLINK: return CWAL_RC_RANGE;
    case ENAMETOOLONG: return CWAL_RC_RANGE;
    case ENFILE: return CWAL_RC_RANGE;
    case ENOENT: return CWAL_RC_NOT_FOUND;
    case ENOMEM: return CWAL_RC_OOM;
    case ENOTDIR: return CWAL_RC_TYPE;
    case ENOTTY: return CWAL_RC_TYPE;
    case EMFILE: return CWAL_RC_RANGE;
    case EPIPE: return CWAL_RC_IO;
    case ERANGE: return CWAL_RC_RANGE;
    case EROFS: return CWAL_RC_ACCESS;
    case ESPIPE: return CWAL_RC_UNSUPPORTED;
    default: return dflt;
  }
}

int cwal_new_propref2( cwal_propref_e propType,
                       cwal_flags16_t flags,
                       cwal_value * const container, cwal_value * const key,
                       cwal_value * const xtra,
                       cwal_propref ** rv){
  if(!container || !key || !rv) return CWAL_RC_MISUSE;
  switch(propType){
    case CWAL_PROPREF_PROPERTY: {
      handle_prop:
      if(!CWAL_VOBASE(container) || !cwal_prop_key_can(key)) return CWAL_RC_TYPE;
      cwal_engine * const e = cwal_value_engine(container);
      assert(e);
      cwal_value * const v = cwal_value_new(e, CWAL_TYPE_PROPREF, 0);
      if(!v) return CWAL_RC_OOM;
      cwal_propref * const p = CWAL__V2PROPREF(v);
      p->flags = propType & CWAL__PROPREF_MASK_TYPE;
      if(CWAL_PROPREF_F_WEAK & flags){
        cwal_weakref * const r = cwal_weakref_new(container);
        if(!r){
          cwal_refunref( v );
          return CWAL_RC_OOM;
        }
        p->flags |= CWAL__PROPREF_MASK_WEAK;
        p->target.w = r;
      }else{
        cwal_value_rescope(v->scope, container);
        cwal_value_ref( (p->target.c = container) );
      }
      cwal_value_rescope(v->scope, key);
      cwal_value_ref( (p->key = key) );
      if(xtra){
        cwal_value_rescope(v->scope, xtra);
        cwal_value_ref( (p->xtra = xtra) );
      }
      if(CWAL_PROPREF_F_READONLY & flags){
        p->flags |= CWAL__PROPREF_MASK_RO;
      }
      *rv = p;
      return 0;
    }
    case CWAL_PROPREF_INTERCEPTOR: {
      if(!cwal_value_is_function(xtra)) return CWAL_RC_TYPE;
      goto handle_prop;
    }

#define IS__LIST                                            \
      (CWAL_ARRAY(container) && cwal_value_is_integer(key))
    case CWAL_PROPREF_LIST:
      if(IS__LIST) goto handle_prop;
      return CWAL_RC_TYPE;
    case CWAL_PROPREF_AUTO:
      propType = (IS__LIST) ? CWAL_PROPREF_LIST : CWAL_PROPREF_PROPERTY;
      goto handle_prop;
#undef IS__LIST
  }
  assert(!"not reached");
  return CWAL_RC_ASSERT;
}


cwal_propref * cwal_new_propref( cwal_propref_e propType,
                                 cwal_flags16_t flags,
                                 cwal_value * const container, cwal_value * const key,
                                 cwal_value * const xtra ){
  cwal_propref * p = NULL;
  cwal_new_propref2(propType, flags, container, key, xtra, &p);
  return p;
}

cwal_value * cwal_new_propref_value( cwal_propref_e refType,
                                     cwal_flags16_t flags,
                                     cwal_value * const container,
                                     cwal_value * const key,
                                     cwal_value * const xtra ){
  cwal_propref * p = NULL;
  cwal_new_propref2(refType, flags, container, key, xtra, &p);
  return p ? CWAL_VALPART(p) : NULL;
}

cwal_propref * cwal_value_get_propref( cwal_value * const v ){
  return CWAL__V2PROPREF(v);
}

cwal_value * cwal_propref_key(cwal_propref const * const p) {
  return p->key;
}

cwal_value * cwal_propref_container(cwal_propref const * const p){
  return cwal__propref_is_weak(p)
    ? cwal_weakref_value(p->target.w)
    : p->target.c;
}

cwal_value * cwal_propref_value(cwal_propref * const p){
  return CWAL_VALPART(p);
}

bool cwal_propref_is_readonly(cwal_propref const * const p){
  return !!(p->flags & CWAL__PROPREF_MASK_RO);
}

/**
   The following LIKE/GLOB comparison bits were all ripped directly
   from sqlite3 many moons ago. The names have been changed only to
   fit this project, not to obfuscate their origins. They were
   subsequently touched up to support non-ASCII UTF8 inputs.
*/
/*
** A structure defining how to do GLOB-style comparisons.
** Taken from sqlite3.
*/
struct cwal__compareInfo {
  unsigned char matchAll;          /* "*" or "%" */
  unsigned char matchOne;          /* "?" or "_" */
  unsigned char matchSet;          /* "[" or 0 */
  unsigned char noCase;            /* true to ignore case differences */
};

static const struct cwal__compareInfo cwal__compareInfo_glob = {'*', '?', '[', 0};
static const struct cwal__compareInfo cwal__compareInfo_globnc = {'*', '?', '[', 1};
static const struct cwal__compareInfo cwal__compareInfo_like = {'%', '_', 0, 0};
static const struct cwal__compareInfo cwal__compareInfo_likenc = {'%', '_', 0, 1};

/**
   Optimized cwal_utf8_read_char1() which skips the function call if
   A[0] is an ASCII character.
*/
#define Utf8Read(A)               (A[0]<0x80?*(A++):cwal_utf8_read_char1(&A))
/*
** Assuming zIn points to the first byte of a UTF-8 character,
** advance zIn to point to the first byte of the next UTF-8 character.
*/
#define Utf8SkipChar(zIn) \
  if( (*(zIn++))>=0xc0 ){                              \
    while( (*zIn & 0xc0)==0x80 ){ zIn++; }             \
  } (void)0

/*
** Taken from sqlite3.
**
** Compare two UTF-8 strings for equality where the first string is
** a GLOB or LIKE expression.  Return values:
**
**    CWAL_RC_OK:            Match
**    CWAL_RC_NOT_FOUND:          No match
**    CWAL_RC_NOT_FOUND:  No match in spite of having * or % wildcards.
**
** Globbing rules:
**
**      '*'       Matches any sequence of zero or more characters.
**
**      '?'       Matches exactly one character.
**
**     [...]      Matches one character from the enclosed list of
**                characters.
**
**     [^...]     Matches one character not in the enclosed list.
**
** With the [...] and [^...] matching, a ']' character can be included
** in the list by making it the first character after '[' or '^'.  A
** range of characters can be specified using '-'.  Example:
** "[a-z]" matches any single lower-case letter.  To match a '-', make
** it the last character in the list.
**
** Like matching rules:
** 
**      '%'       Matches any sequence of zero or more characters
**
***     '_'       Matches any one character
**
**      Ec        Where E is the "esc" character and c is any other
**                character, including '%', '_', and esc, match exactly c.
**
** The comments within this routine usually assume glob matching.
**
** This routine is usually quick, but can be N**2 in the worst case.
*/
static int patternCompare(
  const unsigned char *zPattern,              /* The glob pattern */
  const unsigned char *zString,               /* The string to compare against the glob */
  const struct cwal__compareInfo *pInfo, /* Information about how to do the compare */
  unsigned int matchOther                   /* The escape char (LIKE) or '[' (GLOB) */
){
  unsigned int c, c2;                             /* Next pattern and input string chars */
  const unsigned int matchOne = pInfo->matchOne;  /* "?" or "_" */
  const unsigned int matchAll = pInfo->matchAll;  /* "*" or "%" */
  const unsigned char noCase = pInfo->noCase;       /* True if uppercase==lowercase */
  const unsigned char *zEscaped = 0;          /* One past the last escaped input char */
  unsigned int cx;
  int bMatch;
  
  while( (c = Utf8Read(zPattern))!=0 ){
    if( c==matchAll ){  /* Match "*" */
      /* Skip over multiple "*" characters in the pattern.  If there
      ** are also "?" characters, skip those as well, but consume a
      ** single character of the input string for each "?" skipped */
      while( (c=Utf8Read(zPattern)) == matchAll || c == matchOne ){
        if( c==matchOne && cwal_utf8_read_char1(&zString)==0 ){
          return CWAL_RC_NOT_FOUND/*SQLITE_NOWILDCARDMATCH*/;
        }
      }
      if( c==0 ){
        return CWAL_RC_OK;   /* "*" at the end of the pattern matches */
      }else if( c==matchOther ){
        if( pInfo->matchSet==0 ){
          c = cwal_utf8_read_char1(&zPattern);
          if( c==0 ) return CWAL_RC_NOT_FOUND /*SQLITE_NOWILDCARDMATCH*/;
        }else{
          /* "[...]" immediately follows the "*".  We have to do a slow
          ** recursive search in this case, but it is an unusual case. */
          assert( matchOther<0x80 );  /* '[' is a single-byte character */
          while( *zString ){
            bMatch = patternCompare(&zPattern[-1],zString,pInfo,matchOther);
            if( bMatch!=CWAL_RC_NOT_FOUND ) return bMatch;
            Utf8SkipChar(zString);
          }
          return CWAL_RC_NOT_FOUND;
        }
      }

      /* At this point variable c contains the first character of the
      ** pattern string past the "*".  Search in the input string for the
      ** first matching character and recursively continue the match from
      ** that point.
      **
      ** For a case-insensitive search, set variable cx to be the same
      ** as c but case-swapped and search the input string for either
      ** c or cx.
      */
      if( noCase ){
        cx = cwal_utf8_char_toupper(c);
        c = cwal_utf8_char_tolower(c);
      }else{
        cx = c;
      }
      while( (c2 = Utf8Read(zString))!=0 ){
        if( c2!=c && c2!=cx ) continue;
        bMatch = patternCompare(zPattern,zString,pInfo,matchOther);
        if( bMatch!=CWAL_RC_NOT_FOUND ) return bMatch;
      }
      return CWAL_RC_NOT_FOUND;
    }
    if( c==matchOther ){
      if( pInfo->matchSet==0 ){
        c = cwal_utf8_read_char1(&zPattern);
        if( c==0 ) return CWAL_RC_NOT_FOUND;
        zEscaped = zPattern;
      }else{
        unsigned int prior_c = 0;
        int seen = 0;
        int invert = 0;
        c = cwal_utf8_read_char1(&zString);
        if( c==0 ) return CWAL_RC_NOT_FOUND;
        if(noCase) c = cwal_utf8_char_tolower(c);

        c2 = cwal_utf8_read_char1(&zPattern);
        if( c2=='^' ){
          invert = 1;
          c2 = cwal_utf8_read_char1(&zPattern);
        }
        if( c2==']' ){
          if( c==']' ) seen = 1;
          c2 = cwal_utf8_read_char1(&zPattern);
        }
        if(c2 && noCase) c2 = cwal_utf8_char_tolower(c2);
        while( c2 && c2!=']' ){
          if( c2=='-' && zPattern[0]!=']' && zPattern[0]!=0 && prior_c>0 ){
            c2 = cwal_utf8_read_char1(&zPattern);
            if(c2 && noCase) c2 = cwal_utf8_char_tolower(c2);
            if( c>=prior_c && c<=c2 ) seen = 1;
            prior_c = 0;
          }else{
            if( c==c2 ){
              seen = 1;
            }
            prior_c = c2;
          }
          c2 = cwal_utf8_read_char1(&zPattern);
          if(c2 && noCase) c2 = cwal_utf8_char_tolower(c2);
        }
        if( c2==0 || (seen ^ invert)==0 ){
          return CWAL_RC_NOT_FOUND;
        }
        continue;
      }
    }
    c2 = Utf8Read(zString);
    if( c==c2 || (noCase &&
                  cwal_utf8_char_tolower(c)
                  ==cwal_utf8_char_tolower(c2)) ){
      continue;
    } else if( c==matchOne && zPattern!=zEscaped && c2!=0 ){
      continue;
    }
    return CWAL_RC_NOT_FOUND;
  }
  return *zString==0 ? CWAL_RC_OK : CWAL_RC_NOT_FOUND;
}

#undef Utf8Read
#undef Utf8SkipChar

bool cwal_glob_matches_cstr(const char *zGlob,
                            const char *z,
                            enum cwal_glob_style_e globStyle){
  unsigned int esc;
  struct cwal__compareInfo const * ci;
  if(!zGlob || !*zGlob || !z) return false;
  switch(globStyle){
    case CWAL_GLOB_LIKE_NOCASE:
      ci = &cwal__compareInfo_likenc;
      esc = 0;
      break;
    case CWAL_GLOB_LIKE:
      ci = &cwal__compareInfo_like;
      esc = 0;
      break;
    case CWAL_GLOB_WILDCARD:
      ci = &cwal__compareInfo_glob;
      esc = '[';
      break;
    case CWAL_GLOB_WILDCARD_NOCASE:
      ci = &cwal__compareInfo_globnc;
      esc = '[';
      break;
    default:
      ci = 0;
      esc = 0;
      MARKER(("Invalid glob style: %d\n", globStyle));
      assert(!"invalid cwal_glob_style");
      return false;
  }
  return patternCompare((unsigned char const *)zGlob,
                        (unsigned char const *)z,
                        ci, esc)
    ? 0 : 1;
}



#undef MARKER
#undef cwal__fatal
#undef cwal_string_empty_m
#undef cwal_obase_empty_m
#undef cwal_array_empty_m
#undef cwal_kvp_empty_m
#undef cwal_object_empty_m
#undef cwal_value_vtab_empty_m
#undef cwal_value_empty_m
#undef CWAL_VVPCAST
#undef CWAL_VALPART
#undef CWAL_INT
#undef CWAL_DBL
#undef CWAL_BOOL
#undef CWAL_STR
#undef CWAL_OBASE
#undef CWAL_VOBASE
#undef CWAL_OBJ
#undef CWAL_ARRAY
#undef CWAL_HASH
#undef CWAL_TUPLE
#undef CWAL__V2PROPREF
#undef CWAL_V2NATIVE
#undef dump_val
#undef CWAL_MEM_IS_BUILTIN
#undef V_SEEMS_OK
#undef CWAL_VENGINE
#undef SETUP__ARRAY_ARGS
#undef CWAL_STRLEN
#undef CWAL_STRLEN_MASK
#undef CWAL_STR_ISX
#undef CWAL_STR_ISZ
#undef CWAL_STR_ISXZ
#undef CWAL_STR_XMASK
#undef CWAL_STR_ZMASK
#undef CWAL_STR_ASCII_MASK
#undef CWAL_STR_ISASCII
#undef CWAL_KVP_TRY_SORTING
#undef TRY_CLONE_SHARING
#undef COMPARE_TYPE_IDS
#undef CWAL_V_IS_VISITING
#undef CWAL_V_IS_VISITING_LIST
#undef METRICS_REQ_INCR
#undef CWAL_V_IS_VACUUM_SAFE
#undef CWAL_ALLOC_DO_PAD
#undef CWAL_MEMSZ_PAD
#undef CWAL_UNIQUE_VAL
#undef MEMSZ_PTR_FROM_MEM
#undef E_IS_DEAD

#undef CWAL_RCFLAG_MAXRC
#undef CWAL_RCFLAGS_BITS
#undef CWAL_RCFLAGS_BITS_MASK
#undef CWAL_REFCOUNT
#undef CWAL_REFCOUNT_SHIFTED
#undef CWAL_RCFLAGS
#undef CWAL_RCADJ
#undef CWAL_RCDECR
#undef CWAL_RCINCR
#undef CWAL_RCFLAG_ON
#undef CWAL_RCFLAG_ON
#undef CWAL_RCFLAG_OFF
#undef CWAL_RCFLAG_HAS
#undef CWAL_V_IS_IN_CLEANUP
#undef CWAL_V_IS_RESCOPING
#undef CWAL_VVPCAST_NONULL
#undef CWAL_DBL_NONULL
#undef CWAL_INT_NONULL
#undef CWAL_STRLEN_TOO_LONG
#undef CWAL_BUILTIN_INT_VAL
#undef CWAL_BUILTIN_INT_FIRST
#undef CWAL_BUILTIN_INT_LAST
#undef CWAL_BUILTIN_INT_COUNT
#undef CWAL_V_GOES_IN_HEADOBJ
#undef CWAL_OB_LOCK
#undef CWAL_OB_IS_LOCKED
#undef CWAL_OB_UNLOCK
#undef cwal__propref_begin_p
#undef cwal__propref_begin_v
#undef cwal__propref_end
#undef cwal__propref_is_cyclic
#undef cwal__err_reset

#if defined(__cplusplus)
} //extern "C" LOL - the C++-style comments do not upset C89 here.
#endif
