/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=4 et sw=2 tw=80: */
#if !defined(WANDERINGHORSE_NET_CWAL_INTERNAL_H_INCLUDED)
#define WANDERINGHORSE_NET_CWAL_INTERNAL_H_INCLUDED 1
/**
   This file documents the internal/private API of libcwal.  It's
   strictly internal parts are only in the main implementation file,
   but the parts in this file are thought to potentially be useful in
   future sub-systems which live somewhere between the private API and
   public API (not quite public, not quite core).
*/

#if defined(__cplusplus)
extern "C" {
#endif

/** @internal

  CWAL_UNUSED_VAR exists only to squelch, in non-debug builds,
  warnings about the existence of vars and function parameters which
  are only used in assert() expressions (and thus get filtered out in
  non-debug builds).
*/
#define CWAL_UNUSED_VAR __attribute__((__unused__)) /* avoiding unused var in non-debug build */

/**
   A callback type for use with cwal_ptr_table_visit(). The elem
   argument is the entry being visited over. The state argument is
   whatever state the caller passed to cwal_ptr_table_visit().

   If a visitor function returns non-0, iteration ceases and that
   result code is returned to the caller of cwal_ptr_table_visit().

   Hypothetically, it is legal for a visitor to overwrite *elem, but
   that feature is not used in cwal's internals and might or might not
   work (or be semantically legal) in any other context.
*/
typedef int (*cwal_ptr_table_visitor_f)( cwal_value ** elem, void * state );

/** @internal
   Operation values for cwal_ptr_table_op().
*/
enum cwal_ptr_table_ops {
    /** Indicates that cwal_ptr_table_op() should remove the requested
        item. */
    CWAL_PTR_TABLE_OP_REMOVE = -1,
    /** Indicates that cwal_ptr_table_op() should search for the
        requested item. */
    CWAL_PTR_TABLE_OP_SEARCH = 0,
    /** Indicates that cwal_ptr_table_op() should insert the requested
        item. */
    CWAL_PTR_TABLE_OP_INSERT = 1
};
typedef enum cwal_ptr_table_ops cwal_ptr_table_ops;

/** @internal

  Initializes a new pointer table. *T may be NULL, in which case this
  function allocates a new cwal_ptr_table. If *T is not NULL then it is
  assumed to be a freshly-allocated, non-initialized pointer table
  and is initialized as-is.

  Returns 0 on error. On success *T is set to the ptr table (possibly
  freshly allocated) and ownership is transfered to the caller, who
  must eventually call cwal_ptr_table_destroy() to free its resources.
*/
int cwal_ptr_table_create( cwal_engine * e, cwal_ptr_table ** T, uint16_t hashSize, uint16_t step );

/** @internal

  Frees all resources owned by t. t must have been initialized by
  cwal_ptr_table_create(), and if that function allocated t, then t
  will also be free()'d, otherwise the caller must free t using the
  method which complements its allocation technique (e.g. do nothing
  for stack-allocated values).

  Returns 0 on success.
*/
int cwal_ptr_table_destroy( cwal_engine * e, cwal_ptr_table * t );

/** @internal

  Performs a search/remove/insert operation (specified by the op
  parameter) on the given ptr table. Returns 0 on success.

  The valid operations are:

  CWAL_PTR_TABLE_OP_REMOVE: remove the given key from the table.
  Returns CWAL_RC_NOT_FOUND, and has no side effects, if no matching
  entry is found.
  
  CWAL_PTR_TABLE_OP_SEARCH: If a match is found returns 0, else
  returns CWAL_RC_NOT_FOUND.

  CWAL_PTR_TABLE_OP_INSERT: insert the given key into the
  table. Returns CWAL_RC_ALREADY_EXISTS, and has no side effects, if
  key is already in the table.
*/
int cwal_ptr_table_op( cwal_engine * e, cwal_ptr_table * t, void * key, cwal_ptr_table_ops op );

/** @internal
   Equivalent to cwal_ptr_table_op(), passing it (e,t,key,CWAL_PTR_TABLE_OP_SEARCH).
*/
int cwal_ptr_table_search( cwal_engine * e, cwal_ptr_table * t, cwal_value * key );

/** @internal
   Equivalent to cwal_ptr_table_op(), passing it (e,t,key,CWAL_PTR_TABLE_OP_REMOVE).
*/
int cwal_ptr_table_remove( cwal_engine * e, cwal_ptr_table * t, cwal_value * key );

/** @internal
   Equivalent to cwal_ptr_table_op(), passing it (e,t,key,CWAL_PTR_TABLE_OP_INSERT).
*/
int cwal_ptr_table_insert( cwal_engine * e, cwal_ptr_table * t, cwal_value * key );

/** @internal

    Calculates the amount of memory allocated for use with the given
    table.  The results are stored in *mallocs (the total number of
    memory allocation calls) and *memory (the total number of bytes
    allocated for use with t). Either of mallocs or memory may be
    NULL. This is an O(N) operation, where N is the number of pages
    currently managed by t.
 */
int cwal_ptr_table_mem_cost( cwal_ptr_table const * t, uint32_t * mallocs, uint32_t * memory );

/** @internal

    "Visits" each entry in t, calling the given callback for each one
    (NULL entries in the table are skipped - only "live" entries are
    iterated over). The callback is passed a pointer to a pointer to
    the original entry, and if that pointer is re-assigned, that
    change is written back to the table when the visitor
    returns. (Why? i'm not sure why it was done that way - it is a
    dangerous feature and should not be used.)

    Returns 0 on success. if the visitor returns non-0, that code is
    returned to the caller of this function.
*/
int cwal_ptr_table_visit( cwal_ptr_table * t, cwal_ptr_table_visitor_f f, void * state );

    
/** @internal
 
   Strings are allocated as an instances of this class with N+1
   trailing bytes, where N is the length, in bytes, of the string
   being allocated. To convert a cwal_string to c-string we simply
   increment the cwal_string pointer. To do the opposite we use (cstr
   - sizeof(cwal_string)). Zero-length strings are a special case
   handled by a couple of the cwal_string functions.

   The top-most 3 bits of the length field are reserved for
   flags. This is how we mark "x-strings" (external strings, held in
   memory allocated elsewhere (e.g. static globals or client-interned
   strings), z-strings (whose memory comes from cwal_malloc() but is
   allocated separately from the string instance (we use this for
   "taking" memory from buffer instances), and (as a performance
   optimization in some algorithms) ASCII-only strings.
*/
struct cwal_string {
    cwal_size_t length;
};

/** @internal

    Internal state flags. Keep these at 16 bits (and use only the
    bottom 8 unless you know a given type supports it!) or adjust
    various flags types accordingly! Note that some flags apply only
    to particular types, and thus certain flags might have the same
    values to conserve bit-space.
*/
enum cwal_internal_flags {
/**
   Sentinel value for empty flags.
 */
  CWAL_F_NONE = 0,
  /**
     Used by cwal_value_vtab::flags to indicate that the type contains
     a cwal_obase as its first struct member and therefore qualifies
     as an "obase" (object base) sub-type. This is a casting-related
     optimization.

     "OBases" must meet these requirements:

     - It is allocated using the library-conventional single-malloc
     method of (cwal_value,concrete_type) in a single block. See
     cwal_value_new() for the many examples.
                
     - Its concrete value type has a cwal_obase struct as its first
     member. This, in combination with the first requirement, allows
     us (due to an interesting C rule) to efficiently/safely cast
     directly between a cwal_value, its "object type," and a
     convenience base type (cwal_obase) which is used in many internal
     algorithms to consolidate code for container handling. And... we
     don't hold an extra pointer (or two) for this - it comes for free
     as a side-effect of these first 2 requirements.

     See cwal_object and cwal_array for examples of using the obase
     flag.

     Reminder to self: we can re-use this slot for flags on
     cwal_engine, cwal_obase, and cwal_scope :).
  */
  CWAL_F_ISA_OBASE = 0x01,

  /**
     Set on cwal_engine instances during init if their memcap config
     is enabled, to speed up some if/else checking in the allocator.
  */
  CWAL_F_TRACK_MEM_SIZE = 0x02,
  
  /**
     Is used as an extra safety mechanism to avoid a double-delete if
     the refcounts-with-cycles-counting mechanism breaks. This only
     means that we convert a double free() into a leak (or an assert()
     in debug builds).

     Used in cwal_engine::flags and cwal_scope::flags. Values use a
     different flag with similar semantics.
  */
  CWAL_F_IS_DESTRUCTING = 0x04,

  /**
     Used for marking cwal_scope::props (via its cwal_obase::flags) to
     avoid some weird corner cases involving the vacuum-safe flag
     when/if the variable storage Objects are exposed to
     clients/scripts. It's needed to be able to differentiate this
     case from vacuum-safe in order to avoid a corner case if the
     client explicitly sets/unsets the vacuum-safe flag on a prop
     storage container.
  */
  CWAL_F_IS_PROP_STORAGE = 0x10,

  /**
     A flag to briefly note that a value is temporarily locked, e.g.
     currently being sorted, and must not be traversed or modified.
     As of this writing (20191211), only arrays are/need to be locked
     in a way distinct from the is-visiting flag, so we we "could"
     re-use this flag's value with other semantics for non-array
     types, if needed.
  */
  CWAL_F_LOCKED = 0x20
};

/** @internal
    Convenience typedef. */
typedef struct cwal_htable cwal_htable;
/** @internal
   An internal helper class for building hashtables of cwal_kvp
   entries.
*/
struct cwal_htable {
  /**
     Array (hash table) of (cwal_kvp*) values. Its .count property
     holds the current total number of entries (noting that they won't
     typically be in contiguous list slots). Its .alloced property
     holds the length (in entries) of the table. Its .list member is
     the raw array storage.
  */
  cwal_list list;
  /**
     Side effect of the chunk recycler: we need to be able to
     differentiate between the hashtable size and the amount of memory
     allocated for it (which might be larger). This value be less than
     or equal to this->list.alloced.
  */
  cwal_midsize_t hashSize;
};
/** A clean cwal_htable instance for cost-copy initialization. */
#define cwal_htable_empty_m {cwal_list_empty_m,0}

/** @internal
    Convenience typedef. */
typedef struct cwal_obase cwal_obase;

/** @internal
   "Base" type for types which contain other values (which means they
   are subject to cycles). An instance of this struct must be embedded
   at the FIRST MEMBER of any such class, and any type-specific
   members must come after that. See cwal_array and cwal_object for
   examples.
 */
struct cwal_obase {
#if CWAL_OBASE_ISA_HASH
  /**
     Hashtable of object-level properties.
  */
  cwal_htable hprops;
#else
  /**
     Linked list of key/value pairs held by this object.
  */
  cwal_kvp * kvp;
#endif
  /**
     The "prototype" (analog to a parent class).
  */
  cwal_value * prototype;
  /**
     Internal flags.

     Maintenance note: due to struct padding, this can be either 16
     or 32-bits with no real change in struct size on 64-bit, but
     increasing either flags field on 32-bit increases the sizeof
     (4 bytes if we increase both fields to 32 bits). Internally we
     currently need 16 bits for flags.

     As of 20141205, we are using the top few bits of these via
     cwal_container_flags, exposing/modifying the upper byte of
     these flags via cwal_container_flags_set() and
     cwal_container_flags_get().

     As of 20191212, we desperately need more room for flags, but
     cannot do so without increasing the sizeof by up to 4 on 32-bit
     builds, from 12 to 16. We may just have to eat that cost :/.
     On 64-bit builds that change wouldn't change the sizeof().
  */
  cwal_flags16_t flags;
  /**
     Engine-internal flags specifically for container-type-specific
     behaviour.
  */
  cwal_flags16_t containerFlags;

  /**
     Holds client-specified flags.
  */
  cwal_flags16_t clientFlags;

  cwal_flags16_t reservedPadding;
};

/** @internal
    Empty-initialized cwal_obase object.
*/
#if CWAL_OBASE_ISA_HASH
#  define cwal_obase_empty_m {\
    cwal_htable_empty_m/*hprops*/, NULL/*prototype*/,CWAL_F_NONE/*flags*/,\
    0/*containerFlags*/, 0/*clientFlags*/, 0/*reservedPadding*/\
  }
#else
#  define cwal_obase_empty_m {                              \
    NULL/*kvp*/, NULL/*prototype*/, CWAL_F_NONE/*flags*/,\
    0/*containerFlags*/, 0/*clientFlags*/, 0/*reservedPadding*/\
  }
#endif

/** @internal

   Concrete value type for Arrays (type CWAL_TYPE_ARRAY).
*/
struct cwal_array {
    /**
       base MUST be the first member for casting reasons.
    */
    cwal_obase base;
    /**
       Holds (cwal_value*). NULL entries ARE semantically legal.
    */
    cwal_list list;
};
/**
   Empty-initialized cwal_array object.
*/
#define cwal_array_empty_m { cwal_obase_empty_m, cwal_list_empty_m }
/**
   Empty-initialized cwal_array object.
*/
extern const cwal_array cwal_array_empty;

/** @internal

    The metadata for concrete Object values (type CWAL_TYPE_OBJECT).
 */
struct cwal_object {
    /**
       base MUST be the first member for casting reasons.
    */
    cwal_obase base;
};
/**
   Empty-initialized cwal_object object.
*/
#define cwal_object_empty_m { cwal_obase_empty_m }
/**
   Empty-initialized cwal_object object.
*/
extern const cwal_object cwal_object_empty;

/**
   Information for binding a native function to the script engine in
   the form of a Function value (type CWAL_TYPE_FUNCTION).
*/
struct cwal_function {
    /**
       base MUST be the first member for casting reasons.
    */
    cwal_obase base;
    /**
       Client-defined state for the function.
    */
    cwal_state state;

    /**
       The concrete callback implementation.
    */
    cwal_callback_f callback;

    /**
       "Rescoper" for the function. Can be set via
       cwal_function_set_rescoper(), and gives the client a way to
       rescope any function-private data (stored in this struct's
       state member) if needed.

       Use case: s2's s2_func_state wants to hold/manage "static"
       script state at the function level without using a hidden
       property. i.e. we don't want cwal_props_clear() to be able to
       nuke that state.
    */
    cwal_value_rescoper_f rescoper;
};

/**
   Empty-initialized cwal_function object.
*/
#define cwal_function_empty_m { cwal_obase_empty_m, cwal_state_empty_m, NULL, NULL }
/**
   Empty-initialized cwal_function object.
*/
extern const cwal_function cwal_function_empty;

/**
   Concrete type for generic error/exception values.
*/
struct cwal_exception {
    /**
       base MUST be the first member for casting reasons.
    */
    cwal_obase base;
    int code;
};
/**
   Empty-initialized cwal_exception object.
*/
#define cwal_exception_empty_m { cwal_obase_empty_m, -1 }
/**
   Empty-initialized cwal_exception object.
*/
extern const cwal_exception cwal_exception_empty;

/**
   A key/value pair of cwal_values. While key can be an arbitrary
   cwal_value, the engine requires that the key be of a type with a
   stable hash code AND stable comparison semantics (as of 201811,
   cwal_tuple and cwal_buffer are the only types which are "most
   definitely not recommended) for use as keys because modification of
   their contents changes how they compare to other values, which
   means that they can get "out of place" within a sorted property
   list (e.g. cwal_obase::kvp).

   Each of these objects owns its key/value pointers, and they
   are cleaned up by cwal_kvp_clean(). A KVP holds a reference
   to each of its parts.
*/
struct cwal_kvp{
    /**
       The key. Keys are compared using cwal_value_vtab::compare().
    */
    cwal_value * key;
    /**
       Arbitrary value. Objects do not have NULL values - a literal
       NULL means to "unset" a property. cwal_value_null() can be
       used as a value, of course.
    */
    cwal_value * value;
    /**
       Right-hand member in a linked list. cwal_obase and cwal_hash
       use this. Nobody else should.
    */
    cwal_kvp * right;
    /**
       We need this for intepreter-level flags like "const" and
       "hidden/non-iterable." This was increased from 16 to 32 bits on
       20191210, which does not change the sizeof(), because of
       padding, but the public APIs were left as-is (exposing only
       cwal_flags16_t). We can still, on 64-bit builds, stuff another
       4(?) bytes in here without increasing the sizeof(), which would
       allow us to add a refcount to KVPs, though that wouldn't be as
       useful as it may initially sound because we'd need to implement
       Copy-on-Write for shared KVPs, which has some backfire cases
       for how we use these objects.

       It might be interesting to expose the top 16 of these bits for
       use by clients, but a concrete use case for such flags (which
       isn't covered by existing cwal-level flags) eludes me
    */
    cwal_flags16_t flags;
};

/**
   Empty-initialized cwal_kvp object.
*/
#define cwal_kvp_empty_m {NULL,NULL,NULL,0U/*flags*/}
/**
   Empty-initialized cwal_kvp object.
*/
extern const cwal_kvp cwal_kvp_empty;

/** @internal

   Semantically allocates a new cwal_kvp object, owned by e, though it
   may pull a recycled kvp from e's internal recycler. Returns NULL on
   error. On success the returned value is empty-initialized.
*/
cwal_kvp * cwal_kvp_alloc(cwal_engine *e);

/** @internal
   Unrefs kvp->key and kvp->value and sets them to NULL, but does not
   free kvp. If !kvp then this is a no-op.
*/
void cwal_kvp_clean( cwal_engine * e, cwal_kvp * kvp );
    
/** @internal

   Calls cwal_kvp_clean(e,kvp) and then either adds kvp to e's recycle
   bin or frees it, depending on the value of allowRecycle and the
   capacity/size of the associated recycler list.

   Callers must treat this call as if kvp is free()d by it, whether or
   not this function actually does so.
*/
void cwal_kvp_free( cwal_engine * e, cwal_kvp * kvp, char allowRecycle );

/**
   Typedef for cwal_value hashing functions. Must return a hash value
   for the given value.
*/
typedef cwal_hash_t (*cwal_value_hash_f)( cwal_value const * v );

/**
   Returns v's hash value, as computed by v's vtab.
   Returns 0 if !v.
*/
cwal_hash_t cwal_value_hash( cwal_value const * const v );
    
/**
   Typedef for cwal_value comparison functions. Has memcmp()
   semantics. Ordering of mismatched types (e.g. comparing an array to
   an object) is type-dependent, possibly undefined. Implementations
   of this function are type-specific and require that the lhs
   (left-hand-side) argument be of their specific type (and are
   permitted to assert that that is so). When performing conversions,
   they should treat the LHS as the primary type for
   conversion/precision purposes. e.g. comparison of (int 42) and
   (double 42.24) might be different depending on which one is the LHS
   because of changes in precision.

   Beware that these comparisons are primarily intended for
   cwal-internal use (e.g. in the context of property lists and
   hashtables), and are not strictly required to follow the semantics
   of any given scripting environment or specificiation. (That said,
   the public cwal_value_compare() interface uses these, so the
   behaviour must remain stable.)

   Where (lhs,rhs) do not have any sort of natural ordering,
   implementations should return any value other than 0, implementing
   ECMAScript-like semantics if feasible.
   
   Implementations are encouraged to do cross-type comparisons where
   feasible (e.g. "123"==123 is true), and to delegate to the converse
   comparison (swapping lhs/rhs and the return value) when the logic
   for the comparison is non-trivial and already implemented for the
   other type. Strict-equality comparisons (e.g. "123"===123 is false)
   are handled at a higher level which compares type IDs and/or
   pointers before passing the values on to this function. Comparisons
   are prohibited (by convention) from allocating any memory, and the
   API is not set up to be able to report an OOM error to the caller.
*/
typedef int (*cwal_value_compare_f)( cwal_value const * lhs, cwal_value const * rhs );
    
/**
   This type holds the "vtbl" for type-specific operations when
   working with cwal_value objects.

   All cwal_values of a given logical type share a pointer to a single
   library-internal instance of this class.
*/
struct cwal_value_vtab
{
    /**
       The logical data type associated with this object.
     */
    const cwal_type_id typeID;

    /**
       A descriptive type name intented primarily for debuggering, and
       not (necessarily) as a type's name as a client script language
       might see/name it (though, in fact, they are in used that way).
     */
    char const * typeName;

    /**
       Internal flags.
    */
    cwal_flags16_t flags;

    /**
       Must free any memory owned by the second argument, which will
       be a cwal_value of the concrete type associated with this
       instance of this class, but not free the second argument (it is
       owned by the engine and may be recycled). The API shall never
       pass a NULL value to this function.

       The API guarantees that the scope member of the passed-in value
       will be set to the value's pre-cleanup owning scope, but also
       that the value is not in the scope value list at that time. The
       scope member is, however, needed for proper unreferencing of
       child values (which still live in a scope somewhere, very
       possibly the same one). Implementations must not do anything
       with child values other than unref them, as they may very well
       already be dead and in the gc-queue (or recycler) by the time
       this is called. The engine delays (via the gc-queue) any
       free()ing of those children while a cleanup pass is running, so
       handling the memory of a child value is legal, but any usage
       other than an unref is semantically strictly illegal.
    */
    cwal_finalizer_f cleanup;

    /**
       Must return a hash value for this value. Hashes are used only as
       a quick comparison, with the compare() method being used for
       a more detailed comparison.
    */
    cwal_value_hash_f hash;

    /**
       Must perform a comparison on its values. The cwal engine
       guarantees that the left-hand argument will be of the type
       managed by the instance of the cwal_value_tab on which this is
       called, but the right-hand argument may be of an arbitrary
       type.

       This function checks for equivalence, not identity, and uses
       memcmp() semantics: less than 0 means that the left-hand
       argument is "less than" the right, 0 means they are equivalent,
       and 1 means the the lhs is "greater than" the right. It is
       understood, of course, that not all comparisons have meaningful
       results, and implementations should always return non-0 for
       meaningless comparisons. They are not required to return a
       specific value but should behave consistently (e.g. not
       swapping the order on every call or some such sillynesss).

       For many non-POD types, 0 can only be returned when both
       pointers have the same address (that said, the framework should
       short-circuit any such comparison itself).
    */
    cwal_value_compare_f compare;

    /**
       Called by the framework when it determines that v needs to be
       "upscoped." The framework will upscope v but cannot generically
       know if v contains any other values which also need to be
       upscoped, so this function has the following responsibilities:

       For any child values which are "unmanaged" (e.g. they're not
       stored in cwal_object properties), this function must call
       cwal_value_rescope( v->scope->e, v->scope, child ). That, in
       turn, will trigger the recursive rescoping of any children of
       that value. Note that the rescoping problem is not subject to
       "cyclic misbehaviours" - the worst that will happen is a cycle
       gets visited multiple times, but those after the first will be
       no-ops because no re-scoping is necessary: the API only calls
       this when upscoping is necessary.

       The framework guarantees the following:

       Before this is called on a value, the following preconditions
       exist:

       - v is of the value type represented by this vtab instance.

       - v->scope has been set to the "proper" scope by e. This
       function will never be called when v->scope is 0.

       - v->scope->e points to the active engine.

       - This is only called when/if v is actually upscoped. Re-scope
       requests which do not require a rescope will not trigger a call
       to this function.

       If v owns any "unmanaged" child values (e.g. not kept in the
       cwal_obase base class container or as part of a cwal_array)
       then those must be rescoped by this function (the framework
       does not know about them). They likely also need to be made
       vacuum-proof (see cwal_value_make_vacuum_proof()).

       Classes which do not contain other values may set this member
       to 0. It is only called if it is not NULL. Classes which do
       hold other values _must_ implement it (properly!).

       Must return 0 on success and "truly shouldn't fail" because all
       it does it pass each child on to cwal_value_rescope(), which
       cannot fail if all arguments are in order and the internal
       state of the values seems okay (the engine does a good deal of
       extra checking and assert()ing). However, if it fails it should
       return an appropriate value from the cwal_rc_e enum. Returning
       non-0 will be treated as fatal, possibly assert()ing in debug
       builds.
    */
    int (*rescope_children)( cwal_value * v );

    /**
       TODOs???:

       // Using JS semantics for true/value
       char (*bool_value)( cwal_value const * self );

       // To-string ops...
       unsigned char const * to_byte_array( cwal_value const * self, cwal_size_t * len );
       // which is an optimized form of ...
       int to_string( cwal_value const * self, cwal_engine * e, cwal_buffer * dest );
       // with the former being used for (Buffer, String, builtins)
       // and the latter for everything else. Either function may be 0
       // to indicate that the operation is not supported
       // (e.g. Object, Array, Hashtable, Function...).

       // The problem with adding an allocator is...
       int (*allocate)( cwal_engine *, ??? );
       // If we split it into allocation and initialization, might
       // that solve it? Or cause more problems?
       // cwal_value * (*allocate)( cwal_engine * e );
       // int (*initialize)( cwal_engine * e, cwal_value * v, ... )

       // Deep copy.
       int (*clone)( cwal_engine *e, cwal_value const * self, cwal_value ** tgt );

       // Property interceptors:
       int (*get)( cwal_engine *e, cwal_value const * self,
                   cwal_value const * key, cwal_value **rv );

       int (*set)( cwal_engine *e, cwal_value * self,
                   cwal_value * key, cwal_value *v );

       But for convenience the get() op also needs a variant taking a
       c-string key (otherwise the client has to create values when
       searching, more often than not)
    */
};

typedef struct cwal_value_vtab cwal_value_vtab;
/**
   Empty-initialized cwal_value_vtab object.
*/
#define cwal_value_vtab_empty_m {           \
        CWAL_TYPE_UNDEF/*typeID*/,         \
        ""/*typeName*/, \
        0U/*flags*/, \
        NULL/*cleanup()*/,                    \
        NULL/*hash()*/,                    \
        NULL/*compare()*/,             \
        NULL/*rescope_children()*/ \
      }
/**
   Empty-initialized cwal_value_vtab object.
*/
extern const cwal_value_vtab cwal_value_vtab_empty;

/**
   cwal_value represents an opaque Value handle within the cwal
   framework. Values are all represented by this basic type but they
   have certain polymorphic behaviours (via cwal_value_vtab) and many
   concrete types have high-level handle representations
   (e.g. cwal_object and cwal_array).

   @see cwal_new_VALUE()
   @see cwal_value_vtab
*/
struct cwal_value {
    /**
       The "vtbl" of type-specific operations. All instances of a
       given logical value type share a single vtab instance.

       Results are undefined if this value is NULL or points to the
       incorrect vtab instance.
    */
    cwal_value_vtab const * vtab;

    /**
       The "current owner" scope of this value. Its definition is
       as follows:

       When a value is initially allocated its owner is the
       currently-active scope. If a value is referenced by a
       higher-level (older) scope, it is migrated into that scope
       (recursively for containers) so that we can keep cleanup of
       cycles working (because all values in a given graph need to be
       in the same scope for cleanup to work). It is, on rare
       occasion, necessary for code (sometimes even client-side) to
       cwal cwal_value_rescope() to re-parent a value.
     */
    cwal_scope * scope;

    /**
       The left-hand-link for a linked list. Who exactly owns a value,
       and which values they own, are largely handled via this
       list. Each manager (e.g. scope, recycle bin, or gc queue) holds
       the head of a list and adds/removes the entries as needed.

       Note that we require two links because if we only have a single
       link then cleanup of a member in a list can lead traversal
       through that list into places it must not go (like into the
       recycler's memory).

       Design notes: in the original design each value-list manager
       had a separate array-style list to manage its values. Switching
       to this form (where the values can act as a list) actually
       (perhaps non-intuitively) cuts both the number of overall
       allocations and memory cost, converts many of the previous
       operations from O(N) to O(1), and _also_ removes some
       unrecoverable error cases caused by OOM during manipulation of
       the owner's list of values. So it's an overall win despite the
       cost of having 2 pointers per value. Just remember that it's
       not strictly Value-specific overhead... it's overhead Scopes
       and other Value owners would have if this class didn't.
    */
    cwal_value * left;

    /**
       Right-hand link of a linked list.
    */
    cwal_value * right;

    /**
       We use this to allow us to store a refcount and certain flags.

       Notes about the rc implementation:

       - Instances start out with a refcount of 0 (not 1). Adding them
       to a container will increase the refcount. Cleaning up the container
       will decrement the count. cwal_value_ref() can be used to obtain
       a reference when no container is handy.

       - cwal_value_unref() decrements the refcount (if it is not
       already 0) and cleans/frees the value only when the refcount is
       0 (and then it _might_ not destroy the value immediately,
       depending on which scope owns it and where (from which scope)
       its refcount goes to 0).

       - cwal_value_unhand() decrements the refcount (if it is not
       already 0) but does not destroy the value if it reaches 0. If
       it reaches 0, "unhanding" re-sets the value into a so-called
       "probationary" state, making it subject to being swept up if no
       reference is taken before the next cwal_engine_sweep() (or
       similar).

       - This number HAS FLAGS ENCODED IN IT, so don't use this value
       as-is. How many flags, where they are, and what they mean, are
       internal details. Search cwal.c for RCFLAGS for the gory
       details.
    */
    cwal_refcount_t rcflags;

    /*
      Historical notes, no longer entirely relevant but perhaps
      interesting:

      ========
      
      Potential TODO: if we _need_ flags in this class, we can use the
      high byte (or half-byte) of refcount and restrict refcount to
      the lower bytes (possibly only 3 resp. 3.5). We need to make
      sure refcount uses a 32-bit type in that case, as opposed to
      cwal_size_t (which can be changed to uint16_t, which only gives
      us a range of up to 4k if we reserve 4 bits!). While 4k might
      initially sound like a reasonable upper limit for refcounts,
      practice has shown that value prototypes tend to accumulate lots
      and lots of references (one for each value which has it as a
      prototype), so 4kb quickly becomes a real limit.

      16 bits (64k) of refcount might be a feasible upper limit, even
      for large apps. And nobody will ever need more than 640kb of
      RAM, either.

      We could... use a 32-bit type, reserve the bottom 24 bits (16M)
      for the refcount, and the top 8 bits for client-side flags.

      Class-level flags are set in the associated cwal_value_vtab
      instance. Container classes may have their own instance-specific
      flags.

      Reminder to self: interpreters are going to need flags for
      marking special values like constants/non-removable. Oh, but
      that's going to not like built-in constants. So we'll need to
      tag these at the cwal_kvp level (which costs us less, actually,
      because we have one set of flags per key/value pair, instead of
      per Value instance).

      Ideas of what clients could use flags for:

      - tagging values which want special handling. e.g. if scripts
      can override get/set operations, that may be too costly if
      performed on all values. A flag could indicate whether a given
      value has such an override. Tagging constructor/factory
      functions for special handling with the 'new' keyword is
      something we could possibly use in s2.

      - in s2, we could use this to tag Classes and instances of classes,
      such that we could change property lookup on them (and potentially
      reject the addition of new properties to classes).

      Problems:

      - Built in constant values cannot be flagged. The use of the
      numeric (-1, 0, 1) values as built-in constants saves up to 50%
      of numeric-type allocations in many test scripts, so i don't
      want to drop those. If we allow flags only on container types
      (plus buffers), we can move the cost/handling there (and maybe
      get more flag space). Or we add a way for clients to create
      mutable instances of builtin values, such that they can be
      tagged. That would require some fixes/changes in other bits
      which make assumptions about the uniqueness of, e.g. boolean
      values.

    */
};


/**
   Empty-initialized cwal_value object.
*/
#define cwal_value_empty_m { \
    &cwal_value_vtab_empty/*api*/,\
    0/*scope*/,\
    0/*left*/,\
    0/*right*/,                             \
    0/*rcflags*/ \
}
/**
   Empty-initialized cwal_value object.
*/
extern const cwal_value cwal_value_empty;


struct cwal_native {
    /**
       base MUST be the first member for casting reasons.
    */
    cwal_obase base;
    void * native;
    void const * typeID;
    cwal_finalizer_f finalize;
    /**
       If this member is non-NULL then it is called to allow
       the native to rescope properties not visible via its property
       list.
    */
    cwal_value_rescoper_f rescoper;
};
#define cwal_native_empty_m {\
        cwal_obase_empty_m, \
        0/*native*/,\
        0/*typeID*/,\
        0/*finalize*/, \
        0/*rescoper*/ \
        }
extern const cwal_native cwal_native_empty;

/** @internal
   Hash table Value type.
*/
struct cwal_hash {
  /**
     base MUST be the first member for casting reasons.
  */
  cwal_obase base;
  /**
     The actual hashtable. Note that if (CWAL_OBASE_ISA_HASH) then
     this hashtable is a separate one: that one is the object-level
     properties and this one is the "plain" hashtable. The main
     difference is that the latter does not participate in prototype
     property lookup.
  */
  cwal_htable htable;
};
#define cwal_hash_empty_m {            \
    cwal_obase_empty_m/*base*/,        \
    cwal_htable_empty_m/*htable*/    \
    }
extern const cwal_hash cwal_hash_empty;


/** @internal

    An object-style representation for cwal_buffer. This type is
    strictly internal, not exposed to clients.
*/
struct cwal_buffer_obj {
    /**
       base MUST be the first member for casting reasons.
    */
    cwal_obase base;
    /**
       The buffer owned/tracked by this object.
    */
    cwal_buffer buf;
};
typedef struct cwal_buffer_obj cwal_buffer_obj;
#define cwal_buffer_obj_empty_m {\
    cwal_obase_empty_m/*base*/, \
    cwal_buffer_empty_m/*buf*/ \
}
extern const cwal_buffer_obj cwal_buffer_obj_empty;

/**
   Internal state for CWAL_TYPE_TUPLE-typed cwal_values.

*/
struct cwal_tuple {
    /**
       Number of entries in the list (set at init-time and
       never changes).
    */
    uint16_t n;
    /**
       Not yet used. Note that removing this does not shrink this
       type's sizeof(), due to padding.
    */
    uint16_t flags;
    /**
       List of this->n entries.
    */
    cwal_value ** list;
};
#define cwal_tuple_empty_m {0U,0U,NULL}

/**
   Internal state for CWAL_TYPE_PROPREF-typed cwal_values.
*/
struct cwal_propref {
  /**
     The target container for this propref. Which element is active
     is specified in this->flags.
   */
  union {
    /**
       The container proxied by this object.
    */
    cwal_value * c;
    /**
       A weak reference to the container proxied by this object.
    */
    cwal_weakref * w;
  } target;
  /**
     The target[key] property key for this object.
  */
  cwal_value * key;
  /**
     This is per-subtype state.
  */
  cwal_value * xtra;
  /**
     The propref type and various flags.
  */
  unsigned short flags;
};
#define cwal_propref_empty_m {NULL, NULL,NULL,CWAL_PROPREF_KVP,false}


/** @internal

    Internal impl of the weak reference class.
*/
struct cwal_weakref {
    void * value;
    cwal_type_id typeID;
    cwal_refcount_t refcount;
    struct cwal_weakref * next;
};

/** @internal

    Initialized-with-defaults cwal_weakref instance.
*/
#define cwal_weakref_empty_m {NULL, CWAL_TYPE_UNDEF, 0U, NULL}

/** @internal

    Initialized-with-defaults cwal_weakref instance.
*/
extern const cwal_weakref cwal_weakref_empty;

/** @internal

    If v is-a obase then its obase part is returned, else NULL is
    returned.
*/
cwal_obase * cwal_value_obase( cwal_value * const v );


/** @internal

   Internal debuggering function which MIGHT dump some useful info
   about v (not recursively) to some of the standard streams.

   Do not rely on this function from client code. 

   The File/Line params are expected to be the __FILE__/__LINE__
   macros.

   If msg is not NULL then it is included in the output (the exact
   placement, beginning or end, is unspecified).
 */
void cwal_dump_value( char const * File, int Line,
                      cwal_value const * v, char const * msg );

/** @def cwal_dump_v(V,M)

    Internal debuggering macro which calls cwal_dump_value() with the
    current file/line/function info.
*/
#if 1
#  define cwal_dump_v(V,M) cwal_dump_value(__func__,__LINE__,(V),(M))
#else
#  define cwal_dump_v(V,M) assert(1)
#endif
                                    

/**
   Searches e for an internalized string matching (zKey,nKey). If
   found, it returns 0 and sets any of the output parameters which are
   not NULL.

   On success, *out (if out is not NULL) be set to the value matching
   the key. Note that only String values can compare equal here, even
   if the key would normally compare as equivalent to a value of
   another type. e.g. 1=="1" when using cwal_value_compare(), but
   using that comparison here would not be possible unless we
   allocated a temporary string to compare against.
   
   It sets *itemIndex (if not NULL) to the index in the strings table
   for the given key, regardless of success of failure. The other
   output params are only set on success.

   *out (if not NULL) is set to the value in the table.  pageIndex (if
   *not NULL) is set to the page in which the entry was found.

   Reminder to self: we might be able to stack-allocate an x-string
   around zKey and do a cwal_value-keyed search on that. That would
   work around the (1!="1") inconsistency.
*/
int cwal_interned_search( cwal_engine * e, char const * zKey, cwal_size_t nKey,
                          cwal_value ** out, cwal_ptr_page ** pageIndex, uint16_t * itemIndex );

/**
   Equivalent to cwal_interned_search() except that it takes a
   cwal_value parameter and uses cwal_value_compare() for the hashing
   comparisons. A cwal String value inserted this way _will_ compare
*/
int cwal_interned_search_val( cwal_engine * e, cwal_value const * v,
                              cwal_value ** out, cwal_ptr_page ** pageIndex,
                              uint16_t * itemIndex );
/**
   Removes the string matching (zKey,nKey) from e's interned
   strings table. If an entry is found 0 is returned and
   *out (if not NULL) is set to the entry.

   Returns CWAL_RC_NOT_FOUND if no entry is found.
*/
int cwal_interned_remove( cwal_engine * e, cwal_value const * v, cwal_value ** out );

/**
   Inserts the given value, which must be-a String, into
   e's in interned strings list. Returns 0 on success.
   Returns CWAL_RC_ALREADY_EXISTS if the entry's string
   value is already in the table.
   
*/
int cwal_interned_insert( cwal_engine * e, cwal_value * v );

/**
   Pushes the given cwal_value into e->gc for later destruction. We do
   this to avoid prematurely stepping on a being-destroyed Value when
   visiting cycles.

   If insertion of p into the gc list fails then this function frees
   it immediately. We "could" try to stuff it in the recycle bin, but
   that would only potentially delay the problem (of stepping on a
   freed value while cycling).

   This function asserts that e->gcInitiator is not 0.
*/
int cwal_gc_push( cwal_engine * e, cwal_value * p );

/**
   Passes all entries in e->gc to cwal_value_recycle() for recycling
   or freeing.
*/
int cwal_gc_flush( cwal_engine * e );

/**
   If v->vtab->typeID (T) is of a recyclable type and e->recycler entry
   number cwal_recycler_index(T) has space, v is put there, otherwise
   it is cwal_free()'d. This is ONLY to be called when v->refcount
   drops to 0 (in place of calling cwal_free()), or results are
   undefined.

   If e->gcLevel is not 0 AND v is-a obase then v is placed in e->gc
   instead of being recycled so that the destruction process can
   finish to completion without getting tangled up in the recycle bin
   (been there, debugged that). That is a bit of a shame, actually,
   but the good news is that cwal_gc_flush() will try to stick it back
   in the recycle bin. Note that non-Objects do not need to be
   delayed-destroyed because they cannot contribute to cycles.
   
   Returns 0 if the value is freed immediately, 1 if it is recycled,
   and -1 if v is placed in e->gc. If insertion into e->gc is called
   for fails, v is freed immediately (and 0 is returned). (But since
   e-gc is now a linked list, insertion cannot fail except on internal
   mis-use of the GC bits.)
*/
int cwal_value_recycle( cwal_engine * e, cwal_value * v );

/**
   Tracing macros.
 */
#if CWAL_ENABLE_TRACE
#define CWAL_ETR(E) (E)->trace
#define CWAL_TR_SRC(E) CWAL_ETR(E).cFile=__FILE__; CWAL_ETR(E).cLine=__LINE__; CWAL_ETR(E).cFunc=__func__
#define CWAL_TR_MSG(E,MSG) CWAL_ETR(E).msg = MSG; if((MSG) && *(MSG)) CWAL_ETR(E).msgLen = strlen(MSG)
#define CWAL_TR_EV(E,EV) CWAL_ETR(E).event = (EV);
/*if(!(CWAL_ETR(E).msg)) { CWAL_TR_MSG(E,#EV); }*/

#define CWAL_TR_RC(E,RC) CWAL_ETR(E).code = (RC)
#define CWAL_TR_V(E,V) CWAL_ETR(E).value = (V);
#define CWAL_TR_MEM(E,M,SZ) CWAL_ETR(E).memory = (M); CWAL_ETR(E).memorySize = (SZ)
#define CWAL_TR_S(E,S) CWAL_ETR(E).scope = (S)
#define CWAL_TR_SV(E,S,V) CWAL_TR_V(E,V); CWAL_TR_S(E,S)
#define CWAL_TR_VCS(E,V) CWAL_TR_V(E,V); CWAL_TR_S(E,(E)->current)
#define CWAL_TR3(E,EV,MSG)                                              \
    if(MSG && *MSG) { CWAL_TR_MSG(E,MSG); } \
    CWAL_TR_EV(E,EV);                                                \
    if(!(CWAL_ETR(E).scope)) {                                          \
        if(CWAL_ETR(E).value) CWAL_ETR(E).scope = CWAL_ETR(E).value->scope; \
        if(!(CWAL_ETR(E).scope)) CWAL_ETR(E).scope=(E)->current;        \
    }                                                                   \
    CWAL_TR_SRC(E); \
    cwal_trace(E)
#define CWAL_TR2(E,EV) CWAL_TR3(E,EV,"")
#else
#define CWAL_ETR(E)
#define CWAL_TR_SRC(E)
#define CWAL_TR_MSG(E,MSG)
#define CWAL_TR_EV(E,EV)
#define CWAL_TR_RC(E,RC)
#define CWAL_TR_V(E,V)
#define CWAL_TR_S(E,S)
#define CWAL_TR_MEM(E,M,SZ)
#define CWAL_TR_SV(E,S,V)
#define CWAL_TR_VCS(E,V)
#define CWAL_TR3(E,EV,MSG)
#define CWAL_TR2(E,EV)
#endif

/** @internal

   If the library is built with tracing enabled and tracing is enabled
   for e, this function outputs the state of e->trace and then clears
   that state. The intention is that various macros initialize the
   trace state, then call this to output it.
*/
void cwal_trace( cwal_engine * e );

/** @internal
       
   cwal_value_take() "takes" a value away from its owning scope,
   transfering the scope's reference count point to the caller,
   removing the value from any list it is currently in, and settings
   its scope to NULL.

   On error non-0 is returned and ownership is not modified.

   This function works only on "managed" values (with an owning
   scope), and there is no API for creating/managing non-scope-managed
   values from client code.

   Each allocated value is owned by exactly one scope, and this
   function effectively steals the value from the owning scope. This
   function must not be passed the same value instance more than once
   unless the value has been re-scoped since calling this (it will
   fail on subsequent calls, and may assert() that v's is in the
   expected state).

   In all cases, if this function returns 0 the caller effectively
   takes over ownership of v and its memory, and the value IS NOT
   VALID for use with most of the API because, after calling this, it
   has no owning scope, and many APIs assert() that a value has an
   owning scope.

   For built-in values this is a harmless no-op.
       
   Error conditions:

   CWAL_RC_MISUE: either e or v are NULL.

   CWAL_RC_RANGE: means that v has no owning scope.  This
   constellation is highly unlikely but "could happen" if the API ever
   evolves to allow "unscoped" values (not sure how GC could work
   without the scopes, though).
   
   @see cwal_value_unref()   
*/
int cwal_value_take( cwal_engine * e, cwal_value * v );

/** @internal

    An internal helper for routines (specifically JSON)
    to traverse an object tree and detect cycles.

    Passed the object which is about to be traversed and a pointer
    to an opaque state value.

    If this function returns 0, the value is either not capable of
    participating in acyclic traversal (cannot form cyles) or it is
    and was flagged as not being in an acyclic traversal. If non-0 is
    returned, the value was already flagged as being in an acylic
    traversal and was traversed again (by this function), indicating a
    cyclic case (i.e. an error).

    If it returns CWAL_RC_CYCLES_DETECTED: the value is already in the
    process of acyclic traversal. The caller should fail the operation
    with the result code returned by this function
    (CWAL_RC_CYCLES_DETECTED) or a semantic equivalent for the given
    operation.

    If, and only if, the return code is 0, the caller is obligated to
    call cwal_visit_acyclic_end(), passing it the same second
    argument. The caller MAY call cwal_visit_acyclic_end() if this
    function returns non-0, but is not obligated to.

    @see cwal_visit_props_begin()
    @see cwal_visit_list_begin()
    @see cwal_visit_acyclic_end()
*/
int cwal_visit_acyclic_begin( cwal_value * const v, int * const opaque );

/** @internal

    MUST be called if, and only if, the previous call to
    cwal_visit_acyclic_begin() returned 0. MAY be called if the
    previous call to cwal_visit_acyclic_begin() returned non-0, but it
    is not required.

    The 2nd argument must be the same value which was passed to
    cwal_visit_acyclic_begin(), as it contains state relevant to the
    cycle-checking process.

    @see cwal_visit_acyclic_begin()
*/
void cwal_visit_acyclic_end( cwal_value * const v, int opaque );

/** @internal

    Internal helper to flag property visitation/traversal. If this is
    called, the value stored in *opaque MUST be passed to
    cwal_visit_props_end() in the same logical code block.

    v MUST be a type capable of property iteration (and not a
    builtin).

    Calling this obliges the caller to pass the value of *opaque
    to cwal_visit_props_end() when visitation is complete.

    The API guarantees that this routine will set *opaque to a non-0
    value, so that callers may use 0 for their own purposes (e.g.
    determining whether or not they need to call
    cwal_visit_props_end()).

    This function may set the CWAL_RCF_IS_VISITING flag on v, and
    records in the 2nd argument whether or not it did so. When
    cwal_visit_props_end() is called, if its second argument records
    that it set the flag then that call unsets that flag. This allows
    properties to be visited multiple times concurrently, with only
    the top-most visitation toggling that flag. That flag, in turn, is
    checked by routines which would invalidate such iteration, causing
    such routines to return an error code rather than invalidating the
    in-progress iteration. e.g. trying to set a property value while
    the properties are being iterated over will trigger such a case
    because the underlying data model does not support modification
    during traversal.

    @see cwal_visit_props_end()
    @see cwal_visit_acyclic_begin()
*/
void cwal_visit_props_begin( cwal_value * const v, int * const opaque );

/** @internal

    If, and only if, cwal_visit_props_begin() was passed v, the
    resulting integer value from that call MUST be passed to this
    function when property traversal is complete.

    @see cwal_visit_props_begin()
*/
void cwal_visit_props_end( cwal_value * const v, int opaque );

/** @internal

    Internal helper to flag property visitation. If this is called,
    the value stored in *opaque MUST be passed to
    cwal_visit_list_end() in the same logical code block.

    v MUST be a type capable of list iteration (and not a builtin).

    Calling this obliges the caller to pass the value of *opaque
    to cwal_visit_list_end() when visitation is complete.

    The API guarantees that this routine will set *opaque to a non-0
    value, so that callers may use 0 for their own purposes (e.g.
    determining whether or not they need to call
    cwal_visit_props_end()).

    If called recursively, only the top-most call will modify the
    visitation flag.

    @see cwal_visit_list_end()
*/
void cwal_visit_list_begin( cwal_value * const v, int * const opaque );

/** @internal

    If, and only if, cwal_visit_list_begin() was passed v, the
    resulting integer value from that call MUST be passed to this
    function when property traversal is complete.

    @see cwal_visit_list_begin()
*/
void cwal_visit_list_end( cwal_value * const v, int opaque );

/**
   Internal helper for iterating over cwal_obase properties.
*/
struct cwal_obase_kvp_iter {
  cwal_value * v;
  cwal_kvp const * current;
#if CWAL_OBASE_ISA_HASH
  cwal_obase * base;
  cwal_list const * li;
  cwal_midsize_t ndx;
#endif
};
typedef struct cwal_obase_kvp_iter cwal_obase_kvp_iter;

/** @internal

    Initializes oks for iteration over v's properties and returns the
    first property.  Returns NULL if v has no properties.

    This routine may assert that v is currently marked as is-visiting
    or is-list-visiting.

    Use cwal_obase_kvp_iter_next() to iterate over subsequent entries.
*/
cwal_kvp const * cwal_obase_kvp_iter_init( cwal_value * const v,
                                           cwal_obase_kvp_iter * const oks );

/** @internal

    Returns the next property in oks's state, or NULL once the end of
    the property list has been reached.

    This routine may assert that v is currently marked as is-visiting
    or is-list-visiting.

    Results are undefined if oks has has previously been passed to
    cwal_obase_kvp_iter_init().
*/
cwal_kvp const * cwal_obase_kvp_iter_next( cwal_obase_kvp_iter * const oks );

/* LICENSE

This software's source code, including accompanying documentation and
demonstration applications, are licensed under the following
conditions...

Certain files are imported from external projects and have their own
licensing terms. Namely, the JSON_parser.* files. See their files for
their official licenses, but the summary is "do what you want [with
them] but leave the license text and copyright in place."

The author (Stephan G. Beal [http://wanderinghorse.net/home/stephan/])
explicitly disclaims copyright in all jurisdictions which recognize
such a disclaimer. In such jurisdictions, this software is released
into the Public Domain.

In jurisdictions which do not recognize Public Domain property
(e.g. Germany as of 2011), this software is Copyright (c) 2011-2018 by
Stephan G. Beal, and is released under the terms of the MIT License
(see below).

In jurisdictions which recognize Public Domain property, the user of
this software may choose to accept it either as 1) Public Domain, 2)
under the conditions of the MIT License (see below), or 3) under the
terms of dual Public Domain/MIT License conditions described here, as
they choose.

The MIT License is about as close to Public Domain as a license can
get, and is described in clear, concise terms at:

    http://en.wikipedia.org/wiki/MIT_License

The full text of the MIT License follows:

--

Copyright (c) 2011-2021 Stephan G. Beal
(https://wanderinghorse.net/home/stephan/)

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

--END OF MIT LICENSE--

For purposes of the above license, the term "Software" includes
documentation and demonstration source code which accompanies
this software. ("Accompanies" = is contained in the Software's
primary public source code repository.)

*/

#if defined(__cplusplus)
} /*extern "C"*/
#endif

#endif /* WANDERINGHORSE_NET_CWAL_INTERNAL_H_INCLUDED */
