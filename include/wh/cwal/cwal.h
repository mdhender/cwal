/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
#if !defined(WANDERINGHORSE_NET_CWAL_H_INCLUDED)
#define WANDERINGHORSE_NET_CWAL_H_INCLUDED 1

#include "cwal_config.h"
#include <stdarg.h> /* va_list */
#include <stdio.h> /* FILE decl */
#include <stdbool.h>

/**
   @page page_cwal cwal API

   cwal (pronounced "sea wall") is the Scriping Engine Without A
   Language (sewal, shortened to cwal): an object-oriented C99 API
   providing _part_ of a scripting engine, namely the engine and not
   the scripting. The intention is that custom scripting
   languages/mini-languages can be written on top of this basis, which
   takes care of the core type system, de/allocation of values,
   tracking references, and other low-level work which is largely
   independent of any specific language syntax. Its design does impose
   some conventions/requirements on host languages, but nothing too
   onerous, it is hoped. Alternately, it can be used as sort of
   garbage collection system for clients willing to use its type
   system. Another potential use might be a data-binding mechanism
   between, e.g. database drivers and client code, acting as a
   type-normalization layer between the two (so the client code can be
   written without much knowledge of the underlying driver(s)).

   cwal's API is stable in the sense that much client-side code relies
   on it which we don't want broken, but will always be beta in the
   sense that it's open to experimentation and change. It is very rare
   (nowadays) that it is modified in ways which "break stuff."  As of
   mid-2014 we have a good deal of add-on code, in particular with the
   s2 scripting engine, which we would like to keep running, so
   massive changes are unlikely. Though s2 is co-developed with cwal,
   the core cwal engine is intentionally kept free of
   scriping-language-specific artifacts.

   Project home page: https://fossil.wanderinghorse.net/r/cwal

   Author: Stephan Beal (https://www.wanderinghorse.net/home/stephan/)

   License: Dual Public Domain/MIT

   The full license text is in the main header file (cwal.h or
   cwal_amalgamation.h, depending on the distribution used): search
   the file for the word LICENSE. The optional JSON parser, from a 3rd
   party, has its own license, equivalent to the MIT license.

   Examples of how to use the library are scattered throughout the API
   documentation, in the test.c file in the source repo, in the wiki
   on the project home page, and (in a big way) in the s2 subdirectory
   of the main source tree.

   Main properties of cwal:

   - cwal does NOT provide a scripting language. It is an engine which
   "could" be used as a basis for one. It can also be used as simple
   form of garbage collector for client apps willing to live with its
   scoping mechanism.

   - Provides a type system similar (not identical) to that of
   ECMAScript. Values are opaque handles with some polymorphic
   behaviours depending on their logical type. Provides support for
   binding client-specified "native" values, such that they can
   participate in the normal lifetime tracking and destruction process
   and can be converted from their Value handles with 100%
   type-safety.

   - Uses a reference-counting/scope-ownership hybrid for value
   sharing and garbage collection. Its destruction mechanism behaves
   sanely when faced with cycles provided the rules of the API are
   followed (they're pretty easy).

   - Destruction (finalizer calls) is gauranteed and happens more or
   less like it does in C++, with the addition that refcounting can be
   used to extend the lifetime of a value outside of the scope where
   it is created. While refcounting in conjunction with container can
   make destruction order somewhat difficult to predict, it is still
   deterministic in that finalizers are guaranteed to be called, so
   long as the API is properly used.

   - The wrapping of client-provided native types uses a type-safety
   mechanism to ensure that clients are getting the type of pointer
   they expect when fetching the (void*) from its cwal value type
   counterpart. Such types participate as first-class Values and may
   contain properties (meaning they can participate in graphs).

   - Provides optional automatic "internalizing" of new string values,
   and all strings which share the same length and byte content are
   automatically shared via the reference-counting mechanism. When the
   final reference is released the string is un-internalized
   automatically. This causes a couple corner cases internally but can
   drastrically reduce allocations in scripts which make heavy use of
   identifier strings.

   - Highly optimized to reduce calls to malloc() and free(), and
   optionally makes use of memory recycling. The recycling limits can
   be set on a per-data-type basis. Practice has shown recycling to
   be tremendously effective at reducing malloc calls by over 90%
   in typical script code (a 98% reduction is not uncommon!).

   - Optionally provides tracing (via callbacks) so that clients
   can see what it is doing on the inside.

   - "Relatively clean" code is a design goal, as is having relatively
   good documentation.
*/

/**
   @page page_cwal_gc cwal Garbage Collection

   This section describes how cwal manages the memory of Values
   created by the client.
   
   cwal's main concern, above all, is memory management. This includes
   at least the following aspects:

   - Allocation and deallocation of raw memory. All memory allocation
   in the context of cwal is handled via a cwal_engine instance (a
   context type of which an application has one per "scripting
   engine"), and clients may optionally specify their own allocator
   when initializing a cwal_engine instance. cwal internally manages a
   number of optional recycling bins, broken down by data type, where
   it stores freed Values (and other internals) for re-use. Once the
   recycle bins have been populated a bit, it has to allocate memory
   far less often (how often depends on usage and recycler
   configuration).

   - Tracking the lifetimes of components which clients cannot
   reasonably track themselves. This refers specifically to Values,
   since their inter-relationships (via hierachies and key/value
   properties) can easily lead to unpredictable patterns which would
   be unrealistically burdensome for client code to try to properly
   manage.

   cwal's garbage collection mechanism has two basic components:
   reference counts and scopes.

   cwal uses convential refcount semantics. The reference count of a
   Value is increased when code "expresses an interest" in the Value,
   e.g. it is inserted into a container or an explicit call to
   cwal_value_ref() is called.  Reference counts are decreased as
   Values are removed from containers (e.g. Objects and Arrays) or
   cwal_value_unref() is called. When the refcount goes to 0, the
   Value is typically cleaned up (recursively for containers), but the
   engine also offers an operation which says "reduce the refcount but
   do not destroy the object if it reaches 0," which is amazingly
   useful in lifetime management of opaque values.

   Scopes act as a "root" for a collection of values, such that
   cleaning up a scope will clean up all values under that scope
   (regardless of their reference count). cwal uses C++-like scoping
   rules: a stack of scopes, only the youngest of which is active at
   any given time.

   Reference counting is straightforward and easy to implement but is
   conventionally problematic when it comes to managing graphs of
   Values (i.e. cyclic data structures). When a cwal scope is closed,
   it destructs any values it owns by iteratively dereferencing
   them. This process removes the values from the scope in such a way
   that graphs get weeded out incrementally (one level at a time) and
   (more or less) cleanly. Consider this pseudo-script-code:

   var obj = new Object();
   obj.set(obj, obj); // (key, value)

   (Admitedly unusual, but cwal's value system supports this.)

   We now have an object with 3 references: 1 held by the identifier
   mapping (i.e. the declared variable), one held by the property
   key, and one held by the property value. (Possibly others,
   depending on the scripting implementation.)

   Now we do:

   unset obj

   That removes the identifier and its reference. That leaves us with
   a refcount of 2 and no handle back to the object. The object must
   be kept alive because there are references (the key/value refering
   to the object itself). That value will remain an orphan until its
   owning scope is cleaned, at which point the scope's cleaning
   process will weed out the cycles and finalize the object when the
   final reference is removed. (cwal_scope_vacuum() can weed that out
   and clean it up, though.)

   There are of course wrinkles in that equation. For example, the
   finalization process for an object must recursively climb down
   properties, and in the above case doing so will trigger
   finalization of an object while it is traversing itself. cwal's
   cleanup mechanism temporarily delays freeing of Values' memory
   during scope cleanup, queuing them up for later destruction. The
   end effect is that Values get cleaned up but the memory remains
   valid until scope cleanup has finished. This makes it safe (if
   psychologically a bit unsettling) to traverse "destroyed" objects
   during finalization (it is a harmless no-op). Once scope cleanup is
   complete, all queued-up destroyed values are then flushed to the
   deallocator (or the recycling bin, if it has space).

   Summary: in cwal it is possible to orphan graphs in such a way that
   the client has no reference to them but circular references prevent
   cleanup. Such values will be freed when their parent scope is
   cleaned. There is also a "sweep" mechanism to trigger cleanups of
   values with no references and a more intensive "vacuum" operation
   which can also weed out unreachable cyclic structures. (Note that
   cwal does not use a "mark-and-sweep" approach: all memory it
   manages is carefully accounted for in internal bookkeeping, and
   there is no guesswork about which memory might or might not be
   "marked" for cleanup.)

   Speaking of parent scopes... cwal complements the reference
   counting with the concept of "owning scope." Every value belongs to
   exactly one scope, and this scope is always the highest-level
   (oldest) scope which has ever referenced the value. When a value is
   created, it belongs to the scope which is active at the
   time. Values can be rescoped, however, moving up in the stack (into
   an older scope, never a newer scope). When a value is inserted into
   a container, the value is scoped into the container's owning
   scope. When the container is added to another, the container is
   recursively re-scoped (if necessary) to match the scope of its
   parent container. This ensures that values from a higher-level
   (older) scope are always valid through the lifetime of lower-level
   (newer) scopes. It also allows a scope to pass a value to its
   parent scope. When a value is re-scoped, it is removed from its
   owning scope's management list and added to the new scope's
   list. These list are linked lists and use O(1) algos for the list
   management (except for cleanup, which is effectively O(N) on the
   number of values being cleaned up).

   As of this writing, in Dec. 2019, cwal has been in moderately heavy
   use for more than 5 years, first in the now-deceased "th1ish"
   scripting engine and subsequently in the "s2" engine, which has
   demonstrated, beyond the shadow of a doubt, the engine's ability to
   handle modest scripting-language needs. s2's range of features
   extend far, far beyond any initial hopes i had for cwal, and
   provide me with a useful tool with which to script my other
   software or do certain day-to-day tasks such as generate static
   pages for my website from s2-enabled template files (kind of like a
   poor man's PHP) and power a number of my JSON-based CGI
   applications (backend services for various web apps).
*/
#if defined(__cplusplus)
extern "C" {
#endif

/* Forward declarations. Most of these types are opaque to client
   code. */
typedef struct cwal_scope cwal_scope;
typedef struct cwal_engine_vtab cwal_engine_vtab;
typedef struct cwal_engine cwal_engine;
typedef struct cwal_value cwal_value;
typedef struct cwal_array cwal_array;
typedef struct cwal_object cwal_object;
typedef struct cwal_string cwal_string;
typedef struct cwal_kvp cwal_kvp;
typedef struct cwal_native cwal_native;
typedef struct cwal_buffer cwal_buffer;
typedef struct cwal_exception cwal_exception;
typedef struct cwal_hash cwal_hash;
typedef struct cwal_weakref cwal_weakref;
typedef struct cwal_callback_hook cwal_callback_hook;
typedef struct cwal_callback_args cwal_callback_args;
typedef struct cwal_tuple cwal_tuple;
typedef struct cwal_error cwal_error;
typedef struct cwal_propref cwal_propref;

/**
   Typedef for flags fields which are limited to 16 bits.
*/
typedef uint16_t cwal_flags16_t;
/**
   Typedef for flags fields which are limited to 32 bits.
*/
typedef uint32_t cwal_flags32_t;

/**
   A callback type for pre-call callback hooks. If a hook is
   installed via cwal_callback_hook_set(), its "pre" callback is
   called before any "script-bound" function is called.

   The first parameter is the one which will be passed to the
   callback and post-callback hook if this hook succeeds.

   The state parameter (2nd argument) is the `state` member of the
   cwal_callback_hook passed to cwal_callback_hook_set(). Its
   interpretation is implementation-defined.

   The callback must return 0 on success. On error, the non-0
   result code (from the CWAL_RC_xxx set) is returned from the
   cwal API which triggered the hook (cwal_function_call() and
   friends). On success, the callback is called and then
   (regardless of callback success!) the "post" callback is
   called.

   The intention of the callback hook mechanism is to give script
   engines a places to do pre-call setup such as installing local
   variables (e.g. similar to JavaScript's "this", "arguments", and
   "arguments.callee"). It can also be used for instrumentation
   purposes (logging) or to implement multi-casting of client-side
   pre-call hook mechanism to multiple listeners.

   Note that the hook mechanism is "global" - it affects all
   cwal_function_call_in_scope() calls, which means all
   script-triggered callbacks and any other place a
   cwal_function_call() (or similar) is used. It _is_ possible
   (using cwal_function_state_get()) for a client to determine
   whether the callback exists in script code or native code,
   which means that hooks can act dependently of that if they need
   to. e.g. there is generally no need to inject "this" and
   "arguments" symbols into native-level callbacks, whereas it is
   generally useful to do so for script-side callbacks. Native
   callbacks have access to the same information via the argv
   object, so they don't need scope-level copies of those values.
   (That said, native callbacks which call back into script code
   might need such variables in place!)

   @see cwal_callback_hook_post_f()
   @see cwal_callback_hook
   @see cwal_callback_hook_set()
   @see cwal_callback_f
*/
typedef int (*cwal_callback_hook_pre_f)(cwal_callback_args const * argv, void * state);

/**
   The "post" counterpart of cwal_callback_hook_pre_f(), this hook is
   called by the cwal core after calling a cwal_function via and of
   the cwal_function_call() family of functions.

   The state parameter (2nd argument) is the one passed to
   cwal_callback_hook_set(). Its interpretation is
   implementation-defined.

   The fRc (3rd) parameter is the return code of the callback, in case
   it interests the hook (e.g. error logging). If this value is not 0
   then the rv parameter will be NULL.

   The rv (4rd) parameter is the result value of the function. The
   engine will re-scope rv, if necessary, brief milliseconds after
   this hook returns. rv MAY be NULL, indicating that the callback did
   not set a value (which typically equates to the undefined value
   (cwal_value_undefined()) downstream or may be because an exception
   was thrown or a non-exception error code was returned).

   If the "pre" hook is called and returns 0, the API guarantees
   that the post-hook is called. Conversely, if the pre hook
   returns non-0, the post hook is never called.

   If this callback returns non-0, that will be the result returned
   from the cwal_function_call()-like function which triggered it.

   @see cwal_callback_hook_pre_f()
   @see cwal_callback_hook
   @see cwal_callback_hook_set()
*/
typedef int (*cwal_callback_hook_post_f)(cwal_callback_args const * argv,
                                         void * state,
                                         int fRc, cwal_value * rv);

/**
   Holds state information for a set of cwal_engine callback
   hooks.

   @see cwal_callback_hook_pre_f()
   @see cwal_callback_hook_post_f()
   @see cwal_callback_hook_set()
*/
struct cwal_callback_hook {
  /**
     Implementation-dependent state pointer which gets passed
     as the 2nd argument to this->pre() and this->post().
  */
  void * state;
  /**
     The pre-callback hook. May be NULL.
  */
  cwal_callback_hook_pre_f pre;
  /**
     The post-callback hook. May be NULL.
  */
  cwal_callback_hook_post_f post;
};

/**
   An initialized-with-defaults instance of cwal_callback_hook,
   intended for const-copy intialization.
*/
#define cwal_callback_hook_empty_m {NULL,NULL,NULL}

/**
   An initialized-with-defaults instance of cwal_callback_hook,
   intended for copy intialization.
*/
extern const cwal_callback_hook cwal_callback_hook_empty;


/**
   The set of result codes used by most cwal API routines.  Not all of
   these are errors, per se, and may have context-dependent
   interpretations.

   Client code MUST NOT rely on any of these entries having a
   particular value EXCEPT for CWAL_RC_OK, which is guaranteed to
   be 0. All other entries are guaranteed to NOT be 0, but their
   exact values may change from time to time. The values are
   guaranteed to stay within a standard enum range of signed
   integer bits, but no other guarantees are made (e.g. whether
   the values are positive or negative, 2 digits or 5, are
   unspecified and may change).
*/
enum cwal_rc_e {
/**
   The canonical "not an error" code.
*/
CWAL_RC_OK = 0,
/**
   Generic "don't have anything better" error code.
*/
CWAL_RC_ERROR = 1,
/**
   Out-of-memory.
*/
CWAL_RC_OOM = 2,
/**
   Signifies that the cwal engine may be in an unspecific state
   and must not be used further.
*/
CWAL_RC_FATAL = 3,
/**
   Signals that the returning operation wants one of its callers
   to implement "continue" semantics.
*/
CWAL_RC_CONTINUE = 101,
/**
   Signals that the returning operation wants one of its callers
   to implement "break" semantics.
*/
CWAL_RC_BREAK = 102,
/**
   Signals that the returning operation wants one of its callers
   to implement "return" semantics.
*/
CWAL_RC_RETURN = 103,

/**
   Indicates that the interpreter should stop running
   the current script immediately.
*/
CWAL_RC_EXIT = 104,

/**
   Indicates that the interpreter "threw an exception", which
   "should" be reflected by passing a cwal_exception value down
   the call stack via one of the cwal_exception_set() family of
   functions. Callers "should" treat this return value as fatal,
   immediately passing it back to their callers (if possible),
   until a call is reached in the stack which handles this return
   type (e.g. the conventional "catch" handler), at which point
   the propagation should stop.
*/
CWAL_RC_EXCEPTION = 105,

/**
   Indicates that the interpreter triggered an assertion. Whether
   these are handled as outright fatal errors or exceptions (or some
   other mechanism) is up to the interpreter. It is also sometimes
   used in non-debug builds to flag cwal_engine::fatalCode when an
   assert() would have triggered in a debug build.
*/
CWAL_RC_ASSERT = 106,

/**
   Indicates that some argument value is incorrect or a precondition
   is not met.
*/
CWAL_RC_MISUSE = 201,
/**
   Indicates that a resource being searched for was not found.
*/
CWAL_RC_NOT_FOUND = 301,
/**
   Indicates that a resource being searched for or replaced already
   exists (whether or not this is an error is context-dependent).
*/
CWAL_RC_ALREADY_EXISTS = 302,
/**
   A more specific form of CWAL_RC_MISUSE, this indicates that some
   value (argument or data a routine depends on) is "out of range."
*/
CWAL_RC_RANGE = 303,
/**
   Indicates that some value is not of the required type (or family of
   types).
*/
CWAL_RC_TYPE = 304,
/**
   Indicates an unsupported/not-yet-implemented operation was
   requested.
*/
CWAL_RC_UNSUPPORTED = 305,

/**
   Indicates that access to some resource was denied.
*/
CWAL_RC_ACCESS = 306,

/**
   Indicates that an operation has failed because the value in
   question is a container which is being visited, and the operation
   in question is illegal for being-visited containers.

   Historically we have used CWAL_RC_ACCESS to note that iteration or
   modification is not allowed because it would be recursive. That
   code has proven to be ambiguous in some contexts, so this code was
   added specifically for that case.
*/
CWAL_RC_IS_VISITING = 307,

/**
   Indicates that an operation was requested which is disallowed
   because the resources is currently iterating over a list component
   which would be negatively affected. An example would be resizing an
   array while it's being sorted, or sorting twice concurrently.
*/
CWAL_RC_IS_VISITING_LIST = 308,

/**
   Indicates a CWAL_CONTAINER_DISALLOW_NEW_PROPERTIES constraint
   violation.

   A special case of CWAL_RC_ACCESS.
*/
CWAL_RC_DISALLOW_NEW_PROPERTIES = 309,

/**
   Indicates a CWAL_CONTAINER_DISALLOW_PROP_SET constraint violation.

   A special case of CWAL_RC_ACCESS.
*/
CWAL_RC_DISALLOW_PROP_SET = 310,

/**
   Indicates a CWAL_CONTAINER_DISALLOW_PROTOTYPE_SET constraint
   violation.

   A special case of CWAL_RC_ACCESS.
*/
CWAL_RC_DISALLOW_PROTOTYPE_SET = 311,

/**
   Indicates a CWAL_VAR_F_CONST constraint violation.

   A special case of CWAL_RC_ACCESS.
*/
CWAL_RC_CONST_VIOLATION = 312,

/**
   Indicates that a value is locked against the requested operation
   for some reason. e.g. an array may not be traversed while a sort()
   is underway.

   A special case of CWAL_RC_ACCESS.
*/
CWAL_RC_LOCKED = 313,

/**
   Indicates that visitation of a value would step into a cycle
   collision. Whether or not this is an error is context-dependent.
*/
CWAL_RC_CYCLES_DETECTED = 401,
/**
   Used by value cleanup to help ensure that values with cycles do not
   get freed multiple times. Returned only by cwal_value_unref() and
   friends and only when a value encounters a reference to itself
   somewhere in the destruction process. In that context this value is
   a flag, not an error, but it is also used in assert()s to ensure
   that pre- and post-conditions involving cycle traversal during
   destruction are held.
*/
CWAL_RC_DESTRUCTION_RUNNING = 402,

/**
   Returned by cwal_value_unref() when it really finalizes a value.
*/
CWAL_RC_FINALIZED = 403,

/**
   Returned by cwal_value_unref() when it really the value it is given
   still has active references after unref returns.
*/
CWAL_RC_HAS_REFERENCES = 404,

/**
   Indicates that the library detected memory corruption, invariably
   caused by value lifetime mismanagement.
 */
CWAL_RC_CORRUPTION = 405,

/**
   Reserved for future use.
*/
CWAL_RC_INTERRUPTED = 501,
/**
   Reserved for future use.
*/
CWAL_RC_CANCELLED = 502,
/**
   Indicates an i/o error of some sort.
*/
CWAL_RC_IO = 601,
/**
   Intended for use by routines which normally assert() a
   particular condition but do not do so when built in non-debug
   mode.
*/
CWAL_RC_CANNOT_HAPPEN = 666,

CWAL_RC_JSON_INVALID_CHAR = 700,
CWAL_RC_JSON_INVALID_KEYWORD,
CWAL_RC_JSON_INVALID_ESCAPE_SEQUENCE,
CWAL_RC_JSON_INVALID_UNICODE_SEQUENCE,
CWAL_RC_JSON_INVALID_NUMBER,
CWAL_RC_JSON_NESTING_DEPTH_REACHED,
CWAL_RC_JSON_UNBALANCED_COLLECTION,
CWAL_RC_JSON_EXPECTED_KEY,
CWAL_RC_JSON_EXPECTED_COLON,
    
/**
   The CWAL_SCR_xxx family of values are intended for use by
   concrete scripting implementations based on cwal.

   2020-02-10: in practice, the only one of the SCR values which sees
   any use is CWAL_SCR_SYNTAX. The rest are, it turns out, essentially
   special cases of conditions which are covered just as well by the
   more generic CWAL_RC_xxx values. s2 also uses CWAL_SCR_DIV_BY_ZERO,
   but that could just as easily be covered by CWAL_RC_RANGE.
*/
CWAL_SCR_readme = 2000,

/**
   Indicates that the provided token "could not be consumed" by
   the given handler, but that there is otherwise no known error.
*/
CWAL_SCR_CANNOT_CONSUME,

/**
   Indicates that an invalid operation was performed on a value, or
   that the type(s) required for a given operation are incorrect.

   More concrete case of CWAL_RC_TYPE or CWAL_RC_UNSUPPORTED.
*/
CWAL_SCR_INVALID_OP,

/**
   Special case of CWAL_RC_NOT_FOUND, indicates that an
   identifier string could not be found in the scope
   path.
*/
CWAL_SCR_UNKNOWN_IDENTIFIER,

/**
   Indicates a (failed) attempt to call() a non-Function value.
   More concrete case of CWAL_RC_TYPE.
*/
CWAL_SCR_CALL_OF_NON_FUNCTION,

/**
   More concrete case of CWAL_SCR_SYNTAX.
*/
CWAL_SCR_MISMATCHED_BRACE,

/**
   More concrete case of CWAL_SCR_SYNTAX.
*/
CWAL_SCR_MISSING_SEPARATOR,
/**
   More concrete case of CWAL_SCR_SYNTAX.
*/
CWAL_SCR_UNEXPECTED_TOKEN,

/**
   Indicates division or modulus by 0 would have been attempted.
*/
CWAL_SCR_DIV_BY_ZERO,
/**
   Indicates a generic syntax error.
*/
CWAL_SCR_SYNTAX,
/**
   Indicates that an unexpected EOF was encountered (e.g. while
   reading a string literal).
*/
CWAL_SCR_UNEXPECTED_EOF,
/**
   Indicates EOF was encountered. Whether or not this is an error
   is context-dependent, and CWAL_SCR_UNEXPECTED_EOF is intended
   for the error case.
*/
CWAL_SCR_EOF,
/**
   More concrete case of CWAL_RC_RANGE. 
*/
CWAL_SCR_TOO_MANY_ARGUMENTS,
/**
   More concrete case of CWAL_SCR_SYNTAX.
*/
CWAL_SCR_EXPECTING_IDENTIFIER,

/**
   The "legal" result code starting point for adding client-specific
   RC values for use with cwal. In other words, do not use codes with
   values lower than this one in cwal-bound client-side code, e.g. as
   return codes from cwal_callback_f() implementations, as doing so
   risks "semantic collisions" with cwal-internal/cwal-conventional
   handling of certain codes. Examples: CWAL_RC_OOM is used solely to
   propagate out-of-memory (OOM) conditions and CWAL_RC_EXCEPTION
   signifies that a script-side exception has been thrown (which
   typically requires different handling than other C-level errors).

   The range between CWAL_RC_CLIENT_BEGIN1 and CWAL_RC_CLIENT_BEGIN2
   is reserved for scripting engines. CWAL_RC_CLIENT_BEGIN2 denotes
   the lowest-numbered result code which should ever be used by
   clients of those engines when passing codes back through the cwal
   API.
*/
CWAL_RC_CLIENT_BEGIN1 = 3000,

/**
   See CWAL_RC_CLIENT_BEGIN1.
*/
CWAL_RC_CLIENT_BEGIN2 = CWAL_RC_CLIENT_BEGIN1 + 3000
};

/** Convenience typedef. */
typedef enum cwal_rc_e cwal_rc;

/**
   Compile-time limits not covered by configuration macros.
*/
enum cwal_e_options_e {
/**
   Max number of arguments cwal_function_callf() and (variadic)
   friends. Remember that each one takes up sizeof(cwal_value*) in
   stack space.
*/
CWAL_OPT_MAX_FUNC_CALL_ARGS = 32
};


/**
   A collection of values which control what tracing messages get
   emitted by a cwal_engine.

   By an unfortunate fluke of mis-design, entries which are
   themselves not group masks (groups are named xxxx_MASK) cannot
   be effecitvely mixed together via bitmasking. The end effect is
   that only the MASK, NONE, or ALL entries can be
   usefully/predictibly applied. i'll see about fixing that.
*/
enum cwal_trace_flags_e {

CWAL_TRACE_NONE = 0,
CWAL_TRACE_GROUP_MASK = 0x7F000000,

CWAL_TRACE_MEM_MASK = 0x01000000,
CWAL_TRACE_MEM_MALLOC = CWAL_TRACE_MEM_MASK | (1 << 0),
CWAL_TRACE_MEM_REALLOC = CWAL_TRACE_MEM_MASK | (1 << 1),
CWAL_TRACE_MEM_FREE = CWAL_TRACE_MEM_MASK | (1 << 2),
CWAL_TRACE_MEM_TO_RECYCLER = CWAL_TRACE_MEM_MASK | (1 << 3),
CWAL_TRACE_MEM_FROM_RECYCLER = CWAL_TRACE_MEM_MASK | (1 << 4),
CWAL_TRACE_MEM_TO_GC_QUEUE = CWAL_TRACE_MEM_MASK | (1 << 5),

CWAL_TRACE_VALUE_MASK = 0x02000000,
CWAL_TRACE_VALUE_CREATED = CWAL_TRACE_VALUE_MASK | (1 << 0),
CWAL_TRACE_VALUE_SCOPED = CWAL_TRACE_VALUE_MASK | (1 << 1),
CWAL_TRACE_VALUE_UNSCOPED = CWAL_TRACE_VALUE_MASK | (1 << 2),
CWAL_TRACE_VALUE_CLEAN_START = CWAL_TRACE_VALUE_MASK | (1 << 3),
CWAL_TRACE_VALUE_CLEAN_END = CWAL_TRACE_VALUE_MASK | (1 << 4),
CWAL_TRACE_VALUE_CYCLE = CWAL_TRACE_VALUE_MASK | (1 << 5),
CWAL_TRACE_VALUE_INTERNED = CWAL_TRACE_VALUE_MASK | (1 << 6),
CWAL_TRACE_VALUE_UNINTERNED = CWAL_TRACE_VALUE_MASK | (1 << 7),
CWAL_TRACE_VALUE_VISIT_START = CWAL_TRACE_VALUE_MASK | (1 << 8),
CWAL_TRACE_VALUE_VISIT_END = CWAL_TRACE_VALUE_MASK | (1 << 9),
CWAL_TRACE_VALUE_REFCOUNT = CWAL_TRACE_VALUE_MASK | (1 << 10),

CWAL_TRACE_SCOPE_MASK = 0X04000000,
CWAL_TRACE_SCOPE_PUSHED = CWAL_TRACE_SCOPE_MASK | (1 << 0),
CWAL_TRACE_SCOPE_CLEAN_START = CWAL_TRACE_SCOPE_MASK | (1 << 1),
CWAL_TRACE_SCOPE_CLEAN_END = CWAL_TRACE_SCOPE_MASK | (1 << 2),
CWAL_TRACE_SCOPE_SWEEP_START = CWAL_TRACE_SCOPE_MASK | (1 << 3),
CWAL_TRACE_SCOPE_SWEEP_END = CWAL_TRACE_SCOPE_MASK | (1 << 4),

CWAL_TRACE_ENGINE_MASK =0X08000000,
CWAL_TRACE_ENGINE_STARTUP = CWAL_TRACE_ENGINE_MASK | (1 << 1),
CWAL_TRACE_ENGINE_SHUTDOWN_START = CWAL_TRACE_ENGINE_MASK | (1 << 2),
CWAL_TRACE_ENGINE_SHUTDOWN_END = CWAL_TRACE_ENGINE_MASK | (1 << 3),

CWAL_TRACE_FYI_MASK = 0x10000000,
CWAL_TRACE_MESSAGE = CWAL_TRACE_FYI_MASK | (1<<1),

CWAL_TRACE_ERROR_MASK = 0x20000000,
CWAL_TRACE_ERROR = CWAL_TRACE_ERROR_MASK | (1<<0),
    
/**
   Contains all cwal_trace_flags_e values except CWAL_TRACE_NONE.
*/
CWAL_TRACE_ALL = 0x7FFFFFFF/*1..31*/

};
/**
   Convenience typedef.
*/
typedef enum cwal_trace_flags_e cwal_trace_flags_e;

    
#if CWAL_ENABLE_TRACE
typedef struct cwal_trace_state cwal_trace_state;
/**
   State which gets passed to a cwal_engine_tracer_f() callback when
   cwal_engine tracing is enabled.
*/
struct cwal_trace_state {
  cwal_trace_flags_e event;
  int32_t mask;
  cwal_rc code_NYI;
  cwal_engine const * e;
  cwal_value const * value;
  cwal_scope const * scope;
  void const * memory;
  cwal_size_t memorySize;
  char const * msg;
  cwal_size_t msgLen;
  char const * cFile;
  char const * cFunc;
  int cLine;
};
#else
typedef char cwal_trace_state;
#endif

#if CWAL_ENABLE_TRACE
#  define cwal_trace_state_empty_m {                    \
    CWAL_TRACE_NONE/*event*/,                           \
    0/*mask*/, CWAL_RC_OK/*code*/,                    \
    0/*engine*/,0/*scope*/,0/*value*/,                \
    0/*mem*/,0/*memorySize*/,0/*msg*/,0/*msgLen*/,    \
    0/*cFile*/,0/*cFunc*/,0/*cLine*/               \
  }
#else
#  define cwal_trace_state_empty_m 0
#endif
extern const cwal_trace_state cwal_trace_state_empty;
/**
   Converts the given cwal_rc value to "some string", or returns an
   unspecified string if rc is not a cwal_rc value. The returned bytes
   are always the same for a given value, and static, and are thus
   guaranteed to survive at least until main() returns or exit() is
   called.

   If passed a non-cwal_rc value then it will delegate the call to
   one of its registered fallbacks.

   @see cwal_rc_cstr_fallback()
*/
char const * cwal_rc_cstr(int rc);

/**
   Functionally identical to cwal_rc_cstr() except that it returns 0
   if rc is not a code known by this library or one of its registered
   rc-to-string fallbacks.

   @see cwal_rc_cstr_fallback().
*/
char const * cwal_rc_cstr2(int rc);

/**
   Callback signature for a cwal_rc_cstr() fallback. This is intended
   to allow downstream API extensions to plug in their own rc codes
   into cwal_rc_cstr(), such that certain generic/high-level
   algorithms do not need to specifically know about which set of
   result codes they might be dealing with (e.g. cwal vs s2).

   Implementations must accept a result code integer. If it is one of
   that API's designated codes then the function must return a pointer
   to static/immutable memory which contains a human-readable name for
   that code (by convention the string form of its enum value), in the
   same style that cwal_rc_cstr() does. If it does not recognize the
   code then it must return NULL so that the next fallback (if any)
   can be tried.

   @see cwal_rc_cstr_fallback()
*/
typedef char const * (*cwal_rc_cstr_f)(int);

/**
   Installs a fallback handler for cwal_rc_str() and cwal_rc_cstr2(),
   such that if those routines cannot answer a request then they will
   try one of the registered fallbacks.

   CAVEATS:

   - The list of callbacks has a hard-coded maximum size and exceeding
   it will trigger an assert() (in debug builds) or an abort() (in
   non-debug). It is not expected that more than one, maybe two, such
   fallbacks will ever be useful/necessary in a given binary.

   - This function is not thread-safe. It is intended to be called
   once, maybe twice, during an app's main() and then never touched
   again.

   - Fallbacks are called in the _reverse_ order of which they are
   registered.
 */
void cwal_rc_cstr_fallback(cwal_rc_cstr_f);

/**
   Type IDs used by cwal. They correspond roughly to
   JavaScript/JSON-compatible types, plus some extensions.

   These are primarily in the public API to allow O(1) client-side
   dispatching based on cwal_value types, as opposed to using
   O(N) if/else if/else.

   Note that the integer values assigned here are not guaranteed to
   stay stable: they are intended only to assist human-level debug
   work in the cwal code.
*/
enum cwal_type_id {
/**
   GCC likes to make enums unsigned at times, which breaks
   strict comparison of integers with enums. Soooo...
*/
CWAL_TYPE_FORCE_SIGNED_ENUM = -1,
/**
   The special "undefined" value constant.

   Its value must be 0 for internal reasons.
*/
CWAL_TYPE_UNDEF = 0,
/**
   The special "null" value constant.
*/
CWAL_TYPE_NULL = 1,
/**
   The bool value type.
*/
CWAL_TYPE_BOOL = 2,
/**
   The integer value type, represented in this library
   by cwal_int_t.
*/
CWAL_TYPE_INTEGER = 3,
/**
   The double value type, represented in this library
   by cwal_double_t.
*/
CWAL_TYPE_DOUBLE = 4,
/** The immutable string type. This library stores strings
    as immutable UTF8.
*/
CWAL_TYPE_STRING = 5,
/** The "Array" type. */
CWAL_TYPE_ARRAY = 6,
/** The "Object" type. */
CWAL_TYPE_OBJECT = 7,
/** The "Function" type. */
CWAL_TYPE_FUNCTION = 8,
/** A handle to a generic "error" or "exception" type.
 */
CWAL_TYPE_EXCEPTION = 9,
/** A handle to a client-defined "native" handle. */
CWAL_TYPE_NATIVE = 10,
/**
   The "buffer" type, representing a generic memory buffer.
*/
CWAL_TYPE_BUFFER = 11,

/**
   Represents a hashtable type (cwal_hash), which is similar to
   OBJECT but guarantees a faster property store.
*/
CWAL_TYPE_HASH = 12,

/**
   A pseudo-type-id used internaly, and does not see use in the
   public API (it might at some future point).
*/
CWAL_TYPE_SCOPE = 13,

/**
   KVP (Key/Value Pair) is a pseudo-type-id used internally, and
   does not see use in the public API.
*/
CWAL_TYPE_KVP = 14,

/**
   Used _almost_ only internally. The only public API use
   for this entry is with cwal_engine_recycle_max() and friends.
*/
CWAL_TYPE_WEAKREF = 15,
    
/**
   Used only internally during the initialization of "external
   strings." After initializations these take the type
   CWAL_TYPE_STRING. The only public API use for this entry is
   with cwal_engine_recycle_max() and friends.
*/
CWAL_TYPE_XSTRING = 16,

/**
   Used only internally during the initialization of "z-strings."
   After initializations these take the type CWAL_TYPE_STRING.
   The only public API use for this entry is with
   cwal_engine_recycle_max() and friends.
*/
CWAL_TYPE_ZSTRING = 17,

/**
   A type which is true in a boolean context but never compares
   equivalent to any other value, including true. Used as sentries or
   guaranteed-unique property keys. They optionally wrap a single
   arbitrary value, like a single-child container.
*/
CWAL_TYPE_UNIQUE = 18,

/**
   An Array variant optimized for/restricted to a fixed size
   and with different comparison semantics.
*/
CWAL_TYPE_TUPLE = 19,

/**
   A value type which, for get/set operations, acts as a proxy
   for another container and property key.
*/
CWAL_TYPE_PROPREF = 20,

/**
   Only used internally for metrics tracking of memory stored in
   cwal_list::list.
*/
CWAL_TYPE_LISTMEM = 21,

/** CWAL_TYPE_end is internally used as an iteration sentinel and MUST
    be the last entry in this enum. */
CWAL_TYPE_end

};
/**
   Convenience typedef.
*/
typedef enum cwal_type_id cwal_type_id;

/**
   Convenience typedef.
*/
typedef struct cwal_function cwal_function;

/** @struct cwal_callback_args

    A type holding arguments generated from "script" code which
    call()s a Function value. Instances of this type are created only
    by the cwal_function_call() family of functions, never by client
    code.  The populated state is, via cwal_function_call() and
    friends, passed to the cwal_callback_f() implementation which is
    wrapped by the call()'d Function.
*/
struct cwal_callback_args{
  /**
     The engine object making the call.
  */
  cwal_engine * engine;
  /**
     The scope in which the function is called.
  */
  cwal_scope * scope;
  /**
     The "this" value for this call. this->self may be NULL, but the
     long-standing convention in cwal client code is that if a given
     context has no "this" for a call, the function itself should be
     passed as the "this".
  */
  cwal_value * self;
  /**
     In certain call contexts (namely interceptors), this gets set to
     the container in which the callee property was found. It may be
     this->self or some protototype of this->self, and may be of a
     distinctly different type from this->self.

     It may be (and usually is) NULL.

     When an cwal_propref-type value of subtype
     CWAL_PROPREF_INTERCEPTOR is resolved, it sets this to either the
     propref value or the container which contained the propref,
     depending on how the interceptor is resolved.  In some contexts
     we simply don't have a container to associate the property access
     with.
  */
  cwal_value * propertyHolder;
  /**
     The function being called.
  */
  cwal_function * callee;
  /**
     State associated with the function by native client code.
     This is set via cwal_new_function() or equivalent.

     @see cwal_args_state()
     @see cwal_function_state_get()
  */
  void * state;

  /**
     A client-provided "tag" which can be used to determine if
     this->state is of the type the client expects. This allows us
     to do type-safe conversions from (void*) to (T*). This value is
     set via the cwal_new_function() family of APIs.

     In practice this value is simply the pointer to an arbitrary
     file-static data structure. e.g.:

     @code
     // value is irrelevant - we only use the pointer
     static const my_type_id = 1;
     @endcode

     Then we internally use the address of my_type_id as the
     stateTypeID when binding native data to a function instance via
     cwal_new_function().

     @see cwal_args_state()
     @see cwal_function_state_get()
  */
  void const * stateTypeID;

  /**
     Number of arguments.
  */
  uint16_t argc;

  /**
     Array of arguments argc items long.
  */
  cwal_value * const * argv;
};
#define cwal_callback_args_empty_m                          \
  {0/*engine*/,0/*scope*/,0/*self*/,0/*propertyHolder*/,    \
   0/*callee*/,0/*state*/,NULL/*stateTypeID*/,           \
   0/*argc*/,0/*argv*/                                   \
  }
extern const cwal_callback_args cwal_callback_args_empty;

/**
   Callback function interface for cwal "script" functions. args
   contains various state information related to the call.  The
   callback returns a value to the framework by assigning *rv to it
   (assigning it to NULL is equivalent to assigning it to
   cwal_value_undefined()). Implementations can rely on rv being
   non-NULL but must not rely on any previous contents of *rv. In
   practice, callbacks are passed a pointer to an initially-NULL
   value, and callback implementations will, on success, set *rv to
   the result value.

   Callbacks must return 0 on success, CWAL_RC_EXCEPTION if they set
   the cwal exception state, or (preferably) one of the other relevant
   CWAL_RC values on a genuine error. Practice strongly suggests that
   implementations should assign a new value to *rv only if they
   "succeed" (for a client-dependent definition of "succeed"), and
   "really shouldn't" assign it a new value if they "fail" (again,
   where "fail" sometimes has as client-specific meaning).

   This interface is the heart of client-side cwal bindings, and any
   non-trivial binding will likely have many functions of this type.
   
   ACHTUNG: it is critical that implementations return CWAL_RC_xxx
   values, as the framework relies on several specific values to
   report information to the framework and to scripting engines
   built on it. e.g. CWAL_RC_RETURN, CWAL_RC_OOM, CWAL_RC_BREAK,
   and CWAL_RC_EXCEPTION are often treated specially. If clients
   return non-cwal result codes from this function, cwal may get
   confused and downstream behaviour is undefined.
*/
typedef int (*cwal_callback_f)( cwal_callback_args const * args, cwal_value ** rv );

/**
   Framework-wide interface for finalizer functions for memory managed
   by/within a cwal_engine instance. Generally speaking it must
   semantically behave like free(3), but if the implementor knows what
   s/he's doing these can also be used for "cleanup" (as opposed to
   free()ing).

   The memory to free/clean up is passed as the second argument. First
   argument is the cwal_engine instance which is (at least ostensibly)
   managing that memory, and some implementations permit this to be
   NULL.

   For semantic compatibility with free(), implementations must
   accept NULL as the 2nd argument and must "do nothing" if
   passed a NULL second argument.
*/
typedef void (*cwal_finalizer_f)( cwal_engine * e, void * m );

/**
   A cwal_finalizer_f() implementation which requires that s be a
   (FILE*). If s is not NULL and not one of (stdin, stdout, stderr)
   then this routine fclose()s it, otherwise it has no side
   effects. This implementation ignores the e parameter. If s is NULL
   this is a harmless no-op. Results are undefined if s is not NULL
   and is not a valid opened (FILE*).
*/
void cwal_finalizer_f_fclose( cwal_engine * e, void * s );

/**
   Generic list type.

   It is up to the APIs using this type to manage the "count" member
   and use cwal_list_reserve() to manage the "alloced" member.
   
   @see cwal_list_reserve()
   @see cwal_list_append()
*/
struct cwal_list {
  /**
     Array of entries. It contains this->alloced
     entries, this->count of which are "valid"
     (in use).
  */
  void ** list;
  /**
     Number of "used" entries in the list.

     Reminder to self: we could reasonably use a 32-bit size type
     even in 64-bit builds, and that would save 8 bytes per array.
     It would require many changes to list-related API signatures
     which take cwal_size_t (which may be larger than 32-bits).
  */
  cwal_midsize_t count;
  /**
     Number of slots allocated in this->list. Use
     cwal_list_reserve() to modify this. Doing so
     might move the this->list pointer but the values
     it points to will stay stable.
  */
  cwal_midsize_t alloced;

  /**
     An internal consistency/misuse marker to let us know that this
     list is currently undergoing iteration/visitation and must
     therefore not be modified. Do not use this from client-level
     code.
  */
  bool isVisiting;
};
typedef struct cwal_list cwal_list;
/**
   Empty-initialized cwal_list object.
*/
#define cwal_list_empty_m { NULL, 0, 0, false }
/**
   Empty-initialized cwal_list object.
*/
extern const cwal_list cwal_list_empty;

/**
   A helper class for holding arbitrary state with an optional
   associated finalizer. The interpretation of the state and the
   finalizer's requirements are context-specific.
*/
struct cwal_state {
  /**
     The raw data. Its interpretation is of course very
     context-specific. The typeID field can be used to "tag"
     this value with type info so that clients can ensure that
     they do not mis-cast this pointer.
  */
  void * data;
  /**
     An arbitrary "tag" value which clients can use to indicate
     that this->data is of a specific type. In practice this is
     normally set to the address of some internal structure or
     value which is not exposed via public APIs.
  */
  void const * typeID;
  /**
     Cleanup function for this->data. It may be NULL if
     this->data has no cleanup requirements or is owned by
     someone else.
  */
  cwal_finalizer_f finalize;
};
/** Convenience typedef. */
typedef struct cwal_state cwal_state;
/**
   Empty-initialized cwal_state object.
*/
#define cwal_state_empty_m { NULL, NULL, NULL }
/**
   Empty-initialized cwal_state object.
*/
extern const cwal_state cwal_state_empty;

/**
   A generic output interface intended (primarily) to be used via
   cwal_engine_vtab via cwal_output(). Script-side code which
   generates "console-style" output intended for the user should use
   cwal_output() to put it there.  An implementation of this interface
   is then responsible for sending the output somewhere.

   Must return 0 on success or an error code (preferably from cwal_rc)
   on error. Because an output mechanism can modify the output, there
   is not direct 1-to-1 mapping of input and output lengths, and thus
   it returns neither of those.

   The state parameter's meaning is implementation-specific. e.g.
   cwal_output_f_FILE() requires it to be an opened-for-writing
   (FILE*). It is up to the caller to provide a state value which is
   appropriate for the given cwal_output_f() implementation.
*/
typedef int (*cwal_output_f)( void * state, void const * src, cwal_size_t n );

/**
   Library-wide interface for allocating, reallocating, freeing memory.
   It must semantically behave like realloc(3) with the minor clarification
   that the free() operation (size==0) it must return NULL instead of
   "some value suitable for passing to free()."

   The state argument (typically) comes from the state member of the
   cwal_engine_vtab which holds one of these functions. The (mem,size)
   parameters are as for realloc(3). In summary:

   If (mem==NULL) then it must semantically behave like malloc(3).

   If (size==0) then it must sematically behave like free(3).
   
   If (mem!=NULL) and (size!=0) then it must semantically behave like
   realloc(3).
*/
typedef void * (*cwal_engine_realloc_f)( void * state, void * mem, cwal_size_t size );

/**
   A callback for use in low-level tracing of cwal-internal
   activity. It is only used when tracing is enabled via the
   CWAL_ENABLE_TRACE configuration macro.

   2020-02-10: the cwal_engine tracing features are extremely
   low-level and produce absolute tons of output. They are intended
   solely to assist cwal's developer in debugging (in particular
   during early development of the library). It has, as of this
   writing, been 5+ years since tracing has been enabled in a cwal
   build, and there are no guarantees that the traced data have been
   kept entirely relevant vis-a-vis changes in the engine since then.
   i.e. do not use tracing unless you are trying to decode the cwal
   internals.
*/
typedef void (*cwal_engine_tracer_f)( void * state, cwal_trace_state const * event );
/**
   The combined state for a cwal_engine tracer. It is only used when
   tracing is enabled via the CWAL_ENABLE_TRACE configuration macro.
*/
struct cwal_engine_tracer{
  /** Callback for the tracer. */
  cwal_engine_tracer_f trace;
  /** A finalizer for this->state. If not NULL, it gets called
      during finalization of its associated cwal_engine instance. */
  void (*close)( void * state );
  /** Optional client-specific state for tracing. Gets passed,
      without interpretation, to the callback. */
  void * state;
};
typedef struct cwal_engine_tracer cwal_engine_tracer;
#define cwal_engine_tracer_empty_m { 0, 0, 0 }
extern const cwal_engine_tracer cwal_engine_tracer_empty;
extern const cwal_engine_tracer cwal_engine_tracer_FILE;
void cwal_engine_tracer_f_FILE( void * filePtr, cwal_trace_state const * event );
void cwal_engine_tracer_close_FILE( void * filePtr );

/**
   Part of the cwal_engine_vtab interface, this
   defines the API for a memory allocator used
   by the cwal_engine API.
*/
struct cwal_allocator{
  /**
     The memory management function. cwal_engine
     uses this exclusively for all de/re/allocations.
  */
  cwal_engine_realloc_f realloc;
  /**
     State for the allocator. Its requirements/interpretation
     depend on the concrete realloc implementation.
  */
  cwal_state state;
};
/** Convenience typedef. */
typedef struct cwal_allocator cwal_allocator;
/** Empty-initialized cwal_allocator object. */
#define cwal_allocator_empty_m { 0, cwal_state_empty_m }
/** Empty-initialized cwal_allocator object. */
extern const cwal_allocator cwal_allocator_empty;
/** cwal_allocator object configured to use realloc(3). */
extern const cwal_allocator cwal_allocator_std;
    
/**
   Part of the cwal_engine_vtab interface, this defines a generic
   interface for outputing "stuff" (presumably script-generated
   text). The intention is that script-side output should all go
   through a common channel, to provide the client an easy to to
   intercept/redirect it, or to add layers like output buffer
   stacks (this particular output interface originates from such
   an implementation in TH1).
*/
struct cwal_outputer{
  cwal_output_f output;
  /**
     Intended to flush the output channel, if needed.  If not
     needed, this member may be NULL, in which case it is
     ignored, or it may simply return 0.

     It is passed this.state.data as its argument.
  */
  int (*flush)( void * state );
  cwal_state state;
};
typedef struct cwal_outputer cwal_outputer;
/** Empty-initialized cwal_outputer object. */
#define cwal_outputer_empty_m { 0, NULL, cwal_state_empty_m }
/** Empty-initialized cwal_outputer object. */
extern const cwal_outputer cwal_outputer_empty;
/**
   cwal_outputer object set up to use cwal_output_f_FILE. After
   copying this value, set it the copy's state.data to a (FILE*) to
   redirect it. If its file handle needs to be closed during cleanup,
   the state.finalize member should be set to a function which will
   close the file handle (e.g. cwal_finalizer_f_fclose()).
*/
extern const cwal_outputer cwal_outputer_FILE;

/**
   Typedef for a predicate function which tells a cwal_engine
   whether or not a given string is "internable" or not. Clients
   may provide an implementation of this via
   cwal_engine_vtab::internable. If interning is enabled, when a new
   string is created, this function will be called and passed:

   - The state pointer set in cwal_engine_vtab::internable::state.

   - The string which is about to be created as a cwal_string.

   - The length of that string.

   This function is only called for non-empty strings. Thus len is
   always greater than 0, str is never NULL, and never starts with
   a NUL byte. Client implementations need not concern themselves
   with NULL str or a len of 0.

   Once a given series of bytes have been interned, this function
   will not be called again for that same series of bytes as long
   as there is at least one live interned reference to an
   equivalent string.
*/
typedef bool (*cwal_cstr_internable_predicate_f)( void * state, char const * str, cwal_size_t len );

/**
   The default "is this string internable?" predicate. The state parameter is ignored.

   The default impl uses only a basic length cutoff point to
   determine "internalizableness."

   @see cwal_cstr_internable_predicate_f()
*/
bool cwal_cstr_internable_predicate_f_default( void * state, char const * str, cwal_size_t len );

/**
   Configuration used to optionally cap the memory allocations made
   via a cwal_engine instance (i.e. via cwal_malloc() and
   cwal_realloc()). In client apps, these are set up via the
   cwal_engine_vtab::memcap member of the vtab used to initialize a
   cwal_engine.

   It is important that these config values not be modified after
   calling cwal_engine_init(), or undefined behaviour will ensue.

   Some memory-capping features require knowing exactly how much
   memory the vtab's realloc function has doles out and freed over
   time, and the only way to do that is to over-allocate all
   allocations and store their sizes in that memory. Over-allocation
   is only enabled when an option which requires it is enabled.

   The overhead imposed by over-allocation _is_ counted against any
   byte totals configured via this type. It also, when using
   CWAL_SIZE_T_BITS=64, restricts allocation sizes to a 32-bit range
   (as an optimization/compensation for 32-bit builds).


   Client-reported memory totals (via cwal_engine_adjust_client_mem())
   are not counted by this mechanism. In practice, client-reported
   memory is allocated via cwal_malloc() or cwal_realloc(), and those
   track/honor the configured caps.
*/
struct cwal_memcap_config {
  /**
     Caps the cumulative total memory allocated by the engine.

     Rejects (re)allocations which would take the absolute allocated
     byte total (including the memcap tracking overhead) over this
     value. Such a condition is not recoverable, as the total only
     increases (never decreases) over time.

     Set to 0 to disable.

     Requires (forces enabling of) over-allocation so that
     re-allocations can be counted consistently.
  */
  uint64_t maxTotalMem;

  /**
     Caps the cumulative total number of memory allocations made by
     the engine.

     Rejects _new_ allocations which would take the current total
     allocation over this value. This condition is unrecoverable, as
     the total only increases (never decreases) over time.

     Set to 0 to disable.
  */
  uint64_t maxTotalAllocCount;

  /**
     Caps the concurrent total memory usage.

     Rejects (re)allocations which would take the current byte total
     (including the memcap tracking overhead) over this value.

     Set to 0 to disable.

     Requires (forces enabling of) over-allocation.
  */
  cwal_size_t maxConcurrentMem;

  /**
     Caps the concurrent total memory allocation count.

     Rejects _new_ allocations (not reallocs) which would take
     the current total allocation count over this value.

     Set to 0 to disable.
  */
  cwal_size_t maxConcurrentAllocCount;

  /**
     Caps the size of any single allocation.

     If a single allocation request is larger than this, the
     allocator signals an OOM error instead of allocating.

     Set to 0 to disable.

     Design note: this is explicitly uint32_t, not cwal_size_t, so
     that 32-bit builds with CWAL_SIZE_T_BITS=64 do not need to
     over-allocate by 8 bytes.
  */
  uint32_t maxSingleAllocSize;

  /**
     If true (non-0) then during cwal_engine_init() memory
     allocation size tracking (and its required over-allocation) is
     enabled regardless of which other limits are enabled.

     When over-allocation is enabled, apps can generally expect a
     small reduction in malloc counts and a slight increase in
     peak/total memory usage from scripts.

     When recycling is _disabled_, enabling this option can be
     notably more memory-costly.
  */
  char forceAllocSizeTracking;
};
typedef struct cwal_memcap_config cwal_memcap_config;
/**
   Initialized-with-defaults cwal_memcap_config instance, intended for
   const-copy initialization.
*/
#define cwal_memcap_config_empty_m {                            \
    0/*maxTotalMem*/, 0/*maxTotalAllocCount*/,                  \
    0/*maxConcurrentMem*/, 0/*maxConcurrentAllocCount*/,      \
    /*maxSingleAllocSize*/((CWAL_SIZE_T_BITS==16)             \
                           ? 0x7FFF/*32k*/                    \
                           : (cwal_size_t)(1 << 24/*16MB*/)), \
    0/*forceAllocSizeTracking*/                               \
  }
/**
   Cleanly initialized cwal_memcap_config instance intended for
   non-const copy initialization.
*/
extern const cwal_memcap_config cwal_memcap_config_empty;

/**
   A piece of the infrastructure for hooking into cwal_scope
   push/pop operations performed on a cwal_engine.

   If cwal_engine_vtab::hook::scope_push is not NULL, it gets called
   during cwal_scope_push(), _after_ cwal has set up the scope as its
   current scope. Thus, during this callback, cwal_scope_current_get()
   will return the being-pushed scope. If this function returns non-0,
   pushing of the scope will fail, cwal will immediately pop the scope
   _without_ calling the scope_pop hook, and the result of this
   callback will be returned to the caller of cwal_scope_push(). If
   this hook succeeds (returns 0) then cwal will call the
   cwal_engine_vtab::hook::scope_pop callback when the scope is
   popped.

   This hook gets passed the just-pushed scope and any state set in
   cwal_engine_vtab::hook::scope_state.

   It is strictly illegal for this routine to call cwal_scope_push(),
   cwal_scope_pop(), or any other routine which modifies the scope
   stack.

   One interesting (but solvable) problem: cwal necessarily pushes a
   scope during initialization, before the init hook can be triggered
   and before the engine can be used with cwal_malloc(). That is
   likely to pose a problem for client code. This means that the
   client may, depending on where this hook get connected, get one
   more pop call than push calls. Any memory the client wants to
   allocate before cwal_engine_init() can set up the engine must be
   done directly via the engine's vtab::allocator member, rather than
   via cwal_malloc(), because cwal_malloc()'s behaviour is undefined
   before the engine has been initialized. In the case of s2 the
   problem more or less resolves itself: when s2 starts up it pops
   cwal's installed top scope so that it can push its own scope (it's
   always done this, even before this change). The timing of that
   allows for popping all of the scopes, setting up the push/pop
   hooks, and then pushing its own top-most scope, which then goes
   through the push hook.

   Added 20181123.

   @see cwal_scope_hook_pop_f()
   @see cwal_engine_vtab
*/
typedef int (*cwal_scope_hook_push_f)( cwal_scope * s, void * clientState );

/** 
    If cwal_engine_vtab::hook::scope_pop is not NULL, it gets called
    during cwal_scope_pop(), _before_ cwal has removed the scope from
    the stack.

    This hook gets passed the being-popped scope and any state set in
    cwal_engine_vtab::hook::scope_state.

    It is strictly illegal for this routine to call cwal_scope_push(),
    cwal_scope_pop(), or any other routine which modifies the scope
    stack.

    Reminder to self: should we pass any being-propagated result value
    (via cwal_scope_pop2()), if any, to this routine? i don't think we
    need to, but that's something to keep in mind as a possibility.

    Added 20181123.

    @see cwal_scope_hook_push_f()
    @see cwal_engine_vtab
*/
typedef void (*cwal_scope_hook_pop_f)( cwal_scope const *, void * clientState );

/**
   The "virtual table" of cwal_engine instances, providing the
   functionality which clients can override with their own
   implementations.

   Multiple cwal_engine instances may share if vtab instance if
   and only if:

   - The state member (if used) may legally share the same values
   across engines AND state::finalize() does not destroy
   state::state (if it does, each engine will try to clean it up).

   - The vtab does not make values from one engine visible to
   another. This will lead to Undefined Behaviour.

   - The app is single-threaded OR...

   - all access to cwal_engine_vtab is otherwise serialized via
   a client-side mutex OR...
       
   - the vtab instance is otherwise "immune" the threading effects
   (e.g. because its underlying APIs do the locking).

   Mutex locking is not a feature planned for the cwal API.
*/
struct cwal_engine_vtab {
  /*

    Potential TODOs:

    void (*shutdown)( cwal_engine_vtab * self );

    shutdown() would be called when an engine is cleaned up (after
    it has finished cleaning up), instead of state.finalize(), and
    would be responsible for cleaning up allocator.state and
    outputer.state, if needed.
  */

  /**
     The memory allocator. All memory allocated in the context
     of a given cwal_engine is (re)allocated/freed through it's
     vtab's allocator member.
  */
  cwal_allocator allocator;

  /**
     The handler which receives all data passed to
     cwal_output().
  */
  cwal_outputer outputer;

  /**
     Handles cwal_engine tracing events (if tracing is enabled).
  */
  cwal_engine_tracer tracer;

  /**
     A place to store client-defined state. The engine places
     no value on this, other than to (optionally) clean it up
     when the engine is finalized.

     The finalize() method in the state member is called when an
     engine using this object shuts down. Because it happens
     right after the engine is destroyed, it is passed a NULL
     engine argument. Thus it is called like:

     vtab->state.finalize( NULL, vtab->state.data );

     This of course means that a single vtab cannot differentiate
     between multiple engines for the shutdown phase, and we
     might have to add reference counting to the vtab in order to
     account for this (currently it would need to be somewhere in
     state.data).
  */
  cwal_state state;

  /**
     A place to add client-side hooks into engine
     post-initialization, and possibly for other events at some
     point (if we can find a use for it).
  */
  struct {
    /**
       May be used to add post-init code to cwal_engine
       instances. If this member is not 0 then it is called
       right after cwal_engine_init() is finished, before it
       returns. If this function returns non-0 then
       initialization fails, the engine is cleaned up, and the
       return value is passed back to the caller of
       cwal_engine_init().

       Note that the vtab parameter is guaranteed to be the
       vtab which initialized e. e->vtab==vtab is guaranteed
       to be true, but client code "should really" use the
       passed-in vtab pointer instead of relying on the
       private/internal e->vtab member (its name/placement may
       change).

       This is only called one time per initialization of an engine,
       so the client may (if needed) clean up the init_state member
       (i.e. vtab->hook->init_state).
    */
    int (*on_init)( cwal_engine * e, cwal_engine_vtab * vtab );
    /**
       Arbitrary state passed to on_init().
    */
    void * init_state;

    /**
       Gets triggered when cwal_scope_push() and friends are
       called. This callback may be NULL. Clients which need to
       keep their own scope-level state in sync with cwal's scope
       levels should create a pair of scope push/pop hook routines
       to manage that state.

       See cwal_scope_hook_push_f() for the docs.
    */
    cwal_scope_hook_push_f scope_push;

    /**
       Gets triggered when cwal_scope_pop() and friends
       are called.

       See cwal_scope_hook_pop_f() for the docs.
    */
    cwal_scope_hook_pop_f scope_pop;

    /**
       scope_state is passed as-is to scope_push() and
       scope_pop() every time they are called.

       The cwal engine does not manage or own this state. It is up
       to the client to manage it, if needed. In almost(?) every
       conceivable case in which this state is used, the client
       will have to (because of allocator availability) set it
       AFTER cwal has initialized, which means after cwal has
       pushed its first scope.
    */
    void * scope_state;
  } hook;

  /**
     Holds state for determining whether a given string is
     internable or not. It is not expected that such state will need
     to be finalized separately, thus this member holds no finalizer
     function.
  */
  struct {
    /**
       The is-internable predicate. If NULL, interning is
       disabled regardless of any other considerations
       (e.g. the CWAL_FEATURE_INTERN_STRINGS flag).

       @see cwal_cstr_internable_predicate_f()
    */
    cwal_cstr_internable_predicate_f is_internable;
    /**
       State to be passed as the first argument to is_internable().
    */
    void * state;
  } interning;

  /**
     Memory capping configuration.
  */
  cwal_memcap_config memcap;
};
/**
   Empty-initialized cwal_engine_vtab object.
*/
#define cwal_engine_vtab_empty_m {                                      \
  cwal_allocator_empty_m,                                             \
  cwal_outputer_empty_m,                                            \
  cwal_engine_tracer_empty_m,                                       \
  cwal_state_empty_m,                                               \
  {/*hook*/                                                           \
    NULL/*on_init()*/,0/*init_state*/,                                \
    NULL/*scope_push*/,NULL/*scope_pop*/,                           \
    NULL/*scope_state*/},                                           \
  {/*interning*/ cwal_cstr_internable_predicate_f_default, NULL}, \
  cwal_memcap_config_empty_m \
}

/**
   Empty-initialized cwal_engine_vtab object.
*/
extern const cwal_engine_vtab cwal_engine_vtab_empty;

/**
   A cwal_realloc_f() implementation which uses the standard C
   memory allocators.
*/
void * cwal_realloc_f_std( void * state, void * m, cwal_size_t n );

/**
   A cwal_output_f() implementation which requires state to be
   a valid (FILE*) opened in write mode. It sends all output
   to that file and returns n on success. If state is NULL then
   this is a harmless no-op.
*/
int cwal_output_f_FILE( void * state, void const * src, cwal_size_t n );

/**
   A cwal_output_f() implementation which requires that state be a valid
   (cwal_engine*). This outputer passes all output to cwal_output() using
   the given cwal_engine instance.
*/
int cwal_output_f_cwal_engine( void * state, void const * src, cwal_size_t n );

/**
   A cwal_outputer::flush() implementation whichr equires that f
   be a (FILE*). For symmetry with cwal_output_f_FILE, if !f then
   stdout is assumed.
*/
int cwal_output_flush_f_FILE( void * f );

/**
   A state type for use with cwal_output_f_buffer().

   @see cwal_output_f_buffer()
*/
struct cwal_output_buffer_state {
  cwal_engine * e;
  cwal_buffer * b;
};

/**
   Convenience typedef.
*/
typedef struct cwal_output_buffer_state cwal_output_buffer_state;

/**
   Empty-initialized cwal_output_buffer_state instance.
*/
extern const cwal_output_buffer_state cwal_output_buffer_state_empty;

/**
   A cwal_output_f() implementation which requires state to be a
   valid (cwal_output_buffer_state*), that state->e points to a
   valid (cwal_engine*), and that state->b points to a valid
   (cwal_buffer*). It sends all output to state->b, expanding the
   buffer as necessary, and returns 0 on success.  Results are
   undefined if state is not a cwal_output_buffer_state.
*/
int cwal_output_f_buffer( void * state, void const * src, cwal_size_t n );

/**
   A cwal_output_f() implementation which requires state to be a
   (cwal_outputer*). This routine simply redirects all (src,n) input
   to the cwal_outputer::output() method. Results are undefined if
   state is not a cwal_outputer.
*/
int cwal_output_f_cwal_outputer( void * state, void const * src, cwal_size_t n );

/**
   A cwal_finalizer_f() which requires that m be a
   (cwal_output_buffer_state*). This function calls
   cwal_buffer_reserve(e, state->buffer, 0) to free up the
   buffer's memory, then zeroes out state's contents.

   In theory this can be used together with cwal_output_f_buffer()
   and cwal_outputer to provide buffering of all
   cwal_output()-generated output, but there's a chicken-egg
   scenario there, in that the outputer "should" be set up before
   the engine is intialized. In this case it has to be modified
   after the engine is intialized because the engine is part of
   the outputer's state.
*/
void cwal_output_buffer_finalizer( cwal_engine * e, void * bufState );

/**
   A cwal_engine_vtab instance which can be bitwise copied to inialize
   a "basic" vtab instance for use with cwal_engine_init(). It uses
   cwal_allocator_std for its memory, cwal_outputer_FILE for output,
   and cwal_finalizer_f_fclose() as its output file finalizer.
   To enable output, the client must simply assign an
   opened (FILE*) handle to the vtab's outputer.state.state.
   To remove the finalizer and take over responsibility
   for closing the stream, set outputer.state.finalize
   to 0.
*/
extern const cwal_engine_vtab cwal_engine_vtab_basic;

/**
   Allocates n bytes of memory in the context of e.

   The returned memory is "associated with" (but strictly owned by) e
   and is owned (or shared with) the caller, who must eventually pass
   it to cwal_free() or cwal_realloc(), passing the same engine
   instance as used for the allocation.

   It is NEVER legal to share malloc/free/realloc memory across
   engine instances, even if they use the same allocator, because
   doing so can lead to "missing" entries in one engine or the
   other and mis-traversal of Value graphs.

   It is NEVER legal to call this before the given engine
   has been initialized via cwal_engine_init().
*/
void * cwal_malloc( cwal_engine * e, cwal_size_t n );

/**
   Works similarly to cwal_malloc(), but first tries to pull a memory
   chunk from the chunk recycler, looking for a chunk of size n or
   some "reasonable" factor larger (unspecified, but less than
   2*n). If it cannot find one, it returns the result of passing (e,n)
   to cwal_malloc().

   When clients are done with the memory, they "should" pass it to
   cwal_free2(), passing it the same value for n (which may recycle
   it), but they "may" alternately pass it to cwal_free() (which will
   not recycle it). They "must" do one or the other.

   It is NEVER legal to call this before the given engine
   has been initialized via cwal_engine_init().

   Minor caveat: when recycling a larger chunk, the client doesn't
   know how much larger than n it is. Whether or not cwal really knows
   that depends on whether over-allocating memory capping is enabled
   or not. If so, it is able to notice the difference when the memory
   is passed to cwal_free2() and will recover those "slack" bytes. If
   not, those slack bytes will be "lost" if/when they land back in the
   chunk recycler, in that the recycler will not know about the slack
   bytes at the end. The effect _could_ be, depending on usage, that
   such a block gets smaller on each recycling trip, but in practice
   that has never been witnessed to be a problem (and even if it
   was/is, _any_ re-use of the memory is a win, so it's not "really" a
   problem).
*/
void * cwal_malloc2( cwal_engine * e, cwal_size_t n );

/**
   Frees memory allocated via cwal_malloc() or cwal_realloc().

   e MUST be a valid, initialized cwal_engine instance.

   If !m then this is a no-op.

   @see cwal_free2().
*/
void cwal_free( cwal_engine * e, void * m );

/**
   This alternate form of cwal_free() will put mem in the recycling
   bin, if possible, else free it immediately. sizeOfMem must be the
   size of the memory block. If mem or sizeOfMem it is 0 then this
   function behaves like cwal_free()).

   Recycled memory goes into a pool used internally for various forms
   of allocations, e.g. buffers and arrays.
*/
void cwal_free2( cwal_engine * e, void * mem, cwal_size_t sizeOfMem );

/**
   Works as described for cwal_realloc_f(). See cwal_malloc() for
   important notes.
*/
void * cwal_realloc( cwal_engine * e, void * m, cwal_size_t n );

/** Convenience typedef. */
typedef struct cwal_exception_info cwal_exception_info;

/**
   NOT YET USED.
       
   Holds error state information for a cwal_engine
   instance.
*/
struct cwal_exception_info {
  /**
     Current error code.
  */
  cwal_rc code;
  /**
     Length (in bytes) of cMsg.
  */
  cwal_size_t msgLen;
  /**
     Pointer to string memory not owned by the engine but which
     must be guaranteed to live "long enough."  If zMsg is set
     then this must point to zMsg's.  This is primarily a malloc
     optimization, to allow us to point to strings we know are
     static without having to strdup() them or risk accidentally
     free()ing them.
  */
  char const * cMsg;
  /**
     Dynamically-allocated memory which is owned by the containing
     engine and might be freed/invalidated on the next call
     into the engine API.
  */
  char * zMsg;
  /**
     Error value associated with the error. This would
     presumably be some sort of language-specific error type,
     or maybe a cwal_string form of cMsg.
  */
  cwal_value * value;

  /* TODO?: stack trace info, if tracing is on. */
};

/**
   Empty-initialized cwal_exception_info object.
*/
#define cwal_exception_info_empty_m {           \
    CWAL_RC_OK /*code*/,                        \
    0U /*msgLen*/,                            \
    0 /*cMsg*/,                               \
    0 /*zMsg*/,                               \
    0 /*value*/                               \
  }
/**
   Empty-initialized cwal_exception_info object.
*/
extern const cwal_exception_info cwal_exception_info_empty;


/** @internal

    Internal part of the cwal_ptr_table construct.  Each
    cwal_ptr_table is made up of 0 or more cwal_ptr_page
    instances. Each slot in a page is analog to a hash code, and
    hash code collisions are resolved by creating a new page (as
    opposed to linking the individual colliding items into a list
    as a hashtable would do).
*/
struct cwal_ptr_page {
  /** List of pointers, with a length specified by the containing
      cwal_ptr_table::hashSize.
  */
  void ** list;
  /**
     Number of live entries in this page.
  */
  uint16_t entryCount;
  /**
     Link to the next entry in a linked list.
  */
  struct cwal_ptr_page * next;
};
/** Convenience typedef. */
typedef struct cwal_ptr_page cwal_ptr_page;

/** @internal

    A "key-only" hashtable, the intention being, being able to
    quickly answer the question "do we know about this pointer
    already?" It is used for tracking interned strings and weak
    references. It was originally conceived to help track cycles
    during traversal, but it is not used for that purpose.
*/
struct cwal_ptr_table{
  /**
     The number of (void*) entries in each page.
  */
  uint16_t hashSize;
  /**
     A "span" value for our strange hash function. Ideally this
     value should be the least common sizeof() shared by all values
     in the table, and it degrades somewhat when using mixed-size
     values (which most cwal_values actually are, internally, as a
     side-effect of malloc() reduction optimizations).  For tables
     where the sizeof() is the same for all members this type should
     provide near-ideal access speed and a fair memory cost if
     hashSize can be predicted (which it most likely cannot).
  */
  uint16_t step;
  /**
     Where we keep track of pages in the table.
  */
  struct {
    /**
       First page in the list.
    */
    cwal_ptr_page * head;
    /**
       Last page in the list. We keep this pointer only to
       speed up a small handful of operations.
    */
    cwal_ptr_page * tail;
  } pg;
  /**
     Internal allocation marker.
  */
  void const * allocStamp;
};
typedef struct cwal_ptr_table cwal_ptr_table;
/**
   Empty-initialized cwal_ptr_table, for use in in-struct
   initialization.
*/
#define cwal_ptr_table_empty_m {                \
    0/*hashSize*/,                              \
    0/*step*/,                                \
    {/*pg*/ NULL/*head*/, NULL/*tail*/},        \
    NULL/*allocStamp*/                        \
  }
/**
   Empty-initialized cwal_ptr_table, for use in copy
   initialization.
*/
extern const cwal_ptr_table cwal_ptr_table_empty;

/**
   Holds the state for a cwal scope. Scopes provide one layer of
   the cwal memory model, and are modeled more or less off of
   their C++ counterparts.

   All allocation of new values in a cwal_engine context happen
   within an active scope, and the engine tracks a stack of scopes
   which behave more or less as scopes do in C++. When a scope is
   popped from the stack it is cleaned up and unreferences any
   values it currently owns (those allocated by it and not since
   taken over by another scope). Unreferencing might or might not
   destroy the values, depending on factors such as reference
   counts from cycles in the value graph or from other scopes. If
   values remain after cleaning up, it cleans up again and again
   until all values are gone (this is how it resolves cycles).

   When values are manipulated the engine (tries to) keep(s) them
   in the lowest-level (oldest) scope from which they are ever
   referenced. This ensures that the values can survive
   destruction of their originating scope, while also ensuring
   that a destructing scope can clean up values which have _not_
   been taken over by another scope. This "can" (under specific
   usage patterns) potentially lead to some values being
   "orphaned" in a lower-level scope for an undue amount of time
   (unused but still owned by the scope), and the
   cwal_scope_sweep() API is intended to help alleviate that
   problem.
*/
struct cwal_scope {
  /**
     The engine which created and manages this scope.
  */
  cwal_engine * e;

  /**
     Parent scope. All scopes except the top-most have a parent.
  */
  cwal_scope * parent;

  /**
     Stores this object's key/value properties (its local
     variables). It may be an Object (cwal_object) or Hash (cwal_hash)
     Value. Which one gets created depends on the combination of
     compile-time CWAL_OBASE_ISA_HASH setting and the runtime
     cwal_engine-level CWAL_FEATURE_SCOPE_STORAGE_HASH flag.
  */
  cwal_value * props;

  /**
     Internal memory allocation stamp. Client code must never touch
     this.
  */
  void const * allocStamp;

  /**
     Values allocated while this scope is the top of the stack, or
     rescoped here after allocation, are all placed here and unref'd
     when the scope is cleaned up. We split it into multiple lists
     to simplify and improve the performance of certain operations
     (while slightly complicating others ;).

     Client code MUST NOT touch any of the fields in this
     sub-struct.  They are intricate bits of the internal memory
     management and will break if they are modified by client code.

     TODO(?): refactor this into an array of lists, like
     cwal_engine::recycler. We can then refine it easily by adding
     extra lists for specific types or groups of types.
  */
  struct {
    /**
       Head of the "PODs" (Plain old Data) list. This includes
       all non-containers.
    */
    cwal_value * headPod;
    /**
       Head of the "Objects" list. This includes all container
       types. (This distinction largely has to do with cycles.)
    */
    cwal_value * headObj;

    /**
       Holds items which just came into being and have a refcount
       of 0, or have been placed back into a probationary state
       with refcount 0. This potentially gives us a
       faster/safer/easier sweep() operation.
    */
    cwal_value * r0;

    /**
       Values marked with the flag CWAL_F_IS_VACUUM_SAFE are
       managed in this list and treated basically like named vars
       for purposes of vacuuming. The intention is to provide a
       place where clients can put non-script-visible values which
       are safe from sweep/vacuum operations, but otherwise have
       normal lifetimes. Making a value vacuum-proof does not make
       it sweep-proof.
    */
    cwal_value * headSafe;
  } mine;

  /**
     The depth level this scope was created at. This is used in
     figuring out whether a value needs to be migrated to a
     lower-numbered (a.k.a. "higher") scope for memory
     management reasons.

     Scope numbers start at 1, with 0 being reserved for
     "invalid scope.".
  */
  cwal_size_t level;

  /**
     Internal flags.
  */
  uint32_t flags;
};
/**
   Empty-initialized cwal_scope object.
*/
#define cwal_scope_empty_m {                                        \
    NULL/*engine*/,                                                 \
    NULL/*parent*/,                                               \
    NULL/*props*/,                                                \
    NULL/*allocStamp*/,                                           \
    {/*mine*/ 0/*headPod*/,0/*headObj*/,0/*r0*/, 0/*headSafe*/},    \
    0U/*level*/,                                                  \
    0U/*flags*/                                                 \
  }

/**
   Empty-initialized cwal_scope_api object.
*/
extern const cwal_scope cwal_scope_empty;

/**
   Used to store "recyclable memory" - that which has been finalized
   but not yet free()d. Each instance is responsible for holding
   memory of a single type. Most instances manage Value (cwal_value)
   memory, but specialized instances handle recycling of other types
   (cwal_kvp and String-type values, as those need special handling
   due to their allocation mechanism (which needs to be reconsidered
   for refactoring)).
*/
struct cwal_recycler {
  /**
     Client-interpreted "ID" for this instance. It is used
     internally for sanity checking.
  */
  int id;
  /**
     Current length of this->list.
  */
  cwal_size_t count;
  /**
     Preferred maximum length for this list. Algorithms which
     insert in this->list should honor this value and reject
     insertion if it would be exceeded.
  */
  cwal_size_t maxLength;
  /**
     Underlying list. The exact type of entry is
     context-dependent (e.g. cwal_value or cwal_kvp pointers).
  */
  void * list;
  /**
     Each time a request is made to fetch a recycled value and we
     have one to serve the request, this counter gets incremented.
  */
  cwal_size_t hits;
  /**
     Each time a request is made to fetch a recycled value and we do
     not have one to serve the request, this counter gets
     incremented.

     Note this count includes requests which cannot possibly
     succeed, e.g. the initial allocation of any Value.  In the
     general case (barring allocation errors or falling back to the
     chunk recycler), this number will correspond directly to the
     number of allocations made for the type(s) stored in this
     recycler.
  */
  cwal_size_t misses;
};
/**  Convenience typedef. */
typedef struct cwal_recycler cwal_recycler;
/** Default-initialized cwal_recycler object. */

#define cwal_recycler_empty_m {-1/*id*/, 0U/*count*/, 128U/*maxLength*/,NULL/*list*/,0/*hits*/,0/*misses*/}

/** Default-initialized cwal_recycler object. */
extern const cwal_recycler cwal_recycler_empty;
/**
   Configurable bits for cwal_memchunk.
   
   @see cwal_engine_memchunk_config()
*/
struct cwal_memchunk_config {
  /**
     Maximum number of entries to allow (0 means disable). This is
     also the initial capacity of this->pool, so DO NOT set it to
     something obscenely huge. Some related algos are linear, so do
     not set it to something unreasonably large.

     MUST currently be set up before the recycler is used (it is
     allocated the first time something tries to store memory in
     it), as the related at-runtime resizing code is untested.
  */
  cwal_size_t maxChunkCount;

  /**
     The largest single chunk size to recycle (those larger than
     this will be freed immediately instead of recycled). Set to
     (cwal_size_t)-1 for (effectively) no limit or 0 to disable. If
     if there is any semantic dispute between maxTotalSize and
     maxChunkSize, maxTotalSize wins.
  */
  cwal_size_t maxChunkSize;

  /**
     The maximum total chunk size. The recycler will not store
     more than this. Set to (cwal_size_t)-1 for no (effective)
     limit and 0 to disable chunk recycling.
  */
  cwal_size_t maxTotalSize;

  /**
     If true (non-0) then cwal will try to use the
     memchunk allocator for allocating arbitrary new cwal_value
     instances _if_ it cannot find one in the value-type-specific
     recycler. That has the following properties and implications:

     - Requires an exact-size match.

     - Based on s2 tests, lowers the hit/miss ratio in the
     chunk lookup notably: dropping from ~5% to ~20% misses.

     - it's a micro-optimization: <1% total allocation reduction in
     s2's test suite.

     - This adds a O(N) component to Value allocations when the
     type-specific recycler is empty.
  */
  char useForValues;
};
typedef struct cwal_memchunk_config cwal_memchunk_config;

/**
   Convenience typedef.
*/
typedef struct cwal_memchunk_overlay cwal_memchunk_overlay;
/** @internal

    Internal utility for recycling chunks of memory. It can only be
    used with chunks having a size >= sizeof(cwal_memchunk_overlay).
*/
struct cwal_memchunk_overlay {
  /** The size of this chunk, in bytes. */
  cwal_size_t size;
  /**
     Next chunk in this linked list.
  */
  cwal_memchunk_overlay * next;
};
/**
   Initialized-with-defaults cwal_memchunk_config struct, used
   for const copy initialization. The values defined here are
   the defaults for the cwal framework.
*/
#if 16 == CWAL_SIZE_T_BITS
#define cwal_memchunk_config_empty_m {          \
    25/*maxChunkCount*/,                        \
    1024 * 32/*maxChunkSize*/,                \
    1024 * 63/*maxTotalSize*/,                \
    1/*useForValues*/                         \
  }
#else
#define cwal_memchunk_config_empty_m {          \
    25/*maxChunkCount*/,                        \
    1024 * 32/*maxChunkSize*/,                \
    1024 * 64/*maxTotalSize*/,                \
    1/*useForValues*/                         \
  }
#endif
/**
   Intended for use with copy construction. Is guaranteed
   to hold the same bits as cwal_memchunk_config_empty_m.
*/
extern const cwal_memchunk_config cwal_memchunk_config_empty;

/**
   A helper type for recycling "chunks" of memory (for use with
   arrays, buffers, hash tables, etc.).
*/
struct cwal_memchunk_recycler {
  /**
     The head of the active (awaiting recyling) chunks. Holds
     this->headCount entries.
  */
  cwal_memchunk_overlay * head;

  /**
     The number of entries in this->head.
  */
  cwal_size_t headCount;

  /**
     The total value of the size member of all entries of
     this->head. i.e. the amount of memory currently awaiting reuse.
     This does not include the memory held by this->pool, which is
     (this->capacity * sizeof(void*)) bytes.
  */
  cwal_size_t currentTotal;

  /**
     Various internal metrics.
  */
  struct {
    cwal_size_t totalChunksServed;
    cwal_size_t totalBytesServed;
    cwal_size_t peakChunkCount;
    cwal_size_t peakTotalSize;
    cwal_size_t smallestChunkSize;
    cwal_size_t largestChunkSize;
    cwal_size_t requests;
    cwal_size_t searchComparisons;
    cwal_size_t searchMisses;
    cwal_size_t runningAverageSize;
    cwal_size_t runningAverageResponseSize;
  } metrics;
    
  cwal_memchunk_config config;
};
/** Convenience typedef. */
typedef struct cwal_memchunk_recycler cwal_memchunk_recycler;
/**
   An empty-initialized const cwal_memchunk_recycler.
*/
#define cwal_memchunk_recycler_empty_m {            \
    0/*head*/, 0/*headCount*/, 0/*currentTotal*/,   \
    {/*metrics*/                                    \
      0/*totalChunksServed*/,                       \
      0/*totalBytesServed*/,                      \
      0/*peakChunkCount*/,                        \
      0/*peakTotalSize*/,                         \
      0/*smallestChunkSize*/,                     \
      0/*largestChunkSize*/,                      \
      0/*requests*/,                              \
      0/*searchComparisons*/,                     \
      0/*searchMisses*/,                          \
      0/*runningAverageSize*/,                    \
      0/*runningAverageResponseSize*/,            \
    },                                          \
    cwal_memchunk_config_empty_m                  \
  }

/**
   A generic buffer class used throughout the cwal API, most often for
   buffering arbitrary streams and creating dynamic strings.

   For historical reasons (and because a retrofit would be awkward, in
   terms of new APIs, and relatively expensive in terms of new costs),
   cwal_buffer has two distinct uses:

   - "Value Buffers" are created using cwal_new_buffer() and their
   "buffer part" (cwal_value_get_buffer()) is owned by the Value which
   wraps it.

   - "Plain" buffers are created not associated with a Value instance
   and are used as demonstrated below. They are, in practice, always
   created on the stack or embedded in another struct.

   Note that the memory managed by this class does not partake in any
   cwal-level lifetime management. It is up to the client to use it
   properly, such that the memory is freed when no longer needed.

   They can be used like this:

   @code
   cwal_buffer b = cwal_buffer_empty
   // ALWAYS initialize this way, else risk Undefined Bahaviour!
   // For in-struct initialization, use cwal_buffer_empty_m.
   ;
   int rc = cwal_buffer_reserve( e, &buf, 100 );
   if( 0 != rc ) {
   ... allocation error ...
   assert(!buf.mem); // just to demonstrate
   }else{
   ... use buf.mem ...
   ... then free it up ...
   cwal_buffer_reserve( e, &buf, 0 );
   }
   @endcode

   To take over ownership of a buffer's memory:

   @code
   void * mem = b.mem;
   // mem is b.capacity bytes long, but only b.used
   // bytes of it has been "used" by the API.
   b = cwal_buffer_empty;
   @endcode

   The memory now belongs to the caller and must eventually be
   cwal_free()'d. Even better, if you remember the buffer's original
   capacity after taking over ownership, you can use cwal_free2(),
   passing it the memory and the block's size (the buffer's former
   capacity), which may allow cwal to recycle the memory better.
*/
struct cwal_buffer
{
  /**
     The number of bytes allocated for this object.
     Use cwal_buffer_reserve() to change its value.
  */
  cwal_size_t capacity;
  /**
     The number of bytes "used" by this object's mem member. It must
     be <= capacity.
  */
  cwal_size_t used;

  /**
     The memory allocated for and owned by this buffer.
     Use cwal_buffer_reserve() to change its size or
     free it. To take over ownership, do:

     @code
     void * myptr = buf.mem;
     buf = cwal_buffer_empty;
     @endcode

     (You might also need to store buf.used and buf.capacity,
     depending on what you want to do with the memory.)
       
     When doing so, the memory must eventually be passed to
     cwal_free() to deallocate it.
  */
  unsigned char * mem;

  /**
     For internal use by cwal to differentiate between
     container-style buffers and non-container style without
     requiring a complete overhaul of the buffer API to make
     Container-type representations of them.

     Clients MUST NOT modify this. It is only non-NULL for buffers
     created via cwal_new_buffer() resp. cwal_new_buffer_value(),
     and its non-NULL value has a very specific meaning. To
     reiterate: clients MUST NOT modify this.

     20181126: i'm not certain that we still need this. We can(?)
     ensure that a buffer is-a Value by doing the conversion from a
     buffer to a value, then back again, and see if we get the same
     result. If not, it wasn't a Value. If we do, it's _probably_(?
     possibly?) a Value.
  */
  void * self;
};

/**
   An empty-initialized cwal_buffer object.

   ALWAYS initialize embedded-in-struct cwal_buffers by copying
   this object!
*/
#define cwal_buffer_empty_m {0/*capacity*/,0/*used*/,NULL/*mem*/, 0/*self*/}

/**
   An empty-initialized cwal_buffer object. ALWAYS initialize
   stack-allocated cwal_buffers by copying this object!
*/
extern const cwal_buffer cwal_buffer_empty;

/**
   A typedef used by cwal_engine_type_name_proxy() to allow
   clients to hook their own type names into
   cwal_value_type_name().

   Its semantics are as follows:

   v is a valid, non-NULL value. If the implementation can map
   that value to a type name it must return that type name string
   and set *len (if len is not NULL) to the length of that string.
   The returned bytes must be guaranteed to be static/permanent in
   nature (they may be dynamically allocated but must outlive any
   values associated with the name).

   If it cannot map a name to the value then it must return NULL,
   in which case cwal_value_type_name() will fall back to its
   default implementation.

   Example implementation:

   @code
   static char const * my_type_name_proxy( cwal_value const * v,
   cwal_size_t * len ){
   cwal_value const * tn = cwal_prop_get(v, "__typename", 10);
   return tn ? cwal_value_get_cstr(tn, len) : NULL;
   }
   @endcode

   @see cwal_engine_type_name_proxy()
*/
typedef char const * (*cwal_value_type_name_proxy_f)( cwal_value const * v, cwal_size_t * len );

/**
   A generic error code/message combination. Intended for reporting
   non-exception errors, possibly propagating them on their way to
   becoming exceptions. The core library does not need this, but it
   has proven to be a useful abstraction in client-side code, so was
   ported into the main library API.

   @see cwal_error_set()
   @see cwal_error_get()
   @see cwal_error_reset()
   @see cwal_error_clear()
*/
struct cwal_error {
  /**
     Error code, preferably a CWAL_RC_xxx value.
  */
  int code;
  /**
     Line-number of error, if relevant. 1-based, so use 0
     as a sentinel value.
  */
  int line;
  /**
     Column position of error, if relevant. 0-based.
  */
  int col;
  /**
     The error message content.
  */
  cwal_buffer msg;

  /**
     Holds a script name associated with this error (if any). We use a
     buffer instead of a string because it might be re-set fairly
     often, and we can re-use the memory.
  */
  cwal_buffer script;
};

/**
   Empty-initialized cwal_error structure, intended for const-copy
   initialization.
*/
#define cwal_error_empty_m {0, 0, 0, cwal_buffer_empty_m, cwal_buffer_empty_m}

/**
   Empty-initialized cwal_error structure, intended for copy
   initialization.
*/
extern const cwal_error cwal_error_empty;


/**
   The core manager type for the cwal API. Each "engine" instance
   manages a stack of scopes and (indirectly) the memory associated
   with Values created during the life of a Scope.
*/
struct cwal_engine {
  cwal_engine_vtab * vtab;
  /**
     Internal memory allocation marker.
  */
  void const * allocStamp;
  /**
     A handle to the top scope. Used mainly for internal
     convenience and sanity checking of the scope stack
     handling.
  */
  cwal_scope * top;
  /**
     Scope stack. Manipulated via cwal_scope_push() and
     cwal_scope_pop().

     TODO: rename this to currentScope someday.
  */
  cwal_scope * current;
    
  /**
     When a scope is cleaned, if deferred freeing is not active
     then this pointer is set to some opaque value known only by
     the currently-being-cleaned scope before it starts cleaning
     up. As long as this is set, freeing and recycling of
     containers is deferred until cleanup returns to the
     being-freed scope, at which point this value is cleared and
     the gc list is flushed, all of its entries being submitted
     for recycling (or freeing, if recycling is disabled or
     full).

     This mechanism acts as a safety net when traversing cycles
     where one of the traversed values was freed along the way. The
     lowest-level scope from which destruction is initiated
     (normally also the bottom-most scope, but i would like to
     consider having scopes as first-class values) is the "fence"
     for this operation because destruction theoretically cannot
     happen for values in higher scopes during cleanup of a lower
     scope. i.e. when destructing scopes from anywhere but the top
     of the stack the initial scope in the destruction loop is the
     one which will queue up any to-be-freed containers for
     recycling, and it will flush the gc list. Because values form
     linked lists, we use those to form the chain of deferred
     destructions, so this operation costs us no additional memory
     (it just delays deallocation/recycling a bit) and is O(1)
     (flushing the queue is O(N)).

     Note that types which cannot participate in graphs are not
     queued - they are (normally) immediately recycled or
     cwal_free()d when their refcount drops to 0 (or is reduced
     when it is already 0, as is the case for "temporary" values
     which never get a reference).
  */
  void const * gcInitiator;

  /**
     Internal flags. See the API-internal CWAL_FLAGS enum for
     the gory details.
  */
  uint32_t flags;

  /**
     A flag which will someday be used to flag assertion-level
     failures in non-debug builds, such that the engine, if a
     condition arises which is normally fatal/assert()ed, then it
     will refuse to do anything further except (reluctantly)
     cwal_engine_destroy().
  */
  int fatalCode;

  /**
     May hold non-exception error state, potentially on behalf of
     client code.
  */
  cwal_error err;

  /**
     List of list managers for (cwal_value*) of types which we can
     recycle. We can recycle memory for these types:

     integer, double, array, object, hash, native, buffer, function,
     exception, x-string/z-string (in the same list), cwal_kvp,
     cwal_scope, cwal_weakref, unique, cwal_tuple.

     The lists are in an order determined by the internal
     function cwal_recycler_index(). Other types cannot be
     recycled as efficiently (e.g. cwal_string use a separate
     mechanism) or because they are never allocated (null, bool,
     undefined and any built-in shared value instances).

     Reminder: the default recycle bin sizes do not reflect any
     allocation size, simply the number of objects we don't free
     immediately, so there is little harm is setting them
     relatively high (e.g. 100 or 1000). Because cwal_value and
     cwal_kvp objects form a linked list, a given recycle bin
     may grow arbitrarly large without requiring extra memory to
     do so (we just link the values in each recycle bin, and
     those values have already been allocated). This all happens in
     O(1) time by simply making each new entry the head of the
     list (and removing them in that order as well).

     Maintenance reminder: the indexes of this list which actually
     get used depend on how the engine sets up the bins. It combines
     like-sized types into the same bins. Thus this list has to be
     larger than strictly necessary for platforms where it cannot
     combine types. On x86/64, it needs 9 slots as of this writing.
  */
  cwal_recycler recycler[16];

  /**
     reString is a special-case recycler for cwal_string values.
     String values are recycled based on their size. i.e. we
     won't recycle a 36-byte string's memory to serve a 10-byte
     string allocation request.

     As an optimization, we (optionally) pad string allocations
     to a multiple of some small number of bytes (e.g. 4 or 8),
     as this lets us recycle more efficiently (up to 36% more
     string recycling in some quick tests).

     See the API-internal CwalConsts::StringPadSize for details.
  */
  cwal_recycler reString;

  /**
     A generic memory chunk recycler.
  */
  cwal_memchunk_recycler reChunk;

  /**
     A place for client code to associated a data pointer and
     finalizer with the engine. It is cleaned up early in the engine
     finalization process.
  */
  cwal_state client;

  /**
     A value-to-type-name proxy, manipulated via
     cwal_engine_type_name_proxy().
  */
  cwal_value_type_name_proxy_f type_name_proxy;

  /**
     Where we store internalized strings.

     Maintenance note: this is-a cwal_ptr_table but uses its own
     hashing/searching/insertion/removal API. Do NOT use the
     equivalent cwal_ptr_table ops on this instance.  See the
     internal cwal_interned_search(), cwal_interned_insert(),
     and cwal_interned_remove() for details.
  */
  cwal_ptr_table interned;

  /**
     Memory for which we have a weak reference is annotated by
     simply inserting its address into this table. When the
     memory is cleaned up, if it has an entry here, we
     invalidate any cwal_weakrefs which point to it.
  */
  cwal_ptr_table weakp;

  /**
     cwal_weakref instances are stored here, grouped by
     underlying memory type to speed up the invalidate-ref
     operation. Weak refs to buit-in constants are handled
     specially (to avoid allocating new instances and because
     they can never be invalidated). Some slots of this array
     (those of constant types, e.g. null/undefined/bool) are
     unused, but we keep their slots in this array because it
     greatly simplifies our usage of this array.

     The opaque (void*) memory pointed to by weak references is held
     in weakr[CWAL_TYPE_WEAKREF], since that slot is otherwise
     unused. We "could" use the NULL/BOOL/UNDEF slots for similar
     purposes.
  */
  cwal_weakref * weakr[CWAL_TYPE_end];

  /**
     The top-most scope. This is an optimization to avoid an
     allocation.
  */
  cwal_scope topScope;

  /**
     If built with CWAL_ENABLE_TRACE to 0 then this is a no-op
     dummy placeholder, else it holds information regarding
     engine tracing. This state continually gets overwritten if
     tracing is active, and sent to the client via
     this->api->tracer.
  */
  cwal_trace_state trace;

  /**
     Buffer for internal string conversions and whatnot. This
     buffer is volatile and its contents may be re-allocated or
     modified by any calls into the public API. Internal APIs
     need to be careful not to stomp the buffer out from under
     higher-scope public APIs and internal calls.
  */
  cwal_buffer buffer;

  /**
     Where Function-type call() hooks are stored.
  */
  cwal_callback_hook cbHook;

  /**
     _Strictly_ interal bits for managing various specific values or
     categories of values.
  */
  struct {
    /**
       Holds any current pending exception, taking care to
       propagate it up the stack when scopes pop.
    */
    cwal_value * exception;

    /**
       A slot for a single propagating value which will
       automatically be pushed up the stack as scopes
       pop. Intended for keywords which propagate via 'return'
       semantics or error reporting, so that they have a place to
       keep their result (if any).
    */
    cwal_value * propagating;
        
    /**
       Where clients may store their customized base prototypes
       for each cwal_type_id. The indexes in this array correspond
       directly to cwal_type_id values, but (A) that is an
       implementation detail, and (B) some slots are not used (but
       we use this structure as a convenience to save cycles in
       type-to-index conversions and to keep those prototypes
       vacuum-safe).
    */
    cwal_array * prototypes;

    /**
       A place to store values which are being destroyed during
       the traversal of cycles. Used for delayed freeing of
       cwal_value memory during scope cleanup. See gcInitiator
       for more details.
    */
    cwal_value * gcList;

    /**
       Internal optimization for cwal_hash_resize() and
       cwal_hash_take_props() to allow us to transfer a kvp
       directly from the source to the target. This gets
       transfered (or not) from a container in the calling op to
       the target hashtable during the cwal_hash_insert_v(). If,
       after calling cwal_hash_insert_v(), it's not 0, it's up to
       the caller to manage its memory and reset this to 0.
    */
    cwal_kvp * hashXfer;
  } values;

  /**
     Holds metrics related to honoring memory capping. Note that
     recycled memory only counts once for purposes of these metrics.
     i.e. recycling a chunk of memory counts as a single allocation
     (the initial one), not a separate allocation each time the
     chunk is recycled.
  */
  struct {
    /**
       Caps concurrent cwal-allocated memory to this ammount.
    */
    cwal_size_t currentMem;
    /**
       Caps the concurrent cwal allocation count to this number
       (or thereabouts - reallocations are kind of a grey area).
    */
    cwal_size_t currentAllocCount;
    /**
       Caps the peak amount of cwal-allocated memory to
       this amount.
    */
    cwal_size_t peakMem;
    /**
       Records the peak concurrent allocation count (or
       thereabouts - reallocations are kind of a grey area). This
       is not a memory capping constraint.
    */
    cwal_size_t peakAllocCount;
    /**
       Caps the total cwal allocation count to this number (or
       thereabouts - reallocations are kind of a grey area).
    */
    uint64_t totalAllocCount;
    /**
       Caps the total amount of cwal-allocated memory to this
       amount.
    */
    uint64_t totalMem;
  } memcap;

  /**
     A place for storing metrics.
  */
  struct {
    /**
       Each time a request is made to allocate a Value, the
       value type's entry in this array is increments. Does
       not apply to certain optimized-away situations like
       empty strings, bools/null/undef, and the constant
       numeric values.
    */
    cwal_size_t requested[CWAL_TYPE_end];
    /**
       Each time we have to reach into the allocator to
       allocate an engine resource, its type's entry in this
       array is incremented. This values will always be less
       than or equal to the same offered in the 'requested'
       member.
    */
    cwal_size_t allocated[CWAL_TYPE_end];

    /**
       The number of allocated bytes for each type is totaled
       here.  We can't simply use (allocated*sizeof) for some
       types (e.g. strings, arrays, buffers, and hashtables),
       and this value requires some fiddling with in certain
       areas to ensure it gets all memory for some types
       (namely arrays and buffers, whose sizes change with
       time). In any case, it is only a close approximation
       because reallocs play havoc with our counting in some
       cases.
    */
    cwal_size_t bytes[CWAL_TYPE_end];

    /**
       clientMemTotal keeps a running total of memory usage
       declared by the client.

       @see cwal_engine_adjust_client_mem()
    */
    cwal_size_t clientMemTotal;

    /**
       Holds the current amount of memory declared by the client.

       @see cwal_engine_adjust_client_mem()
    */
    cwal_size_t clientMemCurrent;

    /**
       Each time an attempt to recycle a recyclable Value
       type (or cwal_kvp) succeeds, this is incremented.
    */
    cwal_size_t valuesRecycled;

    /**
       Each time we look in the value recyclers for a value and do
       not find one, this is incremented.
    */
    cwal_size_t valuesRecycleMisses;

    /**
       Counts the number of blocks for which
       this->recoveredSlackBytes applies.
    */
    cwal_size_t recoveredSlackCount;
    /**
       When over-allocating to account for memory capping,
       the chunk recycler can recover more "slack" bytes.
       Those are counted here.
    */
    cwal_size_t recoveredSlackBytes;

    /**
       The highest-ever refcount on any Value.
    */
    cwal_refcount_t highestRefcount;

    /**
       The data type of the value for which the highestRefcount
       metric was measured.
    */
    cwal_type_id highestRefcountType;

    /**
       Records the number of allocations saved by the using the
       built-in list of length-1 ASCII strings to serve
       cwal_new_string(), cwal_new_xstring(), and
       cwal_new_zstring() allocations.

       Indexes: 0: string, 1: x-string, 2: z-string
    */
    cwal_size_t len1StringsSaved[3];
  } metrics;
};
/** @def cwal_engine_empty_m
    Empty initialized const cwal_engine struct.
*/
#define cwal_engine_empty_m {                                           \
  NULL /* vtab */,                                                    \
  NULL /* allocStamp */,                                            \
  NULL /* top */,                                                   \
  NULL /* current */,                                               \
  NULL /* gcInitiator */,                                           \
  0U /* flags */,                                                   \
  0 /* fatalCode */,                                                \
  cwal_error_empty_m,                                               \
  {/* recycler */                                                     \
    cwal_recycler_empty_m, cwal_recycler_empty_m,                     \
    cwal_recycler_empty_m, cwal_recycler_empty_m,                   \
    cwal_recycler_empty_m, cwal_recycler_empty_m,                   \
    cwal_recycler_empty_m, cwal_recycler_empty_m,                   \
    cwal_recycler_empty_m, cwal_recycler_empty_m,                   \
    cwal_recycler_empty_m, cwal_recycler_empty_m,                   \
    cwal_recycler_empty_m, cwal_recycler_empty_m,                   \
    cwal_recycler_empty_m, cwal_recycler_empty_m                  \
  },                                                              \
  {/*reString*/ CWAL_TYPE_STRING, 0, 40, NULL, 0, 0 }, \
  /* reChunk*/ cwal_memchunk_recycler_empty_m,                        \
  cwal_state_empty_m /* client */,                                    \
  NULL/*type_name_proxy*/,                                            \
  cwal_ptr_table_empty_m/*interned*/,                                 \
  cwal_ptr_table_empty_m/*weakp*/,                                    \
  {/*weakr*/                                                            \
    NULL,NULL,NULL,NULL,NULL,                                           \
    NULL,NULL,NULL,NULL,NULL,                                         \
    NULL,NULL,NULL,NULL,NULL,                                         \
    NULL,NULL,NULL,NULL,NULL,                                         \
    NULL,NULL                                                         \
  },                                                                \
  cwal_scope_empty_m/*topScope*/,                                     \
  cwal_trace_state_empty_m/*trace*/,                                  \
  cwal_buffer_empty_m/*buffer*/,                                      \
  cwal_callback_hook_empty_m/*cbHook*/,                               \
 {/*values*/                                                           \
   NULL/* exception */,                                                 \
   NULL/* propagating */,                                             \
   NULL/* prototypes */,                                              \
   NULL/* gcList */,                                                  \
   NULL /* hashXfer */                                             \
 },      \
 {/*memcap*/ \
   0/*currentMem*/, 0/*currentAllocCount*/,                             \
   0/*peakMem*/, 0/*peakAllocCount*/,                                 \
   0/*totalAllocCount*/, 0/*totalMem*/                                \
 },                                                                 \
 {/*metrics*/ \
   /*requested[]*/{0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,0},      \
   /*allocated[]*/{0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,0},    \
   /*bytes[]*/    {0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,0},    \
   0/*clientMemTotal*/, 0/*clientMemCurrent*/,                        \
   0/*valuesRecycled*/, 0/*valuesRecycleMisses*/,                     \
   0/*recoveredSlackCount*/, 0/*recoveredSlackBytes*/,                \
   0/*highestRefcount*/, CWAL_TYPE_UNDEF/*highestRefcountType*/,      \
   {0,0,0}/*len1StringsSaved*/                                          \
 }                                                                      \
}/*end cwal_engine_empty_m*/

/**
   Empty-initialized cwal_engine object. When initializing
   stack-allocated cwal_engine instances, always copy this instance
   over them to set up any default state. For in-struct
   initialization, use cwal_engine_empty_m.
*/
extern const cwal_engine cwal_engine_empty;

/**
   Initializes a cwal_engine instance. vtab must not be NULL and must
   be populated in accordance with the cwal_engine_vtab
   documentation. e must either be a pointer to a NULL-initialized
   pointer or a pointer to a pre-allocated instance (possibly from
   the stack or embedded in another struct) with a clean state
   (e.g. by copying cwal_engine_empty or cwal_engine_empty_m over
   it).

   vtab MUST outlive e, and in practice it can normally be static.

   On success 0 is returned and *e points to the engine instance.

   On success e is initialized with a top scope, so clients need not
   use cwal_scope_push() before using the cwal_new_xxx() family
   of factory functions to allocate values.
    
   On error non-0 is returned and *e is NOT cleaned up so that
   any error state in the object can be collected by the client
   before freeing it. If *e is NULL on returning and argument
   validation succeeds, then allocation of the instance failed
   and CWAL_RC_OOM will be returned. If vtab.on_init() returns
   non-0, this function will return that code, but vtab.on_init()
   is only called if the priliminary initialization succeeds
   (it can only fail on allocation errors).

   In short: the caller must, regardless of whether or not this
   function succeeds, if *e is not NULL, eventually pass *e to
   cwal_engine_destroy(). If this function allocated it, that
   function will free it.

   Errors include:

   CWAL_RC_MISUSE: one of the arguments is NULL

   CWAL_RC_OOM: memory allocation failed

   Any other error code should be considered a bug in this code.


   Potential TODO: require the client to push the first scope,
   giving him a time slot between initialization and the first
   scope to set configuration options which might affect the first
   scope (currently we have none built in, and the vtab's on_init()
   hook can be used to make changes for the time being).
*/
int cwal_engine_init( cwal_engine ** e, cwal_engine_vtab * vtab );

/**
   Frees all resources owned by e. If e was allocated dyanamically by
   cwal_engine_init() (or equivalent) then this function
   deallocates it, otherwise e is a left in an empty state after this
   call (suitable for re-using with cwal_engine_init()).

   As a special case, if e has no vtab then it is assumed that
   cwal_engine_init() hhas not yet been called and this function
   returns 0 and has no side effects.
*/
int cwal_engine_destroy( cwal_engine * e );

/**
   Installs f as a proxy for cwal_value_type_name(). See
   cwal_value_type_name_proxy_f for full details. f may be NULL
   but e may not. e may only have one type name proxy installed at
   a time, and this replaces any existing one.

   Returns the existing proxy (possibly NULL), so that the client
   may swap it in and out, but in practice the client sets this up
   only once in the initialization phase.

   @see cwal_value_type_name()
   @see cwal_value_type_name2()
*/
cwal_value_type_name_proxy_f
cwal_engine_type_name_proxy( cwal_engine * e,
                             cwal_value_type_name_proxy_f f );

/**
   Pushes a new scope onto e's stack, making it the current scope.
   If s is not NULL and (*s) is NULL then *s is assigned to a
   newly-allocated/recycled scope on success.
       
   If s is not NULL and (*s) is not NULL when this function is
   called then it must be a cleanly-initialized scope value, and
   this function will use it instead of allocating/recycling
   one. For example:

   @code
   cwal_scope sub_ = cwal_scope_empty;
   cwal_scope * sub = &sub_;
   int rc = cwal_scope_push(e, &sub);
   if(rc) { ...error... do NOT pop the scope ...}
   else {
   ...do your work, then...
   cwal_scope_pop(sub);
   }
   @endcode

   (An easy way to determine, at the end of the function, whether
   'sub' was pushed is to check if sub->parent is 0 (in which case it
   was not pushed).)

   Using scopes this way can be considered a malloc-count
   optimization over simply passing NULL as the 's'
   parameter. Initializing scopes this way does not change how
   they are popped: cwal_scope_pop() is still the correct way to
   clean up the scope. Client who want to ensure that downstream
   code has not corrupted the stack can check if
   cwal_scope_current_get()==theirScopePointer before popping the
   stack (and should fail loudly if they do not match).

   Returns 0 on success. On error:

   CWAL_RC_MISUSE: e is NULL

   CWAL_RC_OOM: memory allocation error. This error historically could
   only happen if the user passes a NULL or a _pointer to NULL_ as the
   second parameter, otherwise this function allocates no memory and
   cannot fail if e and s are valid. However...

   As of 20181123, if the client application has a scope push hook
   (cwal_engine_vtab::hook:scope_push) installed then that hook gets
   called during this process (unless cwal needs to allocate the scope
   but cannot do so), in which case the result code from that hook is
   returned. It's conceivable that it may return a CWAL_RC_OOM, but
   any other error result seems unlikely in practice.

   When passed a client-supplied scope, this function has no error
   conditions as long as e is valid, with the caveat that a
   scope push hook (mentioned above) might fail.

   Reminder to self: practice suggests that, because scopes are
   effectively always stack-allocated, this really should take a
   pointer, not pointer-to-pointer 2nd argument. We'll add a second
   function for that case.

   @see cwal_scope_push2()
   @see cwal_scope_pop()
   @see cwal_scope_pop2()
*/
int cwal_scope_push( cwal_engine * e, cwal_scope ** s );

/**
   This is a convenience form of cwal_scope_push() which requires that
   s be non-NULL and be a freshly-initialized instance by having
   cwal_scope_empty copied over it to initialize its state. In
   practice, cwal_scope instances are always stack-allocated in a
   local function, and this variant simplifies the usage of
   such instances.

   Returns CWAL_RC_MISUSE if e or s are NULL or if s appears to
   contain any non-default state, else it returns as for
   cwal_scope_push().

   This routine is effectively simplifies this:

   @code
   cwal_scope _s = cwal_scope_empty;
   cwal_scope * s = &_s;
   int rc = cwal_scope_push( e, &s );
   @endcode

   to this:

   @code
   cwal_scope s = cwal_scope_empty;
   int rc = cwal_scope_push2( e, &s );
   @endcode

   @see cwal_scope_push()
   @see cwal_scope_pop()
   @see cwal_scope_pop2()
*/
int cwal_scope_push2( cwal_engine * e, cwal_scope * s );

/**
   Pops the current scope from the stack.

   Returns 0 on success. Returns CWAL_RC_MISUSE if !e and
   CWAL_RC_RANGE if e has no current scope (use cwal_scope_push()
   first)).
       
   This "really shouldn't" ever fail, and a failure is likely either
   the result of a cwal-internal bug or serious offenses against the
   memory management rules.

   @see cwal_scope_pop2()
*/
int cwal_scope_pop( cwal_engine * e );

/**
   A variant of cwal_scope_pop() which rescopes the given value (if
   not NULL) to the parent of the to-pop scope (if needed).

   This is intended to simplify propagating result values up the scope
   stack.

   Returns 0 on success, else:

   - CWAL_RC_MISUSE if !e

   - CWAL_RC_RANGE if resultVal is not NULL and e's top-most scope is
   the current scope (because no value may outlive the top scope).

   Returns 0 on success, else (except as mentioned above) as
   documented for cwal_scope_pop().

   Note that the reference count of the 2nd argument is not modified
   by this routine. Whether or not it needs/has a reference is
   entirely up to the surrounding code.

   Example:

   @code
   int rc;
   cwal_value * v;
   cwal_scope scope = cwal_scope_empty;
   rc = cwal_scope_push2( e, &scope );
   if(rc) return rc;
   assert(scope.parent); // just to demonstrate
   v = ...;
   cwal_value_ref(v);
   ... other stuff ...
   rc = cwal_scope_pop2( e, v );
   assert(!scope.parent); // just to demonstrate
   assert(cwal_value_refcount(v) || cwal_value_is_builtin(v));
   // Eventually we need to relinquish that ref. In the case
   // of propagating values, we normaly want:
   cwal_value_unhand(v);
   // So that we can pass the value back up to the caller with
   // a predictable, yet possibly zero, refcount.
   // If we instead want to discard the value altogether then
   // we need: cwal_value_unref(v);
   @endcode

   @see cwal_scope_pop()
   @see cwal_scope_push()
   @see cwal_scope_push2()
   @see cwal_value_unhand()
*/
int cwal_scope_pop2( cwal_engine * e, cwal_value * resultVal );

/**
   "Might partially clean up" the given scope, as follows...

   For each value owned by scope which has a reference count of
   _exactly_ 0, it is unref'd. Values with a refcount of 0 are
   considered "probationary" values, subject to summary cleanup.
   Once a value has ever had a reference added to it, it moves out
   of probationary status and cannot be affected by sweep
   operations unless they are once again re-probated (which can
   happen in one of several special cases). Thus this could be
   called after (or periodically during) loops which create lots
   of anonymous/throw-away values.

   It is only safe to call this if the client has explicitly
   referenced all values from the current scope which he is still
   holding a pointer to (and expects that pointer to be valid after
   this call). See cwal_new_VALUE() for a description of how
   references are acquired.

   Returns the number of unref's triggered by the sweep, or 0
   if any arguments are invalid. Note that it does not count
   recursively-removed values in the return code because
   those cleanups happen at a different level.

   Performance is effectively O(N+M) when no cycles are
   introduced, where N=total number of probationary values and M
   is the cleaning costs of those values cleaned. Cycles
   theoretically cannot happen in a probationary object because
   that would necessarily cause a refcount increase of the value
   partaking in the cycle, which would move the value out of
   probationary state.

   Note that if two calls are made to this function without having
   allocated/transfered new values from/to s, the second (and
   subsequent) will be no-ops because only probationary values are
   affected in any way.

   There are certain abstract operation chains where calling this
   will almost certainly be fatal to the app. For example,
   consider this pseudocode:

   @code
   myFunction( 1+2, 7*8, somethingWhichSweeps );
   @endcode

   The sweep activated in the 3rd argument could (depending on how the
   arguments are collected) destroy the temporaries before they get
   passed on to the function. Thus it is important that evaluation
   engines hold refs to all values they're working with.

   Earlier in the docs we mentioned a special corner case where a
   value can re-enter probationary state. This happens when moving
   values up scopes while containers in lower (newer) scopes holding
   references to them get cleaned up. Example code taken from th1ish:

   @code
   assert 17 === false || scope {
   var obj = object {a:17}
   obj.a // implicit scope result value
   }
   @endcode

   Normally obj.a would be cleaned up by obj at scope's end, but the
   'scope' operator supports implicit returns and thus needs to pass
   it unmolested up the scope chain. In this case, that moves (former)
   obj.a into the parent scope with a refcount of 0, moving it back
   into probationary state.

   @see cwal_engine_sweep()
   @see cwal_engine_vacuum()
*/
cwal_midsize_t cwal_scope_sweep( cwal_scope * s );

/**
   Returns the cwal_engine which owns s, or NULL if s is NULL.
*/
cwal_engine * cwal_scope_engine(cwal_scope const * s);

/**
   Calls cwal_scope_sweep() on e's current scope, returning the
   result of that call. Returns 0 if !e or if e has no current
   scope.

   @see cwal_scope_sweep()
   @see cwal_engine_vacuum()
*/
cwal_midsize_t cwal_engine_sweep( cwal_engine * e );
    
/**
   If allScopes is false, this behaves like cwal_engine_sweep(),
   otherwise it sweeps each scope, as per cwal_scope_sweep(),
   starting at the current scope and working upwards in the
   stack. Returns the total of all resulting cwal_scope_sweep()
   calls.

   This is inherently a dangerous operation as it can sweep up values
   in older scopes which are being used by the current scope if those
   values do not have a reference somewhere. Potential culprits here
   include:

   - Temporaries created while evaluating function arguments,
   which then get passed (without an explicit ref) to a function
   which triggers the recursive sweep. If the arguments get
   a reference, they are not problematic here.

   - Propagating result values, because they are not tracked directly
   by cwal, are not exempt from sweep-up. Initially, cwal kept track
   of a single result value, but it turned out (in th1ish) to be much
   easier to do from client code (the script interpreter). Maybe we
   can revisit that design decision someday. (Someday: see
   cwal_propagating_get() and cwal_propagating_set().)

   - Propagating exceptions have a reference, so are immune to
   sweep-up.
*/
cwal_midsize_t cwal_engine_sweep2( cwal_engine * e, bool allScopes );


/**
   This function cleans up all values owned by the current scope
   by determining whether or not they refer to, or are referred to
   by, scope-level properties (variables).

   The mechanism is relatively simple:

   1) Push a new scope onto the stack with the same parent as e's
   current scope, but fake its level to (s->level-1) so that it
   looks like an older scope. We'll call the current scope s1 and
   this new scope s2.

   2) Upscope s1's object properties and any values in s1 marked
   as vacuum-proof into s2. Because s2 looks like an older scope,
   this will transfer any values in s1 which are variables,
   vacuum-proof, or reachable via either of those, leaving any
   orphaned values in s1 but not in s2.

   3) Clean all values remaining in s1.

   4) Re-set s2's parent to be s1 and fake s2's level to
   (s1->level+1) so that s2 looks like a newer scope.

   5) Upscope (again) the object properties and vacuum-proofed values,
   this time from s2 to s1. Because of step 4, s1 now looks like a
   higher/older scope to the rescope process, which will move the
   variables, and values referenced by them, back into s1. Note that
   we cannot simply move the value lists from s2 to s1 because we need
   to ensure that the value->scope pointers all point to where they
   need to, and the underlying engine does that for us if we just copy
   (well, move) the values back again.

   6) Clean up scope s2, as if it had been popped from the stack.

   The end result is that after this call (on success), only variabes,
   vacuum-proofed values, and values reachable via either variables or
   vacuum-proofed values, will be in the scope, all other (presumably
   script-unreachable) values having been cleaned up.

   This operation requires no allocation, just traversal of values to
   tag them with their new scope. It is, computationally, speaking,
   difficult to predict the performance. For current th1ish/s2 uses it
   is quite fast enough to run very often (after every expression
   evaluation), but very complex graphs will slow it down a bit. For
   most purposes (no graphs or only few simple ones) it can be
   considered linear on the number of values owned by the scope.

   ACHTUNG: this invalidates any and all of the following pointers:

   - Values owned by this scope but which are not reachable from
   either scope-level variables or a vacuum-proof value. They are
   destroyed.

   Returns 0 on success. On success, if sweepCount is not 0 then
   it is set to the number of values removed from the scope by
   this operaiton. If sweepCount is 0, this operation is a few
   ticks faster because it does not have to do any extra counting.

   Any error other than argument validation (CWAL_RC_MISUSE) indicates
   either an allocation problem or unexpected bits were found while
   fiddling around, either of which must be treated as fatal to the
   cwal_engine. In this case, the current scope will be cleaned up in
   its entirety (but not popped from the scope stack) because we
   simply have no other sane recovery strategy where all known values
   have some reasonable lifetime. (Sidebar: that has never happened
   in practice.)

   The only possible errors are invalid arguments or corruption
   detected during the operation. In debug builds it will assert() if
   it detects anything wrong.

   To make specific Values immune to vacuuming, use
   cwal_value_make_vacuum_proof().

   Design note: sweepCount is a signed int because initial tests
   in th1ish have added values to the scope (possibly internals
   not visible from script code), leading to an overall negative
   sweep count. i believe this to be either a th1ish-side usage
   error or bug, however: sweepCount should always be 0 or
   positive on success, and this code assert()s that that is so.

   @see cwal_value_make_vacuum_proof()
   @see cwal_engine_sweep()
*/
int cwal_engine_vacuum( cwal_engine * e, int * sweepCount );

/**
   DANGEROUS! DO NOT USE! It is retained in the public API only for my
   own testing in trying to resolve the inherent danger is poses.

   This works identically to cwal_engine_vacuum() but applies to the
   given scope, as opposed to the current (though it may be the
   current scope). Whether or not this operation is "safe" on any
   scope other than the current one very much depends on how
   client-side code manages its own cwal_value instances. DO NOT call
   this function unless you know for absolute certain that doing so is
   legal vis-a-vis the app's cwal_value management. When uncertain,
   don't call it.

   @see cwal_engine_vacuum().
*/
int cwal_scope_vacuum( cwal_scope * s, int * sweepCount );

/**
   Sets the given pointers as client state in e. What that means is:

   - e applies no meaning to the state but will, at cleanup time, call
   dtor() to clean up the state if dtor is not 0. The dtor will be
   called after all scopes are popped, so much not rely on any state
   related to a cwal_value. (Prior to 2022-03-20 the dtor was called
   before all scopes were popped, but that breaks when cwal_native
   destructors need that state for destruction purposes (been there,
   debugged that).)

   - There can be only one piece of client state installed at a time,
   and this function fails with CWAL_RC_ACCESS if state
   is already set (to avoid having to answer questions about its
   lifetime).

   - cwal_engine_client_state_get() can be used, passed the same
   (e, typeId) values used here, to fetch the pointer later on.
   Calls to that function with other (e, typeId) combinations will
   return 0.

   The typeId can be an arbitrary pointer value, but must outlive e.
   It is typically the address of some static const value associated
   with state's concrete data type.

   Returns 0 on success. Errors include:

   - CWAL_RC_MISUSE if e or state are 0 (typeId and dtor may be 0)

   - CWAL_RC_ACCESS if state has already been sete on e.
*/
int cwal_engine_client_state_set( cwal_engine * e, void * state,
                                  void const * typeId, cwal_finalizer_f dtor);

/**
   If cwal_engine_client_state_set() was passed e and typeId at some
   point, and it has not be re-set since then, then this returns the
   state pointer, otherwise it returns 0.
*/
void * cwal_engine_client_state_get( cwal_engine * e, void const * typeId );

/**
   Simplified form for cwal_scope_current() which returns the
   current scope, or 0 if !e or if there are no scopes.
*/
cwal_scope * cwal_scope_current_get( cwal_engine * e );

/**
   Interpreter-level flags for "variables." Maintenance reminder: keep
   these flags at 16 bits or less (see cwal_kvp::flags).
*/
enum cwal_var_flags_e {
/**
   The no-flags/default value.
*/
CWAL_VAR_F_NONE = 0,
/**
   Indicates that the variable/property should be "const."
   Property-setting routines must refuse to re-set a property which
   has this flag. cwal_props_clear() (and similar) must ignore this
   flag and clear the property.
*/
CWAL_VAR_F_CONST = 0x0001,

/**
   Indicates that property iteration operations on types capable of
   holding key/value pairs should not expose properties with this flag
   via iteration-like routines, e.g. cwal_props_visit_kvp() and
   friends. Such values will be found normally if searched for by
   their key.
*/
CWAL_VAR_F_HIDDEN = 0x0002,

/**
   Indicates that any existing flags of the property should be
   kept as-is. For newly-created properties this is applied as if
   it were CWAL_VAR_F_NONE.

   Maintenance reminder: must currently be 16 bits.
*/
CWAL_VAR_F_PRESERVE = 0xFFFF

};

/**
   Flags for use exclusively with container-type values,
   cwal_container_flags_set(), and cwal_container_flags_get().

   These are limited to 16 bits.

   @see cwal_container_flags_set()
   @see cwal_container_flags_get()
*/
enum cwal_container_flags {
/**
   Tells the engine that setting object properties OR hash entries on
   this container is not allowed, and should trigger a
   CWAL_RC_DISALLOW_PROP_SET error.

   The restriction on hash entries is arguably a bug, but s2 currently
   relies on it (201912). FIXME: separate that case into a separate
   flag and have s2 accommodate that.

   Note that this restriction does not apply to array/tuple indexes,
   but it probably should (noting that tuples do not have container
   flags!). That behaviour may change in the future or another flag
   may be added to cover that case (for arrays, at least). In
   particular, it would be useful for tuples which are used as
   object/hash keys, to ensure that they keep a stable sort order.
*/
CWAL_CONTAINER_DISALLOW_PROP_SET = 0x0001,

/**
   Tells the engine that setting NEW properties on this container is
   not allowed, and should trigger a CWAL_RC_DISALLOW_NEW_PROPERTIES
   error.

   Note that this restriction does not apply to array indexes, but it
   probably should. That behaviour may change in the future.
*/
CWAL_CONTAINER_DISALLOW_NEW_PROPERTIES = 0x0002,

/**
   Specifies that cwal_value_prototype_set() should fail with code
   CWAL_RC_DISALLOW_PROTOTYPE_SET for a value with this flag.
*/
CWAL_CONTAINER_DISALLOW_PROTOTYPE_SET = 0x0004,

CWAL_CONTAINER_RESERVED1 = 0x0008 /* DISALLOW_LIST_SET? arrays? */,
CWAL_CONTAINER_RESERVED2 = 0x0010 /* DISALLOW_HASH_INSERT? */,

/**
   EXPERIMENTAL and likely to never enter public service. A flag for
   Functions indicating that they are to be treated as property
   interceptors via certain getter/setter APIs.
*/
CWAL_CONTAINER_RESERVED3 = 0x0020,
/**
   EXPERIMENTAL and likely to never enter public service. Transient
   flag to avoid recursion.
*/
CWAL_CONTAINER_RESERVED4 = 0x0040

};

/**
   Returns s's property storage object, instantiating it if
   necessary. If s is NULL, or on allocation error, it returns
   NULL. The returned Value will be an Object (prior to 2021-08-01
   it "might" have been a hash, depending on a now-removed feature
   flag).

   s holds a reference to this container and the API offers no way to
   take ownership away from s, other than prying it from s's cold,
   dead fingers (which we'll leave as an exercise to the reader (tip:
   reference and rescope the properties before popping s)).

   It is strongly recommended that client code NOT expose this value
   via script-side features. The temptation to do so is strong, but
   all of the potential side-effects of doing so are not yet fully
   understood, and there are caveats vis-a-vis vacuum-safety, in
   particular if the returned object is transferred to a different
   scope, e.g. via propagation.
*/
cwal_value * cwal_scope_properties( cwal_scope * s );

/**
   Returns the parent scope of s, NULL if !s or s has
   no parent.
*/
cwal_scope * cwal_scope_parent( cwal_scope * s );

/**
   Returns the top scope in s's stack, NULL if !s.
*/
cwal_scope * cwal_scope_top( cwal_scope * s );
    
/**
   Searches s and optionally its parents for the given key.  If
   maxDepth is greater than 0 then only up to that many scope
   levels is searched. If maxDepth is less than 0 then any number
   of parent levels can be searched. A maxDepth of 0 means to
   search only s.

   If foundIn is not NULL, it is assigned the scope in which
   the property is found.
       
   Returns NULL if !s, !k, or no entry is found, else returns the
   searched-for value.

   See cwal_prop_get_kvp_v() for import notes about the
   lookup/property key comparisons.

   This function does resolve propref values. If it expands any
   values, the resulting `*foundIn` value will refer to the container
   of the resolved property. If that distinction is important, use
   cwal_scope_search_kvp_v(), noting that it does not resolve
   proprefs.

   Reminder to self for next time: practice has shown that the
   maxDepth bit is overengineered. A simple boolean stating whether to
   search parents or not suffices.

   @see cwal_prop_get_kvp_v()
   @see cwal_scope_search()
*/
cwal_value * cwal_scope_search_v( cwal_scope * s, int maxDepth,
                                  cwal_value const * const k,
                                  cwal_scope ** foundIn );

/**
   Functionally equivalent to cwal_scope_search_v() except that it
   takes a C-string key and can only match String-typed keys. The
   first keyLen bytes of key are used as the search key.

   Returns as described for cwal_scope_search_v(),
   and also returns NULL if (!key) or keyLen is too long.

   See cwal_prop_get() for details about the lookup key
   comparison.

   If the resolve value is a propref and foundIn is not NULL then
   `*foundIn` is set to the scope which contains the _resolved_
   value. If that distinction is important, use
   cwal_scope_search_kvp(), noting that it does not resolve proprefs.

   See cwal_prop_get_kvp_v() for a caveat about CWAL_TYPE_PROPREF
   values.

   @see cwal_scope_search_kvp_v()
   @see cwal_scope_search_kvp()
*/
cwal_value * cwal_scope_search( cwal_scope * s,
                                int maxDepth,
                                char const * key,
                                cwal_midsize_t keyLen,
                                cwal_scope ** foundIn );

/**
   Similar to cwal_prop_get_kvp_v(), but searches through a scope
   (optionally recursively). upToDepth is interpreted as described
   for cwal_scope_search_v(). If a match is found, the underlying
   key-value pair is returned and foundIn (if not 0) is assigned to
   the scope in which the match was found.

   Returns 0 if no mach is found, !s, or !key.

   This routine explicitly does not search for properties via
   prototypes of the scope's property storage. i.e. only "declared"
   variables can be found this way, not properties inherited by
   the underlying property storage object/hash.

   This function does not resolve propref values, returning them
   as-is.

   ACHTUNG: the returned object is owned by an object which is owned
   either by the scope the key is found in or an older one, and it may
   be invalidated on any modification of that object (i.e. any
   changing of properties in that scope). i.e. don't hold on to this,
   just grab its flags or whatever and let go of it.

   @see cwal_scope_search_kvp_v()
   @see cwal_scope_search()
*/
cwal_kvp * cwal_scope_search_kvp_v( cwal_scope * s,
                                    int upToDepth,
                                    cwal_value const * key,
                                    cwal_scope ** foundIn );

/**
   The C-string counterpart of cwal_scope_search_kvp_v(). The first
   keyLen bytes of key are used as the search key.

   See cwal_prop_get() for details about the lookup key
   comparison.

   See cwal_prop_get_kvp_v() for a caveat about CWAL_TYPE_PROPREF
   values.

   @see cwal_scope_search_kvp_v()
   @see cwal_scope_search()
*/
cwal_kvp * cwal_scope_search_kvp( cwal_scope * s,
                                  int upToDepth,
                                  char const * key,
                                  cwal_midsize_t keyLen,
                                  cwal_scope ** foundIn );


/**
   Identical to cwal_scope_chain_set_with_flags_v(), passing
   CWAL_VAR_F_PRESERVE as the final parameter.
*/
int cwal_scope_chain_set_v( cwal_scope * s, int upToDepth,
                            cwal_value * key, cwal_value * val );

/**
   Sets a property in s or one of its parent scopes. If upToDepth
   is 0 then the property will be set in s, else s and up to
   upToDepth parents will be searched for the key (e.g. a value of
   1 means to check this scope and its parent, but no higher). If upToDepth
   is negative it means "arbitrarily high up in the stack." If
   it is found then it is set in the scope it was found in, else
   it is set in s.

   To unset a key, pass a val of NULL.

   Returns 0 on success. Its error codes and the flags are the same as
   for cwal_prop_set_with_flags_v(), with one exception: if scopes are
   configured to use hashes for property storage then this routine
   will (as of 20191211) return CWAL_RC_IS_VISITING_LIST if the
   property storage hash in which the property would be set is
   currently being iterated over or is otherwise temporarily locked
   against modification.
*/
int cwal_scope_chain_set_with_flags_v( cwal_scope * s, int upToDepth,
                                       cwal_value * k, cwal_value * v,
                                       uint16_t flags );

/**
   Identical to cwal_scope_chain_set_with_flags(), passing
   CWAL_VAR_F_PRESERVE as the final parameter.
*/
int cwal_scope_chain_set( cwal_scope * s, int upToDepth,
                          char const * k, cwal_midsize_t keyLen,
                          cwal_value * v );

/**
   The C-string form of cwal_scope_chain_set_v(), except that only the
   first keyLen bytes of k are considered as the search key.

   See cwal_prop_get() for details about the lookup key
   comparison.
*/
int cwal_scope_chain_set_with_flags( cwal_scope * s, int upToDepth,
                                     char const * k, cwal_midsize_t keyLen,
                                     cwal_value * v, uint16_t flags );

/**
   Copies properties from src to dest, retaining any flags set for
   those properties. If src is-a hashtable, its hash entries are
   used, otherwise if src is a container, those properties are used.

   Returns 0 on success, CWAL_RC_MISUSE if either argument is null,
   CWAL_RC_TYPE if dest is not properties-capable. Returns CWAL_RC_OOM
   if allocation of any resource fails while copying properties. If a
   client has made any properties of the scope "const"
   (CWAL_VAR_F_CONST) then this function will fail with
   CWAL_RC_CONST_VIOLATION if an attempt is made to import a symbol
   with the same key.
*/
int cwal_scope_import_props( cwal_scope * dest, cwal_value * src );

/**
   "Declares" a variable in the given scope. Declaring is almost
   identical to setting (cwal_scope_chain_set() and friends) but fails
   with CWAL_RC_ALREADY_EXISTS if the given entry is already declared
   (or set) in s. In addition, if v==NULL then cwal_value_undefined()
   is used as the default.

   If s is NULL then e's current scope is used. If e is 0 then s's
   engine is used. If both are NULL, CWAL_RC_MISUSE is returned.
       
   Returns CWAL_RC_MISUSE if key is NULL or empty (or otherwise
   starts with a NUL byte).

   Returns CWAL_RC_OOM on allocation error.

   Returns (as of 20191211) CWAL_RC_IS_VISITING if s's property
   storage *object* is currently being iterated over or is otherwise
   locked, and CWAL_RC_IS_VISITING_LIST if s's propert storage *hash*
   is in that state.
*/
int cwal_var_decl_v( cwal_engine * e, cwal_scope * s, cwal_value * key, cwal_value * v,
                     uint16_t flags );
/**
   Functionally identical to cwal_var_decl_v(), but takes a
   C-style string (key) which must be keyLen bytes long.
*/
int cwal_var_decl( cwal_engine * e, cwal_scope * s, char const * key,
                   cwal_midsize_t keyLen, cwal_value * v,
                   uint16_t flags );

/**
   cwal_value_unref() is THE function clients must use for
   destroying values allocated via this framework. It decrements
   the reference count of a cwal_value, cleaning up if needed.

   Whether or not clients must (or should) call this function
   depends on how the values are used. Newly-created values have a
   reference count of 0. This reference count is increased when
   the value is added to a container (as a key _or_ value) or the
   client calls cwal_value_ref(). If a client calls
   cwal_value_ref() then he is obligated to call this function OR
   allow the scope to clean up the value when the scope is
   popped. If a client creates a new value, does not explicitly
   reference, but adds it to a container (implicitly referencing
   it) then he must _NOT_ call cwal_value_unref() (doing so will
   leave a dangling pointer in the container).

   Caveat: unref'ing a STRING value without having explicitly
   referenced it (cwal_value_ref()) can potentially be dangerous
   if string interning is enabled and more than one (shared)
   reference to that string is alive. This does not apply to
   other value types.

   It is never _necessary_ to call this function: if it is not
   called, then the value's owning scope will clean it up whenever
   the scope is cleaned up OR (for newly-allocated values with no
   refcount) during a sweep or vacuum operation (see
   cwal_engine_sweep() and cwal_engine_vacuum()).

   The safest guideline for client usage, unless they really know
   what they're doing and how to use/abuse this properly:

   - DO NOT EVER call this function UNLESS one of the following
   applies:

   A) You have called cwal_value_ref() on that value.

   B) You created the value using cwal_new_TYPE() (or one of the
   various convenience variants) AND it is NOT a CWAL_TYPE_STRING
   value (x-strings and z-strings ARE safe here).

   All other uses, unless the caller is intricately familiar with
   cwal's memory management, "might" be dangerous.

   String interning (if enabled) leaves open a corner case where it is
   not safe to call this on a string (CWAL_TYPE_STRING) value unless
   every instance of that string has been explicitly
   cwal_value_ref()'d string or the client otherwise is very, very
   (almost inconceivably VERY) much certain about its ownership. Note
   that string interning does not apply to x-strings and z-strings, so
   they are "safe" in this context. This does not mean string
   interning is unsafe in general (in normal uses cases it works just
   fine), just that it opens up a corner case involving shared strings
   which does not apply to other types. Specifically: if clients
   ref/unref interned strings 100% symmetrically (meaning always ref
   and always unref), there will will/should be no problems. That is
   not feasible to do in some client code, however, in particular
   where temporary values are involved (which is where interning
   causes the the headaches).

   Clients MUST treat this function as if it destroys v; it has
   semantically the same role as free(3) and v must not be used by
   the client after this function is called.  Likewise, the return
   values may, for essentially all purposes, be ignored by the
   client, but this function returns a value to describe what it
   actually does, the semantics of which are somewhat different
   from the rest of the framework (i.e. non-0 is not necessarily
   an error):

   CWAL_RC_MISUSE if !v.

   CWAL_RC_OK if this function does nothing but that's not an
   error (e.g. if passed a handle to one of the built-in constant
   values).

   CWAL_RC_DESTRUCTION_RUNNING if v is currently being destroyed. This
   result should ONLY be returned while destructing a graph in which v
   has cycles. Client code should never see this unless they are doing
   manual cleanup of container values in the destructors of their own
   custom cwal_native implementations.

   CWAL_RC_HAS_REFERENCES if v was not destroyed because it still has
   pending references.
       
   CWAL_RC_FINALIZED if this function actually finalizes the value
   (refcount drops to (or was) zero).
    
   Implementation notes:

   - This function might recycle v's memory for the next allocation of
   the same value type. Some types are not recycled or are recycled
   differently (namely strings because their allocation mechanism
   limits how well we can recycle them). The interning mechanism,
   however, (if enabled) ensures that we don't need to alloc/free
   strings in many common usage patterns.

   - Note that built-in/constant values do not actually participate in
   reference-counting (see cwal_value_is_builtin()) but, for reasons
   of consistency, should be treated as if they do, and should be
   passed to this function just like any other values (it is a
   harmless no-op).

   @see cwal_value_ref()
   @see cwal_value_unhand()
   @see cwal_refunref()
*/
int cwal_value_unref( cwal_value * v );

/**
   A very close relative of cwal_value_unref(), this
   variant behaves slightly differently:

   - If v is NULL or has no refcount, it is left
   untouched. Builtin values are implicitly covered by this
   condition.

   - If v has a positive refcount, its refcount is reduced by 1.
   If the refcount is still above 0, there are no further
   side-effects. If the refcount drops to 0, v is _reprobated_ in
   its current owning scope. That means that it gets moved into
   the list of values which which can be swept up by
   cwal_engine_sweep() and friends. It will not, however, be
   immediately destroyed by reaching a refcount of 0.

   Returns v with one exception (which will never happen in perfectly
   well-behaved code): if v is not NULL and this function can
   determine that v is no longer valid (e.g. it's officially been
   destroyed, is awaiting destruction, or is sitting in a recycling
   bin) then it will assert() and then return NULL. (The assert() is a
   no-op in non-debug builds.) If it ever returns NULL when passed
   non-NULL then something quite fatal has happened and the app should
   treat it as a cwal-fatal error. That will only happen if the value
   is involved in serious reference mismanagement. Continuing to use
   such a corrupted Value from client code leads to Undefined
   Behaviour.

   The intended use of this function is to "let go" of a value with a
   clean conscience when propagating it, without outright destroying
   it or having to be unduly uncertain about whether it will survive
   the currently-evaluating expression. This allows them to take
   advantage of scope lifetimes and sweep intervals (both of which
   they control) to more closely manage the lifetimes of values which
   are potentially temporary but will possibly be used as result
   values (meaning that they need to survive a bit longer, despite
   having no clear owner-via-reference-count).

   Example:

   @code
   int myfunc( cwal_engine * e, cwal_value ** result ){
   int rc;
   cwal_value * v = 0;
   v = cwal_new_string_value(e, "hi, world...", 12);
   if(!v) return CWAL_RC_OOM;
   cwal_value_ref(v);
   rc = ... do useful work ...;
   if(rc){ // error!
   // probably destroys x (if it's not gained another ref via
   // whatever work we did;
   cwal_value_unref(v);
   }else{
   // remove our reference to v without the possibility of
   // destroying it, so that it has a predictable refcount
   // when the caller gets it...
   *result = cwal_value_unhand(v);
   }
   return rc;
   }
   @endcode

   API change: before 2018-11-24 this function returned void.

   @see cwal_value_ref()
   @see cwal_value_unref()
   @see cwal_refunref()
*/
cwal_value * cwal_value_unhand( cwal_value * v );

/**
   A convenience alias for cwal_value_ref().
*/
#define cwal_ref(V) cwal_value_ref(V)

/**
   A convenience alias for cwal_value_unref().
*/
#define cwal_unref(V) cwal_value_unref(V)

/**
   A convenience alias for cwal_value_unhand()
*/
#define cwal_unhand(V) cwal_value_unhand(V)

/**
   Increments v's reference count by 1 unless v is a built-in, in
   which case this is a harmless no-op.

   Returns 0 on success. The error conditions include:

   - CWAL_RC_MISUSE: v is NULL. This is harmless and okay, and lots of
   real client-side code passes a pointer to this routine without
   checking whether it's NULL or not.

   - CWAL_RC_MISUSE or an assert(): v has no scope, which indicates an
   internal error or memory mis-use/corruption (e.g. trying to ref a
   value which is currently undergoing destruction). An assert() will
   be triggered or CWAL_RC_MISUSE will be returned.

   - CWAL_RC_RANGE: incrementing would overflow past compile-time
   boundaries. This limit is, for all but the most intentionally
   malicious of purpsoses, unreachable. If this indeed ever happens
   then an assert() is triggered (in debug builds) and the associated
   cwal_engine is internally flagged as being "fatally dead", which
   will cause some several other functions to fail with this same
   result.

   In practice, the result value is ignored, as cwal will assert()
   here if it detects any serious lifetime mismanagement problems and
   there is no generic recovery strategy from what ammounts to memory
   corruption.

   Note that some built-in/constant values do not actually participate
   in reference-counting but, for reasons of consistency, should be
   treated as if they do, and may be passed to this function just like
   any other values. (See cwal_value_is_builtin() for the list of
   shared/constant values.)

   Claiming a reference point never requires a new allocation.

   Calling this function obligates the client to eventually either
   call cwal_value_unref() or cwal_value_unhand() to release the
   reference OR be content to wait until the value's owning scope
   (eventually) cleans up (at which point this value is freed
   regardless of its reference count).

   @see cwal_value_unref()
   @see cwal_value_unhand()
*/
int cwal_value_ref( cwal_value * v );

/**
   This is a obscure lifetime/cleanup related hack which can sometimes
   be used to clean up temporaries (values with a refcount of 0)
   without affecting non-temps (those with a positive refcount, or
   built-in values). Specifically, it is for cleaning up values which
   _might_ live in a higher scope but are temporaries. In some
   contexts (during cleanup of lower scopes), such values must be
   "kept around" by the engine, reinstated as temporaries (in their
   owning scope) instead of cleaning them up, in order to be able to
   handle value propagation through stack popping.

   Do not use this function without understanding exactly what it is
   for and when it is safe to use. It is often NOT safe to use, but
   only the higher-level environment can know for sure.

   This is functionally equivalent to:

   @code
   cwal_value_ref(v);
   cwal_value_unref(v);
   @endcode

   but is conserably more efficient, in that it avoids any
   intermediate movement of values those routines have to do on
   temporaries. This is (like those functions) a no-op on
   built-in/constant values (and clients should treat those just like
   any other values, so they're safe to pass here).

   The return result should be ignored by clients - it is only
   intended for use by cwal-level test code to ensure its proper
   functioning. ITS RETURN SEMANTICS ARE NOT PART OF THE PUBLIC API,
   but for the curious: it returns 0 (false) if this function has no
   side effects, true (non-0) if v is immediately destroyed by this
   call. **HOWEVER**: this info is of informational purposes only and
   should be ignored outside of test code. It MUST NOT to be used for
   any sort of application logic (e.g. "if not destroyed, unref it,"
   because that type of logic leads to all sorts of pain and suffering
   in cwal). This function MAY change to a void return at some point,
   so don't get used to actually checking the return value.

   Explaining when and why a client would want to do this would
   require that i refine the two and half years of experience which
   went into creating (well, stumbling upon) this (trivial) hack,
   and i'm finding that difficult to do. In short...

   When dealing with values created from sources other than the local
   function, it is, more often than not, not generically safe to unref
   them. In some particular cases (specific to the client application)
   it becomes, as a side effect of the other Value lifetime-related
   machinery, possible to solve the problem of "kill the value or
   not?" by simply adding a reference, then removing the
   reference. The side effect is that temporary values (those with a
   refcount of 0) will be destroyed by the call to cwal_value_unref(),
   but it has no lasting effect on non-temporaries. This routine is
   more efficient, however, in that it avoids the moving around of
   temparies into and out of the this-is-a-temp list (values of
   refcount 0 are internally kept in a separate list to facilitate
   sweeping and re-temp'ing of values in some cases after their
   refcount was formerly positive). While such movement of values is
   O(1), this routine offers a notably faster O(1) and has the same
   effect.

   cwal_value_unhand() is a close cousin of this routine.

   @see cwal_value_unhand()
*/
bool cwal_refunref( cwal_value * v );

/**
   Sets v as the (single) specially-propagating result value for
   e. This is only to be used by keywords which toss a value up
   the call stack, and use non-0 result codes to do so, but are not
   necessarily errors. e.g. return, break, exit.

   If v is not 0, a reference is added to v, held by e.

   If v is 0, any propagating value is removed from the
   propagating value slot.

   Regardless of whether or not v is 0, if e has a prior
   propagating value, it gets unref'd after referencing v (see
   cwal_value_unref()), possibly destroying it immediately.

   Returns v.

   @see cwal_propagating_get()
   @see cwal_propagating_take()
*/
cwal_value * cwal_propagating_set( cwal_engine * e, cwal_value * v );

/**
   Returns the currently specially-propagating value from e, if
   any. Ownership of the value is not modified by this call, and e
   still holds a reference to it (if the value is is not 0).

   The intention is that this value will be set for an as-yet
   unhandled RETURN or BREAK statements, as well as for an EXIT or
   FATAL (which necessarily can't be handled until the app level,
   or it loses its functionality).

   In practice, this routine is generally only used to check whether a
   propagating value has been set up, and cwal_propagating_take() is
   used for taking over ownership of that value.

   @see cwal_propagating_take()
   @see cwal_propagating_set()
*/
cwal_value * cwal_propagating_get( cwal_engine * e );

/**
   Effectively the same as calling cwal_propagating_get() followed by
   cwal_propagating_set(e,0), except that this keeps the pending
   value alive even if its refcount drops to 0. Returns the result of
   the first call. Note that the returned value may very well be a
   temporary awaiting a reference before the next sweep-up.

   @see cwal_propagating_get()
   @see cwal_propagating_set()
*/
cwal_value * cwal_propagating_take( cwal_engine * e );

/**
   This sets the given value to be e's one and only "exception"
   value.  This value is given special treatment in terms of
   lifetime - it wanders up the call stack as scopes are popped,
   until the client calls this again with x==NULL.

   x may be NULL, in which case any pending exception is cleared
   and its handle gets unreferenced (see cwal_value_unref()) and
   CWAL_RC_OK is returned. Otherwise...

   If x is not NULL then this function returns CWAL_RC_EXCEPTION
   on success(!!!) or a different non-0 cwal_rc value on
   error. The justification for this is so that it can be called
   in as the return expression for a callback which is throwing
   the exception. The exception value gets a reference held by the
   engine, and any pending exception is cwal_value_unhand()ed.

   If x is NULL then 0 indicates success.

   Interpretation of the exception value is client-dependent,
   and cwal's only special handling of it is ensuring that
   it survives the ride up a popping scope stack.

   While the exception value may be of any cwal Value type, the
   cwal_exception class is specifically intended for this purpose.

   Typical usage:

   @code
   // assuming we want to keep an existing exception:
   cwal_value * exc = cwal_exception_get(e);
   cwal_value_ref(exc);
   cwal_exception_set(e, NULL);
   cwal_value_unhand(exc);

   // Or, if we do not need to keep the old value and we KNOW that
   // nobody is holding a pointer to the exception without also
   // having added a reference, then simply:
   cwal_exception_set(e, NULL);
   @endcode

   @see cwal_exception_get()
*/
int cwal_exception_set( cwal_engine * const e, cwal_value * const x );

/**
   Convenience form of cwal_exception_set() which uses
   cwal_new_stringfv() to create an exception message string for
   the new cwal_exception value (which can be fetched using
   cwal_exception_get()). See cwal_printf() for the supported
   formatting options.

   code is the exception's error code. fmt and following arguments
   are the formatted error message.

   If !*fmt then this call creates an exception value with no
   message part.

   As a special case, certain code values will skip the creation of an
   exception and simply return that code. Currently CWAL_RC_OOM is the
   only such case.
*/
int cwal_exception_setfv(cwal_engine * const e, int code, char const * fmt, va_list args);

/**
   Identical to cwal_exception_setfv() but takes its arguments in ellipsis form.
*/
int cwal_exception_setf(cwal_engine * const e, int code, char const * fmt, ...);

/**
   A convenience helper for cwal_callback_f() implementations which
   simply passes all variadic args to cwal_exception_setfv() and
   returns that result. It must be passed the arguments object which
   is passed to the cwal_callback_f().
*/
int cwal_cb_throw( cwal_callback_args const * args, int code,
                   char const * fmt, ... );


/**
   If e has a current exception value, it is returned to the
   caller and (possibly) transfered into the calling scope (for
   lifetime/ownership purposes). If not, NULL is returned.

   Note that the lifetime of the exception value is managed internally
   by the engine to ensure that it survives as scopes are popped. If
   the client wants to stop this from happening for a given exception
   value, he should use cwal_exception_set() to set the current
   exception state to 0 (and use cwal_value_ref() to get a reference,
   if needed). That will, if the exception has a reference, keep the
   (previous) current exception rooted in its current scope, from
   which it will wander only if it is later referenced by/via an older
   scope.
*/
cwal_value * cwal_exception_get( cwal_engine * const e );

/**
   The exception-specific variant of cwal_propagating_take(),
   equivalent to calling cwal_exception_get() followed by
   cwal_exception_set(e,NULL), except that this keeps the pending
   exception alive even if its refcount drops to 0.  Returns the same
   result as cwal_exception_get(). Note that the returned value may
   very well be a temporary awaiting a reference before the next
   sweep-up.

   @see cwal_exception_get()
   @see cwal_exception_set()
*/
cwal_value * cwal_exception_take( cwal_engine * const e );

/**
   NOT IMPLEMENTED.
    
   Frees any message-related memory owned by err (or shared with it,
   in the case of err->value).

   Returns 0 on success, or CWAL_RC_MISUSE if either paramter is 0.

   After calling this, err contains an empty state and must eventually
   be deallocated using whatever mechanism complements its allocation
   (e.g. do nothing more for stack-allocated objects or those embedded
   in another struct).
*/
/*int cwal_exception_info_clear( cwal_engine * e, cwal_exception_info * err );*/

/* NOT IMPLEMENTED. */
/*int cwal_engine_err_set( cwal_engine * e, cwal_exception_info * err );*/

/**
   Returns a pointer to e's current output handler. For purposes of
   swapping them in and out, the returned value should be
   bitwise-copied for later swapping in via
   cwal_engine_outputer_set().  Do not store its pointer, as the
   result's address is stable for a given cwal_engine instance, but
   its contents may be swapped in and out (if done so carefully).

   This will never return NULL unless e is NULL.

   Remember that cwal_outputer::output may legally be NULL, so don't
   just assume that the returned handler can actually output anything.

   @see cwal_engine_outputer_set()
*/
cwal_outputer const * cwal_engine_outputer_get( cwal_engine const * e );

/**
   If tgt is not NULL, this function bitwise-copies e's current output
   handler to *tgt. Bit-wise copies the replacement, making it the
   new output handler.

   @see cwal_engine_outputer_get()
*/
void cwal_engine_outputer_set( cwal_engine * e,
                               cwal_outputer const * replacement,
                               cwal_outputer * tgt );

/**
   Sends (src,n) through the engine-specified output mechanism
   (specified via its vtab). See cwal_output_f() for the
   semantics. Returns 0 on success:

   CWAL_RC_MISUSE: e or src are 0.

   Any other error code is propagated from the output routine.

   This function is a no-op if n==0.

   TODO? consider making (0==src, 0==n) a heuristic for signaling
   a desire to flush the output.
*/
int cwal_output( cwal_engine * e, void const * src, cwal_size_t n );

/**
   If e's vtab is set up to be able to flush its output channel,
   this calls that function and returns its result. Returns
   CWAL_RC_MISUSE if !e or e is not initialized. Returns 0 on
   success or if there is nothing to do.
*/
int cwal_output_flush( cwal_engine * e );

/**
   printf()-like variant of cwal_output(). See cwal_printf.h for
   the format specifiers (they're pretty much standard, plus some
   extensions inherited from sqlite).
*/
int cwal_outputf( cwal_engine * e, char const * fmt, ... );

/**
   va_list variant of cwal_outputf().
*/
int cwal_outputfv( cwal_engine * e, char const * fmt, va_list args );

/**
   The cwal_new_VALUE() function does not really exist - it is
   here for documentation purposes to consolidate the common
   documentation for the large family of cwal_new_xxx() routines.
   These routines typically come in some variation of these
   three forms:

   1) cwal_value * cwal_new_SOMETHING();
   2) cwal_value * cwal_new_SOMETHING(cwal_engine*);
   3) cwal_SOMETHING * cwal_new_SOMETHING(cwal_engine*, ...);

   The first form is only for types which do not allocate memory,
   meaning types with a known set of constant values (boolean,
   undefined, null).

   The second form is only for types which need no initialization
   parameters, e.g. Objects and Arrays.

   The third form is used by types which require more information for
   their initialization. Most such types represent immutable data,
   with values which cannot be changed for the lifetime of the
   cwal_value handle.

   Ownership of the new returned value is initially held by the
   scope which is active during creationg. A newly-created value
   has a reference count of 0 (not 1, though it was in versions
   prior to 20130522). A value with a refcount of 0 is considered
   a "probationary" value, and has a special status in the
   scope-sweep operations. In short, a sweep operation will free
   up _all_ values with refcount 0 in the scope. If clients need
   to ensure a specific lifetime, they must provide the value with
   a reference. This can happen in one of several ways:

   - Insert the value into a container. e.g. set it as an Object
   key or value, or insert it into an Array.

   - Call cwal_value_ref() to increase the refcount by 1.

   If a Value is ever referenced, perhaps indirectly, from an older
   scope, it is automatically moved into that scope for
   ownership/cleanup purposes. This ensures that Values live as long
   as the last scope which references them, or until they are
   otherwise cleaned up.

   Semantically speaking this function Returns NULL on allocation
   error, but the non-allocating factories never actually allocate
   (and so cannot fail). Nonetheless, all Values returned by
   variations of this function must be treated as if they are an
   allocated Value (this consistency is encouraged to avoid
   clients special-casing code due to a cwal-internal
   implementation detail).

   General rules for the cwal_new_XXX() family of functions (all of which
   point the reader to this function) are:

   - Those which (might) allocate memory take a cwal_engine value as
   their first argument. Non-allocating factories SHOULD be treated as
   if they allocate, e.g. by assuming that they participate in the
   normal reference-counting and de/allocation mechanism (which they
   don't, actually). Note that cwal_new_string() and friends can
   return a re-used pointer for an interned string, and it is
   criticial that the client call cwal_value_ref() to increase the
   reference count by 1 to avoid any problems vis-a-vis string
   interning, and each must be followed by a call to
   cwal_value_unref(). It is worth noting that string interning has
   historically caused much Grief with regards to reference counts,
   but it behaves properly if (and only if) all string-allocating
   client code properly refs/unrefs each returned string (even if it's
   a re-used/interened string). Similarly, cwal_new_string() and
   friends may (depending on build-time options) return
   static/shared/constant memory for length-1 ASCII strings, but those
   should also be treated as if they were made up of
   dynamically-allocated memory.

   - To re-iterate: certain built-in/constant values neither allocate
   nor participate in reference-counting/scope-tracking, but that is
   an internal implementation detail, and clients should treat all
   values as equivalent for memory-management purposes except for
   noted for specific APIs.
       
   - All newly-allocated values initially have a reference count of
   0. Clients must call cwal_value_ref() to claim a reference point,
   and adding values to containers also manipulates their reference
   count. Except for strings, client code may call cwal_value_unref()
   to unreference a newly-created (not yet ref'd) value, possibly
   cleaning it up (depending on other references to the value). For
   strings, due to string interning, unref'ing is extremely unsafe
   unless the client code called cwal_value_ref() on that instance.

   Additional notes and bits of wisdom earned via long-time use of
   this library...

   When inserting new values into containers, the safe/proper thing to
   do is to first reference the value, then perform the insertion,
   then, regardless of whether the insertion worked, unref the
   value. For example:

   @code
   int rc;
   cwal_value * v = cwal_new_integer(myEngine, 42);
   if(!v) return CWAL_RC_OOM;
   cwal_value_ref(v); // always obtain a reference
   rc = cwal_array_append(myArray, v);
   cwal_value_unref(v); // unref after the insertion
   if(rc) return rc;
   // On insertion success, the container now has the only reference
   // to v, and v is still alive. On error, v was cleaned up by
   // our call to cwal_value_unref().
   v = 0;
   @endcode

   If the container insertion succeeds, that container will have
   obtained a reference to the Value. If insertion fails, it will not.
   Either way, our call to cwal_value_unref() removes our local
   reference to the Value, making it _semantically_ illegal for us to
   refer to the that Value again.

   Code like the following will "normally" work but has some
   non-obvious pitfalls which may bite the client mightily somewhere
   down the road:

   @code
   // Expanding on the example above...
   // DO NOT DO THIS... DO NOT DO THIS... DO NOT DO THIS...
   v = cwal_new_integer(myEngine, 42);
   // v has a refcount of 0
   rc = cwal_array_append(myArray, v);
   // On success ^^^^ v has a refcount of 1, on error 0.
   if(rc) cwal_value_unref(v); // cleans up v
   v = 0;
   @endcode

   That would actually work fine (most of the time) for most types,
   but that approach breaks down in conjunction with cwal's
   string-interning feature (and _potential_ future interning/re-use
   features for other Value types (e.g. integers)). In the case of
   interned/reused values, the cwal_new_XXX() call will return an
   existing value but not increase its reference count, meaning that
   any unref in the above block is strictly illegal (because it
   doesn't obtain its own reference via cwal_value_ref()). In
   practice, misuse like the above may not trigger a problem until
   much later (especially when recycling is enabled, as that can
   temporarily hide this type of misuse), and will eventually trigger
   an assert() in cwal.

   Likewise, do not do the following:

   @code
   // Expanding on the example above...
   // DO NOT DO THIS... DO NOT DO THIS... DO NOT DO THIS...
   rc = cwal_array_append(myArray, cwal_new_integer(myEngine, 42));
   @endcode

   The problem there is that if the container insertion fails, we've
   "leaked" the newly-created Value and we have no way of referencing
   that value ever again. That "leaked" value will be cleaned up when
   the current cwal scope (see cwal_scope_push()) is popped. Likewise,
   the "leaked" value, because it has no reference count, would be
   cleaned up by a call to cwal_engine_sweep() or cwal_engine_vacuum()
   if that call were made within the context of the current cwal
   scope. So the value is not "really" leaked, in the classic sense of
   a memory leak, but it is effectively leaked until cwal gets around
   to cleaning it up (which, depending on client-specific usage, might
   not be until the cwal_engine instance is finalized).
*/
void cwal_new_VALUE(cwal_engine * e, ...);

/**
   Creates a new cwal_value from the given boolean value.

   Note that there are only two boolean values and they are singletons
   - the engine never allocates memory for booleans.

   @see cwal_new_VALUE()
*/
cwal_value * cwal_new_bool( int v );

    
/**
   Semantically the same as cwal_new_bool(), but for doubles.

   cwal's numeric-type Values (doubles and integers) are immutable -
   they may never be modified after construction.

   The engine reserves the right to compile certain numeric Values as
   built-in constants. Such values do not require any dynamic memory
   but behave just like normal, dynamically-allocated Values except
   that:

   - All refcount operations on such values are no-ops.

   - All flag-setting operations (e.g. vacuum-proofing) on such values
   are no-ops.

   - All such values are immune to garbage-collection.

   Nonetheless, client code must behave exactly as if such values were
   "normal" values, and all cwal APIs support this (by ignoring, where
   appropriate, operations on built-in values). e.g. client code must
   ref/unref these Values as if they were normal Values, even though
   those operations are no-ops for built-in constants.

   Which, if any, numeric Values are built-in constants may differ in
   any given build of this library. Client code must never attempt to
   apply different logic to built-in values, as that logic may or may
   not apply to any given build.

   @see cwal_new_VALUE()
   @see cwal_new_bool()
*/
cwal_value * cwal_new_double( cwal_engine * e, cwal_double_t v );

/**
   Semantically the same as cwal_new_double(), but for integers.

   @see cwal_new_VALUE()
   @see cwal_new_double()
*/
cwal_value * cwal_new_integer( cwal_engine * e, cwal_int_t v );

/**
   Returns a new "unique" value. Unique values are unusual in that:

   - Their identity is their value.

   - They may optionally wrap a single other value. They acquire a
   reference to that value and unref it up when the Unique is cleaned
   up. The value gets upscoped, if necessary, as the Unique gets
   upscoped (but the reverse is not true: the binding is not
   two-way!).

   - They always resolve to true in a boolean context (i.e. via
   cwal_value_get_bool()).

   - They never compare as equivalent to any value other than
   themselves (not even cwal_value_true(), even though it evaluates to
   true in a boolean context).

   They are intended to be used as opaque sentry value or possibly as
   a sort of enum entry substitute.

   Returns 0 if !e or on OOM, else it returns a new value instance
   with a refcount of 0.

   Sidebar: unique values are not containers, per se, and cannot hold
   any per-instance properties. Nonetheless, unlike most
   non-containers, they may participate in cycles. Also, unlike most
   other types, Uniques have no higher-level class representation
   (i.e. there is no Unique-type counterpart of cwal_object or
   cwal_string).

   @see cwal_unique_wrapped_set()
   @see cwal_unique_wrapped_get()
*/
cwal_value * cwal_new_unique( cwal_engine * e, cwal_value * wrapped );

/**
   If uniqueVal is of type CWAL_TYPE_UNIQUE (was created using
   cwal_new_unique()) and it has a wrapped value, that value is
   returned, otherwise 0 is returned. Ownership of the returned
   value is not modified.

   @see cwal_unique_wrapped_set()
   @see cwal_new_unique()
*/
cwal_value * cwal_unique_wrapped_get( cwal_value const * uniqueVal );

/**
   If uniqueVal is of type CWAL_TYPE_UNIQUE then this function sets
   uniqueVal's wrapped value to w. Any prior wrapped value is unref'd
   and may be cleaned up immediately.

   Returns 0 on success, CWAL_RC_TYPE if uniqueVal is not a
   Unique-type value, CWAL_RC_CYCLES_DETECTED if w==uniqueVal. (Note
   that it does not catch nested cycles-to-self, as those are legal
   (and would take arbitrarily long to find if they weren't).)

   w may be 0, but if the client wants to keep any current wrapped
   value alive, he needs to ensure he's got a reference point for it
   before passing 0 here, as this routine will destroy it if its
   reference count drops to 0 during this call.

   This is a no-op, returning 0, if w is the same value uniqueVal
   already wraps.

   @see cwal_unique_wrapped_get()
   @see cwal_new_unique()

*/
int cwal_unique_wrapped_set( cwal_value * uniqueVal, cwal_value * w );


/**
   Returns the special "null" singleton value.

   See cwal_new_VALUE() for notes regarding the returned value's
   memory.

   @see cwal_new_VALUE()
*/
cwal_value * cwal_value_null(void);

/**
   Returns the special "undefined" singleton value.
   
   See cwal_new_VALUE() for notes regarding the returned value's
   memory.

   @see cwal_new_VALUE()
*/
cwal_value * cwal_value_undefined(void);

/**
   Equivalent to cwal_new_bool(0).

   @see cwal_new_VALUE()
   @see cwal_new_bool()
*/
cwal_value * cwal_value_false(void);

/**
   Equivalent to cwal_new_bool(1).

   @see cwal_new_VALUE()
   @see cwal_new_bool()
*/
cwal_value * cwal_value_true(void);

/**
   Converts the given value to a boolean, using JavaScript semantics depending
   on the concrete type of val:

   undef or null: false
   
   boolean: same
   
   integer, double: 0 or 0.0 == false, else true
   
   object, array, hash, function, unique: true

   tuple: the empty tuple is false, all others are true.

   string: length-0 string is false, else true.

   Returns 0 on success and assigns *v (if v is not NULL) to either 0 or 1.
   On error (val is NULL) then v is not modified.

   In practice this is never used by clients - see
   cwal_value_get_bool().
*/
int cwal_value_fetch_bool( cwal_value const * val, char * v );

/**
   Simplified form of cwal_value_fetch_bool() which returns 0 if val
   is "false" and 1 if val is "truthy". Returns 0 if val is NULL.
*/
bool cwal_value_get_bool( cwal_value const * val );

/**
   Similar to cwal_value_fetch_bool(), but fetches an integer value.

   The conversion, if any, depends on the concrete type of val:

   NULL, null, undefined: *v is set to 0 and 0 is returned.
   
   string, object, array: *v is set to 0 and
   CWAL_RC_TYPE is returned. The error may normally be safely
   ignored, but it is provided for those wanted to know whether a direct
   conversion was possible.

   integer: *v is set to the int value and 0 is returned.
   
   double: *v is set to the value truncated to int and 0 is returned.

   In practice this is never used by clients - see
   cwal_value_get_integer().
*/
int cwal_value_fetch_integer( cwal_value const * val, cwal_int_t * v );

/**
   Simplified form of cwal_value_fetch_integer(). Returns 0 if val
   is NULL.
*/
cwal_int_t cwal_value_get_integer( cwal_value const * val );

/**
   The same conversions and return values as
   cwal_value_fetch_integer(), except that the roles of int/double are
   swapped.

   In practice this is never used by clients - see
   cwal_value_get_double().
*/
int cwal_value_fetch_double( cwal_value const * val, cwal_double_t * v );

/**
   Simplified form of cwal_value_fetch_double(). Returns 0.0 if val
   is NULL.
*/
cwal_double_t cwal_value_get_double( cwal_value const * val );

/**
   Equivalent to cwal_string_value( cwal_new_string(e,str,len) ).

   @see cwal_new_string()
   @see cwal_new_VALUE()
*/
cwal_value * cwal_new_string_value(cwal_engine * e, char const * str,
                                   cwal_midsize_t len);

/**
   A convenience form of cwal_new_string_value() which accepts
   a negative length to tell the API to use cwal_strlen() to
   calculate the input string's length.
*/
cwal_value * cwal_new_string_value2(cwal_engine * const e,
                                    char const * const str,
                                    cwal_int_t len);

/**
   Returns a pointer to the NULL-terminated string bytes of str.
   The bytes are owned by string and will be invalided when it
   is cleaned up.

   If str is NULL then NULL is returned. If the string has a length
   of 0 then "" is returned.

   @see cwal_string_length_bytes()
   @see cwal_value_get_string()
   @see cwal_string_cstr2()
*/
char const * cwal_string_cstr(cwal_string const *v);

/**
   Equivalent to cwal_string_cstr(), but if the 2nd argument is not
   NULL, *len is set to the string's length, in bytes.

   @see cwal_string_cstr()
*/
char const * cwal_string_cstr2(cwal_string const *v, cwal_midsize_t * len);

/**
   Case-folds a UTF-8 C-string, placing the result in a new
   cwal_string instance.

   cstr must point to at least cstrLen bytes of valid UTF-8 text. This
   function converts the case of each character to upper or lower (as
   specified by the 5th parameter).

   If the 5th parameter is true (non-0), it performs up-casing, else
   it performs lower-casing. It supports all 1-to-1 case conversions
   and none of the 1-to-N/special-case conversion (such characters are
   left as-is).

   On success *rv is set to a new String value and 0 is returned. On
   error, returns:

   - CWAL_RC_MISUSE if !e, !cstr, or !rv.

   - CWAL_RC_RANGE if folding results in invalid UTF-8 characters.

   - CWAL_RC_OOM on allocation error or if the 3rd parameter exceeds
   cwal's string-length limits. In such cases, *rv may be set to 0 (or
   may be unmodified).

   @see cwal_string_case_fold()
*/
int cwal_utf8_case_fold( cwal_engine * e, char const * cstr,
                         cwal_midsize_t cstrLen,
                         cwal_value **rv,
                         bool doUpper );

/**
   Works just like cwal_utf8_case_fold() with the following
   differences:

   - Case-folded output is appended to buf, which may cause buf->mem
   to get reallocated (and thus any prior pointer to it
   invalidated). To send the output to the start of a re-used buffer,
   rather than appending at the end, set buf->used=0 before calling
   this.

   - If len is 0, this is a no-op: buf is not modified, not even to
   add a NUL byte. (Note that the buffer APIs almost always ensure tha
   buffers get a NUL byte added.)

   - On success, buf->mem gets is NUL-terminated unless len is 0, in
   which case the memory is not modified. On error, no guarantees are
   made as to its NUL-termination.

   - This updates buf->used as it goes. After completion, the "string"
   part of buf is (unless buf->mem previously held non-string data)
   the first buf->used bytes of buf->mem. (Note that that range does
   not include the NUL terminator, but almost all buffer APIs add one
   at buf->mem[buf->used].)

   Returns CWAL_RC_MISUSE if !e, !cstr, !buf, or if cstr is part of
   buf's current memory range. Otherwise it returns as documented for
   cwal_utf8_case_fold().
*/
int cwal_utf8_case_fold_to_buffer( cwal_engine * e, char const * cstr,
                                   cwal_midsize_t len, cwal_buffer *buf,
                                   bool doUpper );


/**
   Searches for the given "needle" in the given "haystack".

   The 1st and 2nd parameters delimit the area to search. The 4th and
   5th specify the thing to look for.

   The 3rd parameter specifies an optional UTF-8 character (not byte!)
   offset within the haystack to start the search. If the offset is
   negative, it is counted as the number of characters from the end of
   the haystack, but does not change the search direction (because
   counting UTF-8 lengths backwards sounds really hard ;).

   Returns a negative value if:

   - No match is found (or cannot be found because, e.g. the needle
   is larger than the haystack) or the haystack appears to not be
   UTF-8.

   - Either string is NULL or either length parameter is 0.

   If a match is found, its position is returned, but its
   interpretation depends on the value of the 6th parameter:

   If returnAsByteOffset is true then the returned value is the byte
   offset of the match, else it is the UTF-8 character offset of the
   match.

   All inputs are assumed to be valid UTF-8 text.
*/
cwal_int_t cwal_utf8_indexof( char const * haystack, cwal_size_t hayByteLen,
                              cwal_int_t charOffset,
                              char const * needle, cwal_size_t needleByteLen,
                              char returnAsByteOffset );

/**
   Identical to cwal_utf8_case_fold() except that it takes its input
   in the form of a cwal_string.

   As a special case, if str is an empty string (length of 0),
   *rv is set to its value part. i.e. the output will be the
   input, but in its alternate type pointer.

   On 20180515 a (cwal_engine*) parameter was added because this
   function otherwise fails when used on built-in static strings
   (length-1 ASCII might, depending on build options, be compiled
   in). We don't internally special-case that because special-casing
   is ugly and because that corner case is a compile-time option which
   cwal_utf.c doesn't know about.

   @see cwal_utf8_case_fold()
*/
int cwal_string_case_fold( cwal_engine * e,
                           cwal_string const * str,
                           cwal_value **rv,
                           bool doUpper );

/**
   This creates a new cwal string Value which copies the first n bytes
   of str. The new string will be NUL-terminated after n bytes,
   regardless of whether the input string is.

   ACHTUNG: prior to 20171005, n=0 meant that this function should use
   cwal_strlen() to determine str's length. That is no longer the
   case, as it caused too much special-case code client-side. n=0 now
   means an empty string (i.e. copying zero bytes from the source).

   If str is NULL or n is 0, this function still returns non-NULL
   value representing the empty string. (The empty string is a
   library-internal constant, shared across all invocations.) This
   function may return shared/constant values for certain strings
   (e.g. the empty string and _possibly_ length-1 ASCII strings). Such
   strings do not actually partake in the lifetime management system
   (e.g. their refcount is always 0) but should, from a client's
   perspective, be treated as if they are normal Values
   (e.g. adding/removing references, even though such is a no-op on
   built-in Values). See cwal_new_double() for more details about
   built-in Values.

   If len is larger than (2^(CWAL_SIZE_T_BITS-3)) then this function
   returns NULL: cwal internally reserves 3 of the size's bits for
   internal information, limiting string lengths to that number of
   bits. (Design note: it's either that or increase the
   sizeof(cwal_string) by another (padded) increment, increasing
   memory costs for all strings.) This length limit is approximately
   8KiB on 16-bit builds (meaning CWAL_SIZE_T_BITS is 16), 512MiB on
   32-bit, and rediculously high on 64-bit. The library reserves the
   right to strip a further bit or two (thereby making 16-bit builds
   unfeasible, but... so what?).

   Returns NULL on allocation error or a range limit violation (see
   above).
   
   See cwal_new_VALUE() for important information about the
   returned memory.

   ACHTUNG: see cwal_value_unref() for important notes involving
   string interning.

   In practice, it's rare for clients to use this function: normally
   cwal_new_string_value() is a more convenient choice.

   Design note/trivia: the reason the length of the string is
   required, rather than counting it automatically, is because
   tokenization of scripts often deals with byte ranges in larger
   source code strings, leading to creation of many strings from bytes
   which are not NUL-terminated. This is slightly onerous in client
   code from time to time but it is a necessary evil.

   @see cwal_string_value()
   @see cwal_new_string_value()
   @see cwal_string_cstr()
   @see cwal_string_cstr2()
*/
cwal_string * cwal_new_string(cwal_engine * e, char const * str,
                              cwal_midsize_t len);
/**
   printf-like form of cwal_new_string(). See cwal_printf() for
   the formatting specifiers.
*/
cwal_string * cwal_new_stringf(cwal_engine * e, char const * fmt, ...);
/**
   printf-like form of cwal_new_string(). See cwal_printfv() for
   the formatting specifiers.
*/
cwal_string * cwal_new_stringfv(cwal_engine * e, char const * fmt, va_list args);

/**
   Creates a new handle for an "x-string" (as in "external"). This
   is different from cwal_new_string() in that it does not copy
   str's bytes. The client must guaranty that len bytes of str are
   valid for at least as long as the returned value is used. i.e.
   this is safe to use on static strings or on buffers which the
   client can guaranty outlive the returned string.

   ACHTUNG: prior to 20171121 this function treated len=0 as a hint to
   use cwal_strlen() to determine the string's length.  It now
   requires len to be the byte length of the input string.

   Returns NULL on error.

   The returned string cannot be differentiated from a non-external
   string using the public API, with the minor exception that calling
   cwal_string_cstr() on the returned string will return the same
   C-string pointer passed to this function unless the string matches
   one of the built-in constant strings.

   Be aware that...

   - See cwal_new_string() for size limitation notes.

   - X-strings might not be NUL-terminated, so routines which
   blindly display strings until the NUL might be surprised by the
   results of doing so with cwal_string_cstr(anXString).

   - Strings shorter than sizeof(char *) are not going to get any
   memory benefits compared to non-X-strings. Use normal strings
   for those.

   - While technically this API allows strings to be non-NUL
   terminated, in practice many C APIs which get bound to scripting
   engines (not just cwal) do not take a length parameter and expect
   their inputs to be NUL-terminated. Thus it is HIGHLY RECOMMENDED
   that clients add a NUL terminator (but don't count the NUL in the
   string's length).

   - X-strings do not partake in string internalization, because doing
   so would potentially invalidate lifetime guarantees. Their "empty
   shells" (all but the external string pointer) participate in
   recycling.

   - X-strings DO partake in the length-0-string optimization, so
   cwal_new_string(e,"",0) and cwal_new_xstring(e,"",0) will
   return the same value (but that's an implementation detail
   clients should not make code-level decisions based on).
       
*/
cwal_string * cwal_new_xstring(cwal_engine * e, char const * str,
                               cwal_midsize_t len);

/**
   Equivalent to passing the return value of
   cwal_new_xstring(e,str,len) to cwal_string_value().
*/
cwal_value * cwal_new_xstring_value(cwal_engine * e, char const * str,
                                    cwal_midsize_t len);


/**
   A "z-string" is closely related to an "x-string" (see
   cwal_new_xstring()) in that the caller allocates the string, but
   (different from x-strings), the caller gives its memory over to a
   new cwal_string value. This can avoid extra copies in some cases,
   e.g. by using cwal_buffer_to_zstring() to construct a string using
   a buffer, then transfering its memory to a Z-string.

   The caller transfers ownership of str to this function, regardless
   of success or failure, and should treat it as if it were
   _immediately_ freed, using the cwal_string APIs to access it
   further.

   Note that transfer of str is only legal if str was allocated by
   the same underlying allocator as the rest of the library
   (i.e. cwal_free(str) must be legal or Undefined Behaviour may
   ensue).

   On success the ownership of str is transfered to the returned
   cwal_string value and it will be freed (via cwal_free()) when the
   cwal_string is freed or even possibly by this function. Thus it is
   critical that clients treat the str memory as invalid after calling
   this, and (to repeat) only use the cwal_string APIs to get its
   string value.

   To simplify usage, if allocation of the new cwal_string fails,
   this function _still_ takes over ownership of the given string
   memory and frees it before returning NULL from this
   call. (Design note: if we did not do this, error checking would
   become more complicated and the caller would have to decide to
   add extra checks or leak.)
       
   ACHTUNG:

   - Prior to 20171121 this function treated len=0 as a hint to use
   cwal_strlen() to determine the string's length. It now requires
   len to be the byte length of the input string.

   - See cwal_new_string() for size limitation notes.

   - z-strings do not participate in string interning, but their
   "empty shells" (and the client-supplied string bytes)
   participate in recycling of some form or another.

   - While technically this API allows strings to be non-NUL
   terminated, in practice many C APIs which get bound to scripting
   engines (not just cwal) do not take a length parameter and expect
   their inputs to be NUL-terminated. Thus it is HIGHLY RECOMMENDED
   that clients add a NUL terminator (but don't count it in the
   string's length).

   - str MUST have been allocated using the same allocator as
   cwal_malloc(e,...)  uses or results are undefined. e.g. memory from
   a cwal_buffer would be safe but memory which can from strdup(),
   malloc(), or similar "might" not be.

   - str's contents MUST NOT be modified after calling this. Doing so
   can lead to very unpredictable behaviour in code using the string
   (e.g. hashing of keys will break). The underlying laws of physics
   cwal is based on assume that string bytes are always immutable.
       
   The term "z-string" refers to a coding convention seen in some
   source tree (not this one) where pointers to strings for which the
   client owns the memory are named with a "z" prefix, e.g. zMyString.

   @see cwal_new_zstring_value()
   @see cwal_new_string()
   @see cwal_new_xstring()
*/
cwal_string * cwal_new_zstring(cwal_engine * e, char * str,
                               cwal_midsize_t len);

/**
   Equivalent to passing the result value of cwal_new_zstring(e,str,len) to
   cwal_string_value().
*/
cwal_value * cwal_new_zstring_value(cwal_engine * e, char * str,
                                    cwal_midsize_t len);

    
    
/**
   Creates a new cwal_string value by concatenating two string
   values. Returns NULL if either argument is NULL or if
   allocation of the new string fails.
*/
cwal_value * cwal_string_concat( cwal_string const * s1, cwal_string const * s2 );

/**
   An enum holding bitmasks for toggleable cwal_engine features.
   See cwal_engine_feature_flags().
*/
enum cwal_engine_features_e {
/** For internal use. All feature flags must have all their bits
    in this range. */
CWAL_FEATURE_MASK = 0xFF00,
/**
   Used in cwal_engine::flag to specify that auto-interning should
   be enabled.

   Reminder to self: these must currently reside in the high byte.
   Need to check/consolidate how the internal flags (low byte)
   are being used.

   ACHTUNG: see cwal_value_unref() for notes involving string
   interning. In short, DO NOT use string interning unless your app
   _always_ refs/unrefs all values (in particular, string values) OR
   doesn't unref any (leaving it to scope-level cleanup -
   _hypothetically_ that's safe, too). Failing to do so can lead to
   corrupting cwal state when interned copies of strings get more
   unrefs than refs (and yet the same number of refs as calls to
   cwal_new_string() and friends).

   When in doubt, leave interning disabled!
*/
CWAL_FEATURE_INTERN_STRINGS = 0x0100,

/**
   Used in cwal_engine::flags to specify that the engine should
   zero out string memory before freeing it.
*/
CWAL_FEATURE_ZERO_STRINGS_AT_CLEANUP = 0x0200
};

/**
   Sets the current set of feature flags and returns the old flags.
   Pass a negative flags value to have it return the current flags
   without setting them. flags is interpreted as a bitmask of
   cwal_engine_features_e values. This may be called during engine
   initialization (via cwal_engine_vtab::hook::on_init()).

   If !e or tracing is disabled at built-time, returns -1.

   Example:

   @code
   // Get current flags:
   uint32_t const flags = cwal_engine_feature_flags(e,-1);
   // Disable string-interning:
   cwal_engine_feature_flags(e, flags & ~CWAL_FEATURE_INTERN_STRINGS );
   @endcode

   Calling this might have side effects other than setting
   the flags. Namely:

   If CWAL_FEATURE_INTERN_STRINGS is disabled by this call (and
   was enabled before it) then the memory used for tracking
   interned strings is released. The strings are left intact and
   unaffected, but future strings with those same values will be
   created anew instead of sharing the interned values. Note that
   interning may be enabled or disabled at any time without any
   adverse effects vis-a-vis string ownership, reference counting,
   etc.

   ACHTUNG:

   One fine debugging session in early 2016 it was discovered that
   string interning (via the CWAL_FEATURE_INTERN_STRINGS flag) has a
   property which makes it dangerous to use if client code uses string
   values without acquiring explicit references to them (e.g. they are
   "temp strings"). It can happen that 2 such strings share an
   interned instance concurrently and one of them is passed to
   cwal_refunref(). That will nuke the shared instance (because it has
   no refcount) but leave one of the 2 client code locations holding a
   stale pointer to it (pointing to memory which might live in the
   recycling bin or might have been deallocated or reallocated for
   another purpose). The only solution for this is to explicitly take
   references everywhere, and release them when done. That's not
   always practical (or fun), however, and the workaround in such
   cases is to disable string interning. For completeness, here is an
   example scenario:

   1) a function uses cwal_new_string[_value]() to create a local temp
   string and does not grab a reference to it (because it's often
   (seemingly) not strictly necessary to). This function calls
   another, which calls another, which...

   2) some downstream call, allocates the same string, which string
   interning, as it's supposed to, doesn't allocate, but returns
   from the interning table (which does _not_ modify its refcount
   because that would make interned strings live forever).

   3) that downstream code, being modern and safe, adds a reference
   when it gets the string and unrefs it when done.

   4) Blamo! When the function from step (1) tries to unref (or
   cwal_refunref()) the string, it will be invoking undefined
   behaviour because the state of the stale pointer is
   indeterminate. By that time, it might have been free()'d, it might
   live in cwal's recycling bin, or it might have been reallocated for
   a different purpose altogether.

   The morale of the story is: always explicitly add/remove references
   and your code will be safe. Failing to do so can lead to
   difficult-to-track disasters. Optionally, disable/do not use string
   interning, and "most" reasonable uses of temp/local strings are
   fine and kosher (provided one does not use cwal_engine_sweep() or
   cwal_engine_vacuum(), which will clean up those temps.

   All that being said: as of 20160111, cwal_new_string() will not
   re-use an interned string which has a refcount. Instead it will go
   through the normal allocation process (which might pull from the
   recycler).
*/
uint32_t cwal_engine_feature_flags( cwal_engine * e, int32_t flags);
    
/**
   Returns the Value associated with s, or NULL if !s.

   @see cwal_new_VALUE()
*/
cwal_value * cwal_string_value(cwal_string const * s);

/**
   Returns the length of str in bytes, or 0 if !str. This is an
   O(1) operation.
*/
cwal_midsize_t cwal_string_length_bytes( cwal_string const * str );

/**
   Returns the length of the first n bytes of str in UTF8
   characters, or 0 if !str. Results are undefined if str is not
   legal UTF8. This is an O(N) operation.

   Note that an embedded NUL byte before (str+n) is counted as a
   byte!
*/
cwal_midsize_t cwal_strlen_utf8( char const * str, cwal_midsize_t n );

/**
   Functionally equivalent to strlen(3) except that if !str it
   returns 0 instead of crashing, and it returns cwal_size_t,
   which very well may not be the same size as size_t.
*/
cwal_midsize_t cwal_strlen( char const * str );

/**
   Equivalent to:

   cwal_strlen_utf8(cwal_string_cstr(str),cwal_string_length_bytes(str))

   Unless str is known to be an ASCII string, in which case it is an
   O(1) operation.

   Returns 0 if !str.
*/
cwal_midsize_t cwal_string_length_utf8( cwal_string const * str );

/**
   If str is composed solely of ASCII characters (in the range
   (0,127), this returns true, else false. While normally of little
   significance, some common algorithms can be sped up notably if
   their input is guaranteed to have only 1 byte per character.

   This immutable flag is set when a string is created.

   Returns 0 if str is NULL or if it is the built-in empty string
   value. Most algorithms avoid all work if the length is zero, so its
   ASCII-ness in that case is irrelevant (and debatable).
*/
bool cwal_string_is_ascii( cwal_string const * str );

/**
   Returns the upper-cased form of the given utf8 character,
   or ch if it doesn't know what to do.

   The mappings cover all the one-to-one mappings defined by
   Unicode:

   https://www.unicode.org/faq/casemap_charprop.html
   ftp://ftp.unicode.org/Public/3.0-Update/UnicodeData-3.0.0.html
   ftp://ftp.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt

   None of the "special cases" are covered.

   @see cwal_utf8_char_tolower()
*/
int cwal_utf8_char_toupper( int ch );

/**
   Returns the lower-cased form of the given utf8 character,
   or ch if it doesn't know what to do.

   @see cwal_utf8_char_toupper()
*/
int cwal_utf8_char_tolower( int ch );
    
/**
   Reads a single UTF-8 character from an input string and returns the
   unicode value.

   zBegin is the start of the string. zTerm points to the logical
   EOF (one-after-the-end). zBegin _must_ be less than zTerm.

   It writes a pointer to the next unread byte back into *pzNext.
   When looping, that value should be the next position passed to
   this function (see the example below).
       
   Notes On Invalid UTF-8:
       
   - This routine never allows a 7-bit character (0x00 through
   0x7f) to be encoded as a multi-byte character.  Any multi-byte
   character that attempts to encode a value between 0x00 and 0x7f
   is rendered as 0xfffd.
       
   - This routine never allows a UTF16 surrogate value to be
   encoded.  If a multi-byte character attempts to encode a value
   between 0xd800 and 0xe000 then it is rendered as 0xfffd.
       
   - Bytes in the range of 0x80 through 0xbf which occur as the first
   byte of a character are interpreted as single-byte characters and
   rendered as themselves even though they are technically invalid
   characters. (That can be considered a bug in the context of cwal!)
       
   - This routine accepts an infinite number of different UTF8
   encodings for unicode values 0x80 and greater.  It does not change
   over-length encodings to 0xfffd as some systems recommend. (That
   can be considered a bug in the context of cwal!)

   - An embedded NUL byte before zTerm is counted as a length-1
   character.

   Credits: the implementation and most of the docs were stolen from
   the public domain sqlite3 source tree.

   Example usage:

   @code
   char unsigned const * pos = inputString;
   char unsigned const * end = pos + inputStringLength;
   char unsigned const * next = 0;
   unsigned int ch;
   for( ; (pos < end)
   && (ch=cwal_utf8_read_char(pos, end, &next));
   pos = next ){
   // do something with ch...
   // but note that 0 is technically a legal value
   // for ch for generic purposes (but not for most practical
   // purposes). The byte-length of the character is (next-pos).
   }
   @endcode

   If zBegin>=zTerm, it returns 0 and *pzNext is set to zEnd.  Note
   that that result is indistinguishable from a NUL-terminated,
   zero-length string (i.e. containing only 1 byte: a NUL), but it's
   as good as we can get for such inputs. The moral is: check the
   input range before starting and do whatever is right for your use
   case.

   @see cwal_utf8_read_char1()
*/
unsigned int cwal_utf8_read_char( const unsigned char *zBegin,
                                  const unsigned char *zTerm,
                                  const unsigned char **pzNext);

/**
   A variant of cwal_utf8_read_char() which assumes that the input
   string is NUL-terminated UTF8. It returns the unicode value of the
   next UTF8 character and assigns *zIn to the first byte following
   that character. For more information about the return value, see
   cwal_utf8_read_char().

   @see cwal_utf8_read_char()
*/
unsigned int cwal_utf8_read_char1(const unsigned char **zIn);

/**
   Given UTF8 character value c, this calculates its length, in bytes,
   writes that many bytes to output, and returns that length. If the
   calculated size is >length then -1 is returned (which cannot happen
   if length>=4). If !output then only the UTF8 length of c is
   calculated and returned (and the length argument is ignored).

   Returns -1 if c is not a valid UTF8 character. The most bytes it
   will ever write to *output is four, so an output buffer of four
   bytes is sufficient for all encoding cases.

   Note that it does not NUL-terminate the output unless the character
   is incidentally a NUL byte. That is the only case in which it
   NUL-terminates the output.

   FIXME: return 1 if !c: a NUL byte has a length of 1. Make sure s2
   callers accommodate this first. Doh, we can't do that if (output)
   is optional, or the caller would not know when to stop!

   FIXME: remove the length param and require at least 4 bytes.

   FIXME?: return -1 for the various "invalid" characters: 0xfffd
   (?undocumented return value of cwal_utf8_read_char()?).

   @see cwal_utf8_char_at()
*/
int cwal_utf8_char_to_cstr(unsigned int c, unsigned char *output, cwal_size_t length);

/**
   Searches the UTF8-encoded string starting at `b` for the `index`'th
   UTF8 character. The string in the half-open range [`b`,`e`) must be
   well-formed UTF8 and `b` must start at a character boundary, not
   mid-character. If found, 0 is returned and `*unicode` (if `unicode`
   is not NULL) is assigned to the code point. If index is out of
   range or invalid UTF8 is traversed, CWAL_RC_RANGE is returned and
   `unicode` is not modified.

   @see cwal_utf8_char_to_cstr()
*/
int cwal_utf8_char_at( unsigned char const * b,
                       unsigned char const * e,
                       cwal_size_t index,
                       unsigned int * unicode );

/**
   Equivalent to cwal_value_unref( e, cwal_string_value(v) ),
   where e is s's owning cwal_engine.
*/
int cwal_string_unref(cwal_string * s);

/**
   If cwal_value_is_string(val) then this function assigns *str to the
   contents of the string. str may be NULL, in which case this function
   functions like cwal_value_is_string() but returns 0 on success.

   Returns 0 if val is-a string, else non-0, in which case *str is not
   modified.

   The bytes are owned by the given value and may be invalidated in any of
   the following ways:

   - The value is cleaned up or freed.

   - An array or object containing the value peforms a re-allocation
   (it shrinks or grows).

   And thus the bytes should be consumed before any further operations
   on val or any container which holds it.

   Note that this routine does not convert non-String values to their
   string representations. (Adding that ability would add more
   overhead to every cwal_value instance.)

   In practice this is never used by clients - see
   cwal_value_get_string().
*/
int cwal_value_fetch_string( cwal_value const * val, cwal_string ** dest );

/**
   Simplified form of cwal_value_fetch_string(). Returns NULL if val
   is-not-a string value.
*/
cwal_string * cwal_value_get_string( cwal_value const * val );

/**
   Convenience function which returns the string bytes of the
   given value if it is-a string or a buffer, otherwise it returns
   NULL. Note that this does no conversion of other types to
   strings, and returns NULL for them.

   The second argument, if not NULL, is set to the length of the
   string or buffer, in bytes (not UTF8 characters). For buffers, this
   value corresponds to their "used" property. If the 2nd argument is
   NULL, it is ignored.

   Using this for buffer values "might" (depending on the contents of
   the buffer and the intended use of the returned bytes) lead to
   undefined behaviour if the returned string is expected to contain
   valid string data (buffers can contain anything).

   The returned bytes are owned by the underlying value. In the case
   of strings their address and contents remain constant for the life
   of the string value. For buffers the contents and address may
   change at any time, so it is illegal to use the returned bytes if
   there is any chance that the buffer which owns/owned them has been
   modified _in any way_ since calling this.

   As a special case, a completely empty buffer value, with no
   buffered memory, will return 0 here and len (if not NULL) will be
   set to 0. An empty string, on the other hand, will return ""
   and set len (if not NULL) to 0.
*/
char const * cwal_value_get_cstr( cwal_value const * val, cwal_size_t * len );


/**
   Allocates a new "array" value and transfers ownership of it to the
   caller. It must eventually be destroyed, by the caller or its
   owning container, by passing it to cwal_value_unref().

   Returns NULL on allocation error.

   Post-conditions: cwal_value_is_array(value) will return true.

   @see cwal_new_object_value()
   @see cwal_new_VALUE()
*/
cwal_value * cwal_new_array_value( cwal_engine * e );

/**
   Convenience form of cwal_new_array_value() which returns its
   result as an array handle.

   Postconditions: cwal_array_value(result) is the value which would
   have been returned had the client called cwal_new_array_value()
   instead of this function.
*/
cwal_array * cwal_new_array(cwal_engine *e);

/**
   Equivalent to cwal_value_unref( cwal_array_value(v) ).
*/
int cwal_array_unref(cwal_array *a);

/**
   Identical to cwal_value_fetch_object(), but works on array values.

   In practice this is never used by clients - see
   cwal_value_get_array().

   @see cwal_value_get_array()
   @see cwal_value_array_part()
*/
int cwal_value_fetch_array( cwal_value const * val, cwal_array ** ar);

/**
   Simplified form of cwal_value_fetch_array(). Returns NULL if val
   is-not-a array value.
*/
cwal_array * cwal_value_get_array( cwal_value const * v );

/**
   The inverse of cwal_value_get_array().
*/
cwal_value * cwal_array_value(cwal_array const * s);

/**
   Sets the given index of the given array to the given value
   (which may be NULL).

   If ar already has an item at that index then it is cleaned up and
   freed before inserting the new item.

   ar is expanded, if needed, to be able to hold at least (ndx+1)
   items, and any new entries created by that expansion are empty
   (NULL values).

   On success, 0 is returned and ownership of v is transfered to ar.

   If resolvePropref is true, v is not NULL, and the list already
   contains an entry at the given index, then this routine resolves
   cwal_propref-typed values, instead redirecting v to the propref's
   proxies container. It returns CWAL_RC_CYCLES_DETECTED if the value
   at the given index is a cwal_propref and cycles were detected in
   resolution of the propref.

   If resolvePropref is false then any propref-type values are
   returned as-is.

   Note that it's possible for a propref redirection to return any
   non-0 code.

   If v is NULL, the entry is cleared even if it's a propref.
  
   On error ownership of v is NOT modified, and the caller may still
   need to clean it up. For example, the following code will introduce
   a leak if this function fails:

   Fails with CWAL_RC_LOCKED if the array is currently locked against
   modification (e.g. while the list is being sorted).

   If it returns CWAL_RC_CYCLES_DETECTED, the value at the given index
   is a cwal_propref and cycles were detected in resolution of the
   propref.

   @code
   cwal_array_append( myArray, cwal_new_integer(42) );
   @endcode

   Because the value created by cwal_new_integer() has no owner
   and is not cleaned up. The "more correct" way to do this is:

   @code
   cwal_value * v = cwal_new_integer(42);
   cwal_ref(v);
   int rc = cwal_array_append( myArray, v );
   cwal_unref(v);
   if(rc) { handle error; }
   @endcode
*/
int cwal_array_set_v2( cwal_array * const ar, cwal_midsize_t ndx, cwal_value * const v,
                       bool resolvePropref );

/**
   Equivalent to calling cwal_array_set() with a true final argument.
*/
int cwal_array_set( cwal_array * const ar, cwal_midsize_t ndx, cwal_value * const v );

/**
   Ensures that ar has allocated space for at least the given
   number of entries. This never shrinks the array and never
   changes its logical size, but may pre-allocate space in the
   array for storing new (as-yet-unassigned) values.

   Returns 0 on success, or non-zero on error:

   - If ar is NULL: CWAL_RC_MISUSE

   - If allocation fails: CWAL_RC_OOM
*/
int cwal_array_reserve( cwal_array * ar, cwal_midsize_t size );

/**
   Sets the length of the given array to n, allocating space if
   needed (as for cwal_array_reserve()), and unreferencing
   truncated objects. New entries will have NULL values.

   It does not free the underlying array storage but may free
   objects removed from the array via shrinking. i.e. this is not
   guaranteed to free all memory associated with ar's storage.

   Fails (as of 20191211) with CWAL_RC_IS_VISITING_LIST if called
   while traversing over the elements using one of the various
   list-traversal/sort APIs. i.e. the length may not be modified
   while a sort or list iteration is underway. (It "could" be made to
   work for iteration, but resizing while sorting would be
   catastrophic.)

   Fails with CWAL_RC_LOCKED if the list is currently locked (e.g.
   being sorted).
*/
int cwal_array_length_set( cwal_array * ar, cwal_midsize_t n );

/**
   Simplified form of cwal_array_length_fetch() which returns 0 if ar
   is NULL.
*/
cwal_midsize_t cwal_array_length_get( cwal_array const * ar );

/**
   Returns the given array's current reserved capacity.
*/
cwal_midsize_t cwal_array_capacity_get( cwal_array const * ar );

/**
   If ar is not NULL, sets *v (if v is not NULL) to the length of the array
   and returns 0. Returns CWAL_RC_MISUSE if ar is NULL.
*/
int cwal_array_length_fetch( cwal_array const * ar, cwal_midsize_t* v );

/**
   Simplified form of cwal_array_fetch_v2() which returns NULL if
   ar is NULL, pos is out of bounds or if ar has no element at that
   position.

   @see cwal_array_get()
*/
cwal_value * cwal_array_get_v2( cwal_array const * ar, cwal_midsize_t pos,
                                bool resolvePropref);

/**
   Equivalent to cwal_array_get_v2() with a true final argument.
*/
cwal_value * cwal_array_get( cwal_array const * ar, cwal_midsize_t pos );

/**
   "Takes" the given index entry out of the array and transfers
   ownership to the caller. Its refcount IS decremented by this,
   but using cwal_value_unhand() instead of cwal_value_unref(),
   so it won't be destroyed as a result of this call but may
   once again be a temporary.

   This routine does not resolve cwal_propref-typed values, taking
   the value at the given position as-is.
*/
cwal_value * cwal_array_take( cwal_array * const ar, cwal_midsize_t pos );

/**
   If ar is at least (pos+1) entries long then `*v` (if v is not NULL)
   is assigned to the value at that position (which may be NULL).

   Ownership of the `*v` return value is unchanged by this
   call. (The containing array may share ownership of the value
   with other containers.)

   If pos is out of range, non-0 is returned and `*v` is not
   modified.

   If v is NULL then this function returns 0 if pos is in bounds,
   but does not otherwise return a value to the caller.

   If resolvePropref is true then any properef-type values in the
   array will be resolved for purposes of the result `*v`, else they
   will be fetched as-is. Note that if it is true then `*v` can
   conceivably be a newly-allocated value.

   Returns 0 on success. It returns CWAL_RC_RANGE if pos is out of
   range and may propagate an error via propref resolution.

   In practice this is never used by clients - see
   cwal_array_get(). That said, the addition of proprefs add potential
   error cases which did not exist before, making this function more
   attractive than it has historically been.
*/
int cwal_array_fetch_v2( cwal_array const * ar, cwal_midsize_t pos,
                         cwal_value ** v, bool resolvePropref );

/**
   Equivalent to calling cwal_array_fetch_v2() with a true final
   argument.
*/
int cwal_array_fetch( cwal_array const * ar, cwal_midsize_t pos, cwal_value ** v );

/**
   Searches for a value in an Array.

   If ar contains v or a value which compares equivalent to v
   using cwal_value_compare(), it returns 0 and sets *index to the
   index the value is found at if index is not NULL.  Returns
   CWAL_RC_NOT_FOUND if no entry is found, CWAL_RC_MISUSE if ar is
   NULL. v may be NULL, in which case it searches for the first
   index in the array with no value in it (it never calls
   cwal_value_compare() in that case).

   If strictComparison is true (non-0), values are only compared if
   they have the same type. e.g. an integer will never match a double
   if this flag is true, but they may match if it is false. If v is
   NULL, this flag has no effect, as only NULL will compare equivalent
   to NULL.

   The cwal_value_compare() (if any) is done with v as the left-hand
   argument and the array's entry on the right.

   Notes regarding cwal_propref-type array values:

   If an array's entry is a cwal_propref value, its resolved value is
   used for comparison purposes, note that that may result any
   different error codes, most significantly CWAL_RC_CYCLES_DETECTED
   if propref resolution is cyclic. v is never evaluated to determine
   whether it is a propref. If v's pointer value matches a value, it
   automatically compares equivalent, and in that case a
   cwal_propref-typed v will match the same propref without any
   propref resolution taking place.
*/
int cwal_array_index_of( cwal_array const * ar, cwal_value const * v,
                         cwal_size_t * index, bool strictComparison );

/**
   Appends the given value to the given array. On error, ownership
   of v is not modified. Ownership of ar is never changed by this
   function. v may be NULL.

   This is functionally equivalent to
   cwal_array_set(ar,cwal_array_length_get(ar),v), but this
   implementation has slightly different array-preallocation policy
   (it grows more eagerly).
   
   Returns 0 on success, non-zero on error. Error cases include:

   - ar is NULL: CWAL_RC_MISUSE

   - Array cannot be expanded to hold enough elements: CWAL_RC_OOM.

   - Appending would cause a numeric overlow in the array's size:
   CWAL_RC_RANGE.  (However, you'll get an CWAL_RC_OOM long before
   that happens!)

   - CWAL_RC_LOCKED if the array is currently locked against
   modification (e.g. while the list is being sorted).

   On error ownership of v is NOT modified, and the caller may still
   need to clean it up. See cwal_array_set() for the details.
*/
int cwal_array_append( cwal_array * const ar, cwal_value * const v );

/**
   The opposite of cwal_array_append(), this prepends a value
   (which may be NULL) to the start of the array.

   This is a relatively expensive operations, as existing entries
   in the array all have to be moved to the right.

   Returns 0 on success, non-0 on error (see cwal_array_append()).
*/
int cwal_array_prepend( cwal_array * const ar, cwal_value * const v );

/**
   "Shifts" the first item from the given array and assigns
   it to *rv (if rv is not NULL).

   Returns 0 on success, CWAL_RC_MISUSE if !ar, and CWAL_RC_RANGE
   if ar is empty. On error, *rv is not modified.

   The array's reference to *rv is removed but if rv is not NULL,
   then then *rv is not immediately destroyed if its refcount goes
   to zero. Instead, it is re-probated in its owning scope. If rv
   is NULL then the shifted value may be reaped immediately
   (before this function returns). i.e. the effect is as if it has
   been cwal_value_unhand()'d, as opposed to cwal_value_unref()'d.
*/
int cwal_array_shift( cwal_array * ar, cwal_value **rv );

/**
   Copies a number of elements from ar into another array.  If
   `!*dest` then this function creates a new array and, on success,
   updates `*dest` to point to that array. On error `*dest`'s
   ownership is not modified.

   Copies (at most) 'count' elements starting at the given
   offset. If 0==count then it copies until the end of the array.
   (This is arguably a design wart: see cwal_array_copy_range2()
   for an implementation with different semantics.)

   If count is too large for the array, count is trimmed to fit
   within bounds.

   If ar is empty or offset/count are out of range, it still
   creates a new array on success, to simplify/unify client-side
   usage.
       
   Returns 0 on success, CWAL_RC_MISUSE if !ar, !dest, or
   (ar==*dest). Returns CWAL_RC_OOM for any number of potential malloc
   errors. Returns (as of 20191212) CWAL_RC_LOCKED if either ar or
   a non-NULL *dest are currently locked.

   @see cwal_array_copy_range2()
*/
int cwal_array_copy_range( cwal_array * ar, cwal_size_t offset,
                           cwal_size_t count,
                           cwal_array **dest );

/**
   This works exactly like cwal_array_copy_range() except for the
   semantics of the 2nd and 3rd parameters:

   - If the offset is negative, it is counted from the end of the
     array. If the absolute value of a negative offset is longer than
     the array, copying starts from the beginning.

   - If the count is negative, it means to copy until the end of the
     array. A count of 0 means not to copy anything.

     See cwal_array_copy_range() for the remaining details.
 */
int cwal_array_copy_range2( cwal_array * ar, cwal_int_t offset,
                            cwal_int_t count, cwal_array **dest );

    
/**
   Clears the contents of the array, optionally releasing the
   underlying list as well (if freeList is true). If the list is
   not released it is available for re-use on subsequent
   insertions. If freeProps is true then key/value properties are
   also cleared from ar, else they are kept intact.
*/
void cwal_array_clear( cwal_array * ar, bool freeList, bool freeProps );
    
/**
   A callback type for "value visitors," intended for use
   with cwal_array_visit() and friends.

   The 1st parameter is the value being visited (it MAY BE NULL for
   containers which may hold NULL values, e.g. arrays or tuples). The
   2nd is the state parameter passed to the visiting function
   (e.g. cwal_array_visit()).

   Implementations MUST NOT unref v unless they explicitly ref it
   first. The container which calls this callback holds at least
   one reference to each value passed here.

   20191211: since when do we pass on NULLs in cwal_array_visit()?
*/
typedef int (*cwal_value_visitor_f)( cwal_value * v, void * state );
    
/**
   A callback type for "array visitors," for use with cwal_array_visit2().

   The first 3 parameters specify the array, value, and index of that
   value in the array. The value may be NULL.
       
   The state parameter is that passed to cwal_array_visit2() (or
   equivalent).

   Implementations MUST NOT unref v unless the also ref it first, but
   may indirectly do so by re-assigning that entry in a.

   Note that changing a list's size while visiting is not allowed, and
   attemping to do so will trigger an error.
*/
typedef int (*cwal_array_visitor_f)( cwal_array * a, cwal_value * v, cwal_size_t index, void * state );

/**
   For each entry in the given array, f(theValue,state) is called.  If
   it returns non-0, looping stops and that value is returned to the
   caller.
       
   When traversing containers (this applies to Objects as well), they
   have a flag set which marks them as "being visited" for the
   moment. Prior to 20191211, concurrent visits were never allowed,
   but they now are, with some restrictions. Some APIs temporarily
   lock lists against traversal, which triggers the result code
   CWAL_RC_LOCKED from other APIs (such as this one).

   Returns 0 on success (note that having no entries is not an
   error).

   Entries which are cwal_propref values ARE NOT resolved by this
   routine. (They arguably should be but that would be inconsistent
   with object property visitation.) See cwal_array_visit_v2() for an
   option which permits that.

   Minor achtung: entries in the array which have no Value in them are
   passed to the visitor as NULL. Script-side visitors may need to
   check for that and pass on cwal_value_undefined() or
   cwal_value_null() in its place (or skip them altogether). They are
   passed to the visitor so that the number of visits matches the
   indexes of the array, which simplifies some script code compared to
   this routine skipping NULL entries. (BTW, it _did_ skip over NULLs
   until 20160225, at which point the older, arguable behaviour was
   noticed and changed. It was likely an artifact of keeping th1ish
   happy.)

   @see cwal_array_visit()
   @see cwal_array_visit_v2()
   @see cwal_array_visit2_v2()
*/
int cwal_array_visit( cwal_array * const a, cwal_value_visitor_f const f, void * const state );

/**
   If the 2nd argument is false then this works exactly like
   cwal_array_visit().  If it's true, any list entries are
   cwal_propref values cause their resolved value (which may be NULL)
   to be visited instead.

   @see cwal_array_visit()
   @see cwal_array_visit2()
   @see cwal_array_visit2_v2()
*/
int cwal_array_visit_v2( cwal_array * const a, bool resolvePropref,
                         cwal_value_visitor_f const f, void * const state );

/**
   An alternative form of cwal_array_visit() which takes a
   different kind of callback. See cwal_array_visit() and
   cwal_array_visitor_f() for the semantics.
*/
int cwal_array_visit2( cwal_array * const a, cwal_array_visitor_f const f, void * const state );

/**
   If the 2nd argument is false then this works exactly like
   cwal_array_visit2().  If it's true, any list entries are
   cwal_propref values cause their resolved value (which may be NULL)
   to be visited instead.
*/
int cwal_array_visit2_v2( cwal_array * const a, bool resolvePropref,
                          cwal_array_visitor_f const f, void * const state );

/**
   Runs a qsort(2) on ar, using the given comparison function. The
   values passed to the comp routine will be a pointer to either
   (cwal_value const *) or NULL (empty array elements are NULL
   unless the client populates them with something else).

   Returns 0 on success, and the error conditions are quite
   limited:
       
   This function will fail with CWAL_RC_MISUSE if either argument
   is NULL.

   Fails with CWAL_RC_IS_VISITING_LIST if called while traversing over the
   elements using one of the various traversal APIs. (This was result
   code CWAL_RC_IS_VISITING prior to 20191211.)

   Fails with CWAL_RC_LOCKED if the list is currently locked.

   Achtung: this routine does not honor resolving of propref-type
   values for comparison purposes. cwal_array_sort_v2() can.

   Sidebar: this routine's 2nd argument is less useful than initially
   envisioned because there's not much one can really do with a stateless
   comparison function. cwal_array_sort_stateful() is far more useful.

   @see cwal_compare_value_void()
   @see cwal_compare_value_reverse_void()
   @see cwal_array_reverse()
*/
int cwal_array_sort( cwal_array * const ar, int(*comp)(void const *, void const *) );

/**
   This cwal_array_sort_stateful() variant uses
   cwal_value_compare_v2() to perform the sorting. If resolvePropref
   is true then propref-type values are resolved during sorting.

   Returns 0 on success. On error it may return any codes documented
   for cwal_array_sort() or it may propagate an error via propref
   resolution.

   Potential fixme: proprefs are resolved on every comparison. We
   "should" resolve all values to a temp array and sort that (without
   propref resolution), then swap the array contents. Because proprefs
   can potentially trigger arbitrary code.
*/
int cwal_array_sort_v2( cwal_array * const ar, bool resolvePropref );

/**
   Reverses the order of all elements in the array. Returns 0 on success,
   non-zero on error:

   - CWAL_RC_MISUSE if !ar.

   While this is technically legal during traversal of a list, the
   results may cause undue confusion.

   @see cwal_array_sort()
*/
int cwal_array_reverse( cwal_array * ar );
    
/**
   A comparison function for use with cwal_array_sort() which
   requires that lhs and rhs be either NULL or valid cwal_value
   pointers. It simply casts the arguments and returns
   the result of passing them to cwal_value_compare().

   @see cwal_array_sort()
*/
int cwal_compare_value_void( void const * lhs, void const * rhs );

/**
   A comparison function for use with cwal_array_sort() which
   requires that lhs and rhs be either NULL or valid cwal_value
   pointers. It simply casts the arguments, calls
   cwal_value_compare(), and returns that result, negated if it is
   not 0.
*/
int cwal_compare_value_reverse_void( void const * lhs, void const * rhs );

/**
   A cwal_value comparison function intended for use with
   cwal_array_sort_stateful(). Implementations must compare the given
   lhs/rhs values and return an integer using memcmp() semantics.

   The lhs and rhs value are the left/right values to compare (either
   or both _MAY_ be NULL). These arguments "should" be const but for
   the intended uses of this callback it would be impossible for the
   client to ensure that the constness is not (by necessity) cast
   away. Results are undefined if any sort-relevant state of lhs or
   rhs is modified during the sorting process.

   The state argument is provided by the caller of
   cwal_array_sort_stateful().

   Implementations must set *errCode to 0 on success and non-0 (very
   preferably a CWAL_RC_xxx value) on error. This can be used to
   propagate errors back through the sorting process (e.g.
   script-engine syntax errors or interrupt handling errors).  The
   cwal API guarantees that errCode will not be NULL if this callback
   is called from within the cwal API.

   If a comparison function modifies sort-relevant state while a sort
   is underway, sorting behavior is undefined. It will sort, but very
   possibly not as intended. The one case where this is known to be
   possible is that an array entry may be a propref alias. During
   sorting, that alias gets resolved (currently on each comparison).
   Depending on where that alias points to, it's potentially possible
   that a misbehaved comparison function can swap out the value the
   alias resolves to in mid-sort. In such cases, sorting behavior is
   unspecified: it may sort in any order in that case. One potential
   FIXME in the underlying sorting bits is to resolve all proprefs up
   front into a temp list and sort on _that_. That would eliminate the
   mid-sort unspecified behavior but the end effect would be the same:
   after the sort, the alias would resolve to its replaced value,
   potentially leaving it out of sorted order. So it would just trade
   one unit of confusion for another and would cost memory for the
   temp list. Living with the above-described unspecified sorting
   behavior seems like the lesser evil.
*/
typedef int (*cwal_value_stateful_compare_f)( cwal_value * lhs, cwal_value * rhs,
                                              void * state, int * errCode );

/**
   A cwal_value_stateful_compare_f() implementation which uses
   cwal_value_compare_v2() to compare its value arguments. If the
   state argument is NULL then propref resolution is enabled, else any
   non-NULL value means to enable propref resolution.
*/
int cwal_array_stateful_cmp_v2( cwal_value * lhs, cwal_value * rhs, void * state,
                                int * errCode );


/**
   An array sort variant which allows the client to provide a
   stateful comparison operation. The intended use for this is in
   providing script-side callback functions for the sorting, where
   cmp would be a native wrapper around a cwal_function (the state
   param) and would call that function to perform the comparison.

   The exact semantics of the state parameter depend entirely on
   the cmp implementation - this function simply passes the state
   on to the comparison function.

   Note that this routine does _not_ resolve propref values, nor can
   it be made to without some significant surgery. If the comparison
   function needs to resolve values, it may do so, but must be aware
   that propref resolution may create new values, so care must be
   taken with references to resolved values. However, the
   cwal_array_stateful_cmp_v2 is provided as a cmp implementation
   which can resolve proprefs.

   Returns 0 on success, or CWAL_RC_MISUSE if either !ar or !cmp.  If
   any sort-internal call to cmp() sets its final parameter (error
   code pointer) to a non-0 value, sorting is aborted and that error
   code is returned. This function returns 0 without side-effects if
   the length of the given array (see cwal_array_length_get()) is 0 or
   1.

   Fails with CWAL_RC_IS_VISITING_LIST if called while traversing over
   the elements using one of the various traversal APIs: it is illegal
   to sort an array while it is being iterated over. Likewise, it is
   illegal to iterate over an array from a comparison function while
   sorting is underway (because the results would be highly
   unpredictable). (This was code CWAL_RC_IS_VISITING prior to
   20191211.)

   Fails with CWAL_RC_LOCKED if the list is currently locked.

   If the sorting process does not return an error but the underlying
   cwal engine has, after sorting is complete, an exception awaiting
   propagation, CWAL_RC_EXCEPTION is returned. This is largely
   historical behaviour, from before the time when
   cwal_value_stateful_compare_f was capable of returning error codes.
   It may, in some unusual cases, be necessary for the client to
   ensure that no exception is propagating before calling this (by
   calling cwal_exception_set() with a NULL exception). In practice,
   sorting cannot be triggered during exception propagation, so this
   is really a non-issue unless the client is doing some truly odd
   error handling.

   BUG: array sorting does not currently honor resolving propref-type
   values and getting it to do so will require some significant
   reworking.
*/
int cwal_array_sort_stateful( cwal_array * const ar,
                              cwal_value_stateful_compare_f cmp,
                              void * state );

/**
   A wrapper around cwal_array_sort_stateful() which calls the given
   comparison function to perform the sorting comparisons.  The
   function must accept two (cwal_value*) arguments, compare them
   using whatever heuristic it prefers, and "return" (to script-space)
   an numeric value with memcmp() semantics.  Note that
   cwal_new_integer() does not allocate for the values (-1, 0, 1), so
   implementations should ideally use those specific values for their
   returns. Floating-point comparison results are permitted, as in
   JavaScript.

   The self parameter specifies the "this" object for the function
   call. It may be NULL, in which case cwal_function_value(cmp)
   is used.

   The final argument specifies whether or not it should resolve
   propref-type values before comparing them.

   Fails with CWAL_RC_IS_VISITING_LIST if called while traversing over
   the elements using one of the various traversal APIs. (This was
   result code CWAL_RC_IS_VISITING prior to 20191211.)

   Fails with CWAL_RC_LOCKED if the list is currently locked.
*/
int cwal_array_sort_func( cwal_array * const ar, cwal_value * const self,
                          cwal_function * const cmp, bool resolvePropref);
    
/**
   Identical to cwal_new_array_value() except that it creates
   an Object.

   @see cwal_new_VALUE()
*/
cwal_value * cwal_new_object_value( cwal_engine * e );

/**
   Identical to cwal_new_object_value() except that it returns
   the object handle which can converted back to its value
   handle using cwal_value_get_object().
*/
cwal_object * cwal_new_object(cwal_engine *e);

/**
   Equivalent to cwal_value_unref( e, cwal_object_value(v) ).
*/
int cwal_object_unref(cwal_object *v);

/**
   If cwal_value_is_object(val) then this function assigns *obj to the underlying
   object value and returns 0, otherwise non-0 is returned and *obj is not modified.

   obj may be NULL, in which case this function works like cwal_value_is_object()
   but with inverse return value semantics (0==success) (and it's a few
   CPU cycles slower).

   The *obj pointer is owned by val, and will be invalidated when val
   is cleaned up.

   Achtung: for best results, ALWAYS pass a pointer to NULL as the
   second argument, e.g.:

   @code
   cwal_object * obj = NULL;
   int rc = cwal_value_fetch_object( val, &obj );

   // Or, more simply:
   obj = cwal_value_get_object( val );
   @endcode

   In practice this is never used by clients - see
   cwal_value_get_object().

   @see cwal_value_get_object()
*/
int cwal_value_fetch_object( cwal_value const * val, cwal_object ** ar);

/**
   Simplified form of cwal_value_fetch_object(). Returns NULL if val
   is-not-a object value.
*/
cwal_object * cwal_value_get_object( cwal_value const * v );

/**
   The Object form of cwal_string_value(). See that function
   for full details.
*/
cwal_value * cwal_object_value(cwal_object const * s);

/**
   Fetches a property from a child (or [great-]*grand-child) object.

   obj is the object to search.

   path is a delimited string, where the delimiter is the given
   separator character.

   This function searches for the given path, starting at the given object
   and traversing its properties as the path specifies. If a given part of the
   path is not found, then this function fails with CWAL_RC_NOT_FOUND.

   If it finds the given path, it returns the value by assiging *tgt
   to it.  If tgt is NULL then this function has no side-effects but
   will return 0 if the given path is found within the object, so it can be used
   to test for existence without fetching it.
    
   Returns 0 if it finds an entry, CWAL_RC_NOT_FOUND if it finds
   no item, and any other non-zero error code on a "real" error. Errors include:

   - obj or path are NULL: CWAL_RC_MISUSE.
    
   - separator is 0, or path is an empty string or contains only
   separator characters: CWAL_RC_RANGE.

   - There is an upper limit on how long a single path component may
   be (some "reasonable" internal size), and CWAL_RC_RANGE is
   returned if that length is violated.

    
   Limitations:

   - It has no way to fetch data from arrays this way. i could
   imagine, e.g. a path of "subobj.subArray.0" for
   subobj.subArray[0], or "0.3.1" for [0][3][1]. But i'm too
   lazy/tired to add this.

   Example usage:
    

   Assume we have a JSON structure which abstractly looks like:

   @code
   {"subobj":{"subsubobj":{"myValue":[1,2,3]}}}
   @endcode

   Out goal is to get the value of myValue. We can do that with:

   @code
   cwal_value * v = NULL;
   int rc = cwal_prop_fetch_sub( object, &v, "subobj.subsubobj.myValue", '.' );
   @endcode

   Note that because keys in JSON may legally contain a '.', the
   separator must be specified by the caller. e.g. the path
   "subobj/subsubobj/myValue" with separator='/' is equivalent the
   path "subobj.subsubobj.myValue" with separator='.'. The value of 0
   is not legal as a separator character because we cannot
   distinguish that use from the real end-of-string without requiring
   the caller to also pass in the length of the string.
   
   Multiple successive separators in the list are collapsed into a
   single separator for parsing purposes. e.g. the path "a...b...c"
   (separator='.') is equivalent to "a.b.c".

   TODO: change last parameter to an int to support non-ASCII
   separators. s2's string.split() code is close to what we need here.

   @see cwal_prop_get_sub()
   @see cwal_prop_get_sub2()
*/
int cwal_prop_fetch_sub( cwal_value * obj, cwal_value ** tgt, char const * path,
                         char separator );

/**
   Similar to cwal_prop_fetch_sub(), but derives the path separator
   character from the first byte of the path argument. e.g. the
   following arg equivalent:

   @code
   cwal_prop_fetch_sub( obj, &tgt, "foo.bar.baz", '.' );
   cwal_prop_fetch_sub2( obj, &tgt, ".foo.bar.baz" );
   @endcode
*/
int cwal_prop_fetch_sub2( cwal_value * obj, cwal_value ** tgt, char const * path );

/**
   Convenience form of cwal_prop_fetch_sub() which returns NULL if the given
   item is not found.
*/
cwal_value * cwal_prop_get_sub( cwal_value * obj, char const * path, char sep );

/**
   Convenience form of cwal_prop_fetch_sub2() which returns NULL if the given
   item is not found.
*/
cwal_value * cwal_prop_get_sub2( cwal_value * obj, char const * path );

    
/**
   Returns v's reference count, or 0 if !v.
*/
cwal_refcount_t cwal_value_refcount( cwal_value const * v );

/**
   Typedef for generic visitor functions for traversing Objects.  The
   first argument holds they key/value pair and the second holds any
   state passed to cwal_props_visit_kvp() (and friends).

   If it returns non-0 the visit loop stops and that code is returned
   to the caller.

   Implementations MUST NOT unref the key/value parts of kvp. They are
   owned by (or shared with) the kvp object. Similarly, visitors _must
   not_ hold a ref to the given KVP. It may be invalidated _at any
   time_ after the visitor returns.

   TODO (20160205): consider making kvp non-const, as clients could
   hypothetically safely use cwal_kvp_value_set() on them during
   visitation.
*/
typedef int (*cwal_kvp_visitor_f)( cwal_kvp const * kvp, void * state );

/**
   Returns the key associated with the given key/value pair,
   or NULL if !kvp. The memory is owned by the object which contains
   the key/value pair, and may be invalidated by any modifications
   to that object.
*/
cwal_value * cwal_kvp_key( cwal_kvp const * kvp );

/**
   Returns the value associated with the given key/value pair,
   or NULL if !kvp. The memory is owned by the object which contains
   the key/value pair, and may be invalidated by any modifications
   to that object.
*/
cwal_value * cwal_kvp_value( cwal_kvp const * kvp );

/**
   Re-assigns the given kvp's value part.

   On success this adds a ref point to v and unrefs the old value,
   which may destroy the old value immediately.

   Returns 0 on success, non-0:

   - CWAL_RC_MISUSE if either argument is NULL. Values may not
   be unset by passing a NULL 2nd argument.

   Because keys are used in hashing and sorting, they may not be
   replaced, so there is no cwal_kvp_key_set() routine.

   ACHTUNG:

   1) Because this routine is not guaranteed to be able to know
   which scope is correct for v, clients should, on success,
   cwal_value_rescope() v to the scope which owns the container which
   kvp came from (which will move it up only if needed). Do not
   rescope it if this fails or it might end up being stranded in a
   higher-up scope.

   2) This does not honor any CWAL_VAR_F_CONST flag set on kvp,
   but cwal_value_kvp_set2() does.

   3) This does not honor proprefs (see cwal_new_propref()). It will
   overwrite a propref with the given value.

   @see cwal_value_kvp_set2()
*/
int cwal_kvp_value_set( cwal_kvp * const kvp, cwal_value * const v );

/**
   Similar to cwal_kvp_value_set() but honors CWAL_VAR_F_xxx flags set
   on kvp, such that it can return additional error codes:

   - CWAL_RC_CONST_VIOLATION if the CWAL_VAR_F_CONST flag is set.
   As a special case (to keep th1ish working :/), if the const flag
   is set but kvp's value == v (as in, the same C pointer, not cwal_value
   equivalence), 0 is returned.

   Unfortuntately, as this routine does not know which container kvp
   belongs to, it cannot enforce
   CWAL_CONTAINER_DISALLOW_NEW_PROPERTIES and
   CWAL_CONTAINER_DISALLOW_PROP_SET. Likewise, it cannot ensure that v
   is in the scope it needs to be in, so the caller should, for
   safety's sake, rescope v to the underlying container's scope (see
   cwal_value_rescope()).

   @see cwal_value_kvp_set()
*/
int cwal_kvp_value_set2( cwal_kvp * const kvp, cwal_value * const v );

/**
   Returns kvp's flags, or 0 if !kvp. Flags are typically
   set via cwal_var_decl() and friends.

   @see cwal_var_flags_e
*/
cwal_flags16_t cwal_kvp_flags( cwal_kvp const * kvp );

/**
   Sets kvp's flags to the given flags and returns the old
   flags. Only do this if you are absolutely certain of what
   you are doing and the side-effects it might have.

   As a special case, if flags is CWAL_VAR_F_PRESERVE then
   the old value is retained.

   @see cwal_var_flags_e
*/
cwal_flags16_t cwal_kvp_flags_set( cwal_kvp * kvp, cwal_flags16_t flags );


/**
   Clears all properties (set via the cwal_prop_set() family of
   functions) from the given container value. Returns 0 on
   success, or:

   - CWAL_RC_MISUSE if either c is NULL or its associated
   engine cannot be found (indicative of an internal error).

   - If c is not a type capable of holding properties, CWAL_RC_TYPE is
   returned.

   - CWAL_RC_DISALLOW_PROP_SET if the CWAL_CONTAINER_DISALLOW_PROP_SET
   flag is set on the value (via cwal_container_flags_set()).

   This function treats CWAL_TYPE_PROPREF values as plain values -
   it copies them as-is and does not proxy them.
*/
int cwal_props_clear( cwal_value * c );

#if 0
/* This one needs better definition... */
    
/**
   If v is of a type which can contain mutable state
   (e.g. properties or children of any sort, e.g. Array, Object,
   etc.) then all of that mutable state is cleaned up. For other
   types this is a no-op.

   This does not clear the prototype value, but clears everything
   else.

   Note that this effectively destroys Buffers, Natives, and
   Functions.

   Returns 0 on success. Errors include:

   - CWAL_RC_MISUSE: if c is NULL or its engine cannot be found.

   - CWAL_RC_TYPE: if c is not a type capable of holding
   properties.

*/
int cwal_value_clear_mutable_state( cwal_value * c );
#endif

/**
   For each property in container value c which does not have the
   CWAL_VAR_F_HIDDEN flag, f(property,state) is called. If it returns
   non-0 looping stops and that value is returned.

   Returns CWAL_RC_MISUSE if !o or !f, and 0 on success.

   Note that cwal_propref-type values are visited as-is, not resolved
   before calling the callback.

   Prior to 20200118, this routine returned CWAL_RC_CYCLES_DETECTED if
   concurrent iteration would have been triggered but that is, as of
   20191211, permitted by the API, so that condition was removed from
   this function.
*/
int cwal_props_visit_kvp( cwal_value * c, cwal_kvp_visitor_f f, void * state );

/**
   Convenience wrapper around cwal_props_visit_kvp() which visits only the
   keys. f is passed the (cwal_value*) for each property key.
*/
int cwal_props_visit_keys( cwal_value * c, cwal_value_visitor_f f, void * state );

/**
   Convenience variant of cwal_props_visit_kvp() which visits only the
   values. f is passed the (cwal_value*) for each property value.

   Note that cwal_propref-type values are visited as-is, not resolved
   before calling the callback.
*/
int cwal_props_visit_values( cwal_value * o, cwal_value_visitor_f f, void * state );

/**
   If c is a property container type and its object-level properties
   may safely be iterated over without danger of recursion then this
   function returns true (non-0), else false (0).

   As of 20191211, objects and lists may iterate multiple times
   concurrently so long as no "locking" operation which calls back
   into client-side code is underway. As of this writing, there are no
   such operations, so this function always returns true for any
   values of types which are capable of holding properties.

   @see cwal_value_is_iterating_list()
   @see cwal_value_may_iterate_list()
*/
bool cwal_value_may_iterate( cwal_value const * const c );

/**
   If c is a value type with a list (array, tuple, hashtable (kinda))
   and that list is currently being iterated over, or is otherwise
   locked, this function returns true (non-0), else false (0).

   Certain list operations, e.g. resizing, sorting, or hash table
   manipulation, are disallowed when a list is being iterated over.
   (Note that a hashtable is a list, of sorts.)

   @see cwal_value_may_iterate()
   @see cwal_value_may_iterate_list()
   @see cwal_value_is_iterating_list()
*/
bool cwal_value_is_iterating_props( cwal_value const * const c );

/**
   If c is a value type with a list (array, tuple, hashtable (kinda))
   and that list is currently being iterated over, or is otherwise
   locked, this function returns true (non-0), else false (0).

   Certain list operations, e.g. resizing, sorting, or hash table
   manipulation, are disallowed when a list is being iterated over.
   (Note that a hashtable is a list, of sorts.)

   @see cwal_value_may_iterate()
   @see cwal_value_may_iterate_list()
*/
bool cwal_value_is_iterating_list( cwal_value const * const c );

/**
   If c is a value type with a list (array, tuple, hashtable (kinda))
   and that list is currently capable of being iterated over, this
   function returns true (non-0), else false (0).

   Certain list operations, e.g. resizing, sorting, or hash table
   manipulation, are disallowed when a list is being iterated over.
   (Note that a hashtable is a list, of sorts.) A stateful sort
   operation (cwal_array_sort_stateful()) locks a list from
   being iterated while it is running.

   @see cwal_value_is_iterating_list()
   @see cwal_value_may_iterate()
*/
bool cwal_value_may_iterate_list( cwal_value const * const c );

/**
   Copies all non-hidden properties from src to dest.

   Returns 0 on success. Returns CWAL_RC_MISUSE if either pointer is
   NULL and CWAL_RC_TYPE if cwal_props_can() returns false for either
   src or dest.

   It returns CWAL_RC_IS_VISITING if dest is already being traversed
   (the data model does not support modifying properties during
   iteration).

   All property flags, e.g. CWAL_VAR_F_CONST, *EXCEPT* for
   CWAL_VAR_F_HIDDEN, are carried over from the source to the
   destination. Properties marked as CWAL_VAR_F_HIDDEN are "hidden"
   from iteration routines and therefore are not copied by this
   operation.

   Note that this does not actually _copy_ the properties - only
   references are copied. This function must, however, allocate
   internals to store the new properties, and can fail with
   CWAL_RC_OOM.

   This function treats CWAL_TYPE_PROPREF values as plain values - it
   copies the propref, as opposed to copying the proxied container/key
   pair.
*/
int cwal_props_copy( cwal_value * src, cwal_value * dest );

/**
   Removes a property from container-type value c.
   
   If c contains the given key (which must be keyLen bytes long), it
   is removed and 0 is returned. If it is not found, CWAL_RC_NOT_FOUND
   is returned (which can normally be ignored by client code).

   Returns 0 if the given key is found and removed.

   CWAL_RC_MISUSE is returned if obj or key are NULL or key has
   a length of 0.

   Fails with CWAL_RC_IS_VISITING if c is currently being iterated
   over.

   This function treats CWAL_TYPE_PROPREF values as plain values - it
   removes them, as opposed to removing their proxied container/key
   pairs.

   This is functionally equivalent calling
   cwal_prop_set(obj,key,keyLen,NULL).
*/
int cwal_prop_unset( cwal_value * c, char const * key,
                     cwal_midsize_t keyLen );

/**
   Like cwal_prop_unset() but takes a cwal_value key.
*/
int cwal_prop_unset_v( cwal_value * c, cwal_value * key );

/**
   The C-string counterpart of cwal_prop_set_v().

   Searches the given container value for a string-keyed property
   matching the first keyLen bytes of the given key. If found, it is
   returned. If no match is found, or any arguments are NULL, NULL is
   returned. The returned object is owned by c, and may be invalidated
   by ANY operations which change c's property list (i.e. add or
   remove properties).

   This routine will only ever match property keys for which
   cwal_value_get_cstr() returns non-NULL (i.e. property keys of type
   cwal_string or cwal_buffer). It never compares the key to
   non-string property keys (even though they might compare equivalent
   if the search key was a "real" cwal_string).

   See cwal_prop_get_v() for a caveat about CWAL_TYPE_PROPREF
   values.

   @see cwal_prop_get_kvp()
   @see cwal_prop_get_v()
   @see cwal_prop_get_kvp_v()
*/
cwal_value * cwal_prop_get( cwal_value const * c, char const * key,
                            cwal_midsize_t keyLen );

/**
   Searches the given container-type value for the key property,
   searching up c's prototype chain if needed.

   Its property key equivalence comparison depends on
   CWAL_OBASE_ISA_HASH: if true, it only does type-strict key
   comparison, else it may compare keys of different types for
   equivalence. e.g. the lookup key (integer 1) will match a property
   with a key of (double 1) or (string "1").

   Caveats regarding CWAL_TYPE_PROPREF values:

   1) If the search for a given key ends up triggering recursion
   during proxy-handling of CWAL_TYPE_PROPREF values then this
   function will return NULL. In such cases, cwal_engine_error_get()
   will return CWAL_RC_CYCLES_DETECTED and the error string will
   describe the problem. It is thought that this will be a rare (if
   ever), low-impact problem, but potentially tricky to chase down
   without checking cwal_engine_error_get() after each failed search.

   2) Once a propref wraps key/value pair, if that key/value pair is
   subsequently unset from the original container, getter operations
   on the propref will return NULL because they cannot find that
   property in the target.

   3) There's probably another one or three which haven't yet been
   discovered :).

   @see cwal_prop_get_kvp()
   @see cwal_prop_get()
   @see cwal_prop_get_kvp_v()
*/
cwal_value * cwal_prop_get_v( cwal_value const * c, cwal_value const * key );

/**
   The C-string counterpart of _prop_get_kvp(), differing only in the
   form it takes the property key. This getter will only ever match
   string-type keys, not keys which are otherwise semantically
   equivalent.

   See cwal_prop_get_kvp_v() for more details, in particular about the
   special handling of boolean-type lookup- and property keys.

   This function, unlike cwal_pop_get(), does not resolve propref
   properties, returning them as-is.

   Returns 0 if no match is found, !c, or !key.
*/
cwal_kvp * cwal_prop_get_kvp( cwal_value * c, char const * key,
                              cwal_midsize_t keyLen, bool searchProtos,
                              cwal_value ** foundIn );


/**
   The container value c is searched as described for cwal_prop_get(),
   but prototypes are only searched if searchProtos is true, in which
   case they are searched recursively if need. If foundIn is not NULL
   then if a match is found then *foundIn is set to the Value in which
   the key is found (it will be c or a prototype of c).

   Except in the case of a boolean lookup key or property key (see below),
   its property key equivalence comparison depends on CWAL_OBASE_ISA_HASH:
   if true, it only does type-strict key comparison, else it may
   compare keys of different types for equivalence. e.g. the lookup
   key (integer 1) will match a property with a key of (double 1) or
   (string "1").

   Returns 0 if no match is found, !c, or !key.

   ACHTUNG: the returned object is owned by c (or `*foundIn`) and may be
   invalidated on any modification (or cleanup) of c (or `*foundIn`).

   This function, unlike cwal_pop_get_v(), does not resolve propref
   properties, returning them as-is.

   Prior to 20190706 this routine incorrectly handled lookup/property
   keys of type CWAL_TYPE_BOOL, in that its equivalence comparison
   would, for a lookup key of cwal_value_true(), match the first
   "truthy" property key. Likewise, a property key of
   cwal_value_true() would match any truthy lookup key. Complementary
   comparisons applied for cwal_value_false(). This went unnoticed
   because boolean-type keys are apparently never used. As of
   20190706, when either the lookup key or a property key are of type
   boolean, this routine performs a type-strict comparison, so will
   never match anything but an exact match for those cases. This change
   applies to all of the various property lookup routines, not
   just this function.

   @see cwal_prop_get()
   @see cwal_prop_get_v()
   @see cwal_prop_get_kvp()
   @see cwal_prop_take()
   @see cwal_prop_take_v()
*/
cwal_kvp * cwal_prop_get_kvp_v( cwal_value * c, cwal_value const * key,
                                bool searchProtos,
                                cwal_value ** foundIn );
    
/**
   Similar to cwal_prop_get(), but removes the value from the parent
   container's ownership. This removes the owning container's
   reference point but does not destroy the value if its refcount
   reaches 0. If no item is found then NULL is returned, else the
   object (now owned by the caller or possibly shared with other
   containers) is returned.

   This is functionally similar to adding a ref to the property value,
   removing it from/unsetting it in the container (which removes the
   container's ref), and then cwal_value_unhand()ing it.

   Returns NULL if either c or key are NULL, key has a length of 0, or
   c is-not-a container.

   Note that this does not search through prototypes for a property -
   it only takes properties from the given value.

   See cwal_prop_get() for important details about the lookup/property
   key comparisons.

   If c is currently undergoing traversal, NULL is returned (a limitation
   of the data model).
       
   FIXME: #1: add a keyLen parameter, for symmetry with the rest of
   the API.

   @see cwal_prop_take_v()
   @see cwal_prop_get()
   @see cwal_prop_get_v()
*/
cwal_value * cwal_prop_take( cwal_value * c, char const * key );

/**
   Similar to cwal_prop_take() but also optionally takes over
   ownership of the key as well. If takeKeyAsWell is not NULL then
   ownership of the key is taken away, eactly as for the result value,
   and assigned to *takeKeyAsWell, otherwise the key is discarded
   (unreferenced).

   Note that they key passed to this function might be equivalent to
   (cwal_value_compare()), but not be a pointer match for, the key
   found in the the container, thus the passed-in key and
   *takeKeyAsWell may be different pointers on success. To deal with
   that, make sure to pass key to cwal_value_ref() before calling
   this, and to one of cwal_value_unhand() or cwal_value_unref()
   (depending on key's current lifetime requirements) after calling
   this.

   If no entry is found, NULL is returned and *takeKeyAsWell is not
   modified.

   This function treats CWAL_TYPE_PROPREF values as plain values -
   it does not proxy them.

   @see cwal_prop_take()
   @see cwal_prop_get()
   @see cwal_prop_get_v()
*/
cwal_value * cwal_prop_take_v( cwal_value * c, cwal_value * key,
                               cwal_value ** takeKeyAsWell );

/**
   Functionally similar to cwal_array_set(), but uses a string key as
   an index. Like arrays, if a value already exists for the given key,
   it is unreferenced by this function before inserting the new value.

   c must be a "container type" (capable of holding key/value
   pairs). For the list of types capable of having properties, see
   cwal_props_can().

   If v is NULL then this call is equivalent to
   cwal_prop_unset(c,key,keyLen). Note that (v==NULL) is treated
   differently from v having the special null value
   (cwal_value_null()). In the latter case, the key is set to the
   special null value.

   The key may be encoded as ASCII or UTF8. Results are undefined
   with other encodings, and the errors won't show up here, but may
   show up later, e.g. during output.
   
   The flags argument may be a mask of cwal_var_flags_e values. When
   in doubt about whether the property is already set and might
   have other flags set, use CWAL_VAR_F_PRESERVE. If it makes no
   difference for your use case, feel free to pass 0.

   Returns 0 on success, non-0 on error. It has the following error
   cases:

   - CWAL_RC_MISUSE: e, c, or key are NULL or !*key.

   - CWAL_RC_TYPE: c is not of a type capable of holding
   properties.

   - CWAL_RC_OOM: an out-of-memory error

   - CWAL_RC_IS_VISITING: The library does not support modifying a
   container which is being visited/iterated over.

   - CWAL_RC_CONST_VIOLATION: cannot (re)set a const property.

   - CWAL_RC_DISALLOW_NEW_PROPERTIES: the container has been flaged
   with the CWAL_CONTAINER_DISALLOW_NEW_PROPERTIES flag and key refers
   to a property which is not in the container.

   - CWAL_RC_DISALLOW_PROP_SET: the container has been flaged with the
   CWAL_CONTAINER_DISALLOW_PROP_SET flag.

   - CWAL_RC_NOT_FOUND if !v and the entry is not found. This can
   normally be ignored as a non-error, but is provided for
   completeness. (2022-02-17: that may have been a misled bit of
   over-engineering, as this has caused me Grief in both s2 and whcl.)

   - CWAL_RC_DESTRUCTION_RUNNING if c is currently being finalized.
   This "should" only ever be returned in conjunction with cwal_native
   types if they have a finalizer and that finalizer manipulates the
   cwal_native/cwal_value part it has held on to in parallel.


   On error ownership of v is NOT modified, and the caller may still
   need to clean it up. For the variants of this function
   taking a (cwal_value*) key, ownership of the key is also not
   changed on error.

   For example, the following code will introduce a "leak" (insofar as
   cwal really leaks) if this function fails:

   @code
   cwal_prop_set( myObj, "foo", 3, cwal_new_integer(e, 42) );
   @endcode

   Because the value created by cwal_new_integer() has no owner
   and is not cleaned up. The "more correct" way to do this is:

   @code
   cwal_value * v = cwal_new_integer(e, 42);
   cwal_value_ref(v);
   int rc = cwal_prop_set_with_flags( myObj, "foo", 3, v, CWAL_VAR_F_HIDDEN );
   cwal_value_unref(v);
   // if prop_set worked, v has a reference, else it is cleaned up
   if( 0 != rc ) {
   ... handle error.
   }
   @endcode

   However, because the value in that example is owned by the active
   scope, it will be cleaned up when the scope exits if the user does
   not unref it manually. i.e. it is still "safe" vis-a-vis not
   leaking memory, to use the first (simpler) insertion
   option. Likewise, as cwal_scope_sweep() can also clean up the
   errant integer within the current scope (but often has other
   side-effects, so be careful with that).

   See cwal_prop_get() for important details about the lookup/property
   key comparisons.

   @see cwal_prop_set()
   @see cwal_prop_set_v()
   @see cwal_prop_set_with_flags_v()
*/
int cwal_prop_set_with_flags( cwal_value * c, char const * key,
                              cwal_midsize_t keyLen, cwal_value * v,
                              uint16_t flags );

/**
   Equivalent to calling cwal_prop_set_with_flags() using the same
   parameters, and a flags value of CWAL_VAR_F_PRESERVE.
*/
int cwal_prop_set( cwal_value * c, char const * key,
                   cwal_midsize_t keyLen, cwal_value * v );

/**
   Returns non-0 (true) if c is of a type capable of containing
   per-instance properties, else 0 (false).

   The following value types will return true from this function:

   CWAL_TYPE_ARRAY, CWAL_TYPE_OBJECT, CWAL_TYPE_FUNCTION,
   CWAL_TYPE_EXCEPTION, CWAL_TYPE_NATIVE, CWAL_TYPE_HASH,
   CWAL_TYPE_BUFFER (as of 20141217).

   All others return false.

   Note that properties referred to by this function are independent
   of an array's indexed entries or a hashtable's entries.
*/
bool cwal_props_can( cwal_value const * c );

/**
   Returns true if cwal_props_can() returns true and if the value
   actually has any properties (not including those in prototype
   values). This is an O(1) operation.
*/
bool cwal_props_has_any( cwal_value const * c );

/**
   Returns true if c is a value type which is suitable for use as a
   key for object properties or hashtable entries. As of 2021-07-09,
   types whose equivalence comparison takes their mutable state into
   account are disallowed as property keys. That includes
   CWAL_TYPE_BUFFER and CWAL_TYPE_TUPLE.  Prior to that, any data type
   could be used as a property key, but that was a fundamentally
   flawed design decision.
*/
bool cwal_prop_key_can( cwal_value const * c );

/**
   Returns the number of properties in c (not including prototypes),
   or 0 if c is not a properties-capable type. This is an O(1)
   operation of built with CWAL_OBASE_ISA_HASH, else it's an O(N)
   operation.

   Note that this count includes properties flagged with
   CWAL_VAR_F_HIDDEN.
*/
cwal_midsize_t cwal_props_count( cwal_value const * c );

/**
   Like cwal_prop_set_with_flags() but takes a cwal_value key.

   Note that whether this this routine does a type-loose or
   type-script key comparison for the lookup key and property keys
   depends on whether the library is built with CWAL_OBASE_ISA_HASH
   enabled or not. If so, key comparison is necessarily
   type-strict. If not, cwal_value_compare() is used for the
   comparison, which allows a small range of non-allocating type
   conversions for comparison purposes (e.g. integers and
   integer-looking strings).

   If cwal_prop_key_can() returns false for the given key,
   CWAL_RC_TYPE is returned. Prior to 2021-07-09, any data type could
   be used as a property key, but that was a fundamentally flawed
   design decision.

   If passed a NULL v, this function treats CWAL_TYPE_PROPREF values
   as plain values - it removes them, as opposed to removing their
   proxied container/key pairs.

   Note that properties are always set directly on the given object,
   never in a prototype, even if a prototype has a matching property
   key.
*/
int cwal_prop_set_with_flags_v( cwal_value * c, cwal_value * key,
                                cwal_value * v,
                                uint16_t flags );

/**
   Equivalent to calling cwal_prop_set_with_flags_v() using the same
   parameters, and a flags value of CWAL_VAR_F_PRESERVE.
*/
int cwal_prop_set_v( cwal_value * c, cwal_value * key, cwal_value * v );

/**
   Returns true (non-zero) if the value v contains the given
   property key. If searchPrototype is true then the search
   continues up the prototype chain if the property is not found,
   otherwise only v is checked.

   See cwal_prop_get_kvp_v() for details about the lookup/property key
   comparisons.
*/
bool cwal_prop_has( cwal_value const * v, char const * key,
                    cwal_midsize_t keyLen,
                    bool searchPrototype );

/**
   Like cwal_prop_has() but takes a cwal_value key.

   See cwal_prop_get_kvp_v() for details about the lookup/property key
   comparisons.
*/
bool cwal_prop_has_v( cwal_value const * v, cwal_value const * key,
                      bool searchPrototype );
    
/**
   Returns the virtual type of v, or CWAL_TYPE_UNDEF if !v.
*/
cwal_type_id cwal_value_type_id( cwal_value const * v );

/**
   If v is not NULL, returns the internal string name of v's
   concrete type, else it returns NULL. If no type name proxy is
   installed (see cwal_engine_type_name_proxy()) then the returned
   bytes are guaranteed to be static, else they are gauranteed (by
   the proxy implementation) to live at least as long as v.

   If it returns non-NULL and len is not NULL then *len will hold
   the length of the returned string.

   LIMITATION: if v is a builtin value then it has no engine
   associated with it, meaning we cannot get proxied name. We can
   fix that by adding an engine parameter to this function.
*/
char const * cwal_value_type_name2( cwal_value const * v,
                                    cwal_size_t * len);

    
/**
   Equivalent to cwal_value_type_name2(v, NULL).
*/
char const * cwal_value_type_name( cwal_value const * v );

/**
   For the given cwal_type_id value, if that value represents
   a client-instantiable type, this function returns the same
   as cwal_value_type_name() would for an instance of that type,
   else returns NULL.
*/
char const * cwal_type_id_name( cwal_type_id id );

/**
   For the given type ID, returns its "base sizeof()," which
   has a slightly different meaning for different types:

   CWAL_TYPE_BOOL, UNDEF, NULL: are never allocated and return 0.

   CWAL_TYPE_LISTMEM: are internal metrics-counting markers and have
   no sizes, so return 0.

   CWAL_TYPE_STRING/XSTRING/ZSTREAM: returns the base size of string
   values (which may differ for each string sub-type), not including
   their string bytes or trailing NUL.

   CWAL_TYPE_INTEGER, DOUBLE, ARRAY, OBJECT, NATIVE, BUFFER, FUNCTION,
   EXCEPTION, HASH: returns the allocated size of the Value part plus
   its high-level type representation (e.g. cwal_object), not
   including any dynamic data such as hash table/array memory or
   properties.

   CWAL_TYPE_WEAKREF, KVP: returns sizeof(cwal_weakref)
   resp. sizeof(cwal_kvp).

   CWAL_TYPE_SCOPE: sizeof(cwal_scope). Though the API technically
   supports dynamically-allocated scopes, in practice their usage fits
   perfectly with stack allocation.

   Returns 0 if passed an unknown value.
*/
cwal_size_t cwal_type_id_sizeof( cwal_type_id id );

/** Returns true if v is null, v->api is NULL, or v holds the special undefined value. */
bool cwal_value_is_undef( cwal_value const * v );
/** Returns true if v contains a null value. */
bool cwal_value_is_null( cwal_value const * v );
/** Returns true if v contains a bool value. */
bool cwal_value_is_bool( cwal_value const * v );
/** Returns true if v contains an integer value. */
bool cwal_value_is_integer( cwal_value const * v );
/** Returns true if v contains a double value. */
bool cwal_value_is_double( cwal_value const * v );
/** Returns true if v contains a number (double, integer, bool) value. */
bool cwal_value_is_number( cwal_value const * v );
/** Returns true if v contains a cwal_string value. */
bool cwal_value_is_string( cwal_value const * v );
/** Returns true if v contains an cwal_array value. */
bool cwal_value_is_array( cwal_value const * v );
/** Returns true if v contains an cwal_object value. */
bool cwal_value_is_object( cwal_value const * v );
/** Returns true if v contains a cwal_native value. */
bool cwal_value_is_native( cwal_value const * v );
/** Returns true if v contains a cwal_buffer value. */
bool cwal_value_is_buffer( cwal_value const * v );
/** Returns true if v contains a cwal_exception value. */
bool cwal_value_is_exception( cwal_value const * v );
/** Returns true if v contains a cwal_hash value. */
bool cwal_value_is_hash( cwal_value const * v );
/** Returns true if v contains a "unique" value
    (of type CWAL_TYPE_UNIQUE). */
bool cwal_value_is_unique( cwal_value const * v );
/** Returns true if v contains a cwal_tuple value. */
bool cwal_value_is_tuple( cwal_value const * v );

/**
   A special-purpose function which upscopes v into s if v is owned by
   a lower (newer) scope. If v is owned by s, or a higher scope, then
   this function has no side-effects. This is necessary when clients
   create values which they need to survive the current scope. In such
   cases they should pass the scope they want to (potentially)
   reparent the value into.

   Note that this reparenting is only for lifetime management
   purposes, and has nothing at all to do with "scope variables". It
   does not affect the script-side visibility of v.

   Returns 0 on success, CWAL_RC_MISUSE if any argument is 0, and
   is believed to be infalible (hah!) as long as the arguments are
   legal and their underlying cwal_engine is in a legal state.

   This is a no-op (returning 0) if v's current owning scope is older
   than s or if cwal_value_is_builtin(v). It may assert() that neither
   argument is NULL.
*/
int cwal_value_rescope( cwal_scope * s, cwal_value * v );

/**
   Given a pointer, returns true (non-0) if m lives in the memory
   region used for built-in/constant/shared (cwal_value*) instances,
   else returns 0.  Is tolerant of NULL (returns 0). This
   determination is O(1) or 2x O(1) (only pointer range comparisons),
   depending on whether we have to check both sets of builtins.
   
   If this returns true (non-0), m MUST NOT EVER be cwal_free()d
   because it refers to stack memory! If m refers to a (cwal_value*)
   instance, that value is a built-in and it MAY be (harmlessly)
   passed to any public Value-lifetime-management routines (e.g.
   cwal_value_ref(), cwal_value_unref(), and cwal_value_unhand()), all
   of which are no-ops for built-in values. Note that this returns
   true for any address in a range which covers more than just
   cwal_value instances, but in practice it is used only to check
   whether a cwal_value is a built-in. For example, if cwal is
   compiled with length-1 ASCII strings as built-ins then:

   @code
   cwal_value_is_builtin(
   cwal_value_get_cstr(
   cwal_new_string_value(e,"a",1), 0
   )
   )
   @endcode

   will return true, as those string bytes are in the built-in block
   of memory.
*/
bool cwal_value_is_builtin( void const * m );
    
/**
   Creates a new value which refers to a client-provided "native"
   object of an arbitrary type. N is the native object to bind.
   dtor is the optional finalizer function to call when the new
   value is finalized. typeID is an arbitrary pointer which is
   used later to verify that a given cwal_native refers to a
   native of a specific type.

   A stack-allocated native pointer is only legal if it can be
   _guaranteed_ to out-live the wrapping value handle.

   This function returns NULL if (e, N, typeID) are NULL or on
   allocation error.On success it returns a new value, initially
   owned by the current scope.

   Clients can fetch the native value later using
   cwal_native_get(). The typeID passed here can be passed to
   cwal_native_get() to allow cwal to confirm that the caller is
   getting the type of pointer he wants. In practice the typeID is
   a pointer to an app/library-wide value of any type. Its
   contents are irrelevant, only its _address_ is relevant. While
   it might seem intutive to use a string as the type ID,
   compilers may (or may not) combine identical string constants
   into a single string instance, which may or may not foul up
   such usage. If one needs/wants to use a string, set it in 1
   place, e.g.  via a file-scope variable, and expose its address
   to any client code which needs it (as opposed to them each
   inlining the string, which might or might not work at runtime,
   depending on whether the strings get compacted into a single
   instance).
       
   When the returned value is finalized (at a time determined by
   the cwal engine), if dtor is not NULL then dtor(e,N) is called
   to free the value after any Object-level properties are
   destructed. If N was allocated using cwal_malloc() or
   cwal_realloc() and it has no special cleanup requirements then
   cwal_free can be passed as the dtor value. Finalizer functions
   "might currently be prohibited" from performing "certain
   operations" with the cwal API during cleanup, but which ones
   those are (or should be) are not yet known.

   Note that it is perfectly legal to use a static value for N,
   assuming the finalizer function (if any) does not actually try
   to free() it. In the case of a static, the value could be used
   as its own typeID (but since the client has that pointer, and
   it's static, there doesn't seem to be much use for having a
   cwal_native for that case!).
       
   See cwal_new_VALUE() for more details about the return value.
       
   @see cwal_new_VALUE()
   @see cwal_new_native()       
   @see cwal_value_get_native()
*/
cwal_value * cwal_new_native_value( cwal_engine * e, void * N,
                                    cwal_finalizer_f dtor,
                                    void const * typeID );

/**
   Equvalent to passing the return value of
   cwal_new_native_value() to cwal_value_get_native().
*/
cwal_native * cwal_new_native( cwal_engine * e, void * n,
                               cwal_finalizer_f dtor,
                               void const * typeID );

/**
   A special-purpose component for binding native state to
   cwal_natives and cwal_functions, necessary when native bindings
   hold Value pointers whose lifetimes are not managed via
   Object-level properties of the native.

   This callback is called whenever the Value part of the
   function/native is moved to an older scope (for lifetime management
   purposes, not script visibility purposes) and is passed the
   following arguments:

   s: the scope to potentially be rescoped to.

   v: the cwal_value part of the Native or Function. Use
   cwal_value_get_native() or cwal_value_get_function(), as
   appropriate, to get the higher-level part. DO NOT use
   cwal_value_native_part() resp. cwal_value_function_part(),
   as those may return parts of _other_ values higher up
   in v's prototype chain!

   Rescopers MUST do the following:

   a) For each "unmanaged" Value, call cwal_value_rescope(s,
   theValue). They MUST NOT pass a NULL value to cwal_value_rescope(),
   or an assertion may be triggered.

   d) It must not rescope the Native/Function passed to it, as that
   value is in the process of rescoping when this is called.

   Implementations must return 0 on success and any error is
   tantamount to an assertion, leading to undefined results
   in cwal from here on out.

   Implementations MUST NOT perform any work which might allocate
   values. (Potential TODO: disable the allocator during this
   operation, to enforce that requirements.)

   Note that it is often necessary to make such hidden/internal values
   vacuum-proof by using cwal_value_make_vacuum_proof() on it (for
   containers) or adding them to a hidden/internal container which is
   itself vacuum-proofed. Alternately, such refs can be held as hidden
   properties of the cwal_native, perhaps using unique keys (via
   cwal_new_unique()) to keep client code from every being able to
   address them. When stored in the cwal_native's properties, no extra
   Value rescoping/ownership management is necessary on the client
   side.

   @see cwal_native_set_rescoper()
   @see cwal_function_set_rescoper()
*/
typedef int (*cwal_value_rescoper_f)( cwal_scope * s, cwal_value * v );

/**
   A special-case function which is necessary when client-side
   natives manage Values which are not visible to the native's
   Object parent (i.e. they are not tracked as properties). When
   creating such natives, after calling cwal_new_native() or
   cwal_new_native_value(), call this function and pass it your
   rescoper implementation. 

   Results are undefined if nv is NULL.

   When the given rescoper is called, cwal_value_get_native() can be
   called on its argument to get the nv pointer which was passed to
   this function (it will never be NULL when the rescoper is called
   from cwal). DO NOT use cwal_value_native_part()!

   @see cwal_value_rescoper_f()
*/
void cwal_native_set_rescoper( cwal_native * const nv,
                               cwal_value_rescoper_f const rescoper );

/**
   A special-case function which is necessary when client-side
   function state Values which are not visible to the function's
   Object parent (i.e. they are not tracked as properties). When
   creating such natives, after calling cwal_new_function() (or
   equivalent), call this function and pass it your rescoper
   implementation.

   This function returns 0 unless f is NULL, in which case it
   returns CWAL_RC_MISUSE.

   When the given rescoper is called, cwal_value_get_function() can be
   called on its argument to get the nv pointer which was passed to
   this function (it will never be NULL when the rescoper is called
   from cwal). DO NOT use cwal_value_function_part()!

   @see cwal_value_rescoper_f()
*/
int cwal_function_set_rescoper( cwal_function * f,
                                cwal_value_rescoper_f rescoper );

    
/**
   Returns the cwal_value form of n, or 0 if !n.
*/
cwal_value * cwal_native_value( cwal_native const * n );

/**
   If val is of type CWAL_TYPE_NATIVE then this function
   assigns *n (if n is not NULL) to its cwal_native handle
   and returns 0, else it returns CWAL_RC_TYPE and does not
   modify *n.

   In practice this is never used by clients - see
   cwal_value_get_native().

   @see cwal_value_get_native()
   @see cwal_value_native_part()
*/
int cwal_value_fetch_native( cwal_value const * val, cwal_native ** n);

/**
   If v is of type CWAL_TYPE_NATIVE then this function returns its
   native handle, else it returns 0. This is a simplified form of
   cwal_value_fetch_native().
*/
cwal_native * cwal_value_get_native( cwal_value const * v );
    
/**
   Fetches the a raw "native" value (void pointer) associated with
   n.
       
   If (0==typeID) or n's type ID is the same as typeID then *dest (if dest is not NULL)
   is assigned to n's raw native value and 0 is returned, else...

   CWAL_RC_TYPE: typeID does not match.

   CWAL_RC_MISUSE: n is NULL.

   Note that clients SHOULD pass a value for typeID to ensure that
   they are getting back the type of value they expect. The API
   recognizes, however, that the type ID might not be available or
   might be irrelevant to a particular piece of code, and
   therefore allows (but only grudgingly) typeID to be NULL to
   signify that the client knows WTF he is doing and is getting a
   non-type-checked pointer back (via *dest).
*/
int cwal_native_fetch( cwal_native const * n, void const * typeID, void ** dest);

/**
   Convenience form of cwal_native_fetch() which returns NULL if
   n's type ID does not match typeID.
*/
void * cwal_native_get( cwal_native const * n, void const * typeID);    

/**
   Clears the underlying native part of n, such that future calls to
   cwal_native_get() will return NULL and future calls to this
   functions are no-ops. If callFinalizer is true then the native's
   finalizer, if not NULL, is called, otherwise we assume the caller
   knows more about the lifetime of the value than we do and the
   finalizer is not called. As a general rule, clients should pass a
   true value for the second parameter.

   Results are undefined if n is NULL or not in a well-defined state
   (e.g. if it's currently sitting in its owning engine's
   memory-recycling bin).
*/
void cwal_native_clear( cwal_native * const n, bool callFinalizer );
    
/**
   Creates a new "buffer" value. startingSize is the amount of
   memory to reserve in the buffer by default (0 means not to
   reserve any, of course). If reservation of the buffer fails
   then this function returns NULL.

   See cwal_new_VALUE() for details on the ownership.

   See the cwal_buffer API for how to use buffers.
*/
cwal_value * cwal_new_buffer_value(cwal_engine *e, cwal_size_t startingSize);

/**
   Equvalent to passing the return value of
   cwal_new_buffer_value() to cwal_value_get_buffer().
*/
cwal_buffer * cwal_new_buffer(cwal_engine *e, cwal_size_t startingSize);

/**
   Equivalent to cwal_value_unref( e, cwal_buffer_value(v) ).
*/
int cwal_buffer_unref(cwal_engine *e, cwal_buffer *v);

/**
   This convenience routine takes b's buffered memory and
   transfers it to a new Z-string value (see
   cwal_new_zstring()).

   b may either have been created using cwal_new_buffer() or be a
   "non-value" buffer which the client happens to be using.

   See cwal_new_string() for size limitation notes.

   The new string will have a string byte length of b->used.

   If !b or !e, if b->used is too big, or on allocation error, NULL is
   returned. If b has no memory, the empty string value is
   returned. The returned value is owned by e and (unless it is the
   empty string) will initially be owned by e's current scope. If NULL
   is returned, b's memory is not modified, otherwise after calling
   this b will be an empty buffer (but its lifetime is otherwise
   unaffected).

   After returning, if b->mem is not NULL then b still owns its
   managed buffer (and there was an error, so NULL will have been
   returned). For most use cases, clients should unconditionally pass
   b to cwal_buffer_clear() after calling this, as they presumably had
   no interest in managing b's memory. On success, b->mem's ownership
   will have been transfered to the returned string and b->mem will be
   0.

   For metrics-counting purposes, b->mem's memory is counted by whoever
   allocated it first, and not by z-string metrics.
*/
cwal_string * cwal_buffer_to_zstring(cwal_engine * e, cwal_buffer * b);

/**
   Equivalent to cwal_string_value(cwal_buffer_to_zstring(e,b)).

   @see cwal_buffer_to_zstring()
*/
cwal_value * cwal_buffer_to_zstring_value(cwal_engine * e, cwal_buffer * b);

/**
   Returns a pointer to b's managed memory. If n is not NULL then `*n`
   is set to the length, in bytes, of that memory. If b has no memory,
   returns NULL and n is not modified.
*/
char const * cwal_buffer_cstr(cwal_buffer const * const b, cwal_size_t * n);
    
/**
   Equivalent to cwal_value_fetch_object() and friends, but for
   buffer values.

   In practice this is never used by clients - see
   cwal_value_get_buffer().
*/
int cwal_value_fetch_buffer( cwal_value const * val, cwal_buffer ** ar);

/**
   If value is-a Buffer then this returns the cwal_buffer form of the
   value, else it returns 0.
*/
cwal_buffer * cwal_value_get_buffer( cwal_value const * v );

/**
   Returns the cwal_value handle associated with the given buffer,
   or NULL if !s.

   WARNING OH MY GOD SUCH AN IMPORTANT WARNING: NEVER EVER EVER
   pass a cwal_buffer which was NOT created via
   cwal_new_buffer_value() to this function!!! It WILL cause
   invalid memory access if passed e.g. a cwal_buffer which was
   allocated on the stack (or by ANY means other than the
   functions listed above) and might (depending on the state of
   the random memory we're reading) cause the client to get
   invalid memory back (as opposed to NULL).
*/
cwal_value * cwal_buffer_value(cwal_buffer const * s);

/**
   Creates a new "exception" value.
       
   See cwal_new_VALUE() for details on the ownership of the return
   value.

   code is a client-interpreted error code. (Clients are free to
   use the cwal_rc values.) msg is an optional (may be NULL) value
   which stores some form of error message (of an arbitrary value
   type). Exception values may hold key/value pairs, so they may
   be "enriched" with client-specific information like a stack
   trace or source line/column information.

   On success the returned Exception value will contain the
   properties "code" and "message", reflecting the values passed
   here.

   @see cwal_new_exception().
   @see cwal_new_exceptionf()
   @see cwal_new_exceptionfv()
*/
cwal_value * cwal_new_exception_value(cwal_engine *e, int code, cwal_value * msg );

/**
   Equivalent to passing the return value of
   cwal_new_exception_value() to cwal_value_get_exception().
*/
cwal_exception * cwal_new_exception(cwal_engine *e, int code, cwal_value * msg );

/**
   A printf-like form of cwal_new_exception() which uses
   cwal_new_stringf() to create a formatted message to pass to
   cwal_new_exception().  Returns the new Exception value on
   success, NULL on allocation error or if either e is NULL. A
   format string of NULL or "" are treated equivalently as NULL.

   @see cwal_new_exceptionf()
   @see cwal_new_exception()
*/
cwal_exception * cwal_new_exceptionfv(cwal_engine * e, int code, char const * fmt, va_list args );

/**
   Identical to cwal_new_exceptionv() but takes its arguments in ellipsis form.
*/
cwal_exception * cwal_new_exceptionf(cwal_engine * e, int code, char const * fmt, ... );

/**
   Returns true if v is-a Exception, else false.
*/
bool cwal_value_is_exception(cwal_value const *v);

    
/**
   Equivalent to cwal_value_unref( e, cwal_exception_value(v) ).
*/
int cwal_exception_unref(cwal_engine *e, cwal_exception *v);
    
/**
   Equivalent to cwal_value_fetch_object() and friends, but for
   error values.

   In practice this is never used by clients - see
   cwal_value_get_exception().
*/
int cwal_value_fetch_exception( cwal_value const * val, cwal_exception ** ar);

/**
   If value is-a Exception then this returns the cwal_exception
   form of the value, else it returns 0.
*/
cwal_exception * cwal_value_get_exception( cwal_value const * v );

/**
   Returns the cwal_value handle associated with the given error
   value, or NULL if !r.
*/
cwal_value * cwal_exception_value(cwal_exception const * r);

/**
   Returns r's current result code, or some unspecified non-0
   value if !r.
*/
int cwal_exception_code_get( cwal_exception const * r );

/**
   Sets r's result code. Returns 0 on success, CWAL_RC_MISUSE
   if !r.
*/
int cwal_exception_code_set( cwal_exception * r, int code );

/**
   Returns the "message" part of the given error value, NULL if !r
   or r has no message part. The returned value is owned by/shared
   with r via reference counting, and it must not be unref'd by
   the client unless he explicitly references himself.
*/
cwal_value * cwal_exception_message_get( cwal_exception const * r );

/**
   Sets the given msg value to be r's "message" component. Interpretation
   of the message is up to the client.

   Returns 0 on success, CWAL_RC_MISUSE if either e or r are
   NULL. msg may be NULL.

   This function adds a reference to msg and removes a reference
   from its previous message (if any).
*/
int cwal_exception_message_set( cwal_engine * e, cwal_exception * r, cwal_value * msg );
    

/**
   Creates a new value wrapping a function.

   e is the owning engine, callback is the native function to wrap
   (it may not be NULL). state is optional state for the callback
   and may (assuming client application conditions allow for it)
   be NULL.

   The stateTypeID parameter is not directly used by the framework
   but can be used when the callback is called (via
   cwal_function_call() and friends) to determine whether the
   state parameter passed into the function is of a type expected
   by the client (which avoids mis-casting pointers when script
   code criss-crosses methods between object instances and
   classes). See cwal_args_state() for more details.
       
   See cwal_new_VALUE() for details regarding ownership and lifetime
   of the returned value.

   Returns NULL if preconditions are not met (e and callback may
   not be NULL) or on allocation error.

   When the callback is called via cwal_function_call() and
   friends, state->data will be available via the
   cwal_callback_args instance passed to the callback.

   When the returned value is destroyed, if stateDtor is not NULL
   then stateDtor(state) is called at destruction time to clean up
   the state value.
*/
cwal_value * cwal_new_function_value( cwal_engine * e,
                                      cwal_callback_f callback,
                                      void * state,
                                      cwal_finalizer_f stateDtor,
                                      void const * stateTypeID );
/**
   Equvalent to passing the return value of
   cwal_new_function_value() to cwal_value_get_function().
*/
cwal_function * cwal_new_function( cwal_engine * e, cwal_callback_f,
                                   void * state, cwal_finalizer_f stateDtor,
                                   void const * stateTypeID );
/**
   Returns true if v is-a Function, else false.
*/
bool cwal_value_is_function(cwal_value const *v);
/**
   If v is-a Function then this returns that Function handle,
   else it returns 0.
*/
cwal_function * cwal_value_get_function(cwal_value const *v);
/**
   Returns the Value handle part of f, or 0 if !f.
*/
cwal_value * cwal_function_value(cwal_function const *f);
/**
   Equivalent to cwal_value_unref(cwal_function_value(f)).
*/
int cwal_function_unref(cwal_function *f);

/**
   Calls the given function, passing it the given arguments and other
   state via its single cwal_callback_args parameter.

   The given scope is used as the execution context for purposes of
   ownership of new values.
       
   self may technically be 0, but f may require it to be of a specific
   type. Its intention is to be interpreted as the "this" object/value
   for the call (the semantics of which are client-dependent).

   argv must point at at least argc values. Both argv and argc may be
   0. This function takes a reference to each value in the list to
   protect them from being swept up by cwal_engine_sweep() (or
   similar) during the function call, but it does not make them
   vacuum-proof. After the call completes, each reference is released
   using cwal_value_unhand(), as opposed to cwal_value_unref(), so
   that the values survive the return trip to the caller.

   Returns the result from f or CWAL_RC_MISUSE if any arguments are
   invalid. Callback implementors should keep in mind that returning a
   value other than 0 (CWAL_RC_OK) will "usually" (but not always) be
   interpreted as an error condition. The exact details depend on the
   client's use of cwal.

   If resultVal is not NULL then on success *resultVal holds the
   value-level result from the function call (which may be 0, but
   clients typically interpret that as cwal_value_undefined()).
   Clients are recommended to explicitly initialize *resultVal to 0
   before calling this, to avoid potential confusion afterwards. On
   error *resultVal is not modified.

   It is strictly illegal to pop the current scope from within (or
   via) the f->callback(). Subscopes may of course be pushed (and must
   be popped before returning to this function, or an assertion may be
   triggered!).

   If s is not the interpreter's current scope, this function
   artificially changes the current scope, which comes with a
   _potential_ caveat: during the life of the f->callback() call, up
   until the next scope is pushed (if that happens), s is the current
   scope for all intents and purposes. But that's the point of this
   routine. That said: no small amount of practice implies that
   s should always be the engine's current scope.

   If callback hooks have been installed via cwal_callback_hook_set()
   then they are triggered in this function as described in the
   cwal_callback_hook documentation.  The "pre" callback is only
   triggered after it is certain that f will be called (i.e. after
   basic argument validation). If the pre-callback returns non-0 then
   neither f nor the post-callback are triggered. If the pre-callback
   returns 0 then both f and the post-callback are guaranteed to be
   called.

   f and self will be made sweep-proof (via a ref) during the life of
   the call. Both will be made vacuum-proof as well, if they weren't
   already.

   ACHTUNG: NEVER EVER pass this function any scope other than the
   current one. Doing so may violate all kinds of rules and will
   certainly lead to chaos. Exposing the scope pointer as part of the
   API was a bad design decision, from a time when it wasn't yet clear
   how badly running in another scope could break stuff. In particular
   the addition of scope push/pop hooks tie client-side state to the
   scope stack, and pushing a subscope from any scope other than the
   current scope can lead to all sorts of confusion. The cwal_engine
   API has a hard-coded assumption that only the current scope is
   _ever_ active.
*/
int cwal_function_call_in_scope( cwal_scope * const s, 
                                 cwal_function * const f,
                                 cwal_value * const self,
                                 cwal_value ** resultVal,
                                 uint16_t argc,
                                 cwal_value * const * argv );

/**
   Identical to cwal_function_call_in_scope() except that it sets the
   cwal_callback_args::propertyHolder value to the value passed as the
   3rd argument to this function (which may be NULL).

   The propertyHolder member is intended to be used by container
   member functions which may need to distinguish between their "this"
   and the "owner" of the property (insofar as any container owns a
   value it contains). Specifically, it was added to support certain
   property interceptor usages. The vast, vast majority of functions
   have no need for it.

   @see cwal_function_call()
*/
int cwal_function_call_in_scope2( cwal_scope * s, 
                                  cwal_function * const f,
                                  cwal_value * const propertyHolder,
                                  cwal_value * const self,
                                  cwal_value ** resultVal,
                                  uint16_t argc,
                                  cwal_value * const * argv );

/**
   Convenience form of cwal_function_call_in_scope() which pushes a
   new scope onto the stack before calling that function.

   Note that the value-level result of the function call might be
   owned by the pushed scope or a subscope, and therefore be cleaned
   up when the function call returns. If resultValue is not NULL then
   the result value of the call() is moved into the scope which was
   active before the call, such that it is guaranteed to survive when
   the scope created for the call() is closed. If resultValue is null,
   scope ownership of the call()'s result is not modified, and it
   "may" be cleaned up as soon as the scope expires.

   Returns the result of cwal_function_call_in_scope(), or some non-0
   CWAL_RC_xxx code if pushing a new scope fails (which can only
   happen if the client has installed a scope-push hook into the
   cwal_engine and that hook fails).
*/
int cwal_function_call( cwal_function * f,
                        cwal_value * self,
                        cwal_value ** resultVal,
                        uint16_t argc,
                        cwal_value * const * argv );

/**
   Identical to cwal_function_call_in_scope() except that it sets the
   cwal_callback_args::propertyHolder value to the value passed as the
   2nd argument to this function (which may be NULL).

   @see cwal_function_call_in_scope2()
*/
int cwal_function_call2( cwal_function * f,
                         cwal_value * propertyHolder,
                         cwal_value * self,
                         cwal_value ** resultVal,
                         uint16_t argc,
                         cwal_value * const * argv );

/**
   A form of cwal_function_call() which takes its arguments in
   the form of a cwal_array (which may be NULL).

   If s is NULL then this acts as a proxy for cwal_function_call(),
   otherwise it behaves like cwal_function_call_in_scope(), using
   s as the call scope.

   Returns 0 on success, non-0 on error.

   Results are undefined if args is NULL.

   This routine makes the args array safe from sweep-up and
   vacuuming (if it was not already so) for the duration of the
   call. (The f parameter will be made so via the proxied
   function.)

   ACHTUNG: any empty entries in the array will be passed to the
   callback as literal NULLs, and experience has shown that most
   callbacks do not generally expect any literal NULLs (because
   script code cannot generate them). So... be careful with that.
*/
int cwal_function_call_array( cwal_scope * s, cwal_function * f,
                              cwal_value * self, cwal_value ** rv,
                              cwal_array * args);


/**
   Works like cwal_function_call() but has very specific requirements
   on the variadic arguments: the list must contain 0 or more
   (cwal_value*) arguments and MUST ALWAYS be terminated by a NULL
   value (NOT a cwal_value, e.g. cwal_value_null(), but a literal
   NULL).  Due to how variadics are processed, the sentinel value must
   NOT be a literal 0! Only a true NULL (or an explicit NULL pointer,
   e.g. `(void*)0`) will suffice.

   This function places some "reasonable upper limit" on the number of
   arguments to avoid having to allocate non-stack space for them (the
   limit is defined by CWAL_OPT_MAX_FUNC_CALL_ARGS). It returns
   CWAL_RC_RANGE if that limit is exceeded.
*/
int cwal_function_callv( cwal_function * f,
                         cwal_value * self,
                         cwal_value ** resultVal,
                         va_list args );

/**
   Equivalent to cwal_function_callv() but takes its arguments
   in ellipsis form. BE SURE to read the docs for that function
   regarding the arguments!
*/
int cwal_function_callf( cwal_function * f,
                         cwal_value * self,
                         cwal_value ** resultValue,
                         ... );

/**
   The cwal_function_call_in_scope() counterpart of
   cwal_function_callv(). See those functions for more details.
*/
int cwal_function_call_in_scopef( cwal_scope * s, 
                                  cwal_function * f,
                                  cwal_value * self,
                                  cwal_value ** resultValue,
                                  ... );

/**
   A cwal_callback_f() helper for certain function-forwarding use
   cases. This function makes a bitwise copy of the given arguments,
   sets its `callee` to f, trims the argv/argc entries of that copy by
   trimArgCount, invokes f with those arguments and the `rv` argument,
   then returns that result. If trimArgCount is greater than
   args->argc, all arguments are elided.

   This forwarding is run in the current scope and does _not_ trigger
   the callback pre/post-call hooks: those hooks are called in
   response to the callback from which the caller of this function is
   acting.
*/
int cwal_function_forward( cwal_function * const f,
                           uint16_t trimArgCount,
                           cwal_callback_args const * args,
                           cwal_value ** rv );
/**
   This exceedingly special-case predicate returns true if the
   cwal_callback_f bound to f is cb, else it returns false.
*/
bool cwal_function_is( cwal_function * const f,
                       cwal_callback_f cb );

/**
   If args is NULL, returns NULL, else it returns the same as passing
   (args->callee, stateTypeID) to cwal_function_state_get().

   @see cwal_function_state_get()
*/
void * cwal_args_state( cwal_callback_args const * args,
                        void const * stateTypeID );

/**
   If f is not NULL and either stateTypeID is NULL or f was created
   with the same stateTypeID as provided in the 2nd argument, then f's
   native state is returned, else NULL is returned.

   Passing a NULL as the 2nd parameter is not recommended, as cwal cannot
   verify the type of the returned pointer. Clients passing NULL are
   assumed to know what they're doing.

   Example usage:

   @code
   // From within a cwal_callback_f implementation...
   MyType * my = (MyType *)cwal_function_state_get(args->callee, MyTypeID);
   if(!my) { ...args->callee was not created with MyTypeID... }
   @endcode

   @see cwal_args_state()
*/
void * cwal_function_state_get( cwal_function * f,
                                void const * stateTypeID );

/**
   Returns a static array of 1001 integers containing the first
   1000 prime numbers (up to and including 7919) followed by a
   trailing 0 (so clients can use that as an iteration controller
   instead of the length).

   Intended to be use for initializing hash tables.
*/
int const * cwal_first_1000_primes(void);

/**
   Creates a new hash table object. These tables can store
   arbitrary cwal_value keys and values and have amortized O(1)
   search, insertion, and removal performance.

   hashSize is the number of elements in the hash, ideally be a
   prime number. It can be changed after creation using
   cwal_hash_resize(), but doing so will require another
   allocation (tisk tisk) to resize the
   table. cwal_first_1000_primes() can be used to get prime
   numbers if you have not got any handy.

   Returns the new hash table on success, NULL on error. It is
   an error if hashSize is 0.
*/
cwal_hash * cwal_new_hash( cwal_engine * e, cwal_size_t hashSize );

/**
   Equivalent to:

   cwal_hash_value(cwal_new_hash(e,hashSize))
*/
cwal_value * cwal_new_hash_value( cwal_engine * e, cwal_size_t hashSize);

/**
   Searches the given hashtable for a key, returning it if found,
   NULL if not found.


   @see cwal_hash_search()
*/
cwal_value * cwal_hash_search_v( cwal_hash * h, cwal_value const * key );

/**
   Equivalent to cwal_hash_search_v() but returns (on a match) a
   cwal_kvp holding the key/value pair. The returned object is owned
   by h and might be invalidated or modified on any change to h, so
   clients must not hold returned kvp wrapper for long.
*/
cwal_kvp * cwal_hash_search_kvp_v( cwal_hash * h, cwal_value const * key );

/**
   Like cwal_hash_search_v() but takes its key in the form of the
   first keyLen bytes of the given key. It will only ever match true
   string keys, not non-string keys which might otherwise compare (via
   cwal_value_compare()) to equivalent.

   Returns NULL if !h or !key. If keyLen is 0 and *key is not then
   the equivalent of strlen(key) is used to find its length.

   @see cwal_hash_search_v()
   @see cwal_hash_search_kvp()
*/
cwal_value * cwal_hash_search( cwal_hash * h, char const * key,
                               cwal_midsize_t keyLen );


/**
   Equivalent to cwal_hash_search() but returns (on a match) a
   cwal_kvp holding the key/value pair, using the first keyLen bytes
   of key as the search key. It only matches String-typed keys.  The
   returned object is owned by h and might be invalidated or modified
   on any change to h, so clients must not hold returned kvp wrapper
   for long. NEVER, EVER change the internals of the returned cwal_kvp
   value, e.g. changing the key or value instances, as that Will Break
   Things.

   @see cwal_hash_search()
*/
cwal_kvp * cwal_hash_search_kvp( cwal_hash * h, char const * key,
                                 cwal_midsize_t keyLen );

/**
   Returns true (non-0) if v is of the concrete type cwal_hash.
*/
bool cwal_value_is_hash( cwal_value const * v );

/**
   If cwal_value_is_hash(v) then this returns the value's
   cwal_hash representation, else it returns NULL.
*/
cwal_hash * cwal_value_get_hash( cwal_value * v );

/**
   Returns the cwal_value part of a cwal_hash value,
   or NULL if !h.
*/
cwal_value * cwal_hash_value( cwal_hash * h );

/**
   Removes all entries from the hashtable. If freeProps is true then
   non-hash properties (those belonging to the object base type) are
   also cleared. After calling this, cwal_hash_entry_count() will be 0
   until new entries are added.  The hashtable size is not modified by
   this routine.
*/
void cwal_hash_clear( cwal_hash * ar, bool freeProps );

    
/**
   Inserts a value into the given hash. 

   Returns 0 on success. If the given key already exists then
   insertion fails and CWAL_RC_ALREADY_EXISTS is returned unless
   allowOverwrite is true (in which case the entry is replaced). If
   allowOverwrite is true but an existing entry is marked with the
   CWAL_VAR_F_CONST flag, CWAL_RC_CONST_VIOLATION is returned.

   On error the key and value acquire no new references.

   This function returns CWAL_RC_IS_VISITING_LIST if called while h's
   hash properties are being iterated over (e.g. via
   cwal_hash_visit_kvp() and friends), as the data structures do not
   support modification during iteration. (Prior to 20191211,
   CWAL_RC_IS_VISITING was returned for this case.)

   The kvpFlags parameter is treated as described for
   cwal_prop_set_with_flags(). When in doubt, pass
   CWAL_VAR_F_PRESERVE.

   ACHTUNG: when overwriting an existing equivalent key, the older key
   is replaced by the newer one. This is to avoid uncomfortable
   questions about the lifetime management of newly-created keys (the
   management of the older/longer-lived ones is simpler!). This means
   that when replacing an entry, the original copy may (depending on
   references) get freed immediately. The moral of this story is:
   don't hold (cwal_value*) to such keys without holding a reference
   to them (and releasing it when done), or they could be invalidated
   (or their memory reused) in the mean time.

   Other error codes:

   - CWAL_RC_TYPE if cwal_prop_key_can() returns false for the given
   key.

   - CWAL_RC_MISUSE if any arguments are invalid.

   - CWAL_RC_DISALLOW_NEW_PROPERTIES: the container has been flaged
   with the CWAL_CONTAINER_DISALLOW_NEW_PROPERTIES flag and key refers
   to a property which is not in the container.

   - CWAL_RC_DISALLOW_PROP_SET: the container has been flaged with the
   CWAL_CONTAINER_DISALLOW_PROP_SET flag. TODO: this is arguable, as
   that flag was initially intended to apply only to object-level
   properties. We might need another flag/code for this, but cannot do
   so without making the same change in s2 (which uses a mix of
   objects and hashes to implement enums with this flag).

   - CWAL_RC_DESTRUCTION_RUNNING: the container is currently being
   destructed. It "should" be impossible for clients to ever trigger
   this.


*/
int cwal_hash_insert_with_flags_v( cwal_hash * h, cwal_value * key, cwal_value * v,
                                   bool allowOverwrite, cwal_flags16_t kvpFlags );

/**
   Equivalent to calling cwal_hash_insert_with_flags_v() with the same arguments,
   passing CWAL_VAR_F_PRESERVE as the final argument.
*/
int cwal_hash_insert_v( cwal_hash * h, cwal_value * key, cwal_value * v,
                        bool allowOverwrite );

/**
   Like cwal_hash_insert_with_flags_v() but takes its key in the form
   of the first keyLen bytes of the given key.

   This routine allocates a new String value for the key (just in
   case there was any doubt about that).
*/
int cwal_hash_insert_with_flags( cwal_hash * h, char const * key,
                                 cwal_midsize_t keyLen,
                                 cwal_value * v, bool allowOverwrite,
                                 cwal_flags16_t kvpFlags );

/**
   Equivalent to calling cwal_hash_insert_with_flags() with the same arguments,
   passing CWAL_VAR_F_PRESERVE as the final argument.
*/
int cwal_hash_insert( cwal_hash * h, char const * key, cwal_midsize_t keyLen,
                      cwal_value * v, bool allowOverwrite );
/**
   Removes the given key from the given hashtable, potentially
   freeing the value (and possibly even the passed-in key,
   depening on ownership conditions).

   Returns 0 on success, CWAL_RC_MISUSE if either argument is
   NULL, or CWAL_RC_NOT_FOUND if the entry is not found.

   This function returns CWAL_RC_IS_VISITING_LIST if called while h's
   hashtable part is being iterated over (e.g. via
   cwal_hash_visit_kvp() and friends), as modifying the hash during
   iteration could potentially lead the memory-related problems. (This
   case was reported as CWAL_RC_IS_VISITING prior to 20191211.)

   Returns CWAL_RC_DISALLOW_PROP_SET if the container has been flagged
   with the CWAL_CONTAINER_DISALLOW_PROP_SET flag.

   Returns CWAL_RC_DESTRUCTION_RUNNING: the container is currently
   being destructed. It "should" be impossible for clients to ever
   trigger this.
*/
int cwal_hash_remove_v( cwal_hash * h, cwal_value * key );

/**
   Like cwal_hash_remove_v(), with the same result codes, but takes
   its key in the form of the first keyLen bytes of the given key. It
   can only match String-type keys, not non-String-type keys which
   might happen to have similar representations. e.g. passing "1" as a
   key will not match an Integer-typed key with the numeric value 1.
*/
int cwal_hash_remove( cwal_hash * h, char const * key, cwal_midsize_t keyLen );

/**
   Returns the number of entries in the given hash,
   or 0 if !h. This is an O(1) operation.
*/
cwal_midsize_t cwal_hash_entry_count(cwal_hash const * h);

/**
   Returns the table size of h, or 0 if !h.
*/
cwal_midsize_t cwal_hash_size( cwal_hash const * h );

/**
   Resizes the table used by h for key/value storage to the new
   size (which most definitely should be a prime number!). On
   success it returns 0 and cwal_hash_size() will return the size
   passed here. This is a no-op (returning 0) if newSize ==
   cwal_hash_size(h).

   On error it returns...

   - CWAL_RC_MISUSE if !h.

   - CWAL_RC_RANGE if !newSize.

   - CWAL_RC_IS_VISITING_LIST if h's hashtable part is currently being
   "visited" (iterated over), as resizing is not legal while that is
   happening. (This case was reported as CWAL_RC_IS_VISITING prior to
   20191211.)

   - CWAL_RC_OOM on an allocation error. If this happens, h is
   left in its previous state (it is not modified). If allocation
   of the new table succeeds, no other ops performed by this call
   which modify h can fail (exception in debug builds:
   lifetime-related assertions are in place which could be
   triggered if any key/value lifetimes appear to have been
   corrupted before this call).
*/
int cwal_hash_resize( cwal_hash * h, cwal_size_t newSize );


/**
   If h's entry count is at least the 2nd value (load*100) percent of
   its table size, the table is resized to be (roughly) within that
   load size. load must be a value greater than 0, and ideally less an
   1.  e.g. 0.80 means an 80% load factor. Unusually large or small
   values may be trimmed to within some min/max range. A value of 0 or
   less will use the library's built-in default.

   Returns:

   - 0 on success (which may mean it did nothing at all).

   - CWAL_RC_OOM if allocation of a larger hash table size fails.

   - CWAL_RC_IS_VISITING_LIST if called while h's hashtable part is
   currently being iterated over or is otherwise locked against
   concurrent modification.

   Results are undefined if h is NULL or otherwise not a valid
   pointer.
*/
int cwal_hash_grow_if_loaded( cwal_hash * h, double load );

/**
   Returns the "next higher" prime number starting at (n+1), where
   "next" is really the next one in a short list of rather arbitrary
   prime numbers.  If n is larger than some value which we internally
   (and almost arbitrarily) define in this code, a value smaller than
   n will be returned. That maximum value is guaranteed to be at least
   as large as the 1000th prime number (7919 resp.
   cwal_first_1000_primes()[999]).

   This function makes no performance guarantees.
*/
cwal_midsize_t cwal_next_prime( cwal_midsize_t n );

/**
   _Moves_ properties from the containter-type value src to the hash
   table dest. overwritePolicy determines how to handle keys which
   dest already contains:

   overwritePolicy<0: keep existing entries and leave the colliding
   key from src in place.

   overwritePolicy==0: trigger CWAL_RC_ALREADY_EXISTS error on key
   collision.

   overwritePolicy>0: overwrite any keys already existing in the hash
   and removes the property from src.

   If a given property, due to the overwrite policy, is not taken then
   it is kept in src, so src need not be empty when this returns unless
   overwritePolicy>0.

   Property-level key/value pair flags, e.g. constness, are not
   retained.

   Sidebar 1: ideally, this routine moves properties in such a way
   that does not require new allocations, but that's currently only
   the case when the build-time CWAL_OBASE_ISA_HASH option is
   disabled.

   Sidebar 2: it is legal for src and dest to be the same cwal_value,
   in which case its object-level properties are moved into its
   hashtable.

   Returns 0 on success or:

   - CWAL_RC_OOM on allocation error.

   - CWAL_RC_ALREADY_EXISTS if overwritePolicy is 0 and a collision is
   found.

   - CWAL_RC_MISUSE if !src or !dest

   - CWAL_RC_TYPE if src is not a container type.

   - CWAL_RC_IS_VISITING if src is currently being visited (the model
   does not support modification during visitation).

   - CWAL_RC_IS_VISITING_LIST if dest's hash properties (as distinct
   from its base object-level properties) are currently being visited
   (the model does not support modification during visitation). (This
   case was reported as CWAL_RC_IS_VISITING prior to 20191211.)

   On error, src's property list may well have been modified so may be
   in an undefined state, with a subset of the properties moved and a
   subset not.
*/
int cwal_hash_take_props( cwal_hash * const dest, cwal_value * const src, int overwritePolicy );


/**
   Similar to cwal_props_visit_kvp() except that it operates on
   the hash table entries of h. See cwal_props_visit_kvp() for the
   semantics of the visitor and its return value.
*/
int cwal_hash_visit_kvp( cwal_hash * h, cwal_kvp_visitor_f f, void * state );

/**
   Equivalent to cwal_props_visit_keys() except that it operates
   on the hash table entries of h, passing each key in the hashtable
   to f (in an indeterminate order).
*/
int cwal_hash_visit_keys( cwal_hash * h, cwal_value_visitor_f f, void * state );

/**
   Equivalent to cwal_props_visit_keys() except that it operates
   on the hash table entries of h, passing each value in the table
   to f (in an indeterminate order).
*/
int cwal_hash_visit_values( cwal_hash * h, cwal_value_visitor_f f, void * state );

/**
   Hashes n bytes of m and returns its hash. It uses an unspecified
   hash algorithm which is not guaranteed to be stable across
   platforms or compilations of this code. It is guaranteed to be
   deterministic within a single compilation of this code.  m must not
   be NULL.
*/
cwal_hash_t cwal_hash_bytes( void const * m, cwal_size_t n );

/**
   Converts v to a string representation and copies it to dest.  dest
   must be valid memory at least *nDest bytes long. On success (*nDest
   is long enough to hold the number and trailing NUL) then *nDest is
   set to the size of the string (minus the trailing NUL) and dest is
   updated with its contents.

   Returns CWAL_RC_OK on success, else:

   CWAL_RC_MISUSE: dest or nDest are NULL.

   CWAL_RC_RANGE: *nDest is not enough to hold the resulting string
   (including terminating NUL). dest is not modified in this case, but
   *nDest is updated to contain the size which would be needed to
   write the full value.

   For normal use cases, a memory length of 30 or less is more
   than sufficient.
*/
int cwal_int_to_cstr( cwal_int_t v, char * dest, cwal_size_t * nDest );

/**
   Functionally identical to cwal_int_to_cstr() but works on a
   double value.

   For normal use cases, a memory length of 128 or less is more
   than sufficient. The largest result i've ever witnessed was
   about 80 bytes long.

   Prior to 20181127, this routine would output "scientific notation"
   for large/precise-enough numbers, but that was an oversight. It now
   always writes in normal decimal form. If it ever tries to write
   
*/
int cwal_double_to_cstr( cwal_double_t v, char * dest, cwal_size_t * nDest );

/**
   Tries to interpret slen bytes of cstr as an integer value,
   optionally prefixed by a '+' or '-' character. On success 0 is
   returned and *dest (if dest is not NULL) will contain the
   parsed value. On error one of the following is returned:

   - CWAL_RC_MISUSE if !slen, !cstr, or !*cstr.

   - CWAL_RC_TYPE if cstr contains any non-numeric characters.

   - CWAL_RC_RANGE if the numeric string is too large for
   cwal_int_t.

   Potential TODOs: hex with leading 0x or 0X, and octal with
   leading 0o.
*/
int cwal_cstr_to_int( char const * cstr, cwal_size_t slen, cwal_int_t * dest );

/**
   Equivalent to cwal_cstr_to_int() but takes a cwal_string value.
   Returns CWAL_RC_MISUSE if !s, else returns as for
   cwal_cstr_to_int().
*/
int cwal_string_to_int( cwal_string const * s, cwal_int_t * dest );

/**
   Behaves as for cwal_cstr_to_int(), but parses an integer or
   literal double (in decimal form) with an optional leading sign.
*/
int cwal_cstr_to_double( char const * cstr, cwal_size_t slen, cwal_double_t * dest );

/**
   The cwal_string counterpart of cwal_cstr_to_double().
*/
int cwal_string_to_double( cwal_string const * s, cwal_double_t * dest );
    
/**
   Compares the two given strings using memcmp() semantics with
   these exceptions:

   if either of len1 or len2 are 0 then the longer of the two
   strings compares de facto (without a string comparison) to
   greater than the other. If both are 0 they are compared
   as equal.

   len1 and len2 MUST point to their respective number of bytes of
   live memory. If they are 0 their corresponding string is not
   touched. i.e. s1 may be NULL only if len1 is 0, and likewise
   for (s2,len2).
*/
int cwal_compare_cstr( char const * s1, cwal_size_t len1,
                       char const * s2, cwal_size_t len2 );

/**
   A cwal_compare_cstr() proxy which compares the given cwal_string
   to the given c-style string.
*/
int cwal_compare_str_cstr( cwal_string const * s1,
                           char const * s2, cwal_size_t len2 );

/**
   Configures e to recycle, at most, n elements for the given
   type.  If the recycle list already contains more than that then
   any extra elements in it are freed by this call. Set it to 0 to
   disable recycling for the given type.

   typeID must be one of:

   CWAL_TYPE_INTEGER, CWAL_TYPE_DOUBLE (see notes below!),
   CWAL_TYPE_OBJECT, CWAL_TYPE_ARRAY, CWAL_TYPE_NATIVE,
   CWAL_TYPE_BUFFER, CWAL_TYPE_KVP, CWAL_TYPE_WEAKREF,
   CWAL_TYPE_STRING (but see below regarding strings).

   Or, as a special case, CWAL_TYPE_UNDEF means to apply this
   change to all of the above-listed types.

   Also note that any built-in constant values are never
   allocated, and so are not recycled via this mechanism.
       
   Returns 0 on succes, CWAL_RC_MISUSE if !e, and CWAL_RC_TYPE if
   typeID is not refer to one of the recyclable types.

   Notes:

   ACHTUNG: As of 20141129, cwal groups the recycling bins by the size
   of the Value type, and that sizing is platform-dependent and
   determined at runtime. It is not possible for clients to determine
   which types are grouped together, which means that this approach to
   configuring the bin sizes is not as useful as it was before that
   change. e.g. it may well be that Hashes and Buffers share the same
   bin as Arrays, making it impossible to size the bins for exactly
   per type (but giving us better recycling overall). THEREFORE...
   the (probably) best approach to using this function is to call it
   in the order of your preferred priority (highest sizes last), so
   that any shared bins will get the highest size. e.g. passing it
   CWAL_TYPE_INTEGER after CWAL_TYPE_DOUBLE will ensure that the
   INTEGER recycle bin size is used on platforms where those types
   have the same size.

   CWAL_TYPE_KVP is an internal type with no cwal_value
   representation. Each key/value pair in an Object requires one
   instance of cwal_kvp, and clients can control that recycling
   level here.

   CWAL_TYPE_WEAKREF is an internal type with no cwal_value
   representation. We do, however, recycle them, if they are
   configured for it.

   CWAL_TYPE_XSTRING and CWAL_TYPE_ZSTRING are equivalent here,
   as those types use the same recycling bin.

   CWAL_TYPE_STRING recycling is comparatively limited because a
   string's size plays a factor in its reusability. When choosing
   strings from the recycling pool, only strings with the same
   approximate length will be considered. This means it is
   possible, depending on usage, to fill up the recycle pool with
   strings of sizes we won't ever recycle. Internally, the library
   pads new string sizes up to some common boundary because doing
   so saves memory (somewhat ironically) by improving recylability
   of strings from exact-fit-only to a close-fit.
*/
int cwal_engine_recycle_max( cwal_engine * e, cwal_type_id type, cwal_size_t n );

/**
   For the given cwal value type ID, this function returns the
   maximum number of values of that type which e is configured to
   keep in its recycle bin. Returns 0 if !e or recycling is
   disabled or not allowed for the given type.

   Example:

   @code
   cwal_size_t const x = cwal_engine_recyle_max_get(e, CWAL_TYPE_OBJECT);
   @endcode
*/
cwal_size_t cwal_engine_recycle_max_get( cwal_engine * e, cwal_type_id type );

/**
   Sets up e's memory chunk recycler. It must currently be called
   before any memory has been placed in that recycler (i.e. during
   engine initialization). config's contents are copied into e, so the
   object need not live longer than this call.

   Returns:

   - 0 on success

   - CWAL_RC_MISUSE if !e or !config.

   - CWAL_RC_OOM if growing the table fails.

   The ability to resize it at runtime is on the TODO list. It's
   actually implemented but completely untested.
*/
int cwal_engine_memchunk_config( cwal_engine * e,
                                 cwal_memchunk_config const * config);

/**
   Runs the type-specific equivalence comparison operation for lhs
   and rhs, using memcmp() semantics: returns 0 if lhs and rhs are
   equivalent, less than 0 if lhs is "less than" rhs, and greater
   than 0 if lhs is "greater than" rhs. Note that many types do
   not have any sort of sensible orderings. This API attempts to
   do something close to ECMAScript, but it does not exactly match
   that.

   If either argument is NULL, NULL sorts to the left.

   The return value is that of the comparison operation. If errCode is
   not NULL then `*errCode` will be set to 0 on success and non-zero
   if propref resolution triggered an error, in which case the
   engine's error state may have more information about the problem.

   Note that this function does not guaranty return values of exactly
   -1, 0, or 1, but may return any (perhaps varying) negative
   resp. positive values.

   If resolvePropref is true then if the lhs/rhs values are proprefs,
   they are resolved before comparison, else they are not. Note that
   such resolution may trigger arbitrary code, so it's important that
   the caller has ensured that the lhs/right have references and are,
   if appropriate vacuum-safe.

   TODO: find the appropriate place to document the cross-type
   comparisons and weird cases like undefined/null.

   Notes:

   - CWAL_TYPE_NULL and CWAL_TYPE_UNDEF compare equivalently to
   any falsy value. (This was not true before 20140614, but no
   known current code was broken by that change.)
*/
int cwal_value_compare_v2( cwal_value * const lhs, cwal_value * const rhs,
                           bool resolvePropref, int * errCode );

/**
   Equivalent to calling cwal_value_compare_v2() with a 3rd argument
   of false and a final argument of NULL.
 */
int cwal_value_compare( cwal_value const * lhs, cwal_value const * rhs );
    
#if 0
/* th1 has something like this... */
int cwal_engine_call_scoped( cwal_engine * e,
                             int (*callback)(cwal_engine *e, void * state1, void * state2) ); 
#endif
/**
   A generic interface for callback functions which act as a
   streaming input source for... well, for whatever.

   The arguments are:

   - state: implementation-specific state needed by the function.

   - n: when called, *n will be the number of bytes the function
   should read and copy to dest. The function MUST NOT copy more than
   *n bytes to dest. Before returning, *n must be set to the number of
   bytes actually copied to dest. If that number is smaller than the
   original *n value, the input is assumed to be completed (thus this
   is not useful with non-blocking readers).

   - dest: the destination memory to copy the data to.

   Must return 0 on success, non-0 on error (preferably a value from
   cwal_rc).

   There may be specific limitations imposed upon implementations
   or extra effort required by clients.  e.g. a text input parser
   may need to take care to accommodate that this routine might
   fetch a partial character from a UTF multi-byte character.
*/
typedef int (*cwal_input_f)( void * state, void * dest, cwal_size_t * n );

/**
   A cwal_input_f() implementation which requires the state argument
   to be a readable (FILE*) handle.
*/
int cwal_input_f_FILE( void * state, void * dest, cwal_size_t * n );

/**
   A generic streaming routine which copies data from a
   cwal_input_f() to a cwal_outpuf_f().

   Reads all data from inF() in chunks of an unspecified size and
   passes them on to outF(). It reads until inF() returns fewer
   bytes than requested. Returns the result of the last call to
   outF() or (only if reading fails) inF(). Returns CWAL_RC_MISUSE
   if inF or ouF are NULL.

   Here is an example which basically does the same thing as the
   cat(1) command on Unix systems:

   @code
   cwal_stream( cwal_input_f_FILE, stdin, cwal_output_f_FILE, stdout );
   @endcode

   Or copy a FILE to a buffer:

   @code
   cwal_buffer myBuf = cwal_buffer_empty;
   cwal_output_buffer_state outState;
   outState.b = &myBuf;
   outState.e = myCwalEngine;
   rc = cwal_stream( cwal_input_f_FILE, stdin, cwal_output_f_buffer, &outState );
   // Note that on error myBuf might be partially populated.
   // Eventually clean up the buffer:
   cwal_buffer_clear(&myBuf);
   @endcode
*/
int cwal_stream( cwal_input_f inF, void * inState,
                 cwal_output_f outF, void * outState );


/**
   Reserves the given amount of memory for the given buffer object.

   If n is 0 then buf->mem is freed and its state is set to
   NULL/0 values.

   If buf->capacity is less than or equal to n then 0 is returned and
   buf is not modified.

   If n is larger than buf->capacity then buf->mem is (re)allocated
   and buf->capacity contains the new length. Newly-allocated bytes
   are filled with zeroes.

   On success 0 is returned. On error non-0 is returned and buf is not
   modified.

   buf->mem is owned by buf and must eventually be freed by passing an
   n value of 0 to this function.

   buf->used is never modified by this function unless n is 0, in which case
   it is reset.

   Example:

   @code
   cwal_buffer buf = cwal_buffer_empty; // VERY IMPORTANT: copy initialization!
   int rc = cwal_buffer_reserve( e, &buf, 1234 );
   ...
   cwal_buffer_reserve( e, &buf, 0 ); // frees the memory
   @endcode
*/
int cwal_buffer_reserve( cwal_engine * e, cwal_buffer * buf, cwal_size_t n );

/**
   Fills all bytes of the given buffer with the given character.
   Returns the number of bytes set (buf->capacity), or 0 if
   !buf or buf has no memory allocated to it.
*/
cwal_size_t cwal_buffer_fill( cwal_buffer * buf, unsigned char c );

/**
   Uses a cwal_input_f() function to buffer input into a
   cwal_buffer.

   dest must be a non-NULL, initialized (though possibly empty)
   cwal_buffer object. Its contents, if any, will be overwritten by
   this function, and any memory it holds might be re-used.

   The src function is called, and passed the state parameter, to
   fetch the input. If it returns non-0, this function returns that
   error code. src() is called, possibly repeatedly, until it reports
   that there is no more data.

   Whether or not this function succeeds, dest still owns any memory
   pointed to by dest->mem, and the client must eventually free it by
   calling cwal_buffer_reserve(dest,0).

   dest->mem might (and possibly will) be (re)allocated by this
   function, so any pointers to it held from before this call might be
   invalidated by this call.
   
   On error non-0 is returned and dest has almost certainly been
   modified but its state must be considered incomplete.

   Errors include:

   - dest or src are NULL (CWAL_RC_MISUSE)

   - Allocation error (CWAL_RC_OOM)

   - src() returns an error code (that code is returned).

   Whether or not the state parameter may be NULL depends on
   the src implementation requirements.

   On success dest will contain the contents read from the input
   source. dest->used will be the length of the read-in data, and
   dest->mem will point to the memory. dest->mem is automatically
   NUL-terminated if this function succeeds, but dest->used does not
   count that terminator. On error the state of dest->mem must be
   considered incomplete, and is not guaranteed to be NUL-terminated.

   Example usage:

   @code
   cwal_buffer buf = cwal_buffer_empty;
   int rc = cwal_buffer_fill_from( engine, &buf, cwal_input_f_FILE,
   stdin );
   if( rc ){
   fprintf(stderr,"Error %d (%s) while filling buffer.\n",
   rc, cwal_rc_cstr(rc));
   cwal_buffer_reserve( engine, &buf, 0 ); // might be partially populated
   return ...;
   }
   ... use the contents via buf->mem ...
   ... clean up the buffer ...
   cwal_buffer_reserve( engine, &buf, 0 );
   @endcode

   To take over ownership of the buffer's memory, do:

   @code
   void * mem = buf.mem;
   buf = cwal_buffer_empty;
   @endcode

   In which case the memory must eventually be passed to cwal_free()
   to free it.

   TODO: add a flag which tells it whether to append or overwrite the
   contents, or add a second form of this function which appends
   rather than overwrites.
*/
int cwal_buffer_fill_from( cwal_engine * e, cwal_buffer * dest, cwal_input_f src, void * state );

/**
   A cwal_buffer_fill_from() proxy which overwrite's dest->mem
   with the contents of the given FILE handler (which must be
   opened for read access).  Returns 0 on success, after which
   dest->mem contains dest->used bytes of content from the input
   source. On error dest may be partially filled.
*/
int cwal_buffer_fill_from_FILE( cwal_engine * e, cwal_buffer * dest, FILE * src );

/**
   Wrapper for cwal_buffer_fill_from_FILE() which gets its input
   from the given file name. As a special case it interprets the
   name "-" as stdin.
*/
int cwal_buffer_fill_from_filename( cwal_engine * e, cwal_buffer * dest, char const * filename );    

/**
   Works just like cwal_buffer_fill_from_filename() except that it
   takes a required length for the filename. This routine uses an
   internal buffer to copy (on the stack) the given name and
   NUL-terminate it at the nameLen'th byte. This is intended to
   help protect against potentially non-NUL-terminated input
   strings, e.g. from X- or Z-strings.

   Returns 0 on success, CWAL_RC_MISUSE if any pointer argument is
   0, and CWAL_RC_RANGE if nameLen is larger than the internal
   name buffer (of "some reasonable size").
*/
int cwal_buffer_fill_from_filename2( cwal_engine * e, cwal_buffer * dest,
                                     char const * filename,
                                     cwal_size_t nameLen);


/**
   Sets the "used" size of b to 0 and NULs the first byte of
   b->mem if b->capacity is greater than 0. DOES NOT deallocate
   any memory.

   Returns the buffer passed to it.

   @see cwal_buffer_reserve()
*/
cwal_buffer * cwal_buffer_reuse( cwal_buffer * const b );


/**
   Swaps left/right's _contents_. It retains (does not swap) the
   left->self/right->self pointers (swapping those _will_ corrupt
   their memory at some point). Results are undefined if (left==right)
   or if either argument is NULL or points to invalid memory.
*/
void cwal_buffer_swap_mem( cwal_buffer * const left, cwal_buffer * const right );

/**
   Similar to cwal_buffer_reserve() except that...

   - It does not free all memory when n==0. Instead it essentially
   makes the memory a length-0, NUL-terminated string.

   - It will try to shrink (realloc) buf's memory if (n<buf->capacity).

   - It sets buf->capacity to (n+1) and buf->used to n. This routine
   allocates one extra byte to ensure that buf is always
   NUL-terminated.

   - On success it always NUL-terminates the buffer at
   offset buf->used.

   Returns 0 on success, CWAL_RC_MISUSE if !buf, CWAL_RC_OOM if
   (re)allocation fails.

   @see cwal_buffer_reserve()
   @see cwal_buffer_clear()
*/
int cwal_buffer_resize( cwal_engine * e, cwal_buffer * buf, cwal_size_t n );


/**
   Convenience equivalent to cwal_buffer_reserve(e, b, 0).
*/
int cwal_buffer_clear( cwal_engine * e, cwal_buffer * b );

/**
   Appends the first n bytes of data to b->mem at position
   b->used, expanding b if necessary. Returns 0 on success. If
   !data then CWAL_RC_MISUSE is returned.  This function
   NUL-terminates b on success.
*/
int cwal_buffer_append( cwal_engine * e, cwal_buffer * b, void const * data, cwal_size_t n );
    
/**
   Appends printf-style formatted bytes to b using
   cwal_printf(). Returns 0 on success.  Always NUL-terminates the
   buffer on success, but that NUL byte does not count against
   b->used's length.

   If it detects an error while appending to the buffer, it resets
   b->used to the length it had before calling this, and
   NUL-terminates b->mem (if not NULL) at that position. i.e. the
   visible effect on the buffer is as if this has not been called.
*/
int cwal_buffer_printf( cwal_engine * e, cwal_buffer * b, char const * fmt, ... );

/**
   Equivalent to cwal_buffer_printf() but takes a va_list instead
   of ellipsis.
*/
int cwal_buffer_printfv( cwal_engine * e, cwal_buffer * b, char const * fmt, va_list );


/**
   A string formatting function similar to Java's
   java.lang.String.format(), with similar formatting rules.  It
   uses a formatting string to describe how to convert its
   arguments to a formatted string, and appends the output to a
   cwal_buffer instance. If fmtLen is negative then cwal_strlen()
   is used to count the length of the format string, else it
   must be the lenght, in bytes, of that string.

   Overview of formatting rules:

   A format specifier has this syntax:

   %N$[flags][[-]width][.precision][type]

   "%%" is interpreted as a single "%" character, not a format
   specifier.

   N = the 1-based argument (argv) index. It is 1-based because
   that is how java.lang.String.format() does it. The argv value
   at that index is expected to be of the type(s) specified by the
   format specifier, or convertible to that type.

   How the width and precision are handled varies by type. TODO:
   document the various behaviours and ensure semantic
   compatibility (or close) with java.lang.String.format().
       
   [type] must one of the following:

   - b: treat the argument as a boolean, evaluate to "true" or
   "false". Width and precision are ignored. (TODO: treat
   width/precision as padding/truncation, as for strings.)
       
   - B: "blobifies" the argument (which must be a Buffer or
   String), encoding it as a series of hex values, two hex
   characters per byte of length. The precision specifies the
   maximum number of byte pairs to output (so the formatted length
   will be twice the precision).
       
   - d, o, x, X: means interpret the result as an integer in
   decimal, octal, hex (lower-case), or hex (upper-case),
   respectively. If a width is specified and starts with a '0'
   then '0' (instead of ' ') is used for left-side padding if the
   number is shorter than the specified width.  Precision is
   ignored(?).

   - f: double value. Width and precision work like cwal_outputf()
   and friends.

   - J: runs the value through cwal_json_output() to convert it to
   a JSON string. The width can be used to specify
   indentation. Positive values indent by that many spaces per
   level, negative values indicate that many hard tabs per
   level. The precision is ignored.
      
   - N, U: interpret the value as "null" or "undefined",
   respectively. Width and precision are ignored.
       
   - p: evaluates to a string in the form TYPE_NAME\@ADDRESS, using
   the hex notation form of the value's address. Width and
   precision are ignored.

   - q: expects a string or NULL value. Replaces single-quote
   characters with two single-quote characters and interpets NULL
   values as "(NULL)" (without the quotes).

   - Q: like 'q' but surrounds string ouput with single quotes and
   interprets NULL values as "NULL" (without the quotes).
       
   - s: string or buffer value. The precision determines the
   maximum length. The width determines the minimum length.  If
   the string is shorter (in bytes!) than the absolute value of
   the width then a positive width will left-pad the string with
   spaces and a negative width will left-pad the string with
   spaces.  FIXME: USE UTF8 CHARS for precision and width!

   - y: evaluates to cwal_value_type_name(argv[theIndex]). Width
   and precision are ignored.

   The flags field may currently only be a '+', which forces
   numeric conversions to include a sign character. This sign
   character does not count against the width/precision.
       
   Anything which is not a format specifier is appended as-is to
   tgt.

   Note that tgt is appended to by this function, so when re-using
   a buffer one may either need to set tgt->used=0 before calling
   this or the caller should copy tgt->used before calling this
   function and treating (tgt->mem + preCallUsed) as the start of the
   output and (tgt->used - preCallUsed) as its length.

   Note that this function might reallocate tgt->mem, so any
   pointers to it may be invalidated.

   Returns 0 on success. On error it returns non-0 and may replace
   the contents of tgt->mem with an error string. It will do this
   for all cases exception invalid arguments being passed to this
   function (CWAL_RC_MISUSE) or an allocation error
   (CWAL_RC_OOM). For all others, on error it writes an error
   message (NUL-terminated) to (tgt->mem + (tgt->used when this
   function was called)).


   TODOs:

   - Refactor this to take a cwal_output_f() instead of a buffer then
     reimplement this function on top of that one.

   - Implement dynamic width and precision via `*` flags, pulling the
     width/precision via the argument(s) which come(s) before the one
     being modified by the given width/precision. This affects all of
     the arg values in the format string, though.
*/
int cwal_buffer_format( cwal_engine * e, cwal_buffer * tgt,
                        char const * fmt, cwal_int_t fmtLen,
                        uint16_t argc, cwal_value * const * const argv);

/**
   Searches the give buffer for byte sequences matching the first
   needleLen bytes of needle with the first replLen bytes of repl.
   needle may not be NULL and needleLen must be greater than 0.
   replLen may be 0 and repl may not be NULL unless replLen is 0.

   needle is expected to be valid UTF8, but repl is not strictly
   required to be. Results are undefined if needle is not valid UTF8
   (e.g. if needle starts part-way through a multi-byte character or
   if needleLen truncates needle part-way through one).

   If limit>0 then that specifies the maximum number of replacements
   to make. If limit is 0 then all matches are replaced.

   If changeCount is not NULL then *changeCount it is set to the
   number of changes made (regardless of success or failure).

   ACHTUNG: this function needs to create a temporary buffer to work
   on and it will (on success) swap out buf's contents with those of
   the working buffer. Thus any pointers to buf->mem held before this
   call will almost certainly (except in a couple rare corner cases)
   be invalidated by this call.

   On success, returns 0 and replaces any matches in the buffer (up to
   the specified limit, if any). On error, buf's contents/state are
   not modified and non-0 is returned:

   - CWAL_RC_OOM if a memory allocation fails.

   - CWAL_RC_MISUSE if any of (e, buf, needle) are NULL or (!repl &&
   replLen>0).

   - CWAL_RC_RANGE if needleLen==0 (replLen may be 0).


   @see cwal_buffer_replace_byte()
*/
int cwal_buffer_replace_str( cwal_engine * e, cwal_buffer * buf,
                             unsigned char const * needle, cwal_size_t needleLen,
                             unsigned char const * repl, cwal_size_t replLen,
                             cwal_size_t limit,
                             cwal_size_t * changeCount);

/**
   Replaces instances of the given needle byte in the given buffer
   with the given repl byte.

   If limit is 0, all matching bytes are replaced, else only the first
   limit matches are replaced.

   If changeCount is not NULL then *changeCount is assigned to the
   number of changes made (regardless of success or failure). As a
   special case, if needle==repl then no changes are made.

   Returns 0 on success or CWAL_RC_MISUSE if either of (e, buf) are
   NULL.

   Unlike cwal_buffer_replace_str(), this function modifies the input
   buffer in-place and does not risk modifying (via reallocation) the
   buf->mem address.

   @see cwal_buffer_replace_str()
*/
int cwal_buffer_replace_byte( cwal_engine * e, cwal_buffer * buf,
                              unsigned char needle, unsigned char repl,
                              cwal_size_t limit,
                              cwal_size_t * changeCount);


/**
   A callback type for use with cwal_json_output_opt which can
   optionally provide type-specific restructuring or converting of
   values for purposes of JSON serialization. It gets called via the
   cwal_json_output() process as follows:

   Each value we want to serialize (minus a couple exceptions: see
   caveats, below) is passed to this function as the 2nd argument. If
   this function cannot process values of that type, or does not
   override their behavior, it must return 0 and either not modify
   `*rv` or set it to `NULL`. If it can process the src value then it
   must assign `*rv` to a value which is to be used in place of
   src. Normally this would be a new object, but it need not be (and
   it may even be src). On conversion error (e.g. OOM) it must
   propagate any error code or (silently) ignore the error by treating
   it as "cannot convert".

   If this function sets `*rv` to anything other than NULL, that value
   is used in place of src for output purposes. If it returns 0 and
   leaves `*rv` alone or sets it to NULL, JSON output will fall back
   to the builtin behavior.

   The value pointed to by `*rv` will be reffed/dereffed before/after
   output, so if `*rv` was a newly-created value, it will be cleaned
   up immediately after outputing it.

   As a special case, if `*rv` is set to a function, it gets called
   with src as its `this`, and the result of that function is used for
   the JSON conversion (with the same caveats and exceptions as if
   `*rv` were set to a non-JSON-able value).

   If the JSON layer cannot output the returned value type then it is
   reffed/unreffed to potentially clean it up and default processing
   is used. Note that the override check is only performed once on a
   given value, not recursively on new values it returns.

   The final argument to this function is the `override.state` member
   of the cwal_json_output_opt value to which this callback was bound.

   Caveats:

   - Implementations must not modify the src object. It is not const
     because many routines (e.g. iteration) require a non-const
     object.

   - If src is a string, integer, or double, and this routine returns
     a value with a different type, invalid JSON might result.

   - Depending on how cwal_json_output() is called, this callback may
     not get called: if cwal_json_output() is passed a built-in
     values, this callback will not/cannot be called because there is
     no cwal_engine associated with such values, so we have no engine
     to pass in here. In effect, this means that built-in constants
     have hard-coded handling which cannot be overridden.

   - IMPORTANT: implementations _MUST NOT_ trigger
     cwal_engine_vacuum(), else they can end up wiping out values
     which are being recursively processed. cwal_engine_sweep() is
     safe as far as the JSON output API is concerned.

   Potential TODO: allow a client to associate any number of these
   with an engine.
*/
typedef int (*cwal_json_override_f)(cwal_engine * e, cwal_value * src,
                                    cwal_value **rv, void * state);

/**
   Client-configurable options for the cwal_json_output() family of
   functions.
*/
struct cwal_json_output_opt{
  /**
     Specifies how to indent (or not) output. The values
     are:

     (0) == no extra indentation.
       
     (-N) == -N TAB character for each level.

     (N) == N SPACES for each level.

     TODO: replace or supplement this with a ((char const *),
     length) pair.
  */
  int indent;
    
  /**
     indentString offers a more flexible indentation method over the
     (older) indent member. cwal_json_output() will use indentString
     instead of indent if indentString.str is not NULL.
  */
  struct {
    /**
       String to use for each level of indentation. The client
       must ensure that these bytes outlive this object or
       behaviour is undefined.
    */
    char const * str;
    /**
       Number of bytes of this.str to use for each level of
       indentation.
    */
    cwal_size_t len;
  } indentString;

  /**
     An optional hook which enables callers to plug in a callback to
     replace or extend the range of JSON-supported types.
   */
  struct {
    /**
       Format override callback. See the typedef's docs for
       details.
    */
    cwal_json_override_f f;

    /**
       Opaque state to pass to this->f.
    */
    void * state;
  } override;

  /**
     Maximum object/array depth to traverse. Traversing deeply can
     be indicative of cycles in the containers, and this value is
     used to figure out when to abort the traversal. If JSON output
     is triggered by this constraint, the result code will be
     CWAL_RC_RANGE.
  */
  unsigned short maxDepth;
    
  /**
     If true, a newline will be added to the end of the generated
     output, else not.
  */
  bool addNewline;

  /**
     If true, a space will be added after the colon operator
     in objects' key/value pairs.
  */
  bool addSpaceAfterColon;

  /**
     If true, a space will be appended after commas in array/object
     lists, else no space will be appended.
  */
  bool addSpaceAfterComma;

  /**
     If set to 1 then objects/arrays containing only a single value
     will not indent an extra level for that value (but will indent
     on subsequent levels if that value contains multiple values).
  */
  bool indentSingleMemberValues;

  /**
     The JSON format allows, but does not require, JSON generators
     to backslash-escape forward slashes. This option enables/disables
     that feature. According to JSON's inventor, Douglas Crockford:

     (quote)
     It is allowed, not required. It is allowed so that JSON can be
     safely embedded in HTML, which can freak out when seeing
     strings containing "</". JSON tolerates "<\/" for this reason.
     (/quote)

     (from an email on 2011-04-08)

     The default value is 0 (because escaped forward slashes are
     just damned ugly).
  */
  bool escapeForwardSlashes;

  /**
     If true, cyclic structures will not cause an error, but will
     instead be replaced by a symbolic (but useless) placeholder
     string indicating which value cycled. Useful primarily for
     debugging, and not for generating usable JSON output.
  */
  bool cyclesAsStrings;

  /**
     If true, Function values will be output as objects, otherwise
     they will trigger a CWAL_RC_TYPE error.
  */
  bool functionsAsObjects;
};
typedef struct cwal_json_output_opt cwal_json_output_opt;

/** @def cwal_json_output_opt_empty_m

    Empty-initialized cwal_json_output_opt object. Example
    usage:

    @code
    struct {
    ...
    cwal_json_output_opt opt;
    } blah = {
    ...
    cwal_json_output_opt_empty_m
    };
    @endcode
*/
#define cwal_json_output_opt_empty_m { 0/*indent*/, \
    {/*indentString*/0,0},                          \
    {/*override*/NULL/*f*/,NULL/*state*/}, \
    15/*maxDepth*/,                               \
    0/*addNewline*/,                            \
    1/*addSpaceAfterColon*/,                    \
    1/*addSpaceAfterComma*/,                    \
    0/*indentSingleMemberValues*/,              \
    0/*escapeForwardSlashes*/,                  \
    0/*cyclesAsStrings*/,                       \
    1/*functionsAsObjects*/                     \
  }

/** @var cwal_json_output_opt_empty

    Empty-initialized cwal_json_output_opt object, intended
    to be used for initializing all client-side cwal_json_output_opt
    objects, e.g.:

    @code
    cwal_json_output_opt opt = cwal_json_output_opt_empty;
    @endcode

    The cwal_json_output_opt_empty_m macro is the equivalent for
    initializing in-struct members of this type.
*/
extern const cwal_json_output_opt cwal_json_output_opt_empty;

/**
   Outputs the given NON-GRAPH value in JSON format (insofar as
   possible) via the given output function. The state argument is
   passed as the first argument to f(). If f() returns non-0,
   output stops and returns that value to the caller. Note that
   f() will be called very often, so it should be relatively
   efficient.
   
   If fmt is NULL some default is used.

   This function is intended for emitting Objects and Arrays, but
   it can also do the immutable types (just don't try to hand them
   off to a downstream client as a valid JSON object).

   Note that conversion to JSON is fundamentally a const
   operation, and the value is not visibly modified, but in order
   for this code to catch cycles it must mark containers it
   visits. (It unmarks each one as it finishes traversing it.)

   Returns 0 on success.

   Returns CWAL_RC_CYCLES_DETECTED if cycles are detected while
   traversing src (TODO: this should be changed to
   CWAL_RC_IS_VISITING, as that is more precise). Returns
   CWAL_RC_RANGE if the maximum output depth level (as specified in
   the fmt argument or its default) is exceeded.
       
   ACHTUNG: this implementation assumes that all cwal_string
   values are UTF8 and may fail in mysterious ways with other
   encodings.
*/
int cwal_json_output( cwal_value * src, cwal_output_f f,
                      void * state, cwal_json_output_opt const * fmt );
/**
   A wrapper around cwal_json_output() which sends the output via
   cwal_output().
*/
int cwal_json_output_engine( cwal_engine * e, cwal_value * src,
                             cwal_json_output_opt const * fmt );
    
/**
   Wrapper around cwal_json_output() which sends its output to the given
   file handle, which must be opened in write/append mode. If fmt is NULL
   some default is used.

   Minor achtung: if fmt is NULL, this function uses a different default
   than cwal_json_output() does, and it forces the addNewline option
   to be set. If you don't want that, pass in a non-NULL fmt object.
*/
int cwal_json_output_FILE( cwal_value * src, FILE * dest,
                           cwal_json_output_opt const * fmt );

/**
   Convenience wrapper around cwal_json_output_FILE(). This function
   does NOT create directories in the given filename/path, and will
   fail if given a name which refers to a non-existing directory.

   The file name "-" is interpreted as stdout.
*/
int cwal_json_output_filename( cwal_value * src, char const * dest,
                               cwal_json_output_opt const * fmt );

/**
   Wrapper around cwal_json_output() which sends its output to the given
   buffer, which must be opened in write/append mode. If fmt is NULL
   some default is used.
*/
int cwal_json_output_buffer( cwal_engine * e, cwal_value * src,
                             cwal_buffer * dest,
                             cwal_json_output_opt const * fmt );

/**
   A class for holding JSON parser information. It is primarily
   intended for finding the nature and position of a parse error.
*/
struct cwal_json_parse_info {
  /**
     1-based line number, used for error reporting.
  */
  cwal_size_t line;
  /**
     0-based column number, used for error reporting.
  */
  cwal_size_t col;
  /**
     Length, in bytes, parsed. On error this will be "very close to"
     the error position.
  */
  cwal_size_t length;
  /**
     Error code of the parse run (0 for no error).
  */
  int errorCode;
};
typedef struct cwal_json_parse_info cwal_json_parse_info;

/**
   Empty-initialized cwal_json_parse_info object.
*/
#define cwal_json_parse_info_empty_m {          \
    1/*line*/,                                  \
    0/*col*/,                                 \
    0/*length*/,                              \
    0/*errorCode*/                            \
  }

/**
   Empty-initialized cwal_json_parse_info object. Should be copied
   by clients when they initialize an instance of this type.
*/
extern const cwal_json_parse_info cwal_json_parse_info_empty;
    
/**
   Parses input from src as a top-level JSON Object/Array value.

   The state parameter has no meaning for this function but is
   passed on to src(), so state must be compatible with the given
   src implementation.

   The pInfo parameter may be NULL. If it is not then its state is
   updated with parsing information, namely the error location.
   It is modified on success and for any parser-level error, but
   its contents on success are not likely to be useful. Likewise,
   its contents are not useful for errors triggered due to invalid
   arguments or during initial setup of the parser. The caller
   should initialize pInfo by copying cwal_json_parse_info_empty
   over it. After this returns, if pInfo->errorCode is not 0, then
   the failure was either during parsing or an allocation failed
   during parsing.

   On success, 0 is returned and *tgt is assigned to the root
   object/array of the tree (it is initially owned by the
   currently active scope). On success *tgt is guaranteed to be
   either of type Object or Array (i.e. _one_ of
   cwal_value_get_object() or cwal_value_get_array() will return
   non-NULL).

   On error non-0 is returned and *tgt is not modified. pInfo
   will, if not NULL, contain the location of the parse error (if
   any).


   ACHTUNG: if the build-time configuration option
   CWAL_ENABLE_JSON_PARSER is set to 0 then the whole family of
   cwal_json_parse() functions returns CWAL_RC_UNSUPPORTED when
   called, but they will do so after doing any normal argument
   validation, so those codes are still valid in such builds.
*/
int cwal_json_parse( cwal_engine * e, cwal_input_f src,
                     void * state, cwal_value ** tgt,
                     cwal_json_parse_info * pInfo );

/**
   Convenience form of cwal_json_parse() which reads its contents
   from the given opened/readable file handle.
*/
int cwal_json_parse_FILE( cwal_engine * e, FILE * src,
                          cwal_value ** tgt,
                          cwal_json_parse_info * pInfo );

/**
   Convenience form of cwal_json_parse() which reads its contents from
   the given file name.

   The file name "-" is interpreted as stdin.
*/
int cwal_json_parse_filename( cwal_engine * e, char const * src,
                              cwal_value ** tgt,
                              cwal_json_parse_info * pInfo );

/**
   Convenience form of cwal_json_parse() which reads its contents
   from (at most) the first len bytes of the given string.
*/
int cwal_json_parse_cstr( cwal_engine * e, char const * src,
                          cwal_size_t len, cwal_value ** tgt,
                          cwal_json_parse_info * pInfo );

/**
   Sets the current trace mask and returns the old mask. mask is
   interpreted as a bitmask of cwal_trace_flags_e values. If mask ==
   -1 then it returns the current mask without setting it, otherwise
   it sets the trace mask to the given value and returns the previous
   value.

   If !e or tracing is disabled at built-time, returns -1.
*/
int32_t cwal_engine_trace_flags( cwal_engine * e, int32_t mask );

/**
   Sets v's prototype value. Both v and prototype must be
   container types (those compatible with cwal_prop_set() and
   friends), and prototype may be NULL.

   If either v or prototype are not a container type, or v is a
   built-in value, CWAL_RC_TYPE is returned.

   If (v==prototype), CWAL_RC_MISUSE is returned.

   If v has the CWAL_CONTAINER_DISALLOW_PROTOTYPE_SET flag,
   CWAL_RC_DISALLOW_PROTOTYPE_SET is returned. (Added 20191210.)

   Returns CWAL_RC_CYCLES_DETECTED if v appears anywhere in the
   given prototype's prototype chain, with the special allowance
   of prototype already being v's prototype (see above).

   If none of the above-listed conditions apply and if prototype is
   already v's prototype then this is a harmless no-op.

   If v already has a different prototype, it is un-ref'd during
   replacement.

   On success, v adds a reference to the prototype object.

   On success, 0 is returned.
*/
int cwal_value_prototype_set( cwal_value * v, cwal_value * prototpe );

/**
   If v is a type capable of having a prototype, its prototype
   (possibly NULL) is returned, otherwise it is equivalent to
   cwal_value_prototype_base_get(e,cwal_value_type_id(v)) is
   returned.
       
   Reminder to self: the engine argument is only required so that
   this can integrate with cwal_prototype_base_get().
*/
cwal_value * cwal_value_prototype_get( cwal_engine * e, cwal_value const * v );

/**
   Maps the given client-specified prototype value to be the
   prototype for new values of type t. This adds a reference to
   proto and moves it to e's top-most scope so that it will live
   as long as e has scopes.

   Returns 0 on success, CWAL_RC_MISUSE if !e, and CWAL_RC_OOM
   if insertion of the prototype mapping could not allocate
   memory.

   All instances of the given type created after this is called
   will, if they are container types (meaning, by extension,
   capable of having a prototype) have proto assigned as their
   prototype as part of their construction process.

   Note that cwal does not assign prototypes by default - this is
   reserved solely for client-side use.

   Results are of course undefined if t is not a valid type ID (e.g.
   cast from an out-of-range integer).

   Potential uses:

   - Mapping common functions, e.g. toString() implementations,
   for types which cannot normally have prototypes (meaning
   non-container types).

   - A central place to plug in client-defined prototypes, such
   that new instances will inherit their prototypes (having had
   this feature would have saved th1ish a bit of code).

   @see cwal_prototype_base_get()
*/
int cwal_prototype_base_set( cwal_engine * e, cwal_type_id t, cwal_value * proto );

/**
   Returns a prototype value set via cwal_prototype_base_set(),
   or NULL if !e or no entry has been set by the client.
*/
cwal_value * cwal_prototype_base_get( cwal_engine * e, cwal_type_id t );

/**
   Returns true (non-0) if v==proto or v has proto in its
   prototype chain. Returns 0 if any argument is NULL.

   Reminder to self: the engine argument is only necessary so that
   this can integrate with cwal_prototype_base_get().
*/
bool cwal_value_derives_from( cwal_engine * e,
                              cwal_value const * v,
                              cwal_value const * proto );

    
/**
   Reparents v into one scope up from e's current scope, if possible
   and necessary in order to keep v alive (it is not moved if it
   already belongs in an older scope.  Returns CWAL_RC_MISUSE if !v or
   if v has no associated cwal_engine, 0 if v is already in a
   top-level scope. This is a no-op for built-in constant values
   (which do not participate in lifetime tracking).
*/
int cwal_value_upscope( cwal_value * v );

/**
   Returns a handle to v's originating cwal_engine, or NULL if !v or v
   is a builtin constant value. Results are undefined if v is not a
   valid, known-alive value.
*/
cwal_engine * cwal_value_engine( cwal_value const * v );

/**
   Returns the current owning scope of v, or NULL if !v.

   Note that this is always 0 for values for which
   cwal_value_is_builtin() returns true.
*/
cwal_scope * cwal_value_scope( cwal_value const * v );


/**
   Possibly reallocates self->list, changing its size. This
   function ensures that self->list has at least n entries. If n
   is 0 then the list is deallocated (but the self object is not),
   BUT THIS DOES NOT DO ANY TYPE-SPECIFIC CLEANUP of the items. If
   n is less than or equal to self->alloced then there are no side
   effects. If n is greater than self->alloced, self->list is
   reallocated and self->alloced is adjusted to be at least n (it
   might be bigger - this function may pre-allocate a larger
   value).

   Passing an n of 0 when self->alloced is 0 is a no-op.

   Newly-allocated slots will be initialized with NUL bytes.
   
   Returns the total number of items allocated for self->list.  On
   success, the value will be equal to or greater than n (in the
   special case of n==0, 0 is returned). Thus a return value smaller
   than n is an error. Note that if n is 0 or self is NULL then 0 is
   returned.

   The return value should be used like this:

   @code
   cwal_size_t const n = number of bytes to allocate;
   if( n > cwal_list_reserve( e, myList, n ) ) { ... error ... }
   // Or the other way around:
   if( cwal_list_reserve( e, myList, n ) < n ) { ... error ... }
   @endcode

   TODO: supplement this with cwal_list_reserve_v2() with
   returns-0-on-success semantics. The current interface is awkward
   but (fortunately) essentially never needed client-side.
*/
cwal_size_t cwal_list_reserve( cwal_engine * e, cwal_list * self, cwal_size_t n );

/**
   Appends a bitwise copy of cp to self->list, expanding the list as
   necessary and adjusting self->count.
       
   Ownership of cp is unchanged by this call. cp may not be NULL.

   Returns 0 on success, CWAL_RC_MISUSE if any argument is NULL,
   or CWAL_RC_OOM on allocation error.
*/
int cwal_list_append( cwal_engine * e, cwal_list * self, void * cp );

/** @typedef typedef int (*cwal_list_visitor_f)(void * p, void * visitorState )
   
    Generic visitor interface for cwal_list lists.  Used by
    cwal_list_visit(). p is the pointer held by that list entry
    and visitorState is the 4th argument passed to
    cwal_list_visit().

    Implementations must return 0 on success. Any other value
    causes looping to stop and that value to be returned, but
    interpration of the value is up to the caller (it might or
    might not be an error, depending on the context). Note that
    client code may use custom values, and is not restricted to
    CWAL_RC_xxx values.
*/
typedef int (*cwal_list_visitor_f)(void * obj, void * visitorState );

/**
   For each item in self->list, visitor(item,visitorState) is called.
   The item is owned by self. The visitor function MUST NOT free the
   item, but may manipulate its contents if application rules do not
   specify otherwise.

   If order is 0 or greater then the list is traversed from start to
   finish, else it is traverse from end to begin.

   Returns 0 on success, non-0 on error.

   If visitor() returns non-0 then looping stops and that code is
   returned.
*/
int cwal_list_visit( cwal_list * self, int order,
                     cwal_list_visitor_f visitor, void * visitorState );

/**
   Works similarly to the visit operation without the _p suffix except
   that the pointer the visitor function gets is a (**) pointing back
   to the entry within this list. That means that callers can assign
   the entry in the list to another value during the traversal process
   (e.g. set it to 0). If shiftIfNulled is true then if the callback
   sets the list's value to 0 then it is removed from the list and
   self->count is adjusted (self->alloced is not changed).
*/
int cwal_list_visit_p( cwal_list * self, int order, char shiftIfNulled,
                       cwal_list_visitor_f visitor, void * visitorState );


/**
   Parses command-line-style arguments into a cwal object tree.

   argc and argv are expected to be values from main() (or
   similar, possibly adjusted to remove argv[0]).

   It expects arguments to be in any of these forms, and any
   number of leading dashes are treated identically:

   -key : Treats key as a boolean with a true value.

   +key : Treats key as a boolean with a false value.

   -key=VAL : Treats VAL as a boolean, double, integer, string, null,
   or undefined (see cwal_value_from_arg()).

   -key= : Treats key as a cwal null (not literal NULL) value.

   +key=val : identical to -key=val. This "should" produce and
   error, but this routine is intentionally lax.

   All such properties are accumulated in the (*tgt).flags Object
   property. The flags object has no prototype, to avoid any
   unintentional property lookups.

   Arguments not starting with a dash or '+' are treated as
   "non-flags" and are accumulated in the (*tgt).nonFlags array
   property.
   
   Each key/value pair is inserted into the (*tgt).flags object.  If a
   given key appears more than once then only the final entry is
   actually stored.

   Any NULL entries in the argument list are skipped over.

   tgt must be either a pointer to NULL or a pointer to a
   client-provided container value. If (NULL==*tgt) then this function
   allocates a new object and on success it stores the new object in
   *tgt (it is owned by the caller). If (NULL!=*tgt) then it is
   assumed to be a properly allocated object. DO NOT pass a pointer to
   unitialized memory, as that will fool this function into thinking
   it is a valid object and Undefined Behaviour will ensue. If *tgt is
   not NULL (i.e. the caller passes in their own target) then this
   routine does not modify the refcount of the passed-in object. If
   this routine allocates *tgt then (on success) the new object is
   given to the caller with a refcount of 0.

   If *tgt is provided by the caller and is an array
   (cwal_value_get_array() returns non-NULL), then each argument in
   the provided argument list is appended as-is to that array.

   On success:

   - 0 is returned.

   - If (*tgt==NULL) then *tgt is assigned to a newly-allocated
   object, owned by the caller, with a refcount of 0. Note that even
   if no arguments are parsed, the object is still created.

   On error:

   - non-0 is returned: CWAL_RC_MISUSE if e or tgt are
   NULL. CWAL_RC_MISUSE if *tgt is not NULL but cwal_props_can(*tgt)
   returns false (i.e. if *tgt is not a property container type).
   CWAL_RC_RANGE if argc is negative.

   - If (*tgt==NULL) then it is not modified.

   - If (*tgt!=NULL) (i.e. the caller provides his own object) then
   it might contain partial results.

   @see cwal_value_from_arg()
*/
int cwal_parse_argv_flags( cwal_engine * e,
                           int argc, char const * const * argv,
                           cwal_value ** tgt );

/**
   A helper function intended for use in implementing utilities
   like cwal_parse_argv_flags(). This function tries to evaluate
   arg as follows:

   - If it looks like a number, return a numeric value. If the
   number-looking value is too large (would overflow during
   conversion), it is treated like a string instead. (Reminder
   to self: that seems to work properly for integers but there
   are likely ranges of doubles for which overflow is not
   properly noticed, in which case the conversion may truncate
   the resuling double-typed value.)

   - If it is "true" or "false", return the equivalent boolean value.

   - If it is NULL or "null", return the special null value.

   - If it is "undefined", return the special undefined value.

   - Else treat it like a string. If it starts and ends in matching
   quotes (single or double), those are removed from the resulting
   string.

   It is up to the caller to cwal_value_ref() the returned value.

   Returns NULL only on allocation error or if !e.

   Prior to 20181107, this function did not attempt to parse args
   which started with leading '+' or '-' as numbers, but it now
   does. If the input represents a massive number that is too large
   for cwal_int, the internal attempt to convert it to an integer will
   fail and this routine will fall back to treating it like a string.

   @see cwal_parse_argv_flags()
*/
cwal_value * cwal_value_from_arg(cwal_engine * e, char const *arg);

/**
   Creates a new weak reference for the given value. The return
   value can be passed to cwal_weakref_value() to find out if the
   value referenced by the cwal_weakref is still valid.

   Returns NULL if !v or on allocation error. If recycling
   is enabled for the CWAL_TYPE_WEAKREF type then this will
   re-use recyclable memory if any is available.

   Results are strictly undefined if v is not valid at the time
   this is called (e.g. if it has already been destroyed and is a
   dangling pointer).

   The caller must eventually pass the returned instance to
   cwal_weakref_free() to clean it up. Note that cwal_weakrefs
   are not owned by scopes, like values are, so they will not be
   pulled out from under the client if a weak ref survives past
   the cwal_scope under which it is created.

   Minor achtung: weak refs are themselves reference-counted, and
   all weak refs to the same value (assuming it really _is_ the
   same value when all weak refs are created) will be the same
   weak ref instance. However, UNLIKE VALUES, they start life with
   a refcount of 1 instead of 0 (a currently-necessary side-effect
   of the sharing). That, however, is an implementation detail
   which clients must not rely on. i.e. the must pass each
   returned value from this function to cwal_weakref_free(), even
   though this function may return the same value multiple times.

   If v is one of the built-in values then this function might return
   a shared cwal_weakref instance, but this is an optimization and
   implementation detail, and clients should not rely on it.

   The above refcounting and sharing is mentioned here primarily
   in case someone happens to notice this function returning
   duplicate pointers and thinks its a bug. It's not a bug, it
   just means that v is one of the special built-in constants or a
   multiply-weak-ref'd value. For built-ins, the weak reference
   will never become invalidated because the built-in values are
   neither allocated nor freed (and thus valid for the life of the
   program).

   @see cwal_weakref_free()
   @see cwal_weakref_value()
   @see cwal_weakref_custom_new()
*/
cwal_weakref * cwal_weakref_new( cwal_value * v );

/**
   If r was created by cwal_weakref_new() and r's value is
   still alive then this function returns it, else it returns
   NULL. Will return NULL after the referenced value has been
   destroyed via the normal value lifetime processes.

   Returns NULL if !r.

   @see cwal_weakref_new()
   @see cwal_weakref_free()
*/
cwal_value * cwal_weakref_value( cwal_weakref * r );

/**
   Frees (or recycles) the memory associated with a weak
   reference created by cwal_weakref_new() or
   cwal_weakref_custom_new(). If the client fails to do so, the
   reference will effectively leak until the engine is cleaned
   up, at which point it will reap the memory of all dangling
   weak references (at which point it becomes illegal for the
   client to try to do so because both the cwal_engine and the
   weak reference are invalid!).

   cwal_engine_recycle_max() can be used to configure the size of
   the weak reference recycling pool by passing CWAL_TYPE_WEAKREF
   as its second parameter.

   @see cwal_weakref_new()
*/
void cwal_weakref_free( cwal_engine * e, cwal_weakref * r );

/**
   Creates a weak reference which "monitors" p. A call to
   cwal_weakref_custom_invalidate(e,p) will "invalidate" any
   weak references pointing to, such that
   cwal_weakref_custom_check() and cwal_weakref_custom_ptr()
   for references to that memory will return NULL.

   Note that this function recycles cwal_weakref instances for
   any given value of p, meaning that this function may return
   the same instance multiple times when passed the same
   parameters. However, it reference counts them and each
   instance should still be treated as unique and passed to
   cwal_weakref_free() when the client is done with it.

   Clients must at some point call
   cwal_weakref_custom_invalidate() to remove any entries they
   "map" via weak references. Ideally they should do this in the
   moment before their native memory is being finalized or
   otherwise unassociated with script-space. If clients do not do
   so then weak references to that memory will (incorrectly)
   still think it is alive because cwal still holds a copy of
   that pointer.

   @see cwal_weakref_custom_invalidate()
   @see cwal_weakref_custom_check()
   @see cwal_weakref_custom_ptr()
*/
cwal_weakref * cwal_weakref_custom_new( cwal_engine * e, void * p );

/**
   "Invalidates" p, in that future calls to
   cwal_weakref_custom_check(e,p) or cwal_weakref_custom_ptr()
   will return NULL.

   Returns 0 (false) if it does not find p in e's weak ref
   mapping or non-0 (true) if it does (and thereby invalidates
   existing weak refs to it).

   @see cwal_weakref_custom_new()
*/
bool cwal_weakref_custom_invalidate( cwal_engine * e, void * p );

/**
   Searches e to see if p is being monitored by weak references
   created via cwal_weakref_custom_new(e,p). If one is found
   then then p is returned, else NULL is returned. Note that a
   call to cwal_weakref_custom_invalidate() "erases" monitored
   pointers, and if p has been passed to it then this function
   will return NULL. This is essentially an O(1) operation (a
   hashtable lookup).
*/
void * cwal_weakref_custom_check( cwal_engine * e, void * p );

/**
   If r was created by cwal_weakref_custom_new() and has not
   been invalidated then this function returns r's native memory
   pointer (of a type known only to whoever created r, if at
   all). Otherwise it returns NULL. This is faster than
   cwal_weakref_custom_check() (O(1) vs. a slower O(1)).
*/
void * cwal_weakref_custom_ptr( cwal_weakref * r );

/**
   Returns true (non-0) if p has been registered as
   weakly-referenced memory with e, else false (0). Note that p
   is intended to be a client-side native memory address or
   cwal_value pointer, and NOT one of the concrete higher-level types
   like cwal_object, nor a cwal_weakref instance.

   p "should" be a const pointer, but some internals disallow
   that (we don't do anything non-consty with it, though). In
   script bindings, however, const pointers are fairly rare
   because bound data are rarely const.
*/
bool cwal_is_weak_referenced( cwal_engine * e, void * p );


/**
   Tokenizes an input string on a given separator. Inputs are:

   - (inp) = is a pointer to the pointer to the start of the input.

   - (separator) = the separator character

   - (end) = a pointer to NULL. i.e. (*end == NULL)

   This function scans *inp for the given separator char or a NULL char.
   Successive separators at the start of *inp are skipped. The effect is
   that, when this function is called in a loop, all neighboring
   separators are ignored. e.g. the string "aa.bb...cc" will tokenize to
   the list (aa,bb,cc) if the separator is '.' and to (aa.,...cc) if the
   separator is 'b'.

   Returns 0 (false) if it finds no token, else non-0 (true).

   Output:

   - (*inp) will be set to the first character of the next token.

   - (*end) will point to the one-past-the-end point of the token.

   If (*inp == *end) then the end of the string has been reached
   without finding a token.

   Post-conditions:

   - (*end == *inp) if no token is found.

   - (*end > *inp) if a token is found.

   It is intolerant of NULL values for (inp, end), and will assert() in
   debug builds if passed NULL as either parameter.

   When looping, one must be sure to re-set the inp and end
   parameters on each iterator. For example:

   @code
   char const * head = "/a/b/c";
   char const * tail = NULL;
   while( cwal_strtok( &inp, '/', &tail ) ) {
   ...
   head = tail;
   tail = NULL;
   }
   @endcode

   If the loop calls 'continue', it must be careful to
   ensure that the parameters are re-set, to avoid an endless
   loop. This can be simplified with a goto:

   @code
   while( cwal_strtok( &head, '/', &tail ) ) {
   if( some condition ) {
   ... do something ...
   ... then fall through ... 
   }
   else {
   ... do something ...
   ... then fall through ... 
   }
   // setup next iteration:
   head = tail;
   tail = NULL;
   }
   @endcode

   or a for loop:

   @code
   for( ; cwal_strtok(&head, '/', &tail);
   head = tail, tail = NULL){
   ...
   }
   @endcode

   TODO: an implementation which takes a UTF8 char separator, or a
   UTF8 string separator (we have the code in th1ish and s2).
*/
bool cwal_strtok( char const ** inp, char separator,
                  char const ** end );

/**
   Returns the first Function in v's prototype chain, including v.
*/
cwal_function * cwal_value_function_part( cwal_engine * e,
                                          cwal_value * v );

/**
   Returns the first Object in v's prototype chain, including v.
*/
cwal_object * cwal_value_object_part( cwal_engine * e,
                                      cwal_value * v );

/**
   Returns the first Array in v's prototype chain, including v.
*/
cwal_array * cwal_value_array_part( cwal_engine * e,
                                    cwal_value * v );
/**
   Returns the first Hash in v's prototype chain, including v.
*/
cwal_hash * cwal_value_hash_part( cwal_engine * e,
                                  cwal_value * v );

/**
   Returns the first Buffer in v's prototype chain, including v.
*/
cwal_buffer * cwal_value_buffer_part( cwal_engine * e,
                                      cwal_value * v );
/**
   Returns the first Exception in v's prototype chain, including v.
*/
cwal_exception * cwal_value_exception_part( cwal_engine * e,
                                            cwal_value * v );

/**
   Returns the first String in v's prototype chain, including v.
*/
cwal_string * cwal_value_string_part( cwal_engine * e,
                                      cwal_value * v );

/**
   If the 3rd parameter is NULL, this returns the first Native
   in v's prototype chain, including v. If the 3rd param
   is not NULL then it returns the first Native in the
   chain with a matching typeID.

   Returns 0 if no match is found.
*/
cwal_native * cwal_value_native_part( cwal_engine * e,
                                      cwal_value * v,
                                      void const * typeID );

/**
   Returns v, or the first value from v's prototype chain which is
   capable of containing properties. Returns 0 if !v or if no
   prototype exists which can hold properties.
*/
cwal_value * cwal_value_container_part( cwal_engine * e, cwal_value * v );

/**
   Installs or removes a callback hook. If h is not NULL, its
   contents are bitwise copied into space owned by e, replacing
   any existing callback hook. If h is NULL, any installed
   callback hook is cleared (with no notification to the hooks!).

   @see cwal_callback_hook
*/
int cwal_callback_hook_set(cwal_engine * e, cwal_callback_hook const * h );


/**
   Dumps e's internalized strings table to e's output channel.  If
   showEntries is true it lists all entries. If includeStrings is not
   0 then strings of that length or less are also output (longer ones
   are not shown). If includeStrings is 0 then the strings are not
   output. Note that the strings are listed in an unspecified order
   (actually orded by (hash page number/hash code), ascending, but
   that's an implementation detail).
*/
void cwal_dump_interned_strings_table( cwal_engine * const e,
                                       bool showEntries,
                                       cwal_size_t includeStrings );
    
/**
   Dumps some allocation-related metrics to e's output channel.
   Intended only for optimization and debugging purposes.
*/
void cwal_dump_allocation_metrics( cwal_engine * const e );


/**
   Marks v as being exempted (or not) from vacuum operations, but
   otherwise does not affect its lifetimes. Values marked as being
   exempted, and any values they contain/reference (which includes all
   array/tuple entries and property/hash table keys and values), will
   be treated as script-visible, named variables for purposes of
   cwal_engine_vacuum() (that is, a vacuum will not destroy them).

   If the 2nd argument is true, the value is marked as vacuum-proof,
   otherwise it is unmarked, making it _potentially_ (based on its
   exactly place in the universe) subject to subsequent vacuuming.

   Returns 0 on success or if v is a built-in value (they are
   inherently vacuum-proof), CWAL_RC_MISUSE if v is 0.

   The intent of this function is only to make internal Values which
   are not accessible via script code and which need to stay
   alive. Such values require a reference (see cwal_value_ref()) and
   to be vacuum-proofed via this function. As of this writing, in the
   whole cwal/th1ish/s2 constellation, only a small handful of values
   are marked as vacuum-proof: (A) cwal's internal list of prototypes
   (only the list, not the prototypes) and (B) a piece of th1sh's and
   s2's internals where it stashes its own non-script visible
   values. Any values reachable via a vacuum-proof container are safe
   from vacuuming, thanks to side-effects of cwal's lifetime
   management.

   Achtung: if v is ever to be made visible to script code, it most
   certainly should be set to NOT vacuum-proof, by passing 0 as this
   function's 2nd argument, or else it won't ever be able to be
   vacuumed up if it gets orphaned (with cycles) in a script. If it
   gets no cycles and all references are gone, it can still be reaped
   immediately or (depending on other conditions) swept up later.

   @see cwal_engine_vacuum()
*/
int cwal_value_make_vacuum_proof( cwal_value * v, bool yes );

/**
   Returns true if v has explicitly been made vacuum-proof using
   cwal_value_make_vacuum_proof() OR if it is a built-in constant
   value, else false. A value which is not explicitly vacuum-proof may still
   be implicitly vacuum-proofed via a container which creates a path
   leading to the value.
*/
bool cwal_value_is_vacuum_proof( cwal_value const * v );

/**
   Adjusts (optional) metrics (only) which keep track of how much
   memory has been allocated by a client for use with cwal. This
   information is generally only useful for debugging and
   reporting purposes, and cannot be used to enforce any sort of
   memory caps.

   The second parameter is the amount of memory to report (for a
   positive value) or recall (a negative value). Clients who wish
   to use this should pass it a positive value when allocating
   memory and a negative value while cleaning up. A realloc may
   require first passing the negative value of the old size
   followed by the positive value of the new size.

   The point of this routine is to allow higher-level clients to
   help account for memory they allocate for use with their cwal
   bindings. e.g. s2 reports the memory allocated for Functions
   this way, so that they can be counted via the engine's metrics.

   Clients should not use this to report changes in memory to
   cwal_buffer instances, as those are handled at the library
   level. Its primary intended use is for tracking allocation
   totals for memory allocated on behalf of client-side types,
   e.g. the C part of a cwal_native value, which cwal knows is
   there (because it holds a (void*) to it), but does not have any
   information regarding its size or semantics.

   If amount is negative and its absolute value is larger than the
   currently declared allocation total, the total is reduced to 0,
   as opposed to underflowing.

   As of 20141214, cwal_malloc() and friends optionally track all
   memory they manage, such that they can report the exact amount of
   memory they've been requested to process. That is part of the
   cwal_memcap_config mechanism, and when it is enabled, any
   cwal_malloc()-allocated memory which clients report here is
   effectively counted double (once by the allocator and once by the
   client) by cwal_dump_allocation_metrics(). No harm done, though,
   other than double counting of that memory in metrics dumps.
*/
void cwal_engine_adjust_client_mem( cwal_engine * e, cwal_int_t amount );

/**
   Sets client-side flags on the container value v.

   A container is defined as: cwal_props_can() returns true for the
   value.

   If v is a container value, its flags are set to the given flags and
   the old flags value is returned.

   If !v or v is not a container then 0 is returned (which is also
   the default flags value.

   Notes about these flags:

   - They are reserved for cwal client use. Whether that means a
   scripting engine on top of cwal or a client above that is up to the
   layer between cwal and the higher-level client.

   - Their interpretation is of course client-dependent.

   - There are only 16 of them. We can't have more without increasing
   the sizeof() for container values.

   - These are independent of flags set via, e.g.
   cwal_prop_set_with_flags_v().

   @see cwal_container_client_flags_get()
   @see cwal_container_flags_get()
*/
cwal_flags16_t cwal_container_client_flags_set( cwal_value * v, cwal_flags16_t flags );

/**
   Gets any flags set using cwal_container_client_flags_set(), or 0 if !v or
   v is-not-a Container type. Note that 0 is also a legal return value
   for a container, and is the default if no flags have been
   explicitly set on the value.

   @see cwal_container_client_flags_set()
   @see cwal_container_flags_set()
*/
cwal_flags16_t cwal_container_client_flags_get( cwal_value const * v );

/**
   Sets a bitmask of values from the cwal_container_flags enum
   as the given value's flags.

   A container is defined as: cwal_props_can() returns true for the
   value.

   If v is a container value, its flags are set to the given flags and
   the old flags value is returned.

   If !v or v is not a container then 0 is returned (which is also
   the default flags value.

   @see cwal_container_flags_get()
   @see cwal_container_client_flags_set()
   @see cwal_container_client_flags_get()
*/
cwal_flags16_t cwal_container_flags_set( cwal_value * v, cwal_flags16_t flags );

/**
   Gets any flags set using cwal_container_flags_set(), or 0 if !v or
   v is-not-a Container type. Note that 0 is also a legal return value
   for a container, and is the default if no flags have been
   explicitly set on the value.

   @see cwal_container_flags_set()
   @see cwal_container_client_flags_set()
*/
cwal_flags16_t cwal_container_flags_get( cwal_value const * v );


/**
   Works like cwal_printfv(), but appends all output to a
   dynamically-allocated string, expanding the string as necessary to
   collect all formatted data. The returned null-terminated string is
   owned by the caller and it must be cleaned up using cwal_free(). If !fmt
   or if the expanded string evaluates to empty, null is returned, not
   a 0-byte string.
*/
char * cwal_printfv_cstr( cwal_engine * e, char const * fmt, va_list vargs );

/**
   Equivalent to cwal_printfv_cstr(), but takes elipsis arguments instead
   of a va_list.
*/
char * cwal_printf_cstr( cwal_engine * e, char const * fmt, ... );

/**
   Returns the value of the CWAL_VERSION_STRING build-time
   configuration macro (from static memory). If the length param is
   not NULL then the length, in bytes, of the string is written in
   (*length).
*/
char const * cwal_version_string(cwal_size_t * length);

/**
   Returns the value of the CWAL_CPPFLAGS build-time
   configuration macro (from static memory). If the length param is
   not NULL then the length, in bytes, of the string is written in
   (*length).
*/
char const * cwal_cppflags(cwal_size_t * length);

/**
   Returns the value of the CWAL_CFLAGS build-time configuration
   configuration macro (from static memory). If the length param is
   not NULL then the length, in bytes, of the string is written in
   (*length).
*/
char const * cwal_cflags(cwal_size_t * length);

/**
   Returns the value of the CWAL_CXXFLAGS build-time
   configuration macro (from static memory). If the length param is
   not NULL then the length, in bytes, of the string is written in
   (*length).
*/
char const * cwal_cxxflags(cwal_size_t * length);

/**
   If e is NULL, returns CWAL_RC_MISUSE, else returns 0 unless the
   cwal APIs have internally flagged e as being "dead". This state
   ONLY happens when conditions which are normally assert()ed in debug
   builds are noticed in non-debug builds. Once this flag has been
   set, then e is in a corrupt state and might have leaked memory to
   avoid touching memory it believes to be corrupted.

   In debug builds, this will ALWAYS return 0 (if e is valid) because
   the conditions which set this flag all assert() in debug builds,
   crashing the app outright.

   If this function returns non-0 for a valid argument, there is
   generically no recovery option other than letting e's memory
   leak and exiting the app, leaving cleanup to the OS. If
   e is passed to cwal_engine_destroy(), that destruction might
   try to step on some of the apparently corrupted memory, so
   the results are undefined.

   Corruption of the type which triggers such conditions are
   essentially always caused by unref'ing cwal_value pointers too many
   times (i.e. after cwal has stuck it in the recycler or freed it).

   TODO: this flag is not currently set for all assertions, but
   it basically needs to be.
*/
int cwal_is_dead(cwal_engine const * e);

/**
   Creates a new "tuple" value with the given length. Tuples are
   basically fixed-length arrays which cannot hold properties.

   Returns NULL if !e or on allocation error.

   The cwal_value_type_id() for tuples is CWAL_TYPE_TUPLE and their
   cwal_type_id_name() is "tuple".

   @see cwal_new_tuple_value()
   @see cwal_tuple_set()
   @see cwal_tuple_get()
   @see cwal_tuple_length()

*/
cwal_tuple * cwal_new_tuple(cwal_engine * e, uint16_t n);

/**
   Equivalent to passing the result of cwal_new_tuple() to
   cwal_tuple_value().

   @see cwal_new_tuple_value()
*/
cwal_value * cwal_new_tuple_value(cwal_engine * e, uint16_t n);

/**
   Returns the cwal_value part of tp, or NULL if !tp.

   @see cwal_new_tuple()
*/
cwal_value * cwal_tuple_value(cwal_tuple const *tp);

/**
   Returns the length of the given tuple (the number of slots
   it has for storing elements).

   @see cwal_new_tuple_value()
*/
uint16_t cwal_tuple_length(cwal_tuple const * tp);

/**
   Gets the value stored at the given index in the given
   tuple. Returns NULL if n is out of range (not less than
   cwal_tuple_length()) or if that slot has no value.

   If resolvePropref is true then this routine resolves
   cwal_propref-typed values, fetching their resolved value, else
   proprefs are returned as-is.

   @see cwal_new_tuple()
   @see cwal_tuple_set()
*/
cwal_value * cwal_tuple_get_v2(cwal_tuple const * tp, uint16_t n,
                               bool resolvePropref);

/**
   Equivalent to calling cwal_tuple_get() with a true third argument.
*/
cwal_value * cwal_tuple_get(cwal_tuple const * tp, uint16_t n);

/**
   Sets the value at the given index in the given tuple. Returns
   CWAL_RC_RANGE if n is out of range (not less than
   cwal_tuple_length()).

   This might (depending on refcounts) destroy any prior item held in
   that slot.

   This function essentially works identically to cwal_array_set_v2(), so
   please see that function for the specifics, especially regarding
   cwal_propref values. The final argument works as documented for
   that function.

   Returns 0 on success, CWAL_RC_RANGE if n is not less than
   cwal_tuple_length(tp), or some other code if resolution of
   cwal_propref values fails. Results are undefined if tp is NULL or
   invalid.

   Note that, unlike cwal_array_set(), this function never has to
   allocate. However: if resolution of cwal_propref-type values
   encounters an error (e.g. cyclic resolution) then that result code
   is returned and the list entry is not updated.

   @see cwal_new_tuple()
   @see cwal_tuple_get()
   @see cwal_tuple_length()
*/
int cwal_tuple_set_v2(cwal_tuple * const tp, uint16_t n, 
                      cwal_value * const v, bool resolvePropref);

/**
   Equivalent to calling cwal_tuple_set_v2() with a true final
   argument.
*/
int cwal_tuple_set(cwal_tuple * const tp, uint16_t n, cwal_value * const v);

/**
   Works identically to cwal_array_visit().

   @see cwal_new_tuple_value()
*/
int cwal_tuple_visit( cwal_tuple * const tp, cwal_value_visitor_f f,
                      void * const state );

/**
   If v was created using cwal_new_tuple_value() or cwal_new_tuple(),
   this function returns its cwal_tuple part, else it returns NULL.

   @see cwal_new_tuple_value()
   @see cwal_new_tuple()
*/
cwal_tuple * cwal_value_get_tuple( cwal_value * v );

/* tuple is not a property container, thus it cannot be a prototype, thus no:
   cwal_tuple * cwal_tuple_part( cwal_engine * e, cwal_value * v );
*/

/**
   A type for making build-time configuration data of the library
   easily available to clients.

   Use cwal_build_info() to get at this info.
*/
struct cwal_build_info_t {

  /* Config sizes... */
  cwal_size_t const size_t_bits;
  cwal_size_t const int_t_bits;
  cwal_size_t const maxStringLength;

  /* Strings... */
  char const * const versionString;
  char const * const cppFlags;
  char const * const cFlags;
  char const * const cxxFlags;

  /* Booleans... */
  char const isJsonParserEnabled;
  char const isDebug;

  /* sizeof()s... */
  struct {
    cwal_size_t builtinValues;
    cwal_size_t cwalValue;        
    cwal_size_t voidPointer;
  } sizeofs;
};
typedef struct cwal_build_info_t cwal_build_info_t;

/**
   Returns the library's shared/static cwal_build_info_t object.
*/
cwal_build_info_t const * cwal_build_info(void);

/**
   Returns the new value (an Object) on success, 0 on error. The only
   error cases are misuse (e is NULL or does not have an active scope)
   or allocation error.

   The new object is returned without any references - the caller
   effectively takes "ownership" (given what that means in this
   framework).

   @see cwal_callback_f_build_info()
*/
cwal_value * cwal_build_info_object(cwal_engine * e);

/**
   A cwal_callback_f() implementation which wraps
   cwal_build_info_object().

   On success, returns 0 and sets *rv to the build info object. On
   error CWAL_RC_OOM is returned.
*/
int cwal_callback_f_build_info(cwal_callback_args const * args, cwal_value ** rv);

/**
   Sets err's state to the given code/string combination, using
   cwal_buffer_printf() formatting.

   If fmt is 0 or !*fmt then any existing error message is reset.

   As a special case, if code==CWAL_RC_OOM, it behaves as if fmt is
   0 to avoid allocating any new memory.

   The e argument is required for its allocator - this function does
   not directly modify e's state, only err's (noting that err may well
   be embedded in e, so indirect modification of e is possible).

   If the 2nd argument is NULL, e's error state is used.

   On success it returns the 3nd argument (NOT 0!). On error it returns
   some other non-0 code.
*/
int cwal_error_setv( cwal_engine * const e, cwal_error * const err,
                     int code, char const * fmt, va_list );

/**
   Elipses counterpart of cwal_error_setv().
*/
int cwal_error_set( cwal_engine * const e, cwal_error * const err,
                    int code, char const * fmt, ... );

/**
   Copies src's error state over to dest, reusing dest's buffer memory
   if possible.

   If src is NULL then e's error state is used, otherwise if dest is NULL
   then e's error state is used. That is, ONE of the 2nd or 3rd
   arguments may be NULL, but not both.

   Returns 0 on success, CWAL_RC_MISUSE if src==dest, CWAL_RC_OOM on
   allocation error.
*/
int cwal_error_copy( cwal_engine * e, cwal_error const * src, cwal_error * dest );

/**
   Resets any any state in err, but keeps any memory in place for
   re-use.
*/
void cwal_error_reset( cwal_error * const err );

/**
   Resets e's error state using cwal_error_reset(). This only
   affects the error state managed via the cwal_error-related
   APIs, not any exceptions.
*/
void cwal_engine_error_reset( cwal_engine * const e );

/**
   Returns a pointer to e's error state object. The pointer is owned
   by e. When other cwal APIs refer to a cwal_engine's "error state,"
   they's referring to this object unless specified otherwise.

   Note that the core library does not actually use this object. It's
   intended as a convenience for downstream code, and was added to the
   API to support decoupling of certain downstream code.
*/
cwal_error * cwal_engine_error( cwal_engine * const e );

#if 0
/* needed? */
/**
   Const-correct counterpart of cwal_engine_error().
*/
cwal_error const * cwal_engine_error_c( cwal_engine const * e );
#endif

/**
   Swaps the state between two one cwal_error objects and then resets
   the error state of the "from" object. This is intended to assist
   in propagating error state up through layers of the API.

   This "uplifts" an error from the 'from' object to the 'to'
   object. After this returns 'to' will contain the prior error state
   of 'from' and 'from' will contain the old error message memory of
   'to'. 'from' will be re-set to the non-error state (its buffer
   memory is kept intact for later reuse, though).

   Results are undefined if either parameter is NULL or either is not
   properly initialized. i.e. neither may refer to uninitialized
   memory. Copying cwal_error_empty at declaration-time is a simple
   way to ensure that instances are cleanly initialized.
*/
void cwal_error_uplift( cwal_error * const from, cwal_error * const to );

/**
   Frees all memory owned by err, but does not free err. The e
   argument is required for its underlying allocator. If err is NULL
   then e's error state is cleared.
*/
void cwal_error_clear( cwal_engine * e, cwal_error * err );

/**
   If err->code is not 0, *msg and *msgLen (if they are not NULL)
   are assigned to the message string and its length, respectively.
   It is legal for the returned *msg value to be NULL, which simply
   indicates that no error string was provided when the error state
   was set.

   Returns err->code.

   If it returns 0 (err has no error state) then it does not modify
   msg or msgLen.
*/
int cwal_error_get( cwal_error const * err, char const ** msg, cwal_size_t * msgLen );

/**
   Works like cwal_error_get(), using e's error state as the source.
*/
int cwal_engine_error_get( cwal_engine const * e, char const ** msg, cwal_size_t * msgLen );

/**
   Takes err's error string and uses it to create a new Exception
   value. If !err, e's error state is used. On success, it returns a
   new Exception with the err->code error code and a "message"
   property containing err's error string. If err->msg is empty, then
   a generic message is created based on err->code.

   If scriptName is not 0 and (*scriptName) then the exception gets a
   "script" string property with that value.  If scriptName is is null
   and `err->script` is not empty, that name is used for the script.

   If line<=0 and err->line and err->column are used. If the resulting
   line number is >0 then the exception gets line/column properties
   holding the line/col values.

   Returns 0 on error (OOM).

   This does not set e's exception state, but it does (on success)
   clear err's error state (so that we can give the string directly to
   cwal instead of copying it). err may or may not still own memory
   buffer memory after this call, so it must (eventually) be cleaned
   up using cwal_error_clear().
*/
cwal_value * cwal_error_exception( cwal_engine * e,
                                   cwal_error * err,
                                   char const * scriptName,
                                   int line, int col );

/**
   Converts err (or e's error state, if err is NULL) to an exception
   value using cwal_error_exception() (see that func for important
   details) then set's e's exception state to that exception.
    
   Like cwal_exception_set(), this function returns CWAL_RC_EXCEPTION
   on success or some other non-0 code if creation of the exception
   fails (generally speaking, probably CWAL_RC_OOM).

   If line<=0 then err->line and err->col are used in place of the given
   line/column parameters.

   If script is 0 and err->script is populated, that value is used
   instead.
*/
int cwal_error_throw( cwal_engine * e, cwal_error * err,
                      char const * script,
                      int line, int col );

/**
   Expects errNo to be a system-level `errno` value and returns
   the closest semantic match (in its opinion) from the cwal_rc_e
   enum. If it does not know the given code it returns dflt.
*/
int cwal_errno_to_cwal_rc(int errNo, int dflt);

/**
   Glob policies for use with cwal_glob_matches_str().
*/
enum cwal_glob_style_e {
/**
   Match glob using conventional case-sensitive wildcard semantics.
*/
CWAL_GLOB_WILDCARD,
/**
   Match glob using conventional case-insensitive wildcard semantics.
*/
CWAL_GLOB_WILDCARD_NOCASE,
/**
   Match glob using SQL LIKE (case-sensitive) semantics.
*/
CWAL_GLOB_LIKE,
/**
   Match glob using SQL LIKE (case-insensitive) semantics.
*/
CWAL_GLOB_LIKE_NOCASE
};

/**
   Return true if the NUL-terminated string z matches the
   NUL-terminated glob pattern zGlob, or false if the pattern does not
   match. Always returns false if either argument is NULL or zGlob is
   empty. Supports a subset of common glob rules, very similar to
   those supported by sqlite3 (via sqlite3_strglob(), from which this
   code derives).

   Globbing rules for policy CWAL_GLOB_WILDCARD and
   CWAL_GLOB_WILDCARD_NOCASE:

   - `*` Matches any sequence of zero or more characters.
   - `?` Matches exactly one character.
   - `[...]` Matches one character from the enclosed list of
     characters.
    - `[^...]` Matches one character not in the enclosed list.

   With the `[...]` and `[^...]` matching, a ']' character can be
   included in the list by making it the first character after '[' or
   '^'.  A range of characters can be specified using '-'.  Example:
   "[a-z]" matches any single lower-case letter.  To match a '-', make
   it the last character in the list.

   Matching rules for policies CWAL_GLOB_LIKE and CWAL_GLOB_LIKE_NOCASE:
   
   - `%` Matches any sequence of zero or more characters.
   - `_` Matches any one character.

   The policy names ending with `_NOCASE` are case-insensitive. The
   others are case-sensitive.

   Matching is UTF-8-aware but case-sensitivity is limited to the
   cases (haha) covered by cwal_utf8_char_tolower().
*/
bool cwal_glob_matches_cstr(const char *zGlob,
                            const char *z,
                            enum cwal_glob_style_e globStyle);


/**
   An enum describing the types of proprefs.
*/
enum cwal_propref_e {
/**
   Indicates a mapping from an object property to a value. i.e.
   key/value pairs suitable for use with cwal_prop_set_v() and
   friends.
*/
CWAL_PROPREF_PROPERTY,
/**
   Indicates a mapping from a list index to a value.  This requires
   that the property key be an integer and the target container be an
   Array. The alias will then remap get/set operations from/to the
   given list/index. Mostly, anyway: getter APIs which return a
   cwal_kvp cannot do such translation because array entries don't use
   KVPs. List getter APIs which return only values will run alias
   processing before returning unless they offer a toggle to disable
   it. Similarly, foreach/visitor-style APIs which visit values will
   resolve proprefs.
*/
CWAL_PROPREF_LIST,
/**
   Indicates a three-way mapping: a container object, a property
   name, and a callback function. In a "get" context the callback
   is called with no arguments. The origiinal container object
   is the "this" (cwal_callback_args::self) of the call. The
   cwal_callback_args::propertyHolder value gets set to... TODO...
   about to add some infrastructure, but need to get this checked
   in while it works :).
*/
CWAL_PROPREF_INTERCEPTOR,
/**
   Tells cwal_new_propref() to apply the following heuristics in order
   to determine the propref type:

   - If the target container is an array (cwal_value_is_array()) and
     the key is an integer value (cwal_value_is_integer()), treat the
     alias as CWAL_PROPREF_LIST. Note that we cannot currently create
     aliases to Tuple entries, but fixing that is on the TODO list.
     Note also that a value inheriting an array is not treated as an
     array for this purpose, but that behavior may change.

   - Else treat it as CWAL_PROPREF_PROPERTY.
*/
CWAL_PROPREF_AUTO
};
typedef enum cwal_propref_e cwal_propref_e;

/**
   Flags for the 2nd argument to cwal_new_propref2().
*/
enum cwal_propref_flags_e {
/**
   Inidicates that the propref is read-only and will trigger an
   error if a "set" operation is attempted on it.
*/
CWAL_PROPREF_F_READONLY = 0x01,
/**
   Indicates that the propref holds "weak" reference to its target
   container. Such references do not impact the lifetime of the target
   in any way, e.g. the target is not rescoped along with the
   propref. If a propref is used after its wrapped container is
   destroyed, an error will be triggered (as opposed to stepping on a
   stale pointer).
*/
CWAL_PROPREF_F_WEAK = 0x02
};


/**
   Creates a new CWAL_TYPE_PROPREF (property reference) value,
   initially owned by the current scope, with a refcount of 0.

   If any argument is invalid or on allocation error, NULL is
   returned.

   The 1st argument specifies the type of reference, as documented for
   the cwal_propref_e enum. If the type is CWAL_PROPREF_LIST and
   either the key is not an integer (cwal_value_is_integer() is false)
   or the container is not an array (cwal_value_is_array() is false)
   then NULL is returned.

   If readOnly is true then trying to set the propref's value via
   cwal_propref_set() will fail with CWAL_RC_ACCESS.

   If the refType is CWAL_PROPREF_INTERCEPTOR then the xtra argument
   must be a function, and CWAL_RC_TYPE is returned if it is not a
   function. The xtra argument is currently ignored for other
   subtypes, but future types might require it.

   The container and key parameters specify the target container and
   property key. CWAL_RC_TYPE is returned the container argument is
   not a container type or the key cannot be used as a key type (per
   cwal_prop_key_can()).

   Proprefs are not full-fledged containers, so may not hold
   properties of their own. They can only be mapped to a fixed set of
   values at initialization time and the mapping cannot be changed
   later. Proprefs may be used as propery keys, but each one will be
   unique, never comparing as equivalent to any other value. Proprefs
   hold a reference to the values passed here and will rescope them as
   needed.

   Proprefs introduce completely new semantics to the engine, the
   high-level specifics of which are documented below...

   Setting a propref as a _value_ in a properties container changes
   how property lookups and modifications are applied, as described
   below.

   The core-most property get/set APIs have special support for
   propref values. _Normally_, if they encounter one, they will act on
   the container/key pair which the propref is proxying. That is, a
   getter API will (usually) fetch the property of the proxied
   container and a setter API will (usually) set the property of the
   proxied container.

   This special get/set handling of proprefs is currently limited to:

   - Object-level properties.

   - Only if a propref is a _value_ in a getter/setter lookup. Keys
     are never proxied.

   - Only if cwal is compiled with CWAL_OBASE_ISA_HASH (adding support
     for the legacy mode is TODO unless that support gets stripped out
     (which is possible)).

   This handling is recursive, e.g. if a proxy somehow triggers code
   which might recurse, it "should" work so long as the same propref
   is not encountered again while the first request is still
   resolving. If that happens, a CWAL_RC_CYCLES_DETECTED error will be
   triggered. Routines which return NULL pointers when they cannot
   find a property will also return NULL for those cases, but the
   cwal_engine's error state will be updated with a description of the
   problem, so clients have the option of checking for such failures
   (via cwal_engine_error_get()) after a search returns NULL.

   Propref proxying currently specifically does not apply to:

   - cwal_unique wrapped values, though that might change.

   - cwal_hash entries. That seems unlikely to change.

   - The various visitor routines will visit the alias, not the
     resolved value.

   Some operations necessarily treat proprefs as opaque values, not
   subject to propref expansion. A non-exhaustive list...

   - Public getter APIs which return cwal_kvp invariably do not
     process proprefs unless they offer a specific option to
     enable it.

   - cwal_props_copy() copies properties as-is without interpolation,
     so will copy proprefs.

   - cwal_prop_take() takes the propref itself, not what it proxies.

   - cwal_prop_unset() and any variant of cwal_prop_set() with a NULL
     value (which conventionally means unset). These remove the propref's
     property, not what it proxies.

   Note that scope-level variables are stored in object-type values
   and have the same propref features and misfeatures of other
   objects.

   @see cwal_new_propref_value()
   @see cwal_new_propref()
   @see cwal_propref_value()
   @see cwal_value_get_propref()
   @see cwal_value_is_propref()
   @see cwal_value_propref_key()
   @see cwal_value_propref_container()
   @see cwal_value_propref_resolve()
   @see cwal_value_propref_value()
*/
int cwal_new_propref2( cwal_propref_e refType,
                       cwal_flags16_t flags,
                       cwal_value * const container,
                       cwal_value * const key,
                       cwal_value * const xtra,
                       cwal_propref ** rv );

/**
    Convenience form of cwal_new_propref2() which works identically
    except that it returns NULL on any sort of error.
*/
cwal_propref * cwal_new_propref( cwal_propref_e refType,
                                 cwal_flags16_t flags,
                                 cwal_value * const container,
                                 cwal_value * const key,
                                 cwal_value * const xtra );

/**
   Equivalent to passing the result of cwal_new_propref() to
   cwal_value_get_propref().
*/
cwal_value * cwal_new_propref_value( cwal_propref_e refType,
                                     cwal_flags16_t flags,
                                     cwal_value * const container,
                                     cwal_value * const key,
                                     cwal_value * const xtra );

/**
   Resolves the property referenced by p and returns it via `*rv`.

   The 2nd argument influences how CWAL_PROPREF_INTERCEPTOR behaves
   and may be NULL. If propertyHolder is NULL, p an interceptor has
   the propref set as its cwal_callback_args::propertyHolder value,
   else it has propertyHolder set as that value. A guideline is that
   if a given context knows which object the propref is directly
   associated with (if any), i.e. the object containing the propref,
   then that object should be passed as the 2nd argument, else NULL is
   a sensible fallback.

   See cwal_new_propref() for many more details about proprefs and
   their resolution.

   On success it returns 0, `*rv` holds the result, which may be NULL.
   On error it may return any conceivable code, as interceptors may
   report arbitrary error codes. If resolution of proprefs is cyclic,
   it returns CWAL_RC_CYCLES_DETECTED but resolution (if interceptors
   are involved) may have side effects before the cycle is disovered.

   As a special case, if the propref is of type CWAL_PROPREF_LIST and
   the property key (an integer) is negative then resolution will fail
   with CWAL_RC_RANGE and the engine's error state is updated with a
   message describing the problem.
*/
int cwal_propref_resolve(cwal_propref const * const p,
                         cwal_value * const propertyHolder, cwal_value ** rv);

/**
   If v is not NULL and was created with cwal_new_propref(), its
   propref part is returned, otherwise NULL is returned.
*/
cwal_propref * cwal_value_get_propref( cwal_value * const v );

/**
   Returns the cwal_value pointer part of p. Passing that value back
   to cwal_value_get_propref() will result in the same p value.
*/
cwal_value * cwal_propref_value(cwal_propref * const p);

/**
   Returns true if v is not NULL and was created with
   cwal_new_propref() or cwal_new_propref2(), else false.
*/
bool cwal_value_is_propref( cwal_value const * const v );

/**
   Returns the property key mapping for the given propref. It cannot
   be modified after initialization.
*/
cwal_value * cwal_propref_key(cwal_propref const * const p);

/**
   Returns the property target container mapping for the given
   propref. It cannot be modified after initialization, but...

   If p is only weakly bound to its container (via passing the
   CWAL_PROPREF_F_WEAK flag to cwal_new_propref2()) then the value may
   be invalidated at essentially any time without p being
   invalidated. After that weak reference is invalidated, this
   function returns NULL.
*/
cwal_value * cwal_propref_container(cwal_propref const * const p);

/**
   Returns true if p was created as read-only, else false.
*/
bool cwal_propref_is_readonly(cwal_propref const * const p);


/**
   Returns the result of setting the value v to propref p's mapped
   container/value. If p maps to an array/index and the index is
   negative, CWAL_RC_RANGE is returned. If propref resolution results
   in cycles, CWAL_RC_CYCLES_DETECTED is returned.
*/
int cwal_propref_set(cwal_propref const * const p, cwal_value * const v);


/* LICENSE

   This software's source code, including accompanying documentation
   and demonstration applications, are licensed under the following
   conditions...

   Certain files are imported from external projects and have their
   own licensing terms. Namely, the JSON_parser.* files. See their
   files for their official licenses, but the summary is "do what you
   want [with them] but leave the license text and copyright in
   place."

   The author (Stephan G. Beal
   [https://wanderinghorse.net/home/stephan/]) explicitly disclaims
   copyright in all jurisdictions which recognize such a
   disclaimer. In such jurisdictions, this software is released into
   the Public Domain.

   In jurisdictions which do not recognize Public Domain property
   (e.g. Germany as of 2011), this software is Copyright (c)
   2011-2021 by Stephan G. Beal, and is released under the terms of
   the MIT License (see below).

   In jurisdictions which recognize Public Domain property, the user
   of this software may choose to accept it either as 1) Public
   Domain, 2) under the conditions of the MIT License (see below), or
   3) under the terms of dual Public Domain/MIT License conditions
   described here, as they choose.

   The MIT License is about as close to Public Domain as a license
   can get, and is described in clear, concise terms at:

   https://en.wikipedia.org/wiki/MIT_License

   The full text of the MIT License follows:

   --
   Copyright (c) 2011-2022 Stephan G. Beal
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
*/

#if defined(__cplusplus)
} /*extern "C"*/
#endif

#endif /* WANDERINGHORSE_NET_CWAL_H_INCLUDED */
