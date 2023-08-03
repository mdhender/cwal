/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/*
  License: same as cwal. See cwal.h resp. libcwal.h for details.
*/
#ifndef NET_WANDERINGHORSE_CWAL_WHCL_H_INCLUDED_
#define NET_WANDERINGHORSE_CWAL_WHCL_H_INCLUDED_
/** @file */ /* for doxygen */

#include "whcl_config.h" /* must come before libcwal.h */
#include "libcwal.h"
#include <stdio.h> /* FILE * */
#include <time.h>

/** @internal
   
    An internal helper macro to help localize OOM error reports (which
    are most often side effects of other issues, rather than being
    real OOM cases).
*/
#define WHCL__WARN_OOM \
  fprintf(stderr,"OOM @ %s:%d\n", __FILE__, __LINE__)

/** @page page_whcl whcl scripting language

   Whcl is the WanderingHorse.net Command Language for the cwal
   scripting engine.

   It is an incomplete experiment.

   Primary properties of the language:

   - "Command-centric," heavily influenced by TCL, but...

   - Uses cwal data types instead of TCL's EIAS. One implication being
     that...

   - "Bare" string tokens are not a thing. Strings are quoted.

   - Intended primarily for unit testing and basic scripting of
     3rd-party libraries. Not intended to be a language for developing
     full-features apps.

   API notes:

   - Symbols starting with `whcl__` (with TWO underscores) are
     internal. Such types are exposed in the public headers primarily
     so that they can facilitate stack allocation of other structs
     which embed them. _Functions_ with such names are exposed only to
     facilitate access from various project files.
*/

#ifdef __cplusplus
extern "C" {
#endif
typedef struct whcl_engine whcl_engine;
typedef struct whcl_scope whcl_scope;
typedef struct whcl_script whcl_script;
typedef struct whcl__stoken whcl__stoken;
typedef struct whcl__stoken_stack whcl__stoken_stack;
typedef struct whcl__estack whcl__estack;
typedef struct whcl__sweep_guard whcl__sweep_guard;
typedef struct whcl__func whcl__func;
typedef struct whcl__strace_entry whcl__strace_entry;

/**
   Numeric type used for counting script line and column numbers, as
   well as maximum token lengths. Note that we aim for a 16-bit type
   to shave a few bytes from oft-used token types. As of this writing
   (20200105), the single largest s2 script (its amalgamated unit
   tests) is right at 5400 lines (size=160kb), so any dreams of
   scripts with more than 64k lines would seem to be... ah... somewhat
   ambitious.
*/
typedef uint16_t whcl_linecol_t;

/**
   "Extended" result codes for functions whose results might pass
   through the cwal APIs. By and large this project uses cwal_rc_e
   result codes.
*/
enum whcl_rc_e {
WHCL_RC_placeholder = CWAL_RC_CLIENT_BEGIN1,
/**
   Used internally by routines which visit lists of keys/values
   to provide a mechanism for aborting traversal early without
   triggering an error.
*/
WHCL_RC_END_EACH_ITERATION,
/**
   Internal code which indicates that the a (sub)script has no more
   commands.
 */
WHCL_RC_NO_COMMANDS,

/** Sentinel. */
WHCL_RC_end,
/**
   Client-thrown exceptions which use their own error codes "should
   not" use codes below this value. Put differently, result codes
   starting at this value are guaranteed not to collide with
   CWAL_RC_xxx and WHCL_RC_xxx codes. Keep in mind that numerous
   CWAL_RC_xxx and WHCL_RC_xxx codes have very specific meanings in
   various contexts, so returning, from cwal/s2-bound client-side
   code, values which just happen to collide with those may confuse s2
   and/or cwal into Doing The Wrong Thing. (Examples: CWAL_RC_OOM is
   used solely to propagate out-of-memory (OOM) conditions, and
   handling of CWAL_RC_RETURN is context-specific.)
*/
WHCL_RC_CLIENT_BEGIN = CWAL_RC_CLIENT_BEGIN2
};

/**
   This type is reserved for future use in passing init options to
   whcl_engine_init.
*/
struct whcl_init_opt {
  int dummy;
};

/** Convenience typedef. */
typedef struct whcl_init_opt whcl_init_opt;

/** Initialized-with-defaults whcl_init_opt structure, intended for
    const-copy initialization. */
#define whcl_init_opt_empty_m {0}

/** Initialized-with-defaults whcl_init_opt structure, intended for
    non-const copy initialization. */
extern const whcl_init_opt whcl_init_opt_empty;

/** @internal

    An internal helper type for swapping an whcl_engine's sweep-guard
    state in and out at certain points.

    The docs for this struct assume only one context: that this is
    used embedded in an whcl_engine struct.
*/
struct whcl__sweep_guard {
  /**
     If greater than 0, whcl_engine_sweep() will neither sweep nor
     vacuum.
  */
  short sweep;
  /**
     If greater than 0, whcl_engine_sweep() will not vacuum, but will
     fall back to sweep mode (if not disabled) instead. This HAS to
     be >0 if the client makes use of any non-script-visible values
     which are not otherwise vacuum-proofed and may be needed at a
     time when script code may trigger whcl_engine_sweep() (see
     cwal_value_make_vacuum_proof()).
  */
  short vacuum;
  /**
     Helps keeps track of when to sweep and vacuum - incremented once
     per call to whcl_engine_sweep().
  */
  short sweepTick;
};

/** @internal

    Empty-initialized whcl__sweep_guard state, intended for const
    initialization.
*/
#define whcl__sweep_guard_empty_m {0,0,0}

/**
   An abstraction layer over cwal_scope so that we can store more
   per-scope metadata. This type's definition is only in the public
   API in order to facilitate testing of low-level internals from
   non-core code. This type must be considered 100% opaque by
   client-side code (and it may one day actually be made so).

   This type is not _directly_ responsible for cwal_value lifetime
   management: that's what cwal_scope does. This type is primarily for
   holding scope-level variables (which are, themselves, owned by a
   cwal_scope, but are not referenced as variable by that scope).

   One convenient way of thinking of the difference between cwal_scope
   and whcl_scope is that the former acts (in whcl) purely as a "GC
   root" whereas the latter is responsible for managing named
   variables. whcl_scope relies very much on cwal_scope, but the
   opposite is not true. Historically, cwal client code has always
   combined the two concepts into one via cwal_scope's var-tracking
   capabilities. whcl _not_ doing so is a bit of a risk factor for the
   engine but it's an option which has been on my mind for several
   years but there was no expedient way to test it until whcl came
   along. whcl takes the approach of using as few GC roots as it can,
   in order to make best use of the vacuum algorithm and do a better
   job of keeping certain pathological cases of non-script-reachable
   cyclic data structures from leaking for any significant length of
   time. (That said, such cases have never shown up organically in
   cwal client code, but they are easy to intentionally construct,
   creating cases where memory growth is continuous and unbounded.)

   It _might_ seem intuitive, to anyone remotely familiar with how
   cwal works (who, me?), that whcl_scope instances need to be pushed
   and popped within the confines of a single cwal_scope
   push/pop. Though "code cleanliness" implies as much, that's not
   actually the case. whcl_scope instances _are_ managed in a stack,
   and must be popped in reverse order of their pushing, but they
   manage their cwal_value-level memory via the top-most
   (a.k.a. "global") cwal_scope instance. That is a stark deparature
   from all cwal client code conventions which predate this change
   (2022-03-20(ish)).
*/
struct whcl_scope {
  /* This scope's partner cwal_scope. */
  cwal_scope cs;

  /**
     Storage for scope-local variables. This member is made
     vacuum-proof so long as this scope owns it, but that flag is
     unset when the owning scope pops, so it is safe for higher-level
     code to do things such as propagate this value through
     cwal_scopes without danger of it staying vacuum-proof longer than
     it should.

     This member is rescoped to the top-most cwal_scope as soon as
     it's created.
  */
  cwal_value * props;

  /**
     A "safe zone" to store values which are currently undergoing
     evaluation and need a reference held to them. NEVER upscope this
     array to a different scope: it is internal-only, owned by this
     scope.
  */
  cwal_array * evalHolder;

  /**
     State for propagating property access metadata through the
     internal machinery.

     A 3-entry tuple for the so-called "dot-op" state. For a property
     access in the form X[Y], index 0 is X, index 1 is Y, and index
     2 _might_ be set to a prototype of X (the object in which Y
     was actually found).

     Routines which do dot-op-like property access set [0] to the
     LHS container. Extreme care must be taken to ensure that this
     does not point to a stale value.

     [1] is set by the property-access operations to the RHS (key)
     part of the operation. Used by assignment and/or the unset op to
     get access to the property name, as opposed to the resolved
     property value which the dot operator evaluates to.
    
     We probably don't need [2] for whcl - it was grabbed as-is from
     s2. Here's what it does in s2, though:

     Used to differentiate between prop access wrt to types which
     should bind as 'this' in calls to props resolved as
     functions. Namely:

     ```
     obj.FUNC() // binds 'this'
     array.NUMBER // does not
     hash # KEY // does not
     ```

     All three set [0] and [1]. Only "this-respecting" ops (namely,
     the DOT op) sets [2].
  */
  cwal_tuple * dotHolder;  

  /**
     Internal flags.
  */
  uint16_t flags;

  /**
     whcl_scope_push() depth level, starting at 1. This is
     semi-independent of the cwal_scope level. It will always be <=
     the cwal scope depth level.
  */
  uint16_t level;
};

/** @internal

   Holds a stack of whcl__stokens.
*/
struct whcl__stoken_stack {
  /**
     The top of the stack.

     Maintained via whcl__stoken_stack_push(),
     whcl__stoken_stack_pop(), and friends.
  */
  whcl__stoken * top;
  /**
     Number of items in the stack.
  */
  int size;
};

/** @internal

   Empty-initialized whcl__stoken_stack structure, intended for
   const-copy initialization.
*/
#define whcl__stoken_stack_empty_m {0,0}

/** @internal
   Internal representation of a stack trace entry.
*/
struct whcl__strace_entry {
  /**
     The next entry "up" (older) in the stack.
  */
  whcl__strace_entry * up;
  /**
     The next entry "down" (newer) in the stack.
  */
  whcl__strace_entry * down;
  /**
     The tokenizer active when this entry is created.
   */
  whcl_script * ct;
  /**
     Active token when this entry is created.
  */
  struct whcl_stoken const * pos;
};

/** @internal

   Empty-initilized whcl__strace_entry object.
*/
#define whcl__strace_entry_empty_m {                \
  NULL/*up*/, NULL/*down*/, NULL/*ct*/, NULL/*pos*/  \
}

/** @internal

   An "evaluation stack," a thin wrapper over two whcl__stoken_stacks,
   intended to simplify swapping the stacks in and out of an s2_engine
   while parsing subscripts. whcl internally uses a distinct stack per
   sub-eval, rather than one massive stack, as it's simply easier to
   manage. When the main eval loop is (re)entered, a new eval stack is
   put on the C stack (a local var of the eval function) and whcl_engine
   is pointed to it so we know which stack is current.
*/
struct whcl__estack{
  /** The value stack. */
  whcl__stoken_stack vals;
  /** The operator stack. */
  whcl__stoken_stack ops;
};

/** @internal
   Empty-initialized whcl__estack structure, intended for const-copy
   initialization.
*/
#define whcl__estack_empty_m {whcl__stoken_stack_empty_m, whcl__stoken_stack_empty_m}

/** @internal
   Empty-initialized whcl__estack structure, intended for copy
   initialization.
*/
extern const whcl__estack whcl__estack_empty;

/**
   The primary state for an whcl interpreter. Its contents are to be
   considered private and for internal use only, but it is not hidden
   from client-side code so that it may be stack allocated.
*/
struct whcl_engine {
  /** The underlying cwal engine. */
  cwal_engine * ec;

  /**
     Current evaluation chain. This gets temporarily swapped out when
     processing subscripts (like script-side functions).
  */
  whcl_script * ct;

  /**
     State for propagating property access metadata through the
     internal machinery.

     A 3-entry tuple for the so-called "dot-op" state. For a property
     access in the form X[Y], index 0 is X, index 1 is Y, and index
     2 _might_ be set to a prototype of X (the object in which Y
     was actually found).

     Routines which do dot-op-like property access set [0] to the
     LHS container. Extreme care must be taken to ensure that this
     does not point to a stale value.

     [1] is set by the property-access operations to the RHS (key)
     part of the operation. Used by assignment and/or the unset op to
     get access to the property name, as opposed to the resolved
     property value which the dot operator evaluates to.
    
     We probably don't need [2] for whcl - it was grabbed as-is from
     s2. Here's what it does in s2, though:

     Used to differentiate between prop access wrt to types which
     should bind as 'this' in calls to props resolved as
     functions. Namely:

     ```
     obj.FUNC() // binds 'this'
     array.NUMBER // does not
     hash # KEY // does not
     ```

     All three set [0] and [1]. Only "this-respecting" ops (namely,
     the DOT op) sets [2].
  */
  cwal_tuple * dotHolder;  

  /**
     If greater than 0, "skip-mode" must be honored by all evaluation
     code. Skip-mode basically means "consume as much as you normally
     would, but have (if possible) no side-effects while doing so."
     That allows us to consume tokens with or without actually
     evaluating the results as we go (the engine pretends to evaluate,
     but uses the 'undefined' value for everything, so it doesn't
     actually allocate any values). This is the basis of short-circuit
     evaluation.
  */
  short skipLevel;
  
  /**
     Internal state related to managing whcl_scope instances.

     Potential TODO: instead of allocating scopes in a single list
     which may require reallocation, allocate them in fixed-sized
     chunks of, say, 10 or 20 scopes per chunk. The management
     overhead would be minimal and would eliminate the "reallocation
     invalidation" problem but would still have the property that a
     popped scope's memory will later be reused for a pushed scope.
  */
  struct {
    /**
       Manages an array of whcl_scope in response to cwal_scope
       push/pop events.

       This array holds sub-arrays, each with whcl__scope_block_size
       entries. Though this list may get reallocated (invalidating its
       pointer), the sub-arrays are never reallocated, the express intent
       being that any given whcl_scope pointer remain valid even if
       the number of scopes changes.

       The engine will grow this list as necessary, but won't shrink
       it until the engine is finalized, at which point the memory is
       (of course) freed.
    */
    whcl_scope ** blocks;

    /**
       Points to memory in this->list for the current whcl_scope. Note
       that it is generally NOT SAFE to keep a pointer to an whcl_scope
       from this->list because any resize of the list can invalidate
       it, but this member is only modified in the routines which
       manage the list's size.
    */
    whcl_scope * current;

    /**
       Top-most scope of the cwal_engine.
    */
    cwal_scope * topCwal;

    /**
       The number of entries reserved in this->blocks.
    */
    uint16_t blockCount;

    /**
       The current whcl_scope depth level.
    */
    uint16_t level;

    /**
       A flag used for passing scope flags through certain internals.
    */
    uint16_t nextFlags;

    /**
       The highest scope depth reached.
    */
    uint16_t maxLevel;
  } scopes;

  /**
     Internal buffer for (un)escaping stuff and variable expansion. It
     is important that this never be used from routines which invoke
     script code, as that would risk recursion modifying this buffer
     while higher-up code is using it.
  */
  cwal_buffer escBuf;

  /**
     Current expression evaluation stack.
  */
  whcl__estack estack;

  /**
     Various commonly-used values which the engine stashes away for
     its own use. Their lifetimes are (mostly) managed via
     this->stash.
  */
  struct {
    /**
       Some sort of container used by whcl_stash_get() and
       whcl_stash_set(). This is where we keep internally-allocated
       Values which must not be garbage-collected. This value is made
       vacuum-proof.
    */
    cwal_hash * stash;
    /** Stores function script names. */
    cwal_value * scriptNames;
    /** The word "argv". */
    cwal_value * keyArgv;
    cwal_value * keyColumn;
    /** The word "__command" */
    cwal_value * keyCommand;
    cwal_value * keyLine;
    /** The word "prototype". */
    cwal_value * keyPrototype;
    cwal_value * keyScript;
    cwal_value * keyStackTrace;
    cwal_value * keyThis;
    cwal_value * keyTypename;
    cwal_value * keyUsing;
    cwal_value * keyNewCtor;
    /** The function installed by whcl__install_command_cb(). */
    cwal_value * commandCb;
    /** "whcl" builtin value */
    cwal_value * vWhcl;
    /** whcl[prototypes] object */
    cwal_value * vProtos;
    /** Bitmask of APIs which have been installed via
        whcl_install_XX()-style functions. */
    uint32_t installAPI;
  } cache;

  /**
     Stack-trace state.
  */
  struct {
    /**
       Stored as an optimization for sizing the target
       array when collecting stack traces.
    */
    uint16_t count;
    /**
       whcl will bail out if this call stack limit is reached,
       under the assumption that infinite recursion is going on. Set
       to 0 to disable the limt.

       Notes:

       - Only script-called functions count for this purpose, not
       calls to Functions (be they script functions or not) called via
       native code (cwal_function_call() and friends).
    */
    uint16_t max;
    /**
       Head of the current stack trace.
    */
    whcl__strace_entry * head;
    /**
       tail of the current stack trace.
    */
    whcl__strace_entry * tail;
  } strace;
  
  struct {
    /**
       Every this-many calls to whcl_engine_sweep() should either sweep or
       vacuum. When 0, sweeping is disabled (generally not a good
       idea).
    */
    short sweepInterval;

    /**
       Every this-many sweep attempts will be replaced by a vaccuum
       instead if this->guard.vacuum is 0.
    */
    short vacuumInterval;

    /**
       Total number of whcl_engine_sweep() calls which led to a sweep
       or a vacuum. Thus the total number of sweeps is this number
       minus this->vacuumTotal. (The totals are combined here because
       it simplifies an internal calculation of when to trigger a
       vacuum.)
    */
    unsigned int sweepTotal;
    /**
       Total number of whcl_engine_sweep() calls which led to a vacuum.
    */
    unsigned int vacuumTotal;
    /**
       An internal counter to disable sweep/vacuum for a bit. Very
       possibly not needed (and not currently used (it's a porting
       artifact)), subject for removal.

       2022-04-09: we might want to move this back into whcl_scope
       now that whcl now uses a "more deeply-scoped" approach to
       lifetimes.
    */
    whcl__sweep_guard guard;
  } sweeper;

  struct {
    /**
       A special-case error reporting flag. Set by whcl_interrupt()
       and recognized by the evaluation engine as meaning it should
       stop evaluation.
    */
    int interrupted;
    /**
       0 = disable exception stacktraces, <0 = unlimited,
       >0 = limit stacktraces to that many entries.
     */
    short stacktraceLimit;
    /**
       If on, all script-side assert calls are emmited to the
       configured output channel.
    */
    bool traceAssert;
    /**
       If on, all script-side affirm calls are emmited to the
       configured output channel.
    */
    bool traceAffirm;
    /**
       If on then script-side `__debug {...}` blocks are retained,
       else they are stripped.
    */
    bool enableDebugBlock;
  } flags;

  /**
     State for various internal recycling bins.
  */
  struct {
    /**
       Recycle bin for stokens.
    */
    whcl__stoken_stack stok;

    /**
       The max number of items to keep in the stok recycler stack.
    */
    int maxSTokens;

    /**
       Recycle bin for script-function state.
    */
    struct {
      /** Head of the recyling list. */
      whcl__func * head;
      /** Current number of entries in the list. */
      uint16_t count;
      /** Max allowed entry count before we stop accepting new
          items into the list, and free them instead. */
      uint16_t max;
      /** Internal metrics: number of times whcl__func_alloc()
          was able to fetch from the recycler. */
      uint32_t hits;
      /** Internal metrics: number of times whcl__func_alloc()
          was not able to fetch from the recycler. */
      uint32_t misses;
    } scriptFuncs;
  } recycler;
  /**
     Holds cwal_outputer instances managed by the whcl_ob_push() family
     of functions.
  */
  cwal_list ob;
  /**
     Holds DLL/module handles so that the interpreter can close them
     when it cleans up.
  */
  cwal_list modules;

  /**
     State related to internals needed in the context of making
     a script-bound function call.
  */
  struct {
    /**
       Used for propagating the currently-being-called script
       func through certain depths of the internals.
    */
    whcl__func * currentScriptFunc;

    /**
       Available for reuse.
    */
    cwal_value * _reuse;

    /**
       Gets set by function calls so that the `using` builtin value
       can resolve.
    */
    cwal_value * currentUsing;
  } fcall;

  /**
     State for use with (as it were) `with` blocks.
   */
  struct {
    /**
       Holds a list of (cwal_value*) corresponding to the `with` block
       objects. This is a cwal_list, instead of an array, because it
       needs to be global and needs to _not_ impact value
       lifetimes. This list owns nothing but its own memory and
       manages no value lifetimes.
    */
    cwal_list holder;
    /**
       The current `with` object. It's held in, and potentially owned
       by, its `with`-scope's evalHolder.
    */
    cwal_value * current;
  } with;
};

/** @def WHCL_DEFAULT_SWEEP_INTERVAL

    The default interval (measured in number of commands) for between
    sweep-ups. This must be set very low in dev builds because it
    uncovers value lifetime misuses early. Low values are dog slow, though,
    so we default to a much higher value in non-dev builds.
*/
#if !defined(WHCL_DEFAULT_SWEEP_INTERVAL)
#  if defined(WHCL_AMALGAMATION_BUILD)
#    define WHCL_DEFAULT_SWEEP_INTERVAL 20
#    define WHCL_DEFAULT_VACUUM_INTERVAL 10
#  else
#    define WHCL_DEFAULT_SWEEP_INTERVAL 1
#    define WHCL_DEFAULT_VACUUM_INTERVAL 20
#  endif
#endif

#define whcl_engine_empty_m { \
  NULL/*ec*/,NULL/*ct*/, \
  NULL/*dotHolder*/,0/*skipLevel*/,    \
  {/*scopes*/ \
    NULL/*blocks*/,NULL/*current*/,NULL/*topCwal*/,\
    0/*blockCount*/,0/*level*/, 0/*nextFlags*/, \
    0/*maxLevel*/                             \
  },                                      \
  /*escBuf*/cwal_buffer_empty_m, \
  whcl__estack_empty_m/*estack*/,                       \
  {/*cache*/\
    NULL/*stash*/, NULL/*scriptNames*/, \
    NULL/*keyArgv*/, NULL/*keyColumn*/,         \
    NULL/*keyCommand*/, NULL/*keyLine*/,          \
    NULL/*keyPrototype*/, NULL/*keyScript*/, \
    NULL/*keyStackTrace*/, \
    NULL/*keyThis*/,NULL/*keyTypename*/, NULL/*keyUsing*/,    \
    NULL/*keyNewCtor*/, \
    NULL/*commandCb*/,NULL/*vWhcl*/,NULL/*vProtos*/,        \
    0/*installAPI*/\
  },      \
  {/*strace*/ \
    0/*count*/,50/*max*/,NULL/*head*/,NULL/*tail*/\
  },                                            \
  {/*sweeper*/\
    WHCL_DEFAULT_SWEEP_INTERVAL/*sweepInterval*/, \
    WHCL_DEFAULT_VACUUM_INTERVAL/*vacuumInterval*/,\
    0/*sweepTotal*/,0/*vacuumTotal*/,               \
    whcl__sweep_guard_empty_m/*guard*/\
  },                                                    \
  {/*flags*/ 0/*interrupted*/,-1/*stacktraceLimit*/,\
    false/*traceAssert*/, false/*traceAffirm*/, \
    false/*enableDebugBlock*/                           \
  },                                                        \
  {/*recycler*/                                               \
    whcl__stoken_stack_empty_m/*stok*/,                       \
    50 /*maxSTokens*/,                                      \
    {/*scriptFuncs*/ 0/*head*/, 0/*count*/, 20 /*max*/,\
      0/*hits*/, 0/*misses*/}                               \
  },                                                          \
  cwal_list_empty_m/*ob*/,cwal_list_empty_m/*modules*/,     \
  {/*fcall*/\
    NULL/*currentScriptFunc*/, NULL/*_reuse*/,\
    NULL/*currentUsing*/\
  },                \
  {/*with*/cwal_list_empty_m/*holder*/,NULL/*current*/} \
}

extern const whcl_engine whcl_engine_empty;


/** @internal

   The number of whcl_scope objects to allocate in each block of
   whcl_engine::scopes::blocks. Scopes managed via whcl_scope_push()
   and whcl_scope_pop() are allocated in blocks with this many scopes
   per block.

   This value is solely internal but is exposed in the public API so
   that whclsh can use it when producing memory metrics output.
   (TODO: move that output into a public member function.)
*/
#define whcl__scope_block_size ((uint16_t)20)


/**
   Initializes an whcl_engine instances and transfers ownership of the
   given cwal_engine to it.

   The final argument is optional - it may be NULL. As of this writing
   the options object is reserved for future use.

   Returns 0 on success, non-0 on failure. Regardless of success or
   failure, the caller is obligated to eventually pass el to
   whcl_engine_finalize() to clean up any resources it owns.

   Achtung: any scopes created by the given cwal_engine up to this
   point will be destroyed by this routine. Thus if clients have set
   any scope-level variables, they will be lost. This is necessary in
   order to get and keep the cwal/whcl scope stacks in sync. (Reminder
   to self: that may not be the case. We can hypothetically just tie
   new whcl scopes to existing cwal scopes in this routine.)
*/
int whcl_engine_init(whcl_engine * const el, cwal_engine * const ec,
                     whcl_init_opt const * opt);

/**
   Shuts down the given WHCL interpreter and cleans up all resources
   owned by it and its associated cwal_engine. It is safe to call this
   whether or not whcl_engine_init() has been called, or to call it
   multiple times (the 2nd and subsequent being no-ops) but el may not
   be NULL.
*/
void whcl_engine_finalize(whcl_engine * const el);

/**
   Returns the cwal_engine associated with the given whcl_engine.
   Ownership of the pointer is not modified.
*/
cwal_engine * whcl_engine_cwal(whcl_engine * const el);

/**
   Evaluates the first len bytes of the given whcl script code, using
   cwal_strlen() if len is negative. If pushScope is true, a new cwal
   scope is pushed before the script is evaluated and popped when it
   is finished. scriptName may be NULL but that's not terribly
   helpful: it "should" be the name of the script, either a filename
   or a virtual name (e.g. "REPL input"). That name is used in any
   error reporting regarding the script. If rv is not NULL then on
   success any result value from the script is propagated back to the
   caller via `*rv` (in the conventional cwal manner, with all
   responsibilities and caveats regarding its lifetime, ownership,
   etc.). If rv is NULL then any result is discarded. On error `*rv`
   is not modified.

   Returns 0 on success and any number of different non-zero codes on
   error.

   @see whcl_eval_buffer()
   @see whcl_eval_buffer_take()
   @see whcl_eval_file()
*/
int whcl_eval_cstr( whcl_engine * const el,
                    bool pushScope,
                    char const * scriptName,
                    char const * src,
                    cwal_int_t len,
                    cwal_value ** rv );

/**
   Proxy for whcl_eval_cstr() which simply passes on the given
   buffer's memory to that function.

   ACHTUNG: it is up to the client to ensure that the buffer is not
   itself using memory which might be indirectly modified via the
   script code being executed. If buf is a script-bound buffer
   instance then it is not generically possible to ensure that but
   there are at least two defensive strategies:

   1) Before evaluating the buffer, _move_ its memory to a temporary
      buffer using cwal_buffer_swap_mem(), and eval _that_
      buffer. After evaluation, use cwal_buffer_swap_mem() to swap the
      memory back and cwal_buffer_clear() the temporary buffer.  The
      end effect is that if the buffer passed in to this routine is
      modified by the script code, any changes made to its buffer
      contents (as opposed to its object-level properties) are
      reverted after the script runs. In case it's not clear: it is
      not safe to evaluate a script if its source code is modified or
      reallocated (possibly to a different address) while evaluation
      is underway.

   2) Make a call-local bitwise copy of the buffer's memory and
      evaluate that copy.

   @see whcl_eval_buffer_take()
   @see whcl_eval_cstr()
*/
int whcl_eval_buffer( whcl_engine * const el,
                      bool pushScope,
                      char const * scriptName,
                      cwal_buffer const * const buf,
                      cwal_value **rv );

/**
   This variant of whcl_eval_buffer() differs only in that if the
   compilation phase succeeds, it takes over src's memory, as
   documented for whcl_compile_buffer_take(). If the compilation phase
   fails, src's memory is kept intact. Either way, the caller may pass
   src to cwal_buffer_clear() after calling this to be sure that the
   memory (if it's still there) is freed.

   @see whcl_eval_buffer()
   @see whcl_eval_file()
   @see whcl_eval_cstr()
 */
int whcl_eval_buffer_take( whcl_engine * const el,
                           bool pushScope,
                           char const * scriptName,
                           cwal_buffer * const src,
                           cwal_value ** rv );


/**
   A proxy for whcl_eval_cstr() which evaluates the give file with one
   additional behavior: if the script propagates a `return` result,
   this function assigns the propagating returned result to `*rv` and
   returns 0.
*/
int whcl_eval_file( whcl_engine * const el,
                    bool pushScope,
                    char const * scriptName,
                    cwal_value ** rv );

/**
   Creates a new script scope, declares the given var name with the
   given value, evals that script and, if `rv` is not NULL, assigns
   `*rv` to the result of that script (which may validly be `NULL`).

   The scriptName, src, and srcLen arguments are interpreted as for
   whcl_eval_cstr().

   Returns 0 on success. On error `*rv` may have been assigned to by
   deeper-level code but will have been invalidated by the error
   handling.

   This can be used to perform script-side initialization of native
   objects.
*/
int whcl_eval_cstr_with_var( whcl_engine * const el,
                             char const * varName,
                             cwal_value * const varValue,
                             char const * scriptName,
                             char const * src, cwal_int_t srcLen,
                             cwal_value **rv );

/**
   Evaluates the given script code in a new scope and stores the
   result of that script in the given container, using the given
   property name (which must be a NUL-terminated string). This is
   intended to simplify installation of small/embedded
   script-implemented functions from C code.

   If srcLen is negative, cwal_strlen() is used to calculate
   src's length.

   If the script triggers a 'return' then this routine captures
   that return'd value as the result.

   Returns 0 on success. On error any given number of CWAL_RC_xxx
   codes could be returned.
*/
int whcl_set_from_script( whcl_engine * const el, char const * src,
                          cwal_int_t srcLen, cwal_value * const addResultTo,
                          char const * const propName );

/**
   Works identically to whcl_set_from_script() except that it takes
   its property name as a cwal_value.
*/
int whcl_set_from_script_v( whcl_engine * const el, char const * src,
                            cwal_int_t srcLen, cwal_value * const addResultTo,
                            cwal_value * const propName );


/**
   Intended to be passed a result code from whcl_eval_cstr() or
   whcl_eval_buffer() (or functionally similar calls). If that result
   code is CWAL_RC_RETURN (indicating that a `return` is propagating)
   then this function assigns the propagating return value to `*rv`,
   assigns `*rc` to 0, and returns true, else it returns false and
   does not modify `*rc` or `*rv`. If it returns true, the propagating
   value is removed from the engine's scope-pop propagation. Its
   ownership is effectively passed to the caller, with the usual
   caveat that it may be a value shared in other places. If the caller
   intends to keep it around, they must apply the usual proper care
   and feeding of cwal_values, e.g. give it a reference point and, if
   necessary, make it "vacuum-proof".

   If this function returns true and the result `*rv` is NULL, this
   indicates a violation of the framework's use of the CWAL_RC_RETURN
   result code: functions returning that value are required to store
   the propagating result in cwal_propagating_set().

   Example usage:

   ```
   cwal_value * rv = NULL;
   int rc = whcl_eval_cstr(el, ..., &rv);
   whcl_check_for_return(el, &rc, &rv);
   ...
   ```
*/
bool whcl_check_for_return(whcl_engine * const el,
                           int * const rc,
                           cwal_value **rv);

/**
   Returns the whcl_engine associated with the given cwal_engine, if
   any. Returns NULL if ec is not managed by an whcl_engine.
   (whcl_engine_init() binds its whcl_engine to that state slot.)
*/
whcl_engine * whcl_engine_from_state( cwal_engine * const ec );

/**
   Returns the whcl_engine associated with args->engine, or NULL
   if the engine in question is not managed by an whcl_engine.
*/
whcl_engine * whcl_engine_from_args( cwal_callback_args const * const args );


/** @internal

    (Mostly) internal debugging tool which dumps out info about v (may
    be NULL), with an optional descriptive message. Expects file, func and
    line to be the __FILE__, __func__ resp.  __LINE__ macros. Use the
    whcl__dump_val() macro to simplify that.

    Reminder: v cannot be const b/c some types go through JSON output,
    which requires non-const so that it can catch cycles.
*/
void whcl__dump_value( cwal_value * const v, char const * msg, char const * file,
                      char const * func, int line );

/**
   Equivalent to whcl__dump_value(V, MSG, __func__, __LINE__).
*/
#define whcl__dump_val(V, MSG) whcl__dump_value((V), (MSG), __FILE__, __func__, __LINE__)

/**
   Sets the error state of el, as for cwal_error_set(), and
   returns that call's result value.

   Returns the error code, not 0, on success! Returns some other
   non-0 code on error.

   This routine takes pains not to allocate any memory, which also
   means not generating an error message, if the given error code is
   CWAL_RC_OOM.
*/
int whcl_err_set(whcl_engine * const el, int code, char const * fmt, ... );

/**
   Resets el's error and exception state to the non-error state,
   including clearing the was-interrupted flag.
*/
void whcl_err_reset(whcl_engine * const el);

/**
   If el or its cwal_engine have any current non-exception error
   state, the code for the error is returned, else 0 is returned.  If
   alsoCheckException is true, the engine has no non-exception error,
   and an exception is currently propagating then this function
   returns CWAL_RC_EXCEPTION.
*/
int whcl_err_has(whcl_engine const * const el, bool alsoCheckException);

/**
   If el's cwal_engine has non-exception error state set, its code is
   returned and, if not NULL, the message string is assigned to `*msg`
   and its length is assigned to `*msgLen` (if not NULL). The message
   bytes are owned by the engine and may be invalidated on any further
   API calls. If no error state is set, returns 0 and the other output
   arguments are not modified.
*/
int whcl_err_get(whcl_engine const * const el, char const ** msg,
                 cwal_size_t * msgLen );

/**
   Triggers an exception in el's cwal_engine.

   If el has an active script then this function uses that script to
   add error location information to the exception.

   One exception (as it were) is if the code is CWAL_RC_OOM, in which
   case the error state is set but no exception is thrown and no
   formatted message is generated because doing would presumably
   require allocating memory.

   If the 2nd argument is 0, it uses a code of CWAL_RC_EXCEPTION
   instead.

   @see whcl_toker_throw()
   @see whcl_toker_err()
*/
int whcl_err_throw(whcl_engine * const el, int code, char const * fmt, ... );

/**
   Adds a persistent value to the interpreter. These are stored, for
   lifetime purposes, under the top-most scope with one reference to
   it, and they are not visible to script code. They will be made
   vacuum-proof so long as they are in the stash.

   This is where clients might store, e.g. references to their custom
   native-side prototype objects (optionally (and preferably), they
   may set them as normal variables, but that is not always feasible).

   key must be NUL-terminated.

   Returns 0 on success.
*/
int whcl_stash_set( whcl_engine * const el, char const * const key, cwal_value * const v );

/**
   Equivalent to whcl_stash_set() but takes its key in the form of a
   Value instance.
*/
int whcl_stash_set_v( whcl_engine * const el, cwal_value * const key, cwal_value * const v );

/**
   Fetches a value set with whcl_stash_set(). Returns NULL if not found,
   if !el, or if (!key || !*key). key must be NUL-terminated.
*/
cwal_value * whcl_stash_get( whcl_engine * const el, char const * const key );

/**
   Identical to whcl_stash_get() but accepts the length of the key, in bytes.
*/
cwal_value * whcl_stash_get2( whcl_engine * const el, char const * const key,
                              cwal_size_t keyLen );

/**
   Identical to whcl_stash_get2() but returns the matching cwal_kvp on a
   match (else NULL).
*/
cwal_kvp * whcl_stash_get2_kvp( whcl_engine * const el, char const * const key,
                                cwal_size_t keyLen);

/**
   Identical to whcl_stash_get() but takes its key in the form of a Value
   instance.
*/
cwal_value * whcl_stash_get_v( whcl_engine * const el, cwal_value const * const key );

/**
   If cwal_value_is_unique() is true for v then this returns the value
   of passing v to cwal_unique_wrapped_get(), else it returns v.

*/
cwal_value * whcl_value_unwrap( cwal_value * const v );

/**
   Const-friendly brother of whcl_value_unwrap().
*/
cwal_value const * whcl_value_unwrap_c( cwal_value const * const v );

/**
   Sends v to the output channel configured for el's cwal_engine. The
   handling is type-dependent and not configurable. Returns 0 on
   success or any of several non-0 codes on error, e.g. propagated
   from JSON conversion routines or the I/O layer.
*/
int whcl_value_output( whcl_engine * const el, cwal_value * const v );

/**
   Tells el to _possibly_ sweep or vacuum, depending on various
   internal state.
*/
void whcl_engine_sweep( whcl_engine * const el );

/**
   Looks for a property in self (if not NULL) or the current scope
   lookup chain. This takes into account numerous special cases,
   including:

   - If self is not NULL and the key is "__prototype" then self's
     prototype value is returned.

   - If self is an array or string and key is an integer, the
     array/char index at that position is returned, or the undefined
     value if the number is out of range.

   If no property is found;

   - If failIfNotFound is true then CWAL_RC_NOT_FOUND is returned and
     an exception is thrown.

   - If failIfNotFound is false then `*rv` (if rv is not NULL) is set
     to NULL and 0 is returned.

   If one is found, `*rv` (if rv is not NULL) is assigned to it and 0
   is returned.
*/
int whcl_lookup_vsym_v(whcl_engine * const el,
                       cwal_value * self,
                       cwal_value const * const key,
                       bool failIfNotFound,
                       cwal_value **rv);

/**
   C-string counterpart of whcl_lookup_vsym_v().
*/
int whcl_lookup_vsym(whcl_engine * const el, cwal_value * const self,
                     char const * key, cwal_int_t keyLen,
                     bool failIfNotFound, cwal_value **rv);

/**
   Returns el's current scope. It _must not_ be modified by client
   code. The only legal operations client code can perform with this
   is to pass it to a public API function which takes a whcl_scope
   argument.

   ***ACHTUNG***: _never_ hold a copy of the returned pointer because
   the pointer may become invalidated by any evaluation of script
   code. Operations like whcl_set_v() and friends do not invalidate
   it, but anything which evaluates script code, invokes a
   script-bound function, or calls whcl_scope_push() may, If a scope
   may need to be referenced across calls to script code, grab the
   scope, extract its level using whcl_scope_level(), run the script
   code, then fetch the scope at that level again using
   whcl_scope_for_level(). More often than not, the end result will be
   the same pointer, but that is not _always_ the case, so _never_
   rely on it.

   @see whcl_scope_parent()
   @see whcl_scope_level()
   @see whcl_scope_for_level()
   @see whcl_scope_cwal()
*/
whcl_scope * whcl_scope_current( whcl_engine const * const el );

/**
   Returns the cwal scope for the given whcl_scope. (There is a
   1-to-1 relationship.) See whcl_scope_current() for important
   pointer lifetime details.
*/
cwal_scope * whcl_scope_cwal(whcl_scope * const s);


/**
   Returns the parent scope of the given scope or the parent of el's
   current scope if s is NULL. If the current scope is the top-most
   scope, the function will return NULL. See whcl_scope_current() for
   an important warning about _not_ holding this pointer for any
   longer than necessary.

   @see whcl_scope_current()
   @see whcl_scope_level()
   @see whcl_scope_for_level()
 */
whcl_scope * whcl_scope_parent( whcl_engine const * const el,
                                 whcl_scope * const s );

/**
   Returns s's scope level, with 1 being the level of the top-most
   ("global") scope.

   @see whcl_scope_current()
   @see whcl_scope_parent()
   @see whcl_scope_for_level()
 */
uint16_t whcl_scope_level(whcl_scope const * const s);

/**
   Returns the scope for the 1-based level number (1 being the
   top-most/global scope) or NULL if level is out of range.

   @see whcl_scope_current()
   @see whcl_scope_parent()
   @see whcl_scope_level()
*/
whcl_scope * whcl_scope_for_level( whcl_engine const * const el,
                                    uint16_t level );

/**
   Searches for the given variable, starting at the given scope (or
   el's current scope if sc is NULL), returning it if found. If not
   found and `searchParents` is true then the search continues as
   follows:

   Each parent scope is checked until a function call scope is hit
   (and searched). If that call scope does not contain the var then it
   skips to the global scope and continues searching there.

   In other words: it will search within the current function call's
   scope and subscopes but will not cross a call scope boundary except
   to skip to the top-most (global) scope.

   If `foundIn` is not NULL and this function returns non-NULL then
   `*foundIn` will be set to the scope in which the var was found. If
   it returns NULL then no match was found and `*foundIn` is not
   modified.
*/
cwal_value * whcl_var_search_v(whcl_engine * const el,
                               whcl_scope * sc,
                               bool searchParents,
                               cwal_value const * const key,
                               whcl_scope ** foundIn );

/**
   C-string counterpart of whcl_var_search_v(). If keyLen is negative
   then cwal_strlen() is used to calculate its length. This function
   must allocate a cwal_value string to perform the search, and simply
   returns NULL if that allocation fails, so it is not possible to
   distinguish a failed search from an OOM condition with this
   function.
*/
cwal_value * whcl_var_search(whcl_engine * const el,
                             whcl_scope * sc,
                             bool searchParents,
                             char const * key,
                             cwal_int_t keyLen,
                             whcl_scope ** foundIn );

/**
   This sets a property on self (if not NULL) or a scope (the one
   containing the given key, or the current scope if not already
   set). Returns 0 on success. If a scope is the target then this
   function behaves like whcl_scope_set_with_flags_v() (namely,
   in that it does not require that the given key be declared
   as a scope variable).
   
   The propertyFlags argument may be any of the CWAL_VAR_F_xxx
   values. In practice it is either 0 or CWAL_VAR_F_CONST.

   This routine takes into account the following special cases:

   - If self is an array and key is an integer>=0 then this sets
     the given array index.

   - If key is "__prototype" and self is capable of having its own
     prototype then the prototype is set.

   - A `v` of NULL unsets the property except in the case of
     `array[integer]` access, in which case it assigns the given index
     to a C NULL. Note that script code cannot see NULL values and
     care must be taken to translate them to cwal_value_undefined()
     for purposes of passing them to script code. If the property does
     not exist, unsetting it is a harmless no-op. If a scope is the
     target, `v` is NULL, and the given var is not found, no error is
     triggered.

   Non-OOM errors are reported via whcl_err_throw(), as opposed to
   whcl_err_set(), as they tend to be constness or type-related
   violations which should arguably not outright kill a script.

   @see whcl_get()
   @see whcl_get_v()
*/
int whcl_set_with_flags_v(whcl_engine * const el,
                          cwal_value * self,
                          cwal_value * const key,
                          cwal_value * const v,
                          uint16_t propertyFlags);

/**
   The C-string variant of whcl_set_with_flags_v(), differing only in
   that it takes its key from the frist keyLen bytes of the given key
   string. If keyLen is negative then cwal_strlen() is used to
   calculate it.
*/
int whcl_set_with_flags(whcl_engine * const el,
                        cwal_value * self,
                        char const * key,
                        cwal_int_t keyLen, 
                        cwal_value * const v,
                        uint16_t propertyFlags);


/**
   Equivalent to calling whcl_set_with_flags_v() with
   a final argument of 0.

   @see whcl_set()
   @see whcl_get()
   @see whcl_get_v()
*/
int whcl_set_v(whcl_engine * const el,
               cwal_value * self,
               cwal_value * const key,
               cwal_value * const v);

/**
   C-string variant of whcl_set_v().

   If keyLen is negative, cwal_strlen() is used
   to calculate the key's length.

   @see whcl_set_v()
   @see whcl_get()
   @see whcl_get_v()
*/
int whcl_set(whcl_engine * const el, cwal_value * self,
             char const * key,
             cwal_int_t keyLen, cwal_value * const v);

/**
   This is equivalent to passing the first 3 arguments to
   `whcl_lookup_vsym_v(..., false, X)` and returning that X.
   Does not set any error state if no entry is found.
   
   @see whcl_lookup_vsym_v()
   @see whcl_get()
*/
cwal_value * whcl_get_v(whcl_engine * const el,
                        cwal_value * self,
                        cwal_value const * const key);

/**
   C-string counterpart of whcl_get_v().
*/
cwal_value * whcl_get(whcl_engine * const el,
                      cwal_value * self,
                      char const * const key,
                      cwal_int_t keyLen);

/**
   Sets the given key/value pain in a scope in the current scope
   chain. If scope is non-NULL, it becomes the target, else el's
   current scope is used. If searchParents is true then any prior
   scopes in the scope chain are checked for that var first and the
   one containing it is where the var is set. If searchParents is
   false, or no prior scope in the chain has the given key, the value
   is set in the given scope.

   The propertyFlags argument may be any of the CWAL_VAR_F_xxx
   values. In practice it is either 0 or CWAL_VAR_F_CONST.

   If v is NULL then the property, if found, gets unset.

   Returns 0 on success. On error an exception is thrown and non-zero
   is returned. It can return any of the numerous codes which are
   documented for cwal_prop_set_with_flags_v() except for
   CWAL_RC_NOT_FOUND (when v is NULL), which this function treats as a
   non-error.

   This function does NOT enforce that the given key has already been
   declared as a variable. If a given context requires that to be the
   case then it needs to check for the var first using
   whcl_var_search() or whcl_var_search_v(). It does, however,
   necessarily search for the given key so that it can (if found) be
   re-set in the the scope which already holds it.

   Potential TODO: switch from an exception to a non-exception error.
*/
int whcl_scope_set_with_flags_v(whcl_engine * const el,
                                whcl_scope * scope,
                                bool searchParents,
                                cwal_value * const key,
                                cwal_value * v,
                                uint16_t propertyFlags);

/**
   Equivalent to calling whcl_scope_set_with_flags_v() with a final
   argument of 0.
*/
int whcl_scope_set_v(whcl_engine * const el,
                     whcl_scope * scope,
                     bool searchParents,
                     cwal_value * const key,
                     cwal_value * const v);

/**
   "Declares" a variable in the given scope, or el's current scope if
   the given scope is NULL. If the given variable is already in the
   scope. this function returns non-0 and updates el's error state
   with an informative message. If isConst is true then the variable
   is creates with the "const" flag. If v is NULL then
   cwal_value_undefined() is used, but v may not be NULL if isConst is
   true.
*/
int whcl_var_decl_v(whcl_engine * const el, whcl_scope * const scope,
                    bool isConst, cwal_value * const key,
                    cwal_value * const v);

/**
   C-string variant of whcl_var_decl_v(), differing only in that it
   takes its property key as a C string. keyLen is the length of the
   key string. If keyLen is negative, cwal_strlen() is used to
   calculate the key's length.
*/
int whcl_var_decl(whcl_engine * const el, whcl_scope * const scope,
                  bool isConst, char const * key, cwal_int_t keyLen,
                  cwal_value * const v);

/**
   Pushes a new whcl scope (as opposed to cwal scope) onto the stack.
   This only returns NULL if allocation fails. Calling this obligates
   the caller call whcl_scope_pop() one time within the same general
   call context. Results are undefined if this rule is not observed.

   Ideally, all whcl client code "should" use whcl_scope_push(),
   rather than cwal_scope_push(), for scope management, but many APIs
   internally use cwal_scope for local lifetime management. When
   the two APIs are used together, they MUST be pushed/popped
   in matching pairs:

   @code
   cwal_scope_push(...);
   whcl_scope_push(...);
   whcl_scope_pop(...);
   cwal_scope_pop(...);
   @endcode

   Or the other way around, with the cwal scope inside the whcl scope,
   but their lifetimes MUST NOT cross:

   @code
   // DON'T do this..
   cwal_scope_push(...);
   whcl_scope_push(...);
   cwal_scope_pop(...);
   whcl_scope_pop(...);
   @endcode

   The returned pointer remains valid until a matching call to
   whcl_scope_pop() is made.

   Variables set via whcl_var_decl_v() and whcl_scope_set_v() and the
   like are set in the context of a whcl_scope, and all variables
   owned by the scope which are not propagated out by the time the
   scope is popped will be cleaned up by the scope (with the caveat
   that its underlying property storage object (see
   whcl_scope_props()) may change ownership/management before the
   scope is popped). cwal-level scopes are the final arbiter of value
   lifetimes: it is possible that a value owned by a whcl scope cannot
   be destroyed due to other references being held to it, but
   destruction (popping) of the cwal-level scope which owns it will
   destroy it.

   @see whcl_scope_pop()
*/
whcl_scope * whcl_scope_push(whcl_engine * const el);

/**
   Pops the given scope from el. The 2nd argument may be NULL, in
   which case the current scope is popped, but passing the scope
   returned from whcl_scope_push() causes this function to assert()
   that the scope push/pop level expectations match, so that usage is
   recommended.

   If the 3rd argument is non-NULL then it ensures that if propagate
   is owned by the current scope then it survives the popping process
   (its final refcount will be the same as it was before this call and
   it is not reparented to a new scope if it's already managed by an
   older scope). For example, if the value passed as the 3rd argument
   is currently stored as a variable in the current scope and that is
   the only reference to it, it will survive the cleanup of those
   variables and be left with no reference count point (but it will
   not be immediately destroyed by the virtue of having been passed as
   the second argument to this function). Such behavior is the basis
   of implementing propagate-on-return semantics.

   This function semantically invalidates the given (or current) scope
   pointer. The engine is free to keep that memory around for re-use,
   but whether or not it does so is not part of the API.  Client code
   _must_ treat that scope object as if this function frees it, even
   if this function does not really do so.

   The final argument may be NULL and _must_ be NULL if the final
   scope is being popped (else an assert() may be triggered). (That
   said, it is illegal to pop the final scope from client-level code,
   as that one is pushed by the engine during its initialization.)

   @see whcl_scope_push()
*/
void whcl_scope_pop(whcl_engine * const el, whcl_scope * const sc,
                    cwal_value * const propagate);

/**
   Returns the given scope's properties object, creating it if needed.
   Returns NULL only on OOM. If sc is NULL, el's current scope is
   used. The returned value is where scope-level variables are
   stored.

   The returned value is ostensibly owned by the given scope, insofar
   as any ref-counted value is owned by anyone.

   Sidebar: when a properties object is created via this API, it is
   made "vacuum-proof" (see cwal_value_make_vacuum_proof()) and that
   flag is removed when the scope is popped (whcl_scope_pop()). Thus
   if client code takes a reference to this object (including refcount
   point) and that reference outlives the scope, the properties object
   loses any inherent vacuum safety the moment the scope pops. This is
   almost always the desired behavior, but particularly unusual client
   code may need to be aware of that any behave appropriately, either
   by making the properties object explicitly vacuum-proof or by
   adding it to a container which is itself vacuum-proof (either
   directly or by virtue of being contained in (perhaps indirectly) a
   vacuum-proof container).

   @see whcl_scope_push()
   @see whcl_scope_pop()
*/
cwal_value * whcl_scope_props(whcl_engine * const el,
                              whcl_scope * const sc);

/**
   A (somewhat) convenience form of whcl_set() which passes
   the (callback, state, stateDtor, stateTypeID) flags to
   cwal_new_function() and installs the resulting function int the tgt
   container value (or the current scope if tgt is NULL).

   If nameLen is <0 then cwal_strlen() is used to calculate its
   length. propertyFlags may be 0 or a mask of CWAL_VAR_F_xxx flags.

   Returns 0 on success or a CWAL_RC_xxx code on error.
 */
int whcl_install_callback( whcl_engine * const el, cwal_value * const tgt,
                           cwal_callback_f callback,
                           char const * const name,
                           cwal_int_t nameLen, 
                           uint16_t propertyFlags,
                           void * const state,
                           cwal_finalizer_f stateDtor,
                           void const * const stateTypeID );


/**
   Flags for use with whcl_dump_tokens().
*/
enum whcl_dump_tokens_e {
/**
   Outputs additional details, e.g. the content of each token.
*/
WHCL_DUMP_TOKENS_VERBOSE = 0x01,
/**
   Do not rewind tokenizer before dumping.
*/
WHCL_DUMP_TOKENS_NO_REWIND = 0x02,
/**
   Only output to the next EOX (end-of-expression) at the current
   token's level, but still output recursively.
*/
WHCL_DUMP_TOKENS_TO_EOX = 0x04,
/**
   Include virtual EOF tokens in the output. Most scripts have many of
   these. They clutter up the output but also provide a more complete
   picture of how this framework sees the world.
*/
WHCL_DUMP_TOKENS_EOFS = 0x10,
/**
   Emits some metrics after the dump.
*/
WHCL_DUMP_TOKENS_METRICS = 0x20
};
/**
   Dumps out all tokens in the given script. By default
   this rewinds ct but it always resets the token
   position when it's done.
*/
void whcl_dump_tokens(whcl_engine * const el,
                      whcl_script * const ct,
                      uint32_t flags);


/**
   Appends the given value to the given buffer in string form.
   Returns 0 on success.

   Objects and Arrays/Tuples are buffered in JSON form, and this
   function will fail if traversing them discovers cycles.
*/
int whcl_value_to_buffer( whcl_engine * const el, cwal_buffer * const buf,
                          cwal_value * const arg );

/**
   A cwal_callback_f() impl which uses whcl_value_to_buffer() to convert
   args->self to a string. The other arguments are ignored.
*/
int whcl_cb_value_to_string( cwal_callback_args const * args, cwal_value **rv );

/**
   A cwal_callback_f() impl which passes args->self through
   cwal_json_output() to produce JSON output. Assigns the resulting
   string value to *rv. If args->argc is not 0 then args->argv[0] is
   used to specify the indentation, as per cwal_json_output_opt.

   Script usage depends on whether or not args->self is-a (or inherits)
   Buffer. If not, then the function's usage abstractly looks like:

   ```
   string t = self.toJSONToken([indentation=0 [, cyclesAsStrings=false]])
   ```

   and returns the JSON-ified from of self.
   
   If self is-a Buffer, it looks like:

   ```
   self.toJSONToken(Value v [, indentation=0 [, cyclesAsStrings=false]])
   ```

   It appends the JSON form of v to self and returns self.

   If cyclesAsStrings is true, recursion/cycles are rendered in some
   useless (debugging only) string form, otherwise cycles cause an
   exception to be thrown.
*/
int whcl_cb_this_to_json_token( cwal_callback_args const * args, cwal_value **rv );

/**
   A cwal_callback_f() which passes its first argument through
   cwal_json_output() to produce JSON output. Assigns the resulting
   string value to *rv. If args->argc is greater than 1 then the
   second argument specifies the indentation: a positive number for
   that many spaces per level and a negative number for that many hard
   tabs per level.

   ```
   string t = toJSONToken(value, [indentation=0 [, cyclesAsStrings=false]])
   ```

   and returns the JSON-ified from of the value.

   If cyclesAsStrings is true, recursion/cycles are rendered in some
   useless (debugging only) string form, otherwise cycles cause an
   exception to be thrown.
*/
int whcl_cb_arg_to_json_token( cwal_callback_args const * args, cwal_value **rv );

/**
   A cwal_callback_f() implementation which parses JSON string input.

   Script usage:

   ```
   var json = '{"a":"hi!"}';
   var obj = thisFunction(json)
   ```
*/
int whcl_cb_json_parse_string( cwal_callback_args const * args, cwal_value **rv );

/**
   The file-based counterpart of whcl_cb_json_parse_string(). It works
   identically except that it takes a filename as input instead of a
   JSON string.
*/
int whcl_cb_json_parse_file( cwal_callback_args const * args, cwal_value **rv );

/**
   Tries to parse a given C-string as an integer or double value.

   srcLen must be the string length of str or a negative value
   (in which case cwal_strlen() is used to count the length).

   If the string can be parsed to an integer or double value, *rv is
   set to the newly-created value and 0 is returned.  If no conversion
   can be made, *rv is set to NULL and 0 is returned. On OOM error,
   CWAL_RC_OOM is returned.
*/
int whcl_cstr_parse_number( cwal_engine * const e, char const * src,
                            cwal_int_t slen, cwal_value ** rv );


/**
   Flags for use with with cwal_container_client_flags_set() and
   friends.
*/
enum whcl_container_flags_e {
/**
   EXPERIMENTAL. SUBJECT TO CHANGE OR REMOVAL.

   Tells the whcl eval engine that this function should not be treated
   as a boundary for symbol resolution purposes.
*/
WHCL_CONTAINER_F_XSYM = 0x01
};

/**
   Utility type for use with whcl_install_functions(), allowing simple
   installation of a series of callbacks in one go.
*/
struct whcl_func_def {
  /** Name to install the function as. */
  char const * name;
  /** Callback function. */
  cwal_callback_f callback;
  /** State for use with cwal_function_state_get(). */
  void * state;
  /** State destructor for cleaning up this->state when this function
      is finalized. In practice it is very rare for a function to have
      state which requires a finalizer. */
  cwal_finalizer_f stateDtor;
  /** Type ID for use with cwal_function_state_get(). */
  void const * stateTypeID;
  /** Flags from the whcl_container_flags_e enum. */
  cwal_flags16_t cwalContainerFlags;
};
#define WHCL_FUNC6(NAME,CALLBACK,STATE,StateDtor,StateTypeId,CwalContainerFlags)  \
  {NAME,CALLBACK,STATE,StateDtor,StateTypeId, CwalContainerFlags}
/**
   Convenience macro for initializing an whcl_func_def entry.
*/
#define WHCL_FUNC5(NAME,CALLBACK,STATE,StateDtor,StateTypeId) \
  WHCL_FUNC6(NAME,CALLBACK,STATE,StateDtor,StateTypeId, 0)
/**
   Convenience macro for initializing an whcl_func_def entry.
*/
#define WHCL_FUNC2(NAME,CALLBACK) WHCL_FUNC5(NAME,CALLBACK,0,0,0)
/**
   Convenience macro which is requivalent to WHCL_FUNC2 but also sets
   the WHCL_CONTAINER_F_XSYM flag on the function.
*/
#define WHCL_FUNC2_XSYM(NAME,CALLBACK) \
  WHCL_FUNC6(NAME,CALLBACK,0,0,0,WHCL_CONTAINER_F_XSYM)

/**
   Empty-initialized const whcl_func_def struct.
*/
#define whcl_func_def_empty_m {0,0,0,0,0,0}
/** Convenience typedef. */
typedef struct whcl_func_def whcl_func_def;

/**
   Installs a list of cwal callback functions into the given target
   container value. defs must be an array of whcl_func_def objects
   terminated by an entry with a NULL name field (most simply, use
   whcl_func_def_empty_m to intialize the final element). All member
   pointers in each entry must be valid, and 0 is (generally speaking)
   valid for all but the name and callback fields.

   If propertyFlags is not 0 then each property gets set with those
   flags, as per cwal_prop_set_with_flags().

   If tgt is NULL then the functions are installed into the _current_
   scope. Note that outside of initialization of the engine, the
   current scope is very likely not the top-most, and may well
   disappear soon (e.g. call this function with a NULL tgt from within
   a cwal_callback_f() implementation will only install these for the
   duration of the current function call!).

   Returns 0 on success, a non-0 CWAL_RC_xxx code on error.

   Example:

   ```
   const whcl_func_def funcs[] = {
     WHCL_FUNC2("myFunc1", my_callback_1),
     WHCL_FUNC2("myFunc2", my_callback_2),
     whcl_func_def_empty_m // IMPORTANT that the list end with this!
   };
   int rc = whcl_install_functions(se, myObj, funcs, CWAL_VAR_F_CONST);
   ```
*/
int whcl_install_functions( whcl_engine * const el, cwal_value * const tgt,
                            whcl_func_def const * defs,
                            uint16_t propertyFlags );

/**
   If ctrl-c handling is enabled at compile time, this function sets
   el to be the one listening for ctrl-C events, which it will report
   as an error. If not, this is a no-op.

   It goes without saying that using this in a multi-threaded app is
   fraught with peril.
*/
void whcl_set_interrupt_handlable( whcl_engine * const el );

/**
   This sets el's error state to CWAL_RC_INTERRUPTED, a flag it checks
   for at various points during evaluation and which causes the
   interpretter to behave essentially as if 'exit' had been used (but
   without a result value). Calling this is not a guaranty that the
   engine will stop processing its current script - there are corner
   cases where the flag can get "lost" during evaluation.

   On success, returns CWAL_RC_INTERRUPTED. On error (an allocation
   error generating a message string), it will return another non-0
   code (likely CWAL_RC_OOM).

   This is not strictly thread safe, but "should" be okay to call from
   a separate thread (e.g. a UI) in most cases, though (depending on
   timing) it might not have an effect. Known potential race
   conditions include, but are not necessarily limited to:

   - el is clearing its error state (which will clear the
     is-interrupted flag). It resets its error state internally for
     "non-error errors" which propagate a ways, like "return" and
     "throw", but those keywords "should" catch and propagate this
     condition, trumping their own. The lowest-level eval handler
     checks at every sensible opportunity.

   - el is cleaning up (inside s2_engine_finalize()), in which case
     accessing it might (depending on the timing)lead to an illegal
     memory access (if dynamically allocated) or a useless but
     harmless[1] access if it's stack-allocated. [1]=so long as the
     memory itself is still legal to access (e.g. app-level/static).

   - Third-party bindings may clear el's error state (which includes
     this flag) indescriminately, without being aware of this
     condition.


   There are likely others.
*/
int whcl_interrupt( whcl_engine * const el );

/**
   The reverse of cwal_rc_cstr(), this function tries to find a
   CWAL_RC_xxx error code for a string form of an enum entry's name,
   e.g. "CWAL_RC_OOM". On success it returns the code value via
   `*code` and returns true, else it does not modify `*code` and
   returns false.

   The first argument may be the full form of a CWAL_RC_... value or
   the same without the CWAL_RC_ prefix. CWAL_SCR_ values may be
   passed in without the CWAL_ prefix but require the SCR_ part. e.g.
   "MISUSE" may be used in place of "CWAL_RC_MISUSE" and "SCR_EOF" may
   be used in place of "CWAL_SCR_EOF"

   The 2nd argument is the length of the first one. If it is negative,
   cwal_strlen() is used to calculate it.

   This is an O(1) operation, performing one hash calculation and (at
   most) one string comparison.
*/
bool whcl_cstr_to_rc(char const *str, cwal_int_t len, int * code);

/**
   Returns the core object prototype, or NULL on allocation
   error. This object can be extended to add features to
   all values which derive from the Object class.
*/
cwal_value * whcl_prototype_object(whcl_engine * const el);
/** 
   Returns the core function prototype, or NULL on allocation error.
*/
cwal_value * whcl_prototype_function(whcl_engine * const el);
/** 
   Returns the core array prototype, or NULL on allocation error.
*/
cwal_value * whcl_prototype_array(whcl_engine * const el);
/** 
   Returns the core tuple prototype, or NULL on allocation error.
*/
cwal_value * whcl_prototype_tuple(whcl_engine * const el);
/** 
   Returns the core buffer prototype, or NULL on allocation error.
*/
cwal_value * whcl_prototype_buffer(whcl_engine * const el);
/** 
   Returns the core string prototype, or NULL on allocation error.
*/
cwal_value * whcl_prototype_string(whcl_engine * const el);
/** 
   Returns the core integer prototype, or NULL on allocation error.
*/
cwal_value * whcl_prototype_integer(whcl_engine * const el);
/** 
   Returns the core double prototype, or NULL on allocation error.
*/
cwal_value * whcl_prototype_double(whcl_engine * const el);
/** 
    On success, returns 0 and assigns the PathFinder (pf) prototype to
    `*rv`. On error, returns non-0 and does not modify `*rv`. If rv
    is NULL this function returns 0 if it initializes the prototype
    or if it has already been initialized.

    Trivia: this function's signature is different from the other
    whcl_prototype_xxx() functions because this prototype has error
    modes other than an OOM condition and needs to be able to
    propagated them without it being mistaken downstream for an OOM.
*/
int whcl_prototype_pf(whcl_engine * const el, cwal_value ** rv);
/** 
   Returns the Exception prototype, or NULL on allocation error.
*/
cwal_value * whcl_prototype_exception(whcl_engine * const el);
  
/**
   cwal_callback_f() implementation which acts as a proxy for
   cwal_value_compare().

   If passed two values it passes those to cwal_value_compare(), else
   it passes args->self and args->argv[0] to it (in that order).

   If both the lhs/rhs values are the same pointer, they of course
   compare as equivalent (without calling cwal_value_compare()).

   If passed 3 values and the 3rd is truthy, it checks to see if the
   values have the same type. If they do not, it returns an arbitrary
   (but stable) non-0 value. If they do have the same type, or if the
   3rd parameter is falsy, it behaves as if passed 2 parameters.

   Throws on usage error, else returns the result of the comparison
   via *rv.

   Script signatures:

   ```
   integer compare(value rhs); // requires a 'this'
   integer compare(value lhs, value rhs[, boolean typeStrict = false]);
   ```

   The latter form works independently of the current 'this'.
*/
int whcl_cb_value_compare( cwal_callback_args const * args, cwal_value **rv );

/**
   Returns true if the given value is a string and matches the first
   fLen bytes of f. If fLen is negative, cwal_strlen() is used to
   calculate it.
 */
bool whcl_val_is_flag( cwal_value const * const v, char const * const f,
                       cwal_int_t fLen );

/**
   A helper for for cwal_callback_f() flag-style argument handling. If
   `*argNdx<args->argc` and `args->argv[*argNdx]` is a string which
   starts with `-` and has a length of 2 or more, this function
   increments `*argNdx`, sets `*flag` to the string value of that
   argument, and `*len` to its length. Otherwise it returns false.

   Either of `flag` and `len` may be NULL. `argNdx` may not be NULL.

   Note that this routine cannot tell the difference between a
   string-format negative number and a flag.
*/
bool whcl_arg_has_flag( cwal_callback_args const * args,
                        uint16_t * argNdx, char const **flag,
                        cwal_size_t * len);

/**
   This may optionally be called by clients, soon after
   whcl_engine_init(), to install the core-most data types and APIs
   into el. Without this, the language itself will function but it
   will be missing many type-level and utility APIs which this
   installs. Returns 0 on success. The "only conceivable" error case
   is OOM, in which case it returns CWAL_RC_OOM. If called more than
   once, this is a no-op.

   This also installs the script-side function `whcl[install-api]`
   which works as documented for whcl_install_api().
*/
int whcl_install_core_apis(whcl_engine * const el);

/**
   Works just like the C-standard fopen() except that if zName is "-"
   then it returns either stdin or stdout, depending on whether zMode
   contains a "r" (meaning stdin) or one of "w", "a", or "+" (meaning
   stdout). Neither argument may be NULL.

   Returns a newly-opened file handle on success, else NULL. The
   handle should eventually be passed to whcl_fclose() to close it. It
   may optinoally be passed to fclose() instead, but that routine will
   unconditionally close the handle, whereas this one is a no-op for
   the standard streams.

   @see whcl_fclose()
*/
FILE *whcl_fopen(const char *zName, const char *zMode);

/**
   If the given FILE handle is one of (stdin, stdout, stderr), this is
   a no-op, else it passes the file handle to fclose().

   @see whcl_fopen()
*/
void whcl_fclose(FILE *);

/**
   The "Path Finder" class is a utility for searching the filesystem
   for files matching a set of common prefixes and/or suffixes
   (i.e. directories and file extensions).

   @see whcl_new_pf()
   @see whcl_pf_value()
   @see whcl_value_pf()
   @see whcl_value_pf_part()
   @see whcl_pf_dir_add()
   @see whcl_pf_dir_add_v()
   @see whcl_pf_dirs()
   @see whcl_pf_dirs_set()
   @see whcl_pf_ext_add()
   @see whcl_pf_ext_add_v()
   @see whcl_pf_exts_set()
   @see whcl_pf_exts()
   @see whcl_pf_search()
*/
typedef struct whcl_pf whcl_pf;


/**
   Creates a new PathFinder instance. PathFinders are bound to cwal as
   cwal_native instances and are initially owned by the currently
   active scope. Returns NULL on allocation error.
*/
whcl_pf * whcl_pf_new(whcl_engine * const el);

/**
   Returns the underlying cwal_value which acts as pf's "this".

   pf may not be NULL.

   @see whcl_value_pf()
   @see whcl_value_pf_part()
*/
cwal_value * whcl_pf_value(whcl_pf const * const pf);

/**
   If v is-a PathFinder or derives from it, this function returns the
   whcl_pf part of v or one of its prototypes.

   It is legal for v to be NULL.

   @see whcl_value_pf()
   @see whcl_pf_value()
*/
whcl_pf * whcl_value_pf_part(cwal_value const * v);

/**
   If v was created via whcl_pf_new() then this function returns
   its whcl_pf counterpart, else it returns NULL.

   It is legal for v to be NULL.

   @see whcl_pf_value()
   @see whcl_value_pf_part()
*/
whcl_pf * whcl_value_pf(cwal_value const * const v);


/**
   Adds a directory to pf's search path. dir must be at least dirLen bytes
   and may be an empty but may not be NULL. If dirLen is negative,
   cwal_strlen() is used to calculate it.

   Returns 0 on success.

   @see whcl_pf_dir_add_v()
*/
int whcl_pf_dir_add( whcl_pf * const pf, char const * dir, cwal_int_t dirLen);

/**
   Adds a file suffix (extension) to pf's search path. ext must be at
   least extLen bytes and may be an empty but may not be NULL.  If
   extLen is negative, cwal_strlen() is used to calculate it.

   Returns 0 on success.

   @see whcl_pf_ext_add_v()
*/
int whcl_pf_ext_add( whcl_pf * const pf, char const * ext, cwal_int_t extLen);

/**
   Variant of whcl_pf_dir_add() which takes its directory part in the
   form of a cwal_value.

   Returns 0 on success, CWAL_RC_MISUSE if !v, CWAL_RC_OOM on OOM.
*/
int whcl_pf_dir_add_v( whcl_pf * const pf, cwal_value * const v );

/**
   Variant of whcl_pf_ext_add() which takes its directory part in the
   form of a cwal_value.

   Returns 0 on success, CWAL_RC_MISUSE if !v, CWAL_RC_OOM on OOM.
*/
int whcl_pf_ext_add_v( whcl_pf * const pf, cwal_value * const v );

/**
   Replaces pf's directory list with the given one.

   Returns 0 on success, CWAL_RC_MISUSE if !ar, or some other code if
   actually setting the member fails ("probably" CWAL_RC_OOM or
   CWAL_RC_CONST_VIOLATION).
*/
int whcl_pf_dirs_set( whcl_pf * const pf, cwal_array * const ar );

/**
   Replaces pf's extension/suffix list with the given one.

   Returns 0 on success, CWAL_RC_MISUSE if !ar, or some other code if
   actually setting the member fails ("probably" CWAL_RC_OOM or
   CWAL_RC_CONST_VIOLATION).
*/
int whcl_pf_exts_set( whcl_pf * const pf, cwal_array * const ar );

/**
   Symbolic values for use with whcl_pf_search()'s final
   parameter.
*/
enum whcl_pf_search_e {
/**
   Indicates that ONLY directory names will be considered as matches.
*/
WHCL_PF_SEARCH_DIRS = -1,
/**
   Indicates that ONLY file (not directory) names will be considered
   as matches.
*/
WHCL_PF_SEARCH_FILES = 0,
/**
   Indicates that both file and directory names will be considered as
   matches.
*/
WHCL_PF_SEARCH_FILES_DIRS = 1
};

/**
   Searches for a file whose name can be constructed by some
   combination of pf's directory/suffix list and the given base name.
   baseLen is the length of the base name - if it is negative,
   cwal_strlen() is used to calculate it.

   The 5th argument specificies whether searching is allowed to match
   directory names or not. A value of 0 means only files (not
   directories) will be considered for matching purposes. A value
   greater than zero means both files and directories may be
   considered for matching purposes. A value less than zero means only
   directories (not files) may be considered a match.  (See the
   whcl_pf_search_e enum for symbolic names for this policy.)

   BUG: the directory policy does not currently work on non-Unix
   platforms because we don't have the code to check if a file name is
   a directory for such platforms (patches are welcomed!).

   Returns NULL if !pf, !base, !*base, !baseLen, or on allocation
   error (it uses/recycles a buffer to hold its path combinations).

   On success it returns a pointer to the (NUL-terminaed) path under
   which it found the item and rcLen (if not NULL) will be set to the
   length of the returned string. The bytes of the returned string are
   only valid until the next operation on pf, so copy them if you need
   them.

   If no match is found, rcLen is not modified.

   By default the host platform's customary path separator is used to
   separate directory/file parts ('\\' on Windows and '/' everywhere
   else). To change this, set the "separator" property of pf to a
   string value (even an empty one, in which case the directory paths
   added to pf should have the trailing separator added to them in
   order for searching to work).

   Pedantic sidebar: if the search path is empty, a match can still be
   found if the base name by itself, or in combination with one of the
   configured extensions, matches an allowed type of filesystem entry
   (as designated via the final argument).

   @see whcl_pf_search_e
*/
char const * whcl_pf_search( whcl_pf * const pf, char const * base,
                             cwal_int_t baseLen, cwal_size_t * const rcLen,
                             int directoryPolicy);


/**
   Returns pf's list of directories, creating it if needed. Only
   returns NULL if !pf or on allocation error. If this function
   creates the list, it is initially owned by pf.

   In script space this value is available via the "path" property.
*/
cwal_array * whcl_pf_dirs(whcl_pf * const pf);

/**
   Returns pf's list of extensions/suffixes, creating it if
   needed. Only returns NULL if !pf or on allocation error. If this
   function creates the list, it is initially owned by pf.

   In script space this value is available via the "ext" property.
*/
cwal_array * whcl_pf_exts(whcl_pf * const pf);

/**
   Expects to be passed all arguments from main() which appear _after_
   a `--` flag. argc is the argument count and argv is the list. It is
   legal for argv to be 0, in which case this function sets up the
   script-accessible arguments list (see below) as an empty
   list. (Much practice has shown that having an empty list is
   preferable to having to distinguish, client-side, whether the list
   was initialized or not.)

   By long-standing cwal client app convention, all arguments after
   `--` are for script-side consumption. This function processes them
   using cwal_parse_argv_flags() and, on success, installs them as
   `whcl[ARGV]`. Returns 0 on success.  On error it does not install
   them and returns non-0 (with CWAL_RC_OOM being the only "likely"
   error result code).

   This function must only be called once in the lifetime of an app.
   It installs the `whcl[ARGV]` object as const, so any further calls
   to this function will trigger a CWAL_RC_CONST_VIOLATION.
*/
int whcl_install_argv(whcl_engine * const el, int argc,
                      char const * const * argv);

/**
   A helper type for tokenizing conventional PATH-style strings.
   Initialize them with whcl_path_toker_init() and iterate over them
   with whcl_path_toker_next().
*/
struct whcl_path_toker {
  /** Begining of the input range. */
  char const * begin;
  /** One-after-the-end of the input range. */
  char const * end;
  /** Position for the next token lookup. */
  char const * pos;
  /** List of token separator characters (ASCII only). */
  char const * separators;
};
typedef struct whcl_path_toker whcl_path_toker;
/**
   Default-initialized whcl_path_toker instance, intended for const-copy
   initialization. On Windows builds its separators member is set to
   ";" and on other platforms it's set to ":;".
*/
#if defined(WHCL_OS_WINDOWS)
#  define whcl_path_toker_empty_m {NULL,NULL,NULL,";"}
#else
#  define whcl_path_toker_empty_m {NULL,NULL,NULL,":;"}
#endif

/**
   Default-initialized whcl_path_toker instance, intended for
   copy initialization.

   @see whcl_path_toker_empty_m
*/
extern const whcl_path_toker whcl_path_toker_empty;

/**
   Wipes out pt's current state by copying whcl_path_toker_empty over it
   and initializes pt to use the given path as its input. If len is 0
   or more then it must be the length of the string, in bytes. If len
   is less than 0, cwal_strlen() is used to determine the path's
   length.  (When dealing with inputs which are not NUL-terminated,
   it's critical that the user pass the correct non-negative length.)

   If the client wants to modify pt->separators, it must be done so
   *after* calling this.

   Use whcl_path_toker_next() to iterate over the path entries.
*/
void whcl_path_toker_init( whcl_path_toker * const pt, char const * path,
                           cwal_int_t len );

/**
   Given a whcl_path_toker which was formerly initialized using
   whcl_path_toker_init(), this iterates over the next-available path
   component in the input, skipping over empty entries (consecutive
   separator characters). 

   The separator characters are specified by pt->separators, which must
   be a NUL-terminated string of 1 or more characters.

   If a non-empty entry is found then:

   - *token is set to the first byte of the entry.

   - *len is assigned to the byte length of the entry.

   If no entry is found then:

   - *token, and *len are not modified.

   - CWAL_RC_NOT_FOUND is returned if the end of the path was found
   while tokenizing.

   - CWAL_RC_MISUSE is returned if pt->separators is NULL or empty or
   contains any non-ASCII characters.

   - CWAL_RC_RANGE is returned if called after the previous case, or
   if the input object's path has a length of 0.

   In any non-0-return case, it's not a fatal error, it's simply
   information about why tokenization cannot continue, and can
   normally be ignored. After non-0 is returned, the tokenizer must be
   re-initialized if it is to be used again.

   Example:

   @code
   char const * t = 0;
   cwal_size_t tLen = 0;
   whcl_path_toker pt = whcl_path_toker_empty;
   whcl_path_toker_init(&pt, path, pathLen);
   while(0==whcl_path_toker_next(&pt, &t, &tLen)){
      // The next element is the tLen bytes of memory starting at t:
      printf("Path element: %.*s\n", (int)tLen, t);
   }
   @endcode
*/
int whcl_path_toker_next( whcl_path_toker * const pt, char const ** token,
                          cwal_size_t * const len );


/**
   Behaves more or less like the access(2) C function (_access() on
   Windows builds).

   Returns true if the given filename is readable (writeable if
   checkForWriteAccess is true), else false.
*/
bool whcl_file_is_accessible( char const * fn, bool checkForWriteAccess );

/**
   Checks for the existence of a directory with the given NUL-terminated
   name. If passed a true 2nd argument then it also checks whether the
   directory is writeable.

   Returns true (non-0) if the directory exists and (if
   checkForWriteAccess is true) writeable. If checkForWriteAccess is
   false, it returns true if the directory can be stat()ed.

   Returns 0 if the given name cannot be stat()ed or if
   checkForWriteAccess is true and the directory is not writeable.

   Currently on works on Unix platforms. On others it always returns
   0.
*/
bool whcl_is_dir( char const * name, bool checkForWriteAccess );

/**
   cwal_callback_f() impl binding whcl_file_is_accessible() in scriptable form:

   fileIsAccessible(string filename [, bool checkWriteMode=false])

*/
int whcl_cb_file_accessible( cwal_callback_args const * args, cwal_value **rv );

/**
   The directory counterpart of whcl_cb_file_accessible().
*/
int whcl_cb_dir_accessible( cwal_callback_args const * args, cwal_value **rv );

/**
   A cwal_callback_f() implementation which tokenizes conventional
   PATH-style strings. Script-side usage:

   array f( string path [, array target] )

   Each element in the input path is appended to the target array (or
   a new array if no array is passed in) and that array is returned.

   It triggers an exception if passed any invalid arguments, noting
   that an empty input string is not an error but will cause an empty
   list to be returned (resp. no entries to be added to the target
   list).
*/
int whcl_cb_tokenize_path( cwal_callback_args const * args, cwal_value **rv );

/**
   A cwal_callback_f() implementation which tokenizes its input into
   an array of whcl tokens, but supporting only a subset of its core
   token types: numbers, quoted strings, and the built-in constants
   bool/null/undefined values. All identifiers, operators, and the
   like will be tokenized as strings. Heredocs and similar block
   constructs are not handled here.

   Its intended purpose is to tokenize single lines of "commands" for
   use in interactive command-based dispatching in scripts.

   Script usage:

   array tokenizeLine(string input)

   e.g.:

   tokenizeLine('1 2.3 "hi there" true null')

   would result in an array: [1, 2.3, 'hi there', true, null]

   This function does not parse higher-level constructs like objects,
   arrays, or functions. "Junk" tokens, such as whitespace and EOLs,
   are elided. It throws for any sort of tokenization error, e.g.  an
   unclosed quoted string.

   If the input string is empty, it resolves to the undefined value,
   not an empty array (though that decision is up for reconsideration,
   depending on what pending experience suggests).
*/
int whcl_cb_tokenize_line(cwal_callback_args const * args, cwal_value ** rv);


/**
   Tokenizes a conventional PATH-style string into an array.

   *tgt must either be NULL or point to an array owned by the
   caller. If it is NULL, this function creates a new array and (on
   success) assigns it to *tgt. Each entry in the path is added to the
   array.

   None of the pointer-type arguments, including tgt (as opposed to
   *tgt), may be NULL.

   The 4th argument must be the length, in bytes, of the given path
   string. If it is negative, the equivalent of strlen() is used to
   calculate its length. If the argument, or its resulting calculated
   value, is 0 then the result will be that the target array will be
   created, if needed, but will get no entries added to it.

   On success, 0 is returned and 0 or more entries will have been
   added to the target array. If *tgt was NULL when this function was
   called, *tgt will be assigned to a new array, with a refcount of 0,
   which is owned by the caller.

   Assuming all arguments are valid, the only plausible error this
   function will return is CWAL_RC_OOM, indicating that allocation of
   the target array, an entry for the array, or space within the array
   for an entry, failed. On error, if *tgt was NULL when this function
   was called, *tgt is not modified, otherwise *tgt may have been
   modified before the allocation error occurred.

   Example:

   ```
   cwal_array * ar = 0;
   int rc = whcl_tokenize_path_to_array(e, &ar, "foo;bar", -1);
   ```

   On success, ar will be non-NULL and owned by that code.
   Contrast with:

   ```
   int rc;
   cwal_array * ar = cwal_new_array(e);
   if(!ar) return CWAL_RC_OOM;
   rc = whcl_tokenize_path_to_array(e, &ar, "foo;bar", -1);
   ```

   In that case, any entries in the given path string will be appended
   to the array provided by the caller.

   @see whcl_path_toker
   @see whcl_path_toker_init()
 */
int whcl_tokenize_path_to_array( cwal_engine * e, cwal_array ** tgt,
                                 char const * path,
                                 cwal_int_t pathLen );


/**
   Returns the `whcl` "namespace" object. `whcl` is a built-in value
   in the interpreter, intended to be a named global place to store
   classes and such. Ownership of the returned pointer does not change
   and the caller need not add a reference to it: the value is kept
   around until el is finalized.
*/
cwal_value * whcl_namespace(whcl_engine * const el);

/**
   A proxy for getcwd(2) which appends the current working directory's
   name to the end of the given buffer. This function expands tgt
   by some relatively large amount to be able to handle long
   paths. Because of this, it's recommended that a shared/recycled
   buffer be used to handle calls to this function.

   On success, tgt gets appended to, updating tgt's members as
   appropriate, and 0 is returned. On error, non-0 is returned (the
   exact code may depend on the errno set by getcwd(2)).

   ACHTUNG: this function is only implemented for Unix systems. On
   Windows builds it returns CWAL_RC_UNSUPPORTED because i don't have
   a Windows system to implement this on.
*/
int whcl_getcwd( cwal_engine * const e, cwal_buffer * const tgt );

/**
   A cwal_callback_f() implementation wrapping whcl_getcwd(). On
   success, it returns (via *rv) the current working directory's name
   as a string. If script passed an argument from script code, it is
   interpreted as a boolean: if true, the directory separator is
   appended to the result, else it is not. The default is not to
   append the directory separator to the result.
*/
int whcl_cb_getcwd( cwal_callback_args const * args, cwal_value ** rv );

/**
   Works like mkdir(2). Returns 0 on success, else a CWAL_RC_xxx value
   approximating the underlying errno result. If errMsg is not NULL
   then on error, *errMsg will point to an error message string owned
   by the C library. Its contents may be modified by calls to
   strerror(3), so must be copied if it should be retained.

   LIMITATIONS:

   1) Returns CWAL_RC_UNSUPPORTED on builds which don't have mkdir(2).

   2) Does not create intermediate directories, so the parent dir
   of the new directory must already exist. See whcl_mkdir_p().

   3) The message string returned by strerror() may be modified by
   calls to that function from other threads, so the errMsg
   argument is only known to be useful for single-threaded clients.

   @see whcl_mkdir_p()
*/
int whcl_mkdir( char const * name, int mode, char const ** errMsg );

/**
   A convenience wrapper around whcl_mkdir() which creates parent
   directories, if needed, for the target directory name. e.g.  if
   passed "a/b/c" and "a" and/or "b" do not exist, they are created
   before creating "c". Fails if creation of any part of the path
   fails.

   It requires a well-formed Unix-style directory name (relative or
   absolute). Returns 0 on success, else non-0 and an error message,
   as documented for whcl_mkdir().

   @see whcl_mkdir()
*/
int whcl_mkdir_p( char const * name, int mode, char const ** errMsg );

/**
   A cwal_callback_f() implementation wrapping whcl_mkdir(). It mkdir() is
   not available in this built, throws an exception with code
   CWAL_RC_UNSUPPORTED.

   Script-side signatures:

   (string dirName [, bool makeParentDirs=false [, int mode = 0750]])
   (string dirName [, int mode = 0750])

   Noting that the access mode may be modified by the OS, e.g. to
   apply the umask.

   If a directory with the given name already exists, this function
   has no side-effects. Note that the access mode is ignored for
   purposes of that check.

   Throws on error. On success, returns the undefined value.
*/
int whcl_cb_mkdir( cwal_callback_args const * args, cwal_value ** rv );


/**
   Reads the given file (synchronously) until EOF and streams its
   contents (in chunks of an unspecified size) to cwal_output() using
   the given cwal_engine. Returns 0 on success, non-0 (likely
   CWAL_RC_IO) on error. Does not modify ownership of the passed-in
   pointers.
*/
int whcl_passthrough_FILE( cwal_engine * e, FILE * file );

/**
   A convenience wrapper around whcl_passthrough_FILE() which uses
   whcl_fopen() to open the given filename and (on success) stream that
   file's contents to cwal_output(). Returns CWAL_RC_IO if the fopen()
   fails, else returns as documented for whcl_passthrough_FILE().
*/
int whcl_passthrough_filename( cwal_engine * e, char const * filename );

/**
   File/directory types for use with whcl_fstat_t.
*/
enum whcl_fstat_types {
WHCL_FSTAT_TYPE_UNKNOWN = 0,
WHCL_FSTAT_TYPE_REGULAR = 0x01,
WHCL_FSTAT_TYPE_DIR = 0x02,
WHCL_FSTAT_TYPE_LINK = 0x04,
WHCL_FSTAT_TYPE_BLOCK = 0x08,
WHCL_FSTAT_TYPE_CHAR = 0x10,
WHCL_FSTAT_TYPE_FIFO = 0x20,
WHCL_FSTAT_TYPE_SOCKET = 0x40
};
/**
   Filesystem entry info for use with whcl_fstat().
*/
struct whcl_fstat_t {
  /**
     The type of a given entry.
  */
  enum whcl_fstat_types type;
  /**
     Change time (includes metadata changes, e.g. permissions and
     such).
  */
  uint64_t ctime;
  /**
     Modification time (does not account for metadata changes,
     e.g. permissions and such).
  */
  uint64_t mtime;
  /** Size, in bytes. */
  uint64_t size;
  /** Unix permissions. */
  int perm;
};
typedef struct whcl_fstat_t whcl_fstat_t;
/**
   Intended to be used as a copy-construction source for local whcl_fstat_t copies,
   to ensure a clean state.
 */
extern const whcl_fstat_t whcl_fstat_t_empty;
/**
   Cleanly-initialized whcl_fstat_t entry, intended for const copy
   initialization.
*/
#define whcl_fstat_t_empty_m {0,0,0,0,0}

/**
   Wrapper for stat(2) and lstat(2). The filename is taken from the
   first fnLen bytes of the given filename string. (We take the length
   primarily because cwal X-strings and Z-strings need not be
   NUL-terminated.) If fnLen is negative then cwal_strlen() is used to
   calculate it. If tgt is not NULL then the results of the stat are
   written there. If derefSymlinks is true then stat() is used, else
   lstat() is used, which fetches information about a symlink, rather
   than the file the symlink points to.

   On success, returns 0 and, if tgt is not NULL, populates tgt.  On
   error, tgt is not modified.

   If stat() is not available in this build, CWAL_RC_UNSUPPORTED
   is returned.

   If lstat() is not available in this build, it returns
   CWAL_RC_UNSUPPORTED if derefSymlinks is false.

   If tgt is NULL and 0 is returned, it means that stat(2) succeeded.

   If stat(2) or lstat(2) fail, non-0 is returned (a CWAL_RC_xxx value
   approximating the errno from the failed call).
*/
int whcl_fstat( char const * filename,
                cwal_int_t fnLen,
                whcl_fstat_t * const tgt,
                bool derefSymlinks );

/**
   A cwal_callback_f() implementation wrapping whcl_fstat().

   Script signatures:

   1) object stat( string filename, [derefSymlinks=true] )

   Returns an object representing the stat() info (see
   whcl_fstat_to_object() for the structure. Throws an exception if
   stat() fails.

   2) bool stat(string filename, undedfined [, derefSymlinks=true])

   If the argument count is greater than 1 and the the second argument
   has the undefined value then this function returns a boolean
   instead of an object. In that case, it returns false, instead of
   throwing, if stat() fails. That's admittedly a horribly awkward way
   to distinguish between the two return modes, and it may well be
   changed at some point.
*/
int whcl_cb_fstat( cwal_callback_args const * args, cwal_value ** rv );

/**
   Converts a populated whcl_fstat_t struct to an Object with properties describing
   the whcl_fstat_t values:

   ```
   {
     ctime: integer, // state change time (includes perms changes)
     mtime: integer, // modification time
     perm: integer, // Unix permissions bits
     size: integer, // size, in bytes
     type: string // "unknown", "file", "dir", "link", "block", "char", "fifo", "socket"
   }
   ```

   On success, assigns *rv to the newly-created object and returns 0. On error
   *rv is not modified and returns non-zero (likely CWAL_RC_OOM).

   Just FYI: this function caches the keys (via whcl_stash_set()) used
   for the returned object's properties, so it's not quite as
   expensive as it may initially seem.
*/
int whcl_fstat_to_object( whcl_engine * const el,
                          whcl_fstat_t const * const fst,
                          cwal_value ** rv );

/**
   A cwal_callback_f() which expects its first argument to be an
   integer. It tries to sleep for at least that many seconds and
   returns the number of seconds left to sleep if it is interrupted
   (as per sleep(3)).

   @see whcl_install_time()
 */
int whcl_cb_sleep(cwal_callback_args const * args, cwal_value ** rv);

/**
   A cwal_callback_f() which expects its first argument to be an
   integer. It sleeps for that many milliseconds. It throws an
   exception if usleep(3) fails or if the library is built without
   usleep(3) support. It returns the undefined value.

   @see whcl_install_time()
*/
int whcl_cb_mssleep(cwal_callback_args const * args, cwal_value ** rv);

/**
   A cwal_callback_f() impl binding the C-standard time(3).

   @see whcl_install_time()
*/
int whcl_cb_time( cwal_callback_args const * args, cwal_value **rv );

/**
   A cwal_callback_f() impl returning the current time in milliseconds
   since the start of the Unix epoch. This requires platform-specific
   calls and throws an exception, with code CWAL_RC_UNSUPPORTED, if
   built on a platform with the required API.

   @see whcl_install_time()
*/
int whcl_cb_mstime( cwal_callback_args const * args, cwal_value **rv );

/**
   A strftime() implementation.

   dest must be valid memory at least destLen bytes long. The result
   will be written there.

   fmt must contain the format string. See the file toys/strftime.s2
   (or strftime.c, if you're more into C) for the complete list of
   format specifiers and their descriptions.

   timeptr must be the time the caller wants to format.

   Returns 0 if any arguments are NULL.
   
   On success it returns the number of bytes written to dest, not
   counting the terminating NUL byte (which it also writes). It
   returns 0 on any error, and the client may need to distinguish
   between real errors and (destLen==0 or !*fmt), both of which could
   also look like errors.

   TODOs:

   - Refactor this to take a callback or a cwal_buffer, so that we can
   format arbitrarily long output.

   - Refactor it to return an integer error code.

   (i didn't write this implementation - it is derived from public domain
   sources dating back to the early 1990's.)

*/
cwal_midsize_t whcl_strftime(char *dest, cwal_midsize_t destLen,
                           const char *format, const struct tm *timeptr);

    
/**
   A cwal_callback_f() which wraps whcl_strftime().

   Script usage:

   ```
   var tm = time();
   var str = strftime("%Y-%m-%d %H:%M:%S", tm);
   ```

   The default time value, if no second argument is passed in or a
   negative value is passed in, is the current time.

   Note that this implementation has a limit on the length of the
   result string (because whcl_strftime() works that way), and
   throws if that length is violated. It's suitable for "usual"
   time strings but not for formatting whole sentences.

   This function takes an optional boolean 3rd argument: if truthy,
   local time is used, else GMT is used (the default).
*/
int whcl_cb_strftime( cwal_callback_args const * args, cwal_value **rv );

/**
   Pushes one level of output buffer into se's output buffer stack.
   Buffering works similarly to PHP's ob_start() (and friends) support.
   While a buffer is active, all output send to cwal_engine_output()
   and friends is redirected to a buffer. The various whcl_ob_xxx()
   functions can be used to:

   - fetch or discard the contents
   - push a new buffer onto the stack
   - pop the buffer from the stack (discarding its contents)

   When the interpreter is shut down it automatically removes any
   pushed buffers, but clients should call whcl_ob_pop() once
   for each time they call whcl_ob_push()

   Returns 0 on success, CWAL_RC_MISUSE if !se, CWAL_RC_RANGE if there
   has been no corresponding call to whcl_ob_push().

   Results of the whole whcl_ob_XXX() API are undefined if another API
   manipulates the contents of the underlying cwal_engine's output
   redirection bits (i.e. cwal_engine_vtab::outputer via
   cwal_engine::vtab).
   
   @see whcl_ob_pop()
   @see whcl_ob_get()
   @see whcl_ob_take()
   @see whcl_ob_clear()
   @see whcl_ob_level()
   @see whcl_ob_flush()
*/
int whcl_ob_push( whcl_engine * const el );

/**
   Attempts to reserve at least reserveBufSize bytes of memory for the
   current buffering level. This does not change the buffering level.

   Returns 0 on success, CWAL_RC_RANGE if whcl_ob_push() has not
   previously been called, and CWAL_RC_OOM if allocation of new memory
   fails.

   @see whcl_ob_push()
 */
int whcl_ob_reserve( whcl_engine * const el, cwal_size_t reserveBufSize );

/**
   Removes the current level of output buffer from ie.

   Returns 0 on success, CWAL_RC_RANGE if there
   has been no corresponding call to whcl_ob_push().
*/
int whcl_ob_pop( whcl_engine * const el );

/**
   Returns the current buffering level, or 0 if !ie or ie is
   not in buffering mode.

   @see whcl_ob_push()
   @see whcl_ob_pop()
*/
cwal_size_t whcl_ob_level( whcl_engine * const el );

/**
   Gets a pointer to the raw buffer owned by the current level of
   output buffer, assigning it to `*tgt`. The buffer is owned by the
   OB layer and its contents may be modified on any API routines which
   end up calling cwal_engine_output() or the other whcl_ob_xxx()
   APIs. The caller is intended to copy/use the buffer's contents
   immediately, and not hold on to it past the current operation.

   Returns 0 on success, CWAL_RC_RANGE if there has been no
   corresponding call to whcl_ob_push(). On error `*tgt` is
   not modified.
*/
int whcl_ob_get( whcl_engine * const el, cwal_buffer ** tgt );

/**
   Like whcl_ob_get(), but moves the contents of the current
   buffer layer into tgt, clearing the OB buffer but leaving
   it on the buffer stack for later use.

   Returns 0 on success, CWAL_RC_RANGE if there has been no
   corresponding call to whcl_ob_push().

   tgt must be empty-initialized or the caller must call
   cwal_buffer_reserve(..., tgt, 0) before calling this or memory may
   leak. On success ownership of the memory in tgt->mem is transfered
   to the caller. If tgt was created via cwal_new_buffer() or
   cwal_new_buffer_value() then tgt and tgt->mem are owned by se->e.
*/
int whcl_ob_take( whcl_engine * const el, cwal_buffer * const tgt );

/**
   Clears the contents of the current buffering layer. If
   releaseBufferMem is true (non-0) then the buffer memory is
   deallocated, otherwise it is just reset for later use by the OB
   layer. If it is deallocated, it will be re-allocated later if more
   output is buffered.

   Returns 0 on success, CWAL_RC_RANGE if there has been no
   corresponding call to whcl_ob_push().
*/
int whcl_ob_clear( whcl_engine * const el, bool releaseBufferMem );

/**
   Pushes the current contents of the output buffer layer to the next
   output destination in the stack and the current level is cleared of
   contents (but stays on the stack). If the next outputer is a buffer
   then the current buffer is appended to it, otherwise it is sent to
   the originally configured output destination.

   Returns 0 on success, CWAL_RC_RANGE
   if there has been no corresponding call to whcl_ob_push(),
   and potentially some other error if flushing to the lower-level
   implementation fails.

   @see whcl_ob_push()
   @see whcl_ob_pop()
*/
int whcl_ob_flush( whcl_engine * const el );

/**
   Returns a handle to the given interpreter's "whcl object".  This is
   the object to which the `whcl` builtin value resolves to and is, in
   practice, where most global-level functionality gets install. The
   returned value is owned by el and remains valid until el's top-most
   scope is popped (normally during finalization).
*/
cwal_value * whcl_whcl_object(whcl_engine * const el);


/**
   Installs a dispatcher function property named "__command"
   in tgt. The property is defined with the CONST/HIDDEN flags.
   Returns 0 on success.

   The dispatcher looks at the first arg to the function and searches
   for a function-type member property of the same name. If found, it
   forwards the call, minus the first arg, to that function, else it
   fails loudly.

   Reminder to self: do not install this on the Function type. That
   leads to a chicken/egg scenario.

   Note that the whcl-installed types, aside from Function, already
   have such a dispatcher installed. This function is provided for
   client-side use with custom types which do _not_ inherit from
   one of the built-in prototypes.
*/
int whcl_install_command_cb(whcl_engine * const el,
                            cwal_value * const tgt);

/**
   Works like whcl_install_command_cb() but takes a cwal_callback_f
   which gets installed as a command handler into the given target
   object. It gets added with the CONST/HIDDEN flags.

   Returns 0 on success.

   @see whcl_function_forward()
*/
int whcl_install_command_cb2(whcl_engine * const el,
                              cwal_callback_f cb,
                              cwal_value * const tgt);


/**
   A proxy for cwal_function_forward() which works identically except
   that it checks to see if f has the WHCL_CONTAINER_F_XSYM flag
   and, if so, it adjusts the current scope flags to take that into
   account. The primary (perhaps the only) use case for this in whcl
   client code is when clients implement their own `__command`
   dispatchers and need to ensure that the container flags of the
   being-dispatched-to function, rather than the `__command` function,
   are taken into account.

   @see whcl_install_command_cb2()
*/
int whcl_function_forward( whcl_engine * const el, cwal_function * const f,
                           uint16_t trimArgCount, cwal_callback_args const * args,
                           cwal_value ** rv );

/**
   Flags for use with the 4th argument of whcl_function_call(). See that
   function for their meanings.
*/
enum whcl_function_call_e {
/**
   Signals the pending call scope that it should not act as a symbol
   lookup barrier, permitting symbol lookup to continue up the scope
   stack from this point instead of skipping to the global scope.
 */
WHCL_FCALL_F_XSYM = WHCL_CONTAINER_F_XSYM,
/**
   Signals the pending script-function call to re-use the current
   scope, instead of pushing a new one.
*/
WHCL_FCALL_F_REUSE_WSCOPE = 0x02
};

/**
   A wrapper around cwal_function_call() which works _almost_ identically
   except that:

   1) It ensures that the scope pushed for the call acts as a boundary
      for symbol lookup purposes _unless_ f has the
      WHCL_CONTAINER_F_XSYM client container flag set on it OR the 3rd
      argument contains the WHCL_FCALL_F_XSYM flag.

   2) If the 3rd argument has the WHCL_FCALL_F_REUSE_WSCOPE bit set
      then the caller is assumed to have already pushed and populated
      a whcl_scope for the call (using whcl_scope_push() then
      whcl_scope_set_v() and friends). The proxied call will, _if f is
      a script function_, retain that whcl_scope for holding the usual
      call-local vars such as `argv` and `this`.

   The 3rd argument's flags have no effect on non-script functions.

   Results are undefined if f is NULL or invalid.
*/
int whcl_function_call( cwal_function * f,
                        cwal_value * self,
                        cwal_flags16_t callFlags,
                        cwal_value ** resultVal,
                        uint16_t argc,
                        cwal_value * const * argv );

/**
   Installs a property named `__typename` into the given
   properties-capable value, with the hidden/const attributes. Returns
   0 on success, sets el's error or exception state and returns non-0
   on error. It generally triggers an exception rather than a
   non-exception error, but will trigger a non-exception for
   especially serious problems (e.g. OOM).

   On success "someone" will take a new reference to the given name
   value. On error no reference is taken.
*/
int whcl_install_typename_v(whcl_engine * const el, cwal_value * const tgt,
                            cwal_value * const name);
/**
   A proxy for whcl_install_typename_v() which differs only in how it
   accepts its argument (a NUL-terminated string).
*/
int whcl_install_typename(whcl_engine * const el, cwal_value * const tgt,
                          char const * name);

/**
   Returns true if the given value is not NULL and is currently being
   constructed via whcl's `new` builtin command, else returns false.
*/
bool whcl_is_newing(cwal_value const * const v);

/**
   cwal_callback_f() wrapper for cwal_glob_matches_cstr().

   Script-side usages:

   bool glob( string glob, string haystack [, int globStyle=-1] )

   bool glob( string haystack [, int globStyle=-1] )

   Glob matching styles (3rd glob() param):

   <0 = (default) wildcard-style (case sensitive)

   0 = SQL LIKE style (case insensitive)

   >0 = SQL LIKE style (case sensitive)

   If args->self is-a string then this function behaves like the
   second form: it expects that args->self is a string to match glob
   pattern args->argv[0] against, using the optional policy specified
   by args->argv[1]. If args->self is not a string, it behaves like
   the first.
*/
int whcl_cb_glob_matches_str( cwal_callback_args const * args,
                              cwal_value ** rv );

/**
   This cwal_callback_f() impl functions identically to
   whcl_cb_glob_matches_str() except that the order of the glob and
   haystack arguments are swapped, with the haystack on the left.
*/
int whcl_cb_matches_glob_str( cwal_callback_args const * args,
                              cwal_value ** rv );

/**
   Callback signature for whcl module import routines.

   When called by whcl_module_load(), whcl_module_init(), or similar, this
   function type is passed the associated whcl_engine instance and the
   client-provided module result value address.

   Implementations "should" (by convention) return their module by
   assigning it to `*module`. Optionally, they may use the
   whcl_engine's facilities to store the functionality long-term (in
   terms of value lifetimes), e.g. using whcl_stash_set(), or even
   forcing them into the top-most scope. In any case, they should, on
   success, assign some result value to `*module`, even if it's
   NULL. Note, however, that NULL is not generally a useful result
   value. Most modules return a "namespace object" which contains the
   module's functionality.

   When assigning to `*module`, the API expects that this function
   will not hold any extraneous references to the returned
   value. i.e. if it's a new Value with no circular references, its
   refcount "should" be zero when `*module` is assigned to and 0 is
   returned.

   @see whcl_module_load()
*/
typedef int (*whcl_module_init_f)( whcl_engine * const el, cwal_value ** module );

/**
   Holds information for mapping a whcl_module_init_f to a name.
   Its purpose is to get installed by the WHCL_MODULE_xxx family of
   macros and referenced later via a module-loading mechanism.
*/
struct whcl_loadable_module{
  /**
     Symbolic name of the module.
  */
  char const * name;

  /**
     The initialization routine for the module.
  */
  whcl_module_init_f init;
};

/** Convenience typedef. */
typedef struct whcl_loadable_module whcl_loadable_module;

/**
   If compiled without WHCL_ENABLE_MODULES defined to a true value
   then this function always returns CWAL_RC_UNSUPPORTED and updates
   the error state of its first argument with information about that
   code.

   Its first argument is the controlling whcl_engine.

   Its second argument is the name of a DLL file.

   Its third argument is the name of a symbol in the given DLL which
   resolves to a whcl_loadable_module pointer. It may be NULL, in which
   case a default symbol name is used (which is only useful when
   plugins are built one per DLL).

   The final parameter is the target for the module's result value,
   and it may not be NULL (but the value it points to should initially
   be NULL, as it will be overwritten). It is passed directly to the
   module's whcl_loadable_module::init() function, which is responsible
   for (on success) assigning `*mod` to the value the module wants to
   return.

   This function tries to open a DLL named fname using the system's
   DLL loader. If none is found, CWAL_RC_NOT_FOUND is returned and the
   whcl_engine's error state is populated with info about the error. If
   one is found, it looks for a symbol in the DLL: if symName is not
   NULL and is not empty then the symbol "whcl_module_symName" is
   sought, else "whcl_module". (e.g. if symName is "foo" then it
   searches for a symbol names "whcl_module_foo".) If no such symbol is
   found then CWAL_RC_NOT_FOUND (again) is returned and the
   whcl_engine's error state is populated, else the symbol is assumed to
   be a (whcl_loadable_module*), its init() function is called, and its
   result is returned to the caller of this function.

   On error, this routine generally updates the whcl_engine's error
   state with more info (e.g. the name of the symbol on a symbol
   lookup failure). Not all errors update the engine's error state,
   only those with more information to convey than the result code. On
   error, the result Value (final parameter) is not modified.

   Returns 0 on success.

   Note that the API provides no mechanism for unloading DLLs because
   it is not generically possible to know if it is safe to do
   so. Closing a DLL whose resources (e.g. a native class definition
   for a client-bound type) are still in use leads, of course, to
   undefined results. The caveat, however, is that because dlopen()
   and friends allocate memory when we open DLLs, and we don't close
   them, valgrind reports this (rightfully) as a leak. It is not so
   much a leak as it is a required safety net. That said, the
   interpreter will close all DLLs it opened (or believes it opened)
   when it is finalized. That, however, opens up another potential
   problem: interpreters will close a DLL one time for each time they
   opened it. How the underlying (system-level) module API deals with
   that is up to that API. The dlopen()-based and lt_dlopen()-based
   implementations are safe in that regard (at least on Linux,
   according to their man pages and a peek at their sources).

   In practice this routine is not called by client C code, but is
   instead called indirectly via whcl_cb_module_load(), which is a
   script-side binding of this function.

   @see whcl_cb_module_load()
   @see WHCL_MODULE_DECL
   @see WHCL_MODULE_IMPL
   @see WHCL_MODULE_REGISTER
   @see WHCL_MODULE_REGISTER_
*/
int whcl_module_load( whcl_engine * const el, char const * fname,
                      char const * symName, cwal_value ** mod );

/**
   Behaves similarly to whcl_module_load(), and its first 3 parameters
   are used as documented for that function, but this variant does not
   invoke the init() method of the module before returning that module
   via `*mod`.

   On success `*mod` is set to the module object. Its ownship is kinda
   murky: it lives in memory made available via the module loader. It
   remains valid memory until the DLL is closed.

   Returns 0 on success. On error, el's error state may contain more
   information.

   After calling this, the next call would typically be
   whcl_module_init().

   @see whcl_module_load()
   @see whcl_module_init()
*/
int whcl_module_extract( whcl_engine * const el,
                         char const * dllFileName,
                         char const * symName,
                         whcl_loadable_module const ** mod );

/**
   This function pushes a new cwal scope, calls mod->init(), and
   propagates any result value from that routine back out of that new
   scope via `*rv`. On error `*rv` is not modified. If rv is NULL then the
   Value result of the module init is ignored (destroyed before this
   routine returns unless the module stores is somewhere long-lived),
   but any integer result code of the init routine is propagated back
   to the caller of this function and the init routine might update
   el's error state.

   @see whcl_module_load()
   @see whcl_module_extract()
*/
int whcl_module_init( whcl_engine * const el,
                      whcl_loadable_module const * mod,
                      cwal_value ** rv);

/** @def WHCL_MODULE_DECL

   Declares an extern (whcl_loadable_module*) symbol called
   whcl_module_#\#NAME.

   Use WHCL_MODULE_IMPL to create the matching implementation
   code.
   
   This macro should be used in the C or H file for a loadable module.
   It may be compined in a file with a single WHCL_MODULE_IMPL1()
   declaration with the same name, such that the module can be loaded
   both with and without the explicit symbol name.

   @see WHCL_MODULE_IMPL
*/
#define WHCL_MODULE_DECL(NAME)                            \
    extern const whcl_loadable_module * whcl_module_##NAME

/** @def WHCL_MODULE_IMPL
   
   Intended to be used to implement module declarations.  If a module
   has both C and H files, WHCL_MODULE_DECL(NAME) should be used in the
   H file and WHCL_MODULE_IMPL() should be used in the C file. If the
   DLL has only a C file (or no public H file), WHCL_MODULE_DECL is
   unnecessary.

   Implements a static whcl_loadable_module object named
   whcl_module_#\#NAME#\#_impl and a non-static (whcl_loadable_module*)
   named whcl_module_#\#NAME which points to
   whcl_module_#\#NAME#\#_impl. (The latter symbol may optionally be
   declared in a header file via WHCL_MODULE_DECL.)

   INIT_F must be a whcl_module_init_f() function pointer. That function
   is called when whcl_module_load() loads the module.

   This macro may be combined in a file with a single
   WHCL_MODULE_IMPL1() declaration using the same NAME value, such that
   the module can be loaded both with and without the explicit symbol
   name.

   Example usage, in a module's header file, if any:

   ```
   WHCL_MODULE_DECL(mymodule);
   ```

   (The declaration is not strictly necessary - it is more of a matter
   of documentation.)
   
   And in the C file:

   ```
   WHCL_MODULE_IMPL(mymodule,mymodule_module_init);
   ```

   If it will be the only module in the target DLL, one can also add
   this:
   
   ```
   WHCL_MODULE_IMPL1(mymodule,mymodule_install_to_interp);
   // _OR_ (every so slightly different):
   WHCL_MODULE_STANDALONE_IMPL(mymodule,mymodule_install_to_interp);
   ```

   Which simplifies client-side module loading by allowing them to
   leave out the module name when loading, but that approach only
   works if modules are compiled one per DLL (as opposed to being
   packaged together in one DLL).
   
   @see WHCL_MODULE_DECL
   @see WHCL_MODULE_IMPL1
*/
#define WHCL_MODULE_IMPL(NAME,INIT_F)                                     \
  static const whcl_loadable_module                                       \
  whcl_module_##NAME##_impl = { #NAME, INIT_F };                          \
  const whcl_loadable_module * whcl_module_##NAME = &whcl_module_##NAME##_impl
/** @def WHCL_MODULE_IMPL3

    A variant of WHCL_MODULE_IMPL for cases where a module name
    is not a legal C symbol name. The first argument is the C-friendly
    name, the 2nd is the human-friendly one, and the 3rd is the module
    init function.
*/
#define WHCL_MODULE_IMPL3(CNAME,NAME,INIT_F)                            \
  static const whcl_loadable_module                                       \
  whcl_module_##CNAME##_impl = { #NAME, INIT_F };                          \
  const whcl_loadable_module * whcl_module_##CNAME = &whcl_module_##CNAME##_impl


/** @def WHCL_MODULE_IMPL1

   Implements a static whcl_loadable_module symbol called
   whcl_module_impl and a non-static (whcl_loadable_module*) named
   whcl_module which points to whcl_module_impl

   INIT_F must be a whcl_module_init_f.
   
   This macro must only be used in the C file for a loadable module
   when that module is to be the only one in the resuling DLL. Do not
   use it when packaging multiple modules into one DLL: use
   WHCL_MODULE_IMPL for those cases (WHCL_MODULE_IMPL can also be used
   together with this macro).

   @see WHCL_MODULE_IMPL
   @see WHCL_MODULE_DECL
   @see WHCL_MODULE_STANDALONE_IMPL
*/
#define WHCL_MODULE_IMPL1(NAME,INIT_F)                                \
  static const whcl_loadable_module                                   \
  whcl_module_impl = { #NAME, INIT_F };                               \
  const whcl_loadable_module * whcl_module = &whcl_module_impl

/** @def WHCL_MODULE_STANDALONE_IMPL

    WHCL_MODULE_STANDALONE_IMPL() works like WHCL_MODULE_IMPL1() but
    is only fully expanded if the preprocessor variable
    WHCL_MODULE_STANDALONE is defined (to any value).  If
    WHCL_MODULE_STANDALONE is not defined, this macro expands to a
    dummy placeholder which does nothing (but has to expand to
    something to avoid leaving a trailing semicolon in the C code,
    which upsets the compiler (the other alternative would be to not
    require a semicolon after the macro call, but that upsets emacs'
    sense of indentation (and keeping emacs happy is more important
    than keeping compilers happy (all of these parens are _not_ a
    reference to emacs lisp, by the way)))).

    This macro may be used in the same source file as
    WHCL_MODULE_IMPL.

    The intention is that DLLs prefer this option over
    WHCL_MODULE_IMPL1, to allow that the DLLs can be built as
    standalone DLLs, multi-plugin DLLs, and compiled directly into a
    project (in which case the code linking it in needs to resolve and
    call the whcl_loadable_module entry for each built-in module).

   @see WHCL_MODULE_IMPL1
   @see WHCL_MODULE_REGISTER
*/
#if defined(WHCL_MODULE_STANDALONE)
#  define WHCL_MODULE_STANDALONE_IMPL(NAME,INIT_F) WHCL_MODULE_IMPL1(NAME,INIT_F)
#else
#  define WHCL_MODULE_STANDALONE_IMPL(NAME,INIT_F) \
  extern void whcl__module_dummy_does_not_exist_()
#endif

/** @def WHCL_MODULE_REGISTER

   Performs all the necessary setup for registering a loadable module,
   including declaration and definition. NAME is the name of the
   module. This is normally called immediately after defining the
   plugin's init func (which is passed as the 2nd argument to this
   macro).

   See WHCL_MODULE_IMPL() and WHCL_MODULE_STANDALONE_IMPL() for
   the fine details.
*/
#define WHCL_MODULE_REGISTER(NAME,INIT_F)  \
  WHCL_MODULE_IMPL(NAME,INIT_F);           \
  WHCL_MODULE_STANDALONE_IMPL(NAME,INIT_F)
/** @def WHCL_MODULE_REGISTER3

    A variant of WHCL_MODULE_REGISTER for cases where a module name
    is not a legal C symbol name. The first argument is the C-friendly
    name, the 2nd is the human-friendly one, and the 3rd is the module
    init function.
 */
#define WHCL_MODULE_REGISTER3(CNAME,NAME,INIT_F)                    \
  WHCL_MODULE_IMPL3(CNAME,NAME,INIT_F); \
  WHCL_MODULE_STANDALONE_IMPL(NAME,INIT_F)

/**
   Functionally equivalent to:
   WHCL_MODULE_REGISTER(NAME, whcl_module_init_#\#NAME).
*/
#define WHCL_MODULE_REGISTER_(NAME)                             \
  WHCL_MODULE_IMPL(NAME,whcl_module_init_##NAME);          \
  WHCL_MODULE_STANDALONE_IMPL(NAME,whcl_module_init_##NAME)


/**
   cwal_callback_f() impl which wraps whcl_module_load().

   Script-side usages:

   // For single-module DLLs:
   var module = loadModule("filename");
   // Or, for multi-module DLLs:
   var module = loadModule("filename", "symbolName");

   On success it returns the module's value (which can be anything),
   or the undefined value if the module returns nothing (which would be
   unusual).

   If passed a NULL symbol name, it assumes that the DLL is a
   single-module DLL and uses the symbol name "whcl_module". If passed
   a symbol name, it looks for "whcl_module_SYMBOL_NAME". If such a
   symbol is found it is assumed to be a (whcl_loadable_module const
   *) and its init() function is called.

   On success 0 is returned. On error it throws or returns a
   lower-level error code (e.g. CWAL_RC_OOM). This function will
   intercept any script-level `return` triggered by the module
   initialization and treat it as the result value.

   Achtung: there is no module caching going on here, and loading a
   module multiple times may be expensive or confusing (the returned
   objects from separate calls will, unless the module itself somehow
   caches results, be different instances).

   Achtung: this binding requires that whcl_engine_from_args() return
   non-0 (it will do so if args->engine is managed by an whcl_engine
   instance).
*/
int whcl_cb_module_load( cwal_callback_args const * args,
                         cwal_value **rv );

/**
   Loads, if needed, one of a hard-coded set of APIs into the
   script-side `whcl` object as `whcl[apiName]`. If the given name
   matches one of the built-in ones, it is installed/initialized and
   `*rv` (if rv is not NULL) is assigned to its value. If
   `whcl[apiName]` already exists when this is called, this function
   becomes a no-op and returns that value via `*rv` (if rv is not
   NULL). If the given name does not match an existing object or one
   of the built-in/installable APIs, an exception is triggered and
   non-0 is returned.

   The list of built-in API names can be fetched via
   whcl_install_api_list().

   whcl_install_core_apis() installs the script-visible function
   `whcl[install-api]` which is wrapper around this function. It
   accepts one or more string arguments naming an API. It installs, if
   needed, the API and its script-side result is the object associated
   with the final API it loads (the `wchl[X]` value for the final
   argument `X`). It throws if passed an unknown API name and the
   resulting message contains the list of all known APIs. If called
   with no arguments, it instead returns a space-delimited string
   of all modules known by that function.

   In addition to the built-in set of APIs, this library may be built
   with additional external modules which are exposed via this
   function. Thus it _might_ have more than those listed above.
*/
int whcl_install_api(whcl_engine * const el, char const * apiName,
                     cwal_value **rv);

/**
   Returns the built-in list of APIs which are usable with
   whcl_install_api(). If `oneString` is not NULL, `*oneString` is
   assigned a static string which lists all available API names in an
   unspecified order. If `strList` is not NULL then `*strList` is
   assigned a NULL-terminated array of those module names in an
   unspecified order.

   The names in the returned lists are suitable for use as the 2nd
   argument to whcl_install_api(). The first form is guaranteed to be
   formatted like "X Y Z", where X, Y, and Z are module names
   separated by a single space, with no leading or trailing space.

   The returned bytes are immutable and static, so have no particular
   lifetime-/ownership-related issues.
*/
void whcl_install_api_list(char const **oneString,
                           char const * const ** strList);


/**
   A convenience routine to bind a callback function as a constructor
   for use with whcl's "new" builtin command.
   
   Installs method as a hidden/const property named "__new" in the
   given container. The method parameter is assumed to conform to the
   "new" command's constructor conventions.

   This has the same effect as setting the property oneself except
   that this routine uses a cached copy of the key string and sets the
   property to hidden/const.

   Returns 0 on success, else:

   - CWAL_RC_MISUSE if any argument is NULL.

   - CWAL_RC_OOM on allocation error.

   - CWAL_RC_TYPE if !cwal_props_can(container).

   - CWAL_RC_CONST_VIOLATION if container already has a constructor
   property and it is const (otherwise it is overwritten).

   An example of customizing the constructor with state:

   ```
   int rc;
   cwal_function * f;
   cwal_value * fv;
   f = cwal_new_function( el->ec, my_callback_f, my_callback_state,
                          my_callback_state_finalizer_f, my_type_id );
   if(!f) return CWAL_RC_OOM;
   fv = cwal_function_value(fv);
   assert(f == cwal_value_get_function(fv)); // will always be the case if f is valid
   cwal_ref(fv);
   rc = whcl_ctor_method_set( el, myContainer, f );
   cwal_unref(fv);
   return rc;
   ```

   In such a setup, from inside the my_callback() implementation,
   cwal_args_state(args->engine, my_type_id) can be used to
   (type-safely) fetch the my_callback_state pointer. The whcl_engine
   instance associated with the call can be fetched via
   whcl_engine_from_args().

   @see whcl_ctor_callback_set()
*/
int whcl_ctor_method_set( whcl_engine * const el, cwal_value * const container,
                          cwal_function * const method );

/**
   A convenience form of whcl_ctor_method_set() which instantiates a
   cwal_function from the its 3rd argument using cwal_new_function().
   Returns CWAL_RC_MISUSE if any argument is NULL, else returns as for
   whcl_ctor_method_set().
*/
int whcl_ctor_callback_set( whcl_engine * const el,
                            cwal_value * const container,
                            cwal_callback_f method );

/**
   Invokes a "constructor" function in the same manner as the 'new'
   builtin command does. 

   If ctor is NULL then it looks in operand
   (which must be a container type) for a function-type property named
   `__new`. If that property is not found or is not a Function, el's
   error state is set and non-0 is returned.

   For purposes of construction, the default prototype used by the new
   object depends on how this function is called: if ctor is not NULL
   then its prototype is used. Else if a `__new` constructor method
   property is found in the operand, that constructor is used and, the
   operand argument is the prototype. If ctor is NULL and no
   constructor method is found in the operand, CWAL_RC_TYPE is
   returned and the engine's error state is updated.

   All arguments except args and ctor must be non-NULL.

   This routine abstractly does the following:

   - creates a new Object.

   - sets operand to be the new object's prototype.

   - calls the ctor, passing on the new object as the "this" and any
   arguments in the args array (which may be NULL or empty).

   - if that ctor returns successfully: if it returns a cwal_value
   other than cwal_value_undefined() then that value is assigned to
   *rv, otherwise the newly-created Object is set to *rv. Thus a
   "return this", "return undefined", "return" and an implicit return
   all cause the newly-created object to be returned.  The intention
   is to allow constructors to return a different object which should
   then be substituted in the original's place (this is what "new"
   does with it). That capability is required for returning, e.g.,
   dynamically-loaded cwal_native-bound values from a constructor.

   This all happens in an internally-pushed scope, but *rv will be
   rescoped to the caller's scope (for lifetime management purposes)
   if necessary.

   ACHTUNG: make sure you have a ref point on all parameters, else
   this function might end up cleaning any one of them up. If passed,
   e.g. an args array with no reference point, the passed-in array
   may be cleaned up by this function.

   On error, non-0 is returned and *rv will be assigned to 0.

   TODO: a variant of this which takes a C array of cwal_value
   pointers and the length of that array, as that's generally easier
   to work with in client code (but an array-based impl is what the
   "new" keyword needs).
*/
int whcl_ctor_apply( whcl_engine * const el, cwal_value * const operand,
                     cwal_function * ctor, cwal_array * args,
                     cwal_value **rv );

/**
   This odd function is intended to assist in certain types of
   bindings which want to store, e.g. a custom Prototype. This
   function creates a new key using cwal_new_unique() and uses it
   store 'what' in 'where' with the CWAL_VAR_F_HIDDEN and
   CWAL_VAR_F_CONST flags. This binding will ensure that 'what' gets
   referenced and rescoped as necessary to keep it alive along with
   'where'. One caveat: if cwal_props_clear(where) is called, it will
   delete that binding, possibly leading to unexpected behaviour in
   other places which are expecting 'what' to be alive.

   A concrete example: custom modules often need a custom prototype,
   but don't really have a good place to store it. One option is to
   attach it as state to the Function which acts as that type's
   constructor (because the prototype is typically only invoked, at
   the C level, from there). However, Function state is of type
   (void*), and thus is not "Value-aware" and cannot be rescoped as
   needed.  One workaround to keep that prototype alive is to store it
   as a Value in the Function. Because the key is unknown, it cannot
   be looked up, but it can (with a little extra work) be accessed via
   cwal_function_state_get() and friends. The binding created by
   _this_ function keeps that state alive so long as the property does
   not get removed.

   Reminder to self: it might be interesting to add a new
   CWAL_VAR_F_xxx flag which tells even cwal_props_clear() not to
   remove the property. That would be somewhat more invasive and
   special-case than i'm willing to hack right now, though. :/
*/
int whcl_stash_hidden_member( cwal_value * const where,
                              cwal_value * const what );

/**
   This is a simple wrapper around whcl_scope_set_v() which sets the
   scope-level variable `this` to the given value. The main benefit of
   this function is that it uses a cached copy of the `this` string so
   might not have to allocate anything.

   Returns 0 on success, else some error code propagated from
   whcl_scope_set_v().

   If `that` is NULL then the `this` var is removed from the current
   scope.

   Trivia: in client-side wrappers it's often useful to set a specific
   `this` for code blocks which callbacks are intended to eval.
*/
int whcl_set_this(whcl_engine * const el, cwal_value * const that);

/**
   Feature flags for use with whcl_feature_flag_set().
*/
enum whcl_engine_feature_e {
/**
   Tells the whcl_engine to enable execution of:

   `__debug {...}`

   Without the flag, such blocks are not executed.

   Note that `pragma __debug` can be used to change this setting from
   scripts.
*/
WHCL_FEATURE_F_DEBUG_BLOCK,
/**
   When enabled, all script-side `assert` calls are logged to the
   engine's output channel.

   From script code, `pragma trace-assert` can be used to enable this
   by passing it a value which is true when masked with 0x01.
*/
WHCL_FEATURE_F_TRACE_ASSERT,
/**
   When enabled, all script-side `affirm` calls are logged to the
   engine's output channel.

   From script code, `pragma trace-assert` can be used to enable this
   by passing it a value which is true when masked with 0x02.
*/
WHCL_FEATURE_F_TRACE_AFFIRM
};

/**
   Enables or disables, depending on the third argument, the feature
   of whcl_engine described by the second argument.
*/
void whcl_feature_flag_set( whcl_engine * const el,
                            enum whcl_engine_feature_e,
                            bool on );

/**
   "Compiles" the given script source code into a new whcl_script
   object, which can then (on success) be passed to whcl_script_eval()
   to run it.

   `*tgt` must, for proper behavior, initially point to NULL, as
   opposed to being uninitialized. Future potential changes to this
   function will break code which violates that.

   len must be the number of bytes in src to process. If it is
   negative then cwal_strlen() is used to calculate it. On success
   `*tgt` is assigned to the new script instance and ownership of it
   is transferred to the caller, who must eventually pass it to
   whcl_script_free() to free its memory. On error `*tgt` is not
   modified. Errors are reported by returning non-0 and setting the
   error state in the underlying cwal_engine instance.

   If scriptName is not NULL, it must be a NUL-terminated string and
   it gets copied for use as the script's name for error-reporting
   purposes.

   The resulting whcl_script object does not own any cwal_value-level
   state, so has no lifetime issues related to script-engine scoping
   and whatnot, but it _must not_ outlive the engine for which it was
   compiled. i.e. it must be freed before the engine is finalized.

   @see whcl_compile_buffer()
   @see whcl_compile_buffer_take()
   @see whcl_script_free()
   @see whcl_script_eval()
*/
int whcl_compile( whcl_engine * const el,
                  whcl_script ** tgt,
                  char const * scriptName,
                  char const * const src, cwal_int_t len );

/**
   This convenience wrapper for whcl_compile() differs only in that it
   takes its input source code in the form of a buffer. It otherwise
   functions identically to that function. If the caller will not need
   the memory in the buffer after the compilation process,
   whcl_compile_buffer_take() is preferred, as it takes over the
   buffer's memory rather than making a copy for the compiled script.

   @see whcl_compile()
   @see whcl_compile_buffer_take()
   @see whcl_script_free()
   @see whcl_script_eval()
*/
int whcl_compile_buffer( whcl_engine * const el, whcl_script ** tgt,
                         char const * scriptName,
                         cwal_buffer const * const buf );

/**
   This variant of whcl_compile() differs in that (1) it
   accepts its source code as a cwal_buffer object and (2) on success
   it takes over the memory of that buffer (and it must not be
   modified after that). Any non-buffer-memory state of the buffer is
   retained (e.g. if it's a buffer from cwal_new_buffer_value(), its
   Value state is retained). On error, ownership of src->mem is not
   changed.

   @see whcl_compile()
   @see whcl_compile_buffer()
   @see whcl_script_free()
   @see whcl_script_eval()
*/
int whcl_compile_buffer_take(whcl_engine * const el, whcl_script ** tgt,
                             char const * const name,
                             cwal_buffer * const src);

/**
   This is a convenience wrapper aroud whcl_compile() which reads its
   contents from the given filename. Argument and return semantics are
   as for whcl_compile(), these notes:

   - If `scriptName` is not NULL, it is copied for use as the script's
     name, else `filename` is used for that purpose.

   - It reading the file fails it may return any code documented for
     cwal_buffer_fill_from_filename().

   On error, el's error state is updated.
*/
int whcl_compile_file( whcl_engine * const el, whcl_script ** tgt,
                       char const * filename,
                       char const * scriptName);

/**
   If cp is not NULL, finalizes it to free up any resources it owns, then
   frees its own memory.
*/
void whcl_script_free( whcl_script * const cp );

/**
   If v, or one of its prototypes, is a cwal_native value created by
   the whcl framework to wrap a whcl_script object, this returns the
   matching whcl_script instance, else it returns NULL.
*/
whcl_script * whcl_value_get_script(cwal_value * const v);

/**
   Flag bits for use with whcl_script_eval().
*/
enum whcl_script_eval_e {
/**
   Indicates that whcl_script_eval() must push a new whcl scope (as
   opposed to cwal_scope) before processing.
*/
WHCL_SCRIPT_EVAL_SCOPE = 0x01
};

/**
   Runs the given script object and evaluates it in the context of
   of the whcl_engine which was used to whcl_compile() it.

   Returns 0 on success. If rv is not NULL then on success the final
   result of the eval is assigned to `*rv`. On error `*rv` is not
   modified.
*/
int whcl_script_eval(whcl_script * const sc, uint32_t flags,
                     cwal_value ** rv);

/**
   Frees any current name string owned by scr and and replaces it with
   a copy of the given name. If the length is negative, cwal_strlen()
   is used to calculate it. Returns 0 on success, CWAL_RC_OOM on
   error. On allocation error of a new name, the previous name is not
   modified.

   As a special case, if name is NULL then the script's name,
   if any, is freed and its name is set to NULL.
*/
int whcl_script_name_set(whcl_script * const scr,
                         char const * name,
                         cwal_int_t len);

/**
   Returns either t->name or the first name encountered while climbing
   up t's parent chain. Returns NULL if no name is found. If len is
   not 0 then if this function returns non-0, len is set to that
   name's length. The bytes are owned by t or one of its parents, so
   must be copied if that lifetime might be problematic.
*/
char const * whcl_script_name_get( whcl_script const * t,
                                   cwal_midsize_t * const len );

/**
   Fetches the line/column offset of the given script.  Returns true
   if src has an initial token (from which the position is extracted),
   else false. Either of the 2nd or 3rd arguments may be NULL. If they
   are not NULL and this function returns false, they are set to 0
   (which is an invalid line number).

   Line numbering starts with 1, column numbering with 0 (because
   that's how emacs does it).

   Note that top-level scripts will have a position of (1,0), but
   "inner" scripts retain their offset origin from their parent.

   @see whcl_script_renumber()
*/
bool whcl_script_linecol(whcl_script const * const src,
                         whcl_linecol_t * line,
                         whcl_linecol_t * col);

/**
   Renumbers the lines of a script starting at the given values.  The
   given column is only used for the offset of the script's first
   line, and can normally be left at 0. All lines after the first
   retain their column values. When a script is compiled from within
   another script it may (depending on how it was constructed) have a
   non-0 column offset for the first token.

   If a line value of 0 is passed in, it is treated as 1.

   @see whcl_script_linecol()
*/
void whcl_script_renumber(whcl_script * const src,
                          whcl_linecol_t line,
                          whcl_linecol_t col);

/**
   Dumps all sorts of cwal/whcl metrics to the given engine's
   current cwal-level output channel.
*/
void whcl_dump_metrics(whcl_engine * const el);

typedef struct whcl_tmpl_opt whcl_tmpl_opt;
/**
   Holds options for the whcl_tmpl_to_code() function.  Clients
   must initialize them by copying either whcl_tmpl_opt_empty or
   (for const contexts) whcl_tmpl_opt_empty_m.  */
struct whcl_tmpl_opt {
    /**
       This may be set to a command (optionally with flags) which will
       be passed each token of parsed output. It is passed a single
       string argument at a time and should collect or output it
       as-is, with no additional spacing or formatting.

       The command need not be resolvable during the code-generation
       step, but needs to be so when the resulting code is eval'd.

       If this is empty then `echo -n -s` is used.
    */
    char const * outputCommand;

    /**
       This specifies the name of the processed-template-internal
       heredoc delimiter. By default (if this is 0 or empty) some
       cryptic combination of non-ASCII UTF8 character is used.
    */
    char const * heredocId;

    /**
       The opening tag for "code" blocks. Default is `<?`.
       If set, then tagCodeClose must also be set. Must differ
       from all other tag open/close entries.
     */
    char const * tagCodeOpen;

    /**
       The opening tag for "code" blocks. Default is `?>`.
       If set, then tagCodeOpen must also be set. Must differ
       from all other tag open/close entries.
    */
    char const * tagCodeClose;

    /**
       The opening tag for "expression" blocks. Default is `<%`.
       If set, then tagExprClose must also be set. Must differ
       from all other tag open/close entries.
    */
    char const * tagExprOpen;

    /**
       The opening tag for "expression" blocks. Default is `%>`.
       If set, then tagExprOpen must also be set. Must differ
       from all other tag open/close entries.
    */
    char const * tagExprClose;
};

/**
   An initialized-with-defaults instance of whcl_tmpl_opt,
   intended for const-copy initialization.
*/
#define whcl_tmpl_opt_empty_m {0,0,0,0,0,0}

/**
   An initialized-with-defaults instance of whcl_tmpl_opt,
   intended for copy initialization.
*/
extern const whcl_tmpl_opt whcl_tmpl_opt_empty;

/**
   Implements a very basic text template processing mechanism for
   whcl.

   The el arg must be a valid whcl_engine instance.

   src must be the template source code to process. It is treated as
   nearly-opaque text input which may contain markup tags (described
   below) to embed either code blocks or values into the output.

   dest is where all output is appended (the buffer is not reset by
   this function).

   The opt parameter may be 0 (for default options) or an object which
   configures certain parts of the template processing, as described
   in the whcl_tmpl_opt docs.

   Returns 0 on success, non-0 on error. Error codes include:

   - CWAL_RC_MISUSE if !src or !dest.

   - CWAL_RC_RANGE if any of the open/close tags specified in the opt
   parameter are invalid (empty strings or validate rules described in
   the whcl_tmpl_opt docs).

   - CWAL_RC_OOM on allocation errors.

   - CWAL_RC_EXCEPTION if it is reporting an error via a cwal
   exception. On code generation errors it throws an exception
   containing information about the nature and location (in the
   original source) of the problem.

   That said, it may not catch many conceivable malformed content
   cases and in such cases may generate malformed (as in not
   eval'able) code.

   Template processing...

   (Note that while these docs use fixed tag names, the exact
   tags can be configured via the opt parameter.)

   All non-code parts of src are filtered to be output wrapped in
   individual HEREDOCs embedded in the output script. All code parts
   of src are handled as follows:

   `<?` starts a code block, running until and closing `?>` tag. This
   ends any current HEREDOC and passes through the code as-is to dest.
   It does not generate any output in the processed document unless
   the embedded code generates it by calling the output command.

   `<%` starts an "expression block," which is processed a little bit
   differently. The contents between that and the next `%>` tag are
   evaluated as a single expression and the result is passed to the
   configured output command (see below).

   An example input document should clear this up:

   ```
   Hi, world!
   <? decl x 1; decl y 2; ?>
   x = <%$x%>, y = <% $y %>, x+y=<% $x + $y %>
   ```

   The generated code is an whcl script which, when eval'd, outputs a
   processed document. All non-script parts get wrapped in HEREDOCs
   for output. When eval'd, the above script outputs:

   ```
   Hi, world!
   x = 1, y = 2, x+y=3
   ```

   The generated code "should" be evaluated in a scope of its own, but
   it can be run in the current scope if desired. The code relies on
   an output command being defined (resolvable in the evalution
   scope). The command must accept any number of parameters and output
   them "in its conventional string form" (whatever that is). It must
   not perform any formatting such as spaces between the entries or
   newlines afterwards. It may define formatting conventions for
   values passed to it (e.g. it may feel free to reformat doubles to a
   common representation).

   The generator outputs some weird/cryptic UTF8 symbols as heredoc
   markers. It's conceivable, though very unlikely, that these could
   collide with symbols in the document for heredoc processing
   purposes, in which case they can be redefined via the `heredocId`
   member of the options object.

   Whitespace handling:

   - If the script starts with `<?` or `<%`, any whitespace leading up
   to that are discarded, otherwise all leading whitespace is
   retained.

   - Replacement of `<% %>` and `<? ?>` blocks retains whitespace to the
   left of the openener and right of the closer, so `{abc<%$x%>def}` will
   form a single output token (provided 'x' evaluates to such), where
   `{abc <%$x%> def}` will generate three. Inside the `<? ?>` blocks, all
   whitespace is retained. Inside `<% %>` blocks, the contents are
   treated as if they were inside a normal HEREDOC, so their
   leading/trailing spaces are stripped BUT they are not significant -
   the _result_ of evaluating the `<% %>` content gets output when
   executed, not the content itself.

   TODOs:

   - a variant which takes a cwal_output_f() instead of a buffer.
*/
int whcl_tmpl_to_code( whcl_engine * const el, cwal_buffer const * const src,
                       cwal_buffer * const dest, whcl_tmpl_opt const * opt );

/**
   A cwal_callback_f() binding for whcl_tmpl_to_code(). It expects one
   string/buffer argument containing tmpl code and it returns a new
   script or buffer value containing the processed code. Throws on error.

   Script usage:

   ```
   decl compiled [thisFunction [-buffer] templateSource [, optObject]]
   ```

   By default it returns a new whcl_script instance (via cwal_native)
   but the -buffer flag tells it to return the tmpl-generated source
   code as a buffer object instead.

   If optObject is-a Object then the following properties of that
   object may influence template processing:

   - `expr-open` and `expr-close` specify the open/close tags for
     Expression Blocks.

   - `code-open` and `code-close` specify the open/close tags for Code
     Blocks.

   - `output-command` sets the name of the symbol the generated
     template code will (when eval'd) use for output. It may be any
     string which expands to the name of a script-visible command,
     including optional leading flags. The default is unspecified but
     functionally equivalent to `echo -n -s`. The command need not be
     resolvable during the code-generation step, but needs to be so
     when the resulting code is eval'd.
*/
int whcl_cb_tmpl_to_code( cwal_callback_args const * args, cwal_value ** rv );

/**
   A cwal_json_override_f() impl which handles JSONizing of
   wchl_script-type values. The only use for this function is
   assigning to the cwal_json_output_opt::override.f member.  Its
   state argument is ignored, so cwal_json_output_opt::override.state
   may be NULL.
*/
int cwal_json_override_f_whcl(cwal_engine * e, cwal_value * src,
                              cwal_value **rv, void * state);

/**
   Creates a new plain object which represents the JSON-able state of
   the given script. On success, returns 0 and assigns `*rv` the new
   value (an Object). On error, returns non-0 and does not modify
   `*rv`. The only known error, assuming all arguments are in order,
   is CWAL_RC_OOM.

   The returned object has the following properties representing
   the script's state:

   - "script": source code
   - "name": script name, if set, else this property is elided

*/
int whcl_script_to_jsonable( whcl_script * const scr,
                             cwal_value ** rv );

/**
   If str, ignoring any leading (but not trailing) spaces, is a
   well-formed integer, this function returns true and updated
   `*result` (if result is not NULL) to the parse value. Returns false
   and does not modify `*result` if parsing fails.

   slen is the length of the string, or cwal_strlen() is used
   if slen is negative.

   It permits any integer formats supported by whcl script code
   (decimal, octal with leading 0o, binary with leading 0b, and hex
   with leading 0x) with an optional sign prefix.
*/
bool whcl_parse_int(char const * str, cwal_int_t slen,
                    cwal_int_t * result );

/**
   This uses a combination of whcl_parse_int() and cwal_cstr_to_double()
   on the given string to try to determine if it's valid, then
   updates `*rv` with its result.

   If it cannot parse, it sets `*rv` to NULL and returns 0.

   If it can parse, it sets `*rv` to a new integer or double value
   and returns 0. Returns CWAL_RC_OOM if allocation of a value
   fails. The result value follows all of the normal cwal_value
   lifetime rules.
*/
int whcl_parse_number( cwal_engine * const e, char const * src,
                       cwal_int_t slen, cwal_value ** rv );

/**
   Returns a pointer to a static/const cwal_json_output_opt instance
   which provides the whcl-wide default values for JSON output. Most
   notably, the object's `override.f` member is set to a function
   which knows to how to look up and apply `to-jsonable` member
   functions.

   If passed true, the returned pointer is set up for "formatted"
   output, including indentation and whatnot. If passed false, it's
   geared more towards compact output.
*/
cwal_json_output_opt const * whcl_json_output_opt(bool formatted);

#ifdef __cplusplus
}/*extern "C"*/
#endif

#endif
/* include guard */
