/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/*
  License: same as cwal. See cwal.h resp. cwal_amalgamation.h for details.
*/
#ifndef NET_WANDERINGHORSE_CWAL_S2_H_INCLUDED_
#define NET_WANDERINGHORSE_CWAL_S2_H_INCLUDED_
/** @file */ /* for doxygen */

#include "s2_config.h"
#include "libcwal.h"

/** @page page_s2 s2 scripting language

   s2 is a light-weight yet flexible scripting language based on the
   cwal (Scripting Engine Without A Language) library. cwal provides
   the abstract Value types, memory lifetime management, and garbage
   collection systems, and s2 provides a scripting language on top of
   that.

   A brief overview of properties of s2 which are of most interest to
   potential clients:

   - Library licensing: dual Public Domain/MIT, with one optional
   BSD-licensed part (the JSON input parser) which can be compiled
   out if that license is too restrictive.

   - A lightweight, portable[1] C API for binding client-defined
   functionality to script-space, making it scriptable and trackable
   by the script's garbage collector. Tests so far put s2 on par
   with lua in terms of memory usage.

   - s2's primary distributable is two C files: one header and one
   implementation file, called the "amalgamation build," intended for
   direct drop-in use in arbitrary client source trees. The canonical
   build tree is intended primarily for development of cwal/s2, not
   for development of client applications. The amalgamation includes
   the cwal amalgamation, so it need not be acquired separately.

   - Has an expression-oriented syntax with a distinct JavaScript
   flavor, but distinctly different scoping/lifetime rules.

   - cwal's garbage collector guarantees that client-specific
   finalizers (for client-specified types and, optionally, for
   function bindings) get called, provided it is used properly, though
   it cannot always guaranty the order of destruction.

   - cwal and s2 are developed with the compiler set to pedantic
   warning/error levels (and them some), and the s2 test suite
   includes valgrind-based testing and reporting. New code rarely hits
   the trunk without having run the whole test suite through valgrind.

   - Extending s2: the default s2 shell (s2sh) provides not only a
   copy/paste bootstrap for creating client applications, but can be
   extended by clients via script code or C (linking their features in
   directly in or loading them via DLLs) without modifying its
   sources, as described in the manual linked to below.


   [1] = Portable means: C99 (as of July 2021). The canonical copy is
   developed using the highest level of compiler warnings, treating
   all warnings as errors (i.e. -Wall -Werror -Wpedantic -Wextra).

   In addition to these API docs, s2 as a whole is described in gross
   amounts of detail in the s2/manual directory of its canonical
   source tree, browsable online at:

   https://fossil.wanderinghorse.net/r/cwal/doc/ckout/s2/index.md

   That covers both the scripting language itself (in detail) and how
   to use it in C (mainly via overviews and links to working code).
*/
#include "s2_t10n.h"
#include <time.h> /* struct tm */
#include <stdio.h> /* FILE * */

#ifdef __cplusplus
extern "C" {
#endif
typedef struct s2_engine s2_engine;
typedef struct s2_scope s2_scope;
typedef struct s2_stoken s2_stoken;
typedef struct s2_stoken_stack s2_stoken_stack;
typedef struct s2_estack s2_estack;
typedef struct s2_sweep_guard s2_sweep_guard;
typedef struct s2_func_state s2_func_state;
/** Convenience typedef. */
typedef struct s2_strace_entry s2_strace_entry;

enum s2_rc_e {
S2_RC_placeholder = CWAL_RC_CLIENT_BEGIN1,
/**
   Used internally by routines which visit lists of keys/values
   to provide a mechanism for aborting traversal early without
   triggering an error.
*/
S2_RC_END_EACH_ITERATION,

/**
   To be used only by the 'toss' keyword, if it ever gets added.
*/
S2_RC_TOSS,

/** Sentinel. */
S2_RC_end,
/**
   Client-thrown exceptions which use their own error codes "should
   not" use codes below this value. Put differently, result codes
   starting at this value are guaranteed not to collide with
   CWAL_RC_xxx and S2_RC_xxx codes. Keep in mind that numerous
   CWAL_RC_xxx and S2_RC_xxx codes have very specific meanings in
   various contexts, so returning, from cwal/s2-bound client-side
   code, values which just happen to collide with those may confuse s2
   and/or cwal into Doing The Wrong Thing. (Examples: CWAL_RC_OOM is
   used solely to propagate out-of-memory (OOM) conditions, and
   handling of CWAL_RC_RETURN is context-specific.)
*/
S2_RC_CLIENT_BEGIN = CWAL_RC_CLIENT_BEGIN2
};

/**

   Enum specifying the precedences of operators in s2.

   Derived from:

   http://n.ethz.ch/~werdemic/download/week3/C++%20Precedence.html (now 404)

   http://en.cppreference.com/w/cpp/language/operator_precedence

   Regarding operator associativity:

   Wikipedia:

   http://en.wikipedia.org/wiki/Operator_associativity

   Says:

   "To prevent cases where operands would be associated with two
   operators, or no operator at all, operators with the same
   precedence must have the same associativity."

   We don't strictly follow that advice (by accident) but have not had
   any unexpected problems in that regard.
*/
enum s2_precedents {
S2_PR__start = 1,

/**
   Parens precedence is actually irrelevant here, as we parse parens
   groups as atomic values instead of operators.
*/
S2_PR_ParensOpen,
S2_PR_Comma,

/**
   Internal pseudo-operator for RHS evaluation in some case.

   No longer used - can be removed.
*/
S2_PR_RHSEval,

/**
   =  +=  -=  *=   /=  <<=  >>=  %=   &=  ^=  |=
*/
S2_PR_Assignment__,
/**
   The = operator.
*/
S2_PR_OpAssign =S2_PR_Assignment__,
/**
   PHP-style array-append. Essentially works like (array DOT index =
   ...), where the index is the array's current length. Only usable in
   assignment contexts.
*/
S2_PR_ArrayAppend = S2_PR_Assignment__,

/* S2_PR_Conditional__, */
/**
   In JavaScript ternary if has a higher precedence than
   assignment, but we're going to go with the C/C++ precedence
   here.
*/
S2_PR_TernaryIf = S2_PR_Assignment__,

S2_PR_Logical__,
S2_PR_LogicalOr = S2_PR_Logical__ /* || */,
S2_PR_LogicalOr3 = S2_PR_Logical__ /* ||| */,
S2_PR_LogicalOrElvis = S2_PR_Logical__ /* ?: */,
S2_PR_LogicalAnd = S2_PR_Logical__ /* && */,

S2_PR_Bitwise__,
S2_PR_BitwiseOr = S2_PR_Bitwise__,
S2_PR_BitwiseXor = S2_PR_Bitwise__ + 1,
S2_PR_BitwiseAnd = S2_PR_Bitwise__ + 2,

S2_PR_Equality__,
S2_PR_CmpEq = S2_PR_Equality__,
S2_PR_CmpEqStrict = S2_PR_Equality__,
S2_PR_CmpNotEq = S2_PR_Equality__,
S2_PR_CmpNotEqStrict = S2_PR_Equality__,

S2_PR_Relational__,
S2_PR_CmpLT = S2_PR_Relational__,
S2_PR_CmpGT = S2_PR_Relational__,
S2_PR_CmpLE = S2_PR_Relational__,
S2_PR_CmpGE = S2_PR_Relational__,
S2_PR_OpInherits = S2_PR_Relational__,

/**
   '=~'. Should this have Equality precedence?
*/
S2_PR_Contains = S2_PR_Relational__,
/**
   '!~'. Should this have Equality precedence?
*/
S2_PR_NotContains = S2_PR_Relational__,

/*
  TODO?

  <== and >== for strict-type LE resp GE

  ==< resp ==> for strict LT/GT
*/

S2_PR_Bitshift__,
S2_PR_ShiftLeft = S2_PR_Bitshift__,
S2_PR_ShiftRight = S2_PR_Bitshift__,

S2_PR_Additive__,
S2_PR_Plus = S2_PR_Additive__,
S2_PR_Minus = S2_PR_Additive__,

S2_PR_Multiplicative__,
S2_PR_Multiply = S2_PR_Multiplicative__,
S2_PR_Divide = S2_PR_Multiplicative__,
S2_PR_Modulo = S2_PR_Multiplicative__,

S2_PR_Unary__,
S2_PR_PlusUnary = S2_PR_Unary__,
S2_PR_MinusUnary = S2_PR_Unary__,
S2_PR_LogicalNot = S2_PR_Unary__,
S2_PR_BitwiseNegate = S2_PR_Unary__,
S2_PR_Keyword  = S2_PR_Unary__,
S2_PR_IncrDecr  = S2_PR_Unary__,
/**
   Used by 'foreach' to tell evaluation to stop at this operator.  Its
   precedence must be higher than S2_PR_Comma but lower than anything
   else.
*/
S2_PR_OpArrow2 /* => */ = S2_PR_Unary__,
/* C++: S2_PR_Unary__ ==>

   sizeof, new, delete, & (addr of), * (deref),
   (typeCast), 
*/

/* S2_PR_Kludge__, */
/* S2_PR_DotDot = S2_PR_Kludge__, */

S2_PR_Primary__,
S2_PR_FuncCall = S2_PR_Primary__,
S2_PR_Subscript = S2_PR_Primary__,
S2_PR_Braces = S2_PR_Primary__,
S2_PR_DotDeref = S2_PR_Primary__,
S2_PR_DotDot = S2_PR_DotDeref,
S2_PR_OpColon2 = S2_PR_DotDeref,
S2_PR_OpArrow = S2_PR_DotDeref,


/*
  C++: S2_PR_Primary__ ==>

  typeid(), xxx_cast

*/


S2_PR_Specials__,
S2_PR_ParensClose,
S2_PR_NamespaceCpp /* :: */,
S2_PR_end__
};

/**
   @internal

   Represents a combination value/operator for an s2_engine.  Each
   token represents one operand or operator for an s2 evaluation
   stack. They get allocated often, but recycled by their associated
   s2_engine, so allocations after the first few stack-pops are
   O(1) and cost no new memory.

   Token instances must not be in use more than once concurrently,
   e.g. a token may not be in more than one stack at a time, nor may
   it be in the same stack multiple times. Multiple entries may
   reference the same value, provided it is otherwise safe from being
   swept/vacuumed up.
*/
struct s2_stoken{
  /**
     A s2_token_types value.
  */
  int ttype;
  /**
     Certain token types have a value associated with them.  The
     tokenization process will create these, but will not add a
     reference to them (because doing so complicates lifetimes, in
     particular for result values which need up-scoping). This means
     the client must be careful when using them, to ensure that they
     get a ref if one is needed, and to either clean them up or
     leave them to the GC if they don't want them.
  */
  cwal_value * value;

  /**
     Used for creating chains (e.g. a stack).
  */
  s2_stoken * next;

  /**
     Used by the parser to communicate source code location
     information to the stack machine. This is necessary so that
     errors generated at the stack machine level (the operator
     implementations) can report the location information, though the
     stack machine does not otherwise know anything about the source
     code.
  */
  s2_ptoken srcPos;
};


/**
   Empty-initialized s2_stoken structure, intended for
   const-copy initialization.
*/
#define s2_stoken_empty_m {                     \
    S2_T_INVALID/*ttype*/,                      \
    0/*value*/,                               \
    0/*next*/,                                \
    s2_ptoken_empty_m/*srcPos*/               \
  }

/**
   Empty-initialized s2_stoken structure, intended for
   copy initialization.
*/
extern const s2_stoken s2_stoken_empty;
/**
   Holds a stack of s2_stokens.
*/
struct s2_stoken_stack {
  /**
     The top of the stack.

     Maintained via s2_stoken_stack_push(),
     s2_stoken_stack_pop(), and friends.
  */
  s2_stoken * top;
  /**
     Number of items in the stack.
  */
  int size;
};

/**
   Empty-initialized s2_stoken_stack structure, intended for
   const-copy initialization.
*/
#define s2_stoken_stack_empty_m {0,0}

/**
   Empty-initialized s2_stoken_stack structure, intended for
   copy initialization.
*/
extern const s2_stoken_stack s2_stoken_stack_empty;

/**
   An "evaluation stack," a thin wrapper over two s2_stoken_stacks,
   intended to simplify swapping the stacks in and out of an s2_engine
   while parsing subscripts. s2 internally uses a distinct stack per
   sub-eval, rather than one massive stack, as it's simply easier to
   manage. When the main eval loop is (re)entered, a new eval stack is
   put on the C stack (a local var of the eval function) and s2_engine
   is pointed to it so we know which stack is current.

   Reminders to self:

   - 20160131 this type was designed to _not_ ref/unref values added
   to the stack, but that may have turned out to be a mistake. It
   simplifies much other code but string interning combined with our
   internal use of "temp" values leads to unmanagable/incorrect
   refcounts for interned string values in some cases, triggering
   cwal-level corruption detection assertions. Rewiring s2 so that
   this type does much of the reference count managent for a given
   (sub)expression is certainly worth considering, but could require a
   weekend or more of intense concentration :/. Until then, the main
   eval routine uses a "ref holder" array to keep all being-eval'd
   temporaries alive and valid for the duration of the eval. That
   requires an extra cwal_array, but it isn't all that costly if
   memory recycling is on (which it should be except when testing new
   code (as recycling often hides memory misuse)).

   Current (20160215) plans:

   - expand this class to include:

   - Add a (cwal_engine*) or (s2_engine *) handle, as deps allow for
   (see below, regarding s2_engine::skipMode, for why we may need the
   latter).

   - Done: Add a (cwal_array *), allocated in the cwal_scope which is
   active when the stack was initialized. Each token pushed into
   this->vals is added to the array as a NULL entry if it has no
   cwal_value, else that value is placed in the array. This array is
   made vacuum-safe, which would eliminate s2's need for its vacsafe
   flags. It would also hold refs, so no sweep-up during the stack's
   useful lifetime.  When this->vals is popped, we either unhand or
   unref the value, depending on whether we want to (semantically)
   discard it or not. See below for a novella on this topic (it's got
   a happy ending, at least in theory).

   - We'd need to fix a few places in s2's eval engine which
   create/push a token and then add the value themselves. They would
   need to make the s2_estack aware of the token. We could do that by
   limiting token allocation to the s2_estack-internal API.

   - Need explicit init/finalizer funcs for s2_stoken_stack, to clean
   up the stash array.

   - Pushing a value token to this->vals adds it to the current stash
   array at the same index position as the stack length-1. This allows
   us to use the array as a ref holder and vacuum-safe environment for
   the values. We don't want to use it as a replacement for
   s2_stoken_stack because that needs to hold non-cwal_value state
   (and making that type manage ref handling would require a rather
   more invasive round of refactoring to clean up ref handling
   _everywhere_ else because moving ref handling into that type would
   truly bugger some stuff up (essentially all operator handlers, and
   most of the keywords, would need some touching).

   Pros and such:

   - All in-use values get refs and can be made vac-safe. This should
   eliminate any of s2's "vacsafe" flag handling (which is relatively
   invasive). It can eliminates the array used by s2_eval_ptoker(), at
   least in theory. No... i think we still need that one to ensure
   that the pending result value (between expressions) is vacuum-safe.

   - optimization: if an s2_estack starts life when
   (s2_engine::skipLevel>0) then it doesn't need a stash array because
   skipLevel will never (legaly) return to 0 within the lifetime of
   that s2_estack (because skipLevel is also incremented/decremented
   in a stack-like fashion). We need a flag for this or simply check
   this->se->skipLevel in the appropriate places. If we instead add
   the creation of the stash array as a init-time option, we can
   remove a ref on s2_engine to this class (but we'd need a
   cwal_engine ref, i any case). Having such an option might
   internally allow a simpler migration, as we could use the no-array
   setting for the first refactoring step to keep behaviour the same.

   - With that in place, we can remove a deep-seated cwal-core
   property which really annoys me: because of a less-complete
   understanding (on my part) of the intricacies of refcounting, in
   particular the handling of temp vals (in particular in conjunction
   with string interning), th1ish required the following behaviour of
   cwal: during scope cleanup (and only at scope cleanup), when
   being-finalized containers reference a value in an older scope and
   that value has (because of this container's cleanup) a refcount of
   0, the value is not destroyed, but re-temp'd back in its own scope.
   This was required to accommodate return propagation in many
   cases. It is not required, however, if the client simply does the
   right things vis-a-vis refcounting and rescoping. So...  with this
   in place, once we prove that s2 doesn't need that cwal-level
   feature, we can remove that. (i, just now, ran s2's full test suite
   (with various recycling/memory usage option combinations) without
   this behaviour, and s2 handled it fine). However, that would also
   require abandoning th1ish altogether because fixing it to do proper
   ref/unref everywhere would be far more effort than it is worth (the
   only place th1ish is used, AFAIK, is demo apps). (In my defense, at
   the time i _thought_ th1ish _was_ doing the right thing. i have
   since, via s2, discovered that that was not the case.)

   Cons (maybe):

   - We need to add a layer of push/pop APIs and keep the s2 core from
   using the s2_stoken_stack push/pop, as those don't partake in ref
   lifetime tracking.

   - Adding a stash array adds potental allocation errors for push()
   cases which are currently fail-safe. e.g. pushing a value cannot
   currently fail because it's a linked list, but adding it to an
   array might require allocating the array (on the first push) or
   allocation memory for the underlying cwal_value list (on any push).

   - We'd need a hack to keep the stash array from freeing its list
   memory when popped, because often fill/empty the same value stack
   throughout an expression. (i think... maybe...)  Ideally, we could
   use cwal_array_length_set(ar, poppedStackLength) to simply trim the
   list as it's popped, but setting the length to 0 implicitly frees
   (or recycles) the underlying (cwal_value*) list memory. We'd want
   to, as an allocation optimization, avoid that. Maybe not use
   length_set() at all, but simply cwal_array_set(ar, pos, 0), and
   keep a potentially ever-growing array.

   - It will cost an array per subexpression because
   s2_eval_expr_impl() (the main eval loop) stashes/replaces the
   s2_estack each time it is entered/leaves. We cannot change that in
   conjunction with a "stash" array because value lifetimes would go
   pear shaped quickly: the "stash" array would be global-scope (or
   close to it), which would implicity rescope all stack-pushed values
   into that scope. It would be a worst-case scenario for garbage
   collection. Experimentation with adding such an array to
   s2_eval_expr_impl() shows that it works but that the array
   allocation counts (or alloc requests) shoot through the roof, and
   real alloc counts skyrocket if value recycling is disabled. We need
   (for cwal_value lifetime reasons) a new instance for each pushed
   cwal_scope. Aha... we could have s2_estacks have a parent
   (s2_estack*), add a (cwal_scope*) to keep track of which scope it
   was initially initialized in, and inherit (via normal ref counting)
   the array. Yes. Yesssss...  that just might work nicely. We'd also
   need to track exactly which stack created the array, so we know
   which one needs to... no, we don't...  refcounts will clean it
   up. Indeed. Since we already effectively push/pop s2_estack
   instances in s2_eval_expr_impl(), the code is already structured to
   make adding/referencing a parent pointer easy to do. The stash
   array's allocation must NOT be done lazily: it must be done in the
   cwal_scope active when the stack was initialized (and within which
   it must be finalized) because the array must be allocated in a
   scope appropriate for its values, and it might not create any or
   might create them after a couple scopes have been pushed. Hmmm. In
   which case we'd need to be sure to rescope it (right after
   allocation) to s2_estack::scope (new member, a (cwal_scope*)).
   When allocating the array, we can check if the (grand...)parent's
   scope is the same. If it is, borrow its array (if it has one), else
   alloc a local array and assign it back to the parent (only if it's
   in the same scope) as a optimization for the parent's further use
   (as well as for use by a downstream child-s2_estack of that
   scope). DAMN... if we do all that, we'd need s2_estack to track the
   length the array had when it first got ahold of it, and only add
   items at that position and subsequent, and then clean up to its
   starting position when done. Hmmm. Ugly. i'd rather not share the
   arrays than do that :/. It would save some memory, though. Maybe an
   RFC after the simpler (array instance per s2_estack) approach is
   tried out and measured for allocations.
*/
struct s2_estack{
  /** The value stack. */
  s2_stoken_stack vals;
  /** The operator stack. */
  s2_stoken_stack ops;
};

/**
   Empty-initialized s2_estack structure, intended for const-copy
   initialization.
*/
#define s2_estack_empty_m {s2_stoken_stack_empty_m, s2_stoken_stack_empty_m}

/**
   Empty-initialized s2_estack structure, intended for copy
   initialization.
*/
extern const s2_estack s2_estack_empty;

/** @internal

    An internal helper type for swapping an s2_engine's sweep-guard
    state in and out at certain points.

    The docs for this struct assume only one context: that this is
    used embedded in an s2_engine struct.
*/
struct s2_sweep_guard {
  /**
     If greater than 0, s2_engine_sweep() will neither sweep nor
     vacuum.

     Reminder to self: we will likely never be able to use recursive
     sweep. It's inherently dangerous due to how we (mis)handle
     refs during eval.
  */
  int sweep;
  /**
     If greater than 0, s2_engine_sweep() will not vacuum, but will
     fall back to sweep mode (if not disabled) instead. This HAS to
     be >0 if the client makes use of any non-script-visible values
     which are not otherwise vacuum-proofed and may be needed at a
     time when script code may trigger s2_engine_sweep() (see
     cwal_value_make_vacuum_proof()).
  */
  int vacuum;

  /**
     Helps keeps track of when to sweep and vacuum - incremented once
     per call to s2_engine_sweep().
  */
  int sweepTick;
};

/** @internal

    Empty-initialized s2_sweep_guard state, intended for const
    initialization.
*/
#define s2_sweep_guard_empty_m {0,0,0}

/** @internal

   Empty-initialized s2_sweep_guard state, intended for non-const
   copy initialization.
*/
extern const s2_sweep_guard s2_sweep_guard_empty;

/** @internal

   A strictly internal type used for swapping out certain internal
   state when crossing certain subexpression boundaries (namely, block
   constructs).
*/
struct s2_subexpr_savestate {
  int ternaryLevel;
  /* Others are pending, but i've forgotten what they might need
     to be. */
};
typedef struct s2_subexpr_savestate s2_subexpr_savestate;
#define s2_subexpr_savestate_empty_m {0}

/**
   An abstraction layer over cwal_scope so that we can store
   more per-scope metadata.

   Potential TODO (20190911): add a cwal_native wrapper to each scope
   to allow script-side code to potentially do some interesting things
   with scopes. Each instance would have a prototype with methods like
   get(), isDeclared(), declare(key,val[,asConst]), and a const
   property named parent which points to the parent scope native.
   It's not 100% clear what we might want to do with such a beast,
   other than add a __scope keyword which accesses it.
*/
struct s2_scope {
  /**
     The cwal_scope counterpart of this scope. It's owned by cwal and
     must outlive this object. This binding gets set/unset via the
     cwal_scope_push()/pop() hooks.
  */
  cwal_scope * cwalScope;
  
  /**
     Current sweep/vacuum-guard info.
  */
  s2_sweep_guard sguard;

  /**
     Some internal state which gets saved/cleared when a scope is
     pushed and restored when it is popped.
  */
  s2_subexpr_savestate saved;

  /**
     A "safe zone" to store values which are currently undergoing
     evaluation and need a reference held to them. NEVER upscope this
     array to a different scope: it is internal-only, owned by this
     scope. We store this in the scope, rather than in the eval()
     routine, as an allocation optimization. This placement makes only
     a tiny difference when recycling is enabled, but when it's
     disabled, total allocation counts for the current (20171113) unit
     tests are cut by more than a third, from ~35k allocs down to
     ~21k, saving more than 1M of total alloc'd memory. (Woot! A
     _whole megabyte!_ ;)

     This member is only initialized via the main eval loop and only
     freed via the s2-side cwal_scope_pop() hook.

     Summary of why this is needed: the eval engine does not
     historically directly hold refs to temp values which are floating
     around in the eval stack while an expression is being eval'd.
     While that's not normally a problem, there are cases where such
     temp values can get freed up, in particular in the face of string
     interning, where multiple cwal_value pointers are all pointing at
     the same value yet the number of references is lower than the
     number of pointers. Fixing this properly, i.e. holding references
     to everything we put in the stack, would seemingly be a major
     undertaking. This evalHolder is a simpler workaround, but has the
     disadvantage of memory cost (whereas refs via the eval engine
     would not cost us anything we don't already have). "One of these
     days" i'd like to sit down and go patch the world such that the
     eval stack maintains references to its values, at which point we
     can get rid of evalHolder. That would require a fairly major
     effort to get all of the operators and other actors ironed out,
     and would likely introduce a very long tail of bugs while stray
     refs/unrefs were discovered. The eval holder works well, the only
     down-side being its memory cost (a few kb for s2's largest test
     scripts, _possibly_ as much as 8 or 10k on highly recursive tests
     involving require.s2).

     20181123: reminder to self: with this piece in place, a recursive
     sweep/vacuum might become legal. Hmmm. Something to try. If we
     can recursively vacuum then we can eliminate, i believe, all of
     the remaining "GC deathtrap" cases (barring malicious cases
     implemented in C to specifically bypass vacuuming by making
     values unnecessarily vacuum-proof).

     @see s2_eval_hold()
  */
  cwal_array * evalHolder;
  
  /**
     Internal flags.
  */
  uint16_t flags;
};

#define s2_scope_empty_m {\
    NULL/*cwalScope*/,                      \
    s2_sweep_guard_empty_m/*sguard*/,         \
    s2_subexpr_savestate_empty_m/*saved*/,    \
    0/*evalHolder*/,                      \
    0U/*flags*/                           \
}
extern const s2_scope s2_scope_empty;

/**
   This class encapsulates a basic stack engine which uses
   the cwal_value type as its generic operand type.

   Each s2_engine must be initialized with a cwal_engine,
   which the s2_engine owns and uses for all memory management,
   as well as the core Value Type system.

   @see s2_engine_alloc()
   @see s2_engine_init()
   @see s2_engine_finalize()
*/
struct s2_engine{
  /**
     The associated cwal_engine.
  */
  cwal_engine * e;

  /**
     A marker which tells us whether s2_engine_finalize() needs to
     free this instance or not.
  */
  void const * allocStamp;

  /**
     The stacks of operators and values. This container gets
     continually swapped out by the eval engine, such that each
     expression has its own clean stack (this is internally easier to
     handle, or seems to be).
  */
  s2_estack st;

  /**
     A general-purpose buffer for internal (re)use. Internal routines
     must be certain never to simply reset this buffer: it might be in
     use by higher-up calls in the stack. Always locally store
     buffer.used, use memory at offsets (buffer.mem+oldUsed), then
     re-set buffer.used=oldUsed and buffer.mem[oldUsed]=0 when
     finished, so that higher-up callers don't get the rug pulled out
     from under them. Growing the buffer is fine as long as all
     (internal) users properly address buffer.mem (and never hold a
     pointer to it).
  */
  cwal_buffer buffer;

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
  int skipLevel;

  /**
     Every this-many calls to s2_engine_sweep() should either sweep or
     vacuum. When 0, sweeping is disabled (generally not a good
     idea). The lowest-level evaluation routine disables sweeping
     during evaluation of an expression to keep the lifetimes of
     temporaries safe. s2_eval_ptoker(), a high-level eval routine,
     sweeps up after every expression, but only 1-in-skipInterval
     sweep-ups has an effect.

     Reminder to self: something to try: if s2_eval_expr_impl() uses
     an array to control lifetimes of temps (like s2_eval_ptoker()
     does, except that eval needs to keep more than one value at a
     time), we would almost not need sweeping at all, except to
     cleanup those pesky ignored return values, orphaned cycles, and
     such. That might also make a recursive sweep safe (recursive
     vacuum is theoretically not safe).

     A sweepInterval of 1 is very aggressive, but is recommended
     during testing/development because it triggers problems related
     to value lifetime mismanagement more quickly, in particular if
     vacuumInterval is also 1.

     Very basic tests on a large script with a milliseconds-precision
     and massif's instruction counter show only a relatively small
     impact, both in speed (no appreciable change) and total CPU
     instructions (less than 0.5%), when comparing sweepInterval of 1
     vs 7. The former used 90kb RAM and 215M CPU instructions, and the
     latter 92kb RAM and 214M instructions. i.e. there is (so far) no
     compelling argument for increasing this above 1.

     [Much later on... 20181123] That said, since all of the above
     text was written, it seems that all of the outlier cases
     involving temporarily stray values have been eliminated, and
     sweeping is essentially not happening in the whole s2 unit test
     suite (a total of 2 sweeps, as of this writing, meaning there's
     basically nothing to sweep). Vacuuming, on the other hand, is
     picking up stray cyclic values (and vacuuming is the only way to
     eliminate those (but only within the currently active
     scope)). That would argue for increasing the sweepInterval
     considerably, and maybe lowering the vacuumInterval to 1 or 2, so
     that every sweep becomes a vacuum (noting that vacuuming is
     computationally far more expensive). Even so... a sweepInterval
     of 1 helps detect ref/unref misuse more quickly, and the sooner
     such a problem is triggered, the easier it is to track down.  Few
     things are more irritating than an unpredictable crash caused by
     delayed garbage collection tripping over a value which was
     misused several scopes back.
  */
  int sweepInterval;

  /**
     Every this-many sweep attempts will be replaced by a vaccuum
     instead if this->scopes.current->sguard.vacuum is 0. There is no
     single optimal value. 1 is aggressive (always vacuuming instead
     of sweeping, potentially very costly). In generic tests in
     th1ish, 3-5 seemed to be a good compromise, and then only 1 in 10
     or 15 vacuum runs was cleaning up more than sweeping was, because
     vacuuming only does its real magic when there are orphaned cyclic
     structures laying around (which doesn't happen often). In scripts
     with short-lived scopes, a value of 0 here is fine because scope
     cleanup will also get those orphans, provided they're not
     propagated up out of the scope (via explicit propagation or
     containment in a higher-scoped container).
  */
  int vacuumInterval;

  /**
     Total number of s2_engine_sweep() calls which led to a sweep or
     vacuum.
  */
  int sweepTotal;

  /**
     Some sort of container used by s2_stash_get() and
     s2_stash_set().  This is where we keep internally-allocated
     Values which must not be garbage-collected. This value is made
     vacuum-proof.
  */
  cwal_value * stash;

  /**
     Internal holder for script function filename strings. This
     mechanism provides script functions with a lifetime-safe (and
     vacuum-safe) copy of their filename strings, while sharing
     like-stringed values with other function instances. It moves
     those strings into the top scope, and there's currently no
     mechanism for removing them. That's a bummer, but not a huge
     deal. The alternative would be duplicating the same (long)
     filename strings across all functions from a given file.
  */
  cwal_hash * funcStash;

  /**
     Holds the current ternary-if depth to allow the eval loop to
     provide better handling of (and error reporting for) ':' tokens.
  */
  int ternaryLevel;

  /**
     Used to communicate the "argv" value between the interpreter
     parts which call functions and the pre-call hook called by
     cwal.
  */
  cwal_array * callArgV;

  /**
     Gets set by the stack layer when an operator (A) triggers an
     error and (B) has its srcPos set. Used to communicate
     operator-triggered error location information back to the parser
     layer.
  */
  char const * opErrPos;

  /**
     Used for collecting error location info during the parse/eval
     phase of the current script, for places where we don't have
     a direct local handle to the script.
  */
  s2_ptoker const * currentScript;

  /**
     An experiment.

     20160125: reminder to self: the next time i write that without an
     explanation for my future self to understand, please flog
     me. Currently trying to figure out what this is for an am
     at a loss. It doesn't appear to do anything useful.

     20191209: used for implementing the experimental __using keyword
     to access "using" properties from inside a script function.
  */
  s2_func_state const * currentScriptFunc;

  /**
     A pointer to an app-internal type for managing "user-defined
     keywords".
   */
  void * ukwd;

  /**
     Various commonly-used values which the engine stashes away for
     its own use. Their lifetimes are managed via this->stash.
  */
  struct {
    /** The word "prototype". */
    cwal_value * keyPrototype;
    /** The word "this". */
    cwal_value * keyThis;
    /** The word "argv". */
    cwal_value * keyArgv;
    /** The word "__typename" (deprecated). */
    cwal_value * keyTypename;
    /** The word "script". */
    cwal_value * keyScript;
    /** The word "line". */
    cwal_value * keyLine;
    /** The word "column". */
    cwal_value * keyColumn;
    /** The word "stacktrace". */
    cwal_value * keyStackTrace;
    /**
       Name of the ctor function for the 'new' keyword ("__new").
    */
    cwal_value * keyCtorNew;
    /** The word "value". */
    cwal_value * keyValue;
    /** The word "name". */
    cwal_value * keyName;
    /** Part of an ongoing experiment - the word "interceptee". */
    cwal_value * keyInterceptee;
    /** The value for the s2out keyword. Initialized on its first use. */
    cwal_value * s2out;
    /** The value for the import keyword. Initialized on its first use. */
    cwal_value * import;
    /** The key for the import.doPathSearch flag. */
    cwal_value * keyImportFlag;
    /** The value for the define keyword. Initialized on its first use. */
    cwal_value * define;
  } cache;

  /**
     Stack-trace state.
  */
  struct {
    /**
       Stored as an optimization for sizing the target
       array when collecting stack traces.
    */
    cwal_size_t count;
    /**
       s2 will bail out if this call stack limit is reached, under the
       assumption that infinite recursion is going on. Set to 0 to
       disable the limt.

       Notes:

       - Only script-called functions count for this purpose, not
       calls to Functions (be they script functions or not) called via
       native code (cwal_function_call() and friends).

       - The unit test suite, as of 2014106, can get by with values
       under 10. The require.s2 tests cap out somewhere between 20 and
       25 levels. Because require.s2 is inherently deep-stacked,
       prudence dictates a 'max' value notably higher than that.
    */
    cwal_size_t max;
    /**
       Head of the current stack trace.
    */
    s2_strace_entry * head;
    /**
       tail of the current stack trace.
    */
    s2_strace_entry * tail;
  } strace;

  /**
     State for various internal recycling bins.
  */
  struct {
    /**
       Recycle bin for stokens.
    */
    s2_stoken_stack stok;

    /**
       The max number of items to keep in the stok recycler stack.
    */
    int maxSTokens;

    /**
       Recycle bin for script-function state.
    */
    struct {
      /** Head of the recyling list. */
      s2_func_state * head;
      /** Current number of entries in the list. */
      int count;
      /** Max allows entry count before we stop accepting new
          items into the list, and free them instead. */
      int max;
    } scriptFuncs;
  } recycler;

  /**
     Holds cwal_outputer instances managed by the s2_ob_push() family
     of functions.
  */
  cwal_list ob;
  /**
     Holds DLL/module handles so that the interpreter
     can close them when it cleans up.
  */
  cwal_list modules;

  /**
     State for propagating dot-operator metadata between the main
     eval() parts and the operator implementations.
  */
  struct {
    /**
       The dot and # operators sets this to the LHS container resp.
       hash. Extreme care must be taken to ensure that this does not
       point to a stale value.
    */
    cwal_value * lhs;

    /**
       Used to differentiate between prop access wrt to types which
       should bind as 'this' in calls to props resolved as
       functions. Namely:

       obj.FUNC() // binds 'this'

       array.NUMBER // does not

       hash # KEY // does not

       All three set dotOp.lhs and dotOp.key. Only "this-respecting"
       ops (namely, the DOT op) sets dotOp.self.
    */
    cwal_value * self;

    /**
       Set by the dot operator to its RHS (key) part. Used by
       assignment and/or the unset op to get access to the property
       name, as opposed to the resolved property value which the dot
       operator evaluates to.
    */
    cwal_value * key;
  } dotOp;

  /**
     Internal state for keeping s2's scopes in sync with cwal's via
     cwal_engine_vtab's cwal_scope_hook_push_f() and
     cwal_scope_hook_pop_f() mechanism.

     This addition allowed us to remove several now-obsolete
     s2-specific scope management functions and keep the
     cwal_scope/s2_scope stacks in perfect harmony, eliminating some
     weird potential/hypothetical error cases (or confusion-causing
     cases) where the scope stacks didn't line up as desired. This
     change really should have been made years ago. The only real cost
     for the s2 client is memory needed for a block of s2_scopes (via
     the 'list' member of this struct). Aside from that allocation
     (potentially, depending on the scripts, a small handful of
     reallocations totaling anywhere from 400 bytes to a few kb), it's
     been a 100% win.

     Added 20181123.
  */
  struct {
    /**
       Manages an array of s2_scope in response to cwal_scope push/pop
       events.

       How this memory is managed is not guaranteed by the API, but it
       is currently handled via cwal_realloc(), which means that it's
       counted among the memory counted in
       cwal_engine::memcap::currentMem.

       The engine will grow this list as necessary, but won't shrink
       it until the engine is finalized, at which point the memory is
       (of course) freed.

       Reminder to self (20181123): s2's various test scripts/suites
       have a max scope depth of anywhere from 9 to 22.
    */
    s2_scope * list;

    /**
       Points to memory in this->list for the current s2_scope. Note
       that it is generally NOT SAFE to keep a pointer to an s2_scope
       from this->list because any resize of the list can invalidate
       it, but this member is only modified in the routines which
       manage the list's size.
    */
    s2_scope * current;

    /**
       The number of entries reserved in this->list.
    */
    cwal_size_t alloced;

    /**
       A convenience counter which must always be the same as
       s2_engine::e::curent->level.
     */
    cwal_size_t level;

    /**
       We need a cwal_scope to act as our top scope. This is that
       scope.
    */
    cwal_scope topScope;

    /**
       An internal flag used only by s2_scope_push_with_flags() for
       passing flags to the scope-pushed hook.
    */
    uint16_t nextFlags;
  } scopes;
  
  /**
     Various internal flags.
  */
  struct {
    /**
       If greater than 0 then some debug/tracing output of the stack
       machine is generated. Use higher levels for more output. Note
       that this output goes to stdout, not s2's output channel.
    */
    int traceTokenStack;
    /**
       >0 means to trace PASSED assertions to cwal_output().
       >1 means to also trace FAILED assertions.
    */
    int traceAssertions;

    /**
       If true, sweeping keeps metrics (==performance hit) and
       outputs them to stdout. Only for debugging, of course.
    */
    int traceSweeps;

    /**
       If true (the default), creating exceptions generates a stack
       trace, else no stack trace is generated. This is simply a
       performance tweak: stack traces are normally exceedingly
       helpful but for test code which intentionally throws lots of
       exceptions, they are costly.

       Potential TODO: interpret this as a maximum depth, with 0 being
       unlimited and negative being disabled.
    */
    int exceptionStackTrace;
    
    /**
       To avoid that certain constellations miss an "interrupt"
       request, we need a flag for "was interrupted" which lives
       outside of the this->err state. TODO: also propagate
       OOM/EXIT/FATAL this way?
       
       TODO: do not clear this via s2_engine_err_reset(), and add
       a separate API for that. We really want interruption to trump
       everything.
    */
    volatile int interrupted;

    /**
       A bitmask of features which are explicitly disabled, from the
       s2_disabled_features enum.
    */
    cwal_flags32_t disable;
  } flags;

  /**
     Various metrics.
  */
  struct {
    /**
       Total number of s2_stoken_alloc() calls for this
       s2_engine instance.
    */
    unsigned int tokenRequests;
    /**
       Total number of calls into cwal_malloc() to allocate
       an s2_stoken.
    */
    unsigned int tokenAllocs;
    /**
       Number of tokens currently allocated but not yet
       freed nor recycled.
    */
    unsigned int liveTokenCount;
    /**
       Maximum number of s2_stokens alive throughout the life
       of this object.
    */
    unsigned int peakLiveTokenCount;

    /**
       Number of script-side assert()ions which have been run.
    */
    unsigned int assertionCount;
    /**
       Current sub-expression (e.g. parens/brace group) parsing
       leve.
    */
    int subexpDepth;
    /**
       The highest-ever sub-expression depth.
    */
    int peakSubexpDepth;
      
    /**
       Maximum number of cwal_scope levels deep concurrently.
    */
    cwal_size_t maxScopeDepth;
      
    /**
       Number of script-side functions created.
    */
    unsigned int funcStateRequests;

    /**
       Number of script-side functions for which we had
       to allocate an s2_func_state instance.
    */
    unsigned int funcStateAllocs;
    /**
       Total memory allocated for the internal state
       for script-side functions, NOT including
       their sourceInfo bits, as those are already
       recorded in the cwal metrics.
    */
    unsigned int funcStateMemory;

    /**
       The number of calls to s2_next_token().
     */
    unsigned int nextTokenCalls;

    /**
       The number of times which s2_engine::scopes::list is
       successfully grown to add new scopes. It does not increment on
       (re)allocation errors.
    */
    unsigned short totalScopeAllocs;

    /**
       The total number of cwal_scopes pushed via cwal_scope_push()
       and friends. This does not track the top-level scope which the
       cwal engine pushes before s2_engine can take over (which
       s2_engine immediately pops and re-pushes so that is can sync
       its scope levels with cwal's).
    */
    unsigned int totalScopesPushed;

    /**
       Counts how many times we reuse a script filename from
       this->funcStash.
    */
    unsigned int totalReusedFuncStash;
    /** Total number of times which either keyword lookup fell back to
        checking for a UKWD or the defined() keyword (or similar) made
        an explicit check for a UKWD. */
    unsigned ukwdLookups;
    /** Total number of ukwdLookups which resulted in a hit. */
    unsigned ukwdHits;
  } metrics;
};

/** @def S2_ENGINE_SWEEP_VACUUM_INTERVALS

   Two comma-separated integers representing the default values for
   s2_engine::sweepInterval and s2_engine::vacuumInterval (in that
   order).

   Long story short: the sweepInterval represent how often (measured
   in full expressions) s2_engine will "sweep", and the vacuumInterval
   represents every how many "sweeps" will be converted to a "vacuum"
   (a more costly operation, but it can weed out stray/unreachable
   cyclic structures). A sweepInterval of 1 is best when testing new
   code, as it potentially helps uncover ref/unref misuse more quickly
   than higher values do. vacuumInterval should only be 1 if
   sweepInterval is relatively high (e.g. 5-10).
*/
#if !defined(S2_ENGINE_SWEEP_VACUUM_INTERVALS)
#  if defined(NDEBUG)
#    define S2_ENGINE_SWEEP_VACUUM_INTERVALS 5/*sweepInterval*/, 2/*vacuumInterval*/
#  else
#    define S2_ENGINE_SWEEP_VACUUM_INTERVALS 1/*sweepInterval*/, 5/*vacuumInterval*/
#  endif
#endif

/** @def s2_engine_empty_m

   Empty-initialized s2_engine structure, intended for
   const-copy initialization.
*/
#define s2_engine_empty_m {                                     \
    0/*e*/,                                                     \
    0/*allocStamp*/,                                          \
    s2_estack_empty_m/*st*/,                                  \
    cwal_buffer_empty_m/*buffer*/,                            \
    0/*skipLevel*/,                                           \
    S2_ENGINE_SWEEP_VACUUM_INTERVALS,                          \
    0/*sweepTotal*/,                                          \
    0/*stash*/,                                               \
    0/*funcStash*/,\
    0/*ternaryLevel*/,                                      \
    0/*callArgV*/,                                            \
    0/*opErrPos*/,                                            \
    0/*currentScript*/,                                       \
    0/*ukwd*/,                                               \
    0/*currentScriptFunc*/,                                    \
    {/*cache*/\
      0/*keyPrototype*/,\
      0/*keyThis*/,\
      0/*keyArgv*/,\
      0/*keyTypename*/,\
      0/*keyScript*/,\
      0/*keyLine*/,\
      0/*keyColumn*/,\
      0/*keyStackTrace*/,\
      0/*keyCtorNew*/,\
      0/*keyValue*/,\
      0/*keyName*/,\
      0/*keyInterceptee*/,\
      0/*s2out*/,\
      0/*import*/,0/*keyImportFlag*/,                       \
      0/*define*/                                           \
    },                                                        \
    {/*strace*/0/*count*/, 100/*max*/, 0/*head*/,0/*tail*/},      \
    {/*recycler*/                                               \
      s2_stoken_stack_empty_m/*stok*/,                          \
      50 /*maxSTokens*/,                                      \
      {/*scriptFuncs*/ 0/*head*/, 0/*count*/, 20 /*max*/ }    \
    },                                                          \
    cwal_list_empty_m/*ob*/,                                  \
    cwal_list_empty_m/*modules*/,\
    {/*dotOp*/\
      0/*lhs*/,                                        \
      0/*self*/,                                       \
      0/*key*/                                           \
    },                                                      \
    {/*scopes*/                                                 \
      NULL/*list*/,                                             \
      NULL/*current*/,                                        \
      0/*alloced*/,                                           \
      0/*level*/,                                           \
      cwal_scope_empty_m/*topScope*/,                         \
      0/*nextFlags*/                                         \
    },                                                          \
    {/*flags*/                                                  \
      0/*traceTokenStack*/,                                     \
      0/*traceAssertions*/,                                   \
      0/*traceSweeps*/,                                       \
      1/*exceptionStackTrace*/,                             \
      0/*interrupted*/,                                     \
      0U/*disable*/                                        \
    },                                                      \
    {/*metrics*/                                                \
      0/*tokenRequests*/,                                       \
      0/*tokenAllocs*/,                                     \
      0/*liveTokenCount*/,                                \
      0/*peakLiveTokenCount*/,                              \
      0/*assertionCount*/,                                    \
      0/*subexpDepth*/,                                     \
      0/*peakSubexpDepth*/,                                 \
      0/*maxScopeDepth*/,\
      0/*funcStateRequests*/,                               \
      0/*funcStateAllocs*/,                                 \
      0/*funcStateMemory*/,                                \
      0/*nextTokenCalls*/,                                \
      0/*totalScopeAllocs*/,                              \
      0/*totalScopesPushed*/,                             \
      0/*totalReusedFuncStash*/,                        \
      0/*ukwdLookups*/,0/*ukwdHits*/              \
    }                                       \
  }

/**
   Empty-initialized s2_engine structure, intended for
   copy initialization.
*/
extern const s2_engine s2_engine_empty;


/**
   Initializes se and transfers ownership of e to it.

   se must be a cleanly-initialized s2_engine instance, allocated via
   s2_engine_alloc() or stack-allocated and copy-initialized from
   s2_engine_empty or s2_engine_empty_m (same contents, slightly
   different usage contexts).

   e must be a freshly-cwal_engine_init() instance and the client MUST
   NOT install any new functionality to it before this call because:
   in order to gain control of all the scopes, s2 must destroy all of
   e's scopes (it pushes a top scope upon initialization) before
   continuing with its own initialization.

   Returns 0 on success, a CWAL_RC_xxx code on error, the only
   potential ones at this phase of the setup being blatant misuse
   (passing in a NULL pointer) an allocation error.

   After returning:

   A) Ownership of se is not modified.

   B) If this function fails because either of the arguments is 0,
   ownership of e is not modified and CWAL_RC_MISUSE is returned,
   otherwise ownership of e is transferred to se regardless of success
   or failure (because there is no "detach and recover" strategy for
   any errors after the first couple allocations). On success or
   error, if both arguments are non-NULL, (se->e == e) will hold
   after this function returns.

   C) The caller must pass eventually se to s2_engine_finalize() to
   clean it up, regardless of success or error.

*/
int s2_engine_init( s2_engine * se, cwal_engine * e );

/**
   Allocates a new s2_engine instance using e's allocator. Returns 0
   on error.

   In practice this is not used: s2_engine instances are typically
   stack-allocated or embedded as a property of a larger object. In
   both such cases s2_engine_empty or s2_engine_empty_m should be used
   to empty-initialize their state before using them in any way.

   @see s2_engine_finalize()
   @see s2_engine_init()
   @see s2_engine_empty_m
   @see s2_engine_empty
*/
s2_engine * s2_engine_alloc( cwal_engine * e );

/**
   Frees up all resources owned by se. If se was allocated using
   s2_engine_alloc() then se is also freed, otherwise it is assumed
   to have been allocated by the caller (possibly on the stack) and
   is cleaned up but not freed.

   This call pops all cwal scopes from the stack, destroying any and
   all cwal_value instances owned by the engine, as well as cleaning
   up any memory owned/managed by those values (e.g. client-allocated
   memory managed via a cwal_native instance).

   @see s2_engine_alloc()
*/
void s2_engine_finalize( s2_engine * se );


/**
   Proxy for cwal_error_set().
*/
int s2_error_set( s2_engine * se, cwal_error * err, int code, char const * fmt, ... );

/**
   A proxy for cwal_error_exception().
*/
cwal_value * s2_error_exception( s2_engine * se,
                                 cwal_error * err,
                                 char const * scriptName,
                                 int line, int col );

/**
   Converts err (or se's error state, if err is NULL) to an Exception
   value using s2_error_exception() (see that func for important
   details) then set's se's exception state to that exception.
    
   Like cwal_exception_set(), this function returns CWAL_RC_EXCEPTION
   on success or some other non-0 code if creation of the exception
   fails (generally speaking, probably CWAL_RC_OOM).

   If line<=0 then err->line and err->col are used in place of the given
   line/column parameters.

   If script is 0 and err->script is populated, that value is used
   instead.

   To convert an error set via s2_engine_err_set() (or equivalent) to
   a cwal-level exception, pass 0 for all arguments after the first.
*/
int s2_throw_err( s2_engine * se, cwal_error * err,
                  char const * script,
                  int line, int col );

/**
   Sets se->e's error state (via cwal_error_setv()) and exception state
   (via cwal_exception_setf()) and returns CWAL_RC_EXCEPTION on
   success or a "more fatal" non-0 code on error.

   Special case: if the given code is CWAL_RC_OOM, this function has
   no side-effects and returns code as-is.
*/
int s2_throw( s2_engine * se, int code, char const * fmt, ... );

/**
   A proxy for cwal_engine_error_setv(). This does not
   set or modify the _exception_ state.
*/
int s2_engine_err_setv( s2_engine * se, int code, char const * fmt, va_list );

/**
   A proxy for cwal_engine_error_set().
*/
int s2_engine_err_set( s2_engine * se, int code, char const * fmt, ... );

/**
   If se has error state, that error code is returned, else if
   an exception is pending, CWAL_RC_EXCEPTION is returned, else
   0 is returned.
*/
int s2_engine_err_has( s2_engine const * se );

/**
   Resets se's state, as per cwal_error_reset(), plus clears any
   s2-level interruption flag. Does not clear any propagating
   exception.
*/
void s2_engine_err_reset( s2_engine * se );

/**
   Works like s2_engine_err_reset(), but also clears any propagating
   exception or "return"-style value.
*/
void s2_engine_err_reset2( s2_engine * se );

/**
   Clears se's state, as per cwal_error_clear(), plus
   clears any interruption flag.
*/
void s2_engine_err_clear( s2_engine * se );

/**
   Returns se's error state, as per cwal_error_get(), with one special
   case: if se has been "interrupted" via s2_interrupt(),
   CWAL_RC_INTERRUPTED is returned, possibly without a message (*msg
   and *msgLen may be be set to NULL resp. 0 if the interruption does
   not include a message).
*/
int s2_engine_err_get( s2_engine const * se, char const ** msg, cwal_size_t * msgLen );

/**
   A utility function implementing unary and binary addition and
   subtraction of numbers (in the form of cwal_values).

   This routine does not take overloading into account.

   To do binary operations:

   - Both lhs and rhs must be valid values. They will be converted
   to a number if needed. lhs is the left-hand-side argument and rhs
   is the right-hand-side.

   - Set doAdd to true to perform addition, 0 to perform
   subtraction.

   For unary operations:

   - As for binary ops, but lhs must be NULL. The operation is
   applied solely to rhs.

   The result value is assigned to *rv, and rv may not be NULL.

   Returns 0 on success, non-0 on misuse or allocation error.

   If lhs (binary) or rhs (unary) is a double value then the result
   (except for cases listed below) will be a double, otherwise it
   will be an integer.

   This routine makes some optimizations to avoid allocating memory
   when it knows it does not have to. These optimizations are
   internal details, but may change the expected type of the result,
   and so are listed here:

   - If !lhs and doAdd is true (i.e. unary addition), then the
   result is rhs.

   - Binary ops: if either argument is 0 resp. 0.0 then the result
   is the other argument. i.e. A+0===A and 0+A===A.

   Returns 0 on success, non-0 on error. Errors can come in the form
   of CWAL_RC_OOM or, for overloaded operators, any sort of error a
   script-side function can cause. On non-OOM error, se's error
   state will be update or an exception may be propagated.
*/
int s2_values_addsub( s2_engine * se, char doAdd,
                      cwal_value * lhs, cwal_value * rhs,
                      cwal_value **rv );


/**
   The multiply/divide/modulo counterpart of s2_values_addsub(),
   this routine applies one of those three operations to
   its (lhs, rhs) arguments and assigns the result to *rv.

   This routine does not take overloading into account.

   If mode is negative, the division operation is applied,
   If mode is 0, modulo is applies. If mode is greater than 0
   then multiplication is applied.

   Returns 0 on success, a non-0 CWAL_RC_xxx value on error.
   Returns CWAL_SCR_DIV_BY_ZERO for either division or modulo by
   0. On error, se's error state will be update or (for overloaded
   operators) an exception may be propagated.

   This routine applies several optimizations which might change
   the expected result type:

   Modulo:

   - Always has an integer result except on modulo-by-0.

   Multiplication:

   - (0 * 1) === 0
   - (0.0 * 1) === 0.0
   - (N * 1) === N
   - If either the lhs or rhs is a double then the result will be a
   double unless a more specific optimization applies.
*/
int s2_values_multdivmod( s2_engine * se, int mode,
                          cwal_value * lhs, cwal_value * rhs,
                          cwal_value **rv );

/**
   Performs bitwise and bitshift operations on one or two values.

   This routine does not take overloading into account.

   This function has two modes:

   Unary: op must be S2_T_OpNegateBitwise and lhs must be NULL.
   The bitwise negation operation is applied to rhs and the result
   is stored in *rv.

   Binary: op must be one of the many S2_T_OpXXXAssign,
   S2_T_OpXXXAssign3,
   S2_T_Op{AndBitwise,OrBitwise,XOr,ShiftLeft,ShiftRight}. The binary
   operation is applied to lhs and rhs and the result is stored in
   *rv.

   The resulting value is always of type CWAL_TYPE_INTEGER, but it may
   be optimized away to the lhs or rhs instance (as opposed to
   creating a new one).

   Returns 0 on success. On error *rv is not modified.
*/
int s2_values_bitwiseshift( s2_engine * se, int op, cwal_value * lhs,
                            cwal_value * rhs, cwal_value ** rv );


/**
   Prints out a cwal_printf()-style message to stderr and abort()s the
   application.  Does not return. Intended for use in place of
   assert() in test code. The library only uses this internally to
   confirm internal invariants in some places where an assert() would
   be triggered in debug builds.
*/
void s2_fatal( int code, char const * fmt, ... );


/**
   Flags for customizing s2_cb_print_helper()'s behaviour.
*/
enum s2_cb_print_helper_options {
S2_PRINT_OPT_NONE = 0x00,
/**
   Tells s2_cb_print_helper() to output one space character
   (ASCII 0x20) between arguments. It does not output a space
   at the start or end of the arguments.
*/
S2_PRINT_OPT_SPACE = 0x01,
/**
   Tells s2_cb_print_helper() to output one newline character (ASCII 0x0A)
   At the end of the arguments, even if it does not output any arguments.
*/
S2_PRINT_OPT_NEWLINE = 0x02,
/**
   Tells s2_cb_print_helper() to apply s2_value_unwrap() to each argument. That
   will "unwrap" one level of CWAL_TYPE_UNIQUE wrapper, but have no effect on
   non-Unique values.
*/
S2_PRINT_OPT_UNWRAP = 0x04,
/**
   Tells s2_cb_print_helper() to "return" (in the cwal_callback_f()
   sense) the callee Function value, rather than the undefined value.
*/
S2_PRINT_OPT_RETURN_CALLEE = 0x08,
/**
   Tells s2_cb_print_helper() to "return" (in the cwal_callback_f()
   sense) the call's "this" value, rather than the undefined value. If
   both this flag and S2_PRINT_OPT_RETURN_CALLEE are specified, an
   exception is triggered.

   Note that when s2 calls a function "standalone" (outside the
   context of an object property access), the function is its own
   "this", so this flag will have the same effect as
   S2_PRINT_OPT_RETURN_CALLEE in such cases.
*/
S2_PRINT_OPT_RETURN_THIS = 0x10
};

/**
   This is intended to be a proxy for print()-like cwal_callback_f()
   implementations. The first and second arguments should be passed in
   as-is from the wrapping callback's first and second arguments. The
   fourth argument should be a mask of s2_cb_print_helper_options
   flags which customize its output. The third argument...

   If skipArgCount is > 0 then that many arguments will be skipped
   over (ignored by this routine). The intent is that certain helper
   functions might accept "prefix" arguments which it processes itself
   before passing the rest on to this function.

   On success, *rv will, by default, be set to cwal_value_undefined(),
   but that may be modified by including the
   S2_PRINT_OPT_RETURN_CALLEE or S2_PRINT_OPT_RETURN_THIS flag, or
   setting *rv explicitly from the wrapping callback _after_ calling
   this.

   All output generated by this function is emitted via cwal_output(),
   so it ends up in the s2 engine's configured output channel.

   Returns 0 on success, else an error code suitable for returning
   from the wrapping callback. Potential errors include, but are not
   limited to, I/O problems and out-of-memory.

   If both the S2_PRINT_OPT_RETURN_CALLEE and S2_PRINT_OPT_RETURN_THIS
   flags are specified, an exception is triggered and
   CWAL_RC_EXCEPTION is returned, which the caller of this routine is
   expected to propagate back to its own caller. That said, the caller
   will necessarily be C code and should not pass both of those flags
   to this function.
*/
int s2_cb_print_helper( cwal_callback_args const * args,
                        cwal_value **rv,
                        uint16_t skipArgCount,
                        cwal_flags32_t flags );

/**
   A cwal_callback_f() implementation performing basic
   output-to-console. All arguments are output with a single space
   between them and a newline at the end. Output goes to
   cwal_output(), so it might not go to the console.

   The callback returns args->callee via *rv.
*/
int s2_cb_print( cwal_callback_args const * args, cwal_value **rv );

/**
   Works just like s2_cb_print() except that it does not add
   extra whitespace around its arguments, nor a newline at
   the end of the line.

   The callback returns args->callee via *rv.
*/
int s2_cb_write( cwal_callback_args const * args, cwal_value **rv );

/**
   A cwal_callback_f() implementation which sends a flush request to
   args->engine's configured output channel.  Returns 0 on success,
   throws on error.
*/
int s2_cb_flush( cwal_callback_args const * args, cwal_value **rv );

/**
   A cwal_callback_f() which implements a simple script file execution
   method. Script usage:

   thisFunc(filename)

   Runs the script using s2_eval_filename() and assigns the result of
   the last expression to *rv (or the undefined value if the script
   evaluates to NULL (which can happen for a number of reason)).

   Requires that s2_engine_from_args() returns non-0 (i.e. that
   args->engine was initialized along with a s2_engine instance).
*/
int s2_cb_import_script(cwal_callback_args const * args, cwal_value ** rv);

/**
   Returns the prototype object for just about everything. That
   instance gets stashed away in se. Ownership of the returned pointer
   is unchanged.  The caller MUST NOT unreference it
   (cwal_value_unref() or cwal_value_unhand()) unless he explicitly
   obtains a reference.
*/
cwal_value * s2_prototype_object( s2_engine * se );
cwal_value * s2_prototype_function( s2_engine * se );
cwal_value * s2_prototype_array( s2_engine * se );
cwal_value * s2_prototype_exception( s2_engine * se );
cwal_value * s2_prototype_hash( s2_engine * se );
cwal_value * s2_prototype_string( s2_engine * se );
cwal_value * s2_prototype_double( s2_engine * se );
cwal_value * s2_prototype_integer( s2_engine * se );
cwal_value * s2_prototype_buffer( s2_engine * se );
cwal_value * s2_prototype_enum(s2_engine * se);
cwal_value * s2_prototype_tuple( s2_engine * se );

/**
   Returns the first value v of v's prototype chain
   which is an enum value created by the enum keyword,
   or 0 if none are.
*/
cwal_value * s2_value_enum_part( s2_engine * se, cwal_value * v );

/**
   Returns true (non-0) if v (not including prototypes) is an
   enum-type value created by the enum keyword.
*/
int s2_value_is_enum( cwal_value const * v );

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
int s2_stash_set( s2_engine * se, char const * key, cwal_value * v );

/**
   Equivalent to s2_stash_set() but takes its key in the form of a
   Value instance.
*/
int s2_stash_set_v( s2_engine * se, cwal_value * key, cwal_value * v );

/**
   Fetches a value set with s2_stash_set(). Returns NULL if not found,
   if !se, or if (!key || !*key). key must be NUL-terminated.
*/
cwal_value * s2_stash_get( s2_engine * se, char const * key );

/**
   Identical to s2_stash_get() but accepts the length of the key, in bytes.
   If keyLen==0 and *key, then cwal_strlen(key) is used to calculate the
   length.
*/
cwal_value * s2_stash_get2( s2_engine * se, char const * key, cwal_size_t keyLen );

/**
   Identical to s2_stash_get2() but returns the matching cwal_kvp on a
   match (else NULL).
*/
cwal_kvp * s2_stash_get2_kvp( s2_engine * se, char const * key,
                              cwal_size_t keyLen);

/**
   Identical to s2_stash_get() but takes its key in the form of a Value
   instance.
*/
cwal_value * s2_stash_get_v( s2_engine * se, cwal_value const * key );

/**
   Wrapper around cwal_var_decl_v().

   Notable error codes:

   CWAL_RC_ALREADY_EXISTS = key already exists
*/
int s2_var_decl_v( s2_engine * se, cwal_value * key,
                   cwal_value * v, uint16_t flags );

/**
   The C-string counterpart of s2_var_decl().
*/
int s2_var_decl( s2_engine * se, char const * name,
                 cwal_size_t nameLen,
                 cwal_value * v, uint16_t flags );

/**
   Searches for a named variable in the given interpreter
   engine. scopeDepth is as described for cwal_scope_search_v()
   (normally 0 or -1 would be correct).

   Returns the found value on success, NULL if no entry is found or if
   any arguments are invalid (!se, !key).
*/
cwal_value * s2_var_get( s2_engine * se, int scopeDepth,
                         char const * key, cwal_size_t keyLen );

/**
   Functionally identical to s2_var_get(), but takes its
   key as a cwal_value.
*/
cwal_value * s2_var_get_v( s2_engine * se, int scopeDepth,
                           cwal_value const * key );

/**
   If self is not NULL then this performs a lookup in the current 
   scope (recursively up the stack) for a variable with the given 
   key. If self is not NULL then it performs a property search on 
   self, recursive up the prototype chain if necessary.  On success 
   the result is stored in *rv and 0 is returned. See s2_set_v() for 
   how Arrays are (sometimes) handled differently.
   
   If no entry is found:

   - If self has the S2_VAL_F_DISALLOW_UNKNOWN_PROPS container client
   flag then CWAL_RC_NOT_FOUND is returned and se's error state
   contains the details. e.g. enums do that.

   - Otherwise 0 is returned and *rv is set to 0.
   
   @see s2_set_v()
*/
int s2_get_v( s2_engine * se, cwal_value * self,
              cwal_value * key, cwal_value ** rv );

/**
   C-string variant of s2_get_v().
*/
int s2_get( s2_engine * se, cwal_value * self,
             char const * key, cwal_size_t keyLen,
             cwal_value ** rv );

/**
   Sets a named variable in the given interpreter engine. scopeDepth
   is as described for cwal_scope_chain_set_v() (normally 0 would be
   correct). Use a value of NULL to unset an entry.

   Returns 0 on success, CWAL_RC_MISUSE if !ie or !key, and
   potentially other internal/low-level error codes, e.g. CWAL_RC_OOM
   if allocation of space for the property fails.
*/
int s2_var_set( s2_engine * se, int scopeDepth,
                 char const * key, cwal_size_t keyLen,
                 cwal_value * v );

/**
   Functionally identical to s2_var_set(), but takes its
   key as a cwal_value.
*/
int s2_var_set_v( s2_engine * se, int scopeDepth,
                  cwal_value * key, cwal_value * v );

/**
   If self is NULL then this behaves as a proxy for
   cwal_scope_chain_set_with_flags_v(), using the current scope,
   otherwise...

   self must be NULL or a container type (!cwal_props_can(self)),
   or non-0 (not sure _which_ non-0 at the moment) is returned.

   v may be NULL, indicating an "unset" operation.

   If self is an a Array or has one in its prototype: if key is-a
   Integer then cwal_array_set() is used to set the property, else
   cwal_prop_set_v(self,key,v) is used.

   If self is-a Hashtable and s2_hash_dot_like_object() (or
   equivalent) has been used to set its "like-an-object" flag, then
   this function manipulates the hash entries, not object properties
   (hashes can have both).

   Otherwise the key is set as an object-level property of self.

   Returns the result of the underlying setter call (of which there
   are several posibilities).

   If !v then this is an "unset" operation. In that case, if !self
   and key is not found in the scope chain, CWAL_RC_NOT_FOUND is
   returned, but it can normally be ignored as a non-error.

   @see s2_get_v()
*/
int s2_set_with_flags_v( s2_engine * se, cwal_value * self,
                         cwal_value * key, cwal_value * v,
                         uint16_t kvpFlags );

/**
   Identical to s2_set_with_flags_v(), passing CWAL_VAR_F_PRESERVE as
   the final parameter.
*/
int s2_set_v( s2_engine * se, cwal_value * self,
              cwal_value * key, cwal_value * v );

/**
   C-string variant of s2_set_with_flags_v().
*/
int s2_set_with_flags( s2_engine * se, cwal_value * self,
                       char const * key, cwal_size_t keyLen,
                       cwal_value * v, uint16_t flags );

/**
   Equivalent to calling s2_set_with_flags() with the same
   arguments, passing CWAL_VAR_F_PRESERVE as the final
   argument.
*/
int s2_set( s2_engine * se, cwal_value * self,
            char const * key, cwal_size_t keyLen,
            cwal_value * v );

/**
   Installs a core set of Value prototypes into se.

   Returns 0 on success.
*/
int s2_install_core_prototypes(s2_engine * se);

enum s2_next_token_flags {
/**
   Specifies that s2_next_token() should treat EOL tokens as
   significant (non-junk) when looking for the next token.
*/
S2_NEXT_NO_SKIP_EOL = 0x01,
/**
   Specifies that s2_next_token() should not "post-process" any
   tokens. e.g. it normally slurps up whole blocks of parens and
   braces, and re-tags certain token types, but this flag disables
   that. The implication is that the caller of s2_next_token() "really
   should" use a non-null 4th argument, so that the main input parser
   does not get confused by partially-consumed block constructs or
   mis-tagged semicolons.
*/
S2_NEXT_NO_POSTPROCESS = 0x02
};

/**
   A form for s2_ptoker_next() which is specific to s2_engine
   evaluation, performing various token skipping and
   post-processing.  It searches for the next non-junk token, then
   perhaps post-processes it before returning.

   If tgt is NULL then the token is consumed as normal and st->token
   contains its state after returning. If tgt is not-NULL then the
   this works like a lookahead: the consumed token is copied to *tgt
   and st's token/putback state are restored to their pre-call state
   (regardless of success or failure).

   The flags parameter must be 0 or a bitmask of s2_next_token_flags
   values. Note that by default newlines are treated as junk tokens
   and ignored.

   Returns 0 on success. On error the input must be considered
   unparsable/unrecoverable unless st is a sub-parser of a compound
   token in a larger parser (e.g. a (list) or {script} or [braces]),
   in which case parsing may continue outside of the subparser
   (because we know, at that point, that the group tokenizes as an
   atomic token).

   This function does not throw exceptions in se, but instead
   updates se's error state on error.
*/
int s2_next_token( s2_engine * se, s2_ptoker * st, int flags, s2_ptoken * tgt );

/**
   Uses s2_next_token(se,pr,nextFlags,...) to peek at the next token
   in pr. If the next token has the type ttype, this function returns
   ttype, else it returns 0. If consumeOnMatch is true AND the next
   token matches, pr->token is set to the consumed token.
*/
int s2_ptoker_next_is_ttype( s2_engine * se, s2_ptoker * pr, int nextFlags,
                             int ttype, char consumeOnMatch );

/**
   Creates a value for a token. This basically just takes the t->src
   and makes a string from it, but has different handling for certain
   token types. Specifically:

   - Integers (decimal, octal, and hex) and doubles are created as
   the corresponding cwal numeric types.

   - Strings literals get unescaped.

   - Heredocs are not unescaped, but their opening/closing markers are
   trimmed, as are any whitespace implied by their modifier flag (if
   any).

   - S2_T_SquigglyBlock is, for historical reasons, returned as
   a string, but it does not get unescaped like a string literal.
   It does get left/right trimmed like a heredoc, though.

   - Any other token type is returned as-is as a string value,
   without unescaping.

   On success *rv contains the new value.

   Returns 0 on success, CWAL_RC_OOM on OOM, and may return
   CWAL_RC_RANGE if unescaping string content fails (so far only ever
   caused by ostensibly invalid inputs, e.g. `\U` sequences).

   Note: the 2nd argument is only here for error reporting in one
   corner case (unescaping a string fails).

   ACHTUNG: a returned string value might not be a new instance, and
   it is CRITICAL that the client not call cwal_value_unref() on it
   without calling cwal_value_ref() first. Not following this advice
   can lead to the caller pulling the returned string value out from
   under other code. Conversely, failing to call cwal_value_unref()
   may (depending on other conditions) lead to a cwal_scope-level leak
   until that scope is swept up or popped.
*/
int s2_ptoken_create_value( s2_engine * se,
                            s2_ptoker const * pr,
                            s2_ptoken const * t,
                            cwal_value ** rv );

/**
   Sets se's error state, as for s2_engine_err_set(), and returns that
   call's result value. If fmt is NULL or !*fm, st->errMsg is used (if
   set) as the message. If se->opErrPos or s2_ptoker_err_pos(st) (in
   that order) are set to a position within [st->begin,st->end) then
   line/column information, relative to st->begin, is appended to the
   message. Sometimes it is necessary for callers to set or tweak st
   error position explicitly before calling this (see
   s2_ptoker_errtoken_set()).

   Returns the error code, not 0, on success! Returns some other
   non-0 code on error.

   This routine takes pains not to allocate any memory, which also
   means not generating an error message, if the given error code is
   CWAL_RC_OOM.
*/
int s2_err_ptoker( s2_engine * se, s2_ptoker const * st,
                   int code, char const * fmt, ... );


/**
   Similar to s2_err_ptoker(), but clears se's error state and sets
   se's Exception state. One exception (as it were) is if the code is
   CWAL_RC_OOM, in which case se's error state is set but no exception
   is thrown and no formatted message is generated because doing
   either would require allocating memory.
*/
int s2_throw_ptoker( s2_engine * se, s2_ptoker const * pr, int code,
                     char const * fmt, ... );

/**
   Throws se's error state (which must be non-OK), using pr (if not
   NULL, else se->currentScript) as the source for error location
   information. It tries to determine the position of the error
   based on se's and pr's state at the time this is called. It is
   particularly intended to be called from code which:

   a) is calling non-throwing, error-producing code (e.g. op-stack processing).

   b) wants to convert those errors to script-side exceptions.

   Returns CWAL_RC_EXCEPTION on success, some "more serious" non-0
   code on errors like allocation failure (CWAL_RC_OOM).
*/
int s2_throw_err_ptoker( s2_engine * se, s2_ptoker const * pr );

/**
   Throws a value as an exception in se.

   If pr is not NULL, it is used for collecting error location
   information, which is set as properties of the exception. If pr is
   NULL, se's current script is used by default. If both are NULL,
   no script-related information is added to the exception.

   If v is-a Exception (including inheriting one), it is assumed that
   the exception already contains its own error state: errCode is
   ignored and script location information is only added if v does not
   appear to have/inherit that information.

   If v is-not-a Exception, v is used as the "message" property of a
   newly-created exception, and errCode its "code" property.

   20191228: behaviour changed slightly for the case that v is-a
   Exception, in that script location information are no longer always
   harvested because doing so normally leads to having duplicated
   error state (most notably the stack trace) in the inherited
   exception and v. If, however, v appears to inherit no script
   location information (which can happen if the inherited exception
   was born in native space, as opposed to via the script-side
   exception() keyword), script info is harvested and installed in v
   (not in its inherited exception object).
*/
int s2_throw_value( s2_engine * se, s2_ptoker const * pr, int errCode, cwal_value *v );


/**
   Flags for eventual use with s2_eval_expr() and friends.
*/
enum s2_eval_flags_t {
/**
   Treat the eval-parsing as a lookahead of the expression token(s)
   instead of consuming them. It may still _evaluate_ the contents
   on its way to finding the end of the expression, unless
   S2_EVAL_SKIP is used as well.

   Reminder to self: it seems that this is no longer used internally,
   so it's a candidate for removal.
*/
S2_EVAL_NO_CONSUME = 0x01,
/**
   Forces the parsing (if not already done so by a higher-level
   parse) into "skip mode."

   Used for short-circuit evaluation, which only evaluates the
   tokens for syntactical correctness, without having side-effects
   which affect the script's result. It is legal to use S2_EVAL_SKIP
   in conjunction with S2_EVAL_NO_CONSUME, as this can be used to
   confirm syntactic correctness (at least for the top-most level of
   expression) and find the end of the expression point without
   "really" evaluating it. i.e. for short-circuit logic.
*/
S2_EVAL_SKIP = 0x02,
/**
   NOT YET IMPLEMENTED. May never be.

   Treat comma tokens as end-of-expression instead of as a binary
   operator.
*/
S2_EVAL_COMMA_EOX = 0x04,

/**
   Inidicates that a new cwal_scope should be pushed onto the (cwal)
   stack before expression parsing starts, and popped from the stack
   when it ends. The result value of the expression will be up-scoped
   into the calling (cwal) scope.
*/
S2_EVAL_PUSH_SCOPE = 0x10,

/**
   An internal-use flag, and bits at this location and higher this are
   reserved for internal use.
*/
S2_EVAL_flag_bits = 16
};

/**
   Tokenizes and optionally evaluates one complete expression's worth
   of token state from st. This is the core-most evaluator for s2 -
   there is not a lower-level one in the public API. Care must be
   taken with lifetimes, in particular with regards to sweeping and
   vacuuming, when using this routine.

   If flags contains S2_EVAL_NO_CONSUME, it behaves as is if it
   tokenized an expression and then re-set st's token state. This
   can be used to check expressions for eval'ability without
   consuming them. Be careful not to use that flag in a loop, as it
   will continually loop over the same tokens unless the caller
   manually adjusts st->token between calls.

   If rv is not NULL then on success the result of the expression
   is written to *rv. Lifetime/ownership of the expression is not
   modified from its original source (which is indeterminate at this
   level), and the client must take a reference if needed.

   If rv is not NULL and the expression does not generate a value
   (e.g. an empty expression or EOF), *rv is set to 0.

   On any sort of error, *rv is not modified.

   Sweeping is disabled while an expression is running, to avoid that
   any values used in the expression can live as long as they need to
   without requiring explicit references everywhere.

   If flags contains S2_EVAL_SKIP then this function behaves
   slightly differently: it parses the expression and runs it
   through the normal evaluation channels, but it does so with "as
   few side-effects as possible," meaning it can be used to skip
   over expressions without (in effect) evaluating them. When
   running in "skip mode," all operations are aware that they should
   perform no real work (e.g. not allocating any new values), and
   should instead simply consume all their inputs without doing
   anything significant. This allows us to pseudo-evaluate an
   expression to find out if it could be evaluated. In skip mode, if
   rv is not NULL then any result value written to *rv "should" (by
   library convention) be 0 (for an empty expression) or
   cwal_value_undefined(). Achtung: if se->skipLevel is positive
   then this function always behaves as if S2_EVAL_SKIP is set,
   whether it is or not. All this flag does is temporarily
   increments se->skipLevel.

   Returns 0 on success or any number of CWAL_RC_xxx values on
   error. On error, se's error state is updated and/or a cwal-level
   exception is thrown (depending on exactly what type of error and
   when/where it happened).

   On success st->token holds the token which caused expression
   tokenization to terminate. On non-error, if st->token is not at
   its EOF, evaluation may continue by calling this again. When
   running in consuming mode (i.e. not using S2_EVAL_NO_CONSUME),
   then this routine sets st's putback token to the pre-call
   st->token (i.e.  the start of the expression). That means that a
   s2_ptoker_putback() will put back the whole expression.

   Upon returning (regardless of success or error), st.capture will
   point to the range of bytes captured (or partially captured
   before an error) by this expression.

   If st->token is an EOF or end-of-expression (EOX) token after this
   is called, *rv might still be non-0, indicating the expression
   ended at an EOF/EOX. This is unfortunate (leads to more work after
   calling this), but true.

   Before evaluation, se's current eval stack is moved aside, and it
   is restored before returning. This means that calls to this
   function have no relationship with one another vis-a-vis the stack
   machine. Any such relationships must be built up in downstream
   code, e.g. by calling this twice and using their combined result
   values. This property allows the engine to recover from syntax
   errors at the expression boundary level without the stack
   manipulation code getting out of hand. It also means that
   subexpressions cannot corrupt the stack parts used by the parent
   (or LHS) expression(s).

   Garbage collection: this routine cannot safely sweep up while it is
   running (and disables sweep mode for the duration of the
   expression, in case a subexpression triggers a
   sweep). s2_eval_ptoker() and friends can, though. Any temporaries
   created in the current scope by this routine may be swept up after
   it is called, provided the client has references in place wherever
   he needs them (namely, on *rv).  If the S2_EVAL_PUSH_SCOPE flag is
   used, sweepup is not necessary because only *rv will survive past
   the pushed scope. That said, pushing a scope for a single
   expression is just a tad bit of overkill, and not really
   recommended. Much Later: as of late 2017, this routine doesn't leak
   any temporaries by itself, but the arbitrary script code it calls
   might hypothetically do so via native code.
*/
int s2_eval_expr( s2_engine * se, s2_ptoker * st,
                  int flags, cwal_value ** rv);

/**
   Flags for use with s2_eval_ptoker() and (potentially) friends.
*/
enum s2_eval_flags2_t {
/**
   Guaranteed to be 0, indicating no special flags.
 */
S2_EVALP_F_NONE = 0,
/**
   Indicates that s2_eval_ptoker() should propagate CWAL_RC_RETURN
   unconditionally. Normally it will transate that result into a
   "return" value for the caller, the return 0. With this flag, it
   will leave the s2-level return-handling semantics to the caller.
*/
S2_EVALP_F_PROPAGATE_RETURN = 0x01
};

/**
   Evaluates all expressions (iteratively) in the s2 script code
   wrapped by pt. pt contains the source code range, its optional
   name, and its tokenizer lineage (if used in a sub-parser context).

   e2Flags may be 0, indicating no special evaluation flags, or a
   bitmask of values from the s2_eval_flags2_t enum.

   If rv is NULL then any result value from the parsed expressions
   is ignored.

   If rv is not NULL then the final result of the script is stored
   in *rv. Its ownership is unspecified - it might be a new
   temporary awaiting a reference (or to be discarded) or it might
   be a long-lived value which made its way back from the global
   scope. We just can't know at this point. What this means is: if
   the caller needs to use *rv, he needs to do so immediately
   (before the next sweep-up in the current scope or the current
   scope ending). He may obtain a reference in "any of the usual
   ways."

   Returns 0 on success, a CWAL_RC_xxx value on error. On error, se's
   error state and/or se->e's exception state will be set (depending on
   what caused the error) and *rv will not be modified.

   Garbage collection: while iterating over expressions, this routine
   briefly holds a reference to the pending result value, and sweeps
   up temporaries using s2_engine_sweep() (meaning that it may or may
   not periodically clean up temporaries). The reference to *rv is
   released (without destroying *rv) before this function returns,
   meaning that *rv may have been returned to a probationary
   (temporary) state by this call. If the caller needs to ensure its
   safety vis-a-vis sweepup, he must obtain a reference to it. If
   clients are holding temporaries in the current scope, they need to
   push a cwal scope before running this, and pop that scope
   afterwards, upscoping *rv to the previous if necessary (see
   cwal_scope_pop2() and cwal_value_rescope()).

   Nuances:

   - See s2_eval_expr() for lots more details about the parsing and
   evaluation process.

   - If pr->name is set then it is used as the name of the script,
   otherwise a name will be derived from pr->parent (if set),
   recursively.

   - A value followed by an end-of-expression (EOX: semicolon,
   end-of-line, or (in some cases) end-of-file) results to that
   value. A second "hard EOX" (i.e. a semicolon) will set the result
   to NULL. For purposes of counting the first EOX, the EOL token is
   considered an EOX, but multiple EOLs are not treated as multiple
   EOX. Examples: "3;" === Integer 3, but "3;;" === C-level NULL.
   Adding newlines between (or after) the final value and the
   semicolons does not change the result.

   - *rv (if not 0) may be assigned to 0 even if the script
   succeeds. This means either an empty script or a series of
   semicolon/EOX tokens have removed the result from the stack (as
   explained above).

   - If the expression triggers a CWAL_RC_RETURN result AND pt->parent
   is NULL and e2Flags does _not_ have the S2_EVALP_F_PROPAGATE_RETURN
   bit set, then the "return" result is treated as a legal value, and
   any pending/propagating result is passed on to the caller via
   *rv. If pt->parent is not NULL (or S2_EVALP_F_PROPAGATE_RETURN)
   then CWAL_RC_RETURN is (needs to be) propagated up, and is returned
   as an error (it is treated as one until it hits and handler which
   accepts "return" results and knows how to fiddle s2_engine's
   internals when doing so).

   Notes about "return" handling in s2:

   Return semantics are implemented by doing a combination of:

   - Calling s2_propagating_set() to set a "propagating" value. That
   part keeps the value propagating up the popping scope stack.

   - Returning CWAL_RC_RETURN from the routine in question. When this
   value is returned, consumers expecting it normally assert() that
   s2_propagating_get() returns non-NULL, as they expect it to have
   been set to implement 'return' keyword semantics.

   The point being: if callers want to propagate a 'return' from
   the called script, they must:

   - Pass S2_EVALP_F_PROPAGATE_RETURN in the e2Flags bits.

   - Accept the result code of CWAL_RC_RETURN as a non-error.

   - Must call s2_propagating_take(se) to take over the returned value
   or s2_propagating_set(se,0) to clear it (_potentially_ destroying
   it immediately). It might be a temporary, it might not be - its
   origin is indeterminate. If the caller needs to ensure it is kept
   alive, he must arrange to do so, e.g. via getting a reference via
   cwal_value_ref() and potentially (depending on his needs) making it
   vacuum-safe by inserting it into a vacuum-safe container or using
   cwal_value_make_vacuum_proof(). (Note, however, that vacuum-safing
   is a rare need, intended only for use with values which will be
   held around in C-space and never exposed to the script world.)

   - If the caller wants to propagate the result value further up the
   cwal scope stack, he should use cwal_value_rescope() (or
   s2_value_upscope()) to move it one scope up the stack before
   popping his scope. (Alternatively, cwal_scope_pop2() can pop the
   scope and rescope a single propagating value at the same time.)

   Similar handling is used for exit and break keywords, with the
   CWAL_RC_EXIT and CWAL_RC_BREAK result codes. The throw keyword
   returns the CWAL_RC_EXCEPTION code and its exception value is
   fetched/reset using cwal_exception_get(), cwal_exception_set(), and
   cwal_exception_take(), but exceptions are triggered in many places
   from C code, as opposed to only via the throw keyword (which uses
   the same mechanism).
*/
int s2_eval_ptoker( s2_engine * se, s2_ptoker * pt, int e2Flags,
                    cwal_value **rv );

/**
   Functionally equivalent to using s2_eval_ptoker() with a
   s2_ptoker initialized to use the source range
   [src,src+srcLen). The name parameter may be 0 - it is used when
   generating error location information. If srcLen is negative,
   cwal_strlen() is used to calculate src's length.

   If newScope is true, a new scope is used, otherwise the code is
   eval'd in the current scope, which can have unwanted side effects
   if the evaluation sweeps or (more particularly) vacuums up while
   the calling code has non-sweep/vacuum-safe values laying around
   (which it might, without knowing it). Unless you are 100% certain
   about the current state of the scripting engine and all client-side
   values owned by that engine, always use a new scope when running
   this function. (Pro tip: you can almost never be 100% certain about
   those conditions!)

   If newScope is true, rv is not NULL, and evaluation is successful,
   *rv gets rescoped (if needed) into the calling scope before
   returning. On error, *rv is not modified.

   @see s2_eval_buffer()
   @see s2_eval_cstr_with_var()
*/
int s2_eval_cstr( s2_engine * se,
                  char newScope,
                  char const * name,
                  char const * src, int srcLen,
                  cwal_value **rv );

/**
   This is a convenience wrapper around s2_eval_cstr() which does the
   following:

   1) Pushes a new cwal scope, returning immediately if that fails.

   2) Declares a scope-local variable with the name/value provided by
   the 2nd and 3rd arguments.

   3) Evaluates the given script code. (Presumably this script
   references the local new variable.)

   4) Pops the new scope. On success, if rv is not NULL, the result
   value of the eval is written to *rv. If rv is NULL, or on error,
   the result value (if any) is discarded.

   All parameters except for the 2nd and 3rd function as described for
   s2_eval_cstr().

   It is important that the caller hold a reference to the 3rd
   argument before calling this.

   This function fills a relatively common niche where a module wants
   to perform some of its initialization in script form and only needs
   a single local var reference to do it.

   @see s2_eval_buffer()
   @see s2_eval_cstr()
*/
int s2_eval_cstr_with_var( s2_engine * se,
                           char const * varName,
                           cwal_value * varValue,
                           char const * scriptName,
                           char const * src, int srcLen,
                           cwal_value **rv );

/**
   A convenience wrapper around s2_eval_cstr(). An empty
   buffer is treated like a an empty string ("").
*/
int s2_eval_buffer( s2_engine * se,
                    char newScope,
                    char const * name,
                    cwal_buffer const * buf,
                    cwal_value **rv );

/**
   Like s2_eval_cstr(), but evaluates the contents of a file. fname is
   the name of the file. fnlen is the length of the filename, which
   must be non-0, but cwal_strlen() is used if fnlen is negative.

   If pushScope is true then the contents are run in a new scope,
   otherwise they are run in the current scope (quite possibly not
   what you want, but go ahead if you want). If rv is not NULL then
   the result value of the evaluation (if any) is assigned to *rv.  If
   *rv is not 0 upon returning then *rv will (if needed) have been
   moved into the calling cwal scope when this returns.

   If the script triggers a CWAL_RC_RETURN code then this function
   treats that as a success result, sets *rv (if rv is not NULL) to
   the 'return' value, and stops automatic propagation of that
   value. Most non-exception errors get converted to exceptions, so as
   to not be fatal to the importing script.

   Returns 0 on success. On error it may return a number of things:

   - CWAL_RC_OOM indicates an allocation failure.

   - CWAL_RC_EXCEPTION indicates that the file's script contents
   threw or propagated an exception, which is available via
   cwal_exception_get().

   - CWAL_RC_FATAL: means 'fatal' was called (which implicitly
   triggers an exception). Its pending exception can be found in
   cwal_exception_get().

   - CWAL_RC_EXIT: means the 'exit' keyword was called. Its result value
   can be found in cwal_propagating_get().

   - Most other non-0 codes cause se's error state to be updated with
   more information (see s2_engine_err_get()).
*/
int s2_eval_filename( s2_engine * se, char pushScope,
                      char const * fname,
                      cwal_int_t fnlen,
                      cwal_value ** rv );


/**
   Appends the given value to the given buffer in string form.
   Returns 0 on success.

   Objects and Arrays/Tuples are buffered in JSON form, and this
   function will fail if traversing them discovers cycles.
*/
int s2_value_to_buffer( cwal_engine *e, cwal_buffer * buf,
                        cwal_value * arg );

/**
   A cwal_callback_f() impl which uses s2_value_to_buffer() to convert
   args->self to a string. The other arguments are ignored.
*/
int s2_cb_value_to_string( cwal_callback_args const * args, cwal_value **rv );

/**
   Returns se's underlying cwal_engine instance (used by
   much of the lower-level scripting engine API). It is owned
   by se.
*/
cwal_engine * s2_engine_engine(s2_engine * se);

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

   integer compare(value rhs); // requires a 'this'

   integer compare(value lhs, value rhs[, boolean typeStrict = false]);

   The latter form works independently of the current 'this'.
*/
int s2_cb_value_compare( cwal_callback_args const * args, cwal_value **rv );


/**
   Behaves more or less like the access(2) C function (_access() on
   Windows builds).

   Returns true if the given filename is readable (writeable if
   checkForWriteAccess is true), else false.
*/
char s2_file_is_accessible( char const * fn, char checkForWriteAccess );

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
char s2_is_dir( char const * name, char checkForWriteAccess );


/**
   cwal_callback_f() impl binding s2_file_is_accessible() in scriptable form:

   fileIsAccessible(string filename [, bool checkWriteMode=false])

   This function throws an exception if s2_disable_check() for the flag
   S2_DISABLE_FS_STAT fails.
*/
int s2_cb_file_accessible( cwal_callback_args const * args, cwal_value **rv );

/**
   The directory counterpart of s2_cb_file_accessible().
*/
int s2_cb_dir_accessible( cwal_callback_args const * args, cwal_value **rv );

/**
   cwal_callback_f() impl binding realpath(3). Script usage:

   string realpath(path)

   Returns the undefined value if it cannot resolve the name. Throws
   on error or if the library is built without realpath(3) support.
*/
int s2_cb_realpath( cwal_callback_args const * args, cwal_value **rv );

  
/**
   A cwal_callback_f() impl which passes args->self through
   cwal_json_output() to produce JSON output. Assigns the resulting
   string value to *rv. If args->argc is not 0 then args->argv[0] is
   used to specify the indentation, as per cwal_json_output_opt.

   Script usage depends on whether or not args->self is-a (or inherits)
   Buffer. If not, then the function's usage looks like:

   string t = self.toJSONToken([indentation=0 [, cyclesAsStrings=false]])

   and returns the JSON-ified from of self.
   
   If self is-a Buffer, it looks like:

   self.toJSONToken(Value v [, indentation=0 [, cyclesAsStrings=false]])

   It appends the JSON form of v to self and returns self.

   If cyclesAsStrings is true, recursion/cycles are rendered in some
   useless (debugging only) string form, otherwise cycles cause an
   exception to be thrown.
*/
int s2_cb_this_to_json_token( cwal_callback_args const * args, cwal_value **rv );

/**
   A cwal_callback_f() which passes its first argument through
   cwal_json_output() to produce JSON output. Assigns the resulting
   string value to *rv. If args->argc is greater than 1 then the
   second argument specifies the indentation: a positive number for
   that many spaces per level and a negative number for that many hard
   tabs per level.


   string t = toJSONToken(value, [indentation=0 [, cyclesAsStrings=false]])

   and returns the JSON-ified from of the value.

   If cyclesAsStrings is true, recursion/cycles are rendered in some
   useless (debugging only) string form, otherwise cycles cause an
   exception to be thrown.
*/
int s2_cb_arg_to_json_token( cwal_callback_args const * args, cwal_value **rv );


/**
   A cwal_callback_f() implementation which parses JSON string input.

   Script usage:

   ```
   var json = '{"a":"hi!"}';
   var obj = thisFunction(json)
   ```
*/
int s2_cb_json_parse_string( cwal_callback_args const * args, cwal_value **rv );

/**
   The file-based counterpart of s2_cb_json_parse_string(). It works
   identically except that it takes a filename as input instead of a
   JSON string.

   This callback honors the S2_DISABLE_FS_READ limitation, but it also
   suggests that we may want a less-strict fs-read-disabling option
   which allows JSON (since JSON is known to not allow execution of
   foreign code nor loading of non-JSON content).
*/
int s2_cb_json_parse_file( cwal_callback_args const * args, cwal_value **rv );

/**
   cwal_callback_f() impl which works like getenv(3). Expects one
   string argument (throws if it does not get one) and returns (via
   *rv) either a string or the undefined value.
*/
int s2_cb_getenv( cwal_callback_args const * args, cwal_value **rv );

/**
   Installs JSON functionality into the given target value or (if (key
   && *key)) a new Object property (with the given name) of that target
   value. The target must be a container type.

   The functions installed:

   Object parse(String|Buffer jsonString)

   Object parseFile(String filename)

   string stringify(Value [, int indentation=0])

   mixed clone(object|array) is equivalent to parse(stringify(value)),
   but is defined in such a way that its "this" need not be this JSON
   module. i.e. the function reference can be copied and used
   independently of the target object, regardless of whether
   target[parse] and target[stringify] are currently visible symbols.

   Returns CWAL_RC_TYPE if target is not a container-capable type.

   Returns 0 on success.
*/
int s2_install_json( s2_engine * se, cwal_value * target,
                     char const * key);

/**
   If e was created in conjunction with an s2_engine and bound as its
   client state using &s2_engine_empty as the type ID, this function
   returns it, else it returns 0. That binding happens during
   s2_engine_init(), so it will be set if e was successfully processed
   via that routine.
*/
s2_engine * s2_engine_from_state( cwal_engine * e );

/**
   Equivalent to s2_engine_from_state(args->engine).

   This is intended for use in cwal_callback_f() implementations, for
   the case that they need the underlying s2_engine (most don't).
*/
s2_engine * s2_engine_from_args( cwal_callback_args const * args );


/**
   The "Path Finder" class is a utility for searching the filesystem
   for files matching a set of common prefixes and/or suffixes
   (i.e. directories and file extensions).

   @see s2_new_pf()
   @see s2_pf_value()
   @see s2_value_pf()
   @see s2_value_pf_part()
   @see s2_pf_dir_add()
   @see s2_pf_dir_add_v()
   @see s2_pf_dirs()
   @see s2_pf_dirs_set()
   @see s2_pf_ext_add()
   @see s2_pf_ext_add_v()
   @see s2_pf_exts_set()
   @see s2_pf_exts()
   @see s2_pf_search()
*/
typedef struct s2_pf s2_pf;

/**
   Creates a new PathFinder instance. PathFinders are bound to cwal as
   cwal_native instances and are initially owned by the currently
   active scope. Returns NULL on allocation error.
*/
s2_pf * s2_pf_new(s2_engine * se);

/**
   Returns the underlying cwal_value which acts as pf's "this".

   pf may not be NULL.

   @see s2_value_pf()
   @see s2_value_pf_part()
*/
cwal_value * s2_pf_value(s2_pf const * pf);

/**
   If v is-a PathFinder or derives from it, this function returns the
   s2_pf part of v or one of its prototypes.

   It is legal for v to be NULL.

   @see s2_value_pf()
   @see s2_pf_value()
*/
s2_pf * s2_value_pf_part(cwal_value const *v);

/**
   If v was created via s2_pf_new() then this function returns
   its s2_pf counterpart, else it returns NULL.

   It is legal for v to be NULL.

   @see s2_pf_value()
   @see s2_value_pf_part()
*/
s2_pf * s2_value_pf(cwal_value const * v);

/**
   Adds a directory to pf's search path. dir must be at least dirLen bytes
   and may be an empty but may not be NULL.

   Returns 0 on success.

   @see s2_pf_dir_add_v()
*/
int s2_pf_dir_add( s2_pf * pf, char const * dir, cwal_size_t dirLen);

/**
   Adds a file suffix (extension) to pf's search path. ext must be at
   least extLen bytes and may be an empty but may not be NULL.

   Returns 0 on success.

   @see s2_pf_ext_add_v()
*/
int s2_pf_ext_add( s2_pf * pf, char const * ext, cwal_size_t extLen);

/**
   Variant of s2_pf_dir_add() which takes its directory part in the
   form of a cwal_value.

   Returns 0 on success.
*/
int s2_pf_dir_add_v( s2_pf * pf, cwal_value * v );

/**
   Variant of s2_pf_ext_add() which takes its directory part in the
   form of a cwal_value.

   Returns 0 on success.
*/
int s2_pf_ext_add_v( s2_pf * pf, cwal_value * v );

/**
   Replaces pf's directory list with the given one.

   Returns 0 on success.
*/
int s2_pf_dirs_set( s2_pf * pf, cwal_array * ar );

/**
   Replaces pf's extension/suffix list with the given one.

   Returns 0 on success.
*/
int s2_pf_exts_set( s2_pf * pf, cwal_array * ar );

/**
   Symbolic values for use with s2_pf_search()'s final
   parameter.
 */
enum s2_pf_search_policy {
/**
   Indicates that ONLY directory names will be considered as matches.
*/
S2_PF_SEARCH_DIRS = -1,
/**
   Indicates that ONLY file (not directory) names will be considered
   as matches.
*/
S2_PF_SEARCH_FILES = 0,
/**
   Indicates that both file and directory names will be considered as
   matches.
*/
S2_PF_SEARCH_FILES_DIRS = 1
};

/**
   Searches for a file whose name can be constructed by some
   combination of pf's directory/suffix list and the given base name.

   The 5th argument specificies whether searching is allowed to match
   directory names or not. A value of 0 means only files (not
   directories) will be considered for matching purposes. A value
   greater than zero means both files and directories may be
   considered for matching purposes. A value less than zero means only
   directories (not files) may be considered a match.  (See the
   s2_pf_search_policy enum for symbolic names for this policy.)

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

   @see s2_pf_search_policy
*/
char const * s2_pf_search( s2_pf * pf, char const * base,
                           cwal_size_t baseLen, cwal_size_t * rcLen,
                           int directoryPolicy);


/**
   Returns pf's list of directories, creating it if needed. Only
   returns NULL if !pf or on allocation error.

   In script space this value is available via the "prefix" property.
*/
cwal_array * s2_pf_dirs(s2_pf *pf);

/**
   Returns pf's list of extensions/suffixes, creating it if needed. Only returns
   NULL if !pf or on allocation error.

   In script space this value is available via the "suffix" property.
*/
cwal_array * s2_pf_exts(s2_pf *pf);

/**
   A cwal_callback_f() implementing a constructor of PathFinder (s2_pf)
   instances. On success, assigns the new instance to *rv.

   Requires that s2_engine_from_args() returns non-NULL.

   Script usage:

   var pf = ThisFunction()

   it optionally takes up to two array arguments for the
   directory/extension lists, respectively.

   This is a holdover from before the 'new' keyword was added.
*/
int s2_cb_pf_new( cwal_callback_args const * args, cwal_value **rv );

/**
   Installs an Object named PathFinder (the s2_prototype_pf() object)
   into the given value (which must be a container type). Returns 0 on
   success, CWAL_RC_MISUSE if !se or !ns, CWAL_RC_TYPE if ns is not a
   container, and CWAL_RC_OOM if allocating any component fails.
*/
int s2_install_pf( s2_engine * se, cwal_value * ns );

/**
   Returns the prototype object for PathFinder instances. That
   instance gets stashed away in se. Ownership of the returned pointer
   is unchanged.  The caller MUST NOT unreference it
   (cwal_value_unref() or cwal_value_unhand()) unless he explicitly
   obtains a reference.
*/
cwal_value * s2_prototype_pf(s2_engine *se);

/**
   Callback signature for s2 module import routines.

   When called by s2_module_load(), s2_module_init(), or similar, this
   function type is passed the associated s2_engine instance and the
   client-provided module result value address.

   Implementations "should" (by convention) return their module by
   assigning it to *module. Optionally, they may use the s2_engine's
   facilities to store the functionality long-term (in terms of value
   lifetimes), e.g. using s2_stash_set(), or even forcing them into
   the top-most scope. In any case, they should, on success, assign
   some result value to *module, even if it's NULL. Note, however,
   that NULL is not generally a useful result value. Most modules
   return a "namespace object" which contains the module's
   functionality.

   When assigning to *module, the API expects that this function will
   not hold any extraneous references to the returned value. i.e. if
   it's a new Value with no circular references, its refcount "should"
   be zero when *module is assigned to and 0 is returned. The numerous
   sample modules provide examples of how to do this properly.

   @see s2_module_load()
*/
typedef int (*s2_module_init_f)( s2_engine * se, cwal_value ** module );

/**
   Holds information for mapping a s2_module_init_f to a name.
   Its purpose is to get installed by the S2_MODULE_xxx family of
   macros and referenced later via a module-loading mechanism.
*/
struct s2_loadable_module{
  /**
     Symbolic name of the module.
  */
  char const * name;

  /**
     The initialization routine for the module.
  */
  s2_module_init_f init;
};

/** Convenience typedef. */
typedef struct s2_loadable_module s2_loadable_module;

/**
   If compiled without S2_ENABLE_MODULES then this function always
   returns CWAL_RC_UNSUPPORTED and updates the error state of its
   first argument with information about that code.

   Its first argument is the controlling s2_engine.

   Its second argument is the name of a DLL file.

   Its third argument is the name of a symbol in the given DLL which
   resolves to a s2_loadable_module pointer. It may be NULL, in which
   case a default symbol name is used (which is only useful when
   plugins are built one per DLL).

   The final parameter is the target for the module's result value,
   and it may not be NULL (but the value it points to should initially
   be NULL, as it will be overwritten). It is passed directly to the
   module's s2_loadable_module::init() function, which is responsible
   for (on success) assigning *mod to the value the module wants to
   return.

   This function tries to open a DLL named fname using the system's
   DLL loader. If none is found, CWAL_RC_NOT_FOUND is returned and the
   s2_engine's error state is populated with info about the error. If
   one is found, it looks for a symbol in the DLL: if symName is not
   NULL and is not empty then the symbol "s2_module_symName" is
   sought, else "s2_module". (e.g. if symName is "foo" then it
   searches for a symbol names "s2_module_foo".) If no such symbol is
   found then CWAL_RC_NOT_FOUND (again) is returned and the
   s2_engine's error state is populated, else the symbol is assumed to
   be a (s2_loadable_module*), its init() function is called, and its
   result is returned to the caller of this function.

   On error, this routine generally updates the s2_engine's error
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
   instead called indirectly via s2_cb_module_load(), which is a
   script-side binding of this function.

   @see s2_cb_module_load()
   @see S2_MODULE_DECL
   @see S2_MODULE_IMPL
   @see S2_MODULE_REGISTER
   @see S2_MODULE_REGISTER_
*/
int s2_module_load( s2_engine * se, char const * fname,
                    char const * symName, cwal_value ** mod );

/**
   Behaves similarly to s2_module_load(), and its first 3 parameters
   are used as documented for that function, but this variant does not
   invoke the init() method of the module before returning that module
   via *mod.

   On success *mod is set to the module object. Its ownship is kinda
   murky: it lives in memory made available via the module loader. It
   remains valid memory until the DLL is closed.

   Returns 0 on success. On error, se's error state may contain more
   information.

   After calling this, the next call would typically be
   s2_module_init().

   @see s2_module_load()
   @see s2_module_init()
*/
int s2_module_extract( s2_engine * se,
                       char const * dllFileName,
                       char const * symName,
                       s2_loadable_module const ** mod );

/**
   This function pushes a new cwal scope, calls mod->init(), and
   propagates any result value from that routine back out of that new
   scope via *rv. On error *rv is not modified. If rv is NULL then the
   Value result of the module init is ignored (destroyed before this
   routine returns unless the module stores is somewhere long-lived),
   but any integer result code of the init routine is propagated back
   to the caller of this function.

   @see s2_module_load()
   @see s2_module_extract()
*/
int s2_module_init( s2_engine * se,
                    s2_loadable_module const * mod,
                    cwal_value ** rv);

/** @def S2_MODULE_DECL

   Declares an extern (s2_loadable_module*) symbol called
   s2_module_#\#NAME.

   Use S2_MODULE_IMPL to create the matching implementation
   code.
   
   This macro should be used in the C or H file for a loadable module.
   It may be compined in a file with a single S2_MODULE_IMPL1()
   declaration with the same name, such that the module can be loaded
   both with and without the explicit symbol name.

   @see S2_MODULE_IMPL

*/
#define S2_MODULE_DECL(NAME)                            \
    extern const s2_loadable_module * s2_module_##NAME

/** @def S2_MODULE_IMPL
   
   Intended to be used to implement module declarations.  If a module
   has both C and H files, S2_MODULE_DECL(NAME) should be used in the
   H file and S2_MODULE_IMPL() should be used in the C file. If the
   DLL has only a C file (or no public H file), S2_MODULE_DECL is
   unnecessary.

   Implements a static s2_loadable_module object named
   s2_module_#\#NAME#\#_impl and a non-static (s2_loadable_module*)
   named s2_module_#\#NAME which points to
   s2_module_#\#NAME#\#_impl. (The latter symbol may optionally be
   declared in a header file via S2_MODULE_DECL.)

   INIT_F must be a s2_module_init_f() function pointer. That function
   is called when s2_module_load() loads the module.

   This macro may be combined in a file with a single
   S2_MODULE_IMPL1() declaration using the same NAME value, such that
   the module can be loaded both with and without the explicit symbol
   name.

   Example usage, in a module's header file, if any:

   ```
   S2_MODULE_DECL(cpdo);
   ```

   (The declaration is not strictly necessary - it is more of a matter
   of documentation.)
   
   And in the C file:

   ```
   S2_MODULE_IMPL(cpdo,cpdo_module_init);
   ```

   If it will be the only module in the target DLL, one can also add
   this:
   
   ```
   S2_MODULE_IMPL1(cpdo,cpdoish_install_to_interp);
   // _OR_ (every so slightly different):
   S2_MODULE_STANDALONE(cpdo,cpdoish_install_to_interp);
   ```

   Which simplifies client-side module loading by allowing them to
   leave out the module name when loading, but that approach only
   works if modules are compiled one per DLL (as opposed to being
   packaged together in one DLL).
   
   @see S2_MODULE_DECL
   @see S2_MODULE_IMPL1
*/
#define S2_MODULE_IMPL(NAME,INIT_F)                                     \
  static const s2_loadable_module                                       \
  s2_module_##NAME##_impl = { #NAME, INIT_F };                          \
  const s2_loadable_module * s2_module_##NAME = &s2_module_##NAME##_impl


/** @def S2_MODULE_IMPL1

   Implements a static "v1-style" s2_loadable_module symbol called
   s2_module_impl and a non-static (s2_loadable_module*) named
   s2_module which points to s2_module_impl

   INIT_F must be a s2_module_init_f.
   
   This macro must only be used in the C file for a loadable module
   when that module is to be the only one in the resuling DLL. Do not
   use it when packaging multiple modules into one DLL: use
   S2_MODULE_IMPL for those cases (S2_MODULE_IMPL can also be used
   together with this macro).

   @see S2_MODULE_IMPL
   @see S2_MODULE_DECL
   @see S2_MODULE_STANDALONE_IMPL
*/
#define S2_MODULE_IMPL1(NAME,INIT_F)                                \
  static const s2_loadable_module                                   \
  s2_module_impl = { #NAME, INIT_F };                               \
  const s2_loadable_module * s2_module = &s2_module_impl

/** @def S2_MODULE_STANDALONE_IMPL

    S2_MODULE_STANDALONE_IMPL() works like S2_MODULE_IMPL1() but is
    only fully expanded if the preprocessor variable
    S2_MODULE_STANDALONE is defined (to any value).  If
    S2_MODULE_STANDALONE is not defined, this macro expands to a dummy
    placeholder which does nothing (but has to expand to something to
    avoid leaving a trailing semicolon in the C code, which upsets the
    compiler (the other alternative would be to not require a
    semicolon after the macro call, but that upsets emacs' sense of
    indentation)).

    This macro may be used in the same source file as S2_MODULE_IMPL.

    The intention is that DLLs prefer this option over
    S2_MODULE_IMPL1, to allow that the DLLs can be built as standalone
    DLLs, multi-plugin DLLs, and compiled directly into a project (in
    which case the code linking it in needs to resolve and call the
    s2_loadable_module entry for each built-in module).

   @see S2_MODULE_IMPL1
   @see S2_MODULE_REGISTER
*/
#if defined(S2_MODULE_STANDALONE)
#  define S2_MODULE_STANDALONE_IMPL(NAME,INIT_F) S2_MODULE_IMPL1(NAME,INIT_F)
#else
#  define S2_MODULE_STANDALONE_IMPL(NAME,INIT_F) \
  extern void _s2_module_dummy_does_not_exist_()
#endif

/**
   Performs all the necessary setup for a v2-style module, including
   declaration and definition. NAME is the name of the module. This is
   normally called immediately after defining the plugin's init func
   (which is passed as the 2nd argument to this macro).

   See S2_MODULE_IMPL() and S2_MODULE_STANDALONE_IMPL() for
   the fine details.
*/
#define S2_MODULE_REGISTER(NAME,INIT_F)  \
  S2_MODULE_IMPL(NAME,INIT_F);           \
  S2_MODULE_STANDALONE_IMPL(NAME,INIT_F)

/**
   Functionally equivalent to:
   S2_MODULE_REGISTER(NAME, s2_module_init_#\#NAME).
*/
#define S2_MODULE_REGISTER_(NAME)                             \
  S2_MODULE_IMPL(NAME,s2_module_init_##NAME);          \
  S2_MODULE_STANDALONE_IMPL(NAME,s2_module_init_##NAME)


/**
   cwal_callback_f() impl which wraps s2_module_load().

   Script-side usages:

   // For single-module DLLs:
   var module = loadModule("filename");
   // Or, for multi-module DLLs:
   var module = loadModule("filename", "symbolName");

   On success it returns the module's value (which can be anything),
   or the undefined value if the module returns nothing (which would be
   unusual).

   If passed no symbol name, it assumes that the DLL is a
   single-module DLL and uses the symbol name "s2_module". If passed a
   symbol name, it looks for "s2_module_SYMBOL_NAME". If such a symbol
   is found it is assumed to be a (s2_loadable_module const *) and its
   init() function is called.

   On success 0 is returned. On error it throws or returns a
   lower-level error code (e.g. CWAL_RC_OOM).

   Achtung: there is no module caching going on here, and loading a
   module multiple times may be expensive or confusing (the returned
   objects from separate calls will, unless the module itself somehow
   caches results, be different instances).

   Achtung: this binding requires that s2_engine_from_args() return
   non-0 (it will do so if args->engine is managed by an s2_engine
   instance).
*/
int s2_cb_module_load( cwal_callback_args const * args,
                       cwal_value **rv );

#if 0
/* The FFI API was removed because it's unmaintained: see
   mod/_attic/s2_ffi.c */

/**
   A cwal_callback_f() implementation which allows calls to
   near-arbitrary C functions using FFI (Foreign Function Interface:
   https://en.wikipedia.org/wiki/Foreign_function_interface).

   Achtung: this is probably the most dangerous thing that a scripting
   engine could ever be allowed to do, and is implemented purely for
   educational purposes.

   Script-side usage:

   ffiCall("symbolName"); // for void function with no args
   ffiCall("symbolName", returnType); // for functions with no args
   ffiCall("symbolName", [ returnType, argType... ], args...);
   // e.g.
   ffiCall("open", [ FFI_INT, FFI_PTR, FFI_INT], "/etc/fstab", 0);

   On success this calls the foreign function and returns its result.
   On error it throws and returns an error code.
*/
int s2_cb_ffi_exec( cwal_callback_args const * args, cwal_value **rv );
#endif

/**
   Pushes one level of output buffer into se's output buffer stack.
   Buffering works similarly to PHP's ob_start() (and friends) support.
   While a buffer is active, all output send to cwal_engine_output()
   and friends is redirected to a buffer. The various s2_ob_xxx()
   functions can be used to:

   - fetch or discard the contents
   - push a new buffer onto the stack
   - pop the buffer from the stack (discarding its contents)

   When the interpreter is shut down it automatically removes any
   pushed buffers, but clients should call s2_ob_pop() once
   for each time they call s2_ob_push()

   Returns 0 on success, CWAL_RC_MISUSE if !se, CWAL_RC_RANGE if there
   has been no corresponding call to s2_ob_push().

   Results of the whole s2_ob_XXX() API are undefined if another API
   manipulates the contents of the underlying cwal_engine's output
   redirection bits (i.e. cwal_engine_vtab::outputer via
   cwal_engine::vtab).
   
   @see s2_ob_pop()
   @see s2_ob_get()
   @see s2_ob_take()
   @see s2_ob_clear()
   @see s2_ob_level()
   @see s2_ob_flush()
*/
int s2_ob_push( s2_engine * se );

/**
   Attempts to reserve at least reserveBufSize bytes of memory for the
   current buffering level. This does not change the buffering level.

   Returns 0 on success, CWAL_RC_MISUSE if !se, CWAL_RC_RANGE if
   s2_ob_push() has not previously been called, and CWAL_RC_OOM if
   allocation of new memory fails.

   @see s2_ob_push()
 */
int s2_ob_reserve( s2_engine * se, cwal_size_t reserveBufSize );

/**
   Removes the current level of output buffer from ie.

   Returns 0 on success, CWAL_RC_MISUSE if !ie, CWAL_RC_RANGE if there
   has been no corresponding call to s2_ob_push().
*/
int s2_ob_pop( s2_engine * se );

/**
   Returns the current buffering level, or 0 if !ie or ie is
   not in buffering mode.

   @see s2_ob_push()
   @see s2_ob_pop()
*/
cwal_size_t s2_ob_level( s2_engine * se );

/**
   Gets a pointer to the raw buffer owned by the current level of
   output buffer, assigning it to *tgt. The buffer is owned by the OB
   layer and its contents may be modified on any API routines which
   end up calling cwal_engine_output() or the other s2_ob_xxx()
   APIs. The caller is intended to copy/use the buffer's contents
   immediately, and not hold on to it past the current operation.

   Returns 0 on success, CWAL_RC_MISUSE if !ie or !tgt, CWAL_RC_RANGE
   if there has been no corresponding call to s2_ob_push().
*/
int s2_ob_get( s2_engine * se, cwal_buffer ** tgt );

/**
   Like s2_ob_get(), but moves the contents of the current
   buffer layer into tgt, clearing the OB buffer but leaving
   it on the buffer stack for later use.

   Returns 0 on success, CWAL_RC_MISUSE if !se or !tgt, CWAL_RC_RANGE
   if there has been no corresponding call to s2_ob_push().

   tgt must be empty-initialized or the caller must call
   cwal_buffer_reserve(..., tgt, 0) before calling this or memory may
   leak. On success ownership of the memory in tgt->mem is transfered
   to the caller. If tgt was created via cwal_new_buffer() or
   cwal_new_buffer_value() then tgt and tgt->mem are owned by se->e.
*/
int s2_ob_take( s2_engine * se, cwal_buffer * tgt );

/**
   Clears the contents of the current buffering layer. If
   releaseBufferMem is true (non-0) then the buffer memory is
   deallocated, otherwise it is just reset for later use by the OB
   layer. If it is deallocated, it will be re-allocated later if more
   output is buffered.

   Returns 0 on success, CWAL_RC_MISUSE if !se, CWAL_RC_RANGE
   if there has been no corresponding call to s2_ob_push().
*/
int s2_ob_clear( s2_engine * se, char releaseBufferMem );

/**
   Pushes the current contents of the output buffer layer to the next
   output destination in the stack and the current level is cleared of
   contents (but stays on the stack). If the next outputer is a buffer
   then the current buffer is appended to it, otherwise it is sent to
   the originally configured output destination.

   Returns 0 on success, CWAL_RC_MISUSE if !se, CWAL_RC_RANGE
   if there has been no corresponding call to s2_ob_push(),
   and potentially some other error if flushing to the lower-level
   implementation fails.

   @see s2_ob_push()
   @see s2_ob_pop()
*/
int s2_ob_flush( s2_engine * se );


/**
   cwal_callback_f() impl wrapping s2_ob_push(). Requires that
   args->state be a (s2_engine*). Returns argv->self.

   Accepts an optional integer argument which specifies an amount of
   memory to pre-allocate for the buffer (see s2_ob_reserve()).

   On error this function returns with an unchanged buffer level.
*/
int s2_cb_ob_push( cwal_callback_args const * args, cwal_value **rv );

/**
   cwal_callback_f() impl wrapping s2_ob_pop(). Requires
   that args->state be a (s2_engine*).

   Script signature:

   ```
   mixed pop([int takePolicy=0])
   ```

   If passed no args or a 0/falsy value, it discards any buffered
   output. If passed numeric greater than 0 then it returns (via *rv)
   the content as a Buffer. If passed numeric negative then it returns
   the contents as a String.

*/
int s2_cb_ob_pop( cwal_callback_args const * args, cwal_value **rv );

/**
   cwal_callback_f() impl wrapping s2_ob_reserve(). Requires
   that args->state be a (s2_engine*). Returns argv->self.
*/
int s2_cb_ob_reserve( cwal_callback_args const * args, cwal_value **rv );

/**
   cwal_callback_f() impl wrapping s2_ob_get(). Requires
   that args->state be a (s2_engine*).

   Assigns *rv to the string contents of the buffer layer.
*/
int s2_cb_ob_get( cwal_callback_args const * args, cwal_value **rv );
/**
   cwal_callback_f() impl wrapping s2_ob_clear(). Requires
   that args->state be a (s2_engine*). Returns argv->self.
*/
int s2_cb_ob_clear( cwal_callback_args const * args, cwal_value **rv );

/**
   cwal_callback_f() impl wrapping s2_ob_take(). Requires
   that args->state be a (s2_engine*).

   Assigns *rv to the string contents of the buffer layer.

   Design note: the returned string is actually a z-string to avoid
   having to make another copy of the data.
*/
int s2_cb_ob_take_string( cwal_callback_args const * args, cwal_value **rv );

/**
   Functionally identical to s2_cb_ob_take_string() except that it
   returns (via *rv) a cwal_buffer value (owned by args->engine).
*/
int s2_cb_ob_take_buffer( cwal_callback_args const * args, cwal_value **rv );

/**
   cwal_callback_f() impl wrapping s2_ob_flush(). Requires
   that args->state be a (s2_engine*). Returns argv->self.
*/
int s2_cb_ob_flush( cwal_callback_args const * args, cwal_value **rv );    

/**
   cwal_callback_f() impl for...

   mixed capture(string|function callback
                 [, int captureMode=-1 | buffer captureTarget])

   Which does:

   1) Push an OB level.

   2) Runs the given callback. If it's a function, it is call()ed. If it
   is a string, it is eval'd. Any other type, including a buffer, triggers
   an error.

   3) If the 2nd argument is a buffer, all captured output is appended
   to that buffer and that buffer is returned. If it's not a buffer,
   it's interpreted as an integer with the same semantics as pop()'s
   argument but with a different default value: if it's negative (the
   default) then the captured buffered output is returned as a string,
   positive returns the result as a new buffer, and 0 means to simply
   discard the result.

   4) Pops its buffer.

   If the callback leaves the buffer stack count with fewer levels
   than than what were active when the callback was triggered, steps
   (3) and (4) are skipped and an exception is triggered. If it pushes
   extra levels, they are assumed to be part of the output: this
   function flushes and pops each one, and captures or discards the
   cumulative output.

   The main advantage to this approach to capturing output, over
   manually pushing and popping OB levels, is that this function keeps
   the levels in sync even in the face of an s2-level
   assert/exit/fatal call, cwal/s2 OOM condition, s2_interrupt(), or
   similar "flow-control event."
*/
int s2_cb_ob_capture( cwal_callback_args const * args, cwal_value **rv );

/**
   Installs the following functions into tgt (which must be a property
   container type), all of which correspond to a similarly named
   s2_ob_XXX() resp. s2_cb_ob_XXX() function:

   push(), pop(), getString(), takeString(), takeBuffer(), clear(),
   flush(), capture()

   Returns 0 on success. On error tgt might have been partially
   populated.

   Returns CWAL_RC_MISUSE if !ie or !tgt, CWAL_RC_TYPE if tgt
   is not a container type.
*/
int s2_install_ob( s2_engine * se, cwal_value * tgt );

 /**
   Variant of s2_install_ob() which installs the OB functionallity
   into a new object with the given name, and places that object in
   tgt. Returns 0 on success.
*/
int s2_install_ob_2( s2_engine * se, cwal_value * tgt,
                     char const * name );


 /**
   Installs various io-related APIs. If name is not NULL and not
   empty, then the APIs get installed in a new object named tgt[name],
   else the functions are installed directly in tgt (which must be a
   container type).

   Returns 0 on success.

   Functions installed by this:

   print(), output(), flush()
*/
int s2_install_io( s2_engine * se, cwal_value * tgt,
                   char const * name );

 /**
   Installs various filesystem-related APIs which are largely
   platform-dependent. If name is not NULL and not empty, then the
   APIs get installed in a new object named tgt[name], else the
   functions are installed directly in tgt (which must be a container
   type).

   Returns 0 on success.

   Functions installed by this:

   chdir(), getcwd(), mkdir(), realpath(), stat(), fileIsAccessible(),
   dirIsAccessible().

   All of these functions are affected by the s2_disable_get() flags
   S2_DISABLE_FS_STAT, and mkdir() is also affected by
   S2_DISABLE_FS_WRITE.
*/
int s2_install_fs( s2_engine * se, cwal_value * tgt,
                   char const * name );

/**
   A cwal_callback_f() which expects its first argument to be an
   integer. It tries to sleep for at least that many seconds and
   returns the number of seconds left to sleep if it is interrupted
   (as per sleep(3)).

   @see s2_install_time()
 */
int s2_cb_sleep(cwal_callback_args const * args, cwal_value ** rv);

/**
   A cwal_callback_f() which expects its first argument to be an
   integer. It sleeps for that many milliseconds. It throws an
   exception if usleep(3) fails or if the library is built without
   usleep(3) support. It returns the undefined value.

   @see s2_install_time()
*/
int s2_cb_mssleep(cwal_callback_args const * args, cwal_value ** rv);

/**
   Installs the following script-side functions:

   sleep(), mssleep(), time(), mstime(), strftime()

   If name is not 0 and *name is not 0 then a new property with that
   name, of type object, is installed to tgt, and the functions are
   stored there, otherwise the functions are installed directly into
   tgt. Returns 0 on success. On error non-0 is returned (a
   CWAL_RC_xxx value) and, depending on where the error happened, the
   module may or may not have been partially installed in the target
   object. The only "likely" (for a given definition of "likely")
   error from this function, assuming all arguments are valid, is
   CWAL_RC_OOM, indicating that it ran out of memory. Returns
   CWAL_RC_MISUSE if either the first or second argument are NULL, and
   CWAL_RC_TYPE if !cwal_props_can(tgt).

   @see s2_cb_sleep()
   @see s2_cb_mssleep()
   @see s2_cb_time()
   @see s2_cb_mstime()
   @see s2_cb_strftime()
*/
int s2_install_time( s2_engine * se, cwal_value * tgt, char const * name );



/**
   Flags for use with s2_tmpl_opt::flags.
*/
enum s2_tmpl_flags_e {
/**
   Indicates that the output function header definition
   which checks for and optionally defines the function
   TMPLOUT (used by the processed template to output
   its content) should be elided (i.e. not output).
 */
S2_TMPL_ELIDE_TMPLOUT = 0x01
};

typedef struct s2_tmpl_opt s2_tmpl_opt;
/**
   Holds options for the s2_tmpl_to_code() function.  Clients
   must initialize them by copying either s2_tmpl_opt_empty or
   (for const contexts) s2_tmpl_opt_empty_m.  */
struct s2_tmpl_opt {
    /**
       0 (for no flags) or a bitmask of values from
       the s2_tmpl_flags_e enum.
     */
    int flags;
    /**
       If this is not 0 and not empty then:

       (A) the flag S2_TMPL_ELIDE_TMPLOUT
       is implied

       (B) this specifies the script function name which will be
       called when the processed template is eval'd, to emit its
       output.

       If 0 then "TMPLOUT" is used and (A) does not apply.
    */
    char const * outputSymbolPublic;

    /**
       In the processed output, the outputSymbolPublic name is only
       used in the header and aliased to a shorter symbol (so that the
       output will, for non-trivial cases, be shorter). This member
       specifies the name it uses after initialization. If it is 0 or
       starts with a NUL byte then some unspecified (but short)
       default is used. It is recommended that clients (if they use
       this) use weird non-ASCII UTF8 character combinations to avoid
       any potential symbol collisions.

       If this is not NULL and has a length greater than 0 then this
       alias is used in place of the default, and is initialized as
       a scope-local variable in the header of the template if
       it has not previously been declared in that scope.

       The likely only reason this should be overridden is that
       1-in-a-gazillion chance that a template actually uses a symbol
       which collides with this one's default value.
    */
    char const * outputSymbolInternal;

    /**
       Similar to outputSymbolInternal, this specifies the name of the
       processed-template-internal heredoc delimiter. By default (if
       this is 0) some cryptic combination of non-ASCII UTF8 character
       is used.
     */
    char const * heredocId;

    /**
       The opening tag for "code" blocks. Default is "<?".
       If set, then tagCodeClose must also be set. Must differ
       from all other tag open/close entries.
     */
    char const * tagCodeOpen;

    /**
       The opening tag for "code" blocks. Default is "?>".
       If set, then tagCodeOpen must also be set. Must differ
       from all other tag open/close entries.
    */
    char const * tagCodeClose;

    /**
       The opening tag for "value" blocks. Default is "<%".
       If set, then tagValueClose must also be set. Must differ
       from all other tag open/close entries.
    */
    char const * tagValueOpen;

    /**
       The opening tag for "value" blocks. Default is "%>".
       If set, then tagValueOpen must also be set. Must differ
       from all other tag open/close entries.
    */
    char const * tagValueClose;
};

/**
   An initialized-with-defaults instance of s2_tmpl_opt,
   intended for const-copy initialization.
*/
#define s2_tmpl_opt_empty_m {0,0,0,0,0,0,0,0}

/**
   An initialized-with-defaults instance of s2_tmpl_opt,
   intended for copy initialization.
*/
extern const s2_tmpl_opt s2_tmpl_opt_empty;

/**
   Implements a very basic text template processing mechanism for
   th1ish.

   The e arg must be a valid cwal_engine instance.

   src must be the template source code to process. It is treated as
   nearly-opaque text input which may contain markup tags (described
   below) to embed either code blocks or values into the output.

   dest is where all output is appended (the buffer is not reset by
   this function).

   The opt parameter may be 0 (for default options) or an object which
   configures certain parts of the template processing, as described
   in the s2_tmpl_opt docs.

   Returns 0 on success, non-0 on error. Error codes include:

   - CWAL_RC_MISUSE if !e, !src, or !dest.

   - CWAL_RC_RANGE if any of the open/close tags specified in the opt
   parameter are invalid (empty strings or validate rules described in
   the s2_tmpl_opt docs).

   - CWAL_RC_OOM on allocation errors.

   - CWAL_RC_EXCEPTION if it is reporting an error via a cwal
   exception. On code generation errors it throws an exception in
   the context of e, containing information about the nature and
   location (in the original source) of the problem.

   That said, it may not catch many conceivable malformed content
   cases and in such cases may generate malformed (as in not
   eval'able) code.


   Template processing...

   (Note that while these docs use fixed tag names, the exact
   tags can be configured via the opt parameter.)

   The output starts with a document prefix which sets up output of
   the text parts of the page.

   All non-code parts of src are filtered to be output wrapped in
   individual HEREDOCs embedded in the output script. All code parts
   of src are handled as follows:

   '<?' (without the quotes) starts a code block, running until and
   closing '?>' tag. This ends any current HEREDOC and passes through
   the code as-is to dest.  It does not generate any output in the
   processed document unless the embedded code generates it.

   '<%' (without the quotes) starts a "value block," which is
   processed a little bit differently. The contents between that and
   the next '%>' tag are simply passed to the configured output
   routine (see below).

   An example input document should clear this up:

   ```
   Hi, world!
   <? var x = 1, y = 2 ?>
   x = <%x%>, y = <% y %>, x+y=<% x + y %>
   ```

   The generated code is an s2 script which, when run (via eval,
   scope, catch...), outputs a processed document. All non-script
   parts get wrapped in HEREDOCs for output.

   The generated code "should" evaluated in a scope of its own, but it
   can be run in the current scope if desired. The code relies on an
   output function being defined (resolvable in the evalution scope).
   That function, if not specified via the opt parameter, is called
   TMPLOUT. No name is specified and the symbol TMPLOUT is undefined
   (when the processed template is eval'd), it uses s2out as its
   default output function (prior to 20191210 it uses
   s2.io.output). The function must accept any number of Value type
   parameters and output them "in its conventional string form"
   (whatever that is). It must not perform any formatting such as
   spaces between the entries or newlines afterwards. It may define
   formatting conventions for values passed to it (e.g. it may feel
   free to reformat doubles to a common representation).

   The generator outputs some weird/cryptic UTF8 symbols as heredoc
   markers. It's conceivable, though very unlikely, that these could
   collide with symbols in the document for heredoc processing
   purposes.

   Whitespace handling:

   - If the script starts with <? or <%, any whitespace leading up to
   that are discarded, otherwise all leading whitespace is retained.

   - Replacement of <% %> and <? ?> blocks retains whitespace to the
   left of the openener and right of the closer, so {abc<%x%>def} will
   form a single output token (provided 'x' evaluates to such), where
   {abc <%x%> def} will generate three. Inside the <? ?> blocks, all
   whitespace is retained. Inside <% %> blocks, the contents are
   treated as if they were inside a normal HEREDOC, so their
   leading/trailing spaces are stripped BUT they are not significant -
   the _result_ of evaluating the <% %> content gets output when
   executed, not the content itself.

   TODOs:

   - a variant which takes a cwal_output_f() instead of a buffer.
*/
int s2_tmpl_to_code( cwal_engine * e, cwal_buffer const * src,
                     cwal_buffer * dest, s2_tmpl_opt const * opt );

/**
   A cwal_callback_f() binding for s2_tmpl_to_code(). It
   expects one string/buffer argument containing tmplish code and it
   returns a new Buffer value containing the processed code. Throws on
   error.

   Script usage:

   var compiled = thisFunction(templateSource [, optObject])

   If optObject is-a Object then the following properties may
   influence template processing:

   - valueOpen and valueClose specify the open/close tags for
   Value Blocks.

   - codeOpen and codeClose specify the open/close tags for
   Code Blocks.

   - outputSymbol sets the name of the symbol the generated tmpl()
   code will (when eval'd) use for output. It may be a compound
   symbol, e.g. "s2.io.output" or even a function call, e.g.
   "proc(){return s2.io.output}()" - anything which is legal as the
   right-hand side of an assignment is (syntactically) legal
   here. That assignment will be called once each time the resulting
   template script is eval'd.
*/
int s2_cb_tmpl_to_code( cwal_callback_args const * args, cwal_value ** rv );

/**
   A cwal_callback_f() impl binding the C-standard time(3).

   @see s2_install_time()
*/
int s2_cb_time( cwal_callback_args const * args, cwal_value **rv );

/**
   A cwal_callback_f() impl returning the current time in milliseconds
   since the start of the Unix epoch. This requires platform-specific
   calls and throws an exception, with code CWAL_RC_UNSUPPORTED, if
   built on a platform with the required API.

   @see s2_install_time()
*/
int s2_cb_mstime( cwal_callback_args const * args, cwal_value **rv );

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
cwal_midsize_t s2_strftime(char *dest, cwal_midsize_t destLen,
                           const char *format, const struct tm *timeptr);

    
/**
   A cwal_callback_f() which wraps s2_strftime().

   Script usage:

   ```
   var tm = time();
   var str = strftime("%Y-%m-%d %H:%M:%S", tm);
   ```

   The default time value, if no second argument is passed in or a
   negative value is passed in, is the current time.

   Note that this implementation has a limit on the length of the
   result string (because s2_strftime() works that way), and
   throws if that length is violated. It's suitable for "usual"
   time strings but not for formatting whole sentences.

   This function takes an optional boolean 3rd argument: if truthy,
   local time is used, else GMT is used (the default).
*/
int s2_cb_strftime( cwal_callback_args const * args, cwal_value **rv );

/**
   Tries to convert an errno value to an equivalent CWAL_RC_xxx value.
   First argument must be the current errno value. Returns an
   equivalent, or dflt is no sematic equivalent is known. If errNo is
   0 then this function evaluates the global errno in its place. If
   both are 0 then this function returns CWAL_RC_OK.
*/
int s2_errno_to_cwal_rc(int errNo, int dflt);

/**
   A cwal_callback_f() impl when behaves like rand(3). It will call
   srand(3), with some pseudo-random seed, the first time it is
   called.

*/
int s2_cb_rand_int( cwal_callback_args const * args, cwal_value **rv );


/**
   cwal_callback_f() implementing fork(). On non-Unix builds this function
   triggers an exception.

   Script usage:

   fork(Function)
   fork(bool,Function)

   The parent process returns from that call. The child process runs
   the given Function and then exits the interpreter as if the 'exit'
   keyword had been used. If the first arg is a boolean true then then
   child process returns (in its own process) instead of exiting, passing
   back the result of the 2nd parameter (the callback function).
*/
int s2_cb_fork( cwal_callback_args const * args, cwal_value **rv );

/**
   A cwal_callback_f() implementation which returns a hashtable value
   which maps all of the cwal/s2-defined result code integers
   (CWAL_RC_xxx and S2_RC_xxx) to strings in the same form as the
   corresponding enum entry. e.g. it maps 0 to "CWAL_RC_OK". It also
   holds the reverse mappings, so "CWAL_RC_OK" ==> 0.

   This function creates the hash the first time it is called and
   caches its result in the s2_engine which owns args->engine, thus
   the second and subsequent calls will return the same value. 
*/
int s2_cb_rc_hash( cwal_callback_args const * args, cwal_value **rv );

/**
   A cwal_callback_f() implementation for formatting "this"
   using cwal_buffer_format (resp. s2.Buffer.appendf()).

   Requires args->argv[0] to be part of a formatting string for
   s2.Buffer.appendf(), namely the part after "$1%". It prepends "$1%"
   to the arg string and passes that string and the argument to
   cwal_buffer_format() to generate the result string.

   Triggers an exception if cwal_buffer_format() returns an error.
*/
int s2_cb_format_self_using_arg( cwal_callback_args const * args, cwal_value **rv );

/**
   A callback which creates and returns a new "unique" value, as per
   cwal_new_unique(). If args->argc then args->argv[0] is passed to
   cwal_new_unique().
*/
int s2_cb_new_unique( cwal_callback_args const * args, cwal_value **rv );

/**
   Experimental!

   Intended to be called by app-level code and be passed its
   shared/global s2_engine instance. This function uses global
   state and is NOT thread-safe in any way, shape, or form. It
   stores a copy of se.

   If se is not NULL then this installs a SIGINT handler which behaves
   like s2_interrupt() (but uses a different message string). If se is
   NULL then it removes any previous binding (it does not remove its
   SIGINT handler, but the handler becomes a no-op if no engine is set
   to be interrupted).
*/
void s2_set_interrupt_handlable( s2_engine * se );

/**
   This sets se's error state to CWAL_RC_INTERRUPTED, a flag it checks
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

   - se is clearing its error state (which will clear the
     is-interrupted flag). It resets its error state internally for
     "non-error errors" which propagate a ways, like "return" and
     "throw", but those keywords "should" catch and propagate this
     condition, trumping their own. The lowest-level eval handler
     checks at every sensible opportunity.

   - se is cleaning up (inside s2_engine_finalize()), in which case
     accessing it might (depending on the timing)lead to an illegal
     memory access (if dynamically allocated) or a useless but
     harmless[1] access if it's stack-allocated. [1]=so long as the
     memory itself is still legal to access (e.g. app-level/static).

   - Third-party bindings may clear se's error state (which includes
     this flag) indescriminately, without being aware of this
     condition.


   There are likely others.
*/
int s2_interrupt( s2_engine * se );

/**
   Tries to parse a given C-string as an integer or double value.

   srcLen must be the string length of str or a negative value
   (in which case cwal_strlen() is used to count the length).

   If the string can be parsed to an integer or double value, *rv is
   set to the newly-created value and 0 is returned.  If no conversion
   can be made, *rv is set to 0 and 0 is returned.  On OOM error,
   CWAL_RC_OOM is returned.
*/
int s2_cstr_parse_number( cwal_engine * e, char const * str,
                          cwal_int_t srcLen, cwal_value ** rv );

/**
   The reverse of s2_rc_cstr(), this function tries to find a
   CWAL_RC_xxx or S2_RC_xxx error code for a string form of an enum
   entry's name, e.g. "CWAL_RC_OOM". On success it returns the code
   value via *code and returns true, else it does not modify
   *code and returns false.

   This is currently an O(1) operation, performing one hash
   calculation and (at most) one string comparison.
*/
bool s2_cstr_to_rc(char const *str, cwal_int_t len, int * code);

/**
   Rescopes v (if necessary per the scoping rules) to one scope
   up from the current cwal scope. This is almost never what you
   want to do.
*/
void s2_value_upscope( s2_engine * se, cwal_value * v );

/**
   If cwal_value_is_unique() is true for v then this returns the value
   of passing v to cwal_unique_wrapped_get(), else it returns v.

   In s2, the cwal_value_is_unique() type is the type used for enum
   entries.

   @see s2_value_cstr()
*/
cwal_value * s2_value_unwrap( cwal_value * v );

/**
   Const-friendly brother of s2_value_unwrap().
*/
cwal_value const * s2_value_unwrap_c( cwal_value const * v );

/**
   Works like cwal_value_cstr() unless cwal_value_is_unique(v) is
   true, in which case it uses v's wrapped value instead of v
   itself. In s2, "unique" values are most often enum entries.

   @see s2_value_unwrap()
*/
char const * s2_value_cstr( cwal_value const * v, cwal_size_t * len );

/**
   A helper for callbacks which honor the do-not-set-properties
   flag on container values.

   If v is tagged with the flag CWAL_CONTAINER_DISALLOW_PROP_SET then
   this function triggers a CWAL_RC_DISALLOW_PROP_SET exception and
   returns non-0 (intended to be propagated back out of the
   callback). If it does not have that flag, 0 is returned.

   If throwIt is true (non-0), the error is transformed to an exception,
   otherwise it is set as a non-exception error.
*/
int s2_immutable_container_check( s2_engine * se, cwal_value const * v, int throwIt );

/**
   Functionally identical to s2_immutable_container_check() but extracts the
   s2_engine from s2_engine_from_args(args) and always passes true
   as the 3rd argument to s2_immutable_container_check().
*/
int s2_immutable_container_check_cb( cwal_callback_args const * args, cwal_value const * v );

/**
   Configures various s2- and cwal-level flags for the given
   container-type value. Returns 0 if it sets/clears the flag(s),
   CWAL_RC_TYPE if v is not a container (not counting prototypes).

   The options are:

   allowPropSet: if true (the default) then the container mutation
   APIs work as normal, otherwise those which set properties will
   fail with a CWAL_RC_DISALLOW_PROP_SET error code.

   allowNewProps: if true (the default) new properties are created
   normally via the various setter operations. If false, trying to set
   a non-existing property will fail with a
   CWAL_RC_DISALLOW_NEW_PROPERTIES error code.

   allowGetUnknownProps: if true (the default) then unknown properties
   resolve to the undefined value or NULL (depending on the
   context). If false, s2_get_v() and friends will trigger a
   CWAL_RC_NOT_FOUND error for unknown properties. Note that
   properties which resolve through a prototype are still "known" for
   this purpose (they must be, or inheritance of methods could
   not work).
*/
int s2_container_config( cwal_value * v, char allowPropSet,
                         char allowNewProps,
                         char allowGetUnknownProps );

/**
   Experimenting with with ideas for things we "could" use
   per(-container-type)-Value flag bits (16 of them) in s2.  cwal
   gives us 16 bits per Container Value instance (not POD types, as we
   can't tag those further without increasing their sizeofs, dropping
   the built-in constants, or splitting the values into two
   allocations, such that we could do a CoW of the built-ins if they
   get flagged). See cwal_container_client_flags_get() and
   cwal_container_client_flags_set().
*/
enum s2_just_thinking_out_loud {

/**
   An alternate encoding might be to have the high (say) 4 (or 8) bits
   control the interpretation of the (say) bottom 12. e.g.

   MODE_CLASS = 0x1000,
   F_CLASS_CONST = MODE_CLASS | 0x01,
   F_CLASS_STATIC = MODE_CLASS | 0x02
   MODE_FUNCTION = 0x2000,
   F_FUNC_CTOR = MODE_FUNCTION | 0x01,
   ...

   Except that each value could have at most 1 mode. Unless we split
   into multiple mode groups:

   MODE_4_MASK = 0x8F00,
   MODE_3_MASK = 0x40F0,
   MODE_2_MASK = 0x200F,
   MODE_1_MASK = 0x1FFF,

   i.e. 3 modes, each with 4 bits, and a fallback mode with 12. Except
   that that gains us nothing (or very little).
*/

/**
  4 mutually exclusive modes, each with the bottom 8 bits reserved for
  itself.
*/
S2_VAL_F_MODE_MASK  = 0xF000U,
S2_VAL_F_MODE_FUNC  = 0x1000,
S2_VAL_F_MODE_CLASS = 0x2000,
S2_VAL_F_MODE_DOT = 0x4000,
S2_VAL_F_MODE_4 = 0x8000,
/**
   Bits (9-12) are "shared" (independent of the mode).
*/
S2_VAL_F_COMMON_MASK = 0x0F00,

/**
   Functions with this tag would get special handling when called by
   the 'new' keyword.
*/
S2_VAL_F_FUNC_CTOR  = S2_VAL_F_MODE_FUNC | 0x01,

/**
   For potential use in creating property interceptors.
*/
S2_VAL_F_FUNC_GETTER  = S2_VAL_F_MODE_FUNC | 0x02,
S2_VAL_F_FUNC_SETTER  = S2_VAL_F_MODE_FUNC | 0x04,
S2_VAL_F_FUNC_INTERCEPTOR  = S2_VAL_F_FUNC_SETTER | S2_VAL_F_FUNC_GETTER,

/**
   Objects created via the 'class' keyword. Get special
   property lookup treatment.
*/
S2_VAL_F_CLASS = S2_VAL_F_MODE_CLASS | 0x01,
/* Problem with some words, like const, is that we can only tag container types.
   So we'd need to use property-level constness. */
S2_VAL_F_CLASS_CONST = S2_VAL_F_MODE_CLASS | 0x02,
S2_VAL_F_CLASS_STATIC = S2_VAL_F_MODE_CLASS | 0x04,
/* It's unlikely that s2's engine can currently support the concept of
   private/protected vars/properties, but for the sake of bitmask
   planning... */
S2_VAL_F_CLASS_PRIVATE = S2_VAL_F_MODE_CLASS | 0x08,  /* or PUBLIC, if we default to private and can enforce it */
S2_VAL_F_CLASS_PROTECTED = S2_VAL_F_MODE_CLASS | 0x10,

/**
   Objects with this tag (created by passing a S2_VAL_F_CLASS-tagged
   Object to the 'new' keyword) might get special property lookup
   (and assignment) semantics, e.g.  limit them to class-defined
   properties.
*/
S2_VAL_F_CLASS_INSTANCE = S2_VAL_F_MODE_CLASS | 0x20,
/**
   Indicates the container is an enum.
*/
S2_VAL_F_CLASS_ENUM     = S2_VAL_F_MODE_CLASS | 0x40,
/**
   Hmm. The enum entry type (cwal type "unique") cannot have
   flags, so this is apparently unusued.
*/
S2_VAL_F_CLASS_ENUM_ENTRY  = S2_VAL_F_MODE_CLASS | 0x80,

/**
   This client container flag (cwal_container_client_flags_get() and
   friends)) causes s2_get_v() and friends to trigger an exception if
   a property request on a container with this flag does not find an
   entry.

   s2_set_v() does this differently to avoid having to do duplicate
   lookups. This feature was added to the core as the
   CWAL_CONTAINER_DISALLOW_NEW_PROPERTIES container flag and
   the CWAL_RC_DISALLOW_NEW_PROPERTIES result code.
*/
S2_VAL_F_DISALLOW_UNKNOWN_PROPS = S2_VAL_F_COMMON_MASK &  0x100,

/**
   This client container flag (cwal_container_client_flags_get() and
   friends) gets temporarily set on values created by the 'new'
   keyword, for the duration of the constructor call.
*/
S2_VAL_F_IS_NEWING = S2_VAL_F_COMMON_MASK & 0x0200,

/**
   Denotes an enum instance. May be a hash or an object.
*/
S2_VAL_F_ENUM = S2_VAL_F_CLASS_ENUM | S2_VAL_F_DISALLOW_UNKNOWN_PROPS,

/**
   Means something like: this Value should be treated like an
   Object for most purposes.

   We currently (experimentally, 20141202) use this on certain Hash
   Table instances. Its interpretation is that the dot op should use
   the Value's hash entries instead of its object properties. We
   somehow need to accommodate inherited methods, though. So we first
   check the hashtable, then object-level properties, then up the
   prototype chain. Any prototypes with this flag would of course
   apply the same logic.
*/
S2_VAL_F_DOT_LIKE_OBJECT = S2_VAL_F_MODE_DOT | 0x002,
/* S2_VAL_F_DOT_ARROW_LIKE_DOT = S2_VAL_F_MODE_DOT | 0x004, */

/**
   A mask of all bits, just as a reminder that we'd be limited to 16
   bits (because cwal has no more space to spare without increasing
   the sizeof() for all properties-capable Values).
*/
S2_VAL_F_MASK = 0xFFFF
};

/**
   Utility type for use with s2_install_functions(), allowing simple
   installation of a series of callbacks in one go.
*/
struct s2_func_def {
  char const * name;
  cwal_callback_f callback;
  void * state;
  cwal_finalizer_f stateDtor;
  void const * stateTypeID;
  /**
     A mask of S2_VAL_F_xxx vars
  */
  uint16_t cwalContainerFlags;
};
#define S2_FUNC6(NAME,CALLBACK,STATE,StateDtor,StateTypeId,CwalContainerFlags)  \
  {NAME,CALLBACK,STATE,StateDtor,StateTypeId, CwalContainerFlags}
/**
   Convenience macro for initializing an s2_func_def entry.
*/
#define S2_FUNC5(NAME,CALLBACK,STATE,StateDtor,StateTypeId) \
  S2_FUNC6(NAME,CALLBACK,STATE,StateDtor,StateTypeId, 0)
/**
   Convenience macro for initializing an s2_func_def entry.
*/
#define S2_FUNC2(NAME,CALLBACK) S2_FUNC5(NAME,CALLBACK,0,0,0)
/**
   EXPERIMENTAL: don't use.

   Convenience macro for initializing an s2_func_def entry.
*/
#define S2_FUNC2_INTERCEPTOR(NAME,CALLBACK) S2_FUNC6(NAME,CALLBACK,0,0,0,S2_VAL_F_FUNC_INTERCEPTOR)

/**
   Empty-initialized const s2_func_def struct.
*/
#define s2_func_def_empty_m {0,0,0,0,0,0}
/** Convenience typedef. */
typedef struct s2_func_def s2_func_def;

/**
   Installs a list of cwal callback functions into the given target
   container value. defs must be an array of s2_func_def objects
   terminated by an entry with a NULL name field (most simply, use
   s2_func_def_empty_m to intialize the final element). All member
   pointers in each entry must be valid, and 0 is (generally speaking)
   valid for all but the name and callback fields.

   If propertyFlags is not 0 then each property gets set with those
   flags, as per cwal_prop_set_with_flags().

   If tgt is NULL then the functions are installed into the _current_
   cwal scope. Note that outside of initialization of the engine, the
   current scope is very likely not the top-most, and may well
   disappear soon (e.g. using this from within a cwal_callback_f()
   implementation will only install these for the duration of the
   current function call!). (Potential TODO: add
   s2_install_functions_in_scope(), which takes a scope pointer)

   Returns 0 on success, a non-0 CWAL_RC_xxx code on error.

   Example:

   ```
   const s2_func_def funcs[] = {
     S2_FUNC2("myFunc1", my_callback_1),
     S2_FUNC2("myFunc2", my_callback_2),
     s2_func_def_empty_m // IMPORTANT that the list end with this!
   };
   int rc = s2_install_functions(se, myObj, funcs, CWAL_VAR_F_CONST);
   ```
*/
int s2_install_functions( s2_engine *se, cwal_value * tgt,
                          s2_func_def const * defs,
                          uint16_t propertyFlags );
/**
   A convenience function to install value v into the tgt object, or
   into the current scope if tgt is NULL. If nameLen is <0 then
   cwal_strlen() is used to calculate its length. propertyFlags may be
   0 or a mask of CWAL_VAR_F_xxx flags.

   Returns 0 on success or a CWAL_RC_xxx code on error.
 */
int s2_install_value( s2_engine *se, cwal_value * tgt,
                      cwal_value * v,
                      char const * name,
                      int nameLen,
                      uint16_t propertyFlags );

/**
   A (somewhat) convenience form of s2_install_value() which passes
   the (callback, state, stateDtor, stateTypeID) flags to
   cwal_new_function() and installs the resulting function int the tgt
   container value (or the current scope if tgt is NULL).

   If nameLen is <0 then cwal_strlen() is used to calculate its
   length. propertyFlags may be 0 or a mask of CWAL_VAR_F_xxx flags.

   Returns 0 on success or a CWAL_RC_xxx code on error.
 */
int s2_install_callback( s2_engine *se, cwal_value * tgt,
                         cwal_callback_f callback,
                         char const * name,
                         int nameLen, 
                         uint16_t propertyFlags,
                         void * state,
                         cwal_finalizer_f stateDtor,
                         void const * stateTypeID );

/**
   A utility class for creating s2 script-side enums from C.

   When declaring/allocating these, be sure to ensure a sane default
   state by copying the global s2_enum_builder_empty object. e.g.

   ```
   s2_enum_builder eb = s2_enum_builder_empty;
   ```

   @see s2_enum_builder_init()
   @see s2_enum_builder_append()
   @see s2_enum_builder_seal()
   @see s2_enum_builder_cleanup()
*/
struct s2_enum_builder {
  /**
     The owning/managing s2_engine instance.
  */
  s2_engine * se;
  /**
     Internal flags.
  */
  cwal_flags16_t flags;
  /**
     Current entry count. This only counts the "primary" mappings
     (entry name to entry value), not the reverse mappings (value to
     name).
  */
  cwal_size_t entryCount;
  /**
     The underlying storage for the enum entries. As of 2020-02-21,
     this is a hash (it was previously an object or hash, depending on
     how many entries it held, but that proved to be more trouble than
     it was worth in downstream code).

     The state of this value is in flux until s2_enum_builder_seal()
     is used to "seal" it.
  */
  cwal_value * entries;
};
/** Convenience typedef. */
typedef struct s2_enum_builder s2_enum_builder;

/**
   An initialized-with-defaults s2_enum_builder instance, intended to
   be copied from when initializing instances on the stack.
*/
extern const s2_enum_builder s2_enum_builder_empty;

/**
   Initializes an s2_enum_builder instance, which MUST have been
   initially initialized via copy-construction from
   s2_enum_builder_empty (or results are undefined).

   entryCountHint is a hint to the system about how many entries are
   to be expected. This allows it to size its storage appropriately.
   If passed 0, it makes a conservative estimate. This routine chooses
   a prime-number hash table size based on the hint, so the hint
   itself need not be prime. (In any case, the storage may be resized
   when s2_enum_builder_seal() is used to finalize the construction
   process.)

   If typeName is not NULL and has a non-0 length then it will be used
   as the generated enum's typename value. If not NULL, typeName MUST
   be NUL-terminated.

   If this function returns 0, the s2_enum_builder instance must
   eventually be passed to s2_enum_builder_seal() or
   s2_enum_builder_cleanup() to free any resources it owns.

   Returns 0 on success and CWAL_RC_OOM if any allocation fails. On
   error, eb gets cleaned up if needed.

   A reference is added to eb->entries, and it is made vacuum-proof,
   until eb is cleaned up or sealed. HOWEVER...

   ACHTUNG:

   1) The builder class is intended primarily to be used during setup
   of a module or "atomically", e.g. in a single call to a
   cwal_callback_f() binding. Specifically, it is not intended to be
   run concurrently with script code due to...

   2) Lifetime considerations: during/after initialization, the
   under-construction enum value is initially managed by the cwal
   scope which is active at the time the enum is initialized (via this
   function). If that scope is popped before the enum is sealed or
   cleaned up, the enum's underlying storage (eb->entries) will be
   destroyed during the finalization of that scope and will leave
   eb->entries pointing to a stale pointer. cwal's recycling
   mechanisms may re-use that pointer soon afterwards, thus
   eb->entries could point to repurposed memory. i.e. Undefined
   Behaviour. Short version: create and seal your enum in the lifetime
   of the cwal/s2 scope it is initialized in unless you want to manage
   eb->entries' scope manually (tip: you _don't_ want to do that).

   3) 2020-02-21: the order of the last two arguments was swapped
   solely to force client-side breakage, rather than having the change
   of semantics for the entryCountHint parameter go unnoticed.

   @see s2_enum_builder_cleanup()
   @see s2_enum_builder_append()
   @see s2_enum_builder_seal()
*/
int s2_enum_builder_init( s2_engine * se, s2_enum_builder * eb,
                          char const * typeName,
                          cwal_size_t entryCountHint );

/**
   Every s2_enum_builder which gets passed to s2_enum_builder_init()
   must eventually be passed to this routine to free any memory owned
   by the builder. If the caller needs to keep eb->entries alive after
   this call, they must take a reference to it (see
   s2_enum_builder_seal()) and they may need to manually rescope it (a
   topic beyond the scope (as it were) of this function's
   documentation).

   It is a harmless no-op to call this multiple times on the same
   instance and it can safely be called after s2_enum_builder_seal()
   (which cleans up the enum in certain circumstances).

   @see s2_enum_builder_seal()
   @see s2_enum_builder_append()
   @see s2_enum_builder_init()
*/
void s2_enum_builder_cleanup( s2_enum_builder * eb );

/**
   Appends a new entry to an under-construction enum.

   eb must have been initialized using s2_enum_builder_init() and must
   not yet have been sealed via s2_enum_builder_seal().

   entryName is the NUL-terminated name of the enum entry. val is the
   optional value associated with the entry (it may be NULL).

   Returns 0 on success, CWAL_RC_OOM if any allocation fails,
   CWAL_RC_MISUSE if entryName is NULL or eb does not seem to be
   properly initialized or has already been sealed (see
   s2_enum_builder_seal()). On CWAL_RC_OOM, eb must be considered to
   be in an undefined state and must not be used further except to
   pass it to s2_enum_builder_cleanup().

   @see s2_enum_builder_seal()
   @see s2_enum_builder_cleanup()
   @see s2_enum_builder_init()
*/
int s2_enum_builder_append( s2_enum_builder * eb, char const * entryName,
                            cwal_value * val);

/**
   Works just like s2_enum_builder_append() but takes its key as a
   cwal_value rather than a c-string.
 */
int s2_enum_builder_append_v( s2_enum_builder * eb,
                              cwal_value * key,
                              cwal_value * wrappedVal );

/**
   "Seals" the given under-construction enum, such that it can no
   more entries can be added to it.

   The caller "should" pass a non-NULL rv value, in which case:

   1) *rv is assigned to the newly-created enum value. It has an
   initial refcount of 0, so the client needs to reference/unreference
   it (or equivalent).

   2) eb is cleaned up (see s2_enum_builder_cleanup()) to avoid
   further (mis)use. Despite this, it is safe to pass it to
   s2_enum_builder_cleanup(), so no special-case code branches
   are needed to accommodate this case.

   If rv is NULL then eb does not get cleaned up and the caller
   must eventually:

   1) Take a reference to eb->entries, if needed.

   2) Pass eb to s2_enum_builder_cleanup(). This will destroy eb->entries
   unless the caller has acquired a reference to it.

   The new enum would typically be stored in a cwal scope to keep it
   it alive, and it may be destroyed if it is not stored in a
   container or scope before its own managing scope is destroyed. (Its
   initial managing scope is the one which is active when the
   s2_enum_builder_init() is called.)

   Returns 0 on success. On error, CWAL_RC_RANGE if no entries have
   been added to the enum (via s2_enum_builder_append()), CWAL_RC_OOM
   on allocation error, and CWAL_RC_MISUSE if eb has not been properly
   initialized or has already been sealed. On error *rv is not
   modified. On error eb must be considered to be in an undefined
   state and must not be used further except to pass it to
   s2_enum_builder_cleanup().

   @see s2_enum_builder_append()
   @see s2_enum_builder_cleanup()
   @see s2_enum_builder_init()   
*/
int s2_enum_builder_seal( s2_enum_builder * eb, cwal_value **rv );

/**
   Toggles the S2_VAL_F_DOT_LIKE_OBJECT flag on the given value,
   which is assumed to be a hash table. The second parameter determines
   whether the flag is toggled on or off.
*/
void s2_hash_dot_like_object( cwal_value * hash, int dotLikeObj );

/**
   Returns true (non-0) if key's string value is the string "value".
*/
char s2_value_is_value_string( s2_engine const * se, cwal_value const * key );

/**
   Returns true (non-0) if key's string value is the string "prototype".
*/
char s2_value_is_prototype_string( s2_engine const * se, cwal_value const * key );


/**
   ACHTUNG: only lightly tested and known to break with certain
   script constructs.

   "Minifies" s2 source code in the given source, appending it to the
   given destination. This strips out all "junk" tokens and most
   newlines, as well as runs of contiguous spaces/tabs.

   This does not do any semantic analysis on the input - it only
   tokenizes the input. Unknown token types, unmatched
   braces/parens/quotes, etc. will cause it to fail.

   Returns 0 on success.

   There's not yet a guaranty that minified code can be eval'd... it's
   a learning process.

   src and dest _must_, on error, return a non-0 code from the
   CWAL_RC_xxx family of codes, e.g. CWAL_RC_IO might be appropriate
   (falling back to CWAL_RC_ERROR if there's no better match).
*/
int s2_minify_script( s2_engine * se, cwal_input_f src, void * srcState,
                      cwal_output_f dest, void * destState );
/**
   Equivalent to s2_minify_script(), using src as the input and dest
   as the output destination. src must not be dest. dest gets appended
   to, so be sure to cwal_buffer_reset() it, if needed, when looping
   over a single output buffer.
*/
int s2_minify_script_buffer( s2_engine * se, cwal_buffer const * src,
                             cwal_buffer * dest );

/**
   A cwal_callback_f() implementation binding s2_minify_script(). It
   expects either 1 or 2 arguments: (srcStringOrBuffer) or
   (srcStringOrBuffer, destBuffer). It returns, via *rv, its second
   argument or a new Buffer instance.
*/
int s2_cb_minify_script( cwal_callback_args const * args, cwal_value ** rv );

/**
   A convenience routine to bind a callback function as a constructor
   for use with s2's "new" keyword.
   
   Installs method as a hidden/const property named "__new" in the
   given container. The method parameter is assumed to conform to the
   "new" keyword's constructor conventions.

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
   f = cwal_new_function( se->e, my_callback_f, my_callback_state,
                          my_callback_state_finalizer_f, my_type_id );
   if(!f) return CWAL_RC_OOM;
   fv = cwal_function_value(fv);
   assert(f == cwal_value_get_function(fv)); // will always be the case if f is valid
   cwal_value_ref(fv);
   rc = s2_ctor_method_set( se, myContainer, f );
   cwal_value_unref(fv);
   return rc;
   ```

   In such a setup, from inside the my_callback() implementation,
   cwal_args_state(args->engine, my_type_id) can be used to
   (type-safely) fetch the my_callback_state pointer. The s2_engine
   instance associated with the call can be fetched via
   s2_engine_from_args(args).

   @see s2_ctor_callback_set()
*/
int s2_ctor_method_set( s2_engine * se, cwal_value * container, cwal_function * method );

/**
   A convenience form of s2_ctor_method_set() which instantiates a
   cwal_function from the its 3rd argument using cwal_new_function().
   Returns CWAL_RC_MISUSE if any argument is NULL, else returns as for
   s2_ctor_method_set().
*/
int s2_ctor_callback_set( s2_engine * se, cwal_value * container, cwal_callback_f method );

/**
   Invokes a "constructor" function in the same manner as the 'new' keyword
   does. If ctor is NULL then s2 looks in operand (which must be a container
   type) for a property named "__new". If that property is not found or
   is not a Function, se's error state is set and non-0 is returned.

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
   does with it).

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
   "new" keyword needs). We "could" expose that array directly from a
   cwal public API but we don't because manipulating it from client
   code could be disastrous.
*/
int s2_ctor_apply( s2_engine * se, cwal_value * operand,
                   cwal_function * ctor, cwal_array * args,
                   cwal_value **rv );

/**
   Returns true (non-0) if v is legal for use as an operand do the
   "new" keyword. This only returns true if v is a container which
   holds (not counting properties inherited from prototypes) a key
   named "__new" with a value which is-a/has-a Function.
*/
char s2_value_is_newable( s2_engine * se, cwal_value * v );

/**
   Returns true (non-0) if the given value is currently being
   initialized as the "this" value of a constructor function via s2's
   "new" keyword. e.g. it can be used in the context of a C-native
   constructor to figure out if the function was used in a constructor
   context or not (if that makes any difference to how it functions).
*/
char s2_value_is_newing(cwal_value const * v);

/**
   Returns true if this function believes that mem (which must be at
   least len bytes of valid memory long) appears to have been
   compressed by s2_buffer_compress() or equivalent. This is not a
   100% reliable check - it could potentially have false positives on
   certain inputs, but that is thought to be unlikely (at least for
   text data). A false positive has never been witnessed in many
   thousands of tests in the libfossil tree. Knock on wood.

   Returns 0 if mem is NULL.

   @see s2_buffer_compress()
   @see s2_buffer_uncompress()
   @see s2_uncompressed_size()
*/
char s2_is_compressed(unsigned char const * mem, cwal_size_t len);

/**
   Equivalent to s2_is_compressed(buf->mem, buf->used), except that
   it returns 0 if !buf.

   @see s2_buffer_compress()
   @see s2_buffer_uncompress()
*/
char s2_buffer_is_compressed(cwal_buffer const *buf);

/**
   If s2_is_compressed(mem,len) returns true then this function
   returns the uncompressed size of the data, else it returns
   (uint32_t)-1. (Remember that an uncompressed size of 0 is legal!)
*/
uint32_t s2_uncompressed_size(unsigned char const *mem,
                              cwal_size_t len);

/**
   The cwal_buffer counterpart of s2_uncompressed_size().
*/
uint32_t s2_buffer_uncompressed_size(cwal_buffer const * b);

/**
   If built without zlib or miniz support, this function always
   returns CWAL_RC_UNSUPPORTED without side-effects.

   Compresses the first pIn->used bytes of pIn to pOut. It is ok for
   pIn and pOut to be the same blob.

   pOut must either be the same as pIn or else a properly
   initialized buffer. Any prior contents will be freed or their
   memory reused.

   Results are undefined if any argument is NULL.

   Returns 0 on success, CWAL_RC_OOM on allocation error, and
   CWAL_RC_ERROR if the lower-level compression routines fail.

   Use s2_buffer_uncompress() to uncompress the data.

   The data is encoded with a big-endian, unsigned 32-bit length as
   the first four bytes, and then the data as compressed by a
   "zlib-compatible" mechanism (which may or may not be zlib or
   miniz).

   After returning 0, pOut->used will hold the new, compressed size
   and s2_buffer_uncompressed_size() can be passed pOut to get the
   original size.

   Special cases:

   - If s2_buffer_is_compressed(pIn) and (pIn != pOut), then pIn is
   simply copied to pOut, replacing any existing content.

   - If s2_buffer_is_compressed(pIn) and (pIn == pOut), it has
   no side effects and returns 0.


   Minor achtung: the underlying compression library (which is
   specified as being "zlib-compatible", without saying it's actually
   zlib) allocates and frees memory from outside of e's memory
   management system, which means that it is not accounted for in,
   e.g. cwal-level metrics and memory capping.

   @see s2_buffer_uncompress()
   @see s2_buffer_is_compressed()
*/
int s2_buffer_compress(cwal_engine * e, cwal_buffer const *pIn,
                       cwal_buffer *pOut);

/**
   If built without zlib or miniz support, this function always
   returns CWAL_RC_UNSUPPORTED without side-effects.

   Uncompresses buffer pIn and stores the result in pOut. It is ok for
   pIn and pOut to be the same buffer, in which case the old contents
   will, on success, be destroyed. Returns 0 on success. On error pOut
   is not modified (whether or not pIn==pOut).

   pOut must be either cleanly initialized/empty or the same as pIn.

   Results are undefined if any argument is NULL or its memory is
   invalid.

   If (pIn == pOut) and !s2_buffer_is_compressed(pIn) then this
   function returns 0 without side-effects.

   Returns 0 on success, CWAL_RC_OOM on allocation error, and
   CWAL_RC_ERROR if the lower-level decompression routines fail.

   @see s2_buffer_compress()
   @see s2_buffer_compress2()
   @see s2_buffer_is_compressed()
*/
int s2_buffer_uncompress(cwal_engine * e, cwal_buffer const *pIn,
                         cwal_buffer *pOut);


/**
   Swaps left/right's contents. It retains (does not swap) the
   left->self/right->self pointers (swapping those _will_ corrupt
   their memory at some point). Results are undefined if (left==right)
   or if either argument is NULL or points to invalid memory.
*/
void s2_buffer_swap( cwal_buffer * left, cwal_buffer * right );

/**
    (Mostly) internal debugging tool which dumps out info about v (may
    be NULL), with an optional descriptive message. Expects file, func and
    line to be the __FILE__, __func__ resp.  __LINE__ macros. Use the
    s2_dump_val() macro to simplify that.

    Reminder: v cannot be const b/c some types go through JSON output,
    which requires non-const so that it can catch cycles.
*/
void s2_dump_value( cwal_value * v, char const * msg, char const * file,
                    char const * func, int line );

/**
   Equivalent to s2_dump_value(V, MSG, __func__, __LINE__).
*/
#define s2_dump_val(V, MSG) s2_dump_value((V), (MSG), __FILE__, __func__, __LINE__)

/**
   Prints out a cwal_printf()-style message to stderr and exit()s
   the application.  Does not return. Intended for use in place of
   assert() in test code. The library does not use this internally.
*/
void s2_fatal( int code, char const * fmt, ... );

/**
   Exists only to avoid adding an s2-internal-API dep in s2sh.

   Returns sizeof(s2_func_state).
*/
unsigned int s2_sizeof_script_func_state(void);


/**
   Removes the specially-propopagating "return" value (if any)
   from its special propagation mode, effectively transferring
   to the caller (with all the usual caveats about refcounts,
   unknowable ownership, etc.) See cwal_propagating_take()
   for details.

   In s2, the following keywords use specially-propagating values:
   return, break, exit (, fatal???). Exceptions use a separate
   propagation slot dedicated to the currently-thrown exception.
*/
cwal_value * s2_propagating_take( s2_engine * se );

/**
   s2 proxy for cwal_propagating_get() for details.
*/
cwal_value * s2_propagating_get( s2_engine * se );

/**
   s2 proxy for cwal_propagating_set() for details.
*/
cwal_value * s2_propagating_set( s2_engine * se, cwal_value * v );


/**
   Might or might not cwal_engine_sweep() or cwal_engine_vacuum() on
   se->e, depending on the state of se's various counters and
   guards. In any case, this function may increase a counter internal
   to the s2_engine instance.

   Returns 0 on success and "should" never fail.

   In debug builds, this routine asserts for any sort of problems. In
   non-debug builds it returns CWAL_RC_MISUSE if se has no current
   scope. It's conceivable that cwal_engine_vacuum() fails (returns
   non-0), but that can only happens if it detects memory corruption
   caused by mismanagement of Values, in which case an assert() is is
   triggered in debug builds. That said, a "real" failure of vacuum,
   at a time where a vacuum should be legal, has never been witnessed,
   so handling of a non-0 result code is largely a hypothetical
   problem. Feel free to ingore the result code.
*/
int s2_engine_sweep( s2_engine * se );


/**
   Intended to be used (if at all, then) for storing prototype
   values long-term in an s2_engine.

   Stores the given value in se's stash using the given name as a
   suffix for some larger unique key reserved for the various base
   prototypes. This implicitly moves proto into the global scope (for
   lifetime purpose) and makes it vacuum-proof.

   This "really" should only to be used by the various 
   s2_prototype_xxx() functions, but libfossil currently also makes 
   use of it, so it can't currently be made an internal API.
*/
int s2_prototype_stash( s2_engine * se, char const * typeName,
                        cwal_value * proto );

/**
   If the given name string was used to stash a prototype with
   s2_prototype_stash(), this function returns that value, else it
   returns 0. Ownership of the returned value is not modified.
*/
cwal_value * s2_prototype_stashed( s2_engine * se, char const * typeName );

/**
	Sets the __typename property on the given container to the given 
	value, with the (CWAL_VAR_F_CONST | CWAL_VAR_F_HIDDEN) flags set.
	This is more efficient than directly setting that property because
	s2_engine caches the __typename key.
*/
int s2_typename_set_v( s2_engine * se, cwal_value * container, cwal_value * name );

/**
   The C-string counterpart of s2_typename_set_v(), differing
   only in that it takes the type name in the form
   of the first nameLen bytes of name.
*/
int s2_typename_set( s2_engine * se, cwal_value * container,
                     char const * name, cwal_size_t nameLen );


/**
	If a byte-exact match of needle is found in haystack, its offset in
	the haystack is returned, else 0 is returned.
*/
char const * s2_strstr( char const * haystack, cwal_size_t hayLen,
                        char const * needle, cwal_size_t needleLen );


/**
   Evaluates the given script code in a new scope and stores the
   result of that script in the given container, using the given
   property name (which must be propNameLen bytes long). This is
   intended to simplify installation of small/embedded
   script-implemented functions from C code.

   If srcLen is negative, cwal_strlen() is used to calculate
   src's length.

   If the script triggers a 'return' then this routine captures
   that return'd value as the result.

   Returns 0 on success, a CWAL_RC_xxx or S2_RC_xxx value on error,
   and can be anything script evaluation could return.
*/
int s2_set_from_script( s2_engine * se, char const * src,
                        int srcLen, cwal_value * addResultTo,
                        char const * propName,
                        cwal_size_t propNameLen );

/**
   Works identically to s2_set_from_script() except that it takes
   its property name as a cwal_value.
*/
int s2_set_from_script_v( s2_engine * se, char const * src,
                          int srcLen, cwal_value * addResultTo,
                          cwal_value * propName );

/**
   A proxy for getcwd(2) which appends the current working directory's
   name to the end of the given buffer. This function expands buffer
   by some relatively large amount to be able to handle long
   paths. Because of this, it's recommended that a shared/recycled
   buffer be used to handle calls to this function
   (e.g. s2_engine::buffer).

   On success, tgt gets appended to, updating tgt's members as
   appropriate, and 0 is returned. On error, non-0 is returned (the
   exact code may depend on the errno set by getcwd(2)).

   ACHTUNG: this function is only implemented for Unix systems. On
   Windows builds it returns CWAL_RC_UNSUPPORTED because i don't have
   a Windows system to implement this on.
*/
int s2_getcwd( cwal_engine * e, cwal_buffer * tgt );

/**
   A cwal_callback_f() implementation wrapping s2_getcwd(). On
   success, it returns (via *rv) the current working directory's name
   as a string. If script passed an argument from script code, it is
   interpreted as a boolean: if true, the directory separator is
   appended to the result, else it is not. The default is not to
   append the directory separator to the result.
*/
int s2_cb_getcwd( cwal_callback_args const * args, cwal_value ** rv );

/**
   Works like mkdir(2). Returns 0 on success, else a CWAL_RC_xxx value
   approximating the underlying errno result. If errMsg is not NULL
   then on error, *errMsg will point to an error message string owned
   by the C library. Its contents may be modified by calls to
   strerror(3), so must be copied if it should be retained.

   LIMITATIONS:

   1) Returns CWAL_RC_UNSUPPORTED on builds which don't have mkdir(2).

   2) Does not create intermediate directories, so the parent dir
   of the new directory must already exist. See s2_mkdir_p().

   3) The message string returned by strerror() may be modified by
   calls to that function from other threads, so the errMsg argument
   is only known to be useful for single-threaded clients.

   @see s2_mkdir_p()
*/
int s2_mkdir( char const * name, int mode, char const ** errMsg );

/**
   A convenience wrapper around s2_mkdir() which creates parent
   directories, if needed, for the target directory name. e.g.  if
   passed "a/b/c" and "a" and/or "b" do not exist, they are created
   before creating "c". Fails if creation of any part of the path
   fails.

   It requires a well-formed Unix-style directory name (relative or
   absolute). Returns 0 on success, else non-0 and an error message,
   as documented for s2_mkdir().

   @see s2_mkdir()
*/
int s2_mkdir_p( char const * name, int mode, char const ** errMsg );

/**
   A cwal_callback_f() implementation wrapping s2_mkdir(). It mkdir() is
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

   Throws a CWAL_RC_ACCESS exception if s2_disable_check() for the
   flag S2_DISABLE_FS_WRITE | S2_DISABLE_FS_STAT fails.

   Throws on error. On success, returns the undefined value.
*/
int s2_cb_mkdir( cwal_callback_args const * args, cwal_value ** rv );

/**
   File/directory types for use with s2_fstat_t.
*/
enum s2_fstat_types {
S2_FSTAT_TYPE_UNKNOWN = 0,
S2_FSTAT_TYPE_REGULAR = 0x01,
S2_FSTAT_TYPE_DIR = 0x02,
S2_FSTAT_TYPE_LINK = 0x04,
S2_FSTAT_TYPE_BLOCK = 0x08,
S2_FSTAT_TYPE_CHAR = 0x10,
S2_FSTAT_TYPE_FIFO = 0x20,
S2_FSTAT_TYPE_SOCKET = 0x40
};
/**
   Filesystem entry info for use with s2_fstat().
*/
struct s2_fstat_t {
  /**
     The type of a given entry.
  */
  enum s2_fstat_types type;
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
typedef struct s2_fstat_t s2_fstat_t;
/**
   Intended to be used as a copy-construction source for local s2_fstat_t copies,
   to ensure a clean state.
 */
extern const s2_fstat_t s2_fstat_t_empty;
/**
   Cleanly-initialized s2_fstat_t entry, intended for const copy
   initialization.
*/
#define s2_fstat_t_empty_m {0,0,0,0,0}

/**
   Wrapper for stat(2) and lstat(2). The filename is taken from the
   first fnLen bytes of the given filename string. (We take the length
   primarily because cwal X-strings and Z-strings need not be
   NUL-terminated.) If tgt is not NULL then the results of the stat
   are written there. If derefSymlinks is 1 then stat() is used, else
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

   Note that this function does not (cannot) honor
   s2_disabled_features flags because it has no s2_engine to check
   against.
*/
int s2_fstat( char const * filename,
              cwal_size_t fnLen,
              s2_fstat_t * tgt,
              char derefSymlinks );

/**
   A cwal_callback_f() implementation wrapping s2_fstat().

   Script signatures:

   1) object stat( string filename, [derefSymlinks=true] )

   Returns an object representing the stat() info (see
   s2_fstat_to_object() for the structure. Throws an exception if
   stat() fails.

   2) bool stat(string filename, undedfined [, derefSymlinks=true])

   If the argument count is greater than 1 and the the second argument
   has the undefined value then this function returns a boolean
   instead of an object. In that case, it returns false, instead of
   throwing, if stat() fails. That's admittedly a horribly awkward way
   to distinguish between the two return modes, and it may well be
   changed at some point.
*/
int s2_cb_fstat( cwal_callback_args const * args, cwal_value ** rv );

/**
   Converts a populated s2_fstat_t struct to an Object with properties describing
   the s2_fstat_t values:

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

   Just FYI: this function caches the keys (via s2_stash_set()) used
   for the returned object's properties, so it's not quite as
   expensive as it may initially seem.
*/
int s2_fstat_to_object( s2_engine * se, s2_fstat_t const * fst, cwal_value ** rv );

/**
   A wrapper around chdir(2). The first dirLen bytes of dir are used
   as a directory name passed to chdir(2). Returns 0 on success, non-0
   (a CWAL_RC_xxx code) on error. If this build does not have chdir(2)
   then CWAL_RC_UNSUPPORTED is returned.
 */
int s2_chdir( char const * dir, cwal_size_t dirLen );

/**
   A cwal_callack_f() impl wrapping s2_chdir().

   Script signature:

   void chdir(string dirname)

   Throws on error.
*/
int s2_cb_chdir( cwal_callback_args const * args, cwal_value **rv );

/**
   cwal_callback_f() wrapper for cwal_glob_matches_cstr().

   Script-side usage:

   bool glob( string glob, string haystack [, int policy=-1] )

   Glob matching policies (3rd glob() param):

   <0 = (default) wildcard-style (case sensitive)

   0 = SQL LIKE style (case insensitive)

   >0 = SQL LIKE style (case sensitive)
*/
int s2_cb_glob_matches_str( cwal_callback_args const * args,
                            cwal_value ** rv );


/**
   A cwal_callback_f() implementation which tokenizes its input into
   an array of s11n tokens, but supporting only a subset of its core
   token types: numbers, strings (literals, heredocs, and, for
   historical reasons, {blocks}), and the built-in constants
   bool/null/undefined values.

   Its intended purpose is to tokenize single lines of "commands" for
   use in command-based dispatching in scripts.

   Script usage:

   array tokenizeLine(string input)

   e.g.:

   tokenizeLine('1 2.3 "hi there" true null')

   would result in an array: [1, 2.3, 'hi there', true, null]

   This function does not parse higher-level constructs like objects,
   arrays, or functions, and cannot do so without significant
   reworking of the evaluation engine (where the logic for such
   constructs is embedded).

   For historical reasons, {blocks} are internally strings, which
   means that {a b c} will be tokenized as the string 'a b c', except
   that: 1) it will not get unescaped like a string literal and 2) its
   leading/trailing spaces will be removed.

   If the input string is empty, it resolves to the undefined value,
   not an empty array (though that decision is up for reconsideration,
   depending on what pending experience suggests).
*/
int s2_cb_tokenize_line(cwal_callback_args const * args, cwal_value ** rv);

/**
   Works just like the C-standard fopen() except that if zName is "-"
   then it returns either stdin or stdout, depending on whether zMode
   contains a "w", "a", or "+" (meaning stdout). Neither argument may
   be NULL.

   Returns a newly-opened file handle on success, else NULL. The
   handle should eventually be passed to s2_fclose() to close it.  It
   may optinoally be passed to fclose() instead, but that routine will
   unconditionally close the handle, whereas this one is a no-op for
   the standard streams.

   @see s2_fclose()
*/
FILE *s2_fopen(const char *zName, const char *zMode);

/**
   If the given FILE handle is one of (stdin, stdout, stderr), this is
   a no-op, else it passes the file handle to fclose().

   @see s2_fopen()
*/
void s2_fclose(FILE *);

/**
   Reads the given file (synchronously) until EOF and streams its
   contents (in chunks of an unspecified size) to cwal_output() using
   the given cwal_engine. Returns 0 on success, non-0 (likely
   CWAL_RC_IO) on error. Does not modify ownership of the passed-in
   pointers.
*/
int s2_passthrough_FILE( cwal_engine * e, FILE * file );

/**
   A convenience wrapper around s2_passthrough_FILE() which uses
   s2_fopen() to open the given filename and (on success) stream that
   file's contents to cwal_output(). Returns CWAL_RC_IO if the fopen()
   fails, else returns as documented for s2_passthrough_FILE().
*/
int s2_passthrough_filename( cwal_engine * e, char const * filename );


/**
   This odd function is intended to assist in certain types of
   bindings which want to store, e.g. a custom Prototype somewhere
   other than in s2_prototype_stash(). This function creates a new key
   using cwal_new_unique() and uses it store 'what' in 'where'
   with the CWAL_VAR_F_HIDDEN and CWAL_VAR_F_CONST flags. This binding
   will ensure that 'what' gets referenced and rescoped as necessary
   to keep it alive along with 'where'. One caveat: if
   cwal_props_clear(where) is called, it will delete that binding,
   possibly leading to unexpected behaviour in other places which are
   expecting 'what' to be alive.

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
   not get removed. Several of the modules in the s2 source tree
   demonstrate this function's use.

   Reminder to self: it might be interesting to add a new
   CWAL_VAR_F_xxx flag which tells even cwal_props_clear() not to
   remove the property.  That would be somewhat more invasive and
   special-case than i'm willing to hack right now, though. :/
*/
int s2_stash_hidden_member( cwal_value * where, cwal_value * what );

/**
   This functions provides cwal_callback_f() implementations the
   possibility of cleanly exiting the current script, as if the "exit"
   keyword had been triggered. Its result is intended to be returned
   from such a callback.

   It uses the given value as the script's exit result. The value may
   be NULL, which is interpreted as cwal_value_undefined().

   This function replaces any pending propagating result (from any
   in-progress return/break/continue/exit) with the given result
   value. Be aware that if you need the old propagating value then you
   must, before calling this, s2_propagating_take() it or
   s2_propagating_get() it and cwal_value_ref() it.

   This function always returns CWAL_RC_EXIT, which gets interpreted
   by the eval engine like the 'exit' keyword. (Note that simply
   returning CWAL_RC_EXIT from a callback will fail in other ways,
   possibly triggering an assert() in s2, because setting up the
   'exit' involves more than just this result code.)

   Design note: this function takes a cwal_callack_args instead of a
   cwal_engine (or s2_engine) only to emphasize that it's only
   intended to be called from cwal_callback_f() implementations. (It
   also relies on s2's specific interpretation of the CWAL_RC_EXIT
   result code.)

   Example:

   ```
   int my_callback(cwal_callback_args const * args, cwal_value ** ){
     ... do something ...;
     return s2_trigger_exit(args, 0);
   }
   ```
*/
int s2_trigger_exit( cwal_callback_args const * args, cwal_value * result );

/**
   If passed a container-type value, it either "seals" or "unseals"
   the value by setting resp. unsetting the
   CWAL_CONTAINER_DISALLOW_PROP_SET and
   CWAL_CONTAINER_DISALLOW_PROTOTYPE_SET flags on it. When set, the
   engine will refuse to set properties or change prototypes on the
   object, and will error out with CWAL_RC_DISALLOW_PROP_SET
   resp. CWAL_RC_DISALLOW_PROTOTYPE_SET if an attempt is made. This
   only affects future property/prototype operations, not existing
   values.

   If passed a truthy 2nd argument, the container is sealed, else it
   is unsealed. Note that unsealing is considered to be a rare
   corner-case, and is not intended to be used willy-nilly.

   Returns 0 on succes, CWAL_RC_MISUSE if v is NULL, and CWAL_RC_TYPE
   if the value is not a container.

   The "seal" restrictions do not currently (20191210) apply to
   array/tuple indexes, but probably should (noting that tuples
   currently have no flags which would allow us to mark them as
   sealed). That behaviour may change in the future.
*/
int s2_seal_container( cwal_value * v, char sealIt );

/**
   A script-bindable callback which expects to be passed one or more
   container-type values. For each one it calls s2_seal_container(),
   passing true as the 2nd argument (so it supports sealing, but not
   unsealing, as unsealing would allow script code to do unsightly
   things with enums and other sealed containers).

   This callback throws if passed no arguments or if any argument is
   not a container type.

   On success it returns, via *rv, the last container it modifies.
*/
int s2_cb_seal_object( cwal_callback_args const * args, cwal_value ** rv );

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
int s2_cb_tokenize_path( cwal_callback_args const * args, cwal_value **rv );

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
   int rc = s2_tokenize_path_to_array(e, &ar, "foo;bar", -1);
   ```

   On success, ar will be non-NULL and owned by that code.
   Contrast with:

   ```
   int rc;
   cwal_array * ar = cwal_new_array(e);
   if(!ar) return CWAL_RC_OOM;
   rc = s2_tokenize_path_to_array(e, &ar, "foo;bar", -1);
   ```

   In that case, any entries in the given path string will be appended
   to the array provided by the caller.


   @see s2_path_toker
   @see s2_path_toker_init()
 */
int s2_tokenize_path_to_array( cwal_engine * e, cwal_array ** tgt,
                               char const * path,
                               cwal_int_t pathLen );

/**
   Never, ever use this unless you are testing s2's internals and
   happen to know that it's potentially useful.
*/
int s2_cb_internal_experiment( cwal_callback_args const * args, cwal_value **rv );

/**
   Returns the path to s2's "home directory," or NULL if it is
   unknown.

   Currently, the home directory is specified via the S2_HOME
   environment variable. If that environment variable is set, this
   function returns its value in memory which will live as long as the
   current app instance. If that variable is not set, NULL is
   returned.

   If non-NULL is returned and len is not NULL, *len is assigned the
   length of the string, in bytes.

   In the future there might or might not be other ways to specify the
   s2 home, perhaps on a per-engine-instance basis (and thus the
   s2_engine parameter for this function, though it is currently
   unused).
*/
char const * s2_home_get(s2_engine * se, cwal_size_t * len);

/**
   Installs a user-defined pseudo-keyword into the given engine,
   granting fast acess to the value and a lifetime as long as the
   engine is running.

   The given string must be a legal s2 identifier with a length equal
   to the 3rd argument. Its bytes are copied, so it need not have a
   well-defined lifetime. If the given length is negative,
   cwal_strlen() is used to calculate the key's length.

   "User Keywords," or UKWDs, as they're colloquially known, use s2's
   keyword infrastructure for their lookups, meaning that they bypass
   all scope-level lookups and have an amortized faster search time
   than var/const symbols. The determination of whether a given symbol
   is a UKWD an average O(log n) operation (n=number of UKWDs), and
   then finding the associated value for that requires a hashtable
   lookup (average O(1)).

   These are not "real" keywords, in that they cannot interact
   directly with s2's parser. They simply resolve to a single value of
   an arbitrary type which can be quickly resolved in the
   keyword-lookup phase of script evaluation.

   Returns:

   - 0 on success.

   - CWAL_RC_RANGE if name is empty.

   - CWAL_SCR_SYNTAX if name is not strictly an identifier token.

   - CWAL_RC_ACCESS if a real keyword with that name exists.

   - CWAL_RC_ALREADY_EXISTS if an entry with that name already exists

   - CWAL_RC_RANGE if the name is empty or "too long" (there is a
   near-arbitrary upper limit).

   - CWAL_RC_UNSUPPORTED if v is NULL or is the undefined value, both
   of which are reserved for *potential* "undefine" support.

   - CWAL_RC_OOM on allocation error.

   On error, se's error state is updated with an explanation of the
   problem.

   Caveats:

   1) Installing these takes up memory (more than a simple property
   mapping), so don't go crazy with 100 of them. Likewise, adding many
   keyword changes the duration of that "1" in "O(1)". Not by terribly
   much, but keyword lookups happen *all the time* internally (every
   time an identifier is seen), so it adds up.

   2) They cannot be re-set or uninstalled once they have been
   installed.
*/
int s2_define_ukwd(s2_engine * se, char const * name,
                   cwal_int_t nameLen, cwal_value * v);

/**
   A set of *advisory* flags for features which well-behaved s2 APIs
   should honor. The flag names listed for each entry are for use with
   s2_disable_set_cstr().

   This is generally only intended to apply to script-side APIs, not
   their equivalent C APIs. e.g. s2_eval_filename() does not honor
   these flags, but its script-binding counterparts,
   s2_cb_import_script() and the import keyword, do.
*/
enum s2_disabled_features {
/**
   The disable-nothing flag.

   Flag name: "none"
*/
S2_DISABLE_NONE = 0,
/**
   Disables stat()'ing of files, as well as checking/changing the
   current directory, in compliant APIs.

   Flag name: "fs-stat"
*/
S2_DISABLE_FS_STAT = 1,

/**
   Disables opening/reading of files in compliant APIs.

   Note that APIs must also normally (but not always) OR this with
   S2_DISABLE_FS_STAT, else a file may be opened for reading without
   stat()'ing it first (i.e. without checking whether it exists).

   Flag name: "fs-read"

   Reminder to self: we may want a separate flag which indicates that
   JSON files are an exception to this limitation. JSON is known to
   not allow execution of foreign code nor loading of non-JSON
   content, so it's "safer" that generic filesystem-read access.
*/
S2_DISABLE_FS_READ = 1 << 1,

/**
   Disables opening/writing and creation of new files in compliant
   APIs.

   Flag name: "fs-write"
*/
S2_DISABLE_FS_WRITE = 1 << 2,

/**
   Flag name: "fs-io"
*/
S2_DISABLE_FS_IO = S2_DISABLE_FS_READ | S2_DISABLE_FS_WRITE,

/**
   Flag name: "fs-all"
*/
S2_DISABLE_FS_ALL = S2_DISABLE_FS_IO | S2_DISABLE_FS_STAT
};

/**
   Assigns the set of API-disabling flags on se.

   @see s2_disable_get()
   @see s2_disable_check()
*/
void s2_disable_set( s2_engine * se, cwal_flags32_t flags );

/**
   A variant of s2_disable_set() which accepts a
   comma-and/or-space-separated list of flag names to set as its 2nd
   argument. The 3rd argument specifies the length of the 2nd, in
   bytes. If the 3rd argument is negative, cwal_strlen() is used to
   determine the 2nd argument's length. A length of 0 causes the flags
   to be set to 0.

   On success, 0 is returned and if result is not NULL, the result of
   the flags is assigned to *result, as well as being applied to se.

   On error, non-0 is returned and neither se nor *result are modified
   other than to set se's error state. An unknown flag is treated as
   an error.

   The flag names are listed in the docs for the s2_disabled_features
   enum entries. Each entry gets OR'd to the result, with one
   exception: the "none" flag resets the flags to 0.
*/
int s2_disable_set_cstr( s2_engine * se, char const * str, cwal_int_t strLen,
                         cwal_flags32_t * result );

/**
   Returns the set of API-disabling flags from se

   @see s2_disable_set()
   @see s2_disable_check()
*/
cwal_flags32_t s2_disable_get( s2_engine const * se );

/**
   If any of se's advisory feature-disable flags match the given
   flags, se's error state is set and non-0 is returned, else se is
   not modified and 0 is returned. If this returns non-0, its
   state can be converted to an exception with s2_throw_err(),
   passing it 0 for all arguments after the first.

   @see s2_disable_set()
   @see s2_disable_get()
   @see s2_disable_check_throw()
   @see s2_cb_disable_check()
*/
int s2_disable_check( s2_engine * se, cwal_flags32_t f );

/**
   Equivalent to s2_disable_check(), but transforms any error state
   to an exception.
*/
int s2_disable_check_throw( s2_engine * se, cwal_flags32_t f );

/**
   Convenience form of s2_disable_check_throw(), intended for use in
   cwal_callback_f() implementations, which checks the given flags
   against the s2_engine returned by s2_engine_from_args().
*/
int s2_cb_disable_check( cwal_callback_args const * args, cwal_flags32_t f );

/**
   "Should" be called ONE TIME from the application's main() in order
   to perform certain static initialization, e.g. install a
   cwal_rc_cstr_f fallback for s2-related result codes.

   If it is not called beforehand, it will be called the first time an
   s2_engine instance is initialized, but that involves a very narrow
   window for a mostly-harmless race condition.
*/
void s2_static_init(void);

/**
   S2_TRY_INTERCEPTORS: _HIGHLY EXPERIMENTAL_ property interceptor
   support. DO NOT USE THIS. It's not known to work, and will very
   likely be removed because of its performance implications.
*/
#define S2_TRY_INTERCEPTORS 0

#if S2_TRY_INTERCEPTORS
cwal_function * s2_value_is_interceptor( cwal_value const * v );
#endif

#if S2_TRY_INTERCEPTORS
/**
   _HIGHLY EXPERIMENTAL_. Do not use. While these basically work, they
   are not going to be enabled because the performance hit is too high
   for non-interceptor cases (i.e. the vast majority).
*/
int s2_cb_add_interceptor( cwal_callback_args const * args, cwal_value **rv );
#endif

#ifdef __cplusplus
}/*extern "C"*/
#endif

#endif
/* include guard */
