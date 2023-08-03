/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#if !defined (NET_WANDERINGHORSE_CWAL_WHCL_INTERNAL_H)
#define NET_WANDERINGHORSE_CWAL_WHCL_INTERNAL_H

#include "libwhcl.h"
#include "script.h"
#include <stdio.h>
#include <time.h>
/**
   Cross-file interal APIs for whcl.
*/
typedef struct whcl__op whcl__op;

/** @internal
   
    Flags for use with whcl_scope::flags.
*/
enum whcl__scope_flags_e {
/**
   Indicates that this scope is part of a loop body and that the
   `break` and `continue` keywords are acceptable. Note that this
   flag needs to get propagated into constructs in the loop body
   where appropriate. e.g. to an `if` but not to a `proc` call.
*/
WHCL__SCOPE_F_LOOP = 0x01,
/**
   EXPERIMENTAL. SUBJECT TO CHANGE OR REMOVAL.

   Flag for use with WHCL__SCOPE_F_CALL which indicates that the scope
   should _not_ stop symbol lookup resolution. This is primarily of use
   when passing scope-local callback functions to native functions, as in
   this type of case:

   ```
   decl x 0
   db.query "select ... from ..." [proc {} {incr x}]
   ```

   Without this flag, the scope created for calling the passed-in proc
   cannot resolve `x`.
*/
WHCL__SCOPE_F_XSYM = 0x02,
/**
   Flag marking a function-call scope. This is primarily used to
   indicate that symbol resolution lookups stop at this scope.
*/
WHCL__SCOPE_F_CALL = 0x10,
/**
   Used internally to convey from whcl_function_call() to
   cwal_callback_f_whcl() that the latter should reuse the caller's
   whcl_scope.
*/
WHCL__SCOPE_F_REUSE = 0x20,

/**
   Marks the scope as being a boundary in the `with` chain.
 */
WHCL__SCOPE_F_WITH = 0x40,

/**
   Mask of flags which get copied via whcl__scope_push2().
*/
WHCL__SCOPE_F_PUSH2 = 0x0F

};

/**
   Flags for use with cwal_container_client_flags_set()
   and friends.
 */
enum whcl__container_flags_e {
/**
   This client container flag gets set on values created by the 'new'
   keyword, but only for the duration of the constructor call. That
   provides the basis of the `info is-newing` command.
*/
WHCL__VAL_F_IS_NEWING = 0x0001
};

/**
   @internal

   Represents a combination value/operator for an whcl_engine.  Each
   token represents one operand or operator for an whcl evaluation
   stack. They get allocated often, but recycled by their associated
   whcl_engine, so allocations after the first few stack-pops are
   O(1) and cost no new memory.

   Token instances must not be in use more than once concurrently,
   e.g. a token may not be in more than one stack at a time, nor may
   it be in the same stack multiple times. Multiple entries may
   reference the same value, provided it is otherwise safe from being
   swept/vacuumed up.
*/
struct whcl__stoken{
  /**
     A tok1_en_types_e value.
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
  whcl__stoken * next;

  /**
     Used by the parser to communicate source code location
     information to the stack machine. This is necessary so that
     errors generated at the stack machine level (the operator
     implementations) can report the location information, though the
     stack machine does not otherwise know anything about the source
     code.
  */
  struct {
    whcl_script * ct;
    whcl_stoken const * tok;
  } srcPos;
};

/**
   Empty-initialized whcl__stoken structure, intended for
   const-copy initialization.
*/
#define whcl__stoken_empty_m {                     \
    0/*ttype*/,                      \
    0/*value*/,                               \
    0/*next*/,                                \
    {NULL,NULL}/*srcPos*/                     \
  }
/**
   Empty-initialized whcl__stoken structure, intended for
   copy initialization.
*/
extern const whcl__stoken whcl__stoken_empty;

/** @internal

   Emits a message to stderr and calls abort(). This is only intended
   for use in parts of the library where "cannot happen" situations
   somehow happen.
*/
void whcl__fatal2( char const * file, int line, char const * function,
                   int code, char const * fmt, ... );
/** @internal Convenience form of whcl__fatal2(): pass it
    (code, printfFormatString ...). */
#define whcl__fatal(code,...)/* ==> code, fmt[, args] */    \
  whcl__fatal2(__FILE__,__LINE__,__func__,code,__VA_ARGS__)


/** @internal */
int whcl__strace_generate( whcl_engine * const el, cwal_value ** rv );

/** @internal */
int whcl__strace_push( whcl_engine * const el,
                       whcl_script * const ct,
                       whcl_stoken const * srcPos,
                       whcl__strace_entry * const ent );

/** @internal */
void whcl__strace_pop( whcl_engine * const el );

/** @internal */
int whcl__add_script_props( whcl_engine * const el, cwal_value * const ex,
                            whcl_script const * script );

/** @internal */
int whcl__add_script_props2( whcl_engine * const el, cwal_value * const ex,
                            char const * scriptName, int line, int col);

/** @internal */
void whcl__stoken_stack_push( whcl__stoken_stack * const ts, whcl__stoken * const t );

/** @internal */
void whcl__estack_clear( whcl_engine * const el, whcl__estack * const st,
                         bool allowRecycle );

/** @internal
 */
void whcl__estack_swap( whcl__estack * const lhs, whcl__estack * const rhs );

/** @internal
 */
void whcl__engine_stack_swap( whcl_engine * const se, whcl__estack * const st );



void whcl__stoken_stack_clear( whcl_engine * const el, whcl__stoken_stack * const st, bool allowRecycle );
whcl__stoken * whcl__stoken_stack_pop( whcl__stoken_stack * const ts );
void whcl__stoken_stack_push( whcl__stoken_stack * const ts, whcl__stoken * const t );
whcl__stoken * whcl__stoken_alloc( whcl_engine * const el );
whcl__stoken * whcl__stoken_alloc2( whcl_engine * const el, int type, cwal_value * const v );
void whcl__stoken_free( whcl_engine * const el, whcl__stoken * const t, bool allowRecycle );
void whcl__engine_push_op( whcl_engine * const se, whcl__stoken * const t );
whcl__stoken * whcl__engine_push_ttype( whcl_engine * const el, int i );
void whcl__engine_push_valtok( whcl_engine * const el, whcl__stoken * const t );
void whcl__engine_push( whcl_engine * const el, whcl__stoken * const t );
whcl__stoken * whcl__engine_push_tv( whcl_engine * const el, int ttype, cwal_value * const v );
whcl__stoken * whcl__engine_push_val( whcl_engine * const el, cwal_value * const v );
whcl__stoken * whcl__engine_push_int( whcl_engine * const el, cwal_int_t i );
whcl__stoken * whcl__engine_peek_token( whcl_engine * const el );
cwal_value * whcl__engine_peek_value( whcl_engine * const el );
whcl__stoken * whcl__engine_peek_op( whcl_engine * const el );
/**
   Pops the top-most value token and returns its value (which may
   hypothetically be NULL but probably isn't ever).
*/
cwal_value * whcl__engine_pop_value( whcl_engine * const el );
whcl__stoken * whcl__engine_pop_op( whcl_engine * const el, bool returnItem );
whcl__stoken * whcl__engine_epop_token( whcl_engine * const el, bool returnItem );

/** @internal

   If ex is NULL, ex is fetches from cwal_exception_get(). If
   either is non-NULL and that exception appears to have no
   script location information then this function adds that state.
   Errors adding that state (OOM conditions) are silently ignored,
   on the basis if (A) we don't want to hide the already-triggered
   error with this one and (B) the information added by this
   routine is "supplemental" and can be safely ignored.
*/
void whcl__annotate_exception(whcl_engine * const el,
                              cwal_value * ex);

/**
   Arguments type used by the internal command-processing bits.
*/
struct whcl__args {
  /**
     The name from which the command was called _if it has a
     name_. Lambda functions, e.g., don't have names of their own.
  */
  char const * commandName;
  /**
     The WHCL engine associated with this call.
  */
  whcl_engine * el;
  /**
     Provided _solely_ for getting access to the string values of
     this->argv entries. Implementations must not evaluate tokens
     outside of that range.
  */
  whcl_script * ct;
  /**
     An array of evaluation tokens with this->argc entries. Note that
     any given token may itself be a "supertoken" which contains a
     tree of child tokens.

     The internals always terminate this array with a NULL entry to
     simplify iteration in some cases.
  */
  whcl_stoken const * const * argv;
  /**
     The number of entries in this->ct.
  */
  uint16_t argc;

  /**
     true if the current call is a "sub-call", delegated via another
     command. This allows builtins to change their behaviour slightly
     depending on whether they are run as "their own command" or on
     behalf of another. As a rule, this is always true when a builtin
     is run at the start of a line or block of script. It is set to
     false by other builtins which pass on calls, e.g. `decl` and
     `set` both accept a builtin as their 2nd argument and pass that
     call on, along with any remaining arguments.
  */
  bool isSubcall;
};

//! Convenience typedef.
typedef struct whcl__args whcl__args;


#if 0
/**
   A callback proxy type for cwal_callback_f() which includes WHCL-specific
   callback state.
*/
typedef int (*whcl_callback_f)( whcl__args const * const args, cwal_value ** rv );

/**
   INCOMPLETE. DO NOT USE.

   A proxy for cwal_new_function() which wraps the given callback function
   behind a cwal_callback_f() instance.

   Note that it is not possible to bind whcl_callback_f()s to cwal with
   the full client-state feature set of cwal, namely per-function
   client state, because that client-side state slot is used for
   mapping the WHCL-specific state to the callback.
*/
cwal_function * whcl_new_callback( whcl_engine * const el, whcl_callback_f cb );
#endif

//! Required forward decl.
typedef struct whcl__bic whcl__bic;

/** @internal

    Callback type for evaling builting commands. These differ from client-bound
    commands in that they work directly with the whcl_stoken chain.
 */
typedef int (*whcl__bic_f)(whcl__bic const * const bic,
                           whcl__args const * const args,
                           cwal_value **rv);

/* Defined in kwhash.c. */
extern uint32_t whcl__hash_keyword( char const * input, uint16_t len );
#define whcl__hash_token(TOKER,TOKEN) \
  whcl__hash_keyword(whcl_stoken_cstr((TOKER), (TOKEN), NULL, false), (uint16_t)(TOKEN)->length)

/**
   A "bic": built-in command/constant.
*/
struct whcl__bic {
  /** Command/constant name. */
  char const * name;
  /**
     Token type mapped to this builtin.
   */
  int16_t ttype;
  /**
     Minimum number of expected arguments.
  */
  int16_t argcMin;
  /**
     Max number of expected args. Use a negative value,
     or a grossly large one, to indicate infinity.
   */
  int16_t argcMax;
  /**
     Handler callback.
  */
  whcl__bic_f f;
  /**
     Brief usage text for error reporting. Do not include
     the command name in this string.
  */
  char const *usage;
};


/** @internal BIC for the pragma command. */
int whcl__bic_f_pragma(whcl__bic const * const bic,
                       whcl__args const * const args,
                       cwal_value **rv);
/** @internal BIC for the info command. */
int whcl__bic_f_info(whcl__bic const * const bic,
                     whcl__args const * const args,
                     cwal_value **rv);
/** @internal BIC for the foreach command. */
int whcl__bic_f_foreach(whcl__bic const * const bic,
                        whcl__args const * const args,
                        cwal_value **rv);
/** @internal BIC for the proc and lambda commands. */
int whcl__bic_f_proc(whcl__bic const * const bic,
                     whcl__args const * const args,
                     cwal_value **rv);

/** @internal

    Flags for whcl__eval_script() and whcl__eval_token().
*/
enum whcl__eval_script_flags_e {
  /**
     Tells whcl__eval_script() to make a single call to
     whcl__eval_token() instead of processing the toker
     as a series of commands.
  */
  WHCL__EVAL_F_EVAL_TOKEN = 0x01,
  /**
     Tells whcl__eval_script() to check for and reject extra tokens
     beyond the first if WHCL__EVAL_F_EVAL_TOKEN is in effect.
  */
  WHCL__EVAL_F_REJECT_TRAILING_JUNK = 0x02,
  /**
     Tells whcl__eval_token() to evaluate a squiggly group as a code
     block instead of a string. This does not affect
     whcl__eval_script() or whcl__eval_sub().
  */
  WHCL__EVAL_F_SQUIGGLY_AS_CODE = 0x04,
  /**
     Works just like WHCL__EVAL_F_SQUIGGLY_AS_CODE, and includes that
     flag, but tells whcl__eval_token() to push a new scope.
  */
  WHCL__EVAL_F_SQUIGGLY_AS_SCOPE = 0x0C,
  /**
     Flags used when evaluating the subscript part of `$var[subscript]`.
  */
  WHCL__EVAL_F_DEREF2_SUB =
  WHCL__EVAL_F_EVAL_TOKEN
  | WHCL__EVAL_F_REJECT_TRAILING_JUNK,

  /**
     Tells whcl__eval_sub() that it must evaluate the sub-tokenizer's
     pieces via whcl__eval_expr() instead of whcl__eval_script().

     This implicitly tells whcl__eval_sub() to pass on the flags
     WHCL__EXPR_F_IGNORE_EOL and WHCL__EXPR_F_EOX_NO_SEMICOLON to
     whcl__eval_expr().
  */
  WHCL__EVAL_F_SUB_EXPR = 0x10,

  /**
     Tells whcl__eval_sub() and whcl__eval_script() that this eval is
     the contents of a `[command ...]` block, which causes it to set
     the isSubcall flag on the on the resulting call (if it's a BIC).
     That, in turn, may cause some BICs to behave slightly
     differently. e.g. proc defaults to (proc -anon) in this case.
  */
  WHCL__EVAL_F_SUB_CALLBLOCK = 0x20,

  /**
     Tells whcl__eval_token() than expansion of a `@...` token is
     legal. Without this flag, such tokens trigger an error.
   */
  WHCL__EVAL_F_EXPANDO_OKAY = 0x40,

  /**
     Tells whcl__eval_script() that if it pushes a new scope, the new
     scope must inherit the whcl_scope::flags from the current scope.
     This is needed, e.g., when evaling from within a loop body, so
     that certain other block constructs in that loop can propagate
     `continue` and `break`.
  */
  WHCL__EVAL_F_SCOPE_INHERIT_FLAGS = 0x100
};


/** @internal

   A proxy for tok2_create_value() which performs some additional
   whcl-specific processing:

   - TOK1_T_BIV: resolves to the BIV specified by t->ttype2 unless...

   - If bivAsString is true then any BIV tokens are evaluated as
     identifiers (that is, unquoted strings) instead of using their
     normal BIV semantics.

   rv may not be NULL.

   If el is currently in skip-mode this is a no-op: it assigns
   `*rv` to the undefined value and returns 0.

   @see whcl__create_value()
*/
int whcl__create_value2( whcl_engine * const el, whcl_script * const ct,
                        whcl_stoken const * const t, bool bivAsString,
                        cwal_value ** rv );

/** @internal

    Equivalent to whcl__create_value2(el,ct,t,false,rv).

    @see whcl__create_value2()
*/
int whcl__create_value( whcl_engine * const el, whcl_script * const ct,
                        whcl_stoken const * const t, cwal_value ** rv );

/** @internal

   Runs all remaining commands in ct. Be sure to whcl_script_rewind(),
   if needed, before calling this. If rv is not NULL then it is
   assigned to the final result value.

   flags may be a bitmask from the whcl__eval_script_flags_e enum.

   If pushScope is true, a new whcl scope is pushed for executing
   the script, and popped before returning (upscoping `*rv`, if needed).
   On error, `*rv` is not modified.
*/
int whcl__eval_script( whcl_engine * const el, bool pushScope,
                       whcl_script * const ct, uint32_t flags,
                       cwal_value **rv );

/**
   Convenience wrapper around:

   ```
   whcl_script sub = whcl_script_empty;
   rc = whcl_script_sub_from_group(ct, &sub);
   if(0==rc) rc = whcl__eval_script(el, pushScope, &sub, flags, rv);
   whcl_script_finalize(&sub);
   ```

   If el is in skip-mode, this is a no-op: it will assign
   `*rv` (if rv is not NULL) to the undefined value and
   return 0.
*/
int whcl__eval_sub( whcl_engine * const el, bool pushScope,
                    whcl_script * const ct, uint32_t flags,
                    cwal_value **rv );

/**
   Convenience wrapper around wchl__eval_sub() which saves ct's
   current token position, sets the position to the given token (which
   _must_ be a supertoken), calls whcl__eval_sub(), restores ct's
   original token position, and returns the result of that call.
*/
int whcl__eval_sub2( whcl_engine * const el, bool pushScope,
                     whcl_script * const ct,
                     whcl_stoken const * const tok,
                     uint32_t flags,
                     cwal_value **rv );


/**
   Evaluates a single token. On succes, stores the result in `*rv` and
   returns 0.

   flags may be a bitmask from the whcl__eval_script_flags_e enum.

   Intended primarily for evaluating the arguments to a command, as
   opposed to evaluating arguments in a command's position. However,
   also intended to be used when the command part of a line refers to
   something other than a builtin value.

   This function stores ct's current token position when it starts and
   restores it before returning, so the end effect is as if ct were
   not traversed.

   This function sets ct's current token to tok before starting and
   restores it before returning.

   This function clears whcl__dotop_set() before it starts work.

   Peculiarities of certain eval types:

   - If whcl__token_has_prop() is true for the given token, it's
     treated like a chain of one or more property access operations
     (`x[y][z][...]`) and is resolved iteratively. The final resolved
     value is set to `*rv` (if rv is not NULL). The right-most LHS and
     right-most property key parts of such a chain will be _briefly_
     available via whcl__dotop_get().
*/
int whcl__eval_token(whcl_engine * const el, whcl_script * const ct,
                     whcl_stoken const * const tok, uint32_t flags,
                     cwal_value ** rv);


/**
   If args->argv[ndx] has type TOK1_T_WHCLFlag then that token is returned,
   else NULL is returned.
*/
whcl_stoken const * whcl__args_hasflag( whcl__args const * const args,
                                       uint16_t ndx );

/**
   If (args->argc - ndx) is less than minNeeded then exception is
   thrown and that result is returned, else 0 is returned.
 */
int whcl__args_ncheck( whcl__args const * const args,
                       uint16_t ndx, uint16_t minNeeded );


/**
   If `args->argv[*ndx]` matches the given flag string then `*ndx` is
   incremented and true is returned, else ndx is not modified and
   false is returned.
*/
bool whcl__args_next_matches(whcl__args const * const args,
                             uint16_t * ndx,
                             char const * flag );

/**
   Enumeration of various scope-sweeping options.
 */
enum WhclSweepModes {
/**
   ACHTUNG: recursive vacuum _cannot_ work reliably because...

   During the vacuum, all values in the scope which are not safe from
   that operation are removed. That's fine when we have only 1 active
   scope. All values in a given scope are guaranteed by internal API
   invariants to reference/contain no values from newer scopes, but
   the converse is not true: a value in a newer scope can reference
   values in an older scope. Vacuuming of an older scope, however,
   requires unreffing all values owned by the scope until they die.
   In the recursive case, a value in a newer scope can be pulled out
   from under that scope, leading to corruption of the newer scope's
   value lists.

   This entry is retained (1) as a reminder to self and (2) in the
   hopes that we can adapt the cleanup for the recursive case.
*/
SweepMode_VacuumRecursive = -2,
SweepMode_SweepRecursive = -1,
SweepMode_Default = 0,
SweepMode_Sweep = 1,
SweepMode_Vacuum = 2
};

/**
   Might or might not trigger a sweep or vacuum, depending partially
   on the initialBroomMode _hint_ and various internal state. If force
   is true then el's internal sweep counters are ignored for purposes
   of determining "might or might not." Returns non-0 only on catastrophic
   failure.
*/
int whcl__sweep_impl( whcl_engine * const el, enum WhclSweepModes initialBroomMode,
                      bool force );

/**
   If ct is not at an EOF token and the _next_ token is not an EOF
   token then an exception is thrown and that result is returned, else
   0 is returned.

   If allowEox is true then any EOX is permitted as the next token.
*/
int whcl__trailing_junk_check(whcl_script * const ct, bool allowEox);

/**
   Flags for use with whcl__eval_expr().
*/
enum whcl__expr_flags_e {
/**
   Tells whcl__eval_expr() to push a scope for the expression.
 */
WHCL__EXPR_F_PUSH_SCOPE = 0x01,
/**
   Disallow a semicolon at the end of the expression. This "should" be
   used on block constructs which are evaluated as expressions, solely
   for the sake of pedantic nicecity.
*/
WHCL__EXPR_F_EOX_NO_SEMICOLON = 0x02,

/**
   Tells expression handling to ignore all EOLs. This is primarily
   intended for use in contexts like flow-control expression check
   blocks.
*/
WHCL__EXPR_F_IGNORE_EOL = 0x04,

/**
   Mask of flags which whcl__eval_expr() should propagate when
   recursively calling itself.
*/
WHCL__EXPR_F_RECURSIVE =
  WHCL__EXPR_F_EOX_NO_SEMICOLON | WHCL__EXPR_F_IGNORE_EOL
};

/** @internal

   Evaluates el->ct's tokens as an expression (not command), starting
   at the _current_ token of el->ct and going until (at most) the next
   EOX/EOF.  The result value is assigned to `*rv`. Returns 0 on
   success. If el->ct is currently rewound then evaluation starts at
   the first token.

   If the current token is a group construct, its contents are
   processed as an expression.

   The flags argument may change how an expression is evaluated.

   May invoke recursion.
*/
int whcl__eval_expr( whcl_engine * const el, uint32_t flags, cwal_value ** rv );

/** @internal
   
    If rc is an "especially fatal"/non-trumpable result code it is
    returned, else if el's interrupted flag has been set, that is
    returned, else 0 is returned.
*/
int whcl__check_interrupted( whcl_engine * const el, int rc );

/** @internal
   Set's el's error state to an OOM error. This will, if able, track the
   script-side location of the error via el->ec. Returns CWAL_RC_OOM.
*/
int whcl__err_oom(whcl_engine * const el);

/**
   Pushes v onto the current eval holder array. To simplify usage,
   this function refs v before insertion and unrefs v after insertion,
   _even on error_. Thus if v is a new/temporary value, this function
   effectively takes over ownership of it. Returns 0 on success,
   CWAL_RC_OOM on allocation error.

   Adding v to the eval holder inherently adds a reference and makes
   it vacuum-proof until the eval holder is destroyed or truncated to
   a shorter length.
*/
int whcl__holder_push(whcl_engine * const el, cwal_value * const v);
/**
   Truncates el's eval holder to the given length. If the 3rd
   argument is not NULL then that value is referenced before
   truncation and unhanded afterwards, so that it survives
   the truncation process (assuming it's in the eval holder
   at all).
*/
void whcl__holder_truncate(whcl_engine * const el, cwal_size_t len,
                           cwal_value * const keepThis);
/**
   Returns el's current eval holder's current length.
*/
cwal_size_t whcl__holder_len(whcl_engine const * const el);

/**

   Enum specifying the precedences of operators in whcl.

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
enum whcl__precedents {
WHCL__PR__start = 1,

/**
   Parens precedence is actually irrelevant here, as we parse parens
   groups as atomic values instead of operators.
*/
WHCL__PR_ParensOpen,
WHCL__PR_Comma,

/**
   =  +=  -=  *=   /=  <<=  >>=  %=   &=  ^=  |=
*/
WHCL__PR_Assignment__,
/**
   The = operator.
*/
WHCL__PR_OpAssign =WHCL__PR_Assignment__,
/**
   PHP-style array-append. Essentially works like (array DOT index =
   ...), where the index is the array's current length. Only usable in
   assignment contexts.
*/
WHCL__PR_ArrayAppend = WHCL__PR_Assignment__,

/* WHCL__PR_Conditional__, */
/**
   In JavaScript ternary if has a higher precedence than
   assignment, but we're going to go with the C/C++ precedence
   here.
*/
WHCL__PR_TernaryIf = WHCL__PR_Assignment__,

WHCL__PR_Logical__,
WHCL__PR_LogicalOr = WHCL__PR_Logical__ /* || */,
WHCL__PR_LogicalOr3 = WHCL__PR_Logical__ /* ||| */,
WHCL__PR_LogicalOrElvis = WHCL__PR_Logical__ /* ?: */,
WHCL__PR_LogicalAnd = WHCL__PR_Logical__ /* && */,

WHCL__PR_Bitwise__,
WHCL__PR_BitwiseOr = WHCL__PR_Bitwise__,
WHCL__PR_BitwiseXor = WHCL__PR_Bitwise__ + 1,
WHCL__PR_BitwiseAnd = WHCL__PR_Bitwise__ + 2,

WHCL__PR_Equality__,
WHCL__PR_CmpEq = WHCL__PR_Equality__,
WHCL__PR_CmpEqStrict = WHCL__PR_Equality__,
WHCL__PR_CmpNotEq = WHCL__PR_Equality__,
WHCL__PR_CmpNotEqStrict = WHCL__PR_Equality__,

WHCL__PR_Relational__,
WHCL__PR_CmpLT = WHCL__PR_Relational__,
WHCL__PR_CmpGT = WHCL__PR_Relational__,
WHCL__PR_CmpLE = WHCL__PR_Relational__,
WHCL__PR_CmpGE = WHCL__PR_Relational__,
WHCL__PR_OpInherits = WHCL__PR_Relational__,

/**
   '=~'. Should this have Equality precedence?
*/
WHCL__PR_Contains = WHCL__PR_Relational__,
/**
   '!~'. Should this have Equality precedence?
*/
WHCL__PR_NotContains = WHCL__PR_Relational__,

/*
  TODO?

  <== and >== for strict-type LE resp GE

  ==< resp ==> for strict LT/GT
*/

WHCL__PR_Bitshift__,
WHCL__PR_ShiftLeft = WHCL__PR_Bitshift__,
WHCL__PR_ShiftRight = WHCL__PR_Bitshift__,

WHCL__PR_Additive__,
WHCL__PR_Plus = WHCL__PR_Additive__,
WHCL__PR_Minus = WHCL__PR_Additive__,

WHCL__PR_Multiplicative__,
WHCL__PR_Multiply = WHCL__PR_Multiplicative__,
WHCL__PR_Divide = WHCL__PR_Multiplicative__,
WHCL__PR_Modulo = WHCL__PR_Multiplicative__,

WHCL__PR_Unary__,
WHCL__PR_PlusUnary = WHCL__PR_Unary__,
WHCL__PR_MinusUnary = WHCL__PR_Unary__,
WHCL__PR_LogicalNot = WHCL__PR_Unary__,
WHCL__PR_BitwiseNegate = WHCL__PR_Unary__,
WHCL__PR_Keyword  = WHCL__PR_Unary__,
WHCL__PR_IncrDecr  = WHCL__PR_Unary__,
/**
   Used by 'foreach' to tell evaluation to stop at this operator.  Its
   precedence must be higher than WHCL__PR_Comma but lower than anything
   else.
*/
WHCL__PR_OpArrow2 /* => */ = WHCL__PR_Unary__,
/* C++: WHCL__PR_Unary__ ==>

   sizeof, new, delete, & (addr of), * (deref),
   (typeCast), 
*/

/* WHCL__PR_Kludge__, */
/* WHCL__PR_DotDot = WHCL__PR_Kludge__, */

WHCL__PR_Primary__,
WHCL__PR_FuncCall = WHCL__PR_Primary__,
WHCL__PR_Subscript = WHCL__PR_Primary__,
WHCL__PR_Braces = WHCL__PR_Primary__,
WHCL__PR_DotDeref = WHCL__PR_Primary__,
WHCL__PR_DotDot = WHCL__PR_DotDeref,
WHCL__PR_OpColon2 = WHCL__PR_DotDeref,
WHCL__PR_OpArrow = WHCL__PR_DotDeref,


/*
  C++: WHCL__PR_Primary__ ==>

  typeid(), xxx_cast

*/
WHCL__PR_Specials__,
WHCL__PR_ParensClose,
WHCL__PR_NamespaceCpp /* :: */,
WHCL__PR_end__
};

/** @internal
   Callback function for operator implementations.
     
   The framework will pass the operator, the underlying engine, the
   number of operatand it was given (argc).

   Preconditions:

   - el's token stack will contain (argc) tokens intended
   for this operator.

   - The top of the operator stack will not be this operator
   (or not this invocation of it).

   - rv will not be NULL, but may point to a NULL pointer.

   Implementation requirements:

   - Implementations MUST pop EXACTLY (argc) tokens from el's
   token stack before returning or calling into any API which
   might modify the token stack further. They must treat those
   values as if they are owned by the caller.

   - A result value, if any, must be assigned to *rv. If the
   operation has no result, assign it to 0. The API will never pass
   a NULL rv to this routine. If the operation creates a scope and
   receives *rv from that scope then it must be sure to upscope *rv
   before returning, to ensure that *rv is still valid after the
   scope is popped. As a rule, the result value is ignored/discarded
   if non-0 (error) is returned.

   - Must return 0 on success, and a non-0 CWAL_RC_xxx code on
   error.

   - If el->skipLevel is greater than 0, then the operator must pop
   all arguments from the stack, do as little work as possible
   (e.g. no allocations or calculations, and no visible side effects),
   assign *rv to cwal_value_undefined(), and return 0. This is used
   for implementing quasi-short-circuit logic, in that we allow the
   operators to run, but skip-mode indicates that we really are only
   interested in getting past the operator and its arguments, without
   having side-effects like creating new values.

   Certain result codes will be treated specially by the framework and
   have certain pre- and post-conditions (not described in detail here):

   CWAL_RC_OOM triggers a fatal OOM error.

   CWAL_RC_EXCEPTION means the function triggered an exception
   and set the engine's exception state.

   CWAL_RC_EXIT, CWAL_RC_FATAL, and CWAL_RC_INTERRUPTED trigger and
   end of the current evaluation and set the engine's error state.
*/
typedef int (*whcl__op_f)( whcl__op const * const self, whcl_engine * const el,
                           int argc, cwal_value **rv );

/** @internal
   Represents a stack machine operation. Each operation in s2 equates
   to a shared/const instance of this class.
*/
struct whcl__op {
  /**
     Generic symbolic representation, not necessarily what appears
     in script code. Need not be unique, either. Primarily intended
     for use in debugging and error messages.
  */
  char const * sym;

  /**
     Operator type ID. Must be one of the tok1_en_types_e values.
  */
  int id;

  /**
     Number of expected operands. Negative value means any number, and the
     syntax will have to determine where to stop looking for operands.
  */
  int8_t arity;

  /**
     Associativity:

     <0 = left

     0 = non-associative

     >0 = right
  */
  int8_t assoc;

  /**
     Precedence. Higher numbers represent a higher precedence.
  */
  int8_t prec;

  /**
     Describes where the operator "sits" in relation to its
     operand(s).

     <0 = has no operands or sits on the left of its
     operand(s). Arity must be >=0.

     0 = sits between its 2 operands. arity must == 2.

     >0 = sits on the right of its 1 operand. arity must == 1.

  */
  int8_t placement;

  /**
     The operator's implementation function.
  */
  whcl__op_f call;
};


/** @internal

   If ttype (token type) represents a known operator then that
   operator's shared/static/const instance is returned, otherwise NULL
   is returned.
*/
whcl__op const * whcl__ttype_op( int ttype );

/**
   If ttype (token type) represents a known operator then that
   operator's shared/static/const instance is returned. Else if it's a
   token type of a common operator which whcl does not directly
   support as such, or may internally transform into a known operator,
   that ttype is returned. Else NULL is returned.
*/
int whcl__ttype_op2(int ttype);

/** @internal

    Equivalent to whcl__ttype_op(st->ttype).
*/
whcl__op const * whcl__stoken_op(whcl__stoken const * const st);

/**
   A utility function implementing unary and binary addition and
   subtraction of numbers (in the form of cwal_values).

   This routine does not take overloading into account.

   To do binary operations:

   - Both lhs and rhs must be valid values. They will be converted
   to a number if needed. lhs is the left-hand-side argument and rhs
   is the right-hand-side.

   - Set doAdd to true to perform addition, false to perform subtraction.

   For unary operations:

   - As for binary ops, but lhs must be NULL. The operation is applied
   solely to rhs.

   The result value is assigned to *rv, and rv may not be NULL.

   Returns 0 on success, non-0 on misuse or allocation error.

   For unary ops, the result is always the same numeric type as the
   the operand. For binary, it's double if either argument is double,
   else it's integer. Note, however, that doubles have a smaller numeric
   range than 64-bit integers.

   Returns 0 on success, non-0 on error. The only foreseen error
   result code is CWAL_RC_OOM.
*/
int whcl__values_addsub( whcl_engine * const el, bool doAdd, cwal_value * const lhs,
                         cwal_value * const rhs, cwal_value **rv );
/** @internal

    Processes the top-most operator in el's expression evaluator.
*/
int whcl__process_top( whcl_engine * const el );

/** @internal

   Sets up el's internals so that it knows the most recent dot-op
   self/lhs/key operands (or clears them, if passed NULLs for the last
   3 arguments). If self is not NULL then self is considered to be
   "this" in the corresponding downstream handling (i.e. it's a
   property access which binds "this"), else it does not. self is
   always lhs or NULL. lhs is either the container for a property
   lookup (if self!=0) or a scope var lookup. key is the associated
   property key.

   The property/scope variable operations set these up and several
   places in the eval engine (namely function calls) consume and/or
   clear/reset them (to avoid leaving stale entries around).

   Pass NULL as the last 3 arguments to clear/reset it.

   Reminder to self: before calling this, MAKE SURE you've got ref to
   any local values which are about to returned/passed along. If
   needed, ref before calling this and unhand afterwards. If a value
   about to be returned/upscoped is one of the current dotop values
   then that ref will save the app's life.
*/
void whcl__dotop_set( whcl_engine * const el, cwal_value * const lhs,
                      cwal_value * const key,
                      cwal_value * const self );

/** @internal

    Convenience form of whcl_scope_push() which re-uses all flags
    from the parent scope which are in the WHCL__SCOPE_F_PUSH2 mask.
    The primary purpose of this is to ensure that such scopes behave
    properly with the break/continue commands when they're called from
    an inner scope inside of a loop construct.
*/
whcl_scope * whcl__scope_push2(whcl_engine * const el);

/** @internal

   Internal impl of whcl_scope_push() which accepts flags from the
   whcl__scope_flags_e enums. This clears el->scopes.nextFlags.
*/
whcl_scope * whcl__scope_push(whcl_engine * const el, cwal_flags16_t flags );

/**
   Expects src to be a container from which to copy properties and scel
   to be the target scope into which the properties should be copied.
   On error, el's error state is updated. If scel is NULL then el's
   current scope is used.
*/
int whcl__scope_import_props(whcl_engine * const el, whcl_scope * scel,
                             cwal_value * const src);

/** @internal
   If the given string matches the name of a builtin value
   then its token type is returned, else 0 is returned.
*/
int whcl__biv_search(char const *s, uint16_t n);

/** @internal

   Proxy for whcl__biv_search() which uses the given
   token's string.
*/
int whcl__biv_search_t(whcl_script const * const ct,
                       whcl_stoken const * const t);


/** @internal
   If ch is an EOX character (semicolon, non-backslashed newline, or
   EOF (perhaps virtual)), returns ch, else returns 0.
*/
int whcl_t_is_eox(int ch);


/**
   If the given value corresponds to an "operator" type from the
   tok1_en_types_e enum, ch is returned, else 0 is returned.

   The list of operators is extremely long.
*/
int whcl__ttype_is_operator(int ch);

/** @internal

    Returns ttype if ttype refers to one of the builtin values (BIVs),
    else returns 0. BIV tokens are encoded with a ttype of TOK1_T_BIV
    and a ttype2 of one of the TOK1_T_BIV_xxx subtypes.
*/
int whcl__ttype_is_biv(int ttype);

/** @

    Returns ttype if ttype refers to one of the builtin commands (BICs),
    else returns 0. BIC tokens are encoded with a ttype of TOK1_T_BIC
    and a ttype2 of one of the TOK1_T_BIC_xxx subtypes.
*/
int whcl__ttype_is_bic(int ttype);

/** @internal

   If the given value (assumed to be an exception) does _not_
   have a stackTrace property of its own, one is added
   and true is returned (regardless of whether adding it failed
   or not - an OOM during creation of the stack trace is
   silently ignored). Returns false if ex already has such
   a property.
*/
bool whcl__exception_add_stacktrace(whcl_engine * const el,
                                    cwal_value * const ex);


/** @internal

   Equivalent to whcl__read_identifier2() with a value of 0
   for the final argument.
*/
int whcl__read_identifier( char const * zPos,
                           char const * zEnd,
                           char const ** zIdEnd );

/** @internal

   Assumes that zPos is the start of an identifier and reads until the
   next non-identifier character. zEnd must be the logical EOF for
   zPos.

   Expects the input to be valid ASCII/UTF-8, else results are
   undefined.

   See whcl__is_var_char() for the list of valid characters, noting tha
   the flags argument may be a bitmask of whcl_script_flags_e values
   to modify the parsing.

   If it reads an identifer it returns the number of bytes read from
   the input and sets `*zIdEnd` to the one-after-the-end position of
   the read identifier (which will be (*zIdEnd-pos) bytes long).

   When dashes are permitted via the final argument, it treats a
   string composed of only dashes as a non-identifier and returns 0.

   If it returns 0, it does not modify `*zIdEnd`.

   @see whcl__read_identifier()
*/
int whcl__read_identifier2( char const * zPos,
                            char const * zEnd,
                            char const ** zIdEnd,
                            uint32_t flags );

/** @internal

   A proxy for tok2_compile() which applies WHCL's tokenizer, always
   includes the TOK2_TOKER_F_IDENTIFIER_DASHES2 tok2_compile() flag, and
   reports any error details via el's underlying cwal_engine.

   If ct->ec is NULL this function passes el->ec and ct to
   whcl__script_init(), otherwise it does not. That's significant
   because that init call will wipe out any name set in ct->name. Thus
   callers who want ct->name to stay intact (for error-reporting
   purposes) must use whcl_script_init() and _then_ set ct->name before
   passing it to this function.

   It is not legal to pass the same script object to this function
   without calling whcl__script_finalize().
*/
int whcl__compile( whcl_engine * const el,
                   whcl_script * const ct,
                   char const * src,
                   cwal_int_t len, uint32_t flags );

/** @internal

   A proxy for whcl__compile() which takes a cwal_buffer as input.

   In order for ct->name to be set during compilation, ct must have been
   passed to whcl_script_init() _before_ calling this and its name set
   after that call.
*/
int whcl__compile_buffer( whcl_engine * const el,
                          whcl_script * const ct,
                          cwal_buffer const * const buf,
                          uint32_t flags );

/** @internal

   Flags for use with whcl__func::flags.
*/
enum whcl__func_flags_e {
/** Flags the function as having no parameters,
    which allows a small internal optimization. */
WHCL__FUNC_F_EMPTY_PARAMS = 0x01,
/** Flags the function as having an empty body, which allows us to
    bypass ever actually calling the function. */
WHCL__FUNC_F_EMPTY_BODY = 0x02,
WHCL__FUNC_F_EMPTY =
  WHCL__FUNC_F_EMPTY_PARAMS | WHCL__FUNC_F_EMPTY_BODY,
/**
   If set, the function's vImports are not imported,
   but are instead available only via the `using` keyword.
*/
WHCL__FUNC_F_NO_IMPORT_USING = 0x04
};

/** @internal

   State representing a script-side function.
*/
struct whcl__func {
  /** An object holding all imported symbols. */
  cwal_value *vImports;
  /** The function's name, if any. */
  cwal_value *vName;
  /**
     The script name associated with each function is, for lifetime
     management sanity's sake, kept in a map in the interpreter. This
     means they have infinite lifetime, which is unfortunate, but the
     number of distinct strings will normally be very small and they
     are reused for all functions defined in a given script.
  */
  cwal_value *vScriptName;

  /**
     For creating a linked list for the recycling subsystem.
  */
  whcl__func * next;

  /**
     Internal flags.
  */
  cwal_flags16_t flags;

  /**
     Source code and tokens for the function. We require this because
     the token processing needs it. We have to copy it from the outer
     parent tokenizer in case the function outlives the parent (which
     is very often the case with script-bound functions).
  */
  struct {
    whcl_script script;
    /** Token holding the function's name. Might be NULL. */
    whcl_stoken const * fname;
    /** {}-type token holding the function's parameters. */
    whcl_stoken const * params;
    /** {}-type token holding the function's body. */
    whcl_stoken const * body;
  } tok;
};

/** @internal

   Allocates new whcl__func, possibly a recycled one, and transfers ownership
   to the caller, who is obligated to eventually pass it to whcl__func_free().
*/
whcl__func * whcl__func_alloc(whcl_engine * const el);

/** @internal

   Transfers ownership of f to this function, cleans up all resources
   owned by f and either frees it or sticks it into el's recycling
   subsystem.
*/
void whcl__func_free(whcl_engine * const el, whcl__func * const f, bool allowRecycle);

/** @internal

   Fetches the whcl__func state bound to f, if any. Returns it or
   returns NULL if it is not a script-side function. Ownership is not
   modified by this call.

   This is intended to be passed the `args->callee` value which is
   passed to a cwal_callback_f() impl.
*/
whcl__func * whcl__func_for_func(cwal_function * const f);

/** @internal

   Creates a new cwal function which wraps a whcl__func object and
   installs the finalizer and rescoper necessary for keeping the
   whcl__func instance well-managed.

   On success returns the new function and assigns `*rv` to the new
   whcl__func instance (which is owned by the returned function and
   will be cleaned up by it).

   It is up to the caller to finish populating the whcl__func state
   object.

   Returns NULL only on OOM.
*/
cwal_function * whcl__func_new(whcl_engine * const el,
                               whcl__func ** rv);

/** @internal

   If the given name refers to a known script file name, that name's
   cached entry is returned, else that string is added to the script
   name cache and its value is returned. It is owned by el and valid
   until engine shutdown. It must not be ref'd or unref'd by the
   caller. Returns NULL only on OOM.
 */
cwal_value * whcl__script_name(whcl_engine * const el,
                               char const * fname,
                               uint16_t nameLen);

/**
   Flags for use with whcl__read_new_object().
*/
enum whcl__read_object_flags_e {
/**
   If set, then the final argument to the `object` command flags must
   be a `{...}` block. Its code is evaluated in a new scope and the
   resulting object is the properties object of that scope. Thus all
   variables defined in that scope are properties of the resulting
   object and they retain special attributes such as `const`.
*/
WHCL__READ_OBJ_F_SCOPE = 0x01,
/**
   If set, for the duration of the object body execution, `this` will
   refer to the being-constructed object.

   This flag implies WHCL__READ_OBJ_F_SCOPE.
*/
WHCL__READ_OBJ_F_THIS = 0x03
};

/** @internal

   Creates a new Object value from its inputs, but how it
   functions depends entirely on the 4th argument.

   `tok` must be the token immediately _before_ the object properties
   are to be read. That is, a call to tok2_next_token2() must take us
   to the first property or the object `{...}` wrapper.

   The flags may be 0 or a bitmask of values from the
   whcl__read_object_flags_e enum.

   If `fromScope` is false then:

   1) For each pair of tokens which follows it, to the next EOX, it
   creates a key/value pair in that object. The keys are evaluated
   using whcl__create_value() and the values via
   whcl__eval_token(). tok must point to the token before the first
   key and this function sets/manipulates ct's current token. It is
   not an error for no key/value pairs to be defined but it is an
   error to have an odd number of tokens.

   2) If the first token is a `{...}` then it is considered to be the
   object body and key/value pairs are pulled from there instead and
   all EOLs in that block are ignored.

   3) Value tokens in the form `{...}` are treated as object literals,
   not strings, and are recursively processed.

   4) Value tokens in the form `(...)` are treated as array literals,
   not strings, and are recursively processed.

   On success, if `rv` is not NULL then `*rv` is set to the new object
   and 0 is returned. On error, `*rv` is not modified, non-0 is
   returned, and el's error state is updated. If `rv` is NULL, the
   processing described above is still run but the result is
   discarded. If `rv` is NULL we "could" arguably skip processing
   entirely or run in skip-mode so that most sorts of errors would
   simply be skipped over, but we don't currently do so.
*/
int whcl__read_new_object(whcl_engine * const el, whcl_script * ct,
                          whcl_stoken const * tok, uint32_t rdobjFlags,
                          cwal_value ** rv);

/**
   The array counterpart of whcl__read_new_object().
   tok must point to the token immediately before the first
   array value. If the initial token is a `(...) then that is
   considered to be the array, and each value in the array
   is appended to it.

   Value tokens in the form `{...}` are treated as object literals,
   not strings, and are recursively processed.

   Value tokens in the form `(...)` are treated as array literals,
   not strings, and are recursively processed.

   On success `*rv` is set to the new array and 0 is returned. On
   error, `*rv` is not modified, non-0 is returned, and el's error
   state is updated.
*/
int whcl__read_new_array(whcl_engine * const el, whcl_script * ct,
                         whcl_stoken const * tok, cwal_value ** rv);


/**
   Works _almost_ exactly like tok2_next_token2() but skips over any
   EOL tokens. Returns 0 on success and non-0 only on a seriously
   unexpected errors (assert()-worth). Aside from EOL skipping, this
   function sets the putback token to the token which is current when
   this function is called, as opposed to setting it to a skipped-over
   EOL.  Thus calling tok2_putback() after calling this will return
   the toker to the token it had before calling this, regardless of
   how many EOLs this function skips. This function always sets `*tgt`
   to NULL before starting, so callers can be certain that, even if it
   returns non-0, it never points to a stale token pointer.
*/
int whcl__next_token_no_eol(whcl_script * const ct,
                            whcl_stoken const ** tgt);


/** @internal
   Rescopes, if needed, v into lhs's scope. lhs must
   be a non-builtin value.
*/
void whcl__value_to_lhs_scope( cwal_value const * lhs,
                               cwal_value * const v);

/**
   Installs the given value into `el->cache.vWhcl[name]`
   as a CONST. Returns 0 on success, CWAL_RC_OOM on OOM, and
   CWAL_RC_CONST_VIOLATION if the same name is used more than
   once.
*/
int whcl__install_into_whcl(whcl_engine * const el,
                            char const * name,
                            cwal_value * const v);
/** @internal

    Fetches a value previously stored via whcl__prototype_stash(), or
    NULL if no match is found.
*/
cwal_value * whcl__prototype_stashed( whcl_engine * el, char const * typeName );

/** @internal

   Adds a mapping to `el->cached.vProtos[typeName] = proto`, setting
   it as CONST. Returns 0 on success, CWAL_RC_OOM on OOM, and
   CWAL_RC_CONST_VIOLATION if the same name is used more than
   once.
*/
int whcl__prototype_stash( whcl_engine * const el, char const * typeName,
                          cwal_value * const proto );

/** @internal

    Installs a new Object-type property into tgt[name] and removes its
    prototype.  If installCommandMethod is true then it also passes
    that new object to whcl__install_command_cb(). Returns 0 on
    succes, CWAL_RC_OOM on OOM.

    On success, if `theSub` is not NULL then the new object is
    assigned to `*theSub`.
*/
int whcl__install_sub(whcl_engine * const el, cwal_value * const tgt,
                      char const * name, bool installCommandMethod,
                      cwal_value **theSub);

/** @internal

   cwal_callback_f() impl which implements "subcommand semantics":

   It looks up args->argv[0] and expects that to be the name of a
   property of args->this which resolves to a function, then calls
   that function, passed on args, with args->argc and args->argv
   trimmed by one. Throws if any of those parts fail.
*/
int whcl__cb_command( cwal_callback_args const * args, cwal_value **rv );


/** @internal
   A cwal_callback_f() implementation which parses JSON string input.

   Script usage:

   ```
   var json = '{"a":"hi!"}';
   var obj = thisFunction(json)
   ```
*/
int whcl__cb_json_parse_string( cwal_callback_args const * args, cwal_value **rv );

/** @internal
   The file-based counterpart of whcl_cb_json_parse_string(). It works
   identically except that it takes a filename as input instead of a
   JSON string.

   This callback honors the WHCL_DISABLE_FS_READ limitation, but it also
   suggests that we may want a less-strict fs-read-disabling option
   which allows JSON (since JSON is known to not allow execution of
   foreign code nor loading of non-JSON content).
*/
int whcl__cb_json_parse_file( cwal_callback_args const * args, cwal_value **rv );

/**
   Returns true if either of:

   - tok->subscriptId is non-0 and the referenced token has type
   TOK1_T_PropAccess. That structure is how we mark the LHS of
   property access.

   - tok->ttype is TOK1_T_PropAccessWith. This is an LHS-less form of
   property access. It is structured identically to normal property
   access but has no tok->subscriptId because the post-tokenization
   process won't let us restructure them the same as normal property
   access without an LHS token to bind to. Instead it refers to the
   X part of .X as tok->innerId.

   else returns false.
*/
bool whcl__token_has_prop(whcl_script const * const ct,
                          whcl_stoken const * const tok);

/**
   Returns ttype if it is a whcl_stoken::ttype (not ttype2!) value
   which is (at least in principle) syntactically legal for use as an
   LHS of a property access operation (`X[Y]`), else returns 0.
*/
int whcl__ttype_legal_prop_lhs(int ttype);

/**
   Returns true if the given token is legal as an LHS of a property
   access op. It works like whcl__ttype_legal_prop_lhs() but also
   checks that tok->subscriptId is 0, as the framework currently
   requires that in order to transform an LHS token to a
   property-accessing one.

   tok may be NULL.

   This function's determination is not definitive. It is up to the
   caller to ensure that the given token directly touches the RHS
   token in question (see whcl_stokens_touch()).
*/
bool whcl__t_legal_prop_lhs(whcl_stoken const * const tok);

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
cwal_midsize_t whcl__strftime(char *dest, cwal_midsize_t destLen,
                              const char *format, const struct tm *timeptr);


/**
   Uses cwal_error_set() to set el's cwal_engine's error state with
   the current token position of ct to create an error message, using
   the given token (if not NULL) as the error position. Returns the
   new error code on succcess (the passed-in code or CWAL_RC_OOM). If
   code is 0 then an unspecified default error code is used.

   If t is NULL then ct's current error token (if set) or current
   token position is used for reporting the error location.
*/
int whcl__errv( whcl_engine * const el, whcl_script * ct,
                       whcl_stoken const * const t,
                       int code, char const * fmt, va_list args );

/**
   Variadic counterpart of whcl__errv().
*/
int whcl__err( whcl_engine * const el, whcl_script * const ct,
                    whcl_stoken const * const t,
                    int code, char const * fmt, ... );
/**
   Works like whcl__errv() but then transforms the error into a
   cwal-level exception in el's cwal_engine. Returns CWAL_RC_EXCEPTION
   on error, or CWAL_RC_OOM if an allocation fails.
*/
int whcl__throwv( whcl_engine * const el, whcl_script * ct,
                  whcl_stoken const * const t,
                  int code, char const * fmt, va_list args );
/**
   Variadic counterpart of whcl__throwv().
*/
int whcl__throw( whcl_engine * const el, whcl_script * const ct,
                 whcl_stoken const * const t,
                 int code, char const * fmt, ... );

/**
   Bitmask of values for use with whcl_engine::cache::installAPI.
   Each one is for use by a single whcl_install_xx() function, which
   should set its flag on whcl_engine::cache::installAPI the first
   time it is called (if the call succeeds) and should check that flag
   on each subsequent call, making the call a no-op if the flag is
   set.
*/
enum whcl__install_api_e {
WHCL__INSTALL_API_core =   1,
WHCL__INSTALL_API_fs =     1 << 1,
WHCL__INSTALL_API_os =     1 << 2,
WHCL__INSTALL_API_ob =     1 << 3,
WHCL__INSTALL_API_json =   1 << 4,
WHCL__INSTALL_API_pf =     1 << 5,
WHCL__INSTALL_API_require =1 << 6,
WHCL__INSTALL_API_script  =1 << 7,
WHCL__INSTALL_API_time =   1 << 8,
WHCL__INSTALL_API_tmpl =   1 << 9
};

/**
   Duplicates the first n bytes of s, using cwal_strlen() to find its
   length if n is negative. Returns a new string on success, NULL on
   OOM. The returned memory must eventually be freed via cwal_free().
*/
char * whcl__strdup(cwal_engine * const e, char const * s, cwal_int_t n);

/**
   Installs various filesystem-related APIs which are largely
   platform-dependent into `whcl[fs]`.

   Returns 0 on success, CWAL_RC_OOM on OOM, and possibly some
   script-generated error code if script-based init code fails.  If
   called multiple times, calls after the first are no-ops.  If it
   fails on the first call, results are undefined if it is called
   a second time.

   Functions installed by this:

   chdir, getcwd, mkdir, realpath, stat, file-accessible,
   dir-accessible, pushd, popd.
*/
int whcl__install_fs( whcl_engine * const el );

/**
   Works identically to whcl__install_fs() except that it installs a
   different set of functions related to the JSON API, installing them
   insto `whcl[json]`. See that function for the semantics.

   Functions installed by this:

   `Object parse(String|Buffer jsonString)`

   `Object parse-file(String filename)`

   `string stringify(Value [, int indentation=0])`

   None of these functions depend on a specific "this" object, so may
   be copied to other contexts, e.g. to members of other objects.
*/
int whcl__install_json( whcl_engine * const el );


/**
   Works identically to whcl__install_fs() except that it installs a
   different set of functions related to the OB (output buffering)
   API, adding them as `whcl[ob]`. See that function for the semantics.

   Function(s) installed by this:

   push, pop, get-string, take-string, take-buffer, clear,
   flush, capture

*/
int whcl__install_ob( whcl_engine * const el );

/**
   Works identically to whcl_install_fs() except that it installs a
   different set of functions, primarily related to the OS/operating
   environtment, installing them as `whcl[os]`. See that function for
   various semantics.

   Function(s) installed by this:

   getenv
*/
int whcl__install_os( whcl_engine * const el );

/**
   Installs the PathFinder class into the whcl_whcl_object(): its
   constructor as `whcl[PathFinder]`.

   Returns 0 on success and
   CWAL_RC_OOM if allocating any component fails.

   If this function fails, calling it a second time results in
   undefined behaviour. If the first call succeeds, calling it
   again is a harmless no-op.

   The prototype object associated with the installed constructor
   can be manipulated via whcl_prototype_pf().

   Achtung: this installation depends on both whcl_install_os()
   and whcl_install_argv() having succeeded. This function cannot
   automatically handle whcl_install_argv() because it lacks the
   argugments needed for that function, but it wil install
   the former if needed.

   Reminder to self: the exposing of its prototype as
   `whcl[prototypes][PathFinder]` is "historical" (but currently
   required) and might go away. The preferred way to access the
   prototype from script code is via `whcl[PathFinder][__prototypes]`.
*/
int whcl__install_pf( whcl_engine * const el );

/**
   Works identically to whcl__install_fs() except that it installs a
   different set of functions, primarily related to the time handling,
   installing them as `whcl[time]`. See that function for various
   semantics.

   Function installed by this:

   sleep, time, mssleep, mstime, strftime

   With the caveat that some of those might not be available on the
   current platform/build, in which case they will throw an exception
   when called.
*/
int whcl__install_time( whcl_engine * const el );

/**
   Works identically to whcl__install_fs() except that it installs a
   single function named `tmpl`, installing it as `whcl[tmpl]`. See
   that function for various semantics.

   The installed function is whcl_cb_tmpl_to_code().
*/
int whcl__install_tmpl( whcl_engine * const el );

/**
   Installs the math API using the module-loading interface.
*/
int whcl__install_math( whcl_engine * const el, cwal_value ** rv );

#endif /* #include guard */
