/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/**
  License: same as cwal. See cwal.h resp. cwal_amalgamation.h for details.

  This file holds s2-internal declarations and types. These are not
  intended for client-side use.
*/
#ifndef NET_WANDERINGHORSE_CWAL_S2_INTERNAL_H_INCLUDED_
#define NET_WANDERINGHORSE_CWAL_S2_INTERNAL_H_INCLUDED_
/** @file */ /* for doxygen */
#include "libs2.h"

#if !defined(DOXYGEN)
#ifdef __cplusplus
extern "C" {
#endif

/** @internal

   S2_UNUSED_VAR exists only to squelch, in non-debug builds, warnings
   about the existence of vars which are only used in assert()
   expressions (and thus get filtered out in non-debug builds).
*/
#define S2_UNUSED_VAR __attribute__((__unused__))
/**
   S2_UNUSED_ARG is used to squelch warnings about interface-level
   function parameters which are not used in a given implementation:

   @code
   int some_interface(type1 arg1, type2 arg2){
     S2_UNUSED_ARG arg1;
     ...
     return 0;
   }
   @code
*/
#define S2_UNUSED_ARG (void)

typedef struct s2_op s2_op;

/** @internal

   Internal state for Object.eachProperty() and similar functions.
*/
struct s2_kvp_each_state {
  /** Interpreter engine for the visitor. */
  cwal_engine * e;
  /** The 'this' for the visitor. */
  cwal_value * self;
  /** The function to call for each iteration of the visit. */
  cwal_function * callback;
  /** If true, the callback gets the value as the first arg. */
  char valueArgFirst;
};
typedef struct s2_kvp_each_state s2_kvp_each_state;
/**
   An empty-initialized s2_kvp_each_state instance, intended for
   copying over new instances to cleany initialize their state.
 */
extern const s2_kvp_each_state s2_kvp_each_state_empty;


/** @internal
   Allocates a new token using se's cwal-level allocator. The value
   must eventually be cleaned up using s2_stoken_free(). Returns 0 on
   allocation error (e.g. if !se or !se->e).
*/
s2_stoken * s2_stoken_alloc( s2_engine * se );

/** @internal
   Convenience form of s2_stoken_alloc() which sets a token's type
   and value. type may be any value and v may be NULL.
*/
s2_stoken * s2_stoken_alloc2( s2_engine * se, int type, cwal_value * v );

/** @internal
   "frees" the given token. If allowRecycle is false, or if se's
   token recycling feature is disabled or has reached its capacity,
   then t is immediate freed using se's allocator, otherwise t will
   be placed into a recycling bin for later re-use via
   s2_stoken_alloc().

   TODO: see if we really use that 3rd argument, and remove it if we
   don't.
*/
void s2_stoken_free( s2_engine * se, s2_stoken * t, char allowRecycle );

/** @internal
   Pushes t to the given stack and transfers ownership of t
   to ts.
*/
void s2_stoken_stack_push( s2_stoken_stack * ts, s2_stoken * t );

/** @internal
   If ts has any tokens, the top-most one is removed and that token
   is returned. Ownership of the returned value is transfered to the
   caller. Returns 0 on error (!ts or ts is empty).
*/
s2_stoken * s2_stoken_stack_pop( s2_stoken_stack * ts );

/** @internal
   Frees all entries owned by ts by popping each one and passing it
   to s2_stoken_free(se, theToken, allowRecycle). See s2_stoken_free()
   for the semantics of the se and allowRecycle parameters.
*/
void s2_stoken_stack_clear( s2_engine * se, s2_stoken_stack * ts, char allowRecycle );

/** @internal
   Works like s2_stoken_stack_clear(), but operates on both stacks owned by st.
*/
void s2_estack_clear( s2_engine * e, s2_estack * st, char allowRecycle );

/** @internal
   Swaps the contents of lhs and rhs (neither may be NULL).
*/
void s2_estack_swap( s2_estack * lhs, s2_estack * rhs );

/** @internal
   Pushes the given token to se's value stack and transfers its
   ownership to se. The only error condition is if either argument is
   0 or points to invalid memory.

   Reminder to self: this API does not internally add/remove refs to
   s2_stokens. This works, but only accidentally. It "should" be
   refactored to take a ref when pushing a value, unhand(?) when
   popping a value at the client's request, and unref when cleaning up
   on its own without the client having taken ahold of
   s2_stoken::value.
*/
void s2_engine_push_valtok( s2_engine * se, s2_stoken * t );

/** @internal
   The operator-stack counterpart of s2_engine_push_valtok().
*/
void s2_engine_push_op( s2_engine * se, s2_stoken * t );

/** @internal
   Creates a new token with the given type, appends it to se,
   and returns it. Return 0 on allocation error.
*/
s2_stoken * s2_engine_push_ttype( s2_engine * se, int i );

/** @internal
   Pushes a new token of type S2_T_Value to se, and assigns v to
   that token's value. It does not change the reference count or
   ownership of v.

   Returns 0 on allocation error or if se or v are 0. On success
   it returns the token pushed onto the stack.
*/
s2_stoken * s2_engine_push_val( s2_engine * se, cwal_value * v );

/** @internal
   Creates a new cwal_value value of type integer and pushes it onto
   se as described for s2_engine_push_val(). Returns 0 on allocation
   error, or the pushed token on success.
*/
s2_stoken * s2_engine_push_int( s2_engine * se, cwal_int_t i );

/** @internal
   Pushes a new token onto VALUE stack, with the given type and
   the given value (which may be NULL but likely should not be).
*/
s2_stoken * s2_engine_push_tv( s2_engine * se, int ttype, cwal_value * v );

/** @internal
   If se's stack contains any tokens, the top-most token is removed.
   If returnEntry is false then the token is placed in se's recycle
   bin and 0 is returned. If returnEntry is true, the token is
   returned to the caller, as is ownership of that token. If se has
   no stack entries, 0 is returned.

*/
s2_stoken * s2_engine_pop_token( s2_engine * se, char returnEntry );


/** @internal
   Similar to s2_engine_pop_token(), this destroys the top-most
   token and returns its value (if any), transfering ownership (or
   stewardship) of that value to the caller.
*/
cwal_value * s2_engine_pop_value( s2_engine * se );

/** @internal
   The operator-stack counterpart of s2_engine_pop_token().
*/
s2_stoken * s2_engine_pop_op( s2_engine * se, char returnEntry );

/** @internal
   Returns the top-most token in se's token stack, without modifying
   the stack. Returns 0 if se's token stack is empty.
*/
s2_stoken * s2_engine_peek_token( s2_engine * se );

/** @internal
   Equivalent to s2_engine_peek_token(se)->value, but returns 0
   (instead of segfaulting) if the value stack is empty. Does not
   modify ownership of the returned value.
*/
cwal_value * s2_engine_peek_value( s2_engine * se );

/** @internal
   Swaps the contents of se->st with st. Neither argument
   may be NULL.
*/
void s2_engine_stack_swap( s2_engine * se, s2_estack * st );

/** @internal
   Clears all entries from se->st, recycling them if possible,
   freeing them if not.
*/
void s2_engine_reset_stack( s2_engine * se );

/** @internal

    Runs cwal_engine_vacuum() on se->e. DO NOT CALL THIS
    unless you are 100% absolutely sure you want to and that it is safe
    to do so (which is very dependent on local conditions).
*/
void s2_engine_vacuum( s2_engine * se );

/** @internal
   The op-stack counterpart of s2_engine_peek_token().
*/
s2_stoken * s2_engine_peek_op( s2_engine * se );

/** @internal
   Pushes t onto either the token stack or (if it represents an
   operator) the operator stack.

   @see s2_ttype_op()
   @see s2_stoken_op()
*/
void s2_engine_push( s2_engine * se, s2_stoken * t );

/** @internal
   Processes the top-most operator on the stack and pushes the
   result value back onto the stack.

   [i don't think this is true anymore:] A small number of "marker"
   operators do not generate a result, and do not push a result onto
   the stack, but leave the value stack as they found it. It is not
   yet clear whether or not this API needs to provide a way for
   clients to know about that. Currently those operators are handled
   directly by the higher-level parser code or are handled as part of
   another operation (e.g. the S2_T_MarkVariadicStart token marks the
   end of N-ary call ops).

   Returns 0 on success, non-0 on error. On error, the stack state
   is not well-defined unless CWAL_RC_TYPE is returned (indicating
   that the top token is not an operator).
*/
int s2_process_top( s2_engine * se );

/** @internal
   A lower-level form of s2_process_top(), this variant does not
   modify the operator stack. It is assumed that op is one which was
   just popped from it by the caller, or "would have" become the top
   of the stack but the push is being elided as an optimization.

   It is up to the caller to ensure that se's stack is set up
   appropriately for the given op before calling this.

   Returns 0 on success, a non-0 CWAL_RC_xxx value on error.
*/
int s2_process_op( s2_engine * se, s2_op const * op );

/** @internal
   Equivalent to s2_process_op(se, s2_ttype_op(ttype)), except that
   it returns CWAL_RC_TYPE if ttype is not an operator.
*/
int s2_process_op_type( s2_engine * se, int ttype );

/** @internal
   Clears any pending tokens from se's stack(s).
*/
void s2_engine_reset_stack( s2_engine * se );

/** @internal

    Post-processor for converting a single "opener" token into
    a larger token containing the whole braced group, writing
    the resulting token to the given output token pointer.

    Requires st->token to be a S2_T_SquigglyOpen, S2_T_BraceOpen, or
    S2_T_ParenOpen token. It "slurps" the content part, from the
    starting brace up to (and include) the ending brace. It uses
    s2_next_token() to parse the group, meaning that it must
    contain tokenizable code. We cannot simply do a byte-scan through
    it because doing so would exclude any constructs (e.g. strings)
    which might themselves contain tokens which would look like
    closing tokens here.

    If !out, then st->token is used as the output destination. If out
    is not NULL then after returning, st->token and st->pbToken will
    have their pre-call state (regardless of error or success).

    Returns 0 on success. On error it will set se's error state.

    On success:

    - out->ttype is set to one of: S2_T_SquigglyBlock,
      S2_T_ParenGroup, S2_T_BraceGroup.

    - [out->begin, out->end) will point to the whole token, including
    its leading and trailing opener/closer tokens. That range will
    have a length of at least 2 (one each for the opening and closing
    token). [out->adjBegin, out->adjEnd) will point to the range
    encompassing the body of the open/close block, stripped of any
    leading or trailing whitespaces. That range may be empty.

*/
int s2_slurp_braces( s2_engine *se, s2_ptoker * st,
                     s2_ptoken * out );


/** @internal

    Post-processor for S2_T_HeredocStart and S2_T_HeredocStart2
    tokens. se is the interpreter, st is the current tokenizer
    state. tgt is where the results are written. If tgt is NULL,
    st->token is used.

    Requires that st->token be a S2_T_HeredocStart or
    S2_T_HeredocStart2 token. This function scans st for the opening
    and closing heredoc tokens.

    On error a non-0 CWAL_RC_xxx value is returned and se's error
    state will be updated. On error st->token is in an unspecified
    state.

    On success tgt->ttype is set to S2_T_SquigglyBlock and
    [out->begin, out->end) will point to the whole token proper,
    including its leading and trailing opener/closer tokens. That
    range will have a length of at least 2 (one each for the opening
    and closing token). [out->adjBegin, out->adjEnd) will point to
    the range encompassing the body of the open/close block, stripped
    of any leading or trailing spaces, as specified below. That range
    may be empty.

    Returns 0 on success and sets se's error state on error. 

    Syntax rules:

    Assuming that the S2_T_HeredocStart token is '<<<', heredocs
    can be constructed as follows:

    <<<EOF blah blah blah EOF
    <<<EOF blah blah blahEOF // equivalent!
    <<<'EOF' ... 'EOF'
    <<<"EOF" ... "EOF"
    <<<:EOF ... EOF // colon changes space-skipping rules

    Alternately, the S2_T_HeredocStart2 syntax is:

    ```
    {{{ blah blah blah }}}
    {{{: blah blah blah }}} // colon works the same as above
    ```

    That form generally plays better with existing syntax highlighting
    and auto-indention modes.

    Failure to find a match for the heredoc identifier will result in
    an error. Identifiers may optionally be quoted, but both the
    opener and closer must use the same quote character.

    Anything between the opening and closing identifiers belongs to
    the heredoc body, but leading/trailing whitespace gets trimmed as
    follows:

    Space-skipping rules:

    - If the first character after the heredoc token is a colon,
    then the heredoc skips exactly 1 newline or space from the beginning
    and end of the heredoc.

    - Otherewise all leading and trailing whitespace characters are
    trimmed from the result.

    Note that the result token's [begin,end) range spans the _whole_
    heredoc, whereas [adjBegin,adjEnd) encompasses the heredoc body.
    i.e. trimming changes the token's adjBegin and adjEnd ranges.

*/
int s2_slurp_heredoc( s2_engine * se, s2_ptoker * st,
                      s2_ptoken * tgt );


/** @internal

    If an exception is pending AND it has not yet been decorated
    with script location information, that is done here. If pr is
    not 0 then it is used for the location information, else
    se->currentScript is used. If se->currentScript is also 0, or no
    error location is recorded somewhere in se->opErrPos or the
    script chain then this function has no side effects.

    Returns 0 for a number of not-strictly-error conditions:

    - !pr && !se->currentScript

    - if there is no pending exception, or if that exception value
    may not have properties (it is not a container type)

    - OR if the pending exception already contains location
    information.
*/
int s2_exception_add_script_props( s2_engine * se, s2_ptoker const * pr );

/** @internal

   Like s2_add_script_props2(), but calculates the script name, line, and column
   as follows:

   - The name comes from s2_ptoker_name_top()

   - The script code position (presumably an error location) comes
   from s2_ptoker_err_pos_first().

*/
int s2_add_script_props( s2_engine * se, cwal_value * v, s2_ptoker const * pr );

/** @internal

   If v is a container, this adds the properties "script", "line",
   and "column" to v, using the values passed in. If scriptName is
   NULL or empty, it is elided.  Likewise, the line/column
   properties are only set if (line>0).

   If se has stack trace info, it is also injected into v. Potential
   TODO: separate this routine into a couple different bits. The
   current behaviour assumes that v is (or will be in a moment) an
   Exception.

   @see s2_count_lines()
*/
int s2_add_script_props2( s2_engine * se, cwal_value * v,
                          char const * scriptName, int line, int col);

/** @internal

    Internal cwal_value_visitor_f() implementation which requires
    state to be a (cwal_array*). This function appends v to that array
    and returns the result.
*/
int s2_value_visit_append_to_array( cwal_value * v, void * state );


/** @internal
   State for script-side functions. Each script function gets
   one of these attached to it.
*/
struct s2_func_state {
  /**
     The source code for this function. For "empty" functions this
     will be NULL. It gets rescoped along with the containing Function
     value.
  */
  cwal_value * vSrc;

  /**
     An Object which holds all "imported" symbols associated with this
     function via the "using" pseudo-keyword when defining a function
     or via Function.importSymbols(). When this function gets called,
     all of these symbols get imported into the call()'s scope.

     20190911: it "might be interesting" to add a keyword, or even a
     local variable, to access this list from inside a
     function. Slightly tricky would be to only resolve that symbol
     from inside the body of the function it applies to (similar to
     how the break keyword only works from within a loop).

     20191209: the using() keyword now provides access to this. Part
     of that change is that this value's prototype is removed. We've
     never needed it in native code and remove it now so that script
     code does not inadvertently do anything silly with it or trip
     over inherited properties.
  */
  cwal_value * vImported;

  /**
     The name of the function, if any, gets defined as a scope-local
     symbol when the function is called, just like this->vImported
     does.
  */
  cwal_value * vName;

  /**
     A hashtable key for the script's name. This key points to a
     string in se->funcStash, and is shared by all functions which
     have the same script name. Unfortunately, those names must
     currently stay in the hash for the life of the interpreter, but
     it is not as though that that poses any real problem other than
     (potentially) unused script names being kept in in the hash. The
     problem is that we don't know when it's safe to remove the entry
     (don't know when the last function-held ref to it is gone, and
     string interning can confuse the matter). A proper fix (i.e.
     which does not keep these strings permanently owned by the global
     scope) requires keeping the name and a counter (number of
     functions using that name).

     TODO?: now that we have rescopers for functions, we could have
     each function store/rescope this itself, without stashing it.
     String interning might make this cheap (and might not - filenames
     can be long and long strings aren't interned). That could get
     expensive for utilities which return lots of small functions
     (e.g. utils which bind values for later use).
  */
  cwal_value * keyScriptName;

  /**
     For the recycling subsystem: this is the next item in the
     recycling list.
  */
  s2_func_state * next;

  /**
     The line (1-based) of the function declaration. Used for
     adjusting script location information.
  */
  uint16_t line;

  /**
     The column (0-based) of the function declaration. Used for
     adjusting script location information.
  */
  uint16_t col;

  /* Internal flags. */
  cwal_flags16_t flags;

};

/**
   Empty-initialized s2_func_state instance. Used as a type ID
   in a few places.
*/
extern const s2_func_state s2_func_state_empty;

/** @internal

   Internal, experimental, and incomplete.

   Intended to be called at various (many) points during s2's
   evaluation process, and passed the (CWAL_RC_xxx) result code of the
   local, just-run operation. If that code is a fatal one, it returns
   that code without side effects. If it is not a fatal one then this
   function returns CWAL_RC_INTERRUPTED _if_ s2_interrupt() has been
   called, else it returns localRc.

   If passed 0 and it returns non-0 then s2_interrupt() was called.

   i.e. a "really bad" result code trumps both localRc and
   the is-interrupted check, and the is-interrupted check trumps
   localRc.

   The "really bad" codes (which are returned as-is) include:
   CWAL_RC_FATAL, CWAL_RC_EXIT, CWAL_RC_ASSERT, CWAL_RC_OOM,
   CWAL_RC_INTERRUPTED.

   TODO (2021-06-26): strongly consider moving the is-interrupted
   check into the cwal core.
*/
int s2_check_interrupted( s2_engine * se, int localRc );

/** @internal

    If f is an s2 script function then its s2 state part
    is returned. The state is owned by s2 and MUST NOT
    be modified or held for longer than f is.
*/
s2_func_state * s2_func_state_for_func(cwal_function * f);

/** @internal

   Looks for a constructor function from operand, _not_ looking in
   prototypes (because ctors should generally not be inherited).

   If pr is not NULL then it is assumed to be the currently-evaluating
   code and will be used for error reporting purposes if errPolicy !=
   0.

   errPolicy determines error handling (meaning no ctor found):

   0 = return error code but set no error state

   <0 = return error code after throwing error state (so the result
   will be CWAL_RC_EXCEPTION unless a lower-level (allocation) failure
   trumps it).

   >0 = return error code after setting non-exception error state.


   If a valid ctor is found:

   - if rv is not NULL, *rv is set to the ctor (in all other cases
   *rv is not modified).

   - 0 is returned.

   The error code returned when no viable ctor found is found is
   CWAL_RC_NOT_FOUND, but the errorPolicy may translate that to
   an exception, returning CWAL_RC_EXCEPTION.
*/
int s2_ctor_fetch( s2_engine * se, s2_ptoker const * pr,
                   cwal_value * operand, cwal_function **rv,
                   int errPolicy );



/** @internal

   Saves some of se's internal state to the 2nd parameter and
   resets it to its initial state in se. If this is called, the
   caller is obligated to call s2_engine_subexpr_restore(),
   passing the same two parameters, before control returns to
   an outer block.
*/
void s2_engine_subexpr_save(s2_engine * se, s2_subexpr_savestate * to);

/** @internal

   Restores state pushed by s2_engine_subexpr_save().
*/
void s2_engine_subexpr_restore(s2_engine * se, s2_subexpr_savestate const * from);

/** @internal

   Flags for use with s2_scope_push_with_flags(). They must fit in a
   uint16_t.
*/
enum s2_scope_flags {
/**
   Sentinel entry. Must be 0.
*/
S2_SCOPE_F_NONE = 0,
/**
   This tells s2_scope_push_with_flags() to retain any current
   s2_engine::ternaryLevel, rather than saving/resetting it when
   pushing and restoring it when popping. It is intended for "inlined"
   (non-block) expressions which push a scope, so that in-process
   ternary op handling can DTRT with a ':' in the RHS of such
   expressions. e.g. (foo ? catch someFunc() : blah). Without this
   flag, the catch expression will try to consume the ':' and fail
   because it doesn't see that there's a ternary-if being
   processed. This flag makes scope/catch/etc. aware that a ':' is
   part of a pending ternary op. Note that (foo ? catch
   {someFunc():blah()}), where the ":" is inside {...} still triggers
   a syntax error.
*/
S2_SCOPE_F_KEEP_TERNARY_LEVEL = 0x0001,

/**
   Not yet quite sure what this is for. It might go away.
*/
S2_SCOPE_F_IS_FUNC_CALL = 0x0002
};

/** @internal

    This is equivalent to calling cwal_scope_push(), then setting
    the given flags on the new s2_scope which gets added to the
    stack via the cwal_engine's scope push hook.

    Returns 0 on success. On error non-0 is returned (a CWAL_RC_xxx
    code) then pushing of the scope has failed (likely CWAL_RC_OOM)
    and something is Seriously Wrong.
*/
int s2_scope_push_with_flags( s2_engine * se, cwal_scope * tgt, uint16_t flags );


/** @internal

   Returns se's current s2-level scope, or 0 if no scope is
   active. ACHTUNG: the returned pointer can be invalidated by future
   pushes/pops of the cwal stack, so it MUST NOT be held on to across
   push/pop boundaries. Rather than holding an (s2_scope*) across
   push/pop boundaries, call this as needed to get the current
   pointer.

   @see cwal_scope_push()
   @see cwal_scope_pop()
   @see cwal_scope_pop2()
*/
s2_scope * s2_scope_current( s2_engine const * se );


/** @internal

   Returns the s2-level scope for the current cwal scope level, or
   NULL if level is out of bounds. cwal scoping starts at level 1 and
   increases by 1 for each new scope popped on the stack. i.e passing
   this a level of 1 will return the top-most scope. The current
   scoping level can be found in s2_engine::scopes::level or
   via the 'level' member of the cwal_scope object returned by
   cwal_scope_current_get().

   Note that the scopes have very strict lifetime rules, and clients
   must never modify their state or clean them up or anything silly
   like that.

   The cwal_scope counterpart of the returned object can be obtained
   via its cwalScope member.

   Scope levels are managed by cwal, but s2 keeps its own scopes
   (s2_scope) for holding s2-specific scope-level state. These are
   kept in sync via the cwal-level scope push/pop hooks.

   IMPORTANT: the returned pointer can be invalidated by future pushes
   onto, and pops from, the scope stack, and thus it must never be
   held across scope-push/pop boundaries. The poitner should be
   fetched as needed and then discard.

   @see cwal_scope_push()
*/
s2_scope * s2_scope_for_level( s2_engine const * se, cwal_size_t level );

/** @internal

   Sets up se's internals so that it knows the most recent dot-op
   self/lhs/key (or is not, if passed NULLs for the last 3
   arguments). If self is not NULL then self is considered to be
   "this" in the corresponding downstream handling (i.e. it's a
   property access which binds "this"), else it does not. self is
   always lhs or 0. lhs is either the container for a property lookup
   (if self!=0) or a scope var lookup. key is the associated property
   key.

   The property/scope variable operations set these up and several
   places in the eval engine (namely function calls) consume and/or
   clear/reset them (to avoid leaving stale entries around).

   Pass all 0's to reset it.

   Reminder to self: before calling this, MAKE SURE you've got ref to
   any local values which are about to returned/passed along. If
   needed, ref before calling this and unhand afterwards. If a value
   about to be returned/upscoped is one of
   se->dotOpThis/dotOp.key/dotOp.lhs then that ref will save the app's
   life.
*/
void s2_dotop_state( s2_engine * se, cwal_value * self,
                     cwal_value * lhs, cwal_value * key );


/** @internal

   An internal operator/keyword-level helper to be passed the result
   code from the s2_set() family of funcs, to allow those routines to
   report error information, including location, for the failure
   point.  Returns rc or a modified form of it when moving
   s2_engine::err state to an exception.

   If rc is not 0 then s2_throw_err_ptoker() may (with a small few
   exceptions) may be called to propagate any error info, in which
   case se->err is assumed to have been set up by s2_set() (and
   friends). Some few error codes are simply returned as-is, without
   collecting error location info: CWAL_RC_OOM, CWAL_RC_EXCEPTION,
   CWAL_RC_NOT_FOUND.

   If !pr then se->currentScript is used. If both are NULL, or !rc,
   this is a no-op.
*/
int s2_handle_set_result( s2_engine * se, s2_ptoker const * pr, int rc );

/** @internal

   The getter analog of s2_handle_set_result().
*/
int s2_handle_get_result( s2_engine * se, s2_ptoker const * pr, int rc );


/** @internal
   Callback function for operator implementations.
     
   The framework will pass the operator, the underlying engine, the
   number of operatand it was given (argc).

   Preconditions:

   - se's token stack will contain (argc) tokens intended
   for this operator.

   - The top of se operator stack will not be this operator
   (or not this invocation of it).

   - rv will not be NULL, but may point to a NULL pointer.

   Implementation requirements:

   - Implementations MUST pop EXACTLY (argc) tokens from se's
   token stack before returning or calling into any API which
   might modify se's token stack further. They must treat those
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

   - If se->skipLevel is greater than 0, then the operator must pop
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
typedef int (*s2_op_f)( s2_op const * self, s2_engine * se,
                        int argc, cwal_value **rv );

/** @internal
   Represents a stack machine operation. Each operation in s2 equates
   to a shared/const instance of this class.
*/
struct s2_op {
  /**
     Generic symbolic representation, not necessarily what appears
     in script code. Need not be unique, either. Primarily intended
     for use in debugging and error messages.
  */
  char const * sym;

  /**
     Operator type ID. Must be one of the s2_token_types
     values.
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
  s2_op_f call;

  /**
     Not yet used.

     An experiment in inferring compound comparison operators from
     simpler operators, e.g. infering '<' and '==' from '<=' and using
     them if the latter op is not available but the former two are.

     inferLeft is the LHS op id, inferRight is the RHS id.
  */
  int inferLeft;
  /**
     Not yet used.
  */
  int inferRight;

  /**
     Not currently used. Intended to replace the inferLeft/inferRight
     bits (which also aren't currently used).

     For "assignment combo" operators (+=, -=, etc). s2 "should" be
     able to derive an implementation if the non-assignment part of
     that has been overloaded (e.g. if operator+ is overloaded, s2
     should be able to derive operator+= from that). We could (in
     theory) safely always evaluate such ops to their own "this"
     value, and wouldn't(?) even need to perform the actual assignment
     because such overloads always need to return their own "this" in
     order to work properly (and the overload presumably applies the
     operator to itself, as opposed to a new copy, which would break
     assignments). If someone really wants to return non-this, they
     could overload it explicitly, which would of course trump a
     derived version.

     Anyway... for assignment combo ops this "should" be the s2_op::id
     of the related non-assignment operator which "could" be used to
     automatically implement the combo op.
   */
  int derivedFromOp;
};

/** @internal
   Empty-initialized s2_op structure, intended for
   const-copy initialization. Only used internally.
*/
#define s2_op_empty_m {                         \
    0/*sym*/, 0/*id*/, 0/*arity*/,              \
    0/*assoc*/, 0/*prec*/,                    \
    0/*placement*/, 0/*call()*/,              \
    0/*inferLeft*/, 0/*inferRight*/,        \
    0/*derivedFromOp*/                    \
  }

#if 0
/** @internal
   Empty-initialized s2_op structure, intended for
   copy initialization.
*/
extern const s2_op s2_op_empty;
#endif

/** @internal
   If ttype (a s2_token_types value) represents a known Operator
   then that operator's shared/static/const instance is returned,
   otherwise NULL is returned.
*/
s2_op const * s2_ttype_op( int ttype );

/** @internal
   Equivalent to s2_stoken_op(t->type).
*/
s2_op const * s2_stoken_op( s2_stoken const * t );

int s2_op_is_math( s2_op const * op );
int s2_op_is_expr_border( s2_op const * op );
int s2_op_is_unary_prefix( s2_op const * op );

/** @internal
   Equivalent to s2_ttype_short_circuits(op ? op->id : 0).
*/
int s2_op_short_circuits( s2_op const * op );

/** @internal

    Rescopes, if needed, v to lhs's scope. Intended for use just after
    v is created and v is held on to by a native value associated with
    lhs (e.g. when lhs is bound to s2_func_state and v is one of
    s2_func_state::vSrc, vImported, or vName). When creating such
    values, it's almost always critical to adjust the new value's
    scope just after creating it. Once the underlying binding is in
    place, the native-level rescoper is responsible for future
    rescoping of the value.
*/
void s2_value_to_lhs_scope( cwal_value const * lhs, cwal_value * v);


/** @internal
   Intended to be passed the result of s2_set_v() (or similar).
   This routine upgrades certain result codes to engine-level
   error state and returns rc or some other error code. If !rc,
   it always returns rc.
*/
int s2_handle_set_result( s2_engine * se, s2_ptoker const * pr, int rc );


/** @internal
   Intended to be passed the result of s2_get_v() (or similar).
   This routine upgrades certain result codes to engine-level
   error state and returns rc or some other error code. If !rc,
   it always returns rc.
*/
int s2_handle_get_result( s2_engine * se, s2_ptoker const * pr, int rc );

/** @internal

   Fetches fst's "imported symbols" holder object, initializing it if
   needed (which includes rescoping it to theFunc's scope). theFunc
   _must_ be the Function Value to which fst is bound.

   If this function creates the imported symbols holder, its prototype
   is removed.

   Is highly intolerant of NULL arguments.

   Returns NULL on OOM, else the properties object (owned by fst
   resp. theFunc).
*/
cwal_value * s2_func_import_props( s2_engine * se,
                                   cwal_value const * theFunc,
                                   s2_func_state * fst );


/** @internal

    A kludge/workaround for the eval engine's lack of references on
    its being-eval'd values (string interning, in particular, can bite
    us, leading to cwal-level assert() failures when lifetime rules
    are violated). VERY LONG STORY made very short: this routine adds
    v (if it's not NULL and not a builtin value) to
    s2->currentScope->evalHolder (if that list is not NULL) to keep a
    reference to them.

    Returns 0 if it does nothing or if adding v to the list succeeds,
    non-0 on serious error (almost certainly CWAL_RC_OOM, as that's
    the only realistic error result).
    See s2_scope::evalHolder for more details.
*/
int s2_eval_hold( s2_engine * se, cwal_value * v );

/** @internal
   
    Flags for s2_func_state::flags.
*/
enum s2_func_state_flags_t {
S2_FUNCSTATE_F_NONE = 0,
/**
   Indicates that the function's parameter list contains no non-junk
   tokens, so it need not be evaluated at call()-time.
*/
S2_FUNCSTATE_F_EMPTY_PARAMS = 0x01,
/**
   Indicates that the function's body contains no non-junk tokens, so
   it need not be evaluated at call()-time.
*/
S2_FUNCSTATE_F_EMPTY_BODY = 0x02,
/**
   This flag indicates that the "using" clause of a function
   definition was tagged to indicate that any imported symbols (via
   "using" or Function.importSymbols()) should not be declared as
   call-local variables. Instead, they may be accessed in the function
   via the "using" keyword. e.g.:

   proc()using.{a: 1}{
    assert !typeinfo(islocal a);
    s2out << using.a << '\n';
   };

   Without the "." modifier, that assert would fail.

   Mnemonic: "using." is exactly how the symbols nee
*/
S2_FUNCSTATE_F_NO_USING_DECL = 0x04,
/**
   Reserved.
*/
S2_FUNCSTATE_F_CLASS = 0x08,
/**
   EXPERIMENTAL and likely to go away:

   When a function has this flag, if its body contains the
   string s2_engine::cache::keyInterceptee then this flag
   gets set.
*/
S2_FUNCSTATE_F_INTERCEPTEE = 0x10
};

typedef struct s2_keyword s2_keyword;

/** @internal

    Keyword handler signature.
   
*/
typedef int (*s2_keyword_f)( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);

/** @internal

    Holds state for a single keyword.
*/
struct s2_keyword {
  int /* non-const needed internally */ id;
  char const * word;
  unsigned short /*non-const needed for ukwd */ wordLen;
  s2_keyword_f call;
  /**
     If true, then LHS invocations of this keyword will, if they terminate
     in a {block}, treat an EOL immediately following as an EOX. This is
     to make 'if' and friends easier on the eyes, as nobody wants to be forced
     to add a semicolon to:

     if(...){

     };
  */
  char allowEOLAsEOXWhenLHS;
#if 0
  /**
     An internal detail of the ukwd bits. Doh - adding this would
     require editing the inlined initialization structs of all the
     built-in keywords :/. Time to go macro-fy that, it seems.
  */
  cwal_value * ukwd;
#endif
};

/**
   Structure for holding a list of "user keywords" (UKWDs, as they're
   colloquially known).
*/
struct s2_ukwd {
  /**
     List of keyword object.
  */
  s2_keyword * list;
  /**
     Hash of UKWD names to their values.
  */
  cwal_hash * h;
  /**
     Number of entries in this->list which are currently in use.
  */
  cwal_size_t count;
  /**
     Number of entries allocated for this->list.
  */
  cwal_size_t alloced;
};
typedef struct s2_ukwd s2_ukwd;

/** @internal

    Frees any memory held by se->ukwd, including se->ukwd itself.
*/
void s2_ukwd_free(s2_engine * se);

/** @internal

   A "perfect hasher" for s2 keywords. This algo uses a simple hashing
   mechanism which is known to produce collision-free hashes for the
   s2 built-in keywords and typeinfo/pragma operators.

   It hashes the first n bytes of src and returns the hash.

   We have a hash code limit of 32 bits because these values are used
   in switch/case statements, and we cannot portably use values >32
   bits as a case value.

   It returns 0 if the input length is 0 or the input contains any
   characters with a value less than '$' or greater than 'z', which
   includes all non-ASCII UTF8, noting that UKWDs are not hashed this
   way, so they may still contain any s2-legal identifier characters.

   Maintenance WARNING: this algo MUST be kept in sync with the one in
   s2-keyword-hasher.s2, as that script generates the C code
   for our keyword/typeinfo/pragma bits.
*/
uint32_t s2_hash_keyword( char const * src, cwal_size_t n );

#ifdef __cplusplus
}/*extern "C"*/
#endif
#endif /* DOXYGEN */

#endif
/* include guard */
