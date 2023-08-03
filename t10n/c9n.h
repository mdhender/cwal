/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

/**
   c9n ("compilation") is an EXPERIMENT in converting the venerable
   t10n tokenization process to use "pre-compiled" chains of
   tokens. This is what s2's predecessor did, but that approach was
   abandoned for s2 for the notable memory savings of evaluating only
   1 token at a time. This approach is, however, computationally much
   faster to evaluate.

   It's not yet clear whether a compiled form of t10n_toker and
   friends can be "hidden" behind the older/uncompiled t10n_toker
   interface. If so, great (especially if it can be turned on and off)
   but if not, it's unlikely to be integrated because doing so would
   require a total overhaul of s2's evaluation engine. An intermediary
   step of changing the eval engine to use a new tokenizer interface,
   which can function for both compiled and uncompiled forms, is a
   potential possiblity, but would still require a significant
   overhaul of s2's eval engine. If it would allow us to use both
   "traditional" and "compiled" tokens with the same interface,
   however, it would be worth it, providing us the best of both
   worlds: low memory cost for uncompiled tokens and the speed of
   compiled ones at the cost of significantly more memory.
*/
#ifndef NET_WANDERINGHORSE_CWAL_C9N_H_INCLUDED_
#define NET_WANDERINGHORSE_CWAL_C9N_H_INCLUDED_
#include "t10n.h"

#ifdef __cplusplus
extern "C" {
#endif
typedef struct c9n_toker c9n_toker;
typedef struct c9n_token c9n_token;

/**
   A "compiled token" for the cwal t10n framework. Unlike its
   predecessor, t10n_token, this token type is intended to be
   heap-allocated en mass, tokenized a single time, and traversed any
   number of times. t10n_token, by contrast, is intended to be used
   from the stack, with only a few of them, at most, active in any
   given parsing-related stack frame. The purpose of this type is to
   eventually provide a more performant alternative to token-based
   script evaluation, as profiling shows that s2's
   tokenize-as-you-go-and-retokenize-often model is overwhelmingly the
   single largest time-sink in s2 evaluation time.

   In addition, this type would be suitable for serializing long-term,
   allowing saving and loading of pre-tokenized scripts.

   In trying to keep the memory size down, this type imposes some
   range limits which t10n_token historically did not, but which have
   never been violated in client-side s2 code before. It also uses
   numbers in places where t10n_token uses pointers, to avoid paying
   unecessarily for 8-byte pointers and to enable us to more easily
   (de)serialize these. That also, not coincidentally, forces client
   code to use the API, instead of direct member access, to do
   anything useful with it.
*/
struct c9n_token{
  /**
     The id of this token in the parent token chain array. We use this
     for addressing elements within a compiled token chain, rather
     than pointers, as well as potential a transition step between
     t10n_toker/t10n_token and c9n_toker/c9n_token. An id of 0
     indicates "no token" and positive value indicates the 1-based
     index into the a c9n_toker::chain array (i.e. id of 1 is the 0th
     element in that array).

     This ID will always equal the token's array offset +1, but we
     store it nonetheless as a sanity-checking measure.
  */
  t10n_token_id id;

  /**
     A value from the t10n_token_types enum.
  */
  int16_t ttype;

  /**
     Token length, in bytes.
  */
  t10n_linecol_t length;

  /**
     Some tokens have 2 lengths: the length of the whole token and the
     length of the "useful" part. e.g. heredocs. This member is the
     offset, from this->begin, at which the "useful" part starts.  For
     heredoc and strings, the useful part is the part without the
     outer delimiters, noting that heredocs will strip all leading and
     trailing whitespace by default but may be told to strip
     differently.
  */
  t10n_linecol_t innerOffset;

  /**
     The counterpart of this->innerOffset, this holds the length, in
     bytes from innerOffset to the end of the "useful" part of the
     token. (begin+innerOffset+innerLength) must always be <=
     (begin+length).

     Reminder to self: We cannot remove this unless we also add
     innerEndOffset, as we have to store one or the other in order to
     calculate the inner range of compound tokens. The calculations
     are apparently easier this way and the memory cost is the same
     either way.
  */
  t10n_linecol_t innerLength;

  /**
     1-based line number. If 0, the current line/column are unknown or
     invalid.
  */
  t10n_linecol_t line;

  /**
     0-based column number. (Why 1-based lines but 0-based columns?
     Because emacs does it that way.)
  */
  t10n_linecol_t column;

  /**
     The starting byte pos of the token, relative to its containing
     tokenizer. Invalid tokens have begin and length values of 0, but
     so do tokens which encompass a completely empty script.
  */
  uint32_t begin;

  /**
     The ID of the next token in the parent tokenizer's token
     chain. Normally this would be token (this->id+1), but certain
     post-compilation handling may change that, e.g. converting "a-b"
     into an identifier in some contexts. A value of 0 indicates that
     this token is the last in its chain (or branch of the chain).

     For "supertokens", e.g. (parens) and [braces] and {squigglies},
     this will point to the token after the closing element of the
     block, and this->innerId will point to the first token inside
     the block.
  */
  t10n_token_id nextId;

  /**
     For "supertokens", e.g. (parens) and [braces] and {squigglies},
     this is the ID of the first token inside the block. A value of 0
     means that this is not a block-level token. Empty supertokens
     have this set to an EOF token (the token which maps to the
     closing token of the supertoken gets re-mapped to an EOF).
  */
  t10n_token_id innerId;
};

/**
   Empty-initialized c9n_token structure, intended for const-copy
   initialization.
*/
#define c9n_token_empty_m {\
    t10n_token_id_none/*id*/,\
    T10N_T_INVALID/*ttype*/,\
    0/*length*/,0/*innerOffset*/,0/*innerLength*/,\
    1/*line*/,0/*column*/,\
    0/*begin*/,t10n_token_id_none/*nextId*/,t10n_token_id_none/*innerId*/}

/**
   Empty-initialized c9n_token structure, intended for copy
   initialization.
*/
extern const c9n_token c9n_token_empty;

/**
   AN ONGOING/INCOMPLETE EXPERIMENT.

   The c9n_toker class is a simple basis for a tokenizer, largely
   syntax- and language-independent. Its origins go back many years
   and several projects.

   This tokenizer requires that all input be available in advance of
   tokenization and remain valid for its own lifetime.

   Reminders to self regarding eventual support for (de)serializing
   these...

   An output stream could look something like:

   [binary file header]
   [toker, with all relevant fields]
   [token chain]
   [raw script source]

   Even "compiled", we still need the source for our particular use
   cases. However, a c9n_toker does not own the script, so we'd need a
   higher-level construct which manages that ownership.

   Some considerations for serialization:

   - Loss of relationship to parent tokenizers.

   - Tokers know nothing about sub-tokenizers. Those would need to be
   reconstructed on demand, if needed (e.g. when evaluating a
   function, as those each store their own sub-tokenizer).
*/
struct c9n_toker {
  /**
     The cwal_engine used for memory management and error reporting.
  */
  cwal_engine * e;
  /**
     Starting position of input.

     The full input range is [begin, end).
  */
  char const * begin;
  /**
     One-past-the-end position of the input (i.e. the position where
     the NUL byte normally is). It must be >= this->begin.  When
     tokenization iteration gets to this point, the tokenization
     process returns an EOF token (ttype of T10N_T_EOF).
  */
  char const * end;

  /**
     This is only used internally for bubbling certain compile-time
     error messages through the call chain so that they can be
     ammended with script/line/column info. These bytes must be
     static/immutable.

     Normally set during the compilation process.
  */
  char const * errMsg;

  /**
     NUL-terminated string used for error reporting. May be a file
     name or a descriptive name like "eval script". The bytes are
     owned by "someone else" - not this object - and must outlive this
     object.
  */
  char const * name;

  /**
     Used for calculating line/col/script name info for sub-parsing
     errors, as well as for sharing overlapping sections of token
     chains between a parent and its children.
  */
  c9n_toker const * parent;

  /**
     Internal optimization: the top-most tokenizer in a
     sub-(sub-...)tokenizer stack. This MUST be set properly for
     sub-tokenizers. The top-most tokenizer is the one
     which owns this->chain.
  */
  c9n_toker const * top;

  /**
     For use in line-counting when working with tokenizers which "deep
     fork" off of others. This allows a standalone tokenizer to
     error-report line/column numbers relative to its original
     location in a higher-level tokenizer, even if the
     sub-tokenizer has migrated out of its parent (who may no longer
     exist). Primarily used to line up (as it were) function body
     line/column info with their origin script.

     This value is 1-based. A value of 0 indicates no offset (neither
     line nor column).
  */
  t10n_linecol_t lineOffset;

  /**
     The column counterpart of lineOffset. Note that it only applies
     when the location being reported is on line 1 of the child
     tokenizer. After the first line, the "inherited" column offset is
     irrelevant.
  */
  t10n_linecol_t colOffset;

  /**
     Flags which may change how this object tokenizes.
  */
  uint16_t flags;
  
  /**
     The current token's ID.
  */
  t10n_token_id token;

  /**
     The put-back token's ID.
  */
  t10n_token_id pbToken;

  /**
     The ID of the current error position token, used in error
     reporting.
  */
  t10n_token_id errToken;

  /**
     An optimization which tells c9n_toker_next_token() to use this
     explit token.

     This may not be needed - it's a bit of a holdover from t10n_toker,
     where it is used to reduce re-tokenization after lookaheads.
     That retokenization does not happen in this tokenizer.
     Nonetheless, we'll keep it around as a t10n_toker compatibility
     bit, if for nothing else.
  */
  t10n_token_id nextToken;

  /**
     The number of "used" tokens in the this->chain array. For
     sub-tokenizers this limits the tokenizer to a specific range
     within the parent.
  */
  uint32_t chainLength;

  /**
     The number of entries currently allocated in this->chain.
     For sub-tokenizers this is 0.
  */
  uint32_t chainCapacity;

  /**
     An array of tokens allocated by c9n_token_compile(). The array
     has this->chainLength entries. For empty chains, we still have a
     single entry - an T10N_T_EOF token.

     If this->parent is non-NULL, this member refers to the start of
     the sub-chain for which this sub-tokenizer is responsible.  This
     is used only for traversing block constructs a.k.a. supertokens.

     It is illegal to modify this chain (e.g. to re-tag token types)
     during evaluation unless the caller knows _precisely_ what
     they're doing and knows for certain that no calls higher in the
     call stack are making use of a section which is being modified.

     Internal note: the token IDs in this chain are their array index
     +1. Testing indicated that having 0-based IDs (which mapped
     directly to array indexes) was more trouble than it was worth
     because it made it easy to accidentally loop back to the start of
     the chain when hitting an EOF/virtual EOF and left us with mixed
     semantics for the ID 0. Now ID 0 is always the token-ID
     equivalent of NULL/not-a-token.
  */
  c9n_token * chain;

  /**
     The s2 eval engine "captures" ranges of tokens for various
     purposes. In this class that is modeled by token IDs in the
     half-open range [begin,end).
  */
  struct {
    /* First token in a captured block. */
    t10n_token_id begin;
    /* One-after-the-end token in a captured block. */
    t10n_token_id end;
  } capture;
  
  /**
     Tells us whether this instance was allocated using
     c9n_toker_alloc() or not. If so, c9n_toker_free() will free this
     object, otherwise it behaves like c9n_toker_finalize().
  */
  void const * allocStamp;

  /**
     For building a singly-linked list for recycling purposes.
  */
  c9n_toker * next;
  
  /**
     Potential TODO: add a (c9n_token chainStatic[X]) which we use,
     rather than this->chain, for chains which require <=X tokens,
     where X is maybe 10 or 15. The concrete use case would be
     single-expression bodies of loops and if/else, which are atypical
     tokenization cases.
  */
};
/** Empty-initialized c9n_toker object. */
#define c9n_toker_empty_m {                        \
    0/*se*/,                                        \
    0/*begin*/,0/*end*/,                          \
    0/*errMsg*/,                                  \
    0/*name*/,                                    \
    0/*parent*/,                                  \
    0/*top*/,                                \
    0/*lineOffset*/,0/*colOffset*/,               \
    0/*flags*/,  \
    t10n_token_id_none/*token*/,                  \
    t10n_token_id_none/*pbToken*/,                \
    t10n_token_id_none/*errToken*/,               \
    t10n_token_id_none/*nextToken*/ ,             \
    0/*chainLength*/,0/*chainCapacity*/,        \
    NULL/*chain*/,                              \
    {/*capture*/0/*begin*/,0/*end*/},           \
    NULL/*allocStamp*/,                       \
    NULL/*next*/                              \
}
/** Empty-initialized c9n_toker object. */
extern const c9n_toker c9n_toker_empty;

/**
   Flags for use with c9n_toker::flags and/or
   c9n_toker_compile().
*/
enum c9n_toker_flags_e {
/**
   Sentinel value. Must be 0.
*/
C9N_TOKER_F_NONE = 0,

/**
   If set, the '-' character is considered a legal identifier
   character except when the '-' appears at the start of a token.
*/
C9N_TOKER_F_IDENTIFIER_DASHES = 1,

/**
   Tells the tokenizer to retain "junk" tokens (whitespace, comments)
   when tokenizing. Why on earth one would really want to do this is a
   riddle. One marginally interesting use is when one wants to
   reconstruct a source script from its compiled form for testing
   purposes.

   Note that the tokenizer must retain *some* newlines, as they are
   syntactically relevant for a few cases. If the tokenizer can
   reliably determine which ones to disregard, it will do so.
*/
C9N_TOKER_F_RETAIN_JUNK = 2
};

/**
   Returns the length of k's string part. If inner is true, the
   token's "inner" length is returned if the token has one, otherwise
   its entire length is returned. If inner is false, the token's whole
   length is returned. The inner length is only valid for the following block-level
   token types:

   - (...parens groups...) (T10N_T_ParenGroup)
   - [...braces groups...] (T10N_T_BraceGroup)
   - {...squiggly groups...} (T10N_T_SquigglyBlock)
   - <<< X heredocs X (T10N_T_Heredoc)

   For such tokens, the inner and outer length will differ, with the
   inner referring to the whole token length minus the open/closing
   parts of the token and (with one exception) leading/trailing spaces
   of that part. For heredocs the inner length refers only to the part
   inside of the heredoc identifier pair, and how much, if any,
   leading/trailing space that includes depends on whethere the
   heredoc uses the ':' modifier or not. In all such cases, the inner
   length will always be at least 2 less than the outer length.
*/
t10n_linecol_t c9n_token_len( c9n_token const * k, bool inner );

/**
   Returns a pointer to the start of the given token, which must be a
   valid token with the given tokenizer. If len is non-NULL, the
   length of the token is written there (note that tokens almost never
   NUL-terminated, so having their length is important for many
   purposes). As a special case, if has a virtual length of 0 (e.g.
   k->ttype is T10N_T_EOF or refers to an empty heredoc) then a pointer
   to static memory (an empty string) is returned, rather than bytes
   encompassed by ct.

   If innerOnly is true and k refers to a block-level token, string
   literal, or a heredoc then the returned string and length
   correspond to its "inner" bytes: the content, minus the outer
   delimiters.

   @see c9n_token_cstr()
*/
char const * c9n_token_cstr( c9n_toker const * ct,
                               c9n_token const * k, cwal_size_t * len,
                               bool innerOnly);


/**
   Allocates a new c9n_toker instance, which must eventually be
   passed to c9n_toker_free() to free it. Returns NULL on allocation
   error. The returned instance must be passed to c9n_toker_init()
   before it is used, and that routine must be passed the same
   cwal_engine instance as was passed here.
*/
c9n_toker * c9n_toker_alloc( cwal_engine * e );

/**
   If cp is not NULL, passes cp to c9n_toker_finalize() and then, if
   cp was allocated via c9n_toker_alloc(), free's cp. If cp is not
   NULL and does not appear to have been allocated via that routine,
   this call is equivalent to c9n_toker_finalize().
*/
void c9n_toker_free( c9n_toker * cp );

/**
   Initializes an c9n_toker instance, after which it is ready to
   "compile" and tokenize input.

   The caller is obligated to eventually pass ct to
   c9n_toker_finalize().

   This routine currently always returns 0 as it has no error
   conditions, but that may change, in which case it will return non-0
   (a CWAL_RC_xxx value) on error.

   @see c9n_toker_compile()
   @see c9n_toker_finalize()
*/
int c9n_toker_init( cwal_engine * e, c9n_toker * ct );

/**
   If parent's current token is a block-level token, this routine
   initializes ct to wrap up that block.

   Returns 0 on success. Returns CWAL_RC_MISUSE if parent is not
   currently at a token and CWAL_RC_TYPE if that token is not a
   supertoken.

   Preconditions:

   - parent must be compiled and its iterator must currently be at a
   block-level supertoken (T10N_T_ParenGroup, T10N_T_BraceGroup, or
   T10N_T_SquigglyGroup), else CWAL_RC_TYPE is returned.

   - ct must be "pristine", i.e. frestly allocated c9n_toker_alloc()
   or stack-allocated and copy-initialized from c9n_toker_empty, or
   results are undefined.

   Postconditions:

   - parent must outlive ct and must not be recompiled, or its input source
   or token chain otherwise modified, so long as ct is alive.

   - ct must be passed c9n_toker_finalize() or c9n_toker_free() when
     the caller is done with it.

   Misc. notes:

   - A sub-tokenizer does not require any dynamic memory of its own:
   it simply navigates a part of the memory owned by the parent.

   - A sub-tokenizer may be initialized from another sub-tokenizer.

   - Sub-tokenizer initialization is a constant-time operation,
   requiring no logic more complex than pointer arithmetic.
*/
int c9n_toker_sub_from_group( c9n_toker const * parent, c9n_toker * ct );

/**
   Must be passed an initialized (via c9n_toker_init()) c9n_toker and
   its input source. If len is negative then the equivalent of
   strlen() is used to calculate its length. This function tokenizes
   the whole input into a "compiled form", eliding "junk" tokens
   (comments, most whitespace) but retaining EOLs because those are
   sometimes syntactically relevant.

   The input string must remain valid and unmodified for so long as
   the tokenizer is using it.

   Returns 0 on success, else:

   - CWAL_RC_MISUSE if !t, !src.

   - CWAL_RC_OOM on allocation error.

   - CWAL_RC_RANGE if invalid UTF8 is detected.

   - CWAL_SRC_SYNTAX if tokenization of a "complex" type fails, e.g.
   a mismatched (), [], {}, or heredoc.

   Errors are reported via the tokenizer's associate cwal_engine
   instance and may, depending on where they happen, refer to
   line/column information in the input range.

   It is legal to call this routine repeatedly on the same instance,
   noting that each call will free (and thereby invalidate) any tokens
   it previously managed. Thus it must never be recompiled if there
   are any sub-tokenizers deriving from it: doing so will result in
   undefined behaviour.

   If ct->name is set (or inherited via a parent tokenizer) then that
   information is included in error reporting.

   The following flags are supported for the final argument:

   - C9N_TOKER_F_IDENTIFIER_DASHES
   - C9N_TOKER_F_RETAIN_JUNK

   Pass a flags value of 0 to compile without any flags set.

   Use c9n_toker_next_token() to fetch the next token and
   c9n_toker_putback() to put a just-fetched token back.

   Implementation notes:

   - The tokenizer does not know which language constructs are valid,
   so only fails if any *tokens* are invalid, noting that this engine
   treats (), [], and {} blocks as single tokens (which may also, in
   this case of c9n_toker/c9n_token, contain other tokens).

   @see c9n_toker_init()
   @see c9n_toker_compile_ptoker()
   @see c9n_toker_compile_buffer()
   @see c9n_toker_finalize()
*/
int c9n_toker_compile( c9n_toker * ct, char const * src, cwal_int_t len,
                       uint32_t compileFlags );

/**
   Functions like c9n_toker_compile(), but takes its input range from
   an existing t10n_toker instance.

   It uses psrc->name for error reporting, when appropriate (which is
   the primary advantage over using c9n_toker_compile()). Before
   returning, ct->name is cleared, so the caller must re-assign it if
   it will be needed. (This routine can't do it because it doesn't
   know the lifetime of src->name.)

   This does not associate psrc with ct. It sets up ct to use the same
   byte range used by psrc and tokenizes that whole byte range into a
   chain of parsed tokens, ready for evaluation. For proper (eventual)
   operation, psrc will need to be internally associated with ct and
   will need to reset its tokenization process so that it is in sync
   with ct (if the two tokenizers do not stay in perfect sync chaos
   ensues).

   TODO: when/if t10n_toker can wrap a c9n_toker, this will need to be
   modified to, for sub-tokenizers, effectively run
   c9n_toker_sub_from_group() and will require that psrc's current token be
   a supertoken.
*/
int c9n_toker_compile_ptoker( c9n_toker * ct, t10n_toker const * psrc, uint32_t flags );

/**
   Convenience wrapper around c9n_token_compile() which compiles the
   given buffer's contents or an empty string (if the buffer is
   empty). The tokenizer will refer directly to the given buffer's
   memory, and it must not be modified or reallocated/deallocated for
   so long as ct is active. As a special case, if buf->mem is NULL, it
   is compiled as if it were an empty string living in static memory.
*/
int c9n_toker_compile_buffer( c9n_toker * ct, cwal_buffer const * buf,
                              uint32_t compileFlags );

/**
   Frees all resources owned by ct, but does not free ct.

   It is safe to pass a non-c9n_toker_init()'ialized instance if, and
   only if, the argument was copy-initialized from c9n_toker_empty
   (or equivalent).
*/
void c9n_toker_finalize( c9n_toker * ct );

/**
   Returns the top-most object from t->parent, or t if !t->parent.
*/
c9n_toker const * c9n_toker_top( c9n_toker const * t );

/**
   Returns either t->name or the first name encountered while climbing
   up the t->parent chain. Returns 0 if no name is found. If len is
   not 0 then if this function returns non-0, len is set to that
   name's length.
*/
char const * c9n_toker_name_first( c9n_toker const * t, cwal_size_t * len );

/**
   Fetches the next token from t. t must have been successfully
   intialized using c9n_toker_init().

   Returns 0 on success. Because token chains are pre-parsed, the only
   error conditions involve the given tokenizer being invalid.

   If tgt is not NULL, a pointer to the resulting token is copied to
   *tgt.

   The current token may be fetched via c9n_toker_token(). If
   tgt is not NULL, calling that function immediately after this
   one succeeds will return the same value as was stored in
   *tgt.

   See c9n_toker_token() for information about the lifetime of
   pointers assigned to *tgt.

   Once the end of the token chain (or the end of any traversed
   sub-path) has been reached, each call to this function will resolve
   to a token with the type T10N_T_EOF. c9n_toker_reset() can be used
   to re-start tokenization.

   Any tokenization error is assumed to be unrecoverable, and it is
   not normally useful to call this again (after an error) without
   re-initializing the tokenizer first.

   Things to be aware of:

   - In this framework, grouping constructs (parens, braces,
     squigglies) and heredocs are treated as single tokens (sometimes
     referred to as "supertokens"). When a tokenizer which is
     currently at such a token is told to go to the "next" token, it
     goes to the next sibling of that group construct, not the first
     token within that construct. Traversing grouping constructs
     requires using sub-tokenizers: see c9n_toker_sub_from_group().

   Example usage:

   @code
   c9n_toker * ct = ...;
   c9n_token const * tok = 0;
   while( 1 ){
     c9n_toker_next_token(ct, &tok);
     assert(tok && "So long as ct is valid, this will always be true.");
     if(t10n_ttype_is_eof(tok->ttype)) break;
     ... do something with tok ...
     if(tok->innerId) {
       // grouping token: traverse that recursively if needed
       // by using c9n_toker_sub_from_group() to initialize a
       // sub-tokenizer
     }
   }
   @endcode
*/
int c9n_toker_next_token( c9n_toker * ct, c9n_token const ** tgt );

/**
   Returns a pointer to ct's current token, or NULL if
   c9n_toker_next_toker() has not yet been called on ct. Its memory
   is owned by ct and its state remains unmodified until ct is
   finalized or ct's token chain is modified (which would not happen
   during most normal operations).
*/
c9n_token const * c9n_toker_token(c9n_toker const * ct);

/**
   If ct has a putback token (a call to c9n_toker_next_token() has
   succeeded), the current token is "put back" (unconsumed), the
   putback token is cleared, and the ID of the put-back token is
   returned. Thus if c9n_toker_next_token() is called, then this is
   called, the next call to c9n_toker_next_token() will return the
   same token as the first.

   On a single level of putback is supported.
*/
t10n_token_id c9n_toker_putback(c9n_toker * ct);

/**
   Sets ct's current token to the one referred to by the given
   argument. The token is expected to have previously been fetched
   using c9n_toker_token() or similar.

   This function uses only certain internal state of the given token
   and does not require that it live longer than this
   call. Specifically, only the token's tokenizer-internal address is
   copied. Any changes made to, e.g., its token type since it was
   fetched by the client are not retained (that may change once/if we
   actually get far enough to be able to use this in s2).

   If tok represents a valid token index in ct, this function returns
   a non-0 value (its index in the tokenizer chain), else it returns
   false (0) and has no side-effects.

   ACHTUNG: if ct is a sub-tokenizer, it is up to the caller to ensure
   that the given token is in fact a token from that sub-tokenizer's
   range of tokens. Giving it a token which is legal for its parent(s)
   but not for the subtokenizer will eventually cause grief.
*/
int c9n_toker_token_set( c9n_toker * ct, c9n_token const * tok );

/**
   The error-token counterpart of c9n_toker_token_set(). The error
   token is used by error reporting routines for specifying where an
   error happened. This routine does not trigger an error.
*/
int c9n_toker_errtoken_set( c9n_toker * ct, c9n_token const * tok );

/**
   The putback-token counterpart of c9n_toker_token_set(). It would
   be highly unusual for a client to need to set the putback token to
   a specific value, but s2's eval engine does that in at least one
   place, so here it is.
*/
int c9n_toker_pb_set( c9n_toker * ct, c9n_token const * tok );

/**
   An internal optimization which tells ct to use the given token on
   the next call to c9n_toker_next_token().

   See c9n_toker_token_set() for the argument and return value
   semantics.
*/
int c9n_toker_next_set( c9n_toker * ct, c9n_token const * tok );

/**
   Returns ct's token at the given 1-based token chain index, or NULL
   if ndx is out of bounds. An index value of 0 is reserved for "no
   token."
*/
c9n_token const * c9n_toker_at(c9n_toker const * ct, t10n_token_id ndx);

/**
   Sets the error state of ct->e, as for cwal_engine_err_set(), and
   returns that call's result value (i.e. the 2nd argument on success
   or a lower-level code on error). If fmt is NULL or !*fm, ct->errMsg
   is used (if set) as the message. For the error position it tries to
   use the t10n_engine::opErrPos (if set) or ct's current error position
   (or, failing that, current position). The report will include
   script file/line/column information from the ct object.

   Returns the error code, not 0, on success! Returns some other
   non-0 code on error.

   This routine takes pains not to allocate any memory, which also
   means not generating an error message, if the given error code is
   CWAL_RC_OOM.
*/
int c9n_err_toker(c9n_toker const * ct,
                    int code, char const * fmt, ... );

/**
   Similar to c9n_err_toker(), but clears se's error state and sets
   se's Exception state.

   One exception (as it were) is if the code is CWAL_RC_OOM, in which
   case se's error state is set but no exception is thrown and no
   formatted message is generated because doing either would require
   allocating memory.

   If the 2nd argument is 0, it uses a code of CWAL_RC_EXCEPTION
   instead.
*/
int c9n_throw_toker(c9n_toker const * ct, int code,
                    char const * fmt, ... );

/**
   If tok is a block-level token of ct and contains only
   junk/whitespace tokens then 0 is returned, else the token type of
   the first non-junk/space token is returned.

   If tok is NULL then ct's current token is used instead.
*/
int c9n_token_block_has_content( c9n_toker const * ct,
                                  c9n_token const * tok );

/**
   Resets the given tokenizer such that the next call to
   c9n_toker_next_token() will start back at the tokenizer's initial
   starting point.
*/
void c9n_toker_reset( c9n_toker * ct );

/**
   Returns T10N_T_EOF if tok is of that type, else returns 0.
*/
int c9n_token_is_eof( c9n_token const * tok );

#ifdef __cplusplus
}/*extern "C"*/
#endif

#endif
/* include guard */
