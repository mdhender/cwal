/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

/**
   s2_c9n ("compilation") is an EXPERIMENT in converting s2's
   tokenization process to use "pre-compiled" chains of tokens. This
   is what s2's predecessor did, but that approach was abandoned for
   s2 for the notable memory savings of evaluating only 1 token at a
   time. This approach is, however, computationally much faster to
   evaluate.

   It's not yet clear whether a compiled form of s2_cptoker and
   friends can be "hidden" behind the older/uncompiler s2_ptoker
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
#ifndef NET_WANDERINGHORSE_CWAL_S2_C9N_H_INCLUDED_
#define NET_WANDERINGHORSE_CWAL_S2_C9N_H_INCLUDED_
#include "s2.h"
/* reminder to self: s2.h is only used for s2_engine,
   which is only used for error reporting because of its
   infrastructure for s2_ptoker error position handling. */

#ifdef __cplusplus
extern "C" {
#endif
typedef struct s2_cptoker s2_cptoker;
typedef struct s2_cptoken s2_cptoken;

#if 0
typedef struct s2_byte_range s2_byte_range;
/**
   Holds a pair of pointers indicating a range
   to an abstract string data source.
*/
struct s2_byte_range {
  /**
     The starting position of source.
  */
  char const * begin;
  /**
     One-past-the-end position.
  */
  char const * end;
};
/**
   Empty s2_byte_range instance, intended for const-copy-initializing.
*/
#define s2_byte_range_empty_m {0,0}

/**
   Empty s2_byte_range instance, intended for copy-initializing.
*/
extern const s2_byte_range s2_byte_range_empty;
#endif

/**
   A "compiled token" for the s2 t10n framework. Unlike its
   predecessor, s2_ptoken, this token type is intended to be
   heap-allocated en mass, tokenized a single time, and traversed any
   number of times. s2_ptoken, by contrast, is intended to be used
   from the stack, with only a few of them, at most, active in any
   given parsing-related stack frame. The purpose of this type is to
   eventually provide a more performant alternative, as profiling
   shows that s2's tokenize-as-you-go-and-retokenize-often model is
   overwhelmingly the single largest time-sink in s2 evaluation time.

   In addition, this type would be suitable for serializing long-term,
   allowing saving and loading of pre-tokenized scripts.

   In trying to keep the memory size down, this type imposes some
   range limits which s2_ptoken historically did not, but which have
   never been violated in client-side s2 code before. It also uses
   numbers in places where s2_ptoken uses pointers, to avoid paying
   unecessarily for 8-byte pointers and to enable us to more easily
   (de)serialize these.
*/
struct s2_cptoken{
  /**
     The id of this token in the parent token chain array. We need
     this as a transition step between s2_ptoker/s2_ptoken and
     s2_cptoker/s2_cptoken. An id of 0 indicates "no token"
     and positive value indicates the 1-based index into
     the a s2_cptoker::chain array (i.e. id of 1 is the 0th
     element in that array).
  */
  s2_token_id id;

  /**
     A value from the s2_token_types enum.
  */
  int16_t ttype;

  /**
     Token length, in bytes.
  */
  s2_linecol_t length;

  /**
     Some tokens have 2 lengths: the length of the whole token and
     the length of the "useful" part. e.g. heredocs. This member is
     the offset, from this->begin, at which the "useful" part starts.
  */
  s2_linecol_t innerOffset;

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
  s2_linecol_t innerLength;

  /**
     1-based line number.
  */
  s2_linecol_t line;

  /**
     0-based column number.
  */
  s2_linecol_t column;

  /**
     The starting byte pos of the token, relative to its containing
     tokenizer. Invalid tokens have begin and end values of 0, but so
     do tokens which encompass a completely empty script.
  */
  uint32_t begin;

  /**
     The ID of the next token in the parent tokenizer's token
     chain. Normally this would be token (this->id+1), but certain
     post-compilation handling may change that, e.g. converting "a-b"
     into an identifier in some contexts. A value of 0 that this token
     is the last in its chain (or branch of the chain).

     For "supertokens", e.g. (parens) and [braces] and {squigglies},
     this will point to the token after the closing element of the
     block, and this->innerId will point to the first token inside
     the block.
    
  */
  s2_token_id nextId;

  /**
     For "supertokens", e.g. (parens) and [braces] and {squigglies},
     this is the ID of the first token inside the block. A value of 0
     means that this is not a block-level token. Empty supertokens
     have this set to an EOF token (the token which maps to the
     closing token of the supertoken gets re-mapped to an EOF).
  */
  s2_token_id innerId;
};

/**
   Empty-initialized s2_ptoken structure, intended for const-copy
   initialization.
*/
#define s2_cptoken_empty_m {\
    s2_token_id_none/*id*/,\
    S2_T_INVALID/*ttype*/,\
    0/*length*/,0/*innerOffset*/,0/*innerLength*/,\
    1/*line*/,0/*column*/,\
    0/*begin*/,s2_token_id_none/*nextId*/,s2_token_id_none/*innerId*/}

/**
   Empty-initialized s2_ptoken structure, intended for copy
   initialization.
*/
extern const s2_cptoken s2_cptoken_empty;

/**
   AN ONGOING/INCOMPLETE EXPERIMENT. DO NOT USE!

   The s2_cptoker class is a simple basis for a tokenizer, largely
   syntax- and language-independent. Its origins go back many years
   and several projects.

   This tokenizer requires that all input be available in advance of
   tokenization and remain valid for its own lifetime.

   Reminders to self regarding eventual support for (de)serializing
   these...

   An output stream could look something like:

   [binary file header]
   [cptoker, with all relevant fields]
   [cptoken chain]
   [raw script source]

   Even "compiled", we still need the source.

   Some considerations for serialization:

   - Loss of relationship to parent tokenizers

   - cptokers know nothing about sub-tokenizers. Those would
   need to be reconstructed on demand, if needed (e.g. when evaluating
   a function, as those each store their own sub-tokenizer).
*/
struct s2_cptoker {
  /**
     The s2 engine used for memory management and error reporting.
  */
  s2_engine * se;
  /**
     Starting position of input.

     The full input range is [begin, end).
  */
  char const * begin;
  /**
     One-past-the-end position of the input (i.e. the position where
     the NUL byte normally is). It must be >= this->begin.  When
     tokenization gets to this point, the tokenization process returns
     an EOF token (ttype of S2_T_EOF).
  */
  char const * end;

  /**
     This is only used internally for bubbling certain compile-time
     error messages through the call chain so that they can be
     ammended with script/line/column info.

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
  s2_cptoker const * parent;
  /**
     Internal optimization: the top-most tokenizer in a
     sub-(sub-...)tokenizer stack. This MUST be set properly for
     sub-tokenizers. The top-most tokenizer is the one
     which owns this->chain.
  */
  s2_cptoker const * top;

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
  s2_linecol_t lineOffset;

  /**
     The column counterpart of lineOffset. Note that it only applies
     when the location being reported is on line 1 of the child
     tokenizer. After the first line, the "inherited" column offset is
     irrelevant.
  */
  s2_linecol_t colOffset;

  /**
     Flags which may change how this object tokenizes.
  */
  uint16_t flags;
  
  /**
     The current token's ID.
  */
  s2_token_id token;

  /**
     The put-back token's ID.
  */
  s2_token_id pbToken;

  /**
     The ID of the current error position token, used in error
     reporting.
  */
  s2_token_id errToken;

  /**
     An optimization which tells s2_cptoker_next_token() to use this
     explit token.

     This may not be needed - it's a bit of a holdover from s2_ptoker,
     where it is used to reduce re-tokenization after lookaheads.
     That retokenization does not happen in this tokenizer.
     Nonetheless, we'll keep it around as a s2_ptoker compatibility
     bit, if for nothing else.
  */
  s2_token_id nextToken;

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
     An array of tokens allocated by s2_cptoken_compile(). The array
     has this->chainLength entries. For empty chains, we still have a
     single entry - an S2_T_EOF token.

     If this->parent is non-NULL, this member refers to the start
     of the sub-chain for which this sub-tokenizer is responsible.
     This is used only for traversing block constructs a.k.a. supertokens.

     Internal note: the token IDs in this chain are their array index
     +1. Testing indicated that having 0-based IDs (which mapped
     directly to array indexes) was more trouble than it was worth
     because it made it easy to accidentally loop back to the start of
     the chain when hitting an EOF/virtual EOF and left us with mixed
     semantics for the ID 0. Now ID 0 is always the token-ID
     equivalent of NULL/not-a-token.
  */
  s2_cptoken * chain;

  /**
     The s2 eval engine "captures" ranges of tokens for various
     purposes. In this class that is modeled by token IDs in the
     half-open range [begin,end).
  */
  struct {
    /* First token in a captured block. */
    s2_token_id begin;
    /* One-after-the-end token in a captured block. */
    s2_token_id end;
  } capture;
  
  /**
     Tells us whether this instance was allocated using
     s2_cptoker_alloc() or not.
   */
  void const * allocStamp;

  /**
     For building a singly-linked list for recycling purposes.
  */
  s2_cptoker * next;
  
  /**
     Potential TODO: add a (s2_cptoken chainStatic[X]) which we use,
     rather than this->chain, for chains which require <=X tokens,
     where X is maybe 10 or 15. The concrete use case would be
     single-expression bodies of loops and if/else, which are atypical
     tokenization cases.
  */
};
/** Empty-initialized s2_cptoker object. */
#define s2_cptoker_empty_m {                        \
    0/*se*/,                                        \
    0/*begin*/,0/*end*/,                          \
    0/*errMsg*/,                                  \
    0/*name*/,                                    \
    0/*parent*/,                                  \
    0/*top*/,                                \
    0/*lineOffset*/,0/*colOffset*/,               \
    0/*flags*/,  \
    s2_token_id_none/*token*/,                  \
    s2_token_id_none/*pbToken*/,                \
    s2_token_id_none/*errToken*/,               \
    s2_token_id_none/*nextToken*/ ,             \
    0/*chainLength*/,0/*chainCapacity*/,        \
    NULL/*chain*/,                              \
    {/*capture*/0/*begin*/,0/*end*/},           \
    NULL/*allocStamp*/,                       \
    NULL/*next*/                              \
}
/** Empty-initialized s2_ptoker object. */
extern const s2_cptoker s2_cptoker_empty;

/**
   Flags for use with s2_cptoker::flags and/or
   s2_cptoker_compile().
*/
enum s2_cptoker_flags {
/**
   Sentinel value. Must be 0.
*/
S2_CPTOKER_F_NONE = 0,

/**
   If set, the '-' character is considered a legal identifier
   character except when the '-' appears at the start of a token.
*/
S2_CPTOKER_F_IDENTIFIER_DASHES = 1,

/**
   Tells the tokenizer to retain "junk" tokens (whitespace, comments)
   when tokenizing. Why on earth one would really want to do this is a
   riddle.

   Note that the tokenizer must retain *some* newlines, as they are
   syntactically relevant for a few cases. If the tokenizer can
   reliably determine which ones to disregard, it will do so.
*/
S2_CPTOKER_F_RETAIN_JUNK = 2
};

/**
   Returns the length of k's string part. If inner is true, the
   token's "inner" length is returned if the token has one, otherwise
   its entire length is returned. If inner is false, the token's whole
   length is returned. The inner length is only valid for the following block-level
   token types:

   - (...parens groups...) (S2_T_ParenGroup)
   - [...braces groups...] (S2_T_BraceGroup)
   - {...squiggly groups...} (S2_T_SquigglyBlock)
   - <<< X heredocs X (S2_T_Heredoc)

   For such tokens, the inner and outer length will differ, with the
   inner referring to the whole token length minus the open/closing
   parts of the token and (with one exception) leading/trailing spaces
   of that part. For heredocs the inner length refers only to the part
   inside of the heredoc identifier pair, and how much, if any,
   leading/trailing space that includes depends on whethere the
   heredoc uses the ':' modifier or not. In all such cases, the inner
   length will always be at least 2 less than the outer length.
*/
s2_linecol_t s2_cptoken_len( s2_cptoken const * k, int inner );

/**
   Returns a pointer to the start of the given token, which must be a
   valid token with the given tokenizer. If len is non-NULL, the
   length of the token is written there (note that tokens almost never
   NUL-terminated, so having their length is important for many
   purposes). As a special case, if has a virtual length of 0 (e.g.
   k->ttype is S2_T_EOF or refers to an empty heredoc) then a pointer
   to static memory (an empty string) is returned, rather than bytes
   encompassed by ct.

   @see s2_cptoken_cstr()
*/
char const * s2_cptoken_cstr( s2_cptoker const * ct,
                              s2_cptoken const * k, cwal_size_t * len );

/**
   Works exactly like s2_cptoken_cstr() except that if k refers to
   a block-level token then the string refers only to its "inner"
   bytes.

   @see s2_cptoken_cstr()
   @see s2_cptoken_len()
*/
char const * s2_cptoken_cstr2( s2_cptoker const * ct,
                               s2_cptoken const * k, cwal_size_t * len );

/**
   Allocates a new s2_cptoker instance, which must eventually be
   passed to s2_cptoker_free() to free it. Returns NULL on allocation
   error. The returned instance must be passed to s2_cptoker_init()
   before it is used, and that routine must be passed the same
   s2_engine instance as was passed here.
*/
s2_cptoker * s2_cptoker_alloc( s2_engine * se );

/**
   If cp is not NULL, passes cp to s2_cptoker_finalize() and then, if
   cp was allocated via s2_cptoker_alloc(), free's cp. If cp is not
   NULL and does not appear to have been allocated via that routine,
   this call is equivalent to s2_cptoker_finalize().
*/
void s2_cptoker_free( s2_cptoker * cp );

/**
   Initializes an s2_cptoker instance, after which it is ready to
   "compile" and tokenize input.

   The caller is obligated to eventually pass ct to
   s2_cptoker_finalize().

   This routine currently always returns 0 as it has no error
   conditions, but that may change, in which case it will return non-0
   (a CWAL_RC_xxx value) on error.

   @see s2_cptoker_compile()
   @see s2_cptoker_finalize()
*/
int s2_cptoker_init( s2_engine * se, s2_cptoker * ct );

/**
   If parent's current token is a block-level token, this routine
   initializes ct to wrap up that block.

   Returns 0 on success. Returns CWAL_RC_MISUSE if parent is not
   currently at a token and CWAL_RC_TYPE if that token is not a
   supertoken.

   Preconditions:

   - parent must be compiled and currently be at a block-level
   supertoken, else CWAL_RC_TYPE is returned.

   - ct must be "pristine", i.e. frestly allocated s2_cptoker_alloc()
   or stack-allocated and copy-initialized from s2_ptoker_empty, or
   results are undefined.

   Postconditions:

   - parent must outlive ct and must not be recompiled, or its input source
   or token chain otherwise modified, so long as ct is alive.

   - ct must be passed s2_cptoker_finalize() or (depending on how it
   was allocated) s2_cptoker_free() when the caller is done with it.

   Misc. notes:

   - A sub-tokenizer does not require any dynamic memory of its own:
   it simply navigates a part of the memory owned by the parent.

   - A sub-tokenizer may be initialized from another sub-tokenizer.

   - Sub-tokenizer initialization is a constant-time operation,
   requiring no logic more complex than pointer arithmetic.
*/
int s2_cptoker_sub_from_group( s2_cptoker const * parent, s2_cptoker * ct );

/**
   Must be passed an initialized (via s2_cptoker_init()) s2_cptoker and
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

   Errors are reported via the tokenizer's associate s2_engine
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

   - S2_CPTOKER_F_IDENTIFIER_DASHES
   - S2_CPTOKER_F_RETAIN_JUNK

   Pass a flags value of 0 to compile without any flags set.

   Use s2_cptoker_next_token() to fetch the next token and
   s2_cptoker_putback() to put a just-fetched token back.

   Implementation notes:

   - The tokenizer does not know which language constructs are valid,
   so only fails if any *tokens* are invalid, noting that this engine
   treats (), [], and {} blocks as single tokens (which may also, in
   this case of s2_cptoker/s2_cptoken, contain other tokens).

   @see s2_cptoker_init()
   @see s2_cptoker_compile_ptoker()
   @see s2_cptoker_compile_buffer()
   @see s2_cptoker_finalize()
*/
int s2_cptoker_compile( s2_cptoker * ct, char const * src, cwal_int_t len,
                        uint32_t compileFlags );

/**
   Functions like s2_cptoker_compile(), but takes its input range from
   an existing s2_ptoker instance.

   It uses psrc->name for error reporting, when appropriate (which is
   the primary advantage over using s2_cptoker_compile()). Before
   returning, ct->name is cleared, so the caller must re-assign it if
   it will be needed. (This routine can't do it because it doesn't
   know the lifetime of psrc->name.)

   This does not associate psrc with ct. It sets up ct to use the same
   byte range used by psrc and tokenizes that whole byte range into a
   chain of parsed tokens, ready for evaluation. For proper (eventual)
   operation, psrc will need to be internally associated with ct and
   will need to reset its tokenization process so that it is in sync
   with ct (if the two tokenizers do not stay in perfect sync chaos
   ensues).

   TODO: when/if s2_ptoker can wrap a s2_cptoker, this will need to be
   modified to, for sub-tokenizers, effectively run
   s2_cptoker_sub_from_group() and will require that psrc's current token be
   a supertoken.
*/
int s2_cptoker_compile_ptoker( s2_cptoker * ct, s2_ptoker const * psrc, uint32_t flags );

/**
   Convenience wrapper around s2_cptoken_compile() which compiles the
   given buffer's contents or an empty string (if the buffer is
   empty). The tokenizer will refer directly to the given buffer's
   memory, and it must not be modified or reallocated/deallocated for
   so long as ct is active.
*/
int s2_cptoker_compile_buffer( s2_cptoker * ct, cwal_buffer const * buf,
                               uint32_t compileFlags );

/**
   Frees all resources owned by ct, but does not free ct.

   It is safe to pass a non-s2_cptoker_init()'ialized instance if, and
   only if, the argument was copy-initialized from s2_cptoker_empty
   (or equivalent).
*/
void s2_cptoker_finalize( s2_cptoker * ct );

/**
   Returns the top-most object from t->parent, or t if !t->parent.
*/
s2_cptoker const * s2_cptoker_top( s2_cptoker const * t );

/**
   Returns either t->name or the first name encountered while climbing
   up the t->parent chain. Returns 0 if no name is found. If len is
   not 0 then if this function returns non-0, len is set to that
   name's length.
*/
char const * s2_cptoker_name_first( s2_cptoker const * t, cwal_size_t * len );

/**
   Fetches the next token from t. t must have been successfully
   intialized using s2_cptoker_init().

   Returns 0 on success. Because token chains are pre-parsed, the only
   error conditions involve the given tokenizer being invalid.

   If tgt is not NULL, a pointer to the resulting token is copied to
   *tgt.

   The current token may be fetched via s2_cptoker_token(). If
   tgt is not NULL, calling that function immediately after this
   one succeeds will return the same value as was stored in
   *tgt.

   See s2_cptoker_token() for information about the lifetime of
   pointers assigned to *tgt.

   Once the end of the token chain (or the end of any traversed
   sub-path) has been reached, each call to this function will resolve
   to a token with the type S2_T_EOF. s2_cptoker_reset() can be used
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
     requires using sub-tokenizers: see s2_cptoker_sub_from_group().

   Example usage:

   @code
   s2_cptoker * ct = ...;
   s2_cptoken const * tok = 0;
   while( 1 ){
     s2_cptoker_next_token(ct, &tok);
     assert(tok && "So long as ct is valid, this will always be true.");
     if(s2_ttype_is_eof(tok->ttype)) break;
     ... do something with tok ...
     if(tok->innerId) {
       // grouping token: traverse that recursively if needed
       // by using s2_cptoker_sub_from_group() to initialize a
       // sub-tokenizer
     }
   }
   @endcode
*/
int s2_cptoker_next_token( s2_cptoker * ct, s2_cptoken const ** tgt );

/**
   Returns a pointer to ct's current token, or NULL if
   s2_cptoker_next_toker() has not yet been called on ct. Its memory
   is owned by ct and its state remains unmodified until ct is
   finalized or ct's token chain is modified (which would not happen
   during most normal operations).
*/
s2_cptoken const * s2_cptoker_token(s2_cptoker const * ct);

/**
   If ct has a putback token (a call to s2_cptoker_next_token() has
   succeeded), the current token is "put back" (unconsumed), the
   putback token is cleared, and the ID of the put-back token is
   returned. Thus if s2_cptoker_next_token() is called, then this is
   called, the next call to s2_cptoker_next_token() will return the
   same token as the first.

   On a single level of putback is supported.
*/
s2_token_id s2_cptoker_putback(s2_cptoker * ct);

/**
   Sets ct's current token to the one referred to by the given
   argument. The token is expected to have previously been fetched
   using s2_cptoker_token() or similar.

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
int s2_cptoker_token_set( s2_cptoker * ct, s2_cptoken const * tok );

/**
   The error-token counterpart of s2_cptoker_token_set(). The error
   token is used by error reporting routines for specifying where an
   error happened. This routine does not trigger an error.
*/
int s2_cptoker_errtoken_set( s2_cptoker * ct, s2_cptoken const * tok );

/**
   The putback-token counterpart of s2_cptoker_token_set(). It would
   be highly unusual for a client to need to set the putback token to
   a specific value, but s2's eval engine does that in at least one
   place, so here it is.
*/
int s2_cptoker_pb_set( s2_cptoker * ct, s2_cptoken const * tok );

/**
   An internal optimization which tells ct to use the given token on
   the next call to s2_cptoker_next_token().

   See s2_cptoker_token_set() for the argument and return value
   semantics.
*/
int s2_cptoker_next_set( s2_cptoker * ct, s2_cptoken const * tok );

/**
   Returns ct's token at the given 1-based token chain index, or NULL
   if ndx is out of bounds. An index value of 0 is reserved for "no
   token."
*/
s2_cptoken const * s2_cptoker_at(s2_cptoker const * ct, s2_token_id ndx);

/**
   Sets the error state of ct->se, as for s2_engine_err_set(), and
   returns that call's result value (i.e. the 2nd argument on success
   or a lower-level code on error). If fmt is NULL or !*fm, ct->errMsg
   is used (if set) as the message. For the error position it tries to
   use the s2_engine::opErrPos (if set) or ct's current error position
   (or, failing that, current position). The report will include
   script file/line/column information from the ct object.

   Returns the error code, not 0, on success! Returns some other
   non-0 code on error.

   This routine takes pains not to allocate any memory, which also
   means not generating an error message, if the given error code is
   CWAL_RC_OOM.
*/
int s2_err_cptoker(s2_cptoker const * ct,
                   int code, char const * fmt, ... );

/**
   Similar to s2_err_cptoker(), but clears se's error state and sets
   se's Exception state.

   One exception (as it were) is if the code is CWAL_RC_OOM, in which
   case se's error state is set but no exception is thrown and no
   formatted message is generated because doing either would require
   allocating memory.

   If the 2nd argument is 0, it uses a code of CWAL_RC_EXCEPTION
   instead.
*/
int s2_throw_cptoker(s2_cptoker const * ct, int code,
                     char const * fmt, ... );

/**
   If tok is a block-level token of ct and contains only
   junk/whitespace tokens then 0 is returned, else the token type of
   the first non-junk/space token is returned.

   If tok is NULL then ct's current token is used instead.
*/
int s2_cptoken_block_has_content( s2_cptoker const * ct,
                                  s2_cptoken const * tok );

/**
   Resets the given tokenizer such that the next call to
   s2_cptoker_next_token() will start back at the tokenizer's initial
   starting point.
*/
void s2_cptoker_reset( s2_cptoker * ct );

/**
   Returns S2_T_EOF if tok is of that type, else returns 0.
*/
int s2_cptoken_is_eof( s2_cptoken const * tok );

#ifdef __cplusplus
}/*extern "C"*/
#endif

#endif
/* include guard */
