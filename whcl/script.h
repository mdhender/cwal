/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/**
   whcl_script is a whcl-internal API modeling a "compiled" script.
   This type and its APIs must remain opaque to client-side code.

   This API uses the venerable stack-based "tok1" API as its
   tokenization basis and builds memory-lighter variants of those
   tokens into dynamically-allocated chains.
   
*/
#ifndef NET_WANDERINGHORSE_CWAL_LCL_WHCL_SCRIPT_H_INCLUDED_
#define NET_WANDERINGHORSE_CWAL_LCL_WHCL_SCRIPT_H_INCLUDED_
#include "libwhcl.h"
#include "tok1.h"

#ifdef __cplusplus
extern "C" {
#endif
//decl'd in whcl.h: typedef struct whcl_whcl_script;
typedef struct whcl_stoken whcl_stoken;

/**
   A callback type for collecting tokens to feed a whcl_script. This
   level of abstraction is almost (but only almost) over-engineering,
   but is intended to allow us to potentially tokenize different parts
   of whcl code differently.

   The first argument to this callback is intended solely to
   facilitate error reporting: implementations should report
   errors via whcl__script_err().
*/
typedef int (*whcl_stoken_izer_f)(whcl_script * const ct,
                                  tok1_izer * const pt,
                                  void * state);

/**
   Represents a token ID in the tok1_en/whcl_stoken APIs. Values
   above 0 always represent valid IDs and most contexts treat 0 as EOF
   (often a "virtual" EOF for a block construct). It's tempting to use
   a 16-bit type for this, but the s2 amalgamated unit tests (as of
   20200106) use up about half of that range (2/3rds if we retain
   "junk" tokens), so it's easy to conceive of overflowing that.

   It's also somewhat tempting to use 18 bits (262144) for the ID and
   the remaining 14 (16348) for the token type. That would be a highly
   invasive change, though, affecting tons of whcl's code.
*/
typedef uint16_t whcl_stoken_id;

/**
   A sentinel whcl_stoken_id meaning "no ID." This "really should" be
   an extern const whcl_stoken_id, but we then cannot use it in struct
   initializers :/. Its value MUST be 0.
*/
#define whcl_stoken_id_none ((whcl_stoken_id)0)

/**
   A "compiled token" for the cwal tok1 framework. Unlike its
   predecessor, tok1_en, this token type is intended to be
   heap-allocated en mass, tokenized a single time, and traversed any
   number of times. tok1_en, by contrast, is intended to be used
   from the stack, with only a few of them, at most, active in any
   given parsing-related stack frame. The purpose of this type is to
   eventually provide a more performant alternative to token-based
   script evaluation, as profiling shows that s2's
   tokenize-as-you-go-and-retokenize-often model is overwhelmingly the
   single largest time-sink in s2 evaluation.

   In addition, this type would be suitable for serializing long-term,
   allowing saving and loading of pre-tokenized scripts.

   In trying to keep the memory size down, this type imposes some
   range limits which tok1_en historically did not, but which have
   never been violated in client-side s2 code before. It also uses
   numbers in places where tok1_en uses pointers, to avoid paying
   unecessarily for 8-byte pointers and to enable us to more easily
   (de)serialize these. That also, not coincidentally, forces client
   code to use the API, instead of direct member access, to do
   anything useful with it.
*/
struct whcl_stoken {
  /**
     The id of this token in the parent token chain array. We use this
     for addressing elements within a compiled token chain, rather
     than pointers, as well as potential a transition step between
     tok1_izer/tok1_en and whcl_script/whcl_stoken. An id of 0
     indicates "no token" and positive value indicates the 1-based
     index into the a whcl_script::chain array (i.e. id of 1 is the 0th
     element in that array).

     This ID will always equal the token's array offset +1, but we
     store it nonetheless as a sanity-checking measure.
  */
  whcl_stoken_id id;

  /**
     A value from the tok1_en_types enum.
  */
  int16_t ttype;

  /**
     A value from the tok1_en_types enum. Intended for use
     in applying a category or sub-type.

     Reminder to self: because of struct padding, adding this did not
     actually increase the sizeof(whcl_stoken), at least on 64-bit
     platforms.
  */
  int16_t ttype2;

  /**
     The starting byte pos of the token, relative to its containing
     tokenizer. Invalid tokens have begin and length values of 0, but
     so do tokens which encompass a completely empty script.

     Reminder to self (2022-03-01): 16 bits is not enough here, as we
     have s2 scripts well beyond that size. As of this writing, the s2
     amalgamated unit test script is 162kb with approximately 36k
     tokens. Thus a token count limit of 16 bits is fine for the time
     being.
  */
  uint32_t begin;

  /**
     Token length, in bytes.
  */
  whcl_linecol_t length;

  /**
     Some tokens have 2 lengths: the length of the whole token and the
     length of the "useful" part. e.g. heredocs. This member is the
     offset, from this->begin, at which the "useful" part starts.  For
     heredoc and strings, the useful part is the part without the
     outer delimiters, noting that heredocs will strip all leading and
     trailing whitespace by default but may be told to strip
     differently.
  */
  whcl_linecol_t innerOffset;

  /**
     The counterpart of this->innerOffset, this holds the length, in
     bytes from innerOffset to the end of the "useful" part of the
     token. (begin+innerOffset+innerLength) must always be <=
     (begin+length).

     For tokens with no "inner" part, innerLength will be the same as
     this->length.
  */
  whcl_linecol_t innerLength;

  /**
     1-based line number. If 0, the current line/column are unknown or
     invalid.
  */
  whcl_linecol_t line;

  /**
     0-based column number. (Why 1-based lines but 0-based columns?
     Because emacs does it that way.)
  */
  whcl_linecol_t column;

  /**
     The ID of the next token in the parent tokenizer's token
     chain. Normally this would be token (this->id+1), but certain
     post-compilation handling may change that, e.g. converting "a-b"
     into an identifier in some contexts. A value of 0 indicates that
     this token is the last in its chain (or branch of the chain).

     For "supertokens", e.g. (parens) and [braces] and {squigglies},
     this will point to the token after the closing element of the
     block, and this->innerId will point to the first token inside the
     block. Similarly subscript-style property access restructures
     tokens so that the access subscript is moved into
     this->subscriptId. In such cases, nextId is redirected to the
     token which follows the inner content and/or subscript access
     tokens, such that it "skips over" all child-level tokens.
  */
  whcl_stoken_id nextId;

  /**
     For "supertokens", e.g. (parens) and [braces] and {squigglies},
     this is the ID of the first token inside the block. A value of 0
     means that this is not a block-level token. Empty supertokens
     have this set to an EOF token (the token which maps to the
     closing token of the supertoken gets re-mapped to an EOF).

     See this->subscriptId for more info.
  */
  whcl_stoken_id innerId;

  /**
     For tokens which refer to property access of an LHS token, the
     tokens get restructured such that the accessor token is stored in
     subscriptId of the LHS token. We cannot use innerId for this
     purpose because that member is already used for other purposes. A
     value of 0 means the token has no subscript. It is legal for a
     token to have both innerId and subscriptId sub-tokens. For example:

     ```
     {X}[Y] Z
     ```

     Gets transformed into a single TOK1_T_SquigglyGroup token
     followed by an `Z` token. The `X` token(s) become a subchain of
     the squiggly: squiggly->innerId points to the `X`
     token. squiggly->subscriptId gets pointed to the `[Y]` token,
     which itself gets broken into a parent/child pair:
     squiggly->subscriptId points to the `[]` part and the `[]` part's
     innerId points to the `Y`. squiggly->nextId ends up, after all of
     that, pointing to the `Z` token.
  */
  whcl_stoken_id subscriptId;
};

/**
   Empty-initialized whcl_stoken structure, intended for const-copy
   initialization.
*/
#define whcl_stoken__empty_m {\
    whcl_stoken_id_none/*id*/,\
    TOK1_T_INVALID/*ttype*/,TOK1_T_INVALID/*ttype2*/,\
    0/*begin*/,0/*length*/,0/*innerOffset*/,0/*innerLength*/,\
    1/*line*/,0/*column*/,\
    whcl_stoken_id_none/*nextId*/,whcl_stoken_id_none/*innerId*/,\
    whcl_stoken_id_none/*subscriptId*/}

/**
   Empty-initialized whcl_stoken structure, intended for copy
   initialization.
*/
extern const whcl_stoken whcl_stoken__empty;

/**
   The whcl_script class is a simple basis for a tokenizer, largely
   syntax- and language-independent. Its origins go back many years
   and several projects.

   This tokenizer requires that all input be available in advance of
   tokenization and remain valid for its own lifetime.

   This type is not a complete replacement for its predecessor,
   tok1_izer. Practice shows that the low-level tokenizing is much
   simpler with the tok1_izer/tok1_en pair, whereas the
   whcl_script/whcl_stoken are intended to be memory-lighter and more
   geared towards runtime evaluation. Thus whcl_script relies on a
   tok1_izer to provide the lowest-level tokenization support.

   Reminders to self regarding eventual support for (de)serializing
   these...

   An output stream could look something like:

   [binary file header]
   [toker, with all relevant fields]
   [token chain]
   [raw script source]

   Even "compiled", we still need the source for our particular use
   cases. However, a whcl_script does not own the script, so we'd need a
   higher-level construct which manages that ownership.

   Some considerations for serialization:

   - Loss of relationship to parent tokenizers.

   - Tokers know nothing about sub-tokenizers. Those would need to be
   reconstructed on demand, if needed (e.g. when evaluating a
   function, as those each store their own sub-tokenizer).
*/
struct whcl_script {
  /**
     The cwal_engine used for memory management and error reporting.
  */
  cwal_engine * ec;
  /**
     Starting position of input.

     The full input range is [begin, end).
  */
  char const * begin;
  /**
     One-past-the-end position of the input (i.e. the position where
     the NUL byte normally is). It must be >= this->begin.  When
     tokenization iteration gets to this point, the tokenization
     process returns an EOF token (ttype of TOK1_T_EOF).
  */
  char const * end;

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
  whcl_script const * parent;

  /**
     Internal optimization: the top-most tokenizer in a
     sub-(sub-...)tokenizer stack. This MUST be set properly for
     sub-tokenizers. The top-most tokenizer is the one
     which owns this->chain.
  */
  whcl_script const * top;

  /**
     Flags which may change how this object tokenizes.
  */
  uint16_t flags;
  
  /**
     The current token's ID.
  */
  whcl_stoken_id token;

  /**
     The put-back token's ID.
  */
  whcl_stoken_id pbToken;

  /**
     The ID of the current error position token, used in error
     reporting.
  */
  whcl_stoken_id errToken;

  /**
     An optimization which tells whcl__script_next_token2() to use this
     explit token.

     This may not be needed - it's a bit of a holdover from tok1_izer,
     where it is used to reduce re-tokenization after lookaheads.
     That retokenization does not happen in this tokenizer.
     Nonetheless, we'll keep it around as a tok1_izer compatibility
     bit, if for nothing else.
  */
  whcl_stoken_id nextToken;

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
     An array of tokens allocated by whcl__script_compile(). The array
     has this->chainLength entries. For empty chains, we still have a
     single entry - an TOK1_T_EOF token.

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
  whcl_stoken * chain;

  /**
     If this is not NULL, it is assumed to be the bytes referred to
     by this->begin and this->end and it is owned by this object,
     cleaned up when it is finalized. Modifying its contents after
     tokenization leads to undefined behavior.
  */
  char * ownSrc;

  /**
     If this is not NULL, it is assumed to be the bytes referred to by
     this->name and it is owned by this object, cleaned up when it is
     finalized.
  */
  char * ownName;
};
/** Empty-initialized whcl_script object. */
#define whcl__script_empty_m {                        \
    0/*ec*/,                                        \
    0/*begin*/,0/*end*/,                          \
    0/*name*/,                                    \
    0/*parent*/,                                  \
    0/*top*/,                                \
    0/*flags*/,  \
    whcl_stoken_id_none/*token*/,                  \
    whcl_stoken_id_none/*pbToken*/,                \
    whcl_stoken_id_none/*errToken*/,               \
    whcl_stoken_id_none/*nextToken*/ ,             \
    0/*chainLength*/,0/*chainCapacity*/,        \
    NULL/*chain*/,NULL/*ownSrc*/,NULL/*ownName*/ \
}
/** Empty-initialized whcl_script object. */
extern const whcl_script whcl__script_empty;

/**
   Flags for use with whcl_script::flags and/or
   whcl__script_compile().

   Maintenance reminder it is currently important that
   these flags align with tok1_flags_e, but that's just
   a stepping stone towards eliminating that part of the API
   from the public interface.
*/
enum whcl__script_flags_e {
/**
   Sentinel value. Must be 0.
*/
WHCL__SCRIPT_F_NONE = 0,

/**
   If set, the '-' character is considered a legal identifier
   character except when the '-' appears at the start of a token.
*/
WHCL__SCRIPT_F_IDENTIFIER_DASHES = 0x01,
/**
   Like WHCL__SCRIPT_F_IDENTIFIER_DASHES, but a leading dash is also
   considered a valid identifier char.
*/
WHCL__SCRIPT_F_IDENTIFIER_DASHES2 = 0x03,

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
WHCL__SCRIPT_F_RETAIN_JUNK = 0x04,

WHCL__SCRIPT_F_CLIENT_OFFSET = 24,
WHCL__SCRIPT_F_CLIENT_MASK = 0x7FF00000,
WHCL__SCRIPT_F_CLIENT1 = 1<<WHCL__SCRIPT_F_CLIENT_OFFSET,
WHCL__SCRIPT_F_CLIENT2 = WHCL__SCRIPT_F_CLIENT1 << 1,
WHCL__SCRIPT_F_CLIENT3 = WHCL__SCRIPT_F_CLIENT2 << 1
};

/**
   If t->ttype2 is not 0, returns tok1_t_cstr(t->ttype2),
   else it returns tok1_t_cstr(t->ttype1).
*/
char const * whcl__script_ttype_cstr( whcl_stoken const * const t );

/**
   Returns the length of k's string part. If inner is true, the
   token's "inner" length is returned if the token has one, otherwise
   its entire length is returned. If inner is false, the token's whole
   length is returned. The inner length is only valid for the following block-level
   token types:

   - (...parens groups...) (TOK1_T_ParenGroup)
   - [...braces groups...] (TOK1_T_BraceGroup)
   - {...squiggly groups...} (TOK1_T_SquigglyBlock)
   - <<< X heredocs X (TOK1_T_Heredoc)
   - 'single-quoted string' (TOK1_T_StringLiteralSQ)
   - "double-quoted string" (TOK1_T_StringLiteralDQ)

   For such tokens, the inner and outer length will differ, with the
   inner referring to the whole token length minus the open/closing
   parts of the token and (except for strings) leading/trailing spaces
   of that part. For heredocs the inner length refers only to the part
   inside of the heredoc identifier pair, and how much, if any,
   leading/trailing space that includes depends on whethere the
   heredoc uses the ':' modifier or not. In all such cases, the inner
   length will always be at least 2 less than the outer length.
*/
whcl_linecol_t whcl_stoken_len2( whcl_stoken const * const k, bool inner );

/**
   Equivalent to whcl_stoken_len2() with a 2nd argument of `false`.
*/
whcl_linecol_t whcl_stoken_len( whcl_stoken const * const k );

/**
   Returns a pointer to k's position within c. If inner is true then
   a pointer to its "inner" part is returned. Only some token
   types have an inner part and those which don't will return the
   same value regardless of the final argument.
*/
char const * whcl_stoken_begin2( whcl_script const * const c,
                              whcl_stoken const * const k,
                              bool inner);
/**
   Equivalent to whcl_stoken_begin2() with a final argument of false.
*/
char const * whcl_stoken_begin( whcl_script const * const c,
                              whcl_stoken const * const k );
/**
   Returns a pointer to the one-after-the-end position of k's body
   within c. If inner is true then a pointer after its "inner" part is
   returned. Only some token types have an inner part and those which
   don't will return the same value regardless of the final argument.
*/
char const * whcl_stoken_end2( whcl_script const * const c,
                            whcl_stoken const * const k,
                            bool inner);
/**
   Equivalent to whcl_stoken_end2() with a final argument of false.
*/
char const * whcl_stoken_end( whcl_script const * const c,
                            whcl_stoken const * const k);

/**
   Returns a pointer to the start of the given token, which must be a
   valid token with the given tokenizer. If len is non-NULL, the
   length of the token is written there (note that tokens almost never
   NUL-terminated, so having their length is important for many
   purposes). As a special case, if has a virtual length of 0 (e.g.
   k->ttype is TOK1_T_EOF or refers to an empty heredoc) then a pointer
   to static memory (an empty string) is returned, rather than bytes
   encompassed by ct.

   If innerOnly is true and k refers to a block-level token, string
   literal, or a heredoc then the returned string and length
   correspond to its "inner" bytes: the content, minus the outer
   delimiters.

   @see whcl_stoken_cstr()
*/
char const * whcl_stoken_cstr( whcl_script const * const ct,
                             whcl_stoken const * const k,
                             cwal_midsize_t * const len,
                             bool innerOnly);

/**
   Compares the string of the given token with the first nStr bytes of
   the given string and returns a value using memcmp() semantics. If
   nStr is negative then cwal_strlen() is used to calculate its
   length.

   If _inner_ is true, only the "inner" string part is compared, else
   the whole token is compared. If the token has no inner part then
   the whole token is compared.
*/
int whcl_stoken_strcmp(whcl_script const * const ct,
                       whcl_stoken const * const k,
                       char const * str, cwal_int_t nStr,
                       bool innerOnly);

/**
   Similar to, _but subtly different than_, wchl_stoken_strcmp().

   Returns true if the given k->length (not k->innerLength) is nStr
   and matches the first nStr bytes of the given string. If nStr is
   negative then cwal_strlen() is used to calculate it.
*/
bool whcl_stoken_matches( whcl_script const * const ct,
                           whcl_stoken const * const k,
                           char const * const str, cwal_int_t nStr );

/**
   Allocates a new whcl_script instance, which must eventually be
   passed to whcl__script_free() to free it. Returns NULL on allocation
   error.
*/
whcl_script * whcl__script_alloc( cwal_engine * const e );

/**
   Initializes an whcl_script instance, after which it is ready to
   "compile" and tokenize input.

   The caller is obligated to eventually pass ct to
   whcl__script_finalize().

   @see whcl__script_compile()
   @see whcl__script_finalize()
*/
void whcl__script_init( cwal_engine * const e, whcl_script * const ct );

/**
   If parent's current token is a block-level token, this routine
   initializes ct to wrap up that block. If useInnerId is true then
   the token's innerId child is used for the group, else its
   subscriptId child is used.

   Returns 0 on success. Returns CWAL_RC_MISUSE if parent is not
   currently at a token and CWAL_RC_TYPE if that token is not a
   supertoken.

   Preconditions:

   - parent must be compiled and its iterator must currently be at a
     token which has either innerId and/or subscriptId set. Which of
     those is used is specified by the final parameter. If the
     innerId/subscriptId is not set then CWAL_RC_TYPE is returned.

   - ct must be "pristine", i.e. frestly allocated whcl__script_alloc()
     or stack-allocated and copy-initialized from whcl__script_empty, or
     results are undefined.

   Postconditions:

   - parent must outlive ct and must not be recompiled, or its input
     source or token chain otherwise modified, so long as ct is alive.

   - parent would be const but we need it to be non-const only in order
     to be able to set its error token position.

   - On success, ct must be passed to whcl__script_finalize() or
     whcl__script_free() when the caller is done with it, in case it has
     collected any error state or other dynamic memory along the way.

   Misc. notes:

   - A sub-tokenizer does not require any dynamic token chain memory
     of its own: it simply navigates a part of the memory owned by the
     parent.

   - A sub-tokenizer may be initialized from another sub-tokenizer.

   - Sub-tokenizer initialization is a constant-time operation,
   requiring no logic more complex than pointer arithmetic.
*/
int whcl__script_sub_from_group( whcl_script * const parent, whcl_script * const ct,
                                 bool useInnerId );

/**
   Must be passed an initialized (via whcl__script_init()) whcl_script and
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

   The caller is obligated to eventually pass the whcl_script object to
   whcl__script_finalize(), regardless of whether this fuction succeeds
   or fails.

   It is legal to call this routine repeatedly on the same instance,
   provided whcl__script_finalize() and whcl__script_init() are called
   between each, noting finalizing and recompiling will free (and
   thereby invalidate) any tokens it previously managed. Thus it must
   never be recompiled if there are any sub-tokenizers deriving from
   it: doing so will result in undefined behaviour.

   If ct->name is set (or inherited via a parent tokenizer) then that
   information is included in error reporting.

   The following flags are supported for the final argument:

   - WHCL__SCRIPT_F_IDENTIFIER_DASHES
   - WHCL__SCRIPT_F_RETAIN_JUNK

   Pass a flags value of 0 to compile without any flags set.

   Use whcl__script_next_token2() to fetch the next token and
   whcl__script_putback() to put a just-fetched token back.

   Implementation notes:

   - The tokenizer does not know which language constructs are valid,
   so only fails if any *tokens* are invalid, noting that this engine
   treats (), [], and {} blocks as single tokens (which may also, in
   this case of whcl_script/whcl_stoken, contain other tokens).

   @see whcl__script_init()
   @see whcl__script_compile_buffer()
   @see whcl__script_finalize()
*/
int whcl__script_compile( whcl_script * const ct, char const * src,
                          cwal_int_t len,
                          uint32_t compileFlags,
                          whcl_stoken_izer_f tp, void * tpState );

/**
   Convenience wrapper around whcl_stoken_compile() which compiles the
   given buffer's contents or an empty string (if the buffer is
   empty). The tokenizer will refer directly to the given buffer's
   memory, and it must not be modified or reallocated/deallocated for
   so long as ct is active. As a special case, if buf->mem is NULL, it
   is compiled as if it were an empty string living in static memory.
*/
int whcl__script_compile_buffer( whcl_script * const ct,
                                 cwal_buffer const * const buf,
                                 uint32_t compileFlags,
                                 whcl_stoken_izer_f tp, void * tpState );

/**
   Frees all resources owned by ct, but does not free ct.

   It is safe to pass a non-whcl__script_init()'ialized instance if, and
   only if, the argument was copy-initialized from whcl__script_empty
   (or equivalent).
*/
void whcl__script_finalize( whcl_script * const ct );

/**
   Returns the top-most object from t->parent, or t if !t->parent.
*/
whcl_script const * whcl__script_top( whcl_script const * t );

/**
   Fetches the next token from t. t must have been successfully
   intialized using whcl__script_init().

   Returns 0 on success. Because token chains are pre-parsed, the only
   error conditions involve the given tokenizer being invalid.

   If tgt is not NULL, a pointer to the resulting token is copied to
   *tgt.

   The current token may be fetched via whcl__script_token(). If
   tgt is not NULL, calling that function immediately after this
   one succeeds will return the same value as was stored in
   *tgt.

   See whcl__script_token() for information about the lifetime of
   pointers assigned to *tgt.

   Once the end of the token chain (or the end of any traversed
   sub-path) has been reached, each call to this function will resolve
   to a token with the type TOK1_T_EOF. whcl__script_rewind() can be used
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
     requires using sub-tokenizers: see whcl__script_sub_from_group().

   Example usage:

   @code
   whcl_script * ct = ...;
   whcl_stoken const * tok = 0;
   while( 1 ){
     whcl__script_next_token2(ct, &tok);
     assert(tok && "So long as ct is valid, this will always be true.");
     if(tok1_t_is_eof(tok->ttype)) break;
     ... do something with tok ...
     if(tok->innerId) {
       // grouping token: traverse that recursively if needed
       // by using whcl__script_sub_from_group() to initialize a
       // sub-tokenizer
     }
   }
   @endcode
*/
int whcl__script_next_token2( whcl_script * const ct, whcl_stoken const ** tgt );
/**
   A convenience form of whcl__script_next_token2() which direcly returns
   the next token. In practice that function only returns non-0 on
   serious misuse. In such cases, this function will return NULL and
   ct's error state will be updated.
*/
whcl_stoken const * whcl__script_next_token1( whcl_script * const ct );

#if 0
/**
   Variant of whcl__script_next_token1() which returns a non-const token.
   Use carefully: any changing of the token's state may invalidate
   it.
*/
whcl_stoken * whcl__script_next_token_nc( whcl_script * const ct );
#endif

/**
   Returns the next token after the given one, or NULL if t is NULL or
   if t has no RHS sibling.
*/
whcl_stoken const * whcl_stoken_sibling( whcl_script const * const ct,
                                    whcl_stoken const * const t );
/**
   Non-const variant of whcl_stoken_sibling(). Use with care.
 */
whcl_stoken * whcl_stoken_sibling_nc( whcl_script const * const ct,
                                 whcl_stoken const * const t );

/**
   Returns a pointer to ct's current token, or NULL if
   whcl__script_next_toker() has not yet been called on ct. Its memory
   is owned by ct and its state remains unmodified until ct is
   finalized or ct's token chain is modified (which would not happen
   during most normal operations).
*/
whcl_stoken const * whcl__script_token(whcl_script const * const ct);

/**
   A non-const counterpart of whcl__script_token(), to be used with great
   care and responsibility. The returned pointer is owned by ct.
   Modify it at your own peril.
*/
whcl_stoken * whcl__script_token_nc(whcl_script * const ct);


/**
   If ct has a putback token (a call to whcl__script_next_token2() has
   succeeded), the current token is "put back" (unconsumed), the
   putback token is cleared, and the ID of the put-back token is
   returned. Thus if whcl__script_next_token2() is called, then this is
   called, the next call to whcl__script_next_token2() will return the
   same token as the first.

   On a single level of putback is supported.
*/
whcl_stoken_id whcl__script_putback(whcl_script * const ct);

/**
   Sets ct's current token to the one referred to by the given
   argument. The token is expected to have previously been fetched
   using whcl__script_token() or similar.

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
int whcl__stoken_set( whcl_script * const ct, whcl_stoken const * const tok );

/**
   The error-token counterpart of whcl__stoken_set(). The error
   token is used by error reporting routines for specifying where an
   error happened. This routine does not trigger an error.
*/
int whcl__script_errtoken_set( whcl_script * const ct, whcl_stoken const * const tok );

/**
   Returns ct's current error token, or NULL if no error token is set.
*/
whcl_stoken const * whcl__script_errtoken_get( whcl_script const * const ct );

/**
   Returns the first token it finds from: the error token, the
   current token, and the putback token. Its purpose is to find
   a token "close" to an error result.
*/
whcl_stoken const * whcl__script_err_pos( whcl_script const * ct );

/**
   The putback-token counterpart of whcl__stoken_set(). It would
   be highly unusual for a client to need to set the putback token to
   a specific value, but s2's eval engine does that in at least one
   place, so here it is.
*/
int whcl__script_pb_set( whcl_script * const ct, whcl_stoken const * const tok );

/**
   An internal optimization which tells ct to use the given token on
   the next call to whcl__script_next_token2().

   See whcl__stoken_set() for the argument and return value
   semantics.
*/
int whcl__script_next_set( whcl_script * const ct, whcl_stoken const * const tok );

/**
   Returns ct's token at the given 1-based token chain index, or NULL
   if ndx is out of bounds. An index value of 0 is reserved for "no
   token."
*/
whcl_stoken const * whcl__script_at(whcl_script const * const ct,
                                 whcl_stoken_id ndx);

/**
   Non-const version of whcl__script_at(). Use with caution, as
   modifying token state must be done precisely in order to avoid
   mucking things up.
*/
whcl_stoken * whcl__script_at_nc(whcl_script const * ct, whcl_stoken_id id);

/**
   The token ID for ct's final EOF token. a whcl_script _may_ have any
   number of embedded virtual EOF tokes to delimit the ends of block
   constructs, but all whcl_scripts have one at the end of their chain.
 */
whcl_stoken_id whcl__script_eof_id(whcl_script const * const ct );

/**
   Sets the error state of ct->ec, as for cwal_engine_err_set(), and
   returns that call's result value (i.e. the 2nd argument on success
   or a lower-level code on error). The report will include
   script file/line/column information from the ct object.

   Returns the error code, not 0, on success! Returns some other
   non-0 code on error.

   The error report will contain line/column info from ct's current
   error token (if set) or current token (if no error token is
   set). Similarly, ct's name, if set, will be included in the report.

   This routine takes pains not to allocate any memory, which also
   means not generating an error message, if the given error code is
   CWAL_RC_OOM.

   Note that while setting error state may appear to violate the first
   argument's constness, the error is actually set in the
   corresponding cwal_engine, not this object.

   Potential TODO: for sub-tokenizers, always set any error state on
   the top-most instance.
*/
int whcl__script_errv(whcl_script const * const ct, int code, char const * fmt, va_list args );

/**
   Variadic counterpart of whcl__script_errv().
*/
int whcl__script_err(whcl_script const * const ct, int code, char const * fmt, ... );

/**
   Works just like whcl__script_err() but sets ct's error token to
   the given token before generating the error.
*/
int whcl__script_err2(whcl_script * const ct,
                      whcl_stoken const * const tok,
                      int code, char const * fmt, ... );

/**
   Similar to whcl__script_err(), but clears ct's cwal_engine's error state
   and sets its Exception state.

   One exception (as it were) is if the code is CWAL_RC_OOM, in which
   case the error state is set but no exception is thrown and no
   formatted message is generated because doing either would require
   allocating memory.

   If the 2nd argument is 0, it uses a code of CWAL_RC_EXCEPTION
   instead.
*/
int whcl__script_throwv( whcl_script * const ct, int code, char const * fmt, va_list args );

/**
   Variadic counterpart of whcl__script_throwv().
*/
int whcl__script_throw(whcl_script * const ct, int code, char const * fmt, ... );

/**
   Works just like whcl__script_throw() but sets ct's error token to
   the given token before generating the error.
*/
int whcl__script_throw2(whcl_script * const ct,
                        whcl_stoken const * const tok,
                        int code, char const * fmt, ... );

/**
   Works just like whcl__script_throwv() but sets ct's error token to
   the given token before generating the error.
*/
int whcl__script_throw2v(whcl_script * const ct,
                         whcl_stoken const * const tok,
                         int code, char const * fmt,
                         va_list args);

/**
   Returns the current error code associated with ct. If msg is not
   NULL and the return code is not 0 then `*msg` is set to point to
   that memory and `*msgLen`, if msgLen is not NULL, is assigned to
   its length in bytes. The msg memory is owned by ct and may be
   invalidated by any further calls into the API. If no error state
   is set then `*msg` will not be modified.
*/
int whcl__script_err_get(whcl_script const * const ct, char const **msg,
                 cwal_size_t * const msgLen);

/**
   A variant of whcl__script_err_get() which returns the error message but not
   the result code. If no error state is set, returns NULL. If msgLen
   is not NULL and this function returns non-NULL, `*msgLen` is set to
   the length of the message, in bytes. If this function returns NULL,
   `*msgLen` is not modified.
*/
char const * whcl__script_err_msg(whcl_script const * const ct,
                          cwal_size_t * const msgLen);

/**
   Resets ct->ec's error state but does not free any error-related memory,
   saving it for later reuse.
*/
void whcl__script_err_reset(whcl_script const * const ct);

/**
   If tok is a block-level token of ct and contains no tokens or only
   junk/whitespace tokens then 0 is returned, else the token type of
   the first non-junk/space token is returned.

   If tok is NULL then ct's current token is used instead.
*/
int whcl_stoken_block_has_content( whcl_script const * const ct,
                                 whcl_stoken const * tok );

/**
   Resets the given tokenizer such that the next call to
   whcl__script_next_token2() will start back at the tokenizer's initial
   starting point. Returns the script passed to it.
*/
whcl_script * whcl__script_rewind( whcl_script * const ct );

/**
   Returns TOK1_T_EOF if tok is of that type, else returns 0.
*/
int whcl_stoken_is_eof( whcl_stoken const * const tok );
/**
   Returns TOK1_T_EOF if ct has no tokens or is current at an EOF
   token, else returns 0.
*/
int whcl__script_is_eof( whcl_script const * const ct );

/**
   Creates a value for a token. This basically just takes the token
   and makes a string from it, but has different handling for certain
   token types. Specifically:

   - Integers (decimal, octal, binayr, and hex) and doubles are
   created as the corresponding cwal numeric types.

   - Strings literals get unescaped.

   - Heredocs are not unescaped, but their opening/closing markers are
   trimmed, as are any whitespace implied by their modifier flag (if
   any).

   - Grouping constructs create strings using only their "inner"
   content.

   - Any other token type is returned as-is as a string value,
   without unescaping, using the token's whole outer content.

   On success *rv contains the new value.

   The 3rd argument is optional and may be NULL. If provided, it is
   used as a scratch buffer for unescaping purposes and its effective
   length is reset to its call-time length before returning (this
   function will not reduce its amount of reserved memory but may
   increase it). If not provided, and internal buffer is needed,
   potentially (likely) leading to more allocations.

   Returns 0 on success, CWAL_RC_OOM on OOM, and may return
   CWAL_RC_RANGE if unescaping string content fails (so far only ever
   caused by ostensibly invalid inputs, e.g. `\U` sequences).

   ACHTUNG: a returned string value might not be a new instance, and
   it is CRITICAL that the client not call cwal_value_unref() on it
   without calling cwal_value_ref() first. Not following this advice
   can lead to the caller pulling the returned string value out from
   under other code. Conversely, failing to call cwal_value_unref()
   may (depending on other conditions) lead to a cwal_scope-level leak
   until that scope is swept up or popped.
*/
int whcl__script_create_value( whcl_script * const c, whcl_stoken const * const t,
                               cwal_buffer * const escBuffer, cwal_value ** rv );

/**
   Returns true if tokens lhs and rhs "touch". That is, if
   lhs's right border and rhs's left border have no gap
   between them.
*/
bool whcl_stokens_touch( whcl_stoken const * const lhs,
                        whcl_stoken const * const rhs );

/** @internal

    For debugging only: dumps the contents of t to stdout.
*/
void whcl__dump_stoken(whcl_script const * const c, whcl_stoken const * t,
                     char const * lbl, char const * file, int line );
#define whcl__dump_stok(C,T,L) whcl__dump_stoken((C),(T),(L), __FILE__,__LINE__)

/**
   If t->ttype2 is TOK1_T_LiteralDouble then this function tries to
   parse it as such. On success it sets `*rc` to the value and returns
   true. In all other cases it returns false.
*/
bool whcl_stoken_parse_double( whcl_script const * const c,
                               whcl_stoken const * const t,
                               cwal_double_t * const rc );

/**
   If t->ttype2 is one of TOK1_T_LiteralIntDec, TOK1_T_LiteralIntOct,
   TOK1_T_LiteralIntBin, or TOK1_T_LiteralIntHex then this function
   tries to parse it as such. On success it sets `*rc` to the value
   and returns true. In all other cases it returns false.
*/
bool whcl_stoken_parse_int( whcl_script const * const c,
                           whcl_stoken const * const t,
                           cwal_int_t * const rc );

/**
   If ct's current token represents an end-of-expression, that token
   type value is returned, else 0 is returned.
*/
int whcl__script_is_eox( whcl_script const * ct );

/**
   Returns t if it is any of the following values:

   TOK1_T_EOX, TOK1_T_EOF, TOK1_T_EOL, TOK1_T_Semicolon

   else returns 0.
*/
int whcl__script_ttype_is_eox( int ttype );

/**
   If t->ttype is an EOX type, that value is returned.  If t is NULL,
   TOK1_T_EOF is returned. Else 0 is returned.
 */
int whcl_stoken_is_eox( whcl_stoken const * const t );


/**
   Clones a segment of the source script into a new empty script,
   resulting in a standalone script with only those tokens plus the
   obligatory internal EOF token. The input range of tokens is
   inclusive: all tokens between the given two, including the final
   one, are cloned.

   The `dest` argument must be either a pointer to NULL or a pointer
   to a cleanly-initialized whcl_script instance (e.g. from
   whcl__script_alloc() or stack-allocated and initialized by copying
   whcl_script_empty). If it points to NULL, this function will allocate
   a new instance, else it will clear any current state of `*dest`
   before overwriting it.

   On success, output is written to the dest object and copies of the
   underlying source code and script name are given to the dest
   script. Ownership of the returned object is transfered to the
   caller. On error, if `*dest` initially pointed to NULL then `*dest`
   is not modified by this function, but if `*dest` was initially not
   NULL then this function will clear `*dest`'s state on error
   (there's nothing useful to be gleaned from an incomplete script).
   It will never _free_ `*dest`, however.

   If `from` is NULL, src's first token is used. If `to` is NULL,
   src's final token is used.

   If `to` is a supertoken, the range is automatically expanded to
   include all tokens inside that token. We have no simple/direct way
   of doing the same thing for the `from` token, but improving that is
   on the TODO list.

   All token IDs and positions are adapted for the destination except
   that the line/column info is retained as-is.

   Achtung: this may well misbehave if the from/to range starts in the
   middle of a supertoken and ends outside of it. It is up to the
   caller to ensure that the given range starts a nesting depth equal
   to or lower than the end, noting that results if the depths are not
   equal may be wonky.

   Returns 0 on success, CWAL_RC_OOM on OOM. Any other error indicates
   serious misuse of the API.
*/
int whcl__script_slice( whcl_script const * const src, whcl_stoken const * from,
                        whcl_stoken const * to, whcl_script ** dest);


/**
   Returns true if tok->innerId is not 0 and does not refer to an EOF
   token, else returns false.
*/
bool whcl_stoken_has_inner_content(whcl_script const * const ct,
                                  whcl_stoken const * const tok);


/**
   Expects digits to point to digLen bytes with the ASCII values '0'
   or '1'. It parses them as a binary value. On success, if out is not
   NULL then the final parsed result is written there, and it returns
   0. On error (non-binary-digit encountered) then CWAL_RC_RANGE is
   returned and out is not modified.

   The digits may contain any number of '_' characters, which are
   treated as "visual separators" (i.e. simply skipped).

   Minor achtung: this routine accepts, for simplicity, '_' as a
   leading or trailing character but the core script does not. It
   expects to be fed only inputs which have already been vetted by the
   script.
*/
int whcl__script_parse_binary_digits( char const * digits, cwal_size_t digLen,
                              cwal_int_t * out );

/**
   The octal-digit counterpart of whcl__script_parse_binary_digits(), and works
   identically except that it requires octal-digit input.
*/
int whcl__script_parse_octal_digits( char const * digits, cwal_size_t digLen,
                             cwal_int_t * out );

/**
   The decimal-digit counterpart of whcl__script_parse_binary_digits(), and
   works identically except that it requires decimal-digit input.  The
   number may optionally have a leading plus or minus sign.
*/
int whcl__script_parse_decimal_digits( char const * digits, cwal_size_t digLen,
                               cwal_int_t * out );

/**
   The hex-digit counterpart of whcl__script_parse_binary_digits(), and works
   identically except that it requires hex-digit input.
*/
int whcl__script_parse_hex_digits( char const * digits, cwal_size_t digLen,
                                   cwal_int_t * out );

/**
   Binds a whcl_script to a new cwal_value of type cwal_native. If
   withFinalizer is true then ownership of scr is (on success) passed
   to the new value, else the value does not own scr and the caller
   _must_ ensure that scr outlives the value. Trivia: generically
   speaking, nobody can assure that outside of extremely trivial
   or well-controlled cases.

   On success, returns 0 and assigns `*rv` to the new value.  Use
   whcl_value_to_script() to extract the whcl_script-bound native
   part.

   Results are undefined (but bad) if passed the same whcl_script
   instance multiple times or if the script is freed from C code and
   script code subsequently tries to use the object (stepping on a
   stale pointer). Using cwal_native_clear() on the resulting
   cwal_native will cleanly disassociate scr from the cwal state,
   optionally calling the finalizer or not.
*/
int whcl__script_to_value(whcl_script * const scr, bool withFinalizer,
                          cwal_value **rv);

/**
   Returns the prototype object used by whcl__script_to_value(), or
   NULL on OOM or initialization failure. If it returns NULL for
   non-OOM cases then the error or exception state of el will possibly
   have been updated.
*/
cwal_value * whcl__script_prototype(whcl_engine * const el);

/**
   Works like whcl_value_get_script() but "fast-tracks" the check
   to return NULL if the script API has not yet been installed
   (which it won't be for most scripts).
*/
whcl_script * whcl__value_get_script(whcl_engine const * const el,
                                     cwal_value * const v);

#ifdef __cplusplus
}/*extern "C"*/
#endif

#endif
/* include guard */
