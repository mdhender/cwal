/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#ifndef NET_WANDERINGHORSE_CWAL_T10N_H_INCLUDED_
#define NET_WANDERINGHORSE_CWAL_T10N_H_INCLUDED_
#include "libcwal.h"
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif
typedef struct t10n_toker t10n_toker;
typedef struct t10n_token t10n_token;

/**
   Represents a token ID in the t10n_token/c9n_token APIs. Values
   above 0 always represent valid IDs and most contexts treat 0 as EOF
   (often a "virtual" EOF for a block construct). It's tempting to use
   a 16-bit type for this, but the s2 amalgamated unit tests (as of
   20200106) use up about half of that range (2/3rds if we retain
   "junk" tokens), so it's easy to conceive of overflowing that.

   It's also somewhat tempting to use 18 bits (262144) for the ID and
   the remaining 14 (16348) for the token type.
*/
typedef uint32_t t10n_token_id;

/**
   A sentinel t10n_token_id meaning "no ID." This "really should" be
   an extern const t10n_token_id, but we then cannot use it in struct
   initializers :/. Its value MUST be 0.
*/
#define t10n_token_id_none ((t10n_token_id)0)

/**
   Numeric type used for counting script line and column numbers.
   Note that we aim for a 16-bit type to shave a few bytes from
   oft-used token types. As of this writing (20200105), the single
   largest s2 script (its amalgamated unit tests) is right at 5400 lines
   (size=160kb), so any dreams of scripts with more than 64k lines
   would seem to be... ah... somewhat ambitious.
*/
typedef uint16_t t10n_linecol_t;

/**
   s2 token type and operator IDs.

   Values >=0 and <=127 can be translated literally to their
   equivalent char value. Values over 127 are symbolic, not
   necessarily mapping to a single byte nor a Unicode code point with
   the same value. (There are very likely numerous collisions with
   Unicode code points in this enum.)

   @see t10n_ttype_cstr()
*/
enum t10n_token_types_e {
/**
   Used as the token type by t10n_toker_next_token() when a
   tokenization-level error is encountered.
*/
T10N_T_TokErr = -2,

/**
   The generic EOF marker. Used by t10n_toker_next_token() when the
   end of the tokenizer's input range is reached. Note that this
   token is also used for "virtual" EOF and does NOT necessarily map
   to a NUL byte in the input. e.g. when sub-parsing part of a
   larger expression, the subexpression will get a subset of the
   parent range to parse, and its virtual EOF will be part of its
   parent parser's input range.
*/
T10N_T_EOF = -1,

/**
   T10N_T_INVALID is guaranteed by the API to be the entry in this
   enum with the value 0, whereas the concrete values for other
   non-ASCII-range tokens is unspecified except that they are
   guaranteed to be non-0. Note that ASCII NUL value is not
   represented as a token: this tokenizer does not support them in
   input. If we ever add the, they will get ID 200, to follow the
   conventions laid out later on in this enum.
*/
T10N_T_INVALID = 0,

T10N_T_Tab = 9,
T10N_T_NL = 10,
T10N_T_VTab = 11,
T10N_T_FF = 12,
T10N_T_CR = 13,
T10N_T_At = 64 /* \@ */,
/**
   Generic EOL token, for \\r, \\n, and \\r\\n.

   Whether or not newlines end an expression is (or should be)
   context-dependent, and may depend on what token(s) lie(s)
   before it in the parsing process.
*/
T10N_T_EOL = 213,
/** ASCII 32d, but runs of spaces are translated to
    T10N_T_Blank. */
T10N_T_Space = 32 /* ' ' */,
/** Generic token for runs of t10n_is_blank() characters. */
T10N_T_Blank = 132,
T10N_T_Whitespace = 232,
T10N_T_UTFBOM = 332 /* UTF byte-order marker (0xEF 0xBB 0xBF) */,

T10N_T_OpNot = 33 /* ! */,

T10N_T_OpHash = 35 /* # */,
T10N_T_Shebang = 135 /* #!... */,

T10N_T_OpModulo = 37 /* % */,
T10N_T_OpModuloAssign = 237 /* %= */,
T10N_T_OpModuloAssign3 = 337 /* X.Y %= Z*/,

T10N_T_OpAndBitwise = 38 /* & */,
T10N_T_OpAnd = 238 /* && */,
T10N_T_OpAndAssign = 338 /* &= */,
T10N_T_OpAndAssign3 = 438 /* X.Y &= Z */,

T10N_T_ParenOpen = 40 /* ( */,
T10N_T_ParenGroup = 140 /* (...) */,
T10N_T_ParenClose = 41 /* ) */,

T10N_T_OpMultiply = 42 /* * */,
T10N_T_OpMultiplyAssign = 242 /* *= */,
T10N_T_OpMultiplyAssign3 = 342 /* X.Y*=Z */,

T10N_T_OpPlus = 43 /* + */,
T10N_T_OpPlusUnary = 243 /* + */,
T10N_T_OpPlusAssign = 343 /* += */,
T10N_T_OpPlusAssign3 = 443 /* X.Y+=Z */,
T10N_T_OpIncr = 543 /* ++ */,
T10N_T_OpIncrPre = 643 /* ++ */,
T10N_T_OpIncrPost = 843 /* ++ */,

T10N_T_Comma = 44 /* , */,
T10N_T_RHSEval = 144 /* internal-use-only pseudo-operator */,

T10N_T_OpMinus = 45 /* - */,
T10N_T_OpMinusUnary = 245 /* - */,
T10N_T_OpMinusAssign = 345 /* -= */,
T10N_T_OpMinusAssign3 = 445 /* X.Y-=y */,
T10N_T_OpDecr = 545  /* -- */,
T10N_T_OpDecrPre = 645  /* -- */,
T10N_T_OpDecrPost = 745  /* -- */,

T10N_T_OpDot = 46 /* . */,
T10N_T_OpArrow = 146 /* -> */,
T10N_T_OpArrow2 = 246 /* => */,
T10N_T_OpDotDot = 346 /* .. */,
T10N_T_OpDotLength = 446 /* .# */,

T10N_T_OpDivide = 47 /* X/Y */,
T10N_T_OpDivideAssign = 147 /* X/=Y */,
T10N_T_OpDivideAssign3 = 247 /* X.Y/=Z */,

T10N_T_Colon = 58 /* : */,
T10N_T_Colon2 = 258 /* :: */,
T10N_T_OpColon2 = T10N_T_Colon2,
T10N_T_OpColonEqual = 358 /* := */,

T10N_T_Semicolon = 59 /* ; */,
/** Generic end-of-expression token. */
T10N_T_EOX = 159,

T10N_T_CmpLT = 60 /* < */,
T10N_T_CmpLE = 260 /* <= */,
T10N_T_OpShiftLeft = 360 /* << */,
T10N_T_OpShiftLeftAssign = 460 /* <<= */,
T10N_T_OpShiftLeftAssign3 = 560 /* X.Y<<=Z */,
T10N_T_HeredocStart = 660 /* <<< */,
T10N_T_Heredoc = 670 /* A "fully slurped" heredoc. Prior to 2020-08-27,
                      heredocs here a special cse of T10N_T_SquigglyGroup
                      for historical reasons. */,

T10N_T_OpAssign = 61 /* = */,
T10N_T_OpAssign3 = 161 /* = */,
T10N_T_CmpEq = 261 /* == */,
T10N_T_CmpNotEq = 361 /* != */,
T10N_T_CmpEqStrict = 461 /* === */,
T10N_T_CmpNotEqStrict = 561 /* !== */,
T10N_T_OpInherits = 661 /* inherits */,
T10N_T_OpNotInherits = 761 /* !inherits */,
T10N_T_OpContains = 861 /* =~ */,
T10N_T_OpNotContains = 961 /* !~ */,
T10N_T_OpAssignConst3 = 1061 /* X.Y:=Z */,

T10N_T_CmpGT = 62 /* > */,
T10N_T_CmpGE = 262 /* >= */,
T10N_T_OpShiftRight = 362 /* >> */,
T10N_T_OpShiftRightAssign = 462 /* >>= */,
T10N_T_OpShiftRightAssign3 = 562 /* X.Y>>=Z */,

T10N_T_Question = 63 /* ? */,
T10N_T_QDot = 163 /* ?. reserved for potential future use */,

T10N_T_BraceOpen = 91 /* [ */,
T10N_T_BraceGroup = 191 /* [...] */,
T10N_T_Backslash = 92 /* \\ */,
T10N_T_BraceClose = 93 /* ] */,

T10N_T_OpXOr = 94 /* ^ */,
T10N_T_OpXOrAssign = 294 /* ^= */,
T10N_T_OpXOrAssign3 = 394 /* X.Y^=Z */,

T10N_T_SquigglyOpen = 123 /* { */,
T10N_T_SquigglyGroup = 223 /* {...} */,

T10N_T_OpOrBitwise = 124 /* | */,
T10N_T_OpOr = 224 /* || */,
T10N_T_OpOr3 = 324 /* ||| */,
T10N_T_OpElvis = 424 /* ?: */,
T10N_T_OpOrAssign = 524 /* |= */,
T10N_T_OpOrAssign3 = 624 /* X.Y|=Z */,
T10N_T_SquigglyClose = 125 /* } */,
T10N_T_OpNegateBitwise = 126 /* ~ */,


T10N_T_Literal__ = 1000,
T10N_T_LiteralInt,
T10N_T_LiteralIntDec,
T10N_T_LiteralIntHex,
T10N_T_LiteralIntOct,
T10N_T_LiteralIntBin,
T10N_T_LiteralDouble,
T10N_T_LiteralStringDQ,
T10N_T_LiteralStringSQ,
T10N_T_LiteralString /* for "untranslated" strings */,
T10N_T_PropertyKey /* special case of LiteralString */,
T10N_T_Identifier,


T10N_T_ValueTypes__ = 2000,
T10N_T_Value,
T10N_T_Undefined,
T10N_T_Null,
T10N_T_False,
T10N_T_True,
T10N_T_Object,
T10N_T_Array,
T10N_T_Function,

T10N_T_Keyword__ = 3000,
T10N_T_KeywordAffirm,
T10N_T_KeywordAssert,
T10N_T_KeywordBREAKPOINT,
T10N_T_KeywordBreak,
T10N_T_KeywordCOLUMN,
T10N_T_KeywordCatch,
T10N_T_KeywordClass,
T10N_T_KeywordConst,
T10N_T_KeywordContinue,
T10N_T_KeywordDefine,
T10N_T_KeywordDefined,
T10N_T_KeywordDelete,
T10N_T_KeywordDo,
T10N_T_KeywordEcho,
T10N_T_KeywordEnum,
T10N_T_KeywordEval,
T10N_T_KeywordException,
T10N_T_KeywordExit,
T10N_T_KeywordFILE,
T10N_T_KeywordFILEDIR,
T10N_T_KeywordFalse,
T10N_T_KeywordFatal,
T10N_T_KeywordFor,
T10N_T_KeywordForEach,
T10N_T_KeywordFunction,
T10N_T_KeywordIf,
T10N_T_KeywordImport,
T10N_T_KeywordInclude,
T10N_T_KeywordInterface,
T10N_T_KeywordIs,
T10N_T_KeywordIsA,
T10N_T_KeywordLINE,
T10N_T_KeywordNameof,
T10N_T_KeywordNew,
T10N_T_KeywordNull,
T10N_T_KeywordPragma,
T10N_T_KeywordPrivate,
T10N_T_KeywordProc,
T10N_T_KeywordProtected,
T10N_T_KeywordPublic,
T10N_T_KeywordRefcount,
T10N_T_KeywordReturn,
T10N_T_KeywordS2Out,
T10N_T_KeywordSRCPOS,
T10N_T_KeywordScope,
T10N_T_KeywordStatic,
T10N_T_KeywordThrow,
T10N_T_KeywordTrue,
T10N_T_KeywordTry,
T10N_T_KeywordTypeinfo,
T10N_T_KeywordTypename,
T10N_T_KeywordUndefined,
T10N_T_KeywordUnset,
T10N_T_KeywordUKWD,
T10N_T_KeywordUsing /* using() function-like keyword, as opposed to
                     function() using(...) {} */,
T10N_T_KeywordVar,
T10N_T_KeywordWhile,

T10N_T_Comment__ = 4000,
T10N_T_CommentC,
T10N_T_CommentCpp,

T10N_T_Mark__ = 5000,
T10N_T_MarkVariadicStart,

T10N_T_Misc__ = 6000,
/**
   A pseudo-token used internally to translate empty [] blocks to a
   PHP-style array-append operation.

   The parser current only allows this op in the context of an assignment
*/
T10N_T_ArrayAppend,
T10N_T_Foo,

T10N_T_comma_kludge_
};


#if 0
typedef struct t10n_byte_range t10n_byte_range;
/**
   Holds a pair of pointers indicating a range
   to an abstract string data source.
*/
struct t10n_byte_range {
  /**
     The starting position of source.
  */
  char const * begin;
  /**
     One-past-the-end position.
  */
  char const * end;
};
#define t10n_byte_range_empty_m {0,0}
extern const t10n_byte_range t10n_byte_range_empty;
#endif

/**
   A "parser token" - tokens used by the s2 tokenization and
   evaluation process.
*/
struct t10n_token{
  /**
     If set to non-0, this token is proxying an c9n_token with this
     ID. This is part of the experimental framework for adding
     optional support for "compiled" tokens to s2's eval engine.
  */
  t10n_token_id id;

  /**
     A t10n_token_types value.
  */
  int16_t ttype;

  /**
     1-based line number relative to the t10n_toker which
     sets this via t10n_toker_next_token().

     It turns out that we can't reliably count this unless (slightly
     over-simplified) the tokenizer moves only forward. Once
     downstream code starts manipulating t10n_token::begin and
     t10n_token::end, the counting gets messed up (and we have lots
     of cases which do that).
  */
  t10n_linecol_t line;

  /**
     0-based column number relative to the t10n_toker which
     sets this via t10n_toker_next_token().
  */
  t10n_linecol_t column;
  
  /**
     The starting point of the token, relative to its containing
     script. Invalid tokens have a NULL begin value.

     202008: direct access to this is being phased out in favor of an
     ongoing abstraction. Use t10n_toker_begin() to access it.
  */
  char const * _begin;

  /**
     The one-after-the-end point for the token. When tokenizing
     iteratively, each next token starts at the end position of the
     previous token.

     202008: direct access to this is being phased out in favor of an
     ongoing abstraction. Use t10n_toker_end() to access it.
  */
  char const * _end;

  /**
     Some token types "trim" their bytes to some subset of [begin,
     end). For such token types, the range [adjBegin, adjEnd) should
     be used for fetching their "inner" bytes, while [begin, end)
     will hold the full token bytes.

     Currently the types for which this is done include:

     T10N_T_SquigglyGroup, T10N_T_Heredoc, T10N_T_BraceGroup,
     T10N_T_ParenGroup.

     202008: direct access to this is being phased out in favor of an
     ongoing abstraction. Use t10n_toker_adjbegin() to access it.
  */
  char const * _adjBegin;

  /**
     The one-after-the-end counterpart of adjBegin.

     202008: direct access to this is being phased out in favor of an
     ongoing abstraction. Use t10n_toker_adjend() to access it.
  */
  char const * _adjEnd;
};

/**
   Empty-initialized t10n_token structure, intended for
   const-copy initialization.
*/
#define t10n_token_empty_m {0,T10N_T_INVALID,0,0,0,0,0,0,}

/**
   Empty-initialized t10n_token structure, intended for
   copy initialization.
*/
extern const t10n_token t10n_token_empty;

/**
   An internal implementation detail to speed up line-counting
   (something s2 has to do quite often).
*/
struct t10n_toker_lccache_entry {
  char const * pos;
  int line;
  int col;
};
typedef struct t10n_toker_lccache_entry t10n_toker_lccache_entry;
/**
   Empty-initialized t10n_toker_lccache_entry structure, intended for
   const-copy initialization.
*/
#define t10n_toker_lccache_entry_empty_m {0,0,0}
/**
   An internal implementation detail to speed up line-counting
   (something s2 has to do inordinately often).
*/
struct t10n_toker_lccache {
  /** Current position in this->lines. */
  volatile int cursor;
  /** "Size" of each cache slot, based on the size of the
      tokenizer's input range. */
  int slotSize;
  /** Cache of line-counting position results. */
  volatile t10n_toker_lccache_entry lines[
     10
     /* reminders to self: 10 or 20 actually, in the 20191228
        amalgamated s2 unit tests, perform slightly better than 60
        does. Multiple tests show 10 to be an all-around good
        value. */
  ];
};
typedef struct t10n_toker_lccache t10n_toker_lccache;
/**
   Empty-initialized t10n_toker_lccache structure, intended for
   const-copy initialization.
*/
#define t10n_toker_lccache_empty_m \
  {0,0,{                             \
    t10n_toker_lccache_entry_empty_m, t10n_toker_lccache_entry_empty_m,  \
    t10n_toker_lccache_entry_empty_m, t10n_toker_lccache_entry_empty_m,  \
    t10n_toker_lccache_entry_empty_m, t10n_toker_lccache_entry_empty_m,  \
    t10n_toker_lccache_entry_empty_m, t10n_toker_lccache_entry_empty_m,  \
    t10n_toker_lccache_entry_empty_m, t10n_toker_lccache_entry_empty_m  \
 }}

/**
   The t10n_toker class is a simple basis for a tokenizer, largely
   syntax- and language-independent. Its origins go back many years
   and several projects.

   This tokenizer requires that all input be available in advance of
   tokenization and remain valid for its own lifetime.

   @see t10n_toker_init()
   @see t10n_toker_next_token()
   @see t10n_toker_lookahead()
   @see t10n_toker_putback()
   @see t10n_toker_next_token_set()
*/
struct t10n_toker {
  /**
     Used for memory management in "v2" operations. Will be NULL for
     "historical-style" instances. These bits are under construction.
  */
  cwal_engine * e;

  /**
     Starting position of input.

     The full input range is [begin, end).
  */
  char const * begin;

  /**
     One-past-the-end position of the input (i.e. the position
     where the NUL byte normally is).
  */
  char const * end;

  /**
     Error string (static memory) from tokenization
     errors. Set by t10n_toker_next_token().
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
     When recursively tokenizing blocks of a larger script, we use
     sub-parsers which point to their parent. This is used for
     calculating line/col info for reporting sub-parser errors.
  */
  t10n_toker const * parent;

  /**
     For ongoing token compilation experimentation. If this->parent is
     non-NULL then this pointer is assumed to belong to the top-most
     parent in the chain, else it is assumed to be owned by this
     instance.
  */
  void * compiled;

  /**
     Used for capturing line/column offset info for "distant child"
     tokenizers, which "know" they derive from another but have no
     access to it (it may be long gone).
  */
  t10n_linecol_t lineOffset;

  /**
     Column counterpart of lineOffset.
  */
  t10n_linecol_t colOffset;

  /**
     1-based current tokenization line number.

     This is maintained by t10n_toker_next_token(), updated
     as it crosses newline boundaries.

     This can only be tracked properly as long as
     t10n_toker_next_token() is called linearly. Once clients (e.g. s2
     internals) starts jumping around the input, this counting breaks.
  */
  t10n_linecol_t currentLine;
  /**
     0-based current tokenization column number.

     This is maintained by t10n_toker_next_token(). See
     notes in this->currentLine.
  */
  t10n_linecol_t currentCol;

  /**
     Flags which may change how this object tokenizes.
  */
  cwal_flags32_t flags;
  
  /**
     The current token. Its state is set up thusly:

     Initially, token.begin must be this->begin and token.end
     must be 0. That state is used by t10n_toker_next_token() to
     recognize the initial token state and DTRT.

     During tokenization, this object's state is updated to reflect
     the range from [this->begin, this->end) matching a token (or
     an error position, in the case of a tokenization error).
  */
  t10n_token token;

  /**
     The put-back token. t10n_toker_next_token() copies this->token to
     this object before attempting any tokenization.
     t10n_toker_putback() copies _pbToken over this->token and clears
     _pbToken.

     Do not manipulate this directly. use t10n_toker_putback(),
     t10n_toker_putback_get(), and (if really needed)
     t10n_toker_putback_set().
  */
  t10n_token _pbToken;

  /**
     An experiment in cutting down on tokenization. Token lookahead
     ops, and some client code, set this after they have done a
     lookahead, conditionally setting this to that looked-ahead
     token. t10n_toker_next_token() will, if this is set, use this
     token and skip the tokenization phase. This member is cleared by
     t10n_toker_token_set() and t10n_toker_next_token().

     Do not manipulate this directly: use t10n_toker_next_token_set()
     to modify it.
  */
  t10n_token _nextToken;

  /**
     Used for marking an error position, which is part of the line/col
     counting mechanism used for error reporting.

     Do not manipulate this directly. Use t10n_toker_errtoken_get(),
     t10n_toker_errtoken_set(), and t10n_toker_errtoken_has().
  */
  t10n_token _errToken;

  /**
     These tokens are used to capture a range of tokens solely for
     their string content. _Some_ APIs set this to a range encompasing
     all input which they consume. e.g. it can be used to record the
     whole result of multiple t10n_toker_next_token() calls by setting
     capture.begin to the start of the first token and capture.end the
     end of the last token captured. The t10n_t10n-internal APIs do not
     manipulate this member.

     We model capturing as a token range, rather than a simple byte
     range, with the hopes that this approach will work with the
     eventual addition of compiled tokens.
  */
  struct {
    /** First token in the capture range. */
    t10n_token begin;
    /**
       The one-after-the-end token in the capture range. Note that
       this means that the string range of the capture is
       [this->begin.begin, this->end.begin). For completely empty
       ranges this token will be the same as this->begin.
    */
    t10n_token end;
  } capture;

  
  /**
     Internal implementation detail for the line-counting results
     cache. Full disclosure: in order to avoid having to modify the
     signatures of a metric boatload of functions to make their
     (t10n_toker const *) parameters non-const, this member may be
     modified in/via one routine where this t10n_toker instance is
     otherwise const. Const-correct behaviour would require that we
     make a whole family of downstream functions non-const just to
     account for this internal optimization, which i'm not willing to
     do (as much as i love my const). This is where we could use the
     "mutable" keyword in C++, but, alas, this is C89.
  */
  t10n_toker_lccache _lcCache;
};
/** Empty-initialized t10n_toker object. */
#define t10n_toker_empty_m {                     \
    0/*e*/, 0/*begin*/,0/*end*/,                 \
    0/*errMsg*/,                              \
    0/*name*/,                                \
    0/*parent*/,                              \
    0/*compiled*/,                              \
    0/*lineOffset*/,0/*colOffset*/,               \
    1/*currentLine*/,0/*currentCol*/,0/*flags*/,  \
    t10n_token_empty_m/*token*/,               \
    t10n_token_empty_m/*_pbToken*/,             \
    t10n_token_empty_m/*_nextToken*/,             \
    t10n_token_empty_m/*_errToken*/,              \
    {t10n_token_empty_m,t10n_token_empty_m}/*capture*/, \
    t10n_toker_lccache_empty_m/*_lcCache*/          \
  }
/** Empty-initialized t10n_toker object. */
extern const t10n_toker t10n_toker_empty;

/**
   Flags for use with t10n_toker::flags and possibly
   related contexts.
*/
enum t10n_flags_e {
/**
   Sentinel value. Must be 0.
*/
T10N_F_NONE = 0,
/**
   If set, the '-' character is considered a legal identifier by
   t10n_read_identifier2() except when the '-' appears at the start of
   the input.
*/
T10N_F_IDENTIFIER_DASHES = 1,

/**
   An internal-only flag which signifies to error-handling routines
   that the given t10n_toker is only ever used in a strictly linear
   fashion, allowing such routines to take advantage of line/column
   state of the tokenizer and avoid counting lines for error
   reporting. Note that the overwhelming majority of t10n_tokers in s2
   are NOT purely linear and it's normally impossible to know in
   advance whether one will be or not. In such cases it will use the
   position of either its error token or current token, in that order.

   Reminder to self: this was added to support c9n_toker
   experimentation, as c9n_toker internally uses t10n_toker and
   "compilation" but tokenizes all the input before "compiling" it.
   This flag allows errors in that step to be reported without an
   extra line-counting step.
*/
T10N_F_LINEAR_TOKER = 0x10
};

/**
   Must be passed a t10n_toker and its input source. If len
   is negative then the equivalent of strlen() is used to calculate
   its length.

   Returns 0 on success, CWAL_RC_MISUSE if !t or !src. It has no other
   error contditions, so "cannot fail" if its arguments are valid.

   Use t10n_toker_next_token() to fetch the next token, t10n_toker_lookahead()
   to "peek" at the next token, and t10n_toker_putback() to put a just-fetched
   token back.

   An t10n_toker instance initialized via this interface does not
   strictly need to be passed to t10n_toker_finalize(), but there is no
   harm in doing so.
*/
int t10n_toker_init( t10n_toker * t, char const * src, cwal_int_t len,
                     uint32_t flags);

/**
   Experimental - don't use.

   Must be passed a t10n_toker and its input source. If len is negative
   then the equivalent of strlen() is used to calculate its
   length. This routine, unlike t10n_toker_init(), may allocate memory
   and may pre-parse its input, and can therefore fail in a number of
   ways. Thus its result code must always be checked.

   Calling this obligates the caller to eventually pass t to
   t10n_toker_finalize(), regardless of whether this routine succeeds
   or not, to free up any resources this routine may have allocated.

   Returns 0 on success, CWAL_RC_MISUSE if !t or !src.

   Use t10n_toker_next_token() to fetch the next token, t10n_toker_lookahead()
   to "peek" at the next token, and t10n_toker_putback() to put a just-fetched
   token back.
*/
int t10n_toker_init_v2( cwal_engine * e, t10n_toker * t, char const * src, cwal_int_t len,
                        uint32_t flags );

/**
   Resets the tokenization state so that the next call to
   t10n_toker_next_token() will start at the beginning of the
   input. This clears the putback and error token state, and between
   this call and the next call to t10n_toker_next_token() (or similar),
   the current token state is invalid.
*/
void t10n_toker_reset( t10n_toker * t );

/**
   Clears t's state and frees any memory is may be using. This does
   not free t, but will free any memory allocated on its behalf.

   Note that if any sub-tokenizers which derive from t (i.e. have t
   set as their t10n_toker::parent value), they MUST be cleaned up
   first, as they may refer to memory owned by a parent.

   It is safe to pass this function an instance which was not passed
   to t10n_toker_init() or t10n_toker_init_v2() if (and only if) the
   instance was copy-initialized from t10n_toker_empty or
   t10n_toker_empty_m.
*/
void t10n_toker_finalize( t10n_toker * t );

/**
   Initializes t as a sub-tokenizer of parent, using parent->token
   as t's input range.

   Returns CWAL_RC_RANGE if parent->token does not have a valid
   byte range.

   Returns 0 on success.

   On success, [t->begin, t->end) point to the sub-tokenization range
   and t->parent points to parent.

   Results are undefined if either argument is NULL or points to
   uninitialized memory.

   If this function succeeds, the caller is obligated to eventually
   pass t to t10n_toker_finalize(). If it fails, t will be finalized if
   needed. If this routine fails, passing t to t10n_toker_finalize() is
   a harmless no-op if t was initially copy-initialized from
   t10n_toker_empty or t10n_toker_empty_m (i.e. it's in a well-defined
   state).
*/
int t10n_toker_sub_from_toker( t10n_toker const * parent,
                              t10n_toker * dest);

/**
   Initializes sub as a sub-tokenizer of the given parent
   tokenizer/token combintation, using the given token as sub's input
   range.

   Returns CWAL_RC_RANGE if parent does not have a valid byte range.

   Returns 0 on success.

   On success, [t->begin, t->end) point to the sub-tokenization range
   and t->parent points to parent.

   Results are undefined if either argument is NULL or points to
   uninitialized memory.
*/
int t10n_toker_sub_from_token( t10n_toker const * parent, t10n_token const * token,
                              t10n_toker * sub );

/* t10n_toker const * t10n_toker_root( t10n_toker const * t ); */

/**
   Returns the top-most object from t->parent, or t if !t->parent.
*/
t10n_toker const * t10n_toker_top_parent( t10n_toker const * t );

/**
   Returns either t->name or the first name encountered while climbing
   up the t->parent chain. Returns 0 if no name is found. If len is
   not 0 then if this function returns non-0, len is set to that
   name's length.
*/
char const * t10n_toker_name_first( t10n_toker const * t, cwal_size_t * len );
/**
   Returns the top-most name from t and its parent chain.
   Returns 0 if no name is found.

   FIXME: add (cwal_size_t * len) parameter.
*/
char const * t10n_toker_name_top( t10n_toker const * t );

/**
   Sets e's error state, as for cwal_error_set(), and returns that
   call's result value. If fmt is NULL or !*fm, st->errMsg is used (if
   set) as the message. Sometimes it is necessary for callers to set
   or tweak st error position explicitly before calling this (see
   t10n_toker_errtoken_set()).

   Returns the error code, not 0, on success! Returns some other
   non-0 code on error.

   If the given error code is CWAL_RC_OOM, this routine takes pains
   not to allocate any memory, which also means not generating an
   error message.
*/
int t10n_err_ptoker( cwal_engine * e, t10n_toker const * st,
                     int code, char const * fmt, ... );


/**
   Returns the first explicit non-0 error position marker from t or
   its nearest ancestor (via t->parent). If the 2nd argument is not
   NULL, *foundIn is assigned to the tokenizer in which the error
   position was found.

   @see t10n_toker_err_pos()
*/
char const * t10n_toker_err_pos_first( t10n_toker const * t,
                                      t10n_toker const ** foundIn);

/**
   Tries to return an "error position" for the given tokenizer, under
   the assumption that something has just "gone wrong" and the client
   wants to know where (or whereabouts) it went wrong. It first tries
   t10n_toker_err_pos_first(). If that returns NULL, it returns either
   the position of pt's current token or putback token. If those are
   NULL, pt->begin is returned (indicating that tokenization has not
   yet started or failed on the first token).

   @see t10n_toker_err_pos_first()
*/
char const * t10n_toker_err_pos( t10n_toker const * pt );

/**
   Fetches the next token from t. t must have been successfully
   intialized using t10n_toker_init().

   This function is intended to be called repeatedly until it either
   returns 0 (success) AND has (t->ttype==T10N_T_EOF) or until it
   returns non-0 (error). On each iteration, clients should collect
   any of the token information they need from t->token before calling
   this again (which changes t's state).

   Note that this is a lower-level function than t10n_next_token().
   That one builds off of this one.
   
   On success 0 is returned and t is updated as follows:

   t->token.begin points to the start of the token. t->token.end
   points to the one-past-the-end character of the token, so the
   length of the token is (t->token.end - t->token.begin). t->ttype
   will be set to one of the T10N_T_xxx constants.

   At the end of input, t->ttype will be T10N_T_EOF and the token
   length with be 0 (t->begin==t->end).

   On error non-0 is returned, t->ttype will be T10N_T_TokErr, and
   t->errMsg will contain a basic description of the error. On success
   t->errMsg will be 0, so clients may use that to check for errors
   instead of checking the result code or token type T10N_T_TokErr. The
   bytes in t->errMsg are guaranteed to be static. On error
   t->token.begin will point to the starting position of the erroneous
   or unrecognized token.

   The underlying tokenizer is fairly grammar-agnostic but tokenizes
   many constructs as they exist in C-like languages, e.g. ++ is a
   single token (as opposed to two + tokens), and >>= is also a single
   token. A 1-byte character in the range (1,127), which is not
   otherwise already tagged with a type, gets its character's ASCII
   value set as its t->ttype.

   This function saves the pre-call current token state to a putback
   token, and the token can be "put back" by calling t10n_toker_putback().

   Use t10n_toker_lookahead() to "peek" at the next token while keeping
   the token iterator in place.

   Any tokenization error is assumed to be unrecoverable, and it is
   not normally useful to call this again (after an error) without
   re-initializing the tokenizer first.
*/
int t10n_toker_next_token( t10n_toker * t );


/**
   Flags for use with t10n_next_token().
*/
enum t10n_next_token_e {
/**
   Specifies that t10n_next_token() should treat EOL tokens as
   significant (non-junk) when looking for the next token.
*/
T10N_NEXT_NO_SKIP_EOL = 0x01,
/**
   Specifies that t10n_next_token() should not "post-process" any
   tokens. e.g. it normally slurps up whole blocks of parens and
   braces, and re-tags certain token types, but this flag disables
   that. The implication is that the caller of t10n_next_token() "really
   should" use a non-null 4th argument, so that the main input parser
   does not get confused by partially-consumed block constructs or
   mis-tagged semicolons.
*/
T10N_NEXT_NO_POSTPROCESS = 0x02
};

/**
   A form for t10n_toker_next() which does some higher-level
   tokenizing which may not be appropriate for all clients, in that it
   performs various token skipping and post-processing such as
   described in t10n_slurp_heredoc() and t10n_slurp_braces(). It
   searches for the next non-junk token, then perhaps post-processes
   it before returning.

   If tgt is NULL then the token is consumed as normal and st->token
   contains its state after returning. If tgt is not-NULL then the
   this works like a lookahead: the consumed token is copied to *tgt
   and st's token/putback state are restored to their pre-call state
   (regardless of success or failure).

   The flags parameter must be 0 or a bitmask of t10n_next_token_e
   values. Note that by default newlines are treated as junk tokens
   and ignored.

   Returns 0 on success. On error the input must be considered
   unparsable/unrecoverable unless st is a sub-parser of a compound
   token in a larger parser (e.g. a (list) or {script} or [braces]),
   in which case parsing may continue outside of the subparser
   (because we know, at that point, that the group tokenizes as an
   atomic token).

   This function does not throw exceptions in e, but instead
   updates e's error state on error.
*/
int t10n_next_token( cwal_engine * e, t10n_toker * t,
                     int flags, t10n_token * tgt);

/**
   Sets pt's next token to be a bitwise copy of tk, such that the next
   call to t10n_toker_next_token() will evaluate to that token. It is
   up to the caller to ensure that tk is valid for pt (e.g. by having
   read tk via a call to t10n_toker_lookahead()).

   This is an optimization useful when one has looked-ahead in pt and
   determined that the looked-ahead token should be the next-consumed
   token. Using t10n_toker_putback() would also be an option, but this
   approach is more efficient because it eliminates the
   re-tokenization of the put-back token.

   Note that this does not modify the putback token. If
   t10n_toker_next_token() is called immediately after this, the
   putback token will be set to whichever token was most recently
   consumed before *this* routine was called (i.e. exactly the same as
   would happen without this routine having been called).

   @see t10n_toker_next_token()
*/
void t10n_toker_next_token_set( t10n_toker * const pt, t10n_token const * const tk );

/**
   For a given t10n_token_types values, this returns a unique
   string representation of its type ID. The returned bytes
   are static. Returns 0 for an unknown value.

   This function serves two main purposes:

   1) So that we can let gcc warn us if any values in t10n_token_types
   collide with one another. (Implementing it caught two collisions.)

   2) Help with debugging. The strings returned from this function are
   intended for "informational" or "entertainment" value only, not
   (de)serialization.
*/
char const * t10n_ttype_cstr( int ttype );

/**
   Similar to t10n_toker_next_token(), but it skips over any tokens for
   which t10n_ttype_is_junk() returns true. On returning, st->token
   holds the last-tokenized position.

   After this call, the putback token will be the previous token
   read before this call. i.e. the intervening junk tokens are not
   placed into the putback token.
*/
int t10n_toker_next_token_skip_junk( t10n_toker * st );

/**
   If st has no putback token, 0 (false) is returned and this function
   has no side-effects, otherwise st->token is replaced by the putback
   token, the putback token is cleared, and non-0 (true) is returned.

   Note that t10n_toker_next_token_set() is often a more efficient
   option, provided semantics allow for it (which they don't always
   do). After calling this routine, t10n_toker_next_token() must
   re-tokenize the next token, whereas t10n_toker_next_token_set()
   allows t10n_toker_next_token() to bypass that tokenization step.
*/
char t10n_toker_putback( t10n_toker * st );

/**
   Sets a bitwise copy of tok as the current putback token. It is up
   to the caller to ensure that tok is valid for the given tokenizer's
   current state.
*/
void t10n_toker_putback_set( t10n_toker * const st, t10n_token const * const tok );

/**
   Returns st's current putback token, which should be bitwise
   copied by the caller because its state may change on any calls
   to the t10n_toker API.
*/
t10n_token const * t10n_toker_putback_get( t10n_toker const * const st );

/**
   Returns st's current error token. It never returns NULL - the
   memory is owned by st and may be modified by any future calls into
   its API. The token will have a ttype of T10N_T_INVALID, and no
   begin/end values, if there is no error.
*/
t10n_token const * t10n_toker_errtoken_get( t10n_toker const * st );

/**
   Bitwise copies tok to be st's current error token. If tok is NULL,
   the error token is cleared, else it is up to the caller to ensure
   that the token is valid for st's current state.

   Note that setting an error token does not trigger an error - it is
   intended only to mark the part of the tokenizer which might be
   associated with an error, for downstream error reporting of that
   position.
*/
void t10n_toker_errtoken_set( t10n_toker * const st, t10n_token const * const tok );

/**
   Returns non-0 if st has an error token set which points to somewhere
   in st's range, else returns 0.
*/
char t10n_toker_errtoken_has( t10n_toker const * st );

/**
   Uses t10n_toker_next_token() to fetch the next token, sets *tgt to the
   state of that token, resets the tokenizer position to its
   pre-call state, and returns the result of the t10n_toker_next_token()
   call. After calling this, both the current token position and the
   putback token will be as they were before this function was
   called, but st->errMsg might contain error details if non-0 is
   returned.


   Pedantic note: the _contents_ of st->token and st->_pbToken will
   change during the life of this call, but they will be reverted
   before it returned. The point being: don't rely on pointers held
   within those two members being stable between before and after
   this call, and always reference the addresses directly from
   the current state of st->token.

   @see t10n_toker_lookahead_skip()
*/
int t10n_toker_lookahead( t10n_toker * st, t10n_token * tgt );


/**
   A function signature for predicates which tell the caller whether
   a t10n_token_types value meets (or does not meet) a certain
   condition.

   Implementations must return ttype if ttype meets their
   predicate condition(s), else false (0). Note that T10N_T_INVALID
   is guaranteed by the API to be 0.
*/
typedef int (*t10n_ttype_predicate_f)( int ttype );

/**
   Similar to t10n_toker_lookahead(), but it skips over any leading
   tokens for which pred() returns true. On success *tgt contains
   the content of the token which either failed the predicate or is
   an EOF token. The client can force st to that tokenization
   position by passing it to t10n_toker_token_set().

   Before this function returns, st->token and st->_pbToken are
   restored to their pre-call state and st->_nextToken is cleared. It
   is legal for tgt to be st->_nextToken, and that will take precedence
   over clearing that value.
*/
int t10n_toker_lookahead_skip( t10n_toker * st, t10n_token * tgt,
                              t10n_ttype_predicate_f pred );

  
/**
   Works like t10n_toker_lookahead_skip(), but inverts the meaning of
   the predicate: it stops at the first token for which pred()
   returns true.
*/
int t10n_toker_lookahead_until( t10n_toker * st, t10n_token * tgt,
                               t10n_ttype_predicate_f pred );


/**
   Sets st->_pbToken to st->token, st->token to *t, and clears
   st->_nextToken.
*/
void t10n_toker_token_set( t10n_toker * st, t10n_token const * t );

#if 0
/**
   Returns a pointer to st's current token, the state of which may
   change on any any future calls into the t10n_toker API. Never
   returns NULL.

   Though the pointer to this token currently (20200105) never changes
   (only its state does), clients are encouraged to behave as if it
   might change, as "planned potential changes" to the API may well
   make that the case.
*/
t10n_token const * t10n_toker_token_get( t10n_toker const * st );
/**
   Returns the token type (ttype) of st's current token.
*/
int t10n_toker_ttype( t10n_toker const * st );
#endif

/**
   Returns st->token.ttype if st's current token represents an EOF.
*/
int t10n_toker_is_eof( t10n_toker const * st );

/**
   Returns st->token.ttype if st's current token represents an
   end-of-expression.
*/
int t10n_toker_is_eox( t10n_toker const * st );

/**
   Returns ttype if ttype is an end-of-line token.
*/
int t10n_ttype_is_eol( int ttype );

/**
   Returns ttype if ttype represents a "space" token
   (in any of its various incarnations).
*/
int t10n_ttype_is_space( int ttype );

/**
   Returns ttype if the given token type is considered a "junk" token
   (with no syntactical meaning).

   Junk includes the following token types:

   ASCII 32d (SPACE), ASCII 13d (CR), ASCII 9d (TAB),
   T10N_T_Blank, T10N_T_CommentC, T10N_T_CommentCpp,
   T10N_T_Whitespace, T10N_T_Shebang, T10N_T_UTFBOM

   Note that T10N_T_NL/T10N_T_EOL (newline/EOL) is not considered junk
   here, as it is an expression separator in some contexts and
   skippable in others.

   Potential TODO: treat T10N_T_CommentCpp as an EOL because this type
   of token implies one.
*/
int t10n_ttype_is_junk( int ttype );

/**
   Returns ttype if ttype is a basic assignment op:

   T10N_T_OpAssign, T10N_T_ArrayAppend (internally treated as
   assignment).
*/
int t10n_ttype_is_assignment( int ttype );

/**
   Returns ttype if ttype refers to one of the "combo assignment"
   operators, e.g. +=, -=, *=, etc.
*/
int t10n_ttype_is_assignment_combo( int ttype );

/**
   Returns ttype if op is 0 or represents an operator which
   may legally directly proceed a unary operator.
*/
int t10n_ttype_may_precede_unary( int ttype );

/**
   Returns ttype if ttype represents a symbol that unambiguously marks
   the end of an expression:

   T10N_T_EOF, T10N_T_EOX, T10N_T_Semicolon
*/
int t10n_ttype_is_eox( int ttype );

/**
   Returns ttype if ttype represents an EOF (or virtual EOF) token.
*/
int t10n_ttype_is_eof( int ttype );

/**
   Returns ttype if ttype presents a "group" type:

   T10N_T_ParenGroup, T10N_T_BraceGroup, T10N_T_SquigglyGroup
*/
int t10n_ttype_is_group( int ttype );

/**
   Returns ttype if it respends a token whose value can be converted
   to a cwal_value with ease.
*/
int t10n_ttype_is_object_keyable( int ttype );

/**
   Returns ttype if it represents an operator which
   is (in principal) capable of short-circuiting part
   of its arguments:

   T10N_T_OpOr, T10N_T_OpAnd, T10N_T_Question (in the context of ternary
   if).
*/
int t10n_ttype_short_circuits( int ttype );

/**
   Returns ttype if ttype represents a type which is a unary operator
   which requires an *identifier* as its operand:

   T10N_T_OpIncr, T10N_T_OpDecr, T10N_T_OpIncrPre, T10N_T_OpIncrPost,
   T10N_T_OpDecrPre, T10N_T_OpDecrPost

   Noting that the pre/post operator name pairs initially represent
   the same token (e.g. T10N_T_OpIncrPre (++x) vs T10N_T_OpIncrPost
   (x++)), requiring eval-time context to be able to distinguish
   between prefix and postfix forms.

   If it is not, 0 is returned.
*/
int t10n_ttype_is_identifier_prefix( int ttype );

/**
   Returns ttype if ttype is T10N_T_LiteralIntDec or one of its
   non-decimal counterparts, else returns 0.
*/
int t10n_ttype_is_int( int ttype );

/**
   Works like t10n_ttype_is_int(), but includes T10N_T_LiteralDouble as a
   matching type.
*/
int t10n_ttype_is_number( int ttype );

/**
   If pos is in the range [src,end) then this function calculates
   the line (1-based) and column (0-based) of pos within [src,end)
   and sets line/col to those values if those pointers are not
   NULL. If pos is out of range CWAL_RC_RANGE is returned and
   this function has no side-effects. Returns 0 on success.

   Note that s2 globally follows emacs line/column conventions:
   lines are 1-based and columns are 0-based.
*/
int t10n_count_lines( char const * src, char const * end_,
                    char const * pos_,
                    t10n_linecol_t *line, t10n_linecol_t *col );

/**
   Wrapper around t10n_count_lines(), which uses [pt->begin, pt->end)
   as the source range.
*/
int t10n_toker_count_lines( t10n_toker const * pt, char const * pos,
                           t10n_linecol_t * line, t10n_linecol_t * col );

/**
   Incomplete/untested. Don't use.

   tok must be a token from pt.

   This variant tries to use line/column info embedded in tok before
   falling back to counting "the hard way". Note that tok->line and
   tok->col are relative to pt->begin, and pt (or one of its parents)
   may have line/column offset info to apply to the results, making
   the line/col relative to the top-most t10n_toker in the hierarchy.

   Bug: the tok->line/column counting is known to not work as expected
   because of how we hop around while parsing :/.
*/
int t10n_toker_count_lines2( t10n_toker const * pt,
                            t10n_token const * tok,
                            t10n_linecol_t * line, t10n_linecol_t * col );

#if 0
/**
   Collects info from pt which is useful in error reporting. pos is expected
   to be a position within [pt->begin,pt->end). Its line/column position
   is calculated as for t10n_count_lines() (so *line will be 0 if pos is out
   of range). If pos is 0 then pt->errPos is used.

   If name is not NULL, *name is set to the value returned from
   t10n_toker_name_top().

   Any of the (name, line, col) parameters may be 0.
*/
void t10n_toker_err_info( t10n_toker const * pt,
                         char const ** name,
                         char const * pos,
                         int * line, int * col );
#endif


/**
   Unescapes the raw source stream defined by [begin,end) and copies
   it to dest (_appending_ to any existing content in dest). Returns 0
   on success. On success, dest->used is set to the length of the
   unescaped content plus its old length, not counting the trailing
   NUL (but the buffer is NUL-terminated).

   Unescapes the following backslash-escaped characters: '0' (zero),
   'b' (backspace), 't' (tab), 'n' (newline), 'r' (carriage return),
   'f' (form-feed), 'v' (vertical tab), uXXXX (2-byte Unicode
   sequences), UXXXXXXXX (4-byte Unicode), backslash (reduces to a
   single backslash), single- and double-quotes[1].

   All other characters (including a NUL byte or a single slash
   appearing at the end of the input string) treat a preceding
   backslash as if it is a normal character (they retain it). The
   reason for this is to avoid that certain client code (e.g. the
   poreg (POSIX Regex) module) does not have to double-escape strings
   which have to be escaped for underlying C libraries.
   
   This is safe to use on an empty string (begin==end), in which case
   the first byte of the result will be the trailing NUL byte.

   Because this appends its results to dest, the caller may (depending
   on how he is using the buffer) need to remember the value of
   dest->used before this is called, as that will mark the point at
   which this function starts appending data.

   Returns 0 on success, CWAL_RC_MISUSE if passed invalid arguments,
   CWAL_RC_RANGE if \\uXXXX and \\UXXXXXXXX are not given 4 resp. 8 hex
   digits or if those digits resolve to a non-UTF-8 character.

   [1] = note that this routine does not know which quotes (if any)
   wrap up the input string, so it cannot know that only single- _or_
   double-quotes need unescaping. So it unescapes both.
*/
int t10n_unescape_string( cwal_engine * e,
                        char const * begin,
                        char const * end,
                        cwal_buffer * dest );

/**
   Assumes that zPos is the start of an identifier and reads until the
   next non-identifier character. zEnd must be the logical EOF for
   zPos. On returning, *zIdEnd is set to the one-after-the-end
   position of the read identifier (which will be (*zIdEnd-pos) bytes
   long).

   Expects the input to be valid ASCII/UTF-8, else results are
   undefined.

   s2 treats ANY UTF-8 character outside the ASCII range as an
   identifier character.

   Returns the number of identifier characters read.

   @see t10n_read_identifier2()
*/
int t10n_read_identifier( char const * zPos, char const * zEnd,
                        char const ** zIdEnd );

/**
   An alternate form of t10n_read_identifier() which honors the
   T10N_F_IDENTIFIER_DASHES flag if it is set in the final
   argument. If that flag is not set it behaves as documented for
   t10n_read_identifier().

   @see t10n_read_identifier()
*/
int t10n_read_identifier2( char const * zPos, char const * zEnd,
                         char const ** zIdEnd,
                         uint32_t flags );

/**
   Returns true if ch is one of:
       
   ' ' (ASCII 32d), \\t (ASCII 9d)
*/
char t10n_is_blank( int ch );

/**
   Returns true if t10n_is_blank(ch) is true of if ch is one of:

   \\n (ASCII 10d), \\r (13d), \\v (11d), \\f (12d)
*/
char t10n_is_space( int ch );

/**
   Returns true if ch is-a digit character (0..9).
*/
char t10n_is_digit( int ch );

/**
   Returns non-0 if ch is-a hexidecimal digit character (0..9,
   a..F, A..F), else returns 0.
*/
char t10n_is_xdigit( int ch );

/**
   Returns non-0 if character ch is an octal digit character (0..7),
   else returns 0.
*/
char t10n_is_octaldigit( int ch );

/**
   Returns non-0 if ch is an ASCII alphabetic character (a..z,
   A..Z), else returns 0.
*/
char t10n_is_alpha( int ch );

/**
   Returns non-0 if t10n_is_alpha(ch) or t10n_is_digit(ch), else returns
   0.
*/
char t10n_is_alnum( int ch );

/**
   Checks whether tok's range contains only "junk" tokens or not. If
   tok's range contains only noise tokens, 0 is returned, otherwise
   the token type ID of the first non-noise token is returned. Note
   that it also returns 0 if there is a tokenization error. This is
   primarily used to determine whether "super-tokens" (e.g.
   parenthesis/brace groups) contain any usable content before
   attempting to evaluate them.
*/
int t10n_token_has_content( t10n_token const * tok );

/**
   If token->begin is not 0 and less than token->end, then (token->end
   - token->begin) is returned, else 0 is returned (which is not a
   valid token length except for an EOF token).
*/
#define t10n_token_len(TOK) \
  ((cwal_size_t)((t10n_token_begin(TOK)            \
    && t10n_token_end(TOK) > t10n_token_begin(TOK)) \
  ? (cwal_size_t)(t10n_token_end(TOK) - t10n_token_begin(TOK)) \
                 : 0))
/*cwal_size_t t10n_token_len( t10n_token const * token );*/

/**
   If the given token has an "adjusted" begin/end range, this function
   returns the length of that range, else it behaves identically to
   t10n_token_len().

   Typically only group-level tokens and heredocs have an adjusted
   range.
*/
#define t10n_token_len2(TOK)                         \
  ((cwal_size_t)(((t10n_token_adjbegin(TOK) &&       \
    t10n_token_adjbegin(TOK)<=t10n_token_adjend(TOK)) \
    ? (cwal_size_t)(t10n_token_adjend(TOK) - t10n_token_adjbegin(TOK)) \
                  : t10n_token_len(TOK))))
/*cwal_size_t t10n_token_len2( t10n_token const * token );*/

/**
   If t is the result of t10n_toker_next_token() and is any of the
   following types then this sets *rc (if rc is not NULL) to its
   integer representation and returns true: T10N_T_LiteralIntOct,
   T10N_T_LiteralIntDec, T10N_T_LiteralIntHex, T10N_T_LiteralIntBin. Results
   are undefined if t's state does not conform to the internal
   requirements for one of the above-listed t->ttype values (i.e. t's
   state must have been set up by t10n_toker_next_token() or a relative
   of that function).

   Returns false (0) if all conditions are not met or if the
   conversion catches a syntax error which the tokenizer did not (but
   that "should not happen", and may trigger an assert() in debug
   builds).

   ACHTUNG: this does not handle a leading sign, as the sign is, at
   this level of the tokenization API, a separate token.

   @see t10n_token_parse_double()
   @see t10n_cstr_parse_double()
   @see t10n_cstr_parse_int()
*/
bool t10n_token_parse_int( t10n_token const * t, cwal_int_t * rc );

/**
   The double counterpart of t10n_token_parse_int(), but only parses
   tokens with ttype T10N_T_LiteralDouble. Results are undefined if
   t->ttype is T10N_T_LiteralDouble but the byte range referred to by t
   is not a valid double token.

   ACHTUNG: this does not handle a leading sign, as the sign is, at
   this level of the tokenization API, a separate token.

   @see t10n_token_parse_int()
   @see t10n_cstr_parse_double()
   @see t10n_cstr_parse_int()
*/
bool t10n_token_parse_double( t10n_token const * t, cwal_double_t * rc );

/**
   Works like t10n_token_parse_int() except that:

   - slen specifies the input length. If slen <0 then cwal_strlen(str)
     is used to calculate the input length.

   - It uses all bytes in the range [str, str+slen) as input and will
   fail if the input contains anything other than a numeric value,
   optionally with leading space.

   - It accepts a leading sign character, regardless of the integer
   notation (decimal, hex, octal). It ignores spaces around the sign
   character.

   @see t10n_token_parse_double()
   @see t10n_token_parse_int()
   @see t10n_cstr_parse_double()
*/
bool t10n_cstr_parse_int( char const * str, cwal_int_t slen, cwal_int_t * result );

/**
   The double counterpart of t10n_cstr_parse_int().

   @see t10n_token_parse_double()
   @see t10n_token_parse_int()
   @see t10n_cstr_parse_int()
*/
bool t10n_cstr_parse_double( char const * str, cwal_int_t slen,
                           cwal_double_t * result );

/**
   Expects a filename-like string in the first slen bytes of str, with
   directory components separated by either '/' or '\\' (it uses the
   first of those it finds, in that order, as the separator). If any
   separator is found, a pointer to the last instance of it in str is
   returned, otherwise 0 is returned.
*/
char const * t10n_last_path_sep(char const * str, cwal_size_t slen );

/**
   If tok->ttype is T10N_T_Identifier and the token's contents are one
   of (true, false, null, undefined) then the corresponding built-in
   cwal value is returned (e.g. cwal_value_true() or
   cwal_value_null()), else NULL is returned. tok may not be NULL. The
   returned value, if not NULL, is guaranteed to be a shared instance
   which lives outside of the normal cwal_value lifetime management.
*/
cwal_value * t10n_token_is_tfnu( t10n_token const * tok );

/**
   Returns a pointer to the token's contents, setting its length to
   *len if len is not NULL. tok must not be NULL. Note that the
   returned string is almost certainly not NUL-terminated (or
   terminates at the very end of the t10n_toker from which tok
   originates), thus capturing the length is normally required.

   This does not strip any leading/spaces of tokens which have been
   "adjusted" to do so. i.e. the whole token's contents are part of
   the returned byte range. Unless...

   If innerOnly is true then only the "inner" bytes and length of the
   token are returned. This only makes a difference for block-level
   tokens (parens, braces, and squigglies), string literals, and
   heredocs. For those constructs, if innerOnly is true then their
   outer delimiters are not included in the result.

   As a special case, if the token has a length of zero (this can
   happen when passing true for the 3rd argument) then the returned
   bytes refer to a static empty string, not a pointer to the token's
   contents.
*/
char const * t10n_token_cstr( t10n_token const * tok,
                               cwal_size_t * len,
                               bool innerOnly);

/**
   If st->capture is set up properly, this returns a pointer to the
   start of the range and *len (if not NULL) is set to the length of
   the captured range. If no capture is set, or it appears to be
   invalid, NULL is returned and len is not modified.
*/
char const * t10n_toker_capture_cstr( t10n_toker const * st,
                                       cwal_size_t * len );

/**
   A helper type for tokenizing conventional PATH-style strings.
   Initialize them with t10n_path_toker_init() and iterate over them
   with t10n_path_toker_next().
*/
struct t10n_path_toker {
  /** Begining of the input range. */
  char const * begin;
  /** One-after-the-end of the input range. */
  char const * end;
  /** Position for the next token lookup. */
  char const * pos;
  /** List of token separator characters (ASCII only). */
  char const * separators;
};
typedef struct t10n_path_toker t10n_path_toker;
/**
   Default-initialized t10n_path_toker instance, intended for const-copy
   initialization. On Windows builds its separators member is set to
   ";" and on other platforms it's set to ":;".
*/
#if defined(T10N_OS_WINDOWS)
#  define t10n_path_toker_empty_m {NULL,NULL,NULL,";"}
#else
#  define t10n_path_toker_empty_m {NULL,NULL,NULL,":;"}
#endif

/**
   Default-initialized t10n_path_toker instance, intended for
   copy initialization.

   @see t10n_path_toker_empty_m
*/
extern const t10n_path_toker t10n_path_toker_empty;

/**
   Wipes out pt's current state by copying t10n_path_toker_empty over it
   and initializes pt to use the given path as its input. If len is 0
   or more then it must be the length of the string, in bytes. If len
   is less than 0, cwal_strlen() is used to determine the path's
   length.  (When dealing with inputs which are not NUL-terminated,
   it's critical that the user pass the correct non-negative length.)

   If the client wants to modify pt->separators, it must be done so
   *after* calling this.

   Use t10n_path_toker_next() to iterate over the path entries.
*/
void t10n_path_toker_init( t10n_path_toker * pt, char const * path, cwal_int_t len );

/**
   Given a t10n_path_toker which was formerly initialized using
   t10n_path_toker_init(), this iterates over the next-available path
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
   t10n_path_toker pt = t10n_path_toker_empty;
   t10n_path_toker_init(&pt, path, pathLen);
   while(0==t10n_path_toker_next(&pt, &t, &tLen)){
      // The next element is the tLen bytes of memory starting at t:
      printf("Path element: %.*s\n", (int)tLen, t);
   }
   @endcode
*/
int t10n_path_toker_next( t10n_path_toker * pt, char const ** token, cwal_size_t * len );

/**
    Post-processor for T10N_T_HeredocStart tokens. e is the
    interpreter engine (used for error reporting), st is the current
    tokenizer state. tgt is where the results are written. If tgt is
    NULL, st->token is used.

    Requires that st->token be a T10N_T_HeredocStart token. This
    function scans st for the opening and closing heredoc tokens.

    On error a non-0 CWAL_RC_xxx value is returned and e's error
    state will be updated. On error st->token is in an unspecified
    state.

    On success tgt->ttype is set to T10N_T_SquigglyGroup and
    [out->begin, out->end) will point to the whole token proper,
    including its leading and trailing opener/closer tokens. That
    range will have a length of at least 2 (one each for the opening
    and closing token). [out->adjBegin, out->adjEnd) will point to
    the range encompassing the body of the open/close block, stripped
    of any leading or trailing spaces, as specified below. That range
    may be empty.

    Returns 0 on success and sets e's error state on error. 

    Syntax rules:

    Assuming that the T10N_T_HeredocStart token is '<<<', heredocs
    can be constructed as follows:

    <<<EOF blah blah blah EOF
    <<<EOF blah blah blahEOF // equivalent!
    <<<'EOF' ... 'EOF'
    <<<"EOF" ... "EOF"
    <<<:EOF ... EOF // colon changes space-skipping rules

    Failure to find a match for the heredoc identifier will result in
    an error. Identifiers may optionally be quoted, but both the
    opener and closer must use the same quote character. If quoted,
    the contents of the string need not be a legal
    identifier. e.g. "123", with the quotes, is a legal delimiter.

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
int t10n_slurp_heredoc( cwal_engine * e, t10n_toker * st,
                        t10n_token * tgt );

/**
   Post-processor for converting a single "opener" token into
   a larger token containing the whole braced group, writing
   the resulting token to the given output token pointer.

   Requires st->token to be a T10N_T_SquigglyOpen, T10N_T_BraceOpen, or
   T10N_T_ParenOpen token. It "slurps" the content part, from the
   starting brace up to (and include) the ending brace. It uses
   t10n_next_token() to parse the group, meaning that it must
   contain tokenizable code. We cannot simply do a byte-scan through
   it because doing so would exclude any constructs (e.g. strings)
   which might themselves contain tokens which would look like
   closing tokens here.

   If !out, then st->token is used as the output destination. If out
   is not NULL then after returning, st->token and st->pbToken will
   have their pre-call state (regardless of error or success).

   Returns 0 on success. On error it will set se's error state.

   On success:

   - out->ttype is set to one of: T10N_T_SquigglyGroup,
   T10N_T_ParenGroup, T10N_T_BraceGroup.

   - [out->begin, out->end) will point to the whole token, including
   its leading and trailing opener/closer tokens. That range will
   have a length of at least 2 (one each for the opening and closing
   token). [out->adjBegin, out->adjEnd) will point to the range
   encompassing the body of the open/close block, stripped of any
   leading or trailing whitespaces. That range may be empty.

*/
int t10n_slurp_braces( cwal_engine *se, t10n_toker * st,
                       t10n_token * out );


/*
  The following macros are part of an ongoing porting/abstraction
  effort to eliminate direct access to token members, the eventual
  goal being to permit the token type to be easily swapped out so that
  we can enable "compiled" (pre-parsed) chains of tokens (which would
  require far more memory than s2 currently uses but would be tons
  faster for most scripts).
*/
#define t10n_token_begin(P) ((P)->_begin)
#define t10n_token_begin2(P) ((P)->_adjBegin ? (P)->_adjBegin : (P)->_begin)
#define t10n_token_begin_set(P,X) (P)->_begin = (X)
#define t10n_token_end(P) ((P)->_end)
#define t10n_token_end2(P) ((P)->_adjEnd ? (P)->_adjEnd : (P)->_end)
#define t10n_token_end_set(P,X) (P)->_end = (X)
#define t10n_token_adjbegin(P) ((P)->_adjBegin)
#define t10n_token_adjbegin_set(P,X) (P)->_adjBegin = (X)
#define t10n_token_adjbegin_incr(P,X) (P)->_adjBegin += (X)
#define t10n_token_adjend(P) ((P)->_adjEnd)
#define t10n_token_adjend_set(P,X) (P)->_adjEnd = (X)
#define t10n_token_adjend_incr(P,X) (P)->_adjEnd += (X)

#define t10n_toker_begin(PT) ((PT)->begin)
#define t10n_toker_end(PT) ((PT)->end)
#define t10n_toker_token(PT) (PT)->token
#define t10n_toker_len(PT) ((PT)->end - (PT)->begin)

#ifdef __cplusplus
}/*extern "C"*/
#endif
#endif
/* include guard */
