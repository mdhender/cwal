/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#ifndef NET_WANDERINGHORSE_CWAL_S2_T10N_H_INCLUDED_
#define NET_WANDERINGHORSE_CWAL_S2_T10N_H_INCLUDED_
#include "wh/cwal/cwal.h"

#ifdef __cplusplus
extern "C" {
#endif
typedef struct s2_ptoker s2_ptoker;
typedef struct s2_ptoken s2_ptoken;

/**
   Represents a token ID in the s2_ptoken/s2_cptoken APIs. Values
   above 0 always represent valid IDs and most contexts treat 0 as EOF
   (often a "virtual" EOF for a block construct). It's tempting to use
   a 16-bit type for this, but the s2 amalgamated unit tests (as of
   20200106) use up about half of that range (2/3rds if we retain
   "junk" tokens), so it's easy to conceive of overflowing that.

   It's also somewhat tempting to use 18 bits (262144) for the ID and
   the remaining 14 (16348) for the token type.
*/
typedef uint32_t s2_token_id;

/**
   A sentinel s2_token_id meaning "no ID." This "really should" be
   an extern const s2_token_id, but we then cannot use it in struct
   initializers :/. Its value MUST be 0.
*/
#define s2_token_id_none (0)

/**
   Numeric type used for counting script line and column numbers.
   Note that we aim for a 16-bit type to shave a few bytes from
   oft-used token types. As of this writing (20200105), the single
   largest s2 script (its amalgamated unit tests) is right at 5400 lines
   (size=160kb), so any dreams of scripts with more than 64k lines
   would seem to be... ah... somewhat ambitious.
*/
typedef uint16_t s2_linecol_t;

/**
   s2 token type and operator IDs.

   Values >=0 and <=127 can be translated literally to their
   equivalent char value. Values over 127 are symbolic, not
   necessarily mapping to a single byte nor a Unicode code point with
   the same value. (There are very likely numerous collisions with
   Unicode code points in this enum.)

   @see s2_ttype_cstr()
*/
enum s2_token_types {
/**
   Used as the token type by s2_ptoker_next_token() when a
   tokenization-level error is encountered.
*/
S2_T_TokErr = -2,

/**
   The generic EOF marker. Used by s2_ptoker_next_token() when the
   end of the tokenizer's input range is reached. Note that this
   token is also used for "virtual" EOF and does NOT necessarily map
   to a NUL byte in the input. e.g. when sub-parsing part of a
   larger expression, the subexpression will get a subset of the
   parent range to parse, and its virtual EOF will be part of its
   parent parser's input range.
*/
S2_T_EOF = -1,

/**
   S2_T_INVALID is guaranteed by the API to be the entry in this
   enum with the value 0, whereas the concrete values for other
   non-ASCII-range tokens is unspecified except that they are
   guaranteed to be non-0.
*/
S2_T_INVALID = 0,

S2_T_Tab = 9,
S2_T_NL = 10,
S2_T_VTab = 11,
S2_T_FF = 12,
S2_T_CR = 13,
S2_T_At = 64 /* \@ */,
/**
   Generic EOL token, for \\r, \\n, and \\r\\n.

   Whether or not newlines end an expression is (or should be)
   context-dependent, and may depend on what token(s) lie(s)
   before it in the parsing process.
*/
S2_T_EOL = 213,
/** ASCII 32d, but runs of spaces are translated to
    S2_T_Blank. */
S2_T_Space = 32 /* ' ' */,
/** Generic token for runs of s2_is_blank() characters. */
S2_T_Blank = 132,
S2_T_Whitespace = 232,
S2_T_UTFBOM = 332 /* UTF byte-order marker (0xEF 0xBB 0xBF) */,

S2_T_OpNot = 33 /* ! */,

S2_T_OpHash = 35 /* # */,
S2_T_Shebang = 135 /* #!... */,

S2_T_OpModulo = 37 /* % */,
S2_T_OpModuloAssign = 237 /* %= */,
S2_T_OpModuloAssign3 = 337 /* X.Y %= Z*/,

S2_T_OpAndBitwise = 38 /* & */,
S2_T_OpAnd = 238 /* && */,
S2_T_OpAndAssign = 338 /* &= */,
S2_T_OpAndAssign3 = 438 /* X.Y &= Z */,

S2_T_ParenOpen = 40 /* ( */,
S2_T_ParenGroup = 140 /* (...) */,
S2_T_ParenClose = 41 /* ) */,

S2_T_OpMultiply = 42 /* * */,
S2_T_OpMultiplyAssign = 242 /* *= */,
S2_T_OpMultiplyAssign3 = 342 /* X.Y*=Z */,

S2_T_OpPlus = 43 /* + */,
S2_T_OpPlusUnary = 243 /* + */,
S2_T_OpPlusAssign = 343 /* += */,
S2_T_OpPlusAssign3 = 443 /* X.Y+=Z */,
S2_T_OpIncr = 543 /* ++ */,
S2_T_OpIncrPre = 643 /* ++ */,
S2_T_OpIncrPost = 843 /* ++ */,

S2_T_Comma = 44 /* , */,
S2_T_RHSEval = 144 /* internal-use-only pseudo-operator */,

S2_T_OpMinus = 45 /* - */,
S2_T_OpMinusUnary = 245 /* - */,
S2_T_OpMinusAssign = 345 /* -= */,
S2_T_OpMinusAssign3 = 445 /* X.Y-=y */,
S2_T_OpDecr = 545  /* -- */,
S2_T_OpDecrPre = 645  /* -- */,
S2_T_OpDecrPost = 745  /* -- */,

S2_T_OpDot = 46 /* . */,
S2_T_OpArrow = 146 /* -> */,
S2_T_OpArrow2 = 246 /* => */,
S2_T_OpDotDot = 346 /* .. */,
S2_T_OpDotLength = 446 /* .# */,

S2_T_OpDivide = 47 /* X/Y */,
S2_T_OpDivideAssign = 147 /* X/=Y */,
S2_T_OpDivideAssign3 = 247 /* X.Y/=Z */,

S2_T_Colon = 58 /* : */,
S2_T_Colon2 = 258 /* :: */,
S2_T_OpColon2 = S2_T_Colon2,
S2_T_OpColonEqual = 358 /* := */,

S2_T_Semicolon = 59 /* ; */,
/** Generic end-of-expression token. */
S2_T_EOX = 159,

S2_T_CmpLT = 60 /* < */,
S2_T_CmpLE = 260 /* <= */,
S2_T_OpShiftLeft = 360 /* << */,
S2_T_OpShiftLeftAssign = 460 /* <<= */,
S2_T_OpShiftLeftAssign3 = 560 /* X.Y<<=Z */,
S2_T_HeredocStart = 660 /* <<< */,
S2_T_HeredocStart2 = 760 /* {{{ */,
S2_T_Heredoc = 670 /* A "fully slurped" heredoc. Prior to 2020-08-27,
                      heredocs here a special cse of S2_T_SquigglyBlock
                      for historical reasons. */,

S2_T_OpAssign = 61 /* = */,
S2_T_OpAssign3 = 161 /* = */,
S2_T_CmpEq = 261 /* == */,
S2_T_CmpNotEq = 361 /* != */,
S2_T_CmpEqStrict = 461 /* === */,
S2_T_CmpNotEqStrict = 561 /* !== */,
S2_T_OpInherits = 661 /* inherits */,
S2_T_OpNotInherits = 761 /* !inherits */,
S2_T_OpContains = 861 /* =~ */,
S2_T_OpNotContains = 961 /* !~ */,
S2_T_OpAssignConst3 = 1061 /* X.Y:=Z */,

S2_T_CmpGT = 62 /* > */,
S2_T_CmpGE = 262 /* >= */,
S2_T_OpShiftRight = 362 /* >> */,
S2_T_OpShiftRightAssign = 462 /* >>= */,
S2_T_OpShiftRightAssign3 = 562 /* X.Y>>=Z */,

S2_T_Question = 63 /* ? */,
S2_T_QDot = 163 /* ?. reserved for potential future use */,

S2_T_BraceOpen = 91 /* [ */,
S2_T_BraceGroup = 191 /* [...] */,
S2_T_Backslash = 92 /* \\ */,
S2_T_BraceClose = 93 /* ] */,

S2_T_OpXOr = 94 /* ^ */,
S2_T_OpXOrAssign = 294 /* ^= */,
S2_T_OpXOrAssign3 = 394 /* X.Y^=Z */,

S2_T_SquigglyOpen = 123 /* { */,
S2_T_SquigglyBlock = 223 /* {...} */,

S2_T_OpOrBitwise = 124 /* | */,
S2_T_OpOr = 224 /* || */,
S2_T_OpOr3 = 324 /* ||| */,
S2_T_OpElvis = 424 /* ?: */,
S2_T_OpOrAssign = 524 /* |= */,
S2_T_OpOrAssign3 = 624 /* X.Y|=Z */,
S2_T_SquigglyClose = 125 /* } */,
S2_T_OpNegateBitwise = 126 /* ~ */,


S2_T_Literal__ = 1000,
S2_T_LiteralInt,
S2_T_LiteralIntDec,
S2_T_LiteralIntHex,
S2_T_LiteralIntOct,
S2_T_LiteralIntBin,
S2_T_LiteralDouble,
S2_T_LiteralStringDQ,
S2_T_LiteralStringSQ,
S2_T_LiteralString /* for "untranslated" strings */,
S2_T_PropertyKey /* special case of LiteralString */,
S2_T_Identifier,


S2_T_ValueTypes__ = 2000,
S2_T_Value,
S2_T_Undefined,
S2_T_Null,
S2_T_False,
S2_T_True,
S2_T_Object,
S2_T_Array,
S2_T_Function,

S2_T_Keyword__ = 3000,
S2_T_KeywordAffirm,
S2_T_KeywordAssert,
S2_T_KeywordBREAKPOINT,
S2_T_KeywordBreak,
S2_T_KeywordCOLUMN,
S2_T_KeywordCatch,
S2_T_KeywordClass,
S2_T_KeywordConst,
S2_T_KeywordContinue,
S2_T_KeywordDefine,
S2_T_KeywordDefined,
S2_T_KeywordDelete,
S2_T_KeywordDo,
S2_T_KeywordEcho,
S2_T_KeywordEnum,
S2_T_KeywordEval,
S2_T_KeywordException,
S2_T_KeywordExit,
S2_T_KeywordFILE,
S2_T_KeywordFILEDIR,
S2_T_KeywordFalse,
S2_T_KeywordFatal,
S2_T_KeywordFor,
S2_T_KeywordForEach,
S2_T_KeywordFunction,
S2_T_KeywordIf,
S2_T_KeywordImport,
S2_T_KeywordInclude,
S2_T_KeywordInterface,
S2_T_KeywordIs,
S2_T_KeywordIsA,
S2_T_KeywordLINE,
S2_T_KeywordNameof,
S2_T_KeywordNew,
S2_T_KeywordNull,
S2_T_KeywordPragma,
S2_T_KeywordPrivate,
S2_T_KeywordProc,
S2_T_KeywordProtected,
S2_T_KeywordPublic,
S2_T_KeywordRefcount,
S2_T_KeywordReturn,
S2_T_KeywordS2Out,
S2_T_KeywordSRCPOS,
S2_T_KeywordScope,
S2_T_KeywordStatic,
S2_T_KeywordThrow,
S2_T_KeywordTrue,
S2_T_KeywordTry,
S2_T_KeywordTypeinfo,
S2_T_KeywordTypename,
S2_T_KeywordUndefined,
S2_T_KeywordUnset,
S2_T_KeywordUKWD,
S2_T_KeywordUsing /* using() function-like keyword, as opposed to
                     function() using(...) {} */,
S2_T_KeywordVar,
S2_T_KeywordWhile,

S2_T_Comment__ = 4000,
S2_T_CommentC,
S2_T_CommentCpp,

S2_T_Mark__ = 5000,
S2_T_MarkVariadicStart,

S2_T_Misc__ = 6000,
/**
   A pseudo-token used internally to translate empty [] blocks to a
   PHP-style array-append operation.

   The parser current only allows this op in the context of an assignment
*/
S2_T_ArrayAppend,
S2_T_Foo,

S2_T_comma_kludge_
};


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
#define s2_byte_range_empty_m {0,0}
extern const s2_byte_range s2_byte_range_empty;
#endif

/**
   A "parser token" - tokens used by the s2 tokenization and
   evaluation process.
*/
struct s2_ptoken{
  /**
     If set to non-0, this token is proxying an s2_cptoken with this
     ID. This is part of the experimental framework for adding
     optional support for "compiled" tokens to s2's eval engine.
  */
  s2_token_id id;

  /**
     A s2_token_types value.
  */
  int16_t ttype;

  /**
     1-based line number relative to the s2_ptoker which
     sets this via s2_ptoker_next_token().

     It turns out that we can't reliably count this unless (slightly
     over-simplified) the tokenizer moves only forward. Once
     downstream code starts manipulating s2_ptoken::begin and
     s2_ptoken::end, the counting gets messed up (and we have lots
     of cases which do that).
  */
  s2_linecol_t line;

  /**
     0-based column number relative to the s2_ptoker which
     sets this via s2_ptoker_next_token().
  */
  s2_linecol_t column;
  
  /**
     The starting point of the token, relative to its containing
     script. Invalid tokens have a NULL begin value.

     202008: direct access to this is being phased out in favor of an
     ongoing abstraction. Use s2_ptoker_begin() to access it.
  */
  char const * _begin;

  /**
     The one-after-the-end point for the token. When tokenizing
     iteratively, each next token starts at the end position of the
     previous token.

     202008: direct access to this is being phased out in favor of an
     ongoing abstraction. Use s2_ptoker_end() to access it.
  */
  char const * _end;

  /**
     Some token types "trim" their bytes to some subset of [begin,
     end). For such token types, the range [adjBegin, adjEnd) should
     be used for fetching their "inner" bytes, while [begin, end)
     will hold the full token bytes.

     Currently the types for which this is done include:

     S2_T_SquigglyBlock, S2_T_Heredoc, S2_T_BraceGroup,
     S2_T_ParenGroup.

     202008: direct access to this is being phased out in favor of an
     ongoing abstraction. Use s2_ptoker_adjbegin() to access it.
  */
  char const * _adjBegin;

  /**
     The one-after-the-end counterpart of adjBegin.

     202008: direct access to this is being phased out in favor of an
     ongoing abstraction. Use s2_ptoker_adjend() to access it.
  */
  char const * _adjEnd;
};

/**
   Empty-initialized s2_ptoken structure, intended for
   const-copy initialization.
*/
#define s2_ptoken_empty_m {0,S2_T_INVALID,0,0,0,0,0,0,}

/**
   Empty-initialized s2_ptoken structure, intended for
   copy initialization.
*/
extern const s2_ptoken s2_ptoken_empty;

/**
   An internal implementation detail to speed up line-counting
   (something s2 has to do quite often).
*/
struct s2_ptoker_lccache_entry {
  char const * pos;
  int line;
  int col;
};
typedef struct s2_ptoker_lccache_entry s2_ptoker_lccache_entry;
/**
   Empty-initialized s2_ptoker_lccache_entry structure, intended for
   const-copy initialization.
*/
#define s2_ptoker_lccache_entry_empty_m {0,0,0}
/**
   An internal implementation detail to speed up line-counting
   (something s2 has to do inordinately often).
*/
struct s2_ptoker_lccache {
  /** Current position in this->lines. */
  volatile int cursor;
  /** "Size" of each cache slot, based on the size of the
      tokenizer's input range. */
  int slotSize;
  /** Cache of line-counting position results. */
  volatile s2_ptoker_lccache_entry lines[
     10
     /* reminders to self: 10 or 20 actually, in the 20191228
        amalgamated s2 unit tests, perform slightly better than 60
        does. Multiple tests show 10 to be an all-around good
        value. */
  ];
};
typedef struct s2_ptoker_lccache s2_ptoker_lccache;
/**
   Empty-initialized s2_ptoker_lccache structure, intended for
   const-copy initialization.
*/
#define s2_ptoker_lccache_empty_m \
  {0,0,{                             \
    s2_ptoker_lccache_entry_empty_m, s2_ptoker_lccache_entry_empty_m,  \
    s2_ptoker_lccache_entry_empty_m, s2_ptoker_lccache_entry_empty_m,  \
    s2_ptoker_lccache_entry_empty_m, s2_ptoker_lccache_entry_empty_m,  \
    s2_ptoker_lccache_entry_empty_m, s2_ptoker_lccache_entry_empty_m,  \
    s2_ptoker_lccache_entry_empty_m, s2_ptoker_lccache_entry_empty_m  \
 }}

/**
   The s2_ptoker class is a simple basis for a tokenizer, largely
   syntax- and language-independent. Its origins go back many years
   and several projects.

   This tokenizer requires that all input be available in advance of
   tokenization and remain valid for its own lifetime.

   @see s2_ptoker_init()
   @see s2_ptoker_next_token()
   @see s2_ptoker_lookahead()
   @see s2_ptoker_putback()
   @see s2_ptoker_next_token_set()
*/
struct s2_ptoker {
  /**
     Used for memory management in "v2" operations. Will be NULL for
     "historical-style" instances.

     We might want to change this to an s2_engine, but then we'd have
     a circular dependency, and those make me lose sleep.
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
     errors. Set by s2_ptoker_next_token().
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
     Used for calculating line/col info for sub-parsing errors.
  */
  s2_ptoker const * parent;

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
  s2_linecol_t lineOffset;

  /**
     Column counterpart of lineOffset.
  */
  s2_linecol_t colOffset;

  /**
     1-based current tokenization line number.

     This is maintained by s2_ptoker_next_token(), updated
     as it crosses newline boundaries.

     This can only be tracked properly as long as
     s2_ptoker_next_token() is called linearly. Once clients (e.g. s2
     internals) starts jumping around the input, this counting breaks.
  */
  s2_linecol_t currentLine;
  /**
     0-based current tokenization column number.

     This is maintained by s2_ptoker_next_token(). See
     notes in this->currentLine.
  */
  s2_linecol_t currentCol;

  /**
     Flags which may change how this object tokenizes.
  */
  cwal_flags32_t flags;
  
  /**
     The current token. Its state is set up thusly:

     Initially, token.begin must be this->begin and token.end
     must be 0. That state is used by s2_ptoker_next_token() to
     recognize the initial token state and DTRT.

     During tokenization, this object's state is updated to reflect
     the range from [this->begin, this->end) matching a token (or
     an error position, in the case of a tokenization error).
  */
  s2_ptoken token;

  /**
     The put-back token. s2_ptoker_next_token() copies this->token to
     this object before attempting any tokenization.
     s2_ptoker_putback() copies _pbToken over this->token and clears
     _pbToken.

     Do not manipulate this directly. use s2_ptoker_putback(),
     s2_ptoker_putback_get(), and (if really needed)
     s2_ptoker_putback_set().
  */
  s2_ptoken _pbToken;

  /**
     An experiment in cutting down on tokenization. Token lookahead
     ops, and some client code, set this after they have done a
     lookahead, conditionally setting this to that looked-ahead
     token. s2_ptoker_next_token() will, if this is set, use this
     token and skip the tokenization phase. This member is cleared by
     s2_ptoker_token_set() and s2_ptoker_next_token().

     Do not manipulate this directly: use s2_ptoker_next_token_set()
     to modify it.
  */
  s2_ptoken _nextToken;

  /**
     Used for marking an error position, which is part of the line/col
     counting mechanism used for error reporting.

     Do not manipulate this directly. Use s2_ptoker_errtoken_get(),
     s2_ptoker_errtoken_set(), and s2_ptoker_errtoken_has().
  */
  s2_ptoken _errToken;

  /**
     These tokens are used to capture a range of tokens solely for
     their string content. _Some_ APIs set this to a range encompasing
     all input which they consume. e.g. it can be used to record the
     whole result of multiple s2_ptoker_next_token() calls by setting
     capture.begin to the start of the first token and capture.end the
     end of the last token captured. The s2_t10n-internal APIs do not
     manipulate this member.

     We model capturing as a token range, rather than a simple byte
     range, with the hopes that this approach will work with the
     eventual addition of compiled tokens.
  */
  struct {
    /** First token in the capture range. */
    s2_ptoken begin;
    /**
       The one-after-the-end token in the capture range. Note that
       this means that the string range of the capture is
       [this->begin.begin, this->end.begin). For completely empty
       ranges this token will be the same as this->begin.
    */
    s2_ptoken end;
  } capture;

  
  /**
     Internal implementation detail for the line-counting results
     cache. Full disclosure: in order to avoid having to modify the
     signatures of a metric boatload of functions to make their
     (s2_ptoker const *) parameters non-const, this member may be
     modified in/via one routine where this s2_ptoker instance is
     otherwise const. Const-correct behaviour would require that we
     make a whole family of downstream functions non-const just to
     account for this internal optimization, which i'm not willing to
     do (as much as i love my const). This is where we could use the
     "mutable" keyword in C++, but, alas, this is C89.
  */
  s2_ptoker_lccache _lcCache;
};
/** Empty-initialized s2_ptoker object. */
#define s2_ptoker_empty_m {                     \
    0/*e*/, 0/*begin*/,0/*end*/,                 \
    0/*errMsg*/,                              \
    0/*name*/,                                \
    0/*parent*/,                              \
    0/*compiled*/,                              \
    0/*lineOffset*/,0/*colOffset*/,               \
    1/*currentLine*/,0/*currentCol*/,0/*flags*/,  \
    s2_ptoken_empty_m/*token*/,               \
    s2_ptoken_empty_m/*_pbToken*/,             \
    s2_ptoken_empty_m/*_nextToken*/,             \
    s2_ptoken_empty_m/*_errToken*/,              \
    {s2_ptoken_empty_m,s2_ptoken_empty_m}/*capture*/, \
    s2_ptoker_lccache_empty_m/*_lcCache*/          \
  }
/** Empty-initialized s2_ptoker object. */
extern const s2_ptoker s2_ptoker_empty;

/**
   Flags for use with s2_ptoker::flags and possibly
   related contexts.
*/
enum s2_t10n_flags {
/**
   Sentinel value. Must be 0.
*/
S2_T10N_F_NONE = 0,
/**
   If set, the '-' character is considered a legal identifier by
   s2_read_identifier2() except when the '-' appears at the start of
   the input.
*/
S2_T10N_F_IDENTIFIER_DASHES = 1,

/**
   An internal-only flag which signifies to error-handling routines
   that the given s2_ptoker is only ever used in a strictly linear
   fashion, allowing such routines to take advantage of line/column
   state of the tokenizer and avoid counting lines for error
   reporting. Note that the overwhelming majority of s2_ptokers in s2
   are NOT purely linear and it's normally impossible to know in
   advance whether one will be or not. In such cases it will use the
   position of either its error token or current token, in that order.

   Reminder to self: this was added to support s2_cptoker
   experimentation, as s2_cptoker internally uses s2_ptoker and
   "compilation" but tokenizes all the input before "compiling" it.
   This flag allows errors in that step to be reported without an
   extra line-counting step.
*/
S2_T10N_F_LINEAR_TOKER = 0x10
};

/**
   Must be passed a s2_ptoker and its input source. If len
   is negative then the equivalent of strlen() is used to calculate
   its length.

   Returns 0 on success, CWAL_RC_MISUSE if !t or !src. It has no other
   error contditions, so "cannot fail" if its arguments are valid.

   Use s2_ptoker_next_token() to fetch the next token, s2_ptoker_lookahead()
   to "peek" at the next token, and s2_ptoker_putback() to put a just-fetched
   token back.

   An s2_ptoker instance initialized via this interface does not
   strictly need to be passed to s2_ptoker_finalize(), but there is no
   harm in doing so.
*/
int s2_ptoker_init( s2_ptoker * t, char const * src, cwal_int_t len );

/**
   INCOMPLETE - DO NOT USE.

   Must be passed a s2_ptoker and its input source. If len is negative
   then the equivalent of strlen() is used to calculate its
   length. This routine, unlike s2_ptoker_init(), may allocate memory
   and may pre-parse its input, and can therefore fail in a number of
   ways. Thus its result code must always be checked.

   Calling this obligates the caller to eventually pass t to
   s2_ptoker_finalize(), regardless of whether this routine succeeds
   or not, to free up any resources this routine may have allocated.

   Returns 0 on success, CWAL_RC_MISUSE if !t or !src.

   Use s2_ptoker_next_token() to fetch the next token, s2_ptoker_lookahead()
   to "peek" at the next token, and s2_ptoker_putback() to put a just-fetched
   token back.
*/
int s2_ptoker_init_v2( cwal_engine * e, s2_ptoker * t, char const * src, cwal_int_t len,
                       uint32_t flagsCurrentlyUnused );

/**
   Resets the tokenization state so that the next call to
   s2_ptoker_next_token() will start at the beginning of the
   input. This clears the putback and error token state, and between
   this call and the next call to s2_ptoker_next_token() (or similar),
   the current token state is invalid.
*/
void s2_ptoker_reset( s2_ptoker * t );

/**
   Clears t's state and frees any memory is may be using. This does
   not free t, but will free any memory allocated on its behalf.

   Note that if any sub-tokenizers which derive from t (i.e. have t
   set as their s2_ptoker::parent value), they MUST be cleaned up
   first, as they may refer to memory owned by a parent.

   It is safe to pass this function an instance which was not passed
   to s2_ptoker_init() or s2_ptoker_init_v2() if (and only if) the
   instance was copy-initialized from s2_ptoker_empty or
   s2_ptoker_empty_m.
*/
void s2_ptoker_finalize( s2_ptoker * t );

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
   pass t to s2_ptoker_finalize(). If it fails, t will be finalized if
   needed. If this routine fails, passing t to s2_ptoker_finalize() is
   a harmless no-op if t was initially copy-initialized from
   s2_ptoker_empty or s2_ptoker_empty_m (i.e. it's in a well-defined
   state).
*/
int s2_ptoker_sub_from_toker( s2_ptoker const * parent,
                              s2_ptoker * dest);

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
int s2_ptoker_sub_from_token( s2_ptoker const * parent, s2_ptoken const * token,
                              s2_ptoker * sub );

/* s2_ptoker const * s2_ptoker_root( s2_ptoker const * t ); */

/**
   Returns the top-most object from t->parent, or t if !t->parent.
*/
s2_ptoker const * s2_ptoker_top_parent( s2_ptoker const * t );

/**
   Returns either t->name or the first name encountered while climbing
   up the t->parent chain. Returns 0 if no name is found. If len is
   not 0 then if this function returns non-0, len is set to that
   name's length.
*/
char const * s2_ptoker_name_first( s2_ptoker const * t, cwal_size_t * len );
/**
   Returns the top-most name from t and its parent chain.
   Returns 0 if no name is found.

   FIXME: add (cwal_size_t * len) parameter.
*/
char const * s2_ptoker_name_top( s2_ptoker const * t );

/**
   Returns the first explicit non-0 error position marker from t or
   its nearest ancestor (via t->parent). If the 2nd argument is not
   NULL, *foundIn is assigned to the tokenizer in which the error
   position was found.

   @see s2_ptoker_err_pos()
*/
char const * s2_ptoker_err_pos_first( s2_ptoker const * t,
                                      s2_ptoker const ** foundIn);

/**
   Tries to return an "error position" for the given tokenizer, under
   the assumption that something has just "gone wrong" and the client
   wants to know where (or whereabouts) it went wrong. It first tries
   s2_ptoker_err_pos_first(). If that returns NULL, it returns either
   the position of pt's current token or putback token. If those are
   NULL, pt->begin is returned (indicating that tokenization has not
   yet started or failed on the first token).

   @see s2_ptoker_err_pos_first()
*/
char const * s2_ptoker_err_pos( s2_ptoker const * pt );

/**
   Fetches the next token from t. t must have been successfully
   intialized using s2_ptoker_init().

   This function is intended to be called repeatedly until it either
   returns 0 (success) AND has (t->ttype==S2_T_EOF) or until it
   returns non-0 (error). On each iteration, clients should collect
   any of the token information they need from t->token before calling
   this again (which changes t's state).

   Note that this is a lower-level function than s2_next_token().
   That one builds off of this one.
   
   On success 0 is returned and t is updated as follows:

   t->token.begin points to the start of the token. t->token.end
   points to the one-past-the-end character of the token, so the
   length of the token is (t->token.end - t->token.begin). t->ttype
   will be set to one of the S2_T_xxx constants.

   At the end of input, t->ttype will be S2_T_EOF and the token
   length with be 0 (t->begin==t->end).

   On error non-0 is returned, t->ttype will be S2_T_TokErr, and
   t->errMsg will contain a basic description of the error. On success
   t->errMsg will be 0, so clients may use that to check for errors
   instead of checking the result code or token type S2_T_TokErr. The
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
   token, and the token can be "put back" by calling s2_ptoker_putback().

   Use s2_ptoker_lookahead() to "peek" at the next token while keeping
   the token iterator in place.

   Any tokenization error is assumed to be unrecoverable, and it is
   not normally useful to call this again (after an error) without
   re-initializing the tokenizer first.
*/
int s2_ptoker_next_token( s2_ptoker * t );

/**
   Sets pt's next token to be a bitwise copy of tk, such that the next
   call to s2_ptoker_next_token() will evaluate to that token. It is
   up to the caller to ensure that tk is valid for pt (e.g. by having
   read tk via a call to s2_ptoker_lookahead()).

   This is an optimization useful when one has looked-ahead in pt and
   determined that the looked-ahead token should be the next-consumed
   token. Using s2_ptoker_putback() would also be an option, but this
   approach is more efficient because it eliminates the
   re-tokenization of the put-back token.

   Note that this does not modify the putback token. If
   s2_ptoker_next_token() is called immediately after this, the
   putback token will be set to whichever token was most recently
   consumed before *this* routine was called (i.e. exactly the same as
   would happen without this routine having been called).

   @see s2_ptoker_next_token()
*/
void s2_ptoker_next_token_set( s2_ptoker * const pt, s2_ptoken const * const tk );

/**
   For a given s2_token_types values, this returns a unique
   string representation of its type ID. The returned bytes
   are static. Returns 0 for an unknown value.

   This function serves two main purposes:

   1) So that we can let gcc warn us if any values in s2_token_types
   collide with one another. (Implementing it caught two collisions.)

   2) Help with debugging. The strings returned from this function are
   intended for "informational" or "entertainment" value only, not
   (de)serialization.
*/
char const * s2_ttype_cstr( int ttype );

/**
   Similar to s2_ptoker_next_token(), but it skips over any tokens for
   which s2_ttype_is_junk() returns true. On returning, st->token
   holds the last-tokenized position.

   After this call, the putback token will be the previous token
   read before this call. i.e. the intervening junk tokens are not
   placed into the putback token.
*/
int s2_ptoker_next_token_skip_junk( s2_ptoker * st );

/**
   If st has no putback token, 0 (false) is returned and this function
   has no side-effects, otherwise st->token is replaced by the putback
   token, the putback token is cleared, and non-0 (true) is returned.

   Note that s2_ptoker_next_token_set() is often a more efficient
   option, provided semantics allow for it (which they don't always
   do). After calling this routine, s2_ptoker_next_token() must
   re-tokenize the next token, whereas s2_ptoker_next_token_set()
   allows s2_ptoker_next_token() to bypass that tokenization step.
*/
char s2_ptoker_putback( s2_ptoker * st );

/**
   Sets a bitwise copy of tok as the current putback token. It is up
   to the caller to ensure that tok is valid for the given tokenizer's
   current state.
*/
void s2_ptoker_putback_set( s2_ptoker * const st, s2_ptoken const * const tok );

/**
   Returns st's current putback token, which should be bitwise
   copied by the caller because its state may change on any calls
   to the s2_ptoker API.
*/
s2_ptoken const * s2_ptoker_putback_get( s2_ptoker const * const st );

/**
   Returns st's current error token. It never returns NULL - the
   memory is owned by st and may be modified by any future calls into
   its API. The token will have a ttype of S2_T_INVALID, and no
   begin/end values, if there is no error.
*/
s2_ptoken const * s2_ptoker_errtoken_get( s2_ptoker const * st );

/**
   Bitwise copies tok to be st's current error token. If tok is NULL,
   the error token is cleared, else it is up to the caller to ensure
   that the token is valid for st's current state.

   Note that setting an error token does not trigger an error - it is
   intended only to mark the part of the tokenizer which might be
   associated with an error, for downstream error reporting of that
   position.
*/
void s2_ptoker_errtoken_set( s2_ptoker * const st, s2_ptoken const * const tok );

/**
   Returns non-0 if st has an error token set which points to somewhere
   in st's range, else returns 0.
*/
char s2_ptoker_errtoken_has( s2_ptoker const * st );

/**
   Uses s2_ptoker_next_token() to fetch the next token, sets *tgt to the
   state of that token, resets the tokenizer position to its
   pre-call state, and returns the result of the s2_ptoker_next_token()
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

   @see s2_ptoker_lookahead_skip()
*/
int s2_ptoker_lookahead( s2_ptoker * st, s2_ptoken * tgt );


/**
   A function signature for predicates which tell the caller whether
   a s2_token_types value meets (or does not meet) a certain
   condition.

   Implementations must return ttype if ttype meets their
   predicate condition(s), else false (0). Note that S2_T_INVALID
   is guaranteed by the API to be 0.
*/
typedef int (*s2_ttype_predicate_f)( int ttype );

/**
   Similar to s2_ptoker_lookahead(), but it skips over any leading
   tokens for which pred() returns true. On success *tgt contains
   the content of the token which either failed the predicate or is
   an EOF token. The client can force st to that tokenization
   position by passing it to s2_ptoker_token_set().

   Before this function returns, st->token and st->_pbToken are
   restored to their pre-call state and st->_nextToken is cleared. It
   is legal for tgt to be st->_nextToken, and that will take precedence
   over clearing that value.
*/
int s2_ptoker_lookahead_skip( s2_ptoker * st, s2_ptoken * tgt,
                              s2_ttype_predicate_f pred );

  
/**
   Works like s2_ptoker_lookahead_skip(), but inverts the meaning of
   the predicate: it stops at the first token for which pred()
   returns true.
*/
int s2_ptoker_lookahead_until( s2_ptoker * st, s2_ptoken * tgt,
                               s2_ttype_predicate_f pred );


/**
   Sets st->_pbToken to st->token, st->token to *t, and clears
   st->_nextToken.
*/
void s2_ptoker_token_set( s2_ptoker * st, s2_ptoken const * t );

#if 0
/**
   Returns a pointer to st's current token, the state of which may
   change on any any future calls into the s2_ptoker API. Never
   returns NULL.

   Though the pointer to this token currently (20200105) never changes
   (only its state does), clients are encouraged to behave as if it
   might change, as "planned potential changes" to the API may well
   make that the case.
*/
s2_ptoken const * s2_ptoker_token_get( s2_ptoker const * st );
/**
   Returns the token type (ttype) of st's current token.
*/
int s2_ptoker_ttype( s2_ptoker const * st );
#endif

/**
   Returns st->token.ttype if st's current token represents an EOF.
*/
int s2_ptoker_is_eof( s2_ptoker const * st );

/**
   Returns st->token.ttype if st's current token represents an
   end-of-expression.
*/
int s2_ptoker_is_eox( s2_ptoker const * st );

/**
   Returns ttype if ttype is an end-of-line token.
*/
int s2_ttype_is_eol( int ttype );

/**
   Returns ttype if ttype represents a "space" token
   (in any of its various incarnations).
*/
int s2_ttype_is_space( int ttype );

/**
   Returns ttype if the given token type is considered a "junk" token
   (with no syntactical meaning).

   Junk includes the following token types:

   ASCII 32d (SPACE), ASCII 13d (CR), ASCII 9d (TAB),
   S2_T_Blank, S2_T_CommentC, S2_T_CommentCpp,
   S2_T_Whitespace, S2_T_Shebang, S2_T_UTFBOM

   Note that S2_T_NL/S2_T_EOL (newline/EOL) is not considered junk
   here, as it is an expression separator in some contexts and
   skippable in others.

   Potential TODO: treat S2_T_CommentCpp as an EOL because this type
   of token implies one.
*/
int s2_ttype_is_junk( int ttype );

/**
   Returns ttype if ttype is a basic assignment op:

   S2_T_OpAssign, S2_T_ArrayAppend (internally treated as
   assignment).
*/
int s2_ttype_is_assignment( int ttype );

/**
   Returns ttype if ttype refers to one of the "combo assignment"
   operators, e.g. +=, -=, *=, etc.
*/
int s2_ttype_is_assignment_combo( int ttype );

/**
   Returns ttype if op is 0 or represents an operator which
   may legally directly proceed a unary operator.
*/
int s2_ttype_may_precede_unary( int ttype );

/**
   Returns ttype if ttype represents a symbol that unambiguously marks
   the end of an expression:

   S2_T_EOF, S2_T_EOX, S2_T_Semicolon
*/
int s2_ttype_is_eox( int ttype );

/**
   Returns ttype if ttype represents an EOF (or virtual EOF) token.
*/
int s2_ttype_is_eof( int ttype );

/**
   Returns ttype if ttype presents a "group" type:

   S2_T_ParenGroup, S2_T_BraceGroup, S2_T_SquigglyBlock
*/
int s2_ttype_is_group( int ttype );

/**
   Returns ttype if it respends a token whose value can be converted
   to a cwal_value with ease.
*/
int s2_ttype_is_object_keyable( int ttype );

/**
   Returns ttype if it represents an operator which
   is (in principal) capable of short-circuiting part
   of its arguments:

   S2_T_OpOr, S2_T_OpAnd, S2_T_Question (in the context of ternary
   if).
*/
int s2_ttype_short_circuits( int ttype );

/**
   Returns ttype if ttype represents a type which is a unary operator
   which requires an *identifier* as its operand:

   S2_T_OpIncr, S2_T_OpDecr, S2_T_OpIncrPre, S2_T_OpIncrPost,
   S2_T_OpDecrPre, S2_T_OpDecrPost

   Noting that the pre/post operator name pairs initially represent
   the same token (e.g. S2_T_OpIncrPre (++x) vs S2_T_OpIncrPost
   (x++)), requiring eval-time context to be able to distinguish
   between prefix and postfix forms.

   If it is not, 0 is returned.
*/
int s2_ttype_is_identifier_prefix( int ttype );

/**
   Returns ttype if ttype is S2_T_LiteralIntDec or one of its
   non-decimal counterparts, else returns 0.
*/
int s2_ttype_is_int( int ttype );

/**
   Works like s2_ttype_is_int(), but includes S2_T_LiteralDouble as a
   matching type.
*/
int s2_ttype_is_number( int ttype );

/**
   If pos is in the range [src,end) then this function calculates
   the line (1-based) and column (0-based) of pos within [src,end)
   and sets line/col to those values if those pointers are not
   NULL. If pos is out of range CWAL_RC_RANGE is returned and
   this function has no side-effects. Returns 0 on success.

   Note that s2 globally follows emacs line/column conventions:
   lines are 1-based and columns are 0-based.
*/
int s2_count_lines( char const * src, char const * end_,
                    char const * pos_,
                    s2_linecol_t *line, s2_linecol_t *col );

/**
   Wrapper around s2_count_lines(), which uses [pt->begin, pt->end)
   as the source range.
*/
int s2_ptoker_count_lines( s2_ptoker const * pt, char const * pos,
                           s2_linecol_t * line, s2_linecol_t * col );

/**
   Incomplete/untested. Don't use.

   tok must be a token from pt.

   This variant tries to use line/column info embedded in tok before
   falling back to counting "the hard way". Note that tok->line and
   tok->col are relative to pt->begin, and pt (or one of its parents)
   may have line/column offset info to apply to the results, making
   the line/col relative to the top-most s2_ptoker in the hierarchy.

   Bug: the tok->line/column counting is known to not work as expected
   because of how we hop around while parsing :/.
*/
int s2_ptoker_count_lines2( s2_ptoker const * pt,
                            s2_ptoken const * tok,
                            s2_linecol_t * line, s2_linecol_t * col );

#if 0
/**
   Collects info from pt which is useful in error reporting. pos is expected
   to be a position within [pt->begin,pt->end). Its line/column position
   is calculated as for s2_count_lines() (so *line will be 0 if pos is out
   of range). If pos is 0 then pt->errPos is used.

   If name is not NULL, *name is set to the value returned from
   s2_ptoker_name_top().

   Any of the (name, line, col) parameters may be 0.
*/
void s2_ptoker_err_info( s2_ptoker const * pt,
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
int s2_unescape_string( cwal_engine * e,
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

   @see s2_read_identifier2()
*/
int s2_read_identifier( char const * zPos, char const * zEnd,
                        char const ** zIdEnd );

/**
   An alternate form of s2_read_identifier() which honors the
   S2_T10N_F_IDENTIFIER_DASHES flag if it is set in the final
   argument. If that flag is not set it behaves as documented for
   s2_read_identifier().

   @see s2_read_identifier()
*/
int s2_read_identifier2( char const * zPos, char const * zEnd,
                         char const ** zIdEnd,
                         uint32_t flags );

/**
   Returns true if ch is one of:
       
   ' ' (ASCII 32d), \\t (ASCII 9d)
*/
char s2_is_blank( int ch );

/**
   Returns true if s2_is_blank(ch) is true of if ch is one of:

   \\n (ASCII 10d), \\r (13d), \\v (11d), \\f (12d)
*/
char s2_is_space( int ch );

/**
   Returns true if ch is-a digit character (0..9).
*/
char s2_is_digit( int ch );

/**
   Returns non-0 if ch is-a hexidecimal digit character (0..9,
   a..F, A..F), else returns 0.
*/
char s2_is_xdigit( int ch );

/**
   Returns non-0 if character ch is an octal digit character (0..7),
   else returns 0.
*/
char s2_is_octaldigit( int ch );

/**
   Returns non-0 if ch is an ASCII alphabetic character (a..z,
   A..Z), else returns 0.
*/
char s2_is_alpha( int ch );

/**
   Returns non-0 if s2_is_alpha(ch) or s2_is_digit(ch), else returns
   0.
*/
char s2_is_alnum( int ch );

/**
   Checks whether tok's range contains only "junk" tokens or not. If
   tok's range contains only noise tokens, 0 is returned, otherwise
   the token type ID of the first non-noise token is returned. Note
   that it also returns 0 if there is a tokenization error. This is
   primarily used to determine whether "super-tokens" (e.g.
   parenthesis/brace groups) contain any usable content before
   attempting to evaluate them.
*/
int s2_ptoken_has_content( s2_ptoken const * tok );

/**
   If token->begin is not 0 and less than token->end, then (token->end
   - token->begin) is returned, else 0 is returned (which is not a
   valid token length except for an EOF token).
*/
#define s2_ptoken_len(TOK) \
  ((cwal_size_t)((s2_ptoken_begin(TOK)            \
    && s2_ptoken_end(TOK) > s2_ptoken_begin(TOK)) \
  ? (cwal_size_t)(s2_ptoken_end(TOK) - s2_ptoken_begin(TOK)) \
                 : 0))
/*cwal_size_t s2_ptoken_len( s2_ptoken const * token );*/

/**
   If the given token has an "adjusted" begin/end range, this function
   returns the length of that range, else it behaves identically to
   s2_ptoken_len().

   Typically only group-level tokens and heredocs have an adjusted
   range.
*/
#define s2_ptoken_len2(TOK)                         \
  ((cwal_size_t)(((s2_ptoken_adjbegin(TOK) &&       \
    s2_ptoken_adjbegin(TOK)<=s2_ptoken_adjend(TOK)) \
    ? (cwal_size_t)(s2_ptoken_adjend(TOK) - s2_ptoken_adjbegin(TOK)) \
                  : s2_ptoken_len(TOK))))
/*cwal_size_t s2_ptoken_len2( s2_ptoken const * token );*/

/**
   If t is the result of s2_ptoker_next_token() and is any of the
   following types then this sets *rc (if rc is not NULL) to its
   integer representation and returns true: S2_T_LiteralIntOct,
   S2_T_LiteralIntDec, S2_T_LiteralIntHex, S2_T_LiteralIntBin. Results
   are undefined if t's state does not conform to the internal
   requirements for one of the above-listed t->ttype values (i.e. t's
   state must have been set up by s2_ptoker_next_token() or a relative
   of that function).

   Returns false (0) if all conditions are not met or if the
   conversion catches a syntax error which the tokenizer did not (but
   that "should not happen", and may trigger an assert() in debug
   builds).

   ACHTUNG: this does not handle a leading sign, as the sign is, at
   this level of the tokenization API, a separate token.

   @see s2_ptoken_parse_double()
   @see s2_cstr_parse_double()
   @see s2_cstr_parse_int()
*/
char s2_ptoken_parse_int( s2_ptoken const * t, cwal_int_t * rc );

/**
   The double counterpart of s2_ptoken_parse_int(), but only parses
   tokens with ttype S2_T_LiteralDouble. Results are undefined if
   t->ttype is S2_T_LiteralDouble but the byte range referred to by t
   is not a valid double token.

   ACHTUNG: this does not handle a leading sign, as the sign is, at
   this level of the tokenization API, a separate token.

   @see s2_ptoken_parse_int()
   @see s2_cstr_parse_double()
   @see s2_cstr_parse_int()
*/
char s2_ptoken_parse_double( s2_ptoken const * t, cwal_double_t * rc );

/**
   Works like s2_ptoken_parse_int() except that:

   - slen specifies the input length. If slen <0 then cwal_strlen(str)
     is used to calculate the input length.

   - It uses all bytes in the range [str, str+slen) as input and will
   fail if the input contains anything other than a numeric value,
   optionally with leading space.

   - It accepts a leading sign character, regardless of the integer
   notation (decimal, hex, octal). It ignores spaces around the sign
   character.

   @see s2_ptoken_parse_double()
   @see s2_ptoken_parse_int()
   @see s2_cstr_parse_double()
*/
char s2_cstr_parse_int( char const * str, cwal_int_t slen, cwal_int_t * result );

/**
   The double counterpart of s2_cstr_parse_int().

   @see s2_ptoken_parse_double()
   @see s2_ptoken_parse_int()
   @see s2_cstr_parse_int()
*/
char s2_cstr_parse_double( char const * str, cwal_int_t slen,
                           cwal_double_t * result );

/**
   Expects a filename-like string in the first slen bytes of str, with
   directory components separated by either '/' or '\\' (it uses the
   first of those it finds, in that order, as the separator). If any
   separator is found, a pointer to the last instance of it in str is
   returned, otherwise 0 is returned.
*/
char const * s2_last_path_sep(char const * str, cwal_size_t slen );

/**
   If tok->ttype is S2_T_Identifier and the token's contents are one
   of (true, false, null, undefined) then the corresponding built-in
   cwal value is returned (e.g. cwal_value_true() or
   cwal_value_null()), else NULL is returned. tok may not be NULL. The
   returned value, if not NULL, is guaranteed to be a shared instance
   which lives outside of the normal cwal_value lifetime management.
*/
cwal_value * s2_ptoken_is_tfnu( s2_ptoken const * tok );

/**
   Returns a pointer to the token's contents, setting its length to
   *len if len is not NULL. tok must not be NULL. Note that the
   returned string is almost certainly not NUL-terminated (or
   terminates at the very end of the s2_ptoker from which tok
   originates), thus capturing the length is normally required.

   This does not strip any leading/spaces of tokens which have been
   "adjusted" to do so. i.e. the whole token's contents are part of
   the returned byte range.
*/
char const * s2_ptoken_cstr( s2_ptoken const * tok,
                             cwal_size_t * len );

/**
   If tok has an "adjusted" begin/end range, this returns a pointer to
   the start of that range and len, if not NULL, is assigned the
   length of that range. If it has no adjusted range, it functions
   identically to s2_ptoken_cstr().

   Typically only group-level tokens and heredocs have an adjusted
   range.
*/
char const * s2_ptoken_cstr2( s2_ptoken const * tok,
                              cwal_size_t * len );

/**
   If st->capture is set up properly, this returns a pointer to the
   start of the range and *len (if not NULL) is set to the length of
   the captured range. If no capture is set, or it appears to be
   invalid, NULL is returned.
*/
char const * s2_ptoker_capture_cstr( s2_ptoker const * st,
                                     cwal_size_t * len );

/**
   A helper type for tokenizing conventional PATH-style strings.
   Initialize them with s2_path_toker_init() and iterate over them
   with s2_path_toker_next().
*/
struct s2_path_toker {
  /** Begining of the input range. */
  char const * begin;
  /** One-after-the-end of the input range. */
  char const * end;
  /** Position for the next token lookup. */
  char const * pos;
  /** List of token separator characters (ASCII only). */
  char const * separators;
};
typedef struct s2_path_toker s2_path_toker;
/**
   Default-initialized s2_path_toker instance, intended for const-copy
   initialization. On Windows builds its separators member is set to
   ";" and on other platforms it's set to ":;".
*/
#if defined(S2_OS_WINDOWS)
#  define s2_path_toker_empty_m {NULL,NULL,NULL,";"}
#else
#  define s2_path_toker_empty_m {NULL,NULL,NULL,":;"}
#endif

/**
   Default-initialized s2_path_toker instance, intended for
   copy initialization.

   @see s2_path_toker_empty_m
*/
extern const s2_path_toker s2_path_toker_empty;

/**
   Wipes out pt's current state by copying s2_path_toker_empty over it
   and initializes pt to use the given path as its input. If len is 0
   or more then it must be the length of the string, in bytes. If len
   is less than 0, cwal_strlen() is used to determine the path's
   length.  (When dealing with inputs which are not NUL-terminated,
   it's critical that the user pass the correct non-negative length.)

   If the client wants to modify pt->separators, it must be done so
   *after* calling this.

   Use s2_path_toker_next() to iterate over the path entries.
*/
void s2_path_toker_init( s2_path_toker * pt, char const * path, cwal_int_t len );

/**
   Given a s2_path_toker which was formerly initialized using
   s2_path_toker_init(), this iterates over the next-available path
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
   s2_path_toker pt = s2_path_toker_empty;
   s2_path_toker_init(&pt, path, pathLen);
   while(0==s2_path_toker_next(&pt, &t, &tLen)){
      // The next element is the tLen bytes of memory starting at t:
      printf("Path element: %.*s\n", (int)tLen, t);
   }
   @endcode
*/
int s2_path_toker_next( s2_path_toker * pt, char const ** token, cwal_size_t * len );

/*
  The following macros are part of an ongoing porting/abstraction
  effort to eliminate direct access to token members, the eventual
  goal being to permit the token type to be easily swapped out so that
  we can enable "compiled" (pre-parsed) chains of tokens (which would
  require far more memory than s2 currently uses but would be tons
  faster for most scripts).
*/
#define s2_ptoken_begin(P) ((P)->_begin)
#define s2_ptoken_begin2(P) ((P)->_adjBegin ? (P)->_adjBegin : (P)->_begin)
#define s2_ptoken_begin_set(P,X) (P)->_begin = (X)
#define s2_ptoken_end(P) ((P)->_end)
#define s2_ptoken_end2(P) ((P)->_adjEnd ? (P)->_adjEnd : (P)->_end)
#define s2_ptoken_end_set(P,X) (P)->_end = (X)
#define s2_ptoken_adjbegin(P) ((P)->_adjBegin)
#define s2_ptoken_adjbegin_set(P,X) (P)->_adjBegin = (X)
#define s2_ptoken_adjbegin_incr(P,X) (P)->_adjBegin += (X)
#define s2_ptoken_adjend(P) ((P)->_adjEnd)
#define s2_ptoken_adjend_set(P,X) (P)->_adjEnd = (X)
#define s2_ptoken_adjend_incr(P,X) (P)->_adjEnd += (X)

#define s2_ptoker_begin(PT) ((PT)->begin)
#define s2_ptoker_end(PT) ((PT)->end)
#define s2_ptoker_token(PT) (PT)->token
#define s2_ptoker_len(PT) ((PT)->end - (PT)->begin)

#ifdef __cplusplus
}/*extern "C"*/
#endif
#endif
/* include guard */
