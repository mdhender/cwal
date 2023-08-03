/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/** @file tok1.h

    This file contains a mishmash of tokenization-related bits which
    have been copied and pasted and mutated over a period of about 20
    years (as of this writing, March. 2022). This particular iteration
    is not standalone code, and is closely tied to the expectations
    and conventions of the whcl project, though its documentation,
    much of it 10+ years old, may imply otherwise.

    In the whcl scheme of things, tok1 plays the part of base-level
    tokenizer for the whcl_script API. tok1's interface is easier to
    tokenize with but tok2 provides the higher-level "compiled" tokens
    in a more memory-compact, faster-to-eval form.
*/
#ifndef NET_WANDERINGHORSE_CWAL_WHCL_TOK1_H_INCLUDED_
#define NET_WANDERINGHORSE_CWAL_WHCL_TOK1_H_INCLUDED_
#include "libcwal.h"
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif
typedef struct tok1_izer tok1_izer;
typedef struct tok1_en tok1_en;

/**
   Numeric type used for counting script line and column numbers.
   Note that we aim for a 16-bit type to shave a few bytes from
   oft-used token types. As of this writing (20200105), the single
   largest s2 script (its amalgamated unit tests) is right at 5400 lines
   (size=160kb), so any dreams of scripts with more than 64k lines
   would seem to be... ah... somewhat ambitious.
*/
typedef uint16_t tok1_linecol_t;

/**
   s2 token type and operator IDs.

   Values >=0 and <=127 can be translated literally to their
   equivalent char value. Values over 127 are symbolic, not
   necessarily mapping to a single byte nor a Unicode code point with
   the same value. (There are very likely numerous collisions with
   Unicode code points in this enum.)

   @see tok1_t_cstr()
*/
enum tok1_en_types_e {
/**
   For use as the token type when a tok1_next_token_f() impl returns
   non-0.
*/
TOK1_T_TokErr = -2,

/**
   The generic EOF marker. Used by tok1_next_token_f() when the
   end of the tokenizer's input range is reached. Note that this
   token is also used for "virtual" EOF and does NOT necessarily map
   to a NUL byte in the input. e.g. when sub-parsing part of a
   larger expression, the subexpression will get a subset of the
   parent range to parse, and its virtual EOF will be part of its
   parent parser's input range.
*/
TOK1_T_EOF = -1,

/**
   TOK1_T_INVALID is guaranteed by the API to be the entry in this
   enum with the value 0, whereas the concrete values for other
   non-ASCII-range tokens is unspecified except that they are
   guaranteed to be non-0. Note that ASCII NUL value is not
   represented as a token: this tokenizer does not support them in
   input. If we ever add the, they will get ID 200, to follow the
   conventions laid out later on in this enum.
*/
TOK1_T_INVALID = 0,

TOK1_T_Tab = 9,
TOK1_T_NL = 10,
TOK1_T_VTab = 11,
TOK1_T_FF = 12,
TOK1_T_CR = 13,
TOK1_T_At = 64 /* \@ */,
TOK1_T_AtExpand = 264 /* \@... */,
/**
   Generic EOL token, for \\r, \\n, and \\r\\n.

   Whether or not newlines end an expression is (or should be)
   context-dependent, and may depend on what token(s) lie(s)
   before it in the parsing process.
*/
TOK1_T_EOL = 213,
/** ASCII 32d, but runs of spaces are translated to
    TOK1_T_Blank. */
TOK1_T_Space = 32 /* ' ' */,
/** Generic token for runs of tok1_is_blank() characters. */
TOK1_T_Blank = 132,
TOK1_T_Whitespace = 232,
TOK1_T_UTFBOM = 332 /* UTF byte-order marker (0xEF 0xBB 0xBF) */,

TOK1_T_OpNot = 33 /* ! */,

TOK1_T_OpHash = 35 /* # */,
TOK1_T_Shebang = 135 /* #!... */,

TOK1_T_Dollar = 36 /* $ */,
TOK1_T_CallBlockOpen = 136 /* $( */,
TOK1_T_CallBlock = 236 /* $(...) */,

TOK1_T_OpModulo = 37 /* % */,
TOK1_T_OpModuloAssign = 237 /* %= */,
TOK1_T_OpModuloAssign3 = 337 /* X.Y %= Z*/,

TOK1_T_OpAndBitwise = 38 /* & */,
TOK1_T_OpAnd = 238 /* && */,
TOK1_T_OpAndAssign = 338 /* &= */,
TOK1_T_OpAndAssign3 = 438 /* X.Y &= Z */,

TOK1_T_ParenOpen = 40 /* ( */,
TOK1_T_ParenGroup = 140 /* (...) */,
TOK1_T_ParenClose = 41 /* ) */,

TOK1_T_OpMultiply = 42 /* * */,
TOK1_T_OpMultiplyAssign = 242 /* *= */,
TOK1_T_OpMultiplyAssign3 = 342 /* X.Y*=Z */,

TOK1_T_OpPlus = 43 /* + */,
TOK1_T_OpPlusUnary = 243 /* + */,
TOK1_T_OpPlusAssign = 343 /* += */,
TOK1_T_OpPlusAssign3 = 443 /* X.Y+=Z */,
TOK1_T_OpIncr = 543 /* ++ */,
TOK1_T_OpIncrPre = 643 /* ++ */,
TOK1_T_OpIncrPost = 843 /* ++ */,

TOK1_T_Comma = 44 /* , */,
TOK1_T_RHSEval = 144 /* internal-use-only pseudo-operator */,

TOK1_T_OpMinus = 45 /* - */,
TOK1_T_OpMinusUnary = 245 /* - */,
TOK1_T_OpMinusAssign = 345 /* -= */,
TOK1_T_OpMinusAssign3 = 445 /* X.Y-=y */,
TOK1_T_OpDecr = 545  /* -- */,
TOK1_T_OpDecrPre = 645  /* -- */,
TOK1_T_OpDecrPost = 745  /* -- */,

TOK1_T_OpDot = 46 /* . */,
TOK1_T_OpArrow = 146 /* -> */,
TOK1_T_OpArrow2 = 246 /* => */,
TOK1_T_OpDotDot = 346 /* .. */,
TOK1_T_OpDotLength = 446 /* .# */,

TOK1_T_OpDivide = 47 /* X/Y */,
TOK1_T_OpDivideAssign = 147 /* X/=Y */,
TOK1_T_OpDivideAssign3 = 247 /* X.Y/=Z */,

TOK1_T_Colon = 58 /* : */,
TOK1_T_Colon2 = 258 /* :: */,
TOK1_T_OpColon2 = TOK1_T_Colon2,
TOK1_T_OpColonEqual = 358 /* := */,

TOK1_T_Semicolon = 59 /* ; */,
/** Generic end-of-expression token. */
TOK1_T_EOX = 159,

TOK1_T_OpCmpLT = 60 /* < */,
TOK1_T_OpCmpLE = 260 /* <= */,
TOK1_T_OpShiftLeft = 360 /* << */,
TOK1_T_OpShiftLeftAssign = 460 /* <<= */,
TOK1_T_OpShiftLeftAssign3 = 560 /* X.Y<<=Z */,
TOK1_T_HeredocStart = 660 /* <<< */,
TOK1_T_HeredocStart2 = 760 /* {{{ */,
TOK1_T_Heredoc = 670 /* A "fully slurped" heredoc. Prior to 2020-08-27,
                      heredocs here a special cse of TOK1_T_SquigglyGroup
                      for historical reasons. */,

TOK1_T_OpAssign = 61 /* = */,
TOK1_T_OpAssign3 = 161 /* = */,
TOK1_T_OpCmpEq = 261 /* == */,
TOK1_T_OpCmpNotEq = 361 /* != */,
TOK1_T_OpCmpEqStrict = 461 /* === */,
TOK1_T_OpCmpNotEqStrict = 561 /* !== */,
TOK1_T_OpInherits = 661 /* inherits */,
TOK1_T_OpNotInherits = 761 /* !inherits */,
TOK1_T_OpContains = 861 /* =~ */,
TOK1_T_OpNotContains = 961 /* !~ */,
TOK1_T_OpAssignConst3 = 1061 /* X.Y:=Z */,

TOK1_T_OpCmpGT = 62 /* > */,
TOK1_T_OpCmpGE = 262 /* >= */,
TOK1_T_OpShiftRight = 362 /* >> */,
TOK1_T_OpShiftRightAssign = 462 /* >>= */,
TOK1_T_OpShiftRightAssign3 = 562 /* X.Y>>=Z */,

TOK1_T_Question = 63 /* ? */,
TOK1_T_QDot = 163 /* ?. reserved for potential future use */,

TOK1_T_BraceOpen = 91 /* [ */,
TOK1_T_BraceGroup = 191 /* [...] */,
TOK1_T_Backslash = 92 /* \\ */,
TOK1_T_BraceClose = 93 /* ] */,

TOK1_T_OpXOr = 94 /* ^ */,
TOK1_T_OpXOrAssign = 294 /* ^= */,
TOK1_T_OpXOrAssign3 = 394 /* X.Y^=Z */,

TOK1_T_SquigglyOpen = 123 /* { */,
TOK1_T_SquigglyGroup = 223 /* {...} */,

TOK1_T_OpOrBitwise = 124 /* | */,
TOK1_T_OpOr = 224 /* || */,
TOK1_T_OpOr3 = 324 /* ||| */,
TOK1_T_OpElvis = 424 /* ?: */,
TOK1_T_OpOrAssign = 524 /* |= */,
TOK1_T_OpOrAssign3 = 624 /* X.Y|=Z */,
TOK1_T_SquigglyClose = 125 /* } */,
TOK1_T_OpNegateBitwise = 126 /* ~ */,


TOK1_T_Literal__ = 1000,
/* Parent ttype for the other, more specific TOK1_T_LiteralIntXYZ and
   TOK1_T_LiteralDouble types, which get set as ttype2. */
TOK1_T_LiteralNumber,
TOK1_T_LiteralIntDec,
TOK1_T_LiteralIntHex,
TOK1_T_LiteralIntOct,
TOK1_T_LiteralIntBin,
TOK1_T_LiteralDouble,
/** ttype for one of the more concrete string types, which get that
    type set as their ttyp2. */
TOK1_T_QuotedString,
TOK1_T_QuotedStringDQ,
TOK1_T_QuotedStringSQ,
TOK1_T_QuotedStringBT /* `backtick` string */,
TOK1_T_PropertyKey /* special case of LiteralString */,
TOK1_T_Identifier,
TOK1_T_IdentifierDashed /* identifier-with-dashes */,
TOK1_T_WHCLFlag /* -x, -x-w, etc. */,
TOK1_T_IdentifierDeref /* $[a-zA-Z_][a-zA-Z0-9_]* */,
TOK1_T_PropAccess /* The [Y] part of X[Y] */,
TOK1_T_PropAccessWith /* Y part of .Y when no immediate LHS */,
TOK1_T_WHCLWord /* TCL-like words: almost arbitrary characters */,

TOK1_T_ValueTypes__ = 2000,
TOK1_T_Value,
TOK1_T_Undefined,
TOK1_T_Null,
TOK1_T_False,
TOK1_T_True,
TOK1_T_Object,
TOK1_T_Array,
TOK1_T_Function,

TOK1_T_Keyword__ = 3000,
TOK1_T_KeywordAffirm,
TOK1_T_KeywordAssert,
TOK1_T_KeywordBREAKPOINT,
TOK1_T_KeywordBreak,
TOK1_T_KeywordCOLUMN,
TOK1_T_KeywordCatch,
TOK1_T_KeywordClass,
TOK1_T_KeywordConst,
TOK1_T_KeywordContinue,
TOK1_T_KeywordDefine,
TOK1_T_KeywordDefined,
TOK1_T_KeywordDelete,
TOK1_T_KeywordDo,
TOK1_T_KeywordEcho,
TOK1_T_KeywordEnum,
TOK1_T_KeywordEval,
TOK1_T_KeywordException,
TOK1_T_KeywordExit,
TOK1_T_KeywordFILE,
TOK1_T_KeywordFILEDIR,
TOK1_T_KeywordFalse,
TOK1_T_KeywordFatal,
TOK1_T_KeywordFor,
TOK1_T_KeywordForEach,
TOK1_T_KeywordFunction,
TOK1_T_KeywordIf,
TOK1_T_KeywordImport,
TOK1_T_KeywordInclude,
TOK1_T_KeywordInterface,
TOK1_T_KeywordIs,
TOK1_T_KeywordIsA,
TOK1_T_KeywordLINE,
TOK1_T_KeywordNameof,
TOK1_T_KeywordNew,
TOK1_T_KeywordNull,
TOK1_T_KeywordPragma,
TOK1_T_KeywordPrivate,
TOK1_T_KeywordProc,
TOK1_T_KeywordProtected,
TOK1_T_KeywordPublic,
TOK1_T_KeywordRefcount,
TOK1_T_KeywordReturn,
TOK1_T_KeywordS2Out,
TOK1_T_KeywordSRCPOS,
TOK1_T_KeywordScope,
TOK1_T_KeywordStatic,
TOK1_T_KeywordThrow,
TOK1_T_KeywordTrue,
TOK1_T_KeywordTry,
TOK1_T_KeywordTypeinfo,
TOK1_T_KeywordTypename,
TOK1_T_KeywordUndefined,
TOK1_T_KeywordUnset,
TOK1_T_KeywordUKWD,
TOK1_T_KeywordUsing /* using() function-like keyword, as opposed to
                     function() using(...) {} */,
TOK1_T_KeywordVar,
TOK1_T_KeywordWhile,

/* WHCL-specific keywords (builtin commands) */
TOK1_T_BIC = 3200,
TOK1_T_BIC_affirm,
TOK1_T_BIC_alias,
TOK1_T_BIC_array,
TOK1_T_BIC_assert,
TOK1_T_BIC_break,
TOK1_T_BIC_catch,
TOK1_T_BIC_concat,
TOK1_T_BIC_const,
TOK1_T_BIC_continue,
TOK1_T_BIC___debug,
TOK1_T_BIC_decl,
TOK1_T_BIC_decr,
TOK1_T_BIC_define,
TOK1_T_BIC_do,
TOK1_T_BIC_echo,
TOK1_T_BIC_eval,
TOK1_T_BIC_exception,
TOK1_T_BIC_exit,
TOK1_T_BIC_expr,
TOK1_T_BIC_for,
TOK1_T_BIC_foreach,
TOK1_T_BIC_if,
TOK1_T_BIC_incr,
TOK1_T_BIC_info,
TOK1_T_BIC_new,
TOK1_T_BIC_object,
TOK1_T_BIC_pragma,
TOK1_T_BIC_proc,
TOK1_T_BIC_return,
TOK1_T_BIC_set,
TOK1_T_BIC_throw,
TOK1_T_BIC_unset,
TOK1_T_BIC_while,
TOK1_T_BIC_with,

/* WHCL-specific built-in values */
TOK1_T_BIV = 3900,
TOK1_T_BIV_true,
TOK1_T_BIV_false,
TOK1_T_BIV_null,
TOK1_T_BIV_this,
TOK1_T_BIV_undefined,
TOK1_T_BIV_using,
TOK1_T_BIV_whcl,
TOK1_T_BIV___COLUMN,
TOK1_T_BIV___FILE,
TOK1_T_BIV___FILEDIR,
TOK1_T_BIV___LINE,
TOK1_T_BIV___FLC,

TOK1_T_Comment__ = 4000,
TOK1_T_CommentC,
TOK1_T_CommentCpp,
TOK1_T_CommentTCL,

TOK1_T_Mark__ = 5000,
TOK1_T_MarkVariadicStart,

TOK1_T_Misc__ = 6000,
/**
   A pseudo-token used internally to translate empty [] blocks to a
   PHP-style array-append operation.

   The parser current only allows this op in the context of an assignment
*/
TOK1_T_ArrayAppend,
TOK1_T_Foo,

TOK1_T_comma_kludge_
};

//! Convenience typedef.
typedef enum tok1_en_types_e tok1_en_types_e;

/**
   A "parser token" - tokens used by the whcl tokenization process.
*/
struct tok1_en{
  /**
     A tok1_en_types_e value.
  */
  int16_t ttype;

  /**
     A tok1_en_types_e value.
  */
  int16_t ttype2;
  /**
     1-based line number relative to the tok1_izer which
     sets this via a tok1_next_token_f().

     It turns out that we can't reliably count this unless (slightly
     over-simplified) the tokenizer moves only forward. Once
     downstream code starts manipulating tok1_en::begin and
     tok1_en::end, the counting gets messed up (and we have lots
     of cases which do that).
  */
  tok1_linecol_t line;

  /**
     0-based column number relative to the tok1_izer which
     sets this via a tok1_next_token_f().
  */
  tok1_linecol_t column;
  
  /**
     The starting point of the token, relative to its containing
     script. Invalid tokens have a NULL begin value.

     202008: direct access to this is being phased out in favor of an
     ongoing abstraction. Use tok1_izer_begin() to access it.
  */
  char const * _begin;

  /**
     The one-after-the-end point for the token. When tokenizing
     iteratively, each next token starts at the end position of the
     previous token.

     202008: direct access to this is being phased out in favor of an
     ongoing abstraction. Use tok1_izer_end() to access it.
  */
  char const * _end;

  /**
     Some token types "trim" their bytes to some subset of [begin,
     end). For such token types, the range [adjBegin, adjEnd) should
     be used for fetching their "inner" bytes, while [begin, end) will
     hold the full token bytes. For types with no adjusted start
     point, this should be NULL.

     Currently the types for which this is done include:

     TOK1_T_SquigglyGroup, TOK1_T_Heredoc, TOK1_T_BraceGroup,
     TOK1_T_ParenGroup.

     202008: direct access to this is being phased out in favor of an
     ongoing abstraction. Use tok1_izer_adjbegin() to access it.
  */
  char const * _adjBegin;

  /**
     The one-after-the-end counterpart of adjBegin.

     202008: direct access to this is being phased out in favor of an
     ongoing abstraction. Use tok1_izer_adjend() to access it.
  */
  char const * _adjEnd;
};

/**
   Empty-initialized tok1_en structure, intended for
   const-copy initialization.
*/
#define tok1_en_empty_m {TOK1_T_INVALID,0,0,0,0,0,0,0,}

/**
   Empty-initialized tok1_en structure, intended for
   copy initialization.
*/
extern const tok1_en tok1_en_empty;

/**
   Typedef for a "next token" function. This is part of
   ongoing/perpetual refactoring. In short: this function must use t's
   current state and fetch the "next" token from t, setting t->token's
   state to the position and type of that token. It must return 0
   on success. The state argument is for this callback's own use,
   ignored by the tok1 API.

   The exact requirements for implementations of this function are
   hard-coded into the one existing implementation and will be
   documented here if this code ever escapes the confines of that
   project.
*/
typedef int (*tok1_next_token_f)(tok1_izer * const t, void * state);

/**
   The tok1_izer class is a simple basis for a tokenizer, largely
   syntax- and language-independent. Its origins go back many years
   and several projects.

   This tokenizer requires that all input be available in advance of
   tokenization and remain valid for its own lifetime.

   @see tok1_izer_init()
   @see tok1_izer_putback()
   @see tok1_izer_next_token_set()
*/
struct tok1_izer {
  /**
     Used for memory management and error reporting.
  */
  cwal_engine * ec;

  /**
     The underlying tokenizer function for this object.
  */
  tok1_next_token_f nextTokenF;

  /**
     Starting position of input.

     The full input range is [begin, end).
  */
  char const * begin;

  /**
     One-past-the-end position of the input (i.e. the position where
     the NUL byte normally is). We often use sub-tokenizers for
     tokenizing block-level constructs within a larger tokenizer, and
     this very often points to a non-NUL byte.
  */
  char const * end;

  /**
     Error string (static memory) from tokenization
     errors. Set by a tok1_next_token_f().
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
  tok1_izer const * parent;

  /**
     Used for capturing line/column offset info for "distant child"
     tokenizers, which "know" they derive from another but have no
     access to it (it may be long gone).
  */
  tok1_linecol_t lineOffset;

  /**
     Column counterpart of lineOffset.
  */
  tok1_linecol_t colOffset;

  /**
     1-based current tokenization line number.

     This is maintained by a tok1_next_token_f(), updated
     as it crosses newline boundaries.

     This can only be tracked properly as long as a
     tok1_next_token_f() is called linearly. Once clients starts
     jumping around the input, this counting breaks.
  */
  tok1_linecol_t currentLine;
  /**
     0-based current tokenization column number.

     This is maintained by a tok1_next_token_f(). See notes in
     this->currentLine.
  */
  tok1_linecol_t currentCol;

  /**
     Flags which may change how this object tokenizes.
  */
  cwal_flags32_t flags;
  
  /**
     The current token. Its state is set up thusly:

     Initially, token.begin must be this->begin and token.end
     must be 0. That state is used by a tok1_next_token_f() to
     recognize the initial token state and DTRT.

     During tokenization, this object's state is updated to reflect
     the range from [this->begin, this->end) matching a token (or
     an error position, in the case of a tokenization error).
  */
  tok1_en token;

  /**
     The put-back token. tok1_next_token_f() must copy this->token to
     this object before attempting any tokenization.
     tok1_izer_putback() copies _pbToken over this->token and clears
     _pbToken.

     Do not manipulate this directly. use tok1_izer_putback(),
     tok1_izer_putback_get(), and (if really needed)
     tok1_izer_putback_set().
  */
  tok1_en _pbToken;

  /**
     An experiment in cutting down on tokenization. Token lookahead
     ops, and some client code, set this after they have done a
     lookahead, conditionally setting this to that looked-ahead
     token. tok1_izer_next_token() will, if this is set, use this
     token and skip the tokenization phase. This member is cleared by
     tok1_izer_token_set() and tok1_next_token_f().

     Do not manipulate this directly: use tok1_izer_next_token_set()
     to modify it.
  */
  tok1_en _nextToken;

  /**
     Used for marking an error position, which is part of the line/col
     counting mechanism used for error reporting.

     Do not manipulate this directly. Use tok1_izer_errtoken_get(),
     tok1_izer_errtoken_set(), and tok1_izer_errtoken_has().
  */
  tok1_en _errToken;
};
/** Empty-initialized tok1_izer object. */
#define tok1_izer_empty_m {                     \
    0/*ec*/, NULL/*nextTokenF*/,                \
    0/*begin*/,0/*end*/,                    \
    0/*errMsg*/,                              \
    0/*name*/,                                \
    0/*parent*/,                              \
    0/*lineOffset*/,0/*colOffset*/,               \
    1/*currentLine*/,0/*currentCol*/,0/*flags*/,  \
    tok1_en_empty_m/*token*/,               \
    tok1_en_empty_m/*_pbToken*/,             \
    tok1_en_empty_m/*_nextToken*/,             \
    tok1_en_empty_m/*_errToken*/              \
  }
/** Empty-initialized tok1_izer object. */
extern const tok1_izer tok1_izer_empty;

/**
   Flags for use with tok1_izer::flags and possibly
   related contexts.
*/
enum tok1_flags_e {
/**
   Sentinel value. Must be 0.
*/
TOK1_F_NONE = 0,
/**
   If set, the '-' character is considered a legal identifier by
   tok1_read_identifier2() except when the '-' appears at the start of
   the input.
*/
TOK1_F_IDENTIFIER_DASHES = 0x01,
/**
   Like TOK1_F_IDENTIFIER_DASHES, but a leading dash is also
   considered a valid identifier char.
*/
TOK1_F_IDENTIFIER_DASHES2 = 0x03,

/**
   An internal-only flag which signifies to error-handling routines
   that the given tok1_izer is only ever used in a strictly linear
   fashion, allowing such routines to take advantage of line/column
   state of the tokenizer and avoid counting lines for error
   reporting. Note that the overwhelming majority of tok1_izers in s2
   are NOT purely linear and it's normally impossible to know in
   advance whether one will be or not. In such cases it will use the
   position of either its error token or current token, in that order.

   Reminder to self: this was added to support whcl_script
   experimentation, as whcl_script internally uses tok1_izer and
   "compilation" but tokenizes all the input before "compiling" it.
   This flag allows errors in that step to be reported without an
   extra line-counting step.
*/
TOK1_F_LINEAR_TOKER = 0x10
};

/**
   Must be passed a tok1_izer and its input source. If len is
   negative then the equivalent of strlen() is used to calculate its
   length.

   Calling this obligates the caller to eventually pass t to
   tok1_izer_finalize(), regardless of whether this routine succeeds
   or not, to free up any resources this routine may have allocated.

   Returns 0 on success, CWAL_RC_MISUSE if !t or !src.

   Results are undefined if any pointer is NULL or invalid.
*/
void tok1_izer_init( cwal_engine * const e, tok1_izer * const t,
                      char const * src, cwal_int_t len,
                      uint32_t flags );

/**
   Resets the tokenization state so that the next call to
   tok1_izer_next_token() will start at the beginning of the
   input. This clears the putback and error token state, and between
   this call and the next call to tok1_izer_next_token() (or similar),
   the current token state is invalid.
*/
void tok1_izer_reset( tok1_izer * const t );

/**
   Clears t's state and frees any memory is may be using. This does
   not free t, but will free any memory allocated on its behalf.

   Note that if any sub-tokenizers which derive from t (i.e. have t
   set as their tok1_izer::parent value), they MUST be cleaned up
   first, as they may refer to memory owned by a parent.

   It is safe to pass this function an instance which was not passed
   to tok1_izer_init() or tok1_izer_init_v2() if (and only if) the
   instance was copy-initialized from tok1_izer_empty or
   tok1_izer_empty_m.
*/
void tok1_izer_finalize( tok1_izer * const t );

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
   pass t to tok1_izer_finalize(). If it fails, t will be finalized if
   needed. If this routine fails, passing t to tok1_izer_finalize() is
   a harmless no-op if t was initially copy-initialized from
   tok1_izer_empty or tok1_izer_empty_m (i.e. it's in a well-defined
   state).
*/
int tok1_izer_sub_from_toker( tok1_izer const * parent,
                               tok1_izer * const dest);

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

   If this function succeeds, the caller is obligated to eventually
   pass t to tok1_izer_finalize(). If it fails, t will be finalized if
   needed. If this routine fails, passing t to tok1_izer_finalize() is
   a harmless no-op if t was initially copy-initialized from
   tok1_izer_empty or tok1_izer_empty_m (i.e. it's in a well-defined
   state).
*/
int tok1_izer_sub_from_token( tok1_izer const * parent, tok1_en const * token,
                              tok1_izer * const sub );

/**
   Returns the top-most object from t->parent, or t if !t->parent.
*/
tok1_izer const * tok1_izer_top_parent( tok1_izer const * t );

/**
   Returns either t->name or the first name encountered while climbing
   up the t->parent chain. Returns 0 if no name is found. If len is
   not 0 then if this function returns non-0, len is set to that
   name's length.
*/
char const * tok1_izer_name_first( tok1_izer const * t, cwal_size_t * len );
/**
   Returns the top-most name from t and its parent chain.
   Returns 0 if no name is found.

   FIXME: add (cwal_size_t * len) parameter.
*/
char const * tok1_izer_name_top( tok1_izer const * t );

/**
   Sets e's error state, as for cwal_error_set(), and returns that
   call's result value. If fmt is NULL or !*fm, st->errMsg is used (if
   set) as the message. Sometimes it is necessary for callers to set
   or tweak st error position explicitly before calling this (see
   tok1_izer_errtoken_set()).

   If e is NULL, st->e is used. One of them _must_ be non-NULL.

   Returns the error code, not 0, on success! Returns some other
   non-0 code on error.

   If the given error code is CWAL_RC_OOM, this routine takes pains
   not to allocate any memory, which also means not generating an
   error message.
*/
int tok1_err_toker( tok1_izer const * st,
                    int code, char const * fmt, ... );

/**
   Works exactly like tok1_err_toker() except that it triggers
   an exception.
*/
int tok1_throw_toker( tok1_izer const * st,
                      int code, char const * fmt, ... );


/**
   Returns the first explicit non-0 error position marker from t or
   its nearest ancestor (via t->parent). If the 2nd argument is not
   NULL, *foundIn is assigned to the tokenizer in which the error
   position was found.

   @see tok1_izer_err_pos()
*/
char const * tok1_izer_err_pos_first( tok1_izer const * t,
                                      tok1_izer const ** foundIn);

/**
   Tries to return an "error position" for the given tokenizer, under
   the assumption that something has just "gone wrong" and the client
   wants to know where (or whereabouts) it went wrong. It first tries
   tok1_izer_err_pos_first(). If that returns NULL, it returns either
   the position of pt's current token or putback token. If those are
   NULL, pt->begin is returned (indicating that tokenization has not
   yet started or failed on the first token).

   @see tok1_izer_err_pos_first()
*/
char const * tok1_izer_err_pos( tok1_izer const * pt );

/**
   Sets pt's next token to be a bitwise copy of tk, such that the next
   call to tok1_izer_next_token() will evaluate to that token. It is
   up to the caller to ensure that tk is valid for pt (e.g. by having
   read tk via a call to tok1_izer_lookahead()).

   This is an optimization useful when one has looked-ahead in pt and
   determined that the looked-ahead token should be the next-consumed
   token. Using tok1_izer_putback() would also be an option, but this
   approach is more efficient because it eliminates the
   re-tokenization of the put-back token.

   Note that this does not modify the putback token. If
   tok1_izer_next_token() is called immediately after this, the
   putback token will be set to whichever token was most recently
   consumed before *this* routine was called (i.e. exactly the same as
   would happen without this routine having been called).

   @see tok1_izer_next_token()
*/
void tok1_izer_next_token_set( tok1_izer * const pt, tok1_en const * const tk );

/**
   For a given tok1_en_types values, this returns a unique
   string representation of its type ID. The returned bytes
   are static. Returns 0 for an unknown value.

   This function serves two main purposes:

   1) So that we can let gcc warn us if any values in tok1_en_types
   collide with one another. (Implementing it caught two collisions.)

   2) Help with debugging. The strings returned from this function are
   intended for "informational" or "entertainment" value only, not
   (de)serialization.
*/
char const * tok1_t_cstr( int ttype );

/**
   If st has no putback token, 0 (false) is returned and this function
   has no side-effects, otherwise st->token is replaced by the putback
   token, the putback token is cleared, and non-0 (true) is returned.

   Note that tok1_izer_next_token_set() is often a more efficient
   option, provided semantics allow for it (which they don't always
   do). After calling this routine, tok1_izer_next_token() must
   re-tokenize the next token, whereas tok1_izer_next_token_set()
   allows tok1_izer_next_token() to bypass that tokenization step.
*/
bool tok1_izer_putback( tok1_izer * const st );

/**
   Sets a bitwise copy of tok as the current putback token. It is up
   to the caller to ensure that tok is valid for the given tokenizer's
   current state.
*/
void tok1_izer_putback_set( tok1_izer * const st, tok1_en const * const tok );

/**
   Returns st's current putback token, which should be bitwise
   copied by the caller because its state may change on any calls
   to the tok1_izer API.
*/
tok1_en const * tok1_izer_putback_get( tok1_izer const * const st );

/**
   Returns st's current error token. It never returns NULL - the
   memory is owned by st and may be modified by any future calls into
   its API. The token will have a ttype of TOK1_T_INVALID, and no
   begin/end values, if there is no error.
*/
tok1_en const * tok1_izer_errtoken_get( tok1_izer const * st );

/**
   Bitwise copies tok to be st's current error token. If tok is NULL,
   the error token is cleared, else it is up to the caller to ensure
   that the token is valid for st's current state.

   Note that setting an error token does not trigger an error - it is
   intended only to mark the part of the tokenizer which might be
   associated with an error, for downstream error reporting of that
   position.
*/
void tok1_izer_errtoken_set( tok1_izer * const st, tok1_en const * const tok );

/**
   Returns true if st has an error token set which points to somewhere
   in st's range, else returns false.
*/
bool tok1_izer_errtoken_has( tok1_izer const * st );

/**
   Sets st->_pbToken to st->token, st->token to *t, and clears
   st->_nextToken.
*/
void tok1_izer_token_set( tok1_izer * const st, tok1_en const * const t );

/**
   Returns st->token.ttype if st's current token represents an EOF.
*/
int tok1_izer_is_eof( tok1_izer const * const st );

/**
   Returns st->token.ttype if st's current token represents an
   end-of-expression.
*/
int tok1_izer_is_eox( tok1_izer const * const st );

/**
   Returns ttype if ttype is an end-of-line token.
*/
int tok1_t_is_eol( int ttype );

/**
   Returns ttype if ttype represents a "space" token
   (in any of its various incarnations).
*/
int tok1_t_is_space( int ttype );

/**
   Returns ttype if the given token type is considered a "junk" token
   (with no syntactical meaning).

   Junk includes the following token types:

   ASCII 32d (SPACE), ASCII 13d (CR), ASCII 9d (TAB), TOK1_T_Blank,
   TOK1_T_CommentC, TOK1_T_CommentCpp, TOK1_T_CommentTCL,
   TOK1_T_Whitespace, TOK1_T_Shebang, TOK1_T_UTFBOM

   Note that TOK1_T_NL/TOK1_T_EOL (newline/EOL) is not considered junk
   here, as it is an expression separator in some contexts and
   skippable in others.
*/
int tok1_t_is_junk( int ttype );

/**
   Returns ttype if ttype represents a symbol that unambiguously marks
   the end of an expression:

   TOK1_T_EOF, TOK1_T_EOX, TOK1_T_Semicolon
*/
int tok1_t_is_eox( int ttype );

/**
   Returns ttype if ttype represents an EOF (or virtual EOF) token.
*/
int tok1_t_is_eof( int ttype );

/**
   Returns ttype if ttype presents a "group" type:

   TOK1_T_ParenGroup, TOK1_T_BraceGroup, TOK1_T_SquigglyGroup
*/
int tok1_t_is_group( int ttype );


/**
   Counts the line/column position of `pos` within the half-open range
   [`src`,`end`). Starts counting at line 1, column 0, and updates the
   final two arguments with the count. Returns 0 on success and the
   only error condition is that the given position is out of the given
   bounds, in which case it returns CWAL_RC_RANGE.
*/
int tok1_count_lines( char const * src, char const * end,
                      char const * pos,
                      tok1_linecol_t *line, tok1_linecol_t *col );

/**
   Incomplete/untested. Don't use.

   tok must be a token from pt.

   This variant tries to use line/column info embedded in tok before
   falling back to counting "the hard way". Note that tok->line and
   tok->col are relative to pt->begin, and pt (or one of its parents)
   may have line/column offset info to apply to the results, making
   the line/col relative to the top-most tok1_izer in the hierarchy.

   This approach to line-counting _only_ works when pt is used
   in a strictly linear fashion.
*/
int tok1_izer_count_lines2( tok1_izer const * pt,
                             tok1_en const * tok,
                             tok1_linecol_t * line, tok1_linecol_t * col );


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
   CWAL_RC_RANGE if (`end<begin`) or \\uXXXX and \\UXXXXXXXX are not
   given 4 resp. 8 hex digits or if those digits resolve to a
   non-UTF-8 character.

   [1] = note that this routine does not know which quotes (if any)
   wrap up the input string, so it cannot know that only single- _or_
   double-quotes need unescaping. So it unescapes both.
*/
int tok1_unescape_string( cwal_engine * const e,
                          char const * begin,
                          char const * end,
                          cwal_buffer * const dest );

/**
   Equivalent to calling tok1_read_identifier2() with a final argument
   of 0.

   @see tok1_read_identifier2()
*/
int tok1_read_identifier( char const * zPos, char const * zEnd,
                        char const ** zIdEnd );

/**
   Assumes that zPos is the start of an identifier and reads until the
   next non-identifier character. zEnd must be the logical EOF for
   zPos. On returning, *zIdEnd is set to the one-after-the-end
   position of the read identifier (which will be (*zIdEnd-pos) bytes
   long).

   Expects the input to be valid ASCII/UTF-8, else results are
   undefined.

   This routine treats ANY UTF-8 character outside the ASCII range as
   an identifier character.

   Returns the number of identifier characters read.

   This funciton honors the TOK1_F_IDENTIFIER_DASHES or
   TOK1_F_IDENTIFIER_DASHES2 flags if they are set in the final
   argument.

   @see tok1_read_identifier()
*/
int tok1_read_identifier2( char const * zPos, char const * zEnd,
                         char const ** zIdEnd,
                         uint32_t flags );

/**
   Returns true if ch is one of:
       
   ` ` (ASCII 32d), `\t` (ASCII 9d)
*/
bool tok1_is_blank( int ch );

/**
   Returns true if tok1_is_blank(ch) is true of if ch is one of:

   `\n` (ASCII 10d), `\r` (13d), `\v` (11d), `\f` (12d)
*/
bool tok1_is_space( int ch );

/**
   Returns true if ch is-a digit character (0..9).
*/
bool tok1_is_digit( int ch );

/**
   Returns non-0 if ch is-a hexidecimal digit character (0..9,
   a..F, A..F), else returns 0.
*/
bool tok1_is_xdigit( int ch );

/**
   Returns non-0 if character ch is an octal digit character (0..7),
   else returns 0.
*/
bool tok1_is_octaldigit( int ch );

/**
   Returns non-0 if ch is an ASCII alphabetic character (a..z,
   A..Z), else returns 0.
*/
bool tok1_is_alpha( int ch );

/**
   Returns non-0 if tok1_is_alpha(ch) or tok1_is_digit(ch), else returns
   0.
*/
bool tok1_is_alnum( int ch );

/**
   If token->begin is not 0 and less than token->end, then (token->end
   - token->begin) is returned, cast to cwal_size_t, else 0 is
   returned (which is not a valid token length except for an EOF
   token).
*/
#define tok1_en_len(TOK) \
  ((cwal_size_t)((tok1_en_begin(TOK)            \
    && tok1_en_end(TOK) > tok1_en_begin(TOK)) \
  ? (cwal_size_t)(tok1_en_end(TOK) - tok1_en_begin(TOK)) \
                 : 0))
/*cwal_size_t tok1_en_len( tok1_en const * token );*/

/**
   If the given token has an "adjusted" begin/end range, this function
   returns the length of that range as a cwal_size_t, else it behaves
   identically to tok1_en_len().

   Typically only group-level tokens, quoted strings, and heredocs
   have an adjusted range.
*/
#define tok1_en_len2(TOK)                         \
  ((cwal_size_t)(((tok1_en_adjbegin(TOK) &&       \
    tok1_en_adjbegin(TOK)<=tok1_en_adjend(TOK)) \
    ? (cwal_size_t)(tok1_en_adjend(TOK) - tok1_en_adjbegin(TOK)) \
                  : tok1_en_len(TOK))))
/*cwal_size_t tok1_en_len2( tok1_en const * token );*/

/**
   Expects a filename-like string in the first slen bytes of str, with
   directory components separated by either '/' or '\\' (it uses the
   first of those it finds, in that order, as the separator). If any
   separator is found, a pointer to the last instance of it in str is
   returned, otherwise 0 is returned.
*/
char const * tok1_last_path_sep(char const * str, cwal_size_t slen );

/**
   Returns a pointer to the token's contents, setting its length to
   *len if len is not NULL. tok must not be NULL. Note that the
   returned string is almost certainly not NUL-terminated (or
   terminates at the very end of the tok1_izer from which tok
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
char const * tok1_en_cstr( tok1_en const * const tok,
                              cwal_size_t * const len,
                              bool innerOnly);

/**
   A helper type for tokenizing conventional PATH-style strings.
   Initialize them with tok1_path_toker_init() and iterate over them
   with tok1_path_toker_next().
*/
struct tok1_path_toker {
  /** Begining of the input range. */
  char const * begin;
  /** One-after-the-end of the input range. */
  char const * end;
  /** Position for the next token lookup. */
  char const * pos;
  /** List of token separator characters (ASCII only). */
  char const * separators;
};
typedef struct tok1_path_toker tok1_path_toker;
/**
   Default-initialized tok1_path_toker instance, intended for const-copy
   initialization. On Windows builds its separators member is set to
   ";" and on other platforms it's set to ":;".
*/
#if defined(TOK1_OS_WINDOWS)
#  define tok1_path_toker_empty_m {NULL,NULL,NULL,";"}
#else
#  define tok1_path_toker_empty_m {NULL,NULL,NULL,":;"}
#endif

/**
   Default-initialized tok1_path_toker instance, intended for
   copy initialization.

   @see tok1_path_toker_empty_m
*/
extern const tok1_path_toker tok1_path_toker_empty;

/**
   Wipes out pt's current state by copying tok1_path_toker_empty over it
   and initializes pt to use the given path as its input. If len is 0
   or more then it must be the length of the string, in bytes. If len
   is less than 0, cwal_strlen() is used to determine the path's
   length.  (When dealing with inputs which are not NUL-terminated,
   it's critical that the user pass the correct non-negative length.)

   If the client wants to modify pt->separators, it must be done so
   *after* calling this.

   Use tok1_path_toker_next() to iterate over the path entries.
*/
void tok1_path_toker_init( tok1_path_toker * const pt,
                           char const * path, cwal_int_t len );

/**
   Given a tok1_path_toker which was formerly initialized using
   tok1_path_toker_init(), this iterates over the next-available path
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
   tok1_path_toker pt = tok1_path_toker_empty;
   tok1_path_toker_init(&pt, path, pathLen);
   while(0==tok1_path_toker_next(&pt, &t, &tLen)){
      // The next element is the tLen bytes of memory starting at t:
      printf("Path element: %.*s\n", (int)tLen, t);
   }
   @endcode
*/
int tok1_path_toker_next( tok1_path_toker * const pt, char const ** token,
                          cwal_size_t * const len );

/**
    Post-processor for TOK1_T_HeredocStart and TOK1_T_HeredocStart2
    tokens. e is the interpreter engine (used for error reporting), st
    is the current tokenizer state. tgt is where the results are
    written. If tgt is NULL, st->token is used.

    Requires that st->token be a TOK1_T_HeredocStart or
    TOK1_T_HeredocStart2 token. This function scans st for the opening
    and closing heredoc tokens. Before scanning, it uses tf to check
    if the next after the start of the heredoc is a ':', and changes
    space-trimming in tht case (see below). tfState is passed on as
    the second argument to tf but is otherwise ignored by this
    function. The tf callback needs to support only the following
    tokens types: TOK1_T_Colon2, TOK1_T_QuotedString,
    TOK1_T_Identifier (just enough to scan the first 1-2 tokens after
    the heredoc opener).

    On error a non-0 CWAL_RC_xxx value is returned and e's error state
    will be updated with a description of the problem. On error
    st->token is in an unspecified state.

    On success tgt->ttype is set to TOK1_T_Heredoc and
    [out->begin, out->end) will point to the whole token proper,
    including its leading and trailing opener/closer tokens. That
    range will have a length of at least 2 (one each for the opening
    and closing token). [out->adjBegin, out->adjEnd) will point to
    the range encompassing the body of the open/close block, stripped
    of any leading or trailing spaces, as specified below. That range
    may be empty.

    Returns 0 on success and sets e's error state on error. 

    Syntax rules:

    Assuming that the TOK1_T_HeredocStart token is '<<<', heredocs
    can be constructed as follows:

    ```
    <<<EOF blah blah blah EOF
    <<<EOF blah blah blahEOF // equivalent!
    <<<'EOF' ... 'EOF'
    <<<"EOF" ... "EOF"
    <<<:EOF ... EOF // colon changes space-skipping rules
    ```

    Alternately, the TOK1_T_HeredocStart2 syntax is:

    ```
    {{{ blah blah blah }}}
    {{{: blah blah blah }}} // colon works the same as above
    ```

    That form generally plays better with existing syntax highlighting
    and auto-indention modes.

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
int tok1_slurp_heredoc( tok1_izer * const st,
                        tok1_next_token_f tf,
                        void * tfState,
                        tok1_en * tgt );

/**
   This function does not exist. It used to but was removed in
   refactoring related to the whcl project. Many docs still refer to
   it, though.  The role of this function has been taken over by the
   tok1_next_token_f() callback.
*/
void tok1_izer_next_token(void);

/**
  The following macros are part of an ongoing porting/abstraction
  effort to eliminate direct access to token members.
*/
#define tok1_en_begin(P) ((P)->_begin)
#define tok1_en_begin2(P) ((P)->_adjBegin ? (P)->_adjBegin : (P)->_begin)
#define tok1_en_begin_set(P,X) (P)->_begin = (X)
#define tok1_en_end(P) ((P)->_end)
#define tok1_en_end2(P) ((P)->_adjEnd ? (P)->_adjEnd : (P)->_end)
#define tok1_en_end_set(P,X) (P)->_end = (X)
#define tok1_en_adjbegin(P) ((P)->_adjBegin)
#define tok1_en_adjbegin_set(P,X) (P)->_adjBegin = (X)
#define tok1_en_adjbegin_incr(P,X) (P)->_adjBegin += (X)
#define tok1_en_adjend(P) ((P)->_adjEnd)
#define tok1_en_adjend_set(P,X) (P)->_adjEnd = (X)
#define tok1_en_adjend_incr(P,X) (P)->_adjEnd += (X)

#define tok1_izer_begin(PT) ((PT)->begin)
#define tok1_izer_end(PT) ((PT)->end)
#define tok1_izer_token(PT) (PT)->token
#define tok1_izer_len(PT) ((PT)->end - (PT)->begin)

#ifdef __cplusplus
}/*extern "C"*/
#endif
#endif
/* include guard */
