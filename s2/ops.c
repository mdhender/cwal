/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "s2_internal.h"

#if 1
#include <stdio.h>
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__); if(1) printf pfexp
#else
#define MARKER(pfexp) (void)0
#endif

#define s2__pop_val s2_engine_pop_value
#define s2__pop_tok s2_engine_pop_token

/**
   Operator for unary and binary addition and subtraction.

   Required stack (from top to bottom):
   Binary: RHS LHS
   Unary: RHS
*/
static int s2_op_f_addsub( s2_op const * self, s2_engine * se,
                              int argc, cwal_value **rv );

/**
   Operator for multiplication, divisision, and modulus.

   Required stack (from top to bottom): RHS LHS
*/
static int s2_op_f_multdivmod( s2_op const * self, s2_engine * se,
                                  int argc, cwal_value **rv );

/**
   Operator for bitwise OR, AND, XOR, ~, and bitshift << and >>.

   Required stack (from top to bottom): RHS LHS
   except for the unary ~ op, which expects 1 operand.
*/
static int s2_op_f_bitwiseshift( s2_op const * self, s2_engine * se,
                               int argc, cwal_value **rv );

static int s2_op_f_not( s2_op const * self, s2_engine * se,
                           int argc, cwal_value **rv );

static int s2_op_f_andor( s2_op const * self, s2_engine * se,
                             int argc, cwal_value **rv );

/**
   Operator for comparison ops.

   Required stack (from top to bottom): RHS LHS
*/
static int s2_op_f_cmp( s2_op const * self, s2_engine * se,
                           int argc, cwal_value **rv );

static int s2_op_f_parens( s2_op const * self, s2_engine * se,
                              int argc, cwal_value **rv );

/**
   Operator for comma. It simply discards its lhs operand
   and evaluates to its rhs.

   Required stack (from top to bottom): RHS LHS
*/
static int s2_op_f_comma( s2_op const * self, s2_engine * se,
                             int argc, cwal_value **rv );


/**
   Operator for the dot and hash operators.

   Required stack (top to bottom): LHS
*/
static int s2_op_f_dot( s2_op const * self, s2_engine * se,
                           int argc, cwal_value **rv );

/**
   Operator for the dot-length operator (X.#).

   Required stack (top to bottom): LHS
*/
static int s2_op_f_dot_length( s2_op const * self, s2_engine * se,
                               int argc, cwal_value **rv );

/**
   Op impl for the arrow operator (X->Y). Fail if used on any value
   which does not overload it.
*/
static int s2_op_f_oload_arrow( s2_op const * self, s2_engine * se,
                                 int argc, cwal_value **rv );

/**
   Impl for the X=Y family of operators.

   Required stack (top to bottom): RHS, KEY
*/
static int s2_op_f_assign2( s2_op const * self, s2_engine * se,
                            int argc, cwal_value **rv );

/**
   Impl for the X.Y=Z family of operators.

   Required stack (top to bottom): RHS, KEY, OBJECT
*/
static int s2_op_f_assign3( s2_op const * op, s2_engine * se,
                            int argc, cwal_value **rv );


static int s2_op_f_incrdecr( s2_op const * self, s2_engine * se,
                              int argc, cwal_value **rv );

static int s2_op_f_array_append( s2_op const * self, s2_engine * se,
                                 int argc, cwal_value **rv );

/**
   A dummy op for experimentation.

   Required stack (from top to bottom):

   ... S2_T_MarkVariadicStart
*/
static int s2_op_f_foo( s2_op const * self, s2_engine * se,
                           int argc, cwal_value **rv );


/**
   Where the global list of operators is stored. When adding new
   membmers, MAKE 100% SURE that:

   a) the entries in the following array line up with the members.

   b) update s2_ttype_op() to return the new member for the appropriate
   token type(s).
 */
static const struct S2_OPS__ {
  const s2_op add1;
  const s2_op add2;
  const s2_op sub1;
  const s2_op sub2;
  const s2_op multiply;
  const s2_op divide;
  const s2_op modulo;

  const s2_op bitOr;
  const s2_op bitXOr;
  const s2_op bitAnd;
  const s2_op bitNegate;
  const s2_op bitShiftLeft;
  const s2_op bitShiftRight;

  const s2_op logNot;
  const s2_op logAnd;
  const s2_op logOr;
  const s2_op logOr3;
  const s2_op elvis;
  /*
    logical && and || are partially handled at the tokenizer level to
    support short-circuiting.
  */

  const s2_op cmpLT;
  const s2_op cmpLE;
  const s2_op cmpEq;
  const s2_op cmpEqStrict;
  const s2_op cmpNotEq;
  const s2_op cmpNotEqStrict;
  const s2_op cmpGT;
  const s2_op cmpGE;
  const s2_op inherits;
  const s2_op notInherits;
  const s2_op contains;
  const s2_op notContains;

  const s2_op assign;
  const s2_op assignConst;
  const s2_op assignMember;
  const s2_op assignMemberConst;
  const s2_op assignPlus;
  const s2_op assignPlus3;
  const s2_op assignMinus;
  const s2_op assignMinus3;
  const s2_op assignMult;
  const s2_op assignMult3;
  const s2_op assignDiv;
  const s2_op assignDiv3;
  const s2_op assignModulo;
  const s2_op assignModulo3;
  const s2_op assignShiftLeft;
  const s2_op assignShiftLeft3;
  const s2_op assignShiftRight;
  const s2_op assignShiftRight3;
  const s2_op assignXOr;
  const s2_op assignXOr3;
  const s2_op assignAnd;
  const s2_op assignAnd3;
  const s2_op assignOr;
  const s2_op assignOr3;

  const s2_op incrPre;
  const s2_op incrPost;
  const s2_op decrPre;
  const s2_op decrPost;

  const s2_op parenOpen;
  const s2_op parenClose;

  const s2_op comma;
  const s2_op dot;
  const s2_op dotDot;
  const s2_op dotLength;
  const s2_op dotHash;
  const s2_op arrow;
  const s2_op arrow2;
  const s2_op colon2;
  const s2_op arrayAppend;

  /**
     We need a ternary s2_op to support the ternary operator, but it
     is not used by the stack machine itself.
  */
  const s2_op ternary;

  const s2_op rhsEval;

  const s2_op foo;
  const s2_op _sentry_;
} S2_OPS = {
/*{ sym,     id,                arity, assoc, prec,      placement, call(), inferLeft, inferRight, derivedFromOp } */
  { "+X",    S2_T_OpPlusUnary,  1,  1, S2_PR_PlusUnary,  -1,  s2_op_f_addsub, 0, 0, 0 },
  { "X+Y",   S2_T_OpPlus,       2, -1, S2_PR_Plus,        0,  s2_op_f_addsub, 0, 0, 0 },

  { "-X",    S2_T_OpMinusUnary, 1,  1, S2_PR_MinusUnary, -1,  s2_op_f_addsub, 0, 0, 0 },
  { "X-Y",   S2_T_OpMinus,      2, -1, S2_PR_Minus,       0,  s2_op_f_addsub, 0, 0, 0 },

  { "X*Y",   S2_T_OpMultiply,   2, -1, S2_PR_Multiply,    0,  s2_op_f_multdivmod, 0, 0, 0 },
  { "X/Y",   S2_T_OpDivide,     2, -1, S2_PR_Divide,      0,  s2_op_f_multdivmod, 0, 0, 0 },
  { "X%Y",   S2_T_OpModulo,     2, -1, S2_PR_Modulo,      0,  s2_op_f_multdivmod, 0, 0, 0 },

  { "X|Y",   S2_T_OpOrBitwise,  2, -1, S2_PR_BitwiseOr,   0,  s2_op_f_bitwiseshift, 0, 0, 0 },
  { "X^Y",   S2_T_OpXOr,        2, -1, S2_PR_BitwiseXor,  0,  s2_op_f_bitwiseshift, 0, 0, 0 },
  { "X&Y",   S2_T_OpAndBitwise, 2, -1, S2_PR_BitwiseAnd,  0,  s2_op_f_bitwiseshift, 0, 0, 0 },
  { "~X",    S2_T_OpNegateBitwise, 1, 1, S2_PR_BitwiseNegate, -1, s2_op_f_bitwiseshift, 0, 0, 0 },
  { "X<<Y",  S2_T_OpShiftLeft,  2, -1, S2_PR_ShiftLeft,   0, s2_op_f_bitwiseshift, 0, 0, 0 },
  { "X>>Y",  S2_T_OpShiftRight, 2, -1, S2_PR_ShiftRight,  0, s2_op_f_bitwiseshift, 0, 0, 0 },

  { "!X",    S2_T_OpNot,        1,  1, S2_PR_LogicalNot, -1, s2_op_f_not, 0, 0, 0 },
  /* &&, ||, and ||| are handled at the parser level for
     short-cicuiting, but the stack machine supports them directly
     without short-circuiting. Why?  It's a stack machine, so the RHS
     has already been processed (or the stack wouldn't be running), so
     short-circuiting evaluation is not possible at that
     point.
  */
  { "X&&Y",  S2_T_OpAnd,          2, -1, S2_PR_LogicalAnd,  0, s2_op_f_andor, 0, 0, 0 },
  { "X||Y",  S2_T_OpOr,           2, -1, S2_PR_LogicalOr,   0, s2_op_f_andor, 0, 0, 0 },
  { "X|||Y", S2_T_OpOr3,          2, -1, S2_PR_LogicalOr3,  0, s2_op_f_andor, 0, 0, 0 },
  { "X?:Y",  S2_T_OpElvis,        2, -1, S2_PR_LogicalOrElvis, 0, s2_op_f_andor, 0, 0, 0 },

  { "X<Y",   S2_T_CmpLT,          2, -1, S2_PR_CmpLT,       0, s2_op_f_cmp, 0, 0, 0 },
  { "X<=Y",  S2_T_CmpLE,          2, -1, S2_PR_CmpLE,       0, s2_op_f_cmp,
    S2_T_CmpLT, S2_T_CmpEq, S2_T_CmpGT /* i.e. X<=Y ==> !(X>Y) */ },
  { "X==Y",  S2_T_CmpEq,          2, -1, S2_PR_CmpEq,       0, s2_op_f_cmp, 0, 0, 0 },
  { "X===Y", S2_T_CmpEqStrict,    2, -1, S2_PR_CmpEqStrict, 0, s2_op_f_cmp, 0, 0, 0 },
  { "X!=Y",  S2_T_CmpNotEq,       2, -1, S2_PR_CmpNotEq,    0, s2_op_f_cmp, 0, 0,
    S2_T_CmpEq },
  { "X!==Y", S2_T_CmpNotEqStrict, 2, -1, S2_PR_CmpNotEqStrict,  0, s2_op_f_cmp, 0, 0,
    S2_T_CmpEqStrict },
  { "X>Y",   S2_T_CmpGT,          2, -1, S2_PR_CmpGT,       0, s2_op_f_cmp, 0, 0, 0 },
  { "X>=Y",  S2_T_CmpGE,          2, -1, S2_PR_CmpGE,       0, s2_op_f_cmp,
    S2_T_CmpGT, S2_T_CmpEq, S2_T_CmpLT /* i.e. X>=Y ==> !(X<Y) */ },
  { "inherits", S2_T_OpInherits,  2, -1, S2_PR_OpInherits,  0, s2_op_f_cmp, 0, 0, 0 },
  { "!inherits", S2_T_OpNotInherits,  2, -1, S2_PR_OpInherits,  0, s2_op_f_cmp, 0, 0, 0 },
  { "X=~Y", S2_T_OpContains,      2, -1, S2_PR_Contains,    0, s2_op_f_cmp, 0, 0, 0 },
  { "X!~Y", S2_T_OpNotContains,   2, -1, S2_PR_NotContains, 0, s2_op_f_cmp, 0, 0, 0 },

  { "X=Y",     S2_T_OpAssign,       2,  1, S2_PR_OpAssign,    0, s2_op_f_assign2, 0, 0, 0 },
  { "X:=Y",    S2_T_OpColonEqual,   2,  1, S2_PR_OpAssign,    0, 0/*handled by parser*/, 0, 0, 0 },
  { "X.Y=Z",   S2_T_OpAssign3,      3,  1, S2_PR_OpAssign,    0, s2_op_f_assign3, 0, 0, 0 },
  { "X.Y:=Z",  S2_T_OpAssignConst3, 3,  1, S2_PR_OpAssign,    0, s2_op_f_assign3, 0, 0, 0 },
  { "X+=Y",    S2_T_OpPlusAssign,   2,  1, S2_PR_OpAssign,    0, s2_op_f_assign2, 0, 0,
    S2_T_OpPlus },
  { "X.Y+=Z",  S2_T_OpPlusAssign3,  3,  1, S2_PR_OpAssign,    0, s2_op_f_assign3, 0, 0,
    S2_T_OpPlus },
  { "X-=Y",    S2_T_OpMinusAssign,  2,  1, S2_PR_OpAssign,    0, s2_op_f_assign2, 0, 0,
    S2_T_OpMinus },
  { "X.Y-=Z",  S2_T_OpMinusAssign3,  3,  1, S2_PR_OpAssign,    0, s2_op_f_assign3, 0, 0,
    S2_T_OpMinus },
  { "X*=Y",    S2_T_OpMultiplyAssign,  2, 1, S2_PR_OpAssign,    0, s2_op_f_assign2, 0, 0,
    S2_T_OpMultiply },
  { "X.Y*=Z",  S2_T_OpMultiplyAssign3, 3, 1, S2_PR_OpAssign,    0, s2_op_f_assign3, 0, 0,
    S2_T_OpMultiply },
  { "X/=Y",    S2_T_OpDivideAssign,    2,  1, S2_PR_OpAssign,    0, s2_op_f_assign2, 0, 0,
    S2_T_OpDivide },
  { "X.Y/=Z",  S2_T_OpDivideAssign3,   3, 1, S2_PR_OpAssign,    0, s2_op_f_assign3, 0, 0,
    S2_T_OpDivide },
  { "X%=Y",    S2_T_OpModuloAssign,    2,  1, S2_PR_OpAssign,    0, s2_op_f_assign2, 0, 0,
    S2_T_OpModulo },
  { "X.Y%=Z",  S2_T_OpModuloAssign3,   3,  1, S2_PR_OpAssign,   0, s2_op_f_assign3, 0, 0,
    S2_T_OpModulo },
  { "X<<=Y",   S2_T_OpShiftLeftAssign, 2,  1, S2_PR_OpAssign,    0, s2_op_f_assign2, 0, 0,
    S2_T_OpShiftLeft },
  { "X.Y<<=Z", S2_T_OpShiftLeftAssign3, 3,  1, S2_PR_OpAssign,    0, s2_op_f_assign3, 0, 0,
    S2_T_OpShiftLeft },
  { "X>>=Y",   S2_T_OpShiftRightAssign, 2, 1, S2_PR_OpAssign,    0, s2_op_f_assign2, 0, 0,
    S2_T_OpShiftRight },
  { "X.Y>>=Z", S2_T_OpShiftRightAssign3, 3,  1, S2_PR_OpAssign,    0, s2_op_f_assign3, 0, 0,
    S2_T_OpShiftRight },
  { "X^=Y",    S2_T_OpXOrAssign,    2, 1, S2_PR_OpAssign,    0, s2_op_f_assign2, 0, 0,
    S2_T_OpXOr },
  { "X.Y^=Z",  S2_T_OpXOrAssign3,   3,  1, S2_PR_OpAssign,    0, s2_op_f_assign3, 0, 0,
    S2_T_OpXOr },
  { "X&=Y",    S2_T_OpAndAssign,    2,  1, S2_PR_OpAssign,    0, s2_op_f_assign2, 0, 0,
    S2_T_OpAnd },
  { "X.Y&=Z",  S2_T_OpAndAssign3,   3,  1, S2_PR_OpAssign,    0, s2_op_f_assign3, 0, 0,
    S2_T_OpAnd },
  { "X|=Y",    S2_T_OpOrAssign,     2,  1, S2_PR_OpAssign,    0, s2_op_f_assign2, 0, 0,
    S2_T_OpOr },
  { "X.Y|=Z",  S2_T_OpOrAssign3,    3,  1, S2_PR_OpAssign,    0, s2_op_f_assign3, 0, 0,
    S2_T_OpOr },

  { "++X", S2_T_OpIncrPre,        1,  1, S2_PR_IncrDecr,   -1, s2_op_f_incrdecr, 0, 0, 0 },
  { "X++", S2_T_OpIncrPost,       1, -1, S2_PR_IncrDecr,    1, s2_op_f_incrdecr, 0, 0, 0 },
  { "--X", S2_T_OpDecrPre,        1,  1, S2_PR_IncrDecr,   -1, s2_op_f_incrdecr, 0, 0, 0 },
  { "X--", S2_T_OpDecrPost,       1, -1, S2_PR_IncrDecr,    1, s2_op_f_incrdecr, 0, 0, 0 },

  { "(",   S2_T_ParenOpen,        0, -1, S2_PR_ParensOpen, -1, s2_op_f_parens, 0, 0, 0 },
  { ")",   S2_T_ParenClose,       0, -1, S2_PR_ParensClose, 1, s2_op_f_parens, 0, 0, 0 },

  { "X,Y",   S2_T_Comma,          2, -1, S2_PR_Comma,       0, s2_op_f_comma, 0, 0, 0 },
  { "X.Y",   S2_T_OpDot,          2, -1, S2_PR_DotDeref,    0, s2_op_f_dot, 0, 0, 0 },
  { "X..",   S2_T_OpDotDot,       2, -1, S2_PR_DotDot,      0, 0, 0, 0, 0 },
  { "X.#",   S2_T_OpDotLength,    1, -1, S2_PR_DotDeref,    1, s2_op_f_dot_length, 0, 0, 0 },
  { "X#Y",   S2_T_OpHash,         2, -1, S2_PR_DotDeref,    0, s2_op_f_dot, 0, 0, 0 },
  { "X->Y",  S2_T_OpArrow,        2, -1, S2_PR_OpArrow,     0, s2_op_f_oload_arrow, 0, 0, 0 },
  /* S2_T_OpArrow2 is an internal-use pseudo-operator used to tell s2_eval_expr_impl() that
     it should stop processing. It is ONLY syntactically valid when a caller of s2_eval_expr_impl()
     passes this op as the "fromLhsOp" parameter.
     
     201608: Is that still true? i'd like to make => overrideable, but that would break
     foreach(x=>...). We could rename that to foreach(x as ...)?
   */
  { "X=>Y",  S2_T_OpArrow2,       0, -1,  S2_PR_OpArrow2,    0, 0, 0, 0, 0 },
  { "X::Y",  S2_T_OpColon2,       2, -1,  S2_PR_OpColon2,    0, s2_op_f_oload_arrow, 0, 0, 0 },

  { "X[]=Y", S2_T_ArrayAppend,    2,  1, S2_PR_ArrayAppend, 0, s2_op_f_array_append, 0, 0, 0 },

  { "X?Y:Z",  S2_T_Question,      3,  1, S2_PR_TernaryIf,   0, 0/*handled at parser level */, 0, 0, 0 },

  /* RHSEval was initially an experiment in handling precedence during
     recursive evaling, but it's no longer used. */
  { "...RHS", S2_T_RHSEval,       1,  1, S2_PR_RHSEval,     0, 0/*handled at parser level */ , 0, 0, 0 },

  /* FOO is/was only used for internal by-hand tests of the stack
     machine. */
  { "FOO...", S2_T_Foo,          -1,  1, S2_PR_Keyword,     0, s2_op_f_foo, 0, 0, 0 },

  /*_sentry_*/s2_op_empty_m
};


s2_op const * s2_ttype_op( int ttype ){
  switch( ttype ){
#define OP(E,M) case E: return &S2_OPS.M
    OP(S2_T_OpPlusUnary,add1);
    OP(S2_T_OpPlus,add2);
    OP(S2_T_OpMinusUnary,sub1);
    OP(S2_T_OpMinus,sub2);

    OP(S2_T_OpMultiply,multiply);
    OP(S2_T_OpDivide,divide);
    OP(S2_T_OpModulo,modulo);

    OP(S2_T_OpOrBitwise,bitOr);
    OP(S2_T_OpXOr,bitXOr);
    OP(S2_T_OpAndBitwise,bitAnd);
    OP(S2_T_OpNegateBitwise,bitNegate);
    OP(S2_T_OpShiftLeft,bitShiftLeft);
    OP(S2_T_OpShiftRight,bitShiftRight);

    OP(S2_T_OpNot,logNot);
    OP(S2_T_OpAnd,logAnd);
    OP(S2_T_OpOr,logOr);
    OP(S2_T_OpOr3,logOr3);
    OP(S2_T_OpElvis,elvis);

    OP(S2_T_CmpLT,cmpLT);
    OP(S2_T_CmpLE,cmpLE);
    OP(S2_T_CmpEq,cmpEq);
    OP(S2_T_CmpEqStrict,cmpEqStrict);
    OP(S2_T_CmpNotEq,cmpNotEq);
    OP(S2_T_CmpNotEqStrict,cmpNotEqStrict);
    OP(S2_T_CmpGT,cmpGT);
    OP(S2_T_CmpGE,cmpGE);
    OP(S2_T_OpInherits,inherits);
    OP(S2_T_OpNotInherits,notInherits);
    OP(S2_T_OpContains,contains);
    OP(S2_T_OpNotContains,notContains);

    OP(S2_T_OpAssign,assign);
    OP(S2_T_OpColonEqual,assignConst);
    OP(S2_T_OpAssign3,assignMember);
    OP(S2_T_OpAssignConst3,assignMemberConst);
    OP(S2_T_OpPlusAssign,assignPlus);
    OP(S2_T_OpPlusAssign3,assignPlus3);
    OP(S2_T_OpMinusAssign,assignMinus);
    OP(S2_T_OpMinusAssign3,assignMinus3);
    OP(S2_T_OpMultiplyAssign,assignMult);
    OP(S2_T_OpMultiplyAssign3,assignMult3);
    OP(S2_T_OpDivideAssign, assignDiv);
    OP(S2_T_OpDivideAssign3, assignDiv3);
    OP(S2_T_OpModuloAssign, assignModulo);
    OP(S2_T_OpModuloAssign3, assignModulo3);
    OP(S2_T_OpShiftLeftAssign, assignShiftLeft);
    OP(S2_T_OpShiftLeftAssign3, assignShiftLeft3);
    OP(S2_T_OpShiftRightAssign, assignShiftRight);
    OP(S2_T_OpShiftRightAssign3, assignShiftRight3);
    OP(S2_T_OpXOrAssign, assignXOr);
    OP(S2_T_OpXOrAssign3, assignXOr3);
    OP(S2_T_OpAndAssign, assignAnd);
    OP(S2_T_OpAndAssign3, assignAnd3);
    OP(S2_T_OpOrAssign, assignOr);
    OP(S2_T_OpOrAssign3, assignOr3);

    OP(S2_T_OpIncrPre, incrPre);
    OP(S2_T_OpIncrPost, incrPost);
    OP(S2_T_OpDecrPre, decrPre);
    OP(S2_T_OpDecrPost, decrPost);

    OP(S2_T_ParenOpen,parenOpen);
    OP(S2_T_ParenClose,parenClose);

    OP(S2_T_Comma,comma);
    OP(S2_T_OpDot,dot);
#if 0
    OP(S2_T_OpDotDot,dotDot);
#else
    case S2_T_OpDotDot: return 0;
#endif
    OP(S2_T_OpDotLength,dotLength);
    OP(S2_T_OpHash,dotHash);
    OP(S2_T_OpArrow,arrow);
    OP(S2_T_OpArrow2,arrow2);
    OP(S2_T_OpColon2,colon2);
    OP(S2_T_ArrayAppend,arrayAppend);
    OP(S2_T_Question,ternary);
    OP(S2_T_RHSEval,rhsEval);
    OP(S2_T_Foo,foo);
#undef OP
    default:
      return 0;
  }
}

s2_op const * s2_stoken_op( s2_stoken const * t ){
  return t ? s2_ttype_op( t->ttype ) : 0;
}

int s2_op_is_unary_prefix( s2_op const * op ){
  return (1==op->arity && op->placement<0) ? op->id : 0;
}


int s2_op_is_math( s2_op const * op ){
  switch(op ? op->id : 0){
    case S2_T_OpPlus:
    case S2_T_OpMinus:
    case S2_T_OpMultiply:
    case S2_T_OpDivide:
    case S2_T_OpModulo:
    case S2_T_OpOrBitwise:
    case S2_T_OpXOr:
    case S2_T_OpAndBitwise:
    case S2_T_OpShiftRight:
    case S2_T_OpShiftLeft:
      return op->id;
    default:
      return 0;
  }
}

int s2_op_is_expr_border( s2_op const * op ){
  switch(op ? op->id : 0){
    case S2_T_ParenOpen:
    case S2_T_ParenClose:
    case S2_T_BraceOpen:
    case S2_T_BraceClose:
    case S2_T_Comma:
    case S2_T_Semicolon:
    case S2_T_Colon:
      return op->id;
    default:
      return 0;
  }
}

int s2_op_short_circuits( s2_op const * op ){
#if 1
  return op ? s2_ttype_short_circuits(op->id) : 0;
#else
  switch( op ? op->id : 0 ){
    case S2_T_OpOr:
    case S2_T_OpAnd:
    case S2_T_Question:
      return op->id;
    default: return 0;
  }
#endif
}

/**
   If ttype refers to an overloadable operator type and v is _capable_
   of overloading that operator, this function returns the string form
   of the overloaded operator method and sets *len (if not NULL) to
   the length of that name. It does not confirm that v actually does
   overload it, only that the type/operator combination is potentially
   legal.

   Returns 0 if v is of a type or type/operator combination for which
   we prohibit overloading.
*/
char const * s2_overload_name( int ttype, cwal_value const * v, cwal_size_t * len ){
  if(len) *len = 0;

  switch(ttype){
    /* overloadable for any types (these only exist for overloading)... */
#define CASE(W,T) case T: if(len) *len=sizeof(W)-1/*NUL!*/; return W
    CASE("operator->", S2_T_OpArrow);
    CASE("operator=~", S2_T_OpContains);
    CASE("operator!~", S2_T_OpNotContains);
    /*CASE("operator..", S2_T_OpDotDot);*/
    CASE("operator::", S2_T_OpColon2);
#undef CASE
    default:
      break;
  };

  switch(cwal_value_type_id(v)){
    /* These type/operator combinations are prohibited */
    case CWAL_TYPE_UNDEF:
    case CWAL_TYPE_NULL:
    case CWAL_TYPE_BOOL:
    case CWAL_TYPE_INTEGER:
    case CWAL_TYPE_DOUBLE:
    case CWAL_TYPE_UNIQUE:
      return 0;
    case CWAL_TYPE_STRING:
      /* This subset is up for reconsideration. Is there a compelling
         reason to disallow the others, aside from the convenience of
         relying on default string-to-number coercion we get with
         them? Maybe just prohibit overloading unary -/+?
      */
      switch(ttype){
        case S2_T_OpPlus: break;
        case S2_T_OpMultiply: break;
        case S2_T_OpModulo: break;
        case S2_T_OpDivide: break;
        default:
          if(s2_ttype_is_assignment_combo(ttype)) break;
          else return 0;
      }
    default:
      break;
  }

  switch(ttype){
    /* These ops require overloads for all types not ruled out
       above. */
#define CASE(W,T) case T: if(len) *len=sizeof(W)-1/*NUL!*/; return W
#define COMBO(T, W,W2) CASE(W, T); CASE(W2, T##Assign); CASE(W2, T##Assign3)
    COMBO(S2_T_OpPlus, "operator+", "operator+=");
    COMBO(S2_T_OpMinus, "operator-", "operator-=");
    COMBO(S2_T_OpMultiply, "operator*", "operator*=");
    COMBO(S2_T_OpDivide, "operator/", "operator/=");
    COMBO(S2_T_OpModulo, "operator%", "operator%=");
    COMBO(S2_T_OpShiftLeft, "operator<<", "operator<<=");
    COMBO(S2_T_OpShiftRight, "operator>>", "operator>>=");
    COMBO(S2_T_OpXOr, "operator^", "operator^=");
    CASE("operator&", S2_T_OpAndBitwise);
    CASE("operator&=", S2_T_OpAndAssign);
    CASE("operator&=", S2_T_OpAndAssign3);
    CASE("operator|", S2_T_OpOrBitwise);
    CASE("operator|=", S2_T_OpOrAssign);
    CASE("operator|=", S2_T_OpOrAssign3);
#undef COMBO

    CASE("+operator", S2_T_OpPlusUnary);
    CASE("-operator", S2_T_OpMinusUnary);

    CASE("++operator", S2_T_OpIncrPre);
    CASE("operator++", S2_T_OpIncrPost);

    CASE("--operator", S2_T_OpDecrPre);
    CASE("operator--", S2_T_OpDecrPost);

    CASE("operator<", S2_T_CmpLT);
    CASE("operator<=", S2_T_CmpLE);

    CASE("operator>", S2_T_CmpGT);
    CASE("operator>=", S2_T_CmpGE);

    CASE("operator==", S2_T_CmpEq);
    CASE("operator!=", S2_T_CmpNotEq);

    /* CASE("operator#", S2_T_OpHash);

       Overloading of # was removed, as it (A) will complicate
       extending # to support assignment (insert()) and (B) it
       essentially negates the reason for having the # operator: an
       access method for hashes which makes them competative with
       Objects (using Hash.search() has function call overhead which
       obj[prop] does not, making objects faster for most cases).
    */
    /* CASE("operator.", S2_T_OpDot); */
#undef CASE
    default:
      if(len) *len = 0;
      return 0;
  };
}

/**
   Internal helper for overloadable operators.

   op is the operator being acted upon and it must have an arity of 1
   or 2. se is the interpreter. lhs and rhs are the LHS and RHS of the
   operator (their requirements are described below).

   If no overload of op is legal for this type (see
   s2_overload_name()) then 0 is returned and *theRc is set to 0.
   i.e. a non-error.

   The POD types either do not participate, or participate in
   castrated form (e.g. CWAL_TYPE_STRING only observes a limited set
   of overloads).

   If non-0 is returned and *theRc is 0 after returning, it means an
   overload was successfully called and its result value is stored in
   *rv. If 1 is returned, an overload was truly found. If 2 is
   returned, a synthesized overload was found, in which case the
   caller should possibly force "return this" semantics on its result
   before applying it.

   If *theRc is set to non-0 after returning, it means an error was
   triggered, and the caller must not proceed with the operation.

   It may return >0 and set *theRc to non-0, meaning that the error
   indicated by *theRc was triggered by/via an overload.

   If an overload is legal but (lhs ? lhs : rhs) does not contain it,
   an error is triggered describing the missing operator (if requireIt
   is true), or *theRc set it to 0 and 0 is returned if !requireIt.

   We require explicit overloading for all non-POD types, and treat
   missing ops as an error unless requireIt is false. The thinking is:
   rather that than using built-in ops in senseless contexts. i don't
   want s2 to turn into a truly type-sloppy language, despite being
   "loosely typed." If this proves to be a hindrance, we can always
   make it more sloppy by calling back to the built-in operators
   (i.e. coercion to numeric types).

   Conventions:

   For overload calls, the 'this' is always (lhs ? lhs : rhs),
   and it may not be 0.

   if op->artity==3, neither lhs nor rhs may be NULL. The op is
   assumed to be an object property assignment form of a binary
   operation. e.g. X+=Z vs X.Y+=Z. The overload is called with lhs as
   the 'this' and (rhs) argument.

   if op->arity==2:

   - lhs!=0, rhs!=0, assumed to be an infix binary operator. The
   overload is called with lhs as the 'this' and (rhs) argument.

   If op->arity==1: 

   - One of lhs or rhs must be 0.

   - If op->placement<0 (prefix op) or op->placement>0 (postfix op)
   then the overload is called without parameters.
*/
static char s2_op_check_overload( s2_op const * op, s2_engine * se,
                                  char requireIt,
                                  cwal_value * lhs, cwal_value * rhs,
                                  cwal_value ** rv, int * theRc ){
  cwal_value * theThis = lhs ? lhs : rhs;
  cwal_size_t opLen = 0;
  char const * opName = s2_overload_name( op->id, theThis, &opLen );
  cwal_value * fv = 0;
  cwal_function * fn;
  int rc;
  /* s2_dump_val(theThis,op->sym); */
  /* MARKER(("op->sym=%s\n", op->sym)); */
  if(!opName) {
#if 0
    switch(cwal_value_type_id(theThis)){
      case CWAL_TYPE_STRING:
        if(s2_ttype_is_assignment_combo(op->id)){
          *theRc = s2_engine_err_set(se, CWAL_RC_UNSUPPORTED,
                                     "Immutable strings may not "
                                     "use the %s operator.",
                                     op->sym);
          break;
        }
        CWAL_SWITCH_FALL_THROUGH;
      default:
        *theRc = 0;
    }
#else
    *theRc = 0;
#endif
    return 0;
  }
  /* MARKER(("opName=%s\n", opName)); */
  rc = s2_get( se, theThis, opName, opLen, &fv );
  if(rc){
    *theRc = rc;
    return 0;
  }
  else if(!fv || !(fn = cwal_value_function_part(se->e, fv))){
    /* Not found or non-function property */

    /*
      TODO: check op->inferLeft and/or op->inferRight and see if we
      can create a combo operator out of one or two others which
      theThis does overload:

      != and !== can be inferred by applying ! to the ==/=== operators.

      Other ops can specify op->inferLeft/Right to tell us which
      two ops to combine.
    */
#if 0
    /*
      Enable this block to automatically derive combo assignment ops
      from their non-assignment variants.

      20160214: this actually seems to work, but has an interesting
      problem/property:

      var a = [1,2,3];
      a.'operator+' = proc(self,arg){return [@self,arg]}; // return !this
      a += 4;
      // ^^^^ [1,2,3,4], but a different instance, so a.operator+
      // is now gone!

      Is seems the only sane behaviour in overloaded ops is to always
      return "this". But maybe there are other valid uses? Can't think
      of any. Should we force 'this' as the result for such overloads?
    */
    if(op->derivedFromOp){
      char cc;
      assert(s2_ttype_is_assignment_combo(op->id));
#  if 1
      MARKER(("Checking derived op %s\n", s2_ttype_op(op->derivedFromOp)->sym ));
#  endif
      assert(s2_ttype_op(op->derivedFromOp));
      cc = s2_op_check_overload( s2_ttype_op(op->derivedFromOp),
                                 se, requireIt, lhs, rhs,
                                 rv, theRc );
      /*if(cc && !*theRc){
        s2_dump_val(lhs,"Forcing 'this' result...");
        *rv = theThis;
        }*/
      return cc ? cc+1 : cc;
    }
#endif
    if(!requireIt) *theRc = 0;
    else *theRc = s2_engine_err_set(se, CWAL_RC_NOT_FOUND,
                                    "LHS (type %s) has no '%s' "
                                    "(%s operator overload).",
                                    cwal_value_type_name(theThis),
                                    opName, op->sym);
    return 0;
  }else{
    cwal_value * argv[2] = {0,0};
    int argc = 0;
    switch(op->arity){
      case 1:
#if 0
        if(op->placement<0){
          /*prefix op*/
        }else{/*postfix*/
#if 0
          /* Historical behaviour. Removed 20190804 when the prefix
             ++/-- ops were renamed so that they no longer collide
             with (postfix ++/--). With that change, this behaviour
             is no longer needed. */
          assert(op->placement!=0);
          argc = 1;
          argv[0] = theThis;
#endif
        }
#endif
        break;
      case 2:
        argc = 1;
        /* argv[0] = lhs; */
        argv[0] = rhs;
        break;
      default:
        /* X.Y OP Z:

          var obj = {a:1};
          obj.a += 1;

          At this point in the processing we only have (a, 1), but
          that's okay, as the caller has the obj part and will apply
          the result back to it after this operation.
        */
        assert(3==op->arity);
        assert(s2_ttype_is_assignment_combo(op->id));
        argc = 1;
        /* argv[0] = lhs; */
        argv[0] = rhs;
        break;
    }
    if(lhs) cwal_value_ref(lhs);
    if(rhs) cwal_value_ref(rhs);
    if(0){
      MARKER(("overload %s: argc=%d\n", op->sym, (int)argc));
      s2_dump_val(theThis,"theThis");
      s2_dump_val(argv[0],"argv[0]");
      s2_dump_val(argv[1],"argv[1]");
    }
    *theRc = cwal_function_call( fn, theThis, rv, argc, argv );
    if(0){
      s2_dump_val(*rv, "overload call *rv");
    }
    if(lhs) cwal_value_unhand(lhs);
    if(rhs) cwal_value_unhand(rhs);
    return 1;
  }
}


int s2_op_f_addsub( s2_op const * op, s2_engine * se, int argc,
                    cwal_value **rv ){
  cwal_value * rhs = 0;
  cwal_value * lhs = 0;
  char isAdd = 0;
  switch(op->id){
    case S2_T_OpPlus:
      isAdd = 1;
      CWAL_SWITCH_FALL_THROUGH;
    case S2_T_OpMinus:
      assert( 2 == argc );
      rhs = s2__pop_val( se );
      lhs = s2__pop_val( se );
      break;
    case S2_T_OpPlusUnary:
      isAdd = 1;
      CWAL_SWITCH_FALL_THROUGH;
    case S2_T_OpMinusUnary:
      assert( 1 == argc );
      rhs = s2__pop_val( se );
      lhs = 0;
      /* s2_dump_val(rhs, "unary addsub rhs"); */
      break;
    default:
      assert(!"WRONG!");
      if(argc){/*avoid unused param warning*/}
      return CWAL_RC_ASSERT;
  }
  if(se->skipLevel>0){
    *rv = cwal_value_undefined();
    return 0;
  }else{
    int rc = 0;
    cwal_value_ref(lhs);
    cwal_value_ref(rhs);
    if(!s2_op_check_overload(op, se, 1, lhs, rhs, rv, &rc)
       && !rc){
      rc = s2_values_addsub( se, isAdd, lhs, rhs, rv);
    }
    if(!rc) cwal_value_ref(*rv);
    cwal_value_unref(lhs);
    cwal_value_unref(rhs);
    if(!rc) cwal_value_unhand(*rv);
    return rc;
  }
}

int s2_op_f_multdivmod( s2_op const * op, s2_engine * se, int argc,
                        cwal_value **rv ){
  cwal_value * rhs;
  cwal_value * lhs;
  int mode;
  assert( 2 == argc );
  rhs = s2__pop_val( se );
  lhs = s2__pop_val( se );
  switch( op->id ){
    case S2_T_OpMultiply: mode = 1; break;
    case S2_T_OpDivide: mode = -1; break;
    case S2_T_OpModulo: mode = 0; break;
    default:
      assert(!"Invalid op binding!");
      if(argc){/*avoid unused param warning*/}
      return CWAL_RC_ASSERT;
  }
  cwal_value_ref(lhs);
  cwal_value_ref(rhs);
  if(se->skipLevel>0){
    *rv = cwal_value_undefined();
    cwal_value_unref(rhs);
    cwal_value_unref(lhs);
    return 0;
  }else{
    int rc = 0;
    if(!s2_op_check_overload(op, se, 1, lhs, rhs, rv, &rc)
       && !rc){
      rc = s2_values_multdivmod( se, mode, lhs, rhs, rv);
    }
    if(!rc) cwal_value_ref(*rv);
    cwal_value_unref(rhs);
    cwal_value_unref(lhs);
    if(!rc) cwal_value_unhand(*rv);
    return rc;
  }
}

int s2_op_f_bitwiseshift( s2_op const * op, s2_engine * se, int argc,
                          cwal_value **rv ){
  cwal_value * vlhs = 0, * vrhs = 0;
  assert((S2_T_OpNegateBitwise==op->id) ? (1==argc) : (2==argc));
  vrhs = s2__pop_val(se);
  assert(vrhs);
  if(S2_T_OpNegateBitwise != op->id){
    vlhs = s2__pop_val(se);
    assert(vlhs);
    cwal_value_ref(vlhs);
    if(argc){/*avoid unused param warning*/}
  }
  cwal_value_ref(vrhs);
  if(se->skipLevel>0){
    *rv = cwal_value_undefined();
    assert(cwal_value_undefined()==vrhs);
    assert(!vlhs || (cwal_value_undefined()==vlhs));
    /*cwal_refunref(vrhs);
      if(vlhs && vlhs != rhs) cwal_refunref(vlhs);*/
    return 0;
  }else{
    int rc = 0;
    if(!s2_op_check_overload(op, se, 1, vlhs, vrhs, rv, &rc)
       && !rc){
      rc = s2_values_bitwiseshift( se, op->id, vlhs, vrhs, rv );
    }
    if(!rc) cwal_value_ref(*rv);
    cwal_value_unref(vlhs);
    cwal_value_unref(vrhs);
    if(!rc) cwal_value_unhand(*rv);
    return rc;
  }
}

int s2_op_f_not( s2_op const * op, s2_engine * se, int argc,
                    cwal_value **rv ){
  cwal_value * rhs;
  if(op || argc){/*avoid unused param warning*/}
  assert(1==argc);
  rhs = s2__pop_val(se);
  cwal_value_ref(rhs);
  *rv = (se->skipLevel>0)
    ? cwal_value_undefined()
    : cwal_new_bool( !cwal_value_get_bool(rhs) );
  assert(cwal_value_is_builtin(*rv));
  cwal_value_unref(rhs);
  return 0;
}

int s2_op_f_andor( s2_op const * op, s2_engine * se, int argc,
                      cwal_value **rv ){
  cwal_value * rhs, * lhs;
  assert(2==argc);
  rhs = s2__pop_val(se);
  lhs = s2__pop_val(se);
  assert(rhs);
  assert(lhs);
  cwal_value_ref(rhs);
  cwal_value_ref(lhs);
  /**
     In theory, if se->skipLevel>0 and if all operators comply and use
     cwal_value_undefined() for all results in skip mode, then rhs is
     guaranteed to be the Undefined value and lhs might (depending on
     where skip-mode was invoked) be Undefined. We "might" want to
     assert here that rhs===Undefined, to enforce the
     must-be-undefined convention, but i've also half a mind to always
     return (AND ? TRUE : FALSE) from this op on short-circuit mode,
     the logic being that short-circuit mode "should" (i think)
     consume as much as possible, and returning that bool from here
     might help ensure that the downstream can do so. Something to
     think about.
  */
  switch((se->skipLevel>0) ? 0 : op->id){
    case 0:
      /**
         Reminder to self: if i'm not mistaken, the short-circuiting
         ops (and/or/or3 and ternary if) are the only ones which can
         have a non-undefined value as an operand in short-circuit
         mode. i.e. we (theoretically) don't need to cwal_refunref()
         in any skip-mode blocks in other operators.
      */
      *rv = cwal_value_undefined();
      break;
    case S2_T_OpAnd:
      *rv = cwal_new_bool( cwal_value_get_bool(lhs)
                           && cwal_value_get_bool(rhs) );
      break;
    case S2_T_OpOr:
      *rv = cwal_new_bool( cwal_value_get_bool(lhs)
                           || cwal_value_get_bool(rhs) );
      break;
    case S2_T_OpElvis:
      *rv = cwal_value_undefined()==lhs ? rhs : lhs;
      break;
    case S2_T_OpOr3:
      *rv = cwal_value_get_bool(lhs) ? lhs : rhs;
      break;
    default:
      if(argc){/*avoid unused param warning*/}
      assert(!"Invalid op mapping!");
  }
  cwal_value_ref(*rv);
  cwal_value_unref(rhs);
  cwal_value_unref(lhs);
  cwal_value_unhand(*rv);
  return 0;
}

static int s2_op_f_cmp( s2_op const * op, s2_engine * se,
                           int argc, cwal_value **rv ){
  cwal_value * rhs;
  cwal_value * lhs;
  int rc = 0;
  assert(2==argc);
  rhs = s2__pop_val(se);
  lhs = s2__pop_val(se);
  cwal_value_ref(rhs);
  cwal_value_ref(lhs);
  if(se->skipLevel>0){
    cwal_value_unref(rhs);
    cwal_value_unref(lhs);
    *rv = cwal_value_undefined();
    return 0;
  }
#if 0
  MARKER(("COMPARE op %s\n", op->sym));
  s2_dump_val(lhs, "LHS");
  s2_dump_val(rhs, "RHS");
#endif
  switch(op->id){
    case S2_T_OpNotInherits:
      rc = !cwal_value_derives_from( se->e, lhs, rhs );
      break;
    case S2_T_OpInherits:
      rc = cwal_value_derives_from( se->e, lhs, rhs );
      break;
    case S2_T_CmpEqStrict:
    case S2_T_CmpNotEqStrict:{
      if(rhs == lhs){
        rc = 0;
      }else{
        int const tidL = cwal_value_type_id(lhs);
        int const tidR = cwal_value_type_id(rhs);
        if(tidL == tidR){
          rc = cwal_value_compare(lhs, rhs);
        }else{
          rc = tidL - tidR;
        }
      }
      rc = (S2_T_CmpNotEqStrict==op->id)
        ? (0!=rc)
        : (0==rc);
      break;
    }
    default:{
#if 1
      char const requiresOverload = 1;
      /* strange: it wasn't until 20160121 that i realized that ({} ==
         {}) throws an exception. Maybe it shouldn't? If it doesn't,
         though, we've also got to allow default impls of other
         comparison ops. Is that wrong? */
#else
      char requiresOverload = 1;
      switch(op->id){
        case S2_T_CmpEq:
        case S2_T_CmpNotEq:
          requiresOverload = 0;
          break;
        default:
          requiresOverload = 1;
          break;
      }
#endif
      if(s2_op_check_overload(op, se, requiresOverload,
                              lhs, rhs, rv, &rc)
         || rc){
        if(!rc) cwal_ref(*rv);
        cwal_value_unref(rhs);
        cwal_value_unref(lhs);
        if(!rc) cwal_unhand(*rv);
        return rc;
      }
      rc = cwal_value_compare(lhs, rhs);
      switch(op->id){
        case S2_T_CmpLT: rc = rc < 0; break;
        case S2_T_CmpLE: rc = rc <= 0; break;
        case S2_T_CmpGT: rc = rc > 0; break;
        case S2_T_CmpGE: rc = rc >= 0; break;
        case S2_T_CmpEq: rc = 0 == rc; break;
        case S2_T_CmpNotEq: rc = 0 != rc; break;
        default:
          assert(!"CANNOT HAPPEN");
          if(argc){/*avoid unused param warning*/}
          cwal_value_unref(rhs);
          cwal_value_unref(lhs);
          return CWAL_RC_CANNOT_HAPPEN;
      }
    }
  }
  *rv = rc ? cwal_value_true() : cwal_value_false();
  cwal_value_unref(rhs);
  cwal_value_unref(lhs);
  assert(cwal_value_is_builtin(*rv));
  return 0;
}


int s2_op_f_parens( s2_op const * op, s2_engine * se,
                       int argc, cwal_value **rv ){

  /*
    TODO someday: re-add parens support into the stack machine, so
    that they can be used independently of script code. Not likely to
    happen until i want to use the stack machine that way, though.
  */
  assert(!"')' is handled higher up!\n");
  if(S2_T_ParenOpen==op->id){
    MARKER(("We've hit the internal parens impl!\n"));
    assert(!"'(' is handled higher up.");
    assert(0==argc);
    /* Tell s2_process_op() to keep the result
       of the prior expression.
    */
    if(se || argc){/*avoid unused param warning*/}
    *rv = 0;
    return 0;
  }
  return 0;
}

int s2_op_f_comma( s2_op const * op, s2_engine * se,
                      int argc, cwal_value **rv ){
  assert(2==argc);
  assert(S2_T_Comma==op->id);
  if(se->skipLevel){
    /* discard both args */
    cwal_value * v1, * v2;
    v1 = s2__pop_val(se);
    v2 = s2__pop_val(se);
    *rv = cwal_value_undefined();
    assert(v1 && v2);
    /* v1/v2 are likely, but not guaranteed, to
       be cwal_value_undefined(), but in case
       they're not... */
    if(v2!=v1) cwal_refunref(v2);
    cwal_refunref(v1);
    if(op || argc){/*avoid unused param warning*/}
  }else{
    cwal_value * v;
    *rv = s2__pop_val(se) /* RHS */;
    v = s2__pop_val(se) /* discard LHS, but... */;
    assert(v);
    cwal_value_ref(*rv);
    /*
      Ref/unref cleans up any temps on the lhs (e.g. x++, y++).
      This allows this for loop to avoid temporarily leaking
      998 integers into a higher scope:

      var x = 0;
      for( var i = 0; x < 1000; x++, ++i );

      Without this hack, the x++ result becomes a temporary in its
      owning scope (x's) until the loop finishes and can clean up.
    */
    cwal_refunref(v);
    cwal_value_unhand(*rv);
  }
  return 0;
}

int s2_op_f_dot( s2_op const * op, s2_engine * se,
                 int argc, cwal_value **rv ){
  int rc = 0;
  cwal_value * rhs = 0, * lhs, *lhsProto = 0;
  char isUniqueDotValue = 0;
  assert(S2_T_OpDot==op->id || S2_T_OpHash==op->id);
  assert(2==argc);
  rhs = s2__pop_val(se);
  lhs = s2__pop_val(se);
  cwal_value_ref(lhs);
  cwal_value_ref(rhs);
  /* s2_dump_val(rhs,"se->dotOp.key"); */
  /* s2_dump_val(lhs,"se->dotOp.lhs"); */
  if(!se->skipLevel && cwal_value_is_unique(lhs)){
    /* assume enum entry, allow only 'value' and 'prototype'
       pseudo-properties */
    if(!s2_value_is_value_string(se, rhs)
       && !s2_value_is_prototype_string(se, rhs)){
      rc = s2_engine_err_set(se, CWAL_RC_TYPE,
                             "Invalid key for '%s' operator on LHS of type '%s'.",
                             op->sym,
                             cwal_value_type_name(lhs)
                             /* ^^^ careful: lifetime! */);
      cwal_value_unref(lhs);
      cwal_value_unref(rhs);
      if(argc){/*avoid unused param warning*/}
      return rc;
    }
    isUniqueDotValue = 1;
  }
  if(!se->skipLevel
     && !isUniqueDotValue
     && ((!(lhsProto=cwal_value_prototype_get(se->e, lhs))
          && !cwal_props_can(lhs))
#if 1
         || (S2_T_OpHash==op->id && s2_value_is_enum(lhs))
      /* we disable the '#' on enums solely to force that
         clients do not use it with them, as enums
         might use Object storage instead of hashes. */
      /* As of 2020-02-21, enums are always hashes, so this
         limitation is no longer necessary. */
      /* However... if we allow this op then it behaves, on enums,
         exaclty as the -> op, except that the one requies an overload
         lookup. Having 2 ops with identical results makes me queasy,
         and i have client-side script code which makes use of ->, so
         we'll keep that one.*/
#endif
         )
     ){
    rc = s2_engine_err_set(se, CWAL_RC_TYPE,
                           "Invalid LHS value (type %s) for "
                           "'%s' operator.",
                           cwal_value_type_name(lhs),
                           op->sym);
  }
  else if(se->skipLevel){
    if(S2_T_OpDot == op->id){
      s2_dotop_state( se, lhs, lhs, rhs );
      assert( se->dotOp.self == se->dotOp.lhs );
      assert( se->dotOp.self == lhs );
      assert( se->dotOp.key == rhs );
    }else{
      s2_dotop_state( se, 0, cwal_value_undefined(),
                      cwal_value_undefined() );
      assert(!se->dotOp.self);
      assert(cwal_value_undefined() == se->dotOp.lhs);
      assert(cwal_value_undefined() == se->dotOp.key);
    }
    *rv = cwal_value_undefined();
    rc = 0;
  }
  else{ /* X.Y and X#Y... */
    cwal_value * xrv = 0;
    /* s2_dotop_state( se, 0, 0, 0 ); */
    /* s2_dump_val(lhs,"dot op lhs"); */
    if(S2_T_OpDot == op->id){
      const int noThis =
        (cwal_value_is_integer(rhs)
         && (cwal_value_is_string(lhs)
             || cwal_value_is_tuple(lhs)
             || cwal_value_array_part(se->e,lhs)))
        /* Workaround to keep array/tuple/string[int]'s LHS from
           becoming 'this' if it resolves to a function which gets
           called. i.e. myArray[1](...) must not bind myArray as
           'this'. */
        ;
      rc = s2_get_v(se, lhs, rhs, &xrv);
      if(!rc){
        s2_dotop_state( se, noThis ? 0 : lhs, lhs, rhs );
      }
    }else{ /* X#Y */
      cwal_hash * h;
      assert(S2_T_OpHash==op->id);
#if 0
      if(!s2_op_check_overload(op, se, 0, lhs, rhs, &xrv, &rc)
         && !rc){
#endif
        h = cwal_value_hash_part(se->e, lhs);
        if(!h){
          rc = s2_engine_err_set(se, CWAL_RC_TYPE,
                                 "Invalid (non-Hash) LHS for '%s' operator.",
                                 op->sym);
        }else{
          xrv = cwal_hash_search_v( h, rhs );
        }
#if 0
      }
#endif
      if(!rc) s2_dotop_state(se, 0, lhs, rhs);
    }
    cwal_value_ref(xrv);
    cwal_value_unref(lhs);
    cwal_value_unref(rhs);
    if(rc){
      cwal_value_unref(xrv);
    }else{
      *rv = xrv ? xrv : cwal_value_undefined();
      cwal_value_unhand(xrv);
    }
  }
  return rc;
}

int s2_op_f_dot_length( s2_op const * op, s2_engine * se,
                        int argc, cwal_value **rv ){
  int rc = 0;
  cwal_value * lhs;
  assert(S2_T_OpDotLength==op->id);
  assert(1==argc);
  lhs = s2__pop_val(se);
  cwal_value_ref(lhs);
  if(se->skipLevel){
    /*s2_dotop_state( se, 0, cwal_value_undefined(),
      cwal_value_undefined() );*/
    *rv = cwal_value_undefined();
    assert(cwal_value_undefined()==lhs);
    cwal_value_unref(lhs);
    rc = 0;
  }
  else{
    /* Translate X.# as X.length() if X if of type (array, tuple,
       string, buffer), but without the cwal Function call()
       overhead. */
    cwal_size_t n = 0;
    cwal_value * xrv = 0;
    switch(cwal_value_type_id(lhs)){
      case CWAL_TYPE_ARRAY:
        n = cwal_array_length_get(cwal_value_get_array(lhs));
        break;
      case CWAL_TYPE_TUPLE:
        n = cwal_tuple_length(cwal_value_get_tuple(lhs));
        break;
      case CWAL_TYPE_STRING:
        n = cwal_string_length_utf8(cwal_value_get_string(lhs));
        break;
      case CWAL_TYPE_BUFFER:
        n = cwal_value_get_buffer(lhs)->used;
        break;
      case CWAL_TYPE_HASH:
        n = cwal_hash_entry_count(cwal_value_get_hash(lhs));
        if(s2_value_is_enum(lhs)) n = n/2 /* do not count reverse mappings */;
        break;
      default:
        if(cwal_props_can(lhs)){
          n = cwal_props_count(lhs);
        }else{
          if(argc){/*avoid unused param warning*/}
          rc = s2_engine_err_set(se, CWAL_RC_TYPE,
                                 "Invalid LHS (type %s) for '%s' operator.",
                                 cwal_value_type_name(lhs),
                                 op->sym);
        }
        break;
    }
    if(!rc){
      xrv = cwal_new_integer(se->e, (cwal_int_t)n);
      if(!xrv) rc = CWAL_RC_OOM;
    }
    cwal_value_ref(xrv);
    cwal_value_unref(lhs);
    if(rc){
      assert(!xrv);
      cwal_value_unref(xrv)/*just for symmetry's sake*/;
    }else{
      assert(xrv);
      cwal_value_unhand(xrv);
      *rv = xrv;
    }
  }
  if(!rc){
    assert(*rv);
  }
  return rc;
}

int s2_op_f_oload_arrow( s2_op const * op, s2_engine * se,
                         int argc, cwal_value **rv ){
  int rc = 0;
  cwal_value * rhs, * lhs, * xrv = 0;
  assert(2==argc);
  rhs = s2__pop_val(se);
  lhs = s2__pop_val(se);
  *rv = 0;
  cwal_value_ref(lhs);
  cwal_value_ref(rhs);
  if(se->skipLevel){
    /*cwal_refunref(lhs);
    cwal_refunref(rhs);*/
    cwal_value_unref(lhs);
    cwal_value_unref(rhs);
    *rv = cwal_value_undefined();
  }else{
    assert(s2_overload_name(op->id, lhs, 0));
    if(!s2_op_check_overload(op, se, 1, lhs, rhs, &xrv, &rc)){
      assert(rc);
    }else{
      cwal_value_ref(xrv);
    }
#if 0
    /*??? can of worms ??? */
    if(!rc && S2_T_OpColon2==op->id){
      /* 20160819: this is causing a cwal-level cleanup assertion downstream. */
      /* Has to be done afterwards b/c overload can overwrite these,
         triggering an assertion in fcall() */
      s2_dotop_state( se, lhs, lhs, rhs );
      s2_dump_val(rhs,"se->dotOp.key");
      s2_dump_val(lhs,"se->dotOp.lhs");
    }
#endif
    cwal_value_unref(lhs);
    cwal_value_unref(rhs);
    if(rc){
      assert(!xrv);
      if(argc){/*avoid unused param warning*/}
    }else{
      cwal_value_unhand(xrv);
      *rv = xrv ? xrv : cwal_value_undefined();
    }
  }
  return rc;
}

int s2_handle_set_result( s2_engine * se, s2_ptoker const * pr, int rc ){
  if(rc && (pr || se->currentScript)){
    switch(rc){
      case CWAL_RC_OOM:
      case CWAL_RC_EXCEPTION:
        break;
      case CWAL_RC_NOT_FOUND:
        rc = 0;
        break;
      default:
        rc = s2_throw_err_ptoker(se, pr ? pr : se->currentScript);
        assert(rc);
        break;
    }
  }
  return rc;
}

int s2_handle_get_result( s2_engine * se, s2_ptoker const * pr, int rc ){
  if(rc && (pr || se->currentScript)){
    switch(rc){
      case CWAL_RC_OOM:
      case CWAL_RC_EXCEPTION:
        break;
      case CWAL_RC_NOT_FOUND:
        rc = 0;
        break;
      default:
        rc = s2_throw_err_ptoker(se, pr ? pr : se->currentScript);
        break;
    }
  }
  return rc;
}

/**
   Maintenance reminder: s2_op_f_assign2() and s2_op_f_assign3() share
   most of their logic, but having them separate improves their
   readability considerably. It might be useful to separate parts of
   the two implementations (e.g. the main operator dispatch switch)
   into a shared function at some point.
*/
int s2_op_f_assign3( s2_op const * op, s2_engine * se,
                     int argc, cwal_value **rv ){
  int rc = 0;
  cwal_value * rhs, * lhs, * self /* Assignment op: self[lhs]=rhs */;
  s2_stoken * vTok;
  int opReturnedLhs = -1
    /* optimization. If an operator returns the (X.Y) part of (X.Y OP
       Z) then we can skip the assignment back to X.Y.  A value of <0
       means we don't yet know. */;
  cwal_flags16_t const fSetFlags = (S2_T_OpAssignConst3 == op->id)
    ? CWAL_VAR_F_CONST : 0;
  S2_UNUSED_ARG argc;
  assert( 3==op->arity );
  rhs = s2__pop_val(se);
  vTok = s2__pop_tok(se, 1);
  lhs = vTok->value;
  self = s2__pop_val(se);
  assert(self);
  cwal_value_ref(lhs);
  cwal_value_ref(rhs);
  cwal_value_ref(self);
  s2_stoken_free(se, vTok, 1);
  vTok = 0;
  if(se->skipLevel>0){
    cwal_value_unref(lhs);
    cwal_value_unref(rhs);
    cwal_value_unref(self);
    *rv = cwal_value_undefined();
    return 0;
  }
  if(op->id != S2_T_OpAssign3 && op->id != S2_T_OpAssignConst3
     /* combo assignment, i.e. not: X.Y=Z or X.Y:=Z*/){
    /* For combo ops (all but and X.Y=Z) we need to resolve the LHS
       first because it's needed for calculating the result. For plain
       assignments we don't need to do this, and we don't allow
       overloading of those, so those can skip all this.
    */
    /*
      Reminder: this doesn't play well with the -> op. For that one,
      we really want to use the overload to get the resolved value.
      But at this point we have lost the info that there was a
      -> op. The same applies to S2_T_OpHash.
    */
    char gotOverload = 0;
    cwal_value * lhsResolved = 0;
    cwal_value * xrv = 0;
    if( (rc = s2_get_v( se, self, lhs, &lhsResolved ))
        /* Reminder to self: if we want full X->Y and X#Y support, we
           need to adjust this to use those overloads. The problem is
           that by the time this op is triggered, we no longer know
           which dot-like op triggered the access, as se->dotOp.lhs and
           friends _might_ (depending on stack conditions) have been
           cleared by now.
        */){
      assert(!lhsResolved);
      cwal_value_unref(lhs);
      cwal_value_unref(rhs);
      cwal_value_unref(self);
      return rc;
    }else if(!lhsResolved){
      /* unknown object property */
      lhsResolved = cwal_value_undefined();
      opReturnedLhs = 0 /* special case: we want to assign back over
                           this even if the op returns the undefined
                           value. */;
    }
    cwal_value_ref(lhsResolved);
    if(!(gotOverload=s2_op_check_overload(op, se, 1, lhsResolved, rhs, &xrv, &rc))
       && !rc){
      assert(!xrv);
      switch(op->id){
        case S2_T_OpPlusAssign3:
          rc = s2_values_addsub( se, 1, lhsResolved, rhs, &xrv);
          break;
        case S2_T_OpMinusAssign3:
          rc = s2_values_addsub( se, 0, lhsResolved, rhs, &xrv);
          break;
        case S2_T_OpMultiplyAssign3:
          rc = s2_values_multdivmod( se, 1, lhsResolved, rhs, &xrv);
          break;
        case S2_T_OpDivideAssign3:
          rc = s2_values_multdivmod( se, -1, lhsResolved, rhs, &xrv);
          break;
        case S2_T_OpModuloAssign3:
          rc = s2_values_multdivmod( se, 0, lhsResolved, rhs, &xrv);
          break;
        default:
          rc = s2_values_bitwiseshift( se, op->id, lhsResolved, rhs, &xrv );
          break;
      }/* the combo-assignment operators */
      if(!rc){
        assert(xrv);
        if(opReturnedLhs<0){
          opReturnedLhs = xrv==lhsResolved ? 1 : 0;
        }
        cwal_value_ref(xrv);
        cwal_value_unref(rhs);
        rhs = xrv;
      }
    }else if(gotOverload){
      if(rc){
        assert(!xrv);
      }else{
        assert(xrv);
        if(opReturnedLhs<0){
          opReturnedLhs = xrv==lhsResolved ? 1 : 0;
        }
        cwal_value_ref(xrv);
        cwal_value_unref(rhs);
        rhs = xrv;
        xrv = 0;
      }
    }
    cwal_value_unref(lhsResolved);
    lhsResolved = 0;
  }/* combo assignments */
  if(!rc){
    assert(lhs);
    assert(rhs);
    /* Initial thought was that we do not want to assign back over
       _overridden_ assignments. e.g.

       obj += 3

       must not re-assign the result over obj, as doing so
       results in non-intuitive results.

       Turns out that breaks:

       var a = "a";
       a += "b";

       which we would expect to have the value 'ab'
       
       Reminder to self:

       holding/unhand'ing the lhs/self reference can lead to
       values not being cleaned up when overwritten.
       e.g.:

       for( var x = 0; x < 1000; ++x ){}

       If we ref/unhand lhs then each x which gets overwritten gets
       unhand()ed here, and thus not cleaned up until sweep-up.
    */
#if 0
    /* an attempt at forcing "return this" semantics for
       combo assignment ops. Together with op->derivedFromOp
       handling, it seems to do what i want.

       Except that it breaks string+=string. :/

       ACH - i think we need to force those semantics on the
       _derived_ call, not this one.
    */
    if(gotOverload>1/*synthesized assignment op overload*/
       && s2_ttype_is_assignment_combo(op->id)){
      cwal_value_ref(lhsResolved);
      cwal_value_unref(rhs);
      rhs = lhsResolved;
      MARKER(("Forcing 'return this' semantics for overload...\n"));
      s2_dump_val(lhsResolved,"this is this");
    }
#endif
#if 0
    if(opReturnedLhs>0){
      MARKER(("%s opReturnedLhs=%d, so skipping assignment.\n", op->sym,
              opReturnedLhs));
    }
#endif
    opReturnedLhs = 0
      /*
        It turns out that this optimization, while it works, breaks
        const expectations. Assume X.Y is an array property which has
        been set const via C code and that array.operator+= behaves
        like array.push() but returns, as required for operator+=,
        'this'). In that case, (X.Y += 3) would (with this
        optimization) append to array X.Y while *appearing* to bypass
        the constness. The constness is not "actually" violated, but
        it would, to an observer who doesn't understand this
        optimization, appear to.
         
        Whether or not "appearances" is a reason to disallow this
        optimization is up for reconsideration.
      */;
    rc = opReturnedLhs>0
      ? 0
      : s2_handle_set_result(se, 0, s2_set_with_flags_v(se, self, lhs,
                                                        rhs, fSetFlags))
      /* Reminder: for full X#Y support this needs to be
         different. */;
  }
  if(!rc){
    *rv = rhs;
    cwal_value_unhand(rhs);
  }else{
    cwal_value_unref(rhs);
  }
  cwal_value_unref(lhs);
  cwal_value_unref(self);
  return rc;  
}

/**
   Maintenance reminder:

   s2_op_f_assign3() shares a good deal of logic with this routine,
   but they were separated to improve readability. See that function
   for other notes.
*/
int s2_op_f_assign2( s2_op const * op, s2_engine * se,
                    int argc, cwal_value **rv ){
  int rc = 0;
  cwal_value * rhs, * lhs /* Asignment op: lhs=rhs */;
  s2_stoken * vTok;
  int ttype;
  int opReturnedLhs = -1
    /* optimization. If an operator returns the X part of (X OP Y)
       then we can skip the assignment back to X. A value of <0 means
       we don't yet know. */;
  char gotOverload = 0;
  S2_UNUSED_ARG argc;
  rhs = s2__pop_val(se);
  vTok = s2__pop_tok(se, 1);
  lhs = vTok->value;
  assert( 2==op->arity );
  cwal_value_ref(rhs);
  cwal_value_ref(lhs);
  ttype = vTok->ttype;
  s2_stoken_free(se, vTok, 1);
  vTok = 0;
  if(se->skipLevel>0){
    *rv = cwal_value_undefined();
    cwal_value_unref(rhs);
    cwal_value_unref(lhs);
    return 0;
  }
  if(S2_T_Identifier != ttype){
    cwal_value_unref(rhs);
    cwal_value_unref(lhs);
    return s2_engine_err_set(se, CWAL_SCR_SYNTAX,
                             "Invalid LHS (%s) for '%s' op.",
                             s2_ttype_cstr(ttype), op->sym);
  }

  if(op->id != S2_T_OpAssign /* a combo assignment, not plain X=Y */){
    /* For combo ops (all but X=Y) we need to resolve the LHS first
       because it's needed for calculating the result. For plain
       assignments we don't need to do this, and we don't allow
       overloading of those, so those can skip all this.
    */
    cwal_value * lhsResolved = 0;
    cwal_value * xrv = 0;
    if( (rc = s2_get_v( se, NULL, lhs, &lhsResolved )) ){
      /* Reminder to self: if we want full X->Y and X#Y support, we
         need to adjust this to use those overloads. The problem is
         that by the time this op is triggered, we no longer know
         which dot-like op triggered the access, as se->dotOp.lhs and
         friends _might_ (depending on stack conditions) have been
         cleared by now.
      */
      assert(!lhsResolved);
      cwal_value_unref(lhs);
      cwal_value_unref(rhs);
      return rc;
    }else if(!lhsResolved){
      cwal_size_t idLen = 0;
      char const * sym = cwal_value_get_cstr(lhs, &idLen);
      rc = s2_engine_err_set(se, CWAL_RC_NOT_FOUND,
                             "'%s' operator cannot resolve "
                             "identifier '%.*s'",
                             op->sym, (int)idLen, sym );
      cwal_value_unref(rhs);
      cwal_value_unref(lhs);
      return rc;
    }
    cwal_value_ref(lhsResolved);
    if(!(gotOverload=s2_op_check_overload(op, se, 1, lhsResolved, rhs, &xrv, &rc))
       && !rc){
      assert(!xrv);
      switch(op->id){
        case S2_T_OpPlusAssign:
          rc = s2_values_addsub( se, 1, lhsResolved, rhs, &xrv);
          break;
        case S2_T_OpMinusAssign:
          rc = s2_values_addsub( se, 0, lhsResolved, rhs, &xrv);
          break;
        case S2_T_OpMultiplyAssign:
          rc = s2_values_multdivmod( se, 1, lhsResolved, rhs, &xrv);
          break;
        case S2_T_OpDivideAssign:
          rc = s2_values_multdivmod( se, -1, lhsResolved, rhs, &xrv);
          break;
        case S2_T_OpModuloAssign:
          rc = s2_values_multdivmod( se, 0, lhsResolved, rhs, &xrv);
          break;
        default:
          rc = s2_values_bitwiseshift( se, op->id, lhsResolved, rhs, &xrv );
          break;
      }/* the combo-assignment operators */
      if(!rc){
        assert(xrv);
        if(opReturnedLhs<0){
          opReturnedLhs = lhsResolved==xrv ? 1 : 0;
        }
        cwal_value_ref(xrv);
        cwal_value_unref(rhs);
        rhs = xrv;
      }
    }else if(gotOverload){
      if(rc){
        assert(!xrv);
      }else{
        assert(xrv);
        if(opReturnedLhs<0){
          opReturnedLhs = lhsResolved==xrv ? 1 : 0;
        }
        cwal_value_ref(xrv);
        cwal_value_unref(rhs);
        rhs = xrv;
        xrv = 0;
      }
    }
    cwal_value_unref(lhsResolved);
    lhsResolved = 0;
  }/* combo assignments */
  if(!rc){
    assert(lhs);
    assert(rhs);
    /* Initial thought was that we do not want to assign back over
       _overridden_ assignments. e.g.

       obj += 3

       must not re-assign the result over obj, as doing so
       results in non-intuitive results.

       Turns out that breaks:

       var a = "a";
       a += "b";
       
       which we would expect to have the value 'ab'
       
       Reminder to self:

       holding/unhand'ing the lhs reference can lead to
       values not being cleaned up when overwritten.
       e.g.:

       for( var x = 0; x < 1000; ++x ){}

       If we ref/unhand lhs then each x which gets overwritten gets
       unhand()ed here, and thus not cleaned up until sweep-up.
    */
#if 0
    if(opReturnedLhs>0){
      MARKER(("%s opReturnedLhs==%d, so skipping assignment.\n", op->sym,
              opReturnedLhs));
    }
#endif
    opReturnedLhs = 0
      /*
        Though this optimization works, we arguably can't use it
        because it allows constructs which, on the surface, *appear*
        to violate costness (for an observer who doesn't understand
        this optimization):

        const a = 5;
        // a += 1; // disallowed: const violation.
        a += 0; // allowed by this optimization.

        That op returns 5, which is (or may be) a built-in constant,
        thus triggering opReturnedLhs. That construct must, however,
        for appearance's/consistency's sake, trigger a const violation
        error.

        Similarly, assume that array.operator+= works like
        array.push() but returns, as is required by operator+=,
        'this':

        const a = [1,2,3];
        a += 4; // allowed by this optimization

        That "should", at least for appearances sake, trigger a const
        violation error. With this optimization, however, it would be
        allowed.

        Whether or not "appearances" is a reason to disallow this
        optimization is up for reconsideration.
       */;
    rc = opReturnedLhs>0
      ? 0
      : s2_handle_set_result(se, 0, s2_set_v(se, NULL, lhs, rhs))
      /* Reminder: for full X#Y support this needs to be
         different. */
      ;
  }
  if(rc) cwal_value_unref(rhs);
  else{
    cwal_value_unhand(rhs);
    *rv = rhs;
  }
  cwal_value_unref(lhs);
  return rc;  
}

int s2_op_f_incrdecr( s2_op const * op, s2_engine * se,
                      int argc, cwal_value **rv ){
  int rc = 0;
  cwal_value * self /* target of X.Y incr/decr*/,
    * key /* the identifier or self[propertyKey] of X resp X.Y */;
  int identType /* token type of the operator ID, for sanity
                   checking. */;
  int direction = 0 /* -1 for prefix incr/decr, +1 for postfix */;
  cwal_value * vResolved = 0 /* Pre-op resolved value of X.Y */
    , * vResult = 0 /* Post-op result */;
  cwal_int_t addVal = 0 /* value to incr/decr by: -1 or +1 */;
  char gotOverLoad = 0
    /* true if we found an overload. Only used for sanity checking. */;
  char resolvedIsContainer = 0
    /* true if vResolved is a container type.
       Used only for sanity checks. */;
  s2_stoken * vTok = 0;
  assert(1==op->arity);
  vTok = s2__pop_tok(se, 1);
  self = se->dotOp.lhs /* LHS of X.Y */;
  key = self
    ? se->dotOp.key /* RHS (property key) of X.Y */
    : vTok->value /* identifier X */;
  identType = vTok->ttype;
  cwal_value_ref(key);
  cwal_value_ref(self);
  if(key != vTok->value){
    /* vTok->value was (IIRC!) the result of an X.Y lookup. We don't
       need that result, only the property key. */
    cwal_refunref(vTok->value);
    vTok->value = 0;
  }
  s2_stoken_free(se, vTok, 1);
  /* s2_dump_val(key, "key"); */
  /* s2_dump_val(self, "self"); */
  s2_dotop_state( se, 0, 0, 0 );
  if(se->skipLevel>0){
    cwal_value_unref(key);
    cwal_value_unref(self);
    *rv = cwal_value_undefined();
    return 0;
  }

  switch(op->id){
    case S2_T_OpIncrPre: addVal = 1; direction = -1; break;
    case S2_T_OpDecrPre: addVal = -1; direction = -1; break;
    case S2_T_OpIncrPost: addVal = 1; direction = 1; break;
    case S2_T_OpDecrPost: addVal = -1; direction = 1; break;
    default:
      if(argc){/*avoid unused param warning*/}
      assert(!"invalid op mapping");
      s2_fatal(CWAL_RC_FATAL,"Invalid op mapping for incr/decr: %s",
               op->sym) /* doesn't return */;
  }
  assert(direction==1 || direction==-1);
  assert(addVal==1 || addVal==-1);

  if(!self && (S2_T_Identifier != identType)){
    return s2_engine_err_set(se, CWAL_SCR_SYNTAX,
                             "Invalid %s (%s) for '%s' op.%s",
                             direction>0 ? "LHS" : "RHS",
                             s2_ttype_cstr(identType),
                             op->sym,
                             direction>0 ? "" :
                             " Expecting identifier or object "
                             "property access.");
  }
  else if(self && !cwal_props_can(self)){
    return s2_engine_err_set(se, CWAL_RC_TYPE,
                             "Invalid non-container type LHS (%s) "
                             "for '%s' op.",
                             cwal_value_type_name(self),
                             op->sym);
  }
  assert(self ? !!key : cwal_value_is_string(key)
         /* Expecting the parser to pass the string value of the
            identifier. */);
  if( (rc = s2_get_v( se, self, key, &vResolved )) ) return rc;
  else if(!vResolved){
    /* cannot_resolve: */
    if(!self){
      cwal_size_t idLen = 0;
      char const * sym = cwal_value_get_cstr(key, &idLen);
      assert(idLen && sym && "this _is_ an identifier, right?");
      return s2_engine_err_set(se, CWAL_RC_NOT_FOUND,
                               "'%s' operator cannot resolve "
                               "identifier '%.*s'",
                               op->sym, (int)idLen, sym );
    }else{
      /* Unknown _properties_ always resolve to undefined. */
      vResolved = cwal_value_undefined();
    }
  }
  cwal_value_ref(vResolved);
  /*
    Reminder: refs are most definitely needed in operator chaining,
    e.g. (a = a++) to keep the values from being destroyed before the
    pending LHS op(s) (yes, it happened via (a = ++a)). We do the key,
    too, so that it's safe whether or not string interning is fooling
    the reference count. Likewise, vResult could be an alias for any
    other value which might otherwise be nuked by the assignment.

    Don't forget that operator overloading might change behaviours
    here, causing the key/value/self references to do unpredictable
    things.
  */
  resolvedIsContainer = cwal_props_can(vResolved);
  if(!resolvedIsContainer){
    /* Bypass overload checks for non-container types. */
    rc = s2_values_addsub( se, addVal>0, vResolved,
                           cwal_new_integer(se->e, 1)
                           /* does not allocate */,
                           &vResult);
  }else{
    switch(op->id){
      case S2_T_OpIncrPre:
        if((gotOverLoad = s2_op_check_overload(op, se, 1, 0, vResolved,
                                               &vResult, &rc))
           || rc) break;
        else rc = s2_values_addsub( se, 1, 0, vResolved, &vResult );
        break;
      case S2_T_OpDecrPre:
        if((gotOverLoad = s2_op_check_overload(op, se, 1, 0, vResolved,
                                               &vResult, &rc))
           || rc) break;
        else rc = s2_values_addsub( se, 0, 0, vResolved, &vResult );
        break;
      case S2_T_OpIncrPost:
        if((gotOverLoad = s2_op_check_overload(op, se, 1, vResolved, 0,
                                               &vResult, &rc))
           || rc) break;
        else rc = s2_values_addsub( se, 1, vResolved, 0, &vResult );
        break;
      case S2_T_OpDecrPost:
        if((gotOverLoad = s2_op_check_overload(op, se, 1, vResolved, 0,
                                               &vResult, &rc))
           || rc) break;
        else rc = s2_values_addsub( se, 0, vResolved, 0, &vResult );
        break;
    }
  }
  while(!rc){
    assert(vResult);
    cwal_value_ref(vResult);
#if 0
    s2_dump_val(self,"self");
    s2_dump_val(key,"key");
    s2_dump_val(vResolved,"vResolved");
    s2_dump_val(vResult,"vResult");
#endif
    if(!gotOverLoad && resolvedIsContainer){
      /* For sanity's sake. */
      assert(!"This should have been caught by s2_op_check_overload().");
      assert(!vResult);
      rc = s2_engine_err_set(se, CWAL_RC_TYPE,
                             "Container type '%s' has no '%s' operator.",
                             cwal_value_type_name(vResolved),
                             op->sym);
      break;
    }
    if(1){
      /* don't assign back over _overridden_ ++/-- because the results
         are unintuitive and normally quite useless.
                            
         Turns out that breaks other stuff, so we always assign back
         over the original.
      */
      rc = s2_handle_set_result(se, 0, s2_set_v(se, self, key, vResult));
    }
    if(!rc){
      switch(op->id){
        case S2_T_OpIncrPre:
        case S2_T_OpDecrPre:
          /* Prefix result = the after-evaluation value. */
          *rv = vResult ? vResult : cwal_value_undefined();
          break;
        case S2_T_OpIncrPost:
        case S2_T_OpDecrPost:
          /* Postfix result = the before-evaluation value. */
          *rv = vResolved ? vResolved : cwal_value_undefined();
          break;
      }
    }
    break;
  }
  /* Let go of our references... */
  /* 
     This snippet helps free up the old value of ++X immediately, but
     it's not yet clear if what we're doing here is really kosher
     vis-a-vis lifetimes. Seems to be.

     The difference can be seen by starting s2sh with the -W flag
     and running a simple loop:

     for(var i = 0; i < 100; ++i );
     
     With this block enabled, that doesn't sweep/vacuum at all because
     the unref() here cleans up quickly, whereas the unhand() delays
     it until the next sweep (necessary for getting X++ results back
     properly).

     We have a related sweeping problem, possibly not solvable
     at this level:

     var a = [];
     for(a.0 = 100; a.0 > 0; a.0-- );

     that doesn't sweep any of the reset values until after the loop
     because they're owned by the array's scope. Run that through
     (s2sh -W). This problem does not show up with the prefix ops
     because those unref() instead of unhand() in that case. We
     resolve that in the for() loop handling using a simple ref/unref
     trick which can nuke values living in older scopes.

     The for/while/do-while loops take care of that themselves (now
     they do, anyway), but we'll leave this in here for the sake of
     for-each callback functions which behave similarly, modifying
     lower-scope values which we can potentially clean up.

     Nonetheless, we still have a problem in loop bodies:

     var a = [100];
     do a.0--; while(a.0)

     Does not free up a.0's previous values until a's owning scope is
     swept because (a.0--) returns a newly-made-temporary value (the
     old one) which is owned by a's scope. After the loop:

     MARKER: s2.c:264:s2_engine_sweep():	Swept up 99 value(s) in sweep mode

     Conversely:

     s2> a.0 = 100; do ; while(a.0--)

     Extra handling in for/do/while() can get rid of the temp created
     in the condition, cleaning up those temps as it goes, but only if
     they are on they are the result in the condition part. If they
     are on the LHS of the condition part, they are still orphaned in
     the parent scope until it returns.

     If we handled refcounts at the stack machine (or eval_impl()
     loop) level this might not be so much of a problem.
  */
#if 0
  s2_dump_val(self,"self");
  s2_dump_val(key,"key");
  s2_dump_val(vResolved,"vResolved");
  s2_dump_val(vResult,"vResult");
#endif
  if(!rc){
    cwal_value_ref(*rv);
    assert(*rv == vResolved || *rv == vResult);
  }
  cwal_value_unref(vResolved);
  cwal_value_unref(vResult);
  cwal_value_unref(key);
  cwal_value_unref(self);
  if(!rc) cwal_value_unhand(*rv);
  /*
    Reminder to self: our extra refs/unhand here cause for-loops
    to require extra sweepups:

    for( var x = 0; x < 2000; ++x);

    each assignment back over x doesn't get cleaned up immediately
    because of our unhand(). One can see this in s2sh s2.dumpMetrics()
    and the above loop. Compare the metrics of multiple runs against
    multiple runs of:

    for( var x = 0; x < 2000; x=x+1);

    which currently does not exhibit this behaviour because it doesn't
    have the same ref requirements.

    The for() loop, comma operator, and a few other places account for
    that by doing extra cleanup.
  */
  return rc;
}

int s2_op_f_array_append( s2_op const * op, s2_engine * se,
                          int argc, cwal_value **rv ){
  int rc = 0;
  cwal_value * rhs, * lhs;
  cwal_array * ar;
  s2_stoken * vTok;
  assert(2==argc);
  rhs = s2__pop_val(se);
  vTok = s2__pop_tok(se, 1);
  lhs = vTok->value;
  cwal_value_ref(rhs);
  cwal_value_ref(lhs);
  if(se->skipLevel>0){
    cwal_value_unref(rhs);
    cwal_value_unref(lhs);
    *rv = cwal_value_undefined();
  }else if(!(ar = cwal_value_array_part(se->e, lhs))){
    /* s2_dump_val(rhs,"rhs"); */
    /* s2_dump_val(lhs,"lhs"); */
    rc = s2_engine_err_set(se, CWAL_SCR_SYNTAX,
                           "Invalid (non-array) LHS (token type '%s' "
                           "with value type '%s') for '%s' op.",
                           s2_ttype_cstr(vTok->ttype),
                           cwal_value_type_name(lhs),
                           op->sym);
    if(argc){/*avoid unused param warning*/}
  }else{
    assert(lhs);
    assert(ar);
    /* s2_dump_val(rhs,"rhs"); */
    /* s2_dump_val(lhs,"lhs"); */
    rc = cwal_array_append(ar, rhs);
    if(!rc){
      *rv = rhs;
      cwal_value_ref(*rv);
    }
    cwal_value_unref(rhs);
    cwal_value_unref(lhs);
    if(!rc) cwal_value_unhand(*rv);
  }
  s2_stoken_free(se, vTok, 1);
  return rc;
}


int s2_op_f_foo( s2_op const * op, s2_engine * se, int argc,
                 cwal_value **rv ){
  MARKER(("%s operator: argc=%d%s\n", op->sym,
          argc, argc ? " Args stack:" : "" ));
  for( ; argc>0; --argc){
    cwal_value * v = s2__pop_val(se);
    assert(v);
    MARKER(("Arg type: %s\n", cwal_value_type_name(v)));
  }
  *rv = cwal_value_true();
  return 0;
}

int s2_values_addsub( s2_engine * se, char doAdd, cwal_value * lhs,
                       cwal_value * rhs, cwal_value **rv ){
  if(!lhs){
    /* Unary prefix op */
    if(cwal_value_is_string(rhs)/*unfortunate special case*/){
      cwal_size_t slen = 0;
      char const * cstr = cwal_value_get_cstr(rhs, &slen);
      if(!slen) *rv = cwal_new_integer(se->e, 0);
      else{
        cwal_int_t inty = 0;
        if(s2_cstr_parse_int(cstr, (cwal_int_t)slen, &inty)){
          *rv = cwal_new_integer(se->e, doAdd ? inty : -inty);
        }else{
          cwal_double_t dbl = 0.0;
          if(s2_cstr_parse_double(cstr, (cwal_int_t)slen, &dbl)){
            *rv = cwal_new_double(se->e, doAdd ? dbl : -dbl);
          }else{
            *rv = cwal_new_integer(se->e, 0);
          }
        }
      }
    }else if(!cwal_value_is_double(rhs)){
      /* Special case for integer math on integers >48 bits.
         This is not a complete solution.
      */
      cwal_int_t const iR = cwal_value_get_integer(rhs);
      *rv = iR
        ? (doAdd
           ? (cwal_value_is_integer(rhs)
              ? rhs /* already an int, make this a no-op. */
              : cwal_new_integer(se->e, iR) /* coerce to an int */
              )
           : cwal_new_integer(se->e, -iR))
        : cwal_new_integer(se->e, 0)
        ;
    }else{
      const cwal_double_t iR = cwal_value_get_double( rhs );
      *rv = doAdd
        ? (cwal_value_is_double(rhs)
           ? rhs
           : cwal_new_double(se->e, iR))
        : cwal_new_double(se->e, -iR);
    }
  }else{
    /* Binary op */
    /*
      Using double for all math here breaks for large integers (>48(?)
      bits). We need to first determine whether the LHS is an int, and
      use the int type for that, as that can potentially hold larger
      numbers.
    */
    /* Reminder: ("1"+n) is caught via string op overload. */
    if(!cwal_value_is_double(lhs)){
      cwal_uint_t const iL = (cwal_uint_t)cwal_value_get_integer(lhs);
      cwal_uint_t const iR = (cwal_uint_t)cwal_value_get_integer(rhs);
      *rv = cwal_new_integer(se->e, doAdd ? (cwal_int_t)(iL+iR) : (cwal_int_t)(iL-iR));
    }else{
      cwal_double_t const iL = cwal_value_get_double( lhs );
      /* FIXME: check if RHS is an integer, and do no to-double
         conversion if it is. Needed for sane(?)  behaviour of
         integers larger than the integer part of a double
      */
      cwal_double_t const iR = cwal_value_get_double( rhs );
      cwal_double_t const res = doAdd ? (iL+iR) : (iL-iR);
      *rv = cwal_new_double(se->e, res);
    }
  }
  return *rv ? 0 : CWAL_RC_OOM;
}

/**
   proxy for s2_op_f_assign2/3() for *, /, % operators.

   mode: -1 = division, 0 = modulo, 1 = multiplication
*/
int s2_values_multdivmod( s2_engine * se, int mode,
                          cwal_value * lhs, cwal_value * rhs,
                          cwal_value **rv ){
  switch(mode){
    case -1: {
      if(!cwal_value_is_double(lhs)){
        /* Integer math */
        cwal_int_t const iL = cwal_value_get_integer(lhs);
        /* We need to special-case the RHS in case it is an integer with more
           than 48 bits. */
        if(!cwal_value_is_double(rhs)){
          cwal_int_t const iR = cwal_value_get_integer( rhs );
          if(0==iR) return s2_engine_err_set(se, CWAL_SCR_DIV_BY_ZERO,
                                             "Divide by 0.");
          *rv = cwal_new_integer(se->e, iL / iR);
        }else{
          cwal_double_t const iR = cwal_value_get_double( rhs );
          if(0.0==iR) return s2_engine_err_set(se, CWAL_SCR_DIV_BY_ZERO,
                                               "Divide by 0.0.");
          *rv = cwal_new_integer(se->e, (cwal_int_t)(iL / iR));
        }
      }else{
        cwal_double_t const iL = cwal_value_get_double( lhs );
        if(!cwal_value_is_double(rhs)){
          cwal_int_t const iR = cwal_value_get_integer( rhs );
          if(0==iR) return s2_engine_err_set(se, CWAL_SCR_DIV_BY_ZERO,
                                             "Divide by 0.");
          *rv = cwal_new_double(se->e, iL / iR);
        }else{
          cwal_double_t const iR = cwal_value_get_double( rhs );
          if(0.0==iR) return s2_engine_err_set(se, CWAL_SCR_DIV_BY_ZERO,
                                               "Divide by 0.0.");
          *rv = cwal_new_double(se->e, iL / iR);
        }
      }
      break;
    }
    case 0: {
      /* Modulo */
      cwal_int_t const iR = cwal_value_get_integer( rhs );
      if(0==iR){
        return s2_engine_err_set(se, CWAL_SCR_DIV_BY_ZERO,
                                 "Modulo by 0.");
      }else{
        cwal_int_t const iL = cwal_value_get_integer( lhs );
        /* Optimization: if 1==lhs && 1!=rhs, return lhs.  If the rhs is
           1 it is already optimized b/c cwal doesn't allocate for
           values (-1,0,1).
        */
        *rv = ((1==iL) && (iR!=1))
          ? (cwal_value_is_integer(lhs)
             ? lhs
             : cwal_new_integer(se->e, iL))
          : cwal_new_integer(se->e, (cwal_int_t)(iL % iR));
      }
      break;
    }
    case 1:
      /* Multiplication */
      if(!cwal_value_is_double(lhs)){
        /* Integer math */
        cwal_uint_t const iL = (cwal_uint_t)cwal_value_get_integer(lhs);
        if(!cwal_value_is_double(rhs)){
          cwal_uint_t const iR = (cwal_uint_t)cwal_value_get_integer( rhs );
          *rv = cwal_new_integer(se->e, (cwal_int_t)(iL * iR));
        }else{
          cwal_double_t const iR = cwal_value_get_double( rhs );
          *rv = cwal_new_integer(se->e, (cwal_int_t)(iL * iR));
        }
      }else{
        cwal_double_t const iL = cwal_value_get_double( lhs );
        if(!cwal_value_is_double(rhs)){
          cwal_int_t const iR = cwal_value_get_integer( rhs );
          *rv = cwal_new_double(se->e, iL * iR);
        }else{
          cwal_double_t const iR = cwal_value_get_double( rhs );
          *rv = cwal_new_double(se->e, iL * iR);
        }
      }
      break;
    default:{
      const char * msg =
        "Internal misuse: s2_values_multdivmod() expecting mode of -1, 0, or 1";
      assert(!msg);
      s2_fatal(CWAL_RC_ASSERT, msg) /* does not return */;
    }
  }
  return *rv ? 0 : CWAL_RC_OOM;
}


int s2_values_bitwiseshift( s2_engine * se, int op, cwal_value * vlhs,
                            cwal_value * vrhs, cwal_value ** rv ){
  cwal_int_t lhs = 0, rhs = 0, res = 0;
  lhs = vlhs ? cwal_value_get_integer(vlhs) : 0;
  rhs = cwal_value_get_integer(vrhs);
  switch( op ){
    case S2_T_OpNegateBitwise:
      assert(!vlhs);
      res = ~rhs;
      break;
    case S2_T_OpShiftLeft:
    case S2_T_OpShiftLeftAssign:
    case S2_T_OpShiftLeftAssign3:
      res = (cwal_int_t)((cwal_uint_t)lhs << (cwal_uint_t)rhs); break;
    case S2_T_OpShiftRight:
    case S2_T_OpShiftRightAssign:
    case S2_T_OpShiftRightAssign3:
      res = (cwal_int_t)((cwal_uint_t)lhs >> (cwal_uint_t)rhs); break;
    case S2_T_OpOrBitwise:
    case S2_T_OpOrAssign:
    case S2_T_OpOrAssign3:
      res = lhs | rhs; break;
    case S2_T_OpAndBitwise:
    case S2_T_OpAndAssign:
    case S2_T_OpAndAssign3:
      res = lhs & rhs; break;
    case S2_T_OpXOr:
    case S2_T_OpXOrAssign:
    case S2_T_OpXOrAssign3:
      res = lhs ^ rhs; break;
    default:
      return s2_engine_err_set(se, CWAL_RC_RANGE,
                               "Invalid operator for "
                               "s2_values_bitwiseshift(): %s",
                               s2_ttype_cstr(op));
  }
  /*
    Optimizations: re-use the LHS or RHS if the result equals one of
    them...
  */
  if(res==rhs && cwal_value_is_integer(vrhs)) *rv = vrhs;
  else if(vlhs && (res==lhs) && cwal_value_is_integer(vlhs)) *rv = vlhs;
  else *rv = cwal_new_integer(se->e, res);
  return *rv ? 0 : CWAL_RC_OOM;
}

#undef MARKER
#undef s2__pop_val
#undef s2__pop_tok
