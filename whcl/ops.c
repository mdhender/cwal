/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/*
  License: same as cwal. See cwal.h resp. libcwal.h for details.
*/
#include "internal.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include <stdio.h>
#if 1
#define MARKER(pfexpr) if(1) printf("MARKER: %s:%d:\t",__FILE__,__LINE__); if(1) printf pfexpr
#else
#define MARKER if(0) printf
#endif

#define whcl__pop_val whcl__engine_pop_value
#define whcl__pop_tok whcl__engine_pop_token
/** @internal
   Empty-initialized whcl__op structure, intended for
   const-copy initialization. Only used internally.
*/
#define whcl__op_empty_m {                    \
  0/*sym*/, 0/*id*/, 0/*arity*/,              \
  0/*assoc*/, 0/*prec*/,                    \
  0/*placement*/, 0/*call()*/              \
}

/**
   Operator for unary and binary addition and subtraction.

   Required stack (from top to bottom):
   Binary: RHS LHS
   Unary: RHS
*/
static int whcl__op_f_addsub( whcl__op const * const self, whcl_engine * const el,
                              int argc, cwal_value **rv );

/**
   Operator handler for && and || ops, noting that the
   short-circuiting logic is part of whcl__eval_expr()'s main
   expression handling. This handler will be passed the results of
   (possibly) short-circuited logic, with the undefined value in place
   of any short-circuited part.
*/
static int whcl__op_f_andor( whcl__op const * const self, whcl_engine * const el,
                             int argc, cwal_value **rv );

/**
   Operator for multiplication, divisision, and modulus.

   Required stack (from top to bottom): RHS LHS
*/
static int whcl__op_f_multdivmod( whcl__op const * self, whcl_engine * const el,
                                  int argc, cwal_value **rv );
/**
   Operator for comparison ops.

   Required stack (from top to bottom): RHS LHS
*/
static int whcl__op_f_cmp( whcl__op const * self, whcl_engine * const el,
                           int argc, cwal_value **rv );

static int whcl__op_f_not( whcl__op const * self, whcl_engine * const el,
                           int argc, cwal_value **rv );

/**
   Operator for bitwise OR, AND, XOR, ~, and bitshift << and >>.

   Required stack (from top to bottom): RHS LHS
   except for the unary ~ op, which expects 1 operand.
*/
static int whcl__op_f_bitwiseshift( whcl__op const * self, whcl_engine * const el,
                                    int argc, cwal_value **rv );

/**
   Where the global list of operators is stored. When adding new
   membmers, MAKE 100% SURE that:

   a) the entries in the following array line up with the members.

   b) update whcl__ttype_op() to return the new member for the appropriate
   token type(s).
 */
static const struct WHCL__OPS__ {
  const whcl__op add1;
  const whcl__op add2;
  const whcl__op sub1;
  const whcl__op sub2;
  const whcl__op multiply;
  const whcl__op divide;
  const whcl__op modulo;

  const whcl__op bitOr;
  const whcl__op bitXOr;
  const whcl__op bitAnd;
  const whcl__op bitNegate;
  const whcl__op bitShiftLeft;
  const whcl__op bitShiftRight;

  const whcl__op logNot;
  const whcl__op logAnd;
  const whcl__op logOr;
  const whcl__op logOr3;
  const whcl__op elvis;
  /*
    logical && and || are partially handled at the eval level to
    support short-circuiting.
  */
  const whcl__op cmpLT;
  const whcl__op cmpLE;
  const whcl__op cmpEq;
  const whcl__op cmpEqStrict;
  const whcl__op cmpNotEq;
  const whcl__op cmpNotEqStrict;
  const whcl__op cmpGT;
  const whcl__op cmpGE;
  const whcl__op inherits;
  const whcl__op notInherits;
  const whcl__op contains;
  const whcl__op notContains;

  const whcl__op assign;
  const whcl__op assignConst;
  const whcl__op assignMember;
  const whcl__op assignMemberConst;
  const whcl__op assignPlus;
  const whcl__op assignPlus3;
  const whcl__op assignMinus;
  const whcl__op assignMinus3;
  const whcl__op assignMult;
  const whcl__op assignMult3;
  const whcl__op assignDiv;
  const whcl__op assignDiv3;
  const whcl__op assignModulo;
  const whcl__op assignModulo3;
  const whcl__op assignShiftLeft;
  const whcl__op assignShiftLeft3;
  const whcl__op assignShiftRight;
  const whcl__op assignShiftRight3;
  const whcl__op assignXOr;
  const whcl__op assignXOr3;
  const whcl__op assignAnd;
  const whcl__op assignAnd3;
  const whcl__op assignOr;
  const whcl__op assignOr3;

  const whcl__op incrPre;
  const whcl__op incrPost;
  const whcl__op decrPre;
  const whcl__op decrPost;

  const whcl__op parenOpen;
  const whcl__op parenClose;

  const whcl__op comma;
  const whcl__op dot;
  const whcl__op dotDot;
  const whcl__op dotLength;
  const whcl__op dotHash;
  const whcl__op arrow;
  const whcl__op arrow2;
  const whcl__op colon2;
  const whcl__op arrayAppend;

  /**
     We need a ternary whcl__op to support the ternary operator, but it
     is not used by the stack machine itself.
  */
  const whcl__op ternary;

  const whcl__op _sentry_;
} WHCL__OPS = {
/*{ sym,     id,                arity, assoc, prec,      placement, call() } */
  { "+X",    TOK1_T_OpPlusUnary,  1,  1, WHCL__PR_PlusUnary,  -1, whcl__op_f_addsub},
  { "X+Y",   TOK1_T_OpPlus,       2, -1, WHCL__PR_Plus,        0, whcl__op_f_addsub},

  { "-X",    TOK1_T_OpMinusUnary, 1,  1, WHCL__PR_MinusUnary, -1, whcl__op_f_addsub},
  { "X-Y",   TOK1_T_OpMinus,      2, -1, WHCL__PR_Minus,       0, whcl__op_f_addsub},
  { "X*Y",   TOK1_T_OpMultiply,   2, -1, WHCL__PR_Multiply,    0, whcl__op_f_multdivmod},
  { "X/Y",   TOK1_T_OpDivide,     2, -1, WHCL__PR_Divide,      0, whcl__op_f_multdivmod},
  { "X%Y",   TOK1_T_OpModulo,     2, -1, WHCL__PR_Modulo,      0, whcl__op_f_multdivmod},

  { "X|Y",   TOK1_T_OpOrBitwise,  2, -1, WHCL__PR_BitwiseOr,   0, whcl__op_f_bitwiseshift},
  { "X^Y",   TOK1_T_OpXOr,        2, -1, WHCL__PR_BitwiseXor,  0, whcl__op_f_bitwiseshift},
  { "X&Y",   TOK1_T_OpAndBitwise, 2, -1, WHCL__PR_BitwiseAnd,  0, whcl__op_f_bitwiseshift},
  { "~X",    TOK1_T_OpNegateBitwise, 1, 1, WHCL__PR_BitwiseNegate, -1, whcl__op_f_bitwiseshift},
  { "X<<Y",  TOK1_T_OpShiftLeft,  2, -1, WHCL__PR_ShiftLeft,   0, whcl__op_f_bitwiseshift},
  { "X>>Y",  TOK1_T_OpShiftRight, 2, -1, WHCL__PR_ShiftRight,  0, whcl__op_f_bitwiseshift},

  { "!X",    TOK1_T_OpNot,        1,  1, WHCL__PR_LogicalNot, -1, whcl__op_f_not},
  /* &&, ||, and ||| are handled at the parser level for
     short-cicuiting, but the stack machine supports them directly
     without short-circuiting. Why?  It's a stack machine, so the RHS
     has already been processed (or the stack wouldn't be running), so
     short-circuiting evaluation is not possible at that
     point.
  */
  { "X&&Y",  TOK1_T_OpAnd,          2, -1, WHCL__PR_LogicalAnd,  0, whcl__op_f_andor},
  { "X||Y",  TOK1_T_OpOr,           2, -1, WHCL__PR_LogicalOr,   0, whcl__op_f_andor},
  { "X|||Y", TOK1_T_OpOr3,          2, -1, WHCL__PR_LogicalOr3,  0, whcl__op_f_andor},
  { "X?:Y",  TOK1_T_OpElvis,        2, -1, WHCL__PR_LogicalOrElvis, 0, whcl__op_f_andor},

  { "X<Y",   TOK1_T_OpCmpLT,          2, -1, WHCL__PR_CmpLT,       0, whcl__op_f_cmp},
  { "X<=Y",  TOK1_T_OpCmpLE,          2, -1, WHCL__PR_CmpLE,       0, whcl__op_f_cmp},
  { "X==Y",  TOK1_T_OpCmpEq,          2, -1, WHCL__PR_CmpEq,       0, whcl__op_f_cmp},
  { "X===Y", TOK1_T_OpCmpEqStrict,    2, -1, WHCL__PR_CmpEqStrict, 0, NULL},
  { "X!=Y",  TOK1_T_OpCmpNotEq,       2, -1, WHCL__PR_CmpNotEq,    0, whcl__op_f_cmp},

  { "X!==Y", TOK1_T_OpCmpNotEqStrict, 2, -1, WHCL__PR_CmpNotEqStrict,  0, NULL},
  { "X>Y",   TOK1_T_OpCmpGT,          2, -1, WHCL__PR_CmpGT,       0, whcl__op_f_cmp},
  { "X>=Y",  TOK1_T_OpCmpGE,          2, -1, WHCL__PR_CmpGE,       0, whcl__op_f_cmp},
  { "inherits", TOK1_T_OpInherits,  2, -1, WHCL__PR_OpInherits,  0, NULL},
  { "!inherits", TOK1_T_OpNotInherits,  2, -1, WHCL__PR_OpInherits, 0, NULL},
  { "X=~Y", TOK1_T_OpContains,      2, -1, WHCL__PR_Contains,    0, NULL},
  { "X!~Y", TOK1_T_OpNotContains,   2, -1, WHCL__PR_NotContains, 0, NULL},
  { "X=Y",     TOK1_T_OpAssign,       2,  1, WHCL__PR_OpAssign,    0, NULL},
  { "X:=Y",    TOK1_T_OpColonEqual,   2,  1, WHCL__PR_OpAssign,    0, NULL},
  { "X.Y=Z",   TOK1_T_OpAssign3,      3,  1, WHCL__PR_OpAssign,    0, NULL},
    { "X.Y:=Z",  TOK1_T_OpAssignConst3, 3,  1, WHCL__PR_OpAssign,    0, NULL},
  { "X+=Y",    TOK1_T_OpPlusAssign,   2,  1, WHCL__PR_OpAssign,    0, NULL},
  { "X.Y+=Z",  TOK1_T_OpPlusAssign3,  3,  1, WHCL__PR_OpAssign,    0, NULL},
  { "X-=Y",    TOK1_T_OpMinusAssign,  2,  1, WHCL__PR_OpAssign,    0, NULL},
  { "X.Y-=Z",  TOK1_T_OpMinusAssign3,  3,  1, WHCL__PR_OpAssign,    0, NULL},
  { "X*=Y",    TOK1_T_OpMultiplyAssign,  2, 1, WHCL__PR_OpAssign,    0, NULL},
  { "X.Y*=Z",  TOK1_T_OpMultiplyAssign3, 3, 1, WHCL__PR_OpAssign,    0, NULL},
  { "X/=Y",    TOK1_T_OpDivideAssign,    2,  1, WHCL__PR_OpAssign,    0, NULL},
  { "X.Y/=Z",  TOK1_T_OpDivideAssign3,   3, 1, WHCL__PR_OpAssign,    0, NULL},
  { "X%=Y",    TOK1_T_OpModuloAssign,    2,  1, WHCL__PR_OpAssign,    0, NULL},
  { "X.Y%=Z",  TOK1_T_OpModuloAssign3,   3,  1, WHCL__PR_OpAssign,   0, NULL},
  { "X<<=Y",   TOK1_T_OpShiftLeftAssign, 2,  1, WHCL__PR_OpAssign,    0, NULL},
  { "X.Y<<=Z", TOK1_T_OpShiftLeftAssign3, 3,  1, WHCL__PR_OpAssign,    0, NULL},
  { "X>>=Y",   TOK1_T_OpShiftRightAssign, 2, 1, WHCL__PR_OpAssign,    0, NULL},
  { "X.Y>>=Z", TOK1_T_OpShiftRightAssign3, 3,  1, WHCL__PR_OpAssign,    0, NULL},
  { "X^=Y",    TOK1_T_OpXOrAssign,    2, 1, WHCL__PR_OpAssign,    0, NULL},
  { "X.Y^=Z",  TOK1_T_OpXOrAssign3,   3,  1, WHCL__PR_OpAssign,    0, NULL},
  { "X&=Y",    TOK1_T_OpAndAssign,    2,  1, WHCL__PR_OpAssign,    0, NULL},
  { "X.Y&=Z",  TOK1_T_OpAndAssign3,   3,  1, WHCL__PR_OpAssign,    0, NULL},
  { "X|=Y",    TOK1_T_OpOrAssign,     2,  1, WHCL__PR_OpAssign,    0, NULL},
  { "X.Y|=Z",  TOK1_T_OpOrAssign3,    3,  1, WHCL__PR_OpAssign,    0, NULL},
  { "++X", TOK1_T_OpIncrPre,        1,  1, WHCL__PR_IncrDecr,   -1, NULL},
  { "X++", TOK1_T_OpIncrPost,       1, -1, WHCL__PR_IncrDecr,    1, NULL},
  { "--X", TOK1_T_OpDecrPre,        1,  1, WHCL__PR_IncrDecr,   -1, NULL},
  { "X--", TOK1_T_OpDecrPost,       1, -1, WHCL__PR_IncrDecr,    1, NULL},

  { "(",   TOK1_T_ParenOpen,        0, -1, WHCL__PR_ParensOpen, -1, NULL},
  { ")",   TOK1_T_ParenClose,       0, -1, WHCL__PR_ParensClose, 1, NULL},

  { "X,Y",   TOK1_T_Comma,          2, -1, WHCL__PR_Comma,       0, NULL},
  { "X.Y",   TOK1_T_OpDot,          2, -1, WHCL__PR_DotDeref,    0, NULL},
  { "X..",   TOK1_T_OpDotDot,       2, -1, WHCL__PR_DotDot,      0, NULL},
  { "X.#",   TOK1_T_OpDotLength,    1, -1, WHCL__PR_DotDeref,    1, NULL},
  { "X#Y",   TOK1_T_OpHash,         2, -1, WHCL__PR_DotDeref,    0, NULL},
  { "X->Y",  TOK1_T_OpArrow,        2, -1, WHCL__PR_OpArrow,     0, NULL},
  { "X=>Y",  TOK1_T_OpArrow2,       0, -1,  WHCL__PR_OpArrow2,    0, NULL},
  { "X::Y",  TOK1_T_OpColon2,       2, -1,  WHCL__PR_OpColon2,    0, NULL},

  { "X[]=Y", TOK1_T_ArrayAppend,    2,  1, WHCL__PR_ArrayAppend, 0, NULL},

  { "X?Y:Z",  TOK1_T_Question,      3,  1, WHCL__PR_TernaryIf,   0, NULL},

  /*_sentry_*/whcl__op_empty_m
};

whcl__op const * whcl__ttype_op( int ttype ){
  switch( ttype ){
#define OP(E,M) case E: return &WHCL__OPS.M
    OP(TOK1_T_OpPlusUnary,add1);
    OP(TOK1_T_OpPlus,add2);
    OP(TOK1_T_OpMinusUnary,sub1);
    OP(TOK1_T_OpMinus,sub2);

    OP(TOK1_T_OpMultiply,multiply);
    OP(TOK1_T_OpDivide,divide);
    OP(TOK1_T_OpModulo,modulo);

    OP(TOK1_T_OpOrBitwise,bitOr);
    OP(TOK1_T_OpXOr,bitXOr);
    OP(TOK1_T_OpAndBitwise,bitAnd);
    OP(TOK1_T_OpNegateBitwise,bitNegate);
    OP(TOK1_T_OpShiftLeft,bitShiftLeft);
    OP(TOK1_T_OpShiftRight,bitShiftRight);

    OP(TOK1_T_OpNot,logNot);
    OP(TOK1_T_OpAnd,logAnd);
    OP(TOK1_T_OpOr,logOr);
    OP(TOK1_T_OpOr3,logOr3);
    OP(TOK1_T_OpElvis,elvis);

    OP(TOK1_T_OpCmpLT,cmpLT);
    OP(TOK1_T_OpCmpLE,cmpLE);
    OP(TOK1_T_OpCmpEq,cmpEq);
    OP(TOK1_T_OpCmpEqStrict,cmpEqStrict);
    OP(TOK1_T_OpCmpNotEq,cmpNotEq);
    OP(TOK1_T_OpCmpNotEqStrict,cmpNotEqStrict);
    OP(TOK1_T_OpCmpGT,cmpGT);
    OP(TOK1_T_OpCmpGE,cmpGE);
    OP(TOK1_T_OpInherits,inherits);
    OP(TOK1_T_OpNotInherits,notInherits);
    OP(TOK1_T_OpContains,contains);
    OP(TOK1_T_OpNotContains,notContains);

    OP(TOK1_T_OpAssign,assign);
    OP(TOK1_T_OpColonEqual,assignConst);
    OP(TOK1_T_OpAssign3,assignMember);
    OP(TOK1_T_OpAssignConst3,assignMemberConst);
    OP(TOK1_T_OpPlusAssign,assignPlus);
    OP(TOK1_T_OpPlusAssign3,assignPlus3);
    OP(TOK1_T_OpMinusAssign,assignMinus);
    OP(TOK1_T_OpMinusAssign3,assignMinus3);
    OP(TOK1_T_OpMultiplyAssign,assignMult);
    OP(TOK1_T_OpMultiplyAssign3,assignMult3);
    OP(TOK1_T_OpDivideAssign, assignDiv);
    OP(TOK1_T_OpDivideAssign3, assignDiv3);
    OP(TOK1_T_OpModuloAssign, assignModulo);
    OP(TOK1_T_OpModuloAssign3, assignModulo3);
    OP(TOK1_T_OpShiftLeftAssign, assignShiftLeft);
    OP(TOK1_T_OpShiftLeftAssign3, assignShiftLeft3);
    OP(TOK1_T_OpShiftRightAssign, assignShiftRight);
    OP(TOK1_T_OpShiftRightAssign3, assignShiftRight3);
    OP(TOK1_T_OpXOrAssign, assignXOr);
    OP(TOK1_T_OpXOrAssign3, assignXOr3);
    OP(TOK1_T_OpAndAssign, assignAnd);
    OP(TOK1_T_OpAndAssign3, assignAnd3);
    OP(TOK1_T_OpOrAssign, assignOr);
    OP(TOK1_T_OpOrAssign3, assignOr3);

    OP(TOK1_T_OpIncrPre, incrPre);
    OP(TOK1_T_OpIncrPost, incrPost);
    OP(TOK1_T_OpDecrPre, decrPre);
    OP(TOK1_T_OpDecrPost, decrPost);

    OP(TOK1_T_ParenOpen,parenOpen);
    OP(TOK1_T_ParenClose,parenClose);

    OP(TOK1_T_Comma,comma);
    OP(TOK1_T_OpDot,dot);
#if 0
    OP(TOK1_T_OpDotDot,dotDot);
#else
    case TOK1_T_OpDotDot: return 0;
#endif
    OP(TOK1_T_OpDotLength,dotLength);
    OP(TOK1_T_OpHash,dotHash);
    OP(TOK1_T_OpArrow,arrow);
    OP(TOK1_T_OpArrow2,arrow2);
    OP(TOK1_T_OpColon2,colon2);
    OP(TOK1_T_ArrayAppend,arrayAppend);
    OP(TOK1_T_Question,ternary);
#undef OP
    default:
      return NULL;
  }
}

int whcl__ttype_op2(int ttype){
  whcl__op const * const op = whcl__ttype_op(ttype);
  if(op) return op->id;
  switch(ttype){
    case TOK1_T_OpIncr:
    case TOK1_T_OpDecr:
      /* TODO: figure out which ttypes we need here */
      return ttype;
    default:
      return 0;
  }
}

static int whcl__process_op_impl( whcl_engine * const el, whcl__op const * op,
                                  bool popOpStack ){
  int rc = el->flags.interrupted;
  whcl__stoken_stack * const st = &el->estack.vals;
  int const oldStackSize = st->size;
  int popArgCount = 0;
  cwal_value * rv = 0;
  whcl__stoken * topOp = 0;
  assert(op);
  //se->opErrPos = 0;
  if(rc) return rc;
  //else if(se->flags.traceTokenStack){
  //  MARKER(("running operator %s (#%d) arity=%d assoc=%d prec=%d\n",
  //          op->sym, op->id, op->arity, op->assoc, op->prec));
  //}
  /**
    pop op (if needed) first so that we can guaranty operators that
    they are not the top op on the stack when they are called. Useful
    for '=', which will want to know if the LHS is a dot operator or
    not.
  */
  if(popOpStack){
    topOp = whcl__engine_pop_op(el, true);
  }
  if(!op->call){
    rc = whcl_err_set(el, CWAL_RC_UNSUPPORTED,
                      "Operator %s does not have an internal "
                      "call() impl.",
                      op->sym);
  }     
  else if(op->arity>=0){
    assert((st->size >= op->arity)
           || (op->assoc>0 && op->arity==1 /* unary +, -, ~ */));
    if(st->size < op->arity){
      rc = whcl_err_set(el, CWAL_RC_RANGE,
                        "Not enough operands on the stack.");
    }else{
      rc = op->call(op, el, op->arity, &rv);
      popArgCount = op->arity;
    }
  }
  if(!rc){
    assert( st->size == (oldStackSize - popArgCount) );
    if(st->size != (oldStackSize - popArgCount)){
      rc = whcl_err_set(el, CWAL_RC_MISUSE,
                        "Unexpected stack size after "
                        "running operator '%s'\n",
                        op->sym);
    }else if(rv){
      /* Push the result value... */
      whcl__stoken * tResult = topOp ? topOp/*recycle it*/ : NULL;
      topOp = NULL;
      if(!tResult){
        tResult = whcl__stoken_alloc(el);
        if(!tResult) rc = CWAL_RC_OOM;
      }else{
        *tResult = whcl__stoken_empty;
      }
      if(tResult){
        tResult->ttype = rv ? TOK1_T_Value : TOK1_T_Undefined;
        tResult->value = rv ? rv : cwal_value_undefined();
        whcl__engine_push_valtok(el, tResult);
      }else{
        if(rv) cwal_refunref(rv);
      }
    }
  }else{
    assert(!rv);
  }
  if(topOp){
    whcl__stoken_free(el, topOp, true);
  }
  return whcl__check_interrupted(el, rc);
}

int whcl__process_top( whcl_engine * const el ){
  whcl__stoken_stack * const so = &el->estack.ops;
  whcl__op const * op = whcl__stoken_op(so->top);
  int rc = whcl__check_interrupted(el, 0);
  if(rc) return rc;
  whcl__stoken pos = *so->top;
#define errpos      \
  if(pos.srcPos.ct) \
    whcl__script_errtoken_set(pos.srcPos.ct, pos.srcPos.tok)

  if(!op){
    errpos;
    rc = whcl_err_set(el, CWAL_SCR_SYNTAX,
                      "Token type %s (#%d) is not an operator.",
                      so->top ? tok1_t_cstr(so->top->ttype) : "<NULL>",
                      so->top ? (int)so->top->ttype : TOK1_T_INVALID);
  }else if(op
           && op->arity>0
           && el->estack.vals.size<op->arity){
    errpos;
    rc = whcl_err_set(el, CWAL_SCR_SYNTAX,
                      "Value stack does not have enough values for "
                      "operator '%s'.", op->sym);
  }else{
    rc = whcl__process_op_impl( el, op, true );
    if(rc){ errpos; }
  }
#undef errpos
  return rc;
}

whcl__op const * whcl__stoken_op(whcl__stoken const * const st){
  return whcl__ttype_op(st->ttype);
}

int whcl__values_addsub( whcl_engine * const el, bool doAdd, cwal_value * const lhs,
                         cwal_value * const rhs, cwal_value **rv ){
  if(!lhs){
    /* Unary prefix op */
    if(cwal_value_is_string(rhs)/*unfortunate special case*/){
      cwal_size_t slen = 0;
      char const * cstr = cwal_value_get_cstr(rhs, &slen);
      if(!slen) *rv = cwal_new_integer(el->ec, 0);
      else{
        cwal_int_t inty = 0;
        if(0==cwal_cstr_to_int(cstr, (cwal_int_t)slen, &inty)){
          *rv = cwal_new_integer(el->ec, doAdd ? inty : -inty);
        }else{
          cwal_double_t dbl = 0.0;
          if(0==cwal_cstr_to_double(cstr, (cwal_int_t)slen, &dbl)){
            *rv = cwal_new_double(el->ec, doAdd ? dbl : -dbl);
          }else{
            *rv = cwal_new_integer(el->ec, 0);
          }
        }
      }
    }else if(cwal_value_is_double(rhs)){
      *rv = doAdd
        ? rhs
        : cwal_new_double(el->ec, -cwal_value_get_double( rhs ));
    }else{
      /* Special case for integer math on integers >48 bits.
         This is not a complete solution. */
      cwal_int_t const iR = cwal_value_get_integer(rhs);
      *rv = iR
        ? (doAdd
           ? (cwal_value_is_integer(rhs)
              ? rhs /* already an int, make this a no-op. */
              : cwal_new_integer(el->ec, iR) /* coerce to an int */
              )
           : cwal_new_integer(el->ec, -iR))
        : cwal_new_integer(el->ec, 0)
        ;
    }
  }else{
    /* Binary op */
    if(cwal_value_is_double(lhs) || cwal_value_is_double(rhs)){
      cwal_double_t const iL = cwal_value_get_double( lhs );
      cwal_double_t const iR = cwal_value_get_double( rhs );
      cwal_double_t const res = doAdd ? (iL+iR) : (iL-iR);
      *rv = cwal_new_double(el->ec, res);
    }else{
      cwal_uint_t const iL = (cwal_uint_t)cwal_value_get_integer(lhs);
      cwal_uint_t const iR = (cwal_uint_t)cwal_value_get_integer(rhs);
      *rv = cwal_new_integer(el->ec, doAdd ? (cwal_int_t)(iL+iR) : (cwal_int_t)(iL-iR));
    }
  }
  return *rv ? 0 : CWAL_RC_OOM;
}

int whcl__op_f_addsub( whcl__op const * const op, whcl_engine * const el,
                       int argc, cwal_value **rv ){
  cwal_value * rhs = 0;
  cwal_value * lhs = 0;
  bool isAdd = 0;
  switch(op->id){
    case TOK1_T_OpPlus:
      isAdd = true;
      CWAL_SWITCH_FALL_THROUGH;
    case TOK1_T_OpMinus:
      assert( 2 == argc );
      rhs = whcl__pop_val( el );
      lhs = whcl__pop_val( el );
      break;
    case TOK1_T_OpPlusUnary:
      isAdd = 1;
      CWAL_SWITCH_FALL_THROUGH;
    case TOK1_T_OpMinusUnary:
      assert( 1 == argc );
      rhs = whcl__pop_val( el );
      lhs = 0;
      break;
    default:
      assert(!"WRONG!");
      if(argc){/*avoid unused param warning*/}
      whcl__fatal(CWAL_RC_ASSERT, "Serious internal API misuse.");
  }
  if(el->skipLevel>0){
    *rv = cwal_value_undefined();
    return 0;
  }
  int rc = 0;
  cwal_ref(lhs);
  cwal_ref(rhs);
  rc = whcl__values_addsub( el, isAdd, lhs, rhs, rv);
  if(!rc) cwal_ref(*rv);
  cwal_unref(lhs);
  cwal_unref(rhs);
  if(!rc) cwal_unhand(*rv);
  return rc;
}

/**
   proxy for whcl_op_f_...() for *, /, % operators.

   mode: -1 = division, 0 = modulo, 1 = multiplication
*/
static int whcl__values_multdivmod( whcl_engine * const el, int mode,
                                    cwal_value * const lhs, cwal_value * const rhs,
                                    cwal_value **rv ){
  assert(mode>-2 && mode<2);
  switch(mode){
    case -1: {
      if(cwal_value_is_double(lhs) || cwal_value_is_double(rhs)){
        cwal_double_t const iL = cwal_value_get_double( lhs );
        if(!cwal_value_is_double(rhs)){
          cwal_int_t const iR = cwal_value_get_integer( rhs );
          if(0==iR) return whcl_err_set(el, CWAL_SCR_DIV_BY_ZERO,
                                        "Divide by 0.");
          *rv = cwal_new_double(el->ec, iL / iR);
        }else{
          cwal_double_t const iR = cwal_value_get_double( rhs );
          if(0.0==iR) return whcl_err_set(el, CWAL_SCR_DIV_BY_ZERO,
                                          "Divide by 0.0.");
          *rv = cwal_new_double(el->ec, iL / iR);
        }
      }else{
        /* Integer math */
        cwal_int_t const iL = cwal_value_get_integer(lhs);
        /* We need to special-case the RHS in case it is an integer with more
           than 48 bits. */
        if(!cwal_value_is_double(rhs)){
          cwal_int_t const iR = cwal_value_get_integer( rhs );
          if(0==iR) return whcl_err_set(el, CWAL_SCR_DIV_BY_ZERO,
                                             "Divide by 0.");
          *rv = cwal_new_integer(el->ec, iL / iR);
        }else{
          cwal_double_t const iR = cwal_value_get_double( rhs );
          if(0.0==iR) return whcl_err_set(el, CWAL_SCR_DIV_BY_ZERO,
                                          "Divide by 0.0.");
          *rv = cwal_new_integer(el->ec, (cwal_int_t)(iL / iR));
        }
      }
      break;
    }
    case 0: {
      /* Modulo: always integer */
      cwal_int_t const iR = cwal_value_get_integer( rhs );
      if(0==iR){
        return whcl_err_set(el, CWAL_SCR_DIV_BY_ZERO, "Modulo by 0.");
      }else{
        cwal_int_t const iL = cwal_value_get_integer( lhs );
        /* Optimization: if 1==lhs && 1!=rhs, return lhs.  If the rhs is
           1 it is already optimized b/c cwal doesn't allocate for
           values (-1,0,1). */
        *rv = ((1==iL) && (iR!=1))
          ? (cwal_value_is_integer(lhs)
             ? lhs
             : cwal_new_integer(el->ec, iL))
          : cwal_new_integer(el->ec, (cwal_int_t)(iL % iR));
      }
      break;
    }
    case 1:
      /* Multiplication */
      if(cwal_value_is_double(lhs) || cwal_value_is_double(rhs)){
        /* Floating point */
        cwal_double_t const iL = cwal_value_get_double( lhs );
        if(cwal_value_is_double(rhs)){
          cwal_double_t const iR = cwal_value_get_double( rhs );
          *rv = cwal_new_double(el->ec, iL * iR);
        }else{
          cwal_int_t const iR = cwal_value_get_integer( rhs );
          *rv = cwal_new_double(el->ec, iL * iR);
        }
      }else{
        /* Integer math */
        cwal_uint_t const iL = (cwal_uint_t)cwal_value_get_integer(lhs);
        if(cwal_value_is_double(rhs)){
          cwal_double_t const iR = cwal_value_get_double( rhs );
          *rv = cwal_new_integer(el->ec, (cwal_int_t)(iL * iR));
        }else{
          cwal_uint_t const iR = (cwal_uint_t)cwal_value_get_integer( rhs );
          *rv = cwal_new_integer(el->ec, (cwal_int_t)(iL * iR));
        }
      }
      break;
    default:{
      const char * msg =
        "Internal misuse: whcl__values_multdivmod() expecting mode of -1, 0, or 1";
      assert(!msg);
      whcl__fatal(CWAL_RC_ASSERT, msg) /* does not return */;
    }
  }
  return *rv ? 0 : CWAL_RC_OOM;
}

int whcl__op_f_multdivmod( whcl__op const * op, whcl_engine * const el,
                           int argc, cwal_value **rv ){
  cwal_value * rhs;
  cwal_value * lhs;
  int mode;
  assert( 2 == argc );
  rhs = whcl__pop_val( el );
  lhs = whcl__pop_val( el );
  switch( op->id ){
    case TOK1_T_OpMultiply: mode = 1; break;
    case TOK1_T_OpDivide: mode = -1; break;
    case TOK1_T_OpModulo: mode = 0; break;
    default:
      assert(!"Invalid op binding!");
      if(argc){/*avoid unused param warning*/}
      whcl__fatal(CWAL_RC_ASSERT,
                  "Invalid operator binding in %s().", __func__);
  }
  cwal_ref(lhs);
  cwal_ref(rhs);
  if(el->skipLevel>0){
    *rv = cwal_value_undefined();
    cwal_unref(rhs);
    cwal_unref(lhs);
    return 0;
  }else{
    int const rc = whcl__values_multdivmod( el, mode, lhs, rhs, rv);
    if(!rc) cwal_ref(*rv);
    cwal_unref(rhs);
    cwal_unref(lhs);
    if(!rc) cwal_unhand(*rv);
    return rc;
  }
}


int whcl__op_f_cmp( whcl__op const * op, whcl_engine * const el,
                    int argc, cwal_value **rv ){
  cwal_value * rhs;
  cwal_value * lhs;
  int rc = 0;
  assert(2==argc);
  rhs = whcl__pop_val(el);
  lhs = whcl__pop_val(el);
  cwal_ref(rhs);
  cwal_ref(lhs);
  if(el->skipLevel>0){
    cwal_unref(rhs);
    cwal_unref(lhs);
    *rv = cwal_value_undefined();
    return 0;
  }
  switch(op->id){
    case TOK1_T_OpNotInherits:
      rc = !cwal_value_derives_from( el->ec, lhs, rhs );
      break;
    case TOK1_T_OpInherits:
      rc = cwal_value_derives_from( el->ec, lhs, rhs );
      break;
    case TOK1_T_OpCmpEq:
    case TOK1_T_OpCmpNotEq:
    /*case TOK1_T_OpCmpEqStrict:*/
    /*case TOK1_T_OpCmpNotEqStrict:*/{
      if(rhs == lhs){
        rc = 0;
      }else if(cwal_value_is_number(lhs)
               && cwal_value_is_number(rhs)){
        /* Special case: though we're generally doing type-strict comparisons,
           we have to special-case the number-vs-number comparison so that
           1.0==1 will compare sensibly. Note that this also includes booleans
           and that `true` will compare equivalent to ANY non-0 value.*/
        rc = cwal_value_compare(lhs, rhs);
      }else{
        int const tidL = cwal_value_type_id(lhs);
        int const tidR = cwal_value_type_id(rhs);
        if(tidL == tidR){
          rc = cwal_value_compare(lhs, rhs);
        }else{
          rc = tidL - tidR;
        }
      }
      rc = (TOK1_T_OpCmpNotEq==op->id)
        ? (0!=rc)
        : (0==rc);
      break;
    }
    default:{
      rc = cwal_value_compare(lhs, rhs);
      switch(op->id){
        case TOK1_T_OpCmpLT: rc = rc < 0; break;
        case TOK1_T_OpCmpLE: rc = rc <= 0; break;
        case TOK1_T_OpCmpGT: rc = rc > 0; break;
        case TOK1_T_OpCmpGE: rc = rc >= 0; break;
        default:
          assert(!"CANNOT HAPPEN");
          if(argc){/*avoid unused param warning*/}
          cwal_value_unref(rhs);
          cwal_value_unref(lhs);
          whcl__fatal(CWAL_RC_CANNOT_HAPPEN,
                      "Invalid operator mapping for '%s'.", op->sym);
      }
    }
  }
  *rv = rc ? cwal_value_true() : cwal_value_false();
  cwal_value_unref(rhs);
  cwal_value_unref(lhs);
  assert(cwal_value_is_builtin(*rv));
  return 0;
}

int whcl__op_f_not( whcl__op const * op, whcl_engine * const el,
                    int argc, cwal_value **rv ){
  cwal_value * rhs;
  if(op || argc){/*avoid unused param warning*/}
  assert(1==argc);
  rhs = whcl__pop_val(el);
  cwal_ref(rhs);
  *rv = (el->skipLevel>0)
    ? cwal_value_undefined()
    : cwal_new_bool( !cwal_value_get_bool(rhs) );
  assert(cwal_value_is_builtin(*rv));
  cwal_unref(rhs);
  return 0;
}

int whcl__values_bitwiseshift( whcl_engine * const el, int op,
                               cwal_value * const vlhs,
                               cwal_value * const vrhs,
                               cwal_value ** rv ){
  cwal_int_t lhs = 0, rhs = 0, res = 0;
  lhs = vlhs ? cwal_value_get_integer(vlhs) : 0;
  rhs = cwal_value_get_integer(vrhs);
  switch( op ){
    case TOK1_T_OpNegateBitwise:
      assert(!vlhs);
      res = ~rhs;
      break;
    case TOK1_T_OpShiftLeft:
    case TOK1_T_OpShiftLeftAssign:
    case TOK1_T_OpShiftLeftAssign3:
      res = (cwal_int_t)((cwal_uint_t)lhs << (cwal_uint_t)rhs); break;
    case TOK1_T_OpShiftRight:
    case TOK1_T_OpShiftRightAssign:
    case TOK1_T_OpShiftRightAssign3:
      res = (cwal_int_t)((cwal_uint_t)lhs >> (cwal_uint_t)rhs); break;
    case TOK1_T_OpOrBitwise:
    case TOK1_T_OpOrAssign:
    case TOK1_T_OpOrAssign3:
      res = lhs | rhs; break;
    case TOK1_T_OpAndBitwise:
    case TOK1_T_OpAndAssign:
    case TOK1_T_OpAndAssign3:
      res = lhs & rhs; break;
    case TOK1_T_OpXOr:
    case TOK1_T_OpXOrAssign:
    case TOK1_T_OpXOrAssign3:
      res = lhs ^ rhs; break;
    default:
      return whcl_err_set(el, CWAL_RC_RANGE,
                          "Invalid operator for "
                          "whcl__values_bitwiseshift(): %s",
                          tok1_t_cstr(op));
  }
  /*
    Optimizations: re-use the LHS or RHS if the result equals one of
    them...
  */
  if(res==rhs && cwal_value_is_integer(vrhs)) *rv = vrhs;
  else if(vlhs && (res==lhs) && cwal_value_is_integer(vlhs)) *rv = vlhs;
  else *rv = cwal_new_integer(el->ec, res);
  return *rv ? 0 : CWAL_RC_OOM;
}


int whcl__op_f_bitwiseshift( whcl__op const * op, whcl_engine * const el,
                             int argc, cwal_value **rv ){
  cwal_value * vlhs = 0, * vrhs = 0;
  assert((TOK1_T_OpNegateBitwise==op->id) ? (1==argc) : (2==argc));
  vrhs = whcl__pop_val(el);
  assert(vrhs);
  if(TOK1_T_OpNegateBitwise != op->id){
    vlhs = whcl__pop_val(el);
    assert(vlhs);
    cwal_ref(vlhs);
    if(argc){/*avoid unused param warning*/}
  }
  cwal_ref(vrhs);
  if(el->skipLevel>0){
    *rv = cwal_value_undefined();
    assert(cwal_value_undefined()==vrhs);
    assert(!vlhs || (cwal_value_undefined()==vlhs));
    /*cwal_refunref(vrhs);
      if(vlhs && vlhs != rhs) cwal_refunref(vlhs);*/
    return 0;
  }else{
    int const rc = whcl__values_bitwiseshift( el, op->id, vlhs,
                                              vrhs, rv );
    if(!rc) cwal_ref(*rv);
    cwal_unref(vlhs);
    cwal_unref(vrhs);
    if(!rc) cwal_unhand(*rv);
    return rc;
  }
}

int whcl__op_f_andor( whcl__op const * const op, whcl_engine * const el,
                      int argc, cwal_value **rv ){
  cwal_value * rhs, * lhs;
  assert(2==argc);
  rhs = whcl__pop_val(el);
  lhs = whcl__pop_val(el);
  assert(rhs);
  assert(lhs);
  cwal_ref(rhs);
  cwal_ref(lhs);
  /**
     In theory, if el->skipLevel>0 and if all operators comply and use
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
  switch((el->skipLevel>0) ? 0 : op->id){
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
    case TOK1_T_OpAnd:
#define USE_JS_SEMANTICS 1
#if USE_JS_SEMANTICS
      *rv = !cwal_value_get_bool(lhs) ? lhs : rhs;
#else
      *rv = cwal_new_bool( cwal_value_get_bool(lhs)
                           && cwal_value_get_bool(rhs) );
#endif
      break;
    case TOK1_T_OpOr:
#if USE_JS_SEMANTICS
      *rv = cwal_value_get_bool(lhs) ? lhs : rhs;
#else
      *rv = cwal_new_bool( cwal_value_get_bool(lhs)
                           || cwal_value_get_bool(rhs) );
#endif
#undef USE_JS_SEMANTICS
      break;
    case TOK1_T_OpElvis:
      *rv = cwal_value_undefined()==lhs ? rhs : lhs;
      break;
    case TOK1_T_OpOr3:
      *rv = cwal_value_get_bool(lhs) ? lhs : rhs;
      break;
    default:
      if(argc){/*avoid unused param warning*/}
      assert(!"Invalid op mapping!");
  }
  cwal_ref(*rv);
  cwal_unref(rhs);
  cwal_unref(lhs);
  cwal_unhand(*rv);
  return 0;
}


#undef MARKER
#undef whcl__pop_val
#undef whcl__pop_tok
#undef whcl__op_empty_m
