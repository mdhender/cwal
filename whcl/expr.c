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

/**
   Empty-initilized whcl__strace_entry object.
*/
//static const whcl__strace_entry whcl__strace_entry_empty = whcl__strace_entry_empty_m;

int whcl__strace_push( whcl_engine * const el,
                       whcl_script * const ct,
                       whcl_stoken const * srcPos,
                       whcl__strace_entry * const ent ){
  if(el->strace.max && el->strace.count == el->strace.max-1){
    return whcl_err_set(el, CWAL_RC_RANGE,
                        "Stack depth too deep. Max is %"PRIu16". "
                        "Potentially caused by infinite recursion.",
                        el->strace.max);
  }
  if(!srcPos) srcPos = whcl__script_token(ct);
  ent->pos = srcPos;
  ent->ct = ct;
  if(el->strace.tail){
    assert(!ent->down);
    el->strace.tail->down = ent;
    ent->up = el->strace.tail;
    el->strace.tail = ent;
  }else{
    assert(!el->strace.head);
    el->strace.head = el->strace.tail = ent;
  }
  whcl__script_errtoken_set(ct, srcPos);
  ++el->strace.count;
  return 0;
}

void whcl__strace_pop( whcl_engine * const el ){
  assert(el->strace.count);
  assert(el->strace.tail);
  if(el->strace.count){
    whcl__strace_entry * x = el->strace.tail;
    assert(!x->down);
    if(x->up){
      assert(x->up->down == x);
      el->strace.tail = x->up;
      x->up->down = NULL;
      x->up = NULL;
    }else{
      el->strace.head = el->strace.tail = NULL;
    }
    --el->strace.count;
  }else{
    whcl__fatal( CWAL_RC_RANGE, "internal error: "
                 "whcl__strace_pop() called on empty stack.");
  }
}

bool whcl__exception_add_stacktrace(whcl_engine * const el,
                                    cwal_value * const ex){
  bool didIt = false;
  if(el->strace.count
     && !cwal_prop_has_v(ex, el->cache.keyStackTrace, false)){
    cwal_value * stackTrace = 0;
    whcl__strace_generate(el, &stackTrace);
    if(stackTrace){
      cwal_ref(stackTrace);
      /* whcl_dump_val(stackTrace, "stackTrace"); */
      cwal_prop_set_v(ex, el->cache.keyStackTrace, stackTrace);
      cwal_unref(stackTrace);
    }
    didIt = true;
  }
  return didIt;
}

int whcl__add_script_props2( whcl_engine * const el,
                            cwal_value * const ex,
                            char const * scriptName,
                            int line, int col){
  cwal_scope scope = cwal_scope_empty;
  int rc = cwal_scope_push2(el->ec, &scope)
    /* simplifies cleanup here considerably */;
  if(rc) return rc;
  else if(scriptName && *scriptName){
    cwal_value * snv = cwal_new_string_value(el->ec,
                                             scriptName,
                                             cwal_strlen(scriptName));
    rc = snv
      ? cwal_prop_set_v(ex, el->cache.keyScript, snv)
      : CWAL_RC_OOM;
  }
  if(!rc && line>0){
    rc = cwal_prop_set_v(ex, el->cache.keyLine,
                         cwal_new_integer(el->ec, line));
    if(!rc) rc = cwal_prop_set_v(ex, el->cache.keyColumn,
                                 cwal_new_integer(el->ec, col));
  }
  if(!rc){
    whcl__exception_add_stacktrace(el, ex);
  }else if(CWAL_RC_OOM==rc){
    WHCL__WARN_OOM;
  }
  cwal_scope_pop(el->ec);
  return rc;
}

int whcl__add_script_props( whcl_engine * const el, cwal_value * const ex,
                            whcl_script const * script ){
  if(ex && !script) script = el->ct;
  if(!ex
     || !script
     || !cwal_props_can(ex)
     || cwal_prop_has_v(ex, el->cache.keyStackTrace, false)
     /* ^^^ very cursory check for "already has this state" */
     ) return 0;
  else{
    cwal_midsize_t scriptNameLen = 0;
    char const * scriptName = whcl_script_name_get(script, &scriptNameLen);
    whcl_stoken const * errPos = whcl__script_err_pos(script);
    assert(errPos && "Set the whcl_script's error position");
    if(scriptName || errPos->line>0){
      return whcl__add_script_props2(el, ex, scriptName,
                                     errPos->line, errPos->column);
    }else{
      return 0;
    }
  }
}

int whcl__strace_generate( whcl_engine * const el, cwal_value ** rv ){
  cwal_array * ar = 0;
  int rc = 0;
  whcl__strace_entry * ent = el->strace.tail;
  cwal_size_t const oldCount = el->strace.count;
  int limit =
    el->flags.stacktraceLimit < 0 ? 10000 : el->flags.stacktraceLimit;
  /* MARKER(("el->strace.count=%u ent=%p\n", el->strace.count, (void const *)ent)); */
  if(!ent || !el->flags.stacktraceLimit){
    *rv = 0;
    return 0;
  }
  el->strace.count = 0
    /* Workaround for co-dependency via
       whcl__add_script_props() */;
  for( ; !rc && ent && limit-->0; ent = ent->up ){
    /* Generate array of stack trace entries */
    cwal_value * c;
    if(!ar){
      ar = cwal_new_array(el->ec);;
      if(!ar){
        rc = CWAL_RC_OOM;
        WHCL__WARN_OOM;
        break;
      }
      cwal_ref(cwal_array_value(ar));
    }
    c = cwal_new_object_value(el->ec);
    if(!c){
      rc = CWAL_RC_OOM;
      WHCL__WARN_OOM;
      break;
    }
    cwal_ref(c);
    rc = cwal_array_append(ar, c);
    cwal_unref(c);
    if(!rc){
      whcl_stoken const * et = whcl__script_errtoken_get(ent->ct);
      whcl__script_errtoken_set(ent->ct, ent->pos);
      rc = whcl__add_script_props(el, c, ent->ct);
      whcl__script_errtoken_set(ent->ct, et);
    }
  }

  el->strace.count = oldCount;
  if(rc) cwal_array_unref(ar);
  else{
    cwal_value_unhand(cwal_array_value(ar));
    *rv = cwal_array_value(ar);
  }
  return rc;
}

whcl__stoken * whcl__stoken_alloc( whcl_engine * const el ){
  whcl__stoken * s;
  //++el->metrics.tokenRequests;
  s = whcl__stoken_stack_pop(&el->recycler.stok);
  if(!s){
    s = (whcl__stoken*)cwal_malloc(el->ec, sizeof(whcl__stoken));
    if(s){
      //++el->metrics.tokenAllocs;
      cwal_engine_adjust_client_mem(el->ec, (cwal_int_t)sizeof(whcl__stoken));
    }
  }
  if(s){
    *s = whcl__stoken_empty;
    //if(++el->metrics.liveTokenCount > el->metrics.peakLiveTokenCount){
    //  el->metrics.peakLiveTokenCount = el->metrics.liveTokenCount;
    //}
  }
  return s;
}

whcl__stoken * whcl__stoken_alloc2( whcl_engine * const el, int type, cwal_value * const v ){
  whcl__stoken * const t = whcl__stoken_alloc(el);
  if(t){
    t->ttype = type;
    t->value = v;
  }
  return t;
}


void whcl__stoken_free( whcl_engine * const el, whcl__stoken * const t,
                        bool allowRecycle ){
  /*
    Reminder: t does not hold a reference to t->value, so we do not
    let a reference go here. Any stray stack machine values
    (e.g. those left over during error handling mid-expression) will
    be cleaned up by the scope which is, more likely than not, about
    to pop as a result of error propagation (or it's the global scope,
    in which case it's free to sweep them up).
  */
  assert(t);
  assert(!t->next);
  if(allowRecycle && (el->recycler.stok.size < el->recycler.maxSTokens)){
    whcl__stoken_stack_push( &el->recycler.stok, t );
  }else{
    *t = whcl__stoken_empty;
    assert(el->ec->metrics.clientMemCurrent>0);
    cwal_engine_adjust_client_mem(el->ec, -((cwal_int_t)sizeof(whcl__stoken)));
    cwal_free2( el->ec, t, sizeof(whcl__stoken) );
  }
}

void whcl__stoken_stack_push( whcl__stoken_stack * const ts,
                              whcl__stoken * const t ){
  assert(ts);
  assert(t);
  assert(!t->next);
  assert(t != ts->top);
  t->next = ts->top;
  ts->top = t;
  ++ts->size;
}

whcl__stoken * whcl__stoken_stack_pop( whcl__stoken_stack * const ts ){
  whcl__stoken * t = 0;
  assert(ts);
  if(ts->size>0){
    t = ts->top;
    assert(t);
    ts->top = t->next;
    t->next = 0;
    --ts->size;
  }
  return t;
}

void whcl__stoken_stack_clear( whcl_engine * const el, whcl__stoken_stack * const st,
                               bool allowRecycle ){
  whcl__stoken * t;
  while( (t = whcl__stoken_stack_pop(st)) ){
    whcl__stoken_free(el, t, allowRecycle);
  }
}

void whcl__estack_clear( whcl_engine * const el, whcl__estack * const st,
                         bool allowRecycle ){
  whcl__stoken_stack_clear( el, &st->vals, allowRecycle );
  whcl__stoken_stack_clear( el, &st->ops, allowRecycle );
}

void whcl__estack_swap( whcl__estack * const lhs, whcl__estack * const rhs ){
  whcl__estack const tmp = *lhs;
  *lhs = *rhs;
  *rhs = tmp;
}

void whcl__engine_stack_swap( whcl_engine * const el, whcl__estack * const st ){
  whcl__estack const tmp = el->estack;
  el->estack = *st;
  *st = tmp;
}

void whcl__engine_push( whcl_engine * const el, whcl__stoken * const t ){
  whcl__stoken_stack_push( whcl__stoken_op(t) ? &el->estack.ops : &el->estack.vals, t );
}

void whcl__engine_push_valtok( whcl_engine * const el, whcl__stoken * const t ){
  whcl__stoken_stack_push( &el->estack.vals, t );
}

void whcl__engine_push_op( whcl_engine * const el, whcl__stoken * const t ){
  whcl__stoken_stack_push( &el->estack.ops, t );
}

whcl__stoken * whcl__engine_push_ttype( whcl_engine * const el, int i ){
  whcl__stoken * const t = whcl__stoken_alloc2( el, i, 0 );
  if(t) whcl__engine_push( el, t );
  return t;
}

whcl__stoken * whcl__engine_push_val( whcl_engine * const el, cwal_value * const v ){
  whcl__stoken * const t = whcl__stoken_alloc2( el, TOK1_T_Value, v );
  if(t) whcl__engine_push_valtok( el, t );
  return t;
}

whcl__stoken * whcl__engine_push_tv( whcl_engine * const el, int ttype, cwal_value * const v ){
  whcl__stoken * t = whcl__stoken_alloc2( el, ttype, v );
  if(t) whcl__engine_push_valtok( el, t );
  return t;
}

whcl__stoken * whcl__engine_push_int( whcl_engine * const el, cwal_int_t i ){
  whcl__stoken * rc = NULL;
  cwal_value * const v = cwal_new_integer(el->ec, i);
  if(v && !(rc = whcl__engine_push_val(el, v))){
    cwal_refunref(v);
  }
  return rc;
}


whcl__stoken * whcl__engine_peek_token( whcl_engine * const el ){
  return el->estack.vals.top;
}

cwal_value * whcl__engine_peek_value( whcl_engine * const el ){
  return el->estack.vals.top ? el->estack.vals.top->value : 0;
}

whcl__stoken * whcl__engine_peek_op( whcl_engine * const el ){
  return el->estack.ops.top;
}

static whcl__stoken * whcl__engine_pop_token_impl( whcl_engine * const el,
                                                  whcl__stoken_stack * const ts,
                                                  bool returnItem ){
  whcl__stoken * rc = whcl__stoken_stack_pop(ts);
  if(rc && !returnItem){
    whcl__stoken_free( el, rc, 1 );
    rc = NULL;
  }
  return rc;
}

whcl__stoken * whcl__engine_pop_token( whcl_engine * const el, bool returnItem ){
  return whcl__engine_pop_token_impl(el, &el->estack.vals, returnItem);
}

whcl__stoken * whcl__engine_pop_op( whcl_engine * const el, bool returnItem ){
  return whcl__engine_pop_token_impl(el, &el->estack.ops, returnItem);
}

cwal_value * whcl__engine_pop_value( whcl_engine * const el ){
  cwal_value * v = 0;
  whcl__stoken * const t = whcl__engine_pop_token(el, true);
  if(t){
    v = t->value;
    t->value = 0;
    whcl__stoken_free(el, t, 1);
  }
  return v;
}

/**
   Internal helper for whcl__eval_expr_impl().

   Possibly processes pending operators in el's stack, depending on op
   and its precedence in relation to the operator (if any) to the left
   (i.e. in engine's operator stack). Returns 0 on success (which includes
   it doing nothing of note).

   Specifically: if el->estack has a pending operator (and operand(s))
   with a higher priority than op, or the same priority but the
   operator is left-associative, then that pending operator is
   processed. This repeats, if needed, to resolve all pending LHS ops
   until el->estack is out of ops or we hit an op with a lower
   precedence than the given op (or equal precedence but not
   left-associative).
*/
static int whcl__eval_lhs_ops(whcl_engine * const el,
                              whcl__op const * op){
  whcl__stoken * topOpTok = whcl__engine_peek_op(el);
  whcl__op const * topOp = topOpTok ? whcl__stoken_op(topOpTok) : NULL;
  int rc = whcl__check_interrupted(el, 0);
  assert(op);
  if(rc) return rc;
#if 0
  else if(topOp
     && topOp->placement!=0
     && topOp->arity>0
     && (s2_ttype_is_assignment(op->id) || s2_ttype_is_assignment_combo(op->id))
     ){
    return s2_err_ptoker( se, pr, CWAL_SCR_SYNTAX,
                          "Invalid operator '%s' preceeding '%s'.",
                          topOp->sym, op->sym);
    /* This is admittedly a workaround for the X.Y=Z family of ops,
       which do not get a chance to set se->dotLhsOp and se->dotLhsKey
       before the ++ is run. It turns out that JavaScript disallows
       such ops on the LHS of an assignment, too.
     */
  }
#endif
  while(topOp &&
        ((op->prec < topOp->prec)
         || (op->assoc<0 && (op->prec==topOp->prec)))
        ){
#if 0
    if(el->flags.traceTokenStack){
      MARKER(("Processing ge-precedent op '%s' to "
              "the left of '%s'.\n",
              topOp->sym, op->sym));
      s2_dump_val(se->dotOp.lhs,"se->dotOp.lhs");
      s2_dump_val(se->dotOp.key,"se->dotOp.key");
    }
#endif
    rc = whcl__process_top(el);
    if(rc){
      //rc = s2_ammend_op_err(se, 0, rc);
      break;
    }
    assert(el->estack.vals.size>0);
    topOpTok = whcl__engine_peek_op(el);
    topOp = topOpTok ? whcl__stoken_op(topOpTok) : NULL;
  }
  return rc;
}


static int whcl__eval_expr_impl( whcl_engine * const el,
                                 whcl__op const * const fromLhsOp,
                                 uint32_t exprFlags, cwal_value ** rv ){
  int rc = 0;
  whcl__estack stackOld = whcl__estack_empty;
  whcl_script * const oldToker = el->ct;
  whcl_script * const ct  = el->ct;
  whcl_stoken const * const firstTok = whcl__script_token(el->ct);
  whcl_stoken const * tok = NULL;
  whcl_stoken const * prevTok = NULL;
  whcl__op const * op = NULL;
  whcl__op const * prevOp = NULL;
  cwal_value * xrv = NULL;
  whcl_scope * scel = NULL;
  cwal_midsize_t tokLen = 0;
  char const * tokStr;
  uint32_t totalValCount = 0;
  cwal_size_t const holderLen = whcl__holder_len(el);
  static const bool ownStack = true;
  assert(el->ct);
  if(ownStack){
    whcl__engine_stack_swap(el, &stackOld);
  }
  
  if(WHCL__EXPR_F_PUSH_SCOPE & exprFlags){
    if(!(scel = whcl__scope_push(el, 0))){
      rc = CWAL_RC_OOM; goto end;
    }
  }
  if(!fromLhsOp){
    whcl__dotop_set(el, NULL, NULL, NULL);
  }

#define ERRTOK whcl__script_errtoken_set(el->ct, tok)
  /*
    The following is adapted from:

    https://en.wikipedia.org/wiki/Stack_(data_structure)#Expression_evaluation_and_syntax_parsing

    https://en.wikipedia.org/wiki/Shunting-yard_algorithm

    https://en.wikipedia.org/wiki/Operator-precedence_parser

    For later reference (but not used here): Pratt Parsing:
    https://en.wikipedia.org/wiki/Pratt_parser
    https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
  */
  typedef int (*nexter)(whcl_script * const, whcl_stoken const **);
  nexter const nextToken = (WHCL__EXPR_F_IGNORE_EOL & exprFlags)
    ? whcl__next_token_no_eol : whcl__script_next_token2;
  for( ; 0==rc
         && (firstTok ? ((WHCL__EXPR_F_IGNORE_EOL & exprFlags)
                         || !whcl__script_is_eox(ct)) : 1)
         // on a ^^^^^^^^^^ freshly-compiled toker, we're not yet at
         // the first token
         && (0==(rc = nextToken(ct, &tok)));
       prevOp = op, prevTok = tok){
    //whcl__dump_stok(ct, tok, "start of expr loop");
    if(whcl_t_is_eox(tok->ttype)){
      /* We can stop now, but first do some error checking on our
         expected state... */
      if(prevOp &&
         ((prevOp->placement<0 && prevOp->arity>0)/*prefix op*/
          || (prevOp->placement==0 && prevOp->arity>1)/*infix op*/)){
        ERRTOK;
        rc = whcl_err_set(el, CWAL_SCR_SYNTAX,
                          "Invalid operator '%s' before EOX.",
                          prevOp->sym);
      }else if(prevTok && tok1_t_is_eof(prevTok->ttype)){
        if(prevOp && prevOp->arity>0 && prevOp->placement<=0){
          ERRTOK;
          rc = whcl_err_set(el, CWAL_SCR_SYNTAX,
                            "Non-nullary operator '%s' before EOF.",
                            prevOp->sym);
        }else if(fromLhsOp){
          ERRTOK;
          rc = whcl_err_set(el, CWAL_SCR_SYNTAX,
                            "Expecting RHS but got EOF.");
        }
      }else if(TOK1_T_EOX == tok->ttype
               && (exprFlags & WHCL__EXPR_F_EOX_NO_SEMICOLON)){
        /* Catch constructs which end in a semicolon before a logical
           EOF, where a semicolon is harmless but should really not
           be. */
        ERRTOK;
        rc = whcl_err_set(el, CWAL_SCR_SYNTAX, "Unexpected semicolon.");
      }
      //whcl__dump_stok(ct, tok, "Ending expr eval for EOX.");
      break;
    }/*if @ eox*/
    xrv = NULL;
    tokStr = whcl_stoken_cstr(ct, tok, &tokLen, false);
    switch(tok->ttype){ /* Some basic sanity checks */
      case TOK1_T_SquigglyOpen:
      case TOK1_T_HeredocStart:
      case TOK1_T_HeredocStart2:
      case TOK1_T_CallBlockOpen:
      case TOK1_T_BraceOpen:
      case TOK1_T_ParenOpen:
        MARKER(("Very unexpected token type: %s\n", tok1_t_cstr(tok->ttype)));
        assert(!"Cannot happen - gets handled by token parsing");
        /* But in case it does, fall through... */
        CWAL_SWITCH_FALL_THROUGH;
      case TOK1_T_SquigglyClose:
      case TOK1_T_BraceClose:
      case TOK1_T_ParenClose:
        ERRTOK;
        rc = whcl_err_set(el, CWAL_SCR_SYNTAX, "Mismatched '%.*s' token",
                          (int)tokLen, tokStr);
        break;
    }
    if((rc = whcl__check_interrupted(el, rc))) break;

    op = whcl__ttype_op( tok->ttype );
    if(op && !fromLhsOp && !prevOp){
      whcl__dotop_set(el, NULL, NULL, NULL);
    }
    if(fromLhsOp && op && (op->prec < fromLhsOp->prec
                           && op->assoc<0)){
      /* This invocation was catching an RHS, but we can stop now. */
      whcl__script_putback(ct);
      break;
    }
    assert(0==rc);
    if(op && (prevOp || !whcl__engine_peek_token(el))){
      /* If previous token is an operator or we're at the start
         of the expression, change plus/minus to their unary
         forms */
      switch(op->id){
        case TOK1_T_OpPlus:
        case TOK1_T_OpMinus:
          //if(!prevOp || tok1_t_may_precede_unary(prevOp->id)){
          /* MARKER(("Changing token '%s' to unary form.\n", op->sym)); */
          op = whcl__ttype_op((TOK1_T_OpPlus==op->id)
                              ? TOK1_T_OpPlusUnary
                              : TOK1_T_OpMinusUnary);
          assert(op);
          //}
          break;
      }
      if(op && prevOp && prevOp->arity>0 && prevOp->placement<=0
         && op->placement>=0 && op->arity!=0
         /* e.g. OpPlus followed by OpMultiply */){
        /* Consecutive non-nullary operators */
        if(op->assoc<=0){
          ERRTOK;
          rc = whcl_err_set(el, CWAL_SCR_SYNTAX,
                            "Unexpected consecutive operators: "
                            "'%s' ==> '%s'", prevOp->sym, op->sym);
          break;
        }
      }
    }/* op vs prevOp checks */
    assert(0==rc);
    if(!op){
      /* A non-operator ("value" or [command...]) token... */
      if(prevOp && prevOp->placement>0 && prevOp->arity>0){
        ERRTOK;
        rc = whcl_err_set(el, CWAL_SCR_SYNTAX,
                          "Unexpected token after postfix operator: "
                          "%s ==> %s", tok1_t_cstr(prevOp->id),
                          tok1_t_cstr(tok->ttype));
        break;
      }else if(!prevOp && whcl__engine_peek_value(el)){
        ERRTOK;
        rc = whcl_err_set(el, CWAL_SCR_SYNTAX,
                          "Unexpected consecutive non-operators: "
                          "%s ==> %s %.*s", tok1_t_cstr(prevTok->ttype),
                          tok1_t_cstr(tok->ttype),
                          (int)tokLen, tokStr);
        break;
      }
    }/* if !op */
    /* Now down to business... */
    int const effectiveTtype = op ? op->id :
           (whcl__token_has_prop(ct, tok)
            ? TOK1_T_PropAccess : tok->ttype);
    switch(effectiveTtype){
      case TOK1_T_OpOr:
      case TOK1_T_OpAnd:{
        /*
          For short-circuit logic we need both this loop and the
          stack to be in skip mode, but we also have to know when
          to back out of skip mode.

          Evaluate RHS expression in skip mode when:

          - op==OpOr and topVal is truthy,
          - op==OpAnd and topVal is falsy
        */
        cwal_value * lhs = 0;
        rc = whcl__eval_lhs_ops(el, op);
        if( rc ) break;
        lhs = whcl__engine_peek_value(el);
        if(!lhs){
          rc = whcl_err_set(el, CWAL_SCR_SYNTAX,
                            "Missing LHS for %s operator.",
                            op->sym);
          goto end;
        }
        else if(el->skipLevel>0){
          assert(cwal_value_undefined()==lhs
                 && "Else skip-mode eval convention was violated");
          break;
        }
        rc = whcl__holder_push(el, lhs);
        if(rc) break;
        bool const buul = cwal_value_get_bool(lhs);
        bool const shortIt = TOK1_T_OpOr==op->id ? buul : !buul;
        if(shortIt){
          /** short-circuit the RHS by calling back into this function
              and passing it the current op as a cut-off precedence. */
          cwal_value * rhs = NULL;
          //MARKER(("op %s should short circuit RHS here.\n", op->sym));
          ++el->skipLevel;
          rc = whcl__eval_expr_impl(el, op,
                                    exprFlags & WHCL__EXPR_F_RECURSIVE,
                                    &rhs);
          //whcl__dump_stok(el->ct, whcl__script_token(el->ct), "???");
          --el->skipLevel;
          /* MARKER(("after %d op: skipLevel=%d\n", pt.ttype, se->skipLevel)); */
          assert(lhs == whcl__engine_peek_token(el)->value);
          if(rc){
            /* Reminder: lhs is not being leaked: it's in the eval-holder. */
            break;
          }
          if(rhs){
            whcl__stoken * topTok =
              whcl__engine_pop_token(el, true) /* the short-circuited value */;
            assert(topTok->value);
            assert(lhs == topTok->value);
            topTok->value = NULL;
            whcl__stoken_free(el, topTok, true);
            assert(cwal_value_undefined()==rhs
                   && "Violation of current API convention for skip-mode evals");
            topTok = whcl__engine_push_val(el, lhs) //cwal_new_bool(buul))
              /* that allocation cannot fail b/c it will recycle the
                 popped one unless someone foolishly disables the
                 stack token recycler. */;
            topTok->srcPos.ct = el->ct;
            topTok->srcPos.tok = tok;
            /* whcl__dump_val(lhs, "new LHS"); */
            op = NULL /* so prevOp does not expose our trickery to the next
                         iteration */;
            continue;
          }else{
            ERRTOK;
            rc = whcl_err_set(el, CWAL_SCR_SYNTAX,
                              "Empty RHS for %s operator.", op->sym);
          }
        }/*if(shortIt)*/
        break;
      }/* end &&, ||, ||| */
      case TOK1_T_OpOrBitwise:
      case TOK1_T_OpAndBitwise:
      case TOK1_T_OpNegateBitwise:
      case TOK1_T_OpXOr:
      case TOK1_T_OpShiftRight:
      case TOK1_T_OpShiftLeft:
      case TOK1_T_OpNot:
      case TOK1_T_OpCmpGT:
      case TOK1_T_OpCmpGE:
      case TOK1_T_OpCmpLT:
      case TOK1_T_OpCmpLE:
      case TOK1_T_OpCmpEq:
      case TOK1_T_OpCmpNotEq:
      case TOK1_T_OpModulo:
      case TOK1_T_OpMultiply:
      case TOK1_T_OpDivide:
      case TOK1_T_OpPlus:
      case TOK1_T_OpPlusUnary:
      case TOK1_T_OpMinus:
      case TOK1_T_OpMinusUnary:
        break;
      case TOK1_T_ParenGroup:{
        rc = whcl__eval_sub(el, false, ct, WHCL__EVAL_F_SUB_EXPR, &xrv);
        if(0==rc){
          if(xrv){
            rc = whcl__holder_push(el, xrv);
          }else{
            whcl__dump_stok(ct, tok, "WARNING (internal misuse): "
                           "expr token did not evaluate to a value");
          }
        }
        break;
      }
      case TOK1_T_BIC:
      case TOK1_T_BIV:
      case TOK1_T_BraceGroup:
      case TOK1_T_CallBlock:
      case TOK1_T_Heredoc:
      case TOK1_T_Identifier:
      case TOK1_T_LiteralNumber:
      case TOK1_T_PropAccess:
      case TOK1_T_QuotedString:
      case TOK1_T_SquigglyGroup:
      case TOK1_T_IdentifierDeref:
        if( (rc = whcl__eval_token(el, el->ct, tok, 0, &xrv)) ) break;
        if(xrv){
          rc = whcl__holder_push(el, xrv);
        }else{
          switch(effectiveTtype){
            case TOK1_T_BIV:
            case TOK1_T_PropAccess:
            case TOK1_T_IdentifierDeref:
              xrv = cwal_value_undefined();
              break;
            default:
              whcl__dump_stok(ct, tok, "WARNING (internal misuse): "
                             "expr token did not evaluate to a value");
              break;
          }
        }
        break;
      default:
        ERRTOK;
        rc = whcl_err_set(el, CWAL_SCR_SYNTAX,
                          "Unhandled expr element: %s %.*s",
                          tok1_t_cstr(tok->ttype),
                          (int)tokLen, tokStr);
        break;
    }/*basic sanity checks*/
    if(rc) break;
    //whcl__dump_stok(el->ct, tok, "current expr token");
    /************************************************************/
    /**
       We're done with the handling of the token. We now either have a
       value (xrv) or an operator (op) to put onto the stack...
    */
    /************************************************************/
    if(op){
      assert(NULL==xrv);
      if(/*check for non-unary operator at the start of the script*/
         op->arity>0
         && op->placement>=0
         && el->estack.vals.size < op->arity-1
         /* ^^^ no values the op can worth with*/){
        ERRTOK;
        rc = whcl_err_set(el, CWAL_SCR_SYNTAX, "Illegal operator '%s' "
                          "at start of expression.", op->sym);
      }else{
        rc = whcl__eval_lhs_ops(el, op);
        if(0==rc && el->estack.vals.size){
          rc = whcl__holder_push(el, whcl__engine_peek_value(el));
        }
        if(!rc){
          whcl__stoken * const topOpTok = whcl__engine_push_ttype(el, op->id)
            /* Reminder: use op->id instead of tok->ttype because of the unary
               plus/minus finagling done above. */;
          if(!topOpTok){
            rc = CWAL_RC_OOM;
            WHCL__WARN_OOM;
          }else{
            topOpTok->srcPos.ct = el->ct;
            topOpTok->srcPos.tok = tok;
          }
        }
      }
    }/*...op*/else/*value...*/{
      assert(xrv);
      whcl__stoken * const vtok = whcl__engine_push_tv(el, tok->ttype, xrv);
      if(!vtok){
        rc = CWAL_RC_OOM;
        WHCL__WARN_OOM;
      }else{
        ++totalValCount;
        vtok->srcPos.ct = el->ct;
        vtok->srcPos.tok = tok;
        if(el->skipLevel>0 && cwal_value_undefined()!=xrv){
          whcl__dump_val(xrv, "what is this???");
          assert(!"current internal convention for skip-mode evaluation was violated");
          whcl__fatal( CWAL_RC_ASSERT, "Internal convntion for "
                       "skip-mode evaluation was violated." );
        }
      }
    }/*value*/
  }/*main for() loop*/
  xrv = NULL;
  if(rc) goto end;
  if(ownStack){
    while(!rc && el->estack.ops.size){
      rc = whcl__process_top(el);
    }
    if(rc) goto end;
  }
  if(ownStack && el->estack.vals.size!=1){
    /* too many or too few items in the stack */
    if(!el->estack.vals.size
       && (!firstTok || whcl_t_is_eox(firstTok->ttype))){
      //---- ^^^^^^^^^^^^^^ i'm not 100% sure this heuristic is
      // correct.
      /* ==> empty expression. Fine. */
    }else if(el->estack.vals.size>0 && totalValCount){
      ERRTOK;
      rc = whcl_err_set(el, CWAL_SCR_SYNTAX,
                        "Unexpected stack size "
                        "(%d) after expression.",
                        (int)el->estack.vals.size);
      goto end;
    }
  }/* stack size error check */
  assert(0==rc);
  xrv = whcl__engine_pop_value(el);
  if(el->skipLevel>0){
    assert((!xrv || (xrv == cwal_value_undefined()))
           && "current internal convention for "
           "skip-mode evaluation was violated");
  }
  end:
  rc = whcl__check_interrupted(el, rc);
  cwal_value * const propagate = rc||!rv ? NULL : xrv;
  whcl__holder_truncate(el, holderLen, propagate);
  el->ct = oldToker;
  if(ownStack){
    whcl__engine_stack_swap(el, &stackOld);
    whcl__estack_clear(el, &stackOld, true);
  }
  if(scel){
    assert(whcl_scope_current(el) == scel);
    whcl_scope_pop(el, scel, propagate);
  }
  if(0==rc && rv) *rv = propagate;
  return rc;
#undef ERRTOK
}

int whcl__eval_expr( whcl_engine * const el, uint32_t flags, cwal_value ** rv ){
  return whcl__eval_expr_impl(el, NULL, flags, rv);
}

#undef MARKER
