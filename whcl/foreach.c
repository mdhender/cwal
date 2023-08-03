/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

/**
   Impl for the foreach builtin command.
*/
#include "internal.h"
#include <string.h>
#include <assert.h>

#include <stdio.h>
#if 1
#define MARKER(pfexpr) if(1) printf("MARKER: %s:%d:\t",__FILE__,__LINE__); if(1) printf pfexpr
#else
#define MARKER if(0) printf
#endif

/**
   State used by whcl__foreach_iterate().
*/
struct whcl__foreach_state {
  whcl_engine * el;
  whcl_script * ct;
  whcl_stoken const * body;
  cwal_value * keyName;
  cwal_value * valName;
  cwal_size_t index;
};
typedef struct whcl__foreach_state whcl__foreach_state;
static const whcl__foreach_state whcl__foreach_state_empty = {
0,0,0,0,0,0U
};

/**
   Internal helper for wchl__foreach_iterate(), processing one entry
   from the result set.

   body holds the whole for-each body. If keyName is not NULL then it
   is the NAME part of:

   ```
   foreach [indexName] NAME value {...}
   ```

   and the key parameter must be the associated property key or array
   index.

   If valName is not NULL, it is the indexName part of
   the above example.

   The val param holds the associated property value or array
   entry.

   Combinations of NULL/non-NULL which are legal depends on the
   operand's type, but it is never legal for both to be NULL.

   Returns 0 on success.
*/
static int whcl__foreach_one( whcl_engine * const el,
                              whcl_script * const ct,
                              whcl_stoken const * const body,
                              cwal_value * const keyName,
                              cwal_value * const key,
                              cwal_value * const valName,
                              cwal_value * const val ){
  whcl_scope * const scel = whcl__scope_push(el, WHCL__SCOPE_F_LOOP);
  if(!scel) return CWAL_RC_OOM;
  int rc;
  assert(keyName ? !!key : !key);
  assert(valName ? !!val : !!keyName);
  if(keyName && (rc = whcl_scope_set_v( el, NULL, false,
                                        keyName, key))){
    goto end;
  }
  if(valName && (rc = whcl_scope_set_v( el, NULL, false,
                                        valName, val))){
    goto end;
  }
  whcl__stoken_set(ct, body);
  rc = whcl__eval_sub(el, false, ct, 0, NULL);
  switch(rc){
    /* case CWAL_RC_BREAK: is handled higher up */
    case CWAL_RC_CONTINUE:
      rc = 0;
      whcl_err_reset(el);
      break;
    default:
      break;
  }
  end:
  assert(el->scopes.current = scel);
  whcl_scope_pop(el, scel, NULL);
  return rc;
}

/* cwal_array_visitor_f() impl for whcl__foreach_iterate(). */
static int whcl__foreach_array_visitor( cwal_array * a, cwal_value * v,
                                        cwal_size_t index, void * state ){
  whcl__foreach_state * const fst = (whcl__foreach_state*)state;
  int rc = 0;
  cwal_value * vIndex;
  cwal_engine * const e = fst->el->ec;
  cwal_size_t const holderLen = whcl__holder_len(fst->el);
  vIndex = fst->keyName ? cwal_new_integer(e, (cwal_int_t)index) : NULL;
  if(fst->keyName && !vIndex){
    if(a){/*avoid unused param warning*/}
    WHCL__WARN_OOM;
    return CWAL_RC_OOM;
  }
  if(vIndex && (rc=whcl__holder_push(fst->el, vIndex))){
    goto end;
  }
  rc = whcl__foreach_one( fst->el, fst->ct, fst->body,
                          fst->keyName, vIndex,
                          fst->valName, v ? v : cwal_value_undefined() );
  end:
  whcl__holder_truncate(fst->el, holderLen, NULL);
  return rc;
}

/* cwal_value_visitor_f() impl for whcl__foreach_iterate(), for
   visiting tuples. */
static int whcl__foreach_tuple_visitor( cwal_value * v, void * state ){
  whcl__foreach_state * const fst = (whcl__foreach_state*)state;
  int rc;
  cwal_value * vIndex = NULL;
  cwal_size_t const holderLen = whcl__holder_len(fst->el);
  cwal_engine * const e = fst->el->ec;
  vIndex = fst->keyName ? cwal_new_integer(e, (cwal_int_t)fst->index++) : 0;
  if(fst->keyName && !vIndex){
    WHCL__WARN_OOM;
    return CWAL_RC_OOM;
  }
  if(vIndex && (rc=whcl__holder_push(fst->el, vIndex))){
    goto end;
  }
  rc = whcl__foreach_one( fst->el, fst->ct, fst->body,
                          fst->keyName, vIndex,
                          fst->valName, v ? v : cwal_value_undefined() );
  end:
  whcl__holder_truncate(fst->el, holderLen, NULL);
  return rc;
}

/**
   foreach impl for strings. vStr must be the X part of
   foreach varname X {...} and it must be of type string (that gets
   asserted). Unlike most places, a Buffer will not act like a string
   here (foreach will use the Object property path for Buffers).

   Note that iteration using this approach is far more efficient than
   iterating over its length because that approach has to (for
   non-ASCII strings) re-traverse the whole string to find the n'th
   character.

   Returns 0 on success.
*/
static int whcl__foreach_string_char( cwal_value * const vStr,
                                      whcl__foreach_state * const fst ){
  int rc = 0;
  cwal_midsize_t slen = 0;
  cwal_string * str = cwal_value_get_string(vStr);
  char unsigned const * cstr = (char unsigned const *)cwal_string_cstr2( str, &slen );
  char unsigned const * pos = cstr;
  char const isAscii = cwal_string_is_ascii(str);
  cwal_engine * const e = fst->el->ec;
  cwal_value * vIndex = 0;
  cwal_value * vVal = 0;
  char unsigned const * const eof = pos + slen;
  assert(str);
  assert(cstr);
  for( ; !rc && pos < eof && fst->index < slen; ++fst->index ){
    unsigned char const * cend = pos;
    cwal_size_t const holderLen = whcl__holder_len(fst->el);
    if(isAscii){
      pos = cstr + fst->index;
      cend = pos + 1;
    }else{
      cwal_utf8_read_char(pos, eof, &cend);
      if(cend<=pos) break /*???*/;
    }
    assert(cend <= eof);
    vVal = cwal_new_string_value(e, (char const*)pos, (cwal_size_t)(cend-pos));
    if(!vVal){
      WHCL__WARN_OOM;
      rc = CWAL_RC_OOM;
      break;
    }
    if(rc || (rc = whcl__holder_push(fst->el, vVal))) break;
    if(fst->keyName){
      vIndex = cwal_new_integer(e, (cwal_int_t)fst->index);
      if(!vIndex){
        WHCL__WARN_OOM;
        rc = CWAL_RC_OOM;
        break;
      }
      if(rc || (rc = whcl__holder_push(fst->el, vIndex))) break;
    }
    rc = whcl__foreach_one( fst->el, fst->ct, fst->body,
                            fst->keyName, vIndex,
                            fst->valName, vVal );
    whcl__holder_truncate(fst->el, holderLen, NULL);
    pos = cend;
  }
  return rc;
}

/* cwal_kvp_visitor_f() impl for whcl__foreach_iterate(). */
static int whcl__foreach_kvp_visitor( cwal_kvp const * kvp, void * state ){
  whcl__foreach_state * fst = (whcl__foreach_state*)state;
  cwal_value * const val = cwal_kvp_value(kvp);
  assert(fst->keyName);
  return whcl__foreach_one( fst->el, fst->ct, fst->body,
                            fst->keyName, cwal_kvp_key(kvp),
                            fst->valName, fst->valName ? val : 0 );
}


/**
   Part of the foreach() loop mechanism. This part loops over the
   {BODY} part of foreach ... {BODY}.

   arguments:

   el and ct: the engine and tokenizer.

   body: the token holding the complete body of the loop. It must
   be of type TOK1_T_SquigglyGroup.

   container: the container or string to iterate over.

   opMode: what to iterate over: 0 = object props. TOK1_T_At = array and
   tuple entries. TOK1_T_QuotedString = a string (iterate over its
   characters), noting that it need not be a literal (we just use this
   token type ID because it's convenient to do so). Anything else is
   illegal and may trigger an assert().

   keyName: the symbol name of the key part of foreach KEY val
   container {}

   valName: the symbol name of the val part of foreach key VAL
   container {}
*/
static int whcl__foreach_iterate( whcl_engine * const el,
                                  whcl_script * const ct,
                                  whcl_stoken const * const body,
                                  cwal_value * const container,
                                  int opMode, cwal_value * const keyName,
                                  cwal_value * const valName){
  int rc = 0;
  whcl__foreach_state fst = whcl__foreach_state_empty;
  assert(0==opMode
         || TOK1_T_At==opMode
         || TOK1_T_OpHash==opMode
         || TOK1_T_QuotedString==opMode);
  fst.el = el;
  fst.ct = ct;
  fst.body = body;
  fst.keyName = keyName;
  fst.valName = valName;
  assert(!el->skipLevel);
  /* whcl__dump_val(container,"container"); */
  /* whcl__dump_val(keyName,"keyName"); */
  /* whcl__dump_val(valName,"valName"); */
  switch(opMode){
    case 0: /* Object properties */{
      assert(cwal_props_can(container));
      rc = cwal_props_visit_kvp( container, whcl__foreach_kvp_visitor,
                                 &fst );
      break;
    }/* Objects/Hashes */
    case TOK1_T_At: /* Array/Tuple entries */{
      cwal_array * const ar = cwal_value_array_part(el->ec, container);
      if(ar){
        rc = cwal_array_visit2( ar, whcl__foreach_array_visitor, &fst );
      }else{
        cwal_tuple * const tp = cwal_value_get_tuple(container);
        assert(tp);
        rc = cwal_tuple_visit(tp, whcl__foreach_tuple_visitor, &fst);
      }
      break;
    }
    case TOK1_T_QuotedString:
      rc = whcl__foreach_string_char( container, &fst );
      break;
    default:
      assert(!"Unknown opMode!");
      whcl__fatal(CWAL_RC_CANNOT_HAPPEN, "Unknown foreach opMode.");
      /* not reached */
      break;
  }
  return rc;
}
/**
   Handler for the 'foreach' command.

   foreach [-props] [indexVarName]
      valueVarName iterableValue {BODY}
*/
int whcl__bic_f_foreach(whcl__bic const * const bic,
                        whcl__args const * const args,
                        cwal_value **rv){
  int rc = 0;
  bool doListAsObj = false;
  cwal_value * xrv = NULL;
  uint16_t ndx = 1;
  cwal_size_t const holderLen = whcl__holder_len(args->el);
  cwal_value * vKey = NULL;
  cwal_value * vVar = NULL;
  cwal_value * vIterable = NULL;
  whcl_stoken const * tKey = NULL;
  whcl_stoken const * tVar = NULL;
  whcl_stoken const * tIterable = NULL;
  whcl_stoken const * tBody = NULL;
  whcl_stoken const * tArg = NULL;
  int operandMode = 0
    /* 0 = object props, enum entries, or tuple entries.
       TOK1_T_At = array and tuple entries.
       TOK1_T_QuotedString = chars of a string. */;
  if(args->argc>4){
    while( (tArg = whcl__args_hasflag(args, ndx)) ){
      ++ndx;
      if(whcl_stoken_matches(args->ct, tArg, "-props", 6)){
        doListAsObj = true;
      }else{
        goto foreach_misuse;
      }
    }
  }
  assert(ndx < args->argc-1);
  tArg = args->argv[ndx++];
  if(TOK1_T_Identifier != tArg->ttype){
    goto foreach_misuse;
  }
  tVar = tArg;
  tArg = args->argv[ndx];
  if(TOK1_T_Identifier == tArg->ttype){
    ++ndx;
    tKey = tVar;
    tVar = tArg;
  }
  assert(ndx < args->argc);
  tIterable = args->argv[ndx++];
  assert(ndx < args->argc);
  tBody = args->argv[ndx++];
  if(TOK1_T_SquigglyGroup != tBody->ttype){
    goto foreach_misuse;
  }
  if(tKey){
    rc = whcl__create_value(args->el, args->ct, tKey, &vKey);
    if(rc || (rc = whcl__holder_push(args->el, vKey))) goto end;
  }
  assert(tVar);
  rc = whcl__create_value(args->el, args->ct, tVar, &vVar);
  if(rc || (rc = whcl__holder_push(args->el, vVar))) goto end;
  rc = whcl__eval_token(args->el, args->ct, tIterable, 0, &vIterable);
  if(rc || (rc = whcl__holder_push(args->el, vIterable))) goto end;
  bool nothingToDo = false;
  if(cwal_value_is_array(vIterable)){
    operandMode = doListAsObj ? 0 : TOK1_T_At;
    nothingToDo = doListAsObj
      ? !cwal_props_has_any(vIterable)
      : 0==cwal_array_length_get(cwal_value_get_array(vIterable));
  }else if(cwal_value_is_string(vIterable)){
    operandMode = TOK1_T_QuotedString;
    nothingToDo =
      0==cwal_string_length_bytes(cwal_value_get_string(vIterable));
  }else if(cwal_value_is_tuple(vIterable)){
    operandMode = TOK1_T_At;
    nothingToDo =
      0==cwal_tuple_length(cwal_value_get_tuple(vIterable));
  }else if(!cwal_props_can(vIterable)){
    rc = whcl_err_throw(args->el, CWAL_RC_TYPE,
                        "Invalid type '%s' for foreach.",
                        cwal_value_type_name(vIterable));
    goto end;
  }else{
    nothingToDo = !cwal_props_has_any(vIterable);
  }
  //MARKER(("foreach stuff. Mode=%d\n", operandMode));
  //whcl__dump_val(vKey, "vKey");
  //whcl__dump_val(vVar, "vVar");
  //whcl__dump_val(vIterable, "vIterable");
  if(nothingToDo){
    *rv = cwal_value_undefined();
    rc = 0;
    goto end;
  }
  if(vVar && !vKey && 0==operandMode){
    /* For object property iteration, iterate over the prop keys
       if only one of the var names is provided. */
    vKey = vVar;
    vVar = NULL;
  }
  rc = whcl__foreach_iterate(args->el, args->ct, tBody,
                             vIterable, operandMode,
                             vKey, vVar);
  if(CWAL_RC_BREAK==rc){
    rc = 0;
    if(rv){
      xrv = cwal_propagating_take(args->el->ec);
      assert(xrv && "Else CWAL_RC_BREAK semantics violated.");
    }
    else cwal_propagating_set(args->el->ec, NULL);
  }
  end:
  whcl__holder_truncate(args->el, holderLen, rc||!rv ? NULL : (*rv = xrv));
  return rc;
  foreach_misuse:
  whcl__script_errtoken_set(args->ct, tArg);
  rc = whcl_err_set(args->el, CWAL_SCR_SYNTAX,
                    "%s usage: %s", bic->name, bic->usage);
  goto end;
}

#undef MARKER
