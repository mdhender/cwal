/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

/**
   Impls for the various pragma commands and the info command (which
   was initially a pragma).
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

enum whcl__info_words {
  WHCL__INFO__invalid = 0,
  WHCL__INFO_can_new /* can-new */,
  WHCL__INFO_has_array /* has-array */,
  WHCL__INFO_has_buffer /* has-buffer */,
  WHCL__INFO_has_enum /* has-enum */,
  WHCL__INFO_has_exception /* has-exception */,
  WHCL__INFO_has_function /* has-function */,
  WHCL__INFO_has_hash /* has-hash */,
  WHCL__INFO_has_native /* has-native */,
  WHCL__INFO_has_object /* has-object */,
  WHCL__INFO_has_prototype /* has-prototype */,
  WHCL__INFO_is_array /* is-array */,
  WHCL__INFO_is_bool /* is-bool */,
  WHCL__INFO_is_buffer /* is-buffer */,
  WHCL__INFO_is_container /* is-container */,
  WHCL__INFO_is_declared /* is-declared */,
  WHCL__INFO_is_derefable /* is-derefable */,
  WHCL__INFO_is_double /* is-double */,
  WHCL__INFO_is_enum /* is-enum */,
  WHCL__INFO_is_exception /* is-exception */,
  WHCL__INFO_is_function /* is-function */,
  WHCL__INFO_is_hash /* is-hash */,
  WHCL__INFO_is_integer /* is-integer */,
  WHCL__INFO_is_iterating /* is-iterating */,
  WHCL__INFO_is_iterating_list /* is-iterating-list */,
  WHCL__INFO_is_iterating_props /* is-iterating-props */,
  WHCL__INFO_is_list /* is-list */,
  WHCL__INFO_is_local /* is-local */,
  WHCL__INFO_is_native /* is-native */,
  WHCL__INFO_is_newing /* is-newing */,
  WHCL__INFO_is_number /* is-number */,
  WHCL__INFO_is_numeric /* is-numeric */,
  WHCL__INFO_is_object /* is-object */,
  WHCL__INFO_is_string /* is-string */,
  WHCL__INFO_is_tuple /* is-tuple */,
  WHCL__INFO_is_unique /* is-unique */,
  WHCL__INFO_may_iterate /* may-iterate */,
  WHCL__INFO_may_iterate_list /* may-iterate-list */,
  WHCL__INFO_may_iterate_props /* may-iterate-props */,
  WHCL__INFO_ref_count /* ref-count */,
  WHCL__INFO_type_name /* type-name */
};

/** Types of args accepted by the various info subcommands. */
enum whcl__info_arg_type {
PINFO__ARG_IDENTIFIER = -1,
PINFO__ARG_NONE = 0,
PINFO__ARG_EXPR = 1,
PINFO__ARG_invalid = 99
};
typedef struct {
  enum whcl__info_words type;
  enum whcl__info_arg_type argType;
} whcl__pinfo_word;
static const whcl__pinfo_word whclPragmaInfoWords[] = {
  /*
   ACHTUNG: their order MUST match the entries of
   the whcl__pinfo_words enum because we reference them
   by index that way!
  */
  { WHCL__INFO__invalid,           PINFO__ARG_NONE},
  { WHCL__INFO_can_new,            PINFO__ARG_EXPR},
  { WHCL__INFO_has_array,          PINFO__ARG_EXPR},
  { WHCL__INFO_has_buffer,         PINFO__ARG_EXPR},
  { WHCL__INFO_has_enum,           PINFO__ARG_EXPR},
  { WHCL__INFO_has_exception,      PINFO__ARG_EXPR},
  { WHCL__INFO_has_function,       PINFO__ARG_EXPR},
  { WHCL__INFO_has_hash,           PINFO__ARG_EXPR},
  { WHCL__INFO_has_native,         PINFO__ARG_EXPR},
  { WHCL__INFO_has_object,         PINFO__ARG_EXPR},
  { WHCL__INFO_has_prototype,      PINFO__ARG_EXPR},
  { WHCL__INFO_is_array,           PINFO__ARG_EXPR},
  { WHCL__INFO_is_bool,            PINFO__ARG_EXPR},
  { WHCL__INFO_is_buffer,          PINFO__ARG_EXPR},
  { WHCL__INFO_is_container,       PINFO__ARG_EXPR},
  { WHCL__INFO_is_declared,        PINFO__ARG_IDENTIFIER},
  { WHCL__INFO_is_derefable,       PINFO__ARG_EXPR},
  { WHCL__INFO_is_double,          PINFO__ARG_EXPR},
  { WHCL__INFO_is_enum,            PINFO__ARG_EXPR},
  { WHCL__INFO_is_exception,       PINFO__ARG_EXPR},
  { WHCL__INFO_is_function,        PINFO__ARG_EXPR},
  { WHCL__INFO_is_hash,            PINFO__ARG_EXPR},
  { WHCL__INFO_is_integer,         PINFO__ARG_EXPR},
  { WHCL__INFO_is_iterating,       PINFO__ARG_EXPR},
  { WHCL__INFO_is_iterating_list,   PINFO__ARG_EXPR},
  { WHCL__INFO_is_iterating_props,  PINFO__ARG_EXPR},
  { WHCL__INFO_is_list,            PINFO__ARG_EXPR},
  { WHCL__INFO_is_local,           PINFO__ARG_IDENTIFIER},
  { WHCL__INFO_is_native,          PINFO__ARG_EXPR},
  { WHCL__INFO_is_newing,          PINFO__ARG_EXPR
    /*special case: is-newing requires argType PINFO__ARG_EXPR but
      optionally accepts no argument. */},
  { WHCL__INFO_is_number,          PINFO__ARG_EXPR},
  { WHCL__INFO_is_numeric,         PINFO__ARG_EXPR},
  { WHCL__INFO_is_object,          PINFO__ARG_EXPR},
  { WHCL__INFO_is_string,          PINFO__ARG_EXPR},
  { WHCL__INFO_is_tuple,           PINFO__ARG_EXPR},
  { WHCL__INFO_is_unique,          PINFO__ARG_EXPR},
  { WHCL__INFO_may_iterate,        PINFO__ARG_EXPR},
  { WHCL__INFO_may_iterate_list,    PINFO__ARG_EXPR},
  { WHCL__INFO_may_iterate_props,   PINFO__ARG_EXPR},
  { WHCL__INFO_ref_count,          PINFO__ARG_EXPR},
  { WHCL__INFO_type_name,              PINFO__ARG_EXPR}
};

static whcl__pinfo_word const * whcl__pragma_info_search(char const *s, uint16_t n){
#define THEN(W,K)                                    \
  return (sizeof(W)-1==n && 0==memcmp(s,W,n))       \
    ? &whclPragmaInfoWords[WHCL__INFO_##K] : NULL

  switch(n<30 ? whcl__hash_keyword(s, (uint16_t)n) : 0){
    /* Values generated by kwasher.c */

    case 0x01c70180: THEN("has-hash",has_hash);
    case 0x00e5a16a: THEN("is-bool",is_bool);
    case 0x0e5b5c3e: THEN("is-function",is_function);
    case 0x0396c35c: THEN("is-native",is_native);
    case 0x0e5a375e: THEN("is-declared",is_declared);
    case 0x0396a7f2: THEN("is-buffer",is_buffer);
    case 0x071c45f0: THEN("has-object",has_object);
    case 0x1c71ca3e: THEN("has-function",has_function);
    case 0x01cb52f1: THEN("is-array",is_array);
    case 0x071c5db3: THEN("has-native",has_native);
    case 0x0396cf64: THEN("is-newing",is_newing);
    case 0x00d8923c: THEN("can-new",can_new);
    case 0x0396b518: THEN("is-double",is_double);
    case 0x071c394e: THEN("has-buffer",has_buffer);
    case 0x1cb66b0e: THEN("is-exception",is_exception);
    case 0x00e5a6a8: THEN("is-enum",is_enum);
    case 0x38e3331e: THEN("has-exception",has_exception);
    case 0x72db8e53: THEN("is-iterating-props",is_iterating_props);
    case 0x072d9e02: THEN("is-integer",is_integer);
    case 0x1cb6d173: THEN("is-iterating",is_iterating);
    case 0x038e1b52: THEN("has-array",has_array);
    case 0x38e5dacc: THEN("has-prototype",has_prototype);
    case 0x00e59eaa: THEN("is-hash",is_hash);
    case 0x03f786c2: THEN("type-name",type_name);
    case 0x773fa415: THEN("may-iterate-props",may_iterate_props);
    case 0x1cb50045: THEN("is-derefable",is_derefable);
    case 0x0396aea6: THEN("is-object",is_object);
    case 0x01cb54b4: THEN("is-local",is_local);
    case 0x01c70b15: THEN("has-enum",has_enum);
    case 0x00e5ab62: THEN("is-list",is_list);
    case 0x1cb610a6: THEN("is-container",is_container);
    case 0x01cb7c31: THEN("is-tuple",is_tuple);
    case 0x03e5375c: THEN("ref-count",ref_count);
    case 0x773f8be4: THEN("may-iterate-list",may_iterate_list);
    case 0x0396e15a: THEN("is-number",is_number);
    case 0x72db796c: THEN("is-iterating-list",is_iterating_list);
    case 0x039704ac: THEN("is-string",is_string);
    case 0x0396f8ba: THEN("is-unique",is_unique);
    case 0x072ddfe1: THEN("is-numeric",is_numeric);
    case 0x0ee7e96e: THEN("may-iterate",may_iterate);
    default: return NULL;

  }
#undef THEN
}

static int whcl__pragma_info(whcl__bic const * const bic, whcl__args const * const args,
                             uint16_t argNdx, cwal_value **rv){
  int rc = 0;
  cwal_midsize_t cmdLen = 0;
  char const * cmd;
  whcl__pinfo_word const * iword;
  int buul = -1 /* <0=unknown, 0=false, >0=true */;
  cwal_value * xrv = NULL;
  whcl_stoken const * tArg = NULL;
  cwal_midsize_t tLen = 0;
  char const * tStr;
  cwal_engine * const e = args->el->ec;
  if(args->argc<=argNdx+1){
    return whcl_err_throw(args->el, CWAL_RC_MISUSE,
                          "Expecting subcommand argument for the info pragma.");
  }
  ++argNdx;
  whcl__stoken_set(args->ct, args->argv[argNdx]);
  cmd = whcl_stoken_cstr(args->ct, args->argv[argNdx], &cmdLen, false);
  iword = whcl__pragma_info_search(cmd, cmdLen);
  ++argNdx;
#define ARGNDX(COND,MSG) if(COND) {\
    rc = whcl_err_throw(args->el, CWAL_RC_MISUSE, "pragma %.*s: %s", \
                        (int)cmdLen, cmd, MSG); goto end; } (void)0
  
#define TODO rc = whcl_err_throw(args->el, CWAL_RC_UNSUPPORTED,         \
                               "TODO: %.*s", (int)cmdLen, cmd); goto end
  switch(iword ? iword->argType : PINFO__ARG_invalid){
    case PINFO__ARG_invalid:
      return whcl_err_throw(args->el, CWAL_RC_MISUSE,
                            "Unknown info subcommand: %.*s", (int)cmdLen, cmd);
    case PINFO__ARG_NONE:
      ARGNDX((argNdx<=args->argc), "Got arguments for no-arg pragma.");
      break;
    case PINFO__ARG_IDENTIFIER:
      ARGNDX((argNdx>=args->argc), "Expecting arguments.");
      switch(iword->type){
        case WHCL__INFO_is_local:
        case WHCL__INFO_is_declared:
          tArg = args->argv[argNdx++];
          if(TOK1_T_Identifier!=tArg->ttype){
            rc = whcl_err_throw(args->el, CWAL_RC_TYPE, "Invalid token type for %.*s. "
                                "Expected %s but got %s.", (int)cmdLen, cmd,
                                tok1_t_cstr(TOK1_T_Identifier),
                                tok1_t_cstr(tArg->ttype));
            goto end;
          }
          tStr = whcl_stoken_cstr(args->ct, tArg, &tLen, false);
          buul = !!whcl_var_search(args->el, NULL,
                                   WHCL__INFO_is_declared==iword->type,
                                   tStr, (cwal_int_t)tLen, NULL);
          goto end;
        default: break;
      }
      break;
    case PINFO__ARG_EXPR:
      if(WHCL__INFO_is_newing == iword->type
         && argNdx<=args->argc){
        /* Special case: is-newing w/ no arg assumes `this` as its arg */
        xrv = whcl_var_search_v(args->el, NULL, true,
                                args->el->cache.keyThis, NULL);
        if(!xrv) xrv = cwal_value_undefined();
        break;
      }
      ARGNDX((argNdx!=args->argc-1), "Expecting exactly one argument.");
      tArg = args->argv[argNdx++];
      rc = whcl__eval_token(args->el, args->ct, tArg, 0, &xrv);
      if(rc) goto end;
      else if(!xrv) xrv = cwal_value_undefined();
      break;
  }
  assert(!rc && "Expecting that non-0 RC did 'goto end'");

  switch(iword->type){
#define PRED1(PRED) buul = PRED(xrv); goto end

    case WHCL__INFO__invalid:
    case WHCL__INFO_is_declared:
    case WHCL__INFO_is_local:
      whcl__fatal(CWAL_RC_ASSERT, "This case cannot happen."); break;

      
    case WHCL__INFO_can_new: TODO;
    case WHCL__INFO_has_array: buul = !!cwal_value_array_part(e, xrv); break;
    case WHCL__INFO_has_buffer: buul = !!cwal_value_buffer_part(e, xrv); break;
    case WHCL__INFO_has_enum: TODO;
    case WHCL__INFO_has_exception: buul = !!cwal_value_exception_part(e, xrv); break;
    case WHCL__INFO_has_function: buul = !!cwal_value_function_part(e, xrv); break;
    case WHCL__INFO_has_hash: buul = !!cwal_value_hash_part(e, xrv); break;
    case WHCL__INFO_has_native: buul = !!cwal_value_native_part(e, xrv, NULL); break;
    case WHCL__INFO_has_object: buul = !!cwal_value_object_part(e, xrv); break;
    case WHCL__INFO_has_prototype: buul = !!cwal_value_prototype_get(e, xrv); break;
    case WHCL__INFO_is_array: PRED1(cwal_value_is_array);
    case WHCL__INFO_is_bool: PRED1(cwal_value_is_bool);
    case WHCL__INFO_is_buffer: PRED1(cwal_value_is_buffer);
    case WHCL__INFO_is_container: PRED1(cwal_props_can);
    case WHCL__INFO_is_derefable: TODO;
      switch(cwal_value_type_id(xrv)){
        case CWAL_TYPE_UNDEF: case CWAL_TYPE_NULL: case CWAL_TYPE_BOOL: buul = 0; break;
        default: buul = 1; break;
      }
      break;
    case WHCL__INFO_is_double: PRED1(cwal_value_is_double);
    case WHCL__INFO_is_enum: TODO;
    case WHCL__INFO_is_exception: PRED1(cwal_value_is_exception);
    case WHCL__INFO_is_function: PRED1(cwal_value_is_function);
    case WHCL__INFO_is_hash: PRED1(cwal_value_is_hash);
    case WHCL__INFO_is_integer: PRED1(cwal_value_is_integer);
    case WHCL__INFO_is_iterating:
      buul = cwal_value_is_iterating_props(xrv) || cwal_value_is_iterating_list(xrv); break;
    case WHCL__INFO_is_iterating_list: PRED1(cwal_value_is_iterating_list);
    case WHCL__INFO_is_iterating_props: PRED1(cwal_value_is_iterating_props);
    case WHCL__INFO_is_list:
      buul = cwal_value_is_array(xrv) || cwal_value_is_tuple(xrv); break;
    case WHCL__INFO_is_native: PRED1(cwal_value_is_native);
    case WHCL__INFO_is_newing: buul = whcl_is_newing(xrv); break;
    case WHCL__INFO_is_number: PRED1(cwal_value_is_number);
    case WHCL__INFO_is_numeric:
      if(!(buul = cwal_value_is_number(xrv)) && cwal_value_is_string(xrv)){
        /* check for numeric-format strings. */
        cwal_size_t slen = 0;
        char const * src = cwal_value_get_cstr(xrv, &slen);
        buul =
#if 1
          0==cwal_cstr_to_int(src, slen, NULL)
          || 0==cwal_cstr_to_double(src, slen, NULL);
#else
          tok1_parse_int_cstr(src, slen, NULL)
          || tok1_parse_double_cstr(src, slen, NULL);
#endif
      }
      break;
    case WHCL__INFO_is_object: PRED1(cwal_value_is_object);
    case WHCL__INFO_is_string: PRED1(cwal_value_is_string);
    case WHCL__INFO_is_tuple: PRED1(cwal_value_is_tuple);
    case WHCL__INFO_is_unique: PRED1(cwal_value_is_unique);
    case WHCL__INFO_may_iterate:
      buul = cwal_value_may_iterate_list(xrv)
        || cwal_value_may_iterate(xrv); break;
    case WHCL__INFO_may_iterate_list:
      buul = cwal_value_may_iterate_list(xrv); break;
    case WHCL__INFO_may_iterate_props:
      buul = cwal_value_may_iterate(xrv); break;
    case WHCL__INFO_ref_count:{
      cwal_int_t const n = (cwal_int_t) cwal_value_refcount(xrv);
      cwal_refunref(xrv);
      xrv = cwal_new_integer(e, n);
      if(!xrv){
        rc = CWAL_RC_OOM;
        WHCL__WARN_OOM;
      }
      break;
    }
    case WHCL__INFO_type_name:{
      cwal_value * vTn = cwal_prop_get_v(xrv, args->el->cache.keyTypename);
      if(!vTn){
        cwal_size_t nlen = 0;
        char const * name = cwal_value_type_name2(xrv, &nlen);
        vTn = nlen
          ? cwal_new_string_value(e, name, nlen)
          : cwal_value_undefined();
        if(!vTn){
          rc = CWAL_RC_OOM;
          WHCL__WARN_OOM;
          break;
        }
      }
      xrv = vTn;
      break;
    }
  }/*switch(iword->type)*/

#undef PRED1
#undef TODO
#undef ARGNDX
  end:
  if(bic){/*unused*/}
  if(0==rc){
    if(buul>=0){
      cwal_refunref(xrv);
      xrv = cwal_new_bool(buul>0);
    }
    if(rv){
      if(xrv){
        cwal_ref(xrv);
        *rv = xrv;
        cwal_value_unhand(xrv);
      }else{
        cwal_refunref(xrv);
        *rv = cwal_value_undefined();
      }
    }else{
      cwal_refunref(xrv);
    }
  }else if(xrv){
    cwal_refunref(xrv);
  }
  return rc;
}

int whcl__bic_f_info(whcl__bic const * const bic,
                     whcl__args const * const args,
                     cwal_value **rv){
  return whcl__pragma_info(bic, args, 0, rv);
}



enum whcl__pragma_e {
  WHCL__PRAGMA__invalid = 0,
  WHCL__PRAGMA___debug /* __debug */,
  WHCL__PRAGMA_dump_tokens /* dump-tokens */,
  WHCL__PRAGMA_memory_metrics /* memory-metrics */,
  WHCL__PRAGMA_trace_assert /* trace-assert */,
  WHCL__PRAGMA_vacuum /* vacuum */
};


static enum whcl__pragma_e whcl__pragma_cmd(whcl__args const * const args){
  cwal_midsize_t n = 0;
  char const * const s =
    whcl_stoken_cstr(args->ct, args->argv[1], &n, false);
  switch(n<20 ?  whcl__hash_keyword(s, (uint16_t)n) : 0){
#define THEN(K,W) \
      return (sizeof(W)-1==n && 0==memcmp(s,W,n)) \
    ? WHCL__PRAGMA_##K : WHCL__PRAGMA__invalid

    case 0x00cffbfc: THEN(__debug,"__debug");
    case 0x0db04e36: THEN(dump_tokens,"dump-tokens");
    case 0x77594cf9: THEN(memory_metrics,"memory-metrics");
    case 0x1fba3e78: THEN(trace_assert,"trace-assert");
    case 0x00811d77: THEN(vacuum,"vacuum");
    default: return WHCL__PRAGMA__invalid;

#undef THEN
  }
}

/** Handler for the pragma command. */
int whcl__bic_f_pragma(whcl__bic const * const bic,
                       whcl__args const * const args,
                       cwal_value **rv){
  if(rv) *rv = cwal_value_undefined();
  assert(args->argc>1);
  //whcl__dump_stok(args->ct, args->argv[0], "arg 0");
  //whcl__dump_stok(args->ct, args->argv[args->argc-1], "arg N");
  if(bic){/*unused*/}
  switch(whcl__pragma_cmd(args)){
    case WHCL__PRAGMA___debug:{
      if(args->argc>3) goto err_extra_args;
      int rc = 0;
      whcl_stoken const * tArg = args->argc>2 ? args->argv[2] : NULL;
      bool on = tArg==NULL;
      if(tArg){
        cwal_value * v = NULL;
        rc = whcl__eval_token(args->el, args->ct, tArg, 0, &v);
        if(0==rc){
          on = cwal_value_get_bool(v);
          cwal_refunref(v);
          whcl_feature_flag_set(args->el, WHCL_FEATURE_F_DEBUG_BLOCK, on);
          *rv = cwal_new_bool(on);
        }
      }else{
        *rv = cwal_new_bool(args->el->flags.enableDebugBlock);
      }
      return rc;
      break;
    }
    case WHCL__PRAGMA_dump_tokens:{
      /* [-v] [-f file | tokens...] */
      int rc = 0;
      whcl_stoken const * oldPos = whcl__script_token(args->ct);
      whcl_script ctOther = whcl__script_empty;
      whcl_script * dumpWho = args->ct;
      uint32_t flags = 0;
      uint16_t ndx = 2;
      cwal_buffer file = cwal_buffer_empty;
      whcl_stoken const * tArg;
      while( (tArg = whcl__args_hasflag(args, ndx)) ){
        ++ndx;
        if(2==tArg->length
           && whcl_stoken_matches(args->ct, tArg, "-v", tArg->length)){
          flags |= WHCL_DUMP_TOKENS_VERBOSE;
        }else if(4==tArg->length
           && whcl_stoken_matches(args->ct, tArg, "-eof", tArg->length)){
          flags |= WHCL_DUMP_TOKENS_EOFS;
        }else if(2==tArg->length
           && whcl_stoken_matches(args->ct, tArg, "-m", tArg->length)){
          flags |= WHCL_DUMP_TOKENS_METRICS;
        }
        else if(ndx < args->argc
                 && whcl_stoken_matches(args->ct, tArg, "-f", 2)){
          /* -f filename. FIXME: support __FILE and such. Doing this
             requires using whcl__create_eval() on the token then
             using that value's string form. */
          whcl_stoken const * const tName = args->argv[ndx++];
          cwal_buffer * const esc = cwal_buffer_reuse(&args->el->escBuf);
          cwal_midsize_t tlen = 0;
          char const *tStr = whcl_stoken_cstr(args->ct, tName, &tlen, true);
          rc = cwal_buffer_append(args->el->ec, esc, tStr, tlen);
          if(rc) goto dump_end;
          rc = cwal_buffer_fill_from_filename(args->el->ec, &file,
                                              cwal_buffer_cstr(esc,NULL));
          if(rc){
            rc = whcl_err_throw(args->el, rc, "Error %s loading file '%b'.",
                                cwal_rc_cstr(rc), esc);
            goto dump_end;
          }
          whcl__script_init(args->el->ec, &ctOther)
            /* Doing this before whcl__compile_buffer() keeps that function
               from re-doing it, which allows us to set ctOther's name. */;
          ctOther.name = "pragma dump-tokens";
          rc = whcl__compile_buffer(args->el, &ctOther, &file, 0);
          if(rc) goto dump_end;
          dumpWho = &ctOther;
        }else{
          rc = whcl_err_throw(args->el, CWAL_RC_MISUSE,
                              "Invalid flag for %s: %.*s",
                              args->commandName,
                              (int)tArg->length,
                              whcl_stoken_cstr(args->ct, tArg, NULL, false));
          goto dump_end;
        }
      }
      if(dumpWho==args->ct && args->argc>ndx){
        whcl__stoken_set(args->ct, args->argv[ndx++]);
        flags |= WHCL_DUMP_TOKENS_TO_EOX
          | WHCL_DUMP_TOKENS_NO_REWIND;
        ndx = args->argc;
      }
      if(args->argc==ndx){
        whcl_dump_tokens(args->el, dumpWho, flags);
      }
      dump_end:
      whcl__stoken_set(args->ct, oldPos);
      cwal_buffer_clear(args->el->ec, &file);
      whcl__script_finalize(&ctOther);
      if(args->argc!=ndx) goto err_extra_args;
      return rc;
    }/*dump-tokens*/
    case WHCL__PRAGMA_memory_metrics:
      cwal_dump_allocation_metrics( args->el->ec );
      return 0;
    case WHCL__PRAGMA_trace_assert:{
      /* [value] */
      cwal_int_t iv = 0;
      if(args->argc>3) goto err_extra_args;
      else if(3==args->argc){
        if(!whcl_stoken_parse_int(args->ct, args->argv[2], &iv) || iv<0){
          iv = 0;
        }
        whcl_feature_flag_set(args->el, WHCL_FEATURE_F_TRACE_ASSERT, iv & 0x01);
        whcl_feature_flag_set(args->el, WHCL_FEATURE_F_TRACE_AFFIRM, iv & 0x02);
      }
      iv = 0 | (args->el->flags.traceAssert ? 1 : 0)
        | (args->el->flags.traceAffirm ? 2 : 0);
      *rv = cwal_new_integer(args->el->ec, iv & 0x03);
      if(*rv) return 0;
      else{
        WHCL__WARN_OOM;
        return CWAL_RC_OOM;
      }
    }
    case WHCL__PRAGMA_vacuum:{
      /* [-r = recursive] */
      enum WhclSweepModes mode = SweepMode_Vacuum;
      uint16_t ndx = 2;
      while(ndx<args->argc){
        if(whcl__args_next_matches(args, &ndx, "-r")){
          mode = SweepMode_VacuumRecursive;
          ++ndx;
        }else{
          goto err_extra_args;
        }
      }
      whcl__sweep_impl(args->el, mode, true);
      *rv = cwal_value_undefined();
      return 0;
    }/*vacuum*/
    case WHCL__PRAGMA__invalid:{
      whcl_stoken const * const cmd = args->argv[1];
      whcl__script_errtoken_set(args->ct, cmd);
      return whcl__script_throw(args->ct, CWAL_RC_NOT_FOUND,
                               "Unknown/unimplemented pragma: %.*s",
                               (int)cmd->length,
                               whcl_stoken_cstr(args->ct, cmd, false, NULL));
    }
  }
  err_extra_args:
  whcl__script_errtoken_set(args->ct, args->argv[1]);
  return whcl__script_throw(args->ct, CWAL_RC_MISUSE,
                           "Extra arguments at end of pragma.");
}/*whcl__bic_f_pragma()*/

#undef MARKER
