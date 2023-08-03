/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

/**
   Impls for the exception prototype.
*/
#include "internal.h"
#include <assert.h>

static int whcl__cb_exception_ctor( cwal_callback_args const * args,
                                    cwal_value **rv ){
  int rc = 0;
  cwal_value * xv;
  if(args->argc<1 || args->argc>2){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "Expecting (value) or (int, value) arguments.");
  }
  int code = CWAL_RC_EXCEPTION;
  if(2==args->argc){
    if(cwal_value_is_string(args->argv[0])){
      cwal_size_t nStr = 0;
      char const * str = cwal_value_get_cstr(args->argv[0], &nStr);
      whcl_cstr_to_rc(str, (cwal_int_t)nStr, &code);
    }else{
      code = cwal_value_get_integer(args->argv[0]);
    }
    if(0==code) code = CWAL_RC_EXCEPTION;
  }
  xv = cwal_new_exception_value(args->engine, code,
                                args->argv[args->argc>1 ? 1 : 0]);
  if(xv){
    whcl_engine * const el = whcl_engine_from_args(args);
    assert(el);
    *rv = xv;
    whcl__annotate_exception(el, xv);
  }else{
    WHCL__WARN_OOM;
    rc = CWAL_RC_OOM;
  }
  return rc;
}

static int whcl__cb_exception_code_string( cwal_callback_args const * args,
                                           cwal_value **rv ){
  int rc = 0;
  cwal_value * v;
  v = args->argc ? args->argv[0] : cwal_prop_get(args->self, "code", 4);
  *rv = NULL;
  if(v){
    cwal_size_t vstrLen = 0;
    char const * const vstr = cwal_value_get_cstr(v, &vstrLen);
    if(vstr){ /* If passed a string, try (the hard way) to
                 find the integer code. */
      int code = 0;
      if(whcl_cstr_to_rc(vstr, (cwal_int_t)vstrLen, &code)){
        if(! (*rv = cwal_new_integer(args->engine, (cwal_int_t)code)) ){
          WHCL__WARN_OOM;
          rc = CWAL_RC_OOM;
        }
      }
      if(!rc && !*rv) *rv = cwal_value_undefined();
    }else{
      /* Assume the code is an integer and get its string
         form. */
      cwal_int_t const code = cwal_value_get_integer(v);
      char const * str = cwal_rc_cstr2((int)code);
      *rv = str
        ? cwal_new_xstring_value(args->engine, str,
                                 cwal_strlen(str))
        : cwal_value_null();
      if(str && !*rv){ WHCL__WARN_OOM; rc = CWAL_RC_OOM; }
    }
  }else{
    /* No arg or "code" property. */
    *rv = cwal_value_undefined();
  }
  return rc;
}

cwal_value * whcl_prototype_exception(whcl_engine * const el){
  int rc = 0;
  cwal_value * ctor;
  cwal_value * proto;
  proto = cwal_prototype_base_get( el->ec, CWAL_TYPE_EXCEPTION );
  if(proto
     || !whcl_prototype_object(el) /* timing hack */
     ) return proto;
  ctor = cwal_new_function_value(el->ec, whcl__cb_exception_ctor, NULL,
                                 NULL, NULL);
  proto = ctor ? cwal_new_object_value(el->ec) : NULL;
  if(!ctor){
    WHCL__WARN_OOM;
    rc = CWAL_RC_OOM;
    goto end;
  }
  cwal_value_prototype_set(ctor, proto);
  cwal_ref(ctor);
  rc = whcl__install_into_whcl(el, "Exception", ctor);
  cwal_unref(ctor) /* on success ^^^, the core now owns a ref */;
  if(rc) goto end;
  cwal_ref(proto);
  rc = cwal_prototype_base_set(el->ec, CWAL_TYPE_EXCEPTION, proto );
  cwal_unref(proto) /* on success ^^^, the core now owns a ref */;
  if(!rc) rc = whcl__prototype_stash(el, "Exception", proto);
  if(rc) goto end;
  assert(proto == cwal_prototype_base_get(el->ec, CWAL_TYPE_EXCEPTION));
  {
    whcl_func_def const funcs[] = {
      WHCL_FUNC2("code-string", whcl__cb_exception_code_string),
      whcl_func_def_empty_m
    };
    rc = whcl_install_functions(el, proto, funcs, 0);
    //if(!rc) rc = s2_ctor_callback_set(el, proto, whcl__cb_buffer_ctor);
    //if(0==rc) rc = whcl_install_command_cb(el, proto);
    if(rc) goto end;
  }
  end:
  return rc ? NULL : proto;
}
