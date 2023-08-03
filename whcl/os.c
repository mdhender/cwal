/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#include "internal.h"
#include <stdlib.h>

static int whcl__cb_getenv( cwal_callback_args const * args,
                      cwal_value **rv ){
  cwal_size_t len = 0;
  char const * key = args->argc
    ? cwal_value_get_cstr(args->argv[0], &len)
    : 0;
  if(!key){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting a string argument.");
  }else{
    char const * val = getenv(key);
    *rv = val
      ? cwal_new_string_value(args->engine, val, cwal_strlen(val))
      /* ^^^ we "could" use x-strings and save a few bytes
         of memory, but if we add setenv() we'd likely shoot
         ourselves in the foot here.
      */
      : (args->argc>1
         ? args->argv[1]
         : cwal_value_undefined());
    return *rv ? 0 : CWAL_RC_OOM;
  }
}

int whcl__install_os( whcl_engine * const el){

  cwal_value * sub = NULL;
  cwal_value * const tgt = el->cache.vWhcl;
  char const * name = "os";
  int rc;
  if(el->cache.installAPI & WHCL__INSTALL_API_os) return 0;
  rc = whcl__install_sub(el, tgt, name, true, &sub);
  if(0==rc){
    whcl_func_def funcs[] = {
      WHCL_FUNC2("getenv", whcl__cb_getenv),
      whcl_func_def_empty_m
    };
    rc = whcl_install_functions(el, sub, funcs, 0);
  }
  if(0==rc) el->cache.installAPI |= WHCL__INSTALL_API_os;
  return rc;
}
