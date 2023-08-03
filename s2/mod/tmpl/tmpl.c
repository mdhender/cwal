/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

#include <assert.h>

#include "libs2.h"

static int s2_module_init_tmpl( s2_engine * se, cwal_value ** rv ){
  int rc = 0;
  cwal_value * mod;
  mod = cwal_new_function_value(se->e, s2_cb_tmpl_to_code, 0, 0, 0);
  if(mod){
    *rv = mod;
  }else{
    rc = CWAL_RC_OOM;
  }
  return rc;
}

S2_MODULE_REGISTER_(tmpl);
