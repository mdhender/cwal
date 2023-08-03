/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#include "libs2.h"

static int s2_module_init_io( s2_engine * se, cwal_value ** rv ){
  int rc;
  cwal_value * mod = cwal_new_object_value(se->e);
  if(!mod) return CWAL_RC_OOM;
  cwal_value_ref(mod);
  rc = s2_install_io( se, mod, 0 );
  if(rc){
      cwal_value_unref(mod);
  }else{
      cwal_value_unhand(mod);
      *rv = mod;
  }
  return rc;
}
S2_MODULE_REGISTER_(io);
