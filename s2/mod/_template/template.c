/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

#include <assert.h>

#include "libs2.h"

static int cb_one( cwal_callback_args const * args,
                   cwal_value **rv ){
  *rv = cwal_value_undefined();
  return cwal_cb_throw(args, CWAL_RC_UNSUPPORTED,
                     "Callback one() is not implemented.");
}


static int cb_two( cwal_callback_args const * args,
                   cwal_value **rv ){
  *rv = cwal_value_undefined();
  return cwal_cb_throw(args, CWAL_RC_UNSUPPORTED,
                     "Callback two() is not implemented.");
}

static int s2_module_init_template( s2_engine * se, cwal_value ** rv ){
  int rc = 0;
  cwal_value * mod = cwal_new_object_value(se->e);
  if(!mod) return CWAL_RC_OOM;
  cwal_value_ref(mod);
  /* From here on, don't use 'return'. Instead,
     set 'rc' and use 'goto end'; */

  /************************************************************/
  /* ... do stuff here... On error, goto end */
  if(rc) goto end;

  /************************************************************/
  /* Install some funcs... */
  {
    s2_func_def const funcs[] = {
      /* Install functions... */
      S2_FUNC2("one", cb_one),
      S2_FUNC2("two", cb_two),
      s2_func_def_empty_m
    };
    rc = s2_install_functions(se, mod, funcs, 0);
    if(rc) goto end;
  }

  /************************************************************/
  /* On error, clean up mod. On success, pass mod on. */
  end:
  if(rc){
    cwal_value_unref(mod);
  }else{
    *rv = mod;
    cwal_value_unhand(mod);
  }
  return rc;
}

S2_MODULE_REGISTER_(template);
