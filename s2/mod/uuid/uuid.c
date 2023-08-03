/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#include "libs2.h"
#if !defined(_POSIX_C_SOURCE)
#define _POSIX_C_SOURCE 200112L
#endif
#if !defined(_XOPEN_SOURCE)
#define _XOPEN_SOURCE 600
#endif
#if !defined(_BSD_SOURCE)
#define _BSD_SOURCE
#endif

#include <assert.h>
#include <stdlib.h>
#include "whuuid.h"

static whuuid_rng whuuid_rng_shared = {
whuuid_lc_rand/*rand*/,
NULL/*cleanup*/,
NULL/*impl*/,
{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}/*distribution*/
};


static int s2_cb_uuid( cwal_callback_args const * args,
                       cwal_value **rv ){
  if(args->argc){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting no arguments.");
  }else{
    whuuid_t u = whuuid_t_empty;
    whuuid_rng * st = &whuuid_rng_shared;
    char buf[whuuid_length_canonical+1] = {0};
    whuuid_fill_rand( &u, st );
    whuuid_to_string( &u, buf );
    *rv = cwal_new_string_value(args->engine, buf,
                                (cwal_size_t)whuuid_length_canonical);
    return *rv ? 0 : CWAL_RC_OOM;
  }
}

static int s2_module_init_uuid( s2_engine * se, cwal_value ** rv ){
  int rc = 0;
  cwal_value * mod;
  static int once = 0;
  if(!once){
    cwal_hash_t const h =
      cwal_hash_bytes(se, sizeof(s2_engine))
      ^ cwal_hash_bytes(se->e, sizeof(cwal_engine))
      /* This "should" provide us with reasonable entropy
         for our purposes... It's worked fine on my tests
         of some 13M UUIDs, generated in batches of 1M, 2M,
         and 2x5M. */
      ;
    whuuid_rng_shared.impl = (void*)h;
    once = 1;
  }
  
  mod = cwal_new_function_value( se->e, s2_cb_uuid, 0, 0, 0);
  cwal_value_ref(mod);
  rc = mod ? 0 : CWAL_RC_OOM;
  /* Potential TODO: return a factory function instead which
     allows the caller to provide their own RNG. That's
     complete overkill for now.
  */

  if(rc){
    cwal_value_unref(mod);
  }else{
    *rv = mod;
    cwal_value_unhand(mod);
  }
  return rc;
}

S2_MODULE_REGISTER_(uuid);
