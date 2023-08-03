/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#include "libs2.h"
#include <assert.h>
#include "whsha1.h"
#include "whsha3.h"

static int s2_cb_sha1( cwal_callback_args const * args,
                       cwal_value **rv ){
  whsha1_cx cx;
  uint16_t i;
  if(!args->argc){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting at least one string or buffer value to hash.");
  }
  whsha1_init(&cx);
  for(i = 0; i < args->argc; ++i){
    cwal_size_t zLen = 0;
    char const * z = cwal_value_get_cstr(args->argv[i], &zLen);
    if(!z){
      return cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "Expecting only string or Buffer arguments.");
    }
    if((cwal_size_t)((unsigned int)zLen) != zLen){
      return cwal_cb_throw(args, CWAL_RC_RANGE,
                         "Numeric overflow in cwal_size_t/unsigned int conversion.");
    }      
    whsha1_update( &cx, z, (unsigned int)zLen );
  }
  whsha1_end(&cx);
  *rv = cwal_new_string_value(args->engine, (char const *)cx.hex, 40);
  return *rv ? 0 : CWAL_RC_OOM;
}

static int s2_cb_sha3_impl( cwal_callback_args const * args,
                            cwal_value **rv,
                            enum whsha3_hash_size hSize,
                            uint16_t skipArgs ){
  whsha3_cx cx;
  uint16_t i;
  whsha3_init(&cx, hSize);
  for(i = skipArgs; i < args->argc; ++i){
    cwal_size_t zLen = 0;
    char const * z = cwal_value_get_cstr(args->argv[i], &zLen);
    if(!z){
      return cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "Expecting only string or Buffer arguments.");
    }
    if((cwal_size_t)((unsigned int)zLen) != zLen){
      return cwal_cb_throw(args, CWAL_RC_RANGE,
                         "Numeric overflow in cwal_size_t/unsigned int conversion.");
    }      
    whsha3_update( &cx, z, (unsigned int)zLen );
  }
  whsha3_end(&cx);
  *rv = cwal_new_string_value(args->engine, (char const *)cx.hex,
                              (cwal_size_t)((int)hSize/4));
  return *rv ? 0 : CWAL_RC_OOM;
}

static int s2_cb_sha3( cwal_callback_args const * args, cwal_value **rv ){
  enum whsha3_hash_size sz = WHSHA3_256;
  uint16_t skipArgs = 0;
  if(args->argc>0 && cwal_value_is_integer(args->argv[0])){
    sz = whsha3_hash_size_for_int( (int)cwal_value_get_integer(args->argv[0]) );
    if(WHSHA3_INVALID == sz){
      return cwal_cb_throw(args, CWAL_RC_RANGE,
                         "SHA3 hash size must be an increment of 32 in the range (128..512).");
    }
    ++skipArgs;
  }
  if(args->argc < skipArgs+1){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting at least one string or buffer value to hash.");
  }else{
    return s2_cb_sha3_impl( args, rv, sz, skipArgs );
  }
}

static int s2_module_init_hashing( s2_engine * se, cwal_value ** rv ){
  int rc = 0;
  cwal_value * mod;
  mod = cwal_new_object_value( se->e );
  if(!mod) return CWAL_RC_OOM;
  cwal_value_ref(mod);
  cwal_value_prototype_set(mod, 0);
  /* Install functions... */
  {
      s2_func_def const funcs[] = {
      S2_FUNC2("sha3", s2_cb_sha3),
      S2_FUNC2("sha1", s2_cb_sha1),
      s2_func_def_empty_m
      };
      rc = s2_install_functions(se, mod, funcs, 0);
  }

  if(rc){
    cwal_value_unref(mod);
  }else{
    *rv = mod;
    cwal_value_unhand(mod);
  }
  return rc;
}

S2_MODULE_REGISTER_(hashing);
