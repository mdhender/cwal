/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

#include "libs2.h"
#include "felta.h"
#include <assert.h>

/* Only for debuggering... */
#if 0
#include <stdio.h>
#define MARKER(pfexp)                                               \
  do{ printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__);   \
    printf pfexp;                                                   \
  } while(0)
#else
#define MARKER(X) (void)0
#endif

static int cb_fsl_delta_create( cwal_callback_args const * args,
                                cwal_value **rv ){
  char const * s1 = NULL;
  char const * s2 = NULL;
  cwal_size_t len1, len2;
  cwal_buffer * cb;
  int rc;
  if(args->argc>1){
    s1 = cwal_value_get_cstr(args->argv[0], &len1);
    s2 = cwal_value_get_cstr(args->argv[1], &len2);
  }
  if(!s1 || !s2){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting two string/buffer arguments.");
  }
  cb = cwal_new_buffer(args->engine, len2+61);
  if(!cb) return CWAL_RC_OOM;
  assert(cb->capacity > (len2+60));
  rc = felta_create( (unsigned char const *)s1, (unsigned int)len1,
                     (unsigned char const *)s2, (unsigned int)len2,
                     cb->mem);
  if(rc < 0){
    rc = cwal_cb_throw(args, rc,
                     "Delta creation failed with code %d (%s).",
                     rc, felta_rc_cstr(rc));
  }else{
    cb->used = (cwal_size_t)rc;
    rc = 0;
  }
  if(!rc){
    cwal_size_t const spare = cb->capacity - cb->used;
    if(spare > 50/*arbitrary*/
       && spare >= cb->capacity / 5/*arbitrary ~20%*/){
      /* If spare capacity is "significant", shrink the buffer to fit. */ 
      MARKER(("Shrinking delta buffer. Capacity=%d, used=%d\n", (int)cb->capacity, (int)cb->used));
      rc = cwal_buffer_resize(args->engine, cb, cb->used);
    }
  }
  if(rc) cwal_value_unref(cwal_buffer_value(cb));
  else *rv = cwal_buffer_value(cb);
  return rc;
}

static int cb_fsl_delta_applied_len( cwal_callback_args const * args,
                                     cwal_value **rv ){
  char const * src;
  cwal_size_t srcLen;
  int appliedLen = 0;
  src = (args->argc>0)
    ? cwal_value_get_cstr(args->argv[0], &srcLen)
    : NULL;
  if(!src){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting one delta string/buffer "
                       "argument.");
  }
  appliedLen = felta_output_size((unsigned char const *)src,
                                 (unsigned)srcLen );
  if(appliedLen<0){
    if(args->argc>1 && !cwal_value_get_bool(args->argv[1])){
      *rv = cwal_value_undefined();
      return 0;
    }else{
      return cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "Input does not appear to be a "
                         "delta. Error #%d (%s).",
                         appliedLen, felta_rc_cstr(appliedLen));
    }
  }else{
    *rv = cwal_new_integer(args->engine, (cwal_int_t)appliedLen);
    return *rv ? 0 : CWAL_RC_OOM;
  }
}

static int cb_fsl_delta_isa( cwal_callback_args const * args,
                             cwal_value **rv ){
  char const * src;
  cwal_size_t srcLen;
  int appliedLen = 0;
  src = (args->argc>0)
    ? cwal_value_get_cstr(args->argv[0], &srcLen)
    : NULL;
  if(1 != args->argc){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting one argument.");
  }
  else if(!src){
    *rv = cwal_value_false();
    return 0;
  }
  appliedLen = felta_output_size((unsigned char const *)src,
                                 (unsigned)srcLen );
  *rv = appliedLen<0
    ? cwal_value_false()
    : cwal_value_true();
  return 0;
}


static int cb_fsl_delta_apply( cwal_callback_args const * args,
                               cwal_value **rv ){
  char const * s1 = NULL;
  char const * s2 = NULL;
  char const * src;
  char const * delta;
  char const * zErr = 0;
  cwal_size_t len1, len2, srcLen, dLen;
  int appliedLen = 0;
  cwal_buffer * cb;
  int rc;
  if(args->argc>1){
    s1 = cwal_value_get_cstr(args->argv[0], &len1);
    s2 = cwal_value_get_cstr(args->argv[1], &len2);
  }
  if(!s1 || !s2){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting two string/buffer arguments.");
  }
  appliedLen = felta_output_size((unsigned char const *)s2,
                                 (unsigned int)len2);
  if(appliedLen>=0){
    src = s1;
    srcLen = len1;
    delta = s2;
    dLen = len2;
  }else{ /* Check if the user perhaps swapped the args. */
    appliedLen = felta_output_size((unsigned char const *)s1,
                                   (unsigned)len1);
    if(appliedLen<0){
      return cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "Expecting a delta string/buffer as one "
                         "of the first two arguments.");
    }
    src = s2;
    srcLen = len2;
    delta = s1;
    dLen = len1;
  }
  cb = cwal_new_buffer(args->engine, (cwal_size_t)appliedLen+1);
  if(!cb) return CWAL_RC_OOM;
  cwal_value_ref(cwal_buffer_value(cb));
#if 1
  MARKER(("srcLen=%d, dLen=%d, appliedLen=%d\n", (int)srcLen, (int)dLen, (int)appliedLen));
#endif
  assert(cb->capacity > (cwal_size_t)appliedLen);
  rc = felta_apply( (unsigned char const *)src, (unsigned)srcLen,
                    (unsigned char const *)delta, (unsigned)dLen,
                    cb->mem, &zErr);
  if(rc>=0){
    cb->used = (cwal_size_t)rc;
    cb->mem[cb->used] = 0;
    rc = 0;
    *rv = cwal_buffer_value(cb);
    cwal_value_unhand(*rv);
  }else{
    rc = cwal_cb_throw(args, rc,
                     "Application of delta failed with "
                     "code #%d: %s", rc,
                     zErr ? zErr : felta_rc_cstr(rc));
    cwal_value_unref(cwal_buffer_value(cb));
  }
  return rc;
}


static int s2_module_init_felta( s2_engine * se, cwal_value ** rv ){
  int rc = 0;
  cwal_value * mod = cwal_new_object_value(se->e);
  if(!mod) return CWAL_RC_OOM;
  cwal_value_ref(mod);
  cwal_value_prototype_set(mod, NULL);
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
      S2_FUNC2("isDelta", cb_fsl_delta_isa),
      S2_FUNC2("appliedLength", cb_fsl_delta_applied_len),
      S2_FUNC2("apply", cb_fsl_delta_apply),
      S2_FUNC2("create", cb_fsl_delta_create),
      s2_func_def_empty_m
    };
    rc = s2_install_functions(se, mod, funcs, 0);
    if(rc) goto end;
  }

  end:
  if(rc){
    cwal_value_unref(mod);
  }else{
    *rv = mod;
    cwal_value_unhand(mod);
  }
  return rc;
}

S2_MODULE_REGISTER_(felta);
