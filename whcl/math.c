/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

/**
   C99 <math.h> bindings for whcl.
*/
#include <assert.h>
#include <math.h>

#include "libwhcl.h"

/** Funcs with signature: double f(double, double) */
#define CB_D_D_D(NAME)                                                  \
  static int cb_##NAME( cwal_callback_args const * args, cwal_value **rv ){    \
    cwal_double_t rd;                                              \
    if(2!=args->argc) return cwal_cb_throw(args, CWAL_RC_MISUSE, "Expecting two arguments."); \
    rd = (cwal_double_t)NAME(cwal_value_get_double(args->argv[0]), \
                             cwal_value_get_double(args->argv[1])); \
    *rv = cwal_new_double(args->engine, rd); \
    return *rv ? 0 : CWAL_RC_OOM;           \
}

CB_D_D_D(atan2)
CB_D_D_D(fmod)
CB_D_D_D(pow)
#undef CB_D_D_D

/** Funcs with signature: double f(double) */
#define CB_D_D(NAME)                                              \
  static int cb_##NAME( cwal_callback_args const * args, cwal_value **rv ){    \
    cwal_double_t rd;                                              \
    if(1!=args->argc) return cwal_cb_throw(args, CWAL_RC_MISUSE, "Expecting one argument."); \
    rd = (cwal_double_t)NAME(cwal_value_get_double(args->argv[0])); \
    *rv = cwal_new_double(args->engine, rd); \
    return *rv ? 0 : CWAL_RC_OOM;           \
}

CB_D_D(acos)
CB_D_D(asin)
CB_D_D(atan)
CB_D_D(ceil)
CB_D_D(cos)
CB_D_D(cosh)
CB_D_D(exp)
CB_D_D(fabs)
CB_D_D(floor)
CB_D_D(log)
CB_D_D(log10)
CB_D_D(sin)
CB_D_D(sinh)
CB_D_D(sqrt)
CB_D_D(tanh)
#undef CB_D_D

/** double ldexp(double, int) */
static int cb_ldexp( cwal_callback_args const * args, cwal_value **rv ){
  cwal_double_t rd;
  if(2!=args->argc) return cwal_cb_throw(args, CWAL_RC_MISUSE,
                                       "Expecting two arguments.");
  rd = (cwal_double_t)ldexp(cwal_value_get_double(args->argv[0]),
                            cwal_value_get_integer(args->argv[1]));
  *rv = cwal_new_double(args->engine, rd);
  return *rv ? 0 : CWAL_RC_OOM;
}

/**
   C: x modf(double y, int *z)

   script: tuple[z, x] modf(y)
*/
static int cb_modf( cwal_callback_args const * args, cwal_value **rv ){
  cwal_double_t rd, di = 0;
  cwal_tuple * t;
  cwal_value * tv = 0;
  int rc = 0;
  if(1!=args->argc) return cwal_cb_throw(args, CWAL_RC_MISUSE,
                                       "Expecting one argument.");
  rd = (cwal_double_t)modf(cwal_value_get_double(args->argv[0]),
                           &di);
  t = cwal_new_tuple(args->engine, 2);
  if(!t) goto oom;
  else{
    cwal_value * v;
    tv = cwal_tuple_value(t);
    cwal_value_ref(tv);

    v = cwal_new_integer(args->engine, (cwal_int_t)di);
    if(!v) goto oom;
    cwal_value_ref(v);
    rc = cwal_tuple_set(t, 0, v);
    cwal_value_unref(v);
    assert(!rc)/*doesn't allocate - cannot fail*/;

    v = cwal_new_double(args->engine, rd);
    if(!v) goto oom;
    cwal_value_ref(v);
    rc = cwal_tuple_set(t, 1, v);
    cwal_value_unref(v);
    assert(!rc)/*doesn't allocate - cannot fail*/;

    cwal_value_unhand(tv);
    *rv = tv;
    return 0;
  }
  oom:
  cwal_value_unref(tv);
  return CWAL_RC_OOM;
}

/**
   C: x frexp(double y, int *z)

   script: tuple[z, x] frexp(y)
*/
static int cb_frexp( cwal_callback_args const * args, cwal_value **rv ){
  cwal_double_t rd;
  int di = 0;
  cwal_tuple * t;
  cwal_value * tv = 0;
  int rc = 0;
  if(1!=args->argc) return cwal_cb_throw(args, CWAL_RC_MISUSE,
                                       "Expecting one argument.");
  rd = (cwal_double_t)frexp(cwal_value_get_double(args->argv[0]),
                            &di);
  t = cwal_new_tuple(args->engine, 2);
  if(!t) goto oom;
  else{
    cwal_value * v;
    tv = cwal_tuple_value(t);
    cwal_value_ref(tv);

    v = cwal_new_integer(args->engine, (cwal_int_t)di);
    if(!v) goto oom;
    cwal_value_ref(v);
    rc = cwal_tuple_set(t, 0, v);
    cwal_value_unref(v);
    assert(!rc)/*doesn't allocate - cannot fail*/;

    v = cwal_new_double(args->engine, rd);
    if(!v) goto oom;
    cwal_value_ref(v);
    rc = cwal_tuple_set(t, 1, v);
    cwal_value_unref(v);
    assert(!rc)/*doesn't allocate - cannot fail*/;

    cwal_value_unhand(tv);
    *rv = tv;
    return 0;
  }
  oom:
  cwal_value_unref(tv);
  return CWAL_RC_OOM;
}

/* bool isnan(double) */
static int cb_isnan( cwal_callback_args const * args,
                     cwal_value **rv ){
  if(1!=args->argc){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting one argument.");
  }
  *rv = isnan(cwal_value_get_double(args->argv[0]))
    ? cwal_value_true()
    : cwal_value_false();
  return 0;
}

/* bool isinf(double) */
static int cb_isinf( cwal_callback_args const * args,
                     cwal_value **rv ){
  if(1!=args->argc){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting one argument.");
  }
  *rv = isinf(cwal_value_get_double(args->argv[0]))
    ? cwal_value_true()
    : cwal_value_false();
  return 0;
}

int whcl__install_math( whcl_engine * const el, cwal_value ** rv ){
  int rc = 0;
  cwal_value * mod = cwal_new_object_value(el->ec);
  if(!mod) return CWAL_RC_OOM;
  cwal_ref(mod);

  {
    whcl_func_def const funcs[] = {
    WHCL_FUNC2("tanh", cb_tanh),
    WHCL_FUNC2("sqrt", cb_sqrt),
    WHCL_FUNC2("sinh", cb_sinh),
    WHCL_FUNC2("sin", cb_sin),
    WHCL_FUNC2("pow", cb_pow),
    WHCL_FUNC2("modf", cb_modf),
    WHCL_FUNC2("log10", cb_log10),
    WHCL_FUNC2("log", cb_log),
    WHCL_FUNC2("ldexp", cb_ldexp),
    WHCL_FUNC2("isnan", cb_isnan),
    WHCL_FUNC2("isinf", cb_isinf),
    WHCL_FUNC2("frexp", cb_frexp),
    WHCL_FUNC2("fmod", cb_fmod),
    WHCL_FUNC2("floor", cb_floor),
    WHCL_FUNC2("fabs", cb_fabs),
    WHCL_FUNC2("exp",cb_exp),
    WHCL_FUNC2("cosh", cb_cosh),
    WHCL_FUNC2("cos", cb_cos),
    WHCL_FUNC2("ceil", cb_ceil),
    WHCL_FUNC2("atan2", cb_atan2),
    WHCL_FUNC2("atan", cb_atan),
    WHCL_FUNC2("asin", cb_asin),
    WHCL_FUNC2("acos", cb_acos),
    whcl_func_def_empty_m
    };
    rc = whcl_install_functions(el, mod, funcs, 0);
    if(rc) goto end;
  }
  {
    cwal_value * v;
#define SET(NAME) if(!v){ rc = CWAL_RC_OOM; goto end; }                 \
    cwal_ref(v);                                                  \
    rc = cwal_prop_set_with_flags( mod, NAME, cwal_strlen(NAME),        \
                                   v, CWAL_VAR_F_CONST );               \
    cwal_unref(v);                                                \
    if(rc) goto end

    v = cwal_new_double(el->ec, 3.14159265);
    SET("PI");
#undef SET
  }

  end:
  if(rc){
    cwal_unref(mod);
  }else{
    *rv = mod;
    cwal_unhand(mod);
  }
  return rc;
}

WHCL_MODULE_IMPL(math,whcl__install_math);

//WHCL_MODULE_REGISTER_(math);
