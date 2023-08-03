/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "s2_internal.h"
#include <stdio.h> /* sprintf() */

#if 1
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__); if(1) printf pfexp
#else
#define MARKER(pfexp) (void)0
#endif


#define ARGS_SE s2_engine * se = s2_engine_from_args(args)


#define THIS_DOUBLE                             \
    cwal_double_t self; \
    ARGS_SE; \
    assert(se); \
    if(!cwal_value_is_number(args->self)){                      \
        return s2_throw( se, CWAL_RC_TYPE, \
                         "'this' is-not-a Number." );   \
    }\
    self = cwal_value_get_double(args->self)

#define THIS_INT                             \
    cwal_int_t self; \
    ARGS_SE; \
    assert(se); \
    if(!cwal_value_is_number(args->self)){                      \
        return s2_throw( se, CWAL_RC_TYPE,                   \
                         "'this' is-not-a Number." );        \
    }\
    self = cwal_value_get_integer(args->self)

static int s2_cb_int_to_dbl( cwal_callback_args const * args, cwal_value **rv ){
  THIS_INT;
  *rv = cwal_value_is_double(args->self)
    ? args->self
    : cwal_new_double( args->engine, (cwal_int_t)self )
    ;
  return *rv ? 0 : CWAL_RC_OOM;
}

static int s2_cb_dbl_to_int( cwal_callback_args const * args, cwal_value **rv ){
  THIS_DOUBLE;
  *rv = cwal_value_is_integer(args->self)
    ? args->self
    : cwal_new_integer( args->engine, (cwal_int_t)self )
    ;
  return *rv ? 0 : CWAL_RC_OOM;
}

int s2_cb_format_self_using_arg( cwal_callback_args const * args, cwal_value **rv ){
  cwal_engine * e = args->engine;
  cwal_size_t fmtLen = 0;
  char const * fmtStr;
  fmtStr = args->argc
    ? cwal_value_get_cstr(args->argv[0], &fmtLen)
    : 0;
#if 0
  if(fmtStr && fmtLen && '%'==*fmtStr){
    ++fmtStr;
    --fmtLen;
  }
#endif
  if(!fmtLen){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting a single cwal_buffer_format()-compatible "
                       "string argument, minus the '$1%%' prefix.");
  }else{
    int rc;
    cwal_buffer fmtBuf = cwal_buffer_empty;
    char * zFmt = 0;
    cwal_buffer_printf( e, &fmtBuf, "%%1$%.*s",
                        (int)fmtLen, fmtStr);
    zFmt = (char *)fmtBuf.mem;
    fmtBuf = cwal_buffer_empty;
    rc = cwal_buffer_format(e, &fmtBuf, zFmt, -1, 1, &args->self);
    if(rc){
      rc = cwal_exception_setf(e, rc,
                    "Formatted conversion from number "
                    "to string using format '%s' failed: %.*s",
                    zFmt,
                    (int)fmtBuf.used,
                    (char const *)fmtBuf.mem);
    }else{
      *rv = cwal_buffer_to_zstring_value(e, &fmtBuf);
      if(!*rv) rc = CWAL_RC_OOM;
    }
    cwal_free(e, zFmt);
    zFmt = 0;
    cwal_buffer_clear(e, &fmtBuf);
    return rc;
  }
}

static int s2_cb_num_to_string( cwal_callback_args const * args, cwal_value **rv ){
  cwal_engine * e = args->engine;
  int rc = 0;
  if(args->argc){
    rc = s2_cb_format_self_using_arg(args, rv);
  }else{
    enum { BufLen = 100 };
    char buf[BufLen] = {0};
    cwal_size_t bLen = (cwal_size_t)BufLen;
    if(cwal_value_is_double(args->self)){
      rc = cwal_double_to_cstr( cwal_value_get_double(args->self),
                                buf, &bLen );
    }else{
      rc = cwal_int_to_cstr( cwal_value_get_integer(args->self),
                             buf, &bLen );
    }
    if(rc){
      return cwal_exception_setf(e, rc,
                      "Conversion from number to string failed.");
    }
    *rv = cwal_new_string_value(args->engine, buf, bLen);
    if(!*rv) rc = CWAL_RC_OOM;
  }
  return rc;
}

static int s2_cb_int_to_utf8char( cwal_callback_args const * args, cwal_value **rv ){
  enum { BufLen = 7 };
  unsigned char buf[BufLen] = {0,0,0, 0,0,0, 0};
  cwal_size_t bLen = (cwal_size_t)BufLen;
  int rc;
  THIS_INT;
  rc = cwal_utf8_char_to_cstr((unsigned int)self, buf, bLen);
  if(rc<0){
#if 1
    *rv = cwal_value_undefined();
    return 0 /* arguably we should throw */;
#else
    return s2_throw(se, CWAL_RC_RANGE,
                    "Integer value %"CWAL_INT_T_PFMT" is not "
                    "a valid UTF8 character.", (cwal_int_t)self);
#endif
  }
  *rv = cwal_new_string_value(args->engine, (char const *)buf,
                              cwal_strlen((char const *)buf));
  return *rv ? 0 : CWAL_RC_OOM;
}


int s2_cstr_parse_number( cwal_engine * e, char const * src,
                          cwal_int_t slen, cwal_value ** rv ){
  cwal_int_t inty = 0;
  if(s2_cstr_parse_int(src, slen, &inty)){
    *rv = cwal_new_integer(e, inty);
    return *rv ? 0 : CWAL_RC_OOM;
  }else{
    cwal_double_t dbl = 0.0;
    if(s2_cstr_parse_double(src, slen, &dbl)){
      *rv = cwal_new_double(e, dbl);
      return *rv ? 0 : CWAL_RC_OOM;
    }else{
      *rv = 0;
      return 0;
    }
  }
}

static int s2_cb_parse_int( cwal_callback_args const * args, cwal_value **rv ){
  cwal_value * arg;
  if(!args->argc){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting an argument to parse as an integer.");
  }
  arg = args->argv[0];
  switch(cwal_value_type_id(arg)){
    case CWAL_TYPE_INTEGER:
      *rv = args->argv[0];
      return 0;
    case CWAL_TYPE_DOUBLE:
      *rv = cwal_new_integer(args->engine,
                             (cwal_double_t)cwal_value_get_double(arg));
      return *rv ? 0 : CWAL_RC_OOM;
    case CWAL_TYPE_BOOL:
      *rv = cwal_new_integer(args->engine,
                             cwal_value_get_bool(arg) ? 1 : 0);
      return 0;
    default:{
      cwal_size_t slen = 0;
      char const * str = cwal_value_get_cstr(arg, &slen);
      if(str && slen){
        cwal_int_t inty = 0;
        if(s2_cstr_parse_int(str, (cwal_int_t)slen, &inty)){
          *rv = cwal_new_integer(args->engine, inty);
          return *rv ? 0 : CWAL_RC_OOM;
        }else{
          cwal_double_t dbl = 0.0;
          if(s2_cstr_parse_double(str, (cwal_int_t)slen, &dbl)){
            *rv = cwal_new_integer(args->engine, (cwal_int_t)dbl);
            return *rv ? 0 : CWAL_RC_OOM;
          }
        }
      }
      *rv = cwal_value_undefined();
      return 0;
    }
  }
}


static int s2_cb_parse_double( cwal_callback_args const * args, cwal_value **rv ){
  cwal_value * arg;
  if(!args->argc){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting an argument to parse as a number.");
  }
  arg = args->argv[0];
  switch(cwal_value_type_id(arg)){
    case CWAL_TYPE_INTEGER:
      *rv = cwal_new_double(args->engine,
                            (cwal_double_t)cwal_value_get_integer(arg));
      return *rv ? 0 : CWAL_RC_OOM;
    case CWAL_TYPE_DOUBLE:
      *rv = args->argv[0];
      return 0;
    case CWAL_TYPE_BOOL:
      *rv = cwal_new_double(args->engine,
                            cwal_value_get_bool(arg) ? 1 : 0);
      return 0;
    default:{
      cwal_size_t slen = 0;
      char const * str = cwal_value_get_cstr(arg, &slen);
      if(str){
        cwal_double_t dbl = 0.0;
        if(s2_cstr_parse_double(str, (cwal_int_t)slen, &dbl)){
          *rv = cwal_new_double(args->engine, dbl);
          return *rv ? 0 : CWAL_RC_OOM;
        }else{
          cwal_int_t dd = 0;
          if(s2_cstr_parse_int(str, (cwal_int_t)slen, &dd)){
            *rv = cwal_new_double(args->engine, (cwal_double_t)dd);
            return *rv ? 0 : CWAL_RC_OOM;
          }
        }
      }
      *rv = cwal_value_undefined();
      return 0;
    }
  }
}

static int s2_cb_parse_number( cwal_callback_args const * args, cwal_value **rv ){
  cwal_value * arg;
  if(!args->argc){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting an argument to parse as a number.");
  }
  arg = args->argv[0];
  switch(cwal_value_type_id(arg)){
    case CWAL_TYPE_INTEGER:
    case CWAL_TYPE_DOUBLE:
      *rv = args->argv[0];
      return 0;
    case CWAL_TYPE_BOOL:
      *rv = cwal_new_integer(args->engine,
                             cwal_value_get_bool(arg) ? 1 : 0);
      return 0;
    default:{
      cwal_size_t slen = 0;
      char const * str = cwal_value_get_cstr(arg, &slen);
      if(str && slen){
        int rc;
        *rv = 0;
        rc = s2_cstr_parse_number(args->engine, str,
                                  (cwal_int_t)slen, rv);
        if(!rc && !*rv) *rv = cwal_value_undefined();
        assert( *rv ? 1 : rc );
        return *rv ? 0 : rc;
      }else{
        *rv = cwal_value_undefined();
        return 0;
      }
    }
  }
}

int s2_cb_rand_int( cwal_callback_args const * args, cwal_value **rv ){
  cwal_int_t const v = (cwal_int_t)(rand() % CWAL_INT_T_MAX /* in case we're running on a "small" build */);
  *rv = cwal_new_integer( args->engine, v);
  return *rv ? 0 : CWAL_RC_OOM;
}

int s2_cb_srand_int( cwal_callback_args const * args, cwal_value **rv ){
  if( 1 != args->argc ){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting exactly 1 integer value.");
  }else{
    srand( (unsigned int)cwal_value_get_integer(args->argv[0]) );
    *rv = cwal_value_undefined();
    return 0;
  }
}

static int s2_cb_nth_prime( cwal_callback_args const * args, cwal_value **rv ){
  static const int max = 1000;
  int const ndx = args->argc
    ? (int)cwal_value_get_integer(args->argv[0])
    : -1;
  if(ndx < 1 || ndx>max){
    return cwal_cb_throw(args, args->argc ? CWAL_RC_RANGE : CWAL_RC_MISUSE,
                       "Expecting an integer value between 1 and %d.",
                       max);
  }
  *rv = cwal_new_integer(args->engine,
                         (cwal_int_t)cwal_first_1000_primes()[ndx-1]);
  return *rv ? 0 : CWAL_RC_OOM;
}


/**
   Base prototype for integer and double.
*/
static cwal_value * s2_prototype_number( s2_engine * se ){
  static char const * stashKey = "Number";
  int rc = 0;
  cwal_value * proto;
  proto = s2_prototype_stashed(se, stashKey);
  if(proto
     || !s2_prototype_object(se) /* timing hack */
     ) return proto;
  proto = cwal_new_object_value(se->e);
  if(!proto){
    rc = CWAL_RC_OOM;
    goto end;
  }
  cwal_value_prototype_set(proto, 0 /* so we don't inherit Object!*/);
  if(!rc) rc = s2_prototype_stash(se, stashKey, proto);
  if(!rc){
    const s2_func_def funcs[] = {
      S2_FUNC2("compare", s2_cb_value_compare),
      S2_FUNC2("nthPrime", s2_cb_nth_prime),
      S2_FUNC2("parseDouble", s2_cb_parse_double),
      S2_FUNC2("parseInt", s2_cb_parse_int),
      S2_FUNC2("parseNumber", s2_cb_parse_number),
      S2_FUNC2("toDouble", s2_cb_int_to_dbl),
      S2_FUNC2("toInt", s2_cb_dbl_to_int),
      S2_FUNC2("toJSONString", s2_cb_this_to_json_token),
      S2_FUNC2("toString", s2_cb_num_to_string),
      s2_func_def_empty_m
    };
    rc = s2_install_functions(se, proto, funcs, 0);
  }
  end:
  return rc ? NULL : proto;
}


cwal_value * s2_prototype_integer( s2_engine * se ){
  static char const * stashKey = "Integer";
  int rc = 0;
  cwal_value * proto;
  cwal_value * v = 0;
  cwal_value * numProto = s2_prototype_number(se);
  if(!numProto) return 0;
  proto = s2_prototype_stashed(se, stashKey);
  if(proto
     || !s2_prototype_object(se) /* timing hack */
     ) return proto;
  proto = cwal_new_object_value(se->e);
  if(!proto){
    rc = CWAL_RC_OOM;
    goto end;
  }
  rc = s2_prototype_stash(se, stashKey, proto);
  if(!rc) rc = cwal_prototype_base_set(se->e, CWAL_TYPE_INTEGER, proto );
  if(rc) goto end;
  cwal_value_prototype_set(proto, numProto);
  assert(proto == cwal_prototype_base_get(se->e, CWAL_TYPE_INTEGER));

#define SETV(NAME) \
  if(!v){ rc = CWAL_RC_OOM; goto end; }                       \
  cwal_value_ref(v);                                          \
  rc = cwal_prop_set( proto, NAME, cwal_strlen(NAME), v );   \
  cwal_value_unref(v);                                          \
  v = 0;                                                        \
  if(rc) goto end

  v = cwal_new_integer(se->e, CWAL_INT_T_MIN);
  SETV("INT_MIN");
  v = cwal_new_integer(se->e, CWAL_INT_T_MAX);
  SETV("INT_MAX");

  {
    const s2_func_def funcs[] = {
      S2_FUNC2("toChar", s2_cb_int_to_utf8char),
      S2_FUNC2("rand", s2_cb_rand_int),
      S2_FUNC2("srand", s2_cb_srand_int),
      s2_func_def_empty_m
    };
    rc = s2_install_functions(se, proto, funcs, 0);
  }

#undef SETV
  end:
  return rc ? NULL : proto;
}


static int s2_cb_dbl_floor( cwal_callback_args const * args, cwal_value **rv ){
  THIS_DOUBLE;
#if 1
  if(!self){
    *rv = cwal_new_integer( args->engine, (cwal_int_t)self );
    return 0;
  }
  *rv = cwal_new_integer(args->engine,
                        (cwal_int_t)(self>0 ? self : (self-1)));
#else
  /* Requires C99 */
  *rv = cwal_new_double( args->engine, (cwal_int_t)floor((double)self));
#endif
  return *rv ? 0 : CWAL_RC_OOM;
}

static int s2_cb_dbl_ceil( cwal_callback_args const * args, cwal_value **rv ){
  THIS_DOUBLE;
#if 1
  if(!self){
    *rv = cwal_new_integer( args->engine, (cwal_int_t)self);
    return 0;
  }else if(self<=0){
    *rv = cwal_new_integer(args->engine, (cwal_int_t)(self));
  }else{
    *rv = cwal_new_integer(args->engine,
                           (cwal_int_t)(((cwal_double_t)(cwal_int_t)self)==self
                                        ? self : self+1));
  }
#else
  /* Requires C99 */
  *rv = cwal_new_integer( args->engine, (cwal_int_t)ceil((double)self));
#endif
  return *rv ? 0 : CWAL_RC_OOM;
}

static int s2_cb_dbl_format( cwal_callback_args const * args, cwal_value **rv ){
  int left = 0, right = 0;
  enum { BufLen = 200 };
  char buf[BufLen] = {0};
  THIS_DOUBLE;
  if(args->argc>0){
    left = (int)cwal_value_get_integer(args->argv[0]);
    if(args->argc>1) right = (int)cwal_value_get_integer(args->argv[1]);
    else{
      right = left;
      left = 0;
    }
  }
  sprintf( buf, "%%%d.%d"CWAL_DOUBLE_T_PFMT, left, right );
  *rv = cwal_string_value( cwal_new_stringf( args->engine, buf, self ) );
  return *rv ? 0 : CWAL_RC_OOM;
}

cwal_value * s2_prototype_double( s2_engine * se ){
  static char const * stashKey = "Double";
  int rc = 0;
  cwal_value * proto;
  cwal_value * numProto = s2_prototype_number(se);
  if(!numProto) return 0;
  proto = s2_prototype_stashed(se, stashKey);
  if(proto
     || !s2_prototype_object(se) /* timing hack */
     ) return proto;
  proto = cwal_new_object_value(se->e);
  if(!proto){
    rc = CWAL_RC_OOM;
    goto end;
  }
  rc = s2_prototype_stash(se, stashKey, proto);
  if(!rc) rc = cwal_prototype_base_set(se->e, CWAL_TYPE_DOUBLE, proto );
  if(rc) goto end;
  cwal_value_prototype_set(proto, numProto);
  assert(proto == cwal_prototype_base_get(se->e, CWAL_TYPE_DOUBLE));

#define CHECKV if(!v){ rc = CWAL_RC_OOM; goto end; } (void)0
#define RC if(rc) goto end;

  {
    const s2_func_def funcs[] = {
      S2_FUNC2("ceil", s2_cb_dbl_ceil),
      S2_FUNC2("floor", s2_cb_dbl_floor),
      S2_FUNC2("format", s2_cb_dbl_format),
      s2_func_def_empty_m
    };
    rc = s2_install_functions(se, proto, funcs, 0);
  }

#if 0
  /* FUNC2("round", s2_cb_dbl_round); */
#endif

#undef FUNC2
#undef CHECKV
#undef RC
  end:
  return rc ? NULL : proto;
}

#undef MARKER
#undef ARGS_SE
#undef THIS_INT
#undef THIS_DOUBLE
