/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

/**
   Impls for the numeric type prototypes.
*/

#include "internal.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>

#if 1
#define MARKER(pfexpr) if(1) printf("MARKER: %s:%d:\t",__FILE__,__LINE__); if(1) printf pfexpr
#else
#define MARKER if(0) printf
#endif

#define WHCL_E_ARGS \
  whcl_engine * const el = whcl_engine_from_args(args); assert(el)

#define THIS_DOUBLE                             \
    cwal_double_t self; \
    if(!cwal_value_is_number(args->self)){                      \
        return cwal_cb_throw( args, CWAL_RC_TYPE, \
                         "'this' is-not-a Number." );   \
    }\
    self = cwal_value_get_double(args->self)

#define THIS_INT                             \
    cwal_int_t self; \
    if(!cwal_value_is_number(args->self)){                      \
        return cwal_cb_throw( args, CWAL_RC_TYPE,                   \
                         "'this' is-not-a Number." );        \
    }\
    self = cwal_value_get_integer(args->self)


static int whcl__cb_int_to_dbl( cwal_callback_args const * args, cwal_value **rv ){
  THIS_INT;
  *rv = cwal_value_is_double(args->self)
    ? args->self
    : cwal_new_double( args->engine, (cwal_int_t)self )
    ;
  return *rv ? 0 : CWAL_RC_OOM;
}

static int whcl__cb_dbl_to_int( cwal_callback_args const * args, cwal_value **rv ){
  THIS_DOUBLE;
  *rv = cwal_value_is_integer(args->self)
    ? args->self
    : cwal_new_integer( args->engine, (cwal_int_t)self )
    ;
  return *rv ? 0 : CWAL_RC_OOM;
}

int whcl__cb_format_self_using_arg( cwal_callback_args const * args, cwal_value **rv ){
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

static int whcl__cb_num_to_string( cwal_callback_args const * args, cwal_value **rv ){
  cwal_engine * e = args->engine;
  int rc = 0;
  if(args->argc){
    rc = whcl__cb_format_self_using_arg(args, rv);
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


static int whcl__cb_rand_int( cwal_callback_args const * args, cwal_value **rv ){
  cwal_int_t const v = (cwal_int_t)(rand() % CWAL_INT_T_MAX /* in case we're running on a "small" build */);
  *rv = cwal_new_integer( args->engine, v);
  return *rv ? 0 : CWAL_RC_OOM;
}

static int whcl__cb_srand_int( cwal_callback_args const * args, cwal_value **rv ){
  if( 1 != args->argc ){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting exactly 1 integer value.");
  }else{
    srand( (unsigned int)cwal_value_get_integer(args->argv[0]) );
    *rv = cwal_value_undefined();
    return 0;
  }
}

#if 0
// the originating motivation for this method (getting sizes for hashtables)
// is not currently part of whcl.
static int whcl__cb_nth_prime( cwal_callback_args const * args, cwal_value **rv ){
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
#endif

static int whcl__cb_int_to_utf8char( cwal_callback_args const * args, cwal_value **rv ){
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
    return cwal_cb_throw(args, CWAL_RC_RANGE,
                         "Integer value %"CWAL_INT_T_PFMT" is not "
                         "a valid UTF8 character.", (cwal_int_t)self);
#endif
  }
  *rv = cwal_new_string_value(args->engine, (char const *)buf,
                              cwal_strlen((char const *)buf));
  return *rv ? 0 : CWAL_RC_OOM;
}


static int whcl__cb_parse_int( cwal_callback_args const * args, cwal_value **rv ){
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
        if(whcl_parse_int(str, slen, &inty)){
          *rv = cwal_new_integer(args->engine, inty);
          return *rv ? 0 : CWAL_RC_OOM;
        }else{
          cwal_double_t dbl = 0.0;
          if(0==cwal_cstr_to_double(str, slen, &dbl)){
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


static int whcl__cb_parse_double( cwal_callback_args const * args, cwal_value **rv ){
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
        if(0==cwal_cstr_to_double(str, slen, &dbl)){
          *rv = cwal_new_double(args->engine, dbl);
          return *rv ? 0 : CWAL_RC_OOM;
        }else{
          cwal_int_t dd = 0;
          if(whcl_parse_int(str, slen, &dd)){
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

static int whcl__cb_parse_number( cwal_callback_args const * args, cwal_value **rv ){
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
        rc = whcl_parse_number(args->engine, str, (cwal_int_t)slen, rv);
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


static int whcl__cb_dbl_ceil( cwal_callback_args const * args, cwal_value **rv ){
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

static int whcl__cb_dbl_floor( cwal_callback_args const * args, cwal_value **rv ){
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

static int whcl__cb_dbl_format( cwal_callback_args const * args, cwal_value **rv ){
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


/**
   Base prototype for integer and double.
*/
static cwal_value * whcl_prototype_number( whcl_engine * const el ){
  static char const * stashKey = "Number";
  int rc = 0;
  cwal_value * proto;
  proto = whcl__prototype_stashed(el, stashKey);
  if(proto
     || !whcl_prototype_object(el) /* timing hack */
     ) return proto;
  proto = cwal_new_object_value(el->ec);
  if(!proto){
    rc = CWAL_RC_OOM;
    goto end;
  }
  cwal_value_prototype_set(proto, NULL /* so we don't inherit Object!*/);
  rc = whcl__prototype_stash(el, stashKey, proto);
  if(!rc){
    const whcl_func_def funcs[] = {
      WHCL_FUNC2("compare", whcl_cb_value_compare),
      //WHCL_FUNC2("nth-prime", whcl__cb_nth_prime),
      WHCL_FUNC2("to-json", whcl_cb_this_to_json_token),
      WHCL_FUNC2("to-string", whcl__cb_num_to_string),
      WHCL_FUNC2("to-double", whcl__cb_int_to_dbl),
      WHCL_FUNC2("to-int", whcl__cb_dbl_to_int),
      WHCL_FUNC2("parse-double", whcl__cb_parse_double),
      WHCL_FUNC2("parse-int", whcl__cb_parse_int),
      WHCL_FUNC2("parse-number", whcl__cb_parse_number),
      whcl_func_def_empty_m
    };
    rc = whcl_install_functions(el, proto, funcs, 0);
    if(0==rc) rc = whcl_install_command_cb(el, proto);
  }
  end:
  return rc ? NULL : proto;
}

cwal_value * whcl_prototype_integer( whcl_engine * const el ){
  static char const * stashKey = "Integer";
  int rc = 0;
  cwal_value * proto;
  cwal_value * v = 0;
  cwal_value * numProto = whcl_prototype_number(el);
  if(!numProto) return 0;
  proto = whcl__prototype_stashed(el, stashKey);
  if(proto
     || !whcl_prototype_object(el) /* timing hack */
     ) return proto;
  proto = cwal_new_object_value(el->ec);
  if(!proto){
    rc = CWAL_RC_OOM;
    goto end;
  }
  rc = whcl__prototype_stash(el, stashKey, proto);
  if(!rc) rc = cwal_prototype_base_set(el->ec, CWAL_TYPE_INTEGER, proto );
  if(rc) goto end;
  cwal_value_prototype_set(proto, numProto);
  assert(proto == cwal_prototype_base_get(el->ec, CWAL_TYPE_INTEGER));

#define SETV(NAME) \
  if(!v){ rc = CWAL_RC_OOM; goto end; }                       \
  cwal_value_ref(v);                                          \
  rc = cwal_prop_set( proto, NAME, cwal_strlen(NAME), v );   \
  cwal_value_unref(v);                                          \
  v = 0;                                                        \
  if(rc) goto end

  v = cwal_new_integer(el->ec, CWAL_INT_T_MIN);
  SETV("INT_MIN");
  v = cwal_new_integer(el->ec, CWAL_INT_T_MAX);
  SETV("INT_MAX");

  {
    const whcl_func_def funcs[] = {
      WHCL_FUNC2("rand", whcl__cb_rand_int),
      WHCL_FUNC2("srand", whcl__cb_srand_int),
      WHCL_FUNC2("to-char", whcl__cb_int_to_utf8char),
      whcl_func_def_empty_m
    };
    rc = whcl_install_functions(el, proto, funcs, 0);
  }

#undef SETV
  end:
  return rc ? NULL : proto;
}

cwal_value * whcl_prototype_double( whcl_engine * const el ){
  static char const * stashKey = "Double";
  int rc = 0;
  cwal_value * proto;
  cwal_value * numProto = whcl_prototype_number(el);
  if(!numProto) return 0;
  proto = whcl__prototype_stashed(el, stashKey);
  if(proto
     || !whcl_prototype_object(el) /* timing hack */
     ) return proto;
  proto = cwal_new_object_value(el->ec);
  if(!proto){
    rc = CWAL_RC_OOM;
    goto end;
  }
  rc = whcl__prototype_stash(el, stashKey, proto);
  if(!rc) rc = cwal_prototype_base_set(el->ec, CWAL_TYPE_DOUBLE, proto );
  if(rc) goto end;
  cwal_value_prototype_set(proto, numProto);
  assert(proto == cwal_prototype_base_get(el->ec, CWAL_TYPE_DOUBLE));

#define CHECKV if(!v){ rc = CWAL_RC_OOM; goto end; } (void)0
#define RC if(rc) goto end;

  {
    const whcl_func_def funcs[] = {
      WHCL_FUNC2("ceil", whcl__cb_dbl_ceil),
      WHCL_FUNC2("floor", whcl__cb_dbl_floor),
      WHCL_FUNC2("format", whcl__cb_dbl_format),
      whcl_func_def_empty_m
    };
    rc = whcl_install_functions(el, proto, funcs, 0);
  }

#if 0
  /* FUNC2("round", whcl__cb_dbl_round); */
#endif

#undef FUNC2
#undef CHECKV
#undef RC
  end:
  return rc ? NULL : proto;
}


#undef MARKER
#undef WHCL_E_ARGS
#undef THIS_INT
#undef THIS_DOUBLE
