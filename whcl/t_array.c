/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

/**
   Impls for the array prototype.
*/

#include "internal.h"
#include <stdio.h>
#include <assert.h>


#if 1
#define MARKER(pfexpr) if(1) printf("MARKER: %s:%d:\t",__FILE__,__LINE__); if(1) printf pfexpr
#else
#define MARKER if(0) printf
#endif


#define WHCL__E_ARGS \
  whcl_engine * const el = whcl_engine_from_args(args); assert(el)

#define THIS_ARRAY                                               \
  cwal_array * self = 0;                                         \
    self = cwal_value_array_part(args->engine, args->self);      \
    if(!self){                                                   \
        return cwal_exception_setf( args->engine, CWAL_RC_TYPE,  \
                                    "'this' is-not-an Array." ); \
    } (void)0

static int whcl__cb_array_clear( cwal_callback_args const * args, cwal_value **rv ){
  bool clearProps;
  THIS_ARRAY;
  clearProps = (args->argc>0)
    ? cwal_value_get_bool(args->argv[0])
    : 0;
  cwal_array_clear(self, 0, clearProps);
  *rv = args->self;
  return 0;
}

static int whcl__cb_array_isempty( cwal_callback_args const * args, cwal_value **rv ){
    THIS_ARRAY;
    *rv = cwal_new_bool( cwal_array_length_get(self) ? 0 : 1 );
    return 0;        
}

static int whcl__cb_array_length( cwal_callback_args const * args, cwal_value **rv ){
  THIS_ARRAY;
  if(args->argc){
    /* SET length */
    cwal_value * const index = args->argv[0];
    cwal_int_t len = cwal_value_get_integer(index);
    int rc;
    if((len<0) || !cwal_value_is_number(index)){
      return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                      "length argument must be "
                      "a non-negative integer.");
    }
    rc = cwal_array_length_set( self, (cwal_size_t)len );
    if(rc){
      return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                      "Setting array length to %"CWAL_INT_T_PFMT
                      " failed with code %d (%s).",
                      len, rc, cwal_rc_cstr(rc));
    }
    *rv = args->self;
  }else{
    *rv = cwal_new_integer( args->engine,
                            (cwal_int_t)cwal_array_length_get(self) );
  }
  return *rv ? 0 : CWAL_RC_OOM;
}


static int whcl__cb_array_reserve( cwal_callback_args const * args, cwal_value **rv ){
  static char const * usage = "Expecting a non-negative integer argument.";
  THIS_ARRAY;
  if(!args->argc){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "%s", usage );
  }
  else{
    cwal_value * const index = args->argv[0];
    cwal_int_t len = cwal_value_get_integer(index);
    int rc;
    if((len<0) || !cwal_value_is_number(index)){
      return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                 "%s", usage);
    }
    rc = cwal_array_reserve( self, (cwal_size_t)len );
    if(rc){
      return cwal_exception_setf(args->engine, rc,
                                 "cwal_array_reserve() failed: %s",
                                 cwal_rc_cstr(rc));
    }
    *rv = args->self;
  }
  return 0;
}

/**
   Internal impl for array.push(), array.'operator+=', and
   array.'operator+'. If isPush is true, it operates as push(), else
   as += or binary +.
*/
static int whcl__cb_array_push_impl( cwal_callback_args const * args,
                                     cwal_value **rv, bool isPush ){
  THIS_ARRAY;
  if(!args->argc){
    *rv = cwal_value_undefined();
    return 0;
  }
  else {
    int rc;
    cwal_size_t i = 0;
    cwal_size_t const curLen = cwal_array_length_get(self);
    cwal_size_t const szNeeded = curLen + args->argc;
    rc = cwal_array_reserve(self, szNeeded);
    if(szNeeded < curLen){
      whcl__fatal(CWAL_RC_RANGE, "Numeric overflow appending to array.");
    }else if(rc){
      WHCL__WARN_OOM;
      return rc;
    }
    for( i = 0; i < args->argc; ++i ){
      rc = cwal_array_append( self, args->argv[i] );
      if(rc) break;
      else if(isPush) *rv = args->argv[i];
    }
    if(rc){
      rc = cwal_exception_setf(args->engine, rc,
                               "Appending to array failed: %s",
                               cwal_rc_cstr(rc));
    }else if(!isPush){
      *rv = args->self;
    }
  }
  return 0;
}


static int whcl__cb_array_push( cwal_callback_args const * args, cwal_value **rv ){
  return whcl__cb_array_push_impl(args, rv, 1);
}
#if 0
static int whcl__cb_array_operator_pluseq( cwal_callback_args const * args, cwal_value **rv ){
  return whcl__cb_array_push_impl(args, rv, 0);
}
#endif

static int whcl__cb_array_pop( cwal_callback_args const * args, cwal_value **rv ){
  cwal_size_t aLen;
  THIS_ARRAY;
  aLen = cwal_array_length_get(self);
  if(!aLen){
    *rv = cwal_value_undefined();
  }else{
    --aLen;
    *rv = cwal_array_take(self, aLen);
    cwal_array_length_set(self, aLen);
  }
  return 0;
}

static int whcl__cb_array_slice( cwal_callback_args const * args, cwal_value **rv ){
  cwal_int_t offset = 0, count = -1;
  cwal_array * acp = NULL;
  int rc;
  THIS_ARRAY;
  if(args->argc>0){
    offset = cwal_value_get_integer(args->argv[0]);
    if(args->argc>1){
      count = cwal_value_get_integer(args->argv[1]);
    }
  }
  rc = cwal_array_copy_range2( self, offset, count, &acp );
  if(!rc){
    assert(acp);
    *rv = cwal_array_value(acp);
  }
  return rc;
}

static int whcl__cb_array_unshift( cwal_callback_args const * args, cwal_value **rv ){
  THIS_ARRAY;
  if(!args->argc){
    *rv = cwal_value_undefined();
    return 0;
  }
  else {
    int rc = 0;
    cwal_size_t i = 0;
    if(args->argc>1){
      rc = cwal_array_reserve(self, args->argc
                              + cwal_array_length_get(self));
      if(rc) return rc;
    }
    for( i = args->argc-1;
         i < args->argc /* reminder to self: we're relying on underflow here. Ugly. */;
         --i ){
      rc = cwal_array_prepend( self, args->argv[i] );
      if(rc) break;
    }
    if(!rc) *rv = args->self;
    return rc;
  }
}

static int whcl__cb_array_shift( cwal_callback_args const * args, cwal_value **rv ){
  int rc = 0;
  cwal_int_t i = 0, n = 1;
  THIS_ARRAY;
  if(args->argc && cwal_value_is_integer(args->argv[0])){
    n = cwal_value_get_integer(args->argv[0]);
    if(n<=0){
      return cwal_exception_setf(args->engine, CWAL_RC_RANGE,
                                 "Integer argument to shift() must be >0.");
    }
  }
  for( ; i < n; ++i ){
    *rv = 0;
    rc = cwal_array_shift( self, rv );
    if(CWAL_RC_RANGE==rc){
      rc = 0;
      *rv = cwal_value_undefined();
    }
    else if(rc) break;
    else if(!*rv){
      *rv = cwal_value_undefined();
    }
  }
  return rc;
}

static int whcl__cb_array_sort( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  cwal_function * cmp;
  THIS_ARRAY;
  if(!args->argc || !(cmp = cwal_value_function_part(args->engine,
                                                     args->argv[0]))){
    rc = cwal_array_sort_v2(self, true);
  }else{
    rc = cwal_array_sort_func( self, args->self, cmp, true);
    if(!rc && cwal_exception_get(args->engine)){
      /* This is the only way to propagate an exception thrown from the
         sort routine for the time being. */
      rc = CWAL_RC_EXCEPTION;
    }
  }
  if(!rc) *rv = args->self;
  return rc;
}

static int whcl__cb_array_reverse( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  THIS_ARRAY;
  rc = cwal_array_reverse(self);
  if(!rc) *rv = args->self;
  return rc;
}

/**
   Internal helper which looks for an integer argument at
   args->argv[atIndex] and checks if it is negative.  Returns non-0
   and triggers an exception in args->engine on error. Assigns the
   fetched integer (if any) to *rv, even if it is negative (which also
   results in a CWAL_RC_RANGE exception).
*/
static int whcl__array_get_index( cwal_callback_args const * args,
                                  uint16_t atIndex,
                                  cwal_int_t * rv ){
  if(args->argc<atIndex){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                    "Too few arguments.");
  }
  *rv = cwal_value_get_integer(args->argv[atIndex]);
  return (*rv<0)
    ? cwal_exception_setf(args->engine, CWAL_RC_RANGE,
               "Array indexes may not be be negative.")
    : 0;
}

int whcl__cb_array_get_index( cwal_callback_args const * args,
                           cwal_value **rv ){
  int rc;
  cwal_int_t i = 0;
  THIS_ARRAY;
  rc = whcl__array_get_index(args, 0, &i);
  if(rc) return rc;
  else{
    cwal_value * v;
    assert(i>=0);
    v = cwal_array_get(self, (cwal_size_t)i);
    *rv = v ? v : cwal_value_undefined();
    return 0;
  }
}

static int whcl__cb_array_remove_index( cwal_callback_args const * args, cwal_value **rv ){
  cwal_int_t ndx;
  THIS_ARRAY;
  if(!args->argc || (0>(ndx=cwal_value_get_integer(args->argv[0])))){
    *rv = cwal_value_false();
  }else {
    cwal_size_t const max = cwal_array_length_get(self);
    if((cwal_size_t)ndx>=max){
      *rv = cwal_value_false();
    }else{
      int rc = 0;
      cwal_size_t i = ndx;
      for( ; i < max; ++i ){
        rc = cwal_array_set(self, i,
                            cwal_array_get(self, i+1));
        assert(!rc && "Cannot fail in this case.");
        if(rc) break;
      }
      if(!rc){
        cwal_array_length_set(self, max-1);
        *rv = cwal_value_true();
      }
    }
  }
  return 0;
}

int whcl__cb_array_set_index( cwal_callback_args const * args,
                              cwal_value **rv ){
  int rc;
  cwal_int_t i;
  THIS_ARRAY;
  rc = whcl__array_get_index(args, 0, &i);
  if(rc) return rc;
  else{
    cwal_value * v = (args->argc>1) ? args->argv[1] : NULL;
    assert(i>=0);
    rc = cwal_array_set(self, (cwal_size_t)i, v);
    if(!rc) *rv = v ? v : cwal_value_undefined();
    return rc;
  }
}

static int whcl__cb_array_index_of( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  cwal_size_t ndx = 0;
  cwal_int_t irc = 0;
  bool const strictCompare =
    (args->argc>1) ? cwal_value_get_bool(args->argv[1]) : 1;
  THIS_ARRAY;
  if(!args->argc){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "indexOf() requires an argument.");
  }
  rc = cwal_array_index_of(self, args->argv[0], &ndx, strictCompare);
  if(CWAL_RC_NOT_FOUND==rc){
    rc = 0;
    irc = -1;
  }else if(!rc){
    irc = (cwal_int_t)ndx;
  }else{
    assert(!"Should not be possible to fail like this with valid args.");
  }
  if(!rc){
    *rv = cwal_new_integer(args->engine, irc);
    rc = *rv ? 0 : CWAL_RC_OOM;
  }
  return rc;
}

static int whcl__cb_array_join( cwal_callback_args const * args, cwal_value **rv ){
  char const * joiner = NULL;
  cwal_size_t jLen = 0;
  cwal_size_t i, aLen, oldUsed;
  int rc = 0;
  cwal_buffer * buf;
  cwal_tuple * tp = 0;
  cwal_array * ar = 0;
  WHCL__E_ARGS;
  ar = cwal_value_array_part(args->engine, args->self);
  tp = ar ? 0 : cwal_value_get_tuple(args->self);
  if(!ar && !tp){
    return cwal_exception_setf( args->engine, CWAL_RC_TYPE,
                                "'this' is-not-an Array or Tuple." );
  }
  buf = &el->escBuf;
  oldUsed = buf->used;
  if(!args->argc){
    misuse:
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting string argument.");
  }
  joiner = cwal_value_get_cstr(args->argv[0], &jLen);
  if(!joiner) goto misuse;
  aLen = ar
    ? cwal_array_length_get(ar)
    : cwal_tuple_length(tp);
  for( i = 0; !rc && i < aLen; ++i ){
    cwal_value * v = ar
      ? cwal_array_get(ar, i)
      : cwal_tuple_get(tp, (uint16_t)i);
    if(i>0){
      rc = cwal_buffer_append(args->engine, buf, joiner, jLen);
    }
    if(v && !rc){
      rc = whcl_value_to_buffer(el, buf, v);
    }
  }
  if(!rc){
    cwal_size_t const ln = buf->used - oldUsed;
    assert(oldUsed <= buf->used);
    *rv = cwal_new_string_value(args->engine,
                                 (char const *)(buf->mem+oldUsed),
                                 ln);
    if(!*rv) rc = CWAL_RC_OOM;
  }
  buf->used = oldUsed;
  return rc;
}


cwal_value * whcl_prototype_array(whcl_engine * const el){
  int rc = 0;
  cwal_value * proto;
  proto = cwal_prototype_base_get( el->ec, CWAL_TYPE_ARRAY );
  if(proto
     || !whcl_prototype_object(el) /* timing hack */
     ) return proto;
  proto = cwal_new_object_value(el->ec);
  if(!proto){
    WHCL__WARN_OOM;
    rc = CWAL_RC_OOM;
    goto end;
  }
  cwal_ref(proto);
  rc = cwal_prototype_base_set(el->ec, CWAL_TYPE_ARRAY, proto );
  cwal_unref(proto) /* on success ^^^, the core now owns a ref */;
  if(!rc) rc = whcl__prototype_stash(el, "Array", proto);
  if(rc) goto end;
  assert(proto == cwal_prototype_base_get(el->ec, CWAL_TYPE_ARRAY));
  /* MARKER(("Setting up ARRAY prototype.\n")); */

  {
    whcl_func_def const funcs[] = {
    WHCL_FUNC2("clear-list", whcl__cb_array_clear),
    WHCL_FUNC2("get-index", whcl__cb_array_get_index),
    WHCL_FUNC2("index-of", whcl__cb_array_index_of),
    WHCL_FUNC2("is-empty", whcl__cb_array_isempty),
    WHCL_FUNC2("join",whcl__cb_array_join),
    WHCL_FUNC2("length",whcl__cb_array_length),
    WHCL_FUNC2("pop", whcl__cb_array_pop),
    WHCL_FUNC2("push", whcl__cb_array_push),
    WHCL_FUNC2("reserve", whcl__cb_array_reserve),
    WHCL_FUNC2("reverse", whcl__cb_array_reverse),
    WHCL_FUNC2("set-index", whcl__cb_array_set_index),
    WHCL_FUNC2("shift", whcl__cb_array_shift),
    WHCL_FUNC2("slice", whcl__cb_array_slice),
    WHCL_FUNC2("sort", whcl__cb_array_sort),
    //WHCL_FUNC2("to-json", whcl_cb_this_to_json_token),
    //WHCL_FUNC2("to-string", whcl_cb_value_to_string),
    WHCL_FUNC2("unset-index", whcl__cb_array_remove_index),
    WHCL_FUNC2("unshift", whcl__cb_array_unshift),
    //WHCL_FUNC2("operator+=", whcl__cb_array_operator_pluseq),
    whcl_func_def_empty_m
    };
    rc = whcl_install_functions(el, proto, funcs, 0);
    if(0==rc) whcl_install_command_cb(el, proto);
    if(rc) goto end;
  }

  assert(0==rc);
  {
    /* Array.filter() impl. */
    char const * src =
      "proc -anon {-v -tuple f} {"
      "affirm [info has-function $f];"
      "affirm [info is-list this];"
      "const a array;"
      "if {argv[-v]} { "
        "foreach v this { if {![f $v]} {a.push $v} } "
      "} else { "
        "foreach v this { if {[f $v]} {a.push $v} } "
      "};"
      "return ("
      "((argv[-tuple] || [info is-tuple this]) && [new whcl.Tuple $a]) "
      " || $a"
      ")}";
    rc = whcl_set_from_script(el, src, -1, proto, "filter");
    if(rc) goto end;
  }
  end:
  if(rc){
    MARKER(("Array init rc=%s\n", cwal_rc_cstr(rc)));
    cwal_value * const ex = cwal_exception_get(el->ec);
    if(ex){
      whcl__dump_val(ex,"Array init exception");
    }
  }
  return rc ? NULL : proto;
}

static int whcl__cb_tuple_ctor( cwal_callback_args const * args, cwal_value **rv ){
  cwal_int_t n = (1==args->argc) ? cwal_value_get_integer(args->argv[0]) : -1;
  if(1 != args->argc){
    goto misuse;
  }else if(cwal_value_is_array(args->argv[0])){
    /* Tuple(array): Copy elements from the array. */
    cwal_array const * ar = cwal_value_get_array(args->argv[0]);
    cwal_size_t an = cwal_array_length_get(ar);
    uint16_t i;
    int rc = 0;
    cwal_tuple * tp;
    if(an > (uint16_t)-1){
      goto toomany;
    }
    tp = cwal_new_tuple(args->engine, an);
    if(!tp) rc = CWAL_RC_OOM;
    else{
      *rv = cwal_tuple_value(tp);
      for(i = 0; i < an; ++i ){
        cwal_value * const v = cwal_array_get_v2(ar, i, false);
        rc = cwal_tuple_set(tp, i, v);
      }
    }
    return rc;
  }else if(cwal_value_is_tuple(args->argv[0])){
    /* Tuple(tuple): Copy elements from the tuple. */
    cwal_tuple const * t1 = cwal_value_get_tuple(args->argv[0]);
    uint16_t const an = cwal_tuple_length(t1);
    uint16_t i;
    int rc = 0;
    cwal_tuple * const tp = cwal_new_tuple(args->engine, an);
    if(!tp) rc = CWAL_RC_OOM;
    else{
      *rv = cwal_tuple_value(tp);
      for(i = 0; i < an; ++i ){
        cwal_value * const v = cwal_tuple_get_v2(t1, i, false);
        rc = cwal_tuple_set(tp, i, v);
        assert(!rc && "No possible error conditions here.");
      }
    }
    return rc;
  }else if(n>=0){
    /* Tuple(integer size) */
    if(n > (cwal_int_t)((uint16_t)-1)){
      goto toomany;
    }
    *rv = cwal_new_tuple_value(args->engine, (uint16_t)n);
    assert(*rv
           ? (whcl_prototype_tuple(whcl_engine_from_args(args))
              == cwal_value_prototype_get(args->engine, *rv))
           : 1);
    if(*rv){
      assert((uint16_t)n == cwal_tuple_length(cwal_value_get_tuple(*rv)));
    }
    return *rv ? 0 : CWAL_RC_OOM;
  }
  misuse:
  return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting an (array | tuple | integer>=0).");
  toomany:
  return cwal_cb_throw(args, CWAL_RC_RANGE, "Too many items for a tuple!");
}


static int whcl__cb_tuple_length( cwal_callback_args const * args, cwal_value **rv ){
  cwal_tuple const * tp = cwal_value_get_tuple(args->self);
  if(!tp){
    return cwal_cb_throw(args, CWAL_RC_TYPE,
                         "'this' is-not-a Tuple.");
  }else if(args->argc){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "Tuple length cannot be changed after construction.");
  }else{
    *rv = cwal_new_integer(args->engine, cwal_tuple_length(tp));
    return *rv ? 0 : CWAL_RC_OOM;
  }
}


cwal_value * whcl_prototype_tuple( whcl_engine * const el ){
    int rc = 0;
    cwal_value * proto = cwal_prototype_base_get(el->ec, CWAL_TYPE_TUPLE);
    if(proto || !whcl_prototype_object(el)/*timing hack*/) return proto;
    proto = cwal_new_object_value(el->ec);
    if(!proto){ WHCL__WARN_OOM; rc = CWAL_RC_OOM; goto end; }
    cwal_value_prototype_set(proto, NULL);
    rc = cwal_prototype_base_set(el->ec, CWAL_TYPE_TUPLE, proto );
    //if(!rc) rc = whcl__prototype_stash(el, "Tuple", proto);
    if(rc) goto end;
    assert(proto == cwal_prototype_base_get(el->ec, CWAL_TYPE_TUPLE));
    {
      whcl_func_def const funcs[] = {
        WHCL_FUNC2("compare", whcl_cb_value_compare),
        WHCL_FUNC2("join",whcl__cb_array_join),
        WHCL_FUNC2("length", whcl__cb_tuple_length),
        WHCL_FUNC2("to-json", whcl_cb_this_to_json_token),
        WHCL_FUNC2("to-string", whcl_cb_value_to_string),
#if 0
        WHCL_FUNC2("operator<=", whcl__cb_tuple_op_le),
        WHCL_FUNC2("operator<", whcl__cb_tuple_op_lt),
        WHCL_FUNC2("operator>=", whcl__cb_tuple_op_ge),
        WHCL_FUNC2("operator>", whcl__cb_tuple_op_gt),
        WHCL_FUNC2("operator!=", whcl__cb_tuple_op_neq),
        WHCL_FUNC2("operator==", whcl__cb_tuple_op_eq),
#endif
        whcl_func_def_empty_m
      };
      rc = whcl_install_functions(el, proto, funcs, 0);
      if(!rc) rc = whcl_ctor_callback_set(el, proto, whcl__cb_tuple_ctor);
      if(!rc) rc = whcl_install_command_cb(el, proto);
      if(!rc) rc = whcl__install_into_whcl(el, "Tuple", proto);
      if(!rc){
        cwal_value * arrayProto =
          cwal_prototype_base_get( el->ec, CWAL_TYPE_ARRAY );
        cwal_kvp * kvp;
        assert(arrayProto && "Array prototype must have been set up already!");
        kvp = cwal_prop_get_kvp( arrayProto, "filter", 6, 0, NULL );
        assert(kvp && "array.filter() must have been installed already!");
        rc = cwal_prop_set_v( proto, cwal_kvp_key(kvp), cwal_kvp_value(kvp) );
      }
    }
    end:
    if(rc){
      MARKER(("Tuple init rc=%s\n", cwal_rc_cstr(rc)));
      cwal_value * const ex = cwal_exception_get(el->ec);
      if(ex){
        whcl__dump_val(ex,"Tuple init exception");
      }
    }
    return rc ? NULL : proto;
}


#undef MARKER
#undef WHCL__E_ARGS
#undef THIS_ARRAY
