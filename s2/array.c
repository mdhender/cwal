/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "s2_internal.h"

#if 1
#include <stdio.h>
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__); if(1) printf pfexp
#else
#define MARKER(pfexp) (void)0
#endif

#define ARGS_SE s2_engine * se = s2_engine_from_args(args); \
    assert(se)

#define THIS_ARRAY \
    cwal_array * self = 0;                          \
    ARGS_SE; \
    self = cwal_value_array_part(se->e, args->self); \
    if(!self){ \
        return s2_throw( se, CWAL_RC_TYPE, \
                         "'this' is-not-an Array." );        \
    } (void)0

static int s2_cb_array_ctor( cwal_callback_args const * args, cwal_value **rv ){
  int rc = 0;
  cwal_array * ar;
  ar = cwal_new_array( args->engine );
  if(!ar) rc = CWAL_RC_OOM;
  else if(args->argc){
    cwal_int_t i = ((cwal_int_t)args->argc)-1;
    /* insert end-first to get all allocation out of the way
       up front. */
    for( ; !rc && i >= 0; --i ){
      rc = cwal_array_set(ar, (cwal_size_t)i, args->argv[i]);
    }
  }
  if(!rc) *rv = cwal_array_value(ar);
  else cwal_value_unref(cwal_array_value(ar));
  return rc;
}

static int s2_cb_array_clear( cwal_callback_args const * args, cwal_value **rv ){
  char clearProps;
  THIS_ARRAY;
  clearProps = (args->argc>0)
    ? cwal_value_get_bool(args->argv[0])
    : 0;
  cwal_array_clear(self, 0, clearProps);
  *rv = args->self;
  return 0;
}

static int s2_cb_array_isempty( cwal_callback_args const * args, cwal_value **rv ){
    THIS_ARRAY;
    *rv = cwal_new_bool( cwal_array_length_get(self) ? 0 : 1 );
    return 0;        
}

static int s2_cb_array_length( cwal_callback_args const * args, cwal_value **rv ){
  THIS_ARRAY;
  if(args->argc){
    /* SET length */
    cwal_value * const index = args->argv[0];
    cwal_int_t len = cwal_value_get_integer(index);
    int rc;
    if((len<0) || !cwal_value_is_number(index)){
      return s2_throw(se, CWAL_RC_MISUSE,
                      "length argument must be "
                      "a non-negative integer.");
    }
    rc = cwal_array_length_set( self, (cwal_size_t)len );
    if(rc){
      return s2_throw(se, CWAL_RC_MISUSE,
                      "Setting array length to %"CWAL_INT_T_PFMT
                      " failed with code %d (%s).",
                      len, rc, cwal_rc_cstr(rc));
    }
    *rv = args->self;
  }else{
    *rv = cwal_new_integer( args->engine,
                            (cwal_int_t)cwal_array_length_get(self) );
  }
  /* dump_val(*rv,"array.length()"); */
  return *rv ? 0 : CWAL_RC_OOM;
}

static int s2_cb_array_reserve( cwal_callback_args const * args, cwal_value **rv ){
  static char const * usage = "Expecting a non-negative integer argument.";
  THIS_ARRAY;
  if(!args->argc){
    return s2_throw(se, CWAL_RC_MISUSE, "%s", usage );
  }
  else{
    cwal_value * const index = args->argv[0];
    cwal_int_t len = cwal_value_get_integer(index);
    int rc;
    if((len<0) || !cwal_value_is_number(index)){
      return s2_throw(se, CWAL_RC_MISUSE, "%s", usage);
    }
    rc = cwal_array_reserve( self, (cwal_size_t)len );
    if(rc){
      return s2_throw(se, rc, "cwal_array_reserve() failed!");
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
static int s2_cb_array_push_impl( cwal_callback_args const * args,
                                  cwal_value **rv, int isPush ){
  THIS_ARRAY;
  if(!args->argc){
    *rv = cwal_value_undefined();
    return 0;
  }
  else {
    int rc;
    cwal_size_t i = 0;
    for( i = 0; i < args->argc; ++i ){
      rc = cwal_array_append( self, args->argv[i] );
      if(rc) break;
      else if(isPush) *rv = args->argv[i];
    }
    if(rc){
      rc = s2_throw(se, rc, "Appending to array failed.");
    }else if(!isPush){
      *rv = args->self;
    }
  }
  return 0;
}


static int s2_cb_array_push( cwal_callback_args const * args, cwal_value **rv ){
  return s2_cb_array_push_impl(args, rv, 1);
}
static int s2_cb_array_operator_pluseq( cwal_callback_args const * args, cwal_value **rv ){
  return s2_cb_array_push_impl(args, rv, 0);
}
#if 0
/* Still unsure about whether (array + X)===array is really valid. */
static int s2_cb_array_operator_plus( cwal_callback_args const * args, cwal_value **rv ){
  return s2_cb_array_push_impl(args, rv, 0);
}
#endif

static int s2_cb_array_pop( cwal_callback_args const * args, cwal_value **rv ){
  cwal_size_t aLen;
  THIS_ARRAY;
  aLen = cwal_array_length_get(self);
  if(!aLen){
    *rv = cwal_value_undefined();
  }else{
    --aLen;
#if 1
    *rv = cwal_array_take(self, aLen);
    cwal_array_length_set(self, aLen);
#else
    *rv = cwal_array_get(self, aLen);
    if(!*rv) *rv = cwal_value_undefined();
    cwal_value_ref(*rv);
    cwal_array_length_set(self, aLen);
    cwal_value_unhand(*rv);
#endif

  }
  return 0;
}


static int s2_cb_array_slice( cwal_callback_args const * args, cwal_value **rv ){
  cwal_int_t offset = 0, count = 0;
  cwal_array * acp = NULL;
  int rc;
  THIS_ARRAY;
  if(args->argc>0){
    offset = cwal_value_get_integer(args->argv[0]);
    if(args->argc>1){
      count = cwal_value_get_integer(args->argv[1]);
    }
  }
  if((offset<0) || (count<0)){
    return s2_throw(se, CWAL_RC_RANGE,
                    "Slice offset and count must both "
                    "be positive numbers.");
  }
  rc = cwal_array_copy_range( self, (cwal_size_t)offset,
                              (cwal_size_t)count, &acp );
  if(!rc){
    assert(acp);
    *rv = cwal_array_value(acp);
  }
  return rc;
}


static int s2_cb_array_unshift( cwal_callback_args const * args, cwal_value **rv ){
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

static int s2_cb_array_shift( cwal_callback_args const * args, cwal_value **rv ){
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
    }else if(rc) break;
    else if(!*rv){
      *rv = cwal_value_undefined();
    }
  }
  return rc;
}

static int s2_cb_array_sort( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  cwal_function * cmp;
  THIS_ARRAY;
  if(!args->argc || !(cmp = cwal_value_function_part(args->engine,
                                                     args->argv[0]))){
    rc = cwal_array_sort(self, cwal_compare_value_void);
  }else{
    rc = cwal_array_sort_func( self, args->self, cmp, false);
    if(!rc && cwal_exception_get(args->engine)){
      /* This is the only way to propagate an exception thrown from the
         sort routine for the time being. */
      rc = CWAL_RC_EXCEPTION;
    }
  }
  if(!rc) *rv = args->self;
  return rc;
}

static int s2_cb_array_reverse( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  THIS_ARRAY;
  rc = cwal_array_reverse(self);
  if(!rc) *rv = args->self;
  return rc;
}

static int s2_cb_array_each( cwal_callback_args const * args, cwal_value **rv ){
  int rc = 0;
  cwal_function * f;
  cwal_size_t i = 0;
  cwal_size_t alen;
  cwal_value * const vnull = cwal_value_undefined();
  cwal_value * av[2] = {NULL,NULL} /* Value, Index arguments for callback */;
  cwal_value * ndx;
  cwal_value * cbRv = NULL;
  THIS_ARRAY;
  f = args->argc ? cwal_value_get_function(args->argv[0]) : NULL;
  if(!f){
    return s2_throw(se, CWAL_RC_MISUSE,
                    "'eachIndex' expects a Function argument, "
                    "but got a %s.",
                    args->argc
                    ? cwal_value_type_name(args->argv[0])
                    : "NULL");
  }
  alen = cwal_array_length_get(self);
  for( i = 0; !rc && i < alen; ++i ){
    av[0] = cwal_array_get( self, i );
    if(!av[0]) av[0] = vnull;
    ndx = cwal_new_integer( args->engine, (cwal_int_t)i );
    if(!ndx){
      rc = CWAL_RC_OOM;
      break;
    }
    cwal_value_ref(ndx);
    av[1] = ndx;
    cbRv = 0;
    rc = cwal_function_call( f, args->self, &cbRv, 2, av );
    av[0] = av[1] = NULL;
    /* Reminder ndx will always end up in the argv array,
       which will clean it up before the call's scope returns.
       No, it won't because the new argv is in a lower scope.
    */
    cwal_value_unref(ndx)
      /* If we unref without explicitly ref()'ing we will likely pull
         the arguments out from the underlying argv array if a ref is
         held to is. If we don't ref it we don't know if it can be
         cleaned up safely. So we ref/unref it manually to force
         cleanup _unless_ someone else got a reference. We'll re-use
         it momentarily (next iteration) if recycling is on.
      */;
    if(!rc && rv && cwal_value_false()==cbRv/*literal false, not another falsy*/){
      /* Treat a "real" false return value as signal to end the
         loop. */
      break;
    }
    if(ndx!=cbRv) cwal_refunref(cbRv);
  }
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
static int s2_array_get_index( cwal_callback_args const * args,
                               uint16_t atIndex,
                               cwal_int_t * rv ){
  ARGS_SE;
  if(args->argc<atIndex){
    return s2_throw(se, CWAL_RC_MISUSE,
                    "Too few arguments.");
  }
  *rv = cwal_value_get_integer(args->argv[atIndex]);
  return (*rv<0)
    ? s2_throw(se, CWAL_RC_RANGE,
               "Array indexes may not be be negative.")
    : 0;
}

int s2_cb_array_get_index( cwal_callback_args const * args,
                           cwal_value **rv ){
  int rc;
  cwal_int_t i = 0;
  THIS_ARRAY;
  rc = s2_array_get_index(args, 0, &i);
  if(rc) return rc;
  else{
    cwal_value * v;
    assert(i>=0);
    v = cwal_array_get(self, (cwal_size_t)i);
    *rv = v ? v : cwal_value_undefined();
    return 0;
  }
}


static int s2_cb_array_remove_index( cwal_callback_args const * args, cwal_value **rv ){
  cwal_int_t ndx;
  THIS_ARRAY;
  if(!args->argc || (0>(ndx=cwal_value_get_integer(args->argv[0])))){
    *rv = cwal_value_false();
  }else {
    cwal_size_t const max = cwal_array_length_get(self);
    if((cwal_size_t)ndx>=max){
      *rv = cwal_value_false();
      return 0;
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


int s2_cb_array_set_index( cwal_callback_args const * args,
                           cwal_value **rv ){
  int rc;
  cwal_int_t i;
  THIS_ARRAY;
  rc = s2_array_get_index(args, 0, &i);
  if(rc) return rc;
  else{
    cwal_value * v = (args->argc>1) ? args->argv[1] : NULL;
    assert(i>=0);
    rc = cwal_array_set(self, (cwal_size_t)i, v);
    if(!rc) *rv = v ? v : cwal_value_undefined();
    return rc;
  }
}

static int s2_cb_array_index_of( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  cwal_size_t ndx = 0;
  cwal_int_t irc = 0;
  char const strictCompare = (args->argc>1) ? cwal_value_get_bool(args->argv[1]) : 1;
  THIS_ARRAY;
  if(!args->argc){
    return s2_throw(se, CWAL_RC_MISUSE, "indexOf() requires an argument.");
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

static int s2_cb_array_join( cwal_callback_args const * args, cwal_value **rv ){
  char const * joiner = NULL;
  cwal_size_t jLen = 0;
  cwal_size_t i, aLen, oldUsed;
  int rc = 0;
  cwal_buffer * buf;
  cwal_tuple * tp = 0;
  cwal_array * ar = 0;
  ARGS_SE;
  ar = cwal_value_array_part(args->engine, args->self);
  tp = ar ? 0 : cwal_value_get_tuple(args->self);
  if(!ar && !tp){
    return cwal_cb_throw( args, CWAL_RC_TYPE,
                       "'this' is-not-an Array or Tuple." );
  }
  buf = &se->buffer;
  oldUsed = buf->used;
  if(!args->argc){
    misuse:
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
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
      rc = s2_value_to_buffer(args->engine, buf, v);
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

cwal_value * s2_prototype_array( s2_engine * se ){
    int rc = 0;
    cwal_value * proto;
    proto = cwal_prototype_base_get( se->e, CWAL_TYPE_ARRAY );
    if(proto) return proto;
    proto = cwal_new_object_value(se->e);
    if(!proto){
        rc = CWAL_RC_OOM;
        goto end;
    }
    cwal_value_ref(proto);
    rc = cwal_prototype_base_set(se->e, CWAL_TYPE_ARRAY, proto );
    cwal_value_unref(proto) /* on success ^^^, the core now owns a ref */;
    if(!rc) rc = s2_prototype_stash(se, "array", proto);
    if(rc) goto end;
    assert(proto == cwal_prototype_base_get(se->e, CWAL_TYPE_ARRAY));
    /* MARKER(("Setting up OBJECT prototype.\n")); */

    {
      s2_func_def const funcs[] = {
      S2_FUNC2("clear", s2_cb_array_clear),
      S2_FUNC2("eachIndex", s2_cb_array_each),
      S2_FUNC2("getIndex", s2_cb_array_get_index),
      S2_FUNC2("indexOf", s2_cb_array_index_of),
      S2_FUNC2("isEmpty", s2_cb_array_isempty),
      S2_FUNC2("join",s2_cb_array_join),
      S2_FUNC2("length",s2_cb_array_length),
      S2_FUNC2("operator+=", s2_cb_array_operator_pluseq),
      /*S2_FUNC2("operator+", s2_cb_array_operator_plus),*/
      S2_FUNC2("push", s2_cb_array_push),
      S2_FUNC2("pop", s2_cb_array_pop),
      S2_FUNC2("removeIndex", s2_cb_array_remove_index),
      S2_FUNC2("reserve", s2_cb_array_reserve),
      S2_FUNC2("reverse", s2_cb_array_reverse),
      S2_FUNC2("setIndex", s2_cb_array_set_index),
      S2_FUNC2("shift", s2_cb_array_shift),
      S2_FUNC2("slice", s2_cb_array_slice),
      S2_FUNC2("sort", s2_cb_array_sort),
      S2_FUNC2("toString", s2_cb_value_to_string),
      S2_FUNC2("unshift", s2_cb_array_unshift),
      s2_func_def_empty_m
      };
      rc = s2_install_functions(se, proto, funcs, 0);
      if(!rc) rc = s2_ctor_callback_set(se, proto,
                                        s2_cb_array_ctor);
      if(rc) goto end;
    }
    
    {
      /* Array.filter() impl. */
      char const * src =
        "proc(f,invert=false){"
          "affirm typeinfo(iscallable f);"
          "affirm typeinfo(islist this);"
          "const a = [];"
          "foreach(@this=>v) f(v) ? (invert ? 0 : a[]=v) : (invert ? a[]=v : 0);"
          "return typeinfo(istuple this) ? [#@a] : a;"
        "}";
      rc = s2_set_from_script(se, src, (int)cwal_strlen(src),
                              proto, "filter", 6);
      if(rc) goto end;
    }

    end:
    return rc ? NULL : proto;
}

static int s2_cb_tuple_ctor( cwal_callback_args const * args, cwal_value **rv ){
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
        cwal_value * v = cwal_array_get(ar, i);
        rc = cwal_tuple_set(tp, i, v);
        assert(!rc && "No possible error conditions here.");
      }
    }
    return rc;
  }else if(cwal_value_is_tuple(args->argv[0])){
    /* Tuple(tuple): Copy elements from the tuple. */
    cwal_tuple const * t1 = cwal_value_get_tuple(args->argv[0]);
    uint16_t const an = cwal_tuple_length(t1);
    uint16_t i;
    int rc = 0;
    cwal_tuple * tp = cwal_new_tuple(args->engine, an);
    if(!tp) rc = CWAL_RC_OOM;
    else{
      *rv = cwal_tuple_value(tp);
      for(i = 0; i < an; ++i ){
        cwal_value * v = cwal_tuple_get(t1, i);
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
    *rv = cwal_new_tuple_value(args->engine, (cwal_size_t)n);;
    assert(*rv
           ? (s2_prototype_tuple(s2_engine_from_args(args))
              == cwal_value_prototype_get(args->engine, *rv))
           : 1);

    assert((cwal_size_t)n == cwal_tuple_length(cwal_value_get_tuple(*rv)));
    return *rv ? 0 : CWAL_RC_OOM;
  }
  misuse:
  return cwal_cb_throw(args, CWAL_RC_MISUSE,
                      "Expecting an (array | tuple | integer>=0).");
  toomany:
  return cwal_cb_throw(args, CWAL_RC_RANGE, "Too many items for a tuple!");
}

static int s2_cb_tuple_length( cwal_callback_args const * args, cwal_value **rv ){
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

/**
   Impl for Tuple's operator!=, <=, <, >, >=, ==.

   direction: <0 means lt, >0 means gt, 0 means eq.

   eq: 0 for (<, >, !=), non-0 for (>=, <=, ==).

   TODO: we can refactor this in a generic routine which injects these
   overloads for any type. Alternately, make the op overloading away
   of the compare() method and use it (if availble) to implement these
   ops. That would cost 6 fewer overloads.
*/
static int s2_cb_tuple_cmp_impl( cwal_callback_args const * args, cwal_value **rv,
                                 int direction, char eq ){
  cwal_value * rhs = args->argc ? args->argv[0] : 0;
  assert(1==args->argc);
  if(1!=args->argc){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                      "Internal error: expecting operator "
                      "to be called with 1 argument.");
  }
  else if(!cwal_value_is_tuple(rhs)){
    return cwal_cb_throw(args, CWAL_RC_TYPE,
                      "Cannot compare a tuple to a non-tuple.");
  }else{
    int const cmp = cwal_value_compare(args->self, rhs);
    if(!direction){
      if(!eq){ /* != op */
        *rv = cmp ? cwal_value_true() : cwal_value_false();
      }else{ /* == op */
        *rv = cmp ? cwal_value_false() : cwal_value_true();
      }
    }else if(!cmp){
      *rv = eq ? cwal_value_true() : cwal_value_false();
    }else{
      *rv = (direction<0 && cmp<0)
        ? cwal_value_true()
        : ((direction>0 && cmp>0)
           ? cwal_value_true()
           : cwal_value_false())
        ;
    }
    return 0;
  }
}

static int s2_cb_tuple_op_lt( cwal_callback_args const * args, cwal_value **rv ){
  return s2_cb_tuple_cmp_impl(args, rv, -1, 0);
}


static int s2_cb_tuple_op_le( cwal_callback_args const * args, cwal_value **rv ){
  return s2_cb_tuple_cmp_impl(args, rv, -1, 1);
}

static int s2_cb_tuple_op_gt( cwal_callback_args const * args, cwal_value **rv ){
  return s2_cb_tuple_cmp_impl(args, rv, 1, 0);
}

static int s2_cb_tuple_op_ge( cwal_callback_args const * args, cwal_value **rv ){
  return s2_cb_tuple_cmp_impl(args, rv, 1, 1);
}

static int s2_cb_tuple_op_neq( cwal_callback_args const * args, cwal_value **rv ){
  return s2_cb_tuple_cmp_impl(args, rv, 0, 0);
}

static int s2_cb_tuple_op_eq( cwal_callback_args const * args, cwal_value **rv ){
  return s2_cb_tuple_cmp_impl(args, rv, 0, 1);
}

cwal_value * s2_prototype_tuple( s2_engine * se ){
    int rc = 0;
    cwal_value * proto;
    proto = 1
      ? s2_prototype_stashed(se, "tuple")
      : cwal_prototype_base_get( se->e, CWAL_TYPE_TUPLE );
    if(proto) return proto;
    proto = cwal_new_object_value(se->e);
    if(!proto){
        rc = CWAL_RC_OOM;
        goto end;
    }
    cwal_value_prototype_set(proto, NULL);
    rc = cwal_prototype_base_set(se->e, CWAL_TYPE_TUPLE, proto );
    if(!rc) rc = s2_prototype_stash(se, "tuple", proto);
    if(rc) goto end;
    assert(proto == cwal_prototype_base_get(se->e, CWAL_TYPE_TUPLE));

    {
      s2_func_def const funcs[] = {
        S2_FUNC2("compare", s2_cb_value_compare),
        S2_FUNC2("join",s2_cb_array_join),
        S2_FUNC2("length", s2_cb_tuple_length),
        S2_FUNC2("toJSONString", s2_cb_this_to_json_token),
        S2_FUNC2("toString", s2_cb_value_to_string),
        S2_FUNC2("operator<=", s2_cb_tuple_op_le),
        S2_FUNC2("operator<", s2_cb_tuple_op_lt),
        S2_FUNC2("operator>=", s2_cb_tuple_op_ge),
        S2_FUNC2("operator>", s2_cb_tuple_op_gt),
        S2_FUNC2("operator!=", s2_cb_tuple_op_neq),
        S2_FUNC2("operator==", s2_cb_tuple_op_eq),
        s2_func_def_empty_m
      };
      rc = s2_install_functions(se, proto, funcs, 0);
      if(!rc) rc = s2_ctor_callback_set(se, proto, s2_cb_tuple_ctor);
      if(!rc){
        cwal_value * arrayProto = cwal_prototype_base_get( se->e, CWAL_TYPE_ARRAY );
        cwal_kvp * kvp;
        assert(arrayProto && "Array prototype must have been set up already!");
        kvp = cwal_prop_get_kvp( arrayProto, "filter", 6, 0, NULL );
        assert(kvp && "array.filter() must have been installed already!");
        rc = cwal_prop_set_v( proto, cwal_kvp_key(kvp), cwal_kvp_value(kvp) );
      }
      if(rc) goto end;
    }
    /* s2_dump_val(proto,"Pair prototype"); */
    assert(!rc);
    end:
    return rc ? NULL : proto;
}

#undef MARKER
#undef ARGS_SE
#undef THIS_ARRAY
