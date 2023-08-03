/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "s2_internal.h"

#include <stdio.h>

#if 1
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__); if(1) printf pfexp
#else
#define MARKER(pfexp) (void)0
#endif

cwal_value * s2_prototype_stashed( s2_engine * se, char const * typeName ){
  enum { BufLen = 128 };
  char buf[BufLen];
  S2_UNUSED_VAR cwal_size_t const slen = cwal_strlen(typeName);
  assert(slen && (slen < BufLen - 11));
  snprintf(buf, BufLen, "prototype.%s", typeName);
  return s2_stash_get( se, buf );
}

int s2_prototype_stash( s2_engine * se,
                        char const * typeName,
                        cwal_value * proto ){
  enum { BufLen = 128 };
  char buf[BufLen];
  S2_UNUSED_VAR cwal_size_t const slen = cwal_strlen(typeName);
  assert(slen && (slen < BufLen - 11));
  snprintf(buf, BufLen, "prototype.%s", typeName);
  return s2_stash_set( se, buf, proto );
}

#define ARGS_SE s2_engine * se = s2_engine_from_args(args); \
    assert(se)

static int s2_cb_object_ctor( cwal_callback_args const * args, cwal_value **rv ){
  if(args->argc){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                      "Expecting no arguments.");
  }else{
    *rv = cwal_new_object_value( args->engine );
    return *rv ? 0 : CWAL_RC_OOM;
  }
}


static int s2_cb_prop_has_own( cwal_callback_args const * args,
                               cwal_value **rv ){
  if(!args->argc)
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting a property key.");
  else {
    *rv = cwal_new_bool(cwal_prop_has_v(args->self,
                                        args->argv[0],
                                        0));
    return 0;
  }
}


static int s2_cb_prop_clear( cwal_callback_args const * args, cwal_value **rv ){
    int rc = s2_immutable_container_check_cb(args, args->self);
    if( !rc && (rc = cwal_props_clear( args->self )) ){
      rc = cwal_cb_throw(args, rc, "Clearing properties failed "
                       "with code %s.", cwal_rc_cstr(rc));
    }
    if(!rc) *rv = args->self;
    return rc;
}

int s2_value_visit_append_to_array( cwal_value * v, void * state ){
  return cwal_array_append((cwal_array *)state, v);
}

/**
   Script usage: var keys = obj.propertyKeys()

   Returns an array containing all keys for all key/value pairs for
   obj.

*/
static int s2_cb_prop_keys( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  cwal_array * ar;
  if(!cwal_props_can(args->self)){
    return cwal_cb_throw(args, CWAL_RC_TYPE,
                       "This value (of type %s) cannot hold properties.",
                       cwal_value_type_name(args->self));
  }
  ar = cwal_new_array(args->engine);
  if(!ar) return CWAL_RC_OOM;
  rc = cwal_props_visit_keys( args->self,
                              s2_value_visit_append_to_array, ar );
  if(!rc) *rv = cwal_array_value(ar);
  else {
    cwal_array_unref(ar);
  }
  return rc;
}

/**
   Script usage: obj.unset(KEY [, ... KEY])

   If obj is-a Array and KEY is-a Integer then the key is interpreted
   as an array index. Sets *rv to true on success.
*/
static int s2_cb_prop_unset( cwal_callback_args const * args, cwal_value **rv ){
  int rc = 0;
  uint16_t i = 0;
  int b = 0;
  s2_engine * se;
  if( (rc = s2_immutable_container_check_cb(args, args->self)) ) return rc;
  se = s2_engine_from_args(args);
  assert(se);
  if(!args->argc){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "'unset' requires (INDEX|KEY,...) arguments.");
  }
  for( ; !rc && (i < args->argc); ++i ){
    rc = s2_set_v( se, args->self, args->argv[i], 0 );
    if(CWAL_RC_NOT_FOUND==rc){
      rc = 0;
    }
    else if(rc) rc = s2_handle_set_result(se, 0, rc);
    else ++b;
  }
  if(!rc) *rv = cwal_new_bool( b>0 );
  return rc;
}

/**
   Script usage: obj.get(KEY)

   Returns the given key for the given obj, or undefined if not
   found. If KEY is an integer and obj is-a Array then this function
   treats the key as an array index. Sets *rv to the fetch value if found,
   else sets it to the undefined value.

   Potential TODO:

   obj.get([list of keys])

   returns [list of values]
*/
static int s2_cb_prop_get( cwal_callback_args const * args,
                           cwal_value **rv ){
  int rc;
  ARGS_SE;
  if( (rc = s2_immutable_container_check_cb(args, args->self)) ) return rc;
  else if(!args->argc){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "'get' requires (INDEX|KEY) argument.");
  }else{
    /* Reminder: we use s2_get_v() instead of cwal_prop_get_v()
       for the prototype pseudo-property and integer array
       index support. OTOH, this makes it impossible to
       get at Object-side props of a Hash which is running
       in object-like mode.
    */
    rc =
      s2_handle_get_result(se, 0, s2_get_v(se, args->self,
                                           args->argv[0], rv));
    if(!rc && !*rv) *rv = cwal_value_undefined();
    return rc;
  }
}


/**
   Script usage: obj.set(KEY,VAL)

   Sets *rv to the VAL argument. If KEY is an integer and obj is-a
   Array then this function treats the key as an array index.

   Or:

   obj.set(OBJ2)

   to copy all keys from OBJ2 to obj.
*/
static int s2_cb_prop_set( cwal_callback_args const * args, cwal_value **rv ){
  int rc = 0;
  ARGS_SE;
  if(1 == args->argc){
    /**
       obj.set(OBJ2) copies all properties of OBJ2.           
    */
    if(!cwal_props_can(args->argv[0])){
      rc = CWAL_RC_TYPE;
      goto misuse;
    }else{
      rc = cwal_props_copy( args->argv[0], args->self );
      if(!rc) *rv = args->self;
      return rc;
    }
  }
  else if(2 == args->argc){
    assert(args->self);
    rc = s2_set_v( se, args->self,
                   args->argv[0], args->argv[1] );
    if(!rc) *rv = args->argv[1];
    return rc;
  }
  /* Else fall through */
  misuse:
  assert(rc);
  return s2_engine_err_set(se, rc,
                           "set() requires (INDEX, VALUE) "
                           "or (OBJECT) arguments.");
}

#if 0
/**
   Just an experiment.
*/
static int s2_cb_container_experiment( cwal_callback_args const * args,
                                        cwal_value **rv ){
  cwal_value * arg = args->argc ? args->argv[0] : args->self;
  s2_container_config( arg, 1, 1, 1 );
  *rv = arg;
  return 0;
}
#endif

static int s2_cb_props_copy_to( cwal_callback_args const * args,
                                cwal_value **rv ){
  if(!args->argc){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting one or more "
                               "container-type arguments.");
  }else if(!cwal_props_can(args->self)){
    return cwal_exception_setf(args->engine, CWAL_RC_TYPE,
                               "'this' is not a container type.");
  }else{
    int rc = 0;
    uint16_t i = 0;
    for( ; !rc && i < args->argc; ++i ){
      cwal_value * arg = args->argv[i];
      if(!arg/*can only happen via from-C calls*/
         || !cwal_props_can(arg)){
        rc = cwal_exception_setf(args->engine, CWAL_RC_TYPE,
                                 "Argument #%d is of type (%s), "
                                 "but we require a container type.",
                                 (int)i+1,
                                 arg ? cwal_value_type_name(arg) : "<NULL>");
      }else{
        rc = cwal_props_copy(args->self, arg)
          /* 2020-02-20: consider exposing
             s2_eval.c:cwal_kvp_visitor_props_copy_s2() for re-use
             here, primarily to improve the error reporting for const
             violations. */;
        if(!rc) *rv = arg;
        /*
          TODO: when i'm on a machine where i can change the docs to
          match: change *rv to args->self, for consistency. OTOH,
          being able to:

          var x = foo.copyPropertiesTo({});

          is convenient.
        */
      }
    }
    return rc;
  }
}


static int s2_cb_value_may_iterate( cwal_callback_args const * args,
                                    cwal_value **rv ){
  *rv = cwal_value_may_iterate(args->self)
    ? cwal_value_true()
    : cwal_value_false();
  return 0;
}

static int s2_cb_container_is_empty( cwal_callback_args const * args,
                                     cwal_value **rv ){
  *rv = cwal_props_has_any(args->self)
    ? cwal_value_false()
    : cwal_value_true();
  return 0;
}

static int s2_cb_props_count( cwal_callback_args const * args,
                                     cwal_value **rv ){
  return (*rv = cwal_new_integer(args->engine,
                                 (cwal_int_t)cwal_props_count(args->self)))
    ? 0 : CWAL_RC_OOM;
}

/**
   Internal cwal_kvp_visitor_f() implementation which requires state
   to be a fully-populated (s2_kvp_each_state*).
*/
int s2_kvp_visitor_prop_each( cwal_kvp const * kvp, void * state_ ){
  cwal_value * av[2] = {NULL,NULL} /* (key, value) arguments for callback */;
  s2_kvp_each_state * state = (s2_kvp_each_state*)state_;
  cwal_value * rv = NULL;
  int rc;
  av[0] = state->valueArgFirst
    ? cwal_kvp_value(kvp)
    : cwal_kvp_key(kvp);
  av[1] = state->valueArgFirst
    ? cwal_kvp_key(kvp)
    : cwal_kvp_value(kvp);
  assert(av[0] && av[1]);
  assert(state->e);
  assert(state->callback);
  assert(state->self);
  assert(cwal_props_can(state->self));
#if 0
  /* missing access to s2_engine object: s2_engine_sweep(state->s2); */
  cwal_engine_sweep(state->e);
#endif
#if 0
  s2_dump_val(av[0],"visiting key");
  s2_dump_val(av[1],"visiting value");
#endif
  assert(state->callback && state->self);
  rc = cwal_function_call(state->callback, state->self, &rv,
                          2, av );
  switch(rc){
    case 0:
      if(rv==cwal_value_false()/*literal false, not another falsy*/){
        rc = S2_RC_END_EACH_ITERATION;
      }
      break;
    case CWAL_RC_IS_VISITING:
    case CWAL_RC_IS_VISITING_LIST /* for hashtable entries */:
      rc = cwal_exception_setf(state->e, rc,
                               "Illegal iteration attempt detected.");
      break;
  }
  cwal_refunref(rv);
  return rc;
}

static int s2_cb_prop_each( cwal_callback_args const * args, cwal_value **rv ){
  cwal_function * f;
  f = args->argc
    ? cwal_value_function_part(args->engine, args->argv[0])
    /* cwal_value_get_function(args->argv[0]) */
    : NULL;
  if(!f){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "'eachProperty' expects a Function argument, "
                       "but got a %s.",
                       args->argc
                       ? cwal_value_type_name(args->argv[0])
                       : "<NULL>");
  }else{
    s2_kvp_each_state state = s2_kvp_each_state_empty;
    int rc;
    state.e = args->engine;
    state.callback = f;
    state.self = args->self /*better option???*/;
    rc = cwal_props_visit_kvp( args->self,
                               s2_kvp_visitor_prop_each,
                               &state );
    if(S2_RC_END_EACH_ITERATION==rc) rc = 0;
    if(!rc) *rv = args->self;
    return rc;
  }
}

static int s2_cb_with_this( cwal_callback_args const * args, cwal_value **rv ){
  cwal_function * f;
  f = args->argc
    ? cwal_value_function_part(args->engine, args->argv[0])
    /* cwal_value_get_function(args->argv[0]) */
    : NULL;
  if(!f){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "'withThis' expects a Function argument, "
                       "but got a %s.",
                       args->argc
                       ? cwal_value_type_name(args->argv[0])
                       : "<NULL>");
  }else{
#if 0
    return cwal_function_call(f, args->self, rv, 0, 0);
#elif 1
    /*
      In practice, the single most common script-side mistake when
      calling this method is forgetting to return 'this'. Also (in
      practice) callbacks must return 'this' for all current use cases
      (all others are hypothetical). So... if the function does not
      return anything (or returns undefined, since that's what script
      function calls will do rather than returning NULL), we'll return
      'this' instead...
    */
    int rc;
    *rv = 0;
    if(!(rc = cwal_function_call(f, args->self, rv, 0, 0))){
      if(!*rv || cwal_value_undefined()==*rv){
        *rv = args->self;
      }
    }
    return rc;
#else
    s2_func_state const * fst = s2_func_state_for_func(f);
    int const rc = cwal_function_call(f, args->self, rv, 0, 0);
    /* if(!rc) *rv = args->self; no, returning the callback
       result is more flexible.*/
    if(fst && !fst->lastCallReturned){
      /* if a SCRIPT func does not explicitly return then
         return 'this' instead of undefined. */
      *rv = args->self;
    }
    return rc;
#endif
  }
}

cwal_value * s2_prototype_object( s2_engine * se ){
    int rc = 0;
    cwal_value * proto;
    cwal_value * v;
    proto = cwal_prototype_base_get( se->e, CWAL_TYPE_OBJECT );
    if(proto) return proto;
    proto = cwal_new_object_value(se->e);
    if(!proto){
        rc = CWAL_RC_OOM;
        goto end;
    }
    assert(!cwal_value_prototype_get(se->e, proto));
    rc = cwal_prototype_base_set(se->e, CWAL_TYPE_OBJECT, proto );
    if(!rc) rc = s2_prototype_stash(se, "object", proto);
    if(rc) goto end;
    assert(proto == cwal_prototype_base_get(se->e, CWAL_TYPE_OBJECT));

    /* MARKER(("Setting up OBJECT prototype.\n")); */
    /*
      Chicken/egg: in order to ensure that, e.g. prototypes get added
      to Object methods, we require the Function prototype to be in
      place before those functions are installed. This applies to all
      Container-level prototypes, but not to PODs because those are
      looked up on demand, as opposed to being set when the value is
      created, because non-Containers don't have a place to store their
      prototype.

      So we'll init the other prototypes right after we set the base
      Object prototype (which is used, indirectly, by all other
      types).
     */
#define PROTO(F) if(!(v = F)) { \
      rc = se->e->err.code ? se->e->err.code : CWAL_RC_OOM; goto end; \
    } (void)0
    PROTO(s2_prototype_function(se));
    PROTO(s2_prototype_array(se));
    PROTO(s2_prototype_exception(se));
    PROTO(s2_prototype_hash(se));
    PROTO(s2_prototype_buffer(se));
    PROTO(s2_prototype_string(se));
    PROTO(s2_prototype_integer(se));
    PROTO(s2_prototype_double(se));
    /* PROTO(s2_prototype_unique(se)); */
    /* PROTO(s2_prototype_tuple(se)); */
#undef PROTO


#define CHECKV if(!v){ rc = CWAL_RC_OOM; goto end; } (void)0
#define RC if(rc) goto end;

#if 0
    v = cwal_new_string_value(se->e, "Objekt", 6);
    CHECKV;
    rc = cwal_prop_set_v( proto, se->cache.keyTypename, v );
    RC;
#endif

    {
      s2_func_def const funcs[] = {
      /* S2_FUNC2("experiment", s2_cb_container_experiment), */
      S2_FUNC2("clearProperties", s2_cb_prop_clear),
      S2_FUNC2("copyPropertiesTo", s2_cb_props_copy_to),
      S2_FUNC2("compare", s2_cb_value_compare),
      S2_FUNC2("eachProperty", s2_cb_prop_each),
      S2_FUNC2("get", s2_cb_prop_get),
      S2_FUNC2("hasOwnProperty", s2_cb_prop_has_own),
      S2_FUNC2("mayIterate", s2_cb_value_may_iterate),
      S2_FUNC2("isEmpty", s2_cb_container_is_empty),
      S2_FUNC2("propertyKeys", s2_cb_prop_keys),
      S2_FUNC2("propertyCount", s2_cb_props_count),
      S2_FUNC2("set", s2_cb_prop_set),
      S2_FUNC2("toJSONString", s2_cb_this_to_json_token),
      S2_FUNC2("toString", s2_cb_value_to_string),
      S2_FUNC2("unset", s2_cb_prop_unset),
      S2_FUNC2("withThis", s2_cb_with_this),
      s2_func_def_empty_m
      };
      rc = s2_install_functions(se, proto, funcs, 0);
      if(!rc) rc = s2_ctor_callback_set(se, proto,
                                        s2_cb_object_ctor);
    }

#undef CHECKV
#undef RC
    end:
    return rc ? NULL : proto;
}

static int s2_cb_exception_code_string( cwal_callback_args const * args,
                                        cwal_value **rv ){
  int rc = 0;
  cwal_value * v;
  v = args->argc ? args->argv[0] : cwal_prop_get(args->self, "code", 4);
  if(v){
    s2_engine * se = s2_engine_from_args(args);
    cwal_value *hv = s2_stash_get(se, "RcHash")
      /* Optimization: if the stashed RcHash (set up in s2.c) is
         available, check it first. This avoids having to allocate
         x-strings which we know are already in that hash. It also
         incidentally supports a reverse mapping, such that passing in
         the string 'CWAL_RC_OOM' will return its integer value.
      */;
    *rv = 0;
    if(hv){
      cwal_hash * h = cwal_value_get_hash(hv);
      assert(h);
      *rv = cwal_hash_search_v(h, v);
    }
    if(!*rv){
      cwal_size_t vstrLen = 0;
      char const * const vstr = cwal_value_get_cstr(v, &vstrLen);
      if(vstr){ /* If passed a string, try (the hard way) to
                   find the integer code. */
        int code = 0;
        if(s2_cstr_to_rc(vstr, (cwal_int_t)vstrLen, &code)){
          if(! (*rv = cwal_new_integer(args->engine, (cwal_int_t)code)) ){
            rc = CWAL_RC_OOM;
          }
        }
        if(!rc && !*rv) *rv = cwal_value_undefined();
      }else{
        /* Assume the code is an integer and get its string
           form. */
        cwal_int_t const code = cwal_value_get_integer(v);
        char const * str = cwal_rc_cstr2((int)code);
        *rv = str
          ? cwal_new_xstring_value(args->engine, str,
                                   cwal_strlen(str))
          /* Reminder to self: cwal recycles that xstring shell
             to those static bytes, so this doesn't actually
             allocate anything so long as we've got an x/z-string
             shell in the recycler bin :-D.
          */
          : cwal_value_null();
        if(str && !*rv) rc = CWAL_RC_OOM;
      }
    }
  }else{
    /* No arg or "code" property. */
    *rv = cwal_value_undefined();
  }
  return rc;
}

cwal_value * s2_prototype_exception( s2_engine * se ){
    int rc = 0;
    cwal_value * proto;
    proto = cwal_prototype_base_get( se->e, CWAL_TYPE_EXCEPTION );
    if(proto
       || !s2_prototype_object(se) /* timing hack */
       ) return proto;
    proto = cwal_new_object_value(se->e);
    if(!proto){
        rc = CWAL_RC_OOM;
        goto end;
    }
    rc = cwal_prototype_base_set(se->e, CWAL_TYPE_EXCEPTION, proto );
    if(!rc) rc = s2_prototype_stash(se, "Exception", proto);
    if(rc) goto end;
    assert(proto == cwal_prototype_base_get(se->e, CWAL_TYPE_EXCEPTION));
    /* MARKER(("Setting up EXCEPTION prototype.\n")); */

#define CHECKV if(!v){ rc = CWAL_RC_OOM; goto end; } (void)0
#define RC if(rc) goto end;

    {
      s2_func_def const funcs[] = {
      S2_FUNC2("codeString", s2_cb_exception_code_string),
      s2_func_def_empty_m
      };
      rc = s2_install_functions(se, proto, funcs, 0);
    }


#undef FUNC2
#undef CHECKV
#undef RC
    end:
    return rc ? NULL : proto;
}

#undef MARKER
#undef ARGS_SE
