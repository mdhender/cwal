/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

/**
   Impls for the core object prototype.
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

int whcl__prototype_stash( whcl_engine * const el,
                          char const * typeName,
                          cwal_value * const proto ){
  int rc;
  if(!el->cache.vProtos){
    cwal_value * const v = cwal_new_object_value(el->ec);
    if(!v){ WHCL__WARN_OOM; return CWAL_RC_OOM; }
    cwal_ref(v);
    cwal_value_prototype_set(v, NULL);
    rc = cwal_prop_set_with_flags(el->cache.vWhcl, "prototypes", 10, v,
                                  CWAL_VAR_F_CONST);
    cwal_unref(v);
    if(rc) return rc;
    el->cache.vProtos = v;
  }
  return cwal_prop_set_with_flags(el->cache.vProtos, typeName,
                                  cwal_strlen(typeName), proto,
                                  CWAL_VAR_F_CONST);
}

cwal_value * whcl__prototype_stashed( whcl_engine * const el,
                                      char const * typeName ){
  return el->cache.vProtos
    ? cwal_prop_get( el->cache.vProtos, typeName, cwal_strlen(typeName) )
    : NULL;
}

int whcl_cb_value_compare( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  cwal_value * lhs;
  cwal_value * rhs;
  if(!args->argc || (args->argc>3)){
    return cwal_exception_setf( args->engine, CWAL_RC_MISUSE,
                                "Expecting (value [,value [,bool typeStrict]]) arguments.");
  }
  lhs = (args->argc > 1)
    ? args->argv[0]
    : args->self;
  rhs = (args->argc > 1)
    ? args->argv[1]
    : args->argv[0];
  if(lhs==rhs){
    *rv = cwal_new_integer(args->engine, 0) /* does not allocate */;
    return 0;
  }
  if((3==args->argc) && cwal_value_get_bool(args->argv[2])){
    cwal_type_id const tL = cwal_value_type_id(lhs);
    cwal_type_id const tR = cwal_value_type_id(rhs);
    if(tL != tR){
      rc = (int)(tL-tR);
      *rv = cwal_new_integer(args->engine, (cwal_int_t)(tL<tR ? -1 : 1))
        /* does not allocate */;
      return 0;
    }
  }
  rc = cwal_value_compare(lhs, rhs);
  /* Minor optimization:
     
     Normalize rc to one of (-1, 0, 1) because we just happen to
     know that cwal_new_integer() does not allocate for those 3
     values. cwal_value_compare() does not guaranty any specific
     values, only general-purpose comaparator semantics.
  */
  *rv = cwal_new_integer(args->engine, (cwal_int_t)(rc>0 ? 1 : (rc < 0 ? -1 : 0)))
    /* does not allocate */;
  return 0;
}


int whcl_immutable_container_check( whcl_engine * const el,
                                    cwal_value const * v, bool throwIt ){
  cwal_flags16_t const containerFlags = cwal_container_flags_get(v);
  if(CWAL_CONTAINER_DISALLOW_PROP_SET & containerFlags){
    char const * fmt = "Setting/clearing properties is "
      "disallowed on this container (of type '%s').";
    char const * typeName = cwal_value_type_name(v);
    if(!typeName) typeName = "<? ? ?>";
    if(throwIt){
      return cwal_exception_setf(el->ec, CWAL_RC_DISALLOW_PROP_SET,
                                 fmt, typeName);
    }else{
      return whcl_err_set(el, CWAL_RC_DISALLOW_PROP_SET,
                          fmt, typeName);
    }
  }
  return 0;
}

int whcl__immutable_container_check_cb( cwal_callback_args const * args, cwal_value const * v ){
  return whcl_immutable_container_check( whcl_engine_from_args(args), v, true );
}


static int whcl__cb_prop_clear( cwal_callback_args const * args, cwal_value **rv ){
    int rc = whcl__immutable_container_check_cb(args, args->self);
    if( !rc && (rc = cwal_props_clear( args->self )) ){
      rc = cwal_cb_throw(args, rc, "Clearing properties failed "
                         "with code %s.", cwal_rc_cstr(rc));
    }
    if(!rc) *rv = args->self;
    return rc;
}

static int whcl__cb_props_copy_to( cwal_callback_args const * args,
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
        rc = cwal_props_copy(args->self, arg);
        if(rc){
          rc = cwal_exception_setf(args->engine, rc,
                                   "Property-copy failed with code %s.",
                                   cwal_rc_cstr(rc));
        }else{
          *rv = arg;
        }
      }
    }
    return rc;
  }
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
static int whcl__cb_prop_get( cwal_callback_args const * args,
                              cwal_value **rv ){
  int rc;
  WHCL__E_ARGS;
  if( (rc = whcl__immutable_container_check_cb(args, args->self)) ) return rc;
  else if(!args->argc){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "'get' requires (INDEX|KEY) argument.");
  }else{
    /* Reminder: we use s2_get_v() instead of cwal_prop_get_v()
       for the prototype pseudo-property and integer array
       index support. */
    *rv = NULL;
    rc = whcl_lookup_vsym_v(el, args->self, args->argv[0], false,
                            rv);
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
static int whcl__cb_prop_set( cwal_callback_args const * args, cwal_value **rv ){
  int rc = 0;
  WHCL__E_ARGS;
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
    rc = whcl_set_v( el, args->self,
                     args->argv[0], args->argv[1] );
    if(!rc) *rv = args->argv[1];
    return rc;
  }
  /* Else fall through */
  misuse:
  assert(rc);
  return whcl_err_throw(el, rc,
                        "set() requires (INDEX, VALUE) "
                        "or (OBJECT) arguments.");
}


/**
   Script usage: obj.unset(KEY [, ... KEY])

   If obj is-a Array and KEY is-a Integer then the key is interpreted
   as an array index. Sets *rv to args->self.
*/
static int whcl__cb_prop_unset( cwal_callback_args const * args, cwal_value **rv ){
  int rc = 0;
  uint16_t i = 0;
  WHCL__E_ARGS;
  if( (rc = whcl__immutable_container_check_cb(args, args->self)) ) return rc;
  if(!args->argc){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "'unset' requires (INDEX|KEY,...) arguments.");
  }
  for( ; !rc && (i < args->argc); ++i ){
    rc = whcl_set_v( el, args->self, args->argv[i], NULL );
  }
  if(!rc) *rv = args->self;
  return rc;
}


static int whcl__cb_prop_has_own( cwal_callback_args const * args,
                                  cwal_value **rv ){
  if(!args->argc)
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "Expecting a property key.");
  else {
    *rv = cwal_new_bool(cwal_prop_has_v(args->self, args->argv[0], 0));
    return 0;
  }
}

static int whcl__cb_container_is_empty( cwal_callback_args const * args,
                                        cwal_value **rv ){
  *rv = cwal_props_has_any(args->self)
    ? cwal_value_false()
    : cwal_value_true();
  return 0;
}

static int whcl__value_visit_append_to_array( cwal_value * v, void * state ){
  return cwal_array_append((cwal_array *)state, v);
}

/**
   Script usage: var keys = obj.propertyKeys()

   Returns an array containing all keys for all key/value pairs for
   obj.
*/
static int whcl__cb_prop_keys( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  cwal_array * ar;
  if(!cwal_props_can(args->self)){
    return cwal_cb_throw(args, CWAL_RC_TYPE,
                         "This value (of type %s) cannot hold properties.",
                         cwal_value_type_name(args->self));
  }
  ar = cwal_new_array(args->engine);
  if(!ar){
    WHCL__WARN_OOM;
    return CWAL_RC_OOM;
  }
  rc = cwal_props_visit_keys( args->self,
                              whcl__value_visit_append_to_array, ar );
  if(rc) cwal_array_unref(ar);
  else *rv = cwal_array_value(ar);
  return rc;
}

static int whcl__cb_props_count( cwal_callback_args const * args,
                                 cwal_value **rv ){
  return (*rv = cwal_new_integer(args->engine,
                                 (cwal_int_t)cwal_props_count(args->self)))
    ? 0 : CWAL_RC_OOM;
}

static int whcl__cb_with_this( cwal_callback_args const * args, cwal_value **rv ){
  cwal_function * f;
  f = args->argc
    ? cwal_value_function_part(args->engine, args->argv[0])
    : NULL;
  if(!f){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "'with-this' expects a Function argument, "
                         "but got a %s.",
                         args->argc
                         ? cwal_value_type_name(args->argv[0])
                         : "<NULL>");
  }else{
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
  }
}


cwal_value * whcl_prototype_object(whcl_engine * const el){

  int rc = 0;
  cwal_value * proto;
  cwal_value * v;
  proto = cwal_prototype_base_get( el->ec, CWAL_TYPE_OBJECT );
  if(proto) return proto;
  proto = cwal_new_object_value(el->ec);
  if(!proto){
    WHCL__WARN_OOM;
    rc = CWAL_RC_OOM;
    goto end;
  }
  assert(!cwal_value_prototype_get(el->ec, proto));
  rc = cwal_prototype_base_set(el->ec, CWAL_TYPE_OBJECT, proto );
  if(!rc) rc = whcl__prototype_stash(el, "Object", proto);
  if(rc) goto end;
  assert(proto == cwal_prototype_base_get(el->ec, CWAL_TYPE_OBJECT));

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
#define PROTO(F) if(!(v = F)) {                                     \
    WHCL__WARN_OOM;                                                   \
    rc = el->ec->err.code ? el->ec->err.code : CWAL_RC_OOM; goto end; \
  } (void)0
  PROTO(whcl_prototype_function(el));
  PROTO(whcl_prototype_array(el));
  PROTO(whcl_prototype_tuple(el));
  PROTO(whcl_prototype_string(el));
  PROTO(whcl_prototype_integer(el));
  PROTO(whcl_prototype_double(el));
  PROTO(whcl_prototype_buffer(el));
  PROTO(whcl_prototype_exception(el));
  /* Reminder to self: many other APIs are not loaded by default,
     but via whcl_install_api(). */
#if 0
  PROTO(whcl_prototype_exception(el));
#endif
#undef PROTO

#define CHECKV if(!v){ rc = CWAL_RC_OOM; goto end; } (void)0
#define RC if(rc) goto end;

  {
    whcl_func_def const funcs[] = {
      WHCL_FUNC2("clear-properties", whcl__cb_prop_clear),
      WHCL_FUNC2("copy-properties-to", whcl__cb_props_copy_to),
      WHCL_FUNC2("get", whcl__cb_prop_get),
      WHCL_FUNC2("has-own-property", whcl__cb_prop_has_own),
      WHCL_FUNC2("is-empty", whcl__cb_container_is_empty),
      WHCL_FUNC2("property-keys", whcl__cb_prop_keys),
      WHCL_FUNC2("set", whcl__cb_prop_set),
      WHCL_FUNC2("property-count", whcl__cb_props_count),
      WHCL_FUNC2("to-json", whcl_cb_this_to_json_token),
      WHCL_FUNC2("to-string", whcl_cb_value_to_string),
      WHCL_FUNC2("unset", whcl__cb_prop_unset),
      WHCL_FUNC2("with-this", whcl__cb_with_this),
      whcl_func_def_empty_m
    };
    rc = whcl_install_functions(el, proto, funcs, CWAL_VAR_F_CONST);
    RC;
    rc = whcl_install_command_cb(el, proto);
    RC;
    //if(!rc) rc = s2_ctor_callback_set(el, proto, s2_cb_object_ctor);
  }

#undef CHECKV
#undef RC
  end:
  return rc ? NULL : proto;
}

#undef MARKER
#undef WHCL__E_ARGS
