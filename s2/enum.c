/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

#include "s2_internal.h"
#include "wh/cwal/cwal_printf.h"
#include <assert.h>

#if 1
#include <stdio.h>
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__); if(1) printf pfexp
#else
#define MARKER(pfexp) (void)0
#endif

#define HAS_ENUM_FLAG(ClientFlags) \
  ((S2_VAL_F_MODE_CLASS & ClientFlags) \
   && (S2_VAL_F_CLASS_ENUM & ClientFlags))

enum s2_enum_builder_state {
S2_ENUM_STATE_UNINITED = 0,
S2_ENUM_STATE_INITED = 1,
S2_ENUM_STATE_SEALED = 2
};

int s2_enum_builder_init( s2_engine * se, s2_enum_builder * eb,
                          char const * typeName,
                          cwal_size_t entryCountHint){
  int rc = 0;
  s2_enum_builder_cleanup(eb);
  eb->se = se;
  if(!entryCountHint) entryCountHint = 6;
  entryCountHint = cwal_next_prime(2 * (entryCountHint
                                        ? entryCountHint-1 : 6) );
  eb->entries = cwal_new_hash_value(se->e, entryCountHint);
  if(eb->entries){
    s2_hash_dot_like_object(eb->entries, 1);
  }else{
    return CWAL_RC_OOM;
  }
  cwal_value_ref(eb->entries) /* we'll hold this ref for the whole
                                 enum build process */;
  if(typeName && *typeName){
    cwal_value * key = cwal_new_string_value(se->e, typeName,
                                             cwal_strlen(typeName));
    if(key){
      cwal_value_ref(key);
      rc = cwal_prop_set_with_flags_v( eb->entries, se->cache.keyTypename,
                                       key,
                                       CWAL_VAR_F_CONST | CWAL_VAR_F_HIDDEN );
      cwal_value_unref(key);
    }else{
      rc = CWAL_RC_OOM;
    }
    if(rc) goto err;
  }
  eb->flags = S2_ENUM_STATE_INITED;
  cwal_value_make_vacuum_proof(eb->entries,1);
  return 0;
  err:
  s2_enum_builder_cleanup(eb);
  assert(rc);
  return rc;
}

void s2_enum_builder_cleanup( s2_enum_builder * eb ){
  if(eb->entries){
    assert(eb->se);
    assert(cwal_value_refcount(eb->entries));
    cwal_value_make_vacuum_proof(eb->entries,0);
    cwal_value_unref(eb->entries);
  }
  *eb = s2_enum_builder_empty;  
}

int s2_enum_builder_append_v( s2_enum_builder * eb,
                              cwal_value * key,
                              cwal_value * wrappedVal ){
  cwal_value * uval;
  int rc;
  if(S2_ENUM_STATE_INITED != eb->flags){
    return CWAL_RC_MISUSE;
  }else if(!key || !wrappedVal){
    return CWAL_RC_MISUSE;
  }
  assert(eb->flags>0);
  assert(eb->entries);
  uval = cwal_new_unique(eb->se->e, wrappedVal);
  if(!uval){
    rc = CWAL_RC_OOM;
  }else{
    cwal_value_ref(uval);
    rc = s2_set_with_flags_v( eb->se, eb->entries, key,
                              uval, CWAL_VAR_F_CONST );
    if(!rc){
      rc = s2_set_with_flags_v( eb->se, eb->entries, uval, key,
                                CWAL_VAR_F_CONST | CWAL_VAR_F_HIDDEN );
    }
    cwal_value_unref(uval);
    if(!rc){
      ++eb->entryCount;
    }
  }
  return rc;
}

int s2_enum_builder_append( s2_enum_builder * eb,
                            char const * entryName,
                            cwal_value * val){
  cwal_value * key = 0;
  cwal_engine * e = eb->se->e;
  if(S2_ENUM_STATE_INITED != eb->flags){
    return CWAL_RC_MISUSE;
  }else if(!entryName){
    return CWAL_RC_MISUSE;
  }
  key = cwal_new_string_value(e, entryName,
                              cwal_strlen(entryName));
  if(!key) return CWAL_RC_OOM;
  else{
    int rc;
    cwal_value_ref(key);
    rc = s2_enum_builder_append_v(eb, key, val);
    cwal_value_unref(key);
    return rc;
  }
}

int s2_enum_builder_seal( s2_enum_builder * eb, cwal_value **rv ){
  int rc = 0;
  cwal_value * enumProto;
  if(S2_ENUM_STATE_INITED != eb->flags){
    return CWAL_RC_MISUSE;
  }else if(!eb->entryCount){
    return CWAL_RC_RANGE;
  }
  enumProto = s2_prototype_enum(eb->se);
  if(!enumProto) return CWAL_RC_OOM;
  assert(eb->entries);
  assert(eb->flags > 0);
#if 0
  v = cwal_new_integer(eb->se->e, eb->entryCount);
  if(!v) rc = CWAL_RC_OOM;
  else{
    cwal_value_ref(v);
    rc = cwal_prop_set_with_flags(eb->entries, "enumEntryCount", 14, v,
                                  CWAL_VAR_F_CONST);
    cwal_value_unref(v);
    v = 0;
  }
  if(rc) goto end;
#endif
  rc = cwal_value_prototype_set(eb->entries, enumProto);
  if(!rc){
    cwal_container_flags_set(eb->entries,
                             CWAL_CONTAINER_DISALLOW_NEW_PROPERTIES
                             | CWAL_CONTAINER_DISALLOW_PROP_SET
                             /* TODO:???
                                | CWAL_CONTAINER_DISALLOW_PROTOTYPE_SET */
                             );
    cwal_container_client_flags_set(eb->entries, S2_VAL_F_ENUM);
    eb->flags = S2_ENUM_STATE_SEALED;
    if(rv){
      *rv = eb->entries;
      cwal_value_ref(*rv);
      s2_enum_builder_cleanup(eb) /* will unref() and de-vacuum-safe
                                     eb->entries */;
      assert(!cwal_value_is_vacuum_proof(*rv));
      cwal_value_unhand(*rv);
    }
  }
  /* end: */
  return rc;
}


/* in s2_protos.c */
int s2_kvp_visitor_prop_each( cwal_kvp const * kvp, void * state_ );

/* KVP Visitor for enums */
static int s2_enum_visitor_each( cwal_kvp const * kvp, void * state_ ){
  /* Reminder: the (unique==>string) pairs are hidden, so they're
     not iterated over. */
  if(cwal_value_is_unique(cwal_kvp_value(kvp))){
    return cwal_value_is_string(cwal_kvp_key(kvp))
      /* so we pass (V,K) to the callback, and only for (Unique==>Name)
         pairs. */
      ? s2_kvp_visitor_prop_each(kvp, state_)
      : 0;
  }else{
    return 0;
  }
}

static int s2_cb_enum_each( cwal_callback_args const * args, cwal_value **rv ){
  cwal_function * f;
  f = args->argc
    ? cwal_value_function_part(args->engine, args->argv[0])
    : NULL;
  if(!f){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting a Function argument, "
                       "but got a %s.",
                       args->argc
                       ? cwal_value_type_name(args->argv[0])
                       : "<NULL>");
  }else{
    s2_kvp_each_state state = s2_kvp_each_state_empty;
    int rc;
    cwal_hash * h = cwal_value_hash_part(args->engine, args->self);
    state.e = args->engine;
    state.callback = f;
    state.self = args->self;
    /* state.valueArgFirst = 1; */
    rc = cwal_hash_visit_kvp( h, s2_enum_visitor_each, &state );
    if(S2_RC_END_EACH_ITERATION==rc) rc = 0;
    if(!rc) *rv = args->self;
    return rc;
  }
}

cwal_value * s2_value_enum_part( s2_engine * se, cwal_value * v ){
    do{
      if(HAS_ENUM_FLAG(cwal_container_client_flags_get(v))) return v;
      else v = cwal_value_prototype_get(se->e, v);
    }while(v);
    return 0;
}

int s2_value_is_enum( const cwal_value * v ){
  return HAS_ENUM_FLAG(cwal_container_client_flags_get(v));
}

/** Internal helper for s2_enum_from_object() (==> efo) */
static int cwal_kvp_visitor_f_efo( cwal_kvp const * kvp, void * state ){
  s2_enum_builder * eb = (s2_enum_builder *)state;
  return s2_enum_builder_append_v( eb, cwal_kvp_key(kvp), cwal_kvp_value(kvp) );
}

/**
   UNTESTED!

   Copies all properties from src to a new enum value.

   The typeName parameter is interpreted as documented for
   s2_enum_builder_init(). It may be NULL.

   On success, *rv is assigned to the new enum value. On error, *rv is
   not modified and non-0 is returned. Returns CWAL_RC_MISUSE if any
   pointer argument is NULL, CWAL_RC_RANGE if src has no properties
   (empty enums are not allowed), CWAL_RC_OOM if an allocation fails,
   and possibly other CWAL_RC_xxx codes from the underlying
   s2_enum_builder API calls.

   On success, *rv becomes the caller's responsibility: it is a new
   value with a refcount of 0.
*/
int s2_enum_from_object( s2_engine * se, cwal_value * src,
                         char const * typeName, cwal_value **rv ){
  int rc;
  cwal_value * enu = 0;
  cwal_size_t nKeys = 0;
  s2_enum_builder eb = s2_enum_builder_empty;
  if(!se || !src || !rv) return CWAL_RC_MISUSE;
  nKeys = cwal_props_count(src);
  if(0==nKeys){
    return s2_engine_err_set(se, CWAL_RC_RANGE,
                             "Empty enum is not permitted.");
  }
  rc = s2_enum_builder_init( se, &eb, typeName, nKeys );
  if(!rc){
    rc = cwal_props_visit_kvp( src, cwal_kvp_visitor_f_efo, &eb );
    if(!rc){
      rc = s2_enum_builder_seal( &eb, &enu );
      cwal_value_ref(enu);
    }
  }
  s2_enum_builder_cleanup(&eb);
  if(rc){
    cwal_value_unref(enu);
  }else{
    assert(enu);
    cwal_value_unhand(enu);
    *rv = enu;
  }
  return rc;
}

#define THIS_ENUM                                                  \
  s2_engine * se = s2_engine_from_args(args);                      \
  cwal_value * eObj = s2_value_enum_part(se, args->self);          \
  cwal_hash * eHash = eObj ? cwal_value_get_hash(eObj) : 0;        \
  if(!eHash) return cwal_exception_setf( args->engine, CWAL_RC_TYPE, \
                                         "Expecting an enum as 'this'." )

/**
   Specialized enum prop search which only searches in the enum part
   of theEnum (which may be an enum-derived value). Returns the value
   it finds or 0 if it does not find one.
*/
static cwal_value * s2_enum_search( s2_engine * se, cwal_value * theEnum,
                                    cwal_value * key){
  cwal_hash * h;
  theEnum = s2_value_enum_part(se, theEnum);
  h = theEnum ? cwal_value_get_hash(theEnum) : 0;
  return h ? cwal_hash_search_v(h, key) : 0;
}

/**
   Internal helper for s2_cb_enum_keys().
*/
static int s2_kvp_visit_enum_keys_to_array( cwal_kvp const * kvp, void * state ){
  return cwal_value_is_unique(cwal_kvp_value(kvp))
    ? cwal_array_append((cwal_array *)state, cwal_kvp_key(kvp))
    : 0;
}

/**
   Returns an array containing all enum entry keys for
   obj.
*/
static int s2_cb_enum_keys( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  cwal_array * ar;
  THIS_ENUM;
  ar = cwal_new_array(args->engine);
  if(!ar) return CWAL_RC_OOM;
  rc = cwal_hash_visit_kvp( eHash, s2_kvp_visit_enum_keys_to_array, ar );
  if(!rc) *rv = cwal_array_value(ar);
  else cwal_array_unref(ar);
  return rc;
}

static int s2_cb_enum_contains( cwal_callback_args const * args, cwal_value **rv ){
  THIS_ENUM;
  if(1 != args->argc){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting exactly one argument.");
  }
  *rv = cwal_new_bool( !!s2_enum_search(se, eObj, args->argv[0]) );
  return 0;
}

static int s2_cb_enum_op_arrow( cwal_callback_args const * args, cwal_value **rv ){
  THIS_ENUM;
  assert(1==args->argc && "expecting operator call usage.");
  if(!(*rv = s2_enum_search( se, eObj, args->argv[0] ))){
    *rv = cwal_value_undefined();
  }
  return 0;  
}

static int s2_cb_enum_op_dotdot( cwal_callback_args const * args, cwal_value **rv ){
  cwal_value * entry;
  cwal_value * key;
  int rc = 0;
  THIS_ENUM;
  assert(1==args->argc && "expecting operator call usage.");
  key = args->argv[0];
  if(!cwal_value_is_string(key)
     /* && !cwal_value_is_number(key) */){
	return cwal_cb_throw(args, CWAL_RC_TYPE,
		              "Invalid type for enum 'operator::' RHS: "
		              "expecting a string/identifier");
  }
  entry = s2_enum_search( se, eObj, key );
  if(entry){
    if(cwal_value_is_unique(entry)){
	  if(!(*rv = cwal_unique_wrapped_get(entry))) *rv = cwal_value_undefined();
    }else{
      cwal_size_t len = 0;
      char const * cKey = cwal_value_get_cstr(key, &len);
      rc = cwal_cb_throw(args, CWAL_RC_TYPE,
                      "Property '%.*s' is (somehow) not an enum entry.",
	                  (int)len, cKey);
    }
  }else{
    cwal_size_t len = 0;
    char const * cKey = cwal_value_get_cstr(key, &len);
    rc = cwal_cb_throw(args, CWAL_RC_NOT_FOUND,
	                "No such enum entry: '%.*s'",
	                (int)len, cKey);
  }
  return rc;  
}


cwal_value * s2_prototype_enum(s2_engine * se){
  static char const * protoKey = "Enum";
  cwal_value * proto = s2_prototype_stashed( se, protoKey );
  if(proto) return proto;
  proto = cwal_new_object_value(se->e);
  if(!proto) return 0;
  else{
    int rc;
    cwal_value_ref(proto);
    cwal_value_prototype_set(proto, 0)
      /* so it does not derive from Object */;
    rc = s2_typename_set(se, proto, "enum", 4);
    if(!rc){
      const s2_func_def funcs[] = {
      S2_FUNC2("eachEnumEntry", s2_cb_enum_each),
      S2_FUNC2("getEnumKeys", s2_cb_enum_keys),
      S2_FUNC2("hasEnumEntry", s2_cb_enum_contains),
      S2_FUNC2("operator->", s2_cb_enum_op_arrow),
      S2_FUNC2("operator::", s2_cb_enum_op_dotdot),
      s2_func_def_empty_m
      };
      rc = s2_install_functions(se, proto, funcs,
                                CWAL_VAR_F_CONST);
    }
    if(rc || (rc=s2_prototype_stash(se, protoKey, proto))){
      cwal_value_unref(proto);
      proto = 0;
    }else{
      cwal_value_unhand(proto);
      assert(cwal_value_refcount(proto) && "But... the s2_stash ref?");
    }
    return proto;
  }
}


#undef MARKER
#undef HAS_ENUM_FLAG
#undef THIS_ENUM
