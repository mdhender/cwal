/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#include <assert.h>
#include "s2_internal.h"

#if 1
#include <stdio.h>
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__); if(1) printf pfexp
#else
#define MARKER(pfexp) (void)0
#endif

static int s2_new_hash( cwal_engine * e, cwal_int_t hashSize,
                        int setDotFlag /* experiment! */, cwal_value ** rv ){
  cwal_hash * h = 0;
  cwal_value * hv = 0;
  if(hashSize<=0) hashSize = 17;
  else if(hashSize>7919) hashSize = 7919;
#if 0
  else hashSize = s2_hash_next_prime(hashSize);
#endif
  h = cwal_new_hash(e, hashSize);
  hv = h
    ? cwal_hash_value(h)
    : NULL;
  if(!hv){
    assert(!h);
    return CWAL_RC_OOM;
  }
  else {
    assert(cwal_value_is_hash(hv));
    assert(cwal_value_prototype_get(e,hv));
    if(setDotFlag){
      s2_hash_dot_like_object(hv, 1);
    }
    *rv = hv;
    return 0;
  }
}


#define ARGS_SE s2_engine * se = s2_engine_from_args(args); \
    assert(se)

#define THIS_HASH                                  \
    cwal_hash * h = 0;                          \
    cwal_value * hv = 0;                        \
    ARGS_SE; \
    h = cwal_value_hash_part(se->e, args->self); \
    hv = h ? cwal_hash_value(h) : 0;               \
    if(!h || !hv){ \
        return s2_throw( se, CWAL_RC_TYPE, \
                         "'this' is-not-a Hash." ); \
    } (void)0

static int s2_cb_hash_create( cwal_callback_args const * args, cwal_value **rv ){
  cwal_int_t hsz = 0;
  int const setDotFlag = 
#if 1
    0
#else
    (args->argc>1)
    ? cwal_value_get_bool(args->argv[1])
    : 0
#endif
    ;
  hsz = (args->argc>0)
    ? cwal_value_get_integer(args->argv[0])
    : 0;
  return (hsz<0)
    ? cwal_exception_setf(args->engine, CWAL_RC_RANGE,
                          "Expecting a positive integer value "
                          "for hash table size.")
    : s2_new_hash(args->engine, hsz, setDotFlag, rv );
}

/**
   Internal impl of s2_cp_hash_keys/values(). mode==0 means
   keys, anything else means values.
*/
static int s2_cb_hash_kv( cwal_callback_args const * args,
                          char mode,
                          cwal_value **rv ){
  int rc;
  cwal_array * ar;
  THIS_HASH;
  ar = cwal_new_array(args->engine);
  if(!ar) return CWAL_RC_OOM;
  if(!mode){
    rc = cwal_hash_visit_keys( h,
                               s2_value_visit_append_to_array,
                               ar );
  }else{
    rc = cwal_hash_visit_values( h,
                                 s2_value_visit_append_to_array,
                                 ar );
  }
  if(!rc) *rv = cwal_array_value(ar);
  else cwal_array_unref(ar);
  return rc;
}

static int s2_cb_hash_keys( cwal_callback_args const * args, cwal_value **rv ){
  return s2_cb_hash_kv(args, 0, rv);
}

static int s2_cb_hash_values( cwal_callback_args const * args, cwal_value **rv ){
    return s2_cb_hash_kv(args, 1, rv);
}

static int s2_cb_hash_entry_count( cwal_callback_args const * args,
                                       cwal_value **rv ){
  THIS_HASH;
  *rv = cwal_new_integer(args->engine,
                         (cwal_int_t)cwal_hash_entry_count(h));
  return *rv ? 0 : CWAL_RC_OOM;
}


int s2_cb_hash_insert( cwal_callback_args const * args, cwal_value **rv ){
  THIS_HASH;
  if(2!=args->argc) return s2_throw(se,
                                    CWAL_RC_MISUSE,
                                    "insert() requires (KEY,VALUE) "
                                    "arguments.");
  else{
    int rc = 0;
    rc = cwal_hash_insert_v( h, args->argv[0],
                             args->argv[1], 1 );
    if(!rc) *rv = args->argv[1];
    else switch(rc){
        case CWAL_RC_ACCESS:
          rc = s2_throw(se, rc, "May not modify a hash while it "
                        "is being iterated over.");
          break;
        default:
          break;
      }
    /* if(!rc) *rv = args->self; */
    return rc;
  }
}

int s2_cb_hash_get( cwal_callback_args const * args, cwal_value **rv ){
  cwal_value * v;
  THIS_HASH;
  if(1!=args->argc){
    return s2_throw(se, CWAL_RC_MISUSE,
                    "get() requires (KEY) argument.");
  }
  v = cwal_hash_search_v( h, args->argv[0] );
  *rv = v ? v : cwal_value_undefined();
  return 0;
}

int s2_cb_hash_has( cwal_callback_args const * args, cwal_value **rv ){
  THIS_HASH;
  if(!args->argc) return s2_throw(se, CWAL_RC_MISUSE,
                                  "has() expects 1 argument.");
  else {
    *rv = cwal_hash_search_v( h, args->argv[0] )
      ? cwal_value_true()
      : cwal_value_false();
    return 0;
  }
}

static int s2_cb_hash_clear( cwal_callback_args const * args, cwal_value **rv ){
  char clearProps;
  THIS_HASH;
  clearProps = (args->argc>0)
    ? cwal_value_get_bool(args->argv[0])
    : 0;
  cwal_hash_clear(h, clearProps);
  *rv = args->self;
  return 0;
}


int s2_cb_hash_remove( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  THIS_HASH;
  if(1!=args->argc){
    return s2_throw(se, CWAL_RC_MISUSE,
                    "remove() requires (KEY) argument.");
  }
  rc = cwal_hash_remove_v( h, args->argv[0] );
  if(!rc) *rv = cwal_value_true();
  else switch(rc){
      case CWAL_RC_ACCESS:
        rc = s2_throw(se, rc, "May not modify a hash while it "
                      "is being iterated over.");
        break;
      case CWAL_RC_NOT_FOUND:
        *rv = cwal_value_false();
        rc = 0;
        break;
      default:
        break;
    } /* seriously, emacs? You're going to do that to me? */
  return rc;
}

static int s2_cb_hash_take_props( cwal_callback_args const * args, cwal_value **rv ){
  int const overwritePolicy = (args->argc>1)
    ? (int)cwal_value_get_integer(s2_value_unwrap(args->argv[1]))
    : 1;
  cwal_value * src = args->argc ? args->argv[0] : 0;
  THIS_HASH;
  if(!src || !cwal_props_can(src)){
    return cwal_cb_throw(args, src ? CWAL_RC_TYPE : CWAL_RC_MISUSE,
                       "Expecting a Container argument.");
  }
  *rv = args->self;
  return cwal_hash_take_props( h, src, overwritePolicy );
}



/**
   Internal cwal_kvp_visitor_f() implementation which requires state
   to be a (cwal_hash*), into which it inserts/overwrites kvp's
   key/value.
*/
static int s2_kvp_visitor_hash_insert( cwal_kvp const * kvp, void * state ){
  return cwal_hash_insert_v( (cwal_hash *)state,
                             cwal_kvp_key(kvp), cwal_kvp_value(kvp), 1 );
}

/* in s2_protos.c */
int s2_kvp_visitor_prop_each( cwal_kvp const * kvp, void * state_ );

/**
   Script usages:

   obj.eachEntry(Function):
   
   The given function is called once per property, passed the key and
   value.

   obj.eachEntry(Object, Function):

   Functionally equivalent to obj.eachEntry(proc(k,v){otherObj.func(k,v)})

   obj.eachEntry(Hash):

   Functionally equivalent to: obj.eachEntry(targetHash, targetHash.insert)

   *rv will be set to args->self on success.
*/
static int s2_cb_hash_each_entry( cwal_callback_args const * args, cwal_value **rv ){
  cwal_function * f;
  int fIndex = (args->argc>1) ? 1 : args->argc ? 0 : -1;
  cwal_value * theThis = (args->argc>1) ? args->argv[0] : args->self;
  cwal_hash * otherHash = 0;
  THIS_HASH;

  f = (fIndex>=0)
    ? cwal_value_get_function(args->argv[fIndex])
    : 0;
  if(!f){
    if(! (otherHash = (1==args->argc)
          ? cwal_value_get_hash(args->argv[0])
          : 0) ){
      return s2_throw(se, CWAL_RC_MISUSE,
                      "'eachEntry' expects (Hash|Function) "
                      "or (Object, Function) arguments.");
    }
  }else if(!cwal_value_may_iterate(hv)){
    return cwal_exception_setf(args->engine, CWAL_RC_ACCESS,
                               "Hashtable is currently iterating.");
  }

  if(otherHash){
    return cwal_hash_visit_kvp(h, s2_kvp_visitor_hash_insert, otherHash);
  }else{
    s2_kvp_each_state state = s2_kvp_each_state_empty;
    int rc;
    state.e = args->engine;
    state.callback = f;
    state.self = theThis;
    rc = cwal_hash_visit_kvp( h, s2_kvp_visitor_prop_each, &state );
    if(S2_RC_END_EACH_ITERATION==rc) rc = 0;
    if(!rc) *rv = args->self;
    return rc;
  }
}

static int s2_cb_hash_resize( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  cwal_int_t hsz;
  THIS_HASH;
  hsz = args->argc
    ? cwal_value_get_integer(args->argv[0])
    : -1;
  if(hsz<1){
    return cwal_cb_throw(args, args->argc ? CWAL_RC_RANGE : CWAL_RC_MISUSE,
                       "Expecting a positive hash table size value.");
  }
  rc = cwal_hash_resize(h, (cwal_size_t)hsz);
  if(!rc) *rv = args->self;
  return rc;
}

static int s2_cb_hash_grow_if_loaded( cwal_callback_args const * args,
                                      cwal_value **rv ){
  int rc;
  cwal_double_t load;
  THIS_HASH;
  load = args->argc
    ? cwal_value_get_double(args->argv[0])
    : 0.8;
  rc = cwal_hash_grow_if_loaded(h, load);
  if(!rc) *rv = args->self;
  return rc;
}

static int s2_cb_hash_size( cwal_callback_args const * args, cwal_value **rv ){
  THIS_HASH;
  *rv = cwal_new_integer(args->engine, (cwal_int_t)cwal_hash_size(h));
  return *rv ? 0 : CWAL_RC_OOM;
}


static int s2_cb_hash_has_entries( cwal_callback_args const * args, cwal_value **rv ){
  THIS_HASH;
  *rv = cwal_new_bool( cwal_hash_entry_count(h) ? 1 : 0 );
  return 0;
}

cwal_value * s2_prototype_hash( s2_engine * se ){
    int rc = 0;
    cwal_value * proto;
    proto = cwal_prototype_base_get( se->e, CWAL_TYPE_HASH );
    if(proto
       || !s2_prototype_object(se) /* timing hack */
       ) return proto;
    proto = cwal_new_object_value(se->e);
    if(!proto){
        rc = CWAL_RC_OOM;
        goto end;
    }
    rc = cwal_prototype_base_set(se->e, CWAL_TYPE_HASH, proto );
    if(!rc) rc = s2_prototype_stash(se, "Hash", proto);
    if(rc) goto end;
    assert(proto == cwal_prototype_base_get(se->e, CWAL_TYPE_HASH));
    /* MARKER(("Setting up OBJECT prototype.\n")); */

#define CHECKV if(!v){ rc = CWAL_RC_OOM; goto end; } (void)0
#define RC if(rc) goto end;

    {
      const s2_func_def funcs[] = {
      S2_FUNC2("clearEntries", s2_cb_hash_clear),
      S2_FUNC2("containsEntry", s2_cb_hash_has),
      S2_FUNC2("eachEntry", s2_cb_hash_each_entry),
      S2_FUNC2("entryCount", s2_cb_hash_entry_count),
      S2_FUNC2("entryKeys", s2_cb_hash_keys),
      S2_FUNC2("entryValues", s2_cb_hash_values),
      S2_FUNC2("growIfLoaded", s2_cb_hash_grow_if_loaded),
      S2_FUNC2("hashSize", s2_cb_hash_size),
      S2_FUNC2("insert", s2_cb_hash_insert),
      S2_FUNC2("hasEntries", s2_cb_hash_has_entries),
      S2_FUNC2("remove", s2_cb_hash_remove),
      S2_FUNC2("resize", s2_cb_hash_resize),
      S2_FUNC2("search", s2_cb_hash_get),
      S2_FUNC2("takeProperties", s2_cb_hash_take_props),
      S2_FUNC2("new", s2_cb_hash_create),
      s2_func_def_empty_m
      };
      rc = s2_install_functions(se, proto, funcs, 0 );
      if(rc) goto end;
      else {
        cwal_value * fv = 0;
        s2_get(se, proto, "new", 3, &fv);
        assert(fv && "we JUST put this in there!");
        rc = s2_ctor_method_set( se, proto,
                                 cwal_value_get_function(fv) );
      }
    }

#undef FUNC2
#undef CHECKV
#undef RC
    end:
    return rc ? NULL : proto;
}


#undef MARKER
#undef THIS_HASH
#undef ARGS_SE
