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

#define THIS_FUNCTION                                  \
    cwal_function * self = 0;                          \
    ARGS_SE; \
    self = cwal_value_function_part(se->e, args->self); \
    if(!self){ \
        return s2_throw( se, CWAL_RC_TYPE, \
                         "'this' is-not-a Function." ); \
    } (void)0

static int s2_cb_func_source_code( cwal_callback_args const * args, cwal_value **rv ){
  s2_func_state * fst;
  THIS_FUNCTION;
  fst = (s2_func_state*)cwal_function_state_get(self, &s2_func_state_empty);
  if(fst){
    cwal_value * src = fst->vSrc;
    if(src || fst->flags){
      if(src){
        *rv = src;
      }else{
        static const char * const shortestSrc = "proc(){}";
        static const cwal_size_t shortestLen = 8/*^^^ strlen*/;
        assert(fst->flags && "Expecting empty-body script function");
        *rv = cwal_new_xstring_value(args->engine, shortestSrc, shortestLen)
          /* trivia: because this string's so short, we don't actually
             save any memory here by using an x-string. That
             particular string won't get interned, so we've got about
             the same memory cost using an x-string or a normal
             string */;
      }
      return *rv ? 0 : CWAL_RC_OOM;
    }else{
      return cwal_exception_setf(args->engine, CWAL_RC_ERROR,
                                 "Missing expected reference to this function's source code!");
    }
  }else{
    /* Non-script function */
    return 0;
  }
}

cwal_value * s2_func_import_props( s2_engine * se, cwal_value const *theFunc,
                                   s2_func_state * fst ){
  cwal_value * props;
  assert(se && theFunc && fst);
  props = fst->vImported;
  if(!props){
    props = cwal_new_object_value(se->e);
    if(!props) return 0;
    s2_value_to_lhs_scope(theFunc, props);
    cwal_value_ref(props);
    cwal_value_prototype_set(props, NULL);
    fst->vImported = props;
  }
  return props;
}

/**
   Implements Function.importSymbols().
*/
static int s2_cb_func_import_symbols( cwal_callback_args const * args, cwal_value **rv ){
  int rc = 0;
  cwal_value * props /* where we store the symbols */;
  uint16_t i, startAt = 0;
  s2_func_state * fst;
  THIS_FUNCTION;
  if(!args->argc){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                      "Expecting at least one symbol name to import.");
  }
  fst = s2_func_state_for_func( self );
  if(!fst){
    return cwal_cb_throw(args, CWAL_RC_TYPE,
                      "This is not a script function, "
                      "so cannot import symbols.");
  }
  props = s2_func_import_props(se, cwal_function_value(self), fst);
  if(!props) return CWAL_RC_OOM;
  if(cwal_value_is_bool(args->argv[0])){
    /* initial bool explicitly specifies whether or not to clear old
       properties before importing the new ones. */
    ++startAt;
    if(cwal_value_get_bool(args->argv[0])){
      cwal_props_clear( props );
    }
  }else{
    cwal_props_clear( props );
  }
  for( i = startAt; !rc && (i < args->argc); ++i ){
    cwal_value * key;
    cwal_value * v;
    /* cwal_array * ary; */
    key = args->argv[i];
#if 0
    /* TODO?: treat as a list of symbols. */
    if((ary = cwal_value_array_part(key))){
      continue;
    }
#endif
    if(cwal_props_can(key)){
      rc = cwal_props_copy( key, props );
      continue;
    }
    v = s2_var_get_v(se, -1, key);
    if(!v){
      cwal_string const * str = cwal_value_get_string(key);
      char const * cstr = str ? cwal_string_cstr(str) : NULL;
      return s2_throw(se, CWAL_RC_NOT_FOUND,
                      "Could not resolve symbol '%.*s' "
                      "in the current scope stack.",
                      str ? (int)cwal_string_length_bytes(str) : 0,
                      cstr ? cstr : "<non-string symbol>"
                      /* Reminder: %.*s is necessary for the
                         case of non-NUL-terminated
                         x-strings. */
                      );
    }
    rc = cwal_prop_set_v( props, key, v );
  }
  if(!rc){
    /* *rv = cwal_function_value(args->callee); */
    *rv =
      args->self
      /* cwal_function_value(self) */;
  }
  return rc;
}


/**
   Script usage:

   Function.apply(thisObject, array[arg1...argN])
*/
static int s2_cb_func_apply( cwal_callback_args const * args, cwal_value **rv ){
  cwal_value * fwdSelf /* args->argv[0], the 'this' object for the next call() */;
  cwal_array * theList = 0 /* args->argv[1], the list of arguments to apply() */;
  THIS_FUNCTION;
  if(!args->argc || args->argc>2){
    return s2_throw(se, CWAL_RC_MISUSE,
                    "'apply' expects (Value [, Array]) argument(s).");
  }
  fwdSelf = args->argv[0];
  if(args->argc>1){
    theList = cwal_value_array_part(se->e, args->argv[1]);
    if(!theList){
      return s2_throw(se, CWAL_RC_TYPE,
                      "Second argument to 'apply' must be an array.");
    }
  }
  return theList
    ? cwal_function_call_array( 0, self, fwdSelf, rv, theList )
    : cwal_function_call( self, fwdSelf, rv, 0, 0 );
}

static int s2_cb_func_call( cwal_callback_args const * args, cwal_value **rv ){
  THIS_FUNCTION;
  if(!args->argc){
    return s2_throw(se, CWAL_RC_MISUSE,
                    "'call' expects (Value [, ...]) argument(s).");
  }
  return cwal_function_call( self, args->argv[0], rv,
                             args->argc-1, args->argv+1 );
}


cwal_value * s2_prototype_function( s2_engine * se ){
    int rc = 0;
    cwal_value * proto;
    proto = cwal_prototype_base_get( se->e, CWAL_TYPE_FUNCTION );
    if(proto
       || !s2_prototype_object(se) /* timing hack */
       ) return proto;
    proto = cwal_new_object_value(se->e);
    if(!proto){
        rc = CWAL_RC_OOM;
        goto end;
    }
    rc = cwal_prototype_base_set(se->e, CWAL_TYPE_FUNCTION, proto );
    if(!rc) rc = s2_prototype_stash(se, "Function", proto);
    if(rc) goto end;
    assert(proto == cwal_prototype_base_get(se->e, CWAL_TYPE_FUNCTION));
    /* MARKER(("Setting up OBJECT prototype.\n")); */

    {
      s2_func_def const funcs[] = {
        S2_FUNC2("sourceCode", s2_cb_func_source_code),
        S2_FUNC2("importSymbols", s2_cb_func_import_symbols),
        S2_FUNC2("call", s2_cb_func_call),
        S2_FUNC2("apply", s2_cb_func_apply),
        s2_func_def_empty_m
      };
      rc = s2_install_functions(se, proto, funcs, 0);
    }

    {
      /* Function.bind() impl. */
      char const * src =
        "proc(x){"
          "affirm typeinfo(isfunction this);"
          "return proc(){ return f.apply(x,argv) }"
          "using {x, f:this} "
        "}";
      int const srcLen = (int)cwal_strlen(src);
      rc = s2_set_from_script(se, src, srcLen, proto, "bind", 4);
      if(rc) goto end;
    }
    
    end:
    return rc ? NULL : proto;
}


#undef MARKER
#undef THIS_FUNCTION
#undef ARGS_SE
