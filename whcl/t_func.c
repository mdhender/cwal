/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

/**
   Impls for the Function prototype.
*/
#include "internal.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>

#if 1
#define MARKER(pfexpr) if(1) printf("MARKER: %s:%d:\t",__FILE__,__LINE__); if(1) printf pfexpr
#else
#define MARKER if(0) printf
#endif

/**
   Returns fst->vImports, creating it if needed. Returns
   NULL only on OOM. theFunc _must_ be the function-type value
   which owns fst.
*/
static cwal_value * whcl__func_import_props( whcl_engine * const el,
                                             cwal_value const *theFunc,
                                             whcl__func * const fst ){
  cwal_value * props = fst->vImports;
  if(!props){
    props = cwal_new_object_value(el->ec);
    if(props){
      whcl__value_to_lhs_scope(theFunc, props);
      cwal_ref(props);
      cwal_value_prototype_set(props, NULL);
      fst->vImports = props;
    }else{
      WHCL__WARN_OOM;
    }
  }
  return props;
}

#define WHCL_E_ARGS \
  whcl_engine * const el = whcl_engine_from_args(args); assert(el)

#define THIS_FUNCTION                                  \
    cwal_function * self = 0;                          \
    self = cwal_value_function_part(args->engine, args->self);  \
    if(!self){ \
        return cwal_cb_throw( args, CWAL_RC_TYPE, \
                              "'this' is-not-a function." );    \
    } (void)0

/**
   Implements Function[import-symbols].
*/
static int whcl__cb_func_import_symbols( cwal_callback_args const * args,
                                         cwal_value **rv ){
  int rc = 0;
  cwal_value * props /* where we store the symbols */;
  uint16_t i, argNdx = 0;
  whcl__func * fst;
  cwal_size_t nArg = 0;
  char const * zArg = NULL;
  bool clearProps = true;
  WHCL_E_ARGS;
  THIS_FUNCTION;
  fst = whcl__func_for_func( self );
  if(!fst){
    return cwal_exception_setf(args->engine, CWAL_RC_TYPE,
                         "This is not a script function, "
                         "so cannot import symbols.");
  }
  for(; whcl_arg_has_flag(args, &argNdx, &zArg, &nArg);
      zArg = NULL, nArg = 0){
    if(5==nArg && 0==memcmp("-keep", zArg, 5)){
      clearProps = false;
      continue;
    }else{
      return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                 "Unknown flag argument: %.*s",
                                 (int)nArg, zArg);
    }
  }
  if(!clearProps && argNdx == args->argc){
    /* Reminder to self: we allow the no-args case without -keep for
       the sake of clearing all existing properties. */
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting at least one symbol "
                               "name to import.");
  }
  props = whcl__func_import_props(el, cwal_function_value(self), fst);
  if(!props) return CWAL_RC_OOM;
  if(clearProps){
    cwal_props_clear( props );
  }
  for( i = argNdx; 0==rc && (i < args->argc); ++i ){
    cwal_value * key;
    cwal_value * v;
    key = args->argv[i];
    if(cwal_props_can(key)){
      rc = cwal_props_copy( key, props );
      continue;
    }
    /* Look up `key` as an identifier, resolving the symbol based on
       the _calling_ scope... */
    v = whcl_var_search_v(el, whcl_scope_parent(el, NULL), true,
                          key, NULL);
    if(!v){
      cwal_string const * str = cwal_value_get_string(key);
      char const * cstr = str ? cwal_string_cstr(str) : NULL;
      return cwal_cb_throw(args, CWAL_RC_NOT_FOUND,
                      "Could not resolve symbol '%.*s' "
                      "in the current scope stack.",
                      str ? (int)cwal_string_length_bytes(str) : 0,
                      cstr ? cstr : "<non-string symbol>"
                      /* Reminder: %.*s is necessary for the
                         case of non-NUL-terminated
                         x-strings. */
                      );
    }
    rc = whcl_set_v( el, props, key, v );
  }
  if(!rc){
    *rv = args->self;
  }
  return rc;
}

/**
   Script usage:

   Function.apply(thisObject, array[arg1...argN])
*/
static int whcl__cb_func_apply( cwal_callback_args const * args, cwal_value **rv ){
  cwal_value * fwdSelf /* args->argv[0], the 'this' object for the next call() */;
  cwal_array * theList = 0 /* args->argv[1], the list of arguments to apply() */;
  THIS_FUNCTION;

  if(!args->argc || args->argc>2){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "'apply' expects (Value [, Array]) argument(s).");
  }
  fwdSelf = args->argv[0];
  if(args->argc>1){
    theList = cwal_value_array_part(args->engine, args->argv[1]);
    if(!theList){
      return cwal_cb_throw(args, CWAL_RC_TYPE,
                           "Second argument to 'apply' must be an array.");
    }
  }
  return theList
    ? cwal_function_call_array( 0, self, fwdSelf, rv, theList )
    : cwal_function_call( self, fwdSelf, rv, 0, 0 );
}

static int whcl__cb_func_call( cwal_callback_args const * args, cwal_value **rv ){
  THIS_FUNCTION;
  return args->argc
    ? cwal_function_call( self, args->argv[0], rv,
                          args->argc-1, args->argv+1 )
    :  cwal_cb_throw(args, CWAL_RC_MISUSE,
                     "'call' expects (value [array]) argument(s).");
}

int whcl__cb_func_source( cwal_callback_args const * args, cwal_value **rv ){
  THIS_FUNCTION;
  whcl__func * const wf = whcl__func_for_func(self);
  if(!wf){
    *rv = cwal_value_undefined();
    return 0;
  }
  cwal_buffer b = cwal_buffer_empty;
  int rc = cwal_buffer_append(args->engine, &b, "proc ", 5);
  if(0==rc){
    rc = cwal_buffer_append(args->engine, &b, wf->tok.script.begin,
                            (cwal_size_t)(wf->tok.script.end
                                          - wf->tok.script.begin));
    if(0==rc){
      cwal_string * const s = cwal_buffer_to_zstring(args->engine, &b);
      if(s) *rv = cwal_string_value(s);
      else {WHCL__WARN_OOM; rc = CWAL_RC_OOM;}
    }
  }
  cwal_buffer_clear(args->engine, &b);
  return rc;
}

cwal_value * whcl_prototype_function(whcl_engine * const el){
  int rc = 0;
  cwal_value * proto;
  proto = cwal_prototype_base_get( el->ec, CWAL_TYPE_FUNCTION );
  if(proto
     || !whcl_prototype_object(el) /* timing hack */
     ) return proto;
  proto = cwal_new_object_value(el->ec);
  if(!proto){
    WHCL__WARN_OOM;
    rc = CWAL_RC_OOM;
    goto end;
  }
  rc = cwal_prototype_base_set(el->ec, CWAL_TYPE_FUNCTION, proto );
  if(!rc) rc = whcl__prototype_stash(el, "Function", proto);
  if(rc) goto end;
  assert(proto == cwal_prototype_base_get(el->ec, CWAL_TYPE_FUNCTION));
  {
    whcl_func_def const funcs[] = {
    WHCL_FUNC2("apply", whcl__cb_func_apply),
    WHCL_FUNC2("call", whcl__cb_func_call),
    WHCL_FUNC2("import-symbols", whcl__cb_func_import_symbols),
    WHCL_FUNC2("source-code", whcl__cb_func_source),
    whcl_func_def_empty_m
    };
    rc = whcl_install_functions(el, proto, funcs, 0);
#if 0
    /* We cannot sensibly make a Function a Command handler because
       the two modes of operation are apparently mutually
       exclusive! */
    if(0==rc) rc = whcl__install_command_cb(el, proto);
#endif
    if(rc) goto end;
  }

#if 0
  {
    /* Function.bind() impl. */
    /* This (haha) might not be possible in whcl so long as we have
       `this` as a builtin value instead of a call-local var. */
    char const * src =
      "proc(x){"
      "affirm typeinfo(isfunction this);"
      "return proc(){ return f.apply(x,argv) }"
      "using {x, f:this} "
      "}";
    int const srcLen = (int)cwal_strlen(src);
    rc = s2_set_from_script(el, src, srcLen, proto, "bind", 4);
    if(rc) goto end;
  }
#endif

  end:
  return rc ? NULL : proto;
}

#undef MARKER
#undef THIS_FUNCTION
#undef WHCL_E_ARGS
