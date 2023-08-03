/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

#include "libs2.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#if 1
#include <stdio.h>
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__); if(1) printf pfexp
#else
#define MARKER(pfexp) (void)0
#endif

int s2_cb_flush( cwal_callback_args const * args, cwal_value **rv ){
  int rc = cwal_output_flush(args->engine);
  if(rc){
    rc = cwal_exception_setf(args->engine, rc,
                             "Flushing output failed with code %d (%s).",
                             rc, cwal_rc_cstr(rc));
  }else{
      *rv = cwal_value_undefined();
  }
  /* Weird: if i output from here, my error messages caught
     via handling linenoise input are flushed immediately,
     else they are not.
  */
  /* MARKER(("Flushed %d\n",rc)); */
  return rc;
}

int s2_cb_print_helper( cwal_callback_args const * args,
                        cwal_value **rv,
                        uint16_t skipArgCount,
                        cwal_flags32_t flags ){
  uint16_t i, n;
  int rc = 0;
  char const * sep = " ";
  cwal_engine * e = args->engine;
  cwal_size_t const sepLen = cwal_strlen(sep);
  char const addSpace = (S2_PRINT_OPT_SPACE & flags) ? 1 : 0;
  /* dump_val(args->self, "'this' for print()"); */
  /* MARKER(("s2_cb_print() called with %"PRIu16" arg(s).\n", args->argc)); */
  if(S2_PRINT_OPT_RETURN_THIS & flags
     && S2_PRINT_OPT_RETURN_CALLEE & flags){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "s2_cp_print_helper() cannot handle both the "
                       "S2_PRINT_OPT_RETURN_THIS and "
                       "S2_PRINT_OPT_RETURN_CALLEE flags. "
                       "Pick one or the other.");
  }
  for(i = skipArgCount, n = 0; !rc && (i < args->argc); ++i ){
    cwal_value * v = (S2_PRINT_OPT_UNWRAP & flags)
      ? s2_value_unwrap(args->argv[i])
      : args->argv[i];
    if(addSpace && n++){
      rc = cwal_output(e, sep, sepLen);
      if(rc) break;
    }
    /* s2_dump_val(v,"arg"); */
    switch(cwal_value_type_id(v)){
      case CWAL_TYPE_ARRAY:
      case CWAL_TYPE_BOOL:
      case CWAL_TYPE_DOUBLE:
      case CWAL_TYPE_EXCEPTION:
      case CWAL_TYPE_INTEGER:
      case CWAL_TYPE_NULL:
      case CWAL_TYPE_OBJECT:
      case CWAL_TYPE_TUPLE:
        rc = cwal_json_output_engine( e, v, NULL );
        break;
      case CWAL_TYPE_UNDEF:
        rc = cwal_output(e, "undefined", 9);
        break;
      case CWAL_TYPE_STRING:{
        cwal_size_t slen = 0;
        char const * cstr = cwal_value_get_cstr(v, &slen);
        rc = slen ? cwal_output(e, cstr, slen) : 0;
        break;
      }
      case CWAL_TYPE_BUFFER:{
        cwal_buffer const * vb = cwal_value_get_buffer(v);
        rc = vb->used ? cwal_output(e, vb->mem, vb->used) : 0;
        break;
      }
      case CWAL_TYPE_HASH:
      case CWAL_TYPE_FUNCTION:
      case CWAL_TYPE_NATIVE:
      case CWAL_TYPE_UNIQUE:
        rc = cwal_outputf(e, "%s@0x%p", cwal_value_type_name2(v, 0),
                          (void const*)v);
        break;
      default:
        break;
    }
  }
  if(rc && (CWAL_RC_EXCEPTION!=rc)){
    rc = cwal_exception_setf(args->engine, rc, "Output error #%d (%s).",
                             rc, cwal_rc_cstr(rc));
  }
  else if(!rc && (S2_PRINT_OPT_NEWLINE & flags)){
    cwal_output(args->engine, "\n", 1);
  }
  if(S2_PRINT_OPT_RETURN_CALLEE & flags){
    *rv = cwal_function_value(args->callee);
  }else if(S2_PRINT_OPT_RETURN_THIS & flags){
    *rv = args->self;
  }else{
    *rv = cwal_value_undefined();
  }
  cwal_output_flush(args->engine);
  return rc;
}


int s2_cb_print( cwal_callback_args const * args, cwal_value **rv ){
  return s2_cb_print_helper(args, rv, 0,
                            S2_PRINT_OPT_SPACE
                            | S2_PRINT_OPT_NEWLINE
                            | S2_PRINT_OPT_RETURN_CALLEE );
}

int s2_cb_write( cwal_callback_args const * args, cwal_value **rv ){
  return s2_cb_print_helper(args, rv, 0,
                            S2_PRINT_OPT_RETURN_CALLEE);
}

int s2_cb_import_script(cwal_callback_args const * args, cwal_value ** rv){
  int i, rc = 0;
  s2_engine * se = s2_engine_from_args(args);
  cwal_value * xrv = 0;
  assert(se);
  if( (rc = s2_cb_disable_check(args, S2_DISABLE_FS_STAT
                                | S2_DISABLE_FS_READ)) ) return rc;
#if 0
  /*
    experimenting with a use for this in require.s2. Doesn't
    work - this gets overwritten by that point and passing this
    through the API would go through way too many layers and impact
    too many things.

    _Seems_ to work okay for the basic case, though, as long as one
    doesn't call this function from cwal_function_call_in_scope(),
    using a scope which already has a 'this'. In the "normal" case
    (when this function is called from s2 scripts) 'this' is the LHS
    of the s2.import() call (or is this function if the import()
    reference is used standalone), and we're overwriting that one (if
    it's set) in its call-local scope.

    Not yet sure i want to export our "this" this way, but it would be
    potentially useful in a few cases.
  */
  rc = s2_var_set( se, 0, "this", 4, args->self);
  if(rc) return rc;
#endif

  /*
    TODO:

    - Create a local array to hold a list of all filenames passed in. 

    - Before running each script, see if it's in that list. If it is,
    assume recursion and fail.

    - Else eval it.

    - At the end of the loop, free the name list.

    Hmmm. Each sub-import needs access to the same list. Where to store it
    so that it's reachable recursively? In args->self['importList'] and
    args->self['importDepth']?
  */
  *rv = 0;
  for( i = 0; !rc && i < args->argc; ++i ){
    cwal_value const * arg = args->argv[i];
    cwal_size_t nLen = 0;
    char const * fn = arg ? cwal_value_get_cstr(arg, &nLen) : 0;
    if(!fn){
      rc = cwal_cb_throw( args, CWAL_RC_TYPE,
                        "Expecting a STRING value, but got '%s'.",
                        arg ? cwal_value_type_name(arg) : "<NULL>");
    }else{
      cwal_value_unref(xrv);
      xrv = 0;
      rc = s2_eval_filename(se, 1, fn, nLen, &xrv);
      cwal_value_ref(xrv);
    }
  }
  switch(rc){
    case 0:
      if(xrv){
        cwal_value_unhand(xrv);
        *rv = xrv;
        assert((cwal_value_scope(*rv) || cwal_value_is_builtin(*rv))
               && "Seems like *rv was cleaned up too early.");
      }else{
        *rv = cwal_value_undefined();
      }
      break;
    case CWAL_RC_RETURN:
      *rv = cwal_propagating_take(se->e);
      assert(*rv && "Misuse of CWAL_RC_RETURN!");
      assert(xrv != *rv && "But... how???");
      cwal_value_unref(xrv);
      s2_engine_err_reset(se);
      rc = 0;
      break;
    default:
      cwal_value_unref(xrv);
      break;
  }
  return rc;
}

int s2_install_io( s2_engine * se, cwal_value * tgt,
                   char const * name ){
  cwal_value * sub;
  int rc;
  if(!se || !tgt) return CWAL_RC_MISUSE;
  else if(!cwal_props_can(tgt)) return CWAL_RC_TYPE;

  if(name && *name){
    sub = cwal_new_object_value(se->e);
    if(!sub) return CWAL_RC_OOM;
    cwal_value_ref(sub);
    rc = cwal_prop_set(tgt, name, cwal_strlen(name), sub);
    cwal_value_unref(sub);
    if(rc) return rc;
  }else{
    sub = tgt;
  }
  cwal_value_ref(sub);
  {
    s2_func_def const funcs[] = {
      S2_FUNC2("flush", s2_cb_flush),
      S2_FUNC2("output", s2_cb_write),
      S2_FUNC2("print", s2_cb_print),
      s2_func_def_empty_m
    };
    rc = s2_install_functions(se, sub, funcs, 0);
    if(rc) goto end;
    /**
       Add output.'operator<<' as a proxy for output(). Why?  Because
       (A) we end up doing something equivalent in a surprising amount
       of client code and (B) to provide a default operator<< impl for
       s2.tmpl(). Because of how s2 tokenizes parens groups, x<<y is,
       for many common cases, faster than x(y) because the latter gets
       tokenized at least twice (once to slurp the (...) group and
       once to evaluate the function call arguments).

       While we're here, we'll also add it to print(), but note that
       this instance of print() is a different one than gets installed
       globally in s2sh!

       As of 20191210, we recycle s2out.operator<< for use with
       s2.io.output/print. We do not make s2.io.output an alias
       s2out because s2out's object may be sealed.
    */
    rc = s2_eval_cstr_with_var( se, "x", sub, "s2.io setup",
                                "x.output.'operator<<'="
                                "x.print.'operator<<'="
                                "s2out.'operator<<'",
                                -1, NULL );
  }
  end:
#undef SET
  cwal_value_unhand(sub) /* it's either ==tgt or a property of tgt */;
  return rc;
}

#undef MARKER
