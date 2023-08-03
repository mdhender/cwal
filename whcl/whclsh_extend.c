/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#include "libwhcl.h"
#include <assert.h>
#include <stdlib.h>

/**
   This file is intended to be copied by whcl/whclsh clients,
   customized for their purposes, and then built with whclsh. whclsh
   must be compiled with -DWHCLSH_EXTEND, linked with this file,
   and whclsh_extend() should be edited to install any client-side
   whcl/whclsh extensions. That function is called during
   initialization of whclsh so the client-installed features area
   available to script-side code.
*/

#if 1
#define MARKER(pfexpr) if(1) printf("MARKER: %s:%d:\t",__FILE__,__LINE__); if(1) printf pfexpr
#else
#define MARKER if(0) printf
#endif


/**
   An example cwal_callback_f(). This is "the" approach to installing
   new functions in cwal (whcl's underlying scripting engine).
*/
static int my_callback1( cwal_callback_args const * args,
                        cwal_value **rv ){
  cwal_size_t len = 0;
  char const * key = args->argc
    ? cwal_value_get_cstr(args->argv[0], &len)
    : 0;
  if(!key){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting a string argument.");
  }else{
    char const * val = getenv(key);
    *rv = val
      ? cwal_new_string_value(args->engine, val, cwal_strlen(val))
      : (args->argc>1
         ? args->argv[1]
         : cwal_value_undefined());
    return *rv ? 0 : CWAL_RC_OOM;
  }
}


/**
   Clients should implement any extensions to the whclsh interpreter
   here.
   
   argc and argv contain all arguments passed to main(). el is the
   whcl interpreter used by the application.

   Client code may use whcl_scope_current() or
   cwal_scope_current_get() (via whcl_engine_cwal()) to get the
   current scope, and may install functionality there if they
   like. More simply, whcl_set() and friends can be used to add
   features to the current scope. whclsh calls this function only from
   the global scope, so any features added to the current scope will,
   if they're not manually removed, live as long as the interpreter
   instance.

   ns (short for "namespace") is whclsh's "whcl" object. In practice,
   that is where most functionality gets installed. Note that we're
   not limited to installing in one specific place, but in practice
   that is typical.  That value is guaranteed by whcl to live until
   the top-most scope is cleaned up (when the shell shuts down) unless
   the client _intentionally_ does something silly from the C level
   and somehow destroys that value.

   Must return 0 on success or one of the CWAL_RC_xxx values on
   error. On error, the application must treat it as fatal to the
   scripting engine. The default shell ends the application
   (gracefully, as opposed to abort()) if this function returns non-0.
*/
int whclsh_extend(whcl_engine * const el, cwal_value * const ns,
                  int argc, char const * const * argv){

  int rc = 0;
  cwal_value * myNs = NULL
    /*namespace object for our custom features */;
  cwal_engine * const ce = whcl_engine_cwal(el);
  if(el || ns || argc || argv){/*unused*/}

  /**
     Create an object to hold our custom features...
  */
  myNs = cwal_new_object_value(ce);
  if(!myNs){ WHCL__WARN_OOM; rc = CWAL_RC_OOM; goto end; }
  cwal_ref(myNs);
  rc = whcl_set(el, ns, "example", 7, myNs);
  cwal_unref(myNs);
  if(rc){
    /* On success, ns now holds a reference ot myNs, so its
       refcount is >0. On error, the call to cwal_unref()
       destroyed it. i.e. either way, we're not leaking
       anything. */
    goto end;
  }
  /* As of ^^^, script code can access myNs via whcl[my]. */
  
  /**
     Add some functions to the 'my' object...
  */
  whcl_func_def funcs[] = {
    WHCL_FUNC2("getenv", my_callback1),
    // Add more WHCL_FUNC2()... here
    whcl_func_def_empty_m
  };
  rc = whcl_install_functions(el, myNs, funcs, CWAL_VAR_F_CONST);
  if(rc) goto end;
  /* As of ^^^, script code can access the install functions via
     whcl[my][FuncName]. */

  /** Tell the world what we've done (just for example's sake)... */
  if(0){
    MARKER(("Installed example whclsh extensions via %s:%s()\n",
            __FILE__, __func__));
  }
  
  end:
  return rc;
}

#undef MARKER
