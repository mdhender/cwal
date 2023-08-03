/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/**
  This file is meant to act as a client-side way to extend the default
  s2 shell app. It demonstrates examples of connecting C code to
  the s2 scripting environment.

  If shell.c is built with S2_SHELL_EXTEND defined, then it will assume
  this function will be linked in by the client:

  int s2_shell_extend(s2_engine * se, int argc, char const * const * argv);

  When built with the macro S2_SHELL_EXTEND, it calls that function right
  after it installs its own core functionality.
*/
#include <assert.h>
#include "libs2.h"

/* Only for debuggering... */
#include <stdio.h>
#define SAY(pfexp)                                                      \
    do{ printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__); \
        printf pfexp; \
    } while(0)

/**
   An example cwal_callback_f() implementation. These make up the meat
   of most cwal/s2 bindings.

*/
static int my_callback(cwal_callback_args const * args, cwal_value ** rv){
  int rc;
  rc = cwal_outputf(args->engine, "%s:%d: Hi, world!\n",
                    __FILE__, __LINE__);
  if(!rc) *rv = cwal_value_undefined();
  return rc;
}


/**
   An example cwal_callback_f() implementation.
*/
static int my_chainable_callback(cwal_callback_args const * args, cwal_value ** rv){
  int rc;
  rc = cwal_outputf(args->engine, "%s:%d: Hi again, world!\n",
                    __FILE__, __LINE__);
  if(!rc) *rv = cwal_function_value(args->callee);
  return rc;
}


/**
   An example cwal_callback_f() implementation.
*/
static int my_mixed_callback(cwal_callback_args const * args, cwal_value ** rv){

  switch(args->argc){
    case 0:
      *rv = cwal_string_value(
                              cwal_new_stringf(args->engine, "%s:%d: Hi again, world!",
                                               __FILE__, __LINE__)
                              );
      break;
    case 1:
      *rv = args->argv[0];
      break;
    case 2:
      *rv = cwal_new_double(args->engine, (cwal_double_t)
                            cwal_value_get_double(args->argv[0])
                            +
                            cwal_value_get_integer(args->argv[1])
                            )
        /* Note: that uses implicit cwal-level conversion to int/double,
           basically meaning: any non-string, or any string which doesn't
           look like a number, will convert to 0. Boolean true converts
           to 1 resp. 1.0. Strings which look like numbers will convert
           to them there.
        */;
      break;
    default:
      return cwal_exception_setf(args->engine, CWAL_RC_RANGE,
                                 "Too beaucoup!");
  }
  return *rv ? 0 : CWAL_RC_OOM;
}

/**
   An example cwal_callback_f() implementation which triggers a
   script-side exception. Returning non-0 without throwing an
   exception may trigger a script-fatal error or (depending on the
   context and result exact code) it might be converted to an
   exception. It is important the clients ONLY use result codes from
   the CWAL_RC_xxx and S2_RC_xxx family of values. When throwing
   exceptions, they may use arbitrary integer values as the
   exception's error code (the 2nd argument to cwal_exception_setf()),
   with the caveat that script code might expect them to be cwal RC
   values.
*/
static int my_error_callback(cwal_callback_args const * args, cwal_value ** rv){
  int const rc = CWAL_RC_UNSUPPORTED;
  if(rv){/*avoid unused param warning*/}
  return cwal_exception_setf(args->engine, rc,
                             "You have requested unsupported feature "
                             "'%s', so you are getting result code "
                             "%d (%s).",
                             "foo", rc, cwal_rc_cstr(rc));
}

/**
   Clients should implement any extensions to the s2 interpreter here.
   
   argc and argv contain the arguments passed to main(). se is the s2
   interpreter used by the application.

   Client code may use s2_scope_current() or cwal_scope_current_get()
   to get the current scope, and may install functionality there if
   they like. More simply, s2_var_set() and friends can be used to add
   features to the current scope. s2sh calls this function only from
   the global scope, so any features added to the current scope will,
   if they're not manually removed, live as long as the interpreter
   instance.

   mainNamespace is s2sh's "s2" object. In practice, that is where
   most functionality gets installed. Note that we're not limited to
   installing in one specific place, but in practice that is typical.
   That value is guaranteed by s2sh to live until the top-most scope
   is cleaned up (when the shell shuts down) unless the client
   _intentionally_ does something silly from the C level and somehow
   destroys that value.

   Must return 0 on success or one of the CWAL_RC_xxx or S2_RC_xxx
   values on error. On error, the application must treat it as fatal
   to the scripting engine. The default shell ends the application
   (gracefully, as opposed to abort()) if this function returns non-0.
*/
int s2_shell_extend(s2_engine * se, cwal_value * mainNamespace,
                    int argc, char const * const * argv){
  int rc = 0;
  /*cwal_engine * e = s2_engine_engine(se)*/
    /* cwal_engine is used by the core language-agnostic script engine
       (cwal). s2_engine is a higher-level abstraction which uses
       cwal_engine to (A) provide a Value type system and (B) manage
       the lifetimes of memory allocated on behalf of the scripting
       language (s2). s2 has to take some part in managing the
       lifetimes, but it basically just sets everything up how cwal
       expects it to be, and lets cwal do the hard parts wrt memory
       management.
     */
    ;
  if(argc || argv){/*avoid potentially unused param warning*/}
#if 0
  SAY(("Running client-side shell extension code...\n"));
#endif

  /*
    Add your functionality here.

    The demo code here is basically copy/pasted from many similar
    routines used by the core lib, except that they are normally given
    a specific object to store their functionality into. Since this is
    app-level init code, it's up to the app to define that. For s2sh,
    extensions may use s2_shell_global(), declare their new features
    as variables/const (see s2_var_decl() and friends), or go all
    low-level and stuff them into the current scope's properties
    (or some combination of all of those).
  */
  
  {
    s2_func_def const funcs[] = {
      S2_FUNC2("sampleCallback", my_callback),
      S2_FUNC2("sampleCallback2", my_chainable_callback),
      S2_FUNC2("sampleCallback3", my_error_callback),
      S2_FUNC2("sampleCallback4", my_mixed_callback),
      s2_func_def_empty_m /* this entry MUST be the last one! */
    };
    rc = s2_install_functions(se, 0 ? mainNamespace : NULL, funcs, 0)
      /* Pass a 2nd argument of NULL to install the functions
         in the current scope instead of in mainNamespace. */
      ;
    if(rc) goto end;
  }

  /* Add any other functions or properties you like, either as
     properties of mainNamespace, variables in the current scope
     (which s2sh ensures is the top-most (global) scope when it calls
     this function), or wherever else is appropriate... */

  end:
  return rc;    
}

#undef SAY
