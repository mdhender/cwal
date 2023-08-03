/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

#include <assert.h>
#include <string.h> /* memset() */
#include "libs2.h"
#include "regexp.h"

#if 1
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:\t",__FILE__,__LINE__); if(1) printf pfexp
#else
#define MARKER(pfexp) (void)0
#endif

/* Shared code for the regex modules. */
#define S2RX_FLAVOR 2
#include "../regex_posix/s2rx_common_static.c"

/**
   Allocator proxy for the JS regexp engine, to allow it to use the
   cwal allocator.
*/
static void * s2rx_js_realloc_cwal_engine( void * state, void * m, int n ){
  return cwal_realloc( (cwal_engine*)state, m, (cwal_size_t)n );
}

/**
   cwal_finalizer_f() impl for Reprog instances.
*/
static void s2rx_js_finalizer( cwal_engine * e, void * v ){
  /*MARKER(("freeing s2rx_js@%p\n", v));*/
  regfreex(s2rx_js_realloc_cwal_engine, e, (Reprog *)v);
}


/**
   Factory for Reprog instances.

   regex thisFunc(string pattern [, mixed flags])

   The available flags are integer properties (with unspecified non-0
   values) of this function named ICASE and NEWLINE. They may be OR'd
   together. Alternately, they may be specified as a string, with the
   letters 'i' and 'n' acting as ICASE and NEWLINE, respectively.

   Throws if compilation of the regex fails.

   Sidebar: regex instances are _huge_: just a tick over 2k for each
   one, _before_ compilation! The regex library allocates, during
   initialization of regex compilation, approximately 64 bytes _per
   byte of the regex pattern string_, so a 100-byte regex costs _at
   least_ 7kb. It then goes on to allocate what appears to be at least
   (8 + number of captures (including the whole-match part)) * 32 =
   256 bytes (that's not exact - that's a conservative estimate).

   Management summary: these regexes are extremely memory-hungry.
*/
static int s2rx_cb_compile( cwal_callback_args const * args,
                            cwal_value **rv ){
  char const * pat = NULL;
  Reprog * re;
  int rc;
  int flags = 0;
  char zExecFlags[10] = {0};
  cwal_value * natv;
  cwal_value * proto =
    (cwal_value*)cwal_args_state(args, &s2rx_prototype_id);
  char const * errMsg = 0;
  cwal_size_t patLen = 0;
  if(!proto){
    return cwal_cb_throw(args, CWAL_RC_UNSUPPORTED,
                       "regex_js prototype is missing. Make sure this "
                       "module is not both statically and "
                       "dynamically linked!");
  }else if(!args->argc || !(pat=cwal_value_get_cstr(args->argv[0], &patLen))
           || !patLen
           ){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting a non-empty string argument.");
    /* Reminder to self; regcomp() supports empty regexes, but i
       can't imagine that they're terribly useful, so we'll disallow
       them for now. */
  }
  rc = s2rx_flag_arg(args->engine, S2RX_FLAG_ARG_COMPILE,
                     args->argc>1 ? args->argv[1] : 0, &flags,
                     &zExecFlags[0]);
  if(rc) return rc;
  re = regcompx( s2rx_js_realloc_cwal_engine, args->engine,
                 pat, flags, &errMsg );
  if(!re){
    return cwal_cb_throw(args, CWAL_RC_ERROR,
                       "Regex compilation failed: %s. Pattern: %.*s",
                       errMsg ? errMsg : "unknown error",
                       (int)patLen, pat);
  }
  natv = cwal_new_native_value(args->engine, re, s2rx_js_finalizer,
                               &s2rx_typeid);
  if(!natv){
    s2rx_js_finalizer(args->engine, re);
    rc = CWAL_RC_OOM;
  }else{
    cwal_value_ref(natv);
    rc = cwal_prop_set(natv, "pattern", 7, args->argv[0]);
    if(!rc){
      cwal_value * iv = cwal_new_string_value(args->engine, &zExecFlags[0],
                                              cwal_strlen(&zExecFlags[0]));
      if(!iv) rc = CWAL_RC_OOM;
      else{
        cwal_value_ref(iv);
        rc = cwal_prop_set(natv, "flags", 5, iv);
        cwal_value_unref(iv);
      }
    }
    if(rc){
      cwal_value_unref(natv)/*destroys re*/;
    }else{
      cwal_value_prototype_set( natv, proto );
      cwal_value_unhand(natv);
      *rv = natv;
    }
  }
  return rc;
}

static int s2_module_init_regex_js( s2_engine * se, cwal_value ** module ){
  return s2rx_module_init(se, module);
}
/**
   Module registration...
*/
S2_MODULE_REGISTER_(regex_js);
