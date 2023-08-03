/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/*
  This file implements a s2 binding for regcomp(3) and regexec(3).
*/
#include <assert.h>

#include <sys/types.h>
#include <regex.h>
#include <errno.h>
#include <string.h> /* memset() */
#include "libs2.h"

/* only for debuggering */
#include <stdio.h>
#define MARKER(pfexp)                                                \
    do{ printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__); \
        printf pfexp; \
    } while(0)

/* Shared code for the regex modules. */
#define S2RX_FLAVOR 1
#include "s2rx_common_static.c"

/* Internal constants */
enum {
/**
   Max number of regex captures, including the 0th full-match
   capture. This number "really should" be the same as the value
   from ../regex_js/regexp.h, so that the regex modules stay as
   API-compatible as possible.
*/
POREX_MAX_CAPTURES = REGEX_MAX_CAPTURES
  /* Note that POSIX doesn't count the $0 whole-string capture
     in regex_t::re_nsub, but JS does. */
};

static void s2rx_posix_finalizer( cwal_engine * e, void * v ){
  /* MARKER(("freeing regex_t@%p sizeof(regex_t)=%d\n", v, (int)sizeof(regex_t))); */
  regfree((regex_t *)v);
  cwal_free2(e, v, (cwal_size_t)sizeof(regex_t));
}

/**
   If (rx->re_nsub >= POREX_MAX_CAPTURES), this function triggers
   an exception of type CWAL_RC_RANGE and returns the result of
   throwing, else it returns 0.

   Reminders to self:

   1) regexec() reports the numbers of captures it
   encountered by setting rx->re_nsub to that value, but it may be
   higher than the number of matches we told it to capture, so
   we have to be careful not to use an invalid (too high) index
   when passing on the matches downstream.

   2) re_nsub does not count the whole-string match ($0), so its value
   is one less than we need for our purposes.

   3) The limit of POREX_MAX_CAPTURES is simply for API compatibility
   with the JS module. In that one, the max captures are baked in to
   the regex_t counterpart, so every regex requires memory for them.
   In POSIX the matches are stack-based, so they're cheaper.
*/
static int s2rx_posix_check_capture_count(cwal_callback_args const * args,
                                          regex_t const * rx ){
#if 0
  /* This impl lets downstream worry about the max index count. The
     result is that a regexec() with too many captures just captures
     what it can and silently elides the rest. Whether that is better
     than a loud failure is up for debate. The JS module, because of
     how it's structured, necessarily fails at compile-time if too
     many captures are in the regex. */
  if(args || rx){/*unused*/}
  return 0;
#else
  return rx->re_nsub>=POREX_MAX_CAPTURES
    ? cwal_cb_throw(args, CWAL_RC_RANGE,
                  "Too many regex captures. Max is %d, including $0.",
                  POREX_MAX_CAPTURES)
    : 0;
#endif
}

/**
   Internal helper for regexec() and regcomp() error reporting. Pass it the current
   callback's arguments, the name of the function which failed, e.g. "regexec()",
   the result code from the failed regexec()/regcomp() call, and the regex object.
   This routine throws an exception and returns CWAL_RC_EXCEPTION on success or
   lower-level non-0 error code on error.
*/
static int s2rx_posix_cb_error(cwal_callback_args const * const args, char const * msgPrefix,
                               int rxcode, regex_t * self){
  enum { POREX_ERRBUF_SIZE = 256 };
  char buf[POREX_ERRBUF_SIZE] = {0};
  regerror(rxcode, self, buf, POREX_ERRBUF_SIZE);
  return cwal_cb_throw(args, CWAL_RC_ERROR, "%s failed with code #%d: %s",
                     msgPrefix, rxcode, buf);
}

/**
   Factory for regex_t instances.
*/
static int s2rx_cb_compile( cwal_callback_args const * args,
                            cwal_value **rv ){
  char const * pat = NULL;
  regex_t re;
  regex_t * newRe;
  int rc;
  int flags = 0;
  char zExecFlags[10] = {0};
  cwal_value * natv;
  cwal_value * proto = cwal_args_state(args, &s2rx_prototype_id);
  assert(proto);
  if(!proto){
    return cwal_cb_throw(args, CWAL_RC_UNSUPPORTED,
                       "porex prototype is missing. Make sure this "
                       "module is not both statically and "
                       "dynamically linked!");
  }else if(!args->argc || !(pat=cwal_value_get_cstr(args->argv[0], NULL))){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting a string argument.");
  }
  rc = s2rx_flag_arg(args->engine, S2RX_FLAG_ARG_COMPILE,
                     args->argc>1 ? args->argv[1] : 0, &flags,
                     &zExecFlags[0]);
  if(rc) return rc;
  rc = regcomp( &re, pat, flags);
  if(rc){
    return s2rx_posix_cb_error(args, "regcomp()", rc, &re);
  }
  newRe = (regex_t*)cwal_malloc2(args->engine, sizeof(regex_t));
  if(!newRe) return CWAL_RC_OOM;
  natv = cwal_new_native_value(args->engine, newRe,
                              s2rx_posix_finalizer,
                              &s2rx_typeid);
  if(!natv){
    regfree(&re);
    s2rx_posix_finalizer(args->engine, newRe);
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
      regfree(&re);
      cwal_value_unref(natv);
    }else{
      *newRe = re /* transfer memory, we hope */;
      cwal_value_prototype_set( natv, proto );
      cwal_value_unhand(natv);
      *rv = natv;
    }
  }
  return rc;
}

static int s2_module_init_regex_posix( s2_engine * se, cwal_value ** module ){
  return s2rx_module_init(se, module);
}
/**
   Module registration...
*/
S2_MODULE_REGISTER_(regex_posix);

#undef MARKER
#undef REGEX_MAX_CAPTURES
