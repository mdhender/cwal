/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

/**
   Impls for the time-related APIs.
*/
#include "internal.h"
#include <assert.h>


#if WHCL_HAVE_USLEEP
#  include <string.h> /* strerror() */
#  include <errno.h>
#endif
#if defined(WHCL_OS_UNIX)
#  include <unistd.h> /* usleep(), possibly _POSIX_TIMERS */
#endif

#include <stdlib.h>

#if WHCL_HAVE_CLOCK_GETTIME
#  include <time.h>
#  include <errno.h>
#endif

#if 1
#include <stdio.h>
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__); if(1) printf pfexp
#else
#define MARKER(pfexp) (void)0
#endif

/**
   Internal impl of sleep()/mssleep(). delayMult must be:

   sleep(): 1000*1000
   mssleep(): 1000
   usleep(): 1
*/
static int whcl__cb_sleep_impl(cwal_callback_args const * args, cwal_value ** rv,
                            unsigned int delayMult){
#if !defined(WHCL_HAVE_USLEEP)
  return cwal_exception_setf(args->engine, CWAL_RC_UNSUPPORTED,
                             "usleep()-based routines not implemented "
                             "in this build.");
#else
  if(!args->argc || !cwal_value_is_number(args->argv[0])){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting an integer argument.");
  }else{
    int rc = -1;
    cwal_int_t const t = cwal_value_get_integer(args->argv[0]);
    if(0<=t){
      if( (rc = usleep( t * delayMult )) ){
        rc = cwal_exception_setf(args->engine,
                                 cwal_errno_to_cwal_rc(errno, CWAL_RC_ERROR),
                                 "usleep() failed with errno %d: %s",
                                 errno, strerror(errno));
      }else{
        *rv = cwal_value_undefined();
      }
    }else{
      rc = cwal_exception_setf(args->engine, CWAL_RC_RANGE,
                               "Expecting a positive sleep time value.");
    }
    return rc;
  }
#endif
}

int whcl_cb_sleep(cwal_callback_args const * args, cwal_value ** rv){
#if !defined(WHCL_OS_UNIX)
  return cwal_exception_setf(args->engine, CWAL_RC_UNSUPPORTED,
                             "sleep() not implemented in this build.");
#else
  if(!args->argc || !cwal_value_is_number(args->argv[0])){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting an integer argument.");
  }else{
    int rc = -1;
    cwal_int_t const t = cwal_value_get_integer(args->argv[0]);
    if(0<=t){
      rc = sleep( (unsigned int)t );
      *rv = cwal_new_integer(args->engine, rc);
      rc = 0;
    }else{
      rc = cwal_exception_setf(args->engine, CWAL_RC_RANGE,
                               "Expecting a positive sleep time value.");
    }
    return rc;
  }
#endif
}

int whcl_cb_mssleep(cwal_callback_args const * args, cwal_value ** rv){
  return whcl__cb_sleep_impl( args, rv, 1000 );
}

int whcl_cb_time( cwal_callback_args const * args, cwal_value **rv ){
  time_t const tm = time(NULL);
  cwal_int_t const it = (cwal_int_t)tm;
  if(tm != (time_t)it){ /* too large for our int type... */
    *rv = cwal_new_double(args->engine, (double)tm);
  }else{
    *rv = cwal_new_integer(args->engine, it);
  }
  return *rv ? 0 : CWAL_RC_OOM;
}

int whcl_cb_mstime( cwal_callback_args const * args, cwal_value **rv ){
#if !WHCL_HAVE_CLOCK_GETTIME
  (void)rv;
  return cwal_exception_setf(args->engine, CWAL_RC_UNSUPPORTED,
                             "mstime() is not implemented in this build.");
#else
  /*
    Per https://upvoid.com/devblog/2014/05/linux-timers/
    CLOCK_REALTIME_COARSE has a resolution of within 1ms, which is
    fine for what we're doing.
  */
  struct timespec spec;
  int rc;
  rc = clock_gettime(CLOCK_REALTIME_COARSE, &spec);
  if(rc){
    int const errNo = errno;
    if(EINVAL==errNo){
      rc = CWAL_RC_UNSUPPORTED;
    }else{
      rc = cwal_errno_to_cwal_rc( errNo, CWAL_RC_ERROR );
    }
    return cwal_exception_setf(args->engine, rc,
                               "clock_gettime() failed with errno %d (%s).",
                               errNo, strerror(errNo));
  }else{
    cwal_int_t const it = (cwal_int_t)spec.tv_sec * 1000;
    if(spec.tv_sec != (time_t)(it/1000)){
      /* too large for our int type. Let's hope it will fit in a
         double... */
      *rv = cwal_new_double(args->engine, (double)it
                            + spec.tv_nsec/1000000);
    }else{
      *rv = cwal_new_integer(args->engine, (cwal_int_t)it
                             + spec.tv_nsec/1000000);
    }
    return *rv ? 0 : CWAL_RC_OOM;
  }
#endif
}

int whcl_cb_strftime( cwal_callback_args const * args, cwal_value **rv ){
  time_t timt = -1;
  enum {BufSize = 512};
  char buf[BufSize];
  struct tm * tim;
  cwal_size_t sf;
  bool isLocal;
  char const * fmt = args->argc
    ? cwal_value_get_cstr(args->argv[0], NULL)
    : NULL;
  if(!fmt){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting a format string.");
  }
  if(args->argc>1){
#if CWAL_INT_T_BITS > 32
    timt = (time_t)cwal_value_get_integer(args->argv[1]);
#else
    /* Let's hope 48 bits is sufficient! */
    timt = (time_t)cwal_value_get_double(args->argv[1]);
#endif
  }
  isLocal = (args->argc>2)
    ? cwal_value_get_bool(args->argv[2])
    : 0;
  if(timt < 0){
    timt = time(NULL);
  }
  tim = isLocal ? localtime(&timt) : gmtime(&timt);
  memset(buf, 0, BufSize);
  sf = whcl__strftime( buf, (cwal_size_t)BufSize, fmt, tim );
  if(!sf && *buf){
    return
      cwal_exception_setf(args->engine, CWAL_RC_RANGE,
                          "strftime() failed. Possibly some "
                          "value is too long or some buffer "
                          "too short.");
  }
  assert(!buf[sf]);
  *rv = cwal_new_string_value(args->engine, buf, sf);
  return *rv ? 0 : CWAL_RC_OOM;    
}

int whcl__install_time( whcl_engine * const el ){
  if(el->cache.installAPI & WHCL__INSTALL_API_time) return 0;
  cwal_value * sub = NULL;
  cwal_value * const tgt = el->cache.vWhcl;
  char const * name = "time";
  int rc;
  rc = whcl_install_core_apis(el);
  if(0==rc) rc = whcl__install_sub(el, tgt, name, true, &sub);
  if(0==rc){
    whcl_func_def const funcs[] = {
      WHCL_FUNC2("sleep", whcl_cb_sleep),
      WHCL_FUNC2("time", whcl_cb_time),
      WHCL_FUNC2("mssleep", whcl_cb_mssleep),
      WHCL_FUNC2("mstime", whcl_cb_mstime),
      WHCL_FUNC2("strftime", whcl_cb_strftime),
      whcl_func_def_empty_m
    };
    rc = whcl_install_functions(el, sub, funcs, 0);
  }
  if(0==rc) el->cache.installAPI |= WHCL__INSTALL_API_time;
  return rc;
}

#include <stdio.h>
#include <assert.h>
#include <string.h>

#undef MARKER
