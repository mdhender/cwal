/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#include "libs2.h"
#include <assert.h>

#if defined(S2_OS_WINDOWS)
#define S2_ENABLE_USLEEP 0
#else
#define S2_ENABLE_USLEEP 1
#endif


#if S2_ENABLE_USLEEP
#  include <string.h> /* strerror() */
#  include <errno.h>
#endif
#if defined(S2_OS_UNIX)
#  include <unistd.h> /* usleep(), possibly _POSIX_TIMERS */
#endif

#include <stdlib.h>

#if defined(_POSIX_TIMERS) && _POSIX_TIMERS>0 && _POSIX_C_SOURCE>=199309L
#  include <time.h>
#  include <errno.h>
#  define S2_ENABLE_CLOCK_GETTIME 1
#else
#  define S2_ENABLE_CLOCK_GETTIME 0
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
static int s2_cb_sleep_impl(cwal_callback_args const * args, cwal_value ** rv,
                            unsigned int delayMult){
#if !defined(S2_ENABLE_USLEEP)
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

int s2_cb_sleep(cwal_callback_args const * args, cwal_value ** rv){
#if !defined(S2_OS_UNIX)
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

int s2_cb_mssleep(cwal_callback_args const * args, cwal_value ** rv){
  return s2_cb_sleep_impl( args, rv, 1000 );
}

int s2_cb_time( cwal_callback_args const * args, cwal_value **rv ){
  time_t const tm = time(NULL);
  cwal_int_t const it = (cwal_int_t)tm;
  if(tm != (time_t)it){ /* too large for our int type... */
    *rv = cwal_new_double(args->engine, (double)tm);
  }else{
    *rv = cwal_new_integer(args->engine, it);
  }
  return *rv ? 0 : CWAL_RC_OOM;
}

int s2_cb_mstime( cwal_callback_args const * args, cwal_value **rv ){
#if !S2_ENABLE_CLOCK_GETTIME
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

int s2_cb_strftime( cwal_callback_args const * args, cwal_value **rv ){
  time_t timt = -1;
  enum {BufSize = 512};
  char buf[BufSize];
  struct tm * tim;
  cwal_size_t sf;
  char isLocal;
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
  sf = s2_strftime( buf, (cwal_size_t)BufSize, fmt, tim );
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

int s2_install_time( s2_engine * se, cwal_value * tgt,
                     char const * name ){
  cwal_value * sub;
  int rc;
  if(!se || !tgt) return CWAL_RC_MISUSE;
  else if(!cwal_props_can(tgt)) return CWAL_RC_TYPE;
  else if(name && *name){
    sub = cwal_new_object_value(se->e);
    if(!sub) return CWAL_RC_OOM;
    cwal_value_ref(sub);
    rc = cwal_prop_set(tgt, name, cwal_strlen(name), sub);
    cwal_value_unref(sub);
    if(rc) return rc;
  }else{
    sub = tgt;
  }

  {
    s2_func_def const funcs[] = {
      S2_FUNC2("sleep", s2_cb_sleep),
      S2_FUNC2("time", s2_cb_time),
      S2_FUNC2("mssleep", s2_cb_mssleep),
      S2_FUNC2("mstime", s2_cb_mstime),
      S2_FUNC2("strftime", s2_cb_strftime),
      s2_func_def_empty_m
    };
    return s2_install_functions(se, sub, funcs, 0);
  }
}


#undef MARKER
