/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

#include "libs2.h"
#include <assert.h>

#if defined(S2_OS_UNIX)
#include <unistd.h> /* fork() and friends */
#include <sys/types.h> /* pid_t, if not picked up via unistd.h */
#include <errno.h> /* errno! */
#include <string.h> /* strerror() */
#endif

int s2_cb_fork( cwal_callback_args const * args, cwal_value **rv ){
#if !defined(S2_OS_UNIX)
  return cwal_exception_setf(args->engine, CWAL_RC_UNSUPPORTED,
                             "fork(2) is not available in this configuration.");
#else
  pid_t pid;
  char doReturn = 0;
  uint16_t funcIndex = 0;
  cwal_function * f = 0;

  if(args->argc>1){
    doReturn = cwal_value_get_bool(args->argv[0]);
    funcIndex = 1;
  }
  if(!args->argc ||
     !(f = cwal_value_function_part(args->engine,
                                    args->argv[funcIndex]))){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting a Function argument.");
  }
  pid = fork();
  if(-1==pid){
    return (ENOMEM==errno)
      ? CWAL_RC_OOM
      : cwal_exception_setf(args->engine,
                            cwal_errno_to_cwal_rc(errno, CWAL_RC_ERROR),
                            "Fork failed with errno %d (%s).",
                            errno, strerror(errno));
  }
  else if(pid){
    /* Parent */
    *rv = cwal_new_integer(args->engine, (cwal_int_t)pid);
    return *rv ? 0 : CWAL_RC_OOM;
  }else{
    /* Child */
    cwal_value * frv = NULL;
    int frc = cwal_function_call(f, args->self, &frv, 0, NULL);
    /* MARKER(("frc=%d/%s, doReturn=%d\n", frc,
       cwal_rc_cstr(frc), doReturn)); */
    switch(frc){
      case CWAL_RC_OOM:
      case CWAL_RC_EXIT: /* assume exit value was already set. */
      case CWAL_RC_FATAL:
        break;
      case 0:
        if(doReturn){
          if(!frc) *rv = frv ? frv : cwal_value_undefined();
        }else{
          *rv = 0;
          cwal_propagating_set(args->engine,
                               frv ? frv : cwal_value_undefined()
                               /* assertions in s2 require a value
                                  to be propagated for RETURN/EXIT.*/);
        }
        break;
      default:
        break;
    }
    /* MARKER(("frc=%d/%s, doReturn=%d\n", frc,
       cwal_rc_cstr(frc), doReturn)); */
    return (doReturn || frc) ? frc : CWAL_RC_EXIT;
  }
#endif
}

