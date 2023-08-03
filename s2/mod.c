/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#include "libs2.h" /* first for any config options */
#include <assert.h>
#include <memory.h> /* strlen() */


#if 1
#include <stdio.h>
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__); if(1) printf pfexp
#else
#define MARKER(pfexp) (void)0
#endif



/** @def S2_HAVE_DLOPEN

    If set to true, use dlopen() and friends. Requires
    linking to -ldl on most platforms.

    Only one of S2_HAVE_DLOPEN and S2_HAVE_LTDLOPEN may be
    true.
*/
/** @def S2_HAVE_LTDLOPEN

    If set to true, use lt_dlopen() and friends. Requires
    linking to -lltdl on most platforms.

    Only one of S2_HAVE_DLOPEN and S2_HAVE_LTDLOPEN may be
    true.
*/

/*
  #define S2_HAVE_DLOPEN 1
  #define S2_HAVE_LTDLOPEN 0
*/

#if !defined(S2_HAVE_DLOPEN)
#define S2_HAVE_DLOPEN 0
#endif

#if !defined(S2_HAVE_LTDLOPEN)
#define S2_HAVE_LTDLOPEN 0
#endif

#if !defined(S2_ENABLE_MODULES)
#  define S2_ENABLE_MODULES (S2_HAVE_LTDLOPEN || S2_HAVE_DLOPEN)
#endif

#ifdef S2_OS_WINDOWS
#  include  <io.h>
#else
#  include <unistd.h> /* access() */
#endif

#if S2_HAVE_DLOPEN && S2_HAVE_LTDLOPEN
#error Only one of S2_HAVE_DLOPEN and S2_HAVE_LTDLOPEN should be true.
#endif

#if !S2_ENABLE_MODULES
typedef int dl_handle_t /* dummy placeholder */;
#elif S2_HAVE_DLOPEN
typedef void * dl_handle_t;
#  include <dlfcn.h> /* this actually has a different name on some platforms! */
#elif S2_HAVE_LTDLOPEN
#  include <ltdl.h>
typedef lt_dlhandle dl_handle_t;
#else
#  error "We have no dlopen() impl for this configuration."
#endif

#if S2_ENABLE_MODULES
static void s2_dl_init(){
  static char once  = 0;
  if(!once && ++once){
#  if S2_HAVE_LTDLOPEN
    lt_dlinit();
    lt_dlopen( 0 );
#  elif S2_HAVE_DLOPEN
    dlopen( 0, RTLD_NOW | RTLD_GLOBAL );
#  endif
  }
}
#endif

#if S2_ENABLE_MODULES
static dl_handle_t dl_open( char const * fname, char const **errMsg ){
  dl_handle_t soh;
#if S2_HAVE_LTDLOPEN
  soh = lt_dlopen( fname );
#elif S2_HAVE_DLOPEN
  soh = dlopen( fname, RTLD_NOW | RTLD_GLOBAL );
#endif
  if(!soh && errMsg){
#if S2_HAVE_LTDLOPEN
    *errMsg = lt_dlerror();
#elif S2_HAVE_DLOPEN
    *errMsg = dlerror();
#endif
  }
  return soh;
}
#endif

#if S2_ENABLE_MODULES
static s2_loadable_module const * s2_dl_sym( dl_handle_t soh, char const * mname ){
  void * sym =
#if S2_HAVE_LTDLOPEN
    lt_dlsym( soh, mname )
#elif S2_HAVE_DLOPEN
    dlsym( soh, mname )
#else
    NULL
#endif
    ;
  return sym ? *((s2_loadable_module const **)sym) : NULL;
}
#endif

#define S2_CLOSE_DLLS 1
/*
  Years of practice have shown that it is literally impossible to
  safely close DLLs because simply opening one may trigger arbitrary
  code (at least for C++ DLLs) which "might" be used by the
  application. e.g. some classloaders use DLL initialization to inject
  new classes into the application without the app having to do anything
  more than open the DLL.

  So s2 does not close DLLs. Except (...sigh...) to try to please
  valgrind.
*/
#if S2_ENABLE_MODULES
static void s2_dl_close( dl_handle_t soh ){
#if S2_CLOSE_DLLS
  /* MARKER(("Closing loaded module @%p.\n", (void const *)soh)); */
#if S2_HAVE_LTDLOPEN
  lt_dlclose( soh );
#elif S2_HAVE_DLOPEN
  dlclose( soh );
#endif
#endif
}
#endif


#if S2_ENABLE_MODULES
/**
   Looks for a symbol in the given DLL handle. If symName is NULL or
   empty, the symbol "s2_module" is used, else the symbols
   ("s2_module_" + symName) is used. If it finds one, it casts it to
   s2_loadable_module and returns it. On error it may update se->err
   with the error information.
*/
static s2_loadable_module const *
s2_module_fish_out_entry_pt(s2_engine * se,
                            dl_handle_t soh,
                            char const * symName){
  enum { MaxLen = 128 };
  char buf[MaxLen] = {0};
  cwal_size_t const slen = symName ? cwal_strlen(symName) : 0;
  s2_loadable_module const * mod;
  if(slen > (MaxLen-20)){
    s2_engine_err_set(se, CWAL_RC_RANGE,
                      "DLL symbol name '%.*s' is too long.",
                      (int)slen, symName);
    return 0;
  }
  if(!slen){
    memcpy(buf, "s2_module", 9);
    buf[9] = 0;
  }else{
    sprintf(buf,"s2_module_%s", symName);
  }
  mod = (s2_loadable_module*)s2_dl_sym( soh, buf );
  /* MARKER(("s2_module_fish_out_entry_pt [%s] ==> %p\n",buf,
     (void const *)mod)); */
  return mod;
}
#endif/*S2_ENABLE_MODULES*/


#if S2_ENABLE_MODULES
/**
   Tries to dlsym() the given s2_loadable_module symbol from the given
   DLL handle. On success, 0 is returned and *mod is assigned to the
   memory. On error, non-0 is returned and se's error state may be
   updated.

   Ownership of the returned module ostensibly lies with se, but
   that's not entirely true. If S2_CLOSE_DLLS is true then a copy of
   the module's pointer is stored in se for later closing. The memory
   itself is owned by the module loader, and "should" stay valid
   until the DLL is closed.
*/
static int s2_module_get_sym( s2_engine * se,
                              dl_handle_t soh,
                              char const * symName,
                              s2_loadable_module const ** mod ){

  s2_loadable_module const * lm;
  int rc;
  s2_engine_err_reset(se);
  lm = s2_module_fish_out_entry_pt(se, soh, symName);
  rc = s2_engine_err_has(se);
  if(rc) return rc;
  else if(!lm){
    s2_dl_close(soh);
    return s2_engine_err_set( se, CWAL_RC_NOT_FOUND,
                              "Did not find module entry point symbol '%s'.",
                              symName);
  }
  *mod = lm;
  if(S2_CLOSE_DLLS){
    /* Stash soh for potential closing later on. */
    cwal_size_t i = 0;
    bool found = 0;
    for( ; i < se->modules.count; ++i ){
      if(soh == se->modules.list[i]){
        found = 1;
        break;
      }
    }
    if(!found){
      rc = cwal_list_append(se->e, &se->modules, soh);
      if(rc){
        s2_dl_close(soh);
        lm = 0;
        /* This is an allocation error, so don't
           bother updating s2_engine::err.
        */
      }
    }
  }/*S2_CLOSE_DLLS*/
  return rc;
}
#endif/*S2_ENABLE_MODULES*/

#if !S2_ENABLE_MODULES
static int s2_module_no_modules( s2_engine * se ){
  return s2_engine_err_set(se, CWAL_RC_UNSUPPORTED,
                           "No dlopen() equivalent is installed "
                           "for this build configuration.");
}
#endif

int s2_module_init( s2_engine * se,
                    s2_loadable_module const * mod,
                    cwal_value ** rv){
  int rc;
  cwal_scope sc = cwal_scope_empty;
  if(!mod->init) return CWAL_RC_MISUSE;
  rc = cwal_scope_push2(se->e, &sc);
  if(!rc){
    cwal_value * xrv = 0;
    rc = mod->init( se, &xrv );
    cwal_scope_pop2(se->e, (rc||!rv) ? 0 : xrv);
    if(!rc && rv) *rv = xrv;
  }
  return rc;
}

int s2_module_extract( s2_engine * se,
                       char const * fname,
                       char const * symName,
                       s2_loadable_module const ** mod ){
#if !S2_ENABLE_MODULES
  if(fname || symName || mod){/*avoid unused param warning*/}
  return s2_module_no_modules(se);
#else
#if !S2_HAVE_LTDLOPEN && !S2_HAVE_DLOPEN
#   error "We have no dlopen() and friends impl for this configuration."
#endif
  if(!se || !fname || !*fname || !mod) return CWAL_RC_MISUSE;
  else {
    dl_handle_t soh;
    char const * errMsg = 0;
    s2_dl_init();
    soh = dl_open( fname, &errMsg );
    if(!soh){
      if(errMsg){
        return s2_engine_err_set(se, CWAL_RC_ERROR,
                                 "DLL open failed: %s",
                                 errMsg);
      }else{
        return CWAL_RC_ERROR;
      }
    }
    else {
      s2_loadable_module const * x = 0;
      int const rc = s2_module_get_sym( se, soh, symName, &x );
      if(!rc){
        assert(x);
        if(mod) *mod = x;
      }
      return rc;
    }
  }
#endif
}


int s2_module_load( s2_engine * se,
                    char const * fname,
                    char const * symName,
                    cwal_value ** rv ){
#if !S2_ENABLE_MODULES
  if(fname || symName || rv){/*avoid unused param warning*/}
  return s2_module_no_modules(se);
#else
#  if !S2_HAVE_LTDLOPEN && !S2_HAVE_DLOPEN
#    error "We have no dlopen() and friends impl for this configuration."
#  endif
  if(!se || !fname || !*fname || !rv) return CWAL_RC_MISUSE;
  else {
    s2_loadable_module const * mod = 0;
    int rc = s2_module_extract( se, fname, symName, &mod );
    if(!rc){
      assert(mod);
      rc = s2_module_init(se, mod, rv);
    }
    return rc;
  }
#endif
}

int s2_cb_module_load( cwal_callback_args const * args,
                       cwal_value **rv ){
  s2_engine * se;
  int rc = 0;
#if !S2_ENABLE_MODULES
  se = s2_engine_from_args(args);
  assert(se);
  rc = s2_module_no_modules(se);
  if(rv){/*avoid unused param warning*/}
  return CWAL_RC_UNSUPPORTED==rc
    ? s2_throw_err( se, 0, 0, 0, 0 )
    : rc /* presumably CWAL_RC_OOM */;
#else
  char const * fn;
  char const * sym = NULL;
  cwal_value * mod = 0;
  se = s2_engine_from_args(args);
  assert(se);
  rc = s2_disable_check_throw(se, S2_DISABLE_FS_READ);
  if(rc) return rc;
  fn = cwal_string_cstr(cwal_value_get_string(args->argv[0]));
  if(!fn){
    goto misuse;
  }
  if(args->argc>1){
    if(cwal_value_is_string(args->argv[1])){
      sym = cwal_value_get_cstr(args->argv[1], 0);
    }else{
      goto misuse;
    }
  }
  rc = s2_module_load(se, fn, sym, &mod);
  /* MARKER(("load_module(%s, %s) rc=%d\n", fn, sym, rc)); */
  switch(rc){
    case 0:
      assert(mod);
      *rv = mod;
      break;
    case CWAL_RC_EXCEPTION:
    case CWAL_RC_INTERRUPTED:
    case CWAL_RC_OOM:
      break;
    case CWAL_RC_BREAK:
    case CWAL_RC_RETURN:
      s2_propagating_set(se, 0);
      CWAL_SWITCH_FALL_THROUGH;
    default:{
      /* We are pre-empting various codes here, e.g. CWAL_RC_EXIT,
         CWAL_RC_FATAL, and CWAL_RC_ASSERT. We do not want plugin
         initialization to be able to quit the top-running script that
         way (e.g. a failed s2 assert in some init script code). */
      if(se->e->err.code){
        rc = s2_throw_err(se, 0, 0, 0, 0);
        s2_engine_err_reset(se);
      }
      else{
        rc = cwal_cb_throw(args, rc,
                         "Loading module failed with code "
                         "%d (%s).", rc, cwal_rc_cstr(rc));
      }
      break;
    }
  }
  return rc;
  misuse:
  return cwal_cb_throw(args, CWAL_RC_MISUSE,
                     "Module loader expects "
                     "(string [,string]]) arguments.");
#endif
}


void s2_modules_close( s2_engine * se ){
    if(se){
#if !S2_ENABLE_MODULES
      assert(!se->modules.list);
#else
      if(se->modules.list){
        /* dlcose() does not seem to care whether they are closed
           in reverse-opened order or not, and it leaks all of
           them on my box. lt_dlclose() leaks even more!
        */
#if S2_CLOSE_DLLS
            int i;
            assert(se->modules.count);
            i = ((int)se->modules.count) - 1;
            for( ; i >= 0; --i ){
                void * soh = se->modules.list[i];
                assert(soh);
                s2_dl_close(soh);
            }
            cwal_list_reserve( se->e, &se->modules, 0 );
#else
            /*
              Closing DLLs is NOT generically safe because we may
              stomp resources used elsewhere. It can't be done 100%
              reliably at this level, i am fully convinced. Let the OS
              clean them up.
            */
#endif
            assert(!se->modules.list);
            assert(!se->modules.count);
            assert(!se->modules.alloced);
        }
#endif
    }
}

#undef MARKER
#undef S2_CLOSE_DLLS
