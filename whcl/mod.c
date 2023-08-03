/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

/**
   Impls for the `whcl.install-api` and loadable modules support.
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
   Internal helper for setting up initialization of compiled-in static
   modules. It whcl_module_init()'s the given module then sets its
   result to tgt[name]. Returns the result of the init call.
*/
static int whcl__static_mod_init( whcl_engine * const el,
                                  whcl_loadable_module const * const mod,
                                  char const * name,
                                  cwal_value **rv ){
  int rc = 0;
  cwal_value * mrv = 0;
  if(!name) name = mod->name;
  rc = whcl_module_init(el, mod, &mrv);
  if(0==rc){
    cwal_ref(mrv);
    rc = whcl_set( el, el->cache.vWhcl, name, -1,
                   mrv ? mrv : cwal_value_undefined() );
    cwal_unref(mrv);
    if(0==rc) *rv = mrv;
  }
  return rc;
}

static int whcl__module_init_require(whcl_engine * const el, cwal_value **rv){
  extern unsigned char const * whcl_src_require(unsigned int *)
    /* Generated code: src_require.c */;
  unsigned int len = 0;
  char const * src = (char const *)whcl_src_require(&len);
  assert(len);
  return whcl_eval_cstr(el, true, "require.whcl", src, (cwal_int_t)len, rv );
}

static int whcl__install_require(whcl_engine * const el){
  if(el->cache.installAPI & WHCL__INSTALL_API_require) return 0;
  cwal_value * sub = NULL;
  int rc = whcl__module_init_require(el, &sub);
  if(0==rc){
    rc = whcl_set_with_flags(el, el->cache.vWhcl, "require", 7,
                             sub, CWAL_VAR_F_CONST);
  }
  if(0==rc) el->cache.installAPI |= WHCL__INSTALL_API_require;
  return rc;
}

static int whcl__install_script(whcl_engine * const el){
  if(el->cache.installAPI & WHCL__INSTALL_API_script) return 0;
  int rc;
  cwal_value * const sub = whcl__script_prototype(el);
  if(!sub){
    rc = whcl_err_has(el, true);
    if(!rc) {WHCL__WARN_OOM; rc = CWAL_RC_OOM;}
  }else{
    assert(whcl_get(el, el->cache.vWhcl, "Script", 6));
    rc = 0;
  }
  if(0==rc) el->cache.installAPI |= WHCL__INSTALL_API_script;
  return rc;
}

/**
   List of APIs which have built-in support via whcl_install_api().
   The types in this list have disparate installation APIs, distinct
   from the module loader API. Ideally we'll eventually port all of
   those to use the module loader API.

   The first entry must be MO(api_name) and all others must be
   M(api_name). Their order is the order they appear in the
   WhclBuiltinModNames and WhclBuiltinModNamesList values. Their order
   has no effect on the module's lookup times - those are O(1).

   Maintenance reminders:

   - Ensure that whcl_install_api() has all of
   these installed and does _not_ have any which are not in this list.

   - The list of builtin modules needs to be mainted in `createHashes.sh`.
   Updating and running `./createHashes.sh install-api` is needed to
   generate the O(1) lookup tables (paste that code into
   whcl_install_api()) and WHCL__BUILTIN_MODULE_INITS.
*/
#define WHCL__BUILTIN_MODULE_INITS \
  M0(fs) M(json) M(ob) M(os) M(PathFinder) \
  M(require) M(Script) M(time) M(tmpl)

/** Init builtin modules which use the module loader install
    interface. */
#define WHCL__STATIC_MODULE_BUILTINS M(math)
#if defined(WHCL_STATIC_MODULE_INITS)
#  define WHCL__STATIC_MODULE_INITS \
  WHCL__STATIC_MODULE_BUILTINS WHCL_STATIC_MODULE_INITS
/* ^^^^ WHCL__STATIC_MODULE_BUILTINS needs to come first to simplify
   createHashes.sh (it doesn't have to care whether it's generating the
   first entry in the list). */
#else
#  define WHCL__STATIC_MODULE_INITS WHCL__STATIC_MODULE_BUILTINS
#endif

/**
   This holds a list of all module names which are being initialized
   via the "static module" process. We use this to give us a way to
   disable activation of built-in APIs which are being installed via
   their static module form.

   The names used here must be the canonical name of the module.
*/
static char const * const WhclBuiltinModNamesList[] = {
#define M0(X) # X,
#define M(X) # X,
WHCL__BUILTIN_MODULE_INITS
#undef M

#define M(X) # X,
#define M2(X,Y) # Y,
WHCL__STATIC_MODULE_INITS
#undef M
#undef M2
#undef M0

NULL /* list sentinel */
};
static char const * const WhclBuiltinModNames =
#define M0(X) # X
#define M(X) " " # X
WHCL__BUILTIN_MODULE_INITS
#undef M

#define M(X) " " # X
#define M2(X,Y) " " # Y
WHCL__STATIC_MODULE_INITS
#undef M
#undef M2
#undef M0
;
#undef WHCL__BUILTIN_MODULE_INITS
void whcl_install_api_list(char const **oneString, char const * const ** strList){
  if(oneString) *oneString = WhclBuiltinModNames;
  if(strList) *strList = WhclBuiltinModNamesList;
}

static int whcl__static_mod_get(whcl_engine * const el, char const *name,
                                whcl_loadable_module const * const mod,
                                cwal_value **rv){
  int rc = 0;
  cwal_value * v = NULL;
  whcl_lookup_vsym(el, el->cache.vWhcl, name, -1, false, &v);
  if(v){ if(rv) *rv = v; }
  else{
    rc = whcl__static_mod_init(el, mod, name, &v);
    if(0==rc && rv) *rv = v;
    else if(v) cwal_refunref(v);
  }
  return rc;
}

int whcl_install_api(whcl_engine * const el, char const * apiName,
                     cwal_value **rv){
  uint16_t const nName = (uint16_t)cwal_strlen(apiName);
  /**
     Initializes the statically-linked module via function
     whcl_module_init_##MODNAME(), installing it as whcl[MODNAME].
  */
  int rc;
  rc = whcl_install_core_apis(el);
  if(rc) return rc;
  rc = CWAL_RC_NOT_FOUND;
  //MARKER(("install-api: %.*s\n", (int)nName, apiName));
#define THEN(W,F)                                               \
    if(sizeof(W)-1==nName && 0==memcmp(apiName,W,nName)) {      \
      rc = F(el);                                               \
      if(0==rc && rv){                                          \
        *rv = cwal_prop_get(el->cache.vWhcl, W, sizeof(W)-1);   \
      } break;                                                  \
    } else goto nope /* fall back to check for an optional API */

  switch(whcl__hash_keyword(apiName, (uint16_t)nName)){
    /* Values generated by kwhasher.c */
    case 0x001fb7e8: THEN("tmpl",whcl__install_tmpl);
    case 0x0006f669: THEN("fs",whcl__install_fs);
    case 0x00f99d1e: THEN("require",whcl__install_require);
    case 0x001cfee6: THEN("json",whcl__install_json);
    case 0x0007937d: THEN("os",whcl__install_os);
    case 0x057a95e6: THEN("PathFinder",whcl__install_pf);
    case 0x000792c2: THEN("ob",whcl__install_ob);
    case 0x001fb5cd: THEN("time",whcl__install_time);
    case 0x005af500: THEN("Script",whcl__install_script);

#undef THEN

    default:
      nope:
#define WHCL__MODULE_INIT2(CNAME,NAME)                               \
    WHCL_MODULE_DECL(CNAME);                                       \
    rc = whcl__static_mod_get(el, #NAME, whcl_module_ ## CNAME, rv); \
    break
      /**
         WHCL__STATIC_MODULE_INITS must be defined as a list of
         M(name) and/or M2(c_name,mod-name) calls, starting with an
         M0(name) call (for reasons unrelated to _this_ code).

         Each M in that list will add the named module to the list
         of installable APIs and the library/binary must link in the
         module's object(s)/library(ies). This is an alternative to
         dynamically loading. After running the module init routine,
         the mode gets installed as whcl[name] and future calls to
         this function will result in the same value (so long as
         it's not deleted from the whcl object).

         M2 takes 2 names: the C-friendly one and the whcl-friendly
         one. The M(X) and M0(X) macros translates simply to M2(X,X).
      */
#define M2(CN,N)                                                 \
      if(sizeof(#N)-1==nName && 0==memcmp(apiName,#N,nName)) {  \
        WHCL__MODULE_INIT2(CN,N);                               \
      }
#define M(N) M2(N,N)
      WHCL__STATIC_MODULE_INITS
#undef M
#undef M2
#undef WHCL__MODULE_INIT2
      rc = whcl_err_throw(el, CWAL_RC_NOT_FOUND,
                          "Unknown API: %.*s. Expecting one of: %s",
                          (int)nName, apiName, WhclBuiltinModNames);
    break;
  }
  return rc;
}
#undef WHCL__STATIC_MODULE_INITS
#undef WHCL__STATIC_MODULE_BUILTINS

int whcl__cb_install_api( cwal_callback_args const * args, cwal_value **rv ){
  int rc = 0;
  if(!args->argc){
    *rv = cwal_new_string_value(args->engine, WhclBuiltinModNames,
                                cwal_strlen(WhclBuiltinModNames));
    if(!*rv){ WHCL__WARN_OOM; rc = CWAL_RC_OOM; }
    return rc;
  }
  cwal_size_t nStr = 0;
  char const * str;
  whcl_engine * const el = whcl_engine_from_args(args);
  *rv = args->self;
  for( uint16_t i = 0; 0==rc && i < args->argc; ++i ){
    str = cwal_value_get_cstr(args->argv[i], &nStr);
    if(!str){
      rc = cwal_cb_throw(args, CWAL_RC_TYPE,
                         "Got a non-string argument.");
      break;
    }
    rc = whcl_install_api(el, str, i==args->argc-1 ? rv : NULL);
  }
  return rc;
}


/** @def WHCL_HAVE_DLOPEN

    If set to true, use dlopen() and friends. Requires
    linking to -ldl on most platforms.

    Only one of WHCL_HAVE_DLOPEN and WHCL_HAVE_LTDLOPEN may be
    true.
*/
/** @def WHCL_HAVE_LTDLOPEN

    If set to true, use lt_dlopen() and friends. Requires
    linking to -lltdl on most platforms.

    Only one of WHCL_HAVE_DLOPEN and WHCL_HAVE_LTDLOPEN may be
    true.
*/

/*
  #define WHCL_HAVE_DLOPEN 1
  #define WHCL_HAVE_LTDLOPEN 0
*/

#if !defined(WHCL_HAVE_DLOPEN)
#define WHCL_HAVE_DLOPEN 0
#endif

#if !defined(WHCL_HAVE_LTDLOPEN)
#define WHCL_HAVE_LTDLOPEN 0
#endif

#if !defined(WHCL_ENABLE_MODULES)
#  define WHCL_ENABLE_MODULES (WHCL_HAVE_LTDLOPEN || WHCL_HAVE_DLOPEN)
#endif

#ifdef WHCL_OS_WINDOWS
#  include  <io.h>
#else
#  include <unistd.h> /* access() */
#endif

#if WHCL_HAVE_DLOPEN && WHCL_HAVE_LTDLOPEN
#error Only one of WHCL_HAVE_DLOPEN and WHCL_HAVE_LTDLOPEN should be true.
#endif

#if !WHCL_ENABLE_MODULES
typedef int dl_handle_t /* dummy placeholder */;
#elif WHCL_HAVE_DLOPEN
typedef void * dl_handle_t;
#  include <dlfcn.h> /* this actually has a different name on some platforms! */
#elif WHCL_HAVE_LTDLOPEN
#  include <ltdl.h>
typedef lt_dlhandle dl_handle_t;
#else
#  error "We have no dlopen() impl for this configuration."
#endif

#if WHCL_ENABLE_MODULES
static void whcl__dl_init(){
  static char once  = 0;
  if(!once && ++once){
#  if WHCL_HAVE_LTDLOPEN
    lt_dlinit();
    lt_dlopen( 0 );
#  elif WHCL_HAVE_DLOPEN
    dlopen( 0, RTLD_NOW | RTLD_GLOBAL );
#  endif
  }
}
#endif

#if WHCL_ENABLE_MODULES
static dl_handle_t dl__open( char const * fname, char const **errMsg ){
  dl_handle_t soh;
#if WHCL_HAVE_LTDLOPEN
  soh = lt_dlopen( fname );
#elif WHCL_HAVE_DLOPEN
  soh = dlopen( fname, RTLD_NOW | RTLD_GLOBAL );
#endif
  if(!soh && errMsg){
#if WHCL_HAVE_LTDLOPEN
    *errMsg = lt_dlerror();
#elif WHCL_HAVE_DLOPEN
    *errMsg = dlerror();
#endif
  }
  return soh;
}
#endif

#if WHCL_ENABLE_MODULES
static whcl_loadable_module const * whcl_dl_sym( dl_handle_t soh, char const * mname ){
  void * sym =
#if WHCL_HAVE_LTDLOPEN
    lt_dlsym( soh, mname )
#elif WHCL_HAVE_DLOPEN
    dlsym( soh, mname )
#else
    NULL
#endif
    ;
  return sym ? *((whcl_loadable_module const **)sym) : NULL;
}
#endif

#define WHCL_CLOSE_DLLS 1
/*
  Years of practice have shown that it is literally impossible to
  safely close DLLs because simply opening one may trigger arbitrary
  code (at least for C++ DLLs) which "might" be used by the
  application. e.g. some classloaders use DLL initialization to inject
  new classes into the application without the app having to do anything
  more than open the DLL.

  So whcl does not close DLLs. Except (...sigh...) to try to please
  valgrind.
*/
#if WHCL_ENABLE_MODULES
static void whcl__dl_close( dl_handle_t soh ){
#if WHCL_CLOSE_DLLS
  /* MARKER(("Closing loaded module @%p.\n", (void const *)soh)); */
#if WHCL_HAVE_LTDLOPEN
  lt_dlclose( soh );
#elif WHCL_HAVE_DLOPEN
  dlclose( soh );
#endif
#endif
}
#endif


#if WHCL_ENABLE_MODULES
/**
   Looks for a symbol in the given DLL handle. If symName is NULL or
   empty, the symbol "whcl_module" is used, else the symbols
   ("whcl_module_" + symName) is used. If it finds one, it casts it to
   whcl_loadable_module and returns it. On error it may update se->err
   with the error information.
*/
static whcl_loadable_module const *
whcl__module_fish_out_entry_pt(whcl_engine * const el,
                               dl_handle_t soh,
                               char const * symName){
  enum { MaxLen = 128 };
  char buf[MaxLen] = {0};
  cwal_size_t const slen = symName ? cwal_strlen(symName) : 0;
  whcl_loadable_module const * mod;
  if(slen > (MaxLen-20)){
    whcl_err_set(el, CWAL_RC_RANGE,
                 "DLL symbol name '%.*s' is too long.",
                 (int)slen, symName);
    return 0;
  }
  if(!slen){
    memcpy(buf, "whcl_module", 11);
    buf[11] = 0;
  }else{
    snprintf(buf, MaxLen,"whcl_module_%s", symName);
  }
  mod = (whcl_loadable_module*)whcl_dl_sym( soh, buf );
  /* MARKER(("whcl_module_fish_out_entry_pt [%s] ==> %p\n",buf,
     (void const *)mod)); */
  return mod;
}
#endif/*WHCL_ENABLE_MODULES*/


#if WHCL_ENABLE_MODULES
/**
   Tries to dlsym() the given whcl_loadable_module symbol from the given
   DLL handle. On success, 0 is returned and *mod is assigned to the
   memory. On error, non-0 is returned and se's error state may be
   updated.

   Ownership of the returned module ostensibly lies with the first
   argument, but that's not entirely true. If WHCL_CLOSE_DLLS is true
   then a copy of the module's pointer is stored in the engine for
   later closing. The memory itself is owned by the module loader, and
   "should" stay valid until the DLL is closed.
*/
static int whcl__module_get_sym( whcl_engine * const el,
                                 dl_handle_t soh,
                                 char const * symName,
                                 whcl_loadable_module const ** mod ){

  whcl_loadable_module const * lm;
  int rc;
  whcl_err_reset(el);
  lm = whcl__module_fish_out_entry_pt(el, soh, symName);
  rc = whcl_err_has(el, false);
  if(rc) return rc;
  else if(!lm){
    whcl__dl_close(soh);
    return whcl_err_set(el, CWAL_RC_NOT_FOUND,
                        "Did not find module entry point symbol '%s'.",
                        symName);
  }
  *mod = lm;
  if(WHCL_CLOSE_DLLS){
    /* Stash soh for potential closing later on. */
    cwal_size_t i = 0;
    bool found = 0;
    for( ; i < el->modules.count; ++i ){
      if(soh == el->modules.list[i]){
        found = 1;
        break;
      }
    }
    if(!found){
      rc = cwal_list_append(el->ec, &el->modules, soh);
      if(rc){
        whcl__dl_close(soh);
        lm = 0;
        /* This is an allocation error, so don't
           bother updating whcl_engine::err.
        */
      }
    }
  }/*WHCL_CLOSE_DLLS*/
  return rc;
}
#endif/*WHCL_ENABLE_MODULES*/

#if !WHCL_ENABLE_MODULES
static int whcl__module_no_modules( whcl_engine * const el ){
  return whcl_err_set(el, CWAL_RC_UNSUPPORTED,
                      "No dlopen() equivalent is installed "
                      "for this build configuration.");
}
#endif

int whcl_module_init( whcl_engine * const el,
                      whcl_loadable_module const * const mod,
                      cwal_value ** rv){
  if(!mod->init){
    return whcl_err_set(el, CWAL_RC_MISUSE,
                        "Module '%s' has no init method",
                        mod->name);
  }
  cwal_scope scope = cwal_scope_empty;
  int rc = cwal_scope_push2(el->ec, &scope);
  if(rc) return rc;
  else{
    cwal_value * xrv = 0;
    rc = mod->init( el, &xrv );
    cwal_scope_pop2(el->ec, (rc||!rv) ? NULL : xrv);
    if(!rc && rv) *rv = xrv;
  }
  return rc;
}

int whcl_module_extract( whcl_engine * const el,
                         char const * fname,
                         char const * symName,
                         whcl_loadable_module const ** mod ){
#if !WHCL_ENABLE_MODULES
  if(fname || symName || mod){/*avoid unused param warning*/}
  return whcl__module_no_modules(el);
#else
#if !WHCL_HAVE_LTDLOPEN && !WHCL_HAVE_DLOPEN
#   error "We have no dlopen() and friends impl for this configuration."
#endif
  if(!fname || !*fname || !mod) return CWAL_RC_MISUSE;
  else {
    dl_handle_t soh;
    char const * errMsg = 0;
    whcl__dl_init();
    soh = dl__open( fname, &errMsg );
    if(!soh){
      return errMsg
        ? whcl_err_set(el, CWAL_RC_ERROR, "DLL open failed: %s",
                       errMsg)
        : whcl_err_set(el, CWAL_RC_ERROR,
                       "DLL open failed for unknown reason.");
    }else{
      whcl_loadable_module const * x = 0;
      int const rc = whcl__module_get_sym( el, soh, symName, &x );
      if(!rc){
        assert(x);
        if(mod) *mod = x;
      }
      return rc;
    }
  }
#endif
}

int whcl_module_load( whcl_engine * const el,
                      char const * fname,
                      char const * symName,
                      cwal_value ** rv ){
#if !WHCL_ENABLE_MODULES
  if(fname || symName || rv){/*avoid unused param warning*/}
  return whcl__module_no_modules(el);
#else
#  if !WHCL_HAVE_LTDLOPEN && !WHCL_HAVE_DLOPEN
#    error "We have no dlopen() and friends impl for this configuration."
#  endif
  if(!fname || !*fname || !rv) return CWAL_RC_MISUSE;
  else {
    whcl_loadable_module const * mod = 0;
    int rc = whcl_module_extract( el, fname, symName, &mod );
    if(!rc){
      assert(mod);
      rc = whcl_module_init(el, mod, rv);
    }
    return rc;
  }
#endif
}

int whcl_cb_module_load( cwal_callback_args const * args,
                         cwal_value **rv ){
#if !WHCL_ENABLE_MODULES
  if(rv){/*avoid unused param warning*/}
  return whcl__module_no_modules(whcl_engine_from_args(args));
#else
  int rc = 0;
  char const * fn;
  char const * sym = NULL;
  cwal_value * mod = 0;
  whcl_engine * const el = whcl_engine_from_args(args);
  assert(el);
  //rc = whcl_disable_check_throw(se, WHCL_DISABLE_FS_READ);
  //if(rc) return rc;
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
  rc = whcl_module_load(el, fn, sym, &mod);
  /*MARKER(("load_module(%s, %s) rc=%s\n", fn, sym, cwal_rc_cstr(rc)));*/
  switch(rc){
    case 0:
      assert(mod);
      *rv = mod;
      break;
    case CWAL_RC_EXCEPTION:
    case CWAL_RC_INTERRUPTED:
    case CWAL_RC_OOM:
      break;
    case CWAL_RC_RETURN:
      *rv = cwal_propagating_take(args->engine);
      assert(*rv && "Else CWAL_RC_RETURN semantics were violated.");
      rc = 0;
      break;
    case CWAL_RC_BREAK:
      cwal_propagating_set(args->engine, NULL);
      CWAL_SWITCH_FALL_THROUGH;
    default:{
      /* We are pre-empting various codes here, e.g. CWAL_RC_EXIT,
         CWAL_RC_FATAL, and CWAL_RC_ASSERT. We do not want plugin
         initialization to be able to quit the top-running script that
         way (e.g. a failed whcl assert in some init script code). */
      if(cwal_engine_error_get(args->engine, NULL, NULL)){
        rc = cwal_error_throw(args->engine, 0, 0, 0, 0);
      }else{
        rc = cwal_cb_throw(args, rc,
                           "Loading module failed with code %s.",
                           cwal_rc_cstr(rc));
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


void whcl__modules_close( whcl_engine * const el ){
#if !WHCL_ENABLE_MODULES
  if(el){/*unused*/}
  assert(!el->modules.list);
#else
  if(el->modules.list){
    /* dlcose() does not seem to care whether they are closed
       in reverse-opened order or not, and it leaks all of
       them on my box. lt_dlclose() leaks even more!
    */
#if WHCL_CLOSE_DLLS
    int i;
    assert(el->modules.count);
    i = ((int)el->modules.count) - 1;
    for( ; i >= 0; --i ){
      void * soh = el->modules.list[i];
      assert(soh);
      whcl__dl_close(soh);
    }
    cwal_list_reserve( el->ec, &el->modules, 0 );
#else
    /*
      Closing DLLs is NOT generically safe because we may
      stomp resources used elsewhere. It can't be done 100%
      reliably at this level, i am fully convinced. Let the OS
      clean them up.
    */
#endif
    assert(!el->modules.list);
    assert(!el->modules.count);
    assert(!el->modules.alloced);
  }
#endif
}

#undef MARKER
#undef WHCL_CLOSE_DLLS
