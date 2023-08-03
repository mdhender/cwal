/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

#include "libwhcl.h"
#include "cgimod.h"

#include <stdlib.h> /* getenv() */
#include <string.h> /* strlen() */
#include <assert.h>

/**
   Only for debuggering...
*/
#include <stdio.h>
#define MARKER(pfexp)                                               \
  do{ printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__);   \
    printf pfexp;                                                   \
  } while(0)


/**
   Serves as a copy-constructable default instance and as a type ID
   pointer for cwal_new_native() and friends.
*/
static const cgi_native cgi_native_empty = {NULL, cwal_cgi_empty_m};

static int cwal_value_rescoper_f_cgi( cwal_scope * s, cwal_value * v ){
  cwal_native * n = cwal_value_native_part( s->e, v, &cgi_native_empty );
  cgi_native * cn = (cgi_native *)cwal_native_get( n, &cgi_native_empty );
  cwal_cgi * cgi = &cn->cgi;
  assert(n);
  assert(cgi);
  //MARKER(("Rescoping value/cgi_native 0x%p / 0x%p to level %d\n",
  //(void*)v, (void*)cn, (int)s->level));
  if(cgi->refHolder){
    cwal_value_rescope(s, cgi->refHolder);
  }
  return 0;
}


/**
   cwal finalizer for cgi_native. v must be a (cgi_native*).
*/
static void cgi_native_finalize( cwal_engine * e, void * v ){
  cgi_native * nat = (cgi_native *)v;
  //MARKER(("Finalizing cgi_native@%p (%s@%p)\n", (void *)nat,
  //        cwal_value_type_name(nat->vSelf), (void*)nat->vSelf));
  cwal_cgi_cleanup( &nat->cgi );
  cwal_free2( e, v, sizeof(cgi_native) );
}


/**
   A helper function which comes in handy when script code
   subclasses our native or overrides its methods.
*/
cgi_native * cgi_native_from_value( cwal_engine * const e, cwal_value * v ){
  cgi_native * rc = NULL;
  do{
    cwal_native * n = cwal_value_get_native(v);
    if(n && (rc = cwal_native_get(n, &cgi_native_empty))) break;
    else v = cwal_value_prototype_get(e, v);
  }while(v);
  return rc;
}


static cwal_value * cgi_get_prop( cwal_value * c, char const * key ){
  return c
    ? cwal_prop_get(c, key, cwal_strlen(key))
    : 0;
}

/**
   A proxy for cwal_cgi_init2(). opt is an optional container
   value which this function inspects for config options:

   importEnv: bool[=false]. If true, the request.ENV bits get imported
   into the cgi object. If not set, getenv("CWAL_CGI_IMPORT_ENV") is
   examined and if it contains a non-0 number then that ENV import is
   enabled.

   pushOb: bool[=true]. If true (the default), s2_ob_push()
   is called after cwal_cgi_init2(). If it's false, it's up to
   the client to push a buffer level before starting.

   Returns non-0 on error but does not set any exception state.
*/
static int cgi_init_impl( cwal_cgi * cgi, cwal_value * opt ){
  int initFlags = 0;
  cwal_value * prop;
  int rc = 0;
  /* if(opt) s2_dump_val(opt, "cgi init options"); */
  if(cgi->inited>1) return 0;
  if(!(prop = cgi_get_prop(opt, "pushOb"))/*default is true*/
     || cwal_value_get_bool(prop)){
    initFlags |= CWAL_CGI_INIT2_PUSH_OB;
  }
  if( (prop = cgi_get_prop(opt, "importEnv")) ){
    if(cwal_value_get_bool(prop)){
      initFlags |= CWAL_CGI_INIT2_IMPORT_ENV;
    }
  }
  else{
    char const * envCheck = getenv("CWAL_CGI_IMPORT_ENV");
    long const n = envCheck ? strtol(envCheck, 0, 10) : 0;
    if(n) initFlags |= CWAL_CGI_INIT2_IMPORT_ENV;
  }
  rc = cwal_cgi_init2(cgi, initFlags);
  return rc;
}

/* Helper macro for cwal_callback_f() implementations bound to
   cgi_native instances which ensures that the arguments->self value is
   a cgi_native instance... */
#define THIS_CGI(REQUIRED_INIT_LEVEL)                               \
  cwal_cgi * cgi; cgi_native * self;                                  \
  whcl_engine * const el = whcl_engine_from_args(args);                     \
  assert(el);                                                       \
  self = cgi_native_from_value(args->engine, args->self);           \
  cgi = self ? &self->cgi : NULL;                                   \
  if(!cgi) return cwal_exception_setf(args->engine, CWAL_RC_TYPE,   \
                                      "'this' is not (or is no longer) " \
                                      "a CGI instance."); \
  else if(cgi->inited < REQUIRED_INIT_LEVEL) \
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,              \
                               "'this' CGI has not yet been init()'d.")


static int cgi_cb_urldecode( cwal_callback_args const * args,
                             cwal_value **rv ){
  cwal_size_t slen = 0;
  char const * cstr;
  int rc;
  THIS_CGI(2);
  cstr = (args->argc)
    ? cwal_value_get_cstr(args->argv[0], &slen)
    : NULL;
  if(!cstr) return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                       "Expecting a String argument.");
  rc = cwal_buffer_reserve(args->engine, &cgi->tmpBuf, slen+1);
  if(rc) return rc;
  cgi->tmpBuf.used = 0;
  cwal_buffer_append( args->engine, &cgi->tmpBuf, cstr, slen );
  cwal_cgi_urldecode_inline( (char *)cgi->tmpBuf.mem, &cgi->tmpBuf.used);
  *rv = cwal_new_string_value(args->engine,
                              (char const *)cgi->tmpBuf.mem,
                              cgi->tmpBuf.used);
  cgi->tmpBuf.used = 0;
  return *rv ? 0 : CWAL_RC_OOM;
}

static int cgi_cb_urlencode( cwal_callback_args const * args,
                             cwal_value **rv ){
  cwal_size_t slen = 0;
  char const * cstr;
  int rc;
  cwal_size_t oldPos;
  THIS_CGI(2);
  cstr = (args->argc)
    ? cwal_value_get_cstr(args->argv[0], &slen)
    : NULL;
  if(!cstr) return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                       "Expecting a String argument.");
  else if(!slen){
    /* Optimization: don't bother with an empty string... */
    if(cwal_value_is_string(args->argv[0])) *rv = args->argv[0];
    else *rv = cwal_new_string_value(args->engine, "", 0) /* cannot fail */;
    return 0;
  }
  oldPos = cgi->tmpBuf.used;
  rc = cwal_buffer_reserve(args->engine, &cgi->tmpBuf, oldPos + slen + 1);
  if(rc) return rc;
  rc = cwal_buffer_printf(args->engine, &cgi->tmpBuf, "%t", cstr);
  if(!rc){
    cwal_size_t const len = cgi->tmpBuf.used - oldPos;
    *rv = cwal_new_string_value(args->engine, (char const *)cgi->tmpBuf.mem + oldPos,
                                len);
    if(!*rv) rc = CWAL_RC_OOM;
  }
  cgi->tmpBuf.used = oldPos;
  if(cgi->tmpBuf.mem){
    cgi->tmpBuf.mem[oldPos] = 0;
  }
  return rc;
}

static int cgi_cb_html_escape( cwal_callback_args const * args,
                               cwal_value **rv ){
  cwal_size_t slen = 0;
  char const * cstr;
  int rc;
  cwal_size_t oldPos;
  THIS_CGI(2);
  cstr = (args->argc)
    ? cwal_value_get_cstr(args->argv[0], &slen)
    : NULL;
  if(!cstr) return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                       "Expecting a String argument.");
  else if(!slen){
    /* Optimization: don't bother with an empty string... */
    if(cwal_value_is_string(args->argv[0])) *rv = args->argv[0];
    else *rv = cwal_new_string_value(args->engine, "", 0) /* cannot fail */;
    return 0;
  }
  oldPos = cgi->tmpBuf.used;
  rc = cwal_buffer_reserve(args->engine, &cgi->tmpBuf, oldPos + slen + 1);
  if(rc) return rc;
  rc = cwal_buffer_printf(args->engine, &cgi->tmpBuf, "%h", cstr);
  if(!rc){
    cwal_size_t const len = cgi->tmpBuf.used - oldPos;
    *rv = cwal_new_string_value(args->engine,
                                (char const *)cgi->tmpBuf.mem + oldPos,
                                len);
    if(!*rv) rc = CWAL_RC_OOM;
  }
  cgi->tmpBuf.used = oldPos;
  if(cgi->tmpBuf.mem){
    cgi->tmpBuf.mem[oldPos] = 0;
  }
  return rc;
}

static int cgi_cb_respond( cwal_callback_args const * args,
                           cwal_value **rv ){
  int rc = 0;
  char const doExit = args->argc
    ? cwal_value_get_bool(args->argv[0])
    : 0;
  THIS_CGI(2);
  if(cgi->inited > 1){
    rc = cwal_cgi_response_output_all(cgi);
  }
  cwal_native_clear( cwal_value_get_native(self->vSelf), 1 );
  if(rc){
    rc = cwal_cb_throw(args, rc, "respond() failed with error #%d (%s).",
                     rc, cwal_rc_cstr(rc));
  }
  if(!rc) *rv = cwal_value_undefined();
  if(rc) return rc;
  if(doExit){
    cwal_propagating_set(args->engine, cwal_value_undefined());
    rc = CWAL_RC_EXIT;
  }
  return rc;
}

static int cgi_cb_passthrough( cwal_callback_args const * args,
                               cwal_value **rv ){
  int rc = 0;
  cwal_size_t fnLen = 0;
  char const * fn;
  char const doExit = args->argc>1
    ? cwal_value_get_bool(args->argv[1])
    : 1;
  THIS_CGI(2);
  fn = args->argc
    ? cwal_value_get_cstr(args->argv[0], &fnLen)
    : 0;
  if(!fn || !fnLen){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting a filename argument.");
  }      
  rc = cwal_cgi_response_passthrough(cgi, fn);
  cwal_native_clear( cwal_value_get_native(self->vSelf), 1 );
  if(rc){
    rc = cwal_cb_throw(args, rc, "Passthrough failed with error #%d (%s).",
                     rc, cwal_rc_cstr(rc));
  }else{
    *rv = cwal_value_undefined();
  }
  if(rc) return rc;
  if(doExit){
    cwal_propagating_set(args->engine, cwal_value_undefined());
    rc = CWAL_RC_EXIT;
  }
  return rc;
}


static int cgi_cb_header_set( cwal_callback_args const * args,
                              cwal_value **rv ){
  char const * key;
  cwal_size_t keyLen = 0;
  int rc;
  THIS_CGI(2);
  if(args->argc != 2) goto misuse;
  key = args->argc
    ? cwal_cgi_value_cstr(args->argv[0], &keyLen)
    : NULL;
  if(!key) goto misuse;
  rc = cwal_cgi_response_header_add( cgi, key, keyLen, args->argv[1] );
  if(!rc) *rv = args->self;
  return rc;
  misuse:
  return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                             "Expecting (key,value) arguments.");
}

static int cgi_cb_cookie_set( cwal_callback_args const * args,
                              cwal_value **rv ){
  cwal_value * key = 0;
  int rc;
  cwal_value * v = 0;
  THIS_CGI(2);
  if(!args->argc) goto misuse;
  else if(2==args->argc){
    key = args->argv[0];
    if(!key) goto misuse;
    v = args->argv[1];
  }
#if 0
  else if(cwal_value_is_object(args->argv[0])){
    v = args->argv[0];
  }
#endif
  else goto misuse;
  /* s2_dump_val(key, "cookie key"); */
  /* s2_dump_val(v, "cookie value"); */
  rc = cwal_cgi_cookie_set_v( cgi, key, v );
  /* MARKER(("setCookie rc=%d\n",rc)); */
  if(!rc) *rv = args->self;
  return rc;
  misuse:
  return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                             "Expecting (key,value) or Object arguments.");
}

/**
   setExpires(int unixEpochTime)

   Sets the "Expires" header to the given time.

   If the given time is negative, the Expires value is set to some
   arbitrarily far time in the future.
*/
static int cgi_cb_expires_set( cwal_callback_args const * args,
                              cwal_value **rv ){
  cwal_int_t sec;
  int rc;
  THIS_CGI(2);
  sec = args->argc
    ? cwal_value_get_integer(args->argv[0])
    : 0;
  if(sec<0){
    sec = (cwal_int_t)time(NULL) + (60 * 60 * 24 * 30 /*arbitrary*/);
  }
  rc = cwal_cgi_response_expires(cgi, sec);
  if(!rc) *rv = args->self;
  return rc;
}

static int cgi_cb_rfc7231_time_str( cwal_callback_args const * args,
                                cwal_value **rv ){
  char buf[32] = {0};
  cwal_int_t theTime = args->argc>0
    ? cwal_value_get_integer(args->argv[0])
    : -1;
  if(theTime < 0) theTime = (cwal_int_t)time(NULL);
  cwal_cgi_rfc7231_timedate( (time_t)theTime, buf, sizeof(buf) );
  *rv = cwal_new_string_value(args->engine,
                              buf,
                              cwal_strlen(buf));
  return *rv ? 0 : CWAL_RC_OOM;  
}


/**
   Script usage:

   cgi.httpStatus(404, "Not found")
   cgi.httpStatus(404) // uses default message
   var code = cgi.httpStatus()
   assert typeinfo(isinteger code)
*/
static int cgi_cb_response_status( cwal_callback_args const * args,
                                   cwal_value **rv ){
  char const * msg;
  cwal_size_t nMsg = 0;
  cwal_int_t code;
  int rc;
  THIS_CGI(2);
  if(!args->argc){
    *rv = cwal_new_integer(args->engine,
                           (cwal_int_t)cgi->response.httpCode);
    return *rv ? 0 : CWAL_RC_OOM;
  }
  if((args->argc != 1) && (args->argc != 2)){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting (integer[,string]) arguments.");
  }
  code = cwal_value_get_integer(args->argv[0]);
  msg = (args->argc > 1)
    ? cwal_value_get_cstr(args->argv[1], &nMsg)
    : NULL;
  rc = cwal_cgi_response_status_set( cgi, (int)code, msg );
  if(!rc) *rv = args->self;
  return rc;
}

static int cgi_cb_content_type_set( cwal_callback_args const * args,
                                    cwal_value **rv ){
  int rc;
  THIS_CGI(2);
  if(args->argc != 1) {
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting Value argument.");
  }
  rc = cwal_cgi_response_header_add( cgi, "Content-Type", 12,
                                   args->argv[0] );
  if(!rc) *rv = args->self;
  return rc;
}

/**
   Initalizes the cwal_cgi (args->self) using cwal_cgi_init2(). Throws if
   it's already been initialized. Returns args->self. Accepts an
   optional object with initialization properties, as documented for
   cgi_init_impl().
*/
static int cgi_cb_init( cwal_callback_args const * args,
                        cwal_value **rv ){
  int rc = 0;
  cwal_value * arg = args->argc ? args->argv[0] : 0;
  THIS_CGI(1);
  if(cgi->inited>1){
    rc = cwal_cb_throw(args, CWAL_RC_MISUSE,
                     "Do not init() the CGI module more than once.");
    /* *rv = args->self; we could optionally ignore this case and
       simply return self. */
    return rc;
  }
  rc = cgi_init_impl(cgi, cwal_props_can(arg) ? arg : NULL);

  if(!rc) {
    /* Add cgi.request... */
    cwal_value * v = 0;
    cwal_value * request = cwal_new_object_value(args->engine);
    if(!request){
      rc = CWAL_RC_OOM;
      goto end;
    }
    cwal_value_ref(request);
    rc = cwal_prop_set( args->self, "request", 7, request );
    cwal_value_unref(request);
    if( rc ) goto end;

#define SET(KEY) \
    cwal_value_ref(v); \
    rc = cwal_prop_set( request, KEY, cwal_strlen(KEY), v );   \
    cwal_value_unref(v);                                        \
    v = 0; \
    if(rc) goto end
    
    v = cgi->request.env;
    if(v){
      SET("ENV");
    }
    v = cgi->request.get;
    if(v){
      SET("GET");
    }
    v = cgi->request.post;
    if(v){
      SET("POST");
    }
    v = cgi->request.cookies;
    if(v){
      SET("COOKIES");
    }
  }
#undef SET

  end:
  if(rc){
    if(rc==cgi->err.rc){
      rc = cwal_cb_throw(args, rc,
                       "cwal_cgi_init2() failed with code %d (%s): %.*s",
                       rc, cwal_rc_cstr(rc),
                       (int)cgi->err.msg.used,
                       cgi->err.msg.used
                       ? (char const *)cgi->err.msg.mem
                       : "<no info>");
    }else{
      rc = cwal_cb_throw(args, rc,
                     "cwal_cgi_init2() failed with code %d (%s).",
                     rc, cwal_rc_cstr(rc));
    }
  }else{
    assert(2 == cgi->inited);
    *rv = args->self;
  }
  return rc;
}

static int whcl_module_init_cgi( whcl_engine * const el, cwal_value ** rv ){
  int rc;
  cwal_value * mod;
  cgi_native * cn;
  cwal_cgi * cgi;
  cwal_engine * const e = el->ec;
  static int once = 0;
  if(once || once++){
    return cwal_error_set( e, NULL, CWAL_RC_UNSUPPORTED,
                           "The cgi module necessarily uses/consumes "
                           "global state and cannot reasonably be "
                           "initialized more than once per application "
                           "instance.");
  }
  cn = (cgi_native*)cwal_malloc(e, sizeof(cgi_native));
  if(!cn) return CWAL_RC_OOM;
  *cn = cgi_native_empty;
  mod = cwal_new_native_value(e, cn, cgi_native_finalize,
                              &cgi_native_empty);
  if(!mod){
    cwal_free(e, cn);
    return CWAL_RC_OOM;
  }
  cwal_native_set_rescoper( cwal_value_get_native(mod),
                            cwal_value_rescoper_f_cgi );
  cwal_value_prototype_set( mod, whcl_prototype_object(el) );
  cn->vSelf = mod;
  cgi = &cn->cgi;
  rc = cwal_cgi_init1(e, cgi);
  if(rc){
    rc = cwal_exception_setf(e, rc,
                             "CGI initialization failed%s%.*s",
                             cgi->err.msg.used ? ": " : "",
                             (int)cgi->err.msg.used,
                             cgi->err.msg.used
                             ? (char const *)cgi->err.msg.mem : "");
    goto end;
  }
  assert(1 == cgi->inited);
  assert(cgi->refHolder);

  rc = whcl_install_typename( el, mod, "CGI" );
  if(rc) goto end;

#if 0
  rc = s2_install_ob_2(se, mod, "ob");
  if(rc) goto end;
#endif

  {
    whcl_func_def const funcs[] = {
      /* Install functions... */
      WHCL_FUNC2("urlencode", cgi_cb_urlencode),
      WHCL_FUNC2("urldecode", cgi_cb_urldecode),
      WHCL_FUNC2("set-header", cgi_cb_header_set),
      WHCL_FUNC2("set-expires", cgi_cb_expires_set),
      WHCL_FUNC2("set-cookie", cgi_cb_cookie_set),
      WHCL_FUNC2("set-content-type", cgi_cb_content_type_set),
      WHCL_FUNC2("respond", cgi_cb_respond),
      WHCL_FUNC2("respond-passthrough", cgi_cb_passthrough),
      WHCL_FUNC2("header-timestamp", cgi_cb_rfc7231_time_str),
      //WHCL_FUNC2("operator<<", cgi_cb_print_op),
      WHCL_FUNC2("init", cgi_cb_init),
      WHCL_FUNC2("html-escape", cgi_cb_html_escape),
      WHCL_FUNC2("http-status", cgi_cb_response_status),
      whcl_func_def_empty_m
    };
    rc = whcl_install_functions(el, mod, funcs, 0);
    if(rc) goto end;
  }
  end:
  if(rc){
    MARKER(("cgi init failed with rc=%d/%s\n", rc, cwal_rc_cstr(rc)));
  }else{
    *rv = mod;
  }
  return rc;
}

WHCL_MODULE_REGISTER_(cgi);

#undef THIS_CGI
#undef MARKER
