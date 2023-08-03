/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
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
static const cgi_native cgi_native_empty = {NULL, s2_cgi_empty_m};

static int cwal_value_rescoper_f_cgi( cwal_scope * s, cwal_value * v ){
  cwal_native * n = cwal_value_native_part( s->e, v, &cgi_native_empty );
  cgi_native * cn = (cgi_native *)cwal_native_get( n, &cgi_native_empty );
  s2_cgi * cgi = &cn->cgi;
  assert(n);
  assert(cgi);
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
  /*MARKER(("Finalizing cgi_native@%p (%s@%p)\n", (void *)nat,
    cwal_value_type_name(nat->vSelf), (void*)nat->vSelf));*/
  s2_cgi_cleanup( &nat->cgi );
  cwal_free2( e, v, sizeof(cgi_native) );
}


/**
   A helper function which comes in handy when script code
   subclasses our native or overrides its methods.
*/
cgi_native * cgi_native_from_value( s2_engine * se, cwal_value * v ){
  cgi_native * rc = NULL;
  do{
    cwal_native * n = cwal_value_get_native(v);
    if(n && (rc = cwal_native_get(n, &cgi_native_empty))) break;
    else v = cwal_value_prototype_get(se->e, v);
  }while(v);
  return rc;
}


static cwal_value * cgi_get_prop( cwal_value * c, char const * key ){
  return c
    ? cwal_prop_get(c, key, cwal_strlen(key))
    : 0;
}

/**
   A proxy for s2_cgi_init2(). opt is an optional container
   value which this function inspects for config options:

   importEnv: bool[=false]. If true, the request.ENV bits get imported
   into the cgi object. If not set, getenv("S2_CGI_IMPORT_ENV") is
   examined and if it contains a non-0 number then that ENV import is
   enabled.

   pushObj: bool[=true]. If true (the default), s2_ob_push()
   is called after s2_cgi_init2(). If it's false, it's up to
   the client to push a buffer level before starting.

   Returns non-0 on error but does not set any exception state.
*/
static int cgi_init_impl( s2_cgi * cgi, cwal_value * opt ){
  int initFlags = 0;
  cwal_value * prop;
  int rc = 0;
  /* if(opt) s2_dump_val(opt, "cgi init options"); */
  if(cgi->inited>1) return 0;
  if(!(prop = cgi_get_prop(opt, "pushOb"))/*default is true*/
     || cwal_value_get_bool(prop)){
    initFlags |= S2_CGI_INIT2_PUSH_OB;
  }
  if( (prop = cgi_get_prop(opt, "importEnv")) ){
    if(cwal_value_get_bool(prop)){
      initFlags |= S2_CGI_INIT2_IMPORT_ENV;
    }
  }
  else{
    char const * envCheck = getenv("S2_CGI_IMPORT_ENV");
    long const n = envCheck ? strtol(envCheck, 0, 10) : 0;
    if(n) initFlags |= S2_CGI_INIT2_IMPORT_ENV;
  }
  rc = s2_cgi_init2(cgi, initFlags);
  return rc;
}

/* Helper macro for cwal_callback_f() implementations bound to
   cgi_native instances which ensures that the arguments->self value is
   a cgi_native instance... */
#define THIS_CGI(REQUIRED_INIT_LEVEL)                               \
  s2_cgi * cgi; cgi_native * self;                                  \
  s2_engine * se = s2_engine_from_args(args);                        \
  assert(se);                                                       \
  self = cgi_native_from_value(se, args->self);                     \
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
  s2_cgi_urldecode_inline( (char *)cgi->tmpBuf.mem, &cgi->tmpBuf.used);
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

/**
   Internal print() impl, stolen from s2_cb_write() implementation.
*/
static int cgi_cb_print_impl( cwal_callback_args const * args, cwal_value **rv,
                              uint16_t skipArgs,
                              char addSpace ){
  uint16_t i;
  int rc = 0;
  char const * sep = " ";
  cwal_engine * e = args->engine;
  cwal_size_t const sepLen = cwal_strlen(sep);
  {
    /* If there's no output buffer yet, add one. */
    THIS_CGI(2);
    if(s2_ob_level(se) <= cgi->originalObLevel){
      rc = s2_ob_push(se);
      if(rc) return rc;
    }
  }
  for(i = skipArgs; !rc && (i < args->argc); ++i ){
    cwal_value * v = args->argv[i];
    if(addSpace && i) rc = cwal_output(e, sep, sepLen);
    if(rc) break;
    /* s2_dump_val(v,"arg"); */
    switch(cwal_value_type_id(v)){
      case CWAL_TYPE_INTEGER:
      case CWAL_TYPE_DOUBLE:
      case CWAL_TYPE_BOOL:
      case CWAL_TYPE_OBJECT:
      case CWAL_TYPE_ARRAY:
      case CWAL_TYPE_TUPLE:
      case CWAL_TYPE_NULL:
      case CWAL_TYPE_EXCEPTION:
        rc = cwal_json_output_engine( e, v, NULL );
        break;
      case CWAL_TYPE_UNDEF:
        rc = cwal_output(e, "undefined", 9);
        break;
      case CWAL_TYPE_STRING:{
        cwal_size_t slen = 0;
        char const * cstr = cwal_value_get_cstr(v, &slen);
        rc = slen ? cwal_output(e, cstr, slen) : 0;
        break;
      }
      case CWAL_TYPE_BUFFER:{
        cwal_buffer const * vb = cwal_value_get_buffer(v);
        rc = vb->used ? cwal_output(e, vb->mem, vb->used) : 0;
        break;
      }
      case CWAL_TYPE_HASH:
      case CWAL_TYPE_FUNCTION:
      case CWAL_TYPE_NATIVE:
        rc = cwal_outputf(e, "%s@%p", cwal_value_type_name(v), (void const*)v);
        break;
      default:
        break;
    }
  }
  if(!rc && addSpace){
    rc = cwal_output(args->engine, "\n", 1);
  }
  if(rc && (CWAL_RC_EXCEPTION!=rc)){
    rc = cwal_cb_throw(args, rc, "Output error #%d (%s).",
                     rc, cwal_rc_cstr(rc));
  }
  *rv = args->self;
  cwal_output_flush(args->engine);
  return rc;
}

static int cgi_cb_print_op( cwal_callback_args const * args,
                            cwal_value **rv ){
  assert(1 == args->argc && "Expecting operator call semantics.");
  return cgi_cb_print_impl(args, rv, 0, 0);
}


static int cgi_cb_respond( cwal_callback_args const * args,
                           cwal_value **rv ){
  int rc = 0;
  char const doExit = args->argc
    ? cwal_value_get_bool(args->argv[0])
    : 0;
  THIS_CGI(2);
  if(cgi->inited > 1){
    rc = s2_cgi_response_output_all(cgi);
  }
  cwal_native_clear( cwal_value_get_native(self->vSelf), 1 );
  if(rc){
    rc = cwal_cb_throw(args, rc, "respond() failed with error #%d (%s).",
                     rc, cwal_rc_cstr(rc));
  }
  if(!rc) *rv = cwal_value_undefined();
  return rc
    ? rc
    : (doExit ? s2_trigger_exit(args, 0) : 0);
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
  rc = s2_cgi_response_passthrough(cgi, fn);
  cwal_native_clear( cwal_value_get_native(self->vSelf), 1 );
  if(rc){
    rc = cwal_cb_throw(args, rc, "Passthrough failed with error #%d (%s).",
                     rc, cwal_rc_cstr(rc));
  }else{
    *rv = cwal_value_undefined();
  }
  return rc
    ? rc
    : (doExit ? s2_trigger_exit(args, 0) : 0);
}


static int cgi_cb_header_set( cwal_callback_args const * args,
                              cwal_value **rv ){
  char const * key;
  cwal_size_t keyLen = 0;
  int rc;
  THIS_CGI(2);
  if(args->argc != 2) goto misuse;
  key = args->argc
    ? s2_value_cstr(args->argv[0], &keyLen)
    : NULL;
  if(!key) goto misuse;
  rc = s2_cgi_response_header_add( cgi, key, keyLen, args->argv[1] );
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
  rc = s2_cgi_cookie_set_v( cgi, key, v );
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
  rc = s2_cgi_response_expires(cgi, sec);
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
  s2_cgi_rfc7231_timedate( (time_t)theTime, buf, sizeof(buf) );
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
  rc = s2_cgi_response_status_set( cgi, (int)code, msg );
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
  rc = s2_cgi_response_header_add( cgi, "Content-Type", 12,
                                   args->argv[0] );
  if(!rc) *rv = args->self;
  return rc;
}

/**
   Initalizes the s2_cgi (args->self) using s2_cgi_init2(). Throws if
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
    cwal_value * request = cwal_new_object_value(se->e);
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
                       "s2_cgi_init2() failed with code %d (%s): %.*s",
                       rc, cwal_rc_cstr(rc),
                       (int)cgi->err.msg.used,
                       cgi->err.msg.used
                       ? (char const *)cgi->err.msg.mem
                       : "<no info>");
    }else{
      rc = cwal_cb_throw(args, rc,
                     "s2_cgi_init2() failed with code %d (%s).",
                     rc, cwal_rc_cstr(rc));
    }
  }else{
    assert(2 == cgi->inited);
    *rv = args->self;
  }
  return rc;
}

static int s2_module_init_cgi( s2_engine * se, cwal_value ** rv ){
  int rc;
  cwal_value * v;
  cwal_value * mod;
  cgi_native * cn;
  s2_cgi * cgi;
  static int once = 0;
  if(once || once++){
    return s2_engine_err_set( se, CWAL_RC_UNSUPPORTED,
                              "The cgi module necessarily uses/consumes "
                              "global state and cannot reasonably be "
                              "initialized more than once per application "
                              "instance.");
  }

  cn = (cgi_native*)cwal_malloc(se->e, sizeof(cgi_native));
  if(!cn) return CWAL_RC_OOM;
  *cn = cgi_native_empty;
  mod = cwal_new_native_value(se->e, cn, cgi_native_finalize,
                              &cgi_native_empty);
  if(!mod){
    cwal_free(se->e, cn);
    return CWAL_RC_OOM;
  }
  cwal_native_set_rescoper( cwal_value_get_native(mod),
                            cwal_value_rescoper_f_cgi );
  cwal_value_prototype_set( mod, s2_prototype_object(se) );
  cn->vSelf = mod;
  cgi = &cn->cgi;
  rc = s2_cgi_init1(se, cgi);
  if(rc){
    rc = cwal_exception_setf(se->e, rc,
                             "CGI initialization failed%s%.*s",
                             cgi->err.msg.used ? ": " : "",
                             (int)cgi->err.msg.used,
                             cgi->err.msg.used
                             ? (char const *)cgi->err.msg.mem : "");
    goto end;
  }
  assert(1 == cgi->inited);
  assert(cgi->refHolder);

  v = cwal_new_string_value(se->e, "CGI", 3);
  if(!v) { rc = CWAL_RC_OOM; goto end; }
  cwal_value_ref(v);
  rc = s2_typename_set_v( se, mod, v );
  cwal_value_unref(v);
  v = 0;                                                  \
  if(rc) goto end;

  rc = s2_install_ob_2(se, mod, "ob");
  if(rc) goto end;

  {
    s2_func_def const funcs[] = {
      /* Install functions... */
      S2_FUNC2("urlencode", cgi_cb_urlencode),
      S2_FUNC2("urldecode", cgi_cb_urldecode),
      S2_FUNC2("setHeader", cgi_cb_header_set),
      S2_FUNC2("setExpires", cgi_cb_expires_set),
      S2_FUNC2("setCookie", cgi_cb_cookie_set),
      S2_FUNC2("setContentType", cgi_cb_content_type_set),
      S2_FUNC2("respond", cgi_cb_respond),
      S2_FUNC2("respondPassthrough", cgi_cb_passthrough),
      S2_FUNC2("headerTimestamp", cgi_cb_rfc7231_time_str),
      S2_FUNC2("operator<<", cgi_cb_print_op),
      S2_FUNC2("init", cgi_cb_init),
      S2_FUNC2("htmlEscape", cgi_cb_html_escape),
      S2_FUNC2("httpStatus", cgi_cb_response_status),
      s2_func_def_empty_m
    };
    rc = s2_install_functions(se, mod, funcs, 0);
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

S2_MODULE_REGISTER_(cgi);

#undef THIS_CGI
#undef MARKER
