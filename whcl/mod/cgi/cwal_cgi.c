/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#include "libwhcl.h"
#include "cwal_cgi.h"
#include <assert.h>
#include <ctype.h> /* isxdigit() */
#include <string.h> /* strlen() */
#include <stdlib.h> /*getenv() and friends */

/**
   Only for debuggering...
*/
#include <stdio.h>
#define MARKER(pfexp)                                               \
  do{ printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__);   \
    printf pfexp;                                                   \
  } while(0)

const cwal_cgi cwal_cgi_empty = cwal_cgi_empty_m;

/**
   Some cson_cgi-internal value keys.
*/
static const struct {
  char const * ENV_GET;
  char const * ENV_POST;
  char const * ENV_COOKIE;
  char const * ENV_SYS;
  char const * ENV_APP;
  char const * ENV_ARGV;
  char const * ENV_CONFIG;
  char const * ENV_SESSION;
  char const * RESPONSE_HEADERS;
  char const * RESPONSE_COOKIES;
} cwal_cgi_keys = {
"request.GET",
"request.POST",
"request.COOKIES",
"request.ENV",
"env.APP",
"env.ARGV",
"env.CONFIG",
"SESSION",
"response.headers",
"response.cookies"
};


/**
   If PATH_INFO is set, this function splits it on '/'
   characters and creates an array out of the elements.
   The array is stored as $ENV["PATH_INFO_SPLIT"].

   Returns non-0 on error. If PATH_INFO is not set,
   0 is returned. If it is set but has no entries,
   an empty array is created.

   A return value of CWAL_RC_RANGE probably means that a path element
   was longer than our internal buffer size, in which case processing
   ends and PATH_INFO_SPLIT is not set. That error can probably be
   ignored by the caller, but all others are probably serious
   (e.g. CWAL_RC_OOM).
*/
static int cwal_cgi_import_path_info(cwal_cgi *cx)
{
  char const * head =
    /* cwal_cgi_getenv_cstr(cx, "e", "PATH_INFO", 9, NULL) */
    getenv("PATH_INFO")
    ;
  if( NULL == head ) return 0;
  else{
    cwal_value * env = cwal_cgi_env_get(cx, 'e', 1);
    cwal_array * ar = env ? cwal_new_array(cx->e) : NULL;
    char const * tail = NULL;
    if( !ar ) return CWAL_RC_OOM;
    else{
      enum { BufSize = 256 };
      char buf[BufSize];
      cwal_value * partV;
      cwal_size_t slen;
      int rc = 0;
      while( cwal_strtok( &head, '/', &tail ) ){
        slen = (tail-head);
        if( slen >= BufSize ){
          rc = CWAL_RC_RANGE;
          goto end_clean;
        }
        memcpy( buf, head, slen );
        buf[slen] = 0;
        cwal_cgi_urldecode_inline( buf, &slen );
        partV = cwal_new_string_value( cx->e, buf, slen );
        if( ! partV ){
          rc = CWAL_RC_OOM;
          goto end_clean;
        }
        rc = cwal_array_append( ar, partV );
        if( rc ){
          cwal_value_unref( partV );
          goto end_clean;
        }
        partV = NULL;
        head = tail;
        tail = NULL;
      }
      assert( 0 == rc );
      rc = cwal_prop_set( env, "PATH_INFO_SPLIT", 15,
                          cwal_array_value(ar) );
      end_clean:
      if( rc ){
        cwal_array_unref( ar );
      }
      return rc;
    }
  }
}

/**
   Imports environment variables into into cx->request.env,
   initializing cx->request.env if needed. If called multiple times
   the environment is re-read each time, but old entries which are no
   longer in the new environment are not removed from cx->request.env.

   If fullEnv is true, (extern char ** environ) is imported, otherwise
   only a core set of environment vars specified for use by CGI
   scripts are imported.

   Returns 0 on success.
*/
static int cwal_cgi_import_environ(cwal_cgi * cx, bool fullEnv)
{
  char const * baseEnv[] = {
  "AUTH_CONTENT",
  "AUTH_TYPE",
  "CONTENT_LENGTH",
  "CONTENT_TYPE",
  "DOCUMENT_ROOT",
  "HTTPS",
  "HTTP_ACCEPT",
  "HTTP_ACCEPT_ENCODING",
  "HTTP_COOKIE",
  "HTTP_HOST",
  "HTTP_IF_MODIFIED_SINCE",
  "HTTP_IF_NONE_MATCH",
  "HTTP_REFERER",
  "HTTP_SCHEME",
  "HTTP_USER_AGENT",
  "PATH",
  "PATH_INFO",
  "QUERY_STRING",
  "REMOTE_ADDR",
  "REMOTE_USER",
  "REQUEST_METHOD",
  "REQUEST_URI",
  "SCGI",
  "SCRIPT_DIRECTORY",
  "SCRIPT_FILENAME",
  "SCRIPT_NAME",
  "SERVER_NAME",
  "SERVER_PORT",
  "SERVER_PROTOCOL",
  "SERVER_SOFTWARE",
  NULL
  };
  extern char const * const * environ;
  int i = 0;
  char const * e;
  char const * v = NULL;
  enum { KeyBufSize = 512 };
  char keybuf[KeyBufSize];
  char * kpos = NULL;
  cwal_size_t vLen;
  int rc = 0;
  cwal_value * jv = NULL;
  char const * const * whichEnv = fullEnv ? environ : baseEnv;
  cwal_value * env = cwal_cgi_env_get( cx, 'e', 1 );
  if(!env) return CWAL_RC_OOM;
  for( e = whichEnv[0]; e && *e; e = whichEnv[++i] ){
    v = NULL;
    if(fullEnv){
      kpos = keybuf;
      for( ; *e && ('=' != *e); ++e ){
        *(kpos++) = *e;
        assert( kpos < (keybuf+KeyBufSize) );
        if( kpos == (keybuf+KeyBufSize) ){
          return CWAL_RC_RANGE;
        }
      }
      *kpos = 0;
      if( '=' == *e ){
        v = e+1;
        vLen = strlen(v);
      }
      else{
        v = "";
        vLen = 0;
      }
    }else{
      v = getenv(e);
      if(v) vLen = strlen(v);
      else{
        v = "";
        vLen = 0;
      }
      snprintf(keybuf, KeyBufSize, "%s", e);
    }
    /* TODO? URLdecode? We could do that using
       cwal_printf()'s %T. */
    jv = cwal_new_string_value( cx->e, v, vLen );
    if(!jv){
      rc = CWAL_RC_OOM;
      break;
    }
    cwal_ref(jv);
    rc = cwal_prop_set( env, keybuf, cwal_strlen(keybuf), jv );
    cwal_unref(jv);
    if( 0 != rc ) break;
  }
  if( !rc ){
    rc = cwal_cgi_import_path_info(cx);
  }
  return rc;
}

/**
   Parses inp as a delimited list, separated by the given
   separator character. Each item in the list is treated
   as a key/value pair in the form KEY=VALUE, and inserted
   into the target cson_object (which must not be NULL).

   This is intended for parsing HTTP GET-style parameter lists.

   If doUrlDecode is true (non-zero) then the VALUE part of the
   key/value pair gets url-decoded before insertion. (FIXME? Also
   decode the keys?)

   If firstOneWins is non-0 then if a given key in the parameters is
   duplicated, entries after the first are ignored. If it is 0 then
   the "last one wins." This is basically a workaround for when we
   have multiple session ID cookies hanging around :/.
   
   On success it returns 0.

   If a given key contains the string "[]", that part is stripped and
   the entry is treated like an array element. e.g. a query string of
   "a[]=3&a[]=7" would result in an array property named "a" with the
   (string) entries ("3", "7").
   
*/
static int cwal_cgi_parse_param_list( cwal_cgi * cx,
                                    cwal_value * tgt,
                                    char const * inp,
                                    char separator,
                                    char doUrlDecode,
                                    char firstOneWins)
{
  if( ! tgt || !separator ) return CWAL_RC_MISUSE;
  else if( !inp || !*inp ) return 0;
  else{
    char const * head = inp;
    char const * tail = NULL;
    char * out = NULL;
    unsigned int inLen = strlen( inp );
    cwal_size_t valLen;
    cwal_value * jval = NULL;
    cwal_value * listV = NULL;
    cwal_array * list = NULL;
    cwal_size_t keyLen;
    cwal_size_t const oldUsed = cx->tmpBuf.used;
    int rc = cwal_buffer_reserve( cx->e, &cx->tmpBuf, inLen+1 );
    if( 0 != rc ) return rc;
    while( cwal_strtok( &head, separator, &tail ) )
    {
      char const * key = head;
      char * value = NULL;
      bool hadKeyEq = 0;
      bool looksLikeArray = false;
      rc = 0;
      if( head == tail ) break;
      out = (char *)cx->tmpBuf.mem;
      memset( cx->tmpBuf.mem, 0, cx->tmpBuf.capacity );
      for( ; (key<tail) && *key && isspace((int)*key); ++key ){
        /* strip leading spaces in the key name
           (happens in cookie values). */
      }
      if( key==tail ) break;
      else if( '='==*key ){
        /* all-space key. Just skip it. */
        goto next_iter;
      }
      /* Write the key part to the buffer... */
      for( ; (key<tail) && *key && ('='!=*key); ++key ) {
        *(out++) = *key;
      }
      *(out++) = 0;
      if( '=' == *key ){
        ++key;
        hadKeyEq = 1;
      }
      value = out;
      valLen = 0;
      /* Write the value part to the buffer... */
      for( ; (key<tail) && *key; ++key, ++valLen ) {
        *(out++) = *key;
      }
      assert(0==*out);
      key = (char const *)cx->tmpBuf.mem;
      keyLen = strlen(key);
      if( firstOneWins && cwal_prop_get( tgt, key, keyLen)){
        goto next_iter;
      }
      if( doUrlDecode && valLen ){
        cwal_cgi_urldecode_inline( value, &valLen );
      }
      /*MARKER("key=[%s], valLen=%u, value=[%s]\n", key, valLen, value );*/
      /**
         If the key is the only token (no '=' and no value) treat it
         as a boolean true, rather than an empty-string value.

         As of 20181107, we use cwal_value_from_arg() to do the other
         conversions.
      */
      if((out = strstr(key,"[]"))){
        /* Strip the [] part from the key. */
        looksLikeArray = true;
        keyLen = (cwal_size_t)(out - key);
        if(0==keyLen){
          /* Curious. Skip it. */
          goto next_iter;
        }
      }
      jval = (!hadKeyEq && !valLen)
        ? cwal_value_true()
        : cwal_value_from_arg(cx->e, value);
      if( NULL == jval ){
        rc = CWAL_RC_OOM;
        goto the_end;
      }
      cwal_value_ref(jval);
      if( looksLikeArray ){
        /* Treat key as an array entry, like PHP does... */
        *out = 0;
        list = NULL;
        listV = cwal_prop_get( tgt, key, keyLen );
        if( listV ){
          if( ! cwal_value_is_array( listV ) ){
            cwal_value_unref( jval );
            jval = NULL;
            goto next_iter;
          }
        }
        else{ /* create a new array to hold the value */
          listV = cwal_new_array_value(cx->e);
          if( ! listV ){
            cwal_value_unref( jval );
            rc = CWAL_RC_OOM;
            goto the_end;
          }
          cwal_value_ref(listV);
          rc = cwal_prop_set( tgt, key, keyLen, listV );
          cwal_value_unref(listV);
          if( 0 != rc ){
            cwal_value_unref( jval );
            goto the_end;
          }
        }
        list = cwal_value_get_array( listV );
        rc = cwal_array_append( list, jval );
        cwal_value_unref(jval);
        jval = 0;
        if( 0 != rc ){
          goto the_end;
        }
      }
      else{
        rc = cwal_prop_set( tgt, key, keyLen, jval );
        cwal_value_unref( jval );
        jval = 0;
        if( 0 != rc ){
          goto the_end;
        }
      }
      next_iter:
      head = tail;
      tail = NULL;
    }
    the_end:
    cx->tmpBuf.used = oldUsed;
    return rc;
  }
}

/**
   Parses key/value pairs from a QUERY_STRING-formatted
   string.

   Returns 0 on success. The "most likely" error condition, in terms
   of potential code paths, is is an allocation error.
   
   TODO: if the key part of any entry ends with "[]", treat it as an
   array entry, like PHP does.
*/
static int cwal_cgi_parse_query_string( cwal_cgi * cx, char const * qstr )
{
  cwal_value * env = NULL;
  if( !qstr || !*qstr ) return 0;
  assert(cx);
  env = cwal_cgi_env_get( cx, 'g', 1 );
  return env
    ? cwal_cgi_parse_param_list( cx, env, qstr, '&', 1, 0 )
    : CWAL_RC_OOM;
}

/**
   Like cwal_cgi_parse_query_string(), but expects qstr to be in COOKIE
   format.
*/
static int cwal_cgi_parse_cookies( cwal_cgi * cx, char const * qstr ){
  cwal_value * env = NULL;
  if( !qstr || !*qstr ) return 0;
  assert(cx);
  env = cwal_cgi_env_get(cx, 'c', 1 );
  return env
    ? cwal_cgi_parse_param_list( cx, env, qstr, ';', 1, 1 )
    : CWAL_RC_OOM /* guess! */;
}

typedef struct CgiPostReadState_ {
  FILE * fh;
  cwal_size_t len;
  cwal_size_t pos;
} CgiPostReadState;

static int cwal_input_FILE_n( void * state,
                              void * dest,
                              cwal_size_t * n ){
  if( ! state || !dest || !n ) return CWAL_RC_MISUSE;
  else{
    CgiPostReadState * st = (CgiPostReadState *)state;
    if( st->pos >= st->len ){
      *n = 0;
      return 0;
    }
    else if( !*n || ((st->pos + *n) > st->len) ){
      return CWAL_RC_RANGE;
    }
    else{
      unsigned int rsz = (unsigned int)fread( dest, 1, *n, st->fh );
      if( ! rsz ){
        *n = (cwal_size_t)rsz;
        return feof(st->fh) ? 0 : CWAL_RC_IO;
      }
      else{
        *n = (cwal_size_t)rsz;
        st->pos += *n;
        return 0;
      }
    }
  }
}

static int cwal_cgi_err_set( cwal_cgi * cx, int code,
                           char const * msg, ... ){
  assert(0 != code);
  cx->err.msg.used = 0;
  switch(code){
    case CWAL_RC_OOM: break;
    default:
      if(msg && *msg){
        va_list args;
        va_start(args,msg);
        cwal_buffer_printfv( cx->e, &cx->err.msg, msg, args );
        va_end(args);
      }
  }
  return cx->err.rc = code;
}

static int cwal_cgi_stash_set( cwal_cgi * cx, char const * key,
                             cwal_value * v ){
  return cwal_prop_set(cx->refHolder, key, cwal_strlen(key), v);
}

#if 0
static cwal_value * cwal_cgi_stash_get( cwal_cgi * cx, char const * key ){
  return cwal_prop_get(cx->refHolder, key, cwal_strlen(key));
}
#endif

#define IGNORE_CONTENT_LENGTH_HEADER 0

static int cwal_cgi_parse_POST_JSON(cwal_cgi * cx, FILE * src,
                                  unsigned int contentLen){
  cwal_value * jv = NULL;
  int rc = 0;
  CgiPostReadState state;
  cwal_json_parse_info pinfo = cwal_json_parse_info_empty;
  assert( 0 != contentLen );
  assert( NULL == cx->request.post );
  state.fh = src;
  state.len = IGNORE_CONTENT_LENGTH_HEADER
    ? (cwal_size_t)-1
    : contentLen;
  state.pos = 0;
  rc = cwal_json_parse( cx->e, cwal_input_FILE_n, &state, &jv, &pinfo );
  if( rc ){
    assert(!jv);
    return cwal_cgi_err_set(cx, rc,
                          "Parsing POST as JSON failed: "
                          "code=%d (%s) "
                          "line=%u, col=%u\n",
                          rc, cwal_rc_cstr(rc),
                          (unsigned)pinfo.line,
                          (unsigned)pinfo.col);
    return rc;
  }
  /* cwal_value_rescope( cx->topScope, jv ); */
  cwal_value_ref(jv);
  rc = cwal_cgi_stash_set(cx, cwal_cgi_keys.ENV_POST, jv)
    /* If that fails, jv is still owned by the current scope, else it's
       owned/ref'd by cx. */;
  cwal_value_unref(jv);
  if(!rc){
      cx->request.post = jv;
  }
  return rc;
}

#define CWAL_CGI_ENABLE_POST_FORM_URLENCODED 1
#if CWAL_CGI_ENABLE_POST_FORM_URLENCODED
static int cwal_cgi_parse_post_urlencoded( cwal_cgi * cx, char const * qstr )
{
  cwal_value * env = NULL;
  if( !qstr || !*qstr ) return 0;
  assert(cx);
  env = cwal_cgi_env_get( cx, 'p', 1 );
  return env
    ? cwal_cgi_parse_param_list( cx, env, qstr, '&', 1, 0 )
    : CWAL_RC_OOM;
}
#endif

static int cwal_cgi_init_POST(cwal_cgi * cx){
  if( ! cx || !cx->request.input ) return CWAL_RC_MISUSE;
  else{
    FILE * src = cx->request.input;
        
    char const * ctype =
      /*cwal_cgi_getenv_cstr( cx, "e",
                          "CONTENT_TYPE", 12,
                          NULL )*/
      getenv("CONTENT_TYPE")
      ;
    if( !src || NULL == ctype ) return 0;
    else{
      char const * clen =
        /*cwal_cgi_getenv_cstr( cx, "e",
                            "CONTENT_LENGTH", 14,
                            NULL )*/
        getenv("CONTENT_LENGTH");
#if !IGNORE_CONTENT_LENGTH_HEADER
      if( NULL == clen ){
        return cwal_cgi_err_set(cx, CWAL_RC_RANGE,
                              "CONTENT_LENGTH not specified.");
      }
#endif
#if CWAL_CGI_ENABLE_POST_FORM_URLENCODED
      else if( 0 == strncmp(ctype,"application/x-www-form-urlencoded",33) ){
        cwal_buffer buf = cwal_buffer_empty;
        int rc = cwal_buffer_fill_from_FILE( cx->e, &buf, src );
        if( rc ) goto end_clean;
        if( buf.mem && buf.used ){
#if 1
          if( strlen((char const *)buf.mem)
              != buf.used ){
            /* assume bad/malicious input. */
            rc = CWAL_RC_RANGE;
            rc = cwal_cgi_err_set(cx, CWAL_RC_RANGE,
                                "Not allowing binary POST input.");
            goto end_clean;
          }
#endif
          rc = cwal_cgi_parse_post_urlencoded( cx,
                                             (char const *)buf.mem );
        }
        end_clean:
        cwal_buffer_reserve( cx->e, &buf, 0 );
        return rc;
      }
#endif
      else{
        char * endpt = NULL;
#if !IGNORE_CONTENT_LENGTH_HEADER
        long len = strtol( clen, &endpt, 10 );
        if( (endpt && *endpt) || (len<=0) ) return CWAL_RC_RANGE;
        else
#endif
        if( (0 == strncmp(ctype,"application/json",16))
            || (0 == strncmp(ctype,"text/plain",10))
            || (0 == strncmp(ctype,"application/javascript",22))
            )
        {
          return cwal_cgi_parse_POST_JSON(cx, src, len);
        }
        else{
          return cwal_cgi_err_set(cx, CWAL_RC_TYPE,
                                "Don't know how to handle "
                                "Content-type [%s]\n", ctype);
        }
      }
    }
  }
}

cwal_value * cgi__value_unwrap( cwal_value * v ){
  return cwal_value_is_unique(v)
    ? cwal_unique_wrapped_get(v)
    : v;
}

cwal_value const * cgi__value_unwrap_c( cwal_value const * v ){
  return cwal_value_is_unique(v)
    ? cwal_unique_wrapped_get(v)
    : v;
}


/**
   If orig is one of the types (string,double,bool,undef,null) then
   this routine stringifies it and replaces dest's contents with that
   string. If orig is of type "unique" then it is assumed to be an s2
   enum entry value and its wrapped value is used in place of the orig
   value. For other cases (or on allocation error), NULL is returned.

   On success a pointer to a string is returned. It will be one of:

   - if orig is-a string then a copy of its underlying string (via
   dest->mem).

   - for (double,integer,bool,undef,null), dest->mem will be
   returned. The encoded form is decimal for (double,integer), the
   number 0 or 1 for bool, and the number 0 for (undef,null).

   Ownership of dest is not modified by this call.

   The returned value is valid until either orig or dest are modified.

   On error dest is not modified. Dest is also not modified if orig
   is-a string, as its own string bytes are returned instead.
*/
static char const * cwal_cgi_pod_to_string( cwal_cgi * cx,
                                            cwal_value const * orig,
                                            cwal_buffer * dest )
{
  if( !cx || !orig || !dest ) return NULL;
  else
  {/* FIXME? use cson's output support for the numeric types. i
      _think_ those bits might not be in the public API, though.
      We could use it for serializing objects/arrays, in any case.
   */
    enum { NumBufSize = 80 };
    int rc = 0;
    cwal_size_t slen = 0;
    char const * cstr;
    dest->used = 0;
    orig = cgi__value_unwrap_c(orig);
    cstr = cwal_value_get_cstr(orig, &slen);
    if(cstr){
      if( (rc=cwal_buffer_append(cx->e, dest, cstr, slen)) ){
        cwal_cgi_err_set(cx, rc, NULL);
        return NULL;
      }
      return (char *)dest->mem;
    }
    else if( cwal_value_is_integer(orig) ){
      if( (rc=cwal_buffer_printf( cx->e, dest,
                                  "%"CWAL_INT_T_PFMT,
                                  (cwal_int_t)cwal_value_get_integer(orig)))){
        cwal_cgi_err_set(cx, rc, NULL);
        return NULL;
      }
      return (char *)dest->mem;
    }
    else if( cwal_value_is_double(orig) ){
      if((rc=cwal_buffer_printf( cx->e, dest,
                                 "%"CWAL_DOUBLE_T_PFMT,
                                 (cwal_double_t)cwal_value_get_double(orig)))){
        cwal_cgi_err_set(cx, rc, NULL);
        return NULL;
      }
      return (char *)dest->mem;
    }
    else if( cwal_value_is_bool( orig ) ){
      char const bv = cwal_value_get_bool(orig);
      if( (rc=cwal_buffer_append( cx->e, dest, bv ? "1": "0", 1)) ){
        cwal_cgi_err_set(cx, rc, NULL);
        return NULL;
      }
      return (char *)dest->mem;
    }
    else if( cwal_value_is_null( orig )
             || cwal_value_is_undef( orig ) ){
      if( (rc=cwal_buffer_append( cx->e, dest, "0", 1)) ){
        cwal_cgi_err_set(cx, rc, NULL);
        return NULL;
      }
      return (char *)dest->mem;
    }
    else{
      return NULL;
    }
  }
}

static int cwal_cgi_kvp_visitor_headers( cwal_kvp const * kvp,
                                       void * state ){
  cwal_cgi * cx = (cwal_cgi*)state;
  char const * key;
  cwal_value const * val;
  char const * valcstr;
  cwal_size_t keyLen = 0;
  cx->tmpBuf.used = 0;
  key = cwal_value_get_cstr(cwal_kvp_key(kvp), &keyLen);
  val = cwal_kvp_value(kvp);
  valcstr = cwal_cgi_pod_to_string( cx, val, &cx->tmpBuf );
  if( ! valcstr ) return 0;
  else{
    int const rc = cwal_outputf(cx->e, "%.*s: %.*s\r\n",
                                (int)keyLen, key,
                                (int)cx->tmpBuf.used, valcstr);
    cx->tmpBuf.used = 0;
    return rc;
  }
}

char * cwal_cgi_rfc7231_timedate( time_t now, char * dest, unsigned int destLen )
{
  static const char * dayNames[] = 
    {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat",
     0 };
  static const char * monthNames[] =
    {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
     0};
  struct tm * t = (dest && (destLen>30)) ? gmtime(&now) : NULL;
  if( ! t || (destLen<31) ) return NULL;
  else
  {
    int const rc = sprintf( dest,
                            "%s, %d %s %02d %02d:%02d:%02d GMT",
                            dayNames[t->tm_wday], t->tm_mday,
                            monthNames[t->tm_mon],
                            t->tm_year+1900, t->tm_hour,
                            t->tm_min, t->tm_sec
                            );
    assert( (rc>0) && ((unsigned int)rc) < destLen );
    if(rc){/*avoid unused var warning in non-debug builds */}
    return dest;
  }
}

static int cwal_cgi_kvp_visitor_cookies( cwal_kvp const * kvp,
                                       void * state ){
  enum { TSBufSize = 32 };
  char tsBuf[TSBufSize] = {0} /* buffer for expiry timestamp */;
  cwal_cgi * cx = (cwal_cgi*)state;
  int rc;
  char const * key;
  cwal_size_t keyLen = 0;
  cwal_value * val;
  key = cwal_value_get_cstr(cwal_kvp_key(kvp), &keyLen);
  if(!key || !*key) return 0 /* ignore non-string keys */;
  val = cwal_kvp_value(kvp);
  if( cwal_value_is_null( val )
      || cwal_value_is_undef( val ) ){
    cwal_outputf(cx->e,"Set-Cookie: %.*s=; Expires=Thu, "
                 "01-Jan-1970 00:00:01 GMT\r\n",
                 (int)keyLen, key);
    return 0;
  }
  else if( cwal_value_is_object(val) ){
    /*
      Accept in Object in the form:
          
      {
      value: VALUE,
      domain: string,
      path: string,
      secure: bool,
      httpOnly: bool,
      sameSite: string,
      expires: integer (UNIX epoch)
      }
    */
    cwal_value * cv = cwal_prop_get( val, "value", 5 );
    char const * valstr = NULL;
    char const valueIsNull = !cv
      || cwal_value_is_null( cv )
      || cwal_value_is_undef( cv )
      /* is valueIsNull, unset the cookie */;
    /* s2_dump_val(val,"cookie object"); */
    /* s2_dump_val(cv,"cookie value"); */
    if( valueIsNull ){
      cwal_outputf(cx->e, "Set-Cookie: %.*s=; Expires=Thu, 01 Jan 1970 00:00:01 GMT", (int)keyLen, key);
    }
    else{
      valstr = cwal_cgi_pod_to_string( cx, cv, &cx->tmpBuf );
      if( ! valstr ) return 0;
      else {
        rc = cwal_cgi_urlencode( cx, valstr, cx->tmpBuf.used,
                               &cx->tmpBuf2 );
        if(rc) return rc;
        cwal_outputf(cx->e, "Set-Cookie: %.*s=%.*s",
                     (int)keyLen, key, (int)cx->tmpBuf2.used,
                     (char const *)cx->tmpBuf2.mem);
        cx->tmpBuf2.used = 0;
      }
    }

#define DOPART(KEY,KEY2) cv = cwal_prop_get( val, KEY, cwal_strlen(KEY) ); \
    if( cv ) {                                                          \
      valstr = cwal_cgi_pod_to_string( cx, cv, &cx->tmpBuf );             \
      if( valstr ) {                                                    \
        cwal_outputf( cx->e, "; "KEY2"=%.*s", (int)cx->tmpBuf.used, valstr ); \
      }                                                                 \
      cx->tmpBuf.used = 0;                                              \
    } (void)0
    DOPART("domain","Domain");
    DOPART("path","Path");
    DOPART("sameSite", "SameSite");
#undef DOPART

    cv = cwal_prop_get( val, "expires", 7 );
    if( cv || valueIsNull ){
      cwal_int_t intVal = valueIsNull ? 1 : cwal_value_get_integer(cv);
      if(intVal < 0) intVal = 1;
      valstr = cwal_cgi_rfc7231_timedate( (time_t)intVal, tsBuf, TSBufSize );
      if( valstr ){
        cwal_outputf( cx->e, "; Expires=%s", valstr );
      }
    }
    cv = cwal_prop_get( val, "secure", 6 );
    if( cwal_value_get_bool(cv) ){
      cwal_output( cx->e, "; Secure", 8 );
    }
        
    cv = cwal_prop_get( val, "httpOnly", 8 );
    if( cwal_value_get_bool(cv) ){
      cwal_output( cx->e, "; HttpOnly", 10 );
    }
    cwal_output( cx->e, "\r\n", 2);
  }
  else{
    char const * valstr;
    valstr = cwal_cgi_pod_to_string( cx, val, &cx->tmpBuf);
    if( ! valstr ) return 0;
    else{
      rc = cwal_cgi_urlencode( cx, valstr, cx->tmpBuf.used, &cx->tmpBuf2 );
      if( rc ) return rc;
      cwal_outputf(cx->e,"Set-Cookie: %.*s=%.*s\r\n",
                   (int)keyLen, key, (int)cx->tmpBuf2.used,
                   (char const *)cx->tmpBuf2.mem);
      cx->tmpBuf2.used = 0;
    }
  }
  return 0;
}

static char const * cwal_cgi_http_status_cstr(int httpCode){
  switch(httpCode/100*100 /* strip httpCode%100 part */){
    case 500:
      switch(httpCode){
        case 501: return "Not Implemented";
        case 502: return "Temporarily Unavailable";
        case 503: return "Gateway Timeout";
        case 500:
        default:
          return "Internal Server Error";
      }
    case 400:
      switch(httpCode){
        case 401: return "Unauthorized";
        case 402: return "Payment Required";
        case 403: return "Forbidden";
        case 404: return "Not Found";
        case 400:
        default:
          return "Bad Request";
      }
    case 300:
      switch(httpCode){
        case 301: return "Moved";
        case 302: return "Found";
        case 303: return "Method";
        case 304: return "Not Modified";
        case 300:
        default:
          return "Redirect";
      }
    case 200:
      switch(httpCode){
        case 201: return "Created";
        case 202: return "Accepted";
        case 203: return "Partial Information";
        case 204: return "No Response";
        case 200:
        default:
          return "OK";
      }
    default:
      return "???";
  }
}

#if 0
/**
   Returns the initial cwal_outputer assigned to cgi->se->e, in order
   to bypass the output buffering layer.
*/
static cwal_outputer const * cwal_cgi_lowest_outputer(cwal_cgi * cgi){
  cwal_outputer const * rc = cgi->se->ob.count
    ? (cwal_outputer const *)cgi->se->ob.list[0] /* initial outputer */
    : cwal_engine_outputer_get(cgi->se->e);
  assert(rc && "Missing outputer!");
  return rc;
}
#else
/**
   Returns the initial cwal_outputer assigned to cgi->se->e, in order
   to bypass the output buffering layer.
*/
static cwal_outputer const * cwal_cgi_lowest_outputer(cwal_cgi * cgi){
  cwal_outputer const * rc = cwal_engine_outputer_get(cgi->e);
  assert(rc && "Missing outputer!");
  return rc;
}
#endif

/**
   Outputs the accumulated headers. If passthrough is true then it
   redirects output to cx->se->e's initially-defined outputer. i.e.
   it bypasses output buffering.
*/
static int cwal_cgi_response_output_headers(cwal_cgi * cx, char passthrough)
{
  int rc;
  cwal_outputer backOut = cwal_outputer_empty;
  if(passthrough){
    cwal_engine_outputer_set( cx->e, cwal_cgi_lowest_outputer(cx),
                              &backOut );
  }
  if(cx->response.contentType){
    rc = cwal_outputf(cx->e, "Content-type: %s\r\n", cx->response.contentType);
    if(rc) goto end;
  }/* else assume it's been set as a header */
  rc = cwal_outputf(cx->e, "Status: %d %s\r\n", cx->response.httpCode,
                    cx->response.httpStatus
                    ? cx->response.httpStatus
                    : cwal_cgi_http_status_cstr(cx->response.httpCode));
  if(rc) goto end;
  ;
  if(cx->response.headers){
    rc = cwal_props_visit_kvp( cx->response.headers,
                               cwal_cgi_kvp_visitor_headers,
                               cx );
    if(rc) goto end;
  }
  if(cx->response.cookies){
    rc = cwal_props_visit_kvp( cx->response.cookies,
                               cwal_cgi_kvp_visitor_cookies,
                               cx );
  }
  if( 0 == rc ){
    rc = cwal_output(cx->e, "\r\n", 2);
  }
  end:
  if(passthrough){
    cwal_engine_outputer_set(cx->e, &backOut, 0);
  }
  return rc;
}

int cwal_cgi_response_status_set(cwal_cgi * cx, int httpCode,
                               char const * msg){
  size_t msgLen;
  if(!cx) return CWAL_RC_MISUSE;
  if(!msg || !*msg){
    msg = cwal_cgi_http_status_cstr(httpCode);
  }
  cx->response.httpCode = httpCode;
  cwal_free(cx->e, cx->response.httpStatus);
  msgLen = msg ? strlen(msg) : 0;
  if(!msgLen) cx->response.httpStatus = NULL;
  else{
    cx->response.httpStatus  = (char *)cwal_malloc(cx->e, msgLen+1);
    if(!cx->response.httpStatus) return CWAL_RC_OOM;
    memcpy(cx->response.httpStatus, msg, msgLen);
    cx->response.httpStatus[msgLen] = 0;
  }
  return 0;
}



int cwal_cgi_response_output_all(cwal_cgi * cx){
  int rc = 0;
  int doHeaders;
  cwal_buffer body = cwal_buffer_empty;
  if(!cx) return CWAL_RC_MISUSE;
  if(cx->hasResponded) return 0;
  doHeaders = cx->response.headerMode;
  if( doHeaders < 0 ){
    doHeaders = getenv("GATEWAY_INTERFACE") ? 1 : 0;
  }

  whcl_engine * const el = whcl_engine_from_state(cx->e);
  if(whcl_ob_level(el) > cx->originalObLevel){
    while(whcl_ob_level(el) > cx->originalObLevel + 1){
      rc = whcl_ob_flush(el);
      if(rc) goto end;
      whcl_ob_pop(el);
    }
    whcl_ob_take( el, &body );
    whcl_ob_pop(el);
    assert(cx->originalObLevel == whcl_ob_level(el));
  }

  if( doHeaders > 0 ){
    rc = cwal_cgi_response_output_headers(cx, 0);
    if(rc) goto end;
  }

  /** Output the body... */
  if(body.used){
    rc = cwal_output(cx->e, body.mem, body.used);
    if(rc) goto end;
  }
  rc = cwal_output_flush( cx->e );
  end:
  cwal_buffer_reserve(cx->e, &body, 0);
  cx->hasResponded = rc ? -1 : 1;
  return rc;
}

/* broken by removal of request.ENV object... */
cwal_value * cwal_cgi_path_part( cwal_cgi * cx,
                               unsigned short ndx ){
  cwal_value * piV = cwal_cgi_getenv( cx, "e", "PATH_INFO_SPLIT", 15 );
  return piV ? cwal_array_get(cwal_value_get_array(piV), ndx) : NULL;
}

char const * cwal_cgi_path_part_cstr( cwal_cgi * cx, unsigned short ndx,
                                    cwal_size_t * strLen )
{
  cwal_value * v = cwal_cgi_path_part( cx, ndx );
  return v
    ? cwal_value_get_cstr( v, strLen )
    : NULL;
}

int cwal_cgi_init2( cwal_cgi * cgi, int initFlags ){
  int rc = 0;
  char const * stepName = 0;
  if(1 != cgi->inited) return CWAL_RC_MISUSE;
  stepName = "HTTP_COOKIE";
  rc = cwal_cgi_parse_cookies( cgi, getenv("HTTP_COOKIE") );
  if(rc) goto end;
  stepName = "QUERY_STRING";
  rc = cwal_cgi_parse_query_string( cgi, getenv("QUERY_STRING") );
  if(rc) goto end;
  stepName = "POST";
  rc = cwal_cgi_init_POST(cgi);
  if(rc) goto end;
  stepName = "IMPORT_ENV";
  rc = cwal_cgi_import_environ(cgi, 0!=(initFlags & CWAL_CGI_INIT2_IMPORT_ENV));
  end: ;
  whcl_engine * const el = whcl_engine_from_state(cgi->e);
  cgi->originalObLevel = whcl_ob_level(el);
  if(!rc && initFlags & CWAL_CGI_INIT2_PUSH_OB){
    stepName = "ob_push()";
    rc = whcl_ob_push(el);
  }
  if(rc && rc!=cgi->err.rc && !cgi->err.msg.used){
    rc = cwal_cgi_err_set( cgi, rc,
                           "CGI initialization failed "
                           "with code %d (%s) at step %s.",
                           rc, cwal_rc_cstr(rc), stepName );
  }else{
    cgi->inited = 2;
  }
  return rc;
}

int cwal_cgi_init1( cwal_engine * const e, cwal_cgi * const cgi ){
  int rc = 0;
  if(cgi->inited > 0) return CWAL_RC_MISUSE;
  *cgi = cwal_cgi_empty;
  cgi->e = e;
  cgi->refHolder = cwal_new_object_value(cgi->e);
  if(!cgi->refHolder){
    return CWAL_RC_OOM;
  }else{
    cwal_ref(cgi->refHolder);
    cwal_value_make_vacuum_proof(cgi->refHolder, true);
  }
  assert(0==rc);
  cgi->request.input = stdin /*TODO: make configurable*/;
  cgi->inited = 1;
  return rc;
}

void cwal_cgi_cleanup( cwal_cgi * cgi ){
  if(cgi && cgi->e){
    cwal_buffer_clear( cgi->e, &cgi->tmpBuf );
    cwal_buffer_clear( cgi->e, &cgi->tmpBuf2 );
    cwal_buffer_clear( cgi->e, &cgi->err.msg );
    whcl_engine * const el = whcl_engine_from_state(cgi->e);
    while(whcl_ob_level(el) > cgi->originalObLevel){
      whcl_ob_pop(el);
    }
    /* cwal_value_unref(cgi->response.cookies); */
    cwal_free(cgi->e, cgi->response.httpStatus);
    if(cgi->refHolder){
      assert(1 == cwal_value_refcount(cgi->refHolder));
      cwal_value_unref(cgi->refHolder)
        /* will clean up the other cwal_values owned by cgi */;
      cgi->refHolder = 0;
    }
    *cgi = cwal_cgi_empty;
  }
}

cwal_value * cwal_cgi_env_get( cwal_cgi * cx,
                             char which,
                             char createIfNeeded ){
  cwal_value ** v = NULL;
  char const * gckey = NULL;
    
  switch( which )
  {
    case 'c':
    case 'C':
      gckey = cwal_cgi_keys.ENV_COOKIE;
      v = &cx->request.cookies;
      break;
    case 'e':
    case 'E':
      gckey = cwal_cgi_keys.ENV_SYS;
      v = &cx->request.env;
      break;
    case 'h':
    case 'H':
      gckey = cwal_cgi_keys.RESPONSE_HEADERS;
      v = &cx->response.headers;
      break;
    case 'g':
    case 'G':
      gckey = cwal_cgi_keys.ENV_GET;
      v = &cx->request.get;
      break;
    case 'f':
    case 'F':
      gckey = cwal_cgi_keys.ENV_CONFIG;
      v = &cx->config;
      break;
    case 'p':
    case 'P':
      gckey = cwal_cgi_keys.ENV_POST;
      v = &cx->request.post;
      break;
#if 0
    case 'a':
    case 'A':
      gckey = cwal_cgi_keys.ENV_APP;
      v = &cx->clientEnv;
      break;
    case 's':
    case 'S':
      gckey = cwal_cgi_keys.ENV_SESSION;
      v = &cx->session.env;
      break;
#endif
    default:
      break;
  }
  if( v ){
    if( !*v && createIfNeeded ){
      *v = cwal_new_object_value(cx->e);
      if(*v){
        int rc;
        cwal_value_prototype_set(*v, 0)
          /* to simplify property lookups (avoid finding
             inherited properties) */;
        cwal_value_ref(*v);
        rc = cwal_cgi_stash_set(cx, gckey, *v);
        /* MARKER(("stash rc=%d/%s\n",rc, cwal_rc_cstr(rc))); */
        cwal_value_unref(*v);
        if(!rc){
          assert(cwal_value_refcount(*v)>0);
        }else{
          *v = NULL;
        }
      }
    }
  }
  return v ? *v : NULL;
}

cwal_value * cwal_cgi_getenv( cwal_cgi * cx, char const * fromWhere,
                            char const * key, cwal_size_t keyLen )
{
  cwal_value * jv = NULL;
  if( !key || !*key ) return NULL;
  else if( !fromWhere || !*fromWhere ) fromWhere = CWAL_CGI_GETENV_DEFAULT;
  for( ; *fromWhere ; ++fromWhere ){
    if( ('r'==*fromWhere)||('R'==*fromWhere) ){
      jv = cwal_cgi_getenv( cx, "gpc", key, keyLen );
    }else{
      jv = cwal_cgi_env_get( cx, *fromWhere, 0 );
    }
    if( jv ){
      jv = cwal_prop_get( jv, key, keyLen );
      if(jv) return jv;
    }
  }
  return NULL;
}

char const * cwal_cgi_getenv_cstr( cwal_cgi * cx, char const * where,
                                 char const * key, cwal_size_t keyLen,
                                 cwal_size_t * strLen )
{
  cwal_value * v = cwal_cgi_getenv(cx, where, key, keyLen);
  return v ? cwal_value_get_cstr(v, strLen) : NULL;
}

int cwal_cgi_setenv_v( cwal_cgi * cx, char env,
                     char const * key,
                     cwal_size_t keyLen,
                     cwal_value * v ){
  if( !cx || !env || !key || !*key ) return CWAL_RC_MISUSE;
  else
  {
    cwal_value * e = cwal_cgi_env_get( cx, env, 1 );
    return !e
      ? CWAL_RC_OOM /* FIXME: expand the above code so we
                       can distinguish between invalid
                       env and allocation error. (Except that
                       there is no allocation on get_obj().*/
      : cwal_prop_set( e, key, keyLen, v )
      ;
  }
}

int cwal_cgi_setenv( cwal_cgi * cx, char const * key,
                   cwal_size_t keyLen, cwal_value * v ){
  return cwal_cgi_setenv_v( cx, 'e', key, keyLen, v );
}

int cwal_cgi_response_header_add( cwal_cgi * cx, char const * key,
                                cwal_size_t keyLen, cwal_value * v )
{
  cwal_value * env;
  if( !cx || ! key || !*key || !keyLen ) return CWAL_RC_MISUSE;
  env = cwal_cgi_env_get( cx, 'h', 1 );
  if(!env) return CWAL_RC_OOM;
  v = cgi__value_unwrap(v);
  {
    cwal_value * vKey = 0;
    int rc;
    rc = cwal_utf8_case_fold(cx->e, key, keyLen, &vKey, 0);
    if(rc) return rc;
    assert(vKey);
    cwal_value_ref(vKey);
    rc = cwal_prop_set_v( env, vKey, v );
    cwal_value_unref(vKey);
    return rc;
  }
}

int cwal_cgi_response_expires( cwal_cgi * cx, cwal_int_t epochTime ){
  enum { TSBufSize = 32 };
  char tsBuf[TSBufSize] = {0} /* buffer for expiry timestamp */;
  char const * str = cwal_cgi_rfc7231_timedate( (time_t)epochTime,
                                             tsBuf, TSBufSize );
  cwal_value * vstr = cwal_new_string_value(cx->e,
                                            str,
                                            cwal_strlen(str));
  int rc = vstr ? 0 : CWAL_RC_OOM;
  if(!rc){
    cwal_value_ref(vstr);
    rc = cwal_cgi_response_header_add(cx, "Expires", 7,
                                    vstr);
    cwal_value_unref(vstr);
  }
  return rc;
}


static cwal_value * cwal_cgi_response_cookies(cwal_cgi * cx){
  if(!cx->response.cookies){
    cx->response.cookies = cwal_new_object_value(cx->e);
    /* FIXME: add a rescoper to cx to keep this properly alive! */
    if(cx->response.cookies){
      int rc;
      cwal_value_ref(cx->response.cookies);
      rc = cwal_cgi_stash_set(cx, cwal_cgi_keys.RESPONSE_COOKIES,
                            cx->response.cookies);
      cwal_value_unref(cx->response.cookies);
      if(rc){
        cx->response.cookies = 0;
      }else{
        assert(cwal_value_refcount(cx->response.cookies) > 0 && "Expecting stashed ref.");
      }
    }
  }
  return cx->response.cookies;
}

int cwal_cgi_cookie_set( cwal_cgi * cx,
                       char const * key,
                       cwal_size_t keyLen,
                       cwal_value * v ){
  if( !key || !*key || !keyLen ) return CWAL_RC_MISUSE;
  else{
    cwal_value * cookies = cwal_cgi_response_cookies(cx);
    return cookies
      ? cwal_prop_set( cookies, key, keyLen, v )
      : CWAL_RC_OOM;
    /* MARKER(("Setting cookie [%s], init rc=%d\n", key, rc)); */
  }
}

int cwal_cgi_cookie_set_v( cwal_cgi * cx,
                         cwal_value * key,
                         cwal_value * v ){
  if( !key ) return CWAL_RC_MISUSE;
  else{
    cwal_value * cookies = cwal_cgi_response_cookies(cx);
    return cookies
      ? cwal_prop_set_v( cookies, key, v )
      : CWAL_RC_OOM;
    /* MARKER(("Setting cookie [%s], init rc=%d\n", key, rc)); */
  }
}

int cwal_cgi_cookie_set2( cwal_cgi * cx,
                        char const * key, cwal_size_t keyLen,
                        cwal_value * v,
                        char const * domain, char const * path,
                        unsigned int expires, char secure,
                        char httponly,
                        char const * sameSite )
{
  if( ! key || !*key ) return CWAL_RC_MISUSE;
  else
  {
    int rc;
    cwal_value * x = NULL;
    cwal_value * jo = cwal_new_object_value(cx->e);
    if( ! jo ) return CWAL_RC_OOM;
    if( !v ) v = cwal_value_null();
    cwal_value_ref(jo);

#define SET(KEY) if(!x) rc=CWAL_RC_OOM;                             \
    else  {                                                         \
      cwal_value_ref(x);                                            \
      rc = cwal_prop_set( jo, KEY, cwal_strlen(KEY), x);             \
      cwal_value_unref(x);                                            \
      x = 0;                                                          \
    }                                                                 \
    if(rc) {                                                          \
      cwal_value_unref( jo );                                         \
      return rc;                                                      \
    }

    rc = cwal_prop_set( jo, "value", 5, v);
    if(rc) return rc;
    if(domain){
      x = cwal_new_string_value( cx->e, domain, cwal_strlen(domain) );
      SET("domain");
    }
    if( path ){
      x = cwal_new_string_value( cx->e, path, cwal_strlen(path) );
      SET("path");
    }
    if( sameSite ){
      x = cwal_new_string_value( cx->e, sameSite, cwal_strlen(sameSite) );
      SET("sameSite");
    }
    if( cwal_value_is_null(v) || cwal_value_is_undef(v) ){
      x = cwal_new_integer( cx->e, 1 );
      SET("expires");
    }
    else if( expires ){
      x = cwal_new_integer( cx->e, (cwal_int_t) expires );
      SET("expires");
    }
    if( secure ){
      x = cwal_new_bool(secure);
      SET("secure");
    }
    if( httponly ){
      x = cwal_new_bool(httponly);
      SET("httpOnly");
    }
#undef SET
    rc = cwal_cgi_cookie_set( cx, key, keyLen, jo );
    cwal_value_unref(jo);
    return rc;
  }
}

/**
   cwal_cgi_hexchar_to_int():

   For 'a'-'f', 'A'-'F' and '0'-'9', returns the appropriate decimal
   number.  For any other character it returns -1.
*/
static int cwal_cgi_hexchar_to_int( int ch ){
  if( (ch>='a' && ch<='f') ) return ch-'a'+10;
  else if( (ch>='A' && ch<='F') ) return ch-'A'+10;
  else if( (ch>='0' && ch<='9') ) return ch-'0';
  return -1;
}

void cwal_cgi_urldecode_inline( char * str, cwal_size_t * sLen ){
  unsigned char ch = 0;
  unsigned char cx1 = 0;
  unsigned char cx2 = 0;
  int decoded;
  unsigned char * pos = (unsigned char *)str;
  unsigned char * out = pos;
  unsigned char const * end;
  size_t slen = (str && *str) ? (sLen ? *sLen : strlen(str)) : 0;
  if( !slen ) return;
  end = pos + slen;
  for( ; pos < end; ++pos ){
    ch = *pos;
    if( ch == '%' )
    {
      cx1 = *(pos+1);
      /* FIXME: with only minor refactoring we can remove the
         isxdigit() calls and use cwal_cgi_hexchar_to_int()
         instead, checking for a negative return value. That
         would potentially save us 2 extra function calls here.
      */
      if( isxdigit(cx1) ){
        cx2 = *(pos+2);
        if( isxdigit(cx2) )
        {
          decoded = (cwal_cgi_hexchar_to_int( cx1 ) * 16)
            + cwal_cgi_hexchar_to_int( cx2 );
          *(out++) = (char)decoded;
          pos += 2;
          continue;
        }
        /* else fall through... */
      }
      /* else fall through... */
    }
    else if( ch == '+' )
    {
      *(out++) = ' ';
      continue;
    }
    *(out++) = ch;
  }
  *out = 0;
  if(sLen) *sLen = (out - (unsigned char *)str);
}

int cwal_cgi_urlencode( cwal_cgi * cx, char const * src,
                      cwal_size_t srcLen,
                      cwal_buffer * _dest ){
#define needs_escape                            \
  ( (ch >= 32 && ch <=47)                       \
    || ( ch>=58 && ch<=64)                      \
    || ( ch>=91 && ch<=96)                      \
    || ( ch>=123 && ch<=126)                    \
    || ( ch<32 || ch>=127)                      \
    )
  char const * pos = src;
  char const * end = src + srcLen;
  char * dest;
  char ch;
  int rc;
  static char const * hex = "0123456789ABCDEF";
  if( !cx || !src ) return CWAL_RC_MISUSE;
  rc = cwal_buffer_reserve( cx->e, _dest,
                            _dest->used + (srcLen*3) + 1);
  if(rc) return rc;
  dest = (char *)_dest->mem + _dest->used;
  for( ; (pos<end) && *pos; ++pos ){
    ch = *pos;
    if( ! needs_escape ){
      *(dest++) = ch;
      continue;
    }
    else
    {
      *(dest++) = '%';
      *(dest++) = hex[((ch>>4)&0xf)];
      *(dest++) = hex[(ch&0xf)];
    }
  }
  _dest->used = dest - (char *)_dest->mem;
  return 0;
#undef needs_escape
}


int cwal_cgi_response_passthrough_FILE( cwal_cgi * cgi, FILE * fp ){
  cwal_outputer const * out;
  int rc = 0;
  if(cgi->hasResponded) return 0 /* silently fail! */;
  whcl_engine * const el = whcl_engine_from_state(cgi->e);
  while(whcl_ob_level(el) > cgi->originalObLevel){
    whcl_ob_pop(el);
  }
  cwal_cgi_response_output_headers(cgi, 1);
  cgi->response.headerMode = 0 /* suppress further output of headers */;
  out = cwal_cgi_lowest_outputer(cgi);
  if(out && out->output){
    rc = cwal_stream( cwal_input_f_FILE, fp,
                      cwal_output_f_cwal_outputer,
                      (void *)out );
  }
  cgi->hasResponded = rc ? -2 : 2;
  return rc;
}

int cwal_cgi_response_passthrough( cwal_cgi * cgi, char const * fn ){
  int rc = 0;
  FILE * fp;
  if(cgi->hasResponded) return 0 /* silently fail! */;
  else if(!fn || !*fn) return CWAL_RC_MISUSE;
  fp = fopen( fn, "r");
  if(!fp) rc = CWAL_RC_IO;
  else{
    rc = cwal_cgi_response_passthrough_FILE(cgi, fp);
    fclose(fp);
  }
  cgi->hasResponded = rc ? -3 : 3;
  return rc;
}

#if 0
char const * cwal_cgi_guess_content_type(){
  char const * cset;
  char doUtf8;
  char const * cstr;
  cset = getenv("HTTP_ACCEPT_CHARSET");
  doUtf8 = (cset && strstr("utf-8",cset))
    ? 1 : 0;
  cstr = getenv("HTTP_ACCEPT");
  if( strstr( cstr, "application/json" )
      || strstr( cstr, "*/*" ) ){
    return doUtf8
      ? "application/json; charset=utf-8"
      : "application/json";
  }else{
    return NULL /*"text/plain"*/;
  }
}
#endif

char const * cwal_cgi_value_cstr( cwal_value const * v, cwal_size_t * len ){
  if(cwal_value_is_unique(v)) v = cwal_unique_wrapped_get(v);
  return cwal_value_get_cstr(v, len);
}

#undef MARKER
