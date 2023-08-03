/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#if !defined(NET_WANDERINGHORSE_CWAL_S2_CGI_H_INCLUDED)
#define NET_WANDERINGHORSE_CWAL_S2_CGI_H_INCLUDED
/**
   This file houses the s2_cgi API, a far-from-complete CGI
   application framework based on s2.
*/
#include "libs2.h"

#if defined(__cplusplus)
extern "C" {
#endif

/**
   Flags for use with s2_cgi_init2().
 */
enum s2_cgi_init2_flags {
  /**
     Specifies that s2_cgi_init2() should import all environment
     variables, storing them as an Object in s2_cgi::request::env.
  */
  S2_CGI_INIT2_IMPORT_ENV = 0x01,
  /**
     Specifies that s2_cgi_init2() should call s2_ob_push().  If this
     flag is not set, the client must call s2_ob_push() before
     outputing any body content, so that headers can get output
     properly (before the content).
  */
  S2_CGI_INIT2_PUSH_OB = 0x02
};

/**
   State container for the s2 CGI module. It holds all kinds of
   stuff related to CGI state.
*/
struct s2_cgi {
  /**
     Convenience handle to this->se->e.
   */
  cwal_engine * e;
  /**
     s2 engine this CGI instance uses.
  */
  s2_engine * se;

  /**
     s2_cgi_init2() sets member to the current s2_ob_level(). s2_cgi
     assumes that it is allowed to close any buffer levels activated
     after s2_cgi_init1() completes, and it will eventually flush
     and/or pop them when it cleans up or sends its response.
  */
  cwal_size_t originalObLevel;

  /**
     Internal marker to tell us whether s2_cgi_init1() and/or
     s2_cgi_init2() has been called. It's initially 0. s2_cgi_init1()
     sets it to 1 and s2_cgi_init2() sets it to 2.
  */
  int inited;

  /**
     Gets set to a non-0 value when any of the response-generation
     routines get called.
  */
  int hasResponded;
  
  /**
     Holds info related to the request.
  */
  struct {
    /**
       POST data. The library handles JSON and transforms
       form/url-encoded input to a JSON form here.
    */
    cwal_value * post;
    /**
       An Object holding values from the HTTP GET parameters
       (i.e. from getenv("QUERY_STRING")).
    */
    cwal_value * get;
    /**
       An Object holding values from the HTTP cookies
       (i.e. from getenv("HTTP_COOKIE")).
    */
    cwal_value * cookies;
    /**
       An Object holding the raw (currently not unencoded)
       values from the environ(7) environment
       (i.e. from getenv(3) and friends).
    */
    cwal_value * env;
    /**
       stdin for reading POST data. Currently hard-wired
       to stdin.
    */
    FILE * input;
  } request;
  struct {
    /**
       Object of HTTP headers as key/value pairs.
    */
    cwal_value * headers;
    /**
       Object of HTTP cookies as key/value pairs.
    */
    cwal_value * cookies;
    /**
       Response content type.
    */
    char const * contentType;
    /**
       Response HTTP code type.
    */
    int httpCode;
    /**
       Response HTTP status message:

       Status: httpCode httpStatus

       Owned by this object.
    */
    char * httpStatus;
    /**
       Specifies whether or not HTTP headers (including cookies)
       should be output.
        
       0==no header output

       >0==output headers

       <0==determine automatically if headers
       should be output.
    */
    int headerMode;
  } response;

  /**
     TODO. Config file (JSON).
  */
  cwal_value * config;

  /**
     An Object to store a ref to all other cwal_values this object
     owns and (greatly) simplify rescoping of them.
  */
  cwal_value * refHolder;
  /**
     Scratchpad buffer.
  */
  cwal_buffer tmpBuf;
  /**
     Second scratchpad buffer, for when tmpBuf is currently being
     used.
  */
  cwal_buffer tmpBuf2;
  /**
     Holds non-HTTP error information.
  */
  struct {
    /**
       Underlying result code. Hopefully one
       of the CWAL_RC_xxx values.
    */
    int rc;
    /**
       _Might_ hold an error string in the first msg.used bytes of
       msg.mem.
    */
    cwal_buffer msg;
  } err;
};
typedef struct s2_cgi s2_cgi;
/**
   "Empty" s2_cgi object, intended for use in copy-constructing.
*/
#define s2_cgi_empty_m {                                \
      NULL/*e*/,                                          \
      NULL/*se*/,                                       \
      0/*originalObLevel*/,                           \
      0/*inited*/,                                    \
      0/*hasResponded*/,                              \
      {/*request*/                                      \
        NULL/*post*/,                                   \
          NULL/*get*/,                                  \
          NULL/*cookies*/,                              \
          NULL/*env*/,                                  \
          NULL/*input*/                                 \
          },                                            \
      {/*response*/                                     \
        NULL/*headers*/,                                \
          NULL/*cookies*/,                              \
          NULL/*contentType*/,                          \
          200/*httpCode*/,                              \
          NULL/*httpStatus*/,                           \
          1/*headerMode*/                               \
          },                                            \
        NULL/*config*/,                               \
        NULL/*refHolder*/,                            \
        cwal_buffer_empty_m/*tmpBuf*/,                \
        cwal_buffer_empty_m/*tmpBuf2*/,               \
        {/*err*/ 0/*rc*/, cwal_buffer_empty_m/*msg*/} \
  }

/**
   "Empty" s2_cgi object, intended for use
   in copy-constructing.
*/
extern const s2_cgi s2_cgi_empty;

/** @def S2_CGI_GETENV_DEFAULT

    The default environment name(s) to use for cwal_cgi_getenv().
*/
#define S2_CGI_GETENV_DEFAULT "gpc"

/**
   Initialization is a two-step process. The first one (via this
   function) initializes the s2_cgi context and allocates any
   low-level resources it needs. The second (s2_cgi_init2()) processes
   the current CGI environment, importing GET/POST data and such.

   tgt must be a cleanly-initialized s2_cgi instance, and it may be
   stack allocated (copy s2_cgi_empty over it to cleanly initialize
   it).

   Ownership of se and tgt are not changed and se must outlive tgt.

   Returns 0 on success.

   On error, if either se or tgt are NULL then there are no side
   effects, otherwise tgt might be partially initialized, so the
   client must eventually pass tgt to s2_cgi_cleanup() before se
   is cleaned up.

   If this call succeeds, the caller should eventually s2_cgi_init2()
   to complete the initialization. It is a separate function due to
   timing requirements of building this module in statically to
   s2sh. (It also incidentally allows us to pass cgi configuration
   information on from client-side sources.)
*/
int s2_cgi_init1( s2_engine * se, s2_cgi * tgt );

/**
   This function performs the "second half" of s2_cgi initialization,
   setting up the "request" object, importing GET/POST data, reading
   cookies, etc.

   It is illegal to call this unless s2_cgi_init1() has succeeded
   for the given s2_cgi instance.

   The 2nd argument may be 0 or a mask of any s2_cgi_init2_flags.
   This function pushes an output buffer on the stack (using
   s2_ob_push()) if initFlags contains the S2_CGI_INIT2_PUSH_OB flag.

   Returns 0 on success, non-0 on error, with the "most likely"
   failure code being CWAL_RC_OOM (meaning an allocation failed). It
   returns CWAL_RC_MISUSE if it's called more than once on a given
   s2_cgi instance.

   s2_cgi assumes that it is allowed to close any buffer levels
   activated after s2_cgi_init2() completes, and it will eventually
   flush and/or pop them when it cleans up.
*/
int s2_cgi_init2( s2_cgi * cgi, int initFlags );

/**
   Cleans up memory owned by the given cgi instance but does not free
   cgi. The caller is responsible for deallocating cgi in a manner
   complementary to its allocation mechanism.
*/
void s2_cgi_cleanup( s2_cgi * cgi );

/**
   Sets the HTTP response code and message string. The msg bytes
   are copied by this function, and need not be static. The default
   response code is 200. If msg is NULL or empty then some built-in
   string is used, depending on the value of httpCode.
*/
int s2_cgi_response_status_set(s2_cgi * cx, int httpCode,
                               char const * msg);


/**
   Fetches one of cgi's environment containers.

   The second parameter specifies which environment container to
   fetch: see s2_cgi_setenv_v() for the list of legal values.

   If the 3rd parameter is given a truthy value (non-0) then the
   environment object is created if needed, and NULL is returned if
   the object cannot be created.

   @see s2_cgi_setenv_v()
 */
cwal_value * s2_cgi_env_get( s2_cgi * cgi,
                             char which,
                             char createIfNeeded );

/**
   Checks one or more of the cgi context's environment containers for
   the given key. fromWhere is a string of characters specifying which
   environment(s) to check, and they are checked in the order
   specified. See s2_cgi_setenv() for the valid list of letters. If
   fromWhere is NULL or *fromWhere is NUL then the value of
   S2_CGI_GETENV_DEFAULT is used.

   The 3rd and 4th parameters specify the key to search for, in the
   form of the first keyLen bytes of the given key.

   Returns NULL if no match is found, else returns an arbitrary value
   owned by its containing environment object.
 */
cwal_value * s2_cgi_getenv( s2_cgi * cx,
                            char const * fromWhere,
                            char const * key,
                            cwal_size_t keyLen);

/**
   Works like s2_cgi_getenv() except that it only returns stringable
   values. If the underlying call to s2_cgi_getenv() returns a value,
   this function returns the result of passing that value to
   cwal_value_get_cstr(). The final argument to this function is
   passed on as-is to cwal_value_get_cstr(), meaning that *strLen (if
   strLen is not NULL) will be set to the byte length of the returned
   C-string. If no match is found, or the value is not stringable then
   NULL is returned and *strLen is not modified.
*/
char const * s2_cgi_getenv_cstr( s2_cgi * cx, char const * where,
                                 char const * key, cwal_size_t keyLen,
                                 cwal_size_t* strLen );
/**
   Sets a variable in one of cx's environment objects.

   whichEnv must be the conventional character representation
   (case-insensitive) for one of the following environment objects:

   - g = GET
   - p = POST
   - e = ENV (i.e. getenv(3))
   - c = COOKIE
   - r = REQUEST (==GET, POST, COOKIE (in that order))

   On success 0 is returned and ownership of v is transfered to (or
   shared with) the appropriate environment object. On error non-zero
   is returned and ownership of v is not modified.
*/
int s2_cgi_setenv_v( s2_cgi * cx, char whichEnv,
                     char const * key,
                     cwal_size_t keyLen,
                     cwal_value * v );

/**
   Convenience form of s2_cgi_setenv_v() which sets
   the property in the 'e' environment.
*/
int s2_cgi_setenv( s2_cgi * cx, char const * key,
                   cwal_size_t keyLen, cwal_value * v );

/**
   Queues a cookie to be sent when the headers are output.

   The 2nd and 3rd parameters specify the cookie's key.

   The final value specifies its value. If v is NULL then any matching
   cookie is _immediately_ removed from the cookie storage (so it will
   not be sent when the headers are output). If it is
   cwal_value_null() or cwal_value_undefined() then the cookie will be
   removed from the client when the cookie headers are output, by
   sending it an empty cookie value with an expiry date in the past.

   For purposes of reference counting v, treat this function as a
   normal container operation: the container will obtain a reference
   to v on success.

   Returns 0 on success, non-0 on error: CWAL_RC_MISUSE if any
   argument is invalid (NULL or empty key), CWAL_RC_OOM if the cookie
   storage or the new entry cannot be allocated.
*/
int s2_cgi_cookie_set( s2_cgi * cx,
                       char const * key,
                       cwal_size_t keyLen,
                       cwal_value * v );

/**
   The Value counterpart of s2_cgi_cookie_set(), differing only in
   that it takes its key as a cwal_value.
*/
int s2_cgi_cookie_set_v( s2_cgi * cx,
                         cwal_value * key,
                         cwal_value * v );

/**
   Sets a cookie with (potentially) all sorts of metadata.

   value: the cookie's value may be any "reasonable" JSON value,
   including numbers, bools, and small strings. If it is NULL,
   cwal_value_null(), or cwal_value_undefined(), the cookie is queued
   for removal on the client by sending an expiring time in the past.

   expires: Unix timestamp. 0 == client session cookie (expired by
   client). Ignored if the value is NULL/null/undefined (see above).

   domain: the internet domain for the cookie.

   path: the top-most path for the cookie.

   expires: Unix epoch expiry time of the cookie.

   secure: sets the "secure" flag on the HTTP cookie.

   httpOnly: specifies whether the cookie should be exposed to script
   code (if httpOnly is false) or not (if httpOnly is true).   

   sameSite: sets the "sameSite" flag to the given string value
   (one of "none", "lax", or "strict"). For details see:

   https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie/SameSite
*/
int s2_cgi_cookie_set2( s2_cgi * cx,
                        char const * key, cwal_size_t keyLen,
                        cwal_value * value,
                        char const * domain, char const * path,
                        unsigned int expires, char secure,
                        char httpOnly,
                        char const * sameSite);

/**
   Queues up a header to be sent when the headers are output.

   ACHTUNG: this routine, because it stores the headers in a cwal
   container, is case-sensitive. To that end, this API lower-cases
   all headers added via this function.

   If v is of type CWAL_TYPE_UNIQUE then its wrapped value is used
   instead.

   If v is NULL (or is a UNIQUE-type value wrapping NULL) then the
   given header will be removed from the queue.

   Returns 0 on success. Returns CWAL_RC_MISUSE if !cx, !key, or !*key.
   Returns CWAL_RC_OOM if an allocation fails.

   Sidebar: the decision to lower-case headers, as opposed to
   upper-case them, was made because HTTP/2 specifies lower-case
   headers. This framework does not support HTTP/2 but that is the
   only(?) HTTP-related standard which specifies the case for headers,
   so we use it as a precedent.
*/
int s2_cgi_response_header_add( s2_cgi * cx, char const * key,
                                cwal_size_t keyLen, cwal_value * v );

/**
   This outputs cx's HTTP headers, flushes all pending output
   (buffered via the api.ob API), and cleans up any output buffers
   which were pushed since cx was initialized using s2_cgi_init1().

   This is a no-op if called more then once, or if called after
   another response-generating mechanism is called.
*/
int s2_cgi_response_output_all(s2_cgi * cx);

/**
   URL-decodes a string inline. sLen may be NULL, but if it is not
   then *sLen must initially be the length of str which should be
   decoded. On returning, if sLen is not NULL then it will contain the
   new virtual length of str (which may be shorter than its original
   length but never longer). str will be NUL-terminated by this
   function. If sLen is NULL then the equivalent of strlen() is used
   to determine how many bytes to decode.

   The only error conditions are if the arguments are invalid, in
   which case it returns without side effects (if it can determine the
   arguments are in error) or invokes undefined behaviour (if *sLen is
   longer than str's valid memory).
*/
void s2_cgi_urldecode_inline( char * str, cwal_size_t * sLen );

#if 0
/* broken by removal of request.ENV object... */
/**
   If the environment variable PATH_INFO is found during
   initialization, it is split into its component dir/file parts as an
   array. This routine returns the Nth element in that array, or NULL
   if out of bounds or no PATH_INFO is availble. The array itself is
   available via s2_cgi_getenv(cx,"e","PATH_INFO_SPLIT", 15).
*/
cwal_value * s2_cgi_path_part( s2_cgi * cx,
                               unsigned short ndx );
/**
   Convenience form of s2_cgi_path_part() with fetches the value
   as a C-string. If strLen is not NULL then it will be set to the
   length (in bytes) of the returned string pointer.  Returns NULL on
   error or if it can't figure out what to do.
*/
char const * s2_cgi_path_part_cstr( s2_cgi * cx, unsigned short ndx,
                                    cwal_size_t * strLen );
#endif

/**
   URL-encodes src to dest and NUL-terminates it. dest is expanded as
   needed.

   Returns 0 on success. On error dest might be partially populated.
*/
int s2_cgi_urlencode( s2_cgi * cx, char const * src,
                      cwal_size_t srcLen,
                      cwal_buffer * dest );

/**
   Adds an "Expires" response header with the given Unix epoch time.
   Returns 0 on success.
*/
int s2_cgi_response_expires( s2_cgi * cx, cwal_int_t epochTime );
    
/**
   Formats the values of the given time as an RFC 7231 timestamp
   string to dest, which must be at least destLen bytes long. On
   success returns dest, else NULL. The only error cases are destLen
   not being at least 31.
*/
char * s2_cgi_rfc7231_timedate( time_t now, char * dest, unsigned int destLen );

/**
   A specialized form of s2_cgi_response_output_all() which bypasses
   the output buffering layer, emits accumulated headers, and then
   streams the contents of the given file to the cwal_outputer in
   charge of output at the time (this will "normally" be the initial
   one installed by the s2-running app).

   Unlike s2_cgi_response_output_all(), this one does NOT pop any
   buffers off of the OB stack. That decision is up for debate,
   pending usage experience. The current thinking is that it's not
   wise to potentially pop layers which downstream clients expect to
   pop themselves.

   Returns 0 on success. On error, a CWAL_RC_xxx value is returned and
   the output is in an indeterminate state.

   This is a no-op (returning 0) if called more then once, or if
   called after another response-generating mechanism is called.
*/
int s2_cgi_response_passthrough_FILE( s2_cgi * cgi, FILE * fp );

/**
   Uses s2_fopen() to open the given file in read-only mode
   before passing it on to s2_cgi_response_passthrough_FILE(),
   returning its result. Returns CWAL_RC_IO if the file cannot
   be opened and CWAL_RC_MISUSE if fn is NULL or !*fn.
*/
int s2_cgi_response_passthrough( s2_cgi * cgi, char const * fn );

#if defined(__cplusplus)
} /*extern "C"*/
#endif

#endif
/* NET_WANDERINGHORSE_CWAL_S2_CGI_H_INCLUDED */
