/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

/**
   Impls for the string and buffer prototypes. They share some internal
   code so are not in separate files.
*/

#include "internal.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <errno.h>


#if 1
#define MARKER(pfexpr) if(1) printf("MARKER: %s:%d:\t",__FILE__,__LINE__); if(1) printf pfexpr
#else
#define MARKER if(0) printf
#endif

#define WHCL__E_ARGS \
  whcl_engine * const el = whcl_engine_from_args(args); assert(el)

#define THIS_STRING \
  cwal_string * self; \
  if(!(self=cwal_value_string_part(args->engine, args->self)) && args->argc) \
    self = cwal_value_string_part(args->engine, args->argv[0]);         \
  if(!self){                                                            \
    return cwal_exception_setf( args->engine, CWAL_RC_TYPE,             \
                                "Expecting a string 'this' or first argument." ); \
  } (void)0


#define THIS_CSTRING(USE_EMPTY_FOR_BUFFER) \
  char const * self; cwal_size_t selfLen; \
  if(!(self=cwal_value_get_cstr(args->self, &selfLen))){          \
    if(USE_EMPTY_FOR_BUFFER && cwal_value_get_buffer(args->self)){        \
      self = ""; selfLen = 0;                                             \
    }else {                                                               \
      return cwal_exception_setf( args->engine, CWAL_RC_TYPE,             \
                                  "Expecting a string or buffer 'this' argument." ); \
    }\
  } (void)0

/**
   Impl for string.glob-matches (globIsSelf==true) and
   string.matches-glob (globIsSelf==false).
*/
static int whcl__cb_glob_matches_impl( cwal_callback_args const * args,
                                       bool globIsSelf,
                                       cwal_value ** rv ){
  int rc = 0;
  enum cwal_glob_style_e policy = CWAL_GLOB_WILDCARD;
  char const * glob;
  char const * str;
  cwal_size_t nStr = 0;
  uint16_t argNdx = 0;
  if( whcl_arg_has_flag(args, &argNdx, &str, &nStr) ){
    cwal_value const * const arg = args->argv[argNdx-1];
    if(6==nStr){
      if(whcl_val_is_flag(arg, "-iglob", 6)) policy = CWAL_GLOB_WILDCARD_NOCASE;
      else if(whcl_val_is_flag(arg, "-ilike", 6)) policy = CWAL_GLOB_LIKE_NOCASE;
      else --argNdx;
    }else if(5==nStr){
      if(whcl_val_is_flag(arg, "-like", 5)) policy = CWAL_GLOB_LIKE;
      else if(whcl_val_is_flag(arg, "-glob", 5)) policy = CWAL_GLOB_WILDCARD;
      else --argNdx;
    }
  }
  if(cwal_value_is_string(args->self)){
    if(globIsSelf){
      str = (args->argc>argNdx) ? cwal_value_get_cstr(args->argv[argNdx++], NULL) : NULL;
      glob = cwal_value_get_cstr(args->self, NULL);
    }else{
      glob = (args->argc>argNdx) ? cwal_value_get_cstr(args->argv[argNdx++], NULL) : NULL;
      str = cwal_value_get_cstr(args->self, NULL);
    }
  }else{
    if(globIsSelf){
      glob = (args->argc>argNdx) ? cwal_value_get_cstr(args->argv[argNdx++], NULL) : NULL;
      str = (args->argc>argNdx) ? cwal_value_get_cstr(args->argv[argNdx++], NULL) : NULL;
    }else{
      str = (args->argc>argNdx) ? cwal_value_get_cstr(args->argv[argNdx++], NULL) : NULL;
      glob = (args->argc>argNdx) ? cwal_value_get_cstr(args->argv[argNdx++], NULL) : NULL;
    }
  }
  if(!glob || !str || args->argc>argNdx){
    if(globIsSelf){
      rc = cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "Expecting ([-like|-ilike] glob-needle hastack) "
                         "arguments "
                         "OR aGlobString.thisFunc(haystack).");
    }else{
      rc = cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "Expecting ([-like|-ilike] haystack glob-needle) "
                         "arguments "
                         "OR aHaystackString.thisFunc(glob-needle).");
    }
  }
  else if(!*glob){
    return cwal_cb_throw(args, CWAL_RC_RANGE,
                       "Glob string may not be empty.");
  }
  else{
    *rv = cwal_glob_matches_cstr(glob, str, policy)
      ? cwal_value_true() : cwal_value_false();
  }
  return rc;
}

int whcl_cb_glob_matches_str( cwal_callback_args const * args,
                                     cwal_value ** rv ){
  return whcl__cb_glob_matches_impl( args, true, rv);
}

int whcl_cb_str_matches_glob( cwal_callback_args const * args,
                              cwal_value ** rv ){
  return whcl__cb_glob_matches_impl( args, false, rv);
}


static int whcl__cb_str_to_string( cwal_callback_args const * args, cwal_value **rv ){
  cwal_string * const str = cwal_value_string_part( args->engine, args->self );
  *rv = str ? cwal_string_value(str) : cwal_value_undefined();
  return 0;
}


static int whcl__cb_str_concat( cwal_callback_args const * args, cwal_value **rv ){
  if(!args->argc){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "'concat' requires at least one argument.");
  }else{
    int rc = 0;
    cwal_size_t i = 0, argc = args->argc;
    cwal_buffer buf = cwal_buffer_empty;
    whcl_engine * const el = whcl_engine_from_args(args);
    cwal_string * const str = cwal_value_get_string(args->self);
    assert(el);
    if(1==args->argc && str
       && 0==cwal_string_length_bytes(str)
       && cwal_value_is_string(args->argv[0])){
      /* optimization: "".concat(anyString) simply returns argv[0] */
      *rv = args->argv[0];
      return 0;
    }
    if(cwal_value_is_string(args->self)){
      rc = whcl_value_to_buffer( el, &buf, args->self );
    }
    for( ; !rc && (i < argc); ++i ){
      rc = whcl_value_to_buffer( el, &buf, args->argv[i] );
    }
    if(!rc && !(*rv = cwal_string_value(cwal_buffer_to_zstring(args->engine,
                                                               &buf)))
       ){
      rc = CWAL_RC_OOM;
    }
    cwal_buffer_reserve( args->engine, &buf, 0 );
    return rc;
  }
}

static int whcl__cb_str_byte_at( cwal_callback_args const * args, cwal_value **rv ){
  cwal_int_t pos;
  THIS_STRING;
  pos = (args->argc>0)
    ? (cwal_value_is_number(args->argv[0])
       ? cwal_value_get_integer(args->argv[0])
       : -1)
    : -1;
  if(pos<0){
    return cwal_exception_setf( args->engine, CWAL_RC_MISUSE,
                                "byteAt expects an integer argument "
                                "with a value of 0 or more.");
  }else{
    unsigned char const * cstr =
      (unsigned char const *)cwal_string_cstr(self);
    cwal_size_t const slen = cwal_string_length_bytes(self);
    if(pos >= (cwal_int_t)slen){
      *rv = cwal_value_undefined();
    }else{
      *rv = cwal_new_integer(args->engine, cstr[pos]);
    }
    return *rv ? 0 : CWAL_RC_OOM;
  }
}

static int whcl__cb_str_isascii( cwal_callback_args const * args, cwal_value **rv ){
  THIS_STRING;
  *rv = cwal_new_bool( cwal_string_is_ascii(self) );
  return 0;
}

static int whcl__cb_str_char_at( cwal_callback_args const * args, cwal_value **rv ){
  cwal_int_t pos;
  THIS_STRING;
  pos = (args->argc>0)
    ? (cwal_value_is_number(args->argv[0])
       ? cwal_value_get_integer(args->argv[0])
       : -1)
    : -1;
  if(pos<0){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "charAt expects an integer argument with a "
                               "value of 0 or more.");
  }else{
    unsigned char const * cstr =
      (unsigned char const *)cwal_string_cstr(self);
    cwal_size_t const slen = cwal_string_length_bytes(self);
    int const asInt = (args->argc>1) && cwal_value_get_bool(args->argv[1]);
    if(pos >= (cwal_int_t)slen){
      *rv = cwal_value_undefined();
    }else{
      unsigned int cpoint = (unsigned int)-1;
      if( cwal_string_is_ascii(self) ){
        cpoint = (unsigned int)cstr[pos];
      }else{
        cwal_utf8_char_at( cstr, cstr + slen, pos, &cpoint);
      }
      if((unsigned int)-1 == cpoint){
        *rv = cwal_value_undefined();
      }else{
        if(asInt){
          *rv = cwal_new_integer(args->engine, (cwal_int_t)cpoint);
        }
        else {
          unsigned char buf[6] = {0,0,0,0,0,0};
          int const clen = cwal_utf8_char_to_cstr(cpoint, buf,
                                                  sizeof(buf));
          assert(clen<(int)sizeof(buf));
          assert(0 != clen);
          if(clen<1) *rv = cwal_value_undefined();
          else *rv =
                 cwal_new_string_value(args->engine, (char const *)buf,
                                       (cwal_size_t)clen);
        }
      }
    }
    return *rv ? 0 : CWAL_RC_OOM;
  }
}

static int whcl__cb_str_toupperlower( cwal_callback_args const * args,
                                   cwal_value **rv,
                                   char doUpper ){
  THIS_STRING;
  return cwal_string_case_fold( args->engine, self, rv, doUpper );
}

static int whcl__cb_str_tolower( cwal_callback_args const * args, cwal_value **rv ){
    return whcl__cb_str_toupperlower( args, rv, 0 );
}

static int whcl__cb_str_toupper( cwal_callback_args const * args, cwal_value **rv ){
    return whcl__cb_str_toupperlower( args, rv, 1 );
}


/**
   Internal helper for whcl__cb_str_split(), which splits the given
   string into an array of all of its individual characters.
*/
static int whcl__cb_str_split_chars(cwal_callback_args const * args,
                                 cwal_value **rv,
                                 unsigned char const * str,
                                 cwal_size_t slen,
                                 cwal_int_t limit ){
  int rc = 0;
  cwal_array * ar;
  unsigned char const * pos = str;
  unsigned char const * eof = str + slen;
  cwal_value * v;
  cwal_int_t count = 0;
  assert(str);
  ar = cwal_new_array(args->engine);
  if(!ar) return CWAL_RC_OOM;
#if 1
  rc = cwal_array_reserve(ar, slen /*in bytes, but it's close enough */);
#endif
  for( ; !rc && pos < eof; ){
    unsigned char const * cend = pos;
    cwal_utf8_read_char(pos, eof, &cend);
    if(!(cend-pos)) break /* ??? */;
    v = cwal_new_string_value(args->engine, (char const *)pos,
                              (cwal_size_t)(cend-pos));
    if(!v){
      rc = CWAL_RC_OOM;
      break;
    }
    cwal_value_ref(v);
    rc = cwal_array_append(ar, v);
    cwal_value_unref(v);
    pos = cend;
    if(limit > 0 && ++count==limit) break;
  }
  if(rc){
    cwal_array_unref(ar);
  }else{
    *rv = cwal_array_value(ar);
  }
  return rc;
  
}

static int whcl__cb_str_split( cwal_callback_args const * args,
                            cwal_value **rv ){
  cwal_size_t sepLen = 0;
  unsigned char const * sep;
  THIS_STRING;
  sep = args->argc
    ? (unsigned char const *)cwal_value_get_cstr(args->argv[0], &sepLen)
    : NULL;
  if(!sep){
    return cwal_exception_setf( args->engine, CWAL_RC_MISUSE,
                                "Expecting a non-empty string argument.");
  }else if(!sepLen){
    /* Split into individual characters */
    cwal_midsize_t slen = 0;
    unsigned char const * pos =
      (unsigned char const *)cwal_string_cstr2(self, &slen);
    cwal_int_t limit;
    assert(pos);
    limit = (args->argc>1)
      ? cwal_value_get_integer(args->argv[1])
      : 0;
    return whcl__cb_str_split_chars(args, rv, pos, slen, limit);
  }else{
    int rc = 0;
    cwal_int_t count = 0;
    cwal_int_t limit;
    cwal_array * ar = NULL;
    cwal_midsize_t slen = 0;
    unsigned char const * pos =
      (unsigned char const *)cwal_string_cstr2(self, &slen);
    unsigned char const * eof = pos + slen;
    unsigned char const * start = pos;
    cwal_value * v;
    limit = (args->argc>1)
      ? cwal_value_get_integer(args->argv[1])
      : 0;
    ar = cwal_new_array(args->engine);
    if(!ar) return CWAL_RC_OOM;
    cwal_value_ref(cwal_array_value(ar));
    if(!slen || (sepLen>slen)){
      rc = cwal_array_append(ar, args->self);
    }
    else while( 1 ){
        if( (pos>=eof)
            || (0==memcmp(sep, pos, sepLen))
            ){
          cwal_size_t sz;
          char last = (pos>=eof);
          if(pos>eof) pos=eof;

          sz = pos-start;
          v = cwal_new_string_value(args->engine,
                                    (char const *)start,
                                    sz);
          cwal_value_ref(v);
          if(!v && sz){
            rc = CWAL_RC_OOM;
            goto end;
          }
          else rc = cwal_array_append(ar, v);
          cwal_value_unref(v);
          v = 0;
          if(rc) goto end;
          if(limit>0 && ++count==limit){
            pos = eof;
            last = 1;
          }

          if(last) goto end;
          /* ++count; */
          pos += sepLen;
          start = pos;
        }else{
          cwal_utf8_read_char( pos, eof, &pos );
        }
      }
    end:
    if(rc && ar){
      cwal_value_unref(cwal_array_value(ar));
    }else if(!rc){
      *rv = cwal_array_value(ar);
      cwal_value_unhand(*rv);
      if(!*rv) rc = CWAL_RC_OOM;
    }
    return rc;
  }
}

/**
   Script usage:

   var newString = aString.replace(needle, replacement [, limit = 0])

   TODO: a variant for Buffers, except that we need different return
   semantics (no returning self, unless we edit the buffer inline).
*/
static int whcl__cb_str_replace( cwal_callback_args const * args,
                              cwal_value **rv ){
  cwal_size_t needleLen = 0;
  unsigned char const * needle;
  cwal_size_t replLen = 0;
  unsigned char const * repl;
  whcl_engine * const el = whcl_engine_from_args( args );
  cwal_string * const self
    = cwal_value_string_part(args->engine, args->self);
  if(!self){
    return cwal_exception_setf( args->engine, CWAL_RC_TYPE,
                                "Expecting a string 'this'." );
  }
  needle = args->argc
    ? (unsigned char const *)cwal_value_get_cstr(args->argv[0], &needleLen)
    : NULL;
  repl = args->argc>1
    ? (unsigned char const *)cwal_value_get_cstr(args->argv[1], &replLen)
    : NULL;
  if(!needle || !repl){
    return cwal_exception_setf( args->engine, CWAL_RC_MISUSE,
                                "Expecting a non-empty string argument.");
  }else if(!needleLen){
    *rv = args->self;
    return 0;
  }else{
    int rc = 0;
    cwal_int_t matchCount = 0;
    cwal_midsize_t slen = 0;
    unsigned char const * pos =
      (unsigned char const *)cwal_string_cstr2(self, &slen);
    unsigned char const * const eof = pos + slen;
    unsigned char const * start = pos;
    cwal_buffer * const buf = &el->escBuf;
    cwal_size_t const oldUsed = buf->used;
    cwal_int_t const limit = args->argc>2
      ? cwal_value_get_integer(args->argv[2])
      : 0;
    if(!slen || (needleLen>slen)){
      *rv = args->self;
      return 0;
    }
    while( 1 ){
      if( (pos>=eof)
          || (0==memcmp(needle, pos, needleLen))
          ){
        cwal_size_t sz;
        char last = (pos>=eof) ? 1 : 0;
        if(!last && matchCount++==limit && limit>0){
          pos = eof;
          last = 1;
        }
        else if(pos>eof) pos=eof;
        sz = pos-start;
        if(sz){
          /* Append pending unmatched part... */
          rc = cwal_buffer_append(args->engine, buf,
                                  start, sz);
        }
        if(!rc && pos<eof && replLen){
          /* Append replacement... */
          rc = cwal_buffer_append(args->engine, buf,
                                  repl, replLen);
        }
        if(rc || last) break;
        pos += needleLen;
        start = pos;
      }else{
        cwal_utf8_read_char( pos, eof, &pos );
      }
    }
    if(!rc){
      if(!matchCount){
        *rv = args->self;
      }else if(buf->used == oldUsed){
        /* replaced the whole string with nothing */
        *rv = cwal_new_string_value(args->engine, "", 0);
      }else{
        assert(buf->used > oldUsed);
        *rv = cwal_new_string_value(args->engine, (char const *)buf->mem,
                                    buf->used - oldUsed);
        if(!*rv) rc = CWAL_RC_OOM;
      }
    }
    buf->used = oldUsed;
    assert( buf->capacity > oldUsed );
    buf->mem[oldUsed] = 0;
    return rc;
  }
}

/**
   Script usage:

   var x = "haystack".indexOf("needle" [, startOffset=0])

   if(x<0) { ...no match found... }
   else { x === index of "needle" in "haystack" }

   If the startOffset is negative, it is counted as the number of
   characters from the end of the haystack.
*/
static int whcl__cb_str_indexof( cwal_callback_args const * args, cwal_value **rv ){
  if(!args->argc){
    misuse:
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting 1 string argument.");
  }else{
    char const * arg;
    cwal_size_t aLen = 0;
    THIS_STRING;
    arg = cwal_value_get_cstr(args->argv[0], &aLen);
    if(!arg) goto misuse;
    else if(!aLen){
      *rv = cwal_new_integer(args->engine, -1);
      return 0;
    }
    else{
      cwal_midsize_t sLen = 0;
      char const * myStr = cwal_string_cstr2(self, &sLen);
      cwal_int_t i = 0;
      cwal_int_t offset;
      if(aLen>sLen){
        *rv = cwal_new_integer(args->engine, -1);
        return 0;
      }
      offset = (args->argc>1)
        ? cwal_value_get_integer(args->argv[1])
        : 0;
      if(!offset && aLen==sLen){
        int const rc = memcmp(myStr, arg, (size_t)sLen);
        *rv = rc
          ? cwal_new_integer(args->engine, -1)
          : cwal_new_integer(args->engine, 0);
        return 0;
      }else{
        i = cwal_utf8_indexof( myStr, sLen, offset, 
                               arg, aLen, 0 );
        *rv = cwal_new_integer(args->engine, i>=0 ? i : -1);
        return *rv ? 0 : CWAL_RC_OOM;
      }
    }
  }
}

static int whcl__cb_str_length( cwal_callback_args const * args, cwal_value **rv ){
  THIS_STRING;
  *rv = cwal_new_integer( args->engine,
                          (cwal_int_t)cwal_string_length_bytes(self) );
  return *rv ? 0 : CWAL_RC_OOM;
}

static int whcl__cb_str_length_utf8( cwal_callback_args const * args, cwal_value **rv ){
  THIS_STRING;
  *rv = cwal_new_integer(args->engine,
                         (cwal_int_t) cwal_string_length_utf8(self) );
  return *rv ? 0 : CWAL_RC_OOM;
}

/**
   substring(offset = 0 [, length=-1])
*/
static int whcl__cb_str_substr( cwal_callback_args const * args, cwal_value **rv ){
  if(!args->argc){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting 1 or 2 integer arguments.");
  }else{
    cwal_int_t offset, len;
    THIS_CSTRING(1);
    offset = cwal_value_get_integer(args->argv[0]);
    len = (args->argc>1)
      ? cwal_value_get_integer(args->argv[1])
      : -1;
    if(len<0){
      len = (cwal_int_t)selfLen;
    }
    if(offset<0){
      offset = (cwal_int_t)selfLen + offset;
      if(offset < 0) offset = 0;
    }
    if(offset>=(cwal_int_t)selfLen){
      *rv = cwal_new_string_value(args->engine, NULL, 0);
      return 0;
    }
    else if((offset+len) > (cwal_int_t)selfLen){
      len = (cwal_int_t)selfLen - offset;
      /* assert(len < sLen); */
      if(len > (cwal_int_t)selfLen) len = (cwal_int_t)selfLen;
    }
    {
      /* Calculate the range/length based on the UTF8
         length. */
      unsigned char const * at = (unsigned char const *)self;
      unsigned char const * eof = at + selfLen;
      unsigned char const * start;
      unsigned char const * end;
      cwal_int_t i = 0;
      for( ; i < offset; ++i ){
        cwal_utf8_read_char( at, eof, &at );
      }
      start = at;
      if(len>=0){
        end = start;
        for( i = 0; (end<eof) && (i<len); ++i ){
          cwal_utf8_read_char( end, eof, &end );
        }
      }else{
        end = eof;
      }
      assert(end >= start);
      len = end - start;
      *rv = cwal_new_string_value(args->engine,
                                   (char const *)start, len);
    }
    return *rv ? 0 : CWAL_RC_OOM;
  }
}

/**
   Impl for trim/trimLeft/trimRight(). mode determines which:

   <0 == left
   0 == both
   >0 == right
*/
static int whcl__cb_str_trim_impl( cwal_callback_args const * args,
                                cwal_value **rv,
                                int mode ){
  THIS_STRING;
  {
    int rc = 0;
    unsigned char const * cs = (unsigned char const *)cwal_string_cstr(self);
    cwal_int_t const len = (cwal_int_t)cwal_string_length_bytes(self);
    unsigned char const * end = cs + len;
    if(!len){
      *rv = args->self;
      return rc;
    }
    if(mode <= 0) for( ; *cs && tok1_is_space((int)*cs); ++cs){}
    if(mode>=0){
      for( --end; (end>cs) && tok1_is_space((int)*end); --end){}
      ++end;
    }
    *rv = ((end-cs) == len)
      ? args->self
      : cwal_new_string_value(args->engine,
                              (char const *)cs, end-cs)
      ;
    return *rv ? 0 : CWAL_RC_OOM;
  }
}

#if 0
static int whcl__cb_str_trim_left( cwal_callback_args const * args,
                                    cwal_value **rv ){
  return whcl__cb_str_trim_impl( args, rv, -1 );
}
static int whcl__cb_str_trim_right( cwal_callback_args const * args,
                                    cwal_value **rv ){
  return whcl__cb_str_trim_impl( args, rv, 1 );
}
static int whcl__cb_str_trim_both( cwal_callback_args const * args,
                                    cwal_value **rv ){
  return whcl__cb_str_trim_impl( args, rv, 0 );
}
#endif

static int whcl__cb_str_trim( cwal_callback_args const * args,
                             cwal_value **rv ){
  uint32_t flags = 0;
  uint16_t ndx = 0;
  while(args->argc > ndx){
    if(whcl_val_is_flag(args->argv[ndx], "-left", 5)){
      flags |= 0x01;
      ++ndx;
    }else if(whcl_val_is_flag(args->argv[ndx], "-right", 6)){
      flags |= 0x02;
      ++ndx;
    }else{
      break;
    }
  }
  return whcl__cb_str_trim_impl( args, rv,
                                (0x01==flags ? -1 : (0x02==flags ? 1 : 0)) );
}

/**
   Internal helper for Buffer.appendf() and String.applyFormat()
*/
static int whcl__helper_appendf( whcl_engine * const el, cwal_buffer * const buf,
                                 char const * fmt,
                                 cwal_size_t fmtLen,
                                 uint16_t argc,
                                 cwal_value * const * argv ){
  cwal_size_t const oldUsed = buf->used;
  int rc = cwal_buffer_format( el->ec, buf, fmt, (cwal_int_t)fmtLen,
                               argc, argv);
  if(rc && (CWAL_RC_OOM != rc)){
    if(buf->used > oldUsed){
      /* Error string is embedded in the buffer... */
      rc = cwal_exception_setf(el->ec, rc, "%s",
                               (char const *)(buf->mem + oldUsed));
      buf->used = oldUsed;
    }else{
      rc = cwal_exception_setf(el->ec, rc,
                               "String formatting failed with code: %s",
                               cwal_rc_cstr(rc));
    }
  }
  return rc;
}

static int whcl__cb_str_apply_format( cwal_callback_args const * args,
                                     cwal_value **rv ){
  char const * fmt;
  cwal_size_t fmtLen;
  cwal_size_t oldUsed;
  cwal_buffer * buf;
  int rc;
  THIS_STRING;
  WHCL__E_ARGS;
  fmt = cwal_string_cstr(self);
  fmtLen = cwal_string_length_bytes(self);
  buf = &el->escBuf;
  oldUsed = buf->used;
  rc = whcl__helper_appendf( el, buf, fmt, fmtLen, args->argc, args->argv);
  if(!rc){
    cwal_size_t const n = buf->used - oldUsed;
    *rv = cwal_new_string_value(args->engine,
                                 ((char const *)buf->mem+oldUsed), n);
    rc = *rv ? 0 : CWAL_RC_OOM;
  }
  buf->used = oldUsed;
  return rc;
}

static int whcl__cb_strish_eval_contents( cwal_callback_args const * args, cwal_value **rv ){
  int rc = 0;
  char const * fname = 0;
  cwal_size_t nameLen = 0, slen = 0;
  bool const catchReturn = 1;
  char const * src;
  whcl_engine * const el = whcl_engine_from_args(args);
  cwal_buffer bufSwap = cwal_buffer_empty;
  cwal_buffer * bufSelf = cwal_value_get_buffer(args->self);
  cwal_value * vars = 0;
  cwal_value * xrv = NULL;
  whcl_scope * scel = NULL;
  /*
    BUG (and fix) REMINDER:

    When this callback is applied to a Buffer, if that buffer's
    contents are modified while this call is active, results are
    undefined. As a partial solution, we move the buffer's contents
    out of the way before evaluation, swapping it back in before
    returning.  That disallows the (corner case) possibility of
    recursion, but it gives us predictable/safe results.
  */
  assert(el);
  src = cwal_value_get_cstr(args->self, &slen)
    /* Corner case reminder: that won't work if args->self
       _derives_ from a Buffer */;
  if(!src){
    if(cwal_value_buffer_part(args->engine, args->self)){
      *rv = cwal_value_undefined();
      return 0;
    }else{
      return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                "'this' type (%s) is not valid for "
                                 "this function.",
                                cwal_value_type_name(args->self));
    }
  }
  if(!slen){
    /* empty buffer/string */
    *rv = cwal_value_undefined();
    return 0;
  }
  if(2==args->argc){
    /*
      Accept (string,object) or (object,string)
    */
    if(cwal_props_can(args->argv[0])){
      vars = args->argv[0];
      fname = cwal_value_get_cstr(args->argv[1], &nameLen);
    }else{
      fname = cwal_value_get_cstr(args->argv[0], &nameLen);
      if(cwal_props_can(args->argv[1])) vars = args->argv[1];
    }
  }else if(args->argc){
    /* Accept (string) or (object) */
    fname = cwal_value_get_cstr(args->argv[0], &nameLen);
    if(!fname && cwal_props_can(args->argv[0])){
      vars = args->argv[0];
    }
  }
  if(!fname){
    fname = cwal_value_type_name(args->self);
    assert(fname);
    nameLen = cwal_strlen(fname);
  }
  if(!(scel = whcl__scope_push(el, 0))) return CWAL_RC_OOM;
  if(vars){
    rc = whcl__scope_import_props( el, scel, vars );
    if(rc){
      //rc = cwal_cb_throw(args, rc, "Import of vars for evaluation failed.");
      assert(cwal_engine_error_get(args->engine, NULL, NULL));
      rc = cwal_error_throw(args->engine, NULL, NULL, 0, 0);
      goto end;
    }
  }
  if(bufSelf){
    /* Move bufSelf->mem out of the way in case this eval modifies
       bufSelf, because we'd otherwise have undefined behaviour.
       If bufSelf is modified by the eval, any modifications to it
       are discarded after the eval. That's probably the safest option
       for how to handle that corner case.
    */
    cwal_buffer_swap_mem(bufSelf, &bufSwap);
  }
  xrv = cwal_value_undefined()
    /* make sure it's populated to avoid an assertion failure below */;
  rc = whcl_eval_cstr(el, false, fname, src, (cwal_int_t)slen, &xrv);
  end:
  if(bufSelf && bufSwap.mem){
    /* restore bufSelf's memory */
    cwal_buffer_swap_mem(bufSelf, &bufSwap);
    cwal_buffer_clear(args->engine, &bufSwap);
  }
  if(catchReturn && CWAL_RC_RETURN==rc){
    xrv = cwal_propagating_take(args->engine);
    assert(xrv && "Else RETURN semantics have been violated.");
    rc = 0;
  }else if(CWAL_RC_EXCEPTION==rc){
    assert(cwal_exception_get(args->engine));
  }
  if(0==rc){
    assert((xrv
           ? ((cwal_value_is_builtin(xrv) || cwal_value_scope(xrv)))
            : 1)
           || "lifetime check failed! cwal memory mismanagement!");
    if(!xrv) xrv = cwal_value_undefined();
    *rv = xrv;
  }
  if(scel) whcl_scope_pop(el, scel, rc ? NULL : xrv);
  return rc;
}

cwal_value * whcl_prototype_string( whcl_engine * const el ){
  int rc = 0;
  cwal_value * proto;
  proto = cwal_prototype_base_get( el->ec, CWAL_TYPE_STRING );
  if(proto
     || !whcl_prototype_object(el) /* timing hack */
     ) return proto;
  proto = cwal_new_object_value(el->ec);
  if(!proto){
    rc = CWAL_RC_OOM;
    goto end;
  }
  cwal_value_prototype_set(proto, 0 /* so we don't inherit Object!*/);
  rc = cwal_prototype_base_set(el->ec, CWAL_TYPE_STRING, proto );
  if(0==rc) rc = whcl__prototype_stash(el, "String", proto);
  if(rc) goto end;
  assert(proto == cwal_prototype_base_get(el->ec, CWAL_TYPE_STRING));
  //MARKER(("Setting up STRING prototype.\n"));
  const whcl_func_def funcs[] = {
    WHCL_FUNC2("apply-format", whcl__cb_str_apply_format),
    WHCL_FUNC2("byte-at", whcl__cb_str_byte_at),
    WHCL_FUNC2("char-at", whcl__cb_str_char_at),
    WHCL_FUNC2("concat", whcl__cb_str_concat),
    /* WHCL_FUNC2("+", whcl__cb_str_concat),
       ^^^ that works but may confuse folks into thinking it's a +
       _operator_, which it's not. The `expr` + does not handle
       strings and i removed the operator overloading bits when
       porting over the operators. Adding them to whcl just felt
       like overkill and unnecessary complication for this simple
       language.
    */
    WHCL_FUNC2("compare", whcl_cb_value_compare),
    WHCL_FUNC2("eval-contents", whcl__cb_strish_eval_contents),
    WHCL_FUNC2("glob-matches", whcl_cb_glob_matches_str),
    WHCL_FUNC2("matches-glob", whcl_cb_str_matches_glob),
    WHCL_FUNC2("index-of", whcl__cb_str_indexof),
    WHCL_FUNC2("is-ascii", whcl__cb_str_isascii),
    WHCL_FUNC2("length", whcl__cb_str_length_utf8),
    WHCL_FUNC2("length-bytes", whcl__cb_str_length),
    WHCL_FUNC2("replace", whcl__cb_str_replace),
    WHCL_FUNC2("split", whcl__cb_str_split),
    WHCL_FUNC2("substr", whcl__cb_str_substr),
    WHCL_FUNC2("to-lower", whcl__cb_str_tolower),
    WHCL_FUNC2("to-json", whcl_cb_this_to_json_token),
    WHCL_FUNC2("to-string", whcl__cb_str_to_string),
    WHCL_FUNC2("to-upper", whcl__cb_str_toupper),
    WHCL_FUNC2("trim", whcl__cb_str_trim),
#if 0
    // TODO
    WHCL_FUNC2("unescape", whcl__cb_str_unescape_c),
#endif
    whcl_func_def_empty_m
  };
  rc = whcl_install_functions(el, proto, funcs, 0);
  if(rc) goto end;
  rc = whcl_install_command_cb(el, proto);
  if(rc) goto end;

  end:
  return rc ? NULL : proto;
}

#define THIS_BUFFER                                               \
  cwal_buffer * self = 0;                                         \
  if(!(self = cwal_value_buffer_part(args->engine, args->self))){   \
    return cwal_exception_setf( args->engine, CWAL_RC_TYPE,  \
                                "'this' is-not-a Buffer." ); \
  } (void)0


/**
   Buffer slice([offset=0 [,count=0]])

   where count==0 means "to the end".
*/
static int whcl__cb_buffer_slice( cwal_callback_args const * args, cwal_value **rv ){
  cwal_int_t offset = 0, count = -1;
  cwal_buffer * bcp = NULL;
  cwal_value * bv = NULL;
  int rc;
  THIS_BUFFER;
  offset = args->argc
    ? cwal_value_get_integer(args->argv[0])
    : 0;
  count = args->argc>1
    ? cwal_value_get_integer(args->argv[1])
    : -1;
  if(offset<0){
    return cwal_exception_setf(args->engine, CWAL_RC_RANGE,
                               "Slice offset must be 0 or higher.");
  }
  if(offset > (cwal_int_t)self->used){
      offset = (cwal_int_t)self->used;
  }
  if(count < 0) count = self->used;
  if((cwal_size_t)(offset + count) >= self->used){
    count = self->used - offset;
  }
  bcp = cwal_new_buffer(args->engine, count ? count+1 : 0 );
  if(!bcp) return CWAL_RC_OOM;
  bv = cwal_buffer_value(bcp);
  cwal_value_ref(bv);
  rc = cwal_buffer_append(args->engine, bcp,
                          self->mem + offset, (cwal_size_t)count);
  if(rc) cwal_value_unref(bv);
  else {
    cwal_value_unhand(bv);
    *rv = bv;
  }
  return rc;
}


/**
   Script usage:

   assert aBuffer === aBuffer.replace(needle, replacement [, limit = 0])

   (needle, replacement) must be (string, string) or (byte, byte),
   where byte means integer in the range 0..255. Potential TODO:
   change 'byte' to mean "UTF codepoint". That's currently easier to do
   in script code: b.replace("...", 0x00a9.toChar())

   Potential TODO: replace(buffer needle,buffer replacement), which
   replaces arbitrary byte-range matches (as opposed to UTF8 chars).
*/
static int whcl__cb_buffer_replace( cwal_callback_args const * args,
                                    cwal_value **rv ){
  cwal_size_t needleLen = 0;
  unsigned char const * needle;
  cwal_size_t replLen = 0;
  unsigned char const * repl;
  cwal_buffer * self;
  static const char * usageError =
    "Expecting (string,string) or (int, int) arguments.";
  //ARGS_SE;
  self = cwal_value_buffer_part(args->engine, args->self);
  if(!self){
    return cwal_exception_setf( args->engine, CWAL_RC_TYPE,
                                "Expecting a Buffer 'this'." );
  }
  needle = args->argc
    ? (unsigned char const *)cwal_value_get_cstr(args->argv[0],
                                                 &needleLen)
    : NULL;
  repl = args->argc>1
    ? (unsigned char const *)cwal_value_get_cstr(args->argv[1],
                                                 &replLen)
    : NULL;
  if(!needle && args->argc && cwal_value_is_integer(args->argv[0])){
    /* replace(byte, byte) */
    cwal_int_t const needleByte =
      cwal_value_get_integer(args->argv[0]);
    cwal_int_t const replByte =
      (args->argc>1 && cwal_value_is_integer(args->argv[1]))
      ? (cwal_value_get_integer(args->argv[1]))
      : -1;
    if(replByte<0){
      return cwal_exception_setf( args->engine, CWAL_RC_MISUSE, "%s", usageError);
    }else if((needleByte & ~0xFF) || (replByte & ~0xFF)) {
      return cwal_exception_setf( args->engine, CWAL_RC_RANGE,
                         "Byte values must be in the range [0,255]." );
    }else{
      cwal_int_t const limit = args->argc>2
        ? cwal_value_get_integer(args->argv[2])
        : 0;
      unsigned char const bN = (unsigned char)(needleByte & 0xFF);
      unsigned char const bR = (unsigned char)(replByte & 0xFF);
      int const rc =
        cwal_buffer_replace_byte(args->engine, self, bN, bR,
                                 (cwal_size_t)(limit>=0
                                               ? (cwal_size_t)limit : 0U),
                                 NULL);
      assert(!rc && "There are no error cases here.");
      if(rc){/*avoid unused param warning*/}
      *rv = args->self;
      return 0;
    }    
  }else if(!needle || !repl){
    return cwal_exception_setf( args->engine, CWAL_RC_MISUSE,
                                "%s", usageError);
  }else if(!needleLen){
    return cwal_exception_setf( args->engine, CWAL_RC_RANGE,
                        "Needle length must be >0.");
  }else if(!self->used || (needleLen>self->used)){
    /* Nothing to do. */
    *rv = args->self;
    return 0;
  }else{
    int rc = 0;
    /* replace(needle, replacement) ... */
    cwal_int_t const limit = args->argc>2
      ? cwal_value_get_integer(args->argv[2])
      : 0;
    if(!needle || !repl){
      rc = cwal_exception_setf( args->engine, CWAL_RC_MISUSE,
                                "%s", usageError);
    }else{
      rc = cwal_buffer_replace_str(args->engine, self, needle,
                                   needleLen, repl, replLen,
                                   (limit>=0 ? (cwal_size_t)limit : 0U),
                                   NULL);
      if(rc){
        rc = cwal_exception_setf( args->engine, rc,
                                  "cwal_buffer_replace_str() "
                                  "failed with code %s.",
                                  cwal_rc_cstr(rc));
      }else{
        *rv = args->self;
      }
    }
    return rc;
  }
}

/**
   Callback handler for buffer.length() (if isCapacity is false) and
   buffer.capacity (if isCapacity is true).
*/
static int whcl__cb_buffer_length_uc( cwal_callback_args const * args,
                                       cwal_value **rv,
                                       char isCapacity){
  THIS_BUFFER;
  if(args->argc>0){
    /* SET length */
    cwal_value * const index = args->argv[0];
    cwal_int_t len = cwal_value_get_integer(index);
    int rc;
    if((len<0) || !cwal_value_is_number(index)){
      return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                 "%s argument must be "
                                 "non-negative integer.",
                                 isCapacity ? "capacity" : "length");
    }
    if(isCapacity || ((cwal_size_t)len > self->capacity)){
      rc = cwal_buffer_reserve(args->engine, self, (cwal_size_t)len );
      if(rc){
        return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                   "Setting buffer length to %"CWAL_INT_T_PFMT
                                   " failed with code %d (%s).",
                                   len, rc, cwal_rc_cstr(rc));
      }
    }
    if(!isCapacity) self->used = (cwal_size_t)len;
    assert(self->used <= self->capacity);
    *rv = args->self;
  }else{
    *rv = cwal_new_integer( args->engine,
                            (cwal_int_t)
                            (isCapacity ? self->capacity : self->used)
                            );
  }
  return *rv ? 0 : CWAL_RC_OOM;
}


static int whcl__cb_buffer_length_u( cwal_callback_args const * args, cwal_value **rv ){
  return whcl__cb_buffer_length_uc( args, rv, 0 );
}

static int whcl__cb_buffer_length_c( cwal_callback_args const * args, cwal_value **rv ){
  return whcl__cb_buffer_length_uc( args, rv, 1 );
}

static int whcl__cb_buffer_is_empty( cwal_callback_args const * args, cwal_value **rv ){
  THIS_BUFFER;
  *rv = cwal_new_bool(self->used>0 ? 0 : 1);
  return 0;
}

static int whcl__cb_buffer_length_utf8( cwal_callback_args const * args, cwal_value **rv ){
  cwal_size_t ulen;
  THIS_BUFFER;
  ulen = cwal_strlen_utf8((char const *)self->mem, self->used);
  *rv = cwal_new_integer(args->engine, (cwal_int_t) ulen);
  return *rv ? 0 : CWAL_RC_OOM;
}


static int whcl__cb_buffer_append( cwal_callback_args const * args, cwal_value **rv ){
  uint16_t i;
  int rc = 0;
  THIS_BUFFER;
  WHCL__E_ARGS;
  for( i = 0; !rc && (i < args->argc); ++i ){
    rc = whcl_value_to_buffer(el, self, args->argv[i]);
  }
  if(!rc) *rv = args->self;
  return rc;
}

static int whcl__cb_buffer_appendf( cwal_callback_args const * args, cwal_value **rv ){
  int rc = 0;
  cwal_size_t fmtlen;
  char const * fmt;
  THIS_BUFFER;
  WHCL__E_ARGS;
  fmt = (args->argc>0)
    ? cwal_value_get_cstr(args->argv[0], &fmtlen)
    : NULL;
  if((!args->argc) || !fmt){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                    "Expecting (String,...) arguments.");
  }
  rc = whcl__helper_appendf(el, self, fmt, fmtlen, args->argc-1, args->argv+1);
  if(!rc){
    *rv = args->self;
  }
  return rc;
}

static int whcl__cb_buffer_ctor( cwal_callback_args const * args,
                                 cwal_value **rv ){
  cwal_int_t len;
  if(cwal_value_buffer_part(args->engine, args->self)){
    /* assert(!"This doesn't seem to be possible any more."); */
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Do not call a buffer as a function.");
  }
  len = args->argc
    ? cwal_value_get_integer(args->argv[0])
    : 0;
  if(len<0) len = 0;
  *rv = cwal_new_buffer_value(args->engine, (cwal_size_t) len);
  return *rv ? 0 : CWAL_RC_OOM;
}


static int whcl__cb_buffer_take_string( cwal_callback_args const * args, cwal_value **rv ){
  THIS_BUFFER;
  *rv = self->mem
    ? cwal_string_value( cwal_buffer_to_zstring(args->engine, self) )
    : cwal_value_null();
  return *rv ? 0 : CWAL_RC_OOM;
}

static int whcl__cb_buffer_to_string( cwal_callback_args const * args, cwal_value **rv ){
  char const * begin = 0;
  char const * end = 0;
  THIS_BUFFER;
  if(!self->mem){
    *rv = cwal_value_null();
    return 0;
  }
  if(args->argc){
    cwal_int_t off, len;
    if(!cwal_value_is_number(args->argv[0])) goto misuse;
    off = cwal_value_get_integer(args->argv[0]);
    if(args->argc>1){
      if(!cwal_value_is_number(args->argv[1])) goto misuse;
      len = cwal_value_get_integer(args->argv[1]);
    }else{
      len = off;
      off = 0;
    }
    if(off<0 || len<0) goto misuse;
    begin = (char const *)self->mem + off;
    end = begin + len;
    if(end>((char const *)self->mem+self->used)){
      end = (char const *)self->mem + self->used;
    }
  }
  else {
    begin = (char const *)self->mem;
    end = begin + self->used;
  }
  *rv = cwal_new_string_value(args->engine, begin,
                               (cwal_size_t)(end - begin) );
  return *rv ? 0 : CWAL_RC_OOM;
  misuse:
  return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                             "Buffer to-string arguments must 0 or "
                             "positive integers.");
}

static int whcl__cb_buffer_reset( cwal_callback_args const * args, cwal_value **rv ){
  THIS_BUFFER;
  self->used = 0;
  memset(self->mem, 0, self->capacity);
  *rv = args->self;
  return 0;
}

static int whcl__cb_buffer_resize( cwal_callback_args const * args, cwal_value **rv ){
  cwal_int_t n;
  THIS_BUFFER;
  n = args->argc>0
    ? cwal_value_get_integer(args->argv[0])
    : -1;
  if(n<0){
    return cwal_exception_setf(args->engine, CWAL_RC_RANGE,
                               "Expecting an integer value 0 or larger.");
  }else{
    int const rc = cwal_buffer_resize(args->engine, self, (cwal_size_t)n );
    if(!rc) *rv = args->self;
    return rc;
  }
}

static cwal_int_t whcl__get_first_cstr_byte(cwal_value const *v){
  char const * cstr = cwal_value_get_cstr(v, NULL);
  return cstr ? *cstr : cwal_value_get_integer(v);
}

static int whcl__cb_buffer_byte_at( cwal_callback_args const * args,
                                    cwal_value **rv ){
  THIS_BUFFER;
  if(!args->argc || !cwal_value_is_number(args->argv[0])){
    return cwal_exception_setf( args->engine, CWAL_RC_MISUSE,
                                "Expecting one integer argument.");
  }
  else {
    cwal_value * const index = args->argv[0];
    cwal_int_t pos = cwal_value_get_integer(index);
    if(pos<0){
      return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                 "Byte position argument must be "
                                 "non-negative integer.");
    }else if(args->argc<2){
      *rv = (pos>=(cwal_int_t)self->used)
        ? cwal_value_undefined()
        : cwal_new_integer(args->engine, self->mem[pos])
        ;
      return *rv ? 0 : CWAL_RC_OOM;
    }else{
      int const rc = cwal_buffer_reserve( args->engine,
                                          self,
                                          (cwal_size_t)pos+1);
      if(rc) return rc;
      self->mem[pos] = 0xFF & whcl__get_first_cstr_byte(args->argv[1]);
      *rv = cwal_value_true();
      if(self->used <= (cwal_size_t)pos) self->used = (cwal_size_t)pos+1;
      return 0;
    }
  }
}

static int whcl__cb_buffer_file_read( cwal_callback_args const * args,
                                      cwal_value **rv ){
  int rc;
  char const * fname;
  cwal_buffer * newBuf = NULL;
  cwal_buffer * const self = cwal_value_buffer_part(args->engine, args->self); 
  // maybe TODO: if( (rc = s2_cb_disable_check(args, S2_DISABLE_FS_READ)) ) return rc;
  fname = args->argc
    ? cwal_string_cstr(cwal_value_get_string(args->argv[0]))
    : NULL;
  if(!fname){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting a filename argument.");
  }
  if(!self){
    newBuf = cwal_new_buffer(args->engine, 0);
    if(!newBuf){
      WHCL__WARN_OOM;
      return CWAL_RC_OOM;
    }
    cwal_ref(cwal_buffer_value(newBuf));
  }
  rc = cwal_buffer_fill_from_filename( args->engine,
                                       newBuf ? newBuf : self, fname );
  if(rc){
    if(newBuf) cwal_value_unref(cwal_buffer_value(newBuf));
    assert(CWAL_RC_EXCEPTION!=rc);
    rc = cwal_exception_setf(args->engine, rc,
                  "Reading file [%s] failed.",
                  fname);
  }else {
    *rv = newBuf ? cwal_buffer_value(newBuf) : args->self;
    if(newBuf) cwal_unhand(*rv);
  }
  return rc;
}

static int whcl__cb_buffer_file_write( cwal_callback_args const * args,
                                       cwal_value **rv ){
  int rc = 0;
  char const * fname;
  cwal_size_t nameLen = 0;
  THIS_BUFFER;
  // maybe TODO: rc = s2_cb_disable_check(args, S2_DISABLE_FS_WRITE);
  // if(rc) return rc;
  fname = args->argc
    ? cwal_value_get_cstr(args->argv[0], &nameLen)
    : NULL;
  if(!fname){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                    "Expecting a filename argument.");
  }
  else{
    FILE * f;
    bool const append = (args->argc>1)
      ? cwal_value_get_bool(args->argv[1])
      : 0;
    f = whcl_fopen( fname, append ? "a" : "w" );
    if(!f){
      if(errno){
        rc = cwal_exception_setf(args->engine,
                                 cwal_errno_to_cwal_rc(errno, CWAL_RC_IO),
                                 "Could not open file [%.*s] "
                                 "in %s mode. "
                                 "errno=%d: %s",
                                 (int)nameLen, fname,
                                 append ? "append" : "truncate/write",
                                 errno, strerror(errno));
      }else{
        rc = cwal_exception_setf(args->engine,
                                 cwal_errno_to_cwal_rc(errno, CWAL_RC_IO),
                                 "Could not open file [%.*s] "
                                 "in %s mode.",
                                 (int)nameLen, fname,
                                 append ? "append" : "truncate/write");
      }
    }else{
      if(self->used &&
         1 != fwrite(self->mem, self->used, 1, f)){
        rc = cwal_exception_setf(args->engine, errno
                                 ? cwal_errno_to_cwal_rc(errno, CWAL_RC_IO)
                                 : CWAL_RC_IO,
                                 "Error writing %"CWAL_SIZE_T_PFMT" byte(s) "
                                 "to file [%.*s].",
                                 self->used, (int)nameLen, fname);
      }
      whcl_fclose(f);
    }
    if(!rc) *rv = args->self;
    return rc;
  }
}

/**
    Script usage:

    buffer.fill( Byte [offset=0 [,length=0]])

    Byte may be an integer or string (in which case the first byte
    is used, even if it is a NUL byte). By default it fills the whole
    buffer with the byte, but the offset and length may be specified.
    It _will_not_ expand the buffer. If the offset/length are out of range
    this is a no-op.
*/
static int whcl__cb_buffer_fill( cwal_callback_args const * args,
                                 cwal_value **rv ){
  cwal_int_t byte;
  THIS_BUFFER;
  if(!args->argc){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                    "Expecting a byte value argument.");
  }
  byte = 0xFF & whcl__get_first_cstr_byte(args->argv[0]);
  if(1 == args->argc){
    cwal_buffer_fill(self, (unsigned char)byte);
    *rv = args->self;
    return 0;
  }else{
    cwal_int_t offset;
    cwal_int_t len;
    cwal_size_t const limit = 1 ? self->used : self->capacity;
    offset = cwal_value_get_integer(args->argv[1]);
    len = (args->argc>2)
      ? cwal_value_get_integer(args->argv[2])
      : 0;
    if((offset<0) || (len<0)){
      return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                      "Neither offset nor length may be negative.");
    }
    if(0==len){
      len = ((cwal_int_t)limit <= offset)
        ? 0
        : ((cwal_int_t)limit - offset);
    }
    if( (len>0) && ((cwal_size_t)offset < limit) ) {
      if((cwal_size_t)(offset+len) > limit){
        len = (cwal_int_t)(limit - (cwal_size_t)offset);
      }
      /*MARKER(("Filling %d byte(s) at offset %d to 0x%02x\n", (int)len, (int)offset, (int)byte));*/
      if(len>0){
        memset( self->mem+offset, byte, (size_t)len );
      }
    }
    *rv = args->self;
    return 0;
  }
}

cwal_value * whcl_prototype_buffer(whcl_engine * const el){
  int rc = 0;
  cwal_value * ctor;
  cwal_value * proto;
  proto = cwal_prototype_base_get( el->ec, CWAL_TYPE_BUFFER );
  if(proto
     || !whcl_prototype_object(el) /* timing hack */
     ) return proto;
  ctor = cwal_new_function_value(el->ec, whcl__cb_buffer_ctor, NULL,
                                 NULL, NULL);
  proto = ctor ? cwal_new_object_value(el->ec) : NULL;
  if(!ctor){
    WHCL__WARN_OOM;
    rc = CWAL_RC_OOM;
    goto end;
  }
  cwal_ref(proto);
  cwal_ref(ctor);
  cwal_value_prototype_set(ctor, proto);
  rc = whcl__install_into_whcl(el, "Buffer", ctor);
  cwal_unref(ctor) /* on success ^^^, the core now owns a ref */;
  if(rc) goto end;
  rc = cwal_prototype_base_set(el->ec, CWAL_TYPE_BUFFER, proto );
  if(!rc) rc = whcl__prototype_stash(el, "Buffer", proto);
  if(rc) goto end;
  assert(proto == cwal_prototype_base_get(el->ec, CWAL_TYPE_BUFFER));

  {
    whcl_func_def const funcs[] = {
      WHCL_FUNC2("append", whcl__cb_buffer_append),
      WHCL_FUNC2("append-json", whcl_cb_this_to_json_token),
      WHCL_FUNC2("appendf", whcl__cb_buffer_appendf),
      WHCL_FUNC2("capacity", whcl__cb_buffer_length_c),
      WHCL_FUNC2("eval-contents", whcl__cb_strish_eval_contents),
      WHCL_FUNC2("is-empty", whcl__cb_buffer_is_empty),
      WHCL_FUNC2("length", whcl__cb_buffer_length_u),
      WHCL_FUNC2("length-utf8", whcl__cb_buffer_length_utf8),
      //WHCL_FUNC2("new", whcl__cb_buffer_ctor),
      WHCL_FUNC2("replace", whcl__cb_buffer_replace),
      WHCL_FUNC2("slice", whcl__cb_buffer_slice),
      WHCL_FUNC2("reset", whcl__cb_buffer_reset),
      WHCL_FUNC2("resize", whcl__cb_buffer_resize),
      WHCL_FUNC2("take-string", whcl__cb_buffer_take_string),
      WHCL_FUNC2("to-string", whcl__cb_buffer_to_string),
      WHCL_FUNC2("byte-at", whcl__cb_buffer_byte_at),
      WHCL_FUNC2("read-file", whcl__cb_buffer_file_read),
      WHCL_FUNC2("write-file", whcl__cb_buffer_file_write),
      WHCL_FUNC2("fill", whcl__cb_buffer_fill),
      WHCL_FUNC2("substr", whcl__cb_str_substr),
#if 0
      // TODO (port from s2)
      WHCL_FUNC2("is-compressed", whcl__cb_buffer_is_compressed),
      WHCL_FUNC2("compress", whcl__cb_buffer_compress),
      WHCL_FUNC2("uncompress", whcl__cb_buffer_uncompress),
      WHCL_FUNC2("uncompressed-size", whcl__cb_buffer_uncompressed_size),
#endif
    whcl_func_def_empty_m
    };
    rc = whcl_install_functions(el, proto, funcs, 0);
    if(rc) goto end;
  }
  end:
  cwal_unref(proto)/*on success it was stashed*/;
  return rc ? NULL : proto;
}


#undef MARKER
#undef THIS_STRING
#undef THIS_CSTRING
#undef THIS_BUFFER
#undef WHCL__E_ARGS
