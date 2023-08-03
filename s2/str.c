/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "s2_internal.h"

#if !defined(S2_ENABLE_ZLIB)
#  define S2_ENABLE_ZLIB 0
#endif

#if S2_ENABLE_ZLIB
#  include <zlib.h>
#endif

#if 1
#include <stdio.h>
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__); if(1) printf pfexp
#else
#define MARKER(pfexp) (void)0
#endif

char s2_is_compressed(unsigned char const * mem, cwal_size_t len){
  if(!mem || (len<6)) return 0;
  else{
    /**
       Adapted from:
       
       http://blog.2of1.org/2011/03/03/decompressing-zlib-images/

       Remember that s2-compressed data has a 4-byte big-endian header
       holding the uncompressed size of the data, so we skip those
       first 4 bytes.

       See also:

       http://tools.ietf.org/html/rfc6713

       search for "magic number".
    */
    int16_t const head = (((int16_t)mem[4]) << 8) | mem[5];
    /* MARKER(("isCompressed header=%04x\n", head)); */
    switch(head){
      case 0x083c: case 0x087a: case 0x08b8: case 0x08f6:
      case 0x1838: case 0x1876: case 0x18b4: case 0x1872:
      case 0x2834: case 0x2872: case 0x28b0: case 0x28ee:
      case 0x3830: case 0x386e: case 0x38ac: case 0x38ea:
      case 0x482c: case 0x486a: case 0x48a8: case 0x48e6:
      case 0x5828: case 0x5866: case 0x58a4: case 0x58e2:
      case 0x6824: case 0x6862: case 0x68bf: case 0x68fd:
      case 0x7801: case 0x785e: case 0x789c: case 0x78da:
        return 1;
      default:
        return 0;
    }
  }
}

char s2_buffer_is_compressed(cwal_buffer const *buf){
  return buf
    ? s2_is_compressed( buf->mem, buf->used )
    : 0;
}

uint32_t s2_uncompressed_size(unsigned char const *mem,
                              cwal_size_t len){
  return s2_is_compressed(mem,len)
    ? (uint32_t)((mem[0]<<24) + (mem[1]<<16) + (mem[2]<<8) + mem[3])
    : (uint32_t)-1;
}

uint32_t s2_buffer_uncompressed_size(cwal_buffer const * b){
  return b
    ? s2_uncompressed_size(b->mem, b->used)
    : (uint32_t)-1;
}

void s2_buffer_swap( cwal_buffer * left, cwal_buffer * right ){
  void * const self1 = left->self, * const self2 = right->self
    /* cwal_value parts of script-side buffers */;
  cwal_buffer const tmp = *left;
  assert(left && right);
  assert(left != right);
  *left = *right;
  *right = tmp;
  left->self = self1;
  right->self = self2;
}

#if S2_ENABLE_ZLIB
/**
   calls s2_buffer_swap(left, right), then...

   clearWhich == 0 means clear neither, <0 means clear the left, >0
   means clear the right.
*/
static void s2_buffer_swap_clear( cwal_engine * e,
                                  cwal_buffer * left,
                                  cwal_buffer * right,
                                  int clearWhich ){
  s2_buffer_swap(left, right);
  if(0 != clearWhich) cwal_buffer_clear(e,
                                        (clearWhich<0) ? left : right);
}
#endif
/* ^^^ S2_ENABLE_ZLIB */


int s2_buffer_compress(cwal_engine * e, cwal_buffer const *pIn,
                       cwal_buffer *pOut){
#if !S2_ENABLE_ZLIB
  if(e || pIn || pOut){/*avoid unused param warning*/}
  return CWAL_RC_UNSUPPORTED;
#else
#define N_OUT (13 + nIn + (nIn+999)/1000)
  unsigned int nIn = (unsigned int)pIn->used;
  unsigned int nOut = N_OUT;
  cwal_buffer temp = cwal_buffer_empty;
  int rc;
#if CWAL_SIZE_T_BITS > 32
  if( (pIn->used != (cwal_size_t)nIn)
      || ((cwal_size_t)nOut != (cwal_size_t)N_OUT)){
    /* Trying to defend against a cwal_size_t too big
       for unsigned int. */
    return CWAL_RC_RANGE;
  }else
#endif
  if(s2_buffer_is_compressed(pIn)){
    if(pIn == pOut) return 0;
    else{
      cwal_buffer_reset(pOut);
      return cwal_buffer_append(e, pOut, pIn->mem, pIn->used);
    }
  }
#undef N_OUT
  rc = cwal_buffer_resize(e, &temp, nOut+4);
  if(rc) return rc;
  else{
    unsigned long int nOut2;
    unsigned char *outBuf;
    unsigned long int outSize;
    outBuf = temp.mem;
    outBuf[0] = nIn>>24 & 0xff;
    outBuf[1] = nIn>>16 & 0xff;
    outBuf[2] = nIn>>8 & 0xff;
    outBuf[3] = nIn & 0xff;
    nOut2 = (long int)nOut;
    rc = compress(&outBuf[4], &nOut2,
                  pIn->mem, pIn->used);
    if(rc){
      cwal_buffer_clear(e, &temp);
      return CWAL_RC_ERROR;
    }
    outSize = nOut2+4;
    rc = cwal_buffer_resize(e, &temp, outSize);
    if(rc){
      cwal_buffer_clear(e, &temp);
    }else{
      s2_buffer_swap_clear(e, &temp, pOut, -1);
      assert(0==temp.used);
      assert((cwal_size_t)outSize==pOut->used);
    }
    return rc;
  }
#endif
/* S2_ENABLE_ZLIB */
}

int s2_buffer_uncompress(cwal_engine * e, cwal_buffer const *pIn,
                         cwal_buffer *pOut){
#if !S2_ENABLE_ZLIB
  if(e || pIn || pOut){/*avoid unused param warning*/}
  return CWAL_RC_UNSUPPORTED;
#else
  unsigned int nOut;
  unsigned char *inBuf;
  unsigned int nIn = (unsigned int)pIn->used;
  cwal_buffer temp = cwal_buffer_empty;
  int rc;
  unsigned long int nOut2;
  if( nIn<=4 ){
    return CWAL_RC_RANGE;
  }else if(pIn == pOut && !s2_buffer_is_compressed(pIn)){
    return 0;
  }
  inBuf = pIn->mem;
  nOut = (inBuf[0]<<24) + (inBuf[1]<<16) + (inBuf[2]<<8) + inBuf[3];
  /* MARKER(("decompress size: %u\n", nOut)); */
  rc = cwal_buffer_reserve(e, &temp, nOut+1);
  if(rc) return rc;
  nOut2 = (long int)nOut;
  rc = uncompress(temp.mem, &nOut2,
                  &inBuf[4], nIn - 4)
    /* valgrind says there's an uninitialized memory access
       somewhere under uncompress(), _presumably_ for one of
       these arguments, but i can't find it. fsl_buffer_reserve()
       always memsets() new bytes to 0.

       Turns out it's a known problem:

       http://www.zlib.net/zlib_faq.html#faq36
    */;
  if( rc ){
    cwal_buffer_clear(e, &temp);
    return CWAL_RC_ERROR;
  }
  rc = cwal_buffer_resize(e, &temp, nOut2);
  if(!rc){
    void * const self = pOut->self
      /* cwal_value part of script-side buffers */;
    temp.used = (cwal_size_t)nOut2;
    if( pOut==pIn ){
      cwal_buffer_clear(e, pOut);
    }
    assert(!pOut->mem);
    *pOut = temp;
    pOut->self = self;
  }else{
    cwal_buffer_clear(e, &temp);
  }
  return rc;
#endif
/* S2_ENABLE_ZLIB */
}


/*
  Various helper macros for callbacks...
*/

#define ARGS_SE s2_engine * se = s2_engine_from_args(args)

#define THIS_STRING                                                    \
  cwal_string * self; ARGS_SE;                                         \
  if(!se) return CWAL_RC_MISUSE;                                        \
  if(!(self=cwal_value_string_part(args->engine, args->self)) && args->argc) \
    self = cwal_value_string_part(args->engine, args->argv[0]);         \
  if(!self){                                                            \
    return cwal_exception_setf( args->engine, CWAL_RC_TYPE,             \
                                "Expecting a string 'this' or first argument." ); \
  } (void)0
#define THIS_CSTRING(USE_EMPTY_FOR_BUFFER)                              \
  char const * self; cwal_size_t selfLen; ARGS_SE;                      \
  if(!se) return CWAL_RC_MISUSE;                                        \
  else if(!(self=cwal_value_get_cstr(args->self, &selfLen))){          \
    if(USE_EMPTY_FOR_BUFFER && cwal_value_get_buffer(args->self)){        \
      self = ""; selfLen = 0;                                             \
    }else {                                                               \
      return cwal_exception_setf( args->engine, CWAL_RC_TYPE,             \
                                  "Expecting a string or buffer 'this' argument." ); \
    }\
  } (void)0


#define THIS_BUFFER                                             \
  cwal_buffer * self; ARGS_SE;                                  \
  if(!se) return CWAL_RC_MISUSE;                                \
  else if(!(self=cwal_value_buffer_part(args->engine, args->self))){ \
    return cwal_exception_setf( args->engine, CWAL_RC_TYPE,      \
                                "'this' is-not-a Buffer." );     \
  } (void)0


static int s2_cb_str_concat( cwal_callback_args const * args, cwal_value **rv ){
  if(!args->argc){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "'concat' requires at least one argument.");
  }else{
    int rc = 0;
    cwal_size_t i = 0, argc = args->argc;
    cwal_buffer buf = cwal_buffer_empty;
    s2_engine * se = s2_engine_from_args(args);
    cwal_string * str = cwal_value_get_string(args->self);
    assert(se);
    if(1==args->argc && str
       && 0==cwal_string_length_bytes(str)
       && cwal_value_is_string(args->argv[0])){
      /* optimization: "".concat(anyString) simply returns argv[0] */
      *rv = args->argv[0];
      return 0;
    }
    if(cwal_value_is_string(args->self)){
      rc = s2_value_to_buffer( se->e, &buf, args->self );
    }
    for( ; !rc && (i < argc); ++i ){
      rc = s2_value_to_buffer( se->e, &buf, args->argv[i] );
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


/**
   Impl of (STRING+VALUE) operator.

   Assumes operator+ or operator+= call form with the operand at args->argv[0].
*/
static int s2_cb_str_op_concat( cwal_callback_args const * args, cwal_value **rv ){
  if(!args->argc){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "'concat' requires at lest one argument.");
  }else{
    int rc = 0;
    cwal_size_t i = 0, argc = args->argc;
    cwal_buffer buf = cwal_buffer_empty;
    assert(1==args->argc);
    rc = s2_value_to_buffer( args->engine, &buf, args->self );
    for( ; !rc && (i < argc); ++i ){
      /* s2_dump_val( args->argv[i], "arg"); */
      rc = s2_value_to_buffer( args->engine, &buf, args->argv[i] );
    }
    if(!rc){
      *rv = cwal_string_value(cwal_buffer_to_zstring(args->engine, &buf));
      if(!*rv) rc = CWAL_RC_OOM;
      else{
        assert(!buf.mem);
        assert(!buf.capacity);
      }
    }
    cwal_buffer_reserve( args->engine, &buf, 0 );
    return rc;
  }
}

#if 0
/* TODO? Move s2 op-level support for unary +/-STRING
   to here?
*/

/**
   Impl of +STRING and -STRING operators.

   Assumes unary call form with the string on the RHS.
*/
static int s2_cb_str_op_unarypm( int mode /*<0==minus, >0==plus*/,
                                 cwal_callback_args const * args,
                                 cwal_value **rv ){
}
#endif


static int s2_cb_str_byte_at( cwal_callback_args const * args, cwal_value **rv ){
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

static int s2_cb_str_isascii( cwal_callback_args const * args, cwal_value **rv ){
  THIS_STRING;
  *rv = cwal_new_bool( cwal_string_is_ascii(self) );
  return 0;
}

static int s2_cb_str_char_at( cwal_callback_args const * args, cwal_value **rv ){
  cwal_int_t pos;
  THIS_STRING;
  pos = (args->argc>0)
    ? (cwal_value_is_number(args->argv[0])
       ? cwal_value_get_integer(args->argv[0])
       : -1)
    : -1;
  if(pos<0){
    return s2_throw( se, CWAL_RC_MISUSE,
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
          else *rv = cwal_new_string_value(se->e, (char const *)buf,
                                           (cwal_size_t)clen);
        }
      }
    }
    return *rv ? 0 : CWAL_RC_OOM;
  }
}

static int s2_cb_str_toupperlower( cwal_callback_args const * args,
                                   cwal_value **rv,
                                   char doUpper ){
  THIS_STRING;
  return cwal_string_case_fold( args->engine, self, rv, doUpper );
}

static int s2_cb_str_tolower( cwal_callback_args const * args, cwal_value **rv ){
    return s2_cb_str_toupperlower( args, rv, 0 );
}

static int s2_cb_str_toupper( cwal_callback_args const * args, cwal_value **rv ){
    return s2_cb_str_toupperlower( args, rv, 1 );
}


/**
   Internal helper for s2_cb_str_split(), which splits the given
   string into an array of all of its individual characters.
*/
static int s2_cb_str_split_chars(cwal_callback_args const * args,
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

static int s2_cb_str_split( cwal_callback_args const * args,
                            cwal_value **rv ){
  cwal_size_t sepLen = 0;
  unsigned char const * sep;
  THIS_STRING;
  sep = args->argc
    ? (unsigned char const *)cwal_value_get_cstr(args->argv[0], &sepLen)
    : NULL;
  if(!sep){
    return s2_throw( se, CWAL_RC_MISUSE,
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
    return s2_cb_str_split_chars(args, rv, pos, slen, limit);
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
static int s2_cb_str_replace( cwal_callback_args const * args,
                              cwal_value **rv ){
  cwal_size_t needleLen = 0;
  unsigned char const * needle;
  cwal_size_t replLen = 0;
  unsigned char const * repl;
  cwal_string * self;
  ARGS_SE;
  assert(se);
  self = cwal_value_string_part(args->engine, args->self);
  if(!self){
    return cwal_cb_throw( args, CWAL_RC_TYPE,
                       "Expecting a string 'this'." );
  }
  needle = args->argc
    ? (unsigned char const *)cwal_value_get_cstr(args->argv[0], &needleLen)
    : NULL;
  repl = args->argc>1
    ? (unsigned char const *)cwal_value_get_cstr(args->argv[1], &replLen)
    : NULL;
  if(!needle || !repl){
    return cwal_cb_throw( args, CWAL_RC_MISUSE,
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
    unsigned char const * eof = pos + slen;
    unsigned char const * start = pos;
    cwal_buffer * buf = &se->buffer;
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
static int s2_cb_str_indexof( cwal_callback_args const * args, cwal_value **rv ){
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

static int s2_cb_str_length( cwal_callback_args const * args, cwal_value **rv ){
  THIS_STRING;
  *rv = cwal_new_integer( args->engine,
                          (cwal_int_t)cwal_string_length_bytes(self) );
  return *rv ? 0 : CWAL_RC_OOM;
}

static int s2_cb_str_length_utf8( cwal_callback_args const * args, cwal_value **rv ){
  THIS_STRING;
  *rv = cwal_new_integer(args->engine,
                         (cwal_int_t) cwal_string_length_utf8(self) );
  return *rv ? 0 : CWAL_RC_OOM;
}

/**

*/
static int s2_cb_str_substr( cwal_callback_args const * args, cwal_value **rv ){
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
static int s2_cb_str_trim_impl( cwal_callback_args const * args,
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
    if(mode <= 0) for( ; *cs && s2_is_space((int)*cs); ++cs){}
    if(mode>=0){
      for( --end; (end>cs) && s2_is_space((int)*end); --end){}
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

static int s2_cb_str_trim_left( cwal_callback_args const * args,
                                    cwal_value **rv ){
    return s2_cb_str_trim_impl( args, rv, -1 );
}
static int s2_cb_str_trim_right( cwal_callback_args const * args,
                                    cwal_value **rv ){
    return s2_cb_str_trim_impl( args, rv, 1 );
}

static int s2_cb_str_trim_both( cwal_callback_args const * args,
                                    cwal_value **rv ){
    return s2_cb_str_trim_impl( args, rv, 0 );
}

static int s2_cb_str_to_string( cwal_callback_args const * args, cwal_value **rv ){
  cwal_string * str;
  ARGS_SE;
  str = cwal_value_string_part( args->engine, args->self );
  if(!str){
    return s2_throw(se, CWAL_RC_TYPE,
                    "FIXME: check for a different toString() impl in this case.");
  }else{
    *rv = cwal_string_value(str);
    return 0;
  }
}

static int s2_cb_str_unescape_c( cwal_callback_args const * args,
                                     cwal_value **rv ){
  if(!args->argc && !cwal_value_is_string(args->self)){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "'unescape' requires at lest one argument.");
  }else{
    int rc = 0;
    unsigned char const * begin = 0;
    cwal_size_t sLen = 0;
    cwal_string * sv;
    cwal_value * theV;
    s2_engine * se = s2_engine_from_args(args);
    cwal_buffer * buf = &se->buffer;
    cwal_size_t const oldUsed = buf->used;
    if(cwal_value_is_string(args->self)){
      theV = args->self;
    }else{
      theV = args->argv[0];
    }
    sv = cwal_value_get_string(theV);
    if(!sv){
      return s2_throw(se, CWAL_RC_TYPE,
                      "Expecting a STRING as 'this' or first argument.");
    }
    begin = (unsigned char const *)cwal_string_cstr(sv);
    sLen = cwal_string_length_bytes(sv);
    if(!sLen){
      *rv = cwal_string_value(sv);
      return 0;
    }
    rc = s2_unescape_string(args->engine, (char const *)begin, (char const *)begin + sLen, buf );
    assert(buf->used >= oldUsed);
    if(rc){
      return s2_throw(se, rc, "Unescaping failed with rc=%s. "
                      "Probably(?) due to invalid UTF8 or an unknown "
                      "\\Uxxxxxxxx sequence.",
                      cwal_rc_cstr(rc));
    }
    if((sLen == (buf->used-oldUsed))
       && (0==cwal_compare_cstr((char const*)buf->mem, buf->used,
                                (char const*)begin, sLen))){
      /* Same byte sequence - re-use it. */
      *rv = theV;
    }else{
      *rv = cwal_new_string_value( args->engine,
                                   ((char const *)buf->mem+oldUsed),
                                   buf->used-oldUsed );
    }
    buf->used = oldUsed;
    return *rv ? 0 : CWAL_RC_OOM;
  }
}


/**
   Internal helper for Buffer.appendf() and String.applyFormat()
*/
static int s2_helper_appendf( s2_engine * se, cwal_buffer * buf,
                              char const * fmt,
                              cwal_size_t fmtLen,
                              uint16_t argc,
                              cwal_value * const * argv ){
  cwal_size_t oldUsed = buf->used;
  int rc = cwal_buffer_format( se->e, buf, fmt, (cwal_int_t)fmtLen,
                               argc, argv);
  if(rc && (CWAL_RC_OOM != rc)){
    if(buf->used > oldUsed){
      /* Error string is embedded in the buffer... */
      rc = s2_throw(se, rc, "%s",
                    (char const *)(buf->mem + oldUsed));
      buf->used = oldUsed;
    }else{
      rc = s2_throw(se, rc, "String formatting failed with code: %s",
                    cwal_rc_cstr(rc));
    }
  }
  return rc;
}

static int s2_cb_str_apply_format( cwal_callback_args const * args, cwal_value **rv ){
  char const * fmt;
  cwal_size_t fmtLen;
  cwal_size_t oldUsed;
  cwal_buffer * buf;
  int rc;
  THIS_STRING;
  fmt = cwal_string_cstr(self);
  fmtLen = cwal_string_length_bytes(self);
  buf = &se->buffer;
  oldUsed = buf->used;
  rc = s2_helper_appendf( se, buf, fmt, fmtLen, args->argc, args->argv);
  if(!rc){
    cwal_size_t const n = buf->used - oldUsed;
    *rv = cwal_new_string_value(args->engine,
                                 ((char const *)buf->mem+oldUsed), n);
    rc = *rv ? 0 : CWAL_RC_OOM;
  }
  buf->used = oldUsed;
  return rc;
}

static int s2_cb_strish_eval_contents( cwal_callback_args const * args, cwal_value **rv ){
  int rc = 0;
  char const * fname = 0;
  cwal_size_t nameLen = 0, slen = 0;
  char const catchReturn = 1;
  char const * src;
  s2_engine * se = s2_engine_from_args(args);
  cwal_buffer bufSwap = cwal_buffer_empty;
  cwal_buffer * bufSelf = cwal_value_get_buffer(args->self);
  cwal_value * vars = 0;
  /*
    BUG (and fix) REMINDER:

    When this callback is applied to a Buffer, if that buffer's
    contents are modified while this call is active, results are
    undefined. As a partial solution, we move the buffer's contents
    out of the way before evaluation, swapping it back in before
    returning.  That disallows the (corner case) possibility of
    recursion, but it gives us predictable/safe results.
  */
  assert(se);
  src = cwal_value_get_cstr(args->self, &slen)
    /* Corner case reminder: that won't work if args->self
       _derives_ from a Buffer */;
  if(!src){
    if(cwal_value_buffer_part(args->engine, args->self)){
      *rv = cwal_value_undefined();
      return 0;
    }else{
      return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                "'this' type (%s) is not valid for this function.",
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
#if 0
  /* this is a nice idea, but the internal script-func-forwarding
     callback will catch a propagated 'return', so we don't get the
     effect i was going for. */
  if(args->argc>0){
    catchReturn = cwal_value_get_bool(args->argv[1]);
  }
#endif

  if(vars){
    rc = cwal_scope_import_props( args->scope, vars );
    if(rc){
      rc = cwal_cb_throw(args, rc, "Import of vars for evaluation failed.");
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
    s2_buffer_swap(bufSelf, &bufSwap);
  }
  *rv = cwal_value_undefined()
    /* make sure it's populated to avoid an assertion failure below */;
  rc = s2_eval_cstr(se, 1, fname, src, (int)slen, rv)
    /* Reminder: if we don't use a new scope or explicitly
       ++se->sguard->vacuum for the eval, vacuuming gets us
       somewhere (seems to be the argv array). That really should
       be vacuum-safe, at least for the duration of the call().
       Toggling vacuum-safeness after the call requires a flag
       in s2_engine. Doing that doesn't seem to solve it - someone else
       is getting vacuumed, but the argv is suspect because the stack
       trace generation for exceptions throw via here includes
       a prior instance of the argv array, complete with the arguments
       intact. i.e. effectively memory corruption.
    */;
  end:
  if(bufSelf && bufSwap.mem){
    /* restore bufSelf's memory */
    s2_buffer_swap(bufSelf, &bufSwap);
    cwal_buffer_clear(args->engine, &bufSwap);
  }
  if(catchReturn && CWAL_RC_RETURN==rc){
    *rv = s2_propagating_take(se);
    assert(*rv);
    rc = 0;
  }else if(CWAL_RC_EXCEPTION==rc){
    assert(cwal_exception_get(se->e));
  }else if(!rc){
    assert((*rv
           ? ((cwal_value_is_builtin(*rv) || cwal_value_scope(*rv)))
            : 1)
           || "lifetime check failed! cwal memory mismanagement!");
    if(!*rv) *rv = cwal_value_undefined();
  }
  return rc;
}


cwal_value * s2_prototype_string( s2_engine * se ){
    int rc = 0;
    cwal_value * proto;
    proto = cwal_prototype_base_get( se->e, CWAL_TYPE_STRING );
    if(proto
       || !s2_prototype_object(se) /* timing hack */
       ) return proto;
    proto = cwal_new_object_value(se->e);
    if(!proto){
        rc = CWAL_RC_OOM;
        goto end;
    }
    cwal_value_prototype_set(proto, 0 /* so we don't inherit Object!*/);
    rc = cwal_prototype_base_set(se->e, CWAL_TYPE_STRING, proto );
    if(!rc) rc = s2_prototype_stash(se, "String", proto);
    if(rc) goto end;
    assert(proto == cwal_prototype_base_get(se->e, CWAL_TYPE_STRING));
    /* MARKER(("Setting up STRING prototype.\n")); */

    {
      cwal_value * v;
      const s2_func_def funcs[] = {
      S2_FUNC2("applyFormat", s2_cb_str_apply_format),
      S2_FUNC2("byteAt", s2_cb_str_byte_at),
      S2_FUNC2("charAt", s2_cb_str_char_at),
      S2_FUNC2("compare", s2_cb_value_compare),
      S2_FUNC2("concat", s2_cb_str_concat),
      S2_FUNC2("evalContents", s2_cb_strish_eval_contents),
      S2_FUNC2("indexOf", s2_cb_str_indexof),
      S2_FUNC2("isAscii", s2_cb_str_isascii),
      S2_FUNC2("length", s2_cb_str_length_utf8),
      S2_FUNC2("lengthBytes", s2_cb_str_length),
      S2_FUNC2("lengthUtf8", s2_cb_str_length_utf8),
      S2_FUNC2("replace", s2_cb_str_replace),
      S2_FUNC2("split", s2_cb_str_split),
      S2_FUNC2("substr", s2_cb_str_substr),
      S2_FUNC2("toLower", s2_cb_str_tolower),
      S2_FUNC2("toJSONString", s2_cb_this_to_json_token),
      S2_FUNC2("toString", s2_cb_str_to_string),
      S2_FUNC2("toUpper", s2_cb_str_toupper),
      S2_FUNC2("trim", s2_cb_str_trim_both),
      S2_FUNC2("trimLeft", s2_cb_str_trim_left),
      S2_FUNC2("trimRight", s2_cb_str_trim_right),
      S2_FUNC2("unescape", s2_cb_str_unescape_c),
      s2_func_def_empty_m
      };
      rc = s2_install_functions(se, proto, funcs, 0 );
      if(rc) goto end;
      /* Create a Function instance for s2_cb_str_op_concat and
         install it manually, to avoid duplicating 2 cwal_function
         wrappers for it via s2_install_functions(). */
      v = cwal_new_function_value(se->e, s2_cb_str_op_concat, 0, 0, 0);
      if(!v){ rc = CWAL_RC_OOM; goto end; }
      cwal_value_ref(v);
      rc = cwal_prop_set_with_flags(proto, "operator+", 9, v,
                                    CWAL_VAR_F_CONST);
      if(!rc){
        rc = cwal_prop_set_with_flags(proto, "operator+=", 10, v,
                                      CWAL_VAR_F_CONST);
      }
      cwal_value_unref(v);
      if(rc) goto end;
    }

    end:
    return rc ? NULL : proto;
}

/**
   Buffer slice([offset=0 [,count=0]])

   where count==0 means "to the end".
*/
static int s2_cb_buffer_slice( cwal_callback_args const * args, cwal_value **rv ){
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
    return s2_throw(se, CWAL_RC_RANGE,
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
static int s2_cb_buffer_replace( cwal_callback_args const * args,
                                 cwal_value **rv ){
  cwal_size_t needleLen = 0;
  unsigned char const * needle;
  cwal_size_t replLen = 0;
  unsigned char const * repl;
  cwal_buffer * self;
  static const char * usageError =
    "Expecting (string,string) or (int, int) arguments.";
  ARGS_SE;
  assert(se);
  self = cwal_value_buffer_part(args->engine, args->self);
  if(!self){
    if(se){/*avoid unused param warning*/}
    return cwal_cb_throw( args, CWAL_RC_TYPE,
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
      return cwal_cb_throw( args, CWAL_RC_MISUSE, "%s", usageError);
    }else if((needleByte & ~0xFF) || (replByte & ~0xFF)) {
      return cwal_cb_throw( args, CWAL_RC_RANGE,
                         "Byte values must be in the range [0,255]." );
    }else{
      cwal_int_t const limit = args->argc>2
        ? cwal_value_get_integer(args->argv[2])
        : 0;
      unsigned char const bN = (unsigned char)(needleByte & 0xFF);
      unsigned char const bR = (unsigned char)(replByte & 0xFF);
      int const rc =
        cwal_buffer_replace_byte(args->engine, self, bN, bR,
                                 (cwal_size_t)(limit>=0 ? (cwal_size_t)limit : 0U),
                                 NULL);
      assert(!rc && "There are no error cases here.");
      if(rc){/*avoid unused param warning*/}
      *rv = args->self;
      return 0;
    }    
  }else if(!needle || !repl){
    return cwal_cb_throw( args, CWAL_RC_MISUSE, "%s", usageError);
  }else if(!needleLen){
    return cwal_cb_throw( args, CWAL_RC_RANGE,
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
      rc = cwal_cb_throw( args, CWAL_RC_MISUSE, "%s", usageError);
    }else{
      rc = cwal_buffer_replace_str(args->engine, self,
                                   needle, needleLen,
                                   repl, replLen,
                                   (limit>=0 ? (cwal_size_t)limit : 0U),
                                   NULL);
      if(rc){
        rc = cwal_cb_throw(args, rc, "cwal_buffer_replace_str() failed "
                         "with code %d (%s).", rc, cwal_rc_cstr(rc));
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
static int s2_cb_buffer_length_uc( cwal_callback_args const * args,
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


static int s2_cb_buffer_length_u( cwal_callback_args const * args, cwal_value **rv ){
  return s2_cb_buffer_length_uc( args, rv, 0 );
}

static int s2_cb_buffer_length_c( cwal_callback_args const * args, cwal_value **rv ){
  return s2_cb_buffer_length_uc( args, rv, 1 );
}

static int s2_cb_buffer_is_empty( cwal_callback_args const * args, cwal_value **rv ){
  THIS_BUFFER;
  *rv = cwal_new_bool(self->used>0 ? 0 : 1);
  return 0;
}

static int s2_cb_buffer_length_utf8( cwal_callback_args const * args, cwal_value **rv ){
  cwal_size_t ulen;
  THIS_BUFFER;
  ulen = cwal_strlen_utf8((char const *)self->mem, self->used);
  *rv = cwal_new_integer(args->engine, (cwal_int_t) ulen);
  return *rv ? 0 : CWAL_RC_OOM;
}


static int s2_cb_buffer_append( cwal_callback_args const * args, cwal_value **rv ){
  uint16_t i;
  int rc = 0;
  THIS_BUFFER;
  for( i = 0; !rc && (i < args->argc); ++i ){
    rc = s2_value_to_buffer(args->engine, self, args->argv[i]);
  }
  if(!rc) *rv = args->self;
  return rc;
}

static int s2_cb_buffer_appendf( cwal_callback_args const * args, cwal_value **rv ){
  int rc = 0;
  cwal_size_t fmtlen;
  char const * fmt;
  THIS_BUFFER;
  fmt = (args->argc>0)
    ? cwal_value_get_cstr(args->argv[0], &fmtlen)
    : NULL;
  if((!args->argc) || !fmt){
    return s2_throw(se, CWAL_RC_MISUSE,
                    "Expecting (String,...) arguments.");
  }
  rc = s2_helper_appendf(se, self, fmt, fmtlen, args->argc-1, args->argv+1);
  if(!rc){
#if 1
    *rv = args->self;
#else
    cwal_size_t const newLen = self->used - oldUsed;
    *rv = cwal_new_integer(args->engine, (int)(newLen));
    rc = *rv ? 0 : CWAL_RC_OOM;
#endif
  }
  return rc;
}

/**
   Assumes operator call form.
*/
static int s2_cb_buffer_op_append( cwal_callback_args const * args, cwal_value **rv ){
  THIS_BUFFER;
  if(!args->argc){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "operator+ requires at least one argument.");
  }else{
    int rc = 0;
    cwal_size_t i = 0, argc = args->argc;
    assert(1==argc);
    for( ; !rc && (i < argc); ++i ){
      /* s2_dump_val( args->argv[i], "arg"); */
      rc = s2_value_to_buffer( args->engine, self, args->argv[i] );
    }
    if(!rc){
      *rv = args->self;
    }
    return rc;
  }
}


static int s2_cb_construct_buffer( cwal_callback_args const * args, cwal_value **rv ){
  cwal_int_t len;
  ARGS_SE;
  assert(se);
  if(cwal_value_buffer_part(args->engine, args->self)){
    /* assert(!"This doesn't seem to be possible any more."); */
    return s2_throw(se, CWAL_RC_MISUSE,
                    "Do not call a buffer as a function.");
  }
  len = args->argc
    ? cwal_value_get_integer(args->argv[0])
    : 0;
  if(len<0) len = 0;
  *rv = cwal_new_buffer_value(args->engine, (cwal_size_t) len);
  return *rv ? 0 : CWAL_RC_OOM;
}

static int s2_cb_buffer_take_string( cwal_callback_args const * args, cwal_value **rv ){
  THIS_BUFFER;
  *rv = self->mem
    ? cwal_string_value( cwal_buffer_to_zstring(args->engine, self) )
    : cwal_value_null();
  return *rv ? 0 : CWAL_RC_OOM;
}

static int s2_cb_buffer_to_string( cwal_callback_args const * args, cwal_value **rv ){
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
  return s2_throw(se, CWAL_RC_MISUSE,
                  "Buffer.toString() arguments must 0 or "
                  "positive integers.");
}

static int s2_cb_buffer_reset( cwal_callback_args const * args, cwal_value **rv ){
  THIS_BUFFER;
  self->used = 0;
  memset(self->mem, 0, self->capacity);
  *rv = args->self;
  return 0;
}

static int s2_cb_buffer_resize( cwal_callback_args const * args, cwal_value **rv ){
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


static cwal_int_t s2_get_byte(cwal_value const *v){
  char const * cstr = cwal_value_get_cstr(v, NULL);
  return cstr ? *cstr : cwal_value_get_integer(v);
}

int s2_cb_buffer_byte_at( cwal_callback_args const * args, cwal_value **rv ){
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
      self->mem[pos] = 0xFF & s2_get_byte(args->argv[1]);
      *rv = cwal_value_true();
      if(self->used <= (cwal_size_t)pos) self->used = (cwal_size_t)pos+1;
      return 0;
    }
  }
}

int s2_cb_buffer_file_read( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  char const * fname;
  cwal_buffer * newBuf = NULL;
  cwal_buffer * self = cwal_value_buffer_part(args->engine, args->self); 
  ARGS_SE;
  assert(se);
  if( (rc = s2_cb_disable_check(args, S2_DISABLE_FS_READ)) ) return rc;
  fname = args->argc
    ? cwal_string_cstr(cwal_value_get_string(args->argv[0]))
    : NULL;
  if(!fname){
    return s2_throw(se, CWAL_RC_MISUSE,
                    "Expecting a filename argument.");
  }
  if(!self/* || (cwal_buffer_value(self) == s2_prototype_buffer(se))*/){
    newBuf = cwal_new_buffer(args->engine, 0);
    if(!newBuf) return CWAL_RC_OOM;
  }
  rc = cwal_buffer_fill_from_filename( args->engine, newBuf ? newBuf : self, fname );
  if(rc){
    if(newBuf) cwal_value_unref(cwal_buffer_value(newBuf));
    assert(CWAL_RC_EXCEPTION!=rc);
    rc = s2_throw(se, rc,
                  "Reading file [%s] failed.",
                  fname);
  }else {
    *rv = newBuf ? cwal_buffer_value(newBuf) : args->self;
  }
  return rc;
}


int s2_cb_buffer_file_write( cwal_callback_args const * args, cwal_value **rv ){
  int rc = 0;
  char const * fname;
  cwal_size_t nameLen = 0;
  THIS_BUFFER;
  rc = s2_cb_disable_check(args, S2_DISABLE_FS_WRITE);
  if(rc) return rc;
  fname = args->argc
    ? cwal_value_get_cstr(args->argv[0], &nameLen)
    : NULL;
  if(!fname){
    return s2_throw(se, CWAL_RC_MISUSE,
                    "Expecting a filename argument.");
  }
  else{
    FILE * f;
    char append = (args->argc>1)
      ? cwal_value_get_bool(args->argv[1])
      : 0;
    f = fopen( fname, append ? "a" : "w" );
    if(!f){
      if(errno){
        rc = s2_throw(se, cwal_errno_to_cwal_rc(errno, CWAL_RC_IO),
                      "Could not open file [%.*s] "
                      "in %s mode. "
                      "errno=%d: %s",
                      (int)nameLen, fname,
                      append ? "append" : "truncate/write",
                      errno, strerror(errno));
      }else{
        rc = s2_throw(se, cwal_errno_to_cwal_rc(errno, CWAL_RC_IO),
                      "Could not open file [%.*s] "
                      "in %s mode.",
                      (int)nameLen, fname,
                      append ? "append" : "truncate/write");
      }
    }else{
      if(self->used &&
         1 != fwrite(self->mem, self->used, 1, f)){
        rc = s2_throw(se, errno
                      ? cwal_errno_to_cwal_rc(errno, CWAL_RC_IO)
                      : CWAL_RC_IO,
                      "Error writing %"CWAL_SIZE_T_PFMT" byte(s) "
                      "to file [%.*s].",
                      self->used, (int)nameLen, fname);
      }
      fclose(f);
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
static int s2_cb_buffer_fill( cwal_callback_args const * args, cwal_value **rv ){
  cwal_int_t byte;
  THIS_BUFFER;
  if(!args->argc){
    return s2_throw(se, CWAL_RC_MISUSE,
                    "Expecting a byte value argument.");
  }
  byte = 0xFF & s2_get_byte(args->argv[0]);
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
      return s2_throw(se, CWAL_RC_MISUSE,
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
#if 0
    *rv = cwal_new_integer( args->engine, len );
    return *rv ? 0 : CWAL_RC_OOM;
#else
    *rv = args->self;
    return 0;
#endif
  }
}

/*
** Combined impl for Buffer.compress() and Buffer.uncompress().  If
** doCompress is true, it is the former, else the latter.
*/
static int s2_cb_buffer_press( cwal_callback_args const * args,
                               cwal_value **rv, char doCompress ){
#if !S2_ENABLE_ZLIB
  if(rv || doCompress){/*avoid unused param warning*/}
  return cwal_cb_throw(args, CWAL_RC_UNSUPPORTED,
                    "Compression support is not enabled in this build.");
#else
  int rc;
  cwal_value * arg = args->argc ? args->argv[0] : args->self;
  cwal_buffer * buf = cwal_value_buffer_part(args->engine, arg);
  char const isPressed = buf ? s2_buffer_is_compressed(buf) : 0;
  if(!buf){
    return cwal_cb_throw(args, CWAL_RC_TYPE,
                      "%s is-not-a Buffer.",
                      args->argc ? "Argument" : "'this'");
  }
  if(doCompress && isPressed){
    rc = 0;
  }else if(!doCompress && !isPressed){
    rc = 0;
  }else{
    rc = doCompress
      ? s2_buffer_compress( args->engine, buf, buf )
      : s2_buffer_uncompress( args->engine, buf, buf );
  }
  if(rc && CWAL_RC_OOM!=rc){
    rc = cwal_cb_throw(args, rc,
                    "Buffer %s failed with code %d (%s).",
                    doCompress ? "compression" : "decompression",
                    rc, cwal_rc_cstr(rc));
  }
  if(!rc) *rv = arg;
  return rc;
#endif
}

/**
   Script usage:

   bufferInstance.compress()

   returns 'this'.

   compress(buffer)

   returns the given buffer.

   Throws if the arg/this is not/does not derive a cwal_buffer.
*/
static int s2_cb_buffer_compress( cwal_callback_args const * args,
                                  cwal_value **rv ){
  return s2_cb_buffer_press(args, rv, 1);
}

/**
   Script usage:

   bufferInstance.uncompress()

   returns 'this'.

   compress(buffer)

   returns the given buffer.

   Throws if the arg/this is not/does not derive a cwal_buffer.
*/
static int s2_cb_buffer_uncompress( cwal_callback_args const * args,
                                    cwal_value **rv ){
  return s2_cb_buffer_press(args, rv, 0);
}

/**
   Script signature:

   boolean bufferInstance.isCompressed([buffer])

   If passed an argument, it evaluates that argument,
   else it evaluates args->self.
*/
static int s2_cb_buffer_is_compressed( cwal_callback_args const * args,
                                       cwal_value **rv ){
  cwal_buffer * buf = cwal_value_buffer_part(args->engine,
                                             args->argc
                                             ? args->argv[0]
                                             : args->self);
  if(buf){
    *rv = cwal_new_bool( s2_buffer_is_compressed(buf) );
    return 0;
  }else{
    return cwal_cb_throw(args, CWAL_RC_TYPE,
                      "%s is-not-a Buffer.",
                      args->argc ? "Argument" : "'this'");
  }
}

static int s2_cb_buffer_uncompressed_size( cwal_callback_args const * args,
                                           cwal_value **rv ){
  cwal_buffer * buf = cwal_value_buffer_part(args->engine,
                                             args->argc
                                             ? args->argv[0]
                                             : args->self);
  if(!buf){
    return cwal_cb_throw(args, CWAL_RC_TYPE,
                      "%s is-not-a Buffer.",
                      args->argc ? "Argument" : "'this'");
  }else{
    uint32_t const sz = s2_buffer_uncompressed_size(buf);
    if((uint32_t)-1 == sz){
      *rv = cwal_value_undefined();
      return 0;
    }else{
      *rv = cwal_new_integer(args->engine, (cwal_int_t)sz);
      return *rv ? 0 : CWAL_RC_OOM;
    }
  }
}



cwal_value * s2_prototype_buffer( s2_engine * se ){
    int rc = 0;
    cwal_value * proto;
    proto = cwal_prototype_base_get( se->e, CWAL_TYPE_BUFFER );
    if(proto
       || !s2_prototype_object(se) /* timing hack */
       ) return proto;
    proto = cwal_new_object_value(se->e);
    if(!proto){
        rc = CWAL_RC_OOM;
        goto end;
    }
    rc = cwal_prototype_base_set(se->e, CWAL_TYPE_BUFFER, proto );
    if(!rc) rc = s2_prototype_stash(se, "Buffer", proto);
    if(rc) goto end;
    assert(proto == cwal_prototype_base_get(se->e, CWAL_TYPE_BUFFER));
    /* MARKER(("Setting up BUFFER prototype.\n")); */

    
    {
      cwal_value * v = 0;
#if S2_ENABLE_ZLIB
      v = cwal_new_string_value(se->e, "zlib", 4);
      if(!v) return NULL /* proto is already stashed
                            at the cwal level, not leaked. */;
#endif
      cwal_value_ref(v);
      rc = s2_set_with_flags(se, proto, "compression", 11,
                              v ? v : cwal_value_false(),
                              CWAL_VAR_F_CONST);
      cwal_value_unref(v);
      v = 0;
      if(rc) goto end;
    }
    
    {
      const s2_func_def funcs[] = {
        S2_FUNC2("append", s2_cb_buffer_append),
        S2_FUNC2("appendJSON", s2_cb_this_to_json_token),
        S2_FUNC2("appendf", s2_cb_buffer_appendf),
        S2_FUNC2("byteAt", s2_cb_buffer_byte_at),
        S2_FUNC2("capacity", s2_cb_buffer_length_c),
        S2_FUNC2("evalContents", s2_cb_strish_eval_contents),
        S2_FUNC2("fill", s2_cb_buffer_fill),
        S2_FUNC2("isEmpty", s2_cb_buffer_is_empty),
        S2_FUNC2("length", s2_cb_buffer_length_u),
        S2_FUNC2("lengthUtf8", s2_cb_buffer_length_utf8),
        S2_FUNC2("operator<<", s2_cb_buffer_op_append),
        S2_FUNC2("readFile", s2_cb_buffer_file_read),
        S2_FUNC2("replace", s2_cb_buffer_replace),
        S2_FUNC2("reserve", s2_cb_buffer_length_c) /* deprecated name for capacity(N) */,
        S2_FUNC2("reset", s2_cb_buffer_reset),
        S2_FUNC2("resize", s2_cb_buffer_resize),
        S2_FUNC2("slice", s2_cb_buffer_slice),
        S2_FUNC2("substr", s2_cb_str_substr),
        S2_FUNC2("takeString", s2_cb_buffer_take_string),
        S2_FUNC2("toString", s2_cb_buffer_to_string),
        S2_FUNC2("writeFile", s2_cb_buffer_file_write),
        S2_FUNC2("new", s2_cb_construct_buffer),
        S2_FUNC2("isCompressed", s2_cb_buffer_is_compressed),
        S2_FUNC2("compress", s2_cb_buffer_compress),
        S2_FUNC2("uncompress", s2_cb_buffer_uncompress),
        S2_FUNC2("uncompressedSize", s2_cb_buffer_uncompressed_size),
        s2_func_def_empty_m
      };
      rc = s2_install_functions(se, proto, funcs, 0);
      if(rc) goto end;
      else {
        cwal_value * fv = 0;
        s2_get(se, proto, "new", 3, &fv);
        assert(fv && "we JUST put this in there!");
        rc = s2_ctor_method_set( se, proto,
                                 cwal_value_get_function(fv) );
      }
    }

    end:
    return rc
      ? NULL /* remember: proto is stashed at this point, so no leak. */
      : proto;
}


#undef MARKER
#undef ARGS_SE
#undef THIS_STRING
#undef THIS_BUFFER
#undef THIS_CSTRING
