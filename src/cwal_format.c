/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=4 et sw=2 tw=80: */
/**
   This file contains the impl for cwal_buffer_format() and friends.
*/
#include "wh/cwal/cwal.h"
#include <string.h> /* memcpy() */
#include <stdlib.h> /* strtol() */
#include <assert.h>

#if 1
/* Only for debuggering */
#include <stdio.h>
#define MARKER(pfexp)                                               \
  do{ printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__);   \
    printf pfexp;                                                   \
  } while(0)
#endif


/**
   Internal type IDs for cwal_format_info instances.
   
   Maintenance reminder: these MUST be aligned with
   the CWAL_FORMAT_INFO array.
*/
enum cwal_format_t {
CWAL_FMT_UNKNOWN = 0,
CWAL_FMT_BOOL,
CWAL_FMT_NULL,
CWAL_FMT_UNDEF,
CWAL_FMT_VOIDP,
CWAL_FMT_INT_DEC,
CWAL_FMT_INT_HEXLC,
CWAL_FMT_INT_HEXUC,
CWAL_FMT_INT_OCTAL,
CWAL_FMT_DOUBLE,
CWAL_FMT_STRING,
CWAL_FMT_STRING_SQL,
CWAL_FMT_CHAR,
CWAL_FMT_TYPENAME,
CWAL_FMT_BLOB,
CWAL_FMT_JSON,
CWAL_FMT_URLENCODE,
CWAL_FMT_URLDECODE,
CWAL_FMT_MAX
};
typedef enum cwal_format_t cwal_format_t;

enum cwal_format_flags {
CWAL_FMT_F_NONE = 0,
CWAL_FMT_F_SIGNED = 0x01,
CWAL_FMT_F_ALT_FORM = 0x02,
CWAL_FMT_F_SQL_QUOTES_DOUBLE = 0x01
};
typedef enum cwal_format_flags cwal_format_flags;

typedef struct cwal_format_info cwal_format_info;
/**
   Callback for concrete cwal_buffer_format() handlers.  fi holds the
   formatting details, v the value. Implementations must interpret v
   for formatting purposes, following the settings in fi (insofar as
   possible for the given type). On error the handler must return
   non-0 and may set fi->errMsg to a string describing the problem.
   Handlers are allowed to silently ignore small misuses like invalid
   width/precision values.
*/
typedef int (*cwal_format_part_f)( cwal_engine * e, cwal_buffer * tgt,
                                   cwal_format_info * fi, cwal_value const * v );
/**
   Holds information related to the handling of a format specifier in
   cwal_buffer_format().
*/
struct cwal_format_info {
  /** The format type for which this handler is intended. */
  cwal_format_t type;
  /** Type-specific flags, from cwal_format_flags. */
  int flags;
  /**
     Precision part of the format string.
  */
  int precision;
  /**
     Width part of the format string.
  */
  int width;
  /**
     Ordinal position in the args processing, 1-based.
  */
  int position;
  /**
     Can be set to propagate an error message in certai
     contexts.
  */
  char const * errMsg;
  /**
     Padding char. TODO: change to int and output it as a UTF8
     char. TODO: split into left/right padding. Currently we
     hard-code space as right-side padding.
  */
  char pad;
  /**
     Callback handler for this formatter.
  */
  cwal_format_part_f f;
};

#define cwalFormatNoPrecision ((int)0x7FFFFFFF)

static int cwal_format_part_int( cwal_engine * e, cwal_buffer * tgt,
                                 cwal_format_info * fi, cwal_value const * v );
static int cwal_format_part_double( cwal_engine * e, cwal_buffer * tgt,
                                    cwal_format_info * fi, cwal_value const * v );
static int cwal_format_part_string( cwal_engine * e, cwal_buffer * tgt,
                                    cwal_format_info * fi, cwal_value const * v );
static int cwal_format_part_sql( cwal_engine * e, cwal_buffer * tgt,
                                 cwal_format_info * fi, cwal_value const * v );
static int cwal_format_part_char( cwal_engine * e, cwal_buffer * tgt,
                                  cwal_format_info * fi, cwal_value const * v );
static int cwal_format_part_basics( cwal_engine * e, cwal_buffer * tgt,
                                    cwal_format_info * fi, cwal_value const * v );
static int cwal_format_part_blob( cwal_engine * e, cwal_buffer * tgt,
                                  cwal_format_info * fi, cwal_value const * v );
static int cwal_format_part_json( cwal_engine * e, cwal_buffer * tgt,
                                  cwal_format_info * fi, cwal_value const * v );
static int cwal_format_part_urlenc( cwal_engine * e, cwal_buffer * tgt,
                                    cwal_format_info * fi, cwal_value const * v );
static int cwal_format_part_urldec( cwal_engine * e, cwal_buffer * tgt,
                                    cwal_format_info * fi, cwal_value const * v );

static const cwal_format_info CWAL_FORMAT_INFO[CWAL_FMT_MAX+1] = {
/*
  Maintenance reminder: these MUST be in the same order the entries
  are defined in cwal_format_t.
*/
/* {type,flags,precision,width,position,errMsg,pad,f} */
{CWAL_FMT_UNKNOWN, 0, 0, 0, 0,NULL,' ', NULL},
#define FMT(T,CB) {CWAL_FMT_##T, 0, 0, 0, 0,NULL,' ', cwal_format_part_ ## CB}
FMT(BOOL,basics),
FMT(NULL,basics),
FMT(UNDEF,basics),
FMT(VOIDP,basics),
FMT(INT_DEC,int),
FMT(INT_HEXLC,int),
FMT(INT_HEXUC,int),
FMT(INT_OCTAL,int),
FMT(DOUBLE,double),
FMT(STRING,string),
FMT(STRING_SQL,sql),
FMT(CHAR,char),
FMT(TYPENAME,basics),
FMT(BLOB,blob),
FMT(JSON,json),
FMT(URLENCODE,urlenc),
FMT(URLDECODE,urldec),
#undef FMT
{CWAL_FMT_MAX, 0, 0, 0, 0,NULL,' ', NULL},
};

static const cwal_format_info cwal_format_info_empty = {
CWAL_FMT_UNKNOWN,
0/*flags*/,
cwalFormatNoPrecision/*precision*/,
0/*width*/,
0/*position*/,
NULL/*errMsg*/,
' '/*pad*/,
NULL/*f*/
};

int cwal_format_part_basics( cwal_engine * e, cwal_buffer * tgt,
                             cwal_format_info * fi, cwal_value const * v ){
  switch(fi->type){
    case CWAL_FMT_BOOL:{
      char const b = cwal_value_get_bool(v);
      return cwal_buffer_append( e, tgt, b ? "true" : "false", b ? 4 : 5);
    }
    case CWAL_FMT_UNDEF:
      return cwal_buffer_append( e, tgt, "undefined", 9);
    case CWAL_FMT_VOIDP:
      return cwal_buffer_printf( e, tgt, "%s@%p",
                                 cwal_value_type_name(v),
                                 (void const*)v);
    case CWAL_FMT_TYPENAME:
      return cwal_buffer_printf(e, tgt, "%s", cwal_value_type_name(v));
    case CWAL_FMT_NULL:
    default:
      /* This can happen for %N when the value is-not NULL. */
      return cwal_buffer_append( e, tgt, "null", 4);
  }
}


int cwal_format_part_int( cwal_engine * e,
                          cwal_buffer * tgt,
                          cwal_format_info * fi,
                          cwal_value const * v ){
  enum { BufSize = CWAL_INT_T_BITS * 2 };
  char fmtBuf[BufSize+1] = {'%',0};
  char * b = fmtBuf + 1;
  int rc;
  cwal_int_t const i = cwal_value_get_integer(v);
  char const * intFmt = 0;
  if((i>0) && (CWAL_FMT_F_SIGNED & fi->flags)){
    *(b++) = '+';
  }
  if(0 != fi->width){
    if('0'==fi->pad) *(b++) = '0';
    b += snprintf(b, BufSize, "%d", fi->width);
  }
  if(fi->precision && cwalFormatNoPrecision!=fi->precision){
    b += snprintf(b, BufSize, ".%d", fi->precision);
  }
  switch(fi->type){
    case CWAL_FMT_INT_DEC:
      intFmt = CWAL_INT_T_PFMT;
      break;
    case CWAL_FMT_INT_HEXLC:
      intFmt = CWAL_INT_T_PFMTx;
      break;
    case CWAL_FMT_INT_HEXUC:
      intFmt = CWAL_INT_T_PFMTX;
      break;
    case CWAL_FMT_INT_OCTAL:
      intFmt = CWAL_INT_T_PFMTo;
      break;
    default:
      assert(!"CANNOT HAPPEN");
      return CWAL_RC_RANGE;
  }
  assert(intFmt);
  {
    cwal_size_t const fmtLen = cwal_strlen(intFmt);
    assert(b + fmtLen <= (fmtBuf + BufSize));
    memcpy(b, intFmt, (size_t)fmtLen);
    b += fmtLen;
    *b = 0;
  }
  rc = cwal_buffer_printf(e, tgt, fmtBuf, i );
  return rc;
}

int cwal_format_part_double( cwal_engine * e,
                             cwal_buffer * tgt,
                             cwal_format_info * fi,
                             cwal_value const * v ){

  enum { BufSize = 120 };
  static unsigned int fmtLen = sizeof(CWAL_DOUBLE_T_PFMT)-1;
  char fmtBuf[BufSize+1] = {'%',0};
  cwal_size_t fmtBufLen = BufSize;
  char * b = fmtBuf + 1;
  int rc = 0;
  cwal_double_t const d = cwal_value_get_double(v);
  if((d>0) && (CWAL_FMT_F_SIGNED & fi->flags)){
    *(b++) = '+';
  }
  if(fi->width){
    if('0'==fi->pad){
      *(b++) = '0';
      --fmtBufLen;
    }
    cwal_int_to_cstr( fi->width, b, &fmtBufLen );
    b += fmtBufLen;
  }
  if(fi->precision && cwalFormatNoPrecision != fi->precision){
    fmtBufLen = BufSize - (b - fmtBuf);
    *(b++) = '.';
    cwal_int_to_cstr( fi->precision, b, &fmtBufLen);
    b += fmtBufLen;
  }
  assert(b < (fmtBuf+BufSize+fmtLen));
  memcpy(b, CWAL_DOUBLE_T_PFMT, fmtLen);
  b += fmtLen;
  *b = 0;
  if(!rc){
    rc = cwal_buffer_printf(e, tgt, fmtBuf, d );
    if(!rc && tgt->used &&
       (!fi->precision || cwalFormatNoPrecision == fi->precision)){
      /* Trim trailing zeroes when no precision is specified... */
      b = (char *)(tgt->mem + tgt->used - 1);
      for( ; ('0'==*b) && tgt->used; --b, --tgt->used){}
      if('.'==*b) ++tgt->used /* leave one trailing 0 if we just
                                 stripped the last one. */;
    }
  }
  return rc;
}

int cwal_format_part_string( cwal_engine * e,
                             cwal_buffer * tgt,
                             cwal_format_info * fi,
                             cwal_value const * v ){
  int rc = 0;
  cwal_size_t slen = 0;
  cwal_size_t ulen;
  char const * cstr = cwal_value_get_cstr(v, &slen);
  int width = fi->width;
  if(!cstr){
#if 0
    cwal_type_id const tid = cwal_value_type_id(v);
    switch(tid){
      /* Reconsider these and whether we need to support the
         width/precision values in these cases. */
      case CWAL_TYPE_BUFFER:
        /* completely empty buffer */
        return 0;
      case CWAL_TYPE_HASH:
      case CWAL_TYPE_FUNCTION:
      case CWAL_TYPE_NATIVE:
        return cwal_buffer_printf(e, tgt, "%s@%p",
                                  cwal_value_type_name(v),
                                  (void const*)v);
      case CWAL_TYPE_BOOL:
        return cwal_buffer_printf(e, tgt, "%s",
                                  cwal_value_get_bool(v)
                                  ? "true" : "false");
      case CWAL_TYPE_INTEGER:
        return cwal_buffer_printf(e, tgt, "%"CWAL_INT_T_PFMT,
                                  (cwal_int_t)cwal_value_get_integer(v));
      case CWAL_TYPE_DOUBLE:
        return cwal_buffer_printf(e, tgt, "%"CWAL_DOUBLE_T_PFMT,
                                  (cwal_int_t)cwal_value_get_double(v));
      default:
        cstr = "(nil)";
        ulen = slen = 5;
    }
#else
    cstr = "(nil)";
    ulen = slen = 5;
#endif
  }
  else{
    ulen = cwal_strlen_utf8( cstr, slen );
  }
  if((cwalFormatNoPrecision != fi->precision)
     && (fi->precision>0) && (fi->precision<(int)ulen)){
    int i = 0;
    unsigned char const * ustr = (unsigned char const *)cstr;
    unsigned char const * uend = (unsigned char const *)cstr + slen;
    unsigned char const * upos = ustr;
    for( ; i < fi->precision; ++i, ustr = upos ){
      cwal_utf8_read_char( ustr, uend, &upos );
      assert(upos > ustr);
    }
    ulen = (cwal_size_t)fi->precision;
    slen = upos - (unsigned char const *)cstr;
    /* slen = (cwal_size_t)fi->precision; */
  }
  if(width>0 && ((int)ulen < width)){ /* right alignment */
    int i = (int)ulen - width;
    for( ; !rc && (i < 0); ++i){
      rc = cwal_buffer_append(e, tgt, &fi->pad, 1);
    }
  }
  if(!rc){
    rc = cwal_buffer_append(e, tgt, cstr, slen);
    if(!rc && (width<0)){ /* right padding */
      int i = (int)ulen - -width;
      for( ; !rc && (i < 0); ++i){
        rc = cwal_buffer_append(e, tgt, &fi->pad, 1);
      }
    }
  }
  return rc;
}

int cwal_format_part_sql( cwal_engine * e,
                          cwal_buffer * tgt,
                          cwal_format_info * fi,
                          cwal_value const * v ){
  int rc = 0;
  cwal_size_t slen = 0;
  unsigned int i, n;
  cwal_size_t j;
  int ch, isnull;
  int needQuote;
  char const q = ((CWAL_FMT_F_SQL_QUOTES_DOUBLE & fi->flags)?'"':'\'');   /* Quote character */
  char const * escarg = cwal_value_get_cstr(v, &slen);
  char * bufpt = NULL;
  cwal_size_t const oldUsed = e->buffer.used;
  isnull = escarg==0;
  if( isnull ) {
    if(CWAL_FMT_F_ALT_FORM & fi->flags){
      escarg = "NULL";
      slen = 4;
    }else{
      escarg = "(NULL)";
      slen = 6;
    }
  }
  for(i=n=0; (i<slen) && ((ch=escarg[i])!=0); ++i){
    if( ch==q ) ++n;
  }
  needQuote = !isnull && (CWAL_FMT_F_ALT_FORM & fi->flags);
  n += i + 1 + needQuote*2;
  /* FIXME: use a static buffer here instead of malloc()! Shame on you! */
  rc = cwal_buffer_reserve( e, &e->buffer, oldUsed + n );
  if(rc) return rc;
  bufpt = (char *)e->buffer.mem + oldUsed;
  if( ! bufpt ) return CWAL_RC_OOM;
  j = 0;
  if( needQuote ) bufpt[j++] = q;
  for(i=0; (ch=escarg[i])!=0; i++){
    bufpt[j++] = ch;
    if( ch==q ) bufpt[j++] = ch;
  }
  if( needQuote ) bufpt[j++] = q;
  bufpt[j] = 0;
  rc = cwal_buffer_append(e, tgt, bufpt, j);
  e->buffer.used = oldUsed;
  e->buffer.mem[e->buffer.used] = 0;
  return 0;
}


int cwal_format_part_blob( cwal_engine * e,
                           cwal_buffer * tgt,
                           cwal_format_info * fi,
                           cwal_value const * v ){
  int rc = 0;
  cwal_size_t slen = 0;
  char const * cstr = cwal_value_get_cstr(v, &slen);
  int width = fi->width;
  if(!cstr) return 0;
  else if((cwalFormatNoPrecision != fi->precision)
          && (fi->precision>0) && (fi->precision<(int)slen)){
    slen = (cwal_size_t)fi->precision;
  }
  if(width>0 && ((int)slen < width)){ /* right alignment */
    int i = (int)slen - width;
    for( ; !rc && (i < 0); ++i){
      rc = cwal_buffer_append(e, tgt, &fi->pad, 1);
    }
  }
  if(!rc){
    rc = cwal_buffer_reserve(e, tgt, tgt->used + (slen*2) + 1);
    if(!rc){
      char const * hchar = "0123456789ABCDEF";
      cwal_size_t i;
      char hex[2] = {0,0};
      for( i = 0; !rc && (i < slen); ++i ){
        hex[0] = hchar[(cstr[i] & 0xF0)>>4];
        hex[1] = hchar[cstr[i] & 0x0F];
        rc = cwal_buffer_append(e, tgt, hex, 2);
      }
      if(!rc && (width<0)){ /* right padding */
        int i = (int)slen - -width;
        for( ; !rc && (i < 0); ++i){
          rc = cwal_buffer_append(e, tgt, &fi->pad, 1);
        }
      }
    }
  }
  return rc;
}

int cwal_format_part_json( cwal_engine * e,
                           cwal_buffer * tgt,
                           cwal_format_info * fi,
                           cwal_value const * v ){
  cwal_json_output_opt outOpt = cwal_json_output_opt_empty;
  outOpt.indent = fi->width;
  return cwal_json_output_buffer(e, (cwal_value *)/*EVIL!*/v,
                                 tgt, &outOpt);
}


int cwal_format_part_char( cwal_engine * e,
                           cwal_buffer * tgt,
                           cwal_format_info * fi,
                           cwal_value const * v ){
  unsigned char intBuf[10] = {0,0,0,0,0,0,0,0,0,0};
  int rc = 0;
  cwal_size_t slen = 0;
  char const * cstr = cwal_value_get_cstr(v, &slen);
  cwal_int_t i;
  if(!cstr){
    i = cwal_value_get_integer(v);
    if(i<0){
      fi->errMsg = "Invalid (negative) character value.";
      return CWAL_RC_RANGE;
    }
    slen = (cwal_size_t)cwal_utf8_char_to_cstr( (unsigned int)i, intBuf,
                                                sizeof(intBuf)/sizeof(intBuf[0]));
    assert(slen && slen<5);
    if(slen>4){
      fi->errMsg = "Invalid UTF character value.";
      return CWAL_RC_RANGE;
    }
    cstr = (char const *)intBuf;
  }
  if(!slen){
    fi->errMsg = "Invalid (empty) character.";
    return CWAL_RC_RANGE;
  }else{
    unsigned char const * begin = (unsigned char const *)cstr;
    unsigned char const * eof = begin + slen;
    unsigned char const * tail = begin;
    int width = !fi->width ? 0 : ((fi->width<0) ? -fi->width : fi->width);
    cwal_size_t chLen;
    cwal_int_t n;
    cwal_utf8_read_char( begin, eof, &tail );
    if(tail == begin){
      fi->errMsg = "Could not decode UTF character.";
      return CWAL_RC_RANGE;
    }
    chLen =  tail - begin;
    n = (cwalFormatNoPrecision == fi->precision)
      ? 1
      : ((fi->precision>=0) ? fi->precision : 0)
      ;
    /* MARKER(("n=%d, width=%d, precision=%d, \n",(int)n, fi->width, fi->precision)); */
    if((fi->width>0) && (width > n)){ /* left-pad */
      for( ; !rc && (width > n); --width){
        rc = cwal_buffer_append( e, tgt, &fi->pad, 1 );
      }
    }
    for( i = 0; !rc && (i < n); ++i ){
      rc = cwal_buffer_append( e, tgt, begin, chLen );
    }
    if(!rc && (fi->width<0) && (width > n)){ /* right-pad */
      for( ; !rc && (width > n); --width){
        rc = cwal_buffer_append( e, tgt, &fi->pad, 1 );
      }
    }
    return rc;
  }
}

static int cwal_httpurl_needs_escape( int c )
{
  /*
    Definition of "safe" and "unsafe" chars
    was taken from:

    http://www.codeguru.com/cpp/cpp/cpp_mfc/article.php/c4029/
  */
  return ( (c >= 32 && c <=47)
           || ( c>=58 && c<=64)
           || ( c>=91 && c<=96)
           || ( c>=123 && c<=126)
           || ( c<32 || c>=127)
           );
}

int cwal_format_part_urlenc( cwal_engine * e,
                             cwal_buffer * tgt,
                             cwal_format_info * fi,
                             cwal_value const * v ){
  cwal_size_t slen = 0;
  char const * str = cwal_value_get_cstr(v, &slen);
  char ch;
  int rc = 0;
  cwal_size_t j = 0;
  char * bufpt = NULL;
  cwal_size_t const oldUsed = e->buffer.used;
  static char const * hex = "0123456789ABCDEF";
  if(!str){
    fi->errMsg = "URL encoding requires a String value.";
    return CWAL_RC_TYPE;
  }
  rc = cwal_buffer_reserve(e, &e->buffer, oldUsed + slen * 3 );
  if(rc) return rc;
  bufpt = (char *)(e->buffer.mem + oldUsed);
  ch = *str;
  for( ; !rc && ch; ch = *(++str) ){
    if(!cwal_httpurl_needs_escape(ch) ){
      bufpt[j++] = ch;
    }else{
      bufpt[j++] = '%';
      bufpt[j++] = hex[((ch>>4)&0xf)];
      bufpt[j++] = hex[(ch&0xf)];
    }
  }
  assert( (void *)bufpt == (void*)e->buffer.mem );
  rc = cwal_buffer_append(e, tgt, bufpt, j);
  e->buffer.used = oldUsed;
  e->buffer.mem[e->buffer.used] = 0;
  return 0;
}

/* 
   cwal_hexchar_to_int():

   For 'a'-'f', 'A'-'F' and '0'-'9', returns the appropriate decimal
   number.  For any other character it returns -1.
*/
static int cwal_hexchar_to_int( int ch )
{
  if( (ch>='0' && ch<='9') ) return ch-'0';
  else if( (ch>='a' && ch<='f') ) return ch-'a'+10;
  else if( (ch>='A' && ch<='F') ) return ch-'A'+10;
  else return -1;
}

static int cwal_is_xdigit( int ch ){
  return ('a'<=ch && 'f'>=ch)
    ? 1
    : (('A'<=ch && 'F'>=ch)
       ? 1
       : ('0'<=ch && '9'>=ch ? 1 : 0));
}

int cwal_format_part_urldec( cwal_engine * e,
                             cwal_buffer * tgt,
                             cwal_format_info * fi,
                             cwal_value const * v ){

  cwal_size_t slen = 0;
  char const * str = cwal_value_get_cstr(v, &slen);
  int rc = 0;
  char ch = 0, ch2 = 0;
  char xbuf[4];
  int decoded;
  if(!str){
    fi->errMsg = "URL decoding requires a String value.";
    return CWAL_RC_TYPE;
  }
  else if(!slen) return 0;
  else if(slen<3){
    /* can't be urlencoded */
    return cwal_buffer_append(e, tgt, str, slen);
  }
  ch = *str;
  while( !rc && ch ){
    if( ch == '%' ){
      ch = *(++str);
      ch2 = *(++str);
      if( cwal_is_xdigit((int)ch) &&
          cwal_is_xdigit((int)ch2) ){
        decoded = (cwal_hexchar_to_int( ch ) * 16)
          + cwal_hexchar_to_int( ch2 );
        xbuf[0] = (char)decoded;
        xbuf[1] = 0;
        rc = cwal_buffer_append(e, tgt, xbuf, 1);
        ch = *(++str);
        continue;
      }
      else{
        xbuf[0] = '%';
        xbuf[1] = ch;
        xbuf[2] = ch2;
        xbuf[3] = 0;
        rc = cwal_buffer_append(e, tgt, xbuf, 3);
        ch = *(++str);
        continue;
      }
    }
    else if( ch == '+' ){
      xbuf[0] = ' ';
      xbuf[1] = 0;
      rc = cwal_buffer_append(e, tgt, xbuf, 1);
      ch = *(++str);
      continue;
    }
    xbuf[0] = ch;
    xbuf[1] = 0;
    rc = cwal_buffer_append(e, tgt, xbuf, 1);
    ch = *(++str);
  }
  return rc;
}

/**
   Parse the range [b,*e) as a java.lang.String.format()-style
   format string:

   %N$[flags][width][.precision][type]
*/
static int cwal_format_parse_info( char const * b, char const ** e,
                                   cwal_format_info * fi,
                                   int * index ){
  char const * p = b;
  char const * x;
  char isMinus = 0;
  *fi = cwal_format_info_empty;
  for( ; (p < *e) && ('$' != *p); ++p ){
    if(('0'>*p) && ('9'<*p)){
      fi->errMsg = "Invalid argument index character after %.";
      return CWAL_RC_RANGE;
    }
  }
  /* MARKER(("fmt= b=[%s] p=[%s] e=[%c]\n",b, p, **e)); */
  if((p==b) || (p==*e)) {
    fi->errMsg = "Invalid format token.";
    return CWAL_RC_RANGE;
  }
  else if('$'!=*p) {
    /* MARKER(("fmt=[%s] [%s]\n",b, p)); */
    fi->errMsg = "Missing '$' after %N.";
    return CWAL_RC_RANGE;
  }
  *index = (int)strtol( b, (char **)&p, 10 );
  if(*index < 1){
    fi->errMsg = "Argument index must be greater than 0.";
    return CWAL_RC_RANGE;
  }
  *index = *index - 1 /* make it zero-based */;
  ++p /* skip '$' */;
  /* MARKER(("p=[%s]\n",p)); */
  if(*e == p){
    no_type:
    fi->errMsg = "No type specifier given in format token.";
    return CWAL_RC_TYPE;
  }
  /* Check for flag. */
  switch(*p){
    case '+':
      fi->flags |= CWAL_FMT_F_SIGNED;
      ++p;
      break;
    default: break;
  }
  /* Read [[-]width] ... */
  if('-'==*p){
    isMinus = 1;
    ++p;
  }
  x = p;
  if('0' == *x){ /* Make it 0-padded */
    fi->pad = '0';
    ++x;
    ++p;
  }
  for( ; (x<*e) && ('1'<=*x) && ('9'>=*x); ++x ){}
  /* MARKER(("x=[%s]\n",x)); */
  if(x==*e){
    fi->errMsg = "Unexpected end of width specifier.";
    return CWAL_RC_RANGE;
  }
  else if(x!=p){
    fi->width = (int)strtol( p, (char **)&p, 10 );
    if(p==*e) goto no_type;
    if(isMinus) fi->width = -fi->width;
    /* MARKER(("p=[%s], width=%d\n",p, fi->width)); */
  }
  /* Read [.precision] ... */
  /* MARKER(("p=[%s]\n",p)); */
  if('.'==*p){
    ++p;
    x = p;
    for( ; (x < *e) && ('0'<=*x) && ('9'>=*x); ++x ){}
    if(x==*e){
      fi->errMsg = "Unexpected end of precision specifier.";
      return CWAL_RC_RANGE;
    }
    else if(x==p){
      fi->errMsg = "Missing digits after '.'.";
      return CWAL_RC_RANGE;
    }
    else{
      fi->precision = (int)strtol( p, (char**)&p, 10 );
      /* MARKER(("p=[%s], precision=%d\n",p, fi->precision)); */
      if(p==*e) goto no_type;
    }
  }
  switch( *p ){
    case 'b': fi->type = CWAL_FMT_BOOL;
      ++p;
      break;
    case 'B': fi->type = CWAL_FMT_BLOB;
      ++p;
      break;
    case 'c': fi->type = CWAL_FMT_CHAR;
      ++p;
      break;
    case 'd': fi->type = CWAL_FMT_INT_DEC;
      ++p;
      break;
    case 'f': fi->type = CWAL_FMT_DOUBLE;
      ++p;
      break;
    case 'J': fi->type = CWAL_FMT_JSON;
      ++p;
      break;
    case 'N': fi->type = CWAL_FMT_NULL;
      ++p;
      break;
    case 'o': fi->type = CWAL_FMT_INT_OCTAL;
      ++p;
      break;
    case 'p': fi->type = CWAL_FMT_VOIDP;
      ++p;
      break;
    case 'q': fi->type = CWAL_FMT_STRING_SQL;
      ++p;
      break;
    case 'Q': fi->type = CWAL_FMT_STRING_SQL;
      fi->flags |= CWAL_FMT_F_ALT_FORM;
      ++p;
      break;
    case 's': fi->type = CWAL_FMT_STRING;
      ++p;
      break;
    case 'y': fi->type = CWAL_FMT_TYPENAME;
      ++p;
      break;
    case 'U': fi->type = CWAL_FMT_UNDEF;
      ++p;
      break;
    case 'x': fi->type = CWAL_FMT_INT_HEXLC;
      ++p;
      break;
    case 'X': fi->type = CWAL_FMT_INT_HEXUC;
      ++p;
      break;
    case 'r': fi->type = CWAL_FMT_URLENCODE;
      ++p;
      break;
    case 'R': fi->type = CWAL_FMT_URLDECODE;
      ++p;
      break;
    default:
      fi->errMsg = "Unknown format type specifier.";
      return CWAL_RC_RANGE;
  }
  switch(fi->type){
    case CWAL_FMT_URLENCODE:
    case CWAL_FMT_URLDECODE:
      if(fi->width){
        fi->errMsg = "Conversion does not support width.";
        return CWAL_RC_MISUSE;
      }
      CWAL_SWITCH_FALL_THROUGH;
    case CWAL_FMT_INT_DEC:
    case CWAL_FMT_INT_HEXLC:
    case CWAL_FMT_INT_HEXUC:
      if(fi->precision && cwalFormatNoPrecision != fi->precision){
        fi->errMsg = "Conversion does not support precision.";
        return CWAL_RC_MISUSE;
      }
      break;
    case CWAL_FMT_CHAR:
      if(fi->precision<0){
        fi->errMsg = "Character precision (repetition) may not be negative.";
        return CWAL_RC_MISUSE;
      }
      break;
    default:
      break;
  }
  *e = p;
  fi->f = CWAL_FORMAT_INFO[fi->type].f;
  return 0;
}

int cwal_buffer_format( cwal_engine * e, cwal_buffer * tgt,
                        char const * fmt, cwal_int_t fmtLen,
                        uint16_t argc, cwal_value * const * const argv){
  int rc = 0;
  char const * fpos = fmt;
  char const * fend;
  char const * tstart = fmt;
  cwal_format_info fi = cwal_format_info_empty;
  int index;
  int gotFmt = 0;
  cwal_size_t oldUsed;
  int ordPos = 0;
  if(!e || !tgt || !fmt ) return CWAL_RC_MISUSE;
  else if(!fmtLen) return 0;
  else if(fmtLen<0) fmtLen = (cwal_int_t)cwal_strlen(fmt);
  fend = fpos + fmtLen;
  oldUsed = tgt->used;
  rc = cwal_buffer_reserve(e, tgt, oldUsed + fmtLen);
  if(rc) return rc;
  for( ; !rc && (fpos < fend); ){
    index = -1;
    fi = cwal_format_info_empty;
    switch( *fpos ){
      case '%':{
        ++fpos;
        ++gotFmt;
        if(fpos==fend){
          fi.errMsg = "Unexpected end of input after '%'.";
          rc = CWAL_RC_RANGE;
          goto end;
        }
        else if('%'==*fpos){
          rc = cwal_buffer_append( e, tgt, tstart, fpos - tstart);
          tstart = ++fpos;
          continue;
        }
        else{
          char const * pend = fend;
          fi.position = ++ordPos;
          rc = cwal_format_parse_info( fpos, &pend, &fi, &index );
          tstart = fpos = pend;
          if(!rc){
            assert(index>=0);
            if(index>=(int)argc){
              fi.errMsg = "Argument index out of range.";
              rc = CWAL_RC_RANGE;
              goto end;
            }
            else if(!argv[index]){
              fi.f = CWAL_FORMAT_INFO[CWAL_FMT_NULL].f;
            }
            if(!fi.f){
              fi.errMsg =
                "Missing formatting function for "
                "type specifier.";
              rc = CWAL_RC_ERROR;
              goto end;
            }
            rc = fi.f( e, tgt, &fi, argv[index] );
            if(!rc) continue;
          }
          break;
        }
      }/* case '%' */
      default:
        ++fpos;
        break;
    }/*switch(*fpos)*/
    if(!rc){
      rc = cwal_buffer_append( e, tgt, tstart, fpos - tstart);
      tstart = fpos;
    }
    else break;
  }
  end:
  if(rc) switch(rc){
      case CWAL_RC_OOM:
        /* Pass through errors which might be introduced indirectly
           via client code if we expand this support to include custom
           formatters or callbacks. Note that if other errors pass
           through here we'll end up "overwriting" them as a formatting
           error.
        */
      case CWAL_RC_ASSERT:
      case CWAL_RC_EXIT:
      case CWAL_RC_FATAL:
      case CWAL_RC_EXCEPTION:
        break;
      default:{
        int errPos = (int)(fpos - fmt);
        int pedanticRc;
        tgt->used = oldUsed;
        pedanticRc = cwal_buffer_printf( e, tgt,
                                         "Formatting failed with "
                                         "code %d (%s) at format string "
                                         "byte offset %d.",
                                         rc, cwal_rc_cstr(rc), errPos - 1);
        if(!pedanticRc && fi.errMsg){
          cwal_buffer_printf(e, tgt, " Format token #%d%s%s", gotFmt,
                             fi.errMsg ? ": " : ".",
                             fi.errMsg ? fi.errMsg : "");
        }
      }
    }
  return rc;
}

#undef MARKER
#undef cwalFormatNoPrecision
