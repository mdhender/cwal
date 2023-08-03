/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=4 et sw=2 tw=80: */
/************************************************************************
The printf-like implementation in this file is based on the one found
in the sqlite3 distribution is in the Public Domain.

This copy was forked for use with the clob API in Feb 2008 by Stephan
Beal (http://wanderinghorse.net/home/stephan/) and modified to send
its output to arbitrary targets via a callback mechanism. Also
refactored the %X specifier handlers a bit to make adding/removing
specific handlers easier.

All code in this file is released into the Public Domain.

The printf implementation (cwal_printfv()) is pretty easy to extend
(e.g. adding or removing %-specifiers for cwal_printfv()) if you're
willing to poke around a bit and see how the specifiers are declared
and dispatched. For an example, grep for 'etSTRING' and follow it
through the process of declaration to implementation.

See below for several WHPRINTF_OMIT_xxx macros which can be set to
remove certain features/extensions.
************************************************************************/

#include <assert.h>
#include <stdio.h> /* FILE */
#include <string.h> /* strlen() */
#include <stdlib.h> /* free/malloc() */
#include <ctype.h>
#include <stdint.h>
#include <stdbool.h>
#include "wh/cwal/cwal_printf.h"
#include "wh/cwal/cwal.h" /* ONLY for cwal_rc_e and cwal_buffer */

typedef long double LONGDOUBLE_TYPE;

#if !defined(CWAL_SWITCH_FALL_THROUGH)
#if defined(__GNUC__) && !defined(__clang__) && (__GNUC__ >= 7)
/*
  gcc v7+ treats implicit 'switch' fallthrough as a warning
  (i.e. error because we always build with -Wall -Werror -Wextra
  -pedantic). Because now it's apparently considered modern to warn
  for using perfectly valid features of the language. Holy cow, guys,
  what the hell were you thinking!?!?!?

  Similarly braindead, clang #defines __GNUC__.
*/
#  define CWAL_SWITCH_FALL_THROUGH __attribute__ ((fallthrough))
#else
#  define CWAL_SWITCH_FALL_THROUGH
#endif
#endif


/*
  If WHPRINTF_OMIT_FLOATING_POINT is defined to a true value, then
  floating point conversions are disabled.
*/
#ifndef WHPRINTF_OMIT_FLOATING_POINT
#  define WHPRINTF_OMIT_FLOATING_POINT 0
#endif

/*
  If WHPRINTF_OMIT_SIZE is defined to a true value, then the %n
  specifier is disabled. This must be disabled as of 2021-07-09, as
  the %n semantics no longer match the appendf() semantics.
*/
#define WHPRINTF_OMIT_SIZE 1

/*
  If WHPRINTF_OMIT_SQL is defined to a true value, then
  the %q and %Q specifiers are disabled.
*/
#ifndef WHPRINTF_OMIT_SQL
#  define WHPRINTF_OMIT_SQL 0
#endif

/*
  If WHPRINTF_OMIT_HTML is defined to a true value then the %h (HTML
  escape), %t (URL escape), and %T (URL unescape) specifiers are
  disabled.
*/
#ifndef WHPRINTF_OMIT_HTML
#  define WHPRINTF_OMIT_HTML 0
#endif

/**
   If true, the %j (JSON string) format is enabled.
*/
#define WHPRINTF_ENABLE_JSON 1

/**
   If WHPRINTF_OMIT_DYNSTRING is defined to a true value then the
   %z (dynamically-allocated string) specifier is disabled.
*/
#ifndef WHPRINTF_OMIT_DYNSTRING
#  define WHPRINTF_OMIT_DYNSTRING 1
#endif

/*
  Most C compilers handle variable-sized arrays, so we enable
  that by default. Some (e.g. tcc) do not, so we provide a way
  to disable it: set WHPRINTF_HAVE_VARARRAY to 0

  One approach would be to look at:

  defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L)

  but some compilers support variable-sized arrays even when not
  explicitly running in c99 mode.
*/
#if !defined(WHPRINTF_HAVE_VARARRAY)
#  if defined(__TINYC__)
#    define WHPRINTF_HAVE_VARARRAY 0
#  else
#    if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L)
#        define WHPRINTF_HAVE_VARARRAY 1 /*use 1 in C99 mode */
#    else
#        define WHPRINTF_HAVE_VARARRAY 0
#    endif
#  endif
#endif

/**
   WHPRINTF_CHARARRAY is a helper to allocate variable-sized arrays.
   This exists mainly so this code can compile with the tcc compiler.
*/
#if WHPRINTF_HAVE_VARARRAY
#  define WHPRINTF_CHARARRAY(V,N) char V[N+1]; memset(V,0,N+1);
#  define WHPRINTF_CHARARRAY_FREE(V)
#else
#  define WHPRINTF_CHARARRAY(V,N) char * V = (char *)malloc(N); memset(V,0,N);
#  define WHPRINTF_CHARARRAY_FREE(V) free(V)
#endif

/*
  Conversion types fall into various categories as defined by the
  following enumeration.
*/
enum PrintfCategory {
etRADIX = 1, /* Integer types.  %d, %x, %o, and so forth */
etFLOAT = 2, /* Floating point.  %f */
etEXP = 3, /* Exponentional notation. %e and %E */
etGENERIC = 4, /* Floating or exponential, depending on exponent. %g */
etSIZE = 5, /* Return number of characters processed so far. %n */
etSTRING = 6, /* Strings. %s */
#if !WHPRINTF_OMIT_DYNSTRING
etDYNSTRING = 7, /* Dynamically allocated strings. %z */
#endif
etPERCENT = 8, /* Percent symbol. %% */
etCHARX = 9, /* Characters. %c */
/* The rest are extensions, not normally found in printf() */
etCHARLIT = 10, /* Literal characters.  %' */
#if !WHPRINTF_OMIT_SQL
etSQLESCAPE = 11, /* Strings with '\'' doubled.  %q */
etSQLESCAPE2 = 12, /* Strings with '\'' doubled and enclosed in '',
                      NULL pointers replaced by SQL NULL.  %Q */
etSQLESCAPE3 = 16, /* %!Q -> "Strings" with '\"' doubled */
#endif /* !WHPRINTF_OMIT_SQL */
etPOINTER = 15, /* The %p conversion */
etORDINAL = 17, /* %r -> 1st, 2nd, 3rd, 4th, etc.  English only */
#if ! WHPRINTF_OMIT_HTML
etHTML = 18, /* %h -> basic HTML escaping. */
etURLENCODE = 19, /* %t -> URL encoding. */
etURLDECODE = 20, /* %T -> URL decoding. */
etCWALBUFFER = 21, /* %b */
#endif
#if WHPRINTF_ENABLE_JSON
etJSONSTR = 22,
#endif
etPLACEHOLDER = 100
};

/*
  An "etByte" is an 8-bit unsigned value.
*/
typedef unsigned char etByte;

/*
  Each builtin conversion character (ex: the 'd' in "%d") is described
  by an instance of the following structure
*/
typedef struct et_info {   /* Information about each format field */
  char fmttype;            /* The format field code letter */
  etByte base;             /* The base for radix conversion */
  etByte flags;            /* One or more of FLAG_ constants below */
  etByte type;             /* Conversion paradigm */
  etByte charset;          /* Offset into aDigits[] of the digits string */
  etByte prefix;           /* Offset into aPrefix[] of the prefix string */
} et_info;

/*
  Allowed values for et_info.flags
*/
enum et_info_flags { FLAG_SIGNED = 1,    /* True if the value to convert is signed */
                     FLAG_EXTENDED = 2,  /* True if for internal/extended use only. */
                     FLAG_STRING = 4     /* Allow infinity precision */
};

/*
  Historically, the following table was searched linearly, so the most
  common conversions were kept at the front.

  Change 2008 Oct 31 by Stephan Beal: we reserve an array or ordered
  entries for all chars in the range [32..126]. Format character
  checks can now be done in constant time by addressing that array
  directly.  This takes more static memory, but reduces the time and
  per-call overhead costs of cwal_printfv().
*/
static const char aDigits[] = "0123456789ABCDEF0123456789abcdef";
static const char aPrefix[] = "-x0\000X0";
static const et_info fmtinfo[] = {
/**
   If WHPRINTF_FMTINFO_FIXED is 1 then we use the original
   implementation: a linear list of entries. Search time is linear. If
   WHPRINTF_FMTINFO_FIXED is 0 then we use a fixed-size array which
   we index directly using the format char as the key.
*/
/*
  These entries MUST stay in ASCII order, sorted
  on their fmttype member!
*/
{' '/*32*/, 0, 0, 0, 0, 0 },
{'!'/*33*/, 0, 0, 0, 0, 0 },
{'"'/*34*/, 0, 0, 0, 0, 0 },
{'#'/*35*/, 0, 0, 0, 0, 0 },
{'$'/*36*/, 0, 0, 0, 0, 0 },
{'%'/*37*/, 0, 0, etPERCENT, 0, 0 },
{'&'/*38*/, 0, 0, 0, 0, 0 },
{'\''/*39*/, 0, 0, 0, 0, 0 },
{'('/*40*/, 0, 0, 0, 0, 0 },
{')'/*41*/, 0, 0, 0, 0, 0 },
{'*'/*42*/, 0, 0, 0, 0, 0 },
{'+'/*43*/, 0, 0, 0, 0, 0 },
{','/*44*/, 0, 0, 0, 0, 0 },
{'-'/*45*/, 0, 0, 0, 0, 0 },
{'.'/*46*/, 0, 0, 0, 0, 0 },
{'/'/*47*/, 0, 0, 0, 0, 0 }
/* reminder to self: i'd like to port in fossil's path-sanitization
   here (%/), which replaces \\ with /, but it requires allocating
   and cwal_printf() has no mechanism to pass in a cwal_engine
   instance, which means that it can neither use the correct
   allocator nor propagate allocation failure properly back to cwal.

   Or... it _could_ be reimplemented to stream output in multiple
   calls, so allocation wouldn't strictly need to allocate. Hmmm.

   Update late 2021: the return semantics have since been changed to 0
   for success, non-0 on error, so it's now possible to propagate an
   allocation error.
*/,
{'0'/*48*/, 0, 0, 0, 0, 0 },
{'1'/*49*/, 0, 0, 0, 0, 0 },
{'2'/*50*/, 0, 0, 0, 0, 0 },
{'3'/*51*/, 0, 0, 0, 0, 0 },
{'4'/*52*/, 0, 0, 0, 0, 0 },
{'5'/*53*/, 0, 0, 0, 0, 0 },
{'6'/*54*/, 0, 0, 0, 0, 0 },
{'7'/*55*/, 0, 0, 0, 0, 0 },
{'8'/*56*/, 0, 0, 0, 0, 0 },
{'9'/*57*/, 0, 0, 0, 0, 0 },
{':'/*58*/, 0, 0, 0, 0, 0 },
{';'/*59*/, 0, 0, 0, 0, 0 },
{'<'/*60*/, 0, 0, 0, 0, 0 },
{'='/*61*/, 0, 0, 0, 0, 0 },
{'>'/*62*/, 0, 0, 0, 0, 0 },
{'?'/*63*/, 0, 0, 0, 0, 0 },
{'@'/*64*/, 0, 0, 0, 0, 0 },
{'A'/*65*/, 0, 0, 0, 0, 0 },
{'B'/*66*/, 0, 0, 0, 0, 0 },
{'C'/*67*/, 0, 0, 0, 0, 0 },
{'D'/*68*/, 0, 0, 0, 0, 0 },
{'E'/*69*/, 0, FLAG_SIGNED, etEXP, 14, 0 },
{'F'/*70*/, 0, 0, 0, 0, 0 },
{'G'/*71*/, 0, FLAG_SIGNED, etGENERIC, 14, 0 },
{'H'/*72*/, 0, 0, 0, 0, 0 },
{'I'/*73*/, 0, 0, 0, 0, 0 },
{'J'/*74*/, 0, 0, 0, 0, 0 },
{'K'/*75*/, 0, 0, 0, 0, 0 },
{'L'/*76*/, 0, 0, 0, 0, 0 },
{'M'/*77*/, 0, 0, 0, 0, 0 },
{'N'/*78*/, 0, 0, 0, 0, 0 },
{'O'/*79*/, 0, 0, 0, 0, 0 },
{'P'/*80*/, 0, 0, 0, 0, 0 },
#if WHPRINTF_OMIT_SQL
{'Q'/*81*/, 0, 0, 0, 0, 0 },
#else
{'Q'/*81*/, 0, FLAG_STRING, etSQLESCAPE2, 0, 0 },
#endif
{'R'/*82*/, 0, 0, 0, 0, 0 },
{'S'/*83*/, 0, 0, 0, 0, 0 },
{'T'/*84*/,  0, FLAG_STRING, etURLDECODE, 0, 0 },
{'U'/*85*/, 0, 0, 0, 0, 0 },
{'V'/*86*/, 0, 0, 0, 0, 0 },
{'W'/*87*/, 0, 0, 0, 0, 0 },
{'X'/*88*/, 16, 0, etRADIX,      0,  4 },
{'Y'/*89*/, 0, 0, 0, 0, 0 },
{'Z'/*90*/, 0, 0, 0, 0, 0 },
{'['/*91*/, 0, 0, 0, 0, 0 },
{'\\'/*92*/, 0, 0, 0, 0, 0 },
{']'/*93*/, 0, 0, 0, 0, 0 },
{'^'/*94*/, 0, 0, 0, 0, 0 },
{'_'/*95*/, 0, 0, 0, 0, 0 },
{'`'/*96*/, 0, 0, 0, 0, 0 },
{'a'/*97*/, 0, 0, 0, 0, 0 },
{'b'/*98*/, 0, 0, etCWALBUFFER, 0, 0 },
{'c'/*99*/, 0, 0, etCHARX,      0,  0 },
{'d'/*100*/, 10, FLAG_SIGNED, etRADIX,      0,  0 },
{'e'/*101*/, 0, FLAG_SIGNED, etEXP,        30, 0 },
{'f'/*102*/, 0, FLAG_SIGNED, etFLOAT,      0,  0},
{'g'/*103*/, 0, FLAG_SIGNED, etGENERIC,    30, 0 },
{'h'/*104*/, 0, FLAG_STRING, etHTML, 0, 0 },
{'i'/*105*/, 10, FLAG_SIGNED, etRADIX,      0,  0},
#if WHPRINTF_ENABLE_JSON
{'j'/*106*/, 0, 0, etJSONSTR, 0, 0 },
#else
{'j'/*106*/, 0, 0, 0, 0, 0 },
#endif
{'k'/*107*/, 0, 0, 0, 0, 0 },
{'l'/*108*/, 0, 0, 0, 0, 0 },
{'m'/*109*/, 0, 0, 0, 0, 0 },
{'n'/*110*/, 0, 0, etSIZE, 0, 0 },
{'o'/*111*/, 8, 0, etRADIX,      0,  2 },
{'p'/*112*/, 16, 0, etPOINTER, 16, 1 },
#if WHPRINTF_OMIT_SQL
{'q'/*113*/, 0, 0, 0, 0, 0 },
#else
{'q'/*113*/, 0, FLAG_STRING, etSQLESCAPE,  0, 0 },
#endif
{'r'/*114*/, 10, (FLAG_EXTENDED|FLAG_SIGNED), etORDINAL,    0,  0},
{'s'/*115*/, 0, FLAG_STRING, etSTRING,     0,  0 },
{'t'/*116*/,  0, FLAG_STRING, etURLENCODE, 0, 0 },
{'u'/*117*/, 10, 0, etRADIX,      0,  0 },
{'v'/*118*/, 0, 0, 0, 0, 0 },
{'w'/*119*/, 0, 0, 0, 0, 0 },
{'x'/*120*/, 16, 0, etRADIX,      16, 1  },
{'y'/*121*/, 0, 0, 0, 0, 0 },
#if !WHPRINTF_OMIT_DYNSTRING
{'z'/*122*/, 0, FLAG_STRING, etDYNSTRING,  0,  0},
#else
{'z'/*122*/, 0, 0, 0,  0,  0},
#endif
{'{'/*123*/, 0, 0, 0, 0, 0 },
{'|'/*124*/, 0, 0, 0, 0, 0 },
{'}'/*125*/, 0, 0, 0, 0, 0 },
{'~'/*126*/, 0, 0, 0, 0, 0 },
};
#define etNINFO  (sizeof(fmtinfo)/sizeof(fmtinfo[0]))

#if ! WHPRINTF_OMIT_FLOATING_POINT
/*
  "*val" is a double such that 0.1 <= *val < 10.0
  Return the ascii code for the leading digit of *val, then
  multiply "*val" by 10.0 to renormalize.
  **
  Example:
  input:     *val = 3.14159
  output:    *val = 1.4159    function return = '3'
  **
  The counter *cnt is incremented each time.  After counter exceeds
  16 (the number of significant digits in a 64-bit float) '0' is
  always returned.
*/
static int et_getdigit(LONGDOUBLE_TYPE *val, int *cnt){
  int digit;
  LONGDOUBLE_TYPE d;
  if( (*cnt)++ >= 16 ) return '0';
  digit = (int)*val;
  d = digit;
  digit += '0';
  *val = (*val - d)*10.0;
  return digit;
}
#endif /* !WHPRINTF_OMIT_FLOATING_POINT */

/*
  On machines with a small(?) stack size, you can redefine the
  WHPRINTF_BUF_SIZE to be less than 350.  But beware - for smaller
  values some %f conversions may go into an infinite loop.
*/
#ifndef WHPRINTF_BUF_SIZE
#  define WHPRINTF_BUF_SIZE 350  /* Size of the output buffer for numeric conversions */
#endif

/**
   cwal_printf_spec_handler is an almost-generic interface for farming
   work out of cwal_printfv()'s code into external functions.  It
   doesn't actually save much (if any) overall code, but it makes the
   cwal_printfv() code more manageable.

   REQUIREMENTS of implementations:

   - Expects an implementation-specific vargp pointer.
   cwal_printfv() passes a pointer to the converted value of
   an entry from the format va_list. If it passes a type
   other than the expected one, undefined results.

   - If it calls pf then it must return the return value
   from that function.

   - If it calls pf it must do: pf( pfArg, D, N ), where D is
   the data to export and N is the number of bytes to export.
   It may call pf() an arbitrary number of times

   - If pf() returns non-0, it must return that code.

   - On success, must return 0. On error, non-0.

   SIGNIFICANT(?) LIMITATIONS:

   - Has no way of iterating over the format string, so handling
   precisions and such here can't work.
*/
typedef int (*cwal_printf_spec_handler)( cwal_printf_appender_f pf,
                                         void * pfArg,
                                         unsigned int pfLen,
                                         void * vargp );


#if !WHPRINTF_OMIT_DYNSTRING
/**
   cwal_printf_spec_handler for etDYNSTRING types.  It assumes that varg
   is a non-const (char *). It behaves identically to spec_string() and
   then calls free() on that (char *).
*/
static int spech_dynstring( cwal_printf_appender_f pf,
                            void * pfArg,
                            unsigned int pfLen,
                            void * varg )
{
  char const * ch = (char const *) varg;
  int const ret = ch ? pf( pfArg, ch, pfLen ) : 0;
  free( (char *) varg );
  return ret;
}
#endif

#if !WHPRINTF_OMIT_HTML
static int spech_string_to_html( cwal_printf_appender_f pf,
                                 void * pfArg,
                                 unsigned int pfLen,
                                 void * varg )
{
  char const * ch = (char const *) varg;
  unsigned int i;
  int rc = 0;
  if( ! ch ) return 0;
  for( i = 0; rc==0 && (i<pfLen) && *ch; ++ch, ++i ){
    switch( *ch ){
      case '<': rc = pf( pfArg, "&lt;", 4 );
        break;
      case '&': rc = pf( pfArg, "&amp;", 5 );
        break;
      default:
        rc = pf( pfArg, ch, 1 );
        break;
    };
  }
  return rc;
}

static int httpurl_needs_escape( int c ){
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

/**
   The handler for the etURLENCODE specifier.

   It expects varg to be a string value, which it will preceed to
   encode using an URL encoding algothrim (certain characters are
   converted to %XX, where XX is their hex value) and passes the
   encoded string to pf(). It returns the total length of the output
   string.
*/
static int spech_urlencode( cwal_printf_appender_f pf,
                            void * pfArg,
                            unsigned int pfLen,
                            void * varg )
{
  char const * str = (char const *) varg;
  int rc = 0;
  char ch = 0;
  char const * hex = "0123456789ABCDEF";
#define xbufsz 10
  char xbuf[xbufsz];
  int slen = 0;
  if( ! str ) return 0;
  memset( xbuf, 0, xbufsz );
  ch = *str;
#define xbufsz 10
  slen = 0;
  for( ; ch && rc==0; ch = *(++str) ){
    if( ! httpurl_needs_escape( ch ) ){
      rc = pf( pfArg, str, 1 );
      continue;
    }
    else {
      xbuf[0] = '%';
      xbuf[1] = hex[((ch>>4)&0xf)];
      xbuf[2] = hex[(ch&0xf)];
      xbuf[3] = 0;
      slen = 3;
      rc = pf( pfArg, xbuf, slen );
    }
  }
#undef xbufsz
  if(pfLen){/*avoid unused param warning*/}
  return rc;
}

/* 
   hexchar_to_int():

   For 'a'-'f', 'A'-'F' and '0'-'9', returns the appropriate decimal
   number.  For any other character it returns -1.
*/
static int hexchar_to_int( int ch ){
  if( (ch>='a' && ch<='f') ) return ch-'a'+10;
  else if( (ch>='A' && ch<='F') ) return ch-'A'+10;
  else if( (ch>='0' && ch<='9') ) return ch-'0';
  return -1;
}

/**
   The handler for the etURLDECODE specifier.

   It expects varg to be a ([const] char *), possibly encoded
   with URL encoding. It decodes the string using a URL decode
   algorithm and passes the decoded string to
   pf(). It returns the total length of the output string.
   If the input string contains malformed %XX codes then this
   function will return prematurely.
*/
static int spech_urldecode( cwal_printf_appender_f pf,
                            void * pfArg,
                            unsigned int pfLen,
                            void * varg ){
  char const * str = (char const *) varg;
  int rc = 0;
  char ch = 0;
  char ch2 = 0;
  char xbuf[4];
  int decoded;
  char const * end = str + pfLen;
  if( !str || !pfLen ) return 0;
  ch = *str;
  while(0==rc && ch && str<end){
    if( ch == '%' ){
      if(str+2>=end) goto outro/*invalid partial encoding - simply skip it*/;
      ch = *(++str);
      ch2 = *(++str);
      if( isxdigit((int)ch) &&
          isxdigit((int)ch2) )
      {
        decoded = (hexchar_to_int( ch ) * 16)
          + hexchar_to_int( ch2 );
        xbuf[0] = (char)decoded;
        xbuf[1] = 0;
        rc = pf( pfArg, xbuf, 1 );
        ch = *(++str);
        continue;
      }else{
        xbuf[0] = '%';
        xbuf[1] = ch;
        xbuf[2] = ch2;
        xbuf[3] = 0;
        rc = pf( pfArg, xbuf, 3 );
        ch = *(++str);
        continue;
      }
    }else if( ch == '+' ){
      xbuf[0] = ' ';
      xbuf[1] = 0;
      rc = pf( pfArg, xbuf, 1 );
      ch = *(++str);
      continue;
    }
    outro:
    xbuf[0] = ch;
    xbuf[1] = 0;
    rc = pf( pfArg, xbuf, 1 );
    ch = *(++str);
  }
  return rc;
}

#endif /* !WHPRINTF_OMIT_HTML */


#if !WHPRINTF_OMIT_SQL
/**
   Quotes the (char *) varg as an SQL string 'should' be quoted. The
   exact type of the conversion is specified by xtype, which must be
   one of etSQLESCAPE, etSQLESCAPE2, etSQLESCAPE3.

   Search this file for those constants to find the associated
   documentation.
*/
static int spech_sqlstring( int xtype, cwal_printf_appender_f pf,
                            void * pfArg, unsigned int pfLen,
                            void * varg ){
  enum { BufLen = 512 };
  char buf[BufLen];
  unsigned int i = 0, j = 0;
  int ch;
  char const q = xtype==etSQLESCAPE3 ?'"':'\''; /* Quote character */
  char const * escarg = (char const *) varg;
  bool const isnull = escarg==0;
  bool const needQuote =
    !isnull && (xtype==etSQLESCAPE2
                //|| xtype==etBLOBSQL
                || xtype==etSQLESCAPE3);
  if( isnull ){
    if(xtype==etSQLESCAPE2||xtype==etSQLESCAPE3){
      escarg = "NULL";
      pfLen = 4;
    }else{
      escarg = "(NULL)";;
      pfLen = 6;
    }
  }
  if( needQuote ) buf[j++] = q;
  for(i=0; (ch=escarg[i])!=0 && i<pfLen; ++i){
    buf[j++] = ch;
    if( ch==q ) buf[j++] = ch;
    if(j+2>=BufLen){
      int const rc = pf( pfArg, &buf[0], j );
      if(rc) return rc;
      j = 0;
    }
  }
  if( needQuote ) buf[j++] = q;
  buf[j] = 0;
  return j>0 ? pf( pfArg, &buf[0], j ) : 0;
}
#endif /* !WHPRINTF_OMIT_SQL */


#if WHPRINTF_ENABLE_JSON
/*
  Pedantic licensing sidebar: much of what follows was ported into
  this file from the BSD-licensed libfossil project, but the person
  who did so (Stephan Beal) is the same person who implemented that
  one and is thus free to relicense it for this tree. Some parts of it
  were derived from the sqlite3 tree, and those were Public Domain to
  begin with.
*/
/* TODO? Move these UTF8 bits into the public API? We have this code
   in the core cwal lib BUT this file tends to get copy/pasted across
   diverse source trees, so having this pared-down form of it here is
   useful.
*/
/*
** This lookup table is used to help decode the first byte of
** a multi-byte UTF8 character.
**
** Taken from sqlite3:
** https://www.sqlite.org/src/artifact?ln=48-61&name=810fbfebe12359f1
*/
static const unsigned char appendf_utfTrans1[] = {
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
  0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
  0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  0x00, 0x01, 0x02, 0x03, 0x00, 0x01, 0x00, 0x00
};
static unsigned int appendf_utf8_read_char(
  const unsigned char *zIn,       /* First byte of UTF-8 character */
  const unsigned char *zTerm,     /* Pretend this byte is 0x00 */
  const unsigned char **pzNext    /* Write first byte past UTF-8 char here */
){
  /*
    Adapted from sqlite3:
    https://www.sqlite.org/src/artifact?ln=155-165&name=810fbfebe12359f1
  */
  unsigned c;
  if(zIn>=zTerm){
    *pzNext = zTerm;
    c = 0;
  }else{
    c = (unsigned int)*(zIn++);
    if( c>=0xc0 ){
      c = appendf_utfTrans1[c-0xc0];
      while( zIn!=zTerm && (*zIn & 0xc0)==0x80 )
        c = (c<<6) + (0x3f & *(zIn++));
      if( c<0x80
          || (c&0xFFFFF800)==0xD800
          || (c&0xFFFFFFFE)==0xFFFE ) c = 0xFFFD;
    }
    *pzNext = zIn;
  }
  return c;
}

static int appendf_utf8_char_to_cstr(unsigned int c, unsigned char *output,
                                     unsigned char length){
    /* Stolen from the internet, adapted from several variations which
      all _seem_ to have derived from librdf. */
    unsigned char size=0;

    /* check for illegal code positions:
     * U+D800 to U+DFFF (UTF-16 surrogates)
     * U+FFFE and U+FFFF
     */
    if((c > 0xD7FF && c < 0xE000)
       || c == 0xFFFE || c == 0xFFFF) return -1;

    /* Unicode 3.2 only defines U+0000 to U+10FFFF and UTF-8 encodings of it */
    if(c > 0x10ffff) return -1;
    
    if (c < 0x00000080) size = 1;
    else if (c < 0x00000800) size = 2;
    else if (c < 0x00010000) size = 3;
    else size = 4;
    if(!output) return (int)size;
    else if(size > length) return -1;
    else switch(size) {
      case 0:
          assert(!"can't happen anymore");
          output[0] = 0;
          return 0;
      case 4:
          output[3] = 0x80 | (c & 0x3F);
          c = c >> 6;
          c |= 0x10000;
          CWAL_SWITCH_FALL_THROUGH;
      case 3:
          output[2] = 0x80 | (c & 0x3F);
          c = c >> 6;
          c |= 0x800;
          CWAL_SWITCH_FALL_THROUGH;
      case 2:
          output[1] = 0x80 | (c & 0x3F);
          c = c >> 6;
          c |= 0xc0; 
          CWAL_SWITCH_FALL_THROUGH;
      case 1:
        output[0] = (unsigned char)c;
        CWAL_SWITCH_FALL_THROUGH;
      default:
        return (int)size;
    }
}

struct SpechJson {
  char const * z;
  bool addQuotes;
  bool escapeSmallUtf8;
};

/**
   cwal_printf_spec_handler for etJSONSTR. It assumes that varg is a
   SpechJson struct instance.
*/
static int spech_json( cwal_printf_appender_f pf, void * pfArg,
                       unsigned int pfLen, void * varg )
{
  struct SpechJson const * state = (struct SpechJson *)varg;
  int pfRc = 0;
  const unsigned char *z = (const unsigned char *)state->z;
  const unsigned char *zEnd = z + pfLen;
  const unsigned char * zNext = 0;
  unsigned int c;
  unsigned char c1;

#define out(X,N) pfRc=pf(pfArg, (char const *)(X), N); \
  if(pfRc) return pfRc
#define outc c1 = (unsigned char)c; out(&c1,1)
  if(!z){
    out("null",4);
    return pfRc;
  }    
  if(state->addQuotes){
    out("\"", 1);
  }
  for( ; (z < zEnd) && (c=appendf_utf8_read_char(z, zEnd, &zNext));
       z = zNext ){
    if( c=='\\' || c=='"' ){
      out("\\", 1);
      outc;
    }else if( c<' ' ){
      out("\\",1);
      if( c=='\n' ){
        out("n",1);
      }else if( c=='\r' ){
        out("r",1);
      }else{
        unsigned char ubuf[5] = {'u',0,0,0,0};
        int i;
        for(i = 4; i>0; --i){
          ubuf[i] = "0123456789abcdef"[c&0xf];
          c >>= 4;
        }
        out(ubuf,5);
      }
    }else if(c<128){
      outc;
    }/* At this point we know that c is part of a multi-byte
        character. We're assuming legal UTF8 input, which means
        emitting a surrogate pair if the value is > 0xffff. */
    else if(c<0xFFFF){
      unsigned char ubuf[12];
      if(state->escapeSmallUtf8){
        /* Output char in \u#### form. */
        snprintf((char *)ubuf, sizeof(ubuf), "\\u%04x", c)
          /* gcc incorrectly misdiagnoses the output length here,
             thus our buffer and "n" value is bigger than
             necessary */;
        out(ubuf, 6);
      }else{
        /* Output character literal. */
        int const n = appendf_utf8_char_to_cstr(c, ubuf, 4);
        if(n<0){
          out("?",1);
        }else{
          assert(n>0);
          out(ubuf, n);
        }
      }
    }else{
      /* Surrogate pair. */
      unsigned char ubuf[24];
      c -= 0x10000;
      snprintf((char *)ubuf, sizeof(ubuf), "\\u%04x\\u%04x",
               (0xd800 | (c>>10)),
               (0xdc00 | (c & 0x3ff))
               /* gcc incorrectly misdiagnoses the output length here,
                  thus our buffer and "n" value is bigger than
                  necessary */);
      out(ubuf, 12);
    }
  }
  if(state->addQuotes){
    out("\"",1);
  }
  return pfRc;
#undef out
#undef outc
}
#endif /* WHPRINTF_ENABLE_JSON */


/*
   Find the length of a string as long as that length does not
   exceed N bytes.  If no zero terminator is seen in the first
   N bytes then return N.  If N is negative, then this routine
   is an alias for strlen().
*/
static int StrNLen32(const char *z, int N){
  int n = 0;
  while( (N-- != 0) && *(z++)!=0 ){ n++; }
  return n;
}

/**
   Internal helper for %#W.Ps format.
*/
static void appendf__utf8_altform(char const * z, int * pLength,
                                  int * pPrecision, int * pWidth){
  /* Treat %#W.Ps as a width/precision limit of W resp. P UTF8
     characters instead of bytes. */
  int pC = 0/*precision, chars*/, pB = 0/*precision, bytes*/,
    wC = 0/*width, chars*/, wB = 0/*width, bytes*/;
  char const * const zEnd = z + *pLength;
  int lc;
  assert(z ? zEnd>=z : NULL==zEnd);
  while( z < zEnd ){
    switch(0xF0 & *z) {
      case 0xF0: lc = (z[1]&0x80 && z[2]&0x80 && z[3]&0x80) ? 4 : 0; break;
      case 0xE0: lc = (z[1]&0x80 && z[2]&0x80) ? 3 : 0; break;
      case 0xC0: lc = (z[1]&0x80) ? 2 : 0; break;
      case 0x80: lc = 0; break;
      default: lc = 1; break;
    }
    if(!lc) break;
    else if(wC<*pWidth && (*pPrecision<=0 || pC<*pPrecision)){ ++wC; wB+=lc;}
    if(pC<*pPrecision){ ++pC; pB+=lc;}
    z+=lc;
  }
  if(*pPrecision>0) *pLength = pB;
  if(*pWidth>0) *pWidth = *pWidth - wC + wB;
}


/*
  The root printf program.  All variations call this core.  It
  implements most of the common printf behaviours plus (optionally)
  some extended ones.

  INPUTS:

  pfAppend : The is a cwal_printf_appender_f function which is responsible
  for accumulating the output. If pfAppend returns a negative integer
  then processing stops immediately.

  pfAppendArg : is ignored by this function but passed as the first
  argument to pfAppend. pfAppend will presumably use it as a data
  store for accumulating its string.

  fmt : This is the format string, as in the usual printf().

  ap : This is a pointer to a list of arguments.  Same as in
  vprintf() and friends.

  OUTPUTS:

  The return value is the total number of characters sent to the
  function "func".  Returns -1 on a error.

  Note that the order in which automatic variables are declared below
  seems to make a big difference in determining how fast this beast
  will run.

  Much of this code dates back to the early 1980's, supposedly.

  Known change history (most historic info has been lost):

  10 Feb 2008 by Stephan Beal: refactored to remove the 'useExtended'
  flag (which is now always on). Added the cwal_printf_appender_f typedef to
  make this function generic enough to drop into other source trees
  without much work.

  31 Oct 2008 by Stephan Beal: refactored the et_info lookup to be
  constant-time instead of linear.

  2021-07 by Stephan Beal: changed semantics to return 0 on success,
  non-0 on error, as the conventional semantics are pretty useless in
  practice and we need to be able to notify clients on OOM conditions.
*/
int cwal_printfv(cwal_printf_appender_f pfAppend,          /* Accumulate results here */
                  void * pfAppendArg,                /* Passed as first arg to pfAppend. */
                  const char *fmt,                   /* Format string */
                  va_list ap                         /* arguments */
                  ){
  /**
     HISTORIC NOTE (author and year unknown, as is whether this still applies):

     Note that the order in which automatic variables are declared below
     seems to make a big difference in determining how fast this beast
     will run.
  */
  int rc = 0;          /* accumulated output count */
  int pfrc = 0;              /* result from calling pfAppend */
  int c;                     /* Next character in the format string */
  char *bufpt = 0;           /* Pointer to the conversion buffer */
  int precision;             /* Precision of the current field */
  int length;                /* Length of the field */
  int idx;                   /* A general purpose loop counter */
  int width;                 /* Width of the current field */
  etByte flag_leftjustify;   /* True if "-" flag is present */
  etByte flag_plussign;      /* True if "+" flag is present */
  etByte flag_blanksign;     /* True if " " flag is present */
  etByte flag_alternateform; /* True if "#" flag is present */
  etByte flag_altform2;      /* True if "!" flag is present */
  etByte flag_zeropad;       /* True if field width constant starts with zero */
  etByte flag_long;          /* True if "l" flag is present */
  etByte flag_longlong;      /* True if the "ll" flag is present */
  etByte done;               /* Loop termination flag */
  uint64_t longvalue;   /* Value for integer types */
  LONGDOUBLE_TYPE realvalue; /* Value for real types */
  const et_info *infop = 0;      /* Pointer to the appropriate info structure */
  char buf[WHPRINTF_BUF_SIZE];       /* Conversion buffer */
  char prefix;               /* Prefix character.  "+" or "-" or " " or '\0'. */
  etByte xtype = 0;              /* Conversion paradigm */
  char * zExtra = 0;              /* Extra memory used for etTCLESCAPE conversions */
#if ! WHPRINTF_OMIT_FLOATING_POINT
  int  exp, e2;              /* exponent of real numbers */
  double rounder;            /* Used for rounding floating point values */
  etByte flag_dp;            /* True if decimal point should be shown */
  etByte flag_rtz;           /* True if trailing zeros should be removed */
  etByte flag_exp;           /* True to force display of the exponent */
  int nsd;                   /* Number of significant digits returned */
#endif


  /* WHPRINTF_RETURN, WHPRINTF_CHECKERR, and WHPRINTF_SPACES
     are internal helpers.
  */
#define WHPRINTF_RETURN if( zExtra ) free(zExtra); return pfrc ? pfrc : rc;
#if WHPRINTF_HAVE_VARARRAY
  /*
    This impl possibly mallocs
  */
#define WHPRINTF_CHECKERR if( pfrc ) { WHPRINTF_RETURN; } (void)0
#define WHPRINTF_SPACES(N)                      \
  if(1){                                        \
    WHPRINTF_CHARARRAY(zSpaces,N);              \
    memset( zSpaces,' ',N);                     \
    pfrc = pfAppend(pfAppendArg, zSpaces, N);   \
    WHPRINTF_CHARARRAY_FREE(zSpaces);           \
    WHPRINTF_CHECKERR;                          \
  }(void)0
#else
  /* But this one is subject to potential wrong output
     on "unusual" inputs.

     FIXME: turn this into a loop on N, so we can do away with the
     limit.

     reminder to self: libfossil's fork uses a similar, arguably
     better, approach, which falls back to allocating.
  */
#define WHPRINTF_CHECKERR if( pfrc!=0 ) { WHPRINTF_RETURN; }
#define WHPRINTF_SPACES(N)                                              \
  if(1){ \
    enum { BufSz = 128 };                                               \
    char zSpaces[BufSz];                                                \
    unsigned int n = ((N)>=BufSz) ? BufSz : (int)N;                     \
    memset( zSpaces,' ',n);                                             \
    pfrc = pfAppend(pfAppendArg, zSpaces, n);                           \
    WHPRINTF_CHECKERR;                                                  \
  }(void)0
#endif

  length = 0;
  bufpt = 0;
  for(; (c=(*fmt))!=0; ++fmt){
    if( c!='%' ){
      int amt;
      bufpt = (char *)fmt;
      amt = 1;
      while( (c=(*++fmt))!='%' && c!=0 ) amt++;
      pfrc = pfAppend( pfAppendArg, bufpt, amt);
      WHPRINTF_CHECKERR;
      if( c==0 ) break;
    }
    if( (c=(*++fmt))==0 ){
      pfrc = pfAppend( pfAppendArg, "%", 1);
      WHPRINTF_CHECKERR;
      break;
    }
    /* Find out what flags are present */
    flag_leftjustify = flag_plussign = flag_blanksign = 
      flag_alternateform = flag_altform2 = flag_zeropad = 0;
    done = 0;
    do{
      switch( c ){
        case '-':   flag_leftjustify = 1;     break;
        case '+':   flag_plussign = 1;        break;
        case ' ':   flag_blanksign = 1;       break;
        case '#':   flag_alternateform = 1;   break;
        case '!':   flag_altform2 = 1;        break;
        case '0':   flag_zeropad = 1;         break;
        default:    done = 1;                 break;
      }
    }while( !done && (c=(*++fmt))!=0 );
    /* Get the field width */
    width = 0;
    if( c=='*' ){
      width = va_arg(ap,int);
      if( width<0 ){
        flag_leftjustify = 1;
        width = width >= -2147483647 ? -width : 0;
      }
      c = *++fmt;
    }else{
      unsigned wx = 0;
      while( c>='0' && c<='9' ){
        wx = wx * 10 + c - '0';
        width = width*10 + c - '0';
        c = *++fmt;
      }
      width = wx & 0x7fffffff;
    }
    if( width > WHPRINTF_BUF_SIZE-10 ){
      width = WHPRINTF_BUF_SIZE-10;
    }
    /* Get the precision */
    if( c=='.' ){
      precision = 0;
      c = *++fmt;
      if( c=='*' ){
        precision = va_arg(ap,int);
        if( precision<0 ) precision = -precision;
        c = *++fmt;
        if( precision<0 ){
          precision = precision >= -2147483647 ? -precision : -1;
        }

      }else{
        unsigned px = 0;
        while( c>='0' && c<='9' ){
          px = px*10 + c - '0';
          c = *++fmt;
        }
        precision = px & 0x7fffffff;
      }
    }else{
      precision = -1;
    }
    /* Get the conversion type modifier */
    if( c=='l' ){
      flag_long = 1;
      c = *++fmt;
      if( c=='l' ){
        flag_longlong = 1;
        c = *++fmt;
      }else{
        flag_longlong = 0;
      }
    }else{
      flag_long = flag_longlong = 0;
    }
    /* Fetch the info entry for the field */
    infop = 0;
#define FMTNDX(N) (N - fmtinfo[0].fmttype)
#define FMTINFO(N) (fmtinfo[ FMTNDX(N) ])
    infop = ((c>=(fmtinfo[0].fmttype)) && (c<fmtinfo[etNINFO-1].fmttype))
      ? &FMTINFO(c)
      : 0;
    /*fprintf(stderr,"char '%c'/%d @ %d,  type=%c/%d\n",c,c,FMTNDX(c),infop->fmttype,infop->type);*/
    if( infop ) xtype = infop->type;
#undef FMTINFO
#undef FMTNDX
    zExtra = 0;
    if( (!infop) || (!infop->type) ){
      WHPRINTF_RETURN;
    }

    /* Limit the precision to prevent overflowing buf[] during conversion */
    if( precision>WHPRINTF_BUF_SIZE-40 && (infop->flags & FLAG_STRING)==0 ){
      precision = WHPRINTF_BUF_SIZE-40;
    }

    /*
      At this point, variables are initialized as follows:
      **
      flag_alternateform          TRUE if a '#' is present.
      flag_altform2               TRUE if a '!' is present.
      flag_plussign               TRUE if a '+' is present.
      flag_leftjustify            TRUE if a '-' is present or if the
      field width was negative.
      flag_zeropad                TRUE if the width began with 0.
      flag_long                   TRUE if the letter 'l' (ell) prefixed
      the conversion character.
      flag_longlong               TRUE if the letter 'll' (ell ell) prefixed
      the conversion character.
      flag_blanksign              TRUE if a ' ' is present.
      width                       The specified field width.  This is
      always non-negative.  Zero is the default.
      precision                   The specified precision.  The default
      is -1.
      xtype                       The class of the conversion.
      infop                       Pointer to the appropriate info struct.
    */
    switch( xtype ){
      case etPOINTER:
        flag_longlong = sizeof(char*)==sizeof(int64_t);
        flag_long = sizeof(char*)==sizeof(long int);
        CWAL_SWITCH_FALL_THROUGH;
      case etORDINAL:
      case etRADIX:
        if( infop->flags & FLAG_SIGNED ){
          int64_t v;
          if( flag_longlong )   v = va_arg(ap,int64_t);
          else if( flag_long )  v = va_arg(ap,long int);
          else                  v = va_arg(ap,int);
          if( v<0 ){
            longvalue = -v;
            prefix = '-';
          }else{
            longvalue = v;
            if( flag_plussign )        prefix = '+';
            else if( flag_blanksign )  prefix = ' ';
            else                       prefix = 0;
          }
        }else{
          if( flag_longlong )   longvalue = va_arg(ap,uint64_t);
          else if( flag_long )  longvalue = va_arg(ap,unsigned long int);
          else                  longvalue = va_arg(ap,unsigned int);
          prefix = 0;
        }
        if( longvalue==0 ) flag_alternateform = 0;
        if( flag_zeropad && precision<width-(prefix!=0) ){
          precision = width-(prefix!=0);
        }
        bufpt = &buf[WHPRINTF_BUF_SIZE-1];
        if( xtype==etORDINAL ){
          /** i sure would like to shake the hand of whoever figured this out: */
          static const char zOrd[] = "thstndrd";
          int x = longvalue % 10;
          if( x>=4 || (longvalue/10)%10==1 ){
            x = 0;
          }
          buf[WHPRINTF_BUF_SIZE-3] = zOrd[x*2];
          buf[WHPRINTF_BUF_SIZE-2] = zOrd[x*2+1];
          bufpt -= 2;
        }
        {
          const char *cset;
          int base;
          cset = &aDigits[infop->charset];
          base = infop->base;
          do{                                           /* Convert to ascii */
            *(--bufpt) = cset[longvalue%base];
            longvalue = longvalue/base;
          }while( longvalue>0 );
        }
        length = &buf[WHPRINTF_BUF_SIZE-1]-bufpt;
        for(idx=precision-length; idx>0; idx--){
          *(--bufpt) = '0';                             /* Zero pad */
        }
        if( prefix ) *(--bufpt) = prefix;               /* Add sign */
        if( flag_alternateform && infop->prefix ){      /* Add "0" or "0x" */
          const char *pre;
          char x;
          pre = &aPrefix[infop->prefix];
          if( *bufpt!=pre[0] ){
            for(; (x=(*pre))!=0; pre++) *(--bufpt) = x;
          }
        }
        length = &buf[WHPRINTF_BUF_SIZE-1]-bufpt;
        break;
      case etFLOAT:
      case etEXP:
      case etGENERIC:
        realvalue = va_arg(ap,double);
#if ! WHPRINTF_OMIT_FLOATING_POINT
        if( precision<0 ) precision = 6;         /* Set default precision */
        if( precision>WHPRINTF_BUF_SIZE/2-10 ) precision = WHPRINTF_BUF_SIZE/2-10;
        if( realvalue<0.0 ){
          realvalue = -realvalue;
          prefix = '-';
        }else{
          if( flag_plussign )          prefix = '+';
          else if( flag_blanksign )    prefix = ' ';
          else                         prefix = 0;
        }
        if( xtype==etGENERIC && precision>0 ) precision--;
#if 0
        /* Rounding works like BSD when the constant 0.4999 is used.  Wierd! */
        for(idx=precision & 0xfff, rounder=0.4999; idx>0; idx--, rounder*=0.1);
#else
        /* It makes more sense to use 0.5 */
        for(idx=precision & 0xfff, rounder=0.5; idx>0; idx--, rounder*=0.1){}
#endif
        if( xtype==etFLOAT ) realvalue += rounder;
        /* Normalize realvalue to within 10.0 > realvalue >= 1.0 */
        exp = 0;
#if 1
        if( (realvalue)!=(realvalue) ){
          /* from sqlite3: #define sqlite3_isnan(X)  ((X)!=(X)) */
          /* This weird array thing is to avoid constness violations
             when assinging, e.g. "NaN" to bufpt.
          */
          static char NaN[4] = {'N','a','N','\0'};
          bufpt = NaN;
          length = 3;
          break;
        }
#endif
        if( realvalue>0.0 ){
          while( realvalue>=1e32 && exp<=350 ){ realvalue *= 1e-32; exp+=32; }
          while( realvalue>=1e8 && exp<=350 ){ realvalue *= 1e-8; exp+=8; }
          while( realvalue>=10.0 && exp<=350 ){ realvalue *= 0.1; exp++; }
          while( realvalue<1e-8 && exp>=-350 ){ realvalue *= 1e8; exp-=8; }
          while( realvalue<1.0 && exp>=-350 ){ realvalue *= 10.0; exp--; }
          if( exp>350 || exp<-350 ){
            if( prefix=='-' ){
              static char Inf[5] = {'-','I','n','f','\0'};
              bufpt = Inf;
            }else if( prefix=='+' ){
              static char Inf[5] = {'+','I','n','f','\0'};
              bufpt = Inf;
            }else{
              static char Inf[4] = {'I','n','f','\0'};
              bufpt = Inf;
            }
            length = strlen(bufpt);
            break;
          }
        }
        bufpt = buf;
        /*
          If the field type is etGENERIC, then convert to either etEXP
          or etFLOAT, as appropriate.
        */
        flag_exp = xtype==etEXP;
        if( xtype!=etFLOAT ){
          realvalue += rounder;
          if( realvalue>=10.0 ){ realvalue *= 0.1; exp++; }
        }
        if( xtype==etGENERIC ){
          flag_rtz = !flag_alternateform;
          if( exp<-4 || exp>precision ){
            xtype = etEXP;
          }else{
            precision = precision - exp;
            xtype = etFLOAT;
          }
        }else{
          flag_rtz = 0;
        }
        if( xtype==etEXP ){
          e2 = 0;
        }else{
          e2 = exp;
        }
        nsd = 0;
        flag_dp = (precision>0) | flag_alternateform | flag_altform2;
        /* The sign in front of the number */
        if( prefix ){
          *(bufpt++) = prefix;
        }
        /* Digits prior to the decimal point */
        if( e2<0 ){
          *(bufpt++) = '0';
        }else{
          for(; e2>=0; e2--){
            *(bufpt++) = et_getdigit(&realvalue,&nsd);
          }
        }
        /* The decimal point */
        if( flag_dp ){
          *(bufpt++) = '.';
        }
        /* "0" digits after the decimal point but before the first
           significant digit of the number */
        for(e2++; e2<0 && precision>0; precision--, e2++){
          *(bufpt++) = '0';
        }
        /* Significant digits after the decimal point */
        while( (precision--)>0 ){
          *(bufpt++) = et_getdigit(&realvalue,&nsd);
        }
        /* Remove trailing zeros and the "." if no digits follow the "." */
        if( flag_rtz && flag_dp ){
          while( bufpt[-1]=='0' ) *(--bufpt) = 0;
          /* assert( bufpt>buf ); */
          if( bufpt[-1]=='.' ){
            if( flag_altform2 ){
              *(bufpt++) = '0';
            }else{
              *(--bufpt) = 0;
            }
          }
        }
        /* Add the "eNNN" suffix */
        if( flag_exp || (xtype==etEXP && exp) ){
          *(bufpt++) = aDigits[infop->charset];
          if( exp<0 ){
            *(bufpt++) = '-'; exp = -exp;
          }else{
            *(bufpt++) = '+';
          }
          if( exp>=100 ){
            *(bufpt++) = (exp/100)+'0';                /* 100's digit */
            exp %= 100;
          }
          *(bufpt++) = exp/10+'0';                     /* 10's digit */
          *(bufpt++) = exp%10+'0';                     /* 1's digit */
        }
        *bufpt = 0;

        /* The converted number is in buf[] and zero terminated. Output it.
           Note that the number is in the usual order, not reversed as with
           integer conversions. */
        length = bufpt-buf;
        bufpt = buf;

        /* Special case:  Add leading zeros if the flag_zeropad flag is
           set and we are not left justified */
        if( flag_zeropad && !flag_leftjustify && length < width){
          int i;
          int nPad = width - length;
          for(i=width; i>=nPad; i--){
            bufpt[i] = bufpt[i-nPad];
          }
          i = prefix!=0;
          while( nPad-- ) bufpt[i++] = '0';
          length = width;
        }
#endif /* !WHPRINTF_OMIT_FLOATING_POINT */
        break;
#if !WHPRINTF_OMIT_SIZE
#error "etSIZE (%n) cannot work with the 2021+ semantics."
      case etSIZE:
        *(va_arg(ap,int*)) = outCount;
        length = width = 0;
        break;
#endif
      case etPERCENT:
        buf[0] = '%';
        bufpt = buf;
        length = 1;
        break;
      case etCHARLIT:
      case etCHARX:
        c = buf[0] = (xtype==etCHARX ? va_arg(ap,int) : *++fmt);
        if( precision>=0 ){
          for(idx=1; idx<precision; idx++) buf[idx] = c;
          length = precision;
        }else{
          length =1;
        }
        bufpt = buf;
        break;
      case etSTRING: {
        bufpt = va_arg(ap,char*);
        length = bufpt
          ? StrNLen32(bufpt,
                      (precision>0 && flag_alternateform)
                      ? precision*4/*max bytes per char*/
                      : (precision>=0 ? precision : -1))
          : (int)0;
        if(flag_alternateform && length && (precision>0 || width>0)){
          appendf__utf8_altform(bufpt, &length, &precision, &width);
        }else if( length && precision>=0 && precision<length ){
          length = precision;
        }
        break;
      }
      case etCWALBUFFER:{
        cwal_buffer * const cb = va_arg(ap,cwal_buffer*);
        bufpt = (char *)cb->mem;
        length = bufpt ? (int)cb->used : (int)0;
        if(flag_alternateform && length && (precision>0 || width>0)){
          appendf__utf8_altform(bufpt, &length, &precision, &width);
        }else if( length && precision>=0 && precision<length ){
          length = precision;
        }
        break;
      }
#if !WHPRINTF_OMIT_DYNSTRING
      case etDYNSTRING: {
        /* etDYNSTRING needs to be handled separately because it
           free()s its argument (which isn't available outside this
           block). This means, though, that %-#z does not work.
        */
        bufpt = va_arg(ap,char*);
        length = bufpt
          ? StrNLen32(bufpt,
                      (precision>0 && flag_alternateform)
                      ? precision*4/*max bytes per char*/
                      : (precision>=0 ? precision : -1))
          : (int)0;
        if(flag_alternateform && length && (precision>0 || width>0)){
          appendf__utf8_altform(bufpt, &length, &precision, &width);
        }else if( length && precision>=0 && precision<length ){
          length = precision;
        }
        pfrc = spech_dynstring( pfAppend, pfAppendArg,
                                length, bufpt );
        bufpt = NULL;
        WHPRINTF_CHECKERR;
        length = 0;
        break;
      }
#endif
#if WHPRINTF_ENABLE_JSON
      case etJSONSTR: {
        struct SpechJson state;
        bufpt = va_arg(ap,char *);
        length = bufpt ? (int)strlen(bufpt) : 0;
        state.z = bufpt;
        state.addQuotes = flag_altform2 ? true : false;
        state.escapeSmallUtf8 = flag_alternateform ? true : false;
        pfrc = spech_json( pfAppend, pfAppendArg, (unsigned)length, &state );
        bufpt = NULL;
        WHPRINTF_CHECKERR;
        length = 0;
        break;
      }
#endif
#if ! WHPRINTF_OMIT_HTML
      case etHTML:{
        bufpt = va_arg(ap,char*);
        length = bufpt ? strlen(bufpt) : 0;
        pfrc = spech_string_to_html( pfAppend, pfAppendArg,
                                     (precision>=0 && precision<length) ? precision : length,
                                     bufpt );
        WHPRINTF_CHECKERR;
        length = 0;
        break;
      }
      case etURLENCODE:{
        bufpt = va_arg(ap,char*);
        length = bufpt ? strlen(bufpt) : 0;
        pfrc = spech_urlencode( pfAppend, pfAppendArg,
                                (precision>=0 && precision<length) ? precision : length,
                                bufpt );
        WHPRINTF_CHECKERR;
        length = 0;
        break;
      }
      case etURLDECODE:{
        bufpt = va_arg(ap,char*);
        length = bufpt ? strlen(bufpt) : 0;
        pfrc = spech_urldecode( pfAppend, pfAppendArg,
                                (precision>=0 && precision<length) ? precision : length,
                                bufpt );
        WHPRINTF_CHECKERR;
        length = 0;
        break;
      }
#endif /* WHPRINTF_OMIT_HTML */
#if ! WHPRINTF_OMIT_SQL
      case etSQLESCAPE:
      case etSQLESCAPE2:
      case etSQLESCAPE3: {
        if(flag_altform2 && etSQLESCAPE2==xtype){
          xtype = etSQLESCAPE3;
        }
#if 0
        /* Potential TODO... */
        if(etBLOBSQL==xtype){
          cwal_buffer * const b = va_arg(ap,cwal_buffer*);
          bufpt = b ? fsl_buffer_str(b) : NULL;
          length = b ? (int)fsl_buffer_size(b) : 0;
          if(flag_altform2) xtype = etSQLESCAPE3;
        }else{
          bufpt = va_arg(ap,char*);
          length = bufpt ? (int)strlen(bufpt) : 0;
        }
#else
        bufpt = va_arg(ap,char*);
        length = bufpt ? (int)strlen(bufpt) : 0;
#endif
        pfrc = spech_sqlstring( xtype, pfAppend, pfAppendArg,
                                (precision>=0 && precision<length) ? precision : length,
                                bufpt );
        WHPRINTF_CHECKERR;
        length = 0;
      }
#endif /* !WHPRINTF_OMIT_SQL */
    }/* End switch over the format type */
    /*
      The text of the conversion is pointed to by "bufpt" and is
      "length" characters long.  The field width is "width".  Do
      the output.
    */
    if( !flag_leftjustify ){
      int nspace;
      nspace = width-length;
      if( nspace>0 ){
        WHPRINTF_SPACES(nspace);
      }
    }
    if( length>0 ){
      pfrc = pfAppend( pfAppendArg, bufpt, length);
      WHPRINTF_CHECKERR;
    }
    if( flag_leftjustify ){
      int nspace;
      nspace = width-length;
      if( nspace>0 ){
        WHPRINTF_SPACES(nspace);
      }
    }
    if( zExtra ){
      free(zExtra);
      zExtra = 0;
    }
  }/* End for loop over the format string */
  WHPRINTF_RETURN;
} /* End of function */


#undef WHPRINTF_SPACES
#undef WHPRINTF_CHECKERR
#undef WHPRINTF_RETURN
#undef WHPRINTF_OMIT_FLOATING_POINT
#undef WHPRINTF_OMIT_SIZE
#undef WHPRINTF_OMIT_SQL
#undef WHPRINTF_BUF_SIZE
#undef WHPRINTF_OMIT_HTML

int cwal_printf(cwal_printf_appender_f pfAppend,
                void * pfAppendArg,
                const char *fmt,
                ... ){
  va_list vargs;
  int ret;
  va_start( vargs, fmt );
  ret = cwal_printfv( pfAppend, pfAppendArg, fmt, vargs );
  va_end(vargs);
  return ret;
}


int cwal_printf_FILE_appender( void * a, char const * s, unsigned int n ){
  FILE * fp = (FILE *)a;
  if( ! fp ) return -1;
  else{
    const unsigned long ret = (unsigned long)fwrite( s, sizeof(char), n, fp );
    return (ret == n) ? 0 : CWAL_RC_IO;
  }
}

int cwal_printfv_FILE( FILE * fp, char const * fmt, va_list vargs ){
  return cwal_printfv( cwal_printf_FILE_appender, fp, fmt, vargs );
}

int cwal_printf_FILE( FILE * fp, char const * fmt, ... ){
  va_list vargs;
  int ret;
  va_start( vargs, fmt );
  ret = cwal_printfv( cwal_printf_FILE_appender, fp, fmt, vargs );
  va_end(vargs);
  return ret;
}
