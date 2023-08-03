/*
** Copyright (c) 2006 D. Richard Hipp
**
** This program is free software; you can redistribute it and/or
** modify it under the terms of the Simplified BSD License (also
** known as the "2-Clause License" or "FreeBSD License").
**
** This program is distributed in the hope that it will be useful,
** but without any warranty; without even the implied warranty of
** merchantability or fitness for a particular purpose.
**
** Author contact information:
**   drh@hwaci.com
**   http://www.hwaci.com/drh/
**
**
** THIS COPY HAS BEEN MODIFIED FROM ITS ORIGINAL by stephan beal
** (http://wanderinghorse.net/home/stephan), who forked it out of
** fossil for generic re-use in the form of a standalone library.  The
** delta format has been named Felta, in reference to its source.
**
** Changes include:
**
** - Deltas are now sent to arbitrary output destinations via
** callbacks.
**
** - Extended to optionally support base 10, 16, or 64 integer
** encodings.  This requires an incompatible change, but this version
** is less than a day old, so there are no compatibility concerns ;).
*******************************************************************************
**
** This module implements the delta compress algorithm.
**
** Though developed specifically for fossil, the code in this file
** is generally appliable and is thus easily separated from the
** fossil source code base.  Nothing in this file depends on anything
** else in fossil.
*/
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h> /* FILE, fwrite() */
#include "felta.h"

#if 0
#  include <inttypes.h>
#else
/* MUST be exactly 2 bytes */
typedef unsigned short uint16_t;
/* MUST be exactly 4 bytes */
typedef unsigned int uint32_t;
#endif

#if !defined(CWAL_SWITCH_FALL_THROUGH)
#  if defined(__GNUC__) && !defined(__clang__) && (__GNUC__ >= 7)
/*
  #define CWAL_USING_GCC

  gcc v7+ treats implicit 'switch' fallthrough as a warning
  (i.e. error because we always build with -Wall -Werror -Wextra
  -pedantic). Because now it's apparently considered modern to warn
  for using perfectly valid features of the language. Holy cow, guys,
  what the hell were you thinking!?!?!?

  Similarly braindead, clang #defines __GNUC__.

  So now we need this ugliness throughout the source tree:

  #if defined(CWAL_USING_GCC)
  __attribute__ ((fallthrough));
  #endif

  It turns out that one can write "fall through", case sensitive (or
  not, depending on the warning level), as a _standalone C-style
  comment_ to (possibly) achieve the same result (depending on the
  -Wimplicit-fallthrough=N warning level, which can be set high enough
  to disable that workaround or change its case-sensitivity).

  Facepalm! FacePalm!! FACEPALM!!!

  PS: i wanted to strip comments from one large piece of generated
  code to reduce its distribution size, but then gcc fails to compile
  it because of these goddamned "fall through" comments. gcc devs, i
  hate you for this.
*/
#    define FELTA_SWITCH_FALL_THROUGH __attribute__ ((fallthrough))
#  else
#    define FELTA_SWITCH_FALL_THROUGH
#  endif
#endif


/**
   Set this to 0 to use the DRH-original encoding, 1 to use the
   SGB-slightly-extended encoded (incompatible).
*/
#define FELTA_EXTENDED_ENCODING 0


/*typedef uint32_t felta_uint_t;*/

/* FELTA_OMIT_DELTA_CKSUM_TEST enables an auto-check of a delta's
   checksum during felta_apply().

   2020-06-10: disabled because felta_verify_checksum(), despite
   having code identical to felta_apply2() (minus the output bits) is
   inexplicably failing in some tests. In the main fossil tree, from
   which this code derives, that test is also disabled.
*/
#define FELTA_OMIT_DELTA_CKSUM_TEST 1

#if !FELTA_EXTENDED_ENCODING
#define FELTA_INTEGER_BASE 64
#else
/**
   Set this to one of (10, 16, 64) to switch between various integer
   encodings. The delta size is marginally smaller for larger bases.

   The encoding/decoding base is a runtime decision but this macro
   sets the default. The delta writer adds a byte to the header to
   note the encoding, and the reader checks this.
*/
#define FELTA_INTEGER_BASE 10
#endif
#ifdef __cplusplus
extern "C" {
#endif

/*
** Macros for turning debugging printfs on and off
*/
#if 0
# define DEBUG1(X) X
#else
# define DEBUG1(X)
#endif
#if 0
#define DEBUG2(X) X
/*
** For debugging:
** Print 16 characters of text from zBuf
*/
static const char *print16(const char *z){
  int i;
  static char zBuf[20];
  for(i=0; i<16; i++){
    if( z[i]>=0x20 && z[i]<=0x7e ){
      zBuf[i] = z[i];
    }else{
      zBuf[i] = '.';
    }
  }
  zBuf[i] = 0;
  return zBuf;
}
#else
# define DEBUG2(X)
#endif

/*
** The width of a hash window in bytes.  The algorithm only works if
** this is a power of 2. Higher values give _lower_ allocation totals
** (per valgrind). The code originally used a 16-bit NHASH.
*/
#define NHASH 16

/*
** The current state of the rolling hash.
**
** z[] holds the values that have been hashed.  z[] is a circular buffer.
** z[i] is the first entry and z[(i+NHASH-1)%NHASH] is the last entry of 
** the window.
**
** Hash.a is the sum of all elements of hash.z[].  Hash.b is a weighted
** sum.  Hash.b is z[i]*NHASH + z[i+1]*(NHASH-1) + ... + z[i+NHASH-1]*1.
** (Each index for z[] should be modulo NHASH, of course.  The %NHASH operator
** is omitted in the prior expression for brevity.)
*/
typedef struct FeltaHash FeltaHash;
struct FeltaHash {
  uint16_t a, b;         /* Hash values */
  uint16_t i;            /* Start of the hash window */
  char z[NHASH];    /* The values that have been hashed */
};

/*
** Initialize the rolling hash using the first NHASH characters of z[]
*/
static void FeltaHash_init(FeltaHash *pHash, unsigned const char *z){
  uint16_t a, b, i;
  a = b = 0;
  for(i=0; i<NHASH; i++){
    a += z[i];
    b += (NHASH-i)*z[i];
    pHash->z[i] = z[i];
  }
  pHash->a = a & 0xffff;
  pHash->b = b & 0xffff;
  pHash->i = 0;
}

/*
** Advance the rolling hash by a single character "c"
*/
static void FeltaHash_next(FeltaHash *pHash, int c){
  uint16_t old = pHash->z[pHash->i];
  pHash->z[pHash->i] = c;
  pHash->i = (pHash->i+1)&(NHASH-1);
  pHash->a = pHash->a - old + c;
  pHash->b = pHash->b - NHASH*old + pHash->a;
}

/*
** Return a 32-bit hash value
*/
static uint32_t FeltaHash_32bit(FeltaHash *pHash){
  return (pHash->a & 0xffff) | (((uint32_t)(pHash->b & 0xffff))<<16);
}


#if FELTA_EXTENDED_ENCODING
/*
** Write a base-10 integer into the given buffer. Returns a negative
** value on error (if *nDest is too short for the number). On success
** returns 0, sets *nDest to the length of the stringified number, and
** copies that many bytes to dest.
*/
static int uintToString10( unsigned int iVal, char * dest, unsigned int * nDest ){
  enum { BufLen = 32 };
  char zBuf[BufLen] = {0,};
  char *z = &zBuf[BufLen];
  int zLen;

  *(--z) = '\0';
  *(--z) = (char)(48+(iVal%10));
  while( (iVal = (iVal/10))>0 ){
    *(--z) = (char)(48+(iVal%10));
    assert(z>zBuf);
  }
  zLen = zBuf+BufLen-z - 1/*NUL byte*/;
  if( *nDest <= zLen ){
    return FELTA_RC_RANGE;
  }else{
    *nDest = zLen;
    memcpy( dest, z, zLen + 1/*NUL byte*/ );
    return 0;
  }
}
#endif
 
#if FELTA_EXTENDED_ENCODING
/*
** Write a base-16 integer into the given buffer. Returns a negative
** value on error (if *nDest is too short for the number). On success
** returns 0, sets *nDest to the length of the stringified number, and
** copies that many bytes to dest.
*/
static int uintToString16( unsigned int iVal, char * dest, unsigned int * nDest ){
  static const char HEX_DIGITS[] = 
    "0123456789abcdef";
  enum { BufLen = 32 };
  char zBuf[BufLen] = {0,};
  char *z = &zBuf[BufLen];
  int zLen;

  *(--z) = '\0';
  *(--z) = (HEX_DIGITS[iVal%16]);
  while( (iVal = (iVal/16))>0 ){
    *(--z) = HEX_DIGITS[iVal%16];
    assert(z>zBuf);
  }
  zLen = zBuf+BufLen-z - 1/*NUL byte*/;
  if( *nDest <= zLen ){
    return FELTA_RC_RANGE;
  }else{
    *nDest = zLen;
    memcpy( dest, z, zLen + 1/*NUL byte*/ );
    return 0;
  }
}
#endif
  
/*
** Write an integer into the given buffer using the given numeric
** base. Returns the number of bytes written to pz.
*/
static unsigned int putInt(unsigned char base, unsigned int v, char *pz, unsigned int pzLen){
#if FELTA_EXTENDED_ENCODING
    switch(base){
    case 10:
      return ( 0 == uintToString10( v, pz, &pzLen ) )
        ? pzLen
        : 0;
    case 16:
      return ( 0 == uintToString16( v, pz, &pzLen ) )
        ? pzLen
        : 0;
    case 64:{
#endif
        static const char zDigits[] = 
        "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~";
      /*  123456789 123456789 123456789 123456789 123456789 123456789 123 */
      unsigned int i;
      int j;
      char zBuf[20];
      if( v==0 ){
        *pz++ = '0';
        return 1;
      }
      for(i=0; v>0; i++, v>>=6){
        zBuf[i] = zDigits[v&0x3f];
      }
      for(j=i-1; j>=0; j--){
        *pz++ = zBuf[j];
      }
      return i;
#if FELTA_EXTENDED_ENCODING
    }
    default:
      assert(!"No put-int impl for this FELTA_INTEGER_BASE");
      return 0;
  }
#else
  if(base || pzLen){/*unused*/}
#endif
}

    
/*
** Read bytes from *pz and convert them into a positive integer.  When
** finished, leave *pz pointing to the first character past the end of
** the integer.  The *pLen parameter holds the length of the string
** in *pz and is decremented once for each character in the integer.
** Returns the converted value. 
*/
static unsigned int getInt(unsigned char base, unsigned const char **pz, unsigned int *pLen){
#if FELTA_EXTENDED_ENCODING
  switch(base){
    case 10:{
      unsigned int i = 0;
      unsigned int v = 0;
      unsigned char ch;
      unsigned char const * z = *pz;
      unsigned char const * zStart = z;
      unsigned short len;
      for( ch = *z; (ch>='0' && ch<='9'); ch=*(++z) ){
      }
      if(z==zStart) {
        *pLen = 0;
        return 0;
      }
      len = z - zStart;
      *pLen -= len;
      *pz = z;
      --z;
      for( i = 1; z >= zStart; --z, i*=10 ){
        v += (*z-'0') * i;
      }
      return v;
    }
    case 16:{
      unsigned int i = 0;
      unsigned int v = 0;
      unsigned char ch;
      unsigned char * z = (unsigned char *)*pz;
      unsigned char const * zStart = z;
      unsigned short len;
#define HEXA(CH) ((CH>='a' && CH<='f') ? (CH-'a'+10) : (CH>='A' && CH<='F') ?(CH-'A'+10) :-1)
#define HEX(CH) ((CH>='0' && CH<='9') ? (CH-'0') : HEXA(CH))
      for( ch = *z; HEX(ch)>=0; ch=*(++z) ){
      }
      if(z==zStart) {
        *pLen = 0;
        return 0;
      }
  
      len = z - zStart;
      *pLen -= len;
      *pz = z;
      --z;
      for( i = 1; z >= zStart; --z, i*=16 ){
        assert(0<=HEX(*z));
        v += i * HEX(*z);
      }
#undef HEXA
#undef HEX
      /*printf("*z == [%c], v=%u\n",*(z), v);*/
      return v;
    }
    case 64:{
#endif
        static const signed char zValue[] = {
      -1, -1, -1, -1, -1, -1, -1, -1,   -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1,   -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1,   -1, -1, -1, -1, -1, -1, -1, -1,
      0,  1,  2,  3,  4,  5,  6,  7,    8,  9, -1, -1, -1, -1, -1, -1,
      -1, 10, 11, 12, 13, 14, 15, 16,   17, 18, 19, 20, 21, 22, 23, 24,
      25, 26, 27, 28, 29, 30, 31, 32,   33, 34, 35, -1, -1, -1, -1, 36,
      -1, 37, 38, 39, 40, 41, 42, 43,   44, 45, 46, 47, 48, 49, 50, 51,
      52, 53, 54, 55, 56, 57, 58, 59,   60, 61, 62, -1, -1, -1, 63, -1,
      };
      unsigned int v = 0;
      int c;
      unsigned char *z = (unsigned char*)*pz;
      unsigned char *zStart = z;
      while( (c = zValue[0x7f&*(z++)])>=0 ){
        v = (v<<6) + c;
      }
      z--;
      *pLen -= z - zStart;
      *pz = z;
      return v;
#if FELTA_EXTENDED_ENCODING
    }
    default:
      assert(!"No get-int impl for this FELTA_INTEGER_BASE");
      return 0;
  }
#else
  if(base){/*unused*/}
#endif
}

/*
** Return the number digits in the base-64 representation of a positive integer
*/
static unsigned int digit_count_b64(unsigned int v){
  unsigned int i, x;
  for(i=1, x=64; v>=x; i++, x <<= 6){}
  return i;
}

/*
** Compute a 32-bit checksum on the N-byte buffer.  Return the result.
*/
static uint32_t felta_checksum(unsigned const char *zIn, uint32_t N){
  const unsigned char *z = (const unsigned char *)zIn;
  unsigned sum0 = 0;
  unsigned sum1 = 0;
  unsigned sum2 = 0;
  unsigned sum3 = 0;
  while(N >= 16){
    sum0 += ((unsigned)z[0] + z[4] + z[8] + z[12]);
    sum1 += ((unsigned)z[1] + z[5] + z[9] + z[13]);
    sum2 += ((unsigned)z[2] + z[6] + z[10]+ z[14]);
    sum3 += ((unsigned)z[3] + z[7] + z[11]+ z[15]);
    z += 16;
    N -= 16;
  }
  while(N >= 4){
    sum0 += z[0];
    sum1 += z[1];
    sum2 += z[2];
    sum3 += z[3];
    z += 4;
    N -= 4;
  }
  sum3 += (sum2 << 8) + (sum1 << 16) + (sum0 << 24);
  switch(N){
    case 3:   sum3 += (z[2] << 8); FELTA_SWITCH_FALL_THROUGH;
    case 2:   sum3 += (z[1] << 16); FELTA_SWITCH_FALL_THROUGH;
    case 1:   sum3 += (z[0] << 24); FELTA_SWITCH_FALL_THROUGH;
    default:  ;
  }
  return sum3;
}

#if FELTA_EXTENDED_ENCODING
static char byteToBase(char b){
    switch(b){
      case 'd': return 10;
      case 'x': return 16;
      case 'b': return 64;
      default: return -1;
    }
}
static char const * baseToByte(char b){
  switch(b){
    case 10: return "d";
    case 16: return "x";
    case 64: return "b";
    default: return 0;
  }
}
#endif

int felta_create2(
  unsigned const char *zSrc,      /* The source or pattern file */
  unsigned int lenSrc,   /* Length of the source file */
  unsigned const char *zOut,      /* The target file */
  unsigned int lenOut,   /* Length of the target file */
  felta_output_f out,
  void * outState
){
  enum { IntegerBufSize = 50 /* buffer size for integer conversions. */};
  unsigned int i, base;
  unsigned int nHash;          /* Number of hash table entries */
  unsigned int *landmark;      /* Primary hash table */
  unsigned int *collide;       /* Collision chain */
  int lastRead = -1;           /* Last byte of zSrc read by a COPY command */
  int rc;                      /* generic return code checker. */
  unsigned int olen = 0;       /* current output length. */
  unsigned int total = 0;      /* total byte count. */
  FeltaHash h;
  char theBuf[IntegerBufSize] = {0,};
  char * intBuf = theBuf;
  static const char numBase = FELTA_INTEGER_BASE;
  /* Add the target file size to the beginning of the delta
  */
#define OUT(BLOB,LEN) rc=out(outState, BLOB, LEN); if(0 != rc) {assert(rc<0); return rc;} else total += LEN
#define OUTCH(CHAR) OUT(CHAR,1)
#define PINT(I) olen=putInt(numBase, I, intBuf, IntegerBufSize); OUT(intBuf,olen)
#if FELTA_EXTENDED_ENCODING
  OUTCH(baseToByte(numBase));
#endif
  PINT(lenOut);
  OUTCH("\n");

  /* If the source file is very small, it means that we have no
  ** chance of ever doing a copy command.  Just output a single
  ** literal segment for the entire target and exit.
  */
  if( lenSrc<=NHASH ){
    PINT(lenOut);
    OUTCH(":");
    OUT(zOut,lenOut);
    PINT((felta_checksum(zOut, lenOut)));
    OUTCH(";");
    return total;
  }

  /* Compute the hash table used to locate matching sections in the
  ** source file.
  */
  nHash = lenSrc/NHASH;
  collide = (unsigned int *)malloc( nHash*2*sizeof(int) );
  if(!collide){
    return FELTA_RC_OOM;
  }
  landmark = &collide[nHash];
  memset(landmark, -1, nHash*sizeof(int));
  memset(collide, -1, nHash*sizeof(int));
  for(i=0; i<lenSrc-NHASH; i+=NHASH){
    int hv;
    FeltaHash_init(&h, &zSrc[i]);
    hv = FeltaHash_32bit(&h) % nHash;
    collide[i/NHASH] = landmark[hv];
    landmark[hv] = i/NHASH;
  }

  /* Begin scanning the target file and generating copy commands and
  ** literal sections of the delta.
  */
  base = 0;    /* We have already generated everything before zOut[base] */
  while( base+NHASH<lenOut ){
    int iSrc, iBlock;
    int bestCnt, bestOfst=0, bestLitsz=0;
    FeltaHash_init(&h, &zOut[base]);
    i = 0;     /* Trying to match a landmark against zOut[base+i] */
    bestCnt = 0;
    while( 1 ){
      int hv;
      int limit = 250;

      hv = FeltaHash_32bit(&h) % nHash;
      DEBUG2( printf("LOOKING: %4d [%s]\n", base+i, print16(&zOut[base+i])); )
      iBlock = landmark[hv];
      while( iBlock>=0 && (limit--)>0 ){
        /*
        ** The hash window has identified a potential match against 
        ** landmark block iBlock.  But we need to investigate further.
        ** 
        ** Look for a region in zOut that matches zSrc. Anchor the search
        ** at zSrc[iSrc] and zOut[base+i].  Do not include anything prior to
        ** zOut[base] or after zOut[outLen] nor anything after zSrc[srcLen].
        **
        ** Set cnt equal to the length of the match and set ofst so that
        ** zSrc[ofst] is the first element of the match.  litsz is the number
        ** of characters between zOut[base] and the beginning of the match.
        ** sz will be the overhead (in bytes) needed to encode the copy
        ** command.  Only generate copy command if the overhead of the
        ** copy command is less than the amount of literal text to be copied.
        */
        int cnt, ofst, litsz;
        int j, k, x, y;
        int sz;

        /* Beginning at iSrc, match forwards as far as we can.  j counts
        ** the number of characters that match */
        iSrc = iBlock*NHASH;
        for(j=0, x=iSrc, y=base+i; (unsigned)x<lenSrc && (unsigned)y<lenOut; j++, x++, y++){
          if( zSrc[x]!=zOut[y] ) break;
        }
        j--;

        /* Beginning at iSrc-1, match backwards as far as we can.  k counts
        ** the number of characters that match */
        for(k=1; k<iSrc && (unsigned)k<=i; k++){
          if( zSrc[iSrc-k]!=zOut[base+i-k] ) break;
        }
        k--;

        /* Compute the offset and size of the matching region */
        ofst = iSrc-k;
        cnt = j+k+1;
        litsz = i-k;  /* Number of bytes of literal text before the copy */
        DEBUG2( printf("MATCH %d bytes at %d: [%s] litsz=%d\n",
                        cnt, ofst, print16(&zSrc[ofst]), litsz); )
        /* sz will hold the number of bytes needed to encode the "insert"
        ** command and the copy command, not counting the "insert" text */
        sz = digit_count_b64(i-k)+digit_count_b64(cnt)+digit_count_b64(ofst)+3;
        if( cnt>=sz && cnt>bestCnt ){
          /* Remember this match only if it is the best so far and it
          ** does not increase the file size */
          bestCnt = cnt;
          bestOfst = iSrc-k;
          bestLitsz = litsz;
          DEBUG2( printf("... BEST SO FAR\n"); )
        }

        /* Check the next matching block */
        iBlock = collide[iBlock];
      }

      /* We have a copy command that does not cause the delta to be larger
      ** than a literal insert.  So add the copy command to the delta.
      */
      if( bestCnt>0 ){
        if( bestLitsz>0 ){
          /* Add an insert command before the copy */
          PINT(bestLitsz);
          OUTCH(":");
          OUT(zOut+base, bestLitsz);
          base += bestLitsz;
          DEBUG2( printf("insert %d\n", bestLitsz); )
        }
        base += bestCnt;
        PINT(bestCnt);
        OUTCH("@");
        PINT(bestOfst);
        DEBUG2( printf("copy %d bytes from %d\n", bestCnt, bestOfst); )
        OUTCH(",");
        if( bestOfst + bestCnt -1 > lastRead ){
          lastRead = bestOfst + bestCnt - 1;
          DEBUG2( printf("lastRead becomes %d\n", lastRead); )
        }
        bestCnt = 0;
        break;
      }

      /* If we reach this point, it means no match is found so far */
      if( base+i+NHASH>=lenOut ){
        /* We have reached the end of the input and have not found any
        ** matches.  Do an "insert" for everything that does not match */
        PINT(lenOut-base);
        OUTCH(":");
        OUT(zOut+base, lenOut-base);
        base = lenOut;
        break;
      }

      /* Advance the hash by one character.  Keep looking for a match */
      FeltaHash_next(&h, zOut[base+i+NHASH]);
      i++;
    }
  }
  free(collide);
  /* Output a final "insert" record to get all the text at the end of
  ** the file that does not match anything in the source file.
  */
  if( base<lenOut ){
    PINT(lenOut-base);
    OUTCH(":");
    OUT(zOut+base, lenOut-base);
  }
  /* Output the final checksum record. */
  PINT(felta_checksum(zOut, lenOut));
  OUTCH(";");
  return (int)total;
#undef PINT
#undef OUT
#undef OUTCH
}

/** Internal state for outputing a delta to a string.
 */
struct OutputString {
  unsigned char * mem;
  unsigned int cursor;
};
typedef struct OutputString OutputString;

/** felta_output_f() impl which requires state to be a (OutputString*). */
static int felta_output_f_string( void * state, void const * src, unsigned int n ){
  OutputString * os = (OutputString*)state;
  memcpy( os->mem + os->cursor, src, n );
  os->cursor += n;
  return 0;
}
int felta_output_f_FILE( void * state, void const * data, unsigned int n ){
  FILE * f = state ? (FILE*)state : stdout;
  return (1==fwrite(data, n, 1, f))
    ? 0
    : -1;
}

int felta_create(unsigned const char *zSrc,
                 unsigned int lenSrc,
                 unsigned const char *zOut,
                 unsigned int lenOut,
                 unsigned char *zDelta){
  int rc;
  OutputString os;
  os.mem = (unsigned char *)zDelta;
  os.cursor = 0;
  rc = felta_create2( zSrc, lenSrc, zOut, lenOut,
                      felta_output_f_string, &os );
  if(rc>=0){
    os.mem[os.cursor] = 0;
  }
  return rc;
}
int felta_apply( unsigned const char *zSrc, unsigned int lenSrc,
                 unsigned const char *zDelta, unsigned int lenDelta,
                 unsigned char *zOut,
                 const char ** zErr){
  int rc;
  OutputString os;
  os.mem = (unsigned char *)zOut;
  os.cursor = 0;
  rc = felta_apply2( zSrc, lenSrc, zDelta, lenDelta,
                     felta_output_f_string, &os, zErr );
  if(rc>=0){
    os.mem[os.cursor] = 0;
#if !FELTA_OMIT_DELTA_CKSUM_TEST
    rc = felta_verify_checksum( zOut, (unsigned)rc,
                                zDelta, lenDelta, zErr );
#endif
  }
  return rc;
}

int felta_output_size(unsigned const char *zDelta, unsigned int lenDelta){
  if(!zDelta || !*zDelta || (lenDelta<2)) return FELTA_RC_RANGE;
  else {
    unsigned int size;
#if !FELTA_EXTENDED_ENCODING
    static char const base = 64;
#else
    char const base = 
        byteToBase(*zDelta);
    if(base <= 0){
        return FELTA_RC_INVALID_SIZE;
    }
    ++zDelta; --lenDelta;
#endif
    size = getInt(base, &zDelta, &lenDelta);
    if( *zDelta!='\n' ){
      /* ERROR: size integer not terminated by "\n" */
      return FELTA_RC_RANGE;
    }
    assert(size>0);
    return (int)size;
  }
}

#if 0
#  define BREAKPOINT {int bp=1;assert(bp); }(void)0
#else
#  define BREAKPOINT (void)0
#endif

int felta_apply2(
  unsigned const char *zSrc,      /* The source or pattern file */
  unsigned int lenSrc,            /* Length of the source file */
  unsigned const char *zDelta,    /* Delta to apply to the pattern */
  unsigned int lenDelta,          /* Length of the delta */
  felta_output_f out,
  void * outState,
  const char ** zErr
){
  unsigned int limit;
  unsigned int total = 0;
  int rc;
#define ERR(STR) if(zErr) *zErr = STR
#if !FELTA_EXTENDED_ENCODING
  static const char numBase = 64;
#else
  /* Read numeric encoding byte. */
  char const numBase = byteToBase(*zDelta);
  if(numBase <= 0){
      return FELTA_RC_INVALID_SIZE;
  }
  ++zDelta; --lenDelta;
#endif

#define OUT(BLOB,LEN) rc=out(outState, BLOB, LEN); if(0 != rc) return FELTA_RC_IO
#define OUTCH(CHAR) OUT(CHAR,1)

  /* Read final expected output size. */
  limit = getInt(numBase, &zDelta, &lenDelta);
  if( *zDelta!='\n' ){
    ERR("size integer not terminated by '\n'");
    return FELTA_RC_INVALID_SEPARATOR;
  }
  zDelta++; lenDelta--;
  while( *zDelta && lenDelta>0 ){
    unsigned int cnt, ofst = 0;
    cnt = getInt(numBase, &zDelta, &lenDelta);
    switch( zDelta[0] ){
      case '@': {
        zDelta++; lenDelta--;
        ofst = getInt(numBase, &zDelta, &lenDelta);
        if( lenDelta>0 && zDelta[0]!=',' ){
          ERR("copy command not terminated by ','");
          BREAKPOINT;
          return FELTA_RC_INVALID_SEPARATOR;
        }
        zDelta++; lenDelta--;
        DEBUG1( printf("COPY %d from %d\n", cnt, ofst); )
        total += cnt;
        if( total>limit ){
          ERR("copy exceeds output file size");
          BREAKPOINT;
          return FELTA_RC_RANGE;
        }
        if( ofst+cnt > lenSrc ){
          ERR("copy extends past end of input");
          BREAKPOINT;
          return FELTA_RC_RANGE;
        }
        OUT(&zSrc[ofst], cnt);
        /*memcpy(zOut, &zSrc[ofst], cnt);
          zOut += cnt;*/
        break;
      }
      case ':': {
        zDelta++; lenDelta--;
        total += cnt;
        if( total>limit ){
          ERR("insert command gives an output larger than predicted");
          BREAKPOINT;
          return FELTA_RC_INVALID_SIZE;
        }
        DEBUG1( printf("INSERT %d\n", cnt); )
        if( cnt>lenDelta ){
          ERR("insert count exceeds size of delta");
          return FELTA_RC_RANGE;
        }
        OUT(zDelta,cnt);
        /*memcpy(zOut, zDelta, cnt);
          zOut += cnt;*/
        zDelta += cnt;
        lenDelta -= cnt;
        break;
      }
      case ';': {
        zDelta++; lenDelta--;
        /*OUT("\0",1);*/
        /*zOut[0] = 0;*/
        if( total!=limit ){
          ERR("generated size does not match predicted size");
          BREAKPOINT;
          return FELTA_RC_INVALID_SIZE;
        }
#if 0 && !FELTA_OMIT_DELTA_CKSUM_TEST
        /* This can no longer be done from this routine because it
           does not have access to the output data.
        */
        if( cnt!=felta_checksum(zOrigOut, total) ){
          ERR("bad checksum");
          BREAKPOINT;
          return FELTA_RC_CHECKSUM_MISMATCH;
        }
#endif
        return (int)total;
      }
      default: {
        ERR("unknown delta operator");
        BREAKPOINT;
        return FELTA_RC_INVALID_OPERATOR;
      }
    }
  }
  ERR("unterminated delta");
  BREAKPOINT;
  return FELTA_RC_UNTERMINATED;
#undef ERR
#undef OUT
#undef OUTCH
}

#undef BREAKPOINT


int felta_verify_checksum( unsigned char const * zOrigOut, unsigned int lenSrc,
                           unsigned char const * zDelta, unsigned int lenDelta,
                           const char ** zErr){
  /* FIXME: this basically duplicates the whole felta_apply() logic,
     just without the output.
  */
  unsigned int limit;
  unsigned int total = 0;
#if !FELTA_EXTENDED_ENCODING
  static char const numBase = FELTA_INTEGER_BASE;
#else
  /* Read numeric encoding byte. */
  char const numBase = byteToBase(*zDelta);
  if(numBase <= 0){
      return FELTA_RC_INVALID_SIZE;
  }
  ++zDelta; --lenDelta;
#endif
#if 0
  printf("verify lenSrc=%d, lenDelta=%d\n", (int)lenSrc, (int)lenDelta);
#endif
#define ERR(STR) if(zErr) *zErr = STR
  /* Read final expected output size. */
  limit = getInt(numBase, &zDelta, &lenDelta);
  if( *zDelta!='\n' ){
    ERR("size integer not terminated by '\n'");
    return FELTA_RC_INVALID_SEPARATOR;
  }
  if(lenSrc!=limit){
    ERR("size mismatch.");
    return FELTA_RC_INVALID_SIZE;
  }
  zDelta++; lenDelta--;
  while( *zDelta && lenDelta>0 ){
    unsigned int cnt, ofst = 0;
    cnt = getInt(numBase, &zDelta, &lenDelta);
    switch( zDelta[0] ){
      case '@': {
        zDelta++; lenDelta--;
        ofst = getInt(numBase, &zDelta, &lenDelta);
        if( lenDelta>0 && zDelta[0]!=',' ){
          ERR("copy command not terminated by ','");
          return FELTA_RC_INVALID_SEPARATOR;
        }
        zDelta++; lenDelta--;
        total += cnt;
        if( total>limit ){
          ERR("copy exceeds output file size");
          return FELTA_RC_RANGE;
        }
        if( ofst+cnt > lenSrc ){
          ERR("copy extends past end of input");
          return FELTA_RC_RANGE;
        }
        break;
      }
      case ':': {
        zDelta++; lenDelta--;
        total += cnt;
        if( total>limit ){
          ERR("insert command gives an output larger than predicted");
          return FELTA_RC_INVALID_SIZE;
        }
        if( cnt>lenDelta ){
          ERR("insert count exceeds size of delta");
          return FELTA_RC_RANGE;
        }
        zDelta += cnt;
        lenDelta -= cnt;
        break;
      }
      case ';': {
        zDelta++; lenDelta--;
        if( total!=limit ){
          ERR("generated size does not match predicted size");
          return FELTA_RC_INVALID_SIZE;
        }
        if( cnt!=felta_checksum(zOrigOut, total) ){
          ERR("bad checksum");
          return FELTA_RC_CHECKSUM_MISMATCH;
        }
        return (int)total;
      }
      default: {
        ERR("unknown delta operator");
        return FELTA_RC_INVALID_OPERATOR;
      }
    }
  }
  ERR("unterminated delta");
  return FELTA_RC_UNTERMINATED;
#undef ERR
}


char const * felta_rc_cstr(int rc){
  switch((enum felta_rc)rc){
      /* we ^^^^ cast so that gcc will warn if the switch() below is
         missing any fsl_rc_t entries. */

#define STR(T) case FELTA_RC_##T: return "FELTA_RC_" #T
    STR(OK);
    STR(OOM);
    STR(RANGE);
    STR(INVALID_SEPARATOR);
    STR(INVALID_SIZE);
    STR(CHECKSUM_MISMATCH);
    STR(INVALID_OPERATOR);
    STR(UNTERMINATED);
    STR(IO);
#undef STR
  }
  return "Unknown result code";
}

#undef NHASH
#undef FELTA_OMIT_DELTA_CKSUM_TEST
#undef FELTA_INTEGER_BASE
#undef DEBUG2
#undef DEBUG1
#ifdef __cplusplus
} /*extern "C"*/
#endif
