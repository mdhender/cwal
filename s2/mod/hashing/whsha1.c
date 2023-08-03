/*#include <assert.h>*/
#include "whsha1.h"
#include <string.h> /* memcpy() */
/*
** The SHA1 implementation below is adapted from:
**
**  $NetBSD: sha1.c,v 1.6 2009/11/06 20:31:18 joerg Exp $
**  $OpenBSD: sha1.c,v 1.9 1997/07/23 21:12:32 kstailey Exp $
**
** SHA-1 in C
** By Steve Reid <steve@edmweb.com>
** 100% Public Domain
**
** THIS copy was adapted from the Fossil SCM
** (http://www.fossil-scm.org) for use in whsha1.
*/


const whsha1_cx whsha1_cx_empty = whsha1_cx_empty_m;


/*
 * blk0() and blk() perform the initial expand.
 * I got the idea of expanding during the round function from SSLeay
 *
 * blk0le() for little-endian and blk0be() for big-endian.
 */
#if 0 && defined(__GNUC__) && (defined(__i386__) || defined(__x86_64__))
/*
 * GCC by itself only generates left rotates.  Use right rotates if
 * possible to be kinder to dinky implementations with iterative rotate
 * instructions.
 */
#define SHA_ROT(op, x, k) \
        ({ unsigned int y; asm(op " %1,%0" : "=r" (y) : "I" (k), "0" (x)); y; })
#define rol(x,k) SHA_ROT("roll", x, k)
#define ror(x,k) SHA_ROT("rorl", x, k)

#else
/* Generic C equivalent */
#define SHA_ROT(x,l,r) ((x) << (l) | (x) >> (r))
#define rol(x,k) SHA_ROT(x,k,32-(k))
#define ror(x,k) SHA_ROT(x,32-(k),k)
#endif


#define blk0le(i) (block[i] = (ror(block[i],8)&0xFF00FF00) \
    |(rol(block[i],8)&0x00FF00FF))
#define blk0be(i) block[i]
#define blk(i) (block[i&15] = rol(block[(i+13)&15]^block[(i+8)&15] \
    ^block[(i+2)&15]^block[i&15],1))

/*
 * (R0+R1), R2, R3, R4 are the different operations (rounds) used in SHA1
 *
 * Rl0() for little-endian and Rb0() for big-endian.  Endianness is 
 * determined at run-time.
 */
#define Rl0(v,w,x,y,z,i) \
    z+=((w&(x^y))^y)+blk0le(i)+0x5A827999+rol(v,5);w=ror(w,2);
#define Rb0(v,w,x,y,z,i) \
    z+=((w&(x^y))^y)+blk0be(i)+0x5A827999+rol(v,5);w=ror(w,2);
#define R1(v,w,x,y,z,i) \
    z+=((w&(x^y))^y)+blk(i)+0x5A827999+rol(v,5);w=ror(w,2);
#define R2(v,w,x,y,z,i) \
    z+=(w^x^y)+blk(i)+0x6ED9EBA1+rol(v,5);w=ror(w,2);
#define R3(v,w,x,y,z,i) \
    z+=(((w|x)&y)|(w&x))+blk(i)+0x8F1BBCDC+rol(v,5);w=ror(w,2);
#define R4(v,w,x,y,z,i) \
    z+=(w^x^y)+blk(i)+0xCA62C1D6+rol(v,5);w=ror(w,2);

/*
 * Hash a single 512-bit block. This is the core of the algorithm.
 */
#define a qq[0]
#define b qq[1]
#define c qq[2]
#define d qq[3]
#define e qq[4]

static void SHA1Transform(unsigned int state[5], const unsigned char buffer[64])
{
  unsigned int qq[5]; /* a, b, c, d, e; */
  static int one = 1;
  unsigned int block[16];
  memcpy(block, buffer, 64);
  memcpy(qq,state,5*sizeof(unsigned int));

  /* Copy context->state[] to working vars */
  /*
  a = state[0];
  b = state[1];
  c = state[2];
  d = state[3];
  e = state[4];
  */

  /* 4 rounds of 20 operations each. Loop unrolled. */
  if( 1 == *(unsigned char*)&one ){
    Rl0(a,b,c,d,e, 0); Rl0(e,a,b,c,d, 1); Rl0(d,e,a,b,c, 2); Rl0(c,d,e,a,b, 3);
    Rl0(b,c,d,e,a, 4); Rl0(a,b,c,d,e, 5); Rl0(e,a,b,c,d, 6); Rl0(d,e,a,b,c, 7);
    Rl0(c,d,e,a,b, 8); Rl0(b,c,d,e,a, 9); Rl0(a,b,c,d,e,10); Rl0(e,a,b,c,d,11);
    Rl0(d,e,a,b,c,12); Rl0(c,d,e,a,b,13); Rl0(b,c,d,e,a,14); Rl0(a,b,c,d,e,15);
  }else{
    Rb0(a,b,c,d,e, 0); Rb0(e,a,b,c,d, 1); Rb0(d,e,a,b,c, 2); Rb0(c,d,e,a,b, 3);
    Rb0(b,c,d,e,a, 4); Rb0(a,b,c,d,e, 5); Rb0(e,a,b,c,d, 6); Rb0(d,e,a,b,c, 7);
    Rb0(c,d,e,a,b, 8); Rb0(b,c,d,e,a, 9); Rb0(a,b,c,d,e,10); Rb0(e,a,b,c,d,11);
    Rb0(d,e,a,b,c,12); Rb0(c,d,e,a,b,13); Rb0(b,c,d,e,a,14); Rb0(a,b,c,d,e,15);
  }
  R1(e,a,b,c,d,16); R1(d,e,a,b,c,17); R1(c,d,e,a,b,18); R1(b,c,d,e,a,19);
  R2(a,b,c,d,e,20); R2(e,a,b,c,d,21); R2(d,e,a,b,c,22); R2(c,d,e,a,b,23);
  R2(b,c,d,e,a,24); R2(a,b,c,d,e,25); R2(e,a,b,c,d,26); R2(d,e,a,b,c,27);
  R2(c,d,e,a,b,28); R2(b,c,d,e,a,29); R2(a,b,c,d,e,30); R2(e,a,b,c,d,31);
  R2(d,e,a,b,c,32); R2(c,d,e,a,b,33); R2(b,c,d,e,a,34); R2(a,b,c,d,e,35);
  R2(e,a,b,c,d,36); R2(d,e,a,b,c,37); R2(c,d,e,a,b,38); R2(b,c,d,e,a,39);
  R3(a,b,c,d,e,40); R3(e,a,b,c,d,41); R3(d,e,a,b,c,42); R3(c,d,e,a,b,43);
  R3(b,c,d,e,a,44); R3(a,b,c,d,e,45); R3(e,a,b,c,d,46); R3(d,e,a,b,c,47);
  R3(c,d,e,a,b,48); R3(b,c,d,e,a,49); R3(a,b,c,d,e,50); R3(e,a,b,c,d,51);
  R3(d,e,a,b,c,52); R3(c,d,e,a,b,53); R3(b,c,d,e,a,54); R3(a,b,c,d,e,55);
  R3(e,a,b,c,d,56); R3(d,e,a,b,c,57); R3(c,d,e,a,b,58); R3(b,c,d,e,a,59);
  R4(a,b,c,d,e,60); R4(e,a,b,c,d,61); R4(d,e,a,b,c,62); R4(c,d,e,a,b,63);
  R4(b,c,d,e,a,64); R4(a,b,c,d,e,65); R4(e,a,b,c,d,66); R4(d,e,a,b,c,67);
  R4(c,d,e,a,b,68); R4(b,c,d,e,a,69); R4(a,b,c,d,e,70); R4(e,a,b,c,d,71);
  R4(d,e,a,b,c,72); R4(c,d,e,a,b,73); R4(b,c,d,e,a,74); R4(a,b,c,d,e,75);
  R4(e,a,b,c,d,76); R4(d,e,a,b,c,77); R4(c,d,e,a,b,78); R4(b,c,d,e,a,79);

  /* Add the working vars back into context.state[] */
  state[0] += a;
  state[1] += b;
  state[2] += c;
  state[3] += d;
  state[4] += e;
}


void whsha1_init(whsha1_cx *cx){
    memset(cx,0, sizeof(whsha1_cx));
    memcpy(&cx->state[0], &whsha1_cx_empty.state[0], sizeof(whsha1_cx_empty.state));
}


void whsha1_update( whsha1_cx *cx, void const *data_, unsigned int len ){
    unsigned int i, j;
    unsigned char const * data = (unsigned char const *)data_;
    if(!cx || !data || !len) return;
    j = cx->count[0];
    if ((cx->count[0] += len << 3) < j)
	cx->count[1] += (len>>29)+1;
    j = (j >> 3) & 63;
    if ((j + len) > 63) {
	(void)memcpy(&cx->buffer[j], data, (i = 64-j));
	SHA1Transform(cx->state, cx->buffer);
	for ( ; i + 63 < len; i += 64)
	    SHA1Transform(cx->state, &data[i]);
	j = 0;
    } else {
	i = 0;
    }
    (void)memcpy(&cx->buffer[j], &data[i], len - i);
}


/**
  Convert a digest into base-16. digest should be declared as
  "unsigned char digest[20]" in the calling function. zBuf must be at
  least 41 bytes long and the hex form of the given digest will be
  written there (40 bytes plus a trailing NUL).
*/
static void sha1_to_hex(unsigned char *digest, char *zBuf){
  static char const zEncode[] = "0123456789abcdef";
  int ix;
  for(ix=0; ix<20; ++ix){
    *zBuf++ = zEncode[(*digest>>4)&0xf];
    *zBuf++ = zEncode[*digest++ & 0xf];
  }
  *zBuf = 0;
}

void whsha1_end(whsha1_cx *cx){
    unsigned int i;
    unsigned char finalcount[8];
    for (i = 0; i < 8; i++) {
	finalcount[i] = (unsigned char)((cx->count[(i >= 4 ? 0 : 1)]
	 >> ((3-(i & 3)) * 8) ) & 255);	 /* Endian independent */
    }
    whsha1_update(cx, (const unsigned char *)"\200", 1);
    while ((cx->count[0] & 504) != 448)
	whsha1_update(cx, (const unsigned char *)"\0", 1);
    whsha1_update(cx, finalcount, 8);  /* Should cause a SHA1Transform() */
    for (i = 0; i < 20; i++){
        cx->digest[i] = (unsigned char)
            ((cx->state[i>>2] >> ((3-(i & 3)) * 8) ) & 255);
    }
    sha1_to_hex( cx->digest, cx->hex );
}

/************************************************************************
  Anything below here is whsha1-specific and can be stripped out to get
  at the good parts (the sha1 impl).
*************************************************************************/


void whsha1_hash( whsha1_cx *cx, void const * mem, unsigned int n )
{
    whsha1_init(cx);
    whsha1_update(cx, mem, n);
    whsha1_end(cx);
}

void whsha1_hash_hex( char * dest, void const * mem, unsigned int n ){
    whsha1_cx cx;
    whsha1_init(&cx);
    whsha1_update(&cx, mem, n);
    whsha1_end(&cx);
    memcpy(dest, cx.hex, 40);
    dest[41] = 0;
}


void whsha1_hash_words_hex_v( char * dest,
                              unsigned int partCount,
                              va_list vargs )
{
    whsha1_cx ctx;
    unsigned int i = 0;
    if(!dest || !partCount) return;
    whsha1_init(&ctx);
    for( i = 0; i < partCount; ++i ){
        char const * part = va_arg(vargs,char const *);
        if(!part) continue;
        whsha1_update(&ctx, (unsigned char const *)part, strlen(part));
    }
    whsha1_end(&ctx);
    memcpy(dest, ctx.hex, 41);
}

void whsha1_hash_words_hex( char * dest,
                            unsigned int partCount,
                            ... )
{
    va_list vargs;
    va_start(vargs, partCount);
    whsha1_hash_words_hex_v(dest, partCount, vargs);
    va_end(vargs);
}

#undef SHA_ROT
#undef rol
#undef ror
#undef blk0le
#undef blk0be
#undef blk

#undef Rl0
#undef Rb0
#undef R1
#undef R2
#undef R3
#undef R4
#undef a
#undef b
#undef c
#undef d
#undef e
