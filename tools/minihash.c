/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* vim: set ts=2 et sw=2 tw=80: */

/**
   This tool reads word-type tokens from stdin or as all arguments
   passed to it and performs a hash on it. This tool's primary purpose
   is testing the "perfect hash" algorithm for s2's keywords and
   keyword-like constructs. It is *NOT* a general-purpose hashing
   routine: it is tuned for that one use case.
*/
#include <stdio.h>
#include <ctype.h> /* isspace() and friends */
#include <stdint.h> /* C99 */
#include <stdlib.h> /* EXIT_ERROR and EXIT_SUCCESS */
#include <string.h>

/**
   Performs the "s2 keyword hash" on the first len bytes of input and
   returns the hash.

   It returns 0 if the input length is 0 or the input contains any
   characters with a value less than '$' or greater than 'z', which
   includes all non-ASCII UTF8.
*/
uint32_t hash( char const * input, unsigned int len ){
  unsigned int i = 0;
  uint64_t h = 0;
  char const * pos = input;
  for( ; i < len; ++i, ++pos ){
    if(*pos > 'z' || *pos < '$') return 0;
    /*All of these variations have worked out so far...
      h = (h << 1) + 100*i + i*pt->begin[i] - i*35;
      h = (h << 1) + 100*i + i*pt->begin[i] - i*35;
      h = (h << 1) + 45*i + pt->begin[i] - 35;

      Note that the magic 35==ASCII '$'-1, the lowest-numbered
      character allowed in an s2 keyword.

      ************************************************************
      WARNING: this algo MUST be kept in sync with the one in
      s2_eval-keyword-hasher.s2, as that script generates the C code
      for our keyword/typeinfo/pragma bits.
      ************************************************************
    */
    h = (h << 1) + (i+1) * (*pos - 35/*==>ASCII '$'-1*/);
    while(h > (uint64_t)0x7fffffff){
      h = h>>1
        /* With a uint64_t hash, trim hash to within 32 bits because
           we need to be able to use these values in switch/case, and
           64 bits are not portable for that. We *have* to use 64-bit
           integers for the calculation because they're also
           calculated in script-space, where we use (by and large)
           64-bit builds. */;
    }
  }
  return (uint32_t)h;
}

int main(int argc, char const * const * argv){
  enum { TOKEN_BUF_SIZE = 100 };
  char buf[TOKEN_BUF_SIZE] = {0};
  int ch;
  char * pos = buf;
  int rc = 0;
  if(argc>1){
    int i;
    unsigned slen;
    for( i = 1; i < argc; ++i ){
      slen = (unsigned)strlen(argv[i]);
      printf("0x%08x %.*s\n", hash(argv[i], slen), (int)slen, argv[i]);
    }
  }else{
#define out printf("0x%08x %.*s\n", hash(buf, (unsigned)(pos - buf)), (int)(pos - buf), buf)
    if(argc || argv){/*unused*/}
    while(EOF != (ch = fgetc(stdin))){
      if(!isspace(ch)){
        *pos++ = (char)ch;
        if(pos == buf + TOKEN_BUF_SIZE){
          fprintf(stderr, "Token is too large. Exiting to avoid a "
                  "buffer overflow.\n");
          rc = 1;
          break;
        }
      }else{
        if(pos>buf){
          out;
        }
        pos = buf;
      }
    }
    if(!rc && pos>buf){
      out;
    }
#undef out
  }
  return rc ? EXIT_FAILURE : EXIT_SUCCESS;
}
