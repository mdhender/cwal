/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* vim: set ts=2 et sw=2 tw=80: */

#include <stdio.h>
#include <ctype.h> /* isspace() and friends */
#include <stdlib.h> /* EXIT_ERROR and EXIT_SUCCESS */
#include <string.h>
#include <stdint.h> /* C99 */

extern uint32_t whcl__hash_keyword( char const * input, uint16_t len );

/**
   This tool reads word-type tokens from stdin or as all arguments
   passed to it and performs a hash on it. This tool's primary purpose
   is testing the "perfect hash" algorithm for whcl's keywords and
   keyword-like constructs. It is *NOT* a general-purpose hashing
   routine: it is tuned for that one use case.
*/
int main(int argc, char const * const * argv){
  enum { TOKEN_BUF_SIZE = 100 };
  char buf[TOKEN_BUF_SIZE] = {0};
  int ch;
  char * pos = buf;
  int i, n = 0, j, rc = 0;
  uint32_t gotHashes[argc];
  char const * gotArgs[argc];
  uint32_t h;
#define DUPCHECK                                   \
  for( j = 0; j < n; ++j ){                           \
    if(h==gotHashes[j]){                         \
      fprintf(stderr, "Got a collision: %s and %s\n", \
              gotArgs[j], arg);                       \
      rc = 1; goto end;                               \
    }                                                 \
  }(void)0
  if(argc>1){
    unsigned slen;
    for( i = 1; i < argc; ++i ){
      char const * arg = argv[i];
      slen = (unsigned)strlen(arg);
      h = whcl__hash_keyword(arg, slen);
      printf("0x%08x %.*s\n", h, (int)slen, arg);
      DUPCHECK;
      gotHashes[n] = h;
      gotArgs[n] = arg;
      ++n;
    }
  }else{
#undef DUPCHECK
#define DUPCHECK                                      \
  for( j = 0; j < n; ++j ){                           \
    if(h==gotHashes[j]){                              \
      fprintf(stderr,                                           \
              "Got a collision at entry number %d: %s\n",      \
              n+1, buf);                                        \
      rc = 1; goto end;                               \
    }                                                 \
  }(void)0
#define out h = whcl__hash_keyword(buf, (uint16_t)(pos-buf));   \
    *pos=0;                                                     \
    printf("0x%08x %.*s\n", h, (int)(pos - buf), buf); \
    DUPCHECK;                                          \
    gotHashes[n++] = h;
    if(argc || argv){/*unused*/}
    while(EOF != (ch = fgetc(stdin))){
      if(!isspace(ch)){
        *pos++ = (char)ch;
        if(pos == buf + TOKEN_BUF_SIZE - 1){
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
  end:
  return rc ? EXIT_FAILURE : EXIT_SUCCESS;
}
