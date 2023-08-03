/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* vim: set ts=2 et sw=2 tw=80: */

#include <stdint.h> /* C99 */
/**
   Calculates a hash value for the first len bytes of the given string
   suitable for use as a switch/case value. This algorithm MUST
   produce unique values for all given inputs and it has been
   hand-tweaked to generate collision-free values for its (small)
   intended use case.

   This function is in its own file to facilitate building a tiny
   standalone binary which does not depend on the rest of whcl.
*/
uint32_t whcl__hash_keyword( char const * input, uint16_t len ){
  uint16_t i = 0;
  uint64_t h = 0;
  char const * pos = input;
  h = *pos * 1117;
  for( ; i < len; ++i, ++pos ){
    /*All of these variations have worked out so far...
      h = (h << 1) + 100*i + i*pt->begin[i] - i*35;
      h = (h << 1) + 100*i + i*pt->begin[i] - i*35;
      h = (h << 1) + 45*i + pt->begin[i] - 35;

      ************************************************************
      WARNING: this algo MUST be kept in sync with the one in
      the external tool which generates the C code
      for our keyword/typeinfo/pragma bits.
      ************************************************************
    */
    if(*pos > 'z' || *pos < '$') return 0;
    h = (h << 1) + (i*11) * (*pos - 64);
    while(h > (uint64_t)0x7fffffff)
      h = h>>1
        /* With a uint64_t hash, trim hash to within 32 bits because
           we need to be able to use these values in switch/case, and
           64 bits are not portable for that. We *have* to use 64-bit
           integers for the calculation because they're also
           calculated in script-space, where we use (by and large)
           64-bit builds.
        */;
  }
  return (uint32_t)h;
}
