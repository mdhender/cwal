/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/*
  A "supermacro" for implementing simple C modules which themselves only
  import a compiled-in s2 script.
*/
#include "libs2.h"
#include <assert.h>

#if !defined(S2_MOD_NAME)
#error define S2_MOD_NAME
#endif
#define PASTE_(A,B) A##B
#define PASTE3_(A,B,C) A##B##C
#define PASTE(A,B) PASTE_(A,B)
#define PASTE3(A,B,C) PASTE3_(A,B,C)
#define STR_(A) #A
#define STR(A) STR_(A)
/*#(A,B)*/

#if !defined(S2_MOD_SCRIPT_FUNC)
#define S2_MOD_SCRIPT_FUNC PASTE(s2_script_,S2_MOD_NAME)
#endif

#if !defined S2_MOD_SCRIPT_NAME
#  define S2_MOD_SCRIPT_NAME STR(S2_MOD_NAME) " module script"
#endif

#define INIT_F PASTE(s2_module_init_,S2_MOD_NAME)
static int INIT_F( s2_engine * se, cwal_value ** rv ){
  extern unsigned char const * S2_MOD_SCRIPT_FUNC(unsigned int *);
  unsigned int len = 0;
  char const * src = (char const *)S2_MOD_SCRIPT_FUNC(&len);
  assert(len);
  return s2_eval_cstr(se, 1, S2_MOD_SCRIPT_NAME,
                      src, (int)len, rv );
}

/*
  i can't seem to get the macro expansion working how i need for
  simply forwarding S2_MOD_NAME to S2_MODULE_REGISTER_, so we'll do
  this the long way...
*/
#if defined(S2_MODULE_STANDALONE)
static const s2_loadable_module s2_module_impl = { STR(S2_MOD_NAME), INIT_F };
const s2_loadable_module * s2_module = &s2_module_impl;
#endif

static const s2_loadable_module PASTE3(s2_module_,S2_MOD_NAME,_impl) =
  { STR(S2_MOD_NAME), INIT_F };
const s2_loadable_module * PASTE(s2_module_,S2_MOD_NAME) =
  &PASTE3(s2_module_,S2_MOD_NAME,_impl);

#undef INIT_F
#undef STR
#undef STR_
#undef PASTE
#undef PASTE_
#undef PASTE3
#undef PASTE3_
#undef S2_MOD_NAME
#undef S2_MOD_SCRIPT_FUNC
#undef S2_MOD_SCRIPT_NAME
