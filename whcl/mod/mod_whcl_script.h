/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/*
  A "supermacro" for implementing simple C modules which themselves only
  import a compiled-in whcl script.
*/
#include "libwhcl.h"
#include <assert.h>

#if !defined(WHCL_MOD_NAME)
#error define WHCL_MOD_NAME
#endif
/**
   WHCL_MOD_NAME is the module's name as the user will see/interact
   with.  WHCL_MOD_CNAME is the C-symbol-friendly form. e.g. the name
   might be foo-bar but the cname would be foo_bar (or similar). The
   cname only needs to be set if the name is not inherently
   C-symbol-friendly.
*/
#if !defined(WHCL_MOD_CNAME)
#  define WHCL_MOD_CNAME WHCL_MOD_NAME
#endif
#define PASTE_(A,B) A##B
#define PASTE3_(A,B,C) A##B##C
#define PASTE(A,B) PASTE_(A,B)
#define PASTE3(A,B,C) PASTE3_(A,B,C)
#define STR_(A) #A
#define STR(A) STR_(A)
/*#(A,B)*/

/**
   WHCL_MOD_SCRIPT_FUNC is the name of the function which returns the
   script source code. It defaults to `whcl_src_##WHCL_MOD_CNAME`.
   The function must have this signature:

   ```
   unsigned char const * WHCL_MOD_SCRIPT_FUNC(unsigned int *);
   ```

   It must return the bytes for the script and if its argument is not
   NULL, it must assign the strlen of those bytes to it.
*/
#if !defined(WHCL_MOD_SCRIPT_FUNC)
#define WHCL_MOD_SCRIPT_FUNC PASTE(whcl_src_,WHCL_MOD_CNAME)
#endif

/**
   WHCL_MOD_SCRIPT_NAME is the name for the script. This will be the
   name used for purposes of error reporting and the like, so it
   should be at least somewhat descriptive. If the module's input
   script has a distinctive name, that's a prime candidate for a value
   for this macro.
*/
#if !defined WHCL_MOD_SCRIPT_NAME
#  define WHCL_MOD_SCRIPT_NAME STR(WHCL_MOD_NAME) " module script"
#endif


#define INIT_F PASTE(whcl_module_init_,WHCL_MOD_CNAME)
static int INIT_F( whcl_engine * const el, cwal_value ** rv ){
  extern unsigned char const * WHCL_MOD_SCRIPT_FUNC(unsigned int *);
  unsigned int len = 0;
  char const * src = (char const *)WHCL_MOD_SCRIPT_FUNC(&len);
  assert(len);
  return whcl_eval_cstr(el, true, WHCL_MOD_SCRIPT_NAME,
                        src, (int)len, rv );
}

/*
  i can't seem to get the macro expansion working how i need for
  simply forwarding WHCL_MOD_CNAME to WHCL_MODULE_REGISTER_, so we'll do
  this the long way...
*/
#if defined(WHCL_MODULE_STANDALONE)
static const whcl_loadable_module whcl_module_impl = { STR(WHCL_MOD_NAME), INIT_F };
const whcl_loadable_module * whcl_module = &whcl_module_impl;
#endif

static const whcl_loadable_module PASTE3(whcl_module_,WHCL_MOD_CNAME,_impl) =
  { STR(WHCL_MOD_NAME), INIT_F };
const whcl_loadable_module * PASTE(whcl_module_,WHCL_MOD_CNAME) =
  &PASTE3(whcl_module_,WHCL_MOD_CNAME,_impl);

#undef INIT_F
#undef STR
#undef STR_
#undef PASTE
#undef PASTE_
#undef PASTE3
#undef PASTE3_
#undef WHCL_MOD_NAME
#undef WHCL_MOD_CNAME
#undef WHCL_MOD_SCRIPT_FUNC
#undef WHCL_MOD_SCRIPT_NAME
