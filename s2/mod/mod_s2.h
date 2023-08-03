#if !defined(s2_engine_empty_m)
#  include "libs2.h"
#endif

/************************************************************************
Don't use this - having a discrepancy between registered module name
and its C-symbol name will cause registration headaches for the build
process when it wants to automatically inject and register modules :/.

Nonetheless, let's keep this around for experimentation purposes...

A "supermacro" to register a module. It is triggers if the following
macros are defined before including this file:

- S2_MOD_REG_SYM = symbolic name of the module. Must be a legal C
  identier.

- S2_MOD_REG_INIT_F = the _name_ of a static function in the module
  impl file which implements the module init interface. This macro
  declares the function. (There's a chicken/egg timing problem if the
  module is left to declare it.)

Optionally:

- S2_MOD_REG_NAME = the string name of the module. By default it is
  the string form of S2_MOD_REG_SYM, but it may differ. e.g.  when a
  module's name has non-identifier characters.

All of the S2_MOD_REG_xxx macros are undefined at the end of this
file.

This installs a module registration for the module. If
S2_MODULE_STANDALONE is defined, a "standalone" module registration is
also installed (intended only for use when the module is the only
module in its resulting DLL/app).
************************************************************************/
#if defined(S2_MOD_REG_SYM) && defined(S2_MOD_REG_INIT_F)
#define CAT_(A1,A2) A1##A2
#define CAT3_(A1,A2,A3) A1##A2##A3
#define CAT(A1,A2) CAT_(A1,A2)
#define CAT3(A1,A2,A3) CAT3_(A1,A2,A3)
#define STR_(A) #A
#define STR(A) STR_(A)
static int S2_MOD_REG_INIT_F( s2_engine * se, cwal_value ** rv );
#  if !defined(S2_MOD_REG_NAME)
#    define S2_MOD_REG_NAME STR(S2_MOD_REG_SYM)
#  endif
#  if defined S2_MODULE_STANDALONE
static const s2_loadable_module
    s2_module_impl = { S2_MOD_REG_NAME, S2_MOD_REG_INIT_F };
const s2_loadable_module * s2_module = &s2_module_impl;
#  endif
static const s2_loadable_module
CAT3(s2_module_,S2_MOD_REG_SYM,_impl) = { S2_MOD_REG_NAME, S2_MOD_REG_INIT_F };
const s2_loadable_module * CAT(s2_module_,S2_MOD_REG_SYM) = &CAT3(s2_module_,S2_MOD_REG_SYM,_impl);
#undef CAT
#undef CAT_
#undef CAT3_
#undef CAT_
#undef STR_
#undef STR_
#undef S2_MOD_REG_SYM
#undef S2_MOD_REG_NAME
#undef S2_MOD_REG_INIT_F
#endif
