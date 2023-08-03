/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/**
   An experiment in multiple-entry-point DLLs and s2.loadModules()'s
   ability to cope with them.
*/
#ifdef S2_AMALGAMATION_BUILD
#  include "s2_amalgamation.h"
#else
#  include "s2.h"
#endif

#include <string.h> /* strlen() */
#include <assert.h>

static int my_install_io( s2_engine * se, cwal_value * ns ){
  return s2_install_io( se, ns, 0 );
}

static int my_install_json( s2_engine * se, cwal_value * ns ){
  return s2_install_json( se, ns, 0 );
}

static int my_install_time( s2_engine * se, cwal_value * ns ){
  return s2_install_time( se, ns, 0 );
}

static int my_install_ob( s2_engine * se, cwal_value * ns ){
  return s2_install_ob( se, ns );
}

S2_MODULE_IMPL(io,my_install_io);
S2_MODULE_IMPL(json,my_install_json);
S2_MODULE_IMPL(time,my_install_time);
S2_MODULE_IMPL(ob,my_install_ob);
