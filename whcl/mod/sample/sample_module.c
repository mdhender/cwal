/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/**
   Example whcl loadable module.
*/
#include "libwhcl.h"

#include <assert.h>

/**
   Only for debuggering...
*/
#include <stdio.h>
#define MARKER(pfexp)                                               \
  do{ printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__);   \
    printf pfexp;                                                   \
  } while(0)


/**
   Custom client-side native type to bind to whcl.
*/
struct my_native {
  /* Put your data here. For now we just use a placeholder.

     Note that if any of this state is a cwal_value type, it MUST be
     given proper lifetime for the scripting engine.  The easiest way
     to do that is to add is as a property of the cwal_native object
     using cwal_prop_set() (or cwal_prop_set_with_flags(), giving it
     the flag CWAL_VAR_F_HIDDEN). The next-easiest way is to install a
     cwal_value_rescoper_f() via cwal_native_set_rescoper(). In that
     rescoper callback, rescope all cwal_value-type members of this
     struct to the new scope, as documented for that function.
  */
  int myField;
};
typedef struct my_native my_native;

/**
   Serves as a copy-constructable default instance and as a type ID
   pointer for cwal_new_native() and friends.
*/
static const my_native my_native_empty = {42};

/**
   This rescoper does nothing in this example because there's nothing
   which needs to be rescoped along with my_native, but is here for
   example's sake.
*/
static int cwal_value_rescoper_f_my( cwal_scope * s, cwal_value * v ){
  if(s || v){/*unused params*/}
  return 0;
}


/**
   A cwal_finalizer_f() implementation which we associate with our
   my_native binding. It will be called by the cwal-side garbage
   collection/finalization process when a cwal-bound my_native is
   cleaned up.
*/
static void my_native_finalize( cwal_engine * e, void * m ){
  //my_native * my = (my_native *)m;
  if(0){
    cwal_outputf(e, "%s:%s(): Finalizing my_native@%p\n",
                 __FILE__, __func__, (void *)m);
  }
  cwal_free2( e, m, sizeof(my_native)
              /* If we know the exact size of the memory we allocated
                 for a binding, passing that same size back to
                 cwal_free2() helps cwal recycle that memory
                 better. If we do not, at this point, know the exact
                 size we requested for the initial allocation, we
                 would call cwal_free() instead, in which case the
                 memory might (depending on recycling/memory-related
                 options) be recycled or returned to the system.
              */);
}


/**
   A helper function which looks for a my_native instance
   bound to v or any prototype of v.
*/
static my_native * my_native_from_value( cwal_engine * const e,
                                         cwal_value * v ){
  my_native * rc = NULL;
  do{
    cwal_native * const n = cwal_value_get_native(v);
    if(n && (rc = (my_native*)cwal_native_get(n, &my_native_empty))) break;
    else v = cwal_value_prototype_get(e, v);
  }while(v);
  return rc;
}

/* Helper macro for cwal_callback_f() implementations bound to
   my_native instances which ensures that the arguments->self value is-a
   my_native instance, noting that it might derive from my_native... */
#define THIS_MY                                                         \
  my_native * self;                                                     \
  self = my_native_from_value(args->engine, args->self);                \
  if(!self) return cwal_cb_throw(args, CWAL_RC_TYPE,      \
                                 "'this' is not (or is no longer) " \
                                 "a MyModule instance.")


/**
   Example cwal_callback_f() implementation which is intended to be
   bound as a method of my_native objects.
*/
static int my_cb_foo( cwal_callback_args const * args,
                      cwal_value **rv ){
  cwal_string * str;
  THIS_MY;
  str = cwal_new_stringf(args->engine,
                         "Hello from 0x%p->foo()", (void const *)self);
  if(!str) return CWAL_RC_OOM;
  else{
    *rv = cwal_string_value(str);
    return 0;
  }
}

/**
   A cwal_callback_f() implementation which implements a "manual
   destructor" for cwal-bound my_native objects.  This binding is not
   strictly necessary but gives us a way to manually, from script
   code, trigger the cleanup of the C-native parts of of a cwal-bound
   my_native instance.
*/
static int my_cb_destroy( cwal_callback_args const * args,
                          cwal_value **rv ){
  cwal_native * nv;
  THIS_MY;
  nv = cwal_value_native_part(args->engine, args->self, &my_native_empty);
  assert(nv && "This cannot be NULL if we got this far.");
  cwal_native_clear(nv, 1)
    /* ^^^ That cleans up 'self' immediately and unlinks nv from
       self, such that future calls to the MyModule methods on
       that instance will result in the exception triggered by the
       THIS_MY macro.
    */;
  *rv = cwal_value_undefined();
  return 0;
}


/**
   A whcl_module_init_f() implementation which gets called by
   whcl_module_load() after opening the module's DLL. (Alternately, if
   the plugin is statically linked in to the application, it gets
   called via a different route.)

   Modules must return their functionality by assigning *rv to a
   cwal_value. Typically it's an Object containing the module's API,
   but it could be an array, an integer, or even a cwal_value_true().
*/
static int whcl_module_init_sample( whcl_engine * const el, cwal_value ** rv ){
  int rc;
  cwal_value * v;
  cwal_value * mod;
  my_native * my;
  cwal_native * modN;
  cwal_engine * const ce = whcl_engine_cwal(el);
  assert(rv);
  my = (my_native*)cwal_malloc(ce, sizeof(my_native));
  if(!my) return CWAL_RC_OOM;
  *my = my_native_empty;
  mod = cwal_new_native_value(ce,
                              my /* native instance */,
                              my_native_finalize /* finalizer function */,
                              &my_native_empty /* custom type ID */);
  if(!mod){
    /* We still own 'my' */
    cwal_free(ce, my);
    return CWAL_RC_OOM;
  }
  modN = cwal_value_get_native(mod);
  /* To demonstrate the relationships between mod, modN, and my: */
  assert(modN);
  assert(mod == cwal_native_value(modN));
  assert(my == cwal_native_get(modN, &my_native_empty));
  assert(NULL == cwal_native_get(modN, ce/*arbitrary pointer*/));
  cwal_ref(mod);
  cwal_native_set_rescoper( modN, cwal_value_rescoper_f_my );
  cwal_value_prototype_set( mod, whcl_prototype_object(el) );
  rc = whcl_install_typename(el, mod, "SampleModuleNative");
  if(rc) goto end;
  /* Some helper macros which don't really pay off in a small
     module like this one, but get lots of copy/paste re-use
     between historical modules...
  */
#define CHECKV if(!v){ WHCL__WARN_OOM; rc = CWAL_RC_OOM; goto end; } (void)0
#define SET(KEY) \
  cwal_ref(v);   \
  rc = whcl_set( el, mod, KEY, (cwal_int_t)cwal_strlen(KEY), v ); \
  cwal_unref(v); \
  v = 0;         \
  if(rc) goto end

  /* Install some arbitrary value... */
  v = cwal_new_integer(ce, 42);
  SET("the-answer");

  /* Install functions... */
  {
      whcl_func_def const funcs[] = {
        WHCL_FUNC2("foo", my_cb_foo),
        WHCL_FUNC2("destroy", my_cb_destroy),
        whcl_func_def_empty_m
      };
      rc = whcl_install_functions(el, mod, funcs, 0);
  }
    
#undef SET
#undef CHECKV
  end:
  if(rc){
    cwal_unref(mod);
  }else{
    cwal_unhand(mod);
    *rv = mod;
  }
  return rc;
}

/**
   This macro sets up the bits which the module loader looks for.
   The macro appends the symbol passed to it to "whcl_module_init_"
   to get the name of our module init function, then creates a
   struct to hold the module's name and init function. The module loader
   looks for that struct under a name installed by this macro and uses
   it to find and call the module's init function.
*/
WHCL_MODULE_REGISTER_(sample);

#undef THIS_MY
#undef MARKER
