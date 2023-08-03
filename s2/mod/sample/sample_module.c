/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/**
   Example s2 loadable module.
*/
#include "libs2.h"

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
   Custom client-side native type to bind to s2.
*/
struct my_native {
  /* Put your data here. For now we just use a placeholder. */
  int myField;
};
typedef struct my_native my_native;

/**
   Serves as a copy-constructable default instance and as a type ID
   pointer for cwal_new_native() and friends.
*/
static const my_native my_native_empty = {42};

/**
   A cwal_finalizer_f() implementation which we associate with our
   my_native binding. It will be called by the cwal-side garbage
   collection/finalization process when a cwal-bound my_native is
   cleaned up.
*/
static void my_native_finalize( cwal_engine * e, void * v ){
  my_native * my = (my_native *)v;
  MARKER(("Finalizing my_native@%p (and avoiding an "
          "unused var warning while doing it)\n", (void *)my));
  cwal_free2( e, v, sizeof(my_native)
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
static my_native * my_native_from_value( cwal_engine * e,
                                         cwal_value * v ){
  my_native * rc = NULL;
  do{
    cwal_native * n = cwal_value_get_native(v);
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
  if(!self) return cwal_exception_setf(args->engine, CWAL_RC_TYPE,      \
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
   A s2_module_init_f() implementation which gets called by
   s2_module_load() after opening the module's DLL. (Alternately, if
   the plugin is statically linked in to the application, it gets
   called via a different route.)

   Modules must return their functionality by assigning *rv to a
   cwal_value. Typically it's an Object containing the module's API,
   but it could be an array, an integer, or even a cwal_value_true().
*/
static int s2_module_init_sample( s2_engine * se, cwal_value ** rv ){
  int rc;
  cwal_value * v;
  cwal_value * mod;
  my_native * my;
  assert(rv);
  my = (my_native*)cwal_malloc(se->e, sizeof(my_native));
  if(!my) return CWAL_RC_OOM;
  *my = my_native_empty;
  mod = cwal_new_native_value(se->e,
                              my /* native instance */,
                              my_native_finalize /* finalizer function */,
                              &my_native_empty /* custom type ID */);
  if(!mod){
    /* We still own 'my' */
    cwal_free(se->e, my);
    return CWAL_RC_OOM;
  }
  cwal_value_ref(mod);
  /*
    mod's prototype is set to the base Object prototype
    automatically by s2/cwal, but if you want to replace a
    prototype, here's how:
       
    cwal_value_prototype_set( mod, s2_prototype_object(se) );
  */

  /* Some helper macros which don't really pay off in a small
     module like this one, but get lots of copy/paste re-use
     between historical modules...
  */
#define CHECKV if(!v){ rc = CWAL_RC_OOM; goto end; } (void)0
#define SET(KEY) \
  cwal_value_ref(v); \
  rc = cwal_prop_set( mod, KEY, cwal_strlen(KEY), v );  \
  cwal_value_unref(v); \
  v = 0;               \
  if(rc) goto end

#define FUNC(NAME,FP)                           \
  v = cwal_new_function_value( se->e, FP, 0, 0, 0 );    \
  CHECKV;                                       \
  SET(NAME);                                    \
  assert(!rc);                                  \
  if(rc) goto end

  /* Install some arbitrary value... */
  v = cwal_new_integer(se->e, 42);
  SET("theAnswer");

  /* Install functions... */
  {
      s2_func_def const funcs[] = {
      S2_FUNC2("foo", my_cb_foo),
      S2_FUNC2("destroy", my_cb_destroy),
      s2_func_def_empty_m
      };
      rc = s2_install_functions(se, mod, funcs, 0);
  }
    
#undef SET
#undef FUNC
#undef CHECKV
  end:
  if(rc){
    cwal_value_unref(mod);
  }else{
    cwal_value_unhand(mod);
    *rv = mod;
  }
  return rc;
}


/**
   Module initializer for when the module is compiled in a DLL with
   other modules.
*/
S2_MODULE_REGISTER_(sample);

#undef THIS_MY
#undef MARKER
