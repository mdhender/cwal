/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

#include "s2_internal.h"

#if S2_ENABLE_FFI

#include <assert.h>
#include <malloc.h>
#include <string.h>
#include <avcall.h>
#include <callback.h>

#if 0
#include <stdio.h>
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__); if(1) printf pfexp
#else
#define MARKER(pfexp) (void)0
#endif

#if !S2_HAVE_DLOPEN && !S2_HAVE_LTDLOPEN
#  error "We have no dlopen() impl for this configuration."
#elif S2_HAVE_LTDLOPEN
#  include <ltdl.h>
#elif S2_HAVE_DLOPEN
#  define __USE_GNU
#  include <dlfcn.h> /* this actually has a different name on some platforms! */
#endif

/* TODO: struct, schar, uchar, ushort, uint, ulong, longlong, ulonglong */
/* NOTE: Certain functionality break the c89 standard... should explicitly enable from Makefile. */

enum {
  FFI_TYPE_VOID,
  FFI_TYPE_CHAR,
  FFI_TYPE_SHORT,
  FFI_TYPE_INT,
  FFI_TYPE_LONG,
  FFI_TYPE_FLOAT,
  FFI_TYPE_DOUBLE,
  FFI_TYPE_CSTR,
  FFI_TYPE_PTR
};

static const int ffi_native_type = 9001; /* it's over 9000!? */

static void *s2_cb_ffi_handle = NULL;

struct s2_cb_ffi_callback_type {
  cwal_engine *engine;
  cwal_value *self;
  cwal_function *f;
  uint16_t num_types;
  uint16_t *types;
};
typedef struct s2_cb_ffi_callback_type s2_cb_ffi_callback_type;

struct s2_ffi_map_type {
  cwal_int_t rtype;
  cwal_size_t num_types;
  cwal_int_t *types;
  uintptr_t func_handle;
};
typedef struct s2_ffi_map_type s2_ffi_map_type;

#if S2_ENABLE_FFI_IMPORT

#include <regex.h>
#include <unistd.h>
#include <libgen.h>
#include <stdlib.h>

int s2_ffi_import_ready = -1;

/* TODO: Add support for a newline escape sequence at the end of a line: \ */
/* TODO: Strip all comments before parsing, because they could definitely cause problems. */
/* NOTE: A comment contained in a string is valid. Those don't count. */

regex_t s2_ffi_regex_include;
#define s2_ffi_regex_include_str "^[[:space:]]*#[[:space:]]*include[[:space:]][[:space:]]*[<\"]\\([^>\"]*\\).*$"

regex_t s2_ffi_regex_define;
#define s2_ffi_regex_define_str "^[[:space:]]*#[[:space:]]*define[[:space:]][[:space:]]*\\([^[:space:]]*\\)[[:space:]]*\\(.*\\)$"

regex_t s2_ffi_regex_typedef;
#define s2_ffi_regex_typedef_str "typedef[[:space:]][[:space:]]*\\([[:alnum:]_\\*[:space:]]*\\)[[:space:]]\\([[:alnum:]_*][[:alnum:]_*]*\\)[[:space:]]*;"

regex_t s2_ffi_regex_proto;
#define s2_ffi_regex_proto_str "\\([[:alnum:]_][[:alnum:]_\\*[:space:]]*\\)[[:space:]][[:space:]]*\\([[:alnum:]_][[:alnum:]_]*\\)[[:space:]]*([[:space:]]*\\([^)]*\\))[[:space:]]*;"

struct s2_ffi_submatch_type {
  size_t matchlen;
  char *match;
};
typedef struct s2_ffi_submatch_type s2_ffi_submatch_type;

struct s2_ffi_match_type {
  int num_matches;
  size_t num_submatches;
  s2_ffi_submatch_type *matches;
};
typedef struct s2_ffi_match_type s2_ffi_match_type;

typedef enum {
  FFI_IMPORT_IDLE,
  FFI_IMPORT_IMPORTING,
  FFI_IMPORT_IMPORTED,
  FFI_IMPORT_FAILED_TO_FIND_FILE,
  FFI_IMPORT_FAILED_TO_READ_FILE,
  FFI_IMPORT_SKIPPED
} FFI_IMPORT_STATE;

struct s2_ffi_import_library_type {
  char *filename;
  void *handle;
};
typedef struct s2_ffi_import_library_type s2_ffi_import_library_type;

struct s2_ffi_import_header_type {
  char *filename;
  char *content;
  FFI_IMPORT_STATE processed;
};
typedef struct s2_ffi_import_header_type s2_ffi_import_header_type;

struct s2_ffi_import_path_type {
  cwal_size_t pathlen;
  char *path;
};
typedef struct s2_ffi_import_path_type s2_ffi_import_path_type;

/* TODO: Make this configurable. */
#define s2_ffi_import_num_paths 2
s2_ffi_import_path_type s2_ffi_import_paths[s2_ffi_import_num_paths] = {
  { 13, "/usr/include/" },
  { 49, "/usr/lib64/gcc/x86_64-gentoo-linux/5.4.0/include/" }
};

struct s2_ffi_import_type {
  cwal_size_t num_libraries;
  s2_ffi_import_library_type *libraries;

  cwal_size_t num_headers;
  s2_ffi_import_header_type *headers;
};
typedef struct s2_ffi_import_type s2_ffi_import_type;

#endif

/**
   Native Callback Test Helper
*/
int s2_cb_ffi_test(cwal_callback_args const *args, cwal_value **rv) {
  static void (*s2_cb_ffi_test0)(void);
  static char (*s2_cb_ffi_test1)(char);
  static short (*s2_cb_ffi_test2)(short);
  static int (*s2_cb_ffi_test3)(int);
  static long (*s2_cb_ffi_test4)(long);
  static float (*s2_cb_ffi_test5)(float);
  static double (*s2_cb_ffi_test6)(double);
  static const char *(*s2_cb_ffi_test7)(const char *);
  static void *(*s2_cb_ffi_test8)(void *);
  if (args == 0) {
    *(void **) (&s2_cb_ffi_test0) = rv;
  } else if (args == (cwal_callback_args const *) 1) {
    *(void **) (&s2_cb_ffi_test1) = rv;
  } else if (args == (cwal_callback_args const *) 2) {
    *(void **) (&s2_cb_ffi_test2) = rv;
  } else if (args == (cwal_callback_args const *) 3) {
    *(void **) (&s2_cb_ffi_test3) = rv;
  } else if (args == (cwal_callback_args const *) 4) {
    *(void **) (&s2_cb_ffi_test4) = rv;
  } else if (args == (cwal_callback_args const *) 5) {
    *(void **) (&s2_cb_ffi_test5) = rv;
  } else if (args == (cwal_callback_args const *) 6) {
    *(void **) (&s2_cb_ffi_test6) = rv;
  } else if (args == (cwal_callback_args const *) 7) {
    *(void **) (&s2_cb_ffi_test7) = rv;
  } else if (args == (cwal_callback_args const *) 8) {
    *(void **) (&s2_cb_ffi_test8) = rv;
  } else if (args == (cwal_callback_args const *) 10) {
    s2_cb_ffi_test0();
    if (s2_cb_ffi_test1(-128) != -128) return -1;
    if (s2_cb_ffi_test2(-32768) != -32768) return -2;
    if (s2_cb_ffi_test3(-2147483647) != -2147483647) return -3;
    if (s2_cb_ffi_test4(2147483647) != 2147483647) return -4;
    if ((int) s2_cb_ffi_test5(9.000001) != 9) return -5;
    if ((int) s2_cb_ffi_test6(9.000000000001) != 9) return -6;
    if (strcmp(s2_cb_ffi_test7("const char *"), "const char *") != 0) return -7;
    if (strcmp((const char *) s2_cb_ffi_test8("void *"), "void *") != 0) return -8;
  } else {
    int (*s2_cb_ffi_test_ptr)(cwal_callback_args const *, cwal_value **) = s2_cb_ffi_test;
    cwal_native *ptr = cwal_new_native(args->engine, *(void **)(&s2_cb_ffi_test_ptr), NULL, &ffi_native_type);
    if(!ptr) return CWAL_RC_OOM;
    *rv = cwal_native_value(ptr);
  }
  return 0;
}

/**
   Native Callback
*/
void s2_cb_ffi_callback(void *data, va_alist valist) {
  s2_cb_ffi_callback_type func_data = *((s2_cb_ffi_callback_type *) data);
  cwal_value *rv = 0;
  uint16_t argc = 0;
  cwal_value **argv = NULL;
  char const * cstr = 0;
  S2_UNUSED_VAR int rc;
  int i = 0;

  MARKER(("callback (@0x%" PRIxPTR ", @0x%" PRIxPTR ")\n", (uintptr_t) func_data.f, (uintptr_t) func_data.self));

  if (func_data.num_types == 0) {
    rc = cwal_function_call(func_data.f, func_data.self, &rv, argc, argv);
    MARKER(("void callback: %i\n", rc));
    return;
  } else {
    /* prep callback ret type */
    switch (func_data.types[0]) {
    case FFI_TYPE_VOID:
      va_start_void(valist);
      break;
    case FFI_TYPE_CHAR:
      va_start_char(valist);
      break;
    case FFI_TYPE_SHORT:
      va_start_short(valist);
      break;
    case FFI_TYPE_INT:
      va_start_int(valist);
      break;
    case FFI_TYPE_LONG:
      va_start_long(valist);
      break;
    case FFI_TYPE_FLOAT:
      va_start_float(valist);
      break;
    case FFI_TYPE_DOUBLE:
      va_start_double(valist);
      break;
    case FFI_TYPE_CSTR:
      va_start_ptr(valist, const char *);
      break;
    case FFI_TYPE_PTR:
      va_start_ptr(valist, void *);
      break;
    default:
      MARKER(("unsupported return type: %i\n", func_data.types[0]));
      break;
    }

    /* prep args */
    /* TODO: if(!argv[i-1]) return CWAL_RC_OOM; */
    argv = malloc(sizeof(cwal_value *) * func_data.num_types - 1);
    for (i = 1; i <= func_data.num_types; ++i) {
      switch (func_data.types[i]) {
      case FFI_TYPE_CHAR:
        argv[i-1] = cwal_new_integer(func_data.engine, (int) va_arg_char(valist));
        break;
      case FFI_TYPE_SHORT:
        argv[i-1] = cwal_new_integer(func_data.engine, (int) va_arg_short(valist));
        break;
      case FFI_TYPE_INT:
        argv[i-1] = cwal_new_integer(func_data.engine, va_arg_int(valist));
        break;
      case FFI_TYPE_LONG:
        argv[i-1] = cwal_new_integer(func_data.engine, (int) va_arg_long(valist));
        break;
      case FFI_TYPE_FLOAT:
        argv[i-1] = cwal_new_double(func_data.engine, (double) va_arg_float(valist));
        break;
      case FFI_TYPE_DOUBLE:
        argv[i-1] = cwal_new_double(func_data.engine, va_arg_double(valist));
        break;
      case FFI_TYPE_CSTR:
        cstr = va_arg_ptr(valist, char *);
        argv[i-1]  = cwal_new_string_value(func_data.engine, cstr, cwal_strlen(cstr));
        break;
      case FFI_TYPE_PTR:
        argv[i-1]  = cwal_new_native_value(func_data.engine, va_arg_ptr(valist, void *), NULL, &ffi_native_type);
        break;
      default:
        MARKER(("unsupported arg type: %i\n", func_data.types[i]));
        break;
      }
    }

    /* make the call! */
    rc = cwal_function_call(func_data.f, func_data.self, &rv, func_data.num_types - 1, argv);
    free(argv);
    MARKER(("typed callback: %i\n", rc));

    /* get the return value and clean up */
    switch (func_data.types[0]) {
    case FFI_TYPE_VOID:
      va_return_void(valist);
      break;
    case FFI_TYPE_CHAR:
      va_return_char(valist, cwal_value_get_integer(rv));
      break;
    case FFI_TYPE_SHORT:
      va_return_short(valist, cwal_value_get_integer(rv));
      break;
    case FFI_TYPE_INT:
      va_return_int(valist, cwal_value_get_integer(rv));
      break;
    case FFI_TYPE_LONG:
      va_return_long(valist, cwal_value_get_integer(rv));
      break;
    case FFI_TYPE_FLOAT:
      va_return_float(valist, cwal_value_get_double(rv));
      break;
    case FFI_TYPE_DOUBLE:
      va_return_double(valist, cwal_value_get_double(rv));
      break;
    case FFI_TYPE_CSTR:
      va_return_ptr(valist, const char *, cwal_value_get_cstr(rv, 0));
      break;
    case FFI_TYPE_PTR:
      va_return_ptr(valist, void *, cwal_native_get(cwal_value_get_native(rv), &ffi_native_type));
      break;
    default:
      MARKER(("unsupported return type: %i\n", func_data.types[0]));
      break;
    }
  }
}

/**
   Cleanup Callbacks
*/
static void s2_cb_ffi_callback_finalizer(cwal_engine *e, void *m){
  void *callback_data = callback_data(m);
  s2_cb_ffi_callback_type func_data = *((s2_cb_ffi_callback_type *) callback_data);
  __TR_function tramp;
  assert(m);

  if (func_data.num_types > 0) {
    MARKER(("cleaning callback args: %i\n", func_data.num_types));
    free(func_data.types);
  }

  MARKER(("cleaning callback: %" PRIxPTR " (data: %" PRIxPTR ")\n", (uintptr_t) m, (uintptr_t) callback_data));
  free(callback_data);

  *(int **) (&tramp) = m;
  free_callback(tramp);
}

/**
   Cleanup After FFI
*/
static void s2_cb_ffi_finalizer(cwal_engine *e, void *m){
  if (s2_cb_ffi_handle != NULL) {
    MARKER(("dlclose handle: %" PRIxPTR "\n", (uintptr_t) s2_cb_ffi_handle));
    dlclose(s2_cb_ffi_handle);
    s2_cb_ffi_handle = NULL;
  }
}

/**
   Execute a Native Function
*/
int s2_cb_ffi_exec(cwal_callback_args const *args, cwal_value **rv) {
  int rc;
  cwal_int_t rtype;
  int x;
  cwal_array *argTypes = NULL;
  uintptr_t func_handle;
  av_alist avlist;
  char ret_char;
  short ret_short;
  int ret_int;
  long ret_long;
  float ret_float;
  double ret_double;
  void *ret_ptr;

  /* is this a mapped function? */
  s2_ffi_map_type *s2_ffi_map = (s2_ffi_map_type *) cwal_function_state_get(args->callee, &ffi_native_type);
  if (s2_ffi_map == NULL) {
    /* unmapped function call */
    if((rc = s2_clampdown_level_check_cb(args, 1))) return rc;
    if(args->argc < 1){
      goto misuse;
    }else if(args->argc == 1){
      /* default to void return type */
      rtype = 0;
    }else{
      if(cwal_value_is_array(args->argv[1])){
        /* get and set return type */
        argTypes = cwal_value_get_array(args->argv[1]);
        rtype = cwal_value_get_integer(cwal_array_get(argTypes, 0));
      }else if(cwal_value_is_integer(args->argv[1])){
        /* set return type */
        rtype = cwal_value_get_integer(args->argv[1]);
      }else{
        goto misuse;
      }
    }
    MARKER(("rtype: %i\n", (int) rtype));

    /* get global handle */
    if (s2_cb_ffi_handle == NULL) {
      s2_cb_ffi_handle = dlopen(0, RTLD_NOW | RTLD_GLOBAL);
      MARKER(("dlopen handle: %" PRIxPTR "\n", (uintptr_t) s2_cb_ffi_handle));
      if(!s2_cb_ffi_handle) return cwal_exception_setf(args->engine, CWAL_RC_ASSERT, "dlopen failed.");
    }

    if (cwal_value_is_native(args->argv[0])) {
      /* get the function handle */
      func_handle = (uintptr_t) cwal_native_get(cwal_value_get_native(args->argv[0]), &ffi_native_type);
      MARKER(("native func_handle: @0x%" PRIxPTR "\n", func_handle));
    } else if (cwal_value_is_string(args->argv[0])) {
      /* find the function handle */
      func_handle =
#if S2_HAVE_LTDLOPEN
        (uintptr_t) lt_dlsym(s2_cb_ffi_handle, cwal_string_cstr(cwal_value_get_string(args->argv[0])));
#elif S2_HAVE_DLOPEN
        (uintptr_t) dlsym(s2_cb_ffi_handle, cwal_string_cstr(cwal_value_get_string(args->argv[0])));
#else
        0;
#endif
      MARKER(("dlsym func_handle: @0x%" PRIxPTR "\n", (uintptr_t) func_handle));
      if(!func_handle) return cwal_exception_setf(args->engine, CWAL_RC_ASSERT, "dlsym failed to find that symbol.");
    } else {
      goto misuse;
    }
    /* end unmapped function call */
  } else {
    /* mapped function call */
    MARKER(("calling mapped function: @0x%" PRIxPTR " (ret: %i) (argc: %i)\n", s2_ffi_map->func_handle, (int) s2_ffi_map->rtype, (int) s2_ffi_map->num_types));
    func_handle = s2_ffi_map->func_handle;
    rtype = s2_ffi_map->rtype;
    /* end mapped function call */
  }

  /* prep function call */
  switch (rtype) {
  case FFI_TYPE_VOID:
    av_start_void(avlist, func_handle);
    break;
  case FFI_TYPE_CHAR:
    av_start_char(avlist, func_handle, &ret_char);
    break;
  case FFI_TYPE_SHORT:
    av_start_short(avlist, func_handle, &ret_short);
    break;
  case FFI_TYPE_INT:
    av_start_int(avlist, func_handle, &ret_int);
    break;
  case FFI_TYPE_LONG:
    av_start_long(avlist, func_handle, &ret_long);
    break;
  case FFI_TYPE_FLOAT:
    av_start_float(avlist, func_handle, &ret_float);
    break;
  case FFI_TYPE_DOUBLE:
    av_start_double(avlist, func_handle, &ret_double);
    break;
  case FFI_TYPE_CSTR:
    av_start_ptr(avlist, func_handle, const char *, &ret_ptr);
    break;
  case FFI_TYPE_PTR:
    av_start_ptr(avlist, func_handle, void *, &ret_ptr);
    break;
  default:
    MARKER(("unsupported return type: %i\n", (int) rtype));
    break;
  }

  /* push arguments to stack */
  MARKER(("argc: %i\n", (s2_ffi_map == NULL) ? args->argc - 2 : args->argc));
  for(x = (s2_ffi_map == NULL) ? 2 : 0; x < args->argc; ++x){
    switch((s2_ffi_map == NULL) ? cwal_value_get_integer(cwal_array_get(argTypes, x-1)) : s2_ffi_map->types[x]) {
    case FFI_TYPE_CHAR:
      av_char(avlist, (char) cwal_value_get_integer(args->argv[x]));
      MARKER(("push char: %i\n", (int) cwal_value_get_integer(args->argv[x])));
      break;
    case FFI_TYPE_SHORT:
      av_short(avlist, (short) cwal_value_get_integer(args->argv[x]));
      MARKER(("push short: %i\n", (int) cwal_value_get_integer(args->argv[x])));
      break;
    case FFI_TYPE_INT:
      av_int(avlist, cwal_value_get_integer(args->argv[x]));
      MARKER(("push int: %i\n", (int) cwal_value_get_integer(args->argv[x])));
      break;
    case FFI_TYPE_LONG:
      av_long(avlist, (long) cwal_value_get_integer(args->argv[x]));
      MARKER(("push long: %i\n", (int) cwal_value_get_integer(args->argv[x])));
      break;
    case FFI_TYPE_FLOAT:
      av_float(avlist, (float) cwal_value_get_double(args->argv[x]));
      MARKER(("push float: %f\n", (float) cwal_value_get_double(args->argv[x])));
      break;
    case FFI_TYPE_DOUBLE:
      av_double(avlist, cwal_value_get_double(args->argv[x]));
      MARKER(("push double: %f\n", (double) cwal_value_get_double(args->argv[x])));
      break;
    case FFI_TYPE_CSTR:
      if (cwal_value_is_string(args->argv[x])) {
        av_ptr(avlist, const char *, cwal_string_cstr(cwal_value_get_string(args->argv[x])));
        MARKER(("push cstr: %s (0x%" PRIxPTR ")\n", cwal_string_cstr(cwal_value_get_string(args->argv[x])), (uintptr_t) cwal_string_cstr(cwal_value_get_string(args->argv[x]))));
      } else {
        av_ptr(avlist, const char *, cwal_native_get(cwal_value_get_native(args->argv[x]), &ffi_native_type));
        MARKER(("push cstr (native): %s (0x%" PRIxPTR ")\n", cwal_string_cstr(cwal_value_get_string(args->argv[x])), (uintptr_t) cwal_native_get(cwal_value_get_native(args->argv[x]), &ffi_native_type)));
      }
      break;
    case FFI_TYPE_PTR:
      if (cwal_value_is_string(args->argv[x])) {
        av_ptr(avlist, void *, cwal_string_cstr(cwal_value_get_string(args->argv[x])));
        MARKER(("push ptr (cstr): 0x%" PRIxPTR "\n", (uintptr_t) cwal_string_cstr(cwal_value_get_string(args->argv[x]))));
      } else if (cwal_value_is_array(args->argv[x])) {
        unsigned int i;
        cwal_array *argArray = cwal_value_get_array(args->argv[x]);
        unsigned int len = cwal_array_length_get(argArray);
        void *ptr = malloc(sizeof(void *));

        if (len > 0) {
          /* TODO: populate an array with the values from s2 */
          MARKER(("UNIMPLEMENTED\n"));
          /* determine array type from first element in array */
          for (i = 0; i < len; ++i) {
            /* switch types */
          }
        }

        av_ptr(avlist, void *, ptr);
        MARKER(("push ptr (array): 0x%" PRIxPTR "\n", (uintptr_t) ptr));
      } else if (cwal_value_is_function(args->argv[x])) {
        void *data = malloc(sizeof(s2_cb_ffi_callback_type));
        s2_cb_ffi_callback_type callback_data;
        cwal_array *types_array;
        uintptr_t callback;
        cwal_value *new_proto;
        cwal_value *prev_proto;

        /* needed later for cwal_function_call */
        callback_data.engine = args->engine;
        callback_data.self = args->self;
        callback_data.f = cwal_value_get_function(args->argv[x]);

        /* prepare the callback types */
        types_array = cwal_value_get_array(cwal_prop_get(args->argv[x], "ffiTypes", 8));
        if (types_array && (callback_data.num_types = cwal_array_length_get(types_array)) > 0 ) {
          int i;
          MARKER(("callback has args: %i\n", callback_data.num_types));
          callback_data.types = malloc(sizeof(uint16_t) * cwal_array_length_get(types_array));
          for (i = 0; i < callback_data.num_types; ++i) {
            callback_data.types[i] = cwal_value_get_integer(cwal_array_get(types_array, i));
          }
        } else {
          callback_data.num_types = 0;
        }
        memcpy(data, (void *) &callback_data, sizeof(s2_cb_ffi_callback_type));

        /* allocate and push the callback argument */
        callback = (uintptr_t) alloc_callback(&s2_cb_ffi_callback, data);
        new_proto = cwal_new_native_value(args->engine, (void *) callback, s2_cb_ffi_callback_finalizer, &ffi_native_type);
        prev_proto = cwal_value_prototype_get(args->engine, args->argv[x]);
        cwal_value_prototype_set(new_proto, prev_proto);
        cwal_value_prototype_set(args->argv[x], new_proto);
        av_ptr(avlist, void *, callback);
        MARKER(("push ptr (function): 0x%" PRIxPTR " (@0x%" PRIxPTR ", @0x%" PRIxPTR ")\n", (uintptr_t) callback, (uintptr_t) callback_data.f, (uintptr_t) callback_data.self));
      } else {
        av_ptr(avlist, void *, cwal_native_get(cwal_value_get_native(args->argv[x]), &ffi_native_type));
        MARKER(("push ptr (native): 0x%" PRIxPTR "\n", (uintptr_t) cwal_native_get(cwal_value_get_native(args->argv[x]), &ffi_native_type)));
      }
      break;
    default:
      MARKER(("unsupported arg type: %i\n", (int) cwal_value_get_integer(cwal_array_get(argTypes, x-1))));
      break;
    }
  }

  /* call the function */
  MARKER(("calling the ffi now...\n"));
  av_call(avlist);

  /* set return value */
  switch (rtype) {
  case FFI_TYPE_VOID:
    MARKER(("ret void\n"));
    return rc;
  case FFI_TYPE_CHAR:
    MARKER(("ret char: %i\n", ret_char));
    *rv = cwal_new_integer(args->engine, (int) ret_char);
    break;
  case FFI_TYPE_SHORT:
    MARKER(("ret short: %i\n", ret_short));
    *rv = cwal_new_integer(args->engine, (int) ret_short);
    break;
  case FFI_TYPE_INT:
    MARKER(("ret int: %i\n", ret_int));
    *rv = cwal_new_integer(args->engine, ret_int);
    break;
  case FFI_TYPE_LONG:
    MARKER(("ret long: %li\n", ret_long));
    *rv = cwal_new_integer(args->engine, (int) ret_long);
    break;
  case FFI_TYPE_FLOAT:
    MARKER(("ret float: %f\n", ret_float));
    *rv = cwal_new_double(args->engine, (double) ret_float);
    break;
  case FFI_TYPE_DOUBLE:
    MARKER(("ret double: %f\n", ret_double));
    *rv = cwal_new_double(args->engine, ret_double);
    break;
  case FFI_TYPE_CSTR:
    MARKER(("ret cstr: %s (0x%" PRIxPTR ")\n", (const char *) ret_ptr, (uintptr_t) ret_ptr));
    *rv = cwal_new_string_value(args->engine, ret_ptr, cwal_strlen(ret_ptr));
    break;
  case FFI_TYPE_PTR:
    MARKER(("ret ptr: 0x%" PRIxPTR "\n", (uintptr_t) ret_ptr));
    *rv = cwal_new_native_value(args->engine, ret_ptr, NULL, &ffi_native_type);
    break;
  default:
    MARKER(("unsupported return type: %i\n", (int) rtype));
    return rc;
  }

  /* return status */
  if (!*rv) rc = CWAL_RC_OOM;
  return rc;

  misuse:
  return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                           "Expecting (String|Native) or (String|Native, s2.ffi.TYPE, ...) "
                           "or (String|Native, [ s2.ffi.TYPE, ... ], ...) arguments.");
}

/**
   Cleanup Mapped Functions
*/
static void s2_cb_ffi_map_finalizer(cwal_engine *e, void *m){
  s2_ffi_map_type *s2_ffi_map = (s2_ffi_map_type *) m;

  if (s2_ffi_map->num_types > 0) {
    MARKER(("cleaning map: %p (%p: %i args)\n", m, (void *) s2_ffi_map->types, (int) s2_ffi_map->num_types));
    free(s2_ffi_map->types);
  } else {
    MARKER(("cleaning map: %p\n", m));
  }
  free(m);
}

/**
   Map a Native Function to s2
*/
int s2_cb_ffi_map(cwal_callback_args const *args, cwal_value **rv) {
  int rc;
  cwal_int_t rtype;
  cwal_array *argTypes = NULL;
  uintptr_t func_handle;
  s2_ffi_map_type *s2_ffi_map;
  cwal_size_t i;

  /* sanity checks */
  if( (rc = s2_clampdown_level_check_cb(args, 1)) ) return rc;
  if(args->argc < 1){
    goto misuse;
  }else if(args->argc == 1){
    /* default to void return type */
    rtype = 0;
  }else{
    if(cwal_value_is_array(args->argv[1])){
      /* get and set return type */
      argTypes = cwal_value_get_array(args->argv[1]);
      rtype = cwal_value_get_integer(cwal_array_get(argTypes, 0));
    }else if(cwal_value_is_integer(args->argv[1])){
      /* set return type */
      rtype = cwal_value_get_integer(args->argv[1]);
    }else{
      goto misuse;
    }
  }
  MARKER(("rtype: %i\n", (int) rtype));

  /* get global handle */
  if (s2_cb_ffi_handle == NULL) {
    s2_cb_ffi_handle = dlopen(0, RTLD_NOW | RTLD_GLOBAL);
    MARKER(("dlopen handle: %" PRIxPTR "\n", (uintptr_t) s2_cb_ffi_handle));
    if(!s2_cb_ffi_handle) return cwal_exception_setf(args->engine, CWAL_RC_ASSERT, "dlopen failed.");
  }

  if (cwal_value_is_native(args->argv[0])) {
    /* get the function handle */
    func_handle = (uintptr_t) cwal_native_get(cwal_value_get_native(args->argv[0]), &ffi_native_type);
    MARKER(("native func_handle: @0x%" PRIxPTR "\n", func_handle));
  } else if (cwal_value_is_string(args->argv[0])) {
    /* find the function handle */
    func_handle =
#if S2_HAVE_LTDLOPEN
      (uintptr_t) lt_dlsym(s2_cb_ffi_handle, cwal_string_cstr(cwal_value_get_string(args->argv[0])));
#elif S2_HAVE_DLOPEN
      (uintptr_t) dlsym(s2_cb_ffi_handle, cwal_string_cstr(cwal_value_get_string(args->argv[0])));
#else
      0;
#endif
    MARKER(("dlsym func_handle: @0x%" PRIxPTR "\n", (uintptr_t) func_handle));
    if(!func_handle) return cwal_exception_setf(args->engine, CWAL_RC_ASSERT, "dlsym failed to find that symbol.");
  } else {
    goto misuse;
  }

  /* All system go! Map to an s2 function. */
  s2_ffi_map = malloc(sizeof(s2_ffi_map_type));
  s2_ffi_map->func_handle = func_handle;
  s2_ffi_map->rtype = rtype;
  s2_ffi_map->num_types = (argTypes == NULL) ? 0 : cwal_array_length_get(argTypes) - 1;
  s2_ffi_map->types = (s2_ffi_map->num_types > 0) ? malloc(sizeof(cwal_int_t) * s2_ffi_map->num_types) : NULL;

  /* store argument types */
  MARKER(("mapped args: %i (%" PRIxPTR ")\n", (int) s2_ffi_map->num_types, (uintptr_t) s2_ffi_map->types));
  for(i = 1; i <= s2_ffi_map->num_types; ++i) {
    s2_ffi_map->types[i-1] = cwal_value_get_integer(cwal_array_get(argTypes, i));
    MARKER(("arg(%i): %i\n", (int) i-1, (int) s2_ffi_map->types[i-1]));
  }

  /* return the mapped function */
  *rv = cwal_new_function_value(args->engine, s2_cb_ffi_exec, (void *) s2_ffi_map, s2_cb_ffi_map_finalizer, &ffi_native_type);
  return 0;

misuse:
  return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                             "Expecting (String|Native) or (String|Native, s2.ffi.TYPE) "
                                 "or (String|Native, [ s2.ffi.TYPE, ... ]) arguments.");
}

#if S2_ENABLE_FFI_IMPORT

/**
   Cleanup Imports
*/
static void s2_cb_ffi_import_finalizer(cwal_engine *e, void *m){
  s2_ffi_import_type *s2_ffi_import = (s2_ffi_import_type *) m;
  cwal_size_t i;

  /* one time tear down */
  if (s2_ffi_import_ready != -1) {
    MARKER(("cleaning regexes\n"));
    regfree(&s2_ffi_regex_include);
    regfree(&s2_ffi_regex_define);
    regfree(&s2_ffi_regex_typedef);
    regfree(&s2_ffi_regex_proto);
    s2_ffi_import_ready = -1;
  }

  MARKER(("cleaning imported libraries: %i (%" PRIxPTR ")\n", (int) s2_ffi_import->num_libraries, (uintptr_t) s2_ffi_import->libraries));
  for (i = 0; i < s2_ffi_import->num_libraries; ++i) {
    MARKER(("cleaning imported library: %s (%" PRIxPTR ")\n", s2_ffi_import->libraries[i].filename, (uintptr_t) s2_ffi_import->libraries[i].handle));
    free(s2_ffi_import->libraries[i].filename);
    dlclose(s2_ffi_import->libraries[i].handle);
  }
  free(s2_ffi_import->libraries);

  MARKER(("cleaning imported headers: %i (%" PRIxPTR ")\n", (int) s2_ffi_import->num_headers, (uintptr_t) s2_ffi_import->headers));
  for (i = 0; i < s2_ffi_import->num_headers; ++i) {
    MARKER(("cleaning imported header: %s (%" PRIxPTR ", %" PRIxPTR ")\n", s2_ffi_import->headers[i].filename, (uintptr_t) s2_ffi_import->headers[i].filename, (uintptr_t) s2_ffi_import->headers[i].content));
    free(s2_ffi_import->headers[i].filename);
    free(s2_ffi_import->headers[i].content);
  }
  free(s2_ffi_import->headers);

  MARKER(("cleaning import: %" PRIxPTR "\n", (uintptr_t) m));
  free(m);
}

/**
   Read and Return Regex Matches (TODO: Rewrite this to be iterative, like strtok_r, instead of all at once.)
*/
s2_ffi_match_type s2_ffi_read_matches(regex_t *regex, const char *file, int include_full_match) {
  int pos = 0, i;
  s2_ffi_match_type matches = {0};
  regmatch_t *match = malloc(sizeof(regmatch_t) * (regex->re_nsub + 1));

  /* counter */
  while (regexec(regex, file + pos, 1, match, 0) == 0) {
    ++matches.num_matches;
    pos += match->rm_eo;
  }

  /* shortcut */
  if (matches.num_matches == 0) {
    free(match);
    return matches;
  }

  /* storage */
  matches.num_submatches = regex->re_nsub;
  matches.matches = malloc(sizeof(s2_ffi_submatch_type) * (matches.num_matches * (matches.num_submatches + 1)));

  /* save matches */
  matches.num_matches = 0;
  pos = 0;
  while (regexec(regex, file + pos, regex->re_nsub + 1, match, 0) == 0) {
    for (i = (include_full_match == 1 ? 0 : 1); i < (int) regex->re_nsub + 1; ++i) {
      matches.matches[matches.num_matches].matchlen = (size_t) match[i].rm_eo - match[i].rm_so;
      matches.matches[matches.num_matches].match = malloc(sizeof(char) * (matches.matches[matches.num_matches].matchlen + 1));
      if (matches.matches[matches.num_matches].matchlen > 0) memcpy(matches.matches[matches.num_matches].match, file + pos + match[i].rm_so, matches.matches[matches.num_matches].matchlen);
      matches.matches[matches.num_matches].match[matches.matches[matches.num_matches].matchlen] = 0;
      ++matches.num_matches;
    }
    pos += match->rm_eo;
  }

  /* housekeeping */
  free(match);

  /* shortcut */
  if (matches.num_matches == 0) {
    free(matches.matches);
    matches.matches = NULL;
    return matches;
  }

  /* result */
  return matches;
}

/**
   Return a File's Contents
*/
char *s2_ffi_read_file(const char *filename) {
  FILE *file;
  size_t fsize;
  char *buffer;
  file = fopen(filename, "r");
  if (!file) return NULL;
  fseek(file, 0, SEEK_END);
  fsize = (size_t) ftell(file);
  fseek(file, 0, SEEK_SET);
  buffer = malloc(fsize + 1);
  fread(buffer, fsize, 1, file);
  fclose(file);
  buffer[fsize] = 0;
  return buffer;
}

/**
   Dedupe Headers
*/
int s2_ffi_dedupe_header(s2_ffi_import_type *s2_ffi_import, const char *filename) {
  unsigned int k;
  for (k = 0; k < s2_ffi_import->num_headers; ++k) {
    if (s2_ffi_import->headers[k].processed != FFI_IMPORT_IDLE && strcmp(filename, s2_ffi_import->headers[k].filename) == 0) return k;
  }
  return -1;
}

/**
   Find Fully Qualified Filename
*/
char *s2_ffi_find_file(const char *filename, size_t namelen, s2_ffi_import_path_type *paths, int num_paths, char *parent_path) {
  int i = 0;
  char *fullname = NULL;
  size_t fullnamelen;

  /* shortcuts */
  if (namelen < 1) return NULL;
  if (access(filename, F_OK) != -1) {
    fullname = malloc(sizeof(char) * (namelen + 1));
    strcpy(fullname, filename);
    return fullname;
  }

  /* another (possible) shortcut */
  if (parent_path != NULL) {
    size_t pathlen = strlen(parent_path);
    fullnamelen = pathlen + 1 + namelen;
    fullname = malloc(sizeof(char) * fullnamelen + 1);
    memcpy(fullname, parent_path, pathlen);
    fullname[pathlen] = '/';
    strcpy(fullname + pathlen + 1, filename);
    if (access(fullname, F_OK) != -1) {
      return fullname;
    }
  }

  /* check paths */
  fullnamelen = 255; /* 255 chosen, mostly, arbitrarily */
  fullname = realloc(fullname, sizeof(char) * fullnamelen);
  if (namelen == 0) namelen = strlen(filename);
  for (i = 0; i < num_paths; ++i) {
    if (paths[i].pathlen + namelen > fullnamelen) {
      fullnamelen = paths[i].pathlen + namelen;
      fullname = realloc(fullname, sizeof(char) * fullnamelen + 1);
    }
    memcpy(fullname, paths[i].path, paths[i].pathlen);
    strcpy(fullname + paths[i].pathlen, filename);
    if (access(fullname, F_OK) != -1) {
      return fullname;
    }
  }

  /* failed */
  free(fullname);
  return NULL;
}

int s2_ffi_resolve_type(char *type, cwal_hash *hash) {
  char *word = NULL;
  char *ptr;
  cwal_value *value;
  int depth;

  /* shortcuts */
  if (strchr(type, '*') != NULL) return FFI_TYPE_PTR; /* TODO: we could determine pointer type, but it's not really necessary... yet? */
  if (strstr(type, "struct") != NULL) return FFI_TYPE_PTR;
  if (strstr(type, "enum") != NULL) return FFI_TYPE_INT;
  if (strstr(type, "void") != NULL) return FFI_TYPE_VOID;
  if (strstr(type, "char") != NULL) return FFI_TYPE_CHAR;
  if (strstr(type, "short") != NULL) return FFI_TYPE_SHORT;
  if (strstr(type, "int") != NULL) return FFI_TYPE_INT;
  if (strstr(type, "long") != NULL) return FFI_TYPE_LONG;
  if (strstr(type, "float") != NULL) return FFI_TYPE_FLOAT;
  if (strstr(type, "double") != NULL) return FFI_TYPE_DOUBLE;

  /* resolve each keyword */
  for (word = type; ; word = NULL) {
    if ((word = strtok_r(word, " ", &ptr)) == NULL) break;

    /* resolve nested defines/typedef (escape at depth of 4 to avoid infinite loop possibility) */
    for (depth = 0; depth < 4; ++depth) {
      value = cwal_hash_search(hash, word, 0);
      if (value == NULL) break;
      word = (char *) cwal_value_get_cstr(value, 0);
      if (word == NULL) break;
      if (strchr(word, '*') != NULL) return FFI_TYPE_PTR;
      if (strstr(word, "struct") != NULL) return FFI_TYPE_PTR;
      if (strstr(word, "enum") != NULL) return FFI_TYPE_INT;
      if (strstr(word, "void") != NULL) return FFI_TYPE_VOID;
      if (strstr(word, "char") != NULL) return FFI_TYPE_CHAR;
      if (strstr(word, "short") != NULL) return FFI_TYPE_SHORT;
      if (strstr(word, "int") != NULL) return FFI_TYPE_INT;
      if (strstr(word, "long") != NULL) return FFI_TYPE_LONG;
      if (strstr(word, "float") != NULL) return FFI_TYPE_FLOAT;
      if (strstr(word, "double") != NULL) return FFI_TYPE_DOUBLE;
    }
  }

  /* failed to resolve type */
  return -1;
}

/**
   Import Native Headers/Libraries
*/
int s2_cb_ffi_import(cwal_callback_args const *args, cwal_value **rv) {
  int rc;
  s2_ffi_import_type *s2_ffi_import;
  cwal_array *libraries;
  cwal_array *headers;
  cwal_hash *hash;
  cwal_size_t i;
  int j, k;
  size_t x;
  s2_ffi_match_type matches;

  /* sanity checks */
  if((rc = s2_clampdown_level_check_cb(args, 1))) return rc;
  if(cwal_function_state_get(args->callee, &ffi_native_type) != NULL) return 0;
  if(args->argc < 1) goto misuse;

  /* one time init */
  if (s2_ffi_import_ready == -1) {
    s2_ffi_import_ready = regcomp(&s2_ffi_regex_include, s2_ffi_regex_include_str, REG_NEWLINE | REG_ICASE);
    if (s2_ffi_import_ready) return s2_ffi_import_ready;

    s2_ffi_import_ready = regcomp(&s2_ffi_regex_define, s2_ffi_regex_define_str, REG_NEWLINE | REG_ICASE);
    if (s2_ffi_import_ready) return s2_ffi_import_ready;

    s2_ffi_import_ready = regcomp(&s2_ffi_regex_typedef, s2_ffi_regex_typedef_str, REG_NEWLINE | REG_ICASE);
    if (s2_ffi_import_ready) return s2_ffi_import_ready;

    s2_ffi_import_ready = regcomp(&s2_ffi_regex_proto, s2_ffi_regex_proto_str, REG_ICASE);
    if (s2_ffi_import_ready) return s2_ffi_import_ready;

    s2_ffi_import_ready = 0;

    /* get global handle */
    if (s2_cb_ffi_handle == NULL) {
      s2_cb_ffi_handle = dlopen(0, RTLD_NOW | RTLD_GLOBAL);
      MARKER(("dlopen handle: %" PRIxPTR "\n", (uintptr_t) s2_cb_ffi_handle));
      if(!s2_cb_ffi_handle) return cwal_exception_setf(args->engine, CWAL_RC_ASSERT, "dlopen failed.");
    }
  }

  /* prepare for the import */
  s2_ffi_import = malloc(sizeof(s2_ffi_import_type));
  *rv = cwal_new_hash_value(args->engine, 10000);
  hash = cwal_value_get_hash(*rv);
  s2_hash_dot_like_object(*rv, 1);
  cwal_value_prototype_set(*rv, cwal_new_native_value(args->engine, (void *) s2_ffi_import, s2_cb_ffi_import_finalizer, &ffi_native_type));

  if(args->argc == 2) {
    /* load libraries */
    libraries = cwal_value_get_array(args->argv[1]);
    s2_ffi_import->num_libraries = cwal_array_length_get(libraries);
    if (s2_ffi_import->num_libraries > 0) {
      s2_ffi_import->libraries = malloc(sizeof(s2_ffi_import_library_type) * s2_ffi_import->num_libraries);
      MARKER(("importing libraries: %i (%" PRIxPTR ")\n", (int) s2_ffi_import->num_libraries, (uintptr_t) s2_ffi_import->libraries));
      for (i = 0; i < s2_ffi_import->num_libraries; ++i) {
        const char *library = cwal_value_get_cstr(cwal_array_get(libraries, i), 0);
        s2_ffi_import->libraries[i].filename = malloc(strlen(library) + 1);
        strcpy(s2_ffi_import->libraries[i].filename, library);
        s2_ffi_import->libraries[i].handle = dlopen(s2_ffi_import->libraries[i].filename, RTLD_LAZY | RTLD_GLOBAL);
        MARKER(("imported library: %s (%" PRIxPTR ")\n", s2_ffi_import->libraries[i].filename, (uintptr_t) s2_ffi_import->libraries[i].handle));
      }
    } else {
      s2_ffi_import->libraries = NULL;
    }
  } else {
    s2_ffi_import->num_libraries = 0;
    s2_ffi_import->libraries = NULL;
  }

  /* load headers */
  headers = cwal_value_get_array(args->argv[0]);
  s2_ffi_import->num_headers = cwal_array_length_get(headers);
  if (s2_ffi_import->num_headers > 0) {
    /* copy and resolve filenames from s2 */
    s2_ffi_import->headers = malloc(sizeof(s2_ffi_import_header_type) * s2_ffi_import->num_headers);
    MARKER(("importing headers: %i (%" PRIxPTR ")\n", (int) s2_ffi_import->num_headers, (uintptr_t) s2_ffi_import->headers));
    for (i = 0; i < s2_ffi_import->num_headers; ++i) {
      const char *filename = cwal_value_get_cstr(cwal_array_get(headers, i), 0);
      size_t namelen = strlen(filename);
      char *find_file = s2_ffi_find_file(filename, namelen, s2_ffi_import_paths, s2_ffi_import_num_paths, NULL);
      if (find_file == NULL) {
        s2_ffi_import->headers[i].filename = malloc(namelen + 1);
        s2_ffi_import->headers[i].processed = FFI_IMPORT_FAILED_TO_FIND_FILE;
        strcpy(s2_ffi_import->headers[i].filename, filename);
      } else {
        s2_ffi_import->headers[i].filename = find_file;
        s2_ffi_import->headers[i].processed = FFI_IMPORT_IDLE;
      }
      s2_ffi_import->headers[i].content = NULL;
    }

    /* 1) process #include */
    for (i = 0; i < s2_ffi_import->num_headers; ++i) {
      /* skip previously processed */
      if (s2_ffi_import->headers[i].processed != FFI_IMPORT_IDLE) {
        MARKER(("skipping header: %s\n", s2_ffi_import->headers[i].filename));
        continue;
      }

      /* skip duplicates */
      k = s2_ffi_dedupe_header(s2_ffi_import, s2_ffi_import->headers[i].filename);
      if (k > -1) {
        s2_ffi_import->headers[i].processed = FFI_IMPORT_SKIPPED;
        MARKER(("skipping header: %s\n", s2_ffi_import->headers[i].filename));
        continue;
      }

      MARKER(("importing header: %s\n", s2_ffi_import->headers[i].filename));
      s2_ffi_import->headers[i].processed = FFI_IMPORT_IMPORTING;

      /* read the file */
      s2_ffi_import->headers[i].content = s2_ffi_read_file(s2_ffi_import->headers[i].filename);
      if (!s2_ffi_import->headers[i].content) {
        s2_ffi_import->headers[i].processed = FFI_IMPORT_FAILED_TO_READ_FILE;
        continue;
      }

      /* find #include matches */
      matches = s2_ffi_read_matches(&s2_ffi_regex_include, s2_ffi_import->headers[i].content, 0);
      if (matches.num_matches > 0) {
        char *parent_path = malloc(strlen(s2_ffi_import->headers[i].filename) + 1);
        strcpy(parent_path, s2_ffi_import->headers[i].filename);
        parent_path = dirname(parent_path);
        x = 0;
        for (j = 0; j < matches.num_matches; ++j) {
          char *find_file;

          /* insert into the list */
          ++s2_ffi_import->num_headers;
          s2_ffi_import->headers = realloc(s2_ffi_import->headers, sizeof(s2_ffi_import_header_type) * s2_ffi_import->num_headers);
          memmove(&s2_ffi_import->headers[i+x+1], &s2_ffi_import->headers[i+x], sizeof(s2_ffi_import_header_type) * (s2_ffi_import->num_headers-i-x-1));

          /* find the fully qualified filename */
          find_file = s2_ffi_find_file(matches.matches[j].match, matches.matches[j].matchlen, s2_ffi_import_paths, s2_ffi_import_num_paths, parent_path);
          if (find_file == NULL) {
            s2_ffi_import->headers[i+x+1].filename = matches.matches[j].match;
            s2_ffi_import->headers[i+x+1].processed = FFI_IMPORT_FAILED_TO_FIND_FILE;
          } else {
            free(matches.matches[j].match);
            s2_ffi_import->headers[i+x+1].filename = find_file;
            s2_ffi_import->headers[i+x+1].processed = FFI_IMPORT_IDLE;
          }
          s2_ffi_import->headers[i+x+1].content = NULL;

          ++x;
        }
        free(matches.matches);
        free(parent_path);
      }

      s2_ffi_import->headers[i].processed = FFI_IMPORT_IMPORTED;
    }

    /* 2) #define and typedef */
    for (i = 0; i < s2_ffi_import->num_headers; ++i) {
      /* skip unimported */
      if (s2_ffi_import->headers[i].processed != FFI_IMPORT_IMPORTED) {
        MARKER(("skipping: %s\n", s2_ffi_import->headers[i].filename));
        continue;
      }

      /* find #define matches */
      matches = s2_ffi_read_matches(&s2_ffi_regex_define, s2_ffi_import->headers[i].content, 0);
      if (matches.num_matches > 0) {
        cwal_value *val;

        /* determine definition type */
        for (j = 0; j < matches.num_matches - 1; j += 2) {
          if (matches.matches[j+1].matchlen == 0) {
            /* empty */
            val = cwal_value_null();
          } else if (matches.matches[j+1].matchlen > 2 && matches.matches[j+1].match[0] == '0' && (matches.matches[j+1].match[1] == 'x' || matches.matches[j+1].match[1] == 'X')) {
            /* hex */
            size_t ws = strcspn(matches.matches[j+1].match, " \t/");
            if (ws > 2) matches.matches[j+1].match[ws] = 0;
            val = cwal_new_integer(args->engine, (int) strtol(matches.matches[j+1].match + 2, NULL, 16));
          } else if (matches.matches[j+1].match[0] == '"') {
            /* quoted string */
            char *endquote = strchr(matches.matches[j+1].match+1, '"');
            if (endquote != NULL) {
              endquote[0] = 0;
              val = cwal_new_string_value(args->engine, matches.matches[j+1].match+1, endquote - matches.matches[j+1].match - 1);
            } else {
              /* huh... no matching quote? */
              val = cwal_new_string_value(args->engine, matches.matches[j+1].match, matches.matches[j+1].matchlen);
            }
          } else {
            /* number */
            char *ptr;
            double num = strtod(matches.matches[j+1].match, &ptr);
            if (ptr != NULL && ptr != matches.matches[j+1].match) {
              /* int or double */
              if (num == (cwal_int_t) num) {
                val = cwal_new_integer(args->engine, (cwal_int_t) num);
              } else {
                val = cwal_new_double(args->engine, num);
              }
            } else {
              /* last restort :: string :: rtrim */
              for (x = matches.matches[j+1].matchlen - 1; x > 0; --x) {
                if (matches.matches[j+1].match[x] == ' ' || matches.matches[j+1].match[x] == '\t') continue;
                matches.matches[j+1].match[x+1] = 0;
                break;
              }
              val = cwal_new_string_value(args->engine, matches.matches[j+1].match, matches.matches[j+1].matchlen);
            }
          }

          /* insert the #define */
          if (cwal_hash_insert(hash, matches.matches[j].match, matches.matches[j].matchlen, val, 0) != CWAL_RC_OK) {
            MARKER(("failed to insert #define (%s) into hash\n", matches.matches[j].match));
          }

          /* clean up! */
          free(matches.matches[j].match);
          free(matches.matches[j+1].match);
        }
        free(matches.matches);
      }

      /* find typedef matches */
      matches = s2_ffi_read_matches(&s2_ffi_regex_typedef, s2_ffi_import->headers[i].content, 0);
      if (matches.num_matches > 0) {
        for (j = 0; j < matches.num_matches - 1; j += 2) {
          /* move pointers back into definition */
          if (matches.matches[j+1].match[0] == '*') {
            /* add asterisks to definition */
            int num = (int) (strrchr(matches.matches[j+1].match, '*') - matches.matches[j+1].match) + 1;
            matches.matches[j].match = realloc(matches.matches[j].match, matches.matches[j].matchlen + num + 1);
            memcpy(matches.matches[j].match + matches.matches[j].matchlen, matches.matches[j+1].match, sizeof(char) * num);
            matches.matches[j].match[matches.matches[j].matchlen + num] = 0;
            matches.matches[j].matchlen = matches.matches[j].matchlen + num;

            /* remove asterisks from name */
            matches.matches[j+1].matchlen -= num;
            memmove(matches.matches[j+1].match, matches.matches[j+1].match + num, sizeof(char) * matches.matches[j+1].matchlen + 1);
          }

          /* determine definition type */
          if (cwal_hash_insert(hash, matches.matches[j+1].match, matches.matches[j+1].matchlen, cwal_new_string_value(args->engine, matches.matches[j].match, matches.matches[j].matchlen), 0) != CWAL_RC_OK) {
            MARKER(("failed to insert typedef (%s) into hash\n", matches.matches[j+1].match));
          }

          /* clean up! */
          free(matches.matches[j].match);
          free(matches.matches[j+1].match);
        }
        free(matches.matches);
      }
    }

    /* 3) hashmap of aliases */
    for (i = 0; i < s2_ffi_import->num_headers; ++i) {
      /* skip unimported */
      if (s2_ffi_import->headers[i].processed != FFI_IMPORT_IMPORTED) {
        MARKER(("skipping: %s\n", s2_ffi_import->headers[i].filename));
        continue;
      }

      /* and finally... the hardest part */
      matches = s2_ffi_read_matches(&s2_ffi_regex_proto, s2_ffi_import->headers[i].content, 0);
      if (matches.num_matches > 0) {
        for (j = 0; j < matches.num_matches - 2; j += 3) {
          uintptr_t func_handle = 0;
          char *word = NULL;
          s2_ffi_map_type *s2_ffi_map = NULL;
          char *ptr;

          /* guilty until proven innocent */
          rc = -1;

          /* shortcut */
          if (cwal_value_is_function(cwal_hash_search(hash, matches.matches[j+1].match, matches.matches[j+1].matchlen))) goto finished;

          /* 4) find the function handle */
          for (k = 0; k <= (int) s2_ffi_import->num_libraries; ++k) {
            func_handle =
#if S2_HAVE_LTDLOPEN
              (uintptr_t) lt_dlsym((k == (int) s2_ffi_import->num_libraries) ? s2_cb_ffi_handle : s2_ffi_import->libraries[i].handle, matches.matches[j+1].match);
#elif S2_HAVE_DLOPEN
              (uintptr_t) dlsym((k == (int) s2_ffi_import->num_libraries) ? s2_cb_ffi_handle : s2_ffi_import->libraries[k].handle, matches.matches[j+1].match);
#endif
          }
          if (!func_handle) goto finished;

          /* resolve return type */
          k = s2_ffi_resolve_type(matches.matches[j].match, hash);
          if (k == -1) goto finished;

          /* attempt to map the function */
          s2_ffi_map = malloc(sizeof(s2_ffi_map_type));
          s2_ffi_map->func_handle = func_handle;
          s2_ffi_map->rtype = k;
          s2_ffi_map->num_types = 0;
          s2_ffi_map->types = NULL;
          for (word = matches.matches[j+2].match; ; word = NULL) {
            if ((word = strtok_r(word, ",", &ptr)) == NULL) break;
            k = s2_ffi_resolve_type(word, hash);
            if (k == -1) {
              MARKER(("unresolved type: %s (%s %s %s)\n", word, matches.matches[j].match, matches.matches[j+1].match, matches.matches[j+2].match));
              goto finished;
            } else if (k == FFI_TYPE_VOID) {
              break;
            }
            ++s2_ffi_map->num_types;
            s2_ffi_map->types = realloc(s2_ffi_map->types, sizeof(cwal_int_t) * s2_ffi_map->num_types);
            s2_ffi_map->types[s2_ffi_map->num_types - 1] = k;
          }

          /* 5) set the mapped function */
          MARKER(("importing map: %i %s(%i) (%p :: %p)\n", (int) s2_ffi_map->rtype, matches.matches[j+1].match, (int) s2_ffi_map->num_types, (void *) s2_ffi_map, (void *) s2_ffi_map->types));
          if (cwal_hash_insert(hash, matches.matches[j+1].match, matches.matches[j+1].matchlen, cwal_new_function_value(args->engine, s2_cb_ffi_exec, (void *) s2_ffi_map, s2_cb_ffi_map_finalizer, &ffi_native_type), 1) != CWAL_RC_OK) {
            MARKER(("failed to insert function (%s) into hash\n", matches.matches[j+1].match));
            goto finished;
          }

          /* success! */
          rc = 0;

finished:
          /* clean up! */
          if (rc == -1 && s2_ffi_map != NULL) {
            if (s2_ffi_map->types != NULL) {
              MARKER(("cleaning map: %p (%p: %i args)\n", (void *) s2_ffi_map, (void *) s2_ffi_map->types, (int) s2_ffi_map->num_types));
              free(s2_ffi_map->types);
            } else {
              MARKER(("cleaning map: %p\n", (void *) s2_ffi_map));
            }
            free(s2_ffi_map);
          }
          free(matches.matches[j].match);
          free(matches.matches[j+1].match);
          free(matches.matches[j+2].match);
        }
        free(matches.matches);
      }
    }
  }

  return 0;

misuse:
  return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                             "Expecting (String|Array) or (String|Array, String|Array) "
                                 "arguments.");
}

#endif /* #if S2_ENABLE_FFI_IMPORT */

#endif /* #if S2_ENABLE_FFI */

cwal_value *s2_prototype_ffi(s2_engine *se){
#if S2_ENABLE_FFI
  int rc = 0;
  cwal_value *v;
  cwal_value *proto = cwal_new_object_value(se->e);
  MARKER(("ffi enabled\n"));
  if(!proto){
    rc = CWAL_RC_OOM;
    goto end;
  }
  if(!rc) rc = s2_prototype_stash(se, "ffi", proto);
  if(rc) goto end;

#define SET(NAME)                                               \
  if(!v) { rc = CWAL_RC_OOM; goto end; }                        \
  cwal_value_ref(v);                                            \
  rc = cwal_prop_set( proto, NAME, cwal_strlen(NAME), v );     \
  cwal_value_unref(v);                                          \
  v = 0;                                                        \
  if(rc) goto end

  MARKER(("setting up ffi type enum\n"));
  v = cwal_new_integer(se->e, (int) FFI_TYPE_VOID); SET("VOID");
  v = cwal_new_integer(se->e, (int) FFI_TYPE_CHAR); SET("CHAR");
  v = cwal_new_integer(se->e, (int) FFI_TYPE_SHORT); SET("SHORT");
  v = cwal_new_integer(se->e, (int) FFI_TYPE_INT); SET("INT");
  v = cwal_new_integer(se->e, (int) FFI_TYPE_LONG); SET("LONG");
  v = cwal_new_integer(se->e, (int) FFI_TYPE_FLOAT); SET("FLOAT");
  v = cwal_new_integer(se->e, (int) FFI_TYPE_DOUBLE); SET("DOUBLE");
  v = cwal_new_integer(se->e, (int) FFI_TYPE_CSTR); SET("CSTR");
  v = cwal_new_integer(se->e, (int) FFI_TYPE_PTR); SET("PTR");

#undef SET

  {
    s2_func_def const funcs[] = {
        S2_FUNC2("test", s2_cb_ffi_test),
        { "exec", s2_cb_ffi_exec, 0, s2_cb_ffi_finalizer, 0 },
        { "map", s2_cb_ffi_map, 0, s2_cb_ffi_finalizer, 0 },
#if S2_ENABLE_FFI_IMPORT
        { "import", s2_cb_ffi_import, 0, s2_cb_ffi_finalizer, 0 },
#endif
        s2_func_def_empty_m
    };
    MARKER(("installing ffi functions\n"));
    rc = s2_install_functions(se, proto, funcs, 0);

    MARKER(("rc: %i\n", rc));
  }

end:
  return rc ? NULL : proto;
#else
  if(se){/*avoid unused param warning*/}
  return NULL;
#endif
}

#undef MARKER
