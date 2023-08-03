/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#include "s2_internal.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#ifdef S2_OS_UNIX
/* for stat(2) */
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

#if 1
#include <stdio.h>
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__); if(1) printf pfexp
#else
#define MARKER(pfexp) (void)0
#endif

struct s2_pf {
  s2_engine * se;
  cwal_value * self;
  cwal_buffer buf;
    /* char (*predicate)( char const * fn ); */
};
static const s2_pf s2_pf_empty = {0,0,cwal_buffer_empty_m};

#define ARGS_SE s2_engine * se = s2_engine_from_args(args); \
    assert(se)

/**
   The "prefix" and "suffix" PathFinder properties are arrays of
   strings holding the search paths (prefixes) and extensions
   (suffixes). The names were, in hindsight, an unfortunate choice:
   "path" and "extensions" (or "ext") would have been more intuitive.
   Lots of my scripts use these names, though :/.
*/
#define PF_PREFIX "prefix"
#define PF_PREFIX_N ((cwal_size_t)sizeof(PF_PREFIX)-1)
#define PF_SUFFIX "suffix"
#define PF_SUFFIX_N ((cwal_size_t)sizeof(PF_SUFFIX)-1)

static void s2_pf_finalizer(cwal_engine * e, void * m){
  s2_pf * pf = (s2_pf*)m;
  cwal_buffer_reserve(e, &pf->buf, 0);
  cwal_free2( e, m, sizeof(s2_pf) );
}

s2_pf * s2_pf_new(s2_engine * se){
  s2_pf * pf;
  cwal_value * vSelf;
  if(!se || !se->e) return NULL;
  pf = (s2_pf*)cwal_malloc(se->e, sizeof(s2_pf));
  if(!pf) return NULL;
  *pf = s2_pf_empty;
  vSelf = cwal_new_native_value(se->e, pf, s2_pf_finalizer,
                                &s2_pf_empty);
  if(!vSelf){
    s2_pf_finalizer(se->e, pf);
    pf = NULL;
  }else{
    pf->self = vSelf;
    pf->se = se;
    assert(cwal_props_can(vSelf));
#if 0
    s2_pf_dirs(pf);
    s2_pf_exts(pf);
    assert(cwal_prop_get(vSelf,PF_SUFFIX,PF_SUFFIX_N));
    assert(cwal_prop_get(vSelf,PF_PREFIX,PF_PREFIX_N));
#endif
    cwal_value_prototype_set( vSelf, s2_prototype_pf(se) );
  }
  return pf;
}

int s2_install_pf( s2_engine * se, cwal_value * ns ){
  return cwal_props_can(ns)
    ?  cwal_prop_set(ns, "PathFinder", 10, s2_prototype_pf(se))
    : CWAL_RC_MISUSE;
}

static cwal_array * s2_pf_member_array(s2_pf *pf, char const * pKey, cwal_size_t pKeyLen ){
  cwal_value * ar = cwal_prop_get(pf->self,pKey, pKeyLen);
  if(!ar || !cwal_value_is_array(ar)){
    ar = cwal_new_array_value(pf->se->e);
    if(ar){
      int rc = 0;
      cwal_value_ref(ar);
      rc = cwal_prop_set(pf->self, pKey, pKeyLen, ar);
      cwal_value_unref(ar);
      if(rc!=0) ar = NULL;
    }
  }
  return cwal_value_get_array(ar);
}

cwal_array * s2_pf_exts(s2_pf *pf){
  return s2_pf_member_array(pf, PF_SUFFIX, PF_SUFFIX_N);
}

cwal_array * s2_pf_dirs(s2_pf *pf){
  return s2_pf_member_array(pf, PF_PREFIX, PF_PREFIX_N);
}

int s2_pf_dirs_set( s2_pf * pf, cwal_array * ar ){
  if(!pf || !pf->self || !ar) return CWAL_RC_MISUSE;
  else{
    return cwal_prop_set(pf->self, PF_PREFIX, PF_PREFIX_N,
                         cwal_array_value(ar));
  }
}

int s2_pf_exts_set( s2_pf * pf, cwal_array * ar ){
  if(!pf || !pf->self || !ar) return CWAL_RC_MISUSE;
  else{
    return cwal_prop_set(pf->self, PF_SUFFIX, PF_SUFFIX_N,
                         cwal_array_value(ar));
  }
}

int s2_pf_dir_add_v( s2_pf * pf, cwal_value * v ){
  if(!pf || !pf->self || !v) return CWAL_RC_MISUSE;
  else{
    cwal_array * ar = s2_pf_dirs(pf);
    return ar
      ? cwal_array_append(ar, v)
      : CWAL_RC_OOM;
  }
}

int s2_pf_dir_add( s2_pf * pf, char const * dir, cwal_size_t dirLen){
  if(!pf || !pf->self || !dir) return CWAL_RC_MISUSE;
  else{
    int rc;
    cwal_value * v = cwal_new_string_value(pf->se->e, dir, dirLen);
    if(!v) rc = CWAL_RC_OOM;
    else {
      cwal_value_ref(v);
      rc = s2_pf_dir_add_v( pf, v);
      cwal_value_unref(v);
    }
    return rc;
  }
}

int s2_pf_ext_add_v( s2_pf * pf, cwal_value * v ){
  if(!pf || !pf->self || !v) return CWAL_RC_MISUSE;
  else{
    cwal_array * ar = s2_pf_exts(pf);
    return ar
      ? cwal_array_append(ar, v)
      : CWAL_RC_OOM;
  }
}

int s2_pf_ext_add( s2_pf * pf, char const * dir, cwal_size_t dirLen){
  if(!pf || !pf->self || !dir) return CWAL_RC_MISUSE;
  else{
    int rc;
    cwal_value * v = cwal_new_string_value(pf->se->e, dir, dirLen);
    if(!v) rc = CWAL_RC_OOM;
    else {
      cwal_value_ref(v);
      rc = s2_pf_ext_add_v( pf, v);
      cwal_value_unref(v);
    }
    return rc;
  }
}

s2_pf * s2_value_pf_part(cwal_value const *v){
  cwal_native * n;
  s2_pf * pf = NULL;
  while(v){
    n = cwal_value_get_native(v);
    if(n){
      pf = (s2_pf *)cwal_native_get(n, &s2_pf_empty);
      if(pf) break;
    }
    v = cwal_value_prototype_get(NULL,v);
  }
  return pf;
}

s2_pf * s2_value_pf(cwal_value const * v){
  cwal_native const * n = v ? cwal_value_get_native(v) : 0;
  return n
    ? (s2_pf *)cwal_native_get(n, &s2_pf_empty)
    : 0;
}

cwal_value * s2_pf_value(s2_pf const * pf){
  return pf->self;
}

#define THIS_PF \
  s2_pf * pf = s2_value_pf_part(args->self);                        \
  if(!pf) return                                                    \
            cwal_exception_setf(args->engine, CWAL_RC_TYPE,          \
                                "'this' is not a PathFinder instance.")

static int s2_cb_pf_add( cwal_callback_args const * args, cwal_value **rv,
                         cwal_array * dest ){
  THIS_PF;
  if(!args->argc){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting string arguments.");
  }
  else if(!dest) return CWAL_RC_OOM;
  else {
    cwal_size_t i = 0;
    int rc = 0;
    for( ; !rc && (i < args->argc); ++i ){
      rc = cwal_array_append( dest, args->argv[i]);
    }
    if(!rc) *rv = args->self;
    return rc;
  }
}


static int s2_cb_pf_dir_add( cwal_callback_args const * args, cwal_value **rv ){
  THIS_PF;
  return s2_cb_pf_add( args, rv, s2_pf_dirs(pf) );
}

static int s2_cb_pf_ext_add( cwal_callback_args const * args, cwal_value **rv ){
  THIS_PF;
  return s2_cb_pf_add( args, rv, s2_pf_exts(pf) );
}


static void s2_pf_separator( s2_pf * pf, char const ** sep, cwal_size_t * sepLen ){
  cwal_value const * vs = cwal_prop_get(pf->self,"separator",9);
  char const * rc = vs ? cwal_value_get_cstr(vs, sepLen) : NULL;
  assert(sep && sepLen);
  if(!rc){
    if(vs){ /* Non-string separator value. FIXME: we _could_ use a
               non-string with just a little more work but there's
               currently no use case for it. If PF is abstracted
               to do other types of searches (where the user
               supplies a predicate function we call for each path
               combination we try) then it will make sense to
               allow non-string separators. For now we're only
               dealing with file paths, which means strings.
            */
      *sep = "";
      *sepLen = 0;
    }
    else{
      rc = S2_DIRECTORY_SEPARATOR;
      *sepLen = sizeof(S2_DIRECTORY_SEPARATOR)-
        sizeof(S2_DIRECTORY_SEPARATOR[0]);
    }
  }
  *sep = rc;
}


char const * s2_pf_search( s2_pf * pf, char const * base,
                           cwal_size_t baseLen, cwal_size_t * rcLen,
                           int directoryPolicy){
  char const * pathSep = NULL;
  cwal_size_t sepLen;
  cwal_buffer * buf;
  cwal_engine * e;
  cwal_size_t d, x, nD, nX, resetLen = 0;
  cwal_array * ad;
  cwal_array * ax;
  int rc = 0;
  if(!pf || !base) return NULL;
  else if(!*base || !baseLen) return NULL;
  buf = &pf->buf;
  buf->used = 0;
  e = pf->se->e;
  
#define CHECK_FILE(NAME) (s2_file_is_accessible(NAME, 0) &&             \
                          ((directoryPolicy < 0 \
                            ? s2_is_dir(NAME,0)     \
                            : (directoryPolicy > 0 \
                               ? 1 \
                               : !s2_is_dir(NAME,0)))))
  if(CHECK_FILE(base)){
    rc = cwal_buffer_append(e, buf, base, baseLen);
    if(rc) return NULL;
    goto gotone;
  }

  s2_pf_separator(pf, &pathSep, &sepLen);
  assert(pathSep);
  assert(sepLen);
  ad = s2_pf_dirs(pf);
  ax = s2_pf_exts(pf);
  nD = cwal_array_length_get(ad);
  nX = cwal_array_length_get(ax);
  for( d = 0; !rc && (nD ? d < nD : 1); ){
    cwal_value * vD = nD
      ? cwal_array_get(ad, d)
      : 0;
    buf->used = 0;
    if(nD && vD){
      cwal_size_t const used = buf->used;
      rc = s2_value_to_buffer(e, buf, vD);
      if(rc) break;
      else if(used != buf->used){
        /* Only append separator if vD is non-empty. */
        rc = cwal_buffer_append(e, buf, pathSep, sepLen);
        if(rc) break;
      }
    }
    rc = cwal_buffer_append(e, buf, base, baseLen);
    if(rc) break;
    if(CHECK_FILE( (char const *)buf->mem )){
      goto gotone;
    }
    resetLen = buf->used;
    for( x = 0; !rc && (x < nX); ++x ){
      cwal_value * vX = cwal_array_get(ax, x);
      if(vX){
        buf->used = resetLen;
        rc = s2_value_to_buffer(e, buf, vX);
        if(rc) break;
      }
      assert(buf->used < buf->capacity);
      buf->mem[buf->used] = 0;
      if(CHECK_FILE((char const *)buf->mem)){
        goto gotone;
      }
    }
    if(++d >= nD) break;
  }
#undef CHECK_FILE
  return NULL;
  gotone:
  if(rcLen) *rcLen = buf->used;
  return (char const *)buf->mem;
}

/**
   Script signature:

   string|undefined search(string baseName [, bool|int dirPolicy=0])

   dirPolicy: bool true or integer>0 mean match files and dirs, false or
   integer 0 mean only files, integer<0 means only match dirs. If this value
   is of the Unique type, its wrapped value is used in its place.

   Returns the first matching file/dir entry, or the undefined value
   if no entry is found.
*/
static int s2_cb_pf_search( cwal_callback_args const * args, cwal_value **rv ){
  cwal_size_t baseLen = 0;
  char const * base;
  char const * rc;
  cwal_size_t rcLen = 0;
  int directoryPolicy = 0;
  THIS_PF;
  {
    int const rc = s2_cb_disable_check(args, S2_DISABLE_FS_STAT);
    if( rc ) return rc;
  }

  if(!args->argc) goto misuse;
  base = cwal_value_get_cstr(args->argv[0], &baseLen);
  if(!base || !baseLen) goto misuse;
  else if(args->argc>1){
    /* dirPolicy: bool|integer directoryPolicy */
    cwal_value const * a1 = s2_value_unwrap(args->argv[1]);
    if(cwal_value_is_bool(a1)){
      directoryPolicy = cwal_value_get_bool(a1)
        ? S2_PF_SEARCH_FILES_DIRS
        : S2_PF_SEARCH_FILES;
    }else{
      cwal_int_t const n = cwal_value_get_integer(a1);
      directoryPolicy = n==0
        ? S2_PF_SEARCH_FILES
        : (n<0
           ? S2_PF_SEARCH_DIRS
           : S2_PF_SEARCH_FILES_DIRS);
    }
  }
  rc = s2_pf_search( pf, base, baseLen, &rcLen, directoryPolicy );
  if(!rc) *rv = cwal_value_undefined();
  else {
    *rv = cwal_new_string_value(args->engine, rc, rcLen);
  }
  return *rv ? 0 : CWAL_RC_OOM;
  misuse:
  return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                             "Expecting a non-empty string argument.");
}

int s2_cb_pf_new( cwal_callback_args const * args, cwal_value **rv ){
  /* Constructor... */
  int rc = 0;
  s2_pf * pf;
  ARGS_SE;
  pf = s2_pf_new(se);
  if(!pf) return CWAL_RC_OOM;
  else if(args->argc){
    /* Set or reserve the PF_PREFIX/PF_SUFFIX properties... */
    uint16_t i;
    typedef int (*setter_f)(s2_pf *, cwal_array *);
    typedef cwal_array * (*getter_f)(s2_pf *);
    for(i = 0; i<args->argc && i<2; ++i){
      cwal_value * const arg = args->argv[i];
      setter_f setter = i ? s2_pf_exts_set : s2_pf_dirs_set;
      getter_f getter = i ? s2_pf_exts : s2_pf_dirs;
      cwal_array * ar;
      if(cwal_value_undefined()==arg || cwal_value_null()==arg){
        /* Special case: skip these without an error to simplify
           certain usage patterns, e.g.: new
           s2.PathFinder(s2.getenv("blah")). */
        continue;
      }
      ar = cwal_value_array_part(args->engine, arg);
      if(ar){
        /* Array argument: use it as-is */
        rc = setter(pf, ar);
      }else{
        if(cwal_value_is_string(arg)){
          /* Parse string as a PATH. */
          cwal_size_t plen;
          char const * pstr = cwal_value_get_cstr(arg, &plen);
          cwal_array * tgt = getter(pf);
          if(!tgt){
            rc = CWAL_RC_OOM;
          }else{
            assert(pstr);
            rc = s2_tokenize_path_to_array(args->engine, &tgt,
                                           pstr, (cwal_int_t)plen)
              /* The only plausible error case here is an OOM, so we
                 don't translate the result to an exception. */;
          }
        }else{
          /* Lightly slap the user's fingers. */
          rc = cwal_cb_throw(args, CWAL_RC_TYPE,
                           "Expecting 0, 1, or 2 string or array arguments.");
        }
      }
      if(rc) break;
    }
  }
  if(!rc) *rv = pf->self;
  else cwal_value_unref(pf->self) /* takes pf with it */;
  return rc;
}

cwal_value * s2_prototype_pf( s2_engine * se ){
  int rc = 0;
  cwal_value * v;
  cwal_value * proto;
  char const * pKey = "class.PathFinder";
  assert(se && se->e);
  proto = s2_prototype_stashed(se, pKey);
  if(proto) return proto;
  proto = cwal_new_object_value(se->e);
  if(!proto){
    rc = CWAL_RC_OOM;
    goto end;
  }
  rc = s2_prototype_stash( se, pKey, proto );
  if(rc) goto end;

#define CHECKV if(!v){ rc = CWAL_RC_OOM; goto end; } (void)0
#define SET(NAME)                                              \
  CHECKV;                                                      \
  cwal_value_ref(v);                                           \
  rc = cwal_prop_set( proto, NAME, cwal_strlen(NAME), v );    \
  cwal_value_unref(v);                                         \
  v = 0;                                                       \
  if(rc) goto end

  v = cwal_new_string_value(se->e, "PathFinder", 10);
  CHECKV;
  cwal_value_ref(v);
  rc = cwal_prop_set_with_flags_v( proto, se->cache.keyTypename,
                                   v, CWAL_VAR_F_HIDDEN );
  cwal_value_unref(v);
  v = 0;
  if(rc) goto end;

  v = cwal_new_xstring_value(se->e,
#ifdef _WIN32
                             "\\",
#else
                             "/",
#endif
                             1);
  SET("separator");

  {
    s2_func_def const funcs[] = {
      S2_FUNC2("addDir", s2_cb_pf_dir_add),
      S2_FUNC2("addExt", s2_cb_pf_ext_add),
      S2_FUNC2("search", s2_cb_pf_search),
      S2_FUNC2("fileIsAccessible", s2_cb_file_accessible),
      S2_FUNC2("new", s2_cb_pf_new),
      S2_FUNC2("tokenizePath", s2_cb_tokenize_path),
      s2_func_def_empty_m
    };
    rc = s2_install_functions(se, proto, funcs, 0);
    if(rc) goto end;
    else {
      cwal_value * fv = 0;
      s2_get(se, proto, "new", 3, &fv);
      assert(fv && "we JUST put this in there!");
      rc = s2_ctor_method_set( se, proto,
                               cwal_value_get_function(fv) );
    }
  }

#undef SET
#undef CHECKV
    end:
    return rc
      ? NULL /* remember: proto is stashed at this point, so no leak. */
      : proto;
}

#undef ARGS_SE
#undef THIS_PF
#undef MARKER

#undef PF_PREFIX
#undef PF_PREFIX_N
#undef PF_SUFFIX
#undef PF_SUFFIX_N
