/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

/**
   Impls for the pathfinder class.
*/

#include "internal.h"
#include <stdio.h>
#include <assert.h>

#if 1
#define MARKER(pfexpr) if(1) printf("MARKER: %s:%d:\t",__FILE__,__LINE__); if(1) printf pfexpr
#else
#define MARKER if(0) printf
#endif


struct whcl_pf {
  whcl_engine * el;
  cwal_value * self;
  cwal_buffer buf;
    /* char (*predicate)( char const * fn ); */
};
static const whcl_pf whcl_pf_empty = {0,0,cwal_buffer_empty_m};

/**
   The property names used for PathFinder path/extension members.
*/
#define PF_PREFIX "path"
#define PF_PREFIX_N ((cwal_size_t)sizeof(PF_PREFIX)-1)
#define PF_SUFFIX "extensions"
#define PF_SUFFIX_N ((cwal_size_t)sizeof(PF_SUFFIX)-1)

static void whcl_pf_finalizer(cwal_engine * e, void * m){
  whcl_pf * pf = (whcl_pf*)m;
  cwal_buffer_reserve(e, &pf->buf, 0);
  cwal_free2( e, m, sizeof(whcl_pf) );
}

whcl_pf * whcl_pf_new(whcl_engine * const el){
  whcl_pf * pf;
  cwal_value * vSelf;
  pf = (whcl_pf*)cwal_malloc(el->ec, sizeof(whcl_pf));
  if(!pf){ WHCL__WARN_OOM; return NULL; }
  *pf = whcl_pf_empty;
  vSelf = cwal_new_native_value(el->ec, pf, whcl_pf_finalizer,
                                &whcl_pf_empty);
  if(!vSelf){
    whcl_pf_finalizer(el->ec, pf);
    pf = NULL;
  }else{
    pf->self = vSelf;
    pf->el = el;
    assert(cwal_props_can(vSelf));
    whcl_pf_dirs(pf);
    whcl_pf_exts(pf);
#if 0
    assert(cwal_prop_get(vSelf,PF_SUFFIX,PF_SUFFIX_N));
    assert(cwal_prop_get(vSelf,PF_PREFIX,PF_PREFIX_N));
#endif
    cwal_value * proto = NULL;
    whcl_prototype_pf(el, &proto);
    assert(proto && "Should have been initialized by now.");
    cwal_value_prototype_set(vSelf, proto);
    assert(proto == cwal_value_prototype_get( el->ec, vSelf ));
  }
  return pf;
}


static cwal_array * whcl_pf_member_array(whcl_pf * const pf,
                                         char const * pKey,
                                         cwal_size_t pKeyLen ){
  cwal_value * ar = cwal_prop_get(pf->self,pKey, pKeyLen);
  if(!ar || !cwal_value_is_array(ar)){
    ar = cwal_new_array_value(pf->el->ec);
    if(ar){
      int rc = 0;
      cwal_ref(ar);
      rc = cwal_prop_set(pf->self, pKey, pKeyLen, ar);
      cwal_unref(ar);
      if(rc!=0) ar = NULL;
    }
  }
  return cwal_value_get_array(ar);
}

cwal_array * whcl_pf_exts(whcl_pf * const pf){
  return whcl_pf_member_array(pf, PF_SUFFIX, PF_SUFFIX_N);
}

cwal_array * whcl_pf_dirs(whcl_pf * const pf){
  return whcl_pf_member_array(pf, PF_PREFIX, PF_PREFIX_N);
}

int whcl_pf_dirs_set( whcl_pf * const pf, cwal_array * const ar ){
  if(!ar) return CWAL_RC_MISUSE;
  else return cwal_prop_set(pf->self, PF_PREFIX, PF_PREFIX_N,
                            cwal_array_value(ar));
}

int whcl_pf_exts_set( whcl_pf * const pf, cwal_array * const ar ){
  if(!ar) return CWAL_RC_MISUSE;
  else return cwal_prop_set(pf->self, PF_SUFFIX, PF_SUFFIX_N,
                            cwal_array_value(ar));
}

static int whcl_pf_xxx_add( whcl_pf * const pf, char const * name, cwal_int_t nameLen,
                            int (*adder)(whcl_pf * const, cwal_value * const) ){
  if(!name) return CWAL_RC_MISUSE;
  else{
    int rc;
    if(nameLen<0) nameLen = (cwal_int_t)cwal_strlen(name);
    cwal_value * const v = cwal_new_string_value(pf->el->ec, name,
                                                 (cwal_size_t)nameLen);
    if(!v) rc = CWAL_RC_OOM;
    else {
      cwal_ref(v);
      rc = adder(pf, v);
      cwal_unref(v);
    }
    return rc;
  }
}

int whcl_pf_dir_add( whcl_pf * const pf, char const * name, cwal_int_t nameLen){
  return whcl_pf_xxx_add(pf, name, nameLen, whcl_pf_dir_add_v );
}

int whcl_pf_ext_add( whcl_pf * const pf, char const * name, cwal_int_t nameLen){
  return whcl_pf_xxx_add(pf, name, nameLen, whcl_pf_ext_add_v );
}

int whcl_pf_dir_add_v( whcl_pf * const pf, cwal_value * const v ){
  if(!v) return CWAL_RC_MISUSE;
  else{
    cwal_array * const ar = whcl_pf_dirs(pf);
    return ar
      ? cwal_array_append(ar, v)
      : CWAL_RC_OOM;
  }
}

int whcl_pf_ext_add_v( whcl_pf * const pf, cwal_value * const v ){
  if(!v) return CWAL_RC_MISUSE;
  else{
    cwal_array * const ar = whcl_pf_exts(pf);
    return ar
      ? cwal_array_append(ar, v)
      : CWAL_RC_OOM;
  }
}

whcl_pf * whcl_value_pf_part(cwal_value const * v){
  cwal_native * n;
  whcl_pf * pf = NULL;
  while(v){
    n = cwal_value_get_native(v);
    if(n){
      pf = (whcl_pf *)cwal_native_get(n, &whcl_pf_empty);
      if(pf) break;
    }
    v = cwal_value_prototype_get(NULL,v);
  }
  return pf;
}

whcl_pf * whcl_value_pf(cwal_value const * const v){
  cwal_native const * n = v ? cwal_value_get_native(v) : 0;
  return n
    ? (whcl_pf *)cwal_native_get(n, &whcl_pf_empty)
    : 0;
}

cwal_value * whcl_pf_value(whcl_pf const * const pf){
  return pf->self;
}


#define WHCL__E_ARGS \
  whcl_engine * const el = whcl_engine_from_args(args); assert(el)

#define THIS_PF                                                     \
  whcl_pf * const pf = whcl_value_pf_part(args->self);              \
  if(!pf) return                                                    \
            cwal_exception_setf(args->engine, CWAL_RC_TYPE,          \
                                "'this' is not a PathFinder instance.")

static int whcl__cb_pf_add( cwal_callback_args const * args, cwal_value **rv,
                            cwal_array * const dest ){
  THIS_PF;
  if(!args->argc){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting string arguments.");
  }
  else if(!dest) return CWAL_RC_OOM;
  else {
    uint16_t i = 0;
    int rc = 0;
    for( ; !rc && (i < args->argc); ++i ){
      rc = cwal_array_append(dest, args->argv[i]);
    }
    if(!rc) *rv = args->self;
    return rc;
  }
}


static int whcl__cb_pf_dir_add( cwal_callback_args const * args, cwal_value **rv ){
  THIS_PF;
  return whcl__cb_pf_add( args, rv, whcl_pf_dirs(pf) );
}

static int whcl__cb_pf_ext_add( cwal_callback_args const * args, cwal_value **rv ){
  THIS_PF;
  return whcl__cb_pf_add( args, rv, whcl_pf_exts(pf) );
}

static void whcl_pf_separator( whcl_pf * pf, char const ** sep, cwal_size_t * sepLen ){
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
      rc = WHCL_DIRECTORY_SEPARATOR;
      *sepLen = sizeof(WHCL_DIRECTORY_SEPARATOR)-
        sizeof(WHCL_DIRECTORY_SEPARATOR[0]);
    }
  }
  *sep = rc;
}

char const * whcl_pf_search( whcl_pf * const pf, char const * base,
                             cwal_int_t baseLen,
                             cwal_size_t * const rcLen,
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
  e = pf->el->ec;
  if(baseLen<0) baseLen = (cwal_int_t)cwal_strlen(base);
#define CHECK_FILE(NAME) (whcl_file_is_accessible(NAME, 0) &&             \
                          ((directoryPolicy < 0 \
                            ? whcl_is_dir(NAME,0)     \
                            : (directoryPolicy > 0 \
                               ? 1 \
                               : !whcl_is_dir(NAME,0)))))
  if(CHECK_FILE(base)){
    rc = cwal_buffer_append(e, buf, base, (cwal_size_t)baseLen);
    if(rc) return NULL;
    goto gotone;
  }

  whcl_pf_separator(pf, &pathSep, &sepLen);
  assert(pathSep);
  assert(sepLen);
  ad = whcl_pf_dirs(pf);
  ax = whcl_pf_exts(pf);
  nD = cwal_array_length_get(ad);
  nX = cwal_array_length_get(ax);
  for( d = 0; !rc && (nD ? d < nD : 1); ){
    cwal_value * vD = nD
      ? cwal_array_get(ad, d)
      : 0;
    buf->used = 0;
    if(nD && vD){
      cwal_size_t const used = buf->used;
      rc = whcl_value_to_buffer(pf->el, buf, vD);
      if(rc) break;
      else if(used != buf->used){
        /* Only append separator if vD is non-empty. */
        rc = cwal_buffer_append(e, buf, pathSep, sepLen);
        if(rc) break;
      }
    }
    rc = cwal_buffer_append(e, buf, base, (cwal_size_t)baseLen);
    if(rc) break;
    if(CHECK_FILE( (char const *)buf->mem )){
      goto gotone;
    }
    resetLen = buf->used;
    for( x = 0; !rc && (x < nX); ++x ){
      cwal_value * vX = cwal_array_get(ax, x);
      if(vX){
        buf->used = resetLen;
        rc = whcl_value_to_buffer(pf->el, buf, vX);
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
static int whcl__cb_pf_search( cwal_callback_args const * args, cwal_value **rv ){
  cwal_size_t baseLen = 0;
  char const * base;
  char const * rc;
  cwal_size_t rcLen = 0;
  int directoryPolicy = 0;
  THIS_PF;

  if(!args->argc) goto misuse;
  base = cwal_value_get_cstr(args->argv[0], &baseLen);
  if(!base || !baseLen) goto misuse;
  else if(args->argc>1){
    /* dirPolicy: bool|integer directoryPolicy */
    cwal_value const * a1 = whcl_value_unwrap(args->argv[1]);
    if(cwal_value_is_bool(a1)){
      directoryPolicy = cwal_value_get_bool(a1)
        ? WHCL_PF_SEARCH_FILES_DIRS
        : WHCL_PF_SEARCH_FILES;
    }else{
      cwal_int_t const n = cwal_value_get_integer(a1);
      directoryPolicy = n==0
        ? WHCL_PF_SEARCH_FILES
        : (n<0
           ? WHCL_PF_SEARCH_DIRS
           : WHCL_PF_SEARCH_FILES_DIRS);
    }
  }
  rc = whcl_pf_search( pf, base, (cwal_int_t)baseLen,
                       &rcLen, directoryPolicy );
  if(!rc) *rv = cwal_value_undefined();
  else {
    *rv = cwal_new_string_value(args->engine, rc, rcLen);
  }
  return *rv ? 0 : CWAL_RC_OOM;
  misuse:
  return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                             "Expecting a non-empty string argument.");
}

static int whcl__cb_pf_to_jsonable( cwal_callback_args const * args,
                                    cwal_value ** rv ){
  THIS_PF;
  int rc = CWAL_RC_OOM;
  cwal_value * const o = cwal_new_object_value(args->engine);
  if(!o) goto end;
  cwal_value * v = cwal_prop_get(args->self, PF_PREFIX, PF_PREFIX_N);
  if(v){
    cwal_ref(v);
    rc = cwal_prop_set(o, PF_PREFIX, PF_PREFIX_N, v);
    cwal_unref(v);
    if(rc) goto end;
  }else{
    rc = CWAL_RC_OOM; goto end;
  }

  v = cwal_prop_get(args->self, PF_SUFFIX, PF_SUFFIX_N);
  if(v){
    cwal_ref(v);
    rc = cwal_prop_set(o, PF_SUFFIX, PF_SUFFIX_N, v);
    cwal_unref(v);
    if(rc) goto end;
  }else{
    rc = CWAL_RC_OOM;
  }
  end:
  if(rc){
    if(CWAL_RC_OOM==rc){WHCL__WARN_OOM;}
    if(o) cwal_refunref(o);
  }else{
    *rv = o;
  }
  return rc;
}

/**
   A cwal_callback_f() implementing a constructor of PathFinder (whcl_pf)
   instances. On success, assigns the new instance to *rv.

   Requires that whcl_engine_from_args() returns non-NULL.

   Script usage:

   var pf = ThisFunction()

   it optionally takes up to two array arguments for the
   directory/extension lists, respectively.
*/
int whcl__cb_pf_ctor( cwal_callback_args const * args, cwal_value **rv ){
  int rc = 0;
  whcl_pf * pf;
  WHCL__E_ARGS;
  pf = whcl_pf_new(el);
  if(!pf) return CWAL_RC_OOM;
  else if(args->argc){
    /* Set or reserve the PF_PREFIX/PF_SUFFIX properties... */
    uint16_t i;
    typedef int (*setter_f)(whcl_pf *, cwal_array *);
    typedef cwal_array * (*getter_f)(whcl_pf *);
    for(i = 0; i<args->argc && i<2; ++i){
      cwal_value * const arg = args->argv[i];
      setter_f setter = i ? whcl_pf_exts_set : whcl_pf_dirs_set;
      getter_f getter = i ? whcl_pf_exts : whcl_pf_dirs;
      cwal_array * ar;
      if(cwal_value_undefined()==arg || cwal_value_null()==arg){
        /* Special case: skip these without an error to simplify
           certain usage patterns, e.g.:
           new PathFinder(getenv("blah")). */
        continue;
      }
      ar = cwal_value_array_part(args->engine, arg);
      if(ar){
        /* Array argument: use it as-is */
        rc = setter(pf, ar);
      }else if(cwal_value_is_string(arg)){
        /* Parse string as a PATH. */
        cwal_size_t plen;
        char const * pstr = cwal_value_get_cstr(arg, &plen);
        cwal_array * tgt = getter(pf);
        if(!tgt){
          rc = CWAL_RC_OOM;
        }else{
          assert(pstr);
          rc = whcl_tokenize_path_to_array(args->engine, &tgt,
                                           pstr, (cwal_int_t)plen)
            /* The only plausible error case here is an OOM, so we
               don't translate the result to an exception. */;
        }
      }else{
        /* Lightly slap the user's fingers. */
        rc = cwal_cb_throw(args, CWAL_RC_TYPE,
                           "Expecting 0, 1, or 2 string or array arguments.");
      }
      if(rc) break;
    }
  }
  if(!rc) *rv = pf->self;
  else cwal_value_unref(pf->self) /* takes pf with it */;
  return rc;
}

static const char * pfKlassName = "PathFinder";

int whcl_prototype_pf(whcl_engine * const el, cwal_value ** rv){
  int rc = 0;
  cwal_value * ctor;
  cwal_value * proto;
  proto = whcl__prototype_stashed( el, pfKlassName );
  if(proto){
    if(rv) *rv = proto;
    return 0;
  }else if(!whcl_prototype_object(el) /* timing hack */){
    return CWAL_RC_OOM;
  }
  ctor = cwal_new_function_value(el->ec, whcl__cb_pf_ctor, NULL,
                                 NULL, NULL);
  proto = ctor ? cwal_new_object_value(el->ec) : NULL;
  if(!ctor){
    WHCL__WARN_OOM;
    rc = CWAL_RC_OOM;
    goto end;
  }
  cwal_value_prototype_set(ctor, proto);
  cwal_ref(ctor);
  rc = whcl__install_into_whcl(el, pfKlassName, ctor);
  cwal_unref(ctor);
  if(0==rc) rc = whcl__prototype_stash(el, pfKlassName, proto);
  if(rc) goto end;
  {
    whcl_func_def const funcs[] = {
      WHCL_FUNC2("add-dir", whcl__cb_pf_dir_add),
      WHCL_FUNC2("add-ext", whcl__cb_pf_ext_add),
      WHCL_FUNC2("is-accessible", whcl_cb_file_accessible),
      WHCL_FUNC2("search", whcl__cb_pf_search),
      WHCL_FUNC2("to-jsonable", whcl__cb_pf_to_jsonable),
      WHCL_FUNC2("tokenize-path", whcl_cb_tokenize_path),
      whcl_func_def_empty_m
    };
    rc = whcl_install_functions(el, proto, funcs, 0);
    if(rc) goto end;
  }
  /** This script requires that the os bits and ARGV be
      installed or that we install getenv() again just for this
      purpose. */
  if(0==rc){
    /* PathFinder[path-from-env] method. */
    char const * src =
#if 1
      "whcl install-api os\n"
#else
      "if {!whcl[os] || !whcl[os][getenv]} "
      "{throw 'Install whcl[os] APIs before PathFinder!'}\n"
#endif
      "if {!whcl[ARGV]} {throw 'Install whcl[ARGV] before PathFinder!'}\n"
      "set -const X[path-from-env] proc {f {e $flag}} {\n"
      "decl p F[$f]\n"
      "if {!$p} {set p [E $e]}\n"
      "if {[info is-string $p]} {return [P tokenize-path $p]}\n"
      "} using -scope {\n"
      "decl -const E whcl[os][getenv]\n"
      "decl -const P $X\n"
      "decl -const F whcl[ARGV][flags]\n"
      "}\n";
    rc = whcl_eval_cstr_with_var(el, "X", proto, "PathFinder API init",
                                 src, -1, NULL);
    if(0==rc){
      src =
        "set -const X.SEARCH_ANY 1;"
        "set -const X.SEARCH_FILES 0;"
        "set -const X.SEARCH_DIRS -1";
      rc = whcl_eval_cstr_with_var(el, "X", proto,
                                   "PathFinder API init 2",
                                   src, -1, NULL);
    }
    if(rc && CWAL_RC_OOM!=rc){
      MARKER(("Internal error: init of PathFinder code failed w/ %s.",
              cwal_rc_cstr(rc)));
      cwal_value * const ex = cwal_exception_get(el->ec);
      if(ex){
        whcl__dump_val(ex, "PathFinder init exception");
      }
    }
  }
  end:
  if(0==rc && rv) *rv = proto;
  return rc;
}

int whcl__install_pf( whcl_engine * const el ){
  if(el->cache.installAPI & WHCL__INSTALL_API_pf) return 0;
  int rc = whcl__install_os(el)/*for os[getenv]*/;
  if(0==rc){
    rc = whcl_prototype_pf(el, NULL);
    if(0==rc) el->cache.installAPI |= WHCL__INSTALL_API_pf;
  }
  return rc;
}

#undef MARKER
#undef WHCL__E_ARGS
#undef PF_PREFIX
#undef PF_PREFIX_N
#undef PF_SUFFIX
#undef PF_SUFFIX_N
