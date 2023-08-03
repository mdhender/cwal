/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#include "libs2.h"
/*
  This file implements a s2 binding for opendir(3) and friends.
*/
#include <assert.h>
#include <memory.h> /* strlen(), strerror() */
#include <stdlib.h> /* free() */

#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#include <unistd.h>

/* only for debuggering */
#include <stdio.h>
#define MARKER(pfexp)                                               \
  do{ printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__);   \
    printf pfexp;                                                   \
  } while(0)
#define ARGS_SE s2_engine * se = s2_engine_from_args(args); \
  assert(se)

#if 0
struct DirentMod {
  cwal_value * x;
};
typedef struct DirentMod DirentMod;
static const DirentMod_empty = {
0
};

static int cwal_value_rescoper_f_DirentMod( cwal_scope * s, cwal_value * v ){
  cwal_native * n = cwal_value_native_part( s->e, v, &DirentMod_empty );
  DirentMod * dn = (DirentMod *)cwal_native_get( n, &DirentMod_empty );
  assert(n);
  assert(dn);
  /* MARKER(("Rescoping db@%p\n", (void*)v)); */
  if(dn->proto){
    rc = cwal_value_rescope(s, dn->proto);
  }
  return rc;
}

static void cwal_finalizer_f_DirentMod( cwal_engine * e, void * m ){
  if(m){
    DirentMod * d = (DirentMod *)m;
    cwal_value_unref(d->proto);
    *d = DirentMod_empty;
    cwal_free2(e, sizeof(DirentMod));
  }
}
#endif /* DirentMod */

static const int direntish_typeid = 42;
static const int direntish_prototype_id = 43;
#define S2X_DIRENT_BUFSIZE 4096
#define THIS_DIRENT                                                 \
  cwal_native * nself = cwal_value_get_native(args->self);          \
  DIR * self = nself                                                \
    ? (DIR *)cwal_native_get(nself, &direntish_typeid) : NULL;      \
  if(!self) return cwal_cb_throw(args, CWAL_RC_TYPE,                  \
                               "'this' is not (or is no longer) "   \
                               "a direntish instance.")

static void direntish_closedir_finalizer( cwal_engine * e, void * v ){
  /* MARKER(("closing dir @%p\n", v)); */
  if(e){/*avoid unused param warning*/}
  if(v) closedir( (DIR*)v );
}

static int direntish_cb_close( cwal_callback_args const * args,
                               cwal_value **rv ){
  THIS_DIRENT;
  cwal_native_clear( nself, 1 );
  *rv = cwal_value_undefined();
  return 0;
}

static char direntish_is_dir( cwal_engine * e, char const * fn, int * theRc ){
  s2_fstat_t st = s2_fstat_t_empty;
  int rc = s2_fstat( fn, cwal_strlen(fn), &st, 1 );
  if(rc){
    /* Throwing here is arguable. We should probably just return 0 in
       that case. This triggers for me when emacs leaves stale symlink
       locks which point to non-existent files.
    */
    *theRc = cwal_exception_setf(e, rc, "s2_fstat(\"%s\") failed.", fn);
    return 0;
  }
  return S2_FSTAT_TYPE_DIR==st.type ? 1 : 0;
}

static char direntish_is_file( cwal_engine * e, char const * fn, int * theRc ){
  s2_fstat_t st = s2_fstat_t_empty;
  int rc = s2_fstat( fn, cwal_strlen(fn), &st, 1 );
  if(rc){
    *theRc = cwal_exception_setf(e, rc, "s2_fstat(\"%s\") failed.", fn);
    return 0;
  }
  return S2_FSTAT_TYPE_REGULAR==st.type ? 1 : 0;
}

static int direntish_cb_is_df( cwal_callback_args const * args,
                               cwal_value **rv,
                               char dirmode ){
  char const * fn;
  int rc = 0;
  char buul;
  char const ignoreErr = args->argc>1
    ? cwal_value_get_bool(args->argv[1])
    : 0;
  if(!args->argc
     || !(fn=cwal_value_get_cstr(args->argv[0], NULL))){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting a string argument.");
  }
  buul = dirmode
    ? direntish_is_dir(args->engine, fn, &rc)
    : direntish_is_file(args->engine, fn, &rc);
  if(!rc || ignoreErr){
    *rv = buul
      ? cwal_value_true()
      : cwal_value_false();
    if(rc && ignoreErr) rc = 0;
  }
  return rc;
}

static int direntish_cb_is_dir( cwal_callback_args const * args,
                                cwal_value **rv ){
  return direntish_cb_is_df( args, rv, 1 );
}

static int direntish_cb_is_file( cwal_callback_args const * args,
                                 cwal_value **rv ){
  return direntish_cb_is_df( args, rv, 0 );
}

/**
   Script usage:

   dir.eachEntry(Function|Array [, integer mode=0 [, bool prependDirName=false]])

   dir.eachEntry(Function|Array [, bool prependDirName=false])

   The name of each dir entry in dir matching the given mode (0==dirs
   and files, <0==dirs only, >0==non-dirs only) is passed to the given
   function or appended to the given array.

   If prependDirName is a truthy, the dirent's name and a directory
   separator character are prepended to each entry's name for purposes
   of passing them to the callback. Whether or not this resolves to
   absolute paths depends on how the dirent was opened. e.g.
   module.openDir('.') is different from module.openDir(module.cwd())
   in that regard.

   This function always rewinds the directory handle before starting.
*/
static int direntish_cb_each( cwal_callback_args const * args,
                              cwal_value **rv ){
  int rc = 0;
  cwal_function * f = NULL;
  cwal_array * ar = NULL;
  int mode;
  struct dirent * dr;
  cwal_value * vname;
  char useFullNames;
  s2_engine * se = s2_engine_from_args(args);
  cwal_buffer * buf = 0;
  cwal_size_t oldUsed = 0;
  cwal_size_t pathEnd = 0;
  static const int modeDefault = 0;
  THIS_DIRENT;
  assert(se);
  if(args->argc>0){
    ar = cwal_value_array_part(args->engine, args->argv[0]);
    f = ar
      ? NULL
      : cwal_value_function_part(args->engine, args->argv[0]);
  }
  if(!f && !ar){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting Function or Array as "
                               "the first argument.");
  }
  mode = (args->argc>1)
    ? (cwal_value_is_bool(args->argv[1])
       ? modeDefault
       : cwal_value_get_integer(s2_value_unwrap(args->argv[1])))
    : modeDefault;
  useFullNames = args->argc>2
    ? cwal_value_get_bool(args->argv[2])
    : (args->argc==2 && cwal_value_is_bool(args->argv[1])
       ? cwal_value_get_bool(args->argv[1])
       : 0);
  if(useFullNames){
    /* use full names... */
    char const * dirName = 0;
    cwal_size_t dirNameLen = 0;
    dirName = cwal_value_get_cstr( cwal_prop_get(args->self, "name", 4),
                                   &dirNameLen );
    assert(dirName && "someone removed the 'name' property.");
    if(!dirName){
      rc = cwal_cb_throw(args, CWAL_RC_MISUSE, "Did not find 'name' property.");
      goto end;
    }
    buf = &se->buffer;
    oldUsed = buf->used;
    rc = cwal_buffer_printf( args->engine, buf, "%.*s%s",
                             (int)dirNameLen, dirName,
                             S2_DIRECTORY_SEPARATOR );
    pathEnd = buf->used;
    if(rc) goto end;
    assert(pathEnd > oldUsed);
  }
    
  rewinddir(self);
  errno = 0;
  while( !rc && (dr = readdir(self)) ){
    char const * fnc = dr->d_name;
    cwal_size_t fncLen = 0;
    if(mode<0 && !direntish_is_dir(args->engine, fnc, &rc)) continue;
    else if(!rc && mode>0 && direntish_is_dir(args->engine, fnc, &rc)) continue;
    if(rc) break;
    fncLen = cwal_strlen(fnc);
#if 0
    if((1==fncLen && '.'==*fnc)
       || (2==fncLen && '.'==*fnc && '.'==fnc[1])){
      /* This is highly arguable... */
      /* Skip over "." and ".." - these are, more often than not, just noise. */
      continue;
    }
#endif
    if(buf){
      /* Prepend path to the entry name... */
      assert(pathEnd);
      buf->used = pathEnd;
      rc = cwal_buffer_printf(args->engine, buf, "%.*s",
                              (int)fncLen, fnc);
      if(rc) break;
      vname = cwal_new_string_value(args->engine,
                                    (char const *)buf->mem+oldUsed,
                                    buf->used - oldUsed);
    }else{
      /* use relative paths */
      vname = cwal_new_string_value(args->engine, fnc, fncLen );
    }
    if(!vname){
      rc = CWAL_RC_OOM;
      break;
    }
    cwal_value_ref( vname );
    rc = ar
      ? cwal_array_append(ar, vname)
      : cwal_function_call( f, args->self, NULL, 1, &vname );
    cwal_value_unref( vname );
    vname = 0;
  }
#if 0
  if(!rc && errno){
    rc = 
      cwal_cb_throw(args, cwal_errno_to_cwal_rc(errno, CWAL_RC_ERROR),
                  "??? failed with errno %d (%s).",
                  errno, strerror(errno));
  }
#endif
  end:
  if(buf){
    buf->used = oldUsed;
  }
  if(!rc){
    *rv = args->self;
  }
  return rc;
}

#if 0
static int direntish_cb_tell( cwal_callback_args const * args,
                              cwal_value **rv ){
  THIS_DIRENT;
  *rv = cwal_new_integer(args->engine,
                         (cwal_int_t)telldir(self));
  return *rv ? 0 : CWAL_RC_OOM;
}

static int direntish_cb_seek( cwal_callback_args const * args,
                              cwal_value **rv ){
  THIS_DIRENT;
  seekdir(self, (long)cwal_value_get_integer(args->argv[0]));
  return *rv ? 0 : CWAL_RC_OOM;
}
#endif

static int direntish_cb_rewind( cwal_callback_args const * args,
                                cwal_value **rv ){
  THIS_DIRENT;
  rewinddir(self);
  *rv = cwal_value_undefined();
  return 0;
}

static int direntish_cb_read( cwal_callback_args const * args,
                              cwal_value **rv ){
  int rc;
  struct dirent * rd;
  cwal_value * vv = 0;
  THIS_DIRENT;
  errno = 0;
  rd = readdir(self);
  if(!rd){
    if(errno){
      return cwal_cb_throw(args, cwal_errno_to_cwal_rc(errno, CWAL_RC_IO),
                         "readdir() failed with errno %d (%s).",
                         errno, strerror(errno));
    }else{
      *rv = cwal_value_undefined();
      return 0;
    }
  }
#if 0
  vv = cwal_new_object_value(args->engine);
  if(!vv) return CWAL_RC_OOM;
  cwal_value_ref(vv);
  rc = cwal_prop_set(vv, "inode", 5,
                     cwal_new_integer(args->engine, (cwal_int_t)rd->d_ino) );
  if(!rc) rc = cwal_prop_set(vv, "name", 4,
                             cwal_new_string_value(args->engine, rd->d_name,
                                                   cwal_strlen(rd->d_name) ) );
#else
  vv = cwal_new_string_value(args->engine, rd->d_name,
                             cwal_strlen(rd->d_name) );
  cwal_value_ref(vv);
  rc = vv ? 0 : CWAL_RC_OOM;
#endif
  if(rc){
    cwal_value_unref(vv);
  }else{
    *rv = vv;
    cwal_value_unhand(vv);
  }
  return rc;
}


static cwal_value * direntish_prototype( s2_engine * se ){
  int rc = 0;
  cwal_value * proto = 0;
  char const * pKey = "dirent";
  cwal_size_t const keyLen = cwal_strlen(pKey);
  assert(se && se->e);
  proto = cwal_new_object_value(se->e);
  if(!proto){
    rc = CWAL_RC_OOM;
    goto end;
  }else{
    cwal_value * v = cwal_new_string_value(se->e, pKey, keyLen);
    cwal_value_ref(proto);
    cwal_value_ref(v);
    rc = v ? s2_typename_set_v(se, proto, v) : CWAL_RC_OOM;
    cwal_value_unref(v);
    if(rc) goto end;
  }
  assert(!rc);
  if(!rc){
    const s2_func_def funcs[] = {
    S2_FUNC2("close", direntish_cb_close),
    S2_FUNC2("rewind", direntish_cb_rewind),
    S2_FUNC2("read", direntish_cb_read),
    S2_FUNC2("isDir", direntish_cb_is_dir),
    S2_FUNC2("isFile", direntish_cb_is_file),
    S2_FUNC2("eachEntry", direntish_cb_each),
    s2_func_def_empty_m
    };
    rc = s2_install_functions( se, proto, funcs, 0 );
  }
  /*FUNC2("seek", direntish_cb_seek);*/
  /*FUNC2("tell", direntish_cb_tell);*/

  if(rc) goto end;
  else{
    /* Add properties for use with eachEntry()'s iteration mode. */
    cwal_value * tgt = 1 ? proto : cwal_prop_get(proto, "eachEntry", 9);
    assert(tgt && "we just installed this!");
    rc = cwal_prop_set_with_flags(tgt, "EachDir", 7, cwal_new_integer(se->e, -1),
                                  CWAL_VAR_F_CONST);
    if(!rc) rc = cwal_prop_set_with_flags(tgt, "EachAll", 7, cwal_new_integer(se->e, 0),
                               CWAL_VAR_F_CONST);
    if(!rc) rc = cwal_prop_set_with_flags(tgt, "EachFile", 8, cwal_new_integer(se->e, 1),
                                          CWAL_VAR_F_CONST);
  }
  
#undef FUNC2
#undef CHECKV

  end:
  if(rc) cwal_value_unref(proto);
  else cwal_value_unhand(proto);
  return rc ? NULL : proto;
}

static int direntish_chdir( cwal_engine * e, char const * dirname ){
  int rc = chdir(dirname);
  if(rc){
    rc = cwal_exception_setf( e, cwal_errno_to_cwal_rc(errno, CWAL_RC_IO),
                              "chdir(\"%s\") failed with errno %d (%s).",
                              dirname, errno, strerror(errno));
  }
  return rc;
}

cwal_array * direntish_dir_dirs( cwal_value * dirV ){
  cwal_value * dirs = cwal_prop_get( dirV, "dirs", 4 );
  if(!dirs || !cwal_value_is_array(dirs)){
    int rc;
    cwal_engine * e = cwal_value_engine(dirV);
    dirs = cwal_new_array_value(e);
    if(!dirs) return NULL;
    cwal_value_ref(dirs);
    rc = cwal_prop_set( dirV, "dirs", 4, dirs );
    cwal_value_unref(dirs);
    if(rc) dirs = 0;
  }
  return cwal_value_get_array(dirs);
}


static int direntish_getcwd( cwal_engine * e, char * buf, cwal_size_t bufSize ){
  char const * fn = NULL;
  fn = getcwd( buf, bufSize );
  if(!fn){
    return cwal_exception_setf( e, cwal_errno_to_cwal_rc(errno, CWAL_RC_ERROR),
                                "getcwd() failed with errno %d (%s).",
                                errno, strerror(errno));
  }
  return 0;
}

static int direntish_cb_pushd( cwal_callback_args const * args,
                               cwal_value **rv ){
  int rc;
  char const * fn = NULL;
  cwal_array * dirs;
  cwal_value * v;
  enum { BufSize = S2X_DIRENT_BUFSIZE };
  char buf[BufSize];
  if(!args->argc || !(fn=cwal_value_get_cstr(args->argv[0], NULL))){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting a string argument.");
  }
  dirs = direntish_dir_dirs(args->self);
  if(!dirs) return CWAL_RC_OOM;
  rc = direntish_getcwd( args->engine, buf, BufSize );
  if(rc) return rc;
  rc = direntish_chdir(args->engine, fn);
  if(rc) return rc;
  v = cwal_new_string_value(args->engine, buf, cwal_strlen(buf));
  if(!v) return CWAL_RC_OOM;
  cwal_value_ref(v);
  rc = cwal_array_prepend( dirs, v );
  cwal_value_unref(v);
  if(!rc) *rv = args->argv[0];
  return rc;
}

static int direntish_cb_popd( cwal_callback_args const * args,
                              cwal_value **rv ){
  int rc;
  cwal_array * dirs;
  char const * fn;
  cwal_value * v;
  cwal_size_t alen;
  dirs = direntish_dir_dirs(args->self);
  if(!dirs) return CWAL_RC_OOM;
  alen = cwal_array_length_get(dirs);
  if(!alen) {
    return cwal_exception_setf(args->engine, CWAL_RC_RANGE,
                               "No directories are on the stack.");
  }
  v = cwal_array_get( dirs, 0 );
  /**
     Oh, no... if we pop the value from the array then it might
     be destroyed immediately, meaning we can't return it. OTOH,
     we do not want to add a reference to it to avoid it being
     destroyed at pop-time. So we have to create it anew and hope
     that string interning does not screw us over?

     Ah, we could set it in this.PWD to keep it alive.

     Or... cwal_array_shift() was designed for that problem,
     so we'll reverse the dirs order and use that.
  */
  fn = v ? cwal_value_get_cstr(v, NULL) : NULL;
  if(!fn){
    assert(!"Cannot happen?");
    return v ? CWAL_RC_TYPE : CWAL_RC_ERROR;
  }
  /*cwal_array_length_set( dirs, alen -1 );*/
  rc = direntish_chdir(args->engine, fn);
  if(!rc) rc = cwal_array_shift(dirs, rv);
  return rc;
}


static int direntish_cb_chdir( cwal_callback_args const * args,
                               cwal_value **rv ){
  char const * fn = NULL;
  if(!args->argc || !(fn=cwal_value_get_cstr(args->argv[0], NULL))){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting a string argument.");
  }else{
    int const rc = direntish_chdir( args->engine, fn );
    if(!rc) *rv = args->self;
    return rc;
  }
}

static int direntish_cb_getcwd( cwal_callback_args const * args,
                                cwal_value **rv ){
  enum { BufSize = S2X_DIRENT_BUFSIZE };
  char buf[BufSize];
  int rc;
  rc = direntish_getcwd( args->engine, buf, BufSize );
  if(!rc){
    *rv = cwal_new_string_value(args->engine, buf, cwal_strlen(buf));
    rc = *rv ? 0 : CWAL_RC_OOM;
  }
  return rc;
}

static int direntish_cb_open( cwal_callback_args const * args,
                              cwal_value **rv ){
  char const * fn = NULL;
  DIR * dir;
  cwal_value * rvv;
  int rc = 0;
  cwal_value * proto =
    (cwal_value *)cwal_args_state(args, &direntish_prototype_id);
  if(!proto){
    return cwal_cb_throw(args, CWAL_RC_UNSUPPORTED,
                       "dirent prototype is missing. Make sure this "
                       "module is not both statically and "
                       "dynamically linked!");
  }
  assert(cwal_value_refcount(proto)>0);

  if(!args->argc || !(fn=cwal_value_get_cstr(args->argv[0], NULL))){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting a string argument.");
  }
  dir = opendir( fn );
  if(!dir){
    return cwal_exception_setf(args->engine, CWAL_RC_ERROR,
                               "opendir(3) failed. errno=%d (%s)\n",
                               errno, strerror(errno));
  }
  rvv = cwal_new_native_value(args->engine, dir,
                              direntish_closedir_finalizer,
                              &direntish_typeid);
  if(rvv){
    /* ARGS_SE; */
    cwal_value_ref(rvv);
    rc = cwal_prop_set( rvv, "name", 4, args->argv[0] );
    if(!rc) rc = cwal_value_prototype_set( rvv, proto );
  }else{
    rc = CWAL_RC_OOM;
  }
  if(rc){
    cwal_value_unref(rvv);
  }else{
    *rv = rvv;
    cwal_value_unhand(rvv);
  }
  return rc;
}

static int s2_module_init_dirent( s2_engine * se, cwal_value ** rv ){
  cwal_value * proto;
  proto = direntish_prototype(se);
  if(!proto) return CWAL_RC_OOM;
  else{
    int rc = 0;
    cwal_value * mod = cwal_new_object_value(se->e);
    s2_func_def funcs[] = {
    S2_FUNC2("openDir", direntish_cb_open)
    /* openDir MUST be first for custom init below to work. */,
    S2_FUNC2("isDir", direntish_cb_is_dir),
    S2_FUNC2("isFile", direntish_cb_is_file),
    S2_FUNC2("chdir", direntish_cb_chdir),
    S2_FUNC2("cwd", direntish_cb_getcwd),
    S2_FUNC2("pushd", direntish_cb_pushd),
    S2_FUNC2("popd", direntish_cb_popd),
    s2_func_def_empty_m
    };
    if(!mod) return CWAL_RC_OOM;

    {
      /* Add dirSeparator property... */
      cwal_value * sep = cwal_new_string_value(se->e, S2_DIRECTORY_SEPARATOR,
                                               sizeof(S2_DIRECTORY_SEPARATOR)-
                                               sizeof(S2_DIRECTORY_SEPARATOR[0])
                                               );
      if(!sep) {
        rc = CWAL_RC_OOM;
        goto end;
      }
      cwal_value_ref(sep);
      rc = cwal_prop_set_with_flags( mod, "dirSeparator", 12,
                                     sep, CWAL_VAR_F_CONST );
      cwal_value_unref(sep);
      if(rc) goto end;
    }
    /* Stash the prototype in openDir(), noting that that is a
       (void*) stash, not a (cwal_value*) stash, meaning that
       there is still a lifetime-level issue to solve (see
       below). */
    funcs[0].state = proto;
    funcs[0].stateTypeID = &direntish_prototype_id;

    cwal_value_ref(mod);
    cwal_value_ref(proto);
    rc = s2_install_functions( se, mod, funcs, 0 );
    if(!rc){
      /* The problem: stashing the prototype in a func's state
         does not account for rescoping and vacuum safety and
         whatnot. So we need to stash it as a property in the
         function and hope that no client calls
         clearProperties() on it. Or we can add a Native wrapper
         and manage/rescope it from a cwal_value_rescoper_f().
      */
      cwal_value * openDir = cwal_prop_get(mod, "openDir", 7);
      assert(openDir && "we JUST stashed this!");
      rc = s2_stash_hidden_member(openDir, proto);
    }
    cwal_value_unref(proto)
      /* on success we still have a ref via s2_stash_hidden_member() */;
    end:
    if(rc){
      cwal_value_unref(mod);
    }else{
      cwal_value_unhand(mod);
      *rv = mod;
    }
    return rc;
  }
}

S2_MODULE_REGISTER_(dirent);

#undef MARKER
#undef ARGS_SE
#undef THIS_DIRENT
#undef S2X_DIRENT_BUFSIZE
