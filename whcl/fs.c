/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#include "internal.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <errno.h>

#if WHCL_HAVE_REALPATH
#  include <limits.h>
#endif

#if WHCL_HAVE_STAT || WHCL_HAVE_MKDIR
#  include <sys/stat.h>
#  include <sys/types.h>
#endif

#ifdef WHCL_OS_UNIX
#  include <unistd.h> /* W_OK, R_OK */
#  include <sys/stat.h>
#  include <sys/types.h>
#else
#  include  <io.h>
#endif

#if 1
#define MARKER(pfexpr) if(1) printf("MARKER: %s:%d:\t",__FILE__,__LINE__); if(1) printf pfexpr
#else
#define MARKER if(0) printf
#endif

enum {
/**
   Internal buffer sized used for various filesystem-related
   routines.
*/
WHCL_FILENAME_BUF_SIZE = 1024 * 2
};

const whcl_fstat_t whcl_fstat_t_empty = whcl_fstat_t_empty_m;


void whcl_fclose(FILE *f){
  if(f
     && f!=stdin
     && f!=stdout
     && f!=stderr){
    fclose(f);
  }
}

FILE *whcl_fopen(const char *zName, const char *zMode){
  FILE *f;
  if(zName && ('-'==*zName && !zName[1])){
    f = (strchr(zMode, 'w')
         || strchr(zMode,'+')
         || strchr(zMode,'a'))
      ? stdout
      : stdin
      ;
  }else{
    f = fopen(zName, zMode);
  }
  return f;
}

int whcl_getcwd( cwal_engine * const e, cwal_buffer * const tgt ){
#ifdef WHCL_OS_WINDOWS
  if(e || tgt){/*unused*/}
  return CWAL_RC_UNSUPPORTED;
#else
  cwal_size_t const bufSize = 1024 * 2;
  int rc = cwal_buffer_reserve( e, tgt, tgt->used + bufSize );
  if(rc) return rc;
  else if( ! getcwd((char *)(tgt->mem+tgt->used), (size_t)bufSize) ){
    rc = cwal_errno_to_cwal_rc( errno, CWAL_RC_IO);
  }else{
    tgt->used += cwal_strlen( (char const *)(tgt->mem+tgt->used) );
    assert( tgt->used <= tgt->capacity );
  }
  return rc;
#endif
}

int whcl_fstat( char const * filename,
                cwal_int_t fnLen,
                whcl_fstat_t * tgt,
                bool derefSymlinks ){
#if !WHCL_HAVE_STAT || defined(WHCL_OS_WINDOWS)
  if(filename || fnLen || tgt || derefSymlinks){/*unused params*/}
  return CWAL_RC_UNSUPPORTED;
#else
  int rc;
  if(!filename || !tgt) rc = CWAL_RC_MISUSE;
  else if(!*filename || !fnLen) rc = CWAL_RC_RANGE;

  if(fnLen<0) fnLen = (cwal_int_t)cwal_strlen(filename);
  if(fnLen >= (cwal_int_t)WHCL_FILENAME_BUF_SIZE) rc = CWAL_RC_RANGE;
  else{
    struct stat buf;
    bool isCwalRc = false;
    char fnBuf[WHCL_FILENAME_BUF_SIZE] = {0};
    memcpy( fnBuf, filename, (size_t)fnLen );
    fnBuf[fnLen] = 0;
    if( derefSymlinks ){
      rc = stat(fnBuf, &buf);
    }else{
#if WHCL_HAVE_LSTAT
      rc = lstat(fnBuf, &buf);
#else
      rc = CWAL_RC_UNSUPPORTED;
      isCwalRc = true;
#endif
    }
    if(rc){
      if(!isCwalRc){
        rc = cwal_errno_to_cwal_rc(errno, CWAL_RC_IO);
      }
    }else if(tgt){
      *tgt = whcl_fstat_t_empty;
      tgt->ctime = (uint64_t)buf.st_ctime;
      tgt->mtime = (uint64_t)buf.st_mtime;
      tgt->size = (uint64_t)buf.st_size;
      tgt->perm = buf.st_mode & 0777 /* Unix file permissions are only the bottom 9 bits */;
#define TCHECK(SMACRO, TYPE) if(SMACRO(buf.st_mode)) tgt->type = WHCL_FSTAT_TYPE_ ## TYPE
      TCHECK(S_ISDIR,DIR);
      else TCHECK(S_ISREG,REGULAR);
#if defined(S_ISLNK)
      else TCHECK(S_ISLNK,LINK);
#endif
#if defined(S_ISSOCK)
      else TCHECK(S_ISSOCK,SOCKET);
#endif
#if defined(S_ISCHR)
      else TCHECK(S_ISCHR,CHAR);
#endif
#if defined(S_ISBLK)
      else TCHECK(S_ISBLK,BLOCK);
#endif
#if defined(S_ISFIFO)
      else TCHECK(S_ISFIFO,FIFO);
#endif
      else tgt->type = WHCL_FSTAT_TYPE_UNKNOWN;
#undef TCHECK
    }
  }
  return rc;
#endif
}

int whcl_fstat_to_object( whcl_engine * const el,
                          whcl_fstat_t const * const fst,
                          cwal_value ** rv ){
  cwal_value * obj = 0;
  cwal_value * v = 0;
  cwal_value * keysV = 0;
  cwal_array * keysA = 0;
  char const * stashKey = "stat()keys";
  int rc = 0;
  enum {
    keyUNKNOWN = 0,
    keyREGULAR,
    keyDIR,
    keyLINK,
    keyBLOCK,
    keyCHAR,
    keyFIFO,
    keySOCKET,
    keyMTIME,
    keyCTIME,
    keySIZE,
    keyPERM,
    keyTYPE,
    keyEND
  };
  keysV = whcl_stash_get(el, stashKey);
  if(keysV){
    keysA = cwal_value_get_array(keysV);
  }else{
    /* set up key names cache... */
    cwal_value * key = 0;
    keysV = cwal_new_array_value(el->ec);
    if(!keysV){
      rc = CWAL_RC_OOM;
      goto end;
    }
    keysA = cwal_value_get_array(keysV);
    cwal_value_ref(keysV);
    rc = whcl_stash_set(el, stashKey, keysV);
    cwal_value_unref(keysV);
    if(rc) goto end;
    else{
      assert(cwal_value_refcount(keysV)>0 && "stash is holding a ref");
    }
    rc = cwal_array_reserve(keysA, (cwal_size_t)keyEND);
    if(rc) goto end;
#define KEY(TYPE,STR)                                                      \
    key = cwal_new_string_value(el->ec, STR, cwal_strlen(STR));   \
    if(!key) { rc = CWAL_RC_OOM; goto end; }                            \
    cwal_value_ref(key);                                                \
    rc = cwal_array_set( keysA, (cwal_size_t)key##TYPE, key );          \
    cwal_value_unref(key);                                              \
    key = 0;                                                            \
    if(rc) goto end
    KEY(UNKNOWN,"unknown");
    KEY(REGULAR,"file");
    KEY(DIR,"dir");
    KEY(LINK,"link");
    KEY(BLOCK,"block");
    KEY(CHAR,"char");
    KEY(FIFO,"fifo");
    KEY(SOCKET,"socket");
    KEY(MTIME,"mtime");
    KEY(CTIME,"ctime");
    KEY(SIZE,"size");
    KEY(PERM,"perm");
    KEY(TYPE,"type");
#undef KEY    
  }/* end key stash setup */

  assert(!rc);

  obj = cwal_new_object_value(el->ec);
  if(!obj) { rc = CWAL_RC_OOM; goto end; }
  cwal_value_ref(obj);

#define VCHECK 
#define VSET(KEY)                                                       \
  if(!v) { rc = CWAL_RC_OOM; goto end; }                                \
  cwal_value_ref(v);                                                    \
  rc = cwal_prop_set_v(obj, cwal_array_get(keysA, (cwal_size_t)key##KEY), v); \
  cwal_value_unref(v);                                                  \
  v = 0;                                                                \
  if(rc) goto end
#define INTVAL(KEY,IV) \
  v = cwal_new_integer(el->ec, (cwal_int_t)(IV)); \
  VSET(KEY);
  
  INTVAL(MTIME,fst->mtime);
  INTVAL(CTIME,fst->ctime);
  INTVAL(SIZE,fst->size);
  INTVAL(PERM,fst->perm);
#undef INTVAL
#undef VCHECK
#undef VSET

  switch(fst->type){
#define CASE(TYPE) case WHCL_FSTAT_TYPE_##TYPE: \
    v = cwal_array_get(keysA, (cwal_size_t)key##TYPE); \
    assert(v); \
    rc = cwal_prop_set_v(obj, cwal_array_get(keysA, (cwal_size_t)keyTYPE), v); \
    break
    CASE(REGULAR);
    CASE(DIR);
    CASE(LINK);
    CASE(BLOCK);
    CASE(CHAR);
    CASE(FIFO);
    CASE(SOCKET);
    default:
      v = cwal_array_get(keysA, (cwal_size_t)keyUNKNOWN);
      assert(v);
      rc = cwal_prop_set_v(obj, cwal_array_get(keysA, (cwal_size_t)keyTYPE), v);
      break;
  }
#undef CASE
  end:
  if(rc){
    cwal_value_unref(obj);
  }else{
    *rv = obj;
    cwal_value_unhand(obj);
  }
  return rc;
}

int whcl_cb_fstat( cwal_callback_args const * args, cwal_value ** rv ){
  int rc = 0;
  whcl_fstat_t stbuf = whcl_fstat_t_empty;
  cwal_size_t fnLen = 0;
  char const * fn = NULL;
  bool derefSymlink = 1;
  bool doQuickCheck = 1;
  uint16_t argNdx = 0;
  //if( (rc = s2_cb_disable_check(args, S2_DISABLE_FS_STAT)) ) return rc;
  *rv = 0;
  for(; whcl_arg_has_flag(args, &argNdx, &fn, &fnLen);
      fn = NULL, fnLen = 0){
    if(5==fnLen){
      if(0==memcmp("-full", fn, 5)){
        doQuickCheck = false;
        continue;
      }else if(0==memcmp("-link", fn, 5)){
        derefSymlink = false;
        continue;
      }
    }
    break;
  }
  if(!fn && args->argc>argNdx){
    fn = cwal_value_get_cstr( args->argv[argNdx], &fnLen );
  }
  if(!fn){
    rc = cwal_exception_setf( args->engine, CWAL_RC_MISUSE,
                              "Expecting: [-full] [-s] filename");
    goto end;
  }
  rc = whcl_fstat( fn, fnLen, &stbuf, derefSymlink );
  if(rc){
    if(doQuickCheck){
      *rv = cwal_value_false();
      rc = 0;
    }else{
      rc = cwal_exception_setf(args->engine, rc,
                               "stat(%.*s) failed with code %d (%s)",
                               (int)fnLen, fn, rc, cwal_rc_cstr(rc));
    }
    goto end;
  }
  end:
  if(!rc){
    if(doQuickCheck){
      /* We just wanted to check for stat()ability */
      if(!*rv) *rv = cwal_value_true();
    }else{
      whcl_engine * const el = whcl_engine_from_args(args);
      assert(el);
      rc = whcl_fstat_to_object( el, &stbuf, rv );
      if(rc){
        assert(CWAL_RC_OOM == rc);
      }
    }
  }
  return rc;
}



int whcl_mkdir( char const * name, int mode, char const ** errMsg ){
#if !WHCL_HAVE_MKDIR
  if(name || mode){/*unused*/}
  if(errMsg) *errMsg = "mkdir() is not available in this build.";
  return CWAL_RC_UNSUPPORTED;
#else
  int rc = mkdir(name, (mode_t)mode);
  if(rc){
    if(errMsg) *errMsg = strerror(errno);
    rc = cwal_errno_to_cwal_rc(errno, CWAL_RC_IO);
  }
  return rc;
#endif
}

int whcl_mkdir_p( char const * name, int mode, char const ** errMsg ){
#if !WHCL_HAVE_MKDIR
  if(name || mode){/*unused*/}
  if(errMsg) *errMsg = "mkdir() is not available in this build.";
  return CWAL_RC_UNSUPPORTED;
#else
  int rc = 0;
  char const * t;
  cwal_size_t tLen = 0;
  whcl_path_toker pt = whcl_path_toker_empty;
  char buf[1024 * 2] = {0};
  char * bufPos = buf;
  char const * bufEnd = buf + sizeof(buf);
  int dirCount = 0;
  char const leadingSlash = '/'==*name ? 1 : 0;
  whcl_path_toker_init(&pt, name, cwal_strlen(name));
  pt.separators = "/";
  while(0==whcl_path_toker_next(&pt, &t, &tLen)){
    if(!tLen) continue /* adjacent dir separators */;
    else if(bufPos+tLen+1 >= bufEnd){
      if(errMsg) *errMsg = "Directory path is too long.";
      return CWAL_RC_RANGE;
    }
    if((0==dirCount && leadingSlash)
       || dirCount>0){
      *bufPos++ = '/';
    }
    ++dirCount;
    memcpy(bufPos, t, (size_t)tLen);
    bufPos += tLen;
    *bufPos = 0;
    if(!whcl_is_dir(buf, 0)){
      rc = whcl_mkdir(buf, mode, errMsg);
      if(rc) break;
    }
  }
  if(!rc && !dirCount){
    rc = CWAL_RC_MISUSE;
    if(errMsg) *errMsg = "No directory names found in the given string.";
  }
  return rc;
#endif
}

int whcl_cb_mkdir( cwal_callback_args const * args, cwal_value ** rv ){
#if !WHCL_HAVE_MKDIR
  if(rv){/*unused*/}
  return cwal_cb_throw(args, CWAL_RC_UNSUPPORTED,
                     "mkdir() is not available in this build.");
#else
  int rc;
  char const * p = 0;
  cwal_size_t pLen = 0;
  cwal_int_t mode = 0750;
  char const *errMsg = 0;
  uint16_t argNdx = 0;
  bool mkp = 0;
  char dirBuf[WHCL_FILENAME_BUF_SIZE]
    /* We copy the given path to a char buffer only so that we can
       ensure that it is NUL-terminated (X-/Z-strings can be created
       without a NUL terminator). We "could" instead use the
       whcl_engine::buffer for this purpose, rather than hard-coding a
       length limit. */;
  if(!args->argc || args->argc>3) goto misuse;
  if(whcl_val_is_flag(args->argv[argNdx], "-p", 2)){
    mkp = true;
    if(++argNdx==args->argc) goto misuse;
  }
  p = cwal_value_get_cstr(args->argv[argNdx++], &pLen);
  if(!p || !pLen) goto misuse;
  if(argNdx<args->argc){
    mode = cwal_value_get_integer(args->argv[argNdx++]);
  }
  if(argNdx<args->argc) goto misuse;
  assert(p && pLen);
  if(pLen >= (cwal_size_t)sizeof(dirBuf)){
    return cwal_exception_setf(args->engine, CWAL_RC_RANGE,
                               "Dir name is too long.");
  }
  *rv = args->self;
  memcpy(dirBuf, p, (size_t)pLen);
  if(whcl_is_dir(dirBuf, 0)){
    return 0;
  }
  rc = mkp
    ? whcl_mkdir_p(dirBuf, (int)mode, &errMsg)
    : whcl_mkdir(dirBuf, (int)mode, &errMsg);
  if(rc){
    rc = cwal_exception_setf(args->engine,
                             cwal_errno_to_cwal_rc(errno, CWAL_RC_IO),
                             "mkdir() failed with errno %d: %s",
                             errno, errMsg);
  }
  return rc;
  misuse:
  return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                             "mkdir expecting: "
                             "[-p] dirName [unixPermissions] "
                             "with a non-empty directory name.");
#endif
}


int whcl_cb_getcwd( cwal_callback_args const * args, cwal_value ** rv ){
  whcl_engine * const el = whcl_engine_from_args(args);
  cwal_buffer * const buf = &el->escBuf;
  cwal_size_t const pos = buf->used;
  int rc;
  bool addSep = false;

  //if( (rc = whcl_cb_disable_check(args, WHCL_DISABLE_FS_STAT)) ) return rc;
  if(args->argc){
    if(whcl_val_is_flag(args->argv[0], "-slash", 6)){
      addSep = true;
    }else{
      return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                 "Unhandled getcwd argument.");
    }
  }
  rc = whcl_getcwd( args->engine, buf );
  if(rc) rc = cwal_exception_setf(args->engine, rc,
                                  "getcwd(3) failed with code %d (%s).",
                                  rc, cwal_rc_cstr(rc));
  else{
    if(addSep){
      rc = cwal_buffer_append( args->engine,
                               buf,
                               WHCL_DIRECTORY_SEPARATOR,
                               sizeof(WHCL_DIRECTORY_SEPARATOR)-
                               sizeof(WHCL_DIRECTORY_SEPARATOR[0]) );
    }
    if(!rc){
      *rv = cwal_new_string_value(args->engine,
                                  (char const *)(buf->mem + pos),
                                  (cwal_size_t)(buf->used - pos));
      rc = *rv ? 0 : CWAL_RC_OOM;
    }
  }
  if(buf->mem){
    buf->used = pos;
    buf->mem[buf->used] = 0;
  }
  return rc;
}

int whcl_passthrough_FILE( cwal_engine * e, FILE * file ){
  return cwal_stream( cwal_input_f_FILE, file,
                      cwal_output_f_cwal_engine, e );
}

int whcl_passthrough_filename( cwal_engine * e, char const * filename ){
  int rc;
  FILE * f = whcl_fopen(filename, "r");
  if(!f) rc = CWAL_RC_IO;
  else{
    rc = whcl_passthrough_FILE(e, f);
    whcl_fclose(f);
  }
  return rc;
}

/**
   Streams a file's contents directly to the script engine's output
   channel, without (unless the file is small) reading the whole file
   into a buffer first.

   Script usage:

   passthrough(filename)
*/
static int whcl_cb_file_passthrough( cwal_callback_args const * args,
                                   cwal_value **rv ){
  char const * fn;
  cwal_size_t len;
  fn = args->argc
    ? cwal_value_get_cstr(args->argv[0], &len)
    : NULL;
  if(!fn){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                       "Expecting non-empty string "
                       "argument.");
  }else if(!whcl_file_is_accessible(fn, 0)){
    return cwal_exception_setf(args->engine, CWAL_RC_NOT_FOUND,
                       "Cannot find file: %s", fn);
  }else{
    FILE * fi = whcl_fopen(fn, "r");
    int rc;
    if(!fi){
      rc = cwal_exception_setf(args->engine, CWAL_RC_IO,
                       "Could not open file for reading: %s",
                       fn);
    }else{
      rc = cwal_stream( cwal_input_f_FILE, fi,
                        cwal_output_f_cwal_engine,
                        args->engine );
      whcl_fclose(fi);
    }
    if(!rc) *rv = args->self;
    return rc;
  }
}


#ifdef _WIN32
#  define CHECKACCESS _access
#  define CHECKRIGHTS (checkWrite ? 2 : 4)
    /*
      2==writeable
      4==readable
      6==r/w
    */
#else
  /* assume unix-like */
#  define CHECKACCESS access
#  define CHECKRIGHTS (checkWrite ? W_OK : R_OK)
#endif

bool whcl_file_is_accessible( char const * fn, bool checkWrite ){
  return (0 == CHECKACCESS( fn, CHECKRIGHTS ));
}

bool whcl_is_dir( char const * fn, bool checkWrite ){
#ifdef WHCL_OS_UNIX
  struct stat buf;
  if(checkWrite && 0!=CHECKACCESS(fn, CHECKRIGHTS)){
    return 0;
  }else if(stat(fn, &buf)) return 0;
  else return S_ISDIR(buf.st_mode) ? 1 : 0;
#else
  /*
    MISSING IMPL for Windows. Potentially requires re-encoding string
    bytes, since we cannot know whether the ones we are given came
    from a Windows console (in which case they might have some Code
    Page encoding). Interested readers are referred to the fossil(1)
    source tree, which jumps through several hoops in that regard.
  */
  #warning "Missing Windows impl - see code comments."
  return 0;
#endif
}

#undef CHECKACCESS
#undef CHECKRIGHTS

static int whcl_cb_fs_accessible( int isFile, cwal_callback_args const * args, cwal_value **rv ){
  bool checkWrite = false;
  cwal_size_t n = 0;
  char const * fn = NULL;
  uint16_t argNdx = 0;
  if(whcl_arg_has_flag(args, &argNdx, &fn, &n)){
    if(2==n && 0==memcmp(fn, "-w", 2)){
      checkWrite = true;
      fn = NULL;
      n = 0;
    }
  }
  if(!fn){
    fn = args->argc>argNdx
      ? cwal_value_get_cstr(args->argv[0], 0)
      : NULL;
    if(!fn) return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                       "Expecting a filename argument.");
  }
  *rv = cwal_new_bool(isFile
                      ? whcl_file_is_accessible(fn, checkWrite)
                      : whcl_is_dir(fn, checkWrite));
  return 0;
}

int whcl_cb_file_accessible( cwal_callback_args const * args, cwal_value **rv ){
  return whcl_cb_fs_accessible(1, args, rv);
}

int whcl_cb_dir_accessible( cwal_callback_args const * args, cwal_value **rv ){
  return whcl_cb_fs_accessible(0, args, rv);
}

int whcl_cb_realpath( cwal_callback_args const * args, cwal_value **rv ){
#if !WHCL_HAVE_REALPATH
  if(rv){/*unused*/}
  return cwal_exception_setf(args->engine, CWAL_RC_UNSUPPORTED,
                             "realpath() not supported in this build.");
#else
  enum { BufSize = PATH_MAX + 1 };
  char buf[BufSize];
  char const * p;
  cwal_size_t strLen = 0;
  char const * str = args->argc
    ? cwal_value_get_cstr(args->argv[0], &strLen)
    : 0;
  //if( (rc = s2_cb_disable_check(args, S2_DISABLE_FS_STAT)) ) return rc;
  if(!str) return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                      "Expecting a string argument.");
  p = realpath(str, buf);
  if(!p){
    switch(errno){
      case ENOENT:
        *rv = cwal_value_undefined();
        return 0;
      default:
        return cwal_exception_setf(args->engine,
                                   cwal_errno_to_cwal_rc( errno,
                                                        CWAL_RC_ERROR ),
                                   "realpath('%.*s') failed with errno %d (%s)",
                                   (int)strLen, str, errno, strerror(errno));
    }
  }
  *rv = cwal_new_string_value(args->engine, p, cwal_strlen(p));
  return *rv ? 0 : CWAL_RC_OOM;
#endif
}

int whcl_chdir( char const * dir, cwal_size_t dirLen ){
#if WHCL_HAVE_CHDIR
  char fnBuf[WHCL_FILENAME_BUF_SIZE] = {0};
  int rc = 0;
  if(dirLen >= (cwal_size_t)WHCL_FILENAME_BUF_SIZE) return CWAL_RC_RANGE;
  memcpy( fnBuf, dir, dirLen );
  fnBuf[dirLen] = 0;
  if(chdir(fnBuf)) rc = cwal_errno_to_cwal_rc(0, CWAL_RC_IO);
  return rc;
#else
  if(dir && dirLen){/* unused */}
  return CWAL_RC_UNSUPPORTED;
#endif
}

int whcl_cb_chdir( cwal_callback_args const * args, cwal_value **rv ){
  cwal_size_t strLen = 0;
  int rc;
  char const * str = args->argc
    ? cwal_value_get_cstr(args->argv[0], &strLen)
    : 0;
  //if( (rc = s2_cb_disable_check(args, S2_DISABLE_FS_STAT)) ) return rc;
  if(!str) rc = cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                            "Expecting a string (directory) argument.");
  else{
    rc = whcl_chdir( str, strLen );
    if(rc) rc = cwal_exception_setf(args->engine, rc,
                            "chdir() failed with code %d (%s).",
                            rc, cwal_rc_cstr(rc));
    else *rv = cwal_value_undefined();
  }
  return rc;  
}


int whcl__install_fs( whcl_engine * const el ){
  cwal_value * v;
  cwal_value * sub = NULL;
  cwal_value * const tgt = el->cache.vWhcl;
  char const * name = "fs";
  int rc;
  if(el->cache.installAPI & WHCL__INSTALL_API_fs) return 0;
  rc = whcl__install_sub(el, tgt, name, true, &sub);
  if(0==rc){
    whcl_func_def const funcs[] = {
      WHCL_FUNC2("stat", whcl_cb_fstat),
      WHCL_FUNC2("realpath", whcl_cb_realpath),
      WHCL_FUNC2("passthrough", whcl_cb_file_passthrough),
      WHCL_FUNC2("mkdir", whcl_cb_mkdir),
      WHCL_FUNC2("getcwd", whcl_cb_getcwd),
      WHCL_FUNC2("file-accessible", whcl_cb_file_accessible),
      WHCL_FUNC2("dir-accessible", whcl_cb_dir_accessible),
      WHCL_FUNC2("chdir", whcl_cb_chdir),
      whcl_func_def_empty_m
    };
    rc = whcl_install_functions(el, sub, funcs, 0);
  }
  if(0==rc){
    /* Add pushd/popd impls, taking care that they behave
       if called after being copied into other contexts. */
    char const * src =
      "set -const F[dir-stack] array\n"
      "set -const F[pushd] proc {d} {"
      "decl c [G]; C $d; S.push $c; return $d"
      "} using -scope {\n"
      "decl -const S F[dir-stack]\n"
      "decl -const G F[getcwd]\n"
      "decl -const C F[chdir]\n"
      "}\n"
      "set -const F[popd] proc {} {\n"
      "if {![S length]} {"
      "throw exception RANGE 'Directory stack is empty.'"
      "}\n"
      "decl d [S.pop]; C $d; return $d\n"
      "} using -scope {\n"
      "decl -const S F[dir-stack]\n"
      "decl -const C F[chdir]\n"
      "}\n";
    rc = whcl_eval_cstr_with_var(el, "F", sub, "fs API init",
                                 src, -1, NULL);
    if(rc) goto end;
  }
  
#define SET(NAME)                                                    \
  if(!v) { WHCL__WARN_OOM; rc = CWAL_RC_OOM; goto end; }             \
  cwal_value_ref(v);                                                 \
  rc = cwal_prop_set( sub, NAME, cwal_strlen(NAME), v );             \
  cwal_value_unref(v);                                               \
  v = 0;                                                             \
  if(rc) goto end;

  v = cwal_new_string_value(el->ec,
                            WHCL_DIRECTORY_SEPARATOR,
                            sizeof(WHCL_DIRECTORY_SEPARATOR)-
                            sizeof(WHCL_DIRECTORY_SEPARATOR[0]) );
  SET("dir-separator");

  end:
#undef SET
  if(0==rc) el->cache.installAPI |= WHCL__INSTALL_API_fs;
  return rc;
}

#undef MARKER
