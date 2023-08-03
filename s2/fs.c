/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

#include "libs2.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

/**
#ifndef S2_HAVE_REALPATH
#  if defined(_BSD_SOURCE)
#    define S2_HAVE_REALPATH 1
#  elif defined(_XOPEN_SOURCE) && _XOPEN_SOURCE>=500
#    define S2_HAVE_REALPATH 1
#  elif defined(_XOPEN_SOURCE) && defined(_XOPEN_SOURCE_EXTENDED)
#    if _XOPEN_SOURCE && _XOPEN_SOURCE_EXTENDED
#      define S2_HAVE_REALPATH 1
#    endif
#  endif
#  ifndef S2_HAVE_REALPATH
#    define S2_HAVE_REALPATH 0
#  endif
#endif
*/

#if S2_HAVE_REALPATH
#  include <limits.h>
#endif

#if S2_HAVE_STAT || S2_HAVE_MKDIR
#  include <sys/stat.h>
#  include <sys/types.h>
#endif


#ifdef S2_OS_UNIX
#  include <unistd.h> /* W_OK, R_OK */
#  include <sys/stat.h>
#  include <sys/types.h>
#else
#  include  <io.h>
#endif

#if 1
#include <stdio.h>
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__); if(1) printf pfexp
#else
#define MARKER(pfexp) (void)0
#endif

enum {
/**
   Internal buffer sized used for various filesystem-related
   routines.
*/
S2_FILENAME_BUF_SIZE = 1024 * 2
};

int s2_getcwd( cwal_engine * e, cwal_buffer * tgt ){
#ifdef S2_OS_WINDOWS
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

int s2_mkdir( char const * name, int mode, char const ** errMsg ){
#if !S2_HAVE_MKDIR
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

int s2_mkdir_p( char const * name, int mode, char const ** errMsg ){
#if !S2_HAVE_MKDIR
  if(name || mode){/*unused*/}
  if(errMsg) *errMsg = "mkdir() is not available in this build.";
  return CWAL_RC_UNSUPPORTED;
#else
  int rc = 0;
  char const * t;
  cwal_size_t tLen = 0;
  s2_path_toker pt = s2_path_toker_empty;
  char buf[1024 * 2] = {0};
  char * bufPos = buf;
  char const * bufEnd = buf + sizeof(buf);
  int dirCount = 0;
  char const leadingSlash = '/'==*name ? 1 : 0;
  s2_path_toker_init(&pt, name, cwal_strlen(name));
  pt.separators = "/";
  while(0==s2_path_toker_next(&pt, &t, &tLen)){
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
    if(!s2_is_dir(buf, 0)){
      rc = s2_mkdir(buf, mode, errMsg);
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

int s2_cb_mkdir( cwal_callback_args const * args, cwal_value ** rv ){
#if !S2_HAVE_MKDIR
  if(rv){/*unused*/}
  return cwal_cb_throw(args, CWAL_RC_UNSUPPORTED,
                     "mkdir() is not available in this build.");
#else
  int rc;
  char const * p;
  cwal_size_t pLen = 0;
  cwal_int_t mode = 0750;
  char const *errMsg = 0;
  uint16_t ndxMode = 0/* args->argv index of the permissions mode argument, 0 for none */;
  char mkp = 0;
  char dirBuf[S2_FILENAME_BUF_SIZE]
    /* We copy the given path to a char buffer only so that we can
       ensure that it is NUL-terminated (X-/Z-strings can be created
       without a NUL terminator). We "could" instead use the
       s2_engine::buffer for this purpose, rather than hard-coding a
       length limit. */;
  if( (rc = s2_cb_disable_check(args, S2_DISABLE_FS_WRITE | S2_DISABLE_FS_STAT)) ) return rc;
  else if(!args->argc || args->argc>3) goto misuse;
  else if(1==args->argc){
    /* (dir) */
  }else if(cwal_value_is_bool(args->argv[1])){
    /* (dir, bool [, mode]); */
    mkp = cwal_value_get_bool(args->argv[1]);
    ndxMode = args->argc>2 ? 2 : 0;
  }else{
    /* (dir, mode) */
    ndxMode = 1;
  }
  p = cwal_value_get_cstr(args->argv[0], &pLen);
  if(!p || !pLen) goto misuse;
  else if(pLen >= (cwal_size_t)sizeof(dirBuf)){
    return cwal_cb_throw(args, CWAL_RC_RANGE, "Dir name is too long.");
  }
  if(ndxMode){
    if(!cwal_value_is_number(args->argv[ndxMode])) goto misuse;
    mode = cwal_value_get_integer(args->argv[ndxMode]);
  }
  *rv = 0;
  memcpy(dirBuf, p, (size_t)pLen);
  if(s2_is_dir(dirBuf, 0)){
    return 0;
  }
  rc = mkp
    ? s2_mkdir_p(dirBuf, (int)mode, &errMsg)
    : s2_mkdir(dirBuf, (int)mode, &errMsg);
  if(rc){
    rc = cwal_cb_throw(args, cwal_errno_to_cwal_rc(errno, CWAL_RC_IO),
                     "mkdir() failed with errno %d: %s",
                     errno, errMsg);
  }
  return rc;
  misuse:
  return cwal_cb_throw(args, CWAL_RC_MISUSE,
                     "mkdir() requires (string dir [,bool createParentDirs] "
                     "[,int unixPermissions]) arguments, "
                     "with a non-empty directory name.");
#endif
}


int s2_cb_getcwd( cwal_callback_args const * args, cwal_value ** rv ){
  s2_engine * se = s2_engine_from_args(args);
  cwal_buffer * buf = &se->buffer;
  cwal_size_t const pos = se->buffer.used;
  int rc;
  char const addSep = args->argc ? cwal_value_get_bool(args->argv[0]) : 0;
  if( (rc = s2_cb_disable_check(args, S2_DISABLE_FS_STAT)) ) return rc;
  rc = s2_getcwd( args->engine, buf );
  if(rc) rc = cwal_cb_throw(args, rc, "getcwd() failed with code %d (%s).",
                         rc, cwal_rc_cstr(rc));
  else{
    if(addSep){
      rc = cwal_buffer_append( args->engine,
                               buf,
                               S2_DIRECTORY_SEPARATOR,
                               sizeof(S2_DIRECTORY_SEPARATOR)-
                               sizeof(S2_DIRECTORY_SEPARATOR[0]) );
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


const s2_fstat_t s2_fstat_t_empty = s2_fstat_t_empty_m;

/*
   Use _stati64 rather than stat on windows, in order to handle files
   larger than 2GB.
*/
#if defined(S2_OS_WINDOWS) && (defined(__MSVCRT__) || defined(_MSC_VER))
# undef stat
# define stat _stati64
#endif
/*
   On Windows S_ISLNK always returns FALSE.
*/
#if !defined(S_ISLNK)
# define S_ISLNK(x) (0)
#endif

int s2_fstat( char const * filename,
              cwal_size_t fnLen,
              s2_fstat_t * tgt,
              char derefSymlinks ){
#if !S2_HAVE_STAT || defined(S2_OS_WINDOWS)
  if(filename || fnLen || tgt || derefSymlinks){/*unused params*/}
  return CWAL_RC_UNSUPPORTED;
#else
  int rc;
  if(!filename || !tgt) rc = CWAL_RC_MISUSE;
  else if(!*filename || !fnLen) rc = CWAL_RC_RANGE;
  else if(fnLen >= (cwal_size_t)S2_FILENAME_BUF_SIZE) rc = CWAL_RC_RANGE;
  else{
    struct stat buf;
    char isCwalRc = 0;
    char fnBuf[S2_FILENAME_BUF_SIZE] = {0};
    memcpy( fnBuf, filename, (size_t)fnLen );
    fnBuf[fnLen] = 0;
    if( derefSymlinks ){
      rc = stat(fnBuf, &buf);
    }else{
#if S2_HAVE_LSTAT
      rc = lstat(fnBuf, &buf);
#else
      rc = CWAL_RC_UNSUPPORTED;
      isCwalRc = 1;
#endif
    }
    if(rc){
      if(!isCwalRc){
        rc = cwal_errno_to_cwal_rc(errno, CWAL_RC_IO);
      }
    }else if(tgt){
      *tgt = s2_fstat_t_empty;
      tgt->ctime = (uint64_t)buf.st_ctime;
      tgt->mtime = (uint64_t)buf.st_mtime;
      tgt->size = (uint64_t)buf.st_size;
      tgt->perm = buf.st_mode & 0777 /* Unix file permissions are only the bottom 9 bits */;
#define TCHECK(SMACRO, TYPE) if(SMACRO(buf.st_mode)) tgt->type = S2_FSTAT_TYPE_ ## TYPE
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
      else tgt->type = S2_FSTAT_TYPE_UNKNOWN;
#undef TCHECK
    }
  }
  return rc;
#endif
}

int s2_fstat_to_object( s2_engine * se, s2_fstat_t const * fst, cwal_value ** rv ){
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
  keysV = s2_stash_get(se, stashKey);
  if(keysV){
    keysA = cwal_value_get_array(keysV);
  }else{
    /* set up key names cache... */
    cwal_value * key = 0;
    keysV = cwal_new_array_value(se->e);
    if(!keysV){
      rc = CWAL_RC_OOM;
      goto end;
    }
    keysA = cwal_value_get_array(keysV);
    cwal_value_ref(keysV);
    rc = s2_stash_set(se, stashKey, keysV);
    cwal_value_unref(keysV);
    if(rc) goto end;
    else{
      assert(cwal_value_refcount(keysV)>0 && "stash is holding a ref");
    }
    rc = cwal_array_reserve(keysA, (cwal_size_t)keyEND);
    if(rc) goto end;
#define KEY(TYPE,STR)                                                      \
    key = cwal_new_string_value(se->e, STR, cwal_strlen(STR));   \
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

  obj = cwal_new_object_value(se->e);
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
  v = cwal_new_integer(se->e, (cwal_int_t)(IV)); \
  VSET(KEY);
  
  INTVAL(MTIME,fst->mtime);
  INTVAL(CTIME,fst->ctime);
  INTVAL(SIZE,fst->size);
  INTVAL(PERM,fst->perm);
#undef INTVAL
#undef VCHECK
#undef VSET

  switch(fst->type){
#define CASE(TYPE) case S2_FSTAT_TYPE_##TYPE: \
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

int s2_cb_fstat( cwal_callback_args const * args, cwal_value ** rv ){
  int rc = 0;
  s2_fstat_t stbuf = s2_fstat_t_empty;
  cwal_size_t fnLen = 0;
  char const * fn = args->argc
    ? cwal_value_get_cstr( args->argv[0], &fnLen )
    : 0;
  char derefSymlinks = 1;
  char doQuickCheck = 0;
  if( (rc = s2_cb_disable_check(args, S2_DISABLE_FS_STAT)) ) return rc;
  *rv = 0;
  if(!fn) {
    rc = cwal_cb_throw( args, CWAL_RC_MISUSE,
                     "Expecting string (filename) argument.");
    goto end;
  }
  if(args->argc>1){
    if(cwal_value_undefined()==args->argv[1]){
      doQuickCheck = 1;
      if(args->argc>2) derefSymlinks = cwal_value_get_bool(args->argv[2]);
    }else{
      derefSymlinks = cwal_value_get_bool(args->argv[1]);
    }
  }
  assert(!rc);
  rc = s2_fstat( fn, fnLen, &stbuf, derefSymlinks );
  if(rc){
    if(doQuickCheck){
      *rv = cwal_value_false();
      rc = 0;
    }else{
      rc = cwal_cb_throw(args, rc, "stat(%.*s) failed with code %d (%s)",
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
      s2_engine * se = s2_engine_from_args(args);
      assert(se);
      rc = s2_fstat_to_object( se, &stbuf, rv );
      if(rc){
        assert(CWAL_RC_OOM == rc);
      }
    }
  }
  return rc;
}

int s2_chdir( char const * dir, cwal_size_t dirLen ){
#if S2_HAVE_CHDIR
  char fnBuf[S2_FILENAME_BUF_SIZE] = {0};
  int rc = 0;
  if(dirLen >= (cwal_size_t)S2_FILENAME_BUF_SIZE) return CWAL_RC_RANGE;
  memcpy( fnBuf, dir, dirLen );
  fnBuf[dirLen] = 0;
  if(chdir(fnBuf)) rc = cwal_errno_to_cwal_rc(0, CWAL_RC_IO);
  return rc;
#else
  if(dir && dirLen){/* unused */}
  return CWAL_RC_UNSUPPORTED;
#endif
}

int s2_cb_chdir( cwal_callback_args const * args, cwal_value **rv ){
  cwal_size_t strLen = 0;
  int rc;
  char const * str = args->argc
    ? cwal_value_get_cstr(args->argv[0], &strLen)
    : 0;
  if( (rc = s2_cb_disable_check(args, S2_DISABLE_FS_STAT)) ) return rc;
  else if(!str) rc = cwal_cb_throw(args, CWAL_RC_MISUSE,
                            "Expecting a string (directory) argument.");
  else{
    rc = s2_chdir( str, strLen );
    if(rc) rc = cwal_cb_throw(args, rc,
                            "chdir() failed with code %d (%s).",
                            rc, cwal_rc_cstr(rc));
    else *rv = cwal_value_undefined();
  }
  return rc;  
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

char s2_file_is_accessible( char const * fn, char checkWrite ){
  return (0 == CHECKACCESS( fn, CHECKRIGHTS ));
}

char s2_is_dir( char const * fn, char checkWrite ){
#ifdef S2_OS_UNIX
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
  return 0;
#endif
}

#undef CHECKACCESS
#undef CHECKRIGHTS

static int s2_cb_fs_accessible( int isFile, cwal_callback_args const * args, cwal_value **rv ){
  char const * fn;
  {
    int const rc = s2_cb_disable_check(args, S2_DISABLE_FS_STAT);
    if( rc ) return rc;
  }
  fn = args->argc
    ? cwal_value_get_cstr(args->argv[0], 0)
    : 0;
  if(!fn) return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                     "Expecting a string argument.");
  else{
    char const checkWrite = (args->argc>1)
      ? cwal_value_get_bool(args->argv[1])
      : 0;
    assert(fn);
    *rv = (isFile ? s2_file_is_accessible(fn, checkWrite) : s2_is_dir(fn, checkWrite))
      ? cwal_value_true()
      : cwal_value_false();
    return 0;
  }
}

int s2_cb_file_accessible( cwal_callback_args const * args, cwal_value **rv ){
  return s2_cb_fs_accessible(1, args, rv);
}

int s2_cb_dir_accessible( cwal_callback_args const * args, cwal_value **rv ){
  return s2_cb_fs_accessible(0, args, rv);
}


int s2_cb_realpath( cwal_callback_args const * args, cwal_value **rv ){
#if !S2_HAVE_REALPATH
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
  int rc;
  if( (rc = s2_cb_disable_check(args, S2_DISABLE_FS_STAT)) ) return rc;
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

void s2_fclose(FILE *f){
  if(f
     && f!=stdin
     && f!=stdout
     && f!=stderr){
    fclose(f);
  }
}

FILE *s2_fopen(const char *zName, const char *zMode){
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



int s2_passthrough_FILE( cwal_engine * e, FILE * file ){
  return cwal_stream( cwal_input_f_FILE, file,
                      cwal_output_f_cwal_engine, e );
}

int s2_passthrough_filename( cwal_engine * e, char const * filename ){
  int rc;
  FILE * f = s2_fopen(filename, "r");
  if(!f) rc = CWAL_RC_IO;
  else{
    rc = s2_passthrough_FILE(e, f);
    s2_fclose(f);
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
static int s2_cb_file_passthrough( cwal_callback_args const * args,
                                   cwal_value **rv ){
  char const * fn;
  cwal_size_t len;
  fn = args->argc
    ? cwal_value_get_cstr(args->argv[0], &len)
    : NULL;
  if(!fn){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting non-empty string "
                       "argument.");
  }else if(!s2_file_is_accessible(fn, 0)){
    return cwal_cb_throw(args, CWAL_RC_NOT_FOUND,
                       "Cannot find file: %s", fn);
  }else{
    FILE * fi = s2_fopen(fn, "r");
    int rc;
    if(!fi){
      rc = cwal_cb_throw(args, CWAL_RC_IO,
                       "Could not open file for reading: %s",
                       fn);
    }else{
      rc = cwal_stream( cwal_input_f_FILE, fi,
                        cwal_output_f_cwal_engine,
                        args->engine );
      s2_fclose(fi);
    }
    if(!rc) *rv = args->self;
    return rc;
  }
}

int s2_install_fs( s2_engine * se, cwal_value * tgt,
                   char const * name ){
  cwal_value * v;
  cwal_value * sub;
  int rc;
  if(!se || !tgt) return CWAL_RC_MISUSE;
  else if(!cwal_props_can(tgt)) return CWAL_RC_TYPE;

  if(name && *name){
    sub = cwal_new_object_value(se->e);
    if(!sub) return CWAL_RC_OOM;
    if( (rc = cwal_prop_set(tgt, name, cwal_strlen(name), sub)) ){
      cwal_value_unref(sub);
      return rc;
    }
  }else{
    sub = tgt;
  }

  {
    s2_func_def const funcs[] = {
      S2_FUNC2("stat", s2_cb_fstat),
      S2_FUNC2("realpath", s2_cb_realpath),
      S2_FUNC2("passthrough", s2_cb_file_passthrough),
      S2_FUNC2("mkdir", s2_cb_mkdir),
      S2_FUNC2("getcwd", s2_cb_getcwd),
      S2_FUNC2("fileIsAccessible", s2_cb_file_accessible),
      S2_FUNC2("dirIsAccessible", s2_cb_dir_accessible),
      S2_FUNC2("chdir", s2_cb_chdir),
      s2_func_def_empty_m
    };
    rc = s2_install_functions(se, sub, funcs, 0);
    if(rc) goto end;
  }

#define SET(NAME)                                                    \
  if(!v) { rc = CWAL_RC_OOM; goto end; }                             \
  cwal_value_ref(v);                                                 \
  rc = cwal_prop_set( sub, NAME, cwal_strlen(NAME), v );             \
  cwal_value_unref(v);                                               \
  v = 0;                                                             \
  if(rc) goto end;

  v = cwal_new_string_value(se->e,
                            S2_DIRECTORY_SEPARATOR,
                            sizeof(S2_DIRECTORY_SEPARATOR)-
                            sizeof(S2_DIRECTORY_SEPARATOR[0]) );
  SET("dirSeparator");

  end:
#undef SET
  return rc;
}

#undef MARKER
