/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

/**
   A basic s2 binding for the C89 FILE class.
*/

#include "libs2.h"
#include <assert.h>
#include <memory.h> /* strerror() */

#include <sys/types.h>
#include <stdio.h>
#include <errno.h>

/* #if defined(_POSIX_C_SOURCE) && (_POSIX_C_SOURCE>=200101) */
#if defined(S2_OS_UNIX)
#  include <unistd.h>
#  define ENABLE_UNLINK 1
#else
#  define ENABLE_UNLINK 0
#endif

#if 0
/* only for debuggering */
#include <stdio.h>
#define MARKER(pfexp)                                               \
  do{ printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__);   \
    printf pfexp;                                                   \
  } while(0)
#endif

/* For these type IDs, the values are irrelevant - we use only the pointer addresses... */
static const int s2_FILE_typeid = 42;
static const int s2_FILE_prototype_id = 43;
#define THIS_FILE                                                   \
  cwal_native * nself = cwal_value_get_native(args->self);          \
  FILE * self = nself                                                \
    ? (FILE *)cwal_native_get(nself, &s2_FILE_typeid) : NULL;      \
  if(!self) return cwal_cb_throw(args, CWAL_RC_TYPE,                  \
                               "'this' is not (or is no longer) "   \
                               "a FILE instance.")

/** cwal_finalizer_f() impl for FILE handles. */
static void s2_FILE_close_finalizer( cwal_engine * e, void * v ){
  if(v){
    FILE * f = (FILE*)v;
    /* MARKER(("fclose(FILE @%p)\n", v)); */
    if(stdin != f && stdout != f && stderr != f){
      fclose( f );
    }
#if 0
    else{
      MARKER(("fclose(FILE @%p) skipping standard handle.\n", v));
    }
#endif
  }else if(e){/*avoid unused param warning*/}
}

/**
   Script usage: aFile.close()

   Disconnects the native FILE from script-space.

   Returns void.
*/
static int s2_FILE_cb_close( cwal_callback_args const * args,
                             cwal_value **rv ){
  THIS_FILE;
  cwal_native_clear( nself, 1 );
  *rv = cwal_value_undefined();
  return 0;
}

/**
   Script usage:

   var pos = aFile.tell()

   Returns an integer (the file's current cursor position).
*/
static int s2_FILE_cb_tell( cwal_callback_args const * args,
                              cwal_value **rv ){
  THIS_FILE;
  *rv = cwal_new_integer(args->engine,
                         (cwal_int_t)ftell(self));
  return *rv ? 0 : CWAL_RC_OOM;
}

/**
   Script usage:

   aFile.seek( offset, [whence = SEEK_SET] )

   where whence is one of (seek.SEEK_SET, seek.SEEK_CUR,
   seek.SEEK_END) and offset is a position/distance relative to
   whence.

   Returns this object, throws on error.
*/
static int s2_FILE_cb_seek( cwal_callback_args const * args,
                            cwal_value **rv ){
  int whence = 0;
  long offset = 0;
  int rc = 0;
  THIS_FILE;
  if(!args->argc){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting (offset [, whence]) arguments.");
  }
  offset = (long)cwal_value_get_integer(args->argv[0]);
  whence = args->argc>1
    ? (int)cwal_value_get_integer(s2_value_unwrap(args->argv[1]))
    : SEEK_SET;
  switch(whence){
    case SEEK_SET:
    case SEEK_CUR:
    case SEEK_END:
      break;
    default:
      rc = cwal_cb_throw(args, CWAL_RC_RANGE,
                       "Expecting one of SEEK_SET, SEEK_CUR, SEEK_END for argv[1].");
  }
  if( !rc && fseek(self, offset, whence) ){
    rc = cwal_cb_throw(args, cwal_errno_to_cwal_rc(errno, CWAL_RC_IO),
                     "fseek(FILE, offset=%ld, whence=%d) failed with errno %d (%s).",
                     offset, whence, errno, strerror(errno));
  }
  if(!rc) *rv = args->self;
  return rc;
}

/**
   Script usage:

   aFile.rewind()

   Sets the file's cursor to the start of the file and (according to
   my man pages) resets the error flag (but it's inconsistent on that point,
   as the ferror() man page says that clearerr() is the only way to reset
   the error flag).

   Returns this object.
*/
static int s2_FILE_cb_rewind( cwal_callback_args const * args,
                              cwal_value **rv ){
  THIS_FILE;
  rewind(self);
  *rv = args->self;
  return 0;
}

/**
   Script usage:

   var isAtEof = aFile.eof()

   Returns true at EOF, else false.

   Interestingly, a newly-opened file is not at EOF.
*/
static int s2_FILE_cb_feof( cwal_callback_args const * args,
                             cwal_value **rv ){
  THIS_FILE;
  *rv = feof(self) ? cwal_value_true() : cwal_value_false();
  return 0;
}

/**
   Script usage:

   var hasError = aFile.hasError()

   Returns true if aFile has an error flag, else false.
*/
static int s2_FILE_cb_ferror( cwal_callback_args const * args,
                              cwal_value **rv ){
  THIS_FILE;
  *rv = ferror(self) ? cwal_value_true() : cwal_value_false();
  return 0;
}

/**
   Script usage:

   aFile.clearError()

   Clears any error and EOF flags.

   Returns this object.
*/
static int s2_FILE_cb_clearerr( cwal_callback_args const * args,
                                cwal_value **rv ){
  THIS_FILE;
  clearerr(self);
  *rv = args->self;
  return 0;
}

/**
   Uses fseek()/ftell() to fetch fh's size. On success the size is
   written to *len. On error e's exception state is set.

   Note that fseek() resets the EOF marker!

   Yes, this only supports long-sized files, but that's okay for the
   scope of s2.
*/
static int s2_fsize( cwal_engine * e, FILE * fh, cwal_int_t * len ){
  long const oldPos = ftell(fh);
  long fl;
  int theErr = 0;
  fseek( fh, 0, SEEK_END );
  fl = ftell( fh );
  if(fl < 0){
    theErr = errno;
  }else{
    *len = (cwal_int_t)fl;
  }
  fseek( fh, oldPos, SEEK_SET );
  return theErr
    ? cwal_exception_setf(e, cwal_errno_to_cwal_rc(theErr, CWAL_RC_IO),
                          "ftell() failed with errno %d (%s).",
                          theErr, strerror(theErr))
    : 0;
}


/**
   Script usage:

   var len = aFile.size();

   Note that this uses fseek() and therefore resets the EOF flag!
   It restores the file's cursor on success, so there are no visible
   side effects except that the EOF flag gets removed.

   Throws on I/O error.
*/
static int s2_FILE_cb_fsize( cwal_callback_args const * args,
                             cwal_value **rv ){
  cwal_int_t fsz;
  int rc;
  THIS_FILE;
  rc = s2_fsize(args->engine, self, &fsz);
  if(!rc){
    if(!(*rv = cwal_new_integer(args->engine, fsz))){
      rc = CWAL_RC_OOM;
    }
  }
  return rc;
}

/**
   Script signatures:

   FILE aFile.read([int byteCount=-1,] Buffer dest])
   Buffer aFile.read([int byteCount=-1]);

   Reads byteCount byte(s) into the destination Buffer, creating a new
   Buffer if one is not passed one. If passed a buffer, it appends to
   that buffer rather than replacing its contents.

   If byteCount is -1 then the whole input file is read. byteCount of
   0 or <-1 is invalid.

   Note that the input length is in bytes, not characters, so when
   reading a text file it may read a partial UTF-8 character. Thus it
   is not generically safe to use toString()/takeString() on the
   destination buffer unless the input is known to be complete UTF
   or 100% ASCII.

   Returns either this object or a new Buffer, depending on its
   arguments. Throws on error.
*/
static int s2_FILE_cb_read( cwal_callback_args const * args,
                            cwal_value **rv ){
  int rc = 0;
  cwal_buffer * buf = 0;
  cwal_value * vbuf = 0;
  cwal_value * rvVal = 0;
  int targetArgIndex = -1;
  cwal_int_t howMuch = 0;
  THIS_FILE;
  if(1==args->argc){
    if(cwal_value_is_buffer(args->argv[0])){
      targetArgIndex = 0;
      howMuch = -1;
    }else{
      howMuch = cwal_value_get_integer(args->argv[0]);
    }
  }else if(args->argc){
    howMuch = cwal_value_get_integer(args->argv[0]);
    targetArgIndex = cwal_value_is_buffer(args->argv[1]) ? 1 : -1;
  }else{
    howMuch = -1;
  }
    
  if(!howMuch || howMuch<-1){
    rc = cwal_cb_throw(args, CWAL_RC_RANGE,
                     "Expecting an integer of -1 or >0 for argv[0].");
    /* We might want to allow a zero-byte read, as i once saw it used
       as a way of confirming read access to a file. */
    goto end;
  }
  if(targetArgIndex>=0){
    /* Use client-supplied target buffer... */
    vbuf = args->argv[(uint16_t)targetArgIndex];
    assert(vbuf);
    buf = cwal_value_get_buffer(vbuf);
    if(!buf){
      rc = cwal_cb_throw(args, CWAL_RC_RANGE, "Expecting a Buffer for argv[%d].",
                       targetArgIndex);
      vbuf = 0 /* avoid unreffing it below */;
      goto end;
    }
    rvVal = args->self;
  }else{
    /* Create a new target buffer... */
    cwal_size_t reserve = 0;
    if(howMuch>0){
      reserve = (cwal_size_t)howMuch;
    }else{
      cwal_int_t flen = 0;
      rc = s2_fsize(args->engine, self, &flen);
      if(rc) goto end;
      if(flen>0) reserve = (cwal_size_t)flen;
    }
    vbuf = cwal_new_buffer_value(args->engine, reserve + 1/* automatic NUL byte */);
    if(!vbuf){
      rc = CWAL_RC_OOM;
      goto end;
    }
    buf = cwal_value_get_buffer(vbuf);
    assert(buf);
    assert(!buf->used);
    rvVal = vbuf;
  }
  cwal_value_ref(vbuf);
  assert(0==rc);
  {
    /** Read the input in chunks... */
    enum { BufSize = 1024 * 4 };
    unsigned char cbuf[BufSize];
    cwal_size_t total = 0;
    cwal_size_t nRemaining = (cwal_size_t)howMuch
      /*underflow is okay: we use it to mean "read it all" */;
    cwal_size_t rdn = (nRemaining <= (cwal_size_t)BufSize)
      ? nRemaining : (cwal_size_t)BufSize;
    while( 0==(rc=cwal_input_f_FILE(self, cbuf, &rdn)) ){
      if(rdn){
        total += rdn;
        rc = cwal_buffer_append( args->engine, buf, cbuf, rdn);
        if(rc) break;
        nRemaining -= rdn;
        if(feof(self) || !nRemaining) break;
        rdn = (nRemaining <= (cwal_size_t)BufSize)
          ? nRemaining : (cwal_size_t)BufSize;
      }
      else break;
    }
    if(!rc && nRemaining>0 && !feof(self)){
      rc = cwal_cb_throw(args, CWAL_RC_IO,
                       "fread() failed (short read but not EOF).");
    }
  }
  end:
  if(rc){
    cwal_value_unref(vbuf);
  }else{
    *rv = rvVal;
    cwal_value_unhand(vbuf);
  }
  return rc;
}


/**
   Script usage:

   FILE aFile.write(Buffer | string [, ... Buffer | string])

   Writes the whole contents of the given buffer or string to the
   file.

   Returns this object. Throws on error.

   Potential TODO: stringify non-string arguments. That would,
   however, mean that this class cannot write binary numbers and
   whatnot unless they were first encoded into a Buffer and that
   Buffer gets written via this API.
*/
static int s2_FILE_cb_write( cwal_callback_args const * args,
                             cwal_value **rv ){
  int rc = 0;
  uint16_t i;
  cwal_value * what = 0;
  cwal_buffer * buf = 0;
  void const * src = 0;
  cwal_size_t srcLen = 0;
  THIS_FILE;
  for( i = 0; i < args->argc; ++i ){
    what = args->argc ? args->argv[i] : 0;
    if(!what) goto err_type;
    else if((buf = cwal_value_get_buffer(what))){
      src = buf->mem;
      srcLen = buf->used;
    }else if((src = cwal_value_get_cstr(what, &srcLen))){
      /* nothing else to do */
    }else{
      goto err_type;
    }
    if(1 != fwrite( src, (size_t)srcLen, 1, self)){
      rc = cwal_cb_throw(args, CWAL_RC_IO, "fwrite() of %u bytes failed.",
                       (unsigned)srcLen);
    }
  }
  goto end;
  err_type:
  rc = cwal_cb_throw(args, CWAL_RC_TYPE, "Expecting a string or buffer argument.");
  end:
  if(!rc){
    *rv = args->self;
  }
  return rc;
}

/**
   Script usage:

   FILE aFile.flush()

   Returns this object. Throws on i/o error.
 */
static int s2_FILE_cb_flush( cwal_callback_args const * args,
                             cwal_value **rv ){
  int rc = 0;
  THIS_FILE;
  if(fflush(self)){
    rc = cwal_cb_throw(args, cwal_errno_to_cwal_rc(errno, CWAL_RC_IO),
                     "fflush(FILE) failed with errno %d (%s).",
                     errno, strerror(errno));
  }
  *rv = args->self;
  return rc;
}


/**
   Script-side usage:

   aFile.unlink()

   Effectively calls thisModule.unlink(aFile.name).

   Returns this object, throws on error.

   Throws if unlink() is disabled in this build or if the unlink()
   fails (e.g. if the file was already unlink()ed). Note that the ability
   to unlink a file is not available on all platforms.
*/
static int s2_FILE_cb_unlinkself( cwal_callback_args const * args,
                                  cwal_value **rv ){
#if ENABLE_UNLINK
  char const * fn = NULL;
  cwal_size_t fnLen = 0;
  int rc = 0;
  cwal_value * vself;
  THIS_FILE;
  vself = cwal_native_value(nself);
  fn=cwal_value_get_cstr(cwal_prop_get(vself, "name", 4), &fnLen);
  if(!fn){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "FILE handle is missing 'name' property.");
  }
  if(unlink(fn)){
    rc = cwal_cb_throw(args, cwal_errno_to_cwal_rc(errno, CWAL_RC_IO),
                     "unlink(%.*s) failed with errno %d (%s).",
                     (int)fnLen, fn, errno, strerror(errno));
  }else{
    *rv = args->self;
  }
  return rc;
#else
  return cwal_cb_throw(args, CWAL_RC_UNSUPPORTED,
                     "unlink() is not available in this build.");
#endif
}

/**
   Returns a new instnace of the Prototype FILE object, or NULL
   on error.
*/
static cwal_value * s2_FILE_prototype( s2_engine * se ){
  int rc = 0;
  cwal_value * proto = 0;
  char const * pKey = "FILE";
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
    S2_FUNC2("clearError", s2_FILE_cb_clearerr),
    S2_FUNC2("close", s2_FILE_cb_close),
    S2_FUNC2("eof", s2_FILE_cb_feof),
    S2_FUNC2("flush", s2_FILE_cb_flush),
    S2_FUNC2("hasError", s2_FILE_cb_ferror),
    S2_FUNC2("operator<<", s2_FILE_cb_write),
    S2_FUNC2("read", s2_FILE_cb_read),
    S2_FUNC2("rewind", s2_FILE_cb_rewind),
    S2_FUNC2("seek", s2_FILE_cb_seek),
    S2_FUNC2("size", s2_FILE_cb_fsize),
    S2_FUNC2("tell", s2_FILE_cb_tell),
    S2_FUNC2("unlink", s2_FILE_cb_unlinkself),
    S2_FUNC2("write", s2_FILE_cb_write),
    s2_func_def_empty_m
    };
    rc = s2_install_functions( se, proto, funcs, 0 );
  }

  if(!rc){
    cwal_value * seek = cwal_prop_get(proto, "seek", 4);
    assert(seek && "we JUST installed this!");
    rc = cwal_prop_set(seek, "SEEK_SET", 8, cwal_new_integer(se->e, SEEK_SET));
    if(!rc) cwal_prop_set(seek, "SEEK_CUR", 8, cwal_new_integer(se->e, SEEK_CUR));
    if(!rc) cwal_prop_set(seek, "SEEK_END", 8, cwal_new_integer(se->e, SEEK_END));
    /* reminder: those values are built-in constants, so allocation of
       the ints cannot fail, but allocation of the keys can. Since
       this module init is called from a scope, any such "leaks" will
       get cleaned up as soon as this function returns. */
  }
  
  end:
  if(rc) cwal_value_unref(proto);
  else cwal_value_unhand(proto);
  return rc ? NULL : proto;
}

/**
   Script-side usage:

   unlink(string filename [, bool reportError=true])

   If reportError is true (the default) then it throws if
   it cannot unlink the file, otherwise it returns false
   on error.

   Returns true if the underlying call to unlink() succeeds.

   Throws if unlink() is disabled in this build.
*/
static int s2_FILE_cb_unlink( cwal_callback_args const * args,
                              cwal_value **rv ){
#if ENABLE_UNLINK
  char const * fn = NULL;
  cwal_size_t fnLen = 0;
  int rc = 0;
  if(!args->argc || !(fn=cwal_value_get_cstr(args->argv[0], &fnLen))){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting a string 'filename' argument.");
  }
  if(unlink(fn)){
    if(args->argc<2 || cwal_value_get_bool(args->argv[1])){
      rc = cwal_cb_throw(args, cwal_errno_to_cwal_rc(errno, CWAL_RC_IO),
                       "unlink(%.*s) failed with errno %d (%s).",
                       (int)fnLen, fn, errno, strerror(errno));
    }else{
      *rv = cwal_value_false();
    }
  }else{
    *rv = cwal_value_true();
  }
  return rc;
#else
  return cwal_cb_throw(args, CWAL_RC_UNSUPPORTED,
                     "unlink() is not available in this build.");
#endif
}

/**
   script-side usage:

   var aFile = FILE.open(string filename [, string mode = "rb"])

   Or as a ctor:

   var aFile = new FILE(...same as for open()...)

   The open-mode string is as defined for fopen(3).

   Throws if given invalid arguments or if it cannot open resp. create
   the given file.

   The returned FILE object has its "name" property set to the exact
   name which was passed to this function (it is not
   resolved/normalized by this function).

   Special cases: the file names :stdin:, :stdout:, and :stderr: all
   refer to the standard I/O channels. The open-mode argument is
   ignored for those streams: stdin/stdout are output-only and stdin
   is read-only. None of them are seek()able. Their close() methods
   disconnect the script binding from the stream but do not close the
   streams. A functionally equivalent effect can be had on many
   Unix-like systems by using the device names /dev/stdout,
   /dev/stderr, and /dev/stdin.

   It is tempting to add the special name :cwal: to refer to cwal's
   default output channel, but (A) there really is no need for it and
   (B) it would require a great deal of special-casing, as that
   channel is abstract, not necessarily bound to a FILE handle (and if
   it is bound to a FILE, we can't see that at this level of the API).
*/
static int s2_FILE_cb_open( cwal_callback_args const * args,
                              cwal_value **rv ){
  char const * fn = NULL;
  FILE * fh = 0;
  char const * mode = 0;
  cwal_value * rvv;
  int rc = 0;
  cwal_value * proto =
    (cwal_value *)cwal_args_state(args, &s2_FILE_prototype_id);
  if(!proto){
    return cwal_cb_throw(args, CWAL_RC_UNSUPPORTED,
                       "FILE prototype is missing. Make sure this "
                       "module is not both statically and "
                       "dynamically linked!");
  }
  assert(cwal_value_refcount(proto)>0);
  if(!args->argc || !(fn=cwal_value_get_cstr(args->argv[0], NULL))){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting a string 'filename' argument.");
  }
  if(args->argc>1){
    mode = cwal_value_get_cstr(args->argv[1], NULL);
    if(!mode){
      return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                 "Expecting a string 'mode' argument.");
    }
  }else{
    mode = "rb";
  }
  if(':' == *fn && 's' == fn[1]){
    /* Special case stdin/stdout/stderr... */
    if(0==strcmp(":stdin:", fn)){
      fh = stdin;
      mode = "rb";
    }else if(0==strcmp(":stdout:", fn)){
      fh = stdout;
      mode = "ab";
    }else if(0==strcmp(":stderr:", fn)){
      fh = stderr;
      mode = "ab";
    }
  }
  if(!fh){
    fh = fopen( fn, mode );
  }
  if(!fh){
    return cwal_exception_setf(args->engine,
                               cwal_errno_to_cwal_rc( errno, CWAL_RC_IO ),
                               "fopen(%s, %s) failed. errno=%d (%s)\n",
                               fn, mode, errno, strerror(errno));
  }
  rvv = cwal_new_native_value(args->engine, fh,
                              s2_FILE_close_finalizer,
                              &s2_FILE_typeid);
  if(rvv){
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

/**
   Module init routine.
*/
static int s2_module_init_FILE( s2_engine * se, cwal_value ** rv ){
  cwal_value * proto;
  proto = s2_FILE_prototype(se);
  if(!proto) return CWAL_RC_OOM;
  else{
    int rc = 0;
    cwal_value * mod = cwal_new_object_value(se->e);
    s2_func_def funcs[] = {
      S2_FUNC2("open", s2_FILE_cb_open)
      /* open() MUST be first for custom init below to work. */,
      S2_FUNC2("unlink", s2_FILE_cb_unlink),
      s2_func_def_empty_m
    };
    if(!mod) return CWAL_RC_OOM;

    /* Stash the prototype in open(), noting that that is a
       (void*) stash, not a (cwal_value*) stash, meaning that
       there is still a lifetime-level issue to solve (see
       below). */
    funcs[0].state = proto;
    funcs[0].stateTypeID = &s2_FILE_prototype_id;

    cwal_value_ref(mod);
    cwal_value_ref(proto);
    rc = s2_install_functions( se, mod, funcs, 0 );
    if(!rc){
      /* The problem: stashing the prototype in a func's state does
         not account for rescoping and vacuum safety and whatnot. Thus
         we need to stash it as a property in the function and hope
         that no client calls clearProperties() on the function (which
         would wipe out the prototype object but leave its stale
         (void*) reference in funcs[0].state.
      */
      cwal_value * open = cwal_prop_get(mod, "open", 4);
      assert(open && "we JUST stashed this!");
      rc = s2_stash_hidden_member(open, proto);
      if(!rc){
        rc = s2_ctor_method_set(se, mod, cwal_value_get_function(open));
      }
    }
    cwal_value_unref(proto)
      /* on success we still have a ref via s2_stash_hidden_member() */;
    if(rc){
      cwal_value_unref(mod);
    }else{
      cwal_value_unhand(mod);
      *rv = mod;
    }
    return rc;
  }
}

S2_MODULE_REGISTER_(FILE);

#undef MARKER
#undef THIS_FILE
#undef ENABLE_UNLINK
