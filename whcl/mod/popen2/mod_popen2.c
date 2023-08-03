/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/*
** Copyright (c) 2010 D. Richard Hipp
**
** This program is free software; you can redistribute it and/or
** modify it under the terms of the Simplified BSD License (also
** known as the "2-Clause License" or "FreeBSD License".)

** This program is distributed in the hope that it will be useful,
** but without any warranty; without even the implied warranty of
** merchantability or fitness for a particular purpose.
**
** Author contact information:
**   drh@hwaci.com
**   http://www.hwaci.com/drh/
**
************************************************************************
**
** This file contains an implementation of a bi-directional popen().
**
************************************************************************
** THIS copy was derived from the fossil-scm source tree
** (http://fossil-scm.org) for use as a cwal/whcl add-on
** (http://fossil.wanderinghorse.net/r/cwal) by
** Stephan Beal (stephan@wanderinghorse.net).
*/
#include "libwhcl.h"
#if !defined(_POSIX_C_SOURCE)
#  error "Expecting _POSIX_C_SOURCE to be defined by this point."
#endif

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#ifdef _WIN32
#include <windows.h>
#include <fcntl.h>
/*
** Print a fatal error and quit.
*/
static void win32_fatal_error(const char *zMsg){
  printf("%s", zMsg);
  abort();
}
#else
#include <unistd.h>
#include <sys/types.h> /* kill() */
#include <signal.h> /* kill() */
#endif

#include <stdio.h>
#if 1
#define MARKER(pfexpr) if(1) printf("MARKER: %s:%d:\t",__FILE__,__LINE__); if(1) printf pfexpr
#else
#define MARKER if(0) printf
#endif

/*
** The following macros are used to cast pointers to integers and
** integers to pointers.  The way you do this varies from one compiler
** to the next, so we have developed the following set of #if statements
** to generate appropriate macros for a wide range of compilers.
**
** The correct "ANSI" way to do this is to use the intptr_t type. 
** Unfortunately, that typedef is not available on all compilers, or
** if it is available, it requires an #include of specific headers
** that vary from one machine to the next.
**
** This code is copied out of SQLite.
*/
#if defined(__PTRDIFF_TYPE__)  /* This case should work for GCC */
# define INT_TO_PTR(X)  ((void*)(__PTRDIFF_TYPE__)(X))
# define PTR_TO_INT(X)  ((int)(__PTRDIFF_TYPE__)(X))
#elif !defined(__GNUC__)       /* Works for compilers other than LLVM */
# define INT_TO_PTR(X)  ((void*)&((char*)0)[X])
# define PTR_TO_INT(X)  ((int)(((char*)X)-(char*)0))
#elif defined(HAVE_STDINT_H)   /* Use this case if we have ANSI headers */
# define INT_TO_PTR(X)  ((void*)(intptr_t)(X))
# define PTR_TO_INT(X)  ((int)(intptr_t)(X))
#else                          /* Generates a warning - but it always works */
# define INT_TO_PTR(X)  ((void*)(X))
# define PTR_TO_INT(X)  ((int)(X))
#endif


#ifdef _WIN32
/*
** On windows, create a child process and specify the stdin, stdout,
** and stderr channels for that process to use.
**
** Return the number of errors.
*/
static int win32_create_child_process(
  wchar_t *zCmd,       /* The command that the child process will run */
  HANDLE hIn,          /* Standard input */
  HANDLE hOut,         /* Standard output */
  HANDLE hErr,         /* Standard error */
  DWORD *pChildPid     /* OUT: Child process handle */
){
  STARTUPINFOW si;
  PROCESS_INFORMATION pi;
  BOOL rc;

  memset(&si, 0, sizeof(si));
  si.cb = sizeof(si);
  si.dwFlags = STARTF_USESTDHANDLES;
  SetHandleInformation(hIn, HANDLE_FLAG_INHERIT, TRUE);
  si.hStdInput  = hIn;
  SetHandleInformation(hOut, HANDLE_FLAG_INHERIT, TRUE);
  si.hStdOutput = hOut;
  SetHandleInformation(hErr, HANDLE_FLAG_INHERIT, TRUE);
  si.hStdError  = hErr;
  rc = CreateProcessW(
     NULL,  /* Application Name */
     zCmd,  /* Command-line */
     NULL,  /* Process attributes */
     NULL,  /* Thread attributes */
     TRUE,  /* Inherit Handles */
     0,     /* Create flags  */
     NULL,  /* Environment */
     NULL,  /* Current directory */
     &si,   /* Startup Info */
     &pi    /* Process Info */
  );
  if( rc ){
    CloseHandle( pi.hProcess );
    CloseHandle( pi.hThread );
    *pChildPid = pi.dwProcessId;
  }else{
    win32_fatal_error("cannot create child process");
  }
  return rc!=0;
}
#endif

/*
** Create a child process running shell command "zCmd".  *ppOut is
** a FILE that becomes the standard input of the child process.  
** (The caller writes to *ppOut in order to send text to the child.)
** *ppIn is stdout from the child process.  (The caller
** reads from *ppIn in order to receive input from the child.)
** Note that *ppIn is an unbuffered file descriptor, not a FILE.
** The process ID of the child is written into *pChildPid.
**
** Returns 0 on success or a cwal approximation of the corresponding
** errno on error.
*/
static int popen2(const char *zCmd, int *pfdIn, FILE **ppOut, int *pChildPid){
#ifdef _WIN32
#error this port is incomplete - see the call to fossil_utf8_to_unicode()
  HANDLE hStdinRd, hStdinWr, hStdoutRd, hStdoutWr, hStderr;
  SECURITY_ATTRIBUTES saAttr;    
  DWORD childPid = 0;
  int fd;

  saAttr.nLength = sizeof(saAttr);
  saAttr.bInheritHandle = TRUE;
  saAttr.lpSecurityDescriptor = NULL; 
  hStderr = GetStdHandle(STD_ERROR_HANDLE);
  if( !CreatePipe(&hStdoutRd, &hStdoutWr, &saAttr, 4096) ){
    win32_fatal_error("cannot create pipe for stdout");
  }
  SetHandleInformation( hStdoutRd, HANDLE_FLAG_INHERIT, FALSE);

  if( !CreatePipe(&hStdinRd, &hStdinWr, &saAttr, 4096) ){
    win32_fatal_error("cannot create pipe for stdin");
  }
  SetHandleInformation( hStdinWr, HANDLE_FLAG_INHERIT, FALSE);
  
  win32_create_child_process(fossil_utf8_to_unicode(zCmd),
                             hStdinRd, hStdoutWr, hStderr,&childPid);
  *pChildPid = childPid;
  *pfdIn = _open_osfhandle(PTR_TO_INT(hStdoutRd), 0);
  fd = _open_osfhandle(PTR_TO_INT(hStdinWr), 0);
  *ppOut = _fdopen(fd, "w");
  CloseHandle(hStdinRd); 
  CloseHandle(hStdoutWr);
  return 0;
#else
  int pin[2], pout[2];
  *pfdIn = 0;
  *ppOut = 0;
  *pChildPid = 0;

  if( pipe(pin)<0 ){
    return cwal_errno_to_cwal_rc(errno, CWAL_RC_IO);
  }
  if( pipe(pout)<0 ){
    close(pin[0]);
    close(pin[1]);
    return cwal_errno_to_cwal_rc(errno, CWAL_RC_IO);
  }
  *pChildPid = fork();
  if( *pChildPid<0 ){
    close(pin[0]);
    close(pin[1]);
    close(pout[0]);
    close(pout[1]);
    *pChildPid = 0;
    return cwal_errno_to_cwal_rc(errno, CWAL_RC_ERROR);
  }
  if( *pChildPid==0 ){
    int fd;
    int nErr = 0;
    /* This is the child process */
    close(0);
    fd = dup(pout[0]);
    if( fd!=0 ) nErr++;
    close(pout[0]);
    close(pout[1]);
    close(1);
    fd = dup(pin[1]);
    if( fd!=1 ) nErr++;
    close(pin[0]);
    close(pin[1]);
    execl("/bin/sh", "/bin/sh", "-c", zCmd, (char*)0);
    return cwal_errno_to_cwal_rc(errno, CWAL_RC_ERROR);
  }else{
    /* This is the parent process */
    close(pin[1]);
    *pfdIn = pin[0];
    close(pout[0]);
    *ppOut = fdopen(pout[1], "w");
    return 0;
  }
#endif
}

/*
** Close the connection to a child process previously created using
** popen2().  Kill off the child process, then close the pipes.
*/
static void pclose2(int fdIn, FILE *pOut, int childPid){
#ifdef _WIN32
  /* Not implemented, yet */
  if(fdIn>=0) close(fdIn);
  if(pOut) fclose(pOut);
#else
  if(fdIn>=0) close(fdIn);
  if(pOut) fclose(pOut);
  if(childPid>=0) kill(childPid, SIGINT);
#endif
}



/************************************************************************
End of imported bits. What follows is the whcl binding for it.
************************************************************************/

struct popen_native {
    FILE * fOut;
    int fdIn;
    int pidChild;
    /**
       Flag for triggering flush() in read() if the previous
       op was a write(), to keep the read() from blocking.
     */
    bool lastOpWasWrite;
};
typedef struct popen_native popen_native;
/**
   Serves as a copy-constructable default instance and as a type ID
   pointer for cwal_new_native() and friends.
*/
static const popen_native popen_native_empty = {NULL, -1, -1, false};

#if 0
/**
   Only for debuggering...
*/
#include <stdio.h>
#define MARKER(pfexp)                                               \
  do{ printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__);   \
    printf pfexp;                                                   \
  } while(0)
#endif

static void popen_native_finalize( cwal_engine * e, void * v ){
    popen_native * pn = (popen_native *)v;
#if 0
    MARKER(("Finalizing %s popen() handle @%p\n",
            pn->fdIn>=0 ? "closed" : "opened", (void const *)pn));
#endif
    if(pn->fdIn >= 0){
        pclose2(pn->fdIn, pn->fOut, pn->pidChild);
    }
    cwal_free2( e, pn, sizeof(popen_native) );
}

#define THIS_POPEN \
    cwal_native * nself = cwal_value_get_native(args->self);    \
    popen_native * self = nself \
        ? (popen_native *)cwal_native_get(nself, &popen_native_empty) : NULL; \
    whcl_engine * const el = whcl_engine_from_args(args);      \
    assert(el); \
    if(!el){/*avoid potentially unused var*/} \
    if(!self) return cwal_exception_setf(args->engine, CWAL_RC_TYPE,    \
                                         "'this' is not (or is no longer) " \
                                         "a popen instance.")

/**
   popen2 read(buffer,int N) = read only up to N bytes. A negative N
   means to read it all.

   or:

   int read(buffer) = read the whole stream

   When it reads only part of a file (N>=0 or not specified), it
   returns the number of bytes read, else returns `this`.
*/
static int popen_cb_read( cwal_callback_args const * args,
                          cwal_value **rv ){
    int rc = 0;
    cwal_int_t n;
    cwal_buffer * buf;
    ssize_t rdrc;
    cwal_value * theRv = 0;
    THIS_POPEN;

    if(self->lastOpWasWrite){
        /* necessary to keep this read() blocking */
        self->lastOpWasWrite = 0;
        fflush( self->fOut );
    }

    buf = (args->argc > 0)
      ? cwal_value_buffer_part(args->engine, args->argv[0])
      : NULL;
    n = (args->argc > 1)
      ? cwal_value_get_integer(args->argv[1])
      : -1;
    if(!buf){
        return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                   "Expecting (Buffer [, readLength]) arguments).");
    }
    if(n>0){
        rc = cwal_buffer_reserve( args->engine, buf, buf->used + n + 1);
        if(rc) return rc;
        rdrc = read( self->fdIn, buf->mem + buf->used, (size_t)n );
        if(rdrc>0){
            buf->used += rdrc;
            buf->mem[buf->used] = 0;
        }
    }
    else if(n<0){
        /* Read whole input. This can, if one does not flush(), cause
           it block to block forever waiting on input. TODO: add
           temporary sigint handler during read?
        */
        enum { BufSize = 1024 * 4 };
        char cbuf[BufSize];
        cwal_size_t total = 0;
        do{
            rdrc = read( self->fdIn, cbuf, BufSize );
            if(rdrc>0){
                total += rdrc;
                cwal_buffer_append( args->engine, buf, cbuf, (cwal_size_t)rdrc);
            }
        }
        while((ssize_t)BufSize == rdrc);
        if(rdrc>=0){
          rdrc = total;
          theRv = args->self;
        }
    }
    else{
      assert(0==n);
      rdrc = 0;
    }
    if((ssize_t)-1 == rdrc){
      return cwal_exception_setf(args->engine,
                                 cwal_errno_to_cwal_rc(errno, CWAL_RC_IO),
                                 "read() failed with errno %d (%s).",
                                 errno, strerror(errno));
    }
    *rv = theRv
      ? theRv
      : cwal_new_integer( args->engine, (cwal_int_t)rdrc )
      /* When reading only part of the input, returning anything other
         than the number of read bytes (e.g. the buffer or args->self)
         incremental reading in blocks. When reading the whole input,
         returning self seems like a reasonable thing to do so that we
         can: me.read(...).close(). */;
    return *rv ? 0 : CWAL_RC_OOM;
}

/**
   int write [-close] buffer input [int offset=0 [int N = -1]]

   Write up to N bytes from the input buffer to the stream.  A
   negative N means "all of it". The offset is what byte position in
   the buffer to start at.

   The -close flag means to close the outbound stream at the end of
   the write. If this flag is provided, an input value argument is
   optional.

   Returns the number of bytes written.
*/
static int popen_cb_write( cwal_callback_args const * args,
                          cwal_value **rv ){
    cwal_int_t len;
    cwal_buffer * buf;
    size_t wrrc;
    void const * mem = 0;
    cwal_size_t memLen = 0;
    uint16_t argNdx = 0;
    char const * flag = 0;
    cwal_size_t nFlag = 0;
    bool closeIt = false;
    THIS_POPEN;
    if(!self->fOut){
      return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                 "Output stream is closed.");
    }
    if( whcl_arg_has_flag(args, &argNdx, &flag, &nFlag) ){
      if(6==nFlag && 0==memcmp(flag, "-close", nFlag)) closeIt = true;
      else --argNdx;
    }
    if(argNdx >= args->argc){
      if(closeIt) goto close_it;
      else return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                      "Missing argument(s).");
    }
    buf = cwal_value_buffer_part(args->engine, args->argv[argNdx]);
    if(buf){
      cwal_int_t offset;
      ++argNdx;
      offset = (args->argc > argNdx)
        ? cwal_value_get_integer(args->argv[argNdx++])
        : 0;
      len =  (args->argc > argNdx)
        ? cwal_value_get_integer(args->argv[argNdx++])
        : -1;
      if(offset<0){
        return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                   "Expecting (Buffer, offset >= 0 [, length]) "
                                   "arguments).");
      }
      if((cwal_size_t)offset >= buf->used){
        *rv = cwal_new_integer(args->engine, 0);
        return 0;
      }
      if(len <=0) len = (cwal_int_t)buf->used;
      if( (len+offset) > (cwal_int_t)buf->used){
        len = (cwal_int_t)buf->used - offset;
      }
      mem = buf->mem + offset;
      memLen = (cwal_size_t)len;
    }else if(cwal_value_is_string(args->argv[argNdx])){
      mem = cwal_value_get_cstr(args->argv[argNdx++], &memLen);
      assert(mem);
    }
    if(!mem){
      return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                 "Invalid first argument type (%s): "
                                 "expecting a Buffer or string.",
                                 cwal_value_type_name(args->argv[0]));
    }
    if(memLen){
      self->lastOpWasWrite = 1;
      wrrc = fwrite( mem, memLen, 1, self->fOut);
      if(1 != wrrc){
        return cwal_exception_setf(args->engine,
                                   cwal_errno_to_cwal_rc(errno, CWAL_RC_IO),
                                   "fwrite() of %"CWAL_SIZE_T_PFMT" byte(s) "
                                   "failed with errno %d (%s).",
                                   (cwal_size_t)memLen,
                                   errno, strerror(errno));
      }
    }
    close_it:
    if(closeIt){
      self->lastOpWasWrite = 0;
      fflush(self->fOut);
      pclose2(-1, self->fOut, -1);
      self->fOut = NULL;
    }
    *rv = /*it might indeed be useful to know how many bytes a given
            value translated to. */
      cwal_new_integer( args->engine, (cwal_int_t)memLen );
    return *rv ? 0 : CWAL_RC_OOM;
}

/**
   popen flush()

   Flushes the stream and returns this object.
*/
static int popen_cb_flush( cwal_callback_args const * args,
                           cwal_value **rv ){
    THIS_POPEN;
    if(!self->fOut){
      return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                 "Output stream is closed.");
    }
    int rc = fflush( self->fOut );
    self->lastOpWasWrite = 0;
    *rv = args->self;
    return rc
      ? cwal_exception_setf(args->engine,
                            cwal_errno_to_cwal_rc(errno, CWAL_RC_IO),
                            "fflush() failed with errno=%d (%s)",
                            errno, strerror(errno))
      : 0;
}

/**
   void close()

   Closes the stream and finalizes the native value. Further
   member calls on this object will result in an exception.
*/
static int popen_cb_close( cwal_callback_args const * args,
                           cwal_value **rv ){
    THIS_POPEN;
    cwal_native_clear( nself, 1 );
    *rv = cwal_value_undefined();
    return 0;
}

/**
   Creates a new popen prototype. The result needs to be stashed
   somewhere by the caller.

   Returns NULL on error (most likely an OOM).
*/
static cwal_value * popen_prototype(whcl_engine * const el){
    char const * pName = "popen2";
    cwal_value * proto;
    int rc;
    proto = cwal_new_object_value(el->ec);
    if(!proto) return NULL;
    else {
        whcl_func_def const funcs[] = {
          WHCL_FUNC2("close", popen_cb_close),
          WHCL_FUNC2("flush", popen_cb_flush),
          WHCL_FUNC2("read", popen_cb_read),
          WHCL_FUNC2("write", popen_cb_write),
          whcl_func_def_empty_m
        };
        rc = whcl_install_functions(el, proto, funcs, 0);
        if(rc) goto end;
    }
    rc = whcl_install_typename(el, proto, pName);
    end:
    if(rc) cwal_unref(proto);
    return rc ? NULL : proto;
}

/**
   Creates and initializes a new popen_native using the given
   command to pass to the stream. On success, returns 0 and
   sets *rv to the new value.
*/
static int popen_new_native(cwal_engine * e,
                            char const * cmd,
                            popen_native ** rv ){
    popen_native pn = popen_native_empty;
    int rc = popen2( cmd, &pn.fdIn, &pn.fOut, &pn.pidChild );
    if(rc){
        /* Turns out popen2() doesn't fail in the parent process when
           given an invalid binary. Because it exec()'s /bin/sh, and
           that part succeeds? Valgrind, however, sees the failed
           exec() and leaks tons.
        */
        return cwal_exception_setf(e, rc,
                                   "popen2({%s}) failed with code %s",
                                   cmd, cwal_rc_cstr(rc));
    }else if(fflush(pn.fOut)){
        pclose2( pn.fdIn, pn.fOut, pn.pidChild );
        return cwal_exception_setf(e, cwal_errno_to_cwal_rc(errno, CWAL_RC_IO),
                                   "Opened stream, but it seems to be invalid.");
    }
    *rv = (popen_native *)cwal_malloc(e, sizeof(popen_native));
    if(*rv){
        **rv = pn;
        return 0;
    }
    else return CWAL_RC_OOM;
}

/**
   cwal_callback_f() binding for popen2()

   Script usage:

   var handle = popen("/path/to/binary -flag ...");
*/
static int popen_ctor( cwal_callback_args const * args,
                       cwal_value **rv ){
    popen_native * pn = NULL;
    cwal_value * vp;
    int rc;
    char const * cmd = (args->argc)
        ? cwal_value_get_cstr(args->argv[0], NULL)
        : NULL;
    cwal_value * proto =
      (cwal_value *)cwal_args_state(args, &popen_native_empty);
    if(!proto){
      return cwal_cb_throw(args, CWAL_RC_UNSUPPORTED,
                         "popen prototype is missing. Make sure this "
                         "module is not both statically and "
                         "dynamically linked!");
    }
    if(!cmd || !*cmd){
        return cwal_exception_setf(args->engine,
                                   CWAL_RC_MISUSE,
                                   "Expecting a non-empty "
                                   "string argument.");
    }
    rc = popen_new_native(args->engine, cmd, &pn);
    if(rc) return rc;
    assert(pn);
    vp = cwal_new_native_value(args->engine, pn, popen_native_finalize,
                               &popen_native_empty);
    if(!vp){
        popen_native_finalize(args->engine, pn);
        return CWAL_RC_OOM;
    }
    cwal_value_prototype_set(vp, proto);
    *rv = vp;
    return 0;
}

static int whcl_module_init_popen2( whcl_engine * const el, cwal_value ** rv ){
  cwal_value * module;
  int rc = 0;
  cwal_value * proto;
  proto = popen_prototype(el);
  if(!proto) return CWAL_RC_OOM;
  cwal_value_ref(proto);
  module = cwal_new_function_value(el->ec, popen_ctor,
                                   proto, 0, &popen_native_empty);
  if(!module) rc = CWAL_RC_OOM;
  else{
    cwal_ref(module);
    rc = whcl_stash_hidden_member(module, proto);
    if(rc){
      cwal_unref(module);
    }else{
      cwal_unhand(module);
      *rv = module;
    }
  }
  cwal_unref(proto)
    /* On error, destroyed, on success stashed in module */;
  return rc;
}

WHCL_MODULE_REGISTER_(popen2);

#undef THIS_POPEN
#undef MARKER
