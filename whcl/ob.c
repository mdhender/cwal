/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#include "internal.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#if 0
#include <stdio.h>
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__); if(1) printf pfexp
#else
#define MARKER(pfexp) (void)0
#endif

/**
   Internal state for the OB APIs.
*/
struct ObBuffer {
    whcl_engine * el;
    cwal_buffer buf;
};
typedef struct ObBuffer ObBuffer;

static const int ObBuffer_typeid = 0;

static void whcl__ObBuffer_finalize( cwal_engine * e, void * m ){
  ObBuffer * st = (ObBuffer *)m;
  cwal_buffer_reserve(e, &st->buf, 0);
  st->el = NULL;
  cwal_free2(e, m, sizeof(ObBuffer));
}

/**
   cwal_output_f() impl which appends to the current OB buffer.
*/
static int whcl__ObBuffer_output_f( void * state, void const * src, cwal_size_t n ){
  int rc = 0;
  ObBuffer * ob = (ObBuffer*) state;
  if((ob->buf.used+n) >= ob->buf.capacity
     /* >= b/c of implicitly added NUL */){
    cwal_size_t nSize = ob->buf.capacity
      ? ((ob->buf.capacity+n+1) * 3/2)
      : n*2;
    cwal_size_t const oflow = nSize;
    if(oflow > nSize) goto overflow;
    else if(!nSize) nSize = 50/*arbitrary*/;
    /* MARKER(("nSize=%d\n", (int)nSize)); */
    if(nSize == (ob->buf.used+n)){
      /* corner case: the buffer API appends a NUL, so account for
         that here to avoid yet another reallocation in
         cwal_buffer_append(). */
      ++nSize;
      if(!nSize) goto overflow;
    }
    /* MARKER(("nSize=%d\n", (int)nSize)); */
    rc = cwal_buffer_reserve(ob->el->ec, &ob->buf, nSize);
  }
  return rc ? rc : cwal_buffer_append(ob->el->ec, &ob->buf, src, n);
  overflow:
  return cwal_exception_setf(ob->el->ec, CWAL_RC_RANGE,
                             "cwal_size_t overflow in buffer "
                             "size calculation.");
}

static int whcl__ob_push_outputer( whcl_engine * const el, cwal_outputer const * out ){
  int rc;
  cwal_outputer * co =
    (cwal_outputer *)cwal_malloc2(el->ec, sizeof(cwal_outputer));
  if(co){
    rc = cwal_list_append( el->ec, &el->ob, co );
  }else{
    rc = CWAL_RC_OOM;
  }
  if(rc){
    if(co) cwal_free2(el->ec, co, sizeof(cwal_outputer));
  }else{
    *co = el->ec->vtab->outputer /* Store the old one */;
    el->ec->vtab->outputer = *out /* Replace it with the new one. */;
  }
  return rc;
}

#if 0
/**
   This is how we "could" integrate OB's flush() with cwal_output(),
   but doing so causes functions which flush (e.g. the default print()
   impl) to Do The Wrong Thing when we do that. So s2.io.flush()
   becomes a no-op while buffering is on.
 */
int whcl_ob_flush( whcl_engine * ie );
static int whcl__ObBuffer_flush(void * ob){
  return whcl_ob_flush( ((ObBuffer*)ob)->ie );
}
#endif

cwal_size_t whcl_ob_level( whcl_engine * const el ){
  return el ? el->ob.count : 0;
}

static ObBuffer * whcl__ob_state_ptr( cwal_outputer const * from ){
  ObBuffer * rc = from->state.typeID == &ObBuffer_typeid
    ? (ObBuffer*)from->state.data
    : NULL;
  assert(rc && "Seems someone has mucked with a cwal_outputer instance");
  return rc;
}

#define whcl__ob_current_buffer(EL) whcl__ob_state_ptr(&(EL)->ec->vtab->outputer)

int whcl_ob_push( whcl_engine * const el ){
  int rc;
  cwal_outputer out = cwal_outputer_empty;
  ObBuffer * ob;
  ob = (ObBuffer *)cwal_malloc2(el->ec, sizeof(ObBuffer));
  if(!ob) return CWAL_RC_OOM;
  ob->el = el;
  ob->buf = cwal_buffer_empty;
  out.state.data = ob;
  out.state.finalize = whcl__ObBuffer_finalize;
  out.state.typeID = &ObBuffer_typeid;
  out.output = whcl__ObBuffer_output_f;
  /* out.flush = whcl__ObBuffer_flush; */
  rc = whcl__ob_push_outputer( el, &out );
  if(rc){
    cwal_free2( el->ec, ob, sizeof(ObBuffer) );
  }
  return rc;   
}

int whcl_ob_reserve( whcl_engine * const el, cwal_size_t reserveBufSize ){
  if(!el->ob.count) return CWAL_RC_RANGE;
  else{
    ObBuffer * const ob = whcl__ob_current_buffer(el);
    return cwal_buffer_reserve(el->ec, &ob->buf, reserveBufSize);
  }
}

int whcl_ob_pop( whcl_engine * const el ){
  if(!el->ob.count) return CWAL_RC_RANGE;
  else{
    cwal_size_t const i = el->ob.count-1;
    cwal_outputer * ob = (cwal_outputer *)el->ob.list[i];
    cwal_outputer * io = &el->ec->vtab->outputer;
    if(io->state.finalize){
      io->state.finalize(el->ec, io->state.data);
    }
    *io = *ob;
    cwal_free2(el->ec, ob, sizeof(cwal_outputer));
    --el->ob.count;
    return 0;
  }
}

int whcl_ob_get( whcl_engine * const el, cwal_buffer ** tgt ){
  if(!el->ob.count) return CWAL_RC_RANGE;
  else{
    ObBuffer * const ob = whcl__ob_current_buffer(el);
    *tgt = &ob->buf;
    return 0;
  }
}

int whcl_ob_take( whcl_engine * const el, cwal_buffer * const tgt ){
  if(!el->ob.count) return CWAL_RC_RANGE;
  else{
    ObBuffer * const ob = whcl__ob_current_buffer(el);
    if(tgt != &ob->buf){
      void * self = tgt->self;
      *tgt = ob->buf;
      tgt->self = self;
    }
    ob->buf = cwal_buffer_empty;
    return 0;
  }
}

int whcl_ob_clear( whcl_engine * const el, bool releaseBufferMem ){
  if(!el->ob.count) return CWAL_RC_RANGE;
  else{
    ObBuffer * const ob = whcl__ob_current_buffer(el);
    if(releaseBufferMem) cwal_buffer_reserve(el->ec, &ob->buf, 0);
    else ob->buf.used = 0;
    return 0;
  }
}

int whcl_ob_flush( whcl_engine * const el ){
  if(!el->ob.count) return CWAL_RC_RANGE;
  else{
    int rc = 0;
    cwal_size_t const i = el->ob.count-1;
    cwal_outputer * to = (cwal_outputer *)el->ob.list[i];
    ObBuffer * const ob = whcl__ob_current_buffer(el);
    assert(to != &el->ec->vtab->outputer);
    if(ob->buf.used){
      rc = to->output( to->state.data, ob->buf.mem,
                       ob->buf.used );
    }
    ob->buf.used = 0;
    return rc;
  }
}

#define OB_THROW_IF_NO_OB if(!el->ob.count){                            \
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,            \
                               "Output buffering is not in effect.");   \
  }(void)0

/**
   cwal_callback_f() impl wrapping whcl_ob_take(). Requires
   that args->state be a (whcl_engine*).

   Assigns *rv to the string contents of the buffer layer.

   Design note: the returned string is actually a z-string to avoid
   having to make another copy of the data.
*/
static int whcl__cb_ob_take_string( cwal_callback_args const * args,
                                    cwal_value **rv ){
  int rc;
  whcl_engine * const el = whcl_engine_from_args(args);
  cwal_buffer buf = cwal_buffer_empty;
  OB_THROW_IF_NO_OB;
  rc = whcl_ob_take(el, &buf);
  if(rc) return rc;
  /* Finally get to use a z-string... */
  cwal_string * const s = cwal_buffer_to_zstring(args->engine, &buf);
  if(!s) rc = CWAL_RC_OOM;
  else{
    *rv = cwal_string_value(s);
    /* we just took over ownership of buf.mem */;
    assert(!buf.mem);
    assert(!buf.used);
    assert(!buf.capacity);
  }
  cwal_buffer_reserve(args->engine, &buf, 0);
  return rc;
}

/**
   Functionally identical to whcl_cb_ob_take_string() except that it
   returns (via *rv) a cwal_buffer value (owned by args->engine).
*/
static int whcl__cb_ob_take_buffer( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  whcl_engine * const el = whcl_engine_from_args(args);
  cwal_buffer * buf;
  cwal_value * bv;
  OB_THROW_IF_NO_OB;
  bv = cwal_new_buffer_value(args->engine, 0);
  if(!bv) return CWAL_RC_OOM;
  buf = cwal_value_get_buffer(bv);
  rc = whcl_ob_take(el, buf);
  if(rc) cwal_value_unref(bv);
  else *rv = bv;
  return rc;
} 


/**
   cwal_callback_f() impl wrapping whcl_ob_pop(). Requires
   that args->state be a (whcl_engine*).

   Script signature:

   ```
   mixed pop([int takePolicy=0])
   ```

   If passed no args or a 0/falsy value, it discards any buffered
   output. If passed numeric greater than 0 then it returns (via *rv)
   the content as a Buffer. If passed numeric negative then it returns
   the contents as a String.

*/
static int whcl__cb_ob_pop( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  cwal_int_t take = 0;
  whcl_engine * const el = whcl_engine_from_args(args);
  uint16_t argNdx = 1;
  assert(el);
  if(!whcl_ob_level(el)){
    return cwal_exception_setf(args->engine, CWAL_RC_RANGE,
                               "Cannot pop output buffer: stack is empty.");
  }
  else if(args->argc>argNdx){
    take = cwal_value_get_integer(args->argv[argNdx]);
  }

  if(!take){
    rc = whcl_ob_pop( el );
    if(!rc) *rv = args->self;
  }else{
    rc = (take<0)
      ? whcl__cb_ob_take_string(args,rv)
      : whcl__cb_ob_take_buffer(args,rv);
    if(!rc) rc = whcl_ob_pop(el);
  }
  if(rc && (CWAL_RC_OOM!=rc)
     && (CWAL_RC_EXCEPTION!=rc)
     && (CWAL_RC_INTERRUPTED!=rc)){
    rc = cwal_exception_setf(args->engine,
                             rc, "Error #%d (%s) popping buffer.",
                             rc, cwal_rc_cstr(rc));
  }
  return rc;
}    

/**
   cwal_callback_f() impl wrapping whcl_ob_push(). Requires that
   args->state be a (whcl_engine*). Returns argv->self.

   Accepts an optional integer argument which specifies an amount of
   memory to pre-allocate for the buffer (see whcl_ob_reserve()).

   On error this function returns with an unchanged buffer level.
*/
static int whcl__cb_ob_push( cwal_callback_args const * args, cwal_value **rv ){
  whcl_engine * const el = whcl_engine_from_args(args);
  int rc;
  uint16_t argNdx = 1;
  assert(el);
  rc = whcl_ob_push(el);
  if(rc){
    return cwal_cb_throw(args, rc, "whcl_ob_push() failed.");
  }else{
    int rc = 0;
    if(args->argc>argNdx
       && cwal_value_is_integer(args->argv[argNdx])){
      cwal_int_t const sz = cwal_value_get_integer(args->argv[argNdx]);
      if(sz<0){
        rc = cwal_cb_throw(args, CWAL_RC_RANGE,
                         "Expecting an integer argument >= 0.");
      }else{
        rc = whcl_ob_reserve(el, (cwal_size_t)sz);
        if(rc){
          assert(CWAL_RC_OOM == rc);
          whcl_ob_pop(el);
        }
      }
    }
    if(!rc) *rv = args->self;
    return rc;
  }
}    

static int whcl__cb_ob_level( cwal_callback_args const * args, cwal_value **rv ){
  whcl_engine * const el = whcl_engine_from_args(args);
  assert(el);
  *rv = cwal_new_integer(args->engine, (cwal_int_t)el->ob.count);
  return *rv ? 0 : CWAL_RC_OOM;
}    

/**
   cwal_callback_f() impl wrapping whcl_ob_get(). Requires
   that args->state be a (whcl_engine*).

   Assigns *rv to the string contents of the buffer layer.
*/
static int whcl__cb_ob_get_string( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  whcl_engine * const el = whcl_engine_from_args(args);
  cwal_buffer * buf = NULL;
  assert(el);
  OB_THROW_IF_NO_OB;
  rc = whcl_ob_get(el, &buf);
  if(rc) return rc;
  else{
    assert(buf);
  }
  *rv = cwal_new_string_value(args->engine,
                              (char const *)buf->mem,
                              buf->used);
  return *rv ? 0 : CWAL_RC_OOM;
}    


/***
static int whcl__cb_ob_reserve( cwal_callback_args const * args, cwal_value **rv ){
  cwal_int_t const sz = args->argc ? cwal_value_get_integer(args->argv[0]) : -1;
  if(sz<0) return cwal_cb_throw(args, CWAL_RC_RANGE,
                              "Expecting an integer value >= 0.");
  else{
    whcl_engine * const el = whcl_engine_from_args(args);
    int const rc = whcl_ob_reserve( el, (cwal_size_t)sz );
    assert(el);
    return rc ? cwal_cb_throw(args, rc, "whcl_ob_reserve() failed "
                            "with code %d (%s).", rc, cwal_rc_cstr(rc))
      : 0;
  }
}    
***/


/**
   cwal_callback_f() impl wrapping whcl_ob_clear(). Requires
   that args->state be a (whcl_engine*). Returns argv->self.
*/
static int whcl__cb_ob_clear( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  whcl_engine * const el = whcl_engine_from_args(args);
  assert(el);
  OB_THROW_IF_NO_OB;
  rc = whcl_ob_clear( el, 0 );
  if(rc) rc = cwal_exception_setf(args->engine, rc,
                                  "whcl_ob_clear() failed.");
  else *rv = args->self;
  return rc;
}

/**
   cwal_callback_f() impl for...

   mixed capture(string|function callback
                 [, int captureMode=-1 | buffer captureTarget])

   Which does:

   1) Push an OB level.

   2) Runs the given callback. If it's a function, it is call()ed. If it
   is a string, it is eval'd. Any other type, including a buffer, triggers
   an error.

   3) If the 2nd argument is a buffer, all captured output is appended
   to that buffer and that buffer is returned. If it's not a buffer,
   it's interpreted as an integer with the same semantics as pop()'s
   argument but with a different default value: if it's negative (the
   default) then the captured buffered output is returned as a string,
   positive returns the result as a new buffer, and 0 means to simply
   discard the result.

   4) Pops its buffer.

   If the callback leaves the buffer stack count with fewer levels
   than than what were active when the callback was triggered, steps
   (3) and (4) are skipped and an exception is triggered. If it pushes
   extra levels, they are assumed to be part of the output: this
   function flushes and pops each one, and captures or discards the
   cumulative output.

   The main advantage to this approach to capturing output, over
   manually pushing and popping OB levels, is that this function keeps
   the levels in sync even in the face of a script-level
   assert/exit/fatal call, OOM condition, whcl_interrupt(), or
   similar "flow-control event."
*/
static int whcl__cb_ob_capture( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  whcl_engine * const el = whcl_engine_from_args(args);
  cwal_value * arg;
  cwal_function * fCallback;
  char const * sCallback;
  cwal_size_t nCallback = 0;
  cwal_size_t const oldLevel = whcl_ob_level(el);
  uint16_t argNdx = 1;
  cwal_int_t mode = -1
    /* <0 = take ob buffer
       0 = discard, return undefined
       >0 = take buffer contents as a string. */;
  cwal_value * xrv = 0;
  assert(el);
  if(1==args->argc || args->argc>3){
    misuse:
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting (string|function [,int mode]) arguments.");
  }
  arg = args->argv[argNdx++];
  sCallback = cwal_value_is_string(arg)
    ? cwal_value_get_cstr(arg, &nCallback)
    : 0 /* disallow buffer input b/c of potential for Undefined Behaviour
           if it's modified during the buffering process */;
  if(!sCallback){
    fCallback = cwal_value_get_function(arg);
    if(!fCallback){
      goto misuse;
    }
  }
  if(args->argc>argNdx){
    arg = args->argv[argNdx++];
    if(cwal_value_is_buffer(arg)){
      mode = 1;
    }else{
      mode = cwal_value_get_integer(arg);
    }
  }
  rc = whcl_ob_push(el);
  if(rc) return rc;

  if(sCallback){
    rc = whcl_eval_cstr(el, false, "OB capture() callback",
                        sCallback, (cwal_int_t)nCallback, NULL);
  }else{
    assert(fCallback);
    rc = cwal_function_call(fCallback, args->self, NULL, 0, NULL);
  }
  if(!rc){
    if(el->ob.count <= oldLevel){
      return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                 "Too many OB levels were popped via "
                                 "the callback.");
    }
  }
  while(el->ob.count > oldLevel+1){
    if(mode){
      int const rc2 = rc ? rc : whcl_ob_flush(el);
      if(rc2 && !rc){
        rc = rc2;
        /* Don't break - keep popping so that our API guarantees
           hold. */
      }
    }
    whcl_ob_pop(el)/*cannot fail*/;
  }
  if(!rc){
    assert(oldLevel + 1 == el->ob.count
           && "Should have been caught above.");
    if(0==mode){
      xrv = cwal_value_undefined();
    }else if(0 > mode){
      rc = whcl__cb_ob_take_string(args,&xrv);
    }else{
      assert(args->argc>2);
      arg = args->argv[2];
      if(cwal_value_is_buffer(arg)){
        cwal_buffer * const tgt = cwal_value_get_buffer(arg);
        ObBuffer * const ob = whcl__ob_current_buffer(el);
        /* Potential TODO/optimization: this lazy/easy approach
           captures (via the above whcl_ob_flush()/pop combination) all
           captured OB levels into the current OB buffer layer before
           copying the memory to tgt. We "could" instead walk UP the
           OB stack, before popping it, and append each layer's memory
           to tgt (for the first level, if tgt->mem is NULL, we could
           transfer ownership of the OB buffer's memory to tgt). We
           would need to walk up, not down, the stack so that the
           order of the buffers is properly retained.
        */
        if(tgt->mem){
          rc = cwal_buffer_append(args->engine, tgt, ob->buf.mem,
                                  ob->buf.used);
        }else{
          /* Optimization: simply change ownership of the memory... */
          cwal_buffer_swap_mem(tgt, &ob->buf);
          assert(!ob->buf.mem);
        }
        if(!rc) xrv = arg;
      }else{
        rc = whcl__cb_ob_take_buffer(args,&xrv);
      }
    }
  }
  whcl_ob_pop(el) /* ignore result - cannot fail */;
  if(rc){
    assert(!xrv);
  }else{
    assert(xrv);
    *rv = xrv;
  }
  return rc;
}    

static int whcl__cb_ob_flush( cwal_callback_args const * args, cwal_value **rv ){
  whcl_engine * const el = whcl_engine_from_args(args);
  int rc;
  assert(el);
  OB_THROW_IF_NO_OB;
  rc = whcl_ob_flush(el);
  if(rc) rc = cwal_exception_setf(args->engine, rc,
                                  "whcl_ob_flush() failed.");
  else *rv = args->self;
  return rc;
}

static cwal_callback_f whcl__ob_subcommand(char const * name, cwal_size_t n){
  uint32_t const h = whcl__hash_keyword( name, (uint16_t)n );
#define THEN(W,F) return (sizeof(W)-1==n && 0==memcmp(name,W,n))\
    ? F : NULL
  switch(h){
    /* Values generated by kwasher.c */
    case 0x00d8e3f6: THEN("capture",whcl__cb_ob_capture);
    case 0x00362bd2: THEN("clear",whcl__cb_ob_clear);
    case 0x0037d71e: THEN("flush",whcl__cb_ob_flush);
    case 0x070b74a9: THEN("get-string",whcl__cb_ob_get_string);
    case 0x003b17e2: THEN("level",whcl__cb_ob_level);
    case 0x000f4daa: THEN("pop",whcl__cb_ob_pop);
    case 0x001ea208: THEN("push",whcl__cb_ob_push);
    case 0x0fdbbfaa: THEN("take-buffer",whcl__cb_ob_take_buffer);
    case 0x0fdc53a6: THEN("take-string",whcl__cb_ob_take_string);
  }
#undef THEN
  return NULL;
}

#define WHCL__E_ARGS                                                \
  whcl_engine * const el = whcl_engine_from_args(args); assert(el)

static int whcl__cb_ob_command( cwal_callback_args const * args, cwal_value **rv ){
  if(!args->argc){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "Expecting: subcommand [args...]");
  }
  cwal_size_t nName = 0;
  cwal_value * const arg = args->argv[0];
  char const * subName = cwal_value_get_cstr(arg, &nName);
  WHCL__E_ARGS;
  cwal_callback_f cb = whcl__ob_subcommand(subName, nName);
  return cb
    ? cb(args, rv)
    : cwal_function_forward(cwal_value_get_function(el->cache.commandCb),
                            0, args, rv);
}

int whcl__install_ob( whcl_engine * const el ){
  if(el->cache.installAPI & WHCL__INSTALL_API_ob) return 0;
  cwal_value * sub = NULL;
  cwal_value * const tgt = el->cache.vWhcl;
  char const * name = "ob";
  int rc;
  rc = whcl_install_core_apis(el);
  if(0==rc) rc = whcl__install_sub(el, tgt, name, false, &sub);
  if(0==rc){
    if(1){
      rc = whcl_install_command_cb2(el, whcl__cb_ob_command, sub);
    }else{
      whcl_func_def const funcs[] = {
      WHCL_FUNC2("take-string", whcl__cb_ob_take_string),
      WHCL_FUNC2("take-buffer", whcl__cb_ob_take_buffer),
      /* WHCL_FUNC2("reserve", whcl__cb_ob_reserve), */
      WHCL_FUNC2("push", whcl__cb_ob_push),
      WHCL_FUNC2("pop", whcl__cb_ob_pop),
      WHCL_FUNC2("level", whcl__cb_ob_level),
      WHCL_FUNC2("get-string", whcl__cb_ob_get_string),
      WHCL_FUNC2("flush", whcl__cb_ob_flush),
      WHCL_FUNC2("clear", whcl__cb_ob_clear),
      WHCL_FUNC2("capture", whcl__cb_ob_capture),
      whcl_func_def_empty_m
      };
      rc = whcl_install_functions(el, sub, funcs, 0);
    }
  }
  if(0==rc) el->cache.installAPI |= WHCL__INSTALL_API_ob;
  return rc;
}

#undef OB_THROW_IF_NO_OB
#undef whcl__ob_current_buffer
#undef MARKER
#undef WHCL__E_ARGS
