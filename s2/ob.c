/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#include "libs2.h"
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
struct S2ObBuffer {
    s2_engine * se;
    cwal_buffer buf;
};
typedef struct S2ObBuffer S2ObBuffer;

static const int S2ObBuffer_typeid = 0;

static void s2_S2ObBuffer_finalize( cwal_engine * e, void * m ){
  S2ObBuffer * st = (S2ObBuffer *)m;
  cwal_buffer_reserve(e, &st->buf, 0);
  st->se = NULL;
  cwal_free2(e, m, sizeof(S2ObBuffer));
}

static int s2_S2ObBuffer_output_f( void * state, void const * src, cwal_size_t n ){
  int rc = 0;
  S2ObBuffer * ob = (S2ObBuffer*) state;
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
    rc = cwal_buffer_reserve(ob->se->e, &ob->buf, nSize);
  }
  return rc ? rc : cwal_buffer_append(ob->se->e, &ob->buf, src, n);
  overflow:
  return cwal_exception_setf(ob->se->e, CWAL_RC_RANGE,
                             "cwal_size_t overflow in buffer "
                             "size calculation.");
}

static int s2_ob_push_outputer( s2_engine * se, cwal_outputer const * out ){
  if(!se || !out) return CWAL_RC_MISUSE;
  else{
    int rc;
    cwal_outputer * co =
      (cwal_outputer *)cwal_malloc2(se->e, sizeof(cwal_outputer));
    if(co){
      rc = cwal_list_append( se->e, &se->ob, co );
    }else{
      rc = CWAL_RC_OOM;
    }
    if(rc){
      if(co) cwal_free2(se->e, co, sizeof(cwal_outputer));
    }else{
      *co = se->e->vtab->outputer /* Store the old one */;
      se->e->vtab->outputer = *out /* Replace it with the new one. */;
    }
    return rc;
  }
}

#if 0
/**
   This is how we "could" integrate OB's flush() with cwal_output(),
   but doing so causes functions which flush (e.g. the default print()
   impl) to Do The Wrong Thing when we do that. So s2.io.flush()
   becomes a no-op while buffering is on.
 */
int s2_ob_flush( s2_engine * ie );
static int s2_S2ObBuffer_flush(void * ob){
  return s2_ob_flush( ((S2ObBuffer*)ob)->ie );
}
#endif

cwal_size_t s2_ob_level( s2_engine * se ){
  return se ? se->ob.count : 0;
}

static S2ObBuffer * s2_ob_state_ptr( cwal_outputer const * from ){
  S2ObBuffer * rc = from->state.typeID == &S2ObBuffer_typeid
    ? (S2ObBuffer*)from->state.data
    : NULL;
  assert(rc && "Seems someone has mucked with a cwal_outputer instance");
  return rc;
}

#define s2__ob_current_buffer(SE) s2_ob_state_ptr(&(SE)->e->vtab->outputer)

int s2_ob_push( s2_engine * se ){
  int rc;
  cwal_outputer out = cwal_outputer_empty;
  S2ObBuffer * ob;
  assert(se);
  ob = (S2ObBuffer *)cwal_malloc2(se->e, sizeof(S2ObBuffer));
  if(!ob) return CWAL_RC_OOM;
  ob->se = se;
  ob->buf = cwal_buffer_empty;
  out.state.data = ob;
  out.state.finalize = s2_S2ObBuffer_finalize;
  out.state.typeID = &S2ObBuffer_typeid;
  out.output = s2_S2ObBuffer_output_f;
  /* out.flush = s2_S2ObBuffer_flush; */
  rc = s2_ob_push_outputer( se, &out );
  if(rc){
    cwal_free2( se->e, ob, sizeof(S2ObBuffer) );
  }
  return rc;   
}

int s2_ob_reserve( s2_engine * se, cwal_size_t reserveBufSize ){
  if(!se) return CWAL_RC_MISUSE;
  else if(!se->ob.count) return CWAL_RC_RANGE;
  else{
    S2ObBuffer * const ob = s2__ob_current_buffer(se);
    return cwal_buffer_reserve(se->e, &ob->buf, reserveBufSize);
  }
}

int s2_ob_pop( s2_engine * se ){
  if(!se) return CWAL_RC_MISUSE;
  else if(!se->ob.count) return CWAL_RC_RANGE;
  else{
    cwal_size_t const i = se->ob.count-1;
    cwal_outputer * ob = (cwal_outputer *)se->ob.list[i];
    cwal_outputer * io = &se->e->vtab->outputer;
    if(io->state.finalize){
      io->state.finalize(se->e, io->state.data);
    }
    *io = *ob;
    cwal_free2(se->e, ob, sizeof(cwal_outputer));
    --se->ob.count;
    return 0;
  }
}

int s2_cb_ob_pop( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  cwal_int_t take = 0;
  s2_engine * se = s2_engine_from_args(args);
  assert(se);
  if(!s2_ob_level(se)){
    return cwal_exception_setf(args->engine, CWAL_RC_RANGE,
                               "Cannot pop output buffer: stack is empty.");
  }
  else if(args->argc) take = cwal_value_get_integer(args->argv[0]);

  if(!take){
    rc = s2_ob_pop( se );
    if(!rc) *rv = args->self;
  }else{
    rc = (take<0)
      ? s2_cb_ob_take_string(args,rv)
      : s2_cb_ob_take_buffer(args,rv);
    if(!rc) rc = s2_ob_pop(se);
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

int s2_cb_ob_push( cwal_callback_args const * args, cwal_value **rv ){
  s2_engine * se = s2_engine_from_args(args);
  int rc;
  assert(se);
  rc = s2_ob_push( se );
  if(rc){
    return cwal_cb_throw(args, rc, "s2_ob_push() failed.");
  }else{
    int rc = 0;
    if(args->argc
       && cwal_value_is_integer(args->argv[0])){
      cwal_int_t const sz = cwal_value_get_integer(args->argv[0]);
      if(sz<0){
        rc = cwal_cb_throw(args, CWAL_RC_RANGE,
                         "Expecting an integer argument >= 0.");
      }else{
        rc = s2_ob_reserve(se, (cwal_size_t)sz);
        if(rc){
          assert(CWAL_RC_OOM == rc);
          s2_ob_pop(se);
        }
      }
    }
    if(!rc) *rv = args->self;
    return rc;
  }
}    

int s2_ob_get( s2_engine * se, cwal_buffer ** tgt ){
  if(!se || !tgt) return CWAL_RC_MISUSE;
  else if(!se->ob.count) return CWAL_RC_RANGE;
  else{
    S2ObBuffer * const ob = s2__ob_current_buffer(se);
    *tgt = &ob->buf;
    return 0;
  }
}

int s2_ob_take( s2_engine * se, cwal_buffer * tgt ){
  if(!se || !tgt) return CWAL_RC_MISUSE;
  else if(!se->ob.count) return CWAL_RC_RANGE;
  else{
    S2ObBuffer * const ob = s2__ob_current_buffer(se);
    if(tgt != &ob->buf){
      void * self = tgt->self;
      *tgt = ob->buf;
      tgt->self = self;
    }
    ob->buf = cwal_buffer_empty;
    return 0;
  }
}

int s2_ob_clear( s2_engine * se, char releaseBufferMem ){
  if(!se) return CWAL_RC_MISUSE;
  else if(!se->ob.count) return CWAL_RC_RANGE;
  else{
    S2ObBuffer * const ob = s2__ob_current_buffer(se);
    if(releaseBufferMem) cwal_buffer_reserve(se->e, &ob->buf, 0);
    else ob->buf.used = 0;
    return 0;
  }
}

int s2_ob_flush( s2_engine * se ){
  if(!se) return CWAL_RC_MISUSE;
  else if(!se->ob.count) return CWAL_RC_RANGE;
  else{
    int rc = 0;
    cwal_size_t const i = se->ob.count-1;
    cwal_outputer * to = (cwal_outputer *)se->ob.list[i];
    S2ObBuffer * const ob = s2__ob_current_buffer(se);
    assert(to != &se->e->vtab->outputer);
    if(ob->buf.used){
      rc = to->output( to->state.data, ob->buf.mem,
                       ob->buf.used );
    }
    ob->buf.used = 0;
    return rc;
  }
}

int s2_cb_ob_level( cwal_callback_args const * args, cwal_value **rv ){
  s2_engine * se = s2_engine_from_args(args);
  assert(se);
  *rv = cwal_new_integer(args->engine, (cwal_int_t)se->ob.count);
  return *rv ? 0 : CWAL_RC_OOM;
}    

#define OB_THROW_IF_NO_OB if(!se->ob.count){                            \
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,            \
                               "Output buffering is not in effect.");   \
  }(void)0

int s2_cb_ob_get( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  s2_engine * se = s2_engine_from_args(args);
  cwal_buffer * buf = NULL;
  assert(se);
  OB_THROW_IF_NO_OB;
  rc = s2_ob_get(se, &buf);
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
int s2_cb_ob_reserve( cwal_callback_args const * args, cwal_value **rv ){
  cwal_int_t const sz = args->argc ? cwal_value_get_integer(args->argv[0]) : -1;
  if(sz<0) return cwal_cb_throw(args, CWAL_RC_RANGE,
                              "Expecting an integer value >= 0.");
  else{
    s2_engine * se = s2_engine_from_args(args);
    int const rc = s2_ob_reserve( se, (cwal_size_t)sz );
    assert(se);
    return rc ? cwal_cb_throw(args, rc, "s2_ob_reserve() failed "
                            "with code %d (%s).", rc, cwal_rc_cstr(rc))
      : 0;
  }
}    
***/

int s2_cb_ob_take_string( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  s2_engine * se = s2_engine_from_args(args);
  cwal_buffer buf = cwal_buffer_empty;
  assert(se);
  OB_THROW_IF_NO_OB;
  rc = s2_ob_take(se, &buf);
  if(rc) return rc;
#if 0
  *rv = cwal_new_string_value(args->engine,
                               (char const *)buf.mem,
                               buf.used);
  rc = *rv ? 0 : CWAL_RC_OOM;
#else
  { /* Finally get to use a z-string... */
    cwal_string * s = cwal_buffer_to_zstring(args->engine, &buf);
    if(!s) rc = CWAL_RC_OOM;
    else{
      *rv = cwal_string_value(s);
      /* we just took over ownership of buf.mem */;
      assert(!buf.mem);
      assert(!buf.used);
      assert(!buf.capacity);
    }
  }
#endif
  cwal_buffer_reserve(args->engine, &buf, 0);
  return rc;
}

int s2_cb_ob_take_buffer( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  s2_engine * se = s2_engine_from_args(args);
  cwal_buffer * buf;
  cwal_value * bv;
  assert(se);
  OB_THROW_IF_NO_OB;
  bv = cwal_new_buffer_value(args->engine, 0);
  if(!bv) return CWAL_RC_OOM;
  buf = cwal_value_get_buffer(bv);
  rc = s2_ob_take(se, buf);
  if(rc) cwal_value_unref(bv);
  else *rv = bv;
  return rc;
}

int s2_cb_ob_clear( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  s2_engine * se = s2_engine_from_args(args);
  assert(se);
  OB_THROW_IF_NO_OB;
  rc = s2_ob_clear( se, 0 );
  if(rc) rc = cwal_exception_setf(args->engine, rc,
                                  "s2_ob_clear() failed.");
  else *rv = args->self;
  return rc;
}

int s2_cb_ob_capture( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  s2_engine * se = s2_engine_from_args(args);
  cwal_value * arg;
  cwal_function * fCallback;
  char const * sCallback;
  cwal_size_t nCallback = 0;
  cwal_size_t const oldLevel = s2_ob_level(se);
  cwal_int_t mode = -1;
  cwal_value * xrv = 0;
  assert(se);
  if(0==args->argc || args->argc>2){
    misuse:
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting (string|function [,int mode]) arguments.");
  }
  arg = args->argv[0];
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
  if(args->argc>1){
    arg = args->argv[1];
    if(cwal_value_is_buffer(arg)){
      mode = 1;
    }else{
      mode = cwal_value_get_integer(arg);
    }
  }
  rc = s2_ob_push(se);
  if(rc) return rc;

  if(sCallback){
    rc = s2_eval_cstr(se, 0, "OB capture() callback",
                      sCallback, (int)nCallback, 0);
  }else{
    assert(fCallback);
    rc = cwal_function_call(fCallback, args->self, 0, 0, 0);
  }
  if(!rc){
    if(se->ob.count <= oldLevel){
      return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                 "Too many OB levels were popped via "
                                 "the callback.");
    }
  }
  while(se->ob.count > oldLevel+1){
    if(mode){
      int const rc2 = rc ? rc : s2_ob_flush(se);
      if(rc2 && !rc){
        rc = rc2;
        /* Don't break - keep popping so that our API guarantees
           hold. */
      }
    }
    s2_ob_pop(se)/*cannot fail*/;
  }
  if(!rc){
    assert(oldLevel + 1 == se->ob.count
           && "Should have been caught above.");
    if(0==mode){
      xrv = cwal_value_undefined();
    }else if(0 > mode){
      rc = s2_cb_ob_take_string(args,&xrv);
    }else{
      assert(args->argc>0);
      arg = args->argv[1];
      if(cwal_value_is_buffer(arg)){
        cwal_buffer * const tgt = cwal_value_get_buffer(arg);
        S2ObBuffer * const ob = s2__ob_current_buffer(se);
        /* Potential TODO/optimization: this lazy/easy approach
           captures (via the above s2_ob_flush()/pop combination) all
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
          s2_buffer_swap(tgt, &ob->buf);
          assert(!ob->buf.mem);
        }
        if(!rc) xrv = arg;
      }else{
        rc = s2_cb_ob_take_buffer(args,&xrv);
      }
    }
  }
  s2_ob_pop(se) /* ignore result - cannot fail */;
  if(rc){
    assert(!xrv);
  }else{
    assert(xrv);
    *rv = xrv;
  }
  return rc;
}    

int s2_cb_ob_flush( cwal_callback_args const * args, cwal_value **rv ){
  s2_engine * se = s2_engine_from_args(args);
  int rc;
  assert(se);
  OB_THROW_IF_NO_OB;
  rc = s2_ob_flush( se );
  if(rc) rc = cwal_exception_setf(args->engine, rc,
                                  "s2_ob_flush() failed.");
  else *rv = args->self;
  return rc;
}

int s2_install_ob( s2_engine * se, cwal_value * tgt ){
  if(!se || !tgt) return CWAL_RC_MISUSE;
  else if(!cwal_props_can(tgt)) return CWAL_RC_TYPE;
  else{
    s2_func_def const funcs[] = {
      S2_FUNC2("takeString", s2_cb_ob_take_string),
      S2_FUNC2("takeBuffer", s2_cb_ob_take_buffer),
      /* S2_FUNC2("reserve", s2_cb_ob_reserve), */
      S2_FUNC2("push", s2_cb_ob_push),
      S2_FUNC2("pop", s2_cb_ob_pop),
      S2_FUNC2("level", s2_cb_ob_level),
      S2_FUNC2("getString", s2_cb_ob_get),
      S2_FUNC2("flush", s2_cb_ob_flush),
      S2_FUNC2("clear", s2_cb_ob_clear),
      S2_FUNC2("capture", s2_cb_ob_capture),
      s2_func_def_empty_m
    };
    return s2_install_functions(se, tgt, funcs, 0);
  }
}

int s2_install_ob_2( s2_engine * se, cwal_value * tgt,
                     char const * name ){
  if(!se || !tgt || !name || !*name) return CWAL_RC_MISUSE;
  else {
    int rc;
    cwal_value * ob = cwal_new_object_value(se->e);
    if(!ob) rc = CWAL_RC_OOM;
    else if((rc = s2_install_ob(se, ob))
            ||
            (rc = cwal_prop_set(tgt, name, cwal_strlen(name), ob))){
      cwal_value_unref(ob);
    }
    return rc;
  }
}

#undef OB_THROW_IF_NO_OB
#undef s2__ob_current_buffer
#undef MARKER
