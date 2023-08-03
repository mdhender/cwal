/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/*
  License: same as cwal. See cwal.h resp. libcwal.h for details.
*/
#include "internal.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include <stdio.h>
#if 1
#define MARKER(pfexpr) if(1) printf("MARKER: %s:%d:\t",__FILE__,__LINE__); if(1) printf pfexpr
#else
#define MARKER if(0) printf
#endif


int whcl_value_output( whcl_engine * const el, cwal_value * const v ){
  int rc;
  switch(cwal_value_type_id(v)){
    case CWAL_TYPE_ARRAY:
    case CWAL_TYPE_BOOL:
    case CWAL_TYPE_DOUBLE:
    case CWAL_TYPE_EXCEPTION:
    case CWAL_TYPE_INTEGER:
    case CWAL_TYPE_NULL:
    case CWAL_TYPE_OBJECT:
    case CWAL_TYPE_TUPLE:
      rc = cwal_json_output_engine( el->ec, v, whcl_json_output_opt(false) );
      break;
    case CWAL_TYPE_UNDEF:
      rc = cwal_output(el->ec, "undefined", 9);
      break;
    case CWAL_TYPE_STRING:{
      cwal_size_t slen = 0;
      char const * const cstr = cwal_value_get_cstr(v, &slen);
      rc = slen ? cwal_output(el->ec, cstr, slen) : 0;
      break;
    }
    case CWAL_TYPE_BUFFER:{
      cwal_buffer const * const vb = cwal_value_get_buffer(v);
      rc = vb->used ? cwal_output(el->ec, vb->mem, vb->used) : 0;
      break;
    }
    case CWAL_TYPE_HASH:
    case CWAL_TYPE_FUNCTION:
    case CWAL_TYPE_UNIQUE:
      fmt_fallback:
      rc = cwal_outputf(el->ec, "%s@0x%p", cwal_value_type_name2(v, NULL),
                        (void const*)v);
      break;
    case CWAL_TYPE_NATIVE: {
      whcl_script * const scr = whcl_value_get_script(v);
      if(scr){
        cwal_value * vv = NULL;
        rc = whcl_script_to_jsonable(scr, &vv);
        if(vv){
          assert(0==rc);
          cwal_ref(vv);
          rc = whcl_value_output(el, vv);
          cwal_value_unref(vv);
        }
        break;
      }else{
        goto fmt_fallback;
      }
    }
    default:{
      char const * msg = "Don't know how to output this value type: %s";
      return whcl_err_throw(el, CWAL_RC_TYPE, msg,
                            cwal_value_type_name2(v, NULL));
      break;
    }
  }
  if(rc && (CWAL_RC_EXCEPTION!=rc)){
    rc = whcl_err_throw(el, rc, "Output error #%d (%s).",
                        rc, cwal_rc_cstr(rc));
  }
  return rc;
}

int whcl_value_to_buffer( whcl_engine * const el, cwal_buffer * const buf,
                          cwal_value * const arg ){
  cwal_engine * const e = el->ec;
  int rc = 0;
  switch(cwal_value_type_id(arg)){
    case CWAL_TYPE_STRING:{
      cwal_string const * s = cwal_value_get_string(arg);
      rc = cwal_buffer_append( e, buf, cwal_string_cstr(s),
                               cwal_string_length_bytes(s) );
      break;
    }
    case CWAL_TYPE_BOOL: {
      char const b = cwal_value_get_bool(arg);
      rc = cwal_buffer_append( e, buf,
                               b ? "true" : "false",
                               b ? 4 : 5);
      break;
    }
    case CWAL_TYPE_UNDEF:
      rc = cwal_buffer_append( e, buf, "undefined", 9);
      break;
    case CWAL_TYPE_NULL:
      rc = cwal_buffer_append( e, buf, "null", 4);
      break;
    case CWAL_TYPE_INTEGER:
      rc = cwal_buffer_printf( e, buf, "%"CWAL_INT_T_PFMT,
                               cwal_value_get_integer(arg));
      break;
    case CWAL_TYPE_DOUBLE:
      rc = cwal_buffer_printf( e, buf, "%"CWAL_DOUBLE_T_PFMT,
                               cwal_value_get_double(arg));
      if(!rc){
        /* Trim trailing zeroes... */
        unsigned char * pos = buf->mem + buf->used - 1;
        while(pos>buf->mem && '0' == *pos && '.' != *(pos-1)) {
          *pos = 0;
          --pos;
          --buf->used;
        }
      }
      break;
    case CWAL_TYPE_BUFFER:{
      cwal_buffer const * vb = cwal_value_get_buffer(arg);
      assert(vb);
      if(vb->used){
        rc = cwal_buffer_reserve( e, buf, buf->used + vb->used + 1 )
          /* Preallocation required in case buf===vb */;
        if(!rc){
          if(vb==buf){
            memmove(buf->mem + buf->used , vb->mem, vb->used);
            buf->used *= 2;
            buf->mem[buf->used] = 0;
            break;
          }else{
            rc = cwal_buffer_append( e, buf, vb->mem, vb->used );
          }
        }
      }
      break;
    }
    case CWAL_TYPE_OBJECT:
    case CWAL_TYPE_ARRAY:
    case CWAL_TYPE_TUPLE:{
      cwal_output_buffer_state job = cwal_output_buffer_state_empty;
      job.e = e;
      job.b = buf;
      rc = cwal_json_output( arg, cwal_output_f_buffer, &job,
                             whcl_json_output_opt(false) );
      break;
    }          
    case CWAL_TYPE_HASH:
    case CWAL_TYPE_FUNCTION:
    case CWAL_TYPE_UNIQUE:
      fmt_fallback:
      rc = cwal_buffer_printf(e, buf, "%s@%p",
                              cwal_value_type_name(arg),
                              (void const*)arg);
      break;
    case CWAL_TYPE_NATIVE: {
      whcl_script * const scr = whcl_value_get_script(arg);
      if(scr){
        cwal_value * vv = NULL;
        rc = whcl_script_to_jsonable(scr, &vv);
        if(vv){
          assert(0==rc);
          cwal_ref(vv);
          rc = whcl_value_to_buffer(el, buf, vv);
          cwal_value_unref(vv);
        }
        break;
      }else{
        goto fmt_fallback;
      }
    }
    default:
      rc = cwal_exception_setf( e, CWAL_RC_TYPE,
                                "Don't know how to to-string arguments "
                                "of type '%s'.",
                                cwal_value_type_name(arg));
      break;
  }
  switch(rc){
    case CWAL_RC_CYCLES_DETECTED:
      /* Convert this to an exception. */
      rc = whcl_err_throw(el, rc, "Cycles detected in JSON data.");
      break;
    case 0:
    default: break;
  }
  return rc;
}

int whcl_cb_value_to_string( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  cwal_buffer buf = cwal_buffer_empty;
  whcl_engine * const el = whcl_engine_from_args(args);
  rc = whcl_value_to_buffer(el, &buf, args->self);
  if(!rc
     && !(*rv = cwal_string_value(cwal_buffer_to_zstring( args->engine, &buf)))
     ){
    rc = CWAL_RC_OOM;
  }
  cwal_buffer_reserve(args->engine, &buf, 0);
  return rc;
}

static int whcl_cb_to_json_token_impl( cwal_callback_args const * args,
                                       cwal_value **rv,
                                       bool useSelf
                                       /*0=use args->argv[0], else args->self*/
                                       ){
  if(!args->argc && !useSelf){
    misuse:
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting one argument.");
  }else{
    int rc = 0;
    cwal_value * v = NULL;
    cwal_value * vIndent;
    cwal_json_output_opt outOpt = *whcl_json_output_opt(false);
    cwal_buffer buf = cwal_buffer_empty;
    cwal_buffer * jb = NULL;
    cwal_size_t oldUsed;
    bool selfBuf = 0;
    int indentIndex = -1;
    int cyclesIndex = -1;
    if(useSelf){
      jb = cwal_value_buffer_part(args->engine, args->self);
      /* Change semantics when this is-a Buffer */
      v = jb
        ? (args->argc ? args->argv[0] : NULL)
        : args->self;
      indentIndex = jb ? 1 : 0;
    }
    else{
      assert(args->argc);
      v = args->argv[0];
      indentIndex = 1;
    }
    if(!v) goto misuse;
    cyclesIndex = indentIndex + 1;
    assert(indentIndex >= 0);
    vIndent = (indentIndex >= (int)args->argc)
      ? NULL
      : args->argv[indentIndex];
    outOpt.cyclesAsStrings = (cyclesIndex < (int)args->argc)
      ? cwal_value_get_bool(args->argv[cyclesIndex])
      : 0;
    selfBuf = !!jb;
    if(!selfBuf) jb = &buf;
    oldUsed = jb->used;
    if(vIndent){
      /* Accept indentation as either a string, an integer, or an enum
         entry which wraps one of those.
      */
      vIndent = whcl_value_unwrap(vIndent);
      if(!(outOpt.indentString.str =
           cwal_value_get_cstr(vIndent, &outOpt.indentString.len))){
        outOpt.indent = cwal_value_get_integer(vIndent);
      }
    }
    rc = cwal_json_output_buffer( args->engine, v,
                                  jb, &outOpt );
    if(CWAL_RC_CYCLES_DETECTED==rc){
      rc = cwal_exception_setf(args->engine, rc,
                               "Cycles detected in JSON output.");
    }else if(!rc){
      v = selfBuf
        /* ? cwal_new_integer(args->engine,
           (cwal_int_t)(jb->used - oldUsed))*/
        ? args->self
        /* TODO? Use a z-string and transfer the buffer memory? */
        : cwal_new_string_value( args->engine,
                                  ((char const *)jb->mem + oldUsed),
                                  jb->used - oldUsed );
      if(!v) rc = CWAL_RC_OOM;
      else *rv = v;
    }
    if(buf.mem) cwal_buffer_reserve( args->engine, &buf, 0 );
    return rc;
  }
}

int whcl_cb_this_to_json_token( cwal_callback_args const * args, cwal_value **rv ){
    return whcl_cb_to_json_token_impl( args, rv, true );
}

int whcl_cb_arg_to_json_token( cwal_callback_args const * args, cwal_value **rv ){
    return whcl_cb_to_json_token_impl( args, rv, false );
}

static int whcl_cb_json_parse_impl( cwal_callback_args const * args,
                                    cwal_value **rv, bool isFilename ){
  int rc;
  cwal_value * root = NULL;
  cwal_size_t slen = 0;
  cwal_value * const arg = args->argc ? args->argv[0] : args->self;
  cwal_engine * const e = args->engine;
  cwal_json_parse_info pInfo = cwal_json_parse_info_empty;
  char const * cstr = cwal_value_get_cstr(arg, &slen);
  if(!cstr){
    cwal_buffer * const b = cwal_value_buffer_part(e, arg);
    if(b){
      cstr = (char const *)b->mem;
      slen = b->used;
    }
    if(!cstr){
      return cwal_exception_setf(e, CWAL_RC_MISUSE,
                                 "Expecting a %s as argument or 'this'.",
                                 isFilename
                                 ? "filename" : "JSON (string|Buffer)");
    }
  }
  rc = isFilename
    ? cwal_json_parse_filename( e, cstr, &root, &pInfo )
    : cwal_json_parse_cstr( e, cstr, slen, &root, &pInfo );
  if(rc){
    if(pInfo.errorCode){
      return cwal_exception_setf(e, rc,
                                 "Parsing JSON failed at byte "
                                 "offset %"CWAL_SIZE_T_PFMT
                                 ", line %"CWAL_SIZE_T_PFMT
                                 ", column %"CWAL_SIZE_T_PFMT
                                 " with code %d (%s).",
                                 (cwal_size_t)pInfo.length,
                                 (cwal_size_t)pInfo.line,
                                 (cwal_size_t)pInfo.col,
                                 (int)pInfo.errorCode,
                                 cwal_rc_cstr(pInfo.errorCode));
    }else{
      return cwal_exception_setf(e, rc,
                                 "Parsing JSON failed with code %d (%s).",
                                 rc, cwal_rc_cstr(rc));
    }
  }
  assert(root);
  *rv = root;
  return 0;
}

int whcl_cb_json_parse_string( cwal_callback_args const * args, cwal_value **rv ){
  return whcl_cb_json_parse_impl(args, rv, false);
}

int whcl_cb_json_parse_file( cwal_callback_args const * args, cwal_value **rv ){
  return whcl_cb_json_parse_impl(args, rv, true);
}

#undef MARKER
