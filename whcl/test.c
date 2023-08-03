/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=4 et sw=2 tw=80: */
/**
   Test/demo code for the whcl library.
*/
#if defined(NDEBUG)
#  undef NDEBUG
#endif
#include "internal.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>

#if 1
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:\t",__FILE__,__LINE__); if(1) printf pfexp
#else
static void noop_printf(char const * fmt, ...) {}
#define MARKER if(0) printf
#endif
#define RC if(rc){                              \
    MARKER(("rc=%d=%s\n", rc, cwal_rc_cstr(rc)));   \
  } assert(0==rc)

struct {
  bool enableTracing;
  bool enableValueRecycling;
  bool enableStringInterning;
  bool showMetrics;
  bool keepJunkTokens;
  bool dumpTokens;
  short verboseMode;
  char const *scriptFile;
  cwal_engine ec;
  whcl_engine el;
} App = {
0/*enableTracing*/,
1/*enableValueRecycling*/,
1/*enableStringInterning*/,
0/*showMetrics*/,
0/*keepJunkTokens*/,
0/*dumpTokens*/,
0/*verboseMode*/,
NULL/*scriptFile*/,
cwal_engine_empty_m,
whcl_engine_empty_m
};

static void dump_whcl_script_commands(whcl_script * const ct, unsigned depth){
  int rc = 0;
  whcl_stoken const * tok = 0;
  char const * cstr = 0;
  char const * cstrEnd = 0;
  cwal_midsize_t nCstr = 0;
  int const pad = depth * 4;
  whcl__script_rewind(ct);
  while(0==(rc = whcl__script_next_token2(ct, &tok))
         && !whcl_stoken_is_eof(tok)){
    //printf("%s ", tok1_t_cstr(tok->ttype));
    switch(tok->ttype){
      case TOK1_T_Blank:
      case TOK1_T_EOX:
      case TOK1_T_EOL:
        break;
      case TOK1_T_BraceGroup: {
        whcl_script sub = whcl__script_empty;
        rc = whcl__script_sub_from_group(ct, &sub, true);
        assert(0==rc);
        dump_whcl_script_commands(&sub, depth+1);
        whcl__script_finalize(&sub);
        continue;
      }
      case TOK1_T_CommentC:
      case TOK1_T_CommentCpp:
      case TOK1_T_CommentTCL:
        cstr = whcl_stoken_cstr(ct, tok, &nCstr, false);
        printf("%*scomment: %.*s\n", pad, "", (int)nCstr, cstr);
        break;
      default:
        cstrEnd = NULL;
        printf("T%-5d %*sCOMMAND lv %d: ", tok->id, pad, "", depth+1);
        do{
          cstr = whcl_stoken_cstr(ct, tok, &nCstr, false);
          if(cstrEnd && cstrEnd<cstr){
            // ^^^ keep adjacent tokens adjacent on output
            putchar(' ');
          }
          switch(tok->ttype){
            case TOK1_T_BraceGroup: {
              whcl_script sub = whcl__script_empty;
              rc = whcl__script_sub_from_group(ct, &sub, true);
              assert(0==rc);
              assert(sub.ec == ct->ec);
              puts("[");
              dump_whcl_script_commands(&sub, depth+1);
              printf("%*s]", pad, "");
              whcl__script_finalize(&sub);
              continue;
            }
          }
          printf("%.*s", (int)nCstr, cstr);
          cstrEnd = cstr + nCstr;
        }while(0==(rc = whcl__script_next_token2(ct, &tok))
               && !whcl_stoken_is_eof(tok)
               && !whcl_t_is_eox(tok->ttype));
        putchar('\n');
        break;
    }
  }
  if(rc){
    char const * msg = 0;
    assert(!"whcl__script_next_token2() \"cannot fail.\"");
    rc = whcl__script_err_get(ct, &msg, NULL);
    MARKER(("Error via whcl__script_next_token2(): %d (%s): %s",
            rc, cwal_rc_cstr(rc), msg));
  }
}

static int test_tok2(whcl_engine * const el, char const *zFile){
  cwal_engine * const e = el->ec;
  cwal_buffer buf = cwal_buffer_empty;
  int rc = 0;
  rc = cwal_buffer_fill_from_filename( e, &buf, zFile );    
  if(rc){
    MARKER(("Reading file failed.\n"));
    goto end;
  }
  //MARKER(("Read in %d bytes from %s\n", (int)buf.used, zFile));
  if(0 && App.verboseMode>1){
    printf("c9 test file contents:\n%s\n", (char const*)buf.mem);
  }
  if(App.dumpTokens){
    whcl_script * const ct = whcl__script_alloc(e);
    assert(ct);
    whcl__script_init(e, ct);
    assert(ct->ec==e);
    //ct->lineOffset = ct->colOffset = 100;
    ct->name = zFile;
    rc = whcl__compile_buffer( el, ct, &buf, App.keepJunkTokens
                               ? WHCL__SCRIPT_F_RETAIN_JUNK : 0 );
    if(rc) goto end_one;
    MARKER(("Compiled ctoker. Token count=%d (%d bytes)\n",
            (int)ct->chainLength,
            (int)(ct->chainLength * sizeof(whcl_stoken))));

    if(1){
      whcl_stoken const * tok1 = 0;
      whcl_stoken const * tok2 = 0;
      whcl_dump_tokens(el, ct, App.verboseMode
                       ? WHCL_DUMP_TOKENS_VERBOSE
                       : 0);

      /* Test putback... */
      whcl__script_rewind(ct);
      whcl__script_next_token2(ct, &tok1);
      assert(tok1);
      whcl__script_putback(ct);
      whcl__script_next_token2(ct, &tok2);
      assert(tok2);
      assert(tok1 == tok2);
    }
    if(1){
      putchar('\n');
      MARKER(("Split by commands:\n"));
      dump_whcl_script_commands(ct, 0);
    }
    end_one:
    MARKER(("c9 chain: token count=%"PRIu32", capacity=%"PRIu32", memory=%u\n",
            ct->chainLength, ct->chainCapacity,
            (unsigned)(ct->chainCapacity * sizeof(whcl_stoken))));
    whcl_script_free(ct);
    if(rc) goto end;
  }
  if(1){
    //MARKER(("via whcl_eval_cstr()\n"));
    cwal_value * v = 0;
    rc = whcl_eval_buffer(el, true, zFile, &buf, &v);
    if(WHCL_RC_NO_COMMANDS==rc) rc = 0;
    if(0==rc){
      whcl__dump_val(v, "eval_script result");
    }
    cwal_refunref(v);
  }

  end:
  cwal_buffer_clear(e, &buf);
  if(rc){
    char const * msg = 0;
    cwal_engine_error_get(el->ec, &msg, NULL);
    MARKER(("test_tok2() failed: rc=%s %s\n",
            cwal_rc_cstr(rc), msg));
  }
  return rc;
}


int my_callback( cwal_callback_args const * args, cwal_value **rv ){
  if(args || rv){/*unused*/}
  return CWAL_RC_OK;
}

/**
   Called via cwal_engine_init() using our customized
   cwal_engine_vtab.
*/
int e_init_engine(cwal_engine *e, cwal_engine_vtab * vtab){

  int32_t featureFlags = 0;
  /*MARKER(("cwal_engine_vtab::hook::on_init() callback. e=%p state=%p\n",
    (void const *)e, vtab->hook.init_state));*/
  if(vtab){/*unused*/}
  if(1){
    /*Enable tracing.*/
    int32_t flags = CWAL_TRACE_NONE;
    flags |= CWAL_TRACE_ERROR_MASK;
    if(App.enableTracing){
      flags |=
        CWAL_TRACE_VALUE_MASK
        | CWAL_TRACE_SCOPE_MASK
        ;
    }
    cwal_engine_trace_flags( e, flags );
  }

  if(App.enableStringInterning){
    /* Enable auto-interning of strings:

       Costs a lot of memory but is pretty cool.
    */
    featureFlags |= CWAL_FEATURE_INTERN_STRINGS;
  }

    
  /* rc = cwal_engine_feature_flags(e,-1); */
  cwal_engine_feature_flags(e, /* rc |  */featureFlags );


#define REMAX(T,N) cwal_engine_recycle_max( e, CWAL_TYPE_ ## T, (N) )
  REMAX(UNDEF,0);
  if(App.enableValueRecycling){
    /* A close guess based on the post-20141129 model...
       List them in "priority order," highest priority last.
       Lower prio ones might get trumped by a higher prio one.
    */
    REMAX(UNIQUE,10)/* will end up being trumped by integer (32-bit)
                       or double (64-bit) */;
    REMAX(KVP,30) /* guaranteed individual recycler */;
    REMAX(WEAKREF,5) /* guaranteed individual recycler */;
    REMAX(STRING,30) /* guaranteed individual recycler */;
    REMAX(EXCEPTION,3);
    REMAX(HASH,5) /* might also include: function, native */;
    REMAX(BUFFER,5) /* might also include: object */ ;
    REMAX(XSTRING,5/*also Z-strings, might also include doubles*/);
    REMAX(NATIVE,5)  /* might also include: function, hash */;
    REMAX(DOUBLE,20)/* might also include z-/x-strings */;
    REMAX(FUNCTION,20) /* might include: hash, native */;
    REMAX(ARRAY,20);
    REMAX(OBJECT,20) /* might include: buffer */;
    REMAX(INTEGER,40) /* might include: double */;
    REMAX(TUPLE,30) /* might include: x-/z-strings, propref */;
    REMAX(PROPREF,30) /* might include: x-/z-strings, tuple */;
  }
#undef REMAX
  return 0;
}

int e_main(void){
  whcl_engine el = whcl_engine_empty_m;
  cwal_engine_vtab vtab = cwal_engine_vtab_basic;
  cwal_engine E = cwal_engine_empty;
  cwal_engine * e =
    1 ? &E : 0;
  int rc;
  assert(cwal_output_f_FILE == vtab.outputer.output);
  assert(cwal_finalizer_f_fclose == vtab.outputer.state.finalize);
  vtab.outputer.state.data = stdout;
  vtab.tracer = cwal_engine_tracer_FILE;
  vtab.tracer.state = stdout;
  vtab.hook.on_init = e_init_engine;
  vtab.hook.init_state = e;

  if(0){
    if(0) vtab.memcap.maxSingleAllocSize = 48;
    if(0) vtab.memcap.maxTotalAllocCount = 10;
    vtab.memcap.maxConcurrentMem = 1024 * 50;
  }

  rc = cwal_engine_init( &e, &vtab );
  RC;
  rc = whcl_engine_init(&el, e, NULL);
  RC;

  if(App.scriptFile){
    rc = test_tok2(&el, App.scriptFile);
  }

  //end:
  if(0==rc && App.showMetrics){
    cwal_dump_allocation_metrics( e );
  }
  if(CWAL_RC_EXCEPTION==rc){
    cwal_value * const ex = cwal_exception_get(el.ec);
    if(ex){
      //cwal_buffer * const b = cwal_buffer_reuse(&el.escBuf);
      //cwal_buffer_format(el.ec, b, "%1$4J", -1, 1, &ex);
      //MARKER(("Exception %d: %s\n", (int)b->used, cwal_buffer_cstr(b,NULL)));
      whcl__dump_val(ex, "EXCEPTION");
    }
  }

  if(0){
    char const * zNum = "-5";
    cwal_int_t i = 0;
    int xc = cwal_cstr_to_int(zNum, 2, &i);
    MARKER(("xc=%d, i=%"CWAL_INT_T_PFMT"\n", xc, i));
    cwal_double_t d = 0.0;
    xc = cwal_cstr_to_double(zNum, 2, &d);
    MARKER(("xc=%d, i=%"CWAL_DOUBLE_T_PFMT"\n", xc, d));
  }

  whcl_engine_finalize(&el);
  return rc; 
}

void show_sizeofs(){
  cwal_size_t total = 0;
  MARKER(("Various library-level sizeof()s...\n"));
#define C(M) MARKER((#M"=%u\n", M));
  C(CWAL_SIZE_T_BITS);
  C(CWAL_INT_T_BITS);
  C(CWAL_VOID_PTR_IS_BIG);
#undef C
#define SO(T) total += sizeof(T); MARKER(("sizeof(%s)=%u\n", #T, (unsigned int)sizeof(T)))
  SO(void*);
  SO(cwal_buffer);
  SO(cwal_engine);
  SO(cwal_int_t);
  SO(cwal_midsize_t);
  SO(cwal_scope);
  SO(cwal_size_t);
  SO(tok1_en);
  SO(tok1_izer);
  SO(whcl__func);
  SO(whcl_engine);
  SO(whcl_scope);
  SO(whcl_script);
  SO(whcl_stoken);
#undef SO
}

int main(int argc, char const * const * argv)
{
  int i, rc = 0;
  bool showSizes = 0;

#define ARG(X) else if(0==strcmp(X, arg))
  for( i = 1; i < argc; ++i ){
    char const * arg = argv[i];
    if(0) {}
    ARG("-S"){
      App.enableStringInterning = !App.enableStringInterning;
      continue;
    }
    ARG("-R"){
      App.enableValueRecycling = !App.enableValueRecycling;
      continue;
    }
    ARG("-f"){
      if(++i==argc || !(App.scriptFile = argv[i])){
        MARKER(("Missing -f FILENAME\n"));
        goto misuse;
      }
      continue;
    }
    ARG("-V"){
      ++App.verboseMode;
      continue;
    }
    ARG("-z"){
      showSizes = 1;
      continue;
    }
    ARG("-d"){
      App.dumpTokens = true;
      continue;
    }
    ARG("-j"){
      App.keepJunkTokens = true;
      continue;
    }
    ARG("-m"){
      App.showMetrics = 1;
      continue;
    }
    /*ARG("-tt"){
      App.enableTracing = 2;
      continue;
    }
    ARG("-t"){
      App.enableTracing = 1;
      continue;
      }*/
    else if(!App.scriptFile){
      App.scriptFile = arg;
      continue;
    }
    MARKER(("Unhandled argument: %s", arg));
    goto misuse;
  }

  /*test_toker();*/
  if(showSizes) show_sizeofs();
  if(!showSizes && !App.scriptFile){
    MARKER(("Use -f FILENAME to process a test file.\n"));
    goto misuse;
  }else{
    rc = e_main();
  }
  goto end;
  misuse:
  rc = CWAL_RC_MISUSE;
  end:
  if(rc){
    MARKER(("Result code: %s\n", cwal_rc_cstr(rc)));
  }
  return rc ? EXIT_FAILURE : EXIT_SUCCESS;
}
