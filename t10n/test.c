/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/**
   The core sanity tests for the t10n API...
*/
#if defined(NDEBUG)
#  undef NDEBUG
#endif
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <errno.h>
#include <stdbool.h>
#include "c9n.h"

#if 1
#define MARKER(pfexp) if(1) printf("%s:%d():\t",__FILE__,__LINE__); if(1) printf pfexp
#else
#define MARKER(X) (void)0
#endif

static struct {
  int verboseMode;
  char const * runC9N;
  cwal_engine e;
} App = {
0/*verboseMode*/,
0/*runC9N*/,
cwal_engine_empty_m
};


static void err(const char *zFmt, ...){
  va_list vargs;
  va_start(vargs, zFmt);
  fputs("ERROR: ",stderr);
  vfprintf(stderr, zFmt, vargs);
  fputc('\n', stderr);
  va_end(vargs);
}

/*
** Reads file zFilename, stores its contents in *zContent, and sets the
** length of its contents to *nContent.
**
** Returns 0 on success or an errno on error. On error, *zContent and
** *nContent are not modified and it may emit a message describing the
** problem.
**
** Limitation: the file must be seekable. It cannot work on streams
** like stdin. This is so we can allocation enough memory for the
** contents in a single alloc. To handle a stream we'll need to loop
** and realloc as we read more.
*/
static int read_file(char const *zFilename, unsigned char ** zContent,
                     uint64_t * nContent){
  long fpos;
  int rc = 0;
  unsigned char * zMem = 0;
  FILE * f = fopen(zFilename, "rb");
  if(!f){
    err("Cannot open file %s. Errno=%d", zFilename, errno);
    return errno;
  }
  fseek(f, 0L, SEEK_END);
  rc = errno;
  if(rc){
    err("Cannot seek() file %s. Errno=%d", zFilename, rc);
    goto end;
  }
  fpos = ftell(f);
  fseek(f, 0L, SEEK_SET);
  zMem = (unsigned char *)malloc((size_t)fpos + 1);
  if(!zMem){
    err("Malloc failed.");
    rc = ENOMEM;
    goto end;
  }
  zMem[fpos] = 0;
  if((size_t)1 != fread(zMem, (size_t)fpos, 1, f)){
    rc = EIO;
    err("Error #%d reading file %s", rc, zFilename);
    goto end;
  }
  end:
  fclose(f);
  if(rc){
    free(zMem);
  }else{
    *zContent = zMem;
    *nContent = (uint64_t)fpos;
  }    
  return rc;
}

static void dump_c9n_chain(c9n_toker * ct, unsigned int level){
  c9n_token const * ctok = 0;
  static const int includeEofs = 1;
  int rc = 0; 
  if(!level){
    puts("c9n_toker->chain:");
    c9n_toker_reset(ct);
  }
  while(0==(rc = c9n_toker_next_token(ct, &ctok))){
    assert(ctok);
    if(includeEofs || !c9n_token_is_eof(ctok)){
      printf("%*s#%d: %s @ %d, %d: %d byte(s) ==> #%d",
             level*2, "",
             ctok->id, t10n_ttype_cstr(ctok->ttype),
             ctok->line, ctok->column,
             (int)c9n_token_len(ctok,0),
             ctok->nextId);
      switch(App.verboseMode ? ctok->ttype : 0){
        case T10N_T_LiteralStringDQ:
        case T10N_T_LiteralStringSQ:
        case T10N_T_Heredoc:
        case T10N_T_Identifier:{
          cwal_size_t n = 0;
          char const * s =
            c9n_token_cstr(ct, ctok, &n, App.verboseMode>1);
          printf(" %.*s", (int)n, s);
          break;
        }
        default:
          break;
      }
      puts("");
    }
    if(T10N_T_EOF==ctok->ttype) break;
    if(ctok->innerId){
      c9n_toker sub = c9n_toker_empty;
      rc = c9n_toker_sub_from_group(ct, &sub);
      if(rc) break;
      dump_c9n_chain(&sub, level + 1);
      c9n_toker_finalize(&sub);
    }
  }
  if(rc){
    MARKER(("Error via c9n_toker_next_token(): %d (%s): %s",
            rc, cwal_rc_cstr(rc), ct->errMsg));
  }
}


static int test_c9n(cwal_engine * e){
  c9n_toker * ct = c9n_toker_alloc(e);
  cwal_buffer buf = cwal_buffer_empty;
  int rc = 0;
  assert(App.runC9N);
  assert(ct);
  assert(ct->e==e);
  assert(&c9n_toker_empty == ct->allocStamp);
  rc = cwal_buffer_fill_from_filename( e, &buf, App.runC9N );    
  if(rc){
    MARKER(("Reading file failed.\n"));
    goto end;
  }
  MARKER(("Read in %d bytes from %s\n", (int)buf.used, App.runC9N));
  if(App.verboseMode>1){
    printf("c9 test file contents:\n%s\n", (char const*)buf.mem);
  }
  rc = c9n_toker_init(e, ct);
  assert(!rc && "Cannot currently fail.");
  assert(ct->e==e);
  assert(&c9n_toker_empty == ct->allocStamp);
  //ct->lineOffset = ct->colOffset = 100;
  ct->name = App.runC9N;
  rc = c9n_toker_compile_buffer( ct, &buf,
#if 0
                           C9N_TOKER_F_RETAIN_JUNK
#else
                           0
#endif
                           );
  if(rc) goto end;
  MARKER(("Compiled cptoker. Token count=%d (%d bytes)\n",
          (int)ct->chainLength,
          (int)(ct->chainLength * sizeof(c9n_token))));

  if(1){
    c9n_token const * tok1 = 0;
    c9n_token const * tok2 = 0;
    dump_c9n_chain(ct, 0);

    /* Test putback... */
    c9n_toker_reset(ct);
    c9n_toker_next_token(ct, &tok1);
    assert(tok1);
    c9n_toker_putback(ct);
    c9n_toker_next_token(ct, &tok2);
    assert(tok2);
    assert(tok1 == tok2);
  }else{
    c9n_token const * ctok;
    while( !(rc = c9n_toker_next_token(ct, &ctok)) ){
      cwal_size_t tlen = 0;
      char const * tstr = c9n_token_cstr(ct, ctok, &tlen, false);
      printf("Token #%d: %s @ %d, %d: %.*s\n",
             ctok->id, t10n_ttype_cstr(ctok->ttype),
             ctok->line, ctok->column,
             (int)tlen, tstr);
      if(T10N_T_EOF==ctok->ttype) break;
    }
  }

  end:
  cwal_buffer_reserve(e, &buf, 0);
  MARKER(("c9 chain: token count=%"PRIu32", capacity=%"PRIu32", memory=%u\n",
          ct->chainLength, ct->chainCapacity,
          (unsigned)(ct->chainCapacity * sizeof(c9n_token))));
  c9n_toker_free(ct);
  if(rc){
    MARKER(("test_c9n() failed: rc=%s\n", cwal_rc_cstr(rc)));
  }
  return rc;
}

static void show_sizeofs(){
#define SO(T) MARKER(("sizeof("#T")=%d\n", (int)sizeof(T)))
  SO(t10n_token);
  SO(t10n_toker);
  SO(((t10n_toker*)NULL)->_lcCache);
  SO(c9n_token);
  SO(c9n_toker);
#undef SO
}  

static int toker_dump(c9n_toker * pt, bool verbose){
  int rc = 0;
  c9n_token const * t = 0;
  int i = 0;
  cwal_size_t tlen = 0;
  while( 0 == (rc = c9n_toker_next_token(pt, &t))){
    char const * tstr;
    assert(!rc);
    assert(t);
    tstr = c9n_token_cstr(pt, t, &tlen, false);
    ++i;
    if(verbose){
      printf("#%d [%s] %.*s\n", i, t10n_ttype_cstr(t->ttype),
             (int)tlen, tstr);
    }else{
      printf("#%d[%s]\n", i, t10n_ttype_cstr(t->ttype));
    }
    if(c9n_token_is_eof(t)) break;
  }
  return rc;
}

static int toker_dump_script(char const *zScript, cwal_int_t nScript, bool verbose,
                              bool expectError){
  int rc = 0, trc = 0;
  c9n_toker toker = c9n_toker_empty;

  if(nScript<0){
    nScript = (cwal_int_t)cwal_strlen(zScript);
  }
  rc = c9n_toker_init(&App.e, &toker);
  assert(!rc && "cannot fail if args are valid");
  rc = c9n_toker_compile(&toker, zScript, nScript, 0);
  if(rc){
    cwal_size_t nErr = 0;
    char const * zErr = 0;
    cwal_engine_error_get(&App.e, &zErr, &nErr);
    if(expectError){
      err("EXPECTED error: Cannot compile input [%s]: %.*s",
          cwal_rc_cstr(rc), (int)nErr, zErr);
      cwal_engine_error_reset(&App.e);
      rc = 0;
    }else{
      err("Cannot compile input [%s]: %.*s", cwal_rc_cstr(rc),
          (int)nErr, zErr);
    }
    goto end;
  }else if(expectError){
    err("We were expecting this input to fail: %.*s",
        (int)nScript, zScript);
    rc = CWAL_RC_ERROR;
    goto end;
  }
  trc = toker_dump(&toker, verbose);
  if(trc && verbose){
    err("Tokenization error %s: %s", cwal_rc_cstr(trc), toker.errMsg);
    rc = trc;
  }
  end:
  c9n_toker_finalize(&toker);
  return rc;
}

static int toker_dump_file(char const *zFile, bool verbose){
  unsigned char *zScript = 0;
  uint64_t nScript = 0;
  int rc = 0;
  rc = read_file(zFile, &zScript, &nScript);
  if(rc){
    err("Cannot open file [%s]. err=%d %s",
        zFile, rc, strerror(rc));
  }else{
    rc = toker_dump_script((char const *)zScript, (cwal_int_t)nScript,
                            verbose, false);
  }
  free(zScript);
  return rc;
}

static int test_compile(char const * zScript, cwal_int_t nScript,
                        c9n_toker * tgt){
  int rc = 0;
  rc = c9n_toker_init(&App.e, tgt);
  assert(!rc && "cannot fail if args are valid");
  rc = c9n_toker_compile(tgt, zScript, nScript, 0);
  return rc;
}

/**
   Expects a script, a verbosity flag, and a number of t10n token type
   values equal to the number of tokens in the script. The script is
   compiled with c9 and iterated over. For each token, its type is
   compared with the corresponding vararg. On a mismatch, App.e's
   error state is updated and non-0 is returned. If verbose is true,
   it prints out its results to stdout.
*/
static int test_the_tokens(char const * script, int verbose, ...){
  /* reminder: verbose is an INT here, rather than bool, because clang 10
     says:

     test.c:341:17: error: passing an object that undergoes default
     argument promotion to 'va_start' has undefined behavior

     For:
     va_start(args,verbose); // w/ bool verbose
  */
  int rc = 0;
  c9n_toker toker = c9n_toker_empty;
  c9n_token const * token = 0;
  cwal_int_t const nScript = (cwal_int_t)cwal_strlen(script);
  va_list args;
  va_start(args,verbose);
  rc = test_compile(script, nScript, &toker);
  if(rc) goto end;
  if(verbose){
    printf("Token-type checking: %.*s\n", (int)nScript, script);
  }
  while(0==(rc = c9n_toker_next_token(&toker, &token))){
    int const vType = (int)va_arg(args,int);
    if(T10N_T_EOF==token->ttype) break;
    if(vType != token->ttype){
      if(verbose){
        printf("\tNOT OK: %s\n", t10n_ttype_cstr(vType));
      }
      rc = c9n_err_toker(&toker, CWAL_RC_TYPE,
                         "Token expectation mismatch: "
                         "expected %s but got %s",
                         t10n_ttype_cstr(vType),
                         t10n_ttype_cstr(token->ttype));
      break;
    }
    if(verbose){
      printf("\tOK: %s\n", t10n_ttype_cstr(vType));
    }
  }
  end:
  va_end(args);
  c9n_toker_finalize(&toker);
  return rc;
}

static int test_1(void){
  int rc = 0;
  const bool verbose = !!App.verboseMode;
  rc = toker_dump_file("test1.t10n", verbose);
#define RC if(rc) goto end
  RC;
#define TOKIT(S,FAIL) MARKER(("Test script: %s\n", S)); \
  rc = toker_dump_script(S, -1, verbose, FAIL)
#define TPASS(S) TOKIT(S,false); RC
#define TFAIL(S) TOKIT(S,true); RC; \
  rc = 0; cwal_engine_error_reset(&App.e)

  TPASS("'single-quoted string' 123.456");
  TFAIL("\n\n  'missing closing quote");
  TFAIL("abc def <<<missing ending of heredoc");
  TPASS("<<<abc blah abc def 123");

  rc = test_the_tokens("0b10 0o12 1 0x1 2.3", true,
                       T10N_T_LiteralIntBin,
                       T10N_T_LiteralIntOct,
                       T10N_T_LiteralIntDec,
                       T10N_T_LiteralIntHex,
                       T10N_T_LiteralDouble);
  RC;
  rc = test_the_tokens("<<<'1' hi '1' 'hi' \"hi\"", true,
                       T10N_T_Heredoc,
                       T10N_T_LiteralStringSQ,
                       T10N_T_LiteralStringDQ);
  RC;

  rc = test_the_tokens("(a b c) [a b c] {a b c}", true,
                       T10N_T_ParenGroup,
                       T10N_T_BraceGroup,
                       T10N_T_SquigglyGroup);
  RC;
  rc = test_the_tokens("a _ _$_", true,
                       T10N_T_Identifier,
                       T10N_T_Identifier,
                       T10N_T_Identifier);
  RC;
  
#undef RC
#undef TOKIT
#undef TPASS
#undef TFAIL
  end:
  return rc;
}

/**
   Called via cwal_engine_init() using our customized
   cwal_engine_vtab.
 */
static int e_init_engine(cwal_engine *e, cwal_engine_vtab * vtab){
    int32_t featureFlags = 0;
    MARKER(("cwal_engine_vtab::hook::on_init() callback. e=%p state=%p\n",
            (void const *)e, vtab->hook.init_state));
    featureFlags |= CWAL_FEATURE_INTERN_STRINGS;
    cwal_engine_feature_flags(e, featureFlags );

#define REMAX(T,N) cwal_engine_recycle_max( e, CWAL_TYPE_ ## T, (N) )
    REMAX(UNDEF,0);
    if(1){
      /* A close guess based on the post-20141129 model...
         List them in "priority order," highest priority last.
         Lower prio ones might get trumped by a higher prio one.
      */
      REMAX(UNIQUE,10)/* will end up being trumped by integer (32-bit)
                         or double (64-bit) */;
      REMAX(KVP,30) /* guaranteed individual recycler */;
      REMAX(WEAKREF,10) /* guaranteed individual recycler */;
      REMAX(STRING,30) /* guaranteed individual recycler */;
      REMAX(EXCEPTION,5);
      REMAX(HASH,20) /* might also include: function, native */;
      REMAX(BUFFER,20) /* might also include: object */ ;
      REMAX(XSTRING,20/*also Z-strings, might also include doubles*/);
      REMAX(NATIVE,20)  /* might also include: function, hash */;
      REMAX(DOUBLE,20)/* might also include x-/z-strings */;
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


int main(int argc, char const * const * argv){
  int rc = 0;
  int i;
  char const * arg;
  int gotNoop = 0;
  cwal_engine_vtab vtab = cwal_engine_vtab_basic;
  cwal_engine * e = &App.e;

#define ARG(X) else if(0==strcmp(X, arg))
  for( i = 1; i < argc; ++i ){
    arg = argv[i];
    if(0){}
    ARG("-v"){
      ++App.verboseMode;
    }
    ARG("-9"){
      if(++i==argc || !(App.runC9N = argv[i])){
        MARKER(("Missing -9 filename\n"));
        goto misuse;
      }
    }
    ARG("-z"){
      ++gotNoop;
      show_sizeofs();
    }
    else{
      MARKER(("Unknown argument: %s\n", arg));
      goto misuse;
    }
  }

  if(gotNoop) goto end;
  vtab.outputer.state.data = stdout;
  vtab.hook.on_init = e_init_engine;
  vtab.hook.init_state = 0;
  rc = cwal_engine_init( &e, &vtab );
  if(rc){
    err("Engine init failed: %s\n", cwal_rc_cstr(rc));
  }

  if(App.runC9N){
    rc = test_c9n(e);
  }else{
    rc = test_1();
  }

  goto end;
  misuse:
  rc = CWAL_RC_MISUSE;
  end:
  MARKER(("Done! rc=%d (%s)\n",
          rc, cwal_rc_cstr(rc)));
  {
    char const * msg = 0;
    int const erc = cwal_engine_error_get(e, &msg, NULL);
    if(erc){
      err("#%d %s: %s", erc, cwal_rc_cstr(erc), msg);
    }
  }
  cwal_engine_destroy(&App.e);
  return rc ? EXIT_FAILURE : EXIT_SUCCESS;
}
