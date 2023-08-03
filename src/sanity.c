/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=4 et sw=2 tw=80: */
/**
   UNDER CONSTRUCTION. FAR FROM COMPLETEl

   The "new" (2020) main cwal test app. The goal here is to eventually
   test (nearly) every feature of the library.
*/
#if defined(NDEBUG)
#  undef NDEBUG
#endif
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#if defined(CWAL_AMALGAMATION_BUILD)
#  include "cwal_amalgamation.h"
#else
#  include "wh/cwal/cwal.h"
#  include "cwal_internal.h" /* don't do this at home, clients. */
#endif

/*
  MARKER((...)) is an Internally-used output routine which
  includes file/line/column info.
*/
#if 1
#define MARKER(pfexp) if(1) printf("%s:%d:\t",__FILE__,__LINE__); if(1) printf pfexp
#else
static void noop_printf(char const * fmt, ...) {}
#define MARKER(pfexp) if(0) noop_printf pfexp
#endif
#define MESSAGE(pfexp) printf pfexp

#define dump_val(V,M) cwal_dump_value(__FILE__,__LINE__,(V),(M))

struct {
  char enableTracing;
  char enableValueRecycling;
  char enableStringInterning;
  char showMetrics;
  char logScopePushPop;
  int scopePushes;
  int scopePops;
  cwal_json_output_opt jsonOutOpt;
} App = {
0/*enableTracing*/,
1/*enableValueRecycling*/,
1/*enableStringInterning*/,
0/*showMetrics*/,
0/*logScopePushPop*/,
0,0/*scopePushes, scopePops*/,
cwal_json_output_opt_empty_m
};

typedef void (*e_tester)(cwal_engine *);

#define RC if(rc){                                  \
    MARKER(("rc=%d=%s\n", rc, cwal_rc_cstr(rc)));   \
  } assert(0==rc)

int visitorCount = 0;
int ptr_visitor_foo( cwal_value ** V, void * S ){
#if 0
  cwal_value *v = *V;
  MARKER(("Visiting value #%d @ %p\n", ++visitorCount, (void *)v));
#endif
  if(S){/*avoid unused var warning*/}
  *V = NULL;
  return CWAL_RC_OK;
}

void e_comparisons(cwal_engine * e){
  cwal_value * s = cwal_new_string_value(e, "hi", 2);
  cwal_value * s2 = cwal_new_string_value(e, "bi", 2);
  cwal_value * i = cwal_new_integer(e, 3);
  cwal_value * i2 = cwal_new_integer(e, 42);
  cwal_value * i0 = cwal_new_integer(e, 0);
  cwal_value * d = cwal_new_double(e, 42.24);
  cwal_value * d2 = cwal_new_double(e, 42.242);
  cwal_value * sd2 = cwal_new_string_value(e, "42.242", 6);
  cwal_value * null = cwal_value_null();
  cwal_value * undef = cwal_value_undefined();
  CWAL_UNUSED_VAR int rc;
#define CMP(L,R,COND) rc = cwal_value_compare(L,R); assert(COND)
  CMP(s,s2,(rc>0));
  CMP(null,undef,(0==rc));
  CMP(undef,null,(0==rc));
  CMP(i,i,(0==rc));
  CMP(i,i2,(0>rc));
  CMP(i2,i,(0<rc));
  CMP(d,d2,(0>rc));
  CMP(d2,d,(0<rc));
  CMP(d,i2,(0<rc));
  CMP(i2,d,(0>rc));
  CMP(i2,d2,(0>rc));
  CMP(sd2,d2,(0==rc));
  CMP(d2,sd2,(0==rc));
  CMP(sd2,d,(0!=rc));
  CMP(cwal_value_true(),cwal_value_false(),(0<rc));
  CMP(i0,cwal_value_false(),(0==rc));
  CMP(i2,cwal_value_false(),(rc>0));
#undef CMP
  assert(1 == cwal_value_get_bool(cwal_value_true()));
  assert(0 == cwal_value_get_bool(cwal_value_false()));

  { /* string/x-string compatibility... */
    char const * key = "foo";
    cwal_size_t const keyLen = strlen(key);
    cwal_string * xstr = cwal_new_xstring(e, key, keyLen);
    cwal_string * str = cwal_new_string(e, key, keyLen);
    cwal_midsize_t lenCheck = 0;
    char const * x = cwal_string_cstr2(xstr, &lenCheck);
    cwal_value_ref(cwal_string_value(xstr));
    cwal_value_ref(cwal_string_value(str));
    assert(keyLen == lenCheck);
    assert(x == key);
    assert(keyLen==cwal_string_length_bytes(xstr));
    assert(xstr->length & keyLen);
    assert(xstr->length > keyLen) /* from the x-string flag */;
    assert(0==cwal_compare_str_cstr(xstr, key, keyLen));
    assert(0==cwal_compare_str_cstr(xstr, "foo", 3));
    assert(0<cwal_compare_str_cstr(xstr,"foo",2));
    assert(0>cwal_compare_str_cstr(xstr,"goo",3));
    assert(0==cwal_value_compare(cwal_string_value(xstr),
                                 cwal_string_value(str)));
    cwal_value_unref(cwal_string_value(xstr));
    cwal_value_unref(cwal_string_value(str));
  }
}

void e_ptr_table( cwal_engine * e ){
  enum { ValTableSize = 113,
         ValsToCreate = ValTableSize * 8 / 10  };
  int rc;
  uint32_t i, x;
  cwal_array * ar = NULL;
  cwal_ptr_table * vt = NULL;
  cwal_value * v;
  cwal_string * s;
  s = cwal_new_stringf( e, "%s", "hi, world!" );
  v = cwal_string_value( s );
  assert(s && (s == cwal_value_get_string(v)));
  assert(10==cwal_string_length_bytes(s));
  assert(0==cwal_compare_str_cstr(s, "hi, world!", 10));
    
  rc = cwal_ptr_table_create(e, &vt, (unsigned)ValTableSize, 0 );
  assert( 0 == rc );

  rc = cwal_ptr_table_search(e, vt, v );
  assert( 0 != rc && "Search unexpectedly succeeded." );
  rc = cwal_ptr_table_insert(e, vt, v );
  assert( 0 == rc && "Insert unexpectedly failed." );
  rc = cwal_ptr_table_search(e, vt, v );
  assert( 0 == rc && "Search unexpectedly failed." );
  rc = cwal_ptr_table_insert(e, vt, v );
  assert( CWAL_RC_ALREADY_EXISTS == rc && "Unexpected insert result." );
  rc = cwal_ptr_table_remove( e, vt, v );
  assert( 0 == rc );
  rc = cwal_ptr_table_search(e, vt, v );
  assert( CWAL_RC_NOT_FOUND == rc && "Search failed in unexpected manner." );

  /*MARKER(("Val list loop starts here...\n"));*/
  ar = cwal_new_array(e);
  cwal_value_ref( cwal_array_value(ar) );
  assert(ar);
  rc = cwal_scope_push( e, 0 );
  RC;
  for( x = 0, i = 0; i < ValsToCreate; ++i ){
    cwal_value * vv = cwal_new_integer( e, i );
    rc = cwal_ptr_table_insert( e, vt, vv );
    ++x;
    assert( 0 == rc );
    rc = cwal_ptr_table_search( e, vt, vv );
    assert( 0 == rc );
#if 0
    if(i && !(i%2)){
      rc = cwal_ptr_table_remove( e, vt, vv );
      assert( 0 == rc && "Remove failed.");
      /*cwal_value_unref( e, vv );*/
      --x;
      continue;
    }
#endif
    rc = cwal_array_append( ar, vv );
    RC;
  }
  rc = cwal_scope_pop(e);
  RC;
  assert( cwal_array_length_get(ar) == x );

  cwal_ptr_table_visit( vt, ptr_visitor_foo, NULL );

  {
    unsigned int pageCount = 0;
    uint32_t a = 0, m = 0;
    cwal_ptr_page const * p = vt->pg.head;
    for( ; p; p = p->next ) ++pageCount;
    rc = cwal_ptr_table_mem_cost( vt, &a, &m );
    assert(0==rc);
#if 0
    MARKER(("table depth=%u with hashSize=%u, step=%u, value count=%d\n"
            "Memory (approximate!): mallocs~=%u memory~=%u\n",
            pageCount, (unsigned)vt->hashSize, (unsigned)vt->step, (unsigned)x,
            a, m));
#endif
    for( i = 0, p = vt->pg.head; p; ++i, p = p->next ){
      /*MARKER(("Page #%u entry count=%u\n", i+1, p->entryCount));*/
      assert(p->entryCount && "Expecting an entry in every page");
    }
  }

  cwal_ptr_table_destroy( e, vt );
  /*cwal_string_unref( e, s );*/
  if(0){
    cwal_array * arB = arB = ar;
    v = cwal_array_value(arB);
    rc = cwal_array_append( ar, v );
    assert(0 == rc);
    /* rc = cwal_array_unref( ar ); */
    ar = arB;
  }
  cwal_array_unref(ar);
}

void e_propref( cwal_engine * e ){
  cwal_scope s = cwal_scope_empty;
  cwal_scope_push2(e, &s);
  cwal_value * const k = cwal_new_string_value(e, "a key", 5);
  cwal_value * v = cwal_new_integer(e, 1);
  cwal_value * const o = cwal_new_object_value(e);
  int rc = cwal_prop_set_v(o, k, v);
  assert(0==rc);
  cwal_value * const o2 = cwal_new_object_value(e);
  cwal_value * const r =
    cwal_new_propref_value(CWAL_PROPREF_AUTO, 0, o, k, NULL);
  cwal_ref(o); cwal_ref(o2); cwal_ref(r);
  assert(cwal_value_is_propref(r));
  assert(cwal_propref_value(cwal_value_get_propref(r)) == r);
  dump_val(o,"o");
  dump_val(k,"k");
  dump_val(r,"r");
  dump_val(o2,"o2");
  rc = cwal_prop_set_v(o2, k, r);

  assert(r != cwal_prop_get_v(o2, k));
  assert(v == cwal_prop_get_v(o2, k));
  v = cwal_new_integer(e, 2);
  rc = cwal_prop_set_v(o2, k, v);
  assert(0==rc);
  assert(v == cwal_prop_get_v(o, k));
  assert(v == cwal_prop_get_v(o2, k));

  dump_val(k, "Unsetting k via o2");
  assert(cwal_value_refcount(k));
  rc = cwal_prop_set_v(o2, k, NULL);
  assert(0==rc);
  assert(cwal_value_refcount(k));
  dump_val(k, "k unset, expecting a live refpoint");
  assert(NULL == cwal_prop_get_v(o2, k));
  dump_val(cwal_prop_get_v(o, k), "get (o, k)");
  assert(v == cwal_prop_get_v(o, k));


  cwal_scope_pop(e);
}

void e_call( cwal_engine * e, e_tester func ){
  int rc;
  cwal_scope _p1 = cwal_scope_empty;
  cwal_scope s2 = cwal_scope_empty;
  cwal_scope * s1 = &_p1;
  /* We push 2 scopes ONLY to test the variant APIs for pushing
     scopes.
  */
  rc = cwal_scope_push(e, &s1); RC;
  rc = cwal_scope_push2(e, &s2); RC;
  func(e);
  rc = cwal_scope_pop(e); RC;
  rc = cwal_scope_pop(e); RC;
  if(0 && e->interned.pg.head){
    cwal_dump_interned_strings_table( e, 1, 30  );
  }
}

/**
   Called via cwal_engine_init() using our customized
   cwal_engine_vtab.
*/
int e_init_engine(cwal_engine *e, cwal_engine_vtab * vtab){

  int32_t featureFlags = 0;
  if(vtab){/*unused*/}
  App.jsonOutOpt.addNewline = 1;
  if(1){
    /*Enable tracing.*/
    int32_t flags = CWAL_TRACE_NONE;
    flags |= CWAL_TRACE_ERROR_MASK;
    if(App.enableTracing){
      flags |=
        CWAL_TRACE_VALUE_MASK
        | CWAL_TRACE_SCOPE_MASK
        ;
      if(App.enableTracing>1){
        flags |= CWAL_TRACE_ENGINE_MASK;
        flags |= CWAL_TRACE_MEM_MASK;
        flags |= CWAL_TRACE_FYI_MASK;
      }
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
  }
#undef REMAX
  return 0;
}


static int my_scope_hook_push( cwal_scope * s,
                               CWAL_UNUSED_VAR void * clientState ){
  ++App.scopePushes;
  if(App.logScopePushPop){
    MARKER(("Pushing scope level %d\n", (int)s->level));
  }
  return 0;
}
static void my_scope_hook_pop( cwal_scope const * s,
                               CWAL_UNUSED_VAR void * clientState ){
  ++App.scopePops;
  if(App.logScopePushPop){
    MARKER(("Popping scope level %d\n", (int)s->level));
    if(1==s->level){
      MARKER(("pop hook: the engine is shutting down.\n"));
    }
  }
}

static void e_run_all(cwal_engine *e){
  typedef struct {
    bool active;
    e_tester f;
    char const * desc;
  } FuncEntry;
  FuncEntry funcs[] = {
  {1, e_comparisons, "value comparisons"},
  {1, e_ptr_table, "ptr_table stuff"},
  {1, e_propref, "cwal_propref stuff"}
  };
  int i;
  FuncEntry * fe;
  for( i = 0, fe = funcs; i < (int)(sizeof(funcs)/sizeof(funcs[0]));
       ++fe, ++i ){
    MARKER(("Test #%d: %s\n", i+1, fe->desc));
    if(fe->active) e_call(e, fe->f);
  }
}

void e_main(){
  cwal_engine_vtab vtab = cwal_engine_vtab_basic;
  cwal_engine E = cwal_engine_empty;
  cwal_engine * e =
    1 ? &E : 0;
  int rc;
  App.jsonOutOpt.cyclesAsStrings = 0;
  assert(cwal_output_f_FILE == vtab.outputer.output);
  assert(cwal_finalizer_f_fclose == vtab.outputer.state.finalize);
  vtab.outputer.state.data = stdout;
  vtab.tracer = cwal_engine_tracer_FILE;
  vtab.tracer.state = stdout;
  vtab.hook.on_init = e_init_engine;
  vtab.hook.init_state = e;
  vtab.hook.scope_push = my_scope_hook_push;
  vtab.hook.scope_pop = my_scope_hook_pop;

  if(1){
    if(0) vtab.memcap.maxSingleAllocSize = 48;
    if(0) vtab.memcap.maxTotalAllocCount = 10;
    if(1) vtab.memcap.maxConcurrentMem = 1024 * 50;
  }

  rc = cwal_engine_init( &e, &vtab );
  RC;

  cwal_outputf( e, "e_main(): hi, world!\n" );

  { /* weak reference sanity check... */
    cwal_value * v = cwal_new_integer(e,
                                      10000/*some value which will allocate*/);
    cwal_weakref * r = cwal_weakref_new(v);
    cwal_weakref * r2 = cwal_weakref_new(v);
    assert(r);
    assert(v == cwal_weakref_value(r));
    assert(r==r2);
    cwal_value_unref(v);
    assert(!cwal_weakref_value(r));
    cwal_weakref_free(e, r);
    cwal_weakref_free(e, r2);

    r = cwal_weakref_new(cwal_value_undefined());
    assert(r);
    assert(cwal_weakref_new(cwal_value_undefined())==r);
    cwal_weakref_free(e, r);
  }

  e_run_all(e);

  MARKER(("Shutting down engine. All values (except maybe the "
          "exception/result values) should have been cleaned "
          "up by now-dead scopes by now.\n"));
  if(e->interned.pg.head){
    cwal_dump_interned_strings_table( e, 1, 30 );
  }
  if(App.showMetrics){
    cwal_dump_allocation_metrics( e );
  }
  rc = cwal_engine_destroy( e );
  RC;
}

int main(int argc, char const * const * argv)
{
  int i;
  for( i = 1; i < argc; ++i ){
    char const * arg = argv[i];
    if(0==strcmp("-S",arg)){
      App.enableStringInterning = !App.enableStringInterning;
      continue;
    }
    else if(0==strcmp("-R",arg)){
      App.enableValueRecycling = !App.enableValueRecycling;
      continue;
    }
    else if(0==strcmp("-tt",arg)){
      App.enableTracing = 2;
      continue;
    }
    else if(0==strcmp("-t",arg)){
      App.enableTracing = 1;
      continue;
    }
    else if(0==strcmp("-m",arg)){
      App.showMetrics = 1;
      continue;
    }
    if(0==strcmp("-p",arg)){
      App.logScopePushPop = !App.logScopePushPop;
      continue;
    }
  }

  /*test_toker();*/
  e_main();
  MARKER(("Done!\n"));
  return 0;
}
