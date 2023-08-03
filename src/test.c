/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=4 et sw=2 tw=80: */
/**
   Test/demo code for the cwal library. This file tests not only
   public APIs, but also private ones, so it's probably not the best
   starting point for potential clients, but it's the one we currently
   have. The scripting languages implemented on top of cwal, as of
   this writing s2 and whcl, are full-fledged clients which make no
   use of libcwal-internal APIs.
*/
#if defined(NDEBUG)
#  undef NDEBUG
#endif
#include "libcwal.h"
/* When building in the canonical source tree, we need the
   internal-API header for some of the tests this file performs. When
   building outside of the canonical tree (for demo purposes), we need
   to directly #include libcwal.c for access to those bits... */
#if defined(CWAL_AMALGAMATION_BUILD)
#  include "libcwal.c"
#  define CWAL_UNUSED_VAR __attribute__((__unused__)) /* avoiding unused var in non-debug build */
#else
#  include "cwal_internal.h"
#endif
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>

#if 1
#define MARKER if(1) printf("MARKER: %s:%d:\t",__FILE__,__LINE__); if(1) printf
#else
static void noop_printf(char const * fmt, ...) {}
#define MARKER if(0) printf
#endif

struct {
  char enableTracing;
  char enableValueRecycling;
  char enableStringInterning;
  char showMetrics;
  char logScopePushPop;
} App = {
0/*enableTracing*/,
1/*enableValueRecycling*/,
1/*enableStringInterning*/,
0/*showMetrics*/,
1/*logScopePushPop*/
};

/**

   Tokenizes an input string on a given separator. Inputs are:

   - (inp) = is a pointer to the pointer to the start of the input.

   - (separator) = the separator character

   - (end) = a pointer to NULL. i.e. (*end == NULL)

   This function scans *inp for the given separator char or a NUL char.
   Successive separators at the start of *inp are skipped. The effect is
   that, when this function is called in a loop, all neighboring
   separators are ignored. e.g. the string "aa.bb...cc" will tokenize to
   the list (aa,bb,cc) if the separator is '.' and to (aa.,...cc) if the
   separator is 'b'.

   Returns 0 (false) if it finds no token, else non-0 (true).

   Output:

   - (*inp) will be set to the first character of the next token.

   - (*end) will point to the one-past-the-end point of the token.

   If (*inp == *end) then the end of the string has been reached
   without finding a token.

   Post-conditions:

   - (*end == *inp) if no token is found.

   - (*end > *inp) if a token is found.

   It is intolerant of NULL values for (inp, end), and will assert() in
   debug builds if passed NULL as either parameter.
*/
static char wh_str_toker_next_token( char const ** inp, char separator, char const ** end )
{
  char const * pos = NULL;
  assert( inp && end && *inp );
  if( *inp == *end ) return 0;
  pos = *inp;
  if( !*pos )
  {
    *end = pos;
    return 0;
  }
  for( ; *pos && (*pos == separator); ++pos) { /* skip preceeding splitters */ }
  *inp = pos;
  for( ; *pos && (*pos != separator); ++pos) { /* find next splitter */ }
  *end = pos;
  return (pos > *inp) ? 1 : 0;
}

struct wh_str_toker {
  char separator;
  char const * tokenBegin;
  char const * tokenEnd;
  char const * theEnd;
};
typedef struct wh_str_toker wh_str_toker;
#define wh_str_toker_empty_m {' ', 0, 0, 0}
const wh_str_toker wh_str_toker_empty = wh_str_toker_empty_m;

char wh_str_toker_init( wh_str_toker * t, char const * begin,
                        char const * end, char separator )
{
  if(!t || !begin || (end<=begin) ) return 0;
  else {
    t->tokenBegin = begin;
    t->theEnd = end;
    t->tokenEnd = NULL;
    t->separator = separator;
    return 1;
  }
}

char wh_str_toker_next( wh_str_toker * t, char const ** begin, char const ** end )
{
  if(!t || !begin || ! end || (t->tokenBegin >= t->theEnd)) return 0;
  else {
    t->tokenEnd = NULL;
    if( ! wh_str_toker_next_token( &t->tokenBegin, t->separator, &t->tokenEnd ) ) return 0;
    else {
      assert( t->tokenEnd > t->tokenBegin );
      *begin = t->tokenBegin;
      if(t->tokenEnd > t->theEnd){
        /* read trailing parts... */
        t->tokenEnd = t->theEnd;
      }
      *end = t->tokenEnd;
      t->tokenBegin = t->tokenEnd;
      return 1;
    }
  }
}

#define RC if(rc){                              \
    MARKER("rc=%d=%s\n", rc, cwal_rc_cstr(rc)); \
  } assert(0==rc)

void test_toker(){
  wh_str_toker t = wh_str_toker_empty;
  char const * in = "///aA/bB/C////de/F";
  CWAL_UNUSED_VAR const int expect = 4;
  CWAL_UNUSED_VAR int count = 0;
  char const * b = NULL;
  char const * e = NULL;
  CWAL_UNUSED_VAR char rc = wh_str_toker_init( &t, in, in+strlen(in)-3, '/');
  assert(rc);
  while( wh_str_toker_next(&t, &b, &e) ){
    const int len = (int)(e-b);
    ++count;
    assert(e>b);
    MARKER("len=%d token=%.*s\n", len, len, b);
    if(4==count){
      assert(1==len && *b=='d');
    }
  }
  assert(expect == count);
}

int visitorCount = 0;
int ptr_visitor_foo( cwal_value ** V, void * S ){
#if 0
  cwal_value *v = *V;
  MARKER("Visiting value #%d @ %p\n", ++visitorCount, (void *)v);
#endif
  if(S){/*avoid unused var warning*/}
  *V = NULL;
  return CWAL_RC_OK;
}

#define dump_val(V,MSG) cwal_dump_value(__FILE__,__LINE__,(V),(MSG))

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

  MARKER("Val list loop starts here...\n");
  ar = cwal_new_array(e);
  cwal_value_ref( cwal_array_value(ar) );
  assert(ar);
  rc = cwal_scope_push( e, 0 );
  assert(0 == rc && "Scope push failed");
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
  assert(0 == rc && "Scope pop failed");
  assert( cwal_array_length_get(ar) == x );

  cwal_ptr_table_visit( vt, ptr_visitor_foo, NULL );

  {
    unsigned int pageCount = 0;
    uint32_t a = 0, m = 0;
    cwal_ptr_page const * p = vt->pg.head;
    for( ; p; p = p->next ) ++pageCount;
    rc = cwal_ptr_table_mem_cost( vt, &a, &m );
    assert(0==rc);
    MARKER("table depth=%u with hashSize=%u, step=%u, value count=%d\n"
           "Memory (approximate!): mallocs~=%u memory~=%u\n",
           pageCount, (unsigned)vt->hashSize, (unsigned)vt->step, (unsigned)x,
           a, m);
    for( i = 0, p = vt->pg.head; p; ++i, p = p->next ){
      MARKER("Page #%u entry count=%u\n", i+1, p->entryCount);
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

typedef struct NativeType NativeType;
struct NativeType {
  int x;
  int y;
};
static const NativeType NativeType_empty = {0,0};
static void finalizer_NativeType( cwal_engine * e, void * F ){
  NativeType * f = (NativeType*)F;
  MARKER("Freeing NativeType@%p x=%d y=%d\n", F, f->x, f->y);
  cwal_free(e, F);
}

cwal_native * new_native(cwal_engine * e){
  cwal_native * n;
  CWAL_UNUSED_VAR cwal_value * v;
  void * pCheck = NULL;
  NativeType * foo;
  int rc;

  foo = cwal_malloc(e, sizeof(NativeType));
  foo->x = 3; foo->y = 7;
  assert(foo);
  n = cwal_new_native( e, foo, finalizer_NativeType, &NativeType_empty );
  assert(n);
  v = cwal_native_value(n);
  assert(0 == cwal_value_refcount(v));
  assert(v);
  assert(n == cwal_value_get_native(v));
  assert( CWAL_TYPE_NATIVE == cwal_value_type_id(v) );
  assert(0 == cwal_native_get(n, ""));
  rc = cwal_native_fetch(n, &NativeType_empty, &pCheck);
  RC;
  assert(pCheck == foo);
  assert(foo == cwal_native_get(n, &NativeType_empty));
  return n;
}


static cwal_json_output_opt JsonOutOpt = cwal_json_output_opt_empty_m;

void e_json1( cwal_engine * e ){
  cwal_value * v;
  cwal_array * a;
  cwal_object * o;
  cwal_value * oV;
  int rc;
  cwal_json_output_opt opt = cwal_json_output_opt_empty;
  a = cwal_new_array( e );
  o = cwal_new_object( e );
  oV = cwal_object_value(o);
  cwal_array_append( a, cwal_object_value(o) );
  v = cwal_new_double(e, 39);
  cwal_array_append( a, v );
  cwal_prop_set( oV, "foooooo", 3, v );

  cwal_array_append(a, cwal_value_null() );
  cwal_array_append( a, cwal_new_integer(e, 100) );

  opt.addNewline = 1;
  opt.indent = 2;

  rc = cwal_json_output_FILE( cwal_array_value(a), stdout, &opt );
  RC;

#define AOUT rc = cwal_json_output_FILE( cwal_array_value(a), stdout, &opt ); RC
    
  MARKER("Sorted:\n");
  cwal_array_sort( a, cwal_compare_value_void );
  AOUT;

  /* cwal_array_set( a, 1, NULL ); */

  MARKER("Reverse-sorted:\n");
  cwal_array_sort( a, cwal_compare_value_reverse_void );
  AOUT;

  MARKER("via array_reverse():\n");
  cwal_array_reverse( a );
  AOUT;

#undef AOUT

#if 0
  opt.indent = -1;
  opt.indentSingleMemberValues = 1;
  rc = cwal_json_output_engine( e, cwal_object_value(o), &opt );
  RC;
#endif
}

void e_interned_strings( cwal_engine * e ){
  cwal_value * s1;
  cwal_value * s2;
  char const * cstr = "foo";
  cwal_size_t clen;
  clen = cwal_strlen(cstr);
  /*
    Interning introduces an interesting problem:

    We don't use cwal_value_push() for interned strings, because it
    seems silly to do so. However, this causes that the refcount
    exceeds what we will ever unref.  If we do not ref() each "new"
    value then we can get invalid refcounts when we allocate a
    string from 2 scopes, because only one of them would have the
    value in its stack.
      
    If we push the value into the scope for each allocation
    we fix that we have two problems:
      
    a) code which re-uses a string a lot (in a loop) will cause
    the scope to grow inordinately. 

    b) Fixing (a) requires adding more infrastructure (==memory) to
    the scope class. The cwal_ptr_table could be used to track "have
    we seen this", but costs us a great deal of memory and a usable
    table size is impossible to guess. We "could" add the scope pointer
    back to the value instances and push the value to the scope only
    when it differs... no, that has other problems...
  */
  s1 = cwal_new_string_value( e, cstr, clen );
  cwal_value_ref(s1)
    /* 20160111: interning won't re-use strings
       with a refcount of 0, to avoid ugly client-side bugs
       in certain usage patterns.

       20160126: not true anymore. That only delayed
       the problem.
    */;
  s2 = cwal_new_string_value( e, cstr, clen );
  cwal_value_ref(s2);
  MARKER("String interning is %s.\n",
         (s1==s2) ? "ENABLED" : "DISABLED");
  if(App.enableStringInterning){
    assert(s1==s2);
    assert(2==cwal_value_refcount(s1));
  }else{
    assert(s1!=s2);
    assert(1==cwal_value_refcount(s1));
    assert(1==cwal_value_refcount(s2));
  }
  if(s1==s2){
    /**
       allocate lots of them and make sure they don't really allocate.
    */
    int i, max = 1000;
    cwal_value * vbar = 0;
    CWAL_UNUSED_VAR cwal_value * prev = 0;
    cstr = "bar";
    clen = 3;
    MARKER("Looping over %d \"new\" strings which \"should\" "
           "be interned.\n", max);
    for( i = 0; i < max; ++i ){
      prev = vbar;
      vbar = cwal_new_string_value( e, cstr, clen );
      if(0==i){
        cwal_value_ref(vbar);
        /* see note above about interning an refcount */
      }else{
        assert(prev == vbar);
      }
    }
    assert(1==cwal_value_refcount(vbar));
    cwal_value_unref(vbar);
    MARKER("Make sure valgrind doesn't show a lot of string re-allocs here.\n");
  }
  cwal_value_unref(s1);
  cwal_value_unref(s2);

#if 0
  /* this heuristic was removed on 20160126... */
  if( cwal_engine_feature_flags(e, -1) & CWAL_FEATURE_INTERN_STRINGS ){
    /* ensure that the "never re-use temporary interned strings" heuristic
       works... */
    s1 = cwal_new_string_value( e, cstr, clen );
    /* not taking a ref, so interning is not supposed to kick in... */
    s2 = cwal_new_string_value( e, cstr, clen );
    assert(s1 && s2 && s1 != s2);
    assert(0 == cwal_value_compare(s1,s2));
    cwal_value_unref(s1);
    cwal_value_unref(s2);
  }
#endif
}

void e_random_strings( cwal_engine * e ){
  enum {
  StringCount = 500,
  MaxStrLen = 3
  };
  static const char alphanum[] =
#if 0
    "0123456789"
    " !@#$%^&*()_+|[]{}<>/?.,\\\"'~`"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "abcdefghijklmnopqrstuvwxyz"
#else
    /* use this with a small MaxStrLen to get lots of re-use. */
    "abc"
#endif
    ;
  unsigned const slen = (sizeof(alphanum)-sizeof(alphanum[0]/*NUL byte!*/))/sizeof(alphanum[0]);
  cwal_size_t i, x;
  CWAL_UNUSED_VAR cwal_string * s;
  uint32_t totalSize = 0;
  char buf[MaxStrLen+3] = {0,};
  srand(time(0));
  MARKER("Allocating %d strings with a maximum length of %d...\n",
         StringCount, MaxStrLen );
  for( i = 0; i < StringCount; ++i ){
    unsigned mylen = rand() % MaxStrLen + 1;
    unsigned mySize = mylen + sizeof(cwal_string) + 1 /*NUL byte!*/;
    memset( buf, 0, sizeof(buf)/sizeof(buf[0]) );
    for (x = 0; x < mylen; ++x) {
      buf[x] = alphanum[rand() % slen];
    }
    buf[x--] = 0;
    s = cwal_new_string( e, buf, x );
    assert(s);
    totalSize += mySize;
  }
  if(e->interned.pg.head){
    cwal_dump_interned_strings_table( e, 1, slen );
  }
  MARKER("APPROXIMATE total memory usage, had we allocated "
         "all these strings: %u bytes\n",
         (unsigned)totalSize);
}


void e_relaunch1( cwal_engine * e ){
  /*cwal_size_t i, x;*/
  cwal_string * s;
  cwal_value * sv;
  cwal_value * v;
  cwal_array * ar;
  cwal_object * obj;
  cwal_value * objV;
  cwal_weakref * wr = 0;
  int rc;
  cwal_size_t expectRC;
  s = cwal_new_stringf( e, "%s", "HI! world!" );
  sv = cwal_string_value(s);
  assert(s && (s == cwal_value_get_string(sv)));
  assert(10==cwal_string_length_bytes(s));
  assert(0==cwal_compare_str_cstr(s, "HI! world!", 10));

  expectRC = App.enableStringInterning
    ? 0 /* depends on cwal-internal magic/kludge */
    : 0;
  assert( expectRC == cwal_value_refcount( sv ) );
  cwal_value_ref( sv );
  ++expectRC;
  assert( expectRC == cwal_value_refcount( sv ) );
    
  v = cwal_new_integer(e, 0);
  assert( 0 == cwal_value_refcount(v) /* special built-in rc */ );
  rc = cwal_value_unref(v);
  assert(0 == rc);
  v = cwal_new_integer(e, 1);
  assert( 0 == cwal_value_refcount(v) );
  assert(1 == cwal_value_get_integer(v));
  /* assert(CWAL_RC_OK == cwal_value_unref(v)); */

  v = cwal_new_integer(e, 200);
  assert( 0 == cwal_value_refcount(v) );
  assert(CWAL_RC_FINALIZED == cwal_value_unref(v));
    
  v = cwal_new_double(e, 0.0);
  assert( 0 == cwal_value_refcount(v) /* special built-in value */ );
  assert(0 == cwal_value_unref(v));
  v = cwal_new_double(e, 42.24);
  assert( 0 == cwal_value_refcount(v) );
  assert(CWAL_RC_FINALIZED == cwal_value_unref(v));

  ar = cwal_new_array(e);
  assert(ar);
#define APP(V) rc = cwal_array_append( ar, (V) ); RC
  APP(sv); /* Hmmmm. should NULL be a legal array append value? */
  v = cwal_array_value( ar );
  rc = cwal_array_append( ar, v );
  RC;


  obj = cwal_new_object(e);
  objV = cwal_object_value(obj);
  assert(e->current == v->scope);
  v = cwal_object_value(obj);
  assert(v && (0==cwal_value_refcount(v)));
  /*rc = cwal_value_ref(v);
    RC;*/
  /*APP(v); assert(0 == v->scope);*/
#define SET(K,V) rc = cwal_prop_set( objV, K, cwal_strlen((K)), (V) );
  SET("feezle", cwal_array_value(ar) );
  RC;
  APP(v);
  APP(v);
  assert(2 == cwal_value_refcount(v));
  if(1){
    cwal_native * n;
    CWAL_UNUSED_VAR void * pv;
    MARKER("Adding a Native to the mix...\n");
    n = new_native(e);
    pv = cwal_native_get( n, &NativeType_empty );
    v = cwal_native_value(n);
    assert(v && pv);
    assert(0==cwal_value_refcount(v));

    wr = cwal_weakref_new(v);
    assert(wr);
    assert(cwal_weakref_value(wr)==v);

    APP(v);
    assert(1 == cwal_value_refcount(v));        
    rc = cwal_prop_set( objV, "native", 6, v );
    assert(2 == cwal_value_refcount(v));
    RC;
    APP(v);
    assert(3 == cwal_value_refcount(v));

    if(0){
      cwal_buffer * buf;
      cwal_size_t const bufSize = 300;
      MARKER("Adding a scope-local buffer of %u bytes...\n", (unsigned)bufSize);
      buf = cwal_new_buffer(e,bufSize);
      assert(buf);
      assert(bufSize == buf->capacity);
      assert(bufSize == cwal_buffer_fill( buf, '!' ));
      cwal_buffer_unref( e, buf );
    }

  }


#if 1
  {/* Try some cross-scope referencing.
    */
    /* cwal_size_t sz; */
    cwal_array *ar2;
    MARKER("Pushing subscope.\n");
    rc = cwal_scope_push(e, 0);
    RC;
    v = cwal_new_string_value(e, "subscoped", 0);
    APP(v);

    ar2 = cwal_new_array(e);
    v = cwal_array_value(ar2);
    assert(0==cwal_value_refcount(v));

    if(1){
      cwal_value * o2;
      cwal_string * estr;
      MARKER("And now for multi-scope distance...\n");
      rc = cwal_scope_push(e,0);
      RC;
      cwal_new_string(e, "alone", 5);
      rc = cwal_scope_push(e,0);
      RC;

      v = cwal_new_integer(e, 73);
      o2 = cwal_new_object_value(e);
      rc = cwal_prop_set( o2, "myInt", 5, v );
      RC;
      rc = cwal_prop_set( o2, "true", 4, cwal_value_true() );
      RC;
            
      rc = cwal_array_append(ar2, cwal_array_value(ar));
      RC;
      SET("subArray",cwal_array_value(ar2));

      SET("subSubObj",o2);
      SET("subSubObjB",o2);
      APP(o2);

      estr = cwal_new_string(e,"",0);
      rc = strcmp("",cwal_string_cstr(estr));
      assert( 0==rc );
      rc = cwal_prop_set( o2, "emptyString", 11, cwal_string_value(estr) );

      rc = cwal_scope_pop(e);
      RC;
      rc = cwal_scope_pop(e);
      RC;
      if(0){
        cwal_size_t sz;
        MARKER("Sweeping subscope. We should not see any freeing going on here...\n");
        sz = cwal_engine_sweep(e);
        MARKER("Sweep count: %u\n",(unsigned)sz);
        RC;
      }
    }

    if(1){
      cwal_size_t sz;
      cwal_new_integer(e, 32000)
        /* Just to see it get swept up. It must be a value
           which will allocate (not builtin). */;
      MARKER("Sweeping subscope. We should see at least one clean-up here...\n");
      sz = cwal_engine_sweep(e);
      MARKER("Sweep count: %u\n",(unsigned)sz);
      assert(sz>0);
    }
    MARKER("Popping subscope.\n");
    rc = cwal_scope_pop(e);
    RC;

    MARKER("Popped subscope.\n");
    assert(cwal_weakref_value(wr));

    if(1){
      cwal_value const * check;
      cwal_scope * s = cwal_scope_current_get(e);
      int sweepCount = 0;
#if 1
      /* We need at least one var to see this in action... */
      rc = cwal_scope_chain_set(s, 0, "foo", 3, cwal_value_true());
      RC;
      rc = cwal_scope_chain_set(s, 0, "bar", 3, cwal_value_false());
      RC;
#endif
      MARKER("Vacuuming scope #%d.\n", (int)s->level);
      if(cwal_value_get_object(cwal_scope_properties(s))){
        /* using hashes for scope storage breaks this */
        MARKER("Vars before vacuum:\n");
        rc = cwal_json_output_FILE( cwal_scope_properties(s), stdout, 0 );
        RC;
      }
      rc = cwal_engine_vacuum(e, &sweepCount);
      RC;

      assert(!cwal_weakref_value(wr) && "We expect this to have been vacuumed up");

      MARKER("Vacuumed away %d value(s).\n", sweepCount);
      if(cwal_value_get_object(cwal_scope_properties(s))){
        /* using hashes for scope storage breaks this */
        MARKER("Vars after vacuum:\n");
        rc = cwal_json_output_FILE( cwal_scope_properties(s), stdout, 0 );
        RC;
      }

#  define VLIST_COUNT(WHO) check = WHO; rc = 0; while(check){++rc; check=check->right;}(void)0
      VLIST_COUNT(s->mine.headPod);
      assert(2==rc) /* one for each key. The bool values are not owned by the scope */;
      VLIST_COUNT(s->mine.headSafe) /* s->props */;
      assert(rc == 1);
      VLIST_COUNT(s->mine.headObj) /* nothing! */;
      assert(rc == 0);
      assert(cwal_value_true() == cwal_scope_search(s, 0, "foo", 3, 0));
      assert(cwal_value_false() == cwal_scope_search(s, 0, "bar", 3, 0));

#undef VLIST_COUNT
    }
    cwal_weakref_free(e, wr);

  }
#endif

    
#undef SET
#undef APP
#if 0
  cwal_dump_interned_strings_table( e, 1, 30  );
#endif
  MARKER("And now let us pray as the scope gets popped "
         "(or did vacuuming already clean it up?)..."
         " What will happen to our dear graph?\n");
}

static int val_visitor_counter( cwal_value * v, void * state ){
  ++*(cwal_size_t*)state;
  MARKER("Visiting kvp part #%u @%p\n",
         (unsigned) *(cwal_size_t*)state, (void*)v);
  return 0;
}

void e_object2(cwal_engine * e){
  cwal_object * o = cwal_new_object(e);
  cwal_value * ov = cwal_object_value(o);
  cwal_prop_set_v( ov, ov, ov );
  cwal_prop_set( ov, "hi", 2, ov );
#if 0
  cwal_prop_set( ov, "hi", 2, 0 );
  cwal_prop_set_v( ov, ov, 0 );
#endif
  /*cwal_value_unref(ov);*/
}

void e_object(cwal_engine * e){
  cwal_object * o = cwal_new_object(e);
  cwal_value * oV = cwal_object_value(o);
  cwal_string * key = cwal_new_string(e, "key", 3);
  cwal_value * keyV = cwal_string_value(key);
  int rc;
  cwal_size_t pCount = 0, x;
  assert(o);
  assert(0==cwal_props_count(oV));
  rc = cwal_prop_set(oV, "foo", 3, cwal_value_true());
  RC;
  ++pCount;
  assert(1==cwal_props_count(oV));
  //assert(o->base.kvp->value == cwal_value_true());
  rc = cwal_prop_set(oV, "bar", 3, oV);
  RC;
  ++pCount;
  assert(2==cwal_props_count(oV));
  cwal_prop_set_v(oV, keyV, keyV);
  ++pCount;
  assert(3==cwal_props_count(oV));

  MARKER("Running props key visitor...\n");
  x = 0;
  rc = cwal_props_visit_keys( oV, val_visitor_counter, &x );
  RC;
  assert(pCount == x);
  MARKER("Running props value visitor (don't be alarmed by dupe pointers)...\n");
  x = 0;
  rc = cwal_props_visit_values( oV, val_visitor_counter, &x );
  RC;
  assert(pCount == x);

    
  if(1){
    /* Should fail because of the cycle. */
    rc = cwal_json_output_engine( e, oV, &JsonOutOpt );
    putchar('\n')/*assumes output dest is the same :/ */;
    assert(CWAL_RC_CYCLES_DETECTED==rc);
    MARKER("JSON output aborted because of cycles. (This was expected.)\n");
  }
}

void e_buffer_format( cwal_engine * e ){

  cwal_buffer b = cwal_buffer_empty;
  cwal_value * argv[7] = {
  NULL,NULL,NULL,NULL,NULL,
  NULL,NULL
  };
  CWAL_UNUSED_VAR int rc;
  char const * fmt;
  cwal_size_t flen;
  const cwal_size_t argc = sizeof(argv)/sizeof(argv[0]);

  assert(!cwal_buffer_value(&b));
  rc = cwal_buffer_reserve(e, &b, 100);
  assert(!rc);

#define FMT(X) ((fmt=X), flen=strlen(X))
#define DUMP(F)                                                         \
  FMT(F);                                                               \
  rc = cwal_buffer_format(e, &b, fmt, flen, argc, argv);                \
  MARKER("\n\tFormat string: [%s]\n\tBuffer contents: <<<%.*s>>>\n",    \
         fmt, (int)b.used, (char const *)b.mem);                        \
  b.used = 0;                                                           \
  assert(0==rc)

  argv[0] = cwal_new_integer(e, 42);
  argv[1] = cwal_new_string_value(e, "string formatter", 16);
  argv[2] = cwal_new_string_value(e, "abcde", 5);
  argv[3] = cwal_new_double(e, 19.17);
  argv[4] = cwal_value_true();
  argv[5] = cwal_value_false();
  argv[6] = cwal_new_double(e, -13.19);
  DUMP("%1$y %1$p %1$d %1$03d %1$03X %1$04x");
  DUMP("(%1$10d) (%1$-10d)");
  DUMP("(%1$+10d) (%1$+-10d)");
  DUMP("%1$-10d");
  DUMP("%1$05d");
  DUMP("%1$0.5f");
  DUMP("%2$s");
  DUMP("%2$s and %2$s");
  DUMP("(%3$-10s) (%3$10s)");
  DUMP("(%3$10s) (%3$-10s)");
  DUMP("(%3$10.3s) (%3$-10.3s)");
  DUMP("(%3$-10.3s) (%3$10.3s)");
  DUMP("(%3$.4s) (%3$-8.4s)");
  DUMP("%4$f %4$.1f %4$.2f %4$.3f");
  DUMP("%4$+0f (%4$-10.3f) (%4$10.3f) (%4$+010.3f)");
  DUMP("%5$d %5$b %6$b %5$N %5$U");
  DUMP("%6$+f");
  DUMP("%7$+f");
#if 0
  /* must fail: */
  DUMP("%1$d %10$d 2nd formatter is invalid");
#endif
#undef FMT
#undef DUMP
  cwal_buffer_reserve(e, &b, 0);
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

  {
    char const * key = "foo";
    cwal_size_t const keyLen = strlen(key);
    cwal_string * xstr = cwal_new_xstring(e, key, keyLen);
    CWAL_UNUSED_VAR cwal_string * str;
    char const * x = cwal_string_cstr(xstr);
    assert(x == key);
    assert(keyLen==cwal_string_length_bytes(xstr));
    assert(xstr->length & keyLen);
    assert(xstr->length > keyLen);
    assert(0==cwal_compare_str_cstr(xstr, key, keyLen));
    assert(0==cwal_compare_str_cstr(xstr, "foo", 3));
    assert(0!=cwal_compare_str_cstr(xstr,"foo",2));
    assert(0!=cwal_compare_str_cstr(xstr,"bar",3));
    MARKER("xstr->length=%d => %u\n", (int)xstr->length, (unsigned)cwal_string_length_bytes(xstr));
    MARKER("xstr=[%s]\n", x);
    str = cwal_new_string( e, key, keyLen );
    assert(0==cwal_value_compare(cwal_string_value(xstr),
                                 cwal_string_value(str)));
#if 0
    cwal_string_unref(xstr);
    cwal_string_unref(str);
#endif
  }
}

int my_callback( cwal_callback_args const * args, cwal_value **rv ){
  NativeType * nt;
  int rc;
  cwal_value * v = 0;
  cwal_value * funcV = cwal_function_value(args->callee);
  cwal_kvp const * kvp;
  MARKER("argc=%u\n", args->argc);
  nt = (NativeType *)args->state;
  MARKER("function state: NativeType@%p x=%d, y=%d\n", (void*)nt, nt->x, nt->y);

    
  kvp = cwal_scope_search_kvp( args->scope, 0, "myKey", 5, 0 );
  rc = kvp ? 0 : CWAL_RC_NOT_FOUND;
  RC;
  v = cwal_kvp_value(kvp);
  assert(v && (cwal_value_true()==v));

  {
    cwal_value * keyCheck = cwal_new_string_value(args->engine, "funcProp", 8);
    v = cwal_prop_get( funcV, "funcProp", 8 );
    assert(cwal_value_true()==v);
    v = cwal_prop_get( funcV, "funcProp", 8 );
    assert(cwal_value_true()==v);
    
    {
      v = cwal_prop_get_v( funcV, keyCheck );
      assert(v);
      assert(cwal_value_true()==v);
    }

    rc = cwal_prop_set_v( funcV, keyCheck, NULL );
    RC;
    v = cwal_prop_get( funcV, "funcProp", 8 );
    assert(!v);
    rc = cwal_prop_set( funcV, "funcProp", 8, NULL );
    assert(CWAL_RC_NOT_FOUND==rc);
  }

  /* Check for magic properties which were set by our
     init code... */
  {
    cwal_value * intKey = cwal_new_integer(args->engine,42);
    assert(intKey);
    cwal_value_ref( intKey );
    v = cwal_prop_get_v( funcV, intKey );
    assert(cwal_value_true()==v);
    rc = cwal_prop_set_v( funcV, intKey, NULL );
    RC;
    cwal_value_unref( intKey );
    v = cwal_prop_get_v( funcV, funcV );
    assert(!v);
  }


  {
    rc = cwal_prop_set_v( funcV, funcV, funcV );
    RC;
    v = cwal_prop_get_v( funcV, funcV );
    assert(v == funcV);
    MARKER("Woohoo: func as a key :)\n");
    rc = cwal_prop_set_v( funcV, funcV, NULL );
    RC;
    rc = cwal_prop_set_v( funcV, funcV, NULL );
    /* arguable behaviour:*/ assert(CWAL_RC_NOT_FOUND==rc);
    v = cwal_prop_get_v( funcV, funcV );
    assert(!v);
  }

  rc = cwal_props_clear( funcV );
  RC;

  *rv = cwal_new_string_value(args->engine,"function result",15);
  assert(*rv);
  return CWAL_RC_OK;
}

void e_function(cwal_engine * e){
  cwal_value * fv;
  cwal_function * f;
  cwal_scope * scope = 0;
  int rc;
  NativeType* n;
  cwal_value * v;

  rc = cwal_scope_push(e, &scope);
  RC;
  assert(0!=scope);
  {
    cwal_value * key1 = cwal_new_string_value( e, "myKey", 5);
    cwal_value_ref(key1);
    rc = cwal_var_decl_v( e, scope, key1, cwal_value_true(), 0 );
    RC;
    rc = cwal_var_decl_v( e, scope, key1, cwal_value_null(), 0 );
    assert(CWAL_RC_ALREADY_EXISTS==rc);

    v = cwal_scope_search_v(scope, 0, key1, 0);
    cwal_value_unref(key1);
    key1 = 0;
    assert(cwal_value_true()==v);
    
    n = cwal_malloc(e, sizeof(NativeType));
    assert(n);
    *n = NativeType_empty;
    n->x = -42;
    n->y = 17;
    fv = cwal_new_function_value(e, my_callback,
                                 n, finalizer_NativeType, &NativeType_empty);
    f = cwal_value_get_function(fv);
    assert(f && fv);
    rc = cwal_prop_set( fv, "padding1", 8, cwal_value_null() );
    RC;
    rc = cwal_prop_set( fv, "funcProp", 8, cwal_value_true() );
    RC;
    rc = cwal_prop_set( fv, "padding2", 8, cwal_value_null() );
    RC;
    rc = cwal_prop_set_v( fv, cwal_new_integer(e,42), cwal_value_true() );
    RC;

    cwal_value_ref(fv) /* Very important if property fv[fv] === fv!!!! */;
    rc = cwal_function_call_in_scopef(scope, f, NULL, NULL, fv, NULL);
    RC;
  }
  rc = cwal_scope_pop(e);
  RC;
}

void e_exception(cwal_engine * e){
  /* Make sure exceptions get moved down the scope stack
     as scopes are popped.
  */
  cwal_value * xv;
  CWAL_UNUSED_VAR cwal_value * xv2;
  int rc;
  cwal_weakref * wr;
  rc = cwal_scope_push(e, 0); RC;
  rc = cwal_scope_push(e, 0); RC;
  rc = cwal_scope_push(e, 0); RC;
  rc = cwal_exception_setf( e, CWAL_RC_TYPE, "Type exception: #%d", CWAL_RC_TYPE);
  assert(CWAL_RC_EXCEPTION==rc);
  xv = cwal_exception_get(e);
  assert(xv);
  wr = cwal_weakref_new(xv);
  assert(wr);
  MARKER("exception value type name = %s\n", cwal_value_type_name(xv));
  assert(cwal_value_is_exception(xv));
#define POP rc = cwal_scope_pop(e); RC; assert(xv->scope==e->current)
  POP;
  POP;
  POP;
  xv2 = cwal_exception_get(e);
  assert(xv2==xv);
  assert(cwal_weakref_value(wr)==xv2);
  cwal_weakref_free(e, wr);
}


void e_prototypes( cwal_engine * e ){
  cwal_object * o1 = cwal_new_object(e);
  cwal_object * o2 = cwal_new_object(e);
  cwal_value * ov1 = cwal_object_value(o1);
  cwal_value * ov2 = cwal_object_value(o2);
  CWAL_UNUSED_VAR cwal_value * v;
  char const * key = "protoProp";
  cwal_size_t keyLen = cwal_strlen(key);
  CWAL_UNUSED_VAR int rc;
  
  assert( !cwal_value_derives_from(e, ov1, ov2) );
  rc = cwal_value_prototype_set( ov1, ov2 );
  assert(!rc && "Prototype set failed.");
  assert(ov2 == cwal_value_prototype_get(e, ov1));
  assert(NULL == cwal_value_prototype_get(e, ov2));
  assert( cwal_value_derives_from(e, ov1, ov2) );

  cwal_prop_set(ov2, key, keyLen, cwal_value_true() );
  v = cwal_prop_get( ov1, key, keyLen );
  assert(v && "search through prototype failed.");
  assert(!cwal_prop_has(ov1, key, keyLen, 0));
  assert(cwal_prop_has(ov2, key, keyLen, 0));

  rc = cwal_value_prototype_set( ov2, ov2 );
  assert( CWAL_RC_MISUSE == rc );

#if 0
  /* i removed circular prototypes - too easy to
     get into loops.
  */
  rc = cwal_value_prototype_set( ov2, ov1 );
  assert(!rc && "circular prototypes failed.");
  assert(ov1 == cwal_value_prototype_get(ov2));
  assert(ov2 == cwal_value_prototype_get(ov1));
#endif    
}

void e_sweeping1( cwal_engine * e ){
  cwal_scope * s = NULL;
  cwal_size_t i, x, outer = 100, inner = 10;
  int rc = cwal_scope_push( e, &s );
  RC;
  assert(s);
  MARKER("If recycling is enabled, the following should add "
         "very few malloc()s (check valgrind stats).\n");
  for( i = 1; i <= outer; ++i ){
    for( x = 1; x <= inner; ++x ){
      CWAL_UNUSED_VAR cwal_value * v = cwal_new_integer(e, 100 * (i+x));
      assert(v);
      assert(0 == cwal_value_refcount(v));
    }
    {
      CWAL_UNUSED_VAR cwal_size_t sz;
      /*MARKER("Sweeping. Expecting %d sweep-ups...\n",inner);*/
      sz = cwal_scope_sweep( s );
      /*MARKER("Sweeping. Expecting %d, got %u sweep-ups...\n",
        inner, (unsigned)sz);*/
      assert(sz == inner)
        /* LOL: if v is one of the built-in constants this
           assertion fails b/c 0/1 are a special integer
           values which never gets scoped. */;
    }
  }
  rc = cwal_scope_pop(e);
  RC;

}

void e_basic_assertions( CWAL_UNUSED_VAR cwal_engine *ie ){
  cwal_int_t i = 0;
  cwal_double_t d = 0;
  CWAL_UNUSED_VAR int rc;
  rc = cwal_cstr_to_int( "342", 3, &i );
  assert(0==rc);
  assert(342==i);

  rc = cwal_cstr_to_int( "-42", 3, &i );
  assert(0==rc);
  assert(-42==i);

  /* Interesting... on my 32-bit box the double
     comparisons fail. Printf'ing the values shows
     the values are correct but the comparison
     assertions fail. This does not happen on my
     64-bit box.
  */
  rc = cwal_cstr_to_double( "1.2", 3, &d );
  /* MARKER("d=%f\n", d); */
  assert(0==rc);
  assert(1.2==d);

  rc = cwal_cstr_to_double( "1.3abcd", 3, &d );
  /* MARKER("d=%f\n", d); */
  assert(0==rc);
  assert(1.3==d);
    
  rc = cwal_cstr_to_double( "-31.3", 5, &d );
  assert(0==rc);
  assert(-31.3==d);

  rc = cwal_cstr_to_double( "+17.17 junk", 6, &d );
  assert(0==rc);
  assert(17.17==d);

  rc = cwal_cstr_to_double( "-1333", 5, &d );
  assert(0==rc);
  assert(-1333==d);

  rc = cwal_cstr_to_double( "-1333.-1", 8, &d );
  assert(CWAL_RC_TYPE==rc);

  assert(cwal_new_string_value(ie, "abc", 0) == cwal_new_string_value(ie,0,0));
}

#if 0
/* do not use - fundamentally broken interface */
void e_utf8_char_next( cwal_engine *ie ){
  char const * str = "hi\0→wÜrld";
  char const * const end = str + 12 /* careful! */;
  char const * pos = str;
  unsigned int unicode = 0;
  int len = 0, bytes = 0, utfLen = 0;
  assert(9U == cwal_strlen_utf8(pos, (end-str)));
  for( ; 0<(len = cwal_utf8_char_next(pos, end, &unicode));
       pos += len, bytes += len, ++utfLen){
    MARKER("Got len-%d character: %.*s (0x%x)\n", (int)len, (int)len, pos, unicode);
    assert(len>=1 && len<=4) /* a NUL byte has a len of 1! */;
  }
  assert(12 == bytes);
  assert(9 == utfLen);
  {
    cwal_string * s = cwal_new_string(ie, str, end-str);
    assert(s);
    assert(!cwal_string_is_ascii(s));
    cwal_string_unref(s);
  }
}
#endif

void e_unique(cwal_engine * e){

  cwal_value * u1 = cwal_new_unique(e, 0);
  cwal_value * u2 = cwal_new_unique(e, u1);
  CWAL_UNUSED_VAR int cmp = cwal_value_compare(u1,u2);
  CWAL_UNUSED_VAR char const * tname = cwal_value_type_name(u1);
  CWAL_UNUSED_VAR int rc;
  cwal_value_ref(u1);
  cwal_value_ref(u2);
  assert(0!=cmp);
  assert( cwal_value_is_unique(u1) );
  assert( 0 == cwal_compare_cstr("unique", 6, tname, cwal_strlen(tname)) );
  assert( cwal_value_compare(u2,u1)>0 ? (cmp<0) : (cmp>0) );
  assert( 0 == cwal_value_compare(u1,u1) );
  assert( 0 != cwal_value_compare(u1, cwal_value_true()) );
  assert( cwal_value_get_bool(u1) );

  assert( u1 == cwal_unique_wrapped_get(u2) );
  assert( 0 == cwal_unique_wrapped_get(u1) );
  assert( 2 == cwal_value_refcount(u1) );
  rc = cwal_unique_wrapped_set(u2, 0);
  assert(!rc);
  assert( 1 == cwal_value_refcount(u1) );
  rc = cwal_unique_wrapped_set(u2, u2);
  assert(CWAL_RC_CYCLES_DETECTED==rc);
  cwal_value_unref(u1);
  cwal_value_unref(u2);
}

void e_call( cwal_engine * e, void (*func)( cwal_engine * ), char const * descr ){
  int rc;
  cwal_scope _p1 = cwal_scope_empty;
  cwal_scope _p2 = cwal_scope_empty;
  cwal_scope * s1 = &_p1, * s2 = &_p2;
  MARKER("Pushing scope for test [%s]...\n", descr);
  /* Workaround:
     Push 2 scopes to keep the result value from poluting our global
     so that i can better watch the lifetimes.
  */
  rc = cwal_scope_push(e, &s1); RC;
  rc = cwal_scope_push(e, &s2); RC;
  func(e);
  MARKER("Popping scope [%s]...\n", descr);
  rc = cwal_scope_pop(e); RC;
  rc = cwal_scope_pop(e); RC;
  MARKER("Scope popped [%s].\n", descr);
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
  MARKER("cwal_engine_vtab::hook::on_init() callback. e=%p state=%p\n",
         (void const *)e, vtab->hook.init_state);
  JsonOutOpt.addNewline = 1;
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
    REMAX(DOUBLE,20)/* might also include x-/z-strings */;
    REMAX(FUNCTION,20) /* might include: hash, native */;
    REMAX(ARRAY,20);
    REMAX(OBJECT,20) /* might include: buffer */;
    REMAX(INTEGER,40) /* might include: double */;
    REMAX(TUPLE,20) /* might include: propref, x-/z-strings */;
    REMAX(PROPREF,20) /* might include: tuple, x-/z-strings */;
  }
#undef REMAX
  return 0;
}


static int my_scope_hook_push( cwal_scope * s,
                               CWAL_UNUSED_VAR void * clientState ){
  MARKER("Pushing scope level %d\n", (int)s->level);
  return 0;
}
static void my_scope_hook_pop( cwal_scope const * s,
                               CWAL_UNUSED_VAR void * clientState ){
  MARKER("Popping scope level %d\n", (int)s->level);
  if(1==s->level){
    MARKER("pop hook: the engine is shutting down.\n");
  }
}

void e_main(){
  cwal_engine_vtab vtab = cwal_engine_vtab_basic;
  cwal_engine E = cwal_engine_empty;
  cwal_engine * e =
    1 ? &E : 0;
  int rc;
  JsonOutOpt.cyclesAsStrings = 0;
  assert(cwal_output_f_FILE == vtab.outputer.output);
  assert(cwal_finalizer_f_fclose == vtab.outputer.state.finalize);
  vtab.outputer.state.data = stdout;
  vtab.tracer = cwal_engine_tracer_FILE;
  vtab.tracer.state = stdout;
  vtab.hook.on_init = e_init_engine;
  vtab.hook.init_state = e;
  if(App.logScopePushPop){
    vtab.hook.scope_push = my_scope_hook_push;
    vtab.hook.scope_pop = my_scope_hook_pop;
  }

  if(1){
    if(0) vtab.memcap.maxSingleAllocSize = 48;
    if(0) vtab.memcap.maxTotalAllocCount = 10;
    vtab.memcap.maxConcurrentMem = 1024 * 50;
  }

  rc = cwal_engine_init( &e, &vtab );
  RC;

  cwal_outputf( e, "e_main(): %!j\n", "hi, world!" );

#if 0
  cwal_new_string( e, "hi, world!", 10 );
  cwal_new_string( e, "foo", 3 )
    /* for checking if interning is working. */;
#endif

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

#if CWAL_VOID_PTR_IS_BIG
  /*
    The double-type assertions in e_basic_assertions() are failing
    on my 32-bit box even though the values are provably correct
    (meaning that printf() shows the expected values).
  */
  if(1) e_call( e, e_basic_assertions,
                "basic assertions");
#endif
  if(1) e_call( e, e_ptr_table,
                "ptr_table stuff");
  if(1) e_call( e, e_interned_strings,
                "interned strings tests..." );
  if(1) e_call( e, e_random_strings,
                "random strings test..." );
  if(0) goto end;
  if(1) e_call( e, e_json1,
                "cwal_json_output()..." );
  if(1) e_call( e, e_object2,
                "Basic Object cycle of death...");
  if(1) e_call( e, e_object,
                "Object basics...");
  if(1) e_call( e, e_relaunch1,
                "Cycle-mania...");
  if(1) e_call( e, e_comparisons,
                "Value comparison...");
  if(1) e_call( e, e_function,
                "function() calls...");
  if(1) e_call( e, e_exception,
                "exception lifetime...");
  if(1) e_call( e, e_prototypes,
                "prototypes...");
  if(1) e_call( e, e_sweeping1,
                "alloc/sweep loops...");
  if(1) e_call( e, e_buffer_format,
                "cwal_buffer_format()...");
#if 0
  if(1) e_call( e, e_utf8_char_next,
                "cwal_utf8_char_next()...");
#endif
  if(1) e_call( e, e_unique,
                "cwal_new_unique()...");

  MARKER("Shutting down engine. All values (except maybe the "
         "exception/result values) should have been cleaned "
         "up by now-dead scopes by now.\n");

  end:
  if(e->interned.pg.head){
    cwal_dump_interned_strings_table( e, 1, 30 );
  }
  if(App.showMetrics){
    cwal_dump_allocation_metrics( e );
  }
  rc = cwal_engine_destroy( e );
  RC;
}

void show_sizeofs(){
  cwal_size_t total = 0;
  cwal_engine e = cwal_engine_empty;
  MARKER("Various library-level sizeof()s...\n");
#define C(M) MARKER(#M"=%u\n", M);
  C(CWAL_SIZE_T_BITS);
  C(CWAL_INT_T_BITS);
  C(CWAL_VOID_PTR_IS_BIG);
    
#undef C
#define SO(T) total += sizeof(T); MARKER("sizeof(%s)=%u\n", #T, (unsigned int)sizeof(T))
  SO(void*);
  SO(cwal_array);
  SO(cwal_buffer);
  SO(cwal_callback_args);
  SO(cwal_double_t);
  SO(cwal_engine);
  SO(cwal_engine_vtab);
  SO(cwal_hash);
  SO(cwal_htable);
  SO(cwal_int_t);
  SO(cwal_kvp);
  SO(cwal_list);
  SO(cwal_memchunk_overlay);
  SO(cwal_native);
  SO(cwal_obase);
  SO(cwal_object);
  SO(cwal_propref);
  SO(cwal_ptr_page);
  SO(cwal_ptr_table);
  SO(cwal_recycler);
  SO(cwal_scope);
  SO(cwal_size_t);
  SO(cwal_state);
  SO(cwal_string);
  SO(cwal_tuple);
  SO(cwal_value);
  SO(cwal_value_vtab);
  SO(cwal_weakref);
  SO(e.metrics);
  SO(e.recycler);
  SO(e.weakr);
  SO(e.metrics);
  SO(e.recycler);
  SO(e.memcap);
#undef SO
  MARKER("sizeof() total: %u\n", (unsigned)total );
}

int main(int argc, char const * const * argv)
{
  int i;
  char showSizes = 0;
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
    else if(0==strcmp("-z",arg)){
      showSizes = 1;
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
  }

  /*test_toker();*/
  e_main();
  if(showSizes) show_sizeofs();
  MARKER("Done!\n");
  return 0;
}
