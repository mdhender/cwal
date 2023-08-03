/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

#include <cassert>
#include <iostream>
#include <fstream>
#include <list>
#include <cstdlib> /* EXIT_SUCCESS, EXIT_FAILURE */

#define CERR std::cerr << __FILE__ << ':' << std::dec << __LINE__ << ':' << __FUNCTION__ << "(): "
#define COUT std::cout << __FILE__ << ':' << std::dec << __LINE__ << ':' << __FUNCTION__ << "(): "
#include "s2_internal.h"
#include "wh/cwal/cwal_convert.hpp"

/**
   Called via cwal_engine_init().
*/
static int my_init_engine(cwal_engine *e, cwal_engine_vtab *){
  int rc = 0;
  int32_t featureFlags = 1
    ? 0
    : CWAL_FEATURE_ZERO_STRINGS_AT_CLEANUP;
  cwal_memchunk_config mcConfig = cwal_memchunk_config_empty;
  
  if(0) featureFlags |= CWAL_FEATURE_INTERN_STRINGS;
  cwal_engine_feature_flags(e, featureFlags);

  cwal_engine_memchunk_config(e, &mcConfig);
#define REMAX(T,N) cwal_engine_recycle_max( e, CWAL_TYPE_ ## T, (N) )
  REMAX(UNDEF,0);
  if(1){
    /* A close guess based on the post-20141129 model...  List them in
       "priority order," highest priority last.  Lower prio ones might
       get trumped by a higher prio one: they get grouped based on the
       platform's sizeof() of their concrete underlying bytes.
    */
    REMAX(UNIQUE,20)/* will end up being trumped by integer (32-bit)
                       and/or double (64-bit) */;
    REMAX(KVP,80) /* guaranteed individual recycler */;
    REMAX(WEAKREF,30) /* guaranteed individual recycler */;
    REMAX(STRING,50) /* guaranteed individual recycler */;
    REMAX(EXCEPTION,3);
    REMAX(HASH,15) /* might also include: function, native, buffer */;
    REMAX(BUFFER,20) /* might also include: function, native, buffer, hash */ ;
    REMAX(XSTRING,20 /* also Z-strings and tuples, might also include doubles */);
    REMAX(NATIVE,20)  /* might also include: function, hash */;
    REMAX(DOUBLE,50)/* might also include z-/x-strings,
                       integer (64-bit), unique (64-bit)*/;
    REMAX(FUNCTION,50) /* might include: hash, native, buffer */;
    REMAX(ARRAY,30);
    REMAX(OBJECT,30) /* might include: buffer */;
    REMAX(INTEGER,80) /* might include: double, unique */;
    REMAX(TUPLE,30) /* also x-/z-strings, might include: double */;
    REMAX(PROPREF,30) /* might include x-/z-strings, tuple */;
  }
#undef REMAX

  /*
    Reminder to self: s2 will nuke the top scope so that it can
    push one of its own, so don't allocate any values here - they
    will be destroyed right after this function returns!
  */
  return rc;
}

/** String-is-internable predicate for cwal_engine. */
static bool cstr_is_internable( void * /*state*/,
                                char const * str_,
                                cwal_size_t len ){
  enum { MaxLen = 32U };
  unsigned char const * str = (unsigned char const *)str_;
  assert(str);
  assert(len>0);
  if(len>MaxLen) return 0;
  else if(1==len){
    return (*str>127) ? 0 : 1;
    /* Noting that (since this code was written) cwal compiles all
       length-1 ASCII strings in as constants, so this routine will
       never be called for such strings, as those don't get interned.
    */
  }
  /* else if(len<4) return 1; */
  else{
    /* Read a UTF8 identifier... */
    char const * zEnd = str_+len;
    char const * tail = str_;
    s2_read_identifier(str_, zEnd, &tail);
    if((tail>str_)
       && (len==(cwal_size_t)(tail-str_))
       ){
      /* MARKER(("internable: %.*s\n", (int)len, str_)); */
      return 1;
    }
    /* MARKER(("Not internable: %.*s\n", (int)len, str_)); */
    return 0;
  }
}

struct MyType {
  static const void * TypeID;
  int value;
  MyType() : value(1){
    COUT << "MyType@"<<(void const *)this<<"::MyType()\n";
  }
  ~MyType(){
    COUT << "MyType@"<<(void const *)this<<"::~MyType()\n";
  }

  int func0(){
    COUT << "MyType@"<<(void const *)this<<"::func0()\n";
    return this->value;
  }

  int func0c() const{
    COUT << "MyType@"<<(void const *)this<<"::func0c()\n";
    return this->value;
  }

  int func1c(int i) const{
    COUT << "MyType@"<<(void const *)this<<"::func1c("<<i<<")\n";
    return this->value * i;
  }

  double funcStr(std::string const &str) const{
    COUT << "MyType@"<<(void const *)this<<"::funcStr("<<str<<")\n";
    return this->value;
  }
};

namespace cwal {
  CWAL_TYPE_NAME_DECL((MyType));
  CWAL_TYPE_NAME_IMPL2((MyType),"MyType");

  template <>
  struct CwalToNative<MyType> : CwalToNative_NativeTypeID<MyType>
  {};
}
void const * MyType::TypeID = cwal::TypeName<MyType>::TypeID;

int throwing_callback(cwal_callback_args const * args, cwal_value **/*rv*/){
  cwal::Argv av(args);
  MyType & my = av.native<MyType &>(0);
  assert( (cwal::ArgAt_IsA<0,MyType>()(args)) );
  assert( (!cwal::ArgAt_IsA<1,MyType>()(args)) );
  assert( (!cwal::ArgAt_IsA<0,int>()(args)) );
  assert( (cwal::Argv_AndN<
             CWAL_TYPELIST((
                            cwal::Argv_Length<1,2>,
                            cwal::ArgAt_IsA<0,MyType>
                            ))>()(args)
           ));
  assert( !(cwal::Argv_OrN<
             CWAL_TYPELIST((
                            cwal::Argv_Length<0>,
                            cwal::ArgAt_IsA<1,MyType>,
                            cwal::ArgAt_IsA<0,double>
                            ))>()(args)
          ));

  COUT << "callback arg to "<<__FUNCTION__<<"() "<<cwal::TypeName<MyType>::Value<<"="<<&my<<"\n";
  assert(0 == av.native<MyType>(1));
  throw std::runtime_error("Just testing.")
    /************************************************************
      ACHTUNG ACHTUNG ACHTUNG: (and, again, ACHTUNG)

      Never, ever, EVER (EVER EVER EVER) throw an exception from a C
      callback UNLESS it is wrapped (tightly) in a "catcher" such as
      cwal::CbCatcher. Failing to heed that WILL lead to corruption of
      cwal's internal state. The job of CbCatcher is to catch C++
      exceptions and convert them to cwal (C) exceptions.
    ************************************************************/;
  return 0;
}


int test_binding_0(){
  COUT << "test_binding_0()\n";
  return 42;
}

int test_binding_1(int x){
  COUT << "test_binding_1("<<x<<")\n";
  return 2 * x;
}

void test_binding_1v(int x){
  COUT << "test_binding_1v("<<x<<")\n";
}


double test_binding_3(int x, double d, char const * str){
  COUT << "test_binding_1v("<<x<<", " << d << ", " << str<<")\n";
  return d;
}

static void test1(cwal_engine * e){
#if 0
  cwal_scope SC = cwal_scope_empty;
  cwal_scope * sc = &SC;
  int rc;
  rc = cwal_scope_push(e, &sc);
  assert(!rc);
#endif

  cwal_value * v = 0;
  v = cwal::toCwal<int32_t>(e, 4);
  assert(v);
  cwal_value_ref(v);
  s2_dump_val(v, "v");
  int32_t i = cwal::toNative<int32_t>(e, v);
  COUT << "Round-trip integer: "<<i<<'\n';
  cwal_value_unref(v);
  v = 0;

  MyType * my = new MyType;
  my->value = 42;
  cwal_value * myV;
  myV = cwal_new_native_value(e, my,
                                  cwal::cwal_finalizer_f_delete<MyType>,
                                  MyType::TypeID);
  assert(cwal_value_get_native(myV));
  assert(cwal_native_get(cwal_value_get_native(myV), MyType::TypeID));
  assert(myV);
  cwal_value_ref(myV);
  assert( my == cwal::toNative<MyType>(e, myV) );

  if(1){
    char const forceNullDeref = 0;
    cwal_function * f = cwal_new_function(e, cwal::CbCatcher<throwing_callback>::callback,
                                          0, 0, 0);
    assert(f);
    S2_UNUSED_VAR int rc = cwal_function_call(f, myV, 0, forceNullDeref ? 0 : 1, &myV);
    assert(CWAL_RC_EXCEPTION==rc);
    s2_dump_val(cwal_exception_get(e),"exception");
    cwal_exception_set(e, 0);
    cwal_value_unref(cwal_function_value(f));
  }



  if(1){
    cwal_value * rv = 0;
    cwal_value * argList[] = {0,0};
    typedef cwal::FunctionPtr< int (int), test_binding_1> FP0;
    cwal_function * f;
#if 0
    // The "long" way of doing this...
    f = cwal_new_function(e, cwal::ToCb< FP0, 1, 0 >::callback,
                          0, 0, 0);
#else
    // The "short" way...
    f = cwal::newFunction<FP0>(e);
#endif
    assert(f);
    argList[0] = cwal_new_integer(e, 17);
    COUT << "Callback via free function...\n";
    S2_UNUSED_VAR int rc = cwal_function_call(f, myV, &rv, 1, argList);
    assert(!rc);
    assert(rv);
    if( !cwal::sigli::ReturnsVoid<FP0>::Value ) {
      assert(34 == cwal_value_get_integer(rv));
      COUT << "Got expected int return.\n";
    }
    cwal_engine_sweep(e);
  }

  if(1){
    cwal_value * rv = 0;
    cwal_value * argList[] = {0,0};
    typedef cwal::MethodPtr<MyType const, int(int), &MyType::func1c> tFunc;
    cwal_function * f = cwal::newFunction<tFunc>(e);
    assert(f);
    argList[0] = cwal_new_integer(e, 2);
    S2_UNUSED_VAR int rc = cwal_function_call(f, myV, &rv, 1, argList);
    assert(!rc);
    assert(rv);
    s2_dump_val(rv, "rv");
    assert(my->value*2 == cwal_value_get_integer(rv));
    COUT << "Got expected int return.\n";
    cwal_engine_sweep(e);
  }

  if(1){
    cwal_value * rv = 0;
    cwal_value * argList[] = {0,0};
    typedef cwal::MethodPtr<MyType const, double(std::string const &), &MyType::funcStr> tFunc;
    cwal_function * f = cwal::newFunction<tFunc>(e);
    assert(f);
    {
      char const * str = "hi via std::string conversion";
      argList[0] = cwal_new_string_value(e, str, cwal_strlen(str));
    }
    S2_UNUSED_VAR int rc = cwal_function_call(f, myV, &rv, 1, argList);
    assert(!rc);
    assert(rv);
    s2_dump_val(rv, "rv");
    assert(cwal_value_is_double(rv));
    assert((double)my->value == cwal_value_get_double(rv));
    COUT << "Got expected double return.\n";
    cwal_engine_sweep(e);
  }

  if(1){
    cwal_value * rv = 0;
    cwal_value * argList[] = {0,0,0};
    typedef cwal::FunctionPtr< double (int, double, char const *), test_binding_3> FP;
    enum { IgnoreResult = 0 };
    typedef cwal::ToCb< FP, 1, IgnoreResult > CB;
    cwal_scope _scope = cwal_scope_empty;
    cwal_scope * scope = &_scope;
    S2_UNUSED_VAR int rc = cwal_scope_push(e, &scope);
    assert(0==rc && "cannot fail if all args are correct.");
    cwal_function * f = cwal_new_function(e, CB::callback,
                                          0, 0, 0);
    assert(f);
    // Note that we're allocating new values here but are being sloppy
    // about referencing them - we'll let the current cwal_scope clean
    // them up when we pop it in a moment.
    argList[0] = cwal_new_integer(e, 17);
    argList[1] = cwal_new_double(e, 12.1);
    argList[2] = cwal_new_string_value(e, "hi, world", 9);
    rc = cwal_function_call(f, myV, &rv, 3, argList);
    assert(!rc);
    assert(rv);
    s2_dump_val(rv, "rv");
    if(IgnoreResult){
      assert(cwal_value_undefined()==rv);
    }else{
      COUT << "C double val=" << cwal_value_get_double(rv) << '\n';
      assert(cwal_value_is_double(rv));
      assert(rv != argList[1] /* b/c of round-trip conversion */);
      assert( 12.1 == cwal_value_get_double(rv) );
    }
    cwal_scope_pop( e );
  }

  COUT << "'my' should get destroyed in a moment...\n";
  cwal_value_unref(myV);


  cwal_engine_sweep(e);
  COUT << "Done with test1()\n";
#if 0
  cwal_scope_pop(e);
#endif
}

static void test2(cwal_engine * ){

  namespace sigli = cwal::sigli;
  namespace tmp = cwal::tmp;

  S2_UNUSED_VAR typedef cwal::MethodSignature<MyType,int (double,char)> MSig;
  S2_UNUSED_VAR typedef cwal::MethodSignature<MyType const,int (double,char)> MSig2;
  assert( 2 == sigli::Arity<MSig>::Value );
  assert( 2 == sigli::Arity<MSig2>::Value );

  S2_UNUSED_VAR typedef cwal::MethodPtr<MyType, int(), &MyType::func0> F0;
  assert(0 == sigli::Arity<F0::SignatureType>::Value);
  assert( !sigli::IsConstMethod<F0>::Value );

  S2_UNUSED_VAR typedef cwal::MethodPtr<MyType const, int(int), &MyType::func1c> F0C;
  assert(1 == sigli::Arity<F0C::SignatureType>::Value);
  assert( sigli::IsConstMethod<F0C>::Value );

  S2_UNUSED_VAR typedef cwal::FunctionSignature<int (cwal_callback_args const *, cwal_value**)> FCB;
  assert( sigli::HasCallbackArgs<FCB>::Value );
  assert( sigli::IsCallback<FCB>::Value );
  assert( -1 == sigli::Arity< FCB >::Value );

  S2_UNUSED_VAR typedef cwal::FunctionSignature<int (int, cwal_callback_args const *, cwal_value**)> PseudoCB;
#if 0
  COUT << "length=" << sigli::Length<PseudoCB>::Value
       << ", cba index="<<sigli::Index<cwal_callback_args const *,PseudoCB>::Value
       << ", rv index="<<sigli::Index<cwal_value **,PseudoCB>::Value
       << ", cbIndex="<<sigli::CallbackArgsIndex<PseudoCB>::Value
       << '\n';
#endif
  assert( !sigli::HasCallbackArgs<PseudoCB>::Value );
  assert( 1 == sigli::CallbackArgsIndex<PseudoCB>::Value );
  assert( !sigli::IsCallback<PseudoCB>::Value );
  assert( 3 == sigli::Arity< PseudoCB >::Value );

  assert( (tmp::SameType<void,void>::Value) );
  assert( !(tmp::SameType<void*,void>::Value) );

  // So why does this template not compile in cwal_convert.hpp???
  typedef
    tmp::IfElse<
      tmp::SameType<double, long double>::Value,
      cwal::Detail::DummyPlaceholder<long double>,
      long double
      >::Type whiskey;
  S2_UNUSED_VAR whiskey x = 1.0;
  assert(x);

  COUT << "Done with test2()\n";
}

static void test3(cwal_engine *){
#if 0
  typedef cwal::CtorArgsProxy<MyType* ()> CtorProxy;
  assert(0==cwal::sigli::Arity<CtorProxy::MethodSigType>::Value);
#endif
}


int main(int argc, char const * const * /*argv*/ ){
  cwal_engine E = cwal_engine_empty;
  cwal_engine * e = &E;
  cwal_engine_vtab vtab = cwal_engine_vtab_basic;
  int rc;

  vtab.tracer = cwal_engine_tracer_FILE;
  vtab.tracer.state = stdout;
  vtab.hook.on_init = my_init_engine;
  vtab.interning.is_internable = cstr_is_internable;

  rc = cwal_engine_init( &e, &vtab );
  if(rc){
    cwal_engine_destroy(e);
    e = 0;
    goto end;
  }

  COUT << "Here we go...\n";

  try {
    test1(e);
    test2(e);
    test3(e);
  }catch(std::exception const & ex){
    CERR << "EXCEPTION: " << ex.what() << '\n';
    rc = CWAL_RC_EXCEPTION;
  }


  end:
  if(e){
    if(!rc && argc>1){
      cwal_dump_allocation_metrics(e);
    }
    cwal_engine_destroy(e);
  }
  return rc ? EXIT_FAILURE : EXIT_SUCCESS;
}
