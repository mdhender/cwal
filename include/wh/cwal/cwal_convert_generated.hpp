/* GENERATED CODE! EDIT AT YOUR OWN RISK! */
#if !defined(DOXYGEN)

template <typename RV>
struct Signature< RV () >
{
    typedef RV ResultType;
    enum { IsConst = 0 };
    typedef void Context;
    typedef RV (*FunctionType)();
    typedef tmp::NilType Head;
    typedef Head Tail;
};
template <typename RV>
struct Signature< RV (*)() > : Signature<RV ()>
{};

template <typename T, typename RV>
struct Signature< RV (T::*)() > : Signature<RV ()>
{
    typedef T Context;
    typedef RV (T::*FunctionType)();
};
template <typename T, typename RV>
struct Signature< RV (T::*)() const > : Signature<RV ()>
{
    typedef T const Context;
    typedef RV (T::*FunctionType)() const;
    enum { IsConst = 1 };
};
//! Specialization for 1 arg(s).
template <typename RV, typename A1>
struct Signature< RV (A1) >
{
    typedef RV ResultType;
    typedef void Context;
    typedef RV (*FunctionType)(A1);
    typedef A1 Head;
    typedef Signature< RV () > Tail;
};

//! Specialization for 1 arg(s).
template <typename RV, typename A1>
struct Signature< RV (*)(A1) > : Signature<RV (A1)>
{};

//! Specialization for T non-const methods taking 1 arg(s).
template <typename T, typename RV, typename A1>
struct Signature< RV (T::*)(A1) > : Signature<RV (A1)>
{
    typedef T Context;
    typedef RV (T::*FunctionType)(A1);
};

//! Specialization for T const methods taking 1 arg(s).
template <typename T, typename RV, typename A1>
struct Signature< RV (T::*)(A1) const > : Signature<RV (A1)>
{
    typedef T const Context;
    typedef RV (T::*FunctionType)(A1) const;
};

//! Specialization for 1 args(s)
template <typename T, typename RV, typename A1>
struct MethodSignature< T, RV (A1) >
  : Signature< RV (T::*)(A1) >
{
};

template <typename T, typename RV, typename A1>
struct MethodSignature< T const, RV (A1) >
  : Signature< RV (T::*)(A1) const >
{
};

template <typename T, typename RV, typename A1 >
struct MethodSignature< T, RV (*)(A1) >
  : MethodSignature< T, RV (A1) >
{
};
template <typename T, typename RV, typename A1 >
struct MethodSignature< T const, RV (*)(A1) >
  : MethodSignature< T const, RV (A1) >
{
};

template <typename T, typename RV, typename A1 >
struct MethodSignature< T, RV (T::*)(A1) >
  : MethodSignature< T, RV (A1) >
{};

template <typename T, typename RV, typename A1 >
struct MethodSignature< T const, RV (T::*)(A1) >
  : MethodSignature< T const, RV (A1) >
{};

namespace Detail {
template <>
struct F2CbProxy<1, false> {
  template <typename FuncPtrT>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 1 == sigli::Arity<FuncPtrT>::Value > AssertArity;
    if(!AssertArity::Value){}
    Argv const av(args);
    typedef typename sigli::At<0,FuncPtrT>::Type A0;
    typedef typename FuncPtrT::ResultType RT;
    *rv = toCwal<RT>( args->engine,
                      (RT)FuncPtrT::Function(
                        av.native<A0>(0)
                      ) );
     return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail

namespace Detail {
template <>
struct F2CbProxy<1, true> {
  template <typename FuncPtrT>
  static int callback(cwal_callback_args const * args, cwal_value **){
    typedef tmp::Assertion< 1 == sigli::Arity<FuncPtrT>::Value > AssertArity;
    if(!AssertArity::Value){}
    Argv const av(args);
    typedef typename sigli::At<0,FuncPtrT>::Type A0;
    FuncPtrT::Function(
      av.native<A0>(0)
    );
    return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail
namespace Detail {
template <>
struct M2CbProxy<1, false> {
  template <typename MemberPtr>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 1 == sigli::Arity<MemberPtr>::Value > AssertArity;
    if(!AssertArity::Value){}
    typedef typename MemberPtr::ResultType RT;
    typedef typename MemberPtr::Context T;
    T * that = toNative<T>(args->engine, args->self);
    if(!that){
      return tossMissingThis<T>(args);
    }else{
      const Argv av(args);
      typedef typename sigli::At<0,MemberPtr>::Type A0;
      *rv = toCwal<RT>( args->engine, (RT)(that->*MemberPtr::Function)(
        av.native<A0>(0)
      ) );
      return 0; // FIXME: check s2_engine_err_has()
    }
  }
};

} // namespace Detail
namespace Detail {
template <>
struct M2CbProxy<1, true> {
  template <typename MemberPtr>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 1 == sigli::Arity<MemberPtr>::Value > AssertArity;
    if(!AssertArity::Value){}
    typedef typename MemberPtr::ResultType RT;
    typedef typename MemberPtr::Context T;
    T * that = toNative<T>(args->engine, args->self);
    if(!that){
      return tossMissingThis<T>(args);
    }else{
      const Argv av(args);
      typedef typename sigli::At<0,MemberPtr>::Type A0;
      (RT)(that->*MemberPtr::Function)(
        av.native<A0>(0)
      );
      *rv = cwal_value_undefined();
      return 0; // FIXME: check s2_engine_err_has()
    }
  }
};
} // namespace Detail
namespace Detail {
template <typename MethodSigT>
struct CtorArgsProxy<1,MethodSigT,true> {
  typedef typename MethodSigT::ResultType ResultType;
  static ResultType construct(cwal_callback_args const * args){
    typedef typename MethodSigT::Context T;
    typedef tmp::Assertion< 1 == sigli::Arity<MethodSigT>::Value > AssertArity;
    if(!AssertArity::Value){}
    ResultType that = 0;
    void * mem = allocFromCwal<T>(args->engine);
      typedef typename sigli::At<0,MethodSigT>::Type A0;
    if(!mem) return NULL;
    try {
      const Argv av(args);
      that = new (mem) T(
        av.native<A0>(0)        );
    }catch(std::exception const &ex){
      cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                          "%s constructor (arity=1) threw: %s",
                           TypeName<T>::Value, ex.what());
    }catch(...){
      cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                          "%s constructor (arity=1) threw an unknown exception",
                          TypeName<T>::Value);
    }
    if(!that) freeFromCwal<T>(args->engine, mem);
    return that;
  }
};

} // namespace Detail
namespace Detail {
template <typename MethodSigT>
struct CtorArgsProxy<1,MethodSigT,false> {
  typedef typename MethodSigT::ResultType ResultType;
  static ResultType construct(cwal_callback_args const * args){
    typedef typename MethodSigT::Context T;
    typedef tmp::Assertion< 1 == sigli::Arity<MethodSigT>::Value > AssertArity;
    if(!AssertArity::Value){}
    ResultType that = 0;
      typedef typename sigli::At<0,MethodSigT>::Type A0;
    try {
      const Argv av(args);
      that = new T(
        av.native<A0>(0)        );
      }catch(std::exception const &ex){
        cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                            "%s constructor (arity=1) threw: %s",
                             TypeName<T>::Value, ex.what());
      }catch(...){
        cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                            "%s constructor (arity=1) threw an unknown exception",
                            TypeName<T>::Value);
      }
      return that;
    }
};

} // namespace Detail
namespace Detail {
template <typename FuncPtrT>
struct QuasiMethodProxy<1, false, FuncPtrT> : CallBackable {
  typedef CallbackSignature SignatureType;
  typedef int ResultType;
  typedef void Context;
  typedef SignatureType::Head Head;
  typedef SignatureType::Tail Tail;

  typedef tmp::Assertion< 1 == sigli::Arity<FuncPtrT>::Value > AssertArity;
  typedef typename sigli::At<0,FuncPtrT>::Type SelfType;
  //typedef tmp::Assertion< tmp::HasPtr< SelfType >::Value  > AssertIsPtr;
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef Dereffer<SelfType> Deref;
    Argv const av(args);
    SelfType that = toNative<SelfType>(args->engine, args->self);
    if(tmp::HasPtr< SelfType >::Value && !Deref()(that)){
      return tossMissingThis<SelfType>(args);
    }
    typedef typename FuncPtrT::ResultType RT;
    *rv = toCwal<RT>( args->engine,
                      (RT)FuncPtrT::Function(
                             that
                      ) );
     return cwal_exception_get(args->engine)
            ? CWAL_RC_EXCEPTION
            : 0;
  }
};
} // namespace Detail

namespace Detail {
template <typename FuncPtrT>
struct QuasiMethodProxy<1, true, FuncPtrT> : CallBackable {
  typedef CallbackSignature SignatureType;
  typedef int ResultType;
  typedef void Context;
  typedef SignatureType::Head Head;
  typedef SignatureType::Tail Tail;
  typedef tmp::Assertion< 1 == sigli::Arity<FuncPtrT>::Value > AssertArity;
  typedef typename sigli::At<0,FuncPtrT>::Type SelfType;
  //typedef tmp::Assertion< tmp::HasPtr< SelfType >::Value  > AssertIsPtr;
  static int callback(cwal_callback_args const * args, cwal_value **){
    Argv const av(args);
    typedef Dereffer<SelfType> Deref;
    SelfType that = toNative<SelfType>(args->engine, args->self);
    if(tmp::HasPtr< SelfType >::Value && !Deref()(that)){
      return tossMissingThis<SelfType>(args);
    }

    FuncPtrT::Function(
      that
    );
    return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail
//! Specialization for 2 arg(s).
template <typename RV, typename A1, typename A2>
struct Signature< RV (A1, A2) >
{
    typedef RV ResultType;
    typedef void Context;
    typedef RV (*FunctionType)(A1, A2);
    typedef A1 Head;
    typedef Signature<RV ( A2)> Tail;
};

//! Specialization for 2 arg(s).
template <typename RV, typename A1, typename A2>
struct Signature< RV (*)(A1, A2) > : Signature<RV (A1, A2)>
{};

//! Specialization for T non-const methods taking 2 arg(s).
template <typename T, typename RV, typename A1, typename A2>
struct Signature< RV (T::*)(A1, A2) > : Signature<RV (A1, A2)>
{
    typedef T Context;
    typedef RV (T::*FunctionType)(A1, A2);
};

//! Specialization for T const methods taking 2 arg(s).
template <typename T, typename RV, typename A1, typename A2>
struct Signature< RV (T::*)(A1, A2) const > : Signature<RV (A1, A2)>
{
    typedef T const Context;
    typedef RV (T::*FunctionType)(A1, A2) const;
};

//! Specialization for 2 args(s)
template <typename T, typename RV, typename A1, typename A2>
struct MethodSignature< T, RV (A1, A2) >
  : Signature< RV (T::*)(A1, A2) >
{
};

template <typename T, typename RV, typename A1, typename A2>
struct MethodSignature< T const, RV (A1, A2) >
  : Signature< RV (T::*)(A1, A2) const >
{
};

template <typename T, typename RV, typename A1, typename A2 >
struct MethodSignature< T, RV (*)(A1, A2) >
  : MethodSignature< T, RV (A1, A2) >
{
};
template <typename T, typename RV, typename A1, typename A2 >
struct MethodSignature< T const, RV (*)(A1, A2) >
  : MethodSignature< T const, RV (A1, A2) >
{
};

template <typename T, typename RV, typename A1, typename A2 >
struct MethodSignature< T, RV (T::*)(A1, A2) >
  : MethodSignature< T, RV (A1, A2) >
{};

template <typename T, typename RV, typename A1, typename A2 >
struct MethodSignature< T const, RV (T::*)(A1, A2) >
  : MethodSignature< T const, RV (A1, A2) >
{};

namespace Detail {
template <>
struct F2CbProxy<2, false> {
  template <typename FuncPtrT>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 2 == sigli::Arity<FuncPtrT>::Value > AssertArity;
    if(!AssertArity::Value){}
    Argv const av(args);
    typedef typename sigli::At<0,FuncPtrT>::Type A0;
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename FuncPtrT::ResultType RT;
    *rv = toCwal<RT>( args->engine,
                      (RT)FuncPtrT::Function(
                        av.native<A0>(0),
                        av.native<A1>(1)
                      ) );
     return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail

namespace Detail {
template <>
struct F2CbProxy<2, true> {
  template <typename FuncPtrT>
  static int callback(cwal_callback_args const * args, cwal_value **){
    typedef tmp::Assertion< 2 == sigli::Arity<FuncPtrT>::Value > AssertArity;
    if(!AssertArity::Value){}
    Argv const av(args);
    typedef typename sigli::At<0,FuncPtrT>::Type A0;
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    FuncPtrT::Function(
      av.native<A0>(0),
      av.native<A1>(1)
    );
    return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail
namespace Detail {
template <>
struct M2CbProxy<2, false> {
  template <typename MemberPtr>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 2 == sigli::Arity<MemberPtr>::Value > AssertArity;
    if(!AssertArity::Value){}
    typedef typename MemberPtr::ResultType RT;
    typedef typename MemberPtr::Context T;
    T * that = toNative<T>(args->engine, args->self);
    if(!that){
      return tossMissingThis<T>(args);
    }else{
      const Argv av(args);
      typedef typename sigli::At<0,MemberPtr>::Type A0;
      typedef typename sigli::At<1,MemberPtr>::Type A1;
      *rv = toCwal<RT>( args->engine, (RT)(that->*MemberPtr::Function)(
        av.native<A0>(0),
        av.native<A1>(1)
      ) );
      return 0; // FIXME: check s2_engine_err_has()
    }
  }
};

} // namespace Detail
namespace Detail {
template <>
struct M2CbProxy<2, true> {
  template <typename MemberPtr>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 2 == sigli::Arity<MemberPtr>::Value > AssertArity;
    if(!AssertArity::Value){}
    typedef typename MemberPtr::ResultType RT;
    typedef typename MemberPtr::Context T;
    T * that = toNative<T>(args->engine, args->self);
    if(!that){
      return tossMissingThis<T>(args);
    }else{
      const Argv av(args);
      typedef typename sigli::At<0,MemberPtr>::Type A0;
      typedef typename sigli::At<1,MemberPtr>::Type A1;
      (RT)(that->*MemberPtr::Function)(
        av.native<A0>(0),
        av.native<A1>(1)
      );
      *rv = cwal_value_undefined();
      return 0; // FIXME: check s2_engine_err_has()
    }
  }
};
} // namespace Detail
namespace Detail {
template <typename MethodSigT>
struct CtorArgsProxy<2,MethodSigT,true> {
  typedef typename MethodSigT::ResultType ResultType;
  static ResultType construct(cwal_callback_args const * args){
    typedef typename MethodSigT::Context T;
    typedef tmp::Assertion< 2 == sigli::Arity<MethodSigT>::Value > AssertArity;
    if(!AssertArity::Value){}
    ResultType that = 0;
    void * mem = allocFromCwal<T>(args->engine);
      typedef typename sigli::At<0,MethodSigT>::Type A0;
      typedef typename sigli::At<1,MethodSigT>::Type A1;
    if(!mem) return NULL;
    try {
      const Argv av(args);
      that = new (mem) T(
        av.native<A0>(0),
        av.native<A1>(1)        );
    }catch(std::exception const &ex){
      cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                          "%s constructor (arity=2) threw: %s",
                           TypeName<T>::Value, ex.what());
    }catch(...){
      cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                          "%s constructor (arity=2) threw an unknown exception",
                          TypeName<T>::Value);
    }
    if(!that) freeFromCwal<T>(args->engine, mem);
    return that;
  }
};

} // namespace Detail
namespace Detail {
template <typename MethodSigT>
struct CtorArgsProxy<2,MethodSigT,false> {
  typedef typename MethodSigT::ResultType ResultType;
  static ResultType construct(cwal_callback_args const * args){
    typedef typename MethodSigT::Context T;
    typedef tmp::Assertion< 2 == sigli::Arity<MethodSigT>::Value > AssertArity;
    if(!AssertArity::Value){}
    ResultType that = 0;
      typedef typename sigli::At<0,MethodSigT>::Type A0;
      typedef typename sigli::At<1,MethodSigT>::Type A1;
    try {
      const Argv av(args);
      that = new T(
        av.native<A0>(0),
        av.native<A1>(1)        );
      }catch(std::exception const &ex){
        cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                            "%s constructor (arity=2) threw: %s",
                             TypeName<T>::Value, ex.what());
      }catch(...){
        cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                            "%s constructor (arity=2) threw an unknown exception",
                            TypeName<T>::Value);
      }
      return that;
    }
};

} // namespace Detail
namespace Detail {
template <typename FuncPtrT>
struct QuasiMethodProxy<2, false, FuncPtrT> : CallBackable {
  typedef CallbackSignature SignatureType;
  typedef int ResultType;
  typedef void Context;
  typedef SignatureType::Head Head;
  typedef SignatureType::Tail Tail;

  typedef tmp::Assertion< 2 == sigli::Arity<FuncPtrT>::Value > AssertArity;
  typedef typename sigli::At<0,FuncPtrT>::Type SelfType;
  //typedef tmp::Assertion< tmp::HasPtr< SelfType >::Value  > AssertIsPtr;
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef Dereffer<SelfType> Deref;
    Argv const av(args);
    SelfType that = toNative<SelfType>(args->engine, args->self);
    if(tmp::HasPtr< SelfType >::Value && !Deref()(that)){
      return tossMissingThis<SelfType>(args);
    }
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename FuncPtrT::ResultType RT;
    *rv = toCwal<RT>( args->engine,
                      (RT)FuncPtrT::Function(
                             that,
                             av.native<A1>(0)
                      ) );
     return cwal_exception_get(args->engine)
            ? CWAL_RC_EXCEPTION
            : 0;
  }
};
} // namespace Detail

namespace Detail {
template <typename FuncPtrT>
struct QuasiMethodProxy<2, true, FuncPtrT> : CallBackable {
  typedef CallbackSignature SignatureType;
  typedef int ResultType;
  typedef void Context;
  typedef SignatureType::Head Head;
  typedef SignatureType::Tail Tail;
  typedef tmp::Assertion< 2 == sigli::Arity<FuncPtrT>::Value > AssertArity;
  typedef typename sigli::At<0,FuncPtrT>::Type SelfType;
  //typedef tmp::Assertion< tmp::HasPtr< SelfType >::Value  > AssertIsPtr;
  static int callback(cwal_callback_args const * args, cwal_value **){
    Argv const av(args);
    typedef Dereffer<SelfType> Deref;
    SelfType that = toNative<SelfType>(args->engine, args->self);
    if(tmp::HasPtr< SelfType >::Value && !Deref()(that)){
      return tossMissingThis<SelfType>(args);
    }

    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    FuncPtrT::Function(
      that,
      av.native<A1>(0)
    );
    return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail
//! Specialization for 3 arg(s).
template <typename RV, typename A1, typename A2, typename A3>
struct Signature< RV (A1, A2, A3) >
{
    typedef RV ResultType;
    typedef void Context;
    typedef RV (*FunctionType)(A1, A2, A3);
    typedef A1 Head;
    typedef Signature<RV ( A2, A3)> Tail;
};

//! Specialization for 3 arg(s).
template <typename RV, typename A1, typename A2, typename A3>
struct Signature< RV (*)(A1, A2, A3) > : Signature<RV (A1, A2, A3)>
{};

//! Specialization for T non-const methods taking 3 arg(s).
template <typename T, typename RV, typename A1, typename A2, typename A3>
struct Signature< RV (T::*)(A1, A2, A3) > : Signature<RV (A1, A2, A3)>
{
    typedef T Context;
    typedef RV (T::*FunctionType)(A1, A2, A3);
};

//! Specialization for T const methods taking 3 arg(s).
template <typename T, typename RV, typename A1, typename A2, typename A3>
struct Signature< RV (T::*)(A1, A2, A3) const > : Signature<RV (A1, A2, A3)>
{
    typedef T const Context;
    typedef RV (T::*FunctionType)(A1, A2, A3) const;
};

//! Specialization for 3 args(s)
template <typename T, typename RV, typename A1, typename A2, typename A3>
struct MethodSignature< T, RV (A1, A2, A3) >
  : Signature< RV (T::*)(A1, A2, A3) >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3>
struct MethodSignature< T const, RV (A1, A2, A3) >
  : Signature< RV (T::*)(A1, A2, A3) const >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3 >
struct MethodSignature< T, RV (*)(A1, A2, A3) >
  : MethodSignature< T, RV (A1, A2, A3) >
{
};
template <typename T, typename RV, typename A1, typename A2, typename A3 >
struct MethodSignature< T const, RV (*)(A1, A2, A3) >
  : MethodSignature< T const, RV (A1, A2, A3) >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3 >
struct MethodSignature< T, RV (T::*)(A1, A2, A3) >
  : MethodSignature< T, RV (A1, A2, A3) >
{};

template <typename T, typename RV, typename A1, typename A2, typename A3 >
struct MethodSignature< T const, RV (T::*)(A1, A2, A3) >
  : MethodSignature< T const, RV (A1, A2, A3) >
{};

namespace Detail {
template <>
struct F2CbProxy<3, false> {
  template <typename FuncPtrT>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 3 == sigli::Arity<FuncPtrT>::Value > AssertArity;
    if(!AssertArity::Value){}
    Argv const av(args);
    typedef typename sigli::At<0,FuncPtrT>::Type A0;
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename FuncPtrT::ResultType RT;
    *rv = toCwal<RT>( args->engine,
                      (RT)FuncPtrT::Function(
                        av.native<A0>(0),
                        av.native<A1>(1),
                        av.native<A2>(2)
                      ) );
     return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail

namespace Detail {
template <>
struct F2CbProxy<3, true> {
  template <typename FuncPtrT>
  static int callback(cwal_callback_args const * args, cwal_value **){
    typedef tmp::Assertion< 3 == sigli::Arity<FuncPtrT>::Value > AssertArity;
    if(!AssertArity::Value){}
    Argv const av(args);
    typedef typename sigli::At<0,FuncPtrT>::Type A0;
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    FuncPtrT::Function(
      av.native<A0>(0),
      av.native<A1>(1),
      av.native<A2>(2)
    );
    return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail
namespace Detail {
template <>
struct M2CbProxy<3, false> {
  template <typename MemberPtr>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 3 == sigli::Arity<MemberPtr>::Value > AssertArity;
    if(!AssertArity::Value){}
    typedef typename MemberPtr::ResultType RT;
    typedef typename MemberPtr::Context T;
    T * that = toNative<T>(args->engine, args->self);
    if(!that){
      return tossMissingThis<T>(args);
    }else{
      const Argv av(args);
      typedef typename sigli::At<0,MemberPtr>::Type A0;
      typedef typename sigli::At<1,MemberPtr>::Type A1;
      typedef typename sigli::At<2,MemberPtr>::Type A2;
      *rv = toCwal<RT>( args->engine, (RT)(that->*MemberPtr::Function)(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2)
      ) );
      return 0; // FIXME: check s2_engine_err_has()
    }
  }
};

} // namespace Detail
namespace Detail {
template <>
struct M2CbProxy<3, true> {
  template <typename MemberPtr>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 3 == sigli::Arity<MemberPtr>::Value > AssertArity;
    if(!AssertArity::Value){}
    typedef typename MemberPtr::ResultType RT;
    typedef typename MemberPtr::Context T;
    T * that = toNative<T>(args->engine, args->self);
    if(!that){
      return tossMissingThis<T>(args);
    }else{
      const Argv av(args);
      typedef typename sigli::At<0,MemberPtr>::Type A0;
      typedef typename sigli::At<1,MemberPtr>::Type A1;
      typedef typename sigli::At<2,MemberPtr>::Type A2;
      (RT)(that->*MemberPtr::Function)(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2)
      );
      *rv = cwal_value_undefined();
      return 0; // FIXME: check s2_engine_err_has()
    }
  }
};
} // namespace Detail
namespace Detail {
template <typename MethodSigT>
struct CtorArgsProxy<3,MethodSigT,true> {
  typedef typename MethodSigT::ResultType ResultType;
  static ResultType construct(cwal_callback_args const * args){
    typedef typename MethodSigT::Context T;
    typedef tmp::Assertion< 3 == sigli::Arity<MethodSigT>::Value > AssertArity;
    if(!AssertArity::Value){}
    ResultType that = 0;
    void * mem = allocFromCwal<T>(args->engine);
      typedef typename sigli::At<0,MethodSigT>::Type A0;
      typedef typename sigli::At<1,MethodSigT>::Type A1;
      typedef typename sigli::At<2,MethodSigT>::Type A2;
    if(!mem) return NULL;
    try {
      const Argv av(args);
      that = new (mem) T(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2)        );
    }catch(std::exception const &ex){
      cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                          "%s constructor (arity=3) threw: %s",
                           TypeName<T>::Value, ex.what());
    }catch(...){
      cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                          "%s constructor (arity=3) threw an unknown exception",
                          TypeName<T>::Value);
    }
    if(!that) freeFromCwal<T>(args->engine, mem);
    return that;
  }
};

} // namespace Detail
namespace Detail {
template <typename MethodSigT>
struct CtorArgsProxy<3,MethodSigT,false> {
  typedef typename MethodSigT::ResultType ResultType;
  static ResultType construct(cwal_callback_args const * args){
    typedef typename MethodSigT::Context T;
    typedef tmp::Assertion< 3 == sigli::Arity<MethodSigT>::Value > AssertArity;
    if(!AssertArity::Value){}
    ResultType that = 0;
      typedef typename sigli::At<0,MethodSigT>::Type A0;
      typedef typename sigli::At<1,MethodSigT>::Type A1;
      typedef typename sigli::At<2,MethodSigT>::Type A2;
    try {
      const Argv av(args);
      that = new T(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2)        );
      }catch(std::exception const &ex){
        cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                            "%s constructor (arity=3) threw: %s",
                             TypeName<T>::Value, ex.what());
      }catch(...){
        cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                            "%s constructor (arity=3) threw an unknown exception",
                            TypeName<T>::Value);
      }
      return that;
    }
};

} // namespace Detail
namespace Detail {
template <typename FuncPtrT>
struct QuasiMethodProxy<3, false, FuncPtrT> : CallBackable {
  typedef CallbackSignature SignatureType;
  typedef int ResultType;
  typedef void Context;
  typedef SignatureType::Head Head;
  typedef SignatureType::Tail Tail;

  typedef tmp::Assertion< 3 == sigli::Arity<FuncPtrT>::Value > AssertArity;
  typedef typename sigli::At<0,FuncPtrT>::Type SelfType;
  //typedef tmp::Assertion< tmp::HasPtr< SelfType >::Value  > AssertIsPtr;
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef Dereffer<SelfType> Deref;
    Argv const av(args);
    SelfType that = toNative<SelfType>(args->engine, args->self);
    if(tmp::HasPtr< SelfType >::Value && !Deref()(that)){
      return tossMissingThis<SelfType>(args);
    }
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename FuncPtrT::ResultType RT;
    *rv = toCwal<RT>( args->engine,
                      (RT)FuncPtrT::Function(
                             that,
                             av.native<A1>(0),
                             av.native<A2>(1)
                      ) );
     return cwal_exception_get(args->engine)
            ? CWAL_RC_EXCEPTION
            : 0;
  }
};
} // namespace Detail

namespace Detail {
template <typename FuncPtrT>
struct QuasiMethodProxy<3, true, FuncPtrT> : CallBackable {
  typedef CallbackSignature SignatureType;
  typedef int ResultType;
  typedef void Context;
  typedef SignatureType::Head Head;
  typedef SignatureType::Tail Tail;
  typedef tmp::Assertion< 3 == sigli::Arity<FuncPtrT>::Value > AssertArity;
  typedef typename sigli::At<0,FuncPtrT>::Type SelfType;
  //typedef tmp::Assertion< tmp::HasPtr< SelfType >::Value  > AssertIsPtr;
  static int callback(cwal_callback_args const * args, cwal_value **){
    Argv const av(args);
    typedef Dereffer<SelfType> Deref;
    SelfType that = toNative<SelfType>(args->engine, args->self);
    if(tmp::HasPtr< SelfType >::Value && !Deref()(that)){
      return tossMissingThis<SelfType>(args);
    }

    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    FuncPtrT::Function(
      that,
      av.native<A1>(0),
      av.native<A2>(1)
    );
    return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail
//! Specialization for 4 arg(s).
template <typename RV, typename A1, typename A2, typename A3, typename A4>
struct Signature< RV (A1, A2, A3, A4) >
{
    typedef RV ResultType;
    typedef void Context;
    typedef RV (*FunctionType)(A1, A2, A3, A4);
    typedef A1 Head;
    typedef Signature<RV ( A2, A3, A4)> Tail;
};

//! Specialization for 4 arg(s).
template <typename RV, typename A1, typename A2, typename A3, typename A4>
struct Signature< RV (*)(A1, A2, A3, A4) > : Signature<RV (A1, A2, A3, A4)>
{};

//! Specialization for T non-const methods taking 4 arg(s).
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4>
struct Signature< RV (T::*)(A1, A2, A3, A4) > : Signature<RV (A1, A2, A3, A4)>
{
    typedef T Context;
    typedef RV (T::*FunctionType)(A1, A2, A3, A4);
};

//! Specialization for T const methods taking 4 arg(s).
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4>
struct Signature< RV (T::*)(A1, A2, A3, A4) const > : Signature<RV (A1, A2, A3, A4)>
{
    typedef T const Context;
    typedef RV (T::*FunctionType)(A1, A2, A3, A4) const;
};

//! Specialization for 4 args(s)
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4>
struct MethodSignature< T, RV (A1, A2, A3, A4) >
  : Signature< RV (T::*)(A1, A2, A3, A4) >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4>
struct MethodSignature< T const, RV (A1, A2, A3, A4) >
  : Signature< RV (T::*)(A1, A2, A3, A4) const >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4 >
struct MethodSignature< T, RV (*)(A1, A2, A3, A4) >
  : MethodSignature< T, RV (A1, A2, A3, A4) >
{
};
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4 >
struct MethodSignature< T const, RV (*)(A1, A2, A3, A4) >
  : MethodSignature< T const, RV (A1, A2, A3, A4) >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4 >
struct MethodSignature< T, RV (T::*)(A1, A2, A3, A4) >
  : MethodSignature< T, RV (A1, A2, A3, A4) >
{};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4 >
struct MethodSignature< T const, RV (T::*)(A1, A2, A3, A4) >
  : MethodSignature< T const, RV (A1, A2, A3, A4) >
{};

namespace Detail {
template <>
struct F2CbProxy<4, false> {
  template <typename FuncPtrT>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 4 == sigli::Arity<FuncPtrT>::Value > AssertArity;
    if(!AssertArity::Value){}
    Argv const av(args);
    typedef typename sigli::At<0,FuncPtrT>::Type A0;
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename FuncPtrT::ResultType RT;
    *rv = toCwal<RT>( args->engine,
                      (RT)FuncPtrT::Function(
                        av.native<A0>(0),
                        av.native<A1>(1),
                        av.native<A2>(2),
                        av.native<A3>(3)
                      ) );
     return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail

namespace Detail {
template <>
struct F2CbProxy<4, true> {
  template <typename FuncPtrT>
  static int callback(cwal_callback_args const * args, cwal_value **){
    typedef tmp::Assertion< 4 == sigli::Arity<FuncPtrT>::Value > AssertArity;
    if(!AssertArity::Value){}
    Argv const av(args);
    typedef typename sigli::At<0,FuncPtrT>::Type A0;
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    FuncPtrT::Function(
      av.native<A0>(0),
      av.native<A1>(1),
      av.native<A2>(2),
      av.native<A3>(3)
    );
    return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail
namespace Detail {
template <>
struct M2CbProxy<4, false> {
  template <typename MemberPtr>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 4 == sigli::Arity<MemberPtr>::Value > AssertArity;
    if(!AssertArity::Value){}
    typedef typename MemberPtr::ResultType RT;
    typedef typename MemberPtr::Context T;
    T * that = toNative<T>(args->engine, args->self);
    if(!that){
      return tossMissingThis<T>(args);
    }else{
      const Argv av(args);
      typedef typename sigli::At<0,MemberPtr>::Type A0;
      typedef typename sigli::At<1,MemberPtr>::Type A1;
      typedef typename sigli::At<2,MemberPtr>::Type A2;
      typedef typename sigli::At<3,MemberPtr>::Type A3;
      *rv = toCwal<RT>( args->engine, (RT)(that->*MemberPtr::Function)(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3)
      ) );
      return 0; // FIXME: check s2_engine_err_has()
    }
  }
};

} // namespace Detail
namespace Detail {
template <>
struct M2CbProxy<4, true> {
  template <typename MemberPtr>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 4 == sigli::Arity<MemberPtr>::Value > AssertArity;
    if(!AssertArity::Value){}
    typedef typename MemberPtr::ResultType RT;
    typedef typename MemberPtr::Context T;
    T * that = toNative<T>(args->engine, args->self);
    if(!that){
      return tossMissingThis<T>(args);
    }else{
      const Argv av(args);
      typedef typename sigli::At<0,MemberPtr>::Type A0;
      typedef typename sigli::At<1,MemberPtr>::Type A1;
      typedef typename sigli::At<2,MemberPtr>::Type A2;
      typedef typename sigli::At<3,MemberPtr>::Type A3;
      (RT)(that->*MemberPtr::Function)(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3)
      );
      *rv = cwal_value_undefined();
      return 0; // FIXME: check s2_engine_err_has()
    }
  }
};
} // namespace Detail
namespace Detail {
template <typename MethodSigT>
struct CtorArgsProxy<4,MethodSigT,true> {
  typedef typename MethodSigT::ResultType ResultType;
  static ResultType construct(cwal_callback_args const * args){
    typedef typename MethodSigT::Context T;
    typedef tmp::Assertion< 4 == sigli::Arity<MethodSigT>::Value > AssertArity;
    if(!AssertArity::Value){}
    ResultType that = 0;
    void * mem = allocFromCwal<T>(args->engine);
      typedef typename sigli::At<0,MethodSigT>::Type A0;
      typedef typename sigli::At<1,MethodSigT>::Type A1;
      typedef typename sigli::At<2,MethodSigT>::Type A2;
      typedef typename sigli::At<3,MethodSigT>::Type A3;
    if(!mem) return NULL;
    try {
      const Argv av(args);
      that = new (mem) T(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3)        );
    }catch(std::exception const &ex){
      cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                          "%s constructor (arity=4) threw: %s",
                           TypeName<T>::Value, ex.what());
    }catch(...){
      cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                          "%s constructor (arity=4) threw an unknown exception",
                          TypeName<T>::Value);
    }
    if(!that) freeFromCwal<T>(args->engine, mem);
    return that;
  }
};

} // namespace Detail
namespace Detail {
template <typename MethodSigT>
struct CtorArgsProxy<4,MethodSigT,false> {
  typedef typename MethodSigT::ResultType ResultType;
  static ResultType construct(cwal_callback_args const * args){
    typedef typename MethodSigT::Context T;
    typedef tmp::Assertion< 4 == sigli::Arity<MethodSigT>::Value > AssertArity;
    if(!AssertArity::Value){}
    ResultType that = 0;
      typedef typename sigli::At<0,MethodSigT>::Type A0;
      typedef typename sigli::At<1,MethodSigT>::Type A1;
      typedef typename sigli::At<2,MethodSigT>::Type A2;
      typedef typename sigli::At<3,MethodSigT>::Type A3;
    try {
      const Argv av(args);
      that = new T(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3)        );
      }catch(std::exception const &ex){
        cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                            "%s constructor (arity=4) threw: %s",
                             TypeName<T>::Value, ex.what());
      }catch(...){
        cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                            "%s constructor (arity=4) threw an unknown exception",
                            TypeName<T>::Value);
      }
      return that;
    }
};

} // namespace Detail
namespace Detail {
template <typename FuncPtrT>
struct QuasiMethodProxy<4, false, FuncPtrT> : CallBackable {
  typedef CallbackSignature SignatureType;
  typedef int ResultType;
  typedef void Context;
  typedef SignatureType::Head Head;
  typedef SignatureType::Tail Tail;

  typedef tmp::Assertion< 4 == sigli::Arity<FuncPtrT>::Value > AssertArity;
  typedef typename sigli::At<0,FuncPtrT>::Type SelfType;
  //typedef tmp::Assertion< tmp::HasPtr< SelfType >::Value  > AssertIsPtr;
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef Dereffer<SelfType> Deref;
    Argv const av(args);
    SelfType that = toNative<SelfType>(args->engine, args->self);
    if(tmp::HasPtr< SelfType >::Value && !Deref()(that)){
      return tossMissingThis<SelfType>(args);
    }
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename FuncPtrT::ResultType RT;
    *rv = toCwal<RT>( args->engine,
                      (RT)FuncPtrT::Function(
                             that,
                             av.native<A1>(0),
                             av.native<A2>(1),
                             av.native<A3>(2)
                      ) );
     return cwal_exception_get(args->engine)
            ? CWAL_RC_EXCEPTION
            : 0;
  }
};
} // namespace Detail

namespace Detail {
template <typename FuncPtrT>
struct QuasiMethodProxy<4, true, FuncPtrT> : CallBackable {
  typedef CallbackSignature SignatureType;
  typedef int ResultType;
  typedef void Context;
  typedef SignatureType::Head Head;
  typedef SignatureType::Tail Tail;
  typedef tmp::Assertion< 4 == sigli::Arity<FuncPtrT>::Value > AssertArity;
  typedef typename sigli::At<0,FuncPtrT>::Type SelfType;
  //typedef tmp::Assertion< tmp::HasPtr< SelfType >::Value  > AssertIsPtr;
  static int callback(cwal_callback_args const * args, cwal_value **){
    Argv const av(args);
    typedef Dereffer<SelfType> Deref;
    SelfType that = toNative<SelfType>(args->engine, args->self);
    if(tmp::HasPtr< SelfType >::Value && !Deref()(that)){
      return tossMissingThis<SelfType>(args);
    }

    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    FuncPtrT::Function(
      that,
      av.native<A1>(0),
      av.native<A2>(1),
      av.native<A3>(2)
    );
    return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail
//! Specialization for 5 arg(s).
template <typename RV, typename A1, typename A2, typename A3, typename A4, typename A5>
struct Signature< RV (A1, A2, A3, A4, A5) >
{
    typedef RV ResultType;
    typedef void Context;
    typedef RV (*FunctionType)(A1, A2, A3, A4, A5);
    typedef A1 Head;
    typedef Signature<RV ( A2, A3, A4, A5)> Tail;
};

//! Specialization for 5 arg(s).
template <typename RV, typename A1, typename A2, typename A3, typename A4, typename A5>
struct Signature< RV (*)(A1, A2, A3, A4, A5) > : Signature<RV (A1, A2, A3, A4, A5)>
{};

//! Specialization for T non-const methods taking 5 arg(s).
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5>
struct Signature< RV (T::*)(A1, A2, A3, A4, A5) > : Signature<RV (A1, A2, A3, A4, A5)>
{
    typedef T Context;
    typedef RV (T::*FunctionType)(A1, A2, A3, A4, A5);
};

//! Specialization for T const methods taking 5 arg(s).
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5>
struct Signature< RV (T::*)(A1, A2, A3, A4, A5) const > : Signature<RV (A1, A2, A3, A4, A5)>
{
    typedef T const Context;
    typedef RV (T::*FunctionType)(A1, A2, A3, A4, A5) const;
};

//! Specialization for 5 args(s)
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5>
struct MethodSignature< T, RV (A1, A2, A3, A4, A5) >
  : Signature< RV (T::*)(A1, A2, A3, A4, A5) >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5>
struct MethodSignature< T const, RV (A1, A2, A3, A4, A5) >
  : Signature< RV (T::*)(A1, A2, A3, A4, A5) const >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5 >
struct MethodSignature< T, RV (*)(A1, A2, A3, A4, A5) >
  : MethodSignature< T, RV (A1, A2, A3, A4, A5) >
{
};
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5 >
struct MethodSignature< T const, RV (*)(A1, A2, A3, A4, A5) >
  : MethodSignature< T const, RV (A1, A2, A3, A4, A5) >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5 >
struct MethodSignature< T, RV (T::*)(A1, A2, A3, A4, A5) >
  : MethodSignature< T, RV (A1, A2, A3, A4, A5) >
{};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5 >
struct MethodSignature< T const, RV (T::*)(A1, A2, A3, A4, A5) >
  : MethodSignature< T const, RV (A1, A2, A3, A4, A5) >
{};

namespace Detail {
template <>
struct F2CbProxy<5, false> {
  template <typename FuncPtrT>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 5 == sigli::Arity<FuncPtrT>::Value > AssertArity;
    if(!AssertArity::Value){}
    Argv const av(args);
    typedef typename sigli::At<0,FuncPtrT>::Type A0;
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    typedef typename FuncPtrT::ResultType RT;
    *rv = toCwal<RT>( args->engine,
                      (RT)FuncPtrT::Function(
                        av.native<A0>(0),
                        av.native<A1>(1),
                        av.native<A2>(2),
                        av.native<A3>(3),
                        av.native<A4>(4)
                      ) );
     return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail

namespace Detail {
template <>
struct F2CbProxy<5, true> {
  template <typename FuncPtrT>
  static int callback(cwal_callback_args const * args, cwal_value **){
    typedef tmp::Assertion< 5 == sigli::Arity<FuncPtrT>::Value > AssertArity;
    if(!AssertArity::Value){}
    Argv const av(args);
    typedef typename sigli::At<0,FuncPtrT>::Type A0;
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    FuncPtrT::Function(
      av.native<A0>(0),
      av.native<A1>(1),
      av.native<A2>(2),
      av.native<A3>(3),
      av.native<A4>(4)
    );
    return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail
namespace Detail {
template <>
struct M2CbProxy<5, false> {
  template <typename MemberPtr>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 5 == sigli::Arity<MemberPtr>::Value > AssertArity;
    if(!AssertArity::Value){}
    typedef typename MemberPtr::ResultType RT;
    typedef typename MemberPtr::Context T;
    T * that = toNative<T>(args->engine, args->self);
    if(!that){
      return tossMissingThis<T>(args);
    }else{
      const Argv av(args);
      typedef typename sigli::At<0,MemberPtr>::Type A0;
      typedef typename sigli::At<1,MemberPtr>::Type A1;
      typedef typename sigli::At<2,MemberPtr>::Type A2;
      typedef typename sigli::At<3,MemberPtr>::Type A3;
      typedef typename sigli::At<4,MemberPtr>::Type A4;
      *rv = toCwal<RT>( args->engine, (RT)(that->*MemberPtr::Function)(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4)
      ) );
      return 0; // FIXME: check s2_engine_err_has()
    }
  }
};

} // namespace Detail
namespace Detail {
template <>
struct M2CbProxy<5, true> {
  template <typename MemberPtr>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 5 == sigli::Arity<MemberPtr>::Value > AssertArity;
    if(!AssertArity::Value){}
    typedef typename MemberPtr::ResultType RT;
    typedef typename MemberPtr::Context T;
    T * that = toNative<T>(args->engine, args->self);
    if(!that){
      return tossMissingThis<T>(args);
    }else{
      const Argv av(args);
      typedef typename sigli::At<0,MemberPtr>::Type A0;
      typedef typename sigli::At<1,MemberPtr>::Type A1;
      typedef typename sigli::At<2,MemberPtr>::Type A2;
      typedef typename sigli::At<3,MemberPtr>::Type A3;
      typedef typename sigli::At<4,MemberPtr>::Type A4;
      (RT)(that->*MemberPtr::Function)(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4)
      );
      *rv = cwal_value_undefined();
      return 0; // FIXME: check s2_engine_err_has()
    }
  }
};
} // namespace Detail
namespace Detail {
template <typename MethodSigT>
struct CtorArgsProxy<5,MethodSigT,true> {
  typedef typename MethodSigT::ResultType ResultType;
  static ResultType construct(cwal_callback_args const * args){
    typedef typename MethodSigT::Context T;
    typedef tmp::Assertion< 5 == sigli::Arity<MethodSigT>::Value > AssertArity;
    if(!AssertArity::Value){}
    ResultType that = 0;
    void * mem = allocFromCwal<T>(args->engine);
      typedef typename sigli::At<0,MethodSigT>::Type A0;
      typedef typename sigli::At<1,MethodSigT>::Type A1;
      typedef typename sigli::At<2,MethodSigT>::Type A2;
      typedef typename sigli::At<3,MethodSigT>::Type A3;
      typedef typename sigli::At<4,MethodSigT>::Type A4;
    if(!mem) return NULL;
    try {
      const Argv av(args);
      that = new (mem) T(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4)        );
    }catch(std::exception const &ex){
      cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                          "%s constructor (arity=5) threw: %s",
                           TypeName<T>::Value, ex.what());
    }catch(...){
      cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                          "%s constructor (arity=5) threw an unknown exception",
                          TypeName<T>::Value);
    }
    if(!that) freeFromCwal<T>(args->engine, mem);
    return that;
  }
};

} // namespace Detail
namespace Detail {
template <typename MethodSigT>
struct CtorArgsProxy<5,MethodSigT,false> {
  typedef typename MethodSigT::ResultType ResultType;
  static ResultType construct(cwal_callback_args const * args){
    typedef typename MethodSigT::Context T;
    typedef tmp::Assertion< 5 == sigli::Arity<MethodSigT>::Value > AssertArity;
    if(!AssertArity::Value){}
    ResultType that = 0;
      typedef typename sigli::At<0,MethodSigT>::Type A0;
      typedef typename sigli::At<1,MethodSigT>::Type A1;
      typedef typename sigli::At<2,MethodSigT>::Type A2;
      typedef typename sigli::At<3,MethodSigT>::Type A3;
      typedef typename sigli::At<4,MethodSigT>::Type A4;
    try {
      const Argv av(args);
      that = new T(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4)        );
      }catch(std::exception const &ex){
        cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                            "%s constructor (arity=5) threw: %s",
                             TypeName<T>::Value, ex.what());
      }catch(...){
        cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                            "%s constructor (arity=5) threw an unknown exception",
                            TypeName<T>::Value);
      }
      return that;
    }
};

} // namespace Detail
namespace Detail {
template <typename FuncPtrT>
struct QuasiMethodProxy<5, false, FuncPtrT> : CallBackable {
  typedef CallbackSignature SignatureType;
  typedef int ResultType;
  typedef void Context;
  typedef SignatureType::Head Head;
  typedef SignatureType::Tail Tail;

  typedef tmp::Assertion< 5 == sigli::Arity<FuncPtrT>::Value > AssertArity;
  typedef typename sigli::At<0,FuncPtrT>::Type SelfType;
  //typedef tmp::Assertion< tmp::HasPtr< SelfType >::Value  > AssertIsPtr;
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef Dereffer<SelfType> Deref;
    Argv const av(args);
    SelfType that = toNative<SelfType>(args->engine, args->self);
    if(tmp::HasPtr< SelfType >::Value && !Deref()(that)){
      return tossMissingThis<SelfType>(args);
    }
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    typedef typename FuncPtrT::ResultType RT;
    *rv = toCwal<RT>( args->engine,
                      (RT)FuncPtrT::Function(
                             that,
                             av.native<A1>(0),
                             av.native<A2>(1),
                             av.native<A3>(2),
                             av.native<A4>(3)
                      ) );
     return cwal_exception_get(args->engine)
            ? CWAL_RC_EXCEPTION
            : 0;
  }
};
} // namespace Detail

namespace Detail {
template <typename FuncPtrT>
struct QuasiMethodProxy<5, true, FuncPtrT> : CallBackable {
  typedef CallbackSignature SignatureType;
  typedef int ResultType;
  typedef void Context;
  typedef SignatureType::Head Head;
  typedef SignatureType::Tail Tail;
  typedef tmp::Assertion< 5 == sigli::Arity<FuncPtrT>::Value > AssertArity;
  typedef typename sigli::At<0,FuncPtrT>::Type SelfType;
  //typedef tmp::Assertion< tmp::HasPtr< SelfType >::Value  > AssertIsPtr;
  static int callback(cwal_callback_args const * args, cwal_value **){
    Argv const av(args);
    typedef Dereffer<SelfType> Deref;
    SelfType that = toNative<SelfType>(args->engine, args->self);
    if(tmp::HasPtr< SelfType >::Value && !Deref()(that)){
      return tossMissingThis<SelfType>(args);
    }

    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    FuncPtrT::Function(
      that,
      av.native<A1>(0),
      av.native<A2>(1),
      av.native<A3>(2),
      av.native<A4>(3)
    );
    return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail
//! Specialization for 6 arg(s).
template <typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6>
struct Signature< RV (A1, A2, A3, A4, A5, A6) >
{
    typedef RV ResultType;
    typedef void Context;
    typedef RV (*FunctionType)(A1, A2, A3, A4, A5, A6);
    typedef A1 Head;
    typedef Signature<RV ( A2, A3, A4, A5, A6)> Tail;
};

//! Specialization for 6 arg(s).
template <typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6>
struct Signature< RV (*)(A1, A2, A3, A4, A5, A6) > : Signature<RV (A1, A2, A3, A4, A5, A6)>
{};

//! Specialization for T non-const methods taking 6 arg(s).
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6>
struct Signature< RV (T::*)(A1, A2, A3, A4, A5, A6) > : Signature<RV (A1, A2, A3, A4, A5, A6)>
{
    typedef T Context;
    typedef RV (T::*FunctionType)(A1, A2, A3, A4, A5, A6);
};

//! Specialization for T const methods taking 6 arg(s).
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6>
struct Signature< RV (T::*)(A1, A2, A3, A4, A5, A6) const > : Signature<RV (A1, A2, A3, A4, A5, A6)>
{
    typedef T const Context;
    typedef RV (T::*FunctionType)(A1, A2, A3, A4, A5, A6) const;
};

//! Specialization for 6 args(s)
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6>
struct MethodSignature< T, RV (A1, A2, A3, A4, A5, A6) >
  : Signature< RV (T::*)(A1, A2, A3, A4, A5, A6) >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6>
struct MethodSignature< T const, RV (A1, A2, A3, A4, A5, A6) >
  : Signature< RV (T::*)(A1, A2, A3, A4, A5, A6) const >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6 >
struct MethodSignature< T, RV (*)(A1, A2, A3, A4, A5, A6) >
  : MethodSignature< T, RV (A1, A2, A3, A4, A5, A6) >
{
};
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6 >
struct MethodSignature< T const, RV (*)(A1, A2, A3, A4, A5, A6) >
  : MethodSignature< T const, RV (A1, A2, A3, A4, A5, A6) >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6 >
struct MethodSignature< T, RV (T::*)(A1, A2, A3, A4, A5, A6) >
  : MethodSignature< T, RV (A1, A2, A3, A4, A5, A6) >
{};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6 >
struct MethodSignature< T const, RV (T::*)(A1, A2, A3, A4, A5, A6) >
  : MethodSignature< T const, RV (A1, A2, A3, A4, A5, A6) >
{};

namespace Detail {
template <>
struct F2CbProxy<6, false> {
  template <typename FuncPtrT>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 6 == sigli::Arity<FuncPtrT>::Value > AssertArity;
    if(!AssertArity::Value){}
    Argv const av(args);
    typedef typename sigli::At<0,FuncPtrT>::Type A0;
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    typedef typename sigli::At<5,FuncPtrT>::Type A5;
    typedef typename FuncPtrT::ResultType RT;
    *rv = toCwal<RT>( args->engine,
                      (RT)FuncPtrT::Function(
                        av.native<A0>(0),
                        av.native<A1>(1),
                        av.native<A2>(2),
                        av.native<A3>(3),
                        av.native<A4>(4),
                        av.native<A5>(5)
                      ) );
     return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail

namespace Detail {
template <>
struct F2CbProxy<6, true> {
  template <typename FuncPtrT>
  static int callback(cwal_callback_args const * args, cwal_value **){
    typedef tmp::Assertion< 6 == sigli::Arity<FuncPtrT>::Value > AssertArity;
    if(!AssertArity::Value){}
    Argv const av(args);
    typedef typename sigli::At<0,FuncPtrT>::Type A0;
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    typedef typename sigli::At<5,FuncPtrT>::Type A5;
    FuncPtrT::Function(
      av.native<A0>(0),
      av.native<A1>(1),
      av.native<A2>(2),
      av.native<A3>(3),
      av.native<A4>(4),
      av.native<A5>(5)
    );
    return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail
namespace Detail {
template <>
struct M2CbProxy<6, false> {
  template <typename MemberPtr>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 6 == sigli::Arity<MemberPtr>::Value > AssertArity;
    if(!AssertArity::Value){}
    typedef typename MemberPtr::ResultType RT;
    typedef typename MemberPtr::Context T;
    T * that = toNative<T>(args->engine, args->self);
    if(!that){
      return tossMissingThis<T>(args);
    }else{
      const Argv av(args);
      typedef typename sigli::At<0,MemberPtr>::Type A0;
      typedef typename sigli::At<1,MemberPtr>::Type A1;
      typedef typename sigli::At<2,MemberPtr>::Type A2;
      typedef typename sigli::At<3,MemberPtr>::Type A3;
      typedef typename sigli::At<4,MemberPtr>::Type A4;
      typedef typename sigli::At<5,MemberPtr>::Type A5;
      *rv = toCwal<RT>( args->engine, (RT)(that->*MemberPtr::Function)(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4),
        av.native<A5>(5)
      ) );
      return 0; // FIXME: check s2_engine_err_has()
    }
  }
};

} // namespace Detail
namespace Detail {
template <>
struct M2CbProxy<6, true> {
  template <typename MemberPtr>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 6 == sigli::Arity<MemberPtr>::Value > AssertArity;
    if(!AssertArity::Value){}
    typedef typename MemberPtr::ResultType RT;
    typedef typename MemberPtr::Context T;
    T * that = toNative<T>(args->engine, args->self);
    if(!that){
      return tossMissingThis<T>(args);
    }else{
      const Argv av(args);
      typedef typename sigli::At<0,MemberPtr>::Type A0;
      typedef typename sigli::At<1,MemberPtr>::Type A1;
      typedef typename sigli::At<2,MemberPtr>::Type A2;
      typedef typename sigli::At<3,MemberPtr>::Type A3;
      typedef typename sigli::At<4,MemberPtr>::Type A4;
      typedef typename sigli::At<5,MemberPtr>::Type A5;
      (RT)(that->*MemberPtr::Function)(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4),
        av.native<A5>(5)
      );
      *rv = cwal_value_undefined();
      return 0; // FIXME: check s2_engine_err_has()
    }
  }
};
} // namespace Detail
namespace Detail {
template <typename MethodSigT>
struct CtorArgsProxy<6,MethodSigT,true> {
  typedef typename MethodSigT::ResultType ResultType;
  static ResultType construct(cwal_callback_args const * args){
    typedef typename MethodSigT::Context T;
    typedef tmp::Assertion< 6 == sigli::Arity<MethodSigT>::Value > AssertArity;
    if(!AssertArity::Value){}
    ResultType that = 0;
    void * mem = allocFromCwal<T>(args->engine);
      typedef typename sigli::At<0,MethodSigT>::Type A0;
      typedef typename sigli::At<1,MethodSigT>::Type A1;
      typedef typename sigli::At<2,MethodSigT>::Type A2;
      typedef typename sigli::At<3,MethodSigT>::Type A3;
      typedef typename sigli::At<4,MethodSigT>::Type A4;
      typedef typename sigli::At<5,MethodSigT>::Type A5;
    if(!mem) return NULL;
    try {
      const Argv av(args);
      that = new (mem) T(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4),
        av.native<A5>(5)        );
    }catch(std::exception const &ex){
      cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                          "%s constructor (arity=6) threw: %s",
                           TypeName<T>::Value, ex.what());
    }catch(...){
      cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                          "%s constructor (arity=6) threw an unknown exception",
                          TypeName<T>::Value);
    }
    if(!that) freeFromCwal<T>(args->engine, mem);
    return that;
  }
};

} // namespace Detail
namespace Detail {
template <typename MethodSigT>
struct CtorArgsProxy<6,MethodSigT,false> {
  typedef typename MethodSigT::ResultType ResultType;
  static ResultType construct(cwal_callback_args const * args){
    typedef typename MethodSigT::Context T;
    typedef tmp::Assertion< 6 == sigli::Arity<MethodSigT>::Value > AssertArity;
    if(!AssertArity::Value){}
    ResultType that = 0;
      typedef typename sigli::At<0,MethodSigT>::Type A0;
      typedef typename sigli::At<1,MethodSigT>::Type A1;
      typedef typename sigli::At<2,MethodSigT>::Type A2;
      typedef typename sigli::At<3,MethodSigT>::Type A3;
      typedef typename sigli::At<4,MethodSigT>::Type A4;
      typedef typename sigli::At<5,MethodSigT>::Type A5;
    try {
      const Argv av(args);
      that = new T(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4),
        av.native<A5>(5)        );
      }catch(std::exception const &ex){
        cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                            "%s constructor (arity=6) threw: %s",
                             TypeName<T>::Value, ex.what());
      }catch(...){
        cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                            "%s constructor (arity=6) threw an unknown exception",
                            TypeName<T>::Value);
      }
      return that;
    }
};

} // namespace Detail
namespace Detail {
template <typename FuncPtrT>
struct QuasiMethodProxy<6, false, FuncPtrT> : CallBackable {
  typedef CallbackSignature SignatureType;
  typedef int ResultType;
  typedef void Context;
  typedef SignatureType::Head Head;
  typedef SignatureType::Tail Tail;

  typedef tmp::Assertion< 6 == sigli::Arity<FuncPtrT>::Value > AssertArity;
  typedef typename sigli::At<0,FuncPtrT>::Type SelfType;
  //typedef tmp::Assertion< tmp::HasPtr< SelfType >::Value  > AssertIsPtr;
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef Dereffer<SelfType> Deref;
    Argv const av(args);
    SelfType that = toNative<SelfType>(args->engine, args->self);
    if(tmp::HasPtr< SelfType >::Value && !Deref()(that)){
      return tossMissingThis<SelfType>(args);
    }
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    typedef typename sigli::At<5,FuncPtrT>::Type A5;
    typedef typename FuncPtrT::ResultType RT;
    *rv = toCwal<RT>( args->engine,
                      (RT)FuncPtrT::Function(
                             that,
                             av.native<A1>(0),
                             av.native<A2>(1),
                             av.native<A3>(2),
                             av.native<A4>(3),
                             av.native<A5>(4)
                      ) );
     return cwal_exception_get(args->engine)
            ? CWAL_RC_EXCEPTION
            : 0;
  }
};
} // namespace Detail

namespace Detail {
template <typename FuncPtrT>
struct QuasiMethodProxy<6, true, FuncPtrT> : CallBackable {
  typedef CallbackSignature SignatureType;
  typedef int ResultType;
  typedef void Context;
  typedef SignatureType::Head Head;
  typedef SignatureType::Tail Tail;
  typedef tmp::Assertion< 6 == sigli::Arity<FuncPtrT>::Value > AssertArity;
  typedef typename sigli::At<0,FuncPtrT>::Type SelfType;
  //typedef tmp::Assertion< tmp::HasPtr< SelfType >::Value  > AssertIsPtr;
  static int callback(cwal_callback_args const * args, cwal_value **){
    Argv const av(args);
    typedef Dereffer<SelfType> Deref;
    SelfType that = toNative<SelfType>(args->engine, args->self);
    if(tmp::HasPtr< SelfType >::Value && !Deref()(that)){
      return tossMissingThis<SelfType>(args);
    }

    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    typedef typename sigli::At<5,FuncPtrT>::Type A5;
    FuncPtrT::Function(
      that,
      av.native<A1>(0),
      av.native<A2>(1),
      av.native<A3>(2),
      av.native<A4>(3),
      av.native<A5>(4)
    );
    return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail
//! Specialization for 7 arg(s).
template <typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7>
struct Signature< RV (A1, A2, A3, A4, A5, A6, A7) >
{
    typedef RV ResultType;
    typedef void Context;
    typedef RV (*FunctionType)(A1, A2, A3, A4, A5, A6, A7);
    typedef A1 Head;
    typedef Signature<RV ( A2, A3, A4, A5, A6, A7)> Tail;
};

//! Specialization for 7 arg(s).
template <typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7>
struct Signature< RV (*)(A1, A2, A3, A4, A5, A6, A7) > : Signature<RV (A1, A2, A3, A4, A5, A6, A7)>
{};

//! Specialization for T non-const methods taking 7 arg(s).
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7>
struct Signature< RV (T::*)(A1, A2, A3, A4, A5, A6, A7) > : Signature<RV (A1, A2, A3, A4, A5, A6, A7)>
{
    typedef T Context;
    typedef RV (T::*FunctionType)(A1, A2, A3, A4, A5, A6, A7);
};

//! Specialization for T const methods taking 7 arg(s).
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7>
struct Signature< RV (T::*)(A1, A2, A3, A4, A5, A6, A7) const > : Signature<RV (A1, A2, A3, A4, A5, A6, A7)>
{
    typedef T const Context;
    typedef RV (T::*FunctionType)(A1, A2, A3, A4, A5, A6, A7) const;
};

//! Specialization for 7 args(s)
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7>
struct MethodSignature< T, RV (A1, A2, A3, A4, A5, A6, A7) >
  : Signature< RV (T::*)(A1, A2, A3, A4, A5, A6, A7) >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7>
struct MethodSignature< T const, RV (A1, A2, A3, A4, A5, A6, A7) >
  : Signature< RV (T::*)(A1, A2, A3, A4, A5, A6, A7) const >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7 >
struct MethodSignature< T, RV (*)(A1, A2, A3, A4, A5, A6, A7) >
  : MethodSignature< T, RV (A1, A2, A3, A4, A5, A6, A7) >
{
};
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7 >
struct MethodSignature< T const, RV (*)(A1, A2, A3, A4, A5, A6, A7) >
  : MethodSignature< T const, RV (A1, A2, A3, A4, A5, A6, A7) >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7 >
struct MethodSignature< T, RV (T::*)(A1, A2, A3, A4, A5, A6, A7) >
  : MethodSignature< T, RV (A1, A2, A3, A4, A5, A6, A7) >
{};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7 >
struct MethodSignature< T const, RV (T::*)(A1, A2, A3, A4, A5, A6, A7) >
  : MethodSignature< T const, RV (A1, A2, A3, A4, A5, A6, A7) >
{};

namespace Detail {
template <>
struct F2CbProxy<7, false> {
  template <typename FuncPtrT>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 7 == sigli::Arity<FuncPtrT>::Value > AssertArity;
    if(!AssertArity::Value){}
    Argv const av(args);
    typedef typename sigli::At<0,FuncPtrT>::Type A0;
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    typedef typename sigli::At<5,FuncPtrT>::Type A5;
    typedef typename sigli::At<6,FuncPtrT>::Type A6;
    typedef typename FuncPtrT::ResultType RT;
    *rv = toCwal<RT>( args->engine,
                      (RT)FuncPtrT::Function(
                        av.native<A0>(0),
                        av.native<A1>(1),
                        av.native<A2>(2),
                        av.native<A3>(3),
                        av.native<A4>(4),
                        av.native<A5>(5),
                        av.native<A6>(6)
                      ) );
     return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail

namespace Detail {
template <>
struct F2CbProxy<7, true> {
  template <typename FuncPtrT>
  static int callback(cwal_callback_args const * args, cwal_value **){
    typedef tmp::Assertion< 7 == sigli::Arity<FuncPtrT>::Value > AssertArity;
    if(!AssertArity::Value){}
    Argv const av(args);
    typedef typename sigli::At<0,FuncPtrT>::Type A0;
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    typedef typename sigli::At<5,FuncPtrT>::Type A5;
    typedef typename sigli::At<6,FuncPtrT>::Type A6;
    FuncPtrT::Function(
      av.native<A0>(0),
      av.native<A1>(1),
      av.native<A2>(2),
      av.native<A3>(3),
      av.native<A4>(4),
      av.native<A5>(5),
      av.native<A6>(6)
    );
    return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail
namespace Detail {
template <>
struct M2CbProxy<7, false> {
  template <typename MemberPtr>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 7 == sigli::Arity<MemberPtr>::Value > AssertArity;
    if(!AssertArity::Value){}
    typedef typename MemberPtr::ResultType RT;
    typedef typename MemberPtr::Context T;
    T * that = toNative<T>(args->engine, args->self);
    if(!that){
      return tossMissingThis<T>(args);
    }else{
      const Argv av(args);
      typedef typename sigli::At<0,MemberPtr>::Type A0;
      typedef typename sigli::At<1,MemberPtr>::Type A1;
      typedef typename sigli::At<2,MemberPtr>::Type A2;
      typedef typename sigli::At<3,MemberPtr>::Type A3;
      typedef typename sigli::At<4,MemberPtr>::Type A4;
      typedef typename sigli::At<5,MemberPtr>::Type A5;
      typedef typename sigli::At<6,MemberPtr>::Type A6;
      *rv = toCwal<RT>( args->engine, (RT)(that->*MemberPtr::Function)(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4),
        av.native<A5>(5),
        av.native<A6>(6)
      ) );
      return 0; // FIXME: check s2_engine_err_has()
    }
  }
};

} // namespace Detail
namespace Detail {
template <>
struct M2CbProxy<7, true> {
  template <typename MemberPtr>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 7 == sigli::Arity<MemberPtr>::Value > AssertArity;
    if(!AssertArity::Value){}
    typedef typename MemberPtr::ResultType RT;
    typedef typename MemberPtr::Context T;
    T * that = toNative<T>(args->engine, args->self);
    if(!that){
      return tossMissingThis<T>(args);
    }else{
      const Argv av(args);
      typedef typename sigli::At<0,MemberPtr>::Type A0;
      typedef typename sigli::At<1,MemberPtr>::Type A1;
      typedef typename sigli::At<2,MemberPtr>::Type A2;
      typedef typename sigli::At<3,MemberPtr>::Type A3;
      typedef typename sigli::At<4,MemberPtr>::Type A4;
      typedef typename sigli::At<5,MemberPtr>::Type A5;
      typedef typename sigli::At<6,MemberPtr>::Type A6;
      (RT)(that->*MemberPtr::Function)(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4),
        av.native<A5>(5),
        av.native<A6>(6)
      );
      *rv = cwal_value_undefined();
      return 0; // FIXME: check s2_engine_err_has()
    }
  }
};
} // namespace Detail
namespace Detail {
template <typename MethodSigT>
struct CtorArgsProxy<7,MethodSigT,true> {
  typedef typename MethodSigT::ResultType ResultType;
  static ResultType construct(cwal_callback_args const * args){
    typedef typename MethodSigT::Context T;
    typedef tmp::Assertion< 7 == sigli::Arity<MethodSigT>::Value > AssertArity;
    if(!AssertArity::Value){}
    ResultType that = 0;
    void * mem = allocFromCwal<T>(args->engine);
      typedef typename sigli::At<0,MethodSigT>::Type A0;
      typedef typename sigli::At<1,MethodSigT>::Type A1;
      typedef typename sigli::At<2,MethodSigT>::Type A2;
      typedef typename sigli::At<3,MethodSigT>::Type A3;
      typedef typename sigli::At<4,MethodSigT>::Type A4;
      typedef typename sigli::At<5,MethodSigT>::Type A5;
      typedef typename sigli::At<6,MethodSigT>::Type A6;
    if(!mem) return NULL;
    try {
      const Argv av(args);
      that = new (mem) T(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4),
        av.native<A5>(5),
        av.native<A6>(6)        );
    }catch(std::exception const &ex){
      cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                          "%s constructor (arity=7) threw: %s",
                           TypeName<T>::Value, ex.what());
    }catch(...){
      cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                          "%s constructor (arity=7) threw an unknown exception",
                          TypeName<T>::Value);
    }
    if(!that) freeFromCwal<T>(args->engine, mem);
    return that;
  }
};

} // namespace Detail
namespace Detail {
template <typename MethodSigT>
struct CtorArgsProxy<7,MethodSigT,false> {
  typedef typename MethodSigT::ResultType ResultType;
  static ResultType construct(cwal_callback_args const * args){
    typedef typename MethodSigT::Context T;
    typedef tmp::Assertion< 7 == sigli::Arity<MethodSigT>::Value > AssertArity;
    if(!AssertArity::Value){}
    ResultType that = 0;
      typedef typename sigli::At<0,MethodSigT>::Type A0;
      typedef typename sigli::At<1,MethodSigT>::Type A1;
      typedef typename sigli::At<2,MethodSigT>::Type A2;
      typedef typename sigli::At<3,MethodSigT>::Type A3;
      typedef typename sigli::At<4,MethodSigT>::Type A4;
      typedef typename sigli::At<5,MethodSigT>::Type A5;
      typedef typename sigli::At<6,MethodSigT>::Type A6;
    try {
      const Argv av(args);
      that = new T(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4),
        av.native<A5>(5),
        av.native<A6>(6)        );
      }catch(std::exception const &ex){
        cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                            "%s constructor (arity=7) threw: %s",
                             TypeName<T>::Value, ex.what());
      }catch(...){
        cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                            "%s constructor (arity=7) threw an unknown exception",
                            TypeName<T>::Value);
      }
      return that;
    }
};

} // namespace Detail
namespace Detail {
template <typename FuncPtrT>
struct QuasiMethodProxy<7, false, FuncPtrT> : CallBackable {
  typedef CallbackSignature SignatureType;
  typedef int ResultType;
  typedef void Context;
  typedef SignatureType::Head Head;
  typedef SignatureType::Tail Tail;

  typedef tmp::Assertion< 7 == sigli::Arity<FuncPtrT>::Value > AssertArity;
  typedef typename sigli::At<0,FuncPtrT>::Type SelfType;
  //typedef tmp::Assertion< tmp::HasPtr< SelfType >::Value  > AssertIsPtr;
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef Dereffer<SelfType> Deref;
    Argv const av(args);
    SelfType that = toNative<SelfType>(args->engine, args->self);
    if(tmp::HasPtr< SelfType >::Value && !Deref()(that)){
      return tossMissingThis<SelfType>(args);
    }
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    typedef typename sigli::At<5,FuncPtrT>::Type A5;
    typedef typename sigli::At<6,FuncPtrT>::Type A6;
    typedef typename FuncPtrT::ResultType RT;
    *rv = toCwal<RT>( args->engine,
                      (RT)FuncPtrT::Function(
                             that,
                             av.native<A1>(0),
                             av.native<A2>(1),
                             av.native<A3>(2),
                             av.native<A4>(3),
                             av.native<A5>(4),
                             av.native<A6>(5)
                      ) );
     return cwal_exception_get(args->engine)
            ? CWAL_RC_EXCEPTION
            : 0;
  }
};
} // namespace Detail

namespace Detail {
template <typename FuncPtrT>
struct QuasiMethodProxy<7, true, FuncPtrT> : CallBackable {
  typedef CallbackSignature SignatureType;
  typedef int ResultType;
  typedef void Context;
  typedef SignatureType::Head Head;
  typedef SignatureType::Tail Tail;
  typedef tmp::Assertion< 7 == sigli::Arity<FuncPtrT>::Value > AssertArity;
  typedef typename sigli::At<0,FuncPtrT>::Type SelfType;
  //typedef tmp::Assertion< tmp::HasPtr< SelfType >::Value  > AssertIsPtr;
  static int callback(cwal_callback_args const * args, cwal_value **){
    Argv const av(args);
    typedef Dereffer<SelfType> Deref;
    SelfType that = toNative<SelfType>(args->engine, args->self);
    if(tmp::HasPtr< SelfType >::Value && !Deref()(that)){
      return tossMissingThis<SelfType>(args);
    }

    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    typedef typename sigli::At<5,FuncPtrT>::Type A5;
    typedef typename sigli::At<6,FuncPtrT>::Type A6;
    FuncPtrT::Function(
      that,
      av.native<A1>(0),
      av.native<A2>(1),
      av.native<A3>(2),
      av.native<A4>(3),
      av.native<A5>(4),
      av.native<A6>(5)
    );
    return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail
//! Specialization for 8 arg(s).
template <typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8>
struct Signature< RV (A1, A2, A3, A4, A5, A6, A7, A8) >
{
    typedef RV ResultType;
    typedef void Context;
    typedef RV (*FunctionType)(A1, A2, A3, A4, A5, A6, A7, A8);
    typedef A1 Head;
    typedef Signature<RV ( A2, A3, A4, A5, A6, A7, A8)> Tail;
};

//! Specialization for 8 arg(s).
template <typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8>
struct Signature< RV (*)(A1, A2, A3, A4, A5, A6, A7, A8) > : Signature<RV (A1, A2, A3, A4, A5, A6, A7, A8)>
{};

//! Specialization for T non-const methods taking 8 arg(s).
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8>
struct Signature< RV (T::*)(A1, A2, A3, A4, A5, A6, A7, A8) > : Signature<RV (A1, A2, A3, A4, A5, A6, A7, A8)>
{
    typedef T Context;
    typedef RV (T::*FunctionType)(A1, A2, A3, A4, A5, A6, A7, A8);
};

//! Specialization for T const methods taking 8 arg(s).
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8>
struct Signature< RV (T::*)(A1, A2, A3, A4, A5, A6, A7, A8) const > : Signature<RV (A1, A2, A3, A4, A5, A6, A7, A8)>
{
    typedef T const Context;
    typedef RV (T::*FunctionType)(A1, A2, A3, A4, A5, A6, A7, A8) const;
};

//! Specialization for 8 args(s)
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8>
struct MethodSignature< T, RV (A1, A2, A3, A4, A5, A6, A7, A8) >
  : Signature< RV (T::*)(A1, A2, A3, A4, A5, A6, A7, A8) >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8>
struct MethodSignature< T const, RV (A1, A2, A3, A4, A5, A6, A7, A8) >
  : Signature< RV (T::*)(A1, A2, A3, A4, A5, A6, A7, A8) const >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8 >
struct MethodSignature< T, RV (*)(A1, A2, A3, A4, A5, A6, A7, A8) >
  : MethodSignature< T, RV (A1, A2, A3, A4, A5, A6, A7, A8) >
{
};
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8 >
struct MethodSignature< T const, RV (*)(A1, A2, A3, A4, A5, A6, A7, A8) >
  : MethodSignature< T const, RV (A1, A2, A3, A4, A5, A6, A7, A8) >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8 >
struct MethodSignature< T, RV (T::*)(A1, A2, A3, A4, A5, A6, A7, A8) >
  : MethodSignature< T, RV (A1, A2, A3, A4, A5, A6, A7, A8) >
{};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8 >
struct MethodSignature< T const, RV (T::*)(A1, A2, A3, A4, A5, A6, A7, A8) >
  : MethodSignature< T const, RV (A1, A2, A3, A4, A5, A6, A7, A8) >
{};

namespace Detail {
template <>
struct F2CbProxy<8, false> {
  template <typename FuncPtrT>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 8 == sigli::Arity<FuncPtrT>::Value > AssertArity;
    if(!AssertArity::Value){}
    Argv const av(args);
    typedef typename sigli::At<0,FuncPtrT>::Type A0;
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    typedef typename sigli::At<5,FuncPtrT>::Type A5;
    typedef typename sigli::At<6,FuncPtrT>::Type A6;
    typedef typename sigli::At<7,FuncPtrT>::Type A7;
    typedef typename FuncPtrT::ResultType RT;
    *rv = toCwal<RT>( args->engine,
                      (RT)FuncPtrT::Function(
                        av.native<A0>(0),
                        av.native<A1>(1),
                        av.native<A2>(2),
                        av.native<A3>(3),
                        av.native<A4>(4),
                        av.native<A5>(5),
                        av.native<A6>(6),
                        av.native<A7>(7)
                      ) );
     return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail

namespace Detail {
template <>
struct F2CbProxy<8, true> {
  template <typename FuncPtrT>
  static int callback(cwal_callback_args const * args, cwal_value **){
    typedef tmp::Assertion< 8 == sigli::Arity<FuncPtrT>::Value > AssertArity;
    if(!AssertArity::Value){}
    Argv const av(args);
    typedef typename sigli::At<0,FuncPtrT>::Type A0;
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    typedef typename sigli::At<5,FuncPtrT>::Type A5;
    typedef typename sigli::At<6,FuncPtrT>::Type A6;
    typedef typename sigli::At<7,FuncPtrT>::Type A7;
    FuncPtrT::Function(
      av.native<A0>(0),
      av.native<A1>(1),
      av.native<A2>(2),
      av.native<A3>(3),
      av.native<A4>(4),
      av.native<A5>(5),
      av.native<A6>(6),
      av.native<A7>(7)
    );
    return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail
namespace Detail {
template <>
struct M2CbProxy<8, false> {
  template <typename MemberPtr>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 8 == sigli::Arity<MemberPtr>::Value > AssertArity;
    if(!AssertArity::Value){}
    typedef typename MemberPtr::ResultType RT;
    typedef typename MemberPtr::Context T;
    T * that = toNative<T>(args->engine, args->self);
    if(!that){
      return tossMissingThis<T>(args);
    }else{
      const Argv av(args);
      typedef typename sigli::At<0,MemberPtr>::Type A0;
      typedef typename sigli::At<1,MemberPtr>::Type A1;
      typedef typename sigli::At<2,MemberPtr>::Type A2;
      typedef typename sigli::At<3,MemberPtr>::Type A3;
      typedef typename sigli::At<4,MemberPtr>::Type A4;
      typedef typename sigli::At<5,MemberPtr>::Type A5;
      typedef typename sigli::At<6,MemberPtr>::Type A6;
      typedef typename sigli::At<7,MemberPtr>::Type A7;
      *rv = toCwal<RT>( args->engine, (RT)(that->*MemberPtr::Function)(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4),
        av.native<A5>(5),
        av.native<A6>(6),
        av.native<A7>(7)
      ) );
      return 0; // FIXME: check s2_engine_err_has()
    }
  }
};

} // namespace Detail
namespace Detail {
template <>
struct M2CbProxy<8, true> {
  template <typename MemberPtr>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 8 == sigli::Arity<MemberPtr>::Value > AssertArity;
    if(!AssertArity::Value){}
    typedef typename MemberPtr::ResultType RT;
    typedef typename MemberPtr::Context T;
    T * that = toNative<T>(args->engine, args->self);
    if(!that){
      return tossMissingThis<T>(args);
    }else{
      const Argv av(args);
      typedef typename sigli::At<0,MemberPtr>::Type A0;
      typedef typename sigli::At<1,MemberPtr>::Type A1;
      typedef typename sigli::At<2,MemberPtr>::Type A2;
      typedef typename sigli::At<3,MemberPtr>::Type A3;
      typedef typename sigli::At<4,MemberPtr>::Type A4;
      typedef typename sigli::At<5,MemberPtr>::Type A5;
      typedef typename sigli::At<6,MemberPtr>::Type A6;
      typedef typename sigli::At<7,MemberPtr>::Type A7;
      (RT)(that->*MemberPtr::Function)(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4),
        av.native<A5>(5),
        av.native<A6>(6),
        av.native<A7>(7)
      );
      *rv = cwal_value_undefined();
      return 0; // FIXME: check s2_engine_err_has()
    }
  }
};
} // namespace Detail
namespace Detail {
template <typename MethodSigT>
struct CtorArgsProxy<8,MethodSigT,true> {
  typedef typename MethodSigT::ResultType ResultType;
  static ResultType construct(cwal_callback_args const * args){
    typedef typename MethodSigT::Context T;
    typedef tmp::Assertion< 8 == sigli::Arity<MethodSigT>::Value > AssertArity;
    if(!AssertArity::Value){}
    ResultType that = 0;
    void * mem = allocFromCwal<T>(args->engine);
      typedef typename sigli::At<0,MethodSigT>::Type A0;
      typedef typename sigli::At<1,MethodSigT>::Type A1;
      typedef typename sigli::At<2,MethodSigT>::Type A2;
      typedef typename sigli::At<3,MethodSigT>::Type A3;
      typedef typename sigli::At<4,MethodSigT>::Type A4;
      typedef typename sigli::At<5,MethodSigT>::Type A5;
      typedef typename sigli::At<6,MethodSigT>::Type A6;
      typedef typename sigli::At<7,MethodSigT>::Type A7;
    if(!mem) return NULL;
    try {
      const Argv av(args);
      that = new (mem) T(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4),
        av.native<A5>(5),
        av.native<A6>(6),
        av.native<A7>(7)        );
    }catch(std::exception const &ex){
      cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                          "%s constructor (arity=8) threw: %s",
                           TypeName<T>::Value, ex.what());
    }catch(...){
      cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                          "%s constructor (arity=8) threw an unknown exception",
                          TypeName<T>::Value);
    }
    if(!that) freeFromCwal<T>(args->engine, mem);
    return that;
  }
};

} // namespace Detail
namespace Detail {
template <typename MethodSigT>
struct CtorArgsProxy<8,MethodSigT,false> {
  typedef typename MethodSigT::ResultType ResultType;
  static ResultType construct(cwal_callback_args const * args){
    typedef typename MethodSigT::Context T;
    typedef tmp::Assertion< 8 == sigli::Arity<MethodSigT>::Value > AssertArity;
    if(!AssertArity::Value){}
    ResultType that = 0;
      typedef typename sigli::At<0,MethodSigT>::Type A0;
      typedef typename sigli::At<1,MethodSigT>::Type A1;
      typedef typename sigli::At<2,MethodSigT>::Type A2;
      typedef typename sigli::At<3,MethodSigT>::Type A3;
      typedef typename sigli::At<4,MethodSigT>::Type A4;
      typedef typename sigli::At<5,MethodSigT>::Type A5;
      typedef typename sigli::At<6,MethodSigT>::Type A6;
      typedef typename sigli::At<7,MethodSigT>::Type A7;
    try {
      const Argv av(args);
      that = new T(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4),
        av.native<A5>(5),
        av.native<A6>(6),
        av.native<A7>(7)        );
      }catch(std::exception const &ex){
        cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                            "%s constructor (arity=8) threw: %s",
                             TypeName<T>::Value, ex.what());
      }catch(...){
        cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                            "%s constructor (arity=8) threw an unknown exception",
                            TypeName<T>::Value);
      }
      return that;
    }
};

} // namespace Detail
namespace Detail {
template <typename FuncPtrT>
struct QuasiMethodProxy<8, false, FuncPtrT> : CallBackable {
  typedef CallbackSignature SignatureType;
  typedef int ResultType;
  typedef void Context;
  typedef SignatureType::Head Head;
  typedef SignatureType::Tail Tail;

  typedef tmp::Assertion< 8 == sigli::Arity<FuncPtrT>::Value > AssertArity;
  typedef typename sigli::At<0,FuncPtrT>::Type SelfType;
  //typedef tmp::Assertion< tmp::HasPtr< SelfType >::Value  > AssertIsPtr;
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef Dereffer<SelfType> Deref;
    Argv const av(args);
    SelfType that = toNative<SelfType>(args->engine, args->self);
    if(tmp::HasPtr< SelfType >::Value && !Deref()(that)){
      return tossMissingThis<SelfType>(args);
    }
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    typedef typename sigli::At<5,FuncPtrT>::Type A5;
    typedef typename sigli::At<6,FuncPtrT>::Type A6;
    typedef typename sigli::At<7,FuncPtrT>::Type A7;
    typedef typename FuncPtrT::ResultType RT;
    *rv = toCwal<RT>( args->engine,
                      (RT)FuncPtrT::Function(
                             that,
                             av.native<A1>(0),
                             av.native<A2>(1),
                             av.native<A3>(2),
                             av.native<A4>(3),
                             av.native<A5>(4),
                             av.native<A6>(5),
                             av.native<A7>(6)
                      ) );
     return cwal_exception_get(args->engine)
            ? CWAL_RC_EXCEPTION
            : 0;
  }
};
} // namespace Detail

namespace Detail {
template <typename FuncPtrT>
struct QuasiMethodProxy<8, true, FuncPtrT> : CallBackable {
  typedef CallbackSignature SignatureType;
  typedef int ResultType;
  typedef void Context;
  typedef SignatureType::Head Head;
  typedef SignatureType::Tail Tail;
  typedef tmp::Assertion< 8 == sigli::Arity<FuncPtrT>::Value > AssertArity;
  typedef typename sigli::At<0,FuncPtrT>::Type SelfType;
  //typedef tmp::Assertion< tmp::HasPtr< SelfType >::Value  > AssertIsPtr;
  static int callback(cwal_callback_args const * args, cwal_value **){
    Argv const av(args);
    typedef Dereffer<SelfType> Deref;
    SelfType that = toNative<SelfType>(args->engine, args->self);
    if(tmp::HasPtr< SelfType >::Value && !Deref()(that)){
      return tossMissingThis<SelfType>(args);
    }

    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    typedef typename sigli::At<5,FuncPtrT>::Type A5;
    typedef typename sigli::At<6,FuncPtrT>::Type A6;
    typedef typename sigli::At<7,FuncPtrT>::Type A7;
    FuncPtrT::Function(
      that,
      av.native<A1>(0),
      av.native<A2>(1),
      av.native<A3>(2),
      av.native<A4>(3),
      av.native<A5>(4),
      av.native<A6>(5),
      av.native<A7>(6)
    );
    return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail
//! Specialization for 9 arg(s).
template <typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8, typename A9>
struct Signature< RV (A1, A2, A3, A4, A5, A6, A7, A8, A9) >
{
    typedef RV ResultType;
    typedef void Context;
    typedef RV (*FunctionType)(A1, A2, A3, A4, A5, A6, A7, A8, A9);
    typedef A1 Head;
    typedef Signature<RV ( A2, A3, A4, A5, A6, A7, A8, A9)> Tail;
};

//! Specialization for 9 arg(s).
template <typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8, typename A9>
struct Signature< RV (*)(A1, A2, A3, A4, A5, A6, A7, A8, A9) > : Signature<RV (A1, A2, A3, A4, A5, A6, A7, A8, A9)>
{};

//! Specialization for T non-const methods taking 9 arg(s).
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8, typename A9>
struct Signature< RV (T::*)(A1, A2, A3, A4, A5, A6, A7, A8, A9) > : Signature<RV (A1, A2, A3, A4, A5, A6, A7, A8, A9)>
{
    typedef T Context;
    typedef RV (T::*FunctionType)(A1, A2, A3, A4, A5, A6, A7, A8, A9);
};

//! Specialization for T const methods taking 9 arg(s).
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8, typename A9>
struct Signature< RV (T::*)(A1, A2, A3, A4, A5, A6, A7, A8, A9) const > : Signature<RV (A1, A2, A3, A4, A5, A6, A7, A8, A9)>
{
    typedef T const Context;
    typedef RV (T::*FunctionType)(A1, A2, A3, A4, A5, A6, A7, A8, A9) const;
};

//! Specialization for 9 args(s)
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8, typename A9>
struct MethodSignature< T, RV (A1, A2, A3, A4, A5, A6, A7, A8, A9) >
  : Signature< RV (T::*)(A1, A2, A3, A4, A5, A6, A7, A8, A9) >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8, typename A9>
struct MethodSignature< T const, RV (A1, A2, A3, A4, A5, A6, A7, A8, A9) >
  : Signature< RV (T::*)(A1, A2, A3, A4, A5, A6, A7, A8, A9) const >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8, typename A9 >
struct MethodSignature< T, RV (*)(A1, A2, A3, A4, A5, A6, A7, A8, A9) >
  : MethodSignature< T, RV (A1, A2, A3, A4, A5, A6, A7, A8, A9) >
{
};
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8, typename A9 >
struct MethodSignature< T const, RV (*)(A1, A2, A3, A4, A5, A6, A7, A8, A9) >
  : MethodSignature< T const, RV (A1, A2, A3, A4, A5, A6, A7, A8, A9) >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8, typename A9 >
struct MethodSignature< T, RV (T::*)(A1, A2, A3, A4, A5, A6, A7, A8, A9) >
  : MethodSignature< T, RV (A1, A2, A3, A4, A5, A6, A7, A8, A9) >
{};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8, typename A9 >
struct MethodSignature< T const, RV (T::*)(A1, A2, A3, A4, A5, A6, A7, A8, A9) >
  : MethodSignature< T const, RV (A1, A2, A3, A4, A5, A6, A7, A8, A9) >
{};

namespace Detail {
template <>
struct F2CbProxy<9, false> {
  template <typename FuncPtrT>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 9 == sigli::Arity<FuncPtrT>::Value > AssertArity;
    if(!AssertArity::Value){}
    Argv const av(args);
    typedef typename sigli::At<0,FuncPtrT>::Type A0;
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    typedef typename sigli::At<5,FuncPtrT>::Type A5;
    typedef typename sigli::At<6,FuncPtrT>::Type A6;
    typedef typename sigli::At<7,FuncPtrT>::Type A7;
    typedef typename sigli::At<8,FuncPtrT>::Type A8;
    typedef typename FuncPtrT::ResultType RT;
    *rv = toCwal<RT>( args->engine,
                      (RT)FuncPtrT::Function(
                        av.native<A0>(0),
                        av.native<A1>(1),
                        av.native<A2>(2),
                        av.native<A3>(3),
                        av.native<A4>(4),
                        av.native<A5>(5),
                        av.native<A6>(6),
                        av.native<A7>(7),
                        av.native<A8>(8)
                      ) );
     return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail

namespace Detail {
template <>
struct F2CbProxy<9, true> {
  template <typename FuncPtrT>
  static int callback(cwal_callback_args const * args, cwal_value **){
    typedef tmp::Assertion< 9 == sigli::Arity<FuncPtrT>::Value > AssertArity;
    if(!AssertArity::Value){}
    Argv const av(args);
    typedef typename sigli::At<0,FuncPtrT>::Type A0;
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    typedef typename sigli::At<5,FuncPtrT>::Type A5;
    typedef typename sigli::At<6,FuncPtrT>::Type A6;
    typedef typename sigli::At<7,FuncPtrT>::Type A7;
    typedef typename sigli::At<8,FuncPtrT>::Type A8;
    FuncPtrT::Function(
      av.native<A0>(0),
      av.native<A1>(1),
      av.native<A2>(2),
      av.native<A3>(3),
      av.native<A4>(4),
      av.native<A5>(5),
      av.native<A6>(6),
      av.native<A7>(7),
      av.native<A8>(8)
    );
    return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail
namespace Detail {
template <>
struct M2CbProxy<9, false> {
  template <typename MemberPtr>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 9 == sigli::Arity<MemberPtr>::Value > AssertArity;
    if(!AssertArity::Value){}
    typedef typename MemberPtr::ResultType RT;
    typedef typename MemberPtr::Context T;
    T * that = toNative<T>(args->engine, args->self);
    if(!that){
      return tossMissingThis<T>(args);
    }else{
      const Argv av(args);
      typedef typename sigli::At<0,MemberPtr>::Type A0;
      typedef typename sigli::At<1,MemberPtr>::Type A1;
      typedef typename sigli::At<2,MemberPtr>::Type A2;
      typedef typename sigli::At<3,MemberPtr>::Type A3;
      typedef typename sigli::At<4,MemberPtr>::Type A4;
      typedef typename sigli::At<5,MemberPtr>::Type A5;
      typedef typename sigli::At<6,MemberPtr>::Type A6;
      typedef typename sigli::At<7,MemberPtr>::Type A7;
      typedef typename sigli::At<8,MemberPtr>::Type A8;
      *rv = toCwal<RT>( args->engine, (RT)(that->*MemberPtr::Function)(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4),
        av.native<A5>(5),
        av.native<A6>(6),
        av.native<A7>(7),
        av.native<A8>(8)
      ) );
      return 0; // FIXME: check s2_engine_err_has()
    }
  }
};

} // namespace Detail
namespace Detail {
template <>
struct M2CbProxy<9, true> {
  template <typename MemberPtr>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 9 == sigli::Arity<MemberPtr>::Value > AssertArity;
    if(!AssertArity::Value){}
    typedef typename MemberPtr::ResultType RT;
    typedef typename MemberPtr::Context T;
    T * that = toNative<T>(args->engine, args->self);
    if(!that){
      return tossMissingThis<T>(args);
    }else{
      const Argv av(args);
      typedef typename sigli::At<0,MemberPtr>::Type A0;
      typedef typename sigli::At<1,MemberPtr>::Type A1;
      typedef typename sigli::At<2,MemberPtr>::Type A2;
      typedef typename sigli::At<3,MemberPtr>::Type A3;
      typedef typename sigli::At<4,MemberPtr>::Type A4;
      typedef typename sigli::At<5,MemberPtr>::Type A5;
      typedef typename sigli::At<6,MemberPtr>::Type A6;
      typedef typename sigli::At<7,MemberPtr>::Type A7;
      typedef typename sigli::At<8,MemberPtr>::Type A8;
      (RT)(that->*MemberPtr::Function)(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4),
        av.native<A5>(5),
        av.native<A6>(6),
        av.native<A7>(7),
        av.native<A8>(8)
      );
      *rv = cwal_value_undefined();
      return 0; // FIXME: check s2_engine_err_has()
    }
  }
};
} // namespace Detail
namespace Detail {
template <typename MethodSigT>
struct CtorArgsProxy<9,MethodSigT,true> {
  typedef typename MethodSigT::ResultType ResultType;
  static ResultType construct(cwal_callback_args const * args){
    typedef typename MethodSigT::Context T;
    typedef tmp::Assertion< 9 == sigli::Arity<MethodSigT>::Value > AssertArity;
    if(!AssertArity::Value){}
    ResultType that = 0;
    void * mem = allocFromCwal<T>(args->engine);
      typedef typename sigli::At<0,MethodSigT>::Type A0;
      typedef typename sigli::At<1,MethodSigT>::Type A1;
      typedef typename sigli::At<2,MethodSigT>::Type A2;
      typedef typename sigli::At<3,MethodSigT>::Type A3;
      typedef typename sigli::At<4,MethodSigT>::Type A4;
      typedef typename sigli::At<5,MethodSigT>::Type A5;
      typedef typename sigli::At<6,MethodSigT>::Type A6;
      typedef typename sigli::At<7,MethodSigT>::Type A7;
      typedef typename sigli::At<8,MethodSigT>::Type A8;
    if(!mem) return NULL;
    try {
      const Argv av(args);
      that = new (mem) T(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4),
        av.native<A5>(5),
        av.native<A6>(6),
        av.native<A7>(7),
        av.native<A8>(8)        );
    }catch(std::exception const &ex){
      cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                          "%s constructor (arity=9) threw: %s",
                           TypeName<T>::Value, ex.what());
    }catch(...){
      cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                          "%s constructor (arity=9) threw an unknown exception",
                          TypeName<T>::Value);
    }
    if(!that) freeFromCwal<T>(args->engine, mem);
    return that;
  }
};

} // namespace Detail
namespace Detail {
template <typename MethodSigT>
struct CtorArgsProxy<9,MethodSigT,false> {
  typedef typename MethodSigT::ResultType ResultType;
  static ResultType construct(cwal_callback_args const * args){
    typedef typename MethodSigT::Context T;
    typedef tmp::Assertion< 9 == sigli::Arity<MethodSigT>::Value > AssertArity;
    if(!AssertArity::Value){}
    ResultType that = 0;
      typedef typename sigli::At<0,MethodSigT>::Type A0;
      typedef typename sigli::At<1,MethodSigT>::Type A1;
      typedef typename sigli::At<2,MethodSigT>::Type A2;
      typedef typename sigli::At<3,MethodSigT>::Type A3;
      typedef typename sigli::At<4,MethodSigT>::Type A4;
      typedef typename sigli::At<5,MethodSigT>::Type A5;
      typedef typename sigli::At<6,MethodSigT>::Type A6;
      typedef typename sigli::At<7,MethodSigT>::Type A7;
      typedef typename sigli::At<8,MethodSigT>::Type A8;
    try {
      const Argv av(args);
      that = new T(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4),
        av.native<A5>(5),
        av.native<A6>(6),
        av.native<A7>(7),
        av.native<A8>(8)        );
      }catch(std::exception const &ex){
        cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                            "%s constructor (arity=9) threw: %s",
                             TypeName<T>::Value, ex.what());
      }catch(...){
        cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                            "%s constructor (arity=9) threw an unknown exception",
                            TypeName<T>::Value);
      }
      return that;
    }
};

} // namespace Detail
namespace Detail {
template <typename FuncPtrT>
struct QuasiMethodProxy<9, false, FuncPtrT> : CallBackable {
  typedef CallbackSignature SignatureType;
  typedef int ResultType;
  typedef void Context;
  typedef SignatureType::Head Head;
  typedef SignatureType::Tail Tail;

  typedef tmp::Assertion< 9 == sigli::Arity<FuncPtrT>::Value > AssertArity;
  typedef typename sigli::At<0,FuncPtrT>::Type SelfType;
  //typedef tmp::Assertion< tmp::HasPtr< SelfType >::Value  > AssertIsPtr;
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef Dereffer<SelfType> Deref;
    Argv const av(args);
    SelfType that = toNative<SelfType>(args->engine, args->self);
    if(tmp::HasPtr< SelfType >::Value && !Deref()(that)){
      return tossMissingThis<SelfType>(args);
    }
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    typedef typename sigli::At<5,FuncPtrT>::Type A5;
    typedef typename sigli::At<6,FuncPtrT>::Type A6;
    typedef typename sigli::At<7,FuncPtrT>::Type A7;
    typedef typename sigli::At<8,FuncPtrT>::Type A8;
    typedef typename FuncPtrT::ResultType RT;
    *rv = toCwal<RT>( args->engine,
                      (RT)FuncPtrT::Function(
                             that,
                             av.native<A1>(0),
                             av.native<A2>(1),
                             av.native<A3>(2),
                             av.native<A4>(3),
                             av.native<A5>(4),
                             av.native<A6>(5),
                             av.native<A7>(6),
                             av.native<A8>(7)
                      ) );
     return cwal_exception_get(args->engine)
            ? CWAL_RC_EXCEPTION
            : 0;
  }
};
} // namespace Detail

namespace Detail {
template <typename FuncPtrT>
struct QuasiMethodProxy<9, true, FuncPtrT> : CallBackable {
  typedef CallbackSignature SignatureType;
  typedef int ResultType;
  typedef void Context;
  typedef SignatureType::Head Head;
  typedef SignatureType::Tail Tail;
  typedef tmp::Assertion< 9 == sigli::Arity<FuncPtrT>::Value > AssertArity;
  typedef typename sigli::At<0,FuncPtrT>::Type SelfType;
  //typedef tmp::Assertion< tmp::HasPtr< SelfType >::Value  > AssertIsPtr;
  static int callback(cwal_callback_args const * args, cwal_value **){
    Argv const av(args);
    typedef Dereffer<SelfType> Deref;
    SelfType that = toNative<SelfType>(args->engine, args->self);
    if(tmp::HasPtr< SelfType >::Value && !Deref()(that)){
      return tossMissingThis<SelfType>(args);
    }

    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    typedef typename sigli::At<5,FuncPtrT>::Type A5;
    typedef typename sigli::At<6,FuncPtrT>::Type A6;
    typedef typename sigli::At<7,FuncPtrT>::Type A7;
    typedef typename sigli::At<8,FuncPtrT>::Type A8;
    FuncPtrT::Function(
      that,
      av.native<A1>(0),
      av.native<A2>(1),
      av.native<A3>(2),
      av.native<A4>(3),
      av.native<A5>(4),
      av.native<A6>(5),
      av.native<A7>(6),
      av.native<A8>(7)
    );
    return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail
//! Specialization for 10 arg(s).
template <typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8, typename A9, typename A10>
struct Signature< RV (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) >
{
    typedef RV ResultType;
    typedef void Context;
    typedef RV (*FunctionType)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10);
    typedef A1 Head;
    typedef Signature<RV ( A2, A3, A4, A5, A6, A7, A8, A9, A10)> Tail;
};

//! Specialization for 10 arg(s).
template <typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8, typename A9, typename A10>
struct Signature< RV (*)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) > : Signature<RV (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)>
{};

//! Specialization for T non-const methods taking 10 arg(s).
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8, typename A9, typename A10>
struct Signature< RV (T::*)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) > : Signature<RV (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)>
{
    typedef T Context;
    typedef RV (T::*FunctionType)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10);
};

//! Specialization for T const methods taking 10 arg(s).
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8, typename A9, typename A10>
struct Signature< RV (T::*)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) const > : Signature<RV (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)>
{
    typedef T const Context;
    typedef RV (T::*FunctionType)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) const;
};

//! Specialization for 10 args(s)
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8, typename A9, typename A10>
struct MethodSignature< T, RV (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) >
  : Signature< RV (T::*)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8, typename A9, typename A10>
struct MethodSignature< T const, RV (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) >
  : Signature< RV (T::*)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) const >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8, typename A9, typename A10 >
struct MethodSignature< T, RV (*)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) >
  : MethodSignature< T, RV (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) >
{
};
template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8, typename A9, typename A10 >
struct MethodSignature< T const, RV (*)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) >
  : MethodSignature< T const, RV (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) >
{
};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8, typename A9, typename A10 >
struct MethodSignature< T, RV (T::*)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) >
  : MethodSignature< T, RV (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) >
{};

template <typename T, typename RV, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8, typename A9, typename A10 >
struct MethodSignature< T const, RV (T::*)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) >
  : MethodSignature< T const, RV (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) >
{};

namespace Detail {
template <>
struct F2CbProxy<10, false> {
  template <typename FuncPtrT>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 10 == sigli::Arity<FuncPtrT>::Value > AssertArity;
    if(!AssertArity::Value){}
    Argv const av(args);
    typedef typename sigli::At<0,FuncPtrT>::Type A0;
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    typedef typename sigli::At<5,FuncPtrT>::Type A5;
    typedef typename sigli::At<6,FuncPtrT>::Type A6;
    typedef typename sigli::At<7,FuncPtrT>::Type A7;
    typedef typename sigli::At<8,FuncPtrT>::Type A8;
    typedef typename sigli::At<9,FuncPtrT>::Type A9;
    typedef typename FuncPtrT::ResultType RT;
    *rv = toCwal<RT>( args->engine,
                      (RT)FuncPtrT::Function(
                        av.native<A0>(0),
                        av.native<A1>(1),
                        av.native<A2>(2),
                        av.native<A3>(3),
                        av.native<A4>(4),
                        av.native<A5>(5),
                        av.native<A6>(6),
                        av.native<A7>(7),
                        av.native<A8>(8),
                        av.native<A9>(9)
                      ) );
     return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail

namespace Detail {
template <>
struct F2CbProxy<10, true> {
  template <typename FuncPtrT>
  static int callback(cwal_callback_args const * args, cwal_value **){
    typedef tmp::Assertion< 10 == sigli::Arity<FuncPtrT>::Value > AssertArity;
    if(!AssertArity::Value){}
    Argv const av(args);
    typedef typename sigli::At<0,FuncPtrT>::Type A0;
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    typedef typename sigli::At<5,FuncPtrT>::Type A5;
    typedef typename sigli::At<6,FuncPtrT>::Type A6;
    typedef typename sigli::At<7,FuncPtrT>::Type A7;
    typedef typename sigli::At<8,FuncPtrT>::Type A8;
    typedef typename sigli::At<9,FuncPtrT>::Type A9;
    FuncPtrT::Function(
      av.native<A0>(0),
      av.native<A1>(1),
      av.native<A2>(2),
      av.native<A3>(3),
      av.native<A4>(4),
      av.native<A5>(5),
      av.native<A6>(6),
      av.native<A7>(7),
      av.native<A8>(8),
      av.native<A9>(9)
    );
    return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail
namespace Detail {
template <>
struct M2CbProxy<10, false> {
  template <typename MemberPtr>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 10 == sigli::Arity<MemberPtr>::Value > AssertArity;
    if(!AssertArity::Value){}
    typedef typename MemberPtr::ResultType RT;
    typedef typename MemberPtr::Context T;
    T * that = toNative<T>(args->engine, args->self);
    if(!that){
      return tossMissingThis<T>(args);
    }else{
      const Argv av(args);
      typedef typename sigli::At<0,MemberPtr>::Type A0;
      typedef typename sigli::At<1,MemberPtr>::Type A1;
      typedef typename sigli::At<2,MemberPtr>::Type A2;
      typedef typename sigli::At<3,MemberPtr>::Type A3;
      typedef typename sigli::At<4,MemberPtr>::Type A4;
      typedef typename sigli::At<5,MemberPtr>::Type A5;
      typedef typename sigli::At<6,MemberPtr>::Type A6;
      typedef typename sigli::At<7,MemberPtr>::Type A7;
      typedef typename sigli::At<8,MemberPtr>::Type A8;
      typedef typename sigli::At<9,MemberPtr>::Type A9;
      *rv = toCwal<RT>( args->engine, (RT)(that->*MemberPtr::Function)(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4),
        av.native<A5>(5),
        av.native<A6>(6),
        av.native<A7>(7),
        av.native<A8>(8),
        av.native<A9>(9)
      ) );
      return 0; // FIXME: check s2_engine_err_has()
    }
  }
};

} // namespace Detail
namespace Detail {
template <>
struct M2CbProxy<10, true> {
  template <typename MemberPtr>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< 10 == sigli::Arity<MemberPtr>::Value > AssertArity;
    if(!AssertArity::Value){}
    typedef typename MemberPtr::ResultType RT;
    typedef typename MemberPtr::Context T;
    T * that = toNative<T>(args->engine, args->self);
    if(!that){
      return tossMissingThis<T>(args);
    }else{
      const Argv av(args);
      typedef typename sigli::At<0,MemberPtr>::Type A0;
      typedef typename sigli::At<1,MemberPtr>::Type A1;
      typedef typename sigli::At<2,MemberPtr>::Type A2;
      typedef typename sigli::At<3,MemberPtr>::Type A3;
      typedef typename sigli::At<4,MemberPtr>::Type A4;
      typedef typename sigli::At<5,MemberPtr>::Type A5;
      typedef typename sigli::At<6,MemberPtr>::Type A6;
      typedef typename sigli::At<7,MemberPtr>::Type A7;
      typedef typename sigli::At<8,MemberPtr>::Type A8;
      typedef typename sigli::At<9,MemberPtr>::Type A9;
      (RT)(that->*MemberPtr::Function)(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4),
        av.native<A5>(5),
        av.native<A6>(6),
        av.native<A7>(7),
        av.native<A8>(8),
        av.native<A9>(9)
      );
      *rv = cwal_value_undefined();
      return 0; // FIXME: check s2_engine_err_has()
    }
  }
};
} // namespace Detail
namespace Detail {
template <typename MethodSigT>
struct CtorArgsProxy<10,MethodSigT,true> {
  typedef typename MethodSigT::ResultType ResultType;
  static ResultType construct(cwal_callback_args const * args){
    typedef typename MethodSigT::Context T;
    typedef tmp::Assertion< 10 == sigli::Arity<MethodSigT>::Value > AssertArity;
    if(!AssertArity::Value){}
    ResultType that = 0;
    void * mem = allocFromCwal<T>(args->engine);
      typedef typename sigli::At<0,MethodSigT>::Type A0;
      typedef typename sigli::At<1,MethodSigT>::Type A1;
      typedef typename sigli::At<2,MethodSigT>::Type A2;
      typedef typename sigli::At<3,MethodSigT>::Type A3;
      typedef typename sigli::At<4,MethodSigT>::Type A4;
      typedef typename sigli::At<5,MethodSigT>::Type A5;
      typedef typename sigli::At<6,MethodSigT>::Type A6;
      typedef typename sigli::At<7,MethodSigT>::Type A7;
      typedef typename sigli::At<8,MethodSigT>::Type A8;
      typedef typename sigli::At<9,MethodSigT>::Type A9;
    if(!mem) return NULL;
    try {
      const Argv av(args);
      that = new (mem) T(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4),
        av.native<A5>(5),
        av.native<A6>(6),
        av.native<A7>(7),
        av.native<A8>(8),
        av.native<A9>(9)        );
    }catch(std::exception const &ex){
      cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                          "%s constructor (arity=10) threw: %s",
                           TypeName<T>::Value, ex.what());
    }catch(...){
      cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                          "%s constructor (arity=10) threw an unknown exception",
                          TypeName<T>::Value);
    }
    if(!that) freeFromCwal<T>(args->engine, mem);
    return that;
  }
};

} // namespace Detail
namespace Detail {
template <typename MethodSigT>
struct CtorArgsProxy<10,MethodSigT,false> {
  typedef typename MethodSigT::ResultType ResultType;
  static ResultType construct(cwal_callback_args const * args){
    typedef typename MethodSigT::Context T;
    typedef tmp::Assertion< 10 == sigli::Arity<MethodSigT>::Value > AssertArity;
    if(!AssertArity::Value){}
    ResultType that = 0;
      typedef typename sigli::At<0,MethodSigT>::Type A0;
      typedef typename sigli::At<1,MethodSigT>::Type A1;
      typedef typename sigli::At<2,MethodSigT>::Type A2;
      typedef typename sigli::At<3,MethodSigT>::Type A3;
      typedef typename sigli::At<4,MethodSigT>::Type A4;
      typedef typename sigli::At<5,MethodSigT>::Type A5;
      typedef typename sigli::At<6,MethodSigT>::Type A6;
      typedef typename sigli::At<7,MethodSigT>::Type A7;
      typedef typename sigli::At<8,MethodSigT>::Type A8;
      typedef typename sigli::At<9,MethodSigT>::Type A9;
    try {
      const Argv av(args);
      that = new T(
        av.native<A0>(0),
        av.native<A1>(1),
        av.native<A2>(2),
        av.native<A3>(3),
        av.native<A4>(4),
        av.native<A5>(5),
        av.native<A6>(6),
        av.native<A7>(7),
        av.native<A8>(8),
        av.native<A9>(9)        );
      }catch(std::exception const &ex){
        cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                            "%s constructor (arity=10) threw: %s",
                             TypeName<T>::Value, ex.what());
      }catch(...){
        cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                            "%s constructor (arity=10) threw an unknown exception",
                            TypeName<T>::Value);
      }
      return that;
    }
};

} // namespace Detail
namespace Detail {
template <typename FuncPtrT>
struct QuasiMethodProxy<10, false, FuncPtrT> : CallBackable {
  typedef CallbackSignature SignatureType;
  typedef int ResultType;
  typedef void Context;
  typedef SignatureType::Head Head;
  typedef SignatureType::Tail Tail;

  typedef tmp::Assertion< 10 == sigli::Arity<FuncPtrT>::Value > AssertArity;
  typedef typename sigli::At<0,FuncPtrT>::Type SelfType;
  //typedef tmp::Assertion< tmp::HasPtr< SelfType >::Value  > AssertIsPtr;
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef Dereffer<SelfType> Deref;
    Argv const av(args);
    SelfType that = toNative<SelfType>(args->engine, args->self);
    if(tmp::HasPtr< SelfType >::Value && !Deref()(that)){
      return tossMissingThis<SelfType>(args);
    }
    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    typedef typename sigli::At<5,FuncPtrT>::Type A5;
    typedef typename sigli::At<6,FuncPtrT>::Type A6;
    typedef typename sigli::At<7,FuncPtrT>::Type A7;
    typedef typename sigli::At<8,FuncPtrT>::Type A8;
    typedef typename sigli::At<9,FuncPtrT>::Type A9;
    typedef typename FuncPtrT::ResultType RT;
    *rv = toCwal<RT>( args->engine,
                      (RT)FuncPtrT::Function(
                             that,
                             av.native<A1>(0),
                             av.native<A2>(1),
                             av.native<A3>(2),
                             av.native<A4>(3),
                             av.native<A5>(4),
                             av.native<A6>(5),
                             av.native<A7>(6),
                             av.native<A8>(7),
                             av.native<A9>(8)
                      ) );
     return cwal_exception_get(args->engine)
            ? CWAL_RC_EXCEPTION
            : 0;
  }
};
} // namespace Detail

namespace Detail {
template <typename FuncPtrT>
struct QuasiMethodProxy<10, true, FuncPtrT> : CallBackable {
  typedef CallbackSignature SignatureType;
  typedef int ResultType;
  typedef void Context;
  typedef SignatureType::Head Head;
  typedef SignatureType::Tail Tail;
  typedef tmp::Assertion< 10 == sigli::Arity<FuncPtrT>::Value > AssertArity;
  typedef typename sigli::At<0,FuncPtrT>::Type SelfType;
  //typedef tmp::Assertion< tmp::HasPtr< SelfType >::Value  > AssertIsPtr;
  static int callback(cwal_callback_args const * args, cwal_value **){
    Argv const av(args);
    typedef Dereffer<SelfType> Deref;
    SelfType that = toNative<SelfType>(args->engine, args->self);
    if(tmp::HasPtr< SelfType >::Value && !Deref()(that)){
      return tossMissingThis<SelfType>(args);
    }

    typedef typename sigli::At<1,FuncPtrT>::Type A1;
    typedef typename sigli::At<2,FuncPtrT>::Type A2;
    typedef typename sigli::At<3,FuncPtrT>::Type A3;
    typedef typename sigli::At<4,FuncPtrT>::Type A4;
    typedef typename sigli::At<5,FuncPtrT>::Type A5;
    typedef typename sigli::At<6,FuncPtrT>::Type A6;
    typedef typename sigli::At<7,FuncPtrT>::Type A7;
    typedef typename sigli::At<8,FuncPtrT>::Type A8;
    typedef typename sigli::At<9,FuncPtrT>::Type A9;
    FuncPtrT::Function(
      that,
      av.native<A1>(0),
      av.native<A2>(1),
      av.native<A3>(2),
      av.native<A4>(3),
      av.native<A5>(4),
      av.native<A6>(5),
      av.native<A7>(6),
      av.native<A8>(7),
      av.native<A9>(8)
    );
    return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail

#endif
/* ^^^ defined(DOXYGEN) */
