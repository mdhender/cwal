#!/bin/bash
########################################################################
# A quick hack to generate Signature templates.
# Usage:
#  $0 to
#  $0 from to
#
# The first form is equivalent to: $0 0 to
#
# Generates Signature<> specializations taking $from .. $to arguments.
# e.g. ($0 3) creates specializations taking 0..3 arguments.
########################################################################

from=${1-0}
to=$2

if [[ 'x' = "x${to}" ]]; then
    to=$from
    from=0
fi

tparam='typename RV'
targs=''

echo '#if !defined(DOXYGEN)'

if [[ 0 = $from ]]; then
from=$((from + 1))
cat <<EOF

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
EOF

fi # $from==0

i=$from
while [[ $i -le $to ]]; do
    tparam="${tparam}, typename A${i}"
    if [[ "X" != "X${targs}" ]]; then
        targs="${targs}, A${i}"
    else
        targs="A1"
    fi

    head="${targs%%,*}"
    tail="${targs#*,}"
    if [[ "$tail" = "$head" ]]; then # happens on 1-arity form
        # reminder to self: whether or not Sig<x (y)>::Tail should be NilType
        # or Sig<x ()> is basically a philosophical question, i think. i find
        # the latter more asthetic but it does cause minor problems in some
        # particular type-list-using algorithms.
        if [[ $i -eq 0 ]]; then
            tail="tmp::NilType"
        else
            tail="Signature< RV () >"
        fi
    else
        tail="Signature<RV (${tail})>"
    fi
    
# Signature<>...
    cat <<EOF
//! Specialization for ${i} arg(s).
template <$tparam>
struct Signature< RV (${targs}) >
{
    typedef RV ResultType;
    typedef void Context;
    typedef RV (*FunctionType)(${targs});
    typedef ${head} Head;
    typedef ${tail} Tail;
};

//! Specialization for ${i} arg(s).
template <$tparam>
struct Signature< RV (*)(${targs}) > : Signature<RV (${targs})>
{};

//! Specialization for T non-const methods taking ${i} arg(s).
template <typename T, $tparam>
struct Signature< RV (T::*)(${targs}) > : Signature<RV (${targs})>
{
    typedef T Context;
    typedef RV (T::*FunctionType)(${targs});
};

//! Specialization for T const methods taking ${i} arg(s).
template <typename T, $tparam>
struct Signature< RV (T::*)(${targs}) const > : Signature<RV (${targs})>
{
    typedef T const Context;
    typedef RV (T::*FunctionType)(${targs}) const;
};

EOF

# MethodSignature<>...
        cat <<EOF
//! Specialization for ${i} args(s)
template <typename T, ${tparam}>
struct MethodSignature< T, RV (${targs}) >
  : Signature< RV (T::*)(${targs}) >
{
};

template <typename T, ${tparam}>
struct MethodSignature< T const, RV (${targs}) >
  : Signature< RV (T::*)(${targs}) const >
{
};

template <typename T, ${tparam} >
struct MethodSignature< T, RV (*)(${targs}) >
  : MethodSignature< T, RV (${targs}) >
{
};
template <typename T, ${tparam} >
struct MethodSignature< T const, RV (*)(${targs}) >
  : MethodSignature< T const, RV (${targs}) >
{
};

template <typename T, ${tparam} >
struct MethodSignature< T, RV (T::*)(${targs}) >
  : MethodSignature< T, RV (${targs}) >
{};

template <typename T, ${tparam} >
struct MethodSignature< T const, RV (T::*)(${targs}) >
  : MethodSignature< T const, RV (${targs}) >
{};

EOF

# F2CbProxy<> part 1...
cat <<EOF
namespace Detail {
template <>
struct F2CbProxy<${i}, false> {
  template <typename FuncPtrT>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< ${i} == sigli::Arity<FuncPtrT>::Value > AssertArity;
    if(!AssertArity::Value){}
    Argv const av(args);
EOF
fN=0
while [[ $fN -lt $i ]]; do
  echo "    typedef typename sigli::At<${fN},FuncPtrT>::Type A${fN};"
  fN=$((fN+1))
done
cat <<EOF
    typedef typename FuncPtrT::ResultType RT;
    *rv = toCwal<RT>( args->engine,
                      (RT)FuncPtrT::Function(
EOF
fN=0
while [[ $fN -lt $i ]]; do
  echo -n "                        av.native<A${fN}>(${fN})"
  if [[ $i -eq $(($fN+1)) ]]; then
      echo "";
  else
      echo ",";
  fi
  fN=$((fN+1))
done
cat <<EOF
                      ) );
     return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail
EOF

echo

# F2CbProxy<> part 2...
cat <<EOF
namespace Detail {
template <>
struct F2CbProxy<${i}, true> {
  template <typename FuncPtrT>
  static int callback(cwal_callback_args const * args, cwal_value **){
    typedef tmp::Assertion< ${i} == sigli::Arity<FuncPtrT>::Value > AssertArity;
    if(!AssertArity::Value){}
    Argv const av(args);
EOF
fN=0
while [[ $fN -lt $i ]]; do
  echo "    typedef typename sigli::At<${fN},FuncPtrT>::Type A${fN};"
  fN=$((fN+1))
done
cat <<EOF
    FuncPtrT::Function(
EOF
fN=0
while [[ $fN -lt $i ]]; do
  echo -n "      av.native<A${fN}>(${fN})"
  if [[ $i -eq $(($fN+1)) ]]; then
      echo "";
  else
      echo ",";
  fi
  fN=$((fN+1))
done
cat <<EOF
    );
    return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail
EOF

# M2CbProxy<> part 1...
cat <<EOF
namespace Detail {
template <>
struct M2CbProxy<${i}, false> {
  template <typename MemberPtr>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< ${i} == sigli::Arity<MemberPtr>::Value > AssertArity;
    if(!AssertArity::Value){}
    typedef typename MemberPtr::ResultType RT;
    typedef typename MemberPtr::Context T;
    T * that = toNative<T>(args->engine, args->self);
    if(!that){
      return tossMissingThis<T>(args);
    }else{
      const Argv av(args);
EOF
fN=0
while [[ $fN -lt $i ]]; do
  echo "      typedef typename sigli::At<${fN},MemberPtr>::Type A${fN};"
  fN=$((fN+1))
done
cat <<EOF
      *rv = toCwal<RT>( args->engine, (RT)(that->*MemberPtr::Function)(
EOF
fN=0
while [[ $fN -lt $i ]]; do
  echo -n "        av.native<A${fN}>(${fN})"
  if [[ $i -eq $(($fN+1)) ]]; then
      echo "";
  else
      echo ",";
  fi
  fN=$((fN+1))
done
cat <<EOF
      ) );
      return 0; // FIXME: check s2_engine_err_has()
    }
  }
};

} // namespace Detail
EOF

# M2CbProxy<> part 2...
cat <<EOF
namespace Detail {
template <>
struct M2CbProxy<${i}, true> {
  template <typename MemberPtr>
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef tmp::Assertion< ${i} == sigli::Arity<MemberPtr>::Value > AssertArity;
    if(!AssertArity::Value){}
    typedef typename MemberPtr::ResultType RT;
    typedef typename MemberPtr::Context T;
    T * that = toNative<T>(args->engine, args->self);
    if(!that){
      return tossMissingThis<T>(args);
    }else{
      const Argv av(args);
EOF
fN=0
while [[ $fN -lt $i ]]; do
  echo "      typedef typename sigli::At<${fN},MemberPtr>::Type A${fN};"
  fN=$((fN+1))
done
cat <<EOF
      (RT)(that->*MemberPtr::Function)(
EOF
fN=0
while [[ $fN -lt $i ]]; do
  echo -n "        av.native<A${fN}>(${fN})"
  if [[ $i -eq $(($fN+1)) ]]; then
      echo "";
  else
      echo ",";
  fi
  fN=$((fN+1))
done
cat <<EOF
      );
      *rv = cwal_value_undefined();
      return 0; // FIXME: check s2_engine_err_has()
    }
  }
};
} // namespace Detail
EOF

# CtorArgsProxy<> part 1: placement new (mem) T()
cat <<EOF
namespace Detail {
template <typename MethodSigT>
struct CtorArgsProxy<${i},MethodSigT,true> {
  typedef typename MethodSigT::ResultType ResultType;
  static ResultType construct(cwal_callback_args const * args){
    typedef typename MethodSigT::Context T;
    typedef tmp::Assertion< ${i} == sigli::Arity<MethodSigT>::Value > AssertArity;
    if(!AssertArity::Value){}
    ResultType that = 0;
    void * mem = allocFromCwal<T>(args->engine);
EOF
fN=0
while [[ $fN -lt $i ]]; do
  echo "      typedef typename sigli::At<${fN},MethodSigT>::Type A${fN};"
  fN=$((fN+1))
done
cat <<EOF
    if(!mem) return NULL;
    try {
      const Argv av(args);
      that = new (mem) T(
EOF
fN=0
while [[ $fN -lt $i ]]; do
  [[ $fN -gt 0 ]] && echo ","
  echo -n "        av.native<A${fN}>(${fN})"
  fN=$((fN+1))
done
cat <<EOF
        );
    }catch(std::exception const &ex){
      cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                          "%s constructor (arity=${i}) threw: %s",
                           TypeName<T>::Value, ex.what());
    }catch(...){
      cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                          "%s constructor (arity=${i}) threw an unknown exception",
                          TypeName<T>::Value);
    }
    if(!that) freeFromCwal<T>(args->engine, mem);
    return that;
  }
};

} // namespace Detail
EOF
# CtorArgsProxy<> part 2: non-placement new T()
cat <<EOF
namespace Detail {
template <typename MethodSigT>
struct CtorArgsProxy<${i},MethodSigT,false> {
  typedef typename MethodSigT::ResultType ResultType;
  static ResultType construct(cwal_callback_args const * args){
    typedef typename MethodSigT::Context T;
    typedef tmp::Assertion< ${i} == sigli::Arity<MethodSigT>::Value > AssertArity;
    if(!AssertArity::Value){}
    ResultType that = 0;
EOF
fN=0
while [[ $fN -lt $i ]]; do
  echo "      typedef typename sigli::At<${fN},MethodSigT>::Type A${fN};"
  fN=$((fN+1))
done
cat <<EOF
    try {
      const Argv av(args);
      that = new T(
EOF
fN=0
while [[ $fN -lt $i ]]; do
  [[ $fN -gt 0 ]] && echo ","
  echo -n "        av.native<A${fN}>(${fN})"
  fN=$((fN+1))
done
cat <<EOF
        );
      }catch(std::exception const &ex){
        cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                            "%s constructor (arity=${i}) threw: %s",
                             TypeName<T>::Value, ex.what());
      }catch(...){
        cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                            "%s constructor (arity=${i}) threw an unknown exception",
                            TypeName<T>::Value);
      }
      return that;
    }
};

} // namespace Detail
EOF

# QuasiMethodProxy<> part 1...
thatComma=""
[[ $i -gt 1 ]] && thatComma=","
cat <<EOF
namespace Detail {
template <typename FuncPtrT>
struct QuasiMethodProxy<${i}, false, FuncPtrT> : CallBackable {
  typedef CallbackSignature SignatureType;
  typedef int ResultType;
  typedef void Context;
  typedef SignatureType::Head Head;
  typedef SignatureType::Tail Tail;

  typedef tmp::Assertion< ${i} == sigli::Arity<FuncPtrT>::Value > AssertArity;
  typedef typename sigli::At<0,FuncPtrT>::Type SelfType;
  //typedef tmp::Assertion< tmp::HasPtr< SelfType >::Value  > AssertIsPtr;
  static int callback(cwal_callback_args const * args, cwal_value **rv){
    typedef Dereffer<SelfType> Deref;
    Argv const av(args);
    SelfType that = toNative<SelfType>(args->engine, args->self);
    if(tmp::HasPtr< SelfType >::Value && !Deref()(that)){
      return tossMissingThis<SelfType>(args);
    }
EOF
fN=1
while [[ $fN -lt $i ]]; do
  echo "    typedef typename sigli::At<${fN},FuncPtrT>::Type A${fN};"
  fN=$((fN+1))
done
cat <<EOF
    typedef typename FuncPtrT::ResultType RT;
    *rv = toCwal<RT>( args->engine,
                      (RT)FuncPtrT::Function(
                             that${thatComma}
EOF
fN=1
while [[ $fN -lt $i ]]; do
  echo -n "                             av.native<A${fN}>($((${fN} - 1)))"
  if [[ $i -eq $(($fN+1)) ]]; then
      echo "";
  else
      echo ",";
  fi
  fN=$((fN+1))
done
cat <<EOF
                      ) );
     return cwal_exception_get(args->engine)
            ? CWAL_RC_EXCEPTION
            : 0;
  }
};
} // namespace Detail
EOF

echo

# QuasiMethodProxy<> part 2...
cat <<EOF
namespace Detail {
template <typename FuncPtrT>
struct QuasiMethodProxy<${i}, true, FuncPtrT> : CallBackable {
  typedef CallbackSignature SignatureType;
  typedef int ResultType;
  typedef void Context;
  typedef SignatureType::Head Head;
  typedef SignatureType::Tail Tail;
  typedef tmp::Assertion< ${i} == sigli::Arity<FuncPtrT>::Value > AssertArity;
  typedef typename sigli::At<0,FuncPtrT>::Type SelfType;
  //typedef tmp::Assertion< tmp::HasPtr< SelfType >::Value  > AssertIsPtr;
  static int callback(cwal_callback_args const * args, cwal_value **){
    Argv const av(args);
    typedef Dereffer<SelfType> Deref;
    SelfType that = toNative<SelfType>(args->engine, args->self);
    if(tmp::HasPtr< SelfType >::Value && !Deref()(that)){
      return tossMissingThis<SelfType>(args);
    }

EOF
fN=1
while [[ $fN -lt $i ]]; do
  echo "    typedef typename sigli::At<${fN},FuncPtrT>::Type A${fN};"
  fN=$((fN+1))
done
cat <<EOF
    FuncPtrT::Function(
      that${thatComma}
EOF
fN=1
while [[ $fN -lt $i ]]; do
  echo -n "      av.native<A${fN}>($((${fN} - 1)))"
  if [[ $i -eq $(($fN+1)) ]]; then
      echo "";
  else
      echo ",";
  fi
  fN=$((fN+1))
done
cat <<EOF
    );
    return 0; // FIXME: check s2_engine_err_has()
  }
};
} // namespace Detail
EOF

    #echo $tparam
    #echo $targs
    i=$((i + 1))
done

echo
echo '#endif'
echo '/* ^^^ defined(DOXYGEN) */'
