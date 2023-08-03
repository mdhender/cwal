/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#ifndef NET_WANDERINGHORSE_CWAL_CONVERT_HPP_INCLUDED_
#define NET_WANDERINGHORSE_CWAL_CONVERT_HPP_INCLUDED_


/**
   Define CWAL_CONVERT_ENABLE_S2 to true or false
   to enable/disable s2-only extensions. They are
   enabled by default if s2's main header was included
   before this one.
*/
#if !defined(CWAL_CONVERT_ENABLE_S2)
#  if defined(s2_engine_empty_m)
#    define CWAL_CONVERT_ENABLE_S2 1
#  else
#    define CWAL_CONVERT_ENABLE_S2 0
#  endif
#endif
/**
   If s2 support is not enabled, whcl support may be enabled. It's
   functionally identical to s2's but is based on the whcl API instead
   of s2's. They two are mutually exclusive, however - they cannot
   both be enabled in a single build.
*/
#if !defined(CWAL_CONVERT_ENABLE_WHCL)
#  if !CWAL_CONVERT_ENABLE_S2 && defined(whcl_engine_empty_m)
#    define CWAL_CONVERT_ENABLE_WHCL 1
#  else
#    define CWAL_CONVERT_ENABLE_WHCL 0
#  endif
#endif
/*
  Reminder to self: this code only needs cwal, not s2, but it's
  currently being developed in conjunction with s2 for convenience.
*/
#if !CWAL_CONVERT_ENABLE_S2
#  if !defined(cwal_engine_empty_m)
#    include "libcwal.h"
#  endif
#else
#  include "libs2.h"
#endif

#include <cassert>
#include <string>
#include <iostream>
#include <sstream>
#include <map>

#include <stdexcept>
#include <typeinfo> /* std::bad_cast */

/*
    doxygen REFUSES to document class templates i forward-decl, even when
    adding "@class" to the docs. Being the doc-pedant that i am, i felt
    compelled to work around that. With gcc we can add an empty body to
    those classes but MSVC doesn't like it. So we leave it out unless
    doxygen is building, and then doxygen needs to be configured with:

    - ENABLE_PREPROCESSING = YES
    - EXPAND_AS_DEFINED = DOXYGEN_FWD_DECL_KLUDGE

    and possibly:

    - MACRO_EXPANSION = YES

    and/or:

    - EXPAND_ONLY_PREDEF = YES
*/
#if !defined(DOXYGEN)
#  if !defined DOXYGEN_FWD_DECL_KLUDGE
#    define DOXYGEN_FWD_DECL_KLUDGE
#  endif
#else
#  if !defined DOXYGEN_FWD_DECL_KLUDGE
#    define DOXYGEN_FWD_DECL_KLUDGE {}
#  endif
#endif

/**
   Utilities for assisting s2 bindings to C++ code, primarily in the
   areas of type conversion and binding functions.

   Its primary features:

   - conversions between cwal_value and various "native" types,
   trainable via template specializations.

   - conversion from C/C++ free functions and methods to
   cwal_callback_f() implementations. This does much of
   the drudge work of binding code to cwal.

   - Conversion of C++ ctors to cwal_native bindings, such that
   script code can call (new T(...)) to instantiate instances and
   cwal will manage their lifetimes.

   Based largely off of prior work done for the v8 JavaScript engine:

   https://code.google.com/p/v8-juice/wiki/V8Convert_XTo

   (And, before that, work based on the SpiderMonkey JavaScript
   engine.)

   License: same as cwal: dual MIT/Public Domain.

   This API is demonstrated in the files
   s2/mod/sample_cpp/sample_cpp.{cpp,s2}, relative to the top of this
   project's canonical source tree:

   https://fossil.wanderinghorse.net/r/cwal/finfo/s2/mod/sample_cpp/sample_cpp.cpp
   https://fossil.wanderinghorse.net/r/cwal/finfo/s2/mod/sample_cpp/sample_cpp.s2
   https://fossil.wanderinghorse.net/r/cwal/dir/s2/mod/sample_cpp?ci=trunk
*/
namespace cwal {

  /**
     The tmp namespace contains code related to template
     metaprogramming, a-la Alexandrescu's Loki library or Boost MPL,
     though in miniaturized scope. tmp = Template MetaProgramming, not
     "temporary", by the way.
   
     This is not a complete/full-featured TMP library - it only
     contains the templates needed by our higher-level library-level
     code.
  */
  namespace tmp {

    //! Sentry type for the end of type lists.
    struct NilType {};
    typedef NilType nil;

    /**
       An utmost most-basic compile-time assertion template.
       If Condition is false, an incomplete specialization of
       this type is invoked, causing a compile-time error.

       As of "some newer GCC version," unused local typedefs
       cause a warning (which translates to an error when one
       builds with -Wall). That can be worked around by
       adding:

       @code
       typedef tmp::Assertion< ... > AssertFoo;
       if(!AssertFoo::Value){} // compiler warning kludge!
       @endcode

       Which of course does nothing, but (A) shuts up gcc and (B) will
       likely be optimized out by the compiler because if the
       condition is ever false, the Assertion typedef won't compile.
    */
    template <bool Condition>
    struct Assertion{
      enum { Value = 1 };
    };

    /** Unimplemented - causes a compile-time error if used. */
    template <>
    struct Assertion<false>;


    /** A convenience base type for metafunctions holding a constant value. */
    template <typename ValType,ValType Val>
    struct ConstVal{
      static const ValType Value = Val;
    };

    /** A metafunction holding an integer constant. */
    template <int Val>
    struct IntVal : ConstVal<int,Val> {};

    /** A metafunction holding an unsigned short constant. */
    template <unsigned short Val>
    struct UShortVal : ConstVal<unsigned short,Val> {};

    /** A metafunction holding a bool constant. */
    template <bool Val>
    struct BoolVal : ConstVal<bool,Val> {};

    /** A metafunction whos Value member is true if X and Y are the same type. */
    template <typename X,typename Y>
    struct SameType : BoolVal<false> {};

    /** Specialization for X==Y. */
    template <typename X>
    struct SameType<X,X> : BoolVal<true> {};

    /** Resolves as-is to T. */
    template <typename T>
    struct Identity{
      typedef T Type;
    };

    /** Resolves to T, shorn of pointerness and constness. */
    template <typename T>
    struct PlainType{
      typedef T Type;
    };
    template <typename T>
    struct PlainType<T const> : PlainType<T> {};
    template <typename T>
    struct PlainType<T *> : PlainType<T> {};
    template <typename T>
    struct PlainType<T const *> : PlainType<T> {};
    template <typename T>
    struct PlainType<T &> : PlainType<T> {};
    template <typename T>
    struct PlainType<T const &> : PlainType<T> {};

#if 0
    /** Metatemplate whose Type evaluates to (T*). */
    template <typename T>
    struct AddPointer{
      typedef T * Type;
    };
    /** Specialization whose Type evaluates to (T *). */
    template <typename T>
    struct AddPointer<T *>{
      typedef T * Type;
    };
    /** Specialization whose Type evaluates to (T const *). */
    template <typename T>
    struct AddPointer<T const *>{
      typedef T const * Type;
    };

    //! Unimplemented. How to handle this?
    template <typename T>
    struct AddPointer<T const &>;
    //! Unimplemented. How to handle this?
    template <typename T>
    struct AddPointer<T &>;
#endif

    template <typename T>
    struct IsConst : BoolVal<false> {};
    template <typename T>
    struct IsConst<T const> : BoolVal<true> {};
    template <typename T>
    struct IsConst<T const &> : IsConst<T const> {};
    template <typename T>
    struct IsConst<T const *> : IsConst<T const> {};

    template <typename T>
    struct IsNil : SameType<T,NilType> {};

    template <typename T>
    struct HasPtr : BoolVal<false> {};
    template <typename T>
    struct HasPtr<T &> : HasPtr<T>{};
    template <typename T>
    struct HasPtr<T const &> : HasPtr<T>{};
    template <typename T>
    struct HasPtr<T const> : HasPtr<T>{};
    template <typename T>
    struct HasPtr<T *> : BoolVal<true> {};
    template <typename T>
    struct HasPtr<T const *> : HasPtr<T*>{};

    template <typename T>
    struct HasRef : BoolVal<false> {};
    template <typename T>
    struct HasRef<T &> : BoolVal<true> {};
    template <typename T>
    struct HasRef<T const &> : HasRef<T &>{};
    template <typename T>
    struct HasRef<T const> : HasRef<T>{};
    template <typename T>
    struct HasRef<T *> : BoolVal<false> {};
    template <typename T>
    struct HasRef<T const *> : HasRef<T*>{};


  } // namespace tmp


  /**
     A utility type for holding a type name and, in some
     specializations (void const *) type ID (because we've no better
     place to put it and a new class just for that seems like
     overkill). The primary purpose of this class is to generate more
     useful error strings which include the affected/causing types'
     names.

     See the CWAL_TYPE_NAME_DECL() and CWAL_TYPE_NAME_IMPL2() macros.
  */
  template <typename T>
  struct TypeName{
    typedef T Type;
    static char const * Value;
    /**
       Used in several places to associate an arbitrary pointer as a
       type ID for (void*) associated with a (T*). This approach
       allows the C API (which only knows (void*)) to behave
       type-safely in that its APIs require a type ID match when
       fetching the (void*), as a way of validating the pointer/type
       combination.

       The value pointed to is irrelevant - only this member's address
       is used. Thus it must be unique for all types, with some
     */
    static void const * TypeID;
  };

  template <typename T>
  struct TypeName<T const> : TypeName<T>{};

  template <typename T>
  struct TypeName<T *> : TypeName<T>{};

  template <typename T>
  struct TypeName<T const *> : TypeName<T>{};

  template <typename T>
  struct TypeName<T &> : TypeName<T>{};

  template <typename T>
  struct TypeName<T const &> : TypeName<T>{};

  template <typename T>
  char const * typeName(){ return TypeName<T>::Value; }

/**
   Declared (does not implement) a TypeName< TYPE > specialization.

   Must be called one time, in the cwal namespace, from the
   declaration area of each client type which will use the TypeName
   interface. This implicitly includes most client types passed to
   toNative() or toCwal(), as those routines (and many related ones)
   may use TypeName<T>::Value in error/exception strings.

   Pass TYPE wrapped in extra parens, so that it can support nested templates:

   @code
   namespace cwal{
     CWAL_TYPE_NAME_DECL((MyType<A,B>));
   }
   @endcode

   Use CWAL_TYPE_NAME_IMPL1 or CWAL_TYPE_NAME_IMPL2
   to implement the template (which normally must be done in C++ files,
   not the header(s), except in very simple use cases).
*/
#define CWAL_TYPE_NAME_DECL(TYPE)                  \
  template <>                                   \
  struct TypeName< sigli::At<0, Signature< void TYPE > >::Type > {  \
    typedef sigli::At<0, Signature< void TYPE > >::Type Type; \
    static char const * Value;                   \
    static void const * TypeID;                   \
  }

/**
   Must be called one time, in the cwal namespace, from the
   implementation area of each client type which will use the TypeName
   interface. It specifies the type's name, namely for use in
   generating error messages.

   TYPE is the type to name. Pass TYPE wrapped in extra parens so that
   commas in the name will work.

   NAME is a string name for the type. It need not strictly be unique,
   but non-unique names might cause confusion. It also need not strictly
   be a script-identifier-friendly name, e.g. "Foo<Bar<1,2>>" is a legal
   name.

   Example:

   @code
   namespace cwal{
     CWAL_TYPE_NAME_IMPL2((MyType<A,B>),"MyType");
   }
   @endcode
*/    
#define CWAL_TYPE_NAME_IMPL2(TYPE,NAME)         \
  char const * TypeName< sigli::At<0, Signature<void TYPE > >::Type >::Value = NAME; \
  void const * TypeName< sigli::At<0, Signature<void TYPE > >::Type >::TypeID = NAME

/**
   Convenience form which uses \#TYPE as the type name, but not that
   (A) TYPE must be passed in with extra parens around it and (B)
   those parens become part of the name string.
*/
#define CWAL_TYPE_NAME_IMPL1(TYPE) CWAL_TYPE_NAME_IMPL2(TYPE, #TYPE)

  /**
     Internal utility to check for cwal-level exceptions after
     conversions which might trigger them but cannot (because of
     differing interfaces) report them directly.

     It's only a template so that we don't need an extra implementation
     file for an otherwise header-only library :/.
  */
  template <typename Ignored=void>
  struct RcHelperImpl{
    /**
       Passed callback arguments and the result code for the callback (possibly 0),
       this function:

       a) If rc is not 0, returns it immediately.

       b) If cwal_exception_get() returns non-0,
       CWAL_RC_EXCEPTION. Code should continue propagating this error
       code up the script-space call state so that it can be handled
       properly by the scripting layer.

       c) Else returns 0.

       Bug: we cannot tell from here, without adding flags to s2 and
       a dep on s2 to this code, if s2 has an 'exit' or 'fatal'
       propagating, and it's potentially possible that a type
       conversion which triggers one of those will cause the
       exit/fatal error to be hidden/ignored.
       
       TODO: add a CWAL_CONVERT_ENABLE_S2 block which extracts the s2_engine
       from args and checks the fatal error state. Doh - we can only catch
       the is-interrupted state there, not exit/fatal, without notable
       hacking/additions (small amounts in many places, i suspect).
     */
    int operator()( cwal_callback_args const * args, int rc ) const{
      return rc
      ? rc
      : (cwal_exception_get(args->engine)
         ? CWAL_RC_EXCEPTION
         : 0);
    }
  };

  /**
     Convenience instance of RcHelperImpl.
  */
  static const RcHelperImpl<> RcHelper = RcHelperImpl<>();

  /**
     This unimplemented template specifies the base interface for
     cwal_value-to-X type conversions. It must be specialized on T to be
     useful. Partial specializations alias (T*) and (T const) to this
     specialization by default, and the implementations for
     client-side types are generally expected (because practice has
     shown it to be so) to return pointers (references may be okay,
     too, depending on the type and the the conversion).

     Reminder to self: the Detail::Dereffer template abstracts away the
     pointer-vs-ref difference, and algos which work with these
     conversions "should" use that so that they can cover cases where
     the conversion only returns a reference.
  */
  template <typename T>
  struct CwalToNative{
    typedef T Type;
    typedef T ResultType;
    ResultType operator()( cwal_engine * e, cwal_value * v ) const;
  };

  template <typename T>
  struct CwalToNative<T const> : CwalToNative<T>{};

  template <typename T>
  struct CwalToNative<T *> : CwalToNative<T>{};

  namespace Detail {
    /**
       Internal helper which generates an exception message.
       TypeName<T> must be specialized.
    */
    template <typename T>
    std::string getNullDerefMsg(){
      std::ostringstream os;
      os << "Throwing to avoid a deref of "
         << "a NULL "<< TypeName<T>::Value<<" pointer.";
      return os.str();
    }

    /**
       Internal helper which generates an exception message.
       TypeName<T> must be specialized.
    */
    template <typename T>
    std::string getMissingThisMsg(){
      std::ostringstream os;
      os << "'this' is not (or is no longer) bound to a "
         << TypeName<T>::Value<<" pointer.";
      return os.str();
    }
  }

  /**
     Helper for cwal_callback_f() implementations which returns
     CWAL_RC_EXCEPTION on success (some other non-0 code on more
     serious errors, e.g. OOM) with a message explaining that a "this"
     pointer of type T could not be found in args->self or (depending
     on the context) one of its args->argv entries.
   */
  template <typename T>
  int tossMissingThis(cwal_callback_args const * args){
    static std::string const & msg = Detail::getMissingThisMsg<T>();
    return cwal_exception_setf(args->engine, CWAL_RC_TYPE,
                               "%.*s", (int)msg.size(),
                               msg.c_str());
  }


  namespace Detail {
    /**
       A helper to get a pointer to a T.
    */
    template <typename T>
    struct Dereffer{
      T const * operator()( T const & t ) const{
        return &t;
      }
    };
    template <typename T>
    struct Dereffer<T const &> : Dereffer<T>{};

    template <typename T>
    struct Dereffer<T*>{
      T * operator()( T * t ) const{
        return t;
      }
    };
    template <typename T>
    struct Dereffer<T const *>{
      T const * operator()( T const * t ) const{
        return t;
      }
    };
    template <typename T>
    struct Dereffer<T &>{
      T * operator()( T & t ) const{
        return &t;
      }
    };

    template <typename T>
    struct DummyPlaceholder{
    };
  }

  //! Specialization to attempt conversion to a const reference. Throws on error.
  template <typename T>
  struct CwalToNative<T const &>{
    typedef T Type;
    typedef T const & ResultType;
    ResultType operator()( cwal_engine * e, cwal_value * v ) const{
      typedef CwalToNative<T const *> Proxy;
      typedef typename Proxy::ResultType PR;
      typedef tmp::Assertion<tmp::HasPtr<PR>::Value || tmp::HasRef<PR>::Value> ResultTypeDerefable;
      typedef Detail::Dereffer<PR> Deref;
      if(!ResultTypeDerefable::Value){}
      PR p = Proxy()(e, v);
      Deref const & deref = Deref();
      if(!deref(p)){
        std::string const & msg(Detail::getNullDerefMsg<T>());
        throw std::runtime_error(msg.c_str());
      }else{
        return *deref(p);
      }
    }
  };

  //! Specialization to attempt conversion to a reference. Throws on error.
  template <typename T>
  struct CwalToNative<T &>{
    typedef T Type;
    typedef T & ResultType;
    ResultType operator()( cwal_engine * e, cwal_value * v ) const{
      typedef CwalToNative<T*> Proxy;
      typedef typename Proxy::ResultType PR;
      typedef tmp::Assertion<tmp::HasPtr<PR>::Value || tmp::HasRef<PR>::Value> ResultTypeDerefable;
      typedef Detail::Dereffer<PR> Deref;
      if(!ResultTypeDerefable::Value){}
      PR p = Proxy()(e, v);
      Deref const & deref = Deref();
      if(!deref(p)){
        std::string const & msg(Detail::getNullDerefMsg<T>());
        throw std::runtime_error(msg.c_str());
      }else{
        return *deref(p);
      }
    }
  };


  /**
     This unimplemented template specifies the base interface for
     X-to-cwal_value conversions.
  */
  template <typename T>
  struct NativeToCwal{
    /**
       Required by specializations. They must specify the fully-qualified
       specialized type.
    */
    typedef T Type;
    /**
       Required by specializations. They must specify the type used as
       the 2nd argument for operator().
    */
    typedef const T * ArgType;

    /**
       Implementations must (somehow) convert n to a cwal_value
       representation, allocating a new value if needed (but _not_
       explicitly adding a reference to it). Returning 0 is generally
       interpretted as "out of memory. Compile-time type safety
       ensures that we can restrict and extend what types are legal
       for this operator, and it is expected that all calls which are
       legal produce something.
    */
    cwal_value * operator()( cwal_engine * e, ArgType n ) const;
  };

  template <typename T>
  struct NativeToCwal<T const> : NativeToCwal<T>{};

  template <typename T>
  struct NativeToCwal<T const *> : NativeToCwal<T>{};

  template <typename T>
  struct NativeToCwal<T *> : NativeToCwal<T const *>{};

  template <typename T>
  struct NativeToCwal<T &> : NativeToCwal<T const &>{
    typedef T & Type;
    typedef T & ArgType;
    cwal_value * operator()( cwal_engine * e, ArgType n ) const{
      typedef NativeToCwal<T*> Proxy;
      return Proxy()(e, &n);
    }
  };
  template <typename T>
  struct NativeToCwal<T const &>{
    typedef T const & Type;
    typedef T const & ArgType;
    cwal_value * operator()( cwal_engine * e, ArgType n ) const{
      typedef NativeToCwal<T const *> Proxy;
      return Proxy()(e, &n);
    }
  };

#if 0
  template <typename T>
  struct NativeToCwal<T const &>{
    typedef T const & Type;
    typedef T const & ArgType;
    cwal_value * operator()( cwal_engine * e, ArgType n ) const{
      typedef NativeToCwal<T> Proxy;
      return Proxy()(e, &n);
    }
  };
#endif

  /**
     Returns NativeToCwal<T>()(e, n).
  */
  template <typename T>
  cwal_value * toCwal( cwal_engine * e, typename NativeToCwal<T>::ArgType n ){
    typedef NativeToCwal<T> proxy;
    return proxy()(e, n);
  }

#if 0
  /** Convenience form for use in callback implementations. */
  template <typename T>
  cwal_value * toCwal( cwal_callback_args const * args,
                       typename NativeToCwal<T>::ArgType n ){
    return toCwal<T>( args->engine, n );
  }
#endif

#if 0
  char const * toCstr( cwal_value const * v ){
    return cwal_value_get_cstr(v, 0);
  }
#endif

  /**
     Returns CwalToNative<T>()(e, v).
  */
  template <typename T> typename CwalToNative<T>::ResultType
  toNative( cwal_engine * e, cwal_value * v ){
    typedef CwalToNative<T> proxy;
    return proxy()(e, v);
  }

  /** Convenience form for use in callback implementations. */
  template <typename T> typename CwalToNative<T>::ResultType
  toNative( cwal_callback_args const * args, uint16_t index ){
    return toNative<T>(args->engine, index>=args->argc ? 0 : args->argv[index]);
  }


#if 0
  // ambiguous when passed 0.
  /** Convenience form for use in callback implementations. */
  template <typename T> typename CwalToNative<T>::ResultType
  toNative( cwal_callback_args const * args, cwal_value * v ){
    return toNative<T>(args->engine, v);
  }
#endif

  template <typename IntT>
  struct CwalToNative_int{
    typedef IntT Type;
    typedef Type ResultType;
    ResultType operator()( cwal_engine *, cwal_value * v ) const{
      return (ResultType)cwal_value_get_integer(v);
    }
  };

  template <>
  struct CwalToNative<int16_t> : CwalToNative_int<int16_t>{};

  template <>
  struct CwalToNative<int32_t> : CwalToNative_int<int32_t>{};

  template <>
  struct CwalToNative<int64_t> : CwalToNative_int<int64_t>{};

  template <>
  struct CwalToNative<uint16_t> : CwalToNative_int<uint16_t>{};

  template <>
  struct CwalToNative<uint32_t> : CwalToNative_int<uint32_t>{};

  template <>
  struct CwalToNative<uint64_t> : CwalToNative_int<uint64_t>{};

  template <>
  struct CwalToNative<double>{
    typedef double Type;
    typedef Type ResultType;
    ResultType operator()( cwal_engine *, cwal_value * v ) const{
      return (ResultType)cwal_value_get_double(v);
    }
  };

  template <>
  struct CwalToNative<float>{
    typedef float Type;
    typedef Type ResultType;
    ResultType operator()( cwal_engine *, cwal_value * v ) const{
      return (ResultType)cwal_value_get_double(v);
    }
  };

  template <typename IntT>
  struct NativeToCwal_int {
    typedef IntT Type;
    typedef Type ArgType;
    cwal_value * operator()( cwal_engine * e, ArgType n ) const{
      cwal_value * v = cwal_new_integer(e, (cwal_int_t)n);
      if(!v) throw std::bad_alloc();
      else return v;
    }
  };

  template <>
  struct NativeToCwal<int16_t> : NativeToCwal_int<int16_t>{};
  template <>
  struct NativeToCwal<int32_t> : NativeToCwal_int<int32_t>{};
  template <>
  struct NativeToCwal<int64_t> : NativeToCwal_int<int64_t>{};

  template <>
  struct NativeToCwal<uint16_t> : NativeToCwal_int<uint16_t>{};
  template <>
  struct NativeToCwal<uint32_t> : NativeToCwal_int<uint32_t>{};
  template <>
  struct NativeToCwal<uint64_t> : NativeToCwal_int<uint64_t>{};


  template <>
  struct NativeToCwal<double>{
    typedef double Type;
    typedef Type ArgType;
    cwal_value * operator()( cwal_engine * e, ArgType n ) const{
      cwal_value * v = cwal_new_double(e, (cwal_double_t)n);
      if(!v) throw std::bad_alloc();
      else return v;
    }
  };

  template <>
  struct NativeToCwal<float>{
    typedef float Type;
    typedef Type ArgType;
    cwal_value * operator()( cwal_engine * e, ArgType n ) const{
      cwal_value * v = cwal_new_double(e, (cwal_double_t)n);
      if(!v) throw std::bad_alloc();
      else return v;
    }
  };

  template <>
    struct NativeToCwal<void>{
    typedef void Type;
    typedef void ArgType;
    template <typename ArgT>
      cwal_value * operator()( cwal_engine *, ArgT ) const{
      return cwal_value_undefined();
    }
  };


  template <>
  struct NativeToCwal<char const *>{
    typedef char const * Type;
    typedef Type ArgType;
    cwal_value * operator()( cwal_engine * e, ArgType n ) const{
      cwal_value * rc = n
        ? cwal_new_string_value(e, n, cwal_strlen(n))
        : cwal_value_undefined();
      if(!rc) throw std::bad_alloc();
      return rc;
    }
  };


  template <>
  struct CwalToNative<char const *>{
    typedef char const * Type;
    typedef Type ResultType;
    ResultType operator()( cwal_engine *, cwal_value * v ) const{
      return cwal_value_get_cstr(v, 0);
    }
  };

  /** Not implemented. This conversion cannot be done safely/generically. */
  template <>
  struct NativeToCwal<char *>{};

  template <>
  struct NativeToCwal<std::string>{
    typedef std::string Type;
    typedef Type const & ArgType;
    cwal_value * operator()( cwal_engine * e, ArgType n ) const{
      cwal_value * rc;
      size_t const slen = n.size();
      if(!(rc = cwal_new_string_value(e, slen ? n.c_str() : 0,
                                      (cwal_size_t)slen))){
        throw std::bad_alloc();
      }
      return rc;
    }
  };
  template <>
  struct NativeToCwal<std::string const &> : NativeToCwal<std::string>{};


  template <>
  struct CwalToNative<std::string>{
    typedef std::string Type;
    typedef Type ResultType;
    /**
       If cwal_value_get_cstr() returns non-NULL and a length of >0, a
       copy of its string bytes is returned, else an empty string is
       returned.
    */
    ResultType operator()( cwal_engine *, cwal_value * v ) const{
      cwal_size_t len = 0;
      char const * cstr = cwal_value_get_cstr(v, &len);
      return (cstr && len) ? std::string(cstr,(size_t)len) : std::string();
    }
  };

  template <>
  struct CwalToNative<std::string const &>{
    typedef std::string Type;
    typedef Type ResultType;
    ResultType operator()( cwal_engine * e, cwal_value * v ) const{
      typedef CwalToNative<std::string> proxy;
      return proxy()(e, v);
    }
  };


  template <>
  struct CwalToNative<cwal_value>{
    typedef cwal_value Type;
    typedef Type * ResultType;
    ResultType operator()( cwal_engine *, cwal_value * v ) const{
      return v;
    }
  };
#if 0
  template <>
  struct CwalToNative<cwal_value *> : CwalToNative<cwal_value> {};

  template <>
  struct CwalToNative<cwal_value const> : CwalToNative<cwal_value> {};

  template <>
  struct CwalToNative<cwal_value const *> : CwalToNative<cwal_value> {};
#endif

  template <>
  struct NativeToCwal<cwal_value>{
    typedef cwal_value Type;
    typedef cwal_value * ArgType;
    cwal_value * operator()( cwal_engine *, ArgType n ) const{
      return n;
    }
  };


  /**
     A helper for cwal_native bindings. Classes with cwal_native
     bindings should create a CwalToNative<ThatType> which simply
     subclasses this class. The TypeID parameter is some arbitrary
     unique-per-type pointer. If Recursive is true and a (T*) is not
     found in a given value, its prototypes are searched, otherwise
     they are not.
  */
  template <typename T,
    bool Recursive = false,
    void const *&TypeID = TypeName<T>::TypeID
  >
  struct CwalToNative_NativeTypeID{
      typedef T Type;
      typedef T * ResultType;
      ResultType operator()( cwal_engine * e, cwal_value * v ) const;
  };

  /**
     Partial specialization for non-recursive lookups.
  */
  template <typename T,void const *&TypeID>
  struct CwalToNative_NativeTypeID<T, false, TypeID>{
    typedef T Type;
    typedef T * ResultType;
    ResultType operator()( cwal_engine *, cwal_value * v ) const{
      return (ResultType)cwal_native_get(cwal_value_get_native(v), TypeID);
    }
  };

  /**
     Partial specialization for recursive lookups.
  */
  template <typename T,void const *&TypeID>
  struct CwalToNative_NativeTypeID<T, true, TypeID>{
    typedef T Type;
    typedef T * ResultType;
    ResultType operator()( cwal_engine * e, cwal_value * v ) const{
      cwal_native * n = cwal_value_native_part(e, v, TypeID);
      return n ? reinterpret_cast<ResultType>( cwal_native_get(n, TypeID) ) : 0;
    }
  };

  template <typename T,void const *&TypeID, bool Recursive>
    struct CwalToNative_NativeTypeID<T const, Recursive, TypeID>
    : CwalToNative_NativeTypeID<T, Recursive, TypeID>{
  };

  template <typename T,void const *&TypeID, bool Recursive>
    struct CwalToNative_NativeTypeID<T const *, Recursive, TypeID>
    : CwalToNative_NativeTypeID<T, Recursive, TypeID>{
  };

  template <typename T,void const *&TypeID, bool Recursive>
    struct CwalToNative_NativeTypeID<T *, Recursive, TypeID>
    : CwalToNative_NativeTypeID<T, Recursive, TypeID>{
  };



  /**
     Intended as a cwal_finalizer_f() impl for Native
     bindings which use (new T) for their construction.
     This routine simply calls (delete (T*)m), which
     Must Not Throw.
  */
  template <typename T>
  void cwal_finalizer_f_delete(cwal_engine *, void * m){
    delete reinterpret_cast<T*>(m);
  }

  /**
     Intended as a cwal_finalizer_f() impl for Native bindings which
     use placement-new (new (mem) T()) for their construction and the
     (sizeof(T) bytes of) memory m came from cwal_malloc2().  This
     routine calls t->~T() (which Must Not Throw) then calls
     cwal_free2(e, m, sizeof(T)).
  */
  template <typename T>
  void cwal_finalizer_f_placed_dtor(cwal_engine * e, void * m){
    T * t = reinterpret_cast<T*>(m);
    t->~T();
    cwal_free2(e, m, sizeof(T));
  }

  /**
     If v is-a cwal_native, or inherits one, and that native has a
     (T*) bound to it with a type ID of TypeName<T>::TypeID, this
     function returns that object, else it returns 0.
   */
  template <typename T>
  typename CwalToNative<T>::ResultType
  getNativePart(cwal_engine * e, cwal_value * v){
    typedef typename CwalToNative<T>::ResultType RT;
    cwal_native * n = cwal_value_native_part(e, v, TypeName<T>::TypeID);
    return n 
      ? (RT)cwal_native_get(n, TypeName<T>::TypeID)
      : 0;
  }

  /**
     A generic "manual destructor" callback impl for arbitrary
     cwal_native-bound types. This function tries to extract a T value
     from args->self using getNativePart(). If it succeeds, it calls
     cwal_native_clear(), passing CallDtor as the second param.  On
     error (no native found) it triggers a cwal exception.
  */
  template <typename T, bool CallDtor>
  int cwal_callback_f_native_clear( cwal_callback_args const * args,
                                    cwal_value **rv ){
    typedef CwalToNative<T> proxy;
    typedef typename proxy::ResultType NT;
    cwal_native * nat = cwal_value_native_part(args->engine, args->self,
                                               TypeName<T>::TypeID);
    NT n = nat
      ? reinterpret_cast<NT>( cwal_native_get(nat, TypeName<T>::TypeID) )
      : 0;
    if(n){
      *rv = cwal_value_undefined();
      cwal_native_clear( nat, CallDtor );
      return 0;
    }else{
      return cwal_exception_setf(args->engine, CWAL_RC_TYPE,
                                 "Could not find %s pointer associated with 'this'.",
                                 TypeName<T>::Value);
    }
  }
  /**
     A callback argument conversion utility.
  */
  class Argv {
    cwal_callback_args const * args;
    public:
    uint16_t const argc;

    /**
       Pass it the callback's arguments. This routine
       will add a reference to each argument.
    */
    explicit Argv(cwal_callback_args const * args) throw()
      : args(args), argc(args->argc){
      for(uint16_t i = 0; i < this->argc; ++i ){
        cwal_value_ref(args->argv[i]);
      }
    }

    /**
       Calls cwal_value_unhand() on each of the arguments
       references in the ctor.
    */
    ~Argv() throw(){
      for(uint16_t i = 0; i < this->argc; ++i ){
        cwal_value_unhand(args->argv[i]);
      }
    }

    /**
       Returns the argument at the given index, or 0
       if out of bounds.
    */
    cwal_value * value(uint16_t i) const throw(){
      return argc>i ? args->argv[i] : 0;
    }

    char const * cstr(uint16_t i, cwal_size_t & len) const throw() {
      return argc>i ? cwal_value_get_cstr(args->argv[i], &len) : 0;
    }

    char const * cstr(uint16_t i) const throw() {
      return argc>i ? cwal_value_get_cstr(args->argv[i], 0) : 0;
    }


    /**
       Returns the argument at the given index, as returned
       by CwalToNative<T>. Because there is no general-purpose
       default return value, four out-of-bounds calls it
       passes 0 to CwalToNative's operator().
    */
    template <typename T> typename CwalToNative<T>::ResultType
    native(uint16_t i) const{
      typedef CwalToNative<T> proxy;
      return proxy()(args->engine, argc>i ? args->argv[i] : 0);
    }
  };


  template <typename Sig> struct Signature DOXYGEN_FWD_DECL_KLUDGE;

  /** @def CWAL_TYPELIST

      CWAL_TYPELIST is a (slightly) convenience form of
      Signature for creating typelists where we do not care about the
      "return type" or "context" parts of the list.

      It is used like this:

      @code
      typedef CWAL_TYPELIST(( int, double, char )) MyList;
      @endcode

      NOTE the doubled parenthesis! This makes passing other template
      types with commas in them legal.

      Many members of the API which take a type-list require a
      Signature-compatible typelist because they need to use the
      ResultType and/or Context typedefs. CWAL_TYPELIST is intended
      for cases where niether the ResultType nor Context play a role
      in the result (both typedefs are void for
      CWAL_TYPELIST-generated typelists).

      The maximum number of types the typelist can hold is limited
      to some build-time configuration option. If you run out, we
      can generate more.
  */
#define CWAL_TYPELIST(X) ::cwal::Signature< void X >

  /** \namespace cwal::sigli

      The sigli namespace exclusively holds template metafunctions for working
      with Signature-style typelists.
  */
  namespace sigli {

    /**
       Metafunction whose Value member evaluates to the length of the
       given Signature. The length is the number of parameters, not
       including the result type.
    */
    template < typename ListT >
    struct Length : tmp::IntVal<
      tmp::IsNil<typename ListT::Head>::Value
      ? 0 : (1 + Length<typename ListT::Tail>::Value)
    > {};

    //! End-of-list specialization.
    template <>
    struct Length<tmp::nil> : tmp::IntVal<0> {};
    /**
       Metafunction whose Type member evaluates to the type of the
       I'th argument type in the given Signature. Fails to compile
       if I is out of range.
    */
    template < unsigned short I, typename ListT >
    struct At : At<I-1, typename ListT::Tail>{
      typedef char AssertIndex[ (I >= Length<ListT>::Value) ? -1 : 1 ];
    };

    //! Beginning-of-list specialization.
    template < typename ListT >
    struct At<0, ListT>{
      typedef typename ListT::Head Type;
    };
    
#if 0
    /**
       End-of-list specialization. i don't think we need this, actually.
    */
    template <unsigned short I>
    struct At<I, tmp::nil> : tmp::Identity<tmp::nil>
    {};
#endif

    /**
       Metafunction whose Type Value member evaluates to the 0-based
       index of the first occurrance of the the type T in the 
       given Signature's argument list. Evaluates to -1 if T is not 
       contained in the argument list. Signature::ResultType and 
       Signature::Context are not evaluated.

       Clients _must not_ pass a value for the 3rd template parameter.
    */
    template < typename T, typename ListT, unsigned short Internal = 0 >
    struct Index : tmp::IntVal< tmp::SameType<T, typename ListT::Head>::Value
      ? Internal
      : Index<T, typename ListT::Tail, Internal+1>::Value
    >
    {
    };

    //! End-of-list specialization.
    template < typename T, unsigned short Internal >
    struct Index<T,tmp::nil,Internal> : tmp::IntVal<-1> {};
    
    /**
       Convenience form of Index<T,ListT> which evaluates to true if
       Index<T,ListT>::Value has non-negative value, else it evaluates
       to false.
    */
    template < typename T, typename ListT>
    struct Contains : tmp::BoolVal< Index<T,ListT>::Value >= 0  > {};
    

    /**
       A metatemplate which evaluates to true if SigT has exactly 2
       parameters and they matchi the cwal_callback_f() interface. The
       return type is not considered.
    */
    template <typename SigT>
    struct HasCallbackArgs{
      enum {
      Value = ((2==Length<SigT>::Value)
                 && (0==Index<cwal_callback_args const *,SigT>::Value)
                 && (1==Index<cwal_value **,SigT>::Value))
      };
    };

    /**
       Metatemplate which the 0-based parameter index of the the pair
       of parameter types (cwal_callback args const *, cwal_value**).
       Those two make up the parameter of the cwal_callback_f()
       interface.
    */
    template <typename SigT>
    struct CallbackArgsIndex{
      enum {
      Value = ((Length<SigT>::Value>1)
               && (Index<cwal_callback_args const *,SigT>::Value
                   == Index<cwal_value **,SigT>::Value - 1))
      ? Index<cwal_callback_args const *,SigT>::Value
      : -1
      };
    };


    /**
       A metatemplate which evaluates to true if SigT has the same
       arguments and return type as cwal_callback_f().
    */
    template <typename SigT>
    struct IsCallback{
      enum {
      Value = HasCallbackArgs<SigT>::Value
              && tmp::SameType<int,typename SigT::ResultType>::Value
      };
    };

    
    /**
       A metatemplate which calculates the number of arguments in the
       given typelist, but evaluates to -1 if SigT has 2 arguments
       matching the cwal_callback_f() arguments, as such function
       signatures are considered to be n-arity.
    */
    template <typename SigT>
    struct Arity{
      enum {
      Value = HasCallbackArgs<SigT>::Value
      ? -1
      : Length<SigT>::Value
      };
    };
    
    /**
       This metafunction evaluates to true if SigT appears to be
       "cwal_callback_f-like".
        
       We could implement this a number of different ways. The 
       current impl simply checks if the arity is -1.
    */
    template <typename SigT>
    struct IsCbLike : tmp::BoolVal< -1 == Arity<SigT>::Value > {};

    /**
       A metafunction which has a true Value if the Signature type SigT
       represents a non-member function.
    */
    template <typename SigT>
    struct IsFunction : tmp::BoolVal< tmp::SameType<void, typename SigT::Context>::Value > {};

    /**
       A metafunction which has a true Value if the Signature type SigT
       represents a member function (const or not).
    */
    template <typename SigT>
    struct IsMethod : tmp::BoolVal< !tmp::SameType<void, typename SigT::Context>::Value > {};

    /**
       A metafunction which has a true Value if the Signature type SigT
       represents a non-const member function.
    */
    template <typename SigT>
    struct IsNonConstMethod : tmp::BoolVal< !tmp::IsConst< typename SigT::Context >::Value && IsMethod<SigT>::Value > {};

    /**
       A metafunction which has a true Value if the Signature type SigT
       represents a const member function.
    */
    template <typename SigT>
    struct IsConstMethod :
      tmp::BoolVal< tmp::IsConst< typename SigT::Context >::Value && IsMethod<SigT>::Value > {};
    //tmp::BoolVal< SigT::IsConst && IsMethod<SigT>::Value > {};

    /**
       Metafunction which has a true value if the Signature type SigT
       returns void, else it has a false value.
    */
    template <typename SigT>
    struct ReturnsVoid :
      tmp::BoolVal< tmp::SameType<void, typename SigT::ResultType>::Value >{
    };

  }// namespace sigli

  namespace tmp{
    /**
       A metatemplate whose Type member resolves to IF if Cond is
       true, or ELSE if Cond is false. Its Value member evaluates
       to 1 or 0, accordingly.
    */
    template <bool Cond, typename IF, typename ELSE>
    struct IfElse : sigli::At< Cond ? 0 : 1, Signature<void (IF,ELSE)> >{
      //typedef typename sigli::At< Cond ? 0 : 1, Signature<void (IF,ELSE)> >::Type Type;
    };
  } // namespace tmp

  /**
     Specialization to give "InvacationCallback-like" functions
     an Arity value of -1.
  */
  template <typename RV>
  struct Signature<RV (cwal_callback_args const *, cwal_value **)>
  {
    typedef RV ResultType;
    typedef RV (*FunctionType)(cwal_callback_args const *, cwal_value **);
    typedef void Context;
    typedef cwal_callback_args const * Head;
    typedef Signature<RV (cwal_value **)> Tail;
  };

  template <typename RV>
  struct Signature<RV (*)(cwal_callback_args const *, cwal_value **)>
    : Signature<RV (cwal_callback_args const *, cwal_value **)>
  {};

  template <typename T, typename RV>
  struct Signature<RV (T::*)(cwal_callback_args const *, cwal_value **)>
    : Signature<RV (cwal_callback_args const *, cwal_value **)>{
    typedef T Context;
    typedef RV (Context::*FunctionType)(cwal_callback_args const *, cwal_value **);
  };


  /** @class FunctionSignature
      Base (unimplemented) signature for FunctionSignature
      specializations. The type passed to it must be a function
      signature.

      All implementations must define the interface described for 
      Signature and its Context typedef must be void for this type.

      Examples:

      @code
      // void func_foo():
      typedef FunctionSignature< void () > NoArgsReturnsVoid;

      // int func_foo(double):
      typedef FunctionSignature< int (double) > OneArgReturnsInt;

      // double func_foo(int,int):
      typedef FunctionSignature< double (int,int) > TwoArgsReturnsDouble;
      @endcode
   
  */
  template <typename FunctionSig>
  struct FunctionSignature : Signature< FunctionSig > {};

  /** @class MethodSignature
      Base (unimplemented) signature for MethodSignature
      specializations. The Sig type passed to it must match a member method
      signature of a function from the class T.
      e.g. (void (T::*)(int)) or its equivalent (void (int)).

      All implementations must have the interface called for by Signature
      and the Context typedef must be non-cvp-qualified T.

      Examples: 

      @code
      // void MyType::func():
      typedef MethodSignature< MyType, void () > NoArgsReturnsVoid;

      // int MyType::func(double):
      typedef MethodSignature< MyType, int (double) > OneArgReturnsInt;

      // double MyType::func(int,int):
      typedef MethodSignature< MyType, double (int,int) > TwoArgsReturnsDouble;
      @endcode

      MethodSignature<T const,...> and ConstMethodSignature<T,...> are equivalent.

      Reminders to self:

      i would really like this class to simply subclass Signature<Sig> and we
      would add in a couple typedefs we need. This would cut the specializations
      we generate. However, i don't know how to make this work.
  */
  template <typename T, typename Sig>
  struct MethodSignature DOXYGEN_FWD_DECL_KLUDGE;

  template <typename T, typename RV >
    struct MethodSignature< T, RV () > : Signature< RV () >
  {
    typedef T Context;
    typedef RV (Context::*FunctionType)();
  };


  template <typename T, typename RV >
    struct MethodSignature< T, RV (T::*)() > : MethodSignature<T, RV ()>
  {
  };

  template <typename T, typename RV >
    struct MethodSignature< T const, RV () > : Signature< RV (T::*)() const >
  {
    typedef T const Context;
    typedef RV (Context::*FunctionType)() const;
  };

  template <typename T, typename RV >
    struct MethodSignature< T const, RV (T::*)() > : MethodSignature<T const, RV ()>
  {
  };

  template <typename T, typename RV >
  struct MethodSignature< T const, RV (T::*)() const > : MethodSignature<T const, RV ()>
  {
  };

  /**
     A "type-rich" function pointer.

     Sig must be a function signature type usable in the construct
     FunctionSignature<Sig>. FuncPtr must be a function of that type.
  */
  template <typename Sig, typename FunctionSignature<Sig>::FunctionType FuncPtr>
    struct FunctionPtr : FunctionSignature<Sig>
  {
    /**
       This type's full "signature" type.
    */
    typedef FunctionSignature<Sig> SignatureType;
    /**
       The data type of FuncPtr.
    */
    typedef typename SignatureType::FunctionType FunctionType;
    
    /** The function specified in the template arguments. */
    static const FunctionType Function;

  };
  template <typename Sig, typename FunctionSignature<Sig>::FunctionType FuncPtr>
    typename FunctionPtr<Sig,FuncPtr>::FunctionType const FunctionPtr<Sig,FuncPtr>::Function = FuncPtr;

  /**
     Used like FunctionPtr, but in conjunction with member functions
     ("methods") of the T class. For const methods, T _must_ be
     const-qualified. See FunctionPtr for the requirements of the Sig
     type.
  */
  template <typename T, typename Sig, typename MethodSignature<T,Sig>::FunctionType FuncPtr>
    struct MethodPtr : MethodSignature<T,Sig>
  {
    typedef MethodSignature<T,Sig> SignatureType;
    typedef typename SignatureType::FunctionType FunctionType;
    static const FunctionType Function;
  };
  template <typename T, typename Sig, typename MethodSignature<T,Sig>::FunctionType FuncPtr>
    typename MethodPtr<T,Sig,FuncPtr>::FunctionType const MethodPtr<T,Sig,FuncPtr>::Function = FuncPtr;


  /**
     Counterpart for const methods.
  */
  template <typename T, typename Sig, typename MethodSignature<T const,Sig>::FunctionType FuncPtr>
    struct MethodPtr<T const, Sig, FuncPtr> : MethodSignature<T const,Sig>
  {
    typedef MethodSignature<T const,Sig> SignatureType;
    typedef typename SignatureType::FunctionType FunctionType;
    static const FunctionType Function;
  };
  template <typename T, typename Sig, typename MethodSignature<T const,Sig>::FunctionType FuncPtr>
    typename MethodPtr<T const,Sig,FuncPtr>::FunctionType const MethodPtr<T const,Sig,FuncPtr>::Function = FuncPtr;


  /**
     Marker class, for documentation purposes. Indicates classes which
     have a static callback() function implementing the
     cwal_callback_f() interface.
   */
  struct CallBackable{
    static int callback(cwal_callback_args const * args,
                        cwal_value ** rv);
  };

  //! Convenience typedef for a commonly-used case.
  typedef Signature< int (cwal_callback_args const * args,
                          cwal_value ** rv) > CallbackSignature;

  namespace Detail {
    /**
       Internal impl for callback-to-native free function forwarding.
       Each specialization handles Arity arguments. If IgnoreResult is true,
       specializations must not invoke (in the template sense) any
       conversion of the result value type, either because it is void
       or because it's of a type which is not convertible or should
       not be converted.

       Specializations must implement callback() as a template taking
       a FunctionPtr type argument.
    */
    template <int Arity, bool IgnoreResult>
      struct F2CbProxy : CallBackable {
      static int callback(cwal_callback_args const * args, cwal_value **rv);
    };

    /**
       A helper on top of a helper, this selects an F2CbProxy and forwards
       to it.
    */
    template <int Arity, bool IgnoreResult, typename FuncPtrT>
    struct F2CbHelper : CallBackable{
      static int callback(cwal_callback_args const * args, cwal_value **rv){
        typedef F2CbProxy<Arity,IgnoreResult> F2C;
        return F2C::template callback<FuncPtrT>(args, rv);
      }
    };

    template <>
    struct F2CbProxy<0, true> : CallBackable{
      template <typename FuncPtrT>
      static int callback(cwal_callback_args const *, cwal_value **rv){
        typedef tmp::Assertion< 0 == sigli::Arity<FuncPtrT>::Value > AssertArity;
        if(!AssertArity::Value){
          /* since when does gcc complain about unused local typedefs? */
        }
        FuncPtrT::Function();
        *rv = cwal_value_undefined();
        return 0;
      }
    };

    template <>
    struct F2CbProxy<0, false> : CallBackable{
      template <typename FuncPtrT>
      static int callback(cwal_callback_args const * args, cwal_value **rv){
        typedef tmp::Assertion< 0 == sigli::Arity<FuncPtrT>::Value > AssertArity;
        if(!AssertArity::Value){
          /* since when does gcc complain about unused local typedefs? */
        }
        typedef typename FuncPtrT::ResultType RT;
        *rv = toCwal<RT>( args->engine, (RT)FuncPtrT::Function() );
        return 0;
      }
    };

    /**
       Internal impl of callback-to-native member function forwarder
       template. Each specialization handles Arity arguments. If
       IgnoreResult is true, specializations must not invoke (in the
       template sense) any conversion of the result value type, either
       because it is void or because it's of a type which is not
       convertible or should not be converted.

       Specializations must implement callback() as a template taking
       a MemberPtr type argument.
    */
    template <uint16_t Arity, bool IgnoreResult>
    struct M2CbProxy : CallBackable{
      static int callback(cwal_callback_args const * args, cwal_value **rv);
    };

    template <>
    struct M2CbProxy<0, false> : CallBackable{
      template <typename MemberPtr>
      static int callback(cwal_callback_args const * args, cwal_value **rv){
        typedef tmp::Assertion< 0 == sigli::Arity<MemberPtr>::Value > AssertArity;
        if(!AssertArity::Value){}
        typedef typename MemberPtr::ResultType RT;
        typedef typename MemberPtr::Context T;
        T * that = toNative<T>(args->engine, args->self);
        if(!that){
          return tossMissingThis<T>(args);
        }else{
          *rv = toCwal<RT>( args->engine, (RT)(that->*MemberPtr::Function)() );
          return 0;
        }
      }
    };

    template <>
    struct M2CbProxy<0, true> : CallBackable{
      template <typename MemberPtr>
      static int callback(cwal_callback_args const * args, cwal_value **rv){
        //typedef char AssertArity[ 0 == sigli::Arity<MemberPtr>::Value ? -1 : 1 ];
        typedef tmp::Assertion< 0 == sigli::Arity<MemberPtr>::Value > AssertArity;
        if(!AssertArity::Value){}
        typedef typename MemberPtr::ResultType RT;
        typedef typename MemberPtr::Context T;
        T * that = toNative<T>(args->engine, args->self);
        if(!that){
          return tossMissingThis<T>(args);
        }else{
          (RT)(that->*MemberPtr::Function)();
          *rv = cwal_value_undefined();
          return 0;
        }
      }
    };

    /**
       Template to select either a Function or Method callback proxy.
       IsFunc is true for non-member Functions and false for methods.
    */
    template <typename FMPtr,
      bool IgnoreResult = sigli::ReturnsVoid<FMPtr>::Value,
      bool IsFunc = !sigli::IsMethod<FMPtr>::Value
    >
    struct SelectFuncOrMethod : CallBackable{};

    template <typename FMPtr,
      bool IgnoreResult = sigli::ReturnsVoid<FMPtr>::Value,
      bool IsNativeCallback = sigli::IsCallback<FMPtr>::Value
    >
    struct SelectFuncOrCallback : CallBackable{};

    //! Specialization for native cwal_callback_f(). It ignores IgnoreResult.
    template <typename FMPtr, bool IgnoreResult /*ignored!*/ >
    struct SelectFuncOrCallback<FMPtr, IgnoreResult, true> : CallBackable{
      static int callback(cwal_callback_args const *args, cwal_value **rv){
        return FMPtr::callback(args, rv);
      }
    };

    //! Specialization for "converted" Functions.
    template <typename FMPtr, bool IgnoreResult>
    struct SelectFuncOrCallback<FMPtr, IgnoreResult, false> : CallBackable{
      static int callback(cwal_callback_args const *args, cwal_value **rv){
        typedef F2CbHelper<
          sigli::Arity<FMPtr>::Value,
          IgnoreResult,
          FMPtr
        > proxy;
        return proxy::callback(args, rv);
      }
    };

    //! Specialization for Functions.
    template <typename FMPtr, bool IgnoreResult>
    struct SelectFuncOrMethod<FMPtr, IgnoreResult, true> : CallBackable{
      static int callback(cwal_callback_args const *args, cwal_value **rv){
        typedef SelectFuncOrCallback< FMPtr, IgnoreResult > proxy;
        return proxy::callback(args, rv);
      }
    };

    //! Specialization for Methods.
    template <typename FMPtr, bool IgnoreResult>
      struct SelectFuncOrMethod<FMPtr, IgnoreResult, false> : CallBackable{
      static int callback(cwal_callback_args const *args, cwal_value **rv){
        typedef M2CbProxy<
          sigli::Arity<FMPtr>::Value,
          IgnoreResult
        > proxy;
        return proxy::template callback<FMPtr>(args, rv);
      }
    };

    /**
       The idea is to convert C functions with a signature like:

       ReturnType func( T *[ , ... argN] );

       into script-side methods.

       This template's job is to extract (T*) from args->self, then
       passes that on as the first argument to the
       FuncPtr::Function. It requires:

       a) FuncPtr to be-a FunctionPtr template type (or compatible).

       b) that the first argument type is a pointer type. This also
       implies a requirement of an arity of 1 or higher. There is
       no specialization for 0-arity variants, as those do not
       fit this conversion's model.

       c) a conversion to that pointer type is possible

       Internal impl of callback-to-native pseudo-member function
       forwarder template. Each specialization handles Arity
       arguments. If IgnoreResult is true, specializations must not invoke
       (in the template sense) any conversion of the result value
       type, either because it is void or because it's of a type which
       is not convertible or should not be converted.

       Specializations must implement callback() as a template taking
       a MethodPtr type argument. There must not be a specialization
       for 0 arity, as those functions do not fit this model.

       All default specializations are generated.
    */
    template <uint16_t Arity, bool IgnoreResult, typename FuncPtrT>
    struct QuasiMethodProxy : CallBackable{
      static int callback(cwal_callback_args const * args, cwal_value **rv);
    };

    template <bool IgnoreResult, typename FuncPtrT>
    struct QuasiMethodProxy<0, IgnoreResult, FuncPtrT>{
      /* There is no usable 0-arity specialization of this template. */
    };
  } /* namespace Detail */


  /**
     A cwal_callback_f() proxy impl which calls Callback() and translates
     any exceptions it throws to cwal exceptions. It is fatal to cwal
     for exceptions to pass through its API, thus any C++ exceptions
     from callbacks _must_ be caught somewhere.
  */
  template <cwal_callback_f Callback>
  struct CbCatcher : CallBackable {
    //! These typedefs assist in recursive conversion.
    typedef CallbackSignature SignatureType;
    typedef int ResultType;
    typedef void Context;
    typedef SignatureType::Head Head;
    typedef SignatureType::Tail Tail;
    static  int callback(cwal_callback_args const * args,
                         cwal_value **rv){
      try{
        return Callback(args, rv);
      }catch(std::bad_alloc const &){
        return CWAL_RC_OOM;
      }catch(std::exception const & ex){
        return cwal_exception_setf(args->engine,
                                   CWAL_RC_EXCEPTION,
                                   "Caught std::exception: %s", ex.what());
      }catch(...){
        return cwal_exception_setf(args->engine,
                                   CWAL_RC_EXCEPTION,
                                   "Caught unknown C++ exception");
      }
    }
  };

  /**
     A template to convert "near-arbitrary" C/C++ function pointers
     (including class members) to cwal_callback_f() implementations.

	 FMPtr must be a FunctionPtr, MethodPtr, or QuasiMethod 
	 instantiation.  All argument types must be convertible to 
	 cwal_value using NativeToCwal<TheArgType>. The return type must 
	 be convertible using CwalToNative<TheReturnType> or be void. 
	 For const methods, FMPtr must be a MethodPtr type and 
	 FMPtr::Context must be const-qualified.

     If CatchExceptions is true then CbCatcher is used to catch
     exceptions thrown by the function, otherwise exceptions are
     propagated. It is on by default for methods. It is off by default
     for non-methods because it is expected that most non-member
     functions bound this way are C APIs which will never throw. If
     there is any chance at all that a bound method/function will
     throw or propagate an exception then CatchExceptions _MUST_ be
     true or Undefined Behaviour ensues. When in doubt, enable the
     CatchExceptions option - all it costs is a try/catch block and it
     will protect against Undefined Behaviour if the function throws
     or propagates.

     If IgnoreResult is true then the return value is treated as if
     the function had returned void, so no conversion of its type is
     invoked. This is intended for functions returning void and those
     for which we have no (or want no) native-to-cwal conversion for
     the result type.

     For methods, the generated callback expects args->self (the
     "this" of a script callback) to be convertible to (T*) using
     toNative<T>(args->self). If that returns 0 then those bindings
     will throw an exception. Such a conversion can be installed
     by specializing CwalToNative<T>.

     Examples:

     @code
     cwal_callback_f freeFunc = ToCb< FunctionPtr<unsigned (unsigned), ::sleep> >::callback;
     cwal_callback_f nonConst = ToCb< MethodPtr<T, int(), &T::foo> >::callback;
     cwal_callback_f isConst = ToCb< MethodPtr<T const, int(), &T::bar> >::callback;
     @endcode

  */
  template <typename FMPtr,
    bool CatchExceptions = sigli::IsMethod<FMPtr>::Value,
    bool IgnoreResult = sigli::ReturnsVoid<FMPtr>::Value
  >
  struct ToCb : CallBackable{
    //! These typedefs allow ToCb< ToCb<...> > to DTRT.
    typedef CallbackSignature SignatureType;
    typedef int ResultType;
    typedef void Context;
    typedef SignatureType::Head Head;
    typedef SignatureType::Tail Tail;
    static int callback(cwal_callback_args const *args, cwal_value **rv){
      typedef Detail::SelectFuncOrMethod<FMPtr,IgnoreResult> Selector;
      typedef typename tmp::IfElse<CatchExceptions,
        CbCatcher< Selector::callback >,
        Selector
      > ::Type proxy2;
      return proxy2::callback(args, rv);
    }
  };

  /**
     Creates a new cwal_function wrapping the function or method
     described by FMPtr, which must be a FunctionPtr or MethodPtr
     instantiation, or a type compatible with that interface (the
     Signature typename interface plus the FunctionPtr::Function
     member).

     The (e, state, stateDtor, stateTypeID) parameters are as
     described for cwal_new_function().

     The returned function will be set up to catch any C++ exceptions
     which FMPtr::Function throws and convert them to cwal-level
     exceptions, but this function itself throws std::bad_alloc
     if allocation fails.
  */
  template <typename FMPtr>
  cwal_function * newFunction(cwal_engine * e,
                              void * state = 0,
                              cwal_finalizer_f stateDtor = 0,
                              void const * stateTypeID = 0){
    typedef ToCb<FMPtr, 1> proxy;
    cwal_function * rc = cwal_new_function(e, proxy::callback,
                                           state, stateDtor,
                                           stateTypeID);
    if(!rc) throw std::bad_alloc();
    else return rc;
  }

  /**
     Equivalent to cwal_function_value( newFunction<FMPtr>(...) ).
  */
  template <typename FMPtr>
  cwal_value * newFunctionValue(cwal_engine * e,
                                void * state = 0,
                                cwal_finalizer_f stateDtor = 0,
                                void const * stateTypeID = 0){
    return cwal_function_value(newFunction<FMPtr>(e, state, stateDtor, stateTypeID));
  }


  /**
     Marker class, for documentation purposes.
  */
  struct ValuePredicate{
    /**
       Must evaluate the handle and return true or false.

       Unfortunately, its parameter cannot be const because some
       specializations may need non-const access for their
       conversions.
    */
    bool operator()( cwal_value * v ) const;
  };

  /**
     Functor which evaluates the boolean value of a value.
  */
  template <bool Truthness>
  struct ValIsBooly : ValuePredicate {
    /**
       If Truthness is true this function returns
       cwal_value_get_bool(v), else it returns the inverse of that.
     */
    bool operator()(cwal_value * v) const{
      return Truthness
        ? (v ? cwal_value_get_bool(v) : false)
        : (v ? !cwal_value_get_bool(v) : true);
    }
  };

  /**
     A functor which returns true if its given value
     is true.
  */
  struct ValIsTrue : ValIsBooly<true>{};
  /**
     A functor which returns true if its given value is false.
  */
  struct ValIsFalse : ValIsBooly<false>{};


  /**
     Base template for functors which answer the question "is
     cwal_value X is of type T?".
  */
  template <typename T>
  struct ValIsA : ValuePredicate {
    bool operator()(cwal_value /*const*/ * v) const{
      return 0 != toNative<T>(cwal_value_engine(v), v);
    }
  };

  template <typename T>
    struct ValIsA<T const> : ValIsA<T> {};
  template <typename T>
    struct ValIsA<T *> : ValIsA<T> {};
  template <typename T>
    struct ValIsA<T const *> : ValIsA<T> {};
  template <typename T>
    struct ValIsA<T &> : ValIsA<T> {};
  template <typename T>
    struct ValIsA<T const &> : ValIsA<T> {};

  namespace Detail {
    /**
       An internal helper for ValIsA templates which map directly to
       an existing predicate function.
    */
    template <bool (*Predicate)(cwal_value const *)>
    struct ValIsA_predicate{
      bool operator()(cwal_value const * v) const{
        return Predicate(v);
      }
    };
  }

  template <>
  struct ValIsA<int16_t> : Detail::ValIsA_predicate<cwal_value_is_integer>{};
  template <>
  struct ValIsA<int32_t> : Detail::ValIsA_predicate<cwal_value_is_integer>{};
  template <>
  struct ValIsA<int64_t> : Detail::ValIsA_predicate<cwal_value_is_integer>{};
  template <>
  struct ValIsA<uint16_t> : Detail::ValIsA_predicate<cwal_value_is_integer>{};
  template <>
  struct ValIsA<uint32_t> : Detail::ValIsA_predicate<cwal_value_is_integer>{};
  template <>
  struct ValIsA<uint64_t> : Detail::ValIsA_predicate<cwal_value_is_integer>{};
  template <>
  struct ValIsA<double> : Detail::ValIsA_predicate<cwal_value_is_double>{};
  template <>
  struct ValIsA<float> : Detail::ValIsA_predicate<cwal_value_is_double>{};
  template <>
  struct ValIsA<char const *> : Detail::ValIsA_predicate<cwal_value_is_string>{};
  template <>
  struct ValIsA<cwal_buffer> : Detail::ValIsA_predicate<cwal_value_is_buffer>{};
  template <>
  struct ValIsA<cwal_native> : Detail::ValIsA_predicate<cwal_value_is_native>{};
  template <>
  struct ValIsA<cwal_array> : Detail::ValIsA_predicate<cwal_value_is_array>{};
  template <>
  struct ValIsA<cwal_tuple> : Detail::ValIsA_predicate<cwal_value_is_tuple>{};
  template <>
  struct ValIsA<cwal_object> : Detail::ValIsA_predicate<cwal_value_is_object>{};
  template <>
  struct ValIsA<cwal_function> : Detail::ValIsA_predicate<cwal_value_is_function>{};
  template <>
  struct ValIsA<bool> : Detail::ValIsA_predicate<cwal_value_is_bool>{};

  /**
     A functor which evaluates to true if its given value is
     cwal_value_undefined(), else it evaluates to false.
  */
  template <>
  struct ValIsA<void> : ValuePredicate {
    bool operator()(cwal_value * v) const{
      return !v || cwal_value_undefined()==v;
    }
  };

  template <>
  struct ValIsA<cwal_value> : ValuePredicate {
    bool operator()(cwal_value *) const{
      return true;
    }
  };


  /**
     For use with extractNative().

     Maintenance reminder: the values used here are magical (see
     extractNative()).
  */
  enum extractNativeErrorPolicy {
  /**
     Specifies that a native C++ exception should be thrown
     if no cwal_native can be extracted.
  */
  EXTRACT_ERR_THROW_NATIVE = -1,
  /**
     Specifies that NULL should be returned if no cwal_native can be
     extracted.
  */
  EXTRACT_ERR_IGNORE = 0,
  /**
     Specifies that a cwal_engine-level exception should be thrown,
     then NULL returned, if no cwal_native can be extracted.
  */
  EXTRACT_ERR_THROW_CWAL = 1
  };

  /**
     Like toNative<T>(), but:

     A) expects to be returning a pointer type.

     B) How it handles conversion failure depends on errorPolicy:
     0 means no error reporting (returns 0 on error), less than 0
     means to throw a std::exception if the conversion fails, greater
     thant 0 means to trigger a cwal-level exception (and return 0).

     Example usage in the context of a cwal_callback_f() impl:

     @code
     MyT * my = extractNative(e, args->self, EXTRACT_ERR_THROW_CWAL);
     if(!my) return CWAL_RC_EXCEPTION; // e contains exception state
     @endcode
  */
  template <typename T>
  typename CwalToNative<T>::ResultType
  extractNative( cwal_engine * e, cwal_value * v,
                 extractNativeErrorPolicy errorPolicy = EXTRACT_ERR_IGNORE ){
    typedef CwalToNative<T> proxy;
    typename CwalToNative<T>::ResultType rv;
    if(!(rv = proxy(e, v)) && errorPolicy){
      std::string const & str =
        std::string("Value is not (or is no longer) a ")
        + TypeName<T>::Value
        + " pointer.";
      if(errorPolicy<0){
        throw std::bad_cast(str.c_str());
      }else{
        cwal_exception_setf(e, CWAL_RC_TYPE, "%.*s",
                            (int)str.size(), str.c_str());
      }
    }
    return rv;
  }

  /**
     A functor which fetches an argument by index.
        
     I = the argument index to fetch.
        
     The class is intended mainly to be invoked via code paths 
     selected by template metaprograms, thus the hard-coding of 
     the argument parameter index at compile-time.
  */
  template <int I>
  struct ArgAt{
    /**
       Returns argv[I] if args->argc is > I, else 0
       is returned.
    */
    inline cwal_value * operator()( cwal_callback_args const * args ) const{
      typedef tmp::Assertion< I>=0 > AssertIndex;
      if(!AssertIndex::Value){}
      return (args->argc > I) ? args->argv[I] : 0;
    }
  };

  /**
     Functor to fetch an argument and return its result
     as a native value.
        
     I = the argument index to fetch.
        
     T = the native type to convert to. CwalToNative<T> must be legal.
  */
  template <int I, typename T>
  struct ArgAtCast{
    typedef CwalToNative<T> C2N;
    typedef typename C2N::ResultType ResultType;
    /**
       Returns CwalToNative<T>()( args->engine, ArtAt<I>()(argv) ).
    */
    inline ResultType operator()( cwal_callback_args const * args ) const{
      typedef tmp::Assertion< I>=0 > AssertIndex;
      if(0 == AssertIndex::Value){}
      typedef ArgAt<I> Proxy;
      return C2N()( args->engine, Proxy()(args) );
    }
  };

  /**
     Marker class, mainly for documentation purposes.
        
     Classes matching this concept "evaluate" a cwal_callback_args
     object for validity without actually performing any "application
     logic." These are intended to be used as functors, primarily
     triggered via code paths selected by template metaprograms.

     They must be default-construcable and must have no
     private state. Their public API consists of only operator().

     This Concept's operator() is intended only to be used for 
     decision-making purposes ("are there enough arguments?" or 
     "are the arguments of the proper types?"), and not 
     higher-level application logic.
  */
  struct ArgumentsPredicate{
    /**
       Must "evaluate" the arguments and return true or false. 
    */
    bool operator()( cwal_callback_args const * args ) const;
  };



  template <bool Truthness>
  struct Argv_AbsoluteBooly : ArgumentsPredicate {
    bool operator()(cwal_callback_args const *) const{
      return Truthness;
    }
  };

  struct Argv_True : Argv_AbsoluteBooly<true>{};
  struct Argv_False : Argv_AbsoluteBooly<false>{};

  /**
     The "or" equivalent of Argv_And.

     Use Argv_OrN for chaining more than two predicates.
  */
  template <typename ArgPred1, typename ArgPred2>
  struct Argv_Or : ArgumentsPredicate{
    /**
       Returns true only if one of ArgPred1()(args) or
       ArgPred2()(args) return true.
    */
    inline bool operator()( cwal_callback_args const * args ) const{
      return ArgPred1()( args ) || ArgPred2()( args );
    }
  };

  /**
     This ArgumentsPredicate implementation combines a list of
     other ArgumentsPredicates using an AND operation on the combined
     results of each functor.

     PredList must be a type-list containing ArgumentsPredicate types.
     This functor is a predicate which performs an AND operation on all
     of the predicates in the type list.

     Note that an empty typelist evaluates to True in this context, for
     deeply arcane reasons.

     See Argv_And if you just want to combine two functors
     (it is more succinct for that case).

     Example:

     @code
     // Predicate matching (function, ANYTHING, function) signature:
     typedef Argv_AndN< CWAL_TYPELIST((
     Argv_Length<3>,
     ArgAt_IsFunction<0>,
     ArgAt_IsFunction<2>
     )) > PredFuncXFunc;
     @endcode
  */
  template <typename PredList>
  struct Argv_AndN : ArgumentsPredicate{
    /**
       Returns true only if all predicates in PredList
       return true when passed the args object.
    */
    inline bool operator()( cwal_callback_args const * args ) const{
      typedef typename PredList::Head Head;
      typedef typename tmp::IfElse< tmp::SameType<tmp::nil,Head>::Value,
        Argv_True,
        Head>::Type P1;
      typedef typename PredList::Tail Tail;
      typedef Argv_AndN<Tail> P2;
      return P1()( args ) && P2()(args);
    }
  };
  
  //! End-of-list specialization.
  template <>
  struct Argv_AndN<tmp::nil> : Argv_True {};

  /**
     The "or" equivalent of Argv_AndN.

     When chaining only two predicates Argv_Or offers
     a more succinct equivalent to this type.

     Note that an empty typelist evaluates to False in this context, for
     deeply arcane reasons.
  */
  template <typename PredList>
  struct Argv_OrN : ArgumentsPredicate{
    /**
       Returns true only if one of the predicates in PredList
       returns true when passed the args object.
    */
    inline bool operator()( cwal_callback_args const * args ) const{
      typedef typename PredList::Head Head;
      typedef typename tmp::IfElse< tmp::SameType<tmp::NilType,Head>::Value,
        Argv_False,
        Head>::Type P1;
      typedef typename PredList::Tail Tail;
      typedef Argv_OrN<Tail> P2;
      /* This suboptimal formulation is to please gcc 4.4.7... */
      bool const lhs = P1()( args );
      return lhs ? lhs : P2()(args);
    }
  };

  //! End-of-list specialization.
  template <>
  struct Argv_OrN<tmp::NilType> : Argv_False {};


  /**
     Functor to evaluate whether an Arguments list
     has a certain range of argument count.
        
     Min is the minimum number. Max is the maximum. The range is 
     inclusive. Use (Max<Min) to mean (at least MIN). Use 
     (Min==Max) to mean only that many arguments.
  */
  template <uint16_t Min_, uint16_t Max_ = Min_>
  struct Argv_Length : ArgumentsPredicate{
    private:
    enum { Min = Min_,
           Max = Max_
    };
    public:
    /**
       Returns true if av meets the argument count
       requirements defined by the Min and Max
       values.
    */
    bool operator()( cwal_callback_args const * args ) const{
      bool const kludge = (Max==Min) ? false : (Max < Min) /* gcc 4.4.7 workaround */;
      int const argc = (int)args->argc;
      return kludge /* the int casts are to work around gcc 4.4.7 */
        ? argc >=(int)Min
        : (argc>=(int)Min) && (argc<=(int)Max);
    }
  };

  /**
     Arguments predicate functor.
    
     Index = arg index to check.
        
     ValIsType must match the ValuePredicate interface.
  */
  template <uint16_t Index, typename ValIsType >
  struct ArgAt_Is : ArgumentsPredicate{
    /**
       Returns true if ValType()( av[Index] ) is true.
    */
    inline bool operator()( cwal_callback_args const * args ) const{
      return (Index >= args->argc)
        ? false
        : ValIsType()( args->argv[Index] );
    }
  };

  template <uint16_t Index, bool Truthness>
  struct ArgAt_Booly : ArgumentsPredicate {
    bool operator()(cwal_callback_args const * args) const{
      cwal_value const * v = ArgAt<Index>()(args);
      return v
        ? (Truthness ? cwal_value_get_bool(v) : !cwal_value_get_bool(v))
        : false;
    }
  };

  template <uint16_t Index>
  struct ArgAt_IsTruthy : ArgAt_Booly<Index, true>{};
  template <uint16_t Index>
  struct ArgAt_IsFalsy : ArgAt_Booly<Index, false>{};

  /**
     An ArgumentsPredicate implementation which takes
     two ArgumentsPredicate functors as template parameters
     and combines them using an AND operation.

     See Argv_AndN if you just want to combine more than two functors.
     (Argv_AndN can also be used for two functors but is more verbose
     than this form for that case.)
  */
  template <typename ArgPred1, typename ArgPred2>
  struct Argv_And : ArgumentsPredicate{
    /**
       Returns true only if ArgPred1()(args) and
       ArgPred2()(args) both return true.
    */
    inline bool operator()( cwal_callback_args const * args ) const{
      return ArgPred1()( args ) && ArgPred2()( args );
    }
  };
  /**
     Arguments predicate functor.
    
     Index = arg index to check.
        
     T is a type for which ValIsA<T> is legal. The functor returns
     true if ValIs<T> returns true for the argument at the given
     index.
  */
  template <uint16_t Index, typename T>
  struct ArgAt_IsA : ArgAt_Is< Index, ValIsA<T> > {};

  template <uint16_t Index>
  struct ArgAt_IsArray : ArgAt_IsA<Index, cwal_array>{};

  template <uint16_t Index>
  struct ArgAt_IsObject : ArgAt_IsA<Index, cwal_object>{};

  template <uint16_t Index>
  struct ArgAt_IsFunction : ArgAt_IsA<Index, cwal_object>{};

  template <uint16_t Index>
  struct ArgAt_IsInt : ArgumentsPredicate{
    inline bool operator()( cwal_callback_args const * args ) const{
      return args->argc>Index ? cwal_value_is_integer(args->argv[Index]) : false;
    }
  };

  template <uint16_t Index>
  struct ArgAt_IsDouble : ArgumentsPredicate{
    inline bool operator()( cwal_callback_args const * args ) const{
      return args->argc>Index ? cwal_value_is_double(args->argv[Index]) : false;
    }
  };

  template <uint16_t Index>
  struct ArgAt_IsNumber : Argv_Or< ArgAt_IsInt<Index>, ArgAt_IsDouble<Index> >{};

  namespace Detail {
    /**
       An internal level of indirection for overloading-related
       dispatchers.
    */
    template <typename CbT>
    struct OverloadCallHelper : CallBackable {
      inline static int callback( cwal_callback_args const * args,
                                  cwal_value ** rv){
        return CbT::callback(args, rv);
      }
    };
    //! End-of-list specialization.
    template <>
    struct OverloadCallHelper<tmp::NilType> : CallBackable{
      inline static int callback( cwal_callback_args const * args,
                                  cwal_value **){
        return cwal_exception_setf(args->engine, CWAL_RC_RANGE,
                                   "End of type-list reached at "
                                   "position %d.", args->argc);
      }
    };
  }// namespace Detail


  /**
     Intended as a base class for a couple other
     PredicatedXyz types.
        
     This type combines ArgumentsPredicate ArgPred and
     CallT::callback() into one type, for template-based dispatching
     purposes.
        
     ArgPred must be-a ArgumentsPredicate. CallT must be a CallBackable-like type,
     take cwal_callback_f() arguments and returning any type.
        
     This type uses subclassing instead of composition
     to avoid having to specify the CallT::callback() return
     type as an additional template parameter. i don't seem to 
     be able to template-calculate it from here. We can't use
     CallT::ResultType because for most bindings that value
     is different from its callback() return type. We will possibly
     eventually run into a problem with the lack of a way
     to get callback()'s return type.

     @see CbDispatcher
  */
  template <typename ArgPred, typename CallT>
  struct CbRuleLike : ArgPred, CallT {
    typedef CallbackSignature SignatureType;
    typedef int ResultType;
    typedef void Context;
    typedef SignatureType::Head Head;
    typedef SignatureType::Tail Tail;
  };

  /**
     CbRule combines an ArgumentsPredicate functor (ArgPred)
     with an CallBackable type (CbT), such that we can create lists of
     functor/callback pairs for use in dispatching callbacks based on
     the results of ArgumentPredicates.

     CbT must implement the CallBackable interface.

     This type is primarily intended to be used together with
     CbDispatcher.
        
     @see CbRuleLike
     @see CbDispatcher
  */
  template <typename ArgPred, typename CbT>
  struct CbRule : CbRuleLike<ArgPred, CbT> {};

  /**
     A structified form of cwal_callback_f().  It is sometimes
     convenient to be able to use a typedef to create an alias for a
     given callback. Since we cannot typedef function templates this
     way, this class can fill that gap.
  */
  template <cwal_callback_f CB>
  struct CbToCb : ToCb<
    FunctionPtr<int (cwal_callback_args const *,cwal_value **), CB>
  >{
    typedef FunctionSignature<int (cwal_callback_args const *,cwal_value **)> SignatureType;
    typedef typename SignatureType::FunctionType FunctionType;
    typedef void Context;
    typedef int ResultType;
    typedef cwal_callback_args const * Head;
    typedef Signature<int (cwal_value **)> Tail;
    static int callback(cwal_callback_args const * args, cwal_value **rv){
      return CB(args, rv);
    }
  };


  /**
     A template for converting "quasi-methods" (or "C-like methods")
     to cwal callback functions.

     FuncPtrT must be a FunctionPtr instantiation. The
     CatchExceptions and IgnoreResult template parameters work as
     described for ToCb.

     This type is intended to simplify certain bindings to C APIs
     which take a "self" object of some type as their first
     parameter.

     A "quasi-method," for this purpose, is a non-member (or static
     member) function:

     A) which takes a T or const T pointer or reference as its first
     (possibly only) argument. Implicit in this requirement is that
     the function have an arity of 1 or more.

     B) for which toNative<T>(args->self) can extract a (T*). i.e.
     the (T*) must somehow be mapped to cwal, such that
     CwalToNative<T> can work. 'args' is the cwal_callback_args passed
     to this type's generated callback function.

     The T value is passed as the first arugument to the callback and
     all arguments to the script-side call are passed as the second
     and subsequent parameters to the native call. Extra parameters
     are ignored, and too few might or might not trigger an error,
     depending on the conversions being attempted. One can use
     CbDispatcher to set up rules which enforce a certain parameter
     count and/or types, rather than relying on default conversions
     (if any) for any missing parameters.

     This type may be used as a template parameter to ToCb, but it can
     also be used on its own.

     Example legal functions for this purpose:

     @code
     int MyType_non_member( MyType * my, int value );
     int mylib_foo( mylib_context * my, char const * key, double val );
     @endcode


     Reminder to self, since this puzzled you for a while: the reason
     this type works with ToCb< QuasiMethod<...> > is because ToCb's
     internals end up picking this up as a CallBackable, as opposed to
     a FunctionPtr. That's a side-effect of the various
     Signature-related typedefs in this class.
  */
  template <typename FuncPtrT,
    bool CatchExceptions = true,
    bool IgnoreResult = sigli::ReturnsVoid<FuncPtrT>::Value
  >
  struct QuasiMethod : CallBackable{
    typedef CallbackSignature SignatureType;
    typedef int ResultType;
    typedef void Context;
    typedef SignatureType::Head Head;
    typedef SignatureType::Tail Tail;
    static int callback(cwal_callback_args const *args, cwal_value **rv){
      enum {Arity = sigli::Arity<FuncPtrT>::Value};
      typedef Detail::QuasiMethodProxy<Arity, IgnoreResult, FuncPtrT> proxy;
      typedef typename tmp::IfElse<CatchExceptions,
        CbCatcher< proxy::callback >,
        proxy
      > ::Type proxy2;
      return proxy2::callback(args, rv);
    }
  };


  /**
     This class creates a cwal_callback_f() implementation which
     dispatches to one of an arbitrarily large set of other
     cwal_callback_f, as determined by predicate rules which inspect
     the argument metadata.

     PredList must be a type-list (e.g. Signature) of CbRule
     implementations. See callback() for more details.

     Basic example:
     
     @code
     // Overloads for 1-3 arguments:
     typedef CbRule< Argv_Length<1>, ToCb<...> > Cb1;
     typedef CbRule< Argv_Length<2>, ToCb<...> > Cb2;
     typedef CbRule< Argv_Length<3>, ToCb<...> > Cb3;
     // Fallback impl for 0 or 4+ args:
     typedef CbRule< Argv_True, CbToCb<my_callback> > CbN;
     // Side note: this ^^^^^^^^^^^^^^ is the only known use for the
     // Argv_True predicate.
     
     // Combine them into one cwal_callback_f:
     typedef CbDispatcher< CWAL_TYPELIST((
         Cb1, Cb2, Cb3, CbN
     ))> AllOverloads;
     cwal_callback_f cb = AllOverloads::callback;
     @endcode
  */
  template <typename PredList>
  struct CbDispatcher
        : CallBackable{
    //! For compatibility with various ToCb bits.
    //typedef FunctionSignature<int (cwal_callback_args const *,cwal_value **)> SignatureType;
    //typedef typename SignatureType::FunctionType FunctionType;

    typedef CallbackSignature SignatureType;
    typedef typename SignatureType::FunctionType FunctionType;
    typedef typename SignatureType::Context Context;
    typedef typename SignatureType::ResultType ResultType;
    typedef SignatureType::Head Head;
    typedef SignatureType::Tail Tail;

    //typedef cwal_callback_args const * Head;
    //typedef Signature< int (cwal_value **) > Tail;
#if 0
    static const cwal_callback_f Function;
#elif 0
    static int Function( cwal_callback_args const * args,
                         cwal_value **rv ){
      return callback(args,rv);
    }
#endif

    /**
       For each CbRule type P in PredList, if P()(argv) returns true
       then P::callback(argv, rv) is returned, else the next predicate in
       the list is tried.
       
       If no predicates match then a cwal-level exception will be
       triggered.
    */
    static int callback( cwal_callback_args const * args,
                         cwal_value **rv ){
      typedef typename PredList::Head Head;
      typedef typename tmp::IfElse< tmp::SameType<tmp::NilType,Head>::Value,
        Argv_False,
        Head>::Type Predicate;
      typedef typename PredList::Tail Tail;
      typedef tmp::Assertion<
        tmp::SameType<tmp::NilType,Head>::Value
        ? (tmp::SameType<tmp::NilType,Tail>::Value ? 1 : 0)
        : 1
        > AssertEndOfListCheck
        /* verifies that the Argv_False bit does not call any
           specialization other than the NilType one.
        */;
      if(!AssertEndOfListCheck::Value){/*"unused" local assertion typedef*/}
      return ( Predicate()( args ) )
        ? Detail::OverloadCallHelper<Head>::callback( args, rv )
        : CbDispatcher<Tail>::callback( args, rv );
    }
  };

  //! End-of-list specialization.
  template <>
  struct CbDispatcher<tmp::NilType>
    : Detail::OverloadCallHelper<tmp::NilType>{
    typedef CallbackSignature SignatureType;
    typedef SignatureType::FunctionType FunctionType;
    typedef SignatureType::Context Context;
    typedef SignatureType::ResultType ResultType;
    typedef tmp::NilType Head;
    typedef tmp::NilType Tail;
  };

#if 0
  template <typename PredList>
    cwal_callback_f const CbDispatcher<PredList>::Function = CbDispatcher<PredList>::callback;
#endif


#if !defined(DOXYGEN)
  namespace Detail {
    /**
       Internal implementation detail for Argv_TypesMatch.

       The caller must pass an Index value of 0.
       TypeListT must be a Signature-compatible typelist, but
       only the length and argument types are evaluated.
    */
    template <int Index, typename TypeListT>
    struct Argv_TypesCheck : ArgumentsPredicate{
      /**
         Returns true if all arguments in argv
         have the types described in the TypeListT typelist.
         Comparison for type equality is done using
         ArgAt_IsA.

         The caller is assumed to have verified args->argc
         in advance. We do not do it here because it's a waste
         of cycles for all recursions after the first one.
      */
      inline bool operator()( cwal_callback_f const * args )const{
        enum {  Arity = sigli::Arity<TypeListT>::Value };
        /**
           This might seem a bit backwards, but it's correct:

           We start at the 0th index and work UP, but at
           each iteration we pass on TypeListT::Tail, which
           has a length of (Arity-1). Thus on each
           iteration we increase Index but always check the
           0th position of the list (the Head of the Tail!).

           We special-case for the end-of-list, which we
           hit at the Tail of 1-arity typelists. This is
           slightly unsightly but seems simpler/clearer
           than adding another internal level of
           indirection for the 0-arity case.
        */
        typedef ArgAt_IsA<Index, typename sigli::At<0,TypeListT>::Type > P1;
        typedef typename tmp::IfElse< 1==Arity,
          Argv_True,
          Argv_TypesCheck<Index+1, typename TypeListT::Tail >
          >::Type P2;
        return P1()(args) && P2()(args);
      }
    };
  }
#endif /* DOXYGEN */

  /**
     This ArgumentsPredicate implementation expects a typelist
     parameter which describes a list of argument types
     to match against at runtime.

     TypeListT may be "empty", in which case this type is
     functionally equivalent to Argv_Length<0>.

     See the operator() docs for more details.
  */
  template <typename TypeListT>
  struct Argv_TypesMatch : ArgumentsPredicate{
    /**
       Returns true if all of the following apply:

       - (args->argc == sigli::Length<TypeListT>::Value)

       - Argv_IsA<N,X>()(args) returns true for each argument,
       where N is the current argument index and X is the Nth
       type from TypeListT (== sigli::At<N,TypeListT>::Type).

       As a special case, for 0-length typelists this function
       returns true if only the first condition applies.
    */
    bool operator()( cwal_callback_args const * args ) const{
      enum { Arity = sigli::Length<TypeListT>::Value };
      typedef typename tmp::IfElse< 0==Arity,
        Argv_True,
        Detail::Argv_TypesCheck< 0, TypeListT >
        >::Type P;
      return (args->argc == (uint16_t)Arity) && P()(args);
    }
  };

  /**
     Template which creates a cwal_callback_f() to fetch
     the value of a non-const, non-function member of T.
  */
  template <typename T, typename V, V T::*M>
  struct MemberGetter : FunctionSignature<int (cwal_callback_args const *, cwal_value **)>,
    CallBackable {
    /**
      Extracts a T instance from args->self and sets *rv to the cwal-converted
      value of (theT->*M). On error, triggers a cwal-level exception and returns
      non-0.
    */
    static int callback( cwal_callback_args const * args, cwal_value **rv ){
      typedef CwalToNative<T> C2N;
      typedef typename C2N::ResultType RT;
      typedef Detail::Dereffer<RT> Deref;
      Deref const & deref = Deref();
      RT t = toNative<T>(args->engine, args->self);
      if(!deref(t)){
        return tossMissingThis<T>(args);
      }else{
        *rv = toCwal<V>(args->engine, deref(t)->*M);
        return RcHelper(args, 0);
      }
    }
  };

  /**
     Template which creates a cwal_callback_f() to fetch
     the value of a const, non-function member of T.
  */
  template <typename T, typename V, V const T::*M>
  struct ConstMemberGetter :
   FunctionSignature<int (cwal_callback_args const *, cwal_value **)>,
    CallBackable {
    /**
       Equivalent to MemberGetter::callback().
    */
    static int callback( cwal_callback_args const * args, cwal_value **rv ){
      typedef CwalToNative<T> C2N;
      typedef typename C2N::ResultType RT;
      typedef Detail::Dereffer<RT> Deref;
      Deref const & deref = Deref();
      RT t = toNative<T>(args->engine, args->self);
      if(!deref(t)){
        return tossMissingThis<T>(args);
      }else{
        *rv = toCwal<V>(args->engine, deref(t)->*M);
        return RcHelper(args, 0) /* remember: NULL is not, generically speaking, a wrong answer! */;
      }
    }
  };

  /**
     Template which creates a cwal_callback_f() to set the value of a
     non-const, non-function member of T to the first argument passed
     to the callback. It triggers an exception if no argument is
     provided. Conversion errors are silently ignored unless the converter
     itself triggers an exception.

     Returns args->self.
  */
  template <typename T, typename V, V T::*M>
  struct MemberSetter : FunctionSignature<int (cwal_callback_args const *, cwal_value **)>,
    CallBackable {
    /**
      Extracts a T instance from args->self and sets (theT->*M) to the
      toNative<V>()-converted value of args->argv[0]. On error,
      triggers a cwal-level exception and returns non-0. On success,
      *rv is set to args->self.
    */
    static int callback( cwal_callback_args const * args, cwal_value **rv ){
      if(!args->argc){
        return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                   "This property setter expects a value.")
          /* Reminder TypeName<V> is likely not defined for most param
             types, and we cannot define them for the built-in types
             unless we add them in non-header code (i.e. clients would
             need a .cpp file along with this API, or multi-include
             trickery to get them installed, taking care to avoid ODR
             violations).
          */;
      }
      typedef CwalToNative<T> C2N;
      typedef typename C2N::ResultType RT;
      typedef Detail::Dereffer<RT> Deref;
      Deref const & deref = Deref();
      RT t = toNative<T>(args->engine, args->self);
      if(!deref(t)){
        return tossMissingThis<T>(args);
      }else{
        deref(t)->*M = toNative<V>(args, 0);
        /* remember: NULL is not, generically speaking, a wrong answer! */; 
       return cwal_exception_get(args->engine)
          ? CWAL_RC_EXCEPTION
          : (*rv = args->self, 0);
      }
    }
  };

  /**
     Creates a callback which behaves like MemberGetter<T,V,M> if
     called with no arguments and like MemberSetter<T,V,M> if called
     with an argument.
  */
  template <typename T, typename V, V T::*M>
  struct MemberAccessor : FunctionSignature<int (cwal_callback_args const *, cwal_value **)>,
    CallBackable {
    /**
       Dehaves like MemberGetter<T,V,M> if called with no arguments
       and like MemberSetter<T,V,M> if called with an argument.
    */
    static int callback( cwal_callback_args const * args, cwal_value **rv ){
      typedef MemberGetter<T,V,M> Getter;
      typedef MemberSetter<T,V,M> Setter;
      return args->argc ? Setter::callback(args,rv) : Getter::callback(args,rv);
    }
  };


  /**
     May be specialized by clients to tell CtorToCb resp. its helper
     templates to allocate T objects using placement-new via memory
     allocated via cwal_alloc(). If its static/const Value member
     is true then those templates will allocate memory from cwal
     and use placement-new to instantiate new T instances, and the
     corresponding finalizers will use a manual dtor call and
     cwal_free2() to clean up/free the memory.

     The primary advantage of enabling this is that the script-bound
     native instances can share in cwal's memory recycling (if they
     are above a minimum size threshold, namely
     sizeof(cwal_memchunk_overlay)).
  */
  template <typename T>
  struct CtorToCb_AllocUsingCwal : tmp::BoolVal<false>
  {};

#if !defined(DOXYGEN)
  namespace Detail {

    template <typename T>
    T * allocFromCwal( cwal_engine * e ){
      return reinterpret_cast<T*>( cwal_malloc2(e, sizeof(T)) );
    }

    template <typename T>
    void freeFromCwal(cwal_engine * e, void * mem){
      cwal_free2(e, mem, sizeof(T));
    }

    /**
       Internal helper for cwal::CtorArgsProxy and/or CtorToCb.

       Arity is the number of ctor arguments we will be calling the
       ctor with. MethodSigT is a MethodSignature type holding the
       ctor signature.
    */
    template <uint16_t Arity, typename MethodSigT,
      bool mallocFromCwal = true>
    struct CtorArgsProxy{
      typedef typename MethodSigT::ResultType ResultType;
      static ResultType construct(cwal_callback_args const * args);
    };

    template <typename MethodSigT>
    struct CtorArgsProxy<0, MethodSigT, true>{
      typedef typename MethodSigT::ResultType ResultType;
      static ResultType construct(cwal_callback_args const * args){
        typedef typename MethodSigT::Context T;
        typedef typename sigli::Arity<MethodSigT> arity;
        typedef tmp::Assertion< 0 == arity::Value > AssertArity;
        if(!AssertArity::Value){}
        void * mem = allocFromCwal<T>(args->engine);
        ResultType that = 0;
        if(mem) try{
          that = new (mem) T;
        }catch(std::exception const &ex){
          cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                              "%s constructor (arity=%d) threw: %s",
                              TypeName<T>::Value, arity::Value, ex.what());
        }catch(...){
          cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                              "%s constructor (arity=%d) threw an unknown exception",
                              TypeName<T>::Value, arity::Value);
        }
        if(!that) freeFromCwal<T>(args->engine, mem);
        return that;
      }
    };

    template <typename MethodSigT>
    struct CtorArgsProxy<0, MethodSigT, false>{
      typedef typename MethodSigT::ResultType ResultType;
      static ResultType construct(cwal_callback_args const * args){
        typedef typename MethodSigT::Context T;
        typedef typename sigli::Arity<MethodSigT> arity;
        typedef tmp::Assertion< 0 == arity::Value > AssertArity;
        if(!AssertArity::Value){}
        ResultType that = 0;
        try{
          that = new T;
        }catch(std::exception const &ex){
          cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                              "%s constructor (arity=%d) threw: %s",
                              TypeName<T>::Value, arity::Value, ex.what());
        }catch(...){
          cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                              "%s constructor (arity=%d) threw an unknown exception",
                              TypeName<T>::Value, arity::Value);
        }
        return that;
      }
    };

  }/*Detail*/
#endif
/* ^^^ DOYGEN */


#if !defined(DOXYGEN)
#  include "cwal_convert_generated.hpp" // some specializations require these to be in place
  namespace Detail {
    /**
       Internal utility for defining (or not) conversions for certain
       types which might or might not be the same type as another
       type. e.g. (unsigned long) and (uint64_t) are the same type on
       some platforms and not on others.

       On platforms where the being-checked types are the same, we
       generate, instead of a dupe, a useless instantiation which will
       never get used.
    */
    template <typename T1, typename T2>
      struct DupeTypeCheck{
        typedef  typename tmp::IfElse<
          tmp::SameType<T1, T2>::Value,
          Detail::DummyPlaceholder<T1>,
          T1
       >::Type Type;
      };
  }
#endif
/* DOXYGEN */

  /**
     This specialization is a kludge/workaround for use in cases
     where (unsigned long int) is:

     1) The same type as the platform's pointer type.

     2) Somehow NOT the same as one of the standard uintNN_t types.
     (Yes, i've seen this happen, which is why this specialization
     exists.)

     3) Used in toNative() calls.

     If ulong and uint64 are the same type then this specialization
     is a no-op (it generates a converter for a type which will
     never be converted), otherwords it performs a numeric conversion.
  */
  template <>
  struct NativeToCwal<
    Detail::DupeTypeCheck<unsigned long, uint64_t>::Type
    > : NativeToCwal<uint64_t> {};

  //! long double/double variant of NativeToCwal<unsigned long>.
  template <>
  struct NativeToCwal<
    Detail::DupeTypeCheck<long double, double>::Type
  >{
    typedef long double Type;
    typedef Type ArgType;
    cwal_value * operator()( cwal_engine * e, ArgType n ) const{
      cwal_value * v = cwal_new_double(e, (cwal_double_t)n);
      if(!v) throw std::bad_alloc();
      else return v;
    }
  };

  //! CwalToNative variant of of NativeToCwal<unsigned long>.
  template <>
  struct CwalToNative<
    Detail::DupeTypeCheck<unsigned long, uint64_t>::Type
  > : CwalToNative<uint64_t> {};

  //! CwalToNative variant of of NativeToCwal<long double>.
  template <>
  struct CwalToNative<
    Detail::DupeTypeCheck<long double, double>::Type
  > {
      typedef long double Type;
      typedef Type ResultType;
      ResultType operator()( cwal_engine *, cwal_value * v ) const{
        return (ResultType)cwal_value_get_double(v);
      }
  };

#if 0
  /* C++98 doesn't specify long long. */
  template <>
  struct NativeToCwal<
    Detail::DupeTypeCheck<long long, int64_t>::Type
    > : NativeToCwal<int64_t> {};
  template <>
  struct CwalToNative<
    Detail::DupeTypeCheck<long long, int64_t>::Type
  > : CwalToNative<int64_t> {};
#endif

#if 0
  /**
     A utility template which translates cwal_callback_f() arguments from
     the script world into arguments for native constructor call.

     RawSig must be a function signature in this form:

     ConstructibleType * (...arg types...)
  */
  template <typename RawSig,
    typename FactoryT =
    Detail::CtorArgsProxy<
      sigli::Arity< Signature<RawSig> >::Value,
      MethodSignature<typename tmp::PlainType<typename Signature<RawSig>::ResultType>::Type, RawSig>,
      true
    >
  >
  struct CtorArgsProxy {
    typedef MethodSignature<typename tmp::PlainType<typename Signature<RawSig>::ResultType>::Type,
      RawSig> MethodSigType;
    typedef typename MethodSigType::FunctionType MethodSig;
    typedef typename MethodSigType::Context Context;
    typedef typename MethodSigType::ResultType ResultType;
    typedef typename MethodSigType::Head Head;
    typedef typename MethodSigType::Tail Tail;
    /**
       Converts the passed-in arguments to arguments for (one of) the
       Context type's N-arg constructor(s), where N is
       tmp::Arity<Signature<RawSig>>::Value.

       On success it returns a new instance. On error NULL is returned
       and a cwal-level exception is thrown or propagated.
    */
    static ResultType construct(cwal_callback_args const * args){
      typedef typename MethodSigType::ResultType T;
      typedef sigli::Arity<MethodSigType> arity;
      //typedef Detail::CtorArgsProxy<arity::Value, MethodSigType> CB;
      typedef FactoryT CB;
      T that = CB::construct(args);
      if(!that){
        if(!cwal_exception_get(args->engine)){
          cwal_exception_setf(args->engine, CWAL_RC_ERROR,
                              "Unknown error constructing a %s(with %d arg(s)).",
                              TypeName<ResultType>::Value,
                              arity::Value);
        }
      }
      return that;
    }
  };
#endif

  /**
     An interface for use with CtorToCb, for performing
     common post-binding work. Mainly intended to allow this framework to
     add native-to-cwal conversions using a type-specific mapping (e.g.
     via the CwalValueHolder interface). The default implementation is
     a no-op.

     Clients may optionally specialize this type to inject this
     behaviour, e.g.:

     @code
     template <>
     struct PostConstructor<MyType> : PostConstructor_CwalValueHolder<MyType> {};
     @endcode
  */
  template <typename T>
  struct PostConstructor {
    /**
       Returns 0.

       Implementations will be passed the underlying cwal engine, the
       new T instance, and the new cwal_value instance (of type
       cwal_native). If non-0 is returned, the constructor frees the
       new native via cwal_native_clear( cwal_value_native_part(vSelf,
       TypeName<T>::TypeID), 1 ), so the (T*) part will be destroyed
       there.
    */
    static int postconstruct( cwal_engine *, T * /*cSelf*/, cwal_value * /*vSelf*/ ){
      return 0;
    }
  };

#if !defined(DOXYGEN)
  namespace Detail {
    /**
       Internal cwal_finalizer_f() selection utilty.
    */
    template <typename T, typename SubT = T, bool freeUsingCwal = true>
    struct BasicFinalizerSelector {
      /**
         Assumes mem is a (SubT*) allocated using placement-new in
         sizeof(SubT) bytes of memory allocated by cwal_malloc2().
         Calls ~SubT() on the memory then cwal_free2()'s it.
      */
      static void finalize( cwal_engine * e, void * mem ){
        T * t = reinterpret_cast<SubT*>(mem);
        t->~SubT();
        cwal_free2(e, mem, sizeof(SubT));
      }
    };

    template <typename T, typename SubT>
    struct BasicFinalizerSelector<T,SubT,false> {
      /**
         Assumes mem is a (T*) allocated using (new T(...))  or (new
         SubT(...). Simply calls delete reinterpret_cast<T*>(mem).
      */
      static void finalize( cwal_engine *, void * mem ){
        delete reinterpret_cast<T*>(mem);
      }
    };

    /**
       Metatemplate which expects RawSig to be a raw function-style
       signature, e.g.

       T * (int, double)

       Its Type typedef resolves to the return type of that signature,
       shorn of any pointer/ref parts (so, T in the above example).
     */
    template <typename RawSig>
      struct ResultPlainType : 
        tmp::PlainType<typename Signature<RawSig>::ResultType>
    {};

    template <typename RawSig>
      struct ResultPlainType< Signature<RawSig> > : 
        tmp::PlainType<typename Signature<RawSig>::ResultType>
    {};

    template <typename T, typename RawSig>
      struct ResultPlainType< MethodSignature<T, RawSig> > : 
      tmp::PlainType<typename Signature<RawSig>::ResultType>
    {};

    template <typename RawSig>
      struct ResultPlainType< FunctionSignature<RawSig> > : 
      tmp::PlainType<typename Signature<RawSig>::ResultType>
    {};

  } /* namespace Detail */
#endif
/* ^^^ DOYGEN */

  /**
     A template helper which select a cwal_finalizer_f()
     implementation based on the value of
     CtorToCb_AllocUsingCwal<T>::Value.  If it is true then a finalizer
     which calls ~T() and cwal_free2() is selected, else a finalizer
     which uses (delete T) is selected. It has a single static member
     function, called finalize(), conforming to the cwal_finalizer_f()
     interface.
  */
  template <typename T, typename SubT = T>
  struct CtorToCb_FinalizerSelector :
    Detail::BasicFinalizerSelector<T, SubT, CtorToCb_AllocUsingCwal<SubT>::Value>
  {};

  /**
     This template creates a cwal_callback_f implementation which is
     intended to be used as a factory for T objects using s2's "new"
     keyword.

     T must be a non-cv-qualified type. CtorRawSig must be a function
     signature in this form: TOrSubT * (...param types...), where
     TOrSubT is either T or a type for which its pointers are
     implicitly convertible to (T*).

     - creates a new T using the factory specified as the 3rd
     parameter to this template. The default factory calls (new
     TOrSubT(...))  but will use placement-new if
     CtorToCb_AllocUsingCwal<TOrSubT>::Value is true.

     - Creates a new cwal_native value and maps the new T to it using
     a type ID of TypeName<T>::TypeID. It sets its finalizer method to
     the final template parameter of this template. The default
     finalizer is intended to be used with the default 3rd parameter
     (the factory), and calls either (delete T) or (~TOrSubT() and
     cwal_free2()) to clean up/free the instances finalized by this
     framework.

     - Sets the value's prototype to the prototype of args->self (so
     that it behaves in a sane manner by default). Post-construction
     can be used to change the prototype, provided that prototype is
     stored somewhere that the post-constructor can get to it.

     - Sets *rv to the new Value.

     Example:

     @code
     cwal_callback_f cb = CtorToCb<MyType, MyType*(int, int)>::callback;
     @endcode

     From C code, the new T instance can be fethced from its
     cwal_value counterpart by using cwal_value_native_part(theengine,
     theValue, TypeName<T>::TypeID).  That will return non-NULL if the
     given value either _is_ the T/cwal binder object (of type
     cwal_native) or if such an object is found in the value's
     prototype chain. To disregard prototypes, use cwal_native_get()
     instead.

     PostConstructorT must implement the PostConstructor interface and
     may be used to perform bootstrap-level work like adding
     native-to-cwal mapping somewhere in the new instance's members.
     If T implements the CwalValueHolder interface,
     PostConstructor_CwalValueHolder cwan be used here to perform that
     task.

     On error, if the new instance had already been created, it is
     destroyed via its defined finalizer and non-0 is returned.

     The lifetime of the new value is managed by the value stored in *rv,
     and client code must make no assumptions about it.

     T may differ from the return type in CtorRawSig if that return type
     derives from T (is implicitly convertible to (T*)).

     The returned object might not be completely usable. Some potential TODOs
     (from downstream code):

     - add a reverse binding such that toCwal<T>(theT) can work. The
     PostConstructor can potentially do that.

     - set/change the cwal-level prototype of the new instance.

     - cwal_native_set_rescoper() is a special-case need.


     Optional template parameters:

     FactoryT must implement this interface:

     @code
     static SubT * construct(cwal_callback_args const *);
     @endcode

     And must (abstractly) return a new T instance. It may throw but
     must not allow any C++ exceptions to pass through intervening C
     levels (if any).  This function converts std::exception to cwal
     exceptions using std::exception::what() as the message text, and
     unknown exceptions using a generic text.  It may also, on error,
     return NULL and set the cwal-level exception state on error. It
     may return a singleton or shared instance so long as the
     finalizer passed as the final parameter is a semantic match.

     FinalizerF must be a cwal_finalizer_f() implementation which
     destroys a T instance using the mechanism complementary to
     FactoryT::construct(). This finalizer will be called by cwal when
     either the Value part's lifetime is up or the client calls
     cwal_native_clear(theBoundNative, 1).

     The default template params use CtorToCb_AllocUsingCwal<T> to
     determine whether to use (new T(...)) or placement-new (new (mem)
     T(...)) when allocating new instances.


     Uho... if the ctor result type is set to a different type than T
     (which is legal as long as it is implicitly convertible to T)
     then the default template params will end up calling the wrong
     destructor.
  */
  template <typename T, typename CtorRawSig,
    typename PostConstructorT = PostConstructor<T>,
    typename FactoryT =
      Detail::CtorArgsProxy<
        sigli::Arity< Signature<CtorRawSig> >::Value,
        MethodSignature<T, CtorRawSig>,
        CtorToCb_AllocUsingCwal<
          typename Detail::ResultPlainType<CtorRawSig>::Type
        >::Value
    >,
    cwal_finalizer_f FinalizerF =
      &CtorToCb_FinalizerSelector<
         T,
         typename Detail::ResultPlainType<CtorRawSig>::Type
      >::finalize
  >
  struct CtorToCb : CallBackable {

    /**
       A cwal_callback_f() implementation which constructs a new T
       instance by converting args->argv to native arguments (if any)
       for its constructor. Then it creates a new cwal_native wrapping
       that (T*) and assigns it (on success) to *rv.

       It is intended to work with s2's "new" keyword feature, so it
       sets the prototype of the new value to args->self's prototype
       (we "could" instead set it to args->self, with the same end
       result, but that would leave one additional layer of objects
       (one of them empty) in the prototype chain). Clients may set a
       different prototype in their PostConstructor.
    */
    static int callback(cwal_callback_args const *args, cwal_value **rv){
      typedef MethodSignature<T, CtorRawSig> SigT;
      typedef typename SigT::Context TT;
      typedef typename sigli::Arity<SigT> arity;
      TT * that = 0;
      cwal_value * result = 0;
      try {
        that = FactoryT::construct(args);
      }catch(std::exception const &ex){
        return cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                                   "%s<arity=%d>() constructor threw: %s",
                                   TypeName<T>::Value, arity::Value,
                                   ex.what());
      }catch(...){
        return cwal_exception_setf(args->engine, CWAL_RC_EXCEPTION,
                                   "%s<arity=%d>() constructor threw an unknown exception",
                                   TypeName<T>::Value, arity::Value);
      }
      if(!that){
        /* assert(cwal_exception_get(args->engine)); */
        return cwal_exception_get(args->engine)
          ? CWAL_RC_EXCEPTION
          : cwal_exception_setf(args->engine, CWAL_RC_ERROR,
                                "Unknown error instantiating a %s instance.",
                                TypeName<TT>::Value);
      }
      result = cwal_new_native_value(args->engine, that,
                                     FinalizerF,
                                     TypeName<TT>::TypeID);
      if(!result){
        FinalizerF(args->engine, that);
        return CWAL_RC_OOM;
      }else{
        int rc =
          cwal_value_prototype_set(result,
                                   cwal_value_prototype_get(args->engine,args->self)
                                   /*for s2 "new", setting args->self
                                     as the prototype would also work
                                     but would leave one (empty)
                                     object in the prototype chain
                                     between *rv and its intended
                                     prototype. */)
        /* overwrite the prototype s2's "new" set for
           us. Post-construction might override that. */;
        if(rc){
          rc = cwal_exception_setf(args->engine, rc,
                                   "Prototype assignment failed with code %d/%s.",
                                   rc, cwal_rc_cstr(rc));
        }else{
          cwal_value_ref(result);
          rc = PostConstructorT::postconstruct(args->engine, that, result);
          cwal_value_unhand(result);
        }
        if(rc){
          cwal_native_clear(cwal_value_get_native(result), 1);
          result = 0;
        }else{
          *rv = result;
        }
        return rc;
      }
    }
  };

  /**
     A convenience subclass/interface for types which need to do
     a reverse mapping of T instances from cwal to native space.

     T is not actually used in the class: it is only so that this
     trivial class can live in the header files.

     This class does not hold a cwal-level reference to its pointed-to
     value because this type does not/cannot know about scoping-level
     details which could invalidate the value out from under this
     class. The pointed-to value is managed completely by the
     underlying cwal_engine.
  */
  template <typename T>
  class CwalValueHolder {
    private:
      cwal_value * cwv;
    public:
      CwalValueHolder() : cwv(0) {}
      virtual ~CwalValueHolder() {}
      /** Gets this object's cwal-side value, which is NULL or owned
          by the underlying cwal_engine. Clients must never do
          anything untowards with it (e.g. unref'ing it without
          ref'ing it first).

          When instances are destroyed from script-space, this value
          will still be valid but of limited use: it is on its way out
          of the world and must not be modified during destruction.
      */
      cwal_value * cwalValue() const{
        return this->cwv;
      }

      /** Sets this object's value. It does not take a reference
          or inspect the value in any way. This "really should"
          be protected, but bindings cannot be implemented that way
          due to a chicken/egg problem. Clients should only call this
          from PostConstructor routines (or similar).
      */
      void cwalValue( cwal_value * v ){
        this->cwv = v;
      }
  };


  /**
     A PostConstructor-conforming type for use with CtorToCb
     when T implements the CwalValueHolder interface.
  */
  template <typename T>
  struct PostConstructor_CwalValueHolder {
    typedef typename tmp::PlainType<T>::Type Type;
    /**
       Calls cSelf->cwalValue(vSelf) and returns 0.
    */
    static int postconstruct( cwal_engine *, Type * cSelf, cwal_value * vSelf ){
      cSelf->cwalValue(vSelf);
      return 0;
    }
  };

  /**
     A NativeToCwal proxy for types implementing the CwalValueHolder
     interface.

     To use it:

     @code
     template <>
     struct NativeToCwal<MyType> : NativeToCwal_CwalValueHolder<MyType>
     {};
     @endcode

     Where MyType must implement the CwalValueHolder interface.
  */
  template <typename CwalValueHolderT>
  struct NativeToCwal_CwalValueHolder {
    typedef CwalValueHolderT Type;
    typedef const CwalValueHolderT * ArgType;
    /** Returns n->cwalValue() or NULL. */
    cwal_value * operator()( cwal_engine *, ArgType n ) const{
      return n ? n->cwalValue() : NULL;
    }
  };

  /**
     Experimental/incomplete.

     A helper for creating new prototype instances.

     CwalTypeId specifies the base-most type for Value instances
     created via this class. It must be either CWAL_TYPE_OBJECT or
     CWAL_TYPE_ARRAY, and that gets statically asserted.

     ACHTUNG: using this type may cause std::exceptions to be thrown,
     so the caller must be sure to catch those rather than letting
     them pass through any C APIs (all of cwal except this binding
     layer!).
  */
  template <cwal_type_id CwalTypeId = CWAL_TYPE_OBJECT>
  class ProtoMaker{
    typedef tmp::Assertion<
    CwalTypeId == CWAL_TYPE_OBJECT || CwalTypeId == CWAL_TYPE_ARRAY
    > AssertPrototypeBase;

    typedef std::map<std::string, cwal_callback_f> CBMap;
    cwal_engine * e;
    CBMap cbmap;
#if CWAL_CONVERT_ENABLE_S2 | CWAL_CONVERT_ENABLE_WHCL
    /* Add constructor bits... */
    cwal_callback_f valueCtor;
#else
    int valueCtor /* dummy placeholder */;
#endif
    /**
       Returns a new instance of the type implied by the CwalTypeId
       template parameter.
    */
    cwal_value * newProtoBase(){
      switch(CwalTypeId){
        case CWAL_TYPE_OBJECT:
          return cwal_new_object_value(this->e);
        case CWAL_TYPE_ARRAY:
          return cwal_new_object_value(this->e);
        default:
          // can't happen: statically asserted.
          throw std::runtime_error("Unhandled cwal type ID in ProtoMaker.");
      }
    }

    //! not implemented!
    ProtoMaker(const ProtoMaker &);
    //! not implemented!
    ProtoMaker & operator=(const ProtoMaker &);
    public:

    //typedef T Type;
    //typedef ProtoMaker_Factory<T> Factory;

    explicit ProtoMaker(cwal_engine * e)
      : e(e), cbmap(), valueCtor(0){
      assert(e);
      assert(cwal_scope_current_get(e));
    }

    ~ProtoMaker(){
    }

    /**
       Queues the given callback for installation into the (pending)
       prototype using the given name.
    */
    ProtoMaker & cb( char const * name, cwal_callback_f cb ){
      assert(name && *name);
      cbmap[name] = cb;
      return *this;
    }

    /**
       FMPtr must be a FunctionPtr or MethodPtr which is compatible
       with ToCb<FMPtr>. This method creates a callback converter for
       that function/method and installs it into the (pending)
       prototype using the given name.
    */
    template <typename FMPtr>
    ProtoMaker & toCb( char const * name ){
      return this->cb( name, ToCb< FMPtr >::callback );
    }

    /**
       Used by createPrototype() to specify where/whether the new
       prototype should be stored.
    */
    enum StoragePolicy {
    /**
       Tells createPrototype() not to store the new instance anywhere.
    */
    DoNotStore = 0,
    /**
       Tells createPrototype() to store the new instance as a const
       in the top-most scope, using the given class name.
    */
    StoreGlobal = 1,
    /**
       Tells createPrototype() to store the new instance as a const in
       the current scope, using the given class name.
    */
    StoreLocal = 2,
    /**
       May be OR'd with StoreLocal or StoreGlobal to specify that it should
       be "hidden" (which actually has almost no effect on scope-level properties,
       now that i think about it).
     */
    StoreHidden = 0x10
    };

    /**
       Instantiates a new instance of the prototype described by
       calls to this object's API made prior to this call.
       Returns the new instance on success, NULL on error.

       className may be NULL or empty if storePolicy is DoNotStore,
       otherwise a NULL/empty className causes NULL to be returned. If
       not NULL, className must be NUL-terminated. An _empty_ name is
       technically legal but there's normally no way to fetch/resolve
       it from client-side scripts (only from C), so it is disallowed
       here.

       If className is not NULL/empty, the prototype's __typename
       property is set to className.
    */
    cwal_value * createPrototype( char const * className,
                                  StoragePolicy storePolicy = DoNotStore ){
      if(((storePolicy&StoreGlobal) || (storePolicy&StoreLocal))
         && (!className || !*className)) return NULL;
      int rc = 0;

      cwal_scope * sc = (StoreGlobal & storePolicy)
        ? cwal_scope_top(cwal_scope_current_get(this->e))
        : ((StoreLocal & storePolicy)
           ? cwal_scope_current_get(this->e)
           : 0);
      assert(sc || !(storePolicy & 0x0F));
      cwal_value * proto = this->newProtoBase();
      if(!proto) return 0;
      assert(cwal_props_can(proto));
      cwal_value_ref(proto);
      typedef CBMap::const_iterator IT;
      IT it = cbmap.begin();
      IT end = cbmap.end();
#if CWAL_CONVERT_ENABLE_S2
      if(this->valueCtor){
        s2_engine * se = s2_engine_from_state(this->e);
        assert(se);
        rc = s2_ctor_callback_set(se, proto, this->valueCtor);
      }
#elif CWAL_CONVERT_ENABLE_WHCL
      if(this->valueCtor){
        whcl_engine * const el = whcl_engine_from_state(this->e);
        assert(el);
        rc = whcl_ctor_callback_set(el, proto, this->valueCtor);
      }
#endif
      if(!rc && className && *className){
        cwal_value * cv = cwal_new_string_value(this->e, className, cwal_strlen(className));
        cwal_value_ref(cv);
        rc = cv
          ? cwal_prop_set( proto, "__typename", 10, cv )
          : CWAL_RC_OOM;
        cwal_value_unref(cv);
      }
      for( ; !rc && it != end; ++it ){
        cwal_value * fv = cwal_new_function_value(this->e, it->second,
                                                  0, 0, 0 );
        if(!fv){
          rc = CWAL_RC_OOM;
          break;
        }else{
          cwal_value_ref(fv);
          rc = cwal_prop_set( proto, it->first.c_str(),
                              (cwal_size_t)it->first.size(),
                              fv );
          cwal_value_unref(fv);
        }
      }
      if(rc){
        cwal_value_unref(proto);
        proto = 0;
      }else{
        if((storePolicy&StoreGlobal) || (storePolicy&StoreLocal)){
          // The error handling here exemplifies how painful C can be,
          // compared to C++, in this regard...
          assert( className && *className );
          rc = cwal_scope_chain_set_with_flags
            (sc, 0, className, cwal_strlen(className),
             proto, CWAL_VAR_F_CONST | ((storePolicy & StoreHidden)
                                        ? CWAL_VAR_F_HIDDEN
                                        : 0));
          cwal_value_unref(proto);
          if(rc){
            proto = 0;
          }else{
            /* top scope now holds a ref */;
            assert(cwal_value_refcount(proto)>0 && "Expecting ref from scope.");
          }
        }else{
          // Don't stash it - assume the caller knows what he's doing.
          cwal_value_unhand(proto) /* reprobate it until/unless client
                                      takes a ref. */;
          assert(cwal_value_scope(proto) && "Expecting proto to still be alive.");
        }
      }
      return proto;
    }

    template <typename T>
    cwal_value * createPrototype( StoragePolicy storePolicy = DoNotStore ){
      return this->createPrototype( TypeName<T>::Value, storePolicy );
    }

#if CWAL_CONVERT_ENABLE_S2
    /**
       Stores CbT::callback for installation as an s2 constructor
       function (for use with the "new" keyword). The ctor will be
       installed using s2_ctor_callback_set(), but not only
       createPrototype() is called. This only works if the cwal_engine
       passed to this object's constructor is managed by an s2_engine.
    */
    template <typename CbT>
    ProtoMaker & s2Ctor(){
      assert(s2_engine_from_state(this->e));
      this->valueCtor = CbT::callback;
      return *this;
    }
#elif CWAL_CONVERT_ENABLE_WHCL
    /**
       Stores CbT::callback for installation as an s2 constructor
       function (for use with the "new" keyword). The ctor will be
       installed using whcl_ctor_callback_set(), but not only
       createPrototype() is called. This only works if the cwal_engine
       passed to this object's constructor is managed by an whcl_engine.
    */
    template <typename CbT>
    ProtoMaker & whclCtor(){
      assert(whcl_engine_from_state(this->e));
      this->valueCtor = CbT::callback;
      return *this;
    }
#endif

  };//ProtoMaker

  /**
     A cwal_callback_f() intended for use as a method of native-bound
     values, which destroys the "native half" and disconnects the
     "cwal half" from it, such that the cwal half (which is still
     alive) can be safely used without risk of stepping on a stale
     pointer (but methods must be certain to check for a NULL when
     converting via toNative<T>()). It looks for a native with a
     type ID of TypeName<T>::TypeID in args->self and its
     prototypes. If it finds one, it clears the native/cwal
     association for that object by calling cwal_native_clear() on it,
     which calls its native-level cwal_finalizer_f() (if any).

     If no mapping is found, it triggers a cwal-level exception and
     returns non-0.

     Example usage:
     @code
     cwal_function * f = newFunction< CbToCb< cwal_callback_f_native_clear<T> > >(...);
     @endcode
   */
  template<typename T>
  int cwal_callback_f_native_clear( cwal_callback_args const * args,
                                    cwal_value ** ){
    cwal_native * nv = cwal_value_native_part(args->engine, args->self,
                                              cwal::TypeName<T>::TypeID);
    if(nv){
      cwal_native_clear(nv, 1)
        /* ^^^ That cleans up the underlying T part immediately and
           unlinks nv from it, such that future calls to fetch it will
           result in a NULL pointer, rather than a stale/recycled
           pointer.
        */;
      return 0;
    }else{
      return cwal_exception_setf(args->engine, CWAL_RC_TYPE,
                                 "Finalizer could not extract native "
                                 "of type (%s *) from args->self.",
                                 TypeName<T>::Value);
    }
  }


}// namespace cwal


#undef DOXYGEN_FWD_DECL_KLUDGE
#endif
/* NET_WANDERINGHORSE_CWAL_CONVERT_HPP_INCLUDED_ */

#if 0
/* failed experiment, but might want something like this someday... */
#if !defined(CWAL_CONVERT_INSTALL_TYPENAMES_)
#define CWAL_CONVERT_INSTALL_TYPENAMES_ 1
#include "cwal_convert.hpp"
#else
namespace cwal {
#define CWAL_TYPE_NAME(T,N) CWAL_TYPE_NAME_DECL(T); CWAL_TYPE_NAME_IMPL2(T,N)
  CWAL_TYPE_NAME((int16_t),"int16_t");
  CWAL_TYPE_NAME((int32_t),"int32_t");
  CWAL_TYPE_NAME((int64_t),"int64_t");
  CWAL_TYPE_NAME((double),"double");
  CWAL_TYPE_NAME((char const *),"char const *");
#undef CWAL_TYPE_NAME
}
#endif
#endif
