/**
   A "supermacro" intended for use in emiting much of the bootstrapping
   code for binding client classes to cwal's C++ type conversion API.

   Usage:

   @code
   // REQUIRED: type name of class being plugged in. May have a template part.
   #define REG_TYPE MyType

   // REQUIRED: string name of the REG_TYPE class
   #define REG_TYPE_NAME "MyType"

   // OPTIONAL: calls CWAL_TYPE_NAME_IMPL2:
   #define REG_TYPE_NAME_IMPL

   // OPTIONAL: disables searching prototypes for REG_TYPE resolution:
   #define REG_TO_NATIVE_NO_PROTOTYPES

   // OPTIONAL: disables generation of a PostConstructor, in which case
   // the client may need to install one himself:
   #define REG_NO_POSTCONSTRUCTOR

   // Finally...
   #include "this_file.hpp"
   @endcode

   All macros listed above are undef'd by this file, so it can be reused
   multiple times.
*/
#if !defined(REG_TYPE)
#error define REG_TYPE before including this file.
#endif
#if !defined(REG_TYPE_NAME)
#error define REG_TYPE_NAME before including this file.
#endif

namespace cwal {
    CWAL_TYPE_NAME_DECL((REG_TYPE));

#if defined(REG_TYPE_NAME_IMPL)
    CWAL_TYPE_NAME_IMPL2((REG_TYPE), REG_TYPE_NAME);
#  undef REG_TYPE_NAME_IMPL
#endif

#if defined(REG_TO_NATIVE_NO_PROTOTYPES)
#  define REG__SEARCH_PROTO false
#  undef REG_TO_NATIVE_NO_PROTOTYPES
#else
#  define REG__SEARCH_PROTO true
#endif

  /**
     Enables toNative<REG_TYPE>() to (type-safely) extract a REG_TYPE from
     a cwal_value or fail gracefully (returning NULL) for non-REG_TYPE
     values.
  */
  template <>
  struct CwalToNative< REG_TYPE > : CwalToNative_NativeTypeID< REG_TYPE,
                                                               REG__SEARCH_PROTO >
  {};

  /**
     The NativeToCwal<> template Enables toCwal<REG_TYPE>() to
     work. This cannot be done generically - it requires some sort of
     infrastructure to hold the cwal/native mapping.

     See the docs for the primary specialization for details.

     Note that various library-level partial specializations take care
     of the (REG_TYPE&) and const variations of this type by using this
     one as a proxy.
  */
  template <>
  struct NativeToCwal< REG_TYPE > : NativeToCwal_CwalValueHolder< REG_TYPE >{};

#if defined(REG_NO_POSTCONSTRUCTOR)
#  undef REG_NO_POSTCONSTRUCTOR
#else
  /**
     Used by CtorToNativeFactoryCallback to install the native-to-cwal
     mapping, performed after
  */
  template <>
  struct PostConstructor< REG_TYPE >
    : PostConstructor_CwalValueHolder< REG_TYPE > {};

#endif

#if defined(REG_ALLOC_USING_CWAL)
  template <>
  struct CtorToCb_AllocUsingCwal< REG_TYPE > : tmp::BoolVal<true>
  {};
# undef REG_ALLOC_USING_CWAL
#else
  template <>
  struct CtorToCb_AllocUsingCwal< REG_TYPE > : tmp::BoolVal<false>
  {};
#endif

}

#undef REG_TYPE
#undef REG_TYPE_NAME
#undef REG__SEARCH_PROTO
