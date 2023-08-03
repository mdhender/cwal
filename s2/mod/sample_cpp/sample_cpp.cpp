/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/**
   Example s2 loadable module using the cwal_convert.hpp templates
   to create the cwal/C[++] bindings.
*/
#include <cassert>
#include <iostream>
#include <fstream>
#include <list>
//#include <cstdlib>

#include "libs2.h"

/** @internal
   Convenience macros.
*/
#define S2_UNUSED_VAR __attribute__((__unused__)) /* avoiding unused var in non-debug build */

#define CERR std::cerr << __FILE__ << ':' << std::dec << __LINE__ << ':' << __FUNCTION__ << "(): "
#define COUT std::cout << __FILE__ << ':' << std::dec << __LINE__ << ':' << __FUNCTION__ << "(): "
#include "wh/cwal/cwal_convert.hpp"

#ifdef S2_OS_UNIX
#include <unistd.h>
#endif

/**
   Custom client-side native type to bind to s2. We use
   CwalValueHolder only to simplify two-way cwal/native bindings -
   it's not strictly necessary. The bindings can be made completely
   non-instrusively, with the caveat that the native-to-cwal
   conversion for the type may not be feasible (or may be more
   difficult). This conversion is only needed when binding members
   which are of MyType or functions which _return_ that type. When
   function _parameters_ are of that type, the cwal-to-native
   conversions (provided non-instrusively by this framework) apply.
*/
struct MyType : public cwal::CwalValueHolder<MyType> {
  int const constValue;
  int value;
  char padding[sizeof(cwal_memchunk_overlay)
               /* internal testing hack to ensure that cwal-level
                  recycling works - it won't trigger if our sizeof is
                  smaller than this! */
               / 2 /* to make up for our other members */];
  MyType()
    : cwal::CwalValueHolder<MyType>(),
    constValue(-1), value(1){
    COUT << "MyType@"<<(void const *)this<<"::MyType()\n";
  }

  MyType(int x)
    : cwal::CwalValueHolder<MyType>(),
    constValue(1), value(x){
    COUT << "MyType@"<<(void const *)this<<"::MyType(int)\n";
  }

  MyType(int,int)
    : cwal::CwalValueHolder<MyType>(),
    constValue(1), value(0){
    COUT << "MyType@"<<(void const *)this
         <<"::MyType(int,int) IS THROWING "
         << "(just to demonstrate what happens).\n";
    throw std::runtime_error("Just testing ctor throwing.");
  }

  ~MyType(){
    COUT << "MyType@"<<(void const *)this<<"::~MyType() vSelf="
         << this->cwalValue()<<"\n";
  }

  bool operator==(MyType const &rhs) const{
    return this->cwalValue() == rhs.cwalValue();
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

  MyType & self(){
    return *this;
  }

  MyType * passPtr(MyType * x) const{
    return x;
  }

  MyType * passRef(MyType & x) const{
    return &x;
  }

  int overloadOneInt(int v) const{
    COUT << "MyType@"<<this<<"::"<<__FUNCTION__<<"("<<v<<")\n";
    return v;
  }
  double overloadOneDbl(double v) const{
    COUT << "MyType@"<<this<<"::"<<__FUNCTION__<<"("<<v<<")\n";
    return -v;
  }
  int overloadTwo(int v, double v2) const{
    COUT << "MyType@"<<this<<"::"<<__FUNCTION__<<"("<<v<<", "<<v2<<")\n";
    return v;
  }

};

int MyType_non_member( MyType * my, int num ){
  assert(my && "cwal::-level conversion fails if !arg[0].");
  COUT << "MyType@"<<my<<" via " << __FUNCTION__<<"(self,"<<num<<")\n";
  return num;
}

int MyType_non_member2( MyType const & my, int num ){
  COUT << "MyType@"<<&my<<" via " << __FUNCTION__<<"(self,"<<num<<")\n";
  return num;
}

/* Registration bits needed by the conversion layers... */
#if 0
/* Use a "supermacro" to create some of the bootstrap-level C++/cwal
   binding code the "easy way"... */
#  define REG_TYPE MyType
#  define REG_TYPE_NAME "MyType"
#  define REG_TYPE_NAME_IMPL /* if we don't do this, we must call
                                CWAL_TYPE_NAME_IMPL2 _after_
                                registration. */

/* REG_ALLOC_USING_CWAL is optional: tells it to use cwal's
   (de)allocator instead of new/delete. This allows its memory to be
   recyclable using cwal's memory management system. */
#  define REG_ALLOC_USING_CWAL

#  include "cwal_reg_CwalValueHolder.hpp"
// If we didn't define REG_TYPE_NAME_IMPL above, we also need:
//   namespace cwal {  CWAL_TYPE_NAME_IMPL2((MyType), "MyType"); }

#else
/***********************************************************************
  Now do the same thing the "hard" way...
*/
namespace cwal {

  /**
     Declare our TypeName specialization. This would normally
     go in the declaration part (header file), after the class
     has been declared.

     The conversion API uses TypeName<T>::TypeID as a default value in
     some places, and we plug in to that by using it as MyType's type
     ID for cwal registration purposes. This type ID is what allows
     the C layer to type-safely determine whether a given (void*) is-a
     MyType or not.
  */
  CWAL_TYPE_NAME_DECL((MyType));

  /**
     Implement our TypeName specialization. It must come after MyType
     is declared and is subject to the ODR, so it normally goes in the
     class' impl file.
  */
  CWAL_TYPE_NAME_IMPL2((MyType),"MyType");

  /**
     Enables toNative<MyType>() to (type-safely) extract a (MyType*)
     from a (cwal_value*) or fail gracefully (returning NULL) for
     non-MyType values.
  */
  template <>
  struct CwalToNative<MyType> : CwalToNative_NativeTypeID<MyType,true>
  {};

  /**
     The NativeToCwal<> template enables toCwal<MyType>() to
     work. This cannot be done generically - it requires some sort of
     infrastructure to hold the native-to-cwal mapping.

     For MyClass we're simply subclassing CwalValueHolder as a
     convenient way of holding such a mapping (via
     CwalValueHolder::cwalValue() member).
     
     See the docs for the primary specialization for more details.

     Note that various library-level partial specializations take care
     of the (MyType&) and const variations of this type by using this
     one as a proxy. i.e. we only need a specialization for the
     unadorned MyType, not (MyType*), (MyType const), etc.
  */
  template <>
  struct NativeToCwal<MyType> : NativeToCwal_CwalValueHolder<MyType>{};

  /**
     Optional: this policy class tells CtorToCb resp. its helpers
     whether to use a combination of new/delete or
     cwal_malloc2()/placement-new/destructor/cwall_free2() to
     allocate/finalize/free new instances.

     Derive from BoolVal<true> use cwal's memory management or
     BoolVal<false> to use new/delete.
  */
  template <>
  struct CtorToCb_AllocUsingCwal< MyType > : tmp::BoolVal<true>
  {};

  /**
     Used by CtorToCb to install the native-to-cwal mapping, performed
     after the constructor is called (via CtorToCb) but before the
     object is returned to the user.
  */
  template <>
  struct PostConstructor<MyType>
#if 0
  /* To make use of PostConstructor_CwalValueHolder, simply... */
    : PostConstructor_CwalValueHolder<MyType> {};
#else
  /* Or we could do that work ourselves (and output some logging stuff)... */
  {
    static int postconstruct( cwal_engine * e, MyType * cSelf, cwal_value * vSelf ){
      assert(!cSelf->cwalValue());
      COUT << "post-constructing of MyType@"<<cSelf<<" w/ value@"<<vSelf<<".\n";
      cSelf->cwalValue(vSelf);
      /**
         Hmmm... how do we get the prototype in here? Turns out
         "new's" default behaviour is fine for most cases and is
         hooked up by CtorToCb. The native MyType prototype has
         already been set on vSelf (via a cwal_native) by the time
         this has called.
      */
      assert( toNative<MyType>(e, vSelf) == cSelf
              /* this conversion comes "for free" via the binding
                 framework. */);
      assert( toCwal<MyType>(e, cSelf) == vSelf
              /* This conversion requires type-specific native-to-cwal
                 mapping info. We provide that by simply subclassing
                 CwalHolder to use its cSelf->cwalValue() mapping (which
                 we set up above). */ );
      return 0;
    }
  };
#endif

  /**
     With the above templates in place, cwal's C++ API is now ready
     to do all sorts of conversions involving MyType...
   */
}/* cwal namespace */
#endif
/* ^^^ end of cwal/C++ conversion bootstrap stuff. The rest is essentially
   "real client-side code."
***********************************************************************/


/* Internal helper macro for cwal_callback_f() implementations bound
   to MyType instances which ensures that the arguments->self value is
   a MyType instance or triggers a cwal exception... */
#define THIS_MY                                                         \
  MyType * self = cwal::toNative<MyType>(args->engine, args->self);      \
  if(!self) return cwal::tossMissingThis<MyType>(args)

/**
   Example cwal_callback_f() implementation which is intended to be
   bound to MyType values.
*/
static int my_cb_foo( cwal_callback_args const * args,
                      cwal_value **rv ){
  cwal_string * str;
  THIS_MY;
  str = cwal_new_stringf(args->engine,
                         "Hello from 0x%p->foo()", (void const *)self);
  if(!str) return CWAL_RC_OOM;
  else{
    *rv = cwal_string_value(str);
    return 0;
  }
}

#if 0
/* This is now implemented via templates, but demonstrates how to
   write a generic getter/setter callback...*/
static int my_cb_getSetValue( cwal_callback_args const * args,
                              cwal_value **rv ){
  THIS_MY;
  if(args->argc){
    /* setter... */
    self->value = cwal::toNative<int>(args, 0);
    *rv = args->argv[0];
  }else{
    /* getter... */
    *rv = cwal::toCwal<int>(args->engine, self->value);
  }
  return *rv ? 0 : CWAL_RC_OOM;
}
#endif

/**
   Example of installing a custom native instance as a module...

   Module return their result value by assigning it to *rv.  On error,
   they "really should not" modify *rv.

   The naming convention for this function, for the v2-style
   module interface, is s2_module_init_{MODULE_NAME}.
*/
static int s2_module_init_sample_cpp( s2_engine * se, cwal_value ** rv ){
  int rc /* for checking cwal-level error codes */;
  cwal_value * v /* a transient value */;
  cwal_value * myProto = 0 /* a prototype object for our native binding */;
  try {
    /* ^^^^ Modules must NEVER EVER EVER (EVER!!!) let a C++ exception propagate
       out of any routines having to do with cwal!!! Some of the C++ bindings
       catch them and convert them to cwal-level exceptions, but much of what
       we will be doing below is not covered by that. */
    cwal_callback_f fp  = 0/* just for some conversion tests */;
    typedef cwal::ProtoMaker<CWAL_TYPE_OBJECT> PM /* utility to assist in creating prototypes */;
    PM pm(se->e);
    if(!fp){/*avoid assigned-but-unread var warning. fp is only used
              to test compilability of certain conversions which, if
              they compile at all, work.*/}

    ////////////////////////////////////////////////////////////////////////
    // Now the boilerplate is out of the way...

    /**
       Let's make sure our various to/from cwal/native conversions are
       working. If these compile at all, then they either work as-is
       or will throw a C++ exception if (A) they are a reference-type
       conversion and (B) cannot perform a pointer-type conversion
       (because it returns NULL). If a pointer conversion is not legal
       for the type, none of this will compile at all.
       Pointer-conversion contexts fail by simply returning NULL, as
       opposed to throwing, as a failure to convert to a given pointer
       type is not, strictly speaking, generally an error, but it will
       be treated as one in certain contexts.
    */

    /*
       We'll use ProtoMaker to set up the prototype-level bindings, as
       that's the easiest way to do it so far. The typedefs which
       follow each do their part in creating cwal_callback_f()
       functions from non-callback functions and methods. Each one
       stores type information and function/method pointers which we
       convert to cwal_callback_f() functions using template magic.
    */
    typedef cwal::MethodPtr<MyType const, int(int), &MyType::func1c> mFunc1c;
    typedef cwal::MethodPtr<MyType const, double(std::string const &), &MyType::funcStr> mFuncStr;
    /* Let's go crazy and bind native a sleep() function... */
#ifdef S2_OS_UNIX
    typedef cwal::FunctionPtr< unsigned (unsigned), ::sleep> fSleep;
#else
    typedef cwal::CbToCb< s2_cb_sleep > fSleep;
#endif
    typedef cwal::CbToCb< s2_cb_mssleep > fSleepMs;
    /*
      Functions _returning_ a (MyType*) or (MyType&) (or a const
      variant thereof) require a MyType-to-cwal conversion, which
      cannot be done 100% generically - it requires either class-level
      member or an external mapping of the cwal/native binding. In our
      case we simply subclasses CwalValueHolder to get that
      functionality.  It can be done without subclassing, but is more
      work and potentially tricky to manage vis-a-vis Value lifetimes.
    */
    typedef cwal::MethodPtr<MyType, MyType &(), &MyType::self> mSelf;
    typedef cwal::MethodPtr<MyType const, MyType *(MyType *), &MyType::passPtr> mPassPtr;
    typedef cwal::MethodPtr<MyType const, MyType *(MyType &), &MyType::passRef> mPassRef;
    typedef cwal::QuasiMethod<
      /* QuasiMethods are a specific category of non-member function
         which take a class-type pointer as their first argument. We
         can bind those like member functions, automatically passing on
         our bound native as the first parameter when they are called
         from script space. So let's do that... */
      cwal::FunctionPtr< int (MyType*,int), MyType_non_member >
      > quasiMember;
    typedef cwal::QuasiMethod<
      /* Just making sure that the (const &) variant works...
         It will throw a C++ exception, which gets translated to a
         cwal-level exception, if passed a non-MyType first argument
         from script-space. That can happen when our "destroy" method,
         bound below, is called to clean up the native part. */
      cwal::FunctionPtr< int (MyType const &,int), MyType_non_member2 >
      > quasiMember2;
    /*
      Generating getter/setter-style functions which wrap access to
      non-function class members...
    */
    typedef cwal::MemberGetter<MyType,int,&MyType::value> mGetter;
    typedef cwal::MemberSetter<MyType,int,&MyType::value> mSetter;
#if 1
    typedef cwal::ConstMemberGetter<MyType const,int,&MyType::constValue> mGetterConst;
#else
    /* Catching _const_ members through MemberGetter doesn't work. i
       haven't figured out the voodoo for that yet. */
    typedef cwal::MemberGetter<MyType const,int,&MyType::constValue> mGetterConst;
#endif
    /*
      A Combo getter/setter: getter func() and setter func(V) 
    */
    typedef cwal::MemberAccessor<MyType,int,&MyType::value> mGetterSetter;

    /**
       One more step to go: bind several C++ constructors to
       overloaded variants of the s2's "new" keyword. This is not
       directly part of the ProtoMaker's API because plugging in the
       ctor is s2-level functionality, whereas ProtoMaker is "pure
       cwal". That said, when ProtoMaker sees that s2 was included, it
       adds ProtoMaker::s2Ctor() to support us...
    */
    typedef cwal::CbRule<
      // No args == default ctor
      cwal::Argv_Length<0>,
      cwal::CtorToCb<
        MyType,
        MyType*()
      >
    >  Ctor0;
    typedef cwal::CbRule<
      // 1 arg === (int) ctor
      cwal::Argv_Length<1>,
      cwal::CtorToCb<
        MyType,
        MyType*(int)
      >
    > Ctor1;
    typedef cwal::CbRule<
      // The 2-arg ctor always throws a C++ exception (just for testing).
      // That exception gets converted to a cwal exception by the bindings
      // layer.
      cwal::Argv_Length<2>,
      cwal::CtorToCb<
        MyType,
        MyType*(int,int)
      >
    > CtorThrows;
    // Create a dispatcher for the above rules, which runs the first
    // rule which matches...
    typedef cwal::CbDispatcher<
      CWAL_TYPELIST(( Ctor0, Ctor1, CtorThrows ))
      > Ctors;

    // Now let's bind all of those functions to our (upcoming) prototype object...
    pm
      /* how nicely the two-char variable name lines up with 2-space indentation... */
      .cb("destroy", cwal::cwal_callback_f_native_clear<MyType,true>)
      .cb("foo", my_cb_foo )
      .toCb<mFunc1c>("func1c")
      // equivalent, but longer:
      // .cb("func1c", cwal::ToCb<mFunc1C>::callback )
      .toCb<mFuncStr>("funcStr")
      .toCb<fSleep>("sleep")
      .toCb<fSleepMs>("mssleep")
      .toCb<mSelf>("self")
      .toCb<mPassPtr>("passPtr")
      .toCb<mPassRef>("passRef")
      .toCb<quasiMember>("nonMember")
      .toCb<quasiMember2>("nonMember2")
      .toCb<mGetter>("getValue")
      .toCb<mSetter>("setValue")
      .toCb<mGetterConst>("getConstValue")
      .toCb<mGetterSetter>("value")
      .s2Ctor<Ctors>()
      ;

    /*
      Setting up the prototype doesn't actually do much - it queues
      everything up for eventually creation via pm.createPrototype().

      Now we set the prototype (and its prototype)...
    */
    myProto = pm.createPrototype<MyType>( PM::DoNotStore );
    if(!myProto) throw std::bad_alloc();
    cwal_value_ref(myProto);
#if 0
    /* This is how we'd bind the ctors if ProtoMaker didn't have s2Ctor()... */
    // And install that dispatcher as the 'new' ctor...
    rc = s2_ctor_callback_set( se, myProto, Ctors::callback );
    if(rc) goto end;
#endif


    /* Notice how we wrote _no_ "real" code (only templates) to
       define how cwal binds to the constructors. */

    /*
      In s2 (but not vanilla cwal), myProto will automatically inherit
      s2's built-in Object class, but if we want to re-assign or
      remove its prototype, that can be done like:

      cwal_value_prototype_set( myProto, s2_prototype_object(se) );

      That particular 2nd argument simply does what is already done
      for us by default. Pass 0 to remove the prototype.
    */

    /*
      End of Part 1 of the prototype setup demo, and we'll pause with
      this note before continuing with the rest of the demo:

      The question of where to store a prototype for use in multiple
      objects is a tricky one in s2, and an ideal/optimal generic
      solution to the problem is being explored. This particular demo
      does not address it directly - the only reference to myProto is
      the one which the caller of this function will presumably add
      after we return it (myProto gets assigned to *rv) way down below
      somewhere).
    */

    /*************************************************************
      For demonstration's sake, we're going to add the rest of the
      bindings using the more C-like approach, but we'll continue to
      use the template conversions to create callbacks for us...
    *************************************************************/

    /*
      Some helper macros which don't really pay off in small modules,
      but historically see lots of copy/paste re-use between modules...

      Note that the error checking/handling done here is typical
      pedantic-level cwal-client-side error checking, primarily for
      OOM cases which simply don't happen in normaly practice. The
      only error cases which can happen in the various binding-related
      calls below, aside from invalid arguments (which we hopefully
      don't do) are OOM errors. Ignore the result codes at your
      own peril: cwal has undefined behaviour if callers ignore
      its error codes (if you ignore it, it ignores you!).
    */
#define CHECKV (void)0
#define SET(KEY) \
    if(!v){ rc = CWAL_RC_OOM; goto end; }                       \
    cwal_value_ref(v);                                          \
    rc = cwal_prop_set( myProto, KEY, cwal_strlen(KEY), v );    \
    cwal_value_unref(v);                                        \
    v = 0;                                                      \
    if(rc) goto end
#define FUNC(NAME,FP)                                   \
    v = cwal::newFunctionValue< FP >( se->e );          \
    SET(NAME)
    
    
    /* Install some arbitrary value... */
    v = cwal_new_integer(se->e, 42);
    SET("theAnswer");

    /*
      Just checking...
    */
    fp = cwal::ToCb< mGetterSetter >::callback;
    fp = cwal::ToCb< cwal::ToCb< mGetterSetter > >::callback;

    /**
       An alternate approach to argument validation/dispatching is to
       use template-generated rules. CbDispatcher<TYPELIST> expects a
       list pf CbRule-type entries which describe (A) an argument
       validation rule and (B) the callback to trigger if the rule
       matches. When called, if no rules in the list match the
       arguments, a cwal-level exception is triggered.

       Rules can be chained via AND and OR combinations, essentially
       to arbitrary complexity: "if 3 args and the 2nd is an integer,
       call ABC, else if 2 args and the 1st is null, call DEF..."

       Here we'll simply create a clone of the MemberAccessor functionality
       (shown above), using predicates (rules) to figure out whether
       to behave like a getter or a setter.
    */
    // Rule #1: if no args are passed in, call it like a Getter:
    typedef cwal::CbRule<
      cwal::Argv_Length<0>,
      cwal::MemberGetter<MyType,int,&MyType::value>
      > predGetter;
    fp = predGetter::callback;
    // Rule #2: always (fallback) assume a Setter:
    typedef cwal::CbRule<
      cwal::Argv_True, // alternately: Argv_Length<1,0>, but that one does more work
      cwal::MemberSetter<MyType,int,&MyType::value>
      > predSetter;
    fp = predSetter::callback;

    /*
      Now combine the above rules into a single callback via...
      
      CbDispatcher expects to get a list of rules as a typelist. The
      order is important: the rules are checked (at runtime) in the
      given order:
    */
    typedef cwal::CbDispatcher<
      CWAL_TYPELIST(( predGetter, predSetter ))
    > mAccessors;

    /*
      Create a cwal callback function from mAccessors (checking
      via several fp assignments first because it triggers template
      errors sooner for what i'm testing):
    */
    fp = mAccessors::callback;
    fp = cwal::ToCb< cwal::CbToCb< mAccessors::callback > >::callback;
    fp = cwal::ToCb< mAccessors >::callback;
    FUNC("value2", mAccessors );    

    /*
      Here's another use of rules: overloading based on near-arbitrary
      rules which get applied at runtime...

      We split this up into its atomic parts (A) to make it easier to
      understand and (B) because emacs, after all these years, still
      can't indent multi-line templates worth a damn.
    */
    using cwal::ToCb;
    using cwal::MethodPtr;
    using cwal::CbRule;
    using cwal::CbDispatcher;
    typedef ToCb< MethodPtr<MyType const, int(int), &MyType::overloadOneInt> > oloadOneInt;
    typedef ToCb< MethodPtr<MyType const, double(double), &MyType::overloadOneDbl> > oloadOneDbl;
    typedef ToCb< MethodPtr<MyType const, int(int,double), &MyType::overloadTwo> > oloadTwo;
    typedef CbRule< // if two args, use oloadTwo
      cwal::Argv_Length<2>,
      oloadTwo
      > rTwo;
    typedef CbRule< // else if first arg is an integer, use oloadOneInt
      cwal::ArgAt_IsInt<0>,
      oloadOneInt
      > rOneInt;
    typedef CbRule< // else if first arg is a double, use oloadOneDbl
      cwal::ArgAt_IsDouble<0>,
      oloadOneDbl
      > rOneDbl;
    typedef CbDispatcher< // Combine the rules into one callback
      CWAL_TYPELIST(( rTwo, rOneInt, rOneDbl ))
    > mOload;
    // And now bind it...
    FUNC("oload", mOload);


    /*
      We're done, but some final notes...

      Reminders to self regarding the lifetime of myProto...

      On the surface it looks like myProto is not safe against being
      vacuumed up because it is not reachable to script code via
      variable references or similar. However, when a value is
      upscoped, its prototype is also upscoped, if necessary, a
      side-effect of which is that a prototype, even if it's not
      directly script-reachable and only referenced via the C APIs[1],
      won't get vacuumed up until/unless the last value which has it
      has a prototype gets cleaned up. That's good, because i was (at
      some point) wondering how the prototype had managed to avoid
      being vacuumed up (it's always sweep-safe because a prototype
      gets a refcount point from anyone who has it has a prototype).

      [1] = The "prototype" property in s2 isn't really a property,
      and is intercepted as needed to interact with the C-level
      prototype-handling APIs.

      In the meantime we've developed a somewhat useful approach for
      storing the prototype, which we will demonstrate here but won't
      use because we already stashed the proto in the 'ns' object...
    */
    if(0){
      cwal_value * ctor = cwal_prop_get(myProto, "__new", 5);
      assert(ctor && "We set this above via pm.s2Ctor()");
      rc = s2_stash_hidden_member( ctor, myProto )
        /* This will keep the prototype alive so long as nobody
           calls cwal_clear_props() on ctor. */
        ;
    }


    if(0){
      /* just testing... */
      S2_UNUSED_VAR MyType * my = 0;
      cwal_value * myV = 0;
      rc = s2_ctor_apply( se, myProto, 0, 0, &myV)
        /* Runs myProto's designated ctor, as if we had called "new"
           from s2, passing it no arguments, and gives us the result
           Value via &myV. */
        ;
      if(rc){
        assert(!myV);
        goto end;
      }
      cwal_value_ref(myV);
      my = cwal::toNative<MyType>(se->e, myV);
      assert(my && "to-native binding is borked");
      assert(my->cwalValue() == myV);
      COUT << "Created a "<<cwal::TypeName<MyType>::Value
           <<" via script-space APIs. "
           << "Testing type conversions...\n";


      /*
        The Native-to-cwal conversions:
      */
      assert( cwal::toCwal<MyType>(se->e, my) == myV );
      assert( cwal::toCwal<MyType*>(se->e, my) == myV );
      assert( cwal::toCwal<MyType const *>(se->e, my) == myV );
      assert( cwal::toCwal<MyType &>(se->e, *my) == myV );
      assert( cwal::toCwal<MyType const &>(se->e, *my) == myV );

      /*
        The cwal-to-Native conversions...

        To run these we have to instantiate an instance via
        script-space (which is what s2_ctor_apply() did for us).
      */
      assert( cwal::toNative<MyType>(se->e, myV) == my );
      assert( cwal::toNative<MyType *>(se->e, myV) == my );
      assert( cwal::toNative<MyType const *>(se->e, myV) == my );
      assert( cwal::toNative<MyType &>(se->e, myV) == *my );
      assert( cwal::toNative<MyType const &>(se->e, myV) == *my );

      if(0){ // finalization, approach #1:
        /* Important: we cannot 'delete my' now because it is potentially
           allocated via cwal, and in any case is pointed to by myV.
           We can delete it via the scripting engine with:
        */
        assert(cwal_value_get_native(myV) && "type id != CWAL_TYPE_NATIVE?");
        cwal_native_clear(cwal_value_get_native(myV), 1);
        /* myV is still alive, living in the script engine, but is
           disconnected from 'my', which was just destroyed. */
        assert(!cwal::toNative<MyType>(se->e, myV) );
        assert(1 == cwal_value_refcount(myV) /* because of our ref above */);
        cwal_value_unref(myV);
        /* And now myV is destroyed, too. */
      }else{ // finalization, approach #2:
        // Using approach #1, myV is still alive after we call cwal_native_clear().
        // Instead, let's just let go of our reference count point, then both
        // myV and 'my' will be cleaned up when myV is garbage collected:
        COUT << "Expect a "<<cwal::TypeName<MyType>::Value<<" dtor call here...\n";
        cwal_value_unref(myV);
        // That was much easier, but sometimes "manual destruction" of the
        // native part is necessary.
      }
    }/* /just testing */

#undef SET
#undef FUNC
    end:
    (void)0/*avoid illegal empty goto*/;
  }catch(std::bad_alloc const &){
    rc = CWAL_RC_OOM;
  }catch(std::exception const & ex){
    rc = cwal_exception_setf(se->e, CWAL_RC_EXCEPTION,
                             "Caught std::exception: %s",
                             ex.what());
  }catch(...){
    rc = cwal_exception_setf(se->e, CWAL_RC_EXCEPTION,
                             "Caught unknown C++ exception.");
  }
  if(rc){
    cwal_value_unref(myProto);
  }else{
    assert(myProto);
    cwal_value_unhand(myProto);
    *rv = myProto;
  }
  return rc;
}



/**
   Install the symbol needed for making this module loadable by
   s2_module_load() and friends...
*/
S2_MODULE_REGISTER_(sample_cpp);


#undef THIS_MY
#undef MARKER
