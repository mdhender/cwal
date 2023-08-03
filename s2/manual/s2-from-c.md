# s2: Embedding and Extending s2 from C/C++
#### ([&#x2b11;Table of Contents](./))
# Embedding and Extending s2 from C/C++

Jump to...

* [Introduction](#s2-from-c-intro)
    * [cwal Setup](#s2-from-c-cwal-setup)
    * [Core Concept: What is a Value?](#s2-from-c-value)
* [Binding Functions](#s2-from-c-functions)
* [Custom Native Wrappers](#s2-from-c-native-wrappers)
    * [Rescopers](#s2-from-c-rescopers)
* [Simplify Bindings with C++ Template Magic](#s2-from-c-plusplus)

<a id="s2-from-c-intro"></a>
# Introduction

This chapter introduces the process of binding client-side C and C++
code with s2/cwal.

s2, the scripting language, is built upon a language-independent
scripting engine called [cwal](/). cwal provides the base Value and
memory management system, but knows nothing at all about any script
language. Client-side bindings mostly concern themselves with the core
scripting engine (cwal) rather than the concrete language (s2), so
these docs refer far more to the former than the latter.

The first rule of binding client code to cwal is: *never, ever (ever!
(ever ever!!)) let a C++ exception pass through the s2/cwal C
APIs*. ***NEVER!*** Doing so *will* corrupt s2's internal
state. Likewise, do not evaluate script code in the engine from an
interrupt handler, as that will almost certainly (depending on many
factors) also corrupt its internal state.

<a id="s2-from-c-cwal-setup"></a>
## cwal Setup

Because s2 cannot know how a given client would like his
cwal-level engine configured, setting up s2 unfortunately requires a
bit of bootstrapping to configure a cwal engine instance. First a cwal
instance has to be configured, which includes an allocator,
configuration of the output stream and allocation/recycling
strategies, and a number of other flags which have to be set up before
it is handed over to s2. The s2 engine is then given the cwal
instance, taking over ownership of it. As of that point they are
inseparable: cwal provides the basic script-engine services s2 uses
for the creation and management of memory (mostly abstract Values),
including (if needed), the memory to allocate the s2 engine itself.
(That said, the cwal engine and s2 engine are normally allocated on
the stack - only internals need to be dynamically allocated. The
engines both may optionally be dynamically allocated, but in practice
never are except for testing purposes.)

The following apps all demonstrate that setup in full:

-   [cwal's `test.c`](/finfo/test.c) demonstrates standalone cwal
    setup, independent of s2.
-   [s2's `test.c`](/finfo/s2/test.c) is the "original" s2 test app,
    from the time long before s2sh could run tests. It's still used as
    a sanity-checking test, and it provides the simplest (though not
    best-documented) demonstration of setting up an s2 engine.
-   [s2's `shell.c`](/finfo/s2/shell.c) is the main source code for
    [s2sh](s2sh.md) and demonstrates, in gross detail, the
    "real-world" setup of a cwal/s2 pair and the surrounding
    infrastructure.

Those, in particular s2's `shell.c`, demonstrate everything there is
to know about setting up cwal and/or s2 from a client-side
perspective. (That said, it is recommended that interested clients
inspect those files in the order they are listed above, i.e. from most
basic to most advanced.)

<a id="s2-from-c-value"></a>
## Core Concept: What is a Value?

cwal's base-most abstract, script-side data type is called
`cwal_value`, which the documentation abstractly refers to as
Value. It's an opaque type which acts as a handle for one of the
higher-level types, be it an [integer](type-number.md) or an
[array](type-array.md) or a [buffer](type-buffer.md)[^1]. Given a
Value handle, cwal can access its higher-level representation, and
vice versa. For example:

```c
cwal_engine * e = ... /* the app's cwal context (typically 1 per
    app). All APIs which may need to allocate memory, and most which
    may need to free it, take a cwal_engine as their first argument,
    as that type manages *ALL* memory in cwal/s2. The exceptions to
    this pattern are functions from which cwal can derive the
    cwal_engine instance from other arguments.
*/;
cwal_array * a = cwal_new_array( e );
if(!a) return CWAL_RC_OOM;
cwal_value * v = cwal_array_value(a);
// The following hold:
assert( a == cwal_value_get_array(v) );
assert( NULL == cwal_value_get_object(v) );
assert( NULL == cwal_value_get_string(v) );
```

When this documentation refers to a Value, it means a `cwal_value`
or one of its associated higher-level types.


<a id="s2-from-c-functions"></a>
# Binding Native Functions

cwal's primary interface with client code is the `cwal_callback_f()`
interface, which looks like:

```c
int (*)( cwal_callback_args const * args, cwal_value ** resultValue );
```

This is the only interface cwal uses to bind native-side functions with
script-side functions, and any cwal bindings are likely to have many
implementations of this interface (one per script-bound function).

Such callbacks return 0 on success, with non-0 (see below) triggering
an error in the interpreter. The `args` parameter holds the argument
count and list, the `this` value, the callee (the function being
called), and a reference to the underlying cwal engine (required in
many contexts). Callbacks set their script-side result value (if any)
in the `resultValue` parameter (defaulting, in script code, to the
`undefined` value if they provide no result or assign
`*resultValue=NULL`). If they return non-0, any `resultValue` is
ignored, but practice suggests that implementations should not assign
`*resultValue` except on success. The vast majority of
implementations never need concern themselves with the intricacies of
Value lifetimes, reference counts, and whatnot.

***ACHTUNG:*** non-0 values returned from callbacks *MUST* either be
(A) from the `CWAL_RC_xxx` or `CWAL_SCR_xxx` family of codes or (B) be
*guaranteed* (by the user) not to collide with any such cwal-level
result code, or results are *undefined* (as in *Undefined Behaviour*,
not the s2 `undefined` value!). Why? Because many of the `CWAL_xxx`
codes are interpreted specially by cwal and/or s2. For example,
`CWAL_RC_EXCEPTION` means an exception was thrown (the `catch` keyword
looks for this and expects to find an exception stored in the engine),
and `CWAL_RC_OOM` means out of memory (a fatal error which ends script
execution and is propagated back to the C-level caller). Returning a
non-cwal result code which collides with a cwal code will (at some
point) confuse cwal or s2. *As a general rule, callbacks intended for
execution from script-space must return 0, `CWAL_RC_EXCEPTION`, or
`CWAL_RC_OOM`,* as any other result codes are likely to be interpreted
as fatal to the script. Other non-fatal codes, e.g. `CWAL_RC_MISUSE`
or `CWAL_RC_RANGE`, can be wrapped up in an exception, if needed,
using `cwal_exception_setf()` or one of its relatives,
e.g. `s2_cb_throw()`.

We refer the user to [the example loadable
module](/finfo/s2/mod/sample/sample_module.c) for several documented
examples of creating and binding callback functions, as well as
complete error handling.

That's about it for the intro. [The example loadable
module](/finfo/s2/mod/sample/sample_module.c),
along with [its demo script](/finfo/s2/mod/sample/sample_module.s2),
exist to provide a documented introduction to the topic, and interested
readers are referred there for more details.


<a id="s2-from-c-native-wrappers"></a>
# Custom Native Wrappers

cwal supports a mechanism, called "native" values, for binding custom
client-side C/C++ values to Value instances. This mechanism is
type-safe, in that clients can safely determine whether or not a given
Value really holds a native of they type they are expecting. It allows
the client to provide a finalizer function which gets called when the
Value gets cleaned up, and it also provides a mechanism for clients to
cleanly disconnect the native instance from its cwal-side value
(optionally calling its finalizer or not). It allows clients to bind
both dynamically-allocated and stack-allocated native values to the
engine, provided they use valid combinations of finalizers (resp. none
at all) and cleanup logic.

This section provides an overview of how it works and how to use it,
with a small amount of example code. Full-fledged code can be found in
the s2 source tree, most notably in the [PathFinder class'
implementation](/finfo?name=s2/pf.c) and the [example loadable
module](/finfo/s2/mod/sample/sample_module.c), both of which
demonstrates the complete native-binding process.

The first thing we need is a native type to bind to cwal. Let's start
with this:

```c
typedef struct {
 int value;
 // whatever other members we need...
} MyNative;
```

Next, we need (for the common case, anyway) a finalizer function which
implements the `cwal_finalizer_f()` interface. It looks like:

```c
static void MyNative_finalizer( cwal_engine * e, void * mem ){
 MyNative * my = (MyNative*)mem;
 // … do any type-specific cleanup here …
 cwal_free( e, mem )
   /* *ONLY* if mem was allocated using
      cwal_malloc()/cwal_realloc() */;
}
```

Note that not all bindings need a finalizer. Bindings to static
resources, or those with lifetimes outside of (and longer than) s2's
interpreter, often don't need a finalizer.

Normally (because i'm pedantic like that) we've also got an allocator
function which initializes the native's memory to any defaults we
need, but for simple cases, direct calls to `cwal_malloc()` and
`memset()` suffices. Client code is not strictly required to use
cwal's allocation APIs for its own memory, but doing so allows it to
partake in cwal's memory recycling. In any case, memory allocated via
cwal, via `cwal_malloc()` or `cwal_realloc()`, must be freed using
`cwal_free()` or `cwal_free2()` (the latter is an *optional*
optimization which is useful only if the caller knows the *exact* size
of the to-be-freed memory).

Lastly, so that cwal can provide type safety for us, we need an
arbitrary static constant internal pointer address to use as a type
identifier tag. Any internal value we can get a `void const *` pointer
to will suffice.  Here's one:

```c
static const int MyNative_TypeId = 0
  /* the value is 100% irrelevant, we use only its address */;
```

There are other useful utilities we'll want to add once everything is
up and running, but with the above bits in place, we can now bind a
MyNative instance to cwal resp. s2.

Creating new native Values is done using `cwal_new_native()` and
`cwal_new_native_value()` (which does the same thing but returns a
Value handle instead of a Native handle; given one of those two we can
always derive the other). The exact context of such setup is
binding-dependent, but abstractly it looks like the following (including
error handling):

```c
cwal_engine * e = …your local cwal_engine instance…;
MyNative * my = 0; // The "Native" part of the binding
cwal_value * vMy = 0; // The "Value" part of the binding

// First create the client-side half of the native:
my = (MyNative*)cwal_malloc( e, sizeof(MyNative) )/* [^53] */;
if(!my) return CWAL_RC_OOM
  /*    ^^^^^^^^^^^^^^^^^^
    *IMPORTANT*: all cwal/s2 contexts *require* CWAL_RC_xxx
    or S2_RC_xxx codes! Passing code from outside that
    range will cause Grief.
  */;
memset( my, 0, sizeof(MyNative) ); // or proper init code
// Now create the cwal half of the native:
vMy = cwal_new_native_value(e, my, MyNative_finalizer,
                            &MyNative_TypeId);
if(!vMy){ // Alloc failure: clean up...
 MyNative_finalizer(e, my);
 return CWAL_RC_OOM;
}
// As of here, the following assertions hold:
assert(
    my == cwal_native_get(
        cwal_value_get_native(vMy),
        &MyNative_TypeId
        /* ^^^ this part is what makes this operation
          type-safe. If we pass cwal_native_get() a second
          argument which does not match the one we
          passed to cwal_new_native_value(), or if we
          pass a non-MyNative Value instance as the first
          argument, cwal_native_get() will return NULL.
        */
    )
);
assert(
    my == cwal_native_get(
        cwal_value_native_part(e, vMy, &MyNative_TypeId),
        &MyNative_TypeId
        // ^^^ again, these (&MyNative_TypeId) type-id
        // flags are what make this type-safe.
    )
);
```

A complete example of such initialization can be found in the
`s2_pf_new()` function in [the PathFinder code](/finfo/s2/s2_pf.c).

Normally a value will be cleaned up, and its finalizer (if any)
called, by the lifetime management system. For client-side types it is
often necessary (or helpful) to add "explicit finalizers" in order to
guaranty proper behavior in the C parts of the bindings, in particular
when there are relationships between multiple parts which have both C-
and script-side relationships. The canonical example is a database
which creates prepared statement handles. It is normally necessary,
for proper behaviour of the DB driver, to destroy the prepared
statement handles before destroying the DB connection which created
them. While the full intricacies of such lifetime management are
beyond the scope of this introduction, we will say this: the basis of
writing such "manual finalizers" is the `cwal_native_clear()`
function, which disassociates a Value from its Native part and
optionally calls its native finalizer (if any). The Value itself may
still be visible in script-space, but any attempt to extract the
`MyNative` part from it will result in a `NULL` pointer, which
bindings must check to avoid dereferencing it. An example of how this
is used can be seen in [the POSIX regex
module](/finfo/s2/mod/regex_posix/mod_porex.c), some version of which
is demonstrated here:

```c
/* helper macro for use in cwal_callback_f() bindings */
#define THIS_REGEX \
    cwal_native * nself = cwal_value_get_native(args->self);    \
    regex_t * self = \
        nself  ? (regex_t *)cwal_native_get(nself, &s2porex_typeid) : NULL; \
    if(!self) return s2_cb_throw(args, CWAL_RC_TYPE,                      \
                                 "'this' is not (or is no longer) "\
                                 "a regex_t instance.")

/* cwal_finalizer_f() impl. for POSIX regex objects. */
static void s2porex_finalizer( cwal_engine * e, void * v ){
  regfree((regex_t *)v);
  cwal_free2(e, v, (cwal_size_t)sizeof(regex_t));
}

/* "Manual destructor" cwal_callback_f() binding. */
static int s2porex_cb_destroy( cwal_callback_args const * args,
                               cwal_value **rv ){
    THIS_REGEX;
    cwal_native_clear( nself, 1 )
      /* a truthy 2nd argument means to call the native finalizer */;
    *rv = cwal_value_undefined();
    return 0;
}
```

The `THIS_REGEX` macro seen there for "extracting" the native value
from the callback arguments is a common sight in client-side native
bindings, but is difficult to encapsulate directly into s2 because it
requires type-specific knowledge and potentially type-specific
handling, so must be implemented client-side.


<a id="s2-from-c-rescopers"></a>
## Rescopers

One element missing from the examples above is a so-called
"rescoper". A rescoper is a callback used by cwal to tell a native
value that the value is undergoing a re-scope and that the native must
(if appropriate) re-scope any (`cwal_value*`) members it's personally
managing (as opposed to values which are held as, e.g. object-level
properties (which are rescoped by the core library)). This is
necessary only when natives hold `cwal_value` pointers, or pointers to
one of the concrete Value-derived types, directly, outside of some
other lifetime-management construct, and not all natives do
this. There are several examples to be found in the sources for the
[s2 loadable modules](/dir/s2/mod?ci=trunk). For a concrete example,
search [mod\_sqlite3.c](/finfo/s2/mod/sqlite3/mod_sqlite3.c) for the text
`rescoper_f_`. As of this writing, that function looks like (edited
slightly for readability and clarity):

```c
static int
cwal_value_rescoper_f_s2x_sq3( cwal_scope * s, cwal_value * v ){
  cwal_native * n =
      cwal_value_native_part( s->e, v, &cwal_type_id_s2x_sq3 );
  s2x_sq3 * db =
      (s2x_sq3 *)cwal_native_get( n, &cwal_type_id_s2x_sq3 );
  int rc = 0;
  assert(n);
  assert(db);
  if(db->udfStore){
    rc = cwal_value_rescope(s, db->udfStore);
  }
  return rc;
}
```

That function implements the `cwal_value_rescoper_f()` interface in order
to rescope the binding's `udfStore` member, a `cwal_value*`.

Search that file for that function name to see how the rescoper gets
installed.

<a id="s2-from-c-plusplus"></a>
# Simplify Bindings with C++ Template Magic


The file
[`cwal_convert.hpp`](/finfo/s2/cwal_convert.hpp)
defines a header-only[^54], C++98-compatible, template-based mechanism
for converting between cwal value types and "native" C++ types. In
principle it can be "taught" (via template specializations) to convert
any types to which we can apply reasonable conversion semantics. Notably
excluded is `T **`, which has multiple possible semantics, and we have
to be careful with conversions from `T*` to ensure we aren't leaking
memory. This mechanism is derived from earlier projects implemented for
the Google V8 and Mozilla SpiderMonkey JavaScript engines, and is known
to work very well in practice.

By itself, the type conversion APIs can simplify implementing bindings
in C++ *somewhat*. The API takes it a step further, however, and
provides template APIs to convert free *functions*, *class methods*, and
*constructors* to cwal callback functions. Then it goes one *little*
step further to implement (via templates) getter and/or setter function
bindings to non-function class members. These features can easily save
hundreds of lines of code (compared to native C) in any moderately-sized
script bindings.

The file [sample C++ module](/dir/s2/mod/sample_cpp?ci=trunk) fully
demonstrates how to use API and contains copious amounts of
documentation. Here are a few examples of the type conversions from
that code:

```cpp
cwal_engine * e = /* … the app's cwal context */;
cwal_callback_f cb; // the cwal callback function interface
cwal_function * f; // high-level representation of a Function value
cwal_value * fv; // cwal Value representation of a Function/Object/whatever

// Each of the next three statements converts one
// function or method into a script-bindable equivalent:

cwal_callback_f freeFunc =
  ToCb< FunctionPtr<unsigned (unsigned), ::sleep> >::callback;

cwal_callback_f nonConstMethod =
  ToCb< MethodPtr<T, int(), &T::foo> >::callback;

cwal_callback_f constMethod =
  ToCb< MethodPtr<T const, int(), &T::bar> >::callback;

// Creating non-method function bindings: bind sleep(2):

typedef cwal::FunctionPtr<unsigned (unsigned), ::sleep> tSleep;
cb = cwal::ToCb< tSleep >::callback; // that was *easy*!

// or:
f = cwal::newFunction< tSleep >(e);
fv = cwal_function_value(f);

// or:
fv = cwal::newFunctionValue< tSleep >(e);
f = cwal_value_get_function(fv);

// Creating [const] method bindings requires teaching
// (via template specializations) the API how to
// convert one's own types, and enables the following:

typedef cwal::MethodPtr<
    MyType const,
    int(int), &MyType::func1c
> mFunc;
cb = cwal::ToCb< mFunc >::callback;

// or:
f = cwal::newFunction< mFunc >(e);

// or:
fv = cwal::newFunctionValue< mFunc >(e);
```

> Sidebar: doing the equivalent in C requires *at least* 8-10x as much
code (maybe 15-20x), adds a non-trivial amount of test effort, and
leaves lots of room for "manual error," making this API an attractive
approach even for applications which are otherwise (except for the
bindings layer) pure C. If s2 is relegated to use as a testing tool
within a C-only project's source tree, never intended to be distributed,
it may indeed make sense to use C++ for the bindings solely for the sake
of the time savings. (That said, using them properly requires a fair
level of proficiency with C++ templates, and deciphering compilation
errors for such templates takes some practice.)

There are various template options to control whether or not result
values of native functions get converted back to cwal (some cannot be,
either due to non-convertible types or because of semantic
discrepancies), as well was whether or not to convert C++ exceptions to
script exceptions (this is normally desired, but may not be
needed/desired when going through multiple levels of proxies or when one
knows that a given binding cannot throw, simply to avoid the try/catch
overhead). When in doubt, leave "exception conversion" on, to avoid the
possibility that a C++ exception propagates through the C API (which
will, at the very least, corrupt C-level lifetimes/memory).

Similar templates can be used to create getter/setter routines
for member properties.

Interested users can experiment with those example bindings via s2sh:

```bash
s2sh> var my = s2.loadModule('./sample_cpp.so')
sample_cpp.cpp:31:MyType(): MyType@0x1612380::MyType()
result: native@0x16123a0[scope=#1@0x7fffbd5246e8 ref#=2] ==> native@0x16123a0
s2sh> my.value()
result: integer@0x686601[scope=#0@(nil) ref#=0] ==> 1
s2sh> my.value(2)
result: native@0x16123a0[scope=#1@0x7fffbd5246e8 ref#=1] ==> native@0x16123a0
s2sh> my.value()
result: integer@0x1607340[scope=#1@0x7fffbd5246e8 ref#=0] ==> 2
s2sh> my.value(3).value()
result: integer@0x1607340[scope=#1@0x7fffbd5246e8 ref#=0] ==> 3
s2sh> my.funcStr("via std::string conversion")
sample_cpp.cpp:54:funcStr(): MyType@0x1612380::funcStr(via std::string conversion)
result: double@0x1607340[scope=#1@0x7fffbd5246e8 ref#=0] ==> 3.0
```

The `value()` function is mapped directly to a non-function member, and
a template creates a combination getter-setter routine for us. If called
with an argument, it converts and sets the value, then returns `this`
(which these templates then convert from a native C++ type),
otherwise it converts and returns the member's value. The conversions
layer is relatively powerful, and has no problems converting, e.g. `T
const &`, provided there is a legal conversion to `T*` (which the
reference conversions use as their basis) or the client has "trained" it
(via template specializations) to do the conversion for this type. When
using the default cwal-to-native-reference type conversions most of them
will trigger an exception if they cannot perform the conversion (the
alternative would be stepping on an invalid reference). Some common
cases, like `std::string const &`, are handled via template
specializations which try to behave intuitively, insofar as
cross-script/native semantics allow for.


# Footnotes

[^1]: There is one exception: the so-called "unique" data type, which
    s2 uses for [enum](type-enum.md) entries, has no higher-level
    representation.

[^53]:  Tip: if the memory's size is static, prefer `cwal_free2()` when
    freeing it, to allow the core to recycle that memory better.

[^54]:  There's a second, script-generated header as well, which
    implements most of the N-ary templates.
