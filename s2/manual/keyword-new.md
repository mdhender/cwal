# s2: The new Keyword
#### ([&#x2b11;Table of Contents](./))
# It's a Brand `new Car()`!

Jump to...

* Meet [the new keyword](#keyword-new) (the same as the old keyword)!
    * [Changing a class's `typeinfo(name)`](#keyword-new-typename)
* [Post-new Initialization](#keyword-new-post-init)
* [Anonymous Classes](#keyword-new-anonymous )

<a id="keyword-new"></a>
# How to Use Your `new` Toy...

(The `new` keyword was added to the trunk on 20160121.)

Most object-oriented languages provide a language-level mechanism for
creating new instances of client-defined types. s2, on the other hand,
has historically (with the exception of object/array/hash literals)
required that clients provide a factory function for their own types.
Given s2's type system, both approaches are viable, but the `new`
keyword was (eventually) added to help simplify script code usage by
providing clients with a common approach to creating new instances,
rather than each defining their own factory interfaces. This keyword
superficially works similarly to JavaScript's `new` operator, but it
also mixes in a bit of PHP-ness and has a quirk or two of its own which
are side-effects of s2's prototypal class system.

Abstractly speaking, `new`'s syntax is:

```s2-member
new Constructible(arguments…)
```

where the requirements and conventions for the "Constructible" part are
described below. The arguments are processed just like any other
function call arguments and get passed to the Constructible.

In script code, using `new` looks more or less like:

```s2
const MyType = {
  __new : function(...){...}
  // … other methods/properties …
};

var my = new MyType(1,2,3);
assert my inherits MyType;
```

Where *MyType* is a container type which implements an interface used by
the `new` keyword: it must have a function property named `__new`.

A function conforming to that criteria is called a "constructor"
function throughout this documentation. Such a type is said to be a
"Constructible" type. If `new` is called on a value with no valid
constructor (i.e. it is-not-a Constructible), an exception is
thrown. [`typeinfo(cannew EXPR)`](keyword-typeinfo.md) can be
used to figure out if calling `new` on a given operand is legal
and `typeinfo(isnewing)` can tell the constructor function whether
it's really being invoked via `new`, as opposed to being called
"as a normal function".

Constructors are not inherited. That is, `new T()` only works if `T`
itself has a constructor. If an object `X` sets `T` as its prototype,
`new X()` *will not work* unless it defines its own constructor. There
is currently no mechanism for passing constructor calls up to parent
constructors. (When it's needed, it will be added.)

When the Constructible is called in the context of `new`, the following
happens:

-   s2 creates a new plain Object and sets its prototype to `new`'s
    operand.
-   It calls the constructor function, applying the new Object as the
    local `this` and passing on any arguments which were passed to
    `new`.

In the constructor call, `this` will be the newly-created Object and is
the default result of the `new` call unless the constructor explicitly
returns a non-`undefined` value. However, it is often desirable or
necessary to use a different base type for new instances (in particular
when deriving from C-native types). s2 cannot know, at the time the
constructor is called, which "this" type you want, but it provides an
approach which allows clients to tell it that:

-   If a constructor does not explicitly return, or returns `undefined`,
    the newly-created object mentioned above is the result of the `new`
    call. Else…
-   If the constructor *explicitly returns* a non-`undefined` value,
    that value is used as the result of the call to `new`.

In the latter case case:

-   The `this` created by `new` for the constructor call *is discarded*
    when the constructor returns (and is immediately reaped by garbage
    collection[^28] unless the constructor's body stashes a references
    to it).
-   The constructor has the responsibility of properly setting the
    returned value's prototype, if needed (it normally, but not always,
    will be needed). Examples are provided below.
-   The result of the constructor's *explicit return* will become the
    result of the `new` operation. Exception: returning `undefined` is
    equivalent to *not* explicitly returning[^29], so the
    default-created object would be the result instead.

Here are examples which demonstrate the different approaches…

The most basic case is that the client wants to have Object-typed
results:

```s2
const MyClass = {
  __new: function(x,y){
    // this === a new Object.
    // *this.prototype* is the containing Object (===MyClass)
    this.x = x;
    this.y = y;
    // no explicit return is necessary:
    // Without a return, it behaves as if (return this) had been called.
  }
};

assert typeinfo(cannew MyClass);
assert new MyClass() inherits MyClass;
```

Be careful not to reference the *MyClass* symbol in such a constructor
if that symbol is currently being declared (i.e. in the context of
the `var`/`const` RHS): it won't exist until after the outer
(being-defined) object finishes evaluating. Also remember that [s2's
symbol resolution rules](symbol-resolution.md) mean that the symbolic
name "MyClass" might not still be in scope when the constructor is
run. i.e. don't ever rely on the prototype's symbolic name (the "class
name"), if any.

If we want (or need) our new type to behave like Arrays, we can do the
following:

```s2
const MyArray = {
  prototype: [].prototype, // inherit all array methods
  __new: function(){
    // *this* === a new Object, but we don't want that, so we need to…
    // (1) create our result value:
    const rc = [];
    // (2) set its prototype to our new "class":
    rc.prototype = this.prototype; // === MyArray
    // (3) perform any constructor-related work:
    // Here we simply clone the arguments:
    foreach(@argv=>v) rc[]=v;
    // (4) return our new value to replace the Object s2 created for us:
    return rc; // "new" will use rc as its result
  }
 // … add any new array-like methods …
};

assert typeinfo(cannew MyClass);
var a = new MyArray(1,2,3);
assert a inherits MyArray;
assert typeinfo(isarray a);
assert 3 === a.2;
```

Note that even though MyArray inherits from the Array prototype,
that's not *really* what makes it an array: we still had to create a
new array instance in the constructor, set its prototype to our
"class," and return it so that `new MyClass()` would resolve to that
array. Had we not reassigned `rc.prototype`, the returned value would
not derive from MyArray. Had we not returned an Array instance, all
instances of `new MyArray()` would not *really* be arrays at all, they
would simply inherit the Array API (which wouldn't work for them
because they wouldn't have a native-level array instance in their
prototype chain).  Returning an array, after reassigning its prototype
to MyArray, means each MyArray instance is-an Array *and* can behave
like one.

This swap-prototype-and-return approach is especially important when
subclassing client-bound "native" types, so that the constructor can
return a client-specified type.

More examples can be found in the
[new keyword's unit test script](/finfo/s2/unit/400-000-new.s2)
and some "stupid s2 tricks" using the *new* keyword can be found
[in this post](/technote/22d7126c128d7ac3f0685b7b7553139dc2b66310).

<a id="keyword-new-typename"></a>
## Changing a class's `typeinfo(name)`

To change the name that `typeinfo(name anInstance)` reports for
the prototype or instances of a class, simply assign the `__typename`
property of the prototype to the desired name:

```s2
const MyClass = {
    __typename: __FLC,
    ...
};
```

Any given instance may override that property, if desired. If the value is
not a string then it may be ignored in contexts which require the type's name
to be a string.


<a id="keyword-new-post-init"></a>
# Post-new Initialization

Similarly to Java, the `new` keyword allows a {code block} to follow the
constructor call, and this block is run after the constructor completes,
as if it were part of a member function taking no arguments and
returning `this`:

```s2
new T(...) {
 // this === the new T instance (as returned by the ctor).
 // 'return' is not allowed: this is not a function call!
 // It's a scope, but any scope-level result value is discarded
 // (cleaned up with the scope).
}
```

Note that s2 uses a single pair of braces, though, not Java's `{{…}}`
because adding those would require adding new token types (i.e. it
would be more work just for the sake of looking like Java).

The above is functionally equivalent to, but more efficient than:

```s2
new T(...).withThis(proc(){
 // this === the new T instance.
 // 'return' *is* allowed: we're in a callback function now.
 // Unless we return a different (non-undefined) value,
 // the result of this callback is the same as the T constructor.
});
```

With two microscopic caveats: `withThis()` could potentially return a
different value than the constructor did or (hypothetically) recurse
into its own callback, whereas the post-new init block can do neither of
those. A conceivable valid use case for such recursion is difficult to
envision, but it can be done.

Note that `new` stops evaluating at the end of the parameter list or
post-constructor initializer block, so it is legal to call methods on
the returned object directly from `new`'s result: e.g. `new T().doFoo()`.

Because constructors can return any type (except `undefined`, which is
indistinguishable from not explicitly returning and is used to signal
the default behaviour), the following is also legal, but is neither
efficient nor terribly useful:

```s2
const MyBoolean = {
  __new: function(v=throw "MyBoolean ctor requires an argument"/*[^31]*/){
    return !!v; // coerce v to a boolean value and return it
  }
};
assert true === new MyBoolean(0 < 1);
assert false === new MyBoolean(0 > 1);
assert catch {new MyBoolean()}.message.indexOf("MyBoolean ctor") === 0;
```

On the other hand, that could be used to implement singletons by
returning a shared instance on each call:

```s2
const MyType = {
  __new: function(){
    instance.prototype || (instance.prototype = this.prototype);
    return instance;
  } using {instance: { prototype:undefined, … }}
};
```

Okay… so getting the prototype instance there is a bit tricky (because
MyType is still being constructed, and is not yet in scope, when `using`
resp. `importSymbols()` is called), but a slight reformulation works
around that while also nicely demonstrating one of those esoteric uses
for `Object.withThis()`…

```s2
const MyType = {
  __new: function(){
    return instance;
  }
}.withThis(proc(){
  this.__new.importSymbols({
   instance: { prototype: this /* ha! */, …}
  });
});
```

Some of the high-level built-in s2 types implement the `__new`
method[^32] and will behave intuitively:

```s2
var b = new s2.Buffer( … );
var h = new s2.Hash( … );
var p = new s2.PathFinder( … );
```

These classes also have factory methods called "new" (because s2 didn't
always have the `new` keyword), but those do the exact same thing.

Objects, arrays, and strings do not have constructors because there's
simply no need for them. Literals are more efficient and are (IMO) just
as readable as, e.g. (*new Object()*). Feel free to make your own
wrappers, as demonstrated in
[`new`'s unit test script](/finfo/s2/unit/400-000-new.s2).

<a id="keyword-new-anonymous"></a>
# Implementing Anonymous Classes

Before we go, here's how to implement "anonymous classes" via `new`:

```s2
var x = new { __new: proc(){...} }( ctor args… );
```

i.e. the operand to `new` does not need to be an identifier, but can be
an arbitrary expression (parens might be needed around it, depending on
the construct). Which means that silliness like the following actually
works…

```s2
s2sh> new new {__new:proc(){print("from C ctor:",argv); return \[^1]
  {__new:proc(){print("from internal ctor:",argv)}}}}(1,2,3)(4,5,5)
from C ctor: [1, 2, 3]
from internal ctor: [4, 5, 5]

s2sh> new while(1){ break new {__new:proc(){print("from Cctor:",argv); \
  return {__new:proc(){print("from internal ctor:",argv)}}}}(1,2,3)}(4,5,6)
from C ctor: [1, 2, 3]
from internal ctor: [4, 5, 6]
```

Don't try that at home, though. It's rough on the sanity, long-term.

# Footnotes

[^1]: Pedantic note: the backslash there was added for formatting this
    page: s2sh does not support continuing lines that way.

[^28]:  What happens when we run (new MyType()) 1000 times in a loop and
    don't propagate the result out of the loop? The temporary/discarded
    "this" object gets recycled for every loop iteration after the
    first. Even the first iteration might have come from the recycling
    subsystem. i.e. it's very cheap in the aggregate.

[^29]:  s2 currently does not internally differentiate between not
    returning vs. returning undefined because for script purposes there
    has never been a reason to differentiate them (doing so would
    require new script-side semantics).

(Reminder to self: footnote #30 was obsoleted by refactoring while
porting this document from GDocs.)

[^31]:  Yes, that's perfectly legal. A Happy Accident of the design.

[^32]:  As a hidden property, incidentally, but that's not really
    significant.
