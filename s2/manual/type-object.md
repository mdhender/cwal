# s2: The Object Type
#### ([&#x2b11;Table of Contents](./))
# The Object Type

Jump to...

* [Objects](#type-object)
  * [Object Literals](#type-object-literals)
* [Object Methods](#type-object-methods)
* [What Should We do withThis()?](#type-object-withthis)
* [Pattern: Overriding Properties](#type-object-plain-prop-store)

<a id="type-object"></a>
# The Object Type

The Object class is, as in JavaScript, the base-most type for most
other types. Objects are used a general-purposes key/value stores, and
(unlike JavaScript) are capable of holding properties with keys and
values of (almost) any data type. Their property ordering is
unspecified - never rely on it (it may even change at runtime).

## Property Storage Modes

How exactly their properties are stored, and the resulting
performance, is a compile-time build option for the underlying cwal
library:

- By default they use a doubly-linked list of key/value pairs, sorted
to reduce average lookup time to approximately O(N), where N is *half*
the number of properties, with an O(N=prototype count) component when
prototypes must be searched for properties. This option is the default
for historical/compatibility reasons, because it's as memory-light as
it gets, and its performance has proven to be completely adequate for
the scale of scripts s2 targets.

- As of 2021-07-12: if the library is compiled with the
`CWAL_OBASE_ISA_HASH` preprocessor flag set to a true value then the
object properties internally use a hashtable for property storage, so
property access speed is amortized O(1), with an O(N=prototype count)
component when prototypes must be searched for properties. This
ostensibly requires considerably more memory for propery storage, both
in terms of allocation counts and the size of the hashtable
(re)allocations, but when cwal's various memory recycling options are
enabled these costs are cut to a near-negligible point (basic tests
show an increase of about 5% in both the number of allocations and
total allocated memory). Once this option has proven to be as reliable
as the historical one, this will likely be made the default.
However...

  - ***WARNING***: if hash-based storage is enabled then property
  lookup in objects *necessarily becomes type-strict*. That is, the
  integer `1` and string `'1'` will *not* compare as equivalent
  keys. In the classical/legacy storage approach, those two keys
  compare as equivalent. Using two or more such keys will work fine
  but may lead to confusion in JSON output, as demonstrated in the
  following example... \  
\  
```s2
// Example of type-strict keys and how they may confuse JSON output:
s2sh2> {1: 'abc', 2:'def', '1':'ghi'}
result: object@0x55ebe26a0760[scope=#1 ref#=0] ==> {
  "1": "abc", // JSON output stringifies int-type keys
  "2": "def",
  "1": "ghi"  // This key is natively a string.
}
// The same thing in legacy mode, where '1' and 1 are equivalent keys:
s2sh2> {1: 'abc', 2:'def', '1':'ghi'}
result: object@0x556404551980[scope=#1 ref#=0] ==> {
  "1": "ghi", // This '1' overwrote the integer 1
  "2": "def"
}
```

Which mode of operation can, if needed, be determined at runtime using
[`pragma(build-opt CWAL_OBASE_ISA_HASH)`](./keyword-pragma.md#pragma-build-opt):

```s2
if(pragma(build-opt CWAL_OBASE_ISA_HASH)) { ... hash mode mode ... }
else { ... legacy mode ... }
```

Sidebar: the property storage mode is compile-time, rather than
runtime, because the hash-based variant increases the C-level
`sizeof()` of each instance of each container-type value by
approximately 24 bytes. As of this writing (2021-07-29), the plan is
to retain both methods of property storage for the foreseeable
future. i.e. the legacy mode is not deprecated or "unsupported," *but*
its use of non-type-strict keys "should probably" be considered
deprecated. Changing that behaviour, such that keys are type-strict in
legacy mode, is a *potential* TODO but doing so may be disruptive to
client-side scripts.

<a id="type-object-literals"></a>
## Object Literals

Objects are created using an "object literal" syntax, identical to
JavaScript's plus some extensions:

```s2
var o = {}; // empty object
o = {prototype:null}; // assignment to null or undefined unsets the prototype
o = {a: 1, b: 2};
var x = 2, y = 3;
o = {x, y}; // JS5-style shorthand for {x:x, y:y}

// s2-specific extensions to JS-like syntax...

// Expression-as-a-key (Eaak) syntax:
o = { [ 'a'+'b' ]: 1 }; // ==> {"ab":1}
// Const properties:
o = {a:1, b:=2}; // b is const (cannot be re-set later).
                 // For prototype:=x, the := is treated as : because we
                 // have no way to record/enforce that particular const
                 // constraint at the C level.
                 
// Copying of properties from another object, like JS
// "spread syntax":
o = {
  a: 1,
  @anotherObject, // Copies *iterable* properties from anotherObject
                  // into the object literal. More details below.
  b:2
};
```

Property access performance tip: remove the prototype, if it's
not needed, by assigning `null` or `undefined` to it! That will make
*failed* property lookups faster because they do not have to crawl up
the prototype chain. It also removes the possibility of unintentional
collisions with same-name/different-semantics properties inherited via
a prototype.

Caveats and notes regarding the `@anotherObject` syntax shown in the
last example above:

- The `anotherObject` part must be an arbitrary single expression
  which resolves to a container-type value. If the value is not a
  container, an exception is triggered.
- The *object-level* properties are copied, even if the source object
  is [a hash](type-hash.md) (and hash literals do not support this
  syntax due to potential semantic ambiguities). Only properties of
  the given object/expression are copied, not inherited properties.
- The source object may not be an `enum` because there are at least
  two equally valid approaches to importing enum values and
  arbitrarily choosing one of them does not sit well with s2's
  developer.
- `prototype` is not a real property, and *is not copied* by this
  syntax.
- The `const` flag of properties in the source object are retained by
  the copy process.
- When an object literal defines a `__new` method property, that
  property gets set as const/hidden, for consistency with the C-side
  API which installs constructors. (That behaviour is arguable and may
  need changing.) Hidden properties are *not* iterated over, and
  therefore such a constructor *will not be copied* by this syntax.
- Trivia: neither `@` expansion nor expression-as-a-key (`[...]:X`)
  were conciously copied from JS. Rather, they were both
  "independently conceived" and implemented in s2 before having seen
  them in JS.

<a id="type-object-methods"></a>
# Object Methods

The base Object prototype acts as the basis for *nearly* all other
prototypes in the core API. It includes the following methods:

**Constructor:**

```s2-member
new {}.prototype()
```

This exists for completeness, but is functionally identically to (and
*far* less efficient than) an empty object literal. Passing it any
arguments causes an exception to be thrown. It is strongly recommended
that clients use the object-literal syntax instead.


```s2-member
Object clearProperties()
```

Clears all properties in this object (not prototypes) and returns
this object. For objects which have been "sealed" against changes to
their properties, this method will trigger an exception.

> *Achtung:* this method will also remove properties which are set
> as `const`. e.g. `{a:=1}.clearProperties().# === 0`.


```s2-member
int compare(vX)
int compare(v1, v2)
```

Compares either this object against vX or compares v1 against v2,
and returns less than 0, 0, or greater than 0, indicating whether
this object resp. v1 is less than, equivalent to, or greater than vX
resp. v2.

Note that comparison means almost nothing for most types, except that
two instances of types which don't have well-defined comparison
semantics will compare with the same result in subsequent compare
attempts within a given application session. In any given session,
however, they may compare differently because they internally simply
do a pointer comparison.


```s2-member
Container copyPropertiesTo( container [, … containerN] )
```

Copies all non-hidden properties of this object (not including
prototype-inherited properties) to the given container(s). Returns the
last argument it is passed. It does no checking of whether the
destination already contains the properties - it simply overwrites
them if they already exist. Throws if `this` or any of the arguments
is not a Container type, if the source object is also a target, or if
any target object is currently being iterated over (modification
during iteration is not supported).

```s2-member
Object eachProperty( Function(key,value) )
```

For each non-hidden key/value pair in this object (not prototypes), it
calls the given function, passing it the key and value (in that
order). In the context of the callback, `this` will be the containing
object.  If the callback returns a literal `false` then looping stops
without an error. Returns this object. It is not legal to modify an
object while traversing it, nor to traverse it recursively. Doing so
will trigger an exception (see `mayIterate()` for why).

Note that a [foreach() loop](keyword-flow-control.md#keyword-foreach)
offers a much more efficient approach to object property iteration,
but this function predates `foreach` by a couple years.

```s2-member
mixed get(key)
```

Works identically to `this[key]`, but is less efficient because it
requires a script-side function call.


```s2-member
bool hasOwnProperty(key)
```

Returns `true` if this object (not prototypes) contains the given
property key, else returns `false`. This method is kind of a holdover
from JavaScript, and is not generally needed in s2 because iteration
never includes properties derived from prototypes (as it can, in
certain circumstances, in JavaScript). Even so, it has occasional uses
in s2.

```s2-member
bool mayIterate()
```

Returns `true` if it is legal to iterate over this object. It is
illegal to modify an object's properties while an iteration is
underway (e.g. via `eachProperty()` or a `foreach` loop). (The
property storage structure does not support modification during
traversal.) Tip: `typeinfo(mayiterate thisObj)` is more efficient and
can be passed arbitrary values, so it is generally more useful.

The semantics of when it is legal to iterate changed (for the better)
on 20191211. See [typeinfo(mayiterate)](keyword-typeinfo.md) for more
details.

```s2-member
Array propertyKeys()
```

Returns an array of all property keys in this object (not prototypes),
in an unspecified order. Note that the `prototype` property is not a
real property, and is not included in the returned list.

```s2-member
mixed set(key, val)
```

Works just like `this[key]=val`, returning the value passed to it.

```s2-member
string toJSONString([integer|string indentation=0])
```

Returns a JSON string form of this object. Indentation: a positive
value means to use that many space characters per indentation level
and a negative value means that many hard tabs per level (e.g. -3
means 3 hard tabs). The number 0 or an empty string means no
indentation. A non-empty string will use that string as the
indentation. e.g. indentation values of 1 and `" "` are equivalent, as
are -1 and `"\t"`. If the indentation value is an enum entry which
wraps either a string or number, the entry's value is used.  Important
notes:

- If cycles are found during traversal, an exception is thrown
  because we cannot JSON-ize cyclic structures.
- Some property types (e.g. functions) are elided altogether, and
  numeric keys are necessarily converted to string form.
- The API does not currently support overloading a `toJSON()` method
  to customize JSON output on a per-object base. i.e. all type-to-JSON
  conversions are hard-coded.

```s2-member
string toString()
```
Currently works like `toJSONString()` except that it does not support
an indentation level.

```s2-member
mixed withThis(Function callback)
```

This unusual function simply calls the given callback with this object
as its `this`. It is equivalent to calling `callback.apply(thisObj)`,
but this form is easier to use in some contexts. Its primary use is as
a post-construction initializer for anonymous objects. If the callback
makes an explicit `return` of a non-`undefined` value, that value is
returned. If the callback returns `undefined` or makes no explicit
`return` then `this` is returned instead (in practice `this` is the
only useful result for this method, but there are other hypothetical
use cases). See the following subsection for details.


<a id="type-object-withthis"></a>
# What Should We do `withThis()`?

The `withThis()` method is a function which, on the surface, seems pretty
useless, but it can simplify object and function declarations in certain
contexts. All that it does is call its argument (a function) using
`withThis()`'s `this` as the callback function's `this`, then returns
the result of the callback. e.g. the following are equivalent:

```s2
obj.withThis(proc(…){…});
proc(){ … return this; }.apply(obj, […]);
proc(){ … return this; }.call(obj, …);
```

Why? When constructing functions and inlined objects is it sometimes
necessary to take a reference to them (give them a variable name) in
order to make modifications to members after the object has been
initialized. For example:

```s2
var f = proc callee(arg){
  callee.buffer.reset().append(arg);
  // alternately, do the buffer initialization in this call:
  // (callee.buffer ||| (callee.buffer=new s2.Buffer())).reset().append(arg);
  return callee.buffer.toString();
};
f.buffer = s2.Buffer.new();
```

The same effect can be achieved without having a named reference using
`withThis()`:

```s2
var f = proc(...){...}.withThis(proc(){
 this.buffer = …;
 return this;
 // ^^^ optional because: if there is no explicit return,
 // or undefined is returned, then withThis() returns this instead!
 // Related trivia: at this level, s2 cannot differentiate between
 // "no explicit return" and "returned undefined" without adding
 // more per-function-instance overhead.
});
```

It is often of use when (and was created to assist in) building
"module-style" objects, which often do not have concrete symbolic
names but which often need non-trivial initialization. By moving their
setup into a `withThis()` callback, we effectively get a one-shot
initialization function, allowing us to get by without having to name
the top-level object. For example, a [require.s2](../mod/require/)
module might look like the following (the interesting parts are
marked with asterisks):

```s2
return {
 myFunc: proc *callee*(arg = *callee.config.default1*){
   print(arg);
 },
 ...
}.withThis(proc(){
 *this.myFunc.config* = { // this === the anonymous object above
   default1: 3
  };
 // reminder: withThis() returns *this* by unless the callback
 // explicitly returns any value other than *undefined*.
});
```

Used this way, the callback basically acts as a constructor function for
the object. Note that `withThis()` can be used in conjunction with
[Function.importSymbols()](type-function.md#type-function-importsymbols):

```s2
var f = proc(){...}.withThis(proc(){
 // this === f, but at the time this function is run,
 // f is not yet finished being declared, so we cannot resolve it!
}).importSymbols({...});
```

Or equivalently (well *almost*, except that this form's actually more
flexible in some contexts):

```s2
var f = proc(){...}.withThis(proc(){
 this.importSymbols({...});
});
```

But note that the *imported* symbols are not visible in `withThis()`
callback (more correctly, they are not in scope at the time that
callback is run, even if `importSymbols()` is called before
`withThis()`) because imported symbols are declared as call-local
variables when the function they are operating on is called (in the
above example, when `f()` is called). That said, imported functions
*may* reference *other* imported symbols because those will all be in
scope when the imported function is called from within `f()`.


With such uses, it is important that the callback return `this`, and
to that end `withThis()` automatically returns `this` if the callback
returns `undefined` (which is the default if no explicit `return` is
made, as well as the default value for a `return` statement with no
arguments). There are, however, other conceivable use cases (and maybe
one will be discovered someday!), so it allows one to return a
different value if they need to.

Note that the constructor metaphor can also apply to arrays and hash
tables, as in this example:

```s2
var a = [1].withThis(proc(){
 const N = 10; // fill array with [1, 2, 4, 8, …] up to the power of N
 for( var i = 1; i < N; ++i ) this[i] = this[i-1]*2;
});
```

The same effect as `withThis()` can (it was discovered much later) be
achieved in another way…

e.g. this approach is often seen in JavaScript modules:

```s2
var obj = (function(mod){ …; return mod; })({ … });
```

except that s2 does not require the extra layer of parens:

```s2
var obj = proc(mod){ …; return mod; }({ … });
```

Similarly:

```s2
var obj = proc(){ …; return this; }.call({ … });
```

The end results are identical, only the formulation differs.


<a id="type-object-plain-prop-store"></a>
# "Plain Property Objects" and Overriding Options

It's common to want objects to play the part of "pure property lists,"
with no inherited properties to collide with. It's also common (e.g. in
[jQuery](http://jquery.org)) to extend a library-provided set of
properties from a subset provided by clients. In JavaScript this
normally entails copying all properties from the "complete" set into the
destination object, skipping any properties it already has. s2 offers a
cleaner solution, in which the derived properties object inherits from a
base set:

```s2
// A base set of properties we want to derive from:
const defaultOptions = {
 prototype: null, // keeps this object from inheriting Object's methods
 option1: true,
 option2: false,
 option3: "hi"
};

// An Object which derives from that base set of options...
const myOptions = {
 prototype: defaultOptions, // !!!
 // now override any we want:
 option3: "yo"
};

assert true === myOptions.option1; // inherited
myOptions.option1 = 1;
assert 1 === myOptions.option1; // now overridden, but...
assert true === myOptions.prototype.option1;
// ^^^ does not overwrite the derived property
assert false === myOptions.option2;
assert "yo" === myOptions.option3;
```

This could be applied to objects provided by a caller:

```s2
const f = proc(optionsObj){
 affirm typeinfo(iscontainer optionsObj);
 optionsObj.prototype = defaultOptions;
 ...
} using {defaultOptions: myDefaultOptions};
```

Or, more generically:

```s2
const extendProperties = proc(baseOptions, clientOptions){
 affirm typeinfo(iscontainer baseOptions);
 affirm typeinfo(iscontainer clientOptions);
 clientOptions.prototype = baseOptions;
 return clientOptions;
};
```

Note that Objects without a prototype do not have/inherit any methods
unless the clients adds them, but
[`foreach`](keyword-flow-control.md#keyword-foreach) can be used to
iterate over any non-hidden properties.

It's also possible to use `@` expansion to achieve a similar, but less
efficient, effect:

```
const props = {
  @defaultPropsObj,
  @clientOverridePropsObj
};
```

That will first copy all properties from `defaultPropsObj` and then
overwrite any it defines with those from `clientOverridePropsObj`.
