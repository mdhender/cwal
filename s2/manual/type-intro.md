# s2: Data Types
#### ([&#x2b11;Table of Contents](./))
# Data Types

Jump to...

- [Intro](#type-intro)
- [Boolean Contexts](#type-intro-boolean-contexts)
- [Prototypes](#type-intro-prototypes)
- [Literal Objects and Arrays](#type-intro-object-literals)

Per-type pages:

- [String](type-string.md)
- [Numbers](type-number.md)
- [Object](type-object.md)
- [Array](type-array.md)
- [Tuple](type-tuple.md)
- [Function](type-function.md)
- [Exceptions](type-exception.md)
- [Hashtables](type-hash.md)
- [Buffer](type-buffer.md)
- [Enum](type-enum.md)
- [PathFinder](type-pathfinder.md)


<a id="type-intro"></a>
# Introduction

s2 supports a fairly rich set of JavaScript-like data types, plus a
few more exotic ones. All "basic" types are, as is conventional,
immutable (their values cannot be changed), but "containers" (types
which hold other values) may of course be modified (or they'd be quite
useless (in this language)). All values are, in effect, passed by
pointer. There is no such thing as "deep copy" in s2[^34].

The built-in data types and constant values are...

-   Boolean values `true` and `false`.
-   The special `null` and `undefined` values. `undefined` is used as
    the default for anything left unspecified, and `null` is primarily
    intended for script-side use or for use with other data formats
    (like JSON) which support a native null type (or value).
-   *Signed* [integers](type-number.md), with 16, 32, or 64 bits,
    depending on build-time options. May be in decimal format,
    hexadecimal with a leading 0x (zero-x, case-insensitive), octal with
    a leading 0o (zero-small-oh), or (as of 20171202) binary with a
    leading 0b (zero-small-b).
    -   Binary, octal, decimal, and hex literal integers support any
        number of underscores as separators except at the end of the
        sequence.  e.g. `0b10101010` may be written as `0b1010_1010`
        or `0b10_10_10_10`. Likewise `0xFFFFFFFF` can be written as
        `0xFFFF_FFFF`. Separators between the 0b/0o/0x prefix and the
        first digit are permitted, e.g. `0x_ff_ff`.
-   [Double](type-number.md)-precision floating point, with
    build-time-defined precision. They may only be input in decimal
    format. There is *no* E-notation, e.g. 12.3e-7[^35].
-   [String](type-string.md), UTF-8 encoded. String literals support
    any UTF8 characters, standard C escape sequences, as well as
    `\uXXXX` and `\UXXXXXXXX` unicode sequences (see [grammar.md](grammar.md#strings)).
-   *Unique* is a special-purpose type currently only used in enums and
    internally to create "secret" property keys which scripts cannot
    know. A "unique" value never compares equivalent to any other value
    and always evaluates to true in a boolean context. They have no
    intrinsic value other than their identity, but may wrap a single
    other arbitrary value.
-   *Container* is a generic term for types capable of holding other
    Values, either in a list or as key/value pairs (i.e. properties).
    Such relationships may form cycles, but some algorithms will trigger
    an exception if they traverse cycles or iterate over properties at a
    time when doing so is not legal (e.g. the JSON API is allergic to
    cycles). The container types include:
    -   [Object](type-object.md) - basic key/value stores.
    -   [Array](type-array.md) - both a heterogeneous list of
        values and an Object-like key/value store (with separate
        storage for each).
    -   [Tuple](type-tuple.md) are close cousins of arrays, but
        have a fixed length, comparison operators, and are not
        full-fledged containers (they may not hold properties). They are,
        however, lighter-weight than arrays, requiring slightly less
        memory per instance.
    -   [Function](type-function.md) values are first-class values
        created using the `function` [keyword](keywords.md) or
        its alias `proc`. When created from C code (as opposed to
        script code), they may have native (C-level) data attached to
        them and an optional finalizer to clean it up when the
        function instance is finalized.
    -   [Hash](type-hash.md) (hash table) - a key/value store
        with amortized O(1) search, insertion, and removal
        performance, but costing notably more memory than an Object.
        It may also hold normal object properties.\  
        Sidebar: because hash lookups require a script-side function
        call, an Object property lookup may very well be notably
        faster even for mid-sized objects (say, 5-10 properties, just
        kind of randomly speculating). … And thus the `# operator` was
        born.
    -   [Buffer](type-buffer.md) - a general-purpose dynamic
        memory buffer, e.g. for creating dynamic string content or
        working with binary data. Prior to [20141217](/info/73df044fc169aa1e),
        these were not containers.
    -   "Natives" are wrappers for client-specified C types,
        optionally with a C-level finalizer which the garbage
        collector will call when the time comes (or may be triggered
        from C code, which will cleanly disconnect the script instance
        from its Native counterpart, such that further access to the
        native will trigger an exception rather than dereferencing a
        stale/*NULL* pointer). They may also hold object-level
        properties. Natives cannot be created *directly* from script
        code, but can be bound to script code from C, and that code
        may provide one more more functions which create new instances
        of the native type or expose pre-existing C-level instances
        (e.g. a [FILE handle to stdout](../mod/FILE/), which is owned
        by the C runtime but could nonetheless be used from script
        code).  e.g. s2's [PathFinder class](type-pathfinder.md) is
        implemented using this approach, as are several of the
        [loadable modules](../mod/).  In principle, any non-const
        C/C++ data structure can be bound to s2 using a the Native
        type (const data *can* be bound but that would require casting
        away the constness). (FWIW, i have never seen a scripting
        engine which directly supports binding *const* C data without
        having to cast away its constness.)
    - [PathFinder](type-pathfinder.md) is a utility class for
        searching for files using a configurable set of paths and file
        extensions.

<a id="type-intro-boolean-contexts"></a>
# Boolean Contexts

A "boolean context" is whenever s2 needs to reduce a problem's answer to
a simple `true` or `false`. All value types can be implicitly converted
to a boolean true or false, and their interpretations are set in stone
at the cwal engine level, so neither scripts nor s2 may modify them:

-   `true` and `false` are of course themselves.
-   `null` and `undefined` are always `false`.
-   Numbers 0 resp. 0.0 are `false`, all others are `true`.
-   Strings: any *empty* (length-0) string is `false`, any non-empty string is `true`.
-   Tuples: the empty (length-0) tuple evaluates to `false`, all others to
    `true` (regardless of their content). This decision is up for
    re-evaluation: it might be interesting for all of them to be truthy.
    We "could" have them be truthy only if all elements are truthy, but
    that would be slow and would require dealing with cycles. The
    current semantics differ from Arrays, which always (even if empty)
    evaluate to truthy.
-   Higher-level types (containers and buffers[^36], as well as
    "uniques" (enum entries)): always `true`.

<a id="type-intro-prototypes"></a>
# Prototypes

s2 does not have classes, in the sense that high-level languages like
C++ and Java have classes. It has a hard-coded core set of types
(okay, "classes") with which clients can compose their data
structures. s2 uses a prototypal inheritance model, similar to
JavaScript's but has a few subtle, yet significant, semantic
differences. A "class" in s2 is simply a specific use case for a
specific Value, for which the language provides a very small amount of
extra support. Specifically:

- Prototypes are used to share a set of properties amongst a set of
  values, and properties in a prototype are resolved when properly
  lookup in a container-type value fails to find a property.
- The [`new` keyword](keyword-new.md) provides a common mechanism for
  creating new instances of a type, all of which inherit (by default)
  from a shared prorotype. (Interestingly, the features it provides
  can all be implemented entirely in script code without requiring an
  explicit keyword, but this keyword provides a language-level
  interface, instead of having to use per-type factory functions.)

All of the base types except for the booleans, `null`, and `undefined`
have a prototype object which provides common member methods. They are
described in the sections of this document dedicated to each data type.

Clients may change the prototypes of any container-type value via
assignment to the `prototype` pseudo-property, but properties set on
an object are always set on *that* instance, and never its
prototype. To demonstrate the implications of that:

```s2
var obj = {a:1, prototype: {b:2}};
assert 2 === obj.b;
obj.b = 3; // hides the prototype's copy:
assert 3 === obj.b;
assert 2 === obj.prototype.b;
unset obj.b; // but the prototype still has its copy:
assert 2 === obj.b;
var prototype; // it is only a keyword in property access contexts
obj.prototype.prototype = obj; // Throws exception: cyclic prototype relationship
```

Clients may not (for at least two long, boring reasons[^37]) reassign
the prototypes for the non-container types, and trying to do so will
trigger an exception.

There is nothing specifically magical about prototypes except that the
`prototype` property is not *really* a property. Rather, it is
intercepted internally as needed as a proxy for the C-level prototype
getter/setter APIs. This property does not show up in any property
iteration, for example, and `someObj.hasOwnProperty('prototype')` will
always return `false`. Prototype objects, in and of themselves, do not
get any special treatment as prototypes - only access to the prototype
(pseudo-)property is handled individually[^38].

cwal only supports container types as prototypes. Trying to set a
non-container value as a prototype will fail, with two special-case
exceptions (handled s2-side, not in cwal): assigning either `null` or
`undefined` to `anObject.prototype` will remove that object's
prototype. This is often useful for "plain properties objects" when
a user wants to ensure that looked-up properties are not inherited from
a prototype.

Unlike JavaScript, modifying prototype objects does not have any
outwardly visible effects on objects which refer to them. e.g. when
adding methods to the base Object prototype in JavaScript, traversal
of properties in client-created objects can get very strange indeed
(they suddenly include their prototype's properties when iterating,
requiring the use of [`Object.hasOwnProperty()`](type-object.md) to
work around it). To demonstrate this oddity:

```js
js> var o = {a:1}
undefined
js> Object.prototype.foo = 1
1
js> o.foo
1
js> for(var i in o) { console.debug(i, o.hasOwnProperty(i)); }
// outputs:
a true
foo false // this is the weird bit: modifying the prototype changes
          // how iteration of all instances works.
// And yet:
js> Object.keys(o)
Array [ "a" ]
```

(*Wha?!?!*)

In s2, the effect is more intuitive: only the object being modified is
modified, and iteration of other instances is most certainly
not changed. Note that s2's [core Object type](type-object.md)
does indeed have a `hasOwnProperty()` method, but it's essentially
never needed in s2.

Also unlike JavaScript, the "class" and prototype of values in s2 are
the same thing. In JavaScript, new types (classes) are created using
Functions, and inherited properties are set on function's
prototype. New instances of that function, created via `new
thatFunction()`, inherit the properties of that function's prototype
but not any properties of the Function (even though the `instanceof`
keyword says they inherit that Function). In s2, that "extra" level
of indirection is missing. That is, if a class is defined using a
function, that function becomes each instance's prototype (which, in
turn, makes every instance call()-able, in stark contrast to
JavaScript). Whether that's a feature or bug is debatable, but it is
how it is.

To briefly contrast the approaches:

```js
const MyClass = function(){...};
MyClass.prototype.x = 1;
const m = new MyClass();
m instanceof MyClass; // true
m.__proto__ === MyClass.prototype; // true
// ^^^ why m.prototype does not work is one of life's eternal mysteries
```

vs.

```s2
const MyClass = {
  __new: proc(){...},
  x: 1
};
const m = new MyClass();
assert m inherits MyClass;
assert m.prototype === MyClass; 
```

This is not to claim that s2's approach is superior. In practice, JS's
extra level of indirection is arguably more intuitive once one grasps
the idea that new classes are created as functions. s2's approach,
however, is (for better or worse) both memory-lighter and more
flexible, in that any container type may be a prototype and any value
with a `__new` function method can behave like a class for purposes of
the `new` keyword. It does, however, lead to weirdness such as all
containers which inherit a function become call()-able via that
function:

```s2
const MyClass = proc(){s2out<<'the class\n'};
MyClass.__new = proc(){s2out<<'ctor\n'};
const m = new MyClass(/*calls the __new constructor*/);
// ^^^ outputs 'ctor'
m(); /** calls MyClass(), with m as the "this", but not for the reason
         one might think: in s2, a function/callable called with no
         property access operator, i.e. F() instead X.F(), is its own
         "this". Thus, in this unusual case, m is its own this in that
         context. */
// ^^^ outputs 'the class'
```

<a id="type-intro-object-literals"></a>
# Literal Objects and Arrays

Like JavaScript, s2 supports creating arrays and plain objects via an
"inlined" form:

```s2
var a1 = []; // empty array
var a2 = [1, 2, 3];
var o1 = {}; // empty object
var o2 = {
  a: 1, b: 'hi',
  c: 52.3
};
var o3 = {x:1, a1, a2}; // ⇒ {a1: a1, a2: a2, x:1} (JS-like, [as of 20171201](/cwal/info/c2ef5a9c88de652a))
var key1 = "hi", key2 = ", world";
var o4 = {[key1+key2]: 2}; // ⇒ [expression-as-a-key (EaaK)] support [as of 20191117](/cwal/info/37b82ddc38241caa)
assert 2 === o4."hi, world"; // or o4["hi, world"] or o4.("hi, world")
```

Anywhere where a value is expected, the `[]` and `{}` constructs are
interpreted as an array resp. object literal, with one exception:
`[...`] is treated as an expression block if (and only if) it appears as
an object key in an object literal. All block constructs (including
scopes) which use `{}` as bodies are prefixed with a keyword, so there is
never a semantic ambiguity.

New object and array instances may optionally be created via their
respective prototype's constructor, but doing so is *much* less
efficient than simply using a literal:

```s2
const Object = {}.prototype;
var o = new Object(); // (no arguments allowed) ⇒ {}
const Array = [].prototype;
var a = new Array(1,2,3); // ⇒ [1, 2, 3]
```

These constructors are provided only for completeness - they are not
used in practice, nor is their use recommended.

The body of an object or array literal is evaluated in a new scope,
meaning, for example, that one may use local variables inside the body
of an object:

```s2
{
  dirs: typeinfo(isstring var tmp = s2.getenv('SEARCH_PATH'))
    ? tmp.split(':') : ['/foo', '/bar'],
  extensions: typeinfo(isstring tmp = s2.getenv('SEARCH_EXT'))
    ? tmp.split(':') : ['.txt', '.html']
}
```

Notice how the second property can access the var declared in the
definition of the first property. The `tmp` variable will go out of
scope at the end of the object's body. `typeinfo()` is a function-like
[keyword](keywords.md), not a function, and does not start a
new scope in its argument list like a function call does, so the `var`
declaration is made in the same scope as the `typeinfo()` call (which is
a scope created for parsing the object literal which contains the
properties being defined). Object and array literals push an implicit
scope while parsing the body so that temporaries created while
assigning properties, as well as vars (like the one shown above), do
not become part of the scope on whose behalf the object/array is being
created.

# Footnotes

[^34]:  But making deep copies via intermediate expressions is often
    possible. e.g. `(var a=300, b = a*2/2)` ends up with two unique
    copies of integers with the value 300.

[^35]:  In 30 years(!) of programming, i've never once needed scientific
    notation numbers.

[^36]:  We should arguably eval empty buffers as false, seeing as they
    are basically used like mutable strings.

[^37]:  1) cwal only supports a single shared prototype for
    non-containers because doing otherwise would cost more memory for
    *all* values just to cover an as-yet-unneeded use case. 2) Sanity's
    sake.

[^38]:  Minor internal exception: for C-level prototypes we have to
    place the instances somewhere with an "indefinite" lifetime, to
    avoid that they get GC'd before they are used or when the last
    "subclass" instance goes out of scope. Yes, prototypes are subject
    to normal lifetime rules.
