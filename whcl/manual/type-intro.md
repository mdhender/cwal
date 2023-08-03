# whcl: Data Types Intro
##### ([&#x2b11;Table of Contents](./))
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

Jump to...

- [Intro](#type-intro)
- [Boolean Contexts](#type-intro-boolean-contexts)
- [Prototypes](#type-intro-prototypes)
- [Creating Objects and Arrays and such](#type-intro-creating-objects)

Per-type pages:

- [String](type-string.md)
- [Numbers](type-number.md)
- [Object](type-object.md)
- [Array](type-array.md)
- [Function](builtin-proc.md)
- [Exception](type-exception.md)
- [Buffer](type-buffer.md)
- [PathFinder](type-pathfinder.md)
- [Script](type-script.md)

<!--
"Probably" won't implement these in whcl:

- [Tuple](type-tuple.md) (has always been overkill)
- [Hashtables](type-hash.md) (obsolete)
- [Enum](type-enum.md) (was never quite happy with these)
-->

<a id="type-intro"></a>
# Introduction
============================================================

cwal, the underlying scripting engine, supports a fairly rich set of
JavaScript-like data types, plus a few more exotic ones. All "basic"
types are, as is conventional, immutable (their values cannot be
changed), but "containers" (types which hold other values) may of
course be modified (or they'd be quite useless (in this
language)). All values are, in effect, passed by pointer. There is no
such thing as "deep copy" in cwal[^34].

The built-in data types exposed via whcl and constant values are...

-   Boolean values `true` and `false`.
-   The special `null` and `undefined` values. `undefined` is used as
    the default for anything left unspecified, and `null` is primarily
    intended for script-side use or for use with other data formats
    (like JSON) which support a native null type (or value).
-   *Signed* [integers](type-number.md), with 16, 32, or 64 bits,
    depending on build-time options. May be in decimal format,
    hexadecimal with a leading 0x (zero-x, case-insensitive), octal with
    a leading 0o (zero-small-oh), or binary with a leading 0b (zero-small-b).
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
    `\uXXXX` and `\UXXXXXXXX` unicode sequences (see
    [grammar.md](grammar.md#gramma-strings)).
-   *Container* is a generic term for types capable of holding other
    Values, either in a list or as key/value pairs (i.e. properties).
    Such relationships may form cycles, but some algorithms will trigger
    an exception if they traverse cycles or iterate over properties at a
    time when doing so is not legal (e.g. the JSON API is allergic to
    cycles). The container types include:
    -   [Object](type-object.md) - generic key/value stores.
    -   [Array](type-array.md) - both a heterogeneous list of
        values and an Object-like key/value store (with separate
        storage for each).
    -   [Function](type-function.md) values are first-class values
        created using [the `proc` command]. When created from C code
        (as opposed to script code), they may have native (C-level)
        data attached to them and an optional finalizer to clean it up
        when the function instance is finalized.
    -   [Buffer](type-buffer.md) - a general-purpose dynamic
        memory buffer, e.g. for creating dynamic string content or
        working with binary data. They can also have their own
        properties.
    - [PathFinder](type-pathfinder.md) is a utility class for
        searching for files using a configurable set of paths and file
        extensions.
    - [Script](type-script.md) enables script code to "compile" dynamic
        snippets for evaluation multiple times, as a cost-saving
        optimization. These are implemented as...
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
        of the native type or expose pre-existing C-level instances.

Note that the underlying engine supports several other types which we
do not currently plan on exposing to script code, most notably the
hashtable class, as that one is functionally obsolete since the object
type was changed to use hashtable storage.


<a id="type-intro-boolean-contexts"></a>
# Boolean Contexts
============================================================

A "boolean context" is whenever the engine needs to reduce a problem's
answer to a simple `true` or `false`. All value types can be
implicitly converted to a boolean true or false, and their
interpretations are set in stone at the cwal engine level, so neither
scripts nor whcl may modify them:

-   `true` and `false` are of course themselves.
-   `null` and `undefined` are always `false`.
-   Numbers 0 resp. 0.0 are `false`, all others are `true`.
-   Strings: any *empty* (length-0) string is `false`, any non-empty string is `true`.
-   Higher-level types (containers and buffers[^36]): always `true`.

<a id="type-intro-prototypes"></a>
# Prototypes

whcl does not have classes, in the sense that high-level languages
like C++ and Java have classes. It has a hard-coded core set of types
(okay, "classes") with which clients can compose their data
structures. whcl uses a prototypal inheritance model, similar to
JavaScript's but has a few subtle, yet significant, semantic
differences. A "class" in whcl is simply a specific use case for a
specific Value, for which the language provides a very small amount of
extra support. Specifically:

- Prototypes are used to share a set of properties amongst a set of
  values, and properties in a prototype are resolved when properly
  lookup in a container-type value fails to find a property.
- [the `new` command][builtin-new.md] provides a common mechanism for
  creating new instances of a type, all of which inherit (by default)
  from a shared prorotype. (Interestingly, the features it provides
  can all be implemented entirely in script code without requiring an
  explicit keyword, but this keyword provides a language-level
  interface, instead of having to use per-type factory functions.)

All of the base types except for the booleans, `null`, and `undefined`
have a prototype object which provides common member methods. They are
described in the sections of this document dedicated to each data type.

Clients may change the prototypes of any container-type value via
assignment to the `__prototype` pseudo-property, but properties set on
an object are always set on *that* instance, and never its
prototype. Note, however, that assigning over the prototype of a
built-in class will _not_ have the desired effect because those
prototypes are mapped at a lower level in C code. Clients may freely
modify the prototypes of built-in classes but not outright replace
them.

Clients may not (for at least two long, boring reasons[^37]) reassign
the prototypes for the non-container types (numbers and strings), and
trying to do so will trigger an exception.

There is nothing specifically magical about prototypes except that the
`__prototype` property is not *really* a property. Rather, it is
intercepted internally as needed as a proxy for the C-level prototype
getter/setter APIs. This property does not show up in any property
iteration, for example. Prototype objects, in and of themselves, do not
get any special treatment as prototypes - only access to the prototype
(pseudo-)property is handled individually[^38].


cwal only supports container types as prototypes. Trying to set a
non-container value as a prototype will fail, with two special-case
exceptions (handled whcl-side, not in cwal): assigning either `null`
or `undefined` to `$anObject[__prototype]` will remove that object's
prototype. This is often useful for "plain properties objects" when a
user wants to ensure that looked-up properties are not inherited from
a prototype.

Unlike JavaScript, modifying prototype objects does not have any
outwardly visible effects on objects which refer to them. e.g. when
adding methods to the base Object prototype in JavaScript, traversal
of properties in client-created objects can get very strange indeed
(they suddenly include their prototype's properties when iterating,
requiring the use of JS's `Object.hasOwnProperty()` method to
work around it). To demonstrate this JavaScript oddity:

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

In whcl, the effect is more intuitive: only the object being modified is
modified, and iteration of other instances is most certainly
not changed. Note that whcl's [core Object type](type-object.md)
does indeed have a `has-own-property` method, but it's essentially
never needed.

For info about whcl's approaches to creating classes,
see [the `new` command](builtin-new.md).


Listing all Builtin Prototypes and Methods
------------------------------------------------------------

The prototypes of all of whcl's core/built-in types are accessible
from script code via `whcl[prototypes][TYPENAME]`.

To get the complete list of type names try:

```whcl
foreach k v whcl[prototypes] {echo $k $v}
```

Noting that whcl's property storage is unordered, so the keys will not
be sorted except by wild random chance.

To get a list of all of the built-in prototypes and their methods:

```whcl
foreach k v whcl[prototypes] {
  echo $k $v
  foreach kk vv $v {echo "    " $kk $vv}
}
```

Noting that the `Number` prototype is the shared prototype of both
the `Integer` and `Double` prototypes.


<a id="type-intro-creating-objects"></a>
# Creating Objects and Arrays

Though objects and arrays are core data types in whcl, there is no
"literal" syntax for creating them like there is in JavaScript or
[s2](/doc/tip/s2/) because interpretation of such syntax is highly
context-dependent in whcl. Instead, the [object][] or [array][]
builtin command is used to create them. Similarly, many different APIs
return values of these types.

# Footnotes

[^34]: But making deep copies via intermediate expressions is often
    possible. e.g. `(decl a 300; decl b (a * 2 / 2)` ends up with
    two unique copies of integers with the value 300.

[^35]:  In 30+ years(!) of programming, i've never once needed scientific
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

[proc]: builtin-proc.md
[todo]: todo.md
[object]: builtins.md#bic-object
[array]: builtins.md#bic-array

