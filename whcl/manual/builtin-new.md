# whcl: The new Builtin
##### ([&#x2b11;Table of Contents](./)) ([builtins](builtins.md))
# The `new` Builtin
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

See also: [`proc`](builtin-proc.md)

WHCL has a basic "class" mechanism, vaguely akin to JavaScript's...

* [Introduction](#intro)
* [A Brand `new` Card](#new-car)
* [Method Call Syntax](#method-call-syntax)
* [Post-Construction Init](#post-init)
* [Returning not-this from a Constructor](#constructor-return)
* [Complete Examples](#complete-examples)

<a id="intro"></a>
# Introduction
============================================================

As with most languages, WHCL offers a way for client code to create
new classes. It's not, because of WHCL's command-style interface, as
syntactically suave as, say, C++'s or even JavaScript's, but it's
workable.

A class is created in one of two ways, which are functionally
equivalent but have slightly different structures.

Approach #1: a class is defined as a function (called a constructor) and
the class prototype is defined via that function's prototype. Client
code passes that function to the `new` builtin.

Approach #2: a class prototype is some arbitrary object which has or
inherits a method named `__new`. Client code passes that prototype to
the `new` command and it extracts the `__new` method for use as a
constructor.

For both cases, the `new` builtin command does the following:

1. Creates a new plain object. This value will be the default
   result of the `new` command.
2. Sets its prototype based on the class structure (approach #1 or
   #2, as described above).
3. Calls the constructor, setting the value from step 1 as
   the `this` and passing on any arguments except for possibly
   one (described in step 5).
4. If the function returns any explicit value other than `undefined`,
   it replaces the object from step 1 as the result of `new`. This
   feature is rarely needed but is, on occasion, useful when a function
   needs to return a "native" type (bound to a C-level `cwal_native` 
   wrapper). If the function does not explicitly return, or returns
   the `undefined` value, the object created in step 1 remains as the
   result of `new`.
5. If the final argument to the constructor is a `{code block}` then
   it is _not_ passed to the constructor and is instead run after
   the constructor returns. That block will be run in its own scope,
   and `this` will resolve to `new`'s result. This block is _not_
   a function call but _does_ catch any `return` result and discards
   it. The intent of this block is to perform any post-construction
   initialization necessary for the current context.

That might all sound intimidating, but it's rather straightforward,
as demonstrated below.

> But first a sidebar: all of the capabilities provided by the `new`
command, with the exception of the optional post-construction script
block (which is just syntactic sugar), can be implemented entirely in
script code, without the use of a builtin command. It's a builtin
command because practice has shown that it's much simpler in
client-side scripts if the language provides a common way for
constructing new instances of any given type, rather than having
per-type factory functions.

<a id="new-car"></a>
# It's a Brand `new` Car!
============================================================
Our Car class:

```whcl
proc Car {name {color red}} {
    affirm [info is-newing] /*else it was called without `new`*/
    set this[color] $color
    set this[name] $name
}
```

For very, very basic cases, that's all we need, but most cases will
also want member functions (a.k.a. class methods). Those are defined
in the prototype of the class. The syntax for that is, because of
WHCL's command-style syntax, a tiny bit awkward, but it does the job:

```whcl
set Car[__prototype] object {
    say-name [proc {} {
        echo "My name is" this[name]
    }]
    say-color [proc {} {
        echo "My color is" this[color]
    }]
}
```

However, because that syntax is somewhat unsightly and unweildy for
non-trivial cases, a new approach was invented:

```whcl
set Car[__prototype] object -scope {
    proc say-name {} {
        echo "My name is" this[name]
    }
    proc say-color {} {
        echo "My color is" this[color]
    }
    # Any normal script code is legal here and all scope-level
    # vars become the properties of the resulting object,
    # retaining special attributes such as const. e.g.:
    decl foo 1
    decl -const bar 2
}
```

With one of those options in place we can make use of our car like so:

```whcl
decl c new Car "WHCL 2000"
#   dollar ^^^ prefix is optional there.
assert c[__prototype] == Car[__prototype]
assert "WHCL 2000" == $c[name]
assert red == c[color]
c[say-name]   ;# This is the "native" call syntax but...
c say-color   ;# see notes regarding the call syntax below.
```

That's really all there is too it, but we need to say something about
those last two lines...

<a id="method-call-syntax"></a>
# Method Call Syntax
============================================================

When whcl examines the initial token of a line (the so-called "command
position"), it checks whether it is a function or inherits a function
named `__command` via its prototype chain. In the former case, the
command's arguments are all passed to the function. If, however, the
value inherits a `__command` method then that method is called. (If
both cases are true, prefers the former.)

In our example case, `Car` inherits from the default object
class. That class implements a `__command` method. That method looks
for a property in its `this` object named by the first argument to the
command (`say-color` in the example above). Since its `this` (our `c`
object) inherits `say-color`, it calls that method, passing it on all
arguments after `say-color` and applying our `c` object as the `this`
for that call.

That approach will work as-is for classes which inherit (perhaps
indirectly) from the built-in object type. For the sake of discussion,
however, let's clear out the prototype of `Car`'s own prototype,
noting that this is actually a perfectly sensible thing to do for some
use cases:

```whcl
unset Car[__prototype][__prototype]
```

(Or add `__prototype null` or `__prototype undefined` to the object
which defines the prototype - the effect is identical.)

Once we do that, using the `c say-color` syntax will fail with an
exception telling us something like "Symbol 'c' does not resolve to a
command"! We're seemingly forced to use `c[say-color]` syntax after
that! _Oh no!_

We have a few options, however:

1. Limit client usage to the `obj[method-name]` call syntax. _Meh._
   (The one advantage of this, however, not to be understated, is that
   this syntax works for all data types, so users don't need to switch
   syntaxes for cases which don't support it, nor do they need to
   understand why it might not be available for a given class.)
2. Implement our class's own `__command` method which works however
   we like.
3. Copy a generic `__command` method from some other object.

Option 3 is actually very simple. We simply need to copy that method
from any object which inherits it:

```whcl
if {true} { # create a new scope
  decl x object
  set Car[__prototype][__command] x[__command]
}
```

Now the `Car` class has its own copy of the core-most `__command`
implementation without having to inherit all other methods of the
object class.

<a id="post-init"></a>
Post-Construction Initialization
============================================================

A call to `new` may _optionally_ end with a `{code block}`, e.g.:

```whcl
decl c new -post Car "Blayzor 2022" blue {
  set this[license-plate] BLUBLAYZ
}
```

> Note that without the `-post` flag, that final `{...}` arg is
  treated as a string to pass to the constructor.

That block is run only if the constructor returns (does not throw
an exception). It is _not_ a function call but it behaves like one
in two ways:

1. `this` resolves to the just-constructed value.
2. It handles the `return` command intuitively, exiting only that
   block without returning from an outer function or script.

Unlike a function, however, it discards any `return` result because
`new`'s result is object which it's currently constructing.

<a id="constructor-return"></a>
Returning not-this from a Constructor
============================================================

Recall that if a constructor explicitly returns a value other than
`undefined`, _that_ value because the result of a call to `new`. That
isn't _usually_ necessary, but it is when a constructor wants to
return a type other than the core object type. The most glaring
example is when a C-bound constructor wants to return a
`cwal_native`-type value.

Though it is not normally necessary it can, to quote
Captain Malcom Reynolds, on occasion be hilarious:

```whcl
proc MyCtor {} {
    decl -const self array
    return self
}
```

That's enough to return a different type from the constructor _but
it's missing one important piece_: the prototype of that returned
value is _not_ the prototype of the constructor. Instead, it's the
prototype for the array type. In order for this constructor to play
nicely in the `new` ecosystem, it must (or really should) set the
prototype of the returned object to the constructor's own, like so:

```whcl
proc MyCtor {} {
    decl -const self array
    set self[__prototype] this[__prototype]
    ...
    return self
}
```

The obligatory caveat being that `self` will no longer inherit the
various methods provided by the array class. It will still "be-a"
array, in that it is an instance of the C-level `cwal_array` class,
but it will be missing the class methods provided by its previous
prototype (which itself _is not an array_!). How do we retain both
worlds, such that objects returned from `MyCtor` are both true
arrays _and_ retain the inherited array APIs? Here's one of
several valid formulations:

```whcl
set $MyCtor[__prototype] object -this {
    ... our methods and such go in here...
    set this[__prototype] [array][__prototype]
}
```

An alternative formulation is to wrap up all of that in a construct
like...

```whcl
decl MyKlass eval -scope {
    proc ctor {} {
        decl -const self array
        set self[__prototype] this[__prototype]
        ...
        return self
    }
    set ctor[__prototype] object -this {
        # Re-assign the ctor prototype's prototype to be that of an Array:
        set this__prototype [array][__prototype]
        ... add any inherited methods or properties ...
    }
    eval $ctor; # becomes the outer eval -scope result
}
```


<a id="complete-examples"></a>
Complete Example
============================================================

Several fully-formed and documented examples
of creating `new`-compatible classes can be found
[in this unit test script](/doc/$CURRENT/whcl/unit/100-025-new.whcl?mimetype=text/plain).
