# whcl: Working with Variables
##### ([&#x2b11;Table of Contents](./)) ([builtins](builtins.md))
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

Doc sections:

- [Declaring Variables](#decl)
- [Dereferencing Variables](#dereference)
- [Modifying Variables](#set)
- [Destroying Variables](#unset)
- [Variable and Property Aliases](#alias)

<a id='decl'></a>
Declaring Variables
============================================================

Variables in WHCL (1) always have to be declared before use and (2)
are always local to the scope they are declared in. They can be
[resolved by _any newer scope_ which is pushed while a declaration
is active](symbol-resolution.md).

Variables are created using the `decl` builtin command:

```whcl
decl X   ; # creates X with the undefined value
decl Y 1 ; # creates Y with a value of 1
```

Variables can be made "const," meaning that they be neither
replaced nor `unset`:

```whcl
decl -const X 1 ; # const vars require a value
decl -const Y object {a 1 b 2} ; # Y's _contents_ may be modified
incr X ; # will fail because X is const
```

If the `value` part of a declation is itself a builtin command,
`decl` calls it an forwards all remaining arguments on the line
to it. That permits:

```whcl
decl o [object a b c d]
```

To be written more succintly as:

```whcl
decl o object a b c d
```

> Sidebar: see [this section](builtins.md#builtin-vs-non) for why
  `decl` can treat builtins specially in this regard but cannot do the
  same for non-builtin commands.

When declaring multiple values, this alternate syntax may be simpler:

```whcl
decl {
  X 1
  Y 2
}
```

Noting that that syntax does _not_ support commands (builtin or
otherwise) as values unless they're wrapped in `[...]`.

The "simple" form of `decl` evaluates to the value it sets, so:

```whcl
assert 1 == [decl x 1]
```

The `{...}` syntax always evaluates to `undefined`.


<a id='dereference'></a>
Dereferencing Variables
============================================================

To "dereference" a variable means to extract its referenced value.
Variables, in and of themselves, cannot be passed around in the
scripting environment. Instead, we can pass around their _names_
(in the form of identifiers) or the _values_ they refer to:

```whcl
decl a 123
echo a  ; # an identifier, which is normally treated like an unquoted string
echo $a ; # the value referred to by a, i.e. 123
```

To access object properties we use the more-or-less conventional
square brackets:

```whcl
decl o object {a 1 b {c 2}}
echo $o      ; # the value of (a whole object)
echo $o[a]   ; # the value 1 (o's "a" property)
echo $o[b][c]; # the value 2 ("b" property of the "c" property)
```

When these docs refer to "dereferencing" a variable, they simply mean
to access its underlying value.


<a id='set'></a>
Modifying Variables
============================================================

The most straightforward way to modify variables is the `set` builtin:

```whcl
decl X
set X 1
set Y 2 ; # will fail because Y has not been decl'd.
```

`set` shares `decl`'s ability to have a builtin command as its second
argument:

```whcl
decl A
set A proc {} {...}
```

`set` evaluates to the value it sets, so:

```whcl
decl x
assert 1 === [set x 1]
```

`set` also shares `decl`'s ability to set object properties:

```whcl
set $anObject[propertyName] value
```

And it supports setting such properties as "const", meaning that further
calls to `set` them will fail:

```whcl
set -const $anObject[propertyName] value
```

The `-const` flag is only permitted when setting object properties,
not when setting scope-level variables. It will fail if the property
is already const.

One particularly common change made to variables is incrementing or
decrementing them, for which there are shortcuts:

```whcl
decl x 1
assert 2 == [incr x]
assert 5 == [incr x 3]
assert 4 == [decr x]
assert 2 == [decr x 2]
assert 2 == $x
```

Both `incr` and `decr` default to modifying the variable named by
their first argument by 1. The optional second argument may be an
integer value.

Note that the second argument to `incr` and `decr` may be a negative
value, thus `incr x -1` and `decr x 1` behave identically.

<a id='unset'></a>
Destroying Variables
===================

It's not normally necessary, but it is possible to destroy a non-const
variable from the scope that variable lives in. WHCL does not permit
script code to unset variables from older scopes (though C code can).

```whcl
decl x 1
assert [info is-local x]
unset x
assert ![info is-local x]
```

`unset` accepts any number of local variable names or properties
of arbitrary objects or arrays:

```whcl
decl o object {a 1 b 2}
decl a array (1 2 3)
assert 1 == $o[a]
unset $o[a]
assert undefined == $o[a]
assert 2 == $a[1]
unset $a[1]; # clears the entry but does NOT change the array's size
assert undefined == $a[1]
unset $o[a]; # no such member - silently ignored
unset o a
assert ![info is-local a]
assert ![info is-local o]
unset a; # will fail because `a` no longer exists
```

`unset` evaluates to the `undefined` value.

<a id='alias'></a>
Variable and Property Aliases (proprefs)
========================================

***ACHTUNG***: as of 2022-04-03, this support is brand new and it's
not clear whether it will have any undesired side effects which we can
neither live with nor mitigate. i.e. it's possible that this support
will be removed. Very hopefully not. So far so good. Even _too_ good,
which makes me sceptical.

The `alias` builtin command creates values which, when fetched, actually
fetch the value of a different variable or object property. Likewise,
when modified, they update the "remote" copy. These are colloquially
known as property refs, a.k.a. proprefs.

`alias` has the following uses:

- `alias varName` creates a reference to the first instance of the given
  var name which is found in the current scope chain. It fails if no
  such var is declared in the scopes visible to it.\  
  Achtung: aliases to variables (as opposed to properties - see below)
  is Extremely Bad for garbage-collection purposes. Solving that is
  TODO (we "just" have to revert to an older scoping model).
- `alias object[property]` creates a reference to the given
  object/property pair, noting that the property need not be a string:
  it can be any type which is legal for a property key.\  
  ACHTUNG: for this case, `alias` must set a default value (of
  `undefined`) if the property does not already exist. The reason is
  because if the property doesn't exist then it has a `NULL` value and
  future property lookups see that as "does not exist." This is
  normally harmless, but it's certainly not intuitive. Finding a
  workaround for that requirement is TODO.
- `alias array[integer-index]` works just like `object[property]` but
  aliases an array index entry instead of a property. It fails if
  given a negative index or an "unreasonably large" one (with an
  unspecified, but generous, upper limit).

It accepts the following optional flags:

- `-ro` indicates that the alias will be read-only. Attempts to modify
  the original through the alias will trigger an exception.
- `-weak` makes a _weak reference_ to the target container (or scope).
  A weak reference does not affect the lifetime of the target (e.g. it
  won't introduce cycles into the relationship) but also means that
  accessing the alias after the target container/scope is destroyed
  causes the alias to throw an exception.

It accepts an optional 2nd argument of a function. Passing that in
changes the semantics somewhat:

- When the property would be set, it instead calls that function
  with a single value: the new value for the property. Its return
  value is ignored.
- When fetching the property, the function is called with no arguments
  and its result is the value which gets fetched.

Exceptions triggered by such functions are, of course, propagated
normally.

In the context of a property interceptor function call, `this` is the
_original_ container of the property access (the one the alias refers
to). For scope-level vars, that `this` will be the object which stores
all vars for that scope. (Sidebar: scope-level vars are implemented as
an Object for which property lookup semantics are slightly different
than most objects.)

Once a propref is in place, it cannot be directly detected _as a
reference_ from script code (and seeing the difference requires using
special-case APIs in C code). The scripting engine's bits which do the
translation between references and what they point to happens very
close to the deepest levels of the cwal API, outside of whcl's sight.

`alias` is intended to be used in just about any place where a value is
acceptable. For complete examples see [the `alias` unit test
script](/file/whcl/unit/000-010-alias.whcl).

The intent is that the distinction between alias and "real properties"
is seamless in script code, but certain C-level APIs necessarily treat
proprefs as plain opaque values instead of as redirections. Such
APIs/operations include, but is not strictly limited to:

- `Object.copy-properties-to` copies proprefs as-is, essentially
  creating another path back to the original var/property.
- The `Tuple` constructor which copies an array.
- `unset` unsets the propref's key/value pair, not the proxied one.
  At the cwal C level, unsetting a property always disregards the
  propref special case.

Proprefs have the ability to enable a function to refer back to the
original reference of a variable:


```whcl
decl o object x 0
decl x 1
proc f {} {
    set y 2; # y is a call-local var aliasing x
    set z 1; # z is a call-local var aliasing o.x
} using {
    /* The magic happens here... */
    y [alias x]
    z [alias o.x]
}
f
assert 2 == $x
assert 1 == o.x
```

_That capability should be used carefully_, though, because referencing
a variable or property that way will cause the whole property
container to remain alive as long as the reference (which just really
points to _one_ variable or property) remains alive. That is, if a
function like the above one lived in a scope which had 10 values,
creating a propref to `x` would indirectly hold references to the 9
other variables in that scope. Those "extra" variables would hang
around in their parent container until (at least) the propref is
cleaned up. Such constructs are still subject to being vacuum'd up,
but are cyclic, so are inherently immune to sweeping.

In short: functions which are local to a given small scope, and will
not propagate out of it, have no down-sides with regards to the above,
but a function which propagates out of a scope normally won't want to
take a reference to the whole containing scope with it (keeping in
mind that its own variable key/value pair is part of that very scope).
