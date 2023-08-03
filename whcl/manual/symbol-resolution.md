# whcl: Symbol Resolution
##### ([&#x2b11;Table of Contents](./))

<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

- [Symbol Resolution](#intro)
- [Lazy Resolution](#lazy-resolution)
- [The Caveats](#caveats)
- [Lookup Across Call Boundaries](#across-call-boundaries)


<a id='intro'></a>
Symbol Resolution in WHCL
============================================================

whcl's symbol resolution initially followed the [highly unconventional
approach used by its predecessor,
s2](/doc/$CURRENT/s2/manual/symbol-resolution.md), but was changed on
2022-03-14 to use something more conventional but still _slightly_
different enough from potential expectations that it's worth
explaining and demonstrating in detail. This new approach is more
limiting than the prior approach but is also less likely to lead to
the shooting of oneself in the proverbial foot.

> Reminder to self: if this approach doesn't work out, we simply have
to make a small tweak to `whcl_var_search_v()` to restore the previous
behavior.

When whcl goes to look up a symbolic name (an identifier or variable
reference), it goes through the following steps, stopping at the
first one which resolves the symbol:

- Is it a [builtin value](builtins.md#bivs)?
- Look in the properties stored in the _current_ scope.
- If the current scope is _not_ a function call scope, look in the
  parent scope, recursively, until either a function call scope is
  checked or the top-most scope (a.k.a. the "global" scope) is
  checked. (A "function call scope" is the one started specifically in
  response to calling a function. It's the one where builtin variables
  such as `this` and `argv` live, as well as variables which are named
  after function parameters.)
- If the current scope _is_ a function call scope, skip directly to
  the top-most (global) scope and check there.

Many contexts which perform symbol lookup will fail if the
searched-for symbol is not found and some will fail if it _is_ found
in the current scope (e.g. you cannot declare a symbol twice in the
same scope).

The above might seem straightforward but it may leave a scripter
scratching their heads in certain cases. Most notably, functions
created with [the `proc` builtin command](builtin-proc.md) are, by
default, created in the _current_ scope. More often than not, the
current scope is _not_ the global scope. That means, for example, that
the following lookup may not behave as one expects:

```whcl
proc f {} { return 1 }
proc ff {} {
  f; # It may seem intuitive that f will resolve to the
     # function declared above, but that's not the case
     # unless f is global.
}
ff ; # will fail because it cannot resolve f.
```

The reason that will fail (if it's performed outside of the global
scope) is straightforward but possibly not intuitive, especially to
programmers used to languages with "automatic" closure support
(e.g. JavaScript).

Because functions are first-class values in whcl, they may be
referenced like any other values and may be propagated, e.g.  assigned
as properties in arbitrary objects or returned as the result of a
function or `eval` block. Because of that, it's never generically
possibly to know whether a given function is being called in the scope
it was initially declared in or not. The body of a function is, when
it's declared, converted to a form suitable for later evaluation, but
no state about its scope or any values it references (or doesn't) in
its body is available. The code in the function is only evaluated when
the function is called. It "would" be possible for a function
declaration to grab ahold of a reference to the scope which is active
when the function is created, but (A) that would have attrocious
side-effects with regards to garbage collection and (B) it's not
always obvious to an onlooker where the language injects new scopes,
so which scope _is_ the current scope might not always be apparent to
the user.

> Sidebar: OMG. Since the scoping overhaul merged into trunk on
2022-03-22, having a function grab a reference to its scope would not
have _that_ bad of an effect on garbage collection but would, more
often than not, capture far, far more state than the function needs
(and indirectly hold a reference to a bunch of state it doesn't
need). Something worth experimenting with, in any case, or maybe add a
new flag to `proc` which tells it to do so.

In order for the above `ff` to resolve `f` properly, any one of
several conditions must exist (in no particular order):

1. `f` must be a builtin value.
2. `f` must be declared inside of `ff`'s body.
3. `f` must be imported into `ff` with [the `using` modifier](builtin-proc.md#using)
   or [the `import-symbols` method](builtin-proc.md#method-import-symbols).
4. `f` must be declared in the global scope. This can be achieved with
   the `-global` flag to the `proc` command, but practice suggests that
   declaring functions as global is usually unnecessary.

We'll demonstrate those final 3 options below (the first one
not being possible without modifying whcl's C code to add the
new builtin value).

#2: a nested function

```whcl
proc ff {} {
  proc f {} { return 1 }
  assert 1 == [f];
}
ff
```

> Sidebar: *don't do that* (if `ff` will be called many times) because
it requires recreating `f` on every call to `ff`, which is relatively
expensive. Instead, prefer one of the options demonstrated below. The
exception is when the containing function is only called once, or
perhaps a few times in the life of a script. In such cases it can
actually save memory to use the approach demonstrated above.


#3a: `using`

```whcl
proc f {} { return 1 }
proc ff {} {assert 1 == [f]} using {f $f}
ff
```

#3b: `using` again (alternate (and more efficient) formulation)

```whcl
proc ff {} {
    assert 1 == [f]
} using {
    f [proc {} {return 1}]
}
ff
```

#3c: `import-symbols` (equivalent to #3a, above)

```whcl
proc f {} { return 1 }
proc ff {} {assert 1 == [f]}
ff.import-symbols f
```

#4: global `f` declaration

```whcl
proc -global f {} { return 1 }
proc ff {} {assert 1 == [f]}
```

<a id='lazy-resolution'></a>
Be Aware of Lazy Symbol Resolution
============================================================

The symbol resolution gremlins are lazy. They don't like to look
up anything until they're asked to and they don't like to remember
what they've looked up before.

To continue the previous section's example: because any symbols
referenced by `ff`'s default parameter values and its body are looked
up each time it is called, it is entirely possible that the `f`
they're referring to is a different `f` than was in scope in any prior
call. In some use cases this is desireable but in others it may be
_less so_. In order for `ff` to bind `f` to a _specific_ `f` instance,
and keep using that specific `f` instance (even if the global-scope
`f` is replaced), one of the following things has to happen:

1. The `using` or `import-symbols` approach must be used, as those
   resolve `f` immediately when they are invoked and retain that
   reference to `f`.
2. `f` has to be declared `const` so that it cannot be replaced,
   as demonstrated below.

In the general case, a const declaration looks like:

```whcl
decl -const f proc {} {...}
```

But the caveat is that script code doesn't normally run in the global
scope, so the above will not declare a global-scope `f`. One workaround
is to store `f` in the `whcl.client` object, which is reserved specifically
for client-side use, and `set` it const there:

```whcl
set -const whcl.client.f proc {} { ... }
```

<a id='caveats'></a>
The Caveats
============================================================

whcl's symbol resolution rules are different from all other cwal-based
languages so far. Generally speaking, it's an improvement in that it
keeps code from resolving symbols in potentially surprising ways.  On
the other hand, it has at least one significant caveat which the
previous model allowed for. Consider a function which takes a script
snippet as an argument:

```whcl
my-function {a script snippet}
```

It was historically (in whcl, prior to this change, as well as whcl's
predecessors) common to have such functions which did things
along the lines of:

```whcl
decl i 0
myDb for-each "select * from t" {
  incr i
  ... do something with this db record ...
}
```

Using whcl's current symbol lookup rules, the first line of that final
argument cannot resolve the symbol `i` (or will resolve one from the
global scope). Such constructs must now be reformulated as a callback
function, along the lines of:

```whcl
decl o object i 0
myDb for-each "select * from t" [proc {} {
    incr o.i
    ... do something with this db record ...
} using {o $o}]
```

The extra object (or array) around the `i` reference is necessary
because of how imported symbols work.

The caveat here, in case it's not clear, is the added complexity of
requiring a wrapper function, instead of a plain script snippet, and
an additional level of object/container wrapper for the results.

_Hypothetically_, but only hypothetically, it would be possible to
tell whcl to run such `eval`'able code blocks in the scope from which
their containing function was called. That would, however, not work
with current code because its structure is based on the engine's
age-old assumption that only the newest stack is ever active, and the
implications of breaking that assumption are akin to the proverbial
"crossing of the streams."

Those familiar with TCL might fairly say "just add `upvar` support."
The most correct answer to that is: much easier said that done. The
way vars are stored does not directly support such a thing, it would
require adding new tooling to the vars storage to able to handle it,
and it would be easily bypassable because that tooling would happen at
the whcl layer whereas property/variable access, at its lowest level,
happens via the cwal layer's API. Though it _would_ be trivial to add
a builtin which _resolves_ vars from scopes an arbitrary number of
levels up, it would be limited in terms of what it could do with them.
e.g. assignment of values through such resolution would not work.

(Hypothetical script code...)

```whcl
decl i
proc {} {
    echo "i =" [lookup -1 i]   ; # this would be easy to do, but...
    incr [lookup -1 i]         ; # ... could not work. i think.
}
```

It's worth experimenting with someday, though.

Having said all of that...

<a id='across-call-boundaries'></a>
Lookup Across Call Boundaries
============================================================

The `proc` builtin now [has the `-xsym` flag](builtin-proc.md#-xsym)
to tell it that calling that function does not create such a lookup
boundary.
