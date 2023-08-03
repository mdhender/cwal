# s2: Symbol Resolution
#### ([&#x2b11;Table of Contents](./))
# Symbol Resolution

The topic of symbol resolution is second-nature to programmers,
but s2's symbol resolution rules are... how to put this diplomatically...
*unconventional*. Please read this section *carefully* to understand
how s2 resolves symbols in scripts...

s2's scope-level symbol lookup resolution rules are intuitive, in their
own way, but extremely unconventional: when a non-keyword identifier is
found outside of a property access operation, the engine looks in the
current scope for it. If it's not there, it looks in the previous scope
(the one which started this scope), etc., all the way up the chain of
scope-pushers. This effectively means *the entire call stack*, back up
to the global scope if needed. It resolves its symbols from *anywhere*
in the *current* scope/call chain:

```s2
var f = function(){ return x }; // where does 'x' come from?
var ff = proc(){ var x = 1; return f(); } // from here!
assert 1 === ff();
f(); // will throw because it cannot resolve x anymore.
assert 3 === f(var x = 3); // Wha!?!? (Welcome to s2, baby!)
assert !typeinfo(isdeclared x);
```

That might look superficially familiar to JavaScript programmers, but
the above has these *notable differences* from JavaScript:

1.  `x` is not bound when the code for `f` is first traversed, but
    resolved anew each time `f` is called.
2.  `f()` only resolves "our" `x` as long as `x` itself is still in
    scope. If a "different" `x` is in scope at call-time, *that* `x` will
    be used.
3.  `f()` will resolve `x` even if `x` is a local variable (including
    function parameter names) of a *different* function which (possibly
    indirectly) has called this function.
4.  If `x` is not resolvable at call-time, s2 will throw an undefined
    symbol exception.

Here's another one:

```s2
const f = function(){ assert typeinfo(isstring x) };
const fx = function(x){ f() }; // note the x parameter!
fx("hi"); // okay, but...
fx(); // will throw b/c x (declared parameter of fx())
      // has the undefined value, so the assert in f() fails.
```

Now imagine that sort of symbol lookup going up 10 scope levels,
across function calls, even spanning script files if
needed. Contrariwise, if the Function object is moved into a scope
where no `x` symbol is resolvable, calling the function will trigger
an unknown symbol error. Another possibility is that `x` is (at the
time of the call) an entirely different (semantically speaking) symbol
altogether than the one the function is expecting, potentially leading
to a *semantic* error while the script keeps chugging happily along
because it could resolve `x`. Alternately, a simple `++x` intended for
an older-scoped `x` might increment a different `x` which has been
declared somewhere between the intended target's stack frame and the
current call stack frame. To be clear: in practice that has never
become a problem, and would require a really unusual construct, but it
is a hypothetical problem (but is real in that it's easy to construct,
just highly unlikely to do by accident).

[`Function.importSymbols()`](type-function.md#type-function-importsymbols)
and the [`using` function definition
modifier](type-function.md#type-function-using) can be used to bind a
specific instance of symbol resolution to a function for long-term use
(as well as speed up resolution of far-away symbols), and they are
a staple of any non-trivial s2 code.

Assignments can overwrite non-const symbols living in higher-up (older)
scopes, but the `unset` keyword is restricted to working on symbols
declared in the current scope (or container properties, regardless of
the container's owning scope).

That's life in s2's symbol resolution world. While it initially sounds
scary, in practice it has not proven to be problematic, and has
sometimes even [*proved
useful*](/info/3ea486a3e5e4864730d1e460630a94208cfafa74?ln=60-67).

Nonetheless: prefer `const` over `var` where feasible, to help avoid
unwanted side-effects from deeper scopes assigning over your variables!
(That said - i've never actually seen that happen in well-mannered
script code, but it's trivial to construct such cases intentionally.)

Why resolve symbols this way? Because, to be honest, nothing better
\[which fits current code\] comes to mind and this approach is trivial
to implement because it's a 100% natural fit for the underlying C
structures and calls. A more conventional solution would require that we
create artificial border-points which interrupt symbol lookup, or come
up with a mechanism for specifying which namespaces are in effect at any
given time, and that's difficult to do (*philosophically speaking*) when
functions contain functions which contain functions (*ad nauseum*) which
might eventually be propagated up outside of the original outer
function. Likewise, there is not (insofar as script code can tell) any
single, well-defined global scope - an app may push any number of scopes
before it runs a given script, and each script (unless C code runs it
otherwise) runs in its own scope. Where does symbol resolution begin and
end in such a world, if not simply along the current call stack scopes?
That's not clear to me. Suggestions are welcomed, and s2's symbol
resolution code is encapsulated enough that it "should" be fairly simple
to experiment with other approaches without requiring a code overhaul or
major breakage.

Reminder to self: since we now have `s2_scope`, we could move the var
decls there and give each scope a flag telling us whether or not it
blocks lookups (is a function call boundary). That would break a good
bit of code, some of it irrevocably ([require.s2's *requireS2*
symbol](../mod/require/), but that could be
[`define`'d](keywords.md#keyword-define) if needed), but it might be
saner.
