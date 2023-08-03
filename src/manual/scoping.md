# Scoping and Memory Management
##### ([&#x2b11;Central cwal Documentation Hub](./))
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

Achtung: these docs are very much in-progress, being ported and
rewritten from older documentation.

See also: [Value Type model](values.md)

Scoping and Memory Management in cwal
=====================================================================

cwal, at its core, is essentially a highly-specialized memory
management layer. Its primary use is managing memory of so-called
Values on behalf of client code. This document covers the various
abstractions it uses for such management. A summary of the types of
memory cwal manages:

- First and foremost, [Values](values.md). That's the sort of memory
  management this document focuses on. The remaining types are
  "optimizations" - implementation details which do not directly
  impact client code.
- Generic buffers, plus list memory for arrays and hashtables, are
  managed via the same mechanisms.
- String memory is a special case for reasons we won't go into here.

Historically, in particular cwal's first 8 years, an inexplicably
tremendous emphasis was placed on squeezing every possibly byte out of
cwal's core. As of Summer 2021 that's been relaxed slightly, but low
memory usage is still a fundamental goal of the core library. The
higher-level client code, e.g. [s2][] and [whcl][], places less
emphasis on sweating blood to trim every byte while still aiming to be
memory-efficient.

All memory within a cwal session is managed by a single `cwal_engine`
instance. It's not possible, in cwal, to allocate memory without such
an instance. In practice, cwal engines (as they are called) are
themselves stack-allocated or embedded as a member of a higher-level
type.

> Sidebar: the tying of all memory allocation to a specific engine
  instance has proven to be a mixed blessing. On the one hand, it
  gives us a great deal of freedom in how we internally optimize the
  use of that memory. On the other, it's not possible to use generic
  features of the library (like the `cwal_buffer` generic memory
  buffer class) without first instantiating and initializing a cwal
  engine instance (which, admittedly, is a bit "involved").

<a id="cwal_scope"></a>
cwal_scope
============================================================

The `cwal_scope` class, hereafter simply referred to as "scope," plays
two roles in cwal, one of them optional:

1. Every Value belongs to a scope. Initially it belongs to the scope
   which is active when the Value is created, but Values are often
   rescoped into older scopes. This affects only the lifetime
   management, not the client-side visibility of the Values.
2. It _optionally_ offers a feature for storing and fetching
   variables. [s2][], for example, makes use of this, whereas [whcl][]
   implements its own in order to impose different symbol lookup
   rules.  This particular feature of the class is considered to be a
   convenience API for certain clients (or bootstrapping new ones),
   not a core-most feature, and we won't go into detail on that
   feature here.

The library's scoping model is based on the assumption that scopes
work more or less how they do in single-threaded C/C++: only _one_
scope is ever active and that scope is _always_ the newest scope in
the stack of scopes. The library's lifetime management very much
depends on that assumption and its internals require that scopes
logically be managed like a stack, though it does not actually require
(nor use) a stack data structure for doing so.

> Sidebar: we tend to refer to scopes as "older" and "newer" instead
  of "higher" or "lower" because stacks of this sort are commonly
  envisioned as growing from the top-down, which is just plain weird
  and often leads to terminology confusion on my part.

> Trivial: in practice, scope instances tend to be stack-allocated in
  most client code, pushed at the start of some client-side C function
  and popped at the end of that function. [whcl][] allocates them on
  the heap for reasons of its own, but it's an outlier in that regard.
  It still pushes and pops them in the same way, behaving like a
  conventional stack.

Scopes keep track of Values in several linked lists, each of which is
primarily used for constant-time operations. The lists include:

- "Temporaries," as described [in the section on reference
  counts](#refcount).
- "Vacuum-proof," a.k.a. vacuum-safe, Values are Values which are, in
  short, safe from being destroyed by [the vacuum algorithm](#vacuum).
- Everything else: Values which are neither vacuum-proof nor
  temporaries.  Note that Values which one might reasonably call
  "script-visible" inherently fall into the category of "vacuum-proof."

Though the majority of scope-level list manipulation is O(1), scope
cleanup has an O(N) factor directly related to the number of Values
the scope owns and the number of reference points they consume.
[Sweeping](#sweep) is a "fast O(N)" operation, with N being the number
of temporary values to clean up. [Vacuuming](#vacuum)'s computational
complexity is... it's almost certainly worse than O(N), but how much
worse (or whether it's just got a potentially large N) is not clear.
Despite being able to conceive of and implement it, determining its
computational complexity is a bit above my proverbial pay grade.  (As
nobody's paying for cwal's development, though, it seems only
appropriate that some of its edges are "not quite finely-sanded.")


<a id="refcount"></a>
Reference Counts
============================================================

Every cwal-managed Value, except for builtin constants, has a
reference count (a.k.a. refcount) which provides the most fundamental
basis of lifetime management. Were it not for the ability to create
cyclic data structures, refcounts would be the first and last line of
defense in managing Value memory. Cyclic data, however, very
effectively breaks reference counting. So why do we use refcounts?
Because it works great until cycles are introduced and most data
structures do not contribute to cycles.

### Counting Starts at Zero

When a new Value is created in cwal, its initial refcount is _zero_,
not one.  Why?

1. Because an old article from C++ guru Scott Meyers once suggested
   that it's overall easier to manage when the client is required to
   both give and take the initial refcount point.
2. Experience has unequivocally bore that out. When a Value comes into
   existence with a single refcount point, certain usage patterns
   introdue a great deal of uncertainty because the person who has the
   Value in their hand cannot know, with certainty, whether the single
   refcount point a Value is was from them or some other source. This
   point won't be drawn out in detail, but suffice it to say that
   practice has shown the "start at 0 instead of 1" approach to be a
   far, far better fit for working with this library, primarily
   because it allows us to always unambiguously know when to get and
   release a reference point. Certain features, e.g. the built-in
   string interning or fetching a property via a property interceptor
   method, both of which operate behind the scenes where clients do
   not see them, otherwise make the "start at 1" approach _completely
   untennable_ client-side.

Early versions of cwal experimented with both approaches and the
start-at-0 approach was the _clear_ winner in terms of providing
unambiguous guidance to client-side code about Value ownership. The
start-at-1 approach left too many cases ambiguous for reasons we won't
belabor here because, frankly, the library is long past the point of
no return on that design decision. (Later cwal developments, such as
property interceptor methods, where returned Values may or may not be
new or may or may not be owned by someone other than the one receiving
them, "really do" require the start-at-0 approach for well-defined
refcounts.)

Values with a refcount of 0 are tracked in their own category, called
"temporary" or "probationary" Values. Normally when a Value's refcount
is reduced from 1 to 0, or would be decremented when it's already at
0, the Value is immediately cleaned up, but it turns out that it's
_exceedingly useful_ to be able to reduce a Value's refcount before
propagating it back up to client code without risking destroying it,
noting that the code on the receiving end might or might not take
notice of the Value and might abandon it altogether, effectively
stranding it in their current scope.

The act of reducing a Value's refcount is colloquially called
**unreffing** and the special case of reducing it to to 0 _without
destroying it_ is colloquially known as **unhanding**. Unhanding is
the basis of sane refcount management in the face of Value propagation
(e.g. via "return" Values and the like). Though cwal very probably did
not pioneer this capability, it doesn't (to the best of my limited an
fallible knowledge) seem to come up "in the literature" regaring
reference counting.

> TODO: demonstrate examples of where unhanding is useful (or even
  required) for sane refcount management. Simply for argument's sake,
  also demonstrate cases where a starting refcount of 1 is
  problematic. Though i truly hate to harp on it, starting as 0 truly
  is the optimal solution, however counter-intuitive that may seem.

In any case, refcounts are the first line of management of Value
lifetimes.


<a id="refcounts"></a>
Scope Cleanup
============================================================

The final arbiter of a Value's lifetime is the Value's owning
scope. Initially this is the scope the Value is instantiated in,
but Values are often moved into older (_never_ newer!) scopes for
lifetime management purposes. For example:

- Propagation of a Value using "return semantics" requires propagating
  the Value up through one or more dying scopes.
- Inserting a Value into any container will move the Value into the
  container's scope.
- When a Container-type Value is rescoped, its children are also
  rescoped, recursively. (Though this initially sounds like it would
  be slow, no small amount of practice has shown it to be "plenty
  fast" for all use cases to date. Knock on wood.)

To reiterate: such reparenting affects only the Values' lifetimes, not
the app-side visibility of the Values.

A side-effect of this model, and an invariant of the library's Value
management model, is that it is never possible for a Value in an older
scope to refer to/contain a Value which lives in a newer scope because
the act of referencing/containing such a Value will "upscope" the
Value into the older scope for lifetime management purposes.
Conversely, it is possible for a Value in a newer scope to
reference/contain a Value which is owned by an older scope. Because
only the newest scope is every permitted to be active, Values refering
to those in older scopes is perfectly valid and permits them to do so
without impacting the lifetimes of the referenced values.

> When we say "reference" there, we don't specifically mean "holds a
  refcount point," though that is essentially what it amounts to. More
  generally, "reference" means (in this context) "holds a pointer to,"
  typically in the form of a key/value property or a list entry. Every
  time such a pointer is held, a refcount point is also held, so the
  difference there involves some hair-splitting.

In short, the scope which owns a Value is the oldest scope which has
ever referenced that Value, perhaps indirectly (keeping in mind that
containers recursively upscope their contained Values, if needed).

When a scope is popped from the stack of scopes, it is cleaned up and
any Values which it owns are cleaned up with it. The engine has
mechanisms in place for "specially-propagating" Values in order to
implement return-style and exception propagation semantics, such that
those can propagate automatically with only a minimum of client-side
handling. Barring the two "specially-propagating" Values and any
optional _explicitly_ propagating Value, popping a scope will
irrevocably destroy any Values it owns, invalidating any pointers held
to those Values.

> Sidebar: the simplest to make, and most common, mistake in cwal
  client code is using a Value pointer which was cleaned up.  The
  library has _tons_ of `assert()` statements to try to inflict pain
  on clients who use such references, but tracking down the origin of
  such misuse after the Value has been reaped is _notoriously
  difficult_, primarily because the engine recycles almost everything
  and the pointer, though _semantically_ invalidated, is still
  _physically_ valid.  Disabling cwal's Value recycling feature is
  recommended during testing in order to provoke earlier failure of
  such misuse.  If the Value in question is hidden inside cwal's
  internal recycling subsystem then a client misusing such a Value
  might not realize they are doing so until they trigger an `assert()`
  in cwal (and then only if it's built in debug mode (which is
  _always_ recommended!)). Using such a Value is a sure-fire way to
  corrupt cwal's internal state.

The algorithm for cleaning up a scope is somewhat counterintuitive.
In short, skipping over some of the wrenches which get thrown into the
works, the scope simply keeps unreffing the head-most Value of its
internal linked list(s) until the lists are empty. Coupled together
with a so-called garbage-collection queue (which holds destructed
Values in memory long enough for their being-destroyed counterparts to
finish walking over any pointers to them), this approach iteratively
cleans up all Values it owns. The devil is, of course, in the details,
but the core of the algorithm is simply "keep unreffing the head until
there is no more head," noting that the head will be replaced with the
next value in the chain if that unref destroys the head value.

<!--

<a id="cycles"></a>
Cyclic Structures
============================================================

Cyclic data structures - those which refer to themselves, perhaps
indirectly, via multiple pointer paths - are the bane of any
garbage-collected environment. Were it not for cycles, developing
garbage-collected languages would be much easier, but the resulting
environments would be far, _far_ less useful.

-->

<a id="sweep"></a>
Sweeping
============================================================

In cwal, the term "sweep" doesn't _really_ have anything to do with
what is commonly called, in the context of garbage collection, ["mark
and sweep."][mark-sweep] Abstractly, yes it does, but it's implemented
quite differently and any relationship with classical mark and sweep
is coincidental. That is, it was (for better or worse) implemented
without the benefit of having studied any literature on mark/sweep
(because "how hard could it possibly be?" 😉).

In cwal, sweeping is simply the act of cleaning up any Values _in the
current scope_ which have a refcount of exactly zero. Recall that
[cwal not only permits Values to have a refcount of zero, but often
very much relies on that capability](#refcount). A "recursive sweep" -
sweeping up temporaries in all scopes in the stack - is safe only if
all Values in all older scopes which require a reference actually have
one. In practice, a recursive sweep does not clean up much, if
anything, more than a scope-local sweep because scope-pushes tend
(rightfully!) not to leave refcount-0 Values laying around before
pushing new scopes.

> Sidebar: despite relying on the ability to have 0-refcount values,
  the cwal core internals make sure to _never_ call back into client
  code (e.g. via calls to client-bound Function-type Values) without
  first adding a refcount point to all relevant values which it's
  currently working with. In such cases it will also ensure that the
  Values are, for the duration of that usage, vacuum-proof.

All Values with a refcount point are immune to being swept up. They can,
however, be [vacuumed](#vacuum).

> Trivia: early uses of cwal were not careful enough with where they
  added references, leading to much pain when algorithms such as
  recursive sweep were first attempted because it kept wiping out
  Values which code in older scopes expected to stay alive. Much as
  been learned in cwal's first decade, however, and "modern" cwal
  client code is far more thorough and careful, permitting not only
  recursive sweeping but also recursive [vacuuming](#vacuum).


Interestingly, though, the core cwal engine itself _cannot trigger the
sweep algorithm_ because it cannot know when it is safe to do so,
"safe" meaning "won't destroy Values a client is using in native
code." Client-level code, e.g. a scripting language, generally can,
though. So cwal provides the algorithm but leaves its invokation up to
the client (who is never _required_ to invoke it but very possibly,
depending on the client, should do so from time to time). [s2][] does
so after processing every N expressions, where N is some configurable
value, and [whcl][] does so after processing every N commands.


<a id="vacuum"></a>
Vacuuming
============================================================

The so-called "vacuum" algorithm in cwal's answer to cleaning up
cyclic data structures like the one represented by this [whcl][] code:

```whcl
decl o [object]; # create a new Object-type Value
set o[$o] $o; # o[$o] now refers to o. Yes, $o is a valid key.
              # refcount==3: (var) + (o[$o] (property key)) + (property value)
unset o     ; # refcount==2. The value is now alive but unreachable from script code.
```

In short, vacuuming applies a trivial algorithm which takes advantage
of various cwal-internal invariants to weed out Values which are not
what is commonly called "vacuum-proof" (and it does so indirectly,
with very little code and no voodoo). Values which are visible to
script code are inherently vacuum-proof and the library provides a
mechanism for specifically flagging any given Value as vacuum-proof,
so that C code can manage Values which are not exposed to script code
but also not subject to being garbage-collected out from under the C
code. Any Value which is contained in a vacuum-proof Value, or
reachable via such a container entry, no matter how indirectly, is
also vacuum-proof.

Similar to [sweeping](#sweep), cwal itself does not have enough
information to know whether it is safe to run the vacuum algorithm, in
the sense of whether it can be run without pulling in-use Values out
from under the client.  It offers the feature up for client-side code
to invoke, but does not strictly require that it be used. Both [s2][]
and [whcl][] replace every N calls to the sweep algorithm with a
vacuum instead, N being some configurable value. Though vacuuming
infrequently is best for performance, running it as frequently as
possible is helpful during development as it often exposes Value
lifetime mismanagement in client-level code by triggering one of the
hundreds of `assert()` checks in the cwal core. After development has
ironed those out, switching to less frequent vacuuming provides a
significant performance boost at the _potential_ price of higher
peak memory usage.

Vacuuming, as currently implemented, is only safe to do within the
current (i.e. newest) scope. Applying it to older scopes very often
destroys Values which are referenced by newer scopes, leading to
memory corruption. Until/unless a vacuum algorithm can be found which
can work across scopes, cwal will continue to have certain "GC
deathtrap" cases.


[s2]: /wiki/s2
[whcl]: /doc/ckout/whcl/
[mark-sweep]: https://www.geeksforgeeks.org/mark-and-sweep-garbage-collection-algorithm/

