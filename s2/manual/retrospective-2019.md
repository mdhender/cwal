# s2: Retrospective 2019
#### ([&#x2b11;Table of Contents](./))
# Retrospective 2019

Jump to...

* [Retrospective 2019](#retrospective-2019)
* [The Elephant: Symbol Lookup](#retrospective-2019-symbol-lookup)
* [But Then There's…](#retrospective-2019-but-then)
* [require.s2 Needs to be a Core Feature](#retrospective-2019-require.s2)
* [Party Like it's C99?](#retrospective-2019-c99)
* [Strictness vs. Laxness](#retrospective-2019-strictness)
* [Porting of the Manual to Markdown](#retrospective-2019-manual)

<a id="retrospective-2019"></a>
# Retrospective 2019

It's Dec. 7th, 2019, and s2 has been alive and kicking for about 5.5
years now. It's seen far, far more use in my own projects and website
than i ever anticipated it would and it has held up surprisingly well.
That is to say, i have yet to have another project be put on hold
because of an s2 bug. (That's not say that s2 is bug-free, but serious
bugs, especially regarding lifetime-related memory misuse, have been few
and far between since the rules and implications of the lifetime model
\[finally\] sunk in and were \[finally\] followed consistently.)

<a id="retrospective-2019-symbol-lookup"></a>
# Symbol Lookup

In hindsight, there's really only one feature of s2 which consistently
grates on the back of my mind: its symbol lookup rules. They work.
They're trivial to implement at the C level because they're a 100% match
for the underlying data structures. They're 100% intuitive, in their own
way. Yet they're completely unconventional and, if one is not careful,
can have unexpected (and yet correct vis-a-vis s2's rules) results when
one attempts to use what appear to be local symbols across callback
scopes. For example:

```s2
var a;
const f1 = proc(){ a = 3; };
someFuncWhichCallsACallback( f1 );
```

That will work as expected, but only so long as the final function (or
one of its proxies) does not itself define the symbol "a". If it does,
the assignment in function `f1()` will either fail (if that "more local"
"a" symbol is const) or succeed with unexpected results, in that the
local "a" will, after the final function returns, not hold the value
3. e.g. consider what happens if `f1()` has a parameter named a:
`proc f1(a)...`. In that case, the assignment in the function is
simply overwriting the call-local variable `a`.

To be clear: in practice this doesn't happen, but that's because i'm
*extremely careful* about how data gets passed to callbacks, always
using a pattern similar to the following:

```s2
const a = [#0]; // a length-1 tuple
const f1 = proc(){ a[0] = 3; } using(a);
// ^^^ bind f1 to the desired "a" symbol
someFuncWhichCallsACallback( f1 );
// if f1() was called, a[0] === 3.
```

Noting that s2 resolves [`using` symbols](type-function.md#type-function-using) at the time `using`
or [`Function.importSymbols()`](type-function.md#type-function-importsymbols),
is triggered, which means that in order to be able to assign back over
state declared in an older scope, we need to use a container to hold
the result(s). The following variant will not behave as someone
familiar with JavaScript would expect:

```s2
var a;
const f1 = proc() { a = 3; } using(a);
…
```

… because of how `using` and `importSymbols()` work. Both resolve "a"
when the function is created and, for each call, declare symbol "a" as a
call-local variable with the same value it resolved to at the time
`using` resp. `importSymbols()` was triggered. They have no choice but
to resolve it then, as the scoping rules and lightweight internal
infrastructure does not leave them a choice (i.e. it's either then or
never). The infrastructure does not provide them the ability to hold a
reference to the *key/value pair* in the outer scope (i.e. the var
declaration itself), and that scope will, in many non-trivial cases, be
long gone by the time the callback function is called. Thus they must
resolve its value at the first opportunity and keep a reference to that
value (as opposed to being able to reference the original key/value pair
which holds the var mapping).

The advantages to this approach of implementing closures are that it's
easy to implement, it's memory-light, and it works well. The main
disadvantage is that one genuinely needs to understand how it works, and
why it works that way, in order to use it effectively. If cwal/s2 had
the *much heavier* infrastructure of JavaScript, the explicit `using`
wouldn't be necessary in order to implement closures. Memory-light has
always been a Prime Directive in cwal's/s2's development, however, and
sometimes trumps usability concerns.

That's sort of a tangent from, but it's closely related to, this
sections initial point: it "would be cool" if s2 (somehow) implemented
more conventional symbol lookup rules but, to be honest, i don't think
it possibly could given the "lightweightedness" of its infrastructure.
It literally works by tokenizing and parsing a single token at a time
and doesn't keep any more state than it absolutely needs in order to
process that token. It doesn't generally know, at any given token,
whether it's in a function call, whether such a call is from a function
defined within another function or globally, or any sort of similar
state. Without such details, it's seemingly impossible to say exactly
which scope paths a more conventional symbol lookup implementation
should follow. As off-the-cuff examples:

```s2
const f1 = proc(){…};
// ^^^ f1() has no idea whether it's being defined within a function-call
// scope, the global scope, or some other scope.

const f2 = proc(){ … f1(); … };
// ^^^ f1() does not know it's being called from within f2().

foo( proc(){ … } );
// ^^^ the anonymous function has no information about its call context.
scope { … }
/* ^^^ scope's contents have no idea whether they're in a
    function call, or even that they're in an explicit scope block.
    They only know that "some" scope is available because the
    engine requires that one scope is always active. */
```

Given this lack of call context, and the inability (in s2) of a
function, at call-time, to reference state which was defined
in/accessible to its declaring scope but not imported into the function
with `using` or `importSymbols()`, they only reasonable symbol lookup
options we have would be to either do what we're doing now or to... ?

So… even though the current situation bugs me somewhat, i see no better
solution which would be implementable within the infrastructure the
framework provides, aside from adding new, memory-heavy infrastructure,
but such a cost goes against the framework's Prime Directive to such a
degree that the current symbol lookup mechanism bothers me far less than
that cost would.

<a id="retrospective-2019-but-then"></a>
# But Then There's…

Despite the symbol lookup situation, there is much about the language
which i genuinely like and appreciate:

-   Its resource-lightness. Considering how lightweight it is, it's
    remarkably capable.
-   It's relatively performant, provided one remembers to import "far
    away" symbols to local scopes via var aliasing and the `using`
    keyword.
-   The everything-is-an-expression syntax. When i work in JavaScript, i
    genuinely miss the ability to be able to type things like (x ||
    return).


<a id="retrospective-2019-require.s2"></a>
# [require.s2](../mod/) Needs to be a Core Feature

Practice in client-side trees which make heavy use of s2 (namely my
CGIs and website generator) has shown that the ability to load
"well-known" scripts on demand without knowing whether they live in
the filesystem is a must-have feature. The [require.s2 module
loader](../mod/) has proven invaluable, but the catch is that
standalone scripts don't generally have access to it unless they load
it themselves, which requires an unfortunate bit of bootstrapping on
their part. This can easily be resolved by using an [`s2sh.s2` init
script](s2sh.md#s2sh-extend-with-script), but i often disable loading
of that script for certain uses: running s2 tests and CGIs. The build
tree's module infrastructure has matured to the point where we can
compile script-implemented modules directly into s2sh, but loading
require.s2 this way isn't ideal because a script embedded in C code
has no useful `__FILEDIR` value, meaning that require.s2 cannot
automatically determine its "home" directory and therefore cannot
automatically configure its search path based on that. That's largely
resolved by setting an [`S2_HOME` environment variable](s2-home.md)
and requiring that [require.d (require.s2's home)](../mod/) live in
`$S2_HOME`, but that's an out-of-tree convention, not something we can
technically enforce.

It "would be really cool" to reimplement require.s2 in C, so that we
could always be sure it's in the engine, even in clients other than
s2sh (and when the core prototype methods are not installed), but that
would be a *major pain in the butt* to do. Failing that, extending
`s2.import()` with a path-search feature like the script-side
implementation like the one implemented [in this
s2sh.s2](/finfo/s2/s2sh.s2) would be a good step in that direction. We
"could" embed that script snippet directly into s2sh, but we cannot
"reasonably" install it in the core because it requires the symbol
"s2", which the engine does not require that a client install (the
global s2 symbol is installed by s2sh, not the core engine (which
doesn't require it)).

Somewhat related: while i initialy planned to make `import` a keyword
for importing files, the more i think about it, the less i like the
idea of including I/O at all in the keyword-level features. Yes, we
have the `s2out` keyword for generating output, but that's because
nearly every embedding app will want to be able to generate output in
some standardized way. s2's output channel can be defined by the user
when s2 is initialized, so `s2out` could be redirected to whatever UI
component the app sets aside for that purpose. Redirecting input from
a hypothetical `import` keyword would be a different matter entirely.


<a id="retrospective-2019-c99"></a>
# Party Like it's C99?

cwal has, since its inception, been strictly C89 with one single
exception: it requires the *&lt;stdint.h&gt;* and *&lt;inttypes.h&gt;*
headers specified by C99 for their fixed-size integer types and
well-defined printf-style format specifiers for those types. (You might
not believe what a hassle unportable printf-style format specifiers can
be when switching platforms, and C99's inclusion of them is a godsend.)
In particular, C99's family of integer typedefs, like *int32\_t* and
*uint64\_t*, are integral parts of cwal. Aside from those headers, which
have been available on almost all C89 environments i've tried, cwal has
*actively* avoided any other C99-specific features, mainly for perceived
portability reasons. (MSVC versions, at the time this project was
started, reportedly never fully supported C99.) Those
perceptions/reasons may be misguided, though. It "would be cool" to be
able to integrate C99's math library and not-a-number support into s2,
but doing so would require prying my cold dead fingers from my C89
compiler and moving into the 21st century.

Updating to C99, which is now 20 years old and as widespread as it will
ever be, *should* be a no-brainer, and yet i've no clue why this
decision bothers me so much. :/ *Sigh*.

<a id="retrospective-2019-strictness"></a>
# Strictness vs. Laxness

Some languages are lax in what they'll allow as input. e.g. JavaScript
allows an extra comma at the end of an object literal, even though that
extra comma adds nothing to the language (it's presumably just there to
accommodate particularly lazy code generators, which commonly leave
trailing commas). PHP allows *all sorts of stuff* which would trigger a
fatal error in any sane language. (A failed `assert()` in PHP just keeps
on running the code after the `assert()`. Facepalm.) Because i've been
bitten by those sorts of things far too many times, it's always been
s2's policy to be as strict as feasible and, if a given case proves to
be too strict, to laxen up later. That approach is, long-term, far
simpler to implement than lax-first, strict-later, as the latter, more
often than not, requires breaking client-side code, which isn't always
feasible/realistic.

i do not regret this decision one bit.

That is *not* to say that s2 is 100% strict, especially with regard to
ensuring that its grammar (for which there is no formal structure) is
adhered to. It will, at times, allow "illegal code" to be silently
ignored solely because checking for and validating it would, in s2's
parsing model, be not only expensive but also completely unnecessary:

```s2
proc(){
   print(__FLC);
   return;
   anything after the return is junk, but will only trigger an
   error if the tokenizer, as opposed to the parser, does not
   like it. The parser quits caring what is in the function
   as soon as "return" is evaluated. The tokenizer, on the other hand,
   expects to find only syntax-legal tokens here. Note that we do not
   use contractions here - that is to keep the tokenizer happy by
   avoiding creation of any unbalanced apostrophes (string literals).
}();
```


<a id="retrospective-2019-manual"></a>
# Porting of the Manual to Markdown

s2's manual has historically (since 2014) been maintained in [Google
Docs](https://docs.google.com), an amazingly convenient and powerful
online editor. In December 2019 i was finally pushed over the edge by
an admittedly tiny bug which Google has neglected to fix all these
years, and decided to migrate the manual to Markdown, largely due to
built-in support for markdown in the [Fossil
SCM](https://fossil-scm.org) (my SCM of choice). The bug: GDocs
randomly breaks intra-document links. The 160-ish pages of this manual
contained tons of intra-doc links, and random ones would simply stop
working at times. It was also very easy to break them in the process
of via perfect legitimate document maintenance: cut and past a chapter
to a different location and every intra-document link pointing to/into
that chapter breaks. *Sigh*.

So... porting so many docs (160-ish pages of the main manual plus
35-ish of [loadable module docs](../mod/)) was admittedly daunting, but
ultimately... i *seriously* should have done this years ago.

The cons of the new form:

* i can no longer just hop into GDocs (in a browser or an Android app)
  and edit at will. That is GDocs' killer feature.
* The document is no longer a single, printable file. That said, the manual
  is too long to print out, anyway, so that's just a hypothetical limitation.
* We can no longer easily export the doc, as a whole, to other formats, e.g.
  OpenDocument or whatever the cool kids are using these days. Doing so would
  require a converter which can translate the inter-doc links during the
  conversion process. Oh, well. No big deal, really.

That's about it.

Pros:

* The Fossil SCM (cwal's home) natively supports what it calls
  [embedded
  docs](https://fossil-scm.org/home/doc/trunk/www/embeddeddoc.wiki) in
  three different formats, one of them being \[its own dialect of]
  markdown. In short, this feature allows the documentation to partake
  in the SCM's versioning features and allows users to browse any
  given version of the documentation with ease.
* When edits to the docs are committed/checked in to Fossil, those
  changes immediately go live on [the project repository's site](/),
  without requiring any additional action on the user's part.
  (If you're reading this online, you're reading a copy served by
  the SCM).
* Though markdown is considerably more limited in terms of formatting
  the docs than a full-featured word processor, we (the royal we) have
  absolute control over structure and links between/within
  documents. (i often wanted to make structural changes to the GDocs
  manual but could not without breaking countless intra-document
  links, so didn't :/.) In the end, structure is more important than
  formatting, so this limitation doesn't bother me much (and does, in
  fact, free me up from the *temptation* of endlessly micro-formatting
  stuff).
* The new structure, with pages focused on individual topics, seems,
  to me, to be easier to digest than a monolithic document. The
  monolithic manual was a bit of a beast, and difficult to jump back
  and forth between sections because GDocs lacks a "back to previous
  point in the document" feature like we inherently get with
  markdown/HTML-based docs.

The overwhelming majority of the docs - all but some of the cruft and
fluff - were ported in approximately 3 days or so, but it was easily
30-ish hours of effort. Most of the docs were ported by exporting
GDocs to OpenDocument, then from OpenDocument to markdown.  Several
other paths were quickly attempted, including via LaTeX, but the
results were less usable. That said, i did not invest large amounts of
time into looking for an "optimal" solution.
