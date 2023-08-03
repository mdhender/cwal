# WHCL - the WanderingHorse.net Command Language
##### ([&#x2b11;Central whcl Documentation Hub](../))
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

**Status (2022-02-23)**: whcl's core-most bits are "done" and working,
as are the core-most script-visible APIs. There is still refinement and
documentation to do, but what's there works.

--------

Other docs:

- [Building whcl](build.md)
- [Grammar](grammar.md)
  - [Symbol Resolution](symbol-resolution.md).
- [Data Types](type-intro.md)
- [Builtin Commands and Values](builtins.md)
- [API Index](api-index.md)
- [whclsh: the whcl shell](whclsh.md)
- [Example scripts](/dir/whcl/unit?ci=trunk) (whcl's own unit tests)
- [Tips and Tricks](tips-tricks.md) for getting the most out of whcl
- [Tokenization and Evaluation Models](t10n.md)
- [TODOs](todo.md)

# WHCL?

```whcl
echo "Hello, world!"
```

WHCL, the WanderingHorse.net Command Language, is cwal's fourth
programming language, a "command-centric" language strongly influenced
by TCL. Since TCL is, by its advocates, pronounced "tickle," WHCL is,
by its developer, pronounced "whickle."  (It is probably a good thing,
then, that it's not called DCL. The name PCL was avoided because it's
already in use by at least two unrelated programming languages.)

WHCL has reached a usable state, considering its limited intended
scope (namely, for driving unit tests for C/C++ code, not developing
applications). However, it's still an experiment at this stage and may
undergo any number of changes.

Though it's convenient to think of WHCL as a TCL semi-clone, some
of the significant outward differences are:

- TCL's EIAS ("Everything Is A String") approach is not a real thing
  here. It's a half-thing. Numeric literals are internally represented
  as their appropriate numeric types, as are the other object types
  supported by cwal (Array, Object, Function, etc.). {Squiggly-block
  constructs} are treated as strings, subject to command-specific
  parsing, similar to TCL, but...
  1. They may only contain WHCL-compatible tokens. The interpretation of
     those tokens is up to the client, though.
  2. "Bare words" which equate to strings are not a thing. e.g.  in
     TCL we might write:\  
     `command /some/path`\  
     whereas WHCL requires:\  
     `command "/some/path"` (or similar: there are several ways to quote strings)\  
     The reason is simply that the former
     requires making some truly awkward decisions at the tokenization
     and parsing levels. For example is `12z` a malformed integer or
     a string? In WHCL it's the former unless the user quotes it, in
     which case it's a string.  That difference is somewhat
     unfortunate but the fact is that *not* doing so opens up cans of
     worms which add unnecessary decision-making to the
     language-design process. WHCL can, however, handle identifier-like
     words without quotes, and has extra support for dashes in such
     names, so common TCL constructs like (`command -a-flag blah`) are
     possible in WHCL. Client-side C code bound to such commands will
     receive the tokens as strings. WHCL-builtin features would see the
     complete token state, which means that they would know, without
     internal processing, that `-a-flag` is a "flag" token and `blah`
     is an identifier. How/whether such token state can be sensibly
     passed on to client-side C code is still an open question.
- Functions are first-class values.
    - Function calls can be chained. i've yet to figure out how to do
      this in TCL. This makes for much more succinct code in some
      cases, at the cost of legibility (because the TCL-inspired call
      syntax gets rather ugly when chained more than two levels).
- Scoping is more C-/C++-like. Whether or not a given block-construct
  introduces a new scope is up to the command which processes it, but
  each new command is started in its own scope (for garbage collection
  purposes as well as sanity's sake). This exceptions are the built-in
  commands which need to run in the current scope in order to function
  properly.

cwal's [s2 language][s2] has long been the centerpiece and crowning
achievement of the cwal project, and cwal was designed for languages
like s2 in mind. WHCL represents a completely different style of
language, and certain challenges vis a vis prior cwal-based works, are
already visible. None of them seem unsurmountable, though, and WHCL
would be a better fit for one of cwal's original language targets:
mini-languages used for driving unit testing for C/C++ libraries. Though
it is extremely tempting to simply use [JimTCL](http://jim.tcl.tk/) for
this purposes, since we have a copy of jimtcl in cwal's source tree for
the configure-script handling, one of cwal's main motivations was
eliminating my dependencies on 3rd-party language engines because
they invariably change in ways incompatible to my existing code,
requiring me to adapt to them.

WHCL introduces a new approach to parsing compared to earlier
cwal-based languages. Unlike prior efforts, which parse and evaluate a
single token at a time, WHCL tokenizes its whole input before it
starts any work, then post-processes that to perform certain
translations, then hands off that "compiled" chain of tokens to the
interpreter for further handling. This is _far_ more memory-hungry
than its predecessors but eliminates the biggest bugbear in [s2][s2]:
re-tokenization of inputs when re-calling the same code (e.g. any loop
or function body) is computationally slow. What s2 gains in low memory
use, it loses in processing speed. Rewiring s2 for this new model *is*
feasible but would require a large effort. As i truly rely on s2 for
many of my web-side CGI applications, i'd rather not risk breaking it
at such a low level.

# Why TCL-like?

It's fair to ask "why go all old-fashioned when you can go
more modern?" i.e. why choose TCL as a model instead of, say,
JavaScript or python? In short: [s2 already implements something JS-like][s2]
and python's whitespace policy makes my physically ill. Also,
TCL has proven, mainly through
[the autosetup project](https://msteveb.github.io/autosetup/),
to have a nice syntax for one of [the cwal library's][cwal] biggest
initial inspirations: "little languages" intended primarily for
use in testing 3rd-party C and C++ code. WHCL is my (second)
attempt at building a TCL-ish syntax on top of [cwal][]. The first
attempt had some very serious design flaws which were, more or less,
corrected in [s2][]. Though [s2][] works rather well for the
purpose of testing 3rd-party C code, much has been learned throughout
its development, some of which cannot easily be applied to s2 without
breaking it. WHCL is, in short, the next evolutionary step.

Put simply: this is what i do for fun 😀.

[cwal]: /
[s2]: /doc/ckout/s2/manual/
