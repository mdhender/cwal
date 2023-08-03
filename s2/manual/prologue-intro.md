# s2: Prologue
#### ([&#x2b11;Table of Contents](./))
# Prologue

Jump to...

* [Management Summary](#management-summary)
* [Hello, World!](#hello-world)
* [Primary Properties](#primary-properties)
* [Why s2?](#why-s2)
* [Downloading & Building](#download-and-build)
* [s2 in Other Projects](#in-other-projects)

<a id="management-summary"></a>
# Management Summary

s2 is a small-yet-flexible embeddable scripting language engine,
implemented in C, with a strong syntactical resemblance to, and base
feature set similar to, JavaScript. Its two main distributables are a
standalone C library (distributed as two source files) and a
client-extensible shell-like application which provides a shell for
running one's own scripts and acts as a bootstrap for development of
other client applications. These distributables make it simple to plug
in to arbitrary source trees, including one's own copy of the s2sh
shell, to which client-side script bindings can be added, without
having to edit any of s2's sources, by using scripts, DLLs, or linking
them statically into the shell. Low dynamic memory usage is at the
absolute top of s2's design goals, and s2's memory requirements easily
compare favourably with the lightest scripting engines out there, up
to and including (in unscientific tests)
[Lua](http://www.lua.org/). While s2 was initially created to script
unit tests for libraries and applications, it's flexible enough to be
used in a far wider variety of contexts, provided its limitations are
observed. e.g. it is *strictly single-threaded*, is not event-driven,
and it cannot be paused in mid-operation and resumed later. It's a
toy, to be clear, but it's a *power-toy*.


<a id="hello-world"></a>
# Hello, World!

First, the conventional opening statement using various formulations
(they all output the same thing):

```s2
print('Hello, world!');
print(<<<X Hello, world! X);
print(catch{throw "Hello, world!"}.message);
print(catch{throw {hi:"Hello, world!"}}.message.hi);
print(catch{throw ["Hello, world!"]}.message[0]);
s2out << scope{
  const ☺ = ["Hello"];
  ☺[] = "world!";
  ☺.join(", ");
} << '\n';
s2out(["Hello","world!"].join(', '),"\n");
print(["Hello","world!"].withThis(function(){return this.join(', ')}));
s2out << for(;;){break "Hello, world!"} << '\n';
```

s2 is a small embedded scripting engine, in the form of a C library,
initially primarily intended for writing scriptable test harnesses for
C libraries. s2 is an expression-oriented language which uses the
language-agnostic [cwal scripting engine](/) as the basis for its
value types, memory management, and garbage collection. It has a
distinctly JavaScript-like flavor, with almost-C++-like scope
lifetimes (but [***completely different*** symbol
resolution](symbol-resolution.md)), and supports any value types
supported by (or bindable to) cwal (out of the box it has a JS-like
set of types, plus a few custom ones). It supports the binding of
client-defined types to script code, with an optional finalizer to be
called by the garbage collector, and it guarantees finalizers get
called so long as the library is used properly. cwal was designed from
the ground up to make it easy to bind a wide variety of C code to it,
and experience has shown it to be trivial to integrate many sorts
client-side data and functions into the scripting world.

s2 is *not* an enterprise-level, self-optimizing, AST-loving,
bytecode-generating Übermachine. It is a simple-to-use, lightweight,
and relatively performant, expressive, and flexible environment for
scripting one's own C/C++data types and routines. It is a "glue
language," not a full-featured, do-everything scripting
environment. It is designed not so much to stand alone, but to be
extended by clients by binding their own functionality to it (which is
easy to do). Nonetheless, it can do quite a lot on its own, and the
default shell application is easy to extend with custom bindings,
either compiled directly in or loaded from DLLs, for those who want
more out of it. An [optional C++ templates
layer](/finfo/s2/cwal_convert.hpp) can create s2-compatible variants
of your C/C++ functions and class methods/constructors, as well as
bind getter/setter functions to non-function members, *at compile-time*
using only template declarations ([here's an example](/dir/s2/mod/sample_cpp)).

While s2 conceptually derives from its direct predecessor, th1ish,
which had a TCL-like grammar, s2 does not inherit any TH1/TCL-isms
except for the `proc` keyword and how it uses the `catch`
keyword. (Once one gets used to typing `proc`, typing out `function`
just feels barbaric by comparison. Nonetheless, in s2 `proc` is simply
an alias for `function`, and both work identically.) s2 is *almost*,
but not quite, a JavaScript clone in terms of overall syntax and basic
behaviours, yet some of its properties only *superficially* resemble JS
while being *exceedingly different* semantically.


<a id="formalities"></a>
# Formalities

**Author**: [stephan beal](https://wanderinghorse.net/home/stephan/),
with his many thanks to Peter Angerani, Caleb Gray, Richard Hipp,
Simone Mueller, and others over the years who've helped (directly or
otherwise) make this project possible and/or keep it going.

**License**: the software is dual Public Domain/MIT, with one
MIT-like-licensed part (the JSON parser). This documentation is
released into the Public Domain.

**Code Status**: s2 is functionally feature-complete and robust, with
no known catastrophic bugs and only a couple known minor ones. s2's
build process includes fairly rigorous unit testing and
valgrind-assisted analysis. When used properly, it will never leak a
single byte nor misuse memory. Any new s2 features are likely to just
be "sugar," refinements, optimizations, or good old-fashioned
experimentation (we've still got lots of room for that). This
documentation reflects what works, as opposed to being a list of "it
will eventually be able to's," and is maintained hand-in-hand with
[the trunk of the code base](/timeline), so it reflects whatever is
currently the latest trunk code in the source tree.

**Platforms**: s2 is developed exclusively on Linux (primarily on x86/64
and ARM hardware) using gcc and clang, but places a *strong emphasis* on
standards-conforming code and portability. It uses only C89-defined APIs
except for (A) optional features which require platform-specific support
(e.g. loading DLLs) and (B) the fixed-size integers and standard
printf-style format strings specified by C99 (the *inttypes.h* and
*stdint.h* headers). Thus it "should" build as-is (modulo certain
optional, platform-specific features) on any C89 platform, so long as it
has the two required C99 headers (there is a [*free drop-in
replacement*](https://code.google.com/p/msinttypes/) available for older
MSVC platforms). It does not currently accommodate Windows' weird "exports"
declaration rules, simply because there's no Windows here to try such a
thing on. (It should be compilable directly into an application, as
opposed to via a library, despite that, and the "amalgamation" source
distribution format facilitates (indeed, encourages) such embedded use.)
Patches are of course welcomed.


<a id="primary-properties"></a>
# Primary Properties

Some of the primary properties of s2 as a scripting environment:

-   **Very low dynamic memory usage** via a combination "extreme memory
    management," memory recycling (in various forms), cwal's
    highly-memory-optimized [Value (data type) system](/wiki/DataTypes),
    its [lifetime model](/wiki/MemoryModel), [its garbage collector](/wiki/cwal_gc).
    s2 does no pre-allocation or memory pools. It allocates only what it
    needs, then continues to aggressively recycle as much of it as
    possible. It needs less than 10kb of dynamic memory to get "just the
    language" running (without any shared functions/prototypes), and
    needs between 35-70k for the "full package," depending on the
    platform and various core-level memory-related configuration
    options. In bare-bones mode it's been observed running small scripts
    scripts using *as little as 1.5k (32-bit) to 2.6k (64-bit)* of RAM,
    *including* the memory for the input script: see [FIXME: LINK](#fixme)
    for full details.
-   **GC is synchronous**, running as part of normal script execution.
    There is not a separate "GC thread," and cwal is not subject to
    unpredictable GC-related pauses (it can *potentially* have
    GC-related pauses, but they're predictable). At the scale of scripts
    s2 works with, GC-related delays have never been noticeable in
    practice.
-   ***Strictly*** *single-threaded*! **(Can't stress that enough!)** Each
    interpreter instance may only be used by *one single* thread and
    values *must not* (regardless of threading!) be made visible between
    different interpreter instances (doing so *will* effectively corrupt
    lifetime tracking in each interpreter involved!). Each interpreter
    instance is standalone - the only global memory shared between them
    is constant and static. When providing a custom memory allocator,
    clients may need to ensure that their *allocator* is thread-safe, so
    that multiple interpreter instances may run concurrently.
-   **Provides a relatively flexible scripting language**, closely
    related to JavaScript in syntax and semantics, but more free-form in
    terms of where it accepts constructs, often allowing for more
    compact code.
-   cwal as a whole has proven to be **easy to bind 3rd-party C code
    to**, making it straightforward to plug client-defined C/C++ types
    into the scripting language. (Indeed, this is s2's primary purpose -
    to script 3rd-party libraries.) The gory details of lifetime
    management, reference counts, etc. only intrude on the most advanced
    of add-on code, and the vast majority of client-side bindings get by
    with only a small handful of simple, oft-used rules and patterns.
    Bindings written for one cwal-based application can normally be
    copy/pasted into (or even properly reused by ;) other cwal-using
    applications with no real porting effort. e.g. the vast majority of
    s2's prototype-level infrastructure (and the docs for it ;) was
    initially copy/pasted from th1ish (s2's predecessor) because it
    almost exclusively depends on the script *engine* (cwal), not the
    script *language* (th1ish resp. s2).


<a id="why-s2"></a>
# Justification: Why s2?

Why expend so much effort developing yet another scripting language,
as if the world had any shortage of them? This is *what i do for
fun*. That is the *sole reason*. s2 is not intended to become a
popular, general-purpose language. It's there for me to *play with*
because i've never quite grown up - *only the nature of the toys has
changed* over the years.

The impetus for creating this project's underlying scripting engine
(libcwal) was, in short, that i got sick of 3rd-party scripting engines
going through major API changes (sometimes completely undocumented)
which broke my C/C++ code. In particular, it was a
subtle-yet-far-reaching change in the Google V8 JavaScript engine
[which finally pushed me over the
edge](https://groups.google.com/forum/#!topic/v8-users/MUq5WrC2kcE)
and inspired me to sit down and finally code the scripting engine
model i'd been kicking around for several years.



<a id="download-and-build"></a>
# Downloading & Building

Instructions for obtaining the source code are in [](/wiki/download).

Though its canonical build files assume a Unix-like platform with GNU
Make, the sources are (except for the optional dynamic module loader)
platform-neutral and can be plugged in to arbitrary build environments.
In particular, the so-called "amalgamation build", a pair of C/H files,
is intended to be dropped directly into arbitrary projects.

Once downloaded and unpacked, it can be built like this:

```bash
# First, set up certain generated components:
./configure [options... use --help to list all options]

# build the core lib and s2:
make

# Optional: build s2 modules:
make s2-mod

# run unit tests:
cd src
./test
cd ../s2
make units

# optionally run basic valgrind tests: (requires valgrind)
# From the s2 directory...
make vg

# Run most extensive valgrind tests (and go get a coffee):
make vgall

# Optional: create amalgamation builds:
# From the top directory...
make amal

# Optional: create a standalone mini-distribution:
make mini-dist dist-dest=$HOME/tmp
```

That will build the library, [the s2 shell app](s2sh.md),
and possibly other binaries or loadable modules.

How to extend the s2 shell without editing its sources is described
[in the s2sh docs](s2sh.md).

## The Amalgamation Build

s2's primary distributable is a pair of C files: one header and one
implementation file. These files, known as the "amalgamation build,"
are created by running `make amal` from the s2 subdirectory of the
source tree. That filters and combines all the necessary source and
header files into two files: `libs2.{c,h}` (formerly
`s2_amalgamation.{c,h}`) contain all of the library-level code
(including cwal), and can be dropped in to arbitrary source trees and
used as-is. They do not include the *app-level* code
([s2sh](s2sh.md)). Those two files are all one really needs to use s2,
but [s2sh](s2sh.md) provides a good starting point for how to use the
C API and provides an easy way to extend it without having to patch
the shell's source code. This makes it useful as a general-purpose
shell which can be supplemented with client-side features linked right
in or loaded via DLLs.




<a id="in-other-projects"></a>
# s2 in Other Projects

s2 is used quite extensively in my own projects, primarily in the
following roles:

- Generating static content files for [my website](https://wanderinghorse.net)
  from s2-powered template files (using the `s2.tmpl()` API).
- Implementing backend CGI-based JSON services, including the search engine
  on my website and numerous site-specific APIs on various *wanderinghorse.net*
  subdomains.

s2's original intent was to script other libraries, primarily for unit
testing purposes, but the fact is that, since s2 has been developed, i
no longer actively maintain any non-trivial libraries which would
benefit from such testing. s2 was used extensively in one ambitious
project which unfortunately had to be abandoned when chronic RSI
struck me down in late 2014, reducing my coding capacity to a mere
fraction of its former self :/.

