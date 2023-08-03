# s2: The Definitive (The Only!) Guide
#### ([&#x2b11;Central s2 Documentation Hub](../))
# Table of Contents for the s2 manual...

In December 2019, the massive s2 manuals were ported from very nearly
200 pages of GoogleDocs, with more than 5 years of accumulated text,
to markdown format for embedding directly in [the source
repository](/). Welcome to that markdown...

This set of documents cover essentially every aspect of working with
s2, from its grammar to plugging it in to C/C++ code, to tips for
using and abusing its strengths and weaknesses.

These docs get updated as the code is written, so they always reflect
the [current trunk state](/timeline?r=trunk), plus or minus half a day.

### Prologue

- [Brief Introduction to/Overview of s2](prologue-intro.md), including download/installation instructions.
- [Symbol Resolution *(READ THIS!!!)*](symbol-resolution.md).
  \[Potential\] Users *really* need to understand this unusual aspect
  of s2. It's in its own file, separated out from the "yeah, yeah, we *know* this stuff"
  parts of the docs ***for a reason***.

### Grammar

[](grammar.md) covers...

- Grammar/Scripting-related Terminology
- Anatomy of an Expression
- Identifiers and Strings
- Built-in Constants
- Scopes/Scoping in s2
- Error Types: Script-fatal vs. Catchable

Related topics which live in their own pages:

- [Operators](operators.md)
- [Keywords](keywords.md), including flow control constructs: loops
  and if/else.

### Data Types

- [Intro & Basics](type-intro.md): Overview of the types/type system
- [Strings](type-string.md): UTF-8
- [Numbers](type-number.md): Integers and Doubles
- [Objects](type-object.md): General-purpose containers
- [Arrays](type-array.md): Heterogenous lists
- [Tuples](type-tuple.md): Feature-light/weight-light arrays
- [Functions](type-function.md): Ya call() 'em!
- [Exceptions](type-exception.md): Error-reporting objects
- [Hashtables](type-hash.md): High-speed key/value storage
- [Buffers](type-buffer.md): Work with binary data or dynamic strings
- [Enums](type-enum.md): Fixed/immutable property sets
- [PathFinder](type-pathfinder.md): A search-for-files utility

### Misc. Features & APIs

- [Disabling Filesystem Access](misc-disable.md)
- [Loadable Modules](../mod/)
- [JSON API](misc-json.md)
- [I/O-related API](misc-io.md)
- [Filesystem-related API](misc-fs.md)
- [Time-related API](misc-time.md)
- [Output Buffering (OB) API](misc-ob.md)
- [Scriptable Text Templates: `s2.tmpl()`](misc-tmpl.md)
- [Capping s2 Memory Usage](misc-memcap.md)

### The s2 shell - s2sh

- [s2sh](s2sh.md) is "the" app for running s2 code. It also acts as:
    - A demo client for how to embed s2.
    - A customizable basis for creating client-extended s2 shell apps.
- [s2sh2](s2sh2.md) is a slightly modernized rework of `s2sh`, more
  refined in some regards.
- [`S2_HOME`](s2-home.md) is a concept closely related to s2sh.
* Building a static [s2sh](s2sh.md) [with Docker](../../docker/README.md).


### Using s2 in C/C++

- [Embedding and Extending s2 from C/C++](s2-from-c.md)
- [Extending s2sh](s2sh.md#s2sh-extend-with-native)

### If it Doesn't Fit Elsewhere, it's Probably Here...

- [Known Misbehaviours](misbehaviours.md)
- [Incompatible Changes](incompatible-changes.md)
- [Script Snippets](snippets.md) demonstrating various bits and pieces.
- [The Workshop](workshop.md) describes experimental & in-development features.
- [Tips and Tricks](tips-and-tricks.md) for making good use of s2.
- [TODOs: Potential and "Definite"](todos.md)
- [More than anyone needs to know about s2 internals](internals.md) (other than myself, of course).
- [Retrospective 2019](retrospective-2019.md) - looking back at 5.5-ish years of s2.
- [Doc Maintenance Notes](doc-maintenance.md) - reminders to self.
- [Essay: Parsing Parentheses](essay-parens.md) - because s2's approach is "interestingly unconventional."
- [s2 v2](s2v2.md) - if version 2 ever happens, this is what will change.
