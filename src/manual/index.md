# Scripting Engine Without a Language
<style>@import url(../../doc/fossil-doc.css)</style>

<!--script src="./highlightjs/highlight-cwal.min.js"></script-->

cwal Central Documentation Hub
=====================================================================

> Sidebar: since the days of old, most non-API cwal docs are [in the
project's wiki pages](/wcontent), but the hope is to eventually
migrate the "more relevant" wiki pages into so-called "embedded doc"
format (like this one). Though the wiki pages are mostly quite old,
they're not entirely outdated. This doc format, however, is more
flexible than the wiki.

cwal, the Scripting Engine Without a Language, pronounced "Sea Wall,"
is exactly what the name says: it's a "language-agnostic" scripting
engine. It's implemented in portable C99, intended to be used as the
basis for creating scripting languages. It was borne from the idea
that (A) tinkering with scripting languages has been a hobby of mine
since the early 2000s, (B) creating one from scratch each time is a
huge bummer, and (C) much of the basis, specifically the memory
management and data type model, can be moved into a common
framework. cwal is the (C) part of that equation. cwal started life as
a fork of a JSON library in mid-2012 and has been [actively developed
and maintained since then](/reports?view=byyear).

Though cwal is _ostensibly_ language-agnostic, its design does imply a
certain "shape" of languages. As cwal originated as a fork of a
library for working with JSON data, its core data types are very
JavaScript-esque, and that general type of language (without the full
weight of JS) is its primary target. It can, however, fit just as well
into batch-like languages which look nothing at all like JS. Client
languages/applications are in no way required to expose all of its
data types to the language.

Docs still to write or port from the wiki (in no particular order):

- [Overview of the data type model](values.md)
- [Scoping and memory management](scoping.md)
- _All The Things_

For a one-person project (as in "written by" one person as well as
"used by" one person), cwal and the related client code included in
its source repository have an astounding amount of non-API documentation:
as of this writing (2022-04-12) approximately 1.18MB of markdown-format
docs spanning 24350 non-blank lines, most of it belonging to...

**cwal Languages:**

  - [s2][] was cwal's "second" (kinda) scripting language.
  - [whcl][] is, as of early 2022, cwal's latest language. Its
    development laid the groundwork for what will, energy permitting,
    eventually evolve into s2v2.

[s2]: /wiki/s2
[whcl]: /doc/ckout/whcl/
