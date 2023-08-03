# whcl: TODOs
##### ([&#x2b11;Table of Contents](./))
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

An overview of the more significant _potential_ TODOs for whcl, in no
particular order...

Mult-var decl and set...
========================================

This will be easy to do, it just needs to be done:

```whcl
decl {
  x 1
  y 2
  ....
}
set {
  x 1
  y 2
  obj.prop 3
  ...
}
```

The [`decl` part is done](/info/408488e99ad6). `set` support is TODO.

Ideally we could use the existing read-and-object routine, copy
the properties, and be done with it, but...

- If we use that routine, we'd have to first read the whole object and
  then copy over its properties. The main issue with that is that we
  cannot catch errors such as duplicate definitions (when reading an
  object, "last one wins" is the rule).
- It would make it impossible to do things like have the value of
  one decl be based on the value of another decl in that same block.

Thus, in order to implement this, we'll have to reimplement a
specialized version of the read-an-object routine which handles
the nuances needed for proper decl/set behavior.


(De)Serialization of Compiled Code
========================================

Add an API for saving/loading "compiled" scripts. This is easy to do,
but it's a good deal of grunt work which just needs to be done.


Call Chaining
========================================

Call chaining is partially implemented:

```console
whclsh> decl x "abc"
result: string@0x5c7410[scope=#1 ref#=1] ==> "abc"
whclsh> affirm $x == [[x to-upper].to-lower]
result: bool@0x4ce378[scope=#0 ref#=0] ==> true
```

Note that dot before the `to-lower` part. That really shouldn't be
necessary but currently is. Improving that is on the todo list. That
said: the dot-based function call dispatch is faster than the
"prettier" syntax, as the latter requires a second (dispatching)
function call.


Backtick strings
========================================

"The plan" was initially to support backtick-quoted strings similarly
to JavaScript. This would be the only string format in whcl which
supports variable expansion inside the string. "The reality," however,
is that we have `[concat ...]` to do this, and it's done and works,
so backtick string support now seems unlikely.

TODO: figure out what sort of variable/symbol expansion we want to
support. Simplest, but also least efficient, would be to pull in s2's
"tmpl" class, which can handle arbitrarily complex strings comprised
of code and client text. That's appealing for the little effort
involved and the flexibility of the end result, but unappealing in
terms of the tremendous overhead required for processing such strings.
In s2 it has _far_ less overhead because it does not require compiling
a separate token chain for every code segment in the template doc.

Simplest is probably to allow only `expr` syntax in the strings,
wrapped up in some marker like `${...}`, noting that that would still
require an explicit `$` on var refs unless we make engine-level changes
to support `$`-less var derefs in some contexts:

```
echo `Hello, ${$name}.`
```

Kinda unsightly.


Windows Platforms
========================================

... will not be supported until someone who works with Windows does
the porting. The core of the engine is OS-independent and "very
likely" works as-is on Windows, but some of the script-bound APIs
require significantly more code for Windows because Windows is
_entirely schizophrenic_ when it comes to character encoding and
requires conversion of most inputs, in particular if they come from a
CLI app.
