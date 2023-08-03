# whcl: Tips and Tricks
##### ([&#x2b11;Table of Contents](./))
# WHCL Ticks and Tricks
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

... for getting the most out of whcl.

- [Editing WHCL Code](#editing) 
* [Saving Memory](#memory)

<a id="editing"></a>
Editing WHCL Code
============================================================

WHCL's syntax is close enough to TCL's that any TCL-based editor
support, e.g. syntax highlighting, "should" do a reasonably good job
with WHCL. One area which won't work is particularly clever editors
which attempt to perform semantic analysis on the code. If, per
chance, you have an editor with such features, disable them for WHCL
code.

WHCL's own development is primarily done in emacs in `tcl-mode`.


<a id="memory"></a>
Saving Memory
============================================================

Short Strings and Identifiers
------------------------------------------------------------

One of the underlying engine's core-most optimizations is that _all_
single-byte strings consisting of only one character in the ASCII
range 0 to 127 (inclusive) are built-in constants, as is the empty
string. That is, they don't require allocating memory nor do they
participate in lifetime management like most values. WHCL doesn't
always have to allocate a string to handle an identifier, but it
always does when they are used as unquoted strings. Using
single-letter identifiers might not always improve readability, but
they're guaranteed to use less memory than longer ones.

The distinction between these built-in values is visible via
[whclsh](whclsh.md):

```shell
$ whclsh -v
whclsh> eval ""
result: string@0x66493c[scope=#0 ref#=0] ==> ""
# ----------------------^^^^^^^^ anything in "scope 0" exists outside
# of the lifetime management system (i.e. is a builtin value).
whclsh> eval "a"
result: string@0x665c5f[scope=#0 ref#=0] ==> "a"
# ----------------------^^^^^^^^ built-in
whclsh> eval "aa"
result: string@0x16b3a30[scope=#1 ref#=0] ==> "aa"
# -----------------------^^^^^^^^ this is an allocated value
```

The "ref" values are the current refcount, which is always zero for
builtin values. In the final line, with an allocated value, that value
has just reached refcount zero and will be cleaned up as soon as the
result-outputing bit has finished reporting its result.

Builtin Numbers
------------------------------------------------------------

Another core-most optimization is that the integer and floating-point
values -1, 0, and 1 are built-in constants. (Age-old testing showed
that those are the three most common numbers seen in scripts.) The
range of built-in numbers is actually normally higher than that, but
those three are the bare minimum. (That same age-old testing showed
that increasing the range of built-in numbers beyond those three led
to negligible reductions in memory allocation.)

For the curious, there is a way to determine the full range of
built-in numbers, noting that this is a built-time option in the
underlying [cwal](/) library and subject to change...

```shell
$ whclsh -v
whclsh> eval -10
result: integer@0x6642dc[scope=#0 ref#=0] ==> -10
# -----------------------^^^^^^^^ anything in "scope 0" exists outside
# of the lifetime management system (i.e. is a builtin value).
whclsh> eval -11
result: integer@0x16b2dc0[scope=#1 ref#=0] ==> -11
# ------------------------^^^^^^^^ this is an allocated value
whclsh> eval 20
result: integer@0x66487c[scope=#0 ref#=0] ==> 20
# -----------------------^^^^^^^^ built-in
whclsh> eval 21
result: integer@0x16b2d00[scope=#1 ref#=0] ==> 21
# ------------------------^^^^^^^^ not built-in
```
