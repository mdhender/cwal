# whcl: tmpl
##### ([&#x2b11;Table of Contents](./)) ([&#x2b11;API Index](api-index.md))
# tmpl: Scriptable Text Templates
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>


Jump to...

* [whcl.tmpl](#misc-tmpl)
  * [Evaling tmpl Scripts](#tmpl-eval)
* [Customizing Compilation](#misc-tmpl-customize-compilation)
* [Customizing the Output Destination](#misc-tmpl-customize-output-dest)
* [The Obligatory Rant](#misc-tmpl-rant)

<a id="misc-tmpl"></a>
whcl.tmpl
========================================

The `whcl.tmpl` function processes "template-ish" text input and
generates whcl script code from it. The intention is that it can be used
to create text documents which have embedded whcl code, then process
them with this function to generate whcl code which can be eval'd to
output that processed page.

This API may be installed using:

`whcl install-api tmpl`

or calling `whcl_install_tmpl()` from C code.

It adds a `whcl.tmpl` object with the API described below.

Its usage is trivial:

```whcl
decl code [whcl.tmpl $templateSource]; # buffer | string
assert 'Script' == [info type-name $code]
```

> Sidebar: the C-level counterparts are `whcl_tmpl_to_code()` and
`whcl_cb_tmpl_to_code()`.

Template text need not come from a file, but here's a simple way to do
so:

```whcl
decl code [whcl.tmpl [whcl.Buffer.read-file $someInputTemplateFile]]
```

That converts the template text into script code and returns its value
as a [Script object](type-script.md). If `tmpl` is passed the
`-buffer` flag as its first argument, it instead returns a [buffer
object](type-buffer.md) which contains the `tmpl`-processed script
code. The remainder of these docs assume that flag is not in use
unless explicitly noted otherwise.

The generated script does not produce any output until it's run:
`tmpl` just converts the template to a script.

`tmpl` documents are plain text (UTF-8) with two types of embedded
tags: `<?...?>` blocks contain arbitrary script code and `<%...%>`
provides a convenience form which simply passes the expression result
of its contents on to the output routine (described later). Both sets
of tags are configurable (also described later).

Here is an example document which demonstrates both tag types, `<?...?>`
(a.k.a. "code blocks") and `<%...%>` (a.k.a. "expression blocks"):

```
<? /* Starts a "normal" code block. Such blocks get
      output verbatim into the generated script. */
  decl msg 'this is a demo of tmpl: script embedded in text docs';
  decl x 10
  decl y 12

  proc myfunc {} {return [argv.join ' ==> ']}

?>Hi, world! Our message to you is:
<% /* This tag wraps its contents in an expr and output call,
      such that what gets output is the evaluated result of
      this block's body. */
  $msg %>

x=<%$x%>, y=<%$y%>, x+y = <% $x + $y %>!
The tmpl function: <%whcl.tmpl%>
A list of numbers: <% [myfunc 1 2 3] %>
The function which generated that output: <% $myfunc %>
```

When evaluated, the output to the above input looks like:

```
Hi, world! Our message to you is:
this is a demo of tmpl: script embedded in text docs

x=10, y=12, x+y = 22!
The tmpl function: function@0x20129e0
A list of numbers: 1 ==> 2 ==> 3
The function which generated that output: function@0x2015450
```

Note that code blocks almost always need to end with an explicit
newline or semicolon to avoid evaluation errors when the template is
eval'd, but `tmpl` does not automatically inject them because doing
so feels wrong somehow.

Documents may of course be HTML or some such, as long as (A) they do
not use markup which could be confused for template tags (see below
for how to use custom tags) and (B) are encoded in UTF-8. Note that
whitespace outside of tags is retained when the tags are replaced,
with one exception: if a template tag is the first non-space content
of the document then the leading space before that tag is elided.

It is legal for a `<?` tag to be missing a closing `?>` tag, in which
case the rest of the document is considered to be inside that tag
(this is how PHP does it, incidentally, and we do it for the same
reason[^49]). `<% %>` tags, on the other hand, require matching
open/close tags, else an exception is thrown.


<a id='tmpl-eval'></a>
Evaling tmpl Scripts
----------------------------------------

There are two options for evaluating scripts:

```whcl
eval -> $theScript
theScript run
```

They're _almost_ equivalent: `eval` runs the code in the current
scope, whereas the second approach requires a method call, which
inherently brings with it a new scope (and `eval -scope` can be used
to force a new scope). Even so, that particular method is configured
[to not block symbol lookup](builtin-proc.md#-xsym), so scripts can
resolve symbols via the calling scope.

Templates can be embedded in script code - simply wrap them in a heredoc:

```whcl
decl myTemplate {{{
<? decl z 42; ?><% [info type-name $z] %> z = <% $z %>
}}}
```

Note that no code in the template is actually evaluated until the
`tmpl`-processed result is eval'd. `tmpl` simply compiles the template
to a whcl script.

<a id="misc-tmpl-customize-compilation"></a>
# Customizing tmpl Compilation

`tmpl` accepts an Object as an optional second parameter, which can be
used to customize the open/close tags and the output command. The
properties supported are:

- `value-open` = opening tag for EXPRESSION blocks (`<% %>`)
- `value-close` = closing tag for EXPRESSION blocks
- `code-open` = opening tag for CODE blocks (`<? ?>`)
- `code-close` = closing tag for CODE blocks
- `output-command` name and optional flags of output command. \  
  All output from the script is sent to this command, one segment at a
  time. The default command is `whcl.tmpl.out`, which is a function
  which works just like `echo -n -s`. Client code may legally swap out
  the `tmpl.out` function with one of its own.

The rules for custom tags are:

- The tags may essentially be arbitrary strings, but it is up to the
  user to ensure that the inputs do not use those strings in parts not
  intended to be processed as script code. All tag matching is
  case-sensitive. Examples of legal tags (as they would appear in the
  above Object): `"<$>"`, `"</$>"`, `"$$"`, `"/$$"`, `"!?@"`, `"<?blah"`,
  `"</blah>"`, etc., etc., etc. Note that the use of "/" in a closing
  tag is certainly not required - it is just a common convention.
- If one of the Open tags is set but its corresponding Close tag is
  not, then the opening tag is ignored (the default is used instead)!
- No tag may be the empty string, nor may any tag have the same byte
  content as any other tag (all tags in the set must be distinct
  strings)[^1]. e.g. it's not legal for both an opener and closer to
  be `*`[^2].

`tmpl` will throw an exception if any of these rules are violated (or,
perhaps, perceives itself as having been violated). Note that it is
legal to specify only one of the tag pairs, e.g. the value-block tags
and not the code-block tags, or vice versa, in which case the default
will be used for the other. This is particularly useful in
mini-templates, where it is convenient to specify the value-block tags
when embedding template snippets in an outer (page-level) template.

<a id="misc-tmpl-customize-output-dest"></a>
# Customizing the `tmpl` Output Destination

Where does evaluated template output go? When compiled, `tmpl` checks
its optional 2nd argument for an object containing customization
properties, as described above. The `output-command` option is the
string `tmpl` will prepend to all output, effectively sending the
output to that command. The string must be syntactically legal for use
at the start of a whcl command line.

The requirements for the command (which we assume will output the
results somewhere, though it may instead capture/redirect them):

- It must accept a single argument and "output" it (see below). It
  must not introduce any extra formatting between values (not even a
  space or newline).
- It may, instead of outputting the contents, simply append them to a
  buffer, an array, or some such.
- If it throws, the exception is propagated.

As mentioned above, a default command is used if `output-command` is
not defined beforehand, meaning that the template's output will go to
the standard whcl-defined output channel. The default can be used,
e.g., in conjunction with the [output buffering API](api-ob.md) to
capture processed output instead of outputting it.

Here's an example which reads in a template file, compiles it,
runs it, capturing its `tmpl` output in a buffer:

```whcl
whcl.install-api tmpl
decl tmplFile 'xyz.tmpl'
decl s [whcl.tmpl
        [whcl.Buffer.read-file $tmplFile]
        [object output-command out]
       ]
#echo [s.source-code]
proc out {} {
    out.buf append argv[0]
}
set out.buf new whcl.Buffer
s run
echo __FLC "Captured" [out.buf.length] "bytes:"
echo [out.buf.take-string]
```


<a id="misc-tmpl-rant"></a>
# The Obligatory Rant

It is, of course, not considered "best practice" to "mix presentation
and logic" in software (noting that that's exactly what `tmpl`
does). *Bah, humbug!* When writing scripts, it's sometimes exactly
what's needed. When writing business-grade software, keep them
separated, but when writing a quick script to solve a small problem,
there is little reason not to mix the two.


# Footnotes

[^1]: Potential TODO (20191215): allow one, but not both, set of tags
    to be empty/falsy, in order to disable the use of that tag type
    within a given template.

[^2]: Potential TODO (20191217): Allow opening/closing tags to be the
    same byte sequence, like how many lightweight markups using
    matching pairs of `*` for italics or bold. That "should" work
    as-is right now (for well-formed inputs) if that part of the
    open/closing tags validation is simply removed, but that's
    untested.

[^49]: Short version: to avoid difficult questions about how to handle
    trailing whitespace, seeing as they are significant and
    potentially (but not necessarily) desired. Remember that text
    files normally have a trailing newline (which is whitespace and
    can, e.g. mangle HTTP header output).
