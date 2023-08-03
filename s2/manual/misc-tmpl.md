# s2: s2.tmpl()
#### ([&#x2b11;Table of Contents](./)) ([&#x2b11;Misc. Features Index](misc-index.md))
# `s2.tmpl()`: Scriptable Text Templates

Jump to...

* [`s2.tmpl()`](#misc-tmpl)
* [Customizing Compilation](#misc-tmpl-customize-compilation)
* [Customizing the Output Destination](#misc-tmpl-customize-output-dest)
* [The Obligatory Rant](#misc-tmpl-rant)

<a id="misc-tmpl"></a>
# `s2.tmpl()`

(Pedantic achtung: the output interface for `s2.tmpl()` changed
on 20181118. It is not expected that any clients other than myself are
affected by this change, but if you indeed happened to use `tmpl()`
before that time, please re-read this chapter!)

The [s2sh](s2sh.md) shell binds a function named `s2.tmpl()` which
processes "template-ish" text input and generates s2 script code from
it. The intention is that it can be used to create text documents
which have embedded s2 code, then process them with this function to
generate s2 code which can be eval'd to output that processed page.

Its usage is trivial:

```s2
const code = s2.tmpl( templateSource /*buffer|string*/ );
assert 'buffer' === typeinfo(name code);
```

> Sidebar: the C-level counterparts are `s2_tmpl_to_code()` and
`s2_cb_tmpl_to_code()`.

Template text need not come from a file, but here's a simple way to do
so:

```s2
const code = s2.tmpl( s2.Buffer.readFile(someInputTemplateFile) );
```

That converts the template text into s2 script code and returns its
value as a Buffer. Ideally,
[`Buffer.evalContents()`](type-buffer.md#type-buffer-evalcontents)
should be used to evaluate the buffer's contents. Alternately,
`eval->theBuffer.takeString()` or (less efficiently)
`eval->theBuffer.toString()` may be used. However,
`Buffer.evalContents()` is both more flexible and more predictable in
terms of reporting errors which originate from inside the evaluated
template.

For example:

```s2
const ex = catch s2.tmpl( s2.Buffer.readFile(...) )
    .evalContents('filename', {myVar: 1, blah: 2});
if(ex) { ... something threw an exception ... }
```

> Sidebar: practice has shown that it is often useful to provide
groups of related templates with common utility APIs via their
`evalContents()` calls. e.g. templates which render data from [an
sqlite3 database](../mod/sqlite3/) might benefit from having that
database handle (or an accessor function) handed to them via this
approach. A relatively involved example of such can be found in
[this script](https://fossil.wanderinghorse.net/r/www-wh/finfo/site-tools/s2tmpl.s2),
which is used to generate static HTML pages from s2 `tmpl` input.

tmpl documents are plain text (UTF-8) with two types of embedded tags:
`<?...?>` blocks contain arbitrary s2 code and `<%...%>` provides a
convenience form which simply passes the expression result of its
contents on to the output routine (described later). Both sets of tags
are configurable (also described later).

Here is an example document which demonstrates both tag types, `<?...?>`
(a.k.a. "code blocks") and `<%...%>` (a.k.a. "value blocks"):

```
<? /* Starts a "normal" code block. Such blocks get
      output verbatim into the generated script. */
 var msg = 'this is a demo of tmpl: s2 embedded in text docs';
 var x = 10, y = 12;

 const myfunc = proc(){
    return argv.join(' ==> ')
 };

?>Hi, world! Our message to you is:
<% /* This tag wraps its contents in an eval and output call,
      such that what gets output is the evaluated result of
      this block's body. */
  msg %>

x=<%x%>, y=<%y%>, x+y = <% x+y %>!
The s2.tmpl() function: <%s2.tmpl%>
A list of numbers: <% myfunc(1,2,3) %>
The function which generated that output: <% myfunc %>
```

> Sidebar of arguable significance: because of how the output is
streamed, value blocks (`<% … %>`) get wrapped in an `eval{…}` block
in the generated output. The reason is that the output gets streamed
using `operator<<`, and certain compound expressions otherwise
wouldn't behave intuitively with the generated output operator
calls. e.g. `<%x(), y%>` would not behave as one might expect without
such a wrapper. `tmpl()` does not add such a block around content from
code blocks (`<?…?>`). This injected eval block adds another level of
tokenization overhead to those expressions, but it is thought that
that is better than requiring the user to [remember to] wrap all
compound expressions in value blocks in a block construct. The
overhead `tmpl()` saves by using `operator<<` for output, as opposed to a
normal function call, is orders of magnitude higher than the small
performance hit for these eval blocks.

Note that code blocks almost always need to end with a semicolon to
avoid evaluation errors when the template is eval'd, but `tmpl()` does
not automatically inject them because there are cases where they will
cause a syntax error (specifically, a script-level block construct
which spans across code tag blocks may be syntactically invalid if
semicolons are injected).

Documents may of course be HTML or some such, as long as (A) they do
not use markup which could be confused for template tags (see below
for how to use custom tags) and (B) are encoded in UTF-8. Note that
whitespace outside of tags is retained when the tags are replaced,
with one exception: if a template tag is the first non-space content
of the document then the leading space before that tag is elided.

It is legal for a `<?` tag to be missing a closing `?>` tag, in which
case the rest of the document is considered to be inside that tag
(this is how PHP does it, incidentally, and we do it for the same
reason![^49]). `<%%>` tags, on the other hand, require matching
open/close tags, else an exception is thrown.

`tmpl()` returns a "mangled" form of its input document (as a Buffer
value) which is s2 code (tip: see 
[Buffer.evalContents()](type-buffer.md#type-buffer-evalcontents)). The
above document, when evaluated using `theCompiledTemplate.evalContents()`,
outputs the following:

```
Hi, world! Our message to you is:
this is a demo of tmpl: s2 embedded in text docs

x=10, y=12, x+y = 22!
The s2.tmpl() function: function@16EF470
A list of numbers: 1 ==> 2 ==> 3
The function which generated that output: function@16F75A0
```

Templates can be embedded in s2 code - simply wrap them in a heredoc:

```s2
const myTemplate = <<<EOF
<? const z = 42 ?><% typeinfo(name z) %> <% nameof z %> = <% z %>
EOF;
```

When nesting templates within code within templates it may be
necessary to generate the tags from code to avoid confusing an outer
template parser (e.g. `'<'+'script...'`, as is often seen in JavaScript
when generating a SCRIPT tag) or to use custom tags (described later)
for the inner templates.

Note that no code in the template is actually evaluated until the
`tmpl()`-processed result is eval'd. `tmpl()` simply compiles the template
to an s2 script.

The generated code is structured such that all blocks are evaluated
within the same scope unless the template author breaks it up
otherwise. This means that multiple `<??>` blocks can be used to
initialize variables within the same s2 scope, and flow-control
constructs may in fact span across tag blocks, though readability
suffers somewhat. If one feels compelled to have, e.g. if/else blocks
span across tags, using a heredoc as the body part can help improve
readability:

```s2
// Poorly readable in large contexts:
<? if(enableFoo) { ?> ...text… <? } ?>
// This may help a little:
<? if(enableFoo)<<<IfEnableFoo ?> ...text… <? IfEnableFoo ?>
```

In such cases, be sure to have the "end-if" in a `<??>` block, not a
`<%%>` block, or the output will be malformed (and throw an exception
when evaluated).

<a id="misc-tmpl-customize-compilation"></a>
# Customizing `tmpl()` Compilation

`tmpl()` accepts an Object as an optional second parameter, which can
be used to customize the open/close tags for a given call. The
properties supported are:
  
```s2
{
    valueOpen: string, // opening tag for VALUE blocks (<% %>)
    valueClose: string, // closing tag for VALUE blocks
    codeOpen: string, // opening tag for CODE blocks (<? ?>)
    codeClose: string // closing tag for CODE blocks
    outputSymbol: string // output symbol to use in place of TMPLOUT
}
```

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

`s2.tmpl()` will throw an exception if any of these rules are violated
(or, perhaps, perceives itself as having been violated). Note that it
is legal to specify only one of the tag pairs, e.g. the value-block
tags and not the code-block tags, or vice versa, in which case the
default will be used for the other. This is particularly useful in
mini-templates, where it is convenient to specify the value-block tags
when embedding template snippets in an outer (page-level) template.

<a id="misc-tmpl-customize-output-dest"></a>
# Customizing the `tmpl()` Output Destination

Where does evaluated template output go? When evaluated, the generated
code checks if a symbol named `TMPLOUT` is defined (that name can be
customized via the second argument to `tmpl()`, as described
above). If `TMPLOUT` is not defined (nor overridden via the second
argument) then it uses the `s2out` keyword as its output
function. Clients may instead define `TMPLOUT` symbol (before eval'ing
the processed template) in order to capture output generated by the
template-processed code. The requirements for the object (which we
assume will output the results somewhere, though it may instead
capture/redirect them):

- It must either be a function or implement `operator<<`. It must accept
  a single argument and "output" it (see below). It must not introduce
  any extra formatting between values (not even a space or newline).
- It may, instead of outputting the contents, simply append them to a
  buffer, an array, or some such.
- If it has `operator<<`, it "should", as is conventional for
  `operator<<`, return `this`, but `tmpl()` does not currently rely on
  that behaviour. (If it does not implement `operator<<`, `tmpl()`
  internally creates a proxy object which wraps the function call in
  that operator.)
- If it throws, the exception is propagated.

As mentioned above, `s2out` is used by default if `TMPLOUT` is not
defined beforehand, meaning that the template's output will go to the
standard s2-defined output channel. This can be used in conjunction
with the [output buffering API](misc-ob.md) to capture processed
output instead of outputting it:

```s2
s2.ob.push();
const err = catch s2.tmpl(templateSource).evalContents();
const b = s2.ob.pop(err ? 0 : 1);
err && throw err;
// else b is-a Buffer containing the processed output
```

Note that we take some care to ensure that the OB state is ok if the
template throws an exception, otherwise further output may be hidden
by output buffering. Alternately, an implementation like the following
can capture the output in a buffer:

```s2
const TMPLOUT = new s2.Buffer();
s2.tmpl( templateSource ).evalContents();
// If ^^^^ that generates any output then the following holds:
assert TMPLOUT.length() > 0;
```
or, using a more modern approach:

```s2
const buf = new s2.Buffer();
s2.tmpl( templateSource ).evalContents({ TMPLOUT: buf });
// If ^^^^ that generates any output then the following holds:
assert buf.length() > 0;
```

or:

```s2
const obuf = new s2.Buffer();
s2.tmpl( inputBuffer, { outputSymbol: nameof obuf } ).evalContents();
// If ^^^^ that generates any output then the following holds:
assert buf.length() > 0;
```

Also note that all code evaluation in the template is deferred until
it is eval'd. This means that the template may access any symbols
visible from the scope which eval's it (as opposed to the scope which
generates it).


<a id="misc-tmpl-rant"></a>
# The Obligatory Rant

It is, of course, not considered "best practice" to "mix presentation
and logic" in software (noting that that's exactly what `s2.tmpl()`
does). *Bah, humbug!* When writing s2 scripts, it's sometimes exactly
what's needed. When writing business-grade software, keep them
separated, but when writing a quick script to solve a small problem,
there is little reason not to mix the two.

While s2's author would not condone the use `s2.tmpl()` in
business-grade software (or any software a customer is paying for), he
does, in fact, use it to generate all of the static pages of his
website. s2 templates combine site menu, related configurable state,
and each page's static content part into a coherent whole, allowing
the site to host HTML files which do not require further server-side
processing on each request.


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

