# s2: Doc Maintenance Reminders
#### ([&#x2b11;Table of Contents](./))
# Reminders-to-Self About Maintaining these Docs...

**Anchor names** are more verbose than they strictly need to be, e.g.
`#type-function-using` instead of `#using`. This is to facilitate the
fusion of multiple Markdown files together without having colliding
anchors. The "superfluous" prefix "should" match the name of the file
in which the anchor lives, in order to make tracking them down, and
remembering them spontaneously while editing, easier.

**Footnote references** which were imported from the original GDoc
*mostly* retain their original numbers, for the same reason anchor
names are more verbose. Unfortunately, the first dozen or so were
renumbered, so there may be collisions. When adding new footnotes
manually, it would not be reasonable to expect the editor (that'd be me)
to track down the highest-used footnote number, so new footnotes may
get IDs which, at the global scope, collide with others, but they
won't collide in their file-local scope.

# Syntax Highlighting of Triple-Backticks

Markdown's triple-backtick blocks may optionally associate a single
token with those blocks, typically used for styling purposes:

    ```the-token
    contents
    ```

The markdown generator translates such tags to CSS class names in the
form `language-the-token` (because such blocks are conventionally used
for marking up source code), so they must be syntactically legal as
such (e.g. no periods in their names).

These docs use the following custom tags:

- `s2`: s2 script code:
  ```s2
  foreach(foo=>k,v) s2out << k << " = " << v << "\n";
  ```
- `s2-member`: marks function/method/data members of
  classes/modules/whatever. The general convention is that such blocks preceed
  the documentation for each block:
  ```s2-member
  int foo(string bar)
  ```
- `s2-member-deprecated`: marks function/method/data members which have been
  deprecated:
  ```s2-member-deprecated
  int foo(string bar)
  ```

The site uses a checked-in copy of highlightjs to perform syntax
highlighting, as described in [](../../doc/highlightjs/README.md), and
includes a [custom highlightjs module for s2](/finfo/doc/highlightjs/hljs-s2.js).

These docs make use of several backtick tags handled by various
highlightjs modules, including, but not limited to: `css`, `js`,
`console`, `shell`, `sql`, `makefile`, `json`, `c`, `cpp`.

# Fossil Skin Modifications

## Skin CSS

This site's skin is based on the "Ardoise" skin built into Fossil, but uses a number
of CSS overrides which live in [/doc/css/skin-overrides.css](/doc/ckout/doc/css/skin-overrides.css), and that must be
loaded by adding the following to the site's header *after* loading the main site CSS:

```html
<link rel="stylesheet" href="$baseurl/doc/ckout/doc/css/skin-overrides.css" type="text/css" />
```

In addition, the local copy of the Ardoise skin has all of the
"appearance" CSS properties removed, including those with vendor
prefixes. That style property is not portable and causes Firefox to
continually complain on its console.


## Adding Syntax Highlighting

For syntax highlighting support see [](../../doc/highlightjs/README.md).

And then make sure we have the following script bits installed in the
skin's footer:


```html
<script src="$baseurl/doc/ckout/doc/highlightjs/highlight.pack.js"></script>
<script src="$baseurl/doc/ckout/doc/highlightjs/hljs-s2.js"></script>
<script nonce="$<nonce>">
if( "undefined"!==typeof hljs
   /* && /doc\/\w+/.test(window.location.pathname) */ ){
  (function(hljs){
    document.addEventListener(
      "DOMContentLoaded",
      function(event){
        // Jump through some hoops to squelch the console warning about unknown
        // language blocks which we style ourselves (e.g. language-s2-member)
        const langs = [
            "bash", "c", "console", "cpp", "css", "h", "hpp", "html", "javascript",
            "js", "json", "makefile", "markdown", "plaintext",
            "sh", "sql", "s2" // s2 requires hljs-s2.js from this site
        ];
        langs.forEach(function(L){
          document.querySelectorAll(
            "pre > code.language-"+L
          ).forEach((block)=>{hljs.highlightBlock(block)});
        });
      }
    );
  })(hljs);
}</script>
```
