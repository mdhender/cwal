# Highlightjs in Fossil
# Integrating Highlightjs with a Fossil SCM Skin

Highlightjs is a relatively small, expansive syntax-highlighting
library for JavaScript:

<https://highlightjs.org/>

Before we begin:

1) These instructions require modifying a repository's "skin". The structure
of a fossil skin, and exactly how to edit them, are described
[on the fossil web site](https://fossil-scm.org/fossil/doc/trunk/www/customskin.md).
This page describes only the specifics of the edits necessary to integrate
highlightjs.

2) These instructions assume that the user will check a copy of
highlightjs's JavaScript and CSS code into the repository. They are
not large: they may be as small as ~35kb, depending on whether the
user uses a custom highlightjs build which only includes the
language(s) they need.

## Fossil Skin Header

Normally fossil creates and injects its own HTML header as part of its
skinning process. In order to integrate syntax highlighting we need to
customize that header. To integrate highlightjs we first need to
import its CSS via the page header. e.g. this site's fossil skin
header looks something like:

```html
<link rel="stylesheet" href="$baseurl/doc/ckout/doc/highlightjs/hybrid.css" type="text/css" />
<!-- ----------------------- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     see below for multiple options for how to link to that.
-->
...then the rest... (typically the site's main menu)
```

Pick *ONE* CSS file from highlightjs's expansive collection (each CSS
file is a different highlighting theme):

```css
<!-- Default CSS: -->
<!-- link rel="stylesheet" href="$baseurl/doc/ckout/doc/highlightjs/default.min.css" type="text/css" /-->
<!-- _OR_ theme CSS: -->
<link rel="stylesheet" href="$baseurl/doc/ckout/doc/highlightjs/hybrid.css" type="text/css"/>
```

Delivery of the JS and CSS from a fossil repository can be achieved
via multiple fossil-provided URIs...

The `/doc/ckout` path, demonstrated above, uses local files if fossil
is serving directly from a checkout via `fossil ui` or `fossil
server`.  If there is no checkout it serves the *tip* version (the
newest version, regardless of which branch it lives in).

Alternatively, serve a specific static version of a file with:

```css
$baseurl/raw/doc/highlightjs/hybrid.css?name=THE_UUID
```

Where `THE_UUID` is the UUID of the version of that file you
want. Using a checkin tag/branch name here does not work - it must be
the hash of some version of the file's contents. Note that the path
passed to `/raw` is largely irrelevant, and is primarily used by the
`/raw` route to determine which mimetype to set on the response. Thus
passing `foo.css` will work, so long as the `name=UUID` part is
correct.

Or deliver a version from a given branch:

```css
$baseurl/raw?ci=trunk&filename=doc/highlightjs/hybrid.css
```

Noting that in the `ci=trunk` part, `trunk` may be replaced by any tag
or checkin UUID (as opposed to the *file content's* UUID).


## Fossil Skin Footer

Add this, or something like it, to the bottom of the skin footer,
adjusting the paths for your installation:

```html
<script src="$baseurl/doc/ckout/doc/highlightjs/highlight.pack.js"></script>
<script nonce="$<nonce>">
if( "undefined"!==typeof hljs
    && /\/doc\/\w+/.test(window.location.pathname)
    // ^^^^ OPTIONAL: only init on /doc/... pages, noting that the skin-editing
    // process might inject a path element *before* /doc, so don't use the
    // regex ^ anchor: ^/doc
    ){
   hljs.initHighlightingOnLoad();
}</script>
```

Note that the various path-linking options for the CSS (described in
the previous section) work the same for serving JS files.

The above script block will tell highlightjs to perform its default
behaviour on any `pre > code` blocks. That means that it will look for
a `language-XYZ` class on each such block to determine its
syntax-highlighting format or, if there is no such class, it will
attempt to guess the language via brute-force trial and error (which
will, at least on Firefox, generate unsightly noise on the development
console as various brute-force attempts fail).

To control exactly which language highlighting highlightjs applies,
which stops it from attempting brute-force and squelches the
above-mentioned warnings, use something like:

```html
<script nonce="$<nonce>">
if( "undefined"!==typeof hljs
    && /doc\/\w+/.test(window.location.pathname) ){
    // ^^^^ optional - see the snippet above for why this might be desired.
  document.addEventListener(
    "DOMContentLoaded",
    function(event){
      /**
        Jump through some hoops to squelch hljs's console warnings about
        unknown language blocks which we style ourselves (e.g. language-s2-member).
        This approach also stops hljs from brute-force guessing what
        any given block is, which is an unnecessary CPU hog/battery drain.
      */
      [
          "bash", "c", "console", "cpp",
          "css", "h", "hpp", "html", "javascript",
          "js", "json", "makefile",
          "markdown", "plaintext",
          "shell", "sql",
          "s2" // s2 requires /doc/highlightjs/hljs-s2.js from this site
      ].forEach(function(L){
        document.querySelectorAll(
          "pre > code.language-"+L
        ).forEach((block)=>hljs.highlightBlock(block));
      });
    }
  );
}</script>
```

# Highlightjs Custom Build

To trim a few bytes from the highlightjs download, it's possible to
create a custom highlightjs which includes only the language packs one
wants:

<https://highlightjs.org/download/>

The following hjs language packges "might" be useful for this tree:

- CSS
- HTML
- Bash
- C++ (incl. C)
- JavaScript
- Makefile
- Markdown
- SQL
- ActionScript (a better match for s2 than JS highlighting)
