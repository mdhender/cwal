# whcl: API Index
##### ([&#x2b11;Table of Contents](./))
# API Index
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

Index of whcl APIs:

- [Builtin Commands and Values](builtins.md)
- [Builtin Types](type-intro.md)

<a id='install-api'></a>
Static Modules
============================================================

By default, non-core APIs are built into the library but not installed
in the interpreter by default (to reduce the engine startup costs by
adding features most scripts won't use). One of the core APIs,
however, is:

`whcl install-api`

which accepts one or more string arguments which name supplemental
APIs to load. It throws if passed a name it does not know. They can be
loaded as needed and loading them more than once is a harmless
no-op. The result of this call is the value associated with the final
module it loads. If called with no arguments, it instead returns a
space-delimited string of all modules known by that function. From
script code that string can be turned into an array with
`[the-string split ' ']`.

Each module this function installs becomes available in the
`whcl[MODULE_NAME]` property, but the exact type of each of those is
unspecified. They are typically an object with multiple functions but
may be a single function or even a single value.

The list of modules is hard-coded at build time. The standard
list of modules loadable this way is (in alphabetical order):

- [`fs`](api-fs.md): Filesytem APIs
- [`json`](api-json.md): JSON APIs
- [`math`](api-math.md): C99 Math APIs
- [`os`](api-os.md): OS/Environment-related APIs
- [`ob`](api-ob.md): Output Buffering (OB) APIs
- [`PathFinder`](type-pathfinder.md): The PathFinder class searches for files
- [`require`](../require.d/): a "dependency and resource loader" akin
  to [require.js](https://requirejs.org/)
- [`Script`](type-script.md): The Script class compiles scripts
- [`time`](api-time.md): Time-related APIs
- [`tmpl`](api-tmpl.md): An API for creating scripted doc templates

In addition, it _might_, depending on build-time configuration, have
any or all of [the available loadable modules](../mod/) also built
into the library, available for use with `install-api`. (Those are
never built in to [the amalgamation
dstribution](build.md#amalgamation), however.)

Example usage:

```whcl
$ whcl install-api fs ob; # installs whcl[fs] and whcl[ob]
$ decl x [whcl install-api time]; # $x == whcl[time]
$ foreach m [[whcl install-api].split ' '] { echo $m }
```

> Sidebar: if one of those APIs relies on another one, it will
trigger the installation of any dependencies, if needed.

Loadable Modules (DLLs)
============================================================

WHCL supports loading modules which are compiled as DLLs. The interface
for doing so is described in [the loadable modules docs](../mod/).
