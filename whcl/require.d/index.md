# require.whcl
##### ([&#x2b11;Central whcl Documentation Hub](../)) ([&#x2b11;API Index](../manual/api-index.md))
# require.whcl: A require.js-like Resource Loader
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

See also: [the source code][require.whcl] (in directory [][require.d])

Jump to:

- [Intro](#mod-require-intro)
- [Basic Usage](#mod-require-usage)
- [Modules vs. Plugins](#mod-require-modules-vs-plugins)
  - [Modules](#mod-require-modules)
  - [Plugins](#mod-require-plugins)
- [Search Paths and Extensions](#mod-require-search-paths)
- [Testing Modules](#mod-require-testing-modules)
- [Random Tips and Tricks](#mod-require-tips-and-tricks)
  - [Getting Require Modules](#mod-require-getting-modules)


<a id="mod-require-intro"></a>
# Intro

require.whcl (we'll call it Require from here on out) is almost a clone of
the [requirejs JavaScript library](https://requirejs.org/). In short,
Require is a "dependency loader." The caller provides a list of
"dependencies," in the form of symbolic module names, Require loads
them, and then passes all of the dependencies to a callback provided by
the client (or, if no callback is provided, it returns the loaded
dependencies to the caller). require.js simplifies dependency loading in
JS immensely by hiding the asynchronous module loading behind a
synchronous interface and ensuring that all dependencies are loaded
before the client code is called. In whcl modules are not loaded
asynchronously, but Require can nonetheless greatly simplify the
creation of certain types of scripts, in particular when independent
scripts can be used together in various combinations.

Incidentally, this model of resource loading is a perfect fit for
cwal/whcl's lifetime model and garbage collector, ensuring optimal
lifetimes for loaded resources (in particular, non-cached resources).

Require is comprised of a single script and a set of conventions:

TODO: add import-like feature to whcl:

1.  Clients `eval` the require.whcl file contents. The (callable)
    object returned by that script is the Require loader, so clients
    should store it somewhere (in a const/var or property).

2.  Ideally, `require.whcl` lives in its own directory, under which
    loadable extensions are placed. This structure allows
    `require.whcl`, including its extensions, to be easily copied
    between source trees (yes, i do that). This documentation refers
    to that directory (whatever directory it gets `import()`'ed from)
    as Require's "home" directory, and it gets set as the "home"
    property of the Require object.

3.  When loaded, Require adds its home directory to its default search
    path. The intention (not requirement) is that shared Require-loaded
    files will be placed there, using subdirectories for organization.
    The current directory (".") is always added as the first search
    path, but clients are of course free to modify that.


<a id="mod-require-usage"></a>
Loading and Basic Usage
============================================================

These docs will, for brevity's sake, assume that this module has been
installed and aliased with the name `R`:

```whcl
decl -const R [whcl.install-api require]
```

That alias is not strictly necessary. After calling
`[whcl.install-api require]`, Require is globally available
via `whcl.require`.

With that in place:

```whcl
R [array module1 module2] [proc -anon {m1 m2} {
    # … m1 and m2 are the results of eval'ing
    # the files module1 resp. module2 …
}]
```

The first argument may be one of:

- An array of modules to load.
- A string naming a single module. This gets treated as if
  the caller had passed in a length-1 array of module names.

The second argument may be one of:

- A callback function.
- A block of script code.

If the 2nd arg is a function, it is passed one argument for each
entry in the list, in the same order they are imported. If it is a
code string then it is eval'd in a scope and the loaded modules are
made avilable in two forms via the `modules` local var:

1) `modules` is an array, and each index matches up with the indexes
    in the list passed to this function.

2) The `modules` array also gets member properties set with the
   names matching those passed in in the list. (We cannot declare
   those names as local vars because the given names may well
   contain characters not legal for that purpose.)

Note that these callbacks may recursively invoke Require via a call to
`whcl.require`. (It is often useful to do so, it turns out.)

It returns the result of calling the function (if any), the result of
the eval'd code block, or the array of loaded modules (if passed no
function/string).

The default behaviour is to cache each imported module, such that
loading that module again will cause the same result value to be
returned. This can be used to provide data sharing across invocations of
the module. Plugins, described later, may change not only the caching
behaviour, but also how files are searched, and some plugins don't work
with files at all (we call those "virtual" plugins, for lack of a better
term).

Alternately, Require can be called without a 2nd argument, in which
case it returns an array containing the values which would have been
passed to the callback:

```whcl
decl mods [R.require [array mod1 mod2]]
           #          ^^^ still requires the list as a single array argument!
assert info [is-array $mods]; # just for demonstration
```

<a id="mod-require-modules-vs-plugins"></a>
Modules vs. Plugins
============================================================

Some terminology used heavily throughout the API:

**Module** is generically used to mean a script or other resource
loaded by Require. Module references are strings like "moduleName" and
"module/submodule/name".

**Plugin**, in Require, specifically means a proxy which changes how
Require handles loading of a specific type of resource. Plugins allow
it to not only load script code, but arbitrary resources, including
arbitrary raw file content, database records, and whcl-loadable DLLs.

Some examples:

```whcl
R [array (
  "aModule"
  "aPlugin!aModule"
  "aPlugin!"
  # ^^^^^^^^ without a module (only legal for some plugins)
  "aPlugin!aModule?foo=hi there&bar=true&baz=3&faz"
)]
```

Plugins may offer configuration options. While they superficially
appear to be URL-encoded, they are not - no special encoding is used
except that a `&` delimits key/value pairs. A key with no value is
treated as a boolean `true` (under the assumption that it is a flag).

If Require cannot figure out what to do with an input string, or if
loading a resource fails, it will throw (or propagate) an exception.

<a id="mod-require-modules"></a>
Modules
------------------------------------------------------------

Modules, in the most basic sense, are simply whcl scripts which are,
for purposes of Require, expected to resolve to some value usable by
downstream code. In practice, modules tend to resolve to Objects,
Arrays, Functions, and the like, but in principle there is nothing
stopping a module from returning an integer, a boolean, a string, or
even `null` or `undefined`.

Here's an example of a trivial module which provides a couple of
methods:

```whcl
return object {
   method-one [proc -anon {a} {return ($a + $a)}]
   method-two [proc -anon {a} {return ($a * $a)}]
}
```

> Pedantic sidebar: in Require module scripts, a "return" at the end
is not strictly necessary because the final expression in a script is
(unlike functions) its implicit result.

If we place that content in a file named `my-module.whcl` somewhere in
Require's search path, we can then use it like this:

```whcl
R my-module [proc -anon {my} {
    assert 2 == [my.method-one 1]
    assert 4 == [my.method-two 2]
}]
```

Or, with a script snippet as a callback:

```whcl
R my-module {
    decl my modules.my
    assert 2 == [my.method-one 1]
    assert 4 == [my.method-two 2]
}
```


<a id="mod-require-plugins"></a>
Plugins
------------------------------------------------------------

Plugins implement the actual "loading" of a resource. They are defined
as an Object with a minimal interface documented in [require.whcl's
source code](/finfo/whcl/require.d/require.whcl) and summarized
below.

Built-in Plugins

1.  `default`: handles non-plugin module calls and provides the file
    search paths for other plugins which use files but do not provide a
    search path of their own.
2.  `nocache`: works like default, but bypasses the cache and does not
    cache the result.
3.  `text`: resolves to the given file's contents as a String.
4.  `buffer`: resolves to the given file's contents as a Buffer.

It also comes with examples of dynamically-loadable plugins [in its
source dir](/dir/whcl/require.d/plugins?ci=trunk).

For completeness' sake, let's demonstrate how clients can create their
own. First, let's create a plugin which returns entries from a
hypothetical app-level configuration object.

Assume we have an application-level configuration object (somewhere!)
with multiple levels of options. For example's sake, let's assume that
object looks like:

```whcl
whcl.install-api PathFinder
return object {
  ui {
    showLog true
    disableAnimations true
  }
  resources {
      iconLoader [new whcl.PathFinder
        "/opt/myapp/resources/icons"
        [array ".svg" ".png" ".xpm"/*[^3]*/]
      ]
  }
}
```

A plugin can be added to Require using two different approaches. First,
it can be added to a file with the same name as the plugin (optionally
with a subdirectory component), with an `.whcl` extension, and placed in the
directory `REQUIRE_HOME/plugins`. Secondly, it can be passed to
`R.add-plugin`.

Before demonstrating the implementation, let's show how the plugin
should be used:

```whcl
R 'myPlugin!configOptionName' [proc -anon {configOpt} {...}]
```

The `R.get-plugin` method can be used to fetch (lazily loading, if
needed) a plugin object, but it is not expected that clients will ever
really need to do so except possibly to modify the search paths used
by them. In particular, modifying the `path` and `extensions` properties
(search path and extension lists, respectively) of the default plugin
changes the search path/extension list for any other non-virtual
plugin which does not define its own search path and/or extensions.

Now both installation approaches…

Contents of `REQUIRE_HOME/plugins/myPlugin.whcl`:

```whcl
return object {
  is-virtual true /* means Require must not do file lookups for our plugin */
  cache-it false /* means Require must not cache load() calls for this plugin */
  config [getMyGlobalConfigObject], /* how you get this object is your business */
  load [proc -anon {name opt} { /* called by Require when the plugin is used. */
    /* this == the plugin object.
       name is the "name" part after the "!" in the string passed by the client.
       May be a falsy value (no name provided).
       If the caller passes URL-style arguments (?a=b&c=...) then they are
       provided as an object (key/value pairs) via the second parameter.
       Passing options always bypasses the cache, because the options presumably
       affect how the plugin behaves.
       [^4] */
    affirm [info is-string $name]
    return this.config[ $name ]
  }]
}
```

Or install it using `R.add-plugin`:

```whcl
R.add-plugin 'myPlugin' { … the plugin object … }
```

Of course, the above implementation could be enhanced to support
traversing sub-trees of the configuration, e.g. via
`myPlugin!parent/child/option`, but that's beyond the scope of this
demonstration. Note, also, that the config object itself can be a
Require module, such that loading, e.g. the `myConfig` _module_ resolves
to the top-level configuration object. In fact, this particular use case
(serving config options) is arguably better served by a module (which
provides APIs to the client for fetching/modifying config data), as
opposed to a plugin, but... my imagination for creating a custom
plugin to demonstrate is failing me :/.

<a id="mod-require-search-paths"></a>
Search Paths and File Extensions
============================================================

By default, modules are assumed to be base names of files (possibly
with a subdirectory component), and Require searches for them using a
plugin-dependent set of search paths and file extensions, defaulting
to those of the default plugin. Setting the `is-virtual` plugin
property to a truthy value disables this - Require will then perform no
file lookup for the module name, and will pass it on as-is to the
plugin for handling (which might do its own file lookup!).  The search
paths and file extensions are set via the `path` resp. `extensions`
properties of a plugin (these properties are derived from the
[PathFinder class](../../manual/type-pathfinder.md), which is used to
search for files, as it uses those naming conventions). If a plugin is
not virtual but has no `path`/`extensions` properties of its own, those
of the default plugin are used.

See [require.whcl][] (search for
"path" and "extensions") or the [dynamically-loadable
plugins](/dir/whcl/require.d/plugins?ci=trunk)
for several examples of setting up paths and extensions.

By default, the search path includes the current directory and
Require's home directory, and the only file extension used by default
is `".whcl"`. Clients are of course free to modify both of those lists
(whcl does not require any specific file extension for script files,
of course).

Generic modules which have no dependencies on project-local code
"should" be placed under Require's home directory, using subdirectories
to group functionality. One useful convention for is
`ProjectName/FeatureSet/FeatureName.whcl`. For example, `"myProj/db/users"`
or `"myProj/dogs/puppy"`.

The [main require.whcl source dir][require.d] has examples of modules
and plugins, demonstrating some semblance of structure.

<a id="mod-require-testing-modules"></a>
# Testing Modules

The canonical whcl source tree includes
[a shell script](/finfo/whcl/r-tester.sh) which simplifies the testing of
Require modules. To create a module test, place a file in the same
directory as the module, with the same base name but with a ".test.whcl"
extension. A typical test script Requires the module(s) it is testing
and throws an error (or asserts) on failure. Here's an example
session:

```shell
[stephan@host:~/fossil/cwal/whcl]$ ./r-tester.sh
```

Individual tests can be run by passing one or more module names to the
test script (without the `.test.whcl` extension part), and valgrind
tests can be run by passing `-vg` to the script[^8], e.g.:

```shell
$ ./r-tester.sh -vg moduleName1 moduleName2
```

Pass it `-?` to see the full help text.

<a id="mod-require-tips-and-tricks"></a>
Random Tips and Tricks
============================================================

Fetching a single module using conventional call semantics is
simple:

```whcl
decl -const module [R 'moduleName'].0;
```


<!--

Potential TODO: port over these modules from s2...

<a id="mod-require-getting-modules"></a>
## Getting Require Modules

The core source repository contains a number of ready-to-use modules in
the [require.d](/dir/s2/require.d) directory. Some of the highlights include:

- [cliargs](/finfo/s2/require.d/cliargs.s2) provides helper
  functions for working with script-level command-line arguments. In
  `s2sh`, all arguments passed after "--" (a double-dash) are made
  available to scripts via `s2.ARGV`, and this module simplifies usage
  of such parameters.

- [json2](/finfo/s2/require.d/json2.s2) provides an alternative
  to (an extension of) the s2.json APIs, extending the to-JSON support
  to allow clients to customize it by adding a toJSON() method to
  their objects. It also provides more flexible indention support than
  the `s2.json` (C-level) APIs do.  (That's not necessarily true any
  more: in the meantime, the C-level JSON API accepts strings, rather
  than numbers, for specifying indentation.)

- [pubsub](/finfo/s2/require.d/pubsub.s2) offers a simple
  publish/subscriber manager.

- [Ticker](/finfo/s2/require.d/Ticker.s2)[^7] provides a
  synchronous pseudo-timer similar to JavaScript's `setInterval()` and
  `setTimeout()` functions, except that Ticker uses an abstract clock
  which requires the client to increment it. Ticker then takes care of
  firing any callbacks whose time has come, either a single time or
  repeating every N ticks of the virtual clock. It is *not* an
  asynchronous timer: cwal/s2 do not support async operation.

-->

# Footnotes

[^3]: XPM: don't knock it 'til you've tried it!

[^4]: whcl pro tip: add such comments outside of function bodies to save
    memory!

[^8]: Noting that valgrind may well report that the system-level module
   loader leaks memory. Not my fault.

[require.whcl]: /finfo/whcl/require.d/require.whcl
[require.d]: /dir/whcl/require.d/
