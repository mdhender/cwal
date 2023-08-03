# require.s2
#### ([&#x2b11;Main Module Docs](../))
# require.s2: A require.js-like Resource Loader

- Source code:
  - [Main script code](/finfo/s2/require.d/require.s2) (in [](/dir/s2/require.d/))
  - [Loadable module](/dir/s2/mod/require?ci=$SELF)
- Test/demo code: [](/finfo/s2/mod/require/test.s2)



Jump to:

- [Intro](#mod-require-intro)
- [Basic Usage](#mod-require-usage)
- [Modules vs. Plugins](#mod-require-modules-vs-plugins)
  - [Modules](#mod-require-modules)
  - [Plugins](#mod-require-plugins)
- [Search Paths and Extensions](#mod-require-search-paths)
- [Recursive Requiring: the requireS2 Symbol](#mod-require-recursive)
- [Installing Require During App Startup](#mod-require-installing-into-app)
- [Testing Modules](#mod-require-testing-modules)
- [Random Tips and Tricks](#mod-require-tips-and-tricks)
  - [Getting Require Modules](#mod-require-getting-modules)
  - [Embedding require.s2 in C](#mod-require-embedding-in-c)



<a id="mod-require-intro"></a>
# Intro

require.s2 (we'll call it Require from here on out) is almost a clone of
the [requirejs JavaScript library](https://requirejs.org/). In short,
Require is a "dependency loader." The caller provides a list of
"dependencies," in the form of symbolic module names, Require loads
them, and then passes all of the dependencies to a callback provided by
the client (or, if no callback is provided, it returns the loaded
dependencies to the caller). require.js simplifies dependency loading in
JS immensely by hiding the asynchronous module loading behind a
synchronous interface and ensuring that all dependencies are loaded
before the client code is called. In s2 modules are not loaded
asynchronously, but Require can nonetheless greatly simplify the
creation of certain types of scripts, in particular when independent
scripts can be used together in various combinations.

Like require.js, one of its properties is that it does not introduce
any new "global" symbols, but unlike JS, s2 has no global scope which
is directly writable to from script code[^2], which means that even
storing Require into a global symbol can be trickier than it sounds
;). (One approach is suggested later on the chapter.)

Incidentally, this model of resource loading is a perfect fit for
cwal/s2's lifetime model and garbage collector, ensuring optimal
lifetimes for loaded resources (in particular, non-cached resources).

Require is comprised of a single s2 script and a set of conventions:

1.  Clients use [`import()`](../../manual/keyword-import.md) (or
    equivalent[^9]) to load the `require.s2` script. The (callable) object
    returned by that script is the Require loader, so clients should
    store it somewhere (in a const/var or property).

2.  Ideally, `require.s2` lives in its own directory, under which
    loadable extensions are placed. This structure allows
    `require.s2`, including its extensions, to be easily copied
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
# Loading and Basic Usage

It's really easy:

```js
const R = import('require.d/require.s2');
R(['module1', 'module2'],
  function( m1, m2 ){
    // … m1 and m2 are the results of import()'ing
    //  the files module1 resp. module2 …
  });
```

The first argument is a list of modules to load. The second argument
is a function to call after loading them. The function is passed one
argument for each entry in the list, the value of which is the value
from loading the given module. In the above example, Require would
look for two files, named `module1.s2` and `module2.s2`, using a
configurable search path (with configurable extensions, as well). If
found, it uses `import()` to read the files and stores each one's
result in the corresponding index of a result list. If Require is
passed a function, that result list becomes the arguments for the call
to that function, otherwise Require returns the list to the caller. If
loading of any given resource fails, an exception is thrown.  Thus
when the callback is called resp. the list of modules is returned, all
the requested modules (and any modules *they* load) are guaranteed to
have been loaded.

The default behaviour is to cache each imported module, such that
loading that module again will cause the same result value to be
returned. This can be used to provide data sharing across invocations of
the module. Plugins, described later, may change not only the caching
behaviour, but also how files are searched, and some plugins don't work
with files at all (we call those "virtual" plugins, for lack of a better
term).

Alternately, Require can be called without a function argument, in which
case it returns an array containing the values which would have been
passed to the callback:

```s2
var mods = R(['mod1', 'mod2']);
           // ^^^ still requires the list as a single array argument!
assert typeinfo(isarray mods); // just for demonstration
```

<a id="mod-require-modules-vs-plugins"></a>
# Modules vs. Plugins

Some terminology used heavily throughout the API:

**Module** is generically used to mean a script or other resource
loaded by Require. Module references are strings like "moduleName" and
"module/submodule/name".

**Plugin**, in Require, specifically means a proxy which changes how
Require handles loading of a specific type of resource. Plugins allow
it to not only load script code, but arbitrary resources, including
arbitrary raw file content, database records, and s2-loadable DLLs.

Some examples:

```s2
R([
  "aModule",
  "aPlugin!aModule",
  "aPlugin!", // without a module (only legal for some plugins)
  "aPlugin!aModule?foo=hi there&bar=true&baz=3&faz"
])
```

Plugins may offer configuration options. While they superficially
appear to be URL-encoded, they are not - no special encoding is used
except that a `&` delimits key/value pairs. A key with no value is
treated as a boolean `true` (under the assumption that it is a flag).

If Require cannot figure out what to do with an input string, or if
loading a resource fails, it will throw (or propagate) an exception.

<a id="mod-require-modules"></a>
## Modules

Modules, in the most basic sense, are simply s2 scripts which are, for
purposes of Require, expected to resolve to some value usable by
downstream code. In practice, modules tend to resolve to Objects,
Arrays, Functions, and the like, but in principle there is nothing
stopping a module from returning an integer, a boolean, a string, or
even `null` or `undefined`.

Here's an example of a trivial module which provides C++like output
support to the client:

```s2
return {
   'operator<<': proc(a){o(a); return this} using{o:s2.io.output}
};
```

> Sidebar: the functionality demonstrated here has, since this example
was written, been added to the core language as the [`s2out`
keyword](../../manual/keywords.md#keyword-s2out),
so this particular module isn't particularly necessary.

(This example was written before s2 got the , which effectively does the same thing
as this module but does so more efficiently.)

> Pedantic sidebar: in Require module scripts, a "return" at the end is
not strictly necessary, nor is a trailing semicolon, because (A) the
final expression in a script is (unlike functions) its implicit result
value and (B) EOF acts like an implicit semicolon.

If we place that content in a file named "ostream.s2" somewhere in
Require's search path, we can then use it like this:

```s2
R(['ostream', …], function( os, … ){
  os << "Hi, " << "world!\n";
});
```

<a id="mod-require-plugins"></a>
## Plugins

Plugins implement the actual "loading" of a resource. They are defined
as an Object with a minimal interface documented in [require.s2's
source code](/finfo/s2/require.d/require.s2) and summarized
below.

Built-in Plugins

1.  `default`: handles non-plugin module calls and provides the file
    search paths for other plugins which use files but do not provide a
    search path of their own.
2.  `nocache`: works like default, but bypasses the cache and does not
    cache the result.
3.  `text`: resolves to the given file's contents as a String.
4.  `buffer`: resolves to the given file's contents as a Buffer.

It also comes with several dynamically-loadable plugins [in its source
dir](/dir/s2/require.d/plugins?ci=trunk).

For completeness' sake, let's demonstrate how clients can create their
own. Let's create a module which … (thinking…) returns entries from a
hypothetical app-level configuration object.

Assume we have an application-level configuration object (somewhere!)
with multiple levels of options, for example (off the top of my head):

```s2
{
  ui: {
    showLog: true,
    disableAnimations: true
  },
  resources: {
      iconLoader: new s2.PathFinder(
        ["/opt/myapp/resources/icons"],
        [".svg", ".png", ".xpm"/*[^3]*/]
      )
  }
}
```

A plugin can be added to Require using two different approaches. First,
it can be added to a file with the same name as the plugin (optionally
with a subdirectory component), with an `.s2` extension, and placed in the
directory `REQUIRE_HOME/plugins`. Secondly, it can be passed to
`Require.addPlugin()`.

Before demonstrating the implementation, let's show how the plugin
should be used:

```s2
R(['myPlugin!configOptionName'], function( configOpt ) {...});
```

The `Require.getPlugin(pluginName)` method can be used to fetch
(lazily loading, if needed) a plugin object, but it is not expected
that clients will ever really need to do so except possibly to modify
the search paths used by them. In particular, modifying the `prefix` and
`suffix` properties (search path and extension lists, respectively) of
the default plugin changes the search path/extension list for any
other non-virtual plugin which does not define its own search path
and/or extensions.

Now both installation approaches…

Contents of `REQUIRE_HOME/plugins/myPlugin.s2`:

```s2
{
  isVirtual: true, // means Require must not do file lookups for our plugin
  cacheIt: false, // means Require must not cache load() calls for this plugin
  config: myGlobalConfigObject(), // how you get this object is your  business
  load: function( name, opt ){ // called by Require when the plugin is used.
    /* this === the plugin object.
       name is the "name" part after the "!" in the string passed by the client.
       May be a falsy value (no name provided).
       If the caller passes URL-style arguments (?a=b&c=...) then they are
       provided as an object (key/value pairs) via the second parameter.
       Passing options always bypasses the cache, because the options presumably
       affect how the plugin behaves.
    */[^4]
    affirm typeinfo(isstring name);
    return this.config[ name ];
  }
}
// the final expression in the script is its result,
// so explicitly returning the plugin object is optional
```

Or install it using `addPlugin()`:

```s2
R.addPlugin( 'myPlugin', { … the plugin object … } );
```

Of course, the above implementation could be enhanced to support
traversing sub-trees of the configuration, e.g. via
`myPlugin!parent/child/option`, but that's beyond the scope of this
demonstration. Note, also, that the config object itself can be a
Require module, such that loading, e.g. the 'my-config' module resolves
to the top-level configuration object. In fact, this particular use case
(serving config options) is arguably better served by a module (which
provides APIs to the client for fetching/modifying config data), as
opposed to a plugin, but... my imagination for creating a custom
plugin to demonstrate is failing me :/.

<a id="mod-require-search-paths"></a>
# Search Paths and File Extensions

By default, modules are assumed to be base names of files (possibly
with a subdirectory component), and Require searches for them using a
plugin-dependent set of search paths and file extensions, defaulting
to those of the default plugin. Setting the `isVirtual` plugin
property to a truthy value disables this - Require will then perform no
file lookup for the module name, and will pass it on as-is to the
plugin for handling (which might do its own file lookup!).  The search
paths and file extensions are set via the `prefix` resp. `suffix`
properties of a plugin (these properties are derived from the
[PathFinder class](../../manual/type-pathfinder.md), which is used to
search for files, as it uses those naming conventions). If a plugin is
not virtual but has no `prefix`/`suffix` properties of its own, those
of the default plugin are used.

See [require.s2](/finfo/s2/require.d/require.s2) (search for
"prefix" and "suffix") or the [dynamically-loadable
plugins](/dir/s2/require.d/plugins?ci=trunk)
for several examples of setting up paths and extensions.

By default, the search path includes the current directory and
Require's home directory, and the only file extension used by default
is `".s2"`. Clients are of course free to modify both of those lists (s2
and s2sh do not require any specific file extension for script files,
of course).

Generic modules which have no dependencies on project-local code
"should" be placed under Require's home directory, using subdirectories
to group functionality. One useful convention for is
`ProjectName/FeatureSet/FeatureName.s2`. For example, `"myProj/db/users"`
or `"myProj/dogs/puppy"`.

The [main require.s2 source dir](/dir/s2/require.d) has many examples
of modules and plugins, demonstrating some semblance of structure.


<a id="mod-require-recursive"></a>
# Recursive Requiring: the requireS2 Symbol

It is often useful for modules to get access to other modules, without
having to concern themselves with whether or not the module has
already been loaded, etc. i.e. modules often find it useful to call
Require themselves. There's a catch, however: Require (true to its
model) does not install a global symbol which the modules can
reference it via. The solution for that conundrum lies in s2's funky
symbol lookup rules: in the context of a Require call, the symbol
requireS2 will be a (const) reference to the Require module loader
object[^5]. It is not a global symbol, it just happens to be
resolvable from arbitrarily deep under a Require call. Thus a module
might look like the following (taken from once-real/now-defunct code,
but only the first and last expression lines are of real note here):

```s2
return requireS2( // <== note that requireS2() call
   ['fsl/db/repo'],
   proc(repo){
       const rc = [];
       repo.each({
           mode: 0,
           sql:<<<_SQL
           SELECT e.*, b.uuid uuid
           FROM event e JOIN blob b ON e.objid=b.rid
           ORDER BY e.mtime DESC LIMIT 5
           _SQL,
           callback: proc(){rc[] = this} using(rc)
       });
       return rc;
   });
```

The "repo" module makes sure that the app-level state is set up and that
a "repository db" has been opened, throwing an exception if it is not
(or cannot be). Thus when our callback is called, all of the module's
dependencies have been successfully loaded, and it need not concern
itself at all with such details. Note that the return value of the
callback becomes the result value for the module, and when using the
default module loader it will be cached for subsequent calls.

Note that the `requireS2` symbol is defined when plugins are run as well,
so it can be used by their `load()` method.

It is important to remember that `requireS2()` is only available in the
context of a Require call (as a side effect of the symbol resolution
mechanism). It is not a global symbol, meaning that top-level clients
must keep their own copy of the Require symbol somewhere. That leads us
nicely into the next section...

## That said...

Since the above was written, s2 has gained the [`define`
keyword](../../manual/keywords.md#keyword-define), which could be used
to create a globally-accessible name for the module. Whether or not to
do so automatically when Require is loaded is up for consideration.


<a id="mod-require-installing-into-app"></a>
# Installing Require During App Startup

Since scripts never run from the global scope (unless C code
specifically runs them that way), it's not quite straightforward for
client scripts to store anything truly globally. Generally speaking,
one needs a global-level object (installed via C code) where one can
attach their functionality to. In the default s2 shell, the only such
place is the "s2" global object. In client-extended shells, there may
be other options, e.g. the client app may run an init script in the
global scope instead of a temporary subscope).

One may install Require into their persistent s2 setup via the `s2sh` init
script (or equivalent for their application):

```s2
s2.require = import(false,__FILEDIR/*[^6]*/+'require.d/require.s2' );
```

It's generally not considered good form to store client-installed code
in the global s2 object, but:

- My local `s2sh` setup has no other global objects to attach it
  to. In builds which have other global objects (like a custom build
  of s2sh), we can potentially store it in one of those.

- `import()`'d scripts (including the `s2sh` init script) run in
  their own scope, not the global one, so the object needs to be
  stored somewhere or it will be lost shortly after `import()`
  returns. (On the other hand, maybe that's the behaviour you want.)

- i will play the "i wrote s2" card and claim a bit more leeway in the
  placement of my client-side data ;).

- The property `s2.client` is reserved 100% for client-side use, so
  storing it there would be kosher (provided the client has a global
  s2 object at all).

- `require.s2` quickly proved to be tremendously useful, and may
  very well be moved into the core at some point, justifying a place
  in the core "s2" namespace.


<a id="mod-require-testing-modules"></a>
# Testing Modules

The canonical s2 source tree includes
[a shell script](/finfo/s2/r-tester.sh) which simplifies the testing of
Require modules. To create a module test, place a file in the same
directory as the module, with the same base name but with a ".test.s2"
extension. A typical test script Requires the module(s) it is testing
and throws an error (or asserts) on failure. Here's an example
session:

```bash
[stephan@host:~/fossil/cwal/s2]$ ./r-tester.sh
S2SHFLAGS=--a -R -S
Running require.s2 test: BufferFactory.test
Running require.s2 test: Ticker.test
Running require.s2 test: tmpl.test
Done! Tests run: BufferFactory.test Ticker.test tmpl.test
```

Individual tests can be run by passing one or more module names to the
test script (without the ".test.s2" extension part), and valgrind
tests can be run by passing `-vg` to the script[^8], e.g.:

```bash
$ ./r-tester.sh -vg moduleName1 moduleName2
```

Pass it `-?` to see the full help text.

<a id="mod-require-tips-and-tricks"></a>
# Random Tips and Tricks

Fetching a single module using conventional call semantics is
simple:

```s2
const module = R(['moduleName']).0;
```

If Require will be needed more than once in an application, it is
useful to store its `import()`'d somewhere (i.e. a variable or
property) so that its cache can be reused between calls. If, however,
it will only be needed one time, there is no need to give it a
symbolic name: simply directly use the instance returned by
`import()`:
  
```s2
import('path/to/require.s2')(['module1', …], proc( mod1, … ) {} );
```

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

- [ostream](/finfo/s2/require.d/ostream.s2) is a trivial module
  providing a convenience interface for generating output (an
  alternative to `print()` and friends). The module overrides the `<<`
  operator to provide C++-like output via s2's standard output
  channel. (That said: since this module was introduced, the `s2out`
  keyword was added which provides the same functionality.)

- [pubsub](/finfo/s2/require.d/pubsub.s2) offers a simple
  publish/subscriber manager.

- [Ticker](/finfo/s2/require.d/Ticker.s2)[^7] provides a
  synchronous pseudo-timer similar to JavaScript's `setInterval()` and
  `setTimeout()` functions, except that Ticker uses an abstract clock
  which requires the client to increment it. Ticker then takes care of
  firing any callbacks whose time has come, either a single time or
  repeating every N ticks of the virtual clock. It is *not* an
  asynchronous timer: cwal/s2 do not support async operation.

<a id="mod-require-embedding-in-c"></a>
## Embedding require.s2 in C

As of 20171228, s2 has a dynamically-loadable, or statically-linkable,
module which embeds a copy of `require.s2` into C code:

[](/dir?ci=trunk&name=s2/mod/require>)

(In all likelihood, that is the directory you are reading this
document from.)

# Footnotes

[^2]: That's not entirely true, but it's close: scripts, unless C code
    runs them otherwise, run in their own scope, branched off of the
    current scope.

[^3]: XPM: don't knock it 'til you've tried it!

[^4]: s2 pro tip: add such comments outside of function bodies to save
    memory!

[^5]: i initially tried to bind it as 'this' at the root of the script,
    but there are too many levels of indirection at the C API level for
    that to work.

[^6]: Trivia: s2's `__FILEDIR` keyword was added explicitly to simplify
    this type of use case.

[^7]: Ticker's upper-case name is historical, pre-dating `require.s2`
    and `Object.withThis()`, back when modules had to have a public
    symbol name (at least temporarily) in order to be useful.

[^8]: Noting that valgrind may well report that the system-level module
   loader leaks memory. Not my fault.

[^9]: When these docs were written, `s2.import()` was the way to
   import files, but the `import` keyword has since obsoleted that
   function.
