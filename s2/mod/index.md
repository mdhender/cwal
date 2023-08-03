# s2 Modules
#### ([&#x2b11;Central s2 Documentation Hub](../))
# Supplemental Script APIs and Loadable Modules for s2

This document covers "supplemental" (non-core) script APIs and loadable
modules developed for the [s2 scripting
engine](/wiki/s2) which may have generic applicability in arbitrary
s2-using clients. It is assumed that the reader is familiar with s2.

**Jump to**:

* [Loadable Module Mini-HowTo](#mod-mini-howto)
* [Using `loadModule()`](#mod-loadmodule)
    * [Configurable `loadModule()` Paths](#mod-loadmodule-paths)
* [TODO](#mod-todo)

**Modules**:

**Tier One:** these are modules which i actively use, primarily as
part of CGI scripts. Listed in very rough approximate order of their
overall prevalence in their ecosystem:

* [require.s2](require/) - generic "dependency loader" modelled after require.js.
* [CGI](cgi/) - mini-framework for implementing CGIs with s2.
* [sqlite3](sqlite3/) - database access module.
* Regular Expressions
    * [JS-like](regex_js/) - adopted from the [MuJS project](https://github.com/ccxvii/mujs).
    * [POSIX](regex_posix/) - regex API and documentation for the
      common features of both regex modules.
* [Hashing](hashing/) - SHA1 and SHA3.
* [UUID generator](uuid/) - has *one job*.


**Tier Two:** these modules exist essentially "because they can," and
are sometimes interesting to tinker on/with, but do not necessarily
see much real use. In alphabetical order:

* [C99 Math](math/) - wraps up most of the C99-specific math functionality.
* [dirent](dirent/) - wraps the POSIX `dirent`/`opendir()` family of APIs.
* [felta](felta/) - the content-delta generation routines used by the
  [Fossil SCM](https://fossil-scm.org), ported into a standalone library.
* [FILE](FILE/) - wraps the C89 FILE class.
* [popen2](popen2/) - pipe input to external processes and read their output.
* [termbox](termbox/) - a terminal UI toolkit similar to, but orders of
  magnitude easier to use than, curses.

**See also**:

* [`S2_HOME`](../manual/s2-home.md) describes a common installation approach
  for loadable modules.
* [s2sh: the s2 shell](../manual/s2sh.md)
* [Extending s2 from C](../manual/s2-from-c.md)
* [The source code](/dir/s2/mod?ci=trunk) for the modules listed above
  demonstrates how to extend s2 using loadable modules. In particular...
    * [The sample module](/dir/s2/mod/sample?ci=trunk) provides a
  documented introduction to the topic.

<a id="mod-mini-howto"></a>
# s2sh Loadable Modules Mini-HOWTO

Most of the modules covered by this family of docs are available as
"loadable modules" which can either be loaded at runtime using
`s2.loadModule()` or (in most cases) be compiled directly in to
[s2sh](../manual/s2sh.md) or [s2sh2](../manual/s2sh2.md).

Though s2 is designed to be embedded in arbitrary software, it of course
has a "default" shell application, generically called s2sh, which
supports interactive use and running script files. Though the library
can import loadable modules from DLLs both with and without the shell,
the shell supports build-time options for statically linking modules
directly into the shell, such that they are always available to it
without having to load them "by hand". Here's a quick how-to…

First, take a look in the directory
[/s2/mod](/dir/s2/mod?ci=trunk)
and decide which modules you'd like to include. (Not all loadable
modules have the build infrastructure to support static linking into the
shell, but most of them do.) Also note that some of the modules are
superfluous because the shell embeds those APIs directly (e.g. the json
and io modules). Those exist as modules primarily to support apps other
than s2sh which don't bind those APIs themselves.

Secondly, via the configure script, tell the build tree which modules to
include:

```console
$ ./configure --s2sh-modules='regex_js dirent popen2'
```

If you only want dynamically-loadable modules, don't use the
`--s2sh-modules` flag - that flag only specifies which modules to compile
*statically* into the shell. Pass the `--help` flag to see all available
options.

The module name `all` can be used to include all modules which (A)
can be built statically and (B) the configure process does not implicitly
exclude due to missing prerequisites. Additionally, the `--cgi` configure
flag pre-sets a list of modules which have proven useful in implementing
CGI apps (the exact list is defined somewhere in [auto.def](/finfo/auto.def)).

For example:

```bash
$ ./configure --cgi
...
Sanity-checking s2sh modules ...
	[cgi]: found s2/mod/cgi/static_module.c
	[hashing]: found s2/mod/hashing/static_module.c
	[ob]: found s2/mod/ob/static_module.c
	[regex_js]: found s2/mod/regex_js/static_module.c
	[regex_posix]: found s2/mod/regex_posix/static_module.c
	[require]: found s2/mod/require/static_module.c
	[sqlite3]: found s2/mod/sqlite3/static_module.c
	[uuid]: found s2/mod/uuid/static_module.c
...
```

Then build everything:

```bash
# From the top of the source tree:
$ make # builds the core libcwal
$ cd s2
$ make # builds libs2 and s2sh/s2sh2
$ make unit # runs the unit tests
```

That's all there is to it. The resulting `s2sh` and `s2sh2` binaries
contains the given modules and will initialize them when it starts up
(`s2sh`'s `--M` flag, and `s2sh2`'s `-nomi` flag, can be used to
suppress that initialization, but that would rarely be an interesting
thing to do[^1]).

See [s2-home](../manual/s2-home.md) for recommendations about how/where
to install the `s2sh`/`s2sh2` binary.

To build dynamically loadable copies of the modules, one more step is
needed:

```bash
# from the s2 directory
$ cd mod
$ make
$ make test
```

(There may be modules which don't get built - that's because the
configure script didn't find prerequisite libraries, headers,
and/or compiler needed for them (some require C++).)

Each module lives in its own directory and has an `.so` file with the
same base name as the directory. Those `.so` files can be moved
anywhere you like and be loaded at runtime using
`s2.loadModule()`. There is no "standard" for where these need to
live, nor is there currently a common installation process, so simply
copy or move the shared libraries wherever is appropriate for your
installation. That said…

<a id="mod-loadmodule"></a>
# `s2.loadModule()`

The functionality described here is installed by `s2sh` and `s2sh2`
using the name `s2.loadModule()` and/or `$2.loadModule()`, but client
apps may install it using any name they like.

s2 includes a basic module loading system so that new components can be
loaded from DLLs (that's "shared objects" for you Unix users), from C or
script code, at runtime. It currently has implementations for platforms
hosting `dlopen()` or `lt_dlopen()`, and patches for other platforms are
welcomed. See the [sample module](/dir/s2/mod/sample?ci=trunk)
for a complete example.

Achtung: the interface changed significantly (but subtly) on 20180101.
In short: all modules now return their values directly via `loadModule()`,
rather than via a caller-provided intermediary object. (The older
approach led to too much confusion in client scripts.)

The core script-level API for loading modules has one function and two
usages:

```s2-member
mixed loadModule( string dllFileName )
mixed loadModule( string dllFileName, string symbolName )
```
The first argument is a DLL file name (a.k.a. an `.so` file on Unix
platforms).

The first form opens the given DLL and looks for a "standalone" s2
loadable module. If a DLL contains multiple modules, the second form
must be used to distinguish between the modules: they will (if built
properly) each be built with a symbol named `s2_module_{name}`, and
that `{name}` part is what should be passed to this function. For the gory
C-level details, see the docs in
[s2.h](/finfo/s2/s2.h): search for `S2_MODULE_DECL` and `s2_module_load`.
In practice, the second form is never used/required because modules
get built one per DLL.

The result of the module's initialization is returned via
`loadModule()`.  Most modules tend to return an object containing
their APIs. Some return a single function. Hypothetically they can
return an integer or some other trivial value. Some rare few extend
built-in prototypes, e.g. libfossil added zlib compression support to
the [Buffer class](../manual/type-buffer.md) before that feature was
moved into the s2 core library.

If loading the DLL fails for any reason (e.g. cannot be found or inits
init routine fails), `loadModule()` throws an exception.

<a id="mod-loadmodule-paths"></a>
# Configurable `loadModule()` Paths

Note that `loadModule()` requires that the caller know the path to the
module and its platform-specific file extension. The [PathFinder
class](../manual/type-pathfinder.md) can be used to create a wrapper
around this function which uses configurable DLL search paths and file
extensions. With a native binding for `getenv(3)` (like the one
provided by [s2sh](../manual/s2sh.md)), the options could be loaded
from the environment (or the environment could be probed to try to
determine the OS, and adjust the paths accordingly). Rather than leave
that as an exercise for the reader, here's a simple example suitable
for use in script initialization code:

```s2
affirm typeinfo(isfunction s2.loadModule);
affirm typeinfo(isfunction s2.getenv);
const cliFlags = (s2.ARGV ? s2.ARGV.flags : 0) ||| {prototype:null};
const pathFromEnv = proc(f,e){
    var p = F[f] ||| E(e);
    return typeinfo(isstring p)
        ? p.split(p.indexOf(';') >= 0 ? ';' : ':')
    : undefined;
} using {E:s2.getenv, F: cliFlags};

s2.loadModule2 = function(name,symbol){
    affirm typeinfo(isstring name);
    const n = P.search( name );
    n || throw exception('CWAL_RC_NOT_FOUND',
                          "Cannot find '"+name+"' in search path "
                          +P.prefix.toJSONString());
    const a = [R ? R(n) : n];
    symbol && (a[] = symbol);
    return L(@a);
} using {
    L: s2.loadModule,
    R: s2.fs ? s2.fs.realpath : undefined,
    P: new s2.PathFinder(
        // Directories...
        pathFromEnv('s2.module.path','S2_MODULE_PATH') ||| ['.'],
        // Extensions...
        pathFromEnv('s2.module.ext','S2_MODULE_EXTENSIONS') ||| ['.so','.dll']
    )
};
```

See [this script](/finfo/s2/s2sh.s2) for the above code and its
documentation, as well as a similar variant which extends
`s2.import()` (for loading s2 scripts).

Here's an example of how it's used (from the [s2sh interactive
shell](../manual/s2sh.md)):

```s2
s2sh> var m = s2.loadModule2('sample_module');
result: native@0x6e7d0[scope=#1@0xbe8113b0 ref#=1] ==>
native@0x6e7d0
s2sh> m.foo()
result: string@0x6ebf0[scope=#1@0xbe8113b0 ref#=0] ==> "Hello
from 0x6CB70->foo()"
s2sh> var x = s2.loadModule2('nope'); 
rc=105 (CWAL_RC_EXCEPTION)
EXCEPTION: exception@0x6e9f8[scope=#1@0xbe8113b0 ref#=0] ==> {
"code": 104,
"column": 8,
"line": 16,
"message": "Cannot find 'nope' in search path ["."]",
"script": "./s2sh.s2",
"stackTrace": [{
 "column": 22,
 "line": 1,
 "script": "shell input"
 }]
}
s2sh> unset m
MARKER: sample_module.c:43:my_native_finalize():Finalizing
my_native@0x6e7d0 (and avoiding an unused var warning while doing it)
result: undefined@0x6a424[scope=#0@(nil) ref#=0] ==> undefined
```

<a id="mod-todo"></a>
# TODOs

- Come up with a way to package the modules such that they can be more
  easily distributed and built from outside of the main tree,
  primarily for use with app-specific s2sh clients. Maybe set up a
  standalone "module build" package which knows how to find s2sh and
  set up s2-specific compiler/linker flags for any modules which are
  unpacked under it. That would spare the modules from each having to
  carry around the tooling to find s2 and set up the compiler/linker
  flags and such.

- Find a way to allow modules to check for modules they depend on, or
  can make use of, and get access to them (if they're
  loaded). Cross-module use as the C level is currently quite tedious,
  and there's no "standardized" way for a module, script-side, to
  determine whethere any other modules are loaded.

# Footnotes

[^1]: There are hypothetically cases where module initialization could
    fail, which would keep `s2sh` from starting, and that flag can
    provide a temporary workaround. In practice, i've never actually seen
    it happen, but it's technically a possibility.
