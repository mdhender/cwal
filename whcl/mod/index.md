# whcl: Loadable Modules API
##### ([&#x2b11;Central whcl Documentation Hub](../)) ([&#x2b11;API Index](../manual/api-index.md))
# Loadable Modules API
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

- [Available Modules](#available-modules)
- [Loading External DLLs](#loading-modules)
- [Creating Loadable Modules](#creating-modules)
- [Adding New Modules to the Canonical Source Tree](#adding-modules)
- [Module API Limitations](#limitations)

<a id="available-modules"></a>
Available Modules
============================================================

Work is underway to port the [loadable modules from s2](../../s2/mod/),
one significant difference being that an approach was developed for
whcl which allows most modules to be compiled in to whcl without requiring
that they be initialized (and therefore do not have an initialization-time
performance or memory cost). Thus modules are compiled directly in to
whcl when possible, falling back to DLL builds only when necessary.

The current list of available modules is (in alphabetical order):

- [`cgi`](./cgi/): for writing whcl-based CGI apps. Yes, CGI is still
  a thing.
- [`popen2`](./popen2/): A `popen()`-like wrapper for a two-way pipe.
- [`regex-posix`](./regex-posix/): POSIX regular expressions.
- [`sample`](/dir/whcl/mod/sample/): an example loadable module
  which implements a custom native type.
- [`sample_cpp`](/dir/whcl/mod/sample_cpp/): an example loadable
  module which implements a custom native type using C++ and "magic"
  C++ template-based conversions between whcl and native C++ types.
- [`sqlite3`](./sqlite3/): a wrapper around
  [libsqlite3](https://sqlite.org).


<a id="loading-modules"></a>
Loading External DLLs
============================================================

WHCL is capable of loading new features from DLL files (known as
"shared objects," or "SO's," in Unix parlance). The base interface for
doing so is:

```whcl
decl module [whcl load-module "/path/to/the.dll"]
```

Loadable modules do not typically install themselves anywhere, and
instead return a single value to the caller. That value is typically
an object which contains the module's functions, but any single value
is legal. Some modules may return only a single function.

The loader has a second, more obscure, call form as well:

```whcl
whcl load-module "/path/to/the.dll" "entry_point_symbol_name"
```

When modules are compiled standalone (one module per DLL), this form
is unnecessary. When multiple modules are compiled into a single DLL,
the symbol name is the name of the module, e.g. `my_module`. The DLL
loader looks for a symbol in the DLL named `whcl_module_my_module`
and, if found, uses it to initialize the module loader. For the gory
details, see the API documentation for `WHCL_MODULE_REGISTER` and
friends in `whcl.h` or (depending on the whcl build you are using)
`libwhcl.h`.

<a id="creating-modules"></a>
Creating Loadable Modules
============================================================

Creating a new loadable module is as simple as...

- Creating a function which implements the `whcl_module_init_f()`
  interface.
- Calling a macro to register that function.
- Compiling that source file into a DLL.

A documented sample module can be found at [](/dir/whcl/mod/sample/).
The build process used by the canonical whcl source tree "rather
involved" (arguably over-engineered) and the specifics of building
DLLs are platform-dependent, so are out of scope here. The sample
plugin should give a good idea of how to get started, though.


<a id='adding-modules'></a>
Adding New Modules to the Canonical Source Tree
============================================================

(This section is more of a reminder to self than end-user docs!)

The build rules for the in-tree modules are generated on-demand via [a
shell script](/file/whcl/makeModMake.sh). They automatically add any
modules for which the file `whcl/mod/MODULE/module-def.make`
exists. Each module's directory name must be the module's symbolic
name - the one client code will load it with.

The `module-def.make` file gets imported by the build process and
tells the build system all that it needs to know by way of the
following variables:

- `module.enabled` must be set to 1 if the module is to be enabled.
  This can be determined based on makefile state provided by the
  including context, e.g. feature flags.
- `module.builtin` must be set to 1 if the module should be built in
  to the resulting whcl library, else it is built as a DLL. This build
  mode requires that the module include `static_module.c` or
  `static_module.cpp`, as described below.
- <s>`module.name`</s> is _implicitly_ set to the name of the module's
  directory.  In principle it can have another name but this is
  untested.
- `module.cname` is a C-friendly form of this module's name, required
  for installing a module init function. One example is the
  `regex-posix` module, which is a non-C-conformant name. Its `cname`
  is set to `regex_posix` to make up for that. By default the `cname`
  is equal to the `name`.
- `module.obj` lists all object files for the module. All are required
  to be relative to the module's directory.
- `module.CPPFLAGS` any CPP flags required for building the module's
  files.
- `module.CFLAGS` any C compiler required for building the module's
  files.
- `module.LDFLAGS` any linker flags required for building the module's
  files. These flags get applied to both the DLL build of the module
  and to any of whcl's own binaries which build this module in
  statically.
- `module.tests` an optional list of WHCL script files, relative to
  the module directory, which provide unit tests. Running
  `make test` from a module's subdirectory will, when using a
  compliant `GNUmakefile` (see the existing modules), will run each
  of these tests.
- `module.script.whcl` is only used by modules which have no native
  code and are generated from a single WHCL script file. This must be
  set the name of a script file used for generating the module.  The
  name must be either relative to the module dir or relative to the
  top of the build tree.

Before `include`'ing `module-def.make`, the build rules first clear
all of the various `module.xxx` vars and set:

- `WHCL.module.<MODULENAME>.dir` = the directory which houses the module.
- `WHCL.module.<MODULENAME>.def` = the `module-def.make` file name.

The build process has to prefix all of the module-related var names
with `WHCL` because (1) the source tree uses a single monolithic
build file to get optimal parallel build support and (2) without
a prefix, the names collide with modules from the s2 build
(which do not have such a prefix because this build approach was
not yet foreseen when that code was written).

Some not-quite-trivial `module-def.make` files make use of more state
provided by the higher-level makefile. In principle and module
makefile may safely use any vars set by the [top-level main makefile](/finfo/GNUmakefile.in),
the [`auto.make`](/finfo/auto.make.in), and
[`make-whcl.make`](/finfo/make-whcl.make.in).

To build a module as a built-in, the module must have either
`static_module.c` or `static_module.cpp` and that file must directly
`#include` all other C/C++ files it requires. This simplifies the whcl
build process by having only a single file per module with a
well-defined name, at the cost of increased compile time when only a
subset of the `#include`d files changes. See [any of the existing
modules](/dir/whcl/mod) for examples of how these are set up.



<a id="limitations"></a>
Module API Limitations
============================================================

Over the years this same module API has seen a great deal of service
in [the s2 project](/doc/$CURRENT/s2/) and its predecessors. Based on
that experience, the known shortcomings of this API are...

- Building separate loadable modules which can depend on each other is
  far from straightforward. Some experimentation has been done with
  that but a fully-formed working example for doing so has never been
  created. "One of these days" that might genuinely itch me enough to
  entice me to solve it, but today is not that day.

... that's been the only genuine annoyance so far.
