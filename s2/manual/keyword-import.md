# s2: import
#### ([&#x2b11;Table of Contents](./))

Jump to:

* [The `import` Keyword](#keyword-import)
* [Tips and Tricks](#keyword-import-tips)
* [Footnotes](#keyword-import-footnotes)


<a id="keyword-import"></a>
# The `import` Keyword

The `import` keyword was (finally) added on 20200118.

`import` resolves to a function which loads and evaluate s2
scripts. It works similarly to the `s2.import()` function which
[`s2sh`](s2sh.md) has historically provided, with the addition that
its search path and list of file extensions are configurable. It does
not provide a new feature, strictly speaking, but it does add the
feature to the core language, where it previously required
installation of an external function binding.

Its call signatures are:

```s2-member
mixed import([bool doPathSearch=import.doPathSearch,] string filename)
Function import(bool doPathSearchByDefault)
```

`import.path` is a [`PathFinder`](type-pathfinder.md) instance which is
used to look up file names passed to `import()`. As [described in
the `PathFinder` docs](type-pathfinder.md#type-pathfinder-configure),
the `prefix` and `suffix` properties of that object may be manipulated
to configure the search path and extensions, respectively.

The initial values for the lookup path and extensions are taken from
the environment variables `S2_IMPORT_PATH`
resp. `S2_IMPORT_EXTENSIONS`, each of which may be a conventional
PATH-style string parseable by
[`PathFinder.tokenizePath()`](type-pathfinder.md). If either of them
is not set, a default value is used: the default list of directories
is completely empty (not even the current directory) and the default
extension list is `[".s2"]`. (Note that even with an empty search
path, files can still be found if their searched-for name is found
as-is or in combination with one of the configured extensions.)

Its various call forms behave like so:

- `import(true, string filename)` searches for the given filename
using `import.path`. Note that this expects a boolean-type first
argument, not an arbitrary truthy expression.
- `import(false, string filename)` uses the provided filename as-is,
bypassing the `import.path` search. Note that this expects a
boolean-type first argument, not an arbitrary falsy expression.
- `import(string filename)` behaves like `import(bool X, filename)`,
where `X` is the configured default value for whether to use
the `import.path` search. By default it is `true`, but the default
may be modified with...
- `import(bool doPathSearchByDefault)` sets the default value for
the do-path-search flag. When called this way, `import()` returns itself.
This flag is set as `import.doPathSearch` and is initially `true`.

It throws an exception if the given file is not found or not readable,
else it tries to import and evaluate the file's contents as s2
code. If it throws because the file cannot be found *and*
`import.path` searching is enabled for this call, the thrown exception
will contain a property named `notFound`: it's an object containing
information about the failed search, with properties named `filename`,
`path`, and `extensions`.

Imported scripts are run in a new scope[^2] and `import()` evaluates
to the result of the script it imports (`undefined` if it is empty or
has no result). It propagates any exception, failed assertions, `exit`,
or `fatal` calls, but treats a `return` as a clean exit from that
script and propagates the result to the caller. Syntax errors in the
file are reported via exceptions so that they are not fatal to the
outer script so long as an outer script has a `catch` in place to
intercept the exception.

Note that `import()` "can" be used to read in JSON files, as JSON
syntax is a subset of s2 syntax, but it should not be used to read
"untrusted" JSON because the input may then evaluate arbitrary s2
code.


The `import` function is "sealed" - trying to set new properties on
it, or to reassign any of its properties, will fail. The properties of
its `path` member may be freely manipulated, however, e.g. to swap
search paths/extensions in and out.

> Sidebar: whether or not allowing multiple filenames to be passed to
it is under consideration, but it's looking unlikely because its
primary use is to import data from other scripts, which we can't
really do if it accepts multiple filenames (only the result of the
final file would be returned or we'd have to return a list of all
files' results). That said...

> Sidebar: the reason the optional boolean flag comes first in the
list is to allow us to potentially add support for passing multiple
filenames without changing the call signature in an incompatible way.

> Sidebar: to simplify the implementation notably, `import` is a
genuine function, not a function-like keyword (like `typeinfo()` and
`pragma()`). Because of that, we don't have a the syntactic freedom to
make its `()` optional[^1]. Also because of that, however, client code is
free to take a reference to the function, access and manipulate its
`path` member, and use other methods derived from the Function
prototype.

> Sidebar: if s2's file-reading/stat'ing features [are disabled](misc-disable.md),
`import` will refuse to work.

A demonstration, run from a console:

```bash
$ export S2_IMPORT_PATH=require.d
$ s2sh -v
...
// Lookup path:
s2sh> import.path.prefix
result: array@0x5564ec0e9a60[scope=#1 ref#=1] ==> ["require.d"]
s2sh> import.path.suffix
result: array@0x5564ec0e9e60[scope=#1 ref#=1] ==> [".s2"]

// Importing a file by partial name:
s2sh> const r = import('require'); // ==> "require.d/require.s2"
result: require.s2@0x5564ec116ab0[scope=#1 ref#=9] ==> {
  "__typename": "require.s2",
  "home": "/home/stephan/fossil/cwal/s2/require.d",
<...SNIP...>
}
s2sh> typeinfo(name r)
result: string@0x5564ec11c480[scope=#1 ref#=0] ==> "require.s2"

// Changing the lookup path:
s2sh> import.path.prefix = ['.']
result: array@0x560dadaa5380[scope=#1 ref#=1] ==> ["."]
s2sh> import('require.d/require') // ==> "require.d/require.s2" (again)
result: require.s2@0x560dadadd4c0[scope=#1 ref#=8] ==> {
  "__typename": "require.s2",
  "home": "/home/stephan/fossil/cwal/s2/require.d",
<...SNIP...>
}

// The import object may not be modified directly, however:
s2sh> import.path = 1
rc=105 (CWAL_RC_EXCEPTION)
EXCEPTION: exception@0x560dadadcf90[scope=#1 ref#=0] ==> {
  "code": 310,
  "column": 14,
  "line": 1,
  "message": "Setting/clearing properties is disallowed on this container (of type 'function').",
  "script": "shell input"
}
```

<a id="keyword-import-tips"></a>
# Tips and Tricks

## Don't Use `import(bool)` Unless...

The ability to turn `import()`'s default path-search behaviour on and
off was added primarily because it was easy to do and disabling it
provides a bit more security, and *not* because client code should
actually use that feature. Disabling path lookup makes `import()` more
difficult to use, as apps need to be certain of all paths to all files
they import, but it also provides more safety against
unintended/malicious imports for the same reason.

The toggle generally "should not" be flipped back and forth during the
life of a script. It "should" be set once, at the start of the app,
and left that way for the entire session.


## Multiple Custom Search Paths

When an application requires multiple independent search paths, it's
generally a good idea to *not* add those paths to `import.path` and
instead create special-purpose `import()` proxies which do the
following:

- Set up their own [`PathFinder`](type-pathfinder.md) instance(s).
- Resolve paths using that/those instance(s).
- Pass the resolved paths to `import(false, thePath)`.

It may be tempting to swap `import.path.prefix` in and out on demand,
but keeping that in sync in the face of exceptions propagated via
imported scripts quickly gets tedious. It's far simpler, in practice,
to use purpose-built `import()` proxies with customized `PathFinder`
instances.


<a id="keyword-import-footnotes"></a>
# Footnotes

[^1]: Yes, we "could" make usage of the parens optional, but that
    would complicate the implementation greatly.

[^2]: Because `import()` is implemented as a function, as opposed to a
lower-level feature, it cannot run the scripts in the current
(calling) scope.  Even if it "could", though, doing so would probably
not be a great idea because the risk of unwanted side effects like
scope-local symbol collisions would be relatively high. External scripts
can be run in the current scope by loading them via `s2.Buffer.readFile()`
and calling `eval->theFileContents.takeString()` (or similar).
