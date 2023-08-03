# whcl: PathFinder
##### ([&#x2b11;Table of Contents](./)) ([&#x2b11;API Index](api-index.md))
# The PathFinder Class
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

This API may be installed using:

`whcl install-api PathFinder`

or calling `whcl_install_api()` from C code with a module name
of `PathFinder`.

It adds a `whcl[PathFinder]` constructor function and the API
described below.

Jump to...

- [PathFinder](#type-pathfinder)
- [PathFinder Members](#type-pathfinder-members)
  - [Class-level Members](#type-pathfinder-members)
  - [Instance Members](#type-pathfinder-instance-members)
  - [Configuring Search Paths](#type-pathfinder-configure)

<a id="type-pathfinder"></a>
# PathFinder

PathFinder is a utility class for searching for files based on
client-provided search paths and file extensions/suffixes. Its primary
use is for finding library-level scripts and loadable modules based on
a configurable search paths and possibly platform-specific file
extensions (e.g. ".dll" vs ".so"). This class is implemented as a
"native" type binding, and demonstrates the plugging-in of an
arbitrary C-side struct to the scripting engine. Only a very small
amount of its code depends on whcl, per se - it primarily depends on
the engine (cwal) and not the language interpreter (whcl). The
underlying C class itself has been wandering from source tree to
source tree since the early 2000's (though it was initially
implemented in C++).

PathFinder is used like this:

```whcl
decl pf new whcl[PathFinder]
# Configure search paths:
pf add-dir '/etc' '/bin' '/usr/bin'
assert '/etc/hosts' == [pf search 'hosts']
assert [pf search 'ls']
assert undefined == [pf search 'no-such-file-we-hope']
# Replace search path:
set pf[path] array 'unit' '.'
# Configure file suffixes/extensions:
pf add-ext '.whcl'
assert [pf search "000-000-0empty.whcl"]
# ^^^ a local test script
assert undefined == [pf search 'no-such-script']
```

<a id="type-pathfinder-members"></a>
# PathFinder Members

## Constructor

```whcl
new whcl[PathFinder] [array|string dirList [array|string fileExtensions]]
```

The two optional arguments are used to initialize the `path`
resp. `extensions` properties of this object. If they are arrays, they are
assigned as-is to the corresponding property. If they are strings,
they are parsed as conventional PATH-style strings, as documented for
the `tokenize-path` class-level method, and the resulting arrays are
assigned to the corresponding property.

It throws if passed arguments of any other type except, as a special case,
that it silently ignores `undefined` and `null` values (to simplify certain
usage patterns, e.g. `new whcl[PathFinder] [getenv "blah"]`).

## Class-Level Members

The following functions may be called from the prototype or from any
instance of the class. The prototype is accessible via
`whcl[PathFinder][__prototype]`.


### is-accessible

Usage: `is-accessible [-w] filename`

Returns `true` if the given filename refers to a readable file (or
writeable if passed the `-w` flag), else returns `false`. This
function does *no* path/extension-based lookup of the filename and
does not require a specific `this` type, thus it may be called on
the prototype or copied for use outside the context of a PathFinder
instance.

### path-from-env

Usage: `path-from-env cliFlagName [ENV_VAR_NAME=$cliFlagName]`

This function is a proxy for `tokenize-path` (see below) which gets
its path string from either [a CLI flag](whclsh.md#whcl-argv) or an
environment variable (in that order, stopping if it finds a match in
its first argument). Its first argument is the name of a flag which is
presumed to _possibly_ be set in `whcl[ARGV][flags]`, minus any
leading dashes. If such a flag is set, it is used as the source string
for a call to `tokenize-path`. If no such flag is set then it looks in
`getenv(argv[1])`, with `argv[1]` defaulting to `argv[0]`.  If such an
environment variable is found, that string is used for the call to
`tokenize-path`. If no match is found, `undefined` is returned, else
the result of `tokenize-path` is returned.

This function may be called directly from the prototype (or copied to
a new location for convenient reuse elsewhere) or from any PathFinder
instance.

Examples:

```whcl
decl P whcl[PathFinder][__prototype]
P path-from-env a-flag-name; # looks in ARGV[flags][a-flag-name] then getenv("a-flag-name")
P path-from-env flagx PATH; # looks in ARGV[flags][flagx] then getenv("PATH")
```


### tokenize-path

Usage: `tokenize-path pathString [targetArray]`


Tokenizes a conventional PATH-style string, delimited by path
separator characters. On Windows builds, only `;` is considered a
valid delimiter, but on non-Windows builds both `;` and `:` characters
are considered delimiters. It returns the entries (if any) as an
array. If passed an array as its second argument, the entries are
appended to that array and that array gets returned. Empty entries
(leading, trailing, and/or consecutive delimiters) are elided. Throws
if passed invalid arguments, noting that an empty path string, or one
with no path elements, is not an error.

C-level clients may bind their own copy of this function using
`whcl_cb_tokenize_path()`. The non-script-side APIs for this
feature are the `whcl_path_toker` type and its methods (search
[`whcl.h`](/finfo/include/wh/cwal/whcl/whcl.h) for that type name).


<a id="type-pathfinder-instance-methods"></a>
## Instance Methods

The following methods require a PathFinder instance...

### add-dir

Usage: `add-dir dirName [... dirNameN]`


Adds the given directories to this instance's search list, creating
the `path` property (an array), if needed, to hold them.
Alternately, the `path` property may be manipulated directly from
script code. Returns this object.

### add-ext

Usage: `add-ext fileExtension [... fileExtensionN]`


Adds the given file extension(s) to this instance's search list,
creating the `extensions` property (an array), if needed, to hold them.
Alternately, the `extensions` property may be manipulated directly from
script code. Returns this object.

Note that the extension is really a filename suffix and must include
any characters which the to-be-searched-for files might end with,
including the conventional `.` before a file extension. e.g. an
"extension" of `_xyz` will match a filename which ends with `_xyz`, even
though that's not normally what is thought of as a file extension.


### search

Usage: `search baseName [bool|int directoryPolicy=0]`


If the PathFinder can resolve the given base filename, based on its
search path and extensions lists (its `path` resp. `extensions`
properties), it returns the resolved name, otherwise it returns the
`undefined` value. It only checks that the file is readable. The
optional second argument specifies how to handle directory names: a
boolean `true` or a positive integer value means to match both
directories and files; a boolean `false` or integer 0 means never to
match directory names; a negative integer means to only match
directory names, never files. (BUG: on Non-Unix platforms it cannot
determine if a file is a directory.)

The directoryPolicy values are stored as constant values of the
PathFinder prototype: `SEARCH_ANY`, `SEARCH_FILES`, `SEARCH_DIRS`.

Even if the search path is empty, the first argument is still checked
as-is and (if needed) in combination with the configured extensions,
so it may match a filesytem entry in any directory. For example:

```whcl
decl p new whcl[PathFinder]; # w/ empty path/extensions
p search '/etc/hosts'; # will find a match on most Unix-like systems.
# Likewise:
p add-ext 'osts' p.SEARCH_FILES
p search '/etc/h'; # == "/etc/hosts"
```

### to-jsonable

Usage: `to-jsonable`

Overridden to return a plain object with this object's core state:

```json
{ "path": ["..."], "extensions": ["..."] }
```

See [the JSON API](api-json.md#json-to-jsonable) for more details.


<a id="type-pathfinder-configure"></a>
## Configuring Search Paths

The list of directories and file extensions can be manipulated via the
array-type `path` resp. `extensions` properties or via helper methods like
`add-dir` or `add-ext`. The client may either directly assign an array
to them, and manipulate that array directly, or may use the `add-dir`
resp.  `add-ext` methods to append to them (those functions create
them if needed). The `search` method looks up the `path` and `ext`
properties, so any changes made to them will be seen by it.

