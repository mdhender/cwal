# s2: PathFinder
#### ([&#x2b11;Table of Contents](./))
# The PathFinder Class

Jump to...

* [PathFinder](#type-pathfinder)
* [PathFinder Members](#type-pathfinder-members)
* PathFinder Members
    * [Class-level Members](#type-pathfinder-members)
    * [Instance Members](#type-pathfinder-instance-members)
    * [Configuring Search Paths](#type-pathfinder-configure)

<a id="type-pathfinder"></a>
# PathFinder

PathFinder is a utility class for finding files based on
client-provided search paths and file suffixes. Its primary use is for
finding library-level scripts and [loadable modules](../mod/) based on
a client-configurable search paths and possibly platform-specific file
extensions (e.g. ".dll" vs ".so"). This class is implemented as a
"native" type binding, and demonstrates the plugging-in of an
arbitrary C-side struct to the scripting engine. Only a very small
amount of its code depends on s2, per se - it primarily depends on the
engine (cwal) and not the language interpreter (s2). The underlying C
class itself has been wandering from source tree to source tree since
the early 2000's (though it was initially implemented in C++).

PathFinder is used like this:

```s2
const pf = new s2.PathFinder();
assert 'PathFinder' === typeinfo(name pf);
assert pf inherits s2.PathFinder;
pf.prefix = ['/etc', '/bin', '/usr/bin']; // search paths
assert '/etc/hosts' === pf.search('hosts');
assert pf.search('ls').indexOf('/ls') > 0;
assert undefined === pf.search('no-such-file-we-hope');
pf.prefix = ['unit','.']; // search paths
pf.suffix = ['.s2']; // file extensions
assert pf.search('000-000-0empty'); // a local test script
assert undefined === pf.search('no-such-script');
assert typeinfo(isstring pf.search(__FILE));
```


<a id="type-pathfinder-members"></a>
# PathFinder Members

## Class-Level Members

The following functions may be called from the prototype or from any
instance of the class (created via the class' `new()` method)...

```s2-member-deprecated
PathFinder PathFinder.new([array dirList [, array fileExtensions]])
```
Deprecated: prefer `new s2.PathFinder()` instead.

```s2-member
new PathFinder([array|string dirList [, array|string fileExtensions]])
```

Both of those create and return a new PathFinder instance. The two optional
arguments are used to initialize the `prefix` resp. `suffix`
properties of this object. If they are arrays, they are assigned as-is to
the corresponding property. If they are strings, they are parsed as conventional
PATH-style strings, as documented for the `tokenizePath()` class-level method,
and the resulting arrays are assigned to the corresponding property.

It throws if passed arguments of any other type except, as a special case,
that it silently ignores `undefined` and `null` values (to simplify certain
usage patterns, e.g. `new s2.PathFinder(s2.getenv("blah"))`).

The constructor-style usage is preferred over the `new()` factory
function, but the latter predates the former by a couple years.


```s2-member
bool fileIsAccessible( string filename [, bool checkIsWriteable=false] )
```

Returns `true` if the given filename refers to a readable file (or
writeable, if the second argument is true), else returns `false`. This
function does *no* path/extension-based lookup of the filename and
does not require a specific `this` type, thus it may be called on
the prototype or copied for use outside the context of a PathFinder
instance.

```s2-member
string separator
```

This property contains the platform's directory separator string,
namely `/` on Unix and `\` on Windows.


```s2-member
array tokenizePath(string path [, array target])
```

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
`s2_cb_tokenize_path()`. The non-script-side APIs for this
feature are the `s2_path_toker` type and its methods (search
[`t10n.h`](/finfo/s2/t10n.h) for that type name).

Here's a convenient utility function based on this function
which is not a core s2 feature only because it relies on
having various APIs installed which any given s2 client may
or may not have installed (but [`s2sh`](s2sh.md) does unless it's
running in "cleanroom" mode):

```s2
/**
   If a string-type CLI flag OR environment variable (in that
   order) with the given name is found, this function returns an
   array of its contents, tokenized using
   s2.PathFinder.tokenizePath(). If no flag/environment variable
   is found, or a CLI flag with a non-string value is found,
   undefined is returned.

   Interpretation of the arguments is as follows:

   - If only 1 argument is provided, this routine looks for both a
   CLI flag and environment var (in that order) with that name.

   - If 2 arguments are provided, the 1st is checked as a CLI flag
   and the 2nd as an environment variable (in that order).
*/
const pathFromEnv = proc(f,e=f){
    const p = F[f] ||| E(e);
    return typeinfo(isstring p)
        ? P.tokenizePath(p)
        : undefined;
} using {
    E: s2.getenv,
    P: s2.PathFinder,
    F: s2.ARGV.flags
};
```

And it's used like:

```s2
const a = pathFromEnv('S2_REQUIRE_PATH'); // env var or CLI flag
const b = pathFromEnv('some-cli-flag','MY_ENV_VAR');
```

<a id="type-pathfinder-instance-methods"></a>
## Instance Methods

The following methods require a PathFinder instance...

```s2-member
PathFinder addDir(string dirName [,... dirNameN])
```

Adds the given directories to this instance's search list, creating
the `prefix` property (an array), if needed, to hold them.
Alternately, the `prefix` property may be manipulated directly from
script code.

```s2-member
PathFinder addExt(string fileExtension [,... fileExtensionN])
```
Adds the given file extension(s) to this instance's search list,
creating the `suffix` property (an array), if needed, to hold them.
Alternately, the `suffix` property may be manipulated directly from
script code.


```s2-member
string|undefined search(string baseName [, bool|int directoryPolicy=0])
```

If the PathFinder can resolve the given base filename, based on its
search path and extensions lists (its `prefix` resp. `suffix`
properties), it returns the resolved name, otherwise it returns the
undefined value. It only checks that the file is readable. The
optional second argument specifies how to handle directory names: a
boolean `true` or a positive integer value means to match both
directories and files; a boolean `false` or integer 0 means never to
match directory names; a negative integer means to only match
directory names, never files. (BUG: on Non-Unix platforms it cannot
determine if a file is a directory.)

If the 2nd argument is of type `unique` (i.e. it's an `enum` entry)
then its wrapped value is used in its place. (Potential TODO: add an
enum which represents all legal values for the 2nd parameter.)

Even if the search path is empty, the first argument is still checked
as-is and (if needed) in combination with the configured extensions,
so it may match a filesytem entry in any directory. For example:

```s2
const p = new s2.PathFinder(); // empty path/extensions
assert 0 === p.prefix.#; // just to demonstrate that it's empty.
p.search('/etc/hosts'); // will find a match on most Unix-like systems.
// Likewise:
p.addExt('osts');
p.search('/etc/h'); // === "/etc/hosts"
```


<a id="type-pathfinder-configure"></a>
## Configuring Search Paths

The list of directories and file extensions can be manipulated via the
array-type `prefix` resp. `suffix` properties or via helper methods
like `addDir()` or `addExt()`. The client may either directly assign
an array to them, and manipulate that array directly, or may use the
`addDir()` resp.  `addExt()` methods to append to them (those
functions create them if needed). The `search()` method looks up the
`prefix` and `suffix` properties, so any changes made to them will be
seen by it.

Note that a new `PathFinder` instance created with no constructor
arguments has neither a `prefix` nor `suffix` property: they must be
manually added or implicitly added via `addDir()` resp. `addExt()`.

> Sidebar: the names `prefix` and `suffix` are historical, and arguably a
poor choice (i was going for abstract and kind of went overboard). A
considerable amount of script code already uses these names, though,
so changing them, e.g. to `path` and `extensions`, isn't trivial.

