# s2: Filesystem API
#### ([&#x2b11;Table of Contents](./)) ([&#x2b11;Misc. Features Index](misc-index.md))
<a id="misc-fs"></a>
# s2 Filesystem API

This API gets installed by [s2sh](s2sh.md) as `s2.fs`. Client
applications are free to install it (or not) under a different
name.

This object contains functions for working with the local
filesystem, all of which may or may not function on any non-Unix-like
platform. None of its members require that this object be their `this`,
so they can be copied for use in other contexts:

```s2-member
void chdir( string directoryName )
```

Changes the current working directory to the given one or throws on
error.

```s2-member
bool dirIsAccessible( string dirname [, bool checkIsWriteable=false] )
```

Returns `true` if the given name refers to a readable directory (or
writeable, if the second argument is `true`), else returns `false`. This
function does *no* path/extension-based lookup of the name.

```s2-member
bool fileIsAccessible( string filename [, bool checkIsWriteable=false] )
```

Returns `true` if the given filename refers to a readable file (or
writeable, if the second argument is `true`), else returns `false`. This
function does *no* path/extension-based lookup of the filename.

```s2-member
string getcwd( [appendSlash=false] )
```

Returns the name of the current working directly. If passed a truthy
value it appends the directory separator character to the result, else
it does not. Throws an exception on Windows because i don't have a
machine to test the required character set conversions on.

```s2-member
string mkdir( string dirname [, bool makeParentDirs=false] [, int unixPermissions = 0o750] )
```

Works like the C-level `mkdir(2)`, except that it's a no-op if the
target directory exists. If it's passed `true` for its second argument
(a real boolean, not an arbitrary truthy value) then it creates all
directories leading up to the target directory, if needed. Throws on
error. Note that the Unix-style permission mode passed to it may be
modified by the OS, e.g. to apply the `umask`.


```s2-member
mixed passthrough(string filename)
```

Given a filename, this streams the contents of the file to cwal's
current standard output mechanism. If output buffering is enabled, the
output will go there. Throws on error. Returns the calls "this"
value. It accepts the string "-" to mean stdin.

```s2-member
mixed realpath(string)
```

Works like the C-level `realpath(3)`, returning the resolved/absolute
path of the given filename. Returns `undefined` if the given file does
not exist. Throws for any `realpath(3)` error other than `ENOENT`
(file not found) or if `realpath(3)` is not enabled in this build (it
requires XOpen-specific APIs). Notes:

- `realpath(3)` errors out (with `ENOTDIR`) on
   `/path/with/trailing/slash/` if the final path component is a file,
   rather than a directory. That will cause this routine to
   throw. `realpath(3)`'s behaviour here is arguably a bug, but
   there's no sensible way for us to work around it.

- *Achtung*: `realpath(3)` does not do tilde expansion, so a path like
  `~/bin` will not resolve. The only way to resolve such a path is to
  first `chdir()` to it, then `getcwd()`, then `chdir()`
  back. Potential TODO: reimplement this `realpath()` wrapper in terms
  of that operation.

```s2-member
object stat( string filename [, derefSymlink=true] )
bool stat( string filename, undefined [, derefSymlink=true] )
```

The first form calls `stat(2)` on the given filename and returns
an object which describes the `stat(2)` result. It throws an
exception if the file is not stat()'able. If `derefSymlink` is
falsy and the target file is a symlink, then information about
the symlink itself is returned, rather than resolving any
symlink(s). (Passing `false` requires that the `lstat(2)` system
call API be available.) The result object has this structure:

```s2
{
  ctime: integer, // state change time (includes perms changes)
  mtime: integer, // content modification time
  perm: integer,  // Unix permissions bits
  size: integer,  // size, in bytes
  type: string
     // ^^^ "unknown", "file", "dir", "link",
     // ^^^ "block", "char", "fifo", "socket"
}
```

Using the second form, if passed more than 1 argument and the 2nd
argument has the `undefined` value then it returns a boolean
indicating whether calling `stat()` on the target succeeded or not. This
variant does not throw if `stat(2)` fails. (Yes, this calling convention
is a bit unusual, but nothing better currently comes to mind.)

This function throws if the current platform's build does not include
the necessary features (e.g. if `lstat(2)` is missing and `derefSymlink`
is falsy). It always throws on Windows.

