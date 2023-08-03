# whcl: Filesystem APIs
##### ([&#x2b11;Table of Contents](./)) ([&#x2b11;API Index](api-index.md))
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

<a id="api-fs"></a>
whcl Filesystem API
============================================================

This API contains functions for working with the local filesystem, all
of which may or may not function on any non-Unix-like platform. None
of its members require that this object be their `this`, so they can
be copied for use in other contexts.

This API may be installed using:

`whcl install-api fs`

or calling `whcl_install_fs()` from C code.

It adds a `whcl[fs]` object with the API described below.

<a id='fs-method-chdir'></a>
chdir
------------------------------------------------------------

Usage: `chdir directoryName`

Changes the current working directory to the given one or throws on
error. Returns `undefined`.

<a id='fs-method-dir-accessible'></a>
dir-accessible
------------------------------------------------------------

Usage: `dir-accessible [-w] dirname`

Returns `true` if the given name refers to a readable directory (or
writeable if passed the `-w` flag), else returns `false`. This
function does no normalization or transformation of the name it is
given.

<a id='fs-method-file-accessible'></a>
file-accessible
------------------------------------------------------------

Usage: `file-accessible [-w] filename`

Returns `true` if the given filename refers to a readable file (or
writeable if passed the `-w` flag), else returns `false`. This
function does no normalization or transformation of the name it is
given.

<a id='fs-method-getcwd'></a>
getcwd
------------------------------------------------------------

Usage: `getcwd [-slash]`

Returns the name of the current working directly. If passed `-slash`
it appends the directory separator character to the result, else it
does not. Throws an exception on Windows because i don't have a
machine to test the required character set conversions on.

<a id='fs-method-mkdir'></a>
mkdir
------------------------------------------------------------

Usage: `mkdir [-p] dirname [unixPermissions = 0o750]`

Works like the C-level `mkdir(2)`, except that it's a no-op if the
target directory exists. If it's passed `-p` for its first argument
then it creates all directories leading up to the target directory, if
needed. Throws on error. Note that the Unix-style permission mode
passed to it may be modified by the OS, e.g. to apply the
`umask`. Returns `undefined` and throws on error. Returns the call's
`this` value. It accepts the string "-" to mean stdin.


<a id='fs-method-passthrough'></a>
passthrough
------------------------------------------------------------

Usage: `passthrough filename`

Given a filename, this streams the contents of the file to cwal's
current standard output mechanism. If output buffering is enabled, the
output will go there. Throws on error. Returns the call's `this`
value. It accepts the string "-" to mean stdin.

<a id='fs-method-popd'></a>
popd
------------------------------------------------------------

Usage: `popd`

Pops the current directory from [the `pushd` stack](#fs-method-pushd)
and returns the directory name it changes back to. Throws if the dir
stack is empty or changing dirs fails. Note that if changing
directories fails, the stack is still modified.

<a id='fs-method-pushd'></a>
pushd
------------------------------------------------------------

Usage: `pushd dirName`

Adds the current directory to a stack of directory names (stored as
the `dir-stack` const property of the `whcl[fs]` object), changes
directories to the given one, and returns the name of the new
directory. If it cannot change directories, it throws and does not
modify the current directory stack.

It is legal to modify `whcl[fs][dir-stack]` from outside of this
method and `popd`. They will always reference that specific array
instance (noting that it's declared const, so it cannot be outright
replaced). If the directory stack becomes unusable because
directories in it no longer exist, it may be necessary to clear the
array by setting its length to 0.

Both this method and `popd` are implemented such that they may be
copied into other contexts and will still work, always referencing the
`whcl[fs][dir-stack]` array regardless of where they are copied to.

Trivia: `pushd` and `popd` were the first installed-from-C APIs in
whcl which were implemented as script code.


<a id='fs-method-realpath'></a>
realpath
------------------------------------------------------------

Usage: `realpath string`

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
  `~/bin` will not resolve. The portable way to resolve such a path is
  to first `chdir()` to it, then `getcwd()`, then `chdir()`
  back. Potential TODO: reimplement this `realpath()` wrapper in terms
  of that operation.

<a id='fs-method-stat'></a>
stat
------------------------------------------------------------

Usage: `stat [-full] [-link] filename`

Without the `-full` flag it returns a boolean indicating whether
calling `stat()` on the target succeeded or not. If the `-link` flag
is used and the target file is a symlink, then information about the
symlink itself is returned, rather than resolving any
symlink(s). (Passing `-link` requires that the `lstat(2)` system call
API be available.) This returns false if if `stat(2)` fails.

If passed the `-full` flag then on success it returns an object which
describes the `stat(2)` result and throws an exception if the file is
not stat()'able. The result object has this structure:

```whcl
{
  ctime integer # state change time (includes perms changes)
  mtime integer # content modification time
  perm integer  # Unix permissions bits
  size integer  # size, in bytes
  type string
     # ^^^ "unknown", "file", "dir", "link",
     # ^^^ "block", "char", "fifo", "socket"
}
```

This function throws if the current platform's build does not include
the necessary features (e.g. if `lstat(2)` is missing and `-link`
is used). It currently always throws on Windows.
