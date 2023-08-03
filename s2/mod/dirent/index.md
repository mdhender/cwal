# s2 dirent
#### ([&#x2b11;Main Module Docs](../))
# `dirent`: POSIX `opendir(3)` Wrapper

- Source code: [](/dir/s2/mod/dirent?ci=trunk)
- Test/demo code: [](/finfo/s2/mod/dirent/mod_dirent.s2)


The `dirent` module wraps the POSIX `opendir(3)` family of functions,
and gets its name from the primary data type used by that API. This
module allows the user to "open" a directory and traverse its list of
files and subdirectories.

The API is described here in the form of an s2 object:

```s2
/**
   API docs for the dirent module.
*/
var definedInNativeCode = proc(){throw "don't call this!"};
const direntAPIDocs = {
    /**
       A string holding the system's directory separator:
       '/' on Unix and '\\' on Windows.
    */
    dirSeparator: definedInNativeCode,

    /**
       isDir(path[, ignoreErrors=false]) returns true if the given
       filename refers to a dir, else returns false.  By default it
       throws if it cannot stat() the given name, but it will ignore
       such errors (returning false instead) if the 2nd parameter is
       a truthy value.
    */
    isDir: definedInNativeCode,

    /**
       isFile(path[, bool ignoreErrors=false]) returns true if the
       given filename refers to a non-directory, else returns
       false. By default it throws if it cannot stat() the given name,
       but it will ignore such errors (returning false instead) if the
       2nd parameter is a truthy value.
    */
    isFile: definedInNativeCode,

    /**
       chdir(path) changes the app's current directory to the given
       path. Throws on error. Returns this object.
    */
    chdir: definedInNativeCode,

    /**
       cwd() returns the name of the current working directory.
    */
    cwd: definedInNativeCode,

    /**
       pushd(path) stores the current wording dir in the this.dirs
       array (stack) then changes the current wording dir to the given
       path. Throws on error. Returns its argument.
    */
    pushd: definedInNativeCode,

    /**
       popd() changes the current working dir to the top-most one
       in the this.dirs stack. Throws on error. Returns the name
       of the new directory.
    */
    popd: definedInNativeCode,

    /**
       openDir(path) opens the given directory and returns an object
       which has the API documented below. Throws on error.
     */
    openDir: {
        /**
           This directory's name, as it was passed to openDir().
        */
        name: definedInNativeCode,

        /**
           Closes the directory handle and frees all associated native
           resources. Returns void. This also happens when the dirent
           value is cleaned up by garbage collection.
        */
        close: definedInNativeCode,

        /**
           Rewinds the directory entry iterator. Returns this object.
        */
        rewind: definedInNativeCode,

        /**
           Iterates to the next dir entry and returns it as a string
           (the entry's name). Returns undefined at the end of
           iteration.
        */
        read: definedInNativeCode,

        /**
           isDir() works as described for isDir() in the containing
           module.
        */
        isDir: definedInNativeCode,

        /**
           isFile() works as described for isFile() in the containing
           module.
        */
        isFile: definedInNativeCode,

        /**
           eachEntry() iterates over this directory's entries and
           passes their names to a callback or appends them to an
           array.

           Usages:

           eachEntry(Function|Array [, integer mode=0 [, bool prependDirName=false]])

           eachEntry(Function|Array [, bool prependDirName=false])

           The mode values are available as inherited properties of
           the dirent interface:

           EachDir (<0): only iterate over directories.

           EachFile (>0): only iterate over files. Note that this is
           technically "non-directories", and may include "special
           files".

           EachAll (==0): iterate over files and dirs.

           The default is EachAll.

           If prependDirName is a truthy, the dirent's name and a
           directory separator character are prepended to each entry's
           name for purposes of passing them to their target. Whether
           or not this resolves to absolute paths depends on how the
           dirent was opened. e.g.  module.openDir('.') is different
           from module.openDir(module.cwd()) in that regard.
        */
        eachEntry:definedInNativeCode,
        /** Mode policy for eachEntry(). */
        EachDir: definedInNativeCode,
        /** Mode policy for eachEntry(). */
        EachFile: definedInNativeCode,
        /** Mode policy for eachEntry(). */
        EachAll: definedInNativeCode
    }
};
unset definedInNativeCode;
```
