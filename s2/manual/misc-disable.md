# s2: Disabling Filesystem Access
#### ([&#x2b11;Table of Contents](./)) ([&#x2b11;Misc. Features Index](misc-index.md))
# Disabling Filesystem Access

s2's C API supports a set of *advisory* flags for marking certain
types of features as disabled.  These are intended for disabling
certain script-side (not C-side) features, namely any combination of:

- `stat()`ing files and directories. This also limits directory-level operations like `chdir()`
  and `getcwd()`.
- Opening files for read-only access.
- Opening files for write-mode access, which also limits creating new files.

Currently only filesystem-level features are restricted, but others
may be added in the future, e.g. to disable loading loadable modules
(which are currently covered by the no-read flag unless they are
*statically* built into [s2sh](s2sh.md) or [s2sh2](s2sh2.md)). As of
this writing, none of [the loadable modules ](../mod/) account for these flags -
they instead expect the module-loading process to block loading of the
module if filesystem access would be problematic. That will likely
change at some point, but exactly how it should change is still up for
consideration.  ([The modules](../mod/) which work with files,
e.g. [sqlite3](../mod/sqlite3/), would be pretty useless if they
couldn't do so.)

Note that `stat()`ing files is a separate permission from reading
them, intended to block checks of "does this file exist?" while still
allowing direct opening of a file if the caller knows the name. The
border between those two features is somewhat fuzzy, and most, but not
all, operations which block reading also block `stat()`.


Participating APIs check these flags and trigger an error instead of
doing whatever it is they normally do. These flags are *advisory*,
applied only to APIs which *explicitly check them*. They are not (and
cannot realistically be) enforced at a lower level of the API.

There is currently no script-side API for applying these flags (and
being able to *disable* them from script space would be
counter-productive). The relevant C APIs, all documented in
[`/s2/s2.h`](/finfo/s2/s2.h), include:

- The `s2_disabled_features` enum.
- `s2_disable_set()`
- `s2_disable_set_cstr()`

The `--s2-disable=...` flag for [s2sh](s2sh.md) and [s2sh2](s2sh2.md) can
be used to apply these limitations to scripts run via those tools.
(See their double-verbose-mode help text for more information.)

Potential TODOs:

- Allow reading of JSON files if the disable-read flag is set, so long
  as a hypothetical disallow-reading-JSON-files flag is *not* also
  set. (Noting that the flags are explicitely feature disablers, not
  feature enablers.) Justification: JSON parsing is known to not
  execute foreign code and fails for any input which is not strictly
  JSON. JSON makes a convenient format for app-level configuration, so
  an exception for reading such files might make sense.
- Similarly, reading/writing files of specific well-known formats,
  like [the sqlite3 module's databases](../mod/sqlite3/), might be
  covered/coverable by such a flag. As it is, however, all such APIs
  need to do in order to "bypass" these flags is *nothing at all*.
