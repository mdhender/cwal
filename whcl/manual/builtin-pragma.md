# whcl: Pragma Builtin Command
##### ([&#x2b11;Table of Contents](./)) ([builtins](builtins.md))
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

# Pragmas

The `pragma` builtin command is for performing "meta-operations" of
various sorts. None of them are to be considered official parts of the
API. They are primarily intended for performing low-level operations
related to inspecting and testing the interpreter. The pragmas are
subject to change at any time and must not be relied upon in any
script code unless specifically noted otherwise.

Usage: `pragma ...args`

The list of pragmas, in alphabetical order...


__debug
------------------------------------------------------------

If passed no arguments, this evaluates to the current boolean value of
the whcl engine's `__debug {...}` feature. Any argument is treated as
a bool to toggle the feature on or off and the setting's new value is
returned.

When this feature is off `__debug {...}` calls are skipped over, else
they are evaluated similarly to `eval -scope {...}` except that they
always result in the `undefined` value (they do not propagate their
final expression).

dump-tokens
------------------------------------------------------------

Usage: `dump-tokens [-v] [-eof] [-m] [-f FILENAME | tokens...]`

Dumps, in some debug-like form, the tokens of the current script, the
given file, or the given tokens. This is useful for visualizing how
whcl sees the tokens. The `-v` (verbose) flag adds the content of each
token to the output, noting that supertokens (block constructs) can be
arbitrarily large. The `-eof` flag causes the virtual EOF tokens to be
included in the output (most scripts have many of these). The `-m`
flag includes some metrics at the end of the dump.

See also: [the `dump` method of the Script
class](type-script.md#script-dump)

memory-metrics
------------------------------------------------------------
  
Usage: `memory-metrics`

Dumps many details about cwal's current memory usage.


trace-assert
------------------------------------------------------------
  
Usage: `trace-assert [tracingBitmask]`

If passed no arguments, this returns a bitmask of flags: 0x01
indicates that `assert` tracing is enabled and 0x02 indicates that
`affirm` tracing is enabled. Any argument is treated as an integer
with the same semantics. A negative value, or any which evaluates to 0
when masked against 0x01 resp. 0x02, disables it. It returns the new
tracing bitmask.

Assertion tracing causes the full line of all `assert` calls to be
output via the engine's standard output channel. Affirm tracing does
the same for `affirm` calls.

vacuum
------------------------------------------------------------
  
Usage: `vacuum [-r = recursive]`

Forces a vacuum of the underlying cwal engine. If unmanaged cwal
values are held improperly by C-level code, this can (should) crash
the engine in debug builds by triggering various `assert()`s. A crash
is a good thing, in that it reveals memory misuse, but it's bad
because it can be extremely difficult to find the mismanaged cwal
value(s).


***NEVER USE THE -r FLAG.*** It exists solely for my own use in
testing the underlying C code, often with disastrous results.

Though a non-recursive vacuum is believed to be safe ***do not attempt
to run a recursive vacuum in well-loved script code***.  A recursive
vacuum is known to sometimes cause Grief, in particular in conjunction
with string interning (a feature i'm sorely tempted to remove from
cwal altogether). What's _not_ clear is whether recursively vacuuming
is _inherently_ a broken algorithm or whether the aforementioned Grief
is caused by mismanagement of value lifetimes at the C level.


### Sidebar about Vacuuming

Vacuuming is a garbage-collection measure which can weed out values
which have no script-visible references but have a positive refcount
caused by recursive relationships. whcl cleans up values automatically
when their refcounts reach zero but cycles caused via storing
properties in containers can keep refcounts from ever reaching zero,
as in this example:

```whcl
decl obj object
set $obj[o] $obj
unset $obj
```

That object is now a "ghost," with no script-accessible handles but a
cyclic reference to itself. It will hang around until either its
owning scope is popped or it is vacuumed up. The "owning scope" is
initially the scope a value is created in but values can be moved into
older scopes via result propagation and being referenced by container
values which are themselves owned by older scopes. That level of
ownership is an internal lifetime-management artifact, independent of
any script-visible variable references to those values. See
[](MemoryModel) for more details of cwal's lifetime management.

That said: it is fairly common to have values in C code which are not
"script-visible," and a vacuum can destroy those, pulling them out
from under the C code which expects them to be there. In C code, such
values must, for safe operation of the scripting engine, always be
made "vacuum-proof" using one of the approaches provided by the
underlying cwal library. whcl's internals invariably do so when they
hold values which are not script-visible.
