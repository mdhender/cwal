# s2: The pragma Keyword
#### ([&#x2b11;Table of Contents](./))

Jump to...

* [Intro](#pragma-intro)
* [Pragma List](#pragma-list)
* [Pragma Quirks and Notes](#pragma-notes)

<a id="pragma-intro"></a>
# The `pragma` Keyword

The `pragma` function-like keyword was introduced on 20191230 to allow
querying and tweaking of certain s2-level state. Pragmas are not
generally features intended to be used by everyday scripts, and any
given pragma may be removed, or its semantics changed, at any
time. Some of them are intended *only* for testing s2.

It has several different usages, depending on the exact pragma being
invoked:

```s2
pragma(tag); // a "getter", fetches some state
pragma(tag EXPR); // typically a "setter", sets some state and may return something
pragma(tag IDENTIFIER);
```

The `tag` part of a pragma is typically an "extended identifier," that is
an identifier which may include `-` characters.

> Sidebar: the support for `-` in identifiers is a tokenizer feature
added to s2 for the sake of this keyword, but it now seems likely that
[the `typeinfo()` keyword](keyword-typeinfo.md) may eventually be
retro-fitted with these as well, and we could (with literally only 2
lines of code) enable it for keys in object/hash literals as well (but
probably won't).

<a id="pragma-list"></a>
# Pragma List

The list of pragmas follows, ordered by their "tag"...


<a id="pragma-build-opt"></a>
```s2-member
pragma(build-opt IDENTIFIER)
```

This pragma evaluates to the value of a C-level compile-time option
identified by the given identifier, typically a C preprocessor symbol
name. It is primarily intended for s2's own testing, not
general-purpose client code. This pragma throws if passed an unknown
identifier.

The supported (case-sensitive) identifiers, in alphabetical order,
are:

- `CWAL_CFLAGS`: string
- `CWAL_CPPFLAGS`: string
- `CWAL_OBASE_ISA_HASH`: integer 0 or 1
- `CWAL_SIZE_T_BITS`: integer 32 or 64
- `CWAL_VERSION_STRING`: string
- `DEBUG`: `true` for debug builds, else `false`
- `S2_AMALGAMATION_BUILD`: `true` if defined, `undefined` if not
- `S2_OS_UNIX`: `true` if defined, `undefined` if not
- `S2_OS_WINDOWS`: `true` if defined, `undefined` if not


<a id="pragma-exception-stacktrace"></a>
```s2-member
pragma(exception-stacktrace)
pragma(exception-stacktrace bool)
```

The first form evaluates to `true` or `false`, indicating whether s2
exceptions collect stack traces or not (by default they do). The
second form enables or disables exception stacktrace and evaluates to
the previous value of that setting.

The purpose of this flag is mainly performance: when s2 is used for
unit testing, exceptions are often intentionally thrown, but their
stacktraces are almost always ignored. Setting this flag tells s2 not
to collect stacktraces for exceptions, which speeds up their
processing. It should only be used in scripts which (A) intentionally
throw many exceptions which are caught as part of testing and (B) are
not expecting any "unexpected" exceptions. Its specific intended
target is unit test scripts, not app-level code. Note that exceptions,
regardless of this flag, still contain line/column/file info, so
they're not *entirely* useless when this is disabled, they're just not
as informative.


<a id="pragma-rc"></a>
```s2-member
pragma(rc IDENTIFIER)
```

This pragma maps identifiers which refer to the standard
cwal-/s2-level result code values to script-space, noting that such
values are not guaranteed to be stable (which is why they're made
accessible via their symbolic names).

Example:

```
s2sh2> exception(pragma(rc CWAL_RC_IO), "oh no")
result: exception@0x1fd59e0[scope=#1 ref#=0] ==> {
  "script": "shell input",
  "line": 1,
  "message": "oh no",
  "column": 0,
  "code": 601  <<<<================ CWAL_RC_IO
}
```

The whole range of `CWAL_RC_xxx` and `S2_RC_xxx` values are supported,
but the only one with a stable published value is `CWAL_RC_OK`, which
is guaranteed to be 0 and all others are guaranteed to be non-0.


<a id="pragma-refcount"></a>
```s2-member
pragma(refcount EXPR)
```

This pragma, as of 20191230, replaces the
[`typeinfo(refcount)`](keyword-typeinfo.md) query.

It evaluates to the cwal-level reference count of the expression. This
value has *no intrinsic meaning* for a script, and a value of 0 *is*
(briefly) legal in many contexts (and is always 0 for built-in
constant values). i.e. it is FYI only, not intended to be used to
implement any logic. Scripts *cannot* make any informed decisions
based on this value. It is provided mainly for the sake of testing s2,
interest, and completeness. Note that evaluation of this keyword may
hold a reference (or more) at the time its operand returns, so the
reported refcount may *appear* higher than expected. Rest assured,
unless someone has misbehaved at the C level, refcounts will normalize
after the current expression has finished evaluating.


```s2-member
pragma(sweep-interval)
pragma(sweep-interval integer)
```

Gets or sets the sweep interval, measured in complete expressions. A
value of 0 disables sweeping altogether and 1 is extremely
aggressive. A value of 1 is *highly recommended* when testing new
code, as it helps uncover lifetime-related misuse of values early on,
but such low values are not recommended for general-purpose use if
performance is of any concern (it doesn't hurt anything except
performance).

The setter form evaluates to the previous value of the setting.

Hashtag `s2_engine::sweepInterval`, hashtag `s2sh -w`.

```s2-member
pragma(trace-assert)
pragma(trace-assert integer)
```

Gets or sets the `assert`/`affirm` tracing level, an integer value of
0 or higher. A value of 0 (the default) disables tracing. A value of
1+ enables tracing of successful `assert`ions (failed assertions
trigger a fatal error, as usual). A value of 2+ also traces successful
`affirm`ations. The [s2sh](s2sh.md) equivalent is `-A` for a value of
1 and `-A -A` for a value of 2.

The setter form evaluates to the previous value of the setting.

This particular type of tracing goes to s2's configured output
channel, not directly to `stdout`.


```s2-member
pragma(trace-sweep)
pragma(trace-sweep integer)
```

Gets or sets the sweep-trace level, an integer value of 0 or higher.
Note that sweep logging goes to the *real* `stdout`, not s2's
configured output channel.

A value of 0 disables tracing. 1+ enables it when something is swept
up. 2+ enables script location information (which is normally elided
because it's slow to calculate). 3+ enables it even when nothing was
swept up (so it's possible to see where sweeping is being performed).

The setter form evaluates to the previous value of the setting.


```s2-member
pragma(trace-token-stack)
pragma(trace-token-stack integer)
```

Gets or sets the stack-machine tracing level, an integer value of 0 or
higher. This tracing goes to the *real* `stdout`, not s2's configured
output channel, and produces *huge* amounts of output. As of this
writing, there are only 2 levels of tracing: off and on.

The setter form evaluates to the previous value of the setting.


```s2-member
pragma(vacuum-interval)
pragma(vacuum-interval integer)
```

The vacuum counterpart of `sweep-interval`. Gets or sets the vacuum
interval, measured in "sweep increments." Every Nth sweep will be
replaced by a vacuum. An integer value of 0 or higher, with 0 disabling
vacuuming altogether.

The setter form evaluates to the previous value of the setting.

Hashtag `s2_engine::vacuumInterval`, hashtag `s2sh +w`.


<a id="pragma-notes"></a>
# Pragma Quirks and Notes

Quirks:

- `pragma()`, like `typeinfo()`, does not currently push a separate
  scope in which to evaluate its operands, but *that behaviour may
  change* in the future. Do not rely it *not* pushing a scope.


Potential TODO pragmas:

- Modify `exception-stacktrace` to be an integer value representing a
  maximum depth to trace, with 0 being unlimited and `false` disabling
  it altogether.
- Get/set `s2_engine::strace::max` (maximum script-side function call
  stack depth (as opposed to scope depth)).
- Fetch various s2/cwal metrics.
- Trigger a sweep and/or vacuum, noting that a vacuum trigger is often
  fatal when run at the "wrong time."  It may also be interesting to
  sweep/vacuum a scope other than the current one this way, solely for
  testing purposes, so that we can potentially someday get
  upward-recursive vacuuming working reliably (there are valid reasons
  why it cannot work in the general case, but if we're *really careful*
  at *all* of the C level then it "can be made to work").


Potentially interesting pragmas which we can't reasonably implement:

- Modifying s2's memory-capping state. Most of that must be
  initialized before the first allocation and may not legally change.
- Toggling s2 between hashes and objects for scope-level
  storage. *Hypothetically* that's safe to do at runtime, but would
  only apply to future creation of scope-level property storage and it
  very probably can't lead to any good.
