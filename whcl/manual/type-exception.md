# whcl: exception
##### ([&#x2b11;Table of Contents](./)) ([data types](type-intro.md))
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

Exceptions
============================================================

Like most not-entirely-low-level languages, whcl offers support for
so-called "exceptions" for reporting errors from script
code. Exceptions change the flow of a script, propagating an error up
until the next instance of the `catch` builtin command, which stops
propagation of the exception. If no `catch` is in place, the
"uncaught" exceptions bubbles up out of the script and is passed to
the C-level caller, who may deal with it as they please.

An exception contains information about _where_ it was thrown and the
call stack leading up to that point, making it easy to track the
location of an exception.

Exceptions in whcl are relatively limited, namely in that a `catch`
statement cannot distinguish between different types of exceptions.
For the vast majority of cases, all exceptions are equal in
whcl's eyes.

Using Exceptions
============================================================

There are several distinct ways to use
exceptions:

- The builtin `exception` and the `whcl[Exception]` are
  two functionally equivalent ways of creating exceptions. The
  latter is provided primarily so that its prototype may be
  extended by clients.
- The `throw` builtin command will create a new exception if the value
  given to it is not already one. If given an exception object, it
  re-throws it as-is without modification.

An exception by itself does not trigger an error. The triggering
happens via the `throw` builtin (or equivalent C-level code).

Exceptions have three significant key/value pairs:

- `code` is an integer code associated with the error. The
  whcl library always uses codes from the `cwal_rc_e` enum
  but client code is not required to.
- `message` is an arbitrary value but typically a string.
- `stackTrace` gets injected by the interpreter when an exception is
  created so that the developer may trace which commands leds up to
  the throwing of an exception.

As alluded to above, "activating" an exceptional error case requires
the `throw` keyword or equivalent C code. The most common usage is
simply:

```whcl
throw exception "This is my message!"
# Equivalent to:
throw new whcl[Exception] "This is my message!"
```

If passed a single argument, the `code` defaults to
`CWAL_RC_EXCEPTION`, but it may optionally be passed as the first of
two arguments:

```whcl
throw exception RANGE "Number out of range."
# Equivalent to:
throw new whcl[Exception] RANGE "Number out of range."
```

The name `RANGE` there is the suffix of a `cwal_rc_e` enum code (the
One True set of result codes used by the library). The exception
constructor accepts three different forms of `code` value:

- An arbitrary non-0 integer.
- A string in the form `CWAL_RC_...` which matches the name of
  a `cwal_rc_e` enum entry.
- A string in the form `XXX` where `XXX` matches the `XXX` part of a
  `CWAL_RC_XXX` enum entry. e.g. `CWAL_RC_RANGE` and `RANGE` are
  equivalent.

If passed a value of 0, or a value which is not in one of the above
forms, the value `CWAL_RC_EXCEPTION` is used. Note, however, that none
of the values from the `cwal_rc_e` have well-defined values except for
`CWAL_RC_OK`, which is always 0. All others are guaranteed only to be
non-0.

The code-string Method
------------------------------------------------------------

The exception prototype has a method called `code-string` which has
the following distinct uses:

- If passed no argument, it returns the string form of the exception's
  `code` value, if it can find one, in the form `"CWAL_RC_xxx"`
  (without the quotes). It returns `undefined` if the code does not
  match any known C-level `cwal_rc_e` value.
- If passed an integer argument which lies in the `cwal_rc_e` range,
  it return the name of that code, as described above, or `undefined`
  if the integer is out of that range.
- If passed a string argument, it expects it to be in the form
  `CWAL_RC_...` or `XXX`, where `XXX` is the suffix of a `CWAL_RC_XXX`
  entry. If a match is found, it returns the integer code (noting the
  caveat above about the instability of those codes), else it returns
  `undefined`.

This method primarily exists to facilitate testing of C-level code
which uses the cwal result code range (e.g. whcl itself). For example:

```whcl
catch x { ... }
assert CWAL_RC_RANGE == [x code-string]
```

Or, more succinctly:

```whcl
assert CWAL_RC_RANGE == [[catch {...}].code-string]
```
