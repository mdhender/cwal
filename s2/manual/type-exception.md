# s2: Exceptions
#### ([&#x2b11;Table of Contents](./))
# The Exception Type

Jump to...

* [Exception](#type-exception)
* [Exception Methods](#type-exception-methods)
* [Custom Exception Types](#type-exception-custom)
    * [Inheriting from `exception`](#type-exception-custom-inherit)
    * [Using Singleton Error Objects](#type-exception-singleton)
    * [Where to Store Custom Exceptions?](#type-exception-custom-storage)

<a id="type-exception"></a>
# Exceptions

s2 uses exceptions to report non-fatal errors which scripts can
potentially recover from. When thrown, an exception will propagate up
the stack until it is "caught" (via the `catch` keyword) or the top
scope is reached, at which point the script exits and the C/C++-level
app might report the problem to the user. Exceptions are first-class
values created via the [`throw` or `exception()`
keywords](keywords.md). `throw` creates a new Exception object, sets
`throw`'s operand as the exception's `message` property, collects
source location information and a call stack trace, then puts the
interpreter into an error-propagation mode so that the exception (a
Value instance subject to normal lifetime rules) survives its ride
back up the call stack. The `exception()` keyword creates an
Exception, optionally with a custom error code, and captures a stack
trace, but does not *throw* the exception.

> *ACHTUNG:* note that s2's class model makes it somewhat awkward to
differentiate various *types* of exceptions from one another, meaning
that it's usually not feasible to know, at catch-time, whether we have
just caught an exception thrown in a lexically local block or one
which was propagated via a function call, nor whether it was caused by
an intercepted syntax error or via an explicit `throw` in client code.
See [the custom exception types section](#type-exception-custom) for a
discussion of approaches to working around that.

Examples of using *throw*:

```s2
throw "My God! It's full of stars!";
// the message may be any value type *except* exception (see below):
throw { what: "foo", where: __FLC, who: someLocalValue };
// we can also catch and re-throw them:
if(const ex = catch{ throw 42 }) {
 ex.additionalInfo = '…';
 throw ex;
}
// or, if one prefers a geekier style:
(const ex = catch{ throw 42 }) && (ex.additionalInfo = '…', throw ex);
// minor difference: in the second form, ex is declared in the local scope
```

That last line demonstrates an exception (as it were) to the rule: if
`throw` is given an Exception object, it re-thrown it as-is, rather than
creating a new one and setting the older one as its `message` property.
Re-throwing retains the original source location and stack trace
information, and does not include location information for the point
where the exception was re-thrown. Feature or bug? You decide.
(We (the royal we) initially experimented with setting the re-thrown
exception as `message` property of a new exception, but doing so was,
for all cases where it was used, just extra noise.)

Examples of using the `exception()` function-like keyword:

```s2
var e = exception("error!");
assert 'CWAL_RC_EXCEPTION' === e.codeString();
assert 'error!' === e.message;
e = exception(1,2);
assert 1 === e.code;
assert 2 === e.message;
e = exception('CWAL_RC_MISUSE', "don't do that!");
assert 'integer' === typeinfo(name e.code); // some unspecified non-0 value
assert 'CWAL_RC_MISUSE' === e.codeString(); // cwal-level enum entry name for that integer
assert e.code; // non-zero
assert e.message === "don't do that!";
```

The code (first parameter, if two are passed in) is intended to be
either an integer or the string form of one of the `CWAL_RC_xxx` or
`S2_RC_xxx` family of C-level enum values. e.g. the string
`"CWAL_RC_RANGE"` or `"CWAL_RC_MISUSE"`. If passed a string, it tries to
resolve the code to its equivalent C-level integer value, defaulting to
`CWAL_RC_EXCEPTION`. Note that none of the `CWAL_RC_xxx` C-level enum
entries have well-defined values except that `CWAL_RC_OK` is
guaranteed to be 0 and all other values are guaranteed to be non-0.

Trivia: `catch` doesn't need braces around its RHS. Without braces, it
can evaluate any single expression, e.g. a function call:

```s2
if(const ex = catch someFunc()){
 print( __FLC, ": ", ex );
 throw ex;
}
```

When *initially* thrown, the interpreter adds the following properties
to an exception, insofar as it can at that point:

-   integers ***line*** (1-based) and ***column*** (0-based) of the
    source code location. These numbering conventions are taken from
    emacs, and might not line up with *your* editor's 0- or 1-based
    line/columns.)
-   integer ***code*** contains a C-level result code. For script-thrown
    exceptions (and some C-thrown ones), this is almost always
    `CWAL_RC_EXCEPTION`, but that can be changed using the `exception()`
    keyword. Note that s2 does not care what range of integer values is
    used for the code - we simply tend to use, by convention,
    cwal-derived codes because they're within easy reach.
-   ***message*** is the value which was passed to `throw` or set via C
    code. Note that it may be an arbitrary type.
-   ***script*** is the name of the script, if it is known. Note that it
    might be a "virtual" name (like "shell input"), and need not be a
    filename. It might even be empty.
-   ***stackTrace***: if the exception comes from anywhere but the top
    of the call stack, this property holds an array of stack trace
    information, with the closest (in terms of stack frames) entries
    at the *front* of the list. Each entry in the array in an object
    with the properties `line`, `column`, and `script`, with the same
    semantics as described above, listing a function call point
    leading up to the exception. Exceptions thrown from the top of the
    call stack do not have this property. Each function call currently
    active has an entry in the call stack, and this information can be
    invaluable in finding out which code led up to the exception being
    thrown. Trivia: the [`pragma`
    keyword](keyword-pragma.md#pragma-exception-stacktrace) can be
    used to disable the generation of exception stack traces.

Note that error location information in general (not just in exceptions)
might do weird things when second-pass evaluation of an eval'd string
triggers an exception or fatal error. The line/column/script information
cannot generally be trusted in such a case, as it is relative to a
dynamic string which does not exist directly in source code (and might
not exist at all by the time the exception is reported). It's not
entirely clear how best to fix that in a way compatible with how
function location information is gleaned. If it becomes a problem, wrap
the to-be-eval'd contents in a Buffer or String and use its
*evalContents()* method to "anchor" the script for error reporting
purposes.


<a id="type-exception-methods"></a>
# Exception Methods

Exceptions derive from Objects, and thus have the same methods, plus:

```s2-member
string codeString([integer code=this.code])
integer codeString(string enumEntryName)
```

This function has two distinct uses:

-   If passed a string in the form `"CWAL_RC_…"` or `"S2_RC_…"` then
    it (slowly!) performs a reverse mapping from the C-level enum
    entry name to its integer value. Returns `undefined` if the
    string does not match any enum entry names.

-   If passed no arguments or a non-string, it is assumed to be an
    integer, defaulting to the exception's `code` property, and returns the
    equivalent of passing the code value to the C function
    `s2_rc_cstr2()`. i.e. it returns a string-form name of the cwal/s2
    result code value (so it only works with values from that set, but
    all errors triggered directly from s2 use those). It returns
    `undefined` if passed no parameter *and* this exception has no
    `code` property, and `null` if `s2_rc_cstr2()` returns
    `NULL`. Results may be confusing (possibly downright misleading)
    if passed overlapping integer values from other libraries' result
    code ranges.

<a id="type-exception-custom"></a>
# Custom Exception Types

Though it's seldom necessary to do so, given the typically small scale
of s2 scripts, s2's basic exception mechanism can be bent a little bit
to allow it to convey custom error types...

<a id="type-exception-custom-inherit"></a>
## Inheriting from `exception`

It is (apparently) possible to create and throw custom exception types
in s2, but how to do so is admittedly so far from intuitive that s2's
own author didn't see how to do it until 20191228, when s2 was some
5.5 years old.

Creating a custom exception type which will, for most purposes, behave
like an exception (e.g. capture stacktrace info and be caught by the
`catch` keyword), requires doing something like the following:


```s2
const MyEx = {
    prototype: exception, // exception prototype, for inherited method(s)
    __typename: 'MyEx', // optional
    __new: proc(msg = __FLC){ // constructor
        /* Some prototype gymnastics are needed... */
        const p = this.prototype /* === MyEx */;
        this.prototype = exception(msg)
          /* This new exception captures stack trace, but now
             "this" no longer inherits MyEx, so... */;
        this.prototype.prototype = p
          /* ^^^^ without this part, "this" will inherit exception but NOT
             MyEx. */;
    }
};

// That's the entire implementation. What follows is the test code...

const check = proc(x){
    assert x inherits MyEx;
    assert !typeinfo(isexception x) /* because x is not "directly" an exception, but: */;
    assert typeinfo(hasexception x) /* because x inherits an exception */;
    assert "MyEx" === typeinfo(name x);
    assert typeinfo(isarray x.stackTrace) /* actually, it's inherited: */;
    assert x.prototype.stackTrace === x.stackTrace;
    assert x.stackTrace.0.script === __FILE;
    assert "blah!" === x.message /* derived from x.prototype */;
    assert typeinfo(isinteger x.code) /* derived from x.prototype. */;
    assert typeinfo(isinteger x.line) /* derived from x.prototype. */;
    assert typeinfo(isinteger x.column) /* derived from x.prototype. */;
    assert 'CWAL_RC_EXCEPTION' === x.codeString();
    assert !x.hasOwnProperty('script');
    assert x.script === __FILE;
};

// And now...
check(catch throw new MyEx("blah!"));
check(new MyEx("blah!"));
```

Regarding the inheritance of the exception-inherited properties: the
`exception()` keyword uses a default `code` value of
`CWAL_RC_EXCEPTION` if it's passed only a message value or an integer
code value of 0. The integer values of the C-level `CWAL_RC_xxx` and
`S2_RC_xxx` enum entries are unspecified except for `CWAL_RC_OK`,
which is 0 (and never shows up as an exception code). Thus we don't
"really" know, in script code, which integer value the exception will
have, but we do know that it must be an integer (because the C-level
exception type only supports an integer for the `code` property). We
could optionally pass our own integer value as the first argument to
the `exception()` keyword in `MyEx`'s constructor, and such values
need not necessarily correspond to those from the C-level
`CWAL_RC_xxx` and `S2_RC_xxx` enums. In particular, values 5000 and
higher are guaranteed not to be used by those enums.

There's one optional change we can make to the above which is
arguably a usability improvement but also arguably not...

```s2
__new: proc(msg = __FLC){
    const p = this.prototype,
          x = this.prototype = exception(msg);
    x.prototype = p;
    /* ^^^ That's exactly as demonstrated in the example above,
        but now we add... */
    /*
       Optional (but arguable) cosmetic improvement:
       x.line/column/script refer to this constructor function,
       which may be a bit confusing in practice. We can instead
       "steal" those from one level up in the stack trace (which
       may, of course, also be "differently confusing" in practice)...
    */
    if(const s = x.stackTrace.0){
        x.line = s.line;
        x.column = s.column;
        x.script = s.script;
    }
}
```

> Sidebar: this section of the docs above example was the inspiration
for changing the `exception` keyword to resolve to the base exception
prototype if it's invoked without a `(` following it.

<a id="type-exception-singleton"></a>
## Using Singleton Error Objects

Another viable approach to conveying custom error types is to use the
Singleton pattern. By and large, an application only has one relevant
exception at a time, and that can be modelled by creating
singleton-style exception *values*. For example:

```s2
const FooError = {
    __new: proc(msg = __FLC){
      const f = this.protototype /* FooError */;
      f.message = msg;
      return f;
    }
};
const BarError = {
   __new: FooError.__new
};

throw new FooError("Hi, there!");
```

Catching that would result in an exception with `FooError` set as its
`message` property. The constructor returns the prototype object,
which effectively gives it singleton semantics. For `BarError` we've
simply copied `FooError`'s constructor, turning `BarError` into a
functionally equivalent singleton with its own distinct value, so that
we can later distinguish such error types:

```s2
if(const ex = catch { ... }){
  if(FooError === ex.message){ ... }
  else if(BarError === ex.message){ ... }
}
```

Alternately, by taking advantage of the fact that any type of value
may be a property key, we can create a map of them to eliminate our
downstream use of their symbolic names and simplify checking for
them...


```s2
const FooError = {...as above...};
const BarError = {...as above...};
const ErrorTypes = {prototype:null};
foreach(@[FooError, BarError]=>e) ErrorTypes[e] = e;
```

And now catching them looks more like:

```s2
if(const ex = catch { ... }){
  if(const et = ErrorTypes[ex.message]){
    // et is one of our custom error types
  }
}
```

<a id="type-exception-custom-storage"></a>
## Where to Store Custom Exceptions?

Because of [s2's funky symbol resolution rules](symbol-resolution.md),
it makes sense to "anchor" the custom error objects somewhere with
global visibility, e.g. as a property of the `exception` prototype.

Extending the example from the previous section:

```s2
exception.FooError = {...};

// Changing its usage to:
throw new exception.FooError(...);
```
