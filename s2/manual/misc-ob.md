# s2: Output Buffering
#### ([&#x2b11;Table of Contents](./)) ([&#x2b11;Misc. Features Index](misc-index.md))
# Output Buffering

Jump to...

* [OB: Output Buffering](#misc-ob)
* [OB Methods](#misc-ob-methods)
* [Notes & Caveats](#misc-ob-caveats)


<a id="misc-ob"></a>
# Output Buffering

One of cwal's core features is the ability for the client to specify
an "output channel" - a client-defined callback function through which
all script-generated output "should" (by well-behaved clients) be
sent. The "ob" API builds upon that, providing functionality similar
to PHP's `ob_start()` family of functions. This allows capturing
output for emitting later on. e.g. as one might do when buffering
HTTP payload output so that HTTP headers can be sent before the
payload (*cough*[CGI module](../mod/cgi/)*cough*).

In short, one "pushes" a buffer on the stack, and then any output which
is generated via the normal output channel (specifically, output going
through the C-level `cwal_output()` family of functions) are intercepted
and appended to the current buffer. When the client is finished, one pops
the buffer from the stack and output resumes its regularly programmed
course. This API allows the client to discard the buffered output, fetch
it as a string or Buffer, or "flush" it, all demonstrated and described
below.

Reminder to self: we can't pass the *underlying* Buffers back to
scripts because they are necessarily created outside of the Value
management system, without an attached Value instance, because their
lifetimes would otherwise be unduly problematic for many
legal/conceivable use cases unless the client managed all buffer
references himself (e.g. `ob.push()` would return the buffer and
expect the client to manage it)[^1].

This API can be installed into a client-side interpreter using
`s2_install_ob()` or `s2_install_ob_2()`. [s2sh](s2sh.md)
installs it as `s2.ob`.

Example usage (the indentation matches the buffering levels):

```s2
const ob = s2.ob;
const out = print;
assert 1 === ob.push().level();
    out("This will be flushed to stdout.");
    ob.flush();
    out("level 1");
    var v1 = ob.takeString();
    ob.push();
        out("This will be flushed to level 1.");
        ob.flush();
        out("level 2");
        var v2 = ob.takeString();
    ob.pop();
    var v1b = ob.takeString();
    out("discarded");
    //ob.clear()// not needed b/c pop() will do this
ob.pop();
assert v1 === 'level 1\n';
assert v2 === 'level 2\n';
assert v1b === 'This will be flushed to level 1.\n';
```

That outputs only the following:

```
This will be flushed to stdout.
```

While this example uses `print()` for generating output, output
capturing applies when using any routines which use (perhaps
indirectly) `cwal_output()` (or one of its sibling APIs) to generate
their output. It does not (cannot) apply to code output via
lower-level C routines like `puts(3)` or `printf(3)`, and script
binders are strongly discouraged from using those in script
bindings. Using `cwal_output()` routes the data through the same
channel the rest of the script world uses, which is far more flexible
than using lower-level output routines (e.g. it allows us to
implement buffering, send all output to a given file, or disable
output altogether).

It is important to note that the memory owned by the underlying
buffer(s) is managed outside of the interpreter's garbage collection
system, a side effect of which is that `push()`/`pop()` operations may span
script-side scopes. The `getString()` method (and similar ones) transform
the underlying content into something managed by script-space, making
that copy subject to the normal lifetime/garbage collection rules (the
scope calling the method will be the initial owning scope for
newly-created values).

The `capture()` method (added several years after the above text was
written) greatly simplifies the process of keeping the OB levels
consistent, effectively removing the onus of `pop()`ping from the user
and guaranteeing that the levels stay consistent in the face of
exceptions and similar error conditions.


<a id="misc-ob-methods"></a>
# OB Methods

```s2-member
mixed capture(string|function callback [, int captureMode=-1 | buffer captureTarget])
```

This convenience routine pushes an OB level, runs a callback function
or evals a callback string, captures the output of all OB levels
pushed since it was called, then restores the OB level to its pre-call
state.

Throws if, after the callback, the OB level is lower than it was
before the callback was called/eval'd. Such a case indicates serious
mismanagement of the OB levels. The callback may `push()` any number
of OB levels but is not required to `pop()` them: if it leaves extra
OB levels on the stack, this function will capture them and pop them.

If the 2nd argument is a buffer, all captured output is appended to
that buffer and that buffer is returned. If it's not a buffer, it's
interpreted as an integer with the same semantics as `pop()`'s
argument but with a different default value: if it's negative (the
default) then the captured buffered output is returned as a string,
positive returns the result as a new buffer, and 0 means to simply
discard the result.

Managing OB levels is easier and safer with this approach, compared to
manually managing `push()`/`pop()` levels, because it keeps the OB
levels consistent even if the callback triggers an s2-level `assert`,
`exit`, `fatal`, exception, a cwal/s2 out-of-memory failure, C-level
interruption via `s2_interrupt()` (typically via Ctrl-C), or a similar
"flow-control event."


```s2-member
Object clear()
```

Discards the contents of the current buffering level but leaves the
buffer in place. Returns `this`. Throws if buffering is not active.

```s2-member
Object flush()
```

Pushes the current contents of the current buffer level down one
level, such that it either gets appended to another buffer (if
buffering is nested) or goes to the default configured output channel
(if the first buffer level is flushed). Empties out the current buffer
contents but leaves the buffer in place (does not change the
level). Returns `this`. Throws if buffering is not active.


```s2-member
string getString()
```

Returns the current buffer contents as a string and leaves the
buffer unmodified. Throws if buffering is not active.

```s2-member
integer level()
```

Returns the current buffering level, or 0 if not currently
buffering.

```s2-member
mixed pop([int takePolicy=0])
```

Pops the current level of buffering. Must only be called after a
corresponding call to `push()` (or it will throw). If passed no
arguments or passed a falsy value then it discards any buffered data,
freeing up its memory and returning `this`. If passed an numeric value
greater than 0 it returns the buffered contents as a Buffer (exactly
as for `takeBuffer()`). If passed a numeric value less than 0, it
returns the contents as a String (exactly as for `takeString()`).

Throws if buffering is not active.

```s2-member
Object push()
```

Pushes a level to the OB stack. *Must* be accompanied by a matching
call to `pop()` unless (special case) the current OB level is being
`capture()`d, in which case `capture()` will `pop()` it if the script
does not do so. Returns `this`.


```s2-member
Buffer takeBuffer()
```

"Takes" the underlying buffer away from the current buffering level,
effectively `clear()`ing it and transferring the contents, in the form
of a [Buffer](type-buffer.md) value, to the caller. Does not change
the buffering level, and generating more output at this level will
create a new buffer to store it in.

Throws if buffering is not active.

> Sidebar: we cannot return a handle to the underlying Buffer without
taking it away from the OB layer because it is not associated with a
Value instance (because the lifetimes cannot be sanely managed that
way).

```s2-member
string takeString()
```

Equivalent to calling `getString()` then `clear()`. This does not change
the buffering level, only transfers output buffered *at the current
buffer level* (including data `flush()`'d into it from a
subsequently-pushed OB scope). Generating more output at this level
will create a new buffer to store it.

Throws if buffering is not active.

<a id="misc-ob-caveats"></a>
# Notes & Caveats

- It's important that clients keep the buffering levels correct,
  meaning that they must call `pop()` one time for each time they call
  `push()`. Failing to do so can cause output to get "lost" or
  unintentionally hidden from view. It is often necessary to use
  `catch` blocks to ensure that a pending call to `pop()` is not
  missed, or a thrown exception's output might be hidden by that
  buffering level. *Or...*
- Using `capture()`, rather than `push()` and `pop()`, makes
  guaranteeing consistent OB levels trivial, even in cases which
  script code cannot respond to, e.g. when the stack is currently
  unwinding due to an s2-level `exit` or failed `assert`.
- While the API could in principle support various types of buffering
  modules (like PHP's "gz" module), its infrastructure currently does
  not do so. Potentially useful, yes, but not high on the priority
  list.


# Footnotes

[^1]: 20191215: since the addition of cwal-level scope push/pop hooks,
it would be conceivably possible for s2 to track the buffers in its
`s2_scope` class, rescoping them as needed to keep them from being
pulled out from under the client by lifetime management. Hmmm.
