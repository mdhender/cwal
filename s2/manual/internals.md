# s2: Internals
#### ([&#x2b11;Table of Contents](./))
# More than Anyone Needs to Know about s2 Internals

Jump to...

* [Input and Output](#internals-io)
* [Tokenization and Evaluation](#internals-t10n-e8n)
* [cwal's Lifetime Model in a Nutshell](#internals-cwal-lifetime-model)
* [Garbage Collection](#internals-gc)
* [Chunk Recycler](#internals-chunk-recycler)
* [String Padding](#internals-string-padding)
* [String Interning](#internals-string-interning)
* [Memory Usage](#internals-memory-usage)
    * [How Low Can We Go?](#internals-memory-how-low)

<a id="internals-io"></a>
# Input and Output

The cwal engine defines a set of output APIs which get funnelled through
an output function specified by the client during engine setup. Thus the
application which sets up the engine specifies where a given cwal engine
instance's "standard output" will go. Applications using C-level APIs
like `printf()` and `puts()` do not go through this channel. Script
bindings "should," if they need to generate console-style output, use
the `cwal_output()` family of functions, which includes `printf()`-like
variants. While the output channel is typically used for console output,
it could just as easily be redirected to a GUI widget, a file, a buffer,
or be discarded altogether (see the [output buffering API](misc-ob.md)).

Neither cwal nor s2 define any input channels, except for s2's basic
APIs for loading file content. cwal does specify the `cwal_input_f()`
interface, which is used by several APIs which stream input from
arbitrary sources, but s2 currently makes no direct use of them other
than via its few APIs which specifically read files


<a id="internals-t10n-e8n"></a>
# Tokenization and Evaluation

s2 uses the following distinct pieces to evaluate script code:

1.  [libcwal](/) provides the base Value and memory management
    subsystems, including the memory lifetime rules.
2.  A string tokenizer splits the inputs into atomic components like
    identifiers, numeric literals, strings, operators, etc. This is part
    of s2, not the core libcwal.
3.  An evaluation loop uses the tokenizer to stream tokens until EOF or
    an error is triggered. The evaluation loop manages anything having
    to do with syntax evaluation and handling of keywords. Values and
    operators parsed from that are pushed into...
4.  A stack machine, which is used to manage operator evaluation order
    and process the operators, which, in turn, apply themselves to the
    operands on the stack. The stack machine's API cannot see the
    tokenizer's API, which leads to some internally fudgery to propagate
    error location information when an operator triggers an error (as
    opposed to the parser doing so), because the stack machine does not
    (directly) have access to the script being executed (OTOH, it is
    capable of being used with arbitrary input source). Internally we
    use independent sub-stacks for each full expression (or
    subexpression), which makes it impossible for a subexpression to
    impact the stack of its pending LHS and allows the machine to
    recover from errors easily. By simply discarding a sub-stack (which
    happens regardless of success or error, anyway!), we return to our
    last known-good point in the parser/stack machine combination. This
    separation of stacks is done on the stack (as it were), and costs no
    more memory than a single master stack would, while being internally
    easier to manage because we have no danger of messing up the stack
    beyond the current (sub)expression.

Tokenization is really a two-step process: first comes a low-level
tokenizer which knows how to handle individual, atomic script symbols,
e.g. operators, quoted strings (not yet unescaped), identifiers, comment
blocks, and numeric literals. (This tokenizer is lower-level because it
derives from a tokenizer used by several other projects over the years,
and is largely language-agnostic.) Then comes a second, s2-specific
tokenizer which skips over "noise" tokens (comments and spaces) and
compounds certain types of lower-level tokens into larger tokens.
Examples include (parenthesis groups), \[brace groups\], {script
blocks}, and heredocs (which look like {script blocks} to the evaluation
engine, for historical reasons). Once this tokenizer is done with
block-level tokens, the evaluation engine sees them as a single token
with opaque contents. During evaluation, most such "compound" tokens get
evaluated recursively (with regards to the current expression) and
resolve to a single value.

The evaluation engine loops over the 2nd-level tokenizer's outputs,
converting it into a series of operators and their operands (cwal
Values). At certain points (depending on operator precedence,
subexpressions, and other internals), it processes any pending operators
waiting in the stack machine. Keywords are, for the most part, basically
parser-level operators which resolve to a single value. Most do not
directly go through the stack machine, except to evaluate any arguments,
and most resolve to values. Some get translated into operators for
evaluation at a later point, and some (like *fatal* and *throw*) trigger
an error, some of which (e.g. *return*, *break*, *continue*, and *exit*)
are handled at various points in the evaluation process. e.g. only loops
handle (stop propagation of) the *break* and *continue* "errors".

The engine supports a "skip mode," which tells it to run through code as
if it normally would, but to (A) consume as much as would be optimal for
a given expression and (B) to have essentially no side-effects while
doing so. e.g. it does not allocate new values in this mode, does not
call script-callable functions, and uses the *undefined* value as a
result for all expressions. This feature is the basis of
short-circuiting, as it allows us to walk over the syntax without
triggering any effects associated with that code. As an example: (0 &&
print(1,2,3)) will short-circuit the RHS by entering skip mode before
traversing over 'print'. Because the RHS of 'print' is a '(', it
normally looks like a function call. Because skip mode does not resolve
identifiers, skip mode assumes that anything which *could* be a function
call *is* a function call (that's the "optimal consumption" rule), and
walks over it without calling it or processing its arguments. Skip mode
can also be used to determine if a given input is *syntactically* valid
without actually evaluating the code. Skip mode is also used internally
to avoid processing of (while still tokenizing) default values of
function parameters when a caller provides an argument for that
parameter.

The core of s2 evaluates only single expressions. `s2_eval_cstr()` and
friends build atop that to keep evaluating expressions until an error or
the end of their script. Much of the lifetime management magic happens
via those routines, as they are tasked with both keeping one pending
expression's worth of results alive while also cleaning up anything
which isn't needed, and handling that conflict of interests is not as
straightforward as it might initially sound (that said, the (eventual!)
solution turned out to fit quite nicely and led to improvements in
cwal's core).


<a id="internals-cwal-lifetime-model"></a>
# cwal's Lifetime Model in a Nutshell

cwal, s2's core engine, is, in a nutshell, a memory manager aimed at
scripting languages. To that end, it provides APIs for creating and
managing abstract Values (numbers, object, etc.), and cwal makes sure
those can get cleaned up if the client fails to do so.

cwal's Value lifetime model is fundamentally very simple, and is
summarized in this list:

-   The engine owns a single stack of Scopes.
    -   It always has at least one Scope active (which one may think of
        as the "global" scope).
    -   The active Scope is always the newest one (the youngest in the
        stack). In historical/conventional terms the newest scope is
        called the "bottom" of the stack, not the "top." Something to
        do with how stack memory grows at the lowest levels, IIRC.
    -   New Scopes always derive from (link back to) the Scope which was
        active when the new one was pushed. That is, each scope has
        the previous scope as its parent.
    -   Trivia: in practice, scopes are always stack-allocated. The
        library allows them to be heap allocated, but that can open
        them up to more potential lifetime issues, as they *must* be
        destroyed in reverse order for proper operation of the engine.
        A stack-allocated scope costs no dynamic memory unless
        variables (including the "this" and "argv" vars which s2
        automatically defines for functions) are stored in it.
-   When a Value is created (allocated) it is initially owned by the
    active Scope. Note that this is different from a variable-style
    ownership: this ownership does not imply any sort of visibility of
    the value in either C or script code. It is a lifetime management
    artifact, not a script-side variable. (cwal provides, as a
    convenience, support for scope-level variables, but that's another
    topic altogether.) "Values" generically refers to the "Value
    interface", but each Value has a distinct, concrete type (string,
    integer, Object, etc.).
-   Each Value has a reference count (a.k.a. refcount), which is the
    most fundamental part of tracking a Value's lifetime. For
    newly-allocated Values, the refcount starts at zero, not one. It is
    incremented/decremented automatically when values are used in
    containers, and clients may manually ref/unref Values as well.
    Values with a refcount of 0 are colloquially known as "temp" values,
    subject to being "swept up" (a more advanced topic, built on top of
    the model described here and not addressed here).
-   When a Value is placed into a Container Value (Object, Array, etc.)
    as a property or list entry, if the Container's owning Scope is
    older than the Value's owning Scope, the Value is "up-scoped" into
    the Container's owning Scope for lifetime management purposes (its
    refcount and script-side visibility are unaffected by up-scoping).
    The model *prohibits* Values being "down-scoped," i.e. moved into a
    newer scope, and provides no APIs for doing so (but in the meantime
    there is a potential finalization optimization or two we could
    possibly make with such a feature, and internally it "should" work
    if we're exceedingly careful).
-   When a Value's reference count goes to 0, its finalizer (a C
    function, not script function) is called and its memory is freed or
    recycled for later use, depending on configuration options. There
    are cases where, when its refcount is changed from 1 to 0, cwal must
    restore it as a temporary value without immediately destroying it.
    (Nutshell: that is mainly used to ensure that certain Values don't
    get nuked while propagating up a call stack with *return*-like
    semantics.)
-   When a Scope is popped from the stack, all Values it owns,
    regardless of their reference counts or any cycles they partake in,
    are destroyed. Each Value's finalizer is only called once,
    regardless of whether the Value participates in cycles.

That's the basis of the lifetime model, with the rest of cwal building
off of the properties described above. There are exceptions and "fine
print" to some of the details, but the overall model derives from the
properties described the above list, as well as some properties implied
by the above. e.g. the rules combined imply that it is impossible for
any values in a Scope to *be referenced by* values owned by older
Scopes, and the engine relies on that being the case (meaning it
requires that to be the case for proper functioning). Generally this
moving-about of values between scopes happens automatically as part of
its normally inner workings, but higher-level bindings (s2 or client
code) need to tell it to up-scope a Value in certain use cases. This is
needed less often at the client level, and is mostly seen at the s2
level or the part of the app which initializes and reacts to the s2
engine (as opposed to code which gets triggered via scripts, which
essentially never has need for dealing with scopes (and scoping issues)
directly).


<a id="internals-gc"></a>
# Garbage Collection

s2 uses libcwal's core garbage collection mechanism, meaning basically:

-   All values except built-in constants start life with a reference
    count of 0 (the refcount of built-ins is never modified because
    those values are static and constant). Zero is (in general, at least
    temporarily) a legal reference count for non-script-visible values.
    Script-visible values, simply by their nature, always have at least
    one reference because some identifier or property refers to them.
    ("Script-invisible" temporaries abound in the evaluation engine,
    though.)
-   All values except built-in constants belong to a scope. Initially,
    it is the scope they are created in, but ultimately it is the
    *oldest* scope which ever "references" the value (directly, e.g. via
    a *var* decl, or an indirect connection via a container)[^55].
    When a value's owning scope is destroyed, the value will be
    destroyed, reference count and cycles notwithstanding. cwal
    guarantees that finalizers are called for any types which have one
    (all non-constant types have one, but only the Native and Function
    types supports client-provided finalizers). Unlike in C, s2's
    *exit*, *fatal*, and *assert* keywords (as well as s2-managed
    exceptions) unwind the stack so that finalizers will be called.
-   Most values are destroyed the moment their refcount hits 0, but
    occasional exceptions must be (briefly) made, partially for the sake
    of values which are propagating up the stack. Such values, if their
    refcount reaches 0, get reverted to "temporary" status (internally
    known as "probationary") and will either get cleaned up a few ticks
    later or (at the latest) the next sweep unless the caller takes a
    reference.
-   The core provides a *sweep* operation which cleans up all
    temporaries in the *current* scope. It also provides a separate
    [vacuum algorithm](/event/45c756c35197991b0312146bfbe53328aa3759e7)
    which (in addition to sweeping) weeds out and destroys cyclic
    structures *owned by the current scope* which are no longer
    reachable from script code.
-   However...
-   The core cwal engine never has enough information to know whether
    sweeping and/or vacuuming are safe, so it cannot do so on its own
    accord. (s2 can, though - see below.)
-   cwal's core value types optionally participate in memory recycling.
    It is on by default, and it typically saves 90%+ on allocations for
    any mid-sized scripts (95% is common and 97%+ is not uncommon). This
    makes typical loops very cheap (typically "free" after the first
    iteration), and cleanup and reallocation quite fast, too.

"Sweeping" and "vacuuming" are mechanisms the cwal engine provides to
reap "abandoned" values. These algorithms apply only to the current
scope, which is, by API rules, always the newest scope on the stack.
Sweeping cleans up "temporary" values - those with a refcount of 0 and
which have not otherwise been destroyed (most get destroyed as soon as
they reach 0). Vacuuming cleans up not only temporaries, but *all*
values in the current scope which are *not reachable from script code*
and not otherwise protected from vacuuming (via
`cwal_value_make_vacuum_safe()` or via containment in (or via) a
vacuum-safe container). Trivia: Vacuum-safe values do not get unusual
lifetimes or additional references, they are simply made immune to being
vacuumed up (but not immune to sweeping). Vacuum-proofing as a feature
was necessary to keep internal-use Values (with no direct script-visible
reference) from being vacuumed up (examples include, but are not limited
to, the core-most Value prototypes).

To the core cwal-level features, s2 adds a mechanism for figuring out
(or, more correctly, specifying) when it is safe to sweep up temporary
values in the current scope, and sweeps up and/or vacuums at a regular
(configurable) interval (measured in full expressions, as it's never
safe for s2 to sweep a given scope in mid-expression (due largely to our
internal (mis-)use of temporaries)).

At its most aggressive, s2 will sweep or vacuum the current scope after
every complete expression[^56]. At certain points during evaluation,
it toggles sweeping or vacuuming off in order to keep an in-progress
expression from being swept up, but never for longer than it needs to.
An alternative approach would be to explicitly add references to all of
our temporaries, but it gets very cumbersome to make sure we unref them
all at the proper times. It also doesn't solve the problem of a vacuum
cleaning up expression-pending values (which are not visible from script
code, and therefore subject to vacuuming). The current mechanism, it
would seem, is easier to implement, and keeps all values kosher
vis-a-vis sweeping and vacuuming (but implicitly prohibits ever running
a *recursive* sweep across multiple scopes and requires one to be
extra-careful in places where client-side callbacks can potentially
interfere with one's understanding of the current values' lifetimes).

One notable caveat is that sweeping and vacuuming are only safe (when
they're safe at all) to do on the current scope. It cannot be safely
called recursively up the scope stack because doing so may inadvertently
pull values out from under native-level code lower down in the scope
stack which is currently managing non-script-visible Values. The only
way to eliminate that risk is for all native-level code which *might*
recurse into the script world ensure that all of its own values are
vacuum-safe, and that would a relatively invasive requirement to impose
(and easy to forget, leading to mysterious lifetime-related crashes at
unpredictable points downstream).

The core lifetime model, combined with sweeping and vacuuming ensure
that, except in pathological cases which so far are *mostly*
hypothetical (but could trivially be constructed, potentially
unintentionally), script-created values get cleaned up as quickly as is
feasible (i.e. safe), making their memory available for recycling.
Recycling turns allocation of most subsequent values into an O(1)
operation[^57], and the sizes of the recycling pools are configurable
on a per-base-type basis (with default sizes based on prior experience).
Experience with th1ish and s2 have shown recycling to cut the number of
allocations by well over 90% for any appreciably-sized scripts. The
larger the scripts, the better it generally performs. IIRC, the best
i've seen is somewhere just over 98% of allocations saved via recycling,
but a long loop which creates lots of numbers (e.g. loop control
variables) can easily skew those results because the memory used for
loop-local variables will get continually recycled upon each iteration
of the loop. Scripts which never use loops may never even reach a mere
50% savings, but (unless recycling is off) they'll still see some
(possibly notable) benefit from it.


<a id="internals-chunk-recycler"></a>
# Chunk Recycler

cwal internally manages a generic memory chunk recycler which is used by
buffers, arrays, and hashtables when an *empty* instance needs to
allocate memory for its underlying storage. This recycler requires no
dynamic memory of its own - it uses the space inside recycled memory to
manage the chunks. It has a client-configurable peak size and has an
O(N) search component (N is the number of memory chunks being recycled),
but the N is kept relatively small via internal optimizations (and there
is another optimization on the drawing board (reminder to self: group
them into an array, grouping on, e.g. `length / sizeof(void*)`, or some
similar algorithm).

In principle the engine can put any internally discarded memory into the
chunk recycler. Most value types use a more specialized recycler which
guarantees O(1) performance, so this recycler is not used for Values (by
default, though it can optionally be used as a fallback if the value
recycler is full or disabled). Some string memory (that which is
considered too large for the dedicated string recycler) also goes into
this recycler, which means s2 will be able to recycle more function body
memory.

Initial tests show the chunk-based allocator to perform marginally
better than the prior mechanism, saving approximately 6% of allocations
in the amalgamated s2 unit tests.

Here are some numbers gleaned from a 64-bit build on 20141216 using the
amalgamated s2 unit tests as input (\~1900 lines of script code), pasted
from the cwal metrics dump:

```
Chunk recycler capacity=35 chunks, 262144 bytes.
Chunk requests: 2404, Comparisons: 2362, Misses: 70,
  Smallest chunk: 48 bytes, Largest: 472 bytes
Reused 2334 chunk(s) totaling 264543 byte(s),
  with peaks of 14 chunk(s) and 3050 byte(s).
Running averages: chunk size: 63, response size: 72
Currently contains 12 chunk(s) totaling 2828 byte(s).
Current chunk sizes (ascending): 48, 129, 161, 161, 224, 232, 232, 280,
  288, 297, 304, 472
```

The interesting part is really the request count vs. comparisons and
misses. Firstly, the delta between the request count and comparison
count is less than 3%, meaning search time is being kept low (much
lower than its O(N) element would imply). The entries are kept sorted
by size, and the API provides for a hint to specify whether a larger
buffer (and what magnitude larger) may serve the request, allowing it
to stop searching more quickly when it knows it cannot find a suitable
match.  Secondly, roughly 97% of the requests for memory from the
chunk recycler result in memory being reused (in his example - that is
not a generic blanket statement). *However*, the recycler does not
know if the downstream client might immediately need to realloc it, so
a `realloc()` of the memory *might* follow. Over time the chunk
recycler can be improved via better hinting, such that it can help
better serve requests where a pending `realloc()` of the memory is
likely or certain.

<a id="internals-string-padding"></a>
# String Padding

When cwal allocates a String value, it pads its length (internally) up
to a common denominator. Testing has led to a current padding value of 8
bytes. That means a length-1 string and a length-8 string both
internally take up the same amount of memory (cwal stores theirs size
and adds a `NUL` byte to them, as well). The reason for the padding
(initially an experiment) was to allow string recycling to be able to
perform better than its same-length-only recycler could. Perhaps
non-intuitively, this padding turns out to save both allocations and
total memory because it allows the recycler to perform (best case)
padding-size (==8) times better than an exact-match-only recycler can.
To malloc-conscious readers, the moral of this trivia lesson is that
oft-used/reused strings *might* be slightly more efficient (mainly in
loops) if their sizes, *including* the (invisible to client code)
internal padding, are a common multiple of that padding value. Why?
Because it allows the recycler to serve more requests with less
size-comparing if "looping" requests can keep re-serving the same-sized
chunks which the GC just fed it at the end of a loop iteration. (More
trivia: deep, deep in cwal, the padding is only observed in two places:
the string allocator and the recycler. Everyone else (including s2) uses
the string's "public" length, as the padding is an internal recycling
detail.)

Pedantic side-note: the above does not apply to X-strings and Z-strings,
as their memory comes from "elsewhere." Neither scripts nor C code
(outside of cwal's internals) can differentiate between "normal"
strings, X-strings, and Z-strings after they are constructed, but
construction requires using one of their constructors, each of which has
*very* different memory ownership semantics.


<a id="internals-string-interning"></a>
# String Interning

If cwal's optional "string interning" feature is enabled[^67], cwal/s2
will automatically re-use (some) strings which are instantiated 2+ times
concurrently (*basically* meaning, "in the same active call stack").
This does not extend the lifetime of string, but it does note each
currently active string in its "interning table," a cwal-internal hash
table-like construct optimized for low allocation counts and O(1) search
speed. When a given string is used in two subsequent scopes which are
not in the same active call stack chain, it might clean up the string
between scopes (if there are no references to it), but in a given call
stack, many in-use strings (including most identifiers) will be in the
interning table, and using those strings in script code will not require
multiple allocations. For example:

```
var s = "hi"; // gets interned here (if enabled)
var x = "hi"; // will see the existing instance and re-use it.
```

Once those go out of scope, "hi" will be cleaned up unless it was
initially re-used from a higher-up (older) scope. If those two strings
are used in different scopes run one after another, but not in the same
call stack, the first instance would have been cleaned up by the time
the second was requested, in which case cwal will take the memory back
from the recycle bin or (if needed) allocate a new one.

Here's a simple demonstration from s2sh:

```
// without interning...
s2sh> s2.dumpVal("hi","hi")
s2_cb_dump_val():434: dump_val:
string@0x26da900[scope=#2@0x7fff1561e6a0 ref#=1] ==> "hi"
s2_cb_dump_val():434: dump_val:
string@0x26da950[scope=#2@0x7fff1561e6a0 ref#=1] ==> "hi"

// and now with interning (in a different session)...
s2sh> s2.dumpVal("hi","hi")
s2_cb_dump_val():434: dump_val:
string@0x1867e30[scope=#2@0x7fffdf620090 ref#=2] ==> "hi"
s2_cb_dump_val():434: dump_val:
string@0x1867e30[scope=#2@0x7fffdf620090 ref#=2] ==> "hi"
```

That's *essentially* all one needs to know about it, but note that it
also applies to non-keyword identifiers as well as quoted strings
(within the limits explained below).

Whether or not any given string is "internable" at all is determined by
a (C-client customizable) predicate function provided to cwal by its
client application (s2sh, in this case). s2sh only interns
identifier-like strings under a certain (unspecified) maximum length, or
any (byte-)-length-1 string, regardless of its contents (because it's
common to have `\n` and similar, as well as single-letter identifiers,
in script code). (In the meantime, length-1 ASCII strings are normally
(depending on build-time configuration) built-in constants.)

String interning tends to benefit most in longer scripts, especially
those which do any significant amount of looping. It costs a bit of
peak/total memory (anywhere from 2-20kb, depending on many factors), but
generally reduces total allocation counts by a small amount (a few
percentage points for most scripts). Interestingly, it generally
provides a more notable boost, in terms of percentages, if recycling is
*disabled* (savings of just over 25% have been seen in the s2 unit
tests).

To experiment with the effects of interning when using s2sh, pass the
(`-S -m -v -v`) args (in any order) to the interpreter to (A) enable
string interning and (B) dump out lots of metrics at the end, including
memory usage, allocation counts, and the entire contents of the string
interning table as it stands immediately before shutdown of the global
scope (don't expect to see any out-of-scope strings there, as they've
already been cleaned up). One can use `--S` (with two dashes) to
explicitly disable string interning if it's enabled by default in a
given build (or to supercede a prior `-S`, sometimes useful in
shell-script-based testing).


<a id="internals-memory-usage"></a>
# Memory Usage

> Disclosure: most of what follows was written between 2014 and 2016
(and now = Dec. 2019). The core infrastructure costs of s2 have grown
a bit since then, but the memory usage patterns described below have
not changed significantly. Note that scripts/files linked to in this
section may well be very old versions (current at the time these docs
were written).

s2's memory usage is, if i may be so bold as to say so, *outstandingly
low* in the general case. It can evaluate many scripts in less memory
than the script itself requires, averaging less than 1 allocation per
line of code, and can (depending on how one cares to count) average less
than 1 allocation per 3-5[^58] lines. These numbers are based on the
following...

-   Memory usage metrics for a script, as reported by valgrind and
    cwal's internal metrics. (cwal internally tracks a good deal of
    metrics so that the effects of memory-related changes and
    optimization settings can be measured and compared.)
-   Subtract the size of the input script.
-   Subtract the "overhead cost" all scripts have via global functions
    and prototype-level infrastructure. This is calculated by running an
    empty script through valgrind and peeking at cwal's metrics (in
    [s2sh](s2sh.md): the (`-m -v`) flag combination).

The resulting memory usage has, for the s2 unit test scripts,
consistently been as frugal as described above. The memory usage varies,
of course, depending on many factors, but in relatively basic scripts
(like the unit test scripts it targets) it performs remarkably well in
terms of memory usage and allocation counts. If value recycling is
*disabled*, the allocation counts shoot through the roof, but the *peak*
memory tends to drop by a couple KB because the recycling bins are kept
empty.

Here's an example based on the current unit test suite as of this
writing (20141129)...

-   Platform: ODroid U3 (ARM 32-bit) running Linux.
-   Input script: [a concatenation
    of](/info/5f1181f6aadb4253c55ecf1eb3b13fe48ce556ce?ln=311-330)
    all of [s2's core unit test scripts](/dir/s2/unit?ci=trunk)
    (but somewhere around [version 3770d47](/dir/s2/unit?ci=3770d47ace44a740)), with the contents of each file
    wrapped in a `scope{...}` block so that they don't interfere with
    one another's symbols.\  
    60623 bytes with 2405 physical lines, approximately 1636 lines of
    expressions (lines starting with a letter or number).
-   Valgrind reports: 1032 allocations totaling 105647 bytes. Massif
    measures a peak RAM of 97.29KB.
-   Overhead cost (empty script): 514 allocations totaling 24216 bytes.
    Massif says 24.2KB peak. The *vast* majority of those allocations
    are for prototype-level infrastructure (mostly key/value
    (string/function) pairs for prototype methods). 486 allocations, to
    be exact, totaling 16916 bytes, to be pretty close to exact, plus 3
    allocs and 4584 bytes for string interning tables, plus 1 alloc and
    700 bytes for the chunk recycler (which has subsequently been
    refactored to not need additional memory).

Applying the above-described total cost formula: total allocations
(105647) minus script size (60623) minus overhead cost (24216) = 20808
bytes and (1032-overhead=474) = 518 allocations.

If we count the overhead, that's an average of 0.63 allocations per line
of script code. If we discount the overhead cost, it's an aggregate of
0.31 allocations per line of script code. In either case, execution of
the script requires less dynamic memory than the script itself does.

s2's overhead cost grows as infrastructure is added (global functions,
shared class methods, etc.), but the overhead cost is flat for all
scripts and can be re-used for any number of scripts in one interpreter
session. Even if we count the overhead cost, the total memory usage for
the evaluation is less than the size of the input, and still averages
less than 1 allocation per line of script code. That is all due to the
combination of value recycling and the lifetime management/garbage
collection mechanisms. In principle, provided that scripts keep
potentially-orphaned cyclic structures from propagating to
seldom-visited older scopes (where they cannot be swept resp. vacuumed
up until control returns to the scope in question), and do not otherwise
indefinitely collect data they no longer use (but still reference), s2
can run a script (again, in principle) forever with some flat peak
memory cost.

:-D

> Sidebar: the majority of cwal's job is pointer tracking, and pointers
cost 8 bytes each on 64-bit platforms, compared to 4 bytes on 32-bit
platforms like the one used for this test. Because of this, most of the
core cwal data types cost notably more on 64-bit platforms. In practice,
on average, cwal-related memory (largely pointers) tends to cost roughly
around 40-60% more RAM on 64-bit platforms compared to 32-bit. The
allocation counts should be close to the same for both, at least as far
as the cwal/s2 bits are concerned[^59].

As an admittedly apples-vs-oranges point of comparison (because engine
startup costs are only a very small part of the overall picture), here
are the valgrind-generated numbers for running an empty script through
various interpreters installed on this machine (32-bit ARM running
Linux):

-   **lua** allocates 299 times for 22093 bytes for an empty script.
    Notably fewer allocs than s2 (less script-global infrastructure like
    prototype methods) and roughly equivalent total memory.
-   **JimTCL** allocates 1665 times for 83444 bytes on an empty script.
-   **tclsh**: 143 allocs for 306KB, but it leaks 14 allocs and 216KB!
    It leaks more than s2, lua, and JimTCL allocate *combined* ;).
-   **Python 3.3.2**: 23486 allocations[^60] \(646 of which are leaked)
    totaling 3.8MB of RAM (344KB of which is leaked) *for an empty
    script*.
    -   Sidebar: even with *all* recycling options *turned off*, s2
        needs fewer allocations and (far) less memory for its *largest*
        unit tests (as of 20160218 on a 64-bit build): input script
        (91kb, \~2310 LOC) requires 22170 allocs and 1.43M total memory
        (only 154k peak), not a single byte of which is leaked.

Comparatively speaking, s2 clearly falls into the "low memory usage"
category of script interpreters.

Note that startup costs say *essentially nothing* about runtime
allocation behaviour, as startup costs tend to include primarily
resources which are global to the script environment, and not subject to
cleanup like script-created values are.

<a id="internals-memory-how-low"></a>
## How Low Can We Go?

On 20141128 s2sh got a "cleanroom" mode, which sets up the s2
interpreter without any prototype-level infrastructure or global
symbols, leaving just "raw s2." The lack of built-in functions makes it
not much use as anything other than a glorified calculator, but it works
(clients can still create script-side functions, but not having the
prototype methods limits them severely). Maybe someday we'll add
"levels" of APIs, each level installing the previous level and some
additional functionality. e.g. level 1 might be to include prototypes
and their methods, level 2 might include the "most important" global
functions, and level 3 might be "everything but the kitchen sink."
Maybe.

The reason for this feature is to test how much memory the core needs,
without the higher-level infrastructure which clients generally need for
it.

Let's jump straight to some code. Here's a test script we'll run through
clean-room mode:

```
var a = [1];
for( var i = 0; i < 1000; ) i=a[i%3]++;
for( var i = 0; i < 1000; ) i=a[i%3]++;
for( var i = 0; i < 1000; ) i=a[i%3]++;
for( var i = 0; i < 1000; ) i=a[i%3]++;
a; // script result, so we can output the array
```

(Notice that we explicitly use the postfix ++ operator, as it
historically had problems "letting go" of temporaries which live in
higher scopes. We do that to show below that that's no longer a
problem.)

We run it like this:

```
$ ./s2sh --a -cleanroom -f 2.s2 --S -v
verbose: Clean-room mode is enabled: NOT installing globals/prototypes.
result: array@0x7bbc0[scope=#1@0xbebe92dc ref#=0] ==> [
 1004,
 997,
 997
]
```

Note that we disabled [string interning](#internals-string-interning)
(with `--S`) because that's known to take up 2-10kb. We leave the
"value recycler" on because the only reason not to is when testing for
lifetime-related problems which the recycler often hides or delays
(normally when testing new C code). The [chunk
recycler](#internals-chunk-recycler) is left on because it no longer
takes up any memory of its own.

Point of reference and maintenance reminder: this code and the numbers
below were last updated on 20141205. In 2015 nothing changed (because of
extended medical leave), and in 2016/17 not too much changed in terms of
memory costs (modulo the "eval holder" added in late 2017, but that
doesn't affect the memory costs described here).

On a 64-bit build, valgrind reports:

```
==10992== HEAP SUMMARY:
==10992== in use at exit: 0 bytes in 0 blocks
==10992== total heap usage: 63 allocs, 63 frees, 7,154 bytes allocated
```

That's not bad, but … the majority of that memory is going to the
system-level `setlocale()`, not cwal! Valgrind's *massif* tool tells us
that cwal allocated only:

```
->41.52% (2,770B) 0x44479E: cwal_realloc_f_std (cwal.c:2974)
```

That was on a 64-bit Intel system. Here's the same on a 32-bit ARM
[ODroid U3](http://hardkernel.com/main/products/prdt_info.php?g_code=G138745696275):

```
==9072== HEAP SUMMARY:
==9072== in use at exit: 0 bytes in 0 blocks
==9072== total heap usage: 66 allocs, 66 frees, 4,018 bytes allocated
```

And *massif* tell us that cwal takes only:

```
->41.45% (1,522B) 0x3AE8A: cwal_realloc_f_std (cwal.c:2974)
```

With that memory it is reading a small script, running 4 loops of 1000
iterations each, incrementing 4000 entries in 3 cells of an array, and
outputting the resulting array in nicely-indented JSON format (full
disclosure: the JSON formatting actually happens on the stack and is
streamed to its destination, not held in memory).

As a point of comparison: a DOS-style terminal screen (80 columns by
25 lines) has 2000 screen cells, and in the old days, one cell was one
byte, if you didn't count colors and other meta-attributes. My current
terminal is 185x33, or 6105 cells. i.e. all of the
dynamically-allocated memory used in the above demonstration would
easily fit in a terminal window.

cwal's internal metrics give us lots of details about where that memory
is going. The following dump is from the 32-bit run. The sizeof()s are
higher in 64-bit mode but the alloc counts are approximately the same,
except that some types are recycled in different bins (due to differing
`sizeof()`), so alloc counts may differ marginally between 32- and
64-bit:

cwal-level allocation metrics:

```
TypeId/Bin[1]/Name   AllocRequests   Actually allocated count * Value-type sizeof() ==> total bytes from allocator

3   0 integer    14986       6  (000.04%)  * 24 ==> 144
5   1 string     3007        9  (000.30%)  * 24 ==> 345  Incl. string bytes plus NULs
6   4 array      3003        3  (000.10%)  * 44 ==> 132  Not incl. cwal_list memory
7   2 object     6           3  (050.00%)  * 32 ==> 96
8   5 function   4           4  (100.00%)  * 48 ==> 192
11  2 buffer     0           0  (000.00%)  * 32 ==> 201  [2] Incl. total allocated buffer memory and non-Value buffers
12  5 hash       1           1  (100.00%)  * 48 ==> 48   Not incl. cwal_list table memory.
13  8 cwal_scope 3008        0  (000.00%)  * 40 ==> 0    [3]
14  6 cwal_kvp   10          7  (070.00%)  * 16 ==> 112  Key/value pairs (obj. properties/hash entries)
19 -1 cwal_list  0           0  (000.00%)  *  0 ==> 272  Total alloced cwal_list::list memory used by arrays, hashtables, ...

Totals:          21017       33 (000.16%)[1]==> 1542

The recycler moves some bits of memory around, counting them
only once, so do not search for an Absolute Truth in these metrics!

[1] = Types with the same 'bin' value share a recycling bin. A value of -1 indicates
either no recycling or a separate mechanism. "Normal" strings use their own pool,
separate from x-/z-strings, despite the indication otherwise above. The bin numbers
incidentally (almost) correspond to the types' relative sizes.

[2] = % applies to allocation counts, not sizes. The total alloc count does not account
for cwal_buffer::mem (re)allocations, but the total size does. A request/allocation count
of 0 and memory >0 means that only non-Value buffers allocated memory.

[3] = Scopes are always stack allocated and skew the statistics, so only allocated scopes
are counted for totals purposes.

Value/KVP Recycling: 15985 recycled, 33 recycler misses.
Bin #  SlotSize  Capacity  Hits   Misses  Currently holding (recyclable for type(s))
  024       80    9981        6   6     (integer, unique)
  232       30    3           3   2     (object, buffer)
  444       30    3000        3   3     (array)
  548       50    0           5   0     (function, native, hash)
  616       80    3           7   2     (cwal_kvp)
strings:    50    2998        9   3
Currently holding 444 bytes (sans raw string bytes) in 13 slot(s) in 5 bin(s).

Client-reported memory: currently 96 of 96 total reported bytes

Chunk recycler capacity=35 chunks, 262144 bytes.
Chunk requests: 3008, Comparisons: 3000, Misses: 8, Smallest chunk: 24 bytes, Largest: 180 bytes
Reused 3000 chunk(s) totaling 72000 byte(s), with peaks of 3 chunk(s) and 228 byte(s).
Running averages: chunk size: 62, response size: 23
Currently contains 3 chunk(s) totaling 228 byte(s).
Current chunk sizes (ascending):  24, 24, 180

cwal_engine instance appears to have been stack-allocated (not by cwal_engine_init()). sizeof(cwal_engine)=916

Total bytes allocated for metrics-tracked resources: 1638

s2-side metrics:
Peak cwal_scope levels: 4
Peak eval depth: 3
Total s2_stokens (sizeof=16) requested=51040, allocated=6 (=96 bytes), alive=0, peakAlive=6, recyclerSize=6
Total calls to s2_next_token(): 81108
Saved 816544 bytes and 51034 allocs via stack token recycling :D.
```

Apparently either cwal or massif has a small discrepancy in their
accounting, as cwal counts a few more bytes than massif does. It is not
uncommon to see such small (1-2kb) differences between values reported
by valgrind and massif (a valgrind tool), though, so i'm not going to
sweat it.

It only had to allocate 33 values for the whole script, out of 21017
value allocation requests. i.e. only 0.16% of the values used by the
script required memory allocation.

All i can say to that is: *Holy &lt;expletive&gt; :-D!*

> Sidebar: the reason for the 3003 array allocations is: one comes
from the script, one is from cwal's internals, and one from the
evaluation engine. During evaluation it uses an array to keep the
pending result value safe from being garbage collected, and that array
and its list memory (probably that 180-byte chunk in the chunk
recycler) get reused 3000 times. Update 2016-03: that array is no
longer needed, due to improvements in vacuum protection in the cwal
core, and was factored out. Update 2017-11: a similar array was
(re-)added at the scope level, needed in order to handle errant
temporaries in certain obscure script constructs. The hope is that can
eventually be factored out, but doing so requires some detailed
surgery on the stack machine's handling of value references.


# Footnotes

[^55]:  "References," in this context, does not specifically mean "to
    acquire a reference," but explaining the subtleties of the semantics
    here would take us on a very long tangent!

[^56]:  Such a frequent cleanup is primarily useful in trying to trigger
    lifetime-related bugs caused by reference mismanagement. For the
    general case, a sweep interval of 3-5 would seem fairly reasonable,
    and a vacuum interval of 3-5 also seems (from prior experience with
    th1ish) reasonable (but also means vacuuming will rarely trigger
    because most scopes don't run long enough to trigger that, but
    that's generally okay because scope cleanup is even more thorough
    than vacuuming).

[^57]:  *Potentially* O(N) for strings, where N is some relatively small
    number based on the number of strings in the recycle bin modulo some
    small padding number. Strings padding is described
    [elsewhere in this document](#internals-string-padding).

[^58]:  20160303: the best i've measured to date is 1 allocation per 5.4
    lines of code (for a given definition of "line of code", of course).

[^59]:  At least one recycling optimization uses sizeof()s which may
    differ between platforms, potentially causing slight variations in
    the amount of memory recycled.

[^60]:  HOLY COW!

[^67]: When using [s2sh](s2sh.md): the `-S` and `--S` flags turn this
    on and off, respectively.
