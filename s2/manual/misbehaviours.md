# s2: Known Misbehaviours
#### ([&#x2b11;Table of Contents](./))
# Known Misbehaviours


s2 doesn't generally misbehave, but when it does, it tends to express
itself in one of the following ways...

# Script Location Info in Second-round Eval

Script location information (as revealed by error reporting and the
`__FLC` family of keywords) from inside "second-round" evals, e.g.
`eval -> "error here"`, is just *so* completely off.
    
The problem is that the string being evaluated has no real relation,
at the tokenizer level, to the surrounding source code, so there is no
realistic mapping of file/line/column info in the eval'd string.  It's
a corner case, in any case, and using `catch->` instead of `eval->`
can clear up some of the confusion (the general origin
of the error).

**Workaround:** use `Buffer.evalContents()` resp.
`String.evalContents()`, as those "anchor" the eval'ed code for
purposes of reporting error location information, and even
allow the caller to provide a symbolic name of their choosing
for the script.

Hmmm. Maybe we could/should just internally do that? We could name the
eval'd part after the current script location information, and
otherwise rely on the existing mechanisms. Hypothetically.

# 20160106: (Not a) Precedence malfunction

```
a===b && c = d
```


Errors out because it's apparently being parsed as: `(a===b && c)=d`.
Not yet sure where the bug is, but it also affects the `[]=` op on the
RHS of comparison. Oh, wait… JavaScript fails in the exact same
way.

***Not a bug :-D***. i'll leave this commentary here because i've
forgotten and "rediscovered" this "problem" at least three times now,
and want to have this handy for the next time.

# 20171112: ***serious lifetime bug*** (hashtag: #EvalHolder)

**(resolved, but left here as a reminder-to-self for (much) later)**

```
./s2sh --S --a -v --R crash.s2
```

with this `crash.s2`:

```
if((var d=s2.getenv('HOME')) && (d = s2.fs.realpath(d+'/fossil'))) 0;
```

It's crashing (cwal-level *assert()* failing) in the `&&` operator due
to a lifetime problem related to the `d` value (possibly getting
hosed via the 2nd assignment?). Running with recycling ON (`s2sh -R`
instead of `--R`) hides the problem but potentially causes Weird
Broken Stuff to happen later (as is being witnessed in the CGI
plugin, where this problem was initially triggered).

This is the first serious lifetime-related bug we've seen the s2
core since 2014 :(.

Test code snippets:

```
(var d = 'a b c d e f g') && (d = d + 'h i j k') // crashes
(var a = 30) && (a = a + 10) // works
(var a = '30') && (a = a + '10') // crashes
(var d = 'a b c d e f g') && (d = 'h i j k') // crashes
(var a = 'x') && (a = 'x') // crashes
(var a = 'x') + (a = 'y') // crashes
(var a = 'x'), (a = 'y'), a // works. Sometimes.
(var a = 'x'), (a = a + 'y'), a // crashes
(var a = 30), (a = a + 20), a // works
```

With the caveat that "works" really means "appears to work, at
least under certain combinations of recycling and string interning".
The failure is very picky about the engine's current recycling
state, and a case which breaks in one line of script might work in
the next. That's a classic value lifetime corruption symptom.

**So…** the common element appears to be assigning to the same
*string* value on the LHS and RHS of a binary operator.
(Interestingly, string interning is *not* the sole culprit this
time.)

***This has been adequately worked around*** in the s2 core by
re-enabling an old internal toggle (the so-called "eval holder") which
takes an explicit reference to all values currently on the evaluation
stack. That costs us an extra array internally for each scope, but it
is a viable solution. If/when i can figure out how to keep 'd' alive
without that (and can figure out why it's dying off: the var should be
keep it alive), the eval holder can be disabled.  This workaround
costs a significant number of allocations, especially if recycling is
disabled, but the only other solution (unless i'm sorely mistaken) is
to completely rework how the stack machine manages value refs
(currently it doesn't hold any refs, leaving that to higher-level
code… like this "eval holder"). Refs could be held more cheaply that
way, but (A) we would lose vacuum-safety of being-eval'd values and
(B) managing those refs would require *very careful* attention in many
places. ((A) is theoretically not a problem because we never
sweep/vacuum in mid-expression (because, incidentally, of these
"free-floating, ref-less" values).) Maybe someday it'll seem worth
trying out, but today is not that day.
