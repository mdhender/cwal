# s2: Processing Parentheses and Braces
#### ([&#x2b11;Table of Contents](./))
# Processing Parentheses and Braces in s2

This page offers a discussion about how s2 processes parentheses and
brace groups, i.e. `(...)` and `[...]`. This is in no way required
knowledge for using s2, and was written simply because i've yet to see
this approach discussed in any literature about parsing. While i
won't quite go so far as to claim that s2's approach is new or unique,
it is certainly a bit exotic even through it produces the exact same
results as more conventional approaches.

This discussion will attempt to persuade you, dear reader, that there
is indeed more than one viable approach to processing nested
constructs like parenthesis and braces. This document assumes some
basic familiarity with scripting-engine concepts like stack machines
and terms like "LHS".

> First, a note about terminology: people commonly use different terms
for `()`, `[]`, and `{}` characters. In s2 parlance, `()` are
"parentheses" (or "parens"), `[]` are called "braces", and `{}` are
called "squigglies" (yes, *squigglies*).

Every piece of literature i've read discussing parsing treats
parentheses as operators or something approximating them. Whether or
not this is a side effect of [the Shunting Yard algorithm describing
them as such](https://en.wikipedia.org/wiki/Shunting-yard_algorithm)
is anyone's guess. Given an expression like:

```c
1 + (2 + 3)
```

That will conventionally parse/evalate something like:

- Push value 1 onto the stack.
- Push operator +.
- Push the left parenthesis.
    - push 2
    - push +
    - push 3
- Encounter the closing paren.
    - Process the stack until we backtrack to the opening paren, which we pop.
    - The stack machine now contains: `1 + 5`
- ...

The significant thing there is that the parens are effectively
treated like operators.

s2's handling of what it calls "block constructs," meaning `()`, `[]`,
and `{}`, is subtlely different from that conventional approach, and
is handled the same for each type of block. When it encounters such a
group, it does the following:

1. The tokenizer, when it comes across an opening `(`, `[`, or `{`,
   "slurps"[^1] up the whole contents of that block *into a single
   token*. Thus an expression like `1+(2+(3*4))` is, during the early
   stages of evaluation, only *3* tokens: `1` and `+` and `(2+(3*4))`.
   (Pedantically speaking, it's actually only 1 token at a time. By
   the time the single `(2+(3*4))` token is received by the eval
   engine, the tokens `1` and `+` have already been processed and
   transformed into their higher-level forms, namely a script-engine
   value (the number `1`) and an operator, respectively.)

2. The main evaluation engine, when it sees such a construct, has no
   idea what's inside it. It only knows that it's a block-level
   construct and which type of block it is. Depending on the exact
   context, the eval engine does one of the following with such
   blocks:
   - If the LHS of `(...)` is a Function, it passes the function and `(...)` off to
   the function call handler.
   - If the LHS of `(...)` is an operator or appears at the start of
   an expression, it recurses into the block and resolves to a single
   value - the block's internal expression result.
   - If the LHS of `[...]` or `{...}` is an operator or appears at the
   start of an expression, it hands these off to the routines which parse literal arrays
   resp. objects.
   - If the LHS of `[...]` is a value, it recursively evaluates the
   contents of the block and transforms `LHS[RHS]` to `LHS
   . RHSResult` so that the dot operator can do the real work.

Most other cases are syntax errors, barring special-case handling like
the array-append pseudo-operator `anArray[]=x`. Constructs like `scope
{...}` are passed on to keyword-specific parsers which handle their
operands in various ways. e.g. `scope {...}` treats the contents of
that block as a script and simply passes its contents (without the `{`
and `}` parts) through the main tokenization/eval engine.

The main up-sides to this handling are, IMO:

1. Conceptual simplicity. Each block is treated as an opaque blob
   until/unless it's handled by code which exists to handle its
   contents. The main eval engine never concerns itself with the
   question of "what's in the block?" though the *contents* of each
   block, minus the block part, will eventually be passed into it for
   processing. The one exception is that translation of `X[Y]` to
   `X.Y` requires that the eval engine acknowledge the `[...]` part,
   which it handles by recursing into the block's contents, as opposed
   to evaluating it in the stack machine's current context. Appropos...

2. Simple stack handling. When s2's main eval engine is entered, it
   starts a pair of call-local stacks for values and operators. Any
   recursive calls, including evaluating the contents of a block
   group, have their own stacks. This makes error recovery of the
   stack trival by having the eval routine, on error, simply discard
   its call-local stacks, leaving the stacks of high-level evaluation
   calls (from which this one was recursed into) intact.  We don't
   need to "unwind" the stack to a safe point on error - we can simply
   discard it, and when the recursed eval call returns back up the
   call stack, those recursing routines all have their own stacks,
   still intact. This approach inherently eliminates the possiblity of
   corruption of the eval stack across calls into/through the eval
   engine and, incidentally (yet significantly), allows the internal
   eval call structure to easily align with cwal's scope stacks (an
   important aspect of lifetime management for eval results).

3. Short-circuit implementation is simplified. When short-circuiting
   (internally known as *skip-mode*) is in effect, a block is skipped
   over by simply discarding that single block-level token.
   
4. Recursive tokenization is easy and "range-safe." Each tokenizer is
   given a range of bytes to tokenize and it only navigates within
   that range. Recursing into a block construct simply requires
   creating (on the C stack) a new tokenizer and giving it the
   start/end bytes of the block's contents (the range is 0 bytes for a
   completely empty block). This does not require allocating any new
   memory: the stack-allocated sub-tokenizer points to bytes in its
   parent's range. When a tokenizer reaches the end of its defined
   input range, it reports that by giving the caller an "EOF" (end of
   file) token. Thus all block constructs in s2 have a virtual EOF
   where their closing character is, and tokenizing a block component
   cannot "overrun" into a parent tokenizer's byte range, even though
   those ranges physically overlap. The core eval engine neither knows
   nor cares whether it is evaluating "top-level" or "block-level"
   code, as they both tokenize identically, including the use of an
   EOF token to denote that the end has been reached.

Interestingly, there are *no semantic differences* between the results
of this approach and the more conventional approach of treating
parentheses as operators.

> Sidebar: to be honest, when this model was first implemented (in
s2's immediate predecessor), i wasn't absolutely sure that the
semantics would align 100% with conventional usage, and was prepared
to document any differences as potential gotchas, but was certain that
it would be easier to deal with in the eval engine framework i had in
mind. This approach has since proven to be semantically equivalent to
the more conventional approach.

The most notable down-side (perhaps the only notable one) is, quite
simply, tokenization inefficiency. s2 only tokenizes and evaluates,
insofar as possible, a single token at a time. Its eval engine reads
the next token and figures out how to handle it, then reads a token
and figures out how to handle it, *ad infinitum*. The engine has only
the raw bytes of a script's source code, no higher-level (and
memory-expensive) construct representing a script's contents. This
means, for example, that the tokenization of the contents of block
constructs normally has to be repeated, potentially many times:

1. To slurp the whole block into a single token. While it may
   initially sound quite efficient to slurp up a block construct by
   simply doing a byte-scan for the closing block character, a
   byte-scan does not suffice: the slurping process has to recursively
   tokenize the contents of a block in order to reliably find the
   closing character. e.g. when slurping up `(a ("b)"))`, the slurping
   process has to understand that the `)` in `"b)"` is not relevant
   for its purposes, and that can only be reliably achieved by
   properly tokenizing the block's contents, as opposed to simply
   byte-scanning it. Note, also, that tokenizing UTF-8 (s2's sole
   input format) is inherently less efficient than tokenizing a
   fixed-width text encoding like ASCII, UCS-2, or UCS-4.

2. Evaluating a block's contents requires recursing into them, which
   tokenizes them yet again, this time examining each token for
   evaluation, as opposed to just looking for a block-closing
   character. Each time a block's contents are evaluated, they get
   re-tokenized.

This inefficiency is marginal for small constructs like function
argument blocks or small object/array literals, but can be quite
costly for constructs like scopes, conditions, loops, and function
bodies, all of which can be (effectively) arbitrarily large[^2]. In
short, all s2 which is not at the top-most level of a given script
will (if it is executed) be tokenized at least twice.  The only s2
code which is ever tokenized only a single time lives in the top-most
section of a file (not in a block construct) or is never actually
executed, e.g. the `else` part of `if(true){...}else{...}`.  (To
reiterate, though: its approach is *time-inefficient* but is highly
*memory-efficient*.)

Tokenization in s2 is, by leaps and bounds, the single most
time-intensive component of the engine. The only practical way to
speed it up is to "compile" tokens into a higher-level form. Some
experimentation has been done with that, which uses the same
eval-one-token approach but retains tokenized parts in their
higher-level token form, as opposed to "losing" each token after it's
been traversed once. However, the memory costs of doing so go very
much against s2's core-most design goal of low memory usage. It's
possible that such a mechanism may be added as an option, or used only
on "hotspot" code (e.g. function bodies and loops), but it's not, as
of this writing (20200107) clear whether the deeply-embedded
tokenize-as-we-eval model can be ammended in this way without an
uncomfortably large overhaul of the tokenization API and, far more
onerously, the client code which uses it (that primarily means [the
entirety of `eval.c`](/finfo/s2/eval.c), which is easily the
single largest component of s2, containing at least a thousand uses of
the tokenization API). It's not a question of "is it possible?" but is
certainly one of "is it really worth the considerable effort?"


# Footnotes

[^1]: "slurp" is, in fact, the term internally used for converting a
    block-opening token into a block-level token which wraps the
    block's contents, with function names like `s2_slurp_braces()` and
    `s2_slurp_heredoc()`.

[^2]: 20200107: As part of the "token compilation" experimentation, in
    order to keep memory costs as low as possible, certain limits are
    starting to be imposed, e.g., a maximum size of 64kb for any
    single token (which means, by extension, any single block, since a
    block is initially treated like a single token). Even so, these
    limits are far out of the range of both current s2 uses and its
    intended uses. If they become problematic those ranges can be
    easily increased, noting that doing so will cost memory for every
    single compiled token.
