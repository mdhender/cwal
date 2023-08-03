# s2: TODOs
#### ([&#x2b11;Table of Contents](./))
# s2/cwal TODOs

The obligatory list of TODOs and such…

Jump to...

* ["Definite" TODOs](#todos-definite)
* [Potential TODOs](#todos-potential)

<a id="todos-definite"></a>
# "Definite" TODOs

(For a given value of "definite," of course.)

-   <s>We need a way of cancelling/interrupting scripts. e.g. a flag
    which triggers a fatal error. The only real effort there is then
    making sure that we check for that everywhere which needs it. Use
    cases: (1) Ctrl-C signal handler in s2sh, (2) providing a way for
    clients to break apparently infinite loops and long-running scripts.
    (My own experimentation in trying to heuristically determine if the
    currently running loop is "infinite," and bail out if it is, have
    failed - is there a generic heuristic which can determine if a loop
    is infinite without having to know the exact contents of the
    loop?)</s>\  
    Basic support for interrupting an engine via setting a flag in it,
    which "seems" to work when connecting it to Ctrl-C in s2sh. e.g. we
    can interrupt a script like (`while(true){}`), though sometimes it
    takes 2-3 tries to interrupt it (still working on catching the flag
    everywhere it needs to be checked, which is quite a lot of places).
    The flag basically works like a "trumping result code," meaning that
    it trumps the result code of the currently running internal API,
    getting propagated back up the call stack just like the `fatal` and
    `exit` keywords work (reminder to self: need to consolidate the
    handling of "non-error errors," e.g. return/exit/break, and
    interrupt, into the separate interrupt flag, to keep them all from
    potentially being trumped/overridden by certain unusual/hypothetical
    code constructs). 20160107: Experience has shown that it can fail to
    trigger in some cases, but finding out exactly where that is going
    on is difficult to do. e.g. `proc(){while(1);}()` can be
    interrupted, but it might require several tries. Profiling is
    showing that the interruption check is called disproportionately
    often, so some optimizations are in order.


<a id="todos-potential"></a>
# Potential TODOs

-   20171114: i would really like a `X?=Y` operator to complement
    `X?:Y`. It would mean "if the LHS (assignment target) has the
    undefined value then assign it to the RHS, otherwise short-circuit
    the RHS". This would require not only operator-level support, but
    also some work in the main eval loop for the short-circuit
    behaviour. An implementation was created in early 2020 but without
    proper short-circuiting behaviour it cannot skip over function
    calls.

-   20160225: we might be able to implement Groovy-style (a?.b?.c) in
    terms of skip-mode, by treating ?. as a unary postfix
    short-circuiting operator with dot-op precedence, left-associative,
    which increments the skip level if its LHS is not dereffable. The
    real trick, i think, would be getting back *out* of skip-mode at the
    right time. OTOH, it could be implemented as a plain binary infix
    operator far more easily, but then short-circuiting could not be
    implemented and its RHS parts could have side-effects even when the
    LHS parts are undefined. e.g. in foo?.(bar())?.baz, bar() would be
    called even if foo was undefined because both the LHS and RHS of
    (foo DOT bar()) have to be resolved before the operator can be
    triggered. The possibility of side-effects seems highly undesirable
    even though, in practice, constructs such that one are "never" used
    (only in sample/test code, really, but the language allows them, so
    they need to be accounted for).

-   20160220: would adding refcounts to `cwal_kvp` make it possible to
    safely modify Objects during traversal (with the caveat that
    iteration might or might not traverse any new properties)? It would
    end up costing an arm and a leg, though. Hm… the flags are currently
    16 bits, so we could get away with a 16-bit refcount without
    additional `sizeof(cwal_kvp)`. For the things we'd need refcounts
    for, we could live with that: they'd likely only be needed during
    traversal/modification. Internally, on a modification, we'd need to
    move the older kvp out of the way somewhere until after iteration,
    when we would unref it. Note quite sure what that would need to look
    like. We'd also need(?) copy-on-write for KVPs, but that seemingly
    can't(?) work because we expose KVPs to client code, and two
    iterators working on the same list might be referencing the same
    KVP in the moment when an assignment op CoW's it, leaving the client
    code holding the ref. Perhaps we need to extend the client-side KVP
    API to include the ref management, so they deal with them like they
    do values? Hmmm.

-   Potential function sugar: `proc(a,b)->a+b`\  
    i.e. `->` followed by a single expression, which translates to
    {return EXPR}. `proc()->{...}` could be used to mean (essentially)
    that the function's implicit result is that of the {script} block.
    Alternately: proc() expr; could be used to do the same thing.
    -   Similarly, possibly, like JS6, `(...)=>{...}` as another way
        of defining anonymous funcs. This is unlikely to happen
        because it would require adding a token-lookahead operation to
        almost every parens-group token to see if the next token is
        `=>`, and that would be unduly costly. OTOH, we can tell the
        tokenizer to re-use a peeked token as the next token, which
        allows it to bypass all parsing of the token, so it might not
        be all that expensive. (It's like a put-back but retains all
        of the token's state, saving parsing effort.)

-   <s>cwal core: when variable lookup finds no value in the current
    scope, but finds a const in a parent scope, import a (const) var in
    the local scope pointing back to it (transparently, as part of the
    getter operation, and ignore allocation failures on the assignment).
    We can't do this with non-consts, but consts should be safe for this
    as long as C-level code does nothing unsightly with them. This would
    confuse typeinfo(islocal) for such refs, though.</s>\  
    This was attempted, but the potential for side-effects and/or
    confusion (e.g.  via typeinfo and declaring of local symbols with
    the same names) was too great. Reminder to self: we could set a
    (new) `cwal_kvp` flag, e.g. `CWAL_VAR_F_SHADOW`, which tells us
    that such symbols are imported from older scopes and it's okay to
    redeclare them. That might be reasonable.

-   <s>s2 should be clever enough to see that if one has overloaded
    'operator+', then it could derive functionality for operator+= from
    that. It doesn't currently do so. In theory it could simply apply
    the non-assignment arg then assign that result back to the original
    (which is what overloaded assignment enable do when they "return
    this" from their operator). This applies to all of the
    assignment-combo operators, e.g. +=, -=, \*=, etc.</s>\  
    Proof of concept code is in place which does indeed derive and
    call the overloaded ops, but differences in return semantics are
    making that seemingly impossible to consolidate. operator+ wants
    to return a new value, but += needs to return 'this' for proper
    assignment semantics. Doing that as part of the operator+=
    synthesis is proving to be challenging (and breaks
    string+=string... aaha - because that overload necessarily returns
    non-this from the += operator (because strings are immutable)). So
    scratch that idea for the time being.  Nonetheless, a similar
    mechanism which is clever enough to derive, e.g. the operator
    &lt;= from the ops &lt; and ==, would simplify script-side
    overrides of those ops.
