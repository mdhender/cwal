# s2: Grammar
#### ([&#x2b11;Table of Contents](./))
# s2 Grammar

This section provides an overview of s2's built-in symbols, as well as
the rules regarding client-defined symbols (i.e. identifiers).

Jump to...

* [Terminology](#grammar-terminology)
* [Anatomy of an Expression](#grammar-expression-anatomy)
* [Identifiers and Strings](#grammar-identifiers-strings)
    * [String Syntaxes](#grammar-strings)
* [Built-in Constants](#grammar-builtin-constants)
* [Scopes and Scoping](#grammar-scopes)
* [Script-fatal Errors vs. Exceptions](#grammar-error-types)

Related topics covered in separate files:

* [Operators](operators.md)
* [Keywords](keywords.md), which includes flow-control: if/else and loops.


<a id="grammar-terminology"></a>
# Terminology

This documentation generally assumes some familiarity with at least
one other scripting language, but does not assume a detailed knowledge
of any of them. Anyone familiar with one or more scripting
environments will have no trouble following along.

Some terminology used throughout this documentation which might be
unfamiliar to readers (in no particular order):

-   **Expression** - a single value or a series of values and operators
    which resolve to a value. An empty expression (with no value) is
    allowed in some contexts and disallowed in others.
-   **LHS** - the *Left-Hand Side* of an expression, as delimited by an
    operator.
-   **RHS** - the *Right-Hand Side* of an expression, as delimited by an
    operator.
-   **EOX** - *end-of-expression*. Normally a semicolon or **EOF**
    (end-of-file). In s2, the end of any block-level construct or
    parenthesized group has an implicit virtual EOF, meaning that an
    explicit EOX is optional for its final expression.
-   **Boolean Context**: any point where the interpreter needs to reduce
    an answer to either "true" or "false," causing it to interpret the
    applicable result value as a boolean. Each data type has (mostly
    intuitive) rules for determining their boolean value.
-   **Truthy**: any expression which evaluates as *true* in a boolean
    context is "truthy." All others are…
-   **Falsy**: any expression which evaluates as *false* in a boolean
    context.
-   **Script**: source code (UTF-8 encoded) intended to be run by an s2
    interpreter, whether it be a file or a C string in memory. Normally
    contains a series of expressions, but an empty script is also valid.
-   **Garbage Collection (GC)**: a part of the script engine which
    cleans up values when it thinks it is safe to do so, either
    freeing their memory or (depending on configuration options and
    runtime conditions) making the memory available for recycling
    within the script engine. In s2, "temporary" values get created
    during expression evaluation, and the GC gets rid of those for us
    (normally very quickly). Temporaries, by the definition, cannot
    exist in script-visible space because making them visible to
    scripts inherently involves holding a reference to them.
-   **Throws**: this term comes primarily from the C++ world, where a
    function "throws on error." "Throws" simply means that an exception
    is thrown (or might be, depending on runtime conditions). s2
    currently has only one base exception type.
-   **Value**: all expressions in s2 resolve to a common type,
    generically known as the *Value* type. All values, regardless of
    their higher-level types, share certain common interfaces (such as
    the ability to determine whether they are truthy or falsy, and to
    compare them to one another). In C this refers to the `cwal_value`
    type, which is the abstract basis from which all other types
    derive. Some values, like boolean `true`/`false`, `null`, and
    `undefined`, are constant/immutable singletons, while (most) others
    require allocating a new instance of a higher-level type.
-   **Value Context**: any point in an expression where the interpreter
    expects to be given a Value (or possibly a unary prefix operator).
-   **Keyword**: a word reserved by the language which changes how the
    following token(s) is/are interpreted. In s2, unlike most other
    languages, keywords evaluate to a value and can be used in any Value
    Context. e.g. it's possible to use an `if/else` block any place where
    a boolean value is legal.
-   **Method**: is essentially an alias for *function*, but the term
    *method*, by long-standing convention, specifically means a
    function which is being used as a member property of an object.


<a id="grammar-expression-anatomy"></a>
# Anatomy of an Expression

s2 has no formal grammar chart which tells us what, precisely, is and
is not legal. It is an expression parser which linearly reads values
and operators, applying the operators to the values, following
conventional (C-like) operator precedence rules to govern their
processing order, with the exception that it also treats keywords as
expressions. Any "misplaced" tokens (values or operators) produce
parsing errors, whereas valid ones run through a stack machine. The
results, one will find, are rather conventional, and *mostly*
intuitive to anyone who is familiar with any of the more common
JavaScript-like languages languages. With the exception of its [symbol
lookup rules](symbol-resolution.md), there should be few surprises to
anyone with experience with at least a couple programming
environments.

## Non-symbols and Expression Boundaries

s2 generally treats any whitespace, newlines, and comments as
insignificant non-symbols, with the exception that it recognizes
end-of-line (EOL) as an expression terminator (a.k.a. EOX: end of
expression) in some *very few* special cases. End-of-file (EOF) is
always treated as an expression terminator, so it may cause a syntax
error if encountered mid-expression. As a special exception, if the
*first* line of a script starts with "\#!", it is assumed to be a Unix
shebang line, and is skipped (up to and including the first newline
character).

*In theory* s2 handles both common end-of-line styles (`\n` and
`\r\n`) equivalently, but in truth nobody's tested the latter as of
this writing because… well, because Windows is *icky*. Newlines in
string literals will be retained as they are entered in the string
(that might or might not be a bug - hasn't been a problem so far, but
it could potentially be one).

s2 supports both C- and C++-style comments, which may appear essentially
anywhere in code:

```s2
// comment until end of line
/* comment until … */
```

> Sidebar: inside of comments, backslash escapes are not processed.
That means that a `//`-style comment which ends in a backslash
followed by a newline does *not*, contrary to the C standard, combine
the next line with the comment.

When evaluating, comments are ignored as if they were spaces. Newlines
are *almost* always treated as whitespace, with the exception that *some*
block-level constructs allow (for convenience and convention) a
newline to act as an end-of-expression. Here is an example of the effect
of semicolons and newlines around an expression:

```s2
expr ; // === expr
expr <NEWLINE>;<NEWLINE> // === expr, no matter how many newlines surround ';'
expr ; /* === expr */ ; // === <NULL> see below
```

A second semicolon "deletes" the current pending expression result
value. A single semicolon, along with any number of newlines, or a
series of newlines without a semicolon, retain the pending expression's
value. This is more notable in eval/scope blocks, where too many
semicolons can cause it to "lose" its result value, causing the
eval/scope block to evaluate to the undefined value (but that can also
be used to avoid propagating a local value out of a scope). While an EOF
is implicitly an EOX, only semicolons are counted for purposes of
tracking the current expression result. i.e. an EOF in place of any of
those semicolons except that very last one has no effect on the result
of the expression, and replacing the last semicolon with an EOF would
cause the script to evaluate to the result of the expression on the last
line.

s2 evaluates parenthesis/brace groups and {script} blocks as atomic
tokens, a side-effect of which is that such blocks end (from the
perspective of the sub-parser) at a virtual EOF at the closing
parenthesis/brace/bracket. Because EOF is treated as an implicit EOX, a
semicolon is optional for the final expression at the end of any such
construct (and might even be a syntax error, depending on the context).

> Design note regarding semicolons: s2 initially used an EOL as an
optional expression terminator. This was comfortable to use, but
caused several annoying limitations in the placement of operators
(namely, that binary and ternary ops required their LHS to be on the
same line as the operator). To get around such limitations, without
requiring special-case lookahead in many places, and allow the removal
of a good deal of special-case handling of EOLs, it was changed to
require semicolons to end expressions, with a small handful of cases
where it special-cases EOL as EOX because (let's face it) nobody
really wants to have to end their if/while/for/scope blocks with a
semicolon. This switch, in hindsight, was the right thing to do, as it
allows much more flexibility in script coding style than the
EOL-as-EOX world does, provides better predictability for those
writing script code, and makes the parser more robust.


<a id="grammar-identifiers-strings"></a>
# Identifiers and Strings

s2 supports only UTF-8 inputs. Any input scripts and their script-side
strings may contain arbitrary UTF-8. Identifiers in s2 are
case-sensitive and may consist of:

-   Any conventional C-style identifier character: letters, underscores,
    digits 0-9.
-   The dollar sign (`$`).
-   *Any* UTF-8 character with two or more bytes (i.e. outside the ASCII
    range). Thus the UTF-8 ellipses (…) is a legal identifier character(!),
    as are any non-ASCII whitespace characters, ↻, Æ, ©, π, etc. Have
    fun with that ☺ (that's a legal identifier character, too, by the
    way ;).
-   Must start with a non-numeric character.

s2 expects its UTF-8 inputs to be well-formed. When encountering invalid
UTF-8, it will stop or skip whatever it's doing with those bytes,
possibly silently, and behaviour with non-UTF-8 encodings is essentially
undefined (but "should" never outright crash, corrupt memory, or
similar). When client C code creates string values (using
`cwal_new_string()` and the various printf-style string generators), it
is up to that client code to ensure that the string is legal UTF-8. cwal
provides `cwal_utf8_read_char()`, which reads a single UTF-8 character
from a C-string, and that's the routine the library uses to traverse
UTF-8 internally.

<a id="grammar-strings"></a>
## String Syntaxes

s2 supports several string syntaxes: single-quoted, double-quoted, and
heredocs.

`"double-quoted"` and `'single-quoted'` strings are *functionally
identical*:

- s2 *never* performs any sort of automatic symbol expansion on string
  content other than backslash unescaping.
- Treated to conventional C-like backslash unescaping, plus `\uXXXX`
 and `\UXXXXXXXX` Unicode unescaping. The following "escape sequences"
 get "unescaped" when a quoted string is processed :
    - `\b` (backspace)
    - `\t` (tab)
    - `\n` (newline)
    - `\r` (carriage return)
    - `\f` (form feed)
    - `\v` (vertical tab)
    - `\0` (zero ⇒ unescapes to a *NUL* byte)
    - `\\` (unescapes to a single backslash)
    - `'` and `"`, *regardless* of which type of quotes contain the
    string.
- All other characters (including a *NUL* byte or a single slash
 appearing at the end of the input string) treat a preceding backslash
 as if it is a normal character (they retain it). The reason for this
 is to avoid that client script code (e.g. the [POSIX Regex
 module](../mod/regex_posix/) has to double-escape strings
 which have to be escaped for underlying script-bound C libraries.

***Heredocs*** come in two forms, the first being `<<<IDENTIFIER
content IDENTIFIER`.  They are treated like string literals, but are
not subject to *any* form of unescaping (but see the `unescape()`
string method) and have "trimming" behaviour described below. They
behave *mostly* like heredocs in other languages:

-   The opening "tag" must syntactically be a series of identifier
 characters or a single- or double-quoted string. The first instance
 of that *identical byte sequence* after the opening tag ends the
 heredoc. When using quoted strings as tags, they must match
 byte-for-byte, *including their quotes*, and the quoted heredoc
 markers are are not subject to any unescaping rules. e.g.
 (`<<<"!\r?\n$" … "!\r?\n$"`) is a valid heredoc.
- Unlike most languages, the closing tag need not start on a word
 boundary. Any bytes which match the opening identifier *exactly*
 will close the heredoc. e.g. (`<<<X hereX`) === "here".
- All leading whitespace after the opening identifier is trimmed.
 Trailing whitespace, if any, before the closing tag is also
 trimmed.
- A single colon character between the `<<<` and the
 identifier change the whitespace policy somewhat: only one
 leading or trailing whitespace character is trimmed. e.g.
 (`<<<:X y <many spaces>  X`) would retain all but one of the leading/trailing
 spaces or newlines. This allows more control over output formatting in some use cases.
- No interpretation/unescaping is performed on the heredoc content
 except that it is assumed to be valid UTF-8 text.
- After creation, a heredoc is internally treated exactly like any
 other string literal, and can be used anywhere a string may be
 used.
- In addition, they can be used in many places where a {script}
 block is expected (e.g. as the body block for a control
 structure which uses a squiggly string) because internally
 they are the same type after tokenization is done with them.
 One notable exception: heredocs are never treated as object
 literals.
     - That property/behaviour is a historical artifact of the
     copy/pasting of tokenization code from th1ish to s2 early on, and
     is now deprecated. It is rarely used and *might* be removed
     someday. OTOH, it has one obscure use when writing large `tmpl()`
     templates (specifically: it can be used to improve readability a
     small bit when control blocks span template blocks, by giving the
     control block start/end symbolic tags via a heredoc).

Heredocs have a second syntax which plays better with syntax
highlighting and auto-indention modes: `{{{heredoc}}}`. Examples:

```
{{{ blah }}};
{{{
   spaces are stripped
   as for the <<< heredoc form
}}};
{{{: colon works as for the <<<: form. }}}
```


<a id="grammar-builtin-constants"></a>
# Built-in Constants

The following constant values are implemented as keywords, and behave
like any other values:

-   `true` and `false` are the conventional boolean constants. Their
    typename is "bool". While, technically speaking, in a numeric
    context `false` is 0 and `true` is "non-0," cwal explicitly
    guarantees that `true` evaluates to 1 in a numeric context.
-   `null` is a generic placeholder for a "non-value." Its typename is
    "null". In practice, null is used relatively seldomly, as the next
    constant suits that role better quite often.
-   `undefined` is the default value for essentially everything, and
    is often used to mean "no such value." Its typename is
    "undefined".  In JSON output, it is either converted to `null` or,
    depending on the context, elided altogether because JSON does not
    know about `undefined`. Some documentation, in particular for
    functions, refers to a `void` type, and such references really
    mean "the `undefined` value" (the name `void` comes from C).

The integer values (-1, 0, 1), double values (-1.0, 0.0, 1.0),
length-0 strings, and all length-1 ASCII strings (byte values 0 to 127,
inclusive) are also constant/shared values which, like the above-listed
constants, never require allocation and do not partake in lifetime
management. i.e. `''`, `""`, and `<<<EOF EOF` (all empty strings)
all refer to the exact same, shared C-level empty string as well as the same
cwal-level Value instance (and, despite that, they all have a refcount
of 0 because builtins do not partake in lifetime management!).

> Sidebar: cwal can, and normally does, compile a larger range of
integer (but not double) values in as built-in constants. Looking at
the source code right this minute, it seems i went overboard and
configured it for the inclusive range [-10,20]. Those interested in
the details should take a peek at [cwal.c](/finfo/cwal.c) and search
for `CWAL_BUILTIN_INT_FIRST` - a reasonable amount of documentation
can be found there.

<a id="grammar-scopes"></a>
# Scopes and Scoping

Scopes are containers for variables and also play a major role in the
lifetime management of non-variables (temporary/unnamed Values, as
well as C-side Values not visible from script code). Scopes are
organized in a stack and are created implicitly by almost all
block-level constructs, including array- and object-literals. Scopes
use [objects](./type-object.md) as property storage for scope-level
variables, so share the same search performance characteristics.

s2 always, except in the case of an abnormal C-level exit (e.g. a
C-level `assert()` failure crashes the app, C's `exit()` or `abort()`
functions are called, or the s2 client fails to clean up the s2 engine
instance), unwinds its scope stack, even in the case of a syntax error,
the s2 `exit` or `fatal` [keywords](keywords.md) are used, or when a script-side
assertion fails. The primary side-effect of this is that *all*
finalizers[^1] for *all* values will get called even in the face of
s2-level `exit`/`fatal`/`assert`. This is of note mainly for Native
bindings which require proper finalization for correct behaviour (which
effectively means any non-trivial types which are worth scripting). Not
all scripting engines which allow client-defined finalizers guaranty
that they will ever be called, and that was, in fact, one of the
catalysts which got the cwal project started (after several years of
bikeshedding about the GC model). (Specifically, the Google V8
JavaScript engine essentially never calls them, and its developers
justify that behaviour as being a valid speed optimization for their
engine. Nevermind that it's *semantically incorrect* to *not* properly
finalize a file or database handle if you expect the underlying
resources to behave properly.)

## Where Scopes are Created

This list describes every(?) place where s2 creates a scope:

-   s2's lifetime always starts with a single scope (the
    **top-most/global scope**).
-   The following **eval-family keywords**: `catch`, `scope`
-   The bodies of **object literals**, including arrays, hashes, tuples,
    and enums.
-   **Object property access** in the forms `x[y]` and `x.(y)`. These
    require new scopes in order to prevent them from, e.g., creating
    new variables in the invoking scope: `x[var y = 'hi'] =1` is
    perfectly legal but we don't want that `y` to exist outside of the
    property access operator. Prior to 2021-06-24 these operations did
    not use a new scope.
-   **Function calls** push a scope at the opening `(`. This means that
    temporary values created as parameters are cleaned up before the
    call returns. Native functions may push their own scopes (but
    essentially never need to do so).
    -   Hypothetical corner case: if the parameters contain a var decl
        and the function body tries to declare a same-named var, it
        will fail. If that ever actually happens, we can push another
        scope after processing the arguments or simply overwrite
        collisions. e.g.:
        ```s2
        proc(x){ var b /* will fail! Already declared → */}(var b = 7)
        ```
        Interestingly, that collision potential does not apply to
        [symbols imported into the function](type-function.md#type-function-importsymbols):
        imported symbols shadow such variables (as they should).
-   **Flow Control:**
    -   **if()**: at the opening `(`.
    -   **while()**: at the opening `(` and at the start of the loop
        body on each iteration.
    -   **do/while()**: at the start of the loop body on each iteration.
    -   **for(;;)**: at the opening `(`' and on each iteration right
        after the first ';'.
    -   **foreach()**: at the opening `(` and at the start of the body
        on each iteration.
-   Reminder to self: the function-like keywords (e.g. `exception()`) do
    not (or mostly do not) push a scope. Some of them cannot (some of
    *typeinfo()*'s functionality only works because it does *not* push a
    scope).

It seems that's about it.



<a id="grammar-error-types"></a>
# Script-fatal Errors vs. Exceptions

s2 is (at the C level) capable of maintaining error state as exceptions
(intended primarily for the script level) or non-exception errors
(intended solely for the C level), both with script location
information, an error code (integer), and a descriptive error string.
Non-exception errors stop the execution of the script entirely, whereas
exceptions can potentially be caught by script code and recovered from.
The current thinking is that we will convert C-level errors to
exceptions for all cases which we believe are (at least potentially)
recoverable from script code. i.e. when a syntax error happens inside of
a function imported from a separate script file, we don't necessarily
want the whole script to die. These details are being decided on the fly
as s2 is developed.

For the most part, s2 is capable of pinpointing error locations at the
exact symbols which trigger them, and exceptions provides line/column
and stack trace info to help narrow down the search. There is a case or
two where it will produce confusing results, namely when a
dynamically-constructed string gets *eval*'d but contains an error. In
such a case, the location information refers to a "virtual" script which
the client cannot see (and possibly no longer exists by the time the
*eval* returns), which (for error reporting purposes) starts at the
*eval* point.

# Footnotes

[^1]:  Internally, *all* value types have a finalizer (one shared by
all instances of that type). Client-defined Native bindings and
client-defined Function state can also have finalizers which are
called when the containing native/function is finalized by cwal.

