# whcl: Grammar
##### ([&#x2b11;Table of Contents](./))
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

- [Core Grammar Constructs](#grammar)
  - [Identifiers](#grammar-identifiers)
  - [Dereferencing Variables](#grammar-deref-vars)
  - [Dereferencing Object Properties](#grammar-deref-props)
  - [Commands](#grammar-commands)
     - [Chaining Commands](#grammar-commands-chaining)
  - [Numeric Literals](#grammar-numbers)
  - [Strings](#grammar-strings)
  - [Operators](#grammar-operators)
  - [Code Comments](#grammar-comments)
  - [Math/Logic Expressions](#grammar-expr)
  - [@rray Expansion](#expando) (a.k.a. "expandos")
  - [Everything Else](#grammar-everything-else)
- ["Lines" of Script](#lines)
- [Expressions vs Commands](#expr-vs-command)

See also: [symbol resolution](symbol-resolution.md)

<a id='grammar'></a>
Grammar
============================================================

WHCL, a.k.a. whcl, like its namesake TCL, is "command-oriented." In
this regard it's much like a Unix-style shell. Its overall grammar is
extremely simple:

```shell
command ...arguments
command ...arguments
command ...arguments
```

Each command lives on a single line, but the definition of "line"
is slightly fluid and can depend on the context. Before we cover that,
a brief overview of arguments:

WHCL reads commands as a series of delimited tokens. It attempts to
match a well-known concrete type to each token (e.g. "identifier" or
"decimal integer"). Failing that, it will complain loudly.

For the most part tokens are space-delimited, but this is not always
strictly the case. Many cases permit that two or more tokens which
"touch" (i.e. have no itervening spaces) will be recognized as
multiple tokens. For example, `x[y]` is _initially_ 4 individual
tokens but the compilation process restructures them into 3: `x`,
`[]`, and `y`. For this particular case, spaces inside the `[...]`
_are_ permitted, but adding a space between the `x` and `[`
drastically change the semantics: the former is an [object property
lookup](#grammar-deref-props) and the latter is an `x` token followed
by a nested [command call](#grammar-commands).

The various types of tokens are...

<a id='grammar-identifiers'></a>
Identifiers
------------------------------------------------------------

**Identifiers** are resolved like a plain string except at the start
of a command, in which case they're interpreted as a command
name. Identifiers in WHCL may contain ASCII alphanumeric and
underscore characters, as well as dashes (ASCII 45dec), plus _any_
UTF-8 characters from outside of the ASCII range (😀, 🤫, 😰, 😻, and
✊ are valid identifier characters). They may contain dashes but
"really should not" contain sequential dashes, and a name made up of
only dashes is not valid, to avoid potential conflicts with
operators. (The grammar _may_ change to disallow more than one dash in
a row at some point.)

_Some_ contexts permit identifiers to begin with a `-`, but most do
not:

```whcl
decl -x 1; # will fail
some-command -x ...; # is okay: -x is treated as a "flag" token here.
```

<a id='grammar-deref-vars'></a>
Dereferencing Variables
------------------------------------------------------------

Dereferencing a variable - resolving to its value - requires
prepending its name (an identifier) with a dollar sign: `$varName`.
In some contexts the `$` prefix is optional.

Referencing a non-existing variable triggers an error. Variables must
always be declared before use.

_Please_ read the [symbol resolution doc](symbol-resolution.md).

`$varName` may also refer to [a built-in value](builtins.md#bivs),
in which case it evaluates exactly as it would without the `$`.


<a id='grammar-deref-props'></a>
Dereferencing Object Properties
------------------------------------------------------------

Properties of object-type and array-type values are accessed
using one of the following syntaxes:

First, `varName[X]` and `$varName[X]` are equivalent and search for
property `X` in the value specified by the variable `$varName`.
Specifics and limitations:

- The opening brace must "touch" the LHS token - no space is permitted
  between them. *Almost* all types of value support this
  operation. Even the base-most data types, e.g. numbers, derive from
  a class, and this operation will look up class members for values
  which have no properties of their own.
- The **LHS** may be a `varName`, `$varDereference` (same effect for
  both), a `[commmand call]` block, a literal value (number or quoted
  string), or [a built-in value](builtins.md#bivs) (noting that
  property access only works for some builtin values).
- The contents of the `[...]` must be a single token which resolves
  to a property key (noting that _almost_ any data type can be used
  as a property key). If it is a command, it must be wrapped up like
  a nested command call, e.g. `foo[[the command]]`.
  - For array-type values, a `[...]` value of type integer will
    resolve only to an array list entry, not an integer-keyed
    property.
- The `$` is optional because the existence of neighboring
  identifier and `[...]` tokens unambiguously describes this as a
  property access operation. (This differs strongly from TCL,
  where `[...]` is _only_ a command call and adjacency is used
  to create a single value from multiple touching tokens.)

> Design note: `[...]` was chosen over TCL-like `(...)` because,
  frankly, the latter is (A) too foreign/exotic and (B) more work to
  type (twice the keystrokes on a US-layout keyboard). The square
  brackets are more conventional for this use and easier on my
  eyes. The grammar "could" easily support both types of property
  access but supporting both would likely only add confusion.


Second, `varName.X` and `$varName.X` are equivalent, work like
`varname[x]`, and may be chained together with that syntax, but the
dot operator supports a different set of operands:
  
- The **LHS** may be: a built-in value, an identifier which resolves to a
  variable (with or without a leading `$`), a `[command call]`, or a
  quoted string.
  - Numeric literals are not permitted here, thought they are for
  `123[...]`, because the lower-level tokenizer sees `123.non-digit`
  as a malformed floating-point value.
- The **RHS** may be: an identifier, an identifier deref (`$x`), a
  quoted string, a `{...}` string, a heredoc, a literal integer, a
  `(...)` (evaled as an expression), or a `[...]` (evaluated as a call
  block). Floating point RHS is not permitted, to avoid any confusion
  caused by the ambiguity in the interpretation of `X.1.2` (`X[1][2]`
  vs `X[1.2]`). Floating points _can_ be used as property keys but are
  never recommended for use as keys, in any case, because (ironically)
  of their imprecision.

Note that array-type values accessed via an integer-type property key
interpret that key as an array index, not a property key.

> Sidebar: The `$` prefix being optional in some contexts was not an
initial design goal. Initially the `$` was always required to
disambiguate an identifier's role as an unquoted string vs. its role
as a named reference to a value. As the language evolved, it
eventually became possible to use other context to
disambiguate. Rather than outright remove the `$` where it's not
strictly needed, even though that would lead to cleaner-looking script
code, it is retained because it may well serve to help humans
understand the code better.

The third property access option is `.X` on its own, not touching an
LHS token, is interpreted as `W.X`, where `W` is the [currently-active
`with` block](builtins.md#bic-with). This syntax is _only_ valid with
a `with` block.

Examples:

```whcl
# Scalar (non-object type) variable:
decl x 1
echo x;  # outputs unquoted string x
echo $x; # outputs integer 1

# Object-type variable:
decl obj object
set obj[x]  1; # equivalent to...
set $obj[x] 1; # $ is optional in this context
set obj.x   1; # equivalent to...
set $obj.x  1; # $ is optional in this context

# In command-call contexts (see the Commands section):
x.rand  ; # calls the inherited 'rand' method
$x.rand ; # equivalent to previous line. $ is optional in this context.
x[rand] ; # equivalent to previous line. $ is optional in this context.
x rand  ; # equivalent to previous line but only
          # because of inherited command dispatcher.
          # $ is optional in this context.
$x rand ; # equivalent to previous line

# Literal values as the LHS of a property access,
# invoking inherited methods:
assert [0[rand]] > 0; # but 0.rand does not work - see above.
assert 'ABC' == ["abc".to-upper]
assert 'ABC' == ["abc"[to-upper]]
assert 'abc' == ["".concat a b c]

# "with" blocks:
with $obj {
  assert 1 == .x ; # refers to obj.x
}
```


<a id='grammar-commands'></a>
Commands
------------------------------------------------------------

Each logical line of whcl code starts with a command: an identifer,
a var dereference, or an object property access. For var dereferences,
the leading `$` is optional here because the fact that a token is at
the start of a line unambiguously makes it a command and the token will
resolve as an identifier (the name of a variable or built-in command):

```whcl
builtinCommand ...args
commandVarName ...args
obj[commandMethod] ...args
```

Additionally, almost everywhere the language expects a single token,
the results of a command may be used by wrapping the command in `[...]`.
For example:

```whcl
foreach k [some-command ...] {
}
```

The `[...]` bits are not needed when the command is on its own line,
only when it's embedded as a command argument or similar
contexts. Inside of `[...]` blocks, all newlines are ignored except
(due to a small deficiency in the parse) leading ones.

> Sidebar: in fact, `[...]` starting a line won't currently work
unless it's the LHS of a propery access. It "should" run, and then its
_result_ should be checked to see if it's a command/function to run,
then execute that, but it currently does not do so.

Note that in a property access context `[...]` has a different meaning
which trumps this one. The primary syntactic distinction between the
two is that property access `[...]` must "touch" its LHS operand. That
is, there must be no space between them. Similarly, a `[...]` which
invokes a function must _not_ touch its LHS. (This differs strongly
from TCL, where a call's output can be injected mid-word.)

> Design note: `$(...)` was initially chosen over `[...]` because of a
perceived ambiguity with property access. That perception turned out
to be false because the structure of property access makes it (in
whcl) unambiguously not a function call. Rather than have two syntaxes
for the same thing, `$(...)` was phased out.

<a id='grammar-commands-chaining'></a>
### Command Call Chaining

Call chaining is partially implemented:

```whcl
decl x "abc"
affirm "ABC" == [x to-upper]
affirm $x == [[x to-upper].to-lower]
affirm $x == [[x to-upper][to-lower]]
```

On the last two lines, note that the string-class method `to-lower` is
accessed using the property access approach instead of the [command
name dispatch approach](builtin-proc.md#calling-conventions):

```whcl
# Does not currently work:
affirm $x == [[x to-upper] to-lower]
```

That is currently a limitation of the evaluation engine and whether or
not that limitation will be lifted, such that the name-based dispatch
approach will/can work for this case, is under consideration. The full
implications of making that change are larger than just that one
side-effect and are not yet fully understood.


<a id='grammar-numbers'></a>
Numeric Literals
------------------------------------------------------------

**Integer literals** may take any of the following forms:

- Decimal
- Binary with leading `0b`
- Octal with leading `0o`
- Hex with leading `0x`

Integers may use `_` as a separator to improve readability,
e.g. `0b_1011_0111` and `0b100110111` are equivalent.

> Mini-achtung: if a numeric token has a leading `-` or `+` character
immediately adjacent to it (no tokens, not even spaces, between them)
then that character becomes part of the numeric token and not its
own token. This is significant for operations such as `[echo -3]`,
which will print out the expected `-3` instead of treating it like a
`-` token followed by a `3` token.

**Floating-point literals** are supported in standard standard
notation (no exponent notation) and have an *unspecified maximum
precision* (in short, whatever the local C library uses).

<a id='grammar-strings'></a>
Strings
------------------------------------------------------------

Strings may contain any valid UTF-8 and take the
following forms: 

- **"Double-quoted"** and **'single-quoted'**  
  Are semantically identical. Unlike TCL, no var or command expansion
  happens in these. They may span lines and all newlines are retained.
  Standard C-style unescaping of backslash-escaped characters is performed
  on these characters: `\b` `\t` `\n` `\r` `\f` `\v`. Unicode characters
  can optionally be encoded as `\u####` or `\U########`.
- **`{Squiggly blocks}`**  
  Are also strings but may contain only valid whcl tokens. No
  unescaping or whitespace stripping takes place. Some contexts
  interpret these as strings and some interpret them as script
  code.
- **Heredocs** come in two flavors:
  - **`<<<XXX heredoc XXX`** are stripped of all leading and trailing
    spaces unless a `:` appears immediately after the `<<<`, in which
    case only (at most) a single whitespace character is stripped from
    either end. No unescaping takes place. The `XXX` part may be an
    identifier or a single-/double-quoted string but must match
    byte-for-byte on both ends, including quoting. The trailing `XXX`
    need not be a standalone token: `<<<ABC defABC` is valid.
  - **`{{{ heredoc }}}`** functions identically, including the
    behavior of a colon immediately after the opening sequence, and is
    offered because it plays better with existing syntax highlighting
    and auto-indention modes than the previous syntax.


<a id='grammar-operators'></a>
Operators
------------------------------------------------------------

Being command-oriented, WHCL does not _directly_ support the so-called
"operators" conventional in most programming languages, e.g. `+`, `-`,
`/`, `*`, etc. Many operators are supported by the main tokenizer but
the main language treats them as unquoted string tokens. [The `expr`
command](builtin-expr.md) and certain ["expression" contexts](#expr-vs-command) apply more
conventional math or logic semantics to them. Client-side C functions
bound to WHCL will get see such arguments as strings.


<a id='grammar-comments'></a>
Comments
------------------------------------------------------------

Comments have two flavors:

- `#` denotes a comment until the end of the current line, _however_...
  For compatibility with TCL (so that we can use TCL syntax highlighting
  modes in our editors!), this only counts as a comment when it's the
  first non-whitespace token in a given line or follows a `;`. If it
  follows any other tokens, the `#` is retained as most operators
  are: it will be interpreted as a string in most contexts.
- `/* ... */` C-style comment block which may span lines.


<a id='grammar-expr'></a>
Math/Logic Expressions
------------------------------------------------------------

The language does not _directly_ support math and logical operators:
that's the domain of [the `expr` command][expr]. However, it offers a
shortcut: `(...)` is equivalent to `[expr ...]` except:

- At the start of a line.
- The [array command](builtins.md#bic-array) and [object
  command](builtins.md#bic-object) treat operands in the form
  `(...)` as a shorthand form for arrays.

In such cases, `expr` or (depending on the context) `[expr ...]` is
required.

<a id='expando'></a>
@rray Expansion ("expandos")
------------------------------------------------------------

The `@...` construct, colloquially known as an "expando," expands
list-type values into what amounts to individual value-type tokens (as
opposed to lower-level constructs like builtin commands or
`[call blocks]`). Because this construct requires context-specific
interaction with the evaluation engine, it is only supported in very
limited contexts, namely:

- An argument to a non-builtin command, including constructors invoked
  via [the `new` builtin command](builtin-new.md).
- An argument to [the `echo` builtin command](builtins.md#bic-echo).
- An argument to [the `array` builtin command](builtins.md#bic-array),
  which implicitly includes "inlined arrays" via [the `object` builtin
  command](builtins.md#bic-object).

The RHS of the `@...` must be immediately adjacent to the `@` (no
spaces between them are permitted) and must be any single token, noting
that:

- If the RHS is an identifier, it is treated as if it were prefixed
  with a `$`, i.e. it gets resolved as if it were a variable or
  [built-in value's](builtins.md#bivs) name. The `$` is still legal,
  but is optional.
- `object[property]` and `object.property` access is a single token
  from the `@`'s point of view.
- Block constructs are also effectively single tokens: `[...]`,
  `(...)`, and `{...}`, noting that...
  - `{...}` is evaluated as a code block (rather than a string) which
    runs in a new scope.

(More contexts may be added in the future as experience unveils useful
(and feasible) cases.)

After evalution of that RHS token, its value is expanded in place of
the `@` construct as follows:

- An array or tuple value expands to each of its values (_not_
  recursively), such that each one acts as if it were its own token
  (as opposed to part of a single list-type value).
- A non-array/tuple type is treated exactly as if the `@` were not
  there.

For example:

```whcl
decl a1 array 2 3     ; # array [2, 3]
decl a2 array 1 @a1 4 ; # array [1, 2, 3, 4]
echo x @a2 y          ; # ==> x 1 2 3 4 y
                        # As opposed to:
echo x $a2 y          ; # ==> x [1, 2, 3, 4] y
```

<a id='grammar-everything-else'></a>
Everything Else
------------------------------------------------------------

Any token which does not unambiguously match another token's type will
trigger an error. This is in strong contrast to TCL, in which
*Everything Is A String*, but supporting such semantics is, it turns
out, rather untennable in conjunction with WHCL's/cwal's data type
model. If WHCL were more TCL-like, in the sense that all arguments are
parsed as strings and downstream interpretation is left to the user,
then WHCL could support TCL-like "plain word" symbols. As it is,
though, doing so within the confines of our type system requires
making so many special cases that all the fun is sucked right out of
it. (An honest effort was made early on in WHCL's development to
support TCL-style words, but it led only to frustration.)

<a id="lines"></a>
# Lines

WHCL is line-oriented. "Lines" follow any of these forms:

```whcl
# One line:
command arg arg arg arg

# One logical line:
command arg \
  arg arg \
  arg

# Also one logical line because of how {...} blocks resolve:
if {$x} {
  this is the 'if' body
} else {
  this is the 'else' body
}
# ^^^ That's actually _one_ command ('if') with 4 string
# arguments: {$x} {...} else {...}

# Similarly, {...}, [...], and (...) blocks do not require
# backslash-escaping of EOLs:
command arg1 [other-command
  subarg
  subarg2
] arg3

# ^^^ is equivalent to:
command arg1 [other-command subarg subarg2] arg3

# Semicolons can optionally end a command:
command arg1; command arg2; command arg3
# ^^^^ three separate logical lines of commands
```

Some examples of illegal constructs:

```whcl
# Illegal because it spans lines:
if {...}
{ ... 'if' body ... }
# ^^^ that's actually 2 commands, but the 'if' command requires
# at least two arguments so its evaluation would fail.

while {true}
{ broken for the same reason as the 'if' above }

set x
  1
# ^^^ again, two lines. Each command must be on a single logical
# line, so this reformulation works:
set x \
  1
```

<a id="expr-vs-command"></a>
# Expressions vs Commands

This documentation will often use the terms "expression" and
"command", the two being distinctly different constructs in WHCL. In
short:

- A *command* is what starts each non-comment line of a script or
  each `[command ...]` construct.
- An *expression* is a math or logical construct as evaluated by [the
  `expr` command][expr]. Many commands unrelated to `expr`
  treat certain constructs as expressions, but `expr` is the command
  which directly exposes that functionality to clients.

In the context of this documentation, in particular in syntax diagrams
and code examples, *expression* is often denoted as `EXPR`. Such a
notation simply means that that section of code is interpreted
as described for [the `expr` command][expr].


[expr]: builtin-expr.md
