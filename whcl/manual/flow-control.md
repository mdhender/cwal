# whcl: Flow Control
##### ([&#x2b11;Table of Contents](./)) ([builtins](builtins.md))
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

Doc sections:

- [if/else](#if)
- [while loop](#while)
- [do-while loop](#do-while)
- [for loop](#for)
- [foreach](#foreach)

# Flow Control

The core WHCL language processor does not actually know about "flow
control" concepts such as conventional if/else branches and
for/while/do loops. Instead, these are delegated to commands.  The
conventional flow-control commands are, however, built-in commands, so
the distinction between "is" and "is not" part of the core language is
one of some fine hair-splitting. They "are not" a core feature in the
sense that any given one of them can be removed from the C code
without affecting any of the others or the core evaluation engine.
They "are" a core feature in that it's difficult to do much of
anything useful without them.

This page documents the currently-available flow-control constructs.

<a id='if'></a>
# What `if`...

If/else blocks look like:

```whcl
if {EXPR} {CODE}
if {EXPR} {CODE} else {CODE}
```

The `EXPR` part is evaluated [as an expression][expr] and `CODE`
part as "normal" script code. The `EXPR` may optionally be written as:

```whcl
if (EXPR) ...
if [command ...] ...
```

The `(EXPR)` form is semantically equivalent to `{EXPR}` and the
`[command...]` form is semantically equivalent to `{[command...]}`.

Another `if` block may follow each `else` part:

```whcl
if {EXPR} {
  CODE
} else if {EXPR} {
  CODE
} else if {EXPR} {
  CODE
} else {
  CODE
}
```

Keep in mind, however, that all elements must be on a single
logical line, so the following is illegal:

```whcl
if {EXPR}
{CODE}
```

As is:

```whcl
if {EXPR} {CODE}
else ...
```

For purposes of [`expr`][expr] and command evaluation, `if` evaluates
to `true` if any `if {EXPR}` part resolves to true, else the construct
as a whole evaluates to `false`.

<a id='while'></a>
# `while` We Have Your Attention...

WHCL `while` loops look like:

```whcl
while {EXPR} {...}
```

The `{EXPR}` part contains [an expression][expr] which is
evaluated before each iteration of the loop. The loop body may contain
arbitrary script code. A new scope is pushed before the expression and
popped at the end of each iteration.

The `EXPR` may optionally be written as:

```whcl
while (EXPR) ...
while [command ...] ...
```

The `(EXPR)` form is semantically equivalent to `{EXPR}` and the
`[command...]` form is semantically equivalent to `{[command...]}`.

Within the body of a `while` loop the `continue` and `break`
pseudo-keywords can be used to jump back to the start of the loop or
exit it, respectively.

By default a `while` loop evaluates to the value `undefined`.  The
`break` pseudo-keyword can optionally be given a value which becomes
the result value of the loop:

```whcl
assert "hello" == [while {true} {
  break "hello"
}]
```

<a id='do-while'></a>
# Let's `do` something for a `while`...

WHCL `do` loops look like:

```whcl
do {...} while {EXPR}
```

It functions exactly like a `while` loop except that the "keep
looping?" check happens after the loop body.

The `EXPR` may optionally be written as:

```whcl
do {...} while (EXPR)
do {...} while [command ...]
```

The `(EXPR)` form is semantically equivalent to `{EXPR}` and the
`[command...]` form is semantically equivalent to `{[command...]}`.

> Sidebar: the "while" token is technically extraneous but is
currently thought to improve readability. It _might_ at some point be
made optional.

<a id='for'></a>
# `for` Ever and Ever

WHCL's `for` loop is fairly conventional, with the slight oddities
being a TCL-like syntax and the ability to evaluate to an arbitrary
value via the `break` command. The basic syntax is:

```whcl
for {pre-code} {condition (expr)} {post-code} {loop-body}
```

Sidebar: the reason the "post" code, which runs after the loop body,
is placed _before_ the loop body is because this ordering mimics the
conventional for-loop structure of C-like languages:\  
`for(pre; condition; post) {body}`.

All of the `{...}` blocks are evaluated as arbitrary code except for
the second one, which [evaluates as an expression](builtin-expr.md).

A `for` loop uses _two_ scopes: one starts immediately before the
"pre" block is evaluated and another wraps up the three remaining
components and is reset between each iteration. Thus variables
declared in the "pre" block are local to the entire execution of the
loop but variables declared in the loop body are destroyed and reset
on each iteration of the loop.

Examples:

```whcl
for {decl i 0} {$i < 10} {incr i} {
    echo "Hello world" $i
}

for {
  decl x 0; decl y 0; decl z 0
} {
  $z < 10
} {
  incr x; incr y; set z expr $x + $y
} {
    echo z = $z
}
```

Note that newlines within each `{...}` part delimit commands
as usual, as do semicolons. The exception is the condition block,
where all newlines are ignored and no semicolons should appear.

<a id='foreach'></a>
# `foreach` and Every One of Us...

`foreach` is capable of iterating over list entries, object
properties, and string characters. Its basic syntax is:

```whcl
foreach [-props] [indexVar] valueVar iterableValue {CODE}
```

`indexVar` and `valueVar` are names for scope-local variables which
get set in the loop body.  Their exact meanings may differ depending
on the type of value. If both are set, they refer to the list entry
index and value (for arrays strings) or the property key and value
(for object properties). If only one of them is provided, the semanics
differ:

- Arrays and strings: the one variable gets set to the _value_ of
  the current iteration. For strings, each iteration is a single
  character (a length-1 string).
- Object properties: the one variable gets set to the _key_ (name) of
  the current property (noting that keys need not be strings and have
  no well-defined order).

Arrays are a special case: they may have both list entries and object
properties. By default `foreach` will iterate over list properties but
the `-props` flag changes that behavior to iterate over the object
properties.

By default a `foreach` loop evaluates to the value `undefined`.  The
`break` pseudo-keyword can optionally be given a value which becomes
the result value of the loop:

```whcl
assert 'h' == [foreach c "hi" {break $c}]
```

Examples of each supported type of iteration follow...

## `foreach` List Iteration

```whcl
decl -array ar ('a' 'b' 'c')
# Iterate with indexes and values:
foreach ndx val $ar {
  assert $ar[$ndx] == $val
  echo "ar[" $ndx "] =" $val
}
# Iterate with values only:
foreach val $ar {
  echo "current value =" $val
}
# Iterate over object-level property key/value pairs:
set $ar['hi'] = 'world'
set $ar['hello'] = 'world';
foreach -props key val $ar {
  echo "ar[" $key "] =" $val
}
# Iterate over object-level property keys:
foreach -props key $ar {
  echo "ar[" $key "] =" $ar[$key]
}
```

## `foreach` Object Property Iteration

```whcl
decl -object o key1 val1 key2 val2
# Iterate over object-level property key/value pairs:
foreach key val $o {
  assert $o[$key] == $val
  echo "o[" $key "] =" $val
}
# Iterate over object-level property keys:
foreach key $o {
  echo "o[" $key "] =" $o[key]
}
```

## `foreach` String Iteration

```whcl
decl str "hällö" # strings may be UTF-8
foreach i c $str {
    echo "char #" $i "=" $c
    assert $str[$i] == $c
    # note:^^^^^^^^ is computationaly slow for non-ASCII
    # strings but O(1) for ASCII.
}
foreach c "hällö" {
    echo "char = " $c
}
```


[expr]: builtin-expr.md
