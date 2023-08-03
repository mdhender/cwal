# whcl: expr Builtin Command
##### ([&#x2b11;Table of Contents](./)) ([builtins](builtins.md))
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

# It's Just an Expression

Like TCL, whcl does not build math or logical operations into the core
of the language syntax. Instead, it delegates such operations to a
builtin command known as `expr`. Many constructs use expression
evaluation, but `expr` is the command which exposes that feature for
client-side use. The `expr` builtin command is responsible for the
language's math, logical, and bitwise operations.

Usage: `expr ...operators and operands...`

Arguments to expr may be any of the following:

- Literal values: strings, numbers, and built-in values.
- [Variable dereferences](grammar.md#grammar-deref-vars)
- [Object property lookups](grammar.md#grammar-deref-props)
- Commands in `[...]`.

> Achtung: all tokens must be space-separated. e.g. `1+2` is not legal
  in this context because it's actually two tokens: numbers `1` and
  `+2`. By the point that it's evaluated, the fact that it _could_
  have been interpreted as three tokens is lost.

The above conststructs are all treated as values. The remaining
operators have C-like precedence and associativity but their semantics
may differ slightly:

- Math operators:
  - Addition or subtraction: `+` `-` (unary or binary). Note that
    numeric literals may include their own `+` or `-` prefix which
    is independent of this operator.
  - Multiplication and division: `*` `/`
  - Modulo: `%` (results are always an integer)
  - Numeric result types for math operations are
    [described below](#expr-math-result-type).
- Comparison ops: `<` `<=` `>` `>=` `==`\  
  Noting that `==` is type-strict, and will never compare two values
  of different types as equivalent except that it will compare
  differing numeric types and booleans: `false`==0 and
  `true`==_any_ non-0 number.
- Logical not `!` is a unary operator.
- Parenthesized groups are treated as sub-expressions, evaluated
  recursively.
- Bitwise operators:
  - Binary: `&` `|` `^`
  - Unary: `~`
- Logical operators `&&` and `||` with short-circuiting. The result of
  `&&` is always the LHS value if the LHS is falsy, else the RHS
  value (JavaScript-like). The result of `||` is the LHS value if it
  is truthy, else the RHS (JavaScript-like). Short-circuiting ensures
  that the RHS of an expression is only evaluated if necessary.

**Achtung:** unlike TCL, operators and operands do not (normally)
strictly require spaces between them. This is a side effect of WHCL
being more strongly-typed than TCL.

Note that the math and bitwise operators expect to be given numeric
values and will coerce non-numeric values to numbers. Any values which
cannot be coerced to numbers are silently treated as the number 0.

The comparison operations can take any value type and will try to do
something sensible with them, noting that:

1. There are no built-in, deterministic comparison operations for the
   various container types, with one exception: any value, regardless
   of its type, will always compare equivalent to itself.
2. It will not compare different types as equivalent except that it
   will freely compare integers and floating-point numbers. It will
   not compare string-like numbers to numeric types (all comparisons
   will evaluate as `false`). However, number-like strings can be
   coerced to numbers by prefixing them with `+`. e.g.
   `expr +"123" == 123` is true.


The following examples implicitly invoke `expr` via the `assert`
keyword, which takes an expression.

```whcl
pragma trace-assert 2
decl a [array 1 2 3]
assert $a[0] == 1
assert $a[2] == 3
assert $a[0] < $a[1]
assert $a[1] > $a[0]
decl o [object {1 'int one' "1" 'string one' 1.0 "double one"}]
assert $o["1"] == "string one"
assert $o[1] != $o["1"]
# Numeric comparison is an exception to the type-strict compare rule:
assert 1.0 == 1
assert 1 + 4 == 1 + 2 * 2
assert "123" != 123
assert +"123" == 123
assert !(+"123" != 123)
assert -"123" == -123
assert +"-123" == -123
assert !(+"123" !=
                   123)
assert 1<<2>>1 == 2
# ^^^^ note that spaces between operators and operands are
# not strictly required except between two operators.
assert 0b1001 | 0b1100 == 0b1101
assert 0b1001 ^ 0b1100 == 0b0101
decl -const MAX_INT [expr ~0 >> 1]
decl -const MIN_INT [expr $MAX_INT + 1]
# set MIN_INT 0 # will fail with a constness violation
assert $MAX_INT > 0
assert $MIN_INT < 0
assert $MIN_INT - 1 == $MAX_INT
assert $MAX_INT + 1 == $MIN_INT

assert -3 == -0b11
assert -3 == -0x3
assert -3 == -0o3

# Short-circuiting:
assert 1 || [exit "should have been skipped"]
assert !(0 && [exit "should have been skipped"])

assert 2 == [
        expr
        1
        +
        1
]
# Or, more succintly:
assert 2 == (
        1
        +
        1
)
```

<a id='expr-math-result-type'></a>
# Result Types of Math Operations


When adding an integer and a floating-point number, what should the
result type be? It's tempting to say "floating point, obviously," but
there's a catch: C `double` types have a smaller numeric range than
64-bit integers. So obviously we should switch to integers when the
number is too large, right? If we did, math operations would not
necessarily be commutative.

In order to ensure that math operations are commutative, if a math
operator other than modulo (`%`) has any floating-point argument, the
result is also floating-point.

```whcl
1.0 * 3; # ==> double 3.0
3 * 1.0; # ==> double 3.0
```

The obligatory exception is the modulo (`%`) operator: in whcl its
result is always an integer, regardless of the input types.

For integer artithmatic, whcl internally uses unsigned values so that
overflow and underflow behave predictably. (whcl's implementation
language, C, specifically has undefined behavior for overflow and
underflow of *signed* integers.)
