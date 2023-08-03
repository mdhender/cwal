# s2: I/O API
#### ([&#x2b11;Table of Contents](./)) ([&#x2b11;Misc. Features Index](misc-index.md))
<a id="misc-io"></a>
# s2 I/O API

This API gets installed by [s2sh](s2sh.md) as `s2.io`. Client
applications are free to install it (or not) under a different
name.

This object contains a few I/O-related functions. None of its members
require that this object be their `this`, so they can be copied for
use in other contexts:

```s2-member
Function print( ... )
```

Outputs each of its arguments to s2's configured ouput channel, with
a single space between each argument and a newline after the last one.
Returns itself, so calls may be chained.

``` s2-member-deprecated
Function output(...)
Function output << ...
```

The `output` function provides the [same feature as `s2out`][s2out]
but predates [`s2out`][s2out] by several years. [`s2out`][s2out]
should be preferred.

[s2out]: keywords.md#keyword-s2out

```s2-member
void flush()
```

"Flushes" s2's configured output channel. This is truly never needed,
and is provided in the API only for pedantic completeness.
