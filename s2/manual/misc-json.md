# s2: JSON API
#### ([&#x2b11;Table of Contents](./)) ([&#x2b11;Misc. Features Index](misc-index.md))
<a id="misc-json"></a>
# s2 JSON API

This API gets installed by [s2sh](s2sh.md) as `s2.json`. Client
applications are free to install it (or not) under a different
name.

cwal originally derived from [a JSON
library](https://fossil.wanderinghorse.net/r/cson), and has always had
a fairly close relationship with JSON (at the cwal level, as opposed
to the s2 level). This API implements JSON parsing/writing as specified
by [RFC 4627](http://www.ietf.org/rfc/rfc4627.txt), not the
2013/2014 IEEE standardization effort (which came much later but was
essentially a ratification of RFC 4627).

This object contains the following members, none of which
require that this object be their `this` object, so they can be copied for
use in other contexts:

```s2-member
object|array clone(object|array)
```

Functionally equivalent to
`thisModule.parse(thisModule.stringify(value))`, but is defined in
such a way that `thisModule` and its `parse()`/`stringify()` methods
need not be in scope when `clone()` is called. i.e., this function
reference may be copied and used independently of this module or
those two methods.


```s2-member
object|array parse(string|buffer jsonString)
```

Parses the given input as a JSON string. Throws an exception with
detailed information about any parsing errors. This is the "safe"
way to parse JSON. The "easy" way is simply: `eval->theJsonString`.

```s2-member
object|array parseFile(string filename)
```

The file counterpart of `parse()`. Throws if it cannot read the
file.

```s2-member
string stringify(Value [,int indentation = 0])
```

Behaves like the various core-type `toJSONString()` methods do, but
can also convert individual instances of value types which do not have
that method, like booleans, `null`, and `undefined` (which it
translates to `null` (but should, like JS, probably elide
altogether). It does not support customization of the output (e.g.
overriding a `toJSON()` method) because the conversions to JSON happen
at a lower level than s2. Any cycles in the structure will cause an
exception to be thrown (as JSON cannot handle cycles without
introducing client-specific conventions for doing so).

> *Tip*: the [json2](/finfo/s2/require.d/json2.s2) [require.s2
module](../mod/require/) provides a script-side implementation of
`stringify()` which is orders of magnitude less efficient than its C
counterpart (above) but supports client-provided `toJSON()`
methods. That module may be used as a drop-in replacement for this
API.

The `Object.toJSONString()` method can be used to convert "simple"
Objects and Arrays to JSON form, but any key or value types not
representable in JSON will be elided from the output (e.g. functions
properties).

[`Buffer.writeFile()`](type-buffer.md#type-buffer-members)
can be used to write JSON to files.

