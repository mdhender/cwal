# whcl: JSON API
##### ([&#x2b11;Table of Contents](./)) ([&#x2b11;API Index](api-index.md))
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

whcl JSON API
============================================================

This object contains functions for working with JSON-format data.  of
these functions require that their `this` be a particular object, so
they can be copied for use in other contexts.

This API may be installed using:

`whcl install-api json`

or calling `whcl_install_json()` from C code.

It adds a `whcl[json]` object with the API described below.

Most whcl-hosted types can be directly output to JSON.
Some more exotic types can do so [with a little help](#json-to-jsonable).

<a id='json-method-clone'></a>
clone
------------------------------------------------------------

Usage: `clone object|array`

Equivalent to passing the results of `stringify` to `parse`, resulting
in what amounts to a clone of the originally-stringified object. Will
throw if serialization or deserialization fails.


<a id='json-method-parse'></a>
parse
------------------------------------------------------------

Usage: `parse JSON-string`

Parse a JSON-format string and returns an object or array. The string
must be an Object or Array, not an arbitrary JSON-format value such as
a string or number. Throws if the input is not strictly JSON. (The
underlying parser is quite strict and does not support any extensions
such as comment blocks or unquoted object keys.)


<a id='json-method-parse-file'></a>
parse-file
------------------------------------------------------------

Usage: `parse-file filename`

Works like `parse` but takes its input from a file.

<a id='json-method-stringify'></a>
stringify
------------------------------------------------------------

Usage: `stringify value [indent=0 [cyclesAsStrings=false]]`

Returns the JSON-escaped string form of a single value.  It functions
identically to the `to-json` method of whcl's core classes.

Values of types which are not supported by JSON, e.g. functions, are
simply skipped. The `undefined` value gets translated to JSON `null`
(that's most relevant for serialization of arrays which have missing
entries).

The optional 2nd argument may be an integer or string: a negative
integer indents with that many hard tabs per nesting level, a positive
integer indents with that many spaces per nesting level, and zero adds
very little extra space to the output (it _does_ add _some_,
though). A string value indents with that string once per nesting
level. e.g. a value of -3 indents with 3 tabs per level and a value of
3 intents with 2 spaces per level.

The optional 3rd argument tells it how to deal with cyclic data structures.
By default they trigger an exception, but if this argument is true then
cycles are output in some debugging-style string form, not useful for
data storage but perhaps of some use to the developer in weeding out
cycles from their JSON data.

<a id='json-to-jsonable'></a>
to-jsonable: Overriding JSON Output Format
============================================================

The whcl-related JSON APIs install a custom JSON handler
on top of the underlying cwal-level JSON routines. This handler,
when used (meaning when cwal's to-JSON APIs are called through whcl,
as opposed to direct use of `cwal_json_output()`), checks for...

Values which have, or inherit, a function-type property named
`to-jsonsable` will use that routine to change how the JSON output is
formatted for that value. The method takes no arguments and must
return a new JSON-able value or `this` (assuming `this` is itself
JSON-compatible). For example, here's one for use as the [the Script
type's](type-script.md) `to-jsonable` method:

```whcl
proc {} {
    return object script [this.source-code] name [this.name]
}
```

The Script type is a so-called "native" type, so cannot be output
as-is by JSON. The above method is used to proxy it for JSON
output purposes. The minor caveat is that it only works when run
through whcl-hosted JSON APIs, as the core cwal JSON APIs don't
have the language-specific information necessary for handling
this by themselves.
