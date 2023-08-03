# whcl: info Builtin Command
##### ([&#x2b11;Table of Contents](./)) ([builtins](builtins.md))
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

# Querying `info` About Values and Variables

The `info` builtin command is for querying all sorts of properties
about variables and values.

Usage: `info [args]` (some queries need an argument, some don't)

The current tags are listed in full below but first a description of
their naming conventions is in order. The tags are all lower-case.
**Many start with "is-" or "has-", and both of those have similar
meanings best explained with an example:** Consider the *is-array* and
*has-array* tags: an Array value (created via the `array` built-in
command or the C-level `cwal_new_array()`) "is" an array, whereas a
non-array value which includes an Array in its prototype chain "has"
an array (and can be treated like one for many purposes). What makes
an array an Array is *really* the fact that it has the C-level type ID
`CWAL_TYPE_ARRAY`. Whether or not it behaves like an array in whcl
depends largely on its prototype, where most script-visible behavior
is implemented.


# Type Queries

- **`is-array`** `VALUE`
  True if the value is an array.
- **`has-array`** `VALUE`\  
  True if the value is an array or has an array in its prototype
  chain.
- **`is-tuple`** `VALUE`\  
  True if the value is-a tuple. (As of this writing, tuples are not
  exposed via the scripting interface but may bubble up via
  C-level bindings.)
- **`is-list`** `VALUE`\  
  True if either `is-array` or `is-tuple` is true.
- **`is-bool`** `VALUE`\  
  True only for the built-in constant values `true` and `false`.
- **`is-buffer`** `VALUE`
- **`has-buffer`** `VALUE`
- **`is-container`** `VALUE`\  
  True if the value is capable of holding its own set of properties.
- **`is-derefable`** `VALUE`\  
  True if the value is valid for use with a `$var[propertyKey]`
  operation. This is true for the majority of values, including
  numbers (which inherit a common prototype).
- **`is-integer`** `VALUE`\  
  True only for integer values.
- **`is-double`** `VALUE`\  
  True only for floating-point values.
- **`is-number`** `VALUE`\  
  True for integers, floating point, and boolean values.
- **`is-numeric`** `VALUE`\  
  True for number types and number-like strings.
- **`is-exception`** `VALUE`\  
  True only for exceptions.
- **`has-exception`** `VALUE`
- **`is-function`** `VALUE`\  
  True only for functions (native and script-created).
- **`has-function`** `VALUE`
- **`is-native`** `VALUE`\  
  True only for Native values (of C type `cwal_native`).
- **`has-native`** `VALUE`
- **`is-object`** `VALUE`\  
  True only for values of the base object type.
- **`has-object`** `VALUE`
- **`has-prototype`** `VALUE`\  
  True if the value has a prototype.
- **`is-string`** `VALUE`\  
  True only for strings. Buffers can be used like strings in some contexts, but
  will evaluate to false here.
- **`type-name`** `VALUE`\  
  Evaluates to a string form of the given value's type. For the core
  types these values are hard-coded at the C level. Objects which have
  (or inherit) a string-type `__typename` property will use that value
  for their type name.

# Variable State Queries

- **`is-declared`** `IDENTIFIER`\  
  True if the given identifier can be resolved as a variable.
- **`is-local`** `IDENTIFIER`\  
  True if the given identifier refers to a variable declared in the
  current scope.

# The Obligatory Misc. Category

- **`is-newing`** `[VALUE=this]`\  
  True if the given value (defaulting to `this` is currently in the
  construction process via a constructor call (via [the `new`
  command](builtin-new.md)). This is true during both the constructor
  call itself but not during the (optional) post-constructor init code
  block (it's simply not needed there, but we sometimes need to
  know whether a function is being called as a constructor or not).
- **`ref-count`** `VALUE`\  
  Evaluates to the value's current reference count. This is solely for
  whcl/cwal debugging purposes. This is _nothing_ useful which script
  code can do with this value and scripts must never base any logic on
  it. Builtin constant values may have a constant refcount of 0.

TODO: `X inherits Y`, or similar. That requires adding new syntax
options for the `info` command. It's not 100% clear how we might
implement this under whcl class inheritance model, other than to see
if `X` has `Y` or `Y[__prototype]` somewhere in `X`'s prototype chain.
The latter case would be needed for compatibility with the `new`
command's conventions but the former makes more intuitive sense.


# Iteration-related Queries

The iteration-related `info` queries exist to assist in determining
whether it's currenly legal to iterate over an object or list
or whether iteration is currently underway.

The object model of whcl's underlying scripting engine does not allow
object-type properties to be iterated over in certain (rare)
cirumstances, nor does it support modifying properties during
iteration (because its data structures simply do not support
that). Lists (array entries) may be iterated concurrently and their
values modified, as doing so can be done without invaliding their
internal cursors. Note that appending to an array during iteration may
well cause and "endless" loop until the engine reaches its limit or
all system memory is consumed.

`info` queries related to querying iteration-related state:

- **`is-iterating`** `VALUE`\  
  True if either `is-iterating-list` or `is-iterating-props` is true.
- **`is-iterating-list`** `VALUE`\  
  True if the value's list elements are currently being iterated
  over.
- **`is-iterating-props`** `VALUE`\  
  True if the value's non-list properties are currently being
  iterated over.
- **`may-iterate`** `VALUE`\  
  Equivalent to OR'ing the results of `may-iterate-list` and
  `may-iterate-props`.
- **`may-iterate-list`** `VALUE`\  
  True if the given value may currently have its list-type properties
  iterated over. Currently the only time list iteration is _not_
  permitted is when the list is currently being sorted. The only
  way for script code to trigger such an error is to attempt to iterate
  the list from a stateful sort-order comparison function.
- **`may-iterate-props`** `VALUE`\  
  True if the given value may currently have its list-type properties
  iterated over.
