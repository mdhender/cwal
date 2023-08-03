# s2: Incompatible Changes
#### ([&#x2b11;Table of Contents](./))
# Incompatible Changes

This section describes any fundamental changes in the language or core
APIs which might break existing client code (C or script). Minor API
changes are not noted here. As s2 does not have any version number
other than its [checkin hash value](/timeline), we use dates
(YYYY-MM-DD) as the point of reference.

2021-08-01: **default for object property storage is now hashtables**.
Though this is not expected to cause grief for any client(s), it is
worth mentioning. See the next entry for details. *This* change simply
swaps *that one's* default.

2021-07-12: **object property storage is now compile-time
configurable** between sorted doubly-linked lists (the classic/legacy
method) and hashtables.  These work identically except that hashtables
are necessarily type-strict in how they compare values. That is, the
property keys `"1"` (string) and `1` (integer) will compare as
equivalent in the legacy model but not the hashtable model. This
difference is covered in more detail in [the Object class
docs](type-object.md).

2020-02-21: **enums are now always hashes**. Previously, enums where
schizophrenic, using either objects or hashes for their underlying
storage, depending on their size or whether the `#` character appeared
at the start of their body block. This led to an ever-increasing
amount of special-case handling for them, so they were changed to be
hashes only. The primary effect on script code is that the `#`
modifier to enums (`enum {# ...}`) was removed and will trigger a
syntax error. Their script-side usage is otherwise the same. The hash
search operator (`aHash#'key'`) still explicitly does not work on
enums. C-side, the semantics of `s2_enum_builder_init()` were slightly
modified, and two of its parameters were swapped solely to force
client-side code to check the docs (where they will presumably see the
modified semantics for the enum size hint parameter).


2019-12-29: **the `refcount` keyword, which was deprecated a full 5
years ago, was removed**. **The `typename` keyword was
un-deprecated**, as it fills a useful role and is slightly more
efficient, at the tokenization level, than `typeinfo(name)`.


2019-12-12: **the core cwal object iteration model was improved to allow
multiple concurrent iterations over object properties, hashtable
entries, and list entries.** Thus the `typeinfo(mayiterate)` and
`Object.mayIterate()` family of queries may respond differently than
before (allowing iteration where it was previously denied) and [new
`typeinfo()` queries](keyword-typeinfo.md) were added to account for
the new separation of "object property iteration" vs. "list iteration"
(the latter includes, incidentally, iteration over hashtable
entries). Modifying object properties and hash entries during
iteration is still prohibited (the data model does not support it),
but modifying array entries during iteration is legal. Certain
operations which *may* have been allowed during list iteration, e.g.
resizing or sorting an array, are now disallowed (though it's
conceivable that sorting during iteration may be enabled if it can be
proven to be harmless (but potentially confusing)). Likewise, list
iteration, resizing, and array index modification during array sorting
is now explicitly disallowed (the results before were undefined).

2019-08-04: **overloaded operator signatures for prefix and postfix
++/-- were changed.** Prefix versions are now `--operator()` resp.
`++operator()` and postfix are `operator--()` resp. `operator++()`.
Previously, they had the same names but the postfix form was passed
its own `this` as its only argument, to differentiate it from the
prefix form. To the best of my knowledge, only s2-internal test code
ever overloaded these operators.

2017-11-04: **overloaded operator signatures were changed** to
remove the superfluous passing of their own `this` as an argument.
Thus, e.g. `operator+=(self, arg)` is now `operator+=(arg)`.

2014-12-07: **deprecated keywords** `typename` and `refcount`. Use
`typeinfo(name …)` and `typeinfo (refcount …)` instead (keeping in
mind that script code can do nothing useful with the refcount - it's
only for debugging s2). When using `typename` for argument type
validation, clients will find that [`typeinfo()` provides a richer set
of options](keyword-typeinfo.md) (and is likely faster than typename
comparisons). The deprecated keywords may or may not eventually be
removed.
