# whcl POSIX Regexes
#### ([&#x2b11;Main Module Docs](../))
# POSIX Regexes and Features Common to POSIX and JS Regexes
<style>@import url(../../../doc/fossil-doc.css)</style>
<script src="../../../doc/highlightjs/highlight-cwal.min.js"></script>

- Source code: [](/dir/whcl/mod/regex-posix?ci=trunk)
- Test/demo code: [](/finfo/whcl/mod/regex-posix/test.whcl)

Jump to:

- [The Regular Expressions API](#mod-regex)
    - [Minor Achtung: Capture Limits](#mod-regex-capture-limits)
- [Methods](#mod-regex-common-methods)
    - [Minor Achtung: Arrays vs Tuples](#mod-regex-lists)
    - [Big Achtung: Strings vs. Buffers](#mod-regex-buffers)
- [Properties](#mod-regex-common-properties)
    - [Module-level Properties](#mod-regex-common-properties-module)
    - [Per-Instance Properties](#mod-regex-common-properties-instance)
- [Regex Flags](#mod-regex-flags)
    - [Compilation Flags](#mod-regex-flags-compile)
    - [Match-time Flags](#mod-regex-flags-match)
- [Symbol Resolution via Callbacks](#symbol-resolution)
- [Achtung: Locale-dependent Matching](#mod-regex-common-locale)


<a id='mod-regex'></a>
# The Regular Expressions API

This module exposes a single function which compiles regular
expression strings in the POSIX regex grammar and returns
a module-specific type:

```whcl
RegexType module string pattern [string compileFlags]
```

The first parameter is a regular expression pattern in the module's
regex dialect.

The second parameter is an optional string describing regex
compilation flags, each one a single letter. The POSIX-specific
compilation flags are described [in a subsection
below](#mod-regex-flags-compile). An empty string is legal for
the flags.

The compilation function throws an exception if the pattern is
syntactically invalid or an invalid flag letter is provided.

The compilation function is used like this:

```whcl
decl -const regcomp [whcl load-module '/path/to/regex-posix.so']
# or, if it's built in to whcl:
decl -const regcomp [whcl install-api regex-posix]
decl regex [regcomp "foo" i]
```

<a id='mod-regex-capture-limits'></a>
## Minor Achtung: Capture Limits

The POSIX regex API has some peculiar rules regarding capture limits,
how it deals with them, and limitations it places on regexes which
are compiled with the `NOSUB` flag.

This library imposes a compile-time limit on the number of capture
groups which a regex may have (10, as of this writing, including the
full-string match). The POSIX regex will gladly process a regex which
has more captures than that limit, but will silently ignore any
captures above that limit.  This API, however, will trigger an
exception if a regex is run with more captures than this limit.  We
"could" silently ignore such a situation, and that design decision is
up for reconsideration later, but practice implies that we should
apply the stricter option until/unless we later decide that the laxer
approach would be more useful.

<a id='mod-regex-common-methods'></a>
Instance Methods
============================================================

The regex type returned from module is a different type but they have
nearly-identical interfaces. Their common methods and behaviours, as
well as any significant differences in behaviour, are described below.


destroy
------------------------------------------------------------

Usage: `destroy`

Immediately frees all native resources used by this regex. That also
happens when garbage collection reaps the regex, but clients may force
it immediately with this method. After this method is called, calling any
regex methods on this object will throw an exception because the
underlying C-level regex instance no longer exists.

<a id='mod-regex-each-match'></a>
each-match
------------------------------------------------------------

usage: `each-match text string|function callback [matchFlags]`


For each match of this regex in the given text, this function calls
the given callback:

- If it is a function, it is passed the complete match text as its
  only argument.
- If it is a string or [Script][ScriptType], it is `eval`'d for each
  match.
- In both cases, the call/eval-local `this` refers to the regex
  object.

Any return/result value of the callback is ignored.

`matchFlags` may be any flags accepted by `exec` or `replace`, noting
that the [`$` and `E` `replace`-specific flags](#mod-regex-replace)
are automatically implied if the callback is a *string* or
[Script][ScriptType] (because this function would be useless with a
string callback without those flags). The `E` flag is ignored if the
callback is a function. The `$` flag can be used to provide a callback
function access to sub-captures.

> *Achtung:* the POSIX regex C API does not support `each-match` for
regexes which are compiled with the `s` (`NOSUB`) flag, and attempting
it will trigger an exception.


exec
------------------------------------------------------------

Usage: `exec text [matchFlags]`


Returns `false` if the input string does not match, else it returns a
list of the matches (but see the caveat below!). Element 0 in the list
is the entire match and each subsequent element is the contents of a
captured subexpression. Thus capture number N is element N in the
result list. Both flavours of regex have a hard upper limit of
captures (see the comments above on this topic), including the
whole-match entry.

The second argument is an optional string of letters representing
regex-flavor-specific match-time flags. The legal flags and letters
for POSIX are [listed in a following
subsection](#mod-regex-flags-match).

> *Achtung:* the POSIX `regexec()` C API cannot report the substring
position of matches for regexes which are compiled with the `s`
(`NOSUB`) flag (not even for the whole-match part), so this method will,
for such regexes, return `true`, instead of a list, on a match.


match-all
------------------------------------------------------------

Usages:

- `match-all -capture text [matchFlags]`
- `match-all -capture text`

This function has two distinct modes:

- If the `-capture` flag is not specified then if a match is found, a
  list of all complete match strings (not split into sub-captures) is
  returned. i.e. a single-dimensional list of strings.
- If `-capture` is specified then if a match is found, a list of lists
  is returned, with each sub-list having the same structure as the
  result of `exec`. i.e.
  `[["full match 1","capture 1",..."capture N"], ["full match 2",...] ...]`.

In both cases, if no match is found a falsy value is returned.

`matchFlags` is an optional flag to change how matching works,
exactly as described for `exec`.

> *Achtung:* the POSIX regex C API does not support `match-all` for
regexes which are compiled with the `s` (`NOSUB`) flag, and attempting
it will trigger an exception.

> *Minor Achtung:* remember that captures for the POSIX regex API are a
genuine pain in the butt when compiled with the `BASIC` flag.


<a id='mod-regex-replace'></a>
replace
------------------------------------------------------------

Usages:

- `replace text replacement [maxReplacements=0 [matchFlags]]`
- `replace text replacement [matchFlags]`

Replaces instances of the regex's match in the given string with the
given replacement. `replace` only accepts strings, not buffers, as
input text.

> *Achtung:* the POSIX regex C API does not support `replace` for
regexes which are compiled with the `s` (`NOSUB`) flag, and attempting
it will trigger an exception.

The given `replacement` value is applied in the part(s) of the `text`
argument which match the regex. High-level `replacement` values like
objects and arrays will be appended in JSON form, but any cycles in
such constructs will trigger an exception. If `replacement` is a
function then:

1. It is passed the complete match text of each match and the result
   of the call becomes the replacement.
2. In the context of the callback, `this` refers to the regex instance.

If `replacement` is [a Script value][ScriptType] then it's
treated as if it were a string and the `E_` match-flags (see below)
are in effect. This is far more efficient than passing a string with
that flag, as this for only has to be compiled once, instead of on
each iteration.

If `maxReplacements` is passed in, it must be an integer. A value of 0
or less means unlimited, else the number of replacements is limited to
the given value.

`matchFlags` is an optional flag to change how matching works, exactly
as described for `exec`, plus it supports the following string-form
flag letters which only work for this routine and
[`each-match`](#mod-regex-each-match):

- `E` causes the replacement value to be `eval`'d for each
  match. (This option isn't of any use unless the replacement is a
  string or script and is ignored if the replacement is a function.)
  Its result becomes the replacement text. Like with the function
  replacement form, `this` resolves to the regex being operated on.
- `$` or `_` causes all regex capture groups to be expanded into a
  list named...  well, it *would* be named `$`, as is conventional for
  regex APIs, but this language's syntax cannot handle that symbol in
  that context, so it's named `_` instead. It's accessible from the
  replacement callback/eval. Index 0 is the whole match, and
  subsequent captures are at the subsequent positions. e.g. use `_.3`
  or `_[3]` to get the third capture from within a callback. `_` is
  local to the callback/eval scope, so it is not available after
  `replace` returns. (Using `€` in place of `$` is exceedingly
  tempting!)

If the `replacement` is [a Script value][ScriptType] then both of those
flags are implicit.

> TODO: replace the final arg with flags.

> Sidebar: we do not export symbols named `$0`...`$N`, as would be
more conventional vis-a-vis other regex APIs, because: (A) it would be
much less efficient to do so. (B) it won't work with our var naming
syntax.

When using a function for the replacement, the function is passed the
full string of each match. If the `$` `matchFlag` is used then
captures are available as described above. Without that flag, the
function may nonetheless perform replacement based on capture groups
by calling `exec` on the regex from within the replacement
callback, like so:

```whcl
decl x [regcomp '[ \t]*([a-z]+)[ \t]*(;)?']
# Normalize inputs to upper-case and strip extraneous spaces:
assert 'A;B;C' == [re replace 'a\t  ;\tb; c' [proc -anon {a} {
    decl m [this.exec $a]
    # At this point we know this regex matches the input,
    # so we don't need to bother checking whether m is falsy.
    decl u [m.1.to-upper]
    if {m.2} {set u [$u concat m.2]}
    return $u
}]]
```

However, that requires running the regex *twice* on each matching part
of the input, so using the `$` flag is recommended if captures beyond
the whole-match capture are needed.

Here's a functionally equivalent example which uses the `$` flag to
make the captures available and the `E` flags to demonstrate using a
string as a callback body:

```whcl
# Using the same regex as above:
assert 'A;B;C' == [re.replace
    'a\t  ;\tb; c'
    {
        decl rc
        if {_.2} {
            set rc [[_.1.to-upper].concat _.2]
        } else {
            set rc [_.1.to-upper]
        }
        eval $rc; # sets result of the outer eval block
    }
    '$E']
```

Remember that a `{...}`, when passed to a function, is just a special
case of string, not a code block which gets evaluated before calling
the function. The `E` flag then causes that string to be `eval`'d for
each replacement.

> Sidebar: *no buffers as input?* Buffers are not allowed as input
here because, when used in conjunction with a replacement
callback/eval, it would be possible for the buffer to be modified
during traversal, which would lead to Undefined Behaviour. Working
around that (by moving its contents out of the way during traversal,
similar to how `Buffer.eval-contents` works) would be rather
fidgety. Interestingly, though, passing a buffer as a replacement
value is legal because without a callback there is no risk of it being
modified during the replacement process. It might seem sane to permit
buffer inputs when the replacement is *not* a callback, but that could
still potentially backfire badly when invoked in certain convoluted
recursive contexts.


split
------------------------------------------------------------

Usages:

- `split text [limit = -1 [matchFlags]]`
- `split text matchFlags`

This works similarly to `"astring".split "pattern"` except that it
splits the first argument on this regex's pattern.

If a `limit` is 0 or greater, it captures, at most, that many
elements, otherwise it captures all it can (just like
`string.split`). (Yes a limit of 0 is valid, but i have no idea
why - JS allows it. Note that this differs from `string.split`,
where a limit of 0 means unlimited.)

`matchFlags` is an optional flag to change how matching works,
exactly as described for `exec`.

Note that, like `string.split`, this treats matching separators at
the start and end of the input as empty entries at the start resp.
end of the result list.

Note also that while `string.split ''` splits the string into its
componenent characters, there is no equivalent with this API because
the API forbids empty regexes. Splitting on a regex of `"."` will
behave much differently, treating each character as a separator and
returning a list of what's *between* those separators (empty strings).

> *Achtung:* the POSIX regex C API does not support `split` for
regexes which are compiled with the `s` (`NOSUB`) flag, and attempting
it will trigger an exception.


test
------------------------------------------------------------

Usage: `test text [matchFlags]`

Works like `exec` but returns `true` if the given text matches the
regex, else `false`. i.e. it does not allocate a data structure for
the result, so it's more efficient (but less informative) than
`exec`.


<a id='mod-regex-lists'></a>
Minor Achtung: Arrays vs Tuples
------------------------------------------------------------

The regex APIs explicitely do not use the terms "array" or "tuple"
when refferring to list-type return values. Any given routine may
return either in any context where a list is returned, that type may
differ between versions of the modules.

For 99+% of use client-side cases, it doesn't make a difference either
way: the two list types are used the same way until/unless the client
wants to change their size or sort them or perform some other
array-only operation. Element access and [`foreach`
iteration](../../manual/flow-control.md#foreach) are
the same for both list types, and those are the only operations
*normally* performed on string-matching APIs of this sort.

FWIW, the "preferred" return type is a tuple, as they're
memory-lighter, but that's only possible when a given function knows,
in advance, how many slots the result list will need (which isn't
always the case).


<a id='mod-regex-buffers'></a>
Big Achtung: Strings vs. Buffers
------------------------------------------------------------

*Most* places where the regex APIs accept a string, they also accept a
Buffer, but results are undefined if such a buffer contains non-String
data. Routines which accept accept callbacks (e.g. to iterate over
matches or for implementing dynamic string replacement) disallow
buffers as input because it would be disastrous if the buffers were
modified by/via such callbacks while this API is traversing its
contents.

<a id="mod-regex-common-properties"></a>
Properties
============================================================

<a id="mod-regex-common-properties-module"></a>
Module-level Properties
------------------------------------------------------------

Each regex module's compilation function has a property named
`instance-prototype`, the prototype which gets assigned to each new
regex instance. This can be used to modify the behaviour of regexes
without having to first instantiate one to get at its prototype.

An example of such modification would be to extend the *string* prototype
in order to be able to make use of certain regex functionality. For example,
the following code replaces `string.split` with a proxy which can make use
of either (or both) of the regex modules when a regex is passed as its first
argument:

```whcl
decl R [whcl install-api regex-posix]
decl S "".__prototype
if {S.split} {
    set S.split proc proxy {/*pattern, limit*/} {
        if {[info is-native argv.0] && argv[0][__prototype] == $P} {
            return [argv[0].split this (argv.1 || -1)]
        } else {
            return [X.apply this $argv]
        }
    } using -scope {
        decl -const P R.instance-prototype
        decl -const X S.split
    }
}
# Now demonstrate it...
# First, split on a regex:
decl m ["a;b;c".split [R " *; *"]]
assert 3 == [m.length]
assert 'c' == m.2
# Now split on a string:
set m ["A;B;C".split ";"]
assert 3 == [m.length]
assert 'C' == m.2
```

A similar proxy could be used to allow
`string.replace(pattern,replacement)` to accept `(regex,function)`
arguments:

```whcl
decl -const R [whcl install-api regex-posix]
decl -const S "".__prototype
if {S.replace} {
    set S.replace proc {/*needle, replacement*/} {
        if {[info is-native argv.0] && argv[0][__prototype] == $P} {
            return [argv[0].replace this argv.1]
        } else {
            return [X.apply this $argv]
        }
    } using -scope {
        decl -const P R.instance-prototype
        decl -const X S.replace
    }
}
# Now try it out...
# Replace via regex:
assert abc == ["AbC".replace [R '[A-Z]'] [proc -anon {x} {return [x.to-lower]}]]
# Replace via string:
assert abc == ["Abc".replace A a]
```

<a id="mod-regex-common-properties-instance"></a>
Per-Instance Properties
------------------------------------------------------------

Each regex instance has the following standard properties assigned to it:

* `pattern` holds the original pattern string which was passed to the
  compile function.
* `flags` holds the string-form regex compilation flags.

The values of those properties can be used to save and restore a regex
for later use.


<a id='mod-regex-flags'></a>
# Regex Flags

<a id='mod-regex-flags-compile'></a>
## Regex Compilation Flags

Regex compilation may be modified by providing a string of
single-letter flags. Unknown flags cause an exception to be
thrown. The flags for this module are listed below. The upper-case
names listed next to each flag are the C-level names for the flags,
with the exception of `BASIC`, which is does not exist at the C level
(C-level POSIX regexes default to `BASIC` mode unless the `EXTENDED`
flag is used, whereas this API does the opposite because basic-style
regexes are not terribly useful.)

- `"B"` (`BASIC`) mode: by default, this module compiles regexes in
"extended" form because practice has shown that its "basic" regexes
are too primitive and problematic for all but the most basic of
cases. To force it to use basic mode for a given regex, use this flag.

- `"e"` (`EXTENDED`) mode: this is the default. See `B`, above.

- `"i"` (`ICASE`): make the regex case-`i`nsensitive.

- `"n"` (`NEWLINE`): tells the `$` end-of-pattern anchor (e.g. `foo$`) to also
match after a `n`ewline character. e.g. with this flag, the input text
`foo\nbar` would match the pattern `foo$`, but it would not match
without this flag.

- `"s"` (`NOSUB`): tells the regex not to do any capturing of
`s`ub-expressions. *Curiously*, this also means that the C-level APIs
do not report the whole-match part of a match. That is, when this flag
is in effect, the POSIX C APIs can tell us that there's a match but
not *where* that match is. Thus regexes compiled with this flag are
not usable with APIs which require knowing *where* a match is found,
e.g. `match-all`, `replace`, and `split`.


<a id="mod-regex-flags-match"></a>
## Regex Match-time Flags

Like compilation flags, flags which change regex matching behaviour
at match-time may be provided as a string of letters describing the
flag(s):

- `"b"` (`NOTBOL`): do not treat the beginning of the input string as the
`b`eginning of a line. That is, do not match `^X` at the start of the
input.

- `"e"` (`NOTEOL`): do not treat the end of the input string as the `e`nd of a
line. That is, do not match `X$` at the end of the input.

<a id="symbol-resolution"></a>
Symbol Resolution via Callbacks
============================================================

Normally in whcl, the search for resolving symbols stops at a function
call boundary. That makes using certain types of callbacks tedious, in
particular it severely limits the use of `eval`'able strings as
callback handlers. The following methods are configured such that
symbol resolution is permitted to pass on through them, to simplify
implementing client-side callbacks:

- `replace`
- `each-match`

When passing callback functions, as opposed to `eval`'able script
snippets, to these methods, the callbacks still need the `-xlookup`
flag if they want to reference symbols which are accessible from the
scope in which the above method is called.

See [the `proc` docs](../../manual/builtin-proc.md#-xsym) for more
details about this feature.


<a id='mod-regex-common-locale'></a>
Achtung: Locale-dependent Matching
============================================================

The POSIX regex API uses locale-dependent pattern matching and this
module does not set the software's locale because it cannot know if
the overlying software has done so, or what effects changing that
setting might have on the rest of the app. Thus, unless the software
or environment changes the locale, these regexes will use the "C"
locale for matching purposes and will likely not match non-ASCII
strings or patterns.


[ScriptType]: ../../manual/type-script.md
