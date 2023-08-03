# s2 POSIX Regexes
#### ([&#x2b11;Main Module Docs](../))
# POSIX Regexes and Features Common to POSIX and JS Regexes

- Source code: [](/dir/s2/mod/regex_posix?ci=trunk)
- Test/demo code:
  - [](/finfo/s2/mod/regex_posix/test.s2)
  - [](/finfo/s2/mod/regex_posix/test_rx_common.s2)

Jump to:

* [The Regular Expressions APIs](#s2-mod-regex)
    * [Minor Achtung: Capture Limits](#s2-mod-regex-capture-limits)
* [Common Methods](#s2-mod-regex-common-methods)
    * [Minor Achtung: Arrays vs Tuples](#s2-mod-regex-lists)
    * [Big Achtung: Strings vs. Buffers](#s2-mod-regex-buffers)
* [Common Properties](#s2-mod-regex-common-properties)
    * [Module-level Properties](#s2-mod-regex-common-properties-module)
    * [Per-Instance Properties](#s2-mod-regex-common-properties-instance)
* Methods in More Detail
    * [`replace()`](#s2-mod-regex-replace)
* [POSIX Regex Specifics](#s2-mod-regex-posix)
    * [Compilation Flags](#s2-mod-regex-posix-compile-flags)
    * [Match-time Flags](#s2-mod-regex-posix-match-flags)
    * [Achtung: Locale-dependent Matching](#s2-mod-regex-common-locale)
* [JS Regex Specifics](../regex_js/)


<a id='s2-mod-regex'></a>
# The Regular Expressions APIs

This library has two separate loadable modules which provide regular
expression support. They have very-nearly-identical script-side APIs,
the most notable exception being the set of regex-flavor-specific
flags each API offers for configuring compilation and execution of
regexes.

**This page describes the common regex APIs and the APIs for the POSIX
flavor of regular expressions.** [The specifics of the JavaScript
flavor are documented in ../regex_js/](../regex_js/index.md). Note that
it is perfectly legal to load and use both of the JS modules at the
same time.

These modules each expose a single function which compiles regular
expression strings in the module's own particular grammar and returns
a module-specific type:

```s2
RegexType module( string pattern [, string compileFlags] )
```

The first parameter is a regular expression pattern in the module's
regex dialect.

The second parameter is an optional string describing module-specific
compilation flags, each one a single letter. The POSIX-specific
compilation flags are described [in a subsection
below](#s2-mod-regex-posix-compile-flags) and those for JS flavor are
described [in that module's
documentation](../regex_js/index.md#s2-mod-regex-js-compile-flags).
An empty string is legal for the flags.

The compilation function throws an exception if the pattern is
syntactically invalid for the given regex flavor or an invalid flag
letter is provided.

The compilation function is used like this:

```s2
const regcomp = s2.loadModule('/path/to/regex_posix.so');
// or ----------------------------------^^^^^^^^^^^ regex_js.so
const regex = regcomp( 'foo', 'i' );
```

<a id='s2-mod-regex-capture-limits'></a>
## Minor Achtung: Capture Limits

The JS and POSIX APIs differ in how they react if a regex contains
more capture groups than the compile-time limit (10, as of this
writing, *including* the full-string match (a.k.a. `$0`)).

The JS-flavor API will throw an exception when the regex is compiled
if it contains too many captures. (It has to in that step because of
how it stores the captures.)

The POSIX C API can execute such regexes but will only record as many
of the captures as the user (i.e. this module) has configured space
for. If more captures than that are found, they are considered for
matching purposes but are not captured. In order to keep some
semblance of compatibility with the JS API, the POSIX module will,
instead of following the native C behaviour, trigger an exception if a
given call to `exec()`, or any other method which performs string
matching via `regexec()`, encounters too many captures. We "would"
have the option of simply silently ignoring all captures past the
configured limit, but we fail for compatibility with the JS module.
That design decision is up for reconsideration later, but practice
implies that we should apply the stricter option until/unless we
later decide that the laxer approach would be more useful.

> Sidebar: the reason for the relatively low capture limit is because
increasing it increases the memory cost of every single JS-module
instance. The dynamic memory cost of the POSIX API is unaffected by
the capture count, but the JS module statically compiles in the
capture buffers into each regex instance, and each capture cost more
memory.

<a id='s2-mod-regex-common-methods'></a>
# Common Methods

The regex type returned from module is a different type but they have
nearly-identical interfaces. Their common methods and behaviours, as
well as any significant differences in behaviour, are described below.

```s2-member
void destroy()
```

Immediately frees all native resources used by this regex. That also
happens when garbage collection reaps the regex, but clients may force
it immediately with this method. After this method is called, calling any
regex methods on this object will throw an exception because the
underlying C-level regex instance no longer exists.


```s2-member
void eachMatch(string text, string|function callback [,string matchFlags])
```

For each match of this regex in the given text, this function calls
the given callback:

- If it is a function, it is passed the complete match text as its
  only argument.
- If it is a string, it is `eval`'d for each match.
- In both cases, the call/eval-local `this` refers to the regex
  object.

Any return/result value of the callback is ignored.

`matchFlags` may be any flags accepted by `exec()` or `replace()`,
noting that the [`$` and `E` `replace()`-specific
flags](#s2-mod-regex-replace) are automatically implied if the
callback is a *string* (because this function would be useless with a
string callback without those flags). The `E` flag is ignored if the
callback is a function. The `$` flag can be used to provide a callback
function access to sub-captures.

> *Achtung:* the POSIX regex C API does not support `eachMatch()` for
regexes which are compiled with the `s` (`NOSUB`) flag, and attempting
it will trigger an exception.


```s2-member
mixed exec(string text [, string matchFlags])
```

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
subsection](#s2-mod-regex-posix-match-flags) and those for JS are [in
that module's
documentation](../regex_js/index.md#s2-mod-regex-js-match-flags).

> *Achtung:* the POSIX `regexec()` C API cannot report the substring
position of matches for regexes which are compiled with the `s`
(NOSUB) flag (not even for the whole-match part), so this method will,
for such regexes, return `true`, instead of a list, on a match.


```s2-member
mixed matchAll(string text [, string matchFlags [, bool captureAll=false])
mixed matchAll(string text [, bool captureAll=false])
```

This function has two distinct modes:

- If `captureAll` is `false` (the default) then if a match is found, a
list of all complete match strings (not split into sub-captures) is
returned. i.e. a single-dimensional list of strings.

- If `captureAll` is `true` then if a match is found, a list of lists
is returned, with each sub-list having the same structure as the
result of `exec()`. i.e. `[["full match 1","capture 1",..."capture
N"], ["full match 2",...] ...]`.

In both cases, if no match is found a falsy value is returned.

`matchFlags` is an optional flag to change how matching works,
exactly as described for `exec()`.

Note that for the two-argument form, the 2nd argument is only
recognized as the `captureAll` toggle if it is a genuine boolean, not
an arbitrary truthy/falsy value. i.e. `matchAll("blah",true)` will
toggle `captureAll` on, but `matchAll("blah", "1")` would treat the
2nd argument as the `flags` and use the default value for
`captureAll`.

> *Achtung:* the POSIX regex C API does not support `matchAll()` for
regexes which are compiled with the `s` (`NOSUB`) flag, and attempting
it will trigger an exception.

> *Minor Achtung:* remember that captures for the POSIX regex API are a
pain in the butt when compiled with the `BASIC` flag.


```s2-member
string replace(string text, mixed replacement
               [, int maxReplacements = 0 [, string matchFlags]])
string replace(string text, mixed replacement, string matchFlags])
```

Replaces instances of the regex's match in the given string with the
given replacement. This function is described in detail
in [its own section](#s2-mod-regex-replace).

> *Achtung:* the POSIX regex C API does not support `replace()` for
regexes which are compiled with the `s` (`NOSUB`) flag, and attempting
it will trigger an exception.

```s2-member
array split(string text [, int limit = -1 [, string matchFlags]])
array split(string text, string matchFlags)
```

This works similarly to `"astring".split("pattern")` except that it
splits the first argument on this regex's pattern.

If a `limit` is 0 or greater, it captures, at most, that many
elements, otherwise it captures all it can (just like
`string.split()`). (Yes a limit of 0 is valid, but i have no idea
why - JS allows it.)

`matchFlags` is an optional flag to change how matching works,
exactly as described for `exec()`.

Note that, like `string.split()`, this treats matching separators at
the start and end of the input as empty entries at the start resp.
end of the result list.

Note also that while `string.split('')` splits the string into its
componenent characters, there is no equivalent with this API because
the API forbids empty regexes. Splitting on a regex of `"."` will
behave much differently, treating each character as a separator and
returning a list of what's *between* those separators (empty strings).

> *Achtung:* the POSIX regex C API does not support `split()` for
regexes which are compiled with the `s` (`NOSUB`) flag, and attempting
it will trigger an exception.


```s2-member
bool test(string text [, string matchFlags])
```

Works like `exec()` but returns `true` if the given text matches the
regex, else `false`. i.e. it does not allocate a data structure for
the result, so it's more efficient (but less informative) than
`exec()`.


<a id='s2-mod-regex-lists'></a>
## Minor Achtung: Arrays vs Tuples

The regex APIs explicitely do not use the terms "array" or "tuple"
when refferring to list-type return values. Any given routine may
return either in any context where a list is returned, that type may
differ between versions of the modules, and the different regex
implementations may differ in this regard. i.e. it's possible that the
`exec()` method for one module returns a tuple while the other returns
an array.

For 99+% of use client-side cases, it doesn't make a difference either
way: the two list types are used the same way until/unless the client
wants to change their size or sort them or perform some other
array-only operation. Element access and [`foreach()`
iteration](../../manual/keyword-flow-control.md#keyword-foreach) are
the same for both list types, and those are the only operations
*normally* performed on string-matching APIs of this sort.

FWIW, the "preferred" return type is a tuple, as they're
memory-lighter, but that's only possible when a given function knows,
in advance, how many slots the result list will need (which isn't
always the case).


<a id='s2-mod-regex-buffers'></a>
## Big Achtung: Strings vs. Buffers

*Most* places where the regex APIs accept a string, they also accept a
Buffer, but results are undefined if such a buffer contains non-String
data. Routines which accept accept callbacks (e.g. to iterate over
matches or for implementing dynamic string replacement) disallow
buffers as input because it would be disastrous if the buffers were
modified by/via such callbacks while this API is traversing its
contents.

<a id="s2-mod-regex-common-properties"></a>
# Common Properties

<a id="s2-mod-regex-common-properties-module"></a>
## Module-level Properties

The compilation functions each have a `flavor` property which
names that implementation ("flavor") of regexes. It has the value
`"posix"` or `"js"`.

### `instancePrototype`

Each regex module's compilation function has a property named
`instancePrototype`, the prototype which gets assigned to each new
regex instance. This can be used to modify the behaviour of regexes
without having to first instantiate one to get at its prototype.

An example of such modification would be to extend the *string* prototype
in order to be able to make use of certain regex functionality. For example,
the following code replaces `string.split()` with a proxy which can make use
of either (or both) of the regex modules when a regex is passed as its first
argument:

```s2
// Assume that the module's name in this context is regcomp.
if(const S = "".prototype.split){
    // Proxy function:
    "".prototype.split = proc(/*pattern,limit*/){
        return argv.0 inherits X
                ? argv.0.split(this, argv.1|||0)
                : S.apply(this, argv);
    }using{S, X:regcomp.instancePrototype};

    // Demonstration:
    var m = "a;b;c".split(regcomp(" *; *"));
    assert 3 === m.#;
    assert 'c' === m.2;
    m = "A;B;C".split(";");
    assert 3 === m.#;
    assert 'C' === m.2;
}
```

If the above were performed for both modules, `string.split()` would
accept both types of regex, passing on the call if the first argument
is not the module's own type.

A similar proxy could be used to allow
`string.replace(pattern,replacement)` to accept `(regex,function)`
arguments:

```s2
if(const R = "".prototype.replace){
    "".prototype.replace = proc(/*needle,replacement*/){
        return argv.0 inherits X
                ? argv.0.replace(this, argv.1)
                : R.apply(this, argv);
    }using{R, X:regcomp.instancePrototype};

    assert 'abc' === "AbC".replace(regcomp('[A-Z]'), proc(x){return x.toLower()});
    assert 'abc' === "Abc".replace("A","a");
}
```

<a id="s2-mod-regex-common-properties-instance"></a>
## Per-Instance Properties

Each regex instance has the following standard properties assigned to it:

* `pattern` holds the original pattern string which was passed to the
  compile function.
* `flags` holds the string-form regex compilation flags.

The values of those properties can be used to save and restore a regex
for later use.

# Specific Methods in More Detail

<a id='s2-mod-regex-replace'></a>
## `replace()` 

```s2-member
string replace(string text, mixed replacement
               [, int maxReplacements = 0 [, string matchFlags]])
string replace(string text, mixed replacement, string matchFlags)
```

Replaces instances of the regex's match in the given string with the
given replacement. `replace()` only accepts strings, not buffers, as
input text.

High-level `replacement` values like objects and
arrays will be appended in JSON form, but any cycles in such
constructs will trigger an exception. If `replacement` is a function then:

1. It is passed the complete match text of each match and the result
   of the call becomes the replacement.
2. In the context of the callback, `this` refers to the regex instance.

If `maxReplacements` is passed in, it must be an integer. A value of 0
or less means unlimited, else the number of replacements is limited to
the given value.

`matchFlags` is an optional flag to change how matching works, exactly
as described for `exec()`, plus it supports the following string-form
flag letters which only work for this routine:

- `E` causes the replacement value to be `eval`'d for each
match. (This option isn't of any use unless the replacement is a string
and is ignored if the replacement is a function.) Its result becomes
the replacement text. Like in the function replacement form, `this`
resolves to the regex being operated on. This flag is most useful when
combined with...
- `$` causes all regex capture groups to be expanded into a list named
`$`, accessible from the replacement callback/eval. Index 0 is the
whole match, and subsequent captures are at the subsequent
positions. e.g. use `$.3` to get the third capture from within a
callback. `$` is local to the callback/eval scope, so it is not
available after `replace()` returns.

> Sidebar: we do not export symbols named `$0`...`$N`, as would be
more conventional vis-a-vis other regex APIs, because: (A) it would be
much less efficient to do so. (B) it would be possible to
inadvertently resolve, e.g., `$4` from an older scope (because of s2's
liberal [symbol lookup rules](../../manual/symbol-resolution.md)),
which cannot happen when referencing the captures via `$`.

When using a function for the replacement, the function is passed the
full string of each match. If the `$` `matchFlag` is used then
captures are available as described above. Without that flag, the
function may nonetheless perform replacement based on capture groups
by calling `exec()` on the regex from within the replacement
callback, like so:

```s2
// JS syntax:
const x = regcomp('\s*([a-z]+)\s*(;)?');
// POSIX syntax
const x = regcomp('[ \t]*([a-z]+)[ \t]*(;)?');
// Normalize inputs to upper-case and strip extraneous spaces:
affirm 'A;B;C' === x.replace( 'a\t  ;\tb; c', proc(oneMatch) {
    const m = this.exec(oneMatch);
    // At this point we know this regex matches the input (oneMatch),
    // so we don't need to bother checking whether m is falsy.
    return m.1.toUpper() + (m.2 ||| '');
} );
```

However, that requires running the regex *twice* on each matching part
of the input, so using the `$` flag is recommended if captures beyond
the whole-match capture are needed.

Here's a functionally equivalent example which uses the `$` flag to
make the captures available and the `E` flags to demonstrate using a
string as a callback body:

```s2
// Using the same regex as above:
affirm 'A;B;C' ===
  x.replace( 'a\t  ;\tb; c',
            eval=>{ $.1.toUpper() + ($.2 ||| '') },
            '$E' );
```

Remember that `eval=>{...}` does not evaluate its contents: it
*captures* them *as a string*. The `E` flag then causes that string
to be `eval`'d for each replacement.

> Sidebar: *no buffers as input?* Buffers are not allowed as input
here because, when used in conjunction with a replacement
callback/eval, it would be possible for the buffer to be modified
during traversal, which would lead to Undefined Behaviour. Working
around that (by moving its contents out of the way during traversal,
similar to how `s2.Buffer.evalContents()` works) would be rather
fidgety. Interestingly, though, passing a buffer as a replacement
value is legal because without a callback there is no risk of it being
modified during the replacement process. It might seem sane to permit
buffer inputs when the replacement is *not* a callback, but that could
still potentially backfire badly when invoked in certain convoluted
recursive contexts.


<a id='s2-mod-regex-posix'></a>
# POSIX Regex Specifics

<a id='s2-mod-regex-posix-compile-flags'></a>
## POSIX Compilation Flags

See also: [JS compilation flags](../regex_js/index.md#s2-mod-regex-js-compile-flags)

Regex compilation may be modified by providing a string of
single-letter flags. Unknown flags cause an exception to be
thrown. The flags for this module are listed below. The upper-case
names listed next to each flag are the C-level names for the flags,
with the exception of `BASIC`, which is does not exist at the C level
(C-level POSIX regexes default to `BASIC` mode unless the `EXTENDED`
flag is used, whereas this API does the opposite because basic-style
regexes are not terribly useful).

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
`s`ub-expressions.  *Curiously*, this also means that the C-level APIs
do not report the whole-match part of a match. That is, when this flag
is in effect, the POSIX C APIs can tell us that there's a match but
not *where* that match is. Thus regexes compiled with this flag are
not usable with APIs which require knowing *where* a match is found,
e.g. `matchAll()`, `replace()`, and `split()`.


<a id="s2-mod-regex-posix-match-flags"></a>
## POSIX Match-time Flags

See also: [JS match-time flags](../regex_js/index.md#s2-mod-regex-js-match-flags)

Like compilation flags, flags which change regex matching behaviour
at match-time may be provided as a string of letters describing the
flag(s):

- `"b"` (`NOTBOL`): do not treat the beginning of the input string as the
`b`eginning of a line. That is, do not match `^X` at the start of the
input.

- `"e"` (`NOTEOL`): do not treat the end of the input string as the `e`nd of a
line. That is, do not match `X$` at the end of the input.


<a id='s2-mod-regex-common-locale'></a>
## Achtung: Locale-dependent Matching

The POSIX regex API uses locale-dependent pattern matching and this
module does not set the software's locale because it cannot know if
the overlying software has done so, or what effects changing that
setting might have on the rest of the app. Thus, unless the software
or environment changes the locale, these regexes will use the "C"
locale for matching purposes and will likely not match non-ASCII
strings or patterns.


# Potential Regex Module TODOs

The following list applies to both of the modules, though priority
would be given to the JS-style module because, frankly, that regex
flavor is more familiar to this developer.

- It "should" be possible to extend this API to include data like
  [JS's RegExp's
  lastIndex](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp),
  in order to be able to apply the regex to a given string multiple
  times and iteratively find multiple matches. Internally
  applying/accounting for `lastIndex`, however, would break current
  uses of the API, where a given regex instance can be used any number
  of times on any number of strings. Thus this would probably involve
  adding a new regex compilation or exec option, e.g. "multi-mode".
  The index of the end of the last match would need to be counted in
  characters, not bytes, which would slow down its calculation
  considerably. That could be optimized for strings which we know to
  be ASCII (cwal records that for string values), but for Buffer-based
  input, as opposed to strings, we don't know in advance if the data
  is ASCII-only, and have no choice but to iterate over all characters
  on each execution. Hmmm.. if the `lastIndex` is only used
  internally, not exposed to scripts, we could get away with using
  byte offsets, which would be faster. Hmmmm.
