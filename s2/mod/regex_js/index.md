# s2 JavaScript-style Regexes
#### ([&#x2b11;Main Module Docs](../))
# JavaScript-style Regular Expressions

- Source code: [](/dir/s2/mod/regex_js?ci=trunk)
- Test/demo code:
  - [](/finfo/s2/mod/regex_js/test.s2)
  - [](/finfo/s2/mod/regex_posix/test_rx_common.s2)

------------------------------------------------------------------------

***ACHTUNG (2020-03-01)***: since this plugin was written, numerous
regex bugs have been discovered in the upstream JS-regex library upon which
it is based. In short, its results currently *cannot be trusted*:

<https://github.com/ccxvii/mujs/issues/130>


------------------------------------------------------------------------

**This library has two distinct regular expression modules with
nearly-identical APIs:**
[the common features are described in the POSIX regex module docs](../regex_posix/) and this page
describes only those features peculiar to this flavor.
      
This module, ported in from [Tor Andersson's MuJS
project](https://github.com/ccxvii/mujs), provides support for
JavaScript-style regular expressions, noting that *only the pattern
syntax* is JS-like, not the API of the compiled RegExp objects. e.g.
these cannot (unlike in JS) be used to iterate over a string looking for
multiple matches. (That said, the first time i desperately want/need
such a feature, something approximating it will be added!)


<a id="s2-mod-regex-js-compile-flags"></a>
## JS-flavor Compilation Flags

See also: [POSIX compilation flags](../regex_posix/index.md#s2-mod-regex-posix-compile-flags)

Regex compilation may be modified by providing a string of
single-letter flags. Unknown flags cause an exception to be
thrown. The flags for this module are listed below. The upper-case
names listed next to each flag are the C-level names for the flags.

- `"i"` (`ICASE`): make the regex case-`i`nsensitive.

- `"n"` (`NEWLINE`): tells the `$` end-of-pattern anchor (e.g. `foo$`)
  to also match at the end of a line (i.e. at a `n`ewline
  character). e.g. with this flag, the input text `foo\nbar` would
  match the pattern `foo$`, but it would not match without this flag.

<a id="s2-mod-regex-js-match-flags"></a>
## JS-flavor Match-time Flags

See also: [POSIX match-time flags](../regex_posix/index.md#s2-mod-regex-posix-match-flags)

Like compilation flags, flags which change regex matching behaviour
at match-time may be provided as a string of letters describing the
flag(s):

- `"b"` (`NOTBOL`): do not treat the beginning of the input string as the
`b`eginning of a line. That is, do not match `^X` at the start of the
input.
