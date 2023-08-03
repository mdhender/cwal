# s2: s2sh2
#### ([&#x2b11;Table of Contents](./))
# s2sh2: The *Other* s2 Shell

See also: [](s2sh.md)

<a id="intro"></a>
# `s2sh2`

`s2sh2` is a standalone overhaul and cleanup of the venerable
[s2sh](s2sh.md), started in early February, 2020. It provides
essentially the same functionality - a front-end into the s2 scripting
engine - but with the following notable (or at least mentionable)
differences:

- First and foremost, the way it processes command-line options has
been rewritten from scratch, using a *highly over-engineered* argument
parsing system which provides more conventional/flexible CLI arg
handling. The names of the flags vary wildly from s2sh's, which is
s2sh2's primary incompatibility with its predecessor. (Their
functionality is identical, though.)

- The help system is more friendly, breaking the options into "tiers"
to avoid overwhelming the user[^1] with *Too Much Info*.

- The `S2` [user-defined keyword](keywords.md#keyword-define) (with a
capital `S`) refers to the same object as the historical `s2`
(lower-case) global variable, but resolves more quickly than the
latter. Unfortunately, exposing the historical `s2` symbol this way
would break a great many scripts which create same-named scope-local
aliases to it as a lookup speed optimization.

- [Loadable modules](../mod/) built *statically* into s2sh have
historically been installed directly into the `s2` object using their
module's name. Groups of related modules may now get installed into
their own namespace. e.g. `s2.regex_posix` and `s2.regex_js` are now
installed as `s2.regex.posix` resp. `s2.regex.js`, without retaining
their historical names.

- `s2.ARGV` gets fully populated regardless of whether or not any
script-side flags are passed in to the app. Practice has shown that
the predecessor's behaviour of only populating it when there are
flags, while *technically* better, is a pain in the backside in
client scripts. ("The road to hell," and all that.)


As of this writing (2020-02-03), the shells are, aside from the
differences listed above, functionally compatible. See [the s2sh
docs](s2sh.md) for much more information about these shells, e.g. how
to statically link in loadable modules or extend s2sh\[2] using
client-side C code.

# Footnotes

[^1]: That'd be me.

