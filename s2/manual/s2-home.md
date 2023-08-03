# s2: S2_HOME
#### ([&#x2b11;Table of Contents](./))
# There's No Place Like `S2_HOME`

This section describes one common convention and suggestion for where
to install s2-related files. These are not rules enforced by the
framework, just suggestions borne of long-time experience.

In my many wanderings with s2, the following installation structure
has evolved to serve me well:

-  The [`s2sh` wrapper script](s2sh.md#s2sh-wrapper-script) is installed as
    `~/bin/s2sh` and `~/bin` is in my `$PATH` (as it has been (no pun
    intended) since the 1990s).
-  The directory `~/s2` acts as s2's "home base", and the `s2sh` wrapper
    script defines the `S2_HOME` environment variable with that value.

My `${S2_HOME}` contains:

- `s2sh.bin`: the compiled `s2sh` binary which the [`s2sh` wrapper
  script](s2sh.md#s2sh-wrapper-script) proxies.
- `require.d/`: holds my [require.s2](../mod/require/)
  scripts. The `s2sh` wrapper script sets the `S2_REQUIRE_PATH`
  environment variable to include this directory, and [`require.s2` uses
  that environment variable](/finfo/s2/require.d/require.s2) to configure
  itself.
- `cgi/`: is where [s2cgi](../mod/cgi/) lives. Several of my websites
  use this for various JSON APIs.
- `mod/`: where [compiled loadable modules](../mod/) (DLLs) go. The `s2sh`
  wrapper script sets `S2_MODULE_PATH` to include this directory and
  a script-implemented wrapper for `s2.loadModule()` uses that path to
  look for DLLs.
