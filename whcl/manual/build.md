# whcl: Building whcl
##### ([&#x2b11;Table of Contents](./))
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

<a id="canonical"></a>
Canonical Build
============================================================

whcl's canonical build tree is part of [the cwal project](/) and it
builds as part of that main build process. That build process is
intended soley for whcl's own development. The build approach for use
client-side use is...


<a id="amalgamation"></a>
The Amalgamation Build
============================================================

The amalgamation build is a concatenation of the whcl source files
into a small collection of files which can be dropped directly into
client source trees, as opposed to being managed at the system level:

- `libwhcl.h` and `libwhcl.c` are the main library and include the
  underlying libcwal amalgamation. These hold everything needed for
  the library itself, but see below for details about changing compiled-in
  configuration defaults.
- `whclsh.c`, `cliapp.h`, and `cliapp.c` make up the shell app. Optionally,
  `whclsh_extend.c` may be provided by clients. to add functionality to
  the shell without modifying it. Building the shell is covered in
  [the whclsh docs](whclsh.md).

With the amalgamation files, building libwhcl is as easy as:

> ```shell
cc -c -I. libwhcl.c
```

(Noting that the canonical copy compiles warning-free (until gcc adds
yet more warnings to their `-Wall` flag), so this one should as well.)

Linking it requires the C99 math library (`-lm`).

Changing Compile-time Configuration Options
------------------------------------------------------------

libwhcl has numerious compile-time configuration options which may
influence which features get compiled in (depending mostly on the
availabity of the underlying support on the client platform). By
default it necessarily assumes a highly conservative set of config
options, but client code may define various configuration options
via one of the following approaches:

1. Define the `HAVE_CONFIG_H` macro to tell `libwhcl.h` to include
   `"config.h"`.
2. Define the `HAVE_AUTOCONFIG_H` macro to tell `libwhcl.h` to include
   `"autoconfig.h"`.
3. Define the macro `WHCL_CUSTOM_CONFIG_H=filename.h` to have it include
   `"filename.h"`.

The first two options are preferred because they are processed at the
libcwal level, long before libwhcl gets to try to include anything.

The complete list of compile-configurable macros can be found in
[whcl_config.h.in](/file/include/wh/cwal/whcl/whcl_config.h.in), and
any of the macros named `*_HAVE_*` redefined via one of the
above-listed approaches will override any from that file.
