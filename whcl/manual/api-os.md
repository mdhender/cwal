# whcl: OS-level APIs
##### ([&#x2b11;Table of Contents](./)) ([&#x2b11;API Index](api-index.md))
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

<a id="api-os"></a>
whcl OS-level APIs
============================================================

This API contains functions related to the underlying operating
environment. None of its members require that this object be their
`this`, so they can be copied for use in other contexts.

This API may be installed using:

`whcl install-api os`

or calling `whcl_install_os()` from C code.

It adds a `whcl[os]` object with the API described below.

<a id='fs-method-getenv'></a>
getenv
------------------------------------------------------------

Usage: `getenv VARNAME [defaultValue=undefined]`

Searches for the given environment variable and returns it
or, if no match is found, returns the given default value, or
`undefined` if no default is provided.
