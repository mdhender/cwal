# whcl: Time APIs
##### ([&#x2b11;Table of Contents](./)) ([&#x2b11;API Index](api-index.md))
<style>@import url(../../doc/fossil-doc.css)</style>
<script src="../../doc/highlightjs/highlight-cwal.min.js"></script>

<a id="api-time"></a>
whcl Time API
============================================================

This API contains functions for working with time, all
of which may or may not function on any non-Unix-like platform. None
of its members require that this object be their `this`, so they can
be copied for use in other contexts.

This API may be installed using:

`whcl install-api time`

or calling `whcl_install_time()` from C code.

It adds a `whcl[time]` object with the API described below.

<a id=`time-method-mssleep'></a>
mssleep
------------------------------------------------------------

Usage: `mssleep integer`

Tries to sleep for the given number of milliseconds. Throws on error.


<a id=`time-method-sleep'></a>
sleep
------------------------------------------------------------

Usage: `sleep integer`

Works just like C's `sleep(3)`.

<a id=`time-method-strftime'></a>
strftime
------------------------------------------------------------

Usage: `strftime formatTtring [unixTime=now [useLocalTime=false]]`

Formats Unix Epoch timestamps. The 3rd parameter determines whether
the time value is treated as UTC (the default) or the local time
zone. The full list of formatting options, and a code snippet for
trying them out can be found in [this demo
script](/doc/$CURRENT/whcl/toys/demo-strftime.whcl?mimetype=text/plain).


<a id=`time-method-time'></a>
time
------------------------------------------------------------

Usage: `time`

Returns the current Unix Epoch time, as per C's `time(3)`. If the
platform's `cwal_int_t` is too small for the value then a double is
returned instead of an integer (note that that can only happen on
32-bit builds and then might only work properly up to 48 bits of
integer time precision).
