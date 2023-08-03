# s2: Time API
#### ([&#x2b11;Table of Contents](./)) ([&#x2b11;Misc. Features Index](misc-index.md))
<a id="misc-time"></a>
# s2 Time API

This API gets installed by [s2sh](s2sh.md) as `s2.time`. Client
applications are free to install it (or not) under a different
name.

This object contains a few time-related functions. None of its members
require that this object be their `this`, so they can be copied for
use in other contexts:

This object provides the following features:

```s2-member
int sleep(integer seconds)
```

Works like C's `sleep(3)`.

```s2-member
void mssleep(integer milliseconds)
```

Tries to sleep for the given number of milliseconds. Throws on
error.

```s2-member
integer time()
```

Returns the current Unix Epoch time, as per C's `time(3)`. If the
platform's `cwal_int_t` is too small for the value then a double is
returned instead of an integer (note that that can only happen on
32-bit builds and then might only work properly up to 48 bits of
integer time precision).

```s2-member
string strftime(string format
                [, int unixTime = current time
                [, useLocalTime=false]])
```

Formats Unix Epoch timestamps. The 3rd parameter determines whether
the time value is treated as UTC (the default) or the local time
zone. The full list of formatting options, and a code snippet for
trying them out:

```
const formatList = {
'%%': 'Literal percent',
'%a': 'Abbr. weekday name',
'%A': 'Full weekday name',
'%b': 'Abbr. month name',
'%e': 'day of month, blank-padded',
'%h': 'Same as %b',
'%B': 'Full month name',
'%c': '??? "appropriate date/time representation"',
'%C': 'Century as two digits',
'%d': 'day of month, 01-31',
'%D': '%m/%d/%y',
'%E': 'ignored',
'%H': 'hour, 00-23',
'%I': 'hour, 01-12',
'%j': 'day of year, 001-366',
'%k': 'hour, 0-24, blank-padded',
'%l': 'hour, 1-12, blank-padded',
'%m': 'month, 01-12',
'%M': 'minute, 00-59',
'%n': '\\n', // only double-escaped for presentation purposes!
'%O': 'ignored',
'%p': '"am" or "pm"',
'%r': '%I:%M:%S %p',
'%R': '%H:%M',
'%S': 'seconds, 00-61',
'%t': '\\t', // only double-escaped for presentation purposes!
'%T': '%H:%M:%S',
'%u': 'ISO-8601 weeday as number 1-7, 1=Monday',
'%U': 'week of year, Sunday as first day',
'%v': 'dd-bbb-YYYY',
'%V': 'ISO-8601 week number',
'%w': 'weekday, 0-6, Sunday=0',
'%W': 'week of year, Monday as first day',
'%x': '??? "appropriate date representation"',
'%X': '??? "appropriate time representation"',
'%y': 'year, 00-99',
'%Y': 'year with century',
'%a, %d %b %Y %H:%M:%S +0000': 'RFC-822 (timezone/offset is hard-coded!)'
};

const now = s2.time.time(),
      strftime = s2.time.strftime;
foreach(@formatList.propertyKeys().sort()=>k){
    print("Format:",k,'\n\t',formatList[k],'\n\t ==>',strftime(k, now))
};

print('Current time UTC:',strftime('%Y-%m-%d %H:%M:%S', now));
print('Current local time:',strftime('%Y-%m-%d %H:%M:%S', now, true));
```

> Sidebar: this implementation of `strftime()` dates back to the early
1990s and contains several option descriptions which don't make all
that much sense, e.g. `%x`.
