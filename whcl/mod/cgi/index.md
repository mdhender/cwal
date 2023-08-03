# whcl CGI
#### ([&#x2b11;Main Module Docs](../))
# CGI: You know, for whcl-powered CGI scripts!
<style>@import url(../../../doc/fossil-doc.css)</style>
<script src="../../../doc/highlightjs/highlight-cwal.min.js"></script>

- Source code: [](/dir/whcl/mod/cgi?ci=trunk)

<!--

- Test/demo code: [](/finfo/whcl/mod/cgi/test.whcl)

-->


Jump to:

- [Intro](#mod-cgi-intro)
  - [Loading the CGI module](#mod-cgi-loading)
- [CGI Environment Vars](#mod-cgi-env)
- [CGI Methods](#mod-cgi-methods)
- [Non-function Members](#mod-cgi-non-methods)
  - [Request Data (`cgi.request`)](#mod-cgi-request)
- [Example Usage](#mod-cgi-example)


<a id="mod-cgi-intro"></a>
# Intro

The CGI module provides a basic framework for implementing CGI scripts
using whcl. It's possibly missing a few useful features. What's there
works reasonably well, though. Its immediate code predecessor (from
which this one was forked) is in active use in several of my own sites
to power JSON-centric back-end services.

The CGI loadable module, when its `init` method is called, populates
script-space with various CGI input data and utilities for working
with it. It is intended to be used for implementing CGI applications
entirely in whcl, and its direct code-predecessor was used to
implement a number of CGI-based backend apps for whcl's developer's
own sites. This module provides a *basis* for writing CGI
applications, but *not a complete framework*. All but the lowest-level
work is intended to be done in script code (e.g. routing/dispatching,
output buffering management (assisted by the C layer), page rendering,
...).

Its main services/features include:

- Converts all GET parameters and cookies into a script-usable form.
- Makes incoming POSTed JSON or form-urlencoded data available to the
  script.
- Intended to be used with [the output buffering
  API](../../manual/api-ob.md) (`whcl.ob.*`), to enable setting of HTTP
  headers (e.g. cookies) at arbitrary points throughout the page
  generation process (headers must be output first, but buffering
  allows headers to be changed throughout the generation of the page).


<a id="mod-cgi-loading"></a>
## Loading the CGI module:

If this module is built in to libwhcl then:

```whcl
decl -const CGI [whcl load-api cgi]
```

If it's a shared library:

```whcl
decl -const CGI [whcl.load-module '/path/to/cgi.so']
```

In order to properly accommodate this module's usage via both a DLL
and statically built in to an app, its `init` method must be called
once before its other methods are used. This module can only work a
single time, however. It may only be initialized once and its
`respond` method only works once.

<a id="mod-cgi-env"></a>
CGI Environment Variables
============================================================

The following description of the relationship of certain CGI
environment variables is taken verbatim from code comments in the
[Fossil SCM project](https://fossil-scm.org):

REQUEST_URI, PATH_INFO, and SCRIPT_NAME are related as follows:

>  REQUEST_URI == SCRIPT_NAME + PATH_INFO

Or if QUERY_STRING is not empty:

> REQUEST_URI == SCRIPT_NAME + PATH_INFO + '?' + QUERY_STRING

Where "+" means concatenate.

Sometimes PATH_INFO is missing and SCRIPT_NAME is not a prefix of
REQUEST_URI. (See <https://fossil-scm.org/forum/forumpost/049e8650ed>.)

SCGI typically omits PATH_INFO.  CGI sometimes omits REQUEST_URI and
PATH_INFO when it is empty.

CGI Parameter quick reference:

```
                               REQUEST_URI
                       _____________|________________
                      /                              \
https://fossil-scm.org/forum/info/12736b30c072551a?t=c
\___/   \____________/\____/\____________________/ \_/
  |           |          |             |            |
  |       HTTP_HOST      |        PATH_INFO     QUERY_STRING
  |                      |
REQUEST_SCHEMA         SCRIPT_NAME
```



<a id="mod-cgi-methods"></a>
CGI Methods
============================================================

This module defines the methods described in this section.  The intent
is that the CGI object be fleshed out via script code, e.g. with
convenience methods for working with the CGI environment's GET/POST
data, as well as any app-specific routing functionality. Those sort of
features are *much* simpler to implement and evolve in script code
than in C.


http-status
------------------------------------------------------------

Usages:

- getter: `http-status`
- setter: `http-status integerCode [message = code-dependent default]`

Gets or sets the HTTP response status code. The default, if not explicitly
set, is 200 (success). If called with no arguments (the getter) it
returns the current status code, else it returns this object. When
passing a message, it must not contain any newlines, nor should it be
"unduly long", else it may corrupt the response header


init
------------------------------------------------------------

Usage: `init [object options]`

To accommodate this module's inclusion in both a DLL
and statically linked in, this module must be manually initialized, by calling
this function, before using any of its other methods. Returns this
object and throws if called more than once. It optionally takes an
object with configuration options for the CGI framework:

- `pushOb (bool, default=true)`: If truthy, push an output buffer
  ("ob") layer. If false, the client must ensure that output buffering
  is enabled before generating body output (e.g. by using `echo`). It
  is necessary to buffer the body so that HTTP response headers can be
  sent in the proper order (before the body, but new headers may be
  created up until the body is output).
- `importEnv (bool, default=false)`: If truthy, it imports all
  environment variables into `cgi.request.ENV`, otherwise it only
  imports those which are specified for CGI scripts. Any which have
  no value are set as empty strings. The core environment vars are:
  - `AUTH_CONTENT`
  - `AUTH_TYPE`
  - `CONTENT_LENGTH` (noting that the content will have already been
     consumed by the native-level CGI bits).
  - `CONTENT_TYPE`
  - `DOCUMENT_ROOT`
  - `HTTP_ACCEPT`
  - `HTTP_ACCEPT_ENCODING`
  - `HTTP_COOKIE`
  - `HTTP_HOST`
  - `HTTP_IF_MODIFIED_SINCE`
  - `HTTP_IF_NONE_MATCH`
  - `HTTP_REFERER`
  - `HTTP_SCHEME`
  - `HTTP_USER_AGENT`
  - `HTTPS`
  - `PATH`
  - `PATH_INFO`
  - `QUERY_STRING`
  - `REMOTE_ADDR`
  - `REQUEST_METHOD`
  - `REQUEST_URI`
  - `REMOTE_USER`
  - `SCGI`
  - `SCRIPT_DIRECTORY`
  - `SCRIPT_FILENAME`
  - `SCRIPT_NAME`
  - `SERVER_NAME`
  - `SERVER_PORT`
  - `SERVER_PROTOCOL`
  - `SERVER_SOFTWARE`


header-timestamp
------------------------------------------------------------

Usage: `header-timestamp [unixEpochTime = -1]`

For a given Unix epoch time, this returns the
[RFC-7231](https://tools.ietf.org/html/rfc7231) time string. A
negative time value is interpreted as the current time. This time
format is often seen in HTTP headers.


respond
------------------------------------------------------------

Usage: `respond [bool exit=false]`


Outputs all pending output, submitting the HTTP headers first,
followed by any body content living in the output buffering subsystem
(it flushes each buffering level, one at a time - all levels pushed by
`init` or since `init` was called). This must only be called one
time, and calling it cleans up the module internals. Calling any
C-native methods on this object after this will trigger an
exception. If it is passed a truthy value, it triggers an exit, such
that control never returns to the calling script. After this object
answers its request, there's really no more for it to do, in any case,
and exiting immediately seems easier for script code than handling
waiting-to-unwind client code which is expecting to be able to pop
output buffers which are no longer there.


respond-passthrough
------------------------------------------------------------

Usage: `respond-passthrough filename [exit=false]`

An alternative to `respond` (see above) intended for efficiently
serving static files. This function bypasses all output buffering,
emits any pending output headers, then streams the contents of the
given filename directly to the lowest-level cwal-defined output
channel (typically sent to stdout, but that can be configured by the
cwal client). See `respond` for details and implications of this
function cleaning up the native object, as well as a description of
the 2nd parameter.


set-content-type
------------------------------------------------------------

Usage: `set-content-type string`

Sets the Content-type header to the given value, replacing any
previous one. Please use this function instead of setting that header
directly so that we can avoid any case-sensitivity problems (the HTTP
standard is case-insensitive with regards to the header keys, but this
C code is not!). Returns this object.


set-cookie
------------------------------------------------------------

Usage: `set-cookie key value`

Sets the given cookie. Set the value to `null` to cause it to
expire. Setting a cookie multiple times replaces the to-send value
each time. By default, all cookies are session cookies, expiring when
the browser is closed. Note that this simply queues up a cookie for
the `respond` method, and does not change any visible cookie
state. Its value will not be available in `cgi.request.COOKIES` until
the client sends it again. Any basic value type is fine as a cookie
value, but if the value is container type then it is expected to have
the following structure (all listed properties are optional and extra
properties are ignored):

```whcl
{
  value mixed     # Use null/undefined to unset the cookie
  domain string
  path string
  expires integer # Unix Epoch time. null/undefined/values<=0 expire the cookie.
  secure bool
  httpOnly bool
  sameSite string # https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie/SameSite
}
```

Those properties all correspond to [standard cookie
attributes](https://en.wikipedia.org/wiki/HTTP_cookie#Cookie_attributes).


set-header
------------------------------------------------------------

Usage: `set-header key value`

Queues up the given HTTP response header to be sent via `respond`,
overwriting any prior value. It technically accepts any value type,
but your mileage may vary with non-strings/numbers. Returns this
object. Minor achtung: because HTTP headers are case-insensitive, but
this native code is not, this implementation lower-cases all headers
so that clients do not have to be 100% careful to use consistent
casing.


set-expires
------------------------------------------------------------

Usage: `set-expiresdunixEpochTime`

Sets an "Expires" HTTP header to the specified UNIX timestamp. A
negative value sets it for some arbitrarily long time in the
future. Each call to this function overwrites any previous call's
header value. Returns this object.


urldecode
------------------------------------------------------------

Usage: `urldecode string`

Returns the URL-decoded copy of the given string. Note that the API
decodes incoming request data automatically, so this is not normally
needed. Intended for decoding individual elements at a time, not a
whole query string.


urlencode
------------------------------------------------------------

Usage: `urlencode string`

Returns the URL-encoded form of the given string. Use this for
escaping, e.g. values for use in hyperlinks.  It is intended to be
passed individual values, not a whole query string (it will URL-escape
the query string's special characters!).


<a id="mod-cgi-non-methods"></a>
Non-function Members
============================================================


<a id="mod-cgi-request"></a>
Request Data (`cgi.request`)
------------------------------------------------------------

Data can arrive to the CGI script via several paths, and those data
are encapsulated in the `cgi.request` Object.


- `COOKIES`: this object contains any HTTP cookies sent by the client,
  in the form of key/value pairs. COOKIES is undefined if no cookies
  were sent.

- `ENV`: this object only gets installed if `init` is explicitly
  told to (see `init` above). Contains all variables from the
  C-level `environ` array. Note that `ENV` values are always strings,
  even if they look like numbers.  Apply `unary +` to a numeric value
  to convert a value to a number (or 0 if it's not a number). Note
  that this object may contain security-relevant data (e.g.
  system-level paths) and should not generally be exposed to remote
  users via script code.

- `GET`: `undefined` if no URL parameters are passed in, otherwise
   this Object contains any GET parameters passed to the URL. If
   parameters are named like `name[]` then the value is an Array and
   each new instance of that name is appended to that array. It does
   not support indexes/names inside the brackets, nor nested arrays,
   however. See `ENV` (above) for notes regarding the "stringiness" of
   the values. Though GET parameters are technically always strings,
   this API tries to convert them to their "apparent" cwal type
   (number, boolean, etc.). A GET parameter with no '=' is considered
   to be a flag with a boolean true value.

- `POST`: The module accepts either raw UTF-8-encoded JSON or
  form-urlencoded data as POST data (which it internally transforms
  into a JSON-like construct). That data will then be set here. `POST`
  is undefined if no `POST` data was submitted to the script or parsing
  it failed (silently). Note that form-urlencoded data are technically
  always strings, even if they look like numbers, but this module
  tries to convert them to their "apparent" type (numbers, booleans,
  etc.). JSON data retains whatever data type(s) the JSON was
  generated with. Note that it only accepts Arrays or Objects as
  top-level JSON values (as [RFC 4627](http://www.ietf.org/rfc/rfc4627.txt) specifies) and not
  arbitrary single values (as some JSON parsers allow). The API
  optimistically assumes that inbound content with any of the types
  "application/json", "application/javascript", or "text/plain" might
  be JSON. Any other type of content which arrives via POST, except
  for form-urlencoded, is not consumed. Invalid JSON is discarded
  without any sort of error (downstream code which expects it may fail
  loudly, of course).

- `user`: Reserved for client-side use. Script-side frameworks which
  use user authentication are encouraged to store relevant information
  about the current user in the request object for easy access
  throughout scripts.

- TODO? `HEADERS`: if someone can tell me how to access arbitrary
  request headers via CGI, i'll add those to the request object. i'm
  not actually sure we get those at this level, other than the ones
  which CGI specifies go in the environment? How to get X-MyHeader
  headers?

Sidebar: Why uppercase names? It's inherited from PHP, which uses
similarly-named globals for this same purpose. It also incidentally
avoids any confusion with the built-in `Object.get` method.  It's
worth noting that the CGI object removes the prototypes from the
`GET`, `POST`, etc. objects, to ensure that clients don't
inadvertently get (as it were) any inherited properties when checking
for request data.

<a id="mod-cgi-example"></a>
Example Usage
============================================================

Here is an example, using the interactive shell, which demonstrates the
general workflow for a CGI app:

```bash
[stephan@host:~/...]$ QUERY_STRING='a[]=1%202&a[]=2' whclsh
...
whclsh> decl -const c [whcl load-api cgi]
whclsh> c init; # Required before use. init pushes an output buffer
                # onto the stack by default!
whclsh> c set-cookie one 1; # headers get buffered separately
whclsh> c set-content-type 'application/json'
whclsh> echo [c.request.to-json 2]; # goes to the output buffer
whclsh> c respond
        # ^^^^ sends headers first, then flushes pending buffered content
Status: 200 OK
Content-type: application/json
Set-Cookie: one=1

{"GET": {"a": [
      "1 2",
      "2"]
    }}
```

The use of the output buffering API is important so that the content
type, cookies, and other headers may be changed throughout the life of
the app. Once any output is sent, headers may no longer be set/changed.
The `respond` call will output any headers first and flush any pending
buffered output (any number of buffering levels). `respond` must only be
called a single time, and calling it actually shuts down/cleans up the
CGI module. Calling any of its methods after `respond` will cause an
exception because they will no longer be able to find their (already
destroyed) C-side data.


