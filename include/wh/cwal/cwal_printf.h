#ifndef WANDERINGHORSE_NET_CWAL_APPENDF_H_INCLUDED
#define WANDERINGHORSE_NET_CWAL_APPENDF_H_INCLUDED 1
#ifdef _MSC_VER
    #define _CRT_NONSTDC_NO_DEPRECATE
#endif
#include <stdarg.h>
#include <stdio.h> /* FILE handle */
#ifdef __cplusplus
extern "C" {
#endif
/** @page cwal_printf_page_main cwal_printf printf-like API

   This API contains a printf-like implementation which supports
   aribtrary data destinations.

   Authors: many, probably. This code supposedly goes back to the
   early 1980's.

   Current maintainer: Stephan Beal (https://wanderinghorse.net/home/stephan)

   License: Public Domain.

   The primary functions of interest are cwal_printfv() and cwal_printf(), which works
   similarly to printf() except that they take a callback function which they
   use to send the generated output to arbitrary destinations. e.g. one can
   supply a callback to output formatted text to a UI widget or a C++ stream
   object.
*/

/**
   @typedef int (*cwal_printf_appender_f)( void * arg, char const * data, unsigned n )

   The cwal_printf_appender_f typedef is used to provide
   cwal_printfv() with a flexible output routine, so that it can be
   easily send its output to arbitrary targets.

   The policies which implementations need to follow are:

   - arg is an implementation-specific pointer (may be 0) which is
   passed to cwal_printfv(). cwal_printfv() doesn't know what this
   argument is but passes it to its cwal_printf_appender_f
   argumnt. Typically it will be an object or resource handle to which
   string data is pushed or output.

   - The 'data' parameter is the data to append. If it contains
   embedded nulls, this function will stop at the first one. Thus
   it is not binary-safe.

   - n is the number of bytes to read from data.

   - Returns 0 on success, some non-0 value on error. Ideally it
   should return a value from the cwal_rc_e enum or a value which is
   guaranteed not to collide with that enum.
*/
typedef int (*cwal_printf_appender_f)( void * arg,
                                     char const * data,
                                     unsigned int n );

/**
  This function works similarly to classical printf implementations,
  but instead of outputing somewhere specific, it uses a callback
  function to push its output somewhere. This allows it to be used for
  arbitrary external representations. It can be used, for example, to
  output to an external string, a UI widget, or file handle (it can
  also emulate printf by outputing to stdout this way).

 INPUTS:

 pfAppend : The is a cwal_printf_appender_f function which is
 responsible for accumulating the output. If pfAppend returns non-zero
 then processing stops immediately and that code is returned.

 pfAppendArg : is ignored by this function but passed as the first
 argument to pfAppend. pfAppend will presumably use it as a data
 store for accumulating its string.

 fmt : This is the format string, as in the usual printf().

 ap : This is a pointer to a list of arguments.  Same as in
 vprintf() and friends.

 OUTPUTS:

 Returns 0 on success. (Years of practice have shown that classical
 printf() return semantics don't make terribly much sense for this
 API.) On error, it is not generically possible to know how much, if any
 output was generated.

 CURRENT (documented) exceptions to conventional PRINTF format
 specifiers:

 `%n` IS NOT SUPPORTED. Years of practice have shown that the classical
 return semantics of printf() are not useful for this particular API,
 and thus the semantics of this API were changed to something more
 useful (which does not support the notion of `%n`).

 `%s` works like conventional printf `%s` except that any precision
 value can be modified via the '#' flag to counts in UTF8 characters
 instead of bytes! That is, if an `%#.10s` argument has a byte
 length of 20, a precision of 10, and contains only 8 UTF8, its
 precision will allow it to output all 8 characters, even though
 they total 20 bytes. The '#' flag works this way for both width and
 precision.

`%b` works like `%s` but expects a (`cwal_buffer [const] *`) argument.

 `%h` (HTML) works like `%s` but (A) does not support the '#' flag and
 (B) converts certain characters (namely '<' and '&') to their HTML
 escaped equivalents.

 `%t` (URL encode) works like `%h` but converts certain characters
 into a representation suitable for use in an HTTP URL. (e.g. ' ' 
 gets converted to `%20`)

 `%T` (URL decode) does the opposite of `%t` - it decodes URL-encoded
 strings and outputs their decoded form. ACHTUNG: fossil(1) interprets
 this the same as `%t` except that it leaves '/' characters unescaped
 (did that change at some point? This code originally derived from
 that one some years ago!). It is still to be determined whether we
 "really need" that behaviour (we don't really need either one, seeing
 as the library is not CGI-centric like fossil(1) is).

 `%r` requires an int and renders it in "ordinal form". That is,
 the number 1 converts to "1st" and 398 converts to "398th".

 `%q` quotes a string as required for SQL. That is, '\'' characters
 get doubled. It does NOT included the outer quotes and NULL values
 get replaced by the string "(NULL)" (without quotes). See `%Q`...

 `%Q` works like `%q`, but includes the outer '\'' characters and NULL
 pointers get output as the string literal "NULL" (without quotes),
 i.e. an SQL NULL. If modified with `%!Q` then it instead uses double
 quotes, the intent being for use with identifiers.  In that form it
 still emits `NULL` without quotes, but it is not intended to be used
 with `NULL` values.

 `%j`: works like `%s` but JSON-encodes the string. It does not
 include the outer quotation marks by default, but using the '!'
 flag, i.e. `%!j`, causes those to be added. The length and precision
 flags are NOT supported for this format. Results are undefined if
 given input which is not legal UTF8. By default non-ASCII characters
 with values less than 0xffff are emitted as as literal characters (no
 escaping), but the '#' modifier flag will cause it to emit such
 characters in the `\u####` form. It always encodes characters above
 0xFFFF as UTF16 surrogate pairs (as JSON requires). Invalid UTF8
 characters may get converted to '?' or may produce invalid JSON
 output. As a special case, if the value is NULL pointer, it resolves
 to "null" without quotes (regardless of the '!'  modifier).

 These extensions may be disabled by setting certain macros when
 compiling the implementation file (see that file for details).
*/
int cwal_printfv(cwal_printf_appender_f pfAppend, void * pfAppendArg,
                 const char *fmt, va_list ap);

/**
   The elipsis counterpart of cwal_printfv().
*/
int cwal_printf(cwal_printf_appender_f pfAppend, void * pfAppendArg,
                const char *fmt, ... );

/**
   Emulates fprintf() using cwal_printfv().
*/
int cwal_printf_FILE( FILE * fp, char const * fmt, ... );

/**
   va_list variant of cwal_printf_FILE().
*/
int cwal_printfv_FILE( FILE * fp, char const * fmt, va_list args );

#ifdef __cplusplus
} /* extern "C" */
#endif
#endif /* WANDERINGHORSE_NET_CWAL_APPENDF_H_INCLUDED */
