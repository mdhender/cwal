/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=4 et sw=2 tw=80: */
#include <assert.h>
#include <string.h> /* for a single sprintf() need :/ */
#include <ctype.h> /* toupper(), tolower() */
#include "wh/cwal/cwal.h"
#include "cwal_internal.h" /* unfortunate dependency: please refactor to not need this. */
#include "JSON_parser/JSON_parser.h"

#if 1
#include <stdio.h>
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__); if(1) printf pfexp
#else
#define MARKER(pfexp) (void)0
#endif


#if defined(__cplusplus)
extern "C" {
#endif

const cwal_json_output_opt cwal_json_output_opt_empty =
  cwal_json_output_opt_empty_m;
const cwal_json_parse_info cwal_json_parse_info_empty =
  cwal_json_parse_info_empty_m;

/**
   Returns true if v is of a type which has a direct JSON representation,
   else false.
*/
static bool cwal__json_can_output( cwal_value const * v ){
  switch(v ? cwal_value_type_id(v) : CWAL_TYPE_UNDEF){
    case CWAL_TYPE_ARRAY:
    case CWAL_TYPE_BOOL:
    case CWAL_TYPE_DOUBLE:
    case CWAL_TYPE_EXCEPTION:
    case CWAL_TYPE_INTEGER:
    case CWAL_TYPE_NULL:
    case CWAL_TYPE_OBJECT:
    case CWAL_TYPE_STRING:
    case CWAL_TYPE_TUPLE:
    case CWAL_TYPE_PROPREF:
      return true;
    case CWAL_TYPE_UNDEF:
    default:
      return false;
  }
}

/**
   Internal helper to get the value (potentially a proxy) to be used
   for JSON-izing a given value.

   If opt->override.f is not NULL, it is called to determine whether
   it wants to override v. If that call fails, the result is
   propagated. If that function succeeds and the value it returns
   passes cwal__json_can_output() then `*rv` is set to that object and
   0 is returned. If that value cannot be output, it is discarded and
   `*rv` is set to NULL. If that value is a function then this routine
   calls it and, on success, sets `*rv` to the result value if
   cwal__json_can_output() returns true for that value (else it is
   discarded).

   If opt->override.f is NULL and cwal__json_can_output(v) then this
   function assigns `*rv` to v and returns 0.

   A return of 0 and `*rv` of NULL means that v cannot be JSONized and
   should be skipped or (depending on the context) treated as a JSON
   null.

   A return of 0 and `*rv` of non-NULL means that `*rv` is to be used
   in place of v (though it may be v) for purposes of outputing.
   `*rv` must be referenced on acquisition and dereferenced after
   outputing it.

   A return of non-0 does not modify `*rv` and indicates a
   serialization-ending problem.
*/
static int cwal__json_v2j(cwal_engine * const e,
                          cwal_value * const v,
                          cwal_value ** rv,
                          cwal_json_output_opt const * opt){
  *rv = NULL;
  if(!e || !v) return 0;
  if(opt->override.f){
    cwal_value * xrv = NULL;
    cwal_ref(v)/*just in case of a few corner cases.*/;
    int rc = opt->override.f(e, v, &xrv, opt->override.state);
    if(rc) return rc;
    else if(xrv){
      if(cwal__json_can_output(xrv)){
        *rv = xrv;
        cwal_unhand(v);
        return 0;
      }else if(cwal_value_is_function(xrv)){
        cwal_value * frv = NULL;
        cwal_function * const f = cwal_value_get_function(xrv);
        cwal_ref(xrv);
        rc = cwal_function_call(f, v, &frv, 0, NULL);
        if(rc){
          cwal_unhand(v);
          cwal_unref(xrv);
          return rc;
        }else if(cwal__json_can_output(frv)){
          *rv = frv;
          cwal_unhand(v);
          cwal_unhand(xrv);
          return 0;
        }else if(frv){
          cwal_refunref(frv);
        }
        cwal_unref(xrv);
      }else{
        cwal_refunref(xrv);
      }
    }
    cwal_unhand(v);
  }
  if(cwal__json_can_output(v)){
    *rv = v;
  }
  return 0;
}

/**
   Escapes the first len bytes of the given string as JSON and sends
   it to the given output function (which will be called often - once
   for each logical character). The output is also surrounded by
   double-quotes.

   A NULL str will be escaped as an empty string, though we should
   arguably export it as "null" (without quotes). We do this because
   in JavaScript (typeof null === "object"), and by outputing null
   here we would effectively change the data type from string to
   object.
*/
static int cwal_str_to_json( char const * str, unsigned int len,
                             bool escapeFwdSlash,
                             cwal_output_f f, void * state ){
  if( NULL == f ) return CWAL_RC_MISUSE;
  else if( !str || !*str || (0 == len) ){
    /* special case for 0-length strings. */
    return f( state, "\"\"", 2 );
  }else{
    unsigned char const * pos = (unsigned char const *)str;
    unsigned char const * end = (unsigned char const *)(str ? (str + len) : NULL);
    unsigned char const * next = NULL;
    int ch;
    unsigned char clen = 0;
    char escChar[3] = {'\\',0,0};
    enum {
    UBLen = 13 * 2
    /* gcc 7.3 is misdiagnosing ubuf (below) as being "between 3
       and 5" bytes in one sprintf() call, so we have to inflate
       UBLen beyond what we really need. */
    };
    char ubuf[UBLen];
    int rc = 0;
    rc = f(state, "\"", 1 );
    for( ; (pos < end) && (0 == rc); pos += clen ){
      ch = cwal_utf8_read_char(pos, end, &next);
      if( 0 == ch ) break;
      assert( next > pos );
      clen = next - pos;
      assert( clen );
      if( 1 == clen ){ /* ASCII */
        assert( *pos == ch );
        escChar[1] = 0;
        switch(ch){
          case '\t': escChar[1] = 't'; break;
          case '\r': escChar[1] = 'r'; break;
          case '\n': escChar[1] = 'n'; break;
          case '\f': escChar[1] = 'f'; break;
          case '\b': escChar[1] = 'b'; break;
          case '/':
            /*
              Regarding escaping of forward-slashes. See the main exchange below...

              --------------
              From: Douglas Crockford <douglas@crockford.com>
              To: Stephan Beal <sgbeal@googlemail.com>
              Subject: Re: Is escaping of forward slashes required?

              It is allowed, not required. It is allowed so that JSON can be safely
              embedded in HTML, which can freak out when seeing strings containing
              "</". JSON tolerates "<\/" for this reason.

              On 4/8/2011 2:09 PM, Stephan Beal wrote:
              > Hello, Jsonites,
              >
              > i'm a bit confused on a small grammatic detail of JSON:
              >
              > if i'm reading the grammar chart on http://www.json.org/ correctly,
              > forward slashes (/) are supposed to be escaped in JSON. However, the
              > JSON class provided with my browsers (Chrome and FF, both of which i
              > assume are fairly standards/RFC-compliant) do not escape such characters.
              >
              > Is backslash-escaping forward slashes required? If so, what is the
              > justification for it? (i ask because i find it unnecessary and hard to
              > look at.)
              --------------
            */
            if( escapeFwdSlash ) escChar[1] = '/';
            break;
          case '\\': escChar[1] = '\\'; break;
          case '"': escChar[1] = '"'; break;
          default: break;
        }
        if( escChar[1]){
          rc = f(state, escChar, 2);
        }else{
          rc = f(state, (char const *)pos, clen);
        }
        continue;
      }else{
        /* UTF: transform it to \uXXXX */
        memset(ubuf,0,UBLen);
        if(ch <= 0xFFFF){
          rc = sprintf(ubuf, "\\u%04x",ch);
          if( rc != 6 ){
            rc = CWAL_RC_RANGE;
            break;
          }
          rc = f( state, ubuf, 6 );
        }else{ /* encode as a UTF16 surrugate pair */
          /* http://unicodebook.readthedocs.org/en/latest/unicode_encodings.html#surrogates */
          ch -= 0x10000;
          rc = sprintf(ubuf, "\\u%04x\\u%04x",
                       (0xd800 | (ch>>10)),
                       (0xdc00 | (ch & 0x3ff)));
          if( rc != 12 ){
            rc = CWAL_RC_RANGE;
            break;
          }
          rc = f( state, ubuf, 12 );
        }
        continue;
      }
    }
    if( 0 == rc ) rc = f(state, "\"", 1 );
    return rc;
  }
}

static int cwal_json_output_null( cwal_output_f f, void * state ){
  return f(state, "null", 4);
}

static int cwal_json_output_bool( cwal_value const * src, cwal_output_f f, void * state )
{
  char const v = cwal_value_get_bool(src);
  return f(state, v ? "true" : "false", v ? 4 : 5);
}

static int cwal_json_output_integer( cwal_value const * src, cwal_output_f f, void * state )
{
  if( !f ) return CWAL_RC_MISUSE;
  else if( !cwal_value_is_integer(src) ) return CWAL_RC_TYPE;
  else {
    char b[100];
    cwal_size_t bLen = sizeof(b)/sizeof(b[0]);
    int rc;
    memset( b, 0, bLen );
    rc = cwal_int_to_cstr( cwal_value_get_integer(src), b, &bLen );
    return rc ? rc : f( state, b, bLen );
  }
}

static int cwal_json_output_double( cwal_value const * src, cwal_output_f f, void * state )
{
  if( !f ) return CWAL_RC_MISUSE;
  else if( !cwal_value_is_double(src) ) return CWAL_RC_TYPE;
  else
  {
    enum { BufLen = 128 /* this must be relatively large or huge
                           doubles can cause us to overrun here,
                           resulting in stack-smashing errors.
                        */};
    char b[BufLen];
    cwal_size_t bLen = BufLen;
    int rc;
    memset( b, 0, BufLen );
    rc = cwal_double_to_cstr( cwal_value_get_double(src), b, &bLen );
    if( rc ) return rc;
    else if(0) {
      /* Strip trailing zeroes before passing it on... */
      char * pos = b + bLen - 1;
      for( ; ('0' == *pos) && bLen && (*(pos-1) != '.');
           --pos, --bLen ){
        *pos = 0;
      }
      assert(bLen && *pos);
      return f( state, b, bLen );
    }
    else{
      return f( state, b, bLen );
    }
    return 0;
  }
}

static int cwal_json_output_string( cwal_value const * src, bool escapeFwdSlash,
                                    cwal_output_f f, void * state )
{
  char const * cstr;
  cwal_size_t strLen = 0;
  if(!cwal_value_is_string(src) && !cwal_value_is_buffer(src)){
    return CWAL_RC_TYPE;
  }else{
    cstr = cwal_value_get_cstr(src,&strLen);
    return cstr
      ? cwal_str_to_json(cstr, strLen, escapeFwdSlash, f, state)
      : /*only applies to buffers:*/
      cwal_json_output_null( f, state );
  }
}


/**
   Outputs indention spacing to f().

   blanks: (0)=no indentation, (-N)=-N TABs per/level, (+N)=n spaces/level

   depth is the current depth of the output tree, and determines how much
   indentation to generate.

   If blanks is 0 this is a no-op. Returns non-0 on error, and the
   error code will always come from f().
*/
static int cwal_json_output_indent( cwal_output_f f, void * state,
                                    cwal_json_output_opt const * opt,
                                    unsigned int depth )
{
  if(!opt->indent && (!opt->indentString.str || !opt->indentString.len)){
    return 0;
  }
  else if(opt->indentString.str){
    int i;
    int rc = f(state, "\n", 1 );
    assert(opt->indentString.len);
    for( i = 0; (i < (int)depth) && (0 == rc); ++i ){
      rc = f(state, opt->indentString.str,
             opt->indentString.len);
    }
    return rc;
  }else{
    int blanks = opt->indent;
    int i;
    int x;
    char const ch = (blanks<0) ? '\t' : ' ';
    int rc = f(state, "\n", 1 );
    if(blanks<0) blanks = -blanks;
    for( i = 0; (i < (int)depth) && (0 == rc); ++i ){
      for( x = 0; (x < blanks) && (0 == rc); ++x ){
        rc = f(state, &ch, 1);
      }
    }
    return rc;
  }
}

static int cwal_json_output_array( cwal_value * src, cwal_output_f f, void * state,
                                   cwal_json_output_opt const * fmt, unsigned int level );
static int cwal_json_output_tuple( cwal_value * src, cwal_output_f f, void * state,
                                   cwal_json_output_opt const * fmt, unsigned int level );
#if 0
static int cwal_json_output_object( cwal_value * src, cwal_output_f f, void * state,
                                    cwal_json_output_opt const * fmt, unsigned int level );
#endif

/**
   Outputs a JSON Object from the base->kvp list.
*/
static int cwal_json_output_obase( cwal_value * base, cwal_output_f f, void * state,
                                   cwal_json_output_opt const * fmt, unsigned int level );


static int cwal_json_cycle_string( cwal_value * cycled, cwal_output_f f, void * state,
                                   cwal_json_output_opt const * fmt ){
  enum {BufSize = 128};
  char buf[BufSize];
  cwal_size_t nLen = 0;
  char const * tname = cwal_value_type_name2(cycled, &nLen);
  int slen;
  assert(tname);
  assert(nLen);
  slen = sprintf(buf, "<CYCLE:%.*s@%p>", (int)(nLen>60U ? 60U : nLen), tname, (void const *)cycled);
  assert(slen>0);
  return cwal_str_to_json( buf, (unsigned)slen, fmt->escapeForwardSlashes, f, state);
}

/**
   Main cwal_json_output() implementation. Dispatches to a different impl depending
   on src->vtab->typeID.

   If e is not NULL and doJsonizeConversion is true,
   cwal__json_v2j() is used to convert/proxy src.

   Returns 0 on success.
*/
static int cwal__json_output_impl( cwal_engine * const e,
                                  bool doJsonizeConversion,
                                  cwal_value * src, cwal_output_f f, void * state,
                                  cwal_json_output_opt const * fmt, unsigned int level )
{
  int rc = 0;
  cwal_value * vp = NULL;
  if(e && doJsonizeConversion){
    rc = cwal__json_v2j(e, src, &vp, fmt);
    if(rc) return rc;
    else if(!vp) vp = src;
  }else{
    vp = src;
  }
  cwal_ref(src);
  cwal_ref(vp);
  cwal_type_id const tid = cwal_value_type_id( vp );
  switch( tid ){
    case CWAL_TYPE_UNDEF:
#if 0
      rc = f( state, "undefined", 9);
      break;
      /* We should arguably elide this values - that's what JS's
         standard impl does. */
#endif
      /* fall through */ /* transform it to null */;
    case CWAL_TYPE_NULL:
      rc = cwal_json_output_null(f, state);
      break;
    case CWAL_TYPE_BOOL:
      rc = cwal_json_output_bool(vp, f, state);
      break;
    case CWAL_TYPE_INTEGER:
      rc = cwal_json_output_integer(vp, f, state);
      break;
    case CWAL_TYPE_DOUBLE:
      rc = cwal_json_output_double(vp, f, state);
      break;
    case CWAL_TYPE_BUFFER:
    case CWAL_TYPE_STRING:
      rc = cwal_json_output_string(vp, fmt->escapeForwardSlashes,f, state);
      break;
    case CWAL_TYPE_ARRAY:
    case CWAL_TYPE_TUPLE:
    case CWAL_TYPE_FUNCTION:
    case CWAL_TYPE_EXCEPTION:
    case CWAL_TYPE_OBJECT:{
      int opaque = 0;
      rc = cwal_visit_acyclic_begin(vp, &opaque);
      if(rc){
        if(fmt->cyclesAsStrings){
          rc = cwal_json_cycle_string(vp, f, state, fmt);
        }
        break;
      }
      switch(tid){
        case CWAL_TYPE_ARRAY:
          rc = cwal_json_output_array( vp, f, state, fmt, level );
          break;
        case CWAL_TYPE_TUPLE:
          rc = cwal_json_output_tuple( vp, f, state, fmt, level );
          break;
        case CWAL_TYPE_FUNCTION:
          if(!fmt->functionsAsObjects){
            rc = CWAL_RC_TYPE;
          } else{
            rc = cwal_json_output_obase( vp, f, state, fmt, level );
          }
          break;
        case CWAL_TYPE_EXCEPTION:
        case CWAL_TYPE_OBJECT:
          rc = cwal_json_output_obase( vp, f, state, fmt, level );
          break;
        default:
          assert(!"impossible!");
          break;
      }
      cwal_visit_acyclic_end(vp, opaque);
      break;
    }
    case CWAL_TYPE_PROPREF:{
      cwal_propref * const p = cwal_value_get_propref(vp);
      cwal_value * const t = cwal_propref_container(p);
      cwal_value * const k = cwal_propref_key(p);
      cwal_value * const vv = cwal_prop_get_v(t, k);
      if(!vv){
        rc = cwal_engine_error_get(e, NULL, NULL);
        if(CWAL_RC_CYCLES_DETECTED!=rc){
          rc = cwal_json_output_null(f, state);
        }
      }else{
        rc = cwal__json_output_impl(e, doJsonizeConversion, vv, f,
                                    state, fmt, level);
      }
      break;
    }
    default:
      rc = CWAL_RC_TYPE;
      break;
  }
  cwal_unref(vp);
  cwal_unhand(src);
  return rc;
}



static int cwal_json_output_array( cwal_value * src, cwal_output_f f, void * state,
                                   cwal_json_output_opt const * fmt, unsigned int level )
{
  if( !src || !f || !fmt ) return CWAL_RC_MISUSE;
  else if( ! cwal_value_is_array(src) ) return CWAL_RC_TYPE;
  else if( level > fmt->maxDepth ) return CWAL_RC_RANGE;
  else
  {
    int rc;
    unsigned int i;
    cwal_value * v;
    char doIndent = (fmt->indent || fmt->indentString.len) ? 1 : 0;
    cwal_list const * li;
    cwal_array const * ar = cwal_value_get_array(src);
    cwal_engine * const e = cwal_value_engine(src);
    int opaque = 0;
    li = &ar->list;
    assert( NULL != li );
    if( 0 == li->count ){
      rc = f(state, "[]", 2 );
      goto end;
    }
    else if( (1 == li->count) && !fmt->indentSingleMemberValues ) doIndent = 0;
    cwal_visit_list_begin( src, &opaque );
    rc = f(state, "[", 1);
    ++level;
    if( doIndent ){
      rc = cwal_json_output_indent( f, state, fmt, level );
    }
    for( i = 0; (i < li->count) && (0 == rc); ++i ){
      v = (cwal_value *) li->list[i];
      rc = v
        ? cwal__json_output_impl( e, true, v, f, state, fmt, level )
        : cwal_json_output_null( f, state );
      if( 0 == rc ){
        if(i+1 < li->count){
          rc = f(state, ",", 1);
          if( 0 == rc ){
            rc = doIndent
              ? cwal_json_output_indent( f, state, fmt, level )
              : (fmt->addSpaceAfterComma
                 ? f( state, " ", 1 )
                 : 0);
          }
        }
      }
    }
    if(!rc){
      if( doIndent ){
        rc = cwal_json_output_indent( f, state, fmt, --level );
      }
      if(!rc) rc = f(state, "]", 1);
    }
    cwal_visit_list_end(src, opaque);
    end:
    return rc;
  }
}

int cwal_json_output_tuple( cwal_value * src, cwal_output_f f, void * state,
                            cwal_json_output_opt const * fmt, unsigned int level )
{
  if( !src || !f || !fmt ) return CWAL_RC_MISUSE;
  else if( ! cwal_value_is_tuple(src) ) return CWAL_RC_TYPE;
  else if( level > fmt->maxDepth ) return CWAL_RC_RANGE;
  else
  {
    int rc;
    unsigned int i;
    cwal_value * v;
    cwal_engine * const e = cwal_value_engine(src);
    bool doIndent = (fmt->indent || fmt->indentString.len);
    cwal_tuple const * tp = cwal_value_get_tuple(src);
    cwal_size_t const n = cwal_tuple_length(tp);
    int opaque = 0;
    if( 0 == n ){
      rc = f(state, "[]", 2 );
      goto end;
    }
    else if( (1 == n) && !fmt->indentSingleMemberValues ) doIndent = false;
    cwal_visit_list_begin( src, &opaque );
    rc = f(state, "[", 1);
    ++level;
    if( doIndent ){
      rc = cwal_json_output_indent( f, state, fmt, level );
    }
    for( i = 0; (i < n) && (0 == rc); ++i ){
      if(i>0){
        rc = f(state, ",", 1);
        if( 0 == rc ){
          rc = doIndent
            ? cwal_json_output_indent( f, state, fmt, level )
            : (fmt->addSpaceAfterComma
               ? f( state, " ", 1 )
               : 0);
        }
        if(rc) break;
      }
      v = (cwal_value *) cwal_tuple_get(tp, i);
      rc = v
        ? cwal__json_output_impl(e, true, v, f, state, fmt, level )
        : cwal_json_output_null( f, state );
    }
    if(!rc){
      if( doIndent ){
        rc = cwal_json_output_indent( f, state, fmt, --level );
      }
      if(!rc) rc = f(state, "]", 1);
    }
    cwal_visit_list_end( src, opaque );
    end:
    return rc;
  }
}

int cwal_json_output_obase( cwal_value * self, cwal_output_f f, void * state,
                            cwal_json_output_opt const * fmt, unsigned int level ){
  if( !self || !f || !fmt ) return CWAL_RC_MISUSE;
  else if( level > fmt->maxDepth ) return CWAL_RC_RANGE;
  else{
    int rc = 0;
    unsigned int i;
    cwal_kvp const * kvp;
    cwal_string const * sKey;
    cwal_value * val;
    char doIndent = (fmt->indent || fmt->indentString.len) ? 1 : 0;
    cwal_obase * base = cwal_value_obase(self);
    int outputCount = 0 /* keep track of where we need a comma */;
    int opaque = 0;
    cwal_obase_kvp_iter iter;
    cwal_engine * const e = cwal_value_engine(self);
    if(!base) return CWAL_RC_MISUSE;
    assert( (NULL != fmt));
    cwal_visit_props_begin(self, &opaque);
    kvp = cwal_obase_kvp_iter_init(self, &iter);
    if( 0 == kvp ){
      cwal_visit_props_end(self, opaque);
      return f(state, "{}", 2 );
    }else if( cwal_props_count(self)<2
             && !fmt->indentSingleMemberValues ){
      doIndent = 0;
    }
    rc = f(state, "{", 1);
    ++level;
    if( !rc && doIndent ){
      rc = cwal_json_output_indent( f, state, fmt, level );
    }
    for( i = 0; (0==rc) && kvp;
         ++i, kvp = cwal_obase_kvp_iter_next(&iter) ){
      char const * cKey;
      cwal_value * key;
      cwal_value * vp = NULL;
      if(CWAL_VAR_F_HIDDEN & kvp->flags) continue;
      key = cwal_kvp_key( kvp );
      sKey = cwal_value_get_string(key);
      if(!sKey){
        /*assert(sKey && "FIXME: cannot handle non-string keys.");*/
        switch(cwal_value_type_id(kvp->key)){
          case CWAL_TYPE_INTEGER:
          case CWAL_TYPE_DOUBLE:
            break;
          default: continue;
        }
      }
      val = cwal_kvp_value(kvp);
      if(cwal_value_undefined()==val){
        continue;
      }
      rc = cwal__json_v2j(e, val, &vp, fmt);
      if(rc) break;
      else if(!vp) continue;
      else{
        cwal_ref(vp);
        val = vp;
      }
      if(outputCount){
        /* Output comma between us and our left-side neighbor */
        rc = f(state, ",", 1);
        if( 0 == rc ){
          rc = doIndent
            ? cwal_json_output_indent( f, state, fmt, level )
            : (fmt->addSpaceAfterComma
               ? f( state, " ", 1 )
               : 0);
        }
        if(rc){cwal_unref(vp); vp = NULL; break;}
      }

      if(sKey){
        cKey = cwal_string_cstr(sKey);
        rc = cwal_str_to_json(cKey,
                              cwal_string_length_bytes(sKey),
                              fmt->escapeForwardSlashes, f, state);
      }else{
        rc = f(state, "\"", 1);
        if(!rc) rc = cwal__json_output_impl(e, true, key, f, state, fmt, level);
        if(!rc) rc = f(state, "\"", 1);
      }
      if(0 == rc) rc = fmt->addSpaceAfterColon
                    ? f(state, ": ", 2 )
                    : f(state, ":", 1 )
                    ;
      if(0 == rc) {
        rc = ( val )
          ? cwal__json_output_impl( e, false, val, f, state, fmt, level )
          : cwal_json_output_null( f, state );
        ++outputCount;
      }
      if(vp){cwal_unref(vp);}
    }
    --level;
    if( doIndent && (0 == rc) ){
      rc = cwal_json_output_indent( f, state, fmt, level );
    }
    cwal_visit_props_end(self, opaque);
    return rc ? rc : f(state, "}", 1);
  }
}


int cwal_json_output( cwal_value * src, cwal_output_f f,
                      void * state, cwal_json_output_opt const * fmt ){
  int rc;
  if(! fmt ) fmt = &cwal_json_output_opt_empty;
  cwal_ref(src);
  rc = cwal__json_output_impl(cwal_value_engine(src), true,
                             src, f, state, fmt, 0 );
  if( 0==rc && fmt->addNewline ){
    rc = f(state, "\n", 1);
  }
  cwal_unhand(src);
  return rc;
}

int cwal_json_output_FILE( cwal_value * src, FILE * dest,
                           cwal_json_output_opt const * fmt ){
  static cwal_json_output_opt sopt = cwal_json_output_opt_empty_m;
  int rc = 0;
  if(!sopt.addNewline){
    sopt.addNewline = 1;
  }
  if(!fmt) fmt = &sopt;
  rc = cwal_json_output( src, cwal_output_f_FILE, dest, fmt );
  if( 0 == rc ) fflush( dest );
  return rc;
}

int cwal_json_output_filename( cwal_value * src,
                               char const * fn,
                               cwal_json_output_opt const * fmt )
{
  if( !src || !fn || !*fn ) return CWAL_RC_MISUSE;
  else {
    FILE * f = (*fn && !fn[1] && ('-'==*fn))
      ? stdout
      : fopen( fn, "wb" );
    if( !f ) return CWAL_RC_IO;
    else {
      int const rc = cwal_json_output_FILE( src, f, fmt );
      if(stdout != f) fclose(f);
      return rc;
    }
  }
}

int cwal_json_output_buffer( cwal_engine * e, cwal_value * src,
                             cwal_buffer * dest,
                             cwal_json_output_opt const * fmt ){

  static cwal_json_output_opt const outOpt = cwal_json_output_opt_empty_m;
  cwal_output_buffer_state job = cwal_output_buffer_state_empty;
  if(!e || !src || !dest) return CWAL_RC_MISUSE;
  else if(!fmt) fmt = &outOpt;
  job.e = e;
  job.b = dest;
  return cwal_json_output( src, cwal_output_f_buffer, &job, fmt );
}


int cwal_json_output_engine( cwal_engine * e, cwal_value * src,
                             cwal_json_output_opt const * fmt ){
  return (src && e && e->vtab && e->vtab->outputer.output)
    ? cwal_json_output( src, e->vtab->outputer.output, e->vtab->outputer.state.data, fmt )
    : CWAL_RC_MISUSE;
}

#if CWAL_ENABLE_JSON_PARSER
struct cwal_json_parser{
  JSON_parser p;
  cwal_engine * e;
  cwal_value * root;
  cwal_value * node;
  cwal_string * ckey;
  int errNo;
  char const * errMsg;
  cwal_list stack;
};
typedef struct cwal_json_parser cwal_json_parser;
static const cwal_json_parser cwal_json_parser_empty = {
NULL/*e*/,
NULL/*p*/,
NULL/*root*/,
NULL/*node*/,
NULL/*ckey*/,
0/*errNo*/,
NULL/*errMsg*/,
cwal_list_empty_m/*stack*/
};
#endif
/* CWAL_ENABLE_JSON_PARSER */

#if CWAL_ENABLE_JSON_PARSER
/**
   Converts a JSON_error code to one of the cwal_rc values.
*/
static int cwal_json_err_to_rc( JSON_error jrc ){
  switch(jrc){
    case JSON_E_NONE: return 0;
    case JSON_E_INVALID_CHAR: return CWAL_RC_JSON_INVALID_CHAR;
    case JSON_E_INVALID_KEYWORD: return CWAL_RC_JSON_INVALID_KEYWORD;
    case JSON_E_INVALID_ESCAPE_SEQUENCE: return CWAL_RC_JSON_INVALID_ESCAPE_SEQUENCE;
    case JSON_E_INVALID_UNICODE_SEQUENCE: return CWAL_RC_JSON_INVALID_UNICODE_SEQUENCE;
    case JSON_E_INVALID_NUMBER: return CWAL_RC_JSON_INVALID_NUMBER;
    case JSON_E_NESTING_DEPTH_REACHED: return CWAL_RC_JSON_NESTING_DEPTH_REACHED;
    case JSON_E_UNBALANCED_COLLECTION: return CWAL_RC_JSON_UNBALANCED_COLLECTION;
    case JSON_E_EXPECTED_KEY: return CWAL_RC_JSON_EXPECTED_KEY;
    case JSON_E_EXPECTED_COLON: return CWAL_RC_JSON_EXPECTED_COLON;
    case JSON_E_OUT_OF_MEMORY: return CWAL_RC_OOM;
    default:
      return CWAL_RC_ERROR;
  }
}
#endif
/* CWAL_ENABLE_JSON_PARSER */

#if CWAL_ENABLE_JSON_PARSER
/** @internal

    Cleans up all contents of p but does not free p.

    To properly take over ownership of the parser's root node on a
    successful parse:

    - Copy p->root's pointer and set p->root to NULL.
    - Eventually free up p->root with cwal_value_free().
   
    If you do not set p->root to NULL, p->root will be freed along with
    any other items inserted into it (or under it) during the parsing
    process.
*/
static void cwal_json_parser_clean( cwal_json_parser * p ){
  if(p->p) delete_JSON_parser(p->p);
  if(p->ckey){
    cwal_value * ckeyV = cwal_string_value(p->ckey);
    assert(cwal_value_is_builtin(ckeyV) || cwal_value_refcount(ckeyV));
    cwal_value_unref(ckeyV);
  }
  cwal_list_reserve(p->e, &p->stack, 0);
  if(p->root) cwal_value_unref( p->root );
  *p = cwal_json_parser_empty;
}
#endif
/* CWAL_ENABLE_JSON_PARSER */


#if CWAL_ENABLE_JSON_PARSER
/** @internal

    If p->node is-a Object then value is inserted into the object
    using p->key. In any other case cwal_rc.InternalError is returned.

    Returns cwal_rc.AllocError if an allocation fails.

    Returns 0 on success. On error, parsing must be ceased immediately.
   
    This function always takes and removes a ref to val, regardless of
    success or failure. Thus it will, on error, clean up val if there
    is no other reference to it. (This simplifies error handling in
    the core parser.)
*/
static int cwal_json_parser_set_key( cwal_json_parser * p, cwal_value * val ){
  int rc;
  assert( p && val );
  cwal_value_ref(val);
  if( p->ckey && cwal_value_is_object(p->node) ){
    cwal_value * ckeyV = cwal_string_value(p->ckey);
    assert(cwal_value_is_builtin(ckeyV) || cwal_value_refcount(ckeyV));
    rc = cwal_prop_set_v( p->node, ckeyV, val );
    cwal_value_unref(ckeyV);
    p->ckey = NULL /* required to avoid mis-cleanup */;
  }
  else{
    rc = p->errNo = CWAL_RC_ERROR;
  }
  cwal_value_unref(val);
  return rc;
}
#endif
/* CWAL_ENABLE_JSON_PARSER */

#if CWAL_ENABLE_JSON_PARSER
/** @internal

    Pushes val into the current object/array parent node, depending on
    the internal state of the parser.

    This function always takes and removes a ref to val, regardless of
    success or failure. Thus it will, on error, clean up val if there
    is no other reference to it. (This simplifies error handling in
    the core parser.)

    Returns 0 on success. On error, parsing must be ceased immediately.
*/
static int cwal_json_parser_push_value( cwal_json_parser * p, cwal_value * val ){
  int rc;
  cwal_value_ref(val);
  if( p->ckey ){
    /* we're in Object mode */
    assert( cwal_value_is_object( p->node ) );
    rc = cwal_json_parser_set_key( p, val );
  }
  else if( cwal_value_is_array( p->node ) ){
    /* we're in Array mode */
    cwal_array * ar = cwal_value_get_array( p->node );
    rc = cwal_array_append( ar, val );
  }
  else{ /* Wha??? */
    assert( !"Internal error in cwal_json_parser code" );
    rc = p->errNo = CWAL_RC_ERROR;
  }
  cwal_value_unref(val);
  return rc;
}
#endif
/* CWAL_ENABLE_JSON_PARSER */

#if CWAL_ENABLE_JSON_PARSER
/**
   Callback for JSON_parser API. Reminder: it returns 0 (meaning false)
   on error!
*/
static int cwal_json_parse_callback( void * cx, int type,
                                     JSON_value const * value ){
  cwal_json_parser * p = (cwal_json_parser *)cx;
  int rc = 0;
  cwal_value * v = NULL;
#define CHECKV if( !v ) { rc = CWAL_RC_OOM; break; } else
  switch(type) {
    case JSON_T_ARRAY_BEGIN:
    case JSON_T_OBJECT_BEGIN: {
      cwal_value * obja = (JSON_T_ARRAY_BEGIN == type)
        ? cwal_new_array_value(p->e)
        : cwal_new_object_value(p->e);
      if( ! obja ){
        p->errNo = CWAL_RC_OOM;
        break;
      }
      if( ! p->root ){
        cwal_value_ref(p->root);
        p->root = p->node = obja;
        rc = cwal_list_append( p->e, &p->stack, obja );
        if( rc ){
          /* work around a (potential) corner case in the cleanup code. */
          p->root = p->node = NULL;
          cwal_value_unref(obja);
        }
      }
      else{
        cwal_value_ref(obja);
        rc = cwal_json_parser_push_value( p, obja );
        if(!rc){
          rc = cwal_list_append( p->e, &p->stack, obja );
          if( !rc ){
            p->node = obja;
          }
        }
        cwal_value_unref(obja);
      }
      break;
    }
    case JSON_T_ARRAY_END:
    case JSON_T_OBJECT_END: {
      if(!p->stack.count){
        rc = CWAL_RC_RANGE;
        break;
      }
      /* Reminder: do not use cwal_array_pop_back( &p->stack )
         because that will clean up the object, and we don't want
         that.  We just want to forget this reference
         to it. The object is either the root or was pushed into
         an object/array in the parse tree (and is owned by that
         object/array).
      */
      --p->stack.count;
      assert( p->node == p->stack.list[p->stack.count] );
      p->stack.list[p->stack.count] = NULL;
      if( p->stack.count ){
        p->node = (cwal_value *)p->stack.list[p->stack.count-1];
      }
      else p->node = p->root;
      break;
    }
    case JSON_T_INTEGER: {
      v = cwal_new_integer(p->e, value->vu.integer_value);
      CHECKV {
        rc = cwal_json_parser_push_value( p, v );
        break;
      }
    }
    case JSON_T_FLOAT: {
      v = cwal_new_double(p->e, value->vu.float_value);
      CHECKV {
        rc =  cwal_json_parser_push_value( p, v );
        break;
      }
    }
    case JSON_T_NULL: {
      rc = cwal_json_parser_push_value( p, cwal_value_null() );
      break;
    }
    case JSON_T_TRUE: {
      rc = cwal_json_parser_push_value( p, cwal_value_true() );
      break;
    }
    case JSON_T_FALSE: {
      rc = cwal_json_parser_push_value( p, cwal_value_false() );
      break;
    }
    case JSON_T_KEY: {
      assert(!p->ckey);
      p->ckey = cwal_new_string( p->e,
                                 value->vu.str.value,
                                 (cwal_size_t)value->vu.str.length );
      if( ! p->ckey ){
        rc = CWAL_RC_OOM;
        break;
      }
      cwal_value_ref(cwal_string_value(p->ckey));
      break;
    }
    case JSON_T_STRING: {
      v = cwal_new_string_value( p->e,
                                 value->vu.str.value,
                                 (cwal_size_t)value->vu.str.length );
      CHECKV {
        rc = cwal_json_parser_push_value( p, v );
        break;
      }
    }
    default:
      assert(0);
      rc = CWAL_RC_ERROR;
      break;
  }
#undef CHECKV
  return ((p->errNo = rc)) ? 0 : 1;
}
#endif
/* CWAL_ENABLE_JSON_PARSER */

int cwal_json_parse( cwal_engine * e, cwal_input_f src,
                     void * state, cwal_value ** tgt,
                     cwal_json_parse_info * pInfo){
#if CWAL_ENABLE_JSON_PARSER
  unsigned char ch[2] = {0,0};
  int rc = 0;
  cwal_size_t len = 1;
  cwal_json_parse_info info = pInfo ? *pInfo : cwal_json_parse_info_empty;
  cwal_json_parser p = cwal_json_parser_empty;
  JSON_config jopt;
  if( !e || !tgt || !src ) return CWAL_RC_MISUSE;
  memset( &jopt, 0, sizeof(JSON_config) );
  init_JSON_config( &jopt );
  jopt.allow_comments = 0;
  jopt.depth = 30;
  jopt.callback_ctx = &p;
  jopt.handle_floats_manually = 0;
  jopt.callback = cwal_json_parse_callback;
  p.p = new_JSON_parser(&jopt);
  if( !p.p ) return CWAL_RC_OOM;
  p.e = e;
  do
  { /* FIXME: buffer the input in multi-kb chunks. */
    len = 1;
    ch[0] = 0;
    rc = src( state, ch, &len );
    if( 0 != rc ) break;
    else if( !len /* EOF */ ) break;
    ++info.length;
    if('\n' == ch[0]){
      ++info.line;
      info.col = 0;
    }
    if( ! JSON_parser_char(p.p, ch[0]) ){
      rc = cwal_json_err_to_rc( JSON_parser_get_last_error(p.p) );
      if(0==rc) rc = p.errNo ? p.errNo : CWAL_RC_ERROR;
      info.errorCode = rc;
      break;
    }
    if( '\n' != ch[0]) ++info.col;
  } while(1);
  if(pInfo) *pInfo = info;
  if( 0 != rc ){
    cwal_json_parser_clean(&p);
    return rc;
  }
  if( ! JSON_parser_done(p.p) ){
    rc = cwal_json_err_to_rc( JSON_parser_get_last_error(p.p) );
    cwal_json_parser_clean(&p);
    if(0==rc) rc = p.errNo ? p.errNo : CWAL_RC_ERROR;
  }
  else{
    cwal_value * root = p.root;
    p.root = NULL;
    cwal_json_parser_clean(&p);
    if( root ) *tgt = root;
    else{ /* this can happen on empty input. */
      rc = CWAL_RC_ERROR;
    }
  }
  return rc;
#else
  return CWAL_RC_UNSUPPORTED;
#endif
  /* CWAL_ENABLE_JSON_PARSER */
}


int cwal_json_parse_FILE( cwal_engine * e, FILE * src, cwal_value ** tgt,
                          cwal_json_parse_info * pInfo ){
  return cwal_json_parse( e, cwal_input_f_FILE, src, tgt, pInfo );
}

int cwal_json_parse_filename( cwal_engine * e, char const * src,
                              cwal_value ** tgt,
                              cwal_json_parse_info * pInfo ){
#if CWAL_ENABLE_JSON_PARSER
  if( !src || !tgt ) return CWAL_RC_MISUSE;
  else{
    FILE * f = (*src && !src[1] && ('-'==*src))
      ? stdin
      : fopen(src, "rb");
    if( !f ) return CWAL_RC_IO;
    else{
      int const rc = cwal_json_parse_FILE( e, f, tgt, pInfo );
      if(stdin != f) fclose(f);
      return rc;
    }
  }
#else
  return CWAL_RC_UNSUPPORTED;
#endif
  /* CWAL_ENABLE_JSON_PARSER */
}

#if CWAL_ENABLE_JSON_PARSER
/** Internal type to hold state for a JSON input string.
 */
typedef struct cwal_input_StringSource_{
  /** Start of input string. */
  char const * str;
  /** Current iteration position. Must initially be == str. */
  char const * pos;
  /** Logical EOF, one-past-the-end of str. */
  char const * end;
}  cwal_input_StringSource_t;

/**
   A cwal_input_f() implementation which requires the state argument
   to be a properly populated (cwal_input_StringSource_t*).
*/
static int cwal_input_StringSource( void * state, void * dest, cwal_size_t * n ){
  if( !state || !n || !dest ) return CWAL_RC_MISUSE;
  else if( !*n ) return 0 /* ignore this */;
  else{
    cwal_size_t i;
    cwal_input_StringSource_t * ss = (cwal_input_StringSource_t*) state;
    unsigned char * tgt = (unsigned char *)dest;
    for( i = 0; (i < *n) && (ss->pos < ss->end); ++i, ++ss->pos, ++tgt )
    {
      *tgt = *ss->pos;
    }
    *n = i;
    return 0;
  }
}
#endif
/* CWAL_ENABLE_JSON_PARSER */


int cwal_json_parse_cstr( cwal_engine * e, char const * src,
                          cwal_size_t len, cwal_value ** tgt,
                          cwal_json_parse_info * pInfo ){
#if CWAL_ENABLE_JSON_PARSER
  if( ! tgt || !src ) return CWAL_RC_MISUSE;
  else if( !*src || (len<2/*2==len of {} and []*/) ) return CWAL_RC_RANGE;
  else{
    cwal_input_StringSource_t ss;
    ss.str = ss.pos = src;
    ss.end = src + len;
    return cwal_json_parse( e, cwal_input_StringSource, &ss, tgt, pInfo );
  }
#else
  return CWAL_RC_UNSUPPORTED;
#endif
  /* CWAL_ENABLE_JSON_PARSER */
}

#if defined(__cplusplus)
} /*extern "C"*/
#endif
#undef MARKER
