/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#include <assert.h>
#include "libs2.h"
/* #include <stdlib.h> */
#include <string.h>

#if 1
#include <stdio.h>
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__); if(1) printf pfexp
#else
#define MARKER(pfexp) (void)0
#endif

const s2_tmpl_opt s2_tmpl_opt_empty = s2_tmpl_opt_empty_m;

int s2_tmpl_to_code( cwal_engine *e, cwal_buffer const * src,
                     cwal_buffer * dest,
                     s2_tmpl_opt const * opt){
  char const * pos;
  char const * end;
  char const * origin;
  char const * tagOpenScript = "<?";
  int nOpenScript = 2;
  char const * tagCloseScript = "?>";
  int nCloseScript = 2;
  char const * tagOpenValue = "<%";
  int nOpenValue = 2;
  char const * tagCloseValue = "%>";
  int nCloseValue = 2;
  char const * outAlias = "ⓞⓤⓣ" /*"➤➤"*/ /* "ᐊᐊ" */ /*"ᗛᗛ"*/ /* local var alias for OUT */;
  char const * hDocPrefix;
  char const * hDocId = "ⒺⓄⒻ" /*"ᐊᐊ"*/ /*heredoc ID used by output blocks */;
  char const * errMsg = 0;
  char const * openStart = 0;
  char const * outsymLong = "TMPLOUT";
  int rc = 0;
  int hIsOpen = 0 /* heredoc currently opened? */;
  cwal_buffer hTag = cwal_buffer_empty;
  if(!e || !src || !dest) return CWAL_RC_MISUSE;
  if(!opt) opt = &s2_tmpl_opt_empty;
  else{
    /* Set up custom options... */
    if(opt->outputSymbolInternal && *opt->outputSymbolInternal){
      outAlias = opt->outputSymbolInternal;
    }
    if(opt->outputSymbolPublic && *opt->outputSymbolPublic){
      outsymLong = opt->outputSymbolPublic;
    }
    if(opt->heredocId && *opt->heredocId) hDocId = opt->heredocId;
    if(opt->tagCodeOpen && opt->tagCodeClose){
      tagOpenScript = opt->tagCodeOpen;
      nOpenScript = (int)cwal_strlen(tagOpenScript);
      tagCloseScript = opt->tagCodeClose;
      nCloseScript = (int)cwal_strlen(tagCloseScript);
    }
    if(opt->tagValueOpen && opt->tagValueClose){
      tagOpenValue = opt->tagValueOpen;
      nOpenValue = (int)cwal_strlen(tagOpenValue);
      tagCloseValue = opt->tagValueClose;
      nCloseValue = (int)cwal_strlen(tagCloseValue);
    }
    if(nOpenValue<1 /* all tags/ids need a length of at least 1 */
       || nCloseValue<1
       || nOpenScript<1
       || nCloseScript<1
       || cwal_strlen(outAlias)<1
       || cwal_strlen(outsymLong)<1
       || cwal_strlen(hDocId)<1
       ){
      return CWAL_RC_RANGE;
    }else if(/* ensure that no two open/close tags are the same. */
             0==strcmp(tagOpenValue,tagCloseValue)
             || 0==strcmp(tagOpenValue,tagOpenScript)
             || 0==strcmp(tagOpenValue,tagCloseScript)
             || 0==strcmp(tagOpenScript,tagCloseScript)
             || 0==strcmp(tagOpenScript,tagCloseValue)
             ){
      return CWAL_RC_RANGE;
    }
    /* MARKER(("tagOpenValue=%.*s, tagCloseValue=%.*s, "
       "tagOpenScript=%.*s, tagCloseScript=%.*s\n",
                nOpenValue, tagOpenValue, nCloseValue, tagCloseValue,
                nOpenScript, tagOpenScript, nCloseScript, tagCloseScript));*/
  }
  origin = pos = (char const *)src->mem;
  end = pos + src->used;
  rc = cwal_buffer_printf(e, &hTag, "%s << <<<:", outAlias);
  hDocPrefix = (char const *)hTag.mem;
  if(!rc) rc = cwal_buffer_reserve(e, dest, src->capacity)
            /* very possibly not enough, but we cannot guess
               the approximate final size. */;
  while(!rc){/* not a loop - we only use while() so we can break out
                of this block */
    /**
       Set up some bootstrapping stuff, e.g. make sure we have
       outsymLong in place.
    */
    rc = cwal_buffer_append( e, dest, "(", 1);
    if(!rc
       && (!opt->outputSymbolPublic || !*opt->outputSymbolPublic)
       && !(opt->flags & S2_TMPL_ELIDE_TMPLOUT)){
      /* Set up local vars, if not done already */
      rc = cwal_buffer_printf( e, dest,
                               "(typeinfo(isdeclared %s) || var %s),\n"
                               "((undefined === %s) "
                               "&& (%s=s2out)),\n",
                               outsymLong,
                               outsymLong,
                               outsymLong, outsymLong);
    }
    if(rc) break;
    else if(!opt->outputSymbolInternal || !*opt->outputSymbolInternal){
      rc = cwal_buffer_printf( e, dest,
                               "(typeinfo(islocal %s) || (var %s=%s))",
                               /* ^^^ can't be const b/c of operator<< rewrite below */
                               outAlias, outAlias, outsymLong);
    }
    if(!rc) rc = cwal_buffer_append( e, dest, ");\n", 3);
    if(rc) break;
    /*
       If TMPLOUT does not have operator<<, re-map outAlias to be a
       local wrapper object which proxies operator<< to TMPLOUT.
    */
    rc = cwal_buffer_printf( e, dest,
                             "(typeinfo(isfunction %s.'operator<<')"
                             " || (%s={prototype:null,"
                             "'operator<<':"
                             "proc(a)using{$:%s}{return $(a),this}"
                             "}));\n",
                             outAlias, outAlias, outAlias)
      /* reminder to self: we use an intermediary proc() here, instead
         of directly using TMPLOUT as operator<< impl, to avoid any
         problems wrt expectations of the value of 'this' in TMPLOUT's
         body. */;
    /*
       The problem with operator<< is that it interferes with (has
       unexpected side effects wrt) code in the <% %> blocks, e.g.:

       TMPLOUT << 1 << 2

       Will not behave the same as TMPLOUT(1<<2). The -> op has
       similar problems with unary values: <% -1 +2 %>.

       20181118: we solve that by wrapping the output of value blocks
       (but not code blocks) with an eval{...}. That minor
       tokenization overhead is orders of magnitude smaller than what
       we save by using operator<<, rather than a normal function
       call, for the code block output.
    */
    break;
  }
  if(rc) goto end;
  if(1){
    /*
      Scan the first bytes and ignore any leading spaces before an opening
      <? or <%. If the first non-space character is not one of those patterns,
      retain the leading space.
    */
    char const * x = pos;
    for( ; x<end && *x && ('\n'==*x || s2_is_space(*x)); ++x) continue;
    if(*x && (0==memcmp(x, tagOpenScript, nOpenScript)
              || 0==memcmp(x, tagOpenValue, nOpenValue))){
      origin = pos = x;
    }
  }

  for( ; !rc && (pos < end); ++pos ){
    if(dest->capacity <= dest->used-2){
      rc = cwal_buffer_reserve(e, dest, dest->capacity * 3 / 2 );
    }
    if(((end-pos) < nCloseScript)
       && ((end-pos) < nCloseValue)) {
      rc = cwal_buffer_append(e, dest, pos, end - pos);
      break;
    }
    else if(((end-pos) > nOpenScript) /* <? */
            && (0==memcmp(pos, tagOpenScript, nOpenScript))){
      openStart = pos;
      pos += nOpenScript;
      if(hIsOpen){
        rc = cwal_buffer_printf(e, dest, " %s;", hDocId);
        hIsOpen = 0;
      }
      for( ; !rc && (pos < end); ++pos ){
        if(pos > (end-nCloseScript)){
          /* Allow <? tags to end the document unclosed,
             to simplify generation of docs which do not
             output any whitespace (e.g. a trailing
             newline).  PHP does this (for the same
             reason, AFAIK).
          */
          rc = cwal_buffer_append(e, dest, pos, (end-pos));
          break;
        }
        else if(0==memcmp(pos, tagCloseScript, nCloseScript)){
          pos += nCloseScript -1 /*outer loop will re-add one*/;
          rc = cwal_buffer_printf(e, dest, "\n%s%s ",
                                  hDocPrefix, hDocId);
          hIsOpen = 1;
          break;
        }
        else {
          rc = cwal_buffer_append(e, dest, pos, 1);
        }
      }
      continue;
    }
    else if(((end-pos) > nOpenValue) /* <% */
            && 0==memcmp(pos, tagOpenValue, nOpenValue)){
      /**
         20181118: we wrap VALUE blocks in eval{...} so that compound
         expressions won't interfere with the operator<<. e.g.: <%
         1,2,3 %> would not behave inutitively without this. In
         practice, <% %> blocks are generally small, so we don't have
         an arbitrarily large re-tokenization performance hit here
         (like we would if we did the same with <? ... ?> blocks).
      */
      openStart = pos;
      pos += nOpenValue;
      if(hIsOpen){
        rc = cwal_buffer_printf(e, dest, " %s;\n%s<< eval{ ",
                                hDocId, outAlias);
        hIsOpen = 0;
      }else if(openStart == origin
               /*workaround for <% at starting pos. */){
        rc = cwal_buffer_printf(e, dest, "%s<< eval {", outAlias);
        hIsOpen = 1;
      }
      for( ; !rc && (pos < end); ++pos ){
        if(pos > (end-nCloseValue)){
          rc = CWAL_RC_RANGE;
          errMsg = "Missing closing tag.";
          pos = openStart;
          goto syntax_err;
        }
        else if(0==memcmp(pos, tagCloseValue, nCloseValue)){
          pos += nCloseValue-1 /*b/c outer loop will add one*/;
          rc = cwal_buffer_printf(e, dest,
                                  " }"/*<-- end eval block*/";\n%s%s ",
                                  hDocPrefix, hDocId);
          hIsOpen = 1;
          break;
        }else{
          rc = cwal_buffer_append(e, dest, pos, 1);
        }
      }
    }else{
      /* Pipe the rest through as-is. TODO: optimize this
         by scouting for the next '<' character before
         pushing the output.*/
      if(!hIsOpen){
        rc = cwal_buffer_printf(e, dest, "\n%s%s ",
                                hDocPrefix, hDocId);
        hIsOpen = 1;
      }
      if(!rc) rc = cwal_buffer_append(e, dest, pos, 1);
    }
  }
  end:
  if(!rc){
    if(hIsOpen){
      rc = cwal_buffer_printf(e, dest, " %s;", hDocId);
    }
    if(!rc) rc = cwal_buffer_printf(e, dest,
                                    "\ntrue /*result value*/;\n");
  }
  cwal_buffer_reserve(e, &hTag, 0);
  return rc;
  syntax_err:
  {
    s2_linecol_t line = 1, col = 0;
    assert(errMsg);
    assert(0 != rc);
    s2_count_lines( (char const *)src->mem,
                    (char const *)src->mem+src->used,
                    pos, &line, &col);
    return cwal_exception_setf(e, rc,
                               "tmpl syntax error at "
                               "line %d, col %d: %s",
                               line, col, errMsg);
  }
}

int s2_cb_tmpl_to_code( cwal_callback_args const * args, cwal_value ** rv ){
  int rc;
  char const * src;
  cwal_buffer srcB = cwal_buffer_empty;
  cwal_buffer xbuf = cwal_buffer_empty;
  cwal_engine * e = args->engine;
  s2_tmpl_opt topt = s2_tmpl_opt_empty;
  cwal_value * optObj = args->argc>1
    ? args->argv[1]
    : 0;
  src = args->argc
    ? cwal_value_get_cstr(args->argv[0], &srcB.used)
    : 0;
  if(!src){
    return cwal_exception_setf(e, CWAL_RC_MISUSE,
                               "Expecting a string/buffer argument.");
  }else if(!srcB.used){
    *rv = args->argv[0];
    return 0;
  }

  if( optObj && cwal_props_can(optObj)){
    cwal_value const * v;
        char const * vKey;
#define CHECK(SPROP,CPROP)                          \
        if((v = cwal_prop_get(optObj, SPROP, cwal_strlen(SPROP))) \
           && (vKey=cwal_value_get_cstr(v,0)))                      \
          topt.CPROP = vKey
        CHECK("codeOpen", tagCodeOpen);
        CHECK("codeClose", tagCodeClose);
        CHECK("valueOpen", tagValueOpen);
        CHECK("valueClose", tagValueClose);
        CHECK("outputSymbol", outputSymbolPublic);
#undef CHECK
  }

  if(0){ /* just testing, but this makes for a much shorter header! */
    topt.outputSymbolPublic = "s2out";
    /* topt.outputSymbolInternal = "print"; */
    /* topt.heredocId = "'~_~'"; */
  }
#if 0
  else if(cwal_scope_search(args->scope, -1, "TMPLOUT", 10, 0)
           /* ^^^^^ Kind of a cheap (and not always valid)
              optimization */){
    topt.flags |= S2_TMPL_ELIDE_TMPLOUT;
  }
#endif

  srcB.mem = (unsigned char *)src;
  /* Note that our misuse of srcB here, pointing it to foreign memory,
     is only legal because it is used only in const contexts. */
  rc = s2_tmpl_to_code(e, &srcB, &xbuf, &topt);
  if(!rc){
    cwal_buffer * rb = cwal_new_buffer(e, 0);
    if(!rb) rc = CWAL_RC_OOM;
    else{
      s2_buffer_swap(rb, &xbuf)
        /* transfer ownership of memory, but be sure to restore
           rb->self!!! */;
      assert(!xbuf.self);
      assert(!xbuf.mem);
      assert(rb->self);
      *rv = cwal_buffer_value(rb);
    }
  }else{
    switch(rc){
      case CWAL_RC_RANGE:
        rc = cwal_exception_setf(args->engine, rc,
                                 "Invalid tag configuration(?). "
                                 "See the s2_tmpl_to_code() API "
                                 "docs.");
        break;
      default:
        break;
    }
  }
  cwal_buffer_clear(e, &xbuf);
  return rc;
}



#undef MARKER
