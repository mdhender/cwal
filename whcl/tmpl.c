/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#include <assert.h>
#include "internal.h"
/* #include <stdlib.h> */
#include <string.h>
#include <ctype.h>

#if 1
#include <stdio.h>
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__); if(1) printf pfexp
#else
#define MARKER(pfexp) (void)0
#endif

const whcl_tmpl_opt whcl_tmpl_opt_empty = whcl_tmpl_opt_empty_m;

int whcl_tmpl_to_code( whcl_engine * const el,
                       cwal_buffer const * const src,
                       cwal_buffer * const dest,
                       whcl_tmpl_opt const * opt){
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
  char const * hDocPrefix;
  char const * hDocId = "ⒺⓄⒻ" /*"ᐊᐊ"*/ /*heredoc ID used by output blocks */;
  char const * errMsg = 0;
  char const * openStart = 0;
  char const * outCmd =
    0 ? "whcl.tmpl.out" : "echo -n -s";
  char const * outAlias =
    1 ? NULL : "ⓞⓤⓣ" /*"➤➤"*/ /* "ᐊᐊ" */ /*"ᗛᗛ"*/ /* local var alias for OUT */
    /* "The problem" here is that we don't want to inject any new
       symbols, as they may well get eval'd in the caller's scope. Because
       outCmd can be a builtin, we cannot take a reference to it like we can
       a function. */;
  int rc = 0;
  bool hIsOpen = 0 /* heredoc currently opened? */;
  cwal_buffer hTag = cwal_buffer_empty;
  cwal_engine * const e = el->ec;
  if(!src || !dest) return CWAL_RC_MISUSE;
  if(!opt) opt = &whcl_tmpl_opt_empty;
  else{
    /* Set up custom options... */
    if(opt->outputCommand && *opt->outputCommand){
      outCmd = opt->outputCommand;
      outAlias = NULL;
    }
    if(opt->heredocId && *opt->heredocId) hDocId = opt->heredocId;
    if(opt->tagCodeOpen && opt->tagCodeClose){
      tagOpenScript = opt->tagCodeOpen;
      nOpenScript = (int)cwal_strlen(tagOpenScript);
      tagCloseScript = opt->tagCodeClose;
      nCloseScript = (int)cwal_strlen(tagCloseScript);
    }
    if(opt->tagExprOpen && opt->tagExprClose){
      tagOpenValue = opt->tagExprOpen;
      nOpenValue = (int)cwal_strlen(tagOpenValue);
      tagCloseValue = opt->tagExprClose;
      nCloseValue = (int)cwal_strlen(tagCloseValue);
    }
    if(nOpenValue<1 /* all tags/ids need a length of at least 1 */
       || nCloseValue<1
       || nOpenScript<1
       || nCloseScript<1
       || cwal_strlen(outCmd)<1
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
  rc = cwal_buffer_reserve(e, dest, src->capacity)
    /* very possibly not enough, but we cannot guess
       the approximate final size. */;
  if(rc) goto end;
  //rc = cwal_buffer_printf(e, dest,
  //                        "whcl.install-api tmpl\n");
  if(rc) goto end;
  if(outAlias){
    rc = cwal_buffer_printf(e, dest,
                            "expr [info is-local %s] || "
                            "[decl %s whcl.tmpl.out]\n",
                            outAlias, outAlias);
    if(rc) goto end;
    outCmd = outAlias;
  }
  rc = cwal_buffer_printf(e, &hTag, "%s <<<:", outCmd);
  if(rc) goto end;
  hDocPrefix = (char const *)hTag.mem;
  if(1){
    /*
      Scan the first bytes and ignore any leading spaces before an opening
      <? or <%. If the first non-space character is not one of those patterns,
      retain the leading space.
    */
    char const * x = pos;
    for( ; x<end && *x && ('\n'==*x || tok1_is_space(*x)); ++x) continue;
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
        rc = cwal_buffer_printf(e, dest, " %s\n", hDocId);
        hIsOpen = false;
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
          hIsOpen = true;
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
        rc = cwal_buffer_printf(e, dest, " %s\n%s ( ",
                                hDocId, outCmd);
        hIsOpen = false;
      }else if(openStart == origin
               /*workaround for <% at starting pos. */){
        rc = cwal_buffer_printf(e, dest, "%s ( ", outCmd);
        hIsOpen = true;
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
                                  " )"/*<-- end expr block*/"\n%s%s ",
                                  hDocPrefix, hDocId);
          hIsOpen = true;
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
        rc = cwal_buffer_printf(e, dest, "%s%s ",
                                hDocPrefix, hDocId);
        hIsOpen = true;
      }
      if(!rc) rc = cwal_buffer_append(e, dest, pos, 1);
    }
  }
  end:
  if(!rc){
    if(hIsOpen){
      rc = cwal_buffer_printf(e, dest, " %s\n", hDocId);
    }
    if(rc) goto end;
  }
  cwal_buffer_reserve(e, &hTag, 0);
  return rc;
  syntax_err:
  {
    tok1_linecol_t line = 1, col = 0;
    assert(errMsg);
    assert(0 != rc);
    tok1_count_lines( (char const *)src->mem,
                      (char const *)src->mem+src->used,
                      pos, &line, &col);
    return cwal_exception_setf(e, rc,
                               "tmpl syntax error at "
                               "line %d, col %d: %s",
                               line, col, errMsg);
  }
}

static int whcl__cb_tmpl_out( cwal_callback_args const * args, cwal_value ** rv ){
  int rc = 0;
  whcl_engine * const el = whcl_engine_from_args(args);
  cwal_buffer * const buf =
    (cwal_buffer *)cwal_args_state(args, &cwal_buffer_empty);
  assert(buf);
  for(uint16_t i = 0; 0==rc && i < args->argc; ++i){
    rc = whcl_value_to_buffer(el, cwal_buffer_reuse(buf),
                              args->argv[i]);
    if(0==rc && buf->used){
      rc = cwal_output(args->engine, buf->mem, buf->used);
    }
  }
  if(0==rc) *rv = args->self;
  return rc;
}


int whcl_cb_tmpl_to_code( cwal_callback_args const * args, cwal_value ** rv ){
  int rc;
  char const * src;
  cwal_buffer srcB = cwal_buffer_empty;
  cwal_buffer xbuf = cwal_buffer_empty;
  whcl_engine * const el = whcl_engine_from_args( args );
  cwal_engine * const e = args->engine;
  whcl_tmpl_opt topt = whcl_tmpl_opt_empty;
  uint16_t argNdx = 0;
  char const * argStr = NULL;
  cwal_size_t argLen = 0;
  bool doScript = true;
  for(; whcl_arg_has_flag(args, &argNdx, &argStr, &argLen);
      argStr = NULL, argLen = 0){
    if(7==argLen && 0==memcmp("-buffer", argStr, 7)){
      doScript = false;
      continue;
    }
    --argNdx;
    argStr = NULL;
    argLen = 0;
    break;
  }
  cwal_value * optObj = args->argc > argNdx+1
    ? args->argv[argNdx+1]
    : 0;
  src = args->argc > argNdx
    ? cwal_value_get_cstr(args->argv[argNdx], &srcB.used)
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
        CHECK("code-open", tagCodeOpen);
        CHECK("code-close", tagCodeClose);
        CHECK("expr-open", tagExprOpen);
        CHECK("expr-close", tagExprClose);
        CHECK("output-command", outputCommand);
#undef CHECK
  }

  srcB.mem = (unsigned char *)src;
  /* Note that our misuse of srcB here, pointing it to foreign memory,
     is only legal because it is used only in const contexts. */
  rc = whcl_tmpl_to_code(el, &srcB, &xbuf, &topt);
  if(rc){
    switch(rc){
      case CWAL_RC_RANGE:
        rc = cwal_exception_setf(args->engine, rc,
                                 "Invalid tag configuration(?). "
                                 "See the whcl_tmpl_to_code() API "
                                 "docs.");
        break;
      default:
        break;
    }
  }else if(doScript){
    whcl_script * sc = NULL;
    //MARKER(("xbuf=%.*s\n", (int)xbuf.used, (char const*)xbuf.mem));
    rc = whcl_compile_buffer_take(el, &sc, "tmpl script",
                                  &xbuf);
    if(0==rc){
      rc = whcl__script_to_value(sc, true, rv);
      if(rc) whcl_script_free(sc);
    }
  }else{
    cwal_buffer * rb = cwal_new_buffer(e, 0);
    if(!rb){WHCL__WARN_OOM; rc = CWAL_RC_OOM;}
    else{
      cwal_buffer_swap_mem(rb, &xbuf);
      *rv = cwal_buffer_value(rb);
    }
  }
  cwal_buffer_clear(e, &xbuf);
  return rc;
}

int whcl__install_tmpl( whcl_engine * const el ){
  if(el->cache.installAPI & WHCL__INSTALL_API_tmpl) return 0;
  int rc = 0;
  while(0==rc){
    whcl_func_def const funcs[] = {
      WHCL_FUNC2("tmpl", whcl_cb_tmpl_to_code),
      whcl_func_def_empty_m
    };
    rc = whcl_install_functions(el, el->cache.vWhcl, funcs,
                                CWAL_VAR_F_CONST);
    if(rc) break;
    cwal_value * const vTmpl = whcl_get(el, el->cache.vWhcl, "tmpl", 4);
    assert(vTmpl);
    cwal_value * const tmplBuf = cwal_new_buffer_value(el->ec, 256);
    if(!tmplBuf){
      WHCL__WARN_OOM;
      rc = CWAL_RC_OOM;
      break;
    }
    // Add some internal state to vTmpl...
    cwal_ref(tmplBuf);
    rc = whcl_stash_set(el, "tmpl.out buffer", tmplBuf)
      /* Keep tmplBuf alive. */;
    cwal_unref(tmplBuf);
    if(rc) break;
    whcl_func_def const funcs2[] = {
    WHCL_FUNC5("out", whcl__cb_tmpl_out,
               cwal_value_get_buffer(tmplBuf), NULL,
               &cwal_buffer_empty),
      whcl_func_def_empty_m
    };
    rc = whcl_install_functions(el, vTmpl, funcs2, 0);
    if(rc) break;
    break;
  }
  if(0==rc) el->cache.installAPI |= WHCL__INSTALL_API_tmpl;
  return rc;
}


#undef MARKER
