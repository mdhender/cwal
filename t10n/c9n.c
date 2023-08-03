/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "c9n.h"

#if 1
#include <stdio.h>
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:\t",__FILE__,__LINE__); if(1) printf pfexp
#define c9n__dump_cptoken(lbl,CT,TOK) MARKER(("%s: token type %d/%s @ %d,%d: %.*s\n", lbl, (TOK)->ttype, t10n_ttype_cstr((TOK)->ttype), \
                                             (TOK)->line, (TOK)->column, \
                                             (int)((TOK)->length), (CT)->begin +(TOK)->begin))
#else
#define MARKER(pfexp) (void)0
#define c9n__dump_cptoken(lbl,CT,TOK) (void)0
#endif

/**
   Still on the fence about this... If we report c9n_toker allocs via
   this mechanism then we end up effectively duplicating the metrics.
   If we don't, the cwal-level metrics do not report this memory.
*/
#define cwal_engine_adjust_client_mem(X,Y) (void)0

#define NO_ID t10n_token_id_none
/**
   The ID of a c9n_toker::chain's EOF token.
*/
#define CT_EOF_ID(CT) ((t10n_token_id)((CT)->chainLength))
#if 0
const t10n_byte_range t10n_byte_range_empty = t10n_byte_range_empty_m;
#endif
const c9n_toker c9n_toker_empty = c9n_toker_empty_m;
const c9n_token c9n_token_empty = c9n_token_empty_m;

/**
   Returns the top-most c9n_toker in ct's hierarchy, following
   ->parent. May return ct.
*/
c9n_toker const * c9n_toker_top( c9n_toker const * ct ){
  return ct->top ? ct->top : ct;
}

t10n_linecol_t c9n_token_len( c9n_token const * k, bool inner ){
  return inner && k->innerOffset ? k->innerLength : k->length;
}

char const * c9n_token_cstr( c9n_toker const * ct,
                             c9n_token const * k, cwal_size_t * len,
                             bool innerOnly ){
  char const * tBegin = c9n_toker_top(ct)->begin;
  if(innerOnly && k->innerOffset){
    if(len) *len = (cwal_size_t)(k->innerLength);
    return k->innerLength ? (tBegin + k->begin + k->innerOffset) : "";
  }
  if(len) *len = (cwal_size_t)(k->length);
  return k->length ? (tBegin + k->begin) : "";
}

char const * c9n_toker_name_first( c9n_toker const * t,
                                   cwal_size_t * len ){
  while(t && !t->name) t = t->parent;
  if(t && t->name && len){
    *len = cwal_strlen(t->name);
  }
  return t->name;
}

#if 0
/**
   UNTESTED!

   If pos is a position in [ct->begin,ct->end), this returns the token
   closest to pos, prefering to return a token whose range contains
   pos. If pos does not lie exactly within a token (e.g.  because it
   is at a position where a "junk" token lives) then the closest token
   to the left is returned, but that case "shouldn't" happen if the
   API is used properly, as no public-facing tokens will point to such
   an address. It returns 0 if it can't find anything, which could
   potentially happen if pos is in a junk token before the first
   non-junk token in ct->chain.
*/
static c9n_token const * c9n_toker_token_near( c9n_toker const * ct,
                                                     char const * pos ){
  c9n_token const * rc = 0;
  ct = c9n_toker_top( ct );
  if(pos >= ct->begin && pos<ct->end){
    c9n_token const * tok = 0;
    c9n_token const * prev = 0;
    t10n_token_id ndx;
    char const * tbeg;
    /* TODO: binary search rather than linear. */
    for( ndx = 0; ndx < (t10n_token_id)ct->chainLength;
         ++ndx, prev = tok ){
      tok = ct->chain + ndx;
      tbeg = ct->begin + tok->begin;
      if(tbeg >= pos){
        rc = (pos < tbeg + tok->length)
          ? tok
          : prev;
        break;
      }
    }
  }
  return rc;
}
#endif /* c9n_toker_token_near() */

#if 0
static c9n_token const * c9n_toker_eof_token(c9n_toker const * ct){
  assert(CT_EOF_ID(ct) >= 0);
  return &ct->chain[CT_EOF_ID(ct)];
}
#endif

static c9n_token const * c9n_toker_err_pos( c9n_toker const * ct ){
  c9n_token const * rc = 0;
  if(ct->chain){
    rc = c9n_toker_at( ct, ct->errToken );
    if(!rc){
      rc = c9n_toker_at( ct, ct->token );
      if(!rc){
        rc = c9n_toker_at( ct, ct->pbToken );
        if(!rc) rc = &ct->chain[0];
      }
    }
  }
  return rc;
}

/**
   Internal helper to fetch ct's line/column. It uses the position of
   ct->errToken or ct->token (in that order). It accounts for
   ct->lineOffset and ct->colOffset, using the first one of those
   which is set in ct or one of its parents.
*/
static void c9n_toker_linecol( c9n_toker const * ct,
                                  t10n_linecol_t * line,
                                  t10n_linecol_t * col ){
  c9n_token const * errPos = c9n_toker_err_pos(ct);
  *line = errPos->line;
  *col = errPos->column;
  /**
     See if ct or one of its parents has line/col offset info. This
     historically happens for functions, where the body of the
     function (living in one tokenizer) necessarily gets copied for
     later use by the function (potentially after the original
     tokenizer is long gone). The function stores that line/col/script
     name info so that error reporting from the function body can
     report the proper error location.
  */
  for( ; ct ; ct = ct->parent){
    /* FIXME: this loop probably isn't quite right when we have
       multiple levels of nesting and the offset isn't set directly on
       ct. That said, code which makes use of the offset is expected
       to set it up properly. This algo follows what t10n_toker has
       historically done and it behaves for the use(s) its applied to
       (which has only been to correct the file location of function
       bodies). */
    if(ct->lineOffset){
      MARKER(("Adjusting for line/colOffset: %d,%d\n", *line, *col));
      *col += (1==*line) ? ct->colOffset : 0;
      *line += ct->lineOffset - 1;
      MARKER(("Adjusted for line/colOffset: %d,%d\n", *line, *col));
      break;
    }
  }
}

static int c9n_err_toker_impl(bool throwIt,
                                c9n_toker const * ct,
                                int code, char const * fmt,
                                va_list vargs ){
  cwal_engine * e = ct->e;
  int rc = 0;
  char const * name;
  cwal_size_t nameLen = 0;
  cwal_error * const err = &e->err;
  cwal_buffer * const obuf = &err->msg;
  t10n_linecol_t line = 0, column = 0;
  c9n_toker_linecol(ct, &line, &column);
  cwal_engine_error_reset(e);
  err->code = code ? code : (throwIt ? CWAL_RC_EXCEPTION : CWAL_SCR_SYNTAX);
  name = c9n_toker_name_first(ct, &nameLen);
  if(CWAL_RC_OOM==code){
    rc = CWAL_RC_OOM;
  }
  else if(!throwIt && line) {
    /*    ^^^^^^^^ if it will be thrown, the name/line/col get
          injected differently */
    char const * tailPart = ": ";
    if(nameLen){
      rc = cwal_buffer_printf( e, obuf, "%.*s:",
                               (int)nameLen, name );
    }
    if(!rc){
      if(nameLen){
        rc = cwal_buffer_printf( e, obuf, "%d:%d%s",
                                 line, column,
                                 tailPart);
      }else{
        rc = cwal_buffer_printf( e, obuf, "line %d, col %d%s",
                                 line, column,
                                 tailPart);
      }
    }
  }

  if(!rc){
    if(fmt && *fmt){
      rc = cwal_buffer_printfv(e, obuf, fmt, vargs);
    }else{
      rc = cwal_buffer_printf(e, obuf,
                              "Error #%d (%s)%s%s",
                              code, cwal_rc_cstr(code),
                              (ct->errMsg ? ": " : ""),
                              ct->errMsg ? ct->errMsg : "");
    }
  }
  err->line = line;
  err->col = column;
  err->script.used = 0;
  if(!rc && name){
    rc = cwal_buffer_append( e, &err->script, name, nameLen );
  }
  if(!rc && throwIt){
    rc = cwal_error_throw(e, err, name, line, column);
  }
  return rc ? rc : code;
}

int c9n_err_toker( c9n_toker const * ct, int code, char const * fmt, ... ){
  int rc;
  va_list args;
  va_start(args,fmt);
  rc = c9n_err_toker_impl(0, ct, code, fmt, args);
  va_end(args);
  return rc;
}

int c9n_throw_toker( c9n_toker const * ct, int code, char const * fmt, ... ){
  int rc;
  va_list args;
  va_start(args,fmt);
  rc = c9n_err_toker_impl(1, ct, code ? code : CWAL_RC_EXCEPTION,
                          fmt, args);
  va_end(args);
  return rc;
}



/**
   Copies the state from p to c. ct must be the tokenizer in charge of
   both p and c.
*/
static int t10n_token_to_cptoken( c9n_toker * const ct,
                                 t10n_token const * const p,
                                 c9n_token * const c ){
  static uint32_t const maxLCval = (t10n_linecol_t)-1;
  assert(t10n_token_begin(p) >= ct->begin && t10n_token_end(p)<=ct->end);
#define ERR_RNG(M) ct->errMsg=M; return CWAL_RC_RANGE
  c->ttype = p->ttype;
  if(c->line>maxLCval){ERR_RNG("Line counter overflow.");}
  c->line = (t10n_linecol_t)p->line;
  if(c->column>maxLCval){ERR_RNG("Column counter overflow.");}
  c->column = (t10n_linecol_t)p->column;
  c->begin = (uint32_t)(t10n_token_begin(p) - ct->begin);
  if((uint32_t)t10n_token_len(p)>maxLCval){
    ERR_RNG("Single token exceeds size limit.");
  }
  c->length = (t10n_linecol_t)t10n_token_len(p);
  if(t10n_token_adjbegin(p)){
    c->innerOffset = (t10n_linecol_t)(t10n_token_adjbegin(p)
                                    - t10n_token_begin(p));
    assert(t10n_token_adjend(p) && "adjEnd must be set if adjBegin is!");
    c->innerLength = (t10n_linecol_t)(t10n_token_adjend(p)
                                    - t10n_token_adjbegin(p));
  }else{
    c->innerLength = c->length;
  }
#undef ERR_RNG
  return 0;
}

/**
   UNTESTED.
*/
void c9n_token_to_ptoken( c9n_toker * ct, c9n_token const * src,
                           t10n_token * dest ){
  dest->id = src->id;
  dest->ttype = src->ttype;
  t10n_token_begin_set(dest, ct->begin + src->begin);
  t10n_token_end_set(dest, ct->begin + src->begin + src->length);
  dest->line = src->line;
  dest->column = src->column;
  if(src->innerOffset){
    t10n_token_adjbegin_set(dest, ct->begin + src->begin + src->innerOffset);
    t10n_token_adjend_set(dest, t10n_token_adjbegin(dest) + src->innerLength);
  }
}

/**
   Internal helper to fetch the next token from pt. Used both for
   counting the tokens (to figure out how much memory to allocate) and
   then for compilation. Errors are reported via ct->se.

   If n is not NULL, it is incremented on success. Returns 0 on
   success and updates ct->se's error state on error.
*/
static int c9n_toker_next_for_compile( c9n_toker * ct, t10n_toker * pt,
                                         uint32_t flags, uint32_t * n ){
  int rc = 0;
  int prevTokType = pt->token.ttype;
  cwal_engine_error_reset(ct->e);
  while(1){
    rc = t10n_toker_next_token(pt);
    if(rc) break;
    else if(t10n_ttype_is_junk(pt->token.ttype)){
      if(!(C9N_TOKER_F_RETAIN_JUNK & flags)){
        prevTokType = pt->token.ttype/* so we can catch EOLs after comments */;
        continue;
      }
    }
    switch(pt->token.ttype){
      case T10N_T_HeredocStart:
        rc = t10n_slurp_heredoc(ct->e, pt, 0);
        break;
#if 0
      case T10N_T_Semicolon:
        pt->token.ttype = T10N_T_EOX;
        break;
#endif
      case T10N_T_NL:
        pt->token.ttype = T10N_T_EOL;
        /* FALLTHRU */
      case T10N_T_EOL:
        switch(prevTokType){
#if 1
          case T10N_T_EOL:
            /* Collapse runs of EOLs */
            continue;
#else
          case T10N_T_SquigglyClose:
          case T10N_T_CommentCpp:
          case T10N_T_CommentC:
            /* The only place newlines are *potentially* relevant in
               s2's grammar is immediately after certain LHS
               expressions which end with a {block}. The various token
               type checks above are a feeble attempt to catching a
               case of a {} followed by a newline and/or comment (or a
               series of C-style comments, possibly followed by a
               C++-style comment!). We can improve that later, but
               have to add state to this call to do it. */
            break;
          default:
            /* Treat is as junk. */
          continue;
#endif
        }
        /* t10n_next_token() does this translation and s2_eval.c currently
           relies on it happening there:
           pt->token.ttype = T10N_T_EOL;
        */
        break;
      default:
        break;
    }
    if(n) ++*n;
    break;
  }
  if(rc && !ct->e->err.code){
    rc = t10n_err_ptoker(ct->e, pt, rc, "%s", pt->errMsg);
  }
  return rc;
}

static t10n_token_id c9n_toker_t_set( c9n_toker * ct,
                                        c9n_token const * tok,
                                        t10n_token_id * member ){
  c9n_toker const * top = c9n_toker_top(ct);
  if(tok && tok->id>0 && tok->id<=(t10n_token_id)top->chainLength){
    return *member = tok->id;
  }else{
    return 0;
  }
}

int c9n_toker_token_set( c9n_toker * ct, c9n_token const * tok ){
    return c9n_toker_t_set(ct, tok, &ct->token);
}

int c9n_toker_errtoken_set( c9n_toker * ct, c9n_token const * tok ){
  return c9n_toker_t_set(ct, tok, &ct->errToken);
}

int c9n_toker_pb_set( c9n_toker * ct, c9n_token const * tok ){
    return c9n_toker_t_set(ct, tok, &ct->pbToken);
}

int c9n_toker_next_set( c9n_toker * ct, c9n_token const * tok ){
    return c9n_toker_t_set(ct, tok, &ct->nextToken);
}


#if 0
/**
   Tokenizes pt from its current position until the end and returns
   a count of tokens via *n. Returns 0 on success. Errors are reported
   via ct->se and the non-0 error code is returned.
*/
static int c9n_toker_count_tokens( c9n_toker * ct, t10n_toker * pt,
                                    uint32_t flags, uint32_t * n ){
  int rc = 0;
  while( !(rc = c9n_toker_next_for_compile(ct, pt, flags, n) ) ){
    /*MARKER(("token #%u: %s\n", c, t10n_ttype_cstr(pt->token.ttype)));*/
    if(T10N_T_EOF==pt->token.ttype) break;
  }
  return rc;
}
#endif

/**
   Internal non-const version of c9n_toker_at().
*/
static c9n_token * c9n_toker_at_nc(c9n_toker const * ct, t10n_token_id id){
  c9n_toker const * top = c9n_toker_top(ct);
  return top->chainLength && id>0 && id<=(t10n_token_id)top->chainLength
    ? top->chain+id-1 : NULL;
}

c9n_token const * c9n_toker_at(c9n_toker const * ct,
                                 t10n_token_id id){
  return c9n_toker_at_nc(ct, id);
}


/**
   Ensures that ct->chain has at least n elements reserved.
   It never shrinks the chain unless n is 0, in which case
   it frees the chain (it's up to the caller to only reserve
   entries on instances which own their chain).

   On success, ct->chainCapacity is updated. If n is 0, ct->chain and
   ct->chainLength are also zeroed out.

   Returns ct->chain on success (noting that it may have been
   reallocated), NULL on allocation error.
*/
static c9n_token * c9n_toker_chain_reserve( c9n_toker * ct, uint32_t n ){
  if(!n){
    if(ct->chain){
      assert(ct->chainCapacity && "Trying to free an unowned chain?");
      cwal_free2(ct->e, ct->chain, sizeof(c9n_token) * ct->chainCapacity);
    }
    cwal_engine_adjust_client_mem(ct->e,
                                  -(cwal_int_t)(ct->chainCapacity * sizeof(c9n_token)));
    ct->chain = 0;
    ct->chainLength = ct->chainCapacity = 0;
    return 0;
  }
  if(ct->chainCapacity < n){
    c9n_token * chain = (c9n_token *)cwal_realloc(ct->e, ct->chain, n * sizeof(c9n_token));
    if(!chain){
      ct->errMsg = "Out of memory - cannot allocate token chain.";
      return 0;
    }
    if(ct->chainCapacity){
      cwal_engine_adjust_client_mem(ct->e,
                                    -((cwal_int_t)(ct->chainCapacity * sizeof(c9n_token))));
    }
    cwal_engine_adjust_client_mem(ct->e, (cwal_int_t)(n * sizeof(c9n_token)));
    ct->chainCapacity = n;
    ct->chain = chain;
  }
  return ct->chain;
}


/**
   Expects its argument to be one of:

   T10N_T_SquigglyOpen, T10N_T_SquigglyGroup,
   T10N_T_ParenOpen, T10N_T_ParenGroup,
   T10N_T_BraceOpen, T10N_T_BraceGroup:

   and asserts if passed any other type.

   Returns the matching group token type.
*/
static enum t10n_token_types_e t10n_ttype_group_type(int16_t ttype){
  switch(ttype){
    case T10N_T_SquigglyOpen:
    case T10N_T_SquigglyGroup:
      return T10N_T_SquigglyGroup;
    case T10N_T_ParenOpen:
    case T10N_T_ParenGroup:
      return T10N_T_ParenGroup;
      break;
    case T10N_T_BraceOpen:
    case T10N_T_BraceGroup:
      return T10N_T_BraceGroup;
    default:
      assert(!"internal misuse");
      return T10N_T_INVALID;
  }
}

/**
   Expects its argument to be one of:

   T10N_T_SquigglyOpen, T10N_T_ParenOpen, T10N_T_BraceOpen:

   and asserts if passed any other type.

   Returns the matching closing token type.
*/
static enum t10n_token_types_e t10n_ttype_closer_type(int16_t ttype){
  switch(ttype){
    case T10N_T_SquigglyOpen:
      return T10N_T_SquigglyClose;
    case T10N_T_ParenOpen:
      return T10N_T_ParenClose;
      break;
    case T10N_T_BraceOpen:
      return T10N_T_BraceClose;
    default:
      assert(!"internal misuse");
      return T10N_T_INVALID;
  }
}

/**
   Requires tok->ttype to be one of:

   T10N_T_SquigglyOpen, T10N_T_ParenOpen, T10N_T_BraceOpen:

   This routine searches ct->chain for a matching closing element. If
   found, *ender is assigned to that token and 0 is returned, else an
   error is reported via ct->e and that error's code is returned.
*/
static int c9n_toker_find_closer( c9n_toker * ct, c9n_token const * tok,
                                  c9n_token ** ender ){
  enum t10n_token_types_e const closer = t10n_ttype_closer_type(tok->ttype);
  c9n_token * at = 0;
  unsigned int level = 0;
  assert(tok->ttype==T10N_T_SquigglyOpen
         || tok->ttype==T10N_T_ParenOpen
         || tok->ttype==T10N_T_BraceOpen);
  while(1){
    at = c9n_toker_at_nc(ct, at ? at->nextId : tok->nextId);
    switch(at->ttype){
      case 0:
        assert(!"should not be able to happen at this level");
        CWAL_SWITCH_FALL_THROUGH;
      case T10N_T_EOF:
        goto mismatch;
      case T10N_T_SquigglyOpen:
      case T10N_T_ParenOpen:
      case T10N_T_BraceOpen:
        if(at->ttype == tok->ttype) ++level;
        break;
      default:
        if(closer == at->ttype){
          if(level) --level;
          else{
            *ender = at;
            return 0;
          }
        }
        break;
    }
  }
  mismatch:
  ct->errToken = tok->id;
  return c9n_err_toker( ct, CWAL_SCR_SYNTAX,
                         "Mismatched '%c' starting at line %d column %d.",
                         (char)tok->ttype, tok->line, tok->column);
}

/**
   Walks through the given token chain and may make "certain
   modifications" to it:

   - A sequence of ((T10N_T_OpDot|T10N_T_OpColon2) T10N_T_Identifier)
   re-tags the identifier token as T10N_T_PropertyKey.

   - Grouping tokens (), [], {}, are transformed such that:

   1) The left-most token gets converted from (e.g.) T10N_T_ParenOpen
   to (e.g.) T10N_T_ParenGroup.

   2) The left-most token->innerId gets set to the ID of the first
   token "inside" the group.

   3) The left-most token->nextId gets set to the token *after* the group's
   closing paren/brace/squiggly token.

   4) The group-closing token gets transformed to an EOF token, so
   that traversing that section of the chain will inherently end when
   it is encountered (without having to know what type of group it
   is). These tokens, unlike "real" EOF tokens, have a length of 1,
   thus it is possible to differentiate them (should it be needed
   (historically it has not been needed)).

   e.g. a token chain of:

   a ( b c() d ) e<EOF>

   gets transformed to:

   a e<EOF>
    \
     b c d<EOF>
        \
         <EOF>

   Recursively, of course.


   The second argument is for recursion purposes. It must initially be
   passed ct->chain.

   Returns 0 on success. Since the input chain has already been fully
   tokenized, the only error cases this routine will encounter are
   mismatched grouping tokens.
*/
static int c9n_toker_post_compile_chain( c9n_toker * ct, c9n_token * chain ){
  c9n_token * tok = chain;
  c9n_token const * tPrev = 0;
  int rc = 0;
  assert(tok && "Internal misuse.");
  while(1){
    switch(tok->ttype){
      case T10N_T_Identifier:
        if(tPrev && (T10N_T_OpDot==tPrev->ttype
                     ||T10N_T_OpColon2==tPrev->ttype)){
          tok->ttype = T10N_T_PropertyKey;
        }
        break;
      case T10N_T_SquigglyOpen:
      case T10N_T_ParenOpen:
      case T10N_T_BraceOpen:{
        c9n_token * closer = 0;
        c9n_token * inner = 0;
        rc = c9n_toker_find_closer(ct, tok, &closer);
        if(rc) break;
        assert(closer);
        if(closer==c9n_toker_at(ct, tok->nextId)){
          /* Empty block. */
          /* The closer token (tok->nextId) will get transformed into
             an EOF below, so we'll use that token rather than the
             outermost EOF. */
          tok->innerId = tok->nextId;
        }else{
          tok->innerId = tok->nextId;
          inner = c9n_toker_at_nc(ct, tok->innerId);
        }
        /* Extend tok to wrap the whole block... */
        tok->nextId = closer->nextId;
        tok->length = closer->begin - tok->begin + closer->length;
        tok->ttype = t10n_ttype_group_type(tok->ttype);
        assert(tok->length >= 2);
        tok->innerOffset = 1;
        tok->innerLength = tok->length - 2;
        /*MARKER(("#%d Block %s: begin=%d, innerLength=%d "
                "body=<<<%.*s>>>\nouter=<<<%.*s>>>\n",
                tok->ndx, t10n_ttype_cstr(tok->ttype),
                (int)tok->begin+tok->innerOffset,
                (int)tok->innerLength,
                (int)tok->innerLength,
                ct->begin + tok->begin + tok->innerOffset,
                (int)tok->length,
                ct->begin+tok->begin));*/
        /* Convert the block's closing token into a virtual EOF for
           the inner part. Algorithms which climb down branches of
           this script rely on the existince of such a token. */
        closer->ttype = T10N_T_EOF;
        closer->nextId = NO_ID;
        closer->length = 1/* Group-closing character. We can hypothetically
                             use this to distinguish the real EOF from virtual
                             ones. */;
        if(inner){
          rc = c9n_toker_post_compile_chain( ct, inner );
        }
        break;
      }
      case T10N_T_EOF:
        break;
      default:
        break;
    }
    if(rc || T10N_T_EOF==tok->ttype) break;
    tPrev = tok;
    tok = c9n_toker_at_nc(ct, tok->nextId);
  }
  return rc;
}

c9n_toker * c9n_toker_alloc( cwal_engine * e ){
  c9n_toker * cp = (c9n_toker *)cwal_malloc2(e, sizeof(c9n_toker));
  if(cp){
    *cp = c9n_toker_empty;
    cwal_engine_adjust_client_mem(e, (cwal_int_t)sizeof(c9n_toker));
    cp->e = e;
    cp->allocStamp = &c9n_toker_empty;
  }
  return cp;
}

void c9n_toker_free( c9n_toker * cp ){
  assert(cp ? !!cp->e : 1);
  if(cp){
    cwal_engine * const e = cp->e;
    void const * const allocStamp = cp->allocStamp;
    c9n_toker_finalize(cp);
    if(&c9n_toker_empty == allocStamp){
      cwal_engine_adjust_client_mem(e, -(cwal_int_t)sizeof(c9n_toker));
      cwal_free2(e, cp, sizeof(c9n_toker));
    }
  }
}

int c9n_toker_init( cwal_engine * e, c9n_toker * ct ){
  void const * const allocStamp = ct->allocStamp;
  if(&c9n_toker_empty == ct->allocStamp){
    assert(e && ct->e==e);
  }
  *ct = c9n_toker_empty;
  ct->e = e;
  ct->allocStamp = allocStamp;
  return 0;
}


void c9n_toker_reset( c9n_toker * ct ){
  ct->token = ct->pbToken = ct->errToken = NO_ID;
  if(ct->chainLength){
    ct->nextToken = ct->chain[0].id;
  }else{
    ct->nextToken = 0;
  }
  /*ct->capture = t10n_byte_range_empty;*/
}



int c9n_toker_sub_from_group( c9n_toker const * parent,
                              c9n_toker * ct ){
  c9n_toker const * top = c9n_toker_top( parent );
  c9n_token * chain = c9n_toker_at_nc(parent, parent->token);
  if(!chain) return CWAL_RC_MISUSE;
  else if(!t10n_ttype_group_type(chain->ttype)){
    return CWAL_RC_TYPE;
  }
  assert(chain->innerId);
  ct->top = top;
  ct->parent = parent;
  ct->chain = c9n_toker_at_nc(parent, chain->innerId);
  assert(ct->chain);
  ct->e = parent->e;
  c9n_toker_reset(ct);
  ct->lineOffset = chain->line;
  ct->colOffset = chain->column;
  ct->chainLength = ct->chainCapacity = 0 /*tag that ct does not own ct->chain*/;
  ct->begin = top->begin + chain->begin;
  ct->end = ct->begin + chain->length;
  ct->name = parent->name;
  return 0;
}


int c9n_toker_compile_buffer( c9n_toker * ct, cwal_buffer const * buf,
                              uint32_t compileFlags ){
  return c9n_toker_compile(ct, buf->mem ? (char const *)buf->mem : "",
                            buf->mem ? (cwal_int_t)buf->used : 0,
                            compileFlags);
}

int c9n_toker_compile( c9n_toker * ct, char const * src,
                       cwal_int_t len, uint32_t flags ){
  t10n_toker pt = t10n_toker_empty;
  int rc = t10n_toker_init( &pt, src, len, 0 );
  if(!rc){
    rc = c9n_toker_compile_ptoker( ct, &pt, flags );
  }
  t10n_toker_finalize( &pt );
  return rc;
}

/**
   Returns a guesstimated number of tokens which are remaining in the
   given tokenizer. This is a dumb heuristic based solely on the
   "remaining" size of the tokenizer, counting from the start of its
   current token to the end of the tokenizer, plus 1 (to account for
   an EOF token).
*/
static uint32_t c9n_toker_estimate_token_count(t10n_toker const * pt){
  if(t10n_toker_is_eof(pt)) return 1;
  else{
    const char * begin = t10n_token_begin(&pt->token)
      ? t10n_token_begin(&pt->token)
      : t10n_toker_begin(pt);
    const char * end = t10n_toker_end(pt);
    return (((uint32_t)(end - begin))*55/10) / sizeof(c9n_token) + 1;
    /*
      Basic tests suggests that the input size times 5.5 is the
      approximate amount of token memory we'll need on 32-bit builds.
      Hmmm... this ARM box is 64-bit, but Raspberry hasn't yet
      released a 64-bit OS for it. This whole time i've been thinking
      sizeof(void*) was 8 :/.
    */
  }
}

/**
   If ct->chain's capacity is "significantly larger" than its length,
   reallocate the chain to trim it to its current length, otherwise
   don't bother. Reallocation errors are simply ignored, as they're
   not fatal here.
*/
static void c9n_toker_chain_trim( c9n_toker * ct ){
  if(ct->chainCapacity >= 20/*arbitrary*/
     && ct->chainCapacity > ct->chainLength * 12/10){
    void * re = cwal_realloc(ct->e, ct->chain, ct->chainLength * sizeof(c9n_token));
    if(re){
      cwal_engine_adjust_client_mem(ct->e,
                                    -((cwal_int_t)(ct->chainCapacity * sizeof(c9n_token))));
      ct->chain = (c9n_token*)re;
      ct->chainCapacity = ct->chainLength;
      cwal_engine_adjust_client_mem(ct->e,
                                    ((cwal_int_t)(ct->chainCapacity * sizeof(c9n_token))));
    }
    /* Ignore realloc error - it's not fatal here. */
  }
}

int c9n_toker_compile_ptoker( c9n_toker * ct, t10n_toker const * psrc, uint32_t flags ){
  int rc = 0;
  t10n_token_id ndx = NO_ID;
  t10n_toker pt = *psrc;
  assert(ct->e && "ct has not been c9n_toker_init()'d");
  assert(!psrc->compiled);
  if(ct->parent){
    return cwal_error_set(ct->e, NULL, CWAL_RC_MISUSE,
                             "Cannot compile a sub-tokenizer.");
  }
  ct->chainLength = 0 /* re-use any chain we currently have */;
  c9n_toker_reset(ct);
  ct->begin = t10n_toker_begin(&pt);
  ct->end = t10n_toker_end(&pt);
  if(!c9n_toker_chain_reserve(ct, c9n_toker_estimate_token_count(&pt))){
    return CWAL_RC_OOM;
  }
  if(pt.name){
    ct->name = t10n_toker_name_first(psrc, 0);
  }
  t10n_toker_reset(&pt);
  if(C9N_TOKER_F_IDENTIFIER_DASHES & flags){
    pt.flags |= T10N_F_IDENTIFIER_DASHES;
  }else{
    pt.flags &= ~T10N_F_IDENTIFIER_DASHES;
  }
  pt.flags |= T10N_F_LINEAR_TOKER;
  assert(ct->chainCapacity>0);
  for(ndx = 0; 0==(rc = c9n_toker_next_for_compile(ct, &pt, flags, 0));
      ++ndx){
    c9n_token * ctok;
    if(ct->chainCapacity < ndx+1){
      uint32_t const newCap = ct->chainCapacity
        + c9n_toker_estimate_token_count(&pt);
      if(!c9n_toker_chain_reserve(ct, newCap)){
        rc = CWAL_RC_OOM;
        goto err;
      }
    }
    ++ct->chainLength;
    ctok = ct->chain + ndx;
    *ctok = c9n_token_empty;
    ctok->id = ndx+1;
    if(ndx){
      ct->chain[ndx-1].nextId = ctok->id;
    }
    rc = t10n_token_to_cptoken(ct, &pt.token, ctok);
    if(rc) goto err;
#if 0
    {
      cwal_size_t tlen = 0;
      char const * tstr = c9n_token_cstr(ct, ctok, &tlen, false);
      MARKER(("Token #%d: %s @ %d, %d: %.*s\n",
              ctok->id, t10n_ttype_cstr(ctok->ttype),
              ctok->line, ctok->column,
              (int)tlen, tstr));
    }
#endif
    if(t10n_toker_is_eof(&pt)) break;
  }
  if(rc) goto end;
  assert(t10n_toker_is_eof(&pt));
  assert(T10N_T_EOF==ct->chain[ndx].ttype);
  rc = c9n_toker_post_compile_chain(ct, ct->chain);
  if(!rc){
    goto end;
  }
  err:
  assert(ct->errMsg || pt.errMsg);
  assert(rc);
  if(!ct->errMsg){
    assert(pt.errMsg);
    ct->errMsg = pt.errMsg;
  }
  if(ct->chain){
    c9n_token const * et = c9n_toker_at(ct, ndx+1);
    ct->errToken = et ? et->id : NO_ID;
  }
#if 0
  if(!ct->e->err.code){
    rc = c9n_err_toker( ct, rc, "%s",
                        ct->errMsg ? ct->errMsg : pt.errMsg  );
  }
#endif
  end:
  if(!rc){
    c9n_toker_chain_trim(ct);
  }
  ct->name = 0;
  c9n_toker_reset(ct);
  return rc;
}

void c9n_toker_finalize( c9n_toker * ct ){
  if(ct->e){
    /* If ct->top is not 0 then any ct->chain component belongs to a
       parent tokenizer (or someone else, hypothetically). */
    if(!ct->top){
      c9n_toker_chain_reserve(ct, 0);
    }
  }
  *ct = c9n_toker_empty;
}

c9n_token const * c9n_toker_token(c9n_toker const * ct){
  return c9n_toker_at(ct, ct->token);
}

t10n_token_id c9n_toker_putback(c9n_toker * ct){
  ct->token = ct->pbToken;
  ct->pbToken = NO_ID;
  return ct->token;
}

int c9n_toker_next_token( c9n_toker * ct, c9n_token const ** tgt ){
  t10n_token_id id = NO_ID;
  c9n_token const * ctok = 0;
  ct->errToken = NO_ID;
  if(!ct->chain){
    if(tgt) *tgt = 0;
    ct->errMsg = "This chain has no tokens.";
    return CWAL_RC_MISUSE;
  }
  if(NO_ID!=ct->nextToken){
    id = ct->nextToken;
    ct->nextToken = NO_ID;
  }else if(NO_ID==ct->token){
    id = ct->chain[0].id;
  }else{
    ctok = c9n_toker_at(ct, ct->token);
    assert(ctok);
    if(T10N_T_EOF==ctok->ttype){
      /* Once we hit EOF, keep returning that same EOF
        token. Eventually the caller will get the hint. */
      id = ct->token;
    }
    else if(!(id = ctok->nextId)){
      /* Return EOF token */
      id = CT_EOF_ID(ct);
      assert(!"can this even happen?");
    }
  }
  assert(id && "No token ID???");
  if(tgt) *tgt = c9n_toker_at(ct, id);
  ct->pbToken = ct->token;
  ct->token = id;
  return 0;
}

int c9n_token_block_has_content( c9n_toker const * ct,
                                  c9n_token const * tok ){
  c9n_token const * tt;
  if(!tok) tok = c9n_toker_token(ct);
  tt = tok ? c9n_toker_at(ct, tok->innerId) : 0;
  for( ; tt && T10N_T_EOF!=tt->ttype;
       tt = c9n_toker_at(ct, tt->nextId) ){
    switch(tt->ttype){
      case T10N_T_NL:
      case T10N_T_EOL:
        break;
      default:
        if(!t10n_ttype_is_junk(tt->ttype)){
          return tt->ttype;
        }
        break;
    }
  }
  return 0;
}

int c9n_token_is_eof( c9n_token const * tok ){
  return tok && tok->ttype==T10N_T_EOF ? tok->ttype : 0;
}


#undef MARKER
#undef NO_ID
#undef CT_EOF_ID
#undef c9n__dump_cptoken
#undef cwal_engine_adjust_client_mem
