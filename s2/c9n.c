/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "s2_c9n.h"
#include "t10n.h"
#include "s2_internal.h" /* s2_slurp_heredoc() and friends */

#if 1
#include <stdio.h>
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:\t",__FILE__,__LINE__); if(1) printf pfexp
#define s2__dump_cptoken(lbl,CT,TOK) MARKER(("%s: token type %d/%s @ %d,%d: %.*s\n", lbl, (TOK)->ttype, s2_ttype_cstr((TOK)->ttype), \
                                             (TOK)->line, (TOK)->column, \
                                             (int)((TOK)->length), (CT)->begin +(TOK)->begin))
#else
#define MARKER(pfexp) (void)0
#define s2__dump_cptoken(lbl,CT,TOK) (void)0
#endif

/**
   Still on the fence about this... If we report s2_cptoker allocs via
   this mechanism then we end up effectively duplicating the metrics.
   If we don't, the cwal-level metrics do not report this memory.
*/
#define cwal_engine_adjust_client_mem(X,Y) (void)0

#define NO_ID s2_token_id_none
/**
   FIXME: CT_EOF_ID evaluates to the ID of the EOF token in the parent
   chain. It "should" evaluate to the EOF token of the inner chain, if
   appropriate.
*/
#define CT_EOF_ID(CT) ((s2_token_id)((CT)->chainLength))
#if 0
const s2_byte_range s2_byte_range_empty = s2_byte_range_empty_m;
#endif
const s2_cptoker s2_cptoker_empty = s2_cptoker_empty_m;
const s2_cptoken s2_cptoken_empty = s2_cptoken_empty_m;


/**
   Returns the top-most s2_cptoker in ct's hierarchy, following
   ->parent. May return ct.
*/
s2_cptoker const * s2_cptoker_top( s2_cptoker const * ct ){
  return ct->top ? ct->top : ct;
}

s2_linecol_t s2_cptoken_len( s2_cptoken const * k, int inner ){
  return inner && k->innerOffset ? k->innerLength : k->length;
}

char const * s2_cptoken_cstr( s2_cptoker const * ct,
                              s2_cptoken const * k, cwal_size_t * len ){
  if(len){
    *len = (cwal_size_t)(k->length);
  }
  return k->length ? (s2_cptoker_top(ct)->begin + k->begin) : "";
}

char const * s2_cptoken_cstr2( s2_cptoker const * ct,
                               s2_cptoken const * k, cwal_size_t * len ){
  char const * tBegin = s2_cptoker_top(ct)->begin;
  if(k->innerOffset){
    if(len) *len = (cwal_size_t)(k->innerLength);
    return k->innerLength ? (tBegin + k->begin + k->innerOffset) : "";
  }else{
    if(len) *len = (cwal_size_t)(k->length);
    return k->length ? (tBegin + k->begin) : "";
  }
}

char const * s2_cptoker_name_first( s2_cptoker const * t, cwal_size_t * len ){
  while(t && !t->name) t = t->parent;
  if(t && t->name && len){
    *len = cwal_strlen(t->name);
  }
  return t->name;
}


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
static s2_cptoken const * s2_cptoker_token_near( s2_cptoker const * ct,
                                                 char const * pos ){
  s2_cptoken const * rc = 0;
  ct = s2_cptoker_top( ct );
  if(pos >= ct->begin && pos<ct->end){
    s2_cptoken const * tok = 0;
    s2_cptoken const * prev = 0;
    s2_token_id ndx;
    char const * tbeg;
    /* TODO: binary search rather than linear. */
    for( ndx = 0; ndx < (s2_token_id)ct->chainLength;
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

#if 0
static s2_cptoken const * s2_cptoker_eof_token(s2_cptoker const * ct){
  assert(CT_EOF_ID(ct) >= 0);
  return &ct->chain[CT_EOF_ID(ct)];
}
#endif

static s2_cptoken const * s2_cptoker_err_pos( s2_cptoker const * ct ){
  s2_cptoken const * rc = 0;
  if(ct->chain){
    rc = s2_cptoker_at( ct, ct->errToken );
    if(!rc){
      rc = s2_cptoker_at( ct, ct->token );
      if(!rc){
        rc = s2_cptoker_at( ct, ct->pbToken );
        if(!rc) rc = &ct->chain[0];
      }
    }
  }
  return rc;
}

/**
   Internal helper to fetch ct's line/column. If ct->se->opErrPos is
   set then it tries to select a token near that position, else it
   uses ct->errToken or ct->token (in that order). It accounts for
   ct->lineOffset and ct->colOffset, using the first one of those
   which is set in ct or one of its parents.
*/
static void s2_cptoker_linecol( s2_cptoker const * ct,
                                s2_linecol_t * line,
                                s2_linecol_t * col ){
  s2_cptoken const * errPos =
    ct->se->opErrPos
    ? s2_cptoker_token_near(ct, ct->se->opErrPos)
    : 0;
  if(!errPos) errPos = s2_cptoker_err_pos(ct);
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
       to set it up properly. This algo follows what s2_ptoker has
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

static int s2_err_cptoker_impl(char throwIt,
                               s2_cptoker const * ct,
                               int code, char const * fmt,
                               va_list vargs ){
  s2_engine * se = ct->se;
  int rc = 0;
  char const * name;
  cwal_size_t nameLen = 0;
  cwal_buffer * obuf = &se->e->err.msg;
  s2_linecol_t line = 0, column = 0;
  s2_cptoker_linecol(ct, &line, &column);
  s2_engine_err_reset(se);
  se->e->err.code = code ? code : (throwIt ? CWAL_RC_EXCEPTION : CWAL_SCR_SYNTAX);
  name = s2_cptoker_name_first(ct, &nameLen);
  if(CWAL_RC_OOM==code){
    rc = CWAL_RC_OOM;
  }
  else if(!throwIt && line) {
    /*    ^^^^^^^^ if it will be thrown, the name/line/col get
          injected differently */
    char const * tailPart = ": ";
    if(nameLen){
      rc = cwal_buffer_printf( se->e, obuf, "%.*s:",
                               (int)nameLen, name );
    }
    if(!rc){
      if(nameLen){
        rc = cwal_buffer_printf( se->e, obuf, "%d:%d%s",
                                 line, column,
                                 tailPart);
      }else{
        rc = cwal_buffer_printf( se->e, obuf, "line %d, col %d%s",
                                 line, column,
                                 tailPart);
      }
    }
  }

  if(!rc){
    if(fmt && *fmt){
      rc = cwal_buffer_printfv(se->e, obuf, fmt, vargs);
    }else{
      rc = cwal_buffer_printf(se->e, obuf,
                              "Error #%d (%s)%s%s",
                              code, cwal_rc_cstr(code),
                              (ct->errMsg ? ": " : ""),
                              ct->errMsg ? ct->errMsg : "");
    }
  }
  se->err.line = line;
  se->err.col = column;
  se->err.script.used = 0;
  if(!rc && name){
    rc = cwal_buffer_append( se->e, &se->err.script, name, nameLen );
  }
  if(!rc && throwIt){
    rc = s2_throw_err(se, &se->err, name, line, column);
  }
  return rc ? rc : code;
}

int s2_err_cptoker( s2_cptoker const * ct, int code, char const * fmt, ... ){
  int rc;
  va_list args;
  va_start(args,fmt);
  rc = s2_err_cptoker_impl(0, ct, code, fmt, args);
  va_end(args);
  return rc;
}

int s2_throw_cptoker( s2_cptoker const * ct, int code, char const * fmt, ... ){
  int rc;
  va_list args;
  va_start(args,fmt);
  rc = s2_err_cptoker_impl(1, ct, code ? code : CWAL_RC_EXCEPTION,
                           fmt, args);
  va_end(args);
  return rc;
}



/**
   Copies the state from p to c. ct must be the tokenizer in charge of
   both p and c.
*/
static int s2_ptoken_to_cptoken( s2_cptoker * const ct,
                                 s2_ptoken const * const p,
                                 s2_cptoken * const c ){
  static uint32_t const maxLCval = (s2_linecol_t)-1;
  assert(s2_ptoken_begin(p) >= ct->begin && s2_ptoken_end(p)<=ct->end);
#define ERR_RNG(M) ct->errMsg=M; return CWAL_RC_RANGE
  c->ttype = p->ttype;
  if(c->line>maxLCval){ERR_RNG("Line counter overflow.");}
  c->line = (s2_linecol_t)p->line;
  if(c->column>maxLCval){ERR_RNG("Column counter overflow.");}
  c->column = (s2_linecol_t)p->column;
  c->begin = (uint32_t)(s2_ptoken_begin(p) - ct->begin);
  if((uint32_t)s2_ptoken_len(p)>maxLCval){
    ERR_RNG("Single token exceeds size limit.");
  }
  c->length = (s2_linecol_t)s2_ptoken_len(p);
  if(s2_ptoken_adjbegin(p)){
    c->innerOffset = (s2_linecol_t)(s2_ptoken_adjbegin(p)
                                    - s2_ptoken_begin(p));
    assert(s2_ptoken_adjend(p) && "adjEnd must be set if adjBegin is!");
    c->innerLength = (s2_linecol_t)(s2_ptoken_adjend(p)
                                    - s2_ptoken_adjbegin(p));
  }else{
    c->innerLength = c->length;
  }
#undef ERR_RNG
  return 0;
}

/**
   UNTESTED.
*/
void s2_cptoken_to_ptoken( s2_cptoker * ct, s2_cptoken const * src,
                           s2_ptoken * dest ){
  dest->id = src->id;
  dest->ttype = src->ttype;
  s2_ptoken_begin_set(dest, ct->begin + src->begin);
  s2_ptoken_end_set(dest, ct->begin + src->begin + src->length);
  dest->line = src->line;
  dest->column = src->column;
  if(src->innerOffset){
    s2_ptoken_adjbegin_set(dest, ct->begin + src->begin + src->innerOffset);
    s2_ptoken_adjend_set(dest, s2_ptoken_adjbegin(dest) + src->innerLength);
  }
}

/**
   Internal helper to fetch the next token from pt. Used both for
   counting the tokens (to figure out how much memory to allocate) and
   then for compilation. Errors are reported via ct->se.

   If n is not NULL, it is incremented on success. Returns 0 on
   success and updates ct->se's error state on error.
*/
static int s2_cptoker_next_for_compile( s2_cptoker * ct, s2_ptoker * pt,
                                        uint32_t flags, uint32_t * n ){
  int rc = 0;
  int prevTokType = pt->token.ttype;
  while(1){
    rc = s2_ptoker_next_token(pt);
    if(rc) break;
    else if(s2_ttype_is_junk(pt->token.ttype)){
      if(!(S2_CPTOKER_F_RETAIN_JUNK & flags)){
        prevTokType = pt->token.ttype/* so we can catch EOLs after comments */;
        continue;
      }
    }
    switch(pt->token.ttype){
      case S2_T_HeredocStart:
        rc = s2_slurp_heredoc(ct->se, pt, 0);
        break;
#if 0
      case S2_T_Semicolon:
        pt->token.ttype = S2_T_EOX;
        break;
#endif
      case S2_T_NL:
        pt->token.ttype = S2_T_EOL;
        CWAL_SWITCH_FALL_THROUGH;
      case S2_T_EOL:
        switch(prevTokType){
          case S2_T_SquigglyClose:
          case S2_T_CommentCpp:
          case S2_T_CommentC:
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
        }
        /* s2_next_token() does this translation and s2_eval.c currently
           relies on it happening there:
           pt->token.ttype = S2_T_EOL;
        */
        break;
      default:
        break;
    }
    if(n) ++*n;
    break;
  }
  if(rc && !ct->se->err.code){
    rc = s2_err_ptoker(ct->se, pt, rc, 0);
  }
  return rc;
}

static s2_token_id s2_cptoker_t_set( s2_cptoker * ct,
                                     s2_cptoken const * tok,
                                     s2_token_id * member ){
  s2_cptoker const * top = s2_cptoker_top(ct);
  if(tok && tok->id>0 && tok->id<=(s2_token_id)top->chainLength){
    return *member = tok->id;
  }else{
    return 0;
  }
}

int s2_cptoker_token_set( s2_cptoker * ct, s2_cptoken const * tok ){
    return s2_cptoker_t_set(ct, tok, &ct->token);
}

int s2_cptoker_errtoken_set( s2_cptoker * ct, s2_cptoken const * tok ){
  return s2_cptoker_t_set(ct, tok, &ct->errToken);
}

int s2_cptoker_pb_set( s2_cptoker * ct, s2_cptoken const * tok ){
    return s2_cptoker_t_set(ct, tok, &ct->pbToken);
}

int s2_cptoker_next_set( s2_cptoker * ct, s2_cptoken const * tok ){
    return s2_cptoker_t_set(ct, tok, &ct->nextToken);
}


#if 0
/**
   Tokenizes pt from its current position until the end and returns
   a count of tokens via *n. Returns 0 on success. Errors are reported
   via ct->se and the non-0 error code is returned.
*/
static int s2_cptoker_count_tokens( s2_cptoker * ct, s2_ptoker * pt,
                                    uint32_t flags, uint32_t * n ){
  int rc = 0;
  while( !(rc = s2_cptoker_next_for_compile(ct, pt, flags, n) ) ){
    /*MARKER(("token #%u: %s\n", c, s2_ttype_cstr(pt->token.ttype)));*/
    if(S2_T_EOF==pt->token.ttype) break;
  }
  return rc;
}
#endif

/**
   Internal non-const version of s2_cptoker_at().
*/
static s2_cptoken * s2_cptoker_at_nc(s2_cptoker const * ct, s2_token_id id){
  s2_cptoker const * top = s2_cptoker_top(ct);
  return top->chainLength && id>0 && id<=(s2_token_id)top->chainLength
    ? top->chain+id-1 : NULL;
}

s2_cptoken const * s2_cptoker_at(s2_cptoker const * ct,
                                 s2_token_id id){
  return s2_cptoker_at_nc(ct, id);
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
static s2_cptoken * s2_cptoker_chain_reserve( s2_cptoker * ct, uint32_t n ){
  if(!n){
    if(ct->chain){
      assert(ct->chainCapacity && "Trying to free an unowned chain?");
      cwal_free2(ct->se->e, ct->chain, sizeof(s2_cptoken) * ct->chainCapacity);
    }
    cwal_engine_adjust_client_mem(ct->se->e,
                                  -(cwal_int_t)(ct->chainCapacity * sizeof(s2_cptoken)));
    ct->chain = 0;
    ct->chainLength = ct->chainCapacity = 0;
    return 0;
  }
  if(ct->chainCapacity < n){
    s2_cptoken * chain = (s2_cptoken *)cwal_realloc(ct->se->e, ct->chain, n * sizeof(s2_cptoken));
    if(!chain){
      ct->errMsg = "Out of memory - cannot allocate token chain.";
      return 0;
    }
    ct->se->metrics.cptokerMemory -= ct->chainCapacity * sizeof(s2_cptoken);
    ct->se->metrics.cptokerMemory += n * sizeof(s2_cptoken);
    if(ct->chainCapacity){
      cwal_engine_adjust_client_mem(ct->se->e,
                                    -((cwal_int_t)(ct->chainCapacity * sizeof(s2_cptoken))));
    }
    cwal_engine_adjust_client_mem(ct->se->e, (cwal_int_t)(n * sizeof(s2_cptoken)));
    ++ct->se->metrics.cptokerAllocs;
    ct->chainCapacity = n;
    ct->chain = chain;
  }
  return ct->chain;
}


/**
   Expects its argument to be one of:

   S2_T_SquigglyOpen, S2_T_SquigglyBlock,
   S2_T_ParenOpen, S2_T_ParenGroup,
   S2_T_BraceOpen, S2_T_BraceGroup:

   and asserts if passed any other type.

   Returns the matching group token type.
*/
static enum s2_token_types s2_ttype_group_type(int16_t ttype){
  switch(ttype){
    case S2_T_SquigglyOpen:
    case S2_T_SquigglyBlock:
      return S2_T_SquigglyBlock;
    case S2_T_ParenOpen:
    case S2_T_ParenGroup:
      return S2_T_ParenGroup;
      break;
    case S2_T_BraceOpen:
    case S2_T_BraceGroup:
      return S2_T_BraceGroup;
    default:
      assert(!"internal misuse");
      return S2_T_INVALID;
  }
}

/**
   Expects its argument to be one of:

   S2_T_SquigglyOpen, S2_T_ParenOpen, S2_T_BraceOpen:

   and asserts if passed any other type.

   Returns the matching closing token type.
*/
static enum s2_token_types s2_ttype_closer_type(int16_t ttype){
  switch(ttype){
    case S2_T_SquigglyOpen:
      return S2_T_SquigglyClose;
    case S2_T_ParenOpen:
      return S2_T_ParenClose;
      break;
    case S2_T_BraceOpen:
      return S2_T_BraceClose;
    default:
      assert(!"internal misuse");
      return S2_T_INVALID;
  }
}

/**
   Requires tok->ttype to be one of:

   S2_T_SquigglyOpen, S2_T_ParenOpen, S2_T_BraceOpen:

   This routine searches ct->chain for a matching closing element. If
   found, *ender is assigned to that token and 0 is returned, else an
   error is reported via ct->se and that error's code is returned.
*/
static int s2_cptoker_find_closer( s2_cptoker * ct, s2_cptoken const * tok,
                                   s2_cptoken ** ender ){
  enum s2_token_types const closer = s2_ttype_closer_type(tok->ttype);
  s2_cptoken * at = 0;
  unsigned int level = 0;
  assert(tok->ttype==S2_T_SquigglyOpen
         || tok->ttype==S2_T_ParenOpen
         || tok->ttype==S2_T_BraceOpen);
  while(1){
    at = s2_cptoker_at_nc(ct, at ? at->nextId : tok->nextId);
    switch(at->ttype){
      case 0:
        assert(!"should not be able to happen at this level");
        CWAL_SWITCH_FALL_THROUGH;
      case S2_T_EOF:
        goto mismatch;
      case S2_T_SquigglyOpen:
      case S2_T_ParenOpen:
      case S2_T_BraceOpen:
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
  return s2_err_cptoker( ct, CWAL_SCR_SYNTAX,
                         "Mismatched '%c' starting at line %d column %d.",
                         (char)tok->ttype, tok->line, tok->column);
}

/**
   Walks through the given token chain and may make "certain
   modifications" to it:

   - Grouping tokens (), [], {}, are transformed such that:

   1) the left-most token gets converted from (e.g.) S2_T_ParenOpen to
   (e.g.) S2_T_ParenGroup.

   2) The left-most token->innerId gets set to the ID of the first token
   "inside" the group.

   3) The left-most token->nextId gets set to the token *after* the group's
   closing paren/brace/squiggly token.

   4) The group-closing token gets transformed to an EOF token, so
   that traversing that section of the chain will inherently end when
   it is encountered (without having to know what type of group it is).

   e.g. a token chain of:

   a ( b c d ) e<EOF>

   gets transformed to:

   a e<EOF>
    \
     b c d<EOF>

   Recursively, of course.

   Returns 0 on success. Since the input chain has already been fully
   tokenized, the only error cases this routine will encounter are
   mismatched grouping tokens.
*/
static int s2_cptoker_post_compile_chain( s2_cptoker * ct, s2_cptoken * chain ){
  s2_cptoken * tok = chain;
  int rc = 0;
  assert(tok && "Internal misuse.");
  while(1){
    switch(tok->ttype){
      case S2_T_SquigglyOpen:
      case S2_T_ParenOpen:
      case S2_T_BraceOpen:{
        s2_cptoken * closer = 0;
        s2_cptoken * inner = 0;
        rc = s2_cptoker_find_closer(ct, tok, &closer);
        if(rc) break;
        assert(closer);
        if(closer==s2_cptoker_at(ct, tok->nextId)){
          /* Empty block. */
          /* closer (tok->nextId) will get transformed into an EOF
             below, so we'll use that token rather than the outermost
             EOF. */
          /*tok->innerId = CT_EOF_ID(ct);*/
          tok->innerId = tok->nextId;
        }else{
          tok->innerId = tok->nextId;
          inner = s2_cptoker_at_nc(ct, tok->innerId);
        }
        /* Extend tok to wrap the whole block... */
        tok->nextId = closer->nextId;
        tok->length = closer->begin - tok->begin + closer->length;
        tok->ttype = s2_ttype_group_type(tok->ttype);
        assert(tok->length >= 2);
        tok->innerOffset = 1;
        tok->innerLength = tok->length - 2;
        /*MARKER(("#%d Block %s: begin=%d, innerLength=%d "
                "body=<<<%.*s>>>\nouter=<<<%.*s>>>\n",
                tok->ndx, s2_ttype_cstr(tok->ttype),
                (int)tok->begin+tok->innerOffset,
                (int)tok->innerLength,
                (int)tok->innerLength,
                ct->begin + tok->begin + tok->innerOffset,
                (int)tok->length,
                ct->begin+tok->begin));*/
        /* Convert the block's closing token into a virtual EOF for
           the inner part. Algorithms which climb down branches of
           this script rely on the existince of such a token. */
        closer->ttype = S2_T_EOF;
        closer->nextId = 0 /* NO_ID */;
        closer->length = 1/* Group-closing character. We can hypothetically
                             use this to distinguish the real EOF from virtual
                             ones. */;
        if(inner){
          rc = s2_cptoker_post_compile_chain( ct, inner );
        }
        break;
      }
      case S2_T_EOF:
        break;
      default:
        break;
    }
    if(rc || S2_T_EOF==tok->ttype) break;
    tok = s2_cptoker_at_nc(ct, tok->nextId);
  }
  return rc;
}

s2_cptoker * s2_cptoker_alloc( s2_engine * se ){
  s2_cptoker * cp = (s2_cptoker *)cwal_malloc2(se->e, sizeof(s2_cptoker));
  if(cp){
    *cp = s2_cptoker_empty;
    se->metrics.cptokerMemory += sizeof(s2_cptoker);
    ++se->metrics.cptokerAllocs;
    cwal_engine_adjust_client_mem(se->e, (cwal_int_t)sizeof(s2_cptoker));
    cp->se = se;
    cp->allocStamp = &s2_cptoker_empty;
  }
  return cp;
}

void s2_cptoker_free( s2_cptoker * cp ){
  assert(cp ? !!cp->se : 1);
  if(cp){
    cwal_engine * const e = cp->se->e;
    void const * const allocStamp = cp->allocStamp;
    s2_cptoker_finalize(cp);
    if(&s2_cptoker_empty == allocStamp){
      cwal_engine_adjust_client_mem(e, -(cwal_int_t)sizeof(s2_cptoker));
      cwal_free2(e, cp, sizeof(s2_cptoker));
    }
  }
}

int s2_cptoker_init( s2_engine * se, s2_cptoker * ct ){
  void const * const allocStamp = ct->allocStamp;
  if(&s2_cptoker_empty == ct->allocStamp){
    assert(se && ct->se==se);
  }
  *ct = s2_cptoker_empty;
  ct->se = se;
  ct->allocStamp = allocStamp;
  return 0;
}


void s2_cptoker_reset( s2_cptoker * ct ){
  ct->token = ct->pbToken = ct->errToken = NO_ID;
  if(ct->chainLength){
    ct->nextToken = ct->chain[0].id;
  }else{
    ct->nextToken = 0;
  }
  /*ct->capture = s2_byte_range_empty;*/
}



int s2_cptoker_sub_from_group( s2_cptoker const * parent,
                               s2_cptoker * ct ){
  s2_cptoker const * top = s2_cptoker_top( parent );
  s2_cptoken * chain = s2_cptoker_at_nc(parent, parent->token);
  if(!chain) return CWAL_RC_MISUSE;
  else if(!s2_ttype_group_type(chain->ttype)){
    return CWAL_RC_TYPE;
  }
  assert(chain->innerId);
  ct->top = top;
  ct->parent = parent;
  ct->chain = s2_cptoker_at_nc(parent, chain->innerId);
  assert(ct->chain);
  ct->se = parent->se;
  s2_cptoker_reset(ct);
  ct->lineOffset = chain->line;
  ct->colOffset = chain->column;
  ct->chainLength = ct->chainCapacity = 0 /*tag that ct does not own ct->chain*/;
  ct->begin = top->begin + chain->begin;
  ct->end = ct->begin + chain->length;
  ct->name = parent->name;
  assert(chain);
  return 0;
}


int s2_cptoker_compile_buffer( s2_cptoker * ct, cwal_buffer const * buf,
                               uint32_t compileFlags ){
  return s2_cptoker_compile(ct, buf->mem ? (char const *)buf->mem : "",
                            buf->mem ? (cwal_int_t)buf->used : 0,
                            compileFlags);
}

int s2_cptoker_compile( s2_cptoker * ct, char const * src,
                        cwal_int_t len, uint32_t flags ){
  s2_ptoker pt = s2_ptoker_empty;
  int rc = s2_ptoker_init_v2( ct->se->e, &pt, src, len, 0 );
  if(!rc){
    rc = s2_cptoker_compile_ptoker( ct, &pt, flags );
  }
  s2_ptoker_finalize( &pt );
  return rc;
}

/**
   Returns a guesstimated number of tokens which are remaining in the
   given tokenizer. This is a dumb heuristic based solely on the
   "remaining" size of the tokenizer, counting from the start of its
   current token to the end of the tokenizer, plus 1 (to account for
   an EOF token).
*/
static uint32_t s2_cptoker_estimate_token_count(s2_ptoker const * pt){
  if(s2_ptoker_is_eof(pt)) return 1;
  else{
    const char * begin = s2_ptoken_begin(&pt->token)
      ? s2_ptoken_begin(&pt->token)
      : s2_ptoker_begin(pt);
    const char * end = s2_ptoker_end(pt);
    return (((uint32_t)(end - begin))*55/10) / sizeof(s2_cptoken) + 1;
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
static void s2_cptoker_chain_trim( s2_cptoker * ct ){
  if(ct->chainCapacity >= 20/*arbitrary*/
     && ct->chainCapacity > ct->chainLength * 12/10){
    void * re = cwal_realloc(ct->se->e, ct->chain, ct->chainLength * sizeof(s2_cptoken));
    if(re){
    ++ct->se->metrics.cptokerAllocs;
      ct->se->metrics.cptokerMemory -= ct->chainCapacity * sizeof(s2_cptoken);
      cwal_engine_adjust_client_mem(ct->se->e,
                                    -((cwal_int_t)(ct->chainCapacity * sizeof(s2_cptoken))));
      ct->chain = (s2_cptoken*)re;
      ct->chainCapacity = ct->chainLength;
      ct->se->metrics.cptokerMemory += ct->chainCapacity * sizeof(s2_cptoken);
      cwal_engine_adjust_client_mem(ct->se->e,
                                    ((cwal_int_t)(ct->chainCapacity * sizeof(s2_cptoken))));
    }
    /* Ignore realloc error - it's not fatal here. */
  }
}

int s2_cptoker_compile_ptoker( s2_cptoker * ct, s2_ptoker const * psrc, uint32_t flags ){
  int rc = 0;
  s2_token_id ndx = NO_ID;
  s2_ptoker pt = *psrc;
  assert(ct->se && "ct has not been s2_cptoker_init()'d");
  assert(!psrc->compiled);
  if(ct->parent){
    return s2_engine_err_set(ct->se, CWAL_RC_MISUSE,
                             "Cannot compile a sub-tokenizer.");
  }
  ct->chainLength = 0 /* re-use any chain we currently have */;
  s2_cptoker_reset(ct);
  ct->begin = s2_ptoker_begin(&pt);
  ct->end = s2_ptoker_end(&pt);
  if(!s2_cptoker_chain_reserve(ct, s2_cptoker_estimate_token_count(&pt))){
    return CWAL_RC_OOM;
  }
  if(pt.name){
    ct->name = s2_ptoker_name_first(psrc, 0);
  }
  s2_ptoker_reset(&pt);
  if(S2_CPTOKER_F_IDENTIFIER_DASHES & flags){
    pt.flags |= S2_T10N_F_IDENTIFIER_DASHES;
  }else{
    pt.flags &= ~S2_T10N_F_IDENTIFIER_DASHES;
  }
  pt.flags |= S2_T10N_F_LINEAR_TOKER;
  assert(ct->chainCapacity>0);
  for(ndx = 0; 0==(rc = s2_cptoker_next_for_compile(ct, &pt, flags, 0));
      ++ndx){
    s2_cptoken * ctok;
    if(ct->chainCapacity < ndx+1){
      uint32_t const newCap = ct->chainCapacity
        + s2_cptoker_estimate_token_count(&pt);
      if(!s2_cptoker_chain_reserve(ct, newCap)){
        rc = CWAL_RC_OOM;
        goto err;
      }
    }
    ++ct->chainLength;
    ctok = ct->chain + ndx;
    *ctok = s2_cptoken_empty;
    ctok->id = ndx+1;
    if(ndx){
      ct->chain[ndx-1].nextId = ctok->id;
    }
    rc = s2_ptoken_to_cptoken(ct, &pt.token, ctok);
    if(rc) goto err;
#if 0
    {
      cwal_size_t tlen = 0;
      char const * tstr = s2_cptoken_cstr(ct, ctok, &tlen);
      MARKER(("Token #%d: %s @ %d, %d: %.*s\n",
              ctok->id, s2_ttype_cstr(ctok->ttype),
              ctok->line, ctok->column,
              (int)tlen, tstr));
    }
#endif
    if(s2_ptoker_is_eof(&pt)) break;
  }
  if(rc) goto end;
  assert(s2_ptoker_is_eof(&pt));
  assert(S2_T_EOF==ct->chain[ndx].ttype);
  rc = s2_cptoker_post_compile_chain(ct, ct->chain);
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
    s2_cptoken const * et = s2_cptoker_at(ct, ndx+1);
    ct->errToken = et ? et->id : NO_ID;
  }
  if(!ct->se->err.code){
    rc = s2_err_cptoker( ct, rc, 0 );
  }
  end:
  if(!rc){
    s2_cptoker_chain_trim(ct);
  }
  ct->name = 0;
  s2_cptoker_reset(ct);
  return rc;
}

void s2_cptoker_finalize( s2_cptoker * ct ){
  if(ct->se){
    /* If ct->top is not 0 then any ct->chain component belongs to a
       parent tokenizer (or someone else, hypothetically). */
    if(!ct->top){
      s2_cptoker_chain_reserve(ct, 0);
    }
  }
  *ct = s2_cptoker_empty;
}

s2_cptoken const * s2_cptoker_token(s2_cptoker const * ct){
  return s2_cptoker_at(ct, ct->token);
}

s2_token_id s2_cptoker_putback(s2_cptoker * ct){
  ct->token = ct->pbToken;
  ct->pbToken = NO_ID;
  return ct->token;
}

int s2_cptoker_next_token( s2_cptoker * ct, s2_cptoken const ** tgt ){
  s2_token_id id = NO_ID;
  s2_cptoken const * ctok = 0;
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
    ctok = s2_cptoker_at(ct, ct->token);
    assert(ctok);
    if(S2_T_EOF==ctok->ttype){
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
  if(tgt) *tgt = s2_cptoker_at(ct, id);
  ct->pbToken = ct->token;
  ct->token = id;
  return 0;
}

int s2_cptoken_block_has_content( s2_cptoker const * ct,
                                  s2_cptoken const * tok ){
  s2_cptoken const * tt;
  if(!tok) tok = s2_cptoker_token(ct);
  tt = tok ? s2_cptoker_at(ct, tok->innerId) : 0;
  for( ; tt && S2_T_EOF!=tt->ttype;
       tt = s2_cptoker_at(ct, tt->nextId) ){
    switch(tt->ttype){
      case S2_T_NL:
      case S2_T_EOL:
        break;
      default:
        if(!s2_ttype_is_junk(tt->ttype)){
          return tt->ttype;
        }
        break;
    }
  }
  return 0;
}

int s2_cptoken_is_eof( s2_cptoken const * tok ){
  return tok && tok->ttype==S2_T_EOF ? tok->ttype : 0;
}

#undef MARKER
#undef NO_ID
#undef CT_EOF_ID
#undef s2__dump_cptoken
#undef cwal_engine_adjust_client_mem
