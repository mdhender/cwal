/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

#include "internal.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#if 1
#include <stdio.h>
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:\t",__FILE__,__LINE__); if(1) printf pfexp
#else
#define MARKER(pfexp) (void)0
#endif

/**
   Still on the fence about this... If we report whcl_script allocs via
   this mechanism then we end up effectively duplicating the metrics.
   If we don't, the cwal-level metrics do not report this memory.
*/
#define cwal_engine_adjust_client_mem(X,Y) (void)0


/** @internal
   Functions like whcl__script_compile(), but takes its input range from
   an existing tok1_izer instance.

   It uses psrc->name for error reporting, when appropriate (which is
   the primary advantage over using whcl__script_compile()). Before
   returning, ct->name is cleared, so the caller must re-assign it if
   it will be needed. (This routine can't do it because it doesn't
   know the lifetime of src->name.)

   This does not associate psrc with ct. It sets up ct to use the same
   byte range used by psrc and tokenizes that whole byte range into a
   chain of parsed tokens, ready for evaluation. After this call,
   psrc is no longer needed by ct but the script bytes it points to
   are.
*/
static int whcl__script_compile_toker( whcl_script * const ct,
                                       tok1_izer const * const psrc,
                                       uint32_t flags,
                                       whcl_stoken_izer_f tp, void * tpState );


#define NO_ID whcl_stoken_id_none
const whcl_script whcl__script_empty = whcl__script_empty_m;
const whcl_stoken whcl_stoken__empty = whcl_stoken__empty_m;

#define whcl___script_top(T) (T->top ? T->top : T)
/**
   The ID of a whcl_script::chain's EOF token.
*/
#define whcl___script_eof_id(CT) ((whcl_stoken_id)(whcl__script_top(CT)->chainLength))
/**
   Returns the top-most whcl_script in ct's hierarchy, following
   ->parent. May return ct.
*/
whcl_script const * whcl__script_top( whcl_script const * ct ){
  return whcl___script_top(ct);
}

whcl_linecol_t whcl_stoken_len( whcl_stoken const * const k ){
  return k->length;
}

whcl_linecol_t whcl_stoken_len2( whcl_stoken const * const k, bool inner ){
  return inner && k->innerOffset ? k->innerLength : k->length;
}

char const * whcl__script_ttype_cstr( whcl_stoken const * const t ){
  char const * z = t->ttype2 ? tok1_t_cstr(t->ttype2) : NULL;
  return z ? z : tok1_t_cstr(t->ttype);
}

char const * whcl_stoken_cstr( whcl_script const * const ct,
                               whcl_stoken const * const k,
                               cwal_midsize_t * const len,
                               bool innerOnly ){
  char const * tBegin = whcl__script_top(ct)->begin;
  if(innerOnly && k->innerOffset){
    if(len) *len = (cwal_midsize_t)(k->innerLength);
    return k->innerLength ? (tBegin + k->begin + k->innerOffset) : "";
  }
  if(len) *len = (cwal_midsize_t)(k->length);
  return k->length ? (tBegin + k->begin) : "";
}

char const * whcl_script_name_get( whcl_script const * t,
                                   cwal_midsize_t * const len ){
  while(t && !t->name) t = t->parent;
  if(t && t->name && len){
    *len = (cwal_midsize_t)cwal_strlen(t->name);
  }
  return t ? t->name : NULL;
}

static inline void c9__err_reset(whcl_script * const ct){
  cwal_error_reset(cwal_engine_error(ct->ec));
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
static whcl_stoken const * whcl__script_token_near( whcl_script const * ct,
                                               char const * pos ){
  whcl_stoken const * rc = 0;
  ct = whcl__script_top( ct );
  if(pos >= ct->begin && pos<ct->end){
    whcl_stoken const * tok = 0;
    whcl_stoken const * prev = 0;
    whcl_stoken_id ndx;
    char const * tbeg;
    /* TODO: binary search rather than linear. */
    for( ndx = 0; ndx < (whcl_stoken_id)ct->chainLength;
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
#endif /* whcl__script_token_near() */

#if 0
static whcl_stoken const * whcl__script_eof_token(whcl_script const * ct){
  assert(whcl__script_eof_id(ct) > 0);
  return &whcl__script_top(ct)[whcl__script_eof_id(ct)-1];
}
#endif
whcl_stoken_id whcl__script_eof_id(whcl_script const * const ct ){
  return whcl___script_eof_id(ct);
}

whcl_stoken const * whcl__script_err_pos( whcl_script const * ct ){
  whcl_stoken const * rc = 0;
  if(ct->chain){
    rc = whcl__script_at( ct, ct->errToken );
    if(!rc){
      rc = whcl__script_at( ct, ct->token );
      if(!rc){
        rc = whcl__script_at( ct, ct->pbToken );
        if(!rc) rc = &ct->chain[0];
      }
    }
  }
  return rc;
}

/**
   Internal helper to fetch ct's line/column. It uses the position of
   ct->errToken or ct->token (in that order).
*/
static inline void whcl__script_linecol( whcl_script const * ct,
                                         whcl_linecol_t * line,
                                         whcl_linecol_t * col ){
  whcl_stoken const * errPos = whcl__script_err_pos(ct);
  *line = errPos->line;
  *col = errPos->column;
  //whcl__dump_stok(ct, errPos, "errPos?");
}

static int whcl__script_err_impl(bool throwIt,
                             whcl_script const * const ct,
                             int code, char const * fmt,
                             va_list vargs ){
  cwal_engine * const e = ct->ec;
  int rc = 0;
  char const * name;
  cwal_midsize_t nameLen = 0;
  cwal_error * const err = cwal_engine_error(ct->ec);
  cwal_buffer * const obuf = &err->msg;
  whcl_linecol_t line = 0, column = 0;
  whcl__script_linecol(ct, &line, &column);
  cwal_engine_error_reset(e);
  err->code = code ? code : (throwIt ? CWAL_RC_EXCEPTION : CWAL_SCR_SYNTAX);
  name = whcl_script_name_get(ct, &nameLen);
  if(CWAL_RC_OOM==code){
    rc = CWAL_RC_OOM;
  }else if(!throwIt && line) {
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
                                 (int)line, (int)column,
                                 tailPart);
      }else{
        rc = cwal_buffer_printf( e, obuf, "line %d, col %d%s",
                                 (int)line, (int)column,
                                 tailPart);
      }
    }
  }
  if(!rc){
    if(fmt && *fmt){
      rc = cwal_buffer_printfv(e, obuf, fmt, vargs);
    }else{
      char const *errMsg = whcl__script_err_msg(ct, NULL);
      rc = cwal_buffer_printf(e, obuf,
                              "Error #%d (%s)%s%s",
                              code, cwal_rc_cstr(code),
                              (errMsg ? ": " : ""),
                              errMsg ? errMsg : "");
    }
  }
  err->line = line;
  err->col = column;
  cwal_buffer_reuse(&err->script);
  if(!rc && name){
    rc = cwal_buffer_append(e, &err->script, name, nameLen);
  }
  if(!rc && throwIt){
    rc = cwal_error_throw(e, err, name, line, column);
  }
  return rc ? rc : code;
}

int whcl__script_errv(whcl_script const * const ct, int code,
                          char const * fmt, va_list args ){
  return whcl__script_err_impl(false, ct, code, fmt, args);
}

int whcl__script_err( whcl_script const * const ct, int code,
                          char const * fmt, ... ){
  int rc;
  va_list args;
  va_start(args,fmt);
  rc = whcl__script_err_impl(false, ct, code, fmt, args);
  va_end(args);
  return rc;
}

int whcl__script_err2(whcl_script * const ct,
                          whcl_stoken const * const tok,
                          int code, char const * fmt, ... ){
    int rc;
  va_list args;
  va_start(args,fmt);
  whcl__script_errtoken_set(ct, tok);
  rc = whcl__script_err_impl(false, ct, code, fmt, args);
  va_end(args);
  return rc;
}

int whcl__script_throwv( whcl_script * const ct, int code, char const * fmt, va_list args ){
  return whcl__script_err_impl(true, ct, code ? code : CWAL_RC_EXCEPTION,
                                   fmt, args);
}

int whcl__script_throw( whcl_script * const ct, int code, char const * fmt, ... ){
  int rc;
  va_list args;
  va_start(args,fmt);
  rc = whcl__script_throwv(ct, code, fmt, args);
  va_end(args);
  return rc;
}

int whcl__script_throw2v(whcl_script * const ct,
                         whcl_stoken const * const tok,
                         int code, char const * fmt,
                         va_list args){
  if(tok) whcl__script_errtoken_set(ct, tok);
  return whcl__script_throwv(ct, code, fmt, args);
}

int whcl__script_throw2(whcl_script * const ct,
                       whcl_stoken const * const tok,
                       int code, char const * fmt, ... ){
  int rc;
  va_list args;
  va_start(args,fmt);
  if(tok) whcl__script_errtoken_set(ct, tok);
  rc = whcl__script_throwv(ct, code, fmt, args);
  va_end(args);
  return rc;
}

int whcl__script_err_get(whcl_script const * const ct, char const **msg,
                 cwal_size_t * const msgLen){
  return cwal_engine_error_get(ct->ec, msg, msgLen);
}

char const * whcl__script_err_msg(whcl_script const * const ct,
                         cwal_size_t * const msgLen){
  cwal_error * const err = cwal_engine_error(ct->ec);
  if(err->code){
    if(msgLen && err->msg.mem){
      *msgLen = err->msg.used;
    }
    return (char const *)err->msg.mem;
  }
  else{
    return NULL;
  }
}

/**
   Copies the state from p to c. ct must be the tokenizer in charge of
   both p and c.
*/
static int tok1_en_to_cptoken( whcl_script * const ct,
                                  tok1_en const * const p,
                                  whcl_stoken * const c ){
  static uint32_t const maxLCval = (whcl_linecol_t)-1;
  assert(tok1_en_begin(p) >= ct->begin && tok1_en_end(p)<=ct->end);
#define ERR_RNG(M) \
  whcl__script_errtoken_set(ct, c);                   \
  return whcl__script_err(ct, CWAL_RC_RANGE, "%s", M)
  c->ttype = p->ttype;
  c->ttype2 = p->ttype2;
  if(c->line>maxLCval){ERR_RNG("Line counter overflow.");}
  c->line = (whcl_linecol_t)p->line;
  if(c->column>maxLCval){ERR_RNG("Column counter overflow.");}
  c->column = (whcl_linecol_t)p->column;
  c->begin = (uint32_t)(tok1_en_begin(p) - ct->begin);
  if((uint32_t)tok1_en_len(p)>maxLCval){
    ERR_RNG("Single token exceeds size limit.");
  }
  c->length = (whcl_linecol_t)tok1_en_len(p);
  if(tok1_en_adjbegin(p)){
    c->innerOffset = (whcl_linecol_t)(tok1_en_adjbegin(p)
                                    - tok1_en_begin(p));
    assert(tok1_en_adjend(p) && "adjEnd must be set if adjBegin is!");
    c->innerLength = (whcl_linecol_t)(tok1_en_adjend(p)
                                     - tok1_en_adjbegin(p));
  }else{
    c->innerLength = c->length;
  }
#undef ERR_RNG
  return 0;
}

void whcl__script_err_reset(whcl_script const * const ct){
  cwal_engine_error_reset(ct->ec);
}


/**
   Internal helper to fetch the next token from pt. Used both for
   counting the tokens (to figure out how much memory to allocate) and
   then for compilation. Errors are reported via ct->ec.

   Returns 0 on success and updates ct's error state on error.

   Flags may be a bitmask of any whcl__script_flags_e. 
*/
static int whcl__script_next_for_compile( whcl_script * const ct, tok1_izer * const pt,
                                          uint32_t flags,
                                          whcl_stoken_izer_f tpf, void * tpfState ){
  int rc = 0;
  int prevTokType = pt->token.ttype;
  cwal_error * const ecErr = cwal_engine_error(ct->ec);
  cwal_error_reset(ecErr);
  while(1){
    rc = tpf(ct, pt, tpfState);
    if(rc) break;
    else if(tok1_t_is_junk(pt->token.ttype)){
      if(!(WHCL__SCRIPT_F_RETAIN_JUNK & flags)){
        prevTokType = pt->token.ttype/* so we can catch EOLs after comments */;
        continue;
      }
    }
    switch(pt->token.ttype){
      case TOK1_T_NL:
        pt->token.ttype = TOK1_T_EOL;
        CWAL_SWITCH_FALL_THROUGH;
      case TOK1_T_EOL:
        switch(prevTokType){
          case TOK1_T_EOL:
            /* Collapse runs of EOLs */
            continue;
          default:
            break;
        }
        break;
      default:
        break;
    }
    break;
  }
  if(rc && !cwal_engine_error(ct->ec)->code){
    assert(pt->errMsg);
    assert(pt->ec);
    rc = tok1_err_toker(pt, rc, "%s", pt->errMsg);
  }
  return rc;
}

static whcl_stoken_id whcl__script_t_set( whcl_script * const ct,
                                     whcl_stoken const * const tok,
                                     whcl_stoken_id * const member ){
  whcl_script const * top = whcl__script_top(ct);
  if(tok && tok->id>0 && tok->id<=(whcl_stoken_id)top->chainLength){
    return *member = tok->id;
  }else{
    return 0;
  }
}

int whcl__stoken_set( whcl_script * const ct, whcl_stoken const * const tok ){
    return whcl__script_t_set(ct, tok, &ct->token);
}

int whcl__script_errtoken_set( whcl_script * const ct,
                            whcl_stoken const * const tok ){
  return whcl__script_t_set(ct, tok, &ct->errToken);
}
whcl_stoken const * whcl__script_errtoken_get( whcl_script const * const ct ){
  return whcl__script_at(ct, ct->errToken);
}

int whcl__script_pb_set( whcl_script * const ct, whcl_stoken const * const tok ){
    return whcl__script_t_set(ct, tok, &ct->pbToken);
}

int whcl__script_next_set( whcl_script * const ct, whcl_stoken const * const tok ){
    return whcl__script_t_set(ct, tok, &ct->nextToken);
}

whcl_stoken * whcl__script_at_nc(whcl_script const * ct, whcl_stoken_id id){
  whcl_script const * top = whcl__script_top(ct);
  return top->chainLength && id>0 && id<=(whcl_stoken_id)top->chainLength
    ? top->chain+id-1 : NULL;
}

whcl_stoken const * whcl__script_at(whcl_script const * const ct,
                                    whcl_stoken_id id){
  return whcl__script_at_nc(ct, id);
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
static whcl_stoken * whcl__script__toker_chain_reserve( whcl_script * const ct, uint32_t n ){
  assert(ct->ec);
  if(!n){
    if(ct->chain){
      assert(ct->chainCapacity && "Trying to free an unowned chain?");
      cwal_free2(ct->ec, ct->chain, sizeof(whcl_stoken) * ct->chainCapacity);
    }
    cwal_engine_adjust_client_mem(ct->ec,
                                  -(cwal_int_t)(ct->chainCapacity * sizeof(whcl_stoken)));
    ct->chain = NULL;
    ct->chainLength = ct->chainCapacity = 0;
    return 0;
  }
  if(ct->chainCapacity < n){
    whcl_stoken * chain = (whcl_stoken *)cwal_realloc(ct->ec, ct->chain, n * sizeof(whcl_stoken));
    if(!chain){
      WHCL__WARN_OOM;
      fprintf(stderr,"Failed to allocate memory for %u tokens.\n", (unsigned)n);
      cwal_error_set(ct->ec, NULL, CWAL_RC_OOM, NULL);
      return NULL;
    }
    if(ct->chainCapacity){
      cwal_engine_adjust_client_mem(ct->ec,
                                    -((cwal_int_t)(ct->chainCapacity * sizeof(whcl_stoken))));
    }
    cwal_engine_adjust_client_mem(ct->ec, (cwal_int_t)(n * sizeof(whcl_stoken)));
    ct->chainCapacity = n;
    ct->chain = chain;
  }
  return ct->chain;
}


/**
   Expects its argument to be one of:

   TOK1_T_SquigglyOpen, TOK1_T_SquigglyGroup, TOK1_T_ParenOpen,
   TOK1_T_ParenGroup, TOK1_T_IdentifierDeref, TOK1_T_PropAccess,
   TOK1_T_BraceOpen, TOK1_T_BraceGroup, TOK1_T_CallBlock,
   TOK1_T_CallBlockOpen

   and asserts if passed any other type.

   Returns the matching group token type.
*/
static tok1_en_types_e tok1_t_group_type(int16_t ttype){
  switch(ttype){
    case TOK1_T_SquigglyOpen:
    case TOK1_T_SquigglyGroup:
      return TOK1_T_SquigglyGroup;
    case TOK1_T_ParenOpen:
    case TOK1_T_ParenGroup:
      return TOK1_T_ParenGroup;
    case TOK1_T_BraceOpen:
    case TOK1_T_BraceGroup:
      return TOK1_T_BraceGroup;
    case TOK1_T_PropAccess:
      return ttype;
    case TOK1_T_CallBlockOpen:
    case TOK1_T_CallBlock:
      return TOK1_T_CallBlock;
    default:
      MARKER(("Invalid group type: %s\n", tok1_t_cstr(ttype)));
      assert(!"internal misuse");
      return TOK1_T_INVALID;
  }
}

/**
   Expects its argument to be one of:

   TOK1_T_SquigglyOpen, TOK1_T_ParenOpen, TOK1_T_BraceOpen,
   TOK1_T_CallBlockOpen

   and asserts if passed any other type.

   Returns the matching closing token type.
*/
static tok1_en_types_e tok1_t_closer_type(int16_t ttype){
  switch(ttype){
    case TOK1_T_SquigglyOpen:
      return TOK1_T_SquigglyClose;
    case TOK1_T_CallBlockOpen:
    case TOK1_T_ParenOpen:
      return TOK1_T_ParenClose;
      break;
    case TOK1_T_BraceOpen:
      return TOK1_T_BraceClose;
    default:
      assert(!"internal misuse");
      return TOK1_T_INVALID;
  }
}

/**
   Requires tok->ttype to be one of:

   TOK1_T_SquigglyOpen, TOK1_T_ParenOpen, TOK1_T_BraceOpen,
   TOK1_T_CallBlockOpen

   This routine searches ct->chain for a matching closing element. If
   found, *ender is assigned to that token and 0 is returned, else an
   error is reported via ct->ec and that error's code is returned.
*/
static int whcl__script_find_closer( whcl_script * ct,
                                   whcl_stoken const * const tok,
                                   whcl_stoken ** ender ){
  enum tok1_en_types_e const closer = tok1_t_closer_type(tok->ttype);
  whcl_stoken * at = 0;
  unsigned int level = 0;
  bool const tokUsesParenClose = TOK1_T_ParenClose==closer;
  assert(tok->ttype==TOK1_T_SquigglyOpen
         || tok->ttype==TOK1_T_ParenOpen
         || tok->ttype==TOK1_T_BraceOpen
         || tok->ttype==TOK1_T_CallBlockOpen);
  while(1){
    at = whcl__script_at_nc(ct, at ? at->nextId : tok->nextId);
    switch(at->ttype){
      case 0:
        assert(!"should not be able to happen at this level");
        CWAL_SWITCH_FALL_THROUGH;
      case TOK1_T_EOF:
        goto mismatch;
      case TOK1_T_SquigglyOpen:
      case TOK1_T_BraceOpen:
        if(at->ttype == tok->ttype) ++level;
        break;
      case TOK1_T_CallBlockOpen:
      case TOK1_T_ParenOpen:
        if(tokUsesParenClose) ++level;
        break;
      case TOK1_T_ParenClose:
        if(tokUsesParenClose && closer == at->ttype){
          if(level) --level;
          else{
            *ender = at;
            return 0;
          }
        }
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
  return whcl__script_err(ct, CWAL_SCR_SYNTAX,
                     "Mismatched %s starting at line %d column %d.",
                     tok1_t_cstr(tok->ttype),
                     (int)tok->line, (int)tok->column);
}

bool whcl_stokens_touch( whcl_stoken const * const lhs,
                        whcl_stoken const * const rhs ){
  return lhs->begin + lhs->length == rhs->begin;
}


/**
   Walks through the given token chain and may make "certain
   modifications" to it:

   - Grouping tokens (), [], {}, are transformed to have a
     parent/child relationship with the content they wrap:

   1) The left-most token gets converted from (e.g.) TOK1_T_ParenOpen
      to (e.g.) TOK1_T_ParenGroup.

   2) The left-most token->innerId gets set to the ID of the first
      token "inside" the group. If the group is empty, innerId gets
      set to the ID of the closing element (see point #4).

   3) The left-most token->nextId gets set to the token _after_ the
      group's closing paren/brace/squiggly token.

   4) The group-closing token gets transformed to an EOF token, so
      that traversing that section of the chain will inherently end
      when it is encountered (without having to know what type of
      group it is). These tokens, unlike "real" EOF tokens, have a
      length of 1, thus it is possible to differentiate them (should
      it be needed (historically it has not been needed)).

   e.g. a token chain of:

   ```
   a ( b c() d ) e<EOF>
   ```

   gets transformed to:

   ```
   a e<EOF>
    \
     b c d<EOF>
        \
         <EOF>
   ```

   Recursively, of course.

   The second argument is for recursion purposes. It must initially be
   passed ct->chain.

   Tokens of a limited subset of types which immediately neighbor a
   `[...]` construct (no gaps between them) are restructured such
   that:

   ```
   X[y][z] A
   ```

   Gets transformed to:

   ```
   X --> A
    \
    SUB --> SUB --> EOF
      \       \
       y       z
   ```

   Where SUB is type TOK1_T_PropAccess.

   Returns 0 on success. Since the input chain has already been fully
   tokenized, the only error cases this routine catches are mismatched
   grouping tokens.

   Maintenance reminder: this does some "highly WHCL-specific" work in
   order to avoid having to do a second pass on the chain by
   delegating those bits to WHCL-level code. It works in cooperation
   with some of the WHCL-specific processing done via the token
   provider impl.
*/
static int whcl__script__post_compile_chain( whcl_script * const ct,
                                     whcl_stoken * const chain );

/**
   Internal helper for whcl__script__post_compile_chain() which restructures
   supertokens and their contents into a parent/child relationship
   instead of their original sibling relationship.

   Expects tok->ttype to be an opener token for one of the supertoken
   types and may assert that that is the case.
*/
static int whcl__script__regroup(whcl_script * const ct,
                                 whcl_stoken * const tok,
                                 whcl_stoken ** ptPrev,
                                 whcl_stoken ** propAccessHead,
                                 whcl_stoken_id *nextId){
  switch(tok->ttype){
    case TOK1_T_CallBlockOpen:
    case TOK1_T_SquigglyOpen:
    case TOK1_T_ParenOpen:
    case TOK1_T_BraceOpen:
      break;
    default:
      whcl__fatal(CWAL_RC_CANNOT_HAPPEN,
                  "WARNING; internal API misuse: %s() was passed "
                  "an unexpected %s token.",
                  __func__, tok1_t_cstr(tok->ttype));
      return 0/*not reached*/;
  }
  int rc;
  whcl_stoken * tPrev = *ptPrev;
  whcl_stoken * closer = NULL;
  whcl_stoken * inner = NULL;
  rc = whcl__script_find_closer(ct, tok, &closer);
  if(rc) return rc;
  assert(closer);
  if(closer==whcl__script_at(ct, tok->nextId)){
    /* Empty block. */
    /* The closer token (tok->nextId) will get transformed into
       an EOF below, so we'll use that token rather than the
       outermost EOF. */
    assert(tok->nextId==closer->id);
    tok->innerId = closer->id;
  }else{
    tok->innerId = tok->nextId;
    inner = whcl__script_at_nc(ct, tok->innerId);
  }
  /* Extend tok to wrap the whole block... */
  *nextId = tok->nextId = closer->nextId;
  tok->length = closer->begin - tok->begin + closer->length;
  tok->innerOffset = (TOK1_T_CallBlockOpen==tok->ttype)
    ? 2 : 1;
  tok->ttype = tok1_t_group_type(tok->ttype);
  assert(tok->length >= 2);
  tok->innerLength = tok->length
    - (TOK1_T_CallBlockOpen==tok->ttype ? 3 : 2);
  /* Convert the block's closing token into a virtual EOF for
     the inner part. Algorithms which climb down branches of
     this script rely on the existince of such a token. */
  closer->ttype = TOK1_T_EOF;
  closer->nextId = NO_ID;
  if(TOK1_T_SquigglyGroup!=tok->ttype){
    if(whcl_stoken_is_eof(whcl__script_at(ct, tok->innerId))){
      whcl__script_errtoken_set(ct, tok);
      return whcl__script_err(ct, CWAL_SCR_SYNTAX,
                          "Empty () and [] blocks are illegal.");

    }
  }
  if(TOK1_T_BraceGroup == tok->ttype
     && tPrev && whcl_stokens_touch(tPrev, tok)){
    if(*propAccessHead){
      /** Transform $var[x][y][...z] ... to:
          $var -->...
          \
          [x] --> [y] --> [...z] --> EOF

          Search whcl__script__post_compile_chain() for OpDot copious
          notes. */
      whcl_stoken * const ppa = *propAccessHead;
      *nextId = tok->nextId
        /* Skip over tok in the top chain. */;

      /* Make tok the tail of the sub-chain, noting that tPrev might
         == ppa, so order is important. */
      if(TOK1_T_PropAccessWith == ppa->ttype){
        tPrev->nextId = tok->id;
        ppa->nextId = tok->nextId;
      }else{
        ppa->nextId = tok->nextId;
        tPrev->nextId = tok->id;
      }
      tok->nextId = whcl__script_eof_id(ct);
      if(!ppa->subscriptId){
        ppa->subscriptId = tok->id;
      }
      tok->ttype = TOK1_T_PropAccess
        /* Re-tag it so that the evaluation process can Do The Right
           Thing. */;
    }else if(whcl__t_legal_prop_lhs(tPrev)){
      /* Transform $var[xyz] and BIV[...] token pairs so
         that they're easier to handle during evaluation:

         $var ---> ...
          \
           xyz --> EOF
           
         Search whcl__script__post_compile_chain() for OpDot copious
         notes. */
      *propAccessHead = tPrev
        /* Remember this element for the [x][y] chaining
           case (see previous block) */;
      tPrev->nextId = tok->nextId
        /* Skip over tok in the top chain. */;
      tPrev->subscriptId = tok->id
        /* Move tok into the inner chain. */;
      tok->nextId = closer->id
        /* Terminate tok with an EOF. */;
      tok->ttype = TOK1_T_PropAccess;
    }
  }/*BraceGroup*/
  assert(0==rc);
  if(inner){
    rc = whcl__script__post_compile_chain( ct, inner );
    if(rc) return rc;
    switch(tok->ttype){
      /* Eliminate newlines in brace groups, treating the whole
         contents of the block as a single line. We
         don't/shouldn't need to backslash-escape EOLs in [...]
         and (...) for the same reason reason we don't need to
         backslash-escape the contents of {...}, namely that we
         already know where the end of the chain is.

         Bug: unless we want to adjust/renumber `inner`, we cannot
         strip _leading_ newlines.
      */
      case TOK1_T_CallBlock:
      case TOK1_T_ParenGroup:
      case TOK1_T_BraceGroup:{
        whcl_stoken * ti = inner;
        whcl_stoken * prevNonJunk = NULL;
        whcl_stoken const * firstNonJunk = NULL;
        tPrev = NULL;
        for( ; 0==rc && ti && ti->nextId;
             tPrev = ti, ti = whcl__script_at_nc(ct, ti->nextId) ){
          switch(ti->ttype){
            case TOK1_T_EOL:
              if(prevNonJunk){
                prevNonJunk->nextId = ti->nextId;
              }else if(inner == ti){
                tok->innerId = ti->nextId;
              }
              if(tPrev){
                if(tok1_t_is_junk(tPrev->ttype)
                   || tok1_t_is_eol(tPrev->ttype)){
                  tPrev->nextId = ti->nextId;
                  tPrev->length = ti->begin + ti->length - tPrev->begin;
                }
              }else{
                ti->ttype = TOK1_T_Whitespace;
              }
              break;
            case TOK1_T_EOX:
              whcl__script_errtoken_set(ct, ti);
              return whcl__script_err(ct, CWAL_SCR_SYNTAX,
                                  "Semicolon is not a legal EOX "
                                  "within a [...] or (...) block.");
              break;
            default:
              /* Snip out any junk tokens */
              if(!tok1_t_is_junk(ti->ttype)){
                if(!firstNonJunk) firstNonJunk = ti;
                if(prevNonJunk) prevNonJunk->nextId = ti->id;
                prevNonJunk = ti;
              }
              break;
          }
        }/*for() loop*/
        if(firstNonJunk){
          tok->innerId = firstNonJunk->id;
        }
        break;
      }/* ^^^ [brace] and (parens) groups newline stripping */
    }/* switch() for brace/paren groups */
  }/* if(inner) */
  assert(0==rc);
  return rc;
}

int whcl__script__post_compile_chain( whcl_script * const ct,
                                      whcl_stoken * const chain ){
  whcl_stoken * tok = chain /* current token */;
  whcl_stoken * tPrev = NULL /* previous token */;
  whcl_stoken * propHead = NULL
    /* The first prop-access token in a chain of such tokens. Used for
       restructuring such chains. */;
  int rc = 0;
  assert(tok && "Internal misuse.");
  /*
    We want two passes for the sake of being able to use block
    constructs as the RHS of the dot op. We can only do that if the
    block constructs are handled before the dot. The x.[...] construct
    is tricky because it is itself translated into a dot op and shares
    state with it, so we can apparently only chain them if the [...]
    is processed _before_ its LHS dot.
  */
  while(1){
    whcl_stoken_id nextId = tok->nextId;
    switch(tok->ttype){
      case TOK1_T_SquigglyOpen:
      case TOK1_T_ParenOpen:
        rc = whcl__script__regroup(ct, tok, &tPrev, &propHead, &nextId);
        break;
    }
    if(rc || TOK1_T_EOF==tok->ttype) break;
    tPrev = tok;
    tok = whcl__script_at_nc(ct, nextId);
  }
  if(rc) return rc;
  tok = chain;
  tPrev = propHead = NULL;
  while(1){
    whcl_stoken_id nextId = tok->nextId;
    switch(tok->ttype){
      case TOK1_T_Whitespace:
      case TOK1_T_CommentTCL:
      case TOK1_T_CommentCpp:
      case TOK1_T_CommentC:
      case TOK1_T_EOL:
        /* Collapse runs of identical tokens of this type */
        if(tPrev && tok->ttype==tPrev->ttype){
          tPrev->nextId = tok->nextId;
        }
        break;
      case TOK1_T_CallBlockOpen:
      case TOK1_T_BraceOpen:
        rc = whcl__script__regroup(ct, tok, &tPrev, &propHead, &nextId);
        break;
      case TOK1_T_OpDot: {
        /* Restructure X.Y to look like a X[Y] property access. This
           transformation is identical to the one we do for X[Y] and
           is compatible with it. It is not performed together with
           the X[Y] transformation whcl__script__regroup() because
           mixing of this particular type's bits with those (related
           but distinctly different) would make the logic unduly
           messy.

           Transform a standlone .X (with no LHS) to one with a ttype2
           of TOK1_T_PropAccessWith.
        
           tok->ttype2 holds the number of dots in the token. For
           purposes of nesting `with` blocks, We allow one extra dot
           per level up in the "with" chain. We implement that as a
           new whcl_scope::flags entry and walk up the scopes until we
           find the right "with" or hit a function call
           boundary. tok->ttype2 is set to the number of dots the
           token represents. */
        bool const doWith = !tPrev ||
          (tPrev
           && !propHead
           && !whcl_stokens_touch(tPrev, tok))
          /* i.e. a standalone dot token */
          /* true == use the "with" approach: .Y without an LHS. */;
        //if(tPrev) {whcl__dump_stok(ct, tPrev, "tPrev");}
        //whcl__dump_stok(ct, tok, "tok");
        assert(tok->ttype2 > 0 && "This is how we know how many dots.");
        switch(doWith ? 0 : (tPrev ? tPrev->ttype : -1)){
          /* Types of tokens which are valid for the LHS of X.Y
             access. */
          case 0: break;
          case TOK1_T_BIV:
          case TOK1_T_BraceGroup:
          case TOK1_T_CallBlock:
          case TOK1_T_Identifier:
          case TOK1_T_IdentifierDeref:
          //case TOK1_T_LiteralNumber:
          /* ^^^ Not permitted because tokenization fails for
             DIGIT.IDENTIFIER.  We "could" special-case that
             tokenization but it currently seems unnecessary. */
          case TOK1_T_PropAccess:
          case TOK1_T_PropAccessWith:
          case TOK1_T_QuotedString:
            break;
          default:
            /* We currently have to fail for most LHS types because we
               remap tPrev to a different type below and doing so will
               break most token types. We can possibly improve that by
               moving the is-deref indicator into ttype2, which might
               allow us to have arbitrary LHS's for property deref
               ops. */
            whcl__script_errtoken_set(ct, tok);
            rc = whcl__script_err(ct, CWAL_SCR_SYNTAX,
                              "Invalid LHS token type (%s) for '.'.",
                              tPrev
                              ? whcl__script_ttype_cstr(tPrev)
                              : "start of (sub)script");
            break;
        }
        if(rc) break;
        whcl_stoken * const tNext = whcl_stoken_sibling_nc(ct, tok);
        if((!whcl_stokens_touch(tok, tNext) || whcl_stoken_is_eox(tNext))
           && (!tPrev || !whcl_stokens_touch(tPrev, tok))){
          /* Treat this as a normal dot token. This case can come up
             with, e.g.: echo ..... . */
          break;
        }
        switch(tNext->ttype){
          /* Types of tokens which are valid for the RHS of X.Y
             access. */
          case TOK1_T_BIV:
          case TOK1_T_BIC
            /* Special cases: identifier which got flagged as a
               same-named BIC/BIV. Undo that. */:
            tNext->ttype = TOK1_T_Identifier;
            tNext->ttype2 = 0;
            break;
          case TOK1_T_BraceOpen:
              /*
                tNext has not yet been processed into a block
                construct, so we cannot catch, e.g., TOK1_T_CallBlock
                tokens here.  To fix that we'll need to add another
                post-compile pass which processes all blocks
                first. That second pass, however, is closely tied to
                the handling of propHead, so we have to
                restructure these as part of the dot-prop resolution
                in order t make [x] and .x forms chainable.
              */ break ;
          case TOK1_T_Heredoc:
          case TOK1_T_SquigglyGroup:
          case TOK1_T_ParenGroup:
          case TOK1_T_Identifier:
          case TOK1_T_IdentifierDeref:
          case TOK1_T_QuotedString: break;
          case TOK1_T_LiteralNumber:
            if(TOK1_T_LiteralIntDec==tNext->ttype2) break;
            CWAL_SWITCH_FALL_THROUGH;
          default:
            whcl__script_errtoken_set(ct, tok);
            rc = whcl__script_err(ct, CWAL_SCR_SYNTAX,
                              "Invalid RHS token type (%s) for '.'.",
                              whcl__script_ttype_cstr(tNext));
            break;
        }
        if(rc) break;
        else if(doWith && !whcl_stokens_touch(tok, tNext)){
          /* Treat this as a plain token. */
          break;
        }
        whcl_stoken_id realNext = 0
          /* The real ID we want to use for nextId. This will differ
             from what we'd planned on if the RHS is a brace group. */;
        tok->ttype = doWith ? TOK1_T_PropAccessWith : TOK1_T_PropAccess
          /* We have to re-map this early for RHS [...] handling to
             work. If it turns out that this is not a prop access, we'll
             set it back to OpDot. */
          ;
        if(TOK1_T_BraceOpen==tNext->ttype){
          /* In order to be able to have [call block] as an RHS, we
             have to process the RH from X.[...] before processing the
             dot so that the [...], instead of use the opening `[`,
             becomes part of the property access. Note that (...)  and
             {...} do not require this specific treatment: we can
             simply process all of those blocks in advance in order to
             get this behavior. The [...] needs special treatment in
             order to be able to chain a mix of `.` and
             `[propKey]`-style property access. */
          whcl_stoken_id subNext = 0;
          whcl_stoken * subPrev = NULL;
          whcl_stoken * subPpa = propHead ? propHead : tok;
          rc = whcl__script__regroup(ct, tNext, &subPrev, &subPpa, &subNext);
          if(rc) break;
          assert(subPpa == propHead || subPpa == tok);
          realNext = tok->nextId = subNext;
          assert(TOK1_T_BraceGroup==tNext->ttype || TOK1_T_PropAccess==tNext->ttype);
          tNext->ttype = TOK1_T_BraceGroup;
          if(tok == subPpa && tok->subscriptId){
            /* We need innerId, not subscriptId, for this case, and will
               remap it below. */
            tok->subscriptId = whcl_stoken_id_none;
          }
          tNext->nextId = whcl__script_eof_id(ct);
        }/*brace group RHS*/
        if(propHead){
          /** This is part of a chain of prop access operations (dots
              or [...]). propHead is the left-most entry in that
              chain. tPrev points to the previous entry in the chain
              and might point to propHead.

              Transform $var[x][y][...] Z... to:
              $var --> Z...
              \
              [x] --> [y] --> [...] --> EOF

              See further notes in this if's next else block. */
          nextId = realNext ? realNext : tNext->nextId;
          /* Adjust X.[Y] for parentage. */
          assert(tPrev && "If we have a propHead, we must have a tPrev");
          if(!propHead->subscriptId){
            propHead->subscriptId = tok->id;
          }
          tPrev->nextId = tok->id;
          tok->nextId = whcl__script_eof_id(ct);
          if(propHead->ttype == TOK1_T_PropAccessWith){
            /* Make tok the tail of the sub-chain... */
            if(tPrev && propHead!=tPrev){
              /* Previous property access. If propHead==tPrev we don't
                 want this: propHead->next always needs to point to
                 the token following the full property access
                 chain. */
              tPrev->nextId = tok->id;
            }
            if(!propHead->subscriptId){
              /* For the PropertyAccessWith case, the subscriptId is
                 not set until a prop access chain with 2+ links has
                 started. The initial link is stored in innerId for that
                 case. */
              propHead->subscriptId = tok->id;
            }
          }else{
            tPrev->nextId = tok->id /* Make tok the tail of the sub-chain */;
          }
          tok->nextId = tNext->nextId = whcl__script_eof_id(ct);
          tok->innerId = tNext->id;
          propHead->nextId = nextId
            /* Skip over tok in the top chain */;
        }else if(doWith || (whcl__t_legal_prop_lhs(tPrev)
                            && whcl_stokens_touch(tPrev, tok))){
          /* Transform X.Y Z... to:
             X --> Z...
              \
               Y --> EOF
             and make X looks like a property access LHS.

             propHead is the left-most entry in a chain of prop
             access, and we manipulate its subscriptId list as the chain
             is built up.

             The cases of X.Y (PropAccess) and .Y (PropAccessWith)
             require slightly different structures because for the
             latter case we have no LHS token we can transform to hold
             the structure used by the former case.

             PropAccess is structured in a sub-chain of such tokens,
             all of them siblings, with the
             propHead->subscriptId pointing to the top of that
             chain.

             PropAccessWith places the Y part of the leading .Y into
             the propHead->innerId and any subsequently-chained
             RHS property accesses get added to the sub-chain pointed
             to by propHead->subscriptId. Thus only the first
             prop access in a PropAccessWith differs from the
             PropAccess structure. After that, they're identical.  The
             code which evals tokens has to be aware of this
             difference and have slightly different behavior in the
             _first_ iteration of any loop which crawls the property
             access chain.

             In all cases, propHead->nextId points to the token
             after the final final entry in a prop access chain and
             that final link in the prop access chain has a nextId of
             EOF. */
          whcl_stoken * const tContainer = doWith ? tok : tPrev;
          propHead = tContainer;
            /* Remember this element for x[y][z] chaining */;
          nextId = tContainer->nextId = realNext ? realNext : tNext->nextId;
            /* Redirect X->next to Y->next */;
          tNext->nextId = whcl__script_eof_id(ct);
          assert(!tok->innerId);
          tok->innerId = tNext->id /* Make Y a child of the dot */;
          if(doWith){
            assert(TOK1_T_PropAccessWith==tok->ttype);
            /* Because we have no LHS to bind to, we have to structure
               these slightly differently from PropAccess tokens. */
            tok->nextId = nextId; //tNext->nextId
              /* Redirect dot->next to EOF */;
          }else{
            assert(!tPrev->subscriptId);
            assert(TOK1_T_PropAccess==tok->ttype);
            tok->nextId = tNext->nextId
              /* Redirect dot->next to EOF */;
            tContainer->subscriptId = tok->id
              /* Make the dot from X. a child of X */;
          }
        }else{
          tok->ttype = TOK1_T_OpDot
            /* We had to re-map this earlier for RHS [...] handling to
               work. Turns out it's just a dot, though, not a prop
               access. */;
        }
        if(rc) break;
        switch(tok->ttype){
          default: break;
          case TOK1_T_PropAccess:
          case TOK1_T_PropAccessWith:
            if(tok->innerId || tok->subscriptId){
              tNext->nextId = whcl__script_eof_id(ct)
                /* Redirect Y->next to EOF */;
              tok->length = tNext->begin - tok->begin + tNext->length
                /* Extend tok to wrap the whole block... */;
              tok->innerOffset = 1;
              tok->innerLength = tok->length - 1
                /*Strip the '.' from the token string*/;
            }
            assert(propHead);
            if(0 && propHead != tok){
              /* i would really like to do this but the string part of
                 propHead is used as a property key later on, so we can't
                 without significant surgery on the token type. */
              propHead->length = tNext->begin - propHead->begin + tNext->length
                /* Extend to wrap the whole block... */;
            }
            if(TOK1_T_BraceGroup == tNext->ttype){
              /* This was a combo .[...] operation. */
              assert(whcl__script_eof_id(ct) == tNext->nextId);
              nextId = realNext;
            }
            break;
        }
      }/* TOK1_T_OpDot */
      default:
        break;
    }
    if(rc || TOK1_T_EOF==tok->ttype) break;
    if(TOK1_T_PropAccess!=tok->ttype && TOK1_T_PropAccessWith!=tok->ttype){
      propHead = NULL /* terminate any pending $x[y][z] chain */;
    }
    tPrev = tok;
    tok = whcl__script_at_nc(ct, nextId);
  }/*while(1)*/
  return rc;
#undef TWO_PASS
}

whcl_script * whcl__script_alloc( cwal_engine * const e ){
  whcl_script * const cp = (whcl_script *)cwal_malloc2(e, sizeof(whcl_script));
  if(cp){
    whcl__script_init(e, cp);
    cwal_engine_adjust_client_mem(e, (cwal_int_t)sizeof(whcl_script));
  }
  return cp;
}

void whcl_script_free( whcl_script * const cp ){
  assert(cp ? !!cp->ec : 1);
  if(cp){
    cwal_engine * const e = cp->ec;
    whcl__script_finalize(cp);
    cwal_engine_adjust_client_mem(e, -(cwal_int_t)sizeof(whcl_script));
    cwal_free2(e, cp, sizeof(whcl_script));
  }
}

void whcl__script_init( cwal_engine * const e, whcl_script * const ct ){
  *ct = whcl__script_empty;
  ct->ec = e;
}


whcl_script * whcl__script_rewind( whcl_script * const ct ){
  ct->token = ct->pbToken = ct->errToken = NO_ID;
  if(ct->chain){//whcl__script_top(ct)->chainLength){
    ct->nextToken = ct->chain[0].id;
  }else{
    ct->nextToken = 0;
  }
  return ct;
}


int whcl__script_sub_from_group( whcl_script * const parent,
                                 whcl_script * const ct,
                                 bool useInnerId){
  whcl_script const * const top = whcl__script_top( parent );
  whcl_stoken * const chain = whcl__script_at_nc(top, parent->token);
  whcl_stoken_id childId =
    chain ? (useInnerId ? chain->innerId : chain->subscriptId) : 0;
  if(!chain){
    return whcl__script_err(parent, CWAL_RC_MISUSE,
                        "Parent whcl_script has no chain.");
  }else if(!childId){
    whcl__script_errtoken_set(parent, whcl__script_token(parent));
    return whcl__script_err(parent, CWAL_RC_TYPE,
                        "%s() cannot create group from non-sub "
                        "token type %s.", __func__,
                        tok1_t_cstr(chain->ttype));
  }
  ct->top = top;
  ct->parent = parent;
  ct->chain = whcl__script_at_nc(parent, childId);
  assert(ct->chain);
  ct->ec = parent->ec;
  whcl__script_rewind(ct);
  ct->chainLength = ct->chainCapacity = 0 /*tag that ct does not own ct->chain*/;
  ct->begin = top->begin + chain->begin;
  ct->end = ct->begin + chain->length;
  ct->name = top->name;
  return 0;
}


int whcl__script_compile_buffer( whcl_script * const ct, cwal_buffer const * const buf,
                        uint32_t compileFlags,
                        whcl_stoken_izer_f tp, void * tpState ){
  return whcl__script_compile(ct, buf->mem ? (char const *)buf->mem : "",
                     buf->mem ? (cwal_int_t)buf->used : 0,
                     compileFlags, tp, tpState );
}

int whcl__script_compile( whcl_script * const ct, char const * src,
                          cwal_int_t len, uint32_t flags,
                          whcl_stoken_izer_f tp, void * tpState ){
  tok1_izer pt = tok1_izer_empty;
  tok1_izer_init( ct->ec, &pt, src, len, TOK1_F_LINEAR_TOKER );
  pt.name = ct->name;
  int const rc = whcl__script_compile_toker( ct, &pt, flags, tp, tpState );
  tok1_izer_finalize( &pt );
  return rc;
}

/**
   Returns a guesstimated number of tokens which are remaining in the
   given tokenizer. This is a dumb heuristic based solely on the
   "remaining" size of the tokenizer, counting from the start of its
   current token to the end of the tokenizer, plus 1 (to account for
   an EOF token).
*/
static uint32_t whcl__script_estimate_token_count(tok1_izer const * pt){
  if(tok1_izer_is_eof(pt)) return 1;
  else{
    const char * begin = tok1_en_begin(&pt->token)
      ? tok1_en_begin(&pt->token)
      : tok1_izer_begin(pt);
    const char * end = tok1_izer_end(pt);
    return (((uint32_t)(end - begin))*55/10) / sizeof(whcl_stoken) + 1;
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
static void whcl__script_chain_trim( whcl_script * ct ){
  if(ct->chainCapacity >= 50/*arbitrary*/
     && ct->chainCapacity > ct->chainLength * 12/10){
    void * re = cwal_realloc(ct->ec, ct->chain, ct->chainLength * sizeof(whcl_stoken));
    if(re){
      cwal_engine_adjust_client_mem(ct->ec,
                                    -((cwal_int_t)(ct->chainCapacity * sizeof(whcl_stoken))));
      ct->chain = (whcl_stoken*)re;
      ct->chainCapacity = ct->chainLength;
      cwal_engine_adjust_client_mem(ct->ec,
                                    ((cwal_int_t)(ct->chainCapacity * sizeof(whcl_stoken))));
    }
    /* Ignore realloc error - it's not fatal here. */
  }
}

int whcl__script_compile_toker( whcl_script * const ct,
                                tok1_izer const * const psrc,
                                uint32_t flags,
                                whcl_stoken_izer_f tpf,
                                void * tpfState ){
  int rc = 0;
  whcl_stoken_id ndx = NO_ID;
  tok1_izer pt = *psrc;
  assert(ct->ec && "ct has not been whcl__script_init()'d");
  if(ct->parent){
    return whcl__script_err(ct, CWAL_RC_MISUSE,
                      "Cannot compile a sub-tokenizer.");
  }
  c9__err_reset(ct);
  ct->chainLength = 0 /* re-use any chain we currently have */;
  whcl__script_rewind(ct);
  ct->begin = tok1_izer_begin(&pt);
  ct->end = tok1_izer_end(&pt);
  if(!whcl__script__toker_chain_reserve(ct, whcl__script_estimate_token_count(&pt))){
    WHCL__WARN_OOM;
    return CWAL_RC_OOM;
  }
  /*if(pt.name){
    ct->name = tok1_izer_name_first(psrc, 0);
    }*/
  tok1_izer_reset(&pt);
  if(WHCL__SCRIPT_F_IDENTIFIER_DASHES2
     ==(WHCL__SCRIPT_F_IDENTIFIER_DASHES2 & flags)){
    pt.flags |= TOK1_F_IDENTIFIER_DASHES2;
  }else if(WHCL__SCRIPT_F_IDENTIFIER_DASHES & flags){
    pt.flags |= TOK1_F_IDENTIFIER_DASHES;
  }else{
    pt.flags &= ~TOK1_F_IDENTIFIER_DASHES2;
  }
  pt.flags |= TOK1_F_LINEAR_TOKER;
  assert(ct->chainCapacity>0);
  for(ndx = 0; 0==(rc = whcl__script_next_for_compile(ct, &pt, flags,
                                                   tpf, tpfState));
      ++ndx){
    whcl_stoken * ctok;
    if(ct->chainCapacity < (uint32_t)(ndx+1)){
      /* Reminder: that cast ^^^^^^ is a workaround for a compiler
         mis-diagnosis that (ndx+1) is of type signed int. */
      uint32_t const newCap = ct->chainCapacity
        + whcl__script_estimate_token_count(&pt);
      if(!whcl__script__toker_chain_reserve(ct, newCap)){
        WHCL__WARN_OOM;
        rc = CWAL_RC_OOM;
        goto err;
      }
    }
    ++ct->chainLength;
    ctok = ct->chain + ndx;
    *ctok = whcl_stoken__empty;
    ctok->id = ndx+1;
    if(ndx){
      ct->chain[ndx-1].nextId = ctok->id;
    }
    rc = tok1_en_to_cptoken(ct, &pt.token, ctok);
    if(rc) goto err;
#if 0
    whcl__dump_stok(ct, ctok, "token");
#endif
    if(tok1_izer_is_eof(&pt)) break;
  }
  if(rc) goto end;
  assert(tok1_izer_is_eof(&pt));
  assert(TOK1_T_EOF==ct->chain[ndx].ttype);
  rc = whcl__script__post_compile_chain(ct, ct->chain);
  if(!rc){
    goto end;
  }
  err:
  assert(rc);
  assert(0!=whcl__script_err_get(ct, NULL, NULL) || CWAL_RC_OOM==rc);
  if(ct->chain && !ct->errToken){
    whcl_stoken const * et = whcl__script_at(ct, ndx+1);
    ct->errToken = et ? et->id : NO_ID;
  }
  end:
  if(!rc){
    whcl__script_chain_trim(ct);
  }
  whcl__script_rewind(ct);
  return rc;
}

void whcl__script_finalize( whcl_script * const ct ){
  if(ct->ec){
    /* If ct->top is not NULL then any ct->chain component belongs to
       a parent tokenizer (or someone else, hypothetically). */

    if(!ct->top && ct->chain){
      //MARKER(("Finalizing toker '%s'\n", ct->name));
      //whcl__dump_stok(ct, ct->chain, "first token of finalizing ct");
      whcl__script__toker_chain_reserve(ct, 0);
    }
    if(ct->ownSrc) cwal_free(ct->ec, ct->ownSrc);
    if(ct->ownName) cwal_free(ct->ec, ct->ownName);
  }
  *ct = whcl__script_empty;
}

whcl_stoken const * whcl__script_token(whcl_script const * const ct){
  return whcl__script_at(ct, ct->token);
}

whcl_stoken * whcl__script_token_nc(whcl_script * const ct){
  return whcl__script_at_nc(ct, ct->token);
}

whcl_stoken_id whcl__script_putback(whcl_script * const ct){
  ct->token = ct->pbToken;
  ct->pbToken = NO_ID;
  return ct->token;
}

whcl_stoken const * whcl_stoken_sibling( whcl_script const * const ct,
                                    whcl_stoken const * const t ){
  return (t && t->nextId) ? whcl__script_at(ct, t->nextId) : NULL;
}
whcl_stoken * whcl_stoken_sibling_nc( whcl_script const * const ct,
                                 whcl_stoken const * const t ){
  return (t && t->nextId) ? whcl__script_at_nc(ct, t->nextId) : NULL;
}

int whcl__script_next_token2( whcl_script * const ct, whcl_stoken const ** tgt ){
  whcl_stoken_id id = NO_ID;
  whcl_stoken const * ctok = 0;
  ct->errToken = NO_ID;
  if(!ct->chain){
    if(tgt) *tgt = 0;
    return whcl__script_err(ct, CWAL_RC_MISUSE, "This chain has no tokens.");
  }
  if(NO_ID!=ct->nextToken){
    id = ct->nextToken;
    ct->nextToken = NO_ID;
  }else if(NO_ID==ct->token){
    id = ct->chain[0].id;
  }else{
    ctok = whcl__script_at(ct, ct->token);
    assert(ctok);
    if(TOK1_T_EOF==ctok->ttype){
      /* Once we hit EOF, keep returning that same EOF
        token. Eventually the caller will get the hint. */
      id = ct->token;
    }else if(!(id = ctok->nextId)){
      /* Return top-most EOF token. This normally doesn't happen,
         but can after performing certain token restructuring
         during post-processing */
      id = whcl__script_eof_id(ct);
    }
  }
  assert(id && "No token ID???");
  if(tgt) *tgt = whcl__script_at(ct, id);
  ct->pbToken = ct->token;
  ct->token = id;
  return 0;
}

whcl_stoken const * whcl__script_next_token1( whcl_script * const ct ){
  whcl_stoken const * k = NULL;
  whcl__script_next_token2(ct, &k);
  return k;
}

char const * whcl_stoken_begin( whcl_script const * const c,
                              whcl_stoken const * const k){
  return whcl__script_top(c)->begin + k->begin;
}

char const * whcl_stoken_begin2( whcl_script const * const c,
                               whcl_stoken const * const k,
                               bool inner){
  return whcl__script_top(c)->begin + k->begin + (inner ? k->innerOffset : 0);
}

char const * whcl_stoken_end( whcl_script const * const c,
                            whcl_stoken const * const k){
  return whcl__script_top(c)->begin + k->begin + k->length;
}

char const * whcl_stoken_end2( whcl_script const * const c,
                             whcl_stoken const * const k,
                             bool inner ){
  return inner
    ? whcl__script_top(c)->begin + k->begin + k->innerOffset + k->innerLength
    : whcl__script_top(c)->begin + k->begin + k->length;
}

int whcl_stoken_block_has_content( whcl_script const * const ct,
                                 whcl_stoken const * tok ){
  whcl_stoken const * tt;
  if(!tok) tok = whcl__script_token(ct);
  tt = tok ? whcl__script_at(ct, tok->innerId) : 0;
  for( ; tt && TOK1_T_EOF!=tt->ttype;
       tt = whcl__script_at(ct, tt->nextId) ){
    switch(tt->ttype){
      case TOK1_T_NL:
      case TOK1_T_EOL:
        break;
      default:
        if(!tok1_t_is_junk(tt->ttype)){
          return tt->ttype;
        }
        break;
    }
  }
  return 0;
}

int whcl_stoken_is_eof( whcl_stoken const * const tok ){
  return tok->ttype==TOK1_T_EOF ? tok->ttype : 0;
}

int whcl__script_is_eof( whcl_script const * const ct ){
  whcl_stoken const * const t = whcl__script_token(ct);
  return t ? whcl_stoken_is_eof(t) : 0;
}

int whcl_stoken_strcmp(whcl_script const * const ct,
                       whcl_stoken const * const k,
                       char const * str, cwal_int_t nStr,
                       bool innerOnly){
  cwal_midsize_t tLen = 0;
  char const * ts = whcl_stoken_cstr(ct, k, &tLen, innerOnly);
  return cwal_compare_cstr(ts, tLen, str,
                           nStr>=0 ? (cwal_size_t)nStr
                           : cwal_strlen(str));
}

bool whcl_stoken_matches( whcl_script const * const ct,
                           whcl_stoken const * const k,
                           char const * const flag,
                           cwal_int_t nFlag ){
  uint16_t const n = nFlag<0
    ? (uint16_t)cwal_strlen(flag) : (uint16_t)nFlag;
  return n==k->length
    && 0==whcl_stoken_strcmp(ct, k, flag, (cwal_int_t)n, false);
}

int whcl__script_parse_binary_digits( char const * digits,
                              cwal_size_t digLen,
                              cwal_int_t * out ){
  cwal_uint_t i = 0
    /* use unsigned during parsing for guaranteed overflow behaviour */;
  char const * end = digits + digLen;
  assert(sizeof(cwal_size_t) >= sizeof(cwal_int_t));
  if(end <= digits) return CWAL_RC_RANGE;
  for( ; digits < end; ++digits ){
    switch(*digits){
      case '0':
        i <<= 1;
        continue;
      case '1':
        i = (i << 1) | 1;
        continue;
      case '_':
        continue;
      default:
        return CWAL_RC_RANGE;
    }
  }
  if(out) *out = (cwal_int_t)i;
  return 0;
}

int whcl__script_parse_octal_digits( char const * digits, cwal_size_t digLen,
                                     cwal_int_t * out ){
  cwal_uint_t i = 0
    /* use unsigned during parsing for guaranteed overflow behaviour */;
  char const * end = digits + digLen;
  char digit;
  assert(sizeof(cwal_size_t) >= sizeof(cwal_int_t));
  if(end <= digits) return CWAL_RC_RANGE;
  for( ; digits < end; ++digits ){
    digit = *digits;
    switch(digit){
      case '0': case '1': case '2':
      case '3': case '4': case '5':
      case '6': case '7':
        i = (i << 3) | (cwal_uint_t)(digit-'0');
        break;
      case '_':
        continue;
      default:
        return CWAL_RC_RANGE;
    }
  }
  if(out) *out = (cwal_int_t)i;
  return 0;
}

int whcl__script_parse_decimal_digits( char const * digits, cwal_size_t digLen,
                                       cwal_int_t * out ){
  cwal_uint_t i = 0
    /* use unsigned during parsing for guaranteed overflow behaviour */;
  char const * end = digits + digLen;
  char digit;
  short sign = 1;
  assert(sizeof(cwal_size_t) >= sizeof(cwal_int_t));
  if(end <= digits) return CWAL_RC_RANGE;
  if('-'==*digits){
    sign = -1;
    ++digits;
  }else if('+'==*digits){
    sign = 1;
    ++digits;
  }
  for( ; digits < end; ++digits ){
    digit = *digits;
    switch(digit){
      case '0': case '1': case '2':
      case '3': case '4': case '5':
      case '6': case '7': case '8':
      case '9':
        i = (i * 10) + (cwal_uint_t)(digit-'0');
        break;
      case '_':
        continue;
      default:
        return CWAL_RC_RANGE;
    }
  }
  if(out) *out = (cwal_int_t)i * sign;
  return 0;
}

int whcl__script_parse_hex_digits( char const * digits, cwal_size_t digLen,
                           cwal_int_t * out ){
  cwal_uint_t i = 0
    /* use unsigned during parsing for guaranteed overflow behaviour */;
  char const * end = digits + digLen;
  char digit;
  assert(sizeof(cwal_size_t) >= sizeof(cwal_int_t));
  if(end <= digits) return CWAL_RC_RANGE;
  for( ; digits < end; ++digits ){
    digit = *digits;
    switch(digit){
      case '0':
      case '1': case '2': case '3': case '4': case '5':
      case '6': case '7': case '8': case '9':
        i = (i << 4) | (cwal_uint_t)(digit-'0');
        break;
      case 'a': case 'b': case 'c':
      case 'd': case 'e': case 'f':
        i = (i << 4) | (cwal_uint_t)(10 + digit -'a');
        break;
      case 'A': case 'B': case 'C':
      case 'D': case 'E': case 'F':
        i = (i << 4) | (cwal_uint_t)(10 + digit - 'A');
        break;
      case '_':
        continue;
      default:
        return CWAL_RC_RANGE;
    }
  }
  if(out) *out = (cwal_int_t)i;
  return 0;
}

bool whcl_stoken_parse_int( whcl_script const * const c,
                            whcl_stoken const * const t,
                            cwal_int_t * const rv ){
  char const * begin = whcl_stoken_begin(c,t);
  cwal_midsize_t len = whcl_stoken_len(t);
  int const prefix = ('-'==*begin) ? -1 : ('+'==*begin ? 1 : 0);
  bool rc = false;
  assert(whcl_stoken_end(c,t) > whcl_stoken_begin(c,t));
  if(prefix){
    ++begin;
    --len;
  }
  switch(t->ttype2){
    case TOK1_T_LiteralIntDec:
      assert(len);
      rc = 0==whcl__script_parse_decimal_digits(begin, len, rv);
      break;
    case TOK1_T_LiteralIntOct:
      assert( len > 2 /*"0o" prefix */);
      rc = 0==whcl__script_parse_octal_digits(begin+2, len-2, rv);
      break;
    case TOK1_T_LiteralIntHex:
      assert( len > 2 /*"0x" prefix */);
      rc = 0==whcl__script_parse_hex_digits( begin+2, len-2, rv);
      break;
    case TOK1_T_LiteralIntBin:
      assert( len > 2 /*"0b" prefix */);
      rc = 0==whcl__script_parse_binary_digits( begin+2, len-2, rv);
      break;
    default:
      return false;
  }
  if(rc && prefix<0 && rv) *rv = -*rv;
  return rc;
}

bool whcl_stoken_parse_double( whcl_script const * const c,
                               whcl_stoken const * t,
                               cwal_double_t * const dest ){
  if(t->ttype2 == TOK1_T_LiteralDouble){
    if(dest){
      return 0==cwal_cstr_to_double( whcl_stoken_begin(c, t),
                                     whcl_stoken_len(t), dest);
    }
    return true;
  }else{
    return false;
  }
}

static int whcl__script__hexbyte( int ch ){
  if(ch>='0' && ch<='9') return ch - '0';
  else if(ch>='a' && ch<='f') return ch - 'a' + 10;
  else if(ch>='A' && ch<='F') return ch - 'A' + 10;
  else return -1;
}

static int whcl__script__read_hex_bytes(unsigned char const *zPos,
                               unsigned int howMany,
                               unsigned int * rv ){
  unsigned int rc = 0;
  int ch1, ch2;
  unsigned int n = 0;
  assert(!(howMany%2) && "howMany must be an even number!");
  assert(zPos && rv);
  while(n<howMany){
    ch1 = *zPos ? whcl__script__hexbyte(*zPos++) : -1;
    ch2 = (ch1>=0) ? whcl__script__hexbyte(*zPos++) : -1;
    if(ch2<0) return -1;
    rc = (rc<<8) | (0xF0 & (ch1<<4)) | (0x0F & ch2);
    n += 2;
  }
  *rv = rc;
  return (int)n;
}

/**
   TODO?: return a positive value (the length of the unescaped string)
   on success and the negated byte offset of an error?
*/
int whcl__script_unescape_string( whcl_script const * const c,
                          char const * _begin,
                          char const * _end,
                          cwal_buffer * const dest ){
  cwal_size_t sz;
  unsigned char const * begin = (unsigned char const*)_begin;
  unsigned char const * end = (unsigned char const*)_end;
  unsigned char const * p = begin;
  unsigned char * out;
  int check;
  cwal_size_t oldUsed;
  cwal_engine * const e = c->ec;
  if(!e || !begin || !end || !dest) return CWAL_RC_MISUSE;
  else if(end<begin) return CWAL_RC_RANGE;
  oldUsed = dest->used;
  sz = end - begin + 1 /*NUL*/;
  check = cwal_buffer_reserve( e, dest, sz + oldUsed );
  if(check) return check;
  out = dest->mem + oldUsed;
  for( ; p < end; ++p ){
    switch(*p){
      case '\\':
        if(end==p+1){
          /* stray slash at the end of the string. */
          *(out++) = '\\';
          break;
        }
        switch(*++p){
#if 0
          case 0:
            *(out++) = '\0';
            break;
#endif
          case '0': *(out++) = 0; break;
          case 'b': *(out++) = '\b'; break;
          case 't': *(out++) = '\t'; break;
          case 'n': *(out++) = '\n'; break;
          case 'r': *(out++) = '\r'; break;
          case 'f': *(out++) = '\f'; break;
          case 'v': *(out++) = '\v'; break;
          case 'u':
          case 'U':{
            /* \uXXXX and \UXXXXXXXX */
            unsigned char uCount = 0;
            unsigned int uChar = 0;
            const unsigned ulen = 'u'==*p ? 4U : 8U;
            uCount = whcl__script__read_hex_bytes(p+1, ulen, &uChar);
            if(uCount != ulen) return CWAL_RC_RANGE;
            assert(0 == (uCount % 2));
            check = cwal_utf8_char_to_cstr(uChar, out, uCount);
            if(check<0){
              /*MARKER(("Invalid UTF: \\%c seq: uCount=%d, uChar=0x%08x\n",
               *p, (int)uCount, uChar));*/
              return CWAL_RC_RANGE
                /* Invalid UTF */;
            }
            out += check /* length of the char, in bytes */;
            p += uCount;
            break;
          }
#if 0
          default:
           /* strip the backslash */
            *(out++) = *p;
            break;
#else
          case '\\':
          case '[': case ']':
          case '{': case '}':
          case '(': case ')':
          case '"':
          case '\'':
            *(out++) = *p;
            break;
          default:
           /* retain the backslash. The main benefit of this is that
            libraries which take escaped strings don't have to be
            double-escaped in many cases. This "problem" was
            discovered in the POSIX regex extension, where we had to
            double-escape parens and '+' before this change. */
            *(out++) = '\\';
            *(out++) = *p;
            break;
#endif
        }
        break;
      default:
        *(out++) = *p;
        break;
    }
  }
  *out = 0;
  dest->used = out - dest->mem;
  assert(dest->used <= dest->capacity);
  return 0;
}

int whcl__script_create_value( whcl_script * const c, whcl_stoken const * const t,
                       cwal_buffer * const escBuffer,  cwal_value ** rv ){
  int rc = CWAL_RC_TYPE;
  cwal_value * v = NULL;
  cwal_engine * const e = c->ec;
#define RC if(v) rc = 0; \
  else { WHCL__WARN_OOM; rc = CWAL_RC_OOM; }(void)0
  whcl__script_err_reset(c);
  switch(t->ttype){
    case TOK1_T_LiteralNumber:
      switch(t->ttype2){
        case TOK1_T_LiteralIntBin:
        case TOK1_T_LiteralIntDec:
        case TOK1_T_LiteralIntHex:
        case TOK1_T_LiteralIntOct:{
          cwal_int_t dd = 0;
          bool const check = whcl_stoken_parse_int(c, t, &dd);
          assert(check &&
                 "If this is false, then there's a mismatch "
                 "between the tokenizer and converter.");
          if(check){/*avoid unused var warning in non-debug builds*/}
          v = cwal_new_integer(e, dd);
          RC;
          break;
        }
        case TOK1_T_LiteralDouble:{
          cwal_double_t dd = 0;
          whcl_stoken_parse_double(c, t, &dd);
          v = cwal_new_double(e, dd);
          RC;
          break;
        }
        default:
          whcl__fatal(CWAL_RC_CANNOT_HAPPEN,
                      "Invalid TOK1_T_LiteralNumber mapping.");
          break/*not reached*/;
      }
      break;
    case TOK1_T_QuotedString:{
      cwal_buffer eb = cwal_buffer_empty;
      cwal_size_t const oldBufUsed = escBuffer ? escBuffer->used : 0;
      cwal_buffer * const escapeBuf =
        cwal_buffer_reuse(escBuffer ? escBuffer : &eb);
      rc = whcl__script_unescape_string(c, whcl_stoken_begin2(c, t, true),
                                whcl_stoken_end2(c, t, true),
                                escapeBuf );
      if(rc){
        rc = whcl__script_err(c, CWAL_RC_RANGE==rc ? CWAL_SCR_SYNTAX : rc,
                         /*      allow catch() to block ^^^^^ this! */
                         "Unescaping string failed with rc=%s, "
                         "likely due to non-UTF8 content "
                         "or an unknown \\Uxxxxxxxx sequence.",
                         cwal_rc_cstr(rc))
          /**
             20191220: whether or not this should trigger
             whcl__script_throw_toker() (exception) or whcl__script_err_toker() (fatal)
             is debatable. It seems unfortunate that an invalid \U
             should outright kill a script, especially if it arrives
             via input from outside the script. Treating it as a
             non-exception using the code CWAL_SCR_SYNTAX, we enable
             catch{...} to optionally downgrade it to an exception.
          */;
        goto buffer_cleanup;
      }
      /*MARKER("STRING: [%.*s]\n", (int)(escapeBuf->used - oldUsed),
        (char const *)escapeBuf->mem+oldUsed);*/
      assert(escapeBuf->mem ? (0 == escapeBuf->mem[escapeBuf->used]) : 1);
      if(escapeBuf==&eb){
        v = cwal_buffer_to_zstring_value(e, &eb);
      }else{
        v = cwal_new_string_value(e, escapeBuf->used
                                  ? (char const *)escapeBuf->mem : "",
                                  escapeBuf->used);
      }
      RC;
      /* whcl__dump_val(v,"string literal"); */
      buffer_cleanup:
      if(&eb == escapeBuf) cwal_buffer_clear(e, escapeBuf);
      else{
        escapeBuf->used = oldBufUsed;
        if(escapeBuf->mem) escapeBuf->mem[oldBufUsed] = 0;
      }
      break;
    }
    case TOK1_T_BraceGroup:
    case TOK1_T_CallBlock:
    case TOK1_T_Heredoc:
    case TOK1_T_ParenGroup:
    case TOK1_T_SquigglyGroup:{
      /* Use the adjusted inner area as a string... */
      cwal_size_t const len = whcl_stoken_len2(t, true);
      char const * begin = whcl_stoken_begin2(c, t, true);
      char const * end = whcl_stoken_end2(c, t, true);
      assert(begin); assert(end); assert(begin <= end);
      v = cwal_new_string_value(e, begin, len);
      RC;
      break;
    }
    default:{
      /* Use the raw token bytes as a string... */
      cwal_size_t const len = whcl_stoken_len2(t, false);
      char const * begin = whcl_stoken_begin2(c, t, false);
      char const * end = whcl_stoken_end2(c, t, false);
      assert(begin); assert(end); assert(begin <= end);
      v = cwal_new_string_value(e, begin, len);
      RC;
      break;
    }
  }
  if(!rc) {
    assert(v);
    *rv = v;
  }else{
    assert(!v);
    if(CWAL_RC_OOM==rc){
      WHCL__WARN_OOM;
    }
  }
  return rc;
#undef RC
}

void whcl__dump_stoken(whcl_script const * const c,
                      whcl_stoken const * t,
                      char const * lbl, char const * file, int line){
  if(!t){
    MARKER(("%s:%d (whcl_stoken*)NULL %s\n", file, line, lbl));
    return;
  }
  cwal_midsize_t tLen = 0;
  char const * ts = whcl_stoken_cstr(c, t, &tLen, false);
  char numTtype2[100] = {0};
  char const * zTtype2 = NULL;
  if(t->ttype2){
    zTtype2 = tok1_t_cstr(t->ttype2);
    if(!zTtype2){
      snprintf(numTtype2, sizeof(numTtype2)-1, " (#%d)", (int)t->ttype2);
    }else{
      snprintf(numTtype2, sizeof(numTtype2)-1, " (#%s)", zTtype2);
    }
    zTtype2 = numTtype2;
  }
  MARKER(("%s:%d #%d=>%d (i%d, s%d):%s:%s@%d,%d: %s%s %.*s\n",
          file, line, (int)t->id, (int)t->nextId,
          (int)t->innerId, (int)t->subscriptId,
          lbl,
          c->name, (int)t->line, (int)t->column,
          tok1_t_cstr(t->ttype),
          zTtype2,
          (int)tLen, ts));
}

int whcl__script_ttype_is_eox( int ttype ){
  switch(ttype){
    case TOK1_T_EOF:
    case TOK1_T_EOX:
    case TOK1_T_EOL:
    case TOK1_T_Semicolon:
      return ttype;
    default:
      return 0;
  }
}

int whcl__script_is_eox( whcl_script const * ct ){
  whcl_stoken const * const t = whcl__script_token(ct);
  return t && whcl__script_ttype_is_eox(t->ttype)
    ? t->ttype : 0;
}

int whcl_stoken_is_eox( whcl_stoken const * const t ){
  return t ? whcl__script_ttype_is_eox(t->ttype) : TOK1_T_EOF;
}

bool whcl_script_linecol(whcl_script const * const src,
                         whcl_linecol_t * line,
                         whcl_linecol_t * col) {
  whcl_stoken const * t = whcl__script_at(src, 1);
  if(line) *line = t ? t->line : 0;
  if(col) *col = t ? t->column : 0;
  return !!t;
}

void whcl_script_renumber(whcl_script * const src,
                          whcl_linecol_t line,
                          whcl_linecol_t col) {
  if(!src->chain) return;
  whcl_linecol_t const l = (line ? line : 1);
  whcl_linecol_t origLine = 0;
  whcl_linecol_t origCol = 0;
  whcl_stoken_id pos = 1;
  whcl_stoken * t;
  int stage = 0;
  for( ; (t = whcl__script_at_nc(src, pos++)); ){
    switch(stage){
      case 0: /* First token */
        origCol = t->column;
        origLine = t->line;
        t->line = l;
        t->column = col;
        stage = 1;
        continue;
      case 1:
        if(t->line == origLine){
          /* Still on the first line */
          t->line = t->line - origLine + l;
          t->column = t->column - origCol + col;
          continue;
        }
        stage = 2;
        CWAL_SWITCH_FALL_THROUGH;
      case 2: /* Second and subsequent lines */
        t->line = t->line - origLine + l;
        continue;
    }
  }
}

int whcl__script_slice( whcl_script const * const src,
                        whcl_stoken const * from,
                        whcl_stoken const * to,
                        whcl_script ** pOut ){
  int rc = 0;
  whcl_script * dest = *pOut;
  if(!from) from = whcl__script_at(src, 1);
  if(!to) to = whcl__script_at(src, src->chainLength);
  else if(to->innerId || to->subscriptId){
    /* We have to expand the range to include all sub-tokens. */
    assert(to->nextId && "We're expecting at least an EOF token.");
    to = whcl__script_at(src, to->nextId-1);
  }
  if(!dest){
    dest = whcl__script_alloc(src->ec);
    if(!dest){WHCL__WARN_OOM; return CWAL_RC_OOM;}
  }else{
    whcl__script_finalize(*pOut);
  }
  assert(from->id<to->id);
  assert(whcl__script_at(src, from->id)==from);
  assert(whcl__script_at(src, to->id)==to);
  assert(!dest->ec && "dest must be clean when passed in.");
  whcl_stoken_id const offTid = from->id-1;
  uint32_t const offB = from->begin;
  char const * codeBegin = whcl_stoken_begin(src, from);
  char const * codeEnd = whcl_stoken_end(src, to);
  dest->ec = src->ec;
  whcl__script__toker_chain_reserve(dest, to->id - from->id + 2);
  dest->ownSrc = dest->chain
    ? cwal_printf_cstr(src->ec, "%.*s",
                       (int)(codeEnd - codeBegin),
                       codeBegin)
    : NULL;
  if(!dest->ownSrc){
    WHCL__WARN_OOM;
    rc = CWAL_RC_OOM;
    goto end;
  }else if(src->name){
    rc = whcl_script_name_set(dest, src->name, -1);
    if(rc) goto end;
  }
  dest->parent = dest->top = NULL;
  dest->flags = src->flags;
  dest->token = dest->pbToken = dest->errToken = dest->nextToken = 0;
  dest->begin = dest->ownSrc;
  dest->end = dest->begin + (to->begin + to->length - from->begin);
  whcl_stoken * const dEof = dest->chain + dest->chainCapacity - 1;
  *dEof = whcl_stoken__empty;
  dEof->ttype = TOK1_T_EOF;
  dEof->line = 0;
  dEof->id = dest->chainCapacity;
  dest->chainLength = dest->chainCapacity;
  whcl_stoken_id dId = 1;
  whcl_stoken * dat = whcl__script_at_nc(dest, dId);
  for(whcl_stoken const * at = from; at && at->id<=to->id;
      at = whcl__script_at(src, at->id+1),
      dat = whcl__script_at_nc(dest, ++dId)){
    *dat = *at;
    dat->id -= offTid;
    dat->begin -= offB;
    if(dat->nextId){
      dat->nextId = dat->nextId>to->id
        ? dEof->id : dat->nextId - offTid;
    }
    if(dat->innerId){
      dat->innerId = dat->innerId>to->id
        ? dEof->id : dat->innerId - offTid;
    }
    if(dat->subscriptId){
      dat->subscriptId = dat->subscriptId>to->id
        ? dEof->id : dat->subscriptId - offTid;
    }
    //whcl__dump_stok(src, at, "Cloned from");
    //whcl__dump_stok(dest, dat, "Cloned to");
  }
  whcl__script_rewind(dest);
  end:
  if(rc){
    if(*pOut == dest) whcl__script_finalize(dest);
    else whcl_script_free(dest);
  }
  else if(*pOut != dest){
    *pOut = dest;
  }
  return rc;
}

bool whcl_stoken_has_inner_content( whcl_script const * const ct,
                                   whcl_stoken const * const tok){
  if(!tok->innerId) return false;
  return !whcl_stoken_is_eof(whcl__script_at(ct, tok->innerId));
}

int whcl_script_to_jsonable( whcl_script * const scr,
                             cwal_value ** rv ){
  int rc = CWAL_RC_OOM;
  cwal_value * const o = cwal_new_object_value(scr->ec);
  if(!o) goto end;
  cwal_value * v = cwal_new_string_value(scr->ec, scr->begin,
                                         (cwal_size_t)(scr->end - scr->begin));
  if(!v){ rc = CWAL_RC_OOM; goto end; }
  cwal_ref(v);
  rc = cwal_prop_set(o, "script", 6, v);
  cwal_unref(v);
  if(scr->name){
    v = cwal_new_string_value(scr->ec, scr->name,
                              cwal_strlen(scr->name));
    if(!v){rc = CWAL_RC_OOM; goto end;}
    cwal_ref(v);
    rc = cwal_prop_set(o, "name", 4, v);
    cwal_unref(v);
  }
  end:
  if(rc){
    if(CWAL_RC_OOM==rc){WHCL__WARN_OOM;}
    if(o) cwal_refunref(o);
  }else{
    *rv = o;
  }
  return rc;
}


#define THIS_SCRIPT \
  whcl_script * const scr = whcl_value_get_script(args->self); \
  if(!scr){ \
    return cwal_cb_throw(args, CWAL_RC_TYPE, \
                         "'this' is not (or is no longer) " \
                         "a whcl_script. (%s())", __func__); \
  }

static int whcl__cb_script_run( cwal_callback_args const * args,
                                cwal_value ** rv ){
  if(args->argc > 0){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "Unhandled extra arguments.");
  }
  THIS_SCRIPT;
  return whcl_script_eval(whcl__script_rewind(scr), 0, rv);
}

static int whcl__cb_script_name( cwal_callback_args const * args,
                                 cwal_value ** rv ){
  THIS_SCRIPT;
  if(args->argc){ /* setter */
    cwal_size_t slen = 0;
    cwal_value * const arg = args->argv[0];
    char const * s = cwal_value_get_cstr(arg, &slen);
    if(s){
      int const rc = whcl_script_name_set(scr, s, slen);
      if(0==rc) *rv = arg;
      return rc;
    }else{
      return cwal_cb_throw(args, CWAL_RC_TYPE,
                           "Expecting a string argument.");
    }
  }else{ /* getter */
    *rv = scr->name
      ? cwal_new_string_value(args->engine,
                              scr->name, cwal_strlen(scr->name))
      : cwal_value_undefined();
    return *rv ? 0 : CWAL_RC_OOM;
  }
}

static int whcl__cb_script_destroy( cwal_callback_args const * args,
                                    cwal_value ** rv ){
  THIS_SCRIPT;
  cwal_native * const n =
    cwal_value_native_part(args->engine, args->self, &whcl__script_empty);
  assert(n);
  cwal_native_clear( n, true );
  *rv = cwal_value_undefined();
  return 0;
}

static int whcl__cb_script_code( cwal_callback_args const * args,
                                 cwal_value ** rv ){
  THIS_SCRIPT;
  /* It would be nice if we could use an x-string here to avoid a
     copy, but we cannot be guaranteed that this object would outlive
     it. */
  *rv = cwal_new_string_value(args->engine, scr->begin,
                              (cwal_midsize_t)(scr->end - scr->begin));
  return *rv ? 0 : CWAL_RC_OOM;
}

/**
   Internal helper to extract line/column values from args, starting
   at the given args->argv index. If argNdx is out of range, no args
   are processed. If args->argc>argNdx+1 then both the line and column
   are parsed and validated. Either of the line/col output args may be NULL, in which case
   only non-NULL args will be updated but the arguments will still be
   read in the order line/column. That is, passing a NULL line does not
   change the expected args->argv index of the column.

   If a given arg is not processed, the corresponding output argument
   retains its initial value.

   Throws an exception if it receives numbers which are out of range,
   else it returns 0.

   If it processes no args then *line and *col (if not NULL) are set
   to 0.
*/
static int whcl__script_linecol_args( cwal_callback_args const * args,
                                     uint16_t argNdx, cwal_int_t * line,
                                     cwal_int_t * col){
  cwal_int_t ln = line ? *line : 0;
  cwal_int_t c = col ? *col : 0;
  if(args->argc > argNdx){
    ln = line ? cwal_value_get_integer(args->argv[argNdx]) : 0;
    if(col && args->argc > argNdx+1) c = cwal_value_get_integer(args->argv[argNdx+1]);
    if( (cwal_int_t)((whcl_linecol_t)ln) != ln
        || (cwal_int_t)((whcl_linecol_t)c) != c ){
      return cwal_cb_throw(args, CWAL_RC_RANGE,
                           "Numeric overflow in argument. "
                           "The numeric limit is %u.",
                           (unsigned)((whcl_linecol_t)-1));
    }
  }
  if(line) *line = ln;
  if(col) *col = c;
  return 0;
}

static int whcl__cb_script_renumber( cwal_callback_args const * args,
                                     cwal_value ** rv ){
  THIS_SCRIPT;
  cwal_int_t ln = 1;
  cwal_int_t col = 0;
  int const rc = whcl__script_linecol_args(args, 0, &ln, &col);
  if(0==rc){
    whcl_script_renumber(scr, (whcl_linecol_t)ln, (whcl_linecol_t)col);
    *rv = args->self;
  }
  return rc;
}

static int whcl__cb_script_linecol_impl( bool isLine,
                                         cwal_callback_args const * args,
                                         cwal_value ** rv ){
  THIS_SCRIPT;
  whcl_stoken const * const st = whcl__script_at(scr, 1);
  assert(st);
  if(0==args->argc){
    *rv = cwal_new_integer(args->engine,
                           (cwal_int_t)(isLine ? st->line : st->column));
    return *rv ? 0 : CWAL_RC_OOM;
  }else{
    cwal_int_t ln = 1;
    cwal_int_t c = 0;
    int const rc = whcl__script_linecol_args(args, 0, &ln, &c);
    if(0==rc){
      if(isLine){
        if(args->argc==1) c = (cwal_int_t)st->column;
      }else{
        cwal_int_t const x = ln;
        ln = (args->argc==1) ? (cwal_int_t)st->line : c;
        c = x;
      }
      whcl_script_renumber(scr, (whcl_linecol_t)ln, (whcl_linecol_t)c);
      *rv = args->self;
    }
    return rc;
  }
}

static int whcl__cb_script_line(cwal_callback_args const * args,
                                cwal_value ** rv ){
  return whcl__cb_script_linecol_impl(true, args, rv);
}

static int whcl__cb_script_column(cwal_callback_args const * args,
                                cwal_value ** rv ){
  return whcl__cb_script_linecol_impl(false, args, rv);
}


static int whcl__cb_script_to_jsonable( cwal_callback_args const * args,
                                        cwal_value ** rv ){
  whcl_script * const scr = whcl_value_get_script(args->self);
  if(!scr){
    whcl_engine * const el = whcl_engine_from_args(args);
    cwal_value * const proto = whcl__script_prototype(el);
    if(proto){
      /* Kludge to keep an exception from being thrown when
         to-json'ing the prototype object */
      *rv = proto;
      return 0;
    }
    return cwal_cb_throw(args, CWAL_RC_TYPE,
                         "'this' is not (or is no longer) "
                         "a whcl_script. (%s())", __func__);
  }
  return whcl_script_to_jsonable(scr, rv);
}

static int whcl__cb_script_dump( cwal_callback_args const * args,
                                 cwal_value ** rv ){
  THIS_SCRIPT;
  uint16_t argNdx = 0;
  uint32_t dFlags = 0;
  cwal_size_t nFlag = 0;
  while( whcl_arg_has_flag(args, &argNdx, NULL, &nFlag) ){
    cwal_value const * const arg = args->argv[argNdx-1];
    if(2==nFlag && whcl_val_is_flag(arg, "-v", 2)){
      dFlags |= WHCL_DUMP_TOKENS_VERBOSE;
      continue;
    }else if(2==nFlag && whcl_val_is_flag(arg, "-m", 2)){
      dFlags |= WHCL_DUMP_TOKENS_METRICS;
      continue;
    }else if(4==nFlag && whcl_val_is_flag(arg, "-eof", 4)){
      dFlags |= WHCL_DUMP_TOKENS_EOFS;
      continue;
    }
    --argNdx;
    break;
  }
  whcl_engine * const el = whcl_engine_from_args(args);
  whcl_dump_tokens(el, scr, dFlags);
  *rv = args->self;
  return 0;
}

#if 0
static int whcl__cb_script_file_read( cwal_callback_args const * args,
                                      cwal_value ** rv ){
  uint16_t argNdx = 0;
  char const * fname = (args->argc > argNdx)
    ? cwal_value_get_cstr(args->argv[argNdx], NULL)
    : NULL;
  if(!fname){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "Expecting a string (filename) argument.");
  }
  whcl_engine * const el = whcl_engine_from_args(args);
  whcl_script * scr = NULL;
  cwal_value * vscr = NULL;
  int rc = whcl_compile_file(el, &scr, fname);
  if(0==rc){
    rc = whcl__script_to_value(scr, true, &vscr);
    if(rc){
      whcl_script_free(scr);
    }
  }
  end:
  if(0==rc){
    *rv = vscr;
  }
  return rc;
}
#endif

#undef THIS_SCRIPT

/**
   Script constructor:

   new Script [name] [line [col]] source-code
*/
static int whcl__cb_script_ctor( cwal_callback_args const * args,
                                 cwal_value ** rv ){
  char const * zFlag = NULL;
  cwal_size_t nFlag = 0;
  uint16_t argNdx = 0;
  bool useFile = false;
  while( whcl_arg_has_flag(args, &argNdx, &zFlag, &nFlag) ){
    if(2==nFlag && 0==memcmp(zFlag, "-f", nFlag)){
      useFile = true;
      continue;
    }
    --argNdx;
    break;
  }
  uint16_t const argc = args->argc - argNdx;
  if(argc < 1 || argc > 4){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "Expecting ([-f] [name] source-code) arg(s).");
  }
  int rc = 0;
  whcl_script * sc = NULL;
  char const * name = NULL;
  cwal_int_t line = 0;
  cwal_int_t col = 0;
  if(argc>1){
    name = cwal_value_get_cstr(args->argv[argNdx++], NULL);
    if(!name){
      return cwal_cb_throw(args, CWAL_RC_TYPE,
                           "Name must be a string.");
    }
  }
  if(argc>2){
    rc = whcl__script_linecol_args(args, argNdx, &line,
                                  argc>3 ? &col : NULL);
    if(rc) return rc;
    argNdx += 1 + (argc>3);
  }
  whcl_engine * const el = whcl_engine_from_args(args);
  cwal_value * const vsrc = args->argv[argNdx];
  cwal_size_t nSrc = 0;
  char const * src = cwal_value_get_cstr(vsrc, &nSrc);
  char const * compileName = name ? name : "Script constructor";
  if(useFile && !name && cwal_value_is_string(vsrc)){
    /* Treat source as a filename for the -f flag and use that
       name for the script by default. */
    compileName = src;
  }  
  cwal_engine_error_reset(args->engine);
  if(cwal_value_is_buffer(vsrc)){
    rc = whcl_compile_buffer_take(el, &sc, compileName,
                                  cwal_value_get_buffer(vsrc));
  }else if(src){
    rc = useFile
      ? whcl_compile_file(el, &sc, src, name)
      : whcl_compile(el, &sc, compileName, src, (cwal_int_t)nSrc);
  }else{
    return cwal_cb_throw(args, CWAL_RC_TYPE,
                         "Source code must be a string or buffer.");
  }
  check_rc:
  switch(rc){
    case 0:{
      cwal_value * xrv = NULL;
      if(line>0){
        whcl_script_renumber(sc, (whcl_linecol_t)line,
                             (whcl_linecol_t)col);
      }
      rc = whcl__script_to_value(sc, true, &xrv);
      if(rc) { whcl_script_free(sc);}
      else if(useFile && name){
        /* (-f NAME FILE) uses NAME as the script name */
        rc = whcl_script_name_set(sc, name, -1);
      }else if(compileName){
        rc = whcl_script_name_set(sc, compileName, -1);
      }
      if(rc){
        cwal_refunref(xrv);
        sc = NULL;
        goto check_rc;
      }
      *rv = xrv;
      break;
    }
    case CWAL_RC_EXCEPTION:
    case CWAL_RC_OOM:
      whcl_script_free(sc);
      break;
    default:
      if(cwal_engine_error_get(args->engine, NULL, NULL)){
        rc = cwal_error_throw(args->engine, NULL, NULL, 0, 0);
      }
      whcl_script_free(sc);
      break;
  }
  return rc;
}


cwal_value * whcl__script_prototype(whcl_engine * const el){
  int rc;
  char const * pName = "Script";
  cwal_value * proto = whcl__prototype_stashed(el, pName);
  if(proto) return proto;
  cwal_value * ctor;
  proto = cwal_new_object_value(el->ec);
  if(!proto){WHCL__WARN_OOM; rc = CWAL_RC_OOM; goto end;}
  cwal_ref(proto);
  ctor = cwal_new_function_value(el->ec, whcl__cb_script_ctor, NULL,
                                 NULL, NULL);
  if(!ctor){
    WHCL__WARN_OOM;
    rc = CWAL_RC_OOM;
    goto end;
  }
  cwal_value_prototype_set(ctor, proto);
  cwal_ref(ctor);
  rc = whcl__install_into_whcl(el, pName, ctor);
  cwal_unref(ctor) /* on success ^^^, the core now owns a ref */;
  if(rc) goto end;

  rc = whcl__prototype_stash(el, pName, proto);
  if(0==rc){
    whcl_func_def const funcs[] = {
      WHCL_FUNC2("column", whcl__cb_script_column),
      WHCL_FUNC2("destroy", whcl__cb_script_destroy),
      WHCL_FUNC2("dump", whcl__cb_script_dump),
      WHCL_FUNC2("line", whcl__cb_script_line),
      WHCL_FUNC2("name", whcl__cb_script_name),
      WHCL_FUNC2("renumber", whcl__cb_script_renumber),
      WHCL_FUNC2_XSYM("run", whcl__cb_script_run),
      WHCL_FUNC2("source-code", whcl__cb_script_code),
      WHCL_FUNC2("to-jsonable", whcl__cb_script_to_jsonable),
      whcl_func_def_empty_m
    };
    rc = whcl_install_functions(el, proto, funcs, 0);
    if(0==rc) rc = whcl_install_typename(el, proto, "Script");
#if 0
    if(0==rc){
      char const * src =
        "set X.to-jsonable proc {} {"
        "return object "
        "script [this.source-code] "
        "name [this.name]"
        "}";
      rc = whcl_eval_cstr_with_var(el, "X", proto,
                                   "Script class init",
                                   src, -1, NULL);
    }      
#endif
  }
  end:
  cwal_unref(proto)/*on success it was stashed*/;
  return rc ? NULL : proto;
}

static void whcl__script_finalizer(cwal_engine * e, void * m){
  if(e){/*unused*/}
  whcl_script_free( (whcl_script*)m );
}

whcl_script * whcl_value_get_script(cwal_value * const v){
  cwal_engine * const e = cwal_value_engine(v);
  cwal_native * const n = e
    ? cwal_value_native_part(e, v, &whcl__script_empty)
    : NULL;
  return n
    ? (whcl_script*)cwal_native_get(n, &whcl__script_empty)
    : NULL;
}

whcl_script * whcl__value_get_script(whcl_engine const * const el,
                                     cwal_value * const v){
  return (el->cache.installAPI & WHCL__INSTALL_API_script)
    ? whcl_value_get_script(v)
    : NULL;
}
  

int whcl__script_to_value(whcl_script * const scr, bool withFinalizer,
                         cwal_value **rv){
  int rc = CWAL_RC_OOM;
  cwal_value * const v =
    cwal_new_native_value(scr->ec, scr, withFinalizer
                          ? whcl__script_finalizer : NULL,
                          &whcl__script_empty);
  if(v){
    cwal_ref(v);
    whcl_engine * const el = whcl_engine_from_state(scr->ec);
    cwal_value * const proto = whcl__script_prototype(el);
    if(!proto){
      WHCL__WARN_OOM;
      cwal_unref(v);
    }else{
      cwal_value_prototype_set(v, proto);
      *rv = v;
      rc = 0;
    }
  }
  else {WHCL__WARN_OOM;}
  return rc;
}


int whcl_script_name_set(whcl_script * const scr,
                         char const * s, cwal_int_t n){
  if(s){
    char * x = whcl__strdup(scr->ec, s, n);
    if(!x){WHCL__WARN_OOM; return CWAL_RC_OOM;}
    cwal_free(scr->ec, scr->ownName);
    scr->name = scr->ownName = x;
    return 0;
  }else{
    cwal_free(scr->ec, scr->ownName);
    scr->name = scr->ownName = NULL;
    return 0;
  }
}


#undef MARKER
#undef NO_ID
#undef whcl__script_eof_id
#undef whcl__script_top
#undef cwal_engine_adjust_client_mem
