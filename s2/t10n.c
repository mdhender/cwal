/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

#include "wh/cwal/s2/s2_config.h" /* only for the is-this-windows determination. :/ */
#include "wh/cwal/s2/s2_t10n.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#if 1
#include <stdio.h>
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:\t",__FILE__,__LINE__); if(1) printf pfexp
#else
#define MARKER(pfexp) (void)0
#endif

/* const s2_byte_range s2_byte_range_empty = s2_byte_range_empty_m; */
const s2_ptoker s2_ptoker_empty = s2_ptoker_empty_m;
const s2_ptoken s2_ptoken_empty = s2_ptoken_empty_m;
const s2_path_toker s2_path_toker_empty = s2_path_toker_empty_m;

/**
   S2_T10N_LCCACHE: set to 0 to disable the line-counting cache. It
   appears to work, saving more than half of the CPU cycles in the s2
   unit tests, but is not yet battle-hardened (or even optimized).

   Sidebar: those tests are essentially a pathological case, as they
   intentionally throw many exceptions in their testing duties,
   exceptions being a major culprit in the line-counting rabbit hole,
   along with function definitions.
*/
#define S2_T10N_LCCACHE 1
#if S2_T10N_LCCACHE
#  define S2_T10N_LCCACHE_SLOT 0
/*
  S2_T10N_LCCACHE_SLOT == whether or not to use a predictable cache
  slot placement algorithm which always places the same pos in the
  same slot and orders slots by position.

  Some test metrics (20191229)...

  "Ir" values as reported by callgrind (in debug builds):

  cache size 20: 1=328M w/ 457 hits, 0=325M w/ 458 hits
  cache size 13: 1=330M w/ 457 hits, 0=324.9M w/ 458 hits
  cache size 10: 1=331M w/ 457 hits, 0=324.8M w/ 458 hits
  cache size 9:  1=331M w/ 457 hits, 0=324.7M w/ 458 hits

  (Sidebar: those Ir values drop by about a 3rd in non-debug builds:
  ~330M --> ~195M.)

  Summary: this approach doesn't perform quite as well, presumably
  because closely-located entries take the same slot, and thus can't
  benefit from potentially closer matches, especially on a large
  tokenizer (where each slot spans a larger space).

  Maybe if we spread out the entries, instead of ordering them
  strictly by position (use modulo instead of division to place
  them)...

  cache size 20: 1=345M w/ 447 hits
  cache size 13: 1=345M w/ 451 hits
  cache size 10: 1=340M w/ 446 hits

  (Noting that the fallback handler when a hit is not found on the
  first try is/was not explicitly optimized for this case: it would
  essentially need to degrade to the same loop the circular queue
  does.)

  Summary: simply using a small circular cache, discarding older
  entries as we loop around, performs better in every regard.
*/
static const int s2_ptoker_lccache_size = sizeof(s2_ptoker_empty._lcCache.lines)
  /sizeof(s2_ptoker_empty._lcCache.lines[0]);
#else
#  define S2_T10N_LCCACHE_SLOT 0
#endif

#if 0 && S2_T10N_LCCACHE
#  define LCMARKER MARKER
#else
#  define LCMARKER(PFEXP) (void)0
#endif

#if S2_T10N_LCCACHE && S2_T10N_LCCACHE_SLOT
#  if 1
/* Only a miniscule performance improvement. */
#    define s2_ptoker_lc_slot(PT,POS) (PT)->_lcCache.slotSize ? (int)(POS - (PT)->begin) / (PT)->_lcCache.slotSize : 0
#  else
/*inline (not C89)*/ static int s2_ptoker_lc_slot(s2_ptoker const * pt, char const * pos){
  assert(pos >= pt->begin);
  assert(pos < pt->end);
#    if 1
  return pt->_lcCache.slotSize ? (int)(pos - pt->begin) / pt->_lcCache.slotSize : 0;
#    else
  /* Performs worse, but the fallback lookup is not optimized for this case. */
  return pt->_lcCache.slotSize ? (int)(pos - pt->begin) % s2_ptoker_lccache_size : 0;
#    endif
}
#  endif
#endif

#if S2_T10N_LCCACHE
static s2_ptoker_lccache_entry const *
s2_ptoker_lc_search( s2_ptoker const * pt, char const * pos ){
  if(pos>=pt->begin && pos<pt->end){
    s2_ptoker_lccache const * const lcc = &pt->_lcCache;
#if S2_T10N_LCCACHE_SLOT
    int slot = s2_ptoker_lc_slot(pt, pos);
    assert(slot < s2_ptoker_lccache_size);
    while( slot >= 0 ){
      s2_ptoker_lccache_entry const * const rc
        = (s2_ptoker_lccache_entry const *)&lcc->lines[slot];
      if(rc->pos && rc->pos <= pos){
        LCMARKER(("%p Search slot %d, possible hit for %d, %d\n",
                  (void const *)pt, slot, rc->line, rc->col));
        return rc;
      }
      --slot;
    }
    return 0;
#else
    s2_ptoker_lccache_entry const * rc = 0;
    int i;
    for( i = 0; i < s2_ptoker_lccache_size; ++i ){
      s2_ptoker_lccache_entry const * const m = (s2_ptoker_lccache_entry const *)&lcc->lines[i];
      if(!m->pos) break;
      assert(m->pos>=pt->begin && m->pos<pt->end);
      if(m->pos<=pos){
        if(!rc || rc->pos<m->pos){
          rc = m;
          LCMARKER(("%p Search slot %d, possible hit for %d, %d\n",
                    (void const *)pt, i, rc->line, rc->col));
          if(m->pos==pos) break;
        }
      }
    }
    return rc;
#endif
  }else{
    return 0;
  }
}
#else
#  define s2_ptoker_lc_search(X,Y) 0
#endif

#if S2_T10N_LCCACHE
static void s2_ptoker_lc_cache( s2_ptoker const * pt, char const * pos, int line, int col ){
  if(pos>=pt->begin && pos<pt->end){
    s2_ptoker_lccache * lcc = (s2_ptoker_lccache *)&pt->_lcCache;
    static int once = 0;
    s2_ptoker_lccache_entry * m;
#if S2_T10N_LCCACHE_SLOT
    int const slot = s2_ptoker_lc_slot(pt, pos);
    assert(slot < s2_ptoker_lccache_size);
    m = (s2_ptoker_lccache_entry *)&lcc->lines[slot];
    /*if(m->pos && pos>=m->pos) return; wow, this adds 37M instructions
      in these tests!*/
#else
    if(lcc->cursor==s2_ptoker_lccache_size){
      lcc->cursor = 0;
      /*
        For now, rather than do something clever, we'll just wrap
        around, under the thinking that we're far more likely to hit
        the counter for more recently-visited positions than older
        ones.

        With a large enough cache size (60 entries), when tested against
        the current (20191228) s2 amalgamated unit tests, this literally,...

        1) Cuts the wall-clock time execution of the tests by *HALF*.

        2) Reduces line-counting from the single most CPU-intensive
        operation (~56% of CPU instructions) to something like 1.8% of
        the instructions.

        3) Cuts the overall number of CPU instructions by more than
        half: from ~730-ish million to ~330M (values reported by
        callgrind).

        What i'd *like* to do is divvy the cache up into slots and
        pack cache entries into slots based on their position within
        the tokenizer, so that we can quickly (O(1)), based on the
        search pos, figure out which slot to use, but my brain
        apparently isn't yet big enough to get that math right.
        Update: tried that but it provides far fewer cache hits and
        (curiously) more total CPU instructions (382M vs 324M in the
        same test run. The problem seems to be that the grouping of
        cache slots can easily lead to nearby neighbors not being
        considered as near hits.
      */
    }
    m = (s2_ptoker_lccache_entry *)&lcc->lines[lcc->cursor];
#endif
    if(!once++){
      LCMARKER(("sizeof(s2_ptoker_lccache)=%d\n",(int)sizeof(s2_ptoker_lccache)));
    }
    m->line = line;
    m->col = col;
    m->pos = pos;
#if S2_T10N_LCCACHE_SLOT
    LCMARKER(("%p Cached slot %d @ %d, %d\n", (void const *)pt, slot, m->line, m->col));
#else
    LCMARKER(("%p Cached slot %d @ %d, %d\n", (void const *)pt, lcc->cursor, m->line, m->col));
    ++lcc->cursor;
#endif
  }
}
#endif

int s2_ptoker_init_v2( cwal_engine * e, s2_ptoker * t, char const * src, cwal_int_t len,
                       uint32_t flags ){
  if(!t||!src) return CWAL_RC_MISUSE;
  else{
    *t = s2_ptoker_empty;
    t->e = e;
    t->flags = flags;
    /*memset(&t->_lcCache, 0, sizeof(t->_lcCache));*/
    if(len<0) len = cwal_strlen(src);
    t->begin = src;
    t->end = src+len;
    memset(&t->_lcCache, 0, sizeof(t->_lcCache));
    s2_ptoker_reset(t);
#if S2_T10N_LCCACHE_SLOT
    {
      int ptl = (int)len;
      int const cs = s2_ptoker_lccache_size;
      assert(0==t->_lcCache.lines[cs-1].pos);
      t->_lcCache.slotSize = (ptl + (ptl / cs)) / cs + 1;
      LCMARKER(("%p slot size = %d\n", (void const *)t, t->_lcCache.slotSize));
    }
#endif
    return 0;
  }
}

int s2_ptoker_init( s2_ptoker * t, char const * src, cwal_int_t len ){
  return s2_ptoker_init_v2( NULL, t, src, len, 0 );
}

void s2_ptoker_reset( s2_ptoker * t ){
  t->_pbToken = t->_errToken = t->token = s2_ptoken_empty;
  s2_ptoken_begin_set(&t->token, s2_ptoker_begin(t))
    /* begin w/o an end set is our internal signal that we're
       just starting off tokenization */;
  t->currentLine = 1;
  t->currentCol = 0;
}

void s2_ptoker_finalize( s2_ptoker * pt ){
  *pt = s2_ptoker_empty;
}

int s2_ptoker_sub_from_token( s2_ptoker const * parent,
                              s2_ptoken const * tok,
                              s2_ptoker * dest ){
  char const * begin;
  char const * end;
  int rc;
#if 0
  /* 20200107: this breaks stuff */
  if(!s2_ttype_is_group(tok->ttype)){
    return CWAL_RC_TYPE;
  }
#endif
  if(s2_ptoken_adjbegin(tok)){
    begin = s2_ptoken_adjbegin(tok);
    end = s2_ptoken_adjend(tok);
  }else{
    begin = s2_ptoken_begin(tok);
    end = s2_ptoken_end(tok);
  }
  if(!begin || (begin>end)) return CWAL_RC_RANGE;
  rc = s2_ptoker_init_v2( parent->e, dest, begin,
                          (cwal_int_t)(end - begin), 0 );
  if(!rc){
    dest->parent = parent;
    assert(parent->e == dest->e);
  }
  return rc;
}

int s2_ptoker_sub_from_toker( s2_ptoker const * parent, s2_ptoker * sub ){
  int const rc = s2_ptoker_sub_from_token( parent, &parent->token, sub );
  if(!rc){
    assert(sub->parent == parent);
    assert(sub->e == parent->e);
    /* There are multiple TODOs here if/when we add compiled token
       chains. */
  }
  return rc;
}

s2_ptoker const * s2_ptoker_top_parent( s2_ptoker const * t ){
  while( t && t->parent ){
    t = t->parent;
  }
  return t;
}
char const * s2_ptoker_name_first( s2_ptoker const * t, cwal_size_t * len ){
  while(t && !t->name){
    t = t->parent;
  }
  if(t && len && t->name) *len = cwal_strlen(t->name);
  return t ? t->name : 0;
}

char const * s2_ptoker_err_pos_first( s2_ptoker const * t,
                                      s2_ptoker const ** foundIn){
  while(t && !s2_ptoken_begin(&t->_errToken)){
    t = t->parent;
  }
  if(t && foundIn) *foundIn = t;
  return t ? s2_ptoken_begin(&t->_errToken) : 0;
}

char const * s2_ptoker_name_top( s2_ptoker const * t ){
  char const * n = t ? t->name : 0;
  while(t && t->parent){
    t = t->parent;
    if(t && t->name) n = t->name;
  }
  return n;
}

int s2_ttype_is_junk( int ttype ){
  switch(ttype){
    case ' ':
    case '\t':
    case '\r':
    case S2_T_Blank:
    case S2_T_CommentC:
    case S2_T_CommentCpp:
    case S2_T_Whitespace:
    case S2_T_Shebang:
    case S2_T_UTFBOM:
#if 0
    case S2_T_EOL:
    case S2_T_NL:
#endif
      return ttype;
    default:
      return 0;
  }
}

int s2_ttype_is_assignment( int ttype ){
  switch(ttype){
    case S2_T_OpAssign:
    case S2_T_OpAssign3:
    case S2_T_ArrayAppend:
      return ttype;
    default:
      return 0;
  }
}

int s2_ttype_is_assignment_combo( int ttype ){
  switch(ttype){
    case S2_T_OpPlusAssign:
    case S2_T_OpPlusAssign3:
    case S2_T_OpMinusAssign:
    case S2_T_OpMinusAssign3:
    case S2_T_OpModuloAssign:
    case S2_T_OpModuloAssign3:
    case S2_T_OpDivideAssign:
    case S2_T_OpDivideAssign3:
    case S2_T_OpMultiplyAssign:
    case S2_T_OpMultiplyAssign3:
    case S2_T_OpShiftLeftAssign:
    case S2_T_OpShiftLeftAssign3:
    case S2_T_OpShiftRightAssign:
    case S2_T_OpShiftRightAssign3:
    case S2_T_OpXOrAssign:
    case S2_T_OpXOrAssign3:
    case S2_T_OpOrAssign:
    case S2_T_OpOrAssign3:
    case S2_T_OpAndAssign:
    case S2_T_OpAndAssign3:
      return ttype;
    default:
      return 0;
  }
}


int s2_ttype_is_group( int ttype ){
  switch(ttype){
    case S2_T_ParenGroup:
    case S2_T_BraceGroup:
    case S2_T_SquigglyBlock:
      return ttype;
    default:
      return 0;
  }
}


int s2_ttype_is_eof( int ttype ){
  switch(ttype){
    case S2_T_EOF:
      return ttype;
    default:
      return 0;
  }
}

int s2_ttype_is_eol( int ttype ){
  switch(ttype){
    case S2_T_EOF:
    case S2_T_EOL:
    case S2_T_CR:
    case S2_T_NL:
    /* case S2_T_CommentCpp: A //-style comment implies a newline, so
       we might consider treating it as one (and consuming a trailing
       newline, if any, as part of the token).
    */
      return ttype;
    default:
      return 0;
  }
}

int s2_ttype_is_eox( int ttype ){
  switch(ttype){
    case S2_T_EOF:
    case S2_T_EOX:
    case S2_T_Semicolon:
      return ttype;
    default:
      return 0;
  }
}

int s2_ttype_is_space( int ttype ){
  switch(ttype){
    case S2_T_NL:
    case S2_T_CR:
    case S2_T_EOL:
    case S2_T_Whitespace:
    /* case S2_T_CommentCpp: */
    /* case S2_T_CommentC: */
    case ' ':
    case '\t':
    case '\v':
    case '\f':
      return 1;
    default:
      return 0;
  }
}

int s2_ttype_is_object_keyable( int ttype ){
  switch(ttype){
    case S2_T_LiteralIntBin:
    case S2_T_LiteralIntDec:
    case S2_T_LiteralIntOct:
    case S2_T_LiteralIntHex:
    case S2_T_LiteralDouble:
    case S2_T_LiteralStringSQ:
    case S2_T_LiteralStringDQ:
    case S2_T_LiteralString:
    case S2_T_Identifier:
      return ttype;
    default:
      return 0;
  }
}

int s2_ttype_is_int( int ttype ){
  switch(ttype){
    case S2_T_LiteralIntBin:
    case S2_T_LiteralIntDec:
    case S2_T_LiteralIntOct:
    case S2_T_LiteralIntHex:
      return ttype;
    default:
      return 0;
  }
}

int s2_ttype_is_number( int ttype ){
  switch(ttype){
    case S2_T_LiteralIntBin:
    case S2_T_LiteralIntDec:
    case S2_T_LiteralIntOct:
    case S2_T_LiteralIntHex:
    case S2_T_LiteralDouble:
      return ttype;
    default:
      return 0;
  }
}

int s2_ptoker_is_eof( s2_ptoker const * st ){
  return s2_ttype_is_eof(st->token.ttype);
}

int s2_ptoker_is_eox( s2_ptoker const * st ){
  return s2_ttype_is_eox(st->token.ttype);
}

int s2_ttype_short_circuits( int ttype ){
  switch( ttype ){
    case S2_T_OpOr:
    case S2_T_OpAnd:
    case S2_T_Question:
      return ttype;
    default: return 0;
  }
}


int s2_ttype_is_identifier_prefix( int ttype ){
  switch( ttype ){
    case S2_T_OpIncr:
    case S2_T_OpIncrPre:
    case S2_T_OpIncrPost:
    case S2_T_OpDecr:
    case S2_T_OpDecrPre:
    case S2_T_OpDecrPost:
      return ttype;
    default: return 0;
  }
}

int s2_ttype_may_precede_unary( int ttype ){
  switch(ttype){
#if 1
    /* (...) and [...] are handled at the eval level,
       and are simply treated as values. */
    case S2_T_ParenOpen:
    case S2_T_ParenGroup:
      /* No! case S2_T_ParenClose: */
    case S2_T_BraceOpen:
    case S2_T_BraceGroup:
      /* No! case S2_T_BraceClose: */
#endif
    case S2_T_Comma:
    case S2_T_Semicolon:
    case S2_T_Colon:
    case S2_T_OpArrow: /* So x-> -1 can work. */
    case S2_T_OpNot:
    case S2_T_OpAnd:
    case S2_T_OpAssign:
    case S2_T_OpAssign3:
    /* case S2_T_OpArrow2: */
    case S2_T_OpOr:
    case S2_T_OpOr3:
    case S2_T_OpElvis:
    case S2_T_OpPlus:
    case S2_T_OpPlusUnary:
    case S2_T_OpMinus:
    case S2_T_OpMinusUnary:
    case S2_T_OpMultiply:
    case S2_T_OpDivide:
    case S2_T_OpModulo:
    case S2_T_OpOrBitwise:
    case S2_T_OpNegateBitwise:
    case S2_T_OpXOr:
    case S2_T_CmpLT:
    case S2_T_CmpGT:
    case S2_T_CmpLE:
    case S2_T_CmpGE:
    case S2_T_CmpEq:
    case S2_T_CmpNotEq:
    case S2_T_CmpEqStrict:
    case S2_T_CmpNotEqStrict:
    case S2_T_OpContains:
    case S2_T_OpNotContains:
    case S2_T_OpAndBitwise:
    case S2_T_KeywordThrow:
    case S2_T_KeywordAssert:
    case S2_T_KeywordReturn:
    case S2_T_ArrayAppend /* b/c this op slides over its '=' part */:
      return ttype;
    default:
      return s2_ttype_is_assignment_combo(ttype);
  }
}

/**
   Returns true if ch is a legal char for "identifier" strings
   (e.g. var/function names). Pass 1 as isStart for the first call and
   0 for subsequent calls for the same string. If isStart is true it
   only allows alpha and underscore characters, otherwise is also
   allows numbers.

   To support unicode identifers, this function accepts any character
   with a value >0x7f (127d) as an identifier character. Any character
   value <=0 is not an identifier character.
*/
static char s2_is_id_char( int ch, char isStart ) {
  if(ch<=0) return 0;
  else if(ch>0x7f) return 1 /* TODO: filter to specific ranges? Or
                               at least remove any unicode
                               whitespace/blanks (are there such
                               things?). */;
  else switch(ch){
      case '_':
      case '$':
        /* case '@': */
        return 1;
      default:
        return isStart
          ? s2_is_alpha(ch)
          : s2_is_alnum(ch);
    }
}

void s2_ptoker_putback_set( s2_ptoker * const st,
                            s2_ptoken const * const tok ){
  st->_pbToken = *tok;
}

s2_ptoken const * s2_ptoker_putback_get( s2_ptoker const * const st ){
  return &st->_pbToken;
}

char s2_ptoker_putback( s2_ptoker * st ){
  char const rc = s2_ptoken_begin(&st->_pbToken) ? 1 : 0;
  if(rc){
    st->token = st->_pbToken;
    st->_pbToken = s2_ptoken_empty;
    st->currentCol = st->token.column;
    st->currentLine = st->token.line;
    /*s2_ptoker_token_set(st, &st->_pbToken);*/
  }
  return rc;
}


int s2_ptoker_lookahead( s2_ptoker * st, s2_ptoken * tgt ){
  s2_ptoken const oldT = st->token;
  s2_ptoken const oldP = st->_pbToken;
  int const rc = s2_ptoker_next_token( st );
  *tgt = st->token;
  st->token = oldT;
  st->_nextToken = *tgt;
  st->_pbToken = oldP;
  return rc;
}

/**
   Internal impl for s2_ptoker_lookahead_xxx_pred().

   matchMode: true means skip as long as the predicate matches. False
   means skip until the predicate matches. i.e. true implements a 'while'
   loop and false implements a 'do-until' loop.

   Returns 0 on success.
*/
static int s2_ptoker_lookahead_pred( s2_ptoker * st, s2_ptoken * tgt,
                                     s2_ttype_predicate_f pred, char matchMode ){
  s2_ptoken const oldT = st->token;
  s2_ptoken const oldP = st->_pbToken;
  int rc = 0;
  assert(tgt);
  while( !(rc = s2_ptoker_next_token( st ))
         && (matchMode ? pred(st->token.ttype) : !pred(st->token.ttype))
         && !s2_ptoker_is_eof(st)){
  }
  st->_nextToken = s2_ptoken_empty;
  *tgt = st->token;
  s2_ptoker_token_set(st, &oldT);
  /*if(!rc) st->_nextToken = *tgt;*/
  st->_pbToken = oldP;
  return rc;
}

int s2_ptoker_lookahead_skip( s2_ptoker * st, s2_ptoken * tgt,
                               s2_ttype_predicate_f pred ){
  return s2_ptoker_lookahead_pred( st, tgt, pred, 1 );
}

int s2_ptoker_lookahead_until( s2_ptoker * st, s2_ptoken * tgt,
                               s2_ttype_predicate_f pred ){
  return s2_ptoker_lookahead_pred( st, tgt, pred, 0 );
}

void s2_ptoker_token_set( s2_ptoker * const st, s2_ptoken const * const t ){
  /* Need a temp in case t== one of st's tokens */
  s2_ptoken const tmp = *t;
  st->_pbToken = st->token;
  st->token = tmp;
  st->_nextToken = s2_ptoken_empty;
}

#if 0
s2_ptoken const * s2_ptoker_token_get( s2_ptoker const * st ){
  return &st->token;
}
int s2_ptoker_ttype( s2_ptoker const * st ){
  return st->token.ttype;
}
#endif

void s2_ptoker_next_token_set( s2_ptoker * pt, s2_ptoken const * tk ){
  pt->_nextToken = *tk;
}

int s2_ptoker_next_token( s2_ptoker * t ){
  int rc = 0;
  s2_ptoken * pt = &t->token;
  char const * curpos = s2_ptoken_end(&t->token)
    ? s2_ptoken_end(&t->token) /* for the 2nd and subsequent calls */
    : s2_ptoken_begin(&t->token) /* for the first run through this function */;
  /**
    TODO, but it'd be a lot of work: rewrite this to use a uint32_t
    current character, rather than (char const *), to handle
    UTF better from here.
  */
  assert(curpos);
  assert(curpos >= s2_ptoker_begin(t));
  assert(curpos <= s2_ptoker_end(t));
  t->_pbToken = t->token;
  s2_ptoken_adjbegin_set(&t->token, 0);
  s2_ptoken_adjend_set(&t->token, 0);
  t->errMsg = 0;
  t->_errToken = s2_ptoken_empty;
  if(s2_ptoken_begin(&t->_nextToken)){
    /* Reminder to self: certain s2 logic now depends on _nextToken in
       order to function, so we cannot simply rip it out. */
#if 1
    /*
      Optimization: use _nextToken if it is set. Some lookahead
      ops set this when they end up not consuming the looked-ahead
      (lookaheaded?) token.
    */
#if 0
    {
      /* This actually saves right at 11.6k tokens in the ~3.3k code
         lines of the s2 unit test suite (as of 202001). */
      static int counter = 0;
      MARKER(("Using t->_nextToken for token type %s (%d)\n",
              s2_ttype_cstr(t->_nextToken.ttype), ++counter));
    }
#endif
    t->token = t->_nextToken;
    t->_nextToken = s2_ptoken_empty;
    return 0;
#else
    /* Just to compare results in cachegrind... */
    curpos = s2_ptoken_begin(&t->_nextToken);
#endif
  }

  if(!t->currentLine) t->currentLine = 1;
  pt->line = t->currentLine;
  pt->column = t->currentCol;

#if 0
  /* i want something _like_ this, but it needs to behave properly
     with multi-byte tokens. e.g. hitting EOF after a '+', while
     checking for '++'.
  */
#define CHECKEND if(curpos>=t->end) { t->ttype = TT_EOF; return 0; }(void)0
#else
#define CHECKEND (void)0
#endif
#define BUMP(X) curpos+=(X); CHECKEND; t->currentCol+=(X)/*only correct when skipping ASCII!*/
#define RETURN_ERR(RC,MSG) pt->ttype = S2_T_TokErr; t->errMsg = MSG; rc=RC; goto end
#define NEXT_LINE  ++t->currentLine; t->currentCol = 0
#define IN_BOUNDS (curpos<t->end)
  
  if( curpos >= s2_ptoker_end(t) ) {
    pt->ttype = S2_T_EOF;
    s2_ptoken_begin_set(&t->token, s2_ptoker_end(t));
    s2_ptoken_end_set(&t->token, s2_ptoker_end(t));
    return 0;
  }

  if(!t->parent && curpos == s2_ptoker_begin(t)){
    /* Check for some things which can only appear at the start of a
       script. The if() condition above isn't quite 100% accurate,
       considering how s2 uses sub-tokenizers at times, but it's
       pretty close.
    */
    if('#'==*curpos && '!'==*(curpos+1)){
      /* Workaround: strip shebang line from start of scripts. */
      for( ; ++curpos < s2_ptoker_end(t)
             && *curpos
             && ('\n' != *curpos); ++t->currentCol){}
      ++curpos /* skip NL */;
      if( curpos >= s2_ptoker_end(t) ) {
        pt->ttype = S2_T_EOF;
        s2_ptoken_begin_set(&t->token, s2_ptoker_end(t));
        s2_ptoken_end_set(&t->token, s2_ptoker_end(t));
        return 0;
      }
      s2_ptoken_end_set(&t->token, curpos);
      t->token.ttype = S2_T_Shebang;
      ++t->currentLine;
      t->currentCol = 0;
      return 0;
    }else{
      /* Check for a UTF-8 BOM. */
      unsigned char const * ccp = (unsigned char const*)curpos;
      if(0xEF==*ccp && 0xBB==ccp[1] && 0xBF==ccp[2]){
        curpos += 3;
        s2_ptoken_end_set(&t->token, curpos);
        t->token.ttype = S2_T_UTFBOM;
        t->currentCol += 3;
        return 0;
      }
    }
  }

  pt->ttype = S2_T_INVALID;
  s2_ptoken_begin_set(&t->token, curpos);

  switch(*curpos){
    case 0:
      pt->ttype = S2_T_EOF;
      /*This is a necessary exception to the bump-on-consume
        rule.*/
      break;
    case '\\': /* Treat \<NEWLINE> as a continuation of a line */
      if(('\n'==*(curpos+1)) || (('\r'==*(curpos+1)) && ('\n'==*(curpos+2)))){
        pt->ttype = S2_T_Whitespace;
        BUMP(('\r'==*(curpos+1)) ? 3 : 2);
        NEXT_LINE;
      }else{
        RETURN_ERR(CWAL_SCR_SYNTAX,"Unexpected backslash.");
      }
      break;
    case '\r':
      if('\n'==*(curpos+1)){
        pt->ttype = S2_T_EOL;
        BUMP(2);
        NEXT_LINE;
      }
      else{
        pt->ttype = S2_T_CR;
        BUMP(1);
      }
      break;
    case '\n':
      pt->ttype = S2_T_NL;
      BUMP(1);
      NEXT_LINE;
      break;
    case ' ':
    case '\t':
    case '\v':
    case '\f':
      pt->ttype = *curpos /*S2_T_Blank*/;
      BUMP(1);
#if 1
      while( curpos<s2_ptoker_end(t)
             && *curpos && s2_is_blank(*curpos) ){
        pt->ttype = S2_T_Blank;
        BUMP(1);
      }
#endif
      break;
    case ':': /* colon or namespace */
      if( ':' == *(curpos+1) ){
        pt->ttype = S2_T_Colon2;
        BUMP(2);
      }else if( '=' == *(curpos+1) ){
        pt->ttype = S2_T_OpColonEqual;
        BUMP(2);
      }else{
        pt->ttype = S2_T_Colon;
        BUMP(1);
      }
      break;
    case '~': /* ~ or ~= */
      pt->ttype = *curpos;
      BUMP(1);
#if 0
      if('=' == *curpos){
        pt->ttype = S2_T_NotYet;
        BUMP(1);
      }
#endif
      break;
    case '/': /* numeric division or C-style comment block */
      pt->ttype = S2_T_OpDivide /* *curpos */;
      BUMP(1);
      switch(*curpos){
        case (int)'=':
          pt->ttype = S2_T_OpDivideAssign;
          BUMP(1);
          break;
        case (int)'*':/* C-style comment block */
          pt->ttype = S2_T_CommentC;
          BUMP(1);
          do{
            while( curpos<s2_ptoker_end(t)
                   && *curpos && ('*' != *curpos) ){
              if('\n'==*curpos){
                NEXT_LINE;
              }
              BUMP(1);
            }
            if(s2_ptoker_end(t)==curpos){
              RETURN_ERR(CWAL_SCR_SYNTAX,
                         "Reached (virtual) EOF while looking for "
                         "C-style comment closer.");
            }
            else if( s2_ptoker_end(t)<=curpos || !*curpos ) break;
            BUMP(1); /* skip '*' */
          } while( curpos < s2_ptoker_end(t)
                   && *curpos && ('/' != *curpos));
          if( curpos < s2_ptoker_end(t) && *curpos != '/' ){
            RETURN_ERR(CWAL_SCR_SYNTAX,"End of C-style comment not found."); 
          }else{
            BUMP(1); /* get that last slash */
          }
          break;
        case (int)'/':/* C++-style comment line */
          BUMP(1);
          pt->ttype = S2_T_CommentCpp;
          while( curpos<t->end && *curpos && ('\n' != *curpos) ){
            BUMP(1);
          }
#if 0
          if(curpos<t->end && *curpos){
            /*
              Consume the EOL with the comment, since a CPP
              comment always implies an EOL or EOF.

              Re-try this when other changes aren't pending
              committing.

              This would probably (untested) break those few
              constructs which optionally treat EOL as an EOX unless
              we reported this comment as an EOL token.
            */
            BUMP(1);
          }
#endif
          break;
      } /* end post-/ checks */
      break;
    case '"':
    case '\'': /* read string literal */{
      char const quote = *curpos;
      pt->ttype = ('"' == *curpos)
        ? S2_T_LiteralStringDQ
        : S2_T_LiteralStringSQ;
      BUMP(1)/*leading quote*/;
      while(curpos<s2_ptoker_end(t) && *curpos && (*curpos != quote)){
        /*
          BUG: our counting of t->currentCol (via BUMP())
          is off for non-ASCII characters. Need to loop over
          this as UTF.
        */
        if( (*curpos == '\\') ){
          /* consider next char to be escaped, but keep escape char */
          BUMP(1);
          if(*curpos == 0){
            RETURN_ERR(CWAL_SCR_SYNTAX,
                       "Unexpected EOF while tokenizing "
                       "backslash-escaped char in string literal.");
          }
          BUMP(1);
          continue;
        }
        else if('\n'==*curpos){
          BUMP(1);
          NEXT_LINE;
        }
        else {
          BUMP(1);
        }
      }
      if(s2_ptoker_end(t)<=curpos || *curpos != quote){
        RETURN_ERR(CWAL_SCR_SYNTAX,
                   "Unexpected end of string literal.");
      }else{
        BUMP(1)/*trailing quote*/;
      }
      break;
    } /* end literal string */
    case '&': /* & or && or &= */
      pt->ttype = S2_T_OpAndBitwise;
      BUMP(1);
      switch(*curpos){
        case '&':
          pt->ttype = S2_T_OpAnd;
          BUMP(1);
          break;
        case '=':
          pt->ttype = S2_T_OpAndAssign;
          BUMP(1);
          break;
      }
      break;
    case '|': /* | or || or |= or ||| */
      pt->ttype = S2_T_OpOrBitwise;
      BUMP(1);
      switch(*curpos){
        case '|':
          pt->ttype = S2_T_OpOr;
          BUMP(1);
          if( '|' == *curpos ){
            pt->ttype = S2_T_OpOr3;
            BUMP(1);
          }
          break;
        case '=':
          pt->ttype = S2_T_OpOrAssign;
          BUMP(1);
          break;
      }
      break;
    case '?': /* ? or ?: */
      pt->ttype = S2_T_Question;
      BUMP(1);
      if( ':' == *curpos ){
        pt->ttype = S2_T_OpElvis;
        BUMP(1);
      }
      break;
    case '^': /* ^ or ^= */
      pt->ttype = S2_T_OpXOr;
      BUMP(1);
      if( '=' == *curpos ){
        pt->ttype = S2_T_OpXOrAssign;
        BUMP(1);
      }
      break;
    case '+': /* + or ++ or += */{
      pt->ttype = S2_T_OpPlus;
      BUMP(1);
      switch(*curpos){
        case '+': /* ++ */
          pt->ttype = S2_T_OpIncr;
          BUMP(1);
          break;
        case '=': /* += */
          pt->ttype = S2_T_OpPlusAssign;
          BUMP(1);
          break;
        default: break;
      }
      break;
    }
    case '*': /* * or *= */
      pt->ttype = S2_T_OpMultiply;
      BUMP(1);
      switch(*curpos){
        case '=':
          pt->ttype = S2_T_OpMultiplyAssign;
          BUMP(1);
          break;
        case '/':
          pt->ttype = S2_T_INVALID;
          RETURN_ERR(CWAL_SCR_SYNTAX,
                     "Comment closer (*/) not inside a comment.");
          break;
      }
      break;
    case '-': /* - or -- or -= */{
      pt->ttype = S2_T_OpMinus;
      BUMP(1);
      switch( *curpos ){
        case '-': /* -- */
          pt->ttype = S2_T_OpDecr;
          BUMP(1);
          break;
        case '=':
          pt->ttype = S2_T_OpMinusAssign;
          BUMP(1);
          break;
        case '>':
          pt->ttype = S2_T_OpArrow;
          BUMP(1);
          break;
        default: break;
      }
      break;
    }
    case '<': /* LT or << or <= or <<= or <<< */{
      pt->ttype = S2_T_CmpLT;
      BUMP(1);
      switch( *curpos ){
        case '<': /* tok= << */ {
          pt->ttype = S2_T_OpShiftLeft;
          BUMP(1);
          switch(*curpos){
            case '=': /* <<= */
              pt->ttype = S2_T_OpShiftLeftAssign;
              BUMP(1);
              break;
            case '<': /* <<< */
              pt->ttype = S2_T_HeredocStart;
              BUMP(1);
              break;
            default: break;
          }
          break;
        }
        case '=': /* tok= <= */
          pt->ttype = S2_T_CmpLE;
          BUMP(1);
          break;
        default: break;
      }
      break;
    }
    case '>': /* GT or >> or >= or >>= */{
      pt->ttype = S2_T_CmpGT;
      BUMP(1);
      switch(*curpos){
        case '>': /* >> */
          pt->ttype = S2_T_OpShiftRight;
          BUMP(1);
          if( '=' == *curpos ){/* >>= */
            pt->ttype = S2_T_OpShiftRightAssign;
            BUMP(1);
          }
          break;
        case '=': /* >= */
          pt->ttype = S2_T_CmpGE;
          BUMP(1);
          break;
        default: break;
      }
      break;
    }
    case '=': /* = or == or === or =~ or => */
      pt->ttype = S2_T_OpAssign;
      BUMP(1);
      switch( *curpos ){
        case '~': /* -- */
          pt->ttype = S2_T_OpContains;
          BUMP(1);
          break;
        case '=':
          pt->ttype = S2_T_CmpEq;
          BUMP(1);
          if( '=' == *curpos ){
            pt->ttype = S2_T_CmpEqStrict;
            BUMP(1);
          }
          break;
        case '>':
          pt->ttype = S2_T_OpArrow2;
          BUMP(1);
          break;
        default: break;
      }
      break;
    case '%': /* % or %= */
      pt->ttype = S2_T_OpModulo /* *curpos */;
      BUMP(1);
      if( '=' == *curpos ){
        pt->ttype = S2_T_OpModuloAssign;
        BUMP(1);
      }
      break;
    case '!': /* ! or != or !== or !~ */
      pt->ttype = S2_T_OpNot;
      BUMP(1);
      if( '~' == *curpos ){
        pt->ttype = S2_T_OpNotContains;
        BUMP(1);
      }else if( '=' == *curpos ){
        pt->ttype = S2_T_CmpNotEq;
        BUMP(1);
        if( '=' == *curpos ){
          pt->ttype = S2_T_CmpNotEqStrict;
          BUMP(1);
        }
      }
      break;
    case '.': /* . or .. */
      pt->ttype = S2_T_OpDot;
      BUMP(1);
      if( '.' == *curpos ){
        pt->ttype = S2_T_OpDotDot;
        BUMP(1);
      }
      break;
    case '{': /* { */
      pt->ttype = S2_T_SquigglyOpen;
      BUMP(1);
      if(IN_BOUNDS && '{'==*curpos && '{'==curpos[1]){
        /* {{{ ...heredoc... }}} is provided as an alternate heredoc
           syntax because it plays better with existing syntax
           highlighting and auto-indention modes. */
        pt->ttype = S2_T_HeredocStart2;
        BUMP(2);
      }  
      break;
    case '}': /* } */
      pt->ttype = S2_T_SquigglyClose;
      BUMP(1);
      break;
    case '0': /* 0 or hex or octal or binary literals */
      BUMP(1);
      if(s2_ptoker_end(t) <= curpos){
        /* special case: 0 at the end of input. */
        pt->ttype = S2_T_LiteralIntDec;
        break;
      }
      switch (*curpos)/* try hex or octal or binary */{
        case 'x':
        case 'X':{/** hex digit. */
          int digitCount = 0;
          BUMP(1);
          while(curpos<s2_ptoker_end(t) && *curpos){
            if('_'==*curpos){
              BUMP(1);
              continue /* separator */;
            }else if(s2_is_xdigit(*curpos)){
              ++digitCount;
              BUMP(1);
              continue;
            }
            break;
          }
          /*
            20200828: bug: the following syntax errors are not
            reporting error positions. e.g.

            catch { 0x }
            catch { 0x3_ }

            But only *sometimes*!?!?

            i'm imagining this, certainly.
          */
          if(!digitCount){
            RETURN_ERR(CWAL_SCR_SYNTAX,
                       "No digits in hexidecimal int literal.");
          }else{
            pt->ttype = S2_T_LiteralIntHex;
            if('_' == curpos[-1] /* last char was a separator */
               || s2_is_alnum(*curpos)
               ){
              BUMP(1); /* make sure it shows up in the error string. */
              RETURN_ERR(CWAL_SCR_SYNTAX,
                         "Malformed hexidecimal int literal.");
            }
          }
          break;
        }
        case 'o':{/* try octal... */
          int digitCount = 0;
          BUMP(1);
          while(curpos<s2_ptoker_end(t) && *curpos){
            if('_'==*curpos){
              BUMP(1);
              continue /* separator */;
            }else if(s2_is_octaldigit(*curpos)){
              ++digitCount;
              BUMP(1);
              continue;
            }
            break;
          }
          if(!digitCount){
            RETURN_ERR(CWAL_SCR_SYNTAX,
                       "No digits in octal int literal.");
          }else{
            pt->ttype = S2_T_LiteralIntOct;
            if('_' == curpos[-1] /* last char was a separator */
               || s2_is_alnum(*curpos)
               ){
              BUMP(1); /* make sure it shows up in the error string. */
              RETURN_ERR(CWAL_SCR_SYNTAX,
                         "Malformed octal int literal.");
            }
          }
          break;
        }
        case 'b':{
          /* try binary... */
          int digitCount = 0;
          BUMP(1);
          while(*curpos){
            if(*curpos=='0' || *curpos=='1'){
              ++digitCount;
              BUMP(1);
              continue;
            }else if('_'==*curpos){
              BUMP(1);
              continue /* separator */;
            }
            break;
          }
          if(!digitCount){
            RETURN_ERR(CWAL_SCR_SYNTAX,
                       "No digits in binary int literal.");
          }
          /*else if(digitCount > CWAL_INT_T_BITS){
            RETURN_ERR(CWAL_SCR_SYNTAX,
            "Binary value is too large for this build.");
            }*/
          else{
            pt->ttype = S2_T_LiteralIntBin;
            if('_' == curpos[-1] /* last char was a separator */
               || s2_is_alnum(*curpos)
               ){
              BUMP(1); /* make sure it shows up in the error string. */
              RETURN_ERR(CWAL_SCR_SYNTAX,
                         "Malformed binary int literal.");
            }
          }
          break;
        }/*binary*/
        default:
          if( *curpos && (
                          s2_is_alnum(*curpos)
                          || (*curpos == '_'))
              )
          { /* reject 12334x where x is alphanum or _ */
            BUMP(1); /* make sure it shows up in the error string. */
            RETURN_ERR(CWAL_SCR_SYNTAX,
                       "Malformed numeric literal starts "
                       "with '0' but is neither octal nor "
                       "hex nor binary.");
          }
          if('.'==*curpos){
            if(!s2_is_digit(curpos[1])){
              /* curpos -= 1 */ /* Assume this might be NUMBER.FUNC(),
                 back up and leave it for next time */;
              pt->ttype = S2_T_LiteralIntDec;
              break;
            }
            BUMP(1);
            while( s2_is_digit(*curpos) ){
              BUMP(1);
            }
            if('.'==*(curpos-1)){
              RETURN_ERR(CWAL_SCR_SYNTAX,
                         "Mis-terminated floating point value.");
            }
            pt->ttype = S2_T_LiteralDouble;
          }
          else {
            pt->ttype = S2_T_LiteralIntDec;
            /* A literal 0. This is okay. */
          }
          break;
      }
      break;
    case '1': case '2': case '3':
    case '4': case '5': case '6':
    case '7': case '8': case '9': /* integer or double literal. */{
      int gotSep = 0;
      /*
        Reminder to self: The 0x, 0o, and 0b formats all support '_'
        characters as "visual separators" in their numeric literals.
        Decimals values "should" do the same but...  our downstream
        code for parsing doubles does not handle those characters,
        thus we prohibit the '_' separators in floating-point values
        here. At _this_ point in the tokenization we don't yet know if
        we're reading an integer or double, so we have to remember
        whether we hit a '_' and error out if it turns out we're
        parsing a double. Not elegant, but that's okay.
      */
      BUMP(1);
      while(curpos<s2_ptoker_end(t) && *curpos){
        /* integer or first part of a double. */
        if('_'==*curpos){
          ++gotSep;
          BUMP(1);
          continue;
        }else if(s2_is_digit(*curpos)){
          BUMP(1);
          continue;
        }
        break;
      }
      if( curpos<s2_ptoker_end(t)
          && ('.' == *curpos) && s2_is_digit(*(curpos+1)) ){
        /* double number */
        if(gotSep){
          RETURN_ERR(CWAL_SCR_SYNTAX,
                     "'_' separators are not legal in floating-point literals.");
        }
        pt->ttype = S2_T_LiteralDouble;
        BUMP(1);
        while(curpos<s2_ptoker_end(t) && *curpos && s2_is_digit(*curpos)){
          BUMP(1);
        }
      }
      else {
        pt->ttype = S2_T_LiteralIntDec;
      }
      if( (curpos[-1] == '_'
           /* disallow trailing separator for symmetry
              with 0x/0o/0b literals. */)
          || (curpos<s2_ptoker_end(t)
              && *curpos
              && (s2_is_alnum(*curpos)
                  || (*curpos == '_')))
          ) {
        BUMP(1); /* make sure it shows up in the error string. */
        RETURN_ERR(CWAL_SCR_SYNTAX,
                   "Malformed numeric literal.");
      }
      break;
    }
    case '@':
      pt->ttype = S2_T_At;
      BUMP(1);
      break;
  }/* end of switch(*curpos) */

  if(0==rc
     && (curpos == s2_ptoken_begin(&t->token))
     && (S2_T_EOF != pt->ttype) ){
    /* keep trying... */
    int const idChars = s2_read_identifier2( curpos, s2_ptoker_end(t),
                                             &curpos, t->flags );
    if( idChars ) /* identifier string */{
      t->currentCol += idChars;
      pt->ttype = S2_T_Identifier;
    }
#if 1
    /* i don't like this, but removing it causes errors i'm
       not willing to chase down right now: a falsely-reported
       CWAL_RC_OOM triggered via the eval loop. OTOH, if
       we don't do this then we need to explicitly catch and tag all
       1-char operators. */
    else if( (*((unsigned char const *)curpos) > 0)
             && (*((unsigned char const *)curpos) <= 127) ) {
      pt->ttype = *curpos;
      BUMP(1);
    }
#endif
    else {
      assert(curpos == s2_ptoken_begin(&t->token));
      assert(S2_T_INVALID==pt->ttype);
    }
  }

  if(S2_T_INVALID == pt->ttype){
    unsigned char const cCh = (unsigned char)*curpos;
    if( cCh > 0x7F )
      /* _hope_ for a UTF8 identifier string! */{
      int const chars = s2_read_identifier2( curpos, s2_ptoker_end(t),
                                             &curpos, t->flags );
      if(chars){
        t->currentCol += chars;
        pt->ttype = S2_T_Identifier;
      }else{
        t->errMsg = "Don't know how to tokenize this.";
        rc = CWAL_SCR_SYNTAX;
      }
    }else{
      pt->ttype = S2_T_TokErr;
      /* MARKER(("byte=%02x\n", (unsigned char)*curpos)); */
      t->errMsg = "Don't know how to tokenize this.";
      rc = CWAL_SCR_SYNTAX;
    }
  }
  s2_ptoken_end_set(&t->token, (curpos > s2_ptoker_end(t))
                    ? s2_ptoker_end(t) : curpos);
#undef IN_BOUNDS
#undef CHECKEND
#undef BUMP
#undef RETURN_ERR
#undef NEXT_LINE
  end:
  if(rc){
    assert(t->errMsg);
    t->_errToken = t->token;
  }
  return rc;
}

int s2_ptoker_next_token_skip_junk( s2_ptoker * st ){
  int rc = 0;
  do{
    rc = s2_ptoker_next_token(st);
  }while(!rc && s2_ttype_is_junk(st->token.ttype));
  return rc;
}

char s2_is_space( int ch ){
  switch(ch){
    case ' ':
    case '\n':
    case '\r':
    case '\t':
    case '\v':
    case '\f':
      return 1;
    default:
      return 0;
  }
}

char s2_is_blank( int ch ){
  switch(ch){
    case ' ':
    case '\t':
      return 1;
    default:
      return 0;
  }
}

char s2_is_digit( int ch ){
  return '0'<=ch && '9'>=ch;
}

char s2_is_xdigit( int ch ){
  return ('a'<=ch && 'f'>=ch)
    ? 1
    : (('A'<=ch && 'F'>=ch)
       ? 1
       :s2_is_digit(ch));
}

char s2_is_octaldigit( int ch ){
  return ('0'<=ch && '7'>=ch);
}

char s2_is_alpha( int ch ){
  return ('a'<=ch && 'z'>=ch)
    ? 1
    : ('A'<=ch && 'Z'>=ch);
}

char s2_is_alnum( int ch ){
  return s2_is_alpha(ch) ? 1 : s2_is_digit(ch);
}

int s2_read_identifier2( char const * zPos,
                         char const * zEnd,
                         char const ** zIdEnd,
                         uint32_t flags ){
  unsigned char const * start = (unsigned char const *) zPos;
  unsigned char const * pos = start;
  unsigned char const * end = (unsigned char const *) zEnd;
  unsigned char const * endChar = pos;
  int ch;
  int rc = 0;
  int const allowDash = (S2_T10N_F_IDENTIFIER_DASHES & flags);
  assert(zEnd>zPos);
  for( ; pos < end; ){
    ch = cwal_utf8_read_char( pos, end, &endChar );
    if(endChar == pos) break;
    else if(!s2_is_id_char(ch, (pos==start) ? 1 : 0)){
      if(!('-'==ch && allowDash)){
        break;
      }
    }
    ++rc;
    pos = endChar;
  }
  *zIdEnd = zPos + (pos - start);
  return rc;
}

int s2_read_identifier( char const * zPos,
                        char const * zEnd,
                        char const ** zIdEnd ){
  return s2_read_identifier2( zPos, zEnd, zIdEnd, 0 );
}

#if 0
/* For later study. It's not clear whether this will help us
   count lines more quickly but may have use in the core cwal
   UTF8 APIs.
*/
/**
 Copyright (c) 2008-2009 Bjoern Hoehrmann <bjoern@hoehrmann.de>
 See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.
*/
#define UTF8_ACCEPT 0
#define UTF8_REJECT 1
static const uint8_t utf8d[] = {
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, /* 00..1f */
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, /* 20..3f */
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, /* 40..5f */
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, /* 60..7f */
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9, /* 80..9f */
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7, /* a0..bf */
  8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, /* c0..df */
  0xa,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x4,0x3,0x3, /* e0..ef */
  0xb,0x6,0x6,0x6,0x5,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8, /* f0..ff */
  0x0,0x1,0x2,0x3,0x5,0x8,0x7,0x1,0x1,0x1,0x4,0x6,0x1,0x1,0x1,0x1, /* s0..s0 */
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,0,1,0,1,1,1,1,1,1, /* s1..s2 */
  1,2,1,1,1,1,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1, /* s3..s4 */
  1,2,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,3,1,3,1,1,1,1,1,1, /* s5..s6 */
  1,3,1,1,1,1,1,3,1,3,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1  /* s7..s8 */
};
static uint32_t inline
bh_decode(uint32_t* state, uint32_t* codep, uint32_t byte) {
  uint32_t type = utf8d[byte];

  *codep = (*state != UTF8_ACCEPT) ?
    (byte & 0x3fu) | (*codep << 6) :
    (0xff >> type) & (byte);

  *state = utf8d[256 + *state*16 + type];
  return *state;
}
#endif

int s2_count_lines( char const * src, char const * end_,
                    char const * pos_,
                    s2_linecol_t *line, s2_linecol_t *col ){
  s2_linecol_t ln = 1, c = 0;
  unsigned char const * x = (unsigned char const *)src;
  unsigned char const * const pos = (unsigned char const *)pos_;
  unsigned char const * const end = (unsigned char const *)end_;
  if((pos<x) || (pos>=end)) {
    return CWAL_RC_RANGE;
  }
    /* profiling shows that cwal_utf8_read_char() is, by leaps and
       bounds, the most oft-called func in this whole constellation,
       largely due to this routine. We need a faster, file-local
       multi-byte char skipping routine.

       And yet this function still takes more time than many other
       functions doing much more work and called more often?!?!?
       e.g. cwal_engine_vacuum(). Heh?

       Need to see if we can build line/column counting into s2_ptoker
       and s2_ptoker_next_token(). (That failed because of how we
       continually swap tokens in and out.)

       Misc. interesting UTF-decoding links with free code:

       https://gist.github.com/gorb314/7888804
       https://bjoern.hoehrmann.de/utf-8/decoder/dfa/

    */
#if 1
  /**
     Vaguely derived from: https://nullprogram.com/blog/2017/10/06/

     A notably faster than the daemonology.net impl in the s2
     amalgamated unit tests. :-D i'm not sure which values in
     callgrind are the significant ones, but in the test data i'm
     working from, where this routine is the framework's hotspot, this
     algo cuts total instructions(?) by about 25% compared to that
     one.
  */
  else{
    static const unsigned char lengths[32] = {
    /* ^^^ changing this to int takes more CPU instructions. */
       1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
       0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 3, 3, 4, 0
    };
    while( x < pos  ){
      if(*x > 127U) x += lengths[*x>>3];
      /* ^^^^ oddly, this loop is slower if we invert
         this if/else into if(*x<128U)... */
      else{
        if('\n'==*x){
          ++ln;
          c = -1;
        }
        ++x;
        /* Reminder to future self: we spent more than an hour
           trying out various combinations to reduce the instruction
           count and tiny changes have huge effects. e.g., combining
           the increment of x into:

           if('\n'==*x++){...}

           Increased the overall instruction count on the unit tests
           by a whopping 33M.
         */
      }
      ++c;
    }
  }
#elif 1
  /**
     Vaguely derived from: https://nullprogram.com/blog/2017/10/06/

     We unfortunately can't use that algo as-is because it requires
     that the input be padded to an increment of 4 bytes.

     After notable tinkering, a tiny tick faster than the
     daemonology.net impl.
  */
  else{
    static const char lengths[32] = {
       1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
       0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 3, 3, 4, 0
    };
    char i;
    while(x<pos && (i = lengths[x[0]>>3])){
      if('\n'==x[0]){
        ++ln;
        c = -1;
      }
      x+=i;
      ++c;
    }
  }
#elif 0
  /* Derived from:
     http://www.daemonology.net/blog/2008-06-05-faster-utf8-strlen.html
  */
  for( ; (x < pos) && *x; ++x ){
    switch(*x){
      case '\n':
        ++ln;
        c = 0;
        break;
        /*case '\r':
        if('\n'==x[1]){
          ++ln; 
          c = 0;
          ++x;
        }
        else ++c;
        break;*/
      default:
        ++c;
        if(*x>127U){
          switch(0xF0 & *x) {
            case 0xF0: /* length 4 */
              x += 3;
              break;
            case 0xE0: /* length 3 */
              x+= 2;
              break;
            default: /* length 2 */
              x += 1;
              break;
          }
          break;
        }
    }
  }
#else
  /* EXTREMELY slow impl... */
  else{
    unsigned char const * tail = end;
    int ch;
    for( ; (x < pos) && *x; ++x ){
      if('\n'==*x){
        ++ln;
        c = 0;
      }else if('\r'==*x && '\n'==x[1]){
        ++ln;
        c = 0;
      }
      else {
        ch = cwal_utf8_read_char( x, end, &tail );
        if(ch>0){
          assert(tail>x);
          ++c;
          x = tail - 1/* account for loop incr */;
        }
      }
    }
  }
#endif
  if(line) *line = ln;
  if(col) *col = c;
  /* MARKER(("Updated line/col: %u, %u\n", ln, c )); */
  return 0;
}

int s2_ptoker_count_lines( s2_ptoker const * pt, char const * pos,
                           s2_linecol_t * line, s2_linecol_t * col ){
  s2_ptoker const * top;
  s2_ptoker const * prev;
  int rc = 0;
  s2_linecol_t li = 0, c = 0;
  s2_linecol_t offLine = 0, offCol = 0;
  s2_ptoker_lccache_entry const * cachedPos;
  /* look for an already-calculated starting point... */
  for( prev = top = pt; top; prev = top, top = top->parent){
    if(top->lineOffset){
      offLine = top->lineOffset;
      offCol = top->colOffset;
      break;
    }
  }
  if(!top) top = prev;
  assert(top);
  assert(s2_ptoker_begin(pt) >= s2_ptoker_begin(top)
         && s2_ptoker_end(pt) <= s2_ptoker_end(top));
  assert(pos<s2_ptoker_end(pt) && pos >= s2_ptoker_begin(top));
  if(S2_T10N_F_LINEAR_TOKER & pt->flags){
    s2_ptoken const * et = s2_ptoken_begin(&pt->_errToken)
      ? &pt->_errToken
      : &pt->token;
    li = et->line;
    c = et->column;
    /*MARKER(("Using S2_T10N_F_LINEAR_TOKER line %d, col %d\n",
      li, c));*/
  }
  if(!li && (cachedPos = s2_ptoker_lc_search(top, pos))){
    LCMARKER(("%p Got a hit: (%d, %d) distance from target pos=%d, from start=%d\n",
              (void const *)top, cachedPos->line, cachedPos->col,
              (int)(pos - cachedPos->pos), (int)(pos - s2_ptoker_begin(top))
              ));
    if(cachedPos->pos==pos){
      li = cachedPos->line;
      c = cachedPos->col;
    }else{
      offLine = cachedPos->line;
      offCol = cachedPos->col;
      rc = s2_count_lines( cachedPos->pos, s2_ptoker_end(pt), pos,
                           &li, &c );
      assert(!rc);
    }
  }else if(!li){
    rc = s2_count_lines( s2_ptoker_begin(top), s2_ptoker_end(pt),
                         pos, &li, &c );
    assert(!rc);
  }
  if(!rc && (line || col)){
    if(offLine){
      /*MARKER(("top->lineOffset=%d, top->colOffset=%d\n",
        top->lineOffset, top->colOffset));*/
      c += (1==li) ? offCol : 0;
      li += offLine - 1;
    }
    if(line) *line = li;
    if(col) *col = c;
#if S2_T10N_LCCACHE
      /*LCMARKER(("%p Attempting to cache entry (%d, %d)\n", (void const *)top, li, c));*/
    s2_ptoker_lc_cache(top, pos, li, c);
#endif
  }
  return rc;
}

int s2_ptoker_count_lines2( s2_ptoker const * pt,
                            s2_ptoken const * tok,
                            s2_linecol_t * line, s2_linecol_t * col ){
  s2_linecol_t li = 0, c = 0;
  s2_ptoker const * top;
  int rc = 0;
  if(!tok) tok = &pt->token;
  for( top = pt; top; top = top->parent){
    if(top->lineOffset) break;
  }
  if(!top) top = s2_ptoker_top_parent(pt);
  if(tok->line){
    MARKER(("Using token-embedded line %d, col %d [%.*s] parent?=%d\n",
            tok->line, tok->column,
            (int)s2_ptoken_len(tok), s2_ptoken_begin(tok),
            !!pt->parent));
    li = tok->line;
    c = tok->column;
  }else{
    rc = s2_count_lines( s2_ptoker_begin(top), s2_ptoker_end(pt),
                         s2_ptoken_begin(tok), &li, &c );
  }
  if(!rc && (line || col)){
    /* Adjust the line/column for the line/colOffset
       members of the parent tokenizer... */
    int oL = 0, oC = 0;
    assert(top->lineOffset || !top->parent);
    if(top->lineOffset){
      oL += top->lineOffset /* - 1 */;
      if(1==li) oC += top->colOffset;
      else oC = 0;
      MARKER(("top->lineOffset=%d, top->colOffset=%d\n",
              top->lineOffset, top->colOffset));
      MARKER(("adjusted line=%d=>%d, col=%d=>%d\n",
              li, li+oL, c, c+oC));
      li += oL;
      c += oC;
    }
    if(line) *line = li;
    if(col) *col = c;
  }
  return rc;
}

static int s2_hexbyte( int ch ){
  if(ch>='0' && ch<='9') return ch - '0';
  else if(ch>='a' && ch<='f') return ch - 'a' + 10;
  else if(ch>='A' && ch<='F') return ch - 'A' + 10;
  else return -1;
}

static int s2_read_hex_bytes(unsigned char const *zPos,
                             unsigned int howMany,
                             unsigned int * rv ){
  unsigned int rc = 0;
  int ch1, ch2;
  unsigned int n = 0;
  assert(!(howMany%2) && "howMany must be an even number!");
  assert(zPos && rv);
  while(n<howMany){
    ch1 = *zPos ? s2_hexbyte(*zPos++) : -1;
    ch2 = (ch1>=0) ? s2_hexbyte(*zPos++) : -1;
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
int s2_unescape_string( cwal_engine * e,
                        char const * _begin,
                        char const * _end,
                        cwal_buffer * dest ){
  cwal_size_t sz;
  unsigned char const * begin = (unsigned char const*)_begin;
  unsigned char const * end = (unsigned char const*)_end;
  unsigned char const * p = begin;
  unsigned char * out;
  int check;
  cwal_size_t oldUsed;
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
            uCount = s2_read_hex_bytes(p+1, ulen, &uChar);
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

    
char const * s2_ttype_cstr( int ttype ){
  switch((enum s2_token_types)ttype){
    /* ^^^^ reminder: that cast is so that gcc will warn
       when i forget to update this function after adding
       new entries to that enum. */
#define CASE(TT) case TT: return &(#TT[5])/*==prefix strlen("S2_T_")*/
    CASE(S2_T_TokErr);
    CASE(S2_T_INVALID);
    CASE(S2_T_EOF);
    CASE(S2_T_EOX);
    CASE(S2_T_Tab);
    CASE(S2_T_NL);
    CASE(S2_T_VTab);
    CASE(S2_T_FF);
    CASE(S2_T_CR);
    CASE(S2_T_EOL);
    CASE(S2_T_Space);
    CASE(S2_T_Blank);
    CASE(S2_T_Whitespace);
    CASE(S2_T_At);
    CASE(S2_T_UTFBOM);
    CASE(S2_T_OpNot);
    CASE(S2_T_OpHash);
    CASE(S2_T_Shebang);
    CASE(S2_T_OpModulo);
    CASE(S2_T_OpModuloAssign);
    CASE(S2_T_OpModuloAssign3);
    CASE(S2_T_OpAndBitwise);
    CASE(S2_T_OpAnd);
    CASE(S2_T_OpAndAssign);
    CASE(S2_T_OpAndAssign3);
    CASE(S2_T_ParenOpen);
    CASE(S2_T_ParenGroup);
    CASE(S2_T_ParenClose);
    CASE(S2_T_OpMultiply);
    CASE(S2_T_OpMultiplyAssign);
    CASE(S2_T_OpMultiplyAssign3);
    CASE(S2_T_OpPlus);
    CASE(S2_T_OpPlusUnary);
    CASE(S2_T_OpPlusAssign);
    CASE(S2_T_OpPlusAssign3);
    CASE(S2_T_OpIncr);
    CASE(S2_T_OpIncrPre);
    CASE(S2_T_OpIncrPost);
    CASE(S2_T_Comma);
    CASE(S2_T_RHSEval);
    CASE(S2_T_OpMinus);
    CASE(S2_T_OpMinusUnary);
    CASE(S2_T_OpMinusAssign);
    CASE(S2_T_OpMinusAssign3);
    CASE(S2_T_OpDecr);
    CASE(S2_T_OpDecrPre);
    CASE(S2_T_OpDecrPost);
    CASE(S2_T_OpDot);
    CASE(S2_T_OpDotDot);
    CASE(S2_T_OpDotLength);
    CASE(S2_T_OpInherits);
    CASE(S2_T_OpNotInherits);
    CASE(S2_T_OpContains);
    CASE(S2_T_OpNotContains);
    CASE(S2_T_OpArrow);
    CASE(S2_T_OpArrow2);
    CASE(S2_T_OpDivide);
    CASE(S2_T_OpDivideAssign);
    CASE(S2_T_OpDivideAssign3);
    CASE(S2_T_Colon);
    CASE(S2_T_Colon2);
    CASE(S2_T_OpColonEqual);
    CASE(S2_T_Semicolon);
    CASE(S2_T_CmpLT);
    CASE(S2_T_CmpLE);
    CASE(S2_T_OpShiftLeft);
    CASE(S2_T_OpShiftLeftAssign);
    CASE(S2_T_OpShiftLeftAssign3);
    CASE(S2_T_HeredocStart);
    CASE(S2_T_HeredocStart2);
    CASE(S2_T_Heredoc);
    CASE(S2_T_OpAssign);
    CASE(S2_T_OpAssign3);
    CASE(S2_T_OpAssignConst3);
    CASE(S2_T_CmpEq);
    CASE(S2_T_CmpNotEq);
    CASE(S2_T_CmpEqStrict);
    CASE(S2_T_CmpNotEqStrict);
    CASE(S2_T_CmpGT);
    CASE(S2_T_CmpGE);
    CASE(S2_T_OpShiftRight);
    CASE(S2_T_OpShiftRightAssign);
    CASE(S2_T_OpShiftRightAssign3);
    CASE(S2_T_Question);
    CASE(S2_T_QDot);
    CASE(S2_T_BraceOpen);
    CASE(S2_T_Backslash);
    CASE(S2_T_BraceClose);
    CASE(S2_T_BraceGroup);
    CASE(S2_T_ArrayAppend);
    CASE(S2_T_OpXOr);
    CASE(S2_T_OpXOrAssign);
    CASE(S2_T_OpXOrAssign3);
    CASE(S2_T_SquigglyOpen);
    CASE(S2_T_SquigglyBlock);
    CASE(S2_T_OpOrBitwise);
    CASE(S2_T_OpOr);
    CASE(S2_T_OpOr3);
    CASE(S2_T_OpElvis);
    CASE(S2_T_OpOrAssign);
    CASE(S2_T_OpOrAssign3);
    CASE(S2_T_SquigglyClose);
    CASE(S2_T_OpNegateBitwise);

    CASE(S2_T_Literal__);
    CASE(S2_T_LiteralInt);
    CASE(S2_T_LiteralIntDec);
    CASE(S2_T_LiteralIntHex);
    CASE(S2_T_LiteralIntOct);
    CASE(S2_T_LiteralIntBin);
    CASE(S2_T_LiteralDouble);
    CASE(S2_T_LiteralStringDQ);
    CASE(S2_T_LiteralStringSQ);
    CASE(S2_T_LiteralString);
    CASE(S2_T_PropertyKey);
    CASE(S2_T_Identifier);
    CASE(S2_T_ValueTypes__);
    CASE(S2_T_Value);
    CASE(S2_T_Undefined);
    CASE(S2_T_Null);
    CASE(S2_T_False);
    CASE(S2_T_True);
    CASE(S2_T_Object);
    CASE(S2_T_Array);
    CASE(S2_T_Function);

    CASE(S2_T_Keyword__);
    CASE(S2_T_KeywordAffirm);
    CASE(S2_T_KeywordAssert);
    CASE(S2_T_KeywordBREAKPOINT);
    CASE(S2_T_KeywordBreak);
    CASE(S2_T_KeywordCOLUMN);
    CASE(S2_T_KeywordCatch);
    CASE(S2_T_KeywordClass);
    CASE(S2_T_KeywordConst);
    CASE(S2_T_KeywordContinue);
    CASE(S2_T_KeywordDefine);
    CASE(S2_T_KeywordDefined);
    CASE(S2_T_KeywordDelete);
    CASE(S2_T_KeywordDo);
    CASE(S2_T_KeywordEcho);
    CASE(S2_T_KeywordEnum);
    CASE(S2_T_KeywordEval);
    CASE(S2_T_KeywordException);
    CASE(S2_T_KeywordExit);
    CASE(S2_T_KeywordFILE);
    CASE(S2_T_KeywordFILEDIR);
    CASE(S2_T_KeywordFalse);
    CASE(S2_T_KeywordFatal);
    CASE(S2_T_KeywordFor);
    CASE(S2_T_KeywordForEach);
    CASE(S2_T_KeywordFunction);
    CASE(S2_T_KeywordIf);
    CASE(S2_T_KeywordImport);
    CASE(S2_T_KeywordInclude);
    CASE(S2_T_KeywordInterface);
    CASE(S2_T_KeywordIs);
    CASE(S2_T_KeywordIsA);
    CASE(S2_T_KeywordLINE);
    CASE(S2_T_KeywordNameof);
    CASE(S2_T_KeywordNew);
    CASE(S2_T_KeywordNull);
    CASE(S2_T_KeywordPragma);
    CASE(S2_T_KeywordPrivate);
    CASE(S2_T_KeywordProc);
    CASE(S2_T_KeywordProtected);
    CASE(S2_T_KeywordPublic);
    CASE(S2_T_KeywordReturn);
    CASE(S2_T_KeywordRefcount);
    CASE(S2_T_KeywordS2Out);
    CASE(S2_T_KeywordSRCPOS);
    CASE(S2_T_KeywordScope);
    CASE(S2_T_KeywordStatic);
    CASE(S2_T_KeywordThrow);
    CASE(S2_T_KeywordTrue);
    CASE(S2_T_KeywordTry);
    CASE(S2_T_KeywordTypeinfo);
    CASE(S2_T_KeywordTypename);
    CASE(S2_T_KeywordUndefined);
    CASE(S2_T_KeywordUnset);
    CASE(S2_T_KeywordUKWD);
    CASE(S2_T_KeywordUsing);
    CASE(S2_T_KeywordVar);
    CASE(S2_T_KeywordWhile);

    CASE(S2_T_Comment__);
    CASE(S2_T_CommentC);
    CASE(S2_T_CommentCpp);
    CASE(S2_T_Mark__);
    CASE(S2_T_MarkVariadicStart);
    CASE(S2_T_Misc__);
    CASE(S2_T_Foo);
    CASE(S2_T_comma_kludge_);
#undef CASE
  }
#if 0
  /* Make ttypes matching characters in the ASCII range (even
     unprintable ones) their own strings. Exception: the value 0 is
     reserved for S2_T_INVALID, so we don't have a string for the
     token \0. We don't really need these in scripts, though, and NOT
     having these allows code to call this function to determine
     whether or not it's a known-to-s2 token type.
  */
  if(ttype>0 && ttype<=127){
    static char singleChars[127*2] =
      {0} /* all characters in (1..127), each followed by a NUL */;
    int ntype = ttype-1;
    if(0 == singleChars[0]){
      int i = 0, n = 1;
      for(; n <=127; ++n, ++i ){
        singleChars[i*2] = (char)n;
        singleChars[i*2+1] = 0;
      }
    }
    return &singleChars[ntype*2];
  }
#endif
  return 0;
}

int s2_ptoken_has_content( s2_ptoken const * tok ){
  char const * begin;
  char const * end;
  if(s2_ptoken_adjbegin(tok)){
    begin = s2_ptoken_adjbegin(tok);
    end = s2_ptoken_adjend(tok);
  }else{
    begin = s2_ptoken_begin(tok);
    end = s2_ptoken_end(tok);
  }
  assert(end >= begin);
  if(begin == end) return 0;
  else{
    s2_ptoker pr = s2_ptoker_empty;
    s2_ptoker_init( &pr, begin, (int)(end - begin) );
    do {
      if( s2_ptoker_next_token(&pr) ) return 0;
    }while( s2_ttype_is_junk(pr.token.ttype) );
    return s2_ttype_is_eof(pr.token.ttype) ? 0 : pr.token.ttype;
  }
}

/**
   Expects digits to point to digLen bytes with the ASCII values '0'
   or '1'. It parses them as a binary value. On success, if out is not
   NULL then the final parsed result is written there, and it returns
   0. On error (non-binary-digit encountered) then CWAL_RC_RANGE is
   returned and out is not modified.

   The digits may contain any number of '_' characters, which are
   treated as "visual separators" (i.e. simply skipped).

   Minor achtung: this routine accepts, for simplicity, '_' as a
   leading or trailing character but the core tokenizer does not. It
   expects to be fed only inputs which have already been vetted by the
   tokenizer.
*/
static int s2_parse_binary_digits( char const * digits,
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

/**
   The octal-digit counterpart of s2_parse_binary_digits(), and works
   identically except that it requires octal-digit input.
*/
static int s2_parse_octal_digits( char const * digits,
                                  cwal_size_t digLen,
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

/**
   The decimal-digit counterpart of s2_parse_binary_digits(), and
   works identically except that it requires decimal-digit input.
*/
static int s2_parse_decimal_digits( char const * digits,
                                    cwal_size_t digLen,
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
  if(out) *out = (cwal_int_t)i;
  return 0;
}

/**
   The hex-digit counterpart of s2_parse_binary_digits(), and works
   identically except that it requires hex-digit input.
*/
static int s2_parse_hex_digits( char const * digits,
                                cwal_size_t digLen,
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

char s2_ptoken_parse_int( s2_ptoken const * t, cwal_int_t * rc ){
  switch(t->ttype){
    case S2_T_LiteralIntDec:
      assert(s2_ptoken_end(t) > s2_ptoken_begin(t));
      return s2_parse_decimal_digits(s2_ptoken_begin(t),
                                     s2_ptoken_len(t),
                                     rc)
        ? 0 : 1;
    case S2_T_LiteralIntOct:
      assert( s2_ptoken_len(t) > 2 /*"0o" prefix */);
      return s2_parse_octal_digits( s2_ptoken_begin(t)+2,
                                    s2_ptoken_len(t)-2,
                                    rc )
        ? 0
        : 1;
    case S2_T_LiteralIntHex:
      assert( s2_ptoken_len(t) > 2 /*"0x" prefix */);
      return s2_parse_hex_digits( s2_ptoken_begin(t)+2,
                                  s2_ptoken_len(t)-2,
                                  rc )
        ? 0
        : 1;
    case S2_T_LiteralIntBin:
      assert( s2_ptoken_len(t) > 2/*"0b" prefix*/);
      return s2_parse_binary_digits( s2_ptoken_begin(t)+2,
                                     s2_ptoken_len(t)-2,
                                     rc )
        ? 0
        : 1;
    default:
      return 0;
  }
}

char s2_ptoken_parse_double( s2_ptoken const * t, cwal_double_t * dest ){
  if(t->ttype == S2_T_LiteralDouble){
    if(dest){
      return cwal_cstr_to_double( s2_ptoken_begin(t),
                                  s2_ptoken_len(t), dest)
        ? 0 : 1;
    }
    return 1;
  }else{
    return 0;
  }
}

char s2_cstr_parse_int( char const * str, cwal_int_t slen, cwal_int_t * result ){
  s2_ptoker pr = s2_ptoker_empty;
  int sign = 0;
  cwal_int_t rv = 0;
  if(slen<0) slen = (cwal_int_t)cwal_strlen(str);
  while( slen && s2_is_space(*str) ){
    ++str;
    --slen;
  }
  if(str && slen){
    if( '-' == *str || '+' == *str ){
      sign = ('-'==*str) ? -1 : 1;
      ++str;
      --slen;
    }
  }
  while( slen && s2_is_space(*str) ){
    ++str;
    --slen;
  }
  if(!slen) return 0;
  s2_ptoker_init( &pr, str, slen );
  if(s2_ptoker_next_token(&pr)
     || !s2_ptoken_parse_int( &pr.token, &rv )
     ){
    return 0;
  }else{
    s2_ptoken_begin_set(&pr.token, s2_ptoken_end(&pr.token));
    s2_ptoken_end_set(&pr.token, s2_ptoker_end(&pr));
    if(s2_ptoken_has_content(&pr.token)){
      /* trailing junk */
      return 0;
    }else{
      if(result) *result = sign < 0 ? -rv : rv;
      return 1;
    }
  }
}


char s2_cstr_parse_double( char const * str, cwal_int_t slen, cwal_double_t * result ){
  s2_ptoker pr = s2_ptoker_empty;
  int sign = 0;
  cwal_double_t rv = 0;
  if(slen<0) slen = (cwal_int_t)cwal_strlen(str);
  while( slen && s2_is_space(*str) ){
    ++str;
    --slen;
  }
  if(str && slen){
    if( '-' == *str || '+' == *str ){
      sign = ('-'==*str) ? -1 : 1;
      ++str;
      --slen;
    }
  }
  while( slen && s2_is_space(*str) ){
    ++str;
    --slen;
  }
  if(!slen) return 0;
  s2_ptoker_init( &pr, str, slen );
  if(s2_ptoker_next_token(&pr)
     || !s2_ptoken_parse_double( &pr.token, &rv)){
    return 0;
  }else{
    s2_ptoken_begin_set(&pr.token, s2_ptoken_end(&pr.token));
    s2_ptoken_end_set(&pr.token, s2_ptoker_end(&pr));
    if(s2_ptoken_has_content(&pr.token)){
      /* trailing junk */
      return 0;
    }else{
      if(result) *result = sign < 0 ? -rv : rv;
      return 1;
    }
  }
}

char const * s2_last_path_sep(char const * str, cwal_size_t slen ){
  unsigned char const * pos = (unsigned char const *)str;
  unsigned char const * end = pos ? pos + slen : 0;
  unsigned char const * prevSep = 0;
  int sep = 0;
  /**
     TODO 20200828: we're doing this the hard way. We can simply start
     at the end and backtrack for '/' or '\\'.
  */
  if(!str || !slen) return 0;
  while( pos && pos<end ){
    int const ch = cwal_utf8_read_char( pos, end, &pos );
    if(end==pos) break;
    else if(!sep && ((int)'/'==ch || (int)'\\'==ch)){
      sep = ch;
    }
    if(sep == ch) prevSep = pos;
  }
  return (char const *)prevSep;
}

#if 0
cwal_size_t s2_ptoken_len( s2_ptoken const * token ){
  return (s2_ptoken_begin(token)
          && s2_ptoken_end(token) > s2_ptoken_begin(token))
    ? (cwal_size_t)(s2_ptoken_end(token) - s2_ptoken_begin(token))
    : 0;
}
cwal_size_t s2_ptoken_len2( s2_ptoken const * token ){
  if(token->adjBegin && token->adjEnd && token->adjBegin<=token->adjEnd){
    return (cwal_size_t)(token->adjEnd - token->adjBegin);
  }
  return (token->begin && token->end > token->begin)
    ? (cwal_size_t)(token->end - token->begin)
    : 0;
}
#endif

char const * s2_ptoken_cstr( s2_ptoken const * tok,
                             cwal_size_t * len ){
  if(len) *len = s2_ptoken_len(tok);
  return s2_ptoken_begin(tok);
}

char const * s2_ptoken_cstr2( s2_ptoken const * tok,
                              cwal_size_t * len ){
  if(len) *len = s2_ptoken_len2(tok);
  return s2_ptoken_adjbegin(tok)
    ? s2_ptoken_adjbegin(tok)
    : s2_ptoken_begin(tok);
}

cwal_value * s2_ptoken_is_tfnu( s2_ptoken const * tok ){
  cwal_value * rv = 0;
  if(S2_T_Identifier == tok->ttype){
    cwal_size_t const tlen = s2_ptoken_len(tok);
    switch(tlen){
      case 4:
        if(0==cwal_compare_cstr("true", 4,
                                s2_ptoken_begin(tok), tlen)){
          rv = cwal_value_true();
        }else if(0==cwal_compare_cstr("null", 4,
                                      s2_ptoken_begin(tok), tlen)){
          rv = cwal_value_null();
        }
        break;
      case 5:
        if(0==cwal_compare_cstr("false", 5,
                                s2_ptoken_begin(tok), tlen)){
          rv = cwal_value_false();
        }
        break;
      case 9:
        if(0==cwal_compare_cstr("undefined", 9,
                                s2_ptoken_begin(tok), tlen)){
          rv = cwal_value_undefined();
        }
        break;
      default:
        break;
    }
  }
  return rv;
}

char const * s2_ptoker_err_pos( s2_ptoker const * pt ){
  char const * rc = s2_ptoker_err_pos_first(pt, 0);
  if(!rc){
    if(s2_ptoken_begin(&pt->token) < s2_ptoken_end(&pt->token)){
      rc = s2_ptoken_begin(&pt->token);
    }
    if(!rc){
      if(s2_ptoken_begin(&pt->_pbToken) < s2_ptoken_end(&pt->_pbToken)){
        rc = s2_ptoken_begin(&pt->_pbToken);
      }
      if(!rc) rc = s2_ptoker_begin(pt);
    }
#if 0
    if(rc>=s2_ptoker_end(pt) && s2_ptoker_end(pt)>s2_ptoker_begin(pt)) rc = s2_ptoker_end(pt)-1;
#endif
  }
  return rc;
}


void s2_ptoker_errtoken_set( s2_ptoker * const st, s2_ptoken const * const tok ){
  st->_errToken = tok ? *tok : s2_ptoken_empty;
}

s2_ptoken const * s2_ptoker_errtoken_get( s2_ptoker const * st ){
  return &st->_errToken;
}

char s2_ptoker_errtoken_has( s2_ptoker const * st ){
  return s2_ptoken_begin(&st->_errToken)
    && s2_ptoken_begin(&st->_errToken) >= s2_ptoker_begin(st)
    && s2_ptoken_begin(&st->_errToken) < s2_ptoker_end(st)
    ? 1 : 0;
}

char const * s2_ptoker_capture_cstr( s2_ptoker const * st,
                                     cwal_size_t * len ){
  char const * rc = 0;
  char const * begin = s2_ptoken_begin(&st->capture.begin);
  char const * end = s2_ptoken_begin(&st->capture.end);
  if(begin && begin>=s2_ptoker_begin(st)
     && end>=begin && end<=s2_ptoker_end(st)){
    rc = begin;
    if(len) *len = (cwal_size_t)(end - begin);
  }
  return rc;
}

void s2_path_toker_init( s2_path_toker * pt, char const * path, cwal_int_t len ){
  *pt = s2_path_toker_empty;
  pt->pos = pt->begin = path;
  pt->end = pt->begin + ((len>=0) ? (cwal_size_t)len : cwal_strlen(path));
}

int s2_path_toker_next( s2_path_toker * pt, char const ** token,
                        cwal_size_t * len ){
  if(!pt->pos || pt->pos>=pt->end) return CWAL_RC_RANGE;
  else if(!pt->separators || !*pt->separators) return CWAL_RC_MISUSE;
  else{
    char const * pos = pt->pos;
    char const * t;
    char const * sep;
    for( sep = pt->separators; *sep; ++sep){
      if(*sep & 0x80) return CWAL_RC_MISUSE;
      /* non-ASCII */
    }
    for( ; pos<pt->end; ){
      /*skip leading separators*/
      for( sep = pt->separators;
           *sep && *pos!=*sep; ++sep ){
      }
      if(*pos == *sep) ++pos;
      else break;
    }
    t = pos;
    for( ; pos<pt->end; ){
      /*skip until the next separator*/
      for( sep = pt->separators;
           *sep && *pos!=*sep; ++sep ){
      }
      if(*pos == *sep) break;
      else ++pos;
    }
    pt->pos = pos;
    if(pos>t){
      *token = t;
      *len = (cwal_size_t)(pos - t);
      return 0;
    }
    return CWAL_RC_NOT_FOUND;
  }
}

#if S2_T10N_LCCACHE
#  undef s2_ptoker_lc_slot
#else
#  undef s2_ptoker_lc_search
#endif
#undef MARKER
#undef S2_T10N_LCCACHE
#undef S2_T10N_LCCACHE_SLOT
#undef LCMARKER
