/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "t10n.h"

/** T10N_COMPILE is/was an attempt to hide a c9n_toker behind the
    t10n_toker interface, for the sake of s2. It may or may not be
    a feasible thing to do in general, but its only real benefit
    (retro-fitting s2 for pre-processed tokens) cannot currently
    (2021-06-26) work because s2 has about half a dozen places
    which use/abuse the tokenizer in ways that c9n does not/cannot
    support. */
#define T10N_COMPILE 0
#if T10N_COMPILE
#  include "c9n.h"
#  define COMPILED_MEMBER(T10N_PTOKER) ((c9n_toker*)(T10N_PTOKER)->compiled)
#  define COMPILED_MEMBER_C(T10N_PTOKER) ((c9n_toker const*)(T10N_PTOKER)->compiled)
#  define COMPILED_DECL(T10N_PTOKER) c9n_toker * compiled = COMPILED_MEMBER(T10N_PTOKER)
#  define COMPILED_DECL_C(T10N_PTOKER) c9n_toker const * compiled = COMPILED_MEMBER_C(T10N_PTOKER)
#endif

#if 1
#include <stdio.h>
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:\t",__FILE__,__LINE__); if(1) printf pfexp
#else
#define MARKER(pfexp) (void)0
#endif

/* const t10n_byte_range t10n_byte_range_empty = t10n_byte_range_empty_m; */
const t10n_toker t10n_toker_empty = t10n_toker_empty_m;
const t10n_token t10n_token_empty = t10n_token_empty_m;
const t10n_path_toker t10n_path_toker_empty = t10n_path_toker_empty_m;

/**
   T10N_LCCACHE: set to 0 to disable the line-counting cache. It
   appears to work, saving more than half of the CPU cycles in the s2
   unit tests, but is not yet battle-hardened (or even optimized).

   Sidebar: those tests are essentially a pathological case, as they
   intentionally throw many exceptions in their testing duties,
   exceptions being a major culprit in the line-counting rabbit hole,
   along with function definitions.
*/
#define T10N_LCCACHE 1
#if T10N_LCCACHE
#  define T10N_LCCACHE_SLOT 0
/*
  T10N_LCCACHE_SLOT == whether or not to use a predictable cache
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
static const int t10n_toker_lccache_size = sizeof(t10n_toker_empty._lcCache.lines)
  /sizeof(t10n_toker_empty._lcCache.lines[0]);
#else
#  define T10N_LCCACHE_SLOT 0
#endif

#if 0 && T10N_LCCACHE
#  define LCMARKER MARKER
#else
#  define LCMARKER(PFEXP) (void)0
#endif

#if T10N_LCCACHE && T10N_LCCACHE_SLOT
#  if 1
/* Only a miniscule performance improvement. */
#    define t10n_toker_lc_slot(PT,POS) (PT)->_lcCache.slotSize ? (int)(POS - (PT)->begin) / (PT)->_lcCache.slotSize : 0
#  else
/*inline (not C89)*/ static int t10n_toker_lc_slot(t10n_toker const * pt, char const * pos){
  assert(pos >= pt->begin);
  assert(pos < pt->end);
#    if 1
  return pt->_lcCache.slotSize ? (int)(pos - pt->begin) / pt->_lcCache.slotSize : 0;
#    else
  /* Performs worse, but the fallback lookup is not optimized for this case. */
  return pt->_lcCache.slotSize ? (int)(pos - pt->begin) % t10n_toker_lccache_size : 0;
#    endif
}
#  endif
#endif

#if T10N_LCCACHE
static t10n_toker_lccache_entry const *
t10n_toker_lc_search( t10n_toker const * pt, char const * pos ){
  if(pos>=pt->begin && pos<pt->end){
    t10n_toker_lccache const * const lcc = &pt->_lcCache;
#if T10N_LCCACHE_SLOT
    int slot = t10n_toker_lc_slot(pt, pos);
    assert(slot < t10n_toker_lccache_size);
    while( slot >= 0 ){
      t10n_toker_lccache_entry const * const rc
        = (t10n_toker_lccache_entry const *)&lcc->lines[slot];
      if(rc->pos && rc->pos <= pos){
        LCMARKER(("%p Search slot %d, possible hit for %d, %d\n",
                  (void const *)pt, slot, rc->line, rc->col));
        return rc;
      }
      --slot;
    }
    return 0;
#else
    t10n_toker_lccache_entry const * rc = 0;
    int i;
    for( i = 0; i < t10n_toker_lccache_size; ++i ){
      t10n_toker_lccache_entry const * const m = (t10n_toker_lccache_entry const *)&lcc->lines[i];
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
#  define t10n_toker_lc_search(X,Y) 0
#endif

#if T10N_LCCACHE
static void t10n_toker_lc_cache( t10n_toker const * pt, char const * pos, int line, int col ){
  if(pos>=pt->begin && pos<pt->end){
    t10n_toker_lccache * lcc = (t10n_toker_lccache *)&pt->_lcCache;
    static int once = 0;
    t10n_toker_lccache_entry * m;
#if T10N_LCCACHE_SLOT
    int const slot = t10n_toker_lc_slot(pt, pos);
    assert(slot < t10n_toker_lccache_size);
    m = (t10n_toker_lccache_entry *)&lcc->lines[slot];
    /*if(m->pos && pos>=m->pos) return; wow, this adds 37M instructions
      in these tests!*/
#else
    if(lcc->cursor==t10n_toker_lccache_size){
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
    m = (t10n_toker_lccache_entry *)&lcc->lines[lcc->cursor];
#endif
    if(!once++){
      LCMARKER(("sizeof(t10n_toker_lccache)=%d\n",(int)sizeof(t10n_toker_lccache)));
    }
    m->line = line;
    m->col = col;
    m->pos = pos;
#if T10N_LCCACHE_SLOT
    LCMARKER(("%p Cached slot %d @ %d, %d\n", (void const *)pt, slot, m->line, m->col));
#else
    LCMARKER(("%p Cached slot %d @ %d, %d\n", (void const *)pt, lcc->cursor, m->line, m->col));
    ++lcc->cursor;
#endif
  }
}
#endif

int t10n_toker_init_v2( cwal_engine * e, t10n_toker * t, char const * src, cwal_int_t len,
                       uint32_t flags ){
  if(!t||!src) return CWAL_RC_MISUSE;
  else{
    *t = t10n_toker_empty;
    t->e = e;
    t->flags = flags;
    /*memset(&t->_lcCache, 0, sizeof(t->_lcCache));*/
    if(len<0) len = cwal_strlen(src);
    t->begin = src;
    t->end = src+len;
    memset(&t->_lcCache, 0, sizeof(t->_lcCache));
    t10n_toker_reset(t);
#if T10N_LCCACHE_SLOT
    {
      int ptl = (int)len;
      int const cs = t10n_toker_lccache_size;
      assert(0==t->_lcCache.lines[cs-1].pos);
      t->_lcCache.slotSize = (ptl + (ptl / cs)) / cs + 1;
      LCMARKER(("%p slot size = %d\n", (void const *)t, t->_lcCache.slotSize));
    }
#endif
    return 0;
  }
}

int t10n_toker_init( t10n_toker * t, char const * src, cwal_int_t len,
                     uint32_t flags ){
  return t10n_toker_init_v2( NULL, t, src, len, flags );
}

void t10n_toker_reset( t10n_toker * t ){
  t->_pbToken = t->_errToken = t->token = t10n_token_empty;
  t10n_token_begin_set(&t->token, t10n_toker_begin(t))
    /* begin w/o an end set is our internal signal that we're
       just starting off tokenization */;
  t->currentLine = 1;
  t->currentCol = 0;
}

#if T10N_COMPILE
/** Returns true if pt seems to own pt->compiled. */
static int t10n_toker_owns_compiled(t10n_toker const * const pt){
  if(!pt->parent || !pt->parent->compiled) return 1;
  else return pt->parent->compiled != pt->compiled;
}
#endif

void t10n_toker_finalize( t10n_toker * pt ){
#if T10N_COMPILE
  COMPILED_DECL(pt);
  assert(pt->begin ? !!pt->e : 1);
  if(compiled && t10n_toker_owns_compiled(pt)){
    c9n_toker_free(compiled);
  }
#endif
  *pt = t10n_toker_empty;
}

int t10n_toker_sub_from_token( t10n_toker const * parent,
                              t10n_token const * tok,
                              t10n_toker * dest ){
  char const * begin;
  char const * end;
  int rc;
#if 0
  /* 20200107: this breaks stuff */
  if(!t10n_ttype_is_group(tok->ttype)){
    return CWAL_RC_TYPE;
  }
#endif
  if(t10n_token_adjbegin(tok)){
    begin = t10n_token_adjbegin(tok);
    end = t10n_token_adjend(tok);
  }else{
    begin = t10n_token_begin(tok);
    end = t10n_token_end(tok);
  }
  if(!begin || (begin>end)) return CWAL_RC_RANGE;
  rc = t10n_toker_init_v2( parent->e, dest, begin,
                          (cwal_int_t)(end - begin), 0 );
  if(!rc){
    dest->parent = parent;
    assert(parent->e == dest->e);
  }
  return rc;
}

int t10n_toker_sub_from_toker( t10n_toker const * parent, t10n_toker * sub ){
  int const rc = t10n_toker_sub_from_token( parent, &parent->token, sub );
  if(!rc){
    assert(sub->parent == parent);
    assert(sub->e == parent->e);
    /* There are multiple TODOs here if/when we add compiled token
       chains. */
  }
  return rc;
}

t10n_toker const * t10n_toker_top_parent( t10n_toker const * t ){
  while( t && t->parent ){
    t = t->parent;
  }
  return t;
}
char const * t10n_toker_name_first( t10n_toker const * t, cwal_size_t * len ){
  while(t && !t->name){
    t = t->parent;
  }
  if(t && len && t->name) *len = cwal_strlen(t->name);
  return t ? t->name : 0;
}

char const * t10n_toker_err_pos_first( t10n_toker const * t,
                                      t10n_toker const ** foundIn){
  while(t && !t10n_token_begin(&t->_errToken)){
    t = t->parent;
  }
  if(t && foundIn) *foundIn = t;
  return t ? t10n_token_begin(&t->_errToken) : 0;
}

char const * t10n_toker_name_top( t10n_toker const * t ){
  char const * n = t ? t->name : 0;
  while(t && t->parent){
    t = t->parent;
    if(t && t->name) n = t->name;
  }
  return n;
}

int t10n_ttype_is_junk( int ttype ){
  switch(ttype){
    case ' ':
    case '\t':
    case '\r':
    case T10N_T_Blank:
    case T10N_T_CommentC:
    case T10N_T_CommentCpp:
    case T10N_T_Whitespace:
    case T10N_T_Shebang:
    case T10N_T_UTFBOM:
      return ttype;
    default:
      return 0;
  }
}

int t10n_ttype_is_assignment( int ttype ){
  switch(ttype){
    case T10N_T_OpAssign:
    case T10N_T_OpAssign3:
    case T10N_T_ArrayAppend:
      return ttype;
    default:
      return 0;
  }
}

int t10n_ttype_is_assignment_combo( int ttype ){
  switch(ttype){
    case T10N_T_OpPlusAssign:
    case T10N_T_OpPlusAssign3:
    case T10N_T_OpMinusAssign:
    case T10N_T_OpMinusAssign3:
    case T10N_T_OpModuloAssign:
    case T10N_T_OpModuloAssign3:
    case T10N_T_OpDivideAssign:
    case T10N_T_OpDivideAssign3:
    case T10N_T_OpMultiplyAssign:
    case T10N_T_OpMultiplyAssign3:
    case T10N_T_OpShiftLeftAssign:
    case T10N_T_OpShiftLeftAssign3:
    case T10N_T_OpShiftRightAssign:
    case T10N_T_OpShiftRightAssign3:
    case T10N_T_OpXOrAssign:
    case T10N_T_OpXOrAssign3:
    case T10N_T_OpOrAssign:
    case T10N_T_OpOrAssign3:
    case T10N_T_OpAndAssign:
    case T10N_T_OpAndAssign3:
      return ttype;
    default:
      return 0;
  }
}

int t10n_ttype_is_group( int ttype ){
  switch(ttype){
    case T10N_T_ParenGroup:
    case T10N_T_BraceGroup:
    case T10N_T_SquigglyGroup:
      return ttype;
    default:
      return 0;
  }
}

int t10n_ttype_is_eof( int ttype ){
  switch(ttype){
    case T10N_T_EOF:
      return ttype;
    default:
      return 0;
  }
}

int t10n_ttype_is_eol( int ttype ){
  switch(ttype){
    case T10N_T_EOF:
    case T10N_T_EOL:
    case T10N_T_CR:
    case T10N_T_NL:
    /* case T10N_T_CommentCpp: A //-style comment implies a newline, so
       we might consider treating it as one (and consuming a trailing
       newline, if any, as part of the token).
    */
      return ttype;
    default:
      return 0;
  }
}

int t10n_ttype_is_eox( int ttype ){
  switch(ttype){
    case T10N_T_EOF:
    case T10N_T_EOX:
    case T10N_T_Semicolon:
      return ttype;
    default:
      return 0;
  }
}

int t10n_ttype_is_space( int ttype ){
  switch(ttype){
    case T10N_T_NL:
    case T10N_T_CR:
    case T10N_T_EOL:
    case T10N_T_Whitespace:
    /* case T10N_T_CommentCpp: */
    /* case T10N_T_CommentC: */
    case ' ':
    case '\t':
    case '\v':
    case '\f':
      return 1;
    default:
      return 0;
  }
}

int t10n_ttype_is_object_keyable( int ttype ){
  switch(ttype){
    case T10N_T_LiteralIntBin:
    case T10N_T_LiteralIntDec:
    case T10N_T_LiteralIntOct:
    case T10N_T_LiteralIntHex:
    case T10N_T_LiteralDouble:
    case T10N_T_LiteralStringSQ:
    case T10N_T_LiteralStringDQ:
    case T10N_T_LiteralString:
    case T10N_T_Identifier:
      return ttype;
    default:
      return 0;
  }
}

int t10n_ttype_is_int( int ttype ){
  switch(ttype){
    case T10N_T_LiteralIntBin:
    case T10N_T_LiteralIntDec:
    case T10N_T_LiteralIntOct:
    case T10N_T_LiteralIntHex:
      return ttype;
    default:
      return 0;
  }
}

int t10n_ttype_is_number( int ttype ){
  switch(ttype){
    case T10N_T_LiteralIntBin:
    case T10N_T_LiteralIntDec:
    case T10N_T_LiteralIntOct:
    case T10N_T_LiteralIntHex:
    case T10N_T_LiteralDouble:
      return ttype;
    default:
      return 0;
  }
}

int t10n_toker_is_eof( t10n_toker const * st ){
  return t10n_ttype_is_eof(st->token.ttype);
}

int t10n_toker_is_eox( t10n_toker const * st ){
  return t10n_ttype_is_eox(st->token.ttype);
}

int t10n_ttype_short_circuits( int ttype ){
  switch( ttype ){
    case T10N_T_OpOr:
    case T10N_T_OpAnd:
    case T10N_T_Question:
      return ttype;
    default: return 0;
  }
}


int t10n_ttype_is_identifier_prefix( int ttype ){
  switch( ttype ){
    case T10N_T_OpIncr:
    case T10N_T_OpIncrPre:
    case T10N_T_OpIncrPost:
    case T10N_T_OpDecr:
    case T10N_T_OpDecrPre:
    case T10N_T_OpDecrPost:
      return ttype;
    default: return 0;
  }
}

int t10n_ttype_may_precede_unary( int ttype ){
  switch(ttype){
#if 1
    /* (...) and [...] are handled at the eval level,
       and are simply treated as values. */
    case T10N_T_ParenOpen:
    case T10N_T_ParenGroup:
      /* No! case T10N_T_ParenClose: */
    case T10N_T_BraceOpen:
    case T10N_T_BraceGroup:
      /* No! case T10N_T_BraceClose: */
#endif
    case T10N_T_Comma:
    case T10N_T_Semicolon:
    case T10N_T_Colon:
    case T10N_T_OpArrow: /* So x-> -1 can work. */
    case T10N_T_OpNot:
    case T10N_T_OpAnd:
    case T10N_T_OpAssign:
    case T10N_T_OpAssign3:
    /* case T10N_T_OpArrow2: */
    case T10N_T_OpOr:
    case T10N_T_OpOr3:
    case T10N_T_OpElvis:
    case T10N_T_OpPlus:
    case T10N_T_OpPlusUnary:
    case T10N_T_OpMinus:
    case T10N_T_OpMinusUnary:
    case T10N_T_OpMultiply:
    case T10N_T_OpDivide:
    case T10N_T_OpModulo:
    case T10N_T_OpOrBitwise:
    case T10N_T_OpNegateBitwise:
    case T10N_T_OpXOr:
    case T10N_T_CmpLT:
    case T10N_T_CmpGT:
    case T10N_T_CmpLE:
    case T10N_T_CmpGE:
    case T10N_T_CmpEq:
    case T10N_T_CmpNotEq:
    case T10N_T_CmpEqStrict:
    case T10N_T_CmpNotEqStrict:
    case T10N_T_OpContains:
    case T10N_T_OpNotContains:
    case T10N_T_OpAndBitwise:
    case T10N_T_KeywordThrow:
    case T10N_T_KeywordAssert:
    case T10N_T_KeywordReturn:
    case T10N_T_ArrayAppend /* b/c this op slides over its '=' part */:
      return ttype;
    default:
      return t10n_ttype_is_assignment_combo(ttype);
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
static char t10n_is_id_char( int ch, char isStart ) {
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
          ? t10n_is_alpha(ch)
          : t10n_is_alnum(ch);
    }
}

void t10n_toker_putback_set( t10n_toker * const st,
                            t10n_token const * const tok ){
  st->_pbToken = *tok;
}

t10n_token const * t10n_toker_putback_get( t10n_toker const * const st ){
  return &st->_pbToken;
}

char t10n_toker_putback( t10n_toker * st ){
  char const rc = t10n_token_begin(&st->_pbToken) ? 1 : 0;
  if(rc){
    st->token = st->_pbToken;
    st->_pbToken = t10n_token_empty;
    /*t10n_toker_token_set(st, &st->_pbToken);*/
  }
  return rc;
}


int t10n_toker_lookahead( t10n_toker * st, t10n_token * tgt ){
  t10n_token const oldT = st->token;
  t10n_token const oldP = st->_pbToken;
  int const rc = t10n_toker_next_token( st );
  *tgt = st->token;
  st->token = oldT;
  st->_nextToken = *tgt;
  st->_pbToken = oldP;
  return rc;
}

/**
   Internal impl for t10n_toker_lookahead_xxx_pred().

   matchMode: true means skip as long as the predicate matches. False
   means skip until the predicate matches. i.e. true implements a 'while'
   loop and false implements a 'do-until' loop.

   Returns 0 on success.
*/
static int t10n_toker_lookahead_pred( t10n_toker * st, t10n_token * tgt,
                                     t10n_ttype_predicate_f pred, char matchMode ){
  t10n_token const oldT = st->token;
  t10n_token const oldP = st->_pbToken;
  int rc = 0;
  assert(tgt);
  while( !(rc = t10n_toker_next_token( st ))
         && (matchMode ? pred(st->token.ttype) : !pred(st->token.ttype))
         && !t10n_toker_is_eof(st)){
  }
  st->_nextToken = t10n_token_empty;
  *tgt = st->token;
  t10n_toker_token_set(st, &oldT);
  /*if(!rc) st->_nextToken = *tgt;*/
  st->_pbToken = oldP;
  return rc;
}

int t10n_toker_lookahead_skip( t10n_toker * st, t10n_token * tgt,
                               t10n_ttype_predicate_f pred ){
  return t10n_toker_lookahead_pred( st, tgt, pred, 1 );
}

int t10n_toker_lookahead_until( t10n_toker * st, t10n_token * tgt,
                               t10n_ttype_predicate_f pred ){
  return t10n_toker_lookahead_pred( st, tgt, pred, 0 );
}

void t10n_toker_token_set( t10n_toker * const st, t10n_token const * const t ){
  /* Need a temp in case t== one of st's tokens */
  t10n_token const tmp = *t;
  st->_pbToken = st->token;
  st->token = tmp;
  st->_nextToken = t10n_token_empty;
}

#if 0
t10n_token const * t10n_toker_token_get( t10n_toker const * st ){
  return &st->token;
}
int t10n_toker_ttype( t10n_toker const * st ){
  return st->token.ttype;
}
#endif

void t10n_toker_next_token_set( t10n_toker * pt, t10n_token const * tk ){
  pt->_nextToken = *tk;
}

int t10n_toker_next_token( t10n_toker * t ){
  int rc = 0;
  t10n_token * pt = &t->token;
  char const * curpos = t10n_token_end(&t->token)
    ? t10n_token_end(&t->token) /* for the 2nd and subsequent calls */
    : t10n_token_begin(&t->token) /* for the first run through this function */;
  /**
    TODO, but it'd be a lot of work: rewrite this to use a uint32_t
    current character, rather than (char const *), to handle
    UTF better from here.
  */
  assert(curpos);
  assert(curpos >= t10n_toker_begin(t));
  assert(curpos <= t10n_toker_end(t));
  t->_pbToken = t->token;
  t10n_token_adjbegin_set(&t->token, 0);
  t10n_token_adjend_set(&t->token, 0);
  t->errMsg = 0;
  t->_errToken = t10n_token_empty;
  if(t10n_token_begin(&t->_nextToken)){
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
              t10n_ttype_cstr(t->_nextToken.ttype), ++counter));
    }
#endif
    t->token = t->_nextToken;
    t->_nextToken = t10n_token_empty;
    return 0;
#else
    /* Just to compare results in cachegrind... */
    curpos = t10n_token_begin(&t->_nextToken);
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
#define RETURN_ERR(RC,MSG) pt->ttype = T10N_T_TokErr; t->errMsg = MSG; rc=RC; goto end
#define NEXT_LINE  ++t->currentLine; t->currentCol = 0
  
  if( curpos >= t10n_toker_end(t) ) {
    pt->ttype = T10N_T_EOF;
    t10n_token_begin_set(&t->token, t10n_toker_end(t));
    t10n_token_end_set(&t->token, t10n_toker_end(t));
    return 0;
  }

  if(!t->parent && curpos == t10n_toker_begin(t)){
    /* Check for some things which can only appear at the start of a
       script. The if() condition above isn't quite 100% accurate,
       considering how s2 uses sub-tokenizers at times, but it's
       pretty close.
    */
    if('#'==*curpos && '!'==*(curpos+1)){
      /* Workaround: strip shebang line from start of scripts. */
      for( ; ++curpos < t10n_toker_end(t)
             && *curpos
             && ('\n' != *curpos); ++t->currentCol){}
      ++curpos /* skip NL */;
      if( curpos >= t10n_toker_end(t) ) {
        pt->ttype = T10N_T_EOF;
        t10n_token_begin_set(&t->token, t10n_toker_end(t));
        t10n_token_end_set(&t->token, t10n_toker_end(t));
        return 0;
      }
      t10n_token_end_set(&t->token, curpos);
      t->token.ttype = T10N_T_Shebang;
      ++t->currentLine;
      t->currentCol = 0;
      return 0;
    }else{
      /* Check for a UTF-8 BOM. */
      unsigned char const * ccp = (unsigned char const*)curpos;
      if(0xEF==*ccp && 0xBB==ccp[1] && 0xBF==ccp[2]){
        curpos += 3;
        t10n_token_end_set(&t->token, curpos);
        t->token.ttype = T10N_T_UTFBOM;
        t->currentCol += 3;
        return 0;
      }
    }
  }

  pt->ttype = T10N_T_INVALID;
  t10n_token_begin_set(&t->token, curpos);

  switch(*curpos){
    case 0:
      pt->ttype = T10N_T_EOF;
      /*This is a necessary exception to the bump-on-consume
        rule.*/
      break;
    case '\\': /* Treat \<NEWLINE> as a continuation of a line */
      if(('\n'==*(curpos+1)) || (('\r'==*(curpos+1)) && ('\n'==*(curpos+2)))){
        pt->ttype = T10N_T_Whitespace;
        BUMP(('\r'==*(curpos+1)) ? 3 : 2);
        NEXT_LINE;
      }else{
        RETURN_ERR(CWAL_SCR_SYNTAX,"Unexpected backslash.");
      }
      break;
    case '\r':
      if('\n'==*(curpos+1)){
        pt->ttype = T10N_T_EOL;
        BUMP(2);
        NEXT_LINE;
      }
      else{
        pt->ttype = T10N_T_CR;
        BUMP(1);
      }
      break;
    case '\n':
      pt->ttype = T10N_T_NL;
      BUMP(1);
      NEXT_LINE;
      break;
    case ' ':
    case '\t':
    case '\v':
    case '\f':
      pt->ttype = *curpos /*T10N_T_Blank*/;
      BUMP(1);
#if 1
      while( curpos<t10n_toker_end(t)
             && *curpos && t10n_is_blank(*curpos) ){
        pt->ttype = T10N_T_Blank;
        BUMP(1);
      }
#endif
      break;
    case ':': /* colon or namespace */
      if( ':' == *(curpos+1) ){
        pt->ttype = T10N_T_Colon2;
        BUMP(2);
      }else if( '=' == *(curpos+1) ){
        pt->ttype = T10N_T_OpColonEqual;
        BUMP(2);
      }else{
        pt->ttype = T10N_T_Colon;
        BUMP(1);
      }
      break;
    case '~': /* ~ or ~= */
      pt->ttype = *curpos;
      BUMP(1);
#if 0
      if('=' == *curpos){
        pt->ttype = T10N_T_NotYet;
        BUMP(1);
      }
#endif
      break;
    case '/': /* numeric division or C-style comment block */
      pt->ttype = T10N_T_OpDivide /* *curpos */;
      BUMP(1);
      switch(*curpos){
        case (int)'=':
          pt->ttype = T10N_T_OpDivideAssign;
          BUMP(1);
          break;
        case (int)'*':/* C-style comment block */
          pt->ttype = T10N_T_CommentC;
          BUMP(1);
          do{
            while( curpos<t10n_toker_end(t)
                   && *curpos && ('*' != *curpos) ){
              if('\n'==*curpos){
                NEXT_LINE;
              }
              BUMP(1);
            }
            if(t10n_toker_end(t)==curpos){
              RETURN_ERR(CWAL_SCR_SYNTAX,
                         "Reached (virtual) EOF while looking for "
                         "C-style comment closer.");
            }
            else if( t10n_toker_end(t)<=curpos || !*curpos ) break;
            BUMP(1); /* skip '*' */
          } while( curpos < t10n_toker_end(t)
                   && *curpos && ('/' != *curpos));
          if( curpos < t10n_toker_end(t) && *curpos != '/' ){
            RETURN_ERR(CWAL_SCR_SYNTAX,"End of C-style comment not found."); 
          }else{
            BUMP(1); /* get that last slash */
          }
          break;
        case (int)'/':/* C++-style comment line */
          BUMP(1);
          pt->ttype = T10N_T_CommentCpp;
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
      char const * begin = curpos;
      pt->ttype = ('"' == *curpos)
        ? T10N_T_LiteralStringDQ
        : T10N_T_LiteralStringSQ;
      BUMP(1)/*leading quote*/;
      while(curpos<t10n_toker_end(t) && *curpos && (*curpos != quote)){
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
      if(t10n_toker_end(t)<=curpos || *curpos != quote){
        RETURN_ERR(CWAL_SCR_SYNTAX,
                   "Unexpected end of string literal.");
      }else{
        BUMP(1)/*trailing quote*/;
      }
      pt->_adjBegin = begin+1;
      pt->_adjEnd = curpos-1;
      break;
    } /* end literal string */
    case '&': /* & or && or &= */
      pt->ttype = T10N_T_OpAndBitwise;
      BUMP(1);
      switch(*curpos){
        case '&':
          pt->ttype = T10N_T_OpAnd;
          BUMP(1);
          break;
        case '=':
          pt->ttype = T10N_T_OpAndAssign;
          BUMP(1);
          break;
      }
      break;
    case '|': /* | or || or |= or ||| */
      pt->ttype = T10N_T_OpOrBitwise;
      BUMP(1);
      switch(*curpos){
        case '|':
          pt->ttype = T10N_T_OpOr;
          BUMP(1);
          if( '|' == *curpos ){
            pt->ttype = T10N_T_OpOr3;
            BUMP(1);
          }
          break;
        case '=':
          pt->ttype = T10N_T_OpOrAssign;
          BUMP(1);
          break;
      }
      break;
    case '?': /* ? or ?: */
      pt->ttype = T10N_T_Question;
      BUMP(1);
      if( ':' == *curpos ){
        pt->ttype = T10N_T_OpElvis;
        BUMP(1);
      }
      break;
    case '^': /* ^ or ^= */
      pt->ttype = T10N_T_OpXOr;
      BUMP(1);
      if( '=' == *curpos ){
        pt->ttype = T10N_T_OpXOrAssign;
        BUMP(1);
      }
      break;
    case '+': /* + or ++ or += */{
      pt->ttype = T10N_T_OpPlus;
      BUMP(1);
      switch(*curpos){
        case '+': /* ++ */
          pt->ttype = T10N_T_OpIncr;
          BUMP(1);
          break;
        case '=': /* += */
          pt->ttype = T10N_T_OpPlusAssign;
          BUMP(1);
          break;
        default: break;
      }
      break;
    }
    case '*': /* * or *= */
      pt->ttype = T10N_T_OpMultiply;
      BUMP(1);
      switch(*curpos){
        case '=':
          pt->ttype = T10N_T_OpMultiplyAssign;
          BUMP(1);
          break;
        case '/':
          pt->ttype = T10N_T_INVALID;
          RETURN_ERR(CWAL_SCR_SYNTAX,
                     "Comment closer (*/) not inside a comment.");
          break;
      }
      break;
    case '-': /* - or -- or -= */{
      pt->ttype = T10N_T_OpMinus;
      BUMP(1);
      switch( *curpos ){
        case '-': /* -- */
          pt->ttype = T10N_T_OpDecr;
          BUMP(1);
          break;
        case '=':
          pt->ttype = T10N_T_OpMinusAssign;
          BUMP(1);
          break;
        case '>':
          pt->ttype = T10N_T_OpArrow;
          BUMP(1);
          break;
        default: break;
      }
      break;
    }
    case '<': /* LT or << or <= or <<= or <<< */{
      pt->ttype = T10N_T_CmpLT;
      BUMP(1);
      switch( *curpos ){
        case '<': /* tok= << */ {
          pt->ttype = T10N_T_OpShiftLeft;
          BUMP(1);
          switch(*curpos){
            case '=': /* <<= */
              pt->ttype = T10N_T_OpShiftLeftAssign;
              BUMP(1);
              break;
            case '<': /* <<< */
              pt->ttype = T10N_T_HeredocStart;
              BUMP(1);
              break;
            default: break;
          }
          break;
        }
        case '=': /* tok= <= */
          pt->ttype = T10N_T_CmpLE;
          BUMP(1);
          break;
        default: break;
      }
      break;
    }
    case '>': /* GT or >> or >= or >>= */{
      pt->ttype = T10N_T_CmpGT;
      BUMP(1);
      switch(*curpos){
        case '>': /* >> */
          pt->ttype = T10N_T_OpShiftRight;
          BUMP(1);
          if( '=' == *curpos ){/* >>= */
            pt->ttype = T10N_T_OpShiftRightAssign;
            BUMP(1);
          }
          break;
        case '=': /* >= */
          pt->ttype = T10N_T_CmpGE;
          BUMP(1);
          break;
        default: break;
      }
      break;
    }
    case '=': /* = or == or === or =~ or => */
      pt->ttype = T10N_T_OpAssign;
      BUMP(1);
      switch( *curpos ){
        case '~': /* -- */
          pt->ttype = T10N_T_OpContains;
          BUMP(1);
          break;
        case '=':
          pt->ttype = T10N_T_CmpEq;
          BUMP(1);
          if( '=' == *curpos ){
            pt->ttype = T10N_T_CmpEqStrict;
            BUMP(1);
          }
          break;
        case '>':
          pt->ttype = T10N_T_OpArrow2;
          BUMP(1);
          break;
        default: break;
      }
      break;
    case '%': /* % or %= */
      pt->ttype = T10N_T_OpModulo /* *curpos */;
      BUMP(1);
      if( '=' == *curpos ){
        pt->ttype = T10N_T_OpModuloAssign;
        BUMP(1);
      }
      break;
    case '!': /* ! or != or !== or !~ */
      pt->ttype = T10N_T_OpNot;
      BUMP(1);
      if( '~' == *curpos ){
        pt->ttype = T10N_T_OpNotContains;
        BUMP(1);
      }else if( '=' == *curpos ){
        pt->ttype = T10N_T_CmpNotEq;
        BUMP(1);
        if( '=' == *curpos ){
          pt->ttype = T10N_T_CmpNotEqStrict;
          BUMP(1);
        }
      }
      break;
    case '.': /* . or .. */
      pt->ttype = T10N_T_OpDot;
      BUMP(1);
      if( '.' == *curpos ){
        pt->ttype = T10N_T_OpDotDot;
        BUMP(1);
      }
      break;
    case '{': /* { */
      pt->ttype = T10N_T_SquigglyOpen;
      BUMP(1);
      break;
    case '}': /* } */
      pt->ttype = T10N_T_SquigglyClose;
      BUMP(1);
      break;
    case '0': /* 0 or hex or octal or binary literals */
      BUMP(1);
      if(t10n_toker_end(t) <= curpos){
        /* special case: 0 at the end of input. */
        pt->ttype = T10N_T_LiteralIntDec;
        break;
      }
      switch (*curpos)/* try hex or octal or binary */{
        case 'x':
        case 'X':{/** hex digit. */
          int digitCount = 0;
          BUMP(1);
          while(curpos<t10n_toker_end(t) && *curpos){
            if('_'==*curpos){
              BUMP(1);
              continue /* separator */;
            }else if(t10n_is_xdigit(*curpos)){
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
            pt->ttype = T10N_T_LiteralIntHex;
            if('_' == curpos[-1] /* last char was a separator */
               || t10n_is_alnum(*curpos)
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
          while(curpos<t10n_toker_end(t) && *curpos){
            if('_'==*curpos){
              BUMP(1);
              continue /* separator */;
            }else if(t10n_is_octaldigit(*curpos)){
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
            pt->ttype = T10N_T_LiteralIntOct;
            if('_' == curpos[-1] /* last char was a separator */
               || t10n_is_alnum(*curpos)
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
            pt->ttype = T10N_T_LiteralIntBin;
            if('_' == curpos[-1] /* last char was a separator */
               || t10n_is_alnum(*curpos)
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
                          t10n_is_alnum(*curpos)
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
            if(!t10n_is_digit(curpos[1])){
              /* curpos -= 1 */ /* Assume this might be NUMBER.FUNC(),
                 back up and leave it for next time */;
              pt->ttype = T10N_T_LiteralIntDec;
              break;
            }
            BUMP(1);
            while( t10n_is_digit(*curpos) ){
              BUMP(1);
            }
            if('.'==*(curpos-1)){
              RETURN_ERR(CWAL_SCR_SYNTAX,
                         "Mis-terminated floating point value.");
            }
            pt->ttype = T10N_T_LiteralDouble;
          }
          else {
            pt->ttype = T10N_T_LiteralIntDec;
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
      while(curpos<t10n_toker_end(t) && *curpos){
        /* integer or first part of a double. */
        if('_'==*curpos){
          ++gotSep;
          BUMP(1);
          continue;
        }else if(t10n_is_digit(*curpos)){
          BUMP(1);
          continue;
        }
        break;
      }
      if( curpos<t10n_toker_end(t)
          && ('.' == *curpos) && t10n_is_digit(*(curpos+1)) ){
        /* double number */
        if(gotSep){
          RETURN_ERR(CWAL_SCR_SYNTAX,
                     "'_' separators are not legal in floating-point literals.");
        }
        pt->ttype = T10N_T_LiteralDouble;
        BUMP(1);
        while(curpos<t10n_toker_end(t) && *curpos && t10n_is_digit(*curpos)){
          BUMP(1);
        }
      }
      else {
        pt->ttype = T10N_T_LiteralIntDec;
      }
      if( (curpos[-1] == '_'
           /* disallow trailing separator for symmetry
              with 0x/0o/0b literals. */)
          || (curpos<t10n_toker_end(t)
              && *curpos
              && (t10n_is_alnum(*curpos)
                  || (*curpos == '_')))
          ) {
        BUMP(1); /* make sure it shows up in the error string. */
        RETURN_ERR(CWAL_SCR_SYNTAX,
                   "Malformed numeric literal.");
      }
      break;
    }
    case '@':
      pt->ttype = T10N_T_At;
      BUMP(1);
      break;
  }/* end of switch(*curpos) */

  if(0==rc
     && (curpos == t10n_token_begin(&t->token))
     && (T10N_T_EOF != pt->ttype) ){
    /* keep trying... */
    int const idChars = t10n_read_identifier2( curpos, t10n_toker_end(t),
                                               &curpos, t->flags );
    if( idChars ) /* identifier string */{
      t->currentCol += idChars;
      pt->ttype = T10N_T_Identifier;
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
      assert(curpos == t10n_token_begin(&t->token));
      assert(T10N_T_INVALID==pt->ttype);
    }
  }

  if(T10N_T_INVALID == pt->ttype){
    unsigned char const cCh = (unsigned char)*curpos;
    if( cCh > 0x7F )
      /* _hope_ for a UTF8 identifier string! */{
      int const chars = t10n_read_identifier2( curpos, t10n_toker_end(t),
                                             &curpos, t->flags );
      if(chars){
        t->currentCol += chars;
        pt->ttype = T10N_T_Identifier;
      }else{
        t->errMsg = "Don't know how to tokenize this.";
        rc = CWAL_SCR_SYNTAX;
      }
    }else{
      pt->ttype = T10N_T_TokErr;
      /* MARKER(("byte=%02x\n", (unsigned char)*curpos)); */
      t->errMsg = "Don't know how to tokenize this.";
      rc = CWAL_SCR_SYNTAX;
    }
  }
  t10n_token_end_set(&t->token, (curpos > t10n_toker_end(t))
                    ? t10n_toker_end(t) : curpos);
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

int t10n_toker_next_token_skip_junk( t10n_toker * st ){
  int rc = 0;
  do{
    rc = t10n_toker_next_token(st);
  }while(!rc && t10n_ttype_is_junk(st->token.ttype));
  return rc;
}

char t10n_is_space( int ch ){
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

char t10n_is_blank( int ch ){
  switch(ch){
    case ' ':
    case '\t':
      return 1;
    default:
      return 0;
  }
}

char t10n_is_digit( int ch ){
  return '0'<=ch && '9'>=ch;
}

char t10n_is_xdigit( int ch ){
  return ('a'<=ch && 'f'>=ch)
    ? 1
    : (('A'<=ch && 'F'>=ch)
       ? 1
       :t10n_is_digit(ch));
}

char t10n_is_octaldigit( int ch ){
  return ('0'<=ch && '7'>=ch);
}

char t10n_is_alpha( int ch ){
  return ('a'<=ch && 'z'>=ch)
    ? 1
    : ('A'<=ch && 'Z'>=ch);
}

char t10n_is_alnum( int ch ){
  return t10n_is_alpha(ch) ? 1 : t10n_is_digit(ch);
}

int t10n_read_identifier2( char const * zPos,
                         char const * zEnd,
                         char const ** zIdEnd,
                         uint32_t flags ){
  unsigned char const * start = (unsigned char const *) zPos;
  unsigned char const * pos = start;
  unsigned char const * end = (unsigned char const *) zEnd;
  unsigned char const * endChar = pos;
  int ch;
  int rc = 0;
  int const allowDash = (T10N_F_IDENTIFIER_DASHES & flags);
  assert(zEnd>zPos);
  for( ; pos < end; ){
    ch = cwal_utf8_read_char( pos, end, &endChar );
    if(endChar == pos) break;
    else if(!t10n_is_id_char(ch, (pos==start) ? 1 : 0)){
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

int t10n_read_identifier( char const * zPos,
                        char const * zEnd,
                        char const ** zIdEnd ){
  return t10n_read_identifier2( zPos, zEnd, zIdEnd, 0 );
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
    (byte & (unsigned)0x3f) | (*codep << 6) :
    (0xff >> type) & (byte);

  *state = utf8d[256 + *state*16 + type];
  return *state;
}
#endif

int t10n_count_lines( char const * src, char const * end_,
                    char const * pos_,
                    t10n_linecol_t *line, t10n_linecol_t *col ){
  t10n_linecol_t ln = 1, c = 0;
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

       Need to see if we can build line/column counting into t10n_toker
       and t10n_toker_next_token(). (That failed because of how we
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
      if(*x > (unsigned char)127) x += lengths[*x>>3];
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
        if(*x>(unsigned char)127){
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

int t10n_toker_count_lines( t10n_toker const * pt, char const * pos,
                           t10n_linecol_t * line, t10n_linecol_t * col ){
  t10n_toker const * top;
  t10n_toker const * prev;
  int rc = 0;
  t10n_linecol_t li = 0, c = 0;
  t10n_linecol_t offLine = 0, offCol = 0;
  t10n_toker_lccache_entry const * cachedPos;
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
  assert(t10n_toker_begin(pt) >= t10n_toker_begin(top)
         && t10n_toker_end(pt) <= t10n_toker_end(top));
  assert(pos<t10n_toker_end(pt) && pos >= t10n_toker_begin(top));
  if(T10N_F_LINEAR_TOKER & pt->flags){
    t10n_token const * et = t10n_token_begin(&pt->_errToken)
      ? &pt->_errToken
      : &pt->token;
    li = et->line;
    c = et->column;
    /*MARKER(("Using T10N_F_LINEAR_TOKER line %d, col %d\n",
      li, c));*/
  }
  if(!li && (cachedPos = t10n_toker_lc_search(top, pos))){
    LCMARKER(("%p Got a hit: (%d, %d) distance from target pos=%d, from start=%d\n",
              (void const *)top, cachedPos->line, cachedPos->col,
              (int)(pos - cachedPos->pos), (int)(pos - t10n_toker_begin(top))
              ));
    if(cachedPos->pos==pos){
      li = cachedPos->line;
      c = cachedPos->col;
    }else{
      offLine = cachedPos->line;
      offCol = cachedPos->col;
      rc = t10n_count_lines( cachedPos->pos, t10n_toker_end(pt), pos,
                           &li, &c );
      assert(!rc);
    }
  }else if(!li){
    rc = t10n_count_lines( t10n_toker_begin(top), t10n_toker_end(pt),
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
#if T10N_LCCACHE
      /*LCMARKER(("%p Attempting to cache entry (%d, %d)\n", (void const *)top, li, c));*/
    t10n_toker_lc_cache(top, pos, li, c);
#endif
  }
  return rc;
}

int t10n_toker_count_lines2( t10n_toker const * pt,
                            t10n_token const * tok,
                            t10n_linecol_t * line, t10n_linecol_t * col ){
  t10n_linecol_t li = 0, c = 0;
  t10n_toker const * top;
  int rc = 0;
  if(!tok) tok = &pt->token;
  for( top = pt; top; top = top->parent){
    if(top->lineOffset) break;
  }
  if(!top) top = t10n_toker_top_parent(pt);
  if(tok->line){
    MARKER(("Using token-embedded line %d, col %d [%.*s] parent?=%d\n",
            tok->line, tok->column,
            (int)t10n_token_len(tok), t10n_token_begin(tok),
            !!pt->parent));
    li = tok->line;
    c = tok->column;
  }else{
    rc = t10n_count_lines( t10n_toker_begin(top), t10n_toker_end(pt),
                         t10n_token_begin(tok), &li, &c );
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

static int t10n_hexbyte( int ch ){
  if(ch>='0' && ch<='9') return ch - '0';
  else if(ch>='a' && ch<='f') return ch - 'a' + 10;
  else if(ch>='A' && ch<='F') return ch - 'A' + 10;
  else return -1;
}

static int t10n_read_hex_bytes(unsigned char const *zPos,
                             unsigned int howMany,
                             unsigned int * rv ){
  unsigned int rc = 0;
  int ch1, ch2;
  unsigned int n = 0;
  assert(!(howMany%2) && "howMany must be an even number!");
  assert(zPos && rv);
  while(n<howMany){
    ch1 = *zPos ? t10n_hexbyte(*zPos++) : -1;
    ch2 = (ch1>=0) ? t10n_hexbyte(*zPos++) : -1;
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
int t10n_unescape_string( cwal_engine * e,
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
            const unsigned ulen = 'u'==*p ? (unsigned)4 : (unsigned)8;
            uCount = t10n_read_hex_bytes(p+1, ulen, &uChar);
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

    
char const * t10n_ttype_cstr( int ttype ){
  switch((enum t10n_token_types_e)ttype){
    /* ^^^^ reminder: that cast is so that gcc will warn
       when i forget to update this function after adding
       new entries to that enum. */
#define CASE(TT) case TT: return &(#TT[5])/*==prefix strlen("T10N_T_")*/
    CASE(T10N_T_TokErr);
    CASE(T10N_T_INVALID);
    CASE(T10N_T_EOF);
    CASE(T10N_T_EOX);
    CASE(T10N_T_Tab);
    CASE(T10N_T_NL);
    CASE(T10N_T_VTab);
    CASE(T10N_T_FF);
    CASE(T10N_T_CR);
    CASE(T10N_T_EOL);
    CASE(T10N_T_Space);
    CASE(T10N_T_Blank);
    CASE(T10N_T_Whitespace);
    CASE(T10N_T_At);
    CASE(T10N_T_UTFBOM);
    CASE(T10N_T_OpNot);
    CASE(T10N_T_OpHash);
    CASE(T10N_T_Shebang);
    CASE(T10N_T_OpModulo);
    CASE(T10N_T_OpModuloAssign);
    CASE(T10N_T_OpModuloAssign3);
    CASE(T10N_T_OpAndBitwise);
    CASE(T10N_T_OpAnd);
    CASE(T10N_T_OpAndAssign);
    CASE(T10N_T_OpAndAssign3);
    CASE(T10N_T_ParenOpen);
    CASE(T10N_T_ParenGroup);
    CASE(T10N_T_ParenClose);
    CASE(T10N_T_OpMultiply);
    CASE(T10N_T_OpMultiplyAssign);
    CASE(T10N_T_OpMultiplyAssign3);
    CASE(T10N_T_OpPlus);
    CASE(T10N_T_OpPlusUnary);
    CASE(T10N_T_OpPlusAssign);
    CASE(T10N_T_OpPlusAssign3);
    CASE(T10N_T_OpIncr);
    CASE(T10N_T_OpIncrPre);
    CASE(T10N_T_OpIncrPost);
    CASE(T10N_T_Comma);
    CASE(T10N_T_RHSEval);
    CASE(T10N_T_OpMinus);
    CASE(T10N_T_OpMinusUnary);
    CASE(T10N_T_OpMinusAssign);
    CASE(T10N_T_OpMinusAssign3);
    CASE(T10N_T_OpDecr);
    CASE(T10N_T_OpDecrPre);
    CASE(T10N_T_OpDecrPost);
    CASE(T10N_T_OpDot);
    CASE(T10N_T_OpDotDot);
    CASE(T10N_T_OpDotLength);
    CASE(T10N_T_OpInherits);
    CASE(T10N_T_OpNotInherits);
    CASE(T10N_T_OpContains);
    CASE(T10N_T_OpNotContains);
    CASE(T10N_T_OpArrow);
    CASE(T10N_T_OpArrow2);
    CASE(T10N_T_OpDivide);
    CASE(T10N_T_OpDivideAssign);
    CASE(T10N_T_OpDivideAssign3);
    CASE(T10N_T_Colon);
    CASE(T10N_T_Colon2);
    CASE(T10N_T_OpColonEqual);
    CASE(T10N_T_Semicolon);
    CASE(T10N_T_CmpLT);
    CASE(T10N_T_CmpLE);
    CASE(T10N_T_OpShiftLeft);
    CASE(T10N_T_OpShiftLeftAssign);
    CASE(T10N_T_OpShiftLeftAssign3);
    CASE(T10N_T_HeredocStart);
    CASE(T10N_T_Heredoc);
    CASE(T10N_T_OpAssign);
    CASE(T10N_T_OpAssign3);
    CASE(T10N_T_OpAssignConst3);
    CASE(T10N_T_CmpEq);
    CASE(T10N_T_CmpNotEq);
    CASE(T10N_T_CmpEqStrict);
    CASE(T10N_T_CmpNotEqStrict);
    CASE(T10N_T_CmpGT);
    CASE(T10N_T_CmpGE);
    CASE(T10N_T_OpShiftRight);
    CASE(T10N_T_OpShiftRightAssign);
    CASE(T10N_T_OpShiftRightAssign3);
    CASE(T10N_T_Question);
    CASE(T10N_T_QDot);
    CASE(T10N_T_BraceOpen);
    CASE(T10N_T_Backslash);
    CASE(T10N_T_BraceClose);
    CASE(T10N_T_BraceGroup);
    CASE(T10N_T_ArrayAppend);
    CASE(T10N_T_OpXOr);
    CASE(T10N_T_OpXOrAssign);
    CASE(T10N_T_OpXOrAssign3);
    CASE(T10N_T_SquigglyOpen);
    CASE(T10N_T_SquigglyGroup);
    CASE(T10N_T_OpOrBitwise);
    CASE(T10N_T_OpOr);
    CASE(T10N_T_OpOr3);
    CASE(T10N_T_OpElvis);
    CASE(T10N_T_OpOrAssign);
    CASE(T10N_T_OpOrAssign3);
    CASE(T10N_T_SquigglyClose);
    CASE(T10N_T_OpNegateBitwise);

    CASE(T10N_T_Literal__);
    CASE(T10N_T_LiteralInt);
    CASE(T10N_T_LiteralIntDec);
    CASE(T10N_T_LiteralIntHex);
    CASE(T10N_T_LiteralIntOct);
    CASE(T10N_T_LiteralIntBin);
    CASE(T10N_T_LiteralDouble);
    CASE(T10N_T_LiteralStringDQ);
    CASE(T10N_T_LiteralStringSQ);
    CASE(T10N_T_LiteralString);
    CASE(T10N_T_PropertyKey);
    CASE(T10N_T_Identifier);
    CASE(T10N_T_ValueTypes__);
    CASE(T10N_T_Value);
    CASE(T10N_T_Undefined);
    CASE(T10N_T_Null);
    CASE(T10N_T_False);
    CASE(T10N_T_True);
    CASE(T10N_T_Object);
    CASE(T10N_T_Array);
    CASE(T10N_T_Function);

    CASE(T10N_T_Keyword__);
    CASE(T10N_T_KeywordAffirm);
    CASE(T10N_T_KeywordAssert);
    CASE(T10N_T_KeywordBREAKPOINT);
    CASE(T10N_T_KeywordBreak);
    CASE(T10N_T_KeywordCOLUMN);
    CASE(T10N_T_KeywordCatch);
    CASE(T10N_T_KeywordClass);
    CASE(T10N_T_KeywordConst);
    CASE(T10N_T_KeywordContinue);
    CASE(T10N_T_KeywordDefine);
    CASE(T10N_T_KeywordDefined);
    CASE(T10N_T_KeywordDelete);
    CASE(T10N_T_KeywordDo);
    CASE(T10N_T_KeywordEcho);
    CASE(T10N_T_KeywordEnum);
    CASE(T10N_T_KeywordEval);
    CASE(T10N_T_KeywordException);
    CASE(T10N_T_KeywordExit);
    CASE(T10N_T_KeywordFILE);
    CASE(T10N_T_KeywordFILEDIR);
    CASE(T10N_T_KeywordFalse);
    CASE(T10N_T_KeywordFatal);
    CASE(T10N_T_KeywordFor);
    CASE(T10N_T_KeywordForEach);
    CASE(T10N_T_KeywordFunction);
    CASE(T10N_T_KeywordIf);
    CASE(T10N_T_KeywordImport);
    CASE(T10N_T_KeywordInclude);
    CASE(T10N_T_KeywordInterface);
    CASE(T10N_T_KeywordIs);
    CASE(T10N_T_KeywordIsA);
    CASE(T10N_T_KeywordLINE);
    CASE(T10N_T_KeywordNameof);
    CASE(T10N_T_KeywordNew);
    CASE(T10N_T_KeywordNull);
    CASE(T10N_T_KeywordPragma);
    CASE(T10N_T_KeywordPrivate);
    CASE(T10N_T_KeywordProc);
    CASE(T10N_T_KeywordProtected);
    CASE(T10N_T_KeywordPublic);
    CASE(T10N_T_KeywordReturn);
    CASE(T10N_T_KeywordRefcount);
    CASE(T10N_T_KeywordS2Out);
    CASE(T10N_T_KeywordSRCPOS);
    CASE(T10N_T_KeywordScope);
    CASE(T10N_T_KeywordStatic);
    CASE(T10N_T_KeywordThrow);
    CASE(T10N_T_KeywordTrue);
    CASE(T10N_T_KeywordTry);
    CASE(T10N_T_KeywordTypeinfo);
    CASE(T10N_T_KeywordTypename);
    CASE(T10N_T_KeywordUndefined);
    CASE(T10N_T_KeywordUnset);
    CASE(T10N_T_KeywordUKWD);
    CASE(T10N_T_KeywordUsing);
    CASE(T10N_T_KeywordVar);
    CASE(T10N_T_KeywordWhile);

    CASE(T10N_T_Comment__);
    CASE(T10N_T_CommentC);
    CASE(T10N_T_CommentCpp);
    CASE(T10N_T_Mark__);
    CASE(T10N_T_MarkVariadicStart);
    CASE(T10N_T_Misc__);
    CASE(T10N_T_Foo);
    CASE(T10N_T_comma_kludge_);
#undef CASE
  }
#if 0
  /* Make ttypes matching characters in the ASCII range (even
     unprintable ones) their own strings. Exception: the value 0 is
     reserved for T10N_T_INVALID, so we don't have a string for the
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

int t10n_token_has_content( t10n_token const * tok ){
  char const * begin;
  char const * end;
  if(t10n_token_adjbegin(tok)){
    begin = t10n_token_adjbegin(tok);
    end = t10n_token_adjend(tok);
  }else{
    begin = t10n_token_begin(tok);
    end = t10n_token_end(tok);
  }
  assert(end >= begin);
  if(begin == end) return 0;
  else{
    t10n_toker pr = t10n_toker_empty;
    t10n_toker_init( &pr, begin, (int)(end - begin), 0 );
    do {
      if( t10n_toker_next_token(&pr) ) return 0;
    }while( t10n_ttype_is_junk(pr.token.ttype) );
    return t10n_ttype_is_eof(pr.token.ttype) ? 0 : pr.token.ttype;
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
static int t10n_parse_binary_digits( char const * digits,
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
   The octal-digit counterpart of t10n_parse_binary_digits(), and works
   identically except that it requires octal-digit input.
*/
static int t10n_parse_octal_digits( char const * digits,
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
   The decimal-digit counterpart of t10n_parse_binary_digits(), and
   works identically except that it requires decimal-digit input.
*/
static int t10n_parse_decimal_digits( char const * digits,
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
   The hex-digit counterpart of t10n_parse_binary_digits(), and works
   identically except that it requires hex-digit input.
*/
static int t10n_parse_hex_digits( char const * digits,
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

bool t10n_token_parse_int( t10n_token const * t, cwal_int_t * rc ){
  switch(t->ttype){
    case T10N_T_LiteralIntDec:
      assert(t10n_token_end(t) > t10n_token_begin(t));
      return t10n_parse_decimal_digits(t10n_token_begin(t),
                                     t10n_token_len(t),
                                     rc)
        ? 0 : 1;
    case T10N_T_LiteralIntOct:
      assert( t10n_token_len(t) > 2 /*"0o" prefix */);
      return t10n_parse_octal_digits( t10n_token_begin(t)+2,
                                    t10n_token_len(t)-2,
                                    rc )
        ? 0
        : 1;
    case T10N_T_LiteralIntHex:
      assert( t10n_token_len(t) > 2 /*"0x" prefix */);
      return t10n_parse_hex_digits( t10n_token_begin(t)+2,
                                  t10n_token_len(t)-2,
                                  rc )
        ? 0
        : 1;
    case T10N_T_LiteralIntBin:
      assert( t10n_token_len(t) > 2/*"0b" prefix*/);
      return t10n_parse_binary_digits( t10n_token_begin(t)+2,
                                     t10n_token_len(t)-2,
                                     rc )
        ? 0
        : 1;
    default:
      return 0;
  }
}

bool t10n_token_parse_double( t10n_token const * t, cwal_double_t * dest ){
  if(t->ttype == T10N_T_LiteralDouble){
    if(dest){
      return cwal_cstr_to_double( t10n_token_begin(t),
                                  t10n_token_len(t), dest)
        ? 0 : 1;
    }
    return 1;
  }else{
    return 0;
  }
}

bool t10n_cstr_parse_int( char const * str, cwal_int_t slen, cwal_int_t * result ){
  t10n_toker pr = t10n_toker_empty;
  int sign = 0;
  cwal_int_t rv = 0;
  if(slen<0) slen = (cwal_int_t)cwal_strlen(str);
  while( slen && t10n_is_space(*str) ){
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
  while( slen && t10n_is_space(*str) ){
    ++str;
    --slen;
  }
  if(!slen) return false;
  t10n_toker_init( &pr, str, slen, 0 );
  if(t10n_toker_next_token(&pr)
     || !t10n_token_parse_int( &pr.token, &rv )
     ){
    return false;
  }else{
    t10n_token_begin_set(&pr.token, t10n_token_end(&pr.token));
    t10n_token_end_set(&pr.token, t10n_toker_end(&pr));
    if(t10n_token_has_content(&pr.token)){
      /* trailing junk */
      return false;
    }else{
      if(result) *result = sign < 0 ? -rv : rv;
      return true;
    }
  }
}


bool t10n_cstr_parse_double( char const * str, cwal_int_t slen, cwal_double_t * result ){
  t10n_toker pr = t10n_toker_empty;
  int sign = 0;
  cwal_double_t rv = 0;
  if(slen<0) slen = (cwal_int_t)cwal_strlen(str);
  while( slen && t10n_is_space(*str) ){
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
  while( slen && t10n_is_space(*str) ){
    ++str;
    --slen;
  }
  if(!slen) return false;
  t10n_toker_init( &pr, str, slen, 0 );
  if(t10n_toker_next_token(&pr)
     || !t10n_token_parse_double( &pr.token, &rv)){
    return false;
  }else{
    t10n_token_begin_set(&pr.token, t10n_token_end(&pr.token));
    t10n_token_end_set(&pr.token, t10n_toker_end(&pr));
    if(t10n_token_has_content(&pr.token)){
      /* trailing junk */
      return false;
    }else{
      if(result) *result = sign < 0 ? -rv : rv;
      return true;
    }
  }
}

char const * t10n_last_path_sep(char const * str, cwal_size_t slen ){
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
cwal_size_t t10n_token_len( t10n_token const * token ){
  return (t10n_token_begin(token)
          && t10n_token_end(token) > t10n_token_begin(token))
    ? (cwal_size_t)(t10n_token_end(token) - t10n_token_begin(token))
    : 0;
}
cwal_size_t t10n_token_len2( t10n_token const * token ){
  if(token->adjBegin && token->adjEnd && token->adjBegin<=token->adjEnd){
    return (cwal_size_t)(token->adjEnd - token->adjBegin);
  }
  return (token->begin && token->end > token->begin)
    ? (cwal_size_t)(token->end - token->begin)
    : 0;
}
#endif

char const * t10n_token_cstr( t10n_token const * tok,
                               cwal_size_t * len,
                               bool innerOnly ){
  const char * const adjBegin =
    innerOnly ? t10n_token_adjbegin(tok) : NULL;
  if(adjBegin){
    if(len) *len = t10n_token_len2(tok);
    return adjBegin;
  }
  if(len) *len = t10n_token_len(tok);
  return t10n_token_begin(tok);
}

cwal_value * t10n_token_is_tfnu( t10n_token const * tok ){
  cwal_value * rv = 0;
  if(T10N_T_Identifier == tok->ttype){
    cwal_size_t const tlen = t10n_token_len(tok);
    switch(tlen){
      case 4:
        if(0==cwal_compare_cstr("true", 4,
                                t10n_token_begin(tok), tlen)){
          rv = cwal_value_true();
        }else if(0==cwal_compare_cstr("null", 4,
                                      t10n_token_begin(tok), tlen)){
          rv = cwal_value_null();
        }
        break;
      case 5:
        if(0==cwal_compare_cstr("false", 5,
                                t10n_token_begin(tok), tlen)){
          rv = cwal_value_false();
        }
        break;
      case 9:
        if(0==cwal_compare_cstr("undefined", 9,
                                t10n_token_begin(tok), tlen)){
          rv = cwal_value_undefined();
        }
        break;
      default:
        break;
    }
  }
  return rv;
}

char const * t10n_toker_err_pos( t10n_toker const * pt ){
  char const * rc = t10n_toker_err_pos_first(pt, 0);
  if(!rc){
    if(t10n_token_begin(&pt->token) < t10n_token_end(&pt->token)){
      rc = t10n_token_begin(&pt->token);
    }
    if(!rc){
      if(t10n_token_begin(&pt->_pbToken) < t10n_token_end(&pt->_pbToken)){
        rc = t10n_token_begin(&pt->_pbToken);
      }
      if(!rc) rc = t10n_toker_begin(pt);
    }
#if 0
    if(rc>=t10n_toker_end(pt) && t10n_toker_end(pt)>t10n_toker_begin(pt)) rc = t10n_toker_end(pt)-1;
#endif
  }
  return rc;
}


void t10n_toker_errtoken_set( t10n_toker * const st, t10n_token const * const tok ){
  st->_errToken = tok ? *tok : t10n_token_empty;
}

t10n_token const * t10n_toker_errtoken_get( t10n_toker const * st ){
  return &st->_errToken;
}

char t10n_toker_errtoken_has( t10n_toker const * st ){
  return t10n_token_begin(&st->_errToken)
    && t10n_token_begin(&st->_errToken) >= t10n_toker_begin(st)
    && t10n_token_begin(&st->_errToken) < t10n_toker_end(st)
    ? 1 : 0;
}

char const * t10n_toker_capture_cstr( t10n_toker const * st,
                                     cwal_size_t * len ){
  char const * rc = 0;
  char const * begin = t10n_token_begin(&st->capture.begin);
  char const * end = t10n_token_begin(&st->capture.end);
  if(begin && begin>=t10n_toker_begin(st)
     && end>=begin && end<=t10n_toker_end(st)){
    rc = begin;
    if(len) *len = (cwal_size_t)(end - begin);
  }
  return rc;
}

void t10n_path_toker_init( t10n_path_toker * pt, char const * path, cwal_int_t len ){
  *pt = t10n_path_toker_empty;
  pt->pos = pt->begin = path;
  pt->end = pt->begin + ((len>=0) ? (cwal_size_t)len : cwal_strlen(path));
}

int t10n_path_toker_next( t10n_path_toker * pt, char const ** token,
                        cwal_size_t * len ){
  if(!pt->pos || pt->pos>=t10n_toker_end(pt)) return CWAL_RC_RANGE;
  else if(!pt->separators || !*pt->separators) return CWAL_RC_MISUSE;
  else{
    char const * pos = pt->pos;
    char const * t;
    char const * sep;
    for( sep = pt->separators; *sep; ++sep){
      if(*sep & 0x80) return CWAL_RC_MISUSE;
      /* non-ASCII */
    }
    for( ; pos<t10n_toker_end(pt); ){
      /*skip leading separators*/
      for( sep = pt->separators;
           *sep && *pos!=*sep; ++sep ){
      }
      if(*pos == *sep) ++pos;
      else break;
    }
    t = pos;
    for( ; pos<t10n_toker_end(pt); ){
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

static int t10n_err_ptoker_impl( cwal_engine * e, bool throwIt,
                                 t10n_toker const * st,
                                 int code, char const * fmt,
                                 va_list vargs ){
  int rc = 0;
  char const * name;
  cwal_size_t nameLen = 0;
  cwal_error * const err = &e->err;
  cwal_buffer * const obuf = &err->msg;
  t10n_toker const * top = t10n_toker_top_parent( st );
  char const * errPos = t10n_toker_err_pos(st);
  t10n_linecol_t line = 0, col = 0;
  assert(errPos);
  if(errPos>=t10n_toker_end(st)
     && t10n_toker_end(st) > t10n_toker_begin(st)){
    /* Shameless workaround for syntax errors at
       the end of a script */
    errPos = t10n_toker_end(st)-1;
  }
  cwal_engine_error_reset(e);
  err->code = code ? code : CWAL_RC_EXCEPTION;
  name = t10n_toker_name_first(st, &nameLen);
  /* expand source range to include parent parsers,
     so that we get the right line/column numbers.
  */
  if(errPos>=t10n_toker_begin(top) && errPos<t10n_toker_end(top)){
    t10n_toker_count_lines(st, errPos, &line, &col);
  }
  if(CWAL_RC_OOM==code){
    rc = CWAL_RC_OOM;
  }
  else if(!throwIt && errPos
          /*&& !se->currentScript (from s2 - might need a param
            to specify this flag )*/
     /*  ^^^^^^ elide location info from error string when it looks
         like a script-side exception is up-coming */
     ) {
    char const * tailPart = ": ";
    if(name){
      rc = cwal_buffer_printf( e, obuf, "%s:", name );
    }
    if(!rc && errPos < t10n_toker_end(top)){
      if(name){
        rc = cwal_buffer_printf( e, obuf,
                                 "%d:%d%s",
                                 line, col, tailPart);
      }else{
        rc = cwal_buffer_printf( e, obuf,
                                 "line %d, col %d%s",
                                 line, col, tailPart);
      }
    }else if(errPos == t10n_toker_end(top)){
      rc = cwal_buffer_printf( e, obuf,
                               "@ EOF%s", tailPart);
    }else{
      rc = cwal_buffer_printf( e, obuf,
                               "@ unknown source position%s",
                               tailPart);
    }
  }

  if(!rc){
    if(fmt && *fmt){
      rc = cwal_buffer_printfv(e, obuf, fmt, vargs);
    }else{
      rc = cwal_buffer_printf(e, obuf,
                              "Error #%d (%s)%s%s",
                              code, cwal_rc_cstr(code),
                              (st->errMsg ? ": " : ""),
                              st->errMsg ? st->errMsg : "");
    }
  }
  err->line = line;
  err->col = col;
  err->script.used = 0;
  if(!rc && name){
    rc = cwal_buffer_append( e, &err->script, name, nameLen );
  }
  if(!rc && throwIt){
    rc = cwal_error_throw(e, err, name, line, col);
  }
  return rc ? rc : err->code;
}

int t10n_err_ptoker( cwal_engine * e, t10n_toker const * st,
                     int code, char const * fmt, ... ){
  int rc;
  va_list args;
  va_start(args,fmt);
  rc = t10n_err_ptoker_impl(e, false, st, code, fmt, args);
  va_end(args);
  return rc;
}

static int t10n_next_token_eol_skipper( int ttype ){
  switch(ttype){
    case T10N_T_EOL:
    case T10N_T_NL:
      return ttype;
    default:
      return t10n_ttype_is_junk(ttype);
  }
}

static int t10n_next_token_eol_noskipper( int ttype ){
  switch(ttype){
    case T10N_T_EOL:
    case T10N_T_NL:
      return 0;
    default:
      return t10n_ttype_is_junk(ttype);
  }
}


int t10n_next_token( cwal_engine * e, t10n_toker * st,
                     int flags,
                     t10n_token * tgt ){
  t10n_token tt = t10n_token_empty;
  t10n_token const oldT = st->token;
  t10n_token const oldP = *t10n_toker_putback_get(st);
  t10n_ttype_predicate_f const skipper =
    (T10N_NEXT_NO_SKIP_EOL & flags)
    ? t10n_next_token_eol_noskipper
    : t10n_next_token_eol_skipper;
  int rc;
  /* t10n_engine_err_reset(se); */
  rc = t10n_toker_lookahead_skip( st, &tt, skipper);
  if(rc){
    return t10n_err_ptoker( e, st, rc, "%s", st->errMsg );
  }else{
    t10n_toker_token_set(st, &tt);
    switch((T10N_NEXT_NO_POSTPROCESS & flags) ? 0 : tt.ttype){
      case T10N_T_Semicolon:
        st->token.ttype = T10N_T_EOX;
        break;
      case T10N_T_CR:
        assert(!"skipped by the junk-skipper!");
        CWAL_SWITCH_FALL_THROUGH;
      case T10N_T_NL:
        assert( T10N_NEXT_NO_SKIP_EOL & flags );
        st->token.ttype = T10N_T_EOL;
        break;
      case T10N_T_SquigglyOpen:
      case T10N_T_ParenOpen:
      case T10N_T_BraceOpen:
        /* Group these at the tokenization level to simplify
           evaluation. This completely removes open/close
           parens/braces handling from the eval side by evaluating
           these groups as a Value by recursively eval'ing their
           content.
        */
        rc = t10n_slurp_braces(e, st, 0);
        break;
      case T10N_T_HeredocStart:
        rc = t10n_slurp_heredoc(e, st, 0);
        assert(rc ? 1 : T10N_T_Heredoc==st->token.ttype);
        break;
      default:
        break;
    }
  }
  if(rc && !t10n_toker_errtoken_has(st)){
    t10n_toker_errtoken_set(st, &st->token);
  }
  if(tgt){
    *tgt = st->token;
    t10n_toker_token_set(st, &oldT);
    t10n_toker_putback_set(st, &oldP);
  }else if(!rc){
    t10n_toker_putback_set(st, &oldT);
  }
  return rc;
}


int t10n_slurp_braces( cwal_engine *e, t10n_toker * st,
                       t10n_token * out ){
  int const opener = st->token.ttype;
  int closer;
  int rc = 0;
  int level = 1;
  t10n_token origSrc;
  t10n_token origPb;
  int adjustedOpener = opener;
  char const * typeLabel = 0;
  char const * end = 0;
  if(!out) out = &st->token;
  switch(opener){
    case T10N_T_ParenOpen:
      closer = T10N_T_ParenClose;
      adjustedOpener = T10N_T_ParenGroup;
      typeLabel = "(PARENS)";
      break;
    case T10N_T_BraceOpen:
      closer = T10N_T_BraceClose;
      adjustedOpener = T10N_T_BraceGroup;
      typeLabel = "[BRACES]";
      break;
    case T10N_T_SquigglyOpen:
      adjustedOpener = T10N_T_SquigglyGroup;
      closer = T10N_T_SquigglyClose;
      typeLabel = "{SQUIGGLY}";
      break;
    default:
      return t10n_err_ptoker(e, st, CWAL_RC_TYPE,
                             "Invalid token type #%d (%s) for "
                             "t10n_slurp_braces()",
                             opener, t10n_ttype_cstr(opener));
  }
  origSrc = st->token;
  origPb = *t10n_toker_putback_get(st);
  switch(opener){
    case T10N_T_SquigglyOpen:
    case T10N_T_ParenOpen:
    case T10N_T_BraceOpen:{
      t10n_token errTok = st->token;
      for( ; (0==(rc=t10n_next_token(e, st, 0, 0))); ){
        /*
          consider: ( ..., <<<X ... X, ... )
    
          The heredoc might contain stuff which confuses the parens.
          Because of that, we can't simply do a byte-traversal here,
          but have to go through t10n_toker_next_token().
        */
        t10n_token const * tok = &t10n_toker_token(st)
          /* Reminder to self: this "could" be lifted outside of the
             loop because tok's address is constant, but eventual API
             changes might break that. */;
        end = t10n_token_end(tok);
        if(t10n_ttype_is_eof(tok->ttype)){
          end = 0;
          break;
        }else{
          if(opener == tok->ttype){
            ++level;
            errTok = *tok;
          }
          else if(closer == tok->ttype){
            assert(level>0);
            if(!--level) break;
          }
        }
      }
      if(!rc && 0 != level){
        t10n_toker_errtoken_set(st, &errTok);
        assert(typeLabel);
        rc = t10n_err_ptoker( e, st, CWAL_SCR_SYNTAX,
                              "Unexpected EOF while slurping %s block.",
                              typeLabel);
      }
      break;
    }/*Parens/Braces*/
  }/* switch */

  if(!rc && !end){
    t10n_toker_errtoken_set(st, &origSrc);
#if 0
    MARKER(("slurped <<%.*s>>\n",
            /* (int)t10n_token_len2(out), t10n_token_adjbegin(out), */
            (int)(t10n_token_end(&t10n_toker_token(st)) - t10n_token_begin(&origSrc)),
            t10n_token_begin(&origSrc)));
#endif
    rc = t10n_err_ptoker( e, st, CWAL_SCR_SYNTAX,
                          "Unexpected EOF or mismatched braces "
                          "while slurping %s block.",
                          typeLabel);
  }else if(!rc){
    assert(end);
    out->ttype = adjustedOpener;
    t10n_token_begin_set(out, t10n_token_begin(&origSrc));
    t10n_token_adjbegin_set(out, t10n_token_begin(out));
    t10n_token_end_set(out, end);
    assert(t10n_token_len(out) >= 2);
    assert(opener == (int)*t10n_token_begin(out));
    assert(closer == (int)*(t10n_token_end(out)-1));
    /* Skip leading whitespaces */
    while(((t10n_token_adjbegin_incr(out, 1)) < (end-1))
          && t10n_is_space((int)*t10n_token_adjbegin(out))){
    }
    /* Skip trailing whitespaces */
    t10n_token_adjend_set(out, end - 1/* == closer */);
    while( ((t10n_token_adjend_incr(out,-1)) > t10n_token_adjbegin(out))
           && t10n_is_space((int)*t10n_token_adjend(out)) ){
    }
    t10n_token_adjend_set(out, (t10n_token_adjend(out)+1))/* possibly back to the '}' byte */;
    out->ttype = adjustedOpener;
    rc = 0;
#if 0
    MARKER(("slurped <<%.*s>> out->adjEnd=%d\n",
            /* (int)t10n_token_len2(out), t10n_token_adjbegin(out), */
            (int)t10n_token_len(out), t10n_token_begin(out),
            *t10n_token_adjend(out)));
#endif
    assert(opener == *t10n_token_begin(out));
    assert(closer == *(t10n_token_end(out)-1));
    assert( t10n_token_adjend(out) >= t10n_token_adjbegin(out) );
    assert( t10n_token_adjend(out) <= t10n_token_end(out) );
  }
  if(out != &st->token){
    t10n_toker_token_set(st, &origSrc);
    t10n_toker_putback_set(st, &origPb);
  }
  return rc;
}

int t10n_slurp_heredoc( cwal_engine * e, t10n_toker * st,
                        t10n_token * tgt ){
  int rc = 0;
  char const * docBegin;
  char const * docEnd;
  char const * idBegin;
  char const * idEnd;
  char const * theEnd = NULL;
  t10n_token const origin = st->token;
  t10n_token tId = t10n_token_empty;
  cwal_size_t idLen;
  int modeFlag = 0;
  assert(T10N_T_HeredocStart==st->token.ttype);
  if(!tgt) tgt = &st->token;

  rc = t10n_toker_next_token(st);
  if(rc) goto tok_err;

  if(T10N_T_Colon==st->token.ttype){
    modeFlag = st->token.ttype;
    rc = t10n_toker_next_token(st);
    if(rc) goto tok_err;
  }

  switch(st->token.ttype){
    case T10N_T_Identifier:
    case T10N_T_LiteralStringDQ:
    case T10N_T_LiteralStringSQ:
      break;
    default:
      rc = CWAL_SCR_SYNTAX;
      st->errMsg = "Expecting identifier or "
        "quoted string "
        "at start of HEREDOC.";
      goto tok_err;
  }
  tId = st->token;
  idBegin = t10n_token_begin(&tId);
  idEnd = t10n_token_end(&tId);
  idLen = t10n_token_len(&tId);
  docBegin = idEnd;
  switch(modeFlag){
    case T10N_T_Colon:
      if(('\n'==*docBegin || t10n_is_space((int)*docBegin))
         || ('\r'==*docBegin && '\n'==docBegin[1])){
        docBegin += '\r'==*docBegin ? 2 : 1;
        ++st->currentLine;
        st->currentCol = 0;
      }
      break;
    default:
      while(t10n_is_space((int)*docBegin)){
        if('\n'==*docBegin){
          ++st->currentLine;
          st->currentCol = 0;
        }else{
          ++st->currentCol;
        }
        ++docBegin;
      }
  }
  rc = CWAL_SCR_SYNTAX;
  for( docEnd = docBegin; docEnd < t10n_toker_end(st); ++docEnd ){
    if(*docEnd != *idBegin){
      if('\n'==*docEnd){
        ++st->currentLine;
        st->currentCol = 0;
      }else{
        ++st->currentCol;
      }
      continue;
    }
    else if(0 == memcmp( docEnd, idBegin, idLen)){
      char const * back = docEnd-1;
      theEnd = docEnd + idLen;
      switch(modeFlag){
        case T10N_T_Colon:
          if(back>=docBegin && ('\n'==*back || t10n_is_space((int)*back))) --docEnd;
          break;
        default:
          for( ; back>=docBegin && t10n_is_space((int)*back); --back) --docEnd;
      }
      rc = 0;
      st->currentCol += (t10n_linecol_t)idLen;
      break;
    }
  }
  if(rc){
    t10n_toker_errtoken_set(st, &tId);
    rc = t10n_err_ptoker(e, st,
                       rc, "Did not find end of HEREDOC "
                       "starting with '%.*s'.",
                       (int)idLen, idBegin);
  }else{
    t10n_token_begin_set(tgt, t10n_token_begin(&origin));
    t10n_token_end_set(tgt, theEnd);
    tgt->ttype = T10N_T_Heredoc;
    t10n_token_adjbegin_set(tgt, docBegin);
    t10n_token_adjend_set(tgt, docEnd);
    tgt->line = origin.line;
    tgt->column = origin.column;
    assert(docEnd>=docBegin);
#if 0
    MARKER(("HEREDOC<%.*s> body: %.*s\n",
            (int)(idEnd-idBegin), idBegin,
            (int)(docEnd-docBegin), docBegin));
#endif
  }
  return rc;
  tok_err:
  assert(rc);
  assert(st->errMsg);
  rc = t10n_err_ptoker(e, st, rc, "%s", st->errMsg);
  return rc;
}


#if T10N_LCCACHE
#  undef t10n_toker_lc_slot
#else
#  undef t10n_toker_lc_search
#endif
#undef MARKER
#undef T10N_LCCACHE
#undef T10N_LCCACHE_SLOT
#undef LCMARKER
#undef T10N_COMPILE
#undef COMPILED_DECL
#undef COMPILED_DECL_C
#undef COMPILED_MEMBER
#undef COMPILED_MEMBER_C
