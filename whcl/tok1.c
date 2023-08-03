/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

#include "tok1.h"
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

/* const tok1_byte_range tok1_byte_range_empty = tok1_byte_range_empty_m; */
const tok1_izer tok1_izer_empty = tok1_izer_empty_m;
const tok1_en tok1_en_empty = tok1_en_empty_m;
const tok1_path_toker tok1_path_toker_empty = tok1_path_toker_empty_m;

void tok1_izer_init( cwal_engine * const e, tok1_izer * const t,
                      char const * src, cwal_int_t len,
                      uint32_t flags ){
  *t = tok1_izer_empty;
  t->ec = e;
  t->flags = flags;
  if(len<0){
    len = (cwal_int_t)cwal_strlen(src);
    assert(len>=0);
  }
  t->begin = src;
  t->end = src+len;
  tok1_izer_reset(t);
}

void tok1_izer_reset( tok1_izer * const t ){
  t->_pbToken = t->_errToken = t->token = tok1_en_empty;
  tok1_en_begin_set(&t->token, tok1_izer_begin(t))
    /* begin w/o an end set is our internal signal that we're
       just starting off tokenization */;
  t->currentLine = 1;
  t->currentCol = 0;
}

void tok1_izer_finalize( tok1_izer * const pt ){
  *pt = tok1_izer_empty;
}

int tok1_izer_sub_from_token( tok1_izer const * parent,
                              tok1_en const * tok,
                              tok1_izer * const dest ){
  char const * begin;
  char const * end;
#if 0
  /* 20200107: this breaks stuff */
  if(!tok1_t_is_group(tok->ttype)){
    return CWAL_RC_TYPE;
  }
#endif
  if(tok1_en_adjbegin(tok)){
    begin = tok1_en_adjbegin(tok);
    end = tok1_en_adjend(tok);
  }else{
    begin = tok1_en_begin(tok);
    end = tok1_en_end(tok);
  }
  if(!begin || (begin>end)) return CWAL_RC_RANGE;
  tok1_izer_init( parent->ec, dest, begin,
                   (cwal_int_t)(end - begin), TOK1_F_LINEAR_TOKER);
  dest->parent = parent;
  assert(parent->ec == dest->ec);
  return 0;
}

int tok1_izer_sub_from_toker( tok1_izer const * parent, tok1_izer * const sub ){
  int const rc = tok1_izer_sub_from_token( parent, &parent->token, sub );
  if(!rc){
    assert(sub->parent == parent);
    assert(sub->ec == parent->ec);
    /* There are multiple TODOs here if/when we add compiled token
       chains. */
  }
  return rc;
}

tok1_izer const * tok1_izer_top_parent( tok1_izer const * t ){
  while( t && t->parent ){
    t = t->parent;
  }
  return t;
}
char const * tok1_izer_name_first( tok1_izer const * t, cwal_size_t * len ){
  while(t && !t->name){
    t = t->parent;
  }
  if(t && len && t->name) *len = cwal_strlen(t->name);
  return t ? t->name : 0;
}

char const * tok1_izer_err_pos_first( tok1_izer const * t,
                                      tok1_izer const ** foundIn){
  while(t && !tok1_en_begin(&t->_errToken)){
    t = t->parent;
  }
  if(t && foundIn) *foundIn = t;
  return t ? tok1_en_begin(&t->_errToken) : 0;
}

char const * tok1_izer_name_top( tok1_izer const * t ){
  char const * n = t ? t->name : 0;
  while(t && t->parent){
    t = t->parent;
    if(t && t->name) n = t->name;
  }
  return n;
}

int tok1_t_is_junk( int ttype ){
  switch(ttype){
    case ' ':
    case '\t':
    case '\r':
    case TOK1_T_Blank:
    case TOK1_T_CommentC:
    case TOK1_T_CommentCpp:
    case TOK1_T_CommentTCL:
    case TOK1_T_Whitespace:
    case TOK1_T_Shebang:
    case TOK1_T_UTFBOM:
      return ttype;
    default:
      return 0;
  }
}

int tok1_t_is_group( int ttype ){
  switch(ttype){
    case TOK1_T_CallBlock:
    case TOK1_T_ParenGroup:
    case TOK1_T_BraceGroup:
    case TOK1_T_SquigglyGroup:
      return ttype;
    default:
      return 0;
  }
}

int tok1_t_is_eof( int ttype ){
  return TOK1_T_EOF==ttype ? ttype : 0;
}

int tok1_t_is_eol( int ttype ){
  switch(ttype){
    case TOK1_T_EOF:
    case TOK1_T_EOL:
    case TOK1_T_CR:
    case TOK1_T_NL:
    /* case TOK1_T_CommentCpp: A //-style comment implies a newline, so
       we might consider treating it as one (and consuming a trailing
       newline, if any, as part of the token).
    */
      return ttype;
    default:
      return 0;
  }
}

int tok1_t_is_eox( int ttype ){
  switch(ttype){
    case TOK1_T_EOF:
    case TOK1_T_EOX:
    case TOK1_T_Semicolon:
      return ttype;
    default:
      return 0;
  }
}

int tok1_t_is_space( int ttype ){
  switch(ttype){
    case TOK1_T_NL:
    case TOK1_T_CR:
    case TOK1_T_EOL:
    case TOK1_T_Whitespace:
    /* case TOK1_T_CommentCpp: */
    /* case TOK1_T_CommentC: */
    case ' ':
    case '\t':
    case '\v':
    case '\f':
      return ttype;
    default:
      return 0;
  }
}

int tok1_izer_is_eof( tok1_izer const * const st ){
  return tok1_t_is_eof(st->token.ttype);
}

int tok1_izer_is_eox( tok1_izer const * const st ){
  return tok1_t_is_eox(st->token.ttype);
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
static bool tok1_is_id_char( int ch, bool isStart ) {
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
          ? tok1_is_alpha(ch)
          : tok1_is_alnum(ch);
    }
}

void tok1_izer_putback_set( tok1_izer * const st,
                            tok1_en const * const tok ){
  st->_pbToken = *tok;
}

tok1_en const * tok1_izer_putback_get( tok1_izer const * const st ){
  return &st->_pbToken;
}

bool tok1_izer_putback( tok1_izer * const st ){
  bool const rc = !!tok1_en_begin(&st->_pbToken);
  if(rc){
    st->token = st->_pbToken;
    st->_pbToken = tok1_en_empty;
    st->currentCol = st->token.column;
    st->currentLine = st->token.line;
    /*tok1_izer_token_set(st, &st->_pbToken);*/
  }
  return rc;
}


void tok1_izer_token_set( tok1_izer * const st, tok1_en const * const t ){
  /* Need a temp in case t== one of st's tokens */
  tok1_en const tmp = *t;
  st->_pbToken = st->token;
  st->token = tmp;
  st->_nextToken = tok1_en_empty;
}

void tok1_izer_next_token_set( tok1_izer * pt, tok1_en const * tk ){
  pt->_nextToken = *tk;
}

bool tok1_is_space( int ch ){
  switch(ch){
    case ' ':
    case '\n':
    case '\r':
    case '\t':
    case '\v':
    case '\f':
      return true;
    default:
      return false;
  }
}

bool tok1_is_blank( int ch ){
  switch(ch){
    case ' ':
    case '\t':
      return true;
    default:
      return false;
  }
}

bool tok1_is_digit( int ch ){
  return '0'<=ch && '9'>=ch;
}

bool tok1_is_xdigit( int ch ){
  return ('a'<=ch && 'f'>=ch)
    ? 1
    : (('A'<=ch && 'F'>=ch)
       ? 1
       :tok1_is_digit(ch));
}

bool tok1_is_octaldigit( int ch ){
  return ('0'<=ch && '7'>=ch);
}

bool tok1_is_alpha( int ch ){
  return ('a'<=ch && 'z'>=ch)
    || ('A'<=ch && 'Z'>=ch);
}

bool tok1_is_alnum( int ch ){
  return tok1_is_alpha(ch) ? 1 : tok1_is_digit(ch);
}

int tok1_read_identifier2( char const * zPos,
                           char const * zEnd,
                           char const ** zIdEnd,
                           uint32_t flags ){
  unsigned char const * start = (unsigned char const *) zPos;
  unsigned char const * pos = start;
  unsigned char const * end = (unsigned char const *) zEnd;
  unsigned char const * endChar = pos;
  int ch;
  int rc = 0;
  bool const allowDash = (TOK1_F_IDENTIFIER_DASHES & flags);
  bool const allowDash2 =
    TOK1_F_IDENTIFIER_DASHES2==(TOK1_F_IDENTIFIER_DASHES2 & flags);
  assert(zEnd>zPos);
  for( ; pos < end; ){
    ch = cwal_utf8_read_char( pos, end, &endChar );
    if(endChar == pos) break;
    else if(!tok1_is_id_char(ch, allowDash2 ? 0 : ((pos==start) ? 1 : 0))){
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

int tok1_read_identifier( char const * zPos,
                          char const * zEnd,
                          char const ** zIdEnd ){
  return tok1_read_identifier2( zPos, zEnd, zIdEnd, 0 );
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

int tok1_count_lines( char const * src, char const * end_,
                       char const * pos_,
                       tok1_linecol_t *line, tok1_linecol_t *col ){
  tok1_linecol_t ln = 1, c = 0;
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

       Need to see if we can build line/column counting into tok1_izer
       and tok1_izer_next_token(). (That failed because of how we
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

int tok1__toker_count_lines( tok1_izer const * pt, char const * pos,
                             tok1_linecol_t * line, tok1_linecol_t * col ){
  tok1_izer const * top;
  tok1_izer const * prev;
  int rc = 0;
  tok1_linecol_t li = 0, c = 0;
  tok1_linecol_t offLine = 0, offCol = 0;
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
  assert(tok1_izer_begin(pt) >= tok1_izer_begin(top)
         && tok1_izer_end(pt) <= tok1_izer_end(top));
  assert(pos<tok1_izer_end(pt) && pos >= tok1_izer_begin(top));
  if(TOK1_F_LINEAR_TOKER & pt->flags){
    tok1_en const * et = tok1_en_begin(&pt->_errToken)
      ? &pt->_errToken
      : &pt->token;
    li = et->line;
    c = et->column;
  }else{
    assert(!"We should never hit this condition in the whcl copy of this code.");
    rc = tok1_count_lines( tok1_izer_begin(top), tok1_izer_end(pt),
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
  }
  return rc;
}

int tok1_izer_count_lines2( tok1_izer const * pt,
                            tok1_en const * tok,
                            tok1_linecol_t * line, tok1_linecol_t * col ){
  tok1_linecol_t li = 0, c = 0;
  tok1_izer const * top;
  int rc = 0;
  if(!tok) tok = &pt->token;
  for( top = pt; top; top = top->parent){
    if(top->lineOffset) break;
  }
  if(!top) top = tok1_izer_top_parent(pt);
  if(tok->line){
    MARKER(("Using token-embedded line %d, col %d [%.*s] parent?=%d\n",
            tok->line, tok->column,
            (int)tok1_en_len(tok), tok1_en_begin(tok),
            !!pt->parent));
    li = tok->line;
    c = tok->column;
  }else{
    rc = tok1_count_lines( tok1_izer_begin(top), tok1_izer_end(pt),
                            tok1_en_begin(tok), &li, &c );
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

static int tok1_hexbyte( int ch ){
  if(ch>='0' && ch<='9') return ch - '0';
  else if(ch>='a' && ch<='f') return ch - 'a' + 10;
  else if(ch>='A' && ch<='F') return ch - 'A' + 10;
  else return -1;
}

static int tok1_read_hex_bytes(unsigned char const *zPos,
                             unsigned int howMany,
                             unsigned int * rv ){
  unsigned int rc = 0;
  int ch1, ch2;
  unsigned int n = 0;
  assert(!(howMany%2) && "howMany must be an even number!");
  assert(zPos && rv);
  while(n<howMany){
    ch1 = *zPos ? tok1_hexbyte(*zPos++) : -1;
    ch2 = (ch1>=0) ? tok1_hexbyte(*zPos++) : -1;
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
int tok1_unescape_string( cwal_engine * const e,
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
            uCount = tok1_read_hex_bytes(p+1, ulen, &uChar);
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


char const * tok1_t_cstr( int ttype ){
  switch((enum tok1_en_types_e)ttype){
    /* ^^^^ reminder: that cast is so that gcc will warn
       when i forget to update this function after adding
       new entries to that enum. */
#define CASE(TT) case TT: return &(#TT[7])/*==prefix strlen("TOK1_T_")*/
    CASE(TOK1_T_TokErr);
    CASE(TOK1_T_INVALID);
    CASE(TOK1_T_EOF);
    CASE(TOK1_T_EOX);
    CASE(TOK1_T_Tab);
    CASE(TOK1_T_NL);
    CASE(TOK1_T_VTab);
    CASE(TOK1_T_FF);
    CASE(TOK1_T_CR);
    CASE(TOK1_T_EOL);
    CASE(TOK1_T_Space);
    CASE(TOK1_T_Blank);
    CASE(TOK1_T_Whitespace);
    CASE(TOK1_T_At);
    CASE(TOK1_T_AtExpand);
    CASE(TOK1_T_UTFBOM);
    CASE(TOK1_T_OpNot);
    CASE(TOK1_T_OpHash);
    CASE(TOK1_T_Shebang);
    CASE(TOK1_T_Dollar);
    CASE(TOK1_T_CallBlockOpen);
    CASE(TOK1_T_CallBlock);
    CASE(TOK1_T_OpModulo);
    CASE(TOK1_T_OpModuloAssign);
    CASE(TOK1_T_OpModuloAssign3);
    CASE(TOK1_T_OpAndBitwise);
    CASE(TOK1_T_OpAnd);
    CASE(TOK1_T_OpAndAssign);
    CASE(TOK1_T_OpAndAssign3);
    CASE(TOK1_T_ParenOpen);
    CASE(TOK1_T_ParenGroup);
    CASE(TOK1_T_ParenClose);
    CASE(TOK1_T_OpMultiply);
    CASE(TOK1_T_OpMultiplyAssign);
    CASE(TOK1_T_OpMultiplyAssign3);
    CASE(TOK1_T_OpPlus);
    CASE(TOK1_T_OpPlusUnary);
    CASE(TOK1_T_OpPlusAssign);
    CASE(TOK1_T_OpPlusAssign3);
    CASE(TOK1_T_OpIncr);
    CASE(TOK1_T_OpIncrPre);
    CASE(TOK1_T_OpIncrPost);
    CASE(TOK1_T_Comma);
    CASE(TOK1_T_RHSEval);
    CASE(TOK1_T_OpMinus);
    CASE(TOK1_T_OpMinusUnary);
    CASE(TOK1_T_OpMinusAssign);
    CASE(TOK1_T_OpMinusAssign3);
    CASE(TOK1_T_OpDecr);
    CASE(TOK1_T_OpDecrPre);
    CASE(TOK1_T_OpDecrPost);
    CASE(TOK1_T_OpDot);
    CASE(TOK1_T_OpDotDot);
    CASE(TOK1_T_OpDotLength);
    CASE(TOK1_T_OpInherits);
    CASE(TOK1_T_OpNotInherits);
    CASE(TOK1_T_OpContains);
    CASE(TOK1_T_OpNotContains);
    CASE(TOK1_T_OpArrow);
    CASE(TOK1_T_OpArrow2);
    CASE(TOK1_T_OpDivide);
    CASE(TOK1_T_OpDivideAssign);
    CASE(TOK1_T_OpDivideAssign3);
    CASE(TOK1_T_Colon);
    CASE(TOK1_T_Colon2);
    CASE(TOK1_T_OpColonEqual);
    CASE(TOK1_T_Semicolon);
    CASE(TOK1_T_OpCmpLT);
    CASE(TOK1_T_OpCmpLE);
    CASE(TOK1_T_OpShiftLeft);
    CASE(TOK1_T_OpShiftLeftAssign);
    CASE(TOK1_T_OpShiftLeftAssign3);
    CASE(TOK1_T_HeredocStart);
    CASE(TOK1_T_HeredocStart2);
    CASE(TOK1_T_Heredoc);
    CASE(TOK1_T_OpAssign);
    CASE(TOK1_T_OpAssign3);
    CASE(TOK1_T_OpAssignConst3);
    CASE(TOK1_T_OpCmpEq);
    CASE(TOK1_T_OpCmpNotEq);
    CASE(TOK1_T_OpCmpEqStrict);
    CASE(TOK1_T_OpCmpNotEqStrict);
    CASE(TOK1_T_OpCmpGT);
    CASE(TOK1_T_OpCmpGE);
    CASE(TOK1_T_OpShiftRight);
    CASE(TOK1_T_OpShiftRightAssign);
    CASE(TOK1_T_OpShiftRightAssign3);
    CASE(TOK1_T_Question);
    CASE(TOK1_T_QDot);
    CASE(TOK1_T_BraceOpen);
    CASE(TOK1_T_Backslash);
    CASE(TOK1_T_BraceClose);
    CASE(TOK1_T_BraceGroup);
    CASE(TOK1_T_ArrayAppend);
    CASE(TOK1_T_OpXOr);
    CASE(TOK1_T_OpXOrAssign);
    CASE(TOK1_T_OpXOrAssign3);
    CASE(TOK1_T_SquigglyOpen);
    CASE(TOK1_T_SquigglyGroup);
    CASE(TOK1_T_OpOrBitwise);
    CASE(TOK1_T_OpOr);
    CASE(TOK1_T_OpOr3);
    CASE(TOK1_T_OpElvis);
    CASE(TOK1_T_OpOrAssign);
    CASE(TOK1_T_OpOrAssign3);
    CASE(TOK1_T_SquigglyClose);
    CASE(TOK1_T_OpNegateBitwise);

    CASE(TOK1_T_Literal__);
    CASE(TOK1_T_LiteralNumber);
    CASE(TOK1_T_LiteralIntDec);
    CASE(TOK1_T_LiteralIntHex);
    CASE(TOK1_T_LiteralIntOct);
    CASE(TOK1_T_LiteralIntBin);
    CASE(TOK1_T_LiteralDouble);
    CASE(TOK1_T_QuotedStringDQ);
    CASE(TOK1_T_QuotedStringSQ);
    CASE(TOK1_T_QuotedStringBT);
    CASE(TOK1_T_QuotedString);
    CASE(TOK1_T_PropertyKey);
    CASE(TOK1_T_Identifier);
    CASE(TOK1_T_IdentifierDashed);
    CASE(TOK1_T_WHCLFlag);
    CASE(TOK1_T_IdentifierDeref);
    CASE(TOK1_T_PropAccess);
    CASE(TOK1_T_PropAccessWith);
    CASE(TOK1_T_WHCLWord);
    CASE(TOK1_T_ValueTypes__);
    CASE(TOK1_T_Value);
    CASE(TOK1_T_Undefined);
    CASE(TOK1_T_Null);
    CASE(TOK1_T_False);
    CASE(TOK1_T_True);
    CASE(TOK1_T_Object);
    CASE(TOK1_T_Array);
    CASE(TOK1_T_Function);

    CASE(TOK1_T_Keyword__);
    CASE(TOK1_T_KeywordAffirm);
    CASE(TOK1_T_KeywordAssert);
    CASE(TOK1_T_KeywordBREAKPOINT);
    CASE(TOK1_T_KeywordBreak);
    CASE(TOK1_T_KeywordCOLUMN);
    CASE(TOK1_T_KeywordCatch);
    CASE(TOK1_T_KeywordClass);
    CASE(TOK1_T_KeywordConst);
    CASE(TOK1_T_KeywordContinue);
    CASE(TOK1_T_KeywordDefine);
    CASE(TOK1_T_KeywordDefined);
    CASE(TOK1_T_KeywordDelete);
    CASE(TOK1_T_KeywordDo);
    CASE(TOK1_T_KeywordEcho);
    CASE(TOK1_T_KeywordEnum);
    CASE(TOK1_T_KeywordEval);
    CASE(TOK1_T_KeywordException);
    CASE(TOK1_T_KeywordExit);
    CASE(TOK1_T_KeywordFILE);
    CASE(TOK1_T_KeywordFILEDIR);
    CASE(TOK1_T_KeywordFalse);
    CASE(TOK1_T_KeywordFatal);
    CASE(TOK1_T_KeywordFor);
    CASE(TOK1_T_KeywordForEach);
    CASE(TOK1_T_KeywordFunction);
    CASE(TOK1_T_KeywordIf);
    CASE(TOK1_T_KeywordImport);
    CASE(TOK1_T_KeywordInclude);
    CASE(TOK1_T_KeywordInterface);
    CASE(TOK1_T_KeywordIs);
    CASE(TOK1_T_KeywordIsA);
    CASE(TOK1_T_KeywordLINE);
    CASE(TOK1_T_KeywordNameof);
    CASE(TOK1_T_KeywordNew);
    CASE(TOK1_T_KeywordNull);
    CASE(TOK1_T_KeywordPragma);
    CASE(TOK1_T_KeywordPrivate);
    CASE(TOK1_T_KeywordProc);
    CASE(TOK1_T_KeywordProtected);
    CASE(TOK1_T_KeywordPublic);
    CASE(TOK1_T_KeywordReturn);
    CASE(TOK1_T_KeywordRefcount);
    CASE(TOK1_T_KeywordS2Out);
    CASE(TOK1_T_KeywordSRCPOS);
    CASE(TOK1_T_KeywordScope);
    CASE(TOK1_T_KeywordStatic);
    CASE(TOK1_T_KeywordThrow);
    CASE(TOK1_T_KeywordTrue);
    CASE(TOK1_T_KeywordTry);
    CASE(TOK1_T_KeywordTypeinfo);
    CASE(TOK1_T_KeywordTypename);
    CASE(TOK1_T_KeywordUndefined);
    CASE(TOK1_T_KeywordUnset);
    CASE(TOK1_T_KeywordUKWD);
    CASE(TOK1_T_KeywordUsing);
    CASE(TOK1_T_KeywordVar);
    CASE(TOK1_T_KeywordWhile);

    CASE(TOK1_T_BIC);
    CASE(TOK1_T_BIC_affirm);
    CASE(TOK1_T_BIC_alias);
    CASE(TOK1_T_BIC_array);
    CASE(TOK1_T_BIC_assert);
    CASE(TOK1_T_BIC_break);
    CASE(TOK1_T_BIC_catch);
    CASE(TOK1_T_BIC_concat);
    CASE(TOK1_T_BIC_const);
    CASE(TOK1_T_BIC_continue);
    CASE(TOK1_T_BIC___debug);      
    CASE(TOK1_T_BIC_decl);
    CASE(TOK1_T_BIC_decr);
    CASE(TOK1_T_BIC_define);
    CASE(TOK1_T_BIC_do);
    CASE(TOK1_T_BIC_echo);
    CASE(TOK1_T_BIC_eval);
    CASE(TOK1_T_BIC_exception);
    CASE(TOK1_T_BIC_exit);
    CASE(TOK1_T_BIC_expr);
    CASE(TOK1_T_BIC_for);
    CASE(TOK1_T_BIC_foreach);
    CASE(TOK1_T_BIC_if);
    CASE(TOK1_T_BIC_incr);
    CASE(TOK1_T_BIC_info);
    CASE(TOK1_T_BIC_new);
    CASE(TOK1_T_BIC_object);
    CASE(TOK1_T_BIC_pragma);
    CASE(TOK1_T_BIC_proc);
    CASE(TOK1_T_BIC_return);
    CASE(TOK1_T_BIC_set);
    CASE(TOK1_T_BIC_throw);
    CASE(TOK1_T_BIC_unset);
    CASE(TOK1_T_BIC_while);
    CASE(TOK1_T_BIC_with);

    CASE(TOK1_T_BIV);
    CASE(TOK1_T_BIV_true);
    CASE(TOK1_T_BIV_false);
    CASE(TOK1_T_BIV_null);
    CASE(TOK1_T_BIV_this);
    CASE(TOK1_T_BIV_undefined);
    CASE(TOK1_T_BIV_using);
    CASE(TOK1_T_BIV_whcl);
    CASE(TOK1_T_BIV___COLUMN);
    CASE(TOK1_T_BIV___FILE);
    CASE(TOK1_T_BIV___FILEDIR);
    CASE(TOK1_T_BIV___LINE);
    CASE(TOK1_T_BIV___FLC);
    
    CASE(TOK1_T_Comment__);
    CASE(TOK1_T_CommentC);
    CASE(TOK1_T_CommentCpp);
    CASE(TOK1_T_CommentTCL);
    CASE(TOK1_T_Mark__);
    CASE(TOK1_T_MarkVariadicStart);
    CASE(TOK1_T_Misc__);
    CASE(TOK1_T_Foo);
    CASE(TOK1_T_comma_kludge_);
#undef CASE
  }
#if 0
  /* Make ttypes matching characters in the ASCII range (even
     unprintable ones) their own strings. Exception: the value 0 is
     reserved for TOK1_T_INVALID, so we don't have a string for the
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

char const * tok1_last_path_sep(char const * str, cwal_size_t slen ){
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
cwal_size_t tok1_en_len( tok1_en const * token ){
  return (tok1_en_begin(token)
          && tok1_en_end(token) > tok1_en_begin(token))
    ? (cwal_size_t)(tok1_en_end(token) - tok1_en_begin(token))
    : 0;
}
cwal_size_t tok1_en_len2( tok1_en const * token ){
  if(token->adjBegin && token->adjEnd && token->adjBegin<=token->adjEnd){
    return (cwal_size_t)(token->adjEnd - token->adjBegin);
  }
  return (token->begin && token->end > token->begin)
    ? (cwal_size_t)(token->end - token->begin)
    : 0;
}
#endif

char const * tok1_en_cstr( tok1_en const * const tok,
                               cwal_size_t * const len,
                               bool innerOnly ){
  const char * const adjBegin =
    innerOnly ? tok1_en_adjbegin(tok) : NULL;
  if(adjBegin){
    if(len) *len = tok1_en_len2(tok);
    return adjBegin;
  }
  if(len) *len = tok1_en_len(tok);
  return tok1_en_begin(tok);
}

char const * tok1_izer_err_pos( tok1_izer const * pt ){
  char const * rc = tok1_izer_err_pos_first(pt, 0);
  if(!rc){
    if(tok1_en_begin(&pt->token) < tok1_en_end(&pt->token)){
      rc = tok1_en_begin(&pt->token);
    }
    if(!rc){
      if(tok1_en_begin(&pt->_pbToken) < tok1_en_end(&pt->_pbToken)){
        rc = tok1_en_begin(&pt->_pbToken);
      }
      if(!rc) rc = tok1_izer_begin(pt);
    }
#if 0
    if(rc>=tok1_izer_end(pt) && tok1_izer_end(pt)>tok1_izer_begin(pt)) rc = tok1_izer_end(pt)-1;
#endif
  }
  return rc;
}


void tok1_izer_errtoken_set( tok1_izer * const st, tok1_en const * const tok ){
  st->_errToken = tok ? *tok : tok1_en_empty;
}

tok1_en const * tok1_izer_errtoken_get( tok1_izer const * st ){
  return &st->_errToken;
}

bool tok1_izer_errtoken_has( tok1_izer const * st ){
  return tok1_en_begin(&st->_errToken)
    && tok1_en_begin(&st->_errToken) >= tok1_izer_begin(st)
    && tok1_en_begin(&st->_errToken) < tok1_izer_end(st);
}

void tok1_path_toker_init( tok1_path_toker * const pt, char const * path,
                           cwal_int_t len ){
  *pt = tok1_path_toker_empty;
  pt->pos = pt->begin = path;
  pt->end = pt->begin + ((len>=0) ? (cwal_size_t)len : cwal_strlen(path));
}

int tok1_path_toker_next( tok1_path_toker * const pt, char const ** token,
                          cwal_size_t * const len ){
  if(!pt->pos || pt->pos>=tok1_izer_end(pt)) return CWAL_RC_RANGE;
  else if(!pt->separators || !*pt->separators) return CWAL_RC_MISUSE;
  else{
    char const * pos = pt->pos;
    char const * t;
    char const * sep;
    for( sep = pt->separators; *sep; ++sep){
      if(*sep & 0x80) return CWAL_RC_MISUSE;
      /* non-ASCII */
    }
    for( ; pos<tok1_izer_end(pt); ){
      /*skip leading separators*/
      for( sep = pt->separators;
           *sep && *pos!=*sep; ++sep ){
      }
      if(*pos == *sep) ++pos;
      else break;
    }
    t = pos;
    for( ; pos<tok1_izer_end(pt); ){
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

static int tok1_err_toker_impl( cwal_engine * e, bool throwIt,
                                 tok1_izer const * st,
                                 int code, char const * fmt,
                                 va_list vargs ){
  int rc = 0;
  char const * name;
  cwal_size_t nameLen = 0;
  cwal_error * const err = &e->err;
  cwal_buffer * const obuf = &err->msg;
  tok1_izer const * top = tok1_izer_top_parent( st );
  char const * errPos = tok1_izer_err_pos(st);
  tok1_linecol_t line = 0, col = 0;
  assert(errPos);
  if(errPos>=tok1_izer_end(st)
     && tok1_izer_end(st) > tok1_izer_begin(st)){
    /* Shameless workaround for syntax errors at
       the end of a script */
    errPos = tok1_izer_end(st)-1;
  }
  cwal_engine_error_reset(e);
  err->code = code ? code : CWAL_RC_EXCEPTION;
  name = tok1_izer_name_first(st, &nameLen);
  /* expand source range to include parent parsers,
     so that we get the right line/column numbers.
  */
  if(errPos>=tok1_izer_begin(top) && errPos<tok1_izer_end(top)){
    tok1__toker_count_lines(st, errPos, &line, &col);
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
    if(!rc && errPos < tok1_izer_end(top)){
      if(name){
        rc = cwal_buffer_printf( e, obuf,
                                 "%d,%d%s",
                                 line, col, tailPart);
      }else{
        rc = cwal_buffer_printf( e, obuf,
                                 "line %d, col %d%s",
                                 line, col, tailPart);
      }
    }else if(errPos == tok1_izer_end(top)){
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
  cwal_buffer_reuse(&err->script);
  if(!rc && name){
    rc = cwal_buffer_append( e, &err->script, name, nameLen );
  }
  if(!rc && throwIt){
    rc = cwal_error_throw(e, err, name, line, col);
  }
  return rc ? rc : err->code;
}

int tok1_err_toker( tok1_izer const * st,
                     int code, char const * fmt, ... ){
  int rc;
  va_list args;
  va_start(args,fmt);
  assert(st->ec);
  rc = tok1_err_toker_impl(st->ec, false, st, code, fmt, args);
  va_end(args);
  return rc;
}

int tok1_throw_toker( tok1_izer const * st, int code, char const * fmt, ... ){
  int rc;
  va_list args;
  va_start(args,fmt);
  assert(st->ec);
  rc = tok1_err_toker_impl(st->ec, true, st, code, fmt, args);
  va_end(args);
  return rc;
}

int tok1_slurp_heredoc( tok1_izer * const st,
                        tok1_next_token_f tf,
                        void * tfState,
                        tok1_en * tgt ){
  int rc = 0;
  char const * docBegin;
  char const * docEnd;
  char const * idBegin;
  char const * idEnd;
  char const * theEnd = NULL;
  tok1_en const origin = st->token;
  tok1_en tId = tok1_en_empty;
  cwal_size_t idLen;
  int modeFlag = 0;
  int const docType = st->token.ttype;
  assert(TOK1_T_HeredocStart==st->token.ttype
         || TOK1_T_HeredocStart2==st->token.ttype);
  assert(st->ec);
  if(!tgt) tgt = &st->token;
  if(TOK1_T_HeredocStart2 == docType){
    tId = st->token;
  }
  rc = tf(st, tfState);
  if(rc) goto tok_err;
  else if(TOK1_T_Colon==st->token.ttype){
    modeFlag = st->token.ttype;
  }
  if(TOK1_T_HeredocStart2 == docType){
    if(!modeFlag){
      tok1_izer_putback(st)
        /* Fixes line number if we just read an EOL. */;
    }
    idBegin = "}}}";
    idEnd = tok1_en_end(&origin) + (modeFlag ? 1 : 0);
    idLen = 3;
  }else{
    if(TOK1_T_Colon==modeFlag){
      rc = tf(st, tfState);
      if(rc) goto tok_err;
    }
    tId = st->token;
    switch(st->token.ttype){
      case TOK1_T_Identifier:
      case TOK1_T_QuotedString:
        break;
      default:
        rc = CWAL_SCR_SYNTAX;
        st->errMsg = "Expecting identifier or "
          "quoted string "
          "at start of HEREDOC.";
        goto tok_err;
    }
    idBegin = tok1_en_begin(&tId);
    idEnd = tok1_en_end(&tId);
    idLen = tok1_en_len(&tId);
  }
  docBegin = idEnd;
  switch(modeFlag){ /* Strip leading whitespace... */
    case TOK1_T_Colon:
      if('\n'==*docBegin || ('\r'==*docBegin && '\n'==docBegin[1])){
        docBegin += '\r'==*docBegin ? 2 : 1;
        ++st->currentLine;
        st->currentCol = 0;
      }else if(tok1_is_space((int)*docBegin)){
        ++st->currentCol;
        ++docBegin;
      }
      break;
    default:
      while(tok1_is_space((int)*docBegin)){
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
  for( docEnd = docBegin; docEnd < tok1_izer_end(st); ++docEnd ){
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
        case TOK1_T_Colon:
          if(back>=docBegin && ('\n'==*back || tok1_is_space((int)*back))) --docEnd;
          break;
        default:
          for( ; back>=docBegin && tok1_is_space((int)*back); --back) --docEnd;
      }
      rc = 0;
      st->currentCol += (tok1_linecol_t)idLen;
      break;
    }
  }
  if(rc){
    tok1_izer_errtoken_set(st, &tId);
    rc = tok1_err_toker(st, rc, "Did not find end of HEREDOC "
                        "starting with '%.*s'.",
                        (int)tok1_en_len(&tId),
                        tok1_en_begin(&tId));
  }else{
    tok1_en_begin_set(tgt, tok1_en_begin(&origin));
    tok1_en_end_set(tgt, theEnd);
    tgt->ttype = TOK1_T_Heredoc;
    tok1_en_adjbegin_set(tgt, docBegin);
    tok1_en_adjend_set(tgt, docEnd);
    tgt->line = origin.line;
    tgt->column = origin.column;
    assert(docEnd>=docBegin);
#if 0
    idBegin = tok1_en_begin(&tId);
    idEnd = tok1_en_end(&tId);
    MARKER(("HEREDOC<%.*s> body: %.*s\n",
            (int)(idEnd-idBegin), idBegin,
            (int)(docEnd-docBegin), docBegin));
#endif
  }
  return rc;
  tok_err:
  assert(rc);
  assert(st->errMsg);
  rc = tok1_err_toker(st, rc, "%s", st->errMsg);
  return rc;
}

#undef MARKER

