/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/**
   s2 loadable module wrapping termbox:

   https://github.com/nsf/termbox

   The whole API is wrapped except for tb_put_cell(), but that's just
   a convenience wrapper around tb_change_cell(), so it's not
   necessary.


   The s2_tbwin class (a Window class) presented here is deprecated,
   to be replaced with s2_tbbuf (a screen cell buffer) and a
   script-side Window class. s2_tbwin is only kept around as a
   reference point during the development of s2_tbbuf.
*/
#include "libs2.h"
#include <termbox.h>
typedef struct tb_cell tb_cell;

#include <string.h> /* strlen() */
#include <assert.h>

/**
   Only for debuggering...
*/
#include <stdio.h>
#define MARKER(pfexp)                                               \
  do{ printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__);   \
    printf pfexp;                                                   \
  } while(0)

#define PRINT_CLEAR_TO_EOL 0

static const uint16_t TbRetainAttr = 0xffff;
static const uint32_t TbRetainChar = 0xFFFFffff;

enum s2_tbwin_flags {
TBWIN_F_NONE = 0x00,
TBWIN_F_HIDDEN = 0x01,
TBWIN_F_ISPAD = 0x02
};

#define s2_tbwin_is_pad(WIN) (TBWIN_F_ISPAD & (WIN)->flags)
typedef enum s2_tbwin_flags s2_tbwin_flags;
typedef struct s2_tbwin s2_tbwin;
typedef struct s2_tbbuf s2_tbbuf;

/**
   A buffers of tb_cell abstracting a rectangle with an unspecified
   position. Except for a small number, the APIs for this function do
   not update the termbox virtual screen directly - they only update
   the cells in this buffer.
*/
struct s2_tbbuf {
  /**
     Array of (this->w * this->h) cells. A given (x,y) coordinate
     corresponds to this->cells[this->w * y + x].
  */
  tb_cell * cells;
  /**
     Width of this buffer.
  */
  unsigned short w;
  /**
     Height of this buffer.
  */
  unsigned short h;
};
#define s2_tbbuf_empty_m {0,0,0}
static const s2_tbbuf s2_tbbuf_empty = s2_tbbuf_empty_m;

/**
   DEPRECATED: it's not yet clear whether it really makes sense to
   keep this class or reimplement a Window class completely in script
   code on top of s2_tbbuf.

   A "high level" Window construct for termbox. Intended to be used as
   a cwal_native value. Also used for "pads," analogous to ncurses
   PAD, which represents an off-screen drawing space of an arbitrary
   size, part of which can then be drawn over a window.
*/
struct s2_tbwin {
  /* width/height are stored in this->cells */
  int x;
  int y;
  int z;
  /**
     One active non-Pad Window has cursorX>=0. That window will be
     referenced by S2TbState.cursorWindow. All other windows must have
     a negative cursor position.

     For Pads (with no cursor support) we reinterpret these as current
     scroll positions, for use in viewports.
  */
  int cursorX;
  int cursorY;
  /**
     Default fg, used in some contexts.  Reminder: TB_DEFAULT==0 and
     apparently has different meanings for foreground and background.
  */
  uint16_t fg;
  uint16_t bg;
  /**
     Flags from the s2_tbwin_flags enum.
   */
  int flags;
  /**
     An optimizations used for figuring out when to resort if child
     z-orders change.
  */
  int needsSort;
  /**
     General-purpose buffer used by APIs which need one. Its buffered
     memory must not be exposed outside of the internal window APIs,
     as it can change or be reallocated/freed at any time.
  */
  cwal_buffer buf;
  /**
     The cwal_value (of type cwal_native) to which this instance
     is bound.
  */
  cwal_value * vSelf;
  /**
     Parent window (if any).
  */
  s2_tbwin * parent;

  /**
     A place to store references to child windows.
  */
  cwal_array * childs;

  /**
     left and right are used for linking top-level windows (only) to
     each other, primarily so that we can sort them on z-order.
  */
  s2_tbwin * left;
  s2_tbwin * right;
  s2_tbbuf cells;
};

/**
   Serves as a copy-constructable default instance and as a type ID
   pointer for cwal_new_native() and friends.
*/
static const s2_tbwin s2_tbwin_empty = {
0/*x*/,
0/*y*/,
0/*z*/,
-1/*cursorX*/,
-1/*cursorY*/,
0/*fg*/,
0/*bg*/,
0/*flags*/,
0/*needsSort*/,
cwal_buffer_empty_m/*buf*/,
0/*vSelf*/,
0/*parent*/,
0/*childs*/,
0/*left*/,
0/*right*/,
s2_tbbuf_empty_m/*cells*/
};

static struct {
  s2_tbwin * cursorWindow;
  s2_tbwin * topWindows;
  int needsSort;
} S2TbState = {
0/*cursorWindow*/,
0/*topWindows*/,
0/*needsSort*/
};

/**
   If x and y are in range of the screen size, this returns a pointer
   to the cell responsible for the given coordinates.  If screen mode
   has not been initialized or x/y are out of range, 0 is returned.
*/
tb_cell * tb_x_cell_at( int x, int y );
/**
   Termbox's author hs deprecated tb_blit(), recommending that
   we instead do the memory copying ourselves. Hmpf. So we fork
   that routine here.
*/
void tb_x_blit(int x, int y, int w, int h, const struct tb_cell *cells);

/**
   Clears the characters from all cells in the given line. If fg and
   bg are >=0 then they are used as the corresponding cell attribute,
   else they are ignored.
*/
int tb_x_clear_line(int y, int fg, int bg);

/**
   Allocates a new s2_tbbuf or returns 0.
*/
s2_tbbuf * s2_tbbuf_alloc( cwal_engine * e );

/**
   Frees all memory owned by tb, then frees tb.
*/
void s2_tbbuf_free( cwal_engine * e, s2_tbbuf *tb  );

/**
   Resizes cells to be the given width and height.

   If both width and height are 0, cells->cells is cwal_free()'d,
   making cells is ready for deallocation or reuse, and 0 is returned.

   Else if either w or h are <=0, CWAL_RC_RANGE is returned.

   Else it resizes cells->cells to the new dimensions, copying over
   any old content which still fits in the new size.

   CWAL_RC_OOM is returned if (re)allocating the buffer
   fails.

   On success, cells' state is updated and 0 is returned.
*/
int s2_tbbuf_resize( cwal_engine * e, s2_tbbuf * cells,
                     int w, int h);

/**
   Returns the cell at the given coordinates in c or 0 if the
   coordinates are out of range.
*/
tb_cell * s2_tbbuf_cell_at( s2_tbbuf const * c, int x, int y );

/**
   If the given x/y coordinates are in range for c, *tgt is set to
   the cell at that coordinate and 0 is returned, else an exception
   is triggered in e explaining that x/y are out of bounds.
*/
int s2_tbbuf_cell_at2( cwal_engine * e, s2_tbbuf const * c,
                       int x, int y, tb_cell ** tgt );

/**
   Modifies the contents of the cell at the given coordinates, setting
   its character and fg/bg attributes. If ch is TbRetainChar and/or
   either of fg/bg are TbRetainAttr then that ch resp. fg/bg are
   not modified.

   Returns 0 on success, CWAL_RC_RANGE if x/y is outside of b's
   coordinates.
*/
int s2_tbbuf_change_cell( s2_tbbuf * b, int x, int y, uint32_t ch,
                          uint16_t fg, uint16_t bg );


/**
   Draws c's cells to termbox's virtual screen at the given x/y
   position.
*/
void s2_tbbuf_blit( s2_tbbuf * c, int x, int y );

/**
   Copies cells from one s2_tbbuf instance to another.

   Draws the rectangle defined by (fromX, fromY, w, h)
   to the dest buffer at coordinates (toX, toY).
*/
void s2_tbbuf_draw_to( s2_tbbuf const * src,
                      int fromX, int fromY, int w, int h,
                      s2_tbbuf * dest, int toX, int toY);

/**
   For each tb_cell in tc with the TB_REVERSE reverse attribute set on
   its fg or bg members, this function turns off that attribute. For
   each entry not having that attribute, it toggles that attribute on.
*/
void s2_tbbuf_reverse_attr( s2_tbbuf * tc );


/**
   Equivalent to s2_tbbuf_fill_rect(tb, 0, 0, tb->w, tb->h, ch, fg, bg).
 */
void s2_tbbuf_fill( s2_tbbuf * tb, uint32_t ch, uint16_t fg, uint16_t bg );

/**
   Fills all cells in the given rectangular bounds with the given
   char/fg/bg values. It silently ignores out-of-bound cells.

   If fg or bg are TbRetainAttr, that attribute is not set. Likewise,
   if ch is TbRetainChar then ch is not applied.
*/
void s2_tbbuf_fill_rect( s2_tbbuf * tb, int x, int y, int w, int h,
                         uint32_t ch, uint16_t fg, uint16_t bg );

/**
   Equivalent to s2_tbbuf_fill(c, 0, fg, bg).
*/
void s2_tbbuf_clear( s2_tbbuf * c, uint16_t fg, uint16_t bg );

/**
   Clears the contents of a single line, leaving the attributes
   intact. Is a no-op of line is out of bounds for tb.
 */
void s2_tbbuf_clear_line_contents( s2_tbbuf *tb, int line );

/**
   Updates the tb virtual screen by zeroing out any cells
   in the given buffer, starting at the given x/y offset of
   the virtual screen. Does not call tb_present().
*/
void s2_tbbuf_clear_from_screen( s2_tbbuf const * tbuf, int x, int y );

int s2_tbbuf_line( s2_tbbuf * tw, int fromX, int fromY, int toX, int toY,
                   uint32_t lineChar, uint16_t fg, uint16_t bg );

/**
   Renders a vertical scrollbar on buffer tbb at column x, running
   from the given top row to the row (top+barHeight). The scroll
   indicator is rendered depending on the final two arguments:
   lineCount specifies the number of lines in the scrollable
   input and currentLine specifies the top-most line number of the
   currently-displayed data. e.g. a lineCount of 100 and currentLine
   of 1 puts the indicator 1% of the way down the scrollbar. Likewise,
   a lineCount of 100 and currentLine of 90 puts the indicator at the
   90% point.
 */
int s2_tbbuf_render_scrollbar_v( s2_tbbuf * tbb, int x, int top,
                                 int barHeight,
                                 int lineCount, int currentLine );

/**
   Works like s2_tbbuf_render_scrollbar_v() but renders a horizontal
   scrollbar at the given left/top coordinates of tbb.
*/
int s2_tbbuf_render_scrollbar_h( s2_tbbuf * tbb, int left, int top,
                                 int barWidth,
                                 int colCount, int currentCol );


/**
   Tries to extract an s2_tbbuf value from v or one of v's prototypes.
   On success, returns that value and assigns *foundIn (if foundIn is
   not NULL) to the cwal_value in which the buffer was found. On error,
   returns NULL.
*/
s2_tbbuf * s2_tbbuf_from_value( cwal_engine * e, cwal_value * v,
                                cwal_value ** foundIn );

/**
   Works like s2_tbbuf_from_value() but extracts the buffer
   from the argument corresponding to index, or args->self if
   index is <0.
*/
s2_tbbuf * s2_tbbuf_from_args( cwal_callback_args const * args,
                               int index, cwal_value ** foundIn );

/**
   TODO: rewire tb_prototype_xxx() to use s2_stash_hidden_member()
   to internally store a single instance of each.
*/
static cwal_value * tb_prototype_window( cwal_engine * e);
static cwal_value * tb_prototype_pad( cwal_engine * e);
static cwal_value * tb_prototype_buf( cwal_engine * e);
static const char tbbuf_PrototypeTypeID = 0;

static s2_tbwin * s2_tbwin_from_value( cwal_engine * e, cwal_value const * v );
static s2_tbwin * s2_tbwin_from_args( cwal_callback_args const * args, int index ){
  return s2_tbwin_from_value(args->engine, index < 0
                             ? args->self
                             : (index < args->argc
                                ? args->argv[(uint16_t)index]
                                : 0));
}
static tb_cell * s2_tbwin_cell_at( s2_tbwin const * tw, int x, int y );
static void s2_tbwin_abs_pos( s2_tbwin * tw, int * _x, int * _y );
static cwal_array * s2_tbwin_childs( s2_tbwin * tw, char createIfNotFound);
static int s2_tbwin_present( s2_tbwin * tw );
/**
   Clears the contents of all cells in the window, setting them to
   the given foreground/background colors.
*/
static void s2_tbwin_clear( s2_tbwin * tw, uint16_t fg, uint16_t bg );

/**
   Clears out the global-level cells corresponding to this buffer's
   rectangle, but not the buffer's copy of that state.
*/
static void s2_tbwin_clear_from_screen( s2_tbwin * tw, uint16_t fg, uint16_t bg );

/**
   Toggles the TB_REVERSE bit on tw->bg/fg and all cells of
   tw. Primarily intended as a way of highlighting in-focus windows.
*/
static void s2_tbwin_reverse_attr( s2_tbwin * tw );

static void s2_tbwin_repos( s2_tbwin * tw, int x, int y );

/**
   Transfers all of tw's contents to the global termbox screen,
   trimmed to fit the screen if needed. Because Pad Windows
   (s2_tbwin_is_pad()) have no real relationship with the global
   screen, as Windows do, this is a no-op for Pads.
*/
static void s2_tbwin_blit( s2_tbwin * tw );
static void s2_tbwin_cursor( s2_tbwin * tw, int x, int y );

/**
   Sets the visibility flag. Returns 0 if the new state
   is hidden, else non-0 (shown).
*/
static int s2_tbwin_show( s2_tbwin * tw, int showIt );

static int s2_tbwin_render_scrollbar_v( s2_tbwin * tw, int x, int y,
                                        int barHeight,
                                        int lineCount, int currentLine );
static int s2_tbwin_render_scrollbar_h( s2_tbwin * tw, int x, int y,
                                        int barWidth,
                                        int colCount, int currentCol );



/**
   UTF-8 codepoints for characters used in rendering
   scrollbars.
 */
static const struct _ScrollbarConfig {
  int vTop;
  int vBottom;
  int hLeft;
  int hRight;
  int curPos;
  int filler;
} ScrollbarConfig = {
0x25B3,
0x25BD,
0x25C1,
0x25B7,
0x25A3 /* 0x25C8 *//* 0x25A9 */,
0x2591
};

tb_cell * tb_x_cell_at( int x, int y ){
  int const sw = tb_width(), sh = tb_height();
  if(sw>0 && sh>0 && x>=0 && x<sw && y>=0 && y<sh){
    return tb_cell_buffer() + (sw * y + x);
  }
  return 0;
}

int tb_x_clear_line(int y, int fg, int bg){
  tb_cell * c = tb_x_cell_at(0, y);
  int const sw = tb_width();
  int i = 0;
  assert(c);
  if(!c) return -1;
  for( ; i < sw; ++i, ++c ){
    c->ch = 0;
#if 0
    c->fg = TB_BLACK;
    c->bg = TB_YELLOW | TB_BOLD;
#else
    if(fg>=0) c->fg = (uint16_t)(fg & 0xFFFF);
    if(bg>=0) c->bg = (uint16_t)(bg & 0xFFFF);
#endif
  }
  return 0;
}

void tb_x_blit(int x, int y, int w, int h, const struct tb_cell *cells){
  tb_cell * backbuf = tb_cell_buffer();
  int const sw = tb_width(), sh = tb_height();
  int xo = 0, yo = 0, ww = w, hh = h;
  if(sw<0 || sh<0 || !backbuf) return;
  else if (x + w < 0 || x >= sw) return;
  else if (y + h < 0 || y >= sh) return;
  else if (x < 0) {
    xo = -x;
    ww -= xo;
    x = 0;
  }
  if (y < 0) {
    yo = -y;
    hh -= yo;
    y = 0;
  }
  if (ww > sw - x) ww = sw - x;
  if (hh > sh - y) hh = sh - y;
  {
	int sy;
	struct tb_cell *dst = backbuf+(sw * y + x);
	const struct tb_cell *src = cells + yo * w + xo;
	size_t size = sizeof(struct tb_cell) * ww;
	for (sy = 0; sy < hh; ++sy) {
      memcpy(dst, src, size);
      dst += sw;
      src += w;
	}
  }
}

static int tb__get_int(cwal_value const *v){
  return (int)cwal_value_get_integer(s2_value_unwrap_c(v));
}

/*
static uint16_t tb__get_int2(cwal_value const * v, int dflt){
  if(cwal_value_is_number(v)){
    return (cwal_int_t) tb__get_int(v);
  }else{
    return dflt;
  }
}*/


s2_tbbuf * s2_tbbuf_alloc( cwal_engine * e ){
  s2_tbbuf * c = (s2_tbbuf *)cwal_malloc(e, sizeof(s2_tbbuf));
  if(c){
    *c = s2_tbbuf_empty;
    cwal_engine_adjust_client_mem(e, (int)sizeof(s2_tbbuf));
  }
  return c;
}

void s2_tbbuf_free( cwal_engine * e, s2_tbbuf *c ){
  s2_tbbuf_resize(e, c, 0, 0);
  *c = s2_tbbuf_empty;
  cwal_engine_adjust_client_mem(e, -(int)sizeof(s2_tbbuf));
  cwal_free2(e, c, sizeof(s2_tbbuf));
}

int s2_tbbuf_resize( cwal_engine * e, s2_tbbuf * c,
                       int w, int h){
  if(c->w == w && c->h==h) return 0;
  else if(!w && !h){
    if(c->cells){
      assert(c->w && c->h);
      cwal_free2(e, c->cells, sizeof(tb_cell) * c->w * c->h);
    }      
    *c = s2_tbbuf_empty;
    return 0;
  }
  else if(w<=0 || h<=0) return CWAL_RC_RANGE;
  else {
    /* When shrinking/growing, we need a temp buf
       to copy old contents over. A simple realloc() won't
       DTRT here. */
    const cwal_size_t sz = sizeof(tb_cell) * w * h;
    s2_tbbuf tmp = *c;
    tb_cell * newC = (tb_cell*)cwal_malloc(e, sz);
    if(!newC) return CWAL_RC_OOM;
    c->w = w;
    c->h = h;
    c->cells = newC;
    memset(c->cells, 0, w * h * sizeof(tb_cell));
    if(!tmp.w){
      assert(!tmp.h);
      assert(!tmp.cells);
      return 0;
    }else{
#if 0
      s2_tbbuf_clear( c, 0, 0 );
#endif 
      s2_tbbuf_draw_to( &tmp, 0, 0, tmp.w, tmp.h,
                        c, 0, 0 );
      s2_tbbuf_resize( e, &tmp, 0, 0 );
      return 0;
    }
  }
}

tb_cell * s2_tbbuf_cell_at( s2_tbbuf const * c, int x, int y ){
  if(x<0 || y<0 || x >= c->w || y>=c->h) return 0;
  else{
    assert(c->cells);
    return c->cells + (c->w * y + x);
  }
}

int s2_tbbuf_cell_at2( cwal_engine * e, s2_tbbuf const * c,
                         int x, int y, tb_cell ** tgt ){
  tb_cell * tc = s2_tbbuf_cell_at( c, x, y );
  if(tc){
    *tgt = tc;
    return 0;
  }else{
    return cwal_exception_setf(e, CWAL_RC_RANGE,
                               "Cell (x=%d, y=%d) is out of bounds "
                               "for (w=%d, h=%d).",
                               x, y, c->w, c->h);
  }
}

void s2_tbbuf_draw_to( s2_tbbuf const * src, int fromX, int fromY,
                      int w, int h,
                      s2_tbbuf * dest, int toX, int toY){
  int c, cEof, r, rEof;
  tb_cell const * cSrc;
  tb_cell * cDest;
  assert(src->h>0);
  assert(src->w>0);
  assert(dest->h>0);
  assert(dest->w>0);
  if( (cEof = fromX+w) >= src->w /* || (cEof<0) */ ) cEof = src->w;
  if( (rEof = fromY+h) >= src->h /* || (rEof<0) */ ) rEof = src->h;
  for( r = fromY; r < rEof; ++r ){
    for( c = fromX; c < cEof; ++c ){
      cSrc = s2_tbbuf_cell_at(src, c, r);
      cDest = cSrc
        ? s2_tbbuf_cell_at(dest, toX+c-fromX, toY+r-fromY)
        : 0;
      if(cDest) *cDest = *cSrc;
    }
  }
  return;
}

void s2_tbbuf_blit( s2_tbbuf * tbuf, int x, int y ){
  int r, c;
  int const tbw = tb_width(), tbh = tb_height();
  tb_cell const * bc;
  /* Make sure the pos/dims are in screen range
     or termbox will step on an invalid pointer... */
#if 1
  if(tbuf->w+x<=tbw && tbuf->h+y<=tbh){
    /* Window fits in the screen, so we can dump it directly
       to termbox... */
    tb_x_blit( x, y, tbuf->w, tbuf->h, tbuf->cells );
    return;
  }
#endif
#if 1
  if(!x && !y && tbuf->w==tbw && tbuf->h==tbh){
    /* Exact screen fit, so we can dump it directly
       to termbox... */
    tb_x_blit( 0, 0, tbw, tbh, tbuf->cells );
    return;
  }
#endif
  /* Else trim the changes to only screen-visible parts... */
  for( r = 0; r < tbuf->h; ++r ){
    if(r+y<0) continue;
    else if(r+y >= tbh) break;
    for( c = 0; c < tbuf->w; ++c ){
      if(c+x<0) continue;
      else if(c+x >= tbw) break;
      bc = s2_tbbuf_cell_at(tbuf, c, r);
      assert(bc);
      tb_change_cell( c+x, r+y, bc->ch, bc->fg, bc->bg );
    }
  }
}

void s2_tbbuf_clear_from_screen( s2_tbbuf const * tbuf, int x, int y ){
  int r, c;
  int const tbw = tb_width(), tbh = tb_height();
  /* tb_cell const * bc; */
  tb_cell * sc;
  for( r = 0; r < tbuf->h; ++r ){
    if(r+y >= tbh) break;
    for( c = 0; c < tbuf->w; ++c ){
      if(c+x >= tbw) break;
      sc = tb_x_cell_at( c+x, r+y );
      if(sc){
        sc->ch = 0;
        sc->fg = sc->bg = 0;
      }
    }
  }
}

void s2_tbbuf_reverse_attr( s2_tbbuf * tc ){
  tb_cell * bc;
#if 0
  int const max = tc->w * tc->h;
  int i;
  uint16_t tmp;
  for( i = 0; i < max; ++i ){
    bc = tc->cells+i;
    tmp = bc->bg;
    bc->bg = bc->fg;
    bc->fg = tmp;
  }
#elif 1
  int const max = tc->w * tc->h;
  int i;
  for( i = 0; i < max; ++i ){
    bc = tc->cells+i;
    if(bc->bg & TB_REVERSE) bc->bg &= ~TB_REVERSE;
    else bc->bg |= TB_REVERSE;
    if(bc->fg & TB_REVERSE) bc->fg &= ~TB_REVERSE;
    else bc->fg |= TB_REVERSE;
  }
#else
  int r, c;
  for( r = 0; r < tc->h; ++r ){
    for( c = 0; c < tc->w; ++c ){
      bc = s2_tbbuf_cell_at(tc, c, r);
      assert(bc);
      if(bc->bg & TB_REVERSE) bc->bg &= ~TB_REVERSE;
      else bc->bg |= TB_REVERSE;
      if(bc->fg & TB_REVERSE) bc->fg &= ~TB_REVERSE;
      else bc->fg |= TB_REVERSE;
    }
  }
#endif
}

/**
   Sets the attributes for the given cell. If all bits of fg/bg are set
   then those fields are not modified. Likewise, if all bits of ch are
   set, it is also not set.
*/
int s2_tbbuf_change_cell( s2_tbbuf * tb, int x, int y, uint32_t ch,
                          uint16_t fg, uint16_t bg ){
  tb_cell * c = s2_tbbuf_cell_at(tb, x, y);
  if(!c) return CWAL_RC_RANGE;
  if(TbRetainChar != ch) c->ch = ch;
  if(TbRetainAttr != fg) c->fg = fg;
  if(TbRetainAttr != bg) c->bg = bg;
  return 0;
}

void s2_tbbuf_fill_rect( s2_tbbuf * tb, int x, int y, int w, int h,
                         uint32_t ch, uint16_t fg, uint16_t bg ){
  int r, c;
  tb_cell * tc;
  for( r = y; r < tb->h && r<y+h; ++r ){
    for( c = x; c < tb->w && c<x+w; ++c ){
      tc = s2_tbbuf_cell_at( tb, c, r );
      if(!tc) continue;
      if(TbRetainAttr != fg) tc->fg = fg;
      if(TbRetainAttr != bg) tc->bg = bg;
      if(TbRetainChar != ch) tc->ch = ch;
    }
  }
}

void s2_tbbuf_fill( s2_tbbuf * tb, uint32_t ch, uint16_t fg, uint16_t bg ){
  s2_tbbuf_fill_rect(tb, 0, 0, tb->w, tb->h, ch, fg, bg);
}


void s2_tbbuf_clear( s2_tbbuf * tc, uint16_t fg, uint16_t bg ){
  s2_tbbuf_fill(tc, 0, fg, bg);
}


s2_tbbuf * s2_tbbuf_from_value( cwal_engine * e, cwal_value * v,
                                cwal_value ** foundIn){
  s2_tbbuf * rc = NULL;
  do{
    cwal_native * n = cwal_value_get_native(v);
    if(n && (rc = (s2_tbbuf*)cwal_native_get(n, &s2_tbbuf_empty))){
      if(foundIn) *foundIn = v;
      break;
    }
    else v = cwal_value_prototype_get(e, v);
  }while(v);
  return rc;
}

s2_tbbuf * s2_tbbuf_from_args( cwal_callback_args const * args, int index,
                                      cwal_value ** foundIn){
  return s2_tbbuf_from_value(args->engine, index < 0
                             ? args->self
                             : (index < args->argc
                                ? args->argv[(uint16_t)index]
                                : 0),
                             foundIn);
}

static void cwal_finalize_f_s2_tbbuf( cwal_engine * e, void * v ){
  s2_tbbuf_free(e, (s2_tbbuf *)v);
}


static uint16_t tb__get_u16(cwal_value const * v, uint16_t dflt){
  v = s2_value_unwrap_c(v);
  if(cwal_value_is_number(v)){
    cwal_int_t const i = tb__get_int(v);
    return i<0
      ? dflt
      : (uint16_t)(i & 0xFFFF);
  }else{
    return dflt;
  }
}

static uint32_t tb__get_u32(cwal_value const * v, uint32_t dflt){
  v = s2_value_unwrap_c(v);
  if(cwal_value_is_number(v)){
    cwal_int_t const i = tb__get_int(v);
    return i<0
      ? dflt
      : (uint32_t)i;
  }else{
    return dflt;
  }
}

static uint32_t tb__get_u32_char( cwal_value const * v,
                                  uint32_t dflt){
  cwal_size_t slen = 0;
  unsigned char const * str;
  uint32_t ch;
  v = s2_value_unwrap_c(v);
  str = (unsigned char const *)cwal_value_get_cstr(v, &slen);
  if(str){
    ch = slen
      ? cwal_utf8_read_char( str, str + slen, &str )
      : 0;
  }else if(cwal_value_is_number(v)){
    ch = tb__get_u32(v, dflt);
  }else{
    ch = dflt;
  }
  return ch;
}

void s2_tbbuf_clear_line_contents( s2_tbbuf *tb, int line ){
  if(line>=0 && line<tb->h){
    tb_cell * bc;
    int c;
    for( c = 0; c < tb->w; ++c ){
      bc = s2_tbbuf_cell_at(tb, c, line);
      assert(bc);
      bc->ch = 0;
    }
  }
}


int s2_tbbuf_line( s2_tbbuf * tbb, int fromX, int fromY, int toX, int toY,
                   uint32_t lineChar, uint16_t fg, uint16_t bg ){
  int rc = 0, c, r, tmp;
  tb_cell * bc;
  if(fromX>toX){
    tmp = fromX;
    fromX = toX;
    toX = tmp;
  }
  if(fromY>toY){
    tmp = fromY;
    fromY = toY;
    toY = tmp;
  }
  for( r = fromY; !rc && r <= toY; ++r ){
    for( c = fromX; !rc && c <= toX; ++c ){
      bc = s2_tbbuf_cell_at(tbb, c, r);
      if(bc){
        bc->fg = fg;
        bc->bg = bg;
        bc->ch = lineChar;
      }
      /* rc = s2_tbbuf_change_cell(tbb, c, r, lineChar, fg, bg); */
    }
  }
  return rc;
}

int s2_tbbuf_render_scrollbar_v( s2_tbbuf * tbb, int x, int top,
                                 int barHeight,
                                 int lineCount, int currentLine ){
  int y, bottom = top + barHeight - 1,
    dotPos = top + (int)((double)currentLine / lineCount * barHeight) +1;
  struct _ScrollbarConfig const * const conf = &ScrollbarConfig;
  s2_tbbuf_change_cell( tbb, x, top, conf->vTop, TbRetainAttr, TbRetainAttr);
  for(y = top+1; y < bottom; ++y ){
    s2_tbbuf_change_cell( tbb, x, y, conf->filler, TbRetainAttr, TbRetainAttr);
  }
  s2_tbbuf_change_cell( tbb, x, dotPos>=bottom?bottom-1:dotPos,
                        conf->curPos, TbRetainAttr, TbRetainAttr);
  s2_tbbuf_change_cell( tbb, x, bottom, conf->vBottom,
                        TbRetainAttr, TbRetainAttr);
  return 0;
}

int s2_tbbuf_render_scrollbar_h( s2_tbbuf * tbb, int left, int top,
                                 int barWidth,
                                 int colCount, int currentCol ){
  int x, right = left + barWidth - 1,
    dotPos = left + (int)((double)currentCol / colCount * barWidth) +1;
  struct _ScrollbarConfig const * const conf = &ScrollbarConfig;
  s2_tbbuf_change_cell( tbb, left, top, conf->hLeft, TbRetainAttr, TbRetainAttr);
  for(x = left+1; x < right; ++x ){
    s2_tbbuf_change_cell( tbb, x, top, conf->filler, TbRetainAttr, TbRetainAttr);
  }
  s2_tbbuf_change_cell( tbb, dotPos>=x?x-1:dotPos, top,
                        conf->curPos, TbRetainAttr, TbRetainAttr);
  s2_tbbuf_change_cell( tbb, x, top, conf->hRight,
                        TbRetainAttr, TbRetainAttr);
  return 0;
}

/**
   CellBuffer CellBuffer.drawScrollbarV( left, top, barHeight, totalLineCount, currentLine )
*/
static int cb_buf_scrollbar_v( cwal_callback_args const * args,
                                  cwal_value **rv ){
  int x, top, barHeight, lineCount, currentLine;
  s2_tbbuf * tbb = args->argc
    ? s2_tbbuf_from_args(args, -1, 0)
    : 0;
  if(!tbb || args->argc!=5){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting (x,y,barHeight,lineCount,currentLine) "
                               "arguments.");
  }
  x = tb__get_int(args->argv[0]);
  top = tb__get_int(args->argv[1]);
  barHeight = tb__get_int(args->argv[2]);
  lineCount = tb__get_int(args->argv[3]);
  currentLine = tb__get_int(args->argv[4]);
  if(x<0 || top<0 || barHeight<3 || lineCount < 1 || currentLine < 0){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting larger values for: "
                               "(x=%d,y=%d,barHeight=%d,lineCount=%d,"
                               "currentLine=%d).",
                               x,top,barHeight,lineCount,currentLine);
  }
  s2_tbbuf_render_scrollbar_v(tbb, x, top, barHeight, lineCount, currentLine);
  *rv = args->self;
  return 0;
}

/**
   CellBuffer CellBuffer.drawScrollbarH( left, top, barWidth, totalColumnCount, currentColumn )
*/
static int cb_buf_scrollbar_h( cwal_callback_args const * args,
                                  cwal_value **rv ){
  int top, left, barWidth, colCount, currentCol;
  s2_tbbuf * tbb = args->argc
    ? s2_tbbuf_from_args(args, -1, 0)
    : 0;
  if(!tbb || args->argc!=5){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting (x,y,barWidth,colCount,currentCol) "
                               "arguments.");
  }
  left = tb__get_int(args->argv[0]);
  top = tb__get_int(args->argv[1]);
  barWidth = tb__get_int(args->argv[2]);
  colCount = tb__get_int(args->argv[3]);
  currentCol = tb__get_int(args->argv[4]);
  if(left<0 || top<0 || barWidth<3 || colCount < 1 || currentCol < 0){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting larger values for: "
                               "(x=%d,y=%d,barWidth=%d,colCount=%d,"
                               "currentCol=%d).",
                               left,top,barWidth,colCount,currentCol);
  }
  s2_tbbuf_render_scrollbar_h(tbb, left, top, barWidth, colCount, currentCol);
  *rv = args->self;
  return 0;
}

static int cb_buf_ctor( cwal_callback_args const * args,
                        cwal_value **rv ){
  cwal_native * n;
  s2_tbbuf * buf;
  int rc = 0, w=0, h=0;
  if(args->argc && 2!=args->argc) goto misuse;
  if(2==args->argc){
    w = tb__get_int(args->argv[0]);
    h = tb__get_int(args->argv[1]);
    if(w<=0 || h<=0) goto misuse;
  }
  buf = s2_tbbuf_alloc( args->engine );
  if(!buf) return CWAL_RC_OOM;
  n = cwal_new_native(args->engine, buf, cwal_finalize_f_s2_tbbuf,
                      &s2_tbbuf_empty);
  if(!n){
    s2_tbbuf_free(args->engine, buf);
    return CWAL_RC_OOM;
  }
  if(w>0 && h>0){
    rc = s2_tbbuf_resize(args->engine, buf, w, h);
    if(rc){
      cwal_native_clear(n, 1)/*destroys buf*/;
      n = 0;
      buf = 0;
    }
  }
  if(!rc){
    cwal_value * proto =
      (cwal_value *)cwal_args_state(args, &tbbuf_PrototypeTypeID);
    cwal_value * vn = cwal_native_value(n);
    assert(proto);
    assert(vn);
    assert(buf == s2_tbbuf_from_value(args->engine, vn, 0));
    cwal_value_prototype_set(vn, proto);
    *rv = vn;
  }
  return rc;
  misuse:
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting ([int,int]) args, both >0.");
}

/* Helper macro for cwal_callback_f() implementations bound to
   s2_tbbuf instances which ensures that the arguments->self value is
   a s2_tbbuf instance... */
#define THIS_CBUF                                                     \
  s2_tbbuf * self = s2_tbbuf_from_value(args->engine, args->self, 0);   \
  if(!self) return cwal_exception_setf(args->engine, CWAL_RC_TYPE,      \
                                       "'this' is not (or is no longer) " \
                                       "a CellBuffer instance.")

/**
   int CellBuffer.width()
*/
static int cb_buf_width( cwal_callback_args const * args,
                            cwal_value **rv ){
  THIS_CBUF;
  *rv = cwal_new_integer(args->engine, (cwal_int_t)self->w);
  return *rv ? 0 : CWAL_RC_OOM;
}

/**
   int CellBuffer.height()
*/
static int cb_buf_height( cwal_callback_args const * args,
                             cwal_value **rv ){
  THIS_CBUF;
  *rv = cwal_new_integer(args->engine, (cwal_int_t)self->h);
  return *rv ? 0 : CWAL_RC_OOM;
}

/**
   Object|CellBuffer CellBuffer.size([ int w, int h ])

   If passed no args, returns an object in the form {w: width, h: height}.
   If passed any args, returns cb_window_resize().
*/
static int cb_buf_size( cwal_callback_args const * args,
                        cwal_value **rv ){
  THIS_CBUF;
  if(2==args->argc){
    int const w = tb__get_int(args->argv[0]),
      h = tb__get_int(args->argv[1]);
    if(w<0 || h<0) goto misuse;
    else{
      int const rc = s2_tbbuf_resize(args->engine, self, w, h);
      if(!rc) *rv = args->self;
      return rc;
    }
    assert(!"unreached");
  }
  else if(args->argc) goto misuse;
  else{
    int rc = 0;
    cwal_value * obj;
    cwal_value * v;
    obj = cwal_new_object_value(args->engine);
    if(!obj) return CWAL_RC_OOM;
#define SET(K,V)                                   \
    v = V;                                           \
    cwal_value_ref(v);                               \
    rc = v ? cwal_prop_set(obj, K, cwal_strlen(K), v) : CWAL_RC_OOM;    \
    cwal_value_unref(v);                             \
    if(rc) goto end
#define INT(X) cwal_new_integer(args->engine, (cwal_int_t)(X))
    SET("w",INT(self->w));
    SET("h",INT(self->h));
#undef SET
#undef INT
    end:
    if(rc) cwal_value_unref(obj);
    else *rv = obj;
    return rc;
  }
  assert(!"unreached");
  misuse:
  return cwal_cb_throw(args, CWAL_RC_MISUSE,
                     "Expecting ([int,int]) arguments, "
                     "both positive.");
}

static int cb_buf_blit( cwal_callback_args const * args,
                        cwal_value **rv ){
  int x, y;
  THIS_CBUF;
  if(2!=args->argc){
      return cwal_cb_throw(args, CWAL_RC_MISUSE,
                     "Expecting ([int,int]) arguments.");
  }
  x = tb__get_int(args->argv[0]);
  y = tb__get_int(args->argv[1]);
  s2_tbbuf_blit(self, x, y);
  *rv = args->self;
  return 0;
}

/**
   Usage: this.thisFunc(x, y, theChar[, fg, bg]).
   theChar may be either a stringable type (string/buffer)
   or an integer (default is to assume an integer). If theChar
   is a string, its first character is used.

   Returns this object.
*/
static int cb_buf_change_cell( cwal_callback_args const * args,
                               cwal_value **rv ){
  int rc, x, y;
  tb_cell * cell = 0;
  uint32_t ch = TbRetainChar;
  uint16_t fg = TbRetainAttr, bg = TbRetainAttr;
  THIS_CBUF;
  if(args->argc<3){
    /* TODO: if only 2 args, return cell contents as an object */
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting (x,y,char [,fg, bg]) "
                               "parameters.");
  }
  x = tb__get_int(args->argv[0]);
  y = tb__get_int(args->argv[1]);
  if( (rc = s2_tbbuf_cell_at2(args->engine,
                              self, x, y, &cell)) ){
    return cwal_cb_throw(args, rc, "Cell change at (%d,%d) "
                       "failed with code %d (%s).",
                       x, y, rc, cwal_rc_cstr(rc));
  }
  ch = tb__get_u32_char(args->argv[2], TbRetainChar);
  if(args->argc>3){
    fg = tb__get_u16(args->argv[3], TbRetainAttr);
    if(args->argc>4){
      bg = tb__get_u16(args->argv[4], TbRetainAttr);
    }
  }
  if(TbRetainAttr != fg) cell->fg = fg;
  if(TbRetainAttr != bg) cell->bg = bg;
  if(TbRetainChar != ch) cell->ch = ch;
  /* s2_tbbuf_change_cell(self, x, y, ch, fg, bg); */
  *rv = args->self;
 return 0;
}

static void cb_fg_bg( cwal_callback_args const * args,
                      uint16_t fgIndex, uint16_t * fg,
                      uint16_t bgIndex, uint16_t * bg ){
  uint16_t f = TbRetainAttr, b = TbRetainAttr;
  if(args->argc > fgIndex) f = tb__get_u16(args->argv[fgIndex], TbRetainAttr);
  if(args->argc > bgIndex) b = tb__get_u16(args->argv[bgIndex], TbRetainAttr);
  *fg = f;
  *bg = b;
}

/**
   this f(x, y, w, h, char, fg, bg)
 */
static int cb_buf_fill_rect( cwal_callback_args const * args,
                             cwal_value **rv ){
  uint16_t fg, bg;
  uint32_t ch;
  int x, y, w, h;
  THIS_CBUF;
  if(args->argc<4){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting (x,y,w,h[,char,fg,bg]) arguments.");
  }
  x = tb__get_int(args->argv[0]);
  y = tb__get_int(args->argv[1]);
  w = tb__get_int(args->argv[2]);
  h = tb__get_int(args->argv[3]);
  ch = args->argc>4
    ? tb__get_u32_char(args->argv[4], TbRetainChar)
    : 0;
  cb_fg_bg(args, 5, &fg, 6, &bg);
  s2_tbbuf_fill_rect( self, x, y, w, h, ch, fg, bg );
  *rv = args->self;
  return 0;
}

static int cb_buf_fill( cwal_callback_args const * args,
                        cwal_value **rv ){
  uint16_t fg, bg;
  uint32_t ch;
  THIS_CBUF;
  ch = args->argc
    ? tb__get_u32_char(args->argv[0], TbRetainChar)
    : 0;
  cb_fg_bg(args, 1, &fg, 2, &bg);
  s2_tbbuf_fill( self, ch, fg, bg );
  *rv = args->self;
  return 0;
}

static int cb_buf_clear_line( cwal_callback_args const * args,
                              cwal_value **rv ){
  THIS_CBUF;
  if(1!=args->argc){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting (int line) argument.");
  }else{
    int const line = tb__get_int(args->argv[0]);
    s2_tbbuf_clear_line_contents(self, line);
    *rv = args->self;
    return 0;
  }
}


/**
   argsIndex == 2 for print3() and 4 for print5()
*/
static int cb_buf_print_impl( cwal_callback_args const * args,
                              cwal_value **rv, uint16_t argsIndex ){
  int x, y, rc = 0;
  int w;
  uint16_t i;
  uint16_t fg = TbRetainAttr, bg = TbRetainAttr;
  cwal_buffer buf = cwal_buffer_empty;
  unsigned char const * str = 0;
  THIS_CBUF;
  assert(2==argsIndex || 4==argsIndex);
  assert(2==argsIndex ? (args->argc > 2) : (args->argc>4));
  x = tb__get_int(args->argv[0]);
  y = tb__get_int(args->argv[1]);
  if(4 == argsIndex){
    cb_fg_bg( args, 2, &fg, 3, &bg );
  }
  for( i = argsIndex; !rc && (i < args->argc);++i ){
    rc = s2_value_to_buffer(args->engine, &buf, args->argv[i]);
  }
  if(rc) goto end;
  else if(!buf.used){
    s2_tbbuf_change_cell(self, x, y, 0, fg, bg);
    goto end;
  }
  str = buf.mem;
  w = self->w;
  while( str < buf.mem+buf.used ){
    uint32_t uni;
    str += tb_utf8_char_to_unicode(&uni, (char const *)str);
    switch(uni){
      case (uint32_t)'\n':
        while(x < w){ /* clear to EOL */
          s2_tbbuf_change_cell(self, x++, y, 0, fg, bg);
        }
        break;
#if 0
#define TOGGLE(TBFLAG,BGTOO)                              \
        if(fg & TBFLAG) fg &= ~TBFLAG; else fg |= TBFLAG; \
        if(!BGTOO){} else if(bg & TBFLAG) bg &= ~TBFLAG; else bg |= TBFLAG
      case (uint32_t)'\b':
        TOGGLE(TB_BOLD,0);
        break;
      case (uint32_t)'\v':
        TOGGLE(TB_REVERSE,1);
        break;
      case (uint32_t)'\f':
        TOGGLE(TB_UNDERLINE,0);
        break;
#undef TOGGLE
#endif
      default:
        s2_tbbuf_change_cell(self, x++, y, uni, fg, bg);
        break;
    }
    if(x==w){
      x = 0;
      ++y;
    }
  }
#if 0 && PRINT_CLEAR_TO_EOL
  while(x < w){ /* clear to EOL */
    s2_tbbuf_change_cell(self, x++, y, 0, fg, bg);
  }
#endif
  end:
  cwal_buffer_reserve(args->engine, &buf, 0);
  if(!rc) *rv = args->self;
  return rc;
}

static int cb_buf_print5( cwal_callback_args const * args,
                      cwal_value **rv ){
  return args->argc<5
    ? cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                          "Expecting (x,y,fg,bg,...) "
                          "parameters.")
    : cb_buf_print_impl( args, rv, 4 );
}

static int cb_buf_print3( cwal_callback_args const * args,
                          cwal_value **rv ){
  return (args->argc<3)
    ? cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                          "Expecting (x,y,...) "
                          "parameters.")
    : cb_buf_print_impl( args, rv, 2 );
}

static int cb_buf_reverse_attr( cwal_callback_args const * args,
                                cwal_value **rv ){
  THIS_CBUF;
  s2_tbbuf_reverse_attr(self);
  *rv = args->self;
  return 0;
}

static int cb_buf_line_impl( cwal_callback_args const * args,
                             cwal_value **rv,
                             char isHoriz,
                             uint32_t lineChar){
  int fromX, fromY, toX, toY, rc = 0;
  uint16_t fg = TbRetainAttr, bg = TbRetainAttr;
  THIS_CBUF;
  if( args->argc<3 ){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting (fromX, fromY, "
                               "%s [, fg, bg]) arguments.",
                               isHoriz ? "toX" : "toY");
  }
  fromX = tb__get_int(args->argv[0]);
  fromY = tb__get_int(args->argv[1]);
  if(isHoriz){
    toX = tb__get_int(args->argv[2]);
    toY = fromY;
  }else{
    toX = fromX;
    toY = tb__get_int(args->argv[2]);
  }
  cb_fg_bg( args, 3, &fg, 4, &bg );
  rc = s2_tbbuf_line( self, fromX, fromY, toX, toY, lineChar, fg, bg );
  if(!rc) *rv = args->self;
  return rc;
}

static int cb_buf_vline( cwal_callback_args const * args,
                         cwal_value **rv ){
  return cb_buf_line_impl(args, rv, 0, 0x2502);
}

static int cb_buf_hline( cwal_callback_args const * args,
                         cwal_value **rv ){
  return cb_buf_line_impl(args, rv, 1, 0x2500U);
}

/**
   Script usage:

   this this.boxIn([topBottomLineWidth=1 [,sideLineWidth = topBottomLineWidth
                   [,fg [, bg]]]]);

   The line widths must be 0, 1, or 2 (defaulting 0 if it doesn't know
   what to do).  A width of 0 means to use spaces. By default it
   re-uses whatever fg/bg attributes the cells have.

   It sets up the corner characters based on the line widths of the
   adjoining sides, using either single- or double-lined drawing
   characters.
*/
static int cb_buf_box_in( cwal_callback_args const * args,
                          cwal_value **rv ){
  uint16_t fg, bg;
  int wHoriz = 1, wVert;
  uint32_t hChar = 0x20, vChar = 0x20,
    topLeft = 0x20, topRight = 0x20, bLeft = 0x20, bRight = 0x20; 
  THIS_CBUF;
#define NUM(N,VAR,DFLT) if(args->argc>(N)) VAR = tb__get_u32(args->argv[N], DFLT)
  NUM(0,wHoriz,wHoriz);
  NUM(1,wVert, wHoriz);
  else wVert = wHoriz;
#undef NUM

  cb_fg_bg( args, 2, &fg, 3, &bg );
  
  if(2==wHoriz){ /* double line */
    hChar = 0x2550;
    if(2==wVert){
      vChar = 0x2551;
      topLeft = 0x2554;
      topRight = 0x2557;
      bLeft = 0x255A;
      bRight = 0x255D;
    }else{
      if(1==wVert) vChar = 0x2502;
      topLeft = 0x2552;
      topRight = 0x2555;
      bLeft = 0x2558;
      bRight = 0x255B;
    }
  }else{
    if(1==wHoriz) hChar = 0x2500 /* single line */;
    if(2==wVert){
      vChar = 0x2551;
      topLeft = 0x2553;
      topRight = 0x2556;
      bLeft = 0x2559;
      bRight = 0x255C;
    }else{ /* spaces */
      if(1==wVert) vChar = 0x2502;
      topRight = 0x2510;
      topLeft = 0x250C;
      bLeft = 0x2514;
      bRight = 0x2518;
    }
  }
  if(wVert<=0 && wHoriz<=0){
    hChar = vChar = topLeft = topRight = bLeft = bRight = 0;
  }

  /* Corners... */
  s2_tbbuf_change_cell( self, 0, 0, topLeft, fg, bg);
  s2_tbbuf_change_cell( self, self->w-1, 0, topRight, fg, bg);
  s2_tbbuf_change_cell( self, self->w-1, self->h-1, bRight,
                        fg, bg);
  s2_tbbuf_change_cell( self, 0, self->h-1, bLeft, fg, bg);

  /* Sides... */
  s2_tbbuf_line( self, 1, 0, self->w-2, 0, hChar, fg, bg);
  s2_tbbuf_line( self, 1, self->h-1, self->w-2,
                 self->h-1, hChar, fg, bg);

  /* Top/bottom... */
  s2_tbbuf_line( self, 0, 1, 0, self->h-2, vChar, fg, bg);
  s2_tbbuf_line( self, self->w-1, 1, self->w-1,
                 self->h-2, vChar, fg, bg);
  *rv = args->self;
  return 0;
}

/**
   Script usage:

   this this.boxIn2( [hChar [, vChar [, fg [, bg]]] )
 */
static int cb_buf_box_in2( cwal_callback_args const * args,
                           cwal_value **rv ){
  uint16_t fg, bg;
  uint32_t hChar = 0x2500, vChar = 0x2502,
    topLeft = 0x250C, topRight = 0x2510, bLeft = 0x2514, bRight = 0x2518; 
  THIS_CBUF;

  if(args->argc>0) hChar = tb__get_u32_char(args->argv[0], hChar);
  if(args->argc>1) vChar = tb__get_u32_char(args->argv[1], vChar);
  cb_fg_bg( args, 2, &fg, 3, &bg );

  switch(hChar){
    case 0:
      topLeft = topRight = bLeft = bRight = (uint32_t)' ';
      break;
    case 0x2D /* - */:
      topLeft = topRight = bLeft = bRight = (uint32_t)'+';
      break;
    case 0x2550: /* double-line */
      if(0x2551==vChar){
        topLeft = 0x2554;
        topRight = 0x2557;
        bLeft = 0x255A;
        bRight = 0x255D;
      }else{
        topLeft = 0x2552;
        topRight = 0x2555;
        bLeft = 0x2558;
        bRight = 0x255B;
      }
      break;
  }

  switch(vChar){
#if 1
    case 0x00 /* NUL */:
    case 0x20 /* space */:
      if(vChar == hChar){
        topLeft = topRight = bLeft = bRight = vChar;
      }
      break;
#endif
    case 0x7C /* | */:
      topLeft = topRight = bLeft = bRight = (uint32_t)'+';
      break;
    case 0x2551: /* double-line */
      if(0x2550==hChar){/* double line */
        topLeft = 0x2554;
        topRight = 0x2557;
        bLeft = 0x255A;
        bRight = 0x255D;
      }else{
        topLeft = 0x2553;
        topRight = 0x2556;
        bLeft = 0x2559;
        bRight = 0x255C;
      }
      break;
  }

  /* Corners... */
  s2_tbbuf_change_cell( self, 0, 0, topLeft, fg, bg);
  s2_tbbuf_change_cell( self, self->w-1, 0,
                        topRight, fg, bg);
  s2_tbbuf_change_cell( self, self->w-1, self->h-1,
                        bRight, fg, bg);
  s2_tbbuf_change_cell( self, 0, self->h-1, bLeft, fg, bg);

  /* Sides... */
  s2_tbbuf_line( self, 1, 0, self->w-2, 0, hChar, fg, bg);
  s2_tbbuf_line( self, 1, self->h-1, self->w-2,
                 self->h-1, hChar, fg, bg);

  /* Top/bottom... */
  s2_tbbuf_line( self, 0, 1, 0, self->h-2, vChar, fg, bg);
  s2_tbbuf_line( self, self->w-1, 1, self->w-1,
                 self->h-2, vChar, fg, bg);

  *rv = args->self;
  return 0;
}

/**
   Reminder: this is VERY BAD when a script-exposed s2_tbbuf is used
   by a script-exposed s2_tbwin (which is now deprecated, to be
   removed once any "missing" s2_tbbuf APIs are ported over from
   s2_tbwin).
*/
static int cb_buf_destroy( cwal_callback_args const * args,
                           cwal_value **rv ){
  cwal_native * nat;
  cwal_value * natV = 0;
  s2_tbbuf * tbb =
    s2_tbbuf_from_value(args->engine, args->self, &natV);
  if(!tbb){
    return cwal_cb_throw(args, CWAL_RC_TYPE,
                       "'this' is not (or is no longer) "
                       "a CellBuffer instance.");
  }
  assert(natV);
  nat = cwal_value_get_native(natV);
  assert(nat);
  cwal_native_clear(nat, 1);
  *rv = cwal_value_undefined();
  return 0;
}


static int cb_buf_clear_from_screen( cwal_callback_args const * args,
                                     cwal_value **rv ){
  int x, y;
  THIS_CBUF;
  x = args->argc ? tb__get_int(args->argv[0]) : 0;
  y = args->argc>1 ? tb__get_int(args->argv[1]) : 0;
  s2_tbbuf_clear_from_screen(self, x, y);
  *rv = args->self;
  return 0;
}

/**
   Script usage differs from the native API (arg ordering):

   this f(CellBuffer dest, toX=0, toY=0,
          fromX=0, fromY=0, fromW=this.w, fromH=this.h)

   This order is thought to be more useful in script code,
   where we normally only need the target x/y.
 */
static int cb_buf_draw_to( cwal_callback_args const * args,
                           cwal_value **rv ){
  int fromX, fromY, fromW, fromH, toX, toY;
  uint16_t argn = 0;
  s2_tbbuf * wDest;
  THIS_CBUF;
  if( !args->argc ) goto misuse;
  wDest = s2_tbbuf_from_args(args, argn++, 0);
  if(!wDest) goto misuse;
#define NEXT(dflt) (args->argc>argn) ? tb__get_int(args->argv[argn++]) : dflt
  toX = NEXT(0);
  toY = NEXT(0);
  fromX = NEXT(0);
  fromY = NEXT(0);
  fromW = NEXT(self->w);
  fromH = NEXT(self->h);
#undef NEXT
  s2_tbbuf_draw_to( self, fromX, fromY, fromW, fromH, wDest, toX, toY );
  *rv = args->self;
  return 0;
misuse:
  return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                             "Expecting (CellBuffer [,fromX, fromY, toX, "
                             "toY, fromWidth, fromHeight]) arguments.");
}


cwal_value * tb_prototype_buf( cwal_engine * e ){
  int rc = 0;
  cwal_value * v;
  s2_engine * se = s2_engine_from_state(e);
  cwal_value * proto;

  proto = cwal_new_object_value(se->e);
  if(!proto){
    rc = CWAL_RC_OOM;
    goto end;
  }
  cwal_value_ref(proto);

  rc = s2_typename_set( se, proto, "CellBuffer", 10 );
  if(rc) goto end;

  v = cwal_new_function_value( e, cb_buf_ctor, proto,
                               0, &tbbuf_PrototypeTypeID );
  if(v){
    /* Function state (the prototype) does not (cannot) be
       automatically re-scoped when v is, so we have to store
       it someplace where it is safe in that regard... */
    cwal_function * vf = cwal_value_get_function(v);
    assert(proto == cwal_function_state_get(vf, &tbbuf_PrototypeTypeID));
    cwal_value_ref(v);
    rc = s2_stash_hidden_member(v, proto);
    if(!rc) rc = s2_ctor_method_set(se, proto, vf);
    cwal_value_unref(v);
    v = 0;
  }else{
    rc = CWAL_RC_OOM;
  }
  if(rc) goto end;

  {
    s2_func_def const funcs[] = {
      S2_FUNC2("width", cb_buf_width),
      S2_FUNC2("size", cb_buf_size),
      S2_FUNC2("scrollBarV", cb_buf_scrollbar_v),
      S2_FUNC2("scrollBarH", cb_buf_scrollbar_h),
      S2_FUNC2("reverseAttr", cb_buf_reverse_attr),
      S2_FUNC2("print5", cb_buf_print5),
      S2_FUNC2("print3", cb_buf_print3),
      S2_FUNC2("lineV", cb_buf_vline),
      S2_FUNC2("lineH", cb_buf_hline),
      S2_FUNC2("height", cb_buf_height),
      S2_FUNC2("fillRect", cb_buf_fill_rect),
      S2_FUNC2("fill", cb_buf_fill),
      S2_FUNC2("drawTo", cb_buf_draw_to),
      S2_FUNC2("destroy", cb_buf_destroy),
      S2_FUNC2("clearLine", cb_buf_clear_line),
      S2_FUNC2("clearFromScreen", cb_buf_clear_from_screen),
      S2_FUNC2("cell", cb_buf_change_cell),
      S2_FUNC2("boxIn2", cb_buf_box_in2),
      S2_FUNC2("boxIn", cb_buf_box_in),
      S2_FUNC2("blit", cb_buf_blit),
      s2_func_def_empty_m
    };
    rc = s2_install_functions(se, proto, funcs, 0);
    if(rc) goto end;
  }
  assert(!rc);
  cwal_value_prototype_set(proto, 0);  
  end:
  if(rc){
    cwal_value_unref(proto);
    proto = 0;
  }else{
    cwal_value_unhand(proto);
  }
  return proto;
}


static void s2_tbwin_unlink( s2_tbwin * tw ){
  s2_tbwin * l = tw->left;
  s2_tbwin * r = tw->right;
  if(l){
    assert(l->right == tw);
    l->right = tw->right;
    tw->left = 0;
  }
  if(r){
    assert(r->left == tw);
    r->left = l;
    tw->right = 0;
  }
  if(S2TbState.cursorWindow==tw){
    s2_tbwin_cursor(tw, -1, -1);
    assert(S2TbState.cursorWindow!=tw);
  }
  if(tw == S2TbState.topWindows){
    S2TbState.topWindows = l ? l : r;
  }
}

static void s2_tbwin_link( s2_tbwin * tw ){
  s2_tbwin * list = S2TbState.topWindows;
  assert(!tw->left);
  assert(!tw->right);
  assert(!tw->parent);
  if(list){
    assert(!list->left);
    list->left = tw;
    tw->right = list;
  }
  S2TbState.topWindows = tw;
  ++S2TbState.needsSort;
}


int s2_tbwin_show( s2_tbwin * tw, int showIt ){
  if(showIt){
    tw->flags &= ~TBWIN_F_HIDDEN;
  }else if(!(tw->flags & TBWIN_F_HIDDEN)){
    tw->flags |= TBWIN_F_HIDDEN;
    s2_tbwin_clear_from_screen(tw, TB_WHITE, TB_BLACK /*0, 0*/);
  }
  return (tw->flags & TBWIN_F_HIDDEN) ? 0 : 1;
}

#if 0
static s2_tbwin * s2_tbwin_cursor_parent( s2_tbwin * tw ){
  for( ; tw; tw = tw->parent ){
    if(tw->cursorX>=0) return tw;
  }
  return 0;
}
#endif


/**
   If self has a Function property named cbName, it is call()ed with
   no args and that result is returned. The call is passed no
   parameters and the Value result is ignored.
*/
static int s2_tb_callback_check( cwal_engine * e, cwal_value * self,
                                 char const * cbName ){
  cwal_value * p = cwal_prop_get(self, cbName, cwal_strlen(cbName));
  cwal_function * f;
  if(!p || !(f = cwal_value_function_part(e,p))) return 0;
  return cwal_function_call( f, self, 0, 0, 0 );
}

/**
   Keeps track of whether or not tb has been initialized.
 */
static int tbInitCalled = 0;
#define CB_TB_INIT if(!tbInitCalled){const int initRc = cb_init(args,0); \
    if(initRc) return initRc;} (void)0

/**
   Wraps tb_init().

   Returns 'this'.
*/
static int cb_init( cwal_callback_args const * args,
                    cwal_value **rv ){
  assert(args);
  if(!tbInitCalled){
    int const rc =
      tb_width()<0 /* requires termbox>=20140812 for proper
                      semantics (earlier versions have undefined
                      behaviour here). */
      ? tb_init()
      : 0 /* assume we're already in termbox screen mode */;
    if(rc) return cwal_exception_setf(args->engine, rc,
                                      "tb_init() failed with code %d.",
                                      rc);
    else tbInitCalled = 1;
    S2TbState.needsSort = 0;
  }
  if(rv/* b/c we use this internally and pass NULL*/) *rv = args->self;
  return 0;
}

/**
   Wraps tb_shutdown(). Leaves termbox screen mode and restores
   the previous terminal state.
*/
static int cb_shutdown( cwal_callback_args const * args,
                        cwal_value **rv ){
  if(tbInitCalled){
    /*
      DO NOT call tb_shutdown() unless tbInitCalled
      is true, or it may double-free() its internal
      buffers. :(

      i'm just glad it turned out not to be a cwal-level
      lifetime bug :).
    */
    if(tb_width()>0){
      if(args || rv){/*avoid unused param warning*/}
      tb_shutdown();
    }
    tbInitCalled = 0;
  }
  return 0;
}

static void cb_tbwin_fg_bg( cwal_callback_args const * args,
                          uint16_t fgIndex, uint16_t * fg,
                          uint16_t bgIndex, uint16_t * bg ){
  int f, b;
  s2_tbwin * self = s2_tbwin_from_value(args->engine, args->self);
  assert(args->argc > bgIndex && args->argc > fgIndex);
  f = args->argc>fgIndex ? tb__get_int(args->argv[fgIndex]) : 0;
  b = args->argc>bgIndex ? tb__get_int(args->argv[bgIndex]) : 0;
  *fg = f>0 ? (uint16_t)(f & 0xFFFF) : (self ? self->fg : 0);
  *bg = b>0 ? (uint16_t)(b & 0xFFFF) : (self ? self->bg : 0);
}

/**
   Wraps tb_clear(), but also implements tb_clear(uint16_t fg, uint16_t bg).
   If called with 2 arguments, the latter is used. It blanks out all cells,
   using the given foreground and background attributes.
*/
static int cb_clear( cwal_callback_args const * args,
                     cwal_value **rv ){
  if(args->argc<2){
    tb_clear();
  }else{
    uint16_t fg = 0;
    uint16_t bg = 0;
    int const w = tb_width();
    int const h = tb_height();
    int c, r = 0;
    cb_tbwin_fg_bg(args, 0, &fg, 1, &bg);
    for( ; r < h; ++r ){
      for( c = 0; c < w; ++c ){
        tb_change_cell(c, r, 0, fg, bg);
      }
    }
  }
  *rv = args->self;
  return 0;
}

/**
   Compares wl and wr by z-ordering, using memcmp() return semantics.
   NULL values compare as larger than non-NULL (sorting them to the right).
*/
static int s2_tbwin_cmp_zorder( s2_tbwin const * wl, s2_tbwin const * wr ){
  /* Sort NULLs to the right. We are not expecting duplicate non-NULL pointers. */
  if(!wl) return wr ? 1 : 0;
  else if(!wr) return -1;
  else return wl->z - wr->z;
}

/**
   bsort() compare routine expecting (s2_tbwin const **). Because it's
   called from bsort, it expects one extra level of pointer.
*/
static int s2_cmp_tbwin_zorder( void const * winL, void const * winR ){
  cwal_value const * vL = *((cwal_value const **)winL);
  cwal_value const * vR = *((cwal_value const **)winR);
  cwal_engine * e = vL ? cwal_value_engine(vL) : 0;
  if(!e && vR) e = cwal_value_engine(vR);
  if(!e){
    assert(!vL);
    assert(!vR);
    return 0;
  }else{
    s2_tbwin const * wl = s2_tbwin_from_value(e, vL);
    s2_tbwin const * wr = s2_tbwin_from_value(e, vR);
    /* Sort NULLs to the right. We are not expecting duplicate non-NULL pointers. */
    if(!wl) return wr ? 1 : 0;
    else if(!wr) return -1;
    else return wl->z - wr->z;
  }
}


/*
 * The following list sorting algo derived (edited only slightly)
 * from:
 *
 * http://www.chiark.greenend.org.uk/~sgtatham/algorithms/listsort.c
 *
 * With this license text:
 *
 * This [file] is copyright 2001 Simon Tatham.
 * 
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT.  IN NO EVENT SHALL SIMON TATHAM BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
s2_tbwin *s2_tbwin_sort_zorder(s2_tbwin *list) {
  s2_tbwin *p = 0, *q = 0, *e = 0, *tail = 0, *oldhead = 0;
  int insize = 0, nmerges = 0, psize = 0, qsize = 0, i = 0;
  const char is_circular = 0;
  const char is_double = 1;
  if (!list) return 0;
  else if(!list->right){
    /* single-entry list */
    assert(!list->left);
    return list;
  }
  insize = 1;
  while (1) {
    p = list;
	oldhead = list; /* only used for circular linkage */
    list = NULL;
    tail = NULL;
    nmerges = 0; /* count number of merges we do in this pass */
    while (p) {
      ++nmerges; /* there exists a merge to be done */
      /* step `insize' places along from p */
      q = p;
      psize = 0;
      for (i = 0; i < insize; i++) {
        ++psize;
		if (is_circular) q = (q->right == oldhead ? NULL : q->right);
		else q = q->right;
        if (!q) break;
      }
      /* if q hasn't fallen off end, we have two lists to merge */
      qsize = insize;
      /* now we have two lists; merge them */
      while (psize > 0 || (qsize > 0 && q)) {
        /* decide whether next element of merge comes from p or q */
        if (psize == 0) {
          /* p is empty; e must come from q. */
          e = q; q = q->right; qsize--;
          if (is_circular && q == oldhead) q = NULL;
		} else if (qsize == 0 || !q) {
          /* q is empty; e must come from p. */
          e = p; p = p->right; psize--;
          if (is_circular && p == oldhead) p = NULL;
		} else if (s2_tbwin_cmp_zorder(p,q) <= 0) {
          /* First element of p is lower (or same);
           * e must come from p. */
          e = p; p = p->right; psize--;
          if (is_circular && p == oldhead) p = NULL;
		} else {
          /* First element of q is lower; e must come from q. */
          e = q; q = q->right; qsize--;
          if (is_circular && q == oldhead) q = NULL;
		}
        /* add the next element to the merged list */
		if (tail) {
          tail->right = e;
		} else {
          list = e;
		}
		if (is_double) {
          /* Maintain reverse pointers in a doubly linked list. */
          e->left = tail;
		}
		tail = e;
      }
      /* now p has stepped `insize' places along, and q has too */
      p = q;
    }
	if (is_circular) {
      tail->right = list;
      if (is_double){
		list->left = tail;
      }
	} else{
      tail->right = NULL;
    }
    /* If we have done only one merge, we're finished. */
    if (nmerges <= 1){   /* allow for nmerges==0, the empty list case */
      return list;
    }

    /* Otherwise repeat, merging lists twice the size */
    insize *= 2;
  }
}

static int s2_tbwin_present_toplevel(){
  int rc = 0;
  if(S2TbState.topWindows){
    s2_tbwin * old;
    s2_tbwin * w;
    /* if(S2TbState.needsSort){ */
      S2TbState.topWindows = s2_tbwin_sort_zorder(S2TbState.topWindows);
      S2TbState.needsSort = 0;
    /* } */
    old = w = S2TbState.topWindows;
    S2TbState.topWindows = 0 /* to avoid a loop */;
    for( ; !rc && w; w = w->right ){
      rc = s2_tbwin_present(w);
    }
    S2TbState.topWindows = old;
  }
  if(!rc) tb_present();
  return rc;
}

/**
   Wraps tb_present().

   Returns this object.
 */
static int cb_present( cwal_callback_args const * args,
                     cwal_value **rv ){
  int rc;
  CB_TB_INIT;
  rc = s2_tbwin_present_toplevel();
  if(!rc) *rv = args->self;
  return rc;
}

/**
   Returns the screen width. If passed a boolean true
   and tb has not yet been initialized, it initializes
   the screen state, else it returns (as per newer tb patches),
   a negative value if tb has not been initialized.
*/
static int cb_width( cwal_callback_args const * args,
                     cwal_value **rv ){
  int rc = tb_width();
  if(rc<1 && args->argc && cwal_value_get_bool(args->argv[0])){
    CB_TB_INIT;
    rc = tb_width();
    assert(rc>0);
  }
  *rv = cwal_new_integer(args->engine, (cwal_int_t)rc);
  return *rv ? 0 : CWAL_RC_OOM;
}

/**
   Returns the screen height. If passed a boolean true
   and tb has not yet been initialized, it initializes
   the screen state, else it returns (as per newer tb patches),
   a negative value if tb has not been initialized.
*/
static int cb_height( cwal_callback_args const * args,
                        cwal_value **rv ){
  int rc = tb_height();
  if(rc<1 && args->argc && cwal_value_get_bool(args->argv[0])){
    CB_TB_INIT;
    rc = tb_height();
    assert(rc>0);
  }
  *rv = cwal_new_integer(args->engine, (cwal_int_t)rc);
  return *rv ? 0 : CWAL_RC_OOM;
}

/**
   Returns an object in the form {w: tb_width(), h: tb_height()}.
   Resturn undefined if this.init() has not been called.
*/
static int cb_size( cwal_callback_args const * args,
                    cwal_value **rv ){
  int rc = 0;
  cwal_value * v;
  cwal_value * obj;
  obj = cwal_new_object_value(args->engine);

  if(!obj) return CWAL_RC_OOM;
#define SET(K) cwal_value_ref(v);                                   \
  rc = v ? cwal_prop_set(obj, K, cwal_strlen(K), v) : CWAL_RC_OOM; \
  cwal_value_unref(v);                                              \
  if(rc) goto end

#define INT(X) cwal_new_integer(args->engine, (cwal_int_t)(X))
  v = INT(tb_width());
  SET("w");
  v = INT(tb_height());
  SET("h");

#undef SET
#undef INT
  end:
  if(rc) cwal_value_unref(obj);
  else *rv = obj;
  return rc;
}


/**
   Wraps tb_set_cursor(x,y). If x or x and y are not
   provided, they default to TB_HIDE_CURSOR.

   Returns this object.
*/
static int cb_set_cursor( cwal_callback_args const * args,
                          cwal_value **rv ){
  int x, y;
  s2_tbwin * tw = s2_tbwin_from_value(args->engine, args->self);
  x = args->argc>0
    ? tb__get_int(args->argv[0])
    : -1;
  y = args->argc>1
    ? tb__get_int(args->argv[1])
    : -1;
  if(tw) s2_tbwin_cursor(tw, x, y);
  else tb_set_cursor(x, y);
  *rv = args->self;
  return 0;
}

static int cb_clear_line( cwal_callback_args const * args,
                          cwal_value **rv ){
  cwal_int_t y = args->argc
    ? tb__get_int(args->argv[0])
    : -1;
  cwal_int_t fg = args->argc>1
    ? tb__get_int(args->argv[1])
    : -1;
  cwal_int_t bg = args->argc>2
    ? tb__get_int(args->argv[2])
    : -1;
#if 0
  *rv = tb_x_clear_line((int)y, (int)fg, (int)bg)
    ? cwal_value_false() /* 0==success */
    : cwal_value_true();
#else
  tb_x_clear_line((int)y, (int)fg, (int)bg);
  *rv = args->self;
#endif
  return 0;
}

/**
   Internal impl for cb_print5() and friends.

   Returns this object.
*/
static int cb_print_impl( cwal_callback_args const * args,
                          cwal_value **rv, uint16_t argsIndex,
                          uint16_t fg, uint16_t bg){
  int x, y, rc = 0;
  int w;
  uint16_t i;
  cwal_buffer buf = cwal_buffer_empty;
  unsigned char const * str = 0;
  assert(2==argsIndex || 4==argsIndex);
  assert(2==argsIndex ? (args->argc > 2) : (args->argc>4));
  x = tb__get_int(args->argv[0]);
  y = tb__get_int(args->argv[1]);
  for( i = argsIndex; !rc && (i < args->argc);++i ){
    rc = s2_value_to_buffer(args->engine, &buf, args->argv[i]);
  }
  if(rc) goto end;
  else if(!buf.used){
    tb_change_cell(x, y, 0 /* should this be a space? */,
                   fg, bg);
    goto end;
  }

  str = buf.mem;
  w = tb_width();
  while( str < buf.mem+buf.used ){
    uint32_t uni;
    str += tb_utf8_char_to_unicode(&uni, (char const *)str);
    tb_change_cell(x, y, uni, fg, bg);
    if(++x==w || (uint32_t)'\n' == uni){
#if PRINT_CLEAR_TO_EOL
      while(x < w){ /* clear to EOL */
        tb_change_cell(x++, y, ' ', fg, bg);
      }
#endif
      x = 0;
      ++y;
    }
  }
#if PRINT_CLEAR_TO_EOL
  while(x < w){ /* clear to EOL */
    tb_change_cell(x++, y, 0, fg, bg);
  }
#endif

  end:
  cwal_buffer_reserve(args->engine, &buf, 0);
  if(!rc) *rv = args->self;
  return rc;
}


/**
   Wraps up a series of calls to tb_change_cell(), using the
   first four parameters provided to this function as the
   (x, y, fgAttr, bgAttr) passed to tb_change_call(). The 5th
   and subsequent arguments are stringified as per s2.io.print()
   and appended to the given location. If a '\n' is generated,
   it causes the output to wrap to the next line.

   Returns this object.
*/
static int cb_print5( cwal_callback_args const * args,
                      cwal_value **rv ){
  uint16_t fg = 0, bg = 0;
  if(args->argc<5){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting (x,y,fg,bg,string...) "
                               "parameters.");
  }
  cb_tbwin_fg_bg(args, 2, &fg, 3, &bg);
  return cb_print_impl( args, rv, 4, fg, bg );
}

static int cb_print3( cwal_callback_args const * args,
                      cwal_value **rv ){
  return (args->argc<3)
    ? cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                          "Expecting (x,y,string...) "
                          "parameters.")
    : cb_print_impl( args, rv, 2, 0, 0 );
}

#if 0
/* we can't do this because all variants can take any
   max number of args. i.e. ambiguous for dispatching
   purposes.
*/
static int cb_print( cwal_callback_args const * args,
                     cwal_value **rv ){
  if(args->argc>4) return cb_print5( args, rv );
  else if(args->argc>2) return cb_print3( args, rv );
  else return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                                  "Expecting (x,y[, fg, bg],string...) "
                                  "parameters.");
}
#endif

#if 0
static int s2_tb_obj_to_cell( cwal_engine * e, cwal_value * obj,
                              struct tb_cell * tgt ){
  cwal_size_t slen = 0;
  char const * str = 0;
  uint32_t ch = (uint32_t)-1;
  cwal_value *prop = cwal_prop_get(obj,"ch",2);
  str = prop ? cwal_value_get_cstr(prop,&slen) : 0;
  if(str){
    if(!slen){
      ch = 0;
    }else{
      /* convert first char to unicode. */
      tb_utf8_char_to_unicode( &ch, str);
    }
  }else if(prop){
    ch = (uint32_t)tb__get_int(prop);
  }
  if((uint32_t)-1 != ch) tgt->ch = ch;
  tgt->fg = (uint16_t)tb__get_int(cwal_prop_get(obj,"fg",2));
  tgt->bg = (uint16_t)tb__get_int(cwal_prop_get(obj,"bg",2));
  return 0;
}
#endif

/**
   Wraps tb_change_cell(x, y, theChar, fg, bg), except that
   theChar may be either a stringable type (string/buffer)
   or an integer (default is to assume an integer). If theChar
   is a string, its first character is used.

   Returns this object.
*/
static int cb_change_cell( cwal_callback_args const * args,
                           cwal_value **rv ){
  int x, y;
  uint16_t fg = 0, bg = 0;
  cwal_size_t slen = 0;
  unsigned char const * str = 0;
  uint32_t ch = (uint32_t)-1;
  int const tbW = tb_width(), tbH = tb_height();
  if(5 != args->argc){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting (x,y,char,fg, bg) "
                               "parameters.");
  }
  x = tb__get_int(args->argv[0]);
  y = tb__get_int(args->argv[1]);
  if(x<0 || x>=tbW || y<0 || y>=tbH){
    return cwal_cb_throw(args, CWAL_RC_RANGE,
                       "Coordinates (%d,%d) are out of range of "
                       "(w=%d,h=%d).", x, y, tbW, tbH);
  }
  str = (unsigned char const *)cwal_value_get_cstr(args->argv[2], &slen);
  if(str){
    ch = slen
      ? cwal_utf8_read_char( str, str + slen, &str )
      : 0;
  }else{
    ch = (uint32_t)(tb__get_int(args->argv[2]) & 0xFFFFFFFF);
  }
  cb_tbwin_fg_bg(args, 3, &fg, 4, &bg);
  tb_change_cell(x, y, ch, fg, bg);
  *rv = args->self;
  return 0;
}

#if 0
static int s2_tb_cell_to_obj( cwal_engine * e, struct tb_cell const * c,
                              cwal_value **rv ){
  int rc = 0;
  cwal_value * obj;
  if(TbRetainAttr == c->fg){
    *rv = cwal_value_undefined();
    return 0;
  }
  obj = cwal_new_object_value(e);
  if(!obj) return CWAL_RC_OOM;
#define INT(X) cwal_new_integer(e,(cwal_int_t)(X))
#define SET(K,V) if( (V) && (rc = cwal_prop_set(obj, K, cwal_strlen(K), INT((V)))) ) goto end
  SET("ch", c->ch);
  SET("fg", c->fg);
  SET("bg", c->bg);
#undef SET
  end:
  if(rc) cwal_value_unref(obj);
  else *rv = obj;
  return rc;
}
#endif

#if 0
/* oops - a misunderstanding of the API */
static int cb_get_cell( cwal_callback_args const * args,
                        cwal_value **rv ){
  int x, y;
  struct tb_cell cell;
  memset(&cell, 0, sizeof(cell));
  cell.fg = TbRetainAttr;
  if(2 != args->argc){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting (x,y) parameters.");
  }
  x = tb__get_int(args->argv[0]);
  y = tb__get_int(args->argv[1]);
  tb_put_cell(x, y, &cell);
  return s2_tb_cell_to_obj( args->engine, &cell, rv );
}
#endif

static int s2_tb_event_to_obj( cwal_engine * e, struct tb_event const * ev,
                               cwal_value **rv ){
  int rc = 0;
  cwal_value * obj = cwal_new_object_value(e);
  cwal_value * v = 0;
  if(!obj) return CWAL_RC_OOM;
#define INT(X) cwal_new_integer(e,(cwal_int_t)(X))
#define SET(K,V) \
  v = INT(V); \
  cwal_value_ref(v); \
  rc = v ? cwal_prop_set(obj, K, cwal_strlen(K), v) : CWAL_RC_OOM; \
  cwal_value_unref(v); \
  if(rc) goto end

  /** TODO: cache/reuse these keys via s2_stash_set(). */
  switch(ev->type){
    case TB_EVENT_KEY:
      SET("mod", ev->mod);
      SET("key", ev->key);
      SET("ch", ev->ch);
      break;
    case TB_EVENT_RESIZE:
      SET("w", ev->w);
      SET("h", ev->h);
      break;
    default:
      s2_fatal(CWAL_RC_RANGE,"Unknown event type %d.", ev->type);
  }
  /* SET("type", INT(ev->type)); */
#undef INT
#undef SET
  end:
  if(rc) cwal_value_unref(obj);
  else *rv = obj;
  return rc;
}

/**
   Wraps tb_peek_event(), but takes only an integer value (a timeout
   in milliseconds). If no timeout is provided, some arbitrarily
   chosen default is used.

   Returns an event Object if an event is fired in the given
   timeframe, undefined if not, and throws on error.
*/
static int cb_peek_event( cwal_callback_args const * args,
                          cwal_value **rv ){
  int trc;
  struct tb_event ev;
  int timeout;

  timeout = args->argc>0
    ? tb__get_int(args->argv[0])
    : 100 /* arbitrary */;
  memset(&ev, 0, sizeof(ev));
  trc = tb_peek_event(&ev, timeout);
  if(!trc){
    *rv = cwal_value_undefined();
    return 0;
  }else if(trc<0){
    return cwal_exception_setf(args->engine, trc,
                               "tb_peek_event() failed for "
                               "unknown reasons.");
  }else{
    return s2_tb_event_to_obj(args->engine, &ev, rv);
  }
}

/**
   Wraps tb_poll_event(), but takes no arguments.

   It waits forever until an event is triggered and returns an event
   Object. Throws on error.
*/
static int cb_poll_event( cwal_callback_args const * args,
                          cwal_value **rv ){
  int trc;
  struct tb_event ev;
  memset(&ev, 0, sizeof(ev));
  trc = tb_poll_event(&ev);
  if(!trc){
    *rv = cwal_value_undefined();
    return 0;
  }else if(trc<0){
    return cwal_exception_setf(args->engine, trc,
                               "tb_poll_event() failed for "
                               "unknown reasons.");
  }else{
    return s2_tb_event_to_obj(args->engine, &ev, rv);
  }
}

/**
   Wraps tb_select_input_mode(). If no parameter is provided,
   TB_INPUT_CURRENT is assumed.

   Returns the input mode.
*/
int cb_select_input_mode( cwal_callback_args const * args,
                          cwal_value **rv ){
  int m;
  m = args->argc>0
    ? tb__get_int(args->argv[0])
    : TB_INPUT_CURRENT;
  m = tb_select_input_mode(m);
  *rv = cwal_new_integer(args->engine, (cwal_int_t)m);
  return *rv ? 0 : CWAL_RC_OOM;
}

/**
   Wraps tb_select_output_mode(). If no parameter is provided,
   TB_OUTPUT_CURRENT is assumed.

   Returns the output mode.
*/
int cb_select_output_mode( cwal_callback_args const * args,
                          cwal_value **rv ){
  int m;
  m = args->argc>0
    ? tb__get_int(args->argv[0])
    : TB_OUTPUT_CURRENT;
  m = tb_select_output_mode(m);
  *rv = cwal_new_integer(args->engine, (cwal_int_t)m);
  return *rv ? 0 : CWAL_RC_OOM;
}

/**
   Window.drawScrollbarV( left, top, barHeight, totalLineCount, currentLine )

   Returns this Window.
*/
static int cb_window_scrollbar_v( cwal_callback_args const * args,
                                  cwal_value **rv ){
  int x, top, barHeight, lineCount, currentLine;
  s2_tbwin * tw = args->argc
    ? s2_tbwin_from_args(args, -1)
    : 0;
  if(!tw || args->argc!=5){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting (x,y,barHeight,lineCount,currentLine) "
                               "arguments.");
  }
  x = tb__get_int(args->argv[0]);
  top = tb__get_int(args->argv[1]);
  barHeight = tb__get_int(args->argv[2]);
  lineCount = tb__get_int(args->argv[3]);
  currentLine = tb__get_int(args->argv[4]);
  if(x<0 || top<0 || barHeight<3 || lineCount < 1 || currentLine < 0){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting positive values for: "
                               "(x=%d,y=%d,barHeight=%d,lineCount=%d,"
                               "currentLine=%d).",
                               x,top,barHeight,lineCount,currentLine);
  }
  s2_tbwin_render_scrollbar_v(tw, x, top, barHeight, lineCount, currentLine);
  *rv = args->self;
  return 0;
}

/**
   Window.drawScrollbarH( left, top, barWidth, totalColumnCount, currentColumn )

   Returns this Window.
*/
static int cb_window_scrollbar_h( cwal_callback_args const * args,
                                  cwal_value **rv ){
  int top, left, barWidth, colCount, currentCol;
  s2_tbwin * tw = args->argc
    ? s2_tbwin_from_args(args, -1)
    : 0;
  if(!tw || args->argc!=5){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting (x,y,barWidth,colCount,currentCol) "
                               "arguments.");
  }
  left = tb__get_int(args->argv[0]);
  top = tb__get_int(args->argv[1]);
  barWidth = tb__get_int(args->argv[2]);
  colCount = tb__get_int(args->argv[3]);
  currentCol = tb__get_int(args->argv[4]);
  if(left<0 || top<0 || barWidth<3 || colCount < 1 || currentCol < 0){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting positive values for: "
                               "(x=%d,y=%d,barWidth=%d,colCount=%d,"
                               "currentCol=%d).",
                               left,top,barWidth,colCount,currentCol);
  }
  s2_tbwin_render_scrollbar_h(tw, left, top, barWidth, colCount, currentCol);
  *rv = args->self;
  return 0;
}

#if 0
static int s2_tbwin_in_bounds( s2_tbwin * tw, int x, int y ){
  return x>=0 && x<tw->w && y>=0 && y<tw->h;
}
#endif

void s2_tbwin_cursor( s2_tbwin * tw, int x, int y ){
  s2_tbwin * curWin = S2TbState.cursorWindow;
  assert(!s2_tbwin_is_pad(tw));
  if(x<-1) x = -1;
  if(y<-1) y = -1;
  if(curWin && curWin != tw){
    s2_tbwin_cursor(curWin, -1, -1);
  }
  tw->cursorX = x;
  tw->cursorY = y;
  if(x>=0 && y>=0){
#if 0
    /* just testing */
    if(tw->parent){
      int xAbs = 0, yAbs = 0;
      s2_tbwin_abs_pos( tw, &xAbs, &yAbs );
      x = xAbs;
      y = yAbs;
      assert(x>=0);
      assert(y>=0);
    }
#endif
    S2TbState.cursorWindow = tw;
  }else{
    S2TbState.cursorWindow = 0;
  }
  tb_set_cursor(x, y);
}

int s2_tbwin_draw_to( s2_tbwin const * src, int fromX, int fromY,
                      int w, int h,
                      s2_tbwin * dest, int toX, int toY){
  s2_tbbuf_draw_to(&src->cells, fromX, fromY,
                   w, h,
                   &dest->cells, toX, toY);
  return 0;
}


void s2_tbwin_blit( s2_tbwin * tw ){
  if(!s2_tbwin_is_pad(tw)){
    int yOff = 0, xOff = 0;
    s2_tbwin_abs_pos(tw, &xOff, &yOff);
    s2_tbbuf_blit( &tw->cells, xOff, yOff);
  }
}

static int cwal_value_visitor_f_window_present( cwal_value * key, void * state ){
  cwal_engine * e = (cwal_engine *)state;
  s2_tbwin * tw = s2_tbwin_from_value(e, key);
  return tw
    ? s2_tbwin_present( tw )
    : CWAL_RC_BREAK
    /* corner case: the CWAL_RC_BREAK behaviour (of various visitor
       routines) will hide a mis-placed 'break' call in script code
       unless we switch s2 to its own BREAK result code.
    */;
}

int s2_tbwin_present( s2_tbwin * tw ){
  cwal_engine * e = cwal_value_engine(tw->vSelf);
  int rc;
  /*
    Reminders to self: this will not DTRT in the face of overlapping
    top-level windows. To fix that, we need to see if tw is a top-level
    window. If so, run s2_tbwin_present_toplevel() instead of this one.
    But we need to do so without looping when that function leads back
    to this one.
  */
  if(TBWIN_F_HIDDEN & tw->flags) return 0;
  else if(!tw->parent){
    if(S2TbState.topWindows) return s2_tbwin_present_toplevel()
    /* Else we are being called from s2_tbwin_present_toplevel(). That
       routine temporarily moves topWindows out of the way to avoid
       this loop.
    */;
  }
  rc = s2_tb_callback_check(e,
                            tw->vSelf
                            /* ^^^ fixme? this will break if 'this' is
                               a subclass and the property is in the
                               sub part! */,
                            "onPresent");
  /*
    Oh, crap - what if onPresent() closes the window?
    That will break some assumptions about, e.g. S2TbState.topWindows.
   */
  if(rc) return rc;
  s2_tbwin_blit( tw );
  if(tw->childs && cwal_array_length_get(tw->childs)){
    if(tw->needsSort){
      cwal_size_t ndx = 0;
      cwal_array_sort( tw->childs, s2_cmp_tbwin_zorder );
      tw->needsSort = 0;
      /* Trim off any NULLs (removed entries) we just sorted to the
         right. */
      if(0==cwal_array_index_of(tw->childs, NULL, &ndx, 1)){
        cwal_array_length_set(tw->childs, ndx);
      }
    }
    rc = cwal_array_visit(tw->childs,
                          cwal_value_visitor_f_window_present,
                          e);
  }
#if 0
  /* arguable, but comfortable in script code so far! */
  if(!tw->parent){
    tb_present();
  }
#endif
  return rc;
}

void s2_tbwin_abs_pos( s2_tbwin * tw, int * _x, int * _y ){
  int x = tw->x, y = tw->y;
  while( tw && tw->parent ){
    x += tw->parent->x;
    y += tw->parent->y;
    tw = tw->parent;
  }
  *_x = x;
  *_y = y;
}

void s2_tbwin_repos( s2_tbwin * tw, int x, int y ){
  /* We need to clear the old position's cells before moving. */
  s2_tbwin_clear_from_screen(tw, 0, 0);
  tw->x = x;
  tw->y = y;
  if(S2TbState.cursorWindow){
    /* Update cursor pos on the screen. Remember that child windows do
       not see a move because their position does not change.
    */
    s2_tbwin_cursor(S2TbState.cursorWindow,
                    S2TbState.cursorWindow->cursorX,
                    S2TbState.cursorWindow->cursorY);
  }
}

void s2_tbwin_reverse_attr( s2_tbwin * tw ){
  if(tw->bg & TB_REVERSE) tw->bg &= ~TB_REVERSE;
  else tw->bg |= TB_REVERSE;
  if(tw->fg & TB_REVERSE) tw->fg &= ~TB_REVERSE;
  else tw->fg |= TB_REVERSE;
  s2_tbbuf_reverse_attr(&tw->cells);
}

void s2_tbwin_clear( s2_tbwin * tw, uint16_t fg, uint16_t bg ){
  int r, c;
  int yOff = 0, xOff = 0;
  int const tbw = tb_width(), tbh = tb_height();
  tb_cell * bc;
  char const isPad = s2_tbwin_is_pad(tw);
  s2_tbwin_abs_pos(tw, &xOff, &yOff);
  for( r = 0; r < tw->cells.h; ++r ){
    for( c = 0; c < tw->cells.w; ++c ){
      bc = s2_tbwin_cell_at(tw, c, r);
      assert(bc);
      bc->fg = fg;
      bc->bg = bg;
      bc->ch = 0;
      if(!isPad && r+yOff < tbh && c+xOff < tbw){
        /* tb_x_blit( c+xOff, r+yOff, 1, 1, bc ); */
        tb_change_cell( c+xOff, r+yOff, 0, fg, bg );
      }
    }
  }
}

void s2_tbwin_clear_from_screen( s2_tbwin * tw, uint16_t fg, uint16_t bg ){
  int r, c = 0;
  int xOff = 0, yOff = 0;
  int const tbw = tb_width(), tbh = tb_height();
  assert(tbw>0 && tbh>0);
  if(s2_tbwin_is_pad(tw)) return;
  s2_tbwin_abs_pos(tw, &xOff, &yOff);
  for( r = 0; r < tw->cells.h; ++r ){
    for( c = 0; c < tw->cells.w; ++c ){
      if(r+yOff < tbh && c+xOff < tbw){
        tb_change_cell( c+xOff, r+yOff, 0, fg, bg );
      }
    }
  }
}

/**
   Sets the attributes for the given cell. If fg/bg==-1 then those
   fields are not modified.
*/
static int s2_tbwin_change_cell( s2_tbwin * tw, int x, int y, uint32_t ch,
                                  uint16_t fg, uint16_t bg ){
  cwal_engine * e = cwal_value_engine(tw->vSelf);
  tb_cell * c = s2_tbwin_cell_at(tw, x, y);
  if(!c){
    return cwal_exception_setf(e, CWAL_RC_RANGE,
                               "Coordinates out of range: %d, %d (max: %d, %d)",
                               x, y, tw->cells.w-1,
                               tw->cells.h-1);
  }
  c->ch = ch;
  if(TbRetainAttr != fg) c->fg = fg;
  if(TbRetainAttr != bg) c->bg = bg;
  return 0;
}


static int cwal_value_visitor_f_subwindow_killer( cwal_value * v, void * state ){
  if(v){
    cwal_engine * e = (cwal_engine *)state;
    s2_tbwin * tw = s2_tbwin_from_value(e, v);
    assert(e);
    if(tw){
      tw->parent = 0 /* avoid confusion in the tw->vSelf's
                        native finalizer */;
      cwal_native_clear(cwal_value_native_part(e, tw->vSelf, &s2_tbwin_empty),
                        1);
    }
  }
  return 0;
}

static void s2_tbwin_remove_from_parent( s2_tbwin * child ){
  if(child->parent){
    if(child->parent->childs){
      cwal_size_t ndx = 0;
      int rc = cwal_array_index_of(child->parent->childs,
                                   child->vSelf, &ndx, 1);
      assert(0==rc);
      if(!rc){
        ++child->parent->needsSort;
        cwal_array_set(child->parent->childs, ndx, 0);
        child->vSelf = 0 /* that was the array entry */;
      }
    }
    child->parent = 0;
  }
}

static int s2_tbwin_insert_child( s2_tbwin * child ){
  int rc;
  cwal_array * childs;
  cwal_size_t ndx = 0;
  s2_tbwin * pcheck;
  assert(child->parent);
  assert(child->vSelf);
  for( pcheck = child->parent;
       pcheck; pcheck = pcheck->parent){
    if(child == pcheck){
      return cwal_exception_setf(cwal_value_engine(child->vSelf),
                                 CWAL_RC_CYCLES_DETECTED,
                                 "Window cannot contain itself.");
    }
  }
  childs = s2_tbwin_childs(child->parent, 1);
  if(!childs) return CWAL_RC_OOM;
  if(0==cwal_array_index_of(childs, NULL, &ndx, 1)){
    rc = cwal_array_set(childs, ndx, child->vSelf);
  }else{
    rc = cwal_array_append(childs, child->vSelf);
  }
  if(!rc) ++child->parent->needsSort;
  return rc;
}

/* Only enable for debugging and redirect stderr to a
   different console (or a file, and 'tail' it from another
   console). */
#define VERBOSE_FINALIZERS 0
/**
   cwal_finalizer_f() impl which expects v to be a (s2_tbwin*).
*/
static void s2_tbwin_finalize( cwal_engine * e, void * v ){
  s2_tbwin * tw = (s2_tbwin *)v;
#if VERBOSE_FINALIZERS
  fprintf(stderr,"Finalizing s2_tbwin@%p (%p)\n",
     (void const *)tw, (void const *)tw->vSelf);
#endif
  s2_tbwin_unlink(tw)
    /* Do this first, before the parent relationship,
       if any, is removed. */
    ;

  if(s2_tbwin_childs(tw, 0) ){
    cwal_array * childs = tw->childs;
    tw->childs = 0;
    /* iterate over them and explicitly cwal_native_clear()
       each one, to ensure that script code holding subwin
       refs gets properly invalidated. */
    cwal_array_visit( childs, cwal_value_visitor_f_subwindow_killer, e);
    cwal_array_unref(childs)
      /* That will clean up the "empty shells"
         wrapping the natives unless the client
         still has refs to them.
      */;
  }
  if(tw->parent && tw->parent->childs
     /* During parent cleanup parent->childs will
        not be there. That's okay.
        This block is actually only useful if a
        subwindow is closed before its parent.
     */){
    s2_tbwin_remove_from_parent(tw);
  }else if(!tw->parent){
    ++S2TbState.needsSort;
  }

  if(tw->cells.cells){
    s2_tbbuf_resize(e, &tw->cells, 0, 0);
    assert(!tw->cells.cells);
  }
  if(tw->buf.mem) cwal_buffer_clear(e, &tw->buf);

#if VERBOSE_FINALIZERS
  fprintf(stderr,"Finished finalizing s2_tbwin@%p (%p)\n",
          (void const *)tw, (void const *)tw->vSelf);
#endif

  *tw = s2_tbwin_empty;
  cwal_free2( e, v, sizeof(s2_tbwin) );
  if(!S2TbState.topWindows){
#if 0
    /* quick hack kludge because i'm too far removed from the code
       to understand why the next assert() is failing.

       Later: it was triggered via an exception happening in an
       onMove() handler.
    */
    S2TbState.cursorWindow = 0;
#else
    assert(!S2TbState.cursorWindow);
#endif
    cb_shutdown(0, 0);
  }
}
#undef VERBOSE_FINALIZERS

/**
   A helper function which comes in handy when script code
   subclasses our native or overrides its methods.
*/
static s2_tbwin * s2_tbwin_from_value( cwal_engine * e,
                                       cwal_value const * v ){
  s2_tbwin * rc = NULL;
  do{
    cwal_native * n = cwal_value_get_native(v);
    if(n && (rc = (s2_tbwin*)cwal_native_get(n, &s2_tbwin_empty))) break;
    else v = cwal_value_prototype_get(e, v);
  }while(v);
  return rc;
}


static s2_tbwin * s2_tbwin_alloc( cwal_engine * e ){
  s2_tbwin * tw = (s2_tbwin*)cwal_malloc2(e, sizeof(s2_tbwin));
  if(tw){
    *tw = s2_tbwin_empty;
  }
  return tw;
}

/**
   A cwal_value_rescoper_f() impl which requires that n
   be-a (s2_tbwin*). It rescopes any s2_tbwin member pointers
   which need it.
*/
static int cwal_value_rescoper_f_s2_tbwin(cwal_scope * s,
                                          cwal_value * v){
  cwal_native * n = cwal_value_get_native(v);
  s2_tbwin * tw = (s2_tbwin*)cwal_native_get(n, &s2_tbwin_empty);
  assert(tw && "This assertion found a minor cwal bug :-D");
  if(tw && s2_tbwin_childs(tw, 0)){
    cwal_array * ch = tw->childs;
    tw->childs = 0;
    cwal_value_rescope(s, cwal_array_value(ch));
    tw->childs = ch;
  }
  /* Crap: how do we ensure that the parent is also in the same
     scope without recursing? Initial attempt, temporarily setting
     tw->childs to 0, failed.

     Use case:

     var x = proc(){
       var w = new Window();
       var sub = new Window(w,...);
       return sub;
     }();

     sub needs to be able to rescope w (its parent) into the
     new scope. Hmmm.
  */
  return 0;
}

cwal_array * s2_tbwin_childs( s2_tbwin * tw,
                              char createIfNotFound){
  if(!tw->childs && createIfNotFound){
    cwal_engine * e = cwal_value_engine(tw->vSelf);
    assert(e);
    if( (tw->childs = cwal_new_array(e)) ){
      cwal_value * av = cwal_array_value(tw->childs);
      cwal_value_rescope(cwal_value_scope(tw->vSelf), av);
      cwal_value_ref(av);
    }
  }
  return tw->childs;
}

cwal_value * s2_tbwin_new_value( cwal_engine * e, s2_tbwin * parent ){
  cwal_native * n;
  s2_tbwin * tb = s2_tbwin_alloc( e );
  cwal_value * nv;
  if(!tb) return 0;
  n = cwal_new_native(e, tb, s2_tbwin_finalize, &s2_tbwin_empty);
  if(!n){
    s2_tbwin_finalize(e, tb);
    return 0;
  }
  nv = cwal_native_value(n);
  tb->vSelf = nv;
  cwal_native_set_rescoper( n, cwal_value_rescoper_f_s2_tbwin )
    /* need this for safifying certain s2_tbwin members vis-a-vis
       the scoping part of lifetime management. */
    ;
  if(parent){
    /* We need to ensure that the parent has a cwal-visible
       ref so that child windows do not get swept away from
       under it. */
    cwal_array * chlist = s2_tbwin_childs(parent, 1);
    tb->parent = parent;
    if(!chlist || s2_tbwin_insert_child(tb)){
      tb->parent = 0;
      cwal_native_clear(n, 1)
        /* also cleans up tb */;
      return 0;
    }
  }
  return tb->vSelf;
}

tb_cell * s2_tbwin_cell_at( s2_tbwin const * tw, int x, int y ){
  return s2_tbbuf_cell_at(&tw->cells, x, y);
}

static int s2_tbwin_cell_at2( s2_tbwin const * tw,
                              int x, int y, tb_cell ** tgt ){
  cwal_engine * e = cwal_value_engine(tw->vSelf);
  assert(e);
  return s2_tbbuf_cell_at2(e, &tw->cells, x, y, tgt);
}

static int s2_tbwin_resize( s2_tbwin * tw, int w, int h ){
#if 1
  cwal_engine * e = tw->vSelf
    ? cwal_value_engine(tw->vSelf)
    : 0;
  assert(e);
  return s2_tbbuf_resize(e, &tw->cells, w, h);
#else
  tb_cell * cells;
  int x, y;
  int const oldW = tw->w;
  int const oldH = tw->h;
  cwal_engine * e = cwal_value_engine(tw->vSelf);
  assert(e);
  if(w<=0 || h<=0){
    return cwal_exception_setf(e, CWAL_RC_RANGE,
                               "Both width and height must be greater "
                               "than zero.");
  }
  cells = (tb_cell*)cwal_malloc2(e, w*h*sizeof(tb_cell))
    /* Reminder the layout of the cells means a realloc
       is not suitable here, at least not if we want to retain
       any original contents.
     */;
  if(!cells) return CWAL_RC_OOM;
  s2_tbwin_clear_from_screen(tw, tw->fg, tw->bg);
  tw->w = w;
  tw->h = h;
  /* Zero-fill new cells... */
  if(!oldW){
    assert(!oldH);
    assert(!tw->cells);
    tw->cells = cells;
    memset(tw->cells, 0, w * h * sizeof(tb_cell));
  }else{
    for( x = 0; x < w; ++x ){
      for( y = 0; y < h; ++y ){
        tb_cell * c = cells + (y * w + x);
        assert(c);
        if(x < oldW || y < oldH){
          /* old cell? */
          tb_cell const * oc = s2_tbwin_cell_at(tw, x, y);
          if(oc){
            *c = *oc;
            continue;
          }
          /* else fall through */
        }
        /* new cell */
        memset(c, 0, sizeof(tb_cell));
        c->fg = tw->fg;
        c->bg = tw->bg;
      }
    }
    cwal_free2( e, tw->cells, oldW * oldH * sizeof(tb_cell) );
    tw->cells = cells;
  }
  return 0;
#endif
}

/* Helper macro for cwal_callback_f() implementations bound to
   s2_tbwin instances which ensures that the arguments->self value is
   a s2_tbwin instance... */
#define THIS_WINDOW                                                     \
  s2_tbwin * self = s2_tbwin_from_value(args->engine, args->self);    \
  if(!self) return cwal_exception_setf(args->engine, CWAL_RC_TYPE,      \
                                       "'this' is not (or is no longer) " \
                                       "a Window instance.")

/**
   Internal impl for cb_window_print3/5().
*/
static int cb_window_print( cwal_callback_args const * args,
                            cwal_value **rv, uint16_t argIndex,
                            uint16_t fg, uint16_t bg){
  int x, y, rc = 0;
  uint16_t i;
  unsigned char const * str = 0;
  tb_cell * cell;
  THIS_WINDOW;
  assert(2==argIndex || 4==argIndex);
  assert(2==argIndex ? (args->argc>2) : (args->argc>4));
  x = tb__get_int(args->argv[0]);
  y = tb__get_int(args->argv[1]);

  self->buf.used = 0;
  for( i = argIndex; !rc && (i < args->argc);++i ){
    rc = s2_value_to_buffer(args->engine, &self->buf, args->argv[i]);
  }
  if(rc) goto end;
  else if(!self->buf.used){
#if 0
    /* Nothing to write, but we'll arguably clear the cell anyway. */
    rc = s2_tbwin_change_cell(self, x, y, 0, fg, bg);
#endif
    goto end;
  }

  str = self->buf.mem;
  while( str < self->buf.mem + self->buf.used ){
    uint32_t uni;
    str += tb_utf8_char_to_unicode(&uni, (char const *)str);
    if(! (cell = s2_tbwin_cell_at(self, x, y)) ){
      if(str == self->buf.mem){ /* start of string */
        return cwal_exception_setf(args->engine, CWAL_RC_RANGE,
                                   "Coordinates out of range: %d, %d",
                                   x, y);
      }else{
        /* Just truncate it for now */
        break;
      }
    }else{
      cell->fg = fg;
      cell->bg = bg;
      cell->ch = uni;
    }
    if(++x==self->cells.w
       || (uint32_t)'\n' == uni){
#if PRINT_CLEAR_TO_EOL
      while( x < self->cells.w){ /* clear to EOL (cosmetic) */
        s2_tbwin_change_cell(self, x++, y, 0, fg, bg);
      }
#endif
      ++y;
      x = 0;
    }
  }
#if PRINT_CLEAR_TO_EOL
  while(x < self->cells.w){ /* clear to EOL */
    s2_tbwin_change_cell(self, x++, y, 0, fg, bg);
  }
#endif

  end:
  self->buf.used = 0;
  if(!rc){
    s2_tbwin_blit(self);
    *rv = args->self;
  }
  return rc;
}

/**
   The Window-level variant of cb_print5().

   Usage: (x, y, fg, bg, ... )

   Returns this object.

*/
static int cb_window_print5( cwal_callback_args const * args,
                             cwal_value **rv ){
  uint16_t fg = 0, bg = 0;
  if(args->argc<5){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting (x,y,fg,bg,value...) "
                               "parameters.");
  }
  cb_tbwin_fg_bg(args, 2, &fg, 3, &bg);
  return cb_window_print(args, rv, 4, fg, bg);
}

static int cb_window_print3( cwal_callback_args const * args,
                             cwal_value **rv ){
  THIS_WINDOW;
  if(args->argc<3){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting (x,y,value...) "
                               "parameters.");
  }
  return cb_window_print(args, rv, 2, self->fg, self->bg);
}

static int cb_window_resize( cwal_callback_args const * args,
                             cwal_value **rv ){
  int w, h, rc;
  THIS_WINDOW;
  if( args->argc<2 ){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting (width, height) arguments.");
  }
  w = tb__get_int(args->argv[0]);
  h = tb__get_int(args->argv[1]);
  if( !(rc = s2_tbwin_resize(self, w, h)) ){
    rc = s2_tb_callback_check(args->engine, args->self, "onResize");
    if(!rc){
      *rv = args->self;
    }
  }
  return rc;
}


/**
   Window Window.pos(x, y [, z])
*/
static int cb_window_repos( cwal_callback_args const * args,
                            cwal_value **rv ){
  int rc;
  THIS_WINDOW;
  assert(!s2_tbwin_is_pad(self));
  if( args->argc<2 ){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting (x, y [, z]) arguments.");
  }
  if(args->argc>2){
    int const z = tb__get_int(args->argv[2]);
    if(z != self->z){
      self->z = z;
      if(self->parent) ++self->parent->needsSort;
      else if(S2TbState.topWindows) ++S2TbState.needsSort;
    }
  }
  s2_tbwin_repos( self, tb__get_int(args->argv[0]),
                   tb__get_int(args->argv[1]) );
  rc = s2_tb_callback_check(args->engine, args->self, "onMove");
  if(!rc){
    *rv = args->self;
  }
  return rc;
}

/**
  int Window.width/height()
*/
static int cb_window_dim_impl( cwal_callback_args const * args,
                               cwal_value **rv,
                               char const * name,
                               int member ){
  if(args->argc){
    return cwal_exception_setf(args->engine,
                               CWAL_RC_MISUSE,
                               "Use resize() to change a Window's %s.",
                               name);
  }else{
    *rv = cwal_new_integer(args->engine, (cwal_int_t)member);
    return *rv ? 0 : CWAL_RC_OOM;
  }
}

/**
   int Window.width()
*/
static int cb_window_width( cwal_callback_args const * args,
                            cwal_value **rv ){
  THIS_WINDOW;
  return cb_window_dim_impl(args, rv, "width", self->cells.w);
}

/**
   int Window.height()
*/
static int cb_window_height( cwal_callback_args const * args,
                             cwal_value **rv ){
  THIS_WINDOW;
  return cb_window_dim_impl(args, rv, "height", self->cells.h);
}

/**
   Object|Window Window.size([ int w, int h ])

   If passed no args, returns an object in the form {w: width, h: height}.
   If passed any args, returns cb_window_resize().
*/
static int cb_window_size( cwal_callback_args const * args,
                           cwal_value **rv ){
  if(args->argc) return cb_window_resize(args,rv);
  else{
    int rc = 0;
    cwal_value * obj;
    cwal_value * v;
    THIS_WINDOW;
    obj = cwal_new_object_value(args->engine);
    if(!obj) return CWAL_RC_OOM;
#define SET(K,V)                                   \
    v = V;                                           \
    cwal_value_ref(v);                               \
    rc = v ? cwal_prop_set(obj, K, cwal_strlen(K), v) : CWAL_RC_OOM;    \
    cwal_value_unref(v);                             \
    if(rc) goto end
#define INT(X) cwal_new_integer(args->engine, (cwal_int_t)(X))
    SET("w",INT(self->cells.w));
    SET("h",INT(self->cells.h));

#undef SET
#undef INT
    end:
    if(rc) cwal_value_unref(obj);
    else *rv = obj;
    return rc;
  }
}


/**
   int|Window Window.posX([int])
*/
static int cb_window_posX( cwal_callback_args const * args,
                            cwal_value **rv ){
  int x;
  THIS_WINDOW;
  if(args->argc){
    if(cwal_value_is_number(args->argv[0])){
      self->x = tb__get_int(args->argv[0]);
      *rv = args->self;
      return 0;
    }
    else if(cwal_value_get_bool(args->argv[0])){
      int y = 0;
      s2_tbwin_abs_pos(self, &x, &y);
    }else{
      /* ??? */
      x = -1;
    }
  }else{
    x = self->x;
  }
  *rv = cwal_new_integer(args->engine, (cwal_int_t)x);
  return *rv ? 0 : CWAL_RC_OOM;
}


/**
   int|Window Window.posY([int])
*/
static int cb_window_posY( cwal_callback_args const * args,
                           cwal_value **rv ){
  int y;
  THIS_WINDOW;
  if(args->argc){
    if(cwal_value_is_number(args->argv[0])){
      self->y = tb__get_int(args->argv[0]);
      *rv = args->self;
      return 0;
    }
    else if(cwal_value_get_bool(args->argv[0])){
      int x = 0;
      s2_tbwin_abs_pos(self, &x, &y);
    }else{
      /* ??? */
      y = -1;
    }
  }else{
    y = self->y;
  }
  *rv = cwal_new_integer(args->engine, (cwal_int_t)y);
  return *rv ? 0 : CWAL_RC_OOM;
}


/**
   Window Window.posZ(int)
   int Window.posZ()
*/
static int cb_window_posZ( cwal_callback_args const * args,
                           cwal_value **rv ){
  THIS_WINDOW;
  if(args->argc){
    int const z = tb__get_int(args->argv[0]);
    if(z != self->z){
      self->z = z;
      if(self->parent) ++self->parent->needsSort;
      else if(S2TbState.topWindows){
        ++S2TbState.needsSort;
      }
    }
    *rv = args->self;
    return 0;
  }else{
    *rv = cwal_new_integer(args->engine, (cwal_int_t)self->z);
    return *rv ? 0 : CWAL_RC_OOM;
  }
}

/**
   Object Window.pos(bool getAbsolute)
   Object Window.pos()
   Window Window.pos(int x, int y)

   If passed no args or passed a single boolean, returns an object in
   the form {x: col, y: row, z: zLevel}. The location is relative to
   the parent window unless passed a literal true, in which case it is
   relative to the screen.

   If passed any args other than a single boolean it calls/returns
   cb_window_repos().

*/
static int cb_window_pos( cwal_callback_args const * args,
                          cwal_value **rv ){
  if(args->argc &&
     !(args->argc==1 && cwal_value_is_bool(args->argv[0]))
     /* ^^^^ if passed a single boolean, go to the 'else' block */
     ){
    return cb_window_repos(args,rv);
  }else{
    int rc = 0;
    cwal_value * obj;
    cwal_value * v;
    int xOff = 0, yOff = 0; 
    THIS_WINDOW;
    if(args->argc && cwal_value_get_bool(args->argv[0])){
      s2_tbwin_abs_pos(self, &xOff, &yOff);
    }
    obj = cwal_new_object_value(args->engine);
    if(!obj) return CWAL_RC_OOM;

#define INT(X) cwal_new_integer(args->engine,(cwal_int_t)(X))
#define SET(K,V)                                                        \
    v = INT((V));                                                       \
    cwal_value_ref(v);                                                  \
    rc = v ? cwal_prop_set(obj, K, cwal_strlen(K), v) : CWAL_RC_OOM;   \
    cwal_value_unref(v);                                                \
    if(rc) goto end
    
    SET("x",self->x+xOff);
    SET("y",self->y+yOff);
    SET("z",self->z);

#undef SET
#undef INT
    end:
    if(rc) cwal_value_unref(obj);
    else *rv = obj;
    return rc;
  }
}


static int cb_window_parent( cwal_callback_args const * args,
                            cwal_value **rv ){
  THIS_WINDOW;
  *rv = self->parent
    ? self->parent->vSelf
    : cwal_value_undefined();
  return 0;
}

static int cb_window_close( cwal_callback_args const * args,
                            cwal_value **rv ){
  cwal_native * nat;
  int rc;
  THIS_WINDOW;
  nat = cwal_value_get_native(self->vSelf);
  assert(nat);
  s2_tbwin_clear_from_screen( self, 0, 0 );
  rc = s2_tb_callback_check(args->engine, args->self, "onClose");
  cwal_native_clear(nat, 1);
  if(!rc) *rv = cwal_value_undefined();
  return rc;
}

/**
   Script uage: clear([int fg[, int bg]]). It blanks out all cells,
   using the given foreground and background attributes.
*/
static int cb_window_clear( cwal_callback_args const * args,
                            cwal_value **rv ){
  uint16_t fg = 0, bg = 0;
  THIS_WINDOW;
  if(1==args->argc){
    cb_tbwin_fg_bg(args, 0, &bg, 0, &fg /* intentionally swapped */);
    bg = self->bg;
  }else if(args->argc>1){
    cb_tbwin_fg_bg(args, 0, &fg, 1, &bg);
  }else{
    fg = self->fg;
    bg = self->bg;
  }
  s2_tbwin_clear( self, fg, bg );
  *rv = args->self;
  return 0;
}

static int cb_window_reverse_attr( cwal_callback_args const * args,
                                   cwal_value **rv ){
  THIS_WINDOW;
  s2_tbwin_reverse_attr(self);
  *rv = args->self;
  return 0;
}

/**
   Script uage: defaultAttr(int fg, int bg). Gets or sets the default
   attributes (used by only a few functions) and returns 'this'.
*/
static int cb_window_attr( cwal_callback_args const * args,
                               cwal_value **rv ){
  THIS_WINDOW;
  if(args->argc){
    /* Setter */
    self->fg = (uint16_t)tb__get_int(args->argv[0]);
    if(args->argc>1){
      self->bg = (uint16_t)tb__get_int(args->argv[1]);
    }
    *rv = args->self;
    return 0;
  }else{
    /* Getter: return {fg: FG, bg: BG}

       TODO? return an array of [fg, bg] instead? It'd be cheaper.
    */
    cwal_value * obj = cwal_new_object_value(args->engine);
    int rc = obj ? 0 : CWAL_RC_OOM;
    if(obj){
      cwal_value * v = cwal_new_integer(args->engine,(cwal_int_t)self->fg);
      if(!v) rc = CWAL_RC_OOM;
      else{
        if((rc = cwal_prop_set(obj, "fg", 2, v))){
          cwal_value_unref(v);
        }else if(!(v = cwal_new_integer(args->engine,(cwal_int_t)self->bg))){
          rc = CWAL_RC_OOM;
        }else if((rc = cwal_prop_set(obj, "bg", 2, v))){
          cwal_value_unref(v);
        }
      }
    }
    if(rc) cwal_value_unref(obj);
    else *rv = obj;
    return rc;
  }
}


static int cb_window_present( cwal_callback_args const * args,
                              cwal_value **rv ){
  int rc;
  THIS_WINDOW;
  rc = s2_tbwin_present(self);
  if(!rc){
    /* tb_present(); */
    *rv = args->self;
  }
  return rc;
}

static int cb_window_show( cwal_callback_args const * args,
                           cwal_value **rv ){
  THIS_WINDOW;
  if(args->argc){
    s2_tbwin_show( self, cwal_value_get_bool(args->argv[0]) ? 1 : 0 );
  }else{
    s2_tbwin_show( self, 1 );
  }
  *rv = args->self;
  return 0;
}

static int cb_window_hide( cwal_callback_args const * args,
                           cwal_value **rv ){
  THIS_WINDOW;
  if(args->argc){
    s2_tbwin_show( self, cwal_value_get_bool(args->argv[0]) ? 0 : 1 );
  }else{
    s2_tbwin_show( self, 0 );
  }
  *rv = args->self;
  return 0;
}

static int cb_window_toggle( cwal_callback_args const * args,
                           cwal_value **rv ){
  THIS_WINDOW;
  s2_tbwin_show( self, (self->flags & TBWIN_F_HIDDEN) ? 1 : 0 );
  *rv = args->self;
  return 0;
}


/**
   Usage: this.thisFunc(x, y, theChar[, fg, bg]).
   theChar may be either a stringable type (string/buffer)
   or an integer (default is to assume an integer). If theChar
   is a string, its first character is used.

   Returns this object.
*/
static int cb_window_change_cell( cwal_callback_args const * args,
                                  cwal_value **rv ){
  int rc, x, y;
  cwal_size_t slen = 0;
  unsigned char const * str = 0;
  tb_cell * cell;
  THIS_WINDOW;
  if(args->argc<3){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting (x,y,char [,fg, bg]) "
                               "parameters.");
  }
  x = tb__get_int(args->argv[0]);
  y = tb__get_int(args->argv[1]);
  if( (rc = s2_tbwin_cell_at2(self, x, y, &cell)) ){
    return rc;
  }
  str = (unsigned char const *)cwal_value_get_cstr(args->argv[2], &slen);
  if(str){
    cell->ch = slen
      ? cwal_utf8_read_char( str, str + slen, &str )
      : 0;
  }else{
    cell->ch = (uint32_t)tb__get_int(args->argv[2]);
  }
  if(args->argc>4){
    cell->fg = (uint16_t)tb__get_int(args->argv[3]);
    cell->bg = (uint16_t)tb__get_int(args->argv[4]);
  }
  *rv = args->self;
  return 0;
}

static int cb_window_draw_to( cwal_callback_args const * args,
                              cwal_value **rv ){
  int fromX, fromY, fromW, fromH, toX, toY, rc;
  s2_tbwin * wDest;
  THIS_WINDOW;
  if( !args->argc ){
    misuse:
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting (Window [,fromX, fromY, toX, "
                               "toY, fromWidth, fromHeight]) arguments.");
  }
  wDest = s2_tbwin_from_value(args->engine, args->argv[0]);
  if(!wDest) goto misuse;
  fromX = (args->argc>1) ? tb__get_int(args->argv[1]) : 0;
  fromY = (args->argc>2) ? tb__get_int(args->argv[2]) : 0;
  toX = (args->argc>3) ? tb__get_int(args->argv[3]) : 0;
  toY = (args->argc>4) ? tb__get_int(args->argv[4]) : 0;
  fromW = (args->argc>5) ? tb__get_int(args->argv[5]) : self->cells.w;
  fromH = (args->argc>6) ? tb__get_int(args->argv[6]) : self->cells.h;
  rc = s2_tbwin_draw_to( self, fromX, fromY, fromW, fromH, wDest, toX, toY );
  if(!rc) *rv = args->self;
  return rc;
}

static int s2_tbwin_line( s2_tbwin * tw, int fromX, int fromY, int toX, int toY,
                          uint32_t lineChar, uint16_t fg, uint16_t bg ){
  int rc = 0, c, r, tmp;
#if 0
  char buf[10] = {0};
  buf[tb_utf8_unicode_to_char( buf, lineChar )] = 0;
  fprintf(stderr, "Line char: %s\n", buf);
#endif
  /* lineChar = 0x2500U; */
  if(fromX>toX){
    tmp = fromX;
    fromX = toX;
    toX = tmp;
  }
  if(fromY>toY){
    tmp = fromY;
    fromY = toY;
    toY = tmp;
  }
  for( r = fromY; !rc && r <= toY; ++r ){
    for( c = fromX; !rc && c <= toX; ++c ){
      /* fprintf(stderr,"r=%d c=%d\n", r, c); */
      rc = s2_tbwin_change_cell(tw, c, r, lineChar, fg, bg);
    }
  }
  return rc;
}


int s2_tbwin_render_scrollbar_v( s2_tbwin * tw, int x, int top,
                                 int barHeight,
                                 int lineCount, int currentLine ){
#if 1
  s2_tbbuf_render_scrollbar_v( &tw->cells, x, top, barHeight, lineCount, currentLine );
  return 0;
#else  
  int y, bottom = top + barHeight - 1,
    dotPos = top + (int)((double)currentLine / lineCount * barHeight) +1;
  struct _ScrollbarConfig const * const conf = &ScrollbarConfig;
  s2_tbwin_change_cell( tw, x, top, conf->vTop, TbRetainAttr, TbRetainAttr);
  for(y = top+1; y < bottom; ++y ){
    s2_tbwin_change_cell( tw, x, y, conf->filler, TbRetainAttr, TbRetainAttr);
  }
  s2_tbwin_change_cell( tw, x, dotPos>=bottom?bottom-1:dotPos,
                        conf->curPos, TbRetainAttr, TbRetainAttr);
  s2_tbwin_change_cell( tw, x, bottom, conf->vBottom,
                        TbRetainAttr, TbRetainAttr);
  return 0;
#endif
}

int s2_tbwin_render_scrollbar_h( s2_tbwin * tw, int left, int top,
                                 int barWidth,
                                 int colCount, int currentCol ){
#if 1
  s2_tbbuf_render_scrollbar_h( &tw->cells, left, top, barWidth, colCount, currentCol );
  return 0;
#else  
  int x, right = left + barWidth - 1,
    dotPos = left + (int)((double)currentCol / colCount * barWidth) +1;
  struct _ScrollbarConfig const * const conf = &ScrollbarConfig;
  s2_tbwin_change_cell( tw, left, top, conf->hLeft, TbRetainAttr, TbRetainAttr);
  for(x = left+1; x < right; ++x ){
    s2_tbwin_change_cell( tw, x, top, conf->filler, TbRetainAttr, TbRetainAttr);
  }
  s2_tbwin_change_cell( tw, dotPos>=x?x-1:dotPos, top,
                        conf->curPos, TbRetainAttr, TbRetainAttr);
  s2_tbwin_change_cell( tw, x, top, conf->hRight,
                        TbRetainAttr, TbRetainAttr);
  return 0;
#endif
}

static int cb_window_clear_line( cwal_callback_args const * args, cwal_value **rv ){
  int line, rc = 0;
  uint16_t fg, bg;
  THIS_WINDOW;
  if( !args->argc ){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting a line number argument.");
  }
  line = tb__get_int(args->argv[0]);
  fg = args->argc>1 ? tb__get_u16(args->argv[1], self->fg) : self->fg;
  bg = args->argc>2 ? tb__get_u16(args->argv[2], self->bg) : self->bg;
  rc = s2_tbwin_line( self, 0, line, self->cells.w-1, line,
                      (uint32_t)' ', fg, bg );
  if(!rc) *rv = args->self;
  return rc;
}

static int cb_window_line_impl( cwal_callback_args const * args,
                                cwal_value **rv,
                                char isHoriz,
                                uint32_t lineChar){
  int fromX, fromY, toX, toY, rc = 0;
  uint16_t fg, bg;
  THIS_WINDOW;
  if( args->argc<3 ){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting (fromX, fromY, "
                               "%s [, fg, bg]) arguments.",
                               isHoriz ? "toY" : "toX");
  }
  fromX = tb__get_int(args->argv[0]);
  fromY = tb__get_int(args->argv[1]);
  if(isHoriz){
    toX = tb__get_int(args->argv[2]);
    toY = fromY;
  }else{
    toX = fromX;
    toY = tb__get_int(args->argv[2]);
  }
  fg = args->argc>3 ? tb__get_u16(args->argv[3], self->fg) : self->fg;
  bg = args->argc>4 ? tb__get_u16(args->argv[4], self->bg) : self->bg;
  rc = s2_tbwin_line( self, fromX, fromY, toX, toY, lineChar, fg, bg );
  if(!rc) *rv = args->self;
  return rc;
}

static int cb_window_vline( cwal_callback_args const * args,
                            cwal_value **rv ){
  return cb_window_line_impl(args, rv, 0, 0x2502);
}

static int cb_window_hline( cwal_callback_args const * args,
                            cwal_value **rv ){
  return cb_window_line_impl(args, rv, 1, 0x2500U);
}
/**
   Usage: window.boxIn([topBottomLineWidth=1 [,sideLineWidth = topBottomLineWidth
   [, fg [, bg]]]])

   The line widths must be 0, 1, or 2 (defaulting 0 if it doesn't know what to do).
   A width of 0 means to use spaces.

   It sets up the corner characters based on the line widths of the
   adjoining sides.

   It re-uses whatever attributes the cells have.
*/
static int cb_window_box_in( cwal_callback_args const * args,
                              cwal_value **rv ){
  uint16_t fg, bg;
  int wHoriz = 1, wVert;
  uint32_t hChar = 0x20, vChar = 0x20,
    topLeft = 0x20, topRight = 0x20, bLeft = 0x20, bRight = 0x20; 
  THIS_WINDOW;
#define NUM(N,VAR) if(args->argc>(N)) VAR = tb__get_int(args->argv[N])
  NUM(0,wHoriz);
  NUM(1,wVert);
  else wVert = wHoriz;
#undef NUM
  cb_fg_bg(args, 2, &fg, 3, &bg);

  if(2==wHoriz){ /* double line */
    hChar = 0x2550;
    if(2==wVert){
      vChar = 0x2551;
      topLeft = 0x2554;
      topRight = 0x2557;
      bLeft = 0x255A;
      bRight = 0x255D;
    }else{
      if(1==wVert) vChar = 0x2502;
      topLeft = 0x2552;
      topRight = 0x2555;
      bLeft = 0x2558;
      bRight = 0x255B;
    }
  }else{
    if(1==wHoriz) hChar = 0x2500 /* single line */;
    if(2==wVert){
      vChar = 0x2551;
      topLeft = 0x2553;
      topRight = 0x2556;
      bLeft = 0x2559;
      bRight = 0x255C;
    }else{
      if(1==wVert) vChar = 0x2502;
      topRight = 0x2510;
      topLeft = 0x250C;
      bLeft = 0x2514;
      bRight = 0x2518;
    }
  }
  if(wVert<=0 && wHoriz<=0){
    hChar = vChar = topLeft = topRight = bLeft = bRight = 0;
  }

  /* Corners... */
  s2_tbwin_change_cell( self, 0, 0, topLeft, fg, bg);
  s2_tbwin_change_cell( self, self->cells.w-1, 0, topRight, fg, bg);
  s2_tbwin_change_cell( self, self->cells.w-1, self->cells.h-1, bRight,
                        fg, bg);
  s2_tbwin_change_cell( self, 0, self->cells.h-1, bLeft, fg, bg);

  /* Sides... */
  s2_tbwin_line( self, 1, 0, self->cells.w-2, 0, hChar, fg, bg);
  s2_tbwin_line( self, 1, self->cells.h-1, self->cells.w-2,
                 self->cells.h-1, hChar, fg, bg);

  /* Top/bottom... */
  s2_tbwin_line( self, 0, 1, 0, self->cells.h-2, vChar, fg, bg);
  s2_tbwin_line( self, self->cells.w-1, 1, self->cells.w-1,
                 self->cells.h-2, vChar, fg, bg);

  *rv = args->self;
  return 0;
}

static int cb_window_box_in2( cwal_callback_args const * args,
                              cwal_value **rv ){
  uint16_t fg, bg;
  uint32_t hChar = 0x2500, vChar = 0x2502,
    topLeft = 0x250C, topRight = 0x2510, bLeft = 0x2514, bRight = 0x2518; 
  THIS_WINDOW;

  if(args->argc>0) hChar = (uint32_t)tb__get_int(args->argv[0]);
  if(args->argc>1) vChar = (uint32_t)tb__get_int(args->argv[1]);
  cb_fg_bg(args, 2, &fg, 3, &bg);
  
  switch(hChar){
    case 0:
      topLeft = topRight = bLeft = bRight = (uint32_t)' ';
      break;
    case 0x2D /* - */:
      topLeft = topRight = bLeft = bRight = (uint32_t)'+';
      break;
    case 0x2550: /* double-line */
      if(0x2551==vChar){
        topLeft = 0x2554;
        topRight = 0x2557;
        bLeft = 0x255A;
        bRight = 0x255D;
      }else{
        topLeft = 0x2552;
        topRight = 0x2555;
        bLeft = 0x2558;
        bRight = 0x255B;
      }
      break;
  }

  switch(vChar){
#if 1
    case 0x20 /* space */:
      if(vChar == hChar){
        topLeft = topRight = bLeft = bRight = vChar;
      }
      break;
#endif
    case 0x7C /* | */:
      topLeft = topRight = bLeft = bRight = (uint32_t)'+';
      break;
    case 0x2551: /* double-line */
      if(0x2550==hChar){/* double line */
        topLeft = 0x2554;
        topRight = 0x2557;
        bLeft = 0x255A;
        bRight = 0x255D;
      }else{
        topLeft = 0x2553;
        topRight = 0x2556;
        bLeft = 0x2559;
        bRight = 0x255C;
      }
      break;
  }

  /* Corners... */
  s2_tbwin_change_cell( self, 0, 0, topLeft, fg, bg);
  s2_tbwin_change_cell( self, self->cells.w-1, 0,
                        topRight, fg, bg);
  s2_tbwin_change_cell( self, self->cells.w-1, self->cells.h-1,
                        bRight, fg, bg);
  s2_tbwin_change_cell( self, 0, self->cells.h-1, bLeft, fg, bg);

  /* Sides... */
  s2_tbwin_line( self, 1, 0, self->cells.w-2, 0, hChar, fg, bg);
  s2_tbwin_line( self, 1, self->cells.h-1, self->cells.w-2,
                 self->cells.h-1, hChar, fg, bg);

  /* Top/bottom... */
  s2_tbwin_line( self, 0, 1, 0, self->cells.h-2, vChar, fg, bg);
  s2_tbwin_line( self, self->cells.w-1, 1, self->cells.w-1,
                 self->cells.h-2, vChar, fg, bg);

  *rv = args->self;
  return 0;
}


static int cb_new_subwindow( cwal_callback_args const * args,
                             cwal_value **rv ){
  int rc, w = -1, h = -1, x = -1, y = -1, z = 0;
  cwal_value * vw;
  s2_tbwin * tw;
  s2_tbwin * parent = 0;
  cwal_value * prototype = tb_prototype_window(args->engine);
  int xyoff = 0;
  s2_tbwin * self = s2_tbwin_from_value(args->engine, args->self);

  /*
    Args:

    new Window(Window parent [, w=parent.width-2, h=parent.height-2 [, x=1, y=1]])

    parent.newSubwin(Window parent [, w=parent.width-2, h=parent.height-2 [, x=1, y=1]])
  */
  if(!prototype) return CWAL_RC_OOM;
  else if(args->argc && (parent = s2_tbwin_from_value(args->engine, args->argv[0]))){
    ++xyoff;
  }
  else parent = self;
  if(args->argc>xyoff+1){
    w = tb__get_int(args->argv[xyoff]);
    h = tb__get_int(args->argv[xyoff+1]);
  }else{
    w = parent->cells.w - 2;
    h = parent->cells.h - 2;
    x = y = 1;
  }

  if(w<=0 || h<=0){
    return cwal_exception_setf(args->engine, CWAL_RC_RANGE,
                               "Both width and height must be greater "
                               "than zero.");
  }
  if(x<0) x = args->argc>xyoff+2
            ? tb__get_int(args->argv[xyoff+2])
            : 0;
  if(y<0) y = args->argc>xyoff+3
            ? tb__get_int(args->argv[xyoff+3])
            : 0;
  if(args->argc>xyoff+4){
    z = tb__get_int(args->argv[xyoff+4]);
  }

  vw = s2_tbwin_new_value( args->engine, parent );
  if(!vw){
    return cwal_exception_get(args->engine)
      ? CWAL_RC_EXCEPTION
      : CWAL_RC_OOM;
  }
  tw = s2_tbwin_from_value(args->engine, vw);
  assert(tw);
  assert(tw->vSelf == vw);
  assert(parent == tw->parent);
  tw->x = x;
  tw->y = y;
  tw->z = z;
  assert(parent->childs && "But this was just allocated!");
  assert(cwal_value_refcount(vw) && "parent.children entry?");
  if( (rc = s2_tbwin_resize(tw, w, h)) ){
    s2_tbwin_remove_from_parent(tw)
      /* There goes the last reference to vw a.k.a. tw->vSelf */;
    return rc;
  }
  cwal_value_prototype_set(vw, prototype);
  *rv = vw;
  return 0;
}

static int cb_new_window( cwal_callback_args const * args,
                          cwal_value **rv );

cwal_value * tb_prototype_window( cwal_engine * e){
  int rc = 0;
  cwal_value * v;
  cwal_value * superClass = 0;
  s2_engine * se = s2_engine_from_state(e);
  char const * stashKey = "TbWindow";
  /* cwal_size_t const keyLen = cwal_strlen(stashKey); */
  cwal_value * proto;

  if(!(superClass=tb_prototype_pad(e))) return 0;
  proto = s2_stash_get(se, stashKey)
    /*
      Reminder: we have to store the prototype somewhere vacuum-safe,
      and we don't have a way to directly get to it from
      callback. So we implicitely make its lifetime global
      by stuffing it in the s2 stash.
    */;
  if(proto) return proto;
  assert(se);
  proto = cwal_new_object_value(se->e);
  if(!proto){
    rc = CWAL_RC_OOM;
    goto end;
  }
#if 0
  rc = cwal_prop_set_with_flags(container, stashKey, keyLen, proto,
                                CWAL_VAR_F_CONST | CWAL_VAR_F_HIDDEN);
#else
  rc = s2_stash_set(se, stashKey, proto);
#endif
  if( rc ){
    cwal_value_unref(proto);
    proto = 0;
    goto end;
  }

  cwal_value_prototype_set( proto, superClass );

#define CHECKV if(!v){ rc = CWAL_RC_OOM; goto end; } (void)0
#define RC if(rc) goto end;
#define SETV(NAME)                                            \
  CHECKV;                                                     \
  cwal_value_ref(v);                                          \
  rc = cwal_prop_set( proto, NAME, cwal_strlen(NAME), v );   \
  cwal_value_unref(v);                                        \
  v = 0;                                                      \
  if(rc) goto end

  v = cwal_new_string_value(e, "Window", 6);
  SETV("__typename");

  {
    s2_func_def const funcs[] = {
  /* S2_FUNC2("changeCell", cb_window_change_cell), */
  /* S2_FUNC2("clear", cb_window_clear), */
  /* S2_FUNC2("close", cb_window_close), */
    S2_FUNC2("cursor", cb_set_cursor),
  /* S2_FUNC2("height", cb_window_height), */
    S2_FUNC2("newSubwin", cb_new_subwindow),
    S2_FUNC2("parent", cb_window_parent),
    S2_FUNC2("pos", cb_window_pos),
    S2_FUNC2("posX", cb_window_posX),
    S2_FUNC2("posY", cb_window_posY),
    S2_FUNC2("posZ", cb_window_posZ),
    S2_FUNC2("present", cb_window_present),
    S2_FUNC2("show", cb_window_show),
    S2_FUNC2("hide", cb_window_hide),
    S2_FUNC2("toggle", cb_window_toggle),
  /* S2_FUNC2("print5", cb_window_print), */
  /* S2_FUNC2("size", cb_window_size), */
  /* S2_FUNC2("width", cb_window_width), */
    s2_func_def_empty_m
    };
    rc = s2_install_functions(se, proto, funcs, 0);
    if(rc) goto end;
    rc = s2_ctor_callback_set(se, proto, cb_new_window);
    if(rc) goto end;
  }
#undef SETV
#undef CHECKV
#undef RC
  end:
  return rc ? NULL : proto;
}

static int cb_new_pad( cwal_callback_args const * args, cwal_value **rv );

cwal_value * tb_prototype_pad( cwal_engine * e){
  int rc = 0;
  cwal_value * v;
  s2_engine * se = s2_engine_from_state(e);
  char const * stashKey = "TbPad";
  cwal_value * proto;
  proto = s2_stash_get(se, stashKey);
  if(proto) return proto;
  assert(se);
  proto = cwal_new_object_value(se->e);
  if(!proto){
    rc = CWAL_RC_OOM;
    goto end;
  }
#if 0
  rc = cwal_prop_set_with_flags(container, stashKey, keyLen, proto,
                                CWAL_VAR_F_CONST | CWAL_VAR_F_HIDDEN);
#else
  rc = s2_stash_set( se, stashKey, proto );
#endif
  if( rc ){
    cwal_value_unref(proto);
    proto = 0;
    goto end;
  }

#define CHECKV if(!v){ rc = CWAL_RC_OOM; goto end; } (void)0
#define RC if(rc) goto end;
#define SETV(NAME)                                              \
  CHECKV;                                                       \
  cwal_value_ref(v);                                            \
  rc = cwal_prop_set( proto, NAME, cwal_strlen(NAME), v );     \
  cwal_value_unref(v);                                          \
  v = 0;                                                        \
  if(rc) goto end

  v = cwal_new_string_value(e, "TBPad", 5);
  SETV("__typename");

  {
    s2_func_def const funcs[] = {
    S2_FUNC2("boxIn", cb_window_box_in),
    S2_FUNC2("boxIn2", cb_window_box_in2),
    S2_FUNC2("changeCell", cb_window_change_cell),
    S2_FUNC2("clear", cb_window_clear),
    S2_FUNC2("clearLine", cb_window_clear_line),
    S2_FUNC2("close", cb_window_close),
    S2_FUNC2("defaultAttr", cb_window_attr),
    S2_FUNC2("drawScrollbarV", cb_window_scrollbar_v),
    S2_FUNC2("drawScrollbarH", cb_window_scrollbar_h),
    S2_FUNC2("drawTo", cb_window_draw_to),
    S2_FUNC2("height", cb_window_height),
    S2_FUNC2("hLine", cb_window_hline),
    S2_FUNC2("size", cb_window_size),
    S2_FUNC2("width", cb_window_width),
    S2_FUNC2("print5", cb_window_print5),
    S2_FUNC2("print3", cb_window_print3),
    S2_FUNC2("reverseAttr", cb_window_reverse_attr),
    S2_FUNC2("vLine", cb_window_vline),
    s2_func_def_empty_m
    };
    /*
      TODO: copyTo(x, y, w, h, Window, winX, winY)
    */
    rc = s2_install_functions(se, proto, funcs, 0);
    if(rc) goto end;
    rc = s2_ctor_callback_set(se, proto, cb_new_pad);
    if(rc) goto end;
  }

#undef SETV
#undef CHECKV
#undef RC
  end:
  return rc ? NULL : proto;
}

int cb_new_window( cwal_callback_args const * args, cwal_value **rv ){
  /**
     TODO: consolidate this with newSubwin(), now that s2 has a
     proper ctor mechanism.
  */
  int rc, w = -1, h = -1, x = -1, y = -1, z = 0;
  cwal_value * vw;
  s2_tbwin * tw, * parent = 0;
  cwal_value * prototype;
  CB_TB_INIT;
  
  if(args->argc && s2_tbwin_from_value(args->engine, args->argv[0])){
    return cb_new_subwindow(args, rv);
  }

  prototype = tb_prototype_window(args->engine);
  if(!prototype) return CWAL_RC_OOM;
  if(args->argc>1){
    w = tb__get_int(args->argv[0]);
    h = tb__get_int(args->argv[1]);
  }else{
    w = tb_width();
    h = tb_height();
  }

  if(w<=0 || h<=0){
    return cwal_exception_setf(args->engine, CWAL_RC_RANGE,
                               "Both width and height must be greater "
                               "than zero.");
  }
  if(x<0) x = args->argc>2
            ? tb__get_int(args->argv[2])
            : 0;
  if(y<0) y = args->argc>3
            ? tb__get_int(args->argv[3])
            : 0;
  z = args->argc>4
    ? tb__get_int(args->argv[4])
    : 0;

  vw = s2_tbwin_new_value( args->engine, parent );
  if(!vw){
    return cwal_exception_get(args->engine)
      ? CWAL_RC_EXCEPTION
      : CWAL_RC_OOM;
  }
  tw = s2_tbwin_from_value(args->engine, vw);
  assert(tw);
  assert(tw->vSelf == vw);
  tw->x = x;
  tw->y = y;
  tw->z = z;
  if(parent){
    assert(tw->parent == parent);
    assert(cwal_value_refcount(vw) && "parent.children entry?");
  }else{
    s2_tbwin_link(tw);
  }
  if( (rc = s2_tbwin_resize(tw, w, h)) ){
    if(parent){
      assert(parent->childs && "But this was just allocated!");
      s2_tbwin_remove_from_parent(tw)
        /* There goes the last reference to tw/vw */;
    }else{
      cwal_value_unref(vw);
    }
    return rc;
  }
  cwal_value_prototype_set(vw, prototype);
  *rv = vw;
  return 0;
}

int cb_new_pad( cwal_callback_args const * args, cwal_value **rv ){
  int rc, w = -1, h = -1;
  cwal_value * vw;
  s2_tbwin * tw;
  cwal_value * prototype = tb_prototype_pad(args->engine);
  if(!prototype) return CWAL_RC_OOM;
  if(args->argc>1){
    w = tb__get_int(args->argv[0]);
    h = tb__get_int(args->argv[1]);
  }
  if(w<=0 || h<=0){
    return cwal_exception_setf(args->engine, CWAL_RC_RANGE,
                               "Both width and height must be greater "
                               "than zero.");
  }
  vw = s2_tbwin_new_value( args->engine, 0 );
  if(!vw) return CWAL_RC_OOM;
  tw = s2_tbwin_from_value(args->engine, vw);
  assert(tw);
  assert(tw->vSelf == vw);
  tw->flags |= TBWIN_F_ISPAD;
  tw->cursorX = tw->cursorY = 0;
  if( (rc = s2_tbwin_resize(tw, w, h)) ){
    cwal_value_unref(vw);
    return rc;
  }else{
    cwal_value_prototype_set(vw, prototype);
    *rv = vw;
    return 0;
  }
}

/**
   Returns an Object containing the TB_OUTPUT_XXX mappings. The key names
   are the lower-case (or integer!) XXX part of the TB_OUTPUT_XXX entries.

   If dest is not 0, it is used and returned instead of a new object.
*/
static cwal_value * s2_tb_get_output_modes(cwal_engine *e, cwal_value * dest){
  cwal_value * v = dest ? dest : cwal_new_object_value(e);
#define SET(K,V) cwal_prop_set(v, #K, cwal_strlen(#K), cwal_new_integer(e,(cwal_int_t)(V)))
  SET(current,TB_OUTPUT_CURRENT);
  SET(normal, TB_OUTPUT_NORMAL);
  SET(256, TB_OUTPUT_256);
  SET(216, TB_OUTPUT_216);
  SET(grayscale, TB_OUTPUT_GRAYSCALE);
#undef SET
  return v;
}

/**
   Returns an Object containing the TB_INPUT_XXX mappings. The key names
   are the lower-case (or integer!) XXX part of the TB_INPUT_XXX entries.

   If dest is not 0, it is used and returned instead of a new object.
*/
static cwal_value * s2_tb_get_input_modes(cwal_engine *e, cwal_value * dest){
  cwal_value * v = dest ? dest : cwal_new_object_value(e);
#define SET(K,V) cwal_prop_set(v, #K, cwal_strlen(#K), cwal_new_integer(e,(cwal_int_t)(V)))
  SET(current, TB_INPUT_CURRENT);
  SET(esc, TB_INPUT_ESC);
  SET(alt, TB_INPUT_ALT);
#undef SET
  return v;
}

/**
   Returns an Object containing the TB_XXX color mappings. The key names
   are the lower-case XXX part of the TB_XXX entries.

   If dest is not 0, it is used and returned instead of a new object.
*/
#if 1
static cwal_value * s2_tb_get_colors(s2_engine *se){
  cwal_value * n;
  cwal_value * v = cwal_new_object_value(se->e);
  int rc;
  if(!v) return NULL;
  cwal_value_ref(v);
#define SET(K,V) n = cwal_new_integer(se->e,(cwal_int_t)V); \
  if(!n) goto oom;                                          \
  cwal_value_ref(n);                                        \
  rc = cwal_prop_set(v, K, cwal_strlen(K), n);              \
  cwal_value_unref(n);                                      \
  if(rc) goto oom
  SET("default",TB_DEFAULT);
  SET("black",TB_BLACK);
  SET("red",TB_RED);
  SET("green",TB_GREEN);
  SET("yellow",TB_YELLOW);
  SET("blue",TB_BLUE);
  SET("magenta",TB_MAGENTA);
  SET("cyan",TB_CYAN);
  SET("white",TB_WHITE);
#undef SET
  cwal_value_unhand(v);
  return v;
  oom:
  cwal_value_unref(v);
  return NULL;
}
#else
static int s2_tb_get_colors(s2_engine *se, cwal_value ** dest){
  s2_enum_builder eb = s2_enum_builder_empty;
  int rc;
  cwal_value * v = 0;
  rc = s2_enum_builder_init( se, &eb, 0, 0);
  if(rc) return rc;
#define SET(K,V)                             \
  v = cwal_new_integer(se->e,(cwal_int_t)V); \
  if(!v) { rc = CWAL_RC_OOM; goto end; }     \
  cwal_value_ref(v);                         \
  rc = s2_enum_builder_append(&eb, K, v);    \
  cwal_value_unref(v);                       \
  v = 0;                                     \
  if(rc) goto end
  SET("default",TB_DEFAULT);
  SET("black",TB_BLACK);
  SET("red",TB_RED);
  SET("green",TB_GREEN);
  SET("yellow",TB_YELLOW);
  SET("blue",TB_BLUE);
  SET("magenta",TB_MAGENTA);
  SET("cyan",TB_CYAN);
  SET("white",TB_WHITE);
#undef SET
  end:
  if(!rc){
    rc = s2_enum_builder_seal(&eb, dest);
  }
  s2_enum_builder_cleanup(&eb);
  return rc;
}
#endif/*s2_tb_get_colors() impl*/


/**
   Returns an object containing the TB_XXX attribute mappings. The key
   names are the lower-case XXX part of the TB_XXX entries. For
   symmetry with the colors list, it also has an entry named "default"
   with a value of 0.
*/
#if 1
static cwal_value * s2_tb_get_attr(s2_engine *se){
  int rc;
  cwal_value * n;
  cwal_value * v = cwal_new_object_value(se->e);
  if(!v) return NULL;
  cwal_value_ref(v);
#define SET(K,V) n = cwal_new_integer(se->e,(cwal_int_t)V); \
  if(!n) goto oom;                                          \
  cwal_value_ref(n);                                        \
  rc = cwal_prop_set(v, K, cwal_strlen(K), n);              \
  cwal_value_unref(n);                                      \
  if(rc) goto oom
  SET("default", 0);
  SET("bold",TB_BOLD);
  SET("underline",TB_UNDERLINE);
  SET("reverse",TB_REVERSE);
#undef SET
  cwal_value_unhand(v);
  return v;
  oom:
  cwal_value_unref(v);
  return NULL;
}
#else
/**
   Returns, via *dest, an enum containing the TB_XXX attribute
   mappings. The key names are the lower-case XXX part of the TB_XXX
   entries. For symmetry with the colors list, it also has an entry
   named "default" with a value of 0.
*/
static int s2_tb_get_attr(s2_engine *se, cwal_value ** dest){
  s2_enum_builder eb = s2_enum_builder_empty;
  int rc;
  cwal_value * v = 0;
  rc = s2_enum_builder_init( se, &eb, 0, 0);
  if(rc) return rc;
#define SET(K,V)                             \
  v = cwal_new_integer(se->e,(cwal_int_t)V); \
  if(!v) { rc = CWAL_RC_OOM; goto end; }     \
  cwal_value_ref(v);                         \
  rc = s2_enum_builder_append(&eb, K, v);    \
  cwal_value_unref(v);                       \
  v = 0;                                     \
  if(rc) goto end
  SET("default", 0);
  SET("bold",TB_BOLD);
  SET("underline",TB_UNDERLINE);
  SET("reverse",TB_REVERSE);
#undef SET
  end:
  if(!rc){
    rc = s2_enum_builder_seal(&eb, dest);
  }
  s2_enum_builder_cleanup(&eb);
  return rc;
}
#endif

static cwal_value * s2_tb_get_keys(cwal_engine *e){
  int rc = 0;
  cwal_value * v;
  cwal_value * vk;
  cwal_hash * h = cwal_new_hash(e, 117);
  cwal_value * hv = h ? cwal_hash_value(h) : 0;
  cwal_size_t keyLen;
  if(!h) return 0;
#define SET(K,V) v = 0; keyLen = cwal_strlen(K);                        \
  vk = cwal_new_xstring_value(e,K,keyLen);                              \
  cwal_value_ref(vk);                                                   \
  if( (rc = cwal_hash_insert_v(h, vk, (v=cwal_new_integer(e,(cwal_int_t)V)), 0)) ){ \
    cwal_value_unref(vk);                                               \
    cwal_value_unref(v);                                                \
    goto end;                                                           \
  } \
  if( (rc = cwal_hash_insert_v(h, v, vk, 0)) ){\
    if(CWAL_RC_ALREADY_EXISTS==rc) rc=0; \
    else { cwal_value_unref(vk); goto end; }    \
  } \
  cwal_value_unref(vk)

  /**
     Ordering is important, as several of these clash with others and
     we're only keeping the first of such clashes which gets inserted.
   */
  SET("ARROW_DOWN", TB_KEY_ARROW_DOWN);
  SET("ARROW_LEFT", TB_KEY_ARROW_LEFT);
  SET("ARROW_RIGHT", TB_KEY_ARROW_RIGHT);
  SET("ARROW_UP", TB_KEY_ARROW_UP);
  SET("BACKSPACE", TB_KEY_BACKSPACE);
  SET("DELETE", TB_KEY_DELETE);
  /* SET("BACKSPACE2", TB_KEY_BACKSPACE2); */
  SET("ESC", TB_KEY_ESC);
  SET("^2", TB_KEY_CTRL_2); /* clashes with CTRL_TILDE */
  /* SET("^~", TB_KEY_CTRL_TILDE); */
  /* SET("^3", TB_KEY_CTRL_3); */ /* clashes with ESC */
  /* SET("^\\", TB_KEY_CTRL_BACKSLASH); */ /* clashes with CTRL_4 */
  SET("^4", TB_KEY_CTRL_4);
  SET("^5", TB_KEY_CTRL_5);
  SET("^6", TB_KEY_CTRL_6);
  /* SET("^/", TB_KEY_CTRL_SLASH); */ /* clashes with CTRL_7 */
  /* SET("^_", TB_KEY_CTRL_UNDERSCORE); */ /* clashes with CTRL_7 */
  SET("^7", TB_KEY_CTRL_7);
  /* SET("^8", TB_KEY_CTRL_8); */ /* clashes with DELETE */
  SET("^A", TB_KEY_CTRL_A);
  SET("^B", TB_KEY_CTRL_B);
  SET("^C", TB_KEY_CTRL_C);
  SET("^D", TB_KEY_CTRL_D);
  SET("^E", TB_KEY_CTRL_E);
  SET("^F", TB_KEY_CTRL_F);
  SET("^G", TB_KEY_CTRL_G);
  SET("^H", TB_KEY_CTRL_H);
  SET("TAB", TB_KEY_TAB);
  /* SET("^I", TB_KEY_CTRL_I); */ /* clashes with TAB */
  SET("^J", TB_KEY_CTRL_J);
  SET("^K", TB_KEY_CTRL_K);
  SET("^L", TB_KEY_CTRL_L);
  SET("ENTER", TB_KEY_ENTER);
  /* SET("^M", TB_KEY_CTRL_M); */ /* clashes with ENTER */
  SET("^N", TB_KEY_CTRL_N);
  SET("^O", TB_KEY_CTRL_O);
  SET("^P", TB_KEY_CTRL_P);
  SET("^Q", TB_KEY_CTRL_Q);
  SET("^R", TB_KEY_CTRL_R);
  SET("^S", TB_KEY_CTRL_S);
  SET("^T", TB_KEY_CTRL_T);
  SET("^U", TB_KEY_CTRL_U);
  SET("^V", TB_KEY_CTRL_V);
  SET("^W", TB_KEY_CTRL_W);
  SET("^X", TB_KEY_CTRL_X);
  SET("^Y", TB_KEY_CTRL_Y);
  SET("^Z", TB_KEY_CTRL_Z);
  SET("^[", TB_KEY_CTRL_LSQ_BRACKET);
  SET("^]", TB_KEY_CTRL_RSQ_BRACKET);
  SET("^_", TB_KEY_CTRL_UNDERSCORE);
  SET("END", TB_KEY_END);
  SET("F1", TB_KEY_F1);
  SET("F10", TB_KEY_F10);
  SET("F11", TB_KEY_F11);
  SET("F12", TB_KEY_F12);
  SET("F2", TB_KEY_F2);
  SET("F3", TB_KEY_F3);
  SET("F4", TB_KEY_F4);
  SET("F5", TB_KEY_F5);
  SET("F6", TB_KEY_F6);
  SET("F7", TB_KEY_F7);
  SET("F8", TB_KEY_F8);
  SET("F9", TB_KEY_F9);
  SET("HOME", TB_KEY_HOME);
  SET("INSERT", TB_KEY_INSERT);
  SET("PGDN", TB_KEY_PGDN);
  SET("PGUP", TB_KEY_PGUP);
  SET("SPACE", TB_KEY_SPACE);

  {
    /* Add mappings for keys, a..z, A..Z, and 0..9 */
    int i, pos = 0;

    static char buf[2 * (26 + 26 + 10 /*a..zA..Z0..9*/)] = {0};
    /* static buffer b/c we use X-strings above */
    static char once = 0;
    if(!once){
      for( i = (int)'a'; i<=(int)'z'; ++i ){
        buf[pos++] = (char)i;
        buf[pos++] = 0;
      }
      for( i = (int)'A'; i<=(int)'Z'; ++i ){
        buf[pos++] = (char)i;
        buf[pos++] = 0;
      }
      for( i = (int)'0'; i<=(int)'9'; ++i ){
        buf[pos++] = (char)i;
        buf[pos++] = 0;
      }
      once = 1;
    }             
    for( i = 0; i < (int)(sizeof(buf)/sizeof(buf[0])); i+=2 ){
      SET(buf+i, (int)buf[i]);
    }
  }

#undef SET
  end:
  if(rc){
    cwal_value_unref(hv);
    hv = 0;
  }
  return hv;
}

/**
   s2 module initialization routine.

   Installs an object named "termbox" into ns, wrapping the termbox
   API:

   https://github.com/nsf/termbox
*/
static int s2_module_init_termbox( s2_engine * se, cwal_value ** rv ){
  int rc;
  cwal_value * v;
  cwal_value * mod;
  /* TODO?: use a Native so that we can attach a finalizer which
     restores the screen state if needed. */
  mod = cwal_new_object_value(se->e);
  if(!mod) return CWAL_RC_OOM;
  cwal_value_ref(mod);

  /* Some helper macros which don't really pay off in a small module,
     but get lots of copy/paste re-use between historical modules...
  */
#define CHECKV if(!v){ rc = CWAL_RC_OOM; goto end; } (void)0
#define SET(KEY) CHECKV;                                          \
  cwal_value_ref(v);                                              \
  rc = cwal_prop_set( mod, KEY, strlen(KEY), v );                \
  cwal_value_unref(v);                                            \
  /* reminder to self: do not set v=0, as this breaks downstream bits. */ \
  if(rc) goto end
#define FUNC(NAME,FP)                                   \
  v = cwal_new_function_value( se->e, FP, 0, 0, 0 );    \
  SET(NAME);

  v = cwal_new_string_value(se->e, "termbox", 7);
  SET("__typename");

  {
    s2_func_def const funcs[] = {
    /* Install functions... */
    /* S2_FUNC2("cellAt", cb_get_cell), */
    S2_FUNC2("change", cb_change_cell),
    S2_FUNC2("clear", cb_clear),
    S2_FUNC2("clearLine", cb_clear_line),
    S2_FUNC2("cursor", cb_set_cursor),
    S2_FUNC2("height", cb_height),
    S2_FUNC2("init", cb_init),
    S2_FUNC2("newPad", cb_new_pad),
    S2_FUNC2("newWindow", cb_new_window),
    S2_FUNC2("peek", cb_peek_event),
    S2_FUNC2("poll", cb_poll_event),
    S2_FUNC2("present", cb_present),
    S2_FUNC2("print5", cb_print5),
    S2_FUNC2("print3", cb_print3),
    S2_FUNC2("shutdown", cb_shutdown),
    S2_FUNC2("size", cb_size),
    S2_FUNC2("width", cb_width),
    s2_func_def_empty_m
    };
    rc = s2_install_functions(se, mod, funcs, 0);
    if(rc) goto end;
  }
  v = tb_prototype_pad(se->e);
  SET("Pad");
  v = tb_prototype_window(se->e);
  SET("Window");
  v = tb_prototype_buf(se->e);
  SET("CellBuffer");

  FUNC("inputMode", cb_select_input_mode);
  s2_tb_get_input_modes(se->e, v);

  FUNC("outputMode", cb_select_output_mode);
  s2_tb_get_output_modes(se->e, v);

#if 1
  v = s2_tb_get_colors(se);
#else
  v = 0;
  rc = s2_tb_get_colors(se, &v);
  if(rc) goto end;
#endif
  SET("color");
#if 1
  v = s2_tb_get_attr(se);
#else
  rc = s2_tb_get_attr(se, &v);
  if(rc) goto end;
#endif
  assert(v);
  SET("attr");
  v = s2_tb_get_keys(se->e);
  SET("keys");

#undef SET
#undef FUNC
#undef CHECKV
  end:
  if(rc){
    cwal_value_unref(mod);
  }else{
    *rv = mod;
    cwal_value_unhand(mod);
  }
  return rc;
}

S2_MODULE_REGISTER_(termbox);

#undef THIS_WINDOW
#undef MARKER
#undef PRINT_CLEAR_TO_EOL
#undef CB_TB_INIT
#undef s2_tbwin_is_pad
#undef THIS_CBUF
