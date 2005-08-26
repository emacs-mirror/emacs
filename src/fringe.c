/* Fringe handling (split from xdisp.c).
   Copyright (C) 1985, 1986, 1987, 1988, 1993, 1994, 1995, 1997,
                 1998, 1999, 2000, 2000, 2001, 2002, 2003, 2004,
                 2005 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include <config.h>
#include <stdio.h>

#include "lisp.h"
#include "frame.h"
#include "window.h"
#include "dispextern.h"
#include "buffer.h"
#include "blockinput.h"

#ifdef HAVE_WINDOW_SYSTEM

extern Lisp_Object Qfringe;
extern Lisp_Object Qtop, Qbottom, Qcenter;
extern Lisp_Object Qup, Qdown, Qleft, Qright;

/* Non-nil means that newline may flow into the right fringe.  */

Lisp_Object Voverflow_newline_into_fringe;

/* List of known fringe bitmap symbols.

   The fringe bitmap number is stored in the `fringe' property on
   those symbols.  Names for the built-in bitmaps are installed by
   loading fringe.el.
 */

Lisp_Object Vfringe_bitmaps;

enum fringe_bitmap_type
{
  NO_FRINGE_BITMAP = 0,
  UNDEF_FRINGE_BITMAP,
  LEFT_TRUNCATION_BITMAP,
  RIGHT_TRUNCATION_BITMAP,
  UP_ARROW_BITMAP,
  DOWN_ARROW_BITMAP,
  CONTINUED_LINE_BITMAP,
  CONTINUATION_LINE_BITMAP,
  OVERLAY_ARROW_BITMAP,
  TOP_LEFT_ANGLE_BITMAP,
  TOP_RIGHT_ANGLE_BITMAP,
  BOTTOM_LEFT_ANGLE_BITMAP,
  BOTTOM_RIGHT_ANGLE_BITMAP,
  LEFT_BRACKET_BITMAP,
  RIGHT_BRACKET_BITMAP,
  FILLED_BOX_CURSOR_BITMAP,
  HOLLOW_BOX_CURSOR_BITMAP,
  HOLLOW_SQUARE_BITMAP,
  BAR_CURSOR_BITMAP,
  HBAR_CURSOR_BITMAP,
  ZV_LINE_BITMAP,
  MAX_STANDARD_FRINGE_BITMAPS
};

enum fringe_bitmap_align
{
  ALIGN_BITMAP_CENTER = 0,
  ALIGN_BITMAP_TOP,
  ALIGN_BITMAP_BOTTOM
};

struct fringe_bitmap
{
  unsigned short *bits;
  unsigned height : 8;
  unsigned width : 8;
  unsigned period : 8;
  unsigned align : 2;
  unsigned dynamic : 1;
};


/***********************************************************************
			       Fringe bitmaps
 ***********************************************************************/

/* Undefined bitmap.  A question mark.  */
/*
  ..xxxx..
  .xxxxxx.
  xx....xx
  xx....xx
  ....xx..
  ...xx...
  ...xx...
  ........
  ...xx...
  ...xx...
*/
static unsigned short unknown_bits[] = {
  0x3c, 0x7e, 0x7e, 0x0c, 0x18, 0x18, 0x00, 0x18, 0x18};

/* An arrow like this: `<-'.  */
/*
  ...xx...
  ..xx....
  .xx.....
  xxxxxx..
  xxxxxx..
  .xx.....
  ..xx....
  ...xx...
*/
static unsigned short left_arrow_bits[] = {
   0x18, 0x30, 0x60, 0xfc, 0xfc, 0x60, 0x30, 0x18};


/* Right truncation arrow bitmap `->'.  */
/*
  ...xx...
  ....xx..
  .....xx.
  ..xxxxxx
  ..xxxxxx
  .....xx.
  ....xx..
  ...xx...
*/
static unsigned short right_arrow_bits[] = {
   0x18, 0x0c, 0x06, 0x3f, 0x3f, 0x06, 0x0c, 0x18};


/* Up arrow bitmap.  */
/*
  ...xx...
  ..xxxx..
  .xxxxxx.
  xxxxxxxx
  ...xx...
  ...xx...
  ...xx...
  ...xx...
*/
static unsigned short up_arrow_bits[] = {
   0x18, 0x3c, 0x7e, 0xff, 0x18, 0x18, 0x18, 0x18};


/* Down arrow bitmap.  */
/*
  ...xx...
  ...xx...
  ...xx...
  ...xx...
  xxxxxxxx
  .xxxxxx.
  ..xxxx..
  ...xx...
*/
static unsigned short down_arrow_bits[] = {
   0x18, 0x18, 0x18, 0x18, 0xff, 0x7e, 0x3c, 0x18};

/* Marker for continued lines.  */
/*
  ..xxxx..
  ..xxxxx.
  ......xx
  ..x..xxx
  ..xxxxxx
  ..xxxxx.
  ..xxxx..
  ..xxxxx.
*/
static unsigned short continued_bits[] = {
   0x3c, 0x3e, 0x03, 0x27, 0x3f, 0x3e, 0x3c, 0x3e};

/* Marker for continuation lines.  */
/*
  ..xxxx..
  .xxxxx..
  xx......
  xxx..x..
  xxxxxx..
  .xxxxx..
  ..xxxx..
  .xxxxx..
*/
static unsigned short continuation_bits[] = {
   0x3c, 0x7c, 0xc0, 0xe4, 0xfc, 0x7c, 0x3c, 0x7c};

/* Overlay arrow bitmap.  A triangular arrow.  */
/*
  xx......
  xxxx....
  xxxxx...
  xxxxxx..
  xxxxxx..
  xxxxx...
  xxxx....
  xx......
*/
static unsigned short ov_bits[] = {
   0xc0, 0xf0, 0xf8, 0xfc, 0xfc, 0xf8, 0xf0, 0xc0};

#if 0
/* Reverse Overlay arrow bitmap.  A triangular arrow.  */
/*
  ......xx
  ....xxxx
  ...xxxxx
  ..xxxxxx
  ..xxxxxx
  ...xxxxx
  ....xxxx
  ......xx
*/
static unsigned short rev_ov_bits[] = {
   0x03, 0x0f, 0x1f, 0x3f, 0x3f, 0x1f, 0x0f, 0x03};
#endif

/* First line bitmap.  An top-left angle.  */
/*
  xxxxxx..
  xxxxxx..
  xx......
  xx......
  xx......
  xx......
  xx......
  ........
*/
static unsigned short top_left_angle_bits[] = {
   0xfc, 0xfc, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0x00};

/* First line bitmap.  An right-up angle.  */
/*
  ..xxxxxx
  ..xxxxxx
  ......xx
  ......xx
  ......xx
  ......xx
  ......xx
  ........
*/
static unsigned short top_right_angle_bits[] = {
   0x3f, 0x3f, 0x03, 0x03, 0x03, 0x03, 0x03, 0x00};

/* Last line bitmap.  An left-down angle.  */
/*
  ........
  xx......
  xx......
  xx......
  xx......
  xx......
  xxxxxx..
  xxxxxx..
*/
static unsigned short bottom_left_angle_bits[] = {
   0x00, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xfc, 0xfc};

/* Last line bitmap.  An right-down angle.  */
/*
  ........
  ......xx
  ......xx
  ......xx
  ......xx
  ......xx
  ..xxxxxx
  ..xxxxxx
*/
static unsigned short bottom_right_angle_bits[] = {
   0x00, 0x03, 0x03, 0x03, 0x03, 0x03, 0x3f, 0x3f};

/* First/last line bitmap.  An left bracket.  */
/*
  xxxxxx..
  xxxxxx..
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xxxxxx..
  xxxxxx..
*/
static unsigned short left_bracket_bits[] = {
   0xfc, 0xfc, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xfc, 0xfc};

/* First/last line bitmap.  An right bracket.  */
/*
  ..xxxxxx
  ..xxxxxx
  ......xx
  ......xx
  ......xx
  ......xx
  ......xx
  ......xx
  ..xxxxxx
  ..xxxxxx
*/
static unsigned short right_bracket_bits[] = {
  0x3f, 0x3f, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x3f, 0x3f};

/* Filled box cursor bitmap.  A filled box; max 13 pixels high.  */
/*
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
*/
static unsigned short filled_box_cursor_bits[] = {
   0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe};

/* Hollow box cursor bitmap.  A hollow box; max 13 pixels high.  */
/*
  xxxxxxx.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  xxxxxxx.
*/
static unsigned short hollow_box_cursor_bits[] = {
   0xfe, 0x82, 0x82, 0x82, 0x82, 0x82, 0x82, 0x82, 0x82, 0x82, 0x82, 0x82, 0xfe};

/* Bar cursor bitmap.  A vertical bar; max 13 pixels high.  */
/*
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
*/
static unsigned short bar_cursor_bits[] = {
   0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0};

/* HBar cursor bitmap.  A horisontal bar; 2 pixels high.  */
/*
  xxxxxxx.
  xxxxxxx.
*/
static unsigned short hbar_cursor_bits[] = {
  0xfe, 0xfe};


/* Bitmap drawn to indicate lines not displaying text if
   `indicate-empty-lines' is non-nil.  */
/*
  ........
  ..xxxx..
  ........
  ........
  ..xxxx..
  ........
*/
static unsigned short zv_bits[] = {
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00};

/* Hollow square bitmap.  */
/*
  .xxxxxx.
  .x....x.
  .x....x.
  .x....x.
  .x....x.
  .xxxxxx.
*/
static unsigned short hollow_square_bits[] = {
   0x7e, 0x42, 0x42, 0x42, 0x42, 0x7e};


#define BYTES_PER_BITMAP_ROW  (sizeof (unsigned short))
#define STANDARD_BITMAP_HEIGHT(bits) (sizeof (bits)/BYTES_PER_BITMAP_ROW)
#define FRBITS(bits)  bits, STANDARD_BITMAP_HEIGHT (bits)

struct fringe_bitmap standard_bitmaps[MAX_STANDARD_FRINGE_BITMAPS] =
{
  { NULL, 0, 0, 0, 0, 0 }, /* NO_FRINGE_BITMAP */
  { FRBITS (unknown_bits),            8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (left_arrow_bits),         8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (right_arrow_bits),        8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (up_arrow_bits),           8, 0, ALIGN_BITMAP_TOP,    0 },
  { FRBITS (down_arrow_bits),         8, 0, ALIGN_BITMAP_BOTTOM, 0 },
  { FRBITS (continued_bits),          8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (continuation_bits),       8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (ov_bits),                 8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (top_left_angle_bits),     8, 0, ALIGN_BITMAP_TOP,    0 },
  { FRBITS (top_right_angle_bits),    8, 0, ALIGN_BITMAP_TOP,    0 },
  { FRBITS (bottom_left_angle_bits),  8, 0, ALIGN_BITMAP_BOTTOM, 0 },
  { FRBITS (bottom_right_angle_bits), 8, 0, ALIGN_BITMAP_BOTTOM, 0 },
  { FRBITS (left_bracket_bits),       8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (right_bracket_bits),      8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (filled_box_cursor_bits),  8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (hollow_box_cursor_bits),  8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (hollow_square_bits),      8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (bar_cursor_bits),         8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (hbar_cursor_bits),        8, 0, ALIGN_BITMAP_BOTTOM, 0 },
  { FRBITS (zv_bits),                 8, 3, ALIGN_BITMAP_TOP,    0 },
};

static struct fringe_bitmap **fringe_bitmaps;
static Lisp_Object *fringe_faces;
static int max_fringe_bitmaps;

static int max_used_fringe_bitmap = MAX_STANDARD_FRINGE_BITMAPS;


/* Lookup bitmap number for symbol BITMAP.
   Return 0 if not a bitmap.  */

int
lookup_fringe_bitmap (bitmap)
     Lisp_Object bitmap;
{
  int bn;

  bitmap = Fget (bitmap, Qfringe);
  if (!INTEGERP (bitmap))
    return 0;

  bn = XINT (bitmap);
  if (bn > NO_FRINGE_BITMAP
      && bn < max_used_fringe_bitmap
      && (bn < MAX_STANDARD_FRINGE_BITMAPS
	  || fringe_bitmaps[bn] != NULL))
    return bn;

  return 0;
}

/* Get fringe bitmap name for bitmap number BN.

   Found by traversing Vfringe_bitmaps comparing BN to the
   fringe property for each symbol.

   Return BN if not found in Vfringe_bitmaps.  */

static Lisp_Object
get_fringe_bitmap_name (bn)
     int bn;
{
  Lisp_Object bitmaps;
  Lisp_Object num;

  /* Zero means no bitmap -- return nil.  */
  if (bn <= 0)
    return Qnil;

  bitmaps = Vfringe_bitmaps;
  num = make_number (bn);

  while (CONSP (bitmaps))
    {
      Lisp_Object bitmap = XCAR (bitmaps);
      if (EQ (num, Fget (bitmap, Qfringe)))
	return bitmap;
      bitmaps = XCDR (bitmaps);
    }

  return num;
}


/* Draw the bitmap WHICH in one of the left or right fringes of
   window W.  ROW is the glyph row for which to display the bitmap; it
   determines the vertical position at which the bitmap has to be
   drawn.
   LEFT_P is 1 for left fringe, 0 for right fringe.
*/

void
draw_fringe_bitmap_1 (w, row, left_p, overlay, which)
     struct window *w;
     struct glyph_row *row;
     int left_p, overlay;
     enum fringe_bitmap_type which;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct draw_fringe_bitmap_params p;
  struct fringe_bitmap *fb;
  int period;
  int face_id = DEFAULT_FACE_ID;

  p.cursor_p = 0;
  p.overlay_p = (overlay & 1) == 1;
  p.cursor_p = (overlay & 2) == 2;

  if (which != NO_FRINGE_BITMAP)
    {
    }
  else if (left_p)
    {
      which = row->left_fringe_bitmap;
      face_id = row->left_fringe_face_id;
    }
  else
    {
      which = row->right_fringe_bitmap;
      face_id = row->right_fringe_face_id;
    }

  if (face_id == DEFAULT_FACE_ID)
    {
      Lisp_Object face;

      if ((face = fringe_faces[which], NILP (face))
	  || (face_id = lookup_derived_face (f, face, FRINGE_FACE_ID, 0),
	      face_id < 0))
	face_id = FRINGE_FACE_ID;
    }

  fb = fringe_bitmaps[which];
  if (fb == NULL)
    fb = &standard_bitmaps[which < MAX_STANDARD_FRINGE_BITMAPS
			   ? which : UNDEF_FRINGE_BITMAP];

  period = fb->period;

  /* Convert row to frame coordinates.  */
  p.y = WINDOW_TO_FRAME_PIXEL_Y (w, row->y);

  p.which = which;
  p.bits = fb->bits;
  p.wd = fb->width;

  p.h = fb->height;
  p.dh = (period > 0 ? (p.y % period) : 0);
  p.h -= p.dh;
  /* Clip bitmap if too high.  */
  if (p.h > row->height)
    p.h = row->height;

  p.face = FACE_FROM_ID (f, face_id);

  if (p.face == NULL)
    {
      /* This could happen after clearing face cache.
	 But it shouldn't happen anymore.  ++kfs */
      return;
    }

  PREPARE_FACE_FOR_DISPLAY (f, p.face);

  /* Clear left fringe if no bitmap to draw or if bitmap doesn't fill
     the fringe.  */
  p.bx = -1;
  if (left_p)
    {
      int wd = WINDOW_LEFT_FRINGE_WIDTH (w);
      int x = window_box_left (w, (WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (w)
				   ? LEFT_MARGIN_AREA
				   : TEXT_AREA));
      if (p.wd > wd)
	p.wd = wd;
      p.x = x - p.wd - (wd - p.wd) / 2;

      if (p.wd < wd || row->height > p.h)
	{
	  /* If W has a vertical border to its left, don't draw over it.  */
	  wd -= ((!WINDOW_LEFTMOST_P (w)
		  && !WINDOW_HAS_VERTICAL_SCROLL_BAR (w))
		 ? 1 : 0);
	  p.bx = x - wd;
	  p.nx = wd;
	}
    }
  else
    {
      int x = window_box_right (w,
				(WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (w)
				 ? RIGHT_MARGIN_AREA
				 : TEXT_AREA));
      int wd = WINDOW_RIGHT_FRINGE_WIDTH (w);
      if (p.wd > wd)
	p.wd = wd;
      p.x = x + (wd - p.wd) / 2;
      /* Clear right fringe if no bitmap to draw of if bitmap doesn't fill
	 the fringe.  */
      if (p.wd < wd || row->height > p.h)
	{
	  p.bx = x;
	  p.nx = wd;
	}
    }

  if (p.bx >= 0)
    {
      int header_line_height = WINDOW_HEADER_LINE_HEIGHT (w);

      p.by = WINDOW_TO_FRAME_PIXEL_Y (w, max (header_line_height, row->y));
      p.ny = row->visible_height;
    }

  /* Adjust y to the offset in the row to start drawing the bitmap.  */
  switch (fb->align)
    {
    case ALIGN_BITMAP_CENTER:
      p.y += (row->height - p.h) / 2;
      break;
    case ALIGN_BITMAP_BOTTOM:
      p.h = fb->height;
      p.y += (row->visible_height - p.h);
      break;
    case ALIGN_BITMAP_TOP:
      break;
    }

  rif->draw_fringe_bitmap (w, row, &p);
}

void
draw_fringe_bitmap (w, row, left_p)
     struct window *w;
     struct glyph_row *row;
     int left_p;
{
  int overlay = 0;

  if (!left_p && row->cursor_in_fringe_p)
    {
      int cursor = NO_FRINGE_BITMAP;

      switch (w->phys_cursor_type)
	{
	case HOLLOW_BOX_CURSOR:
	  if (row->visible_height >= STANDARD_BITMAP_HEIGHT (hollow_box_cursor_bits))
	    cursor = HOLLOW_BOX_CURSOR_BITMAP;
	  else
	    cursor = HOLLOW_SQUARE_BITMAP;
	  break;
	case FILLED_BOX_CURSOR:
	  cursor = FILLED_BOX_CURSOR_BITMAP;
	  break;
	case BAR_CURSOR:
	  cursor = BAR_CURSOR_BITMAP;
	  break;
	case HBAR_CURSOR:
	  cursor = HBAR_CURSOR_BITMAP;
	  break;
	case NO_CURSOR:
	default:
	  w->phys_cursor_on_p = 0;
	  row->cursor_in_fringe_p = 0;
	  break;
	}
      if (cursor != NO_FRINGE_BITMAP)
	{
	  draw_fringe_bitmap_1 (w, row, 0, 2, cursor);
	  overlay = cursor == FILLED_BOX_CURSOR_BITMAP ? 3 : 1;
	}
    }

  draw_fringe_bitmap_1 (w, row, left_p, overlay, NO_FRINGE_BITMAP);

  if (left_p && row->overlay_arrow_bitmap != NO_FRINGE_BITMAP)
    draw_fringe_bitmap_1 (w, row, 1, 1,
			  (row->overlay_arrow_bitmap < 0
			   ? OVERLAY_ARROW_BITMAP
			   : row->overlay_arrow_bitmap));
}


/* Draw fringe bitmaps for glyph row ROW on window W.  Call this
   function with input blocked.  */

void
draw_row_fringe_bitmaps (w, row)
     struct window *w;
     struct glyph_row *row;
{
  xassert (interrupt_input_blocked);

  /* If row is completely invisible, because of vscrolling, we
     don't have to draw anything.  */
  if (row->visible_height <= 0)
    return;

  if (WINDOW_LEFT_FRINGE_WIDTH (w) != 0)
    draw_fringe_bitmap (w, row, 1);

  if (WINDOW_RIGHT_FRINGE_WIDTH (w) != 0)
    draw_fringe_bitmap (w, row, 0);
}

/* Draw the fringes of window W.  Only fringes for rows marked for
   update in redraw_fringe_bitmaps_p are drawn.

   Return >0 if left or right fringe was redrawn in any way.

   If NO_FRINGE is non-zero, also return >0 if either fringe has zero width.

   A return value >0 indicates that the vertical line between windows
   needs update (as it may be drawn in the fringe).
*/

int
draw_window_fringes (w, no_fringe)
     struct window *w;
     int no_fringe;
{
  struct glyph_row *row;
  int yb = window_text_bottom_y (w);
  int nrows = w->current_matrix->nrows;
  int y = 0, rn;
  int updated = 0;

  if (w->pseudo_window_p)
    return 0;

  /* Must draw line if no fringe */
  if (no_fringe
      && (WINDOW_LEFT_FRINGE_WIDTH (w) == 0
	  || WINDOW_RIGHT_FRINGE_WIDTH (w) == 0))
    updated++;

  for (y = 0, rn = 0, row = w->current_matrix->rows;
       y < yb && rn < nrows;
       y += row->height, ++row, ++rn)
    {
      if (!row->redraw_fringe_bitmaps_p)
	continue;
      draw_row_fringe_bitmaps (w, row);
      row->redraw_fringe_bitmaps_p = 0;
      updated++;
    }

  return updated;
}


/* Recalculate the bitmaps to show in the fringes of window W.
   Only mark rows with modified bitmaps for update in redraw_fringe_bitmaps_p.

   If KEEP_CURRENT_P is 0, update current_matrix too.  */

int
update_window_fringes (w, keep_current_p)
     struct window *w;
     int keep_current_p;
{
  struct glyph_row *row, *cur = 0;
  int yb = window_text_bottom_y (w);
  int rn, nrows = w->current_matrix->nrows;
  int y;
  int redraw_p = 0;
  Lisp_Object boundary_top = Qnil, boundary_bot = Qnil;
  Lisp_Object arrow_top = Qnil, arrow_bot = Qnil;
  Lisp_Object empty_pos;
  Lisp_Object ind = Qnil;

  if (w->pseudo_window_p)
    return 0;

  if (!MINI_WINDOW_P (w)
      && (ind = XBUFFER (w->buffer)->indicate_buffer_boundaries, !NILP (ind)))
    {
      if (EQ (ind, Qleft) || EQ (ind, Qright))
	boundary_top = boundary_bot = arrow_top = arrow_bot = ind;
      else if (CONSP (ind) && CONSP (XCAR (ind)))
	{
	  Lisp_Object pos;
	  if (pos = Fassq (Qt, ind), !NILP (pos))
	    boundary_top = boundary_bot = arrow_top = arrow_bot = XCDR (pos);
	  if (pos = Fassq (Qtop, ind), !NILP (pos))
	    boundary_top = XCDR (pos);
	  if (pos = Fassq (Qbottom, ind), !NILP (pos))
	    boundary_bot = XCDR (pos);
	  if (pos = Fassq (Qup, ind), !NILP (pos))
	    arrow_top = XCDR (pos);
	  if (pos = Fassq (Qdown, ind), !NILP (pos))
	    arrow_bot = XCDR (pos);
	}
      else
	/* Anything else means boundary on left and no arrows.  */
	boundary_top = boundary_bot = Qleft;
    }

  if (!NILP (ind))
    {
      int done_top = 0, done_bot = 0;

      for (y = 0, rn = 0;
	   y < yb && rn < nrows;
	   y += row->height, ++rn)
	{
	  unsigned indicate_bob_p, indicate_top_line_p;
	  unsigned indicate_eob_p, indicate_bottom_line_p;

	  row = w->desired_matrix->rows + rn;
	  if (!row->enabled_p)
	    row = w->current_matrix->rows + rn;

	  indicate_bob_p = row->indicate_bob_p;
	  indicate_top_line_p = row->indicate_top_line_p;
	  indicate_eob_p = row->indicate_eob_p;
	  indicate_bottom_line_p = row->indicate_bottom_line_p;

	  row->indicate_bob_p = row->indicate_top_line_p = 0;
	  row->indicate_eob_p = row->indicate_bottom_line_p = 0;

	  if (!row->mode_line_p)
	    {
	      if (!done_top)
		{
		  if (MATRIX_ROW_START_CHARPOS (row) <= BUF_BEGV (XBUFFER (w->buffer))
		      && !MATRIX_ROW_PARTIALLY_VISIBLE_AT_TOP_P (w, row))
		    row->indicate_bob_p = !NILP (boundary_top);
		  else
		    row->indicate_top_line_p = !NILP (arrow_top);
		  done_top = 1;
		}

	      if (!done_bot)
		{
		  if (MATRIX_ROW_END_CHARPOS (row) >= BUF_ZV (XBUFFER (w->buffer))
		      && !MATRIX_ROW_PARTIALLY_VISIBLE_AT_BOTTOM_P (w, row))
		    row->indicate_eob_p = !NILP (boundary_bot), done_bot = 1;
		  else if (y + row->height >= yb)
		    row->indicate_bottom_line_p = !NILP (arrow_bot), done_bot = 1;
		}
	    }

	  if (indicate_bob_p != row->indicate_bob_p
	      || indicate_top_line_p != row->indicate_top_line_p
	      || indicate_eob_p != row->indicate_eob_p
	      || indicate_bottom_line_p != row->indicate_bottom_line_p)
	    row->redraw_fringe_bitmaps_p = 1;
	}
    }

  empty_pos = XBUFFER (w->buffer)->indicate_empty_lines;
  if (!NILP (empty_pos) && !EQ (empty_pos, Qright))
    empty_pos = WINDOW_LEFT_FRINGE_WIDTH (w) == 0 ? Qright : Qleft;

  for (y = 0, rn = 0;
       y < yb && rn < nrows;
       y += row->height, rn++)
    {
      enum fringe_bitmap_type left, right;
      unsigned left_face_id, right_face_id;

      row = w->desired_matrix->rows + rn;
      cur = w->current_matrix->rows + rn;
      if (!row->enabled_p)
	row = cur;

      left_face_id = right_face_id = DEFAULT_FACE_ID;

      /* Decide which bitmap to draw in the left fringe.  */
      if (WINDOW_LEFT_FRINGE_WIDTH (w) == 0)
	left = NO_FRINGE_BITMAP;
      else if (row->left_user_fringe_bitmap != NO_FRINGE_BITMAP)
	{
	  left = row->left_user_fringe_bitmap;
	  left_face_id = row->left_user_fringe_face_id;
	}
      else if (row->truncated_on_left_p)
	left = LEFT_TRUNCATION_BITMAP;
      else if (row->indicate_bob_p && EQ (boundary_top, Qleft))
	left = ((row->indicate_eob_p && EQ (boundary_bot, Qleft))
		? LEFT_BRACKET_BITMAP : TOP_LEFT_ANGLE_BITMAP);
      else if (row->indicate_eob_p && EQ (boundary_bot, Qleft))
	left = BOTTOM_LEFT_ANGLE_BITMAP;
      else if (MATRIX_ROW_CONTINUATION_LINE_P (row))
	left = CONTINUATION_LINE_BITMAP;
      else if (row->indicate_empty_line_p && EQ (empty_pos, Qleft))
	left = ZV_LINE_BITMAP;
      else if (row->indicate_top_line_p && EQ (arrow_top, Qleft))
	left = UP_ARROW_BITMAP;
      else if (row->indicate_bottom_line_p && EQ (arrow_bot, Qleft))
	left = DOWN_ARROW_BITMAP;
      else
	left = NO_FRINGE_BITMAP;

      /* Decide which bitmap to draw in the right fringe.  */
      if (WINDOW_RIGHT_FRINGE_WIDTH (w) == 0)
	right = NO_FRINGE_BITMAP;
      else if (row->right_user_fringe_bitmap != NO_FRINGE_BITMAP)
	{
	  right = row->right_user_fringe_bitmap;
	  right_face_id = row->right_user_fringe_face_id;
	}
      else if (row->truncated_on_right_p)
	right = RIGHT_TRUNCATION_BITMAP;
      else if (row->indicate_bob_p && EQ (boundary_top, Qright))
	right = ((row->indicate_eob_p && EQ (boundary_bot, Qright))
		 ? RIGHT_BRACKET_BITMAP : TOP_RIGHT_ANGLE_BITMAP);
      else if (row->indicate_eob_p && EQ (boundary_bot, Qright))
	right = BOTTOM_RIGHT_ANGLE_BITMAP;
      else if (row->continued_p)
	right = CONTINUED_LINE_BITMAP;
      else if (row->indicate_top_line_p && EQ (arrow_top, Qright))
	right = UP_ARROW_BITMAP;
      else if (row->indicate_bottom_line_p && EQ (arrow_bot, Qright))
	right = DOWN_ARROW_BITMAP;
      else if (row->indicate_empty_line_p && EQ (empty_pos, Qright))
	right = ZV_LINE_BITMAP;
      else
	right = NO_FRINGE_BITMAP;

      if (row->y != cur->y
	  || row->visible_height != cur->visible_height
	  || row->ends_at_zv_p != cur->ends_at_zv_p
	  || left != cur->left_fringe_bitmap
	  || right != cur->right_fringe_bitmap
	  || left_face_id != cur->left_fringe_face_id
	  || right_face_id != cur->right_fringe_face_id
	  || cur->redraw_fringe_bitmaps_p)
	{
	  redraw_p = row->redraw_fringe_bitmaps_p = 1;
	  if (!keep_current_p)
	    {
	      cur->redraw_fringe_bitmaps_p = 1;
	      cur->left_fringe_bitmap = left;
	      cur->right_fringe_bitmap = right;
	      cur->left_fringe_face_id = left_face_id;
	      cur->right_fringe_face_id = right_face_id;
	    }
	}

      if (row->overlay_arrow_bitmap != cur->overlay_arrow_bitmap)
	{
	  redraw_p = row->redraw_fringe_bitmaps_p = cur->redraw_fringe_bitmaps_p = 1;
	  cur->overlay_arrow_bitmap = row->overlay_arrow_bitmap;
	}

      row->left_fringe_bitmap = left;
      row->right_fringe_bitmap = right;
      row->left_fringe_face_id = left_face_id;
      row->right_fringe_face_id = right_face_id;

      if (rn > 0 && row->redraw_fringe_bitmaps_p)
	row[-1].redraw_fringe_bitmaps_p = cur[-1].redraw_fringe_bitmaps_p = 1;
    }

  return redraw_p && !keep_current_p;
}


/* Compute actual fringe widths for frame F.

   If REDRAW is 1, redraw F if the fringe settings was actually
   modified and F is visible.

   Since the combined left and right fringe must occupy an integral
   number of columns, we may need to add some pixels to each fringe.
   Typically, we add an equal amount (+/- 1 pixel) to each fringe,
   but a negative width value is taken literally (after negating it).

   We never make the fringes narrower than specified.
*/

void
compute_fringe_widths (f, redraw)
     struct frame *f;
     int redraw;
{
  int o_left = FRAME_LEFT_FRINGE_WIDTH (f);
  int o_right = FRAME_RIGHT_FRINGE_WIDTH (f);
  int o_cols = FRAME_FRINGE_COLS (f);

  Lisp_Object left_fringe = Fassq (Qleft_fringe, f->param_alist);
  Lisp_Object right_fringe = Fassq (Qright_fringe, f->param_alist);
  int left_fringe_width, right_fringe_width;

  if (!NILP (left_fringe))
    left_fringe = Fcdr (left_fringe);
  if (!NILP (right_fringe))
    right_fringe = Fcdr (right_fringe);

  left_fringe_width = ((NILP (left_fringe) || !INTEGERP (left_fringe)) ? 8 :
		       XINT (left_fringe));
  right_fringe_width = ((NILP (right_fringe) || !INTEGERP (right_fringe)) ? 8 :
			XINT (right_fringe));

  if (left_fringe_width || right_fringe_width)
    {
      int left_wid = left_fringe_width >= 0 ? left_fringe_width : -left_fringe_width;
      int right_wid = right_fringe_width >= 0 ? right_fringe_width : -right_fringe_width;
      int conf_wid = left_wid + right_wid;
      int font_wid = FRAME_COLUMN_WIDTH (f);
      int cols = (left_wid + right_wid + font_wid-1) / font_wid;
      int real_wid = cols * font_wid;
      if (left_wid && right_wid)
	{
	  if (left_fringe_width < 0)
	    {
	      /* Left fringe width is fixed, adjust right fringe if necessary */
	      FRAME_LEFT_FRINGE_WIDTH (f) = left_wid;
	      FRAME_RIGHT_FRINGE_WIDTH (f) = real_wid - left_wid;
	    }
	  else if (right_fringe_width < 0)
	    {
	      /* Right fringe width is fixed, adjust left fringe if necessary */
	      FRAME_LEFT_FRINGE_WIDTH (f) = real_wid - right_wid;
	      FRAME_RIGHT_FRINGE_WIDTH (f) = right_wid;
	    }
	  else
	    {
	      /* Adjust both fringes with an equal amount.
		 Note that we are doing integer arithmetic here, so don't
		 lose a pixel if the total width is an odd number.  */
	      int fill = real_wid - conf_wid;
	      FRAME_LEFT_FRINGE_WIDTH (f) = left_wid + fill/2;
	      FRAME_RIGHT_FRINGE_WIDTH (f) = right_wid + fill - fill/2;
	    }
	}
      else if (left_fringe_width)
	{
	  FRAME_LEFT_FRINGE_WIDTH (f) = real_wid;
	  FRAME_RIGHT_FRINGE_WIDTH (f) = 0;
	}
      else
	{
	  FRAME_LEFT_FRINGE_WIDTH (f) = 0;
	  FRAME_RIGHT_FRINGE_WIDTH (f) = real_wid;
	}
      FRAME_FRINGE_COLS (f) = cols;
    }
  else
    {
      FRAME_LEFT_FRINGE_WIDTH (f) = 0;
      FRAME_RIGHT_FRINGE_WIDTH (f) = 0;
      FRAME_FRINGE_COLS (f) = 0;
    }

  if (redraw && FRAME_VISIBLE_P (f))
    if (o_left != FRAME_LEFT_FRINGE_WIDTH (f) ||
	o_right != FRAME_RIGHT_FRINGE_WIDTH (f) ||
	o_cols != FRAME_FRINGE_COLS (f))
      redraw_frame (f);
}


/* Free resources used by a user-defined bitmap.  */

void
destroy_fringe_bitmap (n)
     int n;
{
  struct fringe_bitmap **fbp;

  fringe_faces[n] = Qnil;

  fbp = &fringe_bitmaps[n];
  if (*fbp && (*fbp)->dynamic)
    {
      if (rif && rif->destroy_fringe_bitmap)
	rif->destroy_fringe_bitmap (n);
      xfree (*fbp);
      *fbp = NULL;
    }

  while (max_used_fringe_bitmap > MAX_STANDARD_FRINGE_BITMAPS
	 && fringe_bitmaps[max_used_fringe_bitmap - 1] == NULL)
    max_used_fringe_bitmap--;
}


DEFUN ("destroy-fringe-bitmap", Fdestroy_fringe_bitmap, Sdestroy_fringe_bitmap,
       1, 1, 0,
       doc: /* Destroy fringe bitmap BITMAP.
If BITMAP overrides a standard fringe bitmap, the original bitmap is restored.  */)
  (bitmap)
     Lisp_Object bitmap;
{
  int n;

  CHECK_SYMBOL (bitmap);
  n = lookup_fringe_bitmap (bitmap);
  if (!n)
    return Qnil;

  destroy_fringe_bitmap (n);

  if (n >= MAX_STANDARD_FRINGE_BITMAPS)
    {
      Vfringe_bitmaps = Fdelq (bitmap, Vfringe_bitmaps);
      /* It would be better to remove the fringe property.  */
      Fput (bitmap, Qfringe, Qnil);
    }

  return Qnil;
}


/* Initialize bitmap bit.

   On X, we bit-swap the built-in bitmaps and reduce bitmap
   from short to char array if width is <= 8 bits.

   On MAC with big-endian CPU, we need to byte-swap each short.

   On W32 and MAC (little endian), there's no need to do this.
*/

void
init_fringe_bitmap (which, fb, once_p)
     enum fringe_bitmap_type which;
     struct fringe_bitmap *fb;
     int once_p;
{
  if (once_p || fb->dynamic)
    {
#if defined (HAVE_X_WINDOWS)
      static unsigned char swap_nibble[16]
	= { 0x0, 0x8, 0x4, 0xc,    /* 0000 1000 0100 1100 */
	    0x2, 0xa, 0x6, 0xe,    /* 0010 1010 0110 1110 */
	    0x1, 0x9, 0x5, 0xd,    /* 0001 1001 0101 1101 */
	    0x3, 0xb, 0x7, 0xf };  /* 0011 1011 0111 1111 */
      unsigned short *bits = fb->bits;
      int j;

      if (fb->width <= 8)
	{
	  unsigned char *cbits = (unsigned char *)fb->bits;
	  for (j = 0; j < fb->height; j++)
	    {
	      unsigned short b = *bits++;
	      unsigned char c;
	      c = (unsigned char)((swap_nibble[b & 0xf] << 4)
				  | (swap_nibble[(b>>4) & 0xf]));
	      *cbits++ = (c >> (8 - fb->width));
	    }
	}
      else
	{
	  for (j = 0; j < fb->height; j++)
	    {
	      unsigned short b = *bits;
	      b = (unsigned short)((swap_nibble[b & 0xf] << 12)
				   | (swap_nibble[(b>>4) & 0xf] << 8)
				   | (swap_nibble[(b>>8) & 0xf] << 4)
				   | (swap_nibble[(b>>12) & 0xf]));
	      *bits++ = (b >> (16 - fb->width));
	    }
	}
#endif /* HAVE_X_WINDOWS */

#if defined (MAC_OS) && defined (WORDS_BIG_ENDIAN)
      unsigned short *bits = fb->bits;
      int j;
      for (j = 0; j < fb->height; j++)
	{
	  unsigned short b = *bits;
	  *bits++ = ((b >> 8) & 0xff) | ((b & 0xff) << 8);
	}
#endif /* MAC_OS && WORDS_BIG_ENDIAN */
    }

  if (!once_p)
    {
      destroy_fringe_bitmap (which);

      if (rif && rif->define_fringe_bitmap)
	rif->define_fringe_bitmap (which, fb->bits, fb->height, fb->width);

      fringe_bitmaps[which] = fb;
      if (which >= max_used_fringe_bitmap)
	max_used_fringe_bitmap = which + 1;
    }
}


DEFUN ("define-fringe-bitmap", Fdefine_fringe_bitmap, Sdefine_fringe_bitmap,
       2, 5, 0,
       doc: /* Define fringe bitmap BITMAP from BITS of size HEIGHT x WIDTH.
BITMAP is a symbol or string naming the new fringe bitmap.
BITS is either a string or a vector of integers.
HEIGHT is height of bitmap.  If HEIGHT is nil, use length of BITS.
WIDTH must be an integer between 1 and 16, or nil which defaults to 8.
Optional fifth arg ALIGN may be one of `top', `center', or `bottom',
indicating the positioning of the bitmap relative to the rows where it
is used; the default is to center the bitmap.  Fourth arg may also be a
list (ALIGN PERIODIC) where PERIODIC non-nil specifies that the bitmap
should be repeated.
If BITMAP already exists, the existing definition is replaced.  */)
  (bitmap, bits, height, width, align)
     Lisp_Object bitmap, bits, height, width, align;
{
  int n, h, i, j;
  unsigned short *b;
  struct fringe_bitmap fb, *xfb;
  int fill1 = 0, fill2 = 0;

  CHECK_SYMBOL (bitmap);

  if (STRINGP (bits))
    h = SCHARS (bits);
  else if (VECTORP (bits))
    h = XVECTOR (bits)->size;
  else
    bits = wrong_type_argument (Qsequencep, bits);

  if (NILP (height))
    fb.height = h;
  else
    {
      CHECK_NUMBER (height);
      fb.height = min (XINT (height), 255);
      if (fb.height > h)
	{
	  fill1 = (fb.height - h) / 2;
	  fill2 = fb.height - h - fill1;
	}
    }

  if (NILP (width))
    fb.width = 8;
  else
    {
      CHECK_NUMBER (width);
      fb.width = min (XINT (width), 255);
    }

  fb.period = 0;
  fb.align = ALIGN_BITMAP_CENTER;

  if (CONSP (align))
    {
      Lisp_Object period = XCDR (align);
      if (CONSP (period))
	{
	  period = XCAR (period);
	  if (!NILP (period))
	    {
	      fb.period = fb.height;
	      fb.height = 255;
	    }
	}
      align = XCAR (align);
    }
  if (EQ (align, Qtop))
    fb.align = ALIGN_BITMAP_TOP;
  else if (EQ (align, Qbottom))
    fb.align = ALIGN_BITMAP_BOTTOM;
  else if (!NILP (align) && !EQ (align, Qcenter))
    error ("Bad align argument");

  n = lookup_fringe_bitmap (bitmap);
  if (!n)
    {
      if (max_used_fringe_bitmap < max_fringe_bitmaps)
	n = max_used_fringe_bitmap++;
      else
	{
	  for (n = MAX_STANDARD_FRINGE_BITMAPS;
	       n < max_fringe_bitmaps;
	       n++)
	    if (fringe_bitmaps[n] == NULL)
	      break;

	  if (n == max_fringe_bitmaps)
	    {
	      if ((max_fringe_bitmaps + 20) > MAX_FRINGE_BITMAPS)
		error ("No free fringe bitmap slots");

	      i = max_fringe_bitmaps;
	      max_fringe_bitmaps += 20;
	      fringe_bitmaps
		= ((struct fringe_bitmap **)
		   xrealloc (fringe_bitmaps, max_fringe_bitmaps * sizeof (struct fringe_bitmap *)));
	      fringe_faces
		= (Lisp_Object *) xrealloc (fringe_faces, max_fringe_bitmaps * sizeof (Lisp_Object));

	      for (; i < max_fringe_bitmaps; i++)
		{
		  fringe_bitmaps[i] = NULL;
		  fringe_faces[i] = Qnil;
		}
	    }
	}

      Vfringe_bitmaps = Fcons (bitmap, Vfringe_bitmaps);
      Fput (bitmap, Qfringe, make_number (n));
    }

  fb.dynamic = 1;

  xfb = (struct fringe_bitmap *) xmalloc (sizeof fb
					  + fb.height * BYTES_PER_BITMAP_ROW);
  fb.bits = b = (unsigned short *) (xfb + 1);
  bzero (b, fb.height);

  j = 0;
  while (j < fb.height)
    {
      for (i = 0; i < fill1 && j < fb.height; i++)
	b[j++] = 0;
      for (i = 0; i < h && j < fb.height; i++)
	{
	  Lisp_Object elt = Faref (bits, make_number (i));
	  b[j++] = NUMBERP (elt) ? XINT (elt) : 0;
	}
      for (i = 0; i < fill2 && j < fb.height; i++)
	b[j++] = 0;
    }

  *xfb = fb;

  init_fringe_bitmap (n, xfb, 0);

  return bitmap;
}

DEFUN ("set-fringe-bitmap-face", Fset_fringe_bitmap_face, Sset_fringe_bitmap_face,
       1, 2, 0,
       doc: /* Set face for fringe bitmap BITMAP to FACE.
If FACE is nil, reset face to default fringe face.  */)
  (bitmap, face)
     Lisp_Object bitmap, face;
{
  int n;
  int face_id;

  CHECK_SYMBOL (bitmap);
  n = lookup_fringe_bitmap (bitmap);
  if (!n)
    error ("Undefined fringe bitmap");

  if (!NILP (face))
    {
      face_id = lookup_derived_face (SELECTED_FRAME (), face,
				     FRINGE_FACE_ID, 1);
      if (face_id < 0)
	error ("No such face");
    }

  fringe_faces[n] = face;

  return Qnil;
}

DEFUN ("fringe-bitmaps-at-pos", Ffringe_bitmaps_at_pos, Sfringe_bitmaps_at_pos,
       0, 2, 0,
       doc: /* Return fringe bitmaps of row containing position POS in window WINDOW.
If WINDOW is nil, use selected window.  If POS is nil, use value of point
in that window.  Return value is a list (LEFT RIGHT OV), where LEFT
is the symbol for the bitmap in the left fringe (or nil if no bitmap),
RIGHT is similar for the right fringe, and OV is non-nil if there is an
overlay arrow in the left fringe.
Return nil if POS is not visible in WINDOW.  */)
  (pos, window)
     Lisp_Object pos, window;
{
  struct window *w;
  struct glyph_row *row;
  int textpos;

  if (NILP (window))
    window = selected_window;
  CHECK_WINDOW (window);
  w = XWINDOW (window);

  if (!NILP (pos))
    {
      CHECK_NUMBER_COERCE_MARKER (pos);
      textpos = XINT (pos);
    }
  else if (w == XWINDOW (selected_window))
    textpos = PT;
  else
    textpos = XMARKER (w->pointm)->charpos;

  row = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
  row = row_containing_pos (w, textpos, row, NULL, 0);
  if (row)
    return list3 (get_fringe_bitmap_name (row->left_fringe_bitmap),
		  get_fringe_bitmap_name (row->right_fringe_bitmap),
		  (row->overlay_arrow_bitmap == 0 ? Qnil
		   : row->overlay_arrow_bitmap < 0 ? Qt
		   : get_fringe_bitmap_name (row->overlay_arrow_bitmap)));
  else
    return Qnil;
}


/***********************************************************************
			    Initialization
 ***********************************************************************/

void
syms_of_fringe ()
{
  defsubr (&Sdestroy_fringe_bitmap);
  defsubr (&Sdefine_fringe_bitmap);
  defsubr (&Sfringe_bitmaps_at_pos);
  defsubr (&Sset_fringe_bitmap_face);

  DEFVAR_LISP ("overflow-newline-into-fringe", &Voverflow_newline_into_fringe,
    doc: /* *Non-nil means that newline may flow into the right fringe.
This means that display lines which are exactly as wide as the window
(not counting the final newline) will only occupy one screen line, by
showing (or hiding) the final newline in the right fringe; when point
is at the final newline, the cursor is shown in the right fringe.
If nil, also continue lines which are exactly as wide as the window.  */);
  Voverflow_newline_into_fringe = Qt;

  DEFVAR_LISP ("fringe-bitmaps", &Vfringe_bitmaps,
    doc: /* List of fringe bitmap symbols.
You must (require 'fringe) to use fringe bitmap symbols in your programs." */);
  Vfringe_bitmaps = Qnil;
}

/* Garbage collection hook */

void
mark_fringe_data ()
{
  int i;

  for (i = 0; i < max_fringe_bitmaps; i++)
    if (!NILP (fringe_faces[i]))
      mark_object (fringe_faces[i]);
}

/* Initialize this module when Emacs starts.  */

void
init_fringe_once ()
{
  enum fringe_bitmap_type bt;

  for (bt = NO_FRINGE_BITMAP + 1; bt < MAX_STANDARD_FRINGE_BITMAPS; bt++)
    init_fringe_bitmap(bt, &standard_bitmaps[bt], 1);
}

void
init_fringe ()
{
  int i;

  max_fringe_bitmaps = MAX_STANDARD_FRINGE_BITMAPS + 20;

  fringe_bitmaps
    = (struct fringe_bitmap **) xmalloc (max_fringe_bitmaps * sizeof (struct fringe_bitmap *));
  fringe_faces
    = (Lisp_Object *) xmalloc (max_fringe_bitmaps * sizeof (Lisp_Object));

  for (i = 0; i < max_fringe_bitmaps; i++)
    {
      fringe_bitmaps[i] = NULL;
      fringe_faces[i] = Qnil;
    }
}

#ifdef HAVE_NTGUI

void
w32_init_fringe ()
{
  enum fringe_bitmap_type bt;

  if (!rif)
    return;

  for (bt = NO_FRINGE_BITMAP + 1; bt < MAX_STANDARD_FRINGE_BITMAPS; bt++)
    {
      struct fringe_bitmap *fb = &standard_bitmaps[bt];
      rif->define_fringe_bitmap (bt, fb->bits, fb->height, fb->width);
    }
}

void
w32_reset_fringes ()
{
  /* Destroy row bitmaps.  */
  int bt;

  if (!rif)
    return;

  for (bt = NO_FRINGE_BITMAP + 1; bt < max_used_fringe_bitmap; bt++)
    rif->destroy_fringe_bitmap (bt);
}

#endif /* HAVE_NTGUI */

#endif /* HAVE_WINDOW_SYSTEM */

/* arch-tag: 04596920-43eb-473d-b319-82712338162d
   (do not change this comment) */
