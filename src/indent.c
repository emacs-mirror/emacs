/* Indentation functions.
   Copyright (C) 1985,86,87,88,93,94,95 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "indent.h"
#include "frame.h"
#include "window.h"
#include "termchar.h"
#include "termopts.h"
#include "disptab.h"
#include "intervals.h"
#include "region-cache.h"

/* Indentation can insert tabs if this is non-zero;
   otherwise always uses spaces */
int indent_tabs_mode;

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

#define CR 015

/* These three values memoize the current column to avoid recalculation */
/* Some things in set last_known_column_point to -1
  to mark the memoized value as invalid */
/* Last value returned by current_column */
int last_known_column;
/* Value of point when current_column was called */
int last_known_column_point;
/* Value of MODIFF when current_column was called */
int last_known_column_modified;

/* Get the display table to use for the current buffer.  */

struct Lisp_Vector *
buffer_display_table ()
{
  Lisp_Object thisbuf;

  thisbuf = current_buffer->display_table;
  if (VECTORP (thisbuf) && XVECTOR (thisbuf)->size == DISP_TABLE_SIZE)
    return XVECTOR (thisbuf);
  if (VECTORP (Vstandard_display_table)
      && XVECTOR (Vstandard_display_table)->size == DISP_TABLE_SIZE)
    return XVECTOR (Vstandard_display_table);
  return 0;
}

/* Width run cache considerations.  */

/* Return the width of character C under display table DP.  */
static int
character_width (c, dp)
     int c;
     struct Lisp_Vector *dp;
{
  Lisp_Object elt;

  /* These width computations were determined by examining the cases
     in display_text_line.  */

  /* Some characters are never handled by the display table.  */
  if (c == '\n' || c == '\t' || c == '\015')
    return 0;

  /* Everything else might be handled by the display table, if it's
     present and the element is right.  */
  else if (dp && (elt = DISP_CHAR_VECTOR (dp, c),
                  VECTORP (elt)))
    return XVECTOR (elt)->size;

  /* In the absence of display table perversities, printing characters
     have width 1.  */
  else if (c >= 040 && c < 0177)
    return 1;

  /* Everybody else (control characters, metacharacters) has other
     widths.  We could return their actual widths here, but they
     depend on things like ctl_arrow and crud like that, and they're
     not very common at all.  So we'll just claim we don't know their
     widths.  */
  else
    return 0;
}

/* Return true iff the display table DISPTAB specifies the same widths
   for characters as WIDTHTAB.  We use this to decide when to
   invalidate the buffer's width_run_cache.  */
int
disptab_matches_widthtab (disptab, widthtab)
     struct Lisp_Vector *disptab;
     struct Lisp_Vector *widthtab;
{
  int i;

  if (widthtab->size != 256)
    abort ();

  for (i = 0; i < 256; i++)
    if (character_width (i, disptab)
        != XFASTINT (widthtab->contents[i]))
      return 0;

  return 1;
}

/* Recompute BUF's width table, using the display table DISPTAB.  */
void
recompute_width_table (buf, disptab)
     struct buffer *buf;
     struct Lisp_Vector *disptab;
{
  int i;
  struct Lisp_Vector *widthtab;

  if (!VECTORP (buf->width_table))
    buf->width_table = Fmake_vector (make_number (256), make_number (0));
  widthtab = XVECTOR (buf->width_table);
  if (widthtab->size != 256)
    abort ();

  for (i = 0; i < 256; i++)
    XSETFASTINT (widthtab->contents[i], character_width (i, disptab));
}

/* Allocate or free the width run cache, as requested by the current
   state of current_buffer's cache_long_line_scans variable.  */
static void
width_run_cache_on_off ()
{
  if (NILP (current_buffer->cache_long_line_scans))
    {
      /* It should be off.  */
      if (current_buffer->width_run_cache)
        {
          free_region_cache (current_buffer->width_run_cache);
          current_buffer->width_run_cache = 0;
          current_buffer->width_table = Qnil;
        }
    }
  else
    {
      /* It should be on.  */
      if (current_buffer->width_run_cache == 0)
        {
          current_buffer->width_run_cache = new_region_cache ();
          recompute_width_table (current_buffer, buffer_display_table ());
        }
    }
}


DEFUN ("current-column", Fcurrent_column, Scurrent_column, 0, 0, 0,
  "Return the horizontal position of point.  Beginning of line is column 0.\n\
This is calculated by adding together the widths of all the displayed\n\
representations of the character between the start of the previous line\n\
and point.  (eg control characters will have a width of 2 or 4, tabs\n\
will have a variable width)\n\
Ignores finite width of frame, which means that this function may return\n\
values greater than (frame-width).\n\
Whether the line is visible (if `selective-display' is t) has no effect;\n\
however, ^M is treated as end of line when `selective-display' is t.")
  ()
{
  Lisp_Object temp;
  XSETFASTINT (temp, current_column ());
  return temp;
}

/* Cancel any recorded value of the horizontal position.  */

invalidate_current_column ()
{
  last_known_column_point = 0;
}

int
current_column ()
{
  register int col;
  register unsigned char *ptr, *stop;
  register int tab_seen;
  int post_tab;
  register int c;
  register int tab_width = XINT (current_buffer->tab_width);
  int ctl_arrow = !NILP (current_buffer->ctl_arrow);
  register struct Lisp_Vector *dp = buffer_display_table ();
  int stopchar;

  if (point == last_known_column_point
      && MODIFF == last_known_column_modified)
    return last_known_column;

  /* Make a pointer for decrementing through the chars before point.  */
  ptr = &FETCH_CHAR (point - 1) + 1;
  /* Make a pointer to where consecutive chars leave off,
     going backwards from point.  */
  if (point == BEGV)
    stop = ptr;
  else if (point <= GPT || BEGV > GPT)
    stop = BEGV_ADDR;
  else
    stop = GAP_END_ADDR;

  if (tab_width <= 0 || tab_width > 1000) tab_width = 8;

  col = 0, tab_seen = 0, post_tab = 0;

  while (1)
    {
      if (ptr == stop)
	{
	  /* We stopped either for the beginning of the buffer
	     or for the gap.  */
	  if (ptr == BEGV_ADDR)
	    break;
	  /* It was the gap.  Jump back over it.  */
	  stop = BEGV_ADDR;
	  ptr = GPT_ADDR;
	  /* Check whether that brings us to beginning of buffer.  */
	  if (BEGV >= GPT) break;
	}

      c = *--ptr;
      if (c >= 040 && c < 0177
	  && (dp == 0 || !VECTORP (DISP_CHAR_VECTOR (dp, c))))
	{
	  col++;
	}
      else if (c == '\n')
	break;
      else if (c == '\r' && EQ (current_buffer->selective_display, Qt))
	break;
      else if (c == '\t')
	{
	  if (tab_seen)
	    col = ((col + tab_width) / tab_width) * tab_width;

	  post_tab += col;
	  col = 0;
	  tab_seen = 1;
	}
      else if (dp != 0 && VECTORP (DISP_CHAR_VECTOR (dp, c)))
	col += XVECTOR (DISP_CHAR_VECTOR (dp, c))->size;
      else
	col += (ctl_arrow && c < 0200) ? 2 : 4;
    }

  if (tab_seen)
    {
      col = ((col + tab_width) / tab_width) * tab_width;
      col += post_tab;
    }

  last_known_column = col;
  last_known_column_point = point;
  last_known_column_modified = MODIFF;

  return col;
}

DEFUN ("indent-to", Findent_to, Sindent_to, 1, 2, "NIndent to column: ",
  "Indent from point with tabs and spaces until COLUMN is reached.\n\
Optional second argument MIN says always do at least MIN spaces\n\
even if that goes past COLUMN; by default, MIN is zero.")
  (col, minimum)
     Lisp_Object col, minimum;
{
  int mincol;
  register int fromcol;
  register int tab_width = XINT (current_buffer->tab_width);

  CHECK_NUMBER (col, 0);
  if (NILP (minimum))
    XSETFASTINT (minimum, 0);
  CHECK_NUMBER (minimum, 1);

  fromcol = current_column ();
  mincol = fromcol + XINT (minimum);
  if (mincol < XINT (col)) mincol = XINT (col);

  if (fromcol == mincol)
    return make_number (mincol);

  if (tab_width <= 0 || tab_width > 1000) tab_width = 8;

  if (indent_tabs_mode)
    {
      Lisp_Object n;
      XSETFASTINT (n, mincol / tab_width - fromcol / tab_width);
      if (XFASTINT (n) != 0)
	{
	  Finsert_char (make_number ('\t'), n, Qt);

	  fromcol = (mincol / tab_width) * tab_width;
	}
    }

  XSETFASTINT (col, mincol - fromcol);
  Finsert_char (make_number (' '), col, Qt);

  last_known_column = mincol;
  last_known_column_point = point;
  last_known_column_modified = MODIFF;

  XSETINT (col, mincol);
  return col;
}


DEFUN ("current-indentation", Fcurrent_indentation, Scurrent_indentation,
  0, 0, 0,
  "Return the indentation of the current line.\n\
This is the horizontal position of the character\n\
following any initial whitespace.")
  ()
{
  Lisp_Object val;

  XSETFASTINT (val, position_indentation (find_next_newline (point, -1)));
  return val;
}

position_indentation (pos)
     register int pos;
{
  register int column = 0;
  register int tab_width = XINT (current_buffer->tab_width);
  register unsigned char *p;
  register unsigned char *stop;

  if (tab_width <= 0 || tab_width > 1000) tab_width = 8;

  stop = &FETCH_CHAR (BUFFER_CEILING_OF (pos)) + 1;
  p = &FETCH_CHAR (pos);
  while (1)
    {
      while (p == stop)
	{
	  if (pos == ZV)
	    return column;
	  pos += p - &FETCH_CHAR (pos);
	  p = &FETCH_CHAR (pos);
	  stop = &FETCH_CHAR (BUFFER_CEILING_OF (pos)) + 1;
	}
      switch (*p++)
	{
	case ' ':
	  column++;
	  break;
	case '\t':
	  column += tab_width - column % tab_width;
	  break;
	default:
	  return column;
	}
    }
}

/* Test whether the line beginning at POS is indented beyond COLUMN.
   Blank lines are treated as if they had the same indentation as the
   preceding line.  */
int
indented_beyond_p (pos, column)
     int pos, column;
{
  while (pos > BEGV && FETCH_CHAR (pos) == '\n')
    pos = find_next_newline_no_quit (pos - 1, -1);
  return (position_indentation (pos) >= column);
}


DEFUN ("move-to-column", Fmove_to_column, Smove_to_column, 1, 2, 0,
  "Move point to column COLUMN in the current line.\n\
The column of a character is calculated by adding together the widths\n\
as displayed of the previous characters in the line.\n\
This function ignores line-continuation;\n\
there is no upper limit on the column number a character can have\n\
and horizontal scrolling has no effect.\n\
\n\
If specified column is within a character, point goes after that character.\n\
If it's past end of line, point goes to end of line.\n\n\
A non-nil second (optional) argument FORCE means, if the line\n\
is too short to reach column COLUMN then add spaces/tabs to get there,\n\
and if COLUMN is in the middle of a tab character, change it to spaces.")
  (column, force)
     Lisp_Object column, force;
{
  register int pos;
  register int col = current_column ();
  register int goal;
  register int end;
  register int tab_width = XINT (current_buffer->tab_width);
  register int ctl_arrow = !NILP (current_buffer->ctl_arrow);
  register struct Lisp_Vector *dp = buffer_display_table ();

  Lisp_Object val;
  int prev_col;
  int c;

  if (tab_width <= 0 || tab_width > 1000) tab_width = 8;
  CHECK_NATNUM (column, 0);
  goal = XINT (column);

 retry:
  pos = point;
  end = ZV;

  /* If we're starting past the desired column,
     back up to beginning of line and scan from there.  */
  if (col > goal)
    {
      pos = find_next_newline (pos, -1);
      col = 0;
    }

  while (col < goal && pos < end)
    {
      c = FETCH_CHAR (pos);
      if (c == '\n')
	break;
      if (c == '\r' && EQ (current_buffer->selective_display, Qt))
	break;
      pos++;
      if (c == '\t')
	{
	  prev_col = col;
	  col += tab_width;
	  col = col / tab_width * tab_width;
	}
      else if (dp != 0 && VECTORP (DISP_CHAR_VECTOR (dp, c)))
	col += XVECTOR (DISP_CHAR_VECTOR (dp, c))->size;
      else if (ctl_arrow && (c < 040 || c == 0177))
        col += 2;
      else if (c < 040 || c >= 0177)
        col += 4;
      else
	col++;
    }

  SET_PT (pos);

  /* If a tab char made us overshoot, change it to spaces
     and scan through it again.  */
  if (!NILP (force) && col > goal && c == '\t' && prev_col < goal)
    {
      int old_point;

      del_range (point - 1, point);
      Findent_to (make_number (goal), Qnil);
      old_point = point;
      Findent_to (make_number (col), Qnil);
      SET_PT (old_point);
      /* Set the last_known... vars consistently.  */
      col = goal;
    }

  /* If line ends prematurely, add space to the end.  */
  if (col < goal && !NILP (force))
    Findent_to (make_number (col = goal), Qnil);

  last_known_column = col;
  last_known_column_point = point;
  last_known_column_modified = MODIFF;

  XSETFASTINT (val, col);
  return val;
}


/* compute_motion: compute buffer posn given screen posn and vice versa */

struct position val_compute_motion;

/* Scan the current buffer forward from offset FROM, pretending that
   this is at line FROMVPOS, column FROMHPOS, until reaching buffer
   offset TO or line TOVPOS, column TOHPOS (whichever comes first),
   and return the ending buffer position and screen location.  If we
   can't hit the requested column exactly (because of a tab or other
   multi-column character), overshoot.

   WIDTH is the number of columns available to display text;
   compute_motion uses this to handle continuation lines and such.
   HSCROLL is the number of columns not being displayed at the left
   margin; this is usually taken from a window's hscroll member.
   TAB_OFFSET is the number of columns of the first tab that aren't
   being displayed, perhaps because of a continuation line or
   something.

   compute_motion returns a pointer to a struct position.  The bufpos
   member gives the buffer position at the end of the scan, and hpos
   and vpos give its cartesian location.  prevhpos is the column at
   which the character before bufpos started, and contin is non-zero
   if we reached the current line by continuing the previous.

   Note that FROMHPOS and TOHPOS should be expressed in real screen
   columns, taking HSCROLL and the truncation glyph at the left margin
   into account.  That is, beginning-of-line moves you to the hpos
   -HSCROLL + (HSCROLL > 0).

   Note that FROMHPOS and TOHPOS should be expressed in real screen
   columns, taking HSCROLL and the truncation glyph at the left margin
   into account.  That is, beginning-of-line moves you to the hpos
   -HSCROLL + (HSCROLL > 0).

   For example, to find the buffer position of column COL of line LINE
   of a certain window, pass the window's starting location as FROM
   and the window's upper-left coordinates as FROMVPOS and FROMHPOS.
   Pass the buffer's ZV as TO, to limit the scan to the end of the
   visible section of the buffer, and pass LINE and COL as TOVPOS and
   TOHPOS.

   When displaying in window w, a typical formula for WIDTH is:

	window_width - 1
	 - (has_vertical_scroll_bars
	    ? FRAME_SCROLL_BAR_COLS (XFRAME (window->frame))
	    : (window_width + window_left != frame_width))

	where
	  window_width is XFASTINT (w->width),
	  window_left is XFASTINT (w->left),
	  has_vertical_scroll_bars is
	    FRAME_HAS_VERTICAL_SCROLL_BARS (XFRAME (WINDOW_FRAME (window)))
	  and frame_width = FRAME_WIDTH (XFRAME (window->frame))

   Or you can let window_internal_width do this all for you, and write:
	window_internal_width (w) - 1

   The `-1' accounts for the continuation-line backslashes; the rest
   accounts for window borders if the window is split horizontally, and
   the scroll bars if they are turned on.  */

struct position *
compute_motion (from, fromvpos, fromhpos, to, tovpos, tohpos, width, hscroll, tab_offset, win)
     int from, fromvpos, fromhpos, to, tovpos, tohpos;
     register int width;
     int hscroll, tab_offset;
     struct window *win;
{
  register int hpos = fromhpos;
  register int vpos = fromvpos;

  register int pos;
  register int c;
  register int tab_width = XFASTINT (current_buffer->tab_width);
  register int ctl_arrow = !NILP (current_buffer->ctl_arrow);
  register struct Lisp_Vector *dp = window_display_table (win);
  int selective
    = (INTEGERP (current_buffer->selective_display)
       ? XINT (current_buffer->selective_display)
       : !NILP (current_buffer->selective_display) ? -1 : 0);
  int prev_vpos = vpos, prev_hpos = 0;
  int selective_rlen
    = (selective && dp && VECTORP (DISP_INVIS_VECTOR (dp))
       ? XVECTOR (DISP_INVIS_VECTOR (dp))->size : 0);
#ifdef USE_TEXT_PROPERTIES
  /* The next location where the `invisible' property changes */
  int next_invisible = from;
  Lisp_Object prop, position;
#endif

  /* For computing runs of characters with similar widths.
     Invariant: width_run_width is zero, or all the characters
     from width_run_start to width_run_end have a fixed width of
     width_run_width.  */
  int width_run_start = from;
  int width_run_end   = from;
  int width_run_width = 0;
  Lisp_Object *width_table;

  /* The next buffer pos where we should consult the width run cache. */
  int next_width_run = from;

  width_run_cache_on_off ();
  if (dp == buffer_display_table ())
    width_table = (VECTORP (current_buffer->width_table)
                   ? XVECTOR (current_buffer->width_table)->contents
                   : 0);
  else
    /* If the window has its own display table, we can't use the width
       run cache, because that's based on the buffer's display table.  */
    width_table = 0;

  if (tab_width <= 0 || tab_width > 1000) tab_width = 8;
  for (pos = from; pos < to; )
    {
      /* Stop if past the target screen position.  */
      if (vpos > tovpos
	  || (vpos == tovpos && hpos >= tohpos))
	break;

      prev_vpos = vpos;
      prev_hpos = hpos;

#ifdef USE_TEXT_PROPERTIES
      /* if the `invisible' property is set, we can skip to
	 the next property change */
      while (pos == next_invisible && pos < to)
	{
	  XSETFASTINT (position, pos);
	  prop = Fget_char_property (position,
				     Qinvisible,
				     Fcurrent_buffer ());
	  {
	    Lisp_Object end, limit;

	    recenter_overlay_lists (current_buffer, pos);
	    /* This is just an estimate to give reasonable
	       performance; nothing should go wrong if it is too small.  */
	    limit = Fnext_overlay_change (position);
	    if (XFASTINT (limit) > pos + 100)
	      XSETFASTINT (limit, pos + 100);
	    end = Fnext_single_property_change (position, Qinvisible,
						Fcurrent_buffer (), limit);
	    if (INTEGERP (end))
	      next_invisible = XINT (end);
	    else
	      next_invisible = to;
	    if (! NILP (prop))
	      pos = next_invisible;
	  }
	}
      if (pos >= to)
	break;
#endif

      /* Consult the width run cache to see if we can avoid inspecting
         the text character-by-character.  */
      if (current_buffer->width_run_cache && pos >= next_width_run)
        {
          int run_end;
          int common_width
            = region_cache_forward (current_buffer,
                                    current_buffer->width_run_cache,
                                    pos, &run_end);

          /* A width of zero means the character's width varies (like
             a tab), is meaningless (like a newline), or we just don't
             want to skip over it for some other reason.  */
          if (common_width != 0)
            {
              int run_end_hpos;

              /* Don't go past the final buffer posn the user
                 requested.  */
              if (run_end > to)
                run_end = to;

              run_end_hpos = hpos + (run_end - pos) * common_width;

              /* Don't go past the final horizontal position the user
                 requested.  */
              if (vpos == tovpos && run_end_hpos > tohpos)
                {
                  run_end      = pos + (tohpos - hpos) / common_width;
                  run_end_hpos = hpos + (run_end - pos) * common_width;
                }

              /* Don't go past the margin.  */
              if (run_end_hpos >= width)
                {
                  run_end      = pos + (width  - hpos) / common_width;
                  run_end_hpos = hpos + (run_end - pos) * common_width;
                }

              hpos = run_end_hpos;
              if (run_end > pos)
                prev_hpos = hpos - common_width;
              pos = run_end;
            }

          next_width_run = run_end + 1;
        }

      /* We have to scan the text character-by-character.  */
      else
        {
          c = FETCH_CHAR (pos);
          pos++;

          /* Perhaps add some info to the width_run_cache.  */
          if (current_buffer->width_run_cache)
            {
              /* Is this character part of the current run?  If so, extend
                 the run.  */
              if (pos - 1 == width_run_end
                  && width_table[c] == width_run_width)
                width_run_end = pos;

              /* The previous run is over, since this is a character at a
                 different position, or a different width.  */
              else
                {
                  /* Have we accumulated a run to put in the cache?
                     (Currently, we only cache runs of width == 1).  */
                  if (width_run_start < width_run_end
                      && width_run_width == 1)
                    know_region_cache (current_buffer,
                                       current_buffer->width_run_cache,
                                       width_run_start, width_run_end);

                  /* Start recording a new width run.  */
                  width_run_width = width_table[c];
                  width_run_start = pos - 1;
                  width_run_end = pos;
                }
            }

          if (c >= 040 && c < 0177
              && (dp == 0 || ! VECTORP (DISP_CHAR_VECTOR (dp, c))))
            hpos++;
          else if (c == '\t')
            {
              hpos += tab_width - ((hpos + tab_offset + hscroll - (hscroll > 0)
                                    /* Add tab_width here to make sure
                                       positive.  hpos can be negative
                                       after continuation but can't be
                                       less than -tab_width.  */
                                    + tab_width)
                                   % tab_width);
            }
          else if (c == '\n')
            {
              if (selective > 0 && indented_beyond_p (pos, selective))
                {
                  /* Skip any number of invisible lines all at once */
                  do
                    pos = find_before_next_newline (pos, to, 1) + 1;
                  while (pos < to
                         && indented_beyond_p (pos, selective));
                  /* Allow for the " ..." that is displayed for them. */
                  if (selective_rlen)
                    {
                      hpos += selective_rlen;
                      if (hpos >= width)
                        hpos = width;
                    }
		  --pos;
                  /* We have skipped the invis text, but not the
                     newline after.  */
                }
              else
                {
                  /* A visible line.  */
                  vpos++;
                  hpos = 0;
                  hpos -= hscroll;
                  /* Count the truncation glyph on column 0 */
                  if (hscroll > 0)
                    hpos++;
                  tab_offset = 0;
                }
            }
          else if (c == CR && selective < 0)
            {
              /* In selective display mode,
                 everything from a ^M to the end of the line is invisible.
                 Stop *before* the real newline.  */
              pos = find_before_next_newline (pos, to, 1);
              /* Allow for the " ..." that is displayed for them. */
              if (selective_rlen)
                {
                  hpos += selective_rlen;
                  if (hpos >= width)
                    hpos = width;
                }
            }
          else if (dp != 0 && VECTORP (DISP_CHAR_VECTOR (dp, c)))
            hpos += XVECTOR (DISP_CHAR_VECTOR (dp, c))->size;
          else
            hpos += (ctl_arrow && c < 0200) ? 2 : 4;
        }

      /* Handle right margin.  */
      if (hpos >= width
	  && (hpos > width
	      || (pos < ZV
		  && FETCH_CHAR (pos) != '\n')))
	{
	  if (vpos > tovpos
	      || (vpos == tovpos && hpos >= tohpos))
	    break;
	  if (hscroll
	      || (truncate_partial_width_windows
		  && width + 1 < FRAME_WIDTH (XFRAME (WINDOW_FRAME (win))))
	      || !NILP (current_buffer->truncate_lines))
	    {
	      /* Truncating: skip to newline.  */
              pos = find_before_next_newline (pos, to, 1);
	      hpos = width;
	    }
	  else
	    {
	      /* Continuing.  */
	      vpos++;
	      hpos -= width;
	      tab_offset += width;
	    }

	}
    }

  /* Remember any final width run in the cache.  */
  if (current_buffer->width_run_cache
      && width_run_width == 1
      && width_run_start < width_run_end)
    know_region_cache (current_buffer, current_buffer->width_run_cache,
                       width_run_start, width_run_end);

  val_compute_motion.bufpos = pos;
  val_compute_motion.hpos = hpos;
  val_compute_motion.vpos = vpos;
  val_compute_motion.prevhpos = prev_hpos;

  /* Nonzero if have just continued a line */
  val_compute_motion.contin
    = (pos != from
       && (val_compute_motion.vpos != prev_vpos)
       && c != '\n');

  return &val_compute_motion;
}

#if 0 /* The doc string is too long for some compilers,
	 but make-docfile can find it in this comment.  */
DEFUN ("compute-motion", Ffoo, Sfoo, 7, 7, 0,
  "Scan through the current buffer, calculating screen position.\n\
Scan the current buffer forward from offset FROM,\n\
assuming it is at position FROMPOS--a cons of the form (HPOS . VPOS)--\n\
to position TO or position TOPOS--another cons of the form (HPOS . VPOS)--\n\
and return the ending buffer position and screen location.\n\
\n\
There are three additional arguments:\n\
\n\
WIDTH is the number of columns available to display text;\n\
this affects handling of continuation lines.\n\
This is usually the value returned by `window-width', less one (to allow\n\
for the continuation glyph).\n\
\n\
OFFSETS is either nil or a cons cell (HSCROLL . TAB-OFFSET).\n\
HSCROLL is the number of columns not being displayed at the left\n\
margin; this is usually taken from a window's hscroll member.\n\
TAB-OFFSET is the number of columns of the first tab that aren't\n\
being displayed, perhaps because the line was continued within it.\n\
If OFFSETS is nil, HSCROLL and TAB-OFFSET are assumed to be zero.\n\
\n\
WINDOW is the window to operate on.  Currently this is used only to\n\
find the display table.  It does not matter what buffer WINDOW displays;\n\
`compute-motion' always operates on the current buffer.\n\
\n\
The value is a list of five elements:\n\
  (POS HPOS VPOS PREVHPOS CONTIN)\n\
POS is the buffer position where the scan stopped.\n\
VPOS is the vertical position where the scan stopped.\n\
HPOS is the horizontal position where the scan stopped.\n\
\n\
PREVHPOS is the horizontal position one character back from POS.\n\
CONTIN is t if a line was continued after (or within) the previous character.\n\
\n\
For example, to find the buffer position of column COL of line LINE\n\
of a certain window, pass the window's starting location as FROM\n\
and the window's upper-left coordinates as FROMPOS.\n\
Pass the buffer's (point-max) as TO, to limit the scan to the end of the\n\
visible section of the buffer, and pass LINE and COL as TOPOS.")
  (from, frompos, to, topos, width, offsets, window)
#endif

DEFUN ("compute-motion", Fcompute_motion, Scompute_motion, 7, 7, 0,
  0)
  (from, frompos, to, topos, width, offsets, window)
     Lisp_Object from, frompos, to, topos;
     Lisp_Object width, offsets, window;
{
  Lisp_Object bufpos, hpos, vpos, prevhpos, contin;
  struct position *pos;
  int hscroll, tab_offset;

  CHECK_NUMBER_COERCE_MARKER (from, 0);
  CHECK_CONS (frompos, 0);
  CHECK_NUMBER (XCONS (frompos)->car, 0);
  CHECK_NUMBER (XCONS (frompos)->cdr, 0);
  CHECK_NUMBER_COERCE_MARKER (to, 0);
  CHECK_CONS (topos, 0);
  CHECK_NUMBER (XCONS (topos)->car, 0);
  CHECK_NUMBER (XCONS (topos)->cdr, 0);
  CHECK_NUMBER (width, 0);
  if (!NILP (offsets))
    {
      CHECK_CONS (offsets, 0);
      CHECK_NUMBER (XCONS (offsets)->car, 0);
      CHECK_NUMBER (XCONS (offsets)->cdr, 0);
      hscroll = XINT (XCONS (offsets)->car);
      tab_offset = XINT (XCONS (offsets)->cdr);
    }
  else
    hscroll = tab_offset = 0;

  if (NILP (window))
    window = Fselected_window ();
  else
    CHECK_LIVE_WINDOW (window, 0);

  pos = compute_motion (XINT (from), XINT (XCONS (frompos)->cdr),
			XINT (XCONS (frompos)->car),
			XINT (to), XINT (XCONS (topos)->cdr),
			XINT (XCONS (topos)->car),
			XINT (width), hscroll, tab_offset,
			XWINDOW (window));

  XSETFASTINT (bufpos, pos->bufpos);
  XSETINT (hpos, pos->hpos);
  XSETINT (vpos, pos->vpos);
  XSETINT (prevhpos, pos->prevhpos);

  return Fcons (bufpos,
		Fcons (hpos,
		       Fcons (vpos,
			      Fcons (prevhpos,
				     Fcons (pos->contin ? Qt : Qnil, Qnil)))));

}

/* Return the column of position POS in window W's buffer.
   The result is rounded down to a multiple of the internal width of W.
   This is the amount of indentation of position POS
   that is not visible in its horizontal position in the window.  */

int
pos_tab_offset (w, pos)
     struct window *w;
     register int pos;
{
  int opoint = PT;
  int col;
  int width = window_internal_width (w) - 1;

  if (pos == BEGV || FETCH_CHAR (pos - 1) == '\n')
    return 0;
  TEMP_SET_PT (pos);
  col = current_column ();
  TEMP_SET_PT (opoint);
  return col - (col % width);
}


/* Fvertical_motion and vmotion */
struct position val_vmotion;

struct position *
vmotion (from, vtarget, width, hscroll, window)
     register int from, vtarget, width;
     int hscroll;
     Lisp_Object window;
{
  struct position pos;
  /* vpos is cumulative vertical position, changed as from is changed */
  register int vpos = 0;
  Lisp_Object prevline;
  register int first;
  int lmargin = hscroll > 0 ? 1 - hscroll : 0;
  int selective
    = (INTEGERP (current_buffer->selective_display)
       ? XINT (current_buffer->selective_display)
       : !NILP (current_buffer->selective_display) ? -1 : 0);
  /* The omission of the clause
         && marker_position (XWINDOW (window)->start) == BEG
     here is deliberate; I think we want to measure from the prompt
     position even if the minibuffer window has scrolled.  */
  int start_hpos = (EQ (window, minibuf_window) ? minibuf_prompt_width : 0);

 retry:
  if (vtarget > vpos)
    {
      /* Moving downward is simple, but must calculate from beg of line
	 to determine hpos of starting point */
      if (from > BEGV && FETCH_CHAR (from - 1) != '\n')
	{
	  XSETFASTINT (prevline, find_next_newline_no_quit (from, -1));
	  while (XFASTINT (prevline) > BEGV
		 && ((selective > 0
		      && indented_beyond_p (XFASTINT (prevline), selective))
#ifdef USE_TEXT_PROPERTIES
		     /* watch out for newlines with `invisible' property */
		     || ! NILP (Fget_char_property (prevline,
						    Qinvisible,
						    window))
#endif
		 ))
	    XSETFASTINT (prevline,
			 find_next_newline_no_quit (XFASTINT (prevline) - 1,
						    -1));
	  pos = *compute_motion (XFASTINT (prevline), 0,
				 lmargin + (XFASTINT (prevline) == 1
					    ? start_hpos : 0),
				 from, 1 << (INTBITS - 2), 0,
				 width, hscroll, 0, XWINDOW (window));
	}
      else
	{
	  pos.hpos = lmargin + (from == 1 ? start_hpos : 0);
	  pos.vpos = 0;
	}
      return compute_motion (from, vpos, pos.hpos,
			     ZV, vtarget, - (1 << (INTBITS - 2)),
			     width, hscroll, pos.vpos * width,
			     XWINDOW (window));
    }

  /* To move upward, go a line at a time until
     we have gone at least far enough */

  first = 1;

  while ((vpos > vtarget || first) && from > BEGV)
    {
      XSETFASTINT (prevline, from);
      while (1)
	{
	  XSETFASTINT (prevline,
		       find_next_newline_no_quit (XFASTINT (prevline) - 1,
						  -1));
	  if (XFASTINT (prevline) == BEGV
	      || ((selective <= 0
		   || ! indented_beyond_p (XFASTINT (prevline), selective))
#ifdef USE_TEXT_PROPERTIES
		  /* watch out for newlines with `invisible' property */
		  && NILP (Fget_char_property (prevline, Qinvisible, window))
#endif
		  ))
	    break;
	}
      pos = *compute_motion (XFASTINT (prevline), 0,
			     lmargin + (XFASTINT (prevline) == 1
					? start_hpos : 0),
			     from, 1 << (INTBITS - 2), 0,
			     width, hscroll, 0, XWINDOW (window));
      vpos -= pos.vpos;
      first = 0;
      from = XFASTINT (prevline);
    }

  /* If we made exactly the desired vertical distance,
     or if we hit beginning of buffer,
     return point found */
  if (vpos >= vtarget)
    {
      val_vmotion.bufpos = from;
      val_vmotion.vpos = vpos;
      val_vmotion.hpos = lmargin;
      val_vmotion.contin = 0;
      val_vmotion.prevhpos = 0;
      return &val_vmotion;
    }

  /* Otherwise find the correct spot by moving down */
  goto retry;
}

DEFUN ("vertical-motion", Fvertical_motion, Svertical_motion, 1, 2, 0,
  "Move to start of screen line LINES lines down.\n\
If LINES is negative, this is moving up.\n\
\n\
The optional second argument WINDOW specifies the window to use for\n\
parameters such as width, horizontal scrolling, and so on.\n\
the default is the selected window.\n\
It does not matter what buffer is displayed in WINDOW.\n\
`vertical-motion' always uses the current buffer.\n\
\n\
Sets point to position found; this may be start of line\n\
or just the start of a continuation line.\n\
Returns number of lines moved; may be closer to zero than LINES\n\
if beginning or end of buffer was reached.")
  (lines, window)
     Lisp_Object lines, window;
{
  struct position pos;
  register struct window *w;

  CHECK_NUMBER (lines, 0);
  if (! NILP (window))
    CHECK_WINDOW (window, 0);
  else
    window = selected_window;

  w = XWINDOW (window);

  pos = *vmotion (point, XINT (lines), window_internal_width (w) - 1,
		  /* Not XFASTINT since perhaps could be negative */
		  XINT (w->hscroll), window);

  SET_PT (pos.bufpos);
  return make_number (pos.vpos);
}

/* file's initialization.  */

syms_of_indent ()
{
  DEFVAR_BOOL ("indent-tabs-mode", &indent_tabs_mode,
    "*Indentation can insert tabs if this is non-nil.\n\
Setting this variable automatically makes it local to the current buffer.");
  indent_tabs_mode = 1;

  defsubr (&Scurrent_indentation);
  defsubr (&Sindent_to);
  defsubr (&Scurrent_column);
  defsubr (&Smove_to_column);
  defsubr (&Svertical_motion);
  defsubr (&Scompute_motion);
}
