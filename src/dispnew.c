/* Updating of data structures for redisplay.
   Copyright (C) 1985, 86, 87, 88, 93, 94, 95 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <signal.h>

#include <config.h>

#include <stdio.h>
#include <ctype.h>

#include "lisp.h"
#include "termchar.h"
#include "termopts.h"
#include "termhooks.h"
#include "cm.h"
#include "dispextern.h"
#include "buffer.h"
#include "frame.h"
#include "window.h"
#include "commands.h"
#include "disptab.h"
#include "indent.h"
#include "intervals.h"

#include "systty.h"

#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#endif	/* HAVE_X_WINDOWS */

/* Include systime.h after xterm.h to avoid double inclusion of time.h. */
#include "systime.h"

#include <errno.h>

#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))

/* Get number of chars of output now in the buffer of a stdio stream.
   This ought to be built in in stdio, but it isn't.
   Some s- files override this because their stdio internals differ.  */
#ifdef __GNU_LIBRARY__
/* The s- file might have overridden the definition with one that works for
   the system's C library.  But we are using the GNU C library, so this is
   the right definition for every system.  */
#ifdef GNU_LIBRARY_PENDING_OUTPUT_COUNT
#define PENDING_OUTPUT_COUNT GNU_LIBRARY_PENDING_OUTPUT_COUNT
#else
#undef	PENDING_OUTPUT_COUNT
#define	PENDING_OUTPUT_COUNT(FILE) ((FILE)->__bufp - (FILE)->__buffer)
#endif
#else /* not __GNU_LIBRARY__ */
#ifndef PENDING_OUTPUT_COUNT
#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_ptr - (FILE)->_base)
#endif
#endif

static void change_frame_size_1 ();

/* Nonzero upon entry to redisplay means do not assume anything about
   current contents of actual terminal frame; clear and redraw it.  */

int frame_garbaged;

/* Nonzero means last display completed.  Zero means it was preempted. */

int display_completed;

/* Lisp variable visible-bell; enables use of screen-flash
   instead of audible bell.  */

int visible_bell;

/* Invert the color of the whole frame, at a low level.  */

int inverse_video;

/* Line speed of the terminal.  */

int baud_rate;

/* nil or a symbol naming the window system under which emacs is
   running ('x is the only current possibility).  */

Lisp_Object Vwindow_system;

/* Version number of X windows: 10, 11 or nil.  */
Lisp_Object Vwindow_system_version;

/* Vector of glyph definitions.  Indexed by glyph number,
   the contents are a string which is how to output the glyph.

   If Vglyph_table is nil, a glyph is output by using its low 8 bits
   as a character code.  */

Lisp_Object Vglyph_table;

/* Display table to use for vectors that don't specify their own.  */

Lisp_Object Vstandard_display_table;

/* Nonzero means reading single-character input with prompt
   so put cursor on minibuffer after the prompt.
   positive means at end of text in echo area;
   negative means at beginning of line.  */
int cursor_in_echo_area;

/* The currently selected frame.
   In a single-frame version, this variable always holds the address of
   the_only_frame.  */

FRAME_PTR selected_frame;

/* A frame which is not just a minibuffer, or 0 if there are no such
   frames.  This is usually the most recent such frame that was
   selected.  In a single-frame version, this variable always holds
   the address of the_only_frame.  */
FRAME_PTR last_nonminibuf_frame;

/* In a single-frame version, the information that would otherwise
   exist inside frame objects lives in the following structure instead.

   NOTE: the_only_frame is not checked for garbage collection; don't
   store collectible objects in any of its fields!

   You're not/The only frame in town/...  */

#ifndef MULTI_FRAME
struct frame the_only_frame;
#endif

/* This is a vector, made larger whenever it isn't large enough,
   which is used inside `update_frame' to hold the old contents
   of the FRAME_PHYS_LINES of the frame being updated.  */
struct frame_glyphs **ophys_lines;
/* Length of vector currently allocated.  */
int ophys_lines_length;

FILE *termscript;	/* Stdio stream being used for copy of all output.  */

struct cm Wcm;		/* Structure for info on cursor positioning */

extern short ospeed;	/* Output speed (from sg_ospeed) */

int delayed_size_change;  /* 1 means SIGWINCH happened when not safe.  */

#ifdef MULTI_FRAME

DEFUN ("redraw-frame", Fredraw_frame, Sredraw_frame, 1, 1, 0,
  "Clear frame FRAME and output again what is supposed to appear on it.")
  (frame)
     Lisp_Object frame;
{
  FRAME_PTR f;

  CHECK_LIVE_FRAME (frame, 0);
  f = XFRAME (frame);
  update_begin (f);
  /*  set_terminal_modes (); */
  clear_frame ();
  clear_frame_records (f);
  update_end (f);
  fflush (stdout);
  windows_or_buffers_changed++;
  /* Mark all windows as INaccurate,
     so that every window will have its redisplay done.  */
  mark_window_display_accurate (FRAME_ROOT_WINDOW (f), 0);
  f->garbaged = 0;
  return Qnil;
}

redraw_frame (f)
     FRAME_PTR f;
{
  Lisp_Object frame;
  XSETFRAME (frame, f);
  Fredraw_frame (frame);
}

#else

DEFUN ("redraw-frame", Fredraw_frame, Sredraw_frame, 1, 1, 0,
  /* Don't confuse make-docfile by having two doc strings for this function.
     make-docfile does not pay attention to #if, for good reason!  */
  0)
  (frame)
     Lisp_Object frame;
{
  update_begin (0);
  set_terminal_modes ();
  clear_frame ();
  update_end (0);
  fflush (stdout);
  clear_frame_records (0);
  windows_or_buffers_changed++;
  /* Mark all windows as INaccurate,
     so that every window will have its redisplay done.  */
  mark_window_display_accurate (FRAME_ROOT_WINDOW (0), 0);
  return Qnil;
}

#endif

DEFUN ("redraw-display", Fredraw_display, Sredraw_display, 0, 0, "",
  "Clear and redisplay all visible frames.")
  ()
{
  Lisp_Object tail, frame;

  FOR_EACH_FRAME (tail, frame)
    if (FRAME_VISIBLE_P (XFRAME (frame)))
      Fredraw_frame (frame);

  return Qnil;
}

/* This is used when frame_garbaged is set.
   Redraw the individual frames marked as garbaged.  */

void
redraw_garbaged_frames ()
{
  Lisp_Object tail, frame;

  FOR_EACH_FRAME (tail, frame)
    if (FRAME_VISIBLE_P (XFRAME (frame))
	&& FRAME_GARBAGED_P (XFRAME (frame)))
      Fredraw_frame (frame);
}


static struct frame_glyphs *
make_frame_glyphs (frame, empty)
     register FRAME_PTR frame;
     int empty;
{
  register int i;
  register width = FRAME_WIDTH (frame);
  register height = FRAME_HEIGHT (frame);
  register struct frame_glyphs *new
    = (struct frame_glyphs *) xmalloc (sizeof (struct frame_glyphs));

  SET_GLYPHS_FRAME (new, frame);
  new->height = height;
  new->width = width;
  new->used = (int *) xmalloc (height * sizeof (int));
  new->glyphs = (GLYPH **) xmalloc (height * sizeof (GLYPH *));
  new->charstarts = (int **) xmalloc (height * sizeof (int *));
  new->highlight = (char *) xmalloc (height * sizeof (char));
  new->enable = (char *) xmalloc (height * sizeof (char));
  bzero (new->enable, height * sizeof (char));
  new->bufp = (int *) xmalloc (height * sizeof (int));

#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (frame))
    {
      new->top_left_x = (short *) xmalloc (height * sizeof (short));
      new->top_left_y = (short *) xmalloc (height * sizeof (short));
      new->pix_width = (short *) xmalloc (height * sizeof (short));
      new->pix_height = (short *) xmalloc (height * sizeof (short));
      new->max_ascent = (short *) xmalloc (height * sizeof (short));
    }
#endif

  if (empty)
    {
      /* Make the buffer used by decode_mode_spec.  This buffer is also
         used as temporary storage when updating the frame.  See scroll.c. */
      unsigned int total_glyphs = (width + 2) * sizeof (GLYPH);
      unsigned int total_charstarts = (width + 2) * sizeof (int);

      new->total_contents = (GLYPH *) xmalloc (total_glyphs);
      bzero (new->total_contents, total_glyphs);

      new->total_charstarts = (int *) xmalloc (total_charstarts);
      bzero (new->total_charstarts, total_glyphs);
    }
  else
    {
      unsigned int total_glyphs = height * (width + 2) * sizeof (GLYPH);

      new->total_contents = (GLYPH *) xmalloc (total_glyphs);
      bzero (new->total_contents, total_glyphs);
      for (i = 0; i < height; i++)
	new->glyphs[i] = new->total_contents + i * (width + 2) + 1;

      if (!FRAME_TERMCAP_P (frame))
	{
	  unsigned int total_charstarts = height * (width + 2) * sizeof (int);

	  new->total_charstarts = (int *) xmalloc (total_charstarts);
	  bzero (new->total_charstarts, total_charstarts);
	  for (i = 0; i < height; i++)
	    new->charstarts[i] = new->total_charstarts + i * (width + 2) + 1;
	}
      else
	{
	  /* Without a window system, we don't really need charstarts.
	     So use a small amount of space to make enough data structure
	     to prevent crashes in display_text_line.  */
	  new->total_charstarts = (int *) xmalloc ((width + 2) * sizeof (int));
	  for (i = 0; i < height; i++)
	    new->charstarts[i] = new->total_charstarts;
	}
    }

  return new;
}

void
free_frame_glyphs (frame, glyphs)
     FRAME_PTR frame;
     struct frame_glyphs *glyphs;
{
  if (glyphs->total_contents)
    xfree (glyphs->total_contents);
  if (glyphs->total_charstarts)
    xfree (glyphs->total_charstarts);

  xfree (glyphs->used);
  xfree (glyphs->glyphs);
  xfree (glyphs->highlight);
  xfree (glyphs->enable);
  xfree (glyphs->bufp);
  if (glyphs->charstarts)
    xfree (glyphs->charstarts);

#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (frame))
    {
      xfree (glyphs->top_left_x);
      xfree (glyphs->top_left_y);
      xfree (glyphs->pix_width);
      xfree (glyphs->pix_height);
      xfree (glyphs->max_ascent);
    }
#endif

  xfree (glyphs);
}

void
remake_frame_glyphs (frame)
     FRAME_PTR frame;
{
  if (FRAME_CURRENT_GLYPHS (frame))
    free_frame_glyphs (frame, FRAME_CURRENT_GLYPHS (frame));
  if (FRAME_DESIRED_GLYPHS (frame))
    free_frame_glyphs (frame, FRAME_DESIRED_GLYPHS (frame));
  if (FRAME_TEMP_GLYPHS (frame))
    free_frame_glyphs (frame, FRAME_TEMP_GLYPHS (frame));

  if (FRAME_MESSAGE_BUF (frame))
    {
      /* Reallocate the frame's message buffer; remember that
	 echo_area_glyphs may be pointing here.  */
      char *old_message_buf = FRAME_MESSAGE_BUF (frame);

      FRAME_MESSAGE_BUF (frame)
	= (char *) xrealloc (FRAME_MESSAGE_BUF (frame),
			     FRAME_WIDTH (frame) + 1);

      if (echo_area_glyphs == old_message_buf)
	echo_area_glyphs = FRAME_MESSAGE_BUF (frame);
      if (previous_echo_glyphs == old_message_buf)
	previous_echo_glyphs = FRAME_MESSAGE_BUF (frame);
    }
  else
    FRAME_MESSAGE_BUF (frame)
      = (char *) xmalloc (FRAME_WIDTH (frame) + 1);

  FRAME_CURRENT_GLYPHS (frame) = make_frame_glyphs (frame, 0);
  FRAME_DESIRED_GLYPHS (frame) = make_frame_glyphs (frame, 0);
  FRAME_TEMP_GLYPHS (frame) = make_frame_glyphs (frame, 1);
  if (! FRAME_TERMCAP_P (frame) || frame == selected_frame)
    SET_FRAME_GARBAGED (frame);
}

/* Return the hash code of contents of line VPOS in frame-matrix M.  */

static int
line_hash_code (m, vpos)
     register struct frame_glyphs *m;
     int vpos;
{
  register GLYPH *body, *end;
  register int h = 0;

  if (!m->enable[vpos])
    return 0;

  /* Give all highlighted lines the same hash code
     so as to encourage scrolling to leave them in place.  */
  if (m->highlight[vpos])
    return -1;

  body = m->glyphs[vpos];

  if (must_write_spaces)
    while (1)
      {
	GLYPH g = *body++;

	if (g == 0)
	  break;
	h = (((h << 4) + (h >> 24)) & 0x0fffffff) + g - SPACEGLYPH;
      }
  else
    while (1)
      {
	GLYPH g = *body++;

	if (g == 0)
	  break;
	h = (((h << 4) + (h >> 24)) & 0x0fffffff) + g;
      }

  if (h)
    return h;
  return 1;
}

/* Return number of characters in line in M at vpos VPOS,
   except don't count leading and trailing spaces
   unless the terminal requires those to be explicitly output.  */

static unsigned int
line_draw_cost (m, vpos)
     struct frame_glyphs *m;
     int vpos;
{
  register GLYPH *beg = m->glyphs[vpos];
  register GLYPH *end = m->glyphs[vpos] + m->used[vpos];
  register int i;
  register int tlen = GLYPH_TABLE_LENGTH;
  register Lisp_Object *tbase = GLYPH_TABLE_BASE;

  /* Ignore trailing and leading spaces if we can.  */
  if (!must_write_spaces)
    {
      while ((end != beg) && (*end == SPACEGLYPH))
	--end;
      if (end == beg)
	return (0); /* All blank line. */

      while (*beg == SPACEGLYPH)
	++beg;
    }

  /* If we don't have a glyph-table, each glyph is one character,
     so return the number of glyphs.  */
  if (tbase == 0)
    return end - beg;

  /* Otherwise, scan the glyphs and accumulate their total size in I.  */
  i = 0;
  while ((beg <= end) && *beg)
    {
      register GLYPH g = *beg++;

      if (GLYPH_SIMPLE_P (tbase, tlen, g))
	i += 1;
      else
	i += GLYPH_LENGTH (tbase, g);
    }
  return i;
}

/* The functions on this page are the interface from xdisp.c to redisplay.

   The only other interface into redisplay is through setting
   FRAME_CURSOR_X (frame) and FRAME_CURSOR_Y (frame)
   and SET_FRAME_GARBAGED (frame).  */

/* cancel_line eliminates any request to display a line at position `vpos' */

cancel_line (vpos, frame)
     int vpos;
     register FRAME_PTR frame;
{
  FRAME_DESIRED_GLYPHS (frame)->enable[vpos] = 0;
}

clear_frame_records (frame)
     register FRAME_PTR frame;
{
  bzero (FRAME_CURRENT_GLYPHS (frame)->enable, FRAME_HEIGHT (frame));
}

/* Clear out all display lines for a coming redisplay.  */

void
init_desired_glyphs (frame)
     register FRAME_PTR frame;
{
  register struct frame_glyphs *desired_glyphs = FRAME_DESIRED_GLYPHS (frame);
  int vpos;
  int height = FRAME_HEIGHT (frame);

  for (vpos = 0; vpos < height; vpos++)
    desired_glyphs->enable[vpos] = 0;
}

/* Prepare to display on line VPOS starting at HPOS within it.  */

void
get_display_line (frame, vpos, hpos)
     register FRAME_PTR frame;
     int vpos;
     register int hpos;
{
  register struct frame_glyphs *glyphs;
  register struct frame_glyphs *desired_glyphs = FRAME_DESIRED_GLYPHS (frame);
  register GLYPH *p;

  if (vpos < 0)
    abort ();

  if ((desired_glyphs->enable[vpos]) && desired_glyphs->used[vpos] > hpos)
    abort ();

  if (! desired_glyphs->enable[vpos])
    {
      desired_glyphs->used[vpos] = 0;
      desired_glyphs->highlight[vpos] = 0;
      desired_glyphs->enable[vpos] = 1;
    }

  if (hpos > desired_glyphs->used[vpos])
    {
      GLYPH *g = desired_glyphs->glyphs[vpos] + desired_glyphs->used[vpos];
      GLYPH *end = desired_glyphs->glyphs[vpos] + hpos;

      desired_glyphs->used[vpos] = hpos;
      while (g != end)
	*g++ = SPACEGLYPH;
    }
}

/* Like bcopy except never gets confused by overlap.  */

void
safe_bcopy (from, to, size)
     char *from, *to;
     int size;
{
  if (size <= 0 || from == to)
    return;

  /* If the source and destination don't overlap, then bcopy can
     handle it.  If they do overlap, but the destination is lower in
     memory than the source, we'll assume bcopy can handle that.  */
  if (to < from || from + size <= to)
    bcopy (from, to, size);

  /* Otherwise, we'll copy from the end.  */
  else
    {
      register char *endf = from + size;
      register char *endt = to + size;

      /* If TO - FROM is large, then we should break the copy into
	 nonoverlapping chunks of TO - FROM bytes each.  However, if
	 TO - FROM is small, then the bcopy function call overhead
	 makes this not worth it.  The crossover point could be about
	 anywhere.  Since I don't think the obvious copy loop is too
	 bad, I'm trying to err in its favor.  */
      if (to - from < 64)
	{
	  do
	    *--endt = *--endf;
	  while (endf != from);
	}
      else
	{
	  for (;;)
	    {
	      endt -= (to - from);
	      endf -= (to - from);

	      if (endt < to)
		break;

	      bcopy (endf, endt, to - from);
	    }

	  /* If SIZE wasn't a multiple of TO - FROM, there will be a
	     little left over.  The amount left over is
	     (endt + (to - from)) - to, which is endt - from.  */
	  bcopy (from, to, endt - from);
	}
    }
}     

/* Rotate a vector of SIZE bytes right, by DISTANCE bytes.
   DISTANCE may be negative.  */

static void
rotate_vector (vector, size, distance)
     char *vector;
     int size;
     int distance;
{
  char *temp = (char *) alloca (size);

  if (distance < 0)
    distance += size;

  bcopy (vector, temp + distance, size - distance);
  bcopy (vector + size - distance, temp, distance);
  bcopy (temp, vector, size);
}

/* Scroll lines from vpos FROM up to but not including vpos END
   down by AMOUNT lines (AMOUNT may be negative).
   Returns nonzero if done, zero if terminal cannot scroll them.  */

int
scroll_frame_lines (frame, from, end, amount, newpos)
     register FRAME_PTR frame;
     int from, end, amount, newpos;
{
  register int i;
  register struct frame_glyphs *current_frame
    = FRAME_CURRENT_GLYPHS (frame);
  int pos_adjust;
  int width = FRAME_WIDTH (frame);

  if (!line_ins_del_ok)
    return 0;

  if (amount == 0)
    return 1;

  if (amount > 0)
    {
      update_begin (frame);
      set_terminal_window (end + amount);
      if (!scroll_region_ok)
	ins_del_lines (end, -amount);
      ins_del_lines (from, amount);
      set_terminal_window (0);

      rotate_vector (current_frame->glyphs + from,
		     sizeof (GLYPH *) * (end + amount - from),
		     amount * sizeof (GLYPH *));

      rotate_vector (current_frame->charstarts + from,
		     sizeof (int *) * (end + amount - from),
		     amount * sizeof (int *));

      safe_bcopy (current_frame->used + from,
		  current_frame->used + from + amount,
		  (end - from) * sizeof current_frame->used[0]);

      safe_bcopy (current_frame->highlight + from,
		  current_frame->highlight + from + amount,
		  (end - from) * sizeof current_frame->highlight[0]);

      safe_bcopy (current_frame->enable + from,
		  current_frame->enable + from + amount,
		  (end - from) * sizeof current_frame->enable[0]);

      /* Adjust the lines by an amount
	 that puts the first of them at NEWPOS.  */
      pos_adjust = newpos - current_frame->charstarts[from + amount][0];

      /* Offset each char position in the charstarts lines we moved
	 by pos_adjust.  */
      for (i = from + amount; i < end + amount; i++)
	{
	  int *line = current_frame->charstarts[i];
	  int col;
	  for (col = 0; col < width; col++)
	    if (line[col] > 0)
	      line[col] += pos_adjust;
	}
      for (i = from; i < from + amount; i++)
	{
	  int *line = current_frame->charstarts[i];
	  int col;
	  line[0] = -1;
	  for (col = 0; col < width; col++)
	    line[col] = 0;
	}

      /* Mark the lines made empty by scrolling as enabled, empty and
	 normal video.  */
      bzero (current_frame->used + from,
	     amount * sizeof current_frame->used[0]);
      bzero (current_frame->highlight + from,
	     amount * sizeof current_frame->highlight[0]);
      for (i = from; i < from + amount; i++)
	{
	  current_frame->glyphs[i][0] = '\0';
	  current_frame->charstarts[i][0] = -1;
	  current_frame->enable[i] = 1;
	}

      safe_bcopy (current_frame->bufp + from,
		  current_frame->bufp + from + amount,
		  (end - from) * sizeof current_frame->bufp[0]);

#ifdef HAVE_X_WINDOWS
      if (FRAME_X_P (frame))
	{
	  safe_bcopy (current_frame->top_left_x + from,
		      current_frame->top_left_x + from + amount,
		      (end - from) * sizeof current_frame->top_left_x[0]);

	  safe_bcopy (current_frame->top_left_y + from,
		      current_frame->top_left_y + from + amount,
		      (end - from) * sizeof current_frame->top_left_y[0]);

	  safe_bcopy (current_frame->pix_width + from,
		      current_frame->pix_width + from + amount,
		      (end - from) * sizeof current_frame->pix_width[0]);

	  safe_bcopy (current_frame->pix_height + from,
		      current_frame->pix_height + from + amount,
		      (end - from) * sizeof current_frame->pix_height[0]);

	  safe_bcopy (current_frame->max_ascent + from,
		      current_frame->max_ascent + from + amount,
		      (end - from) * sizeof current_frame->max_ascent[0]);
	}
#endif				/* HAVE_X_WINDOWS */

      update_end (frame);
    }
  if (amount < 0)
    {
      update_begin (frame);
      set_terminal_window (end);
      ins_del_lines (from + amount, amount);
      if (!scroll_region_ok)
	ins_del_lines (end + amount, -amount);
      set_terminal_window (0);

      rotate_vector (current_frame->glyphs + from + amount,
		     sizeof (GLYPH *) * (end - from - amount),
		     amount * sizeof (GLYPH *));

      rotate_vector (current_frame->charstarts + from + amount,
		     sizeof (int *) * (end - from - amount),
		     amount * sizeof (int *));

      safe_bcopy (current_frame->used + from,
		  current_frame->used + from + amount,
		  (end - from) * sizeof current_frame->used[0]);

      safe_bcopy (current_frame->highlight + from,
		  current_frame->highlight + from + amount,
		  (end - from) * sizeof current_frame->highlight[0]);

      safe_bcopy (current_frame->enable + from,
		  current_frame->enable + from + amount,
		  (end - from) * sizeof current_frame->enable[0]);

      /* Adjust the lines by an amount
	 that puts the first of them at NEWPOS.  */
      pos_adjust = newpos - current_frame->charstarts[from + amount][0];

      /* Offset each char position in the charstarts lines we moved
	 by pos_adjust.  */
      for (i = from + amount; i < end + amount; i++)
	{
	  int *line = current_frame->charstarts[i];
	  int col;
	  for (col = 0; col < width; col++)
	    if (line[col] > 0)
	      line[col] += pos_adjust;
	}
      for (i = end + amount; i < end; i++)
	{
	  int *line = current_frame->charstarts[i];
	  int col;
	  line[0] = -1;
	  for (col = 0; col < width; col++)
	    line[col] = 0;
	}

      /* Mark the lines made empty by scrolling as enabled, empty and
	 normal video.  */
      bzero (current_frame->used + end + amount,
	     - amount * sizeof current_frame->used[0]);
      bzero (current_frame->highlight + end + amount,
	     - amount * sizeof current_frame->highlight[0]);
      for (i = end + amount; i < end; i++)
	{
	  current_frame->glyphs[i][0] = '\0';
	  current_frame->charstarts[i][0] = 0;
	  current_frame->enable[i] = 1;
	}

      safe_bcopy (current_frame->bufp + from,
		  current_frame->bufp + from + amount,
		  (end - from) * sizeof current_frame->bufp[0]);

#ifdef HAVE_X_WINDOWS
      if (FRAME_X_P (frame))
	{
	  safe_bcopy (current_frame->top_left_x + from,
		      current_frame->top_left_x + from + amount,
		      (end - from) * sizeof current_frame->top_left_x[0]);

	  safe_bcopy (current_frame->top_left_y + from,
		      current_frame->top_left_y + from + amount,
		      (end - from) * sizeof current_frame->top_left_y[0]);

	  safe_bcopy (current_frame->pix_width + from,
		      current_frame->pix_width + from + amount,
		      (end - from) * sizeof current_frame->pix_width[0]);

	  safe_bcopy (current_frame->pix_height + from,
		      current_frame->pix_height + from + amount,
		      (end - from) * sizeof current_frame->pix_height[0]);

	  safe_bcopy (current_frame->max_ascent + from,
		      current_frame->max_ascent + from + amount,
		      (end - from) * sizeof current_frame->max_ascent[0]);
	}
#endif				/* HAVE_X_WINDOWS */

      update_end (frame);
    }
  return 1;
}

/* After updating a window W that isn't the full frame wide,
   copy all the columns that W does not occupy
   into the FRAME_DESIRED_GLYPHS (frame) from the FRAME_PHYS_GLYPHS (frame)
   so that update_frame will not change those columns.  */

preserve_other_columns (w)
     struct window *w;
{
  register int vpos;
  register struct frame_glyphs *current_frame, *desired_frame;
  register FRAME_PTR frame = XFRAME (w->frame);
  int start = XFASTINT (w->left);
  int end = XFASTINT (w->left) + XFASTINT (w->width);
  int bot = XFASTINT (w->top) + XFASTINT (w->height);

  current_frame = FRAME_CURRENT_GLYPHS (frame);
  desired_frame = FRAME_DESIRED_GLYPHS (frame);

  for (vpos = XFASTINT (w->top); vpos < bot; vpos++)
    {
      if (current_frame->enable[vpos] && desired_frame->enable[vpos])
	{
	  if (start > 0)
	    {
	      int len;

	      bcopy (current_frame->glyphs[vpos],
		     desired_frame->glyphs[vpos],
		     start * sizeof (current_frame->glyphs[vpos][0]));
	      bcopy (current_frame->charstarts[vpos],
		     desired_frame->charstarts[vpos],
		     start * sizeof (current_frame->charstarts[vpos][0]));
	      len = min (start, current_frame->used[vpos]);
	      if (desired_frame->used[vpos] < len)
		desired_frame->used[vpos] = len;
	    }
	  if (current_frame->used[vpos] > end
	      && desired_frame->used[vpos] < current_frame->used[vpos])
	    {
	      while (desired_frame->used[vpos] < end)
		{
		  int used = desired_frame->used[vpos]++;
		  desired_frame->glyphs[vpos][used] = SPACEGLYPH;
		  desired_frame->glyphs[vpos][used] = 0;
		}
	      bcopy (current_frame->glyphs[vpos] + end,
		     desired_frame->glyphs[vpos] + end,
		     ((current_frame->used[vpos] - end)
		      * sizeof (current_frame->glyphs[vpos][0])));
	      bcopy (current_frame->charstarts[vpos] + end,
		     desired_frame->charstarts[vpos] + end,
		     ((current_frame->used[vpos] - end)
		      * sizeof (current_frame->charstarts[vpos][0])));
	      desired_frame->used[vpos] = current_frame->used[vpos];
	    }
	}
    }
}

#if 0

/* If window w does not need to be updated and isn't the full frame wide,
 copy all the columns that w does occupy
 into the FRAME_DESIRED_LINES (frame) from the FRAME_PHYS_LINES (frame)
 so that update_frame will not change those columns.

 Have not been able to figure out how to use this correctly.  */

preserve_my_columns (w)
     struct window *w;
{
  register int vpos, fin;
  register struct frame_glyphs *l1, *l2;
  register FRAME_PTR frame = XFRAME (w->frame);
  int start = XFASTINT (w->left);
  int end = XFASTINT (w->left) + XFASTINT (w->width);
  int bot = XFASTINT (w->top) + XFASTINT (w->height);

  for (vpos = XFASTINT (w->top); vpos < bot; vpos++)
    {
      if ((l1 = FRAME_DESIRED_GLYPHS (frame)->glyphs[vpos + 1])
	  && (l2 = FRAME_PHYS_GLYPHS (frame)->glyphs[vpos + 1]))
	{
	  if (l2->length > start && l1->length < l2->length)
	    {
	      fin = l2->length;
	      if (fin > end) fin = end;
	      while (l1->length < start)
		l1->body[l1->length++] = ' ';
	      bcopy (l2->body + start, l1->body + start, fin - start);
	      l1->length = fin;
	    }
	}
    }
}

#endif

/* Adjust by ADJUST the charstart values in window W
   after vpos VPOS, which counts relative to the frame
   (not relative to W itself).  */

void
adjust_window_charstarts (w, vpos, adjust)
     struct window *w;
     int vpos;
     int adjust;
{
  int left = XFASTINT (w->left);
  int top = XFASTINT (w->top);
  int right = left + window_internal_width (w);
  int bottom = top + window_internal_height (w);
  int i;

  for (i = vpos + 1; i < bottom; i++)
    {
      int *charstart
	= FRAME_CURRENT_GLYPHS (XFRAME (WINDOW_FRAME (w)))->charstarts[i];
      int j;
      for (j = left; j < right; j++)
	if (charstart[j] > 0)
	  charstart[j] += adjust;
    }
}

/* Check the charstarts values in the area of window W
   for internal consistency.  We cannot check that they are "right";
   we can only look for something nonsensical.  */

verify_charstarts (w)
     struct window *w;
{
  FRAME_PTR f = XFRAME (WINDOW_FRAME (w));
  int i;
  int top = XFASTINT (w->top);
  int bottom = top + window_internal_height (w);
  int left = XFASTINT (w->left);
  int right = left + window_internal_width (w);
  int next_line;
  int truncate = (XINT (w->hscroll)
		  || (truncate_partial_width_windows
		      && (XFASTINT (w->width) < FRAME_WIDTH (f)))
		  || !NILP (XBUFFER (w->buffer)->truncate_lines));

  for (i = top; i < bottom; i++)
    {
      int j;
      int last;
      int *charstart = FRAME_CURRENT_GLYPHS (f)->charstarts[i];

      if (i != top)
	{
	  if (truncate)
	    {
	      /* If we are truncating lines, allow a jump
		 in charstarts from one line to the next.  */
	      if (charstart[left] < next_line)
		abort ();
	    }
	  else
	    {
	      if (charstart[left] != next_line)
		abort ();
	    }
	}

      for (j = left; j < right; j++)
	if (charstart[j] > 0)
	  last = charstart[j];
      /* Record where the next line should start.  */
      next_line = last;
      if (BUF_ZV (XBUFFER (w->buffer)) != last)
	{
	  /* If there's a newline between the two lines, count that.  */
	  int endchar = *BUF_CHAR_ADDRESS (XBUFFER (w->buffer), last);
	  if (endchar == '\n')
	    next_line++;
	}
    }
}

/* On discovering that the redisplay for a window was no good,
   cancel the columns of that window, so that when the window is
   displayed over again get_display_line will not complain.  */

cancel_my_columns (w)
     struct window *w;
{
  register int vpos;
  register struct frame_glyphs *desired_glyphs
    = FRAME_DESIRED_GLYPHS (XFRAME (w->frame));
  register int start = XFASTINT (w->left);
  register int bot = XFASTINT (w->top) + XFASTINT (w->height);

  for (vpos = XFASTINT (w->top); vpos < bot; vpos++)
    if (desired_glyphs->enable[vpos]
	&& desired_glyphs->used[vpos] >= start)
      desired_glyphs->used[vpos] = start;
}

/* These functions try to perform directly and immediately on the frame
   the necessary output for one change in the buffer.
   They may return 0 meaning nothing was done if anything is difficult,
   or 1 meaning the output was performed properly.
   They assume that the frame was up to date before the buffer
   change being displayed.  They make various other assumptions too;
   see command_loop_1 where these are called.  */

int
direct_output_for_insert (g)
     int g;
{
  register FRAME_PTR frame = selected_frame;
  register struct frame_glyphs *current_frame
    = FRAME_CURRENT_GLYPHS (frame);

#ifndef COMPILER_REGISTER_BUG
  register
#endif /* COMPILER_REGISTER_BUG */
    struct window *w = XWINDOW (selected_window);
#ifndef COMPILER_REGISTER_BUG
  register
#endif /* COMPILER_REGISTER_BUG */
    int hpos = FRAME_CURSOR_X (frame);
#ifndef COMPILER_REGISTER_BUG
  register
#endif /* COMPILER_REGISTER_BUG */
    int vpos = FRAME_CURSOR_Y (frame);

  /* Give up if about to continue line.  */
  if (hpos >= XFASTINT (w->left) + window_internal_width (w) - 1
    
  /* Avoid losing if cursor is in invisible text off left margin */
      || (XINT (w->hscroll) && hpos == XFASTINT (w->left))
    
  /* Give up if cursor outside window (in minibuf, probably) */
      || cursor_in_echo_area
      || FRAME_CURSOR_Y (frame) < XFASTINT (w->top)
      || FRAME_CURSOR_Y (frame) >= XFASTINT (w->top) + XFASTINT (w->height)

  /* Give up if cursor not really at FRAME_CURSOR_X, FRAME_CURSOR_Y */
      || !display_completed

  /* Give up if buffer appears in two places.  */
      || buffer_shared > 1

#ifdef USE_TEXT_PROPERTIES
  /* Intervals have already been adjusted, point is after the
     character that was just inserted. */
  /* Give up if character is invisible. */
  /* Give up if character has a face property.
     At the moment we only lose at end of line or end of buffer
     and only with faces that have some background */
  /* Instead of wasting time, give up if character has any text properties */
      || ! NILP (Ftext_properties_at (make_number (point - 1), Qnil))
#endif

  /* Give up if w is minibuffer and a message is being displayed there */
      || (MINI_WINDOW_P (w) && echo_area_glyphs))
    return 0;

  {
    int face = 0;
#ifdef HAVE_FACES
    int dummy;

    if (FRAME_X_P (frame))
      face = compute_char_face (frame, w, point - 1, -1, -1, &dummy, point, 0);
#endif
    current_frame->glyphs[vpos][hpos] = MAKE_GLYPH (frame, g, face);
    current_frame->charstarts[vpos][hpos] = point - 1;
    /* Record the entry for after the newly inserted character.  */
    current_frame->charstarts[vpos][hpos + 1] = point;
    adjust_window_charstarts (w, vpos, 1);
  }
  unchanged_modified = MODIFF;
  beg_unchanged = GPT - BEG;
  XSETFASTINT (w->last_point, point);
  XSETFASTINT (w->last_point_x, hpos);
  XSETFASTINT (w->last_modified, MODIFF);

  reassert_line_highlight (0, vpos);
  write_glyphs (&current_frame->glyphs[vpos][hpos], 1);
  fflush (stdout);
  ++FRAME_CURSOR_X (frame);
  if (hpos == current_frame->used[vpos])
    {
      current_frame->used[vpos] = hpos + 1;
      current_frame->glyphs[vpos][hpos + 1] = 0;
    }

  return 1;
}

int
direct_output_forward_char (n)
     int n;
{
  register FRAME_PTR frame = selected_frame;
  register struct window *w = XWINDOW (selected_window);
  Lisp_Object position;
  int hpos = FRAME_CURSOR_X (frame);

  /* Give up if in truncated text at end of line.  */
  if (hpos >= XFASTINT (w->left) + window_internal_width (w) - 1)
    return 0;

  /* Avoid losing if cursor is in invisible text off left margin
     or about to go off either side of window.  */
  if ((FRAME_CURSOR_X (frame) == XFASTINT (w->left)
       && (XINT (w->hscroll) || n < 0))
      || (n > 0
	  && (FRAME_CURSOR_X (frame) + 1 >= window_internal_width (w) - 1))
      || cursor_in_echo_area)
    return 0;

  /* Can't use direct output if highlighting a region.  */
  if (!NILP (Vtransient_mark_mode) && !NILP (current_buffer->mark_active))
    return 0;

  /* Can't use direct output at an overlay boundary; it might have
     before-string or after-string properties.  */
  if (overlay_touches_p (PT) || overlay_touches_p (PT - n))
    return 0;

#ifdef USE_TEXT_PROPERTIES
  /* Don't use direct output next to an invisible character
     since we might need to do something special.  */

  XSETFASTINT (position, point);
  if (XFASTINT (position) < ZV
      && ! NILP (Fget_char_property (position,
				     Qinvisible,
				     selected_window)))
    return 0;

  XSETFASTINT (position, point - 1);
  if (XFASTINT (position) >= BEGV
      && ! NILP (Fget_char_property (position,
				     Qinvisible,
				     selected_window)))
    return 0;
#endif

  FRAME_CURSOR_X (frame) += n;
  XSETFASTINT (w->last_point_x, FRAME_CURSOR_X (frame));
  XSETFASTINT (w->last_point, point);
  cursor_to (FRAME_CURSOR_Y (frame), FRAME_CURSOR_X (frame));
  fflush (stdout);

  return 1;
}

static void update_line ();

/* Update frame F based on the data in FRAME_DESIRED_GLYPHS.
   Value is nonzero if redisplay stopped due to pending input.
   FORCE nonzero means do not stop for pending input.  */

int
update_frame (f, force, inhibit_hairy_id)
     FRAME_PTR f;
     int force;
     int inhibit_hairy_id;
{
  register struct frame_glyphs *current_frame;
  register struct frame_glyphs *desired_frame = 0;
  register int i;
  int pause;
  int preempt_count = baud_rate / 2400 + 1;
  extern input_pending;
#ifdef HAVE_X_WINDOWS
  register int downto, leftmost;
#endif

  if (baud_rate != FRAME_COST_BAUD_RATE (f))
    calculate_costs (f);

  if (preempt_count <= 0)
    preempt_count = 1;

  if (FRAME_HEIGHT (f) == 0) abort (); /* Some bug zeros some core */

  detect_input_pending ();
  if (input_pending && !force)
    {
      pause = 1;
      goto do_pause;
    }

  update_begin (f);

  if (!line_ins_del_ok)
    inhibit_hairy_id = 1;

  /* These are separate to avoid a possible bug in the AIX C compiler.  */
  current_frame = FRAME_CURRENT_GLYPHS (f);
  desired_frame = FRAME_DESIRED_GLYPHS (f);

  /* See if any of the desired lines are enabled; don't compute for
     i/d line if just want cursor motion. */
  for (i = 0; i < FRAME_HEIGHT (f); i++)
    if (desired_frame->enable[i])
      break;

  /* Try doing i/d line, if not yet inhibited.  */
  if (!inhibit_hairy_id && i < FRAME_HEIGHT (f))
    force |= scrolling (f);

  /* Update the individual lines as needed.  Do bottom line first.  */

  if (desired_frame->enable[FRAME_HEIGHT (f) - 1])
    update_line (f, FRAME_HEIGHT (f) - 1);

#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (f))
    {
      leftmost = downto = f->display.x->internal_border_width;
      if (desired_frame->enable[0])
	{
	  current_frame->top_left_x[FRAME_HEIGHT (f) - 1] = leftmost;
	  current_frame->top_left_y[FRAME_HEIGHT (f) - 1]
	    = PIXEL_HEIGHT (f) - f->display.x->internal_border_width
	      - current_frame->pix_height[FRAME_HEIGHT (f) - 1];
	  current_frame->top_left_x[0] = leftmost;
	  current_frame->top_left_y[0] = downto;
	}
    }
#endif /* HAVE_X_WINDOWS */

  /* Now update the rest of the lines. */
  for (i = 0; i < FRAME_HEIGHT (f) - 1 && (force || !input_pending); i++)
    {
      if (desired_frame->enable[i])
	{
	  if (FRAME_TERMCAP_P (f))
	    {
	      /* Flush out every so many lines.
		 Also flush out if likely to have more than 1k buffered
		 otherwise.   I'm told that some telnet connections get
		 really screwed by more than 1k output at once.  */
	      int outq = PENDING_OUTPUT_COUNT (stdout);
	      if (outq > 900
		  || (outq > 20 && ((i - 1) % preempt_count == 0)))
		{
		  fflush (stdout);
		  if (preempt_count == 1)
		    {
#ifdef EMACS_OUTQSIZE
		      if (EMACS_OUTQSIZE (0, &outq) < 0)
			/* Probably not a tty.  Ignore the error and reset
			 * the outq count. */
			outq = PENDING_OUTPUT_COUNT (stdout);
#endif
		      outq *= 10;
		      if (baud_rate <= outq && baud_rate > 0)
			sleep (outq / baud_rate);
		    }
		}
	      if ((i - 1) % preempt_count == 0)
		detect_input_pending ();
	    }

	  update_line (f, i);
#ifdef HAVE_X_WINDOWS
	  if (FRAME_X_P (f))
	    {
	      current_frame->top_left_y[i] = downto;
	      current_frame->top_left_x[i] = leftmost;
	    }
#endif /* HAVE_X_WINDOWS */
	}

#ifdef HAVE_X_WINDOWS
      if (FRAME_X_P (f))
	downto += current_frame->pix_height[i];
#endif
    }
  pause = (i < FRAME_HEIGHT (f) - 1) ? i : 0;

  /* Now just clean up termcap drivers and set cursor, etc.  */
  if (!pause)
    {
      if (cursor_in_echo_area
	  && FRAME_HAS_MINIBUF_P (f)
	  && EQ (FRAME_MINIBUF_WINDOW (f), minibuf_window))
	{
	  int top = XINT (XWINDOW (FRAME_MINIBUF_WINDOW (f))->top);
	  int row, col;

	  if (cursor_in_echo_area < 0)
	    {
	      row = top;
	      col = 0;
	    }
	  else
	    {
	      /* If the minibuffer is several lines high, find the last
		 line that has any text on it.  */
	      row = FRAME_HEIGHT (f);
	      do 
		{
		  row--;
		  if (current_frame->enable[row])
		    col = current_frame->used[row];
		  else
		    col = 0;
		}
	      while (row > top && col == 0);

	      if (col >= FRAME_WIDTH (f))
		{
		  col = 0;
		  if (row < FRAME_HEIGHT (f) - 1)
		    row++;
		}
	    }

	  cursor_to (row, col);
	}
      else
	cursor_to (FRAME_CURSOR_Y (f), max (min (FRAME_CURSOR_X (f),
						  FRAME_WIDTH (f) - 1), 0));
    }

  update_end (f);

  if (termscript)
    fflush (termscript);
  fflush (stdout);

  /* Here if output is preempted because input is detected.  */
 do_pause:

  if (FRAME_HEIGHT (f) == 0) abort (); /* Some bug zeros some core */
  display_completed = !pause;

  bzero (FRAME_DESIRED_GLYPHS (f)->enable, FRAME_HEIGHT (f));
  return pause;
}

/* Called when about to quit, to check for doing so
   at an improper time.  */

void
quit_error_check ()
{
  if (FRAME_DESIRED_GLYPHS (selected_frame) == 0)
    return;
  if (FRAME_DESIRED_GLYPHS (selected_frame)->enable[0])
    abort ();
  if (FRAME_DESIRED_GLYPHS (selected_frame)->enable[FRAME_HEIGHT (selected_frame) - 1])
    abort ();
}

/* Decide what insert/delete line to do, and do it */

extern void scrolling_1 ();

scrolling (frame)
     FRAME_PTR frame;
{
  int unchanged_at_top, unchanged_at_bottom;
  int window_size;
  int changed_lines;
  int *old_hash = (int *) alloca (FRAME_HEIGHT (frame) * sizeof (int));
  int *new_hash = (int *) alloca (FRAME_HEIGHT (frame) * sizeof (int));
  int *draw_cost = (int *) alloca (FRAME_HEIGHT (frame) * sizeof (int));
  int *old_draw_cost = (int *) alloca (FRAME_HEIGHT (frame) * sizeof (int));
  register int i;
  int free_at_end_vpos = FRAME_HEIGHT (frame);
  register struct frame_glyphs *current_frame = FRAME_CURRENT_GLYPHS (frame);
  register struct frame_glyphs *desired_frame = FRAME_DESIRED_GLYPHS (frame);

  /* Compute hash codes of all the lines.
     Also calculate number of changed lines,
     number of unchanged lines at the beginning,
     and number of unchanged lines at the end.  */

  changed_lines = 0;
  unchanged_at_top = 0;
  unchanged_at_bottom = FRAME_HEIGHT (frame);
  for (i = 0; i < FRAME_HEIGHT (frame); i++)
    {
      /* Give up on this scrolling if some old lines are not enabled.  */
      if (!current_frame->enable[i])
	return 0;
      old_hash[i] = line_hash_code (current_frame, i);
      if (! desired_frame->enable[i])
	new_hash[i] = old_hash[i];
      else
	new_hash[i] = line_hash_code (desired_frame, i);

      if (old_hash[i] != new_hash[i])
	{
	  changed_lines++;
	  unchanged_at_bottom = FRAME_HEIGHT (frame) - i - 1;
	}
      else if (i == unchanged_at_top)
	unchanged_at_top++;
      draw_cost[i] = line_draw_cost (desired_frame, i);
      old_draw_cost[i] = line_draw_cost (current_frame, i);
    }

  /* If changed lines are few, don't allow preemption, don't scroll.  */
  if (!scroll_region_ok && changed_lines < baud_rate / 2400
      || unchanged_at_bottom == FRAME_HEIGHT (frame))
    return 1;

  window_size = (FRAME_HEIGHT (frame) - unchanged_at_top
		 - unchanged_at_bottom);

  if (scroll_region_ok)
    free_at_end_vpos -= unchanged_at_bottom;
  else if (memory_below_frame)
    free_at_end_vpos = -1;

  /* If large window, fast terminal and few lines in common between
     current frame and desired frame, don't bother with i/d calc. */
  if (!scroll_region_ok && window_size >= 18 && baud_rate > 2400
      && (window_size >=
	  10 * scrolling_max_lines_saved (unchanged_at_top,
					  FRAME_HEIGHT (frame) - unchanged_at_bottom,
					  old_hash, new_hash, draw_cost)))
    return 0;

  scrolling_1 (frame, window_size, unchanged_at_top, unchanged_at_bottom,
	       draw_cost + unchanged_at_top - 1,
	       old_draw_cost + unchanged_at_top - 1,
	       old_hash + unchanged_at_top - 1,
	       new_hash + unchanged_at_top - 1,
	       free_at_end_vpos - unchanged_at_top);

  return 0;
}

/* Return the offset in its buffer of the character at location col, line
   in the given window.  */
int
buffer_posn_from_coords (window, col, line)
     struct window *window;
     int col, line;
{
  int hscroll = XINT (window->hscroll);
  int window_left = XFASTINT (window->left);

  /* The actual width of the window is window->width less one for the
     DISP_CONTINUE_GLYPH, and less one if it's not the rightmost
     window.  */
  int window_width = window_internal_width (window) - 1;

  int startp = marker_position (window->start);

  /* Since compute_motion will only operate on the current buffer,
     we need to save the old one and restore it when we're done.  */
  struct buffer *old_current_buffer = current_buffer;
  struct position *posn;

  current_buffer = XBUFFER (window->buffer);

  /* We can't get a correct result in this case,
     but at least prevent compute_motion from crashing.  */
  if (startp < BEGV)
    startp = BEGV;

  /* It would be nice if we could use FRAME_CURRENT_GLYPHS (XFRAME
     (window->frame))->bufp to avoid scanning from the very top of
     the window, but it isn't maintained correctly, and I'm not even
     sure I will keep it.  */
  posn = compute_motion (startp, 0,
			 ((window == XWINDOW (minibuf_window) && startp == BEG
			   ? minibuf_prompt_width : 0)
			  + (hscroll ? 1 - hscroll : 0)),
			 0,
			 ZV, line, col,
			 window_width, hscroll, 0, window);

  current_buffer = old_current_buffer;

  /* compute_motion considers frame points past the end of a line
     to be *after* the newline, i.e. at the start of the next line.
     This is reasonable, but not really what we want.  So if the
     result is on a line below LINE, back it up one character.  */
  if (posn->vpos > line)
    return posn->bufpos - 1;
  else
    return posn->bufpos;
}

static int
count_blanks (r)
     register GLYPH *r;
{
  register GLYPH *p = r;
  while (*p++ == SPACEGLYPH);
  return p - r - 1;
}

static int
count_match (str1, str2)
     GLYPH *str1, *str2;
{
  register GLYPH *p1 = str1;
  register GLYPH *p2 = str2;
  while (*p1++ == *p2++);
  return p1 - str1 - 1;
}

/* Char insertion/deletion cost vector, from term.c */
extern int *char_ins_del_vector;

#define char_ins_del_cost(f) (&char_ins_del_vector[FRAME_WIDTH((f))])

static void
update_line (frame, vpos)
     register FRAME_PTR frame;
     int vpos;
{
  register GLYPH *obody, *nbody, *op1, *op2, *np1, *temp;
  int *temp1;
  int tem;
  int osp, nsp, begmatch, endmatch, olen, nlen;
  int save;
  register struct frame_glyphs *current_frame
    = FRAME_CURRENT_GLYPHS (frame);
  register struct frame_glyphs *desired_frame
    = FRAME_DESIRED_GLYPHS (frame);

  if (desired_frame->highlight[vpos]
      != (current_frame->enable[vpos] && current_frame->highlight[vpos]))
    {
      change_line_highlight (desired_frame->highlight[vpos], vpos,
			     (current_frame->enable[vpos] ?
			      current_frame->used[vpos] : 0));
      current_frame->enable[vpos] = 0;
    }
  else
    reassert_line_highlight (desired_frame->highlight[vpos], vpos);

  if (! current_frame->enable[vpos])
    {
      olen = 0;
    }
  else
    {
      obody = current_frame->glyphs[vpos];
      olen = current_frame->used[vpos];
      if (! current_frame->highlight[vpos])
	{
	  if (!must_write_spaces)
	    while (obody[olen - 1] == SPACEGLYPH && olen > 0)
	      olen--;
	}
      else
	{
	  /* For an inverse-video line, remember we gave it
	     spaces all the way to the frame edge
	     so that the reverse video extends all the way across.  */

	  while (olen < FRAME_WIDTH (frame) - 1)
	    obody[olen++] = SPACEGLYPH;
	}
    }

  /* One way or another, this will enable the line being updated.  */
  current_frame->enable[vpos] = 1;
  current_frame->used[vpos] = desired_frame->used[vpos];
  current_frame->highlight[vpos] = desired_frame->highlight[vpos];
  current_frame->bufp[vpos] = desired_frame->bufp[vpos];

#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (frame))
    {
      current_frame->pix_width[vpos]
	= current_frame->used[vpos]
	  * FONT_WIDTH (frame->display.x->font);
      current_frame->pix_height[vpos]
	= frame->display.x->line_height;
    }
#endif /* HAVE_X_WINDOWS */

  if (!desired_frame->enable[vpos])
    {
      nlen = 0;
      goto just_erase;
    }

  nbody = desired_frame->glyphs[vpos];
  nlen = desired_frame->used[vpos];

  /* Pretend trailing spaces are not there at all,
     unless for one reason or another we must write all spaces.  */
  if (! desired_frame->highlight[vpos])
    {
      if (!must_write_spaces)
	/* We know that the previous character byte contains 0.  */
	while (nbody[nlen - 1] == SPACEGLYPH)
	  nlen--;
    }
  else
    {
      /* For an inverse-video line, give it extra trailing spaces
	 all the way to the frame edge
	 so that the reverse video extends all the way across.  */

      while (nlen < FRAME_WIDTH (frame) - 1)
	nbody[nlen++] = SPACEGLYPH;
    }

  /* If there's no i/d char, quickly do the best we can without it.  */
  if (!char_ins_del_ok)
    {
      int i,j;

#if 0
      if (FRAME_X_P (frame))
	{
	  /* Under X, erase everything we are going to rewrite,
	     and rewrite everything from the first char that's changed.
	     This is part of supporting fonts like Courier
	     whose chars can overlap outside the char width.  */
	  for (i = 0; i < nlen; i++)
	    if (i >= olen || nbody[i] != obody[i])
	      break;

	  cursor_to (vpos, i);
	  if (i != olen)
	    clear_end_of_line (olen);
	  write_glyphs (nbody + i, nlen - i);
	}
      else
	{}
#endif /* 0 */
      for (i = 0; i < nlen; i++)
	{
	  if (i >= olen || nbody[i] != obody[i])    /* A non-matching char. */
	    {
	      cursor_to (vpos, i);
	      for (j = 1; (i + j < nlen &&
			   (i + j >= olen || nbody[i+j] != obody[i+j]));
		   j++);

	      /* Output this run of non-matching chars.  */ 
	      write_glyphs (nbody + i, j);
	      i += j - 1;

	      /* Now find the next non-match.  */
	    }
	}

      /* Clear the rest of the line, or the non-clear part of it.  */
      if (olen > nlen)
	{
	  cursor_to (vpos, nlen);
	  clear_end_of_line (olen);
	}

      /* Exchange contents between current_frame and new_frame.  */
      temp = desired_frame->glyphs[vpos];
      desired_frame->glyphs[vpos] = current_frame->glyphs[vpos];
      current_frame->glyphs[vpos] = temp;

      /* Exchange charstarts between current_frame and new_frame.  */
      temp1 = desired_frame->charstarts[vpos];
      desired_frame->charstarts[vpos] = current_frame->charstarts[vpos];
      current_frame->charstarts[vpos] = temp1;

      return;
    }

  if (!olen)
    {
      nsp = (must_write_spaces || desired_frame->highlight[vpos])
	      ? 0 : count_blanks (nbody);
      if (nlen > nsp)
	{
	  cursor_to (vpos, nsp);
	  write_glyphs (nbody + nsp, nlen - nsp);
	}

      /* Exchange contents between current_frame and new_frame.  */
      temp = desired_frame->glyphs[vpos];
      desired_frame->glyphs[vpos] = current_frame->glyphs[vpos];
      current_frame->glyphs[vpos] = temp;

      /* Exchange charstarts between current_frame and new_frame.  */
      temp1 = desired_frame->charstarts[vpos];
      desired_frame->charstarts[vpos] = current_frame->charstarts[vpos];
      current_frame->charstarts[vpos] = temp1;

      return;
    }

  obody[olen] = 1;
  save = nbody[nlen];
  nbody[nlen] = 0;

  /* Compute number of leading blanks in old and new contents.  */
  osp = count_blanks (obody);
  if (!desired_frame->highlight[vpos])
    nsp = count_blanks (nbody);
  else
    nsp = 0;

  /* Compute number of matching chars starting with first nonblank.  */
  begmatch = count_match (obody + osp, nbody + nsp);

  /* Spaces in new match implicit space past the end of old.  */
  /* A bug causing this to be a no-op was fixed in 18.29.  */
  if (!must_write_spaces && osp + begmatch == olen)
    {
      np1 = nbody + nsp;
      while (np1[begmatch] == SPACEGLYPH)
	begmatch++;
    }

  /* Avoid doing insert/delete char
     just cause number of leading spaces differs
     when the following text does not match. */
  if (begmatch == 0 && osp != nsp)
    osp = nsp = min (osp, nsp);

  /* Find matching characters at end of line */
  op1 = obody + olen;
  np1 = nbody + nlen;
  op2 = op1 + begmatch - min (olen - osp, nlen - nsp);
  while (op1 > op2 && op1[-1] == np1[-1])
    {
      op1--;
      np1--;
    }
  endmatch = obody + olen - op1;

  /* Put correct value back in nbody[nlen].
     This is important because direct_output_for_insert
     can write into the line at a later point.
     If this screws up the zero at the end of the line, re-establish it.  */
  nbody[nlen] = save;
  obody[olen] = 0;

  /* tem gets the distance to insert or delete.
     endmatch is how many characters we save by doing so.
     Is it worth it?  */

  tem = (nlen - nsp) - (olen - osp);
  if (endmatch && tem
      && (!char_ins_del_ok || endmatch <= char_ins_del_cost (frame)[tem]))
    endmatch = 0;

  /* nsp - osp is the distance to insert or delete.
     If that is nonzero, begmatch is known to be nonzero also.
     begmatch + endmatch is how much we save by doing the ins/del.
     Is it worth it?  */

  if (nsp != osp
      && (!char_ins_del_ok
	  || begmatch + endmatch <= char_ins_del_cost (frame)[nsp - osp]))
    {
      begmatch = 0;
      endmatch = 0;
      osp = nsp = min (osp, nsp);
    }

  /* Now go through the line, inserting, writing and
     deleting as appropriate.  */

  if (osp > nsp)
    {
      cursor_to (vpos, nsp);
      delete_glyphs (osp - nsp);
    }
  else if (nsp > osp)
    {
      /* If going to delete chars later in line
	 and insert earlier in the line,
	 must delete first to avoid losing data in the insert */
      if (endmatch && nlen < olen + nsp - osp)
	{
	  cursor_to (vpos, nlen - endmatch + osp - nsp);
	  delete_glyphs (olen + nsp - osp - nlen);
	  olen = nlen - (nsp - osp);
	}
      cursor_to (vpos, osp);
      insert_glyphs ((char *)0, nsp - osp);
    }
  olen += nsp - osp;

  tem = nsp + begmatch + endmatch;
  if (nlen != tem || olen != tem)
    {
      cursor_to (vpos, nsp + begmatch);
      if (!endmatch || nlen == olen)
	{
	  /* If new text being written reaches right margin,
	     there is no need to do clear-to-eol at the end.
	     (and it would not be safe, since cursor is not
	     going to be "at the margin" after the text is done) */
	  if (nlen == FRAME_WIDTH (frame))
	    olen = 0;
	  write_glyphs (nbody + nsp + begmatch, nlen - tem);

#ifdef obsolete

/* the following code loses disastrously if tem == nlen.
   Rather than trying to fix that case, I am trying the simpler
   solution found above.  */

	  /* If the text reaches to the right margin,
	     it will lose one way or another (depending on AutoWrap)
	     to clear to end of line after outputting all the text.
	     So pause with one character to go and clear the line then.  */
	  if (nlen == FRAME_WIDTH (frame) && fast_clear_end_of_line && olen > nlen)
	    {
	      /* endmatch must be zero, and tem must equal nsp + begmatch */
	      write_glyphs (nbody + tem, nlen - tem - 1);
	      clear_end_of_line (olen);
	      olen = 0;		/* Don't let it be cleared again later */
	      write_glyphs (nbody + nlen - 1, 1);
	    }
	  else
	    write_glyphs (nbody + nsp + begmatch, nlen - tem);
#endif	/* OBSOLETE */

	}
      else if (nlen > olen)
	{
	  write_glyphs (nbody + nsp + begmatch, olen - tem);
	  insert_glyphs (nbody + nsp + begmatch + olen - tem, nlen - olen);
	  olen = nlen;
	}
      else if (olen > nlen)
	{
	  write_glyphs (nbody + nsp + begmatch, nlen - tem);
	  delete_glyphs (olen - nlen);
	  olen = nlen;
	}
    }

 just_erase:
  /* If any unerased characters remain after the new line, erase them.  */
  if (olen > nlen)
    {
      cursor_to (vpos, nlen);
      clear_end_of_line (olen);
    }

  /* Exchange contents between current_frame and new_frame.  */
  temp = desired_frame->glyphs[vpos];
  desired_frame->glyphs[vpos] = current_frame->glyphs[vpos];
  current_frame->glyphs[vpos] = temp;

  /* Exchange charstarts between current_frame and new_frame.  */
  temp1 = desired_frame->charstarts[vpos];
  desired_frame->charstarts[vpos] = current_frame->charstarts[vpos];
  current_frame->charstarts[vpos] = temp1;
}

/* A vector of size >= NFRAMES + 3 * NBUFFERS + 1, containing the session's
   frames, buffers, buffer-read-only flags, and buffer-modified-flags,
   and a trailing sentinel (so we don't need to add length checks).  */
static Lisp_Object frame_and_buffer_state;

DEFUN ("frame-or-buffer-changed-p", Fframe_or_buffer_changed_p,
  Sframe_or_buffer_changed_p, 0, 0, 0,
  "Return non-nil if the frame and buffer state appears to have changed.\n\
The state variable is an internal vector containing all frames and buffers,\n\
aside from buffers whose names start with space,\n\
along with the buffers' read-only and modified flags, which allows a fast\n\
check to see whether the menu bars might need to be recomputed.\n\
If this function returns non-nil, it updates the internal vector to reflect\n\
the current state.\n")
  ()
{
  Lisp_Object tail, frame, buf;
  Lisp_Object *vecp;
  int n;
  vecp = XVECTOR (frame_and_buffer_state)->contents;
  FOR_EACH_FRAME (tail, frame)
    if (!EQ (*vecp++, frame))
      goto changed;
  /* Check that the buffer info matches.
     No need to test for the end of the vector
     because the last element of the vector is lambda
     and that will always cause a mismatch.  */
  for (tail = Vbuffer_alist; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      buf = XCONS (XCONS (tail)->car)->cdr;
      /* Ignore buffers that aren't included in buffer lists.  */
      if (XSTRING (XBUFFER (buf)->name)->data[0] == ' ')
	continue;
      if (!EQ (*vecp++, buf))
	goto changed;
      if (!EQ (*vecp++, XBUFFER (buf)->read_only))
	goto changed;
      if (!EQ (*vecp++, Fbuffer_modified_p (buf)))
	goto changed;
    }
  /* Detect deletion of a buffer at the end of the list.  */
  if (*vecp == Qlambda)
    return Qnil;
 changed:
  /* Start with 1 so there is room for at least one lambda at the end.  */
  n = 1;
  FOR_EACH_FRAME (tail, frame)
    n++;
  for (tail = Vbuffer_alist; CONSP (tail); tail = XCONS (tail)->cdr)
    n += 3;
  /* Reallocate the vector if it's grown, or if it's shrunk a lot.  */
  if (n > XVECTOR (frame_and_buffer_state)->size
      || n < XVECTOR (frame_and_buffer_state)->size / 2)
    frame_and_buffer_state = Fmake_vector (make_number (n), Qlambda);
  vecp = XVECTOR (frame_and_buffer_state)->contents;
  FOR_EACH_FRAME (tail, frame)
    *vecp++ = frame;
  for (tail = Vbuffer_alist; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      buf = XCONS (XCONS (tail)->car)->cdr;
      /* Ignore buffers that aren't included in buffer lists.  */
      if (XSTRING (XBUFFER (buf)->name)->data[0] == ' ')
	continue;
      *vecp++ = buf;
      *vecp++ = XBUFFER (buf)->read_only;
      *vecp++ = Fbuffer_modified_p (buf);
    }
  /* If we left any slack in the vector, fill it up now.  */
  for (; n < XVECTOR (frame_and_buffer_state)->size; ++n)
    *vecp++ = Qlambda;
  return Qt;
}

DEFUN ("open-termscript", Fopen_termscript, Sopen_termscript,
  1, 1, "FOpen termscript file: ",
  "Start writing all terminal output to FILE as well as the terminal.\n\
FILE = nil means just close any termscript file currently open.")
  (file)
     Lisp_Object file;
{
  if (termscript != 0) fclose (termscript);
  termscript = 0;

  if (! NILP (file))
    {
      file = Fexpand_file_name (file, Qnil);
      termscript = fopen (XSTRING (file)->data, "w");
      if (termscript == 0)
	report_file_error ("Opening termscript", Fcons (file, Qnil));
    }
  return Qnil;
}


#ifdef SIGWINCH
SIGTYPE
window_change_signal (signalnum) /* If we don't have an argument, */
     int signalnum;		/* some compilers complain in signal calls. */
{
  int width, height;
  extern int errno;
  int old_errno = errno;

  get_frame_size (&width, &height);

  /* The frame size change obviously applies to a termcap-controlled
     frame.  Find such a frame in the list, and assume it's the only
     one (since the redisplay code always writes to stdout, not a
     FILE * specified in the frame structure).  Record the new size,
     but don't reallocate the data structures now.  Let that be done
     later outside of the signal handler.  */

  {
    Lisp_Object tail, frame;

    FOR_EACH_FRAME (tail, frame)
      {
	if (FRAME_TERMCAP_P (XFRAME (frame)))
	  {
	    change_frame_size (XFRAME (frame), height, width, 0, 1);
	    break;
	  }
      }
  }

  signal (SIGWINCH, window_change_signal);
  errno = old_errno;
}
#endif /* SIGWINCH */


/* Do any change in frame size that was requested by a signal.  */

do_pending_window_change ()
{
  /* If window_change_signal should have run before, run it now.  */
  while (delayed_size_change)
    {
      Lisp_Object tail, frame;

      delayed_size_change = 0;

      FOR_EACH_FRAME (tail, frame)
	{
	  FRAME_PTR f = XFRAME (frame);

	  int height = FRAME_NEW_HEIGHT (f);
	  int width = FRAME_NEW_WIDTH (f);

	  if (height != 0 || width != 0)
	    change_frame_size (f, height, width, 0, 0);
	}
    }
}


/* Change the frame height and/or width.  Values may be given as zero to
   indicate no change is to take place. 

   If DELAY is non-zero, then assume we're being called from a signal
   handler, and queue the change for later - perhaps the next
   redisplay.  Since this tries to resize windows, we can't call it
   from a signal handler.  */

change_frame_size (f, newheight, newwidth, pretend, delay)
     register FRAME_PTR f;
     int newheight, newwidth, pretend;
{
  Lisp_Object tail, frame;
  if (FRAME_TERMCAP_P (f))
    {
      /* When using termcap, all frames use the same screen,
	 so a change in size affects all termcap frames.  */
      FOR_EACH_FRAME (tail, frame)
	if (FRAME_TERMCAP_P (XFRAME (frame)))
	  change_frame_size_1 (XFRAME (frame), newheight, newwidth,
			       pretend, delay);
    }
  else
    change_frame_size_1 (f, newheight, newwidth, pretend, delay);
}

static void
change_frame_size_1 (frame, newheight, newwidth, pretend, delay)
     register FRAME_PTR frame;
     int newheight, newwidth, pretend, delay;
{
  /* If we can't deal with the change now, queue it for later.  */
  if (delay)
    {
      FRAME_NEW_HEIGHT (frame) = newheight;
      FRAME_NEW_WIDTH (frame) = newwidth;
      delayed_size_change = 1;
      return;
    }

  /* This size-change overrides any pending one for this frame.  */
  FRAME_NEW_HEIGHT (frame) = 0;
  FRAME_NEW_WIDTH  (frame) = 0;

  /* If an argument is zero, set it to the current value.  */
  newheight || (newheight = FRAME_HEIGHT (frame));
  newwidth  || (newwidth  = FRAME_WIDTH  (frame));

  /* Round up to the smallest acceptable size.  */
  check_frame_size (frame, &newheight, &newwidth);

  /* If we're not changing the frame size, quit now.  */
  if (newheight == FRAME_HEIGHT (frame)
      && newwidth == FRAME_WIDTH (frame))
    return;

  if (newheight != FRAME_HEIGHT (frame))
    {
      if (FRAME_HAS_MINIBUF_P (frame)
	  && ! FRAME_MINIBUF_ONLY_P (frame))
	{
	  /* Frame has both root and minibuffer.  */
	  set_window_height (FRAME_ROOT_WINDOW (frame),
			     newheight - 1 - FRAME_MENU_BAR_LINES (frame), 0);
	  XSETFASTINT (XWINDOW (FRAME_MINIBUF_WINDOW (frame))->top,
		       newheight - 1);
	  set_window_height (FRAME_MINIBUF_WINDOW (frame), 1, 0);
	}
      else
	/* Frame has just one top-level window.  */
	set_window_height (FRAME_ROOT_WINDOW (frame),
			   newheight - FRAME_MENU_BAR_LINES (frame), 0);

      if (FRAME_TERMCAP_P (frame) && !pretend)
	FrameRows = newheight;

#if 0
      if (frame->output_method == output_termcap)
	{
	  frame_height = newheight;
	  if (!pretend)
	    FrameRows = newheight;
	}
#endif
    }

  if (newwidth != FRAME_WIDTH (frame))
    {
      set_window_width (FRAME_ROOT_WINDOW (frame), newwidth, 0);
      if (FRAME_HAS_MINIBUF_P (frame))
	set_window_width (FRAME_MINIBUF_WINDOW (frame), newwidth, 0);

      if (FRAME_TERMCAP_P (frame) && !pretend)
	FrameCols = newwidth;
#if 0
      if (frame->output_method == output_termcap)
	{
	  frame_width = newwidth;
	  if (!pretend)
	    FrameCols = newwidth;
	}
#endif
    }

  FRAME_HEIGHT (frame) = newheight;
  FRAME_WIDTH (frame)  = newwidth;

  if (FRAME_CURSOR_X (frame) >= FRAME_WIDTH (frame))
    FRAME_CURSOR_X (frame) = FRAME_WIDTH (frame) - 1;
  if (FRAME_CURSOR_Y (frame) >= FRAME_HEIGHT (frame))
    FRAME_CURSOR_Y (frame) = FRAME_HEIGHT (frame) - 1;

  remake_frame_glyphs (frame);
  calculate_costs (frame);
}

DEFUN ("send-string-to-terminal", Fsend_string_to_terminal,
  Ssend_string_to_terminal, 1, 1, 0,
  "Send STRING to the terminal without alteration.\n\
Control characters in STRING will have terminal-dependent effects.")
  (str)
     Lisp_Object str;
{
  CHECK_STRING (str, 0);
  fwrite (XSTRING (str)->data, 1, XSTRING (str)->size, stdout);
  fflush (stdout);
  if (termscript)
    {
      fwrite (XSTRING (str)->data, 1, XSTRING (str)->size, termscript);
      fflush (termscript);
    }
  return Qnil;
}

DEFUN ("ding", Fding, Sding, 0, 1, 0,
  "Beep, or flash the screen.\n\
Also, unless an argument is given,\n\
terminate any keyboard macro currently executing.")
  (arg)
  Lisp_Object arg;
{
  if (!NILP (arg))
    {
      if (noninteractive)
	putchar (07);
      else
	ring_bell ();
      fflush (stdout);
    }
  else
    bitch_at_user ();

  return Qnil;
}

bitch_at_user ()
{
  if (noninteractive)
    putchar (07);
  else if (!INTERACTIVE)  /* Stop executing a keyboard macro. */
    error ("Keyboard macro terminated by a command ringing the bell");
  else
    ring_bell ();
  fflush (stdout);
}

DEFUN ("sleep-for", Fsleep_for, Ssleep_for, 1, 2, 0,
  "Pause, without updating display, for SECONDS seconds.\n\
SECONDS may be a floating-point value, meaning that you can wait for a\n\
fraction of a second.  Optional second arg MILLISECONDS specifies an\n\
additional wait period, in milliseconds; this may be useful if your\n\
Emacs was built without floating point support.\n\
\(Not all operating systems support waiting for a fraction of a second.)")
  (seconds, milliseconds)
     Lisp_Object seconds, milliseconds;
{
  int sec, usec;

  if (NILP (milliseconds))
    XSETINT (milliseconds, 0);
  else
    CHECK_NUMBER (milliseconds, 1);
  usec = XINT (milliseconds) * 1000;

#ifdef LISP_FLOAT_TYPE
  {
    double duration = extract_float (seconds);
    sec = (int) duration;
    usec += (duration - sec) * 1000000;
  }
#else
  CHECK_NUMBER (seconds, 0);
  sec = XINT (seconds);
#endif

#ifndef EMACS_HAS_USECS
  if (sec == 0 && usec != 0)
    error ("millisecond `sleep-for' not supported on %s", SYSTEM_TYPE);
#endif

  /* Assure that 0 <= usec < 1000000.  */
  if (usec < 0)
    {
      /* We can't rely on the rounding being correct if user is negative.  */
      if (-1000000 < usec)
	sec--, usec += 1000000;
      else
	sec -= -usec / 1000000, usec = 1000000 - (-usec % 1000000);
    }
  else
    sec += usec / 1000000, usec %= 1000000;

  if (sec <= 0)
    return Qnil;

  {
    Lisp_Object zero;

    XSETFASTINT (zero, 0);
    wait_reading_process_input (sec, usec, zero, 0);
  }

  /* We should always have wait_reading_process_input; we have a dummy
     implementation for systems which don't support subprocesses.  */
#if 0
  /* No wait_reading_process_input */
  immediate_quit = 1;
  QUIT;

#ifdef VMS
  sys_sleep (sec);
#else /* not VMS */
/* The reason this is done this way 
    (rather than defined (H_S) && defined (H_T))
   is because the VMS preprocessor doesn't grok `defined' */
#ifdef HAVE_SELECT
  EMACS_GET_TIME (end_time);
  EMACS_SET_SECS_USECS (timeout, sec, usec);
  EMACS_ADD_TIME (end_time, end_time, timeout);
 
  while (1)
    {
      EMACS_GET_TIME (timeout);
      EMACS_SUB_TIME (timeout, end_time, timeout);
      if (EMACS_TIME_NEG_P (timeout)
	  || !select (1, 0, 0, 0, &timeout))
	break;
    }
#else /* not HAVE_SELECT */
  sleep (sec);
#endif /* HAVE_SELECT */
#endif /* not VMS */
  
  immediate_quit = 0;
#endif /* no subprocesses */

  return Qnil;
}

/* This is just like wait_reading_process_input, except that
   it does the redisplay.

   It's also much like Fsit_for, except that it can be used for
   waiting for input as well.  */

Lisp_Object
sit_for (sec, usec, reading, display)
     int sec, usec, reading, display;
{
  Lisp_Object read_kbd;

  if (detect_input_pending ())
    return Qnil;

  if (display)
    redisplay_preserve_echo_area ();

  if (sec == 0 && usec == 0)
    return Qt;

#ifdef SIGIO
  gobble_input (0);
#endif

  XSETINT (read_kbd, reading ? -1 : 1);
  wait_reading_process_input (sec, usec, read_kbd, display);


  /* wait_reading_process_input should always be available now; it is
     simulated in a simple way on systems that don't support
     subprocesses.  */
#if 0
  /* No wait_reading_process_input available.  */
  immediate_quit = 1;
  QUIT;

  waitchannels = 1;
#ifdef VMS
  input_wait_timeout (XINT (arg));
#else				/* not VMS */
#ifndef HAVE_TIMEVAL
  timeout_sec = sec;
  select (1, &waitchannels, 0, 0, &timeout_sec);
#else /* HAVE_TIMEVAL */
  timeout.tv_sec = sec;  
  timeout.tv_usec = usec;
  select (1, &waitchannels, 0, 0, &timeout);
#endif /* HAVE_TIMEVAL */
#endif /* not VMS */

  immediate_quit = 0;
#endif 

  return detect_input_pending () ? Qnil : Qt;
}

DEFUN ("sit-for", Fsit_for, Ssit_for, 1, 3, 0,
  "Perform redisplay, then wait for SECONDS seconds or until input is available.\n\
SECONDS may be a floating-point value, meaning that you can wait for a\n\
fraction of a second.  Optional second arg MILLISECONDS specifies an\n\
additional wait period, in milliseconds; this may be useful if your\n\
Emacs was built without floating point support.\n\
\(Not all operating systems support waiting for a fraction of a second.)\n\
Optional third arg non-nil means don't redisplay, just wait for input.\n\
Redisplay is preempted as always if input arrives, and does not happen\n\
if input is available before it starts.\n\
Value is t if waited the full time with no input arriving.")
  (seconds, milliseconds, nodisp)
     Lisp_Object seconds, milliseconds, nodisp;
{
  int sec, usec;

  if (NILP (milliseconds))
    XSETINT (milliseconds, 0);
  else
    CHECK_NUMBER (milliseconds, 1);
  usec = XINT (milliseconds) * 1000;

#ifdef LISP_FLOAT_TYPE
  {
    double duration = extract_float (seconds);
    sec = (int) duration;
    usec += (duration - sec) * 1000000;
  }
#else
  CHECK_NUMBER (seconds, 0);
  sec = XINT (seconds);
#endif

#ifndef EMACS_HAS_USECS
  if (usec != 0 && sec == 0)
    error ("millisecond `sit-for' not supported on %s", SYSTEM_TYPE);
#endif

  if (NILP (nodisp))
    prepare_menu_bars ();
  return sit_for (sec, usec, 0, NILP (nodisp));
}

char *terminal_type;

/* Initialization done when Emacs fork is started, before doing stty. */
/* Determine terminal type and set terminal_driver */
/* Then invoke its decoding routine to set up variables
  in the terminal package */

init_display ()
{
#ifdef HAVE_X_WINDOWS
  extern int display_arg;
#endif

  meta_key = 0;
  inverse_video = 0;
  cursor_in_echo_area = 0;
  terminal_type = (char *) 0;

  /* Now is the time to initialize this; it's used by init_sys_modes
     during startup.  */
  Vwindow_system = Qnil;

  /* If the user wants to use a window system, we shouldn't bother
     initializing the terminal.  This is especially important when the
     terminal is so dumb that emacs gives up before and doesn't bother
     using the window system.

     If the DISPLAY environment variable is set, try to use X, and die
     with an error message if that doesn't work.  */

#ifdef HAVE_X_WINDOWS
  if (! display_arg)
    {
#ifdef VMS
      display_arg = (getenv ("DECW$DISPLAY") != 0);
#else
      display_arg = (getenv ("DISPLAY") != 0);
#endif
    }

  if (!inhibit_window_system && display_arg)
    {
      Vwindow_system = intern ("x");
#ifdef HAVE_X11
      Vwindow_system_version = make_number (11);
#else
      Vwindow_system_version = make_number (10);
#endif
      return;
    }
#endif /* HAVE_X_WINDOWS */

  /* If no window system has been specified, try to use the terminal.  */
  if (! isatty (0))
    {
      fprintf (stderr, "emacs: standard input is not a tty\n");
      exit (1);
    }

  /* Look at the TERM variable */
  terminal_type = (char *) getenv ("TERM");
  if (!terminal_type)
    {
#ifdef VMS
      fprintf (stderr, "Please specify your terminal type.\n\
For types defined in VMS, use  set term /device=TYPE.\n\
For types not defined in VMS, use  define emacs_term \"TYPE\".\n\
\(The quotation marks are necessary since terminal types are lower case.)\n");
#else
      fprintf (stderr, "Please set the environment variable TERM; see tset(1).\n");
#endif
      exit (1);
    }

#ifdef VMS
  /* VMS DCL tends to upcase things, so downcase term type.
     Hardly any uppercase letters in terminal types; should be none.  */
  {
    char *new = (char *) xmalloc (strlen (terminal_type) + 1);
    char *p;

    strcpy (new, terminal_type);

    for (p = new; *p; p++)
      if (isupper (*p))
	*p = tolower (*p);

    terminal_type = new;
  }	
#endif

  term_init (terminal_type);

  remake_frame_glyphs (selected_frame);
  calculate_costs (selected_frame);

  /* X and Y coordinates of the cursor between updates. */
  FRAME_CURSOR_X (selected_frame) = 0;
  FRAME_CURSOR_Y (selected_frame) = 0;

#ifdef SIGWINCH
#ifndef CANNOT_DUMP
  if (initialized)
#endif /* CANNOT_DUMP */
    signal (SIGWINCH, window_change_signal);
#endif /* SIGWINCH */
}

syms_of_display ()
{
#ifdef MULTI_FRAME
  defsubr (&Sredraw_frame);
#endif
  defsubr (&Sredraw_display);
  defsubr (&Sframe_or_buffer_changed_p);
  defsubr (&Sopen_termscript);
  defsubr (&Sding);
  defsubr (&Ssit_for);
  defsubr (&Ssleep_for);
  defsubr (&Ssend_string_to_terminal);

  frame_and_buffer_state = Fmake_vector (make_number (1), Qlambda);
  staticpro (&frame_and_buffer_state);

  DEFVAR_INT ("baud-rate", &baud_rate,
    "*The output baud rate of the terminal.\n\
On most systems, changing this value will affect the amount of padding\n\
and the other strategic decisions made during redisplay.");
  DEFVAR_BOOL ("inverse-video", &inverse_video,
    "*Non-nil means invert the entire frame display.\n\
This means everything is in inverse video which otherwise would not be.");
  DEFVAR_BOOL ("visible-bell", &visible_bell,
    "*Non-nil means try to flash the frame to represent a bell.");
  DEFVAR_BOOL ("no-redraw-on-reenter", &no_redraw_on_reenter,
    "*Non-nil means no need to redraw entire frame after suspending.\n\
A non-nil value is useful if the terminal can automatically preserve\n\
Emacs's frame display when you reenter Emacs.\n\
It is up to you to set this variable if your terminal can do that.");
  DEFVAR_LISP ("window-system", &Vwindow_system,
    "A symbol naming the window-system under which Emacs is running\n\
\(such as `x'), or nil if emacs is running on an ordinary terminal.");
  DEFVAR_LISP ("window-system-version", &Vwindow_system_version,
    "The version number of the window system in use.\n\
For X windows, this is 10 or 11.");
  DEFVAR_BOOL ("cursor-in-echo-area", &cursor_in_echo_area,
    "Non-nil means put cursor in minibuffer, at end of any message there.");
  DEFVAR_LISP ("glyph-table", &Vglyph_table,
    "Table defining how to output a glyph code to the frame.\n\
If not nil, this is a vector indexed by glyph code to define the glyph.\n\
Each element can be:\n\
 integer: a glyph code which this glyph is an alias for.\n\
 string: output this glyph using that string (not impl. in X windows).\n\
 nil: this glyph mod 256 is char code to output,\n\
    and this glyph / 256 is face code for X windows (see `face-id').");
  Vglyph_table = Qnil;

  DEFVAR_LISP ("standard-display-table", &Vstandard_display_table,
    "Display table to use for buffers that specify none.\n\
See `buffer-display-table' for more information.");
  Vstandard_display_table = Qnil;

  /* Initialize `window-system', unless init_display already decided it.  */
#ifdef CANNOT_DUMP
  if (noninteractive)
#endif
    {
      Vwindow_system = Qnil;
      Vwindow_system_version = Qnil;
    }
}
