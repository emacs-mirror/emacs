/* Updating of data structures for redisplay.
   Copyright (C) 1985, 1986, 1987, 1988, 1990, 1992 Free Software Foundation, Inc.

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


#include <signal.h>

#include "config.h"
#include <stdio.h>
#include <ctype.h>

#include "termchar.h"
#include "termopts.h"
#include "cm.h"
#include "lisp.h"
#include "dispextern.h"
#include "buffer.h"
#include "screen.h"
#include "window.h"
#include "commands.h"
#include "disptab.h"
#include "indent.h"

#include "systerm.h"
#include "systime.h"

#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#endif	/* HAVE_X_WINDOWS */

#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))

#ifndef PENDING_OUTPUT_COUNT
/* Get number of chars of output now in the buffer of a stdio stream.
   This ought to be built in in stdio, but it isn't.
   Some s- files override this because their stdio internals differ.  */
#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_ptr - (FILE)->_base)
#endif

/* Nonzero upon entry to redisplay means do not assume anything about
   current contents of actual terminal screen; clear and redraw it.  */

int screen_garbaged;

/* Nonzero means last display completed.  Zero means it was preempted. */

int display_completed;

/* Lisp variable visible-bell; enables use of screen-flash
   instead of audible bell.  */

int visible_bell;

/* Invert the color of the whole screen, at a low level.  */

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

/* The currently selected screen.
   In a single-screen version, this variable always remains 0.  */

SCREEN_PTR selected_screen;

/* In a single-screen version, the information that would otherwise
   exist inside a `struct screen' lives in the following variables instead.  */

#ifndef MULTI_SCREEN

/* Desired terminal cursor position (to show position of point),
   origin zero */

int cursX, cursY;

/* Description of current screen contents */

struct screen_glyphs *current_glyphs;

/* Description of desired screen contents */

struct screen_glyphs *desired_glyphs;

#endif /* not MULTI_SCREEN */

/* This is a vector, made larger whenever it isn't large enough,
   which is used inside `update_screen' to hold the old contents
   of the SCREEN_PHYS_LINES of the screen being updated.  */
struct screen_glyphs **ophys_lines;
/* Length of vector currently allocated.  */
int ophys_lines_length;

FILE *termscript;	/* Stdio stream being used for copy of all output.  */

struct cm Wcm;		/* Structure for info on cursor positioning */

extern short ospeed;	/* Output speed (from sg_ospeed) */

int in_display;		/* 1 if in redisplay: can't handle SIGWINCH now.  */

int delayed_size_change;  /* 1 means SIGWINCH happened when not safe.  */
int delayed_screen_height;  /* Remembered new screen height.  */
int delayed_screen_width;   /* Remembered new screen width.  */

#ifdef MULTI_SCREEN

DEFUN ("redraw-screen", Fredraw_screen, Sredraw_screen, 1, 1, 0,
  "Clear screen SCREEN and output again what is supposed to appear on it.")
  (screen)
     Lisp_Object screen;
{
  SCREEN_PTR s;

  CHECK_LIVE_SCREEN (screen, 0);
  s = XSCREEN (screen);
  update_begin (s);
  /*  set_terminal_modes (); */
  clear_screen ();
  update_end (s);
  fflush (stdout);
  clear_screen_records (s);
  windows_or_buffers_changed++;
  /* Mark all windows as INaccurate,
     so that every window will have its redisplay done.  */
  mark_window_display_accurate (SCREEN_ROOT_WINDOW (s), 0);
  s->garbaged = 0;
  return Qnil;
}

DEFUN ("redraw-display", Fredraw_display, Sredraw_display, 0, 0, "",
  "Redraw all screens marked as having their images garbled.")
  ()
{
  Lisp_Object screen, tail;

  for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      screen = XCONS (tail)->car;
      if (XSCREEN (screen)->garbaged && XSCREEN (screen)->visible)
	Fredraw_screen (screen);
    }
  return Qnil;
}

redraw_screen (s)
     SCREEN_PTR s;
{
  Lisp_Object screen;
  XSET (screen, Lisp_Screen, s);
  Fredraw_screen (screen);
}

#else /* not MULTI_SCREEN */

DEFUN ("redraw-display", Fredraw_display, Sredraw_display, 0, 0, 0,
  "Clear screen and output again what is supposed to appear on it.")
  ()
{
  update_begin (0);
  set_terminal_modes ();
  clear_screen ();
  update_end (0);
  fflush (stdout);
  clear_screen_records (0);
  windows_or_buffers_changed++;
  /* Mark all windows as INaccurate,
     so that every window will have its redisplay done.  */
  mark_window_display_accurate (XWINDOW (minibuf_window)->prev, 0);
  return Qnil;
}

#endif /* not MULTI_SCREEN */

static struct screen_glyphs *
make_screen_glyphs (screen, empty)
     register SCREEN_PTR screen;
     int empty;
{
  register int i;
  register width = SCREEN_WIDTH (screen);
  register height = SCREEN_HEIGHT (screen);
  register struct screen_glyphs *new =
    (struct screen_glyphs *) xmalloc (sizeof (struct screen_glyphs));

  SET_GLYPHS_SCREEN (new, screen);
  new->height = height;
  new->width = width;
  new->used = (int *) xmalloc (height * sizeof (int));
  new->glyphs = (GLYPH **) xmalloc (height * sizeof (GLYPH *));
  new->highlight = (char *) xmalloc (height * sizeof (char));
  new->enable = (char *) xmalloc (height * sizeof (char));
  bzero (new->enable, height * sizeof (char));
  new->bufp = (int *) xmalloc (height * sizeof (int));

#ifdef HAVE_X_WINDOWS
  if (SCREEN_IS_X (screen))
    {
      new->nruns = (int *) xmalloc (height * sizeof (int));
      new->face_list
	= (struct run **) xmalloc (height * sizeof (struct run *));
      new->top_left_x = (short *) xmalloc (height * sizeof (short));
      new->top_left_y = (short *) xmalloc (height * sizeof (short));
      new->pix_width = (short *) xmalloc (height * sizeof (short));
      new->pix_height = (short *) xmalloc (height * sizeof (short));
    }
#endif

  if (empty)
    {
      /* Make the buffer used by decode_mode_spec.  This buffer is also
         used as temporary storage when updating the screen.  See scroll.c. */
      unsigned int total_glyphs = (width + 2) * sizeof (GLYPH);

      new->total_contents = (GLYPH *) xmalloc (total_glyphs);
      bzero (new->total_contents, total_glyphs);
    }
  else
    {
      unsigned int total_glyphs = height * (width + 2) * sizeof (GLYPH);

      new->total_contents = (GLYPH *) xmalloc (total_glyphs);
      bzero (new->total_contents, total_glyphs);
      for (i = 0; i < height; i++)
	new->glyphs[i] = new->total_contents + i * (width + 2) + 1;
    }

  return new;
}

static void
free_screen_glyphs (screen, glyphs)
     SCREEN_PTR screen;
     struct screen_glyphs *glyphs;
{
  if (glyphs->total_contents)
    free (glyphs->total_contents);

  free (glyphs->used);
  free (glyphs->glyphs);
  free (glyphs->highlight);
  free (glyphs->enable);
  free (glyphs->bufp);

#ifdef HAVE_X_WINDOWS
  if (SCREEN_IS_X (screen))
    {
      free (glyphs->nruns);
      free (glyphs->face_list);
      free (glyphs->top_left_x);
      free (glyphs->top_left_y);
      free (glyphs->pix_width);
      free (glyphs->pix_height);
    }
#endif

  free (glyphs);
}

static void
remake_screen_glyphs (screen)
     SCREEN_PTR screen;
{
  if (SCREEN_CURRENT_GLYPHS (screen))
    free_screen_glyphs (screen, SCREEN_CURRENT_GLYPHS (screen));
  if (SCREEN_DESIRED_GLYPHS (screen))
    free_screen_glyphs (screen, SCREEN_DESIRED_GLYPHS (screen));
  if (SCREEN_TEMP_GLYPHS (screen))
    free_screen_glyphs (screen, SCREEN_TEMP_GLYPHS (screen));

  if (SCREEN_MESSAGE_BUF (screen))
    SCREEN_MESSAGE_BUF (screen)
      = (char *) xrealloc (SCREEN_MESSAGE_BUF (screen),
			   SCREEN_WIDTH (screen) + 1);
  else
    SCREEN_MESSAGE_BUF (screen)
      = (char *) xmalloc (SCREEN_WIDTH (screen) + 1);

  SCREEN_CURRENT_GLYPHS (screen) = make_screen_glyphs (screen, 0);
  SCREEN_DESIRED_GLYPHS (screen) = make_screen_glyphs (screen, 0);
  SCREEN_TEMP_GLYPHS (screen) = make_screen_glyphs (screen, 1);
  SET_SCREEN_GARBAGED (screen);
}

/* Return the hash code of contents of line VPOS in screen-matrix M.  */

static int
line_hash_code (m, vpos)
     register struct screen_glyphs *m;
     int vpos;
{
  register GLYPH *body, *end;
  register int h = 0;

  if (!m->enable[vpos])
    return 0;

  /* Give all lighlighted lines the same hash code
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
     struct screen_glyphs *m;
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
   SCREEN_CURSOR_X (screen) and SCREEN_CURSOR_Y (screen)
   and SET_SCREEN_GARBAGED (screen).  */

/* cancel_line eliminates any request to display a line at position `vpos' */

cancel_line (vpos, screen)
     int vpos;
     register SCREEN_PTR screen;
{
  SCREEN_DESIRED_GLYPHS (screen)->enable[vpos] = 0;
}

clear_screen_records (screen)
     register SCREEN_PTR screen;
{
  bzero (SCREEN_CURRENT_GLYPHS (screen)->enable, SCREEN_HEIGHT (screen));
}

/* Prepare to display on line VPOS starting at HPOS within it.  */

void
get_display_line (screen, vpos, hpos)
     register SCREEN_PTR screen;
     int vpos;
     register int hpos;
{
  register struct screen_glyphs *glyphs;
  register struct screen_glyphs *desired_glyphs = SCREEN_DESIRED_GLYPHS (screen);
  register GLYPH *p;

  if (vpos < 0 || (! SCREEN_VISIBLE_P (screen)))
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
  register char *endf;
  register char *endt;

  if (size == 0)
    return;

  /* If destination is higher in memory, and overlaps source zone,
     copy from the end.  */
  if (from < to && from + size > to)
    {
      endf = from + size;
      endt = to + size;

      /* If TO - FROM is large, then we should break the copy into
	 nonoverlapping chunks of TO - FROM bytes each.  However, if
	 TO - FROM is small, then the bcopy function call overhead
	 makes this not worth it.  The crossover point could be about
	 anywhere.  Since I don't think the obvious copy loop is ever
	 too bad, I'm trying to err in its favor.  */
      if (to - from < 64)
	{
	  do
	    *--endt = *--endf;
	  while (endf != from);
	}
      else
	{
	  /* Since TO - FROM >= 64, the overlap is less than SIZE,
	     so we can always safely do this loop once.  */
	  while (endt > to)
	    {
	      endt -= (to - from);
	      endf -= (to - from);

	      bcopy (endf, endt, to - from);
	    }
	  
	  /* If TO - FROM wasn't a multiple of SIZE, there will be a
	     little left over.  The amount left over is
	     (endt + (to - from)) - to, which is endt - from.  */
	  bcopy (from, to, endt - from);
	}
    }
  else
    bcopy (from, to, size);
}     

#if 0
void
safe_bcopy (from, to, size)
     char *from, *to;
     int size;
{
  register char *endf;
  register char *endt;

  if (size == 0)
    return;

  /* If destination is higher in memory, and overlaps source zone,
     copy from the end. */
  if (from < to && from + size > to)
    {
      endf = from + size;
      endt = to + size;

      do
	*--endt = *--endf;
      while (endf != from);

      return;
    }

  bcopy (from, to, size);
}
#endif

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
scroll_screen_lines (screen, from, end, amount)
     register SCREEN_PTR screen;
     int from, end, amount;
{
  register int i;
  register struct screen_glyphs *current_screen
    = SCREEN_CURRENT_GLYPHS (screen);

  if (!line_ins_del_ok)
    return 0;

  if (amount == 0)
    return 1;

  if (amount > 0)
    {
      update_begin (screen);
      set_terminal_window (end + amount);
      if (!scroll_region_ok)
	ins_del_lines (end, -amount);
      ins_del_lines (from, amount);
      set_terminal_window (0);

      rotate_vector (current_screen->glyphs + from,
		     sizeof (GLYPH *) * (end + amount - from),
		     amount * sizeof (GLYPH *));

      safe_bcopy (current_screen->used + from,
		  current_screen->used + from + amount,
		  (end - from) * sizeof current_screen->used[0]);

      safe_bcopy (current_screen->highlight + from,
		  current_screen->highlight + from + amount,
		  (end - from) * sizeof current_screen->highlight[0]);

      safe_bcopy (current_screen->enable + from,
		  current_screen->enable + from + amount,
		  (end - from) * sizeof current_screen->enable[0]);

      /* Mark the lines made empty by scrolling as enabled, empty and
	 normal video.  */
      bzero (current_screen->used + from,
	     amount * sizeof current_screen->used[0]);
      bzero (current_screen->highlight + from,
	     amount * sizeof current_screen->highlight[0]);
      for (i = from; i < from + amount; i++)
	{
	  current_screen->glyphs[i][0] = '\0';
	  current_screen->enable[i] = 1;
	}

      safe_bcopy (current_screen->bufp + from,
		  current_screen->bufp + from + amount,
		  (end - from) * sizeof current_screen->bufp[0]);

#ifdef HAVE_X_WINDOWS
      if (SCREEN_IS_X (screen))
	{
	  safe_bcopy (current_screen->nruns + from,
		      current_screen->nruns + from + amount,
		      (end - from) * sizeof current_screen->nruns[0]);

	  safe_bcopy (current_screen->face_list + from,
		      current_screen->face_list + from + amount,
		      (end - from) * sizeof current_screen->face_list[0]);

	  safe_bcopy (current_screen->top_left_x + from,
		      current_screen->top_left_x + from + amount,
		      (end - from) * sizeof current_screen->top_left_x[0]);

	  safe_bcopy (current_screen->top_left_y + from,
		      current_screen->top_left_y + from + amount,
		      (end - from) * sizeof current_screen->top_left_y[0]);

	  safe_bcopy (current_screen->pix_width + from,
		      current_screen->pix_width + from + amount,
		      (end - from) * sizeof current_screen->pix_width[0]);

	  safe_bcopy (current_screen->pix_height + from,
		      current_screen->pix_height + from + amount,
		      (end - from) * sizeof current_screen->pix_height[0]);
	}
#endif				/* HAVE_X_WINDOWS */

      update_end (screen);
    }
  if (amount < 0)
    {
      update_begin (screen);
      set_terminal_window (end);
      ins_del_lines (from + amount, amount);
      if (!scroll_region_ok)
	ins_del_lines (end + amount, -amount);
      set_terminal_window (0);

      rotate_vector (current_screen->glyphs + from + amount,
		     sizeof (GLYPH *) * (end - from - amount),
		     amount * sizeof (GLYPH *));

      safe_bcopy (current_screen->used + from,
		  current_screen->used + from + amount,
		  (end - from) * sizeof current_screen->used[0]);

      safe_bcopy (current_screen->highlight + from,
		  current_screen->highlight + from + amount,
		  (end - from) * sizeof current_screen->highlight[0]);

      safe_bcopy (current_screen->enable + from,
		  current_screen->enable + from + amount,
		  (end - from) * sizeof current_screen->enable[0]);

      /* Mark the lines made empty by scrolling as enabled, empty and
	 normal video.  */
      bzero (current_screen->used + end + amount,
	     - amount * sizeof current_screen->used[0]);
      bzero (current_screen->highlight + end + amount,
	     - amount * sizeof current_screen->highlight[0]);
      for (i = end + amount; i < end; i++)
	{
	  current_screen->glyphs[i][0] = '\0';
	  current_screen->enable[i] = 1;
	}

      safe_bcopy (current_screen->bufp + from,
		  current_screen->bufp + from + amount,
		  (end - from) * sizeof current_screen->bufp[0]);

#ifdef HAVE_X_WINDOWS
      if (SCREEN_IS_X (screen))
	{
	  safe_bcopy (current_screen->nruns + from,
		      current_screen->nruns + from + amount,
		      (end - from) * sizeof current_screen->nruns[0]);

	  safe_bcopy (current_screen->face_list + from,
		      current_screen->face_list + from + amount,
		      (end - from) * sizeof current_screen->face_list[0]);

	  safe_bcopy (current_screen->top_left_x + from,
		      current_screen->top_left_x + from + amount,
		      (end - from) * sizeof current_screen->top_left_x[0]);

	  safe_bcopy (current_screen->top_left_y + from,
		      current_screen->top_left_y + from + amount,
		      (end - from) * sizeof current_screen->top_left_y[0]);

	  safe_bcopy (current_screen->pix_width + from,
		      current_screen->pix_width + from + amount,
		      (end - from) * sizeof current_screen->pix_width[0]);

	  safe_bcopy (current_screen->pix_height + from,
		      current_screen->pix_height + from + amount,
		      (end - from) * sizeof current_screen->pix_height[0]);
	}
#endif				/* HAVE_X_WINDOWS */

      update_end (screen);
    }
  return 1;
}

/* After updating a window W that isn't the full screen wide,
   copy all the columns that W does not occupy
   into the SCREEN_DESIRED_GLYPHS (screen) from the SCREEN_PHYS_GLYPHS (screen)
   so that update_screen will not change those columns.  */

preserve_other_columns (w)
     struct window *w;
{
  register int vpos;
  register struct screen_glyphs *current_screen, *desired_screen;
  register SCREEN_PTR screen = XSCREEN (w->screen);
  int start = XFASTINT (w->left);
  int end = XFASTINT (w->left) + XFASTINT (w->width);
  int bot = XFASTINT (w->top) + XFASTINT (w->height);

  current_screen = SCREEN_CURRENT_GLYPHS (screen);
  desired_screen = SCREEN_DESIRED_GLYPHS (screen);

  for (vpos = XFASTINT (w->top); vpos < bot; vpos++)
    {
      if (current_screen->enable[vpos] && desired_screen->enable[vpos])
	{
	  if (start > 0)
	    {
	      int len;

	      bcopy (current_screen->glyphs[vpos],
		     desired_screen->glyphs[vpos], start);
	      len = min (start, current_screen->used[vpos]);
	      if (desired_screen->used[vpos] < len)
		desired_screen->used[vpos] = len;
	    }
	  if (current_screen->used[vpos] > end
	      && desired_screen->used[vpos] < current_screen->used[vpos])
	    {
	      while (desired_screen->used[vpos] < end)
		desired_screen->glyphs[vpos][desired_screen->used[vpos]++]
		  = SPACEGLYPH;
	      bcopy (current_screen->glyphs[vpos] + end,
		     desired_screen->glyphs[vpos] + end,
		     current_screen->used[vpos] - end);
	      desired_screen->used[vpos] = current_screen->used[vpos];
	    }
	}
    }
}

#if 0

/* If window w does not need to be updated and isn't the full screen wide,
 copy all the columns that w does occupy
 into the SCREEN_DESIRED_LINES (screen) from the SCREEN_PHYS_LINES (screen)
 so that update_screen will not change those columns.

 Have not been able to figure out how to use this correctly.  */

preserve_my_columns (w)
     struct window *w;
{
  register int vpos, fin;
  register struct screen_glyphs *l1, *l2;
  register SCREEN_PTR screen = XSCREEN (w->screen);
  int start = XFASTINT (w->left);
  int end = XFASTINT (w->left) + XFASTINT (w->width);
  int bot = XFASTINT (w->top) + XFASTINT (w->height);

  for (vpos = XFASTINT (w->top); vpos < bot; vpos++)
    {
      if ((l1 = SCREEN_DESIRED_GLYPHS (screen)->glyphs[vpos + 1])
	  && (l2 = SCREEN_PHYS_GLYPHS (screen)->glyphs[vpos + 1]))
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

/* On discovering that the redisplay for a window was no good,
   cancel the columns of that window, so that when the window is
   displayed over again get_display_line will not complain.  */

cancel_my_columns (w)
     struct window *w;
{
  register int vpos;
  register SCREEN_PTR screen = XSCREEN (w->screen);
  register struct screen_glyphs *desired_glyphs = screen->desired_glyphs;
  register int start = XFASTINT (w->left);
  register int bot = XFASTINT (w->top) + XFASTINT (w->height);

  for (vpos = XFASTINT (w->top); vpos < bot; vpos++)
    if (desired_glyphs->enable[vpos]
	&& desired_glyphs->used[vpos] >= start)
      desired_glyphs->used[vpos] = start;
}

/* These functions try to perform directly and immediately on the screen
   the necessary output for one change in the buffer.
   They may return 0 meaning nothing was done if anything is difficult,
   or 1 meaning the output was performed properly.
   They assume that the screen was up to date before the buffer
   change being displayed.  THey make various other assumptions too;
   see command_loop_1 where these are called.  */

int
direct_output_for_insert (g)
     int g;
{
  register SCREEN_PTR screen = selected_screen;
  register struct screen_glyphs *current_screen
    = SCREEN_CURRENT_GLYPHS (screen);

#ifndef COMPILER_REGISTER_BUG
  register
#endif /* COMPILER_REGISTER_BUG */
    struct window *w = XWINDOW (selected_window);
#ifndef COMPILER_REGISTER_BUG
  register
#endif /* COMPILER_REGISTER_BUG */
    int hpos = SCREEN_CURSOR_X (screen);
#ifndef COMPILER_REGISTER_BUG
  register
#endif /* COMPILER_REGISTER_BUG */
    int vpos = SCREEN_CURSOR_Y (screen);

  /* Give up if about to continue line */
  if (hpos - XFASTINT (w->left) + 1 + 1 >= XFASTINT (w->width)

  /* Avoid losing if cursor is in invisible text off left margin */
      || (XINT (w->hscroll) && hpos == XFASTINT (w->left))
    
  /* Give up if cursor outside window (in minibuf, probably) */
      || SCREEN_CURSOR_Y (screen) < XFASTINT (w->top)
      || SCREEN_CURSOR_Y (screen) >= XFASTINT (w->top) + XFASTINT (w->height)

  /* Give up if cursor not really at SCREEN_CURSOR_X, SCREEN_CURSOR_Y */
      || !display_completed

  /* Give up if buffer appears in two places.  */
      || buffer_shared > 1

  /* Give up if w is minibuffer and a message is being displayed there */
      || (MINI_WINDOW_P (w) && echo_area_glyphs))
    return 0;

  current_screen->glyphs[vpos][hpos] = g;
  unchanged_modified = MODIFF;
  beg_unchanged = GPT - BEG;
  XFASTINT (w->last_point) = point;
  XFASTINT (w->last_point_x) = hpos;
  XFASTINT (w->last_modified) = MODIFF;

  reassert_line_highlight (0, vpos);
  write_glyphs (&current_screen->glyphs[vpos][hpos], 1);
  fflush (stdout);
  ++SCREEN_CURSOR_X (screen);
  if (hpos == current_screen->used[vpos])
    {
      current_screen->used[vpos] = hpos + 1;
      current_screen->glyphs[vpos][hpos + 1] = 0;
    }

  return 1;
}

int
direct_output_forward_char (n)
     int n;
{
  register SCREEN_PTR screen = selected_screen;
  register struct window *w = XWINDOW (selected_window);

  /* Avoid losing if cursor is in invisible text off left margin
     or about to go off either side of window.  */
  if ((SCREEN_CURSOR_X (screen) == XFASTINT (w->left)
       && (XINT (w->hscroll) || n < 0))
      || (n > 0
	  && (SCREEN_CURSOR_X (screen) + 1
	      >= (XFASTINT (w->left) + XFASTINT (w->width)
		  - (XFASTINT (w->width) < SCREEN_WIDTH (screen))
		  - 1))))
    return 0;

  SCREEN_CURSOR_X (screen) += n;
  XFASTINT (w->last_point_x) = SCREEN_CURSOR_X (screen);
  XFASTINT (w->last_point) = point;
  cursor_to (SCREEN_CURSOR_Y (screen), SCREEN_CURSOR_X (screen));
  fflush (stdout);
  return 1;
}

static void update_line ();

/* Update screen S based on the data in SCREEN_DESIRED_GLYPHS.
   Value is nonzero if redisplay stopped due to pending input.
   FORCE nonzero means do not stop for pending input.  */

int
update_screen (s, force, inhibit_hairy_id)
     SCREEN_PTR s;
     int force;
     int inhibit_hairy_id;
{
  register struct screen_glyphs *current_screen = SCREEN_CURRENT_GLYPHS (s);
  register struct screen_glyphs *desired_screen = SCREEN_DESIRED_GLYPHS (s);
  register int i;
  int pause;
  int preempt_count = baud_rate / 2400 + 1;
  extern input_pending;
#ifdef HAVE_X_WINDOWS
  register int downto, leftmost;
#endif

  if (SCREEN_HEIGHT (s) == 0) abort (); /* Some bug zeros some core */

  detect_input_pending ();
  if (input_pending && !force)
    {
      pause = 1;
      goto do_pause;
    }

  update_begin (s);

  if (!line_ins_del_ok)
    inhibit_hairy_id = 1;

  /* See if any of the desired lines are enabled; don't compute for
     i/d line if just want cursor motion. */
  for (i = 0; i < SCREEN_HEIGHT (s); i++)
    if (desired_screen->enable[i])
      break;

  /* Try doing i/d line, if not yet inhibited.  */
  if (!inhibit_hairy_id && i < SCREEN_HEIGHT (s))
    force |= scrolling (s);

  /* Update the individual lines as needed.  Do bottom line first.  */

  if (desired_screen->enable[SCREEN_HEIGHT (s) - 1])
    update_line (s, SCREEN_HEIGHT (s) - 1);

#ifdef HAVE_X_WINDOWS
  if (SCREEN_IS_X (s))
    {
      leftmost = downto = s->display.x->internal_border_width;
      if (desired_screen->enable[0])
	{
	  current_screen->top_left_x[SCREEN_HEIGHT (s) - 1] = leftmost;
	  current_screen->top_left_y[SCREEN_HEIGHT (s) - 1]
	    = PIXEL_HEIGHT (s) - s->display.x->internal_border_width
	      - LINE_HEIGHT(s, SCREEN_HEIGHT (s) - 1);
	  current_screen->top_left_x[0] = leftmost;
	  current_screen->top_left_y[0] = downto;
	}
    }
#endif /* HAVE_X_WINDOWS */

  /* Now update the rest of the lines. */
  for (i = 0; i < SCREEN_HEIGHT (s) - 1 && (force || !input_pending); i++)
    {
      if (desired_screen->enable[i])
	{
	  if (SCREEN_IS_TERMCAP (s))
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
		      sleep (outq / baud_rate);
		    }
		}
	      if ((i - 1) % preempt_count == 0)
		detect_input_pending ();
	    }

	  update_line (s, i);
#ifdef HAVE_X_WINDOWS
	  if (SCREEN_IS_X (s))
	    {
	      current_screen->top_left_y[i] = downto;
	      current_screen->top_left_x[i] = leftmost;
	    }
#endif /* HAVE_X_WINDOWS */
	}

      if (SCREEN_IS_X (s))
	downto += LINE_HEIGHT(s, i);
    }
  pause = (i < SCREEN_HEIGHT (s) - 1) ? i : 0;

  /* Now just clean up termcap drivers and set cursor, etc.  */
  if (!pause)
    {
      if (cursor_in_echo_area)
	{
	  if (s == selected_screen
	      && cursor_in_echo_area < 0)
	    cursor_to (SCREEN_HEIGHT (s) - 1, 0);
	  else if (s == selected_screen
		   && ! current_screen->enable[SCREEN_HEIGHT (s) - 1])
	    cursor_to (SCREEN_HEIGHT (s) - 1, 0);
	  else
	    cursor_to (SCREEN_HEIGHT (s) - 1,
		       min (SCREEN_WIDTH (s) - 1,
			    current_screen->used[SCREEN_HEIGHT (s) - 1]));
	}
      else
	cursor_to (SCREEN_CURSOR_Y (s), max (min (SCREEN_CURSOR_X (s),
						  SCREEN_WIDTH (s) - 1), 0));
    }

  update_end (s);

  if (termscript)
    fflush (termscript);
  fflush (stdout);

  /* Here if output is preempted because input is detected.  */
 do_pause:

  if (SCREEN_HEIGHT (s) == 0) abort (); /* Some bug zeros some core */
  display_completed = !pause;

  bzero (desired_screen->enable, SCREEN_HEIGHT (s));
  return pause;
}

/* Called when about to quit, to check for doing so
   at an improper time.  */

void
quit_error_check ()
{
  if (SCREEN_DESIRED_GLYPHS (selected_screen) == 0)
    return;
  if (SCREEN_DESIRED_GLYPHS (selected_screen)->enable[0])
    abort ();
  if (SCREEN_DESIRED_GLYPHS (selected_screen)->enable[SCREEN_HEIGHT (selected_screen) - 1])
    abort ();
}

/* Decide what insert/delete line to do, and do it */

extern void scrolling_1 ();

scrolling (screen)
     SCREEN_PTR screen;
{
  int unchanged_at_top, unchanged_at_bottom;
  int window_size;
  int changed_lines;
  int *old_hash = (int *) alloca (SCREEN_HEIGHT (screen) * sizeof (int));
  int *new_hash = (int *) alloca (SCREEN_HEIGHT (screen) * sizeof (int));
  int *draw_cost = (int *) alloca (SCREEN_HEIGHT (screen) * sizeof (int));
  register int i;
  int free_at_end_vpos = SCREEN_HEIGHT (screen);
  register struct screen_glyphs *current_screen = SCREEN_CURRENT_GLYPHS (screen);
  register struct screen_glyphs *desired_screen = SCREEN_DESIRED_GLYPHS (screen);

  /* Compute hash codes of all the lines.
     Also calculate number of changed lines,
     number of unchanged lines at the beginning,
     and number of unchanged lines at the end.  */

  changed_lines = 0;
  unchanged_at_top = 0;
  unchanged_at_bottom = SCREEN_HEIGHT (screen);
  for (i = 0; i < SCREEN_HEIGHT (screen); i++)
    {
      /* Give up on this scrolling if some old lines are not enabled.  */
      if (!current_screen->enable[i])
	return 0;
      old_hash[i] = line_hash_code (current_screen, i);
      if (! desired_screen->enable[i])
	new_hash[i] = old_hash[i];
      else
	new_hash[i] = line_hash_code (desired_screen, i);

      if (old_hash[i] != new_hash[i])
	{
	  changed_lines++;
	  unchanged_at_bottom = SCREEN_HEIGHT (screen) - i - 1;
	}
      else if (i == unchanged_at_top)
	unchanged_at_top++;
      draw_cost[i] = line_draw_cost (desired_screen, i);
    }

  /* If changed lines are few, don't allow preemption, don't scroll.  */
  if (changed_lines < baud_rate / 2400
      || unchanged_at_bottom == SCREEN_HEIGHT (screen))
    return 1;

  window_size = (SCREEN_HEIGHT (screen) - unchanged_at_top
		 - unchanged_at_bottom);

  if (scroll_region_ok)
    free_at_end_vpos -= unchanged_at_bottom;
  else if (memory_below_screen)
    free_at_end_vpos = -1;

  /* If large window, fast terminal and few lines in common between
     current screen and desired screen, don't bother with i/d calc. */
  if (window_size >= 18 && baud_rate > 2400
      && (window_size >=
	  10 * scrolling_max_lines_saved (unchanged_at_top,
					  SCREEN_HEIGHT (screen) - unchanged_at_bottom,
					  old_hash, new_hash, draw_cost)))
    return 0;

  scrolling_1 (screen, window_size, unchanged_at_top, unchanged_at_bottom,
	       draw_cost + unchanged_at_top - 1,
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
  int window_left = XFASTINT (window->left);

  /* The actual width of the window is window->width less one for the
     DISP_CONTINUE_GLYPH, and less one if it's not the rightmost
     window.  */
  int window_width = (XFASTINT (window->width) - 1
		      - (XFASTINT (window->width) + window_left
			 != SCREEN_WIDTH (XSCREEN (window->screen))));

  int startp = marker_position (window->start);

  /* Since compute_motion will only operate on the current buffer,
     we need to save the old one and restore it when we're done.  */
  struct buffer *old_current_buffer = current_buffer;
  struct position *posn;

  current_buffer = XBUFFER (window->buffer);

  /* It would be nice if we could use SCREEN_CURRENT_GLYPHS (XSCREEN
     (window->screen))->bufp to avoid scanning from the very top of
     the window, but it isn't maintained correctly, and I'm not even
     sure I will keep it.  */
  posn = compute_motion (startp, 0,
			 (window == XWINDOW (minibuf_window) && startp == 1
			  ? minibuf_prompt_width : 0),
			 ZV, line, col - window_left,
			 window_width, XINT (window->hscroll), 0);

  current_buffer = old_current_buffer;

  /* compute_motion considers screen points past the end of a line
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
  while (*r++ == SPACEGLYPH);
  return r - p - 1;
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

#define char_ins_del_cost(s) (&char_ins_del_vector[SCREEN_HEIGHT((s))])

static void
update_line (screen, vpos)
     register SCREEN_PTR screen;
     int vpos;
{
  register GLYPH *obody, *nbody, *op1, *op2, *np1, *temp;
  int tem;
  int osp, nsp, begmatch, endmatch, olen, nlen;
  int save;
  register struct screen_glyphs *current_screen
    = SCREEN_CURRENT_GLYPHS (screen);
  register struct screen_glyphs *desired_screen
    = SCREEN_DESIRED_GLYPHS (screen);

  if (desired_screen->highlight[vpos]
      != (current_screen->enable[vpos] && current_screen->highlight[vpos]))
    {
      change_line_highlight (desired_screen->highlight[vpos], vpos,
			     (current_screen->enable[vpos] ?
			      current_screen->used[vpos] : 0));
      current_screen->enable[vpos] = 0;
    }
  else
    reassert_line_highlight (desired_screen->highlight[vpos], vpos);

  if (! current_screen->enable[vpos])
    {
      olen = 0;
    }
  else
    {
      obody = current_screen->glyphs[vpos];
      olen = current_screen->used[vpos];
      if (! current_screen->highlight[vpos])
	{
	  if (!must_write_spaces)
	    while (obody[olen - 1] == SPACEGLYPH && olen > 0)
	      olen--;
	}
      else
	{
	  /* For an inverse-video line, remember we gave it
	     spaces all the way to the screen edge
	     so that the reverse video extends all the way across.  */

	  while (olen < SCREEN_WIDTH (screen) - 1)
	    obody[olen++] = SPACEGLYPH;
	}
    }

  /* One way or another, this will enable the line being updated.  */
  current_screen->enable[vpos] = 1;
  current_screen->used[vpos] = desired_screen->used[vpos];
  current_screen->highlight[vpos] = desired_screen->highlight[vpos];
  current_screen->bufp[vpos] = desired_screen->bufp[vpos];

#ifdef HAVE_X_WINDOWS
  if (SCREEN_IS_X (screen))
    {
      current_screen->pix_width[vpos]
	= current_screen->used[vpos]
	  * FONT_WIDTH (screen->display.x->font);
      current_screen->pix_height[vpos]
	= FONT_HEIGHT (screen->display.x->font);
    }
#endif /* HAVE_X_WINDOWS */

  if (!desired_screen->enable[vpos])
    {
      nlen = 0;
      goto just_erase;
    }

  nbody = desired_screen->glyphs[vpos];
  nlen = desired_screen->used[vpos];

  /* Pretend trailing spaces are not there at all,
     unless for one reason or another we must write all spaces.  */
  if (! desired_screen->highlight[vpos])
    {
      if (!must_write_spaces)
	/* We know that the previous character byte contains 0.  */
	while (nbody[nlen - 1] == SPACEGLYPH)
	  nlen--;
    }
  else
    {
      /* For an inverse-video line, give it extra trailing spaces
	 all the way to the screen edge
	 so that the reverse video extends all the way across.  */

      while (nlen < SCREEN_WIDTH (screen) - 1)
	nbody[nlen++] = SPACEGLYPH;
    }

  /* If there's no i/d char, quickly do the best we can without it.  */
  if (!char_ins_del_ok)
    {
      int i,j;

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

      /* Exchange contents between current_screen and new_screen.  */
      temp = desired_screen->glyphs[vpos];
      desired_screen->glyphs[vpos] = current_screen->glyphs[vpos];
      current_screen->glyphs[vpos] = temp;

      return;
    }

  if (!olen)
    {
      nsp = (must_write_spaces || desired_screen->highlight[vpos])
	      ? 0 : count_blanks (nbody);
      if (nlen > nsp)
	{
	  cursor_to (vpos, nsp);
	  write_glyphs (nbody + nsp, nlen - nsp);
	}

      /* Exchange contents between current_screen and new_screen.  */
      temp = desired_screen->glyphs[vpos];
      desired_screen->glyphs[vpos] = current_screen->glyphs[vpos];
      current_screen->glyphs[vpos] = temp;

      return;
    }

  obody[olen] = 1;
  save = nbody[nlen];
  nbody[nlen] = 0;

  /* Compute number of leading blanks in old and new contents.  */
  osp = count_blanks (obody);
  if (!desired_screen->highlight[vpos])
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
      && (!char_ins_del_ok || endmatch <= char_ins_del_cost (screen)[tem]))
    endmatch = 0;

  /* nsp - osp is the distance to insert or delete.
     If that is nonzero, begmatch is known to be nonzero also.
     begmatch + endmatch is how much we save by doing the ins/del.
     Is it worth it?  */

  if (nsp != osp
      && (!char_ins_del_ok
	  || begmatch + endmatch <= char_ins_del_cost (screen)[nsp - osp]))
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
	  if (nlen == SCREEN_WIDTH (screen))
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
	  if (nlen == SCREEN_WIDTH (screen) && fast_clear_end_of_line && olen > nlen)
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

  /* Exchange contents between current_screen and new_screen.  */
  temp = desired_screen->glyphs[vpos];
  desired_screen->glyphs[vpos] = current_screen->glyphs[vpos];
  current_screen->glyphs[vpos] = temp;
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
window_change_signal ()
{
  int width, height;
  extern int errno;
  int old_errno = errno;

  get_screen_size (&width, &height);

  /* The screen size change obviously applies to a termcap-controlled
     screen.  Find such a screen in the list, and assume it's the only
     one (since the redisplay code always writes to stdout, not a
     FILE * specified in the screen structure).  Record the new size,
     but don't reallocate the data structures now.  Let that be done
     later outside of the signal handler.  */

  {
    Lisp_Object tail;

    for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
      {
	SCREEN_PTR s = XSCREEN (XCONS (tail)->car);
	
	if (SCREEN_IS_TERMCAP (s))
	  {
	    ++in_display;
	    change_screen_size (s, height, width, 0);
	    --in_display;
	    break;
	  }
      }
  }

  signal (SIGWINCH, window_change_signal);
  errno = old_errno;
}
#endif /* SIGWINCH */


/* Do any change in screen size that was requested by a signal.  */

do_pending_window_change ()
{
  /* If window_change_signal should have run before, run it now.  */
  while (delayed_size_change)
    {
      Lisp_Object tail;

      delayed_size_change = 0;

      for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
	{
	  SCREEN_PTR s = XSCREEN (XCONS (tail)->car);
	  int height = SCREEN_NEW_HEIGHT (s);
	  int width = SCREEN_NEW_WIDTH (s);
	    
	  SCREEN_NEW_HEIGHT (s) = 0;
	  SCREEN_NEW_WIDTH (s) = 0;

	  if (height != 0)
	    change_screen_size (s, height, width, 0);
	}
    }
}


/* Change the screen height and/or width.  Values may be given as zero to
   indicate no change is to take place. */

change_screen_size (screen, newlength, newwidth, pretend)
     register SCREEN_PTR screen;
     register int newlength, newwidth, pretend;
{
  /* If we can't deal with the change now, queue it for later.  */
  if (in_display)
    {
      SCREEN_NEW_HEIGHT (screen) = newlength;
      SCREEN_NEW_WIDTH (screen) = newwidth;
      delayed_size_change = 1;
      return;
    }

  /* This size-change overrides any pending one for this screen.  */
  SCREEN_NEW_HEIGHT (screen) = 0;
  SCREEN_NEW_WIDTH (screen) = 0;

  if ((newlength == 0 || newlength == SCREEN_HEIGHT (screen))
      && (newwidth == 0 || newwidth == SCREEN_WIDTH (screen)))
    return;

  if (newlength && newlength != SCREEN_HEIGHT (screen))
    {
      if (SCREEN_HAS_MINIBUF (screen)
	  && ! SCREEN_MINIBUF_ONLY_P (screen))
	{
	  /* Screen has both root and minibuffer.  */
	  set_window_height (SCREEN_ROOT_WINDOW (screen),
			     newlength - 1, 0);
	  XFASTINT (XWINDOW (SCREEN_MINIBUF_WINDOW (screen))->top)
	    = newlength - 1;
	  set_window_height (SCREEN_MINIBUF_WINDOW (screen), 1, 0);
	}
      else
	/* Screen has just one top-level window.  */
	set_window_height (SCREEN_ROOT_WINDOW (screen), newlength, 0);
	
      if (SCREEN_IS_TERMCAP (screen) && !pretend)
	ScreenRows = newlength;

#if 0
      if (screen->output_method == output_termcap)
	{
	  screen_height = newlength;
	  if (!pretend)
	    ScreenRows = newlength;
	}
#endif
    }

  if (newwidth && newwidth != SCREEN_WIDTH (screen))
    {
      set_window_width (SCREEN_ROOT_WINDOW (screen), newwidth, 0);
      if (SCREEN_HAS_MINIBUF (screen))
	set_window_width (SCREEN_MINIBUF_WINDOW (screen), newwidth, 0);
      SCREEN_WIDTH (screen) = newwidth;

      if (SCREEN_IS_TERMCAP (screen) && !pretend)
	ScreenCols = newwidth;
#if 0
      if (screen->output_method == output_termcap)
	{
	  screen_width = newwidth;
	  if (!pretend)
	    ScreenCols = newwidth;
	}
#endif
    }

  if (newlength)
    SCREEN_HEIGHT (screen) = newlength;

  remake_screen_glyphs (screen);
  calculate_costs (screen);
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
  "Pause, without updating display, for ARG seconds.\n\
Optional second arg non-nil means ARG is measured in milliseconds.\n\
\(Not all operating systems support milliseconds.)")
  (arg, millisec)
     Lisp_Object arg, millisec;
{
  int usec = 0;
  int sec;

  CHECK_NUMBER (arg, 0);
  sec = XINT (arg);
  if (sec <= 0)
    return Qnil;

  if (!NILP (millisec))
    {
#ifndef EMACS_HAS_USECS
      error ("millisecond `sleep-for' not supported on %s", SYSTEM_TYPE);
#else
      usec = sec % 1000 * 1000;
      sec /= 1000;
#endif
    }

  {
    Lisp_Object zero;

    XFASTINT (zero) = 0;
    wait_reading_process_input (sec, usec, zero, 0);
  }

#if 0 /* No wait_reading_process_input */
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

   It's also just like Fsit_for, except that it can be used for
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
  gobble_input ();
#endif

  XSET (read_kbd, Lisp_Int, reading ? -1 : 1);
  wait_reading_process_input (sec, usec, read_kbd, display);


#if 0 /* No wait_reading_process_input available.  */
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
  "Perform redisplay, then wait for ARG seconds or until input is available.\n\
Optional second arg non-nil means ARG counts in milliseconds.\n\
Optional third arg non-nil means don't redisplay, just wait for input.\n\
Redisplay is preempted as always if input arrives, and does not happen\n\
if input is available before it starts.\n\
Value is t if waited the full time with no input arriving.")
  (arg, millisec, nodisp)
     Lisp_Object arg, millisec, nodisp;
{
  int usec = 0;
  int sec;

  CHECK_NUMBER (arg, 0);
  sec = XINT (arg);

  if (!NILP (millisec))
    {
#ifndef EMACS_HAS_USECS
      error ("millisecond `sit-for' not supported on %s", SYSTEM_TYPE);
#else
      usec = (sec % 1000) * 1000;
      sec /= 1000;
#endif
    }

  return sit_for (sec, usec, 0, NILP (nodisp));
}

DEFUN ("sleep-for-millisecs", Fsleep_for_millisecs, Ssleep_for_millisecs,
  1, 1, 0,
  "Pause, without updating display, for ARG milliseconds.")
  (arg)
     Lisp_Object arg;
{
  Lisp_Object zero;

#ifndef EMACS_HAS_USECS
  error ("sleep-for-millisecs not supported on %s", SYSTEM_TYPE);
#else
  CHECK_NUMBER (arg, 0);

  XFASTINT (zero) = 0;
  wait_reading_process_input (XINT (arg) / 1000, XINT (arg) % 1000 * 1000,
			      zero, 0);
  return Qnil;
#endif /* EMACS_HAS_USECS */
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

  /* If the DISPLAY environment variable is set, try to use X, and
     die with an error message if that doesn't work.  */

  /* Check if we're using a window system here before trying to
     initialize the terminal.  If we check the terminal first,

     If someone has indicated that they want
     to use a window system, we shouldn't bother initializing the
     terminal.  This is especially important when the terminal is so
     dumb that emacs gives up before  and doesn't bother using the window
     system.  */

#ifdef HAVE_X_WINDOWS
  if (!inhibit_window_system && (display_arg || getenv ("DISPLAY")))
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

  remake_screen_glyphs (selected_screen);
  calculate_costs (selected_screen);

  /* X and Y coordinates of the cursor between updates. */
  SCREEN_CURSOR_X (selected_screen) = 0;
  SCREEN_CURSOR_Y (selected_screen) = 0;

#ifdef SIGWINCH
#ifndef CANNOT_DUMP
  if (initialized)
#endif /* CANNOT_DUMP */
    signal (SIGWINCH, window_change_signal);
#endif /* SIGWINCH */
}

syms_of_display ()
{
#ifdef MULTI_SCREEN
  defsubr (&Sredraw_screen);
#endif
  defsubr (&Sredraw_display);
  defsubr (&Sopen_termscript);
  defsubr (&Sding);
  defsubr (&Ssit_for);
  defsubr (&Ssleep_for);
  defsubr (&Ssend_string_to_terminal);

  DEFVAR_INT ("baud-rate", &baud_rate,
    "The output baud rate of the terminal.\n\
On most systems, changing this value will affect the amount of padding\n\
and the other strategic decisions made during redisplay.");
  DEFVAR_BOOL ("inverse-video", &inverse_video,
    "*Non-nil means invert the entire screen display.\n\
This means everything is in inverse video which otherwise would not be.");
  DEFVAR_BOOL ("visible-bell", &visible_bell,
    "*Non-nil means try to flash the screen to represent a bell.");
  DEFVAR_BOOL ("no-redraw-on-reenter", &no_redraw_on_reenter,
    "*Non-nil means no need to redraw entire screen after suspending.\n\
A non-nil value is useful if the terminal can automatically preserve\n\
Emacs's screen display when you reenter Emacs.\n\
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
    "Table defining how to output a glyph code to the screen.\n\
If not nil, this is a vector indexed by glyph code to define the glyph.\n\
Each element can be:\n\
 integer: a glyph code which this glyph is an alias for.\n\
 string: output this glyph using that string (not impl. in X windows).\n\
 nil: this glyph mod 256 is char code to output,\n\
    and this glyph / 256 is face code for X windows (see `x-set-face').");
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

