/* Updating of data structures for redisplay.
   Copyright (C) 1985, 86, 87, 88, 93, 94, 95, 97, 98, 1999, 2000, 2001
       Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include <config.h>
#include <signal.h>
#include <stdio.h>
#include <ctype.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "lisp.h"
#include "termchar.h"
#include "termopts.h"
#include "termhooks.h"
/* cm.h must come after dispextern.h on Windows.  */
#include "dispextern.h"
#include "cm.h"
#include "buffer.h"
#include "charset.h"
#include "keyboard.h"
#include "frame.h"
#include "window.h"
#include "commands.h"
#include "disptab.h"
#include "indent.h"
#include "intervals.h"
#include "blockinput.h"
#include "process.h"

/* I don't know why DEC Alpha OSF1 fail to compile this file if we
   include the following file.  */
/* #include "systty.h" */
#include "syssignal.h"

#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_NTGUI
#include "w32term.h"
#endif /* HAVE_NTGUI */

#ifdef macintosh
#include "macterm.h"
#endif /* macintosh */

/* Include systime.h after xterm.h to avoid double inclusion of time.h.  */

#include "systime.h"
#include <errno.h>

/* To get the prototype for `sleep'.  */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* Get number of chars of output now in the buffer of a stdio stream.
   This ought to be built in in stdio, but it isn't.  Some s- files
   override this because their stdio internals differ.  */

#ifdef __GNU_LIBRARY__

/* The s- file might have overridden the definition with one that
   works for the system's C library.  But we are using the GNU C
   library, so this is the right definition for every system.  */

#ifdef GNU_LIBRARY_PENDING_OUTPUT_COUNT
#define PENDING_OUTPUT_COUNT GNU_LIBRARY_PENDING_OUTPUT_COUNT
#else
#undef	PENDING_OUTPUT_COUNT
#define	PENDING_OUTPUT_COUNT(FILE) ((FILE)->__bufp - (FILE)->__buffer)
#endif
#else /* not __GNU_LIBRARY__ */
#if !defined (PENDING_OUTPUT_COUNT) && HAVE_STDIO_EXT_H && HAVE___FPENDING
#include <stdio_ext.h>
#define PENDING_OUTPUT_COUNT(FILE) __fpending (FILE)
#endif
#ifndef PENDING_OUTPUT_COUNT
#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_ptr - (FILE)->_base)
#endif
#endif /* not __GNU_LIBRARY__ */

#if defined(HAVE_TERM_H) && defined (GNU_LINUX) && defined (HAVE_LIBNCURSES)
#include <term.h>		/* for tgetent */
#endif

/* Structure to pass dimensions around.  Used for character bounding
   boxes, glyph matrix dimensions and alike.  */

struct dim
{
  int width;
  int height;
};


/* Function prototypes.  */

static struct glyph_matrix *save_current_matrix P_ ((struct frame *));
static void restore_current_matrix P_ ((struct frame *, struct glyph_matrix *));
static void fake_current_matrices P_ ((Lisp_Object));
static void redraw_overlapping_rows P_ ((struct window *, int));
static void redraw_overlapped_rows P_ ((struct window *, int));
static int count_blanks P_ ((struct glyph *, int));
static int count_match P_ ((struct glyph *, struct glyph *,
			    struct glyph *, struct glyph *));
static unsigned line_draw_cost P_ ((struct glyph_matrix *, int));
static void update_frame_line P_ ((struct frame *, int));
static struct dim allocate_matrices_for_frame_redisplay
     P_ ((Lisp_Object, int, int, int, int *));
static void allocate_matrices_for_window_redisplay P_ ((struct window *));
static int realloc_glyph_pool P_ ((struct glyph_pool *, struct dim));
static void adjust_frame_glyphs P_ ((struct frame *));
struct glyph_matrix *new_glyph_matrix P_ ((struct glyph_pool *));
static void free_glyph_matrix P_ ((struct glyph_matrix *));
static void adjust_glyph_matrix P_ ((struct window *, struct glyph_matrix *,
				     int, int, struct dim));
static void change_frame_size_1 P_ ((struct frame *, int, int, int, int, int));
static void swap_glyph_pointers P_ ((struct glyph_row *, struct glyph_row *));
#if GLYPH_DEBUG
static int glyph_row_slice_p P_ ((struct glyph_row *, struct glyph_row *));
#endif
static void fill_up_frame_row_with_spaces P_ ((struct glyph_row *, int));
static void build_frame_matrix_from_window_tree P_ ((struct glyph_matrix *,
						     struct window *));
static void build_frame_matrix_from_leaf_window P_ ((struct glyph_matrix *,
						     struct window *));
static struct glyph_pool *new_glyph_pool P_ ((void));
static void free_glyph_pool P_ ((struct glyph_pool *));
static void adjust_frame_glyphs_initially P_ ((void));
static void adjust_frame_message_buffer P_ ((struct frame *));
static void adjust_decode_mode_spec_buffer P_ ((struct frame *));
static void fill_up_glyph_row_with_spaces P_ ((struct glyph_row *));
static void build_frame_matrix P_ ((struct frame *));
void clear_current_matrices P_ ((struct frame *));
void scroll_glyph_matrix_range P_ ((struct glyph_matrix *, int, int,
				    int, int));
static void clear_window_matrices P_ ((struct window *, int));
static void fill_up_glyph_row_area_with_spaces P_ ((struct glyph_row *, int));
static int scrolling_window P_ ((struct window *, int));
static int update_window_line P_ ((struct window *, int, int *));
static void update_marginal_area P_ ((struct window *, int, int));
static int update_text_area P_ ((struct window *, int));
static void make_current P_ ((struct glyph_matrix *, struct glyph_matrix *,
			      int));
static void mirror_make_current P_ ((struct window *, int));
void check_window_matrix_pointers P_ ((struct window *));
#if GLYPH_DEBUG
static void check_matrix_pointers P_ ((struct glyph_matrix *,
				       struct glyph_matrix *));
#endif
static void mirror_line_dance P_ ((struct window *, int, int, int *, char *));
static int update_window_tree P_ ((struct window *, int));
static int update_window P_ ((struct window *, int));
static int update_frame_1 P_ ((struct frame *, int, int));
static void set_window_cursor_after_update P_ ((struct window *));
static int row_equal_p P_ ((struct window *, struct glyph_row *,
			    struct glyph_row *, int));
static void adjust_frame_glyphs_for_window_redisplay P_ ((struct frame *));
static void adjust_frame_glyphs_for_frame_redisplay P_ ((struct frame *));
static void reverse_rows P_ ((struct glyph_matrix *, int, int));
static int margin_glyphs_to_reserve P_ ((struct window *, int, Lisp_Object));
static void sync_window_with_frame_matrix_rows P_ ((struct window *));
struct window *frame_row_to_window P_ ((struct window *, int));


/* Non-zero means don't pause redisplay for pending input.  (This is
   for debugging and for a future implementation of EDT-like
   scrolling.  */

int redisplay_dont_pause;

/* Nonzero upon entry to redisplay means do not assume anything about
   current contents of actual terminal frame; clear and redraw it.  */

int frame_garbaged;

/* Nonzero means last display completed.  Zero means it was preempted.  */

int display_completed;

/* Lisp variable visible-bell; enables use of screen-flash instead of
   audible bell.  */

int visible_bell;

/* Invert the color of the whole frame, at a low level.  */

int inverse_video;

/* Line speed of the terminal.  */

EMACS_INT baud_rate;

/* Either nil or a symbol naming the window system under which Emacs
   is running.  */

Lisp_Object Vwindow_system;

/* Version number of X windows: 10, 11 or nil.  */

Lisp_Object Vwindow_system_version;

/* Vector of glyph definitions.  Indexed by glyph number, the contents
   are a string which is how to output the glyph.

   If Vglyph_table is nil, a glyph is output by using its low 8 bits
   as a character code.

   This is an obsolete feature that is no longer used.  The variable
   is retained for compatibility.  */

Lisp_Object Vglyph_table;

/* Display table to use for vectors that don't specify their own.  */

Lisp_Object Vstandard_display_table;

/* Nonzero means reading single-character input with prompt so put
   cursor on mini-buffer after the prompt.  Positive means at end of
   text in echo area; negative means at beginning of line.  */

int cursor_in_echo_area;

Lisp_Object Qdisplay_table, Qredisplay_dont_pause;


/* The currently selected frame.  In a single-frame version, this
   variable always equals the_only_frame.  */

Lisp_Object selected_frame;

/* A frame which is not just a mini-buffer, or 0 if there are no such
   frames.  This is usually the most recent such frame that was
   selected.  In a single-frame version, this variable always holds
   the address of the_only_frame.  */

struct frame *last_nonminibuf_frame;

/* Stdio stream being used for copy of all output.  */

FILE *termscript;

/* Structure for info on cursor positioning.  */

struct cm Wcm;

/* 1 means SIGWINCH happened when not safe.  */

int delayed_size_change;

/* 1 means glyph initialization has been completed at startup.  */

static int glyphs_initialized_initially_p;

/* Updated window if != 0.  Set by update_window.  */

struct window *updated_window;

/* Glyph row updated in update_window_line, and area that is updated.  */

struct glyph_row *updated_row;
int updated_area;

/* A glyph for a space.  */

struct glyph space_glyph;

/* Non-zero means update has been performed directly, so that there's
   no need for redisplay_internal to do much work.  Set by
   direct_output_for_insert.  */

int redisplay_performed_directly_p;

/* Counts of allocated structures.  These counts serve to diagnose
   memory leaks and double frees.  */

int glyph_matrix_count;
int glyph_pool_count;

/* If non-null, the frame whose frame matrices are manipulated.  If
   null, window matrices are worked on.  */

static struct frame *frame_matrix_frame;

/* Current interface for window-based redisplay.  Set from init_xterm.
   A null value means we are not using window-based redisplay.  */

struct redisplay_interface *rif;

/* Non-zero means that fonts have been loaded since the last glyph
   matrix adjustments.  Redisplay must stop, and glyph matrices must
   be adjusted when this flag becomes non-zero during display.  The
   reason fonts can be loaded so late is that fonts of fontsets are
   loaded on demand.  */

int fonts_changed_p;

/* Convert vpos and hpos from frame to window and vice versa. 
   This may only be used for terminal frames.  */

#if GLYPH_DEBUG

static int window_to_frame_vpos P_ ((struct window *, int));
static int window_to_frame_hpos P_ ((struct window *, int));
#define WINDOW_TO_FRAME_VPOS(W, VPOS) window_to_frame_vpos ((W), (VPOS))
#define WINDOW_TO_FRAME_HPOS(W, HPOS) window_to_frame_hpos ((W), (HPOS))

/* One element of the ring buffer containing redisplay history
   information.  */

struct redisplay_history
{
  char trace[512 + 100];
};

/* The size of the history buffer.  */

#define REDISPLAY_HISTORY_SIZE	30

/* The redisplay history buffer.  */

static struct redisplay_history redisplay_history[REDISPLAY_HISTORY_SIZE];

/* Next free entry in redisplay_history.  */

static int history_idx;

/* A tick that's incremented each time something is added to the
   history.  */

static unsigned history_tick;

static void add_frame_display_history P_ ((struct frame *, int));
static void add_window_display_history P_ ((struct window *, char *, int));


/* Add to the redisplay history how window W has been displayed.
   MSG is a trace containing the information how W's glyph matrix
   has been constructed.  PAUSED_P non-zero means that the update
   has been interrupted for pending input.  */

static void
add_window_display_history (w, msg, paused_p)
     struct window *w;
     char *msg;
     int paused_p;
{
  char *buf;
  
  if (history_idx >= REDISPLAY_HISTORY_SIZE)
    history_idx = 0;
  buf = redisplay_history[history_idx].trace;
  ++history_idx;
  
  sprintf (buf, "%d: window %p (`%s')%s\n",
	   history_tick++,
	   w,
	   ((BUFFERP (w->buffer)
	     && STRINGP (XBUFFER (w->buffer)->name))
	    ? (char *) XSTRING (XBUFFER (w->buffer)->name)->data
	    : "???"),
	   paused_p ? " ***paused***" : "");
  strcat (buf, msg);
}


/* Add to the redisplay history that frame F has been displayed.
   PAUSED_P non-zero means that the update has been interrupted for
   pending input.  */

static void
add_frame_display_history (f, paused_p)
     struct frame *f;
     int paused_p;
{
  char *buf;
  
  if (history_idx >= REDISPLAY_HISTORY_SIZE)
    history_idx = 0;
  buf = redisplay_history[history_idx].trace;
  ++history_idx;
  
  sprintf (buf, "%d: update frame %p%s",
	   history_tick++,
	   f, paused_p ? " ***paused***" : "");
}


DEFUN ("dump-redisplay-history", Fdump_redisplay_history,
       Sdump_redisplay_history, 0, 0, "",
       doc: /* Dump redisplay history to stderr.  */)
     ()
{
  int i;

  for (i = history_idx - 1; i != history_idx; --i)
    {
      if (i < 0)
	i = REDISPLAY_HISTORY_SIZE - 1;
      fprintf (stderr, "%s\n", redisplay_history[i].trace);
    }

  return Qnil;
}


#else /* GLYPH_DEBUG == 0 */

#define WINDOW_TO_FRAME_VPOS(W, VPOS) ((VPOS) + XFASTINT ((W)->top))
#define WINDOW_TO_FRAME_HPOS(W, HPOS) ((HPOS) + XFASTINT ((W)->left))

#endif /* GLYPH_DEBUG == 0 */


/* Like bcopy except never gets confused by overlap.  Let this be the
   first function defined in this file, or change emacs.c where the
   address of this function is used.  */

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
	     little left over.  The amount left over is (endt + (to -
	     from)) - to, which is endt - from.  */
	  bcopy (from, to, endt - from);
	}
    }
}     



/***********************************************************************
			    Glyph Matrices
 ***********************************************************************/

/* Allocate and return a glyph_matrix structure.  POOL is the glyph
   pool from which memory for the matrix should be allocated, or null
   for window-based redisplay where no glyph pools are used.  The
   member `pool' of the glyph matrix structure returned is set to
   POOL, the structure is otherwise zeroed.  */

struct glyph_matrix *
new_glyph_matrix (pool)
     struct glyph_pool *pool;
{
  struct glyph_matrix *result;

  /* Allocate and clear.  */
  result = (struct glyph_matrix *) xmalloc (sizeof *result);
  bzero (result, sizeof *result);

  /* Increment number of allocated matrices.  This count is used
     to detect memory leaks.  */
  ++glyph_matrix_count;

  /* Set pool and return.  */
  result->pool = pool;
  return result;
}


/* Free glyph matrix MATRIX.  Passing in a null MATRIX is allowed.

   The global counter glyph_matrix_count is decremented when a matrix
   is freed.  If the count gets negative, more structures were freed
   than allocated, i.e. one matrix was freed more than once or a bogus
   pointer was passed to this function.
 
   If MATRIX->pool is null, this means that the matrix manages its own
   glyph memory---this is done for matrices on X frames.  Freeing the
   matrix also frees the glyph memory in this case.  */

static void
free_glyph_matrix (matrix)
     struct glyph_matrix *matrix;
{
  if (matrix)
    {
      int i;

      /* Detect the case that more matrices are freed than were
	 allocated.  */
      if (--glyph_matrix_count < 0)
	abort ();

      /* Free glyph memory if MATRIX owns it.  */
      if (matrix->pool == NULL)
	for (i = 0; i < matrix->rows_allocated; ++i)
	  xfree (matrix->rows[i].glyphs[LEFT_MARGIN_AREA]);
      
      /* Free row structures and the matrix itself.  */
      xfree (matrix->rows);
      xfree (matrix);
    }
}


/* Return the number of glyphs to reserve for a marginal area of
   window W.  TOTAL_GLYPHS is the number of glyphs in a complete
   display line of window W.  MARGIN gives the width of the marginal
   area in canonical character units.  MARGIN should be an integer
   or a float.  */

static int
margin_glyphs_to_reserve (w, total_glyphs, margin)
     struct window *w;
     int total_glyphs;
     Lisp_Object margin;
{
  int n;

  if (NUMBERP (margin))
    {
      int width = XFASTINT (w->width);
      double d = max (0, XFLOATINT (margin));
      d = min (width / 2 - 1, d);
      n = (int) ((double) total_glyphs / width * d);
    }
  else
    n = 0;

  return n;
}


/* Adjust glyph matrix MATRIX on window W or on a frame to changed
   window sizes.

   W is null if the function is called for a frame glyph matrix.
   Otherwise it is the window MATRIX is a member of.  X and Y are the
   indices of the first column and row of MATRIX within the frame
   matrix, if such a matrix exists.  They are zero for purely
   window-based redisplay.  DIM is the needed size of the matrix.

   In window-based redisplay, where no frame matrices exist, glyph
   matrices manage their own glyph storage.  Otherwise, they allocate
   storage from a common frame glyph pool which can be found in
   MATRIX->pool.

   The reason for this memory management strategy is to avoid complete
   frame redraws if possible.  When we allocate from a common pool, a
   change of the location or size of a sub-matrix within the pool
   requires a complete redisplay of the frame because we cannot easily
   make sure that the current matrices of all windows still agree with
   what is displayed on the screen.  While this is usually fast, it
   leads to screen flickering.  */

static void
adjust_glyph_matrix (w, matrix, x, y, dim)
     struct window *w;
     struct glyph_matrix *matrix;
     int x, y;
     struct dim dim;
{
  int i;
  int new_rows;
  int marginal_areas_changed_p = 0;
  int header_line_changed_p = 0;
  int header_line_p = 0;
  int left = -1, right = -1;
  int window_x, window_y, window_width = -1, window_height;

  /* See if W had a header line that has disappeared now, or vice versa.  */
  if (w)
    {
      header_line_p = WINDOW_WANTS_HEADER_LINE_P (w);
      header_line_changed_p = header_line_p != matrix->header_line_p;
    }
  matrix->header_line_p = header_line_p;

  /* Do nothing if MATRIX' size, position, vscroll, and marginal areas
     haven't changed.  This optimization is important because preserving
     the matrix means preventing redisplay.  */
  if (matrix->pool == NULL)
    {
      window_box (w, -1, &window_x, &window_y, &window_width, &window_height);
      left = margin_glyphs_to_reserve (w, dim.width, w->left_margin_width);
      right = margin_glyphs_to_reserve (w, dim.width, w->right_margin_width);
      xassert (left >= 0 && right >= 0);
      marginal_areas_changed_p = (left != matrix->left_margin_glyphs
				  || right != matrix->right_margin_glyphs);

      if (!marginal_areas_changed_p
	  && !fonts_changed_p
	  && !header_line_changed_p
	  && matrix->window_left_x == XFASTINT (w->left)
	  && matrix->window_top_y == XFASTINT (w->top)
	  && matrix->window_height == window_height
	  && matrix->window_vscroll == w->vscroll
	  && matrix->window_width == window_width)
	return;
    }
  
  /* Enlarge MATRIX->rows if necessary.  New rows are cleared.  */
  if (matrix->rows_allocated < dim.height)
    {
      int size = dim.height * sizeof (struct glyph_row);
      new_rows = dim.height - matrix->rows_allocated;
      matrix->rows = (struct glyph_row *) xrealloc (matrix->rows, size);
      bzero (matrix->rows + matrix->rows_allocated,
	     new_rows * sizeof *matrix->rows);
      matrix->rows_allocated = dim.height;
    }
  else
    new_rows = 0;

  /* If POOL is not null, MATRIX is a frame matrix or a window matrix
     on a frame not using window-based redisplay.  Set up pointers for
     each row into the glyph pool.  */
  if (matrix->pool)
    {
      xassert (matrix->pool->glyphs);
      
      if (w)
	{
	  left = margin_glyphs_to_reserve (w, dim.width,
					   w->left_margin_width);
	  right = margin_glyphs_to_reserve (w, dim.width,
					    w->right_margin_width);
	}
      else
	left = right = 0;
      
      for (i = 0; i < dim.height; ++i)
	{
	  struct glyph_row *row = &matrix->rows[i];
	  
	  row->glyphs[LEFT_MARGIN_AREA] 
	    = (matrix->pool->glyphs
	       + (y + i) * matrix->pool->ncolumns
	       + x);
	  
	  if (w == NULL
	      || row == matrix->rows + dim.height - 1
	      || (row == matrix->rows && matrix->header_line_p))
	    {
	      row->glyphs[TEXT_AREA]
		= row->glyphs[LEFT_MARGIN_AREA];
	      row->glyphs[RIGHT_MARGIN_AREA]
		= row->glyphs[TEXT_AREA] + dim.width;
	      row->glyphs[LAST_AREA]
		= row->glyphs[RIGHT_MARGIN_AREA];
	    }
	  else
	    {
	      row->glyphs[TEXT_AREA]
		= row->glyphs[LEFT_MARGIN_AREA] + left;
	      row->glyphs[RIGHT_MARGIN_AREA]
		= row->glyphs[TEXT_AREA] + dim.width - left - right;
	      row->glyphs[LAST_AREA]
		= row->glyphs[LEFT_MARGIN_AREA] + dim.width;
	    }
	}
      
      matrix->left_margin_glyphs = left;
      matrix->right_margin_glyphs = right;
    }
  else
    {
      /* If MATRIX->pool is null, MATRIX is responsible for managing
	 its own memory.  Allocate glyph memory from the heap.  */
      if (dim.width > matrix->matrix_w
	  || new_rows
	  || header_line_changed_p
	  || marginal_areas_changed_p)
	{
	  struct glyph_row *row = matrix->rows;
	  struct glyph_row *end = row + matrix->rows_allocated;
	  
	  while (row < end)
	    {
	      row->glyphs[LEFT_MARGIN_AREA]
		= (struct glyph *) xrealloc (row->glyphs[LEFT_MARGIN_AREA],
					     (dim.width
					      * sizeof (struct glyph)));
	      
	      /* The mode line never has marginal areas.  */
	      if (row == matrix->rows + dim.height - 1
		  || (row == matrix->rows && matrix->header_line_p))
		{
		  row->glyphs[TEXT_AREA]
		    = row->glyphs[LEFT_MARGIN_AREA];
		  row->glyphs[RIGHT_MARGIN_AREA]
		    = row->glyphs[TEXT_AREA] + dim.width;
		  row->glyphs[LAST_AREA]
		    = row->glyphs[RIGHT_MARGIN_AREA];
		}
	      else
		{
		  row->glyphs[TEXT_AREA]
		    = row->glyphs[LEFT_MARGIN_AREA] + left;
		  row->glyphs[RIGHT_MARGIN_AREA]
		    = row->glyphs[TEXT_AREA] + dim.width - left - right;
		  row->glyphs[LAST_AREA]
		    = row->glyphs[LEFT_MARGIN_AREA] + dim.width;
		}
	      ++row;
	    }
	}

      xassert (left >= 0 && right >= 0);
      matrix->left_margin_glyphs = left;
      matrix->right_margin_glyphs = right;
    }
  
  /* Number of rows to be used by MATRIX.  */
  matrix->nrows = dim.height;
  xassert (matrix->nrows >= 0);

  if (w)
    {
      if (matrix == w->current_matrix)
	{
	  /* Mark rows in a current matrix of a window as not having
	     valid contents.  It's important to not do this for
	     desired matrices.  When Emacs starts, it may already be
	     building desired matrices when this function runs.  */
	  if (window_width < 0)
	    window_width = window_box_width (w, -1);
      
	  /* Optimize the case that only the height has changed (C-x 2,
	     upper window).  Invalidate all rows that are no longer part
	     of the window.  */
	  if (!marginal_areas_changed_p
	      && !header_line_changed_p
	      && new_rows == 0
	      && dim.width == matrix->matrix_w
	      && matrix->window_left_x == XFASTINT (w->left)
	      && matrix->window_top_y == XFASTINT (w->top)
	      && matrix->window_width == window_width)
	    {
	      /* Find the last row in the window.  */
	      for (i = 0; i < matrix->nrows && matrix->rows[i].enabled_p; ++i)
		if (MATRIX_ROW_BOTTOM_Y (matrix->rows + i) >= window_height)
		  {
		    ++i;
		    break;
		  }

	      /* Window end is invalid, if inside of the rows that
		 are invalidated below.  */
	      if (INTEGERP (w->window_end_vpos)
		  && XFASTINT (w->window_end_vpos) >= i)
		w->window_end_valid = Qnil;
	  
	      while (i < matrix->nrows)
		matrix->rows[i++].enabled_p = 0;
	    }
	  else
	    {
	      for (i = 0; i < matrix->nrows; ++i)
		matrix->rows[i].enabled_p = 0;
	    }
	}
      else if (matrix == w->desired_matrix)
	{
	  /* Rows in desired matrices always have to be cleared;
	     redisplay expects this is the case when it runs, so it
	     had better be the case when we adjust matrices between
	     redisplays.  */
	  for (i = 0; i < matrix->nrows; ++i)
	    matrix->rows[i].enabled_p = 0;
	}
    }
    
  
  /* Remember last values to be able to optimize frame redraws.  */
  matrix->matrix_x = x;
  matrix->matrix_y = y;
  matrix->matrix_w = dim.width;
  matrix->matrix_h = dim.height;

  /* Record the top y location and height of W at the time the matrix
     was last adjusted.  This is used to optimize redisplay above.  */
  if (w)
    {
      matrix->window_left_x = XFASTINT (w->left);
      matrix->window_top_y = XFASTINT (w->top);
      matrix->window_height = window_height;
      matrix->window_width = window_width;
      matrix->window_vscroll = w->vscroll;
    }
}


/* Reverse the contents of rows in MATRIX between START and END.  The
   contents of the row at END - 1 end up at START, END - 2 at START +
   1 etc.  This is part of the implementation of rotate_matrix (see
   below).  */

static void
reverse_rows (matrix, start, end)
     struct glyph_matrix *matrix;
     int start, end;
{
  int i, j;

  for (i = start, j = end - 1; i < j; ++i, --j)
    {
      /* Non-ISO HP/UX compiler doesn't like auto struct
	 initialization.  */
      struct glyph_row temp;
      temp = matrix->rows[i];
      matrix->rows[i] = matrix->rows[j];
      matrix->rows[j] = temp;
    }
}


/* Rotate the contents of rows in MATRIX in the range FIRST .. LAST -
   1 by BY positions.  BY < 0 means rotate left, i.e. towards lower
   indices.  (Note: this does not copy glyphs, only glyph pointers in
   row structures are moved around).

   The algorithm used for rotating the vector was, I believe, first
   described by Kernighan.  See the vector R as consisting of two
   sub-vectors AB, where A has length BY for BY >= 0.  The result
   after rotating is then BA.  Reverse both sub-vectors to get ArBr
   and reverse the result to get (ArBr)r which is BA.  Similar for
   rotating right.  */

void
rotate_matrix (matrix, first, last, by)
     struct glyph_matrix *matrix;
     int first, last, by;
{
  if (by < 0)
    {
      /* Up (rotate left, i.e. towards lower indices).  */
      by = -by;
      reverse_rows (matrix, first, first + by);
      reverse_rows (matrix, first + by, last);
      reverse_rows (matrix, first, last);
    }
  else if (by > 0)
    {
      /* Down (rotate right, i.e. towards higher indices).  */
      reverse_rows (matrix, last - by, last);
      reverse_rows (matrix, first, last - by);
      reverse_rows (matrix, first, last);
    }
}


/* Increment buffer positions in glyph rows of MATRIX.  Do it for rows
   with indices START <= index < END.  Increment positions by DELTA/
   DELTA_BYTES.  */

void
increment_matrix_positions (matrix, start, end, delta, delta_bytes)
     struct glyph_matrix *matrix;
     int start, end, delta, delta_bytes;
{
  /* Check that START and END are reasonable values.  */
  xassert (start >= 0 && start <= matrix->nrows);
  xassert (end >= 0 && end <= matrix->nrows);
  xassert (start <= end);

  for (; start < end; ++start)
    increment_row_positions (matrix->rows + start, delta, delta_bytes);
}


/* Enable a range of rows in glyph matrix MATRIX.  START and END are
   the row indices of the first and last + 1 row to enable.  If
   ENABLED_P is non-zero, enabled_p flags in rows will be set to 1.  */

void
enable_glyph_matrix_rows (matrix, start, end, enabled_p)
     struct glyph_matrix *matrix;
     int start, end;
     int enabled_p;
{
  xassert (start <= end);
  xassert (start >= 0 && start < matrix->nrows);
  xassert (end >= 0 && end <= matrix->nrows);
  
  for (; start < end; ++start)
    matrix->rows[start].enabled_p = enabled_p != 0;
}


/* Clear MATRIX.

   This empties all rows in MATRIX by setting the enabled_p flag for
   all rows of the matrix to zero.  The function prepare_desired_row
   will eventually really clear a row when it sees one with a zero
   enabled_p flag.

   Resets update hints to defaults value.  The only update hint
   currently present is the flag MATRIX->no_scrolling_p.  */

void
clear_glyph_matrix (matrix)
     struct glyph_matrix *matrix;
{
  if (matrix)
    {
      enable_glyph_matrix_rows (matrix, 0, matrix->nrows, 0);
      matrix->no_scrolling_p = 0;
    }
}
  

/* Shift part of the glyph matrix MATRIX of window W up or down.
   Increment y-positions in glyph rows between START and END by DY,
   and recompute their visible height.  */

void
shift_glyph_matrix (w, matrix, start, end, dy)
     struct window *w;
     struct glyph_matrix *matrix;
     int start, end, dy;
{
  int min_y, max_y;
  
  xassert (start <= end);
  xassert (start >= 0 && start < matrix->nrows);
  xassert (end >= 0 && end <= matrix->nrows);
  
  min_y = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (w);
  max_y = WINDOW_DISPLAY_HEIGHT_NO_MODE_LINE (w);
  
  for (; start < end; ++start)
    {
      struct glyph_row *row = &matrix->rows[start];
      
      row->y += dy;
      row->visible_height = row->height;
      
      if (row->y < min_y)
	row->visible_height -= min_y - row->y;
      if (row->y + row->height > max_y)
	row->visible_height -= row->y + row->height - max_y;
    }
}


/* Mark all rows in current matrices of frame F as invalid.  Marking
   invalid is done by setting enabled_p to zero for all rows in a
   current matrix.  */

void
clear_current_matrices (f)
     register struct frame *f;
{
  /* Clear frame current matrix, if we have one.  */
  if (f->current_matrix)
    clear_glyph_matrix (f->current_matrix);

  /* Clear the matrix of the menu bar window, if such a window exists.
     The menu bar window is currently used to display menus on X when
     no toolkit support is compiled in.  */
  if (WINDOWP (f->menu_bar_window))
    clear_glyph_matrix (XWINDOW (f->menu_bar_window)->current_matrix);

  /* Clear the matrix of the tool-bar window, if any.  */
  if (WINDOWP (f->tool_bar_window))
    clear_glyph_matrix (XWINDOW (f->tool_bar_window)->current_matrix);

  /* Clear current window matrices.  */
  xassert (WINDOWP (FRAME_ROOT_WINDOW (f)));
  clear_window_matrices (XWINDOW (FRAME_ROOT_WINDOW (f)), 0);
}


/* Clear out all display lines of F for a coming redisplay.  */

void
clear_desired_matrices (f)
     register struct frame *f;
{
  if (f->desired_matrix)
    clear_glyph_matrix (f->desired_matrix);
  
  if (WINDOWP (f->menu_bar_window))
    clear_glyph_matrix (XWINDOW (f->menu_bar_window)->desired_matrix);

  if (WINDOWP (f->tool_bar_window))
    clear_glyph_matrix (XWINDOW (f->tool_bar_window)->desired_matrix);

  /* Do it for window matrices.  */
  xassert (WINDOWP (FRAME_ROOT_WINDOW (f)));
  clear_window_matrices (XWINDOW (FRAME_ROOT_WINDOW (f)), 1);
}


/* Clear matrices in window tree rooted in W.  If DESIRED_P is
   non-zero clear desired matrices, otherwise clear current matrices.  */

static void
clear_window_matrices (w, desired_p)
     struct window *w;
     int desired_p;
{
  while (w)
    {
      if (!NILP (w->hchild))
	{
	  xassert (WINDOWP (w->hchild));
	  clear_window_matrices (XWINDOW (w->hchild), desired_p);
	}
      else if (!NILP (w->vchild))
	{
	  xassert (WINDOWP (w->vchild));
	  clear_window_matrices (XWINDOW (w->vchild), desired_p);
	}
      else
	{
	  if (desired_p)
	    clear_glyph_matrix (w->desired_matrix);
	  else
	    {
	      clear_glyph_matrix (w->current_matrix);
	      w->window_end_valid = Qnil;
	    }
	}

      w = NILP (w->next) ? 0 : XWINDOW (w->next);
    }
}



/***********************************************************************
			      Glyph Rows

      See dispextern.h for an overall explanation of glyph rows.
 ***********************************************************************/

/* Clear glyph row ROW.  Do it in a way that makes it robust against
   changes in the glyph_row structure, i.e. addition or removal of
   structure members.  */

static struct glyph_row null_row;

void
clear_glyph_row (row)
     struct glyph_row *row;
{
  struct glyph *p[1 + LAST_AREA];

  /* Save pointers.  */
  p[LEFT_MARGIN_AREA] = row->glyphs[LEFT_MARGIN_AREA];
  p[TEXT_AREA] = row->glyphs[TEXT_AREA];
  p[RIGHT_MARGIN_AREA] = row->glyphs[RIGHT_MARGIN_AREA];
  p[LAST_AREA] = row->glyphs[LAST_AREA];

  /* Clear.  */
  *row = null_row;

  /* Restore pointers.  */
  row->glyphs[LEFT_MARGIN_AREA] = p[LEFT_MARGIN_AREA];
  row->glyphs[TEXT_AREA] = p[TEXT_AREA];
  row->glyphs[RIGHT_MARGIN_AREA] = p[RIGHT_MARGIN_AREA];
  row->glyphs[LAST_AREA] = p[LAST_AREA];

#if 0 /* At some point, some bit-fields of struct glyph were not set,
	 which made glyphs unequal when compared with GLYPH_EQUAL_P.
	 Redisplay outputs such glyphs, and flickering effects were
	 the result.  This also depended on the contents of memory
	 returned by xmalloc.  If flickering happens again, activate
	 the code below.  If the flickering is gone with that, chances
	 are that the flickering has the same reason as here.  */
  bzero (p[0], (char *) p[LAST_AREA] - (char *) p[0]);
#endif
}


/* Make ROW an empty, enabled row of canonical character height,
   in window W starting at y-position Y.  */

void
blank_row (w, row, y)
     struct window *w;
     struct glyph_row *row;
     int y;
{
  int min_y, max_y;
  
  min_y = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (w);
  max_y = WINDOW_DISPLAY_HEIGHT_NO_MODE_LINE (w);
  
  clear_glyph_row (row);
  row->y = y;
  row->ascent = row->phys_ascent = 0;
  row->height = row->phys_height = CANON_Y_UNIT (XFRAME (w->frame));
  row->visible_height = row->height;
      
  if (row->y < min_y)
    row->visible_height -= min_y - row->y;
  if (row->y + row->height > max_y)
    row->visible_height -= row->y + row->height - max_y;

  row->enabled_p = 1;
}


/* Increment buffer positions in glyph row ROW.  DELTA and DELTA_BYTES
   are the amounts by which to change positions.  Note that the first
   glyph of the text area of a row can have a buffer position even if
   the used count of the text area is zero.  Such rows display line
   ends.  */

void
increment_row_positions (row, delta, delta_bytes)
     struct glyph_row *row;
     int delta, delta_bytes;
{
  int area, i;

  /* Increment start and end positions.  */
  MATRIX_ROW_START_CHARPOS (row) += delta;
  MATRIX_ROW_START_BYTEPOS (row) += delta_bytes;
  MATRIX_ROW_END_CHARPOS (row) += delta;
  MATRIX_ROW_END_BYTEPOS (row) += delta_bytes;

  /* Increment positions in glyphs.  */
  for (area = 0; area < LAST_AREA; ++area)
    for (i = 0; i < row->used[area]; ++i)
      if (BUFFERP (row->glyphs[area][i].object)
	  && row->glyphs[area][i].charpos > 0)
	row->glyphs[area][i].charpos += delta;

  /* Capture the case of rows displaying a line end.  */
  if (row->used[TEXT_AREA] == 0
      && MATRIX_ROW_DISPLAYS_TEXT_P (row))
    row->glyphs[TEXT_AREA]->charpos += delta;
}


#if 0
/* Swap glyphs between two glyph rows A and B.  This exchanges glyph
   contents, i.e. glyph structure contents are exchanged between A and
   B without changing glyph pointers in A and B.  */

static void
swap_glyphs_in_rows (a, b)
     struct glyph_row *a, *b;
{
  int area;

  for (area = 0; area < LAST_AREA; ++area)
    {
      /* Number of glyphs to swap.  */
      int max_used = max (a->used[area], b->used[area]);

      /* Start of glyphs in area of row A.  */
      struct glyph *glyph_a = a->glyphs[area];

      /* End + 1 of glyphs in area of row A.  */
      struct glyph *glyph_a_end = a->glyphs[max_used];

      /* Start of glyphs in area of row B.  */
      struct glyph *glyph_b = b->glyphs[area];

      while (glyph_a < glyph_a_end)
	{
	  /* Non-ISO HP/UX compiler doesn't like auto struct
             initialization.  */
	  struct glyph temp;
	  temp = *glyph_a;
	  *glyph_a = *glyph_b;
	  *glyph_b = temp;
	  ++glyph_a;
	  ++glyph_b;
	}
    }
}

#endif /* 0 */

/* Exchange pointers to glyph memory between glyph rows A and B.  */

static INLINE void
swap_glyph_pointers (a, b)
     struct glyph_row *a, *b;
{
  int i;
  for (i = 0; i < LAST_AREA + 1; ++i)
    {
      struct glyph *temp = a->glyphs[i];
      a->glyphs[i] = b->glyphs[i];
      b->glyphs[i] = temp;
    }
}


/* Copy glyph row structure FROM to glyph row structure TO, except
   that glyph pointers in the structures are left unchanged.  */

INLINE void
copy_row_except_pointers (to, from)
     struct glyph_row *to, *from;
{
  struct glyph *pointers[1 + LAST_AREA];

  /* Save glyph pointers of TO.  */
  bcopy (to->glyphs, pointers, sizeof to->glyphs);

  /* Do a structure assignment.  */
  *to = *from;

  /* Restore original pointers of TO.  */
  bcopy (pointers, to->glyphs, sizeof to->glyphs);
}


/* Copy contents of glyph row FROM to glyph row TO.  Glyph pointers in
   TO and FROM are left unchanged.  Glyph contents are copied from the
   glyph memory of FROM to the glyph memory of TO.  Increment buffer
   positions in row TO by DELTA/ DELTA_BYTES.  */

void
copy_glyph_row_contents (to, from, delta, delta_bytes)
     struct glyph_row *to, *from;
     int delta, delta_bytes;
{
  int area;

  /* This is like a structure assignment TO = FROM, except that
     glyph pointers in the rows are left unchanged.  */
  copy_row_except_pointers (to, from);

  /* Copy glyphs from FROM to TO.  */
  for (area = 0; area < LAST_AREA; ++area)
    if (from->used[area])
      bcopy (from->glyphs[area], to->glyphs[area], 
	     from->used[area] * sizeof (struct glyph));

  /* Increment buffer positions in TO by DELTA.  */
  increment_row_positions (to, delta, delta_bytes);
}


/* Assign glyph row FROM to glyph row TO.  This works like a structure
   assignment TO = FROM, except that glyph pointers are not copied but
   exchanged between TO and FROM.  Pointers must be exchanged to avoid
   a memory leak.  */

static INLINE void
assign_row (to, from)
     struct glyph_row *to, *from;
{
  swap_glyph_pointers (to, from);
  copy_row_except_pointers (to, from);
}


/* Test whether the glyph memory of the glyph row WINDOW_ROW, which is
   a row in a window matrix, is a slice of the glyph memory of the
   glyph row FRAME_ROW which is a row in a frame glyph matrix.  Value
   is non-zero if the glyph memory of WINDOW_ROW is part of the glyph
   memory of FRAME_ROW.  */

#if GLYPH_DEBUG

static int
glyph_row_slice_p (window_row, frame_row)
     struct glyph_row *window_row, *frame_row;
{
  struct glyph *window_glyph_start = window_row->glyphs[0];
  struct glyph *frame_glyph_start = frame_row->glyphs[0];
  struct glyph *frame_glyph_end = frame_row->glyphs[LAST_AREA];

  return (frame_glyph_start <= window_glyph_start
	  && window_glyph_start < frame_glyph_end);
}

#endif /* GLYPH_DEBUG */

#if 0

/* Find the row in the window glyph matrix WINDOW_MATRIX being a slice
   of ROW in the frame matrix FRAME_MATRIX.  Value is null if no row
   in WINDOW_MATRIX is found satisfying the condition.  */

static struct glyph_row *
find_glyph_row_slice (window_matrix, frame_matrix, row)
     struct glyph_matrix *window_matrix, *frame_matrix;
     int row;
{
  int i;

  xassert (row >= 0 && row < frame_matrix->nrows);

  for (i = 0; i < window_matrix->nrows; ++i)
    if (glyph_row_slice_p (window_matrix->rows + i,
			   frame_matrix->rows + row))
      break;

  return i < window_matrix->nrows ? window_matrix->rows + i : 0;
}

#endif /* 0 */

/* Prepare ROW for display.  Desired rows are cleared lazily,
   i.e. they are only marked as to be cleared by setting their
   enabled_p flag to zero.  When a row is to be displayed, a prior
   call to this function really clears it.  */

void
prepare_desired_row (row)
     struct glyph_row *row;
{
  if (!row->enabled_p)
    {
      clear_glyph_row (row);
      row->enabled_p = 1;
    }
}


/* Return a hash code for glyph row ROW.  */

int
line_hash_code (row)
     struct glyph_row *row;
{
  int hash = 0;
  
  if (row->enabled_p)
    {
      struct glyph *glyph = row->glyphs[TEXT_AREA];
      struct glyph *end = glyph + row->used[TEXT_AREA];

      while (glyph < end)
	{
	  int c = glyph->u.ch;
	  int face_id = glyph->face_id;
	  if (must_write_spaces)
	    c -= SPACEGLYPH;
	  hash = (((hash << 4) + (hash >> 24)) & 0x0fffffff) + c;
	  hash = (((hash << 4) + (hash >> 24)) & 0x0fffffff) + face_id;
	  ++glyph;
	}

      if (hash == 0)
	hash = 1;
    }

  return hash;
}


/* Return the cost of drawing line VPOS in MATRIX.  The cost equals
   the number of characters in the line.  If must_write_spaces is
   zero, leading and trailing spaces are ignored.  */

static unsigned int
line_draw_cost (matrix, vpos)
     struct glyph_matrix *matrix;
     int vpos;
{
  struct glyph_row *row = matrix->rows + vpos;
  struct glyph *beg = row->glyphs[TEXT_AREA];
  struct glyph *end = beg + row->used[TEXT_AREA];
  int len;
  Lisp_Object *glyph_table_base = GLYPH_TABLE_BASE;
  int glyph_table_len = GLYPH_TABLE_LENGTH;

  /* Ignore trailing and leading spaces if we can.  */
  if (!must_write_spaces)
    {
      /* Skip from the end over trailing spaces.  */
      while (end > beg && CHAR_GLYPH_SPACE_P (*(end - 1)))
	--end;

      /* All blank line.  */      
      if (end == beg)
	return 0;

      /* Skip over leading spaces.  */
      while (CHAR_GLYPH_SPACE_P (*beg))
	++beg;
    }

  /* If we don't have a glyph-table, each glyph is one character,
     so return the number of glyphs.  */
  if (glyph_table_base == 0)
    len = end - beg;
  else
    {
      /* Otherwise, scan the glyphs and accumulate their total length
	 in LEN.  */
      len = 0;
      while (beg < end)
	{
	  GLYPH g = GLYPH_FROM_CHAR_GLYPH (*beg);
	  
	  if (g < 0
	      || GLYPH_SIMPLE_P (glyph_table_base, glyph_table_len, g))
	    len += 1;
	  else
	    len += GLYPH_LENGTH (glyph_table_base, g);
	  
	  ++beg;
	}
    }
  
  return len;
}


/* Test two glyph rows A and B for equality.  Value is non-zero if A
   and B have equal contents.  W is the window to which the glyphs
   rows A and B belong.  It is needed here to test for partial row
   visibility.  MOUSE_FACE_P non-zero means compare the mouse_face_p
   flags of A and B, too.  */

static INLINE int 
row_equal_p (w, a, b, mouse_face_p)
     struct window *w;
     struct glyph_row *a, *b;
     int mouse_face_p;
{
  if (a == b)
    return 1;
  else if (a->hash != b->hash)
    return 0;
  else
    {
      struct glyph *a_glyph, *b_glyph, *a_end;
      int area;

      if (mouse_face_p && a->mouse_face_p != b->mouse_face_p)
	return 0;

      /* Compare glyphs.  */
      for (area = LEFT_MARGIN_AREA; area < LAST_AREA; ++area)
	{
	  if (a->used[area] != b->used[area])
	    return 0;
	  
	  a_glyph = a->glyphs[area];
	  a_end = a_glyph + a->used[area];
	  b_glyph = b->glyphs[area];
	  
	  while (a_glyph < a_end
		 && GLYPH_EQUAL_P (a_glyph, b_glyph))
	    ++a_glyph, ++b_glyph;
	  
	  if (a_glyph != a_end)
	    return 0;
	}

      if (a->truncated_on_left_p != b->truncated_on_left_p
	  || a->fill_line_p != b->fill_line_p
	  || a->truncated_on_right_p != b->truncated_on_right_p
	  || a->overlay_arrow_p != b->overlay_arrow_p
	  || a->continued_p != b->continued_p
	  || a->indicate_empty_line_p != b->indicate_empty_line_p
	  || a->overlapped_p != b->overlapped_p
	  || (MATRIX_ROW_CONTINUATION_LINE_P (a)
	      != MATRIX_ROW_CONTINUATION_LINE_P (b))
	  /* Different partially visible characters on left margin.  */
	  || a->x != b->x
	  /* Different height.  */
	  || a->ascent != b->ascent
	  || a->phys_ascent != b->phys_ascent
	  || a->phys_height != b->phys_height
	  || a->visible_height != b->visible_height)
	return 0;
    }

  return 1;
}



/***********************************************************************
			      Glyph Pool

     See dispextern.h for an overall explanation of glyph pools.
 ***********************************************************************/

/* Allocate a glyph_pool structure.  The structure returned is
   initialized with zeros.  The global variable glyph_pool_count is
   incremented for each pool allocated.  */

static struct glyph_pool *
new_glyph_pool ()
{
  struct glyph_pool *result;

  /* Allocate a new glyph_pool and clear it.  */
  result = (struct glyph_pool *) xmalloc (sizeof *result);
  bzero (result, sizeof *result);
  
  /* For memory leak and double deletion checking.  */
  ++glyph_pool_count;
  
  return result;
}


/* Free a glyph_pool structure POOL.  The function may be called with
   a null POOL pointer.  The global variable glyph_pool_count is
   decremented with every pool structure freed.  If this count gets
   negative, more structures were freed than allocated, i.e. one
   structure must have been freed more than once or a bogus pointer
   was passed to free_glyph_pool.  */

static void
free_glyph_pool (pool)
     struct glyph_pool *pool;
{
  if (pool)
    {
      /* More freed than allocated?  */
      --glyph_pool_count;
      xassert (glyph_pool_count >= 0);

      xfree (pool->glyphs);
      xfree (pool);
    }
}


/* Enlarge a glyph pool POOL.  MATRIX_DIM gives the number of rows and
   columns we need.  This function never shrinks a pool.  The only
   case in which this would make sense, would be when a frame's size
   is changed from a large value to a smaller one.  But, if someone
   does it once, we can expect that he will do it again.

   Value is non-zero if the pool changed in a way which makes
   re-adjusting window glyph matrices necessary.  */

static int
realloc_glyph_pool (pool, matrix_dim)
     struct glyph_pool *pool;
     struct dim matrix_dim;
{
  int needed;
  int changed_p;

  changed_p = (pool->glyphs == 0
	       || matrix_dim.height != pool->nrows
	       || matrix_dim.width != pool->ncolumns);

  /* Enlarge the glyph pool.  */
  needed = matrix_dim.width * matrix_dim.height;
  if (needed > pool->nglyphs)
    {
      int size = needed * sizeof (struct glyph);

      if (pool->glyphs)
	pool->glyphs = (struct glyph *) xrealloc (pool->glyphs, size);
      else
	{
	  pool->glyphs = (struct glyph *) xmalloc (size);
	  bzero (pool->glyphs, size);
	}

      pool->nglyphs = needed;
    }

  /* Remember the number of rows and columns because (a) we use them
     to do sanity checks, and (b) the number of columns determines
     where rows in the frame matrix start---this must be available to
     determine pointers to rows of window sub-matrices.  */
  pool->nrows = matrix_dim.height;
  pool->ncolumns = matrix_dim.width;
  
  return changed_p;
}



/***********************************************************************
			      Debug Code
 ***********************************************************************/

#if GLYPH_DEBUG


/* Flush standard output.  This is sometimes useful to call from
   the debugger.  */

void
flush_stdout ()
{
  fflush (stdout);
}


/* Check that no glyph pointers have been lost in MATRIX.  If a
   pointer has been lost, e.g. by using a structure assignment between
   rows, at least one pointer must occur more than once in the rows of
   MATRIX.  */

void
check_matrix_pointer_lossage (matrix)
     struct glyph_matrix *matrix;
{
  int i, j;
  
  for (i = 0; i < matrix->nrows; ++i)
    for (j = 0; j < matrix->nrows; ++j)
      xassert (i == j
	       || (matrix->rows[i].glyphs[TEXT_AREA]
		   != matrix->rows[j].glyphs[TEXT_AREA]));
}


/* Get a pointer to glyph row ROW in MATRIX, with bounds checks.  */

struct glyph_row *
matrix_row (matrix, row)
     struct glyph_matrix *matrix;
     int row;
{
  xassert (matrix && matrix->rows);
  xassert (row >= 0 && row < matrix->nrows);

  /* That's really too slow for normal testing because this function
     is called almost everywhere.  Although---it's still astonishingly
     fast, so it is valuable to have for debugging purposes.  */
#if 0
  check_matrix_pointer_lossage (matrix);
#endif
  
  return matrix->rows + row;
}


#if 0 /* This function makes invalid assumptions when text is
	 partially invisible.  But it might come handy for debugging
	 nevertheless.  */

/* Check invariants that must hold for an up to date current matrix of
   window W.  */

static void
check_matrix_invariants (w)
     struct window *w;
{
  struct glyph_matrix *matrix = w->current_matrix;
  int yb = window_text_bottom_y (w);
  struct glyph_row *row = matrix->rows;
  struct glyph_row *last_text_row = NULL;
  struct buffer *saved = current_buffer;
  struct buffer *buffer = XBUFFER (w->buffer);
  int c;
  
  /* This can sometimes happen for a fresh window.  */
  if (matrix->nrows < 2)
    return;

  set_buffer_temp (buffer);

  /* Note: last row is always reserved for the mode line.  */
  while (MATRIX_ROW_DISPLAYS_TEXT_P (row)
	 && MATRIX_ROW_BOTTOM_Y (row) < yb)
    {
      struct glyph_row *next = row + 1;

      if (MATRIX_ROW_DISPLAYS_TEXT_P (row))
	last_text_row = row;

      /* Check that character and byte positions are in sync.  */
      xassert (MATRIX_ROW_START_BYTEPOS (row)
	       == CHAR_TO_BYTE (MATRIX_ROW_START_CHARPOS (row)));

      /* CHAR_TO_BYTE aborts when invoked for a position > Z.  We can
	 have such a position temporarily in case of a minibuffer
	 displaying something like `[Sole completion]' at its end.  */
      if (MATRIX_ROW_END_CHARPOS (row) < BUF_ZV (current_buffer))
	xassert (MATRIX_ROW_END_BYTEPOS (row)
		 == CHAR_TO_BYTE (MATRIX_ROW_END_CHARPOS (row)));

      /* Check that end position of `row' is equal to start position
	 of next row.  */
      if (next->enabled_p && MATRIX_ROW_DISPLAYS_TEXT_P (next))
	{
	  xassert (MATRIX_ROW_END_CHARPOS (row)
		   == MATRIX_ROW_START_CHARPOS (next));
	  xassert (MATRIX_ROW_END_BYTEPOS (row)
		   == MATRIX_ROW_START_BYTEPOS (next));
	}
      row = next;
    }

  xassert (w->current_matrix->nrows == w->desired_matrix->nrows);
  xassert (w->desired_matrix->rows != NULL);
  set_buffer_temp (saved);
}

#endif /* 0  */

#endif /* GLYPH_DEBUG != 0 */



/**********************************************************************
		 Allocating/ Adjusting Glyph Matrices
 **********************************************************************/

/* Allocate glyph matrices over a window tree for a frame-based
   redisplay

   X and Y are column/row within the frame glyph matrix where
   sub-matrices for the window tree rooted at WINDOW must be
   allocated.  CH_DIM contains the dimensions of the smallest
   character that could be used during display.  DIM_ONLY_P non-zero
   means that the caller of this function is only interested in the
   result matrix dimension, and matrix adjustments should not be
   performed.

   The function returns the total width/height of the sub-matrices of
   the window tree.  If called on a frame root window, the computation
   will take the mini-buffer window into account.

   *WINDOW_CHANGE_FLAGS is set to a bit mask with bits

   NEW_LEAF_MATRIX set if any window in the tree did not have a
   glyph matrices yet, and

   CHANGED_LEAF_MATRIX set if the dimension or location of a matrix of
   any window in the tree will be changed or have been changed (see
   DIM_ONLY_P)

   *WINDOW_CHANGE_FLAGS must be initialized by the caller of this
   function.

   Windows are arranged into chains of windows on the same level
   through the next fields of window structures.  Such a level can be
   either a sequence of horizontally adjacent windows from left to
   right, or a sequence of vertically adjacent windows from top to
   bottom.  Each window in a horizontal sequence can be either a leaf
   window or a vertical sequence; a window in a vertical sequence can
   be either a leaf or a horizontal sequence.  All windows in a
   horizontal sequence have the same height, and all windows in a
   vertical sequence have the same width.

   This function uses, for historical reasons, a more general
   algorithm to determine glyph matrix dimensions that would be
   necessary.

   The matrix height of a horizontal sequence is determined by the
   maximum height of any matrix in the sequence.  The matrix width of
   a horizontal sequence is computed by adding up matrix widths of
   windows in the sequence.

   |<------- result width ------->|
   +---------+----------+---------+ ---
   |         |		|	  |  |
   |         |		|	  |
   +---------+		|	  |  result height
	     |		+---------+
	     |		|            |
	     +----------+	    ---

   The matrix width of a vertical sequence is the maximum matrix width
   of any window in the sequence.  Its height is computed by adding up
   matrix heights of windows in the sequence.

   |<---- result width -->|
   +---------+		    ---
   |         |               |
   |         |               |
   +---------+--+            |
   |		|            |
   |		|	     result height
   |		|
   +------------+---------+  |
   |			  |  |
   |			  |  |
   +------------+---------+ ---  */

/* Bit indicating that a new matrix will be allocated or has been
   allocated.  */

#define NEW_LEAF_MATRIX		(1 << 0)

/* Bit indicating that a matrix will or has changed its location or
   size.  */

#define CHANGED_LEAF_MATRIX	(1 << 1)

static struct dim
allocate_matrices_for_frame_redisplay (window, x, y, dim_only_p,
				       window_change_flags)
     Lisp_Object window;
     int x, y;
     int dim_only_p;
     int *window_change_flags;
{
  struct frame *f = XFRAME (WINDOW_FRAME (XWINDOW (window)));
  int x0 = x, y0 = y;
  int wmax = 0, hmax = 0;
  struct dim total;
  struct dim dim;
  struct window *w;
  int in_horz_combination_p;

  /* What combination is WINDOW part of?  Compute this once since the
     result is the same for all windows in the `next' chain.  The
     special case of a root window (parent equal to nil) is treated
     like a vertical combination because a root window's `next'
     points to the mini-buffer window, if any, which is arranged
     vertically below other windows.  */
  in_horz_combination_p
    = (!NILP (XWINDOW (window)->parent)
       && !NILP (XWINDOW (XWINDOW (window)->parent)->hchild));

  /* For WINDOW and all windows on the same level.  */
  do 
    {
      w = XWINDOW (window);

      /* Get the dimension of the window sub-matrix for W, depending
	 on whether this is a combination or a leaf window.  */
      if (!NILP (w->hchild))
	dim = allocate_matrices_for_frame_redisplay (w->hchild, x, y, 
						     dim_only_p,
						     window_change_flags);
      else if (!NILP (w->vchild))
	dim = allocate_matrices_for_frame_redisplay (w->vchild, x, y, 
						     dim_only_p,
						     window_change_flags);
      else
	{
	  /* If not already done, allocate sub-matrix structures.  */
	  if (w->desired_matrix == NULL)
	    {
	      w->desired_matrix = new_glyph_matrix (f->desired_pool);
	      w->current_matrix = new_glyph_matrix (f->current_pool);
	      *window_change_flags |= NEW_LEAF_MATRIX;
	    }
  
	  /* Width and height MUST be chosen so that there are no
	     holes in the frame matrix.  */
	  dim.width = required_matrix_width (w);
	  dim.height = required_matrix_height (w);

	  /* Will matrix be re-allocated?  */
	  if (x != w->desired_matrix->matrix_x
	      || y != w->desired_matrix->matrix_y
	      || dim.width != w->desired_matrix->matrix_w
	      || dim.height != w->desired_matrix->matrix_h
	      || (margin_glyphs_to_reserve (w, dim.width,
					    w->right_margin_width)
		  != w->desired_matrix->left_margin_glyphs)
	      || (margin_glyphs_to_reserve (w, dim.width,
					    w->left_margin_width)
		  != w->desired_matrix->right_margin_glyphs))
	    *window_change_flags |= CHANGED_LEAF_MATRIX;

	  /* Actually change matrices, if allowed.  Do not consider
	     CHANGED_LEAF_MATRIX computed above here because the pool
	     may have been changed which we don't now here.  We trust
	     that we only will be called with DIM_ONLY_P != 0 when
	     necessary.  */
	  if (!dim_only_p)
	    {
	      adjust_glyph_matrix (w, w->desired_matrix, x, y, dim);
	      adjust_glyph_matrix (w, w->current_matrix, x, y, dim);
	    }
	}

      /* If we are part of a horizontal combination, advance x for
	 windows to the right of W; otherwise advance y for windows
	 below W.  */
      if (in_horz_combination_p)
	x += dim.width;
      else 
        y += dim.height;

      /* Remember maximum glyph matrix dimensions.  */
      wmax = max (wmax, dim.width);
      hmax = max (hmax, dim.height);

      /* Next window on same level.  */
      window = w->next;
    }
  while (!NILP (window));

  /* Set `total' to the total glyph matrix dimension of this window
     level.  In a vertical combination, the width is the width of the
     widest window; the height is the y we finally reached, corrected
     by the y we started with.  In a horizontal combination, the total
     height is the height of the tallest window, and the width is the
     x we finally reached, corrected by the x we started with.  */
  if (in_horz_combination_p)
    {
      total.width = x - x0;
      total.height = hmax;
    }
  else 
    {
      total.width = wmax;
      total.height = y - y0;
    }

  return total;
}


/* Return the required height of glyph matrices for window W.  */

int
required_matrix_height (w)
     struct window *w;
{
#ifdef HAVE_WINDOW_SYSTEM
  struct frame *f = XFRAME (w->frame);
  
  if (FRAME_WINDOW_P (f))
    {
      int ch_height = FRAME_SMALLEST_FONT_HEIGHT (f);
      int window_pixel_height = window_box_height (w) + abs (w->vscroll);
      return (((window_pixel_height + ch_height - 1)
	       / ch_height)
	      /* One partially visible line at the top and
		 bottom of the window.  */
	      + 2
	      /* 2 for header and mode line.  */
	      + 2);
    }
#endif /* HAVE_WINDOW_SYSTEM */
      
  return XINT (w->height);
}


/* Return the required width of glyph matrices for window W.  */

int
required_matrix_width (w)
     struct window *w;
{
#ifdef HAVE_WINDOW_SYSTEM
  struct frame *f = XFRAME (w->frame);
  if (FRAME_WINDOW_P (f))
    {
      int ch_width = FRAME_SMALLEST_CHAR_WIDTH (f);
      int window_pixel_width = XFLOATINT (w->width) * CANON_X_UNIT (f);
  
      /* Compute number of glyphs needed in a glyph row.  */
      return (((window_pixel_width + ch_width - 1)
	       / ch_width)
	      /* 2 partially visible columns in the text area.  */
	      + 2
	      /* One partially visible column at the right
		 edge of each marginal area.  */
	      + 1 + 1);
    }
#endif /* HAVE_WINDOW_SYSTEM */

  return XINT (w->width);
}


/* Allocate window matrices for window-based redisplay.  W is the
   window whose matrices must be allocated/reallocated.  CH_DIM is the
   size of the smallest character that could potentially be used on W.  */
   
static void
allocate_matrices_for_window_redisplay (w)
     struct window *w;
{
  while (w)
    {
      if (!NILP (w->vchild))
	allocate_matrices_for_window_redisplay (XWINDOW (w->vchild));
      else if (!NILP (w->hchild))
	allocate_matrices_for_window_redisplay (XWINDOW (w->hchild));
      else
	{
	  /* W is a leaf window.  */
	  struct dim dim;

	  /* If matrices are not yet allocated, allocate them now.  */
	  if (w->desired_matrix == NULL)
	    {
	      w->desired_matrix = new_glyph_matrix (NULL);
	      w->current_matrix = new_glyph_matrix (NULL);
	    }

	  dim.width = required_matrix_width (w);
	  dim.height = required_matrix_height (w);
	  adjust_glyph_matrix (w, w->desired_matrix, 0, 0, dim);
	  adjust_glyph_matrix (w, w->current_matrix, 0, 0, dim);
	}
      
      w = NILP (w->next) ? NULL : XWINDOW (w->next);
    }
}


/* Re-allocate/ re-compute glyph matrices on frame F.  If F is null,
   do it for all frames; otherwise do it just for the given frame.
   This function must be called when a new frame is created, its size
   changes, or its window configuration changes.  */

void
adjust_glyphs (f)
     struct frame *f;
{
  /* Block input so that expose events and other events that access
     glyph matrices are not processed while we are changing them.  */
  BLOCK_INPUT;

  if (f)
    adjust_frame_glyphs (f);
  else
    {
      Lisp_Object tail, lisp_frame;
      
      FOR_EACH_FRAME (tail, lisp_frame)
	adjust_frame_glyphs (XFRAME (lisp_frame));
    }

  UNBLOCK_INPUT;
}


/* Adjust frame glyphs when Emacs is initialized.
   
   To be called from init_display. 
   
   We need a glyph matrix because redraw will happen soon.
   Unfortunately, window sizes on selected_frame are not yet set to
   meaningful values.  I believe we can assume that there are only two
   windows on the frame---the mini-buffer and the root window.  Frame
   height and width seem to be correct so far.  So, set the sizes of
   windows to estimated values.  */

static void
adjust_frame_glyphs_initially ()
{
  struct frame *sf = SELECTED_FRAME ();
  struct window *root = XWINDOW (sf->root_window);
  struct window *mini = XWINDOW (root->next);
  int frame_height = FRAME_HEIGHT (sf);
  int frame_width = FRAME_WIDTH (sf);
  int top_margin = FRAME_TOP_MARGIN (sf);

  /* Do it for the root window.  */
  XSETFASTINT (root->top, top_margin);
  XSETFASTINT (root->width, frame_width);
  set_window_height (sf->root_window, frame_height - 1 - top_margin, 0);

  /* Do it for the mini-buffer window.  */
  XSETFASTINT (mini->top, frame_height - 1);
  XSETFASTINT (mini->width, frame_width);
  set_window_height (root->next, 1, 0);

  adjust_frame_glyphs (sf);
  glyphs_initialized_initially_p = 1;
}
  

/* Allocate/reallocate glyph matrices of a single frame F.  */

static void
adjust_frame_glyphs (f)
     struct frame *f;
{
  if (FRAME_WINDOW_P (f))
    adjust_frame_glyphs_for_window_redisplay (f);
  else
    adjust_frame_glyphs_for_frame_redisplay (f);
  
  /* Don't forget the message buffer and the buffer for
     decode_mode_spec.  */
  adjust_frame_message_buffer (f);
  adjust_decode_mode_spec_buffer (f);

  f->glyphs_initialized_p = 1;
}


/* In the window tree with root W, build current matrices of leaf
   windows from the frame's current matrix.  */

static void
fake_current_matrices (window)
     Lisp_Object window;
{
  struct window *w;
      
  for (; !NILP (window); window = w->next)
    {
      w = XWINDOW (window);
      
      if (!NILP (w->hchild))
	fake_current_matrices (w->hchild);
      else if (!NILP (w->vchild))
	fake_current_matrices (w->vchild);
      else
	{
	  int i;
	  struct frame *f = XFRAME (w->frame);
	  struct glyph_matrix *m = w->current_matrix;
	  struct glyph_matrix *fm = f->current_matrix;

	  xassert (m->matrix_h == XFASTINT (w->height));
	  xassert (m->matrix_w == XFASTINT (w->width));
	  
	  for (i = 0; i < m->matrix_h; ++i)
	    {
	      struct glyph_row *r = m->rows + i;
	      struct glyph_row *fr = fm->rows + i + XFASTINT (w->top);

	      xassert (r->glyphs[TEXT_AREA] >= fr->glyphs[TEXT_AREA]
		       && r->glyphs[LAST_AREA] <= fr->glyphs[LAST_AREA]);

	      r->enabled_p = fr->enabled_p;
	      if (r->enabled_p)
		{
		  r->used[LEFT_MARGIN_AREA] = m->left_margin_glyphs;
		  r->used[RIGHT_MARGIN_AREA] = m->right_margin_glyphs;
		  r->used[TEXT_AREA] = (m->matrix_w
					- r->used[LEFT_MARGIN_AREA]
					- r->used[RIGHT_MARGIN_AREA]);
		  r->mode_line_p = 0;
		}
	    }
	}
    }
}


/* Save away the contents of frame F's current frame matrix.  Value is
   a glyph matrix holding the contents of F's current frame matrix.  */

static struct glyph_matrix *
save_current_matrix (f)
     struct frame *f;
{
  int i;
  struct glyph_matrix *saved;

  saved = (struct glyph_matrix *) xmalloc (sizeof *saved);
  bzero (saved, sizeof *saved);
  saved->nrows = f->current_matrix->nrows;
  saved->rows = (struct glyph_row *) xmalloc (saved->nrows
					      * sizeof *saved->rows);
  bzero (saved->rows, saved->nrows * sizeof *saved->rows);

  for (i = 0; i < saved->nrows; ++i)
    {
      struct glyph_row *from = f->current_matrix->rows + i;
      struct glyph_row *to = saved->rows + i;
      size_t nbytes = from->used[TEXT_AREA] * sizeof (struct glyph);
      to->glyphs[TEXT_AREA] = (struct glyph *) xmalloc (nbytes);
      bcopy (from->glyphs[TEXT_AREA], to->glyphs[TEXT_AREA], nbytes);
      to->used[TEXT_AREA] = from->used[TEXT_AREA];
    }

  return saved;
}


/* Restore the contents of frame F's current frame matrix from SAVED,
   and free memory associated with SAVED.  */

static void
restore_current_matrix (f, saved)
     struct frame *f;
     struct glyph_matrix *saved;
{
  int i;

  for (i = 0; i < saved->nrows; ++i)
    {
      struct glyph_row *from = saved->rows + i;
      struct glyph_row *to = f->current_matrix->rows + i;
      size_t nbytes = from->used[TEXT_AREA] * sizeof (struct glyph);
      bcopy (from->glyphs[TEXT_AREA], to->glyphs[TEXT_AREA], nbytes);
      to->used[TEXT_AREA] = from->used[TEXT_AREA];
      xfree (from->glyphs[TEXT_AREA]);
    }
  
  xfree (saved->rows);
  xfree (saved);
}



/* Allocate/reallocate glyph matrices of a single frame F for
   frame-based redisplay.  */

static void
adjust_frame_glyphs_for_frame_redisplay (f)
     struct frame *f;
{
  struct dim ch_dim;
  struct dim matrix_dim;
  int pool_changed_p;
  int window_change_flags;
  int top_window_y;

  if (!FRAME_LIVE_P (f))
    return;

  /* Determine the smallest character in any font for F.  On
     console windows, all characters have dimension (1, 1).  */
  ch_dim.width = ch_dim.height = 1;
  
  top_window_y = FRAME_TOP_MARGIN (f);

  /* Allocate glyph pool structures if not already done.  */
  if (f->desired_pool == NULL)
    {
      f->desired_pool = new_glyph_pool ();
      f->current_pool = new_glyph_pool ();
    }

  /* Allocate frames matrix structures if needed.  */
  if (f->desired_matrix == NULL)
    {
      f->desired_matrix = new_glyph_matrix (f->desired_pool);
      f->current_matrix = new_glyph_matrix (f->current_pool);
    }
  
  /* Compute window glyph matrices.  (This takes the mini-buffer
     window into account).  The result is the size of the frame glyph
     matrix needed.  The variable window_change_flags is set to a bit
     mask indicating whether new matrices will be allocated or
     existing matrices change their size or location within the frame
     matrix.  */
  window_change_flags = 0;
  matrix_dim
    = allocate_matrices_for_frame_redisplay (FRAME_ROOT_WINDOW (f),
					     0, top_window_y,
					     1,
					     &window_change_flags);

  /* Add in menu bar lines, if any.  */
  matrix_dim.height += top_window_y;

  /* Enlarge pools as necessary.  */
  pool_changed_p = realloc_glyph_pool (f->desired_pool, matrix_dim);
  realloc_glyph_pool (f->current_pool, matrix_dim);

  /* Set up glyph pointers within window matrices.  Do this only if 
     absolutely necessary since it requires a frame redraw.  */
  if (pool_changed_p || window_change_flags)
    {
      /* Do it for window matrices.  */
      allocate_matrices_for_frame_redisplay (FRAME_ROOT_WINDOW (f),
					     0, top_window_y, 0,
					     &window_change_flags);

      /* Size of frame matrices must equal size of frame.  Note
	 that we are called for X frames with window widths NOT equal
	 to the frame width (from CHANGE_FRAME_SIZE_1).  */
      xassert (matrix_dim.width == FRAME_WIDTH (f)
	       && matrix_dim.height == FRAME_HEIGHT (f));
  
      /* Pointers to glyph memory in glyph rows are exchanged during
	 the update phase of redisplay, which means in general that a
	 frame's current matrix consists of pointers into both the
	 desired and current glyph pool of the frame.  Adjusting a
	 matrix sets the frame matrix up so that pointers are all into
	 the same pool.  If we want to preserve glyph contents of the
	 current matrix over a call to adjust_glyph_matrix, we must
	 make a copy of the current glyphs, and restore the current
	 matrix' contents from that copy.  */
      if (display_completed
	  && !FRAME_GARBAGED_P (f)
	  && matrix_dim.width == f->current_matrix->matrix_w
	  && matrix_dim.height == f->current_matrix->matrix_h)
	{
	  struct glyph_matrix *copy = save_current_matrix (f);
	  adjust_glyph_matrix (NULL, f->desired_matrix, 0, 0, matrix_dim);
	  adjust_glyph_matrix (NULL, f->current_matrix, 0, 0, matrix_dim);
	  restore_current_matrix (f, copy);
	  fake_current_matrices (FRAME_ROOT_WINDOW (f));
	}
      else
	{
	  adjust_glyph_matrix (NULL, f->desired_matrix, 0, 0, matrix_dim);
	  adjust_glyph_matrix (NULL, f->current_matrix, 0, 0, matrix_dim);
	  SET_FRAME_GARBAGED (f);
	}
    }
}


/* Allocate/reallocate glyph matrices of a single frame F for
   window-based redisplay.  */

static void
adjust_frame_glyphs_for_window_redisplay (f)
     struct frame *f;
{
  struct dim ch_dim;
  struct window *w;

  xassert (FRAME_WINDOW_P (f) && FRAME_LIVE_P (f));
  
  /* Get minimum sizes.  */
#ifdef HAVE_WINDOW_SYSTEM
  ch_dim.width = FRAME_SMALLEST_CHAR_WIDTH (f);
  ch_dim.height = FRAME_SMALLEST_FONT_HEIGHT (f);
#else
  ch_dim.width = ch_dim.height = 1;
#endif
    
  /* Allocate/reallocate window matrices.  */
  allocate_matrices_for_window_redisplay (XWINDOW (FRAME_ROOT_WINDOW (f)));

  /* Allocate/ reallocate matrices of the dummy window used to display
     the menu bar under X when no X toolkit support is available.  */
#ifndef USE_X_TOOLKIT
  {
    /* Allocate a dummy window if not already done.  */
    if (NILP (f->menu_bar_window))
      {
	f->menu_bar_window = make_window ();
	w = XWINDOW (f->menu_bar_window);
	XSETFRAME (w->frame, f);
	w->pseudo_window_p = 1;
      }
    else
      w = XWINDOW (f->menu_bar_window);

    /* Set window dimensions to frame dimensions and allocate or
       adjust glyph matrices of W.  */
    XSETFASTINT (w->top, 0);
    XSETFASTINT (w->left, 0);
    XSETFASTINT (w->height, FRAME_MENU_BAR_LINES (f));
    XSETFASTINT (w->width, FRAME_WINDOW_WIDTH (f));
    allocate_matrices_for_window_redisplay (w);
  }
#endif /* not USE_X_TOOLKIT */

  /* Allocate/ reallocate matrices of the tool bar window.  If we
     don't have a tool bar window yet, make one.  */
  if (NILP (f->tool_bar_window))
    {
      f->tool_bar_window = make_window ();
      w = XWINDOW (f->tool_bar_window);
      XSETFRAME (w->frame, f);
      w->pseudo_window_p = 1;
    }
  else
    w = XWINDOW (f->tool_bar_window);

  XSETFASTINT (w->top, FRAME_MENU_BAR_LINES (f));
  XSETFASTINT (w->left, 0);
  XSETFASTINT (w->height, FRAME_TOOL_BAR_LINES (f));
  XSETFASTINT (w->width, FRAME_WINDOW_WIDTH (f));
  allocate_matrices_for_window_redisplay (w);
}


/* Adjust/ allocate message buffer of frame F. 

   Note that the message buffer is never freed.  Since I could not
   find a free in 19.34, I assume that freeing it would be
   problematic in some way and don't do it either.

   (Implementation note: It should be checked if we can free it
   eventually without causing trouble).  */

static void
adjust_frame_message_buffer (f)
     struct frame *f;
{
  int size = FRAME_MESSAGE_BUF_SIZE (f) + 1;

  if (FRAME_MESSAGE_BUF (f))
    {
      char *buffer = FRAME_MESSAGE_BUF (f);
      char *new_buffer = (char *) xrealloc (buffer, size);
      FRAME_MESSAGE_BUF (f) = new_buffer;
    }
  else
    FRAME_MESSAGE_BUF (f) = (char *) xmalloc (size);
}


/* Re-allocate buffer for decode_mode_spec on frame F.  */

static void
adjust_decode_mode_spec_buffer (f)
     struct frame *f;
{
  f->decode_mode_spec_buffer
    = (char *) xrealloc (f->decode_mode_spec_buffer,
			 FRAME_MESSAGE_BUF_SIZE (f) + 1);
}



/**********************************************************************
			Freeing Glyph Matrices
 **********************************************************************/

/* Free glyph memory for a frame F.  F may be null.  This function can
   be called for the same frame more than once.  The root window of
   F may be nil when this function is called.  This is the case when
   the function is called when F is destroyed.  */

void
free_glyphs (f)
     struct frame *f;
{
  if (f && f->glyphs_initialized_p)
    {
      /* Block interrupt input so that we don't get surprised by an X
         event while we're in an inconsistent state.  */
      BLOCK_INPUT;
      f->glyphs_initialized_p = 0;
      
      /* Release window sub-matrices.  */
      if (!NILP (f->root_window))
        free_window_matrices (XWINDOW (f->root_window));

      /* Free the dummy window for menu bars without X toolkit and its
	 glyph matrices.  */
      if (!NILP (f->menu_bar_window))
	{
	  struct window *w = XWINDOW (f->menu_bar_window);
	  free_glyph_matrix (w->desired_matrix);
	  free_glyph_matrix (w->current_matrix);
	  w->desired_matrix = w->current_matrix = NULL;
	  f->menu_bar_window = Qnil;
	}

      /* Free the tool bar window and its glyph matrices.  */
      if (!NILP (f->tool_bar_window))
	{
	  struct window *w = XWINDOW (f->tool_bar_window);
	  free_glyph_matrix (w->desired_matrix);
	  free_glyph_matrix (w->current_matrix);
	  w->desired_matrix = w->current_matrix = NULL;
	  f->tool_bar_window = Qnil;
	}

      /* Release frame glyph matrices.  Reset fields to zero in
	 case we are called a second time.  */
      if (f->desired_matrix)
	{
	  free_glyph_matrix (f->desired_matrix);
	  free_glyph_matrix (f->current_matrix);
	  f->desired_matrix = f->current_matrix = NULL;
	}

      /* Release glyph pools.  */
      if (f->desired_pool)
	{
	  free_glyph_pool (f->desired_pool);
	  free_glyph_pool (f->current_pool);
	  f->desired_pool = f->current_pool = NULL;
	}
      
      UNBLOCK_INPUT;
    }
}


/* Free glyph sub-matrices in the window tree rooted at W.  This
   function may be called with a null pointer, and it may be called on
   the same tree more than once.  */

void
free_window_matrices (w)
     struct window *w;
{
  while (w)
    {
      if (!NILP (w->hchild))
	free_window_matrices (XWINDOW (w->hchild));
      else if (!NILP (w->vchild))
	free_window_matrices (XWINDOW (w->vchild));
      else 
	{
	  /* This is a leaf window.  Free its memory and reset fields
	     to zero in case this function is called a second time for
	     W.  */
	  free_glyph_matrix (w->current_matrix);
	  free_glyph_matrix (w->desired_matrix);
	  w->current_matrix = w->desired_matrix = NULL;
	}

      /* Next window on same level.  */
      w = NILP (w->next) ? 0 : XWINDOW (w->next);
    }
}


/* Check glyph memory leaks.  This function is called from
   shut_down_emacs.  Note that frames are not destroyed when Emacs
   exits.  We therefore free all glyph memory for all active frames
   explicitly and check that nothing is left allocated.  */

void
check_glyph_memory ()
{
  Lisp_Object tail, frame;

  /* Free glyph memory for all frames.  */
  FOR_EACH_FRAME (tail, frame)
    free_glyphs (XFRAME (frame));

  /* Check that nothing is left allocated.  */
  if (glyph_matrix_count)
    abort ();
  if (glyph_pool_count)
    abort ();
}



/**********************************************************************
		       Building a Frame Matrix
 **********************************************************************/

/* Most of the redisplay code works on glyph matrices attached to
   windows.  This is a good solution most of the time, but it is not
   suitable for terminal code.  Terminal output functions cannot rely
   on being able to set an arbitrary terminal window.  Instead they
   must be provided with a view of the whole frame, i.e. the whole
   screen.  We build such a view by constructing a frame matrix from
   window matrices in this section.

   Windows that must be updated have their must_be_update_p flag set.
   For all such windows, their desired matrix is made part of the
   desired frame matrix.  For other windows, their current matrix is
   made part of the desired frame matrix.

   +-----------------+----------------+
   |     desired     |   desired      |
   |                 |                |
   +-----------------+----------------+
   |               current            |
   |                                  |
   +----------------------------------+

   Desired window matrices can be made part of the frame matrix in a
   cheap way: We exploit the fact that the desired frame matrix and
   desired window matrices share their glyph memory.  This is not
   possible for current window matrices.  Their glyphs are copied to
   the desired frame matrix.  The latter is equivalent to
   preserve_other_columns in the old redisplay.

   Used glyphs counters for frame matrix rows are the result of adding
   up glyph lengths of the window matrices.  A line in the frame
   matrix is enabled, if a corresponding line in a window matrix is
   enabled.
   
   After building the desired frame matrix, it will be passed to
   terminal code, which will manipulate both the desired and current
   frame matrix.  Changes applied to the frame's current matrix have
   to be visible in current window matrices afterwards, of course.

   This problem is solved like this:

   1. Window and frame matrices share glyphs.  Window matrices are
   constructed in a way that their glyph contents ARE the glyph
   contents needed in a frame matrix.  Thus, any modification of
   glyphs done in terminal code will be reflected in window matrices
   automatically.
   
   2. Exchanges of rows in a frame matrix done by terminal code are
   intercepted by hook functions so that corresponding row operations
   on window matrices can be performed.  This is necessary because we
   use pointers to glyphs in glyph row structures.  To satisfy the
   assumption of point 1 above that glyphs are updated implicitly in
   window matrices when they are manipulated via the frame matrix,
   window and frame matrix must of course agree where to find the
   glyphs for their rows.  Possible manipulations that must be
   mirrored are assignments of rows of the desired frame matrix to the
   current frame matrix and scrolling the current frame matrix.  */

/* Build frame F's desired matrix from window matrices.  Only windows
   which have the flag must_be_updated_p set have to be updated.  Menu
   bar lines of a frame are not covered by window matrices, so make
   sure not to touch them in this function.  */

static void
build_frame_matrix (f)
     struct frame *f;
{
  int i;

  /* F must have a frame matrix when this function is called.  */
  xassert (!FRAME_WINDOW_P (f));
  
  /* Clear all rows in the frame matrix covered by window matrices.
     Menu bar lines are not covered by windows.  */
  for (i = FRAME_TOP_MARGIN (f); i < f->desired_matrix->nrows; ++i)
    clear_glyph_row (MATRIX_ROW (f->desired_matrix, i));

  /* Build the matrix by walking the window tree.  */
  build_frame_matrix_from_window_tree (f->desired_matrix,
				       XWINDOW (FRAME_ROOT_WINDOW (f)));
}


/* Walk a window tree, building a frame matrix MATRIX from window
   matrices.  W is the root of a window tree.  */

static void
build_frame_matrix_from_window_tree (matrix, w)
     struct glyph_matrix *matrix;
     struct window *w;
{
  while (w)
    {
      if (!NILP (w->hchild))
	build_frame_matrix_from_window_tree (matrix, XWINDOW (w->hchild));
      else if (!NILP (w->vchild))
	build_frame_matrix_from_window_tree (matrix, XWINDOW (w->vchild));
      else
	build_frame_matrix_from_leaf_window (matrix, w);

      w = NILP (w->next) ? 0 : XWINDOW (w->next);
    }
}


/* Add a window's matrix to a frame matrix.  FRAME_MATRIX is the
   desired frame matrix built.  W is a leaf window whose desired or
   current matrix is to be added to FRAME_MATRIX.  W's flag
   must_be_updated_p determines which matrix it contributes to
   FRAME_MATRIX.  If must_be_updated_p is non-zero, W's desired matrix
   is added to FRAME_MATRIX, otherwise W's current matrix is added.
   Adding a desired matrix means setting up used counters and such in
   frame rows, while adding a current window matrix to FRAME_MATRIX
   means copying glyphs.  The latter case corresponds to
   preserve_other_columns in the old redisplay.  */

static void
build_frame_matrix_from_leaf_window (frame_matrix, w)
     struct glyph_matrix *frame_matrix;
     struct window *w;
{
  struct glyph_matrix *window_matrix;
  int window_y, frame_y;
  /* If non-zero, a glyph to insert at the right border of W.  */
  GLYPH right_border_glyph = 0;

  /* Set window_matrix to the matrix we have to add to FRAME_MATRIX.  */
  if (w->must_be_updated_p)
    {
      window_matrix = w->desired_matrix;

      /* Decide whether we want to add a vertical border glyph.  */
      if (!WINDOW_RIGHTMOST_P (w))
	{
	  struct Lisp_Char_Table *dp = window_display_table (w);
	  right_border_glyph = (dp && INTEGERP (DISP_BORDER_GLYPH (dp))
				? XINT (DISP_BORDER_GLYPH (dp))
				: '|');
	}
    }
  else
    window_matrix = w->current_matrix;

  /* For all rows in the window matrix and corresponding rows in the
     frame matrix.  */
  window_y = 0;
  frame_y = window_matrix->matrix_y;
  while (window_y < window_matrix->nrows)
    {
      struct glyph_row *frame_row = frame_matrix->rows + frame_y;
      struct glyph_row *window_row = window_matrix->rows + window_y;
      int current_row_p = window_matrix == w->current_matrix;

      /* Fill up the frame row with spaces up to the left margin of the
	 window row.  */
      fill_up_frame_row_with_spaces (frame_row, window_matrix->matrix_x);

      /* Fill up areas in the window matrix row with spaces.  */
      fill_up_glyph_row_with_spaces (window_row);

      /* If only part of W's desired matrix has been built, and
         window_row wasn't displayed, use the corresponding current
         row instead.  */
      if (window_matrix == w->desired_matrix
	  && !window_row->enabled_p)
	{
	  window_row = w->current_matrix->rows + window_y;
	  current_row_p = 1;
	}
      
      if (current_row_p)
	{
	  /* Copy window row to frame row.  */
	  bcopy (window_row->glyphs[0],
		 frame_row->glyphs[TEXT_AREA] + window_matrix->matrix_x,
		 window_matrix->matrix_w * sizeof (struct glyph));
	}
      else
	{
	  xassert (window_row->enabled_p);
	  
	  /* Only when a desired row has been displayed, we want
	     the corresponding frame row to be updated.  */
	  frame_row->enabled_p = 1;
	  
          /* Maybe insert a vertical border between horizontally adjacent
	     windows.  */
          if (right_border_glyph)
	    {
              struct glyph *border = window_row->glyphs[LAST_AREA] - 1;
	      SET_CHAR_GLYPH_FROM_GLYPH (*border, right_border_glyph);
	    }

	  /* Window row window_y must be a slice of frame row
	     frame_y.  */
	  xassert (glyph_row_slice_p (window_row, frame_row));
	  
	  /* If rows are in sync, we don't have to copy glyphs because
	     frame and window share glyphs.  */
	  
#if GLYPH_DEBUG
	  strcpy (w->current_matrix->method, w->desired_matrix->method);
	  add_window_display_history (w, w->current_matrix->method, 0);
#endif
	}

      /* Set number of used glyphs in the frame matrix.  Since we fill
         up with spaces, and visit leaf windows from left to right it
         can be done simply.  */
      frame_row->used[TEXT_AREA] 
	= window_matrix->matrix_x + window_matrix->matrix_w;

      /* Next row.  */
      ++window_y;
      ++frame_y;
    }
}


/* Add spaces to a glyph row ROW in a window matrix.

   Each row has the form:

   +---------+-----------------------------+------------+
   | left    |	text			   | right	|
   +---------+-----------------------------+------------+

   Left and right marginal areas are optional.  This function adds
   spaces to areas so that there are no empty holes between areas.
   In other words:  If the right area is not empty, the text area
   is filled up with spaces up to the right area.   If the text area
   is not empty, the left area is filled up.

   To be called for frame-based redisplay, only.  */

static void
fill_up_glyph_row_with_spaces (row)
     struct glyph_row *row;
{
  fill_up_glyph_row_area_with_spaces (row, LEFT_MARGIN_AREA);
  fill_up_glyph_row_area_with_spaces (row, TEXT_AREA);
  fill_up_glyph_row_area_with_spaces (row, RIGHT_MARGIN_AREA);
}


/* Fill area AREA of glyph row ROW with spaces.  To be called for
   frame-based redisplay only.  */

static void
fill_up_glyph_row_area_with_spaces (row, area)
     struct glyph_row *row;
     int area;
{
  if (row->glyphs[area] < row->glyphs[area + 1])
    {
      struct glyph *end = row->glyphs[area + 1];
      struct glyph *text = row->glyphs[area] + row->used[area];

      while (text < end)
	*text++ = space_glyph;
      row->used[area] = text - row->glyphs[area];
    }
}


/* Add spaces to the end of ROW in a frame matrix until index UPTO is
   reached.  In frame matrices only one area, TEXT_AREA, is used.  */

static void
fill_up_frame_row_with_spaces (row, upto)
     struct glyph_row *row;
     int upto;
{
  int i = row->used[TEXT_AREA];
  struct glyph *glyph = row->glyphs[TEXT_AREA];
  
  while (i < upto)
    glyph[i++] = space_glyph;

  row->used[TEXT_AREA] = i;
}



/**********************************************************************
      Mirroring operations on frame matrices in window matrices
 **********************************************************************/

/* Set frame being updated via frame-based redisplay to F.  This
   function must be called before updates to make explicit that we are
   working on frame matrices or not.  */

static INLINE void
set_frame_matrix_frame (f)
     struct frame *f;
{
  frame_matrix_frame = f;
}


/* Make sure glyph row ROW in CURRENT_MATRIX is up to date.
   DESIRED_MATRIX is the desired matrix corresponding to
   CURRENT_MATRIX.  The update is done by exchanging glyph pointers
   between rows in CURRENT_MATRIX and DESIRED_MATRIX.  If
   frame_matrix_frame is non-null, this indicates that the exchange is
   done in frame matrices, and that we have to perform analogous
   operations in window matrices of frame_matrix_frame.  */

static INLINE void
make_current (desired_matrix, current_matrix, row)
     struct glyph_matrix *desired_matrix, *current_matrix;
     int row;
{
  struct glyph_row *current_row = MATRIX_ROW (current_matrix, row);
  struct glyph_row *desired_row = MATRIX_ROW (desired_matrix, row);
  int mouse_face_p = current_row->mouse_face_p;

  /* Do current_row = desired_row.  This exchanges glyph pointers
     between both rows, and does a structure assignment otherwise.  */
  assign_row (current_row, desired_row);

  /* Enable current_row to mark it as valid.  */
  current_row->enabled_p = 1;
  current_row->mouse_face_p = mouse_face_p;

  /* If we are called on frame matrices, perform analogous operations
     for window matrices.  */
  if (frame_matrix_frame)
    mirror_make_current (XWINDOW (frame_matrix_frame->root_window), row);
}


/* W is the root of a window tree.  FRAME_ROW is the index of a row in
   W's frame which has been made current (by swapping pointers between
   current and desired matrix).  Perform analogous operations in the
   matrices of leaf windows in the window tree rooted at W.  */

static void
mirror_make_current (w, frame_row)
     struct window *w;
     int frame_row;
{
  while (w)
    {
      if (!NILP (w->hchild))
 	mirror_make_current (XWINDOW (w->hchild), frame_row);
      else if (!NILP (w->vchild))
	mirror_make_current (XWINDOW (w->vchild), frame_row);
      else
	{
	  /* Row relative to window W.  Don't use FRAME_TO_WINDOW_VPOS
	     here because the checks performed in debug mode there
	     will not allow the conversion.  */
	  int row = frame_row - w->desired_matrix->matrix_y;

	  /* If FRAME_ROW is within W, assign the desired row to the
	     current row (exchanging glyph pointers).  */
	  if (row >= 0 && row < w->desired_matrix->matrix_h)
	    {
	      struct glyph_row *current_row
		= MATRIX_ROW (w->current_matrix, row);
	      struct glyph_row *desired_row
		= MATRIX_ROW (w->desired_matrix, row);

	      if (desired_row->enabled_p)
		assign_row (current_row, desired_row);
	      else
		swap_glyph_pointers (desired_row, current_row);
	      current_row->enabled_p = 1;
	    }
	}
      
      w = NILP (w->next) ? 0 : XWINDOW (w->next);
    }
}


/* Perform row dance after scrolling.  We are working on the range of
   lines UNCHANGED_AT_TOP + 1 to UNCHANGED_AT_TOP + NLINES (not
   including) in MATRIX.  COPY_FROM is a vector containing, for each
   row I in the range 0 <= I < NLINES, the index of the original line
   to move to I.  This index is relative to the row range, i.e. 0 <=
   index < NLINES.  RETAINED_P is a vector containing zero for each
   row 0 <= I < NLINES which is empty.

   This function is called from do_scrolling and do_direct_scrolling.  */
   
void
mirrored_line_dance (matrix, unchanged_at_top, nlines, copy_from,
		     retained_p)
     struct glyph_matrix *matrix;
     int unchanged_at_top, nlines;
     int *copy_from;
     char *retained_p;
{
  /* A copy of original rows.  */
  struct glyph_row *old_rows;

  /* Rows to assign to.  */
  struct glyph_row *new_rows = MATRIX_ROW (matrix, unchanged_at_top);
  
  int i;

  /* Make a copy of the original rows.  */
  old_rows = (struct glyph_row *) alloca (nlines * sizeof *old_rows);
  bcopy (new_rows, old_rows, nlines * sizeof *old_rows);

  /* Assign new rows, maybe clear lines.  */
  for (i = 0; i < nlines; ++i)
    {
      int enabled_before_p = new_rows[i].enabled_p;

      xassert (i + unchanged_at_top < matrix->nrows);
      xassert (unchanged_at_top + copy_from[i] < matrix->nrows);
      new_rows[i] = old_rows[copy_from[i]];
      new_rows[i].enabled_p = enabled_before_p;

      /* RETAINED_P is zero for empty lines.  */
      if (!retained_p[copy_from[i]])
	new_rows[i].enabled_p = 0;
    }

  /* Do the same for window matrices, if MATRIX is a frame matrix.  */
  if (frame_matrix_frame)
    mirror_line_dance (XWINDOW (frame_matrix_frame->root_window),
		       unchanged_at_top, nlines, copy_from, retained_p);
}


/* Synchronize glyph pointers in the current matrix of window W with
   the current frame matrix.  */

static void
sync_window_with_frame_matrix_rows (w)
     struct window *w;
{
  struct frame *f = XFRAME (w->frame);
  struct glyph_row *window_row, *window_row_end, *frame_row;
  int left, right, x, width;

  /* Preconditions: W must be a leaf window on a tty frame.  */
  xassert (NILP (w->hchild) && NILP (w->vchild));
  xassert (!FRAME_WINDOW_P (f));

  left = margin_glyphs_to_reserve (w, 1, w->left_margin_width);
  right = margin_glyphs_to_reserve (w, 1, w->right_margin_width);
  x = w->current_matrix->matrix_x;
  width = w->current_matrix->matrix_w;

  window_row = w->current_matrix->rows;
  window_row_end = window_row + w->current_matrix->nrows;
  frame_row = f->current_matrix->rows + XFASTINT (w->top);
  
  for (; window_row < window_row_end; ++window_row, ++frame_row)
    {
      window_row->glyphs[LEFT_MARGIN_AREA]
	= frame_row->glyphs[0] + x;
      window_row->glyphs[TEXT_AREA]
	= window_row->glyphs[LEFT_MARGIN_AREA] + left;
      window_row->glyphs[LAST_AREA]
	= window_row->glyphs[LEFT_MARGIN_AREA] + width;
      window_row->glyphs[RIGHT_MARGIN_AREA]
	= window_row->glyphs[LAST_AREA] - right;
    }
}


/* Return the window in the window tree rooted in W containing frame
   row ROW.  Value is null if none is found.  */

struct window *
frame_row_to_window (w, row)
     struct window *w;
     int row;
{
  struct window *found = NULL;
  
  while (w && !found)
    {
      if (!NILP (w->hchild))
 	found = frame_row_to_window (XWINDOW (w->hchild), row);
      else if (!NILP (w->vchild))
	found = frame_row_to_window (XWINDOW (w->vchild), row);
      else if (row >= XFASTINT (w->top)
	       && row < XFASTINT (w->top) + XFASTINT (w->height))
	found = w;
      
      w = NILP (w->next) ? 0 : XWINDOW (w->next);
    }

  return found;
}


/* Perform a line dance in the window tree rooted at W, after
   scrolling a frame matrix in mirrored_line_dance.

   We are working on the range of lines UNCHANGED_AT_TOP + 1 to
   UNCHANGED_AT_TOP + NLINES (not including) in W's frame matrix.
   COPY_FROM is a vector containing, for each row I in the range 0 <=
   I < NLINES, the index of the original line to move to I.  This
   index is relative to the row range, i.e. 0 <= index < NLINES.
   RETAINED_P is a vector containing zero for each row 0 <= I < NLINES
   which is empty.  */

static void
mirror_line_dance (w, unchanged_at_top, nlines, copy_from, retained_p)
     struct window *w;
     int unchanged_at_top, nlines;
     int *copy_from;
     char *retained_p;
{
  while (w)
    {
      if (!NILP (w->hchild))
	mirror_line_dance (XWINDOW (w->hchild), unchanged_at_top,
			   nlines, copy_from, retained_p);
      else if (!NILP (w->vchild))
	mirror_line_dance (XWINDOW (w->vchild), unchanged_at_top,
			   nlines, copy_from, retained_p);
      else
	{
	  /* W is a leaf window, and we are working on its current
	     matrix m.  */
	  struct glyph_matrix *m = w->current_matrix;
	  int i, sync_p = 0;
	  struct glyph_row *old_rows;

	  /* Make a copy of the original rows of matrix m.  */
	  old_rows = (struct glyph_row *) alloca (m->nrows * sizeof *old_rows);
	  bcopy (m->rows, old_rows, m->nrows * sizeof *old_rows);

	  for (i = 0; i < nlines; ++i)
	    {
	      /* Frame relative line assigned to.  */
	      int frame_to = i + unchanged_at_top;
	      
	      /* Frame relative line assigned.  */
	      int frame_from = copy_from[i] + unchanged_at_top;
	      
	      /* Window relative line assigned to.  */
	      int window_to = frame_to - m->matrix_y;
	      
	      /* Window relative line assigned.  */
	      int window_from = frame_from - m->matrix_y;
	      
	      /* Is assigned line inside window?  */
	      int from_inside_window_p
		= window_from >= 0 && window_from < m->matrix_h;
	      
	      /* Is assigned to line inside window?  */
	      int to_inside_window_p
		= window_to >= 0 && window_to < m->matrix_h;
	      
	      if (from_inside_window_p && to_inside_window_p)
		{
		  /* Enabled setting before assignment.  */
		  int enabled_before_p;
		  
		  /* Do the assignment.  The enabled_p flag is saved
		     over the assignment because the old redisplay did
		     that.  */
		  enabled_before_p = m->rows[window_to].enabled_p;
		  m->rows[window_to] = old_rows[window_from];
		  m->rows[window_to].enabled_p = enabled_before_p;
		  
		  /* If frame line is empty, window line is empty, too.  */
		  if (!retained_p[copy_from[i]])
		    m->rows[window_to].enabled_p = 0;
		}
	      else if (to_inside_window_p)
		{
		  /* A copy between windows.  This is an infrequent
		     case not worth optimizing.  */
		  struct frame *f = XFRAME (w->frame);
		  struct window *root = XWINDOW (FRAME_ROOT_WINDOW (f));
		  struct window *w2;
		  struct glyph_matrix *m2;
		  int m2_from;

		  w2 = frame_row_to_window (root, frame_to);
		  m2 = w2->current_matrix;
		  m2_from = frame_from - m2->matrix_y;
		  copy_row_except_pointers (m->rows + window_to,
					    m2->rows + m2_from);
		  
		  /* If frame line is empty, window line is empty, too.  */
		  if (!retained_p[copy_from[i]])
		    m->rows[window_to].enabled_p = 0;
		  sync_p = 1;
		}
	      else if (from_inside_window_p)
		sync_p = 1;
	    }

	  /* If there was a copy between windows, make sure glyph
	     pointers are in sync with the frame matrix.  */
	  if (sync_p)
	    sync_window_with_frame_matrix_rows (w);
	  
	  /* Check that no pointers are lost.  */
	  CHECK_MATRIX (m);
	}

      /* Next window on same level.  */
      w = NILP (w->next) ? 0 : XWINDOW (w->next);
    }
}


#if GLYPH_DEBUG

/* Check that window and frame matrices agree about their
   understanding where glyphs of the rows are to find.  For each
   window in the window tree rooted at W, check that rows in the
   matrices of leaf window agree with their frame matrices about
   glyph pointers.  */

void
check_window_matrix_pointers (w)
     struct window *w;
{
  while (w)
    {
      if (!NILP (w->hchild))
	check_window_matrix_pointers (XWINDOW (w->hchild));
      else if (!NILP (w->vchild))
	check_window_matrix_pointers (XWINDOW (w->vchild));
      else
	{
	  struct frame *f = XFRAME (w->frame);
	  check_matrix_pointers (w->desired_matrix, f->desired_matrix);
	  check_matrix_pointers (w->current_matrix, f->current_matrix);
	}
      
      w = NILP (w->next) ? 0 : XWINDOW (w->next);
    }
}


/* Check that window rows are slices of frame rows.  WINDOW_MATRIX is
   a window and FRAME_MATRIX is the corresponding frame matrix.  For
   each row in WINDOW_MATRIX check that it's a slice of the
   corresponding frame row.  If it isn't, abort.  */

static void
check_matrix_pointers (window_matrix, frame_matrix)
     struct glyph_matrix *window_matrix, *frame_matrix;
{
  /* Row number in WINDOW_MATRIX.  */
  int i = 0;

  /* Row number corresponding to I in FRAME_MATRIX.  */
  int j = window_matrix->matrix_y;

  /* For all rows check that the row in the window matrix is a 
     slice of the row in the frame matrix.  If it isn't we didn't
     mirror an operation on the frame matrix correctly.  */
  while (i < window_matrix->nrows)
    {
      if (!glyph_row_slice_p (window_matrix->rows + i,
			      frame_matrix->rows + j))
        abort ();
      ++i, ++j;
    }
}

#endif /* GLYPH_DEBUG != 0 */



/**********************************************************************
		      VPOS and HPOS translations
 **********************************************************************/

#if GLYPH_DEBUG

/* Translate vertical position VPOS which is relative to window W to a
   vertical position relative to W's frame.  */

static int
window_to_frame_vpos (w, vpos)
     struct window *w;
     int vpos;
{
  struct frame *f = XFRAME (w->frame);
  
  xassert (!FRAME_WINDOW_P (f));
  xassert (vpos >= 0 && vpos <= w->desired_matrix->nrows);
  vpos += XFASTINT (w->top);
  xassert (vpos >= 0 && vpos <= FRAME_HEIGHT (f));
  return vpos;
}


/* Translate horizontal position HPOS which is relative to window W to
   a horizontal position relative to W's frame.  */

static int
window_to_frame_hpos (w, hpos)
     struct window *w;
     int hpos;
{
  struct frame *f = XFRAME (w->frame);
  
  xassert (!FRAME_WINDOW_P (f));
  hpos += XFASTINT (w->left);
  return hpos;
}
  
#endif /* GLYPH_DEBUG */



/**********************************************************************
			    Redrawing Frames
 **********************************************************************/

DEFUN ("redraw-frame", Fredraw_frame, Sredraw_frame, 1, 1, 0,
       doc: /* Clear frame FRAME and output again what is supposed to appear on it.  */)
     (frame)
     Lisp_Object frame;
{
  struct frame *f;

  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);

  /* Ignore redraw requests, if frame has no glyphs yet.
     (Implementation note: It still has to be checked why we are
     called so early here).  */
  if (!glyphs_initialized_initially_p)
    return Qnil;

  update_begin (f);
  if (FRAME_MSDOS_P (f))
    set_terminal_modes ();
  clear_frame ();
  clear_current_matrices (f);
  update_end (f);
  fflush (stdout);
  windows_or_buffers_changed++;
  /* Mark all windows as inaccurate, so that every window will have
     its redisplay done.  */
  mark_window_display_accurate (FRAME_ROOT_WINDOW (f), 0);
  set_window_update_flags (XWINDOW (FRAME_ROOT_WINDOW (f)), 1);
  f->garbaged = 0;
  return Qnil;
}


/* Redraw frame F.  This is nothing more than a call to the Lisp
   function redraw-frame.  */

void
redraw_frame (f)
     struct frame *f;
{
  Lisp_Object frame;
  XSETFRAME (frame, f);
  Fredraw_frame (frame);
}


DEFUN ("redraw-display", Fredraw_display, Sredraw_display, 0, 0, "",
       doc: /* Clear and redisplay all visible frames.  */)
     ()
{
  Lisp_Object tail, frame;

  FOR_EACH_FRAME (tail, frame)
    if (FRAME_VISIBLE_P (XFRAME (frame)))
      Fredraw_frame (frame);

  return Qnil;
}


/* This is used when frame_garbaged is set.  Call Fredraw_frame on all
   visible frames marked as garbaged.  */

void
redraw_garbaged_frames ()
{
  Lisp_Object tail, frame;

  FOR_EACH_FRAME (tail, frame)
    if (FRAME_VISIBLE_P (XFRAME (frame))
	&& FRAME_GARBAGED_P (XFRAME (frame)))
      Fredraw_frame (frame);
}



/***********************************************************************
			  Direct Operations
 ***********************************************************************/

/* Try to update display and current glyph matrix directly.

   This function is called after a character G has been inserted into
   current_buffer.  It tries to update the current glyph matrix and
   perform appropriate screen output to reflect the insertion.  If it
   succeeds, the global flag redisplay_performed_directly_p will be
   set to 1, and thereby prevent the more costly general redisplay
   from running (see redisplay_internal).

   This function is not called for `hairy' character insertions.
   In particular, it is not called when after or before change
   functions exist, like they are used by font-lock.  See keyboard.c
   for details where this function is called.  */

int
direct_output_for_insert (g)
     int g;
{
  register struct frame *f = SELECTED_FRAME ();
  struct window *w = XWINDOW (selected_window);
  struct it it, it2;
  struct glyph_row *glyph_row;
  struct glyph *glyphs, *glyph, *end;
  int n;
  /* Non-null means that redisplay of W is based on window matrices.  */
  int window_redisplay_p = FRAME_WINDOW_P (f);
  /* Non-null means we are in overwrite mode.  */
  int overwrite_p = !NILP (current_buffer->overwrite_mode);
  int added_width;
  struct text_pos pos;
  int delta, delta_bytes;

  /* Not done directly.  */
  redisplay_performed_directly_p = 0;

  /* Quickly give up for some common cases.  */
  if (cursor_in_echo_area
      /* Give up if fonts have changed.  */
      || fonts_changed_p
      /* Give up if face attributes have been changed.  */
      || face_change_count
      /* Give up if cursor position not really known.  */
      || !display_completed
      /* Give up if buffer appears in two places.  */
      || buffer_shared > 1
      /* Give up if currently displaying a message instead of the
	 minibuffer contents.  */
      || (EQ (selected_window, minibuf_window)
	  && EQ (minibuf_window, echo_area_window))
      /* Give up for hscrolled mini-buffer because display of the prompt
	 is handled specially there (see display_line).  */
      || (MINI_WINDOW_P (w) && XFASTINT (w->hscroll))
      /* Give up if overwriting in the middle of a line.  */
      || (overwrite_p 
	  && PT != ZV 
	  && FETCH_BYTE (PT) != '\n')
      /* Give up for tabs and line ends.  */
      || g == '\t'
      || g == '\n'
      || g == '\r'
      /* Give up if unable to display the cursor in the window.  */
      || w->cursor.vpos < 0
      /* Give up if we are showing a message or just cleared the message
	 because we might need to resize the echo area window.  */
      || !NILP (echo_area_buffer[0])
      || !NILP (echo_area_buffer[1])
      || (glyph_row = MATRIX_ROW (w->current_matrix, w->cursor.vpos),
	  /* Can't do it in a continued line because continuation
	     lines would change.  */
	  (glyph_row->continued_p
	   /* Can't use this method if the line overlaps others or is
	      overlapped by others because these other lines would
	      have to be redisplayed.  */
	   || glyph_row->overlapping_p
	   || glyph_row->overlapped_p))
      /* Can't do it for partial width windows on terminal frames
	 because we can't clear to eol in such a window.  */
      || (!window_redisplay_p && !WINDOW_FULL_WIDTH_P (w)))
    return 0;

  /* If we can't insert glyphs, we can use this method only
     at the end of a line.  */
  if (!char_ins_del_ok)
    if (PT != ZV && FETCH_BYTE (PT_BYTE) != '\n')
      return 0;

  /* Set up a display iterator structure for W.  Glyphs will be
     produced in scratch_glyph_row.  Current position is W's cursor
     position.  */
  clear_glyph_row (&scratch_glyph_row);
  SET_TEXT_POS (pos, PT, PT_BYTE);
  DEC_TEXT_POS (pos, !NILP (current_buffer->enable_multibyte_characters));
  init_iterator (&it, w, CHARPOS (pos), BYTEPOS (pos), &scratch_glyph_row,
		 DEFAULT_FACE_ID);

  glyph_row = MATRIX_ROW (w->current_matrix, w->cursor.vpos);
  if (glyph_row->mouse_face_p)
    return 0;
  
  /* Give up if highlighting trailing whitespace and we have trailing
     whitespace in glyph_row.  We would have to remove the trailing
     whitespace face in that case.  */
  if (!NILP (Vshow_trailing_whitespace)
      && glyph_row->used[TEXT_AREA])
    {
      struct glyph *last;

      last = glyph_row->glyphs[TEXT_AREA] + glyph_row->used[TEXT_AREA] - 1;
      if (last->type == STRETCH_GLYPH
	  || (last->type == CHAR_GLYPH
	      && last->u.ch == ' '))
	return 0;
    }

  /* Give up if there are overlay strings at pos.  This would fail
     if the overlay string has newlines in it.  */
  if (STRINGP (it.string))
    return 0;
  
  it.hpos = w->cursor.hpos;
  it.vpos = w->cursor.vpos;
  it.current_x = w->cursor.x + it.first_visible_x;
  it.current_y = w->cursor.y;
  it.end_charpos = PT;
  it.stop_charpos = min (PT, it.stop_charpos);
  it.stop_charpos = max (IT_CHARPOS (it), it.stop_charpos);

  /* More than one display element may be returned for PT - 1 if
     (i) it's a control character which is translated into `\003' or
     `^C', or (ii) it has a display table entry, or (iii) it's a 
     combination of both.  */
  delta = delta_bytes = 0;
  while (get_next_display_element (&it))
    {
      PRODUCE_GLYPHS (&it);

      /* Give up if glyph doesn't fit completely on the line.  */
      if (it.current_x >= it.last_visible_x)
	return 0;

      /* Give up if new glyph has different ascent or descent than
	 the original row, or if it is not a character glyph.  */
      if (glyph_row->ascent != it.ascent
	  || glyph_row->height != it.ascent + it.descent
	  || glyph_row->phys_ascent != it.phys_ascent
	  || glyph_row->phys_height != it.phys_ascent + it.phys_descent
	  || it.what != IT_CHARACTER)
	return 0;

      delta += 1;
      delta_bytes += it.len;
      set_iterator_to_next (&it, 1);
    }

  /* Give up if we hit the right edge of the window.  We would have
     to insert truncation or continuation glyphs.  */
  added_width = it.current_x - (w->cursor.x + it.first_visible_x);
  if (glyph_row->pixel_width + added_width >= it.last_visible_x)
    return 0;

  /* Give up if there is a \t following in the line.  */
  it2 = it;
  it2.end_charpos = ZV;
  it2.stop_charpos = min (it2.stop_charpos, ZV);
  while (get_next_display_element (&it2)
	 && !ITERATOR_AT_END_OF_LINE_P (&it2))
    {
      if (it2.c == '\t')
	return 0;
      set_iterator_to_next (&it2, 1);
    }

  /* Number of new glyphs produced.  */
  n = it.glyph_row->used[TEXT_AREA];

  /* Start and end of glyphs in original row.  */
  glyphs = glyph_row->glyphs[TEXT_AREA] + w->cursor.hpos;
  end = glyph_row->glyphs[1 + TEXT_AREA];

  /* Make room for new glyphs, then insert them.  */
  xassert (end - glyphs - n >= 0);
  safe_bcopy ((char *) glyphs, (char *) (glyphs + n),
	      (end - glyphs - n) * sizeof (*end));
  bcopy (it.glyph_row->glyphs[TEXT_AREA], glyphs, n * sizeof *glyphs);
  glyph_row->used[TEXT_AREA] = min (glyph_row->used[TEXT_AREA] + n,
				    end - glyph_row->glyphs[TEXT_AREA]);
  
  /* Compute new line width.  */
  glyph = glyph_row->glyphs[TEXT_AREA];
  end = glyph + glyph_row->used[TEXT_AREA];
  glyph_row->pixel_width = glyph_row->x;
  while (glyph < end)
    {
      glyph_row->pixel_width += glyph->pixel_width;
      ++glyph;
    }

  /* Increment buffer positions for glyphs following the newly 
     inserted ones.  */
  for (glyph = glyphs + n; glyph < end; ++glyph)
    if (glyph->charpos > 0 && BUFFERP (glyph->object))
      glyph->charpos += delta;
  
  if (MATRIX_ROW_END_CHARPOS (glyph_row) > 0)
    {
      MATRIX_ROW_END_CHARPOS (glyph_row) += delta;
      MATRIX_ROW_END_BYTEPOS (glyph_row) += delta_bytes;
    }
      
  /* Adjust positions in lines following the one we are in.  */
  increment_matrix_positions (w->current_matrix,
			      w->cursor.vpos + 1,
			      w->current_matrix->nrows,
			      delta, delta_bytes);

  glyph_row->contains_overlapping_glyphs_p
    |= it.glyph_row->contains_overlapping_glyphs_p;

  glyph_row->displays_text_p = 1;
  w->window_end_vpos = make_number (max (w->cursor.vpos,
					 XFASTINT (w->window_end_vpos)));

  if (!NILP (Vshow_trailing_whitespace))
    highlight_trailing_whitespace (it.f, glyph_row);

  /* Write glyphs.  If at end of row, we can simply call write_glyphs.
     In the middle, we have to insert glyphs.  Note that this is now
     implemented for X frames.  The implementation uses updated_window
     and updated_row.  */
  updated_row = glyph_row;
  updated_area = TEXT_AREA;
  update_begin (f);
  if (rif)
    {
      rif->update_window_begin_hook (w);
      
      if (glyphs == end - n
	  /* In front of a space added by append_space.  */
	  || (glyphs == end - n - 1
	      && (end - n)->charpos <= 0))
	rif->write_glyphs (glyphs, n);
      else
	rif->insert_glyphs (glyphs, n);
    }
  else
    {
      if (glyphs == end - n)
	write_glyphs (glyphs, n);
      else
	insert_glyphs (glyphs, n);
    }

  w->cursor.hpos += n;
  w->cursor.x = it.current_x - it.first_visible_x;
  xassert (w->cursor.hpos >= 0
	   && w->cursor.hpos < w->desired_matrix->matrix_w);

  /* How to set the cursor differs depending on whether we are
     using a frame matrix or a window matrix.   Note that when
     a frame matrix is used, cursor_to expects frame coordinates,
     and the X and Y parameters are not used.  */
  if (window_redisplay_p)
    rif->cursor_to (w->cursor.vpos, w->cursor.hpos,
		    w->cursor.y, w->cursor.x);
  else
    {
      int x, y;
      x = (WINDOW_TO_FRAME_HPOS (w, w->cursor.hpos)
	   + (INTEGERP (w->left_margin_width)
	      ? XFASTINT (w->left_margin_width)
	      : 0));
      y = WINDOW_TO_FRAME_VPOS (w, w->cursor.vpos);
      cursor_to (y, x);
    }

  if (rif)
    rif->update_window_end_hook (w, 1, 0);
  update_end (f);
  updated_row = NULL;
  fflush (stdout);

  TRACE ((stderr, "direct output for insert\n"));

  UNCHANGED_MODIFIED = MODIFF;
  BEG_UNCHANGED = GPT - BEG;
  XSETFASTINT (w->last_point, PT);
  w->last_cursor = w->cursor;
  XSETFASTINT (w->last_modified, MODIFF);
  XSETFASTINT (w->last_overlay_modified, OVERLAY_MODIFF);

  redisplay_performed_directly_p = 1;
  return 1;
}


/* Perform a direct display update for moving PT by N positions
   left or right.  N < 0 means a movement backwards.  This function
   is currently only called for N == 1 or N == -1.  */

int
direct_output_forward_char (n)
     int n;
{
  struct frame *f = SELECTED_FRAME ();
  struct window *w = XWINDOW (selected_window);
  struct glyph_row *row;

  /* Give up if point moved out of or into a composition.  */
  if (check_point_in_composition (current_buffer, XINT (w->last_point),
				  current_buffer, PT))
    return 0;

  /* Give up if face attributes have been changed.  */
  if (face_change_count)
    return 0;
  
  /* Give up if current matrix is not up to date or we are 
     displaying a message.  */
  if (!display_completed || cursor_in_echo_area)
    return 0;

  /* Give up if the buffer's direction is reversed.  */
  if (!NILP (XBUFFER (w->buffer)->direction_reversed))
    return 0;

  /* Can't use direct output if highlighting a region.  */
  if (!NILP (Vtransient_mark_mode) && !NILP (current_buffer->mark_active))
    return 0;

  /* Can't use direct output if highlighting trailing whitespace.  */
  if (!NILP (Vshow_trailing_whitespace))
    return 0;

  /* Give up if we are showing a message or just cleared the message
     because we might need to resize the echo area window.  */
  if (!NILP (echo_area_buffer[0]) || !NILP (echo_area_buffer[1]))
    return 0;

  /* Give up if currently displaying a message instead of the
     minibuffer contents.  */
  if (XWINDOW (minibuf_window) == w
      && EQ (minibuf_window, echo_area_window))
    return 0;
  
  /* Give up if we don't know where the cursor is.  */
  if (w->cursor.vpos < 0)
    return 0;

  row = MATRIX_ROW (w->current_matrix, w->cursor.vpos);

  /* Give up if PT is outside of the last known cursor row.  */
  if (PT <= MATRIX_ROW_START_CHARPOS (row)
      || PT >= MATRIX_ROW_END_CHARPOS (row))
    return 0;

  set_cursor_from_row (w, row, w->current_matrix, 0, 0, 0, 0);
  
  w->last_cursor = w->cursor;
  XSETFASTINT (w->last_point, PT);

  xassert (w->cursor.hpos >= 0
	   && w->cursor.hpos < w->desired_matrix->matrix_w);
  
  if (FRAME_WINDOW_P (f))
    rif->cursor_to (w->cursor.vpos, w->cursor.hpos,
		    w->cursor.y, w->cursor.x);
  else
    {
      int x, y;
      x = (WINDOW_TO_FRAME_HPOS (w, w->cursor.hpos)
	   + (INTEGERP (w->left_margin_width)
	      ? XFASTINT (w->left_margin_width)
	      : 0));
      y = WINDOW_TO_FRAME_VPOS (w, w->cursor.vpos);
      cursor_to (y, x);
    }
  
  fflush (stdout);
  redisplay_performed_directly_p = 1;
  return 1;
}



/***********************************************************************
			     Frame Update
 ***********************************************************************/

/* Update frame F based on the data in desired matrices.

   If FORCE_P is non-zero, don't let redisplay be stopped by detecting
   pending input.  If INHIBIT_HAIRY_ID_P is non-zero, don't try
   scrolling.
   
   Value is non-zero if redisplay was stopped due to pending input.  */

int
update_frame (f, force_p, inhibit_hairy_id_p)
     struct frame *f;
     int force_p;
     int inhibit_hairy_id_p;
{
  /* 1 means display has been paused because of pending input.  */
  int paused_p;
  struct window *root_window = XWINDOW (f->root_window);

  if (FRAME_WINDOW_P (f))
    {
      /* We are working on window matrix basis.  All windows whose
	 flag must_be_updated_p is set have to be updated.  */

      /* Record that we are not working on frame matrices.  */
      set_frame_matrix_frame (NULL);

      /* Update all windows in the window tree of F, maybe stopping
	 when pending input is detected.  */
      update_begin (f);

      /* Update the menu bar on X frames that don't have toolkit
	 support.  */
      if (WINDOWP (f->menu_bar_window))
	update_window (XWINDOW (f->menu_bar_window), 1);

      /* Update the tool-bar window, if present.  */
      if (WINDOWP (f->tool_bar_window))
	{
	  struct window *w = XWINDOW (f->tool_bar_window);

	  /* Update tool-bar window.  */
	  if (w->must_be_updated_p)
	    {
	      Lisp_Object tem;

	      update_window (w, 1);
	      w->must_be_updated_p = 0;

	      /* Swap tool-bar strings.  We swap because we want to
		 reuse strings.  */
	      tem = f->current_tool_bar_string;
	      f->current_tool_bar_string = f->desired_tool_bar_string;
	      f->desired_tool_bar_string = tem;
	    }
	}
  

      /* Update windows.  */
      paused_p = update_window_tree (root_window, force_p);
      update_end (f);
      
#if 0 /* This flush is a performance bottleneck under X,
	 and it doesn't seem to be necessary anyway.  */
      rif->flush_display (f);
#endif
    }
  else
    {
      /* We are working on frame matrix basis.  Set the frame on whose
	 frame matrix we operate.  */
      set_frame_matrix_frame (f);

      /* Build F's desired matrix from window matrices.  */
      build_frame_matrix (f);
      
      /* Update the display  */
      update_begin (f);
      paused_p = update_frame_1 (f, force_p, inhibit_hairy_id_p);
      update_end (f);

      if (termscript)
	fflush (termscript);
      fflush (stdout);

      /* Check window matrices for lost pointers.  */
#if GLYPH_DEBUG
      check_window_matrix_pointers (root_window);
      add_frame_display_history (f, paused_p);
#endif
    }

  /* Reset flags indicating that a window should be updated.  */
  set_window_update_flags (root_window, 0);
  
  display_completed = !paused_p;
  return paused_p;
}



/************************************************************************
			 Window-based updates
 ************************************************************************/

/* Perform updates in window tree rooted at W.  FORCE_P non-zero means
   don't stop updating when input is pending.  */

static int
update_window_tree (w, force_p)
     struct window *w;
     int force_p;
{
  int paused_p = 0;
  
  while (w && !paused_p)
    {
      if (!NILP (w->hchild))
	paused_p |= update_window_tree (XWINDOW (w->hchild), force_p);
      else if (!NILP (w->vchild))
	paused_p |= update_window_tree (XWINDOW (w->vchild), force_p);
      else if (w->must_be_updated_p)
	paused_p |= update_window (w, force_p);

      w = NILP (w->next) ? 0 : XWINDOW (w->next);
    }

  return paused_p;
}


/* Update window W if its flag must_be_updated_p is non-zero.  If
   FORCE_P is non-zero, don't stop updating if input is pending.  */

void
update_single_window (w, force_p)
     struct window *w;
     int force_p;
{
  if (w->must_be_updated_p)
    {
      struct frame *f = XFRAME (WINDOW_FRAME (w));

      /* Record that this is not a frame-based redisplay.  */
      set_frame_matrix_frame (NULL);

      /* Update W.  */
      update_begin (f);
      update_window (w, force_p);
      update_end (f);

      /* Reset flag in W.  */
      w->must_be_updated_p = 0;
    }
}


/* Redraw lines from the current matrix of window W that are
   overlapped by other rows.  YB is bottom-most y-position in W.  */

static void
redraw_overlapped_rows (w, yb)
     struct window *w;
     int yb;
{
  int i;
  
  /* If rows overlapping others have been changed, the rows being
     overlapped have to be redrawn.  This won't draw lines that have
     already been drawn in update_window_line because overlapped_p in
     desired rows is 0, so after row assignment overlapped_p in
     current rows is 0.  */
  for (i = 0; i < w->current_matrix->nrows; ++i)
    {
      struct glyph_row *row = w->current_matrix->rows + i;

      if (!row->enabled_p)
	break;
      else if (row->mode_line_p)
	continue;
      
      if (row->overlapped_p)
	{
	  enum glyph_row_area area;
	  
	  for (area = LEFT_MARGIN_AREA; area < LAST_AREA; ++area)
	    {
	      updated_row = row;
	      updated_area = area;
	      rif->cursor_to (i, 0, row->y, area == TEXT_AREA ? row->x : 0);
	      if (row->used[area])
		rif->write_glyphs (row->glyphs[area], row->used[area]);
	      rif->clear_end_of_line (-1);
	    }
	  
	  row->overlapped_p = 0;
	}

      if (MATRIX_ROW_BOTTOM_Y (row) >= yb)
	break;
    }
}


/* Redraw lines from the current matrix of window W that overlap
   others.  YB is bottom-most y-position in W.  */

static void
redraw_overlapping_rows (w, yb)
     struct window *w;
     int yb;
{
  int i, bottom_y;
  struct glyph_row *row;
  
  for (i = 0; i < w->current_matrix->nrows; ++i)
    {
      row = w->current_matrix->rows + i;

      if (!row->enabled_p)
	break;
      else if (row->mode_line_p)
	continue;
      
      bottom_y = MATRIX_ROW_BOTTOM_Y (row);

      if (row->overlapping_p && i > 0 && bottom_y < yb)
	{
	  if (row->used[LEFT_MARGIN_AREA])
	    rif->fix_overlapping_area (w, row, LEFT_MARGIN_AREA);
  
	  if (row->used[TEXT_AREA])
	    rif->fix_overlapping_area (w, row, TEXT_AREA);
  
	  if (row->used[RIGHT_MARGIN_AREA])
	    rif->fix_overlapping_area (w, row, RIGHT_MARGIN_AREA);

	  /* Record in neighbour rows that ROW overwrites part of their
	     display.  */
	  if (row->phys_ascent > row->ascent && i > 0)
	    MATRIX_ROW (w->current_matrix, i - 1)->overlapped_p = 1;
	  if ((row->phys_height - row->phys_ascent
	       > row->height - row->ascent)
	      && bottom_y < yb)
	    MATRIX_ROW (w->current_matrix, i + 1)->overlapped_p = 1;
	}

      if (bottom_y >= yb)
	break;
    }
}


#ifdef GLYPH_DEBUG

/* Check that no row in the current matrix of window W is enabled
   which is below what's displayed in the window.  */

void
check_current_matrix_flags (w)
     struct window *w;
{
  int last_seen_p = 0;
  int i, yb = window_text_bottom_y (w);

  for (i = 0; i < w->current_matrix->nrows - 1; ++i)
    {
      struct glyph_row *row = MATRIX_ROW (w->current_matrix, i);
      if (!last_seen_p && MATRIX_ROW_BOTTOM_Y (row) >= yb)
	last_seen_p = 1;
      else if (last_seen_p && row->enabled_p)
	abort ();
    }
}

#endif /* GLYPH_DEBUG */


/* Update display of window W.  FORCE_P non-zero means that we should
   not stop when detecting pending input.  */

static int
update_window (w, force_p)
     struct window *w;
     int force_p;
{
  struct glyph_matrix *desired_matrix = w->desired_matrix;
  int paused_p;
  int preempt_count = baud_rate / 2400 + 1;
  extern int input_pending;
  extern Lisp_Object do_mouse_tracking;
#if GLYPH_DEBUG
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  extern struct frame *updating_frame;
#endif

  /* Check that W's frame doesn't have glyph matrices.  */
  xassert (FRAME_WINDOW_P (f));
  xassert (updating_frame != NULL);

  /* Check pending input the first time so that we can quickly return.  */
  if (redisplay_dont_pause)
    force_p = 1;
  else
    detect_input_pending ();

  /* If forced to complete the update, or if no input is pending, do
     the update.  */
  if (force_p || !input_pending || !NILP (do_mouse_tracking))
    {
      struct glyph_row *row, *end;
      struct glyph_row *mode_line_row;
      struct glyph_row *header_line_row;
      int yb, changed_p = 0, mouse_face_overwritten_p = 0, n_updated;

      rif->update_window_begin_hook (w);
      yb = window_text_bottom_y (w);

      /* If window has a header line, update it before everything else.
	 Adjust y-positions of other rows by the header line height.  */
      row = desired_matrix->rows;
      end = row + desired_matrix->nrows - 1;
      
      if (row->mode_line_p)
	{
	  header_line_row = row;
	  ++row;
	}
      else
	header_line_row = NULL;

      /* Update the mode line, if necessary.  */
      mode_line_row = MATRIX_MODE_LINE_ROW (desired_matrix);
      if (mode_line_row->mode_line_p && mode_line_row->enabled_p)
	{
	  mode_line_row->y = yb;
	  update_window_line (w, MATRIX_ROW_VPOS (mode_line_row,
						  desired_matrix),
			      &mouse_face_overwritten_p);
	  changed_p = 1;
	}

      /* Find first enabled row.  Optimizations in redisplay_internal
	 may lead to an update with only one row enabled.  There may
	 be also completely empty matrices.  */
      while (row < end && !row->enabled_p)
	++row;
      
      /* Try reusing part of the display by copying.  */
      if (row < end && !desired_matrix->no_scrolling_p)
	{
	  int rc = scrolling_window (w, header_line_row != NULL);
	  if (rc < 0)
	    {
	      /* All rows were found to be equal.  */
	      paused_p = 0;
	      goto set_cursor;
	    }
	  else if (rc > 0)
	    /* We've scrolled the display.  */
	    force_p = 1;
	  changed_p = 1;
	}

      /* Update the header line after scrolling because a new header
	 line would otherwise overwrite lines at the top of the window
	 that can be scrolled.  */
      if (header_line_row && header_line_row->enabled_p)
	{
	  header_line_row->y = 0;
	  update_window_line (w, 0, &mouse_face_overwritten_p);
	  changed_p = 1;
	}

      /* Update the rest of the lines.  */
      for (n_updated = 0; row < end && (force_p || !input_pending); ++row)
	if (row->enabled_p)
	  {
	    int vpos = MATRIX_ROW_VPOS (row, desired_matrix);
	    int i;
	    
	    /* We'll have to play a little bit with when to
	       detect_input_pending.  If it's done too often,
	       scrolling large windows with repeated scroll-up
	       commands will too quickly pause redisplay.  */
	    if (!force_p && ++n_updated % preempt_count == 0)
	      detect_input_pending ();

	    changed_p |= update_window_line (w, vpos,
					     &mouse_face_overwritten_p);

	    /* Mark all rows below the last visible one in the current
	       matrix as invalid.  This is necessary because of
	       variable line heights.  Consider the case of three
	       successive redisplays, where the first displays 5
	       lines, the second 3 lines, and the third 5 lines again.
	       If the second redisplay wouldn't mark rows in the
	       current matrix invalid, the third redisplay might be
	       tempted to optimize redisplay based on lines displayed
	       in the first redisplay.  */
	    if (MATRIX_ROW_BOTTOM_Y (row) >= yb)
	      for (i = vpos + 1; i < w->current_matrix->nrows - 1; ++i)
		MATRIX_ROW (w->current_matrix, i)->enabled_p = 0;
	  }

      /* Was display preempted?  */
      paused_p = row < end;
      
    set_cursor:
      
      /* Fix the appearance of overlapping/overlapped rows.  */
      if (!paused_p && !w->pseudo_window_p)
	{
	  if (changed_p && rif->fix_overlapping_area)
	    {
	      redraw_overlapped_rows (w, yb);
	      redraw_overlapping_rows (w, yb);
	    }
      
	  /* Make cursor visible at cursor position of W.  */
	  set_window_cursor_after_update (w);

#if 0 /* Check that current matrix invariants are satisfied.  This is
	 for debugging only.  See the comment of check_matrix_invariants.  */
	  IF_DEBUG (check_matrix_invariants (w));
#endif
	}

#if GLYPH_DEBUG
      /* Remember the redisplay method used to display the matrix.  */
      strcpy (w->current_matrix->method, w->desired_matrix->method);
#endif

      /* End the update of window W.  Don't set the cursor if we
         paused updating the display because in this case,
         set_window_cursor_after_update hasn't been called, and
         output_cursor doesn't contain the cursor location.  */
      rif->update_window_end_hook (w, !paused_p, mouse_face_overwritten_p);
    }
  else
    paused_p = 1;

#if GLYPH_DEBUG
  /* check_current_matrix_flags (w); */
  add_window_display_history (w, w->current_matrix->method, paused_p);
#endif
  
  clear_glyph_matrix (desired_matrix);

  return paused_p;
}


/* Update the display of area AREA in window W, row number VPOS.
   AREA can be either LEFT_MARGIN_AREA or RIGHT_MARGIN_AREA.  */

static void
update_marginal_area (w, area, vpos)
     struct window *w;
     int area, vpos;
{
  struct glyph_row *desired_row = MATRIX_ROW (w->desired_matrix, vpos);

  /* Let functions in xterm.c know what area subsequent X positions
     will be relative to.  */
  updated_area = area;

  /* Set cursor to start of glyphs, write them, and clear to the end
     of the area.  I don't think that something more sophisticated is
     necessary here, since marginal areas will not be the default.  */
  rif->cursor_to (vpos, 0, desired_row->y, 0);
  if (desired_row->used[area])
    rif->write_glyphs (desired_row->glyphs[area], desired_row->used[area]);
  rif->clear_end_of_line (-1);
}


/* Update the display of the text area of row VPOS in window W.
   Value is non-zero if display has changed.  */

static int
update_text_area (w, vpos)
     struct window *w;
     int vpos;
{
  struct glyph_row *current_row = MATRIX_ROW (w->current_matrix, vpos);
  struct glyph_row *desired_row = MATRIX_ROW (w->desired_matrix, vpos);
  int changed_p = 0;

  /* Let functions in xterm.c know what area subsequent X positions
     will be relative to.  */
  updated_area = TEXT_AREA;
  
  /* If rows are at different X or Y, or rows have different height,
     or the current row is marked invalid, write the entire line.  */
  if (!current_row->enabled_p
      || desired_row->y != current_row->y
      || desired_row->ascent != current_row->ascent
      || desired_row->phys_ascent != current_row->phys_ascent
      || desired_row->phys_height != current_row->phys_height
      || desired_row->visible_height != current_row->visible_height
      || current_row->overlapped_p
      || current_row->mouse_face_p
      || current_row->x != desired_row->x)
    {
      rif->cursor_to (vpos, 0, desired_row->y, desired_row->x);
      
      if (desired_row->used[TEXT_AREA])
	rif->write_glyphs (desired_row->glyphs[TEXT_AREA],
			   desired_row->used[TEXT_AREA]);
      
      /* Clear to end of window.  */
      rif->clear_end_of_line (-1);
      changed_p = 1;
    }
  else
    {
      int stop, i, x;
      struct glyph *current_glyph = current_row->glyphs[TEXT_AREA];
      struct glyph *desired_glyph = desired_row->glyphs[TEXT_AREA];
      int overlapping_glyphs_p = current_row->contains_overlapping_glyphs_p;
      int desired_stop_pos = desired_row->used[TEXT_AREA];

      /* If the desired row extends its face to the text area end,
	 make sure we write at least one glyph, so that the face
	 extension actually takes place.  */
      if (MATRIX_ROW_EXTENDS_FACE_P (desired_row))
	--desired_stop_pos;
      
      stop = min (current_row->used[TEXT_AREA], desired_stop_pos);
      i = 0;
      x = desired_row->x;

      /* Loop over glyphs that current and desired row may have
	 in common.  */
      while (i < stop)
	{
	  int can_skip_p = 1;
	  
	  /* Skip over glyphs that both rows have in common.  These
	     don't have to be written.  We can't skip if the last
	     current glyph overlaps the glyph to its right.  For
	     example, consider a current row of `if ' with the `f' in
	     Courier bold so that it overlaps the ` ' to its right.
	     If the desired row is ` ', we would skip over the space
	     after the `if' and there would remain a pixel from the
	     `f' on the screen.  */
	  if (overlapping_glyphs_p && i > 0)
	    {
	      struct glyph *glyph = &current_row->glyphs[TEXT_AREA][i - 1];
	      int left, right;
	      
	      rif->get_glyph_overhangs (glyph, XFRAME (w->frame),
					&left, &right);
	      can_skip_p = right == 0;
	    }
	  
	  if (can_skip_p)
	    {
	      while (i < stop
		     && GLYPH_EQUAL_P (desired_glyph, current_glyph))
		{
		  x += desired_glyph->pixel_width;
		  ++desired_glyph, ++current_glyph, ++i;
		}

	      /* Consider the case that the current row contains "xxx
		 ppp ggg" in italic Courier font, and the desired row
		 is "xxx ggg".  The character `p' has lbearing, `g'
		 has not.  The loop above will stop in front of the
		 first `p' in the current row.  If we would start
		 writing glyphs there, we wouldn't erase the lbearing
		 of the `p'.  The rest of the lbearing problem is then
		 taken care of by x_draw_glyphs.  */
	      if (overlapping_glyphs_p
		  && i > 0
		  && i < current_row->used[TEXT_AREA]
		  && (current_row->used[TEXT_AREA]
		      != desired_row->used[TEXT_AREA]))
		{
		  int left, right;
	      
		  rif->get_glyph_overhangs (current_glyph, XFRAME (w->frame),
					    &left, &right);
		  while (left > 0 && i > 0)
		    {
		      --i, --desired_glyph, --current_glyph;
		      x -= desired_glyph->pixel_width;
		      left -= desired_glyph->pixel_width;
		    }
		}
	    }
	  
	  /* Try to avoid writing the entire rest of the desired row
	     by looking for a resync point.  This mainly prevents
	     mode line flickering in the case the mode line is in
	     fixed-pitch font, which it usually will be.  */
	  if (i < desired_row->used[TEXT_AREA])
	    {
	      int start_x = x, start_hpos = i;
	      struct glyph *start = desired_glyph;
	      int current_x = x;
	      int skip_first_p = !can_skip_p;

	      /* Find the next glyph that's equal again.  */
	      while (i < stop
		     && (skip_first_p
			 || !GLYPH_EQUAL_P (desired_glyph, current_glyph))
		     && x == current_x)
		{
		  x += desired_glyph->pixel_width;
		  current_x += current_glyph->pixel_width;
		  ++desired_glyph, ++current_glyph, ++i;
		  skip_first_p = 0;
		}

	      if (i == start_hpos || x != current_x)
		{
		  i = start_hpos;
		  x = start_x;
		  desired_glyph = start;
		  break;
		}

	      rif->cursor_to (vpos, start_hpos, desired_row->y, start_x);
	      rif->write_glyphs (start, i - start_hpos);
	      changed_p = 1;
	    }
	}
      
      /* Write the rest.  */
      if (i < desired_row->used[TEXT_AREA])
	{
	  rif->cursor_to (vpos, i, desired_row->y, x);
	  rif->write_glyphs (desired_glyph, desired_row->used[TEXT_AREA] - i);
	  changed_p = 1;
	}
      
      /* Maybe clear to end of line.  */
      if (MATRIX_ROW_EXTENDS_FACE_P (desired_row))
	{
	  /* If new row extends to the end of the text area, nothing
	     has to be cleared, if and only if we did a write_glyphs
	     above.  This is made sure by setting desired_stop_pos
	     appropriately above.  */
	  xassert (i < desired_row->used[TEXT_AREA]);
	}
      else if (MATRIX_ROW_EXTENDS_FACE_P (current_row))
	{
	  /* If old row extends to the end of the text area, clear.  */
	  if (i >= desired_row->used[TEXT_AREA])
	    rif->cursor_to (vpos, i, desired_row->y,
			    desired_row->x + desired_row->pixel_width);
	  rif->clear_end_of_line (-1);
	  changed_p = 1;
	}
      else if (desired_row->pixel_width < current_row->pixel_width)
	{
	  /* Otherwise clear to the end of the old row.  Everything
	     after that position should be clear already.  */
	  int x;
	  
	  if (i >= desired_row->used[TEXT_AREA])
	    rif->cursor_to (vpos, i, desired_row->y,
			    desired_row->x + desired_row->pixel_width);

	  /* If cursor is displayed at the end of the line, make sure
	     it's cleared.  Nowadays we don't have a phys_cursor_glyph
	     with which to erase the cursor (because this method
	     doesn't work with lbearing/rbearing), so we must do it
	     this way.  */
	  if (vpos == w->phys_cursor.vpos
	      && w->phys_cursor.hpos >= desired_row->used[TEXT_AREA])
	    {
	      w->phys_cursor_on_p = 0;
	      x = -1;
	    }
	  else
	    x = current_row->x + current_row->pixel_width;
	  rif->clear_end_of_line (x);
	  changed_p = 1;
	}
    }

  return changed_p;
}


/* Update row VPOS in window W.  Value is non-zero if display has been
   changed.  */

static int
update_window_line (w, vpos, mouse_face_overwritten_p)
     struct window *w;
     int vpos, *mouse_face_overwritten_p;
{
  struct glyph_row *current_row = MATRIX_ROW (w->current_matrix, vpos);
  struct glyph_row *desired_row = MATRIX_ROW (w->desired_matrix, vpos);
  int changed_p = 0;

  /* Set the row being updated.  This is important to let xterm.c
     know what line height values are in effect.  */
  updated_row = desired_row;

  /* A row can be completely invisible in case a desired matrix was 
     built with a vscroll and then make_cursor_line_fully_visible shifts 
     the matrix.  Make sure to make such rows current anyway, since
     we need the correct y-position, for example, in the current matrix.  */
  if (desired_row->mode_line_p
      || desired_row->visible_height > 0)
    {
      xassert (desired_row->enabled_p);

      /* Update display of the left margin area, if there is one.  */
      if (!desired_row->full_width_p
	  && !NILP (w->left_margin_width))
	{
	  changed_p = 1;
	  update_marginal_area (w, LEFT_MARGIN_AREA, vpos);
	}
      
      /* Update the display of the text area.  */
      if (update_text_area (w, vpos))
	{
	  changed_p = 1;
	  if (current_row->mouse_face_p)
	    *mouse_face_overwritten_p = 1;
	}
      
      /* Update display of the right margin area, if there is one.  */
      if (!desired_row->full_width_p
	  && !NILP (w->right_margin_width))
	{
	  changed_p = 1;
	  update_marginal_area (w, RIGHT_MARGIN_AREA, vpos);
	}
      
      /* Draw truncation marks etc.  */
      if (!current_row->enabled_p
	  || desired_row->y != current_row->y
	  || desired_row->visible_height != current_row->visible_height
	  || desired_row->overlay_arrow_p != current_row->overlay_arrow_p
	  || desired_row->truncated_on_left_p != current_row->truncated_on_left_p
	  || desired_row->truncated_on_right_p != current_row->truncated_on_right_p
	  || desired_row->continued_p != current_row->continued_p
	  || desired_row->mode_line_p != current_row->mode_line_p
	  || (desired_row->indicate_empty_line_p
	      != current_row->indicate_empty_line_p)
	  || (MATRIX_ROW_CONTINUATION_LINE_P (desired_row)
	      != MATRIX_ROW_CONTINUATION_LINE_P (current_row)))
	rif->after_update_window_line_hook (desired_row);
    }
  
  /* Update current_row from desired_row.  */
  make_current (w->desired_matrix, w->current_matrix, vpos);
  updated_row = NULL;
  return changed_p;
}


/* Set the cursor after an update of window W.  This function may only
   be called from update_window.  */

static void
set_window_cursor_after_update (w)
     struct window *w;
{
  struct frame *f = XFRAME (w->frame);
  int cx, cy, vpos, hpos;

  /* Not intended for frame matrix updates.  */
  xassert (FRAME_WINDOW_P (f));
  
  if (cursor_in_echo_area
      && !NILP (echo_area_buffer[0])
      /* If we are showing a message instead of the mini-buffer,
	 show the cursor for the message instead.  */
      && XWINDOW (minibuf_window) == w
      && EQ (minibuf_window, echo_area_window)
      /* These cases apply only to the frame that contains
	 the active mini-buffer window.  */
      && FRAME_HAS_MINIBUF_P (f)
      && EQ (FRAME_MINIBUF_WINDOW (f), echo_area_window))
    {
      cx = cy = vpos = hpos = 0;

      if (cursor_in_echo_area >= 0)
	{
	  /* If the mini-buffer is several lines high, find the last
	     line that has any text on it.  Note: either all lines
	     are enabled or none.  Otherwise we wouldn't be able to
	     determine Y.  */
	  struct glyph_row *row, *last_row;
	  struct glyph *glyph;
	  int yb = window_text_bottom_y (w);

	  last_row = NULL;
	  row = w->current_matrix->rows;
	  while (row->enabled_p
		 && (last_row == NULL
		     || MATRIX_ROW_BOTTOM_Y (row) <= yb))
	    {
	      if (row->used[TEXT_AREA]
		  && row->glyphs[TEXT_AREA][0].charpos >= 0)
		last_row = row;
	      ++row;
	    }
	  
	  if (last_row)
	    {
	      struct glyph *start = last_row->glyphs[TEXT_AREA];
	      struct glyph *last = start + last_row->used[TEXT_AREA] - 1;

	      while (last > start && last->charpos < 0)
		--last;
	      
	      for (glyph = start; glyph < last; ++glyph)
		{
		  cx += glyph->pixel_width;
		  ++hpos;
		}

	      cy = last_row->y;
	      vpos = MATRIX_ROW_VPOS (last_row, w->current_matrix);
	    }
	}
    }
  else
    {
      cx = w->cursor.x;
      cy = w->cursor.y;
      hpos = w->cursor.hpos;
      vpos = w->cursor.vpos;
    }

  /* Window cursor can be out of sync for horizontally split windows.  */
  hpos = max (0, hpos);
  hpos = min (w->current_matrix->matrix_w - 1, hpos);
  vpos = max (0, vpos);
  vpos = min (w->current_matrix->nrows - 1, vpos);
  rif->cursor_to (vpos, hpos, cy, cx);
}


/* Set WINDOW->must_be_updated_p to ON_P for all windows in the window
   tree rooted at W.  */

void
set_window_update_flags (w, on_p)
     struct window *w;
     int on_p;
{
  while (w)
    {
      if (!NILP (w->hchild))
	set_window_update_flags (XWINDOW (w->hchild), on_p);
      else if (!NILP (w->vchild))
	set_window_update_flags (XWINDOW (w->vchild), on_p);
      else
	w->must_be_updated_p = on_p;

      w = NILP (w->next) ? 0 : XWINDOW (w->next);
    }
}    



/***********************************************************************
			Window-Based Scrolling
 ***********************************************************************/

/* Structure describing rows in scrolling_window.  */

struct row_entry
{
  /* Number of occurrences of this row in desired and current matrix.  */
  int old_uses, new_uses;
    
  /* Vpos of row in new matrix.  */
  int new_line_number;

  /* Bucket index of this row_entry in the hash table row_table.  */
  int bucket;
    
  /* The row described by this entry.  */
  struct glyph_row *row;
    
  /* Hash collision chain.  */
  struct row_entry *next;
};

/* A pool to allocate row_entry structures from, and the size of the
   pool.  The pool is reallocated in scrolling_window when we find
   that we need a larger one.  */

static struct row_entry *row_entry_pool;
static int row_entry_pool_size;

/* Index of next free entry in row_entry_pool.  */

static int row_entry_idx;

/* The hash table used during scrolling, and the table's size.  This
   table is used to quickly identify equal rows in the desired and
   current matrix.  */

static struct row_entry **row_table;
static int row_table_size;

/* Vectors of pointers to row_entry structures belonging to the
   current and desired matrix, and the size of the vectors.  */

static struct row_entry **old_lines, **new_lines;
static int old_lines_size, new_lines_size;

/* A pool to allocate run structures from, and its size.  */

static struct run *run_pool;
static int runs_size;

/* A vector of runs of lines found during scrolling.  */

static struct run **runs;

/* Add glyph row ROW to the scrolling hash table during the scrolling
   of window W.  */

static INLINE struct row_entry *
add_row_entry (w, row)
     struct window *w;
     struct glyph_row *row;
{
  struct row_entry *entry;
  int i = row->hash % row_table_size;
  
  entry = row_table[i];
  while (entry && !row_equal_p (w, entry->row, row, 1))
    entry = entry->next;
  
  if (entry == NULL)
    {
      entry = row_entry_pool + row_entry_idx++;
      entry->row = row;
      entry->old_uses = entry->new_uses = 0;
      entry->new_line_number = 0;
      entry->bucket = i;
      entry->next = row_table[i];
      row_table[i] = entry;
    }

  return entry;
}


/* Try to reuse part of the current display of W by scrolling lines.
   HEADER_LINE_P non-zero means W has a header line.

   The algorithm is taken from Communications of the ACM, Apr78 "A
   Technique for Isolating Differences Between Files."  It should take
   O(N) time.

   A short outline of the steps of the algorithm

   1. Skip lines equal at the start and end of both matrices.

   2. Enter rows in the current and desired matrix into a symbol
   table, counting how often they appear in both matrices.

   3. Rows that appear exactly once in both matrices serve as anchors,
   i.e. we assume that such lines are likely to have been moved.

   4. Starting from anchor lines, extend regions to be scrolled both
   forward and backward.

   Value is

   -1	if all rows were found to be equal.
   0	to indicate that we did not scroll the display, or
   1	if we did scroll.  */

static int
scrolling_window (w, header_line_p)
     struct window *w;
     int header_line_p;
{
  struct glyph_matrix *desired_matrix = w->desired_matrix;
  struct glyph_matrix *current_matrix = w->current_matrix;
  int yb = window_text_bottom_y (w);
  int i, j, first_old, first_new, last_old, last_new;
  int nruns, nbytes, n, run_idx;
  struct row_entry *entry;

  /* Skip over rows equal at the start.  */
  for (i = header_line_p ? 1 : 0; i < current_matrix->nrows - 1; ++i)
    {
      struct glyph_row *d = MATRIX_ROW (desired_matrix, i);
      struct glyph_row *c = MATRIX_ROW (current_matrix, i);

      if (c->enabled_p
	  && d->enabled_p
	  && c->y == d->y
	  && MATRIX_ROW_BOTTOM_Y (c) <= yb
	  && MATRIX_ROW_BOTTOM_Y (d) <= yb
	  && row_equal_p (w, c, d, 1))
	{
	  assign_row (c, d);
	  d->enabled_p = 0;
	}
      else
	break;
    }

  /* Give up if some rows in the desired matrix are not enabled.  */
  if (!MATRIX_ROW (desired_matrix, i)->enabled_p)
    return -1;
  
  first_old = first_new = i;

  /* Set last_new to the index + 1 of the last enabled row in the
     desired matrix.  */
  i = first_new + 1;
  while (i < desired_matrix->nrows - 1
	 && MATRIX_ROW (desired_matrix, i)->enabled_p
	 && MATRIX_ROW_BOTTOM_Y (MATRIX_ROW (desired_matrix, i)) <= yb)
    ++i;

  if (!MATRIX_ROW (desired_matrix, i)->enabled_p)
    return 0;

  last_new = i;

  /* Set last_old to the index + 1 of the last enabled row in the
     current matrix.  We don't look at the enabled flag here because
     we plan to reuse part of the display even if other parts are
     disabled.  */
  i = first_old + 1;
  while (i < current_matrix->nrows - 1)
    {
      int bottom = MATRIX_ROW_BOTTOM_Y (MATRIX_ROW (current_matrix, i));
      if (bottom <= yb)
	++i;
      if (bottom >= yb)
	break;
    }

  last_old = i;

  /* Skip over rows equal at the bottom.  */
  i = last_new;
  j = last_old;
  while (i - 1 > first_new
         && j - 1 > first_old
         && MATRIX_ROW (current_matrix, i - 1)->enabled_p
	 && (MATRIX_ROW (current_matrix, i - 1)->y
	     == MATRIX_ROW (desired_matrix, j - 1)->y)
         && row_equal_p (w,
			 MATRIX_ROW (desired_matrix, i - 1),
                         MATRIX_ROW (current_matrix, j - 1), 1))
    --i, --j;
  last_new = i;
  last_old = j;

  /* Nothing to do if all rows are equal.  */
  if (last_new == first_new)
    return 0;

  /* Reallocate vectors, tables etc. if necessary.  */
  
  if (current_matrix->nrows > old_lines_size)
    {
      old_lines_size = current_matrix->nrows;
      nbytes = old_lines_size * sizeof *old_lines;
      old_lines = (struct row_entry **) xrealloc (old_lines, nbytes);
    }
  
  if (desired_matrix->nrows > new_lines_size)
    {
      new_lines_size = desired_matrix->nrows;
      nbytes = new_lines_size * sizeof *new_lines;
      new_lines = (struct row_entry **) xrealloc (new_lines, nbytes);
    }

  n = desired_matrix->nrows + current_matrix->nrows;
  if (3 * n > row_table_size)
    {
      row_table_size = next_almost_prime (3 * n);
      nbytes = row_table_size * sizeof *row_table;
      row_table = (struct row_entry **) xrealloc (row_table, nbytes);
      bzero (row_table, nbytes);
    }

  if (n > row_entry_pool_size)
    {
      row_entry_pool_size = n;
      nbytes = row_entry_pool_size * sizeof *row_entry_pool;
      row_entry_pool = (struct row_entry *) xrealloc (row_entry_pool, nbytes);
    }

  if (desired_matrix->nrows > runs_size)
    {
      runs_size = desired_matrix->nrows;
      nbytes = runs_size * sizeof *runs;
      runs = (struct run **) xrealloc (runs, nbytes);
      nbytes = runs_size * sizeof *run_pool;
      run_pool = (struct run *) xrealloc (run_pool, nbytes);
    }

  nruns = run_idx = 0;
  row_entry_idx = 0;

  /* Add rows from the current and desired matrix to the hash table
     row_hash_table to be able to find equal ones quickly.  */
  
  for (i = first_old; i < last_old; ++i)
    {
      if (MATRIX_ROW (current_matrix, i)->enabled_p)
	{
	  entry = add_row_entry (w, MATRIX_ROW (current_matrix, i));
	  old_lines[i] = entry;
	  ++entry->old_uses;
	}
      else
	old_lines[i] = NULL;
    }

  for (i = first_new; i < last_new; ++i)
    {
      xassert (MATRIX_ROW_ENABLED_P (desired_matrix, i));
      entry = add_row_entry (w, MATRIX_ROW (desired_matrix, i));
      ++entry->new_uses;
      entry->new_line_number = i;
      new_lines[i] = entry;
    }

  /* Identify moves based on lines that are unique and equal
     in both matrices.  */
  for (i = first_old; i < last_old;)
    if (old_lines[i]
	&& old_lines[i]->old_uses == 1
        && old_lines[i]->new_uses == 1)
      {
	int j, k;
	int new_line = old_lines[i]->new_line_number;
	struct run *run = run_pool + run_idx++;

	/* Record move.  */
	run->current_vpos = i;
	run->current_y = MATRIX_ROW (current_matrix, i)->y;
	run->desired_vpos = new_line;
	run->desired_y = MATRIX_ROW (desired_matrix, new_line)->y;
	run->nrows = 1;
	run->height = MATRIX_ROW (current_matrix, i)->height;

	/* Extend backward.  */
	j = i - 1;
	k = new_line - 1;
	while (j > first_old
	       && k > first_new
	       && old_lines[j] == new_lines[k])
	  {
	    int h = MATRIX_ROW (current_matrix, j)->height;
	    --run->current_vpos; 
	    --run->desired_vpos; 
	    ++run->nrows;
	    run->height += h;
	    run->desired_y -= h;
	    run->current_y -= h;
	    --j, --k;
	  }

	/* Extend forward.  */
	j = i + 1;
	k = new_line + 1;
	while (j < last_old
	       && k < last_new
	       && old_lines[j] == new_lines[k])
	  {
	    int h = MATRIX_ROW (current_matrix, j)->height;
	    ++run->nrows; 
	    run->height += h;
	    ++j, ++k;
	  }

	/* Insert run into list of all runs.  Order runs by copied
	   pixel lines.  Note that we record runs that don't have to
	   be copied because they are already in place.  This is done
	   because we can avoid calling update_window_line in this
	   case.  */
	for (j = 0; j < nruns && runs[j]->height > run->height; ++j)
	  ;
	for (k = nruns; k > j; --k)
	  runs[k] = runs[k - 1];
	runs[j] = run;
	++nruns;

	i += run->nrows;
      }
    else
      ++i;

  /* Do the moves.  Do it in a way that we don't overwrite something
     we want to copy later on.  This is not solvable in general
     because there is only one display and we don't have a way to
     exchange areas on this display.  Example:

          +-----------+       +-----------+
          |     A     |       |     B     |
          +-----------+  -->  +-----------+
          |     B     |       |     A     |
          +-----------+       +-----------+

     Instead, prefer bigger moves, and invalidate moves that would
     copy from where we copied to.  */

  for (i = 0; i < nruns; ++i)
    if (runs[i]->nrows > 0)
      {
	struct run *r = runs[i];

	/* Copy on the display.  */
	if (r->current_y != r->desired_y)
	  {
	    rif->scroll_run_hook (w, r);

	    /* Invalidate runs that copy from where we copied to.  */
	    for (j = i + 1; j < nruns; ++j)
	      {
		struct run *p = runs[j];
		
		if ((p->current_y >= r->desired_y
		     && p->current_y < r->desired_y + r->height)
		    || (p->current_y + p->height >= r->desired_y
			&& (p->current_y + p->height
			    < r->desired_y + r->height)))
		  p->nrows = 0;
	      }
	  }

	/* Assign matrix rows.  */
	for (j = 0; j < r->nrows; ++j)
	  {
	    struct glyph_row *from, *to;
	    int to_overlapped_p;

	    to = MATRIX_ROW (current_matrix, r->desired_vpos + j);
	    from = MATRIX_ROW (desired_matrix, r->desired_vpos + j);
	    to_overlapped_p = to->overlapped_p;
	    assign_row (to, from);
	    to->enabled_p = 1, from->enabled_p = 0;
	    to->overlapped_p = to_overlapped_p;
	  }
      }

  /* Clear the hash table, for the next time.  */
  for (i = 0; i < row_entry_idx; ++i)
    row_table[row_entry_pool[i].bucket] = NULL;

  /* Value is non-zero to indicate that we scrolled the display.  */
  return 1;
}



/************************************************************************
			 Frame-Based Updates
 ************************************************************************/

/* Update the desired frame matrix of frame F.

   FORCE_P non-zero means that the update should not be stopped by
   pending input.  INHIBIT_HAIRY_ID_P non-zero means that scrolling
   should not be tried.

   Value is non-zero if update was stopped due to pending input.  */

static int
update_frame_1 (f, force_p, inhibit_id_p)
     struct frame *f;
     int force_p;
     int inhibit_id_p;
{
  /* Frame matrices to work on.  */
  struct glyph_matrix *current_matrix = f->current_matrix;
  struct glyph_matrix *desired_matrix = f->desired_matrix;
  int i;
  int pause;
  int preempt_count = baud_rate / 2400 + 1;
  extern int input_pending;

  xassert (current_matrix && desired_matrix);

  if (baud_rate != FRAME_COST_BAUD_RATE (f))
    calculate_costs (f);

  if (preempt_count <= 0)
    preempt_count = 1;

  if (redisplay_dont_pause)
    force_p = 1;
  else if (!force_p && detect_input_pending ())
    {
      pause = 1;
      goto do_pause;
    }

  /* If we cannot insert/delete lines, it's no use trying it.  */
  if (!line_ins_del_ok)
    inhibit_id_p = 1;

  /* See if any of the desired lines are enabled; don't compute for
     i/d line if just want cursor motion.  */
  for (i = 0; i < desired_matrix->nrows; i++)
    if (MATRIX_ROW_ENABLED_P (desired_matrix, i))
      break;

  /* Try doing i/d line, if not yet inhibited.  */
  if (!inhibit_id_p && i < desired_matrix->nrows)
    force_p |= scrolling (f);

  /* Update the individual lines as needed.  Do bottom line first.  */
  if (MATRIX_ROW_ENABLED_P (desired_matrix, desired_matrix->nrows - 1))
    update_frame_line (f, desired_matrix->nrows - 1);

  /* Now update the rest of the lines.  */
  for (i = 0; i < desired_matrix->nrows - 1 && (force_p || !input_pending); i++)
    {
      if (MATRIX_ROW_ENABLED_P (desired_matrix, i))
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
			   the outq count.  */
			outq = PENDING_OUTPUT_COUNT (stdout);
#endif
		      outq *= 10;
		      if (baud_rate <= outq && baud_rate > 0)
			sleep (outq / baud_rate);
		    }
		}
	    }

	  if ((i - 1) % preempt_count == 0)
	    detect_input_pending ();

	  update_frame_line (f, i);
	}
    }
  
  pause = (i < FRAME_HEIGHT (f) - 1) ? i : 0;

  /* Now just clean up termcap drivers and set cursor, etc.  */
  if (!pause)
    {
      if ((cursor_in_echo_area
	   /* If we are showing a message instead of the mini-buffer,
	      show the cursor for the message instead of for the
	      (now hidden) mini-buffer contents.  */
	   || (EQ (minibuf_window, selected_window)
	       && EQ (minibuf_window, echo_area_window)
	       && !NILP (echo_area_buffer[0])))
	  /* These cases apply only to the frame that contains
	     the active mini-buffer window.  */
	  && FRAME_HAS_MINIBUF_P (f)
	  && EQ (FRAME_MINIBUF_WINDOW (f), echo_area_window))
	{
	  int top = XINT (XWINDOW (FRAME_MINIBUF_WINDOW (f))->top);
	  int row, col;

	  if (cursor_in_echo_area < 0)
	    {
	      /* Negative value of cursor_in_echo_area means put
                 cursor at beginning of line.  */
	      row = top;
	      col = 0;
	    }
	  else
	    {
	      /* Positive value of cursor_in_echo_area means put
		 cursor at the end of the prompt.  If the mini-buffer
		 is several lines high, find the last line that has
		 any text on it.  */
	      row = FRAME_HEIGHT (f);
	      do 
		{
		  --row;
		  col = 0;
		  
		  if (MATRIX_ROW_ENABLED_P (current_matrix, row))
		    {
		      /* Frame rows are filled up with spaces that
			 must be ignored here.  */
		      struct glyph_row *r = MATRIX_ROW (current_matrix,
							row);
		      struct glyph *start = r->glyphs[TEXT_AREA];
		      struct glyph *last = start + r->used[TEXT_AREA];

		      while (last > start
			     && (last - 1)->charpos < 0)
			--last;
		      
		      col = last - start;
		    }
		}
	      while (row > top && col == 0);

	      /* Make sure COL is not out of range.  */
	      if (col >= FRAME_CURSOR_X_LIMIT (f))
		{
		  /* If we have another row, advance cursor into it.  */
		  if (row < FRAME_HEIGHT (f) - 1)
		    {
		      col = FRAME_LEFT_SCROLL_BAR_WIDTH (f);
		      row++;
		    }
		  /* Otherwise move it back in range.  */
		  else
		    col = FRAME_CURSOR_X_LIMIT (f) - 1;
		}
	    }

	  cursor_to (row, col);
	}
      else
	{
	  /* We have only one cursor on terminal frames.  Use it to
	     display the cursor of the selected window.  */
	  struct window *w = XWINDOW (FRAME_SELECTED_WINDOW (f));
	  if (w->cursor.vpos >= 0
	      /* The cursor vpos may be temporarily out of bounds
	         in the following situation:  There is one window,
		 with the cursor in the lower half of it.  The window
		 is split, and a message causes a redisplay before
	         a new cursor position has been computed.  */
	      && w->cursor.vpos < XFASTINT (w->height))
	    {
	      int x = WINDOW_TO_FRAME_HPOS (w, w->cursor.hpos);
	      int y = WINDOW_TO_FRAME_VPOS (w, w->cursor.vpos);

	      if (INTEGERP (w->left_margin_width))
		x += XFASTINT (w->left_margin_width);
	      
	      /* x = max (min (x, FRAME_WINDOW_WIDTH (f) - 1), 0); */
	      cursor_to (y, x);
	    }
	}
    }

 do_pause:

  clear_desired_matrices (f);
  return pause;
}


/* Do line insertions/deletions on frame F for frame-based redisplay.  */

int
scrolling (frame)
     struct frame *frame;
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
  struct glyph_matrix *current_matrix = frame->current_matrix;
  struct glyph_matrix *desired_matrix = frame->desired_matrix;

  if (!current_matrix)
    abort ();

  /* Compute hash codes of all the lines.  Also calculate number of
     changed lines, number of unchanged lines at the beginning, and
     number of unchanged lines at the end.  */
  changed_lines = 0;
  unchanged_at_top = 0;
  unchanged_at_bottom = FRAME_HEIGHT (frame);
  for (i = 0; i < FRAME_HEIGHT (frame); i++)
    {
      /* Give up on this scrolling if some old lines are not enabled.  */
      if (!MATRIX_ROW_ENABLED_P (current_matrix, i))
	return 0;
      old_hash[i] = line_hash_code (MATRIX_ROW (current_matrix, i));
      if (! MATRIX_ROW_ENABLED_P (desired_matrix, i))
	{
	  /* This line cannot be redrawn, so don't let scrolling mess it.  */
	  new_hash[i] = old_hash[i];
#define INFINITY 1000000	/* Taken from scroll.c */
	  draw_cost[i] = INFINITY;
	}
      else
	{
	  new_hash[i] = line_hash_code (MATRIX_ROW (desired_matrix, i));
	  draw_cost[i] = line_draw_cost (desired_matrix, i);
	}

      if (old_hash[i] != new_hash[i])
	{
	  changed_lines++;
	  unchanged_at_bottom = FRAME_HEIGHT (frame) - i - 1;
	}
      else if (i == unchanged_at_top)
	unchanged_at_top++;
      old_draw_cost[i] = line_draw_cost (current_matrix, i);
    }

  /* If changed lines are few, don't allow preemption, don't scroll.  */
  if ((!scroll_region_ok && changed_lines < baud_rate / 2400)
      || unchanged_at_bottom == FRAME_HEIGHT (frame))
    return 1;

  window_size = (FRAME_HEIGHT (frame) - unchanged_at_top
		 - unchanged_at_bottom);

  if (scroll_region_ok)
    free_at_end_vpos -= unchanged_at_bottom;
  else if (memory_below_frame)
    free_at_end_vpos = -1;

  /* If large window, fast terminal and few lines in common between
     current frame and desired frame, don't bother with i/d calc.  */
  if (!scroll_region_ok && window_size >= 18 && baud_rate > 2400
      && (window_size >=
	  10 * scrolling_max_lines_saved (unchanged_at_top,
					  FRAME_HEIGHT (frame) - unchanged_at_bottom,
					  old_hash, new_hash, draw_cost)))
    return 0;

  if (window_size < 2)
    return 0;

  scrolling_1 (frame, window_size, unchanged_at_top, unchanged_at_bottom,
	       draw_cost + unchanged_at_top - 1,
	       old_draw_cost + unchanged_at_top - 1,
	       old_hash + unchanged_at_top - 1,
	       new_hash + unchanged_at_top - 1,
	       free_at_end_vpos - unchanged_at_top);

  return 0;
}


/* Count the number of blanks at the start of the vector of glyphs R
   which is LEN glyphs long.  */

static int
count_blanks (r, len)
     struct glyph *r;
     int len;
{
  int i;
  
  for (i = 0; i < len; ++i)
    if (!CHAR_GLYPH_SPACE_P (r[i]))
      break;

  return i;
}


/* Count the number of glyphs in common at the start of the glyph
   vectors STR1 and STR2.  END1 is the end of STR1 and END2 is the end
   of STR2.  Value is the number of equal glyphs equal at the start.  */

static int
count_match (str1, end1, str2, end2)
     struct glyph *str1, *end1, *str2, *end2;
{
  struct glyph *p1 = str1;
  struct glyph *p2 = str2;
  
  while (p1 < end1
	 && p2 < end2
	 && GLYPH_CHAR_AND_FACE_EQUAL_P (p1, p2))
    ++p1, ++p2;
  
  return p1 - str1;
}


/* Char insertion/deletion cost vector, from term.c */

extern int *char_ins_del_vector;
#define char_ins_del_cost(f) (&char_ins_del_vector[FRAME_WINDOW_WIDTH((f))])


/* Perform a frame-based update on line VPOS in frame FRAME.  */

static void
update_frame_line (f, vpos)
     struct frame *f;
     int vpos;
{
  struct glyph *obody, *nbody, *op1, *op2, *np1, *nend;
  int tem;
  int osp, nsp, begmatch, endmatch, olen, nlen;
  struct glyph_matrix *current_matrix = f->current_matrix;
  struct glyph_matrix *desired_matrix = f->desired_matrix;
  struct glyph_row *current_row = MATRIX_ROW (current_matrix, vpos);
  struct glyph_row *desired_row = MATRIX_ROW (desired_matrix, vpos);
  int must_write_whole_line_p;
  int write_spaces_p = must_write_spaces;
  int colored_spaces_p = (FACE_FROM_ID (f, DEFAULT_FACE_ID)->background
			  != FACE_TTY_DEFAULT_BG_COLOR);

  if (colored_spaces_p)
    write_spaces_p = 1;

  /* Current row not enabled means it has unknown contents.  We must
     write the whole desired line in that case.  */
  must_write_whole_line_p = !current_row->enabled_p;
  if (must_write_whole_line_p)
    {
      obody = 0;
      olen = 0;
    }
  else
    {
      obody = MATRIX_ROW_GLYPH_START (current_matrix, vpos);
      olen = current_row->used[TEXT_AREA];
      
      /* Ignore trailing spaces, if we can.  */
      if (!write_spaces_p)
	while (olen > 0 && CHAR_GLYPH_SPACE_P (obody[olen-1]))
	  olen--;
    }

  current_row->enabled_p = 1;
  current_row->used[TEXT_AREA] = desired_row->used[TEXT_AREA];

  /* If desired line is empty, just clear the line.  */
  if (!desired_row->enabled_p)
    {
      nlen = 0;
      goto just_erase;
    }

  nbody = desired_row->glyphs[TEXT_AREA];
  nlen = desired_row->used[TEXT_AREA];
  nend = nbody + nlen;

  /* If display line has unknown contents, write the whole line.  */
  if (must_write_whole_line_p)
    {
      /* Ignore spaces at the end, if we can.  */
      if (!write_spaces_p)
	while (nlen > 0 && CHAR_GLYPH_SPACE_P (nbody[nlen - 1]))
	  --nlen;

      /* Write the contents of the desired line.  */
      if (nlen)
	{
          cursor_to (vpos, 0);
	  write_glyphs (nbody, nlen);
	}
      
      /* Don't call clear_end_of_line if we already wrote the whole
	 line.  The cursor will not be at the right margin in that
	 case but in the line below.  */
      if (nlen < FRAME_WINDOW_WIDTH (f))
	{
	  cursor_to (vpos, nlen);
          clear_end_of_line (FRAME_WINDOW_WIDTH (f));
	}
      else
	/* Make sure we are in the right row, otherwise cursor movement
	   with cmgoto might use `ch' in the wrong row.  */
	cursor_to (vpos, 0);
      
      make_current (desired_matrix, current_matrix, vpos);
      return;
    }

  /* Pretend trailing spaces are not there at all,
     unless for one reason or another we must write all spaces.  */
  if (!write_spaces_p)
    while (nlen > 0 && CHAR_GLYPH_SPACE_P (nbody[nlen - 1]))
      nlen--;

  /* If there's no i/d char, quickly do the best we can without it.  */
  if (!char_ins_del_ok)
    {
      int i, j;

      /* Find the first glyph in desired row that doesn't agree with
	 a glyph in the current row, and write the rest from there on.  */
      for (i = 0; i < nlen; i++)
	{
	  if (i >= olen || !GLYPH_EQUAL_P (nbody + i, obody + i))
	    {
	      /* Find the end of the run of different glyphs.  */
	      j = i + 1;
	      while (j < nlen
		     && (j >= olen
			 || !GLYPH_EQUAL_P (nbody + j, obody + j)
			 || CHAR_GLYPH_PADDING_P (nbody[j])))
		++j;
		     
	      /* Output this run of non-matching chars.  */ 
	      cursor_to (vpos, i);
	      write_glyphs (nbody + i, j - i);
	      i = j - 1;

	      /* Now find the next non-match.  */
	    }
	}

      /* Clear the rest of the line, or the non-clear part of it.  */
      if (olen > nlen)
	{
	  cursor_to (vpos, nlen);
	  clear_end_of_line (olen);
	}

      /* Make current row = desired row.  */
      make_current (desired_matrix, current_matrix, vpos);
      return;
    }

  /* Here when CHAR_INS_DEL_OK != 0, i.e. we can insert or delete
     characters in a row.  */

  if (!olen)
    {
      /* If current line is blank, skip over initial spaces, if
	 possible, and write the rest.  */
      if (write_spaces_p)
	nsp = 0;
      else
	nsp = count_blanks (nbody, nlen);

      if (nlen > nsp)
	{
	  cursor_to (vpos, nsp);
	  write_glyphs (nbody + nsp, nlen - nsp);
	}

      /* Exchange contents between current_frame and new_frame.  */
      make_current (desired_matrix, current_matrix, vpos);
      return;
    }

  /* Compute number of leading blanks in old and new contents.  */
  osp = count_blanks (obody, olen);
  nsp = (colored_spaces_p ? 0 : count_blanks (nbody, nlen));

  /* Compute number of matching chars starting with first non-blank.  */
  begmatch = count_match (obody + osp, obody + olen,
			  nbody + nsp, nbody + nlen);

  /* Spaces in new match implicit space past the end of old.  */
  /* A bug causing this to be a no-op was fixed in 18.29.  */
  if (!write_spaces_p && osp + begmatch == olen)
    {
      np1 = nbody + nsp;
      while (np1 + begmatch < nend && CHAR_GLYPH_SPACE_P (np1[begmatch]))
	++begmatch;
    }

  /* Avoid doing insert/delete char
     just cause number of leading spaces differs
     when the following text does not match.  */
  if (begmatch == 0 && osp != nsp)
    osp = nsp = min (osp, nsp);

  /* Find matching characters at end of line */
  op1 = obody + olen;
  np1 = nbody + nlen;
  op2 = op1 + begmatch - min (olen - osp, nlen - nsp);
  while (op1 > op2
	 && GLYPH_EQUAL_P (op1 - 1, np1 - 1))
    {
      op1--;
      np1--;
    }
  endmatch = obody + olen - op1;

  /* tem gets the distance to insert or delete.
     endmatch is how many characters we save by doing so.
     Is it worth it?  */

  tem = (nlen - nsp) - (olen - osp);
  if (endmatch && tem
      && (!char_ins_del_ok || endmatch <= char_ins_del_cost (f)[tem]))
    endmatch = 0;

  /* nsp - osp is the distance to insert or delete.
     If that is nonzero, begmatch is known to be nonzero also.
     begmatch + endmatch is how much we save by doing the ins/del.
     Is it worth it?  */

  if (nsp != osp
      && (!char_ins_del_ok
	  || begmatch + endmatch <= char_ins_del_cost (f)[nsp - osp]))
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
      insert_glyphs (0, nsp - osp);
    }
  olen += nsp - osp;

  tem = nsp + begmatch + endmatch;
  if (nlen != tem || olen != tem)
    {
      if (!endmatch || nlen == olen)
	{
	  /* If new text being written reaches right margin, there is
	     no need to do clear-to-eol at the end of this function
	     (and it would not be safe, since cursor is not going to
	     be "at the margin" after the text is done).  */
	  if (nlen == FRAME_WINDOW_WIDTH (f))
	    olen = 0;

	  /* Function write_glyphs is prepared to do nothing
	     if passed a length <= 0.  Check it here to avoid
	     unnecessary cursor movement.  */
	  if (nlen - tem > 0)
	    {
	      cursor_to (vpos, nsp + begmatch);
	      write_glyphs (nbody + nsp + begmatch, nlen - tem);
	    }
	}
      else if (nlen > olen)
	{
	  /* Here, we used to have the following simple code:
	     ----------------------------------------
	     write_glyphs (nbody + nsp + begmatch, olen - tem);
	     insert_glyphs (nbody + nsp + begmatch + olen - tem, nlen - olen);
	     ----------------------------------------
	     but it doesn't work if nbody[nsp + begmatch + olen - tem]
	     is a padding glyph.  */
	  int out = olen - tem;	/* Columns to be overwritten originally.  */
	  int del;

	  cursor_to (vpos, nsp + begmatch);
	  
	  /* Calculate columns we can actually overwrite.  */
	  while (CHAR_GLYPH_PADDING_P (nbody[nsp + begmatch + out]))
	    out--;
	  write_glyphs (nbody + nsp + begmatch, out);
	  
	  /* If we left columns to be overwritten, we must delete them.  */
	  del = olen - tem - out;
	  if (del > 0)
	    delete_glyphs (del);
	  
	  /* At last, we insert columns not yet written out.  */
	  insert_glyphs (nbody + nsp + begmatch + out, nlen - olen + del);
	  olen = nlen;
	}
      else if (olen > nlen)
	{
	  cursor_to (vpos, nsp + begmatch);
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
  make_current (desired_matrix, current_matrix, vpos);
}



/***********************************************************************
		   X/Y Position -> Buffer Position
 ***********************************************************************/

/* Determine what's under window-relative pixel position (*X, *Y).
   Return in *OBJECT the object (string or buffer) that's there.
   Return in *POS the position in that object. Adjust *X and *Y
   to character boundaries.  */

void
buffer_posn_from_coords (w, x, y, object, pos)
     struct window *w;
     int *x, *y;
     Lisp_Object *object;
     struct display_pos *pos;
{
  struct it it;
  struct buffer *old_current_buffer = current_buffer;
  struct text_pos startp;
  int left_area_width;

  current_buffer = XBUFFER (w->buffer);
  SET_TEXT_POS_FROM_MARKER (startp, w->start);
  CHARPOS (startp) = min (ZV, max (BEGV, CHARPOS (startp)));
  BYTEPOS (startp) = min (ZV_BYTE, max (BEGV_BYTE, BYTEPOS (startp)));
  start_display (&it, w, startp);
  
  left_area_width = WINDOW_DISPLAY_LEFT_AREA_PIXEL_WIDTH (w);
  move_it_to (&it, -1, *x + it.first_visible_x - left_area_width, *y, -1,
	      MOVE_TO_X | MOVE_TO_Y);
  
  *x = it.current_x - it.first_visible_x + left_area_width;
  *y = it.current_y;
  current_buffer = old_current_buffer;

  *object = STRINGP (it.string) ? it.string : w->buffer;
  *pos = it.current;
}


/* Value is the string under window-relative coordinates X/Y in the
   mode or header line of window W, or nil if none.  MODE_LINE_P non-zero
   means look at the mode line.  *CHARPOS is set to the position in
   the string returned.  */

Lisp_Object
mode_line_string (w, x, y, mode_line_p, charpos)
     struct window *w;
     int x, y, mode_line_p;
     int *charpos;
{
  struct glyph_row *row;
  struct glyph *glyph, *end;
  struct frame *f = XFRAME (w->frame);
  int x0;
  Lisp_Object string = Qnil;

  if (mode_line_p)
    row = MATRIX_MODE_LINE_ROW (w->current_matrix);
  else
    row = MATRIX_HEADER_LINE_ROW (w->current_matrix);

  if (row->mode_line_p && row->enabled_p)
    {
      /* The mode lines are displayed over scroll bars and fringes,
	 and X is window-relative.  Correct X by the scroll bar
	 and fringe width.  */
      if (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_LEFT (f))
	x += FRAME_SCROLL_BAR_COLS (f) * CANON_X_UNIT (f);
      x += FRAME_LEFT_FRINGE_WIDTH (f);

      /* Find the glyph under X.  If we find one with a string object,
         it's the one we were looking for.  */
      glyph = row->glyphs[TEXT_AREA];
      end = glyph + row->used[TEXT_AREA];
      for (x0 = 0; glyph < end; x0 += glyph->pixel_width, ++glyph)
	if (x >= x0 && x < x0 + glyph->pixel_width)
	  {
	    string = glyph->object;
	    *charpos = glyph->charpos;
	    break;
	  }
    }

  return string;
}


/***********************************************************************
			 Changing Frame Sizes
 ***********************************************************************/

#ifdef SIGWINCH

SIGTYPE
window_change_signal (signalnum) /* If we don't have an argument, */
     int signalnum;		/* some compilers complain in signal calls.  */
{
  int width, height;
#ifndef USE_CRT_DLL
  extern int errno;
#endif
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
	    change_frame_size (XFRAME (frame), height, width, 0, 1, 0);
	    break;
	  }
      }
  }

  signal (SIGWINCH, window_change_signal);
  errno = old_errno;
}
#endif /* SIGWINCH */


/* Do any change in frame size that was requested by a signal.  SAFE
   non-zero means this function is called from a place where it is
   safe to change frame sizes  while a redisplay is in progress.  */

void
do_pending_window_change (safe)
     int safe;
{
  /* If window_change_signal should have run before, run it now.  */
  if (redisplaying_p && !safe)
    return;
  
  while (delayed_size_change)
    {
      Lisp_Object tail, frame;

      delayed_size_change = 0;

      FOR_EACH_FRAME (tail, frame)
	{
	  struct frame *f = XFRAME (frame);

	  int height = FRAME_NEW_HEIGHT (f);
	  int width = FRAME_NEW_WIDTH (f);

	  if (height != 0 || width != 0)
	    change_frame_size (f, height, width, 0, 0, safe);
	}
    }
}


/* Change the frame height and/or width.  Values may be given as zero to
   indicate no change is to take place. 

   If DELAY is non-zero, then assume we're being called from a signal
   handler, and queue the change for later - perhaps the next
   redisplay.  Since this tries to resize windows, we can't call it
   from a signal handler.

   SAFE non-zero means this function is called from a place where it's
   safe to change frame sizes while a redisplay is in progress.  */

void
change_frame_size (f, newheight, newwidth, pretend, delay, safe)
     register struct frame *f;
     int newheight, newwidth, pretend, delay, safe;
{
  Lisp_Object tail, frame;

  if (! FRAME_WINDOW_P (f))
    {
      /* When using termcap, or on MS-DOS, all frames use
	 the same screen, so a change in size affects all frames.  */
      FOR_EACH_FRAME (tail, frame)
	if (! FRAME_WINDOW_P (XFRAME (frame)))
	  change_frame_size_1 (XFRAME (frame), newheight, newwidth,
			       pretend, delay, safe);
    }
  else
    change_frame_size_1 (f, newheight, newwidth, pretend, delay, safe);
}

static void
change_frame_size_1 (f, newheight, newwidth, pretend, delay, safe)
     register struct frame *f;
     int newheight, newwidth, pretend, delay, safe;
{
  int new_frame_window_width;
  int count = specpdl_ptr - specpdl;

  /* If we can't deal with the change now, queue it for later.  */
  if (delay || (redisplaying_p && !safe))
    {
      FRAME_NEW_HEIGHT (f) = newheight;
      FRAME_NEW_WIDTH (f) = newwidth;
      delayed_size_change = 1;
      return;
    }

  /* This size-change overrides any pending one for this frame.  */
  FRAME_NEW_HEIGHT (f) = 0;
  FRAME_NEW_WIDTH  (f) = 0;

  /* If an argument is zero, set it to the current value.  */
  if (newheight == 0)
    newheight = FRAME_HEIGHT (f);
  if (newwidth == 0)
    newwidth  = FRAME_WIDTH  (f);

  /* Compute width of windows in F.
     This is the width of the frame without vertical scroll bars.  */
  new_frame_window_width = FRAME_WINDOW_WIDTH_ARG (f, newwidth);

  /* Round up to the smallest acceptable size.  */
  check_frame_size (f, &newheight, &newwidth);

  /* If we're not changing the frame size, quit now.  */
  if (newheight == FRAME_HEIGHT (f)
      && new_frame_window_width == FRAME_WINDOW_WIDTH (f))
    return;

  BLOCK_INPUT;

#ifdef MSDOS
  /* We only can set screen dimensions to certain values supported
     by our video hardware.  Try to find the smallest size greater
     or equal to the requested dimensions.  */
  dos_set_window_size (&newheight, &newwidth);
#endif

  if (newheight != FRAME_HEIGHT (f))
    {
      if (FRAME_HAS_MINIBUF_P (f) && !FRAME_MINIBUF_ONLY_P (f))
	{
	  /* Frame has both root and mini-buffer.  */
	  XSETFASTINT (XWINDOW (FRAME_ROOT_WINDOW (f))->top,
		       FRAME_TOP_MARGIN (f));
	  set_window_height (FRAME_ROOT_WINDOW (f),
			     (newheight
			      - 1
			      - FRAME_TOP_MARGIN (f)),
			      0);
	  XSETFASTINT (XWINDOW (FRAME_MINIBUF_WINDOW (f))->top,
		       newheight - 1);
	  set_window_height (FRAME_MINIBUF_WINDOW (f), 1, 0);
	}
      else
	/* Frame has just one top-level window.  */
	set_window_height (FRAME_ROOT_WINDOW (f),
			   newheight - FRAME_TOP_MARGIN (f), 0);

      if (FRAME_TERMCAP_P (f) && !pretend)
	FrameRows = newheight;
    }

  if (new_frame_window_width  != FRAME_WINDOW_WIDTH (f))
    {
      set_window_width (FRAME_ROOT_WINDOW (f), new_frame_window_width, 0);
      if (FRAME_HAS_MINIBUF_P (f))
	set_window_width (FRAME_MINIBUF_WINDOW (f), new_frame_window_width, 0);

      if (FRAME_TERMCAP_P (f) && !pretend)
	FrameCols = newwidth;

      if (WINDOWP (f->tool_bar_window))
	XSETFASTINT (XWINDOW (f->tool_bar_window)->width, newwidth);
    }

  FRAME_HEIGHT (f) = newheight;
  SET_FRAME_WIDTH (f, newwidth);

  {
    struct window *w = XWINDOW (FRAME_SELECTED_WINDOW (f));
    int text_area_x, text_area_y, text_area_width, text_area_height;
    
    window_box (w, TEXT_AREA, &text_area_x, &text_area_y, &text_area_width,
		&text_area_height);
    if (w->cursor.x >= text_area_x + text_area_width)
      w->cursor.hpos = w->cursor.x = 0;
    if (w->cursor.y >= text_area_y + text_area_height)
      w->cursor.vpos = w->cursor.y = 0;
  }

  adjust_glyphs (f);
  calculate_costs (f);
  SET_FRAME_GARBAGED (f);
  f->resized_p = 1;

  UNBLOCK_INPUT;

  record_unwind_protect (Fset_buffer, Fcurrent_buffer ());

  /* This isn't quite a no-op: it runs window-configuration-change-hook.  */
  Fset_window_buffer (FRAME_SELECTED_WINDOW (f),
		      XWINDOW (FRAME_SELECTED_WINDOW (f))->buffer);

  unbind_to (count, Qnil);
}



/***********************************************************************
		   Terminal Related Lisp Functions
 ***********************************************************************/

DEFUN ("open-termscript", Fopen_termscript, Sopen_termscript,
       1, 1, "FOpen termscript file: ",
       doc: /* Start writing all terminal output to FILE as well as the terminal.
FILE = nil means just close any termscript file currently open.  */)
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


DEFUN ("send-string-to-terminal", Fsend_string_to_terminal,
       Ssend_string_to_terminal, 1, 1, 0,
       doc: /* Send STRING to the terminal without alteration.
Control characters in STRING will have terminal-dependent effects.  */)
     (string)
     Lisp_Object string;
{
  /* ??? Perhaps we should do something special for multibyte strings here.  */
  CHECK_STRING (string);
  fwrite (XSTRING (string)->data, 1, STRING_BYTES (XSTRING (string)), stdout);
  fflush (stdout);
  if (termscript)
    {
      fwrite (XSTRING (string)->data, 1, STRING_BYTES (XSTRING (string)),
	      termscript);
      fflush (termscript);
    }
  return Qnil;
}


DEFUN ("ding", Fding, Sding, 0, 1, 0,
       doc: /* Beep, or flash the screen.
Also, unless an argument is given,
terminate any keyboard macro currently executing.  */)
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

void
bitch_at_user ()
{
  if (noninteractive)
    putchar (07);
  else if (!INTERACTIVE)  /* Stop executing a keyboard macro.  */
    error ("Keyboard macro terminated by a command ringing the bell");
  else
    ring_bell ();
  fflush (stdout);
}



/***********************************************************************
			  Sleeping, Waiting
 ***********************************************************************/

DEFUN ("sleep-for", Fsleep_for, Ssleep_for, 1, 2, 0,
       doc: /* Pause, without updating display, for SECONDS seconds.
SECONDS may be a floating-point value, meaning that you can wait for a
fraction of a second.  Optional second arg MILLISECONDS specifies an
additional wait period, in milliseconds; this may be useful if your
Emacs was built without floating point support.
\(Not all operating systems support waiting for a fraction of a second.)  */)
     (seconds, milliseconds)
     Lisp_Object seconds, milliseconds;
{
  int sec, usec;

  if (NILP (milliseconds))
    XSETINT (milliseconds, 0);
  else
    CHECK_NUMBER (milliseconds);
  usec = XINT (milliseconds) * 1000;

  {
    double duration = extract_float (seconds);
    sec = (int) duration;
    usec += (duration - sec) * 1000000;
  }

#ifndef EMACS_HAS_USECS
  if (sec == 0 && usec != 0)
    error ("millisecond `sleep-for' not supported on %s", SYSTEM_TYPE);
#endif

  /* Assure that 0 <= usec < 1000000.  */
  if (usec < 0)
    {
      /* We can't rely on the rounding being correct if usec is negative.  */
      if (-1000000 < usec)
	sec--, usec += 1000000;
      else
	sec -= -usec / 1000000, usec = 1000000 - (-usec % 1000000);
    }
  else
    sec += usec / 1000000, usec %= 1000000;

  if (sec < 0 || (sec == 0 && usec == 0))
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
   is because the VMS preprocessor doesn't grok `defined'.  */
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
sit_for (sec, usec, reading, display, initial_display)
     int sec, usec, reading, display, initial_display;
{
  Lisp_Object read_kbd;

  swallow_events (display);

  if (detect_input_pending_run_timers (display) || !NILP (Vexecuting_macro))
    return Qnil;

  if (initial_display)
    redisplay_preserve_echo_area (2);

  if (sec == 0 && usec == 0)
    return Qt;

#ifdef SIGIO
  gobble_input (0);
#endif

  XSETINT (read_kbd, reading ? -1 : 1);
  wait_reading_process_input (sec, usec, read_kbd, display);

  return detect_input_pending () ? Qnil : Qt;
}


DEFUN ("sit-for", Fsit_for, Ssit_for, 1, 3, 0,
       doc: /* Perform redisplay, then wait for SECONDS seconds or until input is available.
SECONDS may be a floating-point value, meaning that you can wait for a
fraction of a second.  Optional second arg MILLISECONDS specifies an
additional wait period, in milliseconds; this may be useful if your
Emacs was built without floating point support.
\(Not all operating systems support waiting for a fraction of a second.)
Optional third arg NODISP non-nil means don't redisplay, just wait for input.
Redisplay is preempted as always if input arrives, and does not happen
if input is available before it starts.
Value is t if waited the full time with no input arriving.  */)
     (seconds, milliseconds, nodisp)
     Lisp_Object seconds, milliseconds, nodisp;
{
  int sec, usec;

  if (NILP (milliseconds))
    XSETINT (milliseconds, 0);
  else
    CHECK_NUMBER (milliseconds);
  usec = XINT (milliseconds) * 1000;

  {
    double duration = extract_float (seconds);
    sec = (int) duration;
    usec += (duration - sec) * 1000000;
  }

#ifndef EMACS_HAS_USECS
  if (usec != 0 && sec == 0)
    error ("millisecond `sit-for' not supported on %s", SYSTEM_TYPE);
#endif

  return sit_for (sec, usec, 0, NILP (nodisp), NILP (nodisp));
}



/***********************************************************************
			 Other Lisp Functions
 ***********************************************************************/

/* A vector of size >= 2 * NFRAMES + 3 * NBUFFERS + 1, containing the
   session's frames, frame names, buffers, buffer-read-only flags, and
   buffer-modified-flags, and a trailing sentinel (so we don't need to
   add length checks).  */

static Lisp_Object frame_and_buffer_state;


DEFUN ("frame-or-buffer-changed-p", Fframe_or_buffer_changed_p,
       Sframe_or_buffer_changed_p, 0, 0, 0,
       doc: /* Return non-nil if the frame and buffer state appears to have changed.
The state variable is an internal vector containing all frames and buffers,
aside from buffers whose names start with space,
along with the buffers' read-only and modified flags, which allows a fast
check to see whether the menu bars might need to be recomputed.
If this function returns non-nil, it updates the internal vector to reflect
the current state.  */)
     ()
{
  Lisp_Object tail, frame, buf;
  Lisp_Object *vecp;
  int n;

  vecp = XVECTOR (frame_and_buffer_state)->contents;
  FOR_EACH_FRAME (tail, frame)
    {
      if (!EQ (*vecp++, frame))
	goto changed;
      if (!EQ (*vecp++, XFRAME (frame)->name))
	goto changed;
    }
  /* Check that the buffer info matches.
     No need to test for the end of the vector
     because the last element of the vector is lambda
     and that will always cause a mismatch.  */
  for (tail = Vbuffer_alist; CONSP (tail); tail = XCDR (tail))
    {
      buf = XCDR (XCAR (tail));
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
  if (EQ (*vecp, Qlambda))
    return Qnil;
 changed:
  /* Start with 1 so there is room for at least one lambda at the end.  */
  n = 1;
  FOR_EACH_FRAME (tail, frame)
    n += 2;
  for (tail = Vbuffer_alist; CONSP (tail); tail = XCDR (tail))
    n += 3;
  /* Reallocate the vector if it's grown, or if it's shrunk a lot.  */
  if (n > XVECTOR (frame_and_buffer_state)->size
      || n + 20 < XVECTOR (frame_and_buffer_state)->size / 2)
    /* Add 20 extra so we grow it less often.  */
    frame_and_buffer_state = Fmake_vector (make_number (n + 20), Qlambda);
  vecp = XVECTOR (frame_and_buffer_state)->contents;
  FOR_EACH_FRAME (tail, frame)
    {
      *vecp++ = frame;
      *vecp++ = XFRAME (frame)->name;
    }
  for (tail = Vbuffer_alist; CONSP (tail); tail = XCDR (tail))
    {
      buf = XCDR (XCAR (tail));
      /* Ignore buffers that aren't included in buffer lists.  */
      if (XSTRING (XBUFFER (buf)->name)->data[0] == ' ')
	continue;
      *vecp++ = buf;
      *vecp++ = XBUFFER (buf)->read_only;
      *vecp++ = Fbuffer_modified_p (buf);
    }
  /* Fill up the vector with lambdas (always at least one).  */
  *vecp++ = Qlambda;
  while  (vecp - XVECTOR (frame_and_buffer_state)->contents
	  < XVECTOR (frame_and_buffer_state)->size)
    *vecp++ = Qlambda;
  /* Make sure we didn't overflow the vector.  */
  if (vecp - XVECTOR (frame_and_buffer_state)->contents
      > XVECTOR (frame_and_buffer_state)->size)
    abort ();
  return Qt;
}



/***********************************************************************
			    Initialization
***********************************************************************/

char *terminal_type;

/* Initialization done when Emacs fork is started, before doing stty.
   Determine terminal type and set terminal_driver.  Then invoke its
   decoding routine to set up variables in the terminal package.  */

void
init_display ()
{
#ifdef HAVE_X_WINDOWS
  extern int display_arg;
#endif

  /* Construct the space glyph.  */
  space_glyph.type = CHAR_GLYPH;
  SET_CHAR_GLYPH_FROM_GLYPH (space_glyph, ' ');
  space_glyph.charpos = -1;

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

     If the DISPLAY environment variable is set and nonempty,
     try to use X, and die with an error message if that doesn't work.  */

#ifdef HAVE_X_WINDOWS
  if (! display_arg)
    {
      char *display;
#ifdef VMS
      display = getenv ("DECW$DISPLAY");
#else
      display = getenv ("DISPLAY");
#endif

      display_arg = (display != 0 && *display != 0);
    }

  if (!inhibit_window_system && display_arg 
#ifndef CANNOT_DUMP
     && initialized
#endif
     )
    {
      Vwindow_system = intern ("x");
#ifdef HAVE_X11
      Vwindow_system_version = make_number (11);
#else
      Vwindow_system_version = make_number (10);
#endif
#if defined (GNU_LINUX) && defined (HAVE_LIBNCURSES)
      /* In some versions of ncurses,
	 tputs crashes if we have not called tgetent.
	 So call tgetent.  */
      { char b[2044]; tgetent (b, "xterm");}
#endif
      adjust_frame_glyphs_initially ();
      return;
    }
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_NTGUI
  if (!inhibit_window_system) 
    {
      Vwindow_system = intern ("w32");
      Vwindow_system_version = make_number (1);
      adjust_frame_glyphs_initially ();
      return;
    }
#endif /* HAVE_NTGUI */

#ifdef macintosh
  if (!inhibit_window_system) 
    {
      Vwindow_system = intern ("mac");
      Vwindow_system_version = make_number (1);
      adjust_frame_glyphs_initially ();
      return;
    }
#endif /* macintosh */

  /* If no window system has been specified, try to use the terminal.  */
  if (! isatty (0))
    {
      fatal ("standard input is not a tty");
      exit (1);
    }

  /* Look at the TERM variable.  */
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
  /* VMS DCL tends to up-case things, so down-case term type.
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
#endif /* VMS */

  term_init (terminal_type);
  
  {
    struct frame *sf = SELECTED_FRAME ();
    int width = FRAME_WINDOW_WIDTH (sf);
    int height = FRAME_HEIGHT (sf);

    unsigned int total_glyphs = height * (width + 2) * sizeof (struct glyph);

    /* If these sizes are so big they cause overflow, just ignore the
       change.  It's not clear what better we could do.  */
    if (total_glyphs / sizeof (struct glyph) / height != width + 2)
      fatal ("screen size %dx%d too big", width, height);
  }

  adjust_frame_glyphs_initially ();
  calculate_costs (XFRAME (selected_frame));

#ifdef SIGWINCH
#ifndef CANNOT_DUMP
  if (initialized)
#endif /* CANNOT_DUMP */
    signal (SIGWINCH, window_change_signal);
#endif /* SIGWINCH */

  /* Set up faces of the initial terminal frame of a dumped Emacs.  */
  if (initialized
      && !noninteractive
#ifdef MSDOS
      /* The MSDOS terminal turns on its ``window system'' relatively
	 late into the startup, so we cannot do the frame faces'
	 initialization just yet.  It will be done later by pc-win.el
	 and internal_terminal_init.  */
      && (strcmp (terminal_type, "internal") != 0 || inhibit_window_system)
#endif
      && NILP (Vwindow_system))
    {
      /* For the initial frame, we don't have any way of knowing what
	 are the foreground and background colors of the terminal.  */
      struct frame *sf = SELECTED_FRAME();

      FRAME_FOREGROUND_PIXEL (sf) = FACE_TTY_DEFAULT_FG_COLOR;
      FRAME_BACKGROUND_PIXEL (sf) = FACE_TTY_DEFAULT_BG_COLOR;
      call0 (intern ("tty-set-up-initial-frame-faces"));
    }
}



/***********************************************************************
			   Blinking cursor
 ***********************************************************************/

DEFUN ("internal-show-cursor", Finternal_show_cursor,
       Sinternal_show_cursor, 2, 2, 0,
       doc: /* Set the cursor-visibility flag of WINDOW to SHOW.
WINDOW nil means use the selected window.  SHOW non-nil means
show a cursor in WINDOW in the next redisplay.  SHOW nil means
don't show a cursor.  */)
     (window, show)
     Lisp_Object window, show;
{
  /* Don't change cursor state while redisplaying.  This could confuse
     output routines.  */
  if (!redisplaying_p)
    {
      if (NILP (window))
	window = selected_window;
      else
	CHECK_WINDOW (window);
      
      XWINDOW (window)->cursor_off_p = NILP (show);
    }

  return Qnil;
}


DEFUN ("internal-show-cursor-p", Finternal_show_cursor_p,
       Sinternal_show_cursor_p, 0, 1, 0,
       doc: /* Value is non-nil if next redisplay will display a cursor in WINDOW.
WINDOW nil or omitted means report on the selected window.  */)
     (window)
     Lisp_Object window;
{
  struct window *w;
  
  if (NILP (window))
    window = selected_window;
  else
    CHECK_WINDOW (window);
  
  w = XWINDOW (window);
  return w->cursor_off_p ? Qnil : Qt;    
}


/***********************************************************************
			    Initialization
 ***********************************************************************/

void
syms_of_display ()
{
  defsubr (&Sredraw_frame);
  defsubr (&Sredraw_display);
  defsubr (&Sframe_or_buffer_changed_p);
  defsubr (&Sopen_termscript);
  defsubr (&Sding);
  defsubr (&Ssit_for);
  defsubr (&Ssleep_for);
  defsubr (&Ssend_string_to_terminal);
  defsubr (&Sinternal_show_cursor);
  defsubr (&Sinternal_show_cursor_p);

#if GLYPH_DEBUG
  defsubr (&Sdump_redisplay_history);
#endif

  frame_and_buffer_state = Fmake_vector (make_number (20), Qlambda);
  staticpro (&frame_and_buffer_state);

  Qdisplay_table = intern ("display-table");
  staticpro (&Qdisplay_table);
  Qredisplay_dont_pause = intern ("redisplay-dont-pause");
  staticpro (&Qredisplay_dont_pause);

  DEFVAR_INT ("baud-rate", &baud_rate,
	      doc: /* *The output baud rate of the terminal.
On most systems, changing this value will affect the amount of padding
and the other strategic decisions made during redisplay.  */);
  
  DEFVAR_BOOL ("inverse-video", &inverse_video,
	       doc: /* *Non-nil means invert the entire frame display.
This means everything is in inverse video which otherwise would not be.  */);
  
  DEFVAR_BOOL ("visible-bell", &visible_bell,
	       doc: /* *Non-nil means try to flash the frame to represent a bell.

See also `ring-bell-function'.  */);
  
  DEFVAR_BOOL ("no-redraw-on-reenter", &no_redraw_on_reenter,
	       doc: /* *Non-nil means no need to redraw entire frame after suspending.
A non-nil value is useful if the terminal can automatically preserve
Emacs's frame display when you reenter Emacs.
It is up to you to set this variable if your terminal can do that.  */);
  
  DEFVAR_LISP ("window-system", &Vwindow_system,
	       doc: /* Name of window system that Emacs is displaying through.
The value is a symbol--for instance, `x' for X windows.
The value is nil if Emacs is using a text-only terminal.  */);
  
  DEFVAR_LISP ("window-system-version", &Vwindow_system_version,
	       doc: /* The version number of the window system in use.
For X windows, this is 10 or 11.  */);
  
  DEFVAR_BOOL ("cursor-in-echo-area", &cursor_in_echo_area,
	       doc: /* Non-nil means put cursor in minibuffer, at end of any message there.  */);
  
  DEFVAR_LISP ("glyph-table", &Vglyph_table,
	       doc: /* Table defining how to output a glyph code to the frame.
If not nil, this is a vector indexed by glyph code to define the glyph.
Each element can be:
 integer: a glyph code which this glyph is an alias for.
 string: output this glyph using that string (not impl. in X windows).
 nil: this glyph mod 524288 is the code of a character to output,
    and this glyph / 524288 is the face number (see `face-id') to use
    while outputting it.  */);
  Vglyph_table = Qnil;

  DEFVAR_LISP ("standard-display-table", &Vstandard_display_table,
	       doc: /* Display table to use for buffers that specify none.
See `buffer-display-table' for more information.  */);
  Vstandard_display_table = Qnil;

  DEFVAR_BOOL ("redisplay-dont-pause", &redisplay_dont_pause,
	       doc: /* *Non-nil means update isn't paused when input is detected.  */);
  redisplay_dont_pause = 0;

  /* Initialize `window-system', unless init_display already decided it.  */
#ifdef CANNOT_DUMP
  if (noninteractive)
#endif
    {
      Vwindow_system = Qnil;
      Vwindow_system_version = Qnil;
    }
}
