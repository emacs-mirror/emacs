/* Calculate what line insertion or deletion to do, and do it,
   Copyright (C) 1985, 1986, 1990 Free Software Foundation, Inc.

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


#include "config.h"
#include "termchar.h"
#include "lisp.h"
#include "dispextern.h"
#include "screen.h"

extern struct display_line **ophys_lines;

#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))

/* All costs measured in characters.
   So no cost can exceed the area of a screen, measured in characters.
   Let's hope this is never more than 15000 characters.  */

#define INFINITY 15000

struct matrix_elt
  {
    /* Cost of outputting through this line
       if no insert/delete is done just above it.  */
    short writecost;
    /* Cost of outputting through this line
       if an insert is done just above it.  */
    short insertcost;
    /* Cost of outputting through this line
       if a delete is done just above it.  */
    short deletecost;
    /* Number of inserts so far in this run of inserts,
       for the cost in insertcost.  */
    char insertcount;
    /* Number of deletes so far in this run of deletes,
       for the cost in deletecost.  */
    char deletecount;
  };

/* See do_line_insertion_deletion_costs for info on these arrays. */

#ifndef MULTI_SCREEN
static int *insert_line_cost;
static int *delete_line_cost;
static int *insert_n_lines_cost;
static int *delete_n_lines_cost;
#endif


/* Determine, in matrix[i,j], the cost of updating the first j old lines
   into the first i new lines.
   This involves using insert or delete somewhere if i != j.
   For each matrix elements, three kinds of costs are recorded:
   the smallest cost that ends with an insert, the smallest
   cost that ends with a delete, and the smallest cost that
   ends with neither one.  These are kept separate because
   on some terminals the cost of doing an insert varies
   depending on whether one was just done, etc.  */

/* draw_cost[VPOS] is the cost of outputting new line at VPOS.
   old_hash[VPOS] is the hash code of the old line at VPOS.
   new_hash[VPOS] is the hash code of the new line at VPOS.
   Note that these are not true screen vpos's, but relative
   to the place at which the first mismatch between old and
   new contents appears.  */

static void
calculate_scrolling (screen, matrix, window_size, lines_below,
		     draw_cost, old_hash, new_hash,
		     free_at_end)
     SCREEN_PTR screen;
     /* matrix is of size window_size + 1 on each side.  */
     struct matrix_elt *matrix;
     int window_size;
     int *draw_cost;
     int *old_hash;
     int *new_hash;
     int free_at_end;
{
  register int i, j;
  int screen_height = SCREEN_HEIGHT (screen);
  register struct matrix_elt *p, *p1;
  register int cost, cost1;

  int lines_moved = window_size + (scroll_region_ok ? 0 : lines_below);
  /* first_insert_cost[I] is the cost of doing the first insert-line
     at the I'th line of the lines we are considering,
     where I is origin 1 (as it is below).  */
  int *first_insert_cost
    = &SCREEN_INSERT_COST (screen)[screen_height - 1 - lines_moved];
  int *first_delete_cost
    = &SCREEN_DELETE_COST (screen)[screen_height - 1 - lines_moved];
  int *next_insert_cost
    = &SCREEN_INSERTN_COST (screen)[screen_height - 1 - lines_moved];
  int *next_delete_cost
    = &SCREEN_DELETEN_COST (screen)[screen_height - 1 - lines_moved];

  /* Discourage long scrolls on fast lines.
     Don't scroll nearly a full screen height unless it saves
     at least 1/4 second.  */
  int extra_cost = baud_rate / (10 * 4 * SCREEN_HEIGHT (screen));

  /* initialize the top left corner of the matrix */
  matrix->writecost = 0;
  matrix->insertcost = INFINITY;
  matrix->deletecost = INFINITY;
  matrix->insertcount = 0;
  matrix->deletecount = 0;

  /* initialize the left edge of the matrix */
  cost = first_insert_cost[1] - next_insert_cost[1];
  for (i = 1; i <= window_size; i++)
    {
      p = matrix + i * (window_size + 1);
      cost += draw_cost[i] + next_insert_cost[i] + extra_cost;
      p->insertcost = cost;
      p->writecost = INFINITY;
      p->deletecost = INFINITY;
      p->insertcount = i;
      p->deletecount = 0;
    }

  /* initialize the top edge of the matrix */
  cost = first_delete_cost[1] - next_delete_cost[1];
  for (j = 1; j <= window_size; j++)
    {
      cost += next_delete_cost[j];
      matrix[j].deletecost = cost;
      matrix[j].writecost = INFINITY;
      matrix[j].insertcost = INFINITY;
      matrix[j].deletecount = j;
      matrix[j].insertcount = 0;
    }

  /* `i' represents the vpos among new screen contents.
     `j' represents the vpos among the old screen contents.  */
  p = matrix + window_size + 2;	/* matrix [1, 1] */
  for (i = 1; i <= window_size; i++, p++)
    for (j = 1; j <= window_size; j++, p++)
      {
	/* p contains the address of matrix [i, j] */

	/* First calculate the cost assuming we do
	   not insert or delete above this line.
	   That is, if we update through line i-1
	   based on old lines through j-1,
	   and then just change old line j to new line i.  */
	p1 = p - window_size - 2; /* matrix [i-1, j-1] */
	cost = p1->writecost;
	if (cost > p1->insertcost)
	  cost = p1->insertcost;
	if (cost > p1->deletecost)
	  cost = p1->deletecost;
	if (old_hash[j] != new_hash[i])
	  cost += draw_cost[i];
	p->writecost = cost;

	/* Calculate the cost if we do an insert-line
	   before outputting this line.
	   That is, we update through line i-1
	   based on old lines through j,
	   do an insert-line on line i,
	   and then output line i from scratch,
	   leaving old lines starting from j for reuse below.  */
	p1 = p - window_size - 1; /* matrix [i-1, j] */
	/* No need to think about doing a delete followed
	   immediately by an insert.  It cannot be as good
	   as not doing either of them.  */
	if (free_at_end == i)
	  {
	    cost = p1->writecost;
	    cost1 = p1->insertcost;
	  }
	else
	  {
	    cost = p1->writecost + first_insert_cost[i];
	    if (p1->insertcount > i)
	      abort ();
	    cost1 = p1->insertcost + next_insert_cost[i - p1->insertcount];
	  }
	p->insertcost = min (cost, cost1) + draw_cost[i] + extra_cost;
	p->insertcount = (cost < cost1) ? 1 : p1->insertcount + 1;
	if (p->insertcount > i)
	  abort ();

	/* Calculate the cost if we do a delete line after
	   outputting this line.
	   That is, we update through line i
	   based on old lines through j-1,
	   and throw away old line j.  */
	p1 = p - 1;		/* matrix [i, j-1] */
	/* No need to think about doing an insert followed
	   immediately by a delete.  */
	if (free_at_end == i)
	  {
	    cost = p1->writecost;
	    cost1 = p1->deletecost;
	  }
	else
	  {
	    cost = p1->writecost + first_delete_cost[i];
	    cost1 = p1->deletecost + next_delete_cost[i];
	  }
	p->deletecost = min (cost, cost1);
	p->deletecount = (cost < cost1) ? 1 : p1->deletecount + 1;
      }
}

/* Perform insert-lines and delete-lines operations
 according to the costs in the matrix.
 Updates the contents of the screen to record what was done. */

static void
do_scrolling (screen, matrix, window_size, unchanged_at_top)
     SCREEN_PTR screen;
     struct matrix_elt *matrix;
     int window_size;
     int unchanged_at_top;
{
  register struct matrix_elt *p;
  register int i, j;
  register struct screen_glyphs *current_screen;
  /* temp_screen->enable[i] means line i has been moved to current_screen.  */
  register struct screen_glyphs *temp_screen;
  struct queue { int count, pos; } *queue;
  int offset = unchanged_at_top;
  int qi = 0;
  int window = 0;
  register int tem;
  int next;

  queue = (struct queue *) alloca (SCREEN_HEIGHT (screen)
				   * sizeof (struct queue));

  current_screen = SCREEN_CURRENT_GLYPHS (screen);
  temp_screen = SCREEN_TEMP_GLYPHS (screen);

  bcopy (current_screen->glyphs, temp_screen->glyphs,
	 current_screen->height * sizeof (GLYPH *));
  bcopy (current_screen->used, temp_screen->used,
	 current_screen->height * sizeof (int));
  bcopy (current_screen->highlight, temp_screen->highlight,
	 current_screen->height * sizeof (char));
  bzero (temp_screen->enable, temp_screen->height * sizeof (char));
  bcopy (current_screen->bufp, temp_screen->bufp,
	 current_screen->height * sizeof (int));

#ifdef HAVE_X_WINDOWS
  if (SCREEN_IS_X (screen))
    {
      bcopy (current_screen->nruns, temp_screen->nruns,
	     current_screen->height * sizeof (int));
      bcopy (current_screen->face_list, temp_screen->face_list,
	     current_screen->height * sizeof (struct run *));
      bcopy (current_screen->top_left_x, temp_screen->top_left_x,
	     current_screen->height * sizeof (short));
      bcopy (current_screen->top_left_y, temp_screen->top_left_y,
	     current_screen->height * sizeof (short));
      bcopy (current_screen->pix_width, temp_screen->pix_width,
	     current_screen->height * sizeof (short));
      bcopy (current_screen->pix_height, temp_screen->pix_height,
	     current_screen->height * sizeof (short));
    }
#endif

  i = j = window_size;

  while (i > 0 || j > 0)
    {
      p = matrix + i * (window_size + 1) + j;
      tem = p->insertcost;
      if (tem < p->writecost && tem < p->deletecost)
	{
	  /* Insert should be done at vpos i-1, plus maybe some before */
	  queue[qi].count = p->insertcount;
	  i -= p->insertcount;
	  queue[qi++].pos = i + unchanged_at_top;
	}
      else if (p->deletecost < p->writecost)
	{
	  /* Old line at vpos j-1, and maybe some before it,
	     should be deleted */
	  j -= p->deletecount;
 	  if (!window)
	    {
	      set_terminal_window (window_size + unchanged_at_top);
	      window = 1;
	    }
	  ins_del_lines (j + unchanged_at_top, - p->deletecount);
	}
      else
	{
	  /* Best thing done here is no insert or delete */
	  /* Old line at vpos j-1 ends up at vpos i-1 */
	  current_screen->glyphs[i + offset - 1]
	    = temp_screen->glyphs[j + offset - 1];
	  current_screen->used[i + offset - 1]
	    = temp_screen->used[j + offset - 1];
	  current_screen->highlight[i + offset - 1]
	    = temp_screen->highlight[j + offset - 1];

	  temp_screen->enable[j + offset - 1] = 1;
	  i--;
	  j--;
	}
    }

  if (!window && qi)
    {
      set_terminal_window (window_size + unchanged_at_top);
      window = 1;
    }

  /* Now do all insertions */

  next = unchanged_at_top;
  for (i = qi - 1; i >= 0; i--)
    {
      ins_del_lines (queue[i].pos, queue[i].count);

      /* Mark the inserted lines as clear,
	 and put into them the line-contents strings
	 that were discarded during the deletions.
	 Those are the ones for which temp_screen->enable was not set.  */
      tem = queue[i].pos;
      for (j = tem + queue[i].count - 1; j >= tem; j--)
	{
	  current_screen->enable[j] = 0;
	  while (temp_screen->enable[next])
	    next++;
	  current_screen->glyphs[j] = temp_screen->glyphs[next++];
	}
    }

  if (window)
    set_terminal_window (0);
}

void
scrolling_1 (screen, window_size, unchanged_at_top, unchanged_at_bottom,
	     draw_cost, old_hash, new_hash, free_at_end)
     SCREEN_PTR screen;
     int window_size, unchanged_at_top, unchanged_at_bottom;
     int *draw_cost;
     int *old_hash;
     int *new_hash;
     int free_at_end;
{
  struct matrix_elt *matrix;
  matrix = ((struct matrix_elt *)
	    alloca ((window_size + 1) * (window_size + 1) * sizeof *matrix));

  calculate_scrolling (screen, matrix, window_size, unchanged_at_bottom,
		       draw_cost, old_hash, new_hash,
		       free_at_end);
  do_scrolling (screen, matrix, window_size, unchanged_at_top);
}

/* Return number of lines in common between current and desired screen contents
   described to us only as vectors of hash codes OLDHASH and NEWHASH.
   Consider only vpos range START to END (not including END).
   Ignore short lines on the assumption that
   avoiding redrawing such a line will have little weight.  */

int
scrolling_max_lines_saved (start, end, oldhash, newhash, cost)
     int start, end;
     int *oldhash, *newhash, *cost;
{
  struct { int hash; int count; } lines[01000];
  register int i, h;
  register int matchcount = 0;
  int avg_length = 0;
  int threshold;

  /* Compute a threshold which is 1/4 of average length of these lines.  */

  for (i = start; i < end; i++)
    avg_length += cost[i];

  avg_length /= end - start;
  threshold = avg_length / 4;

  bzero (lines, sizeof lines);

  /* Put new lines' hash codes in hash table.
     Ignore lines shorter than the threshold.
     Thus, if the lines that are in common
     are mainly the ones that are short,
     they won't count.  */
  for (i = start; i < end; i++)
    {
      if (cost[i] > threshold)
	{
	  h = newhash[i] & 0777;
	  lines[h].hash = newhash[i];
	  lines[h].count++;
	}
    }

  /* Look up old line hash codes in the hash table.
     Count number of matches between old lines and new.  */

  for (i = start; i < end; i++)
    {
      h = oldhash[i] & 0777;
      if (oldhash[i] == lines[h].hash)
	{
	  matchcount++;
	  if (--lines[h].count == 0)
	    lines[h].hash = 0;
	}
    }

  return matchcount;
}

/* Return a measure of the cost of moving the lines
   starting with vpos FROM, up to but not including vpos TO,
   down by AMOUNT lines (AMOUNT may be negative).
   These are the same arguments that might be given to
   scroll_screen_lines to perform this scrolling.  */

scroll_cost (screen, from, to, amount)
     SCREEN_PTR screen;
     int from, to, amount;
{
  /* Compute how many lines, at bottom of screen,
     will not be involved in actual motion.  */
  int limit = to;
  int offset;
  int height = SCREEN_HEIGHT (screen);

  if (amount == 0)
    return 0;

  if (! scroll_region_ok)
    limit = height;
  else if (amount > 0)
    limit += amount;

  if (amount < 0)
    {
      int temp = to;
      to = from + amount;
      from = temp + amount;
      amount = - amount;
    }

  offset = height - limit;

  return
    (SCREEN_INSERT_COST (screen)[offset + from]
     + (amount - 1) * SCREEN_INSERTN_COST (screen)[offset + from]
     + SCREEN_DELETEN_COST (screen)[offset + to]
     + (amount - 1) * SCREEN_DELETE_COST (screen)[offset + to]);
}

/* Calculate the line insertion/deletion
   overhead and multiply factor values */

static void
line_ins_del (screen, ov1, pf1, ovn, pfn, ov, mf)
     SCREEN_PTR screen;
     int ov1, ovn;
     int pf1, pfn;
     register int *ov, *mf;
{
  register int i;
  register int screen_height = SCREEN_HEIGHT (screen);
  register int insert_overhead = ov1 * 10;
  register int next_insert_cost = ovn * 10;

  for (i = screen_height-1; i >= 0; i--)
    {
      mf[i] = next_insert_cost / 10;
      next_insert_cost += pfn;
      ov[i] = (insert_overhead + next_insert_cost) / 10;
      insert_overhead += pf1;
    }
}

static void
ins_del_costs (screen,
	       one_line_string, multi_string,
	       setup_string, cleanup_string,
	       costvec, ncostvec, coefficient)
     SCREEN_PTR screen;
     char *one_line_string, *multi_string;
     char *setup_string, *cleanup_string;
     int *costvec, *ncostvec;
     int coefficient;
{
  if (multi_string)
    line_ins_del (screen,
		  string_cost (multi_string) * coefficient,
		  per_line_cost (multi_string) * coefficient,
		  0, 0, costvec, ncostvec);
  else if (one_line_string)
    line_ins_del (screen,
		  string_cost (setup_string) + string_cost (cleanup_string), 0,
		  string_cost (one_line_string),
		  per_line_cost (one_line_string),
		  costvec, ncostvec);
  else
    line_ins_del (screen,
		  9999, 0, 9999, 0,
		  costvec, ncostvec);
}

/* Calculate the insert and delete line costs.
   Note that this is done even when running with a window system
   because we want to know how long scrolling takes (and avoid it).
   This must be redone whenever the screen height changes.

   We keep the ID costs in a precomputed array based on the position
   at which the I or D is performed.  Also, there are two kinds of ID
   costs: the "once-only" and the "repeated".  This is to handle both
   those terminals that are able to insert N lines at a time (once-
   only) and those that must repeatedly insert one line.

   The cost to insert N lines at line L is
   	    [tt.t_ILov  + (screen_height + 1 - L) * tt.t_ILpf] +
	N * [tt.t_ILnov + (screen_height + 1 - L) * tt.t_ILnpf]

   ILov represents the basic insert line overhead.  ILpf is the padding
   required to allow the terminal time to move a line: insertion at line
   L changes (screen_height + 1 - L) lines.

   The first bracketed expression above is the overhead; the second is
   the multiply factor.  Both are dependent only on the position at
   which the insert is performed.  We store the overhead in
   SCREEN_INSERT_COST (screen) and the multiply factor in
   SCREEN_INSERTN_COST (screen).  Note however that any insertion
   must include at least one multiply factor.  Rather than compute this
   as SCREEN_INSERT_COST (screen)[line]+SCREEN_INSERTN_COST (screen)[line],
   we add SCREEN_INSERTN_COST (screen) into SCREEN_INSERT_COST (screen).
   This is reasonable because of the particular algorithm used in calcM.

   Deletion is essentially the same as insertion.
 */

do_line_insertion_deletion_costs (screen,
				  ins_line_string, multi_ins_string,
				  del_line_string, multi_del_string,
				  setup_string, cleanup_string, coefficient)
     SCREEN_PTR screen;
     char *ins_line_string, *multi_ins_string;
     char *del_line_string, *multi_del_string;
     char *setup_string, *cleanup_string;
     int coefficient;
{
  if (SCREEN_INSERT_COST (screen) != 0)
    {
      SCREEN_INSERT_COST (screen) =
	(int *) xrealloc (SCREEN_INSERT_COST (screen),
			  SCREEN_HEIGHT (screen) * sizeof (int));
      SCREEN_DELETEN_COST (screen) =
	(int *) xrealloc (SCREEN_DELETEN_COST (screen),
			  SCREEN_HEIGHT (screen) * sizeof (int));
      SCREEN_INSERTN_COST (screen) =
	(int *) xrealloc (SCREEN_INSERTN_COST (screen),
			  SCREEN_HEIGHT (screen) * sizeof (int));
      SCREEN_DELETE_COST (screen) =
	(int *) xrealloc (SCREEN_DELETE_COST (screen),
			  SCREEN_HEIGHT (screen) * sizeof (int));
    }
  else
    {
      SCREEN_INSERT_COST (screen) =
	(int *) xmalloc (SCREEN_HEIGHT (screen) * sizeof (int));
      SCREEN_DELETEN_COST (screen) =
	(int *) xmalloc (SCREEN_HEIGHT (screen) * sizeof (int));
      SCREEN_INSERTN_COST (screen) =
	(int *) xmalloc (SCREEN_HEIGHT (screen) * sizeof (int));
      SCREEN_DELETE_COST (screen) = 
	(int *) xmalloc (SCREEN_HEIGHT (screen) * sizeof (int));
    }

  ins_del_costs (screen,
		 ins_line_string, multi_ins_string,
		 setup_string, cleanup_string,
		 SCREEN_INSERT_COST (screen), SCREEN_INSERTN_COST (screen),
		 coefficient);
  ins_del_costs (screen,
		 del_line_string, multi_del_string,
		 setup_string, cleanup_string,
		 SCREEN_DELETEN_COST (screen), SCREEN_DELETE_COST (screen),
		 coefficient);
}
