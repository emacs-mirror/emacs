/* Calculate what line insertion or deletion to do, and do it,
   Copyright (C) 1985, 1986, 1990, 1993, 1994 Free Software Foundation, Inc.

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


#include <config.h>
#include "termchar.h"
#include "lisp.h"
#include "dispextern.h"
#include "frame.h"

extern struct display_line **ophys_lines;

#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))

/* All costs measured in characters.
   So no cost can exceed the area of a frame, measured in characters.
   Let's hope this is never more than 1000000 characters.  */

#define INFINITY 1000000

struct matrix_elt
  {
    /* Cost of outputting through this line
       if no insert/delete is done just above it.  */
    int writecost;
    /* Cost of outputting through this line
       if an insert is done just above it.  */
    int insertcost;
    /* Cost of outputting through this line
       if a delete is done just above it.  */
    int deletecost;
    /* Number of inserts so far in this run of inserts,
       for the cost in insertcost.  */
    unsigned char insertcount;
    /* Number of deletes so far in this run of deletes,
       for the cost in deletecost.  */
    unsigned char deletecount;
  };


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
   Note that these are not true frame vpos's, but relative
   to the place at which the first mismatch between old and
   new contents appears.  */

static void
calculate_scrolling (frame, matrix, window_size, lines_below,
		     draw_cost, old_hash, new_hash,
		     free_at_end)
     FRAME_PTR frame;
     /* matrix is of size window_size + 1 on each side.  */
     struct matrix_elt *matrix;
     int window_size;
     int *draw_cost;
     int *old_hash;
     int *new_hash;
     int free_at_end;
{
  register int i, j;
  int frame_height = FRAME_HEIGHT (frame);
  register struct matrix_elt *p, *p1;
  register int cost, cost1;

  int lines_moved = window_size + (scroll_region_ok ? 0 : lines_below);
  /* first_insert_cost[I] is the cost of doing the first insert-line
     at the I'th line of the lines we are considering,
     where I is origin 1 (as it is below).  */
  int *first_insert_cost
    = &FRAME_INSERT_COST (frame)[frame_height - 1 - lines_moved];
  int *first_delete_cost
    = &FRAME_DELETE_COST (frame)[frame_height - 1 - lines_moved];
  int *next_insert_cost
    = &FRAME_INSERTN_COST (frame)[frame_height - 1 - lines_moved];
  int *next_delete_cost
    = &FRAME_DELETEN_COST (frame)[frame_height - 1 - lines_moved];

  /* Discourage long scrolls on fast lines.
     Don't scroll nearly a full frame height unless it saves
     at least 1/4 second.  */
  int extra_cost = baud_rate / (10 * 4 * FRAME_HEIGHT (frame));

  if (baud_rate <= 0)
    extra_cost = 1;

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

  /* `i' represents the vpos among new frame contents.
     `j' represents the vpos among the old frame contents.  */
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
	    if ((int) p1->insertcount > i)
	      abort ();
	    cost1 = p1->insertcost + next_insert_cost[i - p1->insertcount];
	  }
	p->insertcost = min (cost, cost1) + draw_cost[i] + extra_cost;
	p->insertcount = (cost < cost1) ? 1 : p1->insertcount + 1;
	if ((int) p->insertcount > i)
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

/* Perform insert-lines and delete-lines operations on FRAME
   according to the costs in MATRIX.
   Update the frame's current_glyphs info to record what was done.

   WINDOW_SIZE is the number of lines being considered for scrolling
   and UNCHANGED_AT_TOP is the vpos of the first line being considered.
   These two arguments can specify any contiguous range of lines.
 
   We also shuffle the charstarts vectors for the lines
   along with the glyphs; but the results are not quite right,
   since we cannot offset them for changes in amount of text
   in this line or that line.  Luckily it doesn't matter,
   since update_frame and update_line will copy in the proper
   new charstarts vectors from the frame's desired_glyphs.  */

static void
do_scrolling (frame, matrix, window_size, unchanged_at_top)
     FRAME_PTR frame;
     struct matrix_elt *matrix;
     int window_size;
     int unchanged_at_top;
{
  register struct matrix_elt *p;
  register int i, j;
  register struct frame_glyphs *current_frame;
  /* temp_frame->enable[i] means line i has been moved to current_frame.  */
  register struct frame_glyphs *temp_frame;
  struct queue { int count, pos; } *queue;
  int offset = unchanged_at_top;
  int qi = 0;
  int window = 0;
  register int tem;
  int next;

  queue = (struct queue *) alloca (FRAME_HEIGHT (frame)
				   * sizeof (struct queue));

  current_frame = FRAME_CURRENT_GLYPHS (frame);
  temp_frame = FRAME_TEMP_GLYPHS (frame);

  bcopy (current_frame->glyphs, temp_frame->glyphs,
	 current_frame->height * sizeof (GLYPH *));
  bcopy (current_frame->charstarts, temp_frame->charstarts,
	 current_frame->height * sizeof (GLYPH *));
  bcopy (current_frame->used, temp_frame->used,
	 current_frame->height * sizeof (int));
  bcopy (current_frame->highlight, temp_frame->highlight,
	 current_frame->height * sizeof (char));
  bzero (temp_frame->enable, temp_frame->height * sizeof (char));
  bcopy (current_frame->bufp, temp_frame->bufp,
	 current_frame->height * sizeof (int));

#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (frame))
    {
      bcopy (current_frame->top_left_x, temp_frame->top_left_x,
	     current_frame->height * sizeof (short));
      bcopy (current_frame->top_left_y, temp_frame->top_left_y,
	     current_frame->height * sizeof (short));
      bcopy (current_frame->pix_width, temp_frame->pix_width,
	     current_frame->height * sizeof (short));
      bcopy (current_frame->pix_height, temp_frame->pix_height,
	     current_frame->height * sizeof (short));
      bcopy (current_frame->max_ascent, temp_frame->max_ascent,
	     current_frame->height * sizeof (short));
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
	  current_frame->glyphs[i + offset - 1]
	    = temp_frame->glyphs[j + offset - 1];
	  current_frame->charstarts[i + offset - 1]
	    = temp_frame->charstarts[j + offset - 1];
	  current_frame->used[i + offset - 1]
	    = temp_frame->used[j + offset - 1];
	  current_frame->highlight[i + offset - 1]
	    = temp_frame->highlight[j + offset - 1];

	  temp_frame->enable[j + offset - 1] = 1;
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
	 Those are the ones for which temp_frame->enable was not set.  */
      tem = queue[i].pos;
      for (j = tem + queue[i].count - 1; j >= tem; j--)
	{
	  current_frame->enable[j] = 0;
	  while (temp_frame->enable[next])
	    next++;
	  current_frame->glyphs[j] = temp_frame->glyphs[next];
	  current_frame->charstarts[j] = temp_frame->charstarts[next++];
	}
    }

  if (window)
    set_terminal_window (0);
}

void
scrolling_1 (frame, window_size, unchanged_at_top, unchanged_at_bottom,
	     draw_cost, old_hash, new_hash, free_at_end)
     FRAME_PTR frame;
     int window_size, unchanged_at_top, unchanged_at_bottom;
     int *draw_cost;
     int *old_hash;
     int *new_hash;
     int free_at_end;
{
  struct matrix_elt *matrix;
  matrix = ((struct matrix_elt *)
	    alloca ((window_size + 1) * (window_size + 1) * sizeof *matrix));

  calculate_scrolling (frame, matrix, window_size, unchanged_at_bottom,
		       draw_cost, old_hash, new_hash,
		       free_at_end);
  do_scrolling (frame, matrix, window_size, unchanged_at_top);
}

/* Return number of lines in common between current and desired frame contents
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
   scroll_frame_lines to perform this scrolling.  */

scroll_cost (frame, from, to, amount)
     FRAME_PTR frame;
     int from, to, amount;
{
  /* Compute how many lines, at bottom of frame,
     will not be involved in actual motion.  */
  int limit = to;
  int offset;
  int height = FRAME_HEIGHT (frame);

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
    (FRAME_INSERT_COST (frame)[offset + from]
     + (amount - 1) * FRAME_INSERTN_COST (frame)[offset + from]
     + FRAME_DELETE_COST (frame)[offset + to]
     + (amount - 1) * FRAME_DELETEN_COST (frame)[offset + to]);
}

/* Calculate the line insertion/deletion
   overhead and multiply factor values */

static void
line_ins_del (frame, ov1, pf1, ovn, pfn, ov, mf)
     FRAME_PTR frame;
     int ov1, ovn;
     int pf1, pfn;
     register int *ov, *mf;
{
  register int i;
  register int frame_height = FRAME_HEIGHT (frame);
  register int insert_overhead = ov1 * 10;
  register int next_insert_cost = ovn * 10;

  for (i = frame_height-1; i >= 0; i--)
    {
      mf[i] = next_insert_cost / 10;
      next_insert_cost += pfn;
      ov[i] = (insert_overhead + next_insert_cost) / 10;
      insert_overhead += pf1;
    }
}

static void
ins_del_costs (frame,
	       one_line_string, multi_string,
	       setup_string, cleanup_string,
	       costvec, ncostvec, coefficient)
     FRAME_PTR frame;
     char *one_line_string, *multi_string;
     char *setup_string, *cleanup_string;
     int *costvec, *ncostvec;
     int coefficient;
{
  if (multi_string)
    line_ins_del (frame,
		  string_cost (multi_string) * coefficient,
		  per_line_cost (multi_string) * coefficient,
		  0, 0, costvec, ncostvec);
  else if (one_line_string)
    line_ins_del (frame,
		  string_cost (setup_string) + string_cost (cleanup_string), 0,
		  string_cost (one_line_string),
		  per_line_cost (one_line_string),
		  costvec, ncostvec);
  else
    line_ins_del (frame,
		  9999, 0, 9999, 0,
		  costvec, ncostvec);
}

/* Calculate the insert and delete line costs.
   Note that this is done even when running with a window system
   because we want to know how long scrolling takes (and avoid it).
   This must be redone whenever the frame height changes.

   We keep the ID costs in a precomputed array based on the position
   at which the I or D is performed.  Also, there are two kinds of ID
   costs: the "once-only" and the "repeated".  This is to handle both
   those terminals that are able to insert N lines at a time (once-
   only) and those that must repeatedly insert one line.

   The cost to insert N lines at line L is
   	    [tt.t_ILov  + (frame_height + 1 - L) * tt.t_ILpf] +
	N * [tt.t_ILnov + (frame_height + 1 - L) * tt.t_ILnpf]

   ILov represents the basic insert line overhead.  ILpf is the padding
   required to allow the terminal time to move a line: insertion at line
   L changes (frame_height + 1 - L) lines.

   The first bracketed expression above is the overhead; the second is
   the multiply factor.  Both are dependent only on the position at
   which the insert is performed.  We store the overhead in
   FRAME_INSERT_COST (frame) and the multiply factor in
   FRAME_INSERTN_COST (frame).  Note however that any insertion
   must include at least one multiply factor.  Rather than compute this
   as FRAME_INSERT_COST (frame)[line]+FRAME_INSERTN_COST (frame)[line],
   we add FRAME_INSERTN_COST (frame) into FRAME_INSERT_COST (frame).
   This is reasonable because of the particular algorithm used in calcM.

   Deletion is essentially the same as insertion.
 */

do_line_insertion_deletion_costs (frame,
				  ins_line_string, multi_ins_string,
				  del_line_string, multi_del_string,
				  setup_string, cleanup_string, coefficient)
     FRAME_PTR frame;
     char *ins_line_string, *multi_ins_string;
     char *del_line_string, *multi_del_string;
     char *setup_string, *cleanup_string;
     int coefficient;
{
  if (FRAME_INSERT_COST (frame) != 0)
    {
      FRAME_INSERT_COST (frame) =
	(int *) xrealloc (FRAME_INSERT_COST (frame),
			  FRAME_HEIGHT (frame) * sizeof (int));
      FRAME_DELETEN_COST (frame) =
	(int *) xrealloc (FRAME_DELETEN_COST (frame),
			  FRAME_HEIGHT (frame) * sizeof (int));
      FRAME_INSERTN_COST (frame) =
	(int *) xrealloc (FRAME_INSERTN_COST (frame),
			  FRAME_HEIGHT (frame) * sizeof (int));
      FRAME_DELETE_COST (frame) =
	(int *) xrealloc (FRAME_DELETE_COST (frame),
			  FRAME_HEIGHT (frame) * sizeof (int));
    }
  else
    {
      FRAME_INSERT_COST (frame) =
	(int *) xmalloc (FRAME_HEIGHT (frame) * sizeof (int));
      FRAME_DELETEN_COST (frame) =
	(int *) xmalloc (FRAME_HEIGHT (frame) * sizeof (int));
      FRAME_INSERTN_COST (frame) =
	(int *) xmalloc (FRAME_HEIGHT (frame) * sizeof (int));
      FRAME_DELETE_COST (frame) = 
	(int *) xmalloc (FRAME_HEIGHT (frame) * sizeof (int));
    }

  ins_del_costs (frame,
		 ins_line_string, multi_ins_string,
		 setup_string, cleanup_string,
		 FRAME_INSERT_COST (frame), FRAME_INSERTN_COST (frame),
		 coefficient);
  ins_del_costs (frame,
		 del_line_string, multi_del_string,
		 setup_string, cleanup_string,
		 FRAME_DELETE_COST (frame), FRAME_DELETEN_COST (frame),
		 coefficient);
}
