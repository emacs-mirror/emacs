/* Code for doing intervals.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

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


/* NOTES:

   Have to ensure that we can't put symbol nil on a plist, or some
   functions may work incorrectly.

   An idea:  Have the owner of the tree keep count of splits and/or
   insertion lengths (in intervals), and balance after every N.

   Need to call *_left_hook when buffer is killed.

   Scan for zero-length, or 0-length to see notes about handling
   zero length interval-markers.

   There are comments around about freeing intervals.  It might be
   faster to explicitly free them (put them on the free list) than
   to GC them.

*/


#include <config.h>
#include "lisp.h"
#include "intervals.h"
#include "buffer.h"
#include "puresize.h"
#include "keyboard.h"

/* The rest of the file is within this conditional.  */
#ifdef USE_TEXT_PROPERTIES

/* Test for membership, allowing for t (actually any non-cons) to mean the
   universal set.  */

#define TMEM(sym, set) (CONSP (set) ? ! NILP (Fmemq (sym, set)) : ! NILP (set))

#define min(x, y) ((x) < (y) ? (x) : (y))

Lisp_Object merge_properties_sticky ();

/* Utility functions for intervals.  */


/* Create the root interval of some object, a buffer or string.  */

INTERVAL
create_root_interval (parent)
     Lisp_Object parent;
{
  INTERVAL new;

  CHECK_IMPURE (parent);

  new = make_interval ();

  if (BUFFERP (parent))
    {
      new->total_length = (BUF_Z (XBUFFER (parent))
			   - BUF_BEG (XBUFFER (parent)));
      BUF_INTERVALS (XBUFFER (parent)) = new;
    }
  else if (STRINGP (parent))
    {
      new->total_length = XSTRING (parent)->size;
      XSTRING (parent)->intervals = new;
    }

  new->parent = (INTERVAL) parent;
  new->position = 1;

  return new;
}

/* Make the interval TARGET have exactly the properties of SOURCE */

void
copy_properties (source, target)
     register INTERVAL source, target;
{
  if (DEFAULT_INTERVAL_P (source) && DEFAULT_INTERVAL_P (target))
    return;

  COPY_INTERVAL_CACHE (source, target);
  target->plist = Fcopy_sequence (source->plist);
}

/* Merge the properties of interval SOURCE into the properties
   of interval TARGET.  That is to say, each property in SOURCE
   is added to TARGET if TARGET has no such property as yet.  */

static void
merge_properties (source, target)
     register INTERVAL source, target;
{
  register Lisp_Object o, sym, val;

  if (DEFAULT_INTERVAL_P (source) && DEFAULT_INTERVAL_P (target))
    return;

  MERGE_INTERVAL_CACHE (source, target);

  o = source->plist;
  while (! EQ (o, Qnil))
    {
      sym = Fcar (o);
      val = Fmemq (sym, target->plist);

      if (NILP (val))
	{
	  o = Fcdr (o);
	  val = Fcar (o);
	  target->plist = Fcons (sym, Fcons (val, target->plist));
	  o = Fcdr (o);
	}
      else
	o = Fcdr (Fcdr (o));
    }
}

/* Return 1 if the two intervals have the same properties,
   0 otherwise.  */

int
intervals_equal (i0, i1)
     INTERVAL i0, i1;
{
  register Lisp_Object i0_cdr, i0_sym, i1_val;
  register i1_len;

  if (DEFAULT_INTERVAL_P (i0) && DEFAULT_INTERVAL_P (i1))
    return 1;

  if (DEFAULT_INTERVAL_P (i0) || DEFAULT_INTERVAL_P (i1))
    return 0;

  i1_len = XFASTINT (Flength (i1->plist));
  if (i1_len & 0x1)		/* Paranoia -- plists are always even */
    abort ();
  i1_len /= 2;
  i0_cdr = i0->plist;
  while (!NILP (i0_cdr))
    {
      /* Lengths of the two plists were unequal.  */
      if (i1_len == 0)
	return 0;

      i0_sym = Fcar (i0_cdr);
      i1_val = Fmemq (i0_sym, i1->plist);

      /* i0 has something i1 doesn't.  */
      if (EQ (i1_val, Qnil))
	return 0;

      /* i0 and i1 both have sym, but it has different values in each.  */
      i0_cdr = Fcdr (i0_cdr);
      if (! EQ (Fcar (Fcdr (i1_val)), Fcar (i0_cdr)))
	return 0;

      i0_cdr = Fcdr (i0_cdr);
      i1_len--;
    }

  /* Lengths of the two plists were unequal.  */
  if (i1_len > 0)
    return 0;

  return 1;
}

static int icount;
static int idepth;
static int zero_length;

/* Traverse an interval tree TREE, performing FUNCTION on each node.
   Pass FUNCTION two args: an interval, and ARG.  */

void
traverse_intervals (tree, position, depth, function, arg)
     INTERVAL tree;
     int position, depth;
     void (* function) ();
     Lisp_Object arg;
{
  if (NULL_INTERVAL_P (tree))
    return;

  traverse_intervals (tree->left, position, depth + 1, function, arg);
  position += LEFT_TOTAL_LENGTH (tree);
  tree->position = position;
  (*function) (tree, arg);
  position += LENGTH (tree);
  traverse_intervals (tree->right, position, depth + 1,  function, arg);
}

#if 0
/* These functions are temporary, for debugging purposes only.  */

INTERVAL search_interval, found_interval;

void
check_for_interval (i)
     register INTERVAL i;
{
  if (i == search_interval)
    {
      found_interval = i;
      icount++;
    }
}

INTERVAL
search_for_interval (i, tree)
     register INTERVAL i, tree;
{
  icount = 0;
  search_interval = i;
  found_interval = NULL_INTERVAL;
  traverse_intervals (tree, 1, 0, &check_for_interval, Qnil);
  return found_interval;
}

static void
inc_interval_count (i)
     INTERVAL i;
{
  icount++;
  if (LENGTH (i) == 0)
    zero_length++;
  if (depth > idepth)
    idepth = depth;
}

int
count_intervals (i)
     register INTERVAL i;
{
  icount = 0;
  idepth = 0;
  zero_length = 0;
  traverse_intervals (i, 1, 0, &inc_interval_count, Qnil);

  return icount;
}

static INTERVAL
root_interval (interval)
     INTERVAL interval;
{
  register INTERVAL i = interval;

  while (! ROOT_INTERVAL_P (i))
    i = i->parent;

  return i;
}
#endif

/* Assuming that a left child exists, perform the following operation:

     A		  B
    / \		 / \
   B       =>       A
  / \		   / \
     c		  c
*/

static INTERVAL
rotate_right (interval)
     INTERVAL interval;
{
  INTERVAL i;
  INTERVAL B = interval->left;
  int old_total = interval->total_length;

  /* Deal with any Parent of A;  make it point to B.  */
  if (! ROOT_INTERVAL_P (interval))
    if (AM_LEFT_CHILD (interval))
      interval->parent->left = B;
    else
      interval->parent->right = B;
  B->parent = interval->parent;

  /* Make B the parent of A */
  i = B->right;
  B->right = interval;
  interval->parent = B;

  /* Make A point to c */
  interval->left = i;
  if (! NULL_INTERVAL_P (i))
    i->parent = interval;

  /* A's total length is decreased by the length of B and its left child.  */
  interval->total_length -= B->total_length - LEFT_TOTAL_LENGTH (interval);

  /* B must have the same total length of A.  */
  B->total_length = old_total;

  return B;
}

/* Assuming that a right child exists, perform the following operation:

    A               B   
   / \	           / \  
      B	   =>     A
     / \         / \    
    c               c
*/

static INTERVAL
rotate_left (interval)
     INTERVAL interval;
{
  INTERVAL i;
  INTERVAL B = interval->right;
  int old_total = interval->total_length;

  /* Deal with any parent of A;  make it point to B.  */
  if (! ROOT_INTERVAL_P (interval))
    if (AM_LEFT_CHILD (interval))
      interval->parent->left = B;
    else
      interval->parent->right = B;
  B->parent = interval->parent;

  /* Make B the parent of A */
  i = B->left;
  B->left = interval;
  interval->parent = B;

  /* Make A point to c */
  interval->right = i;
  if (! NULL_INTERVAL_P (i))
    i->parent = interval;

  /* A's total length is decreased by the length of B and its right child.  */
  interval->total_length -= B->total_length - RIGHT_TOTAL_LENGTH (interval);

  /* B must have the same total length of A.  */
  B->total_length = old_total;

  return B;
}

/* Balance an interval tree with the assumption that the subtrees
   themselves are already balanced.  */

static INTERVAL
balance_an_interval (i)
     INTERVAL i;
{
  register int old_diff, new_diff;

  while (1)
    {
      old_diff = LEFT_TOTAL_LENGTH (i) - RIGHT_TOTAL_LENGTH (i);
      if (old_diff > 0)
	{
	  new_diff = i->total_length - i->left->total_length
	    + RIGHT_TOTAL_LENGTH (i->left) - LEFT_TOTAL_LENGTH (i->left);
	  if (abs (new_diff) >= old_diff)
	    break;
	  i = rotate_right (i);
	  balance_an_interval (i->right);
	}
      else if (old_diff < 0)
	{
	  new_diff = i->total_length - i->right->total_length
	    + LEFT_TOTAL_LENGTH (i->right) - RIGHT_TOTAL_LENGTH (i->right);
	  if (abs (new_diff) >= -old_diff)
	    break;
	  i = rotate_left (i);
	  balance_an_interval (i->left);
	}
      else
	break;
    }
  return i;
}

/* Balance INTERVAL, potentially stuffing it back into its parent
   Lisp Object.  */

static INLINE INTERVAL
balance_possible_root_interval (interval)
     register INTERVAL interval;
{
  Lisp_Object parent;

  if (interval->parent == NULL_INTERVAL)
    return interval;

  parent = (Lisp_Object) (interval->parent);
  interval = balance_an_interval (interval);

  if (BUFFERP (parent))
    BUF_INTERVALS (XBUFFER (parent)) = interval;
  else if (STRINGP (parent))
    XSTRING (parent)->intervals = interval;

  return interval;
}

/* Balance the interval tree TREE.  Balancing is by weight
   (the amount of text).  */

static INTERVAL
balance_intervals_internal (tree)
     register INTERVAL tree;
{
  /* Balance within each side.  */
  if (tree->left)
    balance_intervals (tree->left);
  if (tree->right)
    balance_intervals (tree->right);
  return balance_an_interval (tree);
}

/* Advertised interface to balance intervals.  */

INTERVAL
balance_intervals (tree)
     INTERVAL tree;
{
  if (tree == NULL_INTERVAL)
    return NULL_INTERVAL;

  return balance_intervals_internal (tree);
}

/* Split INTERVAL into two pieces, starting the second piece at
   character position OFFSET (counting from 0), relative to INTERVAL.
   INTERVAL becomes the left-hand piece, and the right-hand piece
   (second, lexicographically) is returned.

   The size and position fields of the two intervals are set based upon
   those of the original interval.  The property list of the new interval
   is reset, thus it is up to the caller to do the right thing with the
   result.

   Note that this does not change the position of INTERVAL;  if it is a root,
   it is still a root after this operation.  */

INTERVAL
split_interval_right (interval, offset)
     INTERVAL interval;
     int offset;
{
  INTERVAL new = make_interval ();
  int position = interval->position;
  int new_length = LENGTH (interval) - offset;

  new->position = position + offset;
  new->parent = interval;

  if (NULL_RIGHT_CHILD (interval))
    {
      interval->right = new;
      new->total_length = new_length;

      return new;
    }

  /* Insert the new node between INTERVAL and its right child.  */
  new->right = interval->right;
  interval->right->parent = new;
  interval->right = new;
  new->total_length = new_length + new->right->total_length;

  balance_an_interval (new);
  balance_possible_root_interval (interval);

  return new;
}

/* Split INTERVAL into two pieces, starting the second piece at
   character position OFFSET (counting from 0), relative to INTERVAL.
   INTERVAL becomes the right-hand piece, and the left-hand piece
   (first, lexicographically) is returned.

   The size and position fields of the two intervals are set based upon
   those of the original interval.  The property list of the new interval
   is reset, thus it is up to the caller to do the right thing with the
   result.

   Note that this does not change the position of INTERVAL;  if it is a root,
   it is still a root after this operation.  */

INTERVAL
split_interval_left (interval, offset)
     INTERVAL interval;
     int offset;
{
  INTERVAL new = make_interval ();
  int position = interval->position;
  int new_length = offset;

  new->position = interval->position;
  interval->position = interval->position + offset;
  new->parent = interval;

  if (NULL_LEFT_CHILD (interval))
    {
      interval->left = new;
      new->total_length = new_length;

      return new;
    }

  /* Insert the new node between INTERVAL and its left child.  */
  new->left = interval->left;
  new->left->parent = new;
  interval->left = new;
  new->total_length = new_length + new->left->total_length;

  balance_an_interval (new);
  balance_possible_root_interval (interval);

  return new;
}

/* Find the interval containing text position POSITION in the text
   represented by the interval tree TREE.  POSITION is a buffer
   position; the earliest position is 1.  If POSITION is at the end of
   the buffer, return the interval containing the last character.

   The `position' field, which is a cache of an interval's position,
   is updated in the interval found.  Other functions (e.g., next_interval)
   will update this cache based on the result of find_interval.  */

INLINE INTERVAL
find_interval (tree, position)
     register INTERVAL tree;
     register int position;
{
  /* The distance from the left edge of the subtree at TREE
                    to POSITION.  */
  register int relative_position = position - BEG;

  if (NULL_INTERVAL_P (tree))
    return NULL_INTERVAL;

  if (relative_position > TOTAL_LENGTH (tree))
    abort ();			/* Paranoia */

  tree = balance_possible_root_interval (tree);

  while (1)
    {
      if (relative_position < LEFT_TOTAL_LENGTH (tree))
	{
	  tree = tree->left;
	}
      else if (! NULL_RIGHT_CHILD (tree)
	       && relative_position >= (TOTAL_LENGTH (tree)
					- RIGHT_TOTAL_LENGTH (tree)))
	{
	  relative_position -= (TOTAL_LENGTH (tree)
				- RIGHT_TOTAL_LENGTH (tree));
	  tree = tree->right;
	}
      else
	{
	  tree->position =
	    (position - relative_position /* the left edge of *tree */
	     + LEFT_TOTAL_LENGTH (tree)); /* the left edge of this interval */

	  return tree;
	}
    }
}

/* Find the succeeding interval (lexicographically) to INTERVAL.
   Sets the `position' field based on that of INTERVAL (see
   find_interval).  */

INTERVAL
next_interval (interval)
     register INTERVAL interval;
{
  register INTERVAL i = interval;
  register int next_position;

  if (NULL_INTERVAL_P (i))
    return NULL_INTERVAL;
  next_position = interval->position + LENGTH (interval);

  if (! NULL_RIGHT_CHILD (i))
    {
      i = i->right;
      while (! NULL_LEFT_CHILD (i))
	i = i->left;

      i->position = next_position;
      return i;
    }

  while (! NULL_PARENT (i))
    {
      if (AM_LEFT_CHILD (i))
	{
	  i = i->parent;
	  i->position = next_position;
	  return i;
	}

      i = i->parent;
    }

  return NULL_INTERVAL;
}

/* Find the preceding interval (lexicographically) to INTERVAL.
   Sets the `position' field based on that of INTERVAL (see
   find_interval).  */

INTERVAL
previous_interval (interval)
     register INTERVAL interval;
{
  register INTERVAL i;
  register position_of_previous;

  if (NULL_INTERVAL_P (interval))
    return NULL_INTERVAL;

  if (! NULL_LEFT_CHILD (interval))
    {
      i = interval->left;
      while (! NULL_RIGHT_CHILD (i))
	i = i->right;

      i->position = interval->position - LENGTH (i);
      return i;
    }

  i = interval;
  while (! NULL_PARENT (i))
    {
      if (AM_RIGHT_CHILD (i))
	{
	  i = i->parent;

	  i->position = interval->position - LENGTH (i);
	  return i;
	}
      i = i->parent;
    }

  return NULL_INTERVAL;
}

#if 0
/* Traverse a path down the interval tree TREE to the interval
   containing POSITION, adjusting all nodes on the path for
   an addition of LENGTH characters.  Insertion between two intervals
   (i.e., point == i->position, where i is second interval) means
   text goes into second interval.

   Modifications are needed to handle the hungry bits -- after simply
   finding the interval at position (don't add length going down),
   if it's the beginning of the interval, get the previous interval
   and check the hugry bits of both.  Then add the length going back up
   to the root.  */

static INTERVAL
adjust_intervals_for_insertion (tree, position, length)
     INTERVAL tree;
     int position, length;
{
  register int relative_position;
  register INTERVAL this;

  if (TOTAL_LENGTH (tree) == 0)	/* Paranoia */
    abort ();

  /* If inserting at point-max of a buffer, that position
     will be out of range */
  if (position > TOTAL_LENGTH (tree))
    position = TOTAL_LENGTH (tree);
  relative_position = position;
  this = tree;

  while (1)
    {
      if (relative_position <= LEFT_TOTAL_LENGTH (this))
	{
	  this->total_length += length;
	  this = this->left;
	}
      else if (relative_position > (TOTAL_LENGTH (this)
				    - RIGHT_TOTAL_LENGTH (this)))
	{
	  relative_position -= (TOTAL_LENGTH (this)
				- RIGHT_TOTAL_LENGTH (this));
	  this->total_length += length;
	  this = this->right;
	}
      else
	{
	  /* If we are to use zero-length intervals as buffer pointers,
	     then this code will have to change.  */
	  this->total_length += length;
	  this->position = LEFT_TOTAL_LENGTH (this)
	                   + position - relative_position + 1;
	  return tree;
	}
    }
}
#endif

/* Effect an adjustment corresponding to the addition of LENGTH characters
   of text.  Do this by finding the interval containing POSITION in the
   interval tree TREE, and then adjusting all of its ancestors by adding
   LENGTH to them.

   If POSITION is the first character of an interval, meaning that point
   is actually between the two intervals, make the new text belong to
   the interval which is "sticky".

   If both intervals are "sticky", then make them belong to the left-most
   interval.  Another possibility would be to create a new interval for
   this text, and make it have the merged properties of both ends.  */

static INTERVAL
adjust_intervals_for_insertion (tree, position, length)
     INTERVAL tree;
     int position, length;
{
  register INTERVAL i;
  register INTERVAL temp;
  int eobp = 0;
  
  if (TOTAL_LENGTH (tree) == 0)	/* Paranoia */
    abort ();

  /* If inserting at point-max of a buffer, that position will be out
     of range.  Remember that buffer positions are 1-based.  */
  if (position >= BEG + TOTAL_LENGTH (tree)){
    position = BEG + TOTAL_LENGTH (tree);
    eobp = 1;
  }

  i = find_interval (tree, position);

  /* If in middle of an interval which is not sticky either way,
     we must not just give its properties to the insertion.
     So split this interval at the insertion point.  */
  if (! (position == i->position || eobp)
      && END_NONSTICKY_P (i)
      && ! FRONT_STICKY_P (i))
    {
      temp = split_interval_right (i, position - i->position);
      copy_properties (i, temp);
      i = temp;
    }

  /* If we are positioned between intervals, check the stickiness of
     both of them.  We have to do this too, if we are at BEG or Z.  */
  if (position == i->position || eobp)
    {
      register INTERVAL prev;

      if (position == BEG)
	prev = 0;
      else if (eobp)
	{
	  prev = i;
	  i = 0;
	}
      else
	prev = previous_interval (i);

      /* Even if we are positioned between intervals, we default
	 to the left one if it exists.  We extend it now and split
	 off a part later, if stickyness demands it.  */
      for (temp = prev ? prev : i;! NULL_INTERVAL_P (temp); temp = temp->parent)
	{
	  temp->total_length += length;
	  temp = balance_possible_root_interval (temp);
	}
      
      /* If at least one interval has sticky properties,
	 we check the stickyness property by property.  */
      if (END_NONSTICKY_P (prev) || FRONT_STICKY_P (i))
	{
	  Lisp_Object pleft, pright;
	  struct interval newi;

	  pleft = NULL_INTERVAL_P (prev) ? Qnil : prev->plist;
	  pright = NULL_INTERVAL_P (i) ? Qnil : i->plist;
	  newi.plist = merge_properties_sticky (pleft, pright);

	  if(! prev) /* i.e. position == BEG */
	    {
	      if (! intervals_equal (i, &newi))
		{
		  i = split_interval_left (i, length);
		  i->plist = newi.plist;
		}
	    }
	  else if (! intervals_equal (prev, &newi))
	    {
	      prev = split_interval_right (prev,
					   position - prev->position);
	      prev->plist = newi.plist;
	      if (! NULL_INTERVAL_P (i)
		  && intervals_equal (prev, i))
		merge_interval_right (prev);
	    }

	  /* We will need to update the cache here later.  */
	}
      else if (! prev && ! NILP (i->plist))
        {
	  /* Just split off a new interval at the left.
	     Since I wasn't front-sticky, the empty plist is ok.  */
	  i = split_interval_left (i, length);
        }
    }

  /* Otherwise just extend the interval.  */
  else
    {
      for (temp = i; ! NULL_INTERVAL_P (temp); temp = temp->parent)
	{
	  temp->total_length += length;
	  temp = balance_possible_root_interval (temp);
	}
    }
      
  return tree;
}

/* Any property might be front-sticky on the left, rear-sticky on the left,
   front-sticky on the right, or rear-sticky on the right; the 16 combinations
   can be arranged in a matrix with rows denoting the left conditions and
   columns denoting the right conditions:
      _  __  _
_     FR FR FR FR
FR__   0  1  2  3
 _FR   4  5  6  7
FR     8  9  A  B
  FR   C  D  E  F

   left-props  = '(front-sticky (p8 p9 pa pb pc pd pe pf)
		   rear-nonsticky (p4 p5 p6 p7 p8 p9 pa pb)
		   p0 L p1 L p2 L p3 L p4 L p5 L p6 L p7 L
		   p8 L p9 L pa L pb L pc L pd L pe L pf L)
   right-props = '(front-sticky (p2 p3 p6 p7 pa pb pe pf)
		   rear-nonsticky (p1 p2 p5 p6 p9 pa pd pe)
		   p0 R p1 R p2 R p3 R p4 R p5 R p6 R p7 R
		   p8 R p9 R pa R pb R pc R pd R pe R pf R)

   We inherit from whoever has a sticky side facing us.  If both sides
   do (cases 2, 3, E, and F), then we inherit from whichever side has a
   non-nil value for the current property.  If both sides do, then we take
   from the left.

   When we inherit a property, we get its stickiness as well as its value.
   So, when we merge the above two lists, we expect to get this:

   result      = '(front-sticky (p6 p7 pa pb pc pd pe pf)
		   rear-nonsticky (p6 pa)
		   p0 L p1 L p2 L p3 L p6 R p7 R
		   pa R pb R pc L pd L pe L pf L)

   The optimizable special cases are:
       left rear-nonsticky = nil, right front-sticky = nil (inherit left)
       left rear-nonsticky = t,   right front-sticky = t   (inherit right)
       left rear-nonsticky = t,   right front-sticky = nil (inherit none)
*/

Lisp_Object
merge_properties_sticky (pleft, pright)
     Lisp_Object pleft, pright;
{
  register Lisp_Object props, front, rear;
  Lisp_Object lfront, lrear, rfront, rrear;
  register Lisp_Object tail1, tail2, sym, lval, rval;
  int use_left, use_right;

  props = Qnil;
  front = Qnil;
  rear  = Qnil;
  lfront = textget (pleft, Qfront_sticky);
  lrear  = textget (pleft, Qrear_nonsticky);
  rfront = textget (pright, Qfront_sticky);
  rrear  = textget (pright, Qrear_nonsticky);

  /* Go through each element of PRIGHT.  */
  for (tail1 = pright; ! NILP (tail1); tail1 = Fcdr (Fcdr (tail1)))
    {
      sym = Fcar (tail1);

      /* Sticky properties get special treatment.  */
      if (EQ (sym, Qrear_nonsticky) || EQ (sym, Qfront_sticky))
	continue;

      rval = Fcar (Fcdr (tail1));
      for (tail2 = pleft; ! NILP (tail2); tail2 = Fcdr (Fcdr (tail2)))
	if (EQ (sym, Fcar (tail2)))
	  break;
      lval = (NILP (tail2) ? Qnil : Fcar( Fcdr (tail2)));

      use_left = ! TMEM (sym, lrear);
      use_right = TMEM (sym, rfront);
      if (use_left && use_right)
	{
	  use_left = ! NILP (lval);
	  use_right = ! NILP (rval);
	}
      if (use_left)
	{
	  /* We build props as (value sym ...) rather than (sym value ...)
	     because we plan to nreverse it when we're done.  */
	  if (! NILP (lval))
	    props = Fcons (lval, Fcons (sym, props));
	  if (TMEM (sym, lfront))
	    front = Fcons (sym, front);
	  if (TMEM (sym, lrear))
	    rear = Fcons (sym, rear);
	}
      else if (use_right)
	{
	  if (! NILP (rval))
	    props = Fcons (rval, Fcons (sym, props));
	  if (TMEM (sym, rfront))
	    front = Fcons (sym, front);
	  if (TMEM (sym, rrear))
	    rear = Fcons (sym, rear);
	}
    }

  /* Now go through each element of PLEFT.  */
  for (tail2 = pleft; ! NILP (tail2); tail2 = Fcdr (Fcdr (tail2)))
    {
      sym = Fcar (tail2);

      /* Sticky properties get special treatment.  */
      if (EQ (sym, Qrear_nonsticky) || EQ (sym, Qfront_sticky))
	continue;

      /* If sym is in PRIGHT, we've already considered it.  */
      for (tail1 = pright; ! NILP (tail1); tail1 = Fcdr (Fcdr (tail1)))
	if (EQ (sym, Fcar (tail1)))
	  break;
      if (! NILP (tail1))
	continue;

      lval = Fcar (Fcdr (tail2));

      /* Since rval is known to be nil in this loop, the test simplifies.  */
      if (! TMEM (sym, lrear))
	{
	  if (! NILP (lval))
	    props = Fcons (lval, Fcons (sym, props));
	  if (TMEM (sym, lfront))
	    front = Fcons (sym, front);
	}
      else if (TMEM (sym, rfront))
	{
	  /* The value is nil, but we still inherit the stickiness
	     from the right.  */
	  front = Fcons (sym, front);
	  if (TMEM (sym, rrear))
	    rear = Fcons (sym, rear);
	}
    }
  props = Fnreverse (props);
  if (! NILP (rear))
    props = Fcons (Qrear_nonsticky, Fcons (Fnreverse (rear), props));
  if (! NILP (front))
    props = Fcons (Qfront_sticky, Fcons (Fnreverse (front), props));
  return props;
}


/* Delete an node I from its interval tree by merging its subtrees
   into one subtree which is then returned.  Caller is responsible for
   storing the resulting subtree into its parent.  */

static INTERVAL
delete_node (i)
     register INTERVAL i;
{
  register INTERVAL migrate, this;
  register int migrate_amt;

  if (NULL_INTERVAL_P (i->left))
    return i->right;
  if (NULL_INTERVAL_P (i->right))
    return i->left;

  migrate = i->left;
  migrate_amt = i->left->total_length;
  this = i->right;
  this->total_length += migrate_amt;
  while (! NULL_INTERVAL_P (this->left))
    {
      this = this->left;
      this->total_length += migrate_amt;
    }
  this->left = migrate;
  migrate->parent = this;

  return i->right;
}

/* Delete interval I from its tree by calling `delete_node'
   and properly connecting the resultant subtree.

   I is presumed to be empty; that is, no adjustments are made
   for the length of I.  */

void
delete_interval (i)
     register INTERVAL i;
{
  register INTERVAL parent;
  int amt = LENGTH (i);

  if (amt > 0)			/* Only used on zero-length intervals now.  */
    abort ();

  if (ROOT_INTERVAL_P (i))
    {
      Lisp_Object owner;
      owner = (Lisp_Object) i->parent;
      parent = delete_node (i);
      if (! NULL_INTERVAL_P (parent))
	parent->parent = (INTERVAL) owner;

      if (BUFFERP (owner))
	BUF_INTERVALS (XBUFFER (owner)) = parent;
      else if (STRINGP (owner))
	XSTRING (owner)->intervals = parent;
      else
	abort ();

      return;
    }

  parent = i->parent;
  if (AM_LEFT_CHILD (i))
    {
      parent->left = delete_node (i);
      if (! NULL_INTERVAL_P (parent->left))
	parent->left->parent = parent;
    }
  else
    {
      parent->right = delete_node (i);
      if (! NULL_INTERVAL_P (parent->right))
	parent->right->parent = parent;
    }
}

/* Find the interval in TREE corresponding to the relative position
   FROM and delete as much as possible of AMOUNT from that interval.
   Return the amount actually deleted, and if the interval was
   zeroed-out, delete that interval node from the tree.

   Note that FROM is actually origin zero, aka relative to the
   leftmost edge of tree.  This is appropriate since we call ourselves
   recursively on subtrees.

   Do this by recursing down TREE to the interval in question, and
   deleting the appropriate amount of text.  */

static int
interval_deletion_adjustment (tree, from, amount)
     register INTERVAL tree;
     register int from, amount;
{
  register int relative_position = from;

  if (NULL_INTERVAL_P (tree))
    return 0;

  /* Left branch */
  if (relative_position < LEFT_TOTAL_LENGTH (tree))
    {
      int subtract = interval_deletion_adjustment (tree->left,
						   relative_position,
						   amount);
      tree->total_length -= subtract;
      return subtract;
    }
  /* Right branch */
  else if (relative_position >= (TOTAL_LENGTH (tree)
				 - RIGHT_TOTAL_LENGTH (tree)))
    {
      int subtract;

      relative_position -= (tree->total_length
			    - RIGHT_TOTAL_LENGTH (tree));
      subtract = interval_deletion_adjustment (tree->right,
					       relative_position,
					       amount);
      tree->total_length -= subtract;
      return subtract;
    }
  /* Here -- this node.  */
  else
    {
      /* How much can we delete from this interval?  */
      int my_amount = ((tree->total_length 
			- RIGHT_TOTAL_LENGTH (tree))
		       - relative_position);

      if (amount > my_amount)
	amount = my_amount;

      tree->total_length -= amount;
      if (LENGTH (tree) == 0)
	delete_interval (tree);
      
      return amount;
    }

  /* Never reach here.  */
}

/* Effect the adjustments necessary to the interval tree of BUFFER to
   correspond to the deletion of LENGTH characters from that buffer
   text.  The deletion is effected at position START (which is a
   buffer position, i.e. origin 1).  */

static void
adjust_intervals_for_deletion (buffer, start, length)
     struct buffer *buffer;
     int start, length;
{
  register int left_to_delete = length;
  register INTERVAL tree = BUF_INTERVALS (buffer);
  register int deleted;

  if (NULL_INTERVAL_P (tree))
    return;

  if (start > BEG + TOTAL_LENGTH (tree)
      || start + length > BEG + TOTAL_LENGTH (tree))
    abort ();

  if (length == TOTAL_LENGTH (tree))
    {
      BUF_INTERVALS (buffer) = NULL_INTERVAL;
      return;
    }

  if (ONLY_INTERVAL_P (tree))
    {
      tree->total_length -= length;
      return;
    }

  if (start > BEG + TOTAL_LENGTH (tree))
    start = BEG + TOTAL_LENGTH (tree);
  while (left_to_delete > 0)
    {
      left_to_delete -= interval_deletion_adjustment (tree, start - 1,
						      left_to_delete);
      tree = BUF_INTERVALS (buffer);
      if (left_to_delete == tree->total_length)
	{
	  BUF_INTERVALS (buffer) = NULL_INTERVAL;
	  return;
	}
    }
}

/* Make the adjustments necessary to the interval tree of BUFFER to
   represent an addition or deletion of LENGTH characters starting
   at position START.  Addition or deletion is indicated by the sign
   of LENGTH.  */

INLINE void
offset_intervals (buffer, start, length)
     struct buffer *buffer;
     int start, length;
{
  if (NULL_INTERVAL_P (BUF_INTERVALS (buffer)) || length == 0)
    return;

  if (length > 0)
    adjust_intervals_for_insertion (BUF_INTERVALS (buffer), start, length);
  else
    adjust_intervals_for_deletion (buffer, start, -length);
}

/* Merge interval I with its lexicographic successor. The resulting
   interval is returned, and has the properties of the original
   successor.  The properties of I are lost.  I is removed from the
   interval tree.

   IMPORTANT:
   The caller must verify that this is not the last (rightmost)
   interval.  */

INTERVAL
merge_interval_right (i)
     register INTERVAL i;
{
  register int absorb = LENGTH (i);
  register INTERVAL successor;

  /* Zero out this interval.  */
  i->total_length -= absorb;

  /* Find the succeeding interval.  */
  if (! NULL_RIGHT_CHILD (i))      /* It's below us.  Add absorb
				      as we descend.  */
    {
      successor = i->right;
      while (! NULL_LEFT_CHILD (successor))
	{
	  successor->total_length += absorb;
	  successor = successor->left;
	}

      successor->total_length += absorb;
      delete_interval (i);
      return successor;
    }

  successor = i;
  while (! NULL_PARENT (successor))	   /* It's above us.  Subtract as
					      we ascend.  */
    {
      if (AM_LEFT_CHILD (successor))
	{
	  successor = successor->parent;
	  delete_interval (i);
	  return successor;
	}

      successor = successor->parent;
      successor->total_length -= absorb;
    }

  /* This must be the rightmost or last interval and cannot
     be merged right.  The caller should have known.  */
  abort ();
}

/* Merge interval I with its lexicographic predecessor. The resulting
   interval is returned, and has the properties of the original predecessor.
   The properties of I are lost.  Interval node I is removed from the tree.

   IMPORTANT:
   The caller must verify that this is not the first (leftmost) interval.  */

INTERVAL
merge_interval_left (i)
     register INTERVAL i;
{
  register int absorb = LENGTH (i);
  register INTERVAL predecessor;

  /* Zero out this interval.  */
  i->total_length -= absorb;

  /* Find the preceding interval.  */
  if (! NULL_LEFT_CHILD (i))	/* It's below us. Go down,
				   adding ABSORB as we go.  */
    {
      predecessor = i->left;
      while (! NULL_RIGHT_CHILD (predecessor))
	{
	  predecessor->total_length += absorb;
	  predecessor = predecessor->right;
	}

      predecessor->total_length += absorb;
      delete_interval (i);
      return predecessor;
    }

  predecessor = i;
  while (! NULL_PARENT (predecessor))	/* It's above us.  Go up,
				   subtracting ABSORB.  */
    {
      if (AM_RIGHT_CHILD (predecessor))
	{
	  predecessor = predecessor->parent;
	  delete_interval (i);
	  return predecessor;
	}

      predecessor = predecessor->parent;
      predecessor->total_length -= absorb;
    }

  /* This must be the leftmost or first interval and cannot
     be merged left.  The caller should have known.  */
  abort ();
}

/* Make an exact copy of interval tree SOURCE which descends from
   PARENT.  This is done by recursing through SOURCE, copying
   the current interval and its properties, and then adjusting
   the pointers of the copy.  */

static INTERVAL
reproduce_tree (source, parent)
     INTERVAL source, parent;
{
  register INTERVAL t = make_interval ();

  bcopy (source, t, INTERVAL_SIZE);
  copy_properties (source, t);
  t->parent = parent;
  if (! NULL_LEFT_CHILD (source))
    t->left = reproduce_tree (source->left, t);
  if (! NULL_RIGHT_CHILD (source))
    t->right = reproduce_tree (source->right, t);

  return t;
}

#if 0
/* Nobody calls this.  Perhaps it's a vestige of an earlier design.  */

/* Make a new interval of length LENGTH starting at START in the
   group of intervals INTERVALS, which is actually an interval tree.
   Returns the new interval.

   Generate an error if the new positions would overlap an existing
   interval.  */

static INTERVAL
make_new_interval (intervals, start, length)
     INTERVAL intervals;
     int start, length;
{
  INTERVAL slot;

  slot = find_interval (intervals, start);
  if (start + length > slot->position + LENGTH (slot))
    error ("Interval would overlap");

  if (start == slot->position && length == LENGTH (slot))
    return slot;

  if (slot->position == start)
    {
      /* New right node.  */
      split_interval_right (slot, length);
      return slot;
    }

  if (slot->position + LENGTH (slot) == start + length)
    {
      /* New left node.  */
      split_interval_left (slot, LENGTH (slot) - length);
      return slot;
    }

  /* Convert interval SLOT into three intervals.  */
  split_interval_left (slot, start - slot->position);
  split_interval_right (slot, length);
  return slot;
}
#endif

/* Insert the intervals of SOURCE into BUFFER at POSITION.
   LENGTH is the length of the text in SOURCE.

   This is used in insdel.c when inserting Lisp_Strings into the
   buffer.  The text corresponding to SOURCE is already in the buffer
   when this is called.  The intervals of new tree are a copy of those
   belonging to the string being inserted; intervals are never
   shared.

   If the inserted text had no intervals associated, and we don't
   want to inherit the surrounding text's properties, this function
   simply returns -- offset_intervals should handle placing the
   text in the correct interval, depending on the sticky bits.

   If the inserted text had properties (intervals), then there are two
   cases -- either insertion happened in the middle of some interval,
   or between two intervals.

   If the text goes into the middle of an interval, then new
   intervals are created in the middle with only the properties of
   the new text, *unless* the macro MERGE_INSERTIONS is true, in
   which case the new text has the union of its properties and those
   of the text into which it was inserted.

   If the text goes between two intervals, then if neither interval
   had its appropriate sticky property set (front_sticky, rear_sticky),
   the new text has only its properties.  If one of the sticky properties
   is set, then the new text "sticks" to that region and its properties
   depend on merging as above.  If both the preceding and succeeding
   intervals to the new text are "sticky", then the new text retains
   only its properties, as if neither sticky property were set.  Perhaps
   we should consider merging all three sets of properties onto the new
   text...  */

void
graft_intervals_into_buffer (source, position, length, buffer, inherit)
     INTERVAL source;
     int position, length;
     struct buffer *buffer;
     int inherit;
{
  register INTERVAL under, over, this, prev;
  register INTERVAL tree;
  int middle;

  tree = BUF_INTERVALS (buffer);

  /* If the new text has no properties, it becomes part of whatever
     interval it was inserted into.  */
  if (NULL_INTERVAL_P (source))
    {
      Lisp_Object buf;
      if (!inherit && ! NULL_INTERVAL_P (tree))
	{
	  XSETBUFFER (buf, buffer);
	  Fset_text_properties (make_number (position),
				make_number (position + length),
				Qnil, buf);
	}
      if (! NULL_INTERVAL_P (BUF_INTERVALS (buffer)))
	BUF_INTERVALS (buffer) = balance_an_interval (BUF_INTERVALS (buffer));
      return;
    }

  if (NULL_INTERVAL_P (tree))
    {
      /* The inserted text constitutes the whole buffer, so
	 simply copy over the interval structure.  */
      if ((BUF_Z (buffer) - BUF_BEG (buffer)) == TOTAL_LENGTH (source))
	{
	  Lisp_Object buf;
	  XSETBUFFER (buf, buffer);
	  BUF_INTERVALS (buffer) = reproduce_tree (source, buf);
	  /* Explicitly free the old tree here.  */

	  return;
	}

      /* Create an interval tree in which to place a copy
	 of the intervals of the inserted string.  */
      {
	Lisp_Object buf;
	XSETBUFFER (buf, buffer);
	tree = create_root_interval (buf);
      }
    }
  else if (TOTAL_LENGTH (tree) == TOTAL_LENGTH (source))
    /* If the buffer contains only the new string, but
       there was already some interval tree there, then it may be
       some zero length intervals.  Eventually, do something clever
       about inserting properly.  For now, just waste the old intervals.  */
    {
      BUF_INTERVALS (buffer) = reproduce_tree (source, tree->parent);
      /* Explicitly free the old tree here.  */

      return;
    }
  /* Paranoia -- the text has already been added, so this buffer
     should be of non-zero length.  */
  else if (TOTAL_LENGTH (tree) == 0)
    abort ();

  this = under = find_interval (tree, position);
  if (NULL_INTERVAL_P (under))	/* Paranoia */
    abort ();
  over = find_interval (source, 1);

  /* Here for insertion in the middle of an interval.
     Split off an equivalent interval to the right,
     then don't bother with it any more.  */

  if (position > under->position)
    {
      INTERVAL end_unchanged
	= split_interval_left (this, position - under->position);
      copy_properties (under, end_unchanged);
      under->position = position;
      prev = 0;
      middle = 1;
    }
  else
    {
      prev = previous_interval (under);
      if (prev && !END_NONSTICKY_P (prev))
	prev = 0;
    }

  /* Insertion is now at beginning of UNDER.  */

  /* The inserted text "sticks" to the interval `under',
     which means it gets those properties.
     The properties of under are the result of
     adjust_intervals_for_insertion, so stickyness has
     already been taken care of.  */
     
  while (! NULL_INTERVAL_P (over))
    {
      if (LENGTH (over) < LENGTH (under))
	{
	  this = split_interval_left (under, LENGTH (over));
	  copy_properties (under, this);
	}
      else
	this = under;
      copy_properties (over, this);
      if (inherit)
	merge_properties (over, this);
      else
	copy_properties (over, this);
      over = next_interval (over);
    }

  if (! NULL_INTERVAL_P (BUF_INTERVALS (buffer)))
    BUF_INTERVALS (buffer) = balance_an_interval (BUF_INTERVALS (buffer));
  return;
}

/* Get the value of property PROP from PLIST,
   which is the plist of an interval.
   We check for direct properties and for categories with property PROP.  */

Lisp_Object
textget (plist, prop)
     Lisp_Object plist;
     register Lisp_Object prop;
{
  register Lisp_Object tail, fallback;
  fallback = Qnil;

  for (tail = plist; !NILP (tail); tail = Fcdr (Fcdr (tail)))
    {
      register Lisp_Object tem;
      tem = Fcar (tail);
      if (EQ (prop, tem))
	return Fcar (Fcdr (tail));
      if (EQ (tem, Qcategory))
	{
	  tem = Fcar (Fcdr (tail));
	  if (SYMBOLP (tem))
	    fallback = Fget (tem, prop);
	}
    }

  return fallback;
}

/* Get the value of property PROP from PLIST,
   which is the plist of an interval.
   We check for direct properties only! */

Lisp_Object
textget_direct (plist, prop)
     Lisp_Object plist;
     register Lisp_Object prop;
{
  register Lisp_Object tail;

  for (tail = plist; !NILP (tail); tail = Fcdr (Fcdr (tail)))
    {
      if (EQ (prop, Fcar (tail)))
	return Fcar (Fcdr (tail));
    }

  return Qnil;
}

/* Set point in BUFFER to POSITION.  If the target position is 
   before an intangible character, move to an ok place.  */

void
set_point (position, buffer)
     register int position;
     register struct buffer *buffer;
{
  register INTERVAL to, from, toprev, fromprev, target;
  int buffer_point;
  register Lisp_Object obj;
  int backwards = (position < BUF_PT (buffer)) ? 1 : 0;
  int old_position = BUF_PT (buffer);

  if (position == BUF_PT (buffer))
    return;

  /* Check this now, before checking if the buffer has any intervals.
     That way, we can catch conditions which break this sanity check
     whether or not there are intervals in the buffer.  */
  if (position > BUF_Z (buffer) || position < BUF_BEG (buffer))
    abort ();

  if (NULL_INTERVAL_P (BUF_INTERVALS (buffer)))
    {
      
      BUF_PT (buffer) = position;
      return;
    }

  /* Set TO to the interval containing the char after POSITION,
     and TOPREV to the interval containing the char before POSITION.
     Either one may be null.  They may be equal.  */
  to = find_interval (BUF_INTERVALS (buffer), position);
  if (position == BUF_BEGV (buffer))
    toprev = 0;
  else if (to->position == position)
    toprev = previous_interval (to);
  else
    toprev = to;

  buffer_point = (BUF_PT (buffer) == BUF_ZV (buffer)
		  ? BUF_ZV (buffer) - 1
		  : BUF_PT (buffer));

  /* Set FROM to the interval containing the char after PT,
     and FROMPREV to the interval containing the char before PT.
     Either one may be null.  They may be equal.  */
  /* We could cache this and save time.  */
  from = find_interval (BUF_INTERVALS (buffer), buffer_point);
  if (buffer_point == BUF_BEGV (buffer))
    fromprev = 0;
  else if (from->position == BUF_PT (buffer))
    fromprev = previous_interval (from);
  else if (buffer_point != BUF_PT (buffer))
    fromprev = from, from = 0;
  else
    fromprev = from;

  /* Moving within an interval.  */
  if (to == from && toprev == fromprev && INTERVAL_VISIBLE_P (to))
    {
      BUF_PT (buffer) = position;
      return;
    }

  /* If the new position is between two intangible characters,
     move forward or backward across all such characters.  */
  if (NILP (Vinhibit_point_motion_hooks) && ! NULL_INTERVAL_P (to)
      && ! NULL_INTERVAL_P (toprev))
    {
      if (backwards)
	{
	  /* Make sure the following character is intangible
	     if the previous one is.  */
	  if (toprev == to
	      || ! NILP (textget (to->plist, Qintangible)))
	    /* Ok, that is so.  Back up across intangible text.  */
	    while (! NULL_INTERVAL_P (toprev)
		   && ! NILP (textget (toprev->plist, Qintangible)))
	      {
		to = toprev;
		toprev = previous_interval (toprev);
		if (NULL_INTERVAL_P (toprev))
		  position = BUF_BEGV (buffer);
		else
		  /* This is the only line that's not
		     dual to the following loop.
		     That's because we want the position
		     at the end of TOPREV.  */
		  position = to->position;
	      }
	}
      else
	{
	  /* Make sure the previous character is intangible
	     if the following one is.  */
	  if (toprev == to
	      || ! NILP (textget (toprev->plist, Qintangible)))
	    /* Ok, that is so.  Advance across intangible text.  */
	    while (! NULL_INTERVAL_P (to)
		   && ! NILP (textget (to->plist, Qintangible)))
	      {
		toprev = to;
		to = next_interval (to);
		if (NULL_INTERVAL_P (to))
		  position = BUF_ZV (buffer);
		else
		  position = to->position;
	      }
	}
      /* Here TO is the interval after the stopping point
	 and TOPREV is the interval before the stopping point.
	 One or the other may be null.  */
    }

  BUF_PT (buffer) = position;

  /* We run point-left and point-entered hooks here, iff the
     two intervals are not equivalent.  These hooks take
     (old_point, new_point) as arguments.  */
  if (NILP (Vinhibit_point_motion_hooks)
      && (! intervals_equal (from, to)
	  || ! intervals_equal (fromprev, toprev)))
    {
      Lisp_Object leave_after, leave_before, enter_after, enter_before;

      if (fromprev)
	leave_after = textget (fromprev->plist, Qpoint_left);
      else
	leave_after = Qnil;
      if (from)
	leave_before = textget (from->plist, Qpoint_left);
      else
	leave_before = Qnil;

      if (toprev)
	enter_after = textget (toprev->plist, Qpoint_entered);
      else
	enter_after = Qnil;
      if (to)
	enter_before = textget (to->plist, Qpoint_entered);
      else
	enter_before = Qnil;

      if (! EQ (leave_before, enter_before) && !NILP (leave_before))
	call2 (leave_before, old_position, position);
      if (! EQ (leave_after, enter_after) && !NILP (leave_after))
	call2 (leave_after, old_position, position);

      if (! EQ (enter_before, leave_before) && !NILP (enter_before))
	call2 (enter_before, old_position, position);
      if (! EQ (enter_after, leave_after) && !NILP (enter_after))
	call2 (enter_after, old_position, position);
    }
}

/* Set point temporarily, without checking any text properties.  */

INLINE void
temp_set_point (position, buffer)
     int position;
     struct buffer *buffer;
{
  BUF_PT (buffer) = position;
}

/* Return the proper local map for position POSITION in BUFFER.
   Use the map specified by the local-map property, if any.
   Otherwise, use BUFFER's local map.  */

Lisp_Object
get_local_map (position, buffer)
     register int position;
     register struct buffer *buffer;
{
  register INTERVAL interval;
  Lisp_Object prop, tem;

  if (NULL_INTERVAL_P (BUF_INTERVALS (buffer)))
    return buffer->keymap;

  /* Perhaps we should just change `position' to the limit.  */
  if (position > BUF_Z (buffer) || position < BUF_BEG (buffer))
    abort ();

  interval = find_interval (BUF_INTERVALS (buffer), position);
  prop = textget (interval->plist, Qlocal_map);
  if (NILP (prop))
    return buffer->keymap;

  /* Use the local map only if it is valid.  */
  tem = Fkeymapp (prop);
  if (!NILP (tem))
    return prop;

  return buffer->keymap;
}

/* Call the modification hook functions in LIST, each with START and END.  */

static void
call_mod_hooks (list, start, end)
     Lisp_Object list, start, end;
{
  struct gcpro gcpro1;
  GCPRO1 (list);
  while (!NILP (list))
    {
      call2 (Fcar (list), start, end);
      list = Fcdr (list);
    }
  UNGCPRO;
}

/* Check for read-only intervals and signal an error if we find one.
   Then check for any modification hooks in the range START up to
   (but not including) END.  Create a list of all these hooks in
   lexicographic order, eliminating consecutive extra copies of the
   same hook.  Then call those hooks in order, with START and END - 1
   as arguments.  */

void
verify_interval_modification (buf, start, end)
     struct buffer *buf;
     int start, end;
{
  register INTERVAL intervals = BUF_INTERVALS (buf);
  register INTERVAL i, prev;
  Lisp_Object hooks;
  register Lisp_Object prev_mod_hooks;
  Lisp_Object mod_hooks;
  struct gcpro gcpro1;

  hooks = Qnil;
  prev_mod_hooks = Qnil;
  mod_hooks = Qnil;

  if (NULL_INTERVAL_P (intervals))
    return;

  if (start > end)
    {
      int temp = start;
      start = end;
      end = temp;
    }

  /* For an insert operation, check the two chars around the position.  */
  if (start == end)
    {
      INTERVAL prev;
      Lisp_Object before, after;

      /* Set I to the interval containing the char after START,
	 and PREV to the interval containing the char before START.
	 Either one may be null.  They may be equal.  */
      i = find_interval (intervals, start);

      if (start == BUF_BEGV (buf))
	prev = 0;
      else if (i->position == start)
	prev = previous_interval (i);
      else if (i->position < start)
	prev = i;
      if (start == BUF_ZV (buf))
	i = 0;

      /* If Vinhibit_read_only is set and is not a list, we can
	 skip the read_only checks.  */
      if (NILP (Vinhibit_read_only) || CONSP (Vinhibit_read_only))
	{
	  /* If I and PREV differ we need to check for the read-only
	     property together with its stickyness. If either I or
	     PREV are 0, this check is all we need.
	     We have to take special care, since read-only may be
	     indirectly defined via the category property.  */
	  if (i != prev)
	    {
	      if (! NULL_INTERVAL_P (i))
		{
		  after = textget (i->plist, Qread_only);
		  
		  /* If interval I is read-only and read-only is
		     front-sticky, inhibit insertion.
		     Check for read-only as well as category.  */
		  if (! NILP (after)
		      && NILP (Fmemq (after, Vinhibit_read_only)))
		    {
		      Lisp_Object tem;

		      tem = textget (i->plist, Qfront_sticky);
		      if (TMEM (Qread_only, tem)
			  || (NILP (textget_direct (i->plist, Qread_only))
			      && TMEM (Qcategory, tem)))
			error ("Attempt to insert within read-only text");
		    }
		}

	      if (! NULL_INTERVAL_P (prev))
		{
		  before = textget (prev->plist, Qread_only);
		  
		  /* If interval PREV is read-only and read-only isn't
		     rear-nonsticky, inhibit insertion.
		     Check for read-only as well as category.  */
		  if (! NILP (before)
		      && NILP (Fmemq (before, Vinhibit_read_only)))
		    {
		      Lisp_Object tem;

		      tem = textget (prev->plist, Qrear_nonsticky);
		      if (! TMEM (Qread_only, tem)
			  && (! NILP (textget_direct (prev->plist,Qread_only))
			      || ! TMEM (Qcategory, tem)))
			error ("Attempt to insert within read-only text");
		    }
		}
	    }
	  else if (! NULL_INTERVAL_P (i))
	    {
	      after = textget (i->plist, Qread_only);
		  
	      /* If interval I is read-only and read-only is
		 front-sticky, inhibit insertion.
		 Check for read-only as well as category.  */
	      if (! NILP (after) && NILP (Fmemq (after, Vinhibit_read_only)))
		{
		  Lisp_Object tem;

		  tem = textget (i->plist, Qfront_sticky);
		  if (TMEM (Qread_only, tem)
		      || (NILP (textget_direct (i->plist, Qread_only))
			  && TMEM (Qcategory, tem)))
		    error ("Attempt to insert within read-only text");

		  tem = textget (prev->plist, Qrear_nonsticky);
		  if (! TMEM (Qread_only, tem)
		      && (! NILP (textget_direct (prev->plist, Qread_only))
			  || ! TMEM (Qcategory, tem)))
		    error ("Attempt to insert within read-only text");
		}
	    }
	}

      /* Run both insert hooks (just once if they're the same).  */
      if (!NULL_INTERVAL_P (prev))
	prev_mod_hooks = textget (prev->plist, Qinsert_behind_hooks);
      if (!NULL_INTERVAL_P (i))
	mod_hooks = textget (i->plist, Qinsert_in_front_hooks);
      GCPRO1 (mod_hooks);
      if (! NILP (prev_mod_hooks))
	call_mod_hooks (prev_mod_hooks, make_number (start),
			make_number (end));
      UNGCPRO;
      if (! NILP (mod_hooks) && ! EQ (mod_hooks, prev_mod_hooks))
	call_mod_hooks (mod_hooks, make_number (start), make_number (end));
    }
  else
    {
      /* Loop over intervals on or next to START...END,
	 collecting their hooks.  */

      i = find_interval (intervals, start);
      do
	{
	  if (! INTERVAL_WRITABLE_P (i))
	    error ("Attempt to modify read-only text");

	  mod_hooks = textget (i->plist, Qmodification_hooks);
	  if (! NILP (mod_hooks) && ! EQ (mod_hooks, prev_mod_hooks))
	    {
	      hooks = Fcons (mod_hooks, hooks);
	      prev_mod_hooks = mod_hooks;
	    }

	  i = next_interval (i);
	}
      /* Keep going thru the interval containing the char before END.  */
      while (! NULL_INTERVAL_P (i) && i->position < end);

      GCPRO1 (hooks);
      hooks = Fnreverse (hooks);
      while (! EQ (hooks, Qnil))
	{
	  call_mod_hooks (Fcar (hooks), make_number (start),
			  make_number (end));
	  hooks = Fcdr (hooks);
	}
      UNGCPRO;
    }
}

/* Produce an interval tree reflecting the intervals in
   TREE from START to START + LENGTH.  */

INTERVAL
copy_intervals (tree, start, length)
     INTERVAL tree;
     int start, length;
{
  register INTERVAL i, new, t;
  register int got, prevlen;

  if (NULL_INTERVAL_P (tree) || length <= 0)
    return NULL_INTERVAL;

  i = find_interval (tree, start);
  if (NULL_INTERVAL_P (i) || LENGTH (i) == 0)
    abort ();

  /* If there is only one interval and it's the default, return nil.  */
  if ((start - i->position + 1 + length) < LENGTH (i)
      && DEFAULT_INTERVAL_P (i))
    return NULL_INTERVAL;

  new = make_interval ();
  new->position = 1;
  got = (LENGTH (i) - (start - i->position));
  new->total_length = length;
  copy_properties (i, new);

  t = new;
  prevlen = got;
  while (got < length)
    {
      i = next_interval (i);
      t = split_interval_right (t, prevlen);
      copy_properties (i, t);
      prevlen = LENGTH (i);
      got += prevlen;
    }

  return balance_an_interval (new);
}

/* Give STRING the properties of BUFFER from POSITION to LENGTH.  */

INLINE void
copy_intervals_to_string (string, buffer, position, length)
     Lisp_Object string, buffer;
     int position, length;
{
  INTERVAL interval_copy = copy_intervals (BUF_INTERVALS (XBUFFER (buffer)),
					   position, length);
  if (NULL_INTERVAL_P (interval_copy))
    return;

  interval_copy->parent = (INTERVAL) string;
  XSTRING (string)->intervals = interval_copy;
}

/* Return 1 if string S1 and S2 have identical properties; 0 otherwise.
   Assume they have identical characters.  */

int
compare_string_intervals (s1, s2)
     Lisp_Object s1, s2;
{
  INTERVAL i1, i2;
  int pos = 1;
  int end = XSTRING (s1)->size + 1;

  /* We specify 1 as position because the interval functions
     always use positions starting at 1.  */
  i1 = find_interval (XSTRING (s1)->intervals, 1);
  i2 = find_interval (XSTRING (s2)->intervals, 1);

  while (pos < end)
    {
      /* Determine how far we can go before we reach the end of I1 or I2.  */
      int len1 = (i1 != 0 ? INTERVAL_LAST_POS (i1) : end) - pos;
      int len2 = (i2 != 0 ? INTERVAL_LAST_POS (i2) : end) - pos;
      int distance = min (len1, len2);

      /* If we ever find a mismatch between the strings,
	 they differ.  */
      if (! intervals_equal (i1, i2))
	return 0;

      /* Advance POS till the end of the shorter interval,
	 and advance one or both interval pointers for the new position.  */
      pos += distance;
      if (len1 == distance)
	i1 = next_interval (i1);
      if (len2 == distance)
	i2 = next_interval (i2);
    }
  return 1;
}

#endif /* USE_TEXT_PROPERTIES */
