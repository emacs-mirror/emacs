/* Code for doing intervals.
   Copyright (C) 1993 Free Software Foundation, Inc.

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


#include "config.h"
#include "lisp.h"
#include "intervals.h"
#include "buffer.h"

/* The rest of the file is within this conditional. */
#ifdef USE_TEXT_PROPERTIES

/* Factor for weight-balancing interval trees. */
Lisp_Object interval_balance_threshold;

/* Utility functions for intervals. */


/* Create the root interval of some object, a buffer or string. */

INTERVAL
create_root_interval (parent)
     Lisp_Object parent;
{
  INTERVAL new = make_interval ();

  if (XTYPE (parent) == Lisp_Buffer)
    {
      new->total_length = BUF_Z (XBUFFER (parent)) - 1;
      XBUFFER (parent)->intervals = new;
    }
  else if (XTYPE (parent) == Lisp_String)
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
   0 otherwise. */

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
      /* Lengths of the two plists were unequal */
      if (i1_len == 0)
	return 0;

      i0_sym = Fcar (i0_cdr);
      i1_val = Fmemq (i0_sym, i1->plist);

      /* i0 has something i1 doesn't */
      if (EQ (i1_val, Qnil))
	return 0;

      /* i0 and i1 both have sym, but it has different values in each */
      i0_cdr = Fcdr (i0_cdr);
      if (! Fequal (i1_val, Fcar (i0_cdr)))
	return 0;

      i0_cdr = Fcdr (i0_cdr);
      i1_len--;
    }

  /* Lengths of the two plists were unequal */
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
/* These functions are temporary, for debugging purposes only. */

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
  int len = LENGTH (interval);

  /* Deal with any Parent of A;  make it point to B. */
  if (! ROOT_INTERVAL_P (interval))
    if (AM_LEFT_CHILD (interval))
      interval->parent->left = interval->left;
    else
      interval->parent->right = interval->left;
  interval->left->parent = interval->parent;

  /* B gets the same length as A, since it get A's position in the tree. */
  interval->left->total_length = interval->total_length;

  /* B becomes the parent of A. */
  i = interval->left->right;
  interval->left->right = interval;
  interval->parent = interval->left;

  /* A gets c as left child. */
  interval->left = i;
  if (! NULL_INTERVAL_P (i))
    i->parent = interval;
  interval->total_length = (len + LEFT_TOTAL_LENGTH (interval)
			    + RIGHT_TOTAL_LENGTH (interval));

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
  int len = LENGTH (interval);

  /* Deal with the parent of A. */
  if (! ROOT_INTERVAL_P (interval))
    if (AM_LEFT_CHILD (interval))
      interval->parent->left = interval->right;
    else
      interval->parent->right = interval->right;
  interval->right->parent = interval->parent;

  /* B must have the same total length of A. */
  interval->right->total_length = interval->total_length;

  /* Make B the parent of A */
  i = interval->right->left;
  interval->right->left = interval;
  interval->parent = interval->right;

  /* Make A point to c */
  interval->right = i;
  if (! NULL_INTERVAL_P (i))
    i->parent = interval;
  interval->total_length = (len + LEFT_TOTAL_LENGTH (interval)
			    + RIGHT_TOTAL_LENGTH (interval));

  return B;
}

/* Split INTERVAL into two pieces, starting the second piece at character
   position OFFSET (counting from 1), relative to INTERVAL.  The right-hand
   piece (second, lexicographically) is returned.

   The size and position fields of the two intervals are set based upon
   those of the original interval.  The property list of the new interval
   is reset, thus it is up to the caller to do the right thing with the
   result.

   Note that this does not change the position of INTERVAL;  if it is a root,
   it is still a root after this operation. */

INTERVAL
split_interval_right (interval, offset)
     INTERVAL interval;
     int offset;
{
  INTERVAL new = make_interval ();
  int position = interval->position;
  int new_length = LENGTH (interval) - offset + 1;

  new->position = position + offset - 1;
  new->parent = interval;

  if (LEAF_INTERVAL_P (interval) || NULL_RIGHT_CHILD (interval))
    {
      interval->right = new;
      new->total_length = new_length;

      return new;
    }

  /* Insert the new node between INTERVAL and its right child. */
  new->right = interval->right;
  interval->right->parent = new;
  interval->right = new;

  new->total_length = new_length + new->right->total_length;

  return new;
}

/* Split INTERVAL into two pieces, starting the second piece at character
   position OFFSET (counting from 1), relative to INTERVAL.  The left-hand
   piece (first, lexicographically) is returned.

   The size and position fields of the two intervals are set based upon
   those of the original interval.  The property list of the new interval
   is reset, thus it is up to the caller to do the right thing with the
   result.

   Note that this does not change the position of INTERVAL;  if it is a root,
   it is still a root after this operation. */

INTERVAL
split_interval_left (interval, offset)
     INTERVAL interval;
     int offset;
{
  INTERVAL new = make_interval ();
  int position = interval->position;
  int new_length = offset - 1;

  new->position = interval->position;
  interval->position = interval->position + offset - 1;
  new->parent = interval;

  if (NULL_LEFT_CHILD (interval))
    {
      interval->left = new;
      new->total_length = new_length;

      return new;
    }

  /* Insert the new node between INTERVAL and its left child. */
  new->left = interval->left;
  new->left->parent = new;
  interval->left = new;
  new->total_length = new_length + LEFT_TOTAL_LENGTH (new);

  return new;
}

/* Find the interval containing text position POSITION in the text
   represented by the interval tree TREE.  POSITION is relative to
   the beginning of that text.

   The `position' field, which is a cache of an interval's position,
   is updated in the interval found.  Other functions (e.g., next_interval)
   will update this cache based on the result of find_interval. */

INLINE INTERVAL
find_interval (tree, position)
     register INTERVAL tree;
     register int position;
{
  register int relative_position = position;

  if (NULL_INTERVAL_P (tree))
    return NULL_INTERVAL;

  if (position > TOTAL_LENGTH (tree))
    abort ();			/* Paranoia */
#if 0
    position = TOTAL_LENGTH (tree);
#endif

  while (1)
    {
      if (relative_position <= LEFT_TOTAL_LENGTH (tree))
	{
	  tree = tree->left;
	}
      else if (relative_position > (TOTAL_LENGTH (tree)
				    - RIGHT_TOTAL_LENGTH (tree)))
	{
	  relative_position -= (TOTAL_LENGTH (tree)
				- RIGHT_TOTAL_LENGTH (tree));
	  tree = tree->right;
	}
      else
	{
	  tree->position = LEFT_TOTAL_LENGTH (tree)
	                   + position - relative_position + 1;
	  return tree;
	}
    }
}

/* Find the succeeding interval (lexicographically) to INTERVAL.
   Sets the `position' field based on that of INTERVAL (see
   find_interval). */

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
   find_interval). */

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
   to the root. */

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
	     then this code will have to change. */
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
   interval tree TREE, and then adjusting all of it's ancestors by adding
   LENGTH to them.

   If POSITION is the first character of an interval, meaning that point
   is actually between the two intervals, make the new text belong to
   the interval which is "sticky".

   If both intervals are "sticky", then make them belong to the left-most
   interval.  Another possibility would be to create a new interval for
   this text, and make it have the merged properties of both ends. */

static INTERVAL
adjust_intervals_for_insertion (tree, position, length)
     INTERVAL tree;
     int position, length;
{
  register INTERVAL i;

  if (TOTAL_LENGTH (tree) == 0)	/* Paranoia */
    abort ();

  /* If inserting at point-max of a buffer, that position
     will be out of range. */
  if (position > TOTAL_LENGTH (tree))
    position = TOTAL_LENGTH (tree);

  i = find_interval (tree, position);
  /* If we are positioned between intervals, check the stickiness of
     both of them. */
  if (position == i->position
      && position != 1)
    {
      register INTERVAL prev = previous_interval (i);

      /* If both intervals are sticky here, then default to the
         left-most one.  But perhaps we should create a new
	 interval here instead... */
      if (END_STICKY_P (prev))
	i = prev;
    }

  while (! NULL_INTERVAL_P (i))
    {
      i->total_length += length;
      i = i->parent;
    }

  return tree;
}

/* Delete an node I from its interval tree by merging its subtrees
   into one subtree which is then returned.  Caller is responsible for
   storing the resulting subtree into its parent. */

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
   for the length of I. */

void
delete_interval (i)
     register INTERVAL i;
{
  register INTERVAL parent;
  int amt = LENGTH (i);

  if (amt > 0)			/* Only used on zero-length intervals now. */
    abort ();

  if (ROOT_INTERVAL_P (i))
    {
      Lisp_Object owner = (Lisp_Object) i->parent;
      parent = delete_node (i);
      if (! NULL_INTERVAL_P (parent))
	parent->parent = (INTERVAL) owner;

      if (XTYPE (owner) == Lisp_Buffer)
	XBUFFER (owner)->intervals = parent;
      else if (XTYPE (owner) == Lisp_String)
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

/* Find the interval in TREE corresponding to the character position FROM
   and delete as much as possible of AMOUNT from that interval, starting
   after the relative position of FROM within it.  Return the amount
   actually deleted, and if the interval was zeroed-out, delete that
   interval node from the tree.

   Do this by recursing down TREE to the interval in question, and
   deleting the appropriate amount of text. */

static int
interval_deletion_adjustment (tree, from, amount)
     register INTERVAL tree;
     register int from, amount;
{
  register int relative_position = from;

  if (NULL_INTERVAL_P (tree))
    return 0;

  /* Left branch */
  if (relative_position <= LEFT_TOTAL_LENGTH (tree))
    {
      int subtract = interval_deletion_adjustment (tree->left,
						   relative_position,
						   amount);
      tree->total_length -= subtract;
      return subtract;
    }
  /* Right branch */
  else if (relative_position > (TOTAL_LENGTH (tree)
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
  /* Here -- this node */
  else
    {
      /* If this is a zero-length, marker interval, then
	 we must skip it. */

      if (relative_position == LEFT_TOTAL_LENGTH (tree) + 1)
	{
	  /* This means we're deleting from the beginning of this interval. */
	  register int my_amount = LENGTH (tree);

	  if (amount < my_amount)
	    {
	      tree->total_length -= amount;
	      return amount;
	    }
	  else
	    {
	      tree->total_length -= my_amount;
	      if (LENGTH (tree) != 0)
		abort ();	/* Paranoia */

	      delete_interval (tree);
	      return my_amount;
	    }
	}
      else			/* Deleting starting in the middle. */
	{
	  register int my_amount = ((tree->total_length
				     - RIGHT_TOTAL_LENGTH (tree))
				    - relative_position + 1);

	  if (amount <= my_amount)
	    {
	      tree->total_length -= amount;
	      return amount;
	    }
	  else
	    {
	      tree->total_length -= my_amount;
	      return my_amount;
	    }
	}
    }

  /* Never reach here */
  abort ();
}

/* Effect the adjustments neccessary to the interval tree of BUFFER
   to correspond to the deletion of LENGTH characters from that buffer
   text.  The deletion is effected at position START (relative to the
   buffer). */

static void
adjust_intervals_for_deletion (buffer, start, length)
     struct buffer *buffer;
     int start, length;
{
  register int left_to_delete = length;
  register INTERVAL tree = buffer->intervals;
  register int deleted;

  if (NULL_INTERVAL_P (tree))
    return;

  if (length == TOTAL_LENGTH (tree))
    {
      buffer->intervals = NULL_INTERVAL;
      return;
    }

  if (ONLY_INTERVAL_P (tree))
    {
      tree->total_length -= length;
      return;
    }

  if (start > TOTAL_LENGTH (tree))
    start = TOTAL_LENGTH (tree);
  while (left_to_delete > 0)
    {
      left_to_delete -= interval_deletion_adjustment (tree, start,
						      left_to_delete);
      tree = buffer->intervals;
      if (left_to_delete == tree->total_length)
	{
	  buffer->intervals = NULL_INTERVAL;
	  return;
	}
    }
}

/* Make the adjustments neccessary to the interval tree of BUFFER to
   represent an addition or deletion of LENGTH characters starting
   at position START.  Addition or deletion is indicated by the sign
   of LENGTH. */

INLINE void
offset_intervals (buffer, start, length)
     struct buffer *buffer;
     int start, length;
{
  if (NULL_INTERVAL_P (buffer->intervals) || length == 0)
    return;

  if (length > 0)
    adjust_intervals_for_insertion (buffer->intervals, start, length);
  else
    adjust_intervals_for_deletion (buffer, start, -length);
}

/* Merge interval I with its lexicographic successor. The resulting
   interval is returned, and has the properties of the original
   successor.  The properties of I are lost.  I is removed from the
   interval tree.

   IMPORTANT:
   The caller must verify that this is not the last (rightmost)
   interval. */

INTERVAL
merge_interval_right (i)
     register INTERVAL i;
{
  register int absorb = LENGTH (i);
  register INTERVAL successor;

  /* Zero out this interval. */
  i->total_length -= absorb;

  /* Find the succeeding interval. */
  if (! NULL_RIGHT_CHILD (i))      /* It's below us.  Add absorb
				      as we descend. */
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
					      we ascend. */
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
     be merged right.  The caller should have known. */
  abort ();
}

/* Merge interval I with its lexicographic predecessor. The resulting
   interval is returned, and has the properties of the original predecessor.
   The properties of I are lost.  Interval node I is removed from the tree.

   IMPORTANT:
   The caller must verify that this is not the first (leftmost) interval. */

INTERVAL
merge_interval_left (i)
     register INTERVAL i;
{
  register int absorb = LENGTH (i);
  register INTERVAL predecessor;

  /* Zero out this interval. */
  i->total_length -= absorb;

  /* Find the preceding interval. */
  if (! NULL_LEFT_CHILD (i))	/* It's below us. Go down,
				   adding ABSORB as we go. */
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
				   subtracting ABSORB. */
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
     be merged left.  The caller should have known. */
  abort ();
}

/* Make an exact copy of interval tree SOURCE which descends from
   PARENT.  This is done by recursing through SOURCE, copying
   the current interval and its properties, and then adjusting
   the pointers of the copy. */

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

/* Make a new interval of length LENGTH starting at START in the
   group of intervals INTERVALS, which is actually an interval tree.
   Returns the new interval.

   Generate an error if the new positions would overlap an existing
   interval. */

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
      /* New right node. */
      split_interval_right (slot, length + 1);
      return slot;
    }

  if (slot->position + LENGTH (slot) == start + length)
    {
      /* New left node. */
      split_interval_left (slot, LENGTH (slot) - length + 1);
      return slot;
    }

  /* Convert interval SLOT into three intervals. */
  split_interval_left (slot, start - slot->position + 1);
  split_interval_right (slot, length + 1);
  return slot;
}

/* Insert the intervals of SOURCE into BUFFER at POSITION.

   This is used in insdel.c when inserting Lisp_Strings into
   the buffer.  The text corresponding to SOURCE is already in
   the buffer when this is called.  The intervals of new tree are
   those belonging to the string being inserted;  a copy is not made.

   If the inserted text had no intervals associated, this function
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
   depend on merging as above.  If both the preceding and succeding
   intervals to the new text are "sticky", then the new text retains
   only its properties, as if neither sticky property were set.  Perhaps
   we should consider merging all three sets of properties onto the new
   text... */

void
graft_intervals_into_buffer (source, position, buffer)
     INTERVAL source;
     int position;
     struct buffer *buffer;
{
  register INTERVAL under, over, this, prev;
  register INTERVAL tree = buffer->intervals;
  int middle;

  /* If the new text has no properties, it becomes part of whatever
     interval it was inserted into. */
  if (NULL_INTERVAL_P (source))
    return;

  if (NULL_INTERVAL_P (tree))
    {
      /* The inserted text constitutes the whole buffer, so
	 simply copy over the interval structure. */
      if (BUF_Z (buffer) == TOTAL_LENGTH (source))
	{
	  buffer->intervals = reproduce_tree (source, tree->parent);
	  /* Explicitly free the old tree here. */

	  return;
	}

      /* Create an interval tree in which to place a copy
	 of the intervals of the inserted string. */
      {
	Lisp_Object buf;
	XSET (buf, Lisp_Buffer, buffer);
	tree = create_root_interval (buf);
      }
    }
  else
    if (TOTAL_LENGTH (tree) == TOTAL_LENGTH (source))
      /* If the buffer contains only the new string, but
	 there was already some interval tree there, then it may be
	 some zero length intervals.  Eventually, do something clever
	 about inserting properly.  For now, just waste the old intervals. */
      {
	buffer->intervals = reproduce_tree (source, tree->parent);
	/* Explicitly free the old tree here. */

	return;
      }
    else
      /* Paranoia -- the text has already been added, so this buffer
	 should be of non-zero length. */
      if (TOTAL_LENGTH (tree) == 0)
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
	= split_interval_left (this, position - under->position + 1);
      copy_properties (under, end_unchanged);
      under->position = position;
      prev = 0;
      middle = 1;
    }
  else
    {
      prev = previous_interval (under);
      if (prev && !END_STICKY_P (prev))
	prev = 0;
    }

  /* Insertion is now at beginning of UNDER.  */

  /* The inserted text "sticks" to the interval `under',
     which means it gets those properties. */
  while (! NULL_INTERVAL_P (over))
    {
      position = LENGTH (over) + 1;
      if (position < LENGTH (under))
	this = split_interval_left (under, position);
      else
	this = under;
      copy_properties (over, this);
      /* Insertion at the end of an interval, PREV,
	 inherits from PREV if PREV is sticky at the end.  */
      if (prev && ! FRONT_STICKY_P (under)
	  && MERGE_INSERTIONS (prev))
	merge_properties (prev, this);
      /* Maybe it inherits from the following interval
	 if that is sticky at the front.  */
      else if ((FRONT_STICKY_P (under) || middle)
	       && MERGE_INSERTIONS (under))
	merge_properties (under, this);
      over = next_interval (over);
    }

  buffer->intervals = balance_intervals (buffer->intervals);
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
	fallback = Fget (Fcar (Fcdr (tail)), prop);
    }

  return fallback;
}

/* Set point in BUFFER to POSITION.  If the target position is 
   before an invisible character which is not displayed with a special glyph,
   move back to an ok place to display.  */

void
set_point (position, buffer)
     register int position;
     register struct buffer *buffer;
{
  register INTERVAL to, from, toprev, fromprev, target;
  register int iposition = position;
  int buffer_point;
  register Lisp_Object obj;
  int backwards = (position < BUF_PT (buffer)) ? 1 : 0;
  int old_position = buffer->text.pt;

  if (position == buffer->text.pt)
    return;

  /* Check this now, before checking if the buffer has any intervals.
     That way, we can catch conditions which break this sanity check
     whether or not there are intervals in the buffer.  */
  if (position > BUF_Z (buffer) || position < BUF_BEG (buffer))
    abort ();

  if (NULL_INTERVAL_P (buffer->intervals))
    {
      buffer->text.pt = position;
      return;
    }

  /* Position Z is really one past the last char in the buffer.  */
  if (position == BUF_ZV (buffer))
    iposition = position - 1;

  /* Set TO to the interval containing the char after POSITION,
     and TOPREV to the interval containing the char before POSITION.
     Either one may be null.  They may be equal.  */
  to = find_interval (buffer->intervals, iposition);
  if (position == BUF_BEGV (buffer))
    toprev = 0;
  else if (to->position == position)
    toprev = previous_interval (to);
  else if (iposition != position)
    toprev = to, to = 0;
  else
    toprev = to;

  buffer_point = (BUF_PT (buffer) == BUF_ZV (buffer)
		  ? BUF_ZV (buffer) - 1
		  : BUF_PT (buffer));

  /* Set FROM to the interval containing the char after PT,
     and FROMPREV to the interval containing the char before PT.
     Either one may be null.  They may be equal.  */
  /* We could cache this and save time. */
  from = find_interval (buffer->intervals, buffer_point);
  if (from->position == BUF_BEGV (buffer))
    fromprev = 0;
  else if (from->position == BUF_PT (buffer))
    fromprev = previous_interval (from);
  else if (buffer_point != BUF_PT (buffer))
    fromprev = from, from = 0;
  else
    fromprev = from;

  /* Moving within an interval */
  if (to == from && toprev == fromprev && INTERVAL_VISIBLE_P (to))
    {
      buffer->text.pt = position;
      return;
    }

  /* If the new position is before an invisible character,
     move forward over all such.  */
  while (! NULL_INTERVAL_P (to)
	 && ! INTERVAL_VISIBLE_P (to)
	 && ! DISPLAY_INVISIBLE_GLYPH (to))
    {
      toprev = to;
      to = next_interval (to);
      position = to->position;
    }

  buffer->text.pt = position;

  /* We run point-left and point-entered hooks here, iff the
     two intervals are not equivalent.  These hooks take
     (old_point, new_point) as arguments.  */
  if (! intervals_equal (from, to)
      || ! intervals_equal (fromprev, toprev))
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

/* Set point temporarily, without checking any text properties. */

INLINE void
temp_set_point (position, buffer)
     int position;
     struct buffer *buffer;
{
  buffer->text.pt = position;
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

  if (NULL_INTERVAL_P (buffer->intervals))
    return current_buffer->keymap;

  /* Perhaps we should just change `position' to the limit. */
  if (position > BUF_Z (buffer) || position < BUF_BEG (buffer))
    abort ();

  /* Position Z is really one past the last char in the buffer.  */
  if (position == BUF_ZV (buffer))
    return current_buffer->keymap;

  interval = find_interval (buffer->intervals, position);
  prop = textget (interval->plist, Qlocal_map);
  if (NILP (prop))
    return current_buffer->keymap;

  /* Use the local map only if it is valid.  */
  tem = Fkeymapp (prop);
  if (!NILP (tem))
    return prop;

  return current_buffer->keymap;
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
   (but not including) TO.  Create a list of all these hooks in
   lexicographic order, eliminating consecutive extra copies of the
   same hook.  Then call those hooks in order, with START and END - 1
   as arguments. */

void
verify_interval_modification (buf, start, end)
     struct buffer *buf;
     int start, end;
{
  register INTERVAL intervals = buf->intervals;
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
      i = find_interval (intervals,
			 (start == BUF_ZV (buf) ? start - 1 : start));

      if (start == BUF_BEGV (buf))
	prev = 0;
      if (i->position == start)
	prev = previous_interval (i);
      else if (i->position < start)
	prev = i;
      if (start == BUF_ZV (buf))
	i = 0;

      if (NULL_INTERVAL_P (prev))
	{
	  after = textget (i->plist, Qread_only);
	  if (! NILP (after))
	    error ("Attempt to insert within read-only text");
	}
      else if (NULL_INTERVAL_P (i))
	{
	  before = textget (prev->plist, Qread_only);
	  if (! NILP (before))
	    error ("Attempt to insert within read-only text");
	}
      else
	{
	  before = textget (prev->plist, Qread_only);
	  after = textget (i->plist, Qread_only);
	  if (! NILP (before) && EQ (before, after))
	    error ("Attempt to insert within read-only text");
	}

      /* Run both mod hooks (just once if they're the same).  */
      if (!NULL_INTERVAL_P (prev))
	prev_mod_hooks = textget (prev->plist, Qmodification_hooks);
      if (!NULL_INTERVAL_P (i))
	mod_hooks = textget (i->plist, Qmodification_hooks);
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

/* Balance an interval node if the amount of text in its left and right
   subtrees differs by more than the percentage specified by
   `interval-balance-threshold'. */

static INTERVAL
balance_an_interval (i)
     INTERVAL i;
{
  register int total_children_size = (LEFT_TOTAL_LENGTH (i)
				      + RIGHT_TOTAL_LENGTH (i));
  register int threshold = (XFASTINT (interval_balance_threshold)
			    * (total_children_size / 100));

  if (LEFT_TOTAL_LENGTH (i) > RIGHT_TOTAL_LENGTH (i)
      && (LEFT_TOTAL_LENGTH (i) - RIGHT_TOTAL_LENGTH (i)) > threshold)
    return rotate_right (i);

  if (LEFT_TOTAL_LENGTH (i) > RIGHT_TOTAL_LENGTH (i)
      && (LEFT_TOTAL_LENGTH (i) - RIGHT_TOTAL_LENGTH (i)) > threshold)
    return rotate_right (i);

#if 0
  if (LEFT_TOTAL_LENGTH (i) >
      (RIGHT_TOTAL_LENGTH (i) + XINT (interval_balance_threshold)))
    return rotate_right (i);

  if (RIGHT_TOTAL_LENGTH (i) >
      (LEFT_TOTAL_LENGTH (i) + XINT (interval_balance_threshold)))
    return rotate_left (i);
#endif

  return i;
}

/* Balance the interval tree TREE.  Balancing is by weight
   (the amount of text). */

INTERVAL
balance_intervals (tree)
     register INTERVAL tree;
{
  register INTERVAL new_tree;

  if (NULL_INTERVAL_P (tree))
    return NULL_INTERVAL;

  new_tree = tree;
  do
    {
      tree = new_tree;
      new_tree = balance_an_interval (new_tree);
    }
  while (new_tree != tree);

  return new_tree;
}

/* Produce an interval tree reflecting the intervals in
   TREE from START to START + LENGTH. */

INTERVAL
copy_intervals (tree, start, length)
     INTERVAL tree;
     int start, length;
{
  register INTERVAL i, new, t;
  register int got;

  if (NULL_INTERVAL_P (tree) || length <= 0)
    return NULL_INTERVAL;

  i = find_interval (tree, start);
  if (NULL_INTERVAL_P (i) || LENGTH (i) == 0)
    abort ();

  /* If there is only one interval and it's the default, return nil. */
  if ((start - i->position + 1 + length) < LENGTH (i)
      && DEFAULT_INTERVAL_P (i))
    return NULL_INTERVAL;

  new = make_interval ();
  new->position = 1;
  got = (LENGTH (i) - (start - i->position));
  new->total_length = length;
  copy_properties (i, new);

  t = new;
  while (got < length)
    {
      i = next_interval (i);
      t = split_interval_right (t, got + 1);
      copy_properties (i, t);
      got += LENGTH (i);
    }

  if (got > length)
    t->total_length -= (got - length);

  return balance_intervals (new);
}

/* Give STRING the properties of BUFFER from POSITION to LENGTH. */

INLINE void
copy_intervals_to_string (string, buffer, position, length)
     Lisp_Object string, buffer;
     int position, length;
{
  INTERVAL interval_copy = copy_intervals (XBUFFER (buffer)->intervals,
					   position, length);
  if (NULL_INTERVAL_P (interval_copy))
    return;

  interval_copy->parent = (INTERVAL) string;
  XSTRING (string)->intervals = interval_copy;
}

#endif /* USE_TEXT_PROPERTIES */
