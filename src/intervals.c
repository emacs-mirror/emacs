/* Code for doing intervals.
   Copyright (C) 1991, 1992 Free Software Foundation, Inc.

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
#include "screen.h"

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
   of interval TARGET. */

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

static int depth;

/* Traverse an interval tree TREE, performing FUNCTION on each node.

   Perhaps we should pass the depth as an argument. */

void
traverse_intervals (tree, position, function)
     INTERVAL tree;
     int position;
     void (* function) ();
{
  if (NULL_INTERVAL_P (tree))
    return;

  depth++;
  traverse_intervals (tree->left, position, function);
  position += LEFT_TOTAL_LENGTH (tree);
  tree->position = position;
  (*function) (tree);
  position += LENGTH (tree);
  traverse_intervals (tree->right, position, function);
  depth--;
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
  traverse_intervals (tree, 1, &check_for_interval);
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
  traverse_intervals (i, 1, &inc_interval_count);

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

/* Split an interval into two.  The second (RIGHT) half is returned as
   the new interval.  The size and position of the interval being split are
   stored within it, having been found by find_interval ().  The properties
   are reset;  it is up to the caller to do the right thing.

   Note that this does not change the position of INTERVAL;  if it is a root,
   it is still a root after this operation. */

INTERVAL
split_interval_right (interval, relative_position)
     INTERVAL interval;
     int relative_position;
{
  INTERVAL new = make_interval ();
  int position = interval->position;
  int new_length = LENGTH (interval) - relative_position + 1;

  new->position = position + relative_position - 1;
  new->parent = interval;
#if 0
  copy_properties (interval, new);
#endif

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

/* Split an interval into two.  The first (LEFT) half is returned as
   the new interval.  The size and position of the interval being split
   are stored within it, having been found by find_interval ().  The
   properties are reset;  it is up to the caller to do the right thing.

   Note that this does not change the position of INTERVAL in the tree;  if it
   is a root, it is still a root after this operation.  */

INTERVAL
split_interval_left (interval, relative_position)
     INTERVAL interval;
     int relative_position;
{
  INTERVAL new = make_interval ();
  int position = interval->position;
  int new_length = relative_position - 1;

#if 0
  copy_properties (interval, new);
#endif

  new->position = interval->position;

  interval->position = interval->position + relative_position - 1;
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
  new->total_length = LENGTH (new) + LEFT_TOTAL_LENGTH (new);

  return new;
}

/* Find the interval containing POSITION in TREE.  POSITION is relative
   to the start of TREE. */

INTERVAL
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
   Sets the `position' field based on that of INTERVAL.

   Note that those values are only correct if they were also correct
   in INTERVAL. */

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
   Sets the `position' field based on that of INTERVAL.

   Note that those values are only correct if they were also correct
   in INTERVAL. */

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

/* Merge interval I with its lexicographic successor. Note that
   this does not deal with the properties, or delete I. */

INTERVAL
merge_interval_right (i)
     register INTERVAL i;
{
  register int absorb = LENGTH (i);

  /* Zero out this interval. */
  i->total_length -= absorb;

  /* Find the succeeding interval. */
  if (! NULL_RIGHT_CHILD (i))      /* It's below us.  Add absorb
				      as we descend. */
    {
      i = i->right;
      while (! NULL_LEFT_CHILD (i))
	{
	  i->total_length += absorb;
	  i = i->left;
	}

      i->total_length += absorb;
      return i;
    }

  while (! NULL_PARENT (i))	   /* It's above us.  Subtract as
				      we ascend. */
    {
      if (AM_LEFT_CHILD (i))
	{
	  i = i->parent;
	  return i;
	}

      i = i->parent;
      i->total_length -= absorb;
    }

  return NULL_INTERVAL;
}

/* Merge interval I with its lexicographic predecessor. Note that
   this does not deal with the properties, or delete I.*/

INTERVAL
merge_interval_left (i)
     register INTERVAL i;
{
  register int absorb = LENGTH (i);

  /* Zero out this interval. */
  i->total_length -= absorb;

  /* Find the preceding interval. */
  if (! NULL_LEFT_CHILD (i))	/* It's below us. Go down,
				   adding ABSORB as we go. */
    {
      i = i->left;
      while (! NULL_RIGHT_CHILD (i))
	{
	  i->total_length += absorb;
	  i = i->right;
	}

      i->total_length += absorb;
      return i;
    }

  while (! NULL_PARENT (i))	/* It's above us.  Go up,
				   subtracting ABSORB. */
    {
      if (AM_RIGHT_CHILD (i))
	{
	  i = i->parent;
	  return i;
	}

      i = i->parent;
      i->total_length -= absorb;
    }

  return NULL_INTERVAL;
}

/* Delete an interval node from its btree by merging its subtrees
   into one subtree which is returned.  Caller is responsible for
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

/* Recurse down to the interval containing FROM.  Then delete as much
   as possible (up to AMOUNT) from that interval, adjusting parental
   intervals on the way up.  If an interval is zeroed out, then
   it is deleted.

   Returns the amount deleted. */

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

  abort ();
}

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

/* Note that all intervals in OBJECT after START have slid by LENGTH. */

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

void
map_intervals (source, destination, position)
     INTERVAL source, destination;
     int position;
{
  register INTERVAL i, t;

  if (NULL_INTERVAL_P (source))
    return;
  i = find_interval (destination, position);
  if (NULL_INTERVAL_P (i))
    return;

  t = find_interval (source, 1);
  while (! NULL_INTERVAL_P (t))
    {
      i = make_new_interval (destination, position, LENGTH (t));
      position += LENGTH (t);
      copy_properties (t, i);
      t = next_interval (t);
    }
}

/* Insert the intervals of NEW_TREE into BUFFER at POSITION.

   This is used in insdel.c when inserting Lisp_Strings into
   the buffer.  The text corresponding to NEW_TREE is already in
   the buffer when this is called.  The intervals of new tree are
   those belonging to the string being inserted;  a copy is not made.

   If the inserted text had no intervals associated, this function
   simply returns -- offset_intervals should handle placing the
   text in the correct interval, depending on the hungry bits.

   If the inserted text had properties (intervals), then there are two
   cases -- either insertion happened in the middle of some interval,
   or between two intervals.

   If the text goes into the middle of an interval, then new
   intervals are created in the middle with only the properties of
   the new text, *unless* the macro MERGE_INSERTIONS is true, in
   which case the new text has the union of its properties and those
   of the text into which it was inserted.

   If the text goes between two intervals, then if neither interval
   had its appropriate hungry property set (front_hungry, rear_hungry),
   the new text has only its properties.  If one of the hungry properties
   is set, then the new text "sticks" to that region and its properties
   depend on merging as above.  If both the preceding and succeding
   intervals to the new text are "hungry", then the new text retains
   only its properties, as if neither hungry property were set.  Perhaps
   we should consider merging all three sets of properties onto the new
   text... */

void
graft_intervals_into_buffer (new_tree, position, b)
     INTERVAL new_tree;
     int position;
     struct buffer *b;
{
  register INTERVAL under, over, this;
  register INTERVAL tree = b->intervals;

  /* If the new text has no properties, it becomes part of whatever
    interval it was inserted into. */
  if (NULL_INTERVAL_P (new_tree))
    return;

  /* Paranoia -- the text has already been added, so this buffer
     should be of non-zero length. */
  if (TOTAL_LENGTH (tree) == 0)
    abort ();

  if (NULL_INTERVAL_P (tree))
    {
      /* The inserted text constitutes the whole buffer, so
	 simply copy over the interval structure. */
      if (BUF_Z (b) == TOTAL_LENGTH (new_tree))
	{
	  b->intervals = reproduce_tree (new_tree, tree->parent);
	  /* Explicitly free the old tree here. */

	  return;
	}

      /* Create an interval tree in which to place a copy
         of the intervals of the inserted string. */
      {
	Lisp_Object buffer;
	XSET (buffer, Lisp_Buffer, b);
	create_root_interval (buffer);
      }
    }
  else
    if (TOTAL_LENGTH (tree) == TOTAL_LENGTH (new_tree))

    /* If the buffer contains only the new string, but
       there was already some interval tree there, then it may be
       some zero length intervals.  Eventually, do something clever
       about inserting properly.  For now, just waste the old intervals. */
    {
      b->intervals = reproduce_tree (new_tree, tree->parent);
      /* Explicitly free the old tree here. */

      return;
    }

  this = under = find_interval (tree, position);
  if (NULL_INTERVAL_P (under))	/* Paranoia */
    abort ();
  over = find_interval (new_tree, 1);

  /* Insertion between intervals */
  if (position == under->position)
    {
      /* First interval -- none precede it. */
      if (position == 1)
	{
	  if (! under->front_hungry)
	    /* The inserted string keeps its own properties. */
	    while (! NULL_INTERVAL_P (over))
	    {
	      position = LENGTH (over) + 1;
	      this = split_interval_left (this, position);
	      copy_properties (over, this);
	      over = next_interval (over);
	    }
	  else
	    /* This string sticks to under */
	    while (! NULL_INTERVAL_P (over))
	    {
	      position = LENGTH (over) + 1;
	      this = split_interval_left (this, position);
	      copy_properties (under, this);
	      if (MERGE_INSERTIONS (under))
		merge_properties (over, this);
	      over = next_interval (over);
	    }
	}
       else
	{
	  INTERVAL prev = previous_interval (under);
	  if (NULL_INTERVAL_P (prev))
	    abort ();

	  if (prev->rear_hungry)
	    {
	      if (under->front_hungry)
		/* The intervals go inbetween as the two hungry
		   properties cancel each other.  Should we change
		   this policy? */
		while (! NULL_INTERVAL_P (over))
		  {
		    position = LENGTH (over) + 1;
		    this = split_interval_left (this, position);
		    copy_properties (over, this);
		    over = next_interval (over);
		  }
	      else
		/* The intervals stick to prev */
		while (! NULL_INTERVAL_P (over))
		  {
		    position = LENGTH (over) + 1;
		    this = split_interval_left (this, position);
		    copy_properties (prev, this);
		    if (MERGE_INSERTIONS (prev))
		      merge_properties (over, this);
		    over = next_interval (over);
		  }
	    }
	  else
	    {
	      if (under->front_hungry)
		/* The intervals stick to under */
		while (! NULL_INTERVAL_P (over))
		  {
		    position = LENGTH (over) + 1;
		    this = split_interval_left (this, position);
		    copy_properties (under, this);
		    if (MERGE_INSERTIONS (under))
		      merge_properties (over, this);
		    over = next_interval (over);
		  }
	      else
		/* The intervals go inbetween */
		while (! NULL_INTERVAL_P (over))
		  {
		    position = LENGTH (over) + 1;
		    this = split_interval_left (this, position);
		    copy_properties (over, this);
		    over = next_interval (over);
		  }
	    }
	}

      b->intervals = balance_intervals (b->intervals);
      return;
    }

  /* Here for insertion in the middle of an interval. */

  if (TOTAL_LENGTH (new_tree) < LENGTH (this))
    {
      INTERVAL end_unchanged
	= split_interval_right (this, TOTAL_LENGTH (new_tree) + 1);
      copy_properties (under, end_unchanged);
    }

  position = position - tree->position + 1;
  while (! NULL_INTERVAL_P (over))
    {
      this = split_interval_right (under, position);
      copy_properties (over, this);
      if (MERGE_INSERTIONS (under))
	merge_properties (under, this);

      position = LENGTH (over) + 1;
      over = next_interval (over);
    }

  b->intervals = balance_intervals (b->intervals);
  return;
}

/* Intervals can have properties which are hooks to call.  Look for
   the property HOOK on interval I, and if found, call its value as
   a function.*/

void
run_hooks (i, hook)
     INTERVAL i;
     Lisp_Object hook;
{
  register Lisp_Object tail = i->plist;
  register Lisp_Object sym, val;

  while (! NILP (tail))
    {
      sym = Fcar (tail);
      if (EQ (sym, hook))
	{
	  Lisp_Object begin, end;
	  XFASTINT (begin) = i->position;
	  XFASTINT (end) = i->position + LENGTH (i) - 1;
	  val = Fcar (Fcdr (tail));
	  call2 (val, begin, end);
	  return;
	}

      tail = Fcdr (Fcdr (tail));
    }
}

/* Set point in BUFFER to POSITION.  If the target position is in
   an invisible interval which is not displayed with a special glyph,
   skip intervals until we find one.  Point may be at the first
   position of an invisible interval, if it is displayed with a
   special glyph.

   This is the only place `PT' is an lvalue in all of emacs. */

void
set_point (position, buffer)
     register int position;
     register struct buffer *buffer;
{
  register INTERVAL to, from, target;
  register int iposition = position;
  int buffer_point;
  register Lisp_Object obj;
  int backwards = (position < BUF_PT (buffer)) ? 1 : 0;

  if (position == buffer->text.pt)
    return;

  if (NULL_INTERVAL_P (buffer->intervals))
    {
      buffer->text.pt = position;
      return;
    }

  /* Perhaps we should just change `position' to the limit. */
  if (position > BUF_Z (buffer) || position < BUF_BEG (buffer))
    abort ();

  /* Position Z is really one past the last char in the buffer.  */
  if (position == BUF_Z (buffer))
    iposition = position - 1;

  to = find_interval (buffer->intervals, iposition);
  buffer_point =(BUF_PT (buffer) == BUF_Z (buffer)
		 ? BUF_Z (buffer) - 1
		 : BUF_PT (buffer));
  from = find_interval (buffer->intervals, buffer_point);
  if (NULL_INTERVAL_P (to) || NULL_INTERVAL_P (from))
    abort ();			/* Paranoia */

  /* Moving within an interval */
  if (to == from && INTERVAL_VISIBLE_P (to))
    {
      buffer->text.pt = position;
      return;
    }

  /* Here for the case of moving into another interval. */

  target = to;
  while (! INTERVAL_VISIBLE_P (to) && ! DISPLAY_INVISIBLE_GLYPH (to)
	 && ! NULL_INTERVAL_P (to))
    to = (backwards ? previous_interval (to) : next_interval (to));
  if (NULL_INTERVAL_P (to))
    return;

  /* Here we know we are actually moving to another interval. */
  if (INTERVAL_VISIBLE_P (to))
    {
      /* If we skipped some intervals, go to the closest point
         in the interval we've stopped at. */
      if (to != target)
	buffer->text.pt = (backwards
			   ? to->position + LENGTH (to) - 1
			   : to->position);
      else
	buffer->text.pt = position;
    }
  else
    buffer->text.pt = to->position;

  /* We should run point-left and point-entered hooks here, iff the
     two intervals are not equivalent. */
}

/* Check for read-only intervals.  Call the modification hooks if any.
   Check for the range START up to (but not including) TO.

   First all intervals of the region are checked that they are
   modifiable, then all the modification hooks are called in
   lexicographic order. */

void
verify_interval_modification (buf, start, end)
     struct buffer *buf;
     int start, end;
{
  register INTERVAL intervals = buf->intervals;
  register INTERVAL i;
  register Lisp_Object hooks = Qnil;

  if (NULL_INTERVAL_P (intervals))
    return;

  if (start > end)
    {
      int temp = start;
      start = end;
      end = temp;
    }

  if (start == BUF_Z (buf))
    {
      if (BUF_Z (buf) == 1)
	abort ();

      i = find_interval (intervals, start - 1);
      if (! END_HUNGRY_P (i))
	return;
    }
  else
    i = find_interval (intervals, start);

  do
    {
      register Lisp_Object mod_hook;
      if (! INTERVAL_WRITABLE_P (i))
	error ("Attempt to write in a protected interval");
      mod_hook = Fget (Qmodification, i->plist);
      if (! EQ (mod_hook, Qnil))
	hooks = Fcons (mod_hook, hooks);
      i = next_interval (i);
    }
  while (! NULL_INTERVAL_P (i) && i->position <= end);

  hooks = Fnreverse (hooks);
  while (! EQ (hooks, Qnil))
    call2 (Fcar (hooks), i->position, i->position + LENGTH (i) - 1);
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

/* Produce an interval tree reflecting the interval structure in
   TREE from START to START + LENGTH. */

static INTERVAL
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
  new->total_length = got;
  copy_properties (i, new);

  t = new;
  while (got < length)
    {
      i = next_interval (i);
      t->right = make_interval ();
      t->right->parent = t;
      t->right->position = t->position + got - 1;

      t = t->right;
      t->total_length = length - got;
      copy_properties (i, t);
      got += LENGTH (i);
    }

  if (got > length)
    t->total_length -= (got - length);

  return balance_intervals (new);
}

/* Give buffer SINK the properties of buffer SOURCE from POSITION
   to END.  The properties are attached to SINK starting at position AT.

   No range checking is done. */

void
insert_interval_copy (source, position, end, sink, at)
     struct buffer *source, *sink;
     register int position, end, at;
{
  INTERVAL interval_copy = copy_intervals (source->intervals,
					   position, end - position);
  graft_intervals_into_buffer (interval_copy, at, sink);
}

/* Give STRING the properties of BUFFER from POSITION to LENGTH. */

void
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

INTERVAL
make_string_interval (string, start, length)
     struct Lisp_String *string;
     int start, length;
{
  if (start < 1 || start > string->size)
    error ("Interval index out of range");
  if (length < 1 || length > string->size - start + 1)
    error ("Interval won't fit");

  if (length == 0)
    return NULL_INTERVAL;

  return make_new_interval (string->intervals, start, length);
}

/* Create an interval of length LENGTH in buffer BUF at position START.  */

INTERVAL
make_buffer_interval (buf, start, length)
     struct buffer *buf;
     int start, length;
{
  if (start < BUF_BEG (buf) || start > BUF_Z (buf))
    error ("Interval index out of range");
  if (length < 1 || length > BUF_Z (buf) - start)
    error ("Interval won't fit");

  if (length == 0)
    return NULL_INTERVAL;

  return make_new_interval (buf->intervals, start, length);
}
