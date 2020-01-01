/* Definitions and global variables for intervals.
   Copyright (C) 1993-1994, 2000-2020 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#ifndef EMACS_INTERVALS_H
#define EMACS_INTERVALS_H

#include "buffer.h"
#include "lisp.h"

INLINE_HEADER_BEGIN

/* Basic data type for use of intervals.  */

struct interval
{
  /* The first group of entries deal with the tree structure.  */
  ptrdiff_t total_length;       /* Length of myself and both children.  */
  ptrdiff_t position;	        /* Cache of interval's character position.  */
                                /* This field is valid in the final
                                   target interval returned by
                                   find_interval, next_interval,
                                   previous_interval and
                                   update_interval.  It cannot be
                                   depended upon in any intermediate
                                   intervals traversed by these
                                   functions, or any other
                                   interval. */
  struct interval *left;	/* Intervals which precede me.  */
  struct interval *right;	/* Intervals which succeed me.  */

  /* Parent in the tree, or the Lisp_Object containing this interval tree.  */
  union
  {
    struct interval *interval;
    Lisp_Object obj;
  } up;
  bool_bf up_obj : 1;

  bool_bf gcmarkbit : 1;

  /* The remaining components are `properties' of the interval.
     The first four are duplicates for things which can be on the list,
     for purposes of speed.  */

  bool_bf write_protect : 1;	    /* True means can't modify.  */
  bool_bf visible : 1;		    /* False means don't display.  */
  bool_bf front_sticky : 1;	    /* True means text inserted just
				       before this interval goes into it.  */
  bool_bf rear_sticky : 1;	    /* Likewise for just after it.  */
  Lisp_Object plist;		    /* Other properties.  */
};

/* These are macros for dealing with the interval tree.  */

/* True if this interval has no right child.  */
#define NULL_RIGHT_CHILD(i) ((i)->right == NULL)

/* True if this interval has no left child.  */
#define NULL_LEFT_CHILD(i) ((i)->left == NULL)

/* True if this interval has no parent.  */
#define NULL_PARENT(i) ((i)->up_obj || (i)->up.interval == 0)

/* True if this interval is the left child of some other interval.  */
#define AM_LEFT_CHILD(i)					\
  (! NULL_PARENT (i) && INTERVAL_PARENT (i)->left == (i))

/* True if this interval is the right child of some other interval.  */
#define AM_RIGHT_CHILD(i)					\
  (! NULL_PARENT (i) && INTERVAL_PARENT (i)->right == (i))

/* True if this interval has no children.  */
#define LEAF_INTERVAL_P(i) ((i)->left == NULL && (i)->right == NULL)

/* True if this interval has no parent and is therefore the root.  */
#define ROOT_INTERVAL_P(i) NULL_PARENT (i)

/* True if this interval is the only interval in the interval tree.  */
#define ONLY_INTERVAL_P(i) (ROOT_INTERVAL_P (i) && LEAF_INTERVAL_P (i))

/* True if this interval has both left and right children.  */
#define BOTH_KIDS_P(i) ((i)->left != NULL && (i)->right != NULL)

/* The total size of all text represented by this interval and all its
   children in the tree.   This is zero if the interval is null.  */
#define TOTAL_LENGTH(i) ((i) == NULL ? 0 : (i)->total_length)

/* The size of text represented by this interval alone.  */
#define LENGTH(i) ((i)->total_length			\
		   - TOTAL_LENGTH ((i)->right)		\
		   - TOTAL_LENGTH ((i)->left))

/* The position of the character just past the end of I.  Note that
   the position cache i->position must be valid for this to work.  */
#define INTERVAL_LAST_POS(i) ((i)->position + LENGTH (i))

/* The total size of the left subtree of this interval.  */
#define LEFT_TOTAL_LENGTH(i) ((i)->left ? (i)->left->total_length : 0)

/* The total size of the right subtree of this interval.  */
#define RIGHT_TOTAL_LENGTH(i) ((i)->right ? (i)->right->total_length : 0)

/* These macros are for dealing with the interval properties.  */

/* True if this is a default interval, which is the same as being null
   or having no properties.  */
#define DEFAULT_INTERVAL_P(i) (!i || NILP ((i)->plist))

/* Test what type of parent we have.  Three possibilities: another
   interval, a buffer or string object, or NULL.  */
#define INTERVAL_HAS_PARENT(i) (! (i)->up_obj && (i)->up.interval != 0)
#define INTERVAL_HAS_OBJECT(i) ((i)->up_obj)

/* Use these macros to get parent of an interval.

   The choice of macros is dependent on the type needed.  Don't add
   casts to get around this, it will break some development work in
   progress.  */

#define INTERVAL_PARENT(i)					\
   (eassert ((i) != 0 && ! (i)->up_obj), (i)->up.interval)

#define GET_INTERVAL_OBJECT(d,s) (eassert ((s)->up_obj), (d) = (s)->up.obj)

/* Use these functions to set Lisp_Object
   or pointer slots of struct interval.  */

INLINE void
set_interval_object (INTERVAL i, Lisp_Object obj)
{
  eassert (BUFFERP (obj) || STRINGP (obj));
  i->up_obj = 1;
  i->up.obj = obj;
}

INLINE void
set_interval_parent (INTERVAL i, INTERVAL parent)
{
  i->up_obj = false;
  i->up.interval = parent;
}

INLINE void
set_interval_plist (INTERVAL i, Lisp_Object plist)
{
  i->plist = plist;
}

/* Get the parent interval, if any, otherwise a null pointer.  Useful
   for walking up to the root in a "for" loop; use this to get the
   "next" value, and test the result to see if it's NULL.  */
#define INTERVAL_PARENT_OR_NULL(i) \
   (INTERVAL_HAS_PARENT (i) ? INTERVAL_PARENT (i) : 0)

/* Reset this interval to its vanilla, or no-property state.  */
#define RESET_INTERVAL(i)		      \
 do {					      \
  (i)->total_length = (i)->position = 0;      \
  (i)->left = (i)->right = NULL;	      \
  set_interval_parent (i, NULL);	      \
  (i)->write_protect = false;		      \
  (i)->visible = false;			      \
  (i)->front_sticky = (i)->rear_sticky = false;	\
  set_interval_plist (i, Qnil);		      \
 } while (false)

/* Copy the cached property values of interval FROM to interval TO.  */
#define COPY_INTERVAL_CACHE(from,to)		\
 do {						\
  (to)->write_protect = (from)->write_protect;	\
  (to)->visible = (from)->visible;		\
  (to)->front_sticky = (from)->front_sticky;	\
  (to)->rear_sticky = (from)->rear_sticky;	\
 } while (false)

/* Copy only the set bits of FROM's cache.  */
#define MERGE_INTERVAL_CACHE(from,to)				\
 do {								\
  if ((from)->write_protect) (to)->write_protect = true;	\
  if ((from)->visible) (to)->visible = true;			\
  if ((from)->front_sticky) (to)->front_sticky = true;		\
  if ((from)->rear_sticky) (to)->rear_sticky = true;		\
 } while (false)

/* Is this interval visible?  Replace later with cache access.  */
#define INTERVAL_VISIBLE_P(i) \
  (i && NILP (textget ((i)->plist, Qinvisible)))

/* Is this interval writable?  Replace later with cache access.  */
#define INTERVAL_WRITABLE_P(i)					\
  (NILP (textget ((i)->plist, Qread_only))			\
   || !NILP (textget ((i)->plist, Qinhibit_read_only))		\
   || ((CONSP (Vinhibit_read_only)				\
	? !NILP (Fmemq (textget ((i)->plist, Qread_only),	\
			Vinhibit_read_only))			\
	: !NILP (Vinhibit_read_only))))

/* Macros to tell whether insertions before or after this interval
   should stick to it.  Now we have Vtext_property_default_nonsticky,
   so these macros are unreliable now and never used.  */

#if false
#define FRONT_STICKY_P(i)				\
  (i && ! NILP (textget ((i)->plist, Qfront_sticky)))
#define END_NONSTICKY_P(i)				\
  (i && ! NILP (textget ((i)->plist, Qrear_nonsticky)))
#define FRONT_NONSTICKY_P(i)				\
  (i && ! EQ (Qt, textget ((i)->plist, Qfront_sticky)))
#endif

/* If PROP is the `invisible' property of a character,
   this is 1 if the character should be treated as invisible,
   and 2 if it is invisible but with an ellipsis.  */

#define TEXT_PROP_MEANS_INVISIBLE(prop)					\
  (EQ (BVAR (current_buffer, invisibility_spec), Qt)			\
   ? !NILP (prop)							\
   : invisible_prop (prop, BVAR (current_buffer, invisibility_spec)))

/* Declared in alloc.c.  */

extern INTERVAL make_interval (void);

/* Declared in intervals.c.  */

extern INTERVAL create_root_interval (Lisp_Object);
extern void copy_properties (INTERVAL, INTERVAL);
extern bool intervals_equal (INTERVAL, INTERVAL);
extern void traverse_intervals (INTERVAL, ptrdiff_t,
                                void (*) (INTERVAL, Lisp_Object),
                                Lisp_Object);
extern void traverse_intervals_noorder (INTERVAL,
					void (*) (INTERVAL, void *), void *);
extern INTERVAL split_interval_right (INTERVAL, ptrdiff_t);
extern INTERVAL split_interval_left (INTERVAL, ptrdiff_t);
extern INTERVAL find_interval (INTERVAL, ptrdiff_t);
extern INTERVAL next_interval (INTERVAL);
extern INTERVAL previous_interval (INTERVAL);
extern INTERVAL merge_interval_left (INTERVAL);
extern void offset_intervals (struct buffer *, ptrdiff_t, ptrdiff_t);
extern void graft_intervals_into_buffer (INTERVAL, ptrdiff_t, ptrdiff_t,
                                         struct buffer *, bool);
extern void verify_interval_modification (struct buffer *,
					  ptrdiff_t, ptrdiff_t);
extern INTERVAL balance_intervals (INTERVAL);
extern void copy_intervals_to_string (Lisp_Object, struct buffer *,
                                             ptrdiff_t, ptrdiff_t);
extern INTERVAL copy_intervals (INTERVAL, ptrdiff_t, ptrdiff_t);
extern bool compare_string_intervals (Lisp_Object, Lisp_Object);
extern Lisp_Object textget (Lisp_Object, Lisp_Object);
extern Lisp_Object lookup_char_property (Lisp_Object, Lisp_Object, bool);
extern void move_if_not_intangible (ptrdiff_t);
extern bool get_property_and_range (ptrdiff_t, Lisp_Object, Lisp_Object *,
				    ptrdiff_t *, ptrdiff_t *, Lisp_Object);
extern Lisp_Object get_local_map (ptrdiff_t, struct buffer *, Lisp_Object);
extern INTERVAL update_interval (INTERVAL, ptrdiff_t);
extern void set_intervals_multibyte (bool);
extern INTERVAL validate_interval_range (Lisp_Object, Lisp_Object *,
                                         Lisp_Object *, bool);
extern INTERVAL interval_of (ptrdiff_t, Lisp_Object);

/* Defined in xdisp.c.  */
extern int invisible_prop (Lisp_Object, Lisp_Object);

/* Defined in textprop.c.  */
extern Lisp_Object copy_text_properties (Lisp_Object, Lisp_Object,
                                         Lisp_Object, Lisp_Object,
                                         Lisp_Object, Lisp_Object);
extern Lisp_Object set_text_properties (Lisp_Object, Lisp_Object,
                                        Lisp_Object, Lisp_Object,
                                        Lisp_Object);
extern void set_text_properties_1 (Lisp_Object, Lisp_Object,
                                   Lisp_Object, Lisp_Object, INTERVAL);

Lisp_Object text_property_list (Lisp_Object, Lisp_Object, Lisp_Object,
                                Lisp_Object);
void add_text_properties_from_list (Lisp_Object, Lisp_Object, Lisp_Object);
Lisp_Object extend_property_ranges (Lisp_Object, Lisp_Object, Lisp_Object);
Lisp_Object get_char_property_and_overlay (Lisp_Object, Lisp_Object,
                                           Lisp_Object, Lisp_Object *);
extern int text_property_stickiness (Lisp_Object prop, Lisp_Object pos,
                                     Lisp_Object buffer);

extern void syms_of_textprop (void);

INLINE_HEADER_END

#endif /* EMACS_INTERVALS_H */
