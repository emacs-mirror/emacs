/* Definitions and global variables for intervals.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

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

#ifdef USE_TEXT_PROPERTIES
#ifndef NORMAL_FACE
#include "dispextern.h"
#endif

#define NULL_INTERVAL 0
#define INTERVAL_DEFAULT NULL_INTERVAL

/* These are macros for dealing with the interval tree. */

/* Size of the structure used to represent an interval */
#define INTERVAL_SIZE (sizeof (struct interval))

/* Size of a pointer to an interval structure */
#define INTERVAL_PTR_SIZE (sizeof (struct interval *))

/* True if an interval pointer is null, or is a Lisp_Buffer or
   Lisp_String pointer (meaning it points to the owner of this
   interval tree). */
#define NULL_INTERVAL_P(i) ((i) == NULL_INTERVAL           \
			    || BUFFERP ((Lisp_Object)(i)) \
			    || STRINGP ((Lisp_Object)(i)))

/* True if this interval has no right child. */
#define NULL_RIGHT_CHILD(i) ((i)->right == NULL_INTERVAL)

/* True if this interval has no left child. */
#define NULL_LEFT_CHILD(i) ((i)->left == NULL_INTERVAL)

/* True if this interval has no parent. */
#define NULL_PARENT(i) (NULL_INTERVAL_P ((i)->parent))

/* True if this interval is the left child of some other interval. */
#define AM_LEFT_CHILD(i) (! NULL_INTERVAL_P ((i)->parent) \
			  && (i)->parent->left == (i))

/* True if this interval is the right child of some other interval. */
#define AM_RIGHT_CHILD(i) (! NULL_INTERVAL_P ((i)->parent) \
			   && (i)->parent->right == (i))

/* True if this interval has no children. */
#define LEAF_INTERVAL_P(i) ((i)->left == NULL_INTERVAL \
			    && (i)->right == NULL_INTERVAL)

/* True if this interval has no parent and is therefore the root. */
#define ROOT_INTERVAL_P(i) (NULL_PARENT (i))

/* True if this interval is the only interval in the interval tree. */
#define ONLY_INTERVAL_P(i) (ROOT_INTERVAL_P ((i)) && LEAF_INTERVAL_P ((i)))

/* True if this interval has both left and right children. */
#define BOTH_KIDS_P(i) ((i)->left != NULL_INTERVAL     \
			&& (i)->right != NULL_INTERVAL)

/* The total size of all text represented by this interval and all its
   children in the tree.   This is zero if the interval is null. */
#define TOTAL_LENGTH(i) ((i) == NULL_INTERVAL ? 0 : (i)->total_length)

/* The size of text represented by this interval alone. */
#define LENGTH(i) ((i) == NULL_INTERVAL ? 0 : (TOTAL_LENGTH ((i))          \
					       - TOTAL_LENGTH ((i)->right) \
					       - TOTAL_LENGTH ((i)->left)))

/* The position of the character just past the end of I.  Note that
   the position cache i->position must be valid for this to work. */
#define INTERVAL_LAST_POS(i) ((i)->position + LENGTH ((i)))

/* The total size of the left subtree of this interval. */
#define LEFT_TOTAL_LENGTH(i) ((i)->left ? (i)->left->total_length : 0)

/* The total size of the right subtree of this interval. */
#define RIGHT_TOTAL_LENGTH(i) ((i)->right ? (i)->right->total_length : 0)


/* These macros are for dealing with the interval properties. */

/* True if this is a default interval, which is the same as being null
   or having no properties. */
#define DEFAULT_INTERVAL_P(i) (NULL_INTERVAL_P (i) || EQ ((i)->plist, Qnil))

/* Reset this interval to its vanilla, or no-property state. */
#define RESET_INTERVAL(i) \
{ \
    (i)->total_length = (i)->position = 0;    \
    (i)->left = (i)->right = NULL_INTERVAL;   \
    (i)->parent = NULL_INTERVAL;              \
    (i)->write_protect = 0;                   \
    (i)->visible = 0;                         \
    (i)->front_sticky = (i)->rear_sticky = 0; \
    (i)->plist = Qnil;         	              \
}

/* Copy the cached property values of interval FROM to interval TO. */
#define COPY_INTERVAL_CACHE(from,to) \
{ \
  (to)->write_protect = (from)->write_protect; \
  (to)->visible = (from)->visible;             \
  (to)->front_sticky = (from)->front_sticky;   \
  (to)->rear_sticky = (from)->rear_sticky;     \
}

/* Copy only the set bits of FROM's cache. */
#define MERGE_INTERVAL_CACHE(from,to) \
{ \
  if ((from)->write_protect) (to)->write_protect = 1; \
  if ((from)->visible) (to)->visible = 1;             \
  if ((from)->front_sticky) (to)->front_sticky = 1;   \
  if ((from)->rear_sticky) (to)->rear_sticky = 1;     \
}

/* Macro determining whether the properties of an interval being
   inserted should be merged with the properties of the text where
   they are being inserted. */
#define MERGE_INSERTIONS(i) 1

/* Macro determining if an invisible interval should be displayed
   as a special glyph, or not at all. */
#define DISPLAY_INVISIBLE_GLYPH(i) 0

/* Is this interval visible?  Replace later with cache access */
#define INTERVAL_VISIBLE_P(i) \
  (! NULL_INTERVAL_P (i) && NILP (textget ((i)->plist, Qinvisible)))

/* Is this interval writable?  Replace later with cache access */
#define INTERVAL_WRITABLE_P(i)					\
  (! NULL_INTERVAL_P (i)					\
   && (NILP (textget ((i)->plist, Qread_only))			\
       || ((CONSP (Vinhibit_read_only)				\
	    ? !NILP (Fmemq (textget ((i)->plist, Qread_only),	\
			    Vinhibit_read_only))		\
	    : !NILP (Vinhibit_read_only)))))			\

/* Macros to tell whether insertions before or after this interval
   should stick to it. */
/* Replace later with cache access */
/*#define FRONT_STICKY_P(i) ((i)->front_sticky != 0)
  #define END_STICKY_P(i) ((i)->rear_sticky != 0)*/
#define FRONT_STICKY_P(i) \
  (! NULL_INTERVAL_P (i) && ! NILP (textget ((i)->plist, Qfront_sticky)))
#define END_NONSTICKY_P(i) \
  (! NULL_INTERVAL_P (i) && ! NILP (textget ((i)->plist, Qrear_nonsticky)))


/* Declared in alloc.c */

extern INTERVAL make_interval ();

/* Declared in intervals.c */

extern INTERVAL create_root_interval ();
extern void copy_properties ();
extern int intervals_equal ();
extern void traverse_intervals ();
extern INTERVAL split_interval_right (), split_interval_left ();
extern INLINE INTERVAL find_interval ();
extern INTERVAL next_interval (), previous_interval ();
extern INTERVAL merge_interval_left (), merge_interval_right ();
extern void delete_interval ();
extern INLINE void offset_intervals ();
extern void graft_intervals_into_buffer ();
extern void set_point ();
extern INLINE void temp_set_point ();
extern void verify_interval_modification ();
extern INTERVAL balance_intervals ();
extern INLINE void copy_intervals_to_string ();
extern INTERVAL copy_intervals ();
extern Lisp_Object textget ();
extern Lisp_Object textget_direct ();
extern Lisp_Object get_local_map ();

/* Declared in textprop.c */

/* Types of hooks. */
extern Lisp_Object Qmouse_left;
extern Lisp_Object Qmouse_entered;
extern Lisp_Object Qpoint_left;
extern Lisp_Object Qpoint_entered;
extern Lisp_Object Qmodification_hooks;
extern Lisp_Object Qcategory;
extern Lisp_Object Qlocal_map;

/* Visual properties text (including strings) may have. */
extern Lisp_Object Qforeground, Qbackground, Qfont, Qunderline, Qstipple;
extern Lisp_Object Qinvisible, Qintangible, Qread_only;

extern Lisp_Object Vinhibit_point_motion_hooks;

/* Sticky properties */
extern Lisp_Object Qfront_sticky, Qrear_nonsticky;

extern Lisp_Object Fget_char_property (), Fget_text_property ();
extern Lisp_Object Ftext_properties_at ();
extern Lisp_Object Fnext_property_change (), Fprevious_property_change ();
extern Lisp_Object Fadd_text_properties (), Fset_text_properties ();
extern Lisp_Object Fremove_text_properties (), Ferase_text_properties ();
extern Lisp_Object Ftext_property_any (), Ftext_property_not_all ();
extern Lisp_Object copy_text_properties ();

extern void syms_of_textprop ();

#else  /* don't support text properties */

#define NULL_INTERVAL_P(i) 1
#define INTERVAL_SIZE 0
#define INTERVAL_PTR_SIZE 0

#define copy_intervals_to_string(string,buffer,position,length)
#define verify_interval_modification(buffer,start,end)
#define insert_interval_copy(source,position,end,sink,at)
#define graft_intervals_into_buffer(tree,position,bufferptr)
#define offset_intervals(buffer,position,length)
#define copy_intervals(tree,start,length)

#define syms_of_textprop()

#endif /* don't support text properties */
