/* Definitions and global variables for intervals.
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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

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
#ifdef NO_UNION_TYPE
#define NULL_INTERVAL_P(i) ((i) == NULL_INTERVAL           \
			    || BUFFERP ((Lisp_Object)(i)) \
			    || STRINGP ((Lisp_Object)(i)))
#else
#define NULL_INTERVAL_P(i) ((i) == NULL_INTERVAL           \
			    || BUFFERP ((Lisp_Object){(EMACS_INT)(i)}) \
			    || STRINGP ((Lisp_Object){(EMACS_INT)(i)}))
#endif

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
#define FRONT_NONSTICKY_P(i) \
  (! NULL_INTERVAL_P (i) && ! EQ (Qt, textget ((i)->plist, Qfront_sticky)))


/* If PROP is the `invisible' property of a character,
   this is 1 if the character should be treated as invisible.  */

#define TEXT_PROP_MEANS_INVISIBLE(prop)				\
  (EQ (current_buffer->invisibility_spec, Qt)			\
   ? ! NILP (prop)						\
   : invisible_p (prop, current_buffer->invisibility_spec))

/* If PROP is the `invisible' property of a character,
   this is 1 if the character should be treated as invisible
   and should have an ellipsis.  */

#define TEXT_PROP_MEANS_INVISIBLE_WITH_ELLIPSIS(prop)		\
  (EQ (current_buffer->invisibility_spec, Qt)			\
   ? 0								\
   : invisible_ellipsis_p (prop, current_buffer->invisibility_spec))

/* Declared in alloc.c */

extern INTERVAL make_interval P_ ((void));

/* Declared in intervals.c */

extern INTERVAL create_root_interval P_ ((Lisp_Object));
extern void copy_properties P_ ((INTERVAL, INTERVAL));
extern int intervals_equal P_ ((INTERVAL, INTERVAL));
extern void traverse_intervals P_ ((INTERVAL, int, int,
				    void (*) (INTERVAL, Lisp_Object),
				    Lisp_Object));
extern INTERVAL split_interval_right P_ ((INTERVAL, int));
extern INTERVAL split_interval_left P_ ((INTERVAL, int));
extern INTERVAL find_interval P_ ((INTERVAL, int));
extern INTERVAL next_interval P_ ((INTERVAL));
extern INTERVAL previous_interval P_ ((INTERVAL));
extern INTERVAL merge_interval_left P_ ((INTERVAL));
extern INTERVAL merge_interval_right P_ ((INTERVAL));
extern void delete_interval P_ ((INTERVAL));
extern INLINE void offset_intervals P_ ((struct buffer *, int, int));
extern void graft_intervals_into_buffer P_ ((INTERVAL, int, int,
					     struct buffer *, int));
extern void set_point P_ ((struct buffer *, int));
extern INLINE void temp_set_point P_ ((struct buffer *, int));
extern void set_point_both P_ ((struct buffer *, int, int));
extern INLINE void temp_set_point_both P_ ((struct buffer *, int, int));
extern void verify_interval_modification P_ ((struct buffer *, int, int));
extern INTERVAL balance_intervals P_ ((INTERVAL));
extern INLINE void copy_intervals_to_string P_ ((Lisp_Object, struct buffer *,
						 int, int));
extern INTERVAL copy_intervals P_ ((INTERVAL, int, int));
extern Lisp_Object textget P_ ((Lisp_Object, Lisp_Object));
extern void move_if_not_intangible P_ ((int));
extern Lisp_Object get_local_map P_ ((int, struct buffer *));
extern INTERVAL update_interval P_ ((INTERVAL, int));
extern void set_intervals_multibyte P_ ((int));

/* Defined in xdisp.c */
extern int invisible_ellipsis_p P_ ((Lisp_Object, Lisp_Object));

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
extern Lisp_Object Vdefault_text_properties;

/* Sticky properties */
extern Lisp_Object Qfront_sticky, Qrear_nonsticky;

EXFUN (Fget_char_property, 3);
EXFUN (Fget_text_property, 3);
EXFUN (Ftext_properties_at, 2);
EXFUN (Fnext_property_change, 3);
EXFUN (Fprevious_property_change, 3);
EXFUN (Fadd_text_properties, 4);
EXFUN (Fset_text_properties, 4);
EXFUN (Fremove_text_properties, 4);
EXFUN (Ftext_property_any, 5);
EXFUN (Ftext_property_not_all, 5);
extern Lisp_Object copy_text_properties P_ ((Lisp_Object, Lisp_Object,
					     Lisp_Object, Lisp_Object,
					     Lisp_Object, Lisp_Object));

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
