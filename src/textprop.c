/* Interface code for dealing with text properties.
   Copyright (C) 1993 Free Software Foundation, Inc.

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
#include "lisp.h"
#include "intervals.h"
#include "buffer.h"


/* NOTES:  previous- and next- property change will have to skip
  zero-length intervals if they are implemented.  This could be done
  inside next_interval and previous_interval.

  set_properties needs to deal with the interval property cache.

  It is assumed that for any interval plist, a property appears
  only once on the list.  Although some code i.e., remove_properties,
  handles the more general case, the uniqueness of properties is
  necessary for the system to remain consistent.  This requirement
  is enforced by the subrs installing properties onto the intervals. */

/* The rest of the file is within this conditional */
#ifdef USE_TEXT_PROPERTIES

/* Types of hooks. */
Lisp_Object Qmouse_left;
Lisp_Object Qmouse_entered;
Lisp_Object Qpoint_left;
Lisp_Object Qpoint_entered;
Lisp_Object Qcategory;
Lisp_Object Qlocal_map;

/* Visual properties text (including strings) may have. */
Lisp_Object Qforeground, Qbackground, Qfont, Qunderline, Qstipple;
Lisp_Object Qinvisible, Qread_only, Qhidden;

/* Sticky properties */
Lisp_Object Qfront_sticky, Qrear_nonsticky;

/* If o1 is a cons whose cdr is a cons, return non-zero and set o2 to
   the o1's cdr.  Otherwise, return zero.  This is handy for
   traversing plists.  */
#define PLIST_ELT_P(o1, o2) (CONSP (o1) && CONSP ((o2) = XCONS (o1)->cdr))

Lisp_Object Vinhibit_point_motion_hooks;


/* Extract the interval at the position pointed to by BEGIN from
   OBJECT, a string or buffer.  Additionally, check that the positions
   pointed to by BEGIN and END are within the bounds of OBJECT, and
   reverse them if *BEGIN is greater than *END.  The objects pointed
   to by BEGIN and END may be integers or markers; if the latter, they
   are coerced to integers.

   When OBJECT is a string, we increment *BEGIN and *END
   to make them origin-one.

   Note that buffer points don't correspond to interval indices.
   For example, point-max is 1 greater than the index of the last
   character.  This difference is handled in the caller, which uses
   the validated points to determine a length, and operates on that.
   Exceptions are Ftext_properties_at, Fnext_property_change, and
   Fprevious_property_change which call this function with BEGIN == END.
   Handle this case specially.

   If FORCE is soft (0), it's OK to return NULL_INTERVAL.  Otherwise,
   create an interval tree for OBJECT if one doesn't exist, provided
   the object actually contains text.  In the current design, if there
   is no text, there can be no text properties.  */

#define soft 0
#define hard 1

static INTERVAL
validate_interval_range (object, begin, end, force)
     Lisp_Object object, *begin, *end;
     int force;
{
  register INTERVAL i;
  int searchpos;

  CHECK_STRING_OR_BUFFER (object, 0);
  CHECK_NUMBER_COERCE_MARKER (*begin, 0);
  CHECK_NUMBER_COERCE_MARKER (*end, 0);

  /* If we are asked for a point, but from a subr which operates
     on a range, then return nothing. */
  if (*begin == *end && begin != end)
    return NULL_INTERVAL;

  if (XINT (*begin) > XINT (*end))
    {
      Lisp_Object n;
      n = *begin;
      *begin = *end;
      *end = n;
    }

  if (XTYPE (object) == Lisp_Buffer)
    {
      register struct buffer *b = XBUFFER (object);

      if (!(BUF_BEGV (b) <= XINT (*begin) && XINT (*begin) <= XINT (*end)
	    && XINT (*end) <= BUF_ZV (b)))
	args_out_of_range (*begin, *end);
      i = b->intervals;

      /* If there's no text, there are no properties. */
      if (BUF_BEGV (b) == BUF_ZV (b))
	return NULL_INTERVAL;

      searchpos = XINT (*begin);
    }
  else
    {
      register struct Lisp_String *s = XSTRING (object);

      if (! (0 <= XINT (*begin) && XINT (*begin) <= XINT (*end)
	     && XINT (*end) <= s->size))
	args_out_of_range (*begin, *end);
      /* User-level Positions in strings start with 0,
	 but the interval code always wants positions starting with 1.  */
      XFASTINT (*begin) += 1;
      if (begin != end)
	XFASTINT (*end) += 1;
      i = s->intervals;

      if (s->size == 0)
	return NULL_INTERVAL;

      searchpos = XINT (*begin);
    }

  if (NULL_INTERVAL_P (i))
    return (force ? create_root_interval (object) : i);
    
  return find_interval (i, searchpos);
}

/* Validate LIST as a property list.  If LIST is not a list, then
   make one consisting of (LIST nil).  Otherwise, verify that LIST
   is even numbered and thus suitable as a plist. */

static Lisp_Object
validate_plist (list)
     Lisp_Object list;
{
  if (NILP (list))
    return Qnil;

  if (CONSP (list))
    {
      register int i;
      register Lisp_Object tail;
      for (i = 0, tail = list; !NILP (tail); i++)
	{
	  tail = Fcdr (tail);
	  QUIT;
	}
      if (i & 1)
	error ("Odd length text property list");
      return list;
    }

  return Fcons (list, Fcons (Qnil, Qnil));
}

/* Return nonzero if interval I has all the properties,
   with the same values, of list PLIST. */

static int
interval_has_all_properties (plist, i)
     Lisp_Object plist;
     INTERVAL i;
{
  register Lisp_Object tail1, tail2, sym1, sym2;
  register int found;

  /* Go through each element of PLIST. */
  for (tail1 = plist; ! NILP (tail1); tail1 = Fcdr (Fcdr (tail1)))
    {
      sym1 = Fcar (tail1);
      found = 0;

      /* Go through I's plist, looking for sym1 */
      for (tail2 = i->plist; ! NILP (tail2); tail2 = Fcdr (Fcdr (tail2)))
	if (EQ (sym1, Fcar (tail2)))
	  {
	    /* Found the same property on both lists.  If the
	       values are unequal, return zero. */
	    if (! EQ (Fcar (Fcdr (tail1)), Fcar (Fcdr (tail2))))
	      return 0;

	    /* Property has same value on both lists;  go to next one. */
	    found = 1;
	    break;
	  }

      if (! found)
	return 0;
    }

  return 1;
}

/* Return nonzero if the plist of interval I has any of the
   properties of PLIST, regardless of their values. */

static INLINE int
interval_has_some_properties (plist, i)
     Lisp_Object plist;
     INTERVAL i;
{
  register Lisp_Object tail1, tail2, sym;

  /* Go through each element of PLIST. */
  for (tail1 = plist; ! NILP (tail1); tail1 = Fcdr (Fcdr (tail1)))
    {
      sym = Fcar (tail1);

      /* Go through i's plist, looking for tail1 */
      for (tail2 = i->plist; ! NILP (tail2); tail2 = Fcdr (Fcdr (tail2)))
	if (EQ (sym, Fcar (tail2)))
	  return 1;
    }

  return 0;
}

/* Changing the plists of individual intervals.  */

/* Return the value of PROP in property-list PLIST, or Qunbound if it
   has none.  */
static int
property_value (plist, prop)
{
  Lisp_Object value;

  while (PLIST_ELT_P (plist, value))
    if (EQ (XCONS (plist)->car, prop))
      return XCONS (value)->car;
    else
      plist = XCONS (value)->cdr;

  return Qunbound;
}

/* Set the properties of INTERVAL to PROPERTIES,
   and record undo info for the previous values.
   OBJECT is the string or buffer that INTERVAL belongs to.  */

static void
set_properties (properties, interval, object)
     Lisp_Object properties, object;
     INTERVAL interval;
{
  Lisp_Object sym, value;

  if (BUFFERP (object))
    {
      /* For each property in the old plist which is missing from PROPERTIES,
	 or has a different value in PROPERTIES, make an undo record.  */
      for (sym = interval->plist;
	   PLIST_ELT_P (sym, value);
	   sym = XCONS (value)->cdr)
	if (! EQ (property_value (properties, XCONS (sym)->car),
		  XCONS (value)->car))
	  {
	    modify_region (XBUFFER (object),
			   make_number (interval->position),
			   make_number (interval->position + LENGTH (interval)));
	    record_property_change (interval->position, LENGTH (interval),
				    XCONS (sym)->car, XCONS (value)->car,
				    object);
	  }

      /* For each new property that has no value at all in the old plist,
	 make an undo record binding it to nil, so it will be removed.  */
      for (sym = properties;
	   PLIST_ELT_P (sym, value);
	   sym = XCONS (value)->cdr)
	if (EQ (property_value (interval->plist, XCONS (sym)->car), Qunbound))
	  {
	    modify_region (XBUFFER (object),
			   make_number (interval->position),
			   make_number (interval->position + LENGTH (interval)));
	    record_property_change (interval->position, LENGTH (interval),
				    XCONS (sym)->car, Qnil,
				    object);
	  }
    }

  /* Store new properties.  */
  interval->plist = Fcopy_sequence (properties);
}

/* Add the properties of PLIST to the interval I, or set
   the value of I's property to the value of the property on PLIST
   if they are different.

   OBJECT should be the string or buffer the interval is in.

   Return nonzero if this changes I (i.e., if any members of PLIST
   are actually added to I's plist) */

static int
add_properties (plist, i, object)
     Lisp_Object plist;
     INTERVAL i;
     Lisp_Object object;
{
  register Lisp_Object tail1, tail2, sym1, val1;
  register int changed = 0;
  register int found;

  /* Go through each element of PLIST. */
  for (tail1 = plist; ! NILP (tail1); tail1 = Fcdr (Fcdr (tail1)))
    {
      sym1 = Fcar (tail1);
      val1 = Fcar (Fcdr (tail1));
      found = 0;

      /* Go through I's plist, looking for sym1 */
      for (tail2 = i->plist; ! NILP (tail2); tail2 = Fcdr (Fcdr (tail2)))
	if (EQ (sym1, Fcar (tail2)))
	  {
	    register Lisp_Object this_cdr = Fcdr (tail2);

	    /* Found the property.  Now check its value. */
	    found = 1;

	    /* The properties have the same value on both lists.
	       Continue to the next property. */
	    if (EQ (val1, Fcar (this_cdr)))
	      break;

	    /* Record this change in the buffer, for undo purposes.  */
	    if (XTYPE (object) == Lisp_Buffer)
	      {
		modify_region (XBUFFER (object),
			       make_number (i->position),
			       make_number (i->position + LENGTH (i)));
		record_property_change (i->position, LENGTH (i),
					sym1, Fcar (this_cdr), object);
	      }

	    /* I's property has a different value -- change it */
	    Fsetcar (this_cdr, val1);
	    changed++;
	    break;
	  }

      if (! found)
	{
	  /* Record this change in the buffer, for undo purposes.  */
	  if (XTYPE (object) == Lisp_Buffer)
	    {
	      modify_region (XBUFFER (object),
			     make_number (i->position),
			     make_number (i->position + LENGTH (i)));
	      record_property_change (i->position, LENGTH (i),
				      sym1, Qnil, object);
	    }
	  i->plist = Fcons (sym1, Fcons (val1, i->plist));
	  changed++;
	}
    }

  return changed;
}

/* For any members of PLIST which are properties of I, remove them
   from I's plist.
   OBJECT is the string or buffer containing I.  */

static int
remove_properties (plist, i, object)
     Lisp_Object plist;
     INTERVAL i;
     Lisp_Object object;
{
  register Lisp_Object tail1, tail2, sym;
  register Lisp_Object current_plist = i->plist;
  register int changed = 0;

  /* Go through each element of plist. */
  for (tail1 = plist; ! NILP (tail1); tail1 = Fcdr (Fcdr (tail1)))
    {
      sym = Fcar (tail1);

      /* First, remove the symbol if its at the head of the list */
      while (! NILP (current_plist) && EQ (sym, Fcar (current_plist)))
	{
	  if (XTYPE (object) == Lisp_Buffer)
	    {
	      modify_region (XBUFFER (object),
			     make_number (i->position),
			     make_number (i->position + LENGTH (i)));
	      record_property_change (i->position, LENGTH (i),
				      sym, Fcar (Fcdr (current_plist)),
				      object);
	    }

	  current_plist = Fcdr (Fcdr (current_plist));
	  changed++;
	}

      /* Go through i's plist, looking for sym */
      tail2 = current_plist;
      while (! NILP (tail2))
	{
	  register Lisp_Object this = Fcdr (Fcdr (tail2));
	  if (EQ (sym, Fcar (this)))
	    {
	      if (XTYPE (object) == Lisp_Buffer)
		{
		  modify_region (XBUFFER (object),
				 make_number (i->position),
				 make_number (i->position + LENGTH (i)));
		  record_property_change (i->position, LENGTH (i),
					  sym, Fcar (Fcdr (this)), object);
		}

	      Fsetcdr (Fcdr (tail2), Fcdr (Fcdr (this)));
	      changed++;
	    }
	  tail2 = this;
	}
    }

  if (changed)
    i->plist = current_plist;
  return changed;
}

#if 0
/* Remove all properties from interval I.  Return non-zero
   if this changes the interval. */

static INLINE int
erase_properties (i)
     INTERVAL i;
{
  if (NILP (i->plist))
    return 0;

  i->plist = Qnil;
  return 1;
}
#endif

DEFUN ("text-properties-at", Ftext_properties_at,
       Stext_properties_at, 1, 2, 0,
  "Return the list of properties held by the character at POSITION\n\
in optional argument OBJECT, a string or buffer.  If nil, OBJECT\n\
defaults to the current buffer.\n\
If POSITION is at the end of OBJECT, the value is nil.")
  (pos, object)
     Lisp_Object pos, object;
{
  register INTERVAL i;

  if (NILP (object))
    XSET (object, Lisp_Buffer, current_buffer);

  i = validate_interval_range (object, &pos, &pos, soft);
  if (NULL_INTERVAL_P (i))
    return Qnil;
  /* If POS is at the end of the interval,
     it means it's the end of OBJECT.
     There are no properties at the very end,
     since no character follows.  */
  if (XINT (pos) == LENGTH (i) + i->position)
    return Qnil;

  return i->plist;
}

DEFUN ("get-text-property", Fget_text_property, Sget_text_property, 2, 3, 0,
  "Return the value of position POS's property PROP, in OBJECT.\n\
OBJECT is optional and defaults to the current buffer.\n\
If POSITION is at the end of OBJECT, the value is nil.")
  (pos, prop, object)
     Lisp_Object pos, object;
     register Lisp_Object prop;
{
  register INTERVAL i;
  register Lisp_Object tail;

  if (NILP (object))
    XSET (object, Lisp_Buffer, current_buffer);
  i = validate_interval_range (object, &pos, &pos, soft);
  if (NULL_INTERVAL_P (i))
    return Qnil;

  /* If POS is at the end of the interval,
     it means it's the end of OBJECT.
     There are no properties at the very end,
     since no character follows.  */
  if (XINT (pos) == LENGTH (i) + i->position)
    return Qnil;

  return textget (i->plist, prop);
}

DEFUN ("next-property-change", Fnext_property_change,
       Snext_property_change, 1, 3, 0,
  "Return the position of next property change.\n\
Scans characters forward from POS in OBJECT till it finds\n\
a change in some text property, then returns the position of the change.\n\
The optional second argument OBJECT is the string or buffer to scan.\n\
Return nil if the property is constant all the way to the end of OBJECT.\n\
If the value is non-nil, it is a position greater than POS, never equal.\n\n\
If the optional third argument LIMIT is non-nil, don't search\n\
past position LIMIT; return LIMIT if nothing is found before LIMIT.")
  (pos, object, limit)
     Lisp_Object pos, object, limit;
{
  register INTERVAL i, next;

  if (NILP (object))
    XSET (object, Lisp_Buffer, current_buffer);

  i = validate_interval_range (object, &pos, &pos, soft);
  if (NULL_INTERVAL_P (i))
    return limit;

  next = next_interval (i);
  while (! NULL_INTERVAL_P (next) && intervals_equal (i, next)
	 && (NILP (limit) || next->position < XFASTINT (limit)))
    next = next_interval (next);

  if (NULL_INTERVAL_P (next))
    return limit;
  if (! NILP (limit) && !(next->position < XFASTINT (limit)))
    return limit;

  return next->position - (XTYPE (object) == Lisp_String);
}

/* Return 1 if there's a change in some property between BEG and END.  */

int
property_change_between_p (beg, end)
     int beg, end;
{
  register INTERVAL i, next;
  Lisp_Object object, pos;

  XSET (object, Lisp_Buffer, current_buffer);
  XFASTINT (pos) = beg;

  i = validate_interval_range (object, &pos, &pos, soft);
  if (NULL_INTERVAL_P (i))
    return 0;

  next = next_interval (i);
  while (! NULL_INTERVAL_P (next) && intervals_equal (i, next))
    {
      next = next_interval (next);
      if (NULL_INTERVAL_P (next))
	return 0;
      if (next->position >= end)
	return 0;
    }

  if (NULL_INTERVAL_P (next))
    return 0;

  return 1;
}

DEFUN ("next-single-property-change", Fnext_single_property_change,
       Snext_single_property_change, 2, 4, 0,
  "Return the position of next property change for a specific property.\n\
Scans characters forward from POS till it finds\n\
a change in the PROP property, then returns the position of the change.\n\
The optional third argument OBJECT is the string or buffer to scan.\n\
The property values are compared with `eq'.\n\
Return nil if the property is constant all the way to the end of OBJECT.\n\
If the value is non-nil, it is a position greater than POS, never equal.\n\n\
If the optional fourth argument LIMIT is non-nil, don't search\n\
past position LIMIT; fail if nothing is found before LIMIT.")
  (pos, prop, object, limit)
     Lisp_Object pos, prop, object, limit;
{
  register INTERVAL i, next;
  register Lisp_Object here_val;

  if (NILP (object))
    XSET (object, Lisp_Buffer, current_buffer);

  i = validate_interval_range (object, &pos, &pos, soft);
  if (NULL_INTERVAL_P (i))
    return limit;

  here_val = textget (i->plist, prop);
  next = next_interval (i);
  while (! NULL_INTERVAL_P (next) 
	 && EQ (here_val, textget (next->plist, prop))
	 && (NILP (limit) || next->position < XFASTINT (limit)))
    next = next_interval (next);

  if (NULL_INTERVAL_P (next))
    return limit;
  if (! NILP (limit) && !(next->position < XFASTINT (limit)))
    return limit;

  return next->position - (XTYPE (object) == Lisp_String);
}

DEFUN ("previous-property-change", Fprevious_property_change,
       Sprevious_property_change, 1, 3, 0,
  "Return the position of previous property change.\n\
Scans characters backwards from POS in OBJECT till it finds\n\
a change in some text property, then returns the position of the change.\n\
The optional second argument OBJECT is the string or buffer to scan.\n\
Return nil if the property is constant all the way to the start of OBJECT.\n\
If the value is non-nil, it is a position less than POS, never equal.\n\n\
If the optional third argument LIMIT is non-nil, don't search\n\
back past position LIMIT; fail if nothing is found before LIMIT.")
  (pos, object, limit)
     Lisp_Object pos, object, limit;
{
  register INTERVAL i, previous;

  if (NILP (object))
    XSET (object, Lisp_Buffer, current_buffer);

  i = validate_interval_range (object, &pos, &pos, soft);
  if (NULL_INTERVAL_P (i))
    return limit;

  previous = previous_interval (i);
  while (! NULL_INTERVAL_P (previous) && intervals_equal (previous, i)
	 && (NILP (limit)
	     || previous->position + LENGTH (previous) > XFASTINT (limit)))
    previous = previous_interval (previous);
  if (NULL_INTERVAL_P (previous))
    return limit;
  if (!NILP (limit)
      && !(previous->position + LENGTH (previous) > XFASTINT (limit)))
    return limit;

  return (previous->position + LENGTH (previous)
	  - (XTYPE (object) == Lisp_String));
}

DEFUN ("previous-single-property-change", Fprevious_single_property_change,
       Sprevious_single_property_change, 2, 4, 0,
  "Return the position of previous property change for a specific property.\n\
Scans characters backward from POS till it finds\n\
a change in the PROP property, then returns the position of the change.\n\
The optional third argument OBJECT is the string or buffer to scan.\n\
The property values are compared with `eq'.\n\
Return nil if the property is constant all the way to the start of OBJECT.\n\
If the value is non-nil, it is a position less than POS, never equal.\n\n\
If the optional fourth argument LIMIT is non-nil, don't search\n\
back past position LIMIT; fail if nothing is found before LIMIT.")
     (pos, prop, object, limit)
     Lisp_Object pos, prop, object, limit;
{
  register INTERVAL i, previous;
  register Lisp_Object here_val;

  if (NILP (object))
    XSET (object, Lisp_Buffer, current_buffer);

  i = validate_interval_range (object, &pos, &pos, soft);
  if (NULL_INTERVAL_P (i))
    return limit;

  here_val = textget (i->plist, prop);
  previous = previous_interval (i);
  while (! NULL_INTERVAL_P (previous)
	 && EQ (here_val, textget (previous->plist, prop))
	 && (NILP (limit)
	     || previous->position + LENGTH (previous) > XFASTINT (limit)))
    previous = previous_interval (previous);
  if (NULL_INTERVAL_P (previous))
    return limit;
  if (!NILP (limit)
      && !(previous->position + LENGTH (previous) > XFASTINT (limit)))
    return limit;

  return (previous->position + LENGTH (previous)
	  - (XTYPE (object) == Lisp_String));
}

DEFUN ("add-text-properties", Fadd_text_properties,
       Sadd_text_properties, 3, 4, 0,
  "Add properties to the text from START to END.\n\
The third argument PROPS is a property list\n\
specifying the property values to add.\n\
The optional fourth argument, OBJECT,\n\
is the string or buffer containing the text.\n\
Return t if any property value actually changed, nil otherwise.")
  (start, end, properties, object)
     Lisp_Object start, end, properties, object;
{
  register INTERVAL i, unchanged;
  register int s, len, modified = 0;

  properties = validate_plist (properties);
  if (NILP (properties))
    return Qnil;

  if (NILP (object))
    XSET (object, Lisp_Buffer, current_buffer);

  i = validate_interval_range (object, &start, &end, hard);
  if (NULL_INTERVAL_P (i))
    return Qnil;

  s = XINT (start);
  len = XINT (end) - s;

  /* If we're not starting on an interval boundary, we have to
    split this interval. */
  if (i->position != s)
    {
      /* If this interval already has the properties, we can
         skip it. */
      if (interval_has_all_properties (properties, i))
	{
	  int got = (LENGTH (i) - (s - i->position));
	  if (got >= len)
	    return Qnil;
	  len -= got;
	  i = next_interval (i);
	}
      else
	{
	  unchanged = i;
	  i = split_interval_right (unchanged, s - unchanged->position);
	  copy_properties (unchanged, i);
	}
    }

  /* We are at the beginning of interval I, with LEN chars to scan.  */
  for (;;)
    {
      if (i == 0)
	abort ();

      if (LENGTH (i) >= len)
	{
	  if (interval_has_all_properties (properties, i))
	    return modified ? Qt : Qnil;

	  if (LENGTH (i) == len)
	    {
	      add_properties (properties, i, object);
	      return Qt;
	    }

	  /* i doesn't have the properties, and goes past the change limit */
	  unchanged = i;
	  i = split_interval_left (unchanged, len);
	  copy_properties (unchanged, i);
	  add_properties (properties, i, object);
	  return Qt;
	}

      len -= LENGTH (i);
      modified += add_properties (properties, i, object);
      i = next_interval (i);
    }
}

DEFUN ("put-text-property", Fput_text_property,
       Sput_text_property, 4, 5, 0,
  "Set one property of the text from START to END.\n\
The third and fourth arguments PROP and VALUE\n\
specify the property to add.\n\
The optional fifth argument, OBJECT,\n\
is the string or buffer containing the text.")
  (start, end, prop, value, object)
     Lisp_Object start, end, prop, value, object;
{
  Fadd_text_properties (start, end,
			Fcons (prop, Fcons (value, Qnil)),
			object);
  return Qnil;
}

DEFUN ("set-text-properties", Fset_text_properties,
       Sset_text_properties, 3, 4, 0,
  "Completely replace properties of text from START to END.\n\
The third argument PROPS is the new property list.\n\
The optional fourth argument, OBJECT,\n\
is the string or buffer containing the text.")
  (start, end, props, object)
     Lisp_Object start, end, props, object;
{
  register INTERVAL i, unchanged;
  register INTERVAL prev_changed = NULL_INTERVAL;
  register int s, len;

  props = validate_plist (props);

  if (NILP (object))
    XSET (object, Lisp_Buffer, current_buffer);

  i = validate_interval_range (object, &start, &end, hard);
  if (NULL_INTERVAL_P (i))
    return Qnil;

  s = XINT (start);
  len = XINT (end) - s;

  if (i->position != s)
    {
      unchanged = i;
      i = split_interval_right (unchanged, s - unchanged->position);

      if (LENGTH (i) > len)
	{
	  copy_properties (unchanged, i);
	  i = split_interval_left (i, len);
	  set_properties (props, i, object);
	  return Qt;
	}

      set_properties (props, i, object);

      if (LENGTH (i) == len)
	return Qt;

      prev_changed = i;
      len -= LENGTH (i);
      i = next_interval (i);
    }

  /* We are starting at the beginning of an interval, I */
  while (len > 0)
    {
      if (i == 0)
	abort ();

      if (LENGTH (i) >= len)
	{
	  if (LENGTH (i) > len)
	    i = split_interval_left (i, len);

	  if (NULL_INTERVAL_P (prev_changed))
	    set_properties (props, i, object);
	  else
	    merge_interval_left (i);
	  return Qt;
	}

      len -= LENGTH (i);
      if (NULL_INTERVAL_P (prev_changed))
	{
	  set_properties (props, i, object);
	  prev_changed = i;
	}
      else
	prev_changed = i = merge_interval_left (i);

      i = next_interval (i);
    }

  return Qt;
}

DEFUN ("remove-text-properties", Fremove_text_properties,
       Sremove_text_properties, 3, 4, 0,
  "Remove some properties from text from START to END.\n\
The third argument PROPS is a property list\n\
whose property names specify the properties to remove.\n\
\(The values stored in PROPS are ignored.)\n\
The optional fourth argument, OBJECT,\n\
is the string or buffer containing the text.\n\
Return t if any property was actually removed, nil otherwise.")
  (start, end, props, object)
     Lisp_Object start, end, props, object;
{
  register INTERVAL i, unchanged;
  register int s, len, modified = 0;

  if (NILP (object))
    XSET (object, Lisp_Buffer, current_buffer);

  i = validate_interval_range (object, &start, &end, soft);
  if (NULL_INTERVAL_P (i))
    return Qnil;

  s = XINT (start);
  len = XINT (end) - s;

  if (i->position != s)
    {
      /* No properties on this first interval -- return if
         it covers the entire region. */
      if (! interval_has_some_properties (props, i))
	{
	  int got = (LENGTH (i) - (s - i->position));
	  if (got >= len)
	    return Qnil;
	  len -= got;
	  i = next_interval (i);
	}
      /* Split away the beginning of this interval; what we don't
	 want to modify.  */
      else
	{
	  unchanged = i;
	  i = split_interval_right (unchanged, s - unchanged->position);
	  copy_properties (unchanged, i);
	}
    }

  /* We are at the beginning of an interval, with len to scan */
  for (;;)
    {
      if (i == 0)
	abort ();

      if (LENGTH (i) >= len)
	{
	  if (! interval_has_some_properties (props, i))
	    return modified ? Qt : Qnil;

	  if (LENGTH (i) == len)
	    {
	      remove_properties (props, i, object);
	      return Qt;
	    }

	  /* i has the properties, and goes past the change limit */
	  unchanged = i;
	  i = split_interval_left (i, len);
	  copy_properties (unchanged, i);
	  remove_properties (props, i, object);
	  return Qt;
	}

      len -= LENGTH (i);
      modified += remove_properties (props, i, object);
      i = next_interval (i);
    }
}

DEFUN ("text-property-any", Ftext_property_any,
       Stext_property_any, 4, 5, 0,
  "Check text from START to END to see if PROP is ever `eq' to VALUE.\n\
If so, return the position of the first character whose PROP is `eq'\n\
to VALUE.  Otherwise return nil.\n\
The optional fifth argument, OBJECT, is the string or buffer\n\
containing the text.")
  (start, end, prop, value, object)
       Lisp_Object start, end, prop, value, object;
{
  register INTERVAL i;
  register int e, pos;

  if (NILP (object))
    XSET (object, Lisp_Buffer, current_buffer);
  i = validate_interval_range (object, &start, &end, soft);
  e = XINT (end);

  while (! NULL_INTERVAL_P (i))
    {
      if (i->position >= e)
	break;
      if (EQ (textget (i->plist, prop), value))
	{
	  pos = i->position;
	  if (pos < XINT (start))
	    pos = XINT (start);
	  return make_number (pos - (XTYPE (object) == Lisp_String));
	}
      i = next_interval (i);
    }
  return Qnil;
}

DEFUN ("text-property-not-all", Ftext_property_not_all,
       Stext_property_not_all, 4, 5, 0,
  "Check text from START to END to see if PROP is ever not `eq' to VALUE.\n\
If so, return the position of the first character whose PROP is not\n\
`eq' to VALUE.  Otherwise, return nil.\n\
The optional fifth argument, OBJECT, is the string or buffer\n\
containing the text.")
  (start, end, prop, value, object)
       Lisp_Object start, end, prop, value, object;
{
  register INTERVAL i;
  register int s, e;

  if (NILP (object))
    XSET (object, Lisp_Buffer, current_buffer);
  i = validate_interval_range (object, &start, &end, soft);
  if (NULL_INTERVAL_P (i))
    return (NILP (value) || EQ (start, end)) ? Qnil : start;
  s = XINT (start);
  e = XINT (end);

  while (! NULL_INTERVAL_P (i))
    {
      if (i->position >= e)
	break;
      if (! EQ (textget (i->plist, prop), value))
	{
	  if (i->position > s)
	    s = i->position;
	  return make_number (s - (XTYPE (object) == Lisp_String));
	}
      i = next_interval (i);
    }
  return Qnil;
}

#if 0 /* You can use set-text-properties for this.  */

DEFUN ("erase-text-properties", Ferase_text_properties,
       Serase_text_properties, 2, 3, 0,
  "Remove all properties from the text from START to END.\n\
The optional third argument, OBJECT,\n\
is the string or buffer containing the text.")
  (start, end, object)
     Lisp_Object start, end, object;
{
  register INTERVAL i;
  register INTERVAL prev_changed = NULL_INTERVAL;
  register int s, len, modified;

  if (NILP (object))
    XSET (object, Lisp_Buffer, current_buffer);

  i = validate_interval_range (object, &start, &end, soft);
  if (NULL_INTERVAL_P (i))
    return Qnil;

  s = XINT (start);
  len = XINT (end) - s;

  if (i->position != s)
    {
      register int got;
      register INTERVAL unchanged = i;

      /* If there are properties here, then this text will be modified. */
      if (! NILP (i->plist))
	{
	  i = split_interval_right (unchanged, s - unchanged->position);
	  i->plist = Qnil;
	  modified++;

	  if (LENGTH (i) > len)
	    {
	      i = split_interval_right (i, len);
	      copy_properties (unchanged, i);
	      return Qt;
	    }

	  if (LENGTH (i) == len)
	    return Qt;

	  got = LENGTH (i);
	}
      /* If the text of I is without any properties, and contains
         LEN or more characters, then we may return without changing
	 anything.*/
      else if (LENGTH (i) - (s - i->position) <= len)
	return Qnil;
      /* The amount of text to change extends past I, so just note
	 how much we've gotten. */
      else
	got = LENGTH (i) - (s - i->position);

      len -= got;
      prev_changed = i;
      i = next_interval (i);
    }

  /* We are starting at the beginning of an interval, I. */
  while (len > 0)
    {
      if (LENGTH (i) >= len)
	{
	  /* If I has no properties, simply merge it if possible.  */
	  if (NILP (i->plist))
	    {
	      if (! NULL_INTERVAL_P (prev_changed))
		merge_interval_left (i);

	      return modified ? Qt : Qnil;
	    }

          if (LENGTH (i) > len)
            i = split_interval_left (i, len);
	  if (! NULL_INTERVAL_P (prev_changed))
	    merge_interval_left (i);
	  else
	    i->plist = Qnil;

	  return Qt;
	}

      /* Here if we still need to erase past the end of I */
      len -= LENGTH (i);
      if (NULL_INTERVAL_P (prev_changed))
	{
	  modified += erase_properties (i);
	  prev_changed = i;
	}
      else
	{
	  modified += ! NILP (i->plist);
	  /* Merging I will give it the properties of PREV_CHANGED. */
	  prev_changed = i = merge_interval_left (i);
	}

      i = next_interval (i);
    }

  return modified ? Qt : Qnil;
}
#endif /* 0 */

/* I don't think this is the right interface to export; how often do you
   want to do something like this, other than when you're copying objects
   around?

   I think it would be better to have a pair of functions, one which
   returns the text properties of a region as a list of ranges and
   plists, and another which applies such a list to another object.  */

/* DEFUN ("copy-text-properties", Fcopy_text_properties,
       Scopy_text_properties, 5, 6, 0,
  "Add properties from SRC-START to SRC-END of SRC at DEST-POS of DEST.\n\
SRC and DEST may each refer to strings or buffers.\n\
Optional sixth argument PROP causes only that property to be copied.\n\
Properties are copied to DEST as if by `add-text-properties'.\n\
Return t if any property value actually changed, nil otherwise.") */

Lisp_Object
copy_text_properties (start, end, src, pos, dest, prop)
       Lisp_Object start, end, src, pos, dest, prop;
{
  INTERVAL i;
  Lisp_Object res;
  Lisp_Object stuff;
  Lisp_Object plist;
  int s, e, e2, p, len, modified = 0;

  i = validate_interval_range (src, &start, &end, soft);
  if (NULL_INTERVAL_P (i))
    return Qnil;

  CHECK_NUMBER_COERCE_MARKER (pos, 0);
  {
    Lisp_Object dest_start, dest_end;

    dest_start = pos;
    XFASTINT (dest_end) = XINT (dest_start) + (XINT (end) - XINT (start));
    /* Apply this to a copy of pos; it will try to increment its arguments,
       which we don't want.  */
    validate_interval_range (dest, &dest_start, &dest_end, soft);
  }

  s = XINT (start);
  e = XINT (end);
  p = XINT (pos);

  stuff = Qnil;

  while (s < e)
    {
      e2 = i->position + LENGTH (i);
      if (e2 > e)
	e2 = e;
      len = e2 - s;

      plist = i->plist;
      if (! NILP (prop))
	while (! NILP (plist))
	  {
	    if (EQ (Fcar (plist), prop))
	      {
		plist = Fcons (prop, Fcons (Fcar (Fcdr (plist)), Qnil));
		break;
	      }
	    plist = Fcdr (Fcdr (plist));
	  }
      if (! NILP (plist))
	{
	  /* Must defer modifications to the interval tree in case src
	     and dest refer to the same string or buffer. */
	  stuff = Fcons (Fcons (make_number (p),
				Fcons (make_number (p + len),
				       Fcons (plist, Qnil))),
			stuff);
	}

      i = next_interval (i);
      if (NULL_INTERVAL_P (i))
	break;

      p += len;
      s = i->position;
    }

  while (! NILP (stuff))
    {
      res = Fcar (stuff);
      res = Fadd_text_properties (Fcar (res), Fcar (Fcdr (res)),
				  Fcar (Fcdr (Fcdr (res))), dest);
      if (! NILP (res))
	modified++;
      stuff = Fcdr (stuff);
    }

  return modified ? Qt : Qnil;
}

void
syms_of_textprop ()
{
  DEFVAR_INT ("interval-balance-threshold", &interval_balance_threshold,
	      "Threshold for rebalancing interval trees, expressed as the\n\
percentage by which the left interval tree should not differ from the right.");
  interval_balance_threshold = 8;

  DEFVAR_LISP ("inhibit-point-motion-hooks", &Vinhibit_point_motion_hooks,
	       "If nonnil, don't call the text property values of\n\
`point-left' and `point-entered'.");
  Vinhibit_point_motion_hooks = Qnil;
	       
  /* Common attributes one might give text */

  staticpro (&Qforeground);
  Qforeground = intern ("foreground");
  staticpro (&Qbackground);
  Qbackground = intern ("background");
  staticpro (&Qfont);
  Qfont = intern ("font");
  staticpro (&Qstipple);
  Qstipple = intern ("stipple");
  staticpro (&Qunderline);
  Qunderline = intern ("underline");
  staticpro (&Qread_only);
  Qread_only = intern ("read-only");
  staticpro (&Qinvisible);
  Qinvisible = intern ("invisible");
  staticpro (&Qhidden);
  Qhidden = intern ("hidden");
  staticpro (&Qcategory);
  Qcategory = intern ("category");
  staticpro (&Qlocal_map);
  Qlocal_map = intern ("local-map");
  staticpro (&Qfront_sticky);
  Qfront_sticky = intern ("front-sticky");
  staticpro (&Qrear_nonsticky);
  Qrear_nonsticky = intern ("rear-nonsticky");

  /* Properties that text might use to specify certain actions */

  staticpro (&Qmouse_left);
  Qmouse_left = intern ("mouse-left");
  staticpro (&Qmouse_entered);
  Qmouse_entered = intern ("mouse-entered");
  staticpro (&Qpoint_left);
  Qpoint_left = intern ("point-left");
  staticpro (&Qpoint_entered);
  Qpoint_entered = intern ("point-entered");

  defsubr (&Stext_properties_at);
  defsubr (&Sget_text_property);
  defsubr (&Snext_property_change);
  defsubr (&Snext_single_property_change);
  defsubr (&Sprevious_property_change);
  defsubr (&Sprevious_single_property_change);
  defsubr (&Sadd_text_properties);
  defsubr (&Sput_text_property);
  defsubr (&Sset_text_properties);
  defsubr (&Sremove_text_properties);
  defsubr (&Stext_property_any);
  defsubr (&Stext_property_not_all);
/*  defsubr (&Serase_text_properties); */
/*  defsubr (&Scopy_text_properties); */
}

#else

lose -- this shouldn't be compiled if USE_TEXT_PROPERTIES isn't defined

#endif /* USE_TEXT_PROPERTIES */
