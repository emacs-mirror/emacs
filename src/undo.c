/* undo handling for GNU Emacs.
   Copyright (C) 1990, 1993, 1994, 2000 Free Software Foundation, Inc.

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
#include "lisp.h"
#include "buffer.h"
#include "commands.h"

/* Last buffer for which undo information was recorded.  */
Lisp_Object last_undo_buffer;

Lisp_Object Qinhibit_read_only;

/* The first time a command records something for undo.
   it also allocates the undo-boundary object
   which will be added to the list at the end of the command.
   This ensures we can't run out of space while trying to make
   an undo-boundary.  */
Lisp_Object pending_boundary;

/* Record point as it was at beginning of this command (if necessary)
   And prepare the undo info for recording a change.
   PT is the position of point that will naturally occur as a result of the
   undo record that will be added just after this command terminates.  */

static void
record_point (pt)
{
  int at_boundary;

  /* Allocate a cons cell to be the undo boundary after this command.  */
  if (NILP (pending_boundary))
    pending_boundary = Fcons (Qnil, Qnil);

  if (!BUFFERP (last_undo_buffer)
      || current_buffer != XBUFFER (last_undo_buffer))
    Fundo_boundary ();
  XSETBUFFER (last_undo_buffer, current_buffer);

  if (CONSP (current_buffer->undo_list))
    {
      /* Set AT_BOUNDARY to 1 only when we have nothing other than
         marker adjustment before undo boundary.  */

      Lisp_Object tail = current_buffer->undo_list, elt;

      while (1)
	{
	  if (NILP (tail))
	    elt = Qnil;
	  else
	    elt = XCAR (tail);
	  if (NILP (elt) || ! (CONSP (elt) && MARKERP (XCAR (elt))))
	    break;
	  tail = XCDR (tail);
	}
      at_boundary = NILP (elt);
    }
  else
    at_boundary = 1;

  if (MODIFF <= SAVE_MODIFF)
    record_first_change ();

  /* If we are just after an undo boundary, and 
     point wasn't at start of deleted range, record where it was.  */
  if (at_boundary
      && last_point_position != pt
      /* If we're called from batch mode, this could be nil.  */
      && BUFFERP (last_point_position_buffer)
      && current_buffer == XBUFFER (last_point_position_buffer))
    current_buffer->undo_list
      = Fcons (make_number (last_point_position), current_buffer->undo_list);
}

/* Record an insertion that just happened or is about to happen,
   for LENGTH characters at position BEG.
   (It is possible to record an insertion before or after the fact
   because we don't need to record the contents.)  */

void
record_insert (beg, length)
     int beg, length;
{
  Lisp_Object lbeg, lend;

  if (EQ (current_buffer->undo_list, Qt))
    return;

  record_point (beg);

  /* If this is following another insertion and consecutive with it
     in the buffer, combine the two.  */
  if (CONSP (current_buffer->undo_list))
    {
      Lisp_Object elt;
      elt = XCAR (current_buffer->undo_list);
      if (CONSP (elt)
	  && INTEGERP (XCAR (elt))
	  && INTEGERP (XCDR (elt))
	  && XINT (XCDR (elt)) == beg)
	{
	  XSETCDR (elt, make_number (beg + length));
	  return;
	}
    }

  XSETFASTINT (lbeg, beg);
  XSETINT (lend, beg + length);
  current_buffer->undo_list = Fcons (Fcons (lbeg, lend),
                                     current_buffer->undo_list);
}

/* Record that a deletion is about to take place,
   of the characters in STRING, at location BEG.  */

void
record_delete (beg, string)
     int beg;
     Lisp_Object string;
{
  Lisp_Object sbeg;

  if (EQ (current_buffer->undo_list, Qt))
    return;

  if (PT == beg + XSTRING (string)->size)
    {
      XSETINT (sbeg, -beg);
      record_point (PT);
    }
  else
    {
      XSETFASTINT (sbeg, beg);
      record_point (beg);
    }

  current_buffer->undo_list
    = Fcons (Fcons (string, sbeg), current_buffer->undo_list);
}

/* Record the fact that MARKER is about to be adjusted by ADJUSTMENT.
   This is done only when a marker points within text being deleted,
   because that's the only case where an automatic marker adjustment
   won't be inverted automatically by undoing the buffer modification.  */

void
record_marker_adjustment (marker, adjustment)
     Lisp_Object marker;
     int adjustment;
{
  if (EQ (current_buffer->undo_list, Qt))
    return;

  /* Allocate a cons cell to be the undo boundary after this command.  */
  if (NILP (pending_boundary))
    pending_boundary = Fcons (Qnil, Qnil);

  if (!BUFFERP (last_undo_buffer) 
      || current_buffer != XBUFFER (last_undo_buffer))
    Fundo_boundary ();
  XSETBUFFER (last_undo_buffer, current_buffer);

  current_buffer->undo_list
    = Fcons (Fcons (marker, make_number (adjustment)),
	     current_buffer->undo_list);
}

/* Record that a replacement is about to take place,
   for LENGTH characters at location BEG.
   The replacement must not change the number of characters.  */

void
record_change (beg, length)
     int beg, length;
{
  record_delete (beg, make_buffer_string (beg, beg + length, 1));
  record_insert (beg, length);
}

/* Record that an unmodified buffer is about to be changed.
   Record the file modification date so that when undoing this entry
   we can tell whether it is obsolete because the file was saved again.  */

void
record_first_change ()
{
  Lisp_Object high, low;
  struct buffer *base_buffer = current_buffer;

  if (EQ (current_buffer->undo_list, Qt))
    return;

  if (!BUFFERP (last_undo_buffer)
      || current_buffer != XBUFFER (last_undo_buffer))
    Fundo_boundary ();
  XSETBUFFER (last_undo_buffer, current_buffer);

  if (base_buffer->base_buffer)
    base_buffer = base_buffer->base_buffer;

  XSETFASTINT (high, (base_buffer->modtime >> 16) & 0xffff);
  XSETFASTINT (low, base_buffer->modtime & 0xffff);
  current_buffer->undo_list = Fcons (Fcons (Qt, Fcons (high, low)), current_buffer->undo_list);
}

/* Record a change in property PROP (whose old value was VAL)
   for LENGTH characters starting at position BEG in BUFFER.  */

void
record_property_change (beg, length, prop, value, buffer)
     int beg, length;
     Lisp_Object prop, value, buffer;
{
  Lisp_Object lbeg, lend, entry;
  struct buffer *obuf = current_buffer;
  int boundary = 0;

  if (EQ (XBUFFER (buffer)->undo_list, Qt))
    return;

  /* Allocate a cons cell to be the undo boundary after this command.  */
  if (NILP (pending_boundary))
    pending_boundary = Fcons (Qnil, Qnil);

  if (!EQ (buffer, last_undo_buffer))
    boundary = 1;
  last_undo_buffer = buffer;

  /* Switch temporarily to the buffer that was changed.  */
  current_buffer = XBUFFER (buffer);

  if (boundary)
    Fundo_boundary ();

  if (MODIFF <= SAVE_MODIFF)
    record_first_change ();

  XSETINT (lbeg, beg);
  XSETINT (lend, beg + length);
  entry = Fcons (Qnil, Fcons (prop, Fcons (value, Fcons (lbeg, lend))));
  current_buffer->undo_list = Fcons (entry, current_buffer->undo_list);

  current_buffer = obuf;
}

DEFUN ("undo-boundary", Fundo_boundary, Sundo_boundary, 0, 0, 0,
       doc: /* Mark a boundary between units of undo.
An undo command will stop at this point,
but another undo command will undo to the previous boundary.  */)
     ()
{
  Lisp_Object tem;
  if (EQ (current_buffer->undo_list, Qt))
    return Qnil;
  tem = Fcar (current_buffer->undo_list);
  if (!NILP (tem))
    {
      /* One way or another, cons nil onto the front of the undo list.  */
      if (!NILP (pending_boundary))
	{
	  /* If we have preallocated the cons cell to use here,
	     use that one.  */
	  XSETCDR (pending_boundary, current_buffer->undo_list);
	  current_buffer->undo_list = pending_boundary;
	  pending_boundary = Qnil;
	}
      else
	current_buffer->undo_list = Fcons (Qnil, current_buffer->undo_list);
    }
  return Qnil;
}

/* At garbage collection time, make an undo list shorter at the end,
   returning the truncated list.
   MINSIZE and MAXSIZE are the limits on size allowed, as described below.
   In practice, these are the values of undo-limit and
   undo-strong-limit.  */

Lisp_Object
truncate_undo_list (list, minsize, maxsize)
     Lisp_Object list;
     int minsize, maxsize;
{
  Lisp_Object prev, next, last_boundary;
  int size_so_far = 0;

  prev = Qnil;
  next = list;
  last_boundary = Qnil;

  /* Always preserve at least the most recent undo record.
     If the first element is an undo boundary, skip past it.

     Skip, skip, skip the undo, skip, skip, skip the undo,
     Skip, skip, skip the undo, skip to the undo bound'ry. 
     (Get it?  "Skip to my Loo?")  */
  if (CONSP (next) && NILP (XCAR (next)))
    {
      /* Add in the space occupied by this element and its chain link.  */
      size_so_far += sizeof (struct Lisp_Cons);

      /* Advance to next element.  */
      prev = next;
      next = XCDR (next);
    }
  while (CONSP (next) && ! NILP (XCAR (next)))
    {
      Lisp_Object elt;
      elt = XCAR (next);

      /* Add in the space occupied by this element and its chain link.  */
      size_so_far += sizeof (struct Lisp_Cons);
      if (CONSP (elt))
	{
	  size_so_far += sizeof (struct Lisp_Cons);
	  if (STRINGP (XCAR (elt)))
	    size_so_far += (sizeof (struct Lisp_String) - 1
			    + XSTRING (XCAR (elt))->size);
	}

      /* Advance to next element.  */
      prev = next;
      next = XCDR (next);
    }
  if (CONSP (next))
    last_boundary = prev;

  while (CONSP (next))
    {
      Lisp_Object elt;
      elt = XCAR (next);

      /* When we get to a boundary, decide whether to truncate
	 either before or after it.  The lower threshold, MINSIZE,
	 tells us to truncate after it.  If its size pushes past
	 the higher threshold MAXSIZE as well, we truncate before it.  */
      if (NILP (elt))
	{
	  if (size_so_far > maxsize)
	    break;
	  last_boundary = prev;
	  if (size_so_far > minsize)
	    break;
	}

      /* Add in the space occupied by this element and its chain link.  */
      size_so_far += sizeof (struct Lisp_Cons);
      if (CONSP (elt))
	{
	  size_so_far += sizeof (struct Lisp_Cons);
	  if (STRINGP (XCAR (elt)))
	    size_so_far += (sizeof (struct Lisp_String) - 1
			    + XSTRING (XCAR (elt))->size);
	}

      /* Advance to next element.  */
      prev = next;
      next = XCDR (next);
    }

  /* If we scanned the whole list, it is short enough; don't change it.  */
  if (NILP (next))
    return list;

  /* Truncate at the boundary where we decided to truncate.  */
  if (!NILP (last_boundary))
    {
      XSETCDR (last_boundary, Qnil);
      return list;
    }
  else
    return Qnil;
}

DEFUN ("primitive-undo", Fprimitive_undo, Sprimitive_undo, 2, 2, 0,
       doc: /* Undo N records from the front of the list LIST.
Return what remains of the list.  */)
     (n, list)
     Lisp_Object n, list;
{
  struct gcpro gcpro1, gcpro2;
  Lisp_Object next;
  int count = BINDING_STACK_SIZE ();
  register int arg;
  
#if 0  /* This is a good feature, but would make undo-start
	  unable to do what is expected.  */
  Lisp_Object tem;

  /* If the head of the list is a boundary, it is the boundary
     preceding this command.  Get rid of it and don't count it.  */
  tem = Fcar (list);
  if (NILP (tem))
    list = Fcdr (list);
#endif

  CHECK_NUMBER (n);
  arg = XINT (n);
  next = Qnil;
  GCPRO2 (next, list);

  /* In a writable buffer, enable undoing read-only text that is so
     because of text properties.  */
  if (NILP (current_buffer->read_only))
    specbind (Qinhibit_read_only, Qt);

  /* Don't let `intangible' properties interfere with undo.  */
  specbind (Qinhibit_point_motion_hooks, Qt);

  while (arg > 0)
    {
      while (CONSP (list))
	{
	  next = XCAR (list);
	  list = XCDR (list);
	  /* Exit inner loop at undo boundary.  */
	  if (NILP (next))
	    break;
	  /* Handle an integer by setting point to that value.  */
	  if (INTEGERP (next))
	    SET_PT (clip_to_bounds (BEGV, XINT (next), ZV));
	  else if (CONSP (next))
	    {
	      Lisp_Object car, cdr;

	      car = XCAR (next);
	      cdr = XCDR (next);
	      if (EQ (car, Qt))
		{
		  /* Element (t high . low) records previous modtime.  */
		  Lisp_Object high, low;
		  int mod_time;
		  struct buffer *base_buffer = current_buffer;

		  high = Fcar (cdr);
		  low = Fcdr (cdr);
		  mod_time = (XFASTINT (high) << 16) + XFASTINT (low);

		  if (current_buffer->base_buffer)
		    base_buffer = current_buffer->base_buffer;

		  /* If this records an obsolete save
		     (not matching the actual disk file)
		     then don't mark unmodified.  */
		  if (mod_time != base_buffer->modtime)
		    continue;
#ifdef CLASH_DETECTION
		  Funlock_buffer ();
#endif /* CLASH_DETECTION */
		  Fset_buffer_modified_p (Qnil);
		}
	      else if (EQ (car, Qnil))
		{
		  /* Element (nil prop val beg . end) is property change.  */
		  Lisp_Object beg, end, prop, val;

		  prop = Fcar (cdr);
		  cdr = Fcdr (cdr);
		  val = Fcar (cdr);
		  cdr = Fcdr (cdr);
		  beg = Fcar (cdr);
		  end = Fcdr (cdr);

		  Fput_text_property (beg, end, prop, val, Qnil);
		}
	      else if (INTEGERP (car) && INTEGERP (cdr))
		{
		  /* Element (BEG . END) means range was inserted.  */

		  if (XINT (car) < BEGV
		      || XINT (cdr) > ZV)
		    error ("Changes to be undone are outside visible portion of buffer");
		  /* Set point first thing, so that undoing this undo
		     does not send point back to where it is now.  */
		  Fgoto_char (car);
		  Fdelete_region (car, cdr);
		}
	      else if (STRINGP (car) && INTEGERP (cdr))
		{
		  /* Element (STRING . POS) means STRING was deleted.  */
		  Lisp_Object membuf;
		  int pos = XINT (cdr);

		  membuf = car;
		  if (pos < 0)
		    {
		      if (-pos < BEGV || -pos > ZV)
			error ("Changes to be undone are outside visible portion of buffer");
		      SET_PT (-pos);
		      Finsert (1, &membuf);
		    }
		  else
		    {
		      if (pos < BEGV || pos > ZV)
			error ("Changes to be undone are outside visible portion of buffer");
		      SET_PT (pos);

		      /* Now that we record marker adjustments
			 (caused by deletion) for undo,
			 we should always insert after markers,
			 so that undoing the marker adjustments
			 put the markers back in the right place.  */
		      Finsert (1, &membuf);
		      SET_PT (pos);
		    }
		}
	      else if (MARKERP (car) && INTEGERP (cdr))
		{
		  /* (MARKER . INTEGER) means a marker MARKER
		     was adjusted by INTEGER.  */
		  if (XMARKER (car)->buffer)
		    Fset_marker (car,
				 make_number (marker_position (car) - XINT (cdr)),
				 Fmarker_buffer (car));
		}
	    }
	}
      arg--;
    }

  UNGCPRO;
  return unbind_to (count, list);
}

void
syms_of_undo ()
{
  Qinhibit_read_only = intern ("inhibit-read-only");
  staticpro (&Qinhibit_read_only);

  pending_boundary = Qnil;
  staticpro (&pending_boundary);

  defsubr (&Sprimitive_undo);
  defsubr (&Sundo_boundary);
}
