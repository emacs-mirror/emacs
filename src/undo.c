/* undo handling for GNU Emacs.
   Copyright (C) 1990, 1993, 1994 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */


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

/* Record an insertion that just happened or is about to happen,
   for LENGTH characters at position BEG.
   (It is possible to record an insertion before or after the fact
   because we don't need to record the contents.)  */

record_insert (beg, length)
     int beg, length;
{
  Lisp_Object lbeg, lend;

  if (EQ (current_buffer->undo_list, Qt))
    return;

  /* Allocate a cons cell to be the undo boundary after this command.  */
  if (NILP (pending_boundary))
    pending_boundary = Fcons (Qnil, Qnil);

  if (current_buffer != XBUFFER (last_undo_buffer))
    Fundo_boundary ();
  XSETBUFFER (last_undo_buffer, current_buffer);

  if (MODIFF <= SAVE_MODIFF)
    record_first_change ();

  /* If this is following another insertion and consecutive with it
     in the buffer, combine the two.  */
  if (CONSP (current_buffer->undo_list))
    {
      Lisp_Object elt;
      elt = XCONS (current_buffer->undo_list)->car;
      if (CONSP (elt)
	  && INTEGERP (XCONS (elt)->car)
	  && INTEGERP (XCONS (elt)->cdr)
	  && XINT (XCONS (elt)->cdr) == beg)
	{
	  XSETINT (XCONS (elt)->cdr, beg + length);
	  return;
	}
    }

  XSETFASTINT (lbeg, beg);
  XSETINT (lend, beg + length);
  current_buffer->undo_list = Fcons (Fcons (lbeg, lend),
                                     current_buffer->undo_list);
}

/* Record that a deletion is about to take place,
   for LENGTH characters at location BEG.  */

record_delete (beg, length)
     int beg, length;
{
  Lisp_Object lbeg, lend, sbeg;
  int at_boundary;

  if (EQ (current_buffer->undo_list, Qt))
    return;

  /* Allocate a cons cell to be the undo boundary after this command.  */
  if (NILP (pending_boundary))
    pending_boundary = Fcons (Qnil, Qnil);

  if (current_buffer != XBUFFER (last_undo_buffer))
    Fundo_boundary ();
  XSETBUFFER (last_undo_buffer, current_buffer);

  at_boundary = (CONSP (current_buffer->undo_list)
		 && NILP (XCONS (current_buffer->undo_list)->car));

  if (MODIFF <= SAVE_MODIFF)
    record_first_change ();

  if (point == beg + length)
    XSETINT (sbeg, -beg);
  else
    XSETFASTINT (sbeg, beg);
  XSETFASTINT (lbeg, beg);
  XSETFASTINT (lend, beg + length);

  /* If we are just after an undo boundary, and 
     point wasn't at start of deleted range, record where it was.  */
  if (at_boundary
      && last_point_position != XFASTINT (sbeg)
      && current_buffer == XBUFFER (last_point_position_buffer))
    current_buffer->undo_list
      = Fcons (make_number (last_point_position), current_buffer->undo_list);

  current_buffer->undo_list
    = Fcons (Fcons (Fbuffer_substring (lbeg, lend), sbeg),
	     current_buffer->undo_list);
}

/* Record that a replacement is about to take place,
   for LENGTH characters at location BEG.
   The replacement does not change the number of characters.  */

record_change (beg, length)
     int beg, length;
{
  record_delete (beg, length);
  record_insert (beg, length);
}

/* Record that an unmodified buffer is about to be changed.
   Record the file modification date so that when undoing this entry
   we can tell whether it is obsolete because the file was saved again.  */

record_first_change ()
{
  Lisp_Object high, low;
  struct buffer *base_buffer = current_buffer;

  if (EQ (current_buffer->undo_list, Qt))
    return;

  if (current_buffer != XBUFFER (last_undo_buffer))
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
  "Mark a boundary between units of undo.\n\
An undo command will stop at this point,\n\
but another undo command will undo to the previous boundary.")
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
	  XCONS (pending_boundary)->cdr = current_buffer->undo_list;
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
  if (CONSP (next) && NILP (XCONS (next)->car))
    {
      /* Add in the space occupied by this element and its chain link.  */
      size_so_far += sizeof (struct Lisp_Cons);

      /* Advance to next element.  */
      prev = next;
      next = XCONS (next)->cdr;
    }
  while (CONSP (next) && ! NILP (XCONS (next)->car))
    {
      Lisp_Object elt;
      elt = XCONS (next)->car;

      /* Add in the space occupied by this element and its chain link.  */
      size_so_far += sizeof (struct Lisp_Cons);
      if (CONSP (elt))
	{
	  size_so_far += sizeof (struct Lisp_Cons);
	  if (STRINGP (XCONS (elt)->car))
	    size_so_far += (sizeof (struct Lisp_String) - 1
			    + XSTRING (XCONS (elt)->car)->size);
	}

      /* Advance to next element.  */
      prev = next;
      next = XCONS (next)->cdr;
    }
  if (CONSP (next))
    last_boundary = prev;

  while (CONSP (next))
    {
      Lisp_Object elt;
      elt = XCONS (next)->car;

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
	  if (STRINGP (XCONS (elt)->car))
	    size_so_far += (sizeof (struct Lisp_String) - 1
			    + XSTRING (XCONS (elt)->car)->size);
	}

      /* Advance to next element.  */
      prev = next;
      next = XCONS (next)->cdr;
    }

  /* If we scanned the whole list, it is short enough; don't change it.  */
  if (NILP (next))
    return list;

  /* Truncate at the boundary where we decided to truncate.  */
  if (!NILP (last_boundary))
    {
      XCONS (last_boundary)->cdr = Qnil;
      return list;
    }
  else
    return Qnil;
}

DEFUN ("primitive-undo", Fprimitive_undo, Sprimitive_undo, 2, 2, 0,
  "Undo N records from the front of the list LIST.\n\
Return what remains of the list.")
  (n, list)
     Lisp_Object n, list;
{
  struct gcpro gcpro1, gcpro2;
  Lisp_Object next;
  int count = specpdl_ptr - specpdl;
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

  CHECK_NUMBER (n, 0);
  arg = XINT (n);
  next = Qnil;
  GCPRO2 (next, list);

  /* Don't let read-only properties interfere with undo.  */
  if (NILP (current_buffer->read_only))
    specbind (Qinhibit_read_only, Qt);

  while (arg > 0)
    {
      while (1)
	{
	  next = Fcar (list);
	  list = Fcdr (list);
	  /* Exit inner loop at undo boundary.  */
	  if (NILP (next))
	    break;
	  /* Handle an integer by setting point to that value.  */
	  if (INTEGERP (next))
	    SET_PT (clip_to_bounds (BEGV, XINT (next), ZV));
	  else if (CONSP (next))
	    {
	      Lisp_Object car, cdr;

	      car = Fcar (next);
	      cdr = Fcdr (next);
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
		    break;
#ifdef CLASH_DETECTION
		  Funlock_buffer ();
#endif /* CLASH_DETECTION */
		  Fset_buffer_modified_p (Qnil);
		}
#ifdef USE_TEXT_PROPERTIES
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
#endif /* USE_TEXT_PROPERTIES */
	      else if (INTEGERP (car) && INTEGERP (cdr))
		{
		  /* Element (BEG . END) means range was inserted.  */
		  Lisp_Object end;

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

		      /* Insert before markers so that if the mark is
			 currently on the boundary of this deletion, it
			 ends up on the other side of the now-undeleted
			 text from point.  Since undo doesn't even keep
			 track of the mark, this isn't really necessary,
			 but it may lead to better behavior in certain
			 situations.  */
		      Finsert_before_markers (1, &membuf);
		      SET_PT (pos);
		    }
		}
	    }
	}
      arg--;
    }

  UNGCPRO;
  return unbind_to (count, list);
}

syms_of_undo ()
{
  Qinhibit_read_only = intern ("inhibit-read-only");
  staticpro (&Qinhibit_read_only);

  pending_boundary = Qnil;
  staticpro (&pending_boundary);

  defsubr (&Sprimitive_undo);
  defsubr (&Sundo_boundary);
}
