/* undo handling for GNU Emacs.
   Copyright (C) 1990, 1993-1994, 2000-2017 Free Software Foundation,
   Inc.

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


#include <config.h>

#include "lisp.h"
#include "buffer.h"
#include "keyboard.h"

/* The first time a command records something for undo.
   it also allocates the undo-boundary object
   which will be added to the list at the end of the command.
   This ensures we can't run out of space while trying to make
   an undo-boundary.  */
static Lisp_Object pending_boundary;

/* Prepare the undo info for recording a change. */
static void
prepare_record (void)
{
  /* Allocate a cons cell to be the undo boundary after this command.  */
  if (NILP (pending_boundary))
    pending_boundary = Fcons (Qnil, Qnil);
}

/* Record point, if necessary, as it was at beginning of this command.
   BEG is the position of point that will naturally occur as a result
   of the undo record that will be added just after this command
   terminates.  */
static void
record_point (ptrdiff_t beg)
{
  /* Don't record position of pt when undo_inhibit_record_point holds.  */
  if (undo_inhibit_record_point)
    return;

  bool at_boundary;

  /* Check whether we are at a boundary now, in case we record the
  first change. FIXME: This check is currently dependent on being
  called before record_first_change, but could be made not to by
  ignoring timestamp undo entries */
  at_boundary = ! CONSP (BVAR (current_buffer, undo_list))
                || NILP (XCAR (BVAR (current_buffer, undo_list)));

  /* If this is the first change since save, then record this.*/
  if (MODIFF <= SAVE_MODIFF)
    record_first_change ();

  /* We may need to record point if we are immediately after a
     boundary, so that this will be restored correctly after undo. We
     do not need to do this if point is at the start of a change
     region since it will be restored there anyway, and we must not do
     this if the buffer has changed since the last command, since the
     value of point that we have will be for that buffer, not this.*/
  if (at_boundary
      && point_before_last_command_or_undo != beg
      && buffer_before_last_command_or_undo == current_buffer )
    bset_undo_list (current_buffer,
		    Fcons (make_number (point_before_last_command_or_undo),
			   BVAR (current_buffer, undo_list)));
}

/* Record an insertion that just happened or is about to happen,
   for LENGTH characters at position BEG.
   (It is possible to record an insertion before or after the fact
   because we don't need to record the contents.)  */

void
record_insert (ptrdiff_t beg, ptrdiff_t length)
{
  Lisp_Object lbeg, lend;

  if (EQ (BVAR (current_buffer, undo_list), Qt))
    return;

  prepare_record ();

  record_point (beg);

  /* If this is following another insertion and consecutive with it
     in the buffer, combine the two.  */
  if (CONSP (BVAR (current_buffer, undo_list)))
    {
      Lisp_Object elt;
      elt = XCAR (BVAR (current_buffer, undo_list));
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
  bset_undo_list (current_buffer,
		  Fcons (Fcons (lbeg, lend), BVAR (current_buffer, undo_list)));
}

/* Record the fact that markers in the region of FROM, TO are about to
   be adjusted.  This is done only when a marker points within text
   being deleted, because that's the only case where an automatic
   marker adjustment won't be inverted automatically by undoing the
   buffer modification.  */

static void
record_marker_adjustments (ptrdiff_t from, ptrdiff_t to)
{
  Lisp_Object marker;
  register struct Lisp_Marker *m;
  register ptrdiff_t charpos, adjustment;

  prepare_record();

  for (m = BUF_MARKERS (current_buffer); m; m = m->next)
    {
      charpos = m->charpos;
      eassert (charpos <= Z);

      if (from <= charpos && charpos <= to)
        {
          /* insertion_type nil markers will end up at the beginning of
             the re-inserted text after undoing a deletion, and must be
             adjusted to move them to the correct place.

             insertion_type t markers will automatically move forward
             upon re-inserting the deleted text, so we have to arrange
             for them to move backward to the correct position.  */
          adjustment = (m->insertion_type ? to : from) - charpos;

          if (adjustment)
            {
              XSETMISC (marker, m);
              bset_undo_list
                (current_buffer,
                 Fcons (Fcons (marker, make_number (adjustment)),
                        BVAR (current_buffer, undo_list)));
            }
        }
    }
}

/* Record that a deletion is about to take place, of the characters in
   STRING, at location BEG.  Optionally record adjustments for markers
   in the region STRING occupies in the current buffer.  */
void
record_delete (ptrdiff_t beg, Lisp_Object string, bool record_markers)
{
  Lisp_Object sbeg;

  if (EQ (BVAR (current_buffer, undo_list), Qt))
    return;

  prepare_record ();

  record_point (beg);

  if (PT == beg + SCHARS (string))
    {
      XSETINT (sbeg, -beg);
    }
  else
    {
      XSETFASTINT (sbeg, beg);
    }

  /* primitive-undo assumes marker adjustments are recorded
     immediately before the deletion is recorded.  See bug 16818
     discussion.  */
  if (record_markers)
    record_marker_adjustments (beg, beg + SCHARS (string));

  bset_undo_list
    (current_buffer,
     Fcons (Fcons (string, sbeg), BVAR (current_buffer, undo_list)));
}

/* Record that a replacement is about to take place,
   for LENGTH characters at location BEG.
   The replacement must not change the number of characters.  */

void
record_change (ptrdiff_t beg, ptrdiff_t length)
{
  record_delete (beg, make_buffer_string (beg, beg + length, true), false);
  record_insert (beg, length);
}

/* Record that an unmodified buffer is about to be changed.
   Record the file modification date so that when undoing this entry
   we can tell whether it is obsolete because the file was saved again.  */

void
record_first_change (void)
{
  struct buffer *base_buffer = current_buffer;

  if (EQ (BVAR (current_buffer, undo_list), Qt))
    return;

  if (base_buffer->base_buffer)
    base_buffer = base_buffer->base_buffer;

  bset_undo_list (current_buffer,
		  Fcons (Fcons (Qt, Fvisited_file_modtime ()),
			 BVAR (current_buffer, undo_list)));
}

/* Record a change in property PROP (whose old value was VAL)
   for LENGTH characters starting at position BEG in BUFFER.  */

void
record_property_change (ptrdiff_t beg, ptrdiff_t length,
			Lisp_Object prop, Lisp_Object value,
			Lisp_Object buffer)
{
  Lisp_Object lbeg, lend, entry;
  struct buffer *buf = XBUFFER (buffer);

  if (EQ (BVAR (buf, undo_list), Qt))
    return;

  prepare_record();

  if (MODIFF <= SAVE_MODIFF)
    record_first_change ();

  XSETINT (lbeg, beg);
  XSETINT (lend, beg + length);
  entry = Fcons (Qnil, Fcons (prop, Fcons (value, Fcons (lbeg, lend))));
  bset_undo_list (current_buffer,
		  Fcons (entry, BVAR (current_buffer, undo_list)));
}

DEFUN ("undo-boundary", Fundo_boundary, Sundo_boundary, 0, 0, 0,
       doc: /* Mark a boundary between units of undo.
An undo command will stop at this point,
but another undo command will undo to the previous boundary.  */)
  (void)
{
  Lisp_Object tem;
  if (EQ (BVAR (current_buffer, undo_list), Qt))
    return Qnil;
  tem = Fcar (BVAR (current_buffer, undo_list));
  if (!NILP (tem))
    {
      /* One way or another, cons nil onto the front of the undo list.  */
      if (!NILP (pending_boundary))
	{
	  /* If we have preallocated the cons cell to use here,
	     use that one.  */
	  XSETCDR (pending_boundary, BVAR (current_buffer, undo_list));
	  bset_undo_list (current_buffer, pending_boundary);
	  pending_boundary = Qnil;
	}
      else
	bset_undo_list (current_buffer,
			Fcons (Qnil, BVAR (current_buffer, undo_list)));
    }

  Fset (Qundo_auto__last_boundary_cause, Qexplicit);
  point_before_last_command_or_undo = PT;
  buffer_before_last_command_or_undo = current_buffer;

  return Qnil;
}

/* At garbage collection time, make an undo list shorter at the end,
   returning the truncated list.  How this is done depends on the
   variables undo-limit, undo-strong-limit and undo-outer-limit.
   In some cases this works by calling undo-outer-limit-function.  */

void
truncate_undo_list (struct buffer *b)
{
  Lisp_Object list;
  Lisp_Object prev, next, last_boundary;
  EMACS_INT size_so_far = 0;

  /* Make sure that calling undo-outer-limit-function
     won't cause another GC.  */
  ptrdiff_t count = inhibit_garbage_collection ();

  /* Make the buffer current to get its local values of variables such
     as undo_limit.  Also so that Vundo_outer_limit_function can
     tell which buffer to operate on.  */
  record_unwind_current_buffer ();
  set_buffer_internal (b);

  list = BVAR (b, undo_list);

  prev = Qnil;
  next = list;
  last_boundary = Qnil;

  /* If the first element is an undo boundary, skip past it.  */
  if (CONSP (next) && NILP (XCAR (next)))
    {
      /* Add in the space occupied by this element and its chain link.  */
      size_so_far += sizeof (struct Lisp_Cons);

      /* Advance to next element.  */
      prev = next;
      next = XCDR (next);
    }

  /* Always preserve at least the most recent undo record
     unless it is really horribly big.

     Skip, skip, skip the undo, skip, skip, skip the undo,
     Skip, skip, skip the undo, skip to the undo bound'ry.  */

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
			    + SCHARS (XCAR (elt)));
	}

      /* Advance to next element.  */
      prev = next;
      next = XCDR (next);
    }

  /* If by the first boundary we have already passed undo_outer_limit,
     we're heading for memory full, so offer to clear out the list.  */
  if (INTEGERP (Vundo_outer_limit)
      && size_so_far > XINT (Vundo_outer_limit)
      && !NILP (Vundo_outer_limit_function))
    {
      Lisp_Object tem;

      /* Normally the function this calls is undo-outer-limit-truncate.  */
      tem = call1 (Vundo_outer_limit_function, make_number (size_so_far));
      if (! NILP (tem))
	{
	  /* The function is responsible for making
	     any desired changes in buffer-undo-list.  */
	  unbind_to (count, Qnil);
	  return;
	}
    }

  if (CONSP (next))
    last_boundary = prev;

  /* Keep additional undo data, if it fits in the limits.  */
  while (CONSP (next))
    {
      Lisp_Object elt;
      elt = XCAR (next);

      /* When we get to a boundary, decide whether to truncate
	 either before or after it.  The lower threshold, undo_limit,
	 tells us to truncate after it.  If its size pushes past
	 the higher threshold undo_strong_limit, we truncate before it.  */
      if (NILP (elt))
	{
	  if (size_so_far > undo_strong_limit)
	    break;
	  last_boundary = prev;
	  if (size_so_far > undo_limit)
	    break;
	}

      /* Add in the space occupied by this element and its chain link.  */
      size_so_far += sizeof (struct Lisp_Cons);
      if (CONSP (elt))
	{
	  size_so_far += sizeof (struct Lisp_Cons);
	  if (STRINGP (XCAR (elt)))
	    size_so_far += (sizeof (struct Lisp_String) - 1
			    + SCHARS (XCAR (elt)));
	}

      /* Advance to next element.  */
      prev = next;
      next = XCDR (next);
    }

  /* If we scanned the whole list, it is short enough; don't change it.  */
  if (NILP (next))
    ;
  /* Truncate at the boundary where we decided to truncate.  */
  else if (!NILP (last_boundary))
    XSETCDR (last_boundary, Qnil);
  /* There's nothing we decided to keep, so clear it out.  */
  else
    bset_undo_list (b, Qnil);

  unbind_to (count, Qnil);
}


void
syms_of_undo (void)
{
  DEFSYM (Qinhibit_read_only, "inhibit-read-only");
  DEFSYM (Qundo_auto__last_boundary_cause, "undo-auto--last-boundary-cause");
  DEFSYM (Qexplicit, "explicit");

  /* Marker for function call undo list elements.  */
  DEFSYM (Qapply, "apply");

  pending_boundary = Qnil;
  staticpro (&pending_boundary);

  defsubr (&Sundo_boundary);

  DEFVAR_INT ("undo-limit", undo_limit,
	      doc: /* Keep no more undo information once it exceeds this size.
This limit is applied when garbage collection happens.
When a previous command increases the total undo list size past this
value, the earlier commands that came before it are forgotten.

The size is counted as the number of bytes occupied,
which includes both saved text and other data.  */);
  undo_limit = 80000;

  DEFVAR_INT ("undo-strong-limit", undo_strong_limit,
	      doc: /* Don't keep more than this much size of undo information.
This limit is applied when garbage collection happens.
When a previous command increases the total undo list size past this
value, that command and the earlier commands that came before it are forgotten.
However, the most recent buffer-modifying command's undo info
is never discarded for this reason.

The size is counted as the number of bytes occupied,
which includes both saved text and other data.  */);
  undo_strong_limit = 120000;

  DEFVAR_LISP ("undo-outer-limit", Vundo_outer_limit,
	      doc: /* Outer limit on size of undo information for one command.
At garbage collection time, if the current command has produced
more than this much undo information, it discards the info and displays
a warning.  This is a last-ditch limit to prevent memory overflow.

The size is counted as the number of bytes occupied, which includes
both saved text and other data.  A value of nil means no limit.  In
this case, accumulating one huge undo entry could make Emacs crash as
a result of memory overflow.

In fact, this calls the function which is the value of
`undo-outer-limit-function' with one argument, the size.
The text above describes the behavior of the function
that variable usually specifies.  */);
  Vundo_outer_limit = make_number (12000000);

  DEFVAR_LISP ("undo-outer-limit-function", Vundo_outer_limit_function,
	       doc: /* Function to call when an undo list exceeds `undo-outer-limit'.
This function is called with one argument, the current undo list size
for the most recent command (since the last undo boundary).
If the function returns t, that means truncation has been fully handled.
If it returns nil, the other forms of truncation are done.

Garbage collection is inhibited around the call to this function,
so it must make sure not to do a lot of consing.  */);
  Vundo_outer_limit_function = Qnil;

  DEFVAR_BOOL ("undo-inhibit-record-point", undo_inhibit_record_point,
	       doc: /* Non-nil means do not record `point' in `buffer-undo-list'.  */);
  undo_inhibit_record_point = false;
}
