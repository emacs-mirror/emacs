/* Buffer insertion/deletion and gap motion for GNU Emacs.
   Copyright (C) 1985, 1986, 1993 Free Software Foundation, Inc.

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


#include <config.h>
#include "lisp.h"
#include "intervals.h"
#include "buffer.h"
#include "window.h"
#include "blockinput.h"

/* Move gap to position `pos'.
   Note that this can quit!  */

move_gap (pos)
     int pos;
{
  if (pos < GPT)
    gap_left (pos, 0);
  else if (pos > GPT)
    gap_right (pos);
}

/* Move the gap to POS, which is less than the current GPT.
   If NEWGAP is nonzero, then don't update beg_unchanged and end_unchanged.  */

gap_left (pos, newgap)
     register int pos;
     int newgap;
{
  register unsigned char *to, *from;
  register int i;
  int new_s1;

  pos--;

  if (!newgap)
    {
      if (unchanged_modified == MODIFF)
	{
	  beg_unchanged = pos;
	  end_unchanged = Z - pos - 1;
	}
      else
	{
	  if (Z - GPT < end_unchanged)
	    end_unchanged = Z - GPT;
	  if (pos < beg_unchanged)
	    beg_unchanged = pos;
	}
    }

  i = GPT;
  to = GAP_END_ADDR;
  from = GPT_ADDR;
  new_s1 = GPT - BEG;

  /* Now copy the characters.  To move the gap down,
     copy characters up.  */

  while (1)
    {
      /* I gets number of characters left to copy.  */
      i = new_s1 - pos;
      if (i == 0)
	break;
      /* If a quit is requested, stop copying now.
	 Change POS to be where we have actually moved the gap to.  */
      if (QUITP)
	{
	  pos = new_s1;
	  break;
	}
      /* Move at most 32000 chars before checking again for a quit.  */
      if (i > 32000)
	i = 32000;
#ifdef GAP_USE_BCOPY
      if (i >= 128
	  /* bcopy is safe if the two areas of memory do not overlap
	     or on systems where bcopy is always safe for moving upward.  */
	  && (BCOPY_UPWARD_SAFE
	      || to - from >= 128))
	{
	  /* If overlap is not safe, avoid it by not moving too many
	     characters at once.  */
	  if (!BCOPY_UPWARD_SAFE && i > to - from)
	    i = to - from;
	  new_s1 -= i;
	  from -= i, to -= i;
	  bcopy (from, to, i);
	}
      else
#endif
	{
	  new_s1 -= i;
	  while (--i >= 0)
	    *--to = *--from;
	}
    }

  /* Adjust markers, and buffer data structure, to put the gap at POS.
     POS is where the loop above stopped, which may be what was specified
     or may be where a quit was detected.  */
  adjust_markers (pos + 1, GPT, GAP_SIZE);
  GPT = pos + 1;
  QUIT;
}

gap_right (pos)
     register int pos;
{
  register unsigned char *to, *from;
  register int i;
  int new_s1;

  pos--;

  if (unchanged_modified == MODIFF)
    {
      beg_unchanged = pos;
      end_unchanged = Z - pos - 1;
    }
  else
    {
      if (Z - pos - 1 < end_unchanged)
	end_unchanged = Z - pos - 1;
      if (GPT - BEG < beg_unchanged)
	beg_unchanged = GPT - BEG;
    }

  i = GPT;
  from = GAP_END_ADDR;
  to = GPT_ADDR;
  new_s1 = GPT - 1;

  /* Now copy the characters.  To move the gap up,
     copy characters down.  */

  while (1)
    {
      /* I gets number of characters left to copy.  */
      i = pos - new_s1;
      if (i == 0)
	break;
      /* If a quit is requested, stop copying now.
	 Change POS to be where we have actually moved the gap to.  */
      if (QUITP)
	{
	  pos = new_s1;
	  break;
	}
      /* Move at most 32000 chars before checking again for a quit.  */
      if (i > 32000)
	i = 32000;
#ifdef GAP_USE_BCOPY
      if (i >= 128
	  /* bcopy is safe if the two areas of memory do not overlap
	     or on systems where bcopy is always safe for moving downward.  */
	  && (BCOPY_DOWNWARD_SAFE
	      || from - to >= 128))
	{
	  /* If overlap is not safe, avoid it by not moving too many
	     characters at once.  */
	  if (!BCOPY_DOWNWARD_SAFE && i > from - to)
	    i = from - to;
	  new_s1 += i;
	  bcopy (from, to, i);
	  from += i, to += i;
	}
      else
#endif
	{
	  new_s1 += i;
	  while (--i >= 0)
	    *to++ = *from++;
	}
    }

  adjust_markers (GPT + GAP_SIZE, pos + 1 + GAP_SIZE, - GAP_SIZE);
  GPT = pos + 1;
  QUIT;
}

/* Add `amount' to the position of every marker in the current buffer
   whose current position is between `from' (exclusive) and `to' (inclusive).
   Also, any markers past the outside of that interval, in the direction
   of adjustment, are first moved back to the near end of the interval
   and then adjusted by `amount'.  */

adjust_markers (from, to, amount)
     register int from, to, amount;
{
  Lisp_Object marker;
  register struct Lisp_Marker *m;
  register int mpos;

  marker = current_buffer->markers;

  while (!NILP (marker))
    {
      m = XMARKER (marker);
      mpos = m->bufpos;
      if (amount > 0)
	{
	  if (mpos > to && mpos < to + amount)
	    mpos = to + amount;
	}
      else
	{
	  if (mpos > from + amount && mpos <= from)
	    mpos = from + amount;
	}
      if (mpos > from && mpos <= to)
	mpos += amount;
      m->bufpos = mpos;
      marker = m->chain;
    }
}

/* Make the gap INCREMENT characters longer.  */

make_gap (increment)
     int increment;
{
  unsigned char *result;
  Lisp_Object tem;
  int real_gap_loc;
  int old_gap_size;

  /* If we have to get more space, get enough to last a while.  */
  increment += 2000;

  BLOCK_INPUT;
  result = BUFFER_REALLOC (BEG_ADDR, (Z - BEG + GAP_SIZE + increment));
  UNBLOCK_INPUT;

  if (result == 0)
    memory_full ();
  BEG_ADDR = result;

  /* Prevent quitting in move_gap.  */
  tem = Vinhibit_quit;
  Vinhibit_quit = Qt;

  real_gap_loc = GPT;
  old_gap_size = GAP_SIZE;

  /* Call the newly allocated space a gap at the end of the whole space.  */
  GPT = Z + GAP_SIZE;
  GAP_SIZE = increment;

  /* Move the new gap down to be consecutive with the end of the old one.
     This adjusts the markers properly too.  */
  gap_left (real_gap_loc + old_gap_size, 1);

  /* Now combine the two into one large gap.  */
  GAP_SIZE += old_gap_size;
  GPT = real_gap_loc;

  Vinhibit_quit = tem;
}

/* Insert a string of specified length before point.
   DO NOT use this for the contents of a Lisp string!
   prepare_to_modify_buffer could relocate the string.  */

insert (string, length)
     register unsigned char *string;
     register length;
{
  register Lisp_Object temp;

  if (length < 1)
    return;

  /* Make sure point-max won't overflow after this insertion.  */
  XSET (temp, Lisp_Int, length + Z);
  if (length + Z != XINT (temp))
    error ("maximum buffer size exceeded");

  prepare_to_modify_buffer (point, point);

  if (point != GPT)
    move_gap (point);
  if (GAP_SIZE < length)
    make_gap (length - GAP_SIZE);

  record_insert (point, length);
  MODIFF++;

  bcopy (string, GPT_ADDR, length);

  /* Only defined if Emacs is compiled with USE_TEXT_PROPERTIES */
  offset_intervals (current_buffer, point, length);

  GAP_SIZE -= length;
  GPT += length;
  ZV += length;
  Z += length;
  SET_PT (point + length);

  signal_after_change (point-length, 0, length);
}

/* Insert the part of the text of STRING, a Lisp object assumed to be
   of type string, consisting of the LENGTH characters starting at
   position POS.  If the text of STRING has properties, they are absorbed
   into the buffer.

   It does not work to use `insert' for this, because a GC could happen
   before we bcopy the stuff into the buffer, and relocate the string
   without insert noticing.  */

insert_from_string (string, pos, length, inherit)
     Lisp_Object string;
     register int pos, length;
     int inherit;
{
  register Lisp_Object temp;
  struct gcpro gcpro1;

  if (length < 1)
    return;

  /* Make sure point-max won't overflow after this insertion.  */
  XSET (temp, Lisp_Int, length + Z);
  if (length + Z != XINT (temp))
    error ("maximum buffer size exceeded");

  GCPRO1 (string);
  prepare_to_modify_buffer (point, point);

  if (point != GPT)
    move_gap (point);
  if (GAP_SIZE < length)
    make_gap (length - GAP_SIZE);

  record_insert (point, length);
  MODIFF++;
  UNGCPRO;

  bcopy (XSTRING (string)->data, GPT_ADDR, length);

  /* Only defined if Emacs is compiled with USE_TEXT_PROPERTIES */
  offset_intervals (current_buffer, point, length);

  GAP_SIZE -= length;
  GPT += length;
  ZV += length;
  Z += length;

  /* Only defined if Emacs is compiled with USE_TEXT_PROPERTIES */
  graft_intervals_into_buffer (XSTRING (string)->intervals, point, length,
			       current_buffer, inherit);

  SET_PT (point + length);

  signal_after_change (point-length, 0, length);
}

/* Insert the character C before point */

void
insert_char (c)
     unsigned char c;
{
  insert (&c, 1);
}

/* Insert the null-terminated string S before point */

void
insert_string (s)
     char *s;
{
  insert (s, strlen (s));
}

/* Like `insert' except that all markers pointing at the place where
   the insertion happens are adjusted to point after it.
   Don't use this function to insert part of a Lisp string,
   since gc could happen and relocate it.  */

insert_before_markers (string, length)
     unsigned char *string;
     register int length;
{
  register int opoint = point;
  insert (string, length);
  adjust_markers (opoint - 1, opoint, length);
}

/* Insert part of a Lisp string, relocating markers after.  */

insert_from_string_before_markers (string, pos, length, inherit)
     Lisp_Object string;
     register int pos, length;
     int inherit;
{
  register int opoint = point;
  insert_from_string (string, pos, length, inherit);
  adjust_markers (opoint - 1, opoint, length);
}

/* Delete characters in current buffer
   from FROM up to (but not including) TO.  */

del_range (from, to)
     register int from, to;
{
  register int numdel;

  /* Make args be valid */
  if (from < BEGV)
    from = BEGV;
  if (to > ZV)
    to = ZV;

  if ((numdel = to - from) <= 0)
    return;

  /* Make sure the gap is somewhere in or next to what we are deleting.  */
  if (from > GPT)
    gap_right (from);
  if (to < GPT)
    gap_left (to, 0);

  prepare_to_modify_buffer (from, to);

  record_delete (from, numdel);
  MODIFF++;

  /* Relocate point as if it were a marker.  */
  if (from < point)
    {
      if (point < to)
	SET_PT (from);
      else
	SET_PT (point - numdel);
    }

  /* Only defined if Emacs is compiled with USE_TEXT_PROPERTIES */
  offset_intervals (current_buffer, point, - numdel);

  /* Relocate all markers pointing into the new, larger gap
     to point at the end of the text before the gap.  */
  adjust_markers (to + GAP_SIZE, to + GAP_SIZE, - numdel - GAP_SIZE);

  GAP_SIZE += numdel;
  ZV -= numdel;
  Z -= numdel;
  GPT = from;

  if (GPT - BEG < beg_unchanged)
    beg_unchanged = GPT - BEG;
  if (Z - GPT < end_unchanged)
    end_unchanged = Z - GPT;

  signal_after_change (from, numdel, 0);
}

/* Call this if you're about to change the region of BUFFER from START
   to END.  This checks the read-only properties of the region, calls
   the necessary modification hooks, and warns the next redisplay that
   it should pay attention to that area.  */
modify_region (buffer, start, end)
     struct buffer *buffer;
     int start, end;
{
  struct buffer *old_buffer = current_buffer;

  if (buffer != old_buffer)
    set_buffer_internal (buffer);

  prepare_to_modify_buffer (start, end);

  if (start - 1 < beg_unchanged || unchanged_modified == MODIFF)
    beg_unchanged = start - 1;
  if (Z - end < end_unchanged
      || unchanged_modified == MODIFF)
    end_unchanged = Z - end;
  MODIFF++;

  if (buffer != old_buffer)
    set_buffer_internal (old_buffer);
}

/* Check that it is okay to modify the buffer between START and END.
   Run the before-change-function, if any.  If intervals are in use,
   verify that the text to be modified is not read-only, and call
   any modification properties the text may have. */

prepare_to_modify_buffer (start, end)
     Lisp_Object start, end;
{
  if (!NILP (current_buffer->read_only))
    Fbarf_if_buffer_read_only ();

  /* Only defined if Emacs is compiled with USE_TEXT_PROPERTIES */
  verify_interval_modification (current_buffer, start, end);

  verify_overlay_modification (start, end);

#ifdef CLASH_DETECTION
  if (!NILP (current_buffer->filename)
      && current_buffer->save_modified >= MODIFF)
    lock_file (current_buffer->filename);
#else
  /* At least warn if this file has changed on disk since it was visited.  */
  if (!NILP (current_buffer->filename)
      && current_buffer->save_modified >= MODIFF
      && NILP (Fverify_visited_file_modtime (Fcurrent_buffer ()))
      && !NILP (Ffile_exists_p (current_buffer->filename)))
    call1 (intern ("ask-user-about-supersession-threat"),
	   current_buffer->filename);
#endif /* not CLASH_DETECTION */

  signal_before_change (start, end);

  Vdeactivate_mark = Qt;
}

static Lisp_Object
before_change_function_restore (value)
     Lisp_Object value;
{
  Vbefore_change_function = value;
}

static Lisp_Object
after_change_function_restore (value)
     Lisp_Object value;
{
  Vafter_change_function = value;
}

/* Signal a change to the buffer immediately before it happens.
   START and END are the bounds of the text to be changed,
   as Lisp objects.  */

signal_before_change (start, end)
     Lisp_Object start, end;
{
  /* If buffer is unmodified, run a special hook for that case.  */
  if (current_buffer->save_modified >= MODIFF
      && !NILP (Vfirst_change_hook)
      && !NILP (Vrun_hooks))
    call1 (Vrun_hooks, Qfirst_change_hook);

  /* Now in any case run the before-change-function if any.  */
  if (!NILP (Vbefore_change_function))
    {
      int count = specpdl_ptr - specpdl;
      Lisp_Object function;

      function = Vbefore_change_function;
      record_unwind_protect (after_change_function_restore,
			     Vafter_change_function);
      record_unwind_protect (before_change_function_restore,
			     Vbefore_change_function);
      Vafter_change_function = Qnil;
      Vbefore_change_function = Qnil;

      call2 (function, start, end);
      unbind_to (count, Qnil);
    }
}

/* Signal a change immediately after it happens.
   POS is the address of the start of the changed text.
   LENDEL is the number of characters of the text before the change.
   (Not the whole buffer; just the part that was changed.)
   LENINS is the number of characters in the changed text.  */

signal_after_change (pos, lendel, lenins)
     int pos, lendel, lenins;
{
  if (!NILP (Vafter_change_function))
    {
      int count = specpdl_ptr - specpdl;
      Lisp_Object function;
      function = Vafter_change_function;

      record_unwind_protect (after_change_function_restore,
			     Vafter_change_function);
      record_unwind_protect (before_change_function_restore,
			     Vbefore_change_function);
      Vafter_change_function = Qnil;
      Vbefore_change_function = Qnil;

      call3 (function, make_number (pos), make_number (pos + lenins),
	     make_number (lendel));
      unbind_to (count, Qnil);
    }
}
