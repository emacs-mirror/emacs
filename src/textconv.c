/* String conversion support for graphics terminals.

Copyright (C) 2023 Free Software Foundation, Inc.

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

/* String conversion support.

   Many input methods require access to text surrounding the cursor.
   They may then request that the text editor remove or substitute
   that text for something else, for example when providing the
   ability to ``undo'' or ``edit'' previously composed text.  This is
   most commonly seen in input methods for CJK laguages for X Windows,
   and is extensively used throughout Android by input methods for all
   kinds of scripts.  */

#include <config.h>

#include "textconv.h"
#include "buffer.h"
#include "syntax.h"



/* The window system's text conversion interface.
   NULL when the window system has not set up text conversion.

   This interface will later be heavily extended on the
   feature/android branch to deal with Android's much less
   straightforward text conversion protocols.  */

static struct textconv_interface *text_interface;



/* Copy the portion of the current buffer described by BEG, BEG_BYTE,
   END, END_BYTE to the buffer BUFFER, which is END_BYTE - BEG_BYTEs
   long.  */

static void
copy_buffer (ptrdiff_t beg, ptrdiff_t beg_byte,
	     ptrdiff_t end, ptrdiff_t end_byte,
	     char *buffer)
{
  ptrdiff_t beg0, end0, beg1, end1, size;

  if (beg_byte < GPT_BYTE && GPT_BYTE < end_byte)
    {
      /* Two regions, before and after the gap.  */
      beg0 = beg_byte;
      end0 = GPT_BYTE;
      beg1 = GPT_BYTE + GAP_SIZE - BEG_BYTE;
      end1 = end_byte + GAP_SIZE - BEG_BYTE;
    }
  else
    {
      /* The only region.  */
      beg0 = beg_byte;
      end0 = end_byte;
      beg1 = -1;
      end1 = -1;
    }

  size = end0 - beg0;
  memcpy (buffer, BYTE_POS_ADDR (beg0), size);
  if (beg1 != -1)
    memcpy (buffer, BEG_ADDR + beg1, end1 - beg1);
}



/* Conversion query.  */

/* Perform the text conversion operation specified in QUERY and return
   the results.

   Find the text between QUERY->position from point on F's selected
   window and QUERY->factor times QUERY->direction from that
   position.  Return it in QUERY->text.

   Then, either delete that text from the buffer if QUERY->operation
   is TEXTCONV_SUBSTITUTION, or return 0.

   Value is 0 if QUERY->operation was not TEXTCONV_SUBSTITUTION
   or if deleting the text was successful, and 1 otherwise.  */

int
textconv_query (struct frame *f, struct textconv_callback_struct *query)
{
  specpdl_ref count;
  ptrdiff_t pos, pos_byte, end, end_byte;
  ptrdiff_t temp, temp1;
  char *buffer;

  /* Save the excursion, as there will be extensive changes to the
     selected window.  */
  count = SPECPDL_INDEX ();
  record_unwind_protect_excursion ();

  /* Inhibit quitting.  */
  specbind (Qinhibit_quit, Qt);

  /* Temporarily switch to F's selected window.  */
  Fselect_window (f->selected_window, Qt);

  /* Now find the appropriate text bounds for QUERY.  First, move
     point QUERY->position steps forward or backwards.  */

  pos = PT;

  /* If pos is outside the accessible part of the buffer or if it
     overflows, move back to point or to the extremes of the
     accessible region.  */

  if (ckd_add (&pos, pos, query->position))
    pos = PT;

  if (pos < BEGV)
    pos = BEGV;

  if (pos > ZV)
    pos = ZV;

  /* Move to pos.  */
  set_point (pos);
  pos = PT;
  pos_byte = PT_BYTE;

  /* Now scan forward or backwards according to what is in QUERY.  */

  switch (query->direction)
    {
    case TEXTCONV_FORWARD_CHAR:
      /* Move forward by query->factor characters.  */
      if (ckd_add (&end, pos, query->factor) || end > ZV)
	end = ZV;

      end_byte = CHAR_TO_BYTE (end);
      break;

    case TEXTCONV_BACKWARD_CHAR:
      /* Move backward by query->factor characters.  */
      if (ckd_sub (&end, pos, query->factor) || end < BEGV)
	end = BEGV;

      end_byte = CHAR_TO_BYTE (end);
      break;

    case TEXTCONV_FORWARD_WORD:
      /* Move forward by query->factor word.  */
      end = scan_words (pos, (EMACS_INT) query->factor);

      if (!end)
	{
	  end = ZV;
	  end_byte = ZV_BYTE;
	}
      else
	end_byte = CHAR_TO_BYTE (end);

      break;

    case TEXTCONV_BACKWARD_WORD:
      /* Move backwards by query->factor word.  */
      end = scan_words (pos, 0 - (EMACS_INT) query->factor);

      if (!end)
	{
	  end = BEGV;
	  end_byte = BEGV_BYTE;
	}
      else
	end_byte = CHAR_TO_BYTE (end);

      break;

    case TEXTCONV_CARET_UP:
      /* Move upwards one visual line, keeping the column intact.  */
      Fvertical_motion (Fcons (Fcurrent_column (), make_fixnum (-1)),
			Qnil, Qnil);
      end = PT;
      end_byte = PT_BYTE;
      break;

    case TEXTCONV_CARET_DOWN:
      /* Move downwards one visual line, keeping the column
	 intact.  */
      Fvertical_motion (Fcons (Fcurrent_column (), make_fixnum (1)),
			Qnil, Qnil);
      end = PT;
      end_byte = PT_BYTE;
      break;

    case TEXTCONV_NEXT_LINE:
      /* Move one line forward.  */
      scan_newline (pos, pos_byte, ZV, ZV_BYTE,
		    query->factor, false);
      end = PT;
      end_byte = PT_BYTE;
      break;

    case TEXTCONV_PREVIOUS_LINE:
      /* Move one line backwards.  */
      scan_newline (pos, pos_byte, BEGV, BEGV_BYTE,
		    0 - (EMACS_INT) query->factor, false);
      end = PT;
      end_byte = PT_BYTE;
      break;

    case TEXTCONV_LINE_START:
      /* Move to the beginning of the line.  */
      Fbeginning_of_line (Qnil);
      end = PT;
      end_byte = PT_BYTE;
      break;

    case TEXTCONV_LINE_END:
      /* Move to the end of the line.  */
      Fend_of_line (Qnil);
      end = PT;
      end_byte = PT_BYTE;
      break;

    case TEXTCONV_ABSOLUTE_POSITION:
      /* How to implement this is unclear.  */
      SET_PT (query->factor);
      end = PT;
      end_byte = PT_BYTE;
      break;

    default:
      unbind_to (count, Qnil);
      return 1;
    }

  /* Sort end and pos.  */

  if (end < pos)
    {
      eassert (end_byte < pos_byte);
      temp = pos_byte;
      temp1 = pos;
      pos_byte = end_byte;
      pos = end;
      end = temp1;
      end_byte = temp;
    }

  /* Return the string first.  */
  buffer = xmalloc (end_byte - pos_byte);
  copy_buffer (pos, pos_byte, end, end_byte, buffer);
  query->text.text = buffer;
  query->text.length = end - pos;
  query->text.bytes = end_byte - pos_byte;

  /* Next, perform any operation specified.  */

  switch (query->operation)
    {
    case TEXTCONV_SUBSTITUTION:
      if (safe_del_range (pos, end))
	{
	  /* Undo any changes to the excursion.  */
	  unbind_to (count, Qnil);
	  return 1;
	}

    default:
      break;
    }

  /* Undo any changes to the excursion.  */
  unbind_to (count, Qnil);
  return 0;
}



/* Window system interface.  These are called from the rest of
   Emacs.  */

/* Notice that F's selected window has been set from redisplay.
   Reset F's input method state.  */

void
report_selected_window_change (struct frame *f)
{
  if (!text_interface)
    return;

  text_interface->reset (f);
}

/* Register INTERFACE as the text conversion interface.  */

void
register_texconv_interface (struct textconv_interface *interface)
{
  text_interface = interface;
}
