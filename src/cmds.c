/* Simple built-in editing commands.
   Copyright (C) 1985, 93, 94, 95, 96, 97, 1998, 2001 Free Software Foundation, Inc.

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
#include "commands.h"
#include "buffer.h"
#include "charset.h"
#include "syntax.h"
#include "window.h"
#include "keyboard.h"
#include "keymap.h"
#include "dispextern.h"

Lisp_Object Qkill_forward_chars, Qkill_backward_chars, Vblink_paren_function;

/* A possible value for a buffer's overwrite-mode variable.  */
Lisp_Object Qoverwrite_mode_binary;

/* Non-nil means put this face on the next self-inserting character.  */
Lisp_Object Vself_insert_face;

/* This is the command that set up Vself_insert_face.  */
Lisp_Object Vself_insert_face_command;

extern Lisp_Object Qface;

DEFUN ("forward-point", Fforward_point, Sforward_point, 1, 1, 0,
       doc: /* Return buffer position N characters after (before if N negative) point.  */)
     (n)
     Lisp_Object n;
{
  CHECK_NUMBER (n);

  return make_number (PT + XINT (n));
}

DEFUN ("forward-char", Fforward_char, Sforward_char, 0, 1, "p",
       doc: /* Move point right N characters (left if N is negative).
On reaching end of buffer, stop and signal error.  */)
     (n)
     Lisp_Object n;
{
  if (NILP (n))
    XSETFASTINT (n, 1);
  else
    CHECK_NUMBER (n);

  /* This used to just set point to point + XINT (n), and then check
     to see if it was within boundaries.  But now that SET_PT can
     potentially do a lot of stuff (calling entering and exiting
     hooks, etcetera), that's not a good approach.  So we validate the
     proposed position, then set point.  */
  {
    int new_point = PT + XINT (n);

    if (new_point < BEGV)
      {
	SET_PT (BEGV);
	Fsignal (Qbeginning_of_buffer, Qnil);
      }
    if (new_point > ZV)
      {
	SET_PT (ZV);
	Fsignal (Qend_of_buffer, Qnil);
      }

    SET_PT (new_point);
  }

  return Qnil;
}

DEFUN ("backward-char", Fbackward_char, Sbackward_char, 0, 1, "p",
       doc: /* Move point left N characters (right if N is negative).
On attempt to pass beginning or end of buffer, stop and signal error.  */)
     (n)
     Lisp_Object n;
{
  if (NILP (n))
    XSETFASTINT (n, 1);
  else
    CHECK_NUMBER (n);

  XSETINT (n, - XINT (n));
  return Fforward_char (n);
}

DEFUN ("forward-line", Fforward_line, Sforward_line, 0, 1, "p",
       doc: /* Move N lines forward (backward if N is negative).
Precisely, if point is on line I, move to the start of line I + N.
If there isn't room, go as far as possible (no error).
Returns the count of lines left to move.  If moving forward,
that is N - number of lines moved; if backward, N + number moved.
With positive N, a non-empty line at the end counts as one line
  successfully moved (for the return value).  */)
     (n)
     Lisp_Object n;
{
  int opoint = PT, opoint_byte = PT_BYTE;
  int pos, pos_byte;
  int count, shortage;

  if (NILP (n))
    count = 1;
  else
    {
      CHECK_NUMBER (n);
      count = XINT (n);
    }

  if (count <= 0)
    shortage = scan_newline (PT, PT_BYTE, BEGV, BEGV_BYTE, count - 1, 1);
  else
    shortage = scan_newline (PT, PT_BYTE, ZV, ZV_BYTE, count, 1);

  /* Since scan_newline does TEMP_SET_PT_BOTH,
     and we want to set PT "for real",
     go back to the old point and then come back here.  */
  pos = PT;
  pos_byte = PT_BYTE;
  TEMP_SET_PT_BOTH (opoint, opoint_byte);
  SET_PT_BOTH (pos, pos_byte);

  if (shortage > 0
      && (count <= 0
	  || (ZV > BEGV
	      && PT != opoint
	      && (FETCH_BYTE (PT_BYTE - 1) != '\n'))))
    shortage--;

  return make_number (count <= 0 ? - shortage : shortage);
}

DEFUN ("beginning-of-line", Fbeginning_of_line, Sbeginning_of_line, 0, 1, "p",
       doc: /* Move point to beginning of current line.
With argument N not nil or 1, move forward N - 1 lines first.
If point reaches the beginning or end of buffer, it stops there.

This command does not move point across a field boundary unless doing so
would move beyond there to a different line; If N is nil or 1, and point
starts at a field boundary, point does not move.  To ignore field
boundaries, either bind `inhibit-field-text-motion' to t, or use the
`forward-line' function instead.  For instance, `(forward-line 0)' does
the same thing as `(beginning-of-line)', except that it ignores field
boundaries.  */)
     (n)
     Lisp_Object n;
{
  if (NILP (n))
    XSETFASTINT (n, 1);
  else
    CHECK_NUMBER (n);

  SET_PT (XINT (Fline_beginning_position (n)));

  return Qnil;
}

DEFUN ("end-of-line", Fend_of_line, Send_of_line, 0, 1, "p",
       doc: /* Move point to end of current line.
With argument N not nil or 1, move forward N - 1 lines first.
If point reaches the beginning or end of buffer, it stops there.
To ignore intangibility, bind `inhibit-text-motion-hooks' to t.

This command does not move point across a field boundary unless doing so
would move beyond there to a different line; if N is nil or 1, and
point starts at a field boundary, point does not move.  To ignore field
boundaries bind `inhibit-field-text-motion' to t.  */)
     (n)
     Lisp_Object n;
{
  int newpos;

  if (NILP (n))
    XSETFASTINT (n, 1);
  else
    CHECK_NUMBER (n);

  while (1)
    {
      newpos = XINT (Fline_end_position (n));
      SET_PT (newpos);

      if (PT > newpos
	  && FETCH_CHAR (PT - 1) == '\n')
	{
	  /* If we skipped over a newline that follows
	     an invisible intangible run,
	     move back to the last tangible position
	     within the line.  */

	  SET_PT (PT - 1);
	  break;
	}
      else if (PT > newpos && PT < ZV
	       && FETCH_CHAR (PT) != '\n')
	/* If we skipped something intangible
	   and now we're not really at eol,
	   keep going.  */
	n = make_number (1);
      else
	break;
    }

  return Qnil;
}

DEFUN ("delete-char", Fdelete_char, Sdelete_char, 1, 2, "p\nP",
       doc: /* Delete the following N characters (previous if N is negative).
Optional second arg KILLFLAG non-nil means kill instead (save in kill ring).
Interactively, N is the prefix arg, and KILLFLAG is set if
N was explicitly specified.  */)
     (n, killflag)
     Lisp_Object n, killflag;
{
  int pos;

  CHECK_NUMBER (n);

  pos = PT + XINT (n);
  if (NILP (killflag))
    {
      if (XINT (n) < 0)
	{
	  if (pos < BEGV)
	    Fsignal (Qbeginning_of_buffer, Qnil);
	  else
	    del_range (pos, PT);
	}
      else
	{
	  if (pos > ZV)
	    Fsignal (Qend_of_buffer, Qnil);
	  else
	    del_range (PT, pos);
	}
    }
  else
    {
      call1 (Qkill_forward_chars, n);
    }
  return Qnil;
}

DEFUN ("delete-backward-char", Fdelete_backward_char, Sdelete_backward_char,
       1, 2, "p\nP",
       doc: /* Delete the previous N characters (following if N is negative).
Optional second arg KILLFLAG non-nil means kill instead (save in kill ring).
Interactively, N is the prefix arg, and KILLFLAG is set if
N was explicitly specified.  */)
     (n, killflag)
     Lisp_Object n, killflag;
{
  Lisp_Object value;
  int deleted_special = 0;
  int pos, pos_byte, i;

  CHECK_NUMBER (n);

  /* See if we are about to delete a tab or newline backwards.  */
  pos = PT;
  pos_byte = PT_BYTE;
  for (i = 0; i < XINT (n) && pos_byte > BEGV_BYTE; i++)
    {
      int c;

      DEC_BOTH (pos, pos_byte);
      c = FETCH_BYTE (pos_byte);
      if (c == '\t' || c == '\n')
	{
	  deleted_special = 1;
	  break;
	}
    }

  /* In overwrite mode, back over columns while clearing them out,
     unless at end of line.  */
  if (XINT (n) > 0
      && ! NILP (current_buffer->overwrite_mode)
      && ! deleted_special
      && ! (PT == ZV || FETCH_BYTE (PT_BYTE) == '\n'))
    {
      int column = (int) current_column (); /* iftc */

      value = Fdelete_char (make_number (-XINT (n)), killflag);
      i = column - (int) current_column (); /* iftc */
      Finsert_char (make_number (' '), make_number (i), Qnil);
      /* Whitespace chars are ASCII chars, so we can simply subtract.  */
      SET_PT_BOTH (PT - i, PT_BYTE - i);
    }
  else
    value = Fdelete_char (make_number (-XINT (n)), killflag);

  return value;
}

DEFUN ("self-insert-command", Fself_insert_command, Sself_insert_command, 1, 1, "p",
       doc: /* Insert the character you type.
Whichever character you type to run this command is inserted.  */)
     (n)
     Lisp_Object n;
{
  int character = XINT (last_command_char);

  CHECK_NUMBER (n);

  /* Barf if the key that invoked this was not a character.  */
  if (!INTEGERP (last_command_char))
    bitch_at_user ();
  else if (XINT (n) >= 2 && NILP (current_buffer->overwrite_mode))
    {
      int modified_char = character;
      /* Add the offset to the character, for Finsert_char.
	 We pass internal_self_insert the unmodified character
	 because it itself does this offsetting.  */
      if (! NILP (current_buffer->enable_multibyte_characters))
	modified_char = unibyte_char_to_multibyte (modified_char);

      XSETFASTINT (n, XFASTINT (n) - 2);
      /* The first one might want to expand an abbrev.  */
      internal_self_insert (character, 1);
      /* The bulk of the copies of this char can be inserted simply.
	 We don't have to handle a user-specified face specially
	 because it will get inherited from the first char inserted.  */
      Finsert_char (make_number (modified_char), n, Qt);
      /* The last one might want to auto-fill.  */
      internal_self_insert (character, 0);
    }
  else
    while (XINT (n) > 0)
      {
	/* Ok since old and new vals both nonneg */
	XSETFASTINT (n, XFASTINT (n) - 1);
	internal_self_insert (character, XFASTINT (n) != 0);
      }

  return Qnil;
}

/* Insert character C.  If NOAUTOFILL is nonzero, don't do autofill
   even if it is enabled.

   If this insertion is suitable for direct output (completely simple),
   return 0.  A value of 1 indicates this *might* not have been simple.
   A value of 2 means this did things that call for an undo boundary.  */

int
internal_self_insert (c, noautofill)
     int c;
     int noautofill;
{
  extern Lisp_Object Fexpand_abbrev ();
  int hairy = 0;
  Lisp_Object tem;
  register enum syntaxcode synt;
  Lisp_Object overwrite, string;
  /* Length of multi-byte form of C.  */
  int len;
  /* Working buffer and pointer for multi-byte form of C.  */
  unsigned char str[MAX_MULTIBYTE_LENGTH];
  int chars_to_delete = 0;
  int spaces_to_insert = 0;

  overwrite = current_buffer->overwrite_mode;
  if (!NILP (Vbefore_change_functions) || !NILP (Vafter_change_functions))
    hairy = 1;

  /* At first, get multi-byte form of C in STR.  */
  if (!NILP (current_buffer->enable_multibyte_characters))
    {
      c = unibyte_char_to_multibyte (c);
      len = CHAR_STRING (c, str);
      if (len == 1)
	/* If C has modifier bits, this makes C an appropriate
           one-byte char.  */
	c = *str;
    }
  else
    {
      str[0] = (SINGLE_BYTE_CHAR_P (c)
		? c
		: multibyte_char_to_unibyte (c, Qnil));
      len = 1;
    }
  if (!NILP (overwrite)
      && PT < ZV)
    {
      /* In overwrite-mode, we substitute a character at point (C2,
	 hereafter) by C.  For that, we delete C2 in advance.  But,
	 just substituting C2 by C may move a remaining text in the
	 line to the right or to the left, which is not preferable.
	 So we insert more spaces or delete more characters in the
	 following cases: if C is narrower than C2, after deleting C2,
	 we fill columns with spaces, if C is wider than C2, we delete
	 C2 and several characters following C2.  */

      /* This is the character after point.  */
      int c2 = FETCH_CHAR (PT_BYTE);

      /* Column the cursor should be placed at after this insertion.
         The correct value should be calculated only when necessary.  */
      int target_clm = 0;

      /* Overwriting in binary-mode always replaces C2 by C.
	 Overwriting in textual-mode doesn't always do that.
	 It inserts newlines in the usual way,
	 and inserts any character at end of line
	 or before a tab if it doesn't use the whole width of the tab.  */
      if (EQ (overwrite, Qoverwrite_mode_binary)
	  || (c != '\n'
	      && c2 != '\n'
	      && ! (c2 == '\t'
		    && XINT (current_buffer->tab_width) > 0
		    && XFASTINT (current_buffer->tab_width) < 20
		    && (target_clm = ((int) current_column () /* iftc */
				      + XINT (Fchar_width (make_number (c)))),
			target_clm % XFASTINT (current_buffer->tab_width)))))
	{
	  int pos = PT;
	  int pos_byte = PT_BYTE;

	  if (target_clm == 0)
	    chars_to_delete = 1;
	  else
	    {
	      /* The actual cursor position after the trial of moving
		 to column TARGET_CLM.  It is greater than TARGET_CLM
		 if the TARGET_CLM is middle of multi-column
		 character.  In that case, the new point is set after
		 that character.  */
	      int actual_clm
		= XFASTINT (Fmove_to_column (make_number (target_clm), Qnil));

	      chars_to_delete = PT - pos;

	      if (actual_clm > target_clm)
		{
		  /* We will delete too many columns.  Let's fill columns
		     by spaces so that the remaining text won't move.  */
		  spaces_to_insert = actual_clm - target_clm;
		}
	    }
	  SET_PT_BOTH (pos, pos_byte);
	  hairy = 2;
	}
      hairy = 2;
    }
  if (!NILP (current_buffer->abbrev_mode)
      && SYNTAX (c) != Sword
      && NILP (current_buffer->read_only)
      && PT > BEGV && SYNTAX (XFASTINT (Fprevious_char ())) == Sword)
    {
      int modiff = MODIFF;
      Lisp_Object sym;

      sym = Fexpand_abbrev ();

      /* If we expanded an abbrev which has a hook,
	 and the hook has a non-nil `no-self-insert' property,
	 return right away--don't really self-insert.  */
      if (! NILP (sym) && ! NILP (XSYMBOL (sym)->function)
	  && SYMBOLP (XSYMBOL (sym)->function))
	{
	  Lisp_Object prop;
	  prop = Fget (XSYMBOL (sym)->function, intern ("no-self-insert"));
	  if (! NILP (prop))
	    return 1;
	}

      if (MODIFF != modiff)
	hairy = 2;
    }

  if (chars_to_delete)
    {
      string = make_string_from_bytes (str, 1, len);
      if (spaces_to_insert)
	{
	  tem = Fmake_string (make_number (spaces_to_insert),
			      make_number (' '));
	  string = concat2 (tem, string);
	}

      replace_range (PT, PT + chars_to_delete, string, 1, 1, 1);
      Fforward_char (make_number (1 + spaces_to_insert));
    }
  else
    insert_and_inherit (str, len);

  if ((CHAR_TABLE_P (Vauto_fill_chars)
       ? !NILP (CHAR_TABLE_REF (Vauto_fill_chars, c))
       : (c == ' ' || c == '\n'))
      && !noautofill
      && !NILP (current_buffer->auto_fill_function))
    {
      Lisp_Object tem;

      if (c == '\n')
	/* After inserting a newline, move to previous line and fill
	   that.  Must have the newline in place already so filling and
	   justification, if any, know where the end is going to be.  */
	SET_PT_BOTH (PT - 1, PT_BYTE - 1);
      tem = call0 (current_buffer->auto_fill_function);
      if (c == '\n')
	SET_PT_BOTH (PT + 1, PT_BYTE + 1);
      if (!NILP (tem))
	hairy = 2;
    }

  /* If previous command specified a face to use, use it.  */
  if (!NILP (Vself_insert_face)
      && EQ (current_kboard->Vlast_command, Vself_insert_face_command))
    {
      Fput_text_property (make_number (PT - 1), make_number (PT),
			  Qface, Vself_insert_face, Qnil);
      Vself_insert_face = Qnil;
    }

  synt = SYNTAX (c);
  if ((synt == Sclose || synt == Smath)
      && !NILP (Vblink_paren_function) && INTERACTIVE
      && !noautofill)
    {
      call0 (Vblink_paren_function);
      hairy = 2;
    }
  return hairy;
}

/* module initialization */

void
syms_of_cmds ()
{
  Qkill_backward_chars = intern ("kill-backward-chars");
  staticpro (&Qkill_backward_chars);

  Qkill_forward_chars = intern ("kill-forward-chars");
  staticpro (&Qkill_forward_chars);

  Qoverwrite_mode_binary = intern ("overwrite-mode-binary");
  staticpro (&Qoverwrite_mode_binary);

  DEFVAR_LISP ("self-insert-face", &Vself_insert_face,
	       doc: /* If non-nil, set the face of the next self-inserting character to this.
See also `self-insert-face-command'.  */);
  Vself_insert_face = Qnil;

  DEFVAR_LISP ("self-insert-face-command", &Vself_insert_face_command,
	       doc: /* This is the command that set up `self-insert-face'.
If `last-command' does not equal this value, we ignore `self-insert-face'.  */);
  Vself_insert_face_command = Qnil;

  DEFVAR_LISP ("blink-paren-function", &Vblink_paren_function,
	       doc: /* Function called, if non-nil, whenever a close parenthesis is inserted.
More precisely, a char with closeparen syntax is self-inserted.  */);
  Vblink_paren_function = Qnil;

  defsubr (&Sforward_point);
  defsubr (&Sforward_char);
  defsubr (&Sbackward_char);
  defsubr (&Sforward_line);
  defsubr (&Sbeginning_of_line);
  defsubr (&Send_of_line);

  defsubr (&Sdelete_char);
  defsubr (&Sdelete_backward_char);

  defsubr (&Sself_insert_command);
}

void
keys_of_cmds ()
{
  int n;

  initial_define_key (global_map, Ctl ('I'), "self-insert-command");
  for (n = 040; n < 0177; n++)
    initial_define_key (global_map, n, "self-insert-command");
#ifdef MSDOS
  for (n = 0200; n < 0240; n++)
    initial_define_key (global_map, n, "self-insert-command");
#endif
  for (n = 0240; n < 0400; n++)
    initial_define_key (global_map, n, "self-insert-command");

  initial_define_key (global_map, Ctl ('A'), "beginning-of-line");
  initial_define_key (global_map, Ctl ('B'), "backward-char");
  initial_define_key (global_map, Ctl ('D'), "delete-char");
  initial_define_key (global_map, Ctl ('E'), "end-of-line");
  initial_define_key (global_map, Ctl ('F'), "forward-char");
  initial_define_key (global_map, 0177, "delete-backward-char");
}
