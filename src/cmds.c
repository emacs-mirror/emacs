/* Simple built-in editing commands.
   Copyright (C) 1985, 93, 94, 95, 96, 1997 Free Software Foundation, Inc.

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

Lisp_Object Qkill_forward_chars, Qkill_backward_chars, Vblink_paren_function;

/* A possible value for a buffer's overwrite-mode variable.  */
Lisp_Object Qoverwrite_mode_binary;

/* Non-nil means put this face on the next self-inserting character.  */
Lisp_Object Vself_insert_face;

/* This is the command that set up Vself_insert_face.  */
Lisp_Object Vself_insert_face_command;

/* Offset to add to a non-ASCII value when inserting it.  */
int nonascii_insert_offset;

extern Lisp_Object Qface;

/* Return buffer position which is N characters after `point'.  */
int
forward_point (n)
     int n;
{
  int pos = PT, c;

  if (!NILP (current_buffer->enable_multibyte_characters))
    {
      /* Simply adding N to `point' doesn't work because of multi-byte
	 form.  We had better not use INC_POS and DEC_POS because they
	 check the gap position every time.  But, for the moment, we
	 need working code.  */
      if (n > 0)
	{
	  while (pos < ZV && n--) INC_POS (pos);
	  if (pos < ZV) n++;
	}
      else
	{
	  while (pos > BEGV && n++) DEC_POS (pos);
	  if (pos > BEGV) n--;
	}
    }
  pos += n;

  return pos;
}

DEFUN ("forward-point", Fforward_point, Sforward_point, 1, 1, 0,
  "Return buffer position N characters after (before if N negative) point.")
  (n)
     Lisp_Object n;
{
  CHECK_NUMBER (n, 0);

  return make_number (forward_point (XINT (n)));
}

DEFUN ("forward-char", Fforward_char, Sforward_char, 0, 1, "p",
  "Move point right N characters (left if N is negative).\n\
On reaching end of buffer, stop and signal error.")
  (n)
     Lisp_Object n;
{
  if (NILP (n))
    XSETFASTINT (n, 1);
  else
    CHECK_NUMBER (n, 0);

  /* This used to just set point to point + XINT (n), and then check
     to see if it was within boundaries.  But now that SET_PT can
     potentially do a lot of stuff (calling entering and exiting
     hooks, etcetera), that's not a good approach.  So we validate the
     proposed position, then set point.  */
  {
    int new_point = forward_point (XINT (n));

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
  "Move point left N characters (right if N is negative).\n\
On attempt to pass beginning or end of buffer, stop and signal error.")
  (n)
     Lisp_Object n;
{
  if (NILP (n))
    XSETFASTINT (n, 1);
  else
    CHECK_NUMBER (n, 0);

  XSETINT (n, - XINT (n));
  return Fforward_char (n);
}

DEFUN ("forward-line", Fforward_line, Sforward_line, 0, 1, "p",
  "Move N lines forward (backward if N is negative).\n\
Precisely, if point is on line I, move to the start of line I + N.\n\
If there isn't room, go as far as possible (no error).\n\
Returns the count of lines left to move.  If moving forward,\n\
that is N - number of lines moved; if backward, N + number moved.\n\
With positive N, a non-empty line at the end counts as one line\n\
  successfully moved (for the return value).")
  (n)
     Lisp_Object n;
{
  int pos2 = PT;
  int pos;
  int count, shortage, negp;

  if (NILP (n))
    count = 1;
  else
    {
      CHECK_NUMBER (n, 0);
      count = XINT (n);
    }

  negp = count <= 0;
  pos = scan_buffer ('\n', pos2, 0, count - negp, &shortage, 1);
  if (shortage > 0
      && (negp
	  || (ZV > BEGV
	      && pos != pos2
	      && FETCH_BYTE (pos - 1) != '\n')))
    shortage--;
  SET_PT (pos);
  return make_number (negp ? - shortage : shortage);
}

DEFUN ("beginning-of-line", Fbeginning_of_line, Sbeginning_of_line,
  0, 1, "p",
  "Move point to beginning of current line.\n\
With argument N not nil or 1, move forward N - 1 lines first.\n\
If scan reaches end of buffer, stop there without error.")
  (n)
     Lisp_Object n;
{
  if (NILP (n))
    XSETFASTINT (n, 1);
  else
    CHECK_NUMBER (n, 0);

  SET_PT (XINT (Fline_beginning_position (n)));
  return Qnil;
}

DEFUN ("end-of-line", Fend_of_line, Send_of_line,
  0, 1, "p",
  "Move point to end of current line.\n\
With argument N not nil or 1, move forward N - 1 lines first.\n\
If scan reaches end of buffer, stop there without error.")
  (n)
     Lisp_Object n;
{
  register int pos;
  register int stop;

  if (NILP (n))
    XSETFASTINT (n, 1);
  else
    CHECK_NUMBER (n, 0);

  SET_PT (XINT (Fline_end_position (n)));

  return Qnil;
}

DEFUN ("delete-char", Fdelete_char, Sdelete_char, 1, 2, "p\nP",
  "Delete the following N characters (previous if N is negative).\n\
Optional second arg KILLFLAG non-nil means kill instead (save in kill ring).\n\
Interactively, N is the prefix arg, and KILLFLAG is set if\n\
N was explicitly specified.")
  (n, killflag)
     Lisp_Object n, killflag;
{
  int pos;

  CHECK_NUMBER (n, 0);

  pos = forward_point (XINT (n));
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
  "Delete the previous N characters (following if N is negative).\n\
Optional second arg KILLFLAG non-nil means kill instead (save in kill ring).\n\
Interactively, N is the prefix arg, and KILLFLAG is set if\n\
N was explicitly specified.")
  (n, killflag)
     Lisp_Object n, killflag;
{
  Lisp_Object value;
  int deleted_special = 0;
  int pos, i;

  CHECK_NUMBER (n, 0);

  /* See if we are about to delete a tab or newline backwards.  */
  pos = PT;
  for (i = 0; i < XINT (n) && pos > BEGV; i++)
    {
      int c;

      DEC_POS (pos);
      c = FETCH_BYTE (pos);
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
      && ! (PT == ZV || FETCH_BYTE (PT) == '\n'))
    {
      int column = current_column ();

      value = Fdelete_char (make_number (-XINT (n)), killflag);
      i = column - current_column ();
      Finsert_char (make_number (' '), make_number (i), Qnil);
      SET_PT (PT - i);
    }
  else
    value = Fdelete_char (make_number (-XINT (n)), killflag);

  return value;
}

DEFUN ("self-insert-command", Fself_insert_command, Sself_insert_command, 1, 1, "p",
  "Insert the character you type.\n\
Whichever character you type to run this command is inserted.")
  (n)
     Lisp_Object n;
{
  int character = XINT (last_command_char);

  CHECK_NUMBER (n, 0);

  /* Barf if the key that invoked this was not a character.  */
  if (!INTEGERP (last_command_char))
    bitch_at_user ();
  else if (XINT (n) >= 2 && NILP (current_buffer->overwrite_mode))
    {
      int modified_char = character;
      /* Add the offset to the character, for Finsert_char.
	 We pass internal_self_insert the unmodified character
	 because it itself does this offsetting.  */
      if (modified_char >= 0200 && modified_char <= 0377
	  && ! NILP (current_buffer->enable_multibyte_characters))
	modified_char += nonascii_insert_offset;

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
  unsigned char workbuf[4], *str;
  int number_to_delete = 0;
  int spaces_to_insert = 0;

  if (c >= 0200 && c <= 0377
      && ! NILP (current_buffer->enable_multibyte_characters))
    c += nonascii_insert_offset;

  overwrite = current_buffer->overwrite_mode;
  if (!NILP (Vbefore_change_function) || !NILP (Vafter_change_function)
      || !NILP (Vbefore_change_functions) || !NILP (Vafter_change_functions))
    hairy = 1;

  /* At first, get multi-byte form of C in STR.  */
  if (!NILP (current_buffer->enable_multibyte_characters))
    len = CHAR_STRING (c, workbuf, str);
  else
    workbuf[0] = c, str = workbuf, len = 1;

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

      /* A code at `point'.  Since this is checked only against
         NEWLINE and TAB, we don't need a character code but only the
         first byte of multi-byte form.  */
      unsigned char c2 = FETCH_BYTE (PT);
      /* A column the cursor should be placed at after this insertion.
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
		    && ((NILP (current_buffer->enable_multibyte_characters)
			 ? (target_clm = current_column () + 1)
			 : (target_clm = current_column () + WIDTH_BY_CHAR_HEAD (str[0]))),
			target_clm % XFASTINT (current_buffer->tab_width)))))
	{
	  int pos = PT;

	  if (target_clm == 0)
	    number_to_delete = forward_point (1) - PT;
	  else
	    {
	      /* The actual cursor position after the trial of moving
		 to column TARGET_CLM.  It is greater than TARGET_CLM
		 if the TARGET_CLM is middle of multi-column
		 character.  In that case, the new point is set after
		 that character.  */
	      int actual_clm
		= XFASTINT (Fmove_to_column (make_number (target_clm), Qnil));

	      number_to_delete = PT - pos;

	      if (actual_clm > target_clm)
		{
		  /* We will delete too many columns.  Let's fill columns
		     by spaces so that the remaining text won't move.  */
		  spaces_to_insert = actual_clm - target_clm;
		}
	    }
	  SET_PT (pos);
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

      /* If we expanded an abbrev which has only a hook,
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

  if (number_to_delete)
    {
      string = make_string (str, len);
      if (spaces_to_insert)
	{
	  tem = Fmake_string (make_number (spaces_to_insert),
			      make_number (' '));
	  string = concat2 (tem, string);
	}

      replace_range (PT, PT + number_to_delete, string, 1, 1);
      SET_PT (PT + XSTRING (string)->size);
    }
  else
    insert_and_inherit (str, len);

  if ((c == ' ' || c == '\n')
      && !noautofill
      && !NILP (current_buffer->auto_fill_function))
    {
      Lisp_Object tem;

      if (c == '\n')
	/* After inserting a newline, move to previous line and fill */
	/* that.  Must have the newline in place already so filling and */
	/* justification, if any, know where the end is going to be. */
	SET_PT (PT - 1);
      tem = call0 (current_buffer->auto_fill_function);
      if (c == '\n')
	SET_PT (PT + 1);
      if (!NILP (tem))
	hairy = 2;
    }

#ifdef HAVE_FACES
  /* If previous command specified a face to use, use it.  */
  if (!NILP (Vself_insert_face)
      && EQ (current_kboard->Vlast_command, Vself_insert_face_command))
    {
      Lisp_Object before, after;
      XSETINT (before, PT - len);
      XSETINT (after, PT);
      Fput_text_property (before, after, Qface, Vself_insert_face, Qnil);
      Vself_insert_face = Qnil;
    }
#endif

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

syms_of_cmds ()
{
  Qkill_backward_chars = intern ("kill-backward-chars");
  staticpro (&Qkill_backward_chars);

  Qkill_forward_chars = intern ("kill-forward-chars");
  staticpro (&Qkill_forward_chars);

  Qoverwrite_mode_binary = intern ("overwrite-mode-binary");
  staticpro (&Qoverwrite_mode_binary);

  DEFVAR_LISP ("self-insert-face", &Vself_insert_face,
    "If non-nil, set the face of the next self-inserting character to this.\n\
See also `self-insert-face-command'.");
  Vself_insert_face = Qnil;

  DEFVAR_LISP ("self-insert-face-command", &Vself_insert_face_command,
    "This is the command that set up `self-insert-face'.\n\
If `last-command' does not equal this value, we ignore `self-insert-face'.");
  Vself_insert_face_command = Qnil;

  DEFVAR_LISP ("blink-paren-function", &Vblink_paren_function,
    "Function called, if non-nil, whenever a close parenthesis is inserted.\n\
More precisely, a char with closeparen syntax is self-inserted.");
  Vblink_paren_function = Qnil;

  DEFVAR_INT ("nonascii-insert-offset", &nonascii_insert_offset,
    "Offset to add to a non-ascii code 0200...0377 when inserting it.\n\
This applies only when multibyte characters are enabled, and it serves\n\
to convert a Latin-1 or similar 8-bit character code to the corresponding\n\
Emacs character code.");
  nonascii_insert_offset = 0;

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
