/* Simple built-in editing commands.
   Copyright (C) 1985, 1993 Free Software Foundation, Inc.

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
#include "commands.h"
#include "buffer.h"
#include "syntax.h"

Lisp_Object Qkill_forward_chars, Qkill_backward_chars, Vblink_paren_function;

/* A possible value for a buffer's overwrite-mode variable.  */
Lisp_Object Qoverwrite_mode_binary;


DEFUN ("forward-char", Fforward_char, Sforward_char, 0, 1, "p",
  "Move point right ARG characters (left if ARG negative).\n\
On reaching end of buffer, stop and signal error.")
  (n)
     Lisp_Object n;
{
  if (NILP (n))
    XFASTINT (n) = 1;
  else
    CHECK_NUMBER (n, 0);

  /* This used to just set point to point + XINT (n), and then check
     to see if it was within boundaries.  But now that SET_PT can
     potentially do a lot of stuff (calling entering and exiting
     hooks, etcetera), that's not a good approach.  So we validate the
     proposed position, then set point.  */
  {
    int new_point = point + XINT (n);

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
  "Move point left ARG characters (right if ARG negative).\n\
On attempt to pass beginning or end of buffer, stop and signal error.")
  (n)
     Lisp_Object n;
{
  if (NILP (n))
    XFASTINT (n) = 1;
  else
    CHECK_NUMBER (n, 0);

  XSETINT (n, - XINT (n));
  return Fforward_char (n);
}

DEFUN ("forward-line", Fforward_line, Sforward_line, 0, 1, "p",
  "Move ARG lines forward (backward if ARG is negative).\n\
Precisely, if point is on line I, move to the start of line I + ARG.\n\
If there isn't room, go as far as possible (no error).\n\
Returns the count of lines left to move.  If moving forward,\n\
that is ARG - number of lines moved; if backward, ARG + number moved.\n\
With positive ARG, a non-empty line at the end counts as one line\n\
  successfully moved (for the return value).")
  (n)
     Lisp_Object n;
{
  int pos2 = point;
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
  pos = scan_buffer ('\n', pos2, count - negp, &shortage);
  if (shortage > 0
      && (negp
	  || (ZV > BEGV
	      && pos != pos2
	      && FETCH_CHAR (pos - 1) != '\n')))
    shortage--;
  SET_PT (pos);
  return make_number (negp ? - shortage : shortage);
}

DEFUN ("beginning-of-line", Fbeginning_of_line, Sbeginning_of_line,
  0, 1, "p",
  "Move point to beginning of current line.\n\
With argument ARG not nil or 1, move forward ARG - 1 lines first.\n\
If scan reaches end of buffer, stop there without error.")
  (n)
     Lisp_Object n;
{
  if (NILP (n))
    XFASTINT (n) = 1;
  else
    CHECK_NUMBER (n, 0);

  Fforward_line (make_number (XINT (n) - 1));
  return Qnil;
}

DEFUN ("end-of-line", Fend_of_line, Send_of_line,
  0, 1, "p",
  "Move point to end of current line.\n\
With argument ARG not nil or 1, move forward ARG - 1 lines first.\n\
If scan reaches end of buffer, stop there without error.")
  (n)
     Lisp_Object n;
{
  register int pos;
  register int stop;

  if (NILP (n))
    XFASTINT (n) = 1;
  else
    CHECK_NUMBER (n, 0);

  if (XINT (n) != 1)
    Fforward_line (make_number (XINT (n) - 1));

  pos = point;
  stop = ZV;
  while (pos < stop && FETCH_CHAR (pos) != '\n') pos++;
  SET_PT (pos);

  return Qnil;
}

DEFUN ("delete-char", Fdelete_char, Sdelete_char, 1, 2, "p\nP",
  "Delete the following ARG characters (previous, with negative arg).\n\
Optional second arg KILLFLAG non-nil means kill instead (save in kill ring).\n\
Interactively, ARG is the prefix arg, and KILLFLAG is set if\n\
ARG was explicitly specified.")
  (n, killflag)
     Lisp_Object n, killflag;
{
  CHECK_NUMBER (n, 0);

  if (NILP (killflag))
    {
      if (XINT (n) < 0)
	{
	  if (point + XINT (n) < BEGV)
	    Fsignal (Qbeginning_of_buffer, Qnil);
	  else
	    del_range (point + XINT (n), point);
	}
      else
	{
	  if (point + XINT (n) > ZV)
	    Fsignal (Qend_of_buffer, Qnil);
	  else
	    del_range (point, point + XINT (n));
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
  "Delete the previous ARG characters (following, with negative ARG).\n\
Optional second arg KILLFLAG non-nil means kill instead (save in kill ring).\n\
Interactively, ARG is the prefix arg, and KILLFLAG is set if\n\
ARG was explicitly specified.")
  (n, killflag)
     Lisp_Object n, killflag;
{
  CHECK_NUMBER (n, 0);
  return Fdelete_char (make_number (-XINT (n)), killflag);
}

DEFUN ("self-insert-command", Fself_insert_command, Sself_insert_command, 1, 1, "p",
  "Insert the character you type.\n\
Whichever character you type to run this command is inserted.")
  (arg)
     Lisp_Object arg;
{
  CHECK_NUMBER (arg, 0);

  /* Barf if the key that invoked this was not a character.  */
  if (XTYPE (last_command_char) != Lisp_Int)
    bitch_at_user ();
  else
    while (XINT (arg) > 0)
      {
	XFASTINT (arg)--;	/* Ok since old and new vals both nonneg */
	internal_self_insert (XINT (last_command_char), XFASTINT (arg) != 0);
      }

  return Qnil;
}

DEFUN ("newline", Fnewline, Snewline, 0, 1, "P",
  "Insert a newline.  With arg, insert that many newlines.\n\
In Auto Fill mode, if no numeric arg, break the preceding line if it's long.")
  (arg1)
     Lisp_Object arg1;
{
  int flag;
  Lisp_Object arg;
  char c1 = '\n';

  arg = Fprefix_numeric_value (arg1);

  if (!NILP (current_buffer->read_only))
    Fbarf_if_buffer_read_only ();

  /* Inserting a newline at the end of a line produces better
     redisplay in try_window_id than inserting at the beginning of a
     line, and the textual result is the same.  So, if we're at
     beginning of line, pretend to be at the end of the previous line.  

     We can't use internal_self_insert in that case since it won't do
     the insertion correctly.  Luckily, internal_self_insert's special
     features all do nothing in that case.  */

  flag = point > BEGV && FETCH_CHAR (point - 1) == '\n';
#ifdef USE_TEXT_PROPERTIES
  /* We cannot use this optimization if properties change
     in the vicinity.
     ??? We need to check for change hook properties, etc.  */
  if (flag)
    if (! (point - 1 > BEGV && ! property_change_between_p (point - 2, point)))
      flag = 0;
#endif

  if (flag)
    SET_PT (point - 1);

  while (XINT (arg) > 0)
    {
      if (flag)
	insert (&c1, 1);
      else
	internal_self_insert ('\n', !NILP (arg1));
      XFASTINT (arg)--;		/* Ok since old and new vals both nonneg */
    }

  if (flag)
    SET_PT (point + 1);

  return Qnil;
}

/* Insert character C1.  If NOAUTOFILL is nonzero, don't do autofill
   even if it is enabled.

   If this insertion is suitable for direct output (completely simple),
   return 0.  A value of 1 indicates this *might* not have been simple.  */

internal_self_insert (c1, noautofill)
     char c1;
     int noautofill;
{
  extern Lisp_Object Fexpand_abbrev ();
  int hairy = 0;
  Lisp_Object tem;
  register enum syntaxcode synt;
  register int c = c1;
  Lisp_Object overwrite = current_buffer->overwrite_mode;

  if (!NILP (Vbefore_change_function) || !NILP (Vafter_change_function))
    hairy = 1;

  if (!NILP (overwrite)
      && point < ZV
      && (EQ (overwrite, Qoverwrite_mode_binary)
	  || (c != '\n' && FETCH_CHAR (point) != '\n'))
      && (EQ (overwrite, Qoverwrite_mode_binary)
	  || FETCH_CHAR (point) != '\t'
	  || XINT (current_buffer->tab_width) <= 0
	  || XFASTINT (current_buffer->tab_width) > 20
	  || !((current_column () + 1) % XFASTINT (current_buffer->tab_width))))
    {
      del_range (point, point + 1);
      hairy = 1;
    }
  if (!NILP (current_buffer->abbrev_mode)
      && SYNTAX (c) != Sword
      && NILP (current_buffer->read_only)
      && point > BEGV && SYNTAX (FETCH_CHAR (point - 1)) == Sword)
    {
      int modiff = MODIFF;
      Fexpand_abbrev ();
      /* We can't trust the value of Fexpand_abbrev,
	 but if Fexpand_abbrev changed the buffer,
	 assume it expanded something.  */
      if (MODIFF != modiff)
	hairy = 1;
    }
  if ((c == ' ' || c == '\n')
      && !noautofill
      && !NILP (current_buffer->auto_fill_function)
      && current_column () > XFASTINT (current_buffer->fill_column))
    {
      if (c1 != '\n')
	insert (&c1, 1);
      call0 (current_buffer->auto_fill_function);
      if (c1 == '\n')
	insert (&c1, 1);
      hairy = 1;
    }
  else
    insert (&c1, 1);
  synt = SYNTAX (c);
  if ((synt == Sclose || synt == Smath)
      && !NILP (Vblink_paren_function) && INTERACTIVE)
    {
      call0 (Vblink_paren_function);
      hairy = 1;
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

  DEFVAR_LISP ("blink-paren-function", &Vblink_paren_function,
    "Function called, if non-nil, whenever a close parenthesis is inserted.\n\
More precisely, a char with closeparen syntax is self-inserted.");
  Vblink_paren_function = Qnil;

  defsubr (&Sforward_char);
  defsubr (&Sbackward_char);
  defsubr (&Sforward_line);
  defsubr (&Sbeginning_of_line);
  defsubr (&Send_of_line);

  defsubr (&Sdelete_char);
  defsubr (&Sdelete_backward_char);

  defsubr (&Sself_insert_command);
  defsubr (&Snewline);
}

keys_of_cmds ()
{
  int n;

  initial_define_key (global_map, Ctl('M'), "newline");
  initial_define_key (global_map, Ctl('I'), "self-insert-command");
  for (n = 040; n < 0177; n++)
    initial_define_key (global_map, n, "self-insert-command");
  for (n = 0240; n < 0400; n++)
    initial_define_key (global_map, n, "self-insert-command");

  initial_define_key (global_map, Ctl ('A'), "beginning-of-line");
  initial_define_key (global_map, Ctl ('B'), "backward-char");
  initial_define_key (global_map, Ctl ('D'), "delete-char");
  initial_define_key (global_map, Ctl ('E'), "end-of-line");
  initial_define_key (global_map, Ctl ('F'), "forward-char");
  initial_define_key (global_map, 0177, "delete-backward-char");
}
