/* Keyboard macros.
   Copyright (C) 1985, 1986, 1993, 2000, 2001 Free Software Foundation, Inc.

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
#include "macros.h"
#include "commands.h"
#include "buffer.h"
#include "window.h"
#include "keyboard.h"
#include "keymap.h"

Lisp_Object Qexecute_kbd_macro, Qkbd_macro_termination_hook;

/* Kbd macro currently being executed (a string or vector).  */

Lisp_Object Vexecuting_macro;

/* Index of next character to fetch from that macro.  */

int executing_macro_index;

/* Number of successful iterations so far
   for innermost keyboard macro.
   This is not bound at each level,
   so after an error, it describes the innermost interrupted macro.  */

int executing_macro_iterations;

/* This is the macro that was executing.
   This is not bound at each level,
   so after an error, it describes the innermost interrupted macro.
   We use it only as a kind of flag, so no need to protect it.  */

Lisp_Object executing_macro;

extern Lisp_Object real_this_command;

Lisp_Object Fexecute_kbd_macro ();

DEFUN ("start-kbd-macro", Fstart_kbd_macro, Sstart_kbd_macro, 1, 1, "P",
  "Record subsequent keyboard input, defining a keyboard macro.\n\
The commands are recorded even as they are executed.\n\
Use \\[end-kbd-macro] to finish recording and make the macro available.\n\
Use \\[name-last-kbd-macro] to give it a permanent name.\n\
Non-nil arg (prefix arg) means append to last macro defined;\n\
this begins by re-executing that macro as if you typed it again.")
  (append)
     Lisp_Object append;
{
  if (!NILP (current_kboard->defining_kbd_macro))
    error ("Already defining kbd macro");

  if (!current_kboard->kbd_macro_buffer)
    {
      current_kboard->kbd_macro_bufsize = 30;
      current_kboard->kbd_macro_buffer
	= (Lisp_Object *)xmalloc (30 * sizeof (Lisp_Object));
    }
  update_mode_lines++;
  if (NILP (append))
    {
      if (current_kboard->kbd_macro_bufsize > 200)
	{
	  current_kboard->kbd_macro_bufsize = 30;
	  current_kboard->kbd_macro_buffer
	    = (Lisp_Object *)xrealloc (current_kboard->kbd_macro_buffer,
				       30 * sizeof (Lisp_Object));
	}
      current_kboard->kbd_macro_ptr = current_kboard->kbd_macro_buffer;
      current_kboard->kbd_macro_end = current_kboard->kbd_macro_buffer;
      message ("Defining kbd macro...");
    }
  else
    {
      int i, len;

      /* Check the type of last-kbd-macro in case Lisp code changed it.  */
      if (!STRINGP (current_kboard->Vlast_kbd_macro)
	  && !VECTORP (current_kboard->Vlast_kbd_macro))
	current_kboard->Vlast_kbd_macro
	  = wrong_type_argument (Qarrayp, current_kboard->Vlast_kbd_macro);

      len = XINT (Flength (current_kboard->Vlast_kbd_macro));

      /* Copy last-kbd-macro into the buffer, in case the Lisp code
	 has put another macro there.  */
      if (current_kboard->kbd_macro_bufsize < len + 30)
	{
	  current_kboard->kbd_macro_bufsize = len + 30;
	  current_kboard->kbd_macro_buffer
	    = (Lisp_Object *)xrealloc (current_kboard->kbd_macro_buffer,
				       (len + 30) * sizeof (Lisp_Object));
	}
      for (i = 0; i < len; i++)
	current_kboard->kbd_macro_buffer[i]
	  = Faref (current_kboard->Vlast_kbd_macro, make_number (i));

      current_kboard->kbd_macro_ptr = current_kboard->kbd_macro_buffer + len;
      current_kboard->kbd_macro_end = current_kboard->kbd_macro_ptr;

      /* Re-execute the macro we are appending to,
	 for consistency of behavior.  */
      Fexecute_kbd_macro (current_kboard->Vlast_kbd_macro,
			  make_number (1));

      message ("Appending to kbd macro...");
    }
  current_kboard->defining_kbd_macro = Qt;
  
  return Qnil;
}

DEFUN ("end-kbd-macro", Fend_kbd_macro, Send_kbd_macro, 0, 1, "p",
  "Finish defining a keyboard macro.\n\
The definition was started by \\[start-kbd-macro].\n\
The macro is now available for use via \\[call-last-kbd-macro],\n\
or it can be given a name with \\[name-last-kbd-macro] and then invoked\n\
under that name.\n\
\n\
With numeric arg, repeat macro now that many times,\n\
counting the definition just completed as the first repetition.\n\
An argument of zero means repeat until error.")
  (repeat)
     Lisp_Object repeat;
{
  if (NILP (current_kboard->defining_kbd_macro))
    error ("Not defining kbd macro");

  if (NILP (repeat))
    XSETFASTINT (repeat, 1);
  else
    CHECK_NUMBER (repeat, 0);

  if (!NILP (current_kboard->defining_kbd_macro))
    {
      current_kboard->defining_kbd_macro = Qnil;
      update_mode_lines++;
      current_kboard->Vlast_kbd_macro
	= make_event_array ((current_kboard->kbd_macro_end
			     - current_kboard->kbd_macro_buffer),
			    current_kboard->kbd_macro_buffer);
      message ("Keyboard macro defined");
    }

  if (XFASTINT (repeat) == 0)
    Fexecute_kbd_macro (current_kboard->Vlast_kbd_macro, repeat);
  else
    {
      XSETINT (repeat, XINT (repeat)-1);
      if (XINT (repeat) > 0)
	Fexecute_kbd_macro (current_kboard->Vlast_kbd_macro, repeat);
    }
  return Qnil;
}

/* Store character c into kbd macro being defined */

void
store_kbd_macro_char (c)
     Lisp_Object c;
{
  struct kboard *kb = current_kboard;

  if (!NILP (kb->defining_kbd_macro))
    {
      if (kb->kbd_macro_ptr - kb->kbd_macro_buffer == kb->kbd_macro_bufsize)
	{
	  int ptr_offset, end_offset, nbytes;
	  
	  ptr_offset = kb->kbd_macro_ptr - kb->kbd_macro_buffer;
	  end_offset = kb->kbd_macro_end - kb->kbd_macro_buffer;
	  kb->kbd_macro_bufsize *= 2;
	  nbytes = kb->kbd_macro_bufsize * sizeof *kb->kbd_macro_buffer;
	  kb->kbd_macro_buffer
	    = (Lisp_Object *) xrealloc (kb->kbd_macro_buffer, nbytes);
	  kb->kbd_macro_ptr = kb->kbd_macro_buffer + ptr_offset;
	  kb->kbd_macro_end = kb->kbd_macro_buffer + end_offset;
	}
      
      *kb->kbd_macro_ptr++ = c;
    }
}

/* Declare that all chars stored so far in the kbd macro being defined
 really belong to it.  This is done in between editor commands.  */

void
finalize_kbd_macro_chars ()
{
  current_kboard->kbd_macro_end = current_kboard->kbd_macro_ptr;
}

DEFUN ("cancel-kbd-macro-events", Fcancel_kbd_macro_events,
       Scancel_kbd_macro_events, 0, 0, 0,
  "Cancel the events added to a keyboard macro for this command.")
  ()
{
  current_kboard->kbd_macro_ptr = current_kboard->kbd_macro_end;
  return Qnil;
}

DEFUN ("store-kbd-macro-event", Fstore_kbd_macro_event,
       Sstore_kbd_macro_event, 1, 1, 0,
  "Store EVENT into the keyboard macro being defined.")
  (event)
     Lisp_Object event;
{
  store_kbd_macro_char (event);
  return Qnil;
}

DEFUN ("call-last-kbd-macro", Fcall_last_kbd_macro, Scall_last_kbd_macro,
  0, 1, "p",
  "Call the last keyboard macro that you defined with \\[start-kbd-macro].\n\
\n\
A prefix argument serves as a repeat count.  Zero means repeat until error.\n\
\n\
To make a macro permanent so you can call it even after\n\
defining others, use \\[name-last-kbd-macro].")
  (prefix)
     Lisp_Object prefix;
{
  /* Don't interfere with recognition of the previous command
     from before this macro started.  */
  Vthis_command = current_kboard->Vlast_command;
  /* C-x z after the macro should repeat the macro.  */
  real_this_command = current_kboard->Vlast_kbd_macro;

  if (! NILP (current_kboard->defining_kbd_macro))
    error ("Can't execute anonymous macro while defining one");
  else if (NILP (current_kboard->Vlast_kbd_macro))
    error ("No kbd macro has been defined");
  else
    Fexecute_kbd_macro (current_kboard->Vlast_kbd_macro, prefix);

  /* command_loop_1 sets this to nil before it returns;
     get back the last command within the macro
     so that it can be last, again, after we return.  */
  Vthis_command = current_kboard->Vlast_command;

  return Qnil;
}

/* Restore Vexecuting_macro and executing_macro_index - called when
   the unwind-protect in Fexecute_kbd_macro gets invoked.  */

static Lisp_Object
pop_kbd_macro (info)
     Lisp_Object info;
{
  Lisp_Object tem;
  Vexecuting_macro = XCAR (info);
  tem = XCDR (info);
  executing_macro_index = XINT (XCAR (tem));
  real_this_command = XCDR (tem);
  Frun_hooks (1, &Qkbd_macro_termination_hook);
  return Qnil;
}

DEFUN ("execute-kbd-macro", Fexecute_kbd_macro, Sexecute_kbd_macro, 1, 2, 0,
  "Execute MACRO as string of editor command characters.\n\
If MACRO is a symbol, its function definition is used.\n\
COUNT is a repeat count, or nil for once, or 0 for infinite loop.")
  (macro, count)
     Lisp_Object macro, count;
{
  Lisp_Object final;
  Lisp_Object tem;
  int pdlcount = specpdl_ptr - specpdl;
  int repeat = 1;
  struct gcpro gcpro1;
  int success_count = 0;

  executing_macro_iterations = 0;

  if (!NILP (count))
    {
      count = Fprefix_numeric_value (count);
      repeat = XINT (count);
    }

  final = indirect_function (macro);
  if (!STRINGP (final) && !VECTORP (final))
    error ("Keyboard macros must be strings or vectors");

  tem = Fcons (Vexecuting_macro,
	       Fcons (make_number (executing_macro_index),
		      real_this_command));
  record_unwind_protect (pop_kbd_macro, tem);

  GCPRO1 (final);
  do
    {
      Vexecuting_macro = final;
      executing_macro = final;
      executing_macro_index = 0;

      current_kboard->Vprefix_arg = Qnil;
      command_loop_1 ();

      executing_macro_iterations = ++success_count;

      QUIT;
    }
  while (--repeat
	 && (STRINGP (Vexecuting_macro) || VECTORP (Vexecuting_macro)));

  executing_macro = Qnil;

  real_this_command = Vexecuting_macro;

  UNGCPRO;
  return unbind_to (pdlcount, Qnil);
}

void
init_macros ()
{
  Vexecuting_macro = Qnil;
  executing_macro = Qnil;
}

void
syms_of_macros ()
{
  Qexecute_kbd_macro = intern ("execute-kbd-macro");
  staticpro (&Qexecute_kbd_macro);
  Qkbd_macro_termination_hook = intern ("kbd-macro-termination-hook");
  staticpro (&Qkbd_macro_termination_hook);

  defsubr (&Sstart_kbd_macro);
  defsubr (&Send_kbd_macro);
  defsubr (&Scall_last_kbd_macro);
  defsubr (&Sexecute_kbd_macro);
  defsubr (&Scancel_kbd_macro_events);
  defsubr (&Sstore_kbd_macro_event);

  DEFVAR_KBOARD ("defining-kbd-macro", defining_kbd_macro,
    "Non-nil while a keyboard macro is being defined.  Don't set this!");

  DEFVAR_LISP ("executing-macro", &Vexecuting_macro,
    "Currently executing keyboard macro (string or vector); nil if none executing.");

  DEFVAR_LISP_NOPRO ("executing-kbd-macro", &Vexecuting_macro,
    "Currently executing keyboard macro (string or vector); nil if none executing.");

  DEFVAR_KBOARD ("last-kbd-macro", Vlast_kbd_macro,
    "Last kbd macro defined, as a string or vector; nil if none defined.");
}
