/* Keyboard macros.
   Copyright (C) 1985, 1986, 1993 Free Software Foundation, Inc.

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
#include "macros.h"
#include "commands.h"
#include "buffer.h"
#include "window.h"
#include "keyboard.h"

Lisp_Object Qexecute_kbd_macro;

Lisp_Object Vexecuting_macro;
int executing_macro_index;

Lisp_Object Fexecute_kbd_macro ();

DEFUN ("start-kbd-macro", Fstart_kbd_macro, Sstart_kbd_macro, 1, 1, "P",
  "Record subsequent keyboard input, defining a keyboard macro.\n\
The commands are recorded even as they are executed.\n\
Use \\[end-kbd-macro] to finish recording and make the macro available.\n\
Use \\[name-last-kbd-macro] to give it a permanent name.\n\
Non-nil arg (prefix arg) means append to last macro defined;\n\
 This begins by re-executing that macro as if you typed it again.")
  (append)
     Lisp_Object append;
{
  if (!NILP (current_kboard->defining_kbd_macro))
    error ("Already defining kbd macro");

  if (!current_kboard->kbd_macro_buffer)
    {
      current_kboard->kbd_macro_bufsize = 30;
      current_kboard->kbd_macro_buffer
	= (Lisp_Object *)malloc (30 * sizeof (Lisp_Object));
    }
  update_mode_lines++;
  if (NILP (append))
    {
      current_kboard->kbd_macro_ptr = current_kboard->kbd_macro_buffer;
      current_kboard->kbd_macro_end = current_kboard->kbd_macro_buffer;
      message("Defining kbd macro...");
    }
  else
    {
      message("Appending to kbd macro...");
      current_kboard->kbd_macro_ptr = current_kboard->kbd_macro_end;
      Fexecute_kbd_macro (current_kboard->Vlast_kbd_macro,
			  make_number (1));
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
  (arg)
     Lisp_Object arg;
{
  if (NILP (current_kboard->defining_kbd_macro))
    error ("Not defining kbd macro.");

  if (NILP (arg))
    XSETFASTINT (arg, 1);
  else
    CHECK_NUMBER (arg, 0);

  if (!NILP (current_kboard->defining_kbd_macro))
    {
      current_kboard->defining_kbd_macro = Qnil;
      update_mode_lines++;
      current_kboard->Vlast_kbd_macro
	= make_event_array ((current_kboard->kbd_macro_end
			     - current_kboard->kbd_macro_buffer),
			    current_kboard->kbd_macro_buffer);
      message("Keyboard macro defined");
    }

  if (XFASTINT (arg) == 0)
    Fexecute_kbd_macro (current_kboard->Vlast_kbd_macro, arg);
  else
    {
      XSETINT (arg, XINT (arg)-1);
      if (XINT (arg) > 0)
	Fexecute_kbd_macro (current_kboard->Vlast_kbd_macro, arg);
    }
  return Qnil;
}

/* Store character c into kbd macro being defined */

store_kbd_macro_char (c)
     Lisp_Object c;
{
  if (!NILP (current_kboard->defining_kbd_macro))
    {
      if ((current_kboard->kbd_macro_ptr
	   - current_kboard->kbd_macro_buffer)
	  == current_kboard->kbd_macro_bufsize)
	{
	  register Lisp_Object *new;
	  current_kboard->kbd_macro_bufsize *= 2;
	  new = (Lisp_Object *)xrealloc (current_kboard->kbd_macro_buffer,
					 (current_kboard->kbd_macro_bufsize
					  * sizeof (Lisp_Object)));
	  current_kboard->kbd_macro_ptr
	    += new - current_kboard->kbd_macro_buffer;
	  current_kboard->kbd_macro_end
	    += new - current_kboard->kbd_macro_buffer;
	  current_kboard->kbd_macro_buffer = new;
	}
      *current_kboard->kbd_macro_ptr++ = c;
    }
}

/* Declare that all chars stored so far in the kbd macro being defined
 really belong to it.  This is done in between editor commands.  */

finalize_kbd_macro_chars ()
{
  current_kboard->kbd_macro_end = current_kboard->kbd_macro_ptr;
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
  if (! NILP (current_kboard->defining_kbd_macro))
    error ("Can't execute anonymous macro while defining one");
  else if (NILP (current_kboard->Vlast_kbd_macro))
    error ("No kbd macro has been defined");
  else
    Fexecute_kbd_macro (current_kboard->Vlast_kbd_macro, prefix);
  return Qnil;
}

/* Restore Vexecuting_macro and executing_macro_index - called when
   the unwind-protect in Fexecute_kbd_macro gets invoked.  */
static Lisp_Object
pop_kbd_macro (info)
     Lisp_Object info;
{
  Lisp_Object tem;
  Vexecuting_macro = Fcar (info);
  tem = Fcdr (info);
  executing_macro_index = XINT (tem);
  return Qnil;
}

DEFUN ("execute-kbd-macro", Fexecute_kbd_macro, Sexecute_kbd_macro, 1, 2, 0,
  "Execute MACRO as string of editor command characters.\n\
If MACRO is a symbol, its function definition is used.\n\
COUNT is a repeat count, or nil for once, or 0 for infinite loop.")
  (macro, prefixarg)
     Lisp_Object macro, prefixarg;
{
  Lisp_Object final;
  Lisp_Object tem;
  int count = specpdl_ptr - specpdl;
  int repeat = 1;
  struct gcpro gcpro1;

  if (!NILP (prefixarg))
    prefixarg = Fprefix_numeric_value (prefixarg),
    repeat = XINT (prefixarg);

  final = indirect_function (macro);
  if (!STRINGP (final) && !VECTORP (final))
    error ("Keyboard macros must be strings or vectors.");

  XSETFASTINT (tem, executing_macro_index);
  tem = Fcons (Vexecuting_macro, tem);
  record_unwind_protect (pop_kbd_macro, tem);

  GCPRO1 (final);
  do
    {
      Vexecuting_macro = final;
      executing_macro_index = 0;

      current_kboard->Vprefix_arg = Qnil;
      command_loop_1 ();

      QUIT;
    }
  while (--repeat
	 && (STRINGP (Vexecuting_macro) || VECTORP (Vexecuting_macro)));

  UNGCPRO;
  return unbind_to (count, Qnil);
}

init_macros ()
{
  Vexecuting_macro = Qnil;
}

syms_of_macros ()
{
  Qexecute_kbd_macro = intern ("execute-kbd-macro");
  staticpro (&Qexecute_kbd_macro);

  defsubr (&Sstart_kbd_macro);
  defsubr (&Send_kbd_macro);
  defsubr (&Scall_last_kbd_macro);
  defsubr (&Sexecute_kbd_macro);

  DEFVAR_KBOARD ("defining-kbd-macro", defining_kbd_macro,
    "Non-nil while a keyboard macro is being defined.  Don't set this!");

  DEFVAR_LISP ("executing-macro", &Vexecuting_macro,
    "Currently executing keyboard macro (string or vector); nil if none executing.");

  DEFVAR_LISP_NOPRO ("executing-kbd-macro", &Vexecuting_macro,
    "Currently executing keyboard macro (string or vector); nil if none executing.");

  DEFVAR_KBOARD ("last-kbd-macro", Vlast_kbd_macro,
    "Last kbd macro defined, as a string or vector; nil if none defined.");
}

keys_of_macros ()
{
  initial_define_key (control_x_map, ('e'), "call-last-kbd-macro");
  initial_define_key (control_x_map, ('('), "start-kbd-macro");
  initial_define_key (control_x_map, (')'), "end-kbd-macro");
}
