/* Call a Lisp function interactively.
   Copyright (C) 1985, 1986, 1993, 1994, 1995 Free Software Foundation, Inc.

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
#include "buffer.h"
#include "commands.h"
#include "keyboard.h"
#include "window.h"
#include "mocklisp.h"

extern char *index ();

Lisp_Object Qminus, Qplus;
Lisp_Object Qcall_interactively;
Lisp_Object Vcommand_history;

Lisp_Object Vcommand_debug_status, Qcommand_debug_status;
Lisp_Object Qenable_recursive_minibuffers;

/* Non-nil means treat the mark as active
   even if mark_active is 0.  */
Lisp_Object Vmark_even_if_inactive;

Lisp_Object Vmouse_leave_buffer_hook, Qmouse_leave_buffer_hook;

Lisp_Object Qlist;
static Lisp_Object preserved_fns;

/* Marker used within call-interactively to refer to point.  */
static Lisp_Object point_marker;

/* This comment supplies the doc string for interactive,
   for make-docfile to see.  We cannot put this in the real DEFUN
   due to limits in the Unix cpp.

DEFUN ("interactive", Ffoo, Sfoo, 0, 0, 0,
 "Specify a way of parsing arguments for interactive use of a function.\n\
For example, write\n\
  (defun foo (arg) \"Doc string\" (interactive \"p\") ...use arg...)\n\
to make ARG be the prefix argument when `foo' is called as a command.\n\
The \"call\" to `interactive' is actually a declaration rather than a function;\n\
 it tells `call-interactively' how to read arguments\n\
 to pass to the function.\n\
When actually called, `interactive' just returns nil.\n\
\n\
The argument of `interactive' is usually a string containing a code letter\n\
 followed by a prompt.  (Some code letters do not use I/O to get\n\
 the argument and do not need prompts.)  To prompt for multiple arguments,\n\
 give a code letter, its prompt, a newline, and another code letter, etc.\n\
 Prompts are passed to format, and may use % escapes to print the\n\
 arguments that have already been read.\n\
If the argument is not a string, it is evaluated to get a list of\n\
 arguments to pass to the function.\n\
Just `(interactive)' means pass no args when calling interactively.\n\
\nCode letters available are:\n\
a -- Function name: symbol with a function definition.\n\
b -- Name of existing buffer.\n\
B -- Name of buffer, possibly nonexistent.\n\
c -- Character.\n\
C -- Command name: symbol with interactive function definition.\n\
d -- Value of point as number.  Does not do I/O.\n\
D -- Directory name.\n\
e -- Parametrized event (i.e., one that's a list) that invoked this command.\n\
     If used more than once, the Nth `e' returns the Nth parameterized event.\n\
     This skips events that are integers or symbols.\n\
f -- Existing file name.\n\
F -- Possibly nonexistent file name.\n\
k -- Key sequence (downcase the last event if needed to get a definition).\n\
K -- Key sequence to be redefined (do not downcase the last event).\n\
m -- Value of mark as number.  Does not do I/O.\n\
n -- Number read using minibuffer.\n\
N -- Raw prefix arg, or if none, do like code `n'.\n\
p -- Prefix arg converted to number.  Does not do I/O.\n\
P -- Prefix arg in raw form.  Does not do I/O.\n\
r -- Region: point and mark as 2 numeric args, smallest first.  Does no I/O.\n\
s -- Any string.\n\
S -- Any symbol.\n\
v -- Variable name: symbol that is user-variable-p.\n\
x -- Lisp expression read but not evaluated.\n\
X -- Lisp expression read and evaluated.\n\
In addition, if the string begins with `*'\n\
 then an error is signaled if the buffer is read-only.\n\
 This happens before reading any arguments.\n\
If the string begins with `@', then Emacs searches the key sequence\n\
 which invoked the command for its first mouse click (or any other\n\
 event which specifies a window), and selects that window before\n\
 reading any arguments.  You may use both `@' and `*'; they are\n\
 processed in the order that they appear." */

/* ARGSUSED */
DEFUN ("interactive", Finteractive, Sinteractive, 0, UNEVALLED, 0,
  0 /* See immediately above */)
  (args)
     Lisp_Object args;
{
  return Qnil;
}

/* Quotify EXP: if EXP is constant, return it.
   If EXP is not constant, return (quote EXP).  */
Lisp_Object
quotify_arg (exp)
     register Lisp_Object exp;
{
  if (!INTEGERP (exp) && !STRINGP (exp)
      && !NILP (exp) && !EQ (exp, Qt))
    return Fcons (Qquote, Fcons (exp, Qnil));

  return exp;
}

/* Modify EXP by quotifying each element (except the first).  */
Lisp_Object
quotify_args (exp)
     Lisp_Object exp;
{
  register Lisp_Object tail;
  register struct Lisp_Cons *ptr;
  for (tail = exp; CONSP (tail); tail = ptr->cdr)
    {
      ptr = XCONS (tail);
      ptr->car = quotify_arg (ptr->car);
    }
  return exp;
}

char *callint_argfuns[]
    = {"", "point", "mark", "region-beginning", "region-end"};

static void
check_mark ()
{
  Lisp_Object tem;
  tem = Fmarker_buffer (current_buffer->mark);
  if (NILP (tem) || (XBUFFER (tem) != current_buffer))
    error ("The mark is not set now");
  if (!NILP (Vtransient_mark_mode) && NILP (Vmark_even_if_inactive)
      && NILP (current_buffer->mark_active))
    Fsignal (Qmark_inactive, Qnil);
}


DEFUN ("call-interactively", Fcall_interactively, Scall_interactively, 1, 2, 0,
  "Call FUNCTION, reading args according to its interactive calling specs.\n\
The function contains a specification of how to do the argument reading.\n\
In the case of user-defined functions, this is specified by placing a call\n\
to the function `interactive' at the top level of the function body.\n\
See `interactive'.\n\
\n\
Optional second arg RECORD-FLAG non-nil\n\
means unconditionally put this command in the command-history.\n\
Otherwise, this is done only if an arg is read using the minibuffer.")
  (function, record)
     Lisp_Object function, record;
{
  Lisp_Object *args, *visargs;
  unsigned char **argstrings;
  Lisp_Object fun;
  Lisp_Object funcar;
  Lisp_Object specs;
  Lisp_Object teml;
  Lisp_Object enable;
  int speccount = specpdl_ptr - specpdl;

  /* The index of the next element of this_command_keys to examine for
     the 'e' interactive code.  */
  int next_event;

  Lisp_Object prefix_arg;
  unsigned char *string;
  unsigned char *tem;

  /* If varies[i] > 0, the i'th argument shouldn't just have its value
     in this call quoted in the command history.  It should be
     recorded as a call to the function named callint_argfuns[varies[i]].  */
  int *varies;

  register int i, j;
  int count, foo;
  char prompt[100];
  char prompt1[100];
  char *tem1;
  int arg_from_tty = 0;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  /* Save this now, since use of minibuffer will clobber it. */
  prefix_arg = current_perdisplay->Vcurrent_prefix_arg;

 retry:

  if (SYMBOLP (function))
    enable = Fget (function, Qenable_recursive_minibuffers);

  fun = indirect_function (function);

  specs = Qnil;
  string = 0;

  /* Decode the kind of function.  Either handle it and return,
     or go to `lose' if not interactive, or go to `retry'
     to specify a different function, or set either STRING or SPECS.  */

  if (SUBRP (fun))
    {
      string = (unsigned char *) XSUBR (fun)->prompt;
      if (!string)
	{
	lose:
	  function = wrong_type_argument (Qcommandp, function);
	  goto retry;
	}
      if ((EMACS_INT) string == 1)
	/* Let SPECS (which is nil) be used as the args.  */
	string = 0;
    }
  else if (COMPILEDP (fun))
    {
      if ((XVECTOR (fun)->size & PSEUDOVECTOR_SIZE_MASK) <= COMPILED_INTERACTIVE)
	goto lose;
      specs = XVECTOR (fun)->contents[COMPILED_INTERACTIVE];
    }
  else if (!CONSP (fun))
    goto lose;
  else if (funcar = Fcar (fun), EQ (funcar, Qautoload))
    {
      GCPRO2 (function, prefix_arg);
      do_autoload (fun, function);
      UNGCPRO;
      goto retry;
    }
  else if (EQ (funcar, Qlambda))
    {
      specs = Fassq (Qinteractive, Fcdr (Fcdr (fun)));
      if (NILP (specs))
	goto lose;
      specs = Fcar (Fcdr (specs));
    }
  else if (EQ (funcar, Qmocklisp))
    {
#ifdef MULTI_PERDISPLAY
      display_locked = 1;
#endif
      return ml_apply (fun, Qinteractive);
    }
  else
    goto lose;

  /* If either specs or string is set to a string, use it.  */
  if (STRINGP (specs))
    {
      /* Make a copy of string so that if a GC relocates specs,
	 `string' will still be valid.  */
      string = (unsigned char *) alloca (XSTRING (specs)->size + 1);
      bcopy (XSTRING (specs)->data, string, XSTRING (specs)->size + 1);
    }
  else if (string == 0)
    {
      Lisp_Object input;
      i = num_input_chars;
      input = specs;
      /* Compute the arg values using the user's expression.  */
      specs = Feval (specs);
      if (i != num_input_chars || !NILP (record))
	{
	  /* We should record this command on the command history.  */
	  Lisp_Object values, car;
	  /* Make a copy of the list of values, for the command history,
	     and turn them into things we can eval.  */
	  values = quotify_args (Fcopy_sequence (specs));
	  /* If the list of args was produced with an explicit call to `list',
	     look for elements that were computed with (region-beginning)
	     or (region-end), and put those expressions into VALUES
	     instead of the present values.  */
	  car = Fcar (input);
	  if (EQ (car, Qlist))
	    {
	      Lisp_Object intail, valtail;
	      for (intail = Fcdr (input), valtail = values;
		   CONSP (valtail);
		   intail = Fcdr (intail), valtail = Fcdr (valtail))
		{
		  Lisp_Object elt;
		  elt = Fcar (intail);
		  if (CONSP (elt))
		    {
		      Lisp_Object presflag;
		      presflag = Fmemq (Fcar (elt), preserved_fns);
		      if (!NILP (presflag))
			Fsetcar (valtail, Fcar (intail));
		    }
		}
	    }
	  Vcommand_history
	    = Fcons (Fcons (function, values), Vcommand_history);
	}
#ifdef MULTI_PERDISPLAY
      display_locked = 1;
#endif
      return apply1 (function, specs);
    }

  /* Here if function specifies a string to control parsing the defaults */

  /* Set next_event to point to the first event with parameters.  */
  for (next_event = 0; next_event < this_command_key_count; next_event++)
    if (EVENT_HAS_PARAMETERS
	(XVECTOR (this_command_keys)->contents[next_event]))
      break;
  
  /* Handle special starting chars `*' and `@'.  Also `-'.  */
  while (1)
    {
      if (*string == '*')
	{
	  string++;
	  if (!NILP (current_buffer->read_only))
	    Fbarf_if_buffer_read_only ();
	}
      /* Ignore this for semi-compatibility with Lucid.  */
      else if (*string == '-')
	string++;
      else if (*string == '@')
	{
	  Lisp_Object event;

	  event = XVECTOR (this_command_keys)->contents[next_event];
	  if (EVENT_HAS_PARAMETERS (event)
	      && (event = XCONS (event)->car, CONSP (event))
	      && (event = XCONS (event)->car, CONSP (event))
	      && (event = XCONS (event)->car), WINDOWP (event))
	    {
	      if (MINI_WINDOW_P (XWINDOW (event))
		  && ! (minibuf_level > 0 && EQ (event, minibuf_window)))
		error ("Attempt to select inactive minibuffer window");

	      /* If the current buffer wants to clean up, let it.  */
	      if (!NILP (Vmouse_leave_buffer_hook))
		call1 (Vrun_hooks, Qmouse_leave_buffer_hook);

	      Fselect_window (event);
	    }
	  string++;
	}
      else break;
    }

  /* Count the number of arguments the interactive spec would have
     us give to the function.  */
  tem = string;
  for (j = 0; *tem; j++)
    {
      /* 'r' specifications ("point and mark as 2 numeric args")
	 produce *two* arguments.  */
      if (*tem == 'r') j++;
      tem = (unsigned char *) index (tem, '\n');
      if (tem)
	tem++;
      else
	tem = (unsigned char *) "";
    }
  count = j;

  args = (Lisp_Object *) alloca ((count + 1) * sizeof (Lisp_Object));
  visargs = (Lisp_Object *) alloca ((count + 1) * sizeof (Lisp_Object));
  argstrings = (unsigned char **) alloca ((count + 1) * sizeof (char *));
  varies = (int *) alloca ((count + 1) * sizeof (int));

  for (i = 0; i < (count + 1); i++)
    {
      args[i] = Qnil;
      visargs[i] = Qnil;
      varies[i] = 0;
    }

  GCPRO4 (prefix_arg, function, *args, *visargs);
  gcpro3.nvars = (count + 1);
  gcpro4.nvars = (count + 1);

  if (!NILP (enable))
    specbind (Qenable_recursive_minibuffers, Qt);

  tem = string;
  for (i = 1; *tem; i++)
    {
      strncpy (prompt1, tem + 1, sizeof prompt1 - 1);
      prompt1[sizeof prompt1 - 1] = 0;
      tem1 = index (prompt1, '\n');
      if (tem1) *tem1 = 0;
      /* Fill argstrings with a vector of C strings
	 corresponding to the Lisp strings in visargs.  */
      for (j = 1; j < i; j++)
	argstrings[j]
	  = EQ (visargs[j], Qnil)
	    ? (unsigned char *) ""
	      : XSTRING (visargs[j])->data;

      doprnt (prompt, sizeof prompt, prompt1, 0, j - 1, argstrings + 1);

      switch (*tem)
	{
	case 'a':		/* Symbol defined as a function */
	  visargs[i] = Fcompleting_read (build_string (prompt),
					 Vobarray, Qfboundp, Qt, Qnil, Qnil);
	  /* Passing args[i] directly stimulates compiler bug */
	  teml = visargs[i];
	  args[i] = Fintern (teml, Qnil);
	  break;

	case 'b':   		/* Name of existing buffer */
	  args[i] = Fcurrent_buffer ();
	  if (EQ (selected_window, minibuf_window))
	    args[i] = Fother_buffer (args[i], Qnil);
	  args[i] = Fread_buffer (build_string (prompt), args[i], Qt);
	  break;

	case 'B':		/* Name of buffer, possibly nonexistent */
	  args[i] = Fread_buffer (build_string (prompt),
				  Fother_buffer (Fcurrent_buffer (), Qnil),
				  Qnil);
	  break;

        case 'c':		/* Character */
	  message1 (prompt);
	  args[i] = Fread_char ();
	  /* Passing args[i] directly stimulates compiler bug */
	  teml = args[i];
	  visargs[i] = Fchar_to_string (teml);
	  break;

	case 'C':		/* Command: symbol with interactive function */
	  visargs[i] = Fcompleting_read (build_string (prompt),
					 Vobarray, Qcommandp, Qt, Qnil, Qnil);
	  /* Passing args[i] directly stimulates compiler bug */
	  teml = visargs[i];
	  args[i] = Fintern (teml, Qnil);
	  break;

	case 'd':		/* Value of point.  Does not do I/O.  */
	  Fset_marker (point_marker, make_number (PT), Qnil);
	  args[i] = point_marker;
	  /* visargs[i] = Qnil; */
	  varies[i] = 1;
	  break;

	case 'D':		/* Directory name. */
	  args[i] = Fread_file_name (build_string (prompt), Qnil,
				     current_buffer->directory, Qlambda, Qnil);
	  break;

	case 'f':		/* Existing file name. */
	  args[i] = Fread_file_name (build_string (prompt),
				     Qnil, Qnil, Qlambda, Qnil);
	  break;

	case 'F':		/* Possibly nonexistent file name. */
	  args[i] = Fread_file_name (build_string (prompt),
				     Qnil, Qnil, Qnil, Qnil);
	  break;

	case 'k':		/* Key sequence. */
	  args[i] = Fread_key_sequence (build_string (prompt), Qnil, Qnil);
	  teml = args[i];
	  visargs[i] = Fkey_description (teml);
	  break;

	case 'K':		/* Key sequence to be defined. */
	  args[i] = Fread_key_sequence (build_string (prompt), Qnil, Qt);
	  teml = args[i];
	  visargs[i] = Fkey_description (teml);
	  break;

	case 'e':		/* The invoking event.  */
	  if (next_event >= this_command_key_count)
	    error ("%s must be bound to an event with parameters",
		   (SYMBOLP (function)
		    ? (char *) XSYMBOL (function)->name->data
		    : "command"));
	  args[i] = XVECTOR (this_command_keys)->contents[next_event++];
	  varies[i] = -1;

	  /* Find the next parameterized event.  */
	  while (next_event < this_command_key_count
		 && ! (EVENT_HAS_PARAMETERS
		       (XVECTOR (this_command_keys)->contents[next_event])))
	    next_event++;

	  break;

	case 'm':		/* Value of mark.  Does not do I/O.  */
	  check_mark ();
	  /* visargs[i] = Qnil; */
	  args[i] = current_buffer->mark;
	  varies[i] = 2;
	  break;

	case 'N':		/* Prefix arg, else number from minibuffer */
	  if (!NILP (prefix_arg))
	    goto have_prefix_arg;
	case 'n':		/* Read number from minibuffer.  */
	  do
	    args[i] = Fread_minibuffer (build_string (prompt), Qnil);
	  while (! NUMBERP (args[i]));
	  visargs[i] = last_minibuf_string;
	  break;

	case 'P':		/* Prefix arg in raw form.  Does no I/O.  */
	have_prefix_arg:
	  args[i] = prefix_arg;
	  /* visargs[i] = Qnil; */
	  varies[i] = -1;
	  break;

	case 'p':		/* Prefix arg converted to number.  No I/O. */
	  args[i] = Fprefix_numeric_value (prefix_arg);
	  /* visargs[i] = Qnil; */
	  varies[i] = -1;
	  break;

	case 'r':		/* Region, point and mark as 2 args. */
	  check_mark ();
	  Fset_marker (point_marker, make_number (PT), Qnil);
	  /* visargs[i+1] = Qnil; */
	  foo = marker_position (current_buffer->mark);
	  /* visargs[i] = Qnil; */
	  args[i] = point < foo ? point_marker : current_buffer->mark;
	  varies[i] = 3;
	  args[++i] = point > foo ? point_marker : current_buffer->mark;
	  varies[i] = 4;
	  break;

	case 's':		/* String read via minibuffer.  */
	  args[i] = Fread_string (build_string (prompt), Qnil, Qnil);
	  break;

	case 'S':		/* Any symbol.  */
	  visargs[i] = Fread_string (build_string (prompt), Qnil, Qnil);
	  /* Passing args[i] directly stimulates compiler bug */
	  teml = visargs[i];
	  args[i] = Fintern (teml, Qnil);
	  break;

	case 'v':		/* Variable name: symbol that is
				   user-variable-p. */
	  args[i] = Fread_variable (build_string (prompt));
	  visargs[i] = last_minibuf_string;
	  break;

	case 'x':		/* Lisp expression read but not evaluated */
	  args[i] = Fread_minibuffer (build_string (prompt), Qnil);
	  visargs[i] = last_minibuf_string;
	  break;

	case 'X':		/* Lisp expression read and evaluated */
	  args[i] = Feval_minibuffer (build_string (prompt), Qnil);
	  visargs[i] = last_minibuf_string;
 	  break;

	default:
	  error ("Invalid control letter \"%c\" (%03o) in interactive calling string",
		 *tem, *tem);
	}

      if (varies[i] == 0)
	arg_from_tty = 1;

      if (NILP (visargs[i]) && STRINGP (args[i]))
	visargs[i] = args[i];

      tem = (unsigned char *) index (tem, '\n');
      if (tem) tem++;
      else tem = (unsigned char *) "";
    }
  unbind_to (speccount, Qnil);

  QUIT;

  args[0] = function;

  if (arg_from_tty || !NILP (record))
    {
      visargs[0] = function;
      for (i = 1; i < count + 1; i++)
	{
	  if (varies[i] > 0)
	    visargs[i] = Fcons (intern (callint_argfuns[varies[i]]), Qnil);
	  else
	    visargs[i] = quotify_arg (args[i]);
	}
      Vcommand_history = Fcons (Flist (count + 1, visargs),
				Vcommand_history);
    }

  /* If we used a marker to hold point, mark, or an end of the region,
     temporarily, convert it to an integer now.  */
  for (i = 0; i++; i < count)
    if (varies[i] >= 1 && varies[i] <= 4)
      XSETINT (args[i], marker_position (args[i]));

#ifdef MULTI_PERDISPLAY
  display_locked = 1;
#endif

  {
    Lisp_Object val;
    specbind (Qcommand_debug_status, Qnil);

    val = Ffuncall (count + 1, args);
    UNGCPRO;
    return unbind_to (speccount, val);
  }
}  

DEFUN ("prefix-numeric-value", Fprefix_numeric_value, Sprefix_numeric_value,
  1, 1, 0,
  "Return numeric meaning of raw prefix argument ARG.\n\
A raw prefix argument is what you get from `(interactive \"P\")'.\n\
Its numeric meaning is what you would get from `(interactive \"p\")'.")
  (raw)
     Lisp_Object raw;
{
  Lisp_Object val;
  
  if (NILP (raw))
    XSETFASTINT (val, 1);
  else if (EQ (raw, Qminus))
    XSETINT (val, -1);
  else if (CONSP (raw))
    XSETINT (val, XINT (XCONS (raw)->car));
  else if (INTEGERP (raw))
    val = raw;
  else
    XSETFASTINT (val, 1);

  return val;
}

syms_of_callint ()
{
  point_marker = Fmake_marker ();
  staticpro (&point_marker);

  preserved_fns = Fcons (intern ("region-beginning"),
			 Fcons (intern ("region-end"),
				Fcons (intern ("point"),
				       Fcons (intern ("mark"), Qnil))));
  staticpro (&preserved_fns);

  Qlist = intern ("list");
  staticpro (&Qlist);

  Qminus = intern ("-");
  staticpro (&Qminus);

  Qplus = intern ("+");
  staticpro (&Qplus);

  Qcall_interactively = intern ("call-interactively");
  staticpro (&Qcall_interactively);

  Qcommand_debug_status = intern ("command-debug-status");
  staticpro (&Qcommand_debug_status);

  Qenable_recursive_minibuffers = intern ("enable-recursive-minibuffers");
  staticpro (&Qenable_recursive_minibuffers);

  Qmouse_leave_buffer_hook = intern ("mouse-leave-buffer-hook");
  staticpro (&Qmouse_leave_buffer_hook);

  DEFVAR_LISP ("command-history", &Vcommand_history,
    "List of recent commands that read arguments from terminal.\n\
Each command is represented as a form to evaluate.");
  Vcommand_history = Qnil;

  DEFVAR_LISP ("command-debug-status", &Vcommand_debug_status,
    "Debugging status of current interactive command.\n\
Bound each time `call-interactively' is called;\n\
may be set by the debugger as a reminder for itself.");
  Vcommand_debug_status = Qnil;

  DEFVAR_LISP ("mark-even-if-inactive", &Vmark_even_if_inactive,
    "*Non-nil means you can use the mark even when inactive.\n\
This option makes a difference in Transient Mark mode.\n\
When the option is non-nil, deactivation of the mark\n\
turns off region highlighting, but commands that use the mark\n\
behave as if the mark were still active.");
  Vmark_even_if_inactive = Qnil;

  DEFVAR_LISP ("mouse-leave-buffer-hook", &Vmouse_leave_buffer_hook,
    "Hook to run when about to switch windows with a mouse command.\n\
Its purpose is to give temporary modes such as Isearch mode\n\
a way to turn themselves off when a mouse command switches windows.");
  Vmouse_leave_buffer_hook = Qnil;

  defsubr (&Sinteractive);
  defsubr (&Scall_interactively);
  defsubr (&Sprefix_numeric_value);
}
