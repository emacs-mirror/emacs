/* Call a Lisp function interactively.
   Copyright (C) 1985, 1986, 1993, 1994, 1995, 1997, 2000, 2001, 2002,
                 2003, 2004, 2005, 2006, 2007, 2008
                 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */


#include <config.h>

#include "lisp.h"
#include "buffer.h"
#include "commands.h"
#include "keyboard.h"
#include "window.h"
#include "keymap.h"

#ifdef HAVE_INDEX
extern char *index P_ ((const char *, int));
#endif

extern Lisp_Object Qcursor_in_echo_area;
extern Lisp_Object Qfile_directory_p;

Lisp_Object Vcurrent_prefix_arg, Qminus, Qplus;
Lisp_Object Qcall_interactively;
Lisp_Object Vcommand_history;

extern Lisp_Object Vhistory_length;
extern Lisp_Object Vthis_original_command, real_this_command;

Lisp_Object Vcommand_debug_status, Qcommand_debug_status;
Lisp_Object Qenable_recursive_minibuffers;

/* Non-nil means treat the mark as active
   even if mark_active is 0.  */
Lisp_Object Vmark_even_if_inactive;

Lisp_Object Vmouse_leave_buffer_hook, Qmouse_leave_buffer_hook;

Lisp_Object Qlist, Qlet, Qletx, Qsave_excursion, Qprogn, Qif, Qwhen;
static Lisp_Object preserved_fns;

/* Marker used within call-interactively to refer to point.  */
static Lisp_Object point_marker;

/* String for the prompt text used in Fcall_interactively.  */
static Lisp_Object callint_message;

/* ARGSUSED */
DEFUN ("interactive", Finteractive, Sinteractive, 0, UNEVALLED, 0,
       doc: /* Specify a way of parsing arguments for interactive use of a function.
For example, write
  (defun foo (arg) "Doc string" (interactive "p") ...use arg...)
to make ARG be the prefix argument when `foo' is called as a command.
The "call" to `interactive' is actually a declaration rather than a function;
 it tells `call-interactively' how to read arguments
 to pass to the function.
When actually called, `interactive' just returns nil.

The argument of `interactive' is usually a string containing a code letter
 followed by a prompt.  (Some code letters do not use I/O to get
 the argument and do not need prompts.)  To prompt for multiple arguments,
 give a code letter, its prompt, a newline, and another code letter, etc.
 Prompts are passed to format, and may use % escapes to print the
 arguments that have already been read.
If the argument is not a string, it is evaluated to get a list of
 arguments to pass to the function.
Just `(interactive)' means pass no args when calling interactively.

Code letters available are:
a -- Function name: symbol with a function definition.
b -- Name of existing buffer.
B -- Name of buffer, possibly nonexistent.
c -- Character (no input method is used).
C -- Command name: symbol with interactive function definition.
d -- Value of point as number.  Does not do I/O.
D -- Directory name.
e -- Parametrized event (i.e., one that's a list) that invoked this command.
     If used more than once, the Nth `e' returns the Nth parameterized event.
     This skips events that are integers or symbols.
f -- Existing file name.
F -- Possibly nonexistent file name.
G -- Possibly nonexistent file name, defaulting to just directory name.
i -- Ignored, i.e. always nil.  Does not do I/O.
k -- Key sequence (downcase the last event if needed to get a definition).
K -- Key sequence to be redefined (do not downcase the last event).
m -- Value of mark as number.  Does not do I/O.
M -- Any string.  Inherits the current input method.
n -- Number read using minibuffer.
N -- Numeric prefix arg, or if none, do like code `n'.
p -- Prefix arg converted to number.  Does not do I/O.
P -- Prefix arg in raw form.  Does not do I/O.
r -- Region: point and mark as 2 numeric args, smallest first.  Does no I/O.
s -- Any string.  Does not inherit the current input method.
S -- Any symbol.
U -- Mouse up event discarded by a previous k or K argument.
v -- Variable name: symbol that is user-variable-p.
x -- Lisp expression read but not evaluated.
X -- Lisp expression read and evaluated.
z -- Coding system.
Z -- Coding system, nil if no prefix arg.
In addition, if the string begins with `*'
 then an error is signaled if the buffer is read-only.
 This happens before reading any arguments.
If the string begins with `@', then Emacs searches the key sequence
 which invoked the command for its first mouse click (or any other
 event which specifies a window), and selects that window before
 reading any arguments.  You may use both `@' and `*'; they are
 processed in the order that they appear.
usage: (interactive ARGS)  */)
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
  Lisp_Object next;
  for (tail = exp; CONSP (tail); tail = next)
    {
      next = XCDR (tail);
      XSETCAR (tail, quotify_arg (XCAR (tail)));
    }
  return exp;
}

char *callint_argfuns[]
    = {"", "point", "mark", "region-beginning", "region-end"};

static void
check_mark (for_region)
     int for_region;
{
  Lisp_Object tem;
  tem = Fmarker_buffer (current_buffer->mark);
  if (NILP (tem) || (XBUFFER (tem) != current_buffer))
    error (for_region ? "The mark is not set now, so there is no region"
	   : "The mark is not set now");
  if (!NILP (Vtransient_mark_mode) && NILP (Vmark_even_if_inactive)
      && NILP (current_buffer->mark_active))
    xsignal0 (Qmark_inactive);
}

/* If the list of args INPUT was produced with an explicit call to
   `list', look for elements that were computed with
   (region-beginning) or (region-end), and put those expressions into
   VALUES instead of the present values.

   This function doesn't return a value because it modifies elements
   of VALUES to do its job.  */

static void
fix_command (input, values)
     Lisp_Object input, values;
{
  if (CONSP (input))
    {
      Lisp_Object car;

      car = XCAR (input);
      /* Skip through certain special forms.  */
      while (EQ (car, Qlet) || EQ (car, Qletx)
	     || EQ (car, Qsave_excursion)
	     || EQ (car, Qprogn))
	{
	  while (CONSP (XCDR (input)))
	    input = XCDR (input);
	  input = XCAR (input);
	  if (!CONSP (input))
	    break;
	  car = XCAR (input);
	}
      if (EQ (car, Qlist))
	{
	  Lisp_Object intail, valtail;
	  for (intail = Fcdr (input), valtail = values;
	       CONSP (valtail);
	       intail = Fcdr (intail), valtail = XCDR (valtail))
	    {
	      Lisp_Object elt;
	      elt = Fcar (intail);
	      if (CONSP (elt))
		{
		  Lisp_Object presflag, carelt;
		  carelt = Fcar (elt);
		  /* If it is (if X Y), look at Y.  */
		  if (EQ (carelt, Qif)
		      && EQ (Fnthcdr (make_number (3), elt), Qnil))
		    elt = Fnth (make_number (2), elt);
		  /* If it is (when ... Y), look at Y.  */
		  else if (EQ (carelt, Qwhen))
		    {
		      while (CONSP (XCDR (elt)))
			elt = XCDR (elt);
		      elt = Fcar (elt);
		    }

		  /* If the function call we're looking at
		     is a special preserved one, copy the
		     whole expression for this argument.  */
		  if (CONSP (elt))
		    {
		      presflag = Fmemq (Fcar (elt), preserved_fns);
		      if (!NILP (presflag))
			Fsetcar (valtail, Fcar (intail));
		    }
		}
	    }
	}
    }
}

DEFUN ("call-interactively", Fcall_interactively, Scall_interactively, 1, 3, 0,
       doc: /* Call FUNCTION, reading args according to its interactive calling specs.
Return the value FUNCTION returns.
The function contains a specification of how to do the argument reading.
In the case of user-defined functions, this is specified by placing a call
to the function `interactive' at the top level of the function body.
See `interactive'.

Optional second arg RECORD-FLAG non-nil
means unconditionally put this command in the command-history.
Otherwise, this is done only if an arg is read using the minibuffer.

Optional third arg KEYS, if given, specifies the sequence of events to
supply, as a vector, if the command inquires which events were used to
invoke it.  If KEYS is omitted or nil, the return value of
`this-command-keys-vector' is used.  */)
     (function, record_flag, keys)
     Lisp_Object function, record_flag, keys;
{
  Lisp_Object *args, *visargs;
  Lisp_Object specs;
  Lisp_Object filter_specs;
  Lisp_Object teml;
  Lisp_Object up_event;
  Lisp_Object enable;
  int speccount = SPECPDL_INDEX ();

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
  char prompt1[100];
  char *tem1;
  int arg_from_tty = 0;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;
  int key_count;
  int record_then_fail = 0;

  Lisp_Object save_this_command, save_last_command;
  Lisp_Object save_this_original_command, save_real_this_command;

  save_this_command = Vthis_command;
  save_this_original_command = Vthis_original_command;
  save_real_this_command = real_this_command;
  save_last_command = current_kboard->Vlast_command;

  if (NILP (keys))
    keys = this_command_keys, key_count = this_command_key_count;
  else
    {
      CHECK_VECTOR (keys);
      key_count = XVECTOR (keys)->size;
    }

  /* Save this now, since use of minibuffer will clobber it. */
  prefix_arg = Vcurrent_prefix_arg;

  if (SYMBOLP (function))
    enable = Fget (function, Qenable_recursive_minibuffers);
  else
    enable = Qnil;

  specs = Qnil;
  string = 0;
  /* The idea of FILTER_SPECS is to provide away to
     specify how to represent the arguments in command history.
     The feature is not fully implemented.  */
  filter_specs = Qnil;

  /* If k or K discard an up-event, save it here so it can be retrieved with U */
  up_event = Qnil;

  /* Set SPECS to the interactive form, or barf if not interactive.  */
  {
    Lisp_Object form;
    GCPRO2 (function, prefix_arg);
    form = Finteractive_form (function);
    UNGCPRO;
    if (CONSP (form))
      specs = filter_specs = Fcar (XCDR (form));
    else
      wrong_type_argument (Qcommandp, function);
  }

  /* If SPECS is set to a string, use it as an interactive prompt.  */
  if (STRINGP (specs))
    {
      /* Make a copy of string so that if a GC relocates specs,
	 `string' will still be valid.  */
      string = (unsigned char *) alloca (SBYTES (specs) + 1);
      bcopy (SDATA (specs), string,
	     SBYTES (specs) + 1);
    }
  else
    {
      Lisp_Object input;
      i = num_input_events;
      input = specs;
      /* Compute the arg values using the user's expression.  */
      GCPRO2 (input, filter_specs);
      specs = Feval (specs);
      UNGCPRO;
      if (i != num_input_events || !NILP (record_flag))
	{
	  /* We should record this command on the command history.  */
	  Lisp_Object values;
	  /* Make a copy of the list of values, for the command history,
	     and turn them into things we can eval.  */
	  values = quotify_args (Fcopy_sequence (specs));
	  fix_command (input, values);
	  Vcommand_history
	    = Fcons (Fcons (function, values), Vcommand_history);

	  /* Don't keep command history around forever.  */
	  if (INTEGERP (Vhistory_length) && XINT (Vhistory_length) > 0)
	    {
	      teml = Fnthcdr (Vhistory_length, Vcommand_history);
	      if (CONSP (teml))
		XSETCDR (teml, Qnil);
	    }
	}

      Vthis_command = save_this_command;
      Vthis_original_command = save_this_original_command;
      real_this_command= save_real_this_command;
      current_kboard->Vlast_command = save_last_command;

      temporarily_switch_to_single_kboard (NULL);
      return unbind_to (speccount, apply1 (function, specs));
    }

  /* Here if function specifies a string to control parsing the defaults */

  /* Set next_event to point to the first event with parameters.  */
  for (next_event = 0; next_event < key_count; next_event++)
    if (EVENT_HAS_PARAMETERS (XVECTOR (keys)->contents[next_event]))
      break;

  /* Handle special starting chars `*' and `@'.  Also `-'.  */
  /* Note that `+' is reserved for user extensions.  */
  while (1)
    {
      if (*string == '+')
	error ("`+' is not used in `interactive' for ordinary commands");
      else if (*string == '*')
	{
	  string++;
	  if (!NILP (current_buffer->read_only))
	    {
	      if (!NILP (record_flag))
		{
		  unsigned char *p = string;
		  while (*p)
		    {
		      if (! (*p == 'r' || *p == 'p' || *p == 'P'
			     || *p == '\n'))
			Fbarf_if_buffer_read_only ();
		      p++;
		    }
		  record_then_fail = 1;
		}
	      else
		Fbarf_if_buffer_read_only ();
	    }
	}
      /* Ignore this for semi-compatibility with Lucid.  */
      else if (*string == '-')
	string++;
      else if (*string == '@')
	{
	  Lisp_Object event, tem;

	  event = (next_event < key_count
		   ? XVECTOR (keys)->contents[next_event]
		   : Qnil);
	  if (EVENT_HAS_PARAMETERS (event)
	      && (tem = XCDR (event), CONSP (tem))
	      && (tem = XCAR (tem), CONSP (tem))
	      && (tem = XCAR (tem), WINDOWP (tem)))
	    {
	      if (MINI_WINDOW_P (XWINDOW (tem))
		  && ! (minibuf_level > 0 && EQ (tem, minibuf_window)))
		error ("Attempt to select inactive minibuffer window");

	      /* If the current buffer wants to clean up, let it.  */
	      if (!NILP (Vmouse_leave_buffer_hook))
		call1 (Vrun_hooks, Qmouse_leave_buffer_hook);

	      Fselect_window (tem, Qnil);
	    }
	  string++;
	}
      else break;
    }

  /* Count the number of arguments the interactive spec would have
     us give to the function.  */
  tem = string;
  for (j = 0; *tem;)
    {
      /* 'r' specifications ("point and mark as 2 numeric args")
	 produce *two* arguments.  */
      if (*tem == 'r')
	j += 2;
      else
	j++;
      tem = (unsigned char *) index (tem, '\n');
      if (tem)
	++tem;
      else
	break;
    }
  count = j;

  args = (Lisp_Object *) alloca ((count + 1) * sizeof (Lisp_Object));
  visargs = (Lisp_Object *) alloca ((count + 1) * sizeof (Lisp_Object));
  varies = (int *) alloca ((count + 1) * sizeof (int));

  for (i = 0; i < (count + 1); i++)
    {
      args[i] = Qnil;
      visargs[i] = Qnil;
      varies[i] = 0;
    }

  GCPRO5 (prefix_arg, function, *args, *visargs, up_event);
  gcpro3.nvars = (count + 1);
  gcpro4.nvars = (count + 1);

  if (!NILP (enable))
    specbind (Qenable_recursive_minibuffers, Qt);

  tem = string;
  for (i = 1; *tem; i++)
    {
      strncpy (prompt1, tem + 1, sizeof prompt1 - 1);
      prompt1[sizeof prompt1 - 1] = 0;
      tem1 = (char *) index (prompt1, '\n');
      if (tem1) *tem1 = 0;

      visargs[0] = build_string (prompt1);
      if (index (prompt1, '%'))
	callint_message = Fformat (i, visargs);
      else
	callint_message = visargs[0];

      switch (*tem)
	{
	case 'a':		/* Symbol defined as a function */
	  visargs[i] = Fcompleting_read (callint_message,
					 Vobarray, Qfboundp, Qt,
					 Qnil, Qnil, Qnil, Qnil);
	  /* Passing args[i] directly stimulates compiler bug */
	  teml = visargs[i];
	  args[i] = Fintern (teml, Qnil);
	  break;

	case 'b':   		/* Name of existing buffer */
	  args[i] = Fcurrent_buffer ();
	  if (EQ (selected_window, minibuf_window))
	    args[i] = Fother_buffer (args[i], Qnil, Qnil);
	  args[i] = Fread_buffer (callint_message, args[i], Qt);
	  break;

	case 'B':		/* Name of buffer, possibly nonexistent */
	  args[i] = Fread_buffer (callint_message,
				  Fother_buffer (Fcurrent_buffer (), Qnil, Qnil),
				  Qnil);
	  break;

        case 'c':		/* Character */
	  args[i] = Fread_char (callint_message, Qnil, Qnil);
	  message1_nolog ((char *) 0);
	  /* Passing args[i] directly stimulates compiler bug */
	  teml = args[i];
	  visargs[i] = Fchar_to_string (teml);
	  break;

	case 'C':		/* Command: symbol with interactive function */
	  visargs[i] = Fcompleting_read (callint_message,
					 Vobarray, Qcommandp,
					 Qt, Qnil, Qnil, Qnil, Qnil);
	  /* Passing args[i] directly stimulates compiler bug */
	  teml = visargs[i];
	  args[i] = Fintern (teml, Qnil);
	  break;

	case 'd':		/* Value of point.  Does not do I/O.  */
	  set_marker_both (point_marker, Qnil, PT, PT_BYTE);
	  args[i] = point_marker;
	  /* visargs[i] = Qnil; */
	  varies[i] = 1;
	  break;

	case 'D':		/* Directory name. */
	  args[i] = Fread_file_name (callint_message, Qnil,
				     current_buffer->directory, Qlambda, Qnil,
				     Qfile_directory_p);
	  break;

	case 'f':		/* Existing file name. */
	  args[i] = Fread_file_name (callint_message,
				     Qnil, Qnil, Qlambda, Qnil, Qnil);
	  break;

	case 'F':		/* Possibly nonexistent file name. */
	  args[i] = Fread_file_name (callint_message,
				     Qnil, Qnil, Qnil, Qnil, Qnil);
	  break;

	case 'G':		/* Possibly nonexistent file name,
				   default to directory alone. */
	  args[i] = Fread_file_name (callint_message,
				     Qnil, Qnil, Qnil, empty_unibyte_string, Qnil);
	  break;

	case 'i':		/* Ignore an argument -- Does not do I/O */
	  varies[i] = -1;
	  break;

	case 'k':		/* Key sequence. */
	  {
	    int speccount1 = SPECPDL_INDEX ();
	    specbind (Qcursor_in_echo_area, Qt);
	    args[i] = Fread_key_sequence (callint_message,
					  Qnil, Qnil, Qnil, Qnil);
	    unbind_to (speccount1, Qnil);
	    teml = args[i];
	    visargs[i] = Fkey_description (teml, Qnil);

	    /* If the key sequence ends with a down-event,
	       discard the following up-event.  */
	    teml = Faref (args[i], make_number (XINT (Flength (args[i])) - 1));
	    if (CONSP (teml))
	      teml = XCAR (teml);
	    if (SYMBOLP (teml))
	      {
		Lisp_Object tem2;

		teml = Fget (teml, intern ("event-symbol-elements"));
		/* Ignore first element, which is the base key.  */
		tem2 = Fmemq (intern ("down"), Fcdr (teml));
		if (! NILP (tem2))
		  up_event = Fread_event (Qnil, Qnil, Qnil);
	      }
	  }
	  break;

	case 'K':		/* Key sequence to be defined. */
	  {
	    int speccount1 = SPECPDL_INDEX ();
	    specbind (Qcursor_in_echo_area, Qt);
	    args[i] = Fread_key_sequence (callint_message,
					  Qnil, Qt, Qnil, Qnil);
	    teml = args[i];
	    visargs[i] = Fkey_description (teml, Qnil);
	    unbind_to (speccount1, Qnil);

	    /* If the key sequence ends with a down-event,
	       discard the following up-event.  */
	    teml = Faref (args[i], make_number (XINT (Flength (args[i])) - 1));
	    if (CONSP (teml))
	      teml = XCAR (teml);
	    if (SYMBOLP (teml))
	      {
		Lisp_Object tem2;

		teml = Fget (teml, intern ("event-symbol-elements"));
		/* Ignore first element, which is the base key.  */
		tem2 = Fmemq (intern ("down"), Fcdr (teml));
		if (! NILP (tem2))
		  up_event = Fread_event (Qnil, Qnil, Qnil);
	      }
	  }
	  break;

	case 'U':		/* Up event from last k or K */
	  if (!NILP (up_event))
	    {
	      args[i] = Fmake_vector (make_number (1), up_event);
	      up_event = Qnil;
	      teml = args[i];
	      visargs[i] = Fkey_description (teml, Qnil);
	    }
	  break;

	case 'e':		/* The invoking event.  */
	  if (next_event >= key_count)
	    error ("%s must be bound to an event with parameters",
		   (SYMBOLP (function)
		    ? (char *) SDATA (SYMBOL_NAME (function))
		    : "command"));
	  args[i] = XVECTOR (keys)->contents[next_event++];
	  varies[i] = -1;

	  /* Find the next parameterized event.  */
	  while (next_event < key_count
		 && ! (EVENT_HAS_PARAMETERS
		       (XVECTOR (keys)->contents[next_event])))
	    next_event++;

	  break;

	case 'm':		/* Value of mark.  Does not do I/O.  */
	  check_mark (0);
	  /* visargs[i] = Qnil; */
	  args[i] = current_buffer->mark;
	  varies[i] = 2;
	  break;

	case 'M':		/* String read via minibuffer with
				   inheriting the current input method.  */
	  args[i] = Fread_string (callint_message,
				  Qnil, Qnil, Qnil, Qt);
	  break;

	case 'N':		/* Prefix arg as number, else number from minibuffer */
	  if (!NILP (prefix_arg))
	    goto have_prefix_arg;
	case 'n':		/* Read number from minibuffer.  */
	  {
	    int first = 1;
	    do
	      {
		Lisp_Object tem;
		if (! first)
		  {
		    message ("Please enter a number.");
		    sit_for (make_number (1), 0, 0);
		  }
		first = 0;

		tem = Fread_from_minibuffer (callint_message,
					     Qnil, Qnil, Qnil, Qnil, Qnil,
					     Qnil);
		if (! STRINGP (tem) || SCHARS (tem) == 0)
		  args[i] = Qnil;
		else
		  args[i] = Fread (tem);
	      }
	    while (! NUMBERP (args[i]));
	  }
	  visargs[i] = args[i];
	  break;

	case 'P':		/* Prefix arg in raw form.  Does no I/O.  */
	  args[i] = prefix_arg;
	  /* visargs[i] = Qnil; */
	  varies[i] = -1;
	  break;

	case 'p':		/* Prefix arg converted to number.  No I/O. */
	have_prefix_arg:
	  args[i] = Fprefix_numeric_value (prefix_arg);
	  /* visargs[i] = Qnil; */
	  varies[i] = -1;
	  break;

	case 'r':		/* Region, point and mark as 2 args. */
	  check_mark (1);
	  set_marker_both (point_marker, Qnil, PT, PT_BYTE);
	  /* visargs[i+1] = Qnil; */
	  foo = marker_position (current_buffer->mark);
	  /* visargs[i] = Qnil; */
	  args[i] = PT < foo ? point_marker : current_buffer->mark;
	  varies[i] = 3;
	  args[++i] = PT > foo ? point_marker : current_buffer->mark;
	  varies[i] = 4;
	  break;

	case 's':		/* String read via minibuffer without
				   inheriting the current input method.  */
	  args[i] = Fread_string (callint_message,
				  Qnil, Qnil, Qnil, Qnil);
	  break;

	case 'S':		/* Any symbol.  */
	  visargs[i] = Fread_string (callint_message,
				     Qnil, Qnil, Qnil, Qnil);
	  /* Passing args[i] directly stimulates compiler bug */
	  teml = visargs[i];
	  args[i] = Fintern (teml, Qnil);
	  break;

	case 'v':		/* Variable name: symbol that is
				   user-variable-p. */
	  args[i] = Fread_variable (callint_message, Qnil);
	  visargs[i] = last_minibuf_string;
	  break;

	case 'x':		/* Lisp expression read but not evaluated */
	  args[i] = Fread_minibuffer (callint_message, Qnil);
	  visargs[i] = last_minibuf_string;
	  break;

	case 'X':		/* Lisp expression read and evaluated */
	  args[i] = Feval_minibuffer (callint_message, Qnil);
	  visargs[i] = last_minibuf_string;
 	  break;

	case 'Z':		/* Coding-system symbol, or ignore the
				   argument if no prefix */
	  if (NILP (prefix_arg))
	    {
	      args[i] = Qnil;
	      varies[i] = -1;
	    }
	  else
	    {
	      args[i]
		= Fread_non_nil_coding_system (callint_message);
	      visargs[i] = last_minibuf_string;
	    }
	  break;

	case 'z':		/* Coding-system symbol or nil */
	  args[i] = Fread_coding_system (callint_message, Qnil);
	  visargs[i] = last_minibuf_string;
	  break;

	  /* We have a case for `+' so we get an error
	     if anyone tries to define one here.  */
	case '+':
	default:
	  error ("Invalid control letter `%c' (%03o) in interactive calling string",
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

  if (arg_from_tty || !NILP (record_flag))
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
      /* Don't keep command history around forever.  */
      if (INTEGERP (Vhistory_length) && XINT (Vhistory_length) > 0)
	{
	  teml = Fnthcdr (Vhistory_length, Vcommand_history);
	  if (CONSP (teml))
	    XSETCDR (teml, Qnil);
	}
    }

  /* If we used a marker to hold point, mark, or an end of the region,
     temporarily, convert it to an integer now.  */
  for (i = 1; i <= count; i++)
    if (varies[i] >= 1 && varies[i] <= 4)
      XSETINT (args[i], marker_position (args[i]));

  if (record_then_fail)
    Fbarf_if_buffer_read_only ();

  Vthis_command = save_this_command;
  Vthis_original_command = save_this_original_command;
  real_this_command= save_real_this_command;
  current_kboard->Vlast_command = save_last_command;

  {
    Lisp_Object val;
    specbind (Qcommand_debug_status, Qnil);

    temporarily_switch_to_single_kboard (NULL);
    val = Ffuncall (count + 1, args);
    UNGCPRO;
    return unbind_to (speccount, val);
  }
}

DEFUN ("prefix-numeric-value", Fprefix_numeric_value, Sprefix_numeric_value,
       1, 1, 0,
       doc: /* Return numeric meaning of raw prefix argument RAW.
A raw prefix argument is what you get from `(interactive "P")'.
Its numeric meaning is what you would get from `(interactive "p")'.  */)
     (raw)
     Lisp_Object raw;
{
  Lisp_Object val;

  if (NILP (raw))
    XSETFASTINT (val, 1);
  else if (EQ (raw, Qminus))
    XSETINT (val, -1);
  else if (CONSP (raw) && INTEGERP (XCAR (raw)))
    XSETINT (val, XINT (XCAR (raw)));
  else if (INTEGERP (raw))
    val = raw;
  else
    XSETFASTINT (val, 1);

  return val;
}

void
syms_of_callint ()
{
  point_marker = Fmake_marker ();
  staticpro (&point_marker);

  callint_message = Qnil;
  staticpro (&callint_message);

  preserved_fns = Fcons (intern ("region-beginning"),
			 Fcons (intern ("region-end"),
				Fcons (intern ("point"),
				       Fcons (intern ("mark"), Qnil))));
  staticpro (&preserved_fns);

  Qlist = intern ("list");
  staticpro (&Qlist);
  Qlet = intern ("let");
  staticpro (&Qlet);
  Qif = intern ("if");
  staticpro (&Qif);
  Qwhen = intern ("when");
  staticpro (&Qwhen);
  Qletx = intern ("let*");
  staticpro (&Qletx);
  Qsave_excursion = intern ("save-excursion");
  staticpro (&Qsave_excursion);
  Qprogn = intern ("progn");
  staticpro (&Qprogn);

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

  DEFVAR_KBOARD ("prefix-arg", Vprefix_arg,
		 doc: /* The value of the prefix argument for the next editing command.
It may be a number, or the symbol `-' for just a minus sign as arg,
or a list whose car is a number for just one or more C-u's
or nil if no argument has been specified.

You cannot examine this variable to find the argument for this command
since it has been set to nil by the time you can look.
Instead, you should use the variable `current-prefix-arg', although
normally commands can get this prefix argument with (interactive "P").  */);

  DEFVAR_KBOARD ("last-prefix-arg", Vlast_prefix_arg,
		 doc: /* The value of the prefix argument for the previous editing command.
See `prefix-arg' for the meaning of the value.  */);

  DEFVAR_LISP ("current-prefix-arg", &Vcurrent_prefix_arg,
	       doc: /* The value of the prefix argument for this editing command.
It may be a number, or the symbol `-' for just a minus sign as arg,
or a list whose car is a number for just one or more C-u's
or nil if no argument has been specified.
This is what `(interactive \"P\")' returns.  */);
  Vcurrent_prefix_arg = Qnil;

  DEFVAR_LISP ("command-history", &Vcommand_history,
	       doc: /* List of recent commands that read arguments from terminal.
Each command is represented as a form to evaluate.

Maximum length of the history list is determined by the value
of `history-length', which see.  */);
  Vcommand_history = Qnil;

  DEFVAR_LISP ("command-debug-status", &Vcommand_debug_status,
	       doc: /* Debugging status of current interactive command.
Bound each time `call-interactively' is called;
may be set by the debugger as a reminder for itself.  */);
  Vcommand_debug_status = Qnil;

  DEFVAR_LISP ("mark-even-if-inactive", &Vmark_even_if_inactive,
	       doc: /* *Non-nil means you can use the mark even when inactive.
This option makes a difference in Transient Mark mode.
When the option is non-nil, deactivation of the mark
turns off region highlighting, but commands that use the mark
behave as if the mark were still active.  */);
  Vmark_even_if_inactive = Qt;

  DEFVAR_LISP ("mouse-leave-buffer-hook", &Vmouse_leave_buffer_hook,
	       doc: /* Hook to run when about to switch windows with a mouse command.
Its purpose is to give temporary modes such as Isearch mode
a way to turn themselves off when a mouse command switches windows.  */);
  Vmouse_leave_buffer_hook = Qnil;

  defsubr (&Sinteractive);
  defsubr (&Scall_interactively);
  defsubr (&Sprefix_numeric_value);
}

/* arch-tag: a3a7cad7-bcac-42ce-916e-1bd2546ebf37
   (do not change this comment) */
