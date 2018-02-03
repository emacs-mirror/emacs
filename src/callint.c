/* Call a Lisp function interactively.
   Copyright (C) 1985-1986, 1993-1995, 1997, 2000-2018 Free Software
   Foundation, Inc.

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
#include "ptr-bounds.h"
#include "character.h"
#include "buffer.h"
#include "keyboard.h"
#include "window.h"

static Lisp_Object preserved_fns;

/* Marker used within call-interactively to refer to point.  */
static Lisp_Object point_marker;

/* String for the prompt text used in Fcall_interactively.  */
static Lisp_Object callint_message;

/* ARGSUSED */
DEFUN ("interactive", Finteractive, Sinteractive, 0, UNEVALLED, 0,
       doc: /* Specify a way of parsing arguments for interactive use of a function.
For example, write
 (defun foo (arg buf) "Doc string" (interactive "P\\nbbuffer: ") .... )
 to make ARG be the raw prefix argument, and set BUF to an existing buffer,
 when `foo' is called as a command.

The "call" to `interactive' is actually a declaration rather than a
 function; it tells `call-interactively' how to read arguments to pass
 to the function.  When actually called, `interactive' just returns
 nil.

Usually the argument of `interactive' is a string containing a code
 letter followed optionally by a prompt.  (Some code letters do not
 use I/O to get the argument and do not use prompts.)  To pass several
 arguments to the command, concatenate the individual strings,
 separating them by newline characters.

Prompts are passed to `format', and may use % escapes to print the
 arguments that have already been read.
If the argument is not a string, it is evaluated to get a list of
 arguments to pass to the command.
Just `(interactive)' means pass no arguments to the command when
 calling interactively.

Code letters available are:
a -- Function name: symbol with a function definition.
b -- Name of existing buffer.
B -- Name of buffer, possibly nonexistent.
c -- Character (no input method is used).
C -- Command name: symbol with interactive function definition.
d -- Value of point as number.  Does not do I/O.
D -- Directory name.
e -- Parameterized event (i.e., one that's a list) that invoked this command.
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
v -- Variable name: symbol that is `custom-variable-p'.
x -- Lisp expression read but not evaluated.
X -- Lisp expression read and evaluated.
z -- Coding system.
Z -- Coding system, nil if no prefix arg.

In addition, if the string begins with `*', an error is signaled if
  the buffer is read-only.
If `@' appears at the beginning of the string, and if the key sequence
 used to invoke the command includes any mouse events, then the window
 associated with the first of those events is selected before the
 command is run.
If the string begins with `^' and `shift-select-mode' is non-nil,
 Emacs first calls the function `handle-shift-selection'.
You may use `@', `*', and `^' together.  They are processed in the
 order that they appear, before reading any arguments.
usage: (interactive &optional ARG-DESCRIPTOR)  */
       attributes: const)
  (Lisp_Object args)
{
  return Qnil;
}

/* Quotify EXP: if EXP is constant, return it.
   If EXP is not constant, return (quote EXP).  */
static Lisp_Object
quotify_arg (register Lisp_Object exp)
{
  if (CONSP (exp)
      || (SYMBOLP (exp)
	  && !NILP (exp) && !EQ (exp, Qt)))
    return list2 (Qquote, exp);

  return exp;
}

/* Modify EXP by quotifying each element (except the first).  */
static Lisp_Object
quotify_args (Lisp_Object exp)
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

static const char *callint_argfuns[]
    = {"", "point", "mark", "region-beginning", "region-end"};

static void
check_mark (bool for_region)
{
  Lisp_Object tem;
  tem = Fmarker_buffer (BVAR (current_buffer, mark));
  if (NILP (tem) || (XBUFFER (tem) != current_buffer))
    error (for_region ? "The mark is not set now, so there is no region"
	   : "The mark is not set now");
  if (!NILP (Vtransient_mark_mode) && NILP (Vmark_even_if_inactive)
      && NILP (BVAR (current_buffer, mark_active)))
    xsignal0 (Qmark_inactive);
}

/* If the list of args INPUT was produced with an explicit call to
   `list', look for elements that were computed with
   (region-beginning) or (region-end), and put those expressions into
   VALUES instead of the present values.

   This function doesn't return a value because it modifies elements
   of VALUES to do its job.  */

static void
fix_command (Lisp_Object input, Lisp_Object values)
{
  /* FIXME: Instead of this ugly hack, we should provide a way for an
     interactive spec to return an expression/function that will re-build the
     args without user intervention.  */
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
		  carelt = XCAR (elt);
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

/* Helper function to call `read-file-name' from C.  */

static Lisp_Object
read_file_name (Lisp_Object default_filename, Lisp_Object mustmatch,
		Lisp_Object initial, Lisp_Object predicate)
{
  return CALLN (Ffuncall, intern ("read-file-name"),
		callint_message, Qnil, default_filename,
		mustmatch, initial, predicate);
}

/* BEWARE: Calling this directly from C would defeat the purpose!  */
DEFUN ("funcall-interactively", Ffuncall_interactively, Sfuncall_interactively,
       1, MANY, 0, doc: /* Like `funcall' but marks the call as interactive.
I.e. arrange that within the called function `called-interactively-p' will
return non-nil.
usage: (funcall-interactively FUNCTION &rest ARGUMENTS)  */)
     (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t speccount = SPECPDL_INDEX ();
  temporarily_switch_to_single_kboard (NULL);

  /* Nothing special to do here, all the work is inside
     `called-interactively-p'.  Which will look for us as a marker in the
     backtrace.  */
  return unbind_to (speccount, Ffuncall (nargs, args));
}

DEFUN ("call-interactively", Fcall_interactively, Scall_interactively, 1, 3, 0,
       doc: /* Call FUNCTION, providing args according to its interactive calling specs.
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
  (Lisp_Object function, Lisp_Object record_flag, Lisp_Object keys)
{
  ptrdiff_t speccount = SPECPDL_INDEX ();

  bool arg_from_tty = false;
  ptrdiff_t key_count;
  bool record_then_fail = false;

  Lisp_Object save_this_command = Vthis_command;
  Lisp_Object save_this_original_command = Vthis_original_command;
  Lisp_Object save_real_this_command = Vreal_this_command;
  Lisp_Object save_last_command = KVAR (current_kboard, Vlast_command);

  if (NILP (keys))
    keys = this_command_keys, key_count = this_command_key_count;
  else
    {
      CHECK_VECTOR (keys);
      key_count = ASIZE (keys);
    }

  /* Save this now, since use of minibuffer will clobber it.  */
  Lisp_Object prefix_arg = Vcurrent_prefix_arg;

  Lisp_Object enable = (SYMBOLP (function)
			? Fget (function, Qenable_recursive_minibuffers)
			: Qnil);

  /* If k or K discard an up-event, save it here so it can be retrieved with
     U.  */
  Lisp_Object up_event = Qnil;

  /* Set SPECS to the interactive form, or barf if not interactive.  */
  Lisp_Object form = Finteractive_form (function);
  if (! CONSP (form))
    wrong_type_argument (Qcommandp, function);
  Lisp_Object specs = Fcar (XCDR (form));

  /* At this point the value of SPECS could help provide a way to
     specify how to represent the arguments in command history.
     The feature is not fully implemented.  */

  /* If SPECS is not a string, invent one.  */
  if (! STRINGP (specs))
    {
      Lisp_Object funval = Findirect_function (function, Qt);
      uintmax_t events = num_input_events;
      Lisp_Object input = specs;
      /* Compute the arg values using the user's expression.  */
      specs = Feval (specs,
 		     CONSP (funval) && EQ (Qclosure, XCAR (funval))
		     ? CAR_SAFE (XCDR (funval)) : Qnil);
      if (events != num_input_events || !NILP (record_flag))
	{
	  /* We should record this command on the command history.
	     Make a copy of the list of values, for the command history,
	     and turn them into things we can eval.  */
	  Lisp_Object values = quotify_args (Fcopy_sequence (specs));
	  fix_command (input, values);
	  Lisp_Object this_cmd = Fcons (function, values);
	  if (history_delete_duplicates)
	    Vcommand_history = Fdelete (this_cmd, Vcommand_history);
	  Vcommand_history = Fcons (this_cmd, Vcommand_history);

	  /* Don't keep command history around forever.  */
	  if (INTEGERP (Vhistory_length) && XINT (Vhistory_length) > 0)
	    {
	      Lisp_Object teml = Fnthcdr (Vhistory_length, Vcommand_history);
	      if (CONSP (teml))
		XSETCDR (teml, Qnil);
	    }
	}

      Vthis_command = save_this_command;
      Vthis_original_command = save_this_original_command;
      Vreal_this_command = save_real_this_command;
      kset_last_command (current_kboard, save_last_command);

      return unbind_to (speccount, CALLN (Fapply, Qfuncall_interactively,
					  function, specs));
    }

  /* SPECS is set to a string; use it as an interactive prompt.
     Copy it so that STRING will be valid even if a GC relocates SPECS.  */
  USE_SAFE_ALLOCA;
  ptrdiff_t string_len = SBYTES (specs);
  char *string = SAFE_ALLOCA (string_len + 1);
  memcpy (string, SDATA (specs), string_len + 1);
  char *string_end = string + string_len;

  /* The index of the next element of this_command_keys to examine for
     the 'e' interactive code.  Initialize it to point to the first
     event with parameters.  */
  ptrdiff_t next_event;
  for (next_event = 0; next_event < key_count; next_event++)
    if (EVENT_HAS_PARAMETERS (AREF (keys, next_event)))
      break;

  /* Handle special starting chars `*' and `@'.  Also `-'.  */
  /* Note that `+' is reserved for user extensions.  */
  for (;; string++)
    {
      if (*string == '+')
	error ("`+' is not used in `interactive' for ordinary commands");
      else if (*string == '*')
	{
	  if (!NILP (BVAR (current_buffer, read_only)))
	    {
	      if (!NILP (record_flag))
		{
		  for (char *p = string + 1; p < string_end; p++)
		    if (! (*p == 'r' || *p == 'p' || *p == 'P' || *p == '\n'))
		      Fbarf_if_buffer_read_only (Qnil);
		  record_then_fail = true;
		}
	      else
		Fbarf_if_buffer_read_only (Qnil);
	    }
	}
      /* Ignore this for semi-compatibility with Lucid.  */
      else if (*string == '-')
	;
      else if (*string == '@')
	{
	  Lisp_Object w, event = (next_event < key_count
				  ? AREF (keys, next_event)
				  : Qnil);
	  if (EVENT_HAS_PARAMETERS (event)
	      && (w = XCDR (event), CONSP (w))
	      && (w = XCAR (w), CONSP (w))
	      && (w = XCAR (w), WINDOWP (w)))
	    {
	      if (MINI_WINDOW_P (XWINDOW (w))
		  && ! (minibuf_level > 0 && EQ (w, minibuf_window)))
		error ("Attempt to select inactive minibuffer window");

	      /* If the current buffer wants to clean up, let it.  */
              run_hook (Qmouse_leave_buffer_hook);

	      Fselect_window (w, Qnil);
	    }
	}
      else if (*string == '^')
	call0 (Qhandle_shift_selection);
      else break;
    }

  /* Count the number of arguments, which is two (the function itself and
     `funcall-interactively') plus the number of arguments the interactive spec
     would have us give to the function.  */
  ptrdiff_t nargs = 2;
  for (char const *tem = string; tem < string_end; tem++)
    {
      /* 'r' specifications ("point and mark as 2 numeric args")
	 produce *two* arguments.  */
      nargs += 1 + (*tem == 'r');
      tem = memchr (tem, '\n', string_len - (tem - string));
      if (!tem)
	break;
    }

  if (MOST_POSITIVE_FIXNUM < min (PTRDIFF_MAX, SIZE_MAX) / word_size
      && MOST_POSITIVE_FIXNUM < nargs)
    memory_full (SIZE_MAX);

  /* ARGS will contain the array of arguments to pass to the function.
     VISARGS will contain the same list but in a nicer form, so that if we
     pass it to Fformat_message it will be understandable to a human.
     Allocate them all at one go.  This wastes a bit of memory, but
     it's OK to trade space for speed.  */
  Lisp_Object *args;
  SAFE_NALLOCA (args, 3, nargs);
  Lisp_Object *visargs = args + nargs;
  /* If varies[I] > 0, the Ith argument shouldn't just have its value
     in this call quoted in the command history.  It should be
     recorded as a call to the function named callint_argfuns[varies[I]].  */
  signed char *varies = (signed char *) (visargs + nargs);

  memclear (args, nargs * (2 * word_size + 1));
  args = ptr_bounds_clip (args, nargs * sizeof *args);
  visargs = ptr_bounds_clip (visargs, nargs * sizeof *visargs);
  varies = ptr_bounds_clip (varies, nargs * sizeof *varies);

  if (!NILP (enable))
    specbind (Qenable_recursive_minibuffers, Qt);

  char const *tem = string;
  for (ptrdiff_t i = 2; tem < string_end; i++)
    {
      char *pnl = memchr (tem + 1, '\n', string_len - (tem + 1 - string));
      ptrdiff_t sz = pnl ? pnl - (tem + 1) : string_end - (tem + 1);

      visargs[1] = make_string (tem + 1, sz);
      callint_message = Fformat_message (i - 1, visargs + 1);

      switch (*tem)
	{
	case 'a':		/* Symbol defined as a function.  */
	  visargs[i] = Fcompleting_read (callint_message,
					 Vobarray, Qfboundp, Qt,
					 Qnil, Qnil, Qnil, Qnil);
	  args[i] = Fintern (visargs[i], Qnil);
	  break;

	case 'b':   		/* Name of existing buffer.  */
	  args[i] = Fcurrent_buffer ();
	  if (EQ (selected_window, minibuf_window))
	    args[i] = Fother_buffer (args[i], Qnil, Qnil);
	  args[i] = Fread_buffer (callint_message, args[i], Qt, Qnil);
	  break;

	case 'B':		/* Name of buffer, possibly nonexistent.  */
	  args[i] = Fread_buffer (callint_message,
				  Fother_buffer (Fcurrent_buffer (),
						 Qnil, Qnil),
				  Qnil, Qnil);
	  break;

        case 'c':		/* Character.  */
	  /* Prompt in `minibuffer-prompt' face.  */
	  Fput_text_property (make_number (0),
			      make_number (SCHARS (callint_message)),
			      Qface, Qminibuffer_prompt, callint_message);
	  args[i] = Fread_char (callint_message, Qnil, Qnil);
	  message1_nolog (0);
	  /* See bug#8479.  */
	  if (! CHARACTERP (args[i]))
	    error ("Non-character input-event");
	  visargs[i] = Fchar_to_string (args[i]);
	  break;

	case 'C':	      /* Command: symbol with interactive function.  */
	  visargs[i] = Fcompleting_read (callint_message,
					 Vobarray, Qcommandp,
					 Qt, Qnil, Qnil, Qnil, Qnil);
	  args[i] = Fintern (visargs[i], Qnil);
	  break;

	case 'd':		/* Value of point.  Does not do I/O.  */
	  set_marker_both (point_marker, Qnil, PT, PT_BYTE);
	  args[i] = point_marker;
	  /* visargs[i] = Qnil; */
	  varies[i] = 1;
	  break;

	case 'D':		/* Directory name.  */
	  args[i] = read_file_name (BVAR (current_buffer, directory), Qlambda,
				    Qnil, Qfile_directory_p);
	  break;

	case 'f':		/* Existing file name.  */
	  args[i] = read_file_name (Qnil, Qlambda, Qnil, Qnil);
	  break;

	case 'F':		/* Possibly nonexistent file name.  */
	  args[i] = read_file_name (Qnil, Qnil, Qnil, Qnil);
	  break;

	case 'G':		/* Possibly nonexistent file name,
				   default to directory alone.  */
	  args[i] = read_file_name (Qnil, Qnil, empty_unibyte_string, Qnil);
	  break;

	case 'i':		/* Ignore an argument -- Does not do I/O.  */
	  varies[i] = -1;
	  break;

	case 'k':		/* Key sequence.  */
	  {
	    ptrdiff_t speccount1 = SPECPDL_INDEX ();
	    specbind (Qcursor_in_echo_area, Qt);
	    /* Prompt in `minibuffer-prompt' face.  */
	    Fput_text_property (make_number (0),
				make_number (SCHARS (callint_message)),
				Qface, Qminibuffer_prompt, callint_message);
	    args[i] = Fread_key_sequence (callint_message,
					  Qnil, Qnil, Qnil, Qnil);
	    unbind_to (speccount1, Qnil);
	    visargs[i] = Fkey_description (args[i], Qnil);

	    /* If the key sequence ends with a down-event,
	       discard the following up-event.  */
	    Lisp_Object teml
	      = Faref (args[i], make_number (XINT (Flength (args[i])) - 1));
	    if (CONSP (teml))
	      teml = XCAR (teml);
	    if (SYMBOLP (teml))
	      {
		teml = Fget (teml, Qevent_symbol_elements);
		/* Ignore first element, which is the base key.  */
		Lisp_Object tem2 = Fmemq (Qdown, Fcdr (teml));
		if (! NILP (tem2))
		  up_event = Fread_event (Qnil, Qnil, Qnil);
	      }
	  }
	  break;

	case 'K':		/* Key sequence to be defined.  */
	  {
	    ptrdiff_t speccount1 = SPECPDL_INDEX ();
	    specbind (Qcursor_in_echo_area, Qt);
	    /* Prompt in `minibuffer-prompt' face.  */
	    Fput_text_property (make_number (0),
				make_number (SCHARS (callint_message)),
				Qface, Qminibuffer_prompt, callint_message);
	    args[i] = Fread_key_sequence_vector (callint_message,
						 Qnil, Qt, Qnil, Qnil);
	    visargs[i] = Fkey_description (args[i], Qnil);
	    unbind_to (speccount1, Qnil);

	    /* If the key sequence ends with a down-event,
	       discard the following up-event.  */
	    Lisp_Object teml
	      = Faref (args[i], make_number (XINT (Flength (args[i])) - 1));
	    if (CONSP (teml))
	      teml = XCAR (teml);
	    if (SYMBOLP (teml))
	      {
		teml = Fget (teml, Qevent_symbol_elements);
		/* Ignore first element, which is the base key.  */
		Lisp_Object tem2 = Fmemq (Qdown, Fcdr (teml));
		if (! NILP (tem2))
		  up_event = Fread_event (Qnil, Qnil, Qnil);
	      }
	  }
	  break;

	case 'U':		/* Up event from last k or K.  */
	  if (!NILP (up_event))
	    {
	      args[i] = Fmake_vector (make_number (1), up_event);
	      up_event = Qnil;
	      visargs[i] = Fkey_description (args[i], Qnil);
	    }
	  break;

	case 'e':		/* The invoking event.  */
	  if (next_event >= key_count)
	    error ("%s must be bound to an event with parameters",
		   (SYMBOLP (function)
		    ? SSDATA (SYMBOL_NAME (function))
		    : "command"));
	  args[i] = AREF (keys, next_event);
	  varies[i] = -1;

	  /* Find the next parameterized event.  */
	  do
	    next_event++;
	  while (next_event < key_count
		 && ! EVENT_HAS_PARAMETERS (AREF (keys, next_event)));

	  break;

	case 'm':		/* Value of mark.  Does not do I/O.  */
	  check_mark (false);
	  /* visargs[i] = Qnil; */
	  args[i] = BVAR (current_buffer, mark);
	  varies[i] = 2;
	  break;

	case 'M':		/* String read via minibuffer with
				   inheriting the current input method.  */
	  args[i] = Fread_string (callint_message,
				  Qnil, Qnil, Qnil, Qt);
	  break;

	case 'N':     /* Prefix arg as number, else number from minibuffer.  */
	  if (!NILP (prefix_arg))
	    goto have_prefix_arg;
	  FALLTHROUGH;
	case 'n':		/* Read number from minibuffer.  */
	  args[i] = call1 (Qread_number, callint_message);
	  visargs[i] = Fnumber_to_string (args[i]);
	  break;

	case 'P':		/* Prefix arg in raw form.  Does no I/O.  */
	  args[i] = prefix_arg;
	  /* visargs[i] = Qnil; */
	  varies[i] = -1;
	  break;

	case 'p':		/* Prefix arg converted to number.  No I/O.  */
	have_prefix_arg:
	  args[i] = Fprefix_numeric_value (prefix_arg);
	  /* visargs[i] = Qnil; */
	  varies[i] = -1;
	  break;

	case 'r':		/* Region, point and mark as 2 args.  */
	  {
	    check_mark (true);
	    set_marker_both (point_marker, Qnil, PT, PT_BYTE);
	    ptrdiff_t mark = marker_position (BVAR (current_buffer, mark));
	    /* visargs[i] = visargs[i + 1] = Qnil; */
	    args[i] = PT < mark ? point_marker : BVAR (current_buffer, mark);
	    varies[i] = 3;
	    args[++i] = PT > mark ? point_marker : BVAR (current_buffer, mark);
	    varies[i] = 4;
	  }
	  break;

	case 's':		/* String read via minibuffer without
				   inheriting the current input method.  */
	  args[i] = Fread_string (callint_message,
				  Qnil, Qnil, Qnil, Qnil);
	  break;

	case 'S':		/* Any symbol.  */
	  visargs[i] = Fread_string (callint_message,
				     Qnil, Qnil, Qnil, Qnil);
	  args[i] = Fintern (visargs[i], Qnil);
	  break;

	case 'v':		/* Variable name: symbol that is
				   custom-variable-p.  */
	  args[i] = Fread_variable (callint_message, Qnil);
	  visargs[i] = last_minibuf_string;
	  break;

	case 'x':		/* Lisp expression read but not evaluated.  */
	  args[i] = call1 (intern ("read-minibuffer"), callint_message);
	  visargs[i] = last_minibuf_string;
	  break;

	case 'X':		/* Lisp expression read and evaluated.  */
	  args[i] = call1 (intern ("eval-minibuffer"), callint_message);
	  visargs[i] = last_minibuf_string;
 	  break;

	case 'Z':		/* Coding-system symbol, or ignore the
				   argument if no prefix.  */
	  if (NILP (prefix_arg))
	    {
	      /* args[i] = Qnil; */
	      varies[i] = -1;
	    }
	  else
	    {
	      args[i]
		= Fread_non_nil_coding_system (callint_message);
	      visargs[i] = last_minibuf_string;
	    }
	  break;

	case 'z':		/* Coding-system symbol or nil.  */
	  args[i] = Fread_coding_system (callint_message, Qnil);
	  visargs[i] = last_minibuf_string;
	  break;

	  /* We have a case for `+' so we get an error
	     if anyone tries to define one here.  */
	case '+':
	default:
	  {
	    /* How many bytes are left unprocessed in the specs string?
	       (Note that this excludes the trailing null byte.)  */
	    ptrdiff_t bytes_left = string_len - (tem - string);
	    unsigned letter;

	    /* If we have enough bytes left to treat the sequence as a
	       character, show that character's codepoint; otherwise
	       show only its first byte.  */
	    if (bytes_left >= BYTES_BY_CHAR_HEAD (*((unsigned char *) tem)))
	      letter = STRING_CHAR ((unsigned char *) tem);
	    else
	      letter = *((unsigned char *) tem);

	    error (("Invalid control letter `%c' (#o%03o, #x%04x)"
		    " in interactive calling string"),
		   (int) letter, letter, letter);
	  }
	}

      if (varies[i] == 0)
	arg_from_tty = true;

      if (NILP (visargs[i]) && STRINGP (args[i]))
	visargs[i] = args[i];

      tem = memchr (tem, '\n', string_len - (tem - string));
      if (tem) tem++;
      else tem = string_end;
    }
  unbind_to (speccount, Qnil);

  maybe_quit ();

  args[0] = Qfuncall_interactively;
  args[1] = function;

  if (arg_from_tty || !NILP (record_flag))
    {
      /* We don't need `visargs' any more, so let's recycle it since we need
	 an array of just the same size.  */
      visargs[1] = function;
      for (ptrdiff_t i = 2; i < nargs; i++)
	visargs[i] = (varies[i] > 0
		      ? list1 (intern (callint_argfuns[varies[i]]))
		      : quotify_arg (args[i]));
      Vcommand_history = Fcons (Flist (nargs - 1, visargs + 1),
				Vcommand_history);
      /* Don't keep command history around forever.  */
      if (INTEGERP (Vhistory_length) && XINT (Vhistory_length) > 0)
	{
	  Lisp_Object teml = Fnthcdr (Vhistory_length, Vcommand_history);
	  if (CONSP (teml))
	    XSETCDR (teml, Qnil);
	}
    }

  /* If we used a marker to hold point, mark, or an end of the region,
     temporarily, convert it to an integer now.  */
  for (ptrdiff_t i = 2; i < nargs; i++)
    if (varies[i] >= 1 && varies[i] <= 4)
      XSETINT (args[i], marker_position (args[i]));

  if (record_then_fail)
    Fbarf_if_buffer_read_only (Qnil);

  Vthis_command = save_this_command;
  Vthis_original_command = save_this_original_command;
  Vreal_this_command = save_real_this_command;
  kset_last_command (current_kboard, save_last_command);

  specbind (Qcommand_debug_status, Qnil);

  Lisp_Object val = Ffuncall (nargs, args);
  SAFE_FREE ();
  return unbind_to (speccount, val);
}

DEFUN ("prefix-numeric-value", Fprefix_numeric_value, Sprefix_numeric_value,
       1, 1, 0,
       doc: /* Return numeric meaning of raw prefix argument RAW.
A raw prefix argument is what you get from `(interactive "P")'.
Its numeric meaning is what you would get from `(interactive "p")'.  */)
  (Lisp_Object raw)
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
syms_of_callint (void)
{
  point_marker = Fmake_marker ();
  staticpro (&point_marker);

  callint_message = Qnil;
  staticpro (&callint_message);

  preserved_fns = listn (CONSTYPE_PURE, 4,
			 intern_c_string ("region-beginning"),
			 intern_c_string ("region-end"),
			 intern_c_string ("point"),
			 intern_c_string ("mark"));

  DEFSYM (Qlist, "list");
  DEFSYM (Qlet, "let");
  DEFSYM (Qif, "if");
  DEFSYM (Qwhen, "when");
  DEFSYM (Qletx, "let*");
  DEFSYM (Qsave_excursion, "save-excursion");
  DEFSYM (Qprogn, "progn");
  DEFSYM (Qminus, "-");
  DEFSYM (Qplus, "+");
  DEFSYM (Qhandle_shift_selection, "handle-shift-selection");
  DEFSYM (Qread_number, "read-number");
  DEFSYM (Qfuncall_interactively, "funcall-interactively");
  DEFSYM (Qcommand_debug_status, "command-debug-status");
  DEFSYM (Qenable_recursive_minibuffers, "enable-recursive-minibuffers");
  DEFSYM (Qmouse_leave_buffer_hook, "mouse-leave-buffer-hook");

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

  DEFVAR_LISP ("current-prefix-arg", Vcurrent_prefix_arg,
	       doc: /* The value of the prefix argument for this editing command.
It may be a number, or the symbol `-' for just a minus sign as arg,
or a list whose car is a number for just one or more C-u's
or nil if no argument has been specified.
This is what `(interactive \"P\")' returns.  */);
  Vcurrent_prefix_arg = Qnil;

  DEFVAR_LISP ("command-history", Vcommand_history,
	       doc: /* List of recent commands that read arguments from terminal.
Each command is represented as a form to evaluate.

Maximum length of the history list is determined by the value
of `history-length', which see.  */);
  Vcommand_history = Qnil;

  DEFVAR_LISP ("command-debug-status", Vcommand_debug_status,
	       doc: /* Debugging status of current interactive command.
Bound each time `call-interactively' is called;
may be set by the debugger as a reminder for itself.  */);
  Vcommand_debug_status = Qnil;

  DEFVAR_LISP ("mark-even-if-inactive", Vmark_even_if_inactive,
	       doc: /* Non-nil means you can use the mark even when inactive.
This option makes a difference in Transient Mark mode.
When the option is non-nil, deactivation of the mark
turns off region highlighting, but commands that use the mark
behave as if the mark were still active.  */);
  Vmark_even_if_inactive = Qt;

  DEFVAR_LISP ("mouse-leave-buffer-hook", Vmouse_leave_buffer_hook,
	       doc: /* Hook to run when about to switch windows with a mouse command.
Its purpose is to give temporary modes such as Isearch mode
a way to turn themselves off when a mouse command switches windows.  */);
  Vmouse_leave_buffer_hook = Qnil;

  defsubr (&Sinteractive);
  defsubr (&Scall_interactively);
  defsubr (&Sfuncall_interactively);
  defsubr (&Sprefix_numeric_value);
}
