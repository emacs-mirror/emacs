/* Minibuffer input and completion.
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
#include "commands.h"
#include "buffer.h"
#include "dispextern.h"
#include "frame.h"
#include "window.h"
#include "syntax.h"

#define min(a, b) ((a) < (b) ? (a) : (b))

extern int quit_char;

/* List of buffers for use as minibuffers.
   The first element of the list is used for the outermost minibuffer
   invocation, the next element is used for a recursive minibuffer
   invocation, etc.  The list is extended at the end as deeper
   minibuffer recursions are encountered.  */
Lisp_Object Vminibuffer_list;

/* Data to remember during recursive minibuffer invocations  */
Lisp_Object minibuf_save_list;

/* Depth in minibuffer invocations.  */
int minibuf_level;

/* Nonzero means display completion help for invalid input */
int auto_help;

/* Fread_minibuffer leaves the input here as a string. */
Lisp_Object last_minibuf_string;

/* Nonzero means let functions called when within a minibuffer 
   invoke recursive minibuffers (to read arguments, or whatever) */
int enable_recursive_minibuffers;

/* help-form is bound to this while in the minibuffer.  */

Lisp_Object Vminibuffer_help_form;

/* Variable which is the history list to add minibuffer values to.  */

Lisp_Object Vminibuffer_history_variable;

/* Current position in the history list (adjusted by M-n and M-p).  */

Lisp_Object Vminibuffer_history_position;

Lisp_Object Qminibuffer_history;

Lisp_Object Qread_file_name_internal;

/* Normal hooks for entry to and exit from minibuffer.  */

Lisp_Object Qminibuffer_setup_hook, Vminibuffer_setup_hook;
Lisp_Object Qminibuffer_exit_hook, Vminibuffer_exit_hook;

/* Nonzero means completion ignores case.  */

int completion_ignore_case;

/* List of regexps that should restrict possible completions.  */

Lisp_Object Vcompletion_regexp_list;

/* Nonzero means raise the minibuffer frame when the minibuffer
   is entered.  */

int minibuffer_auto_raise;

/* If last completion attempt reported "Complete but not unique"
   then this is the string completed then; otherwise this is nil.  */

static Lisp_Object last_exact_completion;

Lisp_Object Quser_variable_p;

/* Non-nil means it is the window for C-M-v to scroll
   when the minibuffer is selected.  */
extern Lisp_Object Vminibuf_scroll_window;

extern Lisp_Object Voverriding_local_map;

/* Actual minibuffer invocation. */

void read_minibuf_unwind ();
Lisp_Object get_minibuffer ();
Lisp_Object read_minibuf ();

/* Read from the minibuffer using keymap MAP, initial contents INITIAL
   (a string), putting point minus BACKUP_N chars from the end of INITIAL,
   prompting with PROMPT (a string), using history list HISTVAR
   with initial position HISTPOS.  (BACKUP_N should be <= 0.)

   Normally return the result as a string (the text that was read),
   but if EXPFLAG is nonzero, read it and return the object read.
   If HISTVAR is given, save the value read on that history only if it doesn't
   match the front of that history list exactly.  The value is pushed onto
   the list as the string that was read.  */

Lisp_Object
read_minibuf (map, initial, prompt, backup_n, expflag, histvar, histpos)
     Lisp_Object map;
     Lisp_Object initial;
     Lisp_Object prompt;
     Lisp_Object backup_n;
     int expflag;
     Lisp_Object histvar;
     Lisp_Object histpos;
{
  Lisp_Object val;
  int count = specpdl_ptr - specpdl;
  Lisp_Object mini_frame;
  struct gcpro gcpro1, gcpro2, gcpro3;

#ifdef MULTI_KBOARD
  kboard_locked = 1;
#endif

  val = Qnil;
  /* Don't need to protect PROMPT, HISTVAR, and HISTPOS because we
     store them away before we can GC.  Don't need to protect
     BACKUP_N because we use the value only if it is an integer.  */
  GCPRO3 (map, initial, val);

  if (!STRINGP (prompt))
    prompt = build_string ("");

  if (!enable_recursive_minibuffers
      && minibuf_level > 0
      && (EQ (selected_window, minibuf_window)))
#if 0
	  || selected_frame != XFRAME (WINDOW_FRAME (XWINDOW (minibuf_window)))
#endif
    error ("Command attempted to use minibuffer while in minibuffer");

  /* Could we simply bind these variables instead?  */
  minibuf_save_list
    = Fcons (minibuf_prompt,
	     Fcons (make_number (minibuf_prompt_width),
		    Fcons (Vhelp_form,
			   Fcons (Vcurrent_prefix_arg,
				  Fcons (Vminibuffer_history_position,
					 Fcons (Vminibuffer_history_variable,
						Fcons (Voverriding_local_map,
						       minibuf_save_list)))))));
  minibuf_prompt_width = 0;	/* xdisp.c puts in the right value.  */
  minibuf_prompt = Fcopy_sequence (prompt);
  Vminibuffer_history_position = histpos;
  Vminibuffer_history_variable = histvar;

  record_unwind_protect (Fset_window_configuration,
			 Fcurrent_window_configuration (Qnil));

  /* If the minibuffer window is on a different frame, save that
     frame's configuration too.  */
#ifdef MULTI_FRAME
  mini_frame = WINDOW_FRAME (XWINDOW (minibuf_window));
  if (XFRAME (mini_frame) != selected_frame)
    record_unwind_protect (Fset_window_configuration,
			   Fcurrent_window_configuration (mini_frame));

  /* If the minibuffer is on an iconified or invisible frame,
     make it visible now.  */
  Fmake_frame_visible (mini_frame);

  if (minibuffer_auto_raise)
    Fraise_frame (mini_frame);
#endif

  val = current_buffer->directory;
  Fset_buffer (get_minibuffer (minibuf_level));

  /* The current buffer's default directory is usually the right thing
     for our minibuffer here.  However, if you're typing a command at
     a minibuffer-only frame when minibuf_level is zero, then buf IS
     the current_buffer, so reset_buffer leaves buf's default
     directory unchanged.  This is a bummer when you've just started
     up Emacs and buf's default directory is Qnil.  Here's a hack; can
     you think of something better to do?  Find another buffer with a
     better directory, and use that one instead.  */
  if (STRINGP (val))
    current_buffer->directory = val;
  else
    {
      Lisp_Object buf_list;

      for (buf_list = Vbuffer_alist;
	   CONSP (buf_list);
	   buf_list = XCONS (buf_list)->cdr)
	{
	  Lisp_Object other_buf;

	  other_buf = XCONS (XCONS (buf_list)->car)->cdr;
	  if (STRINGP (XBUFFER (other_buf)->directory))
	    {
	      current_buffer->directory = XBUFFER (other_buf)->directory;
	      break;
	    }
	}
    }

#ifdef MULTI_FRAME
  if (XFRAME (mini_frame) != selected_frame)
    Fredirect_frame_focus (Fselected_frame (), mini_frame);
#endif
  Fmake_local_variable (Qprint_escape_newlines);
  print_escape_newlines = 1;

  record_unwind_protect (read_minibuf_unwind, Qnil);

  Vminibuf_scroll_window = selected_window;
  Fset_window_buffer (minibuf_window, Fcurrent_buffer ());
  Fselect_window (minibuf_window);
  XSETFASTINT (XWINDOW (minibuf_window)->hscroll, 0);

  Ferase_buffer ();
  minibuf_level++;

  if (!NILP (initial))
    {
      Finsert (1, &initial);
      if (!NILP (backup_n) && INTEGERP (backup_n))
	Fforward_char (backup_n);
    }

  echo_area_glyphs = 0;
  /* This is in case the minibuffer-setup-hook calls Fsit_for.  */
  previous_echo_glyphs = 0;

  Vhelp_form = Vminibuffer_help_form;
  current_buffer->keymap = map;

  /* Run our hook, but not if it is empty.
     (run-hooks would do nothing if it is empty,
     but it's important to save time here in the usual case.  */
  if (!NILP (Vminibuffer_setup_hook) && !EQ (Vminibuffer_setup_hook, Qunbound)
      && !NILP (Vrun_hooks))
    call1 (Vrun_hooks, Qminibuffer_setup_hook);

/* ??? MCC did redraw_screen here if switching screens.  */
  recursive_edit_1 ();

  /* If cursor is on the minibuffer line,
     show the user we have exited by putting it in column 0.  */
  if ((FRAME_CURSOR_Y (selected_frame)
       >= XFASTINT (XWINDOW (minibuf_window)->top))
      && !noninteractive)
    {
      FRAME_CURSOR_X (selected_frame) = 0;
      update_frame (selected_frame, 1, 1);
    }

  /* Make minibuffer contents into a string */
  val = make_buffer_string (1, Z);
  bcopy (GAP_END_ADDR, XSTRING (val)->data + GPT - BEG, Z - GPT);

  /* VAL is the string of minibuffer text.  */
  last_minibuf_string = val;

  /* Add the value to the appropriate history list unless it is empty.  */
  if (XSTRING (val)->size != 0
      && SYMBOLP (Vminibuffer_history_variable)
      && ! EQ (XSYMBOL (Vminibuffer_history_variable)->value, Qunbound))
    {
      /* If the caller wanted to save the value read on a history list,
	 then do so if the value is not already the front of the list.  */
      Lisp_Object histval;
      histval = Fsymbol_value (Vminibuffer_history_variable);

      /* The value of the history variable must be a cons or nil.  Other
	 values are unacceptable.  We silently ignore these values.  */
      if (NILP (histval)
	  || (CONSP (histval)
	      && NILP (Fequal (last_minibuf_string, Fcar (histval)))))
	Fset (Vminibuffer_history_variable,
	      Fcons (last_minibuf_string, histval));
    }

  /* If Lisp form desired instead of string, parse it. */
  if (expflag)
    {
      Lisp_Object expr_and_pos;
      unsigned char *p;

      expr_and_pos = Fread_from_string (val, Qnil, Qnil);
      /* Ignore trailing whitespace; any other trailing junk is an error.  */
      for (p = XSTRING (val)->data + XINT (Fcdr (expr_and_pos)); *p; p++)
	if (*p != ' ' && *p != '\t' && *p != '\n')
	  error ("Trailing garbage following expression");
      val = Fcar (expr_and_pos);
    }

  /* The appropriate frame will get selected
     in set-window-configuration.  */
  RETURN_UNGCPRO (unbind_to (count, val));
}

/* Return a buffer to be used as the minibuffer at depth `depth'.
 depth = 0 is the lowest allowed argument, and that is the value
 used for nonrecursive minibuffer invocations */

Lisp_Object
get_minibuffer (depth)
     int depth;
{
  Lisp_Object tail, num, buf;
  char name[24];
  extern Lisp_Object nconc2 ();

  XSETFASTINT (num, depth);
  tail = Fnthcdr (num, Vminibuffer_list);
  if (NILP (tail))
    {
      tail = Fcons (Qnil, Qnil);
      Vminibuffer_list = nconc2 (Vminibuffer_list, tail);
    }
  buf = Fcar (tail);
  if (NILP (buf) || NILP (XBUFFER (buf)->name))
    {
      sprintf (name, " *Minibuf-%d*", depth);
      buf = Fget_buffer_create (build_string (name));

      /* Although the buffer's name starts with a space, undo should be
	 enabled in it.  */
      Fbuffer_enable_undo (buf);

      XCONS (tail)->car = buf;
    }
  else
    {
      int count = specpdl_ptr - specpdl;

      reset_buffer (XBUFFER (buf));
      record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
      Fset_buffer (buf);
      Fkill_all_local_variables ();
      unbind_to (count, Qnil);
    }

  return buf;
}

/* This function is called on exiting minibuffer, whether normally or not,
 and it restores the current window, buffer, etc. */

void
read_minibuf_unwind (data)
     Lisp_Object data;
{
  Lisp_Object old_deactivate_mark;

  /* We are exiting the minibuffer one way or the other,
     so run the hook.  */
  if (!NILP (Vminibuffer_exit_hook) && !EQ (Vminibuffer_exit_hook, Qunbound)
      && !NILP (Vrun_hooks))
    call1 (Vrun_hooks, Qminibuffer_exit_hook);

  /* Erase the minibuffer we were using at this level.  */
  Fset_buffer (XWINDOW (minibuf_window)->buffer);

  /* Prevent error in erase-buffer.  */
  current_buffer->read_only = Qnil;

  old_deactivate_mark = Vdeactivate_mark;
  Ferase_buffer ();
  Vdeactivate_mark = old_deactivate_mark;

  /* If this was a recursive minibuffer,
     tie the minibuffer window back to the outer level minibuffer buffer */
  minibuf_level--;
  /* Make sure minibuffer window is erased, not ignored */
  windows_or_buffers_changed++;
  XSETFASTINT (XWINDOW (minibuf_window)->last_modified, 0);

  /* Restore prompt, etc from outer minibuffer */
  minibuf_prompt = Fcar (minibuf_save_list);
  minibuf_save_list = Fcdr (minibuf_save_list);
  minibuf_prompt_width = XFASTINT (Fcar (minibuf_save_list));
  minibuf_save_list = Fcdr (minibuf_save_list);
  Vhelp_form = Fcar (minibuf_save_list);
  minibuf_save_list = Fcdr (minibuf_save_list);
  Vcurrent_prefix_arg = Fcar (minibuf_save_list);
  minibuf_save_list = Fcdr (minibuf_save_list);
  Vminibuffer_history_position = Fcar (minibuf_save_list);
  minibuf_save_list = Fcdr (minibuf_save_list);
  Vminibuffer_history_variable = Fcar (minibuf_save_list);
  minibuf_save_list = Fcdr (minibuf_save_list);
  Voverriding_local_map = Fcar (minibuf_save_list);
  minibuf_save_list = Fcdr (minibuf_save_list);
}


/* This comment supplies the doc string for read-from-minibuffer, 
   for make-docfile to see.  We cannot put this in the real DEFUN
   due to limits in the Unix cpp.

DEFUN ("read-from-minibuffer", Fread_from_minibuffer, Sread_from_minibuffer, 1, 5, 0,
  "Read a string from the minibuffer, prompting with string PROMPT.\n\
If optional second arg INITIAL-CONTENTS is non-nil, it is a string\n\
  to be inserted into the minibuffer before reading input.\n\
  If INITIAL-CONTENTS is (STRING . POSITION), the initial input\n\
  is STRING, but point is placed POSITION characters into the string.\n\
Third arg KEYMAP is a keymap to use whilst reading;\n\
  if omitted or nil, the default is `minibuffer-local-map'.\n\
If fourth arg READ is non-nil, then interpret the result as a lisp object\n\
  and return that object:\n\
  in other words, do `(car (read-from-string INPUT-STRING))'\n\
Fifth arg HIST, if non-nil, specifies a history list\n\
  and optionally the initial position in the list.\n\
  It can be a symbol, which is the history list variable to use,\n\
  or it can be a cons cell (HISTVAR . HISTPOS).\n\
  In that case, HISTVAR is the history list variable to use,\n\
  and HISTPOS is the initial position (the position in the list\n\
  which INITIAL-CONTENTS corresponds to).\n\
  Positions are counted starting from 1 at the beginning of the list."
*/

DEFUN ("read-from-minibuffer", Fread_from_minibuffer, Sread_from_minibuffer, 1, 5, 0,
  0 /* See immediately above */)
  (prompt, initial_contents, keymap, read, hist)
     Lisp_Object prompt, initial_contents, keymap, read, hist;
{
  int pos = 0;
  Lisp_Object histvar, histpos, position;
  position = Qnil;

  CHECK_STRING (prompt, 0);
  if (!NILP (initial_contents))
    {
      if (CONSP (initial_contents))
	{
	  position = Fcdr (initial_contents);
	  initial_contents = Fcar (initial_contents);
	}
      CHECK_STRING (initial_contents, 1);
      if (!NILP (position))
	{
	  CHECK_NUMBER (position, 0);
	  /* Convert to distance from end of input.  */
	  pos = XINT (position) - 1 - XSTRING (initial_contents)->size;
	}
    }

  if (NILP (keymap))
    keymap = Vminibuffer_local_map;
  else
    keymap = get_keymap (keymap,2);

  if (SYMBOLP (hist))
    {
      histvar = hist;
      histpos = Qnil;
    }
  else
    {
      histvar = Fcar_safe (hist);
      histpos = Fcdr_safe (hist);
    }
  if (NILP (histvar))
    histvar = Qminibuffer_history;
  if (NILP (histpos))
    XSETFASTINT (histpos, 0);

  return read_minibuf (keymap, initial_contents, prompt,
		       make_number (pos), !NILP (read), histvar, histpos);
}

DEFUN ("read-minibuffer", Fread_minibuffer, Sread_minibuffer, 1, 2, 0,
  "Return a Lisp object read using the minibuffer.\n\
Prompt with PROMPT.  If non-nil, optional second arg INITIAL-CONTENTS\n\
is a string to insert in the minibuffer before reading.")
  (prompt, initial_contents)
     Lisp_Object prompt, initial_contents;
{
  CHECK_STRING (prompt, 0);
  if (!NILP (initial_contents))
    CHECK_STRING (initial_contents, 1);
  return read_minibuf (Vminibuffer_local_map, initial_contents,
		       prompt, Qnil, 1, Qminibuffer_history, make_number (0));
}

DEFUN ("eval-minibuffer", Feval_minibuffer, Seval_minibuffer, 1, 2, 0,
  "Return value of Lisp expression read using the minibuffer.\n\
Prompt with PROMPT.  If non-nil, optional second arg INITIAL-CONTENTS\n\
is a string to insert in the minibuffer before reading.")
  (prompt, initial_contents)
     Lisp_Object prompt, initial_contents;
{
  return Feval (Fread_minibuffer (prompt, initial_contents));
}

/* Functions that use the minibuffer to read various things. */

DEFUN ("read-string", Fread_string, Sread_string, 1, 3, 0,
  "Read a string from the minibuffer, prompting with string PROMPT.\n\
If non-nil, second arg INITIAL-INPUT is a string to insert before reading.\n\
The third arg HISTORY, if non-nil, specifies a history list\n\
  and optionally the initial position in the list.\n\
See `read-from-minibuffer' for details of HISTORY argument.")
  (prompt, initial_input, history)
     Lisp_Object prompt, initial_input, history;
{
  return Fread_from_minibuffer (prompt, initial_input, Qnil, Qnil, history);
}

DEFUN ("read-no-blanks-input", Fread_no_blanks_input, Sread_no_blanks_input, 1, 2, 0,
  "Args PROMPT and INIT, strings.  Read a string from the terminal, not allowing blanks.\n\
Prompt with PROMPT, and provide INIT as an initial value of the input string.")
  (prompt, init)
     Lisp_Object prompt, init;
{
  CHECK_STRING (prompt, 0);
  if (! NILP (init))
    CHECK_STRING (init, 1);

  return read_minibuf (Vminibuffer_local_ns_map, init, prompt, Qnil, 0,
		       Qminibuffer_history, make_number (0));
}

DEFUN ("read-command", Fread_command, Sread_command, 1, 1, 0,
  "One arg PROMPT, a string.  Read the name of a command and return as a symbol.\n\
Prompts with PROMPT.")
  (prompt)
     Lisp_Object prompt;
{
  return Fintern (Fcompleting_read (prompt, Vobarray, Qcommandp, Qt, Qnil, Qnil),
		  Qnil);
}

#ifdef NOTDEF
DEFUN ("read-function", Fread_function, Sread_function, 1, 1, 0,
  "One arg PROMPT, a string.  Read the name of a function and return as a symbol.\n\
Prompts with PROMPT.")
  (prompt)
     Lisp_Object prompt;
{
  return Fintern (Fcompleting_read (prompt, Vobarray, Qfboundp, Qt, Qnil, Qnil),
		  Qnil);
}
#endif /* NOTDEF */

DEFUN ("read-variable", Fread_variable, Sread_variable, 1, 1, 0,
  "One arg PROMPT, a string.  Read the name of a user variable and return\n\
it as a symbol.  Prompts with PROMPT.\n\
A user variable is one whose documentation starts with a `*' character.")
  (prompt)
     Lisp_Object prompt;
{
  return Fintern (Fcompleting_read (prompt, Vobarray,
				    Quser_variable_p, Qt, Qnil, Qnil),
		  Qnil);
}

DEFUN ("read-buffer", Fread_buffer, Sread_buffer, 1, 3, 0,
  "One arg PROMPT, a string.  Read the name of a buffer and return as a string.\n\
Prompts with PROMPT.\n\
Optional second arg is value to return if user enters an empty line.\n\
If optional third arg REQUIRE-MATCH is non-nil, only existing buffer names are allowed.")
  (prompt, def, require_match)
     Lisp_Object prompt, def, require_match;
{
  Lisp_Object tem;
  Lisp_Object args[3];
  struct gcpro gcpro1;

  if (BUFFERP (def))
    def = XBUFFER (def)->name;
  if (!NILP (def))
    {
      args[0] = build_string ("%s(default %s) ");
      args[1] = prompt;
      args[2] = def;
      prompt = Fformat (3, args);
    }
  GCPRO1 (def);
  tem = Fcompleting_read (prompt, Vbuffer_alist, Qnil, require_match, Qnil, Qnil);
  UNGCPRO;
  if (XSTRING (tem)->size)
    return tem;
  return def;
}

DEFUN ("try-completion", Ftry_completion, Stry_completion, 2, 3, 0,
  "Return common substring of all completions of STRING in ALIST.\n\
Each car of each element of ALIST is tested to see if it begins with STRING.\n\
All that match are compared together; the longest initial sequence\n\
common to all matches is returned as a string.\n\
If there is no match at all, nil is returned.\n\
For an exact match, t is returned.\n\
\n\
ALIST can be an obarray instead of an alist.\n\
Then the print names of all symbols in the obarray are the possible matches.\n\
\n\
ALIST can also be a function to do the completion itself.\n\
It receives three arguments: the values STRING, PREDICATE and nil.\n\
Whatever it returns becomes the value of `try-completion'.\n\
\n\
If optional third argument PREDICATE is non-nil,\n\
it is used to test each possible match.\n\
The match is a candidate only if PREDICATE returns non-nil.\n\
The argument given to PREDICATE is the alist element\n\
or the symbol from the obarray.")
  (string, alist, pred)
     Lisp_Object string, alist, pred;
{
  Lisp_Object bestmatch, tail, elt, eltstring;
  int bestmatchsize;
  int compare, matchsize;
  int list = CONSP (alist) || NILP (alist);
  int index, obsize;
  int matchcount = 0;
  Lisp_Object bucket, zero, end, tem;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  CHECK_STRING (string, 0);
  if (!list && !VECTORP (alist))
    return call3 (alist, string, pred, Qnil);

  bestmatch = Qnil;

  /* If ALIST is not a list, set TAIL just for gc pro.  */
  tail = alist;
  if (! list)
    {
      index = 0;
      obsize = XVECTOR (alist)->size;
      bucket = XVECTOR (alist)->contents[index];
    }

  while (1)
    {
      /* Get the next element of the alist or obarray. */
      /* Exit the loop if the elements are all used up. */
      /* elt gets the alist element or symbol.
	 eltstring gets the name to check as a completion. */

      if (list)
	{
	  if (NILP (tail))
	    break;
	  elt = Fcar (tail);
	  eltstring = Fcar (elt);
	  tail = Fcdr (tail);
	}
      else
	{
	  if (XFASTINT (bucket) != 0)
	    {
	      elt = bucket;
	      eltstring = Fsymbol_name (elt);
	      if (XSYMBOL (bucket)->next)
		XSETSYMBOL (bucket, XSYMBOL (bucket)->next);
	      else
		XSETFASTINT (bucket, 0);
	    }
	  else if (++index >= obsize)
	    break;
	  else
	    {
	      bucket = XVECTOR (alist)->contents[index];
	      continue;
	    }
	}

      /* Is this element a possible completion? */

      if (STRINGP (eltstring)
	  && XSTRING (string)->size <= XSTRING (eltstring)->size
	  && 0 > scmp (XSTRING (eltstring)->data, XSTRING (string)->data,
		       XSTRING (string)->size))
	{
	  /* Yes. */
	  Lisp_Object regexps;
	  Lisp_Object zero;
	  XSETFASTINT (zero, 0);

	  /* Ignore this element if it fails to match all the regexps.  */
	  for (regexps = Vcompletion_regexp_list; CONSP (regexps);
	       regexps = XCONS (regexps)->cdr)
	    {
	      tem = Fstring_match (XCONS (regexps)->car, eltstring, zero);
	      if (NILP (tem))
		break;
	    }
	  if (CONSP (regexps))
	    continue;

	  /* Ignore this element if there is a predicate
	     and the predicate doesn't like it. */

	  if (!NILP (pred))
	    {
	      if (EQ (pred, Qcommandp))
		tem = Fcommandp (elt);
	      else
		{
		  GCPRO4 (tail, string, eltstring, bestmatch);
		  tem = call1 (pred, elt);
		  UNGCPRO;
		}
	      if (NILP (tem)) continue;
	    }

	  /* Update computation of how much all possible completions match */

	  matchcount++;
	  if (NILP (bestmatch))
	    bestmatch = eltstring, bestmatchsize = XSTRING (eltstring)->size;
	  else
	    {
	      compare = min (bestmatchsize, XSTRING (eltstring)->size);
	      matchsize = scmp (XSTRING (bestmatch)->data,
				XSTRING (eltstring)->data,
				compare);
	      if (matchsize < 0)
		matchsize = compare;
	      if (completion_ignore_case)
		{
		  /* If this is an exact match except for case,
		     use it as the best match rather than one that is not an
		     exact match.  This way, we get the case pattern
		     of the actual match.  */
		  if ((matchsize == XSTRING (eltstring)->size
		       && matchsize < XSTRING (bestmatch)->size)
		      ||
		      /* If there is more than one exact match ignoring case,
			 and one of them is exact including case,
			 prefer that one.  */
		      /* If there is no exact match ignoring case,
			 prefer a match that does not change the case
			 of the input.  */
		      ((matchsize == XSTRING (eltstring)->size)
		       ==
		       (matchsize == XSTRING (bestmatch)->size)
		       && !bcmp (XSTRING (eltstring)->data,
				 XSTRING (string)->data, XSTRING (string)->size)
		       && bcmp (XSTRING (bestmatch)->data,
				XSTRING (string)->data, XSTRING (string)->size)))
		    bestmatch = eltstring;
		}
	      bestmatchsize = matchsize;
	    }
	}
    }

  if (NILP (bestmatch))
    return Qnil;		/* No completions found */
  /* If we are ignoring case, and there is no exact match,
     and no additional text was supplied,
     don't change the case of what the user typed.  */
  if (completion_ignore_case && bestmatchsize == XSTRING (string)->size
      && XSTRING (bestmatch)->size > bestmatchsize)
    return string;

  /* Return t if the supplied string is an exact match (counting case);
     it does not require any change to be made.  */
  if (matchcount == 1 && bestmatchsize == XSTRING (string)->size
      && !bcmp (XSTRING (bestmatch)->data, XSTRING (string)->data,
		bestmatchsize))
    return Qt;

  XSETFASTINT (zero, 0);		/* Else extract the part in which */
  XSETFASTINT (end, bestmatchsize);	/* all completions agree */
  return Fsubstring (bestmatch, zero, end);
}

/* Compare exactly LEN chars of strings at S1 and S2,
   ignoring case if appropriate.
   Return -1 if strings match,
   else number of chars that match at the beginning.  */

int
scmp (s1, s2, len)
     register unsigned char *s1, *s2;
     int len;
{
  register int l = len;

  if (completion_ignore_case)
    {
      while (l && DOWNCASE (*s1++) == DOWNCASE (*s2++))
	l--;
    }
  else
    {
      while (l && *s1++ == *s2++)
	l--;
    }
  if (l == 0)
    return -1;
  else
    return len - l;
}

DEFUN ("all-completions", Fall_completions, Sall_completions, 2, 4, 0,
  "Search for partial matches to STRING in ALIST.\n\
Each car of each element of ALIST is tested to see if it begins with STRING.\n\
The value is a list of all the strings from ALIST that match.\n\
\n\
ALIST can be an obarray instead of an alist.\n\
Then the print names of all symbols in the obarray are the possible matches.\n\
\n\
ALIST can also be a function to do the completion itself.\n\
It receives three arguments: the values STRING, PREDICATE and t.\n\
Whatever it returns becomes the value of `all-completion'.\n\
\n\
If optional third argument PREDICATE is non-nil,\n\
it is used to test each possible match.\n\
The match is a candidate only if PREDICATE returns non-nil.\n\
The argument given to PREDICATE is the alist element\n\
or the symbol from the obarray.\n\
\n\
If the optional fourth argument HIDE-SPACES is non-nil,\n\
strings in ALIST that start with a space\n\
are ignored unless STRING itself starts with a space.")
  (string, alist, pred, hide_spaces)
     Lisp_Object string, alist, pred, hide_spaces;
{
  Lisp_Object tail, elt, eltstring;
  Lisp_Object allmatches;
  int list = CONSP (alist) || NILP (alist);
  int index, obsize;
  Lisp_Object bucket, tem;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  CHECK_STRING (string, 0);
  if (!list && !VECTORP (alist))
    {
      return call3 (alist, string, pred, Qt);
    }
  allmatches = Qnil;

  /* If ALIST is not a list, set TAIL just for gc pro.  */
  tail = alist;
  if (! list)
    {
      index = 0;
      obsize = XVECTOR (alist)->size;
      bucket = XVECTOR (alist)->contents[index];
    }

  while (1)
    {
      /* Get the next element of the alist or obarray. */
      /* Exit the loop if the elements are all used up. */
      /* elt gets the alist element or symbol.
	 eltstring gets the name to check as a completion. */

      if (list)
	{
	  if (NILP (tail))
	    break;
	  elt = Fcar (tail);
	  eltstring = Fcar (elt);
	  tail = Fcdr (tail);
	}
      else
	{
	  if (XFASTINT (bucket) != 0)
	    {
	      elt = bucket;
	      eltstring = Fsymbol_name (elt);
	      if (XSYMBOL (bucket)->next)
		XSETSYMBOL (bucket, XSYMBOL (bucket)->next);
	      else
		XSETFASTINT (bucket, 0);
	    }
	  else if (++index >= obsize)
	    break;
	  else
	    {
	      bucket = XVECTOR (alist)->contents[index];
	      continue;
	    }
	}

      /* Is this element a possible completion? */

      if (STRINGP (eltstring)
	  && XSTRING (string)->size <= XSTRING (eltstring)->size
	  /* If HIDE_SPACES, reject alternatives that start with space
	     unless the input starts with space.  */
	  && ((XSTRING (string)->size > 0 && XSTRING (string)->data[0] == ' ')
	      || XSTRING (eltstring)->data[0] != ' '
	      || NILP (hide_spaces))
	  && 0 > scmp (XSTRING (eltstring)->data, XSTRING (string)->data,
		       XSTRING (string)->size))
	{
	  /* Yes. */
	  Lisp_Object regexps;
	  Lisp_Object zero;
	  XSETFASTINT (zero, 0);

	  /* Ignore this element if it fails to match all the regexps.  */
	  for (regexps = Vcompletion_regexp_list; CONSP (regexps);
	       regexps = XCONS (regexps)->cdr)
	    {
	      tem = Fstring_match (XCONS (regexps)->car, eltstring, zero);
	      if (NILP (tem))
		break;
	    }
	  if (CONSP (regexps))
	    continue;

	  /* Ignore this element if there is a predicate
	     and the predicate doesn't like it. */

	  if (!NILP (pred))
	    {
	      if (EQ (pred, Qcommandp))
		tem = Fcommandp (elt);
	      else
		{
		  GCPRO4 (tail, eltstring, allmatches, string);
		  tem = call1 (pred, elt);
		  UNGCPRO;
		}
	      if (NILP (tem)) continue;
	    }
	  /* Ok => put it on the list. */
	  allmatches = Fcons (eltstring, allmatches);
	}
    }

  return Fnreverse (allmatches);
}

Lisp_Object Vminibuffer_completion_table, Qminibuffer_completion_table;
Lisp_Object Vminibuffer_completion_predicate, Qminibuffer_completion_predicate;
Lisp_Object Vminibuffer_completion_confirm, Qminibuffer_completion_confirm;

/* This comment supplies the doc string for completing-read,
   for make-docfile to see.  We cannot put this in the real DEFUN
   due to limits in the Unix cpp.

DEFUN ("completing-read", Fcompleting_read, Scompleting_read, 2, 6, 0,
  "Read a string in the minibuffer, with completion.\n\
Args: PROMPT, TABLE, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST.\n\
PROMPT is a string to prompt with; normally it ends in a colon and a space.\n\
TABLE is an alist whose elements' cars are strings, or an obarray.\n\
PREDICATE limits completion to a subset of TABLE.\n\
See `try-completion' and `all-completions' for more details
 on completion, TABLE, and PREDICATE.\n\
\n\
If REQUIRE-MATCH is non-nil, the user is not allowed to exit unless\n\
 the input is (or completes to) an element of TABLE or is null.\n\
 If it is also not t, Return does not exit if it does non-null completion.\n\
If the input is null, `completing-read' returns nil,\n\
 regardless of the value of REQUIRE-MATCH.\n\
\n\
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.\n\
  If it is (STRING . POSITION), the initial input\n\
  is STRING, but point is placed POSITION characters into the string.\n\
HIST, if non-nil, specifies a history list\n\
  and optionally the initial position in the list.\n\
  It can be a symbol, which is the history list variable to use,\n\
  or it can be a cons cell (HISTVAR . HISTPOS).\n\
  In that case, HISTVAR is the history list variable to use,\n\
  and HISTPOS is the initial position (the position in the list\n\
  which INITIAL-CONTENTS corresponds to).\n\
  Positions are counted starting from 1 at the beginning of the list.\n\
Completion ignores case if the ambient value of\n\
  `completion-ignore-case' is non-nil."
*/
DEFUN ("completing-read", Fcompleting_read, Scompleting_read, 2, 6, 0,
  0 /* See immediately above */)
  (prompt, table, pred, require_match, init, hist)
     Lisp_Object prompt, table, pred, require_match, init, hist;
{
  Lisp_Object val, histvar, histpos, position;
  int pos = 0;
  int count = specpdl_ptr - specpdl;
  specbind (Qminibuffer_completion_table, table);
  specbind (Qminibuffer_completion_predicate, pred);
  specbind (Qminibuffer_completion_confirm,
	    EQ (require_match, Qt) ? Qnil : Qt);
  last_exact_completion = Qnil;

  position = Qnil;
  if (!NILP (init))
    {
      if (CONSP (init))
	{
	  position = Fcdr (init);
	  init = Fcar (init);
	}
      CHECK_STRING (init, 0);
      if (!NILP (position))
	{
	  CHECK_NUMBER (position, 0);
	  /* Convert to distance from end of input.  */
	  pos = XINT (position) - XSTRING (init)->size;
	}
    }

  if (SYMBOLP (hist))
    {
      histvar = hist;
      histpos = Qnil;
    }
  else
    {
      histvar = Fcar_safe (hist);
      histpos = Fcdr_safe (hist);
    }
  if (NILP (histvar))
    histvar = Qminibuffer_history;
  if (NILP (histpos))
    XSETFASTINT (histpos, 0);

  val = read_minibuf (NILP (require_match)
		      ? Vminibuffer_local_completion_map
		      : Vminibuffer_local_must_match_map,
		      init, prompt, make_number (pos), 0,
		      histvar, histpos);
  return unbind_to (count, val);
}

/* Temporarily display the string M at the end of the current
   minibuffer contents.  This is used to display things like
   "[No Match]" when the user requests a completion for a prefix
   that has no possible completions, and other quick, unobtrusive
   messages.  */

temp_echo_area_glyphs (m)
     char *m;
{
  int osize = ZV;
  Lisp_Object oinhibit;
  oinhibit = Vinhibit_quit;

  /* Clear out any old echo-area message to make way for our new thing.  */
  message (0);

  SET_PT (osize);
  insert_string (m);
  SET_PT (osize);
  Vinhibit_quit = Qt;
  Fsit_for (make_number (2), Qnil, Qnil);
  del_range (PT, ZV);
  if (!NILP (Vquit_flag))
    {
      Vquit_flag = Qnil;
      Vunread_command_events = Fcons (make_number (quit_char), Qnil);
    }
  Vinhibit_quit = oinhibit;
}

Lisp_Object Fminibuffer_completion_help ();
Lisp_Object assoc_for_completion ();
/* A subroutine of Fintern_soft.  */
extern Lisp_Object oblookup ();


/* Test whether TXT is an exact completion.  */
Lisp_Object
test_completion (txt)
     Lisp_Object txt;
{
  Lisp_Object tem;

  if (CONSP (Vminibuffer_completion_table)
      || NILP (Vminibuffer_completion_table))
    return assoc_for_completion (txt, Vminibuffer_completion_table);
  else if (VECTORP (Vminibuffer_completion_table))
    {
      /* Bypass intern-soft as that loses for nil */
      tem = oblookup (Vminibuffer_completion_table,
		      XSTRING (txt)->data, XSTRING (txt)->size);
      if (!SYMBOLP (tem))
	return Qnil;
      else if (!NILP (Vminibuffer_completion_predicate))
	return call1 (Vminibuffer_completion_predicate, tem);
      else
	return Qt;
    }
  else
    return call3 (Vminibuffer_completion_table, txt,
		  Vminibuffer_completion_predicate, Qlambda);
}

/* returns:
 * 0 no possible completion
 * 1 was already an exact and unique completion
 * 3 was already an exact completion
 * 4 completed to an exact completion
 * 5 some completion happened
 * 6 no completion happened
 */
int
do_completion ()
{
  Lisp_Object completion, tem;
  int completedp;
  Lisp_Object last;
  struct gcpro gcpro1, gcpro2;

  completion = Ftry_completion (Fbuffer_string (), Vminibuffer_completion_table,
				Vminibuffer_completion_predicate);
  last = last_exact_completion;
  last_exact_completion = Qnil;

  GCPRO2 (completion, last);

  if (NILP (completion))
    {
      bitch_at_user ();
      temp_echo_area_glyphs (" [No match]");
      UNGCPRO;
      return 0;
    }

  if (EQ (completion, Qt))	/* exact and unique match */
    {
      UNGCPRO;
      return 1;
    }

  /* compiler bug */
  tem = Fstring_equal (completion, Fbuffer_string());
  if (completedp = NILP (tem))
    {
      Ferase_buffer ();		/* Some completion happened */
      Finsert (1, &completion);
    }

  /* It did find a match.  Do we match some possibility exactly now? */
  tem = test_completion (Fbuffer_string ());
  if (NILP (tem))
    {
      /* not an exact match */
      UNGCPRO;
      if (completedp)
	return 5;
      else if (auto_help)
	Fminibuffer_completion_help ();
      else
	temp_echo_area_glyphs (" [Next char not unique]");
      return 6;
    }
  else if (completedp)
    {
      UNGCPRO;
      return 4;
    }
  /* If the last exact completion and this one were the same,
     it means we've already given a "Complete but not unique"
     message and the user's hit TAB again, so now we give him help.  */
  last_exact_completion = completion;
  if (!NILP (last))
    {
      tem = Fbuffer_string ();
      if (!NILP (Fequal (tem, last)))
	Fminibuffer_completion_help ();
    }
  UNGCPRO;
  return 3;
}

/* Like assoc but assumes KEY is a string, and ignores case if appropriate.  */

Lisp_Object
assoc_for_completion (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;

  if (completion_ignore_case)
    key = Fupcase (key);

  for (tail = list; !NILP (tail); tail = Fcdr (tail))
    {
      register Lisp_Object elt, tem, thiscar;
      elt = Fcar (tail);
      if (!CONSP (elt)) continue;
      thiscar = Fcar (elt);
      if (!STRINGP (thiscar))
	continue;
      if (completion_ignore_case)
	thiscar = Fupcase (thiscar);
      tem = Fequal (thiscar, key);
      if (!NILP (tem)) return elt;
      QUIT;
    }
  return Qnil;
}

DEFUN ("minibuffer-complete", Fminibuffer_complete, Sminibuffer_complete, 0, 0, "",
  "Complete the minibuffer contents as far as possible.\n\
Return nil if there is no valid completion, else t.\n\
If no characters can be completed, display a list of possible completions.\n\
If you repeat this command after it displayed such a list,\n\
scroll the window of possible completions.")
  ()
{
  register int i;
  Lisp_Object window, tem;

  /* If the previous command was not this, then mark the completion
     buffer obsolete.  */
  if (! EQ (last_command, this_command))
    Vminibuf_scroll_window = Qnil;

  window = Vminibuf_scroll_window;
  /* If there's a fresh completion window with a live buffer,
     and this command is repeated, scroll that window.  */
  if (! NILP (window) && ! NILP (XWINDOW (window)->buffer)
      && !NILP (XBUFFER (XWINDOW (window)->buffer)->name))
    {
      struct buffer *obuf = current_buffer;

      Fset_buffer (XWINDOW (window)->buffer);
      tem = Fpos_visible_in_window_p (make_number (ZV), window);
      if (! NILP (tem))
	/* If end is in view, scroll up to the beginning.  */
	Fset_window_start (window, BEGV, Qnil);
      else
	/* Else scroll down one screen.  */
	Fscroll_other_window (Qnil);

      set_buffer_internal (obuf);
      return Qnil;
    }

  i = do_completion ();
  switch (i)
    {
    case 0:
      return Qnil;

    case 1:
      temp_echo_area_glyphs (" [Sole completion]");
      break;

    case 3:
      temp_echo_area_glyphs (" [Complete, but not unique]");
      break;
    }

  return Qt;
}

/* Subroutines of Fminibuffer_complete_and_exit.  */

/* This one is called by internal_condition_case to do the real work.  */

Lisp_Object
complete_and_exit_1 ()
{
  return make_number (do_completion ());
}

/* This one is called by internal_condition_case if an error happens.
   Pretend the current value is an exact match.  */

Lisp_Object
complete_and_exit_2 (ignore)
     Lisp_Object ignore;
{
  return make_number (1);
}

DEFUN ("minibuffer-complete-and-exit", Fminibuffer_complete_and_exit,
        Sminibuffer_complete_and_exit, 0, 0, "",
  "If the minibuffer contents is a valid completion then exit.\n\
Otherwise try to complete it.  If completion leads to a valid completion,\n\
a repetition of this command will exit.")
  ()
{
  register int i;
  Lisp_Object val;

  /* Allow user to specify null string */
  if (BEGV == ZV)
    goto exit;

  if (!NILP (test_completion (Fbuffer_string ())))
    goto exit;

  /* Call do_completion, but ignore errors.  */
  val = internal_condition_case (complete_and_exit_1, Qerror,
				 complete_and_exit_2);

  i = XFASTINT (val);
  switch (i)
    {
    case 1:
    case 3:
      goto exit;

    case 4:
      if (!NILP (Vminibuffer_completion_confirm))
	{
	  temp_echo_area_glyphs (" [Confirm]");
	  return Qnil;
	}
      else
	goto exit;

    default:
      return Qnil;
    }
 exit:
  Fthrow (Qexit, Qnil);
  /* NOTREACHED */
}

DEFUN ("minibuffer-complete-word", Fminibuffer_complete_word, Sminibuffer_complete_word,
  0, 0, "",
  "Complete the minibuffer contents at most a single word.\n\
After one word is completed as much as possible, a space or hyphen\n\
is added, provided that matches some possible completion.\n\
Return nil if there is no valid completion, else t.")
  ()
{
  Lisp_Object completion, tem;
  register int i;
  register unsigned char *completion_string;
  struct gcpro gcpro1, gcpro2;

  /* We keep calling Fbuffer_string rather than arrange for GC to
     hold onto a pointer to one of the strings thus made.  */

  completion = Ftry_completion (Fbuffer_string (),
				Vminibuffer_completion_table,
				Vminibuffer_completion_predicate);
  if (NILP (completion))
    {
      bitch_at_user ();
      temp_echo_area_glyphs (" [No match]");
      return Qnil;
    }
  if (EQ (completion, Qt))
    return Qnil;

#if 0 /* How the below code used to look, for reference. */
  tem = Fbuffer_string ();
  b = XSTRING (tem)->data;
  i = ZV - 1 - XSTRING (completion)->size;
  p = XSTRING (completion)->data;
  if (i > 0 ||
      0 <= scmp (b, p, ZV - 1))
    {
      i = 1;
      /* Set buffer to longest match of buffer tail and completion head. */
      while (0 <= scmp (b + i, p, ZV - 1 - i))
	i++;
      del_range (1, i + 1);
      SET_PT (ZV);
    }
#else /* Rewritten code */
  {
    register unsigned char *buffer_string;
    int buffer_length, completion_length;

    tem = Fbuffer_string ();
    GCPRO2 (completion, tem);
    /* If reading a file name,
       expand any $ENVVAR refs in the buffer and in TEM.  */
    if (EQ (Vminibuffer_completion_table, Qread_file_name_internal))
      {
	Lisp_Object substituted;
	substituted = Fsubstitute_in_file_name (tem);
	if (! EQ (substituted, tem))
	  {
	    tem = substituted;
	    Ferase_buffer ();
	    insert_from_string (tem, 0, XSTRING (tem)->size, 0);
	  }
      }
    buffer_string = XSTRING (tem)->data;
    completion_string = XSTRING (completion)->data;
    buffer_length = XSTRING (tem)->size; /* ie ZV - BEGV */
    completion_length = XSTRING (completion)->size;
    i = buffer_length - completion_length;
    /* Mly: I don't understand what this is supposed to do AT ALL */
    if (i > 0 ||
	0 <= scmp (buffer_string, completion_string, buffer_length))
      {
	/* Set buffer to longest match of buffer tail and completion head. */
	if (i <= 0) i = 1;
	buffer_string += i;
	buffer_length -= i;
	while (0 <= scmp (buffer_string++, completion_string, buffer_length--))
	  i++;
	del_range (1, i + 1);
	SET_PT (ZV);
      }
    UNGCPRO;
  }
#endif /* Rewritten code */
  i = ZV - BEGV;

  /* If completion finds next char not unique,
     consider adding a space or a hyphen. */
  if (i == XSTRING (completion)->size)
    {
      GCPRO1 (completion);
      tem = Ftry_completion (concat2 (Fbuffer_string (), build_string (" ")),
			     Vminibuffer_completion_table,
			     Vminibuffer_completion_predicate);
      UNGCPRO;

      if (STRINGP (tem))
	completion = tem;
      else
	{
	  GCPRO1 (completion);
	  tem =
	    Ftry_completion (concat2 (Fbuffer_string (), build_string ("-")),
			     Vminibuffer_completion_table,
			     Vminibuffer_completion_predicate);
	  UNGCPRO;

	  if (STRINGP (tem))
	    completion = tem;
	}
    }      

  /* Now find first word-break in the stuff found by completion.
     i gets index in string of where to stop completing.  */

  completion_string = XSTRING (completion)->data;

  for (; i < XSTRING (completion)->size; i++)
    if (SYNTAX (completion_string[i]) != Sword) break;
  if (i < XSTRING (completion)->size)
    i = i + 1;

  /* If got no characters, print help for user.  */

  if (i == ZV - BEGV)
    {
      if (auto_help)
	Fminibuffer_completion_help ();
      return Qnil;
    }

  /* Otherwise insert in minibuffer the chars we got */

  Ferase_buffer ();
  insert_from_string (completion, 0, i, 1);
  return Qt;
}

DEFUN ("display-completion-list", Fdisplay_completion_list, Sdisplay_completion_list,
       1, 1, 0,
  "Display the list of completions, COMPLETIONS, using `standard-output'.\n\
Each element may be just a symbol or string\n\
or may be a list of two strings to be printed as if concatenated.\n\
`standard-output' must be a buffer.\n\
At the end, run the normal hook `completion-setup-hook'.\n\
It can find the completion buffer in `standard-output'.")
  (completions)
     Lisp_Object completions;
{
  register Lisp_Object tail, elt;
  register int i;
  int column = 0;
  struct gcpro gcpro1;
  struct buffer *old = current_buffer;
  int first = 1;

  /* Note that (when it matters) every variable
     points to a non-string that is pointed to by COMPLETIONS.  */
  GCPRO1 (completions);

  if (BUFFERP (Vstandard_output))
    set_buffer_internal (XBUFFER (Vstandard_output));

  if (NILP (completions))
    write_string ("There are no possible completions of what you have typed.",
		  -1);
  else
    {
      write_string ("Possible completions are:", -1);
      for (tail = completions, i = 0; !NILP (tail); tail = Fcdr (tail), i++)
	{
	  Lisp_Object tem;
	  int length;

	  elt = Fcar (tail);
	  /* Compute the length of this element.  */
	  if (CONSP (elt))
	    {
	      tem = Fcar (elt);
	      CHECK_STRING (tem, 0);
	      length = XINT (XSTRING (tem)->size);

	      tem = Fcar (Fcdr (elt));
	      CHECK_STRING (tem, 0);
	      length += XINT (XSTRING (tem)->size);
	    }
	  else
	    {
	      CHECK_STRING (elt, 0);
	      length = XINT (XSTRING (elt)->size);
	    }

	  /* This does a bad job for narrower than usual windows.
	     Sadly, the window it will appear in is not known
	     until after the text has been made.  */

	  /* If the previous completion was very wide,
	     or we have two on this line already,
	     don't put another on the same line.  */
	  if (column > 33 || first
	      /* If this is really wide, don't put it second on a line.  */
	      || column > 0 && length > 45)
	    {
	      Fterpri (Qnil);
	      column = 0;
	    }
	  /* Otherwise advance to column 35.  */
	  else
	    {
	      if (BUFFERP (Vstandard_output))
		{
		  tem = Findent_to (make_number (35), make_number (2));
		  column = XINT (tem);
		}
	      else
		{
		  do
		    {
		      write_string (" ", -1);
		      column++;
		    }
		  while (column < 35);
		}
	    }

	  /* Output this element and update COLUMN.  */
	  if (CONSP (elt))
	    {
	      Fprinc (Fcar (elt), Qnil);
	      Fprinc (Fcar (Fcdr (elt)), Qnil);
	    }
	  else
	    Fprinc (elt, Qnil);

	  column += length;

	  /* If output is to a buffer, recompute COLUMN in a way
	     that takes account of character widths.  */
	  if (BUFFERP (Vstandard_output))
	    {
	      tem = Fcurrent_column ();
	      column = XINT (tem);
	    }

	  first = 0;
	}
    }

  UNGCPRO;

  if (BUFFERP (Vstandard_output))
    set_buffer_internal (old);

  if (!NILP (Vrun_hooks))
    call1 (Vrun_hooks, intern ("completion-setup-hook"));

  return Qnil;
}

DEFUN ("minibuffer-completion-help", Fminibuffer_completion_help, Sminibuffer_completion_help,
  0, 0, "",
  "Display a list of possible completions of the current minibuffer contents.")
  ()
{
  Lisp_Object completions;

  message ("Making completion list...");
  completions = Fall_completions (Fbuffer_string (),
				  Vminibuffer_completion_table,
				  Vminibuffer_completion_predicate,
				  Qt);
  echo_area_glyphs = 0;

  if (NILP (completions))
    {
      bitch_at_user ();
      temp_echo_area_glyphs (" [No completions]");
    }
  else
    internal_with_output_to_temp_buffer ("*Completions*",
					 Fdisplay_completion_list,
					 Fsort (completions, Qstring_lessp));
  return Qnil;
}

DEFUN ("self-insert-and-exit", Fself_insert_and_exit, Sself_insert_and_exit, 0, 0, "",
  "Terminate minibuffer input.")
  ()
{
  if (INTEGERP (last_command_char))
    internal_self_insert (last_command_char, 0);
  else
    bitch_at_user ();

  Fthrow (Qexit, Qnil);
}

DEFUN ("exit-minibuffer", Fexit_minibuffer, Sexit_minibuffer, 0, 0, "",
  "Terminate this minibuffer argument.")
  ()
{
  Fthrow (Qexit, Qnil);
}

DEFUN ("minibuffer-depth", Fminibuffer_depth, Sminibuffer_depth, 0, 0, 0,
  "Return current depth of activations of minibuffer, a nonnegative integer.")
  ()
{
  return make_number (minibuf_level);
}

DEFUN ("minibuffer-prompt", Fminibuffer_prompt, Sminibuffer_prompt, 0, 0, 0,
  "Return the prompt string of the currently-active minibuffer.\n\
If no minibuffer is active, return nil.")
  ()
{
  return Fcopy_sequence (minibuf_prompt);
}

DEFUN ("minibuffer-prompt-width", Fminibuffer_prompt_width,
  Sminibuffer_prompt_width, 0, 0, 0,
  "Return the display width of the minibuffer prompt.")
  ()
{
  Lisp_Object width;
  XSETFASTINT (width, minibuf_prompt_width);
  return width;
}

init_minibuf_once ()
{
  Vminibuffer_list = Qnil;
  staticpro (&Vminibuffer_list);
}

syms_of_minibuf ()
{
  minibuf_level = 0;
  minibuf_prompt = Qnil;
  staticpro (&minibuf_prompt);

  minibuf_save_list = Qnil;
  staticpro (&minibuf_save_list);

  Qread_file_name_internal = intern ("read-file-name-internal");
  staticpro (&Qread_file_name_internal);

  Qminibuffer_completion_table = intern ("minibuffer-completion-table");
  staticpro (&Qminibuffer_completion_table);

  Qminibuffer_completion_confirm = intern ("minibuffer-completion-confirm");
  staticpro (&Qminibuffer_completion_confirm);

  Qminibuffer_completion_predicate = intern ("minibuffer-completion-predicate");
  staticpro (&Qminibuffer_completion_predicate);

  staticpro (&last_exact_completion);
  last_exact_completion = Qnil;

  staticpro (&last_minibuf_string);
  last_minibuf_string = Qnil;

  Quser_variable_p = intern ("user-variable-p");
  staticpro (&Quser_variable_p);

  Qminibuffer_history = intern ("minibuffer-history");
  staticpro (&Qminibuffer_history);

  Qminibuffer_setup_hook = intern ("minibuffer-setup-hook");
  staticpro (&Qminibuffer_setup_hook);

  Qminibuffer_exit_hook = intern ("minibuffer-exit-hook");
  staticpro (&Qminibuffer_exit_hook);

  DEFVAR_LISP ("minibuffer-setup-hook", &Vminibuffer_setup_hook, 
    "Normal hook run just after entry to minibuffer.");
  Vminibuffer_setup_hook = Qnil;

  DEFVAR_LISP ("minibuffer-exit-hook", &Vminibuffer_exit_hook,
    "Normal hook run just after exit from minibuffer.");
  Vminibuffer_exit_hook = Qnil;

  DEFVAR_BOOL ("completion-auto-help", &auto_help,
    "*Non-nil means automatically provide help for invalid completion input.");
  auto_help = 1;

  DEFVAR_BOOL ("completion-ignore-case", &completion_ignore_case,
    "Non-nil means don't consider case significant in completion.");
  completion_ignore_case = 0;

  DEFVAR_BOOL ("enable-recursive-minibuffers", &enable_recursive_minibuffers,
    "*Non-nil means to allow minibuffer commands while in the minibuffer.\n\
More precisely, this variable makes a difference when the minibuffer window\n\
is the selected window.  If you are in some other window, minibuffer commands\n\
are allowed even if a minibuffer is active.");
  enable_recursive_minibuffers = 0;

  DEFVAR_LISP ("minibuffer-completion-table", &Vminibuffer_completion_table,
    "Alist or obarray used for completion in the minibuffer.\n\
This becomes the ALIST argument to `try-completion' and `all-completion'.\n\
\n\
The value may alternatively be a function, which is given three arguments:\n\
  STRING, the current buffer contents;\n\
  PREDICATE, the predicate for filtering possible matches;\n\
  CODE, which says what kind of things to do.\n\
CODE can be nil, t or `lambda'.\n\
nil means to return the best completion of STRING, or nil if there is none.\n\
t means to return a list of all possible completions of STRING.\n\
`lambda' means to return t if STRING is a valid completion as it stands.");
  Vminibuffer_completion_table = Qnil;

  DEFVAR_LISP ("minibuffer-completion-predicate", &Vminibuffer_completion_predicate,
    "Within call to `completing-read', this holds the PREDICATE argument.");
  Vminibuffer_completion_predicate = Qnil;

  DEFVAR_LISP ("minibuffer-completion-confirm", &Vminibuffer_completion_confirm,
    "Non-nil => demand confirmation of completion before exiting minibuffer.");
  Vminibuffer_completion_confirm = Qnil;

  DEFVAR_LISP ("minibuffer-help-form", &Vminibuffer_help_form,
    "Value that `help-form' takes on inside the minibuffer.");
  Vminibuffer_help_form = Qnil;

  DEFVAR_LISP ("minibuffer-history-variable", &Vminibuffer_history_variable,
    "History list symbol to add minibuffer values to.\n\
Each minibuffer output is added with\n\
  (set minibuffer-history-variable\n\
       (cons STRING (symbol-value minibuffer-history-variable)))");
  XSETFASTINT (Vminibuffer_history_variable, 0);

  DEFVAR_LISP ("minibuffer-history-position", &Vminibuffer_history_position,
    "Current position of redoing in the history list.");
  Vminibuffer_history_position = Qnil;

  DEFVAR_BOOL ("minibuffer-auto-raise", &minibuffer_auto_raise,
    "*Non-nil means entering the minibuffer raises the minibuffer's frame.");
  minibuffer_auto_raise = 0;

  DEFVAR_LISP ("completion-regexp-list", &Vcompletion_regexp_list,
    "List of regexps that should restrict possible completions.");
  Vcompletion_regexp_list = Qnil;

  defsubr (&Sread_from_minibuffer);
  defsubr (&Seval_minibuffer);
  defsubr (&Sread_minibuffer);
  defsubr (&Sread_string);
  defsubr (&Sread_command);
  defsubr (&Sread_variable);
  defsubr (&Sread_buffer);
  defsubr (&Sread_no_blanks_input);
  defsubr (&Sminibuffer_depth);
  defsubr (&Sminibuffer_prompt);
  defsubr (&Sminibuffer_prompt_width);

  defsubr (&Stry_completion);
  defsubr (&Sall_completions);
  defsubr (&Scompleting_read);
  defsubr (&Sminibuffer_complete);
  defsubr (&Sminibuffer_complete_word);
  defsubr (&Sminibuffer_complete_and_exit);
  defsubr (&Sdisplay_completion_list);
  defsubr (&Sminibuffer_completion_help);

  defsubr (&Sself_insert_and_exit);
  defsubr (&Sexit_minibuffer);

}

keys_of_minibuf ()
{
  initial_define_key (Vminibuffer_local_map, Ctl ('g'),
		      "abort-recursive-edit");
  initial_define_key (Vminibuffer_local_map, Ctl ('m'),
		      "exit-minibuffer");
  initial_define_key (Vminibuffer_local_map, Ctl ('j'),
		      "exit-minibuffer");

  initial_define_key (Vminibuffer_local_ns_map, Ctl ('g'),
		      "abort-recursive-edit");
  initial_define_key (Vminibuffer_local_ns_map, Ctl ('m'),
		      "exit-minibuffer");
  initial_define_key (Vminibuffer_local_ns_map, Ctl ('j'),
		      "exit-minibuffer");

  initial_define_key (Vminibuffer_local_ns_map, ' ',
		      "exit-minibuffer");
  initial_define_key (Vminibuffer_local_ns_map, '\t',
		      "exit-minibuffer");
  initial_define_key (Vminibuffer_local_ns_map, '?',
		      "self-insert-and-exit");

  initial_define_key (Vminibuffer_local_completion_map, Ctl ('g'),
		      "abort-recursive-edit");
  initial_define_key (Vminibuffer_local_completion_map, Ctl ('m'),
		      "exit-minibuffer");
  initial_define_key (Vminibuffer_local_completion_map, Ctl ('j'),
		      "exit-minibuffer");

  initial_define_key (Vminibuffer_local_completion_map, '\t',
		      "minibuffer-complete");
  initial_define_key (Vminibuffer_local_completion_map, ' ',
		      "minibuffer-complete-word");
  initial_define_key (Vminibuffer_local_completion_map, '?',
		      "minibuffer-completion-help");

  initial_define_key (Vminibuffer_local_must_match_map, Ctl ('g'),
		      "abort-recursive-edit");
  initial_define_key (Vminibuffer_local_must_match_map, Ctl ('m'),
		      "minibuffer-complete-and-exit");
  initial_define_key (Vminibuffer_local_must_match_map, Ctl ('j'),
		      "minibuffer-complete-and-exit");
  initial_define_key (Vminibuffer_local_must_match_map, '\t',
		      "minibuffer-complete");
  initial_define_key (Vminibuffer_local_must_match_map, ' ',
		      "minibuffer-complete-word");
  initial_define_key (Vminibuffer_local_must_match_map, '?',
		      "minibuffer-completion-help");
}
