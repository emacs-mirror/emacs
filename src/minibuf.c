/* Minibuffer input and completion.
   Copyright (C) 1985, 1986, 1993, 1994, 1995, 1996, 1997, 1998
         Free Software Foundation, Inc.

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
#include "dispextern.h"
#include "frame.h"
#include "window.h"
#include "syntax.h"
#include "keyboard.h"

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

/* Nonzero means display completion help for invalid input.  */

int auto_help;

/* The maximum length of a minibuffer history.  */

Lisp_Object Qhistory_length, Vhistory_length;

/* Fread_minibuffer leaves the input here as a string. */

Lisp_Object last_minibuf_string;

/* Nonzero means let functions called when within a minibuffer 
   invoke recursive minibuffers (to read arguments, or whatever) */

int enable_recursive_minibuffers;

/* Nonzero means don't ignore text properties
   in Fread_from_minibuffer.  */

int minibuffer_allow_text_properties;

/* help-form is bound to this while in the minibuffer.  */

Lisp_Object Vminibuffer_help_form;

/* Variable which is the history list to add minibuffer values to.  */

Lisp_Object Vminibuffer_history_variable;

/* Current position in the history list (adjusted by M-n and M-p).  */

Lisp_Object Vminibuffer_history_position;

Lisp_Object Qminibuffer_history, Qbuffer_name_history;

Lisp_Object Qread_file_name_internal;

/* Normal hooks for entry to and exit from minibuffer.  */

Lisp_Object Qminibuffer_setup_hook, Vminibuffer_setup_hook;
Lisp_Object Qminibuffer_exit_hook, Vminibuffer_exit_hook;

/* Function to call to read a buffer name.  */
Lisp_Object Vread_buffer_function; 

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

/* Non-nil means it is the window for C-M-v to scroll
   when the minibuffer is selected.  */

extern Lisp_Object Vminibuf_scroll_window;

extern Lisp_Object Voverriding_local_map;

Lisp_Object Quser_variable_p;

Lisp_Object Qminibuffer_default;

Lisp_Object Qcurrent_input_method, Qactivate_input_method;

extern Lisp_Object Qmouse_face;


/* Put minibuf on currently selected frame's minibuffer.
   We do this whenever the user starts a new minibuffer
   or when a minibuffer exits.  */

void
choose_minibuf_frame ()
{
  if (selected_frame != 0
      && !EQ (minibuf_window, selected_frame->minibuffer_window))
    {
      /* I don't think that any frames may validly have a null minibuffer
	 window anymore.  */
      if (NILP (selected_frame->minibuffer_window))
	abort ();

      Fset_window_buffer (selected_frame->minibuffer_window,
			  XWINDOW (minibuf_window)->buffer);
      minibuf_window = selected_frame->minibuffer_window;
    }

  /* Make sure no other frame has a minibuffer as its selected window,
     because the text would not be displayed in it, and that would be
     confusing.  Only allow the selected frame to do this,
     and that only if the minibuffer is active.  */
  {
    Lisp_Object tail, frame;

    FOR_EACH_FRAME (tail, frame)
      if (MINI_WINDOW_P (XWINDOW (FRAME_SELECTED_WINDOW (XFRAME (frame))))
	  && !(XFRAME (frame) == selected_frame
	       && minibuf_level > 0))
	Fset_frame_selected_window (frame, Fframe_first_window (frame));
  }
}

Lisp_Object
choose_minibuf_frame_1 (ignore)
     Lisp_Object ignore;
{
  choose_minibuf_frame ();
  return Qnil;
}

DEFUN ("set-minibuffer-window", Fset_minibuffer_window,
       Sset_minibuffer_window, 1, 1, 0,
  "Specify which minibuffer window to use for the minibuffer.\n\
This effects where the minibuffer is displayed if you put text in it\n\
without invoking the usual minibuffer commands.")
  (window)
     Lisp_Object window;
{
  CHECK_WINDOW (window, 1);
  if (! MINI_WINDOW_P (XWINDOW (window)))
    error ("Window is not a minibuffer window");

  minibuf_window = window;

  return window;
}

#include <stdio.h>


/* Actual minibuffer invocation. */

static Lisp_Object read_minibuf_unwind P_ ((Lisp_Object));
static Lisp_Object read_minibuf P_ ((Lisp_Object, Lisp_Object,
				     Lisp_Object, Lisp_Object,
				     int, Lisp_Object,
				     Lisp_Object, Lisp_Object,
				     int, int));

/* Read from the minibuffer using keymap MAP, initial contents INITIAL
   (a string), putting point minus BACKUP_N bytes from the end of INITIAL,
   prompting with PROMPT (a string), using history list HISTVAR
   with initial position HISTPOS.  (BACKUP_N should be <= 0.)

   Normally return the result as a string (the text that was read),
   but if EXPFLAG is nonzero, read it and return the object read.
   If HISTVAR is given, save the value read on that history only if it doesn't
   match the front of that history list exactly.  The value is pushed onto
   the list as the string that was read.

   DEFALT specifies te default value for the sake of history commands.

   If ALLOW_PROPS is nonzero, we do not throw away text properties.

   if INHERIT_INPUT_METHOD is nonzeor, the minibuffer inherit the
   current input method.  */

static Lisp_Object
read_minibuf (map, initial, prompt, backup_n, expflag,
	      histvar, histpos, defalt, allow_props, inherit_input_method)
     Lisp_Object map;
     Lisp_Object initial;
     Lisp_Object prompt;
     Lisp_Object backup_n;
     int expflag;
     Lisp_Object histvar;
     Lisp_Object histpos;
     Lisp_Object defalt;
     int allow_props;
     int inherit_input_method;
{
  Lisp_Object val;
  int count = specpdl_ptr - specpdl;
  Lisp_Object mini_frame, ambient_dir, minibuffer, input_method;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;
  Lisp_Object enable_multibyte;
  extern Lisp_Object Qinvisible, Qintangible, Qread_only, Qfront_sticky;

  specbind (Qminibuffer_default, defalt);

  single_kboard_state ();

  val = Qnil;
  ambient_dir = current_buffer->directory;
  input_method = Qnil;
  enable_multibyte = Qnil;

  /* Don't need to protect PROMPT, HISTVAR, and HISTPOS because we
     store them away before we can GC.  Don't need to protect
     BACKUP_N because we use the value only if it is an integer.  */
  GCPRO5 (map, initial, val, ambient_dir, input_method);

  if (!STRINGP (prompt))
    prompt = build_string ("");

  if (!enable_recursive_minibuffers
      && minibuf_level > 0)
    {
      if (EQ (selected_window, minibuf_window))
	error ("Command attempted to use minibuffer while in minibuffer");
      else
	/* If we're in another window, cancel the minibuffer that's active.  */
	Fthrow (Qexit,
		build_string ("Command attempted to use minibuffer while in minibuffer"));
    }

  /* Choose the minibuffer window and frame, and take action on them.  */

  choose_minibuf_frame ();

  record_unwind_protect (choose_minibuf_frame_1, Qnil);

  record_unwind_protect (Fset_window_configuration,
			 Fcurrent_window_configuration (Qnil));

  /* If the minibuffer window is on a different frame, save that
     frame's configuration too.  */
  mini_frame = WINDOW_FRAME (XWINDOW (minibuf_window));
  if (XFRAME (mini_frame) != selected_frame)
    record_unwind_protect (Fset_window_configuration,
			   Fcurrent_window_configuration (mini_frame));

  /* If the minibuffer is on an iconified or invisible frame,
     make it visible now.  */
  Fmake_frame_visible (mini_frame);

  if (minibuffer_auto_raise)
    Fraise_frame (mini_frame);

  /* We have to do this after saving the window configuration
     since that is what restores the current buffer.  */

  /* Arrange to restore a number of minibuffer-related variables.
     We could bind each variable separately, but that would use lots of
     specpdl slots.  */
  minibuf_save_list
    = Fcons (Voverriding_local_map,
	     Fcons (minibuf_window, minibuf_save_list));
  minibuf_save_list
    = Fcons (minibuf_prompt,
	     Fcons (make_number (minibuf_prompt_width),
		    Fcons (Vhelp_form,
			   Fcons (Vcurrent_prefix_arg,
				  Fcons (Vminibuffer_history_position,
					 Fcons (Vminibuffer_history_variable,
						minibuf_save_list))))));
  minibuf_save_list
    = Fcons (current_buffer->minibuffer_prompt_length,
	     minibuf_save_list);

  record_unwind_protect (read_minibuf_unwind, Qnil);
  minibuf_level++;

  /* Now that we can restore all those variables, start changing them.  */

  minibuf_prompt_width = 0;	/* xdisp.c puts in the right value.  */
  minibuf_prompt = Fcopy_sequence (prompt);
  Vminibuffer_history_position = histpos;
  Vminibuffer_history_variable = histvar;
  Vhelp_form = Vminibuffer_help_form;

  if (inherit_input_method)
    {
      /* `current-input-method' is buffer local.  So, remeber it in
	 INPUT_METHOD before changing the current buffer.  */
      input_method = Fsymbol_value (Qcurrent_input_method);
      enable_multibyte = current_buffer->enable_multibyte_characters;
    }

  /* Switch to the minibuffer.  */

  minibuffer = get_minibuffer (minibuf_level);
  Fset_buffer (minibuffer);

  /* The current buffer's default directory is usually the right thing
     for our minibuffer here.  However, if you're typing a command at
     a minibuffer-only frame when minibuf_level is zero, then buf IS
     the current_buffer, so reset_buffer leaves buf's default
     directory unchanged.  This is a bummer when you've just started
     up Emacs and buf's default directory is Qnil.  Here's a hack; can
     you think of something better to do?  Find another buffer with a
     better directory, and use that one instead.  */
  if (STRINGP (ambient_dir))
    current_buffer->directory = ambient_dir;
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

  if (XFRAME (mini_frame) != selected_frame)
    Fredirect_frame_focus (Fselected_frame (), mini_frame);

  Vminibuf_scroll_window = selected_window;
  Fset_window_buffer (minibuf_window, Fcurrent_buffer ());
  Fselect_window (minibuf_window);
  XSETFASTINT (XWINDOW (minibuf_window)->hscroll, 0);

  Fmake_local_variable (Qprint_escape_newlines);
  print_escape_newlines = 1;
  XSETFASTINT (current_buffer->minibuffer_prompt_length, 0);

  /* Erase the buffer.  */
  {
    int count1 = specpdl_ptr - specpdl;
    specbind (Qinhibit_read_only, Qt);
    Ferase_buffer ();
    unbind_to (count1, Qnil);
  }

  if (!NILP (current_buffer->enable_multibyte_characters)
      && ! STRING_MULTIBYTE (minibuf_prompt))
    minibuf_prompt = Fstring_make_multibyte (minibuf_prompt);

  /* Insert the prompt, record where it ends.  */
  Finsert (1, &minibuf_prompt);
  XSETFASTINT (current_buffer->minibuffer_prompt_length, PT);
  if (PT > BEG)
    {
      Fput_text_property (make_number (BEG), make_number (PT - 1),
			  Qfront_sticky, Qt, Qnil);
      Fput_text_property (make_number (BEG), make_number (PT - 1),
			  Qread_only, Qt, Qnil);
      Fput_text_property (make_number (PT - 1), make_number (Z),
			  Qrear_nonsticky, Qt, Qnil);
    }
      
  /* If appropriate, copy enable-multibyte-characters into the minibuffer.  */
  if (inherit_input_method)
    current_buffer->enable_multibyte_characters = enable_multibyte;

  /* Put in the initial input.  */
  if (!NILP (initial))
    {
      Finsert (1, &initial);
      if (INTEGERP (backup_n))
	Fforward_char (backup_n);
    }

  clear_message (1, 1);
  current_buffer->keymap = map;

  /* Turn on an input method stored in INPUT_METHOD if any.  */
  if (STRINGP (input_method) && !NILP (Ffboundp (Qactivate_input_method)))
    call1 (Qactivate_input_method, input_method);

  /* Run our hook, but not if it is empty.
     (run-hooks would do nothing if it is empty,
     but it's important to save time here in the usual case).  */
  if (!NILP (Vminibuffer_setup_hook) && !EQ (Vminibuffer_setup_hook, Qunbound)
      && !NILP (Vrun_hooks))
    call1 (Vrun_hooks, Qminibuffer_setup_hook);

  recursive_edit_1 ();

  /* If cursor is on the minibuffer line,
     show the user we have exited by putting it in column 0.  */
  if (XWINDOW (minibuf_window)->cursor.vpos >= 0
      && !noninteractive)
    {
      XWINDOW (minibuf_window)->cursor.hpos = 0;
      XWINDOW (minibuf_window)->cursor.x = 0;
      XWINDOW (minibuf_window)->must_be_updated_p = 1;
      update_frame (selected_frame, 1, 1);
      if (rif && rif->flush_display)
	rif->flush_display (XFRAME (XWINDOW (minibuf_window)->frame));
    }

  /* Make minibuffer contents into a string.  */
  Fset_buffer (minibuffer);
  val = make_buffer_string (1, Z, allow_props);

  /* VAL is the string of minibuffer text.  */

  last_minibuf_string = val;

  /* Add the value to the appropriate history list unless it is empty.  */
  if (XSTRING (val)->size != 0
      && SYMBOLP (Vminibuffer_history_variable))
    {
      /* If the caller wanted to save the value read on a history list,
	 then do so if the value is not already the front of the list.  */
      Lisp_Object histval;

      /* If variable is unbound, make it nil.  */
      if (EQ (XSYMBOL (Vminibuffer_history_variable)->value, Qunbound))
	Fset (Vminibuffer_history_variable, Qnil);

      histval = Fsymbol_value (Vminibuffer_history_variable);

      /* The value of the history variable must be a cons or nil.  Other
	 values are unacceptable.  We silently ignore these values.  */
      if (NILP (histval)
	  || (CONSP (histval)
	      && NILP (Fequal (last_minibuf_string, Fcar (histval)))))
	{
	  Lisp_Object length;

	  histval = Fcons (last_minibuf_string, histval);
	  Fset (Vminibuffer_history_variable, histval);

	  /* Truncate if requested.  */
	  length = Fget (Vminibuffer_history_variable, Qhistory_length);
	  if (NILP (length)) length = Vhistory_length;
	  if (INTEGERP (length))
	    {
	      if (XINT (length) <= 0)
		Fset (Vminibuffer_history_variable, Qnil);
	      else
		{
		  Lisp_Object temp;

		  temp = Fnthcdr (Fsub1 (length), histval);
		  if (CONSP (temp)) Fsetcdr (temp, Qnil);
		}
	    }
	}
    }

  /* If Lisp form desired instead of string, parse it. */
  if (expflag)
    {
      Lisp_Object expr_and_pos;
      unsigned char *p;
      int pos;

      if (STRINGP (val) && XSTRING (val)->size == 0
	  && STRINGP (defalt))
	val = defalt;

      expr_and_pos = Fread_from_string (val, Qnil, Qnil);
      pos = XINT (Fcdr (expr_and_pos));
      if (pos != XSTRING (val)->size)
	{
	  /* Ignore trailing whitespace; any other trailing junk is an error.  */
	  int i;
	  pos = string_char_to_byte (val, pos);
	  for (i = pos; i < STRING_BYTES (XSTRING (val)); i++)
	    {
	      int c = XSTRING (val)->data[i];
	      if (c != ' ' && c != '\t' && c != '\n')
		error ("Trailing garbage following expression");
	    }
	}
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

/* This function is called on exiting minibuffer, whether normally or
   not, and it restores the current window, buffer, etc. */

static Lisp_Object
read_minibuf_unwind (data)
     Lisp_Object data;
{
  Lisp_Object old_deactivate_mark;
  Lisp_Object window;

  /* We are exiting the minibuffer one way or the other,
     so run the hook.  */
  if (!NILP (Vminibuffer_exit_hook) && !EQ (Vminibuffer_exit_hook, Qunbound)
      && !NILP (Vrun_hooks))
    safe_run_hooks (Qminibuffer_exit_hook);

  /* If this was a recursive minibuffer,
     tie the minibuffer window back to the outer level minibuffer buffer.  */
  minibuf_level--;

  window = minibuf_window;
  /* To keep things predictable, in case it matters, let's be in the
     minibuffer when we reset the relevant variables.  */
  Fset_buffer (XWINDOW (window)->buffer);

  /* Restore prompt, etc, from outer minibuffer level.  */
  current_buffer->minibuffer_prompt_length = Fcar (minibuf_save_list);
  minibuf_save_list = Fcdr (minibuf_save_list);

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
#if 0
  temp = Fcar (minibuf_save_list);
  if (FRAME_LIVE_P (XFRAME (WINDOW_FRAME (XWINDOW (temp)))))
    minibuf_window = temp;
#endif
  minibuf_save_list = Fcdr (minibuf_save_list);

  /* Erase the minibuffer we were using at this level.  */
  {
    int count = specpdl_ptr - specpdl;
    /* Prevent error in erase-buffer.  */
    specbind (Qinhibit_read_only, Qt);
    old_deactivate_mark = Vdeactivate_mark;
    Ferase_buffer ();
    Vdeactivate_mark = old_deactivate_mark;
    unbind_to (count, Qnil);
  }

  /* When we get to the outmost level, make sure we resize the
     mini-window back to its normal size.  */
  if (minibuf_level == 0)
    resize_mini_window (XWINDOW (window));

  /* Make sure minibuffer window is erased, not ignored.  */
  windows_or_buffers_changed++;
  XSETFASTINT (XWINDOW (window)->last_modified, 0);
  XSETFASTINT (XWINDOW (window)->last_overlay_modified, 0);
  return Qnil;
}


/* This comment supplies the doc string for read-from-minibuffer, 
   for make-docfile to see.  We cannot put this in the real DEFUN
   due to limits in the Unix cpp.

DEFUN ("read-from-minibuffer", Fread_from_minibuffer, Sread_from_minibuffer, 1, 7, 0,
  "Read a string from the minibuffer, prompting with string PROMPT.\n\
If optional second arg INITIAL-CONTENTS is non-nil, it is a string\n\
  to be inserted into the minibuffer before reading input.\n\
  If INITIAL-CONTENTS is (STRING . POSITION), the initial input\n\
  is STRING, but point is placed at position POSITION in the minibuffer.\n\
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
  Positions are counted starting from 1 at the beginning of the list.\n\
Sixth arg DEFAULT-VALUE is the default value.  If non-nil, it is available\n\
 for history commands; but `read-from-minibuffer' does NOT return DEFAULT-VALUE\n\
 if the user enters empty input!  It returns the empty string.\n\
Seventh arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits\n\
 the current input method and the setting of enable-multibyte-characters.\n\
If the variable `minibuffer-allow-text-properties' is non-nil,\n\
 then the string which is returned includes whatever text properties\n\
 were present in the minibuffer.  Otherwise the value has no text properties.")
  (prompt, initial_contents, keymap, read, hist, default_value, inherit_input_method)
  */

DEFUN ("read-from-minibuffer", Fread_from_minibuffer, Sread_from_minibuffer, 1, 7, 0,
  0 /* See immediately above */)
  (prompt, initial_contents, keymap, read, hist, default_value, inherit_input_method)
     Lisp_Object prompt, initial_contents, keymap, read, hist, default_value;
     Lisp_Object inherit_input_method;
{
  int pos = 0;
  Lisp_Object histvar, histpos, position, val;
  struct gcpro gcpro1;

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
	  if (XINT (position) < 1)
	    /* A number too small means the beginning of the string.  */
	    pos =  - XSTRING (initial_contents)->size;
	  else
	    pos = XINT (position) - 1 - XSTRING (initial_contents)->size;
	}
    }

  if (NILP (keymap))
    keymap = Vminibuffer_local_map;
  else
    keymap = get_keymap (keymap);

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

  GCPRO1 (default_value);
  val = read_minibuf (keymap, initial_contents, prompt,
		      make_number (pos), !NILP (read),
		      histvar, histpos, default_value,
		      minibuffer_allow_text_properties,
		      !NILP (inherit_input_method));
  UNGCPRO;
  return val;
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
		       prompt, Qnil, 1, Qminibuffer_history,
		       make_number (0), Qnil, 0, 0);
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

DEFUN ("read-string", Fread_string, Sread_string, 1, 5, 0,
  "Read a string from the minibuffer, prompting with string PROMPT.\n\
If non-nil, second arg INITIAL-INPUT is a string to insert before reading.\n\
The third arg HISTORY, if non-nil, specifies a history list\n\
  and optionally the initial position in the list.\n\
See `read-from-minibuffer' for details of HISTORY argument.\n\
Fourth arg DEFAULT-VALUE is the default value.  If non-nil, it is used\n\
 for history commands, and as the value to return if the user enters\n\
 the empty string.\n\
Fifth arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits\n\
 the current input method and the setting of enable-multibyte-characters.")
  (prompt, initial_input, history, default_value, inherit_input_method)
     Lisp_Object prompt, initial_input, history, default_value;
     Lisp_Object inherit_input_method;
{
  Lisp_Object val;
  val = Fread_from_minibuffer (prompt, initial_input, Qnil,
			       Qnil, history, default_value,
			       inherit_input_method);
  if (STRINGP (val) && XSTRING (val)->size == 0 && ! NILP (default_value))
    val = default_value;
  return val;
}

DEFUN ("read-no-blanks-input", Fread_no_blanks_input, Sread_no_blanks_input, 1, 3, 0,
  "Read a string from the terminal, not allowing blanks.\n\
Prompt with PROMPT, and provide INITIAL as an initial value of the input string.\n\
Third arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits\n\
the current input method and the setting of enable-multibyte-characters.")
  (prompt, initial, inherit_input_method)
     Lisp_Object prompt, initial, inherit_input_method;
{
  CHECK_STRING (prompt, 0);
  if (! NILP (initial))
    CHECK_STRING (initial, 1);

  return read_minibuf (Vminibuffer_local_ns_map, initial, prompt, Qnil,
		       0, Qminibuffer_history, make_number (0), Qnil, 0,
		       !NILP (inherit_input_method));
}

DEFUN ("read-command", Fread_command, Sread_command, 1, 2, 0,
  "Read the name of a command and return as a symbol.\n\
Prompts with PROMPT.  By default, return DEFAULT-VALUE.")
  (prompt, default_value)
     Lisp_Object prompt, default_value;
{
  Lisp_Object name, default_string;

  if (NILP (default_value))
    default_string = Qnil;
  else if (SYMBOLP (default_value))
    XSETSTRING (default_string, XSYMBOL (default_value)->name);
  else
    default_string = default_value;
    
  name = Fcompleting_read (prompt, Vobarray, Qcommandp, Qt,
			   Qnil, Qnil, default_string, Qnil);
  if (NILP (name))
    return name;
  return Fintern (name, Qnil);
}

#ifdef NOTDEF
DEFUN ("read-function", Fread_function, Sread_function, 1, 1, 0,
  "One arg PROMPT, a string.  Read the name of a function and return as a symbol.\n\
Prompts with PROMPT.")
  (prompt)
     Lisp_Object prompt;
{
  return Fintern (Fcompleting_read (prompt, Vobarray, Qfboundp, Qt, Qnil, Qnil, Qnil, Qnil),
		  Qnil);
}
#endif /* NOTDEF */

DEFUN ("read-variable", Fread_variable, Sread_variable, 1, 2, 0,
  "Read the name of a user variable and return it as a symbol.\n\
Prompts with PROMPT.  By default, return DEFAULT-VALUE.\n\
A user variable is one whose documentation starts with a `*' character.")
  (prompt, default_value)
     Lisp_Object prompt, default_value;
{
  Lisp_Object name, default_string;

  if (NILP (default_value))
    default_string = Qnil;
  else if (SYMBOLP (default_value))
    XSETSTRING (default_string, XSYMBOL (default_value)->name);
  else
    default_string = default_value;
    
  name = Fcompleting_read (prompt, Vobarray,
			   Quser_variable_p, Qt,
			   Qnil, Qnil, default_string, Qnil);
  if (NILP (name))
    return name;
  return Fintern (name, Qnil);
}

DEFUN ("read-buffer", Fread_buffer, Sread_buffer, 1, 3, 0,
  "One arg PROMPT, a string.  Read the name of a buffer and return as a string.\n\
Prompts with PROMPT.\n\
Optional second arg DEF is value to return if user enters an empty line.\n\
If optional third arg REQUIRE-MATCH is non-nil, only existing buffer names are allowed.")
  (prompt, def, require_match)
     Lisp_Object prompt, def, require_match;
{
  Lisp_Object tem;
  Lisp_Object args[4];
  
  if (BUFFERP (def))
    def = XBUFFER (def)->name;

  if (NILP (Vread_buffer_function))
    {
      if (!NILP (def))
	{
	  args[0] = build_string ("%s(default %s) ");
	  args[1] = prompt;
	  args[2] = def;
	  prompt = Fformat (3, args);
	}

      return Fcompleting_read (prompt, Vbuffer_alist, Qnil,
			       require_match, Qnil, Qbuffer_name_history,
			       def, Qnil);
    }
  else
    {
      args[0] = Vread_buffer_function;
      args[1] = prompt;
      args[2] = def;
      args[3] = require_match;
      return Ffuncall(4, args);
    }
}

static Lisp_Object
minibuf_conform_representation (string, basis)
     Lisp_Object string, basis;
{
  if (STRING_MULTIBYTE (string) == STRING_MULTIBYTE (basis))
    return string;

  if (STRING_MULTIBYTE (string))
    return Fstring_make_unibyte (string);
  else
    return Fstring_make_multibyte (string);
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
  (string, alist, predicate)
     Lisp_Object string, alist, predicate;
{
  Lisp_Object bestmatch, tail, elt, eltstring;
  /* Size in bytes of BESTMATCH.  */
  int bestmatchsize;
  /* These are in bytes, too.  */
  int compare, matchsize;
  int list = CONSP (alist) || NILP (alist);
  int index, obsize;
  int matchcount = 0;
  Lisp_Object bucket, zero, end, tem;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  CHECK_STRING (string, 0);
  if (!list && !VECTORP (alist))
    return call3 (alist, string, predicate, Qnil);

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
	  && (tem = Fcompare_strings (eltstring, make_number (0),
				      make_number (XSTRING (string)->size),
				      string, make_number (0), Qnil,
				      completion_ignore_case ?Qt : Qnil),
	      EQ (Qt, tem)))
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

	  if (!NILP (predicate))
	    {
	      if (EQ (predicate, Qcommandp))
		tem = Fcommandp (elt);
	      else
		{
		  GCPRO4 (tail, string, eltstring, bestmatch);
		  tem = call1 (predicate, elt);
		  UNGCPRO;
		}
	      if (NILP (tem)) continue;
	    }

	  /* Update computation of how much all possible completions match */

	  matchcount++;
	  if (NILP (bestmatch))
	    {
	      bestmatch = eltstring;
	      bestmatchsize = XSTRING (eltstring)->size;
	    }
	  else
	    {
	      compare = min (bestmatchsize, XSTRING (eltstring)->size);
	      tem = Fcompare_strings (bestmatch, make_number (0),
				      make_number (compare),
				      eltstring, make_number (0),
				      make_number (compare),
				      completion_ignore_case ? Qt : Qnil);
	      if (EQ (tem, Qt))
		matchsize = compare;
	      else if (XINT (tem) < 0)
		matchsize = - XINT (tem) - 1;
	      else
		matchsize = XINT (tem) - 1;

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
		       && (tem = Fcompare_strings (eltstring, make_number (0),
						   make_number (XSTRING (string)->size),
						   string, make_number (0),
						   Qnil,
						   Qnil),
			   EQ (Qt, tem))
		       && (tem = Fcompare_strings (bestmatch, make_number (0),
						   make_number (XSTRING (string)->size),
						   string, make_number (0),
						   Qnil,
						   Qnil),
			   ! EQ (Qt, tem))))
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
    return minibuf_conform_representation (string, bestmatch);

  /* Return t if the supplied string is an exact match (counting case);
     it does not require any change to be made.  */
  if (matchcount == 1 && bestmatchsize == XSTRING (string)->size
      && (tem = Fcompare_strings (bestmatch, make_number (0),
				  make_number (bestmatchsize),
				  string, make_number (0),
				  make_number (bestmatchsize),
				  Qnil),
	  EQ (Qt, tem)))
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
  register unsigned char *start = s1;

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
    {
      int match = len - l;

      /* Now *--S1 is the unmatching byte.  If it is in the middle of
         multi-byte form, we must say that the multi-byte character
         there doesn't match.  */
      while (match && *--s1 >= 0xA0) match--;
      return match;
    }
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
  (string, alist, predicate, hide_spaces)
     Lisp_Object string, alist, predicate, hide_spaces;
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
      return call3 (alist, string, predicate, Qt);
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
	  && ((STRING_BYTES (XSTRING (string)) > 0
	       && XSTRING (string)->data[0] == ' ')
	      || XSTRING (eltstring)->data[0] != ' '
	      || NILP (hide_spaces))
	  && (tem = Fcompare_strings (eltstring, make_number (0),
				      make_number (XSTRING (string)->size),
				      string, make_number (0),
				      make_number (XSTRING (string)->size),
				      completion_ignore_case ? Qt : Qnil),
	      EQ (Qt, tem)))
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

	  if (!NILP (predicate))
	    {
	      if (EQ (predicate, Qcommandp))
		tem = Fcommandp (elt);
	      else
		{
		  GCPRO4 (tail, eltstring, allmatches, string);
		  tem = call1 (predicate, elt);
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
Lisp_Object Vminibuffer_completing_file_name;

/* This comment supplies the doc string for completing-read,
   for make-docfile to see.  We cannot put this in the real DEFUN
   due to limits in the Unix cpp.

DEFUN ("completing-read", Fcompleting_read, Scompleting_read, 2, 8, 0,
  "Read a string in the minibuffer, with completion.\n\
PROMPT is a string to prompt with; normally it ends in a colon and a space.\n\
TABLE is an alist whose elements' cars are strings, or an obarray.\n\
PREDICATE limits completion to a subset of TABLE.\n\
See `try-completion' and `all-completions' for more details\n\
 on completion, TABLE, and PREDICATE.\n\
\n\
If REQUIRE-MATCH is non-nil, the user is not allowed to exit unless\n\
 the input is (or completes to) an element of TABLE or is null.\n\
 If it is also not t, Return does not exit if it does non-null completion.\n\
If the input is null, `completing-read' returns an empty string,\n\
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
  which INITIAL-INPUT corresponds to).\n\
  Positions are counted starting from 1 at the beginning of the list.\n\
DEF, if non-nil, is the default value.\n\
\n\
If INHERIT-INPUT-METHOD is non-nil, the minibuffer inherits\n\
  the current input method and the setting of enable-multibyte-characters.\n\
\n\
Completion ignores case if the ambient value of\n\
  `completion-ignore-case' is non-nil."
*/
DEFUN ("completing-read", Fcompleting_read, Scompleting_read, 2, 8, 0,
  0 /* See immediately above */)
  (prompt, table, predicate, require_match, init, hist, def, inherit_input_method)
     Lisp_Object prompt, table, predicate, require_match, init, hist, def;
     Lisp_Object inherit_input_method;
{
  Lisp_Object val, histvar, histpos, position;
  int pos = 0;
  int count = specpdl_ptr - specpdl;
  struct gcpro gcpro1;
  int disable_multibyte = ! NILP (Vminibuffer_completing_file_name); 

  GCPRO1 (def);

  specbind (Qminibuffer_completion_table, table);
  specbind (Qminibuffer_completion_predicate, predicate);
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
		      histvar, histpos, def, 0,
		      !NILP (inherit_input_method));

  if (STRINGP (val) && XSTRING (val)->size == 0 && ! NILP (def))
    val = def;

  RETURN_UNGCPRO (unbind_to (count, val));
}

Lisp_Object Fminibuffer_completion_help ();
Lisp_Object assoc_for_completion ();

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
		      XSTRING (txt)->data,
		      XSTRING (txt)->size,
		      STRING_BYTES (XSTRING (txt)));
      if (!SYMBOLP (tem))
	{
	  if (STRING_MULTIBYTE (txt))
	    txt = Fstring_make_unibyte (txt);
	  else
	    txt = Fstring_make_multibyte (txt);

	  tem = oblookup (Vminibuffer_completion_table,
			  XSTRING (txt)->data,
			  XSTRING (txt)->size,
			  STRING_BYTES (XSTRING (txt)));
	  if (!SYMBOLP (tem))
	    return Qnil;
	}
      if (!NILP (Vminibuffer_completion_predicate))
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

  for (tail = list; !NILP (tail); tail = Fcdr (tail))
    {
      register Lisp_Object elt, tem, thiscar;
      elt = Fcar (tail);
      if (!CONSP (elt)) continue;
      thiscar = Fcar (elt);
      if (!STRINGP (thiscar))
	continue;
      tem = Fcompare_strings (thiscar, make_number (0), Qnil,
			      key, make_number (0), Qnil,
			      completion_ignore_case ? Qt : Qnil);
      if (EQ (tem, Qt))
	return elt;
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

  /* If the previous command was not this,
     mark the completion buffer obsolete.  */
  if (! EQ (current_kboard->Vlast_command, Vthis_command))
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
	Fset_window_start (window, make_number (BEGV), Qnil);
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
  if (XFASTINT (current_buffer->minibuffer_prompt_length) == ZV)
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
  Lisp_Object completion, tem, tem1;
  register int i, i_byte;
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
    int buffer_nchars, completion_nchars;

    CHECK_STRING (completion, 0);
    tem = Fbuffer_string ();
    GCPRO2 (completion, tem);
    /* If reading a file name,
       expand any $ENVVAR refs in the buffer and in TEM.  */
    if (! NILP (Vminibuffer_completing_file_name))
      {
	Lisp_Object substituted;
	substituted = Fsubstitute_in_file_name (tem);
	if (! EQ (substituted, tem))
	  {
	    tem = substituted;
	    Ferase_buffer ();
	    insert_from_string (tem, 0, 0, XSTRING (tem)->size,
				STRING_BYTES (XSTRING (tem)), 0);
	  }
      }
    buffer_nchars = XSTRING (tem)->size; /* ie ZV - BEGV */
    completion_nchars = XSTRING (completion)->size;
    i = buffer_nchars - completion_nchars;
    if (i > 0
	||
	(tem1 = Fcompare_strings (tem, make_number (0),
				  make_number (buffer_nchars),
				  completion, make_number (0),
				  make_number (buffer_nchars),
				  completion_ignore_case ? Qt : Qnil),
	 ! EQ (tem1, Qt)))
      {
	int start_pos;

	/* Set buffer to longest match of buffer tail and completion head.  */
	if (i <= 0) i = 1;
	start_pos= i;
	buffer_nchars -= i;
	while (i > 0)
	  {
	    tem1 = Fcompare_strings (tem, make_number (start_pos), Qnil,
				     completion, make_number (0),
				     make_number (buffer_nchars),
				     completion_ignore_case ? Qt : Qnil);
	    start_pos++;
	    if (EQ (tem1, Qt))
	      break;
	    i++;
	    buffer_nchars--;
	  }
	del_range (1, i + 1);
	SET_PT_BOTH (ZV, ZV_BYTE);
      }
    UNGCPRO;
  }
#endif /* Rewritten code */
  
  {
    int prompt_end_charpos, prompt_end_bytepos;
    prompt_end_charpos = XFASTINT (current_buffer->minibuffer_prompt_length);
    prompt_end_bytepos = CHAR_TO_BYTE (prompt_end_charpos);
    i = ZV - prompt_end_charpos;
    i_byte = ZV_BYTE - prompt_end_bytepos;
  }

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
  {
    int len, c;

    completion_string = XSTRING (completion)->data;
    for (; i_byte < STRING_BYTES (XSTRING (completion)); i_byte += len, i++)
      {
	c = STRING_CHAR_AND_LENGTH (completion_string + i_byte,
				    XSTRING (completion)->size - i_byte,
				    len);
	if (SYNTAX (c) != Sword)
	  {
	    i_byte += len;
	    i++;
	    break;
	  }
      }
  }

  /* If got no characters, print help for user.  */

  if (i == ZV - XFASTINT (current_buffer->minibuffer_prompt_length))
    {
      if (auto_help)
	Fminibuffer_completion_help ();
      return Qnil;
    }

  /* Otherwise insert in minibuffer the chars we got */

  Ferase_buffer ();
  insert_from_string (completion, 0, 0, i, i_byte, 1);
  return Qt;
}

DEFUN ("display-completion-list", Fdisplay_completion_list, Sdisplay_completion_list,
       1, 1, 0,
  "Display the list of completions, COMPLETIONS, using `standard-output'.\n\
Each element may be just a symbol or string\n\
or may be a list of two strings to be printed as if concatenated.\n\
`standard-output' must be a buffer.\n\
The actual completion alternatives, as inserted, are given `mouse-face'\n\
properties of `highlight'.\n\
At the end, this runs the normal hook `completion-setup-hook'.\n\
It can find the completion buffer in `standard-output'.")
  (completions)
     Lisp_Object completions;
{
  Lisp_Object tail, elt;
  register int i;
  int column = 0;
  struct gcpro gcpro1, gcpro2;
  struct buffer *old = current_buffer;
  int first = 1;

  /* Note that (when it matters) every variable
     points to a non-string that is pointed to by COMPLETIONS,
     except for ELT.  ELT can be pointing to a string
     when terpri or Findent_to calls a change hook.  */
  elt = Qnil;
  GCPRO2 (completions, elt);

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
	  Lisp_Object tem, string;
	  int length;
	  Lisp_Object startpos, endpos;

	  elt = Fcar (tail);
	  /* Compute the length of this element.  */
	  if (CONSP (elt))
	    {
	      tem = XCAR (elt);
	      CHECK_STRING (tem, 0);
	      length = XSTRING (tem)->size;

	      tem = Fcar (XCDR (elt));
	      CHECK_STRING (tem, 0);
	      length += XSTRING (tem)->size;
	    }
	  else
	    {
	      CHECK_STRING (elt, 0);
	      length = XSTRING (elt)->size;
	    }

	  /* This does a bad job for narrower than usual windows.
	     Sadly, the window it will appear in is not known
	     until after the text has been made.  */

	  if (BUFFERP (Vstandard_output))
	    XSETINT (startpos, BUF_PT (XBUFFER (Vstandard_output)));

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

	  if (BUFFERP (Vstandard_output))
	    {
	      XSETINT (endpos, BUF_PT (XBUFFER (Vstandard_output)));
	      Fset_text_properties (startpos, endpos,
				    Qnil, Vstandard_output);
	    }

	  /* Output this element.
	     If necessary, convert it to unibyte or to multibyte first.  */
	  if (CONSP (elt))
	    string = Fcar (elt);
	  else
	    string = elt;
	  if (NILP (current_buffer->enable_multibyte_characters)
	      && STRING_MULTIBYTE (string))
	    string = Fstring_make_unibyte (string);
	  else if (!NILP (current_buffer->enable_multibyte_characters)
		   && !STRING_MULTIBYTE (string))
	    string = Fstring_make_multibyte (string);

	  if (BUFFERP (Vstandard_output))
	    {
	      XSETINT (startpos, BUF_PT (XBUFFER (Vstandard_output)));

	      Fprinc (string, Qnil);

	      XSETINT (endpos, BUF_PT (XBUFFER (Vstandard_output)));

	      Fput_text_property (startpos, endpos,
				  Qmouse_face, intern ("highlight"),
				  Vstandard_output);
	    }
	  else
	    {
	      Fprinc (string, Qnil);
	    }

	  /* Output the annotation for this element.  */
	  if (CONSP (elt))
	    {
	      if (BUFFERP (Vstandard_output))
		{
		  XSETINT (startpos, BUF_PT (XBUFFER (Vstandard_output)));

		  Fprinc (Fcar (Fcdr (elt)), Qnil);

		  XSETINT (endpos, BUF_PT (XBUFFER (Vstandard_output)));

		  Fset_text_properties (startpos, endpos, Qnil,
					Vstandard_output);
		}
	      else
		{
		  Fprinc (Fcar (Fcdr (elt)), Qnil);
		}
	    }


	  /* Update COLUMN for what we have output.  */
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
  clear_message (1, 0);

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
    internal_self_insert (XINT (last_command_char), 0);
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
  "Return the display width of the minibuffer prompt.\n\
Return 0 if current buffer is not a mini-buffer.")
  ()
{
  Lisp_Object width;
  if (NILP (current_buffer->minibuffer_prompt_length))
    width = make_number (0);
  else
    width = make_number (current_buffer->minibuffer_prompt_length);
  return width;
}


DEFUN ("minibuffer-prompt-end", Fminibuffer_prompt_end,
       Sminibuffer_prompt_end, 0, 0, 0,
  "Return the end buffer position of the mini-buffer prompt.\n\
Value is 0 if current buffer is not a mini-buffer.")
     ()
{
  return (NILP (current_buffer->minibuffer_prompt_length)
	  ? make_number (0)
	  : make_number (current_buffer->minibuffer_prompt_length));
}



/* Temporarily display the string M at the end of the current
   minibuffer contents.  This is used to display things like
   "[No Match]" when the user requests a completion for a prefix
   that has no possible completions, and other quick, unobtrusive
   messages.  */

void
temp_echo_area_glyphs (m)
     char *m;
{
  int osize = ZV;
  int osize_byte = ZV_BYTE;
  int opoint = PT;
  int opoint_byte = PT_BYTE;
  Lisp_Object oinhibit;
  oinhibit = Vinhibit_quit;

  /* Clear out any old echo-area message to make way for our new thing.  */
  message (0);

  SET_PT_BOTH (osize, osize_byte);
  insert_string (m);
  SET_PT_BOTH (opoint, opoint_byte);
  Vinhibit_quit = Qt;
  Fsit_for (make_number (2), Qnil, Qnil);
  del_range_both (osize, osize_byte, ZV, ZV_BYTE, 1);
  SET_PT_BOTH (opoint, opoint_byte);
  if (!NILP (Vquit_flag))
    {
      Vquit_flag = Qnil;
      Vunread_command_events = Fcons (make_number (quit_char), Qnil);
    }
  Vinhibit_quit = oinhibit;
}

DEFUN ("minibuffer-message", Fminibuffer_message, Sminibuffer_message,
  1, 1, 0,
  "Temporarily display STRING at the end of the minibuffer.\n\
The text is displayed for two seconds,\n\
or until the next input event arrives, whichever comes first.")
  (string)
     Lisp_Object string;
{
  temp_echo_area_glyphs (XSTRING (string)->data);
  return Qnil;
}

void
init_minibuf_once ()
{
  Vminibuffer_list = Qnil;
  staticpro (&Vminibuffer_list);
}

void
syms_of_minibuf ()
{
  minibuf_level = 0;
  minibuf_prompt = Qnil;
  staticpro (&minibuf_prompt);

  minibuf_save_list = Qnil;
  staticpro (&minibuf_save_list);

  Qread_file_name_internal = intern ("read-file-name-internal");
  staticpro (&Qread_file_name_internal);

  Qminibuffer_default = intern ("minibuffer-default");
  staticpro (&Qminibuffer_default);
  Fset (Qminibuffer_default, Qnil);

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

  Qbuffer_name_history = intern ("buffer-name-history");
  staticpro (&Qbuffer_name_history);
  Fset (Qbuffer_name_history, Qnil);

  Qminibuffer_setup_hook = intern ("minibuffer-setup-hook");
  staticpro (&Qminibuffer_setup_hook);

  Qminibuffer_exit_hook = intern ("minibuffer-exit-hook");
  staticpro (&Qminibuffer_exit_hook);

  Qhistory_length = intern ("history-length");
  staticpro (&Qhistory_length);

  Qcurrent_input_method = intern ("current-input-method");
  staticpro (&Qcurrent_input_method);

  Qactivate_input_method = intern ("activate-input-method");
  staticpro (&Qactivate_input_method);

  DEFVAR_LISP ("read-buffer-function", &Vread_buffer_function, 
    "If this is non-nil, `read-buffer' does its work by calling this function.");
  Vread_buffer_function = Qnil;

  DEFVAR_LISP ("minibuffer-setup-hook", &Vminibuffer_setup_hook, 
    "Normal hook run just after entry to minibuffer.");
  Vminibuffer_setup_hook = Qnil;

  DEFVAR_LISP ("minibuffer-exit-hook", &Vminibuffer_exit_hook,
    "Normal hook run just after exit from minibuffer.");
  Vminibuffer_exit_hook = Qnil;

  DEFVAR_LISP ("history-length", &Vhistory_length,
    "*Maximum length for history lists before truncation takes place.\n\
A number means that length; t means infinite.  Truncation takes place\n\
just after a new element is inserted.  Setting the history-length\n\
property of a history variable overrides this default.");
  XSETFASTINT (Vhistory_length, 30);

  DEFVAR_BOOL ("completion-auto-help", &auto_help,
    "*Non-nil means automatically provide help for invalid completion input.");
  auto_help = 1;

  DEFVAR_BOOL ("completion-ignore-case", &completion_ignore_case,
    "Non-nil means don't consider case significant in completion.");
  completion_ignore_case = 0;

  DEFVAR_BOOL ("enable-recursive-minibuffers", &enable_recursive_minibuffers,
    "*Non-nil means to allow minibuffer commands while in the minibuffer.\n\
This variable makes a difference whenever the minibuffer window is active.");
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

  DEFVAR_LISP ("minibuffer-completing-file-name",
	       &Vminibuffer_completing_file_name,
    "Non-nil means completing file names.");
  Vminibuffer_completing_file_name = Qnil;

  DEFVAR_LISP ("minibuffer-help-form", &Vminibuffer_help_form,
    "Value that `help-form' takes on inside the minibuffer.");
  Vminibuffer_help_form = Qnil;

  DEFVAR_LISP ("minibuffer-history-variable", &Vminibuffer_history_variable,
    "History list symbol to add minibuffer values to.\n\
Each string of minibuffer input, as it appears on exit from the minibuffer,\n\
is added with\n\
  (set minibuffer-history-variable\n\
       (cons STRING (symbol-value minibuffer-history-variable)))");
  XSETFASTINT (Vminibuffer_history_variable, 0);

  DEFVAR_LISP ("minibuffer-history-position", &Vminibuffer_history_position,
    "Current position of redoing in the history list.");
  Vminibuffer_history_position = Qnil;

  DEFVAR_BOOL ("minibuffer-auto-raise", &minibuffer_auto_raise,
    "*Non-nil means entering the minibuffer raises the minibuffer's frame.\n\
Some uses of the echo area also raise that frame (since they use it too).");
  minibuffer_auto_raise = 0;

  DEFVAR_LISP ("completion-regexp-list", &Vcompletion_regexp_list,
    "List of regexps that should restrict possible completions.");
  Vcompletion_regexp_list = Qnil;

  DEFVAR_BOOL ("minibuffer-allow-text-properties",
	       &minibuffer_allow_text_properties,
    "Non-nil means `read-from-minibuffer' should not discard text properties.\n\
This also affects `read-string', but it does not affect `read-minibuffer',\n\
`read-no-blanks-input', or any of the functions that do minibuffer input\n\
with completion; they always discard text properties.");
  minibuffer_allow_text_properties = 0;

  defsubr (&Sset_minibuffer_window);
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
  defsubr (&Sminibuffer_prompt_end);

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

  defsubr (&Sminibuffer_message);
}

void
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
