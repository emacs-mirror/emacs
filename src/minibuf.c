/* Minibuffer input and completion.
   Copyright (C) 1985, 1986, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
   2000, 2001 Free Software Foundation, Inc.

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
#include <stdio.h>

#include "lisp.h"
#include "commands.h"
#include "buffer.h"
#include "charset.h"
#include "dispextern.h"
#include "keyboard.h"
#include "frame.h"
#include "window.h"
#include "syntax.h"
#include "intervals.h"
#include "keymap.h"

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

Lisp_Object Vcompletion_auto_help;

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

/* Text properties that are added to minibuffer prompts.
   These are in addition to the basic `field' property, and stickiness
   properties.  */

Lisp_Object Vminibuffer_prompt_properties;

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

extern Lisp_Object Voverriding_local_map;

Lisp_Object Quser_variable_p;

Lisp_Object Qminibuffer_default;

Lisp_Object Qcurrent_input_method, Qactivate_input_method;

extern Lisp_Object Qmouse_face;

extern Lisp_Object Qfield;

/* Put minibuf on currently selected frame's minibuffer.
   We do this whenever the user starts a new minibuffer
   or when a minibuffer exits.  */

void
choose_minibuf_frame ()
{
  if (FRAMEP (selected_frame)
      && FRAME_LIVE_P (XFRAME (selected_frame))
      && !EQ (minibuf_window, XFRAME (selected_frame)->minibuffer_window))
    {
      struct frame *sf = XFRAME (selected_frame);
      Lisp_Object buffer;
      
      /* I don't think that any frames may validly have a null minibuffer
	 window anymore.  */
      if (NILP (sf->minibuffer_window))
	abort ();

      /* Under X, we come here with minibuf_window being the
	 minibuffer window of the unused termcap window created in
	 init_window_once.  That window doesn't have a buffer.  */
      buffer = XWINDOW (minibuf_window)->buffer;
      if (BUFFERP (buffer))
	Fset_window_buffer (sf->minibuffer_window, buffer);
      minibuf_window = sf->minibuffer_window;
    }

  /* Make sure no other frame has a minibuffer as its selected window,
     because the text would not be displayed in it, and that would be
     confusing.  Only allow the selected frame to do this,
     and that only if the minibuffer is active.  */
  {
    Lisp_Object tail, frame;

    FOR_EACH_FRAME (tail, frame)
      if (MINI_WINDOW_P (XWINDOW (FRAME_SELECTED_WINDOW (XFRAME (frame))))
	  && !(EQ (frame, selected_frame)
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
       doc: /* Specify which minibuffer window to use for the minibuffer.
This effects where the minibuffer is displayed if you put text in it
without invoking the usual minibuffer commands.  */)
     (window)
     Lisp_Object window;
{
  CHECK_WINDOW (window);
  if (! MINI_WINDOW_P (XWINDOW (window)))
    error ("Window is not a minibuffer window");

  minibuf_window = window;

  return window;
}


/* Actual minibuffer invocation. */

static Lisp_Object read_minibuf_unwind P_ ((Lisp_Object));
static Lisp_Object read_minibuf P_ ((Lisp_Object, Lisp_Object,
				     Lisp_Object, Lisp_Object,
				     int, Lisp_Object,
				     Lisp_Object, Lisp_Object,
				     int, int));
static Lisp_Object read_minibuf_noninteractive P_ ((Lisp_Object, Lisp_Object,
						    Lisp_Object, Lisp_Object,
						    int, Lisp_Object,
						    Lisp_Object, Lisp_Object,
						    int, int));
static Lisp_Object string_to_object P_ ((Lisp_Object, Lisp_Object));


/* Read a Lisp object from VAL and return it.  If VAL is an empty
   string, and DEFALT is a string, read from DEFALT instead of VAL.  */

static Lisp_Object
string_to_object (val, defalt)
     Lisp_Object val, defalt;
{
  struct gcpro gcpro1, gcpro2;
  Lisp_Object expr_and_pos;
  int pos;
      
  GCPRO2 (val, defalt);
      
  if (STRINGP (val) && XSTRING (val)->size == 0
      && STRINGP (defalt))
    val = defalt;
      
  expr_and_pos = Fread_from_string (val, Qnil, Qnil);
  pos = XINT (Fcdr (expr_and_pos));
  if (pos != XSTRING (val)->size)
    {
      /* Ignore trailing whitespace; any other trailing junk
	 is an error.  */
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
  RETURN_UNGCPRO (val);
}


/* Like read_minibuf but reading from stdin.  This function is called
   from read_minibuf to do the job if noninteractive.  */

static Lisp_Object
read_minibuf_noninteractive (map, initial, prompt, backup_n, expflag,
			     histvar, histpos, defalt, allow_props,
			     inherit_input_method)
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
  int size, len;
  char *line, *s;
  Lisp_Object val;

  fprintf (stdout, "%s", XSTRING (prompt)->data);
  fflush (stdout);

  val = Qnil;
  size = 100;
  len = 0;
  line = (char *) xmalloc (size * sizeof *line);
  while ((s = fgets (line + len, size - len, stdin)) != NULL
	 && (len = strlen (line),
	     len == size - 1 && line[len - 1] != '\n'))
    {
      size *= 2;
      line = (char *) xrealloc (line, size);
    }

  if (s)
    {
      len = strlen (line);
      
      if (len > 0 && line[len - 1] == '\n')
	line[--len] = '\0';
      
      val = build_string (line);
      xfree (line);
    }
  else
    {
      xfree (line);
      error ("Error reading from stdin");
    }
  
  /* If Lisp form desired instead of string, parse it. */
  if (expflag)
    val = string_to_object (val, defalt);
  
  return val;
}


DEFUN ("minibuffer-prompt-end", Fminibuffer_prompt_end,
       Sminibuffer_prompt_end, 0, 0, 0,
       doc: /* Return the buffer position of the end of the minibuffer prompt.
Return (point-min) if current buffer is not a mini-buffer.  */)
     ()
{
  /* This function is written to be most efficient when there's a prompt.  */
  Lisp_Object beg = make_number (BEGV);
  Lisp_Object end = Ffield_end (beg, Qnil, Qnil);
  
  if (XINT (end) == ZV && NILP (Fget_char_property (beg, Qfield, Qnil)))
    return beg;
  else
    return end;
}

DEFUN ("minibuffer-contents", Fminibuffer_contents,
       Sminibuffer_contents, 0, 0, 0,
       doc: /* Return the user input in a minibuffer as a string.
The current buffer must be a minibuffer.  */)
     ()
{
  int prompt_end = XINT (Fminibuffer_prompt_end ());
  return make_buffer_string (prompt_end, ZV, 1);
}

DEFUN ("minibuffer-contents-no-properties", Fminibuffer_contents_no_properties,
       Sminibuffer_contents_no_properties, 0, 0, 0,
       doc: /* Return the user input in a minibuffer as a string, without text-properties.
The current buffer must be a minibuffer.  */)
     ()
{
  int prompt_end = XINT (Fminibuffer_prompt_end ());
  return make_buffer_string (prompt_end, ZV, 0);
}

DEFUN ("delete-minibuffer-contents", Fdelete_minibuffer_contents,
       Sdelete_minibuffer_contents, 0, 0, 0,
       doc: /* Delete all user input in a minibuffer.
The current buffer must be a minibuffer.  */)
     ()
{
  int prompt_end = XINT (Fminibuffer_prompt_end ());
  if (prompt_end < ZV)
    del_range (prompt_end, ZV);
  return Qnil;
}


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
  extern Lisp_Object Qfront_sticky;
  extern Lisp_Object Qrear_nonsticky;

  specbind (Qminibuffer_default, defalt);

  single_kboard_state ();
#ifdef HAVE_X_WINDOWS
  if (display_hourglass_p)
    cancel_hourglass ();
#endif

  val = Qnil;
  ambient_dir = current_buffer->directory;
  input_method = Qnil;
  enable_multibyte = Qnil;

  /* Don't need to protect PROMPT, HISTVAR, and HISTPOS because we
     store them away before we can GC.  Don't need to protect
     BACKUP_N because we use the value only if it is an integer.  */
  GCPRO5 (map, initial, val, ambient_dir, input_method);

  if (!STRINGP (prompt))
    prompt = empty_string;

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

  if (noninteractive)
    {
      val = read_minibuf_noninteractive (map, initial, prompt, backup_n,
					 expflag, histvar, histpos, defalt,
					 allow_props, inherit_input_method);
      return unbind_to (count, val);
    }

  /* Choose the minibuffer window and frame, and take action on them.  */

  choose_minibuf_frame ();

  record_unwind_protect (choose_minibuf_frame_1, Qnil);

  record_unwind_protect (Fset_window_configuration,
			 Fcurrent_window_configuration (Qnil));

  /* If the minibuffer window is on a different frame, save that
     frame's configuration too.  */
  mini_frame = WINDOW_FRAME (XWINDOW (minibuf_window));
  if (!EQ (mini_frame, selected_frame))
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

  record_unwind_protect (read_minibuf_unwind, Qnil);
  minibuf_level++;

  /* Now that we can restore all those variables, start changing them.  */

  minibuf_prompt_width = 0;
  minibuf_prompt = Fcopy_sequence (prompt);
  Vminibuffer_history_position = histpos;
  Vminibuffer_history_variable = histvar;
  Vhelp_form = Vminibuffer_help_form;

  if (inherit_input_method)
    {
      /* `current-input-method' is buffer local.  So, remember it in
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
	   buf_list = XCDR (buf_list))
	{
	  Lisp_Object other_buf;

	  other_buf = XCDR (XCAR (buf_list));
	  if (STRINGP (XBUFFER (other_buf)->directory))
	    {
	      current_buffer->directory = XBUFFER (other_buf)->directory;
	      break;
	    }
	}
    }

  if (!EQ (mini_frame, selected_frame))
    Fredirect_frame_focus (selected_frame, mini_frame);

  Vminibuf_scroll_window = selected_window;
  if (minibuf_level == 1 || !EQ (minibuf_window, selected_window))
    minibuf_selected_window = selected_window;
  Fset_window_buffer (minibuf_window, Fcurrent_buffer ());
  Fselect_window (minibuf_window);
  XSETFASTINT (XWINDOW (minibuf_window)->hscroll, 0);

  Fmake_local_variable (Qprint_escape_newlines);
  print_escape_newlines = 1;

  /* Erase the buffer.  */
  {
    int count1 = BINDING_STACK_SIZE ();
    specbind (Qinhibit_read_only, Qt);
    specbind (Qinhibit_modification_hooks, Qt);
    Ferase_buffer ();
    unbind_to (count1, Qnil);
  }

  if (!NILP (current_buffer->enable_multibyte_characters)
      && ! STRING_MULTIBYTE (minibuf_prompt))
    minibuf_prompt = Fstring_make_multibyte (minibuf_prompt);

  /* Insert the prompt, record where it ends.  */
  Finsert (1, &minibuf_prompt);
  if (PT > BEG)
    {
      Fput_text_property (make_number (BEG), make_number (PT),
			  Qfront_sticky, Qt, Qnil);
      Fput_text_property (make_number (BEG), make_number (PT),
			  Qrear_nonsticky, Qt, Qnil);
      Fput_text_property (make_number (BEG), make_number (PT),
			  Qfield, Qt, Qnil);
      Fadd_text_properties (make_number (BEG), make_number (PT),
			    Vminibuffer_prompt_properties, Qnil);
    }
  
  minibuf_prompt_width = current_column ();
      
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
     but it's important to save time here in the usual case.)  */
  if (!NILP (Vminibuffer_setup_hook) && !EQ (Vminibuffer_setup_hook, Qunbound)
      && !NILP (Vrun_hooks))
    call1 (Vrun_hooks, Qminibuffer_setup_hook);

  /* Don't allow the user to undo past this point.  */
  current_buffer->undo_list = Qnil;

  recursive_edit_1 ();

  /* If cursor is on the minibuffer line,
     show the user we have exited by putting it in column 0.  */
  if (XWINDOW (minibuf_window)->cursor.vpos >= 0
      && !noninteractive)
    {
      XWINDOW (minibuf_window)->cursor.hpos = 0;
      XWINDOW (minibuf_window)->cursor.x = 0;
      XWINDOW (minibuf_window)->must_be_updated_p = 1;
      update_frame (XFRAME (selected_frame), 1, 1);
      if (rif && rif->flush_display)
	rif->flush_display (XFRAME (XWINDOW (minibuf_window)->frame));
    }

  /* Make minibuffer contents into a string.  */
  Fset_buffer (minibuffer);
  if (allow_props)
    val = Fminibuffer_contents ();
  else
    val = Fminibuffer_contents_no_properties ();

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
      if (EQ (SYMBOL_VALUE (Vminibuffer_history_variable), Qunbound))
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
    val = string_to_object (val, defalt);

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

      XSETCAR (tail, buf);
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
    specbind (Qinhibit_modification_hooks, Qt);
    old_deactivate_mark = Vdeactivate_mark;
    Ferase_buffer ();
    Vdeactivate_mark = old_deactivate_mark;
    unbind_to (count, Qnil);
  }

  /* When we get to the outmost level, make sure we resize the
     mini-window back to its normal size.  */
  if (minibuf_level == 0)
    resize_mini_window (XWINDOW (window), 0);

  /* Make sure minibuffer window is erased, not ignored.  */
  windows_or_buffers_changed++;
  XSETFASTINT (XWINDOW (window)->last_modified, 0);
  XSETFASTINT (XWINDOW (window)->last_overlay_modified, 0);
  return Qnil;
}


DEFUN ("read-from-minibuffer", Fread_from_minibuffer, Sread_from_minibuffer, 1, 7, 0,
       doc: /* Read a string from the minibuffer, prompting with string PROMPT.
If optional second arg INITIAL-CONTENTS is non-nil, it is a string
  to be inserted into the minibuffer before reading input.
  If INITIAL-CONTENTS is (STRING . POSITION), the initial input
  is STRING, but point is placed at position POSITION in the minibuffer.
Third arg KEYMAP is a keymap to use whilst reading;
  if omitted or nil, the default is `minibuffer-local-map'.
If fourth arg READ is non-nil, then interpret the result as a Lisp object
  and return that object:
  in other words, do `(car (read-from-string INPUT-STRING))'
Fifth arg HIST, if non-nil, specifies a history list
  and optionally the initial position in the list.
  It can be a symbol, which is the history list variable to use,
  or it can be a cons cell (HISTVAR . HISTPOS).
  In that case, HISTVAR is the history list variable to use,
  and HISTPOS is the initial position (the position in the list
  which INITIAL-CONTENTS corresponds to).
  Positions are counted starting from 1 at the beginning of the list.
Sixth arg DEFAULT-VALUE is the default value.  If non-nil, it is available
 for history commands; but `read-from-minibuffer' does NOT return DEFAULT-VALUE
 if the user enters empty input!  It returns the empty string.
Seventh arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
 the current input method and the setting of `enable-multibyte-characters'.
If the variable `minibuffer-allow-text-properties' is non-nil,
 then the string which is returned includes whatever text properties
 were present in the minibuffer.  Otherwise the value has no text properties.  */)
     (prompt, initial_contents, keymap, read, hist, default_value, inherit_input_method)
     Lisp_Object prompt, initial_contents, keymap, read, hist, default_value;
     Lisp_Object inherit_input_method;
{
  int pos = 0;
  Lisp_Object histvar, histpos, position, val;
  struct gcpro gcpro1;

  position = Qnil;

  CHECK_STRING (prompt);
  if (!NILP (initial_contents))
    {
      if (CONSP (initial_contents))
	{
	  position = Fcdr (initial_contents);
	  initial_contents = Fcar (initial_contents);
	}
      CHECK_STRING (initial_contents);
      if (!NILP (position))
	{
	  CHECK_NUMBER (position);
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
    keymap = get_keymap (keymap, 1, 0);

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
       doc: /* Return a Lisp object read using the minibuffer.
Prompt with PROMPT.  If non-nil, optional second arg INITIAL-CONTENTS
is a string to insert in the minibuffer before reading.  */)
     (prompt, initial_contents)
     Lisp_Object prompt, initial_contents;
{
  CHECK_STRING (prompt);
  if (!NILP (initial_contents))
    CHECK_STRING (initial_contents);
  return read_minibuf (Vminibuffer_local_map, initial_contents,
		       prompt, Qnil, 1, Qminibuffer_history,
		       make_number (0), Qnil, 0, 0);
}

DEFUN ("eval-minibuffer", Feval_minibuffer, Seval_minibuffer, 1, 2, 0,
       doc: /* Return value of Lisp expression read using the minibuffer.
Prompt with PROMPT.  If non-nil, optional second arg INITIAL-CONTENTS
is a string to insert in the minibuffer before reading.  */)
     (prompt, initial_contents)
     Lisp_Object prompt, initial_contents;
{
  return Feval (Fread_minibuffer (prompt, initial_contents));
}

/* Functions that use the minibuffer to read various things. */

DEFUN ("read-string", Fread_string, Sread_string, 1, 5, 0,
       doc: /* Read a string from the minibuffer, prompting with string PROMPT.
If non-nil, second arg INITIAL-INPUT is a string to insert before reading.
The third arg HISTORY, if non-nil, specifies a history list
  and optionally the initial position in the list.
See `read-from-minibuffer' for details of HISTORY argument.
Fourth arg DEFAULT-VALUE is the default value.  If non-nil, it is used
 for history commands, and as the value to return if the user enters
 the empty string.
Fifth arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
 the current input method and the setting of `enable-multibyte-characters'.  */)
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
       doc: /* Read a string from the terminal, not allowing blanks.
Prompt with PROMPT, and provide INITIAL as an initial value of the input string.
Third arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
the current input method and the setting of `enable-multibyte-characters'.  */)
     (prompt, initial, inherit_input_method)
     Lisp_Object prompt, initial, inherit_input_method;
{
  CHECK_STRING (prompt);
  if (! NILP (initial))
    CHECK_STRING (initial);

  return read_minibuf (Vminibuffer_local_ns_map, initial, prompt, Qnil,
		       0, Qminibuffer_history, make_number (0), Qnil, 0,
		       !NILP (inherit_input_method));
}

DEFUN ("read-command", Fread_command, Sread_command, 1, 2, 0,
       doc: /* Read the name of a command and return as a symbol.
Prompt with PROMPT.  By default, return DEFAULT-VALUE.  */)
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
       doc: /* One arg PROMPT, a string.  Read the name of a function and return as a symbol.
Prompt with PROMPT.  */)
     (prompt)
     Lisp_Object prompt;
{
  return Fintern (Fcompleting_read (prompt, Vobarray, Qfboundp, Qt, Qnil, Qnil, Qnil, Qnil),
		  Qnil);
}
#endif /* NOTDEF */

DEFUN ("read-variable", Fread_variable, Sread_variable, 1, 2, 0,
       doc: /* Read the name of a user variable and return it as a symbol.
Prompt with PROMPT.  By default, return DEFAULT-VALUE.
A user variable is one whose documentation starts with a `*' character.  */)
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
       doc: /* Read the name of a buffer and return as a string.
Prompt with PROMPT.
Optional second arg DEF is value to return if user enters an empty line.
If optional third arg REQUIRE-MATCH is non-nil,
 only existing buffer names are allowed.  */)
     (prompt, def, require_match)
     Lisp_Object prompt, def, require_match;
{
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
       doc: /* Return common substring of all completions of STRING in ALIST.
Each car of each element of ALIST is tested to see if it begins with STRING.
All that match are compared together; the longest initial sequence
common to all matches is returned as a string.
If there is no match at all, nil is returned.
For a unique match which is exact, t is returned.

ALIST can be an obarray instead of an alist.
Then the print names of all symbols in the obarray are the possible matches.

ALIST can also be a function to do the completion itself.
It receives three arguments: the values STRING, PREDICATE and nil.
Whatever it returns becomes the value of `try-completion'.

If optional third argument PREDICATE is non-nil,
it is used to test each possible match.
The match is a candidate only if PREDICATE returns non-nil.
The argument given to PREDICATE is the alist element
or the symbol from the obarray.
Additionally to this predicate, `completion-regexp-list'
is used to further constrain the set of candidates.  */)
     (string, alist, predicate)
     Lisp_Object string, alist, predicate;
{
  Lisp_Object bestmatch, tail, elt, eltstring;
  /* Size in bytes of BESTMATCH.  */
  int bestmatchsize = 0;
  /* These are in bytes, too.  */
  int compare, matchsize;
  int list = NILP (alist) || (CONSP (alist)
			      && (!SYMBOLP (XCAR (alist))
				  || NILP (XCAR (alist))));
  int index = 0, obsize = 0;
  int matchcount = 0;
  Lisp_Object bucket, zero, end, tem;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  CHECK_STRING (string);
  if (!list && !VECTORP (alist))
    return call3 (alist, string, predicate, Qnil);

  bestmatch = bucket = Qnil;

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
	  if (!CONSP (tail))
	    break;
	  elt = XCAR (tail);
	  eltstring = CONSP (elt) ? XCAR (elt) : elt;
	  tail = XCDR (tail);
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
	       regexps = XCDR (regexps))
	    {
	      tem = Fstring_match (XCAR (regexps), eltstring, zero);
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
		tem = Fcommandp (elt, Qnil);
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
		/* When can this happen ?  -stef  */
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
	      if (matchsize <= XSTRING (string)->size
		  && matchcount > 1)
		/* No need to look any further.  */
		break;
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

DEFUN ("all-completions", Fall_completions, Sall_completions, 2, 4, 0,
       doc: /* Search for partial matches to STRING in ALIST.
Each car of each element of ALIST is tested to see if it begins with STRING.
The value is a list of all the strings from ALIST that match.

ALIST can be an obarray instead of an alist.
Then the print names of all symbols in the obarray are the possible matches.

ALIST can also be a function to do the completion itself.
It receives three arguments: the values STRING, PREDICATE and t.
Whatever it returns becomes the value of `all-completions'.

If optional third argument PREDICATE is non-nil,
it is used to test each possible match.
The match is a candidate only if PREDICATE returns non-nil.
The argument given to PREDICATE is the alist element
or the symbol from the obarray.
Additionally to this predicate, `completion-regexp-list'
is used to further constrain the set of candidates.

If the optional fourth argument HIDE-SPACES is non-nil,
strings in ALIST that start with a space
are ignored unless STRING itself starts with a space.  */)
     (string, alist, predicate, hide_spaces)
     Lisp_Object string, alist, predicate, hide_spaces;
{
  Lisp_Object tail, elt, eltstring;
  Lisp_Object allmatches;
  int list = NILP (alist) || (CONSP (alist)
			      && (!SYMBOLP (XCAR (alist))
				  || NILP (XCAR (alist))));
  int index = 0, obsize = 0;
  Lisp_Object bucket, tem;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  CHECK_STRING (string);
  if (!list && !VECTORP (alist))
    {
      return call3 (alist, string, predicate, Qt);
    }
  allmatches = bucket = Qnil;

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
	  if (!CONSP (tail))
	    break;
	  elt = XCAR (tail);
	  eltstring = CONSP (elt) ? XCAR (elt) : elt;
	  tail = XCDR (tail);
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
	       regexps = XCDR (regexps))
	    {
	      tem = Fstring_match (XCAR (regexps), eltstring, zero);
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
		tem = Fcommandp (elt, Qnil);
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

DEFUN ("completing-read", Fcompleting_read, Scompleting_read, 2, 8, 0,
       doc: /* Read a string in the minibuffer, with completion.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
TABLE is an alist whose elements' cars are strings, or an obarray.
TABLE can also be a function to do the completion itself.
PREDICATE limits completion to a subset of TABLE.
See `try-completion' and `all-completions' for more details
 on completion, TABLE, and PREDICATE.

If REQUIRE-MATCH is non-nil, the user is not allowed to exit unless
 the input is (or completes to) an element of TABLE or is null.
 If it is also not t, typing RET does not exit if it does non-null completion.
If the input is null, `completing-read' returns an empty string,
 regardless of the value of REQUIRE-MATCH.

If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
  If it is (STRING . POSITION), the initial input
  is STRING, but point is placed POSITION characters into the string.
  This feature is deprecated--it is best to pass nil for INITIAL-INPUT
  and supply the default value DEF instead.  The user can yank the
  default value into the minibuffer easily using \\[next-history-element].
  
HIST, if non-nil, specifies a history list
  and optionally the initial position in the list.
  It can be a symbol, which is the history list variable to use,
  or it can be a cons cell (HISTVAR . HISTPOS).
  In that case, HISTVAR is the history list variable to use,
  and HISTPOS is the initial position (the position in the list
  which INITIAL-INPUT corresponds to).
  Positions are counted starting from 1 at the beginning of the list.
DEF, if non-nil, is the default value.

If INHERIT-INPUT-METHOD is non-nil, the minibuffer inherits
  the current input method and the setting of `enable-multibyte-characters'.

Completion ignores case if the ambient value of
  `completion-ignore-case' is non-nil.  */)
     (prompt, table, predicate, require_match, initial_input, hist, def, inherit_input_method)
     Lisp_Object prompt, table, predicate, require_match, initial_input;
     Lisp_Object hist, def, inherit_input_method;
{
  Lisp_Object val, histvar, histpos, position;
  Lisp_Object init;
  int pos = 0;
  int count = specpdl_ptr - specpdl;
  struct gcpro gcpro1;

  init = initial_input;
  GCPRO1 (def);

  specbind (Qminibuffer_completion_table, table);
  specbind (Qminibuffer_completion_predicate, predicate);
  specbind (Qminibuffer_completion_confirm,
	    EQ (require_match, Qt) ? Qnil : require_match);
  last_exact_completion = Qnil;

  position = Qnil;
  if (!NILP (init))
    {
      if (CONSP (init))
	{
	  position = Fcdr (init);
	  init = Fcar (init);
	}
      CHECK_STRING (init);
      if (!NILP (position))
	{
	  CHECK_NUMBER (position);
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
Lisp_Object Fassoc_string ();

/* Test whether TXT is an exact completion.  */
DEFUN ("test-completion", Ftest_completion, Stest_completion, 2, 3, 0,
       doc: /* Return non-nil if STRING is a valid completion.
Takes the same arguments as `all-completions' and `try-completion'.
If ALIST is a function, it is called with three arguments:
the values STRING, PREDICATE and `lambda'.  */)
       (string, alist, predicate)
     Lisp_Object string, alist, predicate;
{
  Lisp_Object regexps, tem = Qnil;

  CHECK_STRING (string);

  if ((CONSP (alist) && (!SYMBOLP (XCAR (alist)) || NILP (XCAR (alist))))
      || NILP (alist))
    {
      tem = Fassoc_string (string, alist, completion_ignore_case ? Qt : Qnil);
      if (CONSP (tem))
	tem = XCAR (tem);
      else
	return Qnil;
    }
  else if (VECTORP (alist))
    {
      /* Bypass intern-soft as that loses for nil.  */
      tem = oblookup (alist,
		      XSTRING (string)->data,
		      XSTRING (string)->size,
		      STRING_BYTES (XSTRING (string)));
      if (!SYMBOLP (tem))
	{
	  if (STRING_MULTIBYTE (string))
	    string = Fstring_make_unibyte (string);
	  else
	    string = Fstring_make_multibyte (string);

	  tem = oblookup (Vminibuffer_completion_table,
			  XSTRING (string)->data,
			  XSTRING (string)->size,
			  STRING_BYTES (XSTRING (string)));
	  if (!SYMBOLP (tem))
	    return Qnil;
	}
    }
  else
    return call3 (alist, string, predicate, Qlambda);

  /* Reject this element if it fails to match all the regexps.  */
  for (regexps = Vcompletion_regexp_list; CONSP (regexps);
       regexps = XCDR (regexps))
    {
      if (NILP (Fstring_match (XCAR (regexps),
			       SYMBOLP (tem) ? string : tem,
			       Qnil)))
	return Qnil;
    }

  /* Finally, check the predicate.  */
  if (!NILP (predicate))
    return call1 (predicate, tem);
  else
    return Qt;
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
  Lisp_Object completion, string, tem;
  int completedp;
  Lisp_Object last;
  struct gcpro gcpro1, gcpro2;

  completion = Ftry_completion (Fminibuffer_contents (),
				Vminibuffer_completion_table,
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

  string = Fminibuffer_contents ();

  /* COMPLETEDP should be true if some completion was done, which
     doesn't include simply changing the case of the entered string.
     However, for appearance, the string is rewritten if the case
     changes.  */
  tem = Fcompare_strings (completion, Qnil, Qnil, string, Qnil, Qnil, Qt);
  completedp = !EQ (tem, Qt);

  tem = Fcompare_strings (completion, Qnil, Qnil, string, Qnil, Qnil, Qnil);
  if (!EQ (tem, Qt))
    /* Rewrite the user's input.  */
    {
      Fdelete_minibuffer_contents (); /* Some completion happened */
      Finsert (1, &completion);

      if (! completedp)
	/* The case of the string changed, but that's all.  We're not
	   sure whether this is a unique completion or not, so try again
	   using the real case (this shouldn't recurse again, because
	   the next time try-completion will return either `t' or the
	   exact string).  */
	{
	  UNGCPRO;
	  return do_completion ();
	}
    }

  /* It did find a match.  Do we match some possibility exactly now? */
  tem = Ftest_completion (Fminibuffer_contents (),
			  Vminibuffer_completion_table,
			  Vminibuffer_completion_predicate);
  if (NILP (tem))
    {
      /* not an exact match */
      UNGCPRO;
      if (completedp)
	return 5;
      else if (!NILP (Vcompletion_auto_help))
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
      tem = Fminibuffer_contents ();
      if (!NILP (Fequal (tem, last)))
	Fminibuffer_completion_help ();
    }
  UNGCPRO;
  return 3;
}

/* Like assoc but assumes KEY is a string, and ignores case if appropriate.  */

DEFUN ("assoc-string", Fassoc_string, Sassoc_string, 2, 3, 0,
       doc: /* Like `assoc' but specifically for strings.
Unibyte strings are converted to multibyte for comparison.
And case is ignored if CASE-FOLD is non-nil.
As opposed to `assoc', it will also match an entry consisting of a single
string rather than a cons cell whose car is a string.  */)
       (key, list, case_fold)
     register Lisp_Object key;
     Lisp_Object list, case_fold;
{
  register Lisp_Object tail;

  for (tail = list; !NILP (tail); tail = Fcdr (tail))
    {
      register Lisp_Object elt, tem, thiscar;
      elt = Fcar (tail);
      thiscar = CONSP (elt) ? XCAR (elt) : elt;
      if (!STRINGP (thiscar))
	continue;
      tem = Fcompare_strings (thiscar, make_number (0), Qnil,
			      key, make_number (0), Qnil,
			      case_fold);
      if (EQ (tem, Qt))
	return elt;
      QUIT;
    }
  return Qnil;
}

DEFUN ("minibuffer-complete", Fminibuffer_complete, Sminibuffer_complete, 0, 0, "",
       doc: /* Complete the minibuffer contents as far as possible.
Return nil if there is no valid completion, else t.
If no characters can be completed, display a list of possible completions.
If you repeat this command after it displayed such a list,
scroll the window of possible completions.  */)
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
      tem = Fpos_visible_in_window_p (make_number (ZV), window, Qnil);
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
      if (PT != ZV)
	Fgoto_char (make_number (ZV));
      temp_echo_area_glyphs (" [Sole completion]");
      break;

    case 3:
      if (PT != ZV)
	Fgoto_char (make_number (ZV));
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
       doc: /* If the minibuffer contents is a valid completion then exit.
Otherwise try to complete it.  If completion leads to a valid completion,
a repetition of this command will exit.  */)
     ()
{
  register int i;
  Lisp_Object val;

  /* Allow user to specify null string */
  if (XINT (Fminibuffer_prompt_end ()) == ZV)
    goto exit;

  if (!NILP (Ftest_completion (Fminibuffer_contents (),
			       Vminibuffer_completion_table,
			       Vminibuffer_completion_predicate)))
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
  return Fthrow (Qexit, Qnil);
  /* NOTREACHED */
}

DEFUN ("minibuffer-complete-word", Fminibuffer_complete_word, Sminibuffer_complete_word,
       0, 0, "",
       doc: /* Complete the minibuffer contents at most a single word.
After one word is completed as much as possible, a space or hyphen
is added, provided that matches some possible completion.
Return nil if there is no valid completion, else t.  */)
     ()
{
  Lisp_Object completion, tem, tem1;
  register int i, i_byte;
  register unsigned char *completion_string;
  struct gcpro gcpro1, gcpro2;
  int prompt_end_charpos;

  /* We keep calling Fbuffer_string rather than arrange for GC to
     hold onto a pointer to one of the strings thus made.  */

  completion = Ftry_completion (Fminibuffer_contents (),
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
  tem = Fminibuffer_contents ();
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

    CHECK_STRING (completion);
    tem = Fminibuffer_contents ();
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
	    Fdelete_minibuffer_contents ();
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
  
  prompt_end_charpos = XINT (Fminibuffer_prompt_end ());

  {
    int prompt_end_bytepos;
    prompt_end_bytepos = CHAR_TO_BYTE (prompt_end_charpos);
    i = ZV - prompt_end_charpos;
    i_byte = ZV_BYTE - prompt_end_bytepos;
  }

  /* If completion finds next char not unique,
     consider adding a space or a hyphen. */
  if (i == XSTRING (completion)->size)
    {
      GCPRO1 (completion);
      tem = Ftry_completion (concat2 (Fminibuffer_contents (), build_string (" ")),
			     Vminibuffer_completion_table,
			     Vminibuffer_completion_predicate);
      UNGCPRO;

      if (STRINGP (tem))
	completion = tem;
      else
	{
	  GCPRO1 (completion);
	  tem =
	    Ftry_completion (concat2 (Fminibuffer_contents (), build_string ("-")),
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
    int bytes = STRING_BYTES (XSTRING (completion));
    completion_string = XSTRING (completion)->data;
    for (; i_byte < STRING_BYTES (XSTRING (completion)); i_byte += len, i++)
      {
	c = STRING_CHAR_AND_LENGTH (completion_string + i_byte,
				    bytes - i_byte,
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

  if (i == ZV - prompt_end_charpos)
    {
      if (!NILP (Vcompletion_auto_help))
	Fminibuffer_completion_help ();
      return Qnil;
    }

  /* Otherwise insert in minibuffer the chars we got */

  Fdelete_minibuffer_contents ();
  insert_from_string (completion, 0, 0, i, i_byte, 1);
  return Qt;
}

DEFUN ("display-completion-list", Fdisplay_completion_list, Sdisplay_completion_list,
       1, 1, 0,
       doc: /* Display the list of completions, COMPLETIONS, using `standard-output'.
Each element may be just a symbol or string
or may be a list of two strings to be printed as if concatenated.
`standard-output' must be a buffer.
The actual completion alternatives, as inserted, are given `mouse-face'
properties of `highlight'.
At the end, this runs the normal hook `completion-setup-hook'.
It can find the completion buffer in `standard-output'.  */)
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

	  startpos = Qnil;

	  elt = Fcar (tail);
	  /* Compute the length of this element.  */
	  if (CONSP (elt))
	    {
	      tem = XCAR (elt);
	      CHECK_STRING (tem);
	      length = XSTRING (tem)->size;

	      tem = Fcar (XCDR (elt));
	      CHECK_STRING (tem);
	      length += XSTRING (tem)->size;
	    }
	  else
	    {
	      CHECK_STRING (elt);
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
	      || (column > 0 && length > 45))
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
       doc: /* Display a list of possible completions of the current minibuffer contents.  */)
     ()
{
  Lisp_Object completions;

  message ("Making completion list...");
  completions = Fall_completions (Fminibuffer_contents (),
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
       doc: /* Terminate minibuffer input.  */)
     ()
{
  if (INTEGERP (last_command_char))
    internal_self_insert (XINT (last_command_char), 0);
  else
    bitch_at_user ();

  return Fthrow (Qexit, Qnil);
}

DEFUN ("exit-minibuffer", Fexit_minibuffer, Sexit_minibuffer, 0, 0, "",
       doc: /* Terminate this minibuffer argument.  */)
     ()
{
  return Fthrow (Qexit, Qnil);
}

DEFUN ("minibuffer-depth", Fminibuffer_depth, Sminibuffer_depth, 0, 0, 0,
       doc: /* Return current depth of activations of minibuffer, a nonnegative integer.  */)
     ()
{
  return make_number (minibuf_level);
}

DEFUN ("minibuffer-prompt", Fminibuffer_prompt, Sminibuffer_prompt, 0, 0, 0,
       doc: /* Return the prompt string of the currently-active minibuffer.
If no minibuffer is active, return nil.  */)
     ()
{
  return Fcopy_sequence (minibuf_prompt);
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
       doc: /* Temporarily display STRING at the end of the minibuffer.
The text is displayed for two seconds,
or until the next input event arrives, whichever comes first.  */)
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
	       doc: /* If this is non-nil, `read-buffer' does its work by calling this function.  */);
  Vread_buffer_function = Qnil;

  DEFVAR_LISP ("minibuffer-setup-hook", &Vminibuffer_setup_hook,
	       doc: /* Normal hook run just after entry to minibuffer.  */);
  Vminibuffer_setup_hook = Qnil;

  DEFVAR_LISP ("minibuffer-exit-hook", &Vminibuffer_exit_hook,
	       doc: /* Normal hook run just after exit from minibuffer.  */);
  Vminibuffer_exit_hook = Qnil;

  DEFVAR_LISP ("history-length", &Vhistory_length,
	       doc: /* *Maximum length for history lists before truncation takes place.
A number means that length; t means infinite.  Truncation takes place
just after a new element is inserted.  Setting the history-length
property of a history variable overrides this default.  */);
  XSETFASTINT (Vhistory_length, 30);

  DEFVAR_LISP ("completion-auto-help", &Vcompletion_auto_help,
	       doc: /* *Non-nil means automatically provide help for invalid completion input.  */);
  Vcompletion_auto_help = Qt;

  DEFVAR_BOOL ("completion-ignore-case", &completion_ignore_case,
	       doc: /* Non-nil means don't consider case significant in completion.  */);
  completion_ignore_case = 0;

  DEFVAR_BOOL ("enable-recursive-minibuffers", &enable_recursive_minibuffers,
	       doc: /* *Non-nil means to allow minibuffer commands while in the minibuffer.
This variable makes a difference whenever the minibuffer window is active. */);
  enable_recursive_minibuffers = 0;

  DEFVAR_LISP ("minibuffer-completion-table", &Vminibuffer_completion_table,
	       doc: /* Alist or obarray used for completion in the minibuffer.
This becomes the ALIST argument to `try-completion' and `all-completion'.

The value may alternatively be a function, which is given three arguments:
  STRING, the current buffer contents;
  PREDICATE, the predicate for filtering possible matches;
  CODE, which says what kind of things to do.
CODE can be nil, t or `lambda'.
nil means to return the best completion of STRING, or nil if there is none.
t means to return a list of all possible completions of STRING.
`lambda' means to return t if STRING is a valid completion as it stands.  */);
  Vminibuffer_completion_table = Qnil;

  DEFVAR_LISP ("minibuffer-completion-predicate", &Vminibuffer_completion_predicate,
	       doc: /* Within call to `completing-read', this holds the PREDICATE argument.  */);
  Vminibuffer_completion_predicate = Qnil;

  DEFVAR_LISP ("minibuffer-completion-confirm", &Vminibuffer_completion_confirm,
	       doc: /* Non-nil means to demand confirmation of completion before exiting minibuffer.  */);
  Vminibuffer_completion_confirm = Qnil;

  DEFVAR_LISP ("minibuffer-completing-file-name",
	       &Vminibuffer_completing_file_name,
	       doc: /* Non-nil means completing file names.  */);
  Vminibuffer_completing_file_name = Qnil;

  DEFVAR_LISP ("minibuffer-help-form", &Vminibuffer_help_form,
	       doc: /* Value that `help-form' takes on inside the minibuffer.  */);
  Vminibuffer_help_form = Qnil;

  DEFVAR_LISP ("minibuffer-history-variable", &Vminibuffer_history_variable,
	       doc: /* History list symbol to add minibuffer values to.
Each string of minibuffer input, as it appears on exit from the minibuffer,
is added with
  (set minibuffer-history-variable
  (cons STRING (symbol-value minibuffer-history-variable)))  */);
  XSETFASTINT (Vminibuffer_history_variable, 0);

  DEFVAR_LISP ("minibuffer-history-position", &Vminibuffer_history_position,
	       doc: /* Current position of redoing in the history list.  */);
  Vminibuffer_history_position = Qnil;

  DEFVAR_BOOL ("minibuffer-auto-raise", &minibuffer_auto_raise,
	       doc: /* *Non-nil means entering the minibuffer raises the minibuffer's frame.
Some uses of the echo area also raise that frame (since they use it too).  */);
  minibuffer_auto_raise = 0;

  DEFVAR_LISP ("completion-regexp-list", &Vcompletion_regexp_list,
	       doc: /* List of regexps that should restrict possible completions.  */);
  Vcompletion_regexp_list = Qnil;

  DEFVAR_BOOL ("minibuffer-allow-text-properties",
	       &minibuffer_allow_text_properties,
	       doc: /* Non-nil means `read-from-minibuffer' should not discard text properties.
This also affects `read-string', but it does not affect `read-minibuffer',
`read-no-blanks-input', or any of the functions that do minibuffer input
with completion; they always discard text properties.  */);
  minibuffer_allow_text_properties = 0;

  DEFVAR_LISP ("minibuffer-prompt-properties", &Vminibuffer_prompt_properties,
	       doc: /* Text properties that are added to minibuffer prompts.
These are in addition to the basic `field' property, and stickiness
properties.  */);
  /* We use `intern' here instead of Qread_only to avoid
     initialization-order problems.  */
  Vminibuffer_prompt_properties
    = Fcons (intern ("read-only"), Fcons (Qt, Qnil));

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

  defsubr (&Sminibuffer_prompt_end);
  defsubr (&Sminibuffer_contents);
  defsubr (&Sminibuffer_contents_no_properties);
  defsubr (&Sdelete_minibuffer_contents);

  defsubr (&Stry_completion);
  defsubr (&Sall_completions);
  defsubr (&Stest_completion);
  defsubr (&Sassoc_string);
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

  initial_define_key (Vminibuffer_local_ns_map, ' ',
		      "exit-minibuffer");
  initial_define_key (Vminibuffer_local_ns_map, '\t',
		      "exit-minibuffer");
  initial_define_key (Vminibuffer_local_ns_map, '?',
		      "self-insert-and-exit");

  initial_define_key (Vminibuffer_local_completion_map, '\t',
		      "minibuffer-complete");
  initial_define_key (Vminibuffer_local_completion_map, ' ',
		      "minibuffer-complete-word");
  initial_define_key (Vminibuffer_local_completion_map, '?',
		      "minibuffer-completion-help");

  initial_define_key (Vminibuffer_local_must_match_map, Ctl ('m'),
		      "minibuffer-complete-and-exit");
  initial_define_key (Vminibuffer_local_must_match_map, Ctl ('j'),
		      "minibuffer-complete-and-exit");
}
