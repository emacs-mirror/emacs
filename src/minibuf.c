/* Minibuffer input and completion.

Copyright (C) 1985-1986, 1993-2026 Free Software Foundation, Inc.

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
#include <errno.h>

#include <binary-io.h>

#include "lisp.h"
#include "character.h"
#include "buffer.h"
#include "keyboard.h"
#include "frame.h"
#include "window.h"
#include "keymap.h"
#include "sysstdio.h"
#include "systty.h"
#include "pdumper.h"

#ifdef HAVE_NTGUI
#include "w32term.h"
#endif

/* List of buffers for use as minibuffers.
   The first element of the list is used for the outermost minibuffer
   invocation, the next element is used for a recursive minibuffer
   invocation, etc.  The list is extended at the end as deeper
   minibuffer recursions are encountered.  */

Lisp_Object Vminibuffer_list;
static Lisp_Object Vcommand_loop_level_list;

/* Data to remember during recursive minibuffer invocations.  */

static Lisp_Object minibuf_save_list;

/* Depth in minibuffer invocations.  */

EMACS_INT minibuf_level;

/* Fread_minibuffer leaves the input here as a string.  */

Lisp_Object last_minibuf_string;

/* Prompt to display in front of the mini-buffer contents.  */

static Lisp_Object minibuf_prompt;

/* The frame containing the most recently opened minibuffer.  This is
   used only when `minibuffer-follows-selected-frame' is neither nil
   nor t.  */

static Lisp_Object MB_frame;

/* Width of current mini-buffer prompt.  Only set after display_line
   of the line that contains the prompt.  */

static ptrdiff_t minibuf_prompt_width;

static Lisp_Object nth_minibuffer (EMACS_INT depth);
static EMACS_INT minibuf_c_loop_level (EMACS_INT depth);
static void set_minibuffer_mode (Lisp_Object buf, EMACS_INT depth);
static bool live_minibuffer_p (Lisp_Object);


/* Return TRUE when a frame switch causes a minibuffer on the old
   frame to move onto the new one. */
static bool
minibuf_follows_frame (void)
{
  return EQ (Fdefault_toplevel_value (Qminibuffer_follows_selected_frame),
             Qt);
}

#if 0
/* Return TRUE when a minibuffer always remains on the frame where it
   was first invoked. */
static bool
minibuf_stays_put (void)
{
  return NILP (Fdefault_toplevel_value (Qminibuffer_follows_selected_frame));
}
#endif

/* Return TRUE when opening a (recursive) minibuffer causes
   minibuffers on other frames to move to the selected frame.  */
static bool
minibuf_moves_frame_when_opened (void)
{
  return !NILP (Fdefault_toplevel_value (Qminibuffer_follows_selected_frame));
}

/* Put minibuf on currently selected frame's minibuffer.
   We do this whenever the user starts a new minibuffer
   or when a minibuffer exits.  */

static void
choose_minibuf_frame (void)
{
  if (FRAMEP (selected_frame)
      && FRAME_LIVE_P (XFRAME (selected_frame))
      && WINDOW_LIVE_P (XFRAME (selected_frame)->minibuffer_window)
      && !EQ (minibuf_window, XFRAME (selected_frame)->minibuffer_window))
    {
      struct frame *sf = XFRAME (selected_frame);
      /* I don't think that any frames may validly have a null
	 minibuffer window anymore.  (2021-04-15): Tooltip frames have
	 a null MB.  Comment out the following.  */
      /* if (NILP (sf->minibuffer_window)) */
      /* 	emacs_abort (); */

      minibuf_window = sf->minibuffer_window;
    }
}

/* If ENT1 has a higher minibuffer index than ENT2, return true.  More
precisely, compare the buffer components of each window->prev_buffers
entry.  */
static bool
minibuffer_ent_greater (Lisp_Object ent1, Lisp_Object ent2)
{
  return this_minibuffer_depth (Fcar (ent1))
    > this_minibuffer_depth (Fcar (ent2)) ;
}

/* Move the ordered "stack" of minibuffers from SOURCE_WINDOW to
   DEST_WINDOW, interleaving those minibuffers with any in DEST_WINDOW
   to produce an ordered combination.  The ordering is by minibuffer
   depth.  A stack of minibuffers consists of the minibuffer currently
   in DEST/SOURCE_WINDOW together with any recorded in the
   ->prev_buffers field of the struct window.  */
static void
zip_minibuffer_stacks (Lisp_Object dest_window, Lisp_Object source_window)
{
  struct window *dw = XWINDOW (dest_window);
  struct window *sw = XWINDOW (source_window);
  Lisp_Object acc;
  Lisp_Object d_ent;	/* Entry from dw->prev_buffers */

  if (!live_minibuffer_p (dw->contents)
      && NILP (dw->prev_buffers))
    {
      set_window_buffer (dest_window, sw->contents, 0, 0);
      Fset_window_start (dest_window, Fwindow_start (source_window), Qnil);
      Fset_window_point (dest_window, Fwindow_point (source_window));
      wset_prev_buffers (dw, sw->prev_buffers);
      set_window_buffer (source_window, nth_minibuffer (0), 0, 0);
      wset_prev_buffers (sw, Qnil);
      return;
    }

  calln (Qrecord_window_buffer, dest_window);
  calln (Qrecord_window_buffer, source_window);

  acc = merge_c (dw->prev_buffers, sw->prev_buffers, minibuffer_ent_greater);

  if (!NILP (acc))
    {
      d_ent = Fcar (acc);
      acc = Fcdr (acc);
      set_window_buffer (dest_window, Fcar (d_ent), 0, 0);
      Fset_window_start (dest_window, Fcar (Fcdr (d_ent)), Qnil);
      Fset_window_point (dest_window, Fcar (Fcdr (Fcdr (d_ent))));
    }

  wset_prev_buffers (dw, acc);
  wset_prev_buffers (sw, Qnil);
  set_window_buffer (source_window, nth_minibuffer (0), 0, 0);
}

/* If `minibuffer_follows_selected_frame' is t, or we're about to
   delete a frame which potentially "contains" minibuffers, move them
   from the old frame to the to-be-selected frame.  This function is
   intended to be called from `do_switch_frame' in frame.c.  OF is the
   old frame, FRAME is the to-be-selected frame, and FOR_DELETION is true
   if OF is about to be deleted.  */
void
move_minibuffers_onto_frame (struct frame *of, Lisp_Object frame,
                             bool for_deletion)
{
  struct frame *f = XFRAME (frame);

  minibuf_window = f->minibuffer_window;
  if (!(minibuf_level
	&& (for_deletion || minibuf_follows_frame () || FRAME_INITIAL_P (of))))
    return;
  if (FRAME_LIVE_P (f)
      && !EQ (f->minibuffer_window, of->minibuffer_window)
      && WINDOW_LIVE_P (f->minibuffer_window) /* F not a tooltip frame */
      && WINDOW_LIVE_P (of->minibuffer_window))
    {
      zip_minibuffer_stacks (f->minibuffer_window, of->minibuffer_window);
      if (for_deletion && XFRAME (MB_frame) != of)
	MB_frame = frame;
    }
}

DEFUN ("active-minibuffer-window", Factive_minibuffer_window,
       Sactive_minibuffer_window, 0, 0, 0,
       doc: /* Return the currently active minibuffer window, or nil if none.  */)
     (void)
{
  Lisp_Object frames, frame;
  struct frame *f;
  Lisp_Object innermost_MB;

  if (!minibuf_level)
    return Qnil;

  innermost_MB = nth_minibuffer (minibuf_level);
  if (NILP (innermost_MB))
    emacs_abort ();
  FOR_EACH_FRAME (frames, frame)
    {
      f = XFRAME (frame);
      if (FRAME_LIVE_P (f)
	  && WINDOW_LIVE_P (f->minibuffer_window)
	  && EQ (XWINDOW (f->minibuffer_window)->contents, innermost_MB))
	return f->minibuffer_window;
    }
  return minibuf_window;	/* "Can't happen." */
}

DEFUN ("set-minibuffer-window", Fset_minibuffer_window,
       Sset_minibuffer_window, 1, 1, 0,
       doc: /* Specify which minibuffer window to use for the minibuffer.
This affects where the minibuffer is displayed if you put text in it
without invoking the usual minibuffer commands.  */)
  (Lisp_Object window)
{
  CHECK_WINDOW (window);
  if (! MINI_WINDOW_P (XWINDOW (window)))
    error ("Window is not a minibuffer window");

  minibuf_window = window;

  return window;
}


/* Actual minibuffer invocation.  */

static void read_minibuf_unwind (void);
static void minibuffer_unwind (void);
static void run_exit_minibuf_hook (Lisp_Object minibuf);


/* Read a Lisp object from VAL and return it.  If VAL is an empty
   string, and DEFALT is a string, read from DEFALT instead of VAL.  */

static Lisp_Object
string_to_object (Lisp_Object val, Lisp_Object defalt)
{
  Lisp_Object expr_and_pos;
  ptrdiff_t pos;

  if (STRINGP (val) && SCHARS (val) == 0)
    {
      if (STRINGP (defalt))
	val = defalt;
      else if (CONSP (defalt) && STRINGP (XCAR (defalt)))
	val = XCAR (defalt);
    }

  expr_and_pos = Fread_from_string (val, Qnil, Qnil);
  pos = XFIXNUM (Fcdr (expr_and_pos));
  if (pos != SCHARS (val))
    {
      /* Ignore trailing whitespace; any other trailing junk
	 is an error.  */
      ptrdiff_t i;
      pos = string_char_to_byte (val, pos);
      for (i = pos; i < SBYTES (val); i++)
	{
	  int c = SREF (val, i);
	  if (c != ' ' && c != '\t' && c != '\n')
	    xsignal1 (Qinvalid_read_syntax,
		      build_string ("Trailing garbage following expression"));
	}
    }

  val = Fcar (expr_and_pos);
  return val;
}


/* Like read_minibuf but reading from stdin.  This function is called
   from read_minibuf to do the job if noninteractive.  */

static Lisp_Object
read_minibuf_noninteractive (Lisp_Object prompt, bool expflag,
			     Lisp_Object defalt)
{
  ptrdiff_t size, len;
  char *line;
  Lisp_Object val;
  int c;
  unsigned char hide_char = 0;
  struct emacs_tty etty;
  bool etty_valid UNINIT;

  /* Check, whether we need to suppress echoing.  */
  if (CHARACTERP (Vread_hide_char))
    hide_char = XFIXNAT (Vread_hide_char);

  /* Manipulate tty.  */
  if (hide_char)
    {
      etty_valid = emacs_get_tty (STDIN_FILENO, &etty) == 0;
      if (etty_valid)
	set_binary_mode (STDIN_FILENO, O_BINARY);
      suppress_echo_on_tty (STDIN_FILENO);
    }

  fwrite (SDATA (prompt), 1, SBYTES (prompt), stdout);
  fflush (stdout);

  val = Qnil;
  size = 100;
  len = 0;
  line = xmalloc (size);

  while ((c = getchar ()) != '\n' && c != '\r')
    {
      if (c == EOF)
	{
	  if (errno != EINTR)
	    break;
	}
      else
	{
	  if (hide_char)
	    putchar (hide_char);
	  if (len == size)
	    line = xpalloc (line, &size, 1, -1, sizeof *line);
	  line[len++] = c;
	}
    }

  /* Reset tty.  */
  if (hide_char)
    {
      putc ('\n', stdout);
      if (etty_valid)
	{
	  emacs_set_tty (STDIN_FILENO, &etty, 0);
	  set_binary_mode (STDIN_FILENO, O_TEXT);
	}
    }

  if (len || c == '\n' || c == '\r')
    {
      val = make_string (line, len);
      xfree (line);
    }
  else
    {
      xfree (line);
      xsignal1 (Qend_of_file, build_string ("Error reading from stdin"));
    }

  /* If Lisp form desired instead of string, parse it.  */
  if (expflag)
    val = string_to_object (val, CONSP (defalt) ? XCAR (defalt) : defalt);

  return val;
}

/* Return true when BUFFER is an active minibuffer.  */
static bool
live_minibuffer_p (Lisp_Object buffer)
{
  Lisp_Object tem;
  EMACS_INT i;

  if (EQ (buffer, Fcar (Vminibuffer_list)))
    /*  *Minibuf-0* is never active.  */
    return false;
  tem = Fcdr (Vminibuffer_list);
  for (i = 1; i <= minibuf_level; i++, tem = Fcdr (tem))
    if (EQ (Fcar (tem), buffer))
      return true;
  return false;
}

DEFUN ("minibufferp", Fminibufferp,
       Sminibufferp, 0, 2, 0,
       doc: /* Return t if BUFFER is a minibuffer.
No argument or nil as argument means use current buffer as BUFFER.
BUFFER can be a buffer or a buffer name.  If LIVE is non-nil, then
return t only if BUFFER is an active minibuffer.  */)
  (Lisp_Object buffer, Lisp_Object live)
{
  if (NILP (buffer))
    buffer = Fcurrent_buffer ();
  else if (STRINGP (buffer))
    buffer = Fget_buffer (buffer);
  else
    CHECK_BUFFER (buffer);

  return (NILP (live)
          ? !NILP (Fmemq (buffer, Vminibuffer_list))
          : live_minibuffer_p (buffer))
    ? Qt : Qnil;
}

DEFUN ("innermost-minibuffer-p", Finnermost_minibuffer_p,
       Sinnermost_minibuffer_p, 0, 1, 0,
       doc: /* Return t if BUFFER is the most nested active minibuffer.
No argument or nil as argument means use the current buffer as BUFFER.  */)
  (Lisp_Object buffer)
{
  if (NILP (buffer))
    buffer = Fcurrent_buffer ();
  return BASE_EQ (buffer, (Fcar (Fnthcdr (make_fixnum (minibuf_level),
					  Vminibuffer_list))))
    ? Qt
    : Qnil;
}

DEFUN ("minibuffer-innermost-command-loop-p", Fminibuffer_innermost_command_loop_p,
       Sminibuffer_innermost_command_loop_p, 0, 1, 0,
       doc: /* Return t if BUFFER is a minibuffer at the current command loop level.
No argument or nil as argument means use the current buffer as BUFFER.  */)
  (Lisp_Object buffer)
{
  EMACS_INT depth;
  if (NILP (buffer))
    buffer = Fcurrent_buffer ();
  depth = this_minibuffer_depth (buffer);
  return depth && minibuf_c_loop_level (depth) == command_loop_level
    ? Qt
    : Qnil;
}

/* Return the nesting depth of the active minibuffer BUFFER, or 0 if
   BUFFER isn't such a thing.  If BUFFER is nil, this means use the current
   buffer.  */
EMACS_INT
this_minibuffer_depth (Lisp_Object buffer)
{
  EMACS_INT i;
  Lisp_Object bufs;

  if (NILP (buffer))
    buffer = Fcurrent_buffer ();
  for (i = 1, bufs = Fcdr (Vminibuffer_list);
       i <= minibuf_level;
       i++, bufs = Fcdr (bufs))
    if (EQ (Fcar (bufs), buffer))
      return i;
  return 0;
}

DEFUN ("abort-minibuffers", Fabort_minibuffers, Sabort_minibuffers, 0, 0, "",
       doc: /* Abort the current minibuffer.
If we are not currently in the innermost minibuffer, prompt the user to
confirm the aborting of the current minibuffer and all contained ones.  */)
  (void)
{
  EMACS_INT minibuf_depth = this_minibuffer_depth (Qnil);
  Lisp_Object array[2];
  AUTO_STRING (fmt, "Abort %s minibuffer levels? ");

  if (!minibuf_depth)
    error ("Not in a minibuffer");
  if (NILP (Fminibuffer_innermost_command_loop_p (Qnil)))
    error ("Not in most nested command loop");
  if (minibuf_depth < minibuf_level)
    {
      array[0] = fmt;
      array[1] = make_fixnum (minibuf_level - minibuf_depth + 1);
      if (!NILP (Fyes_or_no_p (Fformat (2, array))))
	{
	  /* Due to the above check, the current minibuffer is in the
	     most nested command loop, which means that we don't have
	     to abort any extra non-minibuffer recursive edits.  Thus,
	     the number of recursive edits we have to abort equals the
	     number of minibuffers we have to abort.  */
	  calln (Qminibuffer_quit_recursive_edit, array[1]);
	}
    }
  else
    call0 (Qminibuffer_quit_recursive_edit);
  return Qnil;
}

DEFUN ("minibuffer-prompt-end", Fminibuffer_prompt_end,
       Sminibuffer_prompt_end, 0, 0, 0,
       doc: /* Return the buffer position of the end of the minibuffer prompt.
Return (point-min) if current buffer is not a minibuffer.  */)
  (void)
{
  /* This function is written to be most efficient when there's a prompt.  */
  Lisp_Object beg, end, tem;
  beg = make_fixnum (BEGV);

  tem = Fmemq (Fcurrent_buffer (), Vminibuffer_list);
  if (NILP (tem))
    return beg;

  end = Ffield_end (beg, Qnil, Qnil);

  if (XFIXNUM (end) == ZV && NILP (Fget_char_property (beg, Qfield, Qnil)))
    return beg;
  else
    return end;
}

DEFUN ("minibuffer-contents", Fminibuffer_contents,
       Sminibuffer_contents, 0, 0, 0,
       doc: /* Return the user input in a minibuffer as a string.
If the current buffer is not a minibuffer, return its entire contents.  */)
  (void)
{
  ptrdiff_t prompt_end = XFIXNUM (Fminibuffer_prompt_end ());
  return make_buffer_string (prompt_end, ZV, 1);
}

DEFUN ("minibuffer-contents-no-properties", Fminibuffer_contents_no_properties,
       Sminibuffer_contents_no_properties, 0, 0, 0,
       doc: /* Return the user input in a minibuffer as a string, without text-properties.
If the current buffer is not a minibuffer, return its entire contents.  */)
  (void)
{
  ptrdiff_t prompt_end = XFIXNUM (Fminibuffer_prompt_end ());
  return make_buffer_string (prompt_end, ZV, 0);
}


/* Read from the minibuffer using keymap MAP and initial contents INITIAL,
   putting point minus BACKUP_N bytes from the end of INITIAL,
   prompting with PROMPT (a string), using history list HISTVAR
   with initial position HISTPOS.  INITIAL should be a string or a
   cons of a string and an integer.  BACKUP_N should be <= 0, or
   Qnil, which is equivalent to 0.  If INITIAL is a cons, BACKUP_N is
   ignored and replaced with an integer that puts point at one-indexed
   position N in INITIAL, where N is the CDR of INITIAL, or at the
   beginning of INITIAL if N <= 0.

   Normally return the result as a string (the text that was read),
   but if EXPFLAG, read it and return the object read.
   If HISTVAR is given, save the value read on that history only if it doesn't
   match the front of that history list exactly.  The value is pushed onto
   the list as the string that was read.

   DEFALT specifies the default value for the sake of history commands.

   If ALLOW_PROPS or `minibuffer-allow-text-properties' (possibly
   buffer-local) is non-nil, do not throw away text properties.

   if INHERIT_INPUT_METHOD, the minibuffer inherits the
   current input method.  */

static Lisp_Object
read_minibuf (Lisp_Object map, Lisp_Object initial, Lisp_Object prompt,
	      bool expflag,
	      Lisp_Object histvar, Lisp_Object histpos, Lisp_Object defalt,
	      bool allow_props, bool inherit_input_method)
{
  Lisp_Object val;
  specpdl_ref count = SPECPDL_INDEX ();
  Lisp_Object mini_frame, ambient_dir, minibuffer, input_method;
  Lisp_Object calling_frame = selected_frame;
  Lisp_Object calling_window = selected_window;
  Lisp_Object enable_multibyte;
  EMACS_INT pos = 0;
  /* String to add to the history.  */
  Lisp_Object histstring;
  Lisp_Object histval;

  Lisp_Object empty_minibuf;

  specbind (Qminibuffer_default, defalt);
  specbind (Qinhibit_read_only, Qnil);

  /* If Vminibuffer_completing_file_name is `lambda' on entry, it was t
     in previous recursive minibuffer, but was not set explicitly
     to t for this invocation, so set it to nil in this minibuffer.
     Save the old value now, before we change it.  */
  specbind (Qminibuffer_completing_file_name,
	    Vminibuffer_completing_file_name);
  if (EQ (Vminibuffer_completing_file_name, Qlambda))
    Vminibuffer_completing_file_name = Qnil;

#ifdef HAVE_WINDOW_SYSTEM
  if (display_hourglass_p)
    cancel_hourglass ();
#endif

  if (!NILP (initial))
    {
      if (CONSP (initial))
	{
	  Lisp_Object backup_n = XCDR (initial);
	  initial = XCAR (initial);
	  CHECK_STRING (initial);
	  if (!NILP (backup_n))
	    {
	      CHECK_FIXNUM (backup_n);
	      /* Convert to distance from end of input.  */
	      if (XFIXNUM (backup_n) < 1)
		/* A number too small means the beginning of the string.  */
		pos =  - SCHARS (initial);
	      else
		pos = XFIXNUM (backup_n) - 1 - SCHARS (initial);
	    }
	}
      else
	CHECK_STRING (initial);
    }
  val = Qnil;
  ambient_dir = BVAR (current_buffer, directory);
  input_method = Qnil;
  enable_multibyte = Qnil;

  if (!STRINGP (prompt))
    prompt = empty_unibyte_string;

  if (!enable_recursive_minibuffers
      && minibuf_level > 0)
    {
      Lisp_Object str
	= build_string ("Command attempted to use minibuffer while in minibuffer");
      if (EQ (selected_window, minibuf_window))
	Fsignal (Quser_error, (list1 (str)));
      else
	/* If we're in another window, cancel the minibuffer that's active.  */
	Fthrow (Qexit, str);
    }

  if ((noninteractive
       /* In case we are running as a daemon, only do this before
	  detaching from the terminal.  */
       || (IS_DAEMON && DAEMON_RUNNING))
      && NILP (Vexecuting_kbd_macro))
    {
      val = read_minibuf_noninteractive (prompt, expflag, defalt);
      return unbind_to (count, val);
    }

  /* Ensure now that the latest minibuffer has been created and pushed
     onto Vminibuffer_list before incrementing minibuf_level, in case
     a hook called during the minibuffer creation calls
     Factive_minibuffer_window.  */
  minibuffer = get_minibuffer (minibuf_level + 1);
  minibuf_level++;		/* Before calling choose_minibuf_frame.  */

  /* Choose the minibuffer window and frame, and take action on them.  */

  /* Prepare for restoring the current buffer since choose_minibuf_frame
     calling Fset_frame_selected_window may change it (Bug#12766).  */
  record_unwind_protect (restore_buffer, Fcurrent_buffer ());

  choose_minibuf_frame ();
  mini_frame = WINDOW_FRAME (XWINDOW (minibuf_window));

  if (minibuf_level > 1
      && WINDOW_LIVE_P (XFRAME (MB_frame)->minibuffer_window)
      && !EQ (XWINDOW (XFRAME (selected_frame)->minibuffer_window)->frame,
	      MB_frame)
      && minibuf_moves_frame_when_opened ()
      && (!minibuf_follows_frame ()))
    {
      struct frame *of = XFRAME (MB_frame);

      zip_minibuffer_stacks (minibuf_window, of->minibuffer_window);
      /* MB_frame's minibuffer can be on a different frame.  */
      if (MINI_WINDOW_P (XWINDOW (FRAME_SELECTED_WINDOW (of))))
	Fset_frame_selected_window (MB_frame,
				    Fframe_first_window (MB_frame), Qnil);
    }
  MB_frame = XWINDOW (XFRAME (selected_frame)->minibuffer_window)->frame;

  calln (Qrecord_window_buffer, minibuf_window);

  record_unwind_protect_void (minibuffer_unwind);
  if (read_minibuffer_restore_windows)
    record_unwind_protect (restore_window_configuration,
			   list3 (Fcurrent_window_configuration (Qnil),
				  Qt, Qt));

  /* If the minibuffer window is on a different frame, save that
     frame's configuration too.  */
  if (read_minibuffer_restore_windows &&
      !EQ (mini_frame, selected_frame))
    record_unwind_protect (restore_window_configuration,
			   list3 (Fcurrent_window_configuration (mini_frame),
				  Qnil, Qt));

  /* If the minibuffer is on an iconified or invisible frame,
     make it visible now.  */
  Fmake_frame_visible (mini_frame);

  if (minibuffer_auto_raise)
    Fraise_frame (mini_frame);

  temporarily_switch_to_single_kboard (XFRAME (mini_frame));

  /* We have to do this after saving the window configuration
     since that is what restores the current buffer.  */

  /* Arrange to restore a number of minibuffer-related variables.
     We could bind each variable separately, but that would use lots of
     specpdl slots.  */
  minibuf_save_list
    = Fcons (Voverriding_local_map,
	     Fcons (minibuf_window,
		    Fcons (calling_frame,
			   Fcons (calling_window,
				  minibuf_save_list))));
  minibuf_save_list
    = Fcons (minibuf_prompt,
	     Fcons (make_fixnum (minibuf_prompt_width),
		    Fcons (Vhelp_form,
			   Fcons (Vcurrent_prefix_arg,
				  Fcons (Vminibuffer_history_position,
					 Fcons (Vminibuffer_history_variable,
						minibuf_save_list))))));
  minibuf_save_list
    = Fcons (Fthis_command_keys_vector (), minibuf_save_list);

  record_unwind_protect_void (read_minibuf_unwind);
  /* We are exiting the minibuffer one way or the other, so run the hook.
     It should be run before unwinding the minibuf settings.  Do it
     separately from read_minibuf_unwind because we need to make sure that
     read_minibuf_unwind is fully executed even if exit-minibuffer-hook
     signals an error.  --Stef  */
  record_unwind_protect (run_exit_minibuf_hook, minibuffer);

  /* Now that we can restore all those variables, start changing them.  */

  minibuf_prompt_width = 0;
  minibuf_prompt = Fcopy_sequence (prompt);
  Vminibuffer_history_position = histpos;
  Vminibuffer_history_variable = histvar;
  Vhelp_form = Vminibuffer_help_form;
  /* If this minibuffer is reading a file name, that doesn't mean
     recursive ones are.  But we cannot set it to nil, because
     completion code still need to know the minibuffer is completing a
     file name.  So use `lambda' as intermediate value meaning
     "t" in this minibuffer, but "nil" in next minibuffer.  */
  if (!NILP (Vminibuffer_completing_file_name))
    Vminibuffer_completing_file_name = Qlambda;

  /* If variable is unbound, make it nil.  */
  histval = find_symbol_value (histvar);
  if (BASE_EQ (histval, Qunbound))
    {
      Fset (histvar, Qnil);
      histval = Qnil;
    }

  if (inherit_input_method)
    {
      /* `current-input-method' is buffer local.  So, remember it in
	 INPUT_METHOD before changing the current buffer.  */
      input_method = Fsymbol_value (Qcurrent_input_method);
      enable_multibyte = BVAR (current_buffer, enable_multibyte_characters);
    }

  /* Switch to the minibuffer.  */

  set_minibuffer_mode (minibuffer, minibuf_level);
  Fset_buffer (minibuffer);

  /* Defeat (setq-default truncate-lines t), since truncated lines do
     not work correctly in minibuffers.  (Bug#5715, etc)  */
  bset_truncate_lines (current_buffer, Qnil);

  /* The current buffer's default directory is usually the right thing
     for our minibuffer here.  However, if you're typing a command at
     a minibuffer-only frame when minibuf_level is zero, then buf IS
     the current_buffer, so reset_buffer leaves buf's default
     directory unchanged.  This is a bummer when you've just started
     up Emacs and buf's default directory is Qnil.  Here's a hack; can
     you think of something better to do?  Find another buffer with a
     better directory, and use that one instead.  */
  if (STRINGP (ambient_dir))
    bset_directory (current_buffer, ambient_dir);
  else
    {
      Lisp_Object tail, buf;

      FOR_EACH_LIVE_BUFFER (tail, buf)
	if (STRINGP (BVAR (XBUFFER (buf), directory)))
	  {
	    bset_directory (current_buffer,
			    BVAR (XBUFFER (buf), directory));
	    break;
	  }
    }

  if (!EQ (mini_frame, selected_frame))
    Fredirect_frame_focus (selected_frame, mini_frame);

  Vminibuf_scroll_window = selected_window;
  if (minibuf_level == 1 || !EQ (minibuf_window, selected_window))
    minibuf_selected_window = selected_window;

  /* Empty out the minibuffers of all frames, except those frames
     where there is an active minibuffer.
     Set them to point to ` *Minibuf-0*', which is always empty.  */
  empty_minibuf = nth_minibuffer (0);
  set_minibuffer_mode (empty_minibuf, 0);

  /* Display this minibuffer in the proper window.  */
  /* Use set_window_buffer instead of Fset_window_buffer (see
     discussion of bug#11984, bug#12025, bug#12026).  */
  set_window_buffer (minibuf_window, Fcurrent_buffer (), 0, 0);
  Fselect_window (minibuf_window, Qnil);
  XWINDOW (minibuf_window)->hscroll = 0;
  XWINDOW (minibuf_window)->suspend_auto_hscroll = 0;

  /* Erase the buffer.  */
  {
    specpdl_ref count1 = SPECPDL_INDEX ();
    specbind (Qinhibit_read_only, Qt);
    specbind (Qinhibit_modification_hooks, Qt);
    Ferase_buffer ();

    /* If appropriate, copy enable-multibyte-characters into the minibuffer.
       In any case don't blindly inherit the multibyteness used previously.  */
    bset_enable_multibyte_characters (current_buffer,
                                      inherit_input_method ? enable_multibyte
                                      : Qt);

    /* Insert the prompt, record where it ends.  */
    Finsert (1, &minibuf_prompt);
    if (PT > BEG)
      {
	Fput_text_property (make_fixnum (BEG), make_fixnum (PT),
			    Qfront_sticky, Qt, Qnil);
	Fput_text_property (make_fixnum (BEG), make_fixnum (PT),
			    Qrear_nonsticky, Qt, Qnil);
	Fput_text_property (make_fixnum (BEG), make_fixnum (PT),
			    Qfield, Qt, Qnil);
	if (CONSP (Vminibuffer_prompt_properties))
	  {
	    /* We want to apply all properties from
	       `minibuffer-prompt-properties' to the region normally,
	       but if the `face' property is present, add that
	       property to the end of the face properties to avoid
	       overwriting faces. */
	    Lisp_Object list = Vminibuffer_prompt_properties;
	    while (CONSP (list))
	      {
		Lisp_Object key = XCAR (list);
		list = XCDR (list);
		if (CONSP (list))
		  {
		    Lisp_Object val = XCAR (list);
		    list = XCDR (list);
		    if (EQ (key, Qface))
		      Fadd_face_text_property (make_fixnum (BEG),
					       make_fixnum (PT), val, Qt, Qnil);
		    else
		      Fput_text_property (make_fixnum (BEG), make_fixnum (PT),
					  key, val, Qnil);
		  }
	      }
	  }
      }
    unbind_to (count1, Qnil);
  }

  minibuf_prompt_width = current_column ();

  /* Put in the initial input.  */
  if (!NILP (initial))
    {
      Finsert (1, &initial);
      Fforward_char (make_fixnum (pos));
    }

  clear_message (1, 1);
  bset_keymap (current_buffer, map);

  /* Turn on an input method stored in INPUT_METHOD if any.  */
  if (STRINGP (input_method) && !NILP (Ffboundp (Qactivate_input_method)))
    calln (Qactivate_input_method, input_method);

  run_hook (Qminibuffer_setup_hook);

  /* Don't allow the user to undo past this point.  */
  bset_undo_list (current_buffer, Qnil);

  recursive_edit_1 ();

  /* If cursor is on the minibuffer line,
     show the user we have exited by putting it in column 0.  */
  if (XWINDOW (minibuf_window)->cursor.vpos >= 0
      && !noninteractive
      && !FRAME_INITIAL_P (SELECTED_FRAME ()))
    {
      XWINDOW (minibuf_window)->cursor.hpos = 0;
      XWINDOW (minibuf_window)->cursor.x = 0;
      XWINDOW (minibuf_window)->must_be_updated_p = true;
      struct frame *sf = XFRAME (selected_frame);
      update_frame (sf, true);
      if (is_tty_frame (sf))
	combine_updates_for_frame (sf, true);

#ifndef HAVE_NTGUI
      flush_frame (XFRAME (XWINDOW (minibuf_window)->frame));
#else
      /* The reason this function isn't `flush_display' in the RIF is
	 that `flush_frame' is also called in many other circumstances
	 when some code wants X requests to be sent to the X server,
	 but there is no corresponding "flush" concept on MS Windows,
	 and flipping buffers every time `flush_frame' is called
	 causes flicker.  */
      w32_flip_buffers_if_dirty (XFRAME (XWINDOW (minibuf_window)->frame));
#endif
    }

  /* Make minibuffer contents into a string.  */
  Fset_buffer (minibuffer);
  if (allow_props || minibuffer_allow_text_properties)
    val = Fminibuffer_contents ();
  else
    val = Fminibuffer_contents_no_properties ();

  /* VAL is the string of minibuffer text.  */

  last_minibuf_string = val;

  /* Choose the string to add to the history.  */
  if (SCHARS (val) != 0)
    histstring = val;
  else if (STRINGP (defalt))
    histstring = defalt;
  else if (CONSP (defalt) && STRINGP (XCAR (defalt)))
    histstring = XCAR (defalt);
  else
    histstring = Qnil;

  /* The appropriate frame will get selected
     in set-window-configuration.  */
  unbind_to (count, Qnil);

  /* Switch the frame back to the calling frame.  */
  if (FRAMEP (calling_frame)
      && FRAME_LIVE_P (XFRAME (calling_frame))
      && (!EQ (selected_frame, calling_frame)
	  || (WINDOW_LIVE_P (XFRAME (calling_frame)->minibuffer_window)
	      && !EQ (XWINDOW (XFRAME (calling_frame)->minibuffer_window)
		      ->frame,
		      calling_frame))))
    calln (Qselect_frame_set_input_focus, calling_frame, Qnil);

  /* Add the value to the appropriate history list, if any.  This is
     done after the previous buffer has been made current again, in
     case the history variable is buffer-local.  */
  if (! (NILP (Vhistory_add_new_input) || NILP (histstring)))
    calln (Qadd_to_history, histvar, histstring);

  /* If Lisp form desired instead of string, parse it.  */
  if (expflag)
    val = string_to_object (val, defalt);

  return val;
}

/* Return true if BUF is a particular existing minibuffer.  */
bool
is_minibuffer (EMACS_INT depth, Lisp_Object buf)
{
  Lisp_Object tail = Fnthcdr (make_fixnum (depth), Vminibuffer_list);
  return
    !NILP (tail)
    && EQ (Fcar (tail), buf);
}

/* Return the DEPTHth minibuffer, or nil if such does not yet exist.  */
static Lisp_Object
nth_minibuffer (EMACS_INT depth)
{
  Lisp_Object tail = Fnthcdr (make_fixnum (depth), Vminibuffer_list);
  return Fcar (tail);
}

/* Set the major mode of the minibuffer BUF, depending on DEPTH, the
   minibuffer depth.  */

static void
set_minibuffer_mode (Lisp_Object buf, EMACS_INT depth)
{
  specpdl_ref count = SPECPDL_INDEX ();

  record_unwind_current_buffer ();
  Fset_buffer (buf);
  if (depth > 0)
    {
      if (!NILP (Ffboundp (Qminibuffer_mode)))
	call0 (Qminibuffer_mode);
    }
  else
    {
      if (!NILP (Ffboundp (Qminibuffer_inactive_mode)))
	call0 (Qminibuffer_inactive_mode);
      else
	Fkill_all_local_variables (Qnil);
    }
  buf = unbind_to (count, buf);
}

/* Return a buffer to be used as the minibuffer at depth `depth'.
   depth = 0 is the lowest allowed argument, and that is the value
   used for nonrecursive minibuffer invocations.  */

Lisp_Object
get_minibuffer (EMACS_INT depth)
{
  Lisp_Object tail = Fnthcdr (make_fixnum (depth), Vminibuffer_list);
  Lisp_Object cll_tail = Fnthcdr (make_fixnum (depth),
				  Vcommand_loop_level_list);
  if (NILP (tail))
    {
      tail = list1 (Qnil);
      Vminibuffer_list = nconc2 (Vminibuffer_list, tail);
      cll_tail = list1 (Qnil);
      Vcommand_loop_level_list = nconc2 (Vcommand_loop_level_list, cll_tail);
    }
  XSETCAR (cll_tail, make_fixnum (depth ? command_loop_level : 0));
  Lisp_Object buf = Fcar (tail);
  if (NILP (buf) || !BUFFER_LIVE_P (XBUFFER (buf)))
    {
      static char const name_fmt[] = " *Minibuf-%"pI"d*";
      char name[sizeof name_fmt + INT_STRLEN_BOUND (EMACS_INT)];
      AUTO_STRING_WITH_LEN (lname, name, sprintf (name, name_fmt, depth));
      buf = Fget_buffer_create (lname, Qnil);
      /* Do this before set_minibuffer_mode.  */
      XSETCAR (tail, buf);
      /* Although the buffer's name starts with a space, undo should be
	 enabled in it.  */
      Fbuffer_enable_undo (buf);
    }
  else
    {
      /* We have to empty both overlay lists.  Otherwise we end
	 up with overlays that think they belong to this buffer
	 while the buffer doesn't know about them any more.  */
      delete_all_overlays (XBUFFER (buf));
      reset_buffer (XBUFFER (buf));
    }

  return buf;
}

static EMACS_INT minibuf_c_loop_level (EMACS_INT depth)
{
  Lisp_Object cll = Fnth (make_fixnum (depth), Vcommand_loop_level_list);
  if (FIXNUMP (cll))
    return XFIXNUM (cll);
  return 0;
}

static void
run_exit_minibuf_hook (Lisp_Object minibuf)
{
  specpdl_ref count = SPECPDL_INDEX ();
  record_unwind_current_buffer ();
  if (BUFFER_LIVE_P (XBUFFER (minibuf)))
    Fset_buffer (minibuf);
  safe_run_hooks (Qminibuffer_exit_hook);
  unbind_to (count, Qnil);
}

/* This variable records the expired minibuffer's frame between the
   calls of `read_minibuf_unwind' and `minibuffer_unwind'.  It should
   be used only by these two functions.  Note that the same search
   method for the MB's frame won't always work in `minibuffer_unwind'
   because the intervening `restore-window-configuration' will have
   changed the buffer in the mini-window.  */
static Lisp_Object exp_MB_frame;

/* This function is called on exiting minibuffer, whether normally or
   not, and it restores the current window, buffer, etc.  */

static void
read_minibuf_unwind (void)
{
  Lisp_Object old_deactivate_mark;
  Lisp_Object calling_frame;
  Lisp_Object calling_window;
  Lisp_Object future_mini_window;
  Lisp_Object saved_selected_frame = selected_frame;
  Lisp_Object window, frames;
  Lisp_Object expired_MB = nth_minibuffer (minibuf_level);
  struct window *w;
  struct frame *f;

  if (NILP (expired_MB))
    emacs_abort ();

  /* Locate the expired minibuffer.  */
  FOR_EACH_FRAME (frames, exp_MB_frame)
    {
      f = XFRAME (exp_MB_frame);
      window = f->minibuffer_window;
      if (WINDOW_LIVE_P (window))
	{
	  w = XWINDOW (window);
	  if (EQ (w->frame, exp_MB_frame)
	      && EQ (w->contents, expired_MB))
	    goto found;
	}
    }
  exp_MB_frame = Qnil;		/* "Can't happen." */

 found:
  if (!EQ (exp_MB_frame, saved_selected_frame)
      && !NILP (exp_MB_frame))
    do_switch_frame (exp_MB_frame, 0, 0, Qt); /* This also sets
					     minibuf_window */

  /* To keep things predictable, in case it matters, let's be in the
     minibuffer when we reset the relevant variables.  Don't depend on
     `minibuf_window' here.  This could by now be the mini-window of any
     frame.  */
  Fset_buffer (expired_MB);
  minibuf_level--;

  /* Restore prompt, etc, from outer minibuffer level.  */
  Lisp_Object key_vec = Fcar (minibuf_save_list);
  this_command_key_count = ASIZE (key_vec);
  this_command_keys = key_vec;
  minibuf_save_list = Fcdr (minibuf_save_list);
  minibuf_prompt = Fcar (minibuf_save_list);
  minibuf_save_list = Fcdr (minibuf_save_list);
  minibuf_prompt_width = XFIXNAT (Fcar (minibuf_save_list));
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
  future_mini_window = Fcar (minibuf_save_list);
  minibuf_save_list = Fcdr (minibuf_save_list);
  calling_frame = Fcar (minibuf_save_list);
  minibuf_save_list = Fcdr (minibuf_save_list);
  calling_window = Fcar (minibuf_save_list);
  minibuf_save_list = Fcdr (minibuf_save_list);

  /* Erase the minibuffer we were using at this level.  */
  {
    specpdl_ref count = SPECPDL_INDEX ();
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
  if (minibuf_level == 0
      || !EQ (selected_frame, WINDOW_FRAME (XWINDOW (future_mini_window))))
    resize_mini_window (XWINDOW (minibuf_window), 0);

  /* Deal with frames that should be removed when exiting the
     minibuffer.  */
  {
    Lisp_Object frames, frame1, val;
    struct frame *f1;

    FOR_EACH_FRAME (frames, frame1)
      {
	f1 = XFRAME (frame1);

	if ((FRAME_PARENT_FRAME (f1)
	     || !NILP (get_frame_param (f1, Qdelete_before)))
	    && !NILP (val = (get_frame_param (f1, Qminibuffer_exit))))
	  {
	    if (EQ (val, Qiconify_frame))
	      Ficonify_frame (frame1);
	    else if (EQ (val, Qdelete_frame))
	      Fdelete_frame (frame1, Qnil);
	    else
	      Fmake_frame_invisible (frame1, Qnil);
	  }
      }
  }

  /* In case the previous minibuffer displayed in this miniwindow is
     dead, we may keep displaying this buffer (tho it's inactive), so reset it,
     to make sure we don't leave around bindings and stuff which only
     made sense during the read_minibuf invocation.  */
  call0 (Qminibuffer_inactive_mode);

  /* We've exited the recursive edit, so switch the current windows
     away from the expired minibuffer window, both in the current
     minibuffer's frame and the original calling frame.  */
  choose_minibuf_frame ();
  if (NILP (XWINDOW (minibuf_window)->prev_buffers))
    {
      if (!EQ (WINDOW_FRAME (XWINDOW (minibuf_window)), calling_frame))
	{
	  Lisp_Object prev = Fprevious_window (minibuf_window, Qnil, Qnil);
	  /* PREV can be on a different frame when we have a minibuffer only
	     frame, the other frame's minibuffer window is MINIBUF_WINDOW,
	     and its "focus window" is also MINIBUF_WINDOW.  */
	  if (!EQ (prev, minibuf_window)
	      && EQ (WINDOW_FRAME (XWINDOW (prev)),
		     WINDOW_FRAME (XWINDOW (minibuf_window))))
	    Fset_frame_selected_window (selected_frame, prev, Qnil);
	}
      else if (WINDOW_LIVE_P (calling_window))
	Fset_frame_selected_window (calling_frame, calling_window, Qnil);
    }

  /* Restore the selected frame. */
  if (!EQ (exp_MB_frame, saved_selected_frame)
      && !NILP (exp_MB_frame))
    do_switch_frame (saved_selected_frame, 0, 0, Qt);
}

/* Replace the expired minibuffer in frame exp_MB_frame with the next less
   nested minibuffer in that frame, if any.  Otherwise, replace it
   with the null minibuffer.  MINIBUF_WINDOW is not changed.  */
static void
minibuffer_unwind (void)
{
  struct frame *f;

  if (NILP (exp_MB_frame)) return; /* "Can't happen." */
  f = XFRAME (exp_MB_frame);
  if (FRAME_LIVE_P (f))
    {
      Lisp_Object window = f->minibuffer_window;

      if (WINDOW_LIVE_P (window))
	{
	  struct window *w = XWINDOW (window);

	  /* minibuf_window = sf->minibuffer_window; */
	  if (!NILP (w->prev_buffers))
	    {
	      Lisp_Object entry = Fcar (w->prev_buffers);

	      if (BUFFERP (Fcar (entry))
		  && BUFFER_LIVE_P (XBUFFER (Fcar (entry))))
		{
		  wset_prev_buffers (w, Fcdr (w->prev_buffers));
		  set_window_buffer (window, Fcar (entry), 0, 0);
		  Fset_window_start (window, Fcar (Fcdr (entry)), Qnil);
		  Fset_window_point (window, Fcar (Fcdr (Fcdr (entry))));
		}
	      else
		set_window_buffer (window, nth_minibuffer (0), 0, 0);
	    }
	  else
	    set_window_buffer (window, nth_minibuffer (0), 0, 0);
	}
    }
}



void
barf_if_interaction_inhibited (void)
{
  if (inhibit_interaction)
    xsignal0 (Qinhibited_interaction);
}

DEFUN ("read-from-minibuffer", Fread_from_minibuffer,
       Sread_from_minibuffer, 1, 7, 0,
       doc: /* Read and return a string from the minibuffer, prompting with string PROMPT.
The optional second arg INITIAL-CONTENTS is an obsolete alternative to
  DEFAULT-VALUE.  It normally should be nil in new code, except when
  HIST is a cons.  It is discussed in more detail below.

Third arg KEYMAP is a keymap to use whilst reading;
  if omitted or nil, the default is `minibuffer-local-map'.

If fourth arg READ is non-nil, interpret the result as a Lisp object
  and return that object:
  in other words, do `(car (read-from-string INPUT-STRING))'

Fifth arg HIST, if non-nil, specifies a history list and optionally
  the initial position in the list.  It can be a symbol, which is the
  history list variable to use, or a cons cell (HISTVAR . HISTPOS).
  In that case, HISTVAR is the history list variable to use, and
  HISTPOS is the initial position for use by the minibuffer history
  commands.  For consistency, you should also specify that element of
  the history as the value of INITIAL-CONTENTS.  Positions are counted
  starting from 1 at the beginning of the list.  If HIST is nil, the
  default history list `minibuffer-history' is used.  If HIST is t,
  history is not recorded.

  If `history-add-new-input' is non-nil (the default), the result will
  be added to the history list using `add-to-history'.

Sixth arg DEFAULT-VALUE, if non-nil, should be a string, which is used
  as the default to `read' if READ is non-nil and the user enters
  empty input.  But if READ is nil, this function does _not_ return
  DEFAULT-VALUE for empty input!  Instead, it returns the empty string.

  Whatever the value of READ, DEFAULT-VALUE is made available via the
  minibuffer history commands.  DEFAULT-VALUE can also be a list of
  strings, in which case all the strings are available in the history,
  and the first string is the default to `read' if READ is non-nil.

Seventh arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
 the current input method and the setting of `enable-multibyte-characters'.

If the variable `minibuffer-allow-text-properties' is non-nil
 (either let-bound or buffer-local in the minibuffer),
 then the string which is returned includes whatever text properties
 were present in the minibuffer.  Otherwise the value has no text properties.

If `inhibit-interaction' is non-nil, this function will signal an
  `inhibited-interaction' error.

The remainder of this documentation string describes the
INITIAL-CONTENTS argument in more detail.  It is only relevant when
studying existing code, or when HIST is a cons.  If non-nil,
INITIAL-CONTENTS is a string to be inserted into the minibuffer before
reading input.  Normally, point is put at the end of that string.
However, if INITIAL-CONTENTS is (STRING . POSITION), the initial
input is STRING, but point is placed at _one-indexed_ position
POSITION in the minibuffer.  Any integer value less than or equal to
one puts point at the beginning of the string.  *Note* that this
behavior differs from the way such arguments are used in `completing-read'
and some related functions, which use zero-indexing for POSITION.  */)
  (Lisp_Object prompt, Lisp_Object initial_contents, Lisp_Object keymap, Lisp_Object read, Lisp_Object hist, Lisp_Object default_value, Lisp_Object inherit_input_method)
{
  Lisp_Object histvar, histpos, val;

  barf_if_interaction_inhibited ();

  CHECK_STRING (prompt);
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

#ifdef HAVE_TEXT_CONVERSION
  /* If overriding-text-conversion-style is set, assume that it was
     changed prior to this call and force text conversion to be reset,
     since redisplay might conclude that the value was retained
     unmodified from a previous call to Fread_from_minibuffer as the
     selected window will not have changed.  */
  if (!EQ (Voverriding_text_conversion_style, Qlambda)
      /* Separate minibuffer frames are not material here, since they
         will already be selected if the situation that this is meant to
         prevent is possible.  */
      && FRAME_WINDOW_P (SELECTED_FRAME ()))
    reset_frame_conversion (SELECTED_FRAME ());
#endif /* HAVE_TEXT_CONVERSION */

  val = read_minibuf (keymap, initial_contents, prompt,
		      !NILP (read),
		      histvar, histpos, default_value,
		      minibuffer_allow_text_properties,
		      !NILP (inherit_input_method));
  return val;
}

/* Functions that use the minibuffer to read various things.  */

DEFUN ("read-string", Fread_string, Sread_string, 1, 5, 0,
       doc: /* Read and return a string from the minibuffer, prompting with PROMPT.

PROMPT is a string, which should normally end with the string ": ".

If non-nil, second arg INITIAL-INPUT is a string to insert before
reading.  This argument has been superseded by DEFAULT-VALUE and should
normally be nil in new code.  It behaves as INITIAL-CONTENTS in
`read-from-minibuffer' (which see).

The third arg HISTORY, if non-nil, specifies a history list and
optionally the initial position in the list.

See `read-from-minibuffer' for details of HISTORY argument.

Fourth arg DEFAULT-VALUE is the default value or the list of default
values.  If non-nil, it is used for history commands, and as the value
(or the first element of the list of default values) to return if the
user enters the empty string.

Fifth arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer
inherits the current input method and the setting of
`enable-multibyte-characters'.  */)
  (Lisp_Object prompt, Lisp_Object initial_input, Lisp_Object history, Lisp_Object default_value, Lisp_Object inherit_input_method)
{
  Lisp_Object val;
  specpdl_ref count = SPECPDL_INDEX ();

  /* Just in case we're in a recursive minibuffer, make it clear that the
     previous minibuffer's completion table does not apply to the new
     minibuffer.
     FIXME: `minibuffer-completion-table' should be buffer-local instead.  */
  specbind (Qminibuffer_completion_table, Qnil);

  val = Fread_from_minibuffer (prompt, initial_input, Qnil,
			       Qnil, history, default_value,
			       inherit_input_method);
  if (STRINGP (val) && SCHARS (val) == 0 && ! NILP (default_value))
    val = CONSP (default_value) ? XCAR (default_value) : default_value;
  return unbind_to (count, val);
}

DEFUN ("read-command", Fread_command, Sread_command, 1, 2, 0,
       doc: /* Read the name of a command and return it as a symbol.
Prompt with PROMPT.  By default, return DEFAULT-VALUE or its first element
if it is a list.  If DEFAULT-VALUE is omitted or nil, and the user enters
null input, return a symbol whose name is an empty string.  */)
  (Lisp_Object prompt, Lisp_Object default_value)
{
  Lisp_Object name, default_string;

  if (NILP (default_value))
    default_string = Qnil;
  else if (SYMBOLP (default_value))
    default_string = SYMBOL_NAME (default_value);
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
  (Lisp_Object prompt)
{
  return Fintern (Fcompleting_read (prompt, Vobarray, Qfboundp, Qt, Qnil, Qnil, Qnil, Qnil),
		  Qnil);
}
#endif /* NOTDEF */

DEFUN ("read-variable", Fread_variable, Sread_variable, 1, 2, 0,
       doc: /* Read the name of a user option and return it as a symbol.
Prompt with PROMPT.  By default, return DEFAULT-VALUE or its first element
if it is a list of strings.
A user option, or customizable variable, is one for which
`custom-variable-p' returns non-nil.  */)
  (Lisp_Object prompt, Lisp_Object default_value)
{
  Lisp_Object name, default_string;

  if (NILP (default_value))
    default_string = Qnil;
  else if (SYMBOLP (default_value))
    default_string = SYMBOL_NAME (default_value);
  else
    default_string = default_value;

  name = Fcompleting_read (prompt, Vobarray,
			   Qcustom_variable_p, Qt,
			   Qnil, Qcustom_variable_history,
			   default_string, Qnil);
  if (NILP (name))
    return name;
  return Fintern (name, Qnil);
}

DEFUN ("read-buffer", Fread_buffer, Sread_buffer, 1, 4, 0,
       doc: /* Read the name of a buffer and return it as a string.
Prompt with PROMPT, which should be a string ending with a colon and a space.
Provides completion on buffer names the user types.
Optional second arg DEF is value to return if user enters an empty line,
 instead of that empty string.
 If DEF is a list of default values, return its first element.
Optional third arg REQUIRE-MATCH has the same meaning as the
 REQUIRE-MATCH argument of `completing-read'.
Optional arg PREDICATE, if non-nil, is a function limiting the buffers that
can be considered.  It will be called with each potential candidate, in
the form of either a string or a cons cell whose `car' is a string, and
should return non-nil to accept the candidate for completion, nil otherwise.
If `read-buffer-completion-ignore-case' is non-nil, completion ignores
case while reading the buffer name.
If `read-buffer-function' is non-nil, this works by calling it as a
function, instead of the usual behavior.  */)
  (Lisp_Object prompt, Lisp_Object def, Lisp_Object require_match,
   Lisp_Object predicate)
{
  Lisp_Object result;
  char *s;
  ptrdiff_t len;
  specpdl_ref count = SPECPDL_INDEX ();

  if (BUFFERP (def))
    def = BVAR (XBUFFER (def), name);

  specbind (Qcompletion_ignore_case,
	    read_buffer_completion_ignore_case ? Qt : Qnil);

  if (NILP (Vread_buffer_function))
    {
      if (!NILP (def))
	{
	  /* A default value was provided: we must change PROMPT,
	     editing the default value in before the colon.  To achieve
	     this, we replace PROMPT with a substring that doesn't
	     contain the terminal space and colon (if present).  They
	     are then added back using Fformat.  */

	  if (STRINGP (prompt))
	    {
	      s = SSDATA (prompt);
	      len = SBYTES (prompt);
	      if (len >= 2 && s[len - 2] == ':' && s[len - 1] == ' ')
		len = len - 2;
	      else if (len >= 1 && (s[len - 1] == ':' || s[len - 1] == ' '))
		len--;

	      prompt = make_specified_string (s, -1, len,
					      STRING_MULTIBYTE (prompt));
	    }

	  prompt = calln (Qformat_prompt, prompt,
			  CONSP (def) ? XCAR (def) : def);
	}

      result = Fcompleting_read (prompt, Qinternal_complete_buffer,
				 predicate, require_match, Qnil,
				 Qbuffer_name_history, def, Qnil);
    }
  else
    result = (NILP (predicate)
	      /* Partial backward compatibility for older read_buffer_functions
		 which don't expect a `predicate' argument.  */
	      ? calln (Vread_buffer_function, prompt, def, require_match)
	      : calln (Vread_buffer_function, prompt, def, require_match,
		       predicate));
  return unbind_to (count, result);
}

static bool
match_regexps (Lisp_Object string, Lisp_Object regexps,
	       bool ignore_case)
{
  ptrdiff_t val;
  for (; CONSP (regexps); regexps = XCDR (regexps))
    {
      CHECK_STRING (XCAR (regexps));

      val = fast_string_match_internal
	(XCAR (regexps), string,
	 (ignore_case ? BVAR (current_buffer, case_canon_table) : Qnil));

      if (val == -2)
	error ("Stack overflow in regexp matcher");
      if (val < 0)
	return false;
    }
  return true;
}

DEFUN ("try-completion", Ftry_completion, Stry_completion, 2, 3, 0,
       doc: /* Return longest common substring of all completions of STRING in COLLECTION.

Test each possible completion specified by COLLECTION
to see if it begins with STRING.  The possible completions may be
strings or symbols.  Symbols are converted to strings before testing,
by using `symbol-name'.

If no possible completions match, the function returns nil; if
there's just one exact match, it returns t; otherwise it returns
the longest initial substring common to all possible completions
that begin with STRING.

If COLLECTION is an alist, the keys (cars of elements) are the
possible completions.  If an element is not a cons cell, then the
element itself is a possible completion.
If COLLECTION is a hash-table, all the keys that are either strings
or symbols are the possible completions.
If COLLECTION is an obarray, the names of all symbols in the obarray
are the possible completions.

COLLECTION can also be a function to do the completion itself.
It receives three arguments: STRING, PREDICATE and nil.
Whatever it returns becomes the value of `try-completion'.

If optional third argument PREDICATE is non-nil, it must be a function
of one or two arguments, and is used to test each possible completion.
A possible completion is accepted only if PREDICATE returns non-nil.

The argument given to PREDICATE is either a string or a cons cell (whose
car is a string) from the alist, or a symbol from the obarray.
If COLLECTION is a hash-table, PREDICATE is called with two arguments:
the string key and the associated value.

To be acceptable, a possible completion must also match all the regexps
in `completion-regexp-list' (unless COLLECTION is a function, in
which case that function should itself handle `completion-regexp-list').

If `completion-ignore-case' is non-nil, possible completions are matched
while ignoring letter-case, but no guarantee is made about the letter-case
of the return value, except that it comes either from the user's input
or from one of the possible completions.  */)
  (Lisp_Object string, Lisp_Object collection, Lisp_Object predicate)
{

  Lisp_Object bestmatch, tail, elt, eltstring;
  /* Size in bytes of BESTMATCH.  */
  ptrdiff_t bestmatchsize = 0;
  /* These are in bytes, too.  */
  ptrdiff_t compare, matchsize;
  if (VECTORP (collection))
    collection = check_obarray (collection);
  enum { function_table, list_table, obarray_table, hash_table}
    type = (HASH_TABLE_P (collection) ? hash_table
	    : OBARRAYP (collection) ? obarray_table
	    : ((NILP (collection)
		|| (CONSP (collection) && !FUNCTIONP (collection)))
	       ? list_table : function_table));
  ptrdiff_t idx = 0;
  int matchcount = 0;
  Lisp_Object bucket, zero, end, tem;

  CHECK_STRING (string);
  if (type == function_table)
    return calln (collection, string, predicate, Qnil);

  bestmatch = bucket = Qnil;
  zero = make_fixnum (0);

  /* If COLLECTION is not a list, set TAIL just for gc pro.  */
  tail = collection;
  obarray_iter_t obit;
  if (type == obarray_table)
    obit = make_obarray_iter (XOBARRAY (collection));

  while (1)
    {
      /* Get the next element of the alist, obarray, or hash-table.  */
      /* Exit the loop if the elements are all used up.  */
      /* elt gets the alist element or symbol.
	 eltstring gets the name to check as a completion.  */

      if (type == list_table)
	{
	  if (!CONSP (tail))
	    break;
	  elt = XCAR (tail);
	  eltstring = CONSP (elt) ? XCAR (elt) : elt;
	  tail = XCDR (tail);
	}
      else if (type == obarray_table)
	{
	  if (obarray_iter_at_end (&obit))
	    break;
	  elt = eltstring = obarray_iter_symbol (&obit);
	  obarray_iter_step (&obit);
	}
      else /* if (type == hash_table) */
	{
	  while (idx < HASH_TABLE_SIZE (XHASH_TABLE (collection))
		 && hash_unused_entry_key_p (HASH_KEY (XHASH_TABLE (collection),
						       idx)))
	    idx++;
	  if (idx >= HASH_TABLE_SIZE (XHASH_TABLE (collection)))
	    break;
	  else
	    elt = eltstring = HASH_KEY (XHASH_TABLE (collection), idx++);
	}

      /* Is this element a possible completion?  */

      if (SYMBOLP (eltstring))
	eltstring = Fsymbol_name (eltstring);

      if (STRINGP (eltstring)
	  && SCHARS (string) <= SCHARS (eltstring)
	  && (tem = Fcompare_strings (eltstring, zero,
				      make_fixnum (SCHARS (string)),
				      string, zero, Qnil,
				      completion_ignore_case ? Qt : Qnil),
	      EQ (Qt, tem)))
	{
	  /* Ignore this element if it fails to match all the regexps.  */
	  if (!match_regexps (eltstring, Vcompletion_regexp_list,
			      completion_ignore_case))
	    continue;

	  /* Ignore this element if there is a predicate
	     and the predicate doesn't like it.  */

	  if (!NILP (predicate))
	    {
	      if (EQ (predicate, Qcommandp))
		tem = Fcommandp (elt, Qnil);
	      else
		{
		  if (type == hash_table)
		    tem = calln (predicate, elt,
				 HASH_VALUE (XHASH_TABLE (collection),
					     idx - 1));
		  else
		    tem = calln (predicate, elt);
		}
	      if (NILP (tem)) continue;
	    }

	  /* Update computation of how much all possible completions match */

	  if (NILP (bestmatch))
	    {
	      matchcount = 1;
	      bestmatch = eltstring;
	      bestmatchsize = SCHARS (eltstring);
	    }
	  else
	    {
	      compare = min (bestmatchsize, SCHARS (eltstring));
	      Lisp_Object lcompare = make_fixnum (compare);
	      tem = Fcompare_strings (bestmatch, zero, lcompare,
				      eltstring, zero, lcompare,
				      completion_ignore_case ? Qt : Qnil);
	      matchsize = EQ (tem, Qt) ? compare : eabs (XFIXNUM (tem)) - 1;

	      Lisp_Object old_bestmatch = bestmatch;
	      if (completion_ignore_case)
		{
		  /* If this is an exact match except for case,
		     use it as the best match rather than one that is not an
		     exact match.  This way, we get the case pattern
		     of the actual match.  */
		  if ((matchsize == SCHARS (eltstring)
		       && matchsize < SCHARS (bestmatch))
		      ||
		      /* If there is more than one exact match ignoring case,
			 and one of them is exact including case,
			 prefer that one.  */
		      /* If there is no exact match ignoring case,
			 prefer a match that does not change the case
			 of the input.  */
		      ((matchsize == SCHARS (eltstring))
		       ==
		       (matchsize == SCHARS (bestmatch))
		       && (tem = Fcompare_strings (eltstring, zero,
						   make_fixnum (SCHARS (string)),
						   string, zero,
						   Qnil,
						   Qnil),
			   EQ (Qt, tem))
		       && (tem = Fcompare_strings (bestmatch, zero,
						   make_fixnum (SCHARS (string)),
						   string, zero,
						   Qnil,
						   Qnil),
			   ! EQ (Qt, tem))))
		    bestmatch = eltstring;
		}
	      if (bestmatchsize != SCHARS (eltstring)
		  || bestmatchsize != matchsize
		  || (completion_ignore_case
		      && !BASE_EQ (Fcompare_strings (old_bestmatch, zero,
						     lcompare, eltstring, zero,
						     lcompare, Qnil),
				   Qt)))
		/* Don't count the same string multiple times.  */
		matchcount += matchcount <= 1;
	      bestmatchsize = matchsize;
	      if (matchsize <= SCHARS (string)
		  /* If completion-ignore-case is non-nil, don't
		     short-circuit because we want to find the best
		     possible match *including* case differences.  */
		  && !completion_ignore_case
		  && matchcount > 1)
		/* No need to look any further.  */
		break;
	    }
	}
    }

  if (NILP (bestmatch))
    return Qnil;		/* No completions found.  */

  /* Return t if the supplied string is an exact match (counting case);
     it does not require any change to be made.  */
  if (matchcount == 1 && !NILP (Fequal (bestmatch, string)))
    return Qt;

  XSETFASTINT (zero, 0);		/* Else extract the part in which */
  XSETFASTINT (end, bestmatchsize);	/* all completions agree.  */
  return Fsubstring (bestmatch, zero, end);
}

DEFUN ("all-completions", Fall_completions, Sall_completions, 2, 3, 0,
       doc: /* Search for partial matches of STRING in COLLECTION.

Test each possible completion specified by COLLECTION
to see if it begins with STRING.  The possible completions may be
strings or symbols.  Symbols are converted to strings before testing,
by using `symbol-name'.

The value is a list of all the possible completions that match STRING.

If COLLECTION is an alist, the keys (cars of elements) are the
possible completions.  If an element is not a cons cell, then the
element itself is the possible completion.
If COLLECTION is a hash-table, all the keys that are strings or symbols
are the possible completions.
If COLLECTION is an obarray, the names of all symbols in the obarray
are the possible completions.

COLLECTION can also be a function to do the completion itself.
It receives three arguments: STRING, PREDICATE and t.
Whatever it returns becomes the value of `all-completions'.

If optional third argument PREDICATE is non-nil, it must be a function
of one or two arguments, and is used to test each possible completion.
A possible completion is accepted only if PREDICATE returns non-nil.

The argument given to PREDICATE is either a string or a cons cell (whose
car is a string) from the alist, or a symbol from the obarray.
If COLLECTION is a hash-table, PREDICATE is called with two arguments:
the string key and the associated value.

To be acceptable, a possible completion must also match all the regexps
in `completion-regexp-list' (unless COLLECTION is a function, in
which case that function should itself handle `completion-regexp-list').  */)
  (Lisp_Object string, Lisp_Object collection, Lisp_Object predicate)
{
  Lisp_Object tail, elt, eltstring;
  Lisp_Object allmatches;
  if (VECTORP (collection))
    collection = check_obarray (collection);
  int type = (HASH_TABLE_P (collection)
	      ? 3 : (OBARRAYP (collection)
		     ? 2 : ((NILP (collection)
			     || (CONSP (collection)
				 && !FUNCTIONP (collection)))
			    ? 1 : 0)));
  ptrdiff_t idx = 0;
  Lisp_Object bucket, tem, zero;

  CHECK_STRING (string);
  if (type == 0)
    return calln (collection, string, predicate, Qt);
  allmatches = bucket = Qnil;
  zero = make_fixnum (0);

  /* If COLLECTION is not a list, set TAIL just for gc pro.  */
  tail = collection;
  obarray_iter_t obit;
  if (type == 2)
    obit = make_obarray_iter (XOBARRAY (collection));

  while (1)
    {
      /* Get the next element of the alist, obarray, or hash-table.  */
      /* Exit the loop if the elements are all used up.  */
      /* elt gets the alist element or symbol.
	 eltstring gets the name to check as a completion.  */

      if (type == 1)
	{
	  if (!CONSP (tail))
	    break;
	  elt = XCAR (tail);
	  eltstring = CONSP (elt) ? XCAR (elt) : elt;
	  tail = XCDR (tail);
	}
      else if (type == 2)
	{
	  if (obarray_iter_at_end (&obit))
	    break;
	  elt = eltstring = obarray_iter_symbol (&obit);
	  obarray_iter_step (&obit);
	}
      else /* if (type == 3) */
	{
	  while (idx < HASH_TABLE_SIZE (XHASH_TABLE (collection))
		 && hash_unused_entry_key_p (HASH_KEY (XHASH_TABLE (collection),
						       idx)))
	    idx++;
	  if (idx >= HASH_TABLE_SIZE (XHASH_TABLE (collection)))
	    break;
	  else
	    elt = eltstring = HASH_KEY (XHASH_TABLE (collection), idx++);
	}

      /* Is this element a possible completion?  */

      if (SYMBOLP (eltstring))
	eltstring = Fsymbol_name (eltstring);

      if (STRINGP (eltstring)
	  && SCHARS (string) <= SCHARS (eltstring)
	  && (tem = Fcompare_strings (eltstring, zero,
				      make_fixnum (SCHARS (string)),
				      string, zero,
				      make_fixnum (SCHARS (string)),
				      completion_ignore_case ? Qt : Qnil),
	      EQ (Qt, tem)))
	{
	  /* Ignore this element if it fails to match all the regexps.  */
	  if (!match_regexps (eltstring, Vcompletion_regexp_list,
			      completion_ignore_case))
	    continue;

	  /* Ignore this element if there is a predicate
	     and the predicate doesn't like it.  */

	  if (!NILP (predicate))
	    {
	      if (EQ (predicate, Qcommandp))
		tem = Fcommandp (elt, Qnil);
	      else
		{
		  if (type == 3)
		    tem = calln (predicate, elt,
				 HASH_VALUE (XHASH_TABLE (collection),
					     idx - 1));
		  else
		    tem = calln (predicate, elt);
		}
	      if (NILP (tem)) continue;
	    }
	  /* Ok => put it on the list.  */
	  allmatches = Fcons (eltstring, allmatches);
	}
    }

  return Fnreverse (allmatches);
}

DEFUN ("completing-read", Fcompleting_read, Scompleting_read, 2, 8, 0,
       doc: /* Read a string in the minibuffer, with completion.
While in the minibuffer, you can use \\<minibuffer-local-completion-map>\\[minibuffer-complete] and \\[minibuffer-complete-word] to complete your input.
You can also use \\<minibuffer-local-map>\\[minibuffer-complete-history] to complete using history items in the
input history HIST, and you can use \\[minibuffer-complete-defaults] to complete using
the default items in DEFAULT-VALUE.

PROMPT is a string to prompt with; normally it ends in a colon and a space.
COLLECTION can be a list of strings, an alist, an obarray or a hash table.
COLLECTION can also be a function to do the completion itself.
PREDICATE limits completion to a subset of COLLECTION.
See `try-completion', `all-completions', `test-completion',
and `completion-boundaries', for more details on completion,
COLLECTION, and PREDICATE.  See also Info node `(elisp)Basic Completion'
for the details about completion, and Info node `(elisp)Programmed
Completion' for expectations from COLLECTION when it's a function.

REQUIRE-MATCH can take the following values:
- t means that the user is not allowed to exit unless the input is (or
  completes to) an element of COLLECTION or is null.
- nil means that the user can exit with any input.
- `confirm' means that the user can exit with any input, but she needs
  to confirm her choice if the input is not an element of COLLECTION.
- `confirm-after-completion' means that the user can exit with any
  input, but she needs to confirm her choice if she called
  `minibuffer-complete' right before `minibuffer-complete-and-exit'
  and the input is not an element of COLLECTION.
- a function, which will be called with the input as the
  argument.  If the function returns a non-nil value, the
  minibuffer is exited with that argument as the value.
- anything else behaves like t except that typing RET does not exit if it
  does non-null completion.

If the input is null, `completing-read' returns DEF, or the first
element of the list of default values, or an empty string if DEF is
nil, regardless of the value of REQUIRE-MATCH.

If INITIAL-INPUT is non-nil, insert it in the minibuffer initially,
  with point positioned at the end.  If it is (STRING . POSITION), the
  initial input is STRING, but point is placed at _zero-indexed_
  position POSITION in STRING.  (*Note* that this is different from
  `read-from-minibuffer' and related functions, which use one-indexing
  for POSITION.)  This feature is deprecated--it is best to pass nil
  for INITIAL-INPUT and supply the default value DEF instead.  The
  user can yank the default value into the minibuffer easily using
  \\<minibuffer-local-map>\\[next-history-element].

HIST, if non-nil, specifies a history list and optionally the initial
  position in the list.  It can be a symbol, which is the history list
  variable to use, or it can be a cons cell (HISTVAR . HISTPOS).  In
  that case, HISTVAR is the history list variable to use, and HISTPOS
  is the initial position (the position in the list used by the
  minibuffer history commands).  For consistency, you should also
  specify that element of the history as the value of INITIAL-INPUT.
  (This is the only case in which you should use INITIAL-INPUT instead
  of DEF.)  Positions are counted starting from 1 at the beginning of
  the list.  The variable `history-length' controls the maximum length
  of a history list.  If HIST is t, history is not recorded.

DEF, if non-nil, is the default value or the list of default values.

If INHERIT-INPUT-METHOD is non-nil, the minibuffer inherits the
  current input method and the setting of `enable-multibyte-characters'.

Completion ignores case if the ambient value of
  `completion-ignore-case' is non-nil.

See also `completing-read-function'.  */)
  (Lisp_Object prompt, Lisp_Object collection, Lisp_Object predicate, Lisp_Object require_match, Lisp_Object initial_input, Lisp_Object hist, Lisp_Object def, Lisp_Object inherit_input_method)
{
  return calln (Fsymbol_value (Qcompleting_read_function),
		prompt, collection, predicate, require_match, initial_input,
		hist, def, inherit_input_method);
}

/* Test whether TXT is an exact completion.  */
DEFUN ("test-completion", Ftest_completion, Stest_completion, 2, 3, 0,
       doc: /* Return non-nil if STRING is a valid completion.
For instance, if COLLECTION is a list of strings, STRING is a
valid completion if it appears in the list and PREDICATE is satisfied.

Takes the same arguments as `all-completions' and `try-completion'.

If COLLECTION is a function, it is called with three arguments:
the values STRING, PREDICATE and `lambda'.  */)
  (Lisp_Object string, Lisp_Object collection, Lisp_Object predicate)
{
  Lisp_Object tem = Qnil, arg = Qnil;

  CHECK_STRING (string);

  if (NILP (collection) || (CONSP (collection) && !FUNCTIONP (collection)))
    {
      tem = Fassoc_string (string, collection, completion_ignore_case ? Qt : Qnil);
      if (NILP (tem))
	return Qnil;
    }
  else if (OBARRAYP (collection) || VECTORP (collection))
    {
      collection = check_obarray (collection);
      /* Bypass intern-soft as that loses for nil.  */
      tem = oblookup (collection,
		      SSDATA (string),
		      SCHARS (string),
		      SBYTES (string));
      if (completion_ignore_case && !BARE_SYMBOL_P (tem))
	DOOBARRAY (XOBARRAY (collection), it)
	  {
	    Lisp_Object obj = obarray_iter_symbol (&it);
	    if (BASE_EQ (Fcompare_strings (string, make_fixnum (0),
					   Qnil,
					   Fsymbol_name (obj),
					   make_fixnum (0) , Qnil, Qt),
			 Qt))
	      {
		tem = obj;
		break;
	      }
	  }

      if (!BARE_SYMBOL_P (tem))
	return Qnil;
    }
  else if (HASH_TABLE_P (collection))
    {
      struct Lisp_Hash_Table *h = XHASH_TABLE (collection);
      ptrdiff_t i = hash_find (h, string);
      if (i >= 0)
        {
          tem = HASH_KEY (h, i);
          arg = HASH_VALUE (h, i);
          goto found_matching_key;
        }
      else
	DOHASH (h, k, v)
          {
            tem = k;
            Lisp_Object strkey = (SYMBOLP (tem) ? Fsymbol_name (tem) : tem);
            if (!STRINGP (strkey)) continue;
            if (BASE_EQ (Fcompare_strings (string, Qnil, Qnil,
					   strkey, Qnil, Qnil,
					   completion_ignore_case ? Qt : Qnil),
			 Qt))
	      {
                arg = v;
                goto found_matching_key;
              }
          }
      return Qnil;
    found_matching_key: ;
    }
  else
    return calln (collection, string, predicate, Qlambda);

  /* Reject this element if it fails to match all the regexps.  */
  if (!match_regexps (string, Vcompletion_regexp_list,
		      completion_ignore_case))
    return Qnil;

  /* Finally, check the predicate.  */
  if (!NILP (predicate))
    {
      return HASH_TABLE_P (collection)
	? calln (predicate, tem, arg)
	: calln (predicate, tem);
    }
  else
    return Qt;
}

DEFUN ("internal-complete-buffer", Finternal_complete_buffer, Sinternal_complete_buffer, 3, 3, 0,
       doc: /* Perform completion on buffer names.
STRING and PREDICATE have the same meanings as in `try-completion',
`all-completions', and `test-completion'.

If FLAG is nil, invoke `try-completion'; if it is t, invoke
`all-completions'; otherwise invoke `test-completion'.  */)
  (Lisp_Object string, Lisp_Object predicate, Lisp_Object flag)
{
  if (NILP (flag))
    return Ftry_completion (string, Vbuffer_alist, predicate);
  else if (EQ (flag, Qt))
    {
      Lisp_Object res = Fall_completions (string, Vbuffer_alist, predicate);
      if (SCHARS (string) > 0)
	return res;
      else
	{ /* Strip out internal buffers.  */
	  Lisp_Object bufs = res;
	  /* First, look for a non-internal buffer in `res'.  */
	  while (CONSP (bufs) && SREF (XCAR (bufs), 0) == ' ')
	    bufs = XCDR (bufs);
	  if (NILP (bufs))
	    return (list_length (res) == list_length (Vbuffer_alist)
		    /* If all bufs are internal don't strip them out.  */
		    ? res : bufs);
	  res = bufs;
	  while (CONSP (XCDR (bufs)))
	    if (SREF (XCAR (XCDR (bufs)), 0) == ' ')
	      XSETCDR (bufs, XCDR (XCDR (bufs)));
	    else
	      bufs = XCDR (bufs);
	  return res;
	}
    }
  else if (EQ (flag, Qlambda))
    return Ftest_completion (string, Vbuffer_alist, predicate);
  else if (EQ (flag, Qmetadata))
    return list3 (Qmetadata,
                  Fcons (Qcategory, Qbuffer),
                  Fcons (Qcycle_sort_function, Qidentity));
  else
    return Qnil;
}

/* Like assoc but assumes KEY is a string, and ignores case if appropriate.  */

DEFUN ("assoc-string", Fassoc_string, Sassoc_string, 2, 3, 0,
       doc: /* Like `assoc' but specifically for strings (and symbols).

This returns the first element of LIST whose car matches the string or
symbol KEY, or nil if no match exists.  When performing the
comparison, symbols are first converted to strings, and unibyte
strings to multibyte.  If the optional arg CASE-FOLD is non-nil, both
KEY and the elements of LIST are upcased for comparison.

Unlike `assoc', KEY can also match an entry in LIST consisting of a
single string, rather than a cons cell whose car is a string.  */)
  (register Lisp_Object key, Lisp_Object list, Lisp_Object case_fold)
{
  register Lisp_Object tail;

  if (SYMBOLP (key))
    key = Fsymbol_name (key);

  for (tail = list; CONSP (tail); tail = XCDR (tail))
    {
      register Lisp_Object elt, tem, thiscar;
      elt = XCAR (tail);
      thiscar = CONSP (elt) ? XCAR (elt) : elt;
      if (SYMBOLP (thiscar))
	thiscar = Fsymbol_name (thiscar);
      else if (!STRINGP (thiscar))
	continue;
      tem = Fcompare_strings (thiscar, make_fixnum (0), Qnil,
			      key, make_fixnum (0), Qnil,
			      case_fold);
      if (EQ (tem, Qt))
	return elt;
      maybe_quit ();
    }
  return Qnil;
}


DEFUN ("minibuffer-depth", Fminibuffer_depth, Sminibuffer_depth, 0, 0, 0,
       doc: /* Return current depth of activations of minibuffer, a nonnegative integer.  */)
  (void)
{
  return make_fixnum (minibuf_level);
}

DEFUN ("minibuffer-prompt", Fminibuffer_prompt, Sminibuffer_prompt, 0, 0, 0,
       doc: /* Return the prompt string of the currently-active minibuffer.
If no minibuffer is active, return nil.  */)
  (void)
{
  return Fcopy_sequence (minibuf_prompt);
}



void
set_initial_minibuffer_mode (void)
{
  Lisp_Object minibuf = get_minibuffer (0);
  set_minibuffer_mode (minibuf, 0);
}

static void init_minibuf_once_for_pdumper (void);

void
init_minibuf_once (void)
{
  staticpro (&Vminibuffer_list);
  staticpro (&Vcommand_loop_level_list);
  pdumper_do_now_and_after_load (init_minibuf_once_for_pdumper);
  /* Ensure our inactive minibuffer exists.  */
  get_minibuffer (0);
}

static void
init_minibuf_once_for_pdumper (void)
{
  PDUMPER_IGNORE (minibuf_level);
  PDUMPER_IGNORE (minibuf_prompt_width);

  /* We run this function on first initialization and whenever we
     restore from a dump file.  pdumper doesn't try to preserve
     frames, windows, and so on, so reset everything related here.  */
  Vminibuffer_list = Qnil;
  Vcommand_loop_level_list = Qnil;
  minibuf_level = 0;
  minibuf_prompt = Qnil;
  minibuf_save_list = Qnil;
  last_minibuf_string = Qnil;
}

void
syms_of_minibuf (void)
{
  staticpro (&minibuf_prompt);
  staticpro (&minibuf_save_list);
  staticpro (&MB_frame);
  MB_frame = Qnil;
  staticpro (&exp_MB_frame);

  DEFSYM (Qminibuffer_follows_selected_frame,
          "minibuffer-follows-selected-frame");
  DEFSYM (Qcompletion_ignore_case, "completion-ignore-case");
  DEFSYM (Qminibuffer_default, "minibuffer-default");
  Fset (Qminibuffer_default, Qnil);

  DEFSYM (Qminibuffer_completion_table, "minibuffer-completion-table");

  staticpro (&last_minibuf_string);

  DEFSYM (Qcustom_variable_history, "custom-variable-history");
  Fset (Qcustom_variable_history, Qnil);

  DEFSYM (Qminibuffer_history, "minibuffer-history");
  DEFSYM (Qbuffer_name_history, "buffer-name-history");
  Fset (Qbuffer_name_history, Qnil);

  DEFSYM (Qcustom_variable_p, "custom-variable-p");

  /* Normal hooks for entry to and exit from minibuffer.  */
  DEFSYM (Qminibuffer_setup_hook, "minibuffer-setup-hook");
  DEFSYM (Qminibuffer_exit_hook, "minibuffer-exit-hook");

  DEFSYM (Qcurrent_input_method, "current-input-method");
  DEFSYM (Qactivate_input_method, "activate-input-method");
  DEFSYM (Qmetadata, "metadata");
  DEFSYM (Qcycle_sort_function, "cycle-sort-function");

  /* A frame parameter.  */
  DEFSYM (Qminibuffer_exit, "minibuffer-exit");

  DEFSYM (Qminibuffer_mode, "minibuffer-mode");
  DEFSYM (Qminibuffer_inactive_mode, "minibuffer-inactive-mode");
  DEFSYM (Qminibuffer_completing_file_name, "minibuffer-completing-file-name");
  DEFSYM (Qselect_frame_set_input_focus, "select-frame-set-input-focus");
  DEFSYM (Qadd_to_history, "add-to-history");
  DEFSYM (Qpush_window_buffer_onto_prev, "push-window-buffer-onto-prev");

  DEFVAR_LISP ("read-expression-history", Vread_expression_history,
	       doc: /* A history list for arguments that are Lisp expressions to evaluate.
For example, `eval-expression' uses this.  */);
  Vread_expression_history = Qnil;

  DEFVAR_LISP ("read-buffer-function", Vread_buffer_function,
	       doc: /* If this is non-nil, `read-buffer' does its work by calling this function.
The function is called with the arguments passed to `read-buffer'.  */);
  Vread_buffer_function = Qnil;

  DEFVAR_LISP ("minibuffer-follows-selected-frame", minibuffer_follows_selected_frame,
               doc: /* t means the active minibuffer always displays on the selected frame.
Nil means that a minibuffer will appear only in the frame which created it.
Any other value means the minibuffer will move onto another frame, but
only when the user starts using a minibuffer there.

Any buffer local or dynamic binding of this variable is ignored.  Only the
default top level value is used.  */);
  minibuffer_follows_selected_frame = Qt;

  DEFVAR_BOOL ("read-buffer-completion-ignore-case",
	       read_buffer_completion_ignore_case,
	       doc: /* Non-nil means completion ignores case when reading a buffer name.  */);
  read_buffer_completion_ignore_case = 0;

  DEFVAR_LISP ("minibuffer-setup-hook", Vminibuffer_setup_hook,
	       doc: /* Normal hook run just after entry to minibuffer.  */);
  Vminibuffer_setup_hook = Qnil;

  DEFVAR_LISP ("minibuffer-exit-hook", Vminibuffer_exit_hook,
	       doc: /* Normal hook run whenever a minibuffer is exited.  */);
  Vminibuffer_exit_hook = Qnil;

  DEFVAR_LISP ("history-length", Vhistory_length,
	       doc: /* Maximum length of history lists before truncation takes place.
A number means truncate to that length; truncation deletes old
elements, and is done just after inserting a new element.
A value of t means no truncation.

This variable only affects history lists that don't specify their own
maximum lengths.  Setting the `history-length' property of a history
variable overrides this default.  */);
  XSETFASTINT (Vhistory_length, 100);

  DEFVAR_BOOL ("history-delete-duplicates", history_delete_duplicates,
	       doc: /* Non-nil means to delete duplicates in history.
If set to t when adding a new history element, all previous identical
elements are deleted from the history list.  */);
  history_delete_duplicates = 0;

  DEFVAR_LISP ("history-add-new-input", Vhistory_add_new_input,
	       doc: /* Non-nil means to add new elements in history.
If set to nil, minibuffer reading functions don't add new elements to the
history list, so it is possible to do this afterwards by calling
`add-to-history' explicitly.  */);
  Vhistory_add_new_input = Qt;

  DEFVAR_BOOL ("completion-ignore-case", completion_ignore_case,
	       doc: /* Non-nil means don't consider case significant in completion.
For file-name completion, `read-file-name-completion-ignore-case'
controls the behavior, rather than this variable.
For buffer name completion, `read-buffer-completion-ignore-case'
controls the behavior, rather than this variable.  */);
  completion_ignore_case = 0;

  DEFVAR_BOOL ("enable-recursive-minibuffers", enable_recursive_minibuffers,
	       doc: /* Non-nil means to allow minibuffer commands while in the minibuffer.
This variable makes a difference whenever the minibuffer window is active.
Also see `minibuffer-depth-indicate-mode', which may be handy if this
variable is non-nil. */);
  enable_recursive_minibuffers = 0;

  DEFVAR_LISP ("minibuffer-completion-table", Vminibuffer_completion_table,
	       doc: /* Alist or obarray used for completion in the minibuffer.
This becomes the ALIST argument to `try-completion' and `all-completions'.
The value can also be a list of strings or a hash table.

The value may alternatively be a function, which is given three arguments:
  STRING, the current buffer contents;
  PREDICATE, the predicate for filtering possible matches;
  CODE, which says what kind of things to do.
CODE can be nil, t or `lambda':
  nil    -- return the best completion of STRING, or nil if there is none.
  t      -- return a list of all possible completions of STRING.
  lambda -- return t if STRING is a valid completion as it stands.  */);
  Vminibuffer_completion_table = Qnil;

  DEFVAR_LISP ("minibuffer-completion-predicate", Vminibuffer_completion_predicate,
	       doc: /* Within call to `completing-read', this holds the PREDICATE argument.  */);
  Vminibuffer_completion_predicate = Qnil;

  DEFVAR_LISP ("minibuffer-completion-confirm", Vminibuffer_completion_confirm,
	       doc: /* Whether to demand confirmation of completion before exiting minibuffer.
If nil, confirmation is not required.
If the value is `confirm', the user may exit with an input that is not
 a valid completion alternative, but Emacs asks for confirmation.
If the value is `confirm-after-completion', the user may exit with an
 input that is not a valid completion alternative, but Emacs asks for
 confirmation if the user submitted the input right after any of the
 completion commands listed in `minibuffer-confirm-exit-commands'.  */);
  Vminibuffer_completion_confirm = Qnil;

  DEFVAR_LISP ("minibuffer-completing-file-name",
	       Vminibuffer_completing_file_name,
	       doc: /* Non-nil means completing file names.  */);
  Vminibuffer_completing_file_name = Qnil;

  DEFVAR_LISP ("minibuffer-help-form", Vminibuffer_help_form,
	       doc: /* Value that `help-form' takes on inside the minibuffer.  */);
  Vminibuffer_help_form = Qnil;

  DEFVAR_LISP ("minibuffer-history-variable", Vminibuffer_history_variable,
	       doc: /* History list symbol to add minibuffer values to.
Each string of minibuffer input, as it appears on exit from the minibuffer,
is added with

  (set minibuffer-history-variable
       (cons STRING (symbol-value minibuffer-history-variable)))

 If the variable is t, no history is recorded.  */);
  XSETFASTINT (Vminibuffer_history_variable, 0);

  DEFVAR_LISP ("minibuffer-history-position", Vminibuffer_history_position,
	       doc: /* Current position of redoing in the history list.  */);
  Vminibuffer_history_position = Qnil;

  DEFVAR_BOOL ("minibuffer-auto-raise", minibuffer_auto_raise,
	       doc: /* Non-nil means entering the minibuffer raises the minibuffer's frame.
Some uses of the echo area also raise that frame (since they use it too).  */);
  minibuffer_auto_raise = 0;

  DEFVAR_LISP ("completion-regexp-list", Vcompletion_regexp_list,
	       doc: /* List of regexps that should restrict possible completions.
The basic completion functions only consider a completion acceptable
if it matches all regular expressions in this list, with
`case-fold-search' bound to the value of `completion-ignore-case'.
See Info node `(elisp)Basic Completion', for a description of these
functions.

Do not set this variable to a non-nil value globally, as that is not
safe and will probably cause errors in completion commands.  This
variable should be only let-bound to non-nil values around calls to
basic completion functions like `try-completion' and `all-completions'.  */);
  Vcompletion_regexp_list = Qnil;

  DEFVAR_BOOL ("minibuffer-allow-text-properties",
	       minibuffer_allow_text_properties,
	       doc: /* Non-nil means `read-from-minibuffer' should not discard text properties.
Lisp code can let-bind this, or make it buffer-local in the minibuffer.
This also affects `read-string', or any of the functions that do
minibuffer input with completion, but it does not affect `read-minibuffer'
that always discards text properties.  */);
  minibuffer_allow_text_properties = 0;

  DEFVAR_LISP ("minibuffer-prompt-properties", Vminibuffer_prompt_properties,
	       doc: /* Text properties that are added to minibuffer prompts.
These are in addition to the basic `field' property, and stickiness
properties.  */);
  Vminibuffer_prompt_properties = list2 (Qread_only, Qt);

  DEFVAR_LISP ("read-hide-char", Vread_hide_char,
	       doc: /* Whether to hide input characters in noninteractive mode.
If non-nil, it must be a character, which will be used to mask the
input characters.  This variable should never be set globally.

This variable also overrides the default character that `read-passwd'
uses to hide passwords.  */);
  Vread_hide_char = Qnil;

  DEFVAR_BOOL ("inhibit-interaction",
	       inhibit_interaction,
	       doc: /* Non-nil means any user interaction will signal an error.
This variable can be bound when user interaction can't be performed,
for instance when running a headless Emacs server.  Functions like
`read-from-minibuffer' (and the like) will signal `inhibited-interaction'
instead. */);
  inhibit_interaction = 0;

  DEFVAR_BOOL ("read-minibuffer-restore-windows", read_minibuffer_restore_windows,
	       doc: /* Non-nil means restore window configurations on exit from minibuffer.
If this is non-nil (the default), reading input with the minibuffer will
restore, on exit, the window configurations of the frame where the
minibuffer was entered from and, if it is different, the frame that owns
the associated minibuffer window.

If this is nil, window configurations are not restored upon exiting
the minibuffer.  However, if `minibuffer-restore-windows' is present
in `minibuffer-exit-hook', exiting the minibuffer will remove the window
showing the *Completions* buffer, if any.  */);
  read_minibuffer_restore_windows = true;

  defsubr (&Sactive_minibuffer_window);
  defsubr (&Sset_minibuffer_window);
  defsubr (&Sread_from_minibuffer);
  defsubr (&Sread_string);
  defsubr (&Sread_command);
  defsubr (&Sread_variable);
  defsubr (&Sinternal_complete_buffer);
  defsubr (&Sread_buffer);
  defsubr (&Sminibuffer_depth);
  defsubr (&Sminibuffer_prompt);

  defsubr (&Sminibufferp);
  defsubr (&Sinnermost_minibuffer_p);
  defsubr (&Sminibuffer_innermost_command_loop_p);
  defsubr (&Sabort_minibuffers);
  defsubr (&Sminibuffer_prompt_end);
  defsubr (&Sminibuffer_contents);
  defsubr (&Sminibuffer_contents_no_properties);

  defsubr (&Stry_completion);
  defsubr (&Sall_completions);
  defsubr (&Stest_completion);
  defsubr (&Sassoc_string);
  defsubr (&Scompleting_read);
  DEFSYM (Qminibuffer_quit_recursive_edit, "minibuffer-quit-recursive-edit");
  DEFSYM (Qinternal_complete_buffer, "internal-complete-buffer");
  DEFSYM (Qcompleting_read_function, "completing-read-function");
  DEFSYM (Qformat_prompt, "format-prompt");
}
