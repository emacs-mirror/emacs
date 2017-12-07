/* Functions related to terminal devices.
   Copyright (C) 2005-2017 Free Software Foundation, Inc.

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

#include <stdio.h>

#include "lisp.h"
#include "character.h"
#include "frame.h"
#include "termchar.h"
#include "termhooks.h"
#include "keyboard.h"

#if HAVE_STRUCT_UNIPAIR_UNICODE
# include <errno.h>
# include <linux/kd.h>
# include <sys/ioctl.h>
#endif

/* Chain of all terminals currently in use.  */
struct terminal *terminal_list;

/* The first unallocated terminal id.  */
static int next_terminal_id;

/* The initial terminal device, created by initial_term_init.  */
struct terminal *initial_terminal;

static void delete_initial_terminal (struct terminal *);

/* This setter is used only in this file, so it can be private.  */
static void
tset_param_alist (struct terminal *t, Lisp_Object val)
{
  t->param_alist = val;
}



void
ring_bell (struct frame *f)
{
  if (!NILP (Vring_bell_function))
    {
      Lisp_Object function;

      /* Temporarily set the global variable to nil
	 so that if we get an error, it stays nil
	 and we don't call it over and over.

	 We don't specbind it, because that would carefully
	 restore the bad value if there's an error
	 and make the loop of errors happen anyway.  */

      function = Vring_bell_function;
      Vring_bell_function = Qnil;

      call0 (function);

      Vring_bell_function = function;
    }
  else if (FRAME_TERMINAL (f)->ring_bell_hook)
    (*FRAME_TERMINAL (f)->ring_bell_hook) (f);
}

void
update_begin (struct frame *f)
{
  if (FRAME_TERMINAL (f)->update_begin_hook)
    (*FRAME_TERMINAL (f)->update_begin_hook) (f);
}

void
update_end (struct frame *f)
{
  if (FRAME_TERMINAL (f)->update_end_hook)
    (*FRAME_TERMINAL (f)->update_end_hook) (f);
}

/* Specify how many text lines, from the top of the window,
   should be affected by insert-lines and delete-lines operations.
   This, and those operations, are used only within an update
   that is bounded by calls to update_begin and update_end.  */

void
set_terminal_window (struct frame *f, int size)
{
  if (FRAME_TERMINAL (f)->set_terminal_window_hook)
    (*FRAME_TERMINAL (f)->set_terminal_window_hook) (f, size);
}

/* Move cursor to row/column position VPOS/HPOS.  HPOS/VPOS are
   frame-relative coordinates.  */

void
cursor_to (struct frame *f, int vpos, int hpos)
{
  if (FRAME_TERMINAL (f)->cursor_to_hook)
    (*FRAME_TERMINAL (f)->cursor_to_hook) (f, vpos, hpos);
}

/* Similar but don't take any account of the wasted characters.  */

void
raw_cursor_to (struct frame *f, int row, int col)
{
  if (FRAME_TERMINAL (f)->raw_cursor_to_hook)
    (*FRAME_TERMINAL (f)->raw_cursor_to_hook) (f, row, col);
}

/* Erase operations.  */

/* Clear from cursor to end of frame.  */
void
clear_to_end (struct frame *f)
{
  if (FRAME_TERMINAL (f)->clear_to_end_hook)
    (*FRAME_TERMINAL (f)->clear_to_end_hook) (f);
}

/* Clear entire frame.  */

void
clear_frame (struct frame *f)
{
  if (FRAME_TERMINAL (f)->clear_frame_hook)
    (*FRAME_TERMINAL (f)->clear_frame_hook) (f);
}

/* Clear from cursor to end of line.
   Assume that the line is already clear starting at column first_unused_hpos.

   Note that the cursor may be moved, on terminals lacking a `ce' string.  */

void
clear_end_of_line (struct frame *f, int first_unused_hpos)
{
  if (FRAME_TERMINAL (f)->clear_end_of_line_hook)
    (*FRAME_TERMINAL (f)->clear_end_of_line_hook) (f, first_unused_hpos);
}

/* Output LEN glyphs starting at STRING at the nominal cursor position.
   Advance the nominal cursor over the text.  */

void
write_glyphs (struct frame *f, struct glyph *string, int len)
{
  if (FRAME_TERMINAL (f)->write_glyphs_hook)
    (*FRAME_TERMINAL (f)->write_glyphs_hook) (f, string, len);
}

/* Insert LEN glyphs from START at the nominal cursor position.

   If start is zero, insert blanks instead of a string at start */

void
insert_glyphs (struct frame *f, struct glyph *start, int len)
{
  if (len <= 0)
    return;

  if (FRAME_TERMINAL (f)->insert_glyphs_hook)
    (*FRAME_TERMINAL (f)->insert_glyphs_hook) (f, start, len);
}

/* Delete N glyphs at the nominal cursor position. */

void
delete_glyphs (struct frame *f, int n)
{
  if (FRAME_TERMINAL (f)->delete_glyphs_hook)
    (*FRAME_TERMINAL (f)->delete_glyphs_hook) (f, n);
}

/* Insert N lines at vpos VPOS.  If N is negative, delete -N lines.  */

void
ins_del_lines (struct frame *f, int vpos, int n)
{
  if (FRAME_TERMINAL (f)->ins_del_lines_hook)
    (*FRAME_TERMINAL (f)->ins_del_lines_hook) (f, vpos, n);
}

/* Return the terminal object specified by TERMINAL.  TERMINAL may
   be a terminal object, a frame, or nil for the terminal device of
   the current frame.  If TERMINAL is neither from the above or the
   resulting terminal object is deleted, return NULL.  */

static struct terminal *
decode_terminal (Lisp_Object terminal)
{
  struct terminal *t;

  if (NILP (terminal))
    terminal = selected_frame;
  t = (TERMINALP (terminal)
       ? XTERMINAL (terminal)
       : FRAMEP (terminal) ? FRAME_TERMINAL (XFRAME (terminal)) : NULL);
  return t && t->name ? t : NULL;
}

/* Like above, but throw an error if TERMINAL is not valid or deleted.  */

struct terminal *
decode_live_terminal (Lisp_Object terminal)
{
  struct terminal *t = decode_terminal (terminal);

  if (!t)
    wrong_type_argument (Qterminal_live_p, terminal);
  return t;
}

/* Like decode_terminal, but ensure that the resulting terminal object refers
   to a text-based terminal device.  */

struct terminal *
decode_tty_terminal (Lisp_Object terminal)
{
  struct terminal *t = decode_live_terminal (terminal);

  return (t->type == output_termcap || t->type == output_msdos_raw) ? t : NULL;
}

/* Return an active (not suspended) text-based terminal device that uses
   the tty device with the given NAME, or NULL if the named terminal device
   is not opened.  */

struct terminal *
get_named_terminal (const char *name)
{
  struct terminal *t;

  eassert (name);

  for (t = terminal_list; t; t = t->next_terminal)
    {
      if ((t->type == output_termcap || t->type == output_msdos_raw)
          && !strcmp (t->display_info.tty->name, name)
          && TERMINAL_ACTIVE_P (t))
        return t;
    }
  return NULL;
}

/* Allocate basically initialized terminal.  */

static struct terminal *
allocate_terminal (void)
{
  return ALLOCATE_ZEROED_PSEUDOVECTOR
    (struct terminal, next_terminal, PVEC_TERMINAL);
}

/* Create a new terminal object of TYPE and add it to the terminal list.  RIF
   may be NULL if this terminal type doesn't support window-based redisplay.  */

struct terminal *
create_terminal (enum output_method type, struct redisplay_interface *rif)
{
  struct terminal *terminal = allocate_terminal ();
  Lisp_Object terminal_coding, keyboard_coding;

  terminal->next_terminal = terminal_list;
  terminal_list = terminal;
  terminal->type = type;
  terminal->rif = rif;
  terminal->id = next_terminal_id++;

  terminal->keyboard_coding = xmalloc (sizeof (struct coding_system));
  terminal->terminal_coding = xmalloc (sizeof (struct coding_system));

  /* If default coding systems for the terminal and the keyboard are
     already defined, use them in preference to the defaults.  This is
     needed when Emacs runs in daemon mode.  */
  keyboard_coding =
    find_symbol_value (intern ("default-keyboard-coding-system"));
  if (NILP (keyboard_coding)
      || EQ (keyboard_coding, Qunbound)
      || NILP (Fcoding_system_p (keyboard_coding)))
    keyboard_coding = Qno_conversion;
  terminal_coding =
    find_symbol_value (intern ("default-terminal-coding-system"));
  if (NILP (terminal_coding)
      || EQ (terminal_coding, Qunbound)
      || NILP (Fcoding_system_p (terminal_coding)))
    terminal_coding = Qundecided;

  setup_coding_system (keyboard_coding, terminal->keyboard_coding);
  setup_coding_system (terminal_coding, terminal->terminal_coding);

  return terminal;
}

/* Low-level function to close all frames on a terminal, remove it
   from the terminal list and free its memory.  */

void
delete_terminal (struct terminal *terminal)
{
  struct terminal **tp;
  Lisp_Object tail, frame;

  /* Protect against recursive calls.  delete_frame calls the
     delete_terminal_hook when we delete our last frame.  */
  if (!terminal->name)
    return;
  xfree (terminal->name);
  terminal->name = NULL;

  /* Check for live frames that are still on this terminal.  */
  FOR_EACH_FRAME (tail, frame)
    {
      struct frame *f = XFRAME (frame);
      if (FRAME_LIVE_P (f) && f->terminal == terminal)
        {
	  /* Pass Qnoelisp rather than Qt.  */
          delete_frame (frame, Qnoelisp);
        }
    }

  for (tp = &terminal_list; *tp != terminal; tp = &(*tp)->next_terminal)
    if (! *tp)
      emacs_abort ();
  *tp = terminal->next_terminal;

  xfree (terminal->keyboard_coding);
  terminal->keyboard_coding = NULL;
  xfree (terminal->terminal_coding);
  terminal->terminal_coding = NULL;

  if (terminal->kboard && --terminal->kboard->reference_count == 0)
    {
      delete_kboard (terminal->kboard);
      terminal->kboard = NULL;
    }
}

DEFUN ("delete-terminal", Fdelete_terminal, Sdelete_terminal, 0, 2, 0,
       doc: /* Delete TERMINAL by deleting all frames on it and closing the terminal.
TERMINAL may be a terminal object, a frame, or nil (meaning the
selected frame's terminal).

Normally, you may not delete a display if all other displays are suspended,
but if the second argument FORCE is non-nil, you may do so. */)
  (Lisp_Object terminal, Lisp_Object force)
{
  struct terminal *t = decode_terminal (terminal);

  if (!t)
    return Qnil;

  if (NILP (force))
    {
      struct terminal *p = terminal_list;
      while (p && (p == t || !TERMINAL_ACTIVE_P (p)))
	p = p->next_terminal;

      if (!p)
	error ("Attempt to delete the sole active display terminal");
    }

  if (NILP (Vrun_hooks))
    ;
  else if (EQ (force, Qnoelisp))
    pending_funcalls
      = Fcons (list3 (Qrun_hook_with_args,
		      Qdelete_terminal_functions, terminal),
	       pending_funcalls);
  else
    safe_call2 (Qrun_hook_with_args, Qdelete_terminal_functions, terminal);

  if (t->delete_terminal_hook)
    (*t->delete_terminal_hook) (t);
  else
    delete_terminal (t);

  return Qnil;
}


DEFUN ("frame-terminal", Fframe_terminal, Sframe_terminal, 0, 1, 0,
       doc: /* Return the terminal that FRAME is displayed on.
If FRAME is nil, the selected frame is used.

The terminal device is represented by its integer identifier.  */)
  (Lisp_Object frame)
{
  struct terminal *t = FRAME_TERMINAL (decode_live_frame (frame));

  if (!t)
    return Qnil;
  else
    {
      Lisp_Object terminal;
      XSETTERMINAL (terminal, t);
      return terminal;
    }
}

DEFUN ("terminal-live-p", Fterminal_live_p, Sterminal_live_p, 1, 1, 0,
       doc: /* Return non-nil if OBJECT is a terminal which has not been deleted.
Value is nil if OBJECT is not a live display terminal.
If object is a live display terminal, the return value indicates what
sort of output terminal it uses.  See the documentation of `framep' for
possible return values.  */)
  (Lisp_Object object)
{
  struct terminal *t = decode_terminal (object);

  if (!t)
    return Qnil;

  switch (t->type)
    {
    case output_initial: /* The initial frame is like a termcap frame. */
    case output_termcap:
      return Qt;
    case output_x_window:
      return Qx;
    case output_w32:
      return Qw32;
    case output_msdos_raw:
      return Qpc;
    case output_ns:
      return Qns;
    default:
      emacs_abort ();
    }
}

DEFUN ("terminal-list", Fterminal_list, Sterminal_list, 0, 0, 0,
       doc: /* Return a list of all terminal devices.  */)
  (void)
{
  Lisp_Object terminal, terminals = Qnil;
  struct terminal *t;

  for (t = terminal_list; t; t = t->next_terminal)
    {
      XSETTERMINAL (terminal, t);
      terminals = Fcons (terminal, terminals);
    }

  return terminals;
}

DEFUN ("terminal-name", Fterminal_name, Sterminal_name, 0, 1, 0,
       doc: /* Return the name of the terminal device TERMINAL.
It is not guaranteed that the returned value is unique among opened devices.

TERMINAL may be a terminal object, a frame, or nil (meaning the
selected frame's terminal). */)
  (Lisp_Object terminal)
{
  struct terminal *t = decode_live_terminal (terminal);

  return t->name ? build_string (t->name) : Qnil;
}



/* Set the value of terminal parameter PARAMETER in terminal D to VALUE.
   Return the previous value.  */

static Lisp_Object
store_terminal_param (struct terminal *t, Lisp_Object parameter, Lisp_Object value)
{
  Lisp_Object old_alist_elt = Fassq (parameter, t->param_alist);
  if (EQ (old_alist_elt, Qnil))
    {
      tset_param_alist (t, Fcons (Fcons (parameter, value), t->param_alist));
      return Qnil;
    }
  else
    {
      Lisp_Object result = Fcdr (old_alist_elt);
      Fsetcdr (old_alist_elt, value);
      return result;
    }
}


DEFUN ("terminal-parameters", Fterminal_parameters, Sterminal_parameters, 0, 1, 0,
       doc: /* Return the parameter-alist of terminal TERMINAL.
The value is a list of elements of the form (PARM . VALUE), where PARM
is a symbol.

TERMINAL can be a terminal object, a frame, or nil (meaning the
selected frame's terminal).  */)
  (Lisp_Object terminal)
{
  return Fcopy_alist (decode_live_terminal (terminal)->param_alist);
}

DEFUN ("terminal-parameter", Fterminal_parameter, Sterminal_parameter, 2, 2, 0,
       doc: /* Return TERMINAL's value for parameter PARAMETER.
TERMINAL can be a terminal object, a frame, or nil (meaning the
selected frame's terminal).  */)
  (Lisp_Object terminal, Lisp_Object parameter)
{
  CHECK_SYMBOL (parameter);
  return Fcdr (Fassq (parameter, decode_live_terminal (terminal)->param_alist));
}

DEFUN ("set-terminal-parameter", Fset_terminal_parameter,
       Sset_terminal_parameter, 3, 3, 0,
       doc: /* Set TERMINAL's value for parameter PARAMETER to VALUE.
Return the previous value of PARAMETER.

TERMINAL can be a terminal object, a frame or nil (meaning the
selected frame's terminal).  */)
  (Lisp_Object terminal, Lisp_Object parameter, Lisp_Object value)
{
  return store_terminal_param (decode_live_terminal (terminal), parameter, value);
}

#if HAVE_STRUCT_UNIPAIR_UNICODE

/* Compute the glyph code table for T.  */

static void
calculate_glyph_code_table (struct terminal *t)
{
  Lisp_Object glyphtab = Qt;
  enum { initial_unipairs = 1000 };
  int entry_ct = initial_unipairs;
  struct unipair unipair_buffer[initial_unipairs];
  struct unipair *entries = unipair_buffer;
  struct unipair *alloced = 0;

  while (true)
    {
      int fd = fileno (t->display_info.tty->output);
      struct unimapdesc unimapdesc = { entry_ct, entries };
      if (ioctl (fd, GIO_UNIMAP, &unimapdesc) == 0)
	{
	  glyphtab = Fmake_char_table (Qnil, make_number (-1));
	  for (int i = 0; i < unimapdesc.entry_ct; i++)
	    char_table_set (glyphtab, entries[i].unicode,
			    make_number (entries[i].fontpos));
	  break;
	}
      if (errno != ENOMEM)
	break;
      entry_ct = unimapdesc.entry_ct;
      entries = alloced = xrealloc (alloced, entry_ct * sizeof *alloced);
    }

  xfree (alloced);
  t->glyph_code_table = glyphtab;
}
#endif

/* Return the glyph code in T of character CH, or -1 if CH does not
   have a font position in T, or nil if T does not report glyph codes.  */

Lisp_Object
terminal_glyph_code (struct terminal *t, int ch)
{
#if HAVE_STRUCT_UNIPAIR_UNICODE
  /* Heuristically assume that a terminal supporting glyph codes is in
     UTF-8 mode if and only if its coding system is UTF-8 (Bug#26396).  */
  if (t->type == output_termcap
      && t->terminal_coding->encoder == encode_coding_utf_8)
    {
      /* As a hack, recompute the table when CH is the maximum
	 character.  */
      if (NILP (t->glyph_code_table) || ch == MAX_CHAR)
	calculate_glyph_code_table (t);

      if (! EQ (t->glyph_code_table, Qt))
	return char_table_ref (t->glyph_code_table, ch);
    }
#endif

  return Qnil;
}

/* Initial frame has no device-dependent output data, but has
   face cache which should be freed when the frame is deleted.  */

static void
initial_free_frame_resources (struct frame *f)
{
  eassert (FRAME_INITIAL_P (f));
  free_frame_faces (f);
}

/* Create the bootstrap display terminal for the initial frame.
   Returns a terminal of type output_initial.  */

struct terminal *
init_initial_terminal (void)
{
  if (initialized || terminal_list || tty_list)
    emacs_abort ();

  initial_terminal = create_terminal (output_initial, NULL);
  initial_terminal->name = xstrdup ("initial_terminal");
  initial_terminal->kboard = initial_kboard;
  initial_terminal->delete_terminal_hook = &delete_initial_terminal;
  initial_terminal->delete_frame_hook = &initial_free_frame_resources;
  /* Other hooks are NULL by default.  */

  return initial_terminal;
}

/* Deletes the bootstrap terminal device.
   Called through delete_terminal_hook. */

static void
delete_initial_terminal (struct terminal *terminal)
{
  if (terminal != initial_terminal)
    emacs_abort ();

  delete_terminal (terminal);
  initial_terminal = NULL;
}

void
syms_of_terminal (void)
{

  DEFVAR_LISP ("ring-bell-function", Vring_bell_function,
    doc: /* Non-nil means call this function to ring the bell.
The function should accept no arguments.  */);
  Vring_bell_function = Qnil;

  DEFVAR_LISP ("delete-terminal-functions", Vdelete_terminal_functions,
    doc: /* Special hook run when a terminal is deleted.
Each function is called with argument, the terminal.
This may be called just before actually deleting the terminal,
or some time later.  */);
  Vdelete_terminal_functions = Qnil;

  DEFSYM (Qterminal_live_p, "terminal-live-p");
  DEFSYM (Qdelete_terminal_functions, "delete-terminal-functions");
  DEFSYM (Qrun_hook_with_args, "run-hook-with-args");

  defsubr (&Sdelete_terminal);
  defsubr (&Sframe_terminal);
  defsubr (&Sterminal_live_p);
  defsubr (&Sterminal_list);
  defsubr (&Sterminal_name);
  defsubr (&Sterminal_parameters);
  defsubr (&Sterminal_parameter);
  defsubr (&Sset_terminal_parameter);

  Fprovide (intern_c_string ("multi-tty"), Qnil);
}
