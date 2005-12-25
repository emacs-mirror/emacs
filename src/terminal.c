/* Functions related to terminal devices.
   Copyright (C) 2005 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include <config.h>

#include "lisp.h"
#include "frame.h"
#include "termchar.h"
#include "termhooks.h"
#include "charset.h"
#include "coding.h"
#include "keyboard.h"

/* Chain of all displays currently in use. */
struct device *device_list;

/* The first unallocated display id. */
static int next_device_id;

/* The initial display device, created by initial_term_init. */
struct device *initial_device;

/* Function to use to ring the bell.  */
Lisp_Object Vring_bell_function;

void delete_initial_device P_ ((struct device *));



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
  else if (FRAME_DEVICE (f)->ring_bell_hook)
    (*FRAME_DEVICE (f)->ring_bell_hook) (f);
}

void
update_begin (struct frame *f)
{
  if (FRAME_DEVICE (f)->update_begin_hook)
    (*FRAME_DEVICE (f)->update_begin_hook) (f);
}

void
update_end (struct frame *f)
{
  if (FRAME_DEVICE (f)->update_end_hook)
    (*FRAME_DEVICE (f)->update_end_hook) (f);
}

/* Specify how many text lines, from the top of the window,
   should be affected by insert-lines and delete-lines operations.
   This, and those operations, are used only within an update
   that is bounded by calls to update_begin and update_end.  */

void
set_terminal_window (struct frame *f, int size)
{
  if (FRAME_DEVICE (f)->set_terminal_window_hook)
    (*FRAME_DEVICE (f)->set_terminal_window_hook) (f, size);
}

/* Move cursor to row/column position VPOS/HPOS.  HPOS/VPOS are
   frame-relative coordinates.  */

void
cursor_to (struct frame *f, int vpos, int hpos)
{
  if (FRAME_DEVICE (f)->cursor_to_hook)
    (*FRAME_DEVICE (f)->cursor_to_hook) (f, vpos, hpos);
}

/* Similar but don't take any account of the wasted characters.  */

void
raw_cursor_to (struct frame *f, int row, int col)
{
  if (FRAME_DEVICE (f)->raw_cursor_to_hook)
    (*FRAME_DEVICE (f)->raw_cursor_to_hook) (f, row, col);  
}

/* Erase operations */

/* Clear from cursor to end of frame. */
void
clear_to_end (struct frame *f)
{
  if (FRAME_DEVICE (f)->clear_to_end_hook)
    (*FRAME_DEVICE (f)->clear_to_end_hook) (f);
}

/* Clear entire frame */

void
clear_frame (struct frame *f)
{
  if (FRAME_DEVICE (f)->clear_frame_hook)
    (*FRAME_DEVICE (f)->clear_frame_hook) (f);
}

/* Clear from cursor to end of line.
   Assume that the line is already clear starting at column first_unused_hpos.

   Note that the cursor may be moved, on terminals lacking a `ce' string.  */

void
clear_end_of_line (struct frame *f, int first_unused_hpos)
{
  if (FRAME_DEVICE (f)->clear_end_of_line_hook)
    (*FRAME_DEVICE (f)->clear_end_of_line_hook) (f, first_unused_hpos);
}

/* Output LEN glyphs starting at STRING at the nominal cursor position.
   Advance the nominal cursor over the text.  */

void
write_glyphs (struct frame *f, struct glyph *string, int len)
{
  if (FRAME_DEVICE (f)->write_glyphs_hook)
    (*FRAME_DEVICE (f)->write_glyphs_hook) (f, string, len);
}

/* Insert LEN glyphs from START at the nominal cursor position.

   If start is zero, insert blanks instead of a string at start */

void
insert_glyphs (struct frame *f, struct glyph *start, int len)
{
  if (len <= 0)
    return;

  if (FRAME_DEVICE (f)->insert_glyphs_hook)
    (*FRAME_DEVICE (f)->insert_glyphs_hook) (f, start, len);
}

/* Delete N glyphs at the nominal cursor position. */

void
delete_glyphs (struct frame *f, int n)
{
  if (FRAME_DEVICE (f)->delete_glyphs_hook)
    (*FRAME_DEVICE (f)->delete_glyphs_hook) (f, n);
}

/* Insert N lines at vpos VPOS.  If N is negative, delete -N lines.  */

void
ins_del_lines (struct frame *f, int vpos, int n)
{
  if (FRAME_DEVICE (f)->ins_del_lines_hook)
    (*FRAME_DEVICE (f)->ins_del_lines_hook) (f, vpos, n);
}




/* Return the display object specified by DEVICE.  DEVICE may be a
   display id, a frame, or nil for the display device of the current
   frame.  If THROW is zero, return NULL for failure, otherwise throw
   an error.  */

struct device *
get_device (Lisp_Object device, int throw)
{
  struct device *result = NULL;

  if (NILP (device))
    device = selected_frame;

  if (INTEGERP (device))
    {
      struct device *d;

      for (d = device_list; d; d = d->next_device)
        {
          if (d->id == XINT (device))
            {
              result = d;
              break;
            }
        }
    }
  else if (FRAMEP (device))
    {
      result = FRAME_DEVICE (XFRAME (device));
    }

  if (result == NULL && throw)
    wrong_type_argument (Qdisplay_live_p, device);

  return result;
}



/* Create a new device object and add it to the device list. */

struct device *
create_device (void)
{
  struct device *device = (struct device *) xmalloc (sizeof (struct device));
  
  bzero (device, sizeof (struct device));
  device->next_device = device_list;
  device_list = device;

  device->id = next_device_id++;

  device->keyboard_coding =
    (struct coding_system *) xmalloc (sizeof (struct coding_system));
  device->terminal_coding =
    (struct coding_system *) xmalloc (sizeof (struct coding_system));

  setup_coding_system (Qnil, device->keyboard_coding);
  setup_coding_system (Qnil, device->terminal_coding);

  device->param_alist = Qnil;
  return device;
}

/* Mark the Lisp pointers in the terminal objects.
   Called by the Fgarbage_collector.  */

void
mark_devices (void)
{
  struct device *d;
  for (d = device_list; d; d = d->next_device)
    {
      mark_object (d->param_alist);
    }
}


/* Remove a device from the device list and free its memory. */

void
delete_device (struct device *device)
{
  struct device **dp;
  Lisp_Object tail, frame;
  
  /* Check for and close live frames that are still on this
     device. */
  FOR_EACH_FRAME (tail, frame)
    {
      struct frame *f = XFRAME (frame);
      if (FRAME_LIVE_P (f) && f->device == device)
        {
          Fdelete_frame (frame, Qt);
        }
    }

  for (dp = &device_list; *dp != device; dp = &(*dp)->next_device)
    if (! *dp)
      abort ();
  *dp = device->next_device;

  if (device->keyboard_coding)
    xfree (device->keyboard_coding);
  if (device->terminal_coding)
    xfree (device->terminal_coding);
  if (device->name)
    xfree (device->name);
  
#ifdef MULTI_KBOARD
  if (device->kboard && --device->kboard->reference_count == 0)
    delete_kboard (device->kboard);
#endif
  
  bzero (device, sizeof (struct device));
  xfree (device);
}

DEFUN ("delete-display", Fdelete_display, Sdelete_display, 0, 2, 0,
       doc: /* Delete DEVICE by deleting all frames on it and closing the device.
DEVICE may be a display device id, a frame, or nil (meaning the
selected frame's display device).

Normally, you may not delete a display if all other displays are suspended,
but if the second argument FORCE is non-nil, you may do so. */)
  (device, force)
     Lisp_Object device, force;
{
  struct device *d, *p;

  d = get_device (device, 0);

  if (!d)
    return Qnil;

  p = device_list;
  while (p && (p == d || !DEVICE_ACTIVE_P (p)))
    p = p->next_device;
  
  if (NILP (force) && !p)
    error ("Attempt to delete the sole active display device");

  if (d->delete_device_hook)
    (*d->delete_device_hook) (d);
  else
    delete_device (d);

  return Qnil;
}

DEFUN ("display-live-p", Fdisplay_live_p, Sdisplay_live_p, 1, 1, 0,
       doc: /* Return non-nil if OBJECT is a device which has not been deleted.
Value is nil if OBJECT is not a live display device.
If object is a live display device, the return value indicates what
sort of output device it uses.  See the documentation of `framep' for
possible return values.

Display devices are represented by their integer identifiers. */)
     (object)
     Lisp_Object object;
{
  struct device *d;
  
  if (!INTEGERP (object))
    return Qnil;

  d = get_device (object, 0);

  if (!d)
    return Qnil;

  switch (d->type)
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
    case output_mac:
      return Qmac;
    default:
      abort ();
    }
}

DEFUN ("display-list", Fdisplay_list, Sdisplay_list, 0, 0, 0,
       doc: /* Return a list of all display devices.
Display devices are represented by their integer identifiers. */)
  ()
{
  Lisp_Object devices = Qnil;
  struct device *d;

  for (d = device_list; d; d = d->next_device)
    devices = Fcons (make_number (d->id), devices);

  return devices;
}

DEFUN ("display-name", Fdisplay_name, Sdisplay_name, 0, 1, 0,
       doc: /* Return the name of the display device DEVICE.
It is not guaranteed that the returned value is unique among opened devices.

DEVICE may be a display device id, a frame, or nil (meaning the
selected frame's display device). */)
  (device)
     Lisp_Object device;
{
  struct device *d = get_device (device, 1);

  if (d->name)
    return build_string (d->name);
  else
    return Qnil;
}



/* Return the value of terminal parameter PARAM in device D.  */
Lisp_Object
get_terminal_param (d, param)
     struct device *d;
     Lisp_Object param;
{
  Lisp_Object tem = Fassq (param, d->param_alist);
  if (EQ (tem, Qnil))
    return tem;
  return Fcdr (tem);
}

/* Set the value of terminal parameter PARAMETER in device D to VALUE.
   Return the previous value.  */

Lisp_Object
store_terminal_param (d, parameter, value)
     struct device *d;
     Lisp_Object parameter;
     Lisp_Object value;
{
  Lisp_Object old_alist_elt = Fassq (parameter, d->param_alist);
  if (EQ (old_alist_elt, Qnil))
    {
      d->param_alist = Fcons (Fcons (parameter, value), d->param_alist);
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

TERMINAL can be a terminal if, a frame or nil (meaning the selected
frame's terminal).  */)
     (terminal)
     Lisp_Object terminal;
{
  struct device *d = get_device (terminal, 1);
  return Fcopy_alist (d->param_alist);
}

DEFUN ("terminal-parameter", Fterminal_parameter, Sterminal_parameter, 2, 2, 0,
       doc: /* Return TERMINAL's value for parameter PARAMETER.
TERMINAL can be a terminal if, a frame or nil (meaning the selected
frame's terminal).  */)
     (terminal, parameter)
     Lisp_Object terminal;
     Lisp_Object parameter;
{
  Lisp_Object value;
  struct device *d = get_device (terminal, 1);
  CHECK_SYMBOL (parameter);
  value = Fcdr (Fassq (parameter, d->param_alist));
  return value;
}

DEFUN ("modify-terminal-parameters", Fmodify_terminal_parameters,
       Smodify_terminal_parameters, 2, 2, 0,
       doc: /* Modify the parameters of terminal TERMINAL according to ALIST.
ALIST is an alist of parameters to change and their new values.
Each element of ALIST has the form (PARM . VALUE), where PARM is a symbol.

TERMINAL can be a terminal if, a frame or nil (meaning the selected
frame's terminal).  */)
     (terminal, alist)
     Lisp_Object terminal;
     Lisp_Object alist;
{
  Lisp_Object tail, prop, val;
  struct device *d = get_device (terminal, 1);
  int length = XINT (Fsafe_length (alist));
  int i;
  Lisp_Object *parms = (Lisp_Object *) alloca (length * sizeof (Lisp_Object));
  Lisp_Object *values = (Lisp_Object *) alloca (length * sizeof (Lisp_Object));

  /* Extract parm names and values into those vectors.  */
  
  i = 0;
  for (tail = alist; CONSP (tail); tail = Fcdr (tail))
    {
      Lisp_Object elt;
      
      elt = Fcar (tail);
      parms[i] = Fcar (elt);
      values[i] = Fcdr (elt);
      i++;
    }
  
  /* Now process them in reverse of specified order.  */
  for (i--; i >= 0; i--)
    {
      prop = parms[i];
      val = values[i];
      store_terminal_param (d, prop, val);
    }
  return Qnil;
}

DEFUN ("set-terminal-parameter", Fset_terminal_parameter,
       Sset_terminal_parameter, 3, 3, 0,
       doc: /* Set TERMINAL's value for parameter PARAMETER to VALUE.
Return the previous value of PARAMETER.

TERMINAL can be a terminal if, a frame or nil (meaning the selected
frame's terminal).  */)
     (terminal, parameter, value)
     Lisp_Object terminal;
     Lisp_Object parameter;
     Lisp_Object value;
{
  struct device *d = get_device (terminal, 1);
  return store_terminal_param (d, parameter, value);
}



/* Create the bootstrap display device for the initial frame.
   Returns a device of type output_initial.  */

struct device *
init_initial_device (void)
{
  if (initialized || device_list || tty_list)
    abort ();

  initial_device = create_device ();
  initial_device->type = output_initial;
  initial_device->name = xstrdup ("initial_device");
  initial_device->kboard = initial_kboard;

  initial_device->delete_device_hook = &delete_initial_device;
  /* All other hooks are NULL. */

  return initial_device;
}

/* Deletes the bootstrap display device.
   Called through delete_device_hook. */

void
delete_initial_device (struct device *device)
{
  if (device != initial_device)
    abort ();

  delete_device (device);
  initial_device = NULL;
}

void
syms_of_terminal ()
{

  DEFVAR_LISP ("ring-bell-function", &Vring_bell_function,
    doc: /* Non-nil means call this function to ring the bell.
The function should accept no arguments.  */);
  Vring_bell_function = Qnil;

  defsubr (&Sdelete_display);
  defsubr (&Sdisplay_live_p);
  defsubr (&Sdisplay_list);
  defsubr (&Sdisplay_name);
  defsubr (&Sterminal_parameters);
  defsubr (&Sterminal_parameter);
  defsubr (&Smodify_terminal_parameters);
  defsubr (&Sset_terminal_parameter);

  Fprovide (intern ("multi-tty"), Qnil);
}

/* arch-tag: e9af6f27-b483-47dc-bb1a-730c1c5cab03
   (do not change this comment) */
