/* X Selection processing for Emacs.
   Copyright (C) 1993, 1994, 1995 Free Software Foundation.

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


/* Rewritten by jwz */

#include <config.h>
#include "lisp.h"
#include "xterm.h"	/* for all of the X includes */
#include "dispextern.h"	/* frame.h seems to want this */
#include "frame.h"	/* Need this to get the X window of selected_frame */
#include "blockinput.h"

#define xfree free

#define CUT_BUFFER_SUPPORT

Lisp_Object QPRIMARY, QSECONDARY, QSTRING, QINTEGER, QCLIPBOARD, QTIMESTAMP,
  QTEXT, QDELETE, QMULTIPLE, QINCR, QEMACS_TMP, QTARGETS, QATOM, QNULL,
  QATOM_PAIR;

#ifdef CUT_BUFFER_SUPPORT
Lisp_Object QCUT_BUFFER0, QCUT_BUFFER1, QCUT_BUFFER2, QCUT_BUFFER3,
  QCUT_BUFFER4, QCUT_BUFFER5, QCUT_BUFFER6, QCUT_BUFFER7;
#endif

static Lisp_Object Vx_lost_selection_hooks;
static Lisp_Object Vx_sent_selection_hooks;

/* If this is a smaller number than the max-request-size of the display,
   emacs will use INCR selection transfer when the selection is larger
   than this.  The max-request-size is usually around 64k, so if you want
   emacs to use incremental selection transfers when the selection is 
   smaller than that, set this.  I added this mostly for debugging the
   incremental transfer stuff, but it might improve server performance.  */
#define MAX_SELECTION_QUANTUM 0xFFFFFF

#ifdef HAVE_X11R4
#define SELECTION_QUANTUM(dpy) ((XMaxRequestSize(dpy) << 2) - 100)
#else
#define SELECTION_QUANTUM(dpy) (((dpy)->max_request_size << 2) - 100)
#endif

/* The timestamp of the last input event Emacs received from the X server.  */
unsigned long last_event_timestamp;

/* This is an association list whose elements are of the form
     ( SELECTION-NAME SELECTION-VALUE SELECTION-TIMESTAMP FRAME)
   SELECTION-NAME is a lisp symbol, whose name is the name of an X Atom.
   SELECTION-VALUE is the value that emacs owns for that selection.
     It may be any kind of Lisp object.
   SELECTION-TIMESTAMP is the time at which emacs began owning this selection,
     as a cons of two 16-bit numbers (making a 32 bit time.)
   FRAME is the frame for which we made the selection.
   If there is an entry in this alist, then it can be assumed that Emacs owns
    that selection.
   The only (eq) parts of this list that are visible from Lisp are the
    selection-values.  */
static Lisp_Object Vselection_alist;

/* This is an alist whose CARs are selection-types (whose names are the same
   as the names of X Atoms) and whose CDRs are the names of Lisp functions to
   call to convert the given Emacs selection value to a string representing 
   the given selection type.  This is for Lisp-level extension of the emacs
   selection handling.  */
static Lisp_Object Vselection_converter_alist;

/* If the selection owner takes too long to reply to a selection request,
   we give up on it.  This is in milliseconds (0 = no timeout.)  */
static int x_selection_timeout;

/* Utility functions */

static void lisp_data_to_selection_data ();
static Lisp_Object selection_data_to_lisp_data ();
static Lisp_Object x_get_window_property_as_lisp_data ();

/* This converts a Lisp symbol to a server Atom, avoiding a server 
   roundtrip whenever possible.  */

static Atom
symbol_to_x_atom (dpyinfo, display, sym)
     struct x_display_info *dpyinfo;
     Display *display;
     Lisp_Object sym;
{
  Atom val;
  if (NILP (sym))	    return 0;
  if (EQ (sym, QPRIMARY))   return XA_PRIMARY;
  if (EQ (sym, QSECONDARY)) return XA_SECONDARY;
  if (EQ (sym, QSTRING))    return XA_STRING;
  if (EQ (sym, QINTEGER))   return XA_INTEGER;
  if (EQ (sym, QATOM))	    return XA_ATOM;
  if (EQ (sym, QCLIPBOARD)) return dpyinfo->Xatom_CLIPBOARD;
  if (EQ (sym, QTIMESTAMP)) return dpyinfo->Xatom_TIMESTAMP;
  if (EQ (sym, QTEXT))	    return dpyinfo->Xatom_TEXT;
  if (EQ (sym, QDELETE))    return dpyinfo->Xatom_DELETE;
  if (EQ (sym, QMULTIPLE))  return dpyinfo->Xatom_MULTIPLE;
  if (EQ (sym, QINCR))	    return dpyinfo->Xatom_INCR;
  if (EQ (sym, QEMACS_TMP)) return dpyinfo->Xatom_EMACS_TMP;
  if (EQ (sym, QTARGETS))   return dpyinfo->Xatom_TARGETS;
  if (EQ (sym, QNULL))	    return dpyinfo->Xatom_NULL;
#ifdef CUT_BUFFER_SUPPORT
  if (EQ (sym, QCUT_BUFFER0)) return XA_CUT_BUFFER0;
  if (EQ (sym, QCUT_BUFFER1)) return XA_CUT_BUFFER1;
  if (EQ (sym, QCUT_BUFFER2)) return XA_CUT_BUFFER2;
  if (EQ (sym, QCUT_BUFFER3)) return XA_CUT_BUFFER3;
  if (EQ (sym, QCUT_BUFFER4)) return XA_CUT_BUFFER4;
  if (EQ (sym, QCUT_BUFFER5)) return XA_CUT_BUFFER5;
  if (EQ (sym, QCUT_BUFFER6)) return XA_CUT_BUFFER6;
  if (EQ (sym, QCUT_BUFFER7)) return XA_CUT_BUFFER7;
#endif
  if (!SYMBOLP (sym)) abort ();

#if 0
  fprintf (stderr, " XInternAtom %s\n", (char *) XSYMBOL (sym)->name->data);
#endif
  BLOCK_INPUT;
  val = XInternAtom (display, (char *) XSYMBOL (sym)->name->data, False);
  UNBLOCK_INPUT;
  return val;
}


/* This converts a server Atom to a Lisp symbol, avoiding server roundtrips
   and calls to intern whenever possible.  */

static Lisp_Object
x_atom_to_symbol (dpyinfo, display, atom)
     struct x_display_info *dpyinfo;
     Display *display;
     Atom atom;
{
  char *str;
  Lisp_Object val;
  if (! atom) return Qnil;
  switch (atom)
    {
    case XA_PRIMARY:
      return QPRIMARY;
    case XA_SECONDARY:
      return QSECONDARY;
    case XA_STRING:
      return QSTRING;
    case XA_INTEGER:
      return QINTEGER;
    case XA_ATOM:
      return QATOM;
#ifdef CUT_BUFFER_SUPPORT
    case XA_CUT_BUFFER0:
      return QCUT_BUFFER0;
    case XA_CUT_BUFFER1:
      return QCUT_BUFFER1;
    case XA_CUT_BUFFER2:
      return QCUT_BUFFER2;
    case XA_CUT_BUFFER3:
      return QCUT_BUFFER3;
    case XA_CUT_BUFFER4:
      return QCUT_BUFFER4;
    case XA_CUT_BUFFER5:
      return QCUT_BUFFER5;
    case XA_CUT_BUFFER6:
      return QCUT_BUFFER6;
    case XA_CUT_BUFFER7:
      return QCUT_BUFFER7;
#endif
    }

  if (atom == dpyinfo->Xatom_CLIPBOARD)
    return QCLIPBOARD;
  if (atom == dpyinfo->Xatom_TIMESTAMP)
    return QTIMESTAMP;
  if (atom == dpyinfo->Xatom_TEXT)
    return QTEXT;
  if (atom == dpyinfo->Xatom_DELETE)
    return QDELETE;
  if (atom == dpyinfo->Xatom_MULTIPLE)
    return QMULTIPLE;
  if (atom == dpyinfo->Xatom_INCR)
    return QINCR;
  if (atom == dpyinfo->Xatom_EMACS_TMP)
    return QEMACS_TMP;
  if (atom == dpyinfo->Xatom_TARGETS)
    return QTARGETS;
  if (atom == dpyinfo->Xatom_NULL)
    return QNULL;

  BLOCK_INPUT;
  str = XGetAtomName (display, atom);
  UNBLOCK_INPUT;
#if 0
  fprintf (stderr, " XGetAtomName --> %s\n", str);
#endif
  if (! str) return Qnil;
  val = intern (str);
  BLOCK_INPUT;
  XFree (str);
  UNBLOCK_INPUT;
  return val;
}

/* Do protocol to assert ourself as a selection owner.
   Update the Vselection_alist so that we can reply to later requests for 
   our selection.  */

static void
x_own_selection (selection_name, selection_value)
     Lisp_Object selection_name, selection_value;
{
  Window selecting_window = FRAME_X_WINDOW (selected_frame);
  Display *display = FRAME_X_DISPLAY (selected_frame);
  Time time = last_event_timestamp;
  Atom selection_atom;
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (selected_frame);

  CHECK_SYMBOL (selection_name, 0);
  selection_atom = symbol_to_x_atom (dpyinfo, display, selection_name);

  BLOCK_INPUT;
  x_catch_errors (display);
  XSetSelectionOwner (display, selection_atom, selecting_window, time);
  x_check_errors (display, "Can't set selection: %s");
  x_uncatch_errors (display);
  UNBLOCK_INPUT;

  /* Now update the local cache */
  {
    Lisp_Object selection_time;
    Lisp_Object selection_data;
    Lisp_Object prev_value;

    selection_time = long_to_cons ((unsigned long) time);
    selection_data = Fcons (selection_name,
			    Fcons (selection_value,
				   Fcons (selection_time,
					  Fcons (Fselected_frame (), Qnil))));
    prev_value = assq_no_quit (selection_name, Vselection_alist);

    Vselection_alist = Fcons (selection_data, Vselection_alist);

    /* If we already owned the selection, remove the old selection data.
       Perhaps we should destructively modify it instead.
       Don't use Fdelq as that may QUIT.  */
    if (!NILP (prev_value))
      {
	Lisp_Object rest;	/* we know it's not the CAR, so it's easy.  */
	for (rest = Vselection_alist; !NILP (rest); rest = Fcdr (rest))
	  if (EQ (prev_value, Fcar (XCONS (rest)->cdr)))
	    {
	      XCONS (rest)->cdr = Fcdr (XCONS (rest)->cdr);
	      break;
	    }
      }
  }
}

/* Given a selection-name and desired type, look up our local copy of
   the selection value and convert it to the type.
   The value is nil or a string.
   This function is used both for remote requests
   and for local x-get-selection-internal.

   This calls random Lisp code, and may signal or gc.  */

static Lisp_Object
x_get_local_selection (selection_symbol, target_type)
     Lisp_Object selection_symbol, target_type;
{
  Lisp_Object local_value;
  Lisp_Object handler_fn, value, type, check;
  int count;

  local_value = assq_no_quit (selection_symbol, Vselection_alist);

  if (NILP (local_value)) return Qnil;

  /* TIMESTAMP and MULTIPLE are special cases 'cause that's easiest.  */
  if (EQ (target_type, QTIMESTAMP))
    {
      handler_fn = Qnil;
      value = XCONS (XCONS (XCONS (local_value)->cdr)->cdr)->car;
    }
#if 0
  else if (EQ (target_type, QDELETE))
    {
      handler_fn = Qnil;
      Fx_disown_selection_internal
	(selection_symbol,
	 XCONS (XCONS (XCONS (local_value)->cdr)->cdr)->car);
      value = QNULL;
    }
#endif

#if 0 /* #### MULTIPLE doesn't work yet */
  else if (CONSP (target_type)
	   && XCONS (target_type)->car == QMULTIPLE)
    {
      Lisp_Object pairs;
      int size;
      int i;
      pairs = XCONS (target_type)->cdr;
      size = XVECTOR (pairs)->size;
      /* If the target is MULTIPLE, then target_type looks like
	  (MULTIPLE . [[SELECTION1 TARGET1] [SELECTION2 TARGET2] ... ])
	 We modify the second element of each pair in the vector and
	 return it as [[SELECTION1 <value1>] [SELECTION2 <value2>] ... ]
       */
      for (i = 0; i < size; i++)
	{
	  Lisp_Object pair;
	  pair = XVECTOR (pairs)->contents [i];
	  XVECTOR (pair)->contents [1]
	    = x_get_local_selection (XVECTOR (pair)->contents [0],
				     XVECTOR (pair)->contents [1]);
	}
      return pairs;
    }
#endif
  else
    {
      /* Don't allow a quit within the converter.
	 When the user types C-g, he would be surprised
	 if by luck it came during a converter.  */
      count = specpdl_ptr - specpdl;
      specbind (Qinhibit_quit, Qt);

      CHECK_SYMBOL (target_type, 0);
      handler_fn = Fcdr (Fassq (target_type, Vselection_converter_alist));
      if (!NILP (handler_fn))
	value = call3 (handler_fn,
		       selection_symbol, target_type,
		       XCONS (XCONS (local_value)->cdr)->car);
      else
	value = Qnil;
      unbind_to (count, Qnil);
    }

  /* Make sure this value is of a type that we could transmit
     to another X client.  */

  check = value;
  if (CONSP (value)
      && SYMBOLP (XCONS (value)->car))
    type = XCONS (value)->car,
    check = XCONS (value)->cdr;
  
  if (STRINGP (check)
      || VECTORP (check)
      || SYMBOLP (check)
      || INTEGERP (check)
      || NILP (value))
    return value;
  /* Check for a value that cons_to_long could handle.  */
  else if (CONSP (check)
	   && INTEGERP (XCONS (check)->car)
	   && (INTEGERP (XCONS (check)->cdr)
	       ||
	       (CONSP (XCONS (check)->cdr)
		&& INTEGERP (XCONS (XCONS (check)->cdr)->car)
		&& NILP (XCONS (XCONS (check)->cdr)->cdr))))
    return value;
  else
    return
      Fsignal (Qerror,
	       Fcons (build_string ("invalid data returned by selection-conversion function"),
		      Fcons (handler_fn, Fcons (value, Qnil))));
}

/* Subroutines of x_reply_selection_request.  */

/* Send a SelectionNotify event to the requestor with property=None,
   meaning we were unable to do what they wanted.  */

static void
x_decline_selection_request (event)
     struct input_event *event;
{
  XSelectionEvent reply;
  reply.type = SelectionNotify;
  reply.display = SELECTION_EVENT_DISPLAY (event);
  reply.requestor = SELECTION_EVENT_REQUESTOR (event);
  reply.selection = SELECTION_EVENT_SELECTION (event);
  reply.time = SELECTION_EVENT_TIME (event);
  reply.target = SELECTION_EVENT_TARGET (event);
  reply.property = None;

  BLOCK_INPUT;
  XSendEvent (reply.display, reply.requestor, False, 0L,
	      (XEvent *) &reply);
  XFlush (reply.display);
  UNBLOCK_INPUT;
}

/* This is the selection request currently being processed.
   It is set to zero when the request is fully processed.  */
static struct input_event *x_selection_current_request;

/* Used as an unwind-protect clause so that, if a selection-converter signals
   an error, we tell the requestor that we were unable to do what they wanted
   before we throw to top-level or go into the debugger or whatever.  */

static Lisp_Object
x_selection_request_lisp_error (ignore)
     Lisp_Object ignore;
{
  if (x_selection_current_request != 0)
    x_decline_selection_request (x_selection_current_request);
  return Qnil;
}


/* This stuff is so that INCR selections are reentrant (that is, so we can
   be servicing multiple INCR selection requests simultaneously.)  I haven't
   actually tested that yet.  */

/* Keep a list of the property changes that are awaited.  */

struct prop_location
{
  int identifier;
  Display *display;
  Window window;
  Atom property;
  int desired_state;
  int arrived;
  struct prop_location *next;
};

static struct prop_location *expect_property_change ();
static void wait_for_property_change ();
static void unexpect_property_change ();
static int waiting_for_other_props_on_window ();

static int prop_location_identifier;

static Lisp_Object property_change_reply;

static struct prop_location *property_change_reply_object;

static struct prop_location *property_change_wait_list;

static Lisp_Object
queue_selection_requests_unwind (frame)
     Lisp_Object frame;
{
  FRAME_PTR f = XFRAME (frame);

  if (! NILP (frame))
    x_stop_queuing_selection_requests (FRAME_X_DISPLAY (f));
}

/* Return some frame whose display info is DPYINFO.
   Return nil if there is none.  */

static Lisp_Object
some_frame_on_display (dpyinfo)
     struct x_display_info *dpyinfo;
{
  Lisp_Object list, frame;

  FOR_EACH_FRAME (list, frame)
    {
      if (FRAME_X_DISPLAY_INFO (XFRAME (frame)) == dpyinfo)
	return frame;
    }

  return Qnil;
}

/* Send the reply to a selection request event EVENT.
   TYPE is the type of selection data requested.
   DATA and SIZE describe the data to send, already converted.
   FORMAT is the unit-size (in bits) of the data to be transmitted.  */

static void
x_reply_selection_request (event, format, data, size, type)
     struct input_event *event;
     int format, size;
     unsigned char *data;
     Atom type;
{
  XSelectionEvent reply;
  Display *display = SELECTION_EVENT_DISPLAY (event);
  Window window = SELECTION_EVENT_REQUESTOR (event);
  int bytes_remaining;
  int format_bytes = format/8;
  int max_bytes = SELECTION_QUANTUM (display);
  struct x_display_info *dpyinfo = x_display_info_for_display (display);

  if (max_bytes > MAX_SELECTION_QUANTUM)
    max_bytes = MAX_SELECTION_QUANTUM;

  reply.type = SelectionNotify;
  reply.display = display;
  reply.requestor = window;
  reply.selection = SELECTION_EVENT_SELECTION (event);
  reply.time = SELECTION_EVENT_TIME (event);
  reply.target = SELECTION_EVENT_TARGET (event);
  reply.property = SELECTION_EVENT_PROPERTY (event);
  if (reply.property == None)
    reply.property = reply.target;

  /* #### XChangeProperty can generate BadAlloc, and we must handle it! */
  BLOCK_INPUT;
  x_catch_errors (display);

  /* Store the data on the requested property.
     If the selection is large, only store the first N bytes of it.
   */
  bytes_remaining = size * format_bytes;
  if (bytes_remaining <= max_bytes)
    {
      /* Send all the data at once, with minimal handshaking.  */
#if 0
      fprintf (stderr,"\nStoring all %d\n", bytes_remaining);
#endif
      XChangeProperty (display, window, reply.property, type, format,
		       PropModeReplace, data, size);
      /* At this point, the selection was successfully stored; ack it.  */
      XSendEvent (display, window, False, 0L, (XEvent *) &reply);
    }
  else
    {
      /* Send an INCR selection.  */
      struct prop_location *wait_object;
      int had_errors;
      int count = specpdl_ptr - specpdl;
      Lisp_Object frame;

      frame = some_frame_on_display (dpyinfo);

      /* If the display no longer has frames, we can't expect
	 to get many more selection requests from it, so don't
	 bother trying to queue them.  */
      if (!NILP (frame))
	{
	  x_start_queuing_selection_requests (display);

	  record_unwind_protect (queue_selection_requests_unwind,
				 frame);
	}

      if (x_window_to_frame (dpyinfo, window)) /* #### debug */
	error ("attempt to transfer an INCR to ourself!");
#if 0
      fprintf (stderr, "\nINCR %d\n", bytes_remaining);
#endif
      wait_object = expect_property_change (display, window, reply.property,
					    PropertyDelete);

      XChangeProperty (display, window, reply.property, dpyinfo->Xatom_INCR,
		       32, PropModeReplace,
		       (unsigned char *) &bytes_remaining, 1);
      XSelectInput (display, window, PropertyChangeMask);
      /* Tell 'em the INCR data is there...  */
      XSendEvent (display, window, False, 0L, (XEvent *) &reply);
      XFlush (display);

      had_errors = x_had_errors_p (display);
      UNBLOCK_INPUT;

      /* First, wait for the requestor to ack by deleting the property.
	 This can run random lisp code (process handlers) or signal.  */
      if (! had_errors)
	wait_for_property_change (wait_object);

      while (bytes_remaining)
	{
	  int i = ((bytes_remaining < max_bytes)
		   ? bytes_remaining
		   : max_bytes);

	  BLOCK_INPUT;

	  wait_object
	    = expect_property_change (display, window, reply.property,
				      PropertyDelete);
#if 0
	  fprintf (stderr,"  INCR adding %d\n", i);
#endif
	  /* Append the next chunk of data to the property.  */
	  XChangeProperty (display, window, reply.property, type, format,
			   PropModeAppend, data, i / format_bytes);
	  bytes_remaining -= i;
	  data += i;
	  XFlush (display);
	  had_errors = x_had_errors_p (display);
	  UNBLOCK_INPUT;

	  if (had_errors)
	    break;

	  /* Now wait for the requestor to ack this chunk by deleting the
	     property.	 This can run random lisp code or signal.
	   */
	  wait_for_property_change (wait_object);
	}
      /* Now write a zero-length chunk to the property to tell the requestor
	 that we're done.  */
#if 0
      fprintf (stderr,"  INCR done\n");
#endif
      BLOCK_INPUT;
      if (! waiting_for_other_props_on_window (display, window))
	XSelectInput (display, window, 0L);

      XChangeProperty (display, window, reply.property, type, format,
		       PropModeReplace, data, 0);

      unbind_to (count, Qnil);
    }

  XFlush (display);
  x_uncatch_errors (display);
  UNBLOCK_INPUT;
}

/* Handle a SelectionRequest event EVENT.
   This is called from keyboard.c when such an event is found in the queue.  */

void
x_handle_selection_request (event)
     struct input_event *event;
{
  struct gcpro gcpro1, gcpro2, gcpro3;
  Lisp_Object local_selection_data;
  Lisp_Object selection_symbol;
  Lisp_Object target_symbol;
  Lisp_Object converted_selection;
  Time local_selection_time;
  Lisp_Object successful_p;
  int count;
  struct x_display_info *dpyinfo
    = x_display_info_for_display (SELECTION_EVENT_DISPLAY (event));

  local_selection_data = Qnil;
  target_symbol = Qnil;
  converted_selection = Qnil;
  successful_p = Qnil;

  GCPRO3 (local_selection_data, converted_selection, target_symbol);

  selection_symbol = x_atom_to_symbol (dpyinfo,
				       SELECTION_EVENT_DISPLAY (event),
				       SELECTION_EVENT_SELECTION (event));

  local_selection_data = assq_no_quit (selection_symbol, Vselection_alist);

  if (NILP (local_selection_data))
    {
      /* Someone asked for the selection, but we don't have it any more.
       */
      x_decline_selection_request (event);
      goto DONE;
    }

  local_selection_time = (Time)
    cons_to_long (XCONS (XCONS (XCONS (local_selection_data)->cdr)->cdr)->car);

  if (SELECTION_EVENT_TIME (event) != CurrentTime
      && local_selection_time > SELECTION_EVENT_TIME (event))
    {
      /* Someone asked for the selection, and we have one, but not the one
	 they're looking for.
       */
      x_decline_selection_request (event);
      goto DONE;
    }

  count = specpdl_ptr - specpdl;
  x_selection_current_request = event;
  record_unwind_protect (x_selection_request_lisp_error, Qnil);

  target_symbol = x_atom_to_symbol (dpyinfo, SELECTION_EVENT_DISPLAY (event),
				    SELECTION_EVENT_TARGET (event));

#if 0 /* #### MULTIPLE doesn't work yet */
  if (EQ (target_symbol, QMULTIPLE))
    target_symbol = fetch_multiple_target (event);
#endif
  
  /* Convert lisp objects back into binary data */
  
  converted_selection
    = x_get_local_selection (selection_symbol, target_symbol);
  
  if (! NILP (converted_selection))
    {
      unsigned char *data;
      unsigned int size;
      int format;
      Atom type;
      int nofree;

      lisp_data_to_selection_data (SELECTION_EVENT_DISPLAY (event),
				   converted_selection,
				   &data, &type, &size, &format, &nofree);
      
      x_reply_selection_request (event, format, data, size, type);
      successful_p = Qt;

      /* Indicate we have successfully processed this event.  */
      x_selection_current_request = 0;

      if (!nofree)
	xfree (data);
    }
  unbind_to (count, Qnil);

 DONE:

  UNGCPRO;

  /* Let random lisp code notice that the selection has been asked for.  */
  {
    Lisp_Object rest;
    rest = Vx_sent_selection_hooks;
    if (!EQ (rest, Qunbound))
      for (; CONSP (rest); rest = Fcdr (rest))
	call3 (Fcar (rest), selection_symbol, target_symbol, successful_p);
  }
}

/* Handle a SelectionClear event EVENT, which indicates that some other
   client cleared out our previously asserted selection.
   This is called from keyboard.c when such an event is found in the queue.  */

void
x_handle_selection_clear (event)
     struct input_event *event;
{
  Display *display = SELECTION_EVENT_DISPLAY (event);
  Atom selection = SELECTION_EVENT_SELECTION (event);
  Time changed_owner_time = SELECTION_EVENT_TIME (event);
  
  Lisp_Object selection_symbol, local_selection_data;
  Time local_selection_time;
  struct x_display_info *dpyinfo = x_display_info_for_display (display);

  selection_symbol = x_atom_to_symbol (dpyinfo, display, selection);

  local_selection_data = assq_no_quit (selection_symbol, Vselection_alist);

  /* Well, we already believe that we don't own it, so that's just fine.  */
  if (NILP (local_selection_data)) return;

  local_selection_time = (Time)
    cons_to_long (XCONS (XCONS (XCONS (local_selection_data)->cdr)->cdr)->car);

  /* This SelectionClear is for a selection that we no longer own, so we can
     disregard it.  (That is, we have reasserted the selection since this
     request was generated.)  */

  if (changed_owner_time != CurrentTime
      && local_selection_time > changed_owner_time)
    return;

  /* Otherwise, we're really honest and truly being told to drop it.
     Don't use Fdelq as that may QUIT;.  */

  if (EQ (local_selection_data, Fcar (Vselection_alist)))
    Vselection_alist = Fcdr (Vselection_alist);
  else
    {
      Lisp_Object rest;
      for (rest = Vselection_alist; !NILP (rest); rest = Fcdr (rest))
	if (EQ (local_selection_data, Fcar (XCONS (rest)->cdr)))
	  {
	    XCONS (rest)->cdr = Fcdr (XCONS (rest)->cdr);
	    break;
	  }
    }

  /* Let random lisp code notice that the selection has been stolen.  */

  {
    Lisp_Object rest;
    rest = Vx_lost_selection_hooks;
    if (!EQ (rest, Qunbound))
      {
	for (; CONSP (rest); rest = Fcdr (rest))
	  call1 (Fcar (rest), selection_symbol);
	prepare_menu_bars ();
	redisplay_preserve_echo_area ();
      }
  }
}

/* Clear all selections that were made from frame F.
   We do this when about to delete a frame.  */

void
x_clear_frame_selections (f)
     FRAME_PTR f;
{
  Lisp_Object frame;
  Lisp_Object rest;

  XSETFRAME (frame, f);

  /* Otherwise, we're really honest and truly being told to drop it.
     Don't use Fdelq as that may QUIT;.  */

  while (!NILP (Vselection_alist)
	 && EQ (frame, Fcar (Fcdr (Fcdr (Fcdr (Fcar (Vselection_alist)))))))
    {
      /* Let random Lisp code notice that the selection has been stolen.  */
      Lisp_Object hooks, selection_symbol;

      hooks = Vx_lost_selection_hooks;
      selection_symbol = Fcar (Vselection_alist);

      if (!EQ (hooks, Qunbound))
	{
	  for (; CONSP (hooks); hooks = Fcdr (hooks))
	    call1 (Fcar (hooks), selection_symbol);
	  redisplay_preserve_echo_area ();
	}

      Vselection_alist = Fcdr (Vselection_alist);
    }

  for (rest = Vselection_alist; !NILP (rest); rest = Fcdr (rest))
    if (EQ (frame, Fcar (Fcdr (Fcdr (Fcdr (Fcar (XCONS (rest)->cdr)))))))
      {
	/* Let random Lisp code notice that the selection has been stolen.  */
	Lisp_Object hooks, selection_symbol;

	hooks = Vx_lost_selection_hooks;
	selection_symbol = Fcar (XCONS (rest)->cdr);

	if (!EQ (hooks, Qunbound))
	  {
	    for (; CONSP (hooks); hooks = Fcdr (hooks))
	      call1 (Fcar (hooks), selection_symbol);
	    redisplay_preserve_echo_area ();
	  }
	XCONS (rest)->cdr = Fcdr (XCONS (rest)->cdr);
	break;
      }
}

/* Nonzero if any properties for DISPLAY and WINDOW
   are on the list of what we are waiting for.  */

static int
waiting_for_other_props_on_window (display, window)
     Display *display;
     Window window;
{
  struct prop_location *rest = property_change_wait_list;
  while (rest)
    if (rest->display == display && rest->window == window)
      return 1;
    else
      rest = rest->next;
  return 0;
}

/* Add an entry to the list of property changes we are waiting for.
   DISPLAY, WINDOW, PROPERTY, STATE describe what we will wait for.
   The return value is a number that uniquely identifies
   this awaited property change.  */

static struct prop_location *
expect_property_change (display, window, property, state)
     Display *display;
     Window window;
     Lisp_Object property;
     int state;
{
  struct prop_location *pl
    = (struct prop_location *) xmalloc (sizeof (struct prop_location));
  pl->identifier = ++prop_location_identifier;
  pl->display = display;
  pl->window = window;
  pl->property = property;
  pl->desired_state = state;
  pl->next = property_change_wait_list;
  pl->arrived = 0;
  property_change_wait_list = pl;
  return pl;
}

/* Delete an entry from the list of property changes we are waiting for.
   IDENTIFIER is the number that uniquely identifies the entry.  */

static void
unexpect_property_change (location)
     struct prop_location *location;
{
  struct prop_location *prev = 0, *rest = property_change_wait_list;
  while (rest)
    {
      if (rest == location)
	{
	  if (prev)
	    prev->next = rest->next;
	  else
	    property_change_wait_list = rest->next;
	  xfree (rest);
	  return;
	}
      prev = rest;
      rest = rest->next;
    }
}

/* Remove the property change expectation element for IDENTIFIER.  */

static Lisp_Object
wait_for_property_change_unwind (identifierval)
     Lisp_Object identifierval;
{
  unexpect_property_change ((struct prop_location *)
			    (XFASTINT (XCONS (identifierval)->car) << 16
			     | XFASTINT (XCONS (identifierval)->cdr)));
}

/* Actually wait for a property change.
   IDENTIFIER should be the value that expect_property_change returned.  */

static void
wait_for_property_change (location)
     struct prop_location *location;
{
  int secs, usecs;
  int count = specpdl_ptr - specpdl;
  Lisp_Object tem;

  tem = Fcons (Qnil, Qnil);
  XSETFASTINT (XCONS (tem)->car, (EMACS_UINT)location >> 16);
  XSETFASTINT (XCONS (tem)->cdr, (EMACS_UINT)location & 0xffff);

  /* Make sure to do unexpect_property_change if we quit or err.  */
  record_unwind_protect (wait_for_property_change_unwind, tem);

  XCONS (property_change_reply)->car = Qnil;

  property_change_reply_object = location;
  /* If the event we are waiting for arrives beyond here, it will set
     property_change_reply, because property_change_reply_object says so.  */
  if (! location->arrived)
    {
      secs = x_selection_timeout / 1000;
      usecs = (x_selection_timeout % 1000) * 1000;
      wait_reading_process_input (secs, usecs, property_change_reply, 0);

      if (NILP (XCONS (property_change_reply)->car))
	error ("timed out waiting for property-notify event");
    }

  unbind_to (count, Qnil);
}

/* Called from XTread_socket in response to a PropertyNotify event.  */

void
x_handle_property_notify (event)
     XPropertyEvent *event;
{
  struct prop_location *prev = 0, *rest = property_change_wait_list;
  while (rest)
    {
      if (rest->property == event->atom
	  && rest->window == event->window
	  && rest->display == event->display
	  && rest->desired_state == event->state)
	{
#if 0
	  fprintf (stderr, "Saw expected prop-%s on %s\n",
		   (event->state == PropertyDelete ? "delete" : "change"),
		   (char *) XSYMBOL (x_atom_to_symbol (dpyinfo, event->display,
						       event->atom))
		   ->name->data);
#endif

	  rest->arrived = 1;

	  /* If this is the one wait_for_property_change is waiting for,
	     tell it to wake up.  */
	  if (rest == property_change_reply_object)
	    XCONS (property_change_reply)->car = Qt;

	  if (prev)
	    prev->next = rest->next;
	  else
	    property_change_wait_list = rest->next;
	  xfree (rest);
	  return;
	}
      prev = rest;
      rest = rest->next;
    }
#if 0
  fprintf (stderr, "Saw UNexpected prop-%s on %s\n",
	   (event->state == PropertyDelete ? "delete" : "change"),
	   (char *) XSYMBOL (x_atom_to_symbol (dpyinfo,
					       event->display, event->atom))
	   ->name->data);
#endif
}



#if 0 /* #### MULTIPLE doesn't work yet */

static Lisp_Object
fetch_multiple_target (event)
     XSelectionRequestEvent *event;
{
  Display *display = event->display;
  Window window = event->requestor;
  Atom target = event->target;
  Atom selection_atom = event->selection;
  int result;

  return
    Fcons (QMULTIPLE,
	   x_get_window_property_as_lisp_data (display, window, target,
					       QMULTIPLE, selection_atom));
}

static Lisp_Object
copy_multiple_data (obj)
     Lisp_Object obj;
{
  Lisp_Object vec;
  int i;
  int size;
  if (CONSP (obj))
    return Fcons (XCONS (obj)->car, copy_multiple_data (XCONS (obj)->cdr));
    
  CHECK_VECTOR (obj, 0);
  vec = Fmake_vector (size = XVECTOR (obj)->size, Qnil);
  for (i = 0; i < size; i++)
    {
      Lisp_Object vec2 = XVECTOR (obj)->contents [i];
      CHECK_VECTOR (vec2, 0);
      if (XVECTOR (vec2)->size != 2)
	/* ??? Confusing error message */
	Fsignal (Qerror, Fcons (build_string ("vectors must be of length 2"),
				Fcons (vec2, Qnil)));
      XVECTOR (vec)->contents [i] = Fmake_vector (2, Qnil);
      XVECTOR (XVECTOR (vec)->contents [i])->contents [0]
	= XVECTOR (vec2)->contents [0];
      XVECTOR (XVECTOR (vec)->contents [i])->contents [1]
	= XVECTOR (vec2)->contents [1];
    }
  return vec;
}

#endif


/* Variables for communication with x_handle_selection_notify.  */
static Atom reading_which_selection;
static Lisp_Object reading_selection_reply;
static Window reading_selection_window;

/* Do protocol to read selection-data from the server.
   Converts this to Lisp data and returns it.  */

static Lisp_Object
x_get_foreign_selection (selection_symbol, target_type)
     Lisp_Object selection_symbol, target_type;
{
  Window requestor_window = FRAME_X_WINDOW (selected_frame);
  Display *display = FRAME_X_DISPLAY (selected_frame);
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (selected_frame);
  Time requestor_time = last_event_timestamp;
  Atom target_property = dpyinfo->Xatom_EMACS_TMP;
  Atom selection_atom = symbol_to_x_atom (dpyinfo, display, selection_symbol);
  Atom type_atom;
  int secs, usecs;
  int count = specpdl_ptr - specpdl;
  Lisp_Object frame;

  if (CONSP (target_type))
    type_atom = symbol_to_x_atom (dpyinfo, display, XCONS (target_type)->car);
  else
    type_atom = symbol_to_x_atom (dpyinfo, display, target_type);

  BLOCK_INPUT;
  x_catch_errors (display);
  XConvertSelection (display, selection_atom, type_atom, target_property,
		     requestor_window, requestor_time);
  XFlush (display);

  /* Prepare to block until the reply has been read.  */
  reading_selection_window = requestor_window;
  reading_which_selection = selection_atom;
  XCONS (reading_selection_reply)->car = Qnil;

  frame = some_frame_on_display (dpyinfo);

  /* If the display no longer has frames, we can't expect
     to get many more selection requests from it, so don't
     bother trying to queue them.  */
  if (!NILP (frame))
    {
      x_start_queuing_selection_requests (display);

      record_unwind_protect (queue_selection_requests_unwind,
			     frame);
    }
  UNBLOCK_INPUT;

  /* This allows quits.  Also, don't wait forever.  */
  secs = x_selection_timeout / 1000;
  usecs = (x_selection_timeout % 1000) * 1000;
  wait_reading_process_input (secs, usecs, reading_selection_reply, 0);

  BLOCK_INPUT;
  x_check_errors (display, "Cannot get selection: %s");
  x_uncatch_errors (display);
  unbind_to (count, Qnil);
  UNBLOCK_INPUT;

  if (NILP (XCONS (reading_selection_reply)->car))
    error ("timed out waiting for reply from selection owner");

  /* Otherwise, the selection is waiting for us on the requested property.  */
  return
    x_get_window_property_as_lisp_data (display, requestor_window,
					target_property, target_type,
					selection_atom);
}

/* Subroutines of x_get_window_property_as_lisp_data */

static void
x_get_window_property (display, window, property, data_ret, bytes_ret,
		       actual_type_ret, actual_format_ret, actual_size_ret,
		       delete_p)
     Display *display;
     Window window;
     Atom property;
     unsigned char **data_ret;
     int *bytes_ret;
     Atom *actual_type_ret;
     int *actual_format_ret;
     unsigned long *actual_size_ret;
     int delete_p;
{
  int total_size;
  unsigned long bytes_remaining;
  int offset = 0;
  unsigned char *tmp_data = 0;
  int result;
  int buffer_size = SELECTION_QUANTUM (display);
  if (buffer_size > MAX_SELECTION_QUANTUM) buffer_size = MAX_SELECTION_QUANTUM;
  
  BLOCK_INPUT;
  /* First probe the thing to find out how big it is.  */
  result = XGetWindowProperty (display, window, property,
			       0L, 0L, False, AnyPropertyType,
			       actual_type_ret, actual_format_ret,
			       actual_size_ret,
			       &bytes_remaining, &tmp_data);
  if (result != Success)
    {
      UNBLOCK_INPUT;
      *data_ret = 0;
      *bytes_ret = 0;
      return;
    }
  xfree ((char *) tmp_data);
  
  if (*actual_type_ret == None || *actual_format_ret == 0)
    {
      UNBLOCK_INPUT;
      return;
    }

  total_size = bytes_remaining + 1;
  *data_ret = (unsigned char *) xmalloc (total_size);
  
  /* Now read, until weve gotten it all.  */
  while (bytes_remaining)
    {
#if 0
      int last = bytes_remaining;
#endif
      result
	= XGetWindowProperty (display, window, property,
			      (long)offset/4, (long)buffer_size/4,
			      False,
			      AnyPropertyType,
			      actual_type_ret, actual_format_ret,
			      actual_size_ret, &bytes_remaining, &tmp_data);
#if 0
      fprintf (stderr, "<< read %d\n", last-bytes_remaining);
#endif
      /* If this doesn't return Success at this point, it means that
	 some clod deleted the selection while we were in the midst of
	 reading it.  Deal with that, I guess....
       */
      if (result != Success) break;
      *actual_size_ret *= *actual_format_ret / 8;
      bcopy (tmp_data, (*data_ret) + offset, *actual_size_ret);
      offset += *actual_size_ret;
      xfree ((char *) tmp_data);
    }

  XFlush (display);
  UNBLOCK_INPUT;
  *bytes_ret = offset;
}

static void
receive_incremental_selection (display, window, property, target_type,
			       min_size_bytes, data_ret, size_bytes_ret,
			       type_ret, format_ret, size_ret)
     Display *display;
     Window window;
     Atom property;
     Lisp_Object target_type; /* for error messages only */
     unsigned int min_size_bytes;
     unsigned char **data_ret;
     int *size_bytes_ret;
     Atom *type_ret;
     unsigned long *size_ret;
     int *format_ret;
{
  int offset = 0;
  struct prop_location *wait_object;
  *size_bytes_ret = min_size_bytes;
  *data_ret = (unsigned char *) xmalloc (*size_bytes_ret);
#if 0
  fprintf (stderr, "\nread INCR %d\n", min_size_bytes);
#endif

  /* At this point, we have read an INCR property.
     Delete the property to ack it.
     (But first, prepare to receive the next event in this handshake.)

     Now, we must loop, waiting for the sending window to put a value on
     that property, then reading the property, then deleting it to ack.
     We are done when the sender places a property of length 0.
   */
  BLOCK_INPUT;
  XSelectInput (display, window, STANDARD_EVENT_SET | PropertyChangeMask);
  XDeleteProperty (display, window, property);
  wait_object = expect_property_change (display, window, property,
					PropertyNewValue);
  XFlush (display);
  UNBLOCK_INPUT;

  while (1)
    {
      unsigned char *tmp_data;
      int tmp_size_bytes;
      wait_for_property_change (wait_object);
      /* expect it again immediately, because x_get_window_property may
	 .. no it wont, I dont get it.
	 .. Ok, I get it now, the Xt code that implements INCR is broken.
       */
      x_get_window_property (display, window, property,
			     &tmp_data, &tmp_size_bytes,
			     type_ret, format_ret, size_ret, 1);

      if (tmp_size_bytes == 0) /* we're done */
	{
#if 0
	  fprintf (stderr, "  read INCR done\n");
#endif
	  if (! waiting_for_other_props_on_window (display, window))
	    XSelectInput (display, window, STANDARD_EVENT_SET);
	  unexpect_property_change (wait_object);
	  if (tmp_data) xfree (tmp_data);
	  break;
	}

      BLOCK_INPUT;
      XDeleteProperty (display, window, property);
      wait_object = expect_property_change (display, window, property,
					    PropertyNewValue);
      XFlush (display);
      UNBLOCK_INPUT;

#if 0
      fprintf (stderr, "  read INCR %d\n", tmp_size_bytes);
#endif
      if (*size_bytes_ret < offset + tmp_size_bytes)
	{
#if 0
	  fprintf (stderr, "  read INCR realloc %d -> %d\n",
		   *size_bytes_ret, offset + tmp_size_bytes);
#endif
	  *size_bytes_ret = offset + tmp_size_bytes;
	  *data_ret = (unsigned char *) xrealloc (*data_ret, *size_bytes_ret);
	}
      bcopy (tmp_data, (*data_ret) + offset, tmp_size_bytes);
      offset += tmp_size_bytes;
      xfree (tmp_data);
    }
}

/* Once a requested selection is "ready" (we got a SelectionNotify event),
   fetch value from property PROPERTY of X window WINDOW on display DISPLAY.
   TARGET_TYPE and SELECTION_ATOM are used in error message if this fails.  */

static Lisp_Object
x_get_window_property_as_lisp_data (display, window, property, target_type,
				    selection_atom)
     Display *display;
     Window window;
     Atom property;
     Lisp_Object target_type;	/* for error messages only */
     Atom selection_atom;	/* for error messages only */
{
  Atom actual_type;
  int actual_format;
  unsigned long actual_size;
  unsigned char *data = 0;
  int bytes = 0;
  Lisp_Object val;
  struct x_display_info *dpyinfo = x_display_info_for_display (display);

  x_get_window_property (display, window, property, &data, &bytes,
			 &actual_type, &actual_format, &actual_size, 1);
  if (! data)
    {
      int there_is_a_selection_owner;
      BLOCK_INPUT;
      there_is_a_selection_owner
	= XGetSelectionOwner (display, selection_atom);
      UNBLOCK_INPUT;
      while (1) /* Note debugger can no longer return, so this is obsolete */
	Fsignal (Qerror,
		 there_is_a_selection_owner ?
		 Fcons (build_string ("selection owner couldn't convert"),
			actual_type
			? Fcons (target_type,
				 Fcons (x_atom_to_symbol (dpyinfo, display,
							  actual_type),
					Qnil))
			: Fcons (target_type, Qnil))
		 : Fcons (build_string ("no selection"),
			  Fcons (x_atom_to_symbol (dpyinfo, display,
						   selection_atom),
				 Qnil)));
    }
  
  if (actual_type == dpyinfo->Xatom_INCR)
    {
      /* That wasn't really the data, just the beginning.  */

      unsigned int min_size_bytes = * ((unsigned int *) data);
      BLOCK_INPUT;
      XFree ((char *) data);
      UNBLOCK_INPUT;
      receive_incremental_selection (display, window, property, target_type,
				     min_size_bytes, &data, &bytes,
				     &actual_type, &actual_format,
				     &actual_size);
    }

  BLOCK_INPUT;
  XDeleteProperty (display, window, property);
  XFlush (display);
  UNBLOCK_INPUT;

  /* It's been read.  Now convert it to a lisp object in some semi-rational
     manner.  */
  val = selection_data_to_lisp_data (display, data, bytes,
				     actual_type, actual_format);
  
  xfree ((char *) data);
  return val;
}

/* These functions convert from the selection data read from the server into
   something that we can use from Lisp, and vice versa.

	Type:	Format:	Size:		Lisp Type:
	-----	-------	-----		-----------
	*	8	*		String
	ATOM	32	1		Symbol
	ATOM	32	> 1		Vector of Symbols
	*	16	1		Integer
	*	16	> 1		Vector of Integers
	*	32	1		if <=16 bits: Integer
					if > 16 bits: Cons of top16, bot16
	*	32	> 1		Vector of the above

   When converting a Lisp number to C, it is assumed to be of format 16 if
   it is an integer, and of format 32 if it is a cons of two integers.

   When converting a vector of numbers from Lisp to C, it is assumed to be
   of format 16 if every element in the vector is an integer, and is assumed
   to be of format 32 if any element is a cons of two integers.

   When converting an object to C, it may be of the form (SYMBOL . <data>)
   where SYMBOL is what we should claim that the type is.  Format and
   representation are as above.  */



static Lisp_Object
selection_data_to_lisp_data (display, data, size, type, format)
     Display *display;
     unsigned char *data;
     Atom type;
     int size, format;
{
  struct x_display_info *dpyinfo = x_display_info_for_display (display);

  if (type == dpyinfo->Xatom_NULL)
    return QNULL;

  /* Convert any 8-bit data to a string, for compactness.  */
  else if (format == 8)
    return make_string ((char *) data, size);

  /* Convert a single atom to a Lisp_Symbol.  Convert a set of atoms to
     a vector of symbols.
   */
  else if (type == XA_ATOM)
    {
      int i;
      if (size == sizeof (Atom))
	return x_atom_to_symbol (dpyinfo, display, *((Atom *) data));
      else
	{
	  Lisp_Object v = Fmake_vector (size / sizeof (Atom), 0);
	  for (i = 0; i < size / sizeof (Atom); i++)
	    Faset (v, i, x_atom_to_symbol (dpyinfo, display,
					   ((Atom *) data) [i]));
	  return v;
	}
    }

  /* Convert a single 16 or small 32 bit number to a Lisp_Int.
     If the number is > 16 bits, convert it to a cons of integers,
     16 bits in each half.
   */
  else if (format == 32 && size == sizeof (long))
    return long_to_cons (((unsigned long *) data) [0]);
  else if (format == 16 && size == sizeof (short))
    return make_number ((int) (((unsigned short *) data) [0]));

  /* Convert any other kind of data to a vector of numbers, represented
     as above (as an integer, or a cons of two 16 bit integers.)
   */
  else if (format == 16)
    {
      int i;
      Lisp_Object v = Fmake_vector (size / 4, 0);
      for (i = 0; i < size / 4; i++)
	{
	  int j = (int) ((unsigned short *) data) [i];
	  Faset (v, i, make_number (j));
	}
      return v;
    }
  else
    {
      int i;
      Lisp_Object v = Fmake_vector (size / 4, 0);
      for (i = 0; i < size / 4; i++)
	{
	  unsigned long j = ((unsigned long *) data) [i];
	  Faset (v, i, long_to_cons (j));
	}
      return v;
    }
}


static void
lisp_data_to_selection_data (display, obj,
			     data_ret, type_ret, size_ret,
			     format_ret, nofree_ret)
     Display *display;
     Lisp_Object obj;
     unsigned char **data_ret;
     Atom *type_ret;
     unsigned int *size_ret;
     int *format_ret;
     int *nofree_ret;
{
  Lisp_Object type = Qnil;
  struct x_display_info *dpyinfo = x_display_info_for_display (display);

  *nofree_ret = 0;

  if (CONSP (obj) && SYMBOLP (XCONS (obj)->car))
    {
      type = XCONS (obj)->car;
      obj = XCONS (obj)->cdr;
      if (CONSP (obj) && NILP (XCONS (obj)->cdr))
	obj = XCONS (obj)->car;
    }

  if (EQ (obj, QNULL) || (EQ (type, QNULL)))
    {				/* This is not the same as declining */
      *format_ret = 32;
      *size_ret = 0;
      *data_ret = 0;
      type = QNULL;
    }
  else if (STRINGP (obj))
    {
      *format_ret = 8;
      *size_ret = XSTRING (obj)->size;
      *data_ret = XSTRING (obj)->data;
      *nofree_ret = 1;
      if (NILP (type)) type = QSTRING;
    }
  else if (SYMBOLP (obj))
    {
      *format_ret = 32;
      *size_ret = 1;
      *data_ret = (unsigned char *) xmalloc (sizeof (Atom) + 1);
      (*data_ret) [sizeof (Atom)] = 0;
      (*(Atom **) data_ret) [0] = symbol_to_x_atom (dpyinfo, display, obj);
      if (NILP (type)) type = QATOM;
    }
  else if (INTEGERP (obj)
	   && XINT (obj) < 0xFFFF
	   && XINT (obj) > -0xFFFF)
    {
      *format_ret = 16;
      *size_ret = 1;
      *data_ret = (unsigned char *) xmalloc (sizeof (short) + 1);
      (*data_ret) [sizeof (short)] = 0;
      (*(short **) data_ret) [0] = (short) XINT (obj);
      if (NILP (type)) type = QINTEGER;
    }
  else if (INTEGERP (obj)
	   || (CONSP (obj) && INTEGERP (XCONS (obj)->car)
	       && (INTEGERP (XCONS (obj)->cdr)
		   || (CONSP (XCONS (obj)->cdr)
		       && INTEGERP (XCONS (XCONS (obj)->cdr)->car)))))
    {
      *format_ret = 32;
      *size_ret = 1;
      *data_ret = (unsigned char *) xmalloc (sizeof (long) + 1);
      (*data_ret) [sizeof (long)] = 0;
      (*(unsigned long **) data_ret) [0] = cons_to_long (obj);
      if (NILP (type)) type = QINTEGER;
    }
  else if (VECTORP (obj))
    {
      /* Lisp_Vectors may represent a set of ATOMs;
	 a set of 16 or 32 bit INTEGERs;
	 or a set of ATOM_PAIRs (represented as [[A1 A2] [A3 A4] ...]
       */
      int i;

      if (SYMBOLP (XVECTOR (obj)->contents [0]))
	/* This vector is an ATOM set */
	{
	  if (NILP (type)) type = QATOM;
	  *size_ret = XVECTOR (obj)->size;
	  *format_ret = 32;
	  *data_ret = (unsigned char *) xmalloc ((*size_ret) * sizeof (Atom));
	  for (i = 0; i < *size_ret; i++)
	    if (SYMBOLP (XVECTOR (obj)->contents [i]))
	      (*(Atom **) data_ret) [i]
		= symbol_to_x_atom (dpyinfo, display, XVECTOR (obj)->contents [i]);
	    else
	      Fsignal (Qerror, /* Qselection_error */
		       Fcons (build_string
		   ("all elements of selection vector must have same type"),
			      Fcons (obj, Qnil)));
	}
#if 0 /* #### MULTIPLE doesn't work yet */
      else if (VECTORP (XVECTOR (obj)->contents [0]))
	/* This vector is an ATOM_PAIR set */
	{
	  if (NILP (type)) type = QATOM_PAIR;
	  *size_ret = XVECTOR (obj)->size;
	  *format_ret = 32;
	  *data_ret = (unsigned char *)
	    xmalloc ((*size_ret) * sizeof (Atom) * 2);
	  for (i = 0; i < *size_ret; i++)
	    if (VECTORP (XVECTOR (obj)->contents [i]))
	      {
		Lisp_Object pair = XVECTOR (obj)->contents [i];
		if (XVECTOR (pair)->size != 2)
		  Fsignal (Qerror,
			   Fcons (build_string 
       ("elements of the vector must be vectors of exactly two elements"),
				  Fcons (pair, Qnil)));
		
		(*(Atom **) data_ret) [i * 2]
		  = symbol_to_x_atom (dpyinfo, display,
				      XVECTOR (pair)->contents [0]);
		(*(Atom **) data_ret) [(i * 2) + 1]
		  = symbol_to_x_atom (dpyinfo, display,
				      XVECTOR (pair)->contents [1]);
	      }
	    else
	      Fsignal (Qerror,
		       Fcons (build_string
		   ("all elements of the vector must be of the same type"),
			      Fcons (obj, Qnil)));
	  
	}
#endif
      else
	/* This vector is an INTEGER set, or something like it */
	{
	  *size_ret = XVECTOR (obj)->size;
	  if (NILP (type)) type = QINTEGER;
	  *format_ret = 16;
	  for (i = 0; i < *size_ret; i++)
	    if (CONSP (XVECTOR (obj)->contents [i]))
	      *format_ret = 32;
	    else if (!INTEGERP (XVECTOR (obj)->contents [i]))
	      Fsignal (Qerror, /* Qselection_error */
		       Fcons (build_string
	("elements of selection vector must be integers or conses of integers"),
			      Fcons (obj, Qnil)));

	  *data_ret = (unsigned char *) xmalloc (*size_ret * (*format_ret/8));
	  for (i = 0; i < *size_ret; i++)
	    if (*format_ret == 32)
	      (*((unsigned long **) data_ret)) [i]
		= cons_to_long (XVECTOR (obj)->contents [i]);
	    else
	      (*((unsigned short **) data_ret)) [i]
		= (unsigned short) cons_to_long (XVECTOR (obj)->contents [i]);
	}
    }
  else
    Fsignal (Qerror, /* Qselection_error */
	     Fcons (build_string ("unrecognised selection data"),
		    Fcons (obj, Qnil)));

  *type_ret = symbol_to_x_atom (dpyinfo, display, type);
}

static Lisp_Object
clean_local_selection_data (obj)
     Lisp_Object obj;
{
  if (CONSP (obj)
      && INTEGERP (XCONS (obj)->car)
      && CONSP (XCONS (obj)->cdr)
      && INTEGERP (XCONS (XCONS (obj)->cdr)->car)
      && NILP (XCONS (XCONS (obj)->cdr)->cdr))
    obj = Fcons (XCONS (obj)->car, XCONS (obj)->cdr);

  if (CONSP (obj)
      && INTEGERP (XCONS (obj)->car)
      && INTEGERP (XCONS (obj)->cdr))
    {
      if (XINT (XCONS (obj)->car) == 0)
	return XCONS (obj)->cdr;
      if (XINT (XCONS (obj)->car) == -1)
	return make_number (- XINT (XCONS (obj)->cdr));
    }
  if (VECTORP (obj))
    {
      int i;
      int size = XVECTOR (obj)->size;
      Lisp_Object copy;
      if (size == 1)
	return clean_local_selection_data (XVECTOR (obj)->contents [0]);
      copy = Fmake_vector (size, Qnil);
      for (i = 0; i < size; i++)
	XVECTOR (copy)->contents [i]
	  = clean_local_selection_data (XVECTOR (obj)->contents [i]);
      return copy;
    }
  return obj;
}

/* Called from XTread_socket to handle SelectionNotify events.
   If it's the selection we are waiting for, stop waiting.  */

void
x_handle_selection_notify (event)
     XSelectionEvent *event;
{
  if (event->requestor != reading_selection_window)
    return;
  if (event->selection != reading_which_selection)
    return;

  XCONS (reading_selection_reply)->car = Qt;
}


DEFUN ("x-own-selection-internal",
       Fx_own_selection_internal, Sx_own_selection_internal,
  2, 2, 0,
  "Assert an X selection of the given TYPE with the given VALUE.\n\
TYPE is a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.\n\
\(Those are literal upper-case symbol names, since that's what X expects.)\n\
VALUE is typically a string, or a cons of two markers, but may be\n\
anything that the functions on `selection-converter-alist' know about.")
  (selection_name, selection_value)
     Lisp_Object selection_name, selection_value;
{
  check_x ();
  CHECK_SYMBOL (selection_name, 0);
  if (NILP (selection_value)) error ("selection-value may not be nil.");
  x_own_selection (selection_name, selection_value);
  return selection_value;
}


/* Request the selection value from the owner.  If we are the owner,
   simply return our selection value.  If we are not the owner, this
   will block until all of the data has arrived.  */

DEFUN ("x-get-selection-internal",
  Fx_get_selection_internal, Sx_get_selection_internal, 2, 2, 0,
  "Return text selected from some X window.\n\
SELECTION is a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.\n\
\(Those are literal upper-case symbol names, since that's what X expects.)\n\
TYPE is the type of data desired, typically `STRING'.")
  (selection_symbol, target_type)
     Lisp_Object selection_symbol, target_type;
{
  Lisp_Object val = Qnil;
  struct gcpro gcpro1, gcpro2;
  GCPRO2 (target_type, val); /* we store newly consed data into these */
  check_x ();
  CHECK_SYMBOL (selection_symbol, 0);

#if 0 /* #### MULTIPLE doesn't work yet */
  if (CONSP (target_type)
      && XCONS (target_type)->car == QMULTIPLE)
    {
      CHECK_VECTOR (XCONS (target_type)->cdr, 0);
      /* So we don't destructively modify this...  */
      target_type = copy_multiple_data (target_type);
    }
  else
#endif
    CHECK_SYMBOL (target_type, 0);

  val = x_get_local_selection (selection_symbol, target_type);

  if (NILP (val))
    {
      val = x_get_foreign_selection (selection_symbol, target_type);
      goto DONE;
    }

  if (CONSP (val)
      && SYMBOLP (XCONS (val)->car))
    {
      val = XCONS (val)->cdr;
      if (CONSP (val) && NILP (XCONS (val)->cdr))
	val = XCONS (val)->car;
    }
  val = clean_local_selection_data (val);
 DONE:
  UNGCPRO;
  return val;
}

DEFUN ("x-disown-selection-internal",
  Fx_disown_selection_internal, Sx_disown_selection_internal, 1, 2, 0,
  "If we own the selection SELECTION, disown it.\n\
Disowning it means there is no such selection.")
  (selection, time)
     Lisp_Object selection;
     Lisp_Object time;
{
  Time timestamp;
  Atom selection_atom;
  XSelectionClearEvent event;
  Display *display;
  struct x_display_info *dpyinfo;

  check_x ();
  display = FRAME_X_DISPLAY (selected_frame);
  dpyinfo = FRAME_X_DISPLAY_INFO (selected_frame);
  CHECK_SYMBOL (selection, 0);
  if (NILP (time))
    timestamp = last_event_timestamp;
  else
    timestamp = cons_to_long (time);

  if (NILP (assq_no_quit (selection, Vselection_alist)))
    return Qnil;  /* Don't disown the selection when we're not the owner.  */

  selection_atom = symbol_to_x_atom (dpyinfo, display, selection);

  BLOCK_INPUT;
  XSetSelectionOwner (display, selection_atom, None, timestamp);
  UNBLOCK_INPUT;

  /* It doesn't seem to be guaranteed that a SelectionClear event will be
     generated for a window which owns the selection when that window sets
     the selection owner to None.  The NCD server does, the MIT Sun4 server
     doesn't.  So we synthesize one; this means we might get two, but
     that's ok, because the second one won't have any effect.  */
  SELECTION_EVENT_DISPLAY (&event) = display;
  SELECTION_EVENT_SELECTION (&event) = selection_atom;
  SELECTION_EVENT_TIME (&event) = timestamp;
  x_handle_selection_clear (&event);

  return Qt;
}

/* Get rid of all the selections in buffer BUFFER.
   This is used when we kill a buffer.  */

void
x_disown_buffer_selections (buffer)
     Lisp_Object buffer;
{
  Lisp_Object tail;
  struct buffer *buf = XBUFFER (buffer);

  for (tail = Vselection_alist; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      Lisp_Object elt, value;
      elt = XCONS (tail)->car;
      value = XCONS (elt)->cdr;
      if (CONSP (value) && MARKERP (XCONS (value)->car)
	  && XMARKER (XCONS (value)->car)->buffer == buf)
	Fx_disown_selection_internal (XCONS (elt)->car, Qnil);
    }
}

DEFUN ("x-selection-owner-p", Fx_selection_owner_p, Sx_selection_owner_p,
  0, 1, 0,
  "Whether the current Emacs process owns the given X Selection.\n\
The arg should be the name of the selection in question, typically one of\n\
the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.\n\
\(Those are literal upper-case symbol names, since that's what X expects.)\n\
For convenience, the symbol nil is the same as `PRIMARY',\n\
and t is the same as `SECONDARY'.)")
  (selection)
     Lisp_Object selection;
{
  check_x ();
  CHECK_SYMBOL (selection, 0);
  if (EQ (selection, Qnil)) selection = QPRIMARY;
  if (EQ (selection, Qt)) selection = QSECONDARY;
  
  if (NILP (Fassq (selection, Vselection_alist)))
    return Qnil;
  return Qt;
}

DEFUN ("x-selection-exists-p", Fx_selection_exists_p, Sx_selection_exists_p,
  0, 1, 0,
  "Whether there is an owner for the given X Selection.\n\
The arg should be the name of the selection in question, typically one of\n\
the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.\n\
\(Those are literal upper-case symbol names, since that's what X expects.)\n\
For convenience, the symbol nil is the same as `PRIMARY',\n\
and t is the same as `SECONDARY'.)")
  (selection)
     Lisp_Object selection;
{
  Window owner;
  Atom atom;
  Display *dpy;

  /* It should be safe to call this before we have an X frame.  */
  if (! FRAME_X_P (selected_frame))
    return Qnil;

  dpy = FRAME_X_DISPLAY (selected_frame);
  CHECK_SYMBOL (selection, 0);
  if (!NILP (Fx_selection_owner_p (selection)))
    return Qt;
  if (EQ (selection, Qnil)) selection = QPRIMARY;
  if (EQ (selection, Qt)) selection = QSECONDARY;
  atom = symbol_to_x_atom (FRAME_X_DISPLAY_INFO (selected_frame),
			   dpy, selection);
  if (atom == 0)
    return Qnil;
  BLOCK_INPUT;
  owner = XGetSelectionOwner (dpy, atom);
  UNBLOCK_INPUT;
  return (owner ? Qt : Qnil);
}


#ifdef CUT_BUFFER_SUPPORT

/* Ensure that all 8 cut buffers exist.  ICCCM says we gotta...  */
static void
initialize_cut_buffers (display, window)
     Display *display;
     Window window;
{
  unsigned char *data = (unsigned char *) "";
  BLOCK_INPUT;
#define FROB(atom) XChangeProperty (display, window, atom, XA_STRING, 8, \
				    PropModeAppend, data, 0)
  FROB (XA_CUT_BUFFER0);
  FROB (XA_CUT_BUFFER1);
  FROB (XA_CUT_BUFFER2);
  FROB (XA_CUT_BUFFER3);
  FROB (XA_CUT_BUFFER4);
  FROB (XA_CUT_BUFFER5);
  FROB (XA_CUT_BUFFER6);
  FROB (XA_CUT_BUFFER7);
#undef FROB
  UNBLOCK_INPUT;
}


#define CHECK_CUT_BUFFER(symbol,n)					\
  { CHECK_SYMBOL ((symbol), (n));					\
    if (!EQ((symbol), QCUT_BUFFER0) && !EQ((symbol), QCUT_BUFFER1)	\
	&& !EQ((symbol), QCUT_BUFFER2) && !EQ((symbol), QCUT_BUFFER3)	\
	&& !EQ((symbol), QCUT_BUFFER4) && !EQ((symbol), QCUT_BUFFER5)	\
	&& !EQ((symbol), QCUT_BUFFER6) && !EQ((symbol), QCUT_BUFFER7))	\
      Fsignal (Qerror,							\
	       Fcons (build_string ("doesn't name a cut buffer"),	\
			     Fcons ((symbol), Qnil)));			\
  }

DEFUN ("x-get-cut-buffer-internal", Fx_get_cut_buffer_internal,
  Sx_get_cut_buffer_internal, 1, 1, 0,
  "Returns the value of the named cut buffer (typically CUT_BUFFER0).")
  (buffer)
     Lisp_Object buffer;
{
  Window window;
  Atom buffer_atom;
  unsigned char *data;
  int bytes;
  Atom type;
  int format;
  unsigned long size;
  Lisp_Object ret;
  Display *display;
  struct x_display_info *dpyinfo;

  check_x ();
  display = FRAME_X_DISPLAY (selected_frame);
  dpyinfo = FRAME_X_DISPLAY_INFO (selected_frame);
  window = RootWindow (display, 0); /* Cut buffers are on screen 0 */
  CHECK_CUT_BUFFER (buffer, 0);
  buffer_atom = symbol_to_x_atom (dpyinfo, display, buffer);

  x_get_window_property (display, window, buffer_atom, &data, &bytes,
			 &type, &format, &size, 0);
  if (!data) return Qnil;
  
  if (format != 8 || type != XA_STRING)
    Fsignal (Qerror,
	     Fcons (build_string ("cut buffer doesn't contain 8-bit data"),
		    Fcons (x_atom_to_symbol (dpyinfo, display, type),
			   Fcons (make_number (format), Qnil))));

  ret = (bytes ? make_string ((char *) data, bytes) : Qnil);
  xfree (data);
  return ret;
}


DEFUN ("x-store-cut-buffer-internal", Fx_store_cut_buffer_internal,
  Sx_store_cut_buffer_internal, 2, 2, 0,
  "Sets the value of the named cut buffer (typically CUT_BUFFER0).")
  (buffer, string)
     Lisp_Object buffer, string;
{
  Window window;
  Atom buffer_atom;
  unsigned char *data;
  int bytes;
  int bytes_remaining;
  int max_bytes;
  Display *display;

  check_x ();
  display = FRAME_X_DISPLAY (selected_frame);
  window = RootWindow (display, 0); /* Cut buffers are on screen 0 */

  max_bytes = SELECTION_QUANTUM (display);
  if (max_bytes > MAX_SELECTION_QUANTUM)
    max_bytes = MAX_SELECTION_QUANTUM;

  CHECK_CUT_BUFFER (buffer, 0);
  CHECK_STRING (string, 0);
  buffer_atom = symbol_to_x_atom (FRAME_X_DISPLAY_INFO (selected_frame),
				  display, buffer);
  data = (unsigned char *) XSTRING (string)->data;
  bytes = XSTRING (string)->size;
  bytes_remaining = bytes;

  if (! FRAME_X_DISPLAY_INFO (selected_frame)->cut_buffers_initialized)
    {
      initialize_cut_buffers (display, window);
      FRAME_X_DISPLAY_INFO (selected_frame)->cut_buffers_initialized = 1;
    }

  BLOCK_INPUT;

  /* Don't mess up with an empty value.  */
  if (!bytes_remaining)
    XChangeProperty (display, window, buffer_atom, XA_STRING, 8,
		     PropModeReplace, data, 0);

  while (bytes_remaining)
    {
      int chunk = (bytes_remaining < max_bytes
		   ? bytes_remaining : max_bytes);
      XChangeProperty (display, window, buffer_atom, XA_STRING, 8,
		       (bytes_remaining == bytes
			? PropModeReplace
			: PropModeAppend),
		       data, chunk);
      data += chunk;
      bytes_remaining -= chunk;
    }
  UNBLOCK_INPUT;
  return string;
}


DEFUN ("x-rotate-cut-buffers-internal", Fx_rotate_cut_buffers_internal,
  Sx_rotate_cut_buffers_internal, 1, 1, 0,
  "Rotate the values of the cut buffers by the given number of steps;\n\
positive means move values forward, negative means backward.")
  (n)
     Lisp_Object n;
{
  Window window;
  Atom props[8];
  Display *display;

  check_x ();
  display = FRAME_X_DISPLAY (selected_frame);
  window = RootWindow (display, 0); /* Cut buffers are on screen 0 */
  CHECK_NUMBER (n, 0);
  if (XINT (n) == 0)
    return n;
  if (! FRAME_X_DISPLAY_INFO (selected_frame)->cut_buffers_initialized)
    {
      initialize_cut_buffers (display, window);
      FRAME_X_DISPLAY_INFO (selected_frame)->cut_buffers_initialized = 1;
    }

  props[0] = XA_CUT_BUFFER0;
  props[1] = XA_CUT_BUFFER1;
  props[2] = XA_CUT_BUFFER2;
  props[3] = XA_CUT_BUFFER3;
  props[4] = XA_CUT_BUFFER4;
  props[5] = XA_CUT_BUFFER5;
  props[6] = XA_CUT_BUFFER6;
  props[7] = XA_CUT_BUFFER7;
  BLOCK_INPUT;
  XRotateWindowProperties (display, window, props, 8, XINT (n));
  UNBLOCK_INPUT;
  return n;
}

#endif

void
syms_of_xselect ()
{
  defsubr (&Sx_get_selection_internal);
  defsubr (&Sx_own_selection_internal);
  defsubr (&Sx_disown_selection_internal);
  defsubr (&Sx_selection_owner_p);
  defsubr (&Sx_selection_exists_p);

#ifdef CUT_BUFFER_SUPPORT
  defsubr (&Sx_get_cut_buffer_internal);
  defsubr (&Sx_store_cut_buffer_internal);
  defsubr (&Sx_rotate_cut_buffers_internal);
#endif

  reading_selection_reply = Fcons (Qnil, Qnil);
  staticpro (&reading_selection_reply);
  reading_selection_window = 0;
  reading_which_selection = 0;

  property_change_wait_list = 0;
  prop_location_identifier = 0;
  property_change_reply = Fcons (Qnil, Qnil);
  staticpro (&property_change_reply);

  Vselection_alist = Qnil;
  staticpro (&Vselection_alist);

  DEFVAR_LISP ("selection-converter-alist", &Vselection_converter_alist,
  "An alist associating X Windows selection-types with functions.\n\
These functions are called to convert the selection, with three args:\n\
the name of the selection (typically `PRIMARY', `SECONDARY', or `CLIPBOARD');\n\
a desired type to which the selection should be converted;\n\
and the local selection value (whatever was given to `x-own-selection').\n\
\n\
The function should return the value to send to the X server\n\
\(typically a string).  A return value of nil\n\
means that the conversion could not be done.\n\
A return value which is the symbol `NULL'\n\
means that a side-effect was executed,\n\
and there is no meaningful selection value.");
  Vselection_converter_alist = Qnil;

  DEFVAR_LISP ("x-lost-selection-hooks", &Vx_lost_selection_hooks,
  "A list of functions to be called when Emacs loses an X selection.\n\
\(This happens when some other X client makes its own selection\n\
or when a Lisp program explicitly clears the selection.)\n\
The functions are called with one argument, the selection type\n\
\(a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.)");
  Vx_lost_selection_hooks = Qnil;

  DEFVAR_LISP ("x-sent-selection-hooks", &Vx_sent_selection_hooks,
  "A list of functions to be called when Emacs answers a selection request.\n\
The functions are called with four arguments:\n\
  - the selection name (typically `PRIMARY', `SECONDARY', or `CLIPBOARD');\n\
  - the selection-type which Emacs was asked to convert the\n\
    selection into before sending (for example, `STRING' or `LENGTH');\n\
  - a flag indicating success or failure for responding to the request.\n\
We might have failed (and declined the request) for any number of reasons,\n\
including being asked for a selection that we no longer own, or being asked\n\
to convert into a type that we don't know about or that is inappropriate.\n\
This hook doesn't let you change the behavior of Emacs's selection replies,\n\
it merely informs you that they have happened.");
  Vx_sent_selection_hooks = Qnil;

  DEFVAR_INT ("x-selection-timeout", &x_selection_timeout,
   "Number of milliseconds to wait for a selection reply.\n\
If the selection owner doens't reply in this time, we give up.\n\
A value of 0 means wait as long as necessary.  This is initialized from the\n\
\"*selectionTimeout\" resource.");
  x_selection_timeout = 0;

  QPRIMARY   = intern ("PRIMARY");	staticpro (&QPRIMARY);
  QSECONDARY = intern ("SECONDARY");	staticpro (&QSECONDARY);
  QSTRING    = intern ("STRING");	staticpro (&QSTRING);
  QINTEGER   = intern ("INTEGER");	staticpro (&QINTEGER);
  QCLIPBOARD = intern ("CLIPBOARD");	staticpro (&QCLIPBOARD);
  QTIMESTAMP = intern ("TIMESTAMP");	staticpro (&QTIMESTAMP);
  QTEXT      = intern ("TEXT"); 	staticpro (&QTEXT);
  QTIMESTAMP = intern ("TIMESTAMP");	staticpro (&QTIMESTAMP);
  QDELETE    = intern ("DELETE");	staticpro (&QDELETE);
  QMULTIPLE  = intern ("MULTIPLE");	staticpro (&QMULTIPLE);
  QINCR      = intern ("INCR");		staticpro (&QINCR);
  QEMACS_TMP = intern ("_EMACS_TMP_");	staticpro (&QEMACS_TMP);
  QTARGETS   = intern ("TARGETS");	staticpro (&QTARGETS);
  QATOM	     = intern ("ATOM");		staticpro (&QATOM);
  QATOM_PAIR = intern ("ATOM_PAIR");	staticpro (&QATOM_PAIR);
  QNULL	     = intern ("NULL");		staticpro (&QNULL);

#ifdef CUT_BUFFER_SUPPORT
  QCUT_BUFFER0 = intern ("CUT_BUFFER0"); staticpro (&QCUT_BUFFER0);
  QCUT_BUFFER1 = intern ("CUT_BUFFER1"); staticpro (&QCUT_BUFFER1);
  QCUT_BUFFER2 = intern ("CUT_BUFFER2"); staticpro (&QCUT_BUFFER2);
  QCUT_BUFFER3 = intern ("CUT_BUFFER3"); staticpro (&QCUT_BUFFER3);
  QCUT_BUFFER4 = intern ("CUT_BUFFER4"); staticpro (&QCUT_BUFFER4);
  QCUT_BUFFER5 = intern ("CUT_BUFFER5"); staticpro (&QCUT_BUFFER5);
  QCUT_BUFFER6 = intern ("CUT_BUFFER6"); staticpro (&QCUT_BUFFER6);
  QCUT_BUFFER7 = intern ("CUT_BUFFER7"); staticpro (&QCUT_BUFFER7);
#endif

}
