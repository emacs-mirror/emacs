/* X Selection processing for Emacs.
   Copyright (C) 1993, 1994, 1995, 1996, 1997, 2000, 2001, 2003, 2004
   Free Software Foundation.

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


/* Rewritten by jwz */

#include <config.h>
#include <stdio.h>      /* termhooks.h needs this */

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "lisp.h"
#include "xterm.h"	/* for all of the X includes */
#include "dispextern.h"	/* frame.h seems to want this */
#include "frame.h"	/* Need this to get the X window of selected_frame */
#include "blockinput.h"
#include "buffer.h"
#include "process.h"
#include "termhooks.h"
#include "keyboard.h"

#include <X11/Xproto.h>

struct prop_location;

static Lisp_Object x_atom_to_symbol P_ ((Display *dpy, Atom atom));
static Atom symbol_to_x_atom P_ ((struct x_display_info *, Display *,
				  Lisp_Object));
static void x_own_selection P_ ((Lisp_Object, Lisp_Object));
static Lisp_Object x_get_local_selection P_ ((Lisp_Object, Lisp_Object, int));
static void x_decline_selection_request P_ ((struct input_event *));
static Lisp_Object x_selection_request_lisp_error P_ ((Lisp_Object));
static Lisp_Object queue_selection_requests_unwind P_ ((Lisp_Object));
static Lisp_Object some_frame_on_display P_ ((struct x_display_info *));
static void x_reply_selection_request P_ ((struct input_event *, int,
					   unsigned char *, int, Atom));
static int waiting_for_other_props_on_window P_ ((Display *, Window));
static struct prop_location *expect_property_change P_ ((Display *, Window,
							 Atom, int));
static void unexpect_property_change P_ ((struct prop_location *));
static Lisp_Object wait_for_property_change_unwind P_ ((Lisp_Object));
static void wait_for_property_change P_ ((struct prop_location *));
static Lisp_Object x_get_foreign_selection P_ ((Lisp_Object,
                                                Lisp_Object,
                                                Lisp_Object));
static void x_get_window_property P_ ((Display *, Window, Atom,
				       unsigned char **, int *,
				       Atom *, int *, unsigned long *, int));
static void receive_incremental_selection P_ ((Display *, Window, Atom,
					       Lisp_Object, unsigned,
					       unsigned char **, int *,
					       Atom *, int *, unsigned long *));
static Lisp_Object x_get_window_property_as_lisp_data P_ ((Display *,
							   Window, Atom,
							   Lisp_Object, Atom));
static Lisp_Object selection_data_to_lisp_data P_ ((Display *, unsigned char *,
						    int, Atom, int));
static void lisp_data_to_selection_data P_ ((Display *, Lisp_Object,
					     unsigned char **, Atom *,
					     unsigned *, int *, int *));
static Lisp_Object clean_local_selection_data P_ ((Lisp_Object));
static void initialize_cut_buffers P_ ((Display *, Window));


/* Printing traces to stderr.  */

#ifdef TRACE_SELECTION
#define TRACE0(fmt) \
  fprintf (stderr, "%d: " fmt "\n", getpid ())
#define TRACE1(fmt, a0) \
  fprintf (stderr, "%d: " fmt "\n", getpid (), a0)
#define TRACE2(fmt, a0, a1) \
  fprintf (stderr, "%d: " fmt "\n", getpid (), a0, a1)
#define TRACE3(fmt, a0, a1, a2) \
  fprintf (stderr, "%d: " fmt "\n", getpid (), a0, a1, a2)
#else
#define TRACE0(fmt)		(void) 0
#define TRACE1(fmt, a0)		(void) 0
#define TRACE2(fmt, a0, a1)	(void) 0
#define TRACE3(fmt, a0, a1)	(void) 0
#endif


#define CUT_BUFFER_SUPPORT

Lisp_Object QPRIMARY, QSECONDARY, QSTRING, QINTEGER, QCLIPBOARD, QTIMESTAMP,
  QTEXT, QDELETE, QMULTIPLE, QINCR, QEMACS_TMP, QTARGETS, QATOM, QNULL,
  QATOM_PAIR;

Lisp_Object QCOMPOUND_TEXT;	/* This is a type of selection.  */
Lisp_Object QUTF8_STRING;	/* This is a type of selection.  */

Lisp_Object Qcompound_text_with_extensions;

#ifdef CUT_BUFFER_SUPPORT
Lisp_Object QCUT_BUFFER0, QCUT_BUFFER1, QCUT_BUFFER2, QCUT_BUFFER3,
  QCUT_BUFFER4, QCUT_BUFFER5, QCUT_BUFFER6, QCUT_BUFFER7;
#endif

static Lisp_Object Vx_lost_selection_functions;
static Lisp_Object Vx_sent_selection_functions;
/* Coding system for communicating with other X clients via cutbuffer,
   selection, and clipboard.  */
static Lisp_Object Vselection_coding_system;

/* Coding system for the next communicating with other X clients.  */
static Lisp_Object Vnext_selection_coding_system;

static Lisp_Object Qforeign_selection;

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
/* Defined in keyboard.c.  */
extern unsigned long last_event_timestamp;

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
static EMACS_INT x_selection_timeout;



/* Define a queue to save up SELECTION_REQUEST_EVENT events for later
   handling.  */

struct selection_event_queue
  {
    struct input_event event;
    struct selection_event_queue *next;
  };

static struct selection_event_queue *selection_queue;

/* Nonzero means queue up SELECTION_REQUEST_EVENT events.  */

static int x_queue_selection_requests;

/* Queue up an SELECTION_REQUEST_EVENT *EVENT, to be processed later.  */

static void
x_queue_event (event)
     struct input_event *event;
{
  struct selection_event_queue *queue_tmp;

  /* Don't queue repeated requests.
     This only happens for large requests which uses the incremental protocol.  */
  for (queue_tmp = selection_queue; queue_tmp; queue_tmp = queue_tmp->next)
    {
      if (!bcmp (&queue_tmp->event, event, sizeof (*event)))
	{
	  TRACE1 ("DECLINE DUP SELECTION EVENT %08lx", (unsigned long)queue_tmp);
	  x_decline_selection_request (event);
	  return;
	}
    }

  queue_tmp
    = (struct selection_event_queue *) xmalloc (sizeof (struct selection_event_queue));

  if (queue_tmp != NULL)
    {
      TRACE1 ("QUEUE SELECTION EVENT %08lx", (unsigned long)queue_tmp);
      queue_tmp->event = *event;
      queue_tmp->next = selection_queue;
      selection_queue = queue_tmp;
    }
}

/* Start queuing SELECTION_REQUEST_EVENT events.  */

static void
x_start_queuing_selection_requests ()
{
  if (x_queue_selection_requests)
    abort ();

  x_queue_selection_requests++;
  TRACE1 ("x_start_queuing_selection_requests %d", x_queue_selection_requests);
}

/* Stop queuing SELECTION_REQUEST_EVENT events.  */

static void
x_stop_queuing_selection_requests ()
{
  TRACE1 ("x_stop_queuing_selection_requests %d", x_queue_selection_requests);
  --x_queue_selection_requests;

  /* Take all the queued events and put them back
     so that they get processed afresh.  */

  while (selection_queue != NULL)
    {
      struct selection_event_queue *queue_tmp = selection_queue;
      TRACE1 ("RESTORE SELECTION EVENT %08lx", (unsigned long)queue_tmp);
      kbd_buffer_unget_event (&queue_tmp->event);
      selection_queue = queue_tmp->next;
      xfree ((char *)queue_tmp);
    }
}


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
  if (EQ (sym, QCOMPOUND_TEXT)) return dpyinfo->Xatom_COMPOUND_TEXT;
  if (EQ (sym, QUTF8_STRING)) return dpyinfo->Xatom_UTF8_STRING;
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

  TRACE1 (" XInternAtom %s", (char *) SDATA (SYMBOL_NAME (sym)));
  BLOCK_INPUT;
  val = XInternAtom (display, (char *) SDATA (SYMBOL_NAME (sym)), False);
  UNBLOCK_INPUT;
  return val;
}


/* This converts a server Atom to a Lisp symbol, avoiding server roundtrips
   and calls to intern whenever possible.  */

static Lisp_Object
x_atom_to_symbol (dpy, atom)
     Display *dpy;
     Atom atom;
{
  struct x_display_info *dpyinfo;
  char *str;
  Lisp_Object val;

  if (! atom)
    return Qnil;

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

  dpyinfo = x_display_info_for_display (dpy);
  if (atom == dpyinfo->Xatom_CLIPBOARD)
    return QCLIPBOARD;
  if (atom == dpyinfo->Xatom_TIMESTAMP)
    return QTIMESTAMP;
  if (atom == dpyinfo->Xatom_TEXT)
    return QTEXT;
  if (atom == dpyinfo->Xatom_COMPOUND_TEXT)
    return QCOMPOUND_TEXT;
  if (atom == dpyinfo->Xatom_UTF8_STRING)
    return QUTF8_STRING;
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
  str = XGetAtomName (dpy, atom);
  UNBLOCK_INPUT;
  TRACE1 ("XGetAtomName --> %s", str);
  if (! str) return Qnil;
  val = intern (str);
  BLOCK_INPUT;
  /* This was allocated by Xlib, so use XFree.  */
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
  struct frame *sf = SELECTED_FRAME ();
  Window selecting_window = FRAME_X_WINDOW (sf);
  Display *display = FRAME_X_DISPLAY (sf);
  Time time = last_event_timestamp;
  Atom selection_atom;
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (sf);
  int count;

  CHECK_SYMBOL (selection_name);
  selection_atom = symbol_to_x_atom (dpyinfo, display, selection_name);

  BLOCK_INPUT;
  count = x_catch_errors (display);
  XSetSelectionOwner (display, selection_atom, selecting_window, time);
  x_check_errors (display, "Can't set selection: %s");
  x_uncatch_errors (display, count);
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
					  Fcons (selected_frame, Qnil))));
    prev_value = assq_no_quit (selection_name, Vselection_alist);

    Vselection_alist = Fcons (selection_data, Vselection_alist);

    /* If we already owned the selection, remove the old selection data.
       Perhaps we should destructively modify it instead.
       Don't use Fdelq as that may QUIT.  */
    if (!NILP (prev_value))
      {
	Lisp_Object rest;	/* we know it's not the CAR, so it's easy.  */
	for (rest = Vselection_alist; !NILP (rest); rest = Fcdr (rest))
	  if (EQ (prev_value, Fcar (XCDR (rest))))
	    {
	      XSETCDR (rest, Fcdr (XCDR (rest)));
	      break;
	    }
      }
  }
}

/* Given a selection-name and desired type, look up our local copy of
   the selection value and convert it to the type.
   The value is nil or a string.
   This function is used both for remote requests (LOCAL_REQUEST is zero)
   and for local x-get-selection-internal (LOCAL_REQUEST is nonzero).

   This calls random Lisp code, and may signal or gc.  */

static Lisp_Object
x_get_local_selection (selection_symbol, target_type, local_request)
     Lisp_Object selection_symbol, target_type;
     int local_request;
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
      value = XCAR (XCDR (XCDR (local_value)));
    }
#if 0
  else if (EQ (target_type, QDELETE))
    {
      handler_fn = Qnil;
      Fx_disown_selection_internal
	(selection_symbol,
	 XCAR (XCDR (XCDR (local_value))));
      value = QNULL;
    }
#endif

#if 0 /* #### MULTIPLE doesn't work yet */
  else if (CONSP (target_type)
	   && XCAR (target_type) == QMULTIPLE)
    {
      Lisp_Object pairs;
      int size;
      int i;
      pairs = XCDR (target_type);
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
				     XVECTOR (pair)->contents [1],
				     local_request);
	}
      return pairs;
    }
#endif
  else
    {
      /* Don't allow a quit within the converter.
	 When the user types C-g, he would be surprised
	 if by luck it came during a converter.  */
      count = SPECPDL_INDEX ();
      specbind (Qinhibit_quit, Qt);

      CHECK_SYMBOL (target_type);
      handler_fn = Fcdr (Fassq (target_type, Vselection_converter_alist));
      /* gcpro is not needed here since nothing but HANDLER_FN
	 is live, and that ought to be a symbol.  */

      if (!NILP (handler_fn))
	value = call3 (handler_fn,
		       selection_symbol, (local_request ? Qnil : target_type),
		       XCAR (XCDR (local_value)));
      else
	value = Qnil;
      unbind_to (count, Qnil);
    }

  /* Make sure this value is of a type that we could transmit
     to another X client.  */

  check = value;
  if (CONSP (value)
      && SYMBOLP (XCAR (value)))
    type = XCAR (value),
    check = XCDR (value);

  if (STRINGP (check)
      || VECTORP (check)
      || SYMBOLP (check)
      || INTEGERP (check)
      || NILP (value))
    return value;
  /* Check for a value that cons_to_long could handle.  */
  else if (CONSP (check)
	   && INTEGERP (XCAR (check))
	   && (INTEGERP (XCDR (check))
	       ||
	       (CONSP (XCDR (check))
		&& INTEGERP (XCAR (XCDR (check)))
		&& NILP (XCDR (XCDR (check))))))
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
  int count;

  reply.type = SelectionNotify;
  reply.display = SELECTION_EVENT_DISPLAY (event);
  reply.requestor = SELECTION_EVENT_REQUESTOR (event);
  reply.selection = SELECTION_EVENT_SELECTION (event);
  reply.time = SELECTION_EVENT_TIME (event);
  reply.target = SELECTION_EVENT_TARGET (event);
  reply.property = None;

  /* The reason for the error may be that the receiver has
     died in the meantime.  Handle that case.  */
  BLOCK_INPUT;
  count = x_catch_errors (reply.display);
  XSendEvent (reply.display, reply.requestor, False, 0L, (XEvent *) &reply);
  XFlush (reply.display);
  x_uncatch_errors (reply.display, count);
  UNBLOCK_INPUT;
}

/* This is the selection request currently being processed.
   It is set to zero when the request is fully processed.  */
static struct input_event *x_selection_current_request;

/* Display info in x_selection_request.  */

static struct x_display_info *selection_request_dpyinfo;

/* Used as an unwind-protect clause so that, if a selection-converter signals
   an error, we tell the requester that we were unable to do what they wanted
   before we throw to top-level or go into the debugger or whatever.  */

static Lisp_Object
x_selection_request_lisp_error (ignore)
     Lisp_Object ignore;
{
  if (x_selection_current_request != 0
      && selection_request_dpyinfo->display)
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
queue_selection_requests_unwind (tem)
     Lisp_Object tem;
{
  x_stop_queuing_selection_requests ();
  return Qnil;
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
  int count;

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
  count = x_catch_errors (display);

#ifdef TRACE_SELECTION
  {
    static int cnt;
    char *sel = XGetAtomName (display, reply.selection);
    char *tgt = XGetAtomName (display, reply.target);
    TRACE3 ("%s, target %s (%d)", sel, tgt, ++cnt);
    if (sel) XFree (sel);
    if (tgt) XFree (tgt);
  }
#endif /* TRACE_SELECTION */

  /* Store the data on the requested property.
     If the selection is large, only store the first N bytes of it.
   */
  bytes_remaining = size * format_bytes;
  if (bytes_remaining <= max_bytes)
    {
      /* Send all the data at once, with minimal handshaking.  */
      TRACE1 ("Sending all %d bytes", bytes_remaining);
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
      Lisp_Object frame;

      frame = some_frame_on_display (dpyinfo);

      /* If the display no longer has frames, we can't expect
	 to get many more selection requests from it, so don't
	 bother trying to queue them.  */
      if (!NILP (frame))
	{
	  x_start_queuing_selection_requests ();

	  record_unwind_protect (queue_selection_requests_unwind,
				 Qnil);
	}

      if (x_window_to_frame (dpyinfo, window)) /* #### debug */
	error ("Attempt to transfer an INCR to ourself!");

      TRACE2 ("Start sending %d bytes incrementally (%s)",
	      bytes_remaining,  XGetAtomName (display, reply.property));
      wait_object = expect_property_change (display, window, reply.property,
					    PropertyDelete);

      TRACE1 ("Set %s to number of bytes to send",
	      XGetAtomName (display, reply.property));
      {
        /* XChangeProperty expects an array of long even if long is more than
           32 bits.  */
        long value[1];

        value[0] = bytes_remaining;
        XChangeProperty (display, window, reply.property, dpyinfo->Xatom_INCR,
                         32, PropModeReplace,
                         (unsigned char *) value, 1);
      }

      XSelectInput (display, window, PropertyChangeMask);

      /* Tell 'em the INCR data is there...  */
      TRACE0 ("Send SelectionNotify event");
      XSendEvent (display, window, False, 0L, (XEvent *) &reply);
      XFlush (display);

      had_errors = x_had_errors_p (display);
      UNBLOCK_INPUT;

      /* First, wait for the requester to ack by deleting the property.
	 This can run random lisp code (process handlers) or signal.  */
      if (! had_errors)
	{
	  TRACE1 ("Waiting for ACK (deletion of %s)",
		  XGetAtomName (display, reply.property));
	  wait_for_property_change (wait_object);
	}
      else
	unexpect_property_change (wait_object);

      TRACE0 ("Got ACK");
      while (bytes_remaining)
	{
          int i = ((bytes_remaining < max_bytes)
                   ? bytes_remaining
                   : max_bytes);

	  BLOCK_INPUT;

	  wait_object
	    = expect_property_change (display, window, reply.property,
				      PropertyDelete);

	  TRACE1 ("Sending increment of %d bytes", i);
	  TRACE1 ("Set %s to increment data",
		  XGetAtomName (display, reply.property));

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

	  /* Now wait for the requester to ack this chunk by deleting the
	     property.	 This can run random lisp code or signal.  */
	  TRACE1 ("Waiting for increment ACK (deletion of %s)",
		  XGetAtomName (display, reply.property));
	  wait_for_property_change (wait_object);
	}

      /* Now write a zero-length chunk to the property to tell the
	 requester that we're done.  */
      BLOCK_INPUT;
      if (! waiting_for_other_props_on_window (display, window))
	XSelectInput (display, window, 0L);

      TRACE1 ("Set %s to a 0-length chunk to indicate EOF",
	      XGetAtomName (display, reply.property));
      XChangeProperty (display, window, reply.property, type, format,
		       PropModeReplace, data, 0);
      TRACE0 ("Done sending incrementally");
    }

  /* rms, 2003-01-03: I think I have fixed this bug.  */
  /* The window we're communicating with may have been deleted
     in the meantime (that's a real situation from a bug report).
     In this case, there may be events in the event queue still
     refering to the deleted window, and we'll get a BadWindow error
     in XTread_socket when processing the events.  I don't have
     an idea how to fix that.  gerd, 2001-01-98.   */
  /* 2004-09-10: XSync and UNBLOCK so that possible protocol errors are
     delivered before uncatch errors.  */
  XSync (display, False);
  UNBLOCK_INPUT;

  /* GTK queues events in addition to the queue in Xlib.  So we
     UNBLOCK to enter the event loop and get possible errors delivered,
     and then BLOCK again because x_uncatch_errors requires it.  */
  BLOCK_INPUT;
  x_uncatch_errors (display, count);
  UNBLOCK_INPUT;
}

/* Handle a SelectionRequest event EVENT.
   This is called from keyboard.c when such an event is found in the queue.  */

static void
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

  TRACE2 ("x_handle_selection_request, from=0x%08lx time=%lu",
	  (unsigned long) SELECTION_EVENT_REQUESTOR (event),
	  (unsigned long) SELECTION_EVENT_TIME (event));

  local_selection_data = Qnil;
  target_symbol = Qnil;
  converted_selection = Qnil;
  successful_p = Qnil;

  GCPRO3 (local_selection_data, converted_selection, target_symbol);

  selection_symbol = x_atom_to_symbol (SELECTION_EVENT_DISPLAY (event),
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
    cons_to_long (XCAR (XCDR (XCDR (local_selection_data))));

  if (SELECTION_EVENT_TIME (event) != CurrentTime
      && local_selection_time > SELECTION_EVENT_TIME (event))
    {
      /* Someone asked for the selection, and we have one, but not the one
	 they're looking for.
       */
      x_decline_selection_request (event);
      goto DONE;
    }

  x_selection_current_request = event;
  count = SPECPDL_INDEX ();
  selection_request_dpyinfo = dpyinfo;
  record_unwind_protect (x_selection_request_lisp_error, Qnil);

  target_symbol = x_atom_to_symbol (SELECTION_EVENT_DISPLAY (event),
				    SELECTION_EVENT_TARGET (event));

#if 0 /* #### MULTIPLE doesn't work yet */
  if (EQ (target_symbol, QMULTIPLE))
    target_symbol = fetch_multiple_target (event);
#endif

  /* Convert lisp objects back into binary data */

  converted_selection
    = x_get_local_selection (selection_symbol, target_symbol, 0);

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

      /* Use xfree, not XFree, because lisp_data_to_selection_data
	 calls xmalloc itself.  */
      if (!nofree)
	xfree (data);
    }
  unbind_to (count, Qnil);

 DONE:

  /* Let random lisp code notice that the selection has been asked for.  */
  {
    Lisp_Object rest;
    rest = Vx_sent_selection_functions;
    if (!EQ (rest, Qunbound))
      for (; CONSP (rest); rest = Fcdr (rest))
	call3 (Fcar (rest), selection_symbol, target_symbol, successful_p);
  }

  UNGCPRO;
}

/* Handle a SelectionClear event EVENT, which indicates that some
   client cleared out our previously asserted selection.
   This is called from keyboard.c when such an event is found in the queue.  */

static void
x_handle_selection_clear (event)
     struct input_event *event;
{
  Display *display = SELECTION_EVENT_DISPLAY (event);
  Atom selection = SELECTION_EVENT_SELECTION (event);
  Time changed_owner_time = SELECTION_EVENT_TIME (event);

  Lisp_Object selection_symbol, local_selection_data;
  Time local_selection_time;
  struct x_display_info *dpyinfo = x_display_info_for_display (display);
  struct x_display_info *t_dpyinfo;

  TRACE0 ("x_handle_selection_clear");

  /* If the new selection owner is also Emacs,
     don't clear the new selection.  */
  BLOCK_INPUT;
  /* Check each display on the same terminal,
     to see if this Emacs job now owns the selection
     through that display.  */
  for (t_dpyinfo = x_display_list; t_dpyinfo; t_dpyinfo = t_dpyinfo->next)
    if (t_dpyinfo->kboard == dpyinfo->kboard)
      {
	Window owner_window
	  = XGetSelectionOwner (t_dpyinfo->display, selection);
	if (x_window_to_frame (t_dpyinfo, owner_window) != 0)
	  {
	    UNBLOCK_INPUT;
	    return;
	  }
      }
  UNBLOCK_INPUT;

  selection_symbol = x_atom_to_symbol (display, selection);

  local_selection_data = assq_no_quit (selection_symbol, Vselection_alist);

  /* Well, we already believe that we don't own it, so that's just fine.  */
  if (NILP (local_selection_data)) return;

  local_selection_time = (Time)
    cons_to_long (XCAR (XCDR (XCDR (local_selection_data))));

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
	if (EQ (local_selection_data, Fcar (XCDR (rest))))
	  {
	    XSETCDR (rest, Fcdr (XCDR (rest)));
	    break;
	  }
    }

  /* Let random lisp code notice that the selection has been stolen.  */

  {
    Lisp_Object rest;
    rest = Vx_lost_selection_functions;
    if (!EQ (rest, Qunbound))
      {
	for (; CONSP (rest); rest = Fcdr (rest))
	  call1 (Fcar (rest), selection_symbol);
	prepare_menu_bars ();
	redisplay_preserve_echo_area (20);
      }
  }
}

void
x_handle_selection_event (event)
     struct input_event *event;
{
  TRACE0 ("x_handle_selection_event");

  if (event->kind == SELECTION_REQUEST_EVENT)
    {
      if (x_queue_selection_requests)
	x_queue_event (event);
      else
	x_handle_selection_request (event);
    }
  else
    x_handle_selection_clear (event);
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

  /* Delete elements from the beginning of Vselection_alist.  */
  while (!NILP (Vselection_alist)
	 && EQ (frame, Fcar (Fcdr (Fcdr (Fcdr (Fcar (Vselection_alist)))))))
    {
      /* Let random Lisp code notice that the selection has been stolen.  */
      Lisp_Object hooks, selection_symbol;

      hooks = Vx_lost_selection_functions;
      selection_symbol = Fcar (Fcar (Vselection_alist));

      if (!EQ (hooks, Qunbound))
	{
	  for (; CONSP (hooks); hooks = Fcdr (hooks))
	    call1 (Fcar (hooks), selection_symbol);
#if 0 /* This can crash when deleting a frame
	 from x_connection_closed.  Anyway, it seems unnecessary;
	 something else should cause a redisplay.  */
	  redisplay_preserve_echo_area (21);
#endif
	}

      Vselection_alist = Fcdr (Vselection_alist);
    }

  /* Delete elements after the beginning of Vselection_alist.  */
  for (rest = Vselection_alist; !NILP (rest); rest = Fcdr (rest))
    if (EQ (frame, Fcar (Fcdr (Fcdr (Fcdr (Fcar (XCDR (rest))))))))
      {
	/* Let random Lisp code notice that the selection has been stolen.  */
	Lisp_Object hooks, selection_symbol;

	hooks = Vx_lost_selection_functions;
	selection_symbol = Fcar (Fcar (XCDR (rest)));

	if (!EQ (hooks, Qunbound))
	  {
	    for (; CONSP (hooks); hooks = Fcdr (hooks))
	      call1 (Fcar (hooks), selection_symbol);
#if 0 /* See above */
	    redisplay_preserve_echo_area (22);
#endif
	  }
	XSETCDR (rest, Fcdr (XCDR (rest)));
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
     Atom property;
     int state;
{
  struct prop_location *pl = (struct prop_location *) xmalloc (sizeof *pl);
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
wait_for_property_change_unwind (loc)
     Lisp_Object loc;
{
  struct prop_location *location = XSAVE_VALUE (loc)->pointer;

  unexpect_property_change (location);
  if (location == property_change_reply_object)
    property_change_reply_object = 0;
  return Qnil;
}

/* Actually wait for a property change.
   IDENTIFIER should be the value that expect_property_change returned.  */

static void
wait_for_property_change (location)
     struct prop_location *location;
{
  int secs, usecs;
  int count = SPECPDL_INDEX ();

  if (property_change_reply_object)
    abort ();

  /* Make sure to do unexpect_property_change if we quit or err.  */
  record_unwind_protect (wait_for_property_change_unwind,
			 make_save_value (location, 0));

  XSETCAR (property_change_reply, Qnil);
  property_change_reply_object = location;

  /* If the event we are waiting for arrives beyond here, it will set
     property_change_reply, because property_change_reply_object says so.  */
  if (! location->arrived)
    {
      secs = x_selection_timeout / 1000;
      usecs = (x_selection_timeout % 1000) * 1000;
      TRACE2 ("  Waiting %d secs, %d usecs", secs, usecs);
      wait_reading_process_output (secs, usecs, 0, 0,
				   property_change_reply, NULL, 0);

      if (NILP (XCAR (property_change_reply)))
	{
	  TRACE0 ("  Timed out");
	  error ("Timed out waiting for property-notify event");
	}
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
      if (!rest->arrived
	  && rest->property == event->atom
	  && rest->window == event->window
	  && rest->display == event->display
	  && rest->desired_state == event->state)
	{
	  TRACE2 ("Expected %s of property %s",
		  (event->state == PropertyDelete ? "deletion" : "change"),
		  XGetAtomName (event->display, event->atom));

	  rest->arrived = 1;

	  /* If this is the one wait_for_property_change is waiting for,
	     tell it to wake up.  */
	  if (rest == property_change_reply_object)
	    XSETCAR (property_change_reply, Qt);

	  return;
	}

      prev = rest;
      rest = rest->next;
    }
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
    return Fcons (XCAR (obj), copy_multiple_data (XCDR (obj)));

  CHECK_VECTOR (obj);
  vec = Fmake_vector (size = XVECTOR (obj)->size, Qnil);
  for (i = 0; i < size; i++)
    {
      Lisp_Object vec2 = XVECTOR (obj)->contents [i];
      CHECK_VECTOR (vec2);
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
x_get_foreign_selection (selection_symbol, target_type, time_stamp)
     Lisp_Object selection_symbol, target_type, time_stamp;
{
  struct frame *sf = SELECTED_FRAME ();
  Window requestor_window = FRAME_X_WINDOW (sf);
  Display *display = FRAME_X_DISPLAY (sf);
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (sf);
  Time requestor_time = last_event_timestamp;
  Atom target_property = dpyinfo->Xatom_EMACS_TMP;
  Atom selection_atom = symbol_to_x_atom (dpyinfo, display, selection_symbol);
  Atom type_atom;
  int secs, usecs;
  int count;
  Lisp_Object frame;

  if (CONSP (target_type))
    type_atom = symbol_to_x_atom (dpyinfo, display, XCAR (target_type));
  else
    type_atom = symbol_to_x_atom (dpyinfo, display, target_type);

  if (! NILP (time_stamp))
    {
      if (CONSP (time_stamp))
        requestor_time = (Time) cons_to_long (time_stamp);
      else if (INTEGERP (time_stamp))
        requestor_time = (Time) XUINT (time_stamp);
      else if (FLOATP (time_stamp))
        requestor_time = (Time) XFLOAT_DATA (time_stamp);
      else
        error ("TIME_STAMP must be cons or number");
    }

  BLOCK_INPUT;

  count = x_catch_errors (display);

  TRACE2 ("Get selection %s, type %s",
	  XGetAtomName (display, type_atom),
	  XGetAtomName (display, target_property));

  XConvertSelection (display, selection_atom, type_atom, target_property,
		     requestor_window, requestor_time);
  XFlush (display);

  /* Prepare to block until the reply has been read.  */
  reading_selection_window = requestor_window;
  reading_which_selection = selection_atom;
  XSETCAR (reading_selection_reply, Qnil);

  frame = some_frame_on_display (dpyinfo);

  /* If the display no longer has frames, we can't expect
     to get many more selection requests from it, so don't
     bother trying to queue them.  */
  if (!NILP (frame))
    {
      x_start_queuing_selection_requests ();

      record_unwind_protect (queue_selection_requests_unwind,
			     Qnil);
    }
  UNBLOCK_INPUT;

  /* This allows quits.  Also, don't wait forever.  */
  secs = x_selection_timeout / 1000;
  usecs = (x_selection_timeout % 1000) * 1000;
  TRACE1 ("  Start waiting %d secs for SelectionNotify", secs);
  wait_reading_process_output (secs, usecs, 0, 0,
			       reading_selection_reply, NULL, 0);
  TRACE1 ("  Got event = %d", !NILP (XCAR (reading_selection_reply)));

  BLOCK_INPUT;
  x_check_errors (display, "Cannot get selection: %s");
  x_uncatch_errors (display, count);
  UNBLOCK_INPUT;

  if (NILP (XCAR (reading_selection_reply)))
    error ("Timed out waiting for reply from selection owner");
  if (EQ (XCAR (reading_selection_reply), Qlambda))
    error ("No `%s' selection", SDATA (SYMBOL_NAME (selection_symbol)));

  /* Otherwise, the selection is waiting for us on the requested property.  */
  return
    x_get_window_property_as_lisp_data (display, requestor_window,
					target_property, target_type,
					selection_atom);
}

/* Subroutines of x_get_window_property_as_lisp_data */

/* Use xfree, not XFree, to free the data obtained with this function.  */

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

  if (buffer_size > MAX_SELECTION_QUANTUM)
    buffer_size = MAX_SELECTION_QUANTUM;

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

  /* This was allocated by Xlib, so use XFree.  */
  XFree ((char *) tmp_data);

  if (*actual_type_ret == None || *actual_format_ret == 0)
    {
      UNBLOCK_INPUT;
      return;
    }

  total_size = bytes_remaining + 1;
  *data_ret = (unsigned char *) xmalloc (total_size);

  /* Now read, until we've gotten it all.  */
  while (bytes_remaining)
    {
#ifdef TRACE_SELECTION
      int last = bytes_remaining;
#endif
      result
	= XGetWindowProperty (display, window, property,
			      (long)offset/4, (long)buffer_size/4,
			      False,
			      AnyPropertyType,
			      actual_type_ret, actual_format_ret,
			      actual_size_ret, &bytes_remaining, &tmp_data);

      TRACE2 ("Read %ld bytes from property %s",
	      last - bytes_remaining,
	      XGetAtomName (display, property));

      /* If this doesn't return Success at this point, it means that
	 some clod deleted the selection while we were in the midst of
	 reading it.  Deal with that, I guess.... */
      if (result != Success)
	break;

      /* The man page for XGetWindowProperty says:
         "If the returned format is 32, the returned data is represented
          as a long array and should be cast to that type to obtain the
          elements."
         This applies even if long is more than 32 bits, the X library
         converts from 32 bit elements received from the X server to long
         and passes the long array to us.  Thus, for that case bcopy can not
         be used.  We convert to a 32 bit type here, because so much code
         assume on that.

         The bytes and offsets passed to XGetWindowProperty refers to the
         property and those are indeed in 32 bit quantities if format is 32.  */

      if (*actual_format_ret == 32 && *actual_format_ret < BITS_PER_LONG)
        {
          unsigned long i;
          int  *idata = (int *) ((*data_ret) + offset);
          long *ldata = (long *) tmp_data;

          for (i = 0; i < *actual_size_ret; ++i)
            {
              idata[i]= (int) ldata[i];
              offset += 4;
            }
        }
      else
        {
          *actual_size_ret *= *actual_format_ret / 8;
          bcopy (tmp_data, (*data_ret) + offset, *actual_size_ret);
          offset += *actual_size_ret;
        }

      /* This was allocated by Xlib, so use XFree.  */
      XFree ((char *) tmp_data);
    }

  XFlush (display);
  UNBLOCK_INPUT;
  *bytes_ret = offset;
}

/* Use xfree, not XFree, to free the data obtained with this function.  */

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

  TRACE1 ("Read %d bytes incrementally", min_size_bytes);

  /* At this point, we have read an INCR property.
     Delete the property to ack it.
     (But first, prepare to receive the next event in this handshake.)

     Now, we must loop, waiting for the sending window to put a value on
     that property, then reading the property, then deleting it to ack.
     We are done when the sender places a property of length 0.
   */
  BLOCK_INPUT;
  XSelectInput (display, window, STANDARD_EVENT_SET | PropertyChangeMask);
  TRACE1 ("  Delete property %s",
	  SDATA (SYMBOL_NAME (x_atom_to_symbol (display, property))));
  XDeleteProperty (display, window, property);
  TRACE1 ("  Expect new value of property %s",
	  SDATA (SYMBOL_NAME (x_atom_to_symbol (display, property))));
  wait_object = expect_property_change (display, window, property,
					PropertyNewValue);
  XFlush (display);
  UNBLOCK_INPUT;

  while (1)
    {
      unsigned char *tmp_data;
      int tmp_size_bytes;

      TRACE0 ("  Wait for property change");
      wait_for_property_change (wait_object);

      /* expect it again immediately, because x_get_window_property may
	 .. no it won't, I don't get it.
	 .. Ok, I get it now, the Xt code that implements INCR is broken. */
      TRACE0 ("  Get property value");
      x_get_window_property (display, window, property,
			     &tmp_data, &tmp_size_bytes,
			     type_ret, format_ret, size_ret, 1);

      TRACE1 ("  Read increment of %d bytes", tmp_size_bytes);

      if (tmp_size_bytes == 0) /* we're done */
	{
	  TRACE0 ("Done reading incrementally");

	  if (! waiting_for_other_props_on_window (display, window))
	    XSelectInput (display, window, STANDARD_EVENT_SET);
	  /* Use xfree, not XFree, because x_get_window_property
	     calls xmalloc itself.  */
	  if (tmp_data) xfree (tmp_data);
	  break;
	}

      BLOCK_INPUT;
      TRACE1 ("  ACK by deleting property %s",
	      XGetAtomName (display, property));
      XDeleteProperty (display, window, property);
      wait_object = expect_property_change (display, window, property,
					    PropertyNewValue);
      XFlush (display);
      UNBLOCK_INPUT;

      if (*size_bytes_ret < offset + tmp_size_bytes)
	{
	  *size_bytes_ret = offset + tmp_size_bytes;
	  *data_ret = (unsigned char *) xrealloc (*data_ret, *size_bytes_ret);
	}

      bcopy (tmp_data, (*data_ret) + offset, tmp_size_bytes);
      offset += tmp_size_bytes;

      /* Use xfree, not XFree, because x_get_window_property
	 calls xmalloc itself.  */
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

  TRACE0 ("Reading selection data");

  x_get_window_property (display, window, property, &data, &bytes,
			 &actual_type, &actual_format, &actual_size, 1);
  if (! data)
    {
      int there_is_a_selection_owner;
      BLOCK_INPUT;
      there_is_a_selection_owner
	= XGetSelectionOwner (display, selection_atom);
      UNBLOCK_INPUT;
      Fsignal (Qerror,
	       there_is_a_selection_owner
	       ? Fcons (build_string ("selection owner couldn't convert"),
			actual_type
			? Fcons (target_type,
				 Fcons (x_atom_to_symbol (display,
							  actual_type),
					Qnil))
			: Fcons (target_type, Qnil))
	       : Fcons (build_string ("no selection"),
			Fcons (x_atom_to_symbol (display,
						 selection_atom),
			       Qnil)));
    }

  if (actual_type == dpyinfo->Xatom_INCR)
    {
      /* That wasn't really the data, just the beginning.  */

      unsigned int min_size_bytes = * ((unsigned int *) data);
      BLOCK_INPUT;
      /* Use xfree, not XFree, because x_get_window_property
	 calls xmalloc itself.  */
      xfree ((char *) data);
      UNBLOCK_INPUT;
      receive_incremental_selection (display, window, property, target_type,
				     min_size_bytes, &data, &bytes,
				     &actual_type, &actual_format,
				     &actual_size);
    }

  BLOCK_INPUT;
  TRACE1 ("  Delete property %s", XGetAtomName (display, property));
  XDeleteProperty (display, window, property);
  XFlush (display);
  UNBLOCK_INPUT;

  /* It's been read.  Now convert it to a lisp object in some semi-rational
     manner.  */
  val = selection_data_to_lisp_data (display, data, bytes,
				     actual_type, actual_format);

  /* Use xfree, not XFree, because x_get_window_property
     calls xmalloc itself.  */
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
   representation are as above.

   Important: When format is 32, data should contain an array of int,
   not an array of long as the X library returns.  This makes a difference
   when sizeof(long) != sizeof(int).  */



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
    {
      Lisp_Object str, lispy_type;

      str = make_unibyte_string ((char *) data, size);
      /* Indicate that this string is from foreign selection by a text
	 property `foreign-selection' so that the caller of
	 x-get-selection-internal (usually x-get-selection) can know
	 that the string must be decode.  */
      if (type == dpyinfo->Xatom_COMPOUND_TEXT)
	lispy_type = QCOMPOUND_TEXT;
      else if (type == dpyinfo->Xatom_UTF8_STRING)
	lispy_type = QUTF8_STRING;
      else
	lispy_type = QSTRING;
      Fput_text_property (make_number (0), make_number (size),
			  Qforeign_selection, lispy_type, str);
      return str;
    }
  /* Convert a single atom to a Lisp_Symbol.  Convert a set of atoms to
     a vector of symbols.
   */
  else if (type == XA_ATOM)
    {
      int i;
      /* On a 64 bit machine sizeof(Atom) == sizeof(long) == 8.
         But the callers of these function has made sure the data for
         format == 32 is an array of int.  Thus, use int instead
         of Atom.  */
      int *idata = (int *) data;

      if (size == sizeof (int))
	return x_atom_to_symbol (display, (Atom) idata[0]);
      else
	{
	  Lisp_Object v = Fmake_vector (make_number (size / sizeof (int)),
					make_number (0));
	  for (i = 0; i < size / sizeof (int); i++)
	    Faset (v, make_number (i),
                   x_atom_to_symbol (display, (Atom) idata[i]));
	  return v;
	}
    }

  /* Convert a single 16 or small 32 bit number to a Lisp_Int.
     If the number is > 16 bits, convert it to a cons of integers,
     16 bits in each half.
   */
  else if (format == 32 && size == sizeof (int))
    return long_to_cons (((unsigned int *) data) [0]);
  else if (format == 16 && size == sizeof (short))
    return make_number ((int) (((unsigned short *) data) [0]));

  /* Convert any other kind of data to a vector of numbers, represented
     as above (as an integer, or a cons of two 16 bit integers.)
   */
  else if (format == 16)
    {
      int i;
      Lisp_Object v;
      v = Fmake_vector (make_number (size / 2), make_number (0));
      for (i = 0; i < size / 2; i++)
	{
	  int j = (int) ((unsigned short *) data) [i];
	  Faset (v, make_number (i), make_number (j));
	}
      return v;
    }
  else
    {
      int i;
      Lisp_Object v = Fmake_vector (make_number (size / 4), make_number (0));
      for (i = 0; i < size / 4; i++)
	{
	  unsigned int j = ((unsigned int *) data) [i];
	  Faset (v, make_number (i), long_to_cons (j));
	}
      return v;
    }
}


/* Use xfree, not XFree, to free the data obtained with this function.  */

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

  if (CONSP (obj) && SYMBOLP (XCAR (obj)))
    {
      type = XCAR (obj);
      obj = XCDR (obj);
      if (CONSP (obj) && NILP (XCDR (obj)))
	obj = XCAR (obj);
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
      if (SCHARS (obj) < SBYTES (obj))
	/* OBJ is a multibyte string containing a non-ASCII char.  */
	Fsignal (Qerror, /* Qselection_error */
		 Fcons (build_string
			("Non-ASCII string must be encoded in advance"),
			Fcons (obj, Qnil)));
      if (NILP (type))
	type = QSTRING;
      *format_ret = 8;
      *size_ret = SBYTES (obj);
      *data_ret = SDATA (obj);
      *nofree_ret = 1;
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
	   || (CONSP (obj) && INTEGERP (XCAR (obj))
	       && (INTEGERP (XCDR (obj))
		   || (CONSP (XCDR (obj))
		       && INTEGERP (XCAR (XCDR (obj)))))))
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
          int data_size = 2;
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

          /* Use sizeof(long) even if it is more than 32 bits.  See comment
             in x_get_window_property and x_fill_property_data.  */

          if (*format_ret == 32) data_size = sizeof(long);
	  *data_ret = (unsigned char *) xmalloc (*size_ret * data_size);
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
	     Fcons (build_string ("unrecognized selection data"),
		    Fcons (obj, Qnil)));

  *type_ret = symbol_to_x_atom (dpyinfo, display, type);
}

static Lisp_Object
clean_local_selection_data (obj)
     Lisp_Object obj;
{
  if (CONSP (obj)
      && INTEGERP (XCAR (obj))
      && CONSP (XCDR (obj))
      && INTEGERP (XCAR (XCDR (obj)))
      && NILP (XCDR (XCDR (obj))))
    obj = Fcons (XCAR (obj), XCDR (obj));

  if (CONSP (obj)
      && INTEGERP (XCAR (obj))
      && INTEGERP (XCDR (obj)))
    {
      if (XINT (XCAR (obj)) == 0)
	return XCDR (obj);
      if (XINT (XCAR (obj)) == -1)
	return make_number (- XINT (XCDR (obj)));
    }
  if (VECTORP (obj))
    {
      int i;
      int size = XVECTOR (obj)->size;
      Lisp_Object copy;
      if (size == 1)
	return clean_local_selection_data (XVECTOR (obj)->contents [0]);
      copy = Fmake_vector (make_number (size), Qnil);
      for (i = 0; i < size; i++)
	XVECTOR (copy)->contents [i]
	  = clean_local_selection_data (XVECTOR (obj)->contents [i]);
      return copy;
    }
  return obj;
}

/* Called from XTread_socket to handle SelectionNotify events.
   If it's the selection we are waiting for, stop waiting
   by setting the car of reading_selection_reply to non-nil.
   We store t there if the reply is successful, lambda if not.  */

void
x_handle_selection_notify (event)
     XSelectionEvent *event;
{
  if (event->requestor != reading_selection_window)
    return;
  if (event->selection != reading_which_selection)
    return;

  TRACE0 ("Received SelectionNotify");
  XSETCAR (reading_selection_reply,
	   (event->property != 0 ? Qt : Qlambda));
}


DEFUN ("x-own-selection-internal", Fx_own_selection_internal,
       Sx_own_selection_internal, 2, 2, 0,
       doc: /* Assert an X selection of the given TYPE with the given VALUE.
TYPE is a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)
VALUE is typically a string, or a cons of two markers, but may be
anything that the functions on `selection-converter-alist' know about.  */)
     (selection_name, selection_value)
     Lisp_Object selection_name, selection_value;
{
  check_x ();
  CHECK_SYMBOL (selection_name);
  if (NILP (selection_value)) error ("SELECTION-VALUE may not be nil");
  x_own_selection (selection_name, selection_value);
  return selection_value;
}


/* Request the selection value from the owner.  If we are the owner,
   simply return our selection value.  If we are not the owner, this
   will block until all of the data has arrived.  */

DEFUN ("x-get-selection-internal", Fx_get_selection_internal,
       Sx_get_selection_internal, 2, 3, 0,
       doc: /* Return text selected from some X window.
SELECTION is a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)
TYPE is the type of data desired, typically `STRING'.
TIME_STAMP is the time to use in the XConvertSelection call for foreign
selections.  If omitted, defaults to the time for the last event.  */)
  (selection_symbol, target_type, time_stamp)
     Lisp_Object selection_symbol, target_type, time_stamp;
{
  Lisp_Object val = Qnil;
  struct gcpro gcpro1, gcpro2;
  GCPRO2 (target_type, val); /* we store newly consed data into these */
  check_x ();
  CHECK_SYMBOL (selection_symbol);

#if 0 /* #### MULTIPLE doesn't work yet */
  if (CONSP (target_type)
      && XCAR (target_type) == QMULTIPLE)
    {
      CHECK_VECTOR (XCDR (target_type));
      /* So we don't destructively modify this...  */
      target_type = copy_multiple_data (target_type);
    }
  else
#endif
    CHECK_SYMBOL (target_type);

  val = x_get_local_selection (selection_symbol, target_type, 1);

  if (NILP (val))
    {
      val = x_get_foreign_selection (selection_symbol, target_type, time_stamp);
      goto DONE;
    }

  if (CONSP (val)
      && SYMBOLP (XCAR (val)))
    {
      val = XCDR (val);
      if (CONSP (val) && NILP (XCDR (val)))
	val = XCAR (val);
    }
  val = clean_local_selection_data (val);
 DONE:
  UNGCPRO;
  return val;
}

DEFUN ("x-disown-selection-internal", Fx_disown_selection_internal,
       Sx_disown_selection_internal, 1, 2, 0,
       doc: /* If we own the selection SELECTION, disown it.
Disowning it means there is no such selection.  */)
     (selection, time)
     Lisp_Object selection;
     Lisp_Object time;
{
  Time timestamp;
  Atom selection_atom;
  union {
    struct selection_input_event sie;
    struct input_event ie;
  } event;
  Display *display;
  struct x_display_info *dpyinfo;
  struct frame *sf = SELECTED_FRAME ();

  check_x ();
  display = FRAME_X_DISPLAY (sf);
  dpyinfo = FRAME_X_DISPLAY_INFO (sf);
  CHECK_SYMBOL (selection);
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
  SELECTION_EVENT_DISPLAY (&event.sie) = display;
  SELECTION_EVENT_SELECTION (&event.sie) = selection_atom;
  SELECTION_EVENT_TIME (&event.sie) = timestamp;
  x_handle_selection_clear (&event.ie);

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

  for (tail = Vselection_alist; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object elt, value;
      elt = XCAR (tail);
      value = XCDR (elt);
      if (CONSP (value) && MARKERP (XCAR (value))
	  && XMARKER (XCAR (value))->buffer == buf)
	Fx_disown_selection_internal (XCAR (elt), Qnil);
    }
}

DEFUN ("x-selection-owner-p", Fx_selection_owner_p, Sx_selection_owner_p,
       0, 1, 0,
       doc: /* Whether the current Emacs process owns the given X Selection.
The arg should be the name of the selection in question, typically one of
the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)
For convenience, the symbol nil is the same as `PRIMARY',
and t is the same as `SECONDARY'.  */)
     (selection)
     Lisp_Object selection;
{
  check_x ();
  CHECK_SYMBOL (selection);
  if (EQ (selection, Qnil)) selection = QPRIMARY;
  if (EQ (selection, Qt)) selection = QSECONDARY;

  if (NILP (Fassq (selection, Vselection_alist)))
    return Qnil;
  return Qt;
}

DEFUN ("x-selection-exists-p", Fx_selection_exists_p, Sx_selection_exists_p,
       0, 1, 0,
       doc: /* Whether there is an owner for the given X Selection.
The arg should be the name of the selection in question, typically one of
the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)
For convenience, the symbol nil is the same as `PRIMARY',
and t is the same as `SECONDARY'.  */)
     (selection)
     Lisp_Object selection;
{
  Window owner;
  Atom atom;
  Display *dpy;
  struct frame *sf = SELECTED_FRAME ();

  /* It should be safe to call this before we have an X frame.  */
  if (! FRAME_X_P (sf))
    return Qnil;

  dpy = FRAME_X_DISPLAY (sf);
  CHECK_SYMBOL (selection);
  if (!NILP (Fx_selection_owner_p (selection)))
    return Qt;
  if (EQ (selection, Qnil)) selection = QPRIMARY;
  if (EQ (selection, Qt)) selection = QSECONDARY;
  atom = symbol_to_x_atom (FRAME_X_DISPLAY_INFO (sf), dpy, selection);
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


#define CHECK_CUT_BUFFER(symbol)					\
  { CHECK_SYMBOL ((symbol));					\
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
       doc: /* Returns the value of the named cut buffer (typically CUT_BUFFER0).  */)
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
  struct frame *sf = SELECTED_FRAME ();

  check_x ();
  display = FRAME_X_DISPLAY (sf);
  dpyinfo = FRAME_X_DISPLAY_INFO (sf);
  window = RootWindow (display, 0); /* Cut buffers are on screen 0 */
  CHECK_CUT_BUFFER (buffer);
  buffer_atom = symbol_to_x_atom (dpyinfo, display, buffer);

  x_get_window_property (display, window, buffer_atom, &data, &bytes,
			 &type, &format, &size, 0);
  if (!data || !format)
    return Qnil;

  if (format != 8 || type != XA_STRING)
    Fsignal (Qerror,
	     Fcons (build_string ("cut buffer doesn't contain 8-bit data"),
		    Fcons (x_atom_to_symbol (display, type),
			   Fcons (make_number (format), Qnil))));

  ret = (bytes ? make_unibyte_string ((char *) data, bytes) : Qnil);
  /* Use xfree, not XFree, because x_get_window_property
     calls xmalloc itself.  */
  xfree (data);
  return ret;
}


DEFUN ("x-store-cut-buffer-internal", Fx_store_cut_buffer_internal,
       Sx_store_cut_buffer_internal, 2, 2, 0,
       doc: /* Sets the value of the named cut buffer (typically CUT_BUFFER0).  */)
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
  struct frame *sf = SELECTED_FRAME ();

  check_x ();
  display = FRAME_X_DISPLAY (sf);
  window = RootWindow (display, 0); /* Cut buffers are on screen 0 */

  max_bytes = SELECTION_QUANTUM (display);
  if (max_bytes > MAX_SELECTION_QUANTUM)
    max_bytes = MAX_SELECTION_QUANTUM;

  CHECK_CUT_BUFFER (buffer);
  CHECK_STRING (string);
  buffer_atom = symbol_to_x_atom (FRAME_X_DISPLAY_INFO (sf),
				  display, buffer);
  data = (unsigned char *) SDATA (string);
  bytes = SBYTES (string);
  bytes_remaining = bytes;

  if (! FRAME_X_DISPLAY_INFO (sf)->cut_buffers_initialized)
    {
      initialize_cut_buffers (display, window);
      FRAME_X_DISPLAY_INFO (sf)->cut_buffers_initialized = 1;
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
       doc: /* Rotate the values of the cut buffers by the given number of step.
Positive means shift the values forward, negative means backward.  */)
     (n)
     Lisp_Object n;
{
  Window window;
  Atom props[8];
  Display *display;
  struct frame *sf = SELECTED_FRAME ();

  check_x ();
  display = FRAME_X_DISPLAY (sf);
  window = RootWindow (display, 0); /* Cut buffers are on screen 0 */
  CHECK_NUMBER (n);
  if (XINT (n) == 0)
    return n;
  if (! FRAME_X_DISPLAY_INFO (sf)->cut_buffers_initialized)
    {
      initialize_cut_buffers (display, window);
      FRAME_X_DISPLAY_INFO (sf)->cut_buffers_initialized = 1;
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

/***********************************************************************
                      Drag and drop support
***********************************************************************/
/* Check that lisp values are of correct type for x_fill_property_data.
   That is, number, string or a cons with two numbers (low and high 16
   bit parts of a 32 bit number).  */

int
x_check_property_data (data)
     Lisp_Object data;
{
  Lisp_Object iter;
  int size = 0;

  for (iter = data; CONSP (iter) && size != -1; iter = XCDR (iter), ++size)
    {
      Lisp_Object o = XCAR (iter);

      if (! NUMBERP (o) && ! STRINGP (o) && ! CONSP (o))
        size = -1;
      else if (CONSP (o) &&
               (! NUMBERP (XCAR (o)) || ! NUMBERP (XCDR (o))))
        size = -1;
    }

  return size;
}

/* Convert lisp values to a C array.  Values may be a number, a string
   which is taken as an X atom name and converted to the atom value, or
   a cons containing the two 16 bit parts of a 32 bit number.

   DPY is the display use to look up X atoms.
   DATA is a Lisp list of values to be converted.
   RET is the C array that contains the converted values.  It is assumed
   it is big enough to hold all values.
   FORMAT is 8, 16 or 32 and denotes char/short/long for each C value to
   be stored in RET.  Note that long is used for 32 even if long is more
   than 32 bits (see man pages for XChangeProperty, XGetWindowProperty and
   XClientMessageEvent).  */

void
x_fill_property_data (dpy, data, ret, format)
     Display *dpy;
     Lisp_Object data;
     void *ret;
     int format;
{
  long val;
  long  *d32 = (long  *) ret;
  short *d16 = (short *) ret;
  char  *d08 = (char  *) ret;
  Lisp_Object iter;

  for (iter = data; CONSP (iter); iter = XCDR (iter))
    {
      Lisp_Object o = XCAR (iter);

      if (INTEGERP (o))
        val = (long) XFASTINT (o);
      else if (FLOATP (o))
        val = (long) XFLOAT_DATA (o);
      else if (CONSP (o))
        val = (long) cons_to_long (o);
      else if (STRINGP (o))
        {
          BLOCK_INPUT;
          val = (long) XInternAtom (dpy, (char *) SDATA (o), False);
          UNBLOCK_INPUT;
        }
      else
        error ("Wrong type, must be string, number or cons");

      if (format == 8)
        *d08++ = (char) val;
      else if (format == 16)
        *d16++ = (short) val;
      else
        *d32++ = val;
    }
}

/* Convert an array of C values to a Lisp list.
   F is the frame to be used to look up X atoms if the TYPE is XA_ATOM.
   DATA is a C array of values to be converted.
   TYPE is the type of the data.  Only XA_ATOM is special, it converts
   each number in DATA to its corresponfing X atom as a symbol.
   FORMAT is 8, 16 or 32 and gives the size in bits for each C value to
   be stored in RET.
   SIZE is the number of elements in DATA.

   Important: When format is 32, data should contain an array of int,
   not an array of long as the X library returns.  This makes a difference
   when sizeof(long) != sizeof(int).

   Also see comment for selection_data_to_lisp_data above.  */

Lisp_Object
x_property_data_to_lisp (f, data, type, format, size)
     struct frame *f;
     unsigned char *data;
     Atom type;
     int format;
     unsigned long size;
{
  return selection_data_to_lisp_data (FRAME_X_DISPLAY (f),
                                      data, size*format/8, type, format);
}

/* Get the mouse position in frame relative coordinates.  */

static void
mouse_position_for_drop (f, x, y)
     FRAME_PTR f;
     int *x;
     int *y;
{
  Window root, dummy_window;
  int dummy;

  BLOCK_INPUT;

  XQueryPointer (FRAME_X_DISPLAY (f),
                 DefaultRootWindow (FRAME_X_DISPLAY (f)),

                 /* The root window which contains the pointer.  */
                 &root,

                 /* Window pointer is on, not used  */
                 &dummy_window,

                 /* The position on that root window.  */
                 x, y,

                 /* x/y in dummy_window coordinates, not used.  */
                 &dummy, &dummy,

                 /* Modifier keys and pointer buttons, about which
                    we don't care.  */
                 (unsigned int *) &dummy);


  /* Absolute to relative.  */
  *x -= f->left_pos + FRAME_OUTER_TO_INNER_DIFF_X (f);
  *y -= f->top_pos + FRAME_OUTER_TO_INNER_DIFF_Y (f);

  UNBLOCK_INPUT;
}

DEFUN ("x-get-atom-name", Fx_get_atom_name,
       Sx_get_atom_name, 1, 2, 0,
       doc: /* Return the X atom name for VALUE as a string.
VALUE may be a number or a cons where the car is the upper 16 bits and
the cdr is the lower 16 bits of a 32 bit value.
Use the display for FRAME or the current frame if FRAME is not given or nil.

If the value is 0 or the atom is not known, return the empty string.  */)
  (value, frame)
     Lisp_Object value, frame;
{
  struct frame *f = check_x_frame (frame);
  char *name = 0;
  Lisp_Object ret = Qnil;
  int count;
  Display *dpy = FRAME_X_DISPLAY (f);
  Atom atom;

  if (INTEGERP (value))
    atom = (Atom) XUINT (value);
  else if (FLOATP (value))
    atom = (Atom) XFLOAT_DATA (value);
  else if (CONSP (value))
    atom = (Atom) cons_to_long (value);
  else
    error ("Wrong type, value must be number or cons");

  BLOCK_INPUT;
  count = x_catch_errors (dpy);

  name = atom ? XGetAtomName (dpy, atom) : "";

  if (! x_had_errors_p (dpy))
    ret = make_string (name, strlen (name));

  x_uncatch_errors (dpy, count);

  if (atom && name) XFree (name);
  if (NILP (ret)) ret = make_string ("", 0);

  UNBLOCK_INPUT;

  return ret;
}

/* Convert an XClientMessageEvent to a Lisp event of type DRAG_N_DROP_EVENT.
   TODO: Check if this client event really is a DND event?  */

int
x_handle_dnd_message (f, event, dpyinfo, bufp)
     struct frame *f;
     XClientMessageEvent *event;
     struct x_display_info *dpyinfo;
     struct input_event *bufp;
{
  Lisp_Object vec;
  Lisp_Object frame;
  /* format 32 => size 5, format 16 => size 10, format 8 => size 20 */
  unsigned long size = 160/event->format;
  int x, y;
  unsigned char *data = (unsigned char *) event->data.b;
  int idata[5];

  XSETFRAME (frame, f);

  /* On a 64 bit machine, the event->data.l array members are 64 bits (long),
     but the x_property_data_to_lisp (or rather selection_data_to_lisp_data)
     function expects them to be of size int (i.e. 32).  So to be able to
     use that function, put the data in the form it expects if format is 32. */

  if (event->format == 32 && event->format < BITS_PER_LONG)
    {
      int i;
      for (i = 0; i < 5; ++i) /* There are only 5 longs in a ClientMessage. */
        idata[i] = (int) event->data.l[i];
      data = (unsigned char *) idata;
    }

  vec = Fmake_vector (make_number (4), Qnil);
  AREF (vec, 0) = SYMBOL_NAME (x_atom_to_symbol (FRAME_X_DISPLAY (f),
                                                 event->message_type));
  AREF (vec, 1) = frame;
  AREF (vec, 2) = make_number (event->format);
  AREF (vec, 3) = x_property_data_to_lisp (f,
                                           data,
                                           event->message_type,
                                           event->format,
                                           size);

  mouse_position_for_drop (f, &x, &y);
  bufp->kind = DRAG_N_DROP_EVENT;
  bufp->frame_or_window = Fcons (frame, vec);
  bufp->timestamp = CurrentTime;
  bufp->x = make_number (x);
  bufp->y = make_number (y);
  bufp->arg = Qnil;
  bufp->modifiers = 0;

  return 1;
}

DEFUN ("x-send-client-message", Fx_send_client_event,
       Sx_send_client_message, 6, 6, 0,
       doc: /* Send a client message of MESSAGE-TYPE to window DEST on DISPLAY.

For DISPLAY, specify either a frame or a display name (a string).
If DISPLAY is nil, that stands for the selected frame's display.
DEST may be a number, in which case it is a Window id.  The value 0 may
be used to send to the root window of the DISPLAY.
If DEST is a cons, it is converted to a 32 bit number
with the high 16 bits from the car and the lower 16 bit from the cdr.  That
number is then used as a window id.
If DEST is a frame the event is sent to the outer window of that frame.
Nil means the currently selected frame.
If DEST is the string "PointerWindow" the event is sent to the window that
contains the pointer.  If DEST is the string "InputFocus" the event is
sent to the window that has the input focus.
FROM is the frame sending the event.  Use nil for currently selected frame.
MESSAGE-TYPE is the name of an Atom as a string.
FORMAT must be one of 8, 16 or 32 and determines the size of the values in
bits.  VALUES is a list of numbers, cons and/or strings containing the values
to send.  If a value is a string, it is converted to an Atom and the value of
the Atom is sent.  If a value is a cons, it is converted to a 32 bit number
with the high 16 bits from the car and the lower 16 bit from the cdr.
If more values than fits into the event is given, the excessive values
are ignored.  */)
     (display, dest, from, message_type, format, values)
     Lisp_Object display, dest, from, message_type, format, values;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);
  Window wdest;
  XEvent event;
  Lisp_Object cons;
  int size;
  struct frame *f = check_x_frame (from);
  int count;
  int to_root;

  CHECK_STRING (message_type);
  CHECK_NUMBER (format);
  CHECK_CONS (values);

  if (x_check_property_data (values) == -1)
    error ("Bad data in VALUES, must be number, cons or string");

  event.xclient.type = ClientMessage;
  event.xclient.format = XFASTINT (format);

  if (event.xclient.format != 8 && event.xclient.format != 16
      && event.xclient.format != 32)
    error ("FORMAT must be one of 8, 16 or 32");

  if (FRAMEP (dest) || NILP (dest))
    {
      struct frame *fdest = check_x_frame (dest);
      wdest = FRAME_OUTER_WINDOW (fdest);
    }
  else if (STRINGP (dest))
    {
      if (strcmp (SDATA (dest), "PointerWindow") == 0)
        wdest = PointerWindow;
      else if (strcmp (SDATA (dest), "InputFocus") == 0)
        wdest = InputFocus;
      else
        error ("DEST as a string must be one of PointerWindow or InputFocus");
    }
  else if (INTEGERP (dest))
    wdest = (Window) XFASTINT (dest);
  else if (FLOATP (dest))
    wdest =  (Window) XFLOAT_DATA (dest);
  else if (CONSP (dest))
    {
      if (! NUMBERP (XCAR (dest)) || ! NUMBERP (XCDR (dest)))
        error ("Both car and cdr for DEST must be numbers");
      else
        wdest = (Window) cons_to_long (dest);
    }
  else
    error ("DEST must be a frame, nil, string, number or cons");

  if (wdest == 0) wdest = dpyinfo->root_window;
  to_root = wdest == dpyinfo->root_window;

  for (cons = values, size = 0; CONSP (cons); cons = XCDR (cons), ++size)
    ;

  BLOCK_INPUT;

  event.xclient.message_type
    = XInternAtom (dpyinfo->display, SDATA (message_type), False);
  event.xclient.display = dpyinfo->display;

  /* Some clients (metacity for example) expects sending window to be here
     when sending to the root window.  */
  event.xclient.window = to_root ? FRAME_OUTER_WINDOW (f) : wdest;


  memset (event.xclient.data.b, 0, sizeof (event.xclient.data.b));
  x_fill_property_data (dpyinfo->display, values, event.xclient.data.b,
                        event.xclient.format);

  /* If event mask is 0 the event is sent to the client that created
     the destination window.  But if we are sending to the root window,
     there is no such client.  Then we set the event mask to 0xffff.  The
     event then goes to clients selecting for events on the root window.  */
  count = x_catch_errors (dpyinfo->display);
  {
    int propagate = to_root ? False : True;
    unsigned mask = to_root ? 0xffff : 0;
    XSendEvent (dpyinfo->display, wdest, propagate, mask, &event);
    XFlush (dpyinfo->display);
  }
  x_uncatch_errors (dpyinfo->display, count);
  UNBLOCK_INPUT;

  return Qnil;
}


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

  defsubr (&Sx_get_atom_name);
  defsubr (&Sx_send_client_message);

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
	       doc: /* An alist associating X Windows selection-types with functions.
These functions are called to convert the selection, with three args:
the name of the selection (typically `PRIMARY', `SECONDARY', or `CLIPBOARD');
a desired type to which the selection should be converted;
and the local selection value (whatever was given to `x-own-selection').

The function should return the value to send to the X server
\(typically a string).  A return value of nil
means that the conversion could not be done.
A return value which is the symbol `NULL'
means that a side-effect was executed,
and there is no meaningful selection value.  */);
  Vselection_converter_alist = Qnil;

  DEFVAR_LISP ("x-lost-selection-functions", &Vx_lost_selection_functions,
	       doc: /* A list of functions to be called when Emacs loses an X selection.
\(This happens when some other X client makes its own selection
or when a Lisp program explicitly clears the selection.)
The functions are called with one argument, the selection type
\(a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD').  */);
  Vx_lost_selection_functions = Qnil;

  DEFVAR_LISP ("x-sent-selection-functions", &Vx_sent_selection_functions,
	       doc: /* A list of functions to be called when Emacs answers a selection request.
The functions are called with four arguments:
  - the selection name (typically `PRIMARY', `SECONDARY', or `CLIPBOARD');
  - the selection-type which Emacs was asked to convert the
    selection into before sending (for example, `STRING' or `LENGTH');
  - a flag indicating success or failure for responding to the request.
We might have failed (and declined the request) for any number of reasons,
including being asked for a selection that we no longer own, or being asked
to convert into a type that we don't know about or that is inappropriate.
This hook doesn't let you change the behavior of Emacs's selection replies,
it merely informs you that they have happened.  */);
  Vx_sent_selection_functions = Qnil;

  DEFVAR_LISP ("selection-coding-system", &Vselection_coding_system,
	       doc: /* Coding system for communicating with other X clients.
When sending or receiving text via cut_buffer, selection, and clipboard,
the text is encoded or decoded by this coding system.
The default value is `compound-text-with-extensions'.  */);
  Vselection_coding_system = intern ("compound-text-with-extensions");

  DEFVAR_LISP ("next-selection-coding-system", &Vnext_selection_coding_system,
	       doc: /* Coding system for the next communication with other X clients.
Usually, `selection-coding-system' is used for communicating with
other X clients.  But, if this variable is set, it is used for the
next communication only.  After the communication, this variable is
set to nil.  */);
  Vnext_selection_coding_system = Qnil;

  DEFVAR_INT ("x-selection-timeout", &x_selection_timeout,
	      doc: /* Number of milliseconds to wait for a selection reply.
If the selection owner doesn't reply in this time, we give up.
A value of 0 means wait as long as necessary.  This is initialized from the
\"*selectionTimeout\" resource.  */);
  x_selection_timeout = 0;

  QPRIMARY   = intern ("PRIMARY");	staticpro (&QPRIMARY);
  QSECONDARY = intern ("SECONDARY");	staticpro (&QSECONDARY);
  QSTRING    = intern ("STRING");	staticpro (&QSTRING);
  QINTEGER   = intern ("INTEGER");	staticpro (&QINTEGER);
  QCLIPBOARD = intern ("CLIPBOARD");	staticpro (&QCLIPBOARD);
  QTIMESTAMP = intern ("TIMESTAMP");	staticpro (&QTIMESTAMP);
  QTEXT      = intern ("TEXT"); 	staticpro (&QTEXT);
  QCOMPOUND_TEXT = intern ("COMPOUND_TEXT"); staticpro (&QCOMPOUND_TEXT);
  QUTF8_STRING = intern ("UTF8_STRING"); staticpro (&QUTF8_STRING);
  QTIMESTAMP = intern ("TIMESTAMP");	staticpro (&QTIMESTAMP);
  QDELETE    = intern ("DELETE");	staticpro (&QDELETE);
  QMULTIPLE  = intern ("MULTIPLE");	staticpro (&QMULTIPLE);
  QINCR      = intern ("INCR");		staticpro (&QINCR);
  QEMACS_TMP = intern ("_EMACS_TMP_");	staticpro (&QEMACS_TMP);
  QTARGETS   = intern ("TARGETS");	staticpro (&QTARGETS);
  QATOM	     = intern ("ATOM");		staticpro (&QATOM);
  QATOM_PAIR = intern ("ATOM_PAIR");	staticpro (&QATOM_PAIR);
  QNULL	     = intern ("NULL");		staticpro (&QNULL);
  Qcompound_text_with_extensions = intern ("compound-text-with-extensions");
  staticpro (&Qcompound_text_with_extensions);

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

  Qforeign_selection = intern ("foreign-selection");
  staticpro (&Qforeign_selection);
}

/* arch-tag: 7c293b0f-9918-4f69-8ac7-03e142307236
   (do not change this comment) */
