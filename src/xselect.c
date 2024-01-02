/* X Selection processing for Emacs.
   Copyright (C) 1993-1997, 2000-2024 Free Software Foundation, Inc.

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


/* Rewritten by jwz */

#include <config.h>
#include <limits.h>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include <unistd.h>

#include "lisp.h"
#include "xterm.h"	/* for all of the X includes */
#include "frame.h"	/* Need this to get the X window of selected_frame */
#include "blockinput.h"
#include "sysstdio.h"	/* TRACE_SELECTION needs this.  */
#include "termhooks.h"
#include "keyboard.h"
#include "pdumper.h"
#include "atimer.h"

#include <X11/Xproto.h>

struct prop_location;
struct selection_data;

static void x_decline_selection_request (struct selection_input_event *);
static bool x_convert_selection (Lisp_Object, Lisp_Object, Atom, bool,
				 struct x_display_info *, bool);
static bool waiting_for_other_props_on_window (Display *, Window);
static struct prop_location *expect_property_change (Display *, Window,
                                                     Atom, int);
static void unexpect_property_change (struct prop_location *);
static void wait_for_property_change (struct prop_location *);
static Lisp_Object x_get_window_property_as_lisp_data (struct x_display_info *,
                                                       Window, Atom,
                                                       Lisp_Object, Atom, bool);
static Lisp_Object selection_data_to_lisp_data (struct x_display_info *,
						const unsigned char *,
						ptrdiff_t, Atom, int);
static void lisp_data_to_selection_data (struct x_display_info *, Lisp_Object,
					 struct selection_data *);
static void x_send_client_event (Lisp_Object, Lisp_Object, Lisp_Object,
				 Atom, Lisp_Object, Lisp_Object);

/* Printing traces to stderr.  */

#ifdef TRACE_SELECTION
#define TRACE0(fmt) \
  fprintf (stderr, "%"PRIdMAX": " fmt "\n", (intmax_t) getpid ())
#define TRACE1(fmt, a0) \
  fprintf (stderr, "%"PRIdMAX": " fmt "\n", (intmax_t) getpid (), a0)
#define TRACE2(fmt, a0, a1) \
  fprintf (stderr, "%"PRIdMAX": " fmt "\n", (intmax_t) getpid (), a0, a1)
#define TRACE3(fmt, a0, a1, a2) \
  fprintf (stderr, "%"PRIdMAX": " fmt "\n", (intmax_t) getpid (), a0, a1, a2)
#else
#define TRACE0(fmt)		(void) 0
#define TRACE1(fmt, a0)		(void) 0
#define TRACE2(fmt, a0, a1)	(void) 0
#endif

/* Bytes needed to represent 'long' data.  This is as per libX11; it
   is not necessarily sizeof (long).  */
#define X_LONG_SIZE 4

/* If this is a smaller number than the max-request-size of the display,
   emacs will use INCR selection transfer when the selection is larger
   than this.  The max-request-size is usually around 64k, so if you want
   emacs to use incremental selection transfers when the selection is
   smaller than that, set this.  I added this mostly for debugging the
   incremental transfer stuff, but it might improve server performance.

   This value cannot exceed INT_MAX / max (X_LONG_SIZE, sizeof (long))
   because it is multiplied by X_LONG_SIZE and by sizeof (long) in
   subscript calculations.  Similarly for PTRDIFF_MAX - 1 or SIZE_MAX
   - 1 in place of INT_MAX.  */
#define MAX_SELECTION_QUANTUM						\
  ((int) min (0xFFFFFF, (min (INT_MAX, min (PTRDIFF_MAX, SIZE_MAX) - 1)	\
			 / max (X_LONG_SIZE, sizeof (long)))))

static int
selection_quantum (Display *display)
{
  long mrs = XExtendedMaxRequestSize (display);

  if (!mrs)
    mrs = XMaxRequestSize (display);

  return (mrs < MAX_SELECTION_QUANTUM / X_LONG_SIZE + 25
	  ? (mrs - 25) * X_LONG_SIZE
	  : MAX_SELECTION_QUANTUM);
}

#define LOCAL_SELECTION(selection_symbol, dpyinfo)			\
  assq_no_quit (selection_symbol, dpyinfo->terminal->Vselection_alist)



/* This converts a Lisp symbol to a server Atom, avoiding a server
   roundtrip whenever possible.  */

Atom
symbol_to_x_atom (struct x_display_info *dpyinfo, Lisp_Object sym)
{
  Atom val;
  if (NILP (sym))
    return 0;
  if (EQ (sym, QPRIMARY))
    return XA_PRIMARY;
  if (EQ (sym, QSECONDARY))
    return XA_SECONDARY;
  if (EQ (sym, QSTRING))
    return XA_STRING;
  if (EQ (sym, QINTEGER))
    return XA_INTEGER;
  if (EQ (sym, QATOM))
    return XA_ATOM;
  if (EQ (sym, QCLIPBOARD))
    return dpyinfo->Xatom_CLIPBOARD;
  if (EQ (sym, QTIMESTAMP))
    return dpyinfo->Xatom_TIMESTAMP;
  if (EQ (sym, QTEXT))
    return dpyinfo->Xatom_TEXT;
  if (EQ (sym, QCOMPOUND_TEXT))
    return dpyinfo->Xatom_COMPOUND_TEXT;
  if (EQ (sym, QUTF8_STRING))
    return dpyinfo->Xatom_UTF8_STRING;
  if (EQ (sym, QDELETE))
    return dpyinfo->Xatom_DELETE;
  if (EQ (sym, QMULTIPLE))
    return dpyinfo->Xatom_MULTIPLE;
  if (EQ (sym, QINCR))
    return dpyinfo->Xatom_INCR;
  if (EQ (sym, Q_EMACS_TMP_))
    return dpyinfo->Xatom_EMACS_TMP;
  if (EQ (sym, QTARGETS))
    return dpyinfo->Xatom_TARGETS;
  if (EQ (sym, QNULL))
    return dpyinfo->Xatom_NULL;
  if (EQ (sym, QXdndSelection))
    return dpyinfo->Xatom_XdndSelection;
  if (EQ (sym, QXmTRANSFER_SUCCESS))
    return dpyinfo->Xatom_XmTRANSFER_SUCCESS;
  if (EQ (sym, QXmTRANSFER_FAILURE))
    return dpyinfo->Xatom_XmTRANSFER_FAILURE;
  if (EQ (sym, QXdndDirectSave0))
    return dpyinfo->Xatom_XdndDirectSave0;
  if (EQ (sym, Qtext_plain))
    return dpyinfo->Xatom_text_plain;
  if (EQ (sym, QXdndActionDirectSave))
    return dpyinfo->Xatom_XdndActionDirectSave;

  if (!SYMBOLP (sym))
    emacs_abort ();

  TRACE1 (" XInternAtom %s", SSDATA (SYMBOL_NAME (sym)));
  block_input ();
  val = x_intern_cached_atom (dpyinfo, SSDATA (SYMBOL_NAME (sym)), false);
  unblock_input ();
  return val;
}


/* This converts a server Atom to a Lisp symbol, avoiding server roundtrips
   and calls to intern whenever possible.  */

Lisp_Object
x_atom_to_symbol (struct x_display_info *dpyinfo, Atom atom)
{
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
    }

  if (dpyinfo == NULL)
    return Qnil;
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
    return Q_EMACS_TMP_;
  if (atom == dpyinfo->Xatom_TARGETS)
    return QTARGETS;
  if (atom == dpyinfo->Xatom_NULL)
    return QNULL;
  if (atom == dpyinfo->Xatom_XdndSelection)
    return QXdndSelection;
  if (atom == dpyinfo->Xatom_XmTRANSFER_SUCCESS)
    return QXmTRANSFER_SUCCESS;
  if (atom == dpyinfo->Xatom_XmTRANSFER_FAILURE)
    return QXmTRANSFER_FAILURE;
  if (atom == dpyinfo->Xatom_XdndDirectSave0)
    return QXdndDirectSave0;
  if (atom == dpyinfo->Xatom_text_plain)
    return Qtext_plain;
  if (atom == dpyinfo->Xatom_XdndActionDirectSave)
    return QXdndActionDirectSave;

  x_catch_errors (dpyinfo->display);
  str = x_get_atom_name (dpyinfo, atom, NULL);
  x_uncatch_errors ();

  TRACE0 ("XGetAtomName --> NULL");
  if (!str)
    return Qnil;
  TRACE1 ("XGetAtomName --> %s", str);

  val = intern (str);
  xfree (str);
  return val;
}

/* Do protocol to assert ourself as a selection owner.
   FRAME shall be the owner; it must be a valid X frame.
   TIMESTAMP should be the timestamp where selection ownership will be
   assumed.
   DND_DATA is the local value that will be used for selection requests
   with `dpyinfo->pending_dnd_time'.
   Update the Vselection_alist so that we can reply to later requests for
   our selection.  */

void
x_own_selection (Lisp_Object selection_name, Lisp_Object selection_value,
		 Lisp_Object frame, Lisp_Object dnd_data, Time timestamp)
{
  struct frame *f = XFRAME (frame);
  Window selecting_window = FRAME_X_WINDOW (f);
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  Display *display = dpyinfo->display;
  Atom selection_atom = symbol_to_x_atom (dpyinfo, selection_name);

  if (!timestamp)
    timestamp = dpyinfo->last_user_time;

  block_input ();
  x_catch_errors (display);
  XSetSelectionOwner (display, selection_atom, selecting_window, timestamp);
  x_check_errors (display, "Can't set selection: %s");
  x_uncatch_errors_after_check ();
  unblock_input ();

  /* Now update the local cache */
  {
    Lisp_Object selection_data;
    Lisp_Object prev_value;

    selection_data = list5 (selection_name, selection_value,
			    INT_TO_INTEGER (timestamp), frame,
			    dnd_data);
    prev_value = LOCAL_SELECTION (selection_name, dpyinfo);

    tset_selection_alist
      (dpyinfo->terminal,
       Fcons (selection_data, dpyinfo->terminal->Vselection_alist));

    /* If we already owned the selection, remove the old selection
       data.  Don't use Fdelq as that may quit.  */
    if (!NILP (prev_value))
      {
	/* We know it's not the CAR, so it's easy.  */
	Lisp_Object rest = dpyinfo->terminal->Vselection_alist;
	for (; CONSP (rest); rest = XCDR (rest))
	  if (EQ (prev_value, CAR (XCDR (rest))))
	    {
	      XSETCDR (rest, XCDR (XCDR (rest)));
	      break;
	    }
      }
  }
}

/* Given a selection-name and desired type, look up our local copy of
   the selection value and convert it to the type.
   Return nil, a string, a vector, a symbol, an integer, or a cons
   that CONS_TO_INTEGER could plausibly handle.
   This function is used both for remote requests (LOCAL_REQUEST is zero)
   and for local x-get-selection-internal (LOCAL_REQUEST is nonzero).

   If LOCAL_VALUE is non-nil, use it as the local copy.  Also allow
   quitting in that case, and let DPYINFO be NULL.

   If NEED_ALTERNATE is true, use the drag-and-drop local value
   instead.

   This calls random Lisp code, and may signal or gc.  */

static Lisp_Object
x_get_local_selection (Lisp_Object selection_symbol, Lisp_Object target_type,
		       bool local_request, struct x_display_info *dpyinfo,
		       Lisp_Object local_value, bool need_alternate)
{
  Lisp_Object tem;
  Lisp_Object handler_fn, value, check;
  bool may_quit;
  specpdl_ref count;

  may_quit = false;

  if (NILP (local_value))
    local_value = LOCAL_SELECTION (selection_symbol, dpyinfo);
  else
    may_quit = true;

  if (NILP (local_value))
    return Qnil;

  /* TIMESTAMP is a special case.  */
  if (EQ (target_type, QTIMESTAMP))
    {
      handler_fn = Qnil;
      value = XCAR (XCDR (XCDR (local_value)));
    }
  else
    {
      /* Don't allow a quit within the converter.
	 When the user types C-g, he would be surprised
	 if by luck it came during a converter.  */
      count = SPECPDL_INDEX ();

      if (!may_quit)
	specbind (Qinhibit_quit, Qt);

      CHECK_SYMBOL (target_type);
      handler_fn = CDR (Fassq (target_type, Vselection_converter_alist));

      if (CONSP (handler_fn))
	handler_fn = XCDR (handler_fn);

      if (!need_alternate)
	tem = XCAR (XCDR (local_value));
      else
	tem = XCAR (XCDR (XCDR (XCDR (XCDR (local_value)))));

      if (STRINGP (tem))
	{
	  local_value = Fget_text_property (make_fixnum (0),
					    target_type, tem);

	  if (!NILP (local_value))
	    tem = local_value;
	}

      if (!NILP (handler_fn))
	value = call3 (handler_fn, selection_symbol,
		       ((local_request
			 && NILP (Vx_treat_local_requests_remotely))
			? Qnil
			: target_type),
		       tem);
      else
	value = Qnil;
      value = unbind_to (count, value);
    }

  /* Make sure this value is of a type that we could transmit
     to another X client.  */

  check = value;
  if (CONSP (value)
      && SYMBOLP (XCAR (value)))
    check = XCDR (value);

  if (STRINGP (check)
      || VECTORP (check)
      || SYMBOLP (check)
      || INTEGERP (check)
      || NILP (value))
    return value;
  /* Check for a value that CONS_TO_INTEGER could handle.  */
  else if (CONSP (check)
	   && INTEGERP (XCAR (check))
	   && (INTEGERP (XCDR (check))
	       ||
	       (CONSP (XCDR (check))
		&& INTEGERP (XCAR (XCDR (check)))
		&& NILP (XCDR (XCDR (check))))))
    return value;

  signal_error ("Invalid data returned by selection-conversion function",
		list2 (handler_fn, value));
}

/* Subroutines of x_reply_selection_request.  */

/* Send a SelectionNotify event to the requestor with property=None,
   meaning we were unable to do what they wanted.  */

static void
x_decline_selection_request (struct selection_input_event *event)
{
  XEvent reply_base;
  XSelectionEvent *reply;
  Display *dpy;
  struct x_display_info *dpyinfo;

  reply = &(reply_base.xselection);
  dpy = SELECTION_EVENT_DISPLAY (event);
  dpyinfo = x_display_info_for_display (dpy);

  if (!dpyinfo)
    return;

  reply->type = SelectionNotify;
  reply->display = dpy;
  reply->requestor = SELECTION_EVENT_REQUESTOR (event);
  reply->selection = SELECTION_EVENT_SELECTION (event);
  reply->time = SELECTION_EVENT_TIME (event);
  reply->target = SELECTION_EVENT_TARGET (event);
  reply->property = None;

  /* The reason for the error may be that the receiver has
     died in the meantime.  Handle that case.  */
  block_input ();
  x_ignore_errors_for_next_request (dpyinfo);
  XSendEvent (dpyinfo->display, reply->requestor,
	      False, 0, &reply_base);
  x_stop_ignoring_errors (dpyinfo);

  XFlush (dpyinfo->display);
  unblock_input ();
}

/* Raw selection data, for sending to a requestor window.  */

struct selection_data
{
  unsigned char *data;
  ptrdiff_t size;
  int format;
  Atom type;
  bool nofree;
  Atom property;
  /* This can be set to non-NULL during x_reply_selection_request, if
     the selection is waiting for an INCR transfer to complete.  Don't
     free these; that's done by unexpect_property_change.  */
  struct prop_location *wait_object;
  struct selection_data *next;
};

struct x_selection_request
{
  /* The last element in this stack.  */
  struct x_selection_request *last;

  /* Its display info.  */
  struct x_display_info *dpyinfo;

  /* Its selection input event.  */
  struct selection_input_event *request;

  /* Linked list of the above (in support of MULTIPLE targets).  */
  struct selection_data *converted_selections;

  /* "Data" to send a requestor for a failed MULTIPLE subtarget.  */
  Atom conversion_fail_tag;

  /* Whether or not conversion was successful.  */
  bool converted;
};

/* Stack of selections currently being processed.
   NULL if all requests have been fully processed.  */

struct x_selection_request *selection_request_stack;

static void
x_push_current_selection_request (struct selection_input_event *se,
				  struct x_display_info *dpyinfo)
{
  struct x_selection_request *frame;

  frame = xmalloc (sizeof *frame);
  frame->converted = false;
  frame->last = selection_request_stack;
  frame->request = se;
  frame->dpyinfo = dpyinfo;
  frame->converted_selections = NULL;
  frame->conversion_fail_tag = None;

  selection_request_stack = frame;
}

static void
x_pop_current_selection_request (void)
{
  struct x_selection_request *tem;

  tem = selection_request_stack;
  selection_request_stack = selection_request_stack->last;

  xfree (tem);
}

/* Used as an unwind-protect clause so that, if a selection-converter signals
   an error, we tell the requestor that we were unable to do what they wanted
   before we throw to top-level or go into the debugger or whatever.  */

static void
x_selection_request_lisp_error (void)
{
  struct selection_data *cs, *next;
  struct x_selection_request *frame;

  frame = selection_request_stack;

  for (cs = frame->converted_selections; cs; cs = next)
    {
      next = cs->next;
      if (! cs->nofree && cs->data)
	xfree (cs->data);
      xfree (cs);
    }
  frame->converted_selections = NULL;

  if (!frame->converted && frame->dpyinfo->display)
    x_decline_selection_request (frame->request);
}

static void
x_catch_errors_unwind (void)
{
  block_input ();
  x_uncatch_errors ();
  unblock_input ();
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
  bool arrived;
  struct prop_location *next;
};

static int prop_location_identifier;

static Lisp_Object property_change_reply;

static struct prop_location *property_change_reply_object;

static struct prop_location *property_change_wait_list;

static void
set_property_change_object (struct prop_location *location)
{
  /* Input must be blocked so we don't get the event before we set these.  */
  if (! input_blocked_p ())
    emacs_abort ();
  XSETCAR (property_change_reply, Qnil);
  property_change_reply_object = location;
}


/* Send the reply to a selection request event EVENT.  */

#ifdef TRACE_SELECTION
static int x_reply_selection_request_cnt;
#endif  /* TRACE_SELECTION */

static void
x_reply_selection_request (struct selection_input_event *event,
                           struct x_display_info *dpyinfo)
{
  XEvent reply_base;
  XSelectionEvent *reply = &(reply_base.xselection);
  Display *display = SELECTION_EVENT_DISPLAY (event);
  Window window = SELECTION_EVENT_REQUESTOR (event);
  ptrdiff_t bytes_remaining;
  int max_bytes = selection_quantum (display);
  specpdl_ref count = SPECPDL_INDEX ();
  struct selection_data *cs;
  struct x_selection_request *frame;

  frame = selection_request_stack;

  reply->type = SelectionNotify;
  reply->display = display;
  reply->requestor = window;
  reply->selection = SELECTION_EVENT_SELECTION (event);
  reply->time = SELECTION_EVENT_TIME (event);
  reply->target = SELECTION_EVENT_TARGET (event);
  reply->property = SELECTION_EVENT_PROPERTY (event);
  if (reply->property == None)
    reply->property = reply->target;

  block_input ();
  /* The protected block contains wait_for_property_change, which can
     run random lisp code (process handlers) or signal.  Therefore, we
     put the x_uncatch_errors call in an unwind.  */
  record_unwind_protect_void (x_catch_errors_unwind);
  x_catch_errors (display);

  /* Loop over converted selections, storing them in the requested
     properties.  If data is large, only store the first N bytes
     (section 2.7.2 of ICCCM).  Note that we store the data for a
     MULTIPLE request in the opposite order; the ICCM says only that
     the conversion itself must be done in the same order. */
  for (cs = frame->converted_selections; cs; cs = cs->next)
    {
      if (cs->property == None)
	continue;

      bytes_remaining = cs->size;
      bytes_remaining *= cs->format >> 3;
      if (bytes_remaining <= max_bytes)
	{
	  /* Send all the data at once, with minimal handshaking.  */
	  TRACE1 ("Sending all %"pD"d bytes", bytes_remaining);
	  XChangeProperty (display, window, cs->property,
			   cs->type, cs->format, PropModeReplace,
			   cs->data, cs->size);
	}
      else
	{
	  /* Send an INCR tag to initiate incremental transfer.  */
	  long value[1];

	  TRACE2 ("Start sending %"pD"d bytes incrementally (%s)",
		  bytes_remaining, XGetAtomName (display, cs->property));
	  cs->wait_object
	    = expect_property_change (display, window, cs->property,
				      PropertyDelete);

	  /* XChangeProperty expects an array of long even if long is
	     more than 32 bits.  */
	  value[0] = min (bytes_remaining, X_LONG_MAX);
	  XChangeProperty (display, window, cs->property,
			   dpyinfo->Xatom_INCR, 32, PropModeReplace,
			   (unsigned char *) value, 1);
	  XSelectInput (display, window, PropertyChangeMask);
	}
    }

  /* Now issue the SelectionNotify event.  */
  XSendEvent (display, window, False, 0, &reply_base);
  XFlush (display);

#ifdef TRACE_SELECTION
  {
    char *sel = XGetAtomName (display, reply->selection);
    char *tgt = XGetAtomName (display, reply->target);
    TRACE3 ("Sent SelectionNotify: %s, target %s (%d)",
	    sel, tgt, ++x_reply_selection_request_cnt);
    if (sel) XFree (sel);
    if (tgt) XFree (tgt);
  }
#endif /* TRACE_SELECTION */

  /* Finish sending the rest of each of the INCR values.  This should
     be improved; there's a chance of deadlock if more than one
     subtarget in a MULTIPLE selection requires an INCR transfer, and
     the requestor and Emacs loop waiting on different transfers.  */
  for (cs = frame->converted_selections; cs; cs = cs->next)
    if (cs->wait_object)
      {
	int format_bytes = cs->format / 8;
	bool had_errors_p = x_had_errors_p (display);

        /* Must set this inside block_input ().  unblock_input may read
           events and setting property_change_reply in
           wait_for_property_change is then too late.  */
        set_property_change_object (cs->wait_object);
	unblock_input ();

	bytes_remaining = cs->size;
	bytes_remaining *= format_bytes;

	/* Wait for the requestor to ack by deleting the property.
	   This can run Lisp code (process handlers) or signal.  */
	if (! had_errors_p)
	  {
	    TRACE1 ("Waiting for ACK (deletion of %s)",
		    XGetAtomName (display, cs->property));
	    wait_for_property_change (cs->wait_object);
	  }
	else
	  unexpect_property_change (cs->wait_object);

	while (bytes_remaining)
	  {
	    int i = ((bytes_remaining < max_bytes)
		     ? bytes_remaining
		     : max_bytes) / format_bytes;
	    block_input ();

	    cs->wait_object
	      = expect_property_change (display, window, cs->property,
					PropertyDelete);

	    TRACE1 ("Sending increment of %d elements", i);
	    TRACE1 ("Set %s to increment data",
		    XGetAtomName (display, cs->property));

	    /* Append the next chunk of data to the property.  */
	    XChangeProperty (display, window, cs->property,
			     cs->type, cs->format, PropModeAppend,
			     cs->data, i);
	    bytes_remaining -= i * format_bytes;
	    cs->data += i * ((cs->format == 32) ? sizeof (long)
			     : format_bytes);
	    XFlush (display);
	    had_errors_p = x_had_errors_p (display);
            /* See comment above about property_change_reply.  */
            set_property_change_object (cs->wait_object);
	    unblock_input ();

	    if (had_errors_p) break;

	    /* Wait for the requestor to ack this chunk by deleting
	       the property.  This can run Lisp code or signal.  */
	    TRACE1 ("Waiting for increment ACK (deletion of %s)",
		    XGetAtomName (display, cs->property));
	    wait_for_property_change (cs->wait_object);
	  }

	/* Now write a zero-length chunk to the property to tell the
	   requestor that we're done.  */
	block_input ();
	if (! waiting_for_other_props_on_window (display, window))
	  XSelectInput (display, window, 0);

	TRACE1 ("Set %s to a 0-length chunk to indicate EOF",
		XGetAtomName (display, cs->property));
	XChangeProperty (display, window, cs->property,
			 cs->type, cs->format, PropModeReplace,
			 cs->data, 0);
	TRACE0 ("Done sending incrementally");
      }

  /* rms, 2003-01-03: I think I have fixed this bug.  */
  /* The window we're communicating with may have been deleted
     in the meantime (that's a real situation from a bug report).
     In this case, there may be events in the event queue still
     referring to the deleted window, and we'll get a BadWindow error
     in XTread_socket when processing the events.  I don't have
     an idea how to fix that.  gerd, 2001-01-98.   */
  /* 2004-09-10: XSync and UNBLOCK so that possible protocol errors are
     delivered before uncatch errors.  */
  XSync (display, False);
  unblock_input ();

  /* GTK queues events in addition to the queue in Xlib.  So we
     UNBLOCK to enter the event loop and get possible errors delivered,
     and then BLOCK again because x_uncatch_errors requires it.  */
  block_input ();
  /* This calls x_uncatch_errors.  */
  unbind_to (count, Qnil);
  unblock_input ();
}

/* Handle a SelectionRequest event EVENT.
   This is called from keyboard.c when such an event is found in the queue.  */

static void
x_handle_selection_request (struct selection_input_event *event)
{
  Time local_selection_time;
  struct x_display_info *dpyinfo = SELECTION_EVENT_DPYINFO (event);
  Atom selection = SELECTION_EVENT_SELECTION (event);
  Lisp_Object selection_symbol = x_atom_to_symbol (dpyinfo, selection);
  Atom target = SELECTION_EVENT_TARGET (event);
  Lisp_Object target_symbol = x_atom_to_symbol (dpyinfo, target);
  Atom property = SELECTION_EVENT_PROPERTY (event);
  Lisp_Object local_selection_data;
  bool success = false;
  specpdl_ref count = SPECPDL_INDEX ();
  bool pushed, use_alternate;
  Lisp_Object alias, tem;

  alias = Vx_selection_alias_alist;

  FOR_EACH_TAIL_SAFE (alias)
    {
      tem = Qnil;

      if (CONSP (alias))
	tem = XCAR (alias);

      if (CONSP (tem)
	  && EQ (XCAR (tem), selection_symbol)
	  && SYMBOLP (XCDR (tem)))
	{
	  selection_symbol = XCDR (tem);
	  break;
	}
    }

  pushed = false;

  if (!dpyinfo)
    goto REALLY_DONE;

  local_selection_data = LOCAL_SELECTION (selection_symbol, dpyinfo);

  /* Decline if we don't own any selections.  */
  if (NILP (local_selection_data)) goto DONE;

  /* Decline requests issued prior to our acquiring the selection.  */
  CONS_TO_INTEGER (XCAR (XCDR (XCDR (local_selection_data))),
		   Time, local_selection_time);
  if (SELECTION_EVENT_TIME (event) != CurrentTime
      && local_selection_time > SELECTION_EVENT_TIME (event))
    goto DONE;

  use_alternate = false;

  /* This is how the XDND protocol recommends dropping text onto a
     target that doesn't support XDND.  */
  if (dpyinfo->pending_dnd_time
      && ((SELECTION_EVENT_TIME (event)
	   == dpyinfo->pending_dnd_time + 1)
	  || (SELECTION_EVENT_TIME (event)
	      == dpyinfo->pending_dnd_time + 2)))
    use_alternate = true;

  block_input ();
  pushed = true;
  x_push_current_selection_request (event, dpyinfo);
  record_unwind_protect_void (x_pop_current_selection_request);
  record_unwind_protect_void (x_selection_request_lisp_error);
  unblock_input ();

  TRACE2 ("x_handle_selection_request: selection=%s, target=%s",
	  SDATA (SYMBOL_NAME (selection_symbol)),
	  SDATA (SYMBOL_NAME (target_symbol)));

  if (EQ (target_symbol, QMULTIPLE))
    {
      /* For MULTIPLE targets, the event property names a list of atom
	 pairs; the first atom names a target and the second names a
	 non-None property.  */
      Window requestor = SELECTION_EVENT_REQUESTOR (event);
      Lisp_Object multprop;
      ptrdiff_t j, nselections;
      struct selection_data cs;

      if (property == None) goto DONE;
      multprop
	= x_get_window_property_as_lisp_data (dpyinfo, requestor, property,
					      QMULTIPLE, selection, true);

      if (!VECTORP (multprop) || ASIZE (multprop) % 2)
	goto DONE;

      nselections = ASIZE (multprop) / 2;
      /* Perform conversions.  This can signal.  */
      for (j = 0; j < nselections; j++)
	{
	  Lisp_Object subtarget = AREF (multprop, 2*j);
	  Atom subproperty = symbol_to_x_atom (dpyinfo,
					       AREF (multprop, 2*j+1));
	  bool subsuccess = false;

	  if (subproperty != None)
	    subsuccess = x_convert_selection (selection_symbol, subtarget,
					      subproperty, true, dpyinfo,
					      use_alternate);
	  if (!subsuccess)
	    ASET (multprop, 2*j+1, Qnil);
	}
      /* Save conversion results */
      lisp_data_to_selection_data (dpyinfo, multprop, &cs);

      /* If cs.type is ATOM, change it to ATOM_PAIR.  This is because
	 the parameters to a MULTIPLE are ATOM_PAIRs.  */

      if (cs.type == XA_ATOM)
	cs.type = dpyinfo->Xatom_ATOM_PAIR;

      XChangeProperty (dpyinfo->display, requestor, property,
		       cs.type, cs.format, PropModeReplace,
		       cs.data, cs.size);
      success = true;
    }
  else
    {
      if (property == None)
	property = SELECTION_EVENT_TARGET (event);
      success = x_convert_selection (selection_symbol,
				     target_symbol, property,
				     false, dpyinfo,
				     use_alternate);
    }

 DONE:

  if (pushed)
    selection_request_stack->converted = true;

  if (success)
    x_reply_selection_request (event, dpyinfo);
  else
    x_decline_selection_request (event);

  /* Run the `x-sent-selection-functions' abnormal hook.  */
  if (!NILP (Vx_sent_selection_functions)
      && !BASE_EQ (Vx_sent_selection_functions, Qunbound))
    CALLN (Frun_hook_with_args, Qx_sent_selection_functions,
	   selection_symbol, target_symbol, success ? Qt : Qnil);

  /* Used to punt when dpyinfo is NULL.  */
 REALLY_DONE:

  unbind_to (count, Qnil);
}

/* Perform the requested selection conversion, and write the data to
   the converted_selections linked list, where it can be accessed by
   x_reply_selection_request.  If FOR_MULTIPLE, write out
   the data even if conversion fails, using conversion_fail_tag.

   Return true if successful.  */

static bool
x_convert_selection (Lisp_Object selection_symbol,
		     Lisp_Object target_symbol, Atom property,
		     bool for_multiple, struct x_display_info *dpyinfo,
		     bool use_alternate)
{
  Lisp_Object lisp_selection;
  struct selection_data *cs;
  struct x_selection_request *frame;

  lisp_selection
    = x_get_local_selection (selection_symbol, target_symbol,
			     false, dpyinfo, Qnil, use_alternate);

  frame = selection_request_stack;

  /* A nil return value means we can't perform the conversion.  */
  if (NILP (lisp_selection)
      || (CONSP (lisp_selection) && NILP (XCDR (lisp_selection))))
    {
      if (for_multiple)
	{
	  cs = xmalloc (sizeof *cs);
	  cs->data = ((unsigned char *)
		      &selection_request_stack->conversion_fail_tag);
	  cs->size = 1;
	  cs->format = 32;
	  cs->type = XA_ATOM;
	  cs->nofree = true;
	  cs->property = property;
	  cs->wait_object = NULL;
	  cs->next = frame->converted_selections;
	  frame->converted_selections = cs;
	}

      return false;
    }

  /* Otherwise, record the converted selection to binary.  */
  cs = xmalloc (sizeof *cs);
  cs->data = NULL;
  cs->nofree = true;
  cs->property = property;
  cs->wait_object = NULL;
  cs->next = frame->converted_selections;
  frame->converted_selections = cs;
  lisp_data_to_selection_data (dpyinfo, lisp_selection, cs);
  return true;
}

/* Handle a SelectionClear event EVENT, which indicates that some
   client cleared out our previously asserted selection.
   This is called from keyboard.c when such an event is found in the queue.  */

static void
x_handle_selection_clear (struct selection_input_event *event)
{
  Atom selection = SELECTION_EVENT_SELECTION (event);
  Time changed_owner_time = SELECTION_EVENT_TIME (event);

  Lisp_Object selection_symbol, local_selection_data;
  Time local_selection_time;
  struct x_display_info *dpyinfo = SELECTION_EVENT_DPYINFO (event);
  Lisp_Object Vselection_alist;

  TRACE0 ("x_handle_selection_clear");

  if (!dpyinfo) return;

  selection_symbol = x_atom_to_symbol (dpyinfo, selection);
  local_selection_data = LOCAL_SELECTION (selection_symbol, dpyinfo);

  /* Well, we already believe that we don't own it, so that's just fine.  */
  if (NILP (local_selection_data)) return;

  CONS_TO_INTEGER (XCAR (XCDR (XCDR (local_selection_data))),
		   Time, local_selection_time);

  /* We have reasserted the selection since this SelectionClear was
     generated, so we can disregard it.  */
  if (changed_owner_time != CurrentTime
      && local_selection_time > changed_owner_time)
    return;

  /* Otherwise, really clear.  Don't use Fdelq as that may quit.  */
  Vselection_alist = dpyinfo->terminal->Vselection_alist;
  if (EQ (local_selection_data, CAR (Vselection_alist)))
    Vselection_alist = XCDR (Vselection_alist);
  else
    {
      Lisp_Object rest;
      for (rest = Vselection_alist; CONSP (rest); rest = XCDR (rest))
	if (EQ (local_selection_data, CAR (XCDR (rest))))
	  {
	    XSETCDR (rest, XCDR (XCDR (rest)));
	    break;
	  }
    }
  tset_selection_alist (dpyinfo->terminal, Vselection_alist);

  /* Run the `x-lost-selection-functions' abnormal hook.  */
  CALLN (Frun_hook_with_args, Qx_lost_selection_functions, selection_symbol);

  /* If Emacs lost ownership of XdndSelection during drag-and-drop,
     there is no point in continuing the drag-and-drop session.  */
  if (x_dnd_in_progress
      && EQ (selection_symbol, QXdndSelection))
    error ("Lost ownership of XdndSelection");

  redisplay_preserve_echo_area (20);
}

void
x_handle_selection_event (struct selection_input_event *event)
{
  TRACE0 ("x_handle_selection_event");
  if (event->kind != SELECTION_REQUEST_EVENT)
    x_handle_selection_clear (event);
  else
    x_handle_selection_request (event);
}

static bool
x_should_preserve_selection (Lisp_Object selection)
{
  Lisp_Object tem;

  tem = Vx_auto_preserve_selections;

  if (CONSP (Vx_auto_preserve_selections))
    {
      FOR_EACH_TAIL_SAFE (tem)
	{
	  if (EQ (XCAR (tem), selection))
	    return true;
	}

      return false;
    }

  return !NILP (tem);
}

/* Clear all selections that were made from frame F.
   We do this when about to delete a frame.  */

void
x_clear_frame_selections (struct frame *f)
{
  Lisp_Object frame, rest, lost, selection;
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  struct terminal *t = dpyinfo->terminal;

  XSETFRAME (frame, f);
  lost = Qnil;

  /* Delete elements from the beginning of Vselection_alist.  */
  while (CONSP (t->Vselection_alist)
	 && EQ (frame, XCAR (XCDR (XCDR (XCDR (XCAR (t->Vselection_alist)))))))
    {
      selection = CAR (CAR (t->Vselection_alist));

      if (!x_should_preserve_selection (selection))
	/* Run the `x-lost-selection-functions' abnormal hook.  */
	CALLN (Frun_hook_with_args, Qx_lost_selection_functions,
	       selection);
      else
	lost = Fcons (CAR (t->Vselection_alist), lost);

      tset_selection_alist (t, XCDR (t->Vselection_alist));
    }

  /* Delete elements after the beginning of Vselection_alist.  */
  for (rest = t->Vselection_alist; CONSP (rest); rest = XCDR (rest))
    if (CONSP (XCDR (rest))
	&& EQ (frame, XCAR (XCDR (XCDR (XCDR (XCAR (XCDR (rest))))))))
      {
	selection = XCAR (XCAR (XCDR (rest)));

	if (!x_should_preserve_selection (selection))
	  CALLN (Frun_hook_with_args, Qx_lost_selection_functions,
		 selection);
	else
	  lost = Fcons (XCAR (XCDR (rest)), lost);

	XSETCDR (rest, XCDR (XCDR (rest)));
	break;
      }

  if (!NILP (lost))
    x_preserve_selections (dpyinfo, lost, frame);
}

/* True if any properties for DISPLAY and WINDOW
   are on the list of what we are waiting for.  */

static bool
waiting_for_other_props_on_window (Display *display, Window window)
{
  for (struct prop_location *p = property_change_wait_list; p; p = p->next)
    if (p->display == display && p->window == window)
      return true;
  return false;
}

/* Add an entry to the list of property changes we are waiting for.
   DISPLAY, WINDOW, PROPERTY, STATE describe what we will wait for.
   The return value is a number that uniquely identifies
   this awaited property change.  */

static struct prop_location *
expect_property_change (Display *display, Window window,
                        Atom property, int state)
{
  struct prop_location *pl = xmalloc (sizeof *pl);
  pl->identifier = ++prop_location_identifier;
  pl->display = display;
  pl->window = window;
  pl->property = property;
  pl->desired_state = state;
  pl->next = property_change_wait_list;
  pl->arrived = false;
  property_change_wait_list = pl;
  return pl;
}

/* Delete an entry from the list of property changes we are waiting for.
   IDENTIFIER is the number that uniquely identifies the entry.  */

static void
unexpect_property_change (struct prop_location *location)
{
  struct prop_location *prop, **pprev = &property_change_wait_list;

  for (prop = property_change_wait_list; prop; prop = *pprev)
    {
      if (prop == location)
	{
	  *pprev = prop->next;
	  xfree (prop);
	  break;
	}
      else
	pprev = &prop->next;
    }
}

/* Remove the property change expectation element for IDENTIFIER.  */

static void
wait_for_property_change_unwind (void *loc)
{
  struct prop_location *location = loc;

  unexpect_property_change (location);
  if (location == property_change_reply_object)
    property_change_reply_object = 0;
}

/* Actually wait for a property change.
   IDENTIFIER should be the value that expect_property_change returned.  */

static void
wait_for_property_change (struct prop_location *location)
{
  specpdl_ref count = SPECPDL_INDEX ();

  /* Make sure to do unexpect_property_change if we quit or err.  */
  record_unwind_protect_ptr (wait_for_property_change_unwind, location);

  /* See comment in x_reply_selection_request about setting
     property_change_reply.  Do not do it here.  */

  /* If the event we are waiting for arrives beyond here, it will set
     property_change_reply, because property_change_reply_object says so.  */
  if (! location->arrived)
    {
      intmax_t timeout = max (0, x_selection_timeout);
      intmax_t secs = timeout / 1000;
      int nsecs = (timeout % 1000) * 1000000;
      TRACE2 ("  Waiting %"PRIdMAX" secs, %d nsecs", secs, nsecs);

      if (!input_blocked_p ())
	wait_reading_process_output (secs, nsecs, 0, false,
				     property_change_reply, NULL, 0);
      else
	x_wait_for_cell_change (property_change_reply,
				make_timespec (secs, nsecs));

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
x_handle_property_notify (const XPropertyEvent *event)
{
  struct prop_location *rest;

  for (rest = property_change_wait_list; rest; rest = rest->next)
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

	  rest->arrived = true;

	  /* If this is the one wait_for_property_change is waiting for,
	     tell it to wake up.  */
	  if (rest == property_change_reply_object)
	    XSETCAR (property_change_reply, Qt);

	  return;
	}
    }
}

static void
x_display_selection_waiting_message (struct atimer *timer)
{
  Lisp_Object val;

  val = build_string ("Waiting for reply from selection owner...");
  message3_nolog (val);
}

static void
x_cancel_atimer (void *atimer)
{
  cancel_atimer (atimer);
}


/* Variables for communication with x_handle_selection_notify.  */
static Atom reading_which_selection;
static Lisp_Object reading_selection_reply;
static Window reading_selection_window;

/* Do protocol to read selection-data from the server.
   Converts this to Lisp data and returns it.
   FRAME is the frame whose X window shall request the selection.  */

static Lisp_Object
x_get_foreign_selection (Lisp_Object selection_symbol, Lisp_Object target_type,
			 Lisp_Object time_stamp, Lisp_Object frame)
{
  struct frame *f = XFRAME (frame);
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  Display *display = dpyinfo->display;
  Window requestor_window = FRAME_X_WINDOW (f);
  Time requestor_time = dpyinfo->last_user_time;
  Atom target_property = dpyinfo->Xatom_EMACS_TMP;
  Atom selection_atom = symbol_to_x_atom (dpyinfo, selection_symbol);
  Atom type_atom = (CONSP (target_type)
		    ? symbol_to_x_atom (dpyinfo, XCAR (target_type))
		    : symbol_to_x_atom (dpyinfo, target_type));
  struct atimer *delayed_message;
  struct timespec message_interval;
  specpdl_ref count;

  count = SPECPDL_INDEX ();

  if (!FRAME_LIVE_P (f))
    return unbind_to (count, Qnil);

  if (! NILP (time_stamp))
    CONS_TO_INTEGER (time_stamp, Time, requestor_time);

  block_input ();
  TRACE2 ("Get selection %s, type %s",
	  XGetAtomName (display, type_atom),
	  XGetAtomName (display, target_property));

  x_catch_errors (display);
  XConvertSelection (display, selection_atom, type_atom, target_property,
		     requestor_window, requestor_time);
  x_check_errors (display, "Can't convert selection: %s");
  x_uncatch_errors_after_check ();

  /* Prepare to block until the reply has been read.  */
  reading_selection_window = requestor_window;
  reading_which_selection = selection_atom;
  XSETCAR (reading_selection_reply, Qnil);

  /* It should not be necessary to stop handling selection requests
     during this time.  In fact, the SAVE_TARGETS mechanism requires
     us to handle a clipboard manager's requests before it returns
     SelectionNotify. */
#if false
  x_start_queuing_selection_requests ();
  record_unwind_protect_void (x_stop_queuing_selection_requests);
#endif

  unblock_input ();

  message_interval = make_timespec (1, 0);
  delayed_message = start_atimer (ATIMER_RELATIVE, message_interval,
				  x_display_selection_waiting_message,
				  NULL);
  record_unwind_protect_ptr (x_cancel_atimer, delayed_message);

  /* This allows quits.  Also, don't wait forever.  */
  intmax_t timeout = max (0, x_selection_timeout);
  intmax_t secs = timeout / 1000;
  int nsecs = (timeout % 1000) * 1000000;
  TRACE1 ("  Start waiting %"PRIdMAX" secs for SelectionNotify.", secs);

  if (input_blocked_p ())
    TRACE0 ("    Input is blocked.");
  else
    TRACE1 ("    Waiting for %d nsecs in addition.", nsecs);

  /* This function can be called with input blocked inside Xt or GTK
     timeouts run inside popup menus, so use a function that works
     when input is blocked.  Prefer wait_reading_process_output
     otherwise, or the toolkit might not get some events.
     (bug#22214) */
  if (!input_blocked_p ())
    wait_reading_process_output (secs, nsecs, 0, false,
				 reading_selection_reply, NULL, 0);
  else
    x_wait_for_cell_change (reading_selection_reply,
			    make_timespec (secs, nsecs));
  TRACE1 ("  Got event = %s", (!NILP (XCAR (reading_selection_reply))
			       ? (SYMBOLP (XCAR (reading_selection_reply))
				  ? SSDATA (SYMBOL_NAME (XCAR (reading_selection_reply)))
				  : "YES")
			       : "NO"));

  if (NILP (XCAR (reading_selection_reply)))
    error ("Timed out waiting for reply from selection owner");
  if (EQ (XCAR (reading_selection_reply), Qlambda))
    return unbind_to (count, Qnil);

  /* Otherwise, the selection is waiting for us on the requested property.  */
  return unbind_to (count,
		    x_get_window_property_as_lisp_data (dpyinfo,
							requestor_window,
							target_property,
							target_type,
							selection_atom,
							false));
}

/* Subroutines of x_get_window_property_as_lisp_data */

/* Use xfree, not XFree, to free the data obtained with this function.  */

static void
x_get_window_property (Display *display, Window window, Atom property,
		       unsigned char **data_ret, ptrdiff_t *bytes_ret,
		       Atom *actual_type_ret, int *actual_format_ret,
		       unsigned long *actual_size_ret)
{
  ptrdiff_t total_size;
  unsigned long bytes_remaining;
  ptrdiff_t offset = 0;
  unsigned char *data = 0;
  unsigned char *tmp_data = 0;
  int result;
  int buffer_size = selection_quantum (display);

  /* Wide enough to avoid overflow in expressions using it.  */
  ptrdiff_t x_long_size = X_LONG_SIZE;

  /* Maximum value for TOTAL_SIZE.  It cannot exceed PTRDIFF_MAX - 1
     and SIZE_MAX - 1, for an extra byte at the end.  And it cannot
     exceed LONG_MAX * X_LONG_SIZE, for XGetWindowProperty.  */
  ptrdiff_t total_size_max =
    ((min (PTRDIFF_MAX, SIZE_MAX) - 1) / x_long_size < LONG_MAX
     ? min (PTRDIFF_MAX, SIZE_MAX) - 1
     : LONG_MAX * x_long_size);

  block_input ();

  /* First probe the thing to find out how big it is.  */
  result = XGetWindowProperty (display, window, property,
			       0, 0, False, AnyPropertyType,
			       actual_type_ret, actual_format_ret,
			       actual_size_ret,
			       &bytes_remaining, &tmp_data);
  if (result != Success)
    goto done;

  /* This was allocated by Xlib, so use XFree.  */
  XFree (tmp_data);

  if (*actual_type_ret == None || *actual_format_ret == 0)
    goto done;

  if (total_size_max < bytes_remaining)
    goto size_overflow;
  total_size = bytes_remaining;
  data = xmalloc (total_size + 1);

  /* Now read, until we've gotten it all.  */
  while (bytes_remaining)
    {
      ptrdiff_t bytes_gotten;
      int bytes_per_item;
      result
	= XGetWindowProperty (display, window, property,
			      offset / X_LONG_SIZE,
			      buffer_size / X_LONG_SIZE,
			      False,
			      AnyPropertyType,
			      actual_type_ret, actual_format_ret,
			      actual_size_ret, &bytes_remaining, &tmp_data);

      /* If this doesn't return Success at this point, it means that
	 some clod deleted the selection while we were in the midst of
	 reading it.  Deal with that, I guess.... */
      if (result != Success)
	break;

      bytes_per_item = *actual_format_ret >> 3;
      eassert (*actual_size_ret <= buffer_size / bytes_per_item);

      /* The man page for XGetWindowProperty says:
         "If the returned format is 32, the returned data is represented
          as a long array and should be cast to that type to obtain the
          elements."
         This applies even if long is more than 32 bits, the X library
         converts from 32 bit elements received from the X server to long
         and passes the long array to us.  Thus, for that case memcpy can not
         be used.  We convert to a 32 bit type here, because so much code
         assume on that.

         The bytes and offsets passed to XGetWindowProperty refers to the
         property and those are indeed in 32 bit quantities if format is 32.  */

      bytes_gotten = *actual_size_ret;
      bytes_gotten *= bytes_per_item;

      TRACE2 ("Read %"pD"d bytes from property %s",
	      bytes_gotten, XGetAtomName (display, property));

      if (total_size - offset < bytes_gotten)
	{
	  unsigned char *data1;
	  ptrdiff_t remaining_lim = total_size_max - offset - bytes_gotten;
	  if (remaining_lim < 0 || remaining_lim < bytes_remaining)
	    goto size_overflow;
	  total_size = offset + bytes_gotten + bytes_remaining;
	  data1 = xrealloc (data, total_size + 1);
	  data = data1;
	}

      if (LONG_WIDTH > 32 && *actual_format_ret == 32)
        {
          unsigned long i;
	  int  *idata = (int *) (data + offset);
          long *ldata = (long *) tmp_data;

          for (i = 0; i < *actual_size_ret; ++i)
	    idata[i] = ldata[i];
        }
      else
	memcpy (data + offset, tmp_data, bytes_gotten);

      offset += bytes_gotten;

      /* This was allocated by Xlib, so use XFree.  */
      XFree (tmp_data);
    }

  XFlush (display);
  data[offset] = '\0';

 done:
  unblock_input ();
  *data_ret = data;
  *bytes_ret = offset;
  return;

 size_overflow:
  if (data)
    xfree (data);
  unblock_input ();
  memory_full (SIZE_MAX);
}

/* Use xfree, not XFree, to free the data obtained with this function.  */

static void
receive_incremental_selection (struct x_display_info *dpyinfo,
			       Window window, Atom property,
			       Lisp_Object target_type,
			       unsigned int min_size_bytes,
			       unsigned char **data_ret,
			       ptrdiff_t *size_bytes_ret,
			       Atom *type_ret, int *format_ret,
			       unsigned long *size_ret,
			       ptrdiff_t *real_bytes_ret)
{
  ptrdiff_t offset = 0;
  struct prop_location *wait_object;
  Display *display = dpyinfo->display;

  if (min (PTRDIFF_MAX, SIZE_MAX) < min_size_bytes)
    memory_full (SIZE_MAX);
  *data_ret = xmalloc (min_size_bytes);
  *size_bytes_ret = min_size_bytes;

  TRACE1 ("Read %u bytes incrementally", min_size_bytes);

  /* At this point, we have read an INCR property.
     Delete the property to ack it.
     (But first, prepare to receive the next event in this handshake.)

     Now, we must loop, waiting for the sending window to put a value on
     that property, then reading the property, then deleting it to ack.
     We are done when the sender places a property of length 0.
   */
  block_input ();
  XSelectInput (display, window, STANDARD_EVENT_SET | PropertyChangeMask);
  TRACE1 ("  Delete property %s",
	  SDATA (SYMBOL_NAME (x_atom_to_symbol (dpyinfo, property))));
  XDeleteProperty (display, window, property);
  TRACE1 ("  Expect new value of property %s",
	  SDATA (SYMBOL_NAME (x_atom_to_symbol (dpyinfo, property))));
  wait_object = expect_property_change (display, window, property,
					PropertyNewValue);
  XFlush (display);
  /* See comment in x_reply_selection_request about property_change_reply.  */
  set_property_change_object (wait_object);
  unblock_input ();

  while (true)
    {
      unsigned char *tmp_data;
      ptrdiff_t tmp_size_bytes;

      TRACE0 ("  Wait for property change");
      wait_for_property_change (wait_object);

      /* expect it again immediately, because x_get_window_property may
	 .. no it won't, I don't get it.
	 .. Ok, I get it now, the Xt code that implements INCR is broken. */
      TRACE0 ("  Get property value");
      x_get_window_property (display, window, property,
			     &tmp_data, &tmp_size_bytes,
			     type_ret, format_ret, size_ret);

      TRACE1 ("  Read increment of %"pD"d bytes", tmp_size_bytes);

      if (tmp_size_bytes == 0) /* we're done */
	{
	  TRACE1 ("Done reading incrementally; total bytes: %"pD"d",
		  *size_bytes_ret);

	  if (! waiting_for_other_props_on_window (display, window))
	    XSelectInput (display, window, STANDARD_EVENT_SET);
	  /* Use xfree, not XFree, because x_get_window_property
	     calls xmalloc itself.  */
	  xfree (tmp_data);
	  break;
	}

      block_input ();
      TRACE1 ("  ACK by deleting property %s",
	      XGetAtomName (display, property));
      XDeleteProperty (display, window, property);
      wait_object = expect_property_change (display, window, property,
					    PropertyNewValue);
      /* See comment in x_reply_selection_request about
	 property_change_reply.  */
      set_property_change_object (wait_object);
      XFlush (display);
      unblock_input ();

      if (*size_bytes_ret - offset < tmp_size_bytes)
	*data_ret = xpalloc (*data_ret, size_bytes_ret,
			     tmp_size_bytes - (*size_bytes_ret - offset),
			     -1, 1);

      memcpy ((*data_ret) + offset, tmp_data, tmp_size_bytes);
      offset += tmp_size_bytes;

      /* *size_bytes_ret is not really the size of the data inside the
	 buffer; it is the size of the buffer allocated by xpalloc.

	 This matters when the cardinal specified in the INCR property
	 (a _lower bound_ on the size of the selection data) is
	 smaller than the actual selection contents, which can happen
	 when programs are streaming selection data from a file
	 descriptor.  In that case, we used to return junk if xpalloc
	 decided to grow the buffer by more than the provided
	 increment; to avoid that, store the actual size of the
	 selection data in *real_bytes_ret.  */
      *real_bytes_ret += tmp_size_bytes;

      /* Use xfree, not XFree, because x_get_window_property
	 calls xmalloc itself.  */
      xfree (tmp_data);
    }
}


/* Fetch a value from property PROPERTY of X window WINDOW on display
   DISPLAY.  TARGET_TYPE and SELECTION_ATOM are used in error message
   if this fails.  */

static Lisp_Object
x_get_window_property_as_lisp_data (struct x_display_info *dpyinfo,
				    Window window, Atom property,
				    Lisp_Object target_type,
				    Atom selection_atom,
				    bool for_multiple)
{
  Atom actual_type;
  int actual_format;
  unsigned long actual_size;
  unsigned char *data = 0;
  ptrdiff_t bytes = 0, array_bytes;
  Lisp_Object val;
  Display *display = dpyinfo->display;

  /* array_bytes is only used as an argument to xpalloc.  The actual
     size of the data inside the buffer is inside bytes.  */
  array_bytes = 0;

  TRACE0 ("Reading selection data");

  x_get_window_property (display, window, property, &data, &bytes,
			 &actual_type, &actual_format, &actual_size);
  if (! data)
    {
      if (for_multiple)
	return Qnil;
      block_input ();
      bool there_is_a_selection_owner
	= XGetSelectionOwner (display, selection_atom) != 0;
      unblock_input ();
      if (there_is_a_selection_owner)
	{
	  AUTO_STRING (format, "Selection owner couldn't convert: %s");
	  CALLN (Fmessage, format,
		 actual_type
		 ? list2 (target_type,
			  x_atom_to_symbol (dpyinfo, actual_type))
		 : target_type);
	  return Qnil;
	}
      else
	{
	  AUTO_STRING (format, "No selection: %s");
	  CALLN (Fmessage, format, x_atom_to_symbol (dpyinfo, selection_atom));
	  return Qnil;
	}
    }

  if (!for_multiple && actual_type == dpyinfo->Xatom_INCR)
    {
      /* That wasn't really the data, just the beginning.  */

      unsigned int min_size_bytes = * ((unsigned int *) data);
      block_input ();
      /* Use xfree, not XFree, because x_get_window_property
	 calls xmalloc itself.  */
      xfree (data);
      unblock_input ();

      /* Clear bytes again.  Previously, receive_incremental_selection
	 would set this to min_size_bytes, but that is now done to
	 array_bytes instead.  */
      bytes = 0;
      receive_incremental_selection (dpyinfo, window, property, target_type,
				     min_size_bytes, &data, &array_bytes,
				     &actual_type, &actual_format,
				     &actual_size, &bytes);
    }

  if (!for_multiple)
    {
      block_input ();
      TRACE1 ("  Delete property %s", XGetAtomName (display, property));
      XDeleteProperty (display, window, property);
      XFlush (display);
      unblock_input ();
    }

  /* It's been read.  Now convert it to a lisp object in some semi-rational
     manner.  */
  val = selection_data_to_lisp_data (dpyinfo, data, bytes,
				     actual_type, actual_format);

  /* Use xfree, not XFree, because x_get_window_property
     calls xmalloc itself.  */
  xfree (data);
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
	*	32	1		Integer
	*	32	> 1		Vector of the above

   When converting an object to C, it may be of the form (SYMBOL . <data>)
   where SYMBOL is what we should claim that the type is.  Format and
   representation are as above.

   Important: When format is 32, data should contain an array of int,
   not an array of long as the X library returns.  This makes a difference
   when sizeof(long) != sizeof(int).  */



static Lisp_Object
selection_data_to_lisp_data (struct x_display_info *dpyinfo,
			     const unsigned char *data,
			     ptrdiff_t size, Atom type, int format)
{
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
      Fput_text_property (make_fixnum (0), make_fixnum (size),
			  Qforeign_selection, lispy_type, str);
      return str;
    }
  /* Convert a single atom to a Lisp_Symbol.  Convert a set of atoms to
     a vector of symbols.  */
  else if (type == XA_ATOM
	   /* Treat ATOM_PAIR type similar to list of atoms.  */
	   || type == dpyinfo->Xatom_ATOM_PAIR)
    {
      ptrdiff_t i;
      /* On a 64 bit machine sizeof(Atom) == sizeof(long) == 8.
         But the callers of these function has made sure the data for
         format == 32 is an array of int.  Thus, use int instead
         of Atom.  */
      int *idata = (int *) data;

      if (size == sizeof (int))
	return x_atom_to_symbol (dpyinfo, (Atom) idata[0]);
      else
	{
	  Lisp_Object v = make_nil_vector (size / sizeof (int));

	  for (i = 0; i < size / sizeof (int); i++)
	    ASET (v, i, x_atom_to_symbol (dpyinfo, (Atom) idata[i]));
	  return v;
	}
    }

  /* Convert a single 16-bit number or a small 32-bit number to a Lisp_Int.
     If the number is 32 bits and won't fit in a Lisp_Int, convert it
     to a bignum.

     INTEGER is a signed type, CARDINAL is unsigned.
     Assume any other types are unsigned as well.
   */
  else if (format == 32 && size == sizeof (int))
    {
      if (type == XA_INTEGER)
        return INT_TO_INTEGER (((int *) data) [0]);
      else
        return INT_TO_INTEGER (((unsigned int *) data) [0]);
    }
  else if (format == 16 && size == sizeof (short))
    {
      if (type == XA_INTEGER)
        return make_fixnum (((short *) data) [0]);
      else
        return make_fixnum (((unsigned short *) data) [0]);
    }

  /* Convert any other kind of data to a vector of numbers, represented
     as above (as an integer, or a cons of two 16 bit integers.)
   */
  else if (format == 16)
    {
      ptrdiff_t i;
      Lisp_Object v = make_uninit_vector (size / 2);

      if (type == XA_INTEGER)
        {
          for (i = 0; i < size / 2; i++)
            {
              short j = ((short *) data) [i];
              ASET (v, i, make_fixnum (j));
            }
        }
      else
        {
          for (i = 0; i < size / 2; i++)
            {
              unsigned short j = ((unsigned short *) data) [i];
              ASET (v, i, make_fixnum (j));
            }
        }
      return v;
    }
  else
    {
      ptrdiff_t i;
      Lisp_Object v = make_nil_vector (size / X_LONG_SIZE);

      if (type == XA_INTEGER)
        {
          for (i = 0; i < size / X_LONG_SIZE; i++)
            {
              int j = ((int *) data) [i];
              ASET (v, i, INT_TO_INTEGER (j));
            }
        }
      else
        {
          for (i = 0; i < size / X_LONG_SIZE; i++)
            {
              unsigned int j = ((unsigned int *) data) [i];
              ASET (v, i, INT_TO_INTEGER (j));
            }
        }
      return v;
    }
}

/* Convert OBJ to an X long value, and return it as unsigned long.
   OBJ should be an integer or a cons representing an integer.
   Treat values in the range X_LONG_MAX + 1 .. X_ULONG_MAX as X
   unsigned long values: in theory these values are supposed to be
   signed but in practice unsigned 32-bit data are communicated via X
   selections and we need to support that.  */
static unsigned long
cons_to_x_long (Lisp_Object obj)
{
  if (X_ULONG_MAX <= INTMAX_MAX
      || NILP (Fnatnump (CONSP (obj) ? XCAR (obj) : obj)))
    return cons_to_signed (obj, X_LONG_MIN, min (X_ULONG_MAX, INTMAX_MAX));
  else
    return cons_to_unsigned (obj, X_ULONG_MAX);
}

/* Use xfree, not XFree, to free the data obtained with this function.  */

static void
lisp_data_to_selection_data (struct x_display_info *dpyinfo,
			     Lisp_Object obj, struct selection_data *cs)
{
  Lisp_Object type = Qnil;

  eassert (cs != NULL);
  cs->nofree = false;

  if (CONSP (obj) && SYMBOLP (XCAR (obj)))
    {
      type = XCAR (obj);
      obj = XCDR (obj);
      if (CONSP (obj) && NILP (XCDR (obj)))
	obj = XCAR (obj);
    }

  if (EQ (obj, QNULL) || (EQ (type, QNULL)))
    {				/* This is not the same as declining */
      cs->format = 32;
      cs->size = 0;
      cs->data = NULL;
      type = QNULL;
    }
  else if (STRINGP (obj))
    {
      if (SCHARS (obj) < SBYTES (obj))
	/* OBJ is a multibyte string containing a non-ASCII char.  */
	signal_error ("Non-ASCII string must be encoded in advance", obj);
      if (NILP (type))
	type = QSTRING;
      cs->format = 8;
      cs->size = SBYTES (obj);
      cs->data = SDATA (obj);
      cs->nofree = true;
    }
  else if (SYMBOLP (obj))
    {
      void *data = xmalloc (sizeof (Atom) + 1);
      Atom *x_atom_ptr = data;
      cs->data = data;
      cs->format = 32;
      cs->size = 1;
      cs->data[sizeof (Atom)] = 0;
      *x_atom_ptr = symbol_to_x_atom (dpyinfo, obj);
      if (NILP (type)) type = QATOM;
    }
  else if (RANGED_FIXNUMP (X_SHRT_MIN, obj, X_SHRT_MAX))
    {
      void *data = xmalloc (sizeof (short) + 1);
      short *short_ptr = data;
      cs->data = data;
      cs->format = 16;
      cs->size = 1;
      cs->data[sizeof (short)] = 0;
      *short_ptr = XFIXNUM (obj);
      if (NILP (type)) type = QINTEGER;
    }
  else if (INTEGERP (obj)
	   || (CONSP (obj) && INTEGERP (XCAR (obj))
	       && (FIXNUMP (XCDR (obj))
		   || (CONSP (XCDR (obj))
		       && FIXNUMP (XCAR (XCDR (obj)))))))
    {
      void *data = xmalloc (sizeof (unsigned long) + 1);
      unsigned long *x_long_ptr = data;
      cs->data = data;
      cs->format = 32;
      cs->size = 1;
      cs->data[sizeof (unsigned long)] = 0;
      *x_long_ptr = cons_to_x_long (obj);
      if (NILP (type)) type = QINTEGER;
    }
  else if (VECTORP (obj))
    {
      /* Lisp_Vectors may represent a set of ATOMs;
	 a set of 16 or 32 bit INTEGERs;
	 or a set of ATOM_PAIRs (represented as [[A1 A2] [A3 A4] ...]
       */
      ptrdiff_t i;
      ptrdiff_t size = ASIZE (obj);

      if (!size)
	{
	  /* This vector is empty and of unknown type.  Assume that it
	     is a vector of integers.  */

	  cs->data = NULL;
	  cs->format = 32;
	  cs->size = 0;
	  type = QINTEGER;
	}
      else if (SYMBOLP (AREF (obj, 0)))
	/* This vector is an ATOM set */
	{
	  void *data;
	  Atom *x_atoms;
	  if (NILP (type)) type = QATOM;
	  for (i = 0; i < size; i++)
	    if (!SYMBOLP (AREF (obj, i)))
	      signal_error ("All elements of selection vector must have same type", obj);

	  cs->data = data = xnmalloc (size, sizeof *x_atoms);
	  x_atoms = data;
	  cs->format = 32;
	  cs->size = size;
	  for (i = 0; i < size; i++)
	    x_atoms[i] = symbol_to_x_atom (dpyinfo, AREF (obj, i));
	}
      else
	/* This vector is an INTEGER set, or something like it */
	{
	  int format = 16;
	  int data_size = sizeof (short);
	  void *data;
	  unsigned long *x_atoms;
	  short *shorts;
	  if (NILP (type)) type = QINTEGER;
	  for (i = 0; i < size; i++)
	    {
	      if (! RANGED_FIXNUMP (X_SHRT_MIN, AREF (obj, i),
				     X_SHRT_MAX))
		{
		  /* Use sizeof (long) even if it is more than 32 bits.
		     See comment in x_get_window_property and
		     x_fill_property_data.  */
		  data_size = sizeof (long);
		  format = 32;
		  break;
		}
	    }
	  cs->data = data = xnmalloc (size, data_size);
	  x_atoms = data;
	  shorts = data;
	  cs->format = format;
	  cs->size = size;
	  for (i = 0; i < size; i++)
	    {
	      if (format == 32)
		x_atoms[i] = cons_to_x_long (AREF (obj, i));
	      else
		shorts[i] = XFIXNUM (AREF (obj, i));
	    }
	}
    }
  else
    signal_error (/* Qselection_error */ "Unrecognized selection data", obj);

  cs->type = symbol_to_x_atom (dpyinfo, type);
}

static Lisp_Object
clean_local_selection_data (Lisp_Object obj)
{
  if (CONSP (obj)
      && INTEGERP (XCAR (obj))
      && CONSP (XCDR (obj))
      && FIXNUMP (XCAR (XCDR (obj)))
      && NILP (XCDR (XCDR (obj))))
    obj = Fcons (XCAR (obj), XCDR (obj));

  if (CONSP (obj)
      && INTEGERP (XCAR (obj))
      && FIXNUMP (XCDR (obj)))
    {
      if (BASE_EQ (XCAR (obj), make_fixnum (0)))
	return XCDR (obj);
      if (BASE_EQ (XCAR (obj), make_fixnum (-1)))
	return make_fixnum (- XFIXNUM (XCDR (obj)));
    }
  if (VECTORP (obj))
    {
      ptrdiff_t i;
      ptrdiff_t size = ASIZE (obj);
      Lisp_Object copy;
      if (size == 1)
	return clean_local_selection_data (AREF (obj, 0));
      copy = make_nil_vector (size);
      for (i = 0; i < size; i++)
	ASET (copy, i, clean_local_selection_data (AREF (obj, i)));
      return copy;
    }
  return obj;
}

/* Called from XTread_socket to handle SelectionNotify events.
   If it's the selection we are waiting for, stop waiting
   by setting the car of reading_selection_reply to non-nil.
   We store t there if the reply is successful, lambda if not.  */

void
x_handle_selection_notify (const XSelectionEvent *event)
{
  if (event->requestor != reading_selection_window)
    return;
  if (event->selection != reading_which_selection)
    return;

  TRACE1 ("Received SelectionNotify: %d", (int) event->property);
  XSETCAR (reading_selection_reply,
	   (event->property != 0 ? Qt : Qlambda));
}


/* From a Lisp_Object, return a suitable frame for selection
   operations.  OBJECT may be a frame, a terminal object, or nil
   (which stands for the selected frame--or, if that is not an X
   frame, the first X display on the list).  If no suitable frame can
   be found, return NULL.  */

static struct frame *
frame_for_x_selection (Lisp_Object object)
{
  Lisp_Object tail, frame;
  struct frame *f;

  if (NILP (object))
    {
      f = XFRAME (selected_frame);
      if (FRAME_X_P (f) && FRAME_LIVE_P (f))
	return f;

      FOR_EACH_FRAME (tail, frame)
	{
	  f = XFRAME (frame);
	  if (FRAME_X_P (f) && FRAME_LIVE_P (f))
	    return f;
	}
    }
  else if (TERMINALP (object))
    {
      struct terminal *t = decode_live_terminal (object);

      if (t->type == output_x_window)
	FOR_EACH_FRAME (tail, frame)
	  {
	    f = XFRAME (frame);
	    if (FRAME_LIVE_P (f) && f->terminal == t)
	      return f;
	  }
    }
  else if (FRAMEP (object))
    {
      f = XFRAME (object);
      if (FRAME_X_P (f) && FRAME_LIVE_P (f))
	return f;
    }

  return NULL;
}


DEFUN ("x-own-selection-internal", Fx_own_selection_internal,
       Sx_own_selection_internal, 2, 3, 0,
       doc: /* Assert an X selection of type SELECTION and value VALUE.
SELECTION is a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)
VALUE is typically a string, or a cons of two markers, but may be
anything that the functions on `selection-converter-alist' know about.

FRAME should be a frame that should own the selection.  If omitted or
nil, it defaults to the selected frame.

On Nextstep, FRAME is unused.  */)
  (Lisp_Object selection, Lisp_Object value, Lisp_Object frame)
{
  if (NILP (frame)) frame = selected_frame;
  if (!FRAME_LIVE_P (XFRAME (frame)) || !FRAME_X_P (XFRAME (frame)))
    error ("X selection unavailable for this frame");

  CHECK_SYMBOL (selection);
  if (NILP (value)) error ("VALUE may not be nil");
  x_own_selection (selection, value, frame, Qnil, 0);
  return value;
}


/* Request the selection value from the owner.  If we are the owner,
   simply return our selection value.  If we are not the owner, this
   will block until all of the data has arrived.  */

DEFUN ("x-get-selection-internal", Fx_get_selection_internal,
       Sx_get_selection_internal, 2, 4, 0,
       doc: /* Return text selected from some X window.
SELECTION-SYMBOL is typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)
TARGET-TYPE is the type of data desired, typically `STRING'.

TIME-STAMP is the time to use in the XConvertSelection call for foreign
selections.  If omitted, defaults to the time for the last event.

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.

On Nextstep, TIME-STAMP and TERMINAL are unused.  */)
  (Lisp_Object selection_symbol, Lisp_Object target_type,
   Lisp_Object time_stamp, Lisp_Object terminal)
{
  Lisp_Object val = Qnil;
  Lisp_Object maybe_alias;
  struct frame *f = frame_for_x_selection (terminal);

  CHECK_SYMBOL (selection_symbol);
  CHECK_SYMBOL (target_type);

  if (EQ (target_type, QMULTIPLE))
    error ("Retrieving MULTIPLE selections is currently unimplemented");
  if (!f)
    error ("X selection unavailable for this frame");

  /* Quitting inside this function is okay, so we don't have to use
     FOR_EACH_TAIL_SAFE.  */
  maybe_alias = Fassq (selection_symbol, Vx_selection_alias_alist);

  if (!NILP (maybe_alias))
    {
      selection_symbol = XCDR (maybe_alias);
      CHECK_SYMBOL (selection_symbol);
    }

  val = x_get_local_selection (selection_symbol, target_type, true,
			       FRAME_DISPLAY_INFO (f), Qnil, false);

  if (NILP (val) && FRAME_LIVE_P (f))
    {
      Lisp_Object frame;
      XSETFRAME (frame, f);
      return x_get_foreign_selection (selection_symbol, target_type,
				      time_stamp, frame);
    }

  if (CONSP (val) && SYMBOLP (XCAR (val)))
    {
      val = XCDR (val);
      if (CONSP (val) && NILP (XCDR (val)))
	val = XCAR (val);
    }
  return clean_local_selection_data (val);
}

DEFUN ("x-disown-selection-internal", Fx_disown_selection_internal,
       Sx_disown_selection_internal, 1, 3, 0,
       doc: /* If we own the selection SELECTION, disown it.
Disowning it means there is no such selection.

Sets the last-change time for the selection to TIME-OBJECT (by default
the time of the last event).

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.

On Nextstep, the TIME-OBJECT and TERMINAL arguments are unused.
On MS-DOS, all this does is return non-nil if we own the selection.  */)
  (Lisp_Object selection, Lisp_Object time_object, Lisp_Object terminal)
{
  Time timestamp;
  Atom selection_atom;
  struct selection_input_event event;
  struct frame *f = frame_for_x_selection (terminal);
  struct x_display_info *dpyinfo;

  if (!f)
    return Qnil;

  dpyinfo = FRAME_DISPLAY_INFO (f);
  CHECK_SYMBOL (selection);

  /* Don't disown the selection when we're not the owner.  */
  if (NILP (LOCAL_SELECTION (selection, dpyinfo)))
    return Qnil;

  selection_atom = symbol_to_x_atom (dpyinfo, selection);

  block_input ();
  if (NILP (time_object))
    timestamp = dpyinfo->last_user_time;
  else
    CONS_TO_INTEGER (time_object, Time, timestamp);
  XSetSelectionOwner (dpyinfo->display, selection_atom, None, timestamp);
  unblock_input ();

  /* It doesn't seem to be guaranteed that a SelectionClear event will be
     generated for a window which owns the selection when that window sets
     the selection owner to None.  The NCD server does, the MIT Sun4 server
     doesn't.  So we synthesize one; this means we might get two, but
     that's ok, because the second one won't have any effect.  */
  SELECTION_EVENT_DPYINFO (&event) = dpyinfo;
  SELECTION_EVENT_SELECTION (&event) = selection_atom;
  SELECTION_EVENT_TIME (&event) = timestamp;
  x_handle_selection_clear (&event);

  return Qt;
}

DEFUN ("x-selection-owner-p", Fx_selection_owner_p, Sx_selection_owner_p,
       0, 2, 0,
       doc: /* Whether the current Emacs process owns the given X Selection.
The arg should be the name of the selection in question, typically one of
the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)
For convenience, the symbol nil is the same as `PRIMARY',
and t is the same as `SECONDARY'.

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.

On Nextstep, TERMINAL is unused.  */)
  (Lisp_Object selection, Lisp_Object terminal)
{
  struct frame *f = frame_for_x_selection (terminal);

  CHECK_SYMBOL (selection);
  if (NILP (selection)) selection = QPRIMARY;
  if (EQ (selection, Qt)) selection = QSECONDARY;

  if (f && !NILP (LOCAL_SELECTION (selection, FRAME_DISPLAY_INFO (f))))
    return Qt;
  else
    return Qnil;
}

DEFUN ("x-selection-exists-p", Fx_selection_exists_p, Sx_selection_exists_p,
       0, 2, 0,
       doc: /* Whether there is an owner for the given X selection.
SELECTION should be the name of the selection in question, typically
one of the symbols `PRIMARY', `SECONDARY', `CLIPBOARD', or
`CLIPBOARD_MANAGER' (X expects these literal upper-case names.)  The
symbol nil is the same as `PRIMARY', and t is the same as `SECONDARY'.

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.

On Nextstep, TERMINAL is unused.  */)
  (Lisp_Object selection, Lisp_Object terminal)
{
  Window owner;
  Atom atom;
#ifdef HAVE_XFIXES
  Window temp_owner;
#endif
  struct frame *f = frame_for_x_selection (terminal);
  struct x_display_info *dpyinfo;

  CHECK_SYMBOL (selection);

  if (NILP (selection))
    selection = QPRIMARY;

  if (EQ (selection, Qt))
    selection = QSECONDARY;

  if (!f)
    return Qnil;

  dpyinfo = FRAME_DISPLAY_INFO (f);

  if (!NILP (LOCAL_SELECTION (selection, dpyinfo)))
    return Qt;

  atom = symbol_to_x_atom (dpyinfo, selection);

  if (!atom)
    return Qnil;

#ifdef HAVE_XFIXES
  /* See if this information can be obtained without a roundtrip.  */
  temp_owner = x_find_selection_owner (dpyinfo, atom);

  if (temp_owner != X_INVALID_WINDOW)
    return (temp_owner != None ? Qt : Qnil);
#endif

  block_input ();
  owner = XGetSelectionOwner (dpyinfo->display, atom);
  unblock_input ();

  return (owner ? Qt : Qnil);
}

DEFUN ("x-get-local-selection", Fx_get_local_selection, Sx_get_local_selection,
       0, 2, 0,
       doc: /* Run selection converters for VALUE, and return the result.
TARGET is the selection target that is used to find a suitable
converter.  VALUE is a list of 4 values NAME, SELECTION-VALUE,
TIMESTAMP and FRAME.  NAME is the name of the selection that will be
passed to selection converters, SELECTION-VALUE is the value of the
selection used by the converter, TIMESTAMP is not meaningful (but must
be a number that fits in an X timestamp), and FRAME is the frame
describing the terminal for which the selection converter will be
run.  */)
  (Lisp_Object value, Lisp_Object target)
{
  Time time;
  Lisp_Object name, timestamp, frame, result;

  CHECK_SYMBOL (target);

  /* Check that VALUE has 4 elements, for x_get_local_selection.  */
  Lisp_Object v = value; CHECK_CONS (v);
  name = XCAR (v); v = XCDR (v); CHECK_CONS (v);
  v = XCDR (v); CHECK_CONS (v);
  timestamp = XCAR (v); v = XCDR (v); CHECK_CONS (v);
  frame = XCAR (v);

  CHECK_SYMBOL (name);
  CONS_TO_INTEGER (timestamp, Time, time);
  check_window_system (decode_live_frame (frame));

  result = x_get_local_selection (name, target, true,
				  NULL, value, false);

  if (CONSP (result) && SYMBOLP (XCAR (result)))
    {
      result = XCDR (result);

      if (CONSP (result) && NILP (XCDR (result)))
	result = XCAR (result);
    }

  return clean_local_selection_data (result);
}


/* Send clipboard manager a SAVE_TARGETS request with a UTF8_STRING
   property (https://www.freedesktop.org/wiki/ClipboardManager/).  */

static Lisp_Object
x_clipboard_manager_save (Lisp_Object frame)
{
  struct frame *f = XFRAME (frame);
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  Atom data = dpyinfo->Xatom_UTF8_STRING;

  XChangeProperty (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		   dpyinfo->Xatom_EMACS_TMP,
		   dpyinfo->Xatom_ATOM, 32, PropModeReplace,
		   (unsigned char *) &data, 1);
  x_get_foreign_selection (QCLIPBOARD_MANAGER, QSAVE_TARGETS,
			   Qnil, frame);
  return Qt;
}

/* Error handler for x_clipboard_manager_save_frame.  */

static Lisp_Object
x_clipboard_manager_error_1 (Lisp_Object err)
{
  AUTO_STRING (format, "X clipboard manager error: %s\n\
If the problem persists, set `%s' to nil.");
  AUTO_STRING (varname, "x-select-enable-clipboard-manager");
  CALLN (Fmessage, format, CAR (CDR (err)), varname);
  return Qnil;
}

/* Error handler for x_clipboard_manager_save_all.  */

static Lisp_Object
x_clipboard_manager_error_2 (Lisp_Object err)
{
  fputs (("Error saving to X clipboard manager.\n"
	  "If the problem persists,"
	  " set 'x-select-enable-clipboard-manager' to nil.\n"),
	 stderr);
  return Qnil;
}

/* Called from delete_frame: save any clipboard owned by FRAME to the
   clipboard manager.  Do nothing if FRAME does not own the clipboard,
   or if no clipboard manager is present.  */

void
x_clipboard_manager_save_frame (Lisp_Object frame)
{
  struct frame *f;

  if (!NILP (Vx_select_enable_clipboard_manager)
      && FRAMEP (frame)
      && (f = XFRAME (frame), FRAME_X_P (f))
      && FRAME_LIVE_P (f))
    {
      struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
      Lisp_Object local_selection
	= LOCAL_SELECTION (QCLIPBOARD, dpyinfo);

      if (!NILP (local_selection)
	  && EQ (frame, XCAR (XCDR (XCDR (XCDR (local_selection)))))
	  && XGetSelectionOwner (dpyinfo->display,
				 dpyinfo->Xatom_CLIPBOARD_MANAGER))
	internal_condition_case_1 (x_clipboard_manager_save, frame, Qt,
				   x_clipboard_manager_error_1);
    }
}

/* Called from Fkill_emacs: save any clipboard owned by FRAME to the
   clipboard manager.  Do nothing if FRAME does not own the clipboard,
   or if no clipboard manager is present.  */

void
x_clipboard_manager_save_all (void)
{
  /* Loop through all X displays, saving owned clipboards.  */
  struct x_display_info *dpyinfo;
  Lisp_Object local_selection, local_frame;

  if (NILP (Vx_select_enable_clipboard_manager))
    return;

  for (dpyinfo = x_display_list; dpyinfo; dpyinfo = dpyinfo->next)
    {
      local_selection = LOCAL_SELECTION (QCLIPBOARD, dpyinfo);
      if (NILP (local_selection)
	  || !XGetSelectionOwner (dpyinfo->display,
				  dpyinfo->Xatom_CLIPBOARD_MANAGER))
	continue;

      local_frame = XCAR (XCDR (XCDR (XCDR (local_selection))));
      if (FRAME_LIVE_P (XFRAME (local_frame)))
	{
	  message ("Saving clipboard to X clipboard manager...");
	  internal_condition_case_1 (x_clipboard_manager_save, local_frame,
				     Qt, x_clipboard_manager_error_2);
	}
    }
}


/***********************************************************************
                      Drag and drop support
***********************************************************************/
/* Check that lisp values are of correct type for x_fill_property_data.
   That is, number, string or a cons with two numbers (low and high 16
   bit parts of a 32 bit number).  Return the number of items in DATA,
   or -1 if there is an error.  */

int
x_check_property_data (Lisp_Object data)
{
  Lisp_Object iter;
  int size = 0;

  for (iter = data; CONSP (iter); iter = XCDR (iter))
    {
      Lisp_Object o = XCAR (iter);

      if (! NUMBERP (o) && ! STRINGP (o) && ! CONSP (o))
        return -1;
      else if (CONSP (o) &&
               (! NUMBERP (XCAR (o)) || ! NUMBERP (XCDR (o))))
        return -1;
      if (size == INT_MAX)
	return -1;
      size++;
    }

  return size;
}

/* Convert lisp values to a C array.  Values may be a number, a string
   which is taken as an X atom name and converted to the atom value, or
   a cons containing the two 16 bit parts of a 32 bit number.

   DPY is the display use to look up X atoms.
   DATA is a Lisp list of values to be converted.
   RET is the C array that contains the converted values.
   NELEMENTS_MAX is the number of values that will fit in RET.
   Any excess values in DATA are ignored.
   FORMAT is 8, 16 or 32 and denotes char/short/long for each C value to
   be stored in RET.  Note that long is used for 32 even if long is more
   than 32 bits (see man pages for XChangeProperty, XGetWindowProperty and
   XClientMessageEvent).  */

void
x_fill_property_data (Display *dpy, Lisp_Object data, void *ret,
		      int nelements_max, int format)
{
  unsigned long val;
  unsigned long  *d32 = (unsigned long  *) ret;
  unsigned short *d16 = (unsigned short *) ret;
  unsigned char  *d08 = (unsigned char  *) ret;
  int nelements;
  Lisp_Object iter;

  for (iter = data, nelements = 0;
       CONSP (iter) && nelements < nelements_max;
       iter = XCDR (iter), nelements++)
    {
      Lisp_Object o = XCAR (iter);

      if (NUMBERP (o) || CONSP (o))
        {
          if (CONSP (o)
	      && RANGED_FIXNUMP (X_LONG_MIN >> 16, XCAR (o), X_LONG_MAX >> 16)
	      && RANGED_FIXNUMP (- (1 << 15), XCDR (o), -1))
            {
	      /* cons_to_x_long does not handle negative values for v2.
                 For XDnd, v2 might be y of a window, and can be negative.
                 The XDnd spec. is not explicit about negative values,
                 but let's assume negative v2 is sent modulo 2**16.  */
	      unsigned long v1 = XFIXNUM (XCAR (o)) & 0xffff;
	      unsigned long v2 = XFIXNUM (XCDR (o)) & 0xffff;
	      val = (v1 << 16) | v2;
            }
          else
            val = cons_to_x_long (o);
        }
      else if (STRINGP (o))
        {
          block_input ();
          val = XInternAtom (dpy, SSDATA (o), False);
          unblock_input ();
        }
      else
        error ("Wrong type, must be string, number or cons");

      if (format == 8)
	{
	  if ((1 << 8) < val && val <= X_ULONG_MAX - (1 << 7))
	    error ("Out of `char' range");
	  *d08++ = val;
	}
      else if (format == 16)
	{
	  if ((1 << 16) < val && val <= X_ULONG_MAX - (1 << 15))
	    error ("Out of `short' range");
	  *d16++ = val;
	}
      else
        *d32++ = val;
    }
}

/* Convert an array of C values to a Lisp list.
   F is the frame to be used to look up X atoms if the TYPE is XA_ATOM.
   DATA is a C array of values to be converted.
   TYPE is the type of the data.  Only XA_ATOM is special, it converts
   each number in DATA to its corresponding X atom as a symbol.
   FORMAT is 8, 16 or 32 and gives the size in bits for each C value to
   be stored in RET.
   SIZE is the number of elements in DATA.

   Important: When format is 32, data should contain an array of int,
   not an array of long as the X library returns.  This makes a difference
   when sizeof(long) != sizeof(int).

   Also see comment for selection_data_to_lisp_data above.  */

Lisp_Object
x_property_data_to_lisp (struct frame *f, const unsigned char *data,
			 Atom type, int format, unsigned long size)
{
  ptrdiff_t format_bytes = format >> 3;
  ptrdiff_t data_bytes;
  if (INT_MULTIPLY_WRAPV (size, format_bytes, &data_bytes))
    memory_full (SIZE_MAX);
  return selection_data_to_lisp_data (FRAME_DISPLAY_INFO (f), data,
				      data_bytes, type, format);
}

DEFUN ("x-get-atom-name", Fx_get_atom_name,
       Sx_get_atom_name, 1, 2, 0,
       doc: /* Return the X atom name for VALUE as a string.
VALUE may be a number or a cons where the car is the upper 16 bits and
the cdr is the lower 16 bits of a 32 bit value.
Use the display for FRAME or the current frame if FRAME is not given or nil.

If the value is 0 or the atom is not known, return the empty string.  */)
  (Lisp_Object value, Lisp_Object frame)
{
  struct frame *f = decode_window_system_frame (frame);
  Display *dpy = FRAME_X_DISPLAY (f);
  struct x_display_info *dpyinfo;
  Atom atom;
  bool had_errors_p, need_sync;
  char *name;
  Lisp_Object ret;

  dpyinfo = FRAME_DISPLAY_INFO (f);
  CONS_TO_INTEGER (value, Atom, atom);

  x_catch_errors (dpy);
  name = x_get_atom_name (dpyinfo, atom, &need_sync);
  had_errors_p = need_sync && x_had_errors_p (dpy);
  x_uncatch_errors_after_check ();

  ret = empty_unibyte_string;

  if (name)
    {
      if (!had_errors_p)
	ret = build_string (name);
      xfree (name);
    }

  return ret;
}

DEFUN ("x-register-dnd-atom", Fx_register_dnd_atom,
       Sx_register_dnd_atom, 1, 2, 0,
       doc: /* Request that dnd events are made for ClientMessages with ATOM.
ATOM can be a symbol or a string.  The ATOM is interned on the display that
FRAME is on.  If FRAME is nil, the selected frame is used.  */)
  (Lisp_Object atom, Lisp_Object frame)
{
  Atom x_atom;
  struct frame *f = decode_window_system_frame (frame);
  ptrdiff_t i;
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);

  if (SYMBOLP (atom))
    x_atom = symbol_to_x_atom (dpyinfo, atom);
  else if (STRINGP (atom))
    {
      block_input ();
      x_atom = x_intern_cached_atom (dpyinfo, SSDATA (atom),
				     false);
      unblock_input ();
    }
  else
    error ("ATOM must be a symbol or a string");

  for (i = 0; i < dpyinfo->x_dnd_atoms_length; ++i)
    if (dpyinfo->x_dnd_atoms[i] == x_atom)
      return Qnil;

  if (dpyinfo->x_dnd_atoms_length == dpyinfo->x_dnd_atoms_size)
    dpyinfo->x_dnd_atoms =
      xpalloc (dpyinfo->x_dnd_atoms, &dpyinfo->x_dnd_atoms_size,
	       1, -1, sizeof *dpyinfo->x_dnd_atoms);

  dpyinfo->x_dnd_atoms[dpyinfo->x_dnd_atoms_length++] = x_atom;
  return Qnil;
}

/* Convert an XClientMessageEvent to a Lisp event of type DRAG_N_DROP_EVENT.  */

bool
x_handle_dnd_message (struct frame *f, const XClientMessageEvent *event,
                      struct x_display_info *dpyinfo, struct input_event *bufp,
		      bool root_window_coords, int root_x, int root_y)
{
  Lisp_Object vec;
  Lisp_Object frame;
  /* format 32 => size 5, format 16 => size 10, format 8 => size 20 */
  unsigned long size = 160/event->format;
  int x, y;
  unsigned char *data = (unsigned char *) event->data.b;
  int idata[5];
  ptrdiff_t i;

  for (i = 0; i < dpyinfo->x_dnd_atoms_length; ++i)
    if (dpyinfo->x_dnd_atoms[i] == event->message_type) break;

  if (i == dpyinfo->x_dnd_atoms_length) return false;

  XSETFRAME (frame, f);

  /* On a 64 bit machine, the event->data.l array members are 64 bits (long),
     but the x_property_data_to_lisp (or rather selection_data_to_lisp_data)
     function expects them to be of size int (i.e. 32).  So to be able to
     use that function, put the data in the form it expects if format is 32. */

  if (LONG_WIDTH > 32 && event->format == 32)
    {
      for (i = 0; i < 5; ++i) /* There are only 5 longs in a ClientMessage. */
	idata[i] = event->data.l[i];
      data = (unsigned char *) idata;
    }

  vec = make_nil_vector (4);
  ASET (vec, 0, SYMBOL_NAME (x_atom_to_symbol (FRAME_DISPLAY_INFO (f),
					       event->message_type)));
  ASET (vec, 1, frame);
  ASET (vec, 2, make_fixnum (event->format));
  ASET (vec, 3, x_property_data_to_lisp (f,
					 data,
					 event->message_type,
					 event->format,
					 size));

  if (!root_window_coords)
    x_relative_mouse_position (f, &x, &y);
  else
    x_translate_coordinates (f, root_x, root_y, &x, &y);

  bufp->kind = DRAG_N_DROP_EVENT;
  bufp->frame_or_window = frame;
  bufp->timestamp = CurrentTime;
  bufp->x = make_fixnum (x);
  bufp->y = make_fixnum (y);
  bufp->arg = vec;
  bufp->modifiers = 0;

  return true;
}

DEFUN ("x-send-client-message", Fx_send_client_message,
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
A value of nil means the currently selected frame.
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
are ignored.

Wait for the event to be sent and signal any error, unless
`x-fast-protocol-requests' is non-nil, in which case errors will be
silently ignored.  */)
  (Lisp_Object display, Lisp_Object dest, Lisp_Object from,
   Lisp_Object message_type, Lisp_Object format, Lisp_Object values)
{
  struct x_display_info *dpyinfo = check_x_display_info (display);

  CHECK_STRING (message_type);
  x_send_client_event (display, dest, from,
                       XInternAtom (dpyinfo->display,
                                    SSDATA (message_type),
                                    False),
                       format, values);

  return Qnil;
}

static void
x_send_client_event (Lisp_Object display, Lisp_Object dest, Lisp_Object from,
                     Atom message_type, Lisp_Object format, Lisp_Object values)
{
  struct x_display_info *dpyinfo = check_x_display_info (display);
  Window wdest;
  XEvent event;
  struct frame *f = decode_window_system_frame (from);
  bool to_root;

  CHECK_FIXNUM (format);
  CHECK_CONS (values);

  if (x_check_property_data (values) == -1)
    error ("Bad data in VALUES, must be number, cons or string");

  if (XFIXNUM (format) != 8 && XFIXNUM (format) != 16 && XFIXNUM (format) != 32)
    error ("FORMAT must be one of 8, 16 or 32");

  event.xclient.type = ClientMessage;
  event.xclient.format = XFIXNUM (format);

  if (FRAMEP (dest) || NILP (dest))
    {
      struct frame *fdest = decode_window_system_frame (dest);
      wdest = FRAME_OUTER_WINDOW (fdest);
    }
  else if (STRINGP (dest))
    {
      if (strcmp (SSDATA (dest), "PointerWindow") == 0)
        wdest = PointerWindow;
      else if (strcmp (SSDATA (dest), "InputFocus") == 0)
        wdest = InputFocus;
      else
        error ("DEST as a string must be one of PointerWindow or InputFocus");
    }
  else if (NUMBERP (dest) || CONSP (dest))
    CONS_TO_INTEGER (dest, Window, wdest);
  else
    error ("DEST must be a frame, nil, string, number or cons");

  if (wdest == 0) wdest = dpyinfo->root_window;
  to_root = wdest == dpyinfo->root_window;

  block_input ();

  event.xclient.send_event = True;
  event.xclient.serial = 0;
  event.xclient.message_type = message_type;
  event.xclient.display = dpyinfo->display;

  /* Some clients (metacity for example) expects sending window to be here
     when sending to the root window.  */
  event.xclient.window = to_root ? FRAME_OUTER_WINDOW (f) : wdest;

  memset (event.xclient.data.l, 0, sizeof (event.xclient.data.l));
  /* event.xclient.data can hold 20 chars, 10 shorts, or 5 longs.  */
  x_fill_property_data (dpyinfo->display, values, event.xclient.data.b,
                        5 * 32 / event.xclient.format,
                        event.xclient.format);

  /* If event mask is 0 the event is sent to the client that created
     the destination window.  But if we are sending to the root window,
     there is no such client.  Then we set the event mask to 0xffffff.  The
     event then goes to clients selecting for events on the root window.  */
  x_catch_errors_for_lisp (dpyinfo);
  {
    bool propagate = !to_root;
    long mask = to_root ? 0xffffff : 0;

    XSendEvent (dpyinfo->display, wdest, propagate, mask, &event);
    XFlush (dpyinfo->display);
  }
  x_check_errors_for_lisp (dpyinfo, "Failed to send client event: %s");
  x_uncatch_errors_for_lisp (dpyinfo);
  unblock_input ();
}



/* Return the timestamp where ownership of SELECTION was asserted, or
   nil if no local selection is present.  */

Lisp_Object
x_timestamp_for_selection (struct x_display_info *dpyinfo,
			   Lisp_Object selection)
{
  Lisp_Object value, local_value;

  local_value = LOCAL_SELECTION (selection, dpyinfo);

  if (NILP (local_value))
    return Qnil;

  value = XCAR (XCDR (XCDR (local_value)));

  return value;
}

static void syms_of_xselect_for_pdumper (void);

void
syms_of_xselect (void)
{
  defsubr (&Sx_get_selection_internal);
  defsubr (&Sx_own_selection_internal);
  defsubr (&Sx_disown_selection_internal);
  defsubr (&Sx_selection_owner_p);
  defsubr (&Sx_selection_exists_p);

  defsubr (&Sx_get_atom_name);
  defsubr (&Sx_send_client_message);
  defsubr (&Sx_register_dnd_atom);
  defsubr (&Sx_get_local_selection);

  reading_selection_reply = Fcons (Qnil, Qnil);
  staticpro (&reading_selection_reply);

  staticpro (&property_change_reply);

  /* FIXME: Duplicate definition in nsselect.c.  */
  DEFVAR_LISP ("selection-converter-alist", Vselection_converter_alist,
	       doc: /* An alist associating X Windows selection-types with functions.
These functions are called to convert the selection, with three args:
the name of the selection (typically `PRIMARY', `SECONDARY', or
`CLIPBOARD'); a desired type to which the selection should be
converted; and the local selection value (whatever was given to
`x-own-selection-internal').

On X Windows, the function can also be a cons of (PREDICATE
. FUNCTION), where PREDICATE determines whether or not the selection
type will appear in the list of selection types available to other
programs, and FUNCTION is the function which is actually called.
PREDICATE is called with the same arguments as FUNCTION, and should
return a non-nil value if the data type is to appear in that list.

The function should return the value to send to the X server
\(typically a string).  A return value of nil
means that the conversion could not be done.
A return value which is the symbol `NULL'
means that a side-effect was executed,
and there is no meaningful selection value.  */);
  Vselection_converter_alist = Qnil;

  DEFVAR_LISP ("x-lost-selection-functions", Vx_lost_selection_functions,
	       doc: /* A list of functions to be called when Emacs loses an X selection.
\(This happens when some other X client makes its own selection
or when a Lisp program explicitly clears the selection.)
The functions are called with one argument, the selection type
\(a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD').  */);
  Vx_lost_selection_functions = Qnil;

  DEFVAR_LISP ("x-sent-selection-functions", Vx_sent_selection_functions,
	       doc: /* A list of functions to be called when Emacs answers a selection request.
The functions are called with three arguments:
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

  DEFVAR_LISP ("x-select-enable-clipboard-manager",
	       Vx_select_enable_clipboard_manager,
	       doc: /* Whether to enable X clipboard manager support.
If non-nil, then whenever Emacs is killed or an Emacs frame is deleted
while owning the X clipboard, the clipboard contents are saved to the
clipboard manager if one is present.  */);
  Vx_select_enable_clipboard_manager = Qt;

  DEFVAR_INT ("x-selection-timeout", x_selection_timeout,
	      doc: /* Number of milliseconds to wait for a selection reply.
If the selection owner doesn't reply in this time, we give up.
A value of 0 means wait as long as necessary.  This is initialized from the
\"*selectionTimeout\" resource.  */);
  x_selection_timeout = 0;

  DEFVAR_LISP ("x-treat-local-requests-remotely", Vx_treat_local_requests_remotely,
    doc: /* Whether to treat local selection requests as remote ones.

If non-nil, selection converters for string types (`STRING',
`UTF8_STRING', `COMPOUND_TEXT', etc) will encode the strings, even
when Emacs itself is converting the selection.  */);
  Vx_treat_local_requests_remotely = Qnil;

  DEFVAR_LISP ("x-selection-alias-alist", Vx_selection_alias_alist,
    doc: /* List of selections to alias to another.
It should be an alist of a selection name to another.  When a
selection request arrives for the first selection, Emacs will respond
as if the request was meant for the other.

Note that this does not affect setting or owning selections.  */);
  Vx_selection_alias_alist = Qnil;

  /* QPRIMARY is defined in keyboard.c.  */
  DEFSYM (QSECONDARY, "SECONDARY");
  DEFSYM (QSTRING, "STRING");
  DEFSYM (QINTEGER, "INTEGER");
  DEFSYM (QCLIPBOARD, "CLIPBOARD");
  DEFSYM (QTIMESTAMP, "TIMESTAMP");
  DEFSYM (QTEXT, "TEXT");

  /* These are types of selection.  */
  DEFSYM (QCOMPOUND_TEXT, "COMPOUND_TEXT");
  DEFSYM (QUTF8_STRING, "UTF8_STRING");

  DEFSYM (QDELETE, "DELETE");
  DEFSYM (QMULTIPLE, "MULTIPLE");
  DEFSYM (QINCR, "INCR");
  DEFSYM (Q_EMACS_TMP_, "_EMACS_TMP_");
  DEFSYM (QTARGETS, "TARGETS");
  DEFSYM (QATOM, "ATOM");
  DEFSYM (QCLIPBOARD_MANAGER, "CLIPBOARD_MANAGER");
  DEFSYM (QSAVE_TARGETS, "SAVE_TARGETS");
  DEFSYM (QNULL, "NULL");
  DEFSYM (QXdndDirectSave0, "XdndDirectSave0");
  DEFSYM (QXdndActionDirectSave, "XdndActionDirectSave");
  DEFSYM (Qtext_plain, "text/plain");
  DEFSYM (Qforeign_selection, "foreign-selection");
  DEFSYM (Qx_lost_selection_functions, "x-lost-selection-functions");
  DEFSYM (Qx_sent_selection_functions, "x-sent-selection-functions");

  DEFSYM (QXmTRANSFER_SUCCESS, "XmTRANSFER_SUCCESS");
  DEFSYM (QXmTRANSFER_FAILURE, "XmTRANSFER_FAILURE");

  pdumper_do_now_and_after_load (syms_of_xselect_for_pdumper);
}

static void
syms_of_xselect_for_pdumper (void)
{
  reading_selection_window = 0;
  reading_which_selection = 0;
  property_change_wait_list = 0;
  prop_location_identifier = 0;
  property_change_reply = Fcons (Qnil, Qnil);
}
