/* Gtk selection processing for emacs.
   Copyright (C) 1993-1994, 2005-2006, 2008-2026 Free Software
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
#include "pgtkterm.h"
#include "termhooks.h"
#include "keyboard.h"
#include "atimer.h"
#include "blockinput.h"

/* This file deliberately does not implement INCR, since it adds a
   bunch of extra code for no real gain, as PGTK isn't supposed to
   support X11 anyway.  */

/* Advance declaration of structs.  */
struct selection_data;
struct prop_location;

static void pgtk_decline_selection_request (struct selection_input_event *);
static bool pgtk_convert_selection (Lisp_Object, Lisp_Object, GdkAtom, bool,
				    struct pgtk_display_info *);
static bool waiting_for_other_props_on_window (GdkDisplay *, GdkWindow *);
#if 0
static struct prop_location *expect_property_change (GdkDisplay *, GdkWindow *,
                                                     GdkAtom, int);
#endif
static void unexpect_property_change (struct prop_location *);
static void wait_for_property_change (struct prop_location *);
static Lisp_Object pgtk_get_window_property_as_lisp_data (struct pgtk_display_info *,
							  GdkWindow *, GdkAtom,
							  Lisp_Object, GdkAtom, bool);
static Lisp_Object selection_data_to_lisp_data (struct pgtk_display_info *,
						const unsigned char *,
						ptrdiff_t, GdkAtom, int);
static void lisp_data_to_selection_data (struct pgtk_display_info *, Lisp_Object,
					 struct selection_data *);
static Lisp_Object pgtk_get_local_selection (Lisp_Object, Lisp_Object,
					     bool, struct pgtk_display_info *);

/* Allocate an array of NITEMS items, each of positive size ITEM_SIZE.
   Make room for an extra byte at the end, as GDK sometimes needs that.  */

static void *
pgtk_nalloc (ptrdiff_t nitems, ptrdiff_t item_size)
{
  /* To pacify gcc --Wanalyzer-allocation-size, make room for an extra
     item at the end instead of just the extra byte GDK sometimes needs.  */
  return xnmalloc (nitems + 1, item_size);
}

/* From a Lisp_Object, return a suitable frame for selection
   operations.  OBJECT may be a frame, a terminal object, or nil
   (which stands for the selected frame--or, if that is not an pgtk
   frame, the first pgtk display on the list).  If no suitable frame can
   be found, return NULL.  */

static struct frame *
frame_for_pgtk_selection (Lisp_Object object)
{
  Lisp_Object tail, frame;
  struct frame *f;

  if (NILP (object))
    {
      f = XFRAME (selected_frame);
      if (FRAME_PGTK_P (f) && FRAME_LIVE_P (f))
	return f;

      FOR_EACH_FRAME (tail, frame)
      {
	f = XFRAME (frame);
	if (FRAME_PGTK_P (f) && FRAME_LIVE_P (f))
	  return f;
      }
    }
  else if (TERMINALP (object))
    {
      struct terminal *t = decode_live_terminal (object);

      if (t->type == output_pgtk)
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
      if (FRAME_PGTK_P (f) && FRAME_LIVE_P (f))
	return f;
    }

  return NULL;
}

#define LOCAL_SELECTION(selection_symbol, dpyinfo)			\
  assq_no_quit (selection_symbol, dpyinfo->terminal->Vselection_alist)

static GdkAtom
symbol_to_gdk_atom (Lisp_Object sym)
{
  if (NILP (sym))
    return GDK_NONE;

  if (EQ (sym, QPRIMARY))
    return GDK_SELECTION_PRIMARY;
  if (EQ (sym, QSECONDARY))
    return GDK_SELECTION_SECONDARY;
  if (EQ (sym, QCLIPBOARD))
    return GDK_SELECTION_CLIPBOARD;

  if (!SYMBOLP (sym))
    emacs_abort ();

  return gdk_atom_intern (SSDATA (SYMBOL_NAME (sym)), FALSE);
}

static Lisp_Object
gdk_atom_to_symbol (GdkAtom atom)
{
  return intern (gdk_atom_name (atom));
}



/* Do protocol to assert ourself as a selection owner.
   FRAME shall be the owner; it must be a valid GDK frame.
   Update the Vselection_alist so that we can reply to later requests for
   our selection.  */

static void
pgtk_own_selection (Lisp_Object selection_name, Lisp_Object selection_value,
		    Lisp_Object frame)
{
  struct frame *f = XFRAME (frame);
  struct pgtk_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  guint32 timestamp = gtk_get_current_event_time ();
  GdkAtom selection_atom = symbol_to_gdk_atom (selection_name);
  Lisp_Object targets;
  ptrdiff_t i, ntargets;
  GtkTargetEntry *gtargets;

  if (timestamp == GDK_CURRENT_TIME)
    timestamp = dpyinfo->last_user_time;

  /* Assert ownership over the selection.  Ideally we would use only
     the GDK selection API for this, but it just doesn't work on
     Wayland.  */

  if (!gdk_selection_owner_set_for_display (dpyinfo->display,
					    FRAME_GDK_WINDOW (f),
					    selection_atom,
					    timestamp, TRUE))
    signal_error ("Could not assert ownership over selection", selection_name);

  /* Update the local cache */
  {
    Lisp_Object selection_data;
    Lisp_Object prev_value;

    selection_data = list4 (selection_name, selection_value,
			    INT_TO_INTEGER (timestamp), frame);
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
	  if (EQ (prev_value, Fcar (XCDR (rest))))
	    {
	      XSETCDR (rest, XCDR (XCDR (rest)));
	      break;
	    }
      }
  }

  /* Announce the targets to the display server.  This isn't required
     on X, but is on Wayland.  */

  targets = pgtk_get_local_selection (selection_name, QTARGETS,
				      true, dpyinfo);

  /* GC must not happen inside this segment.  */
  block_input ();
  gtk_selection_clear_targets (FRAME_GTK_WIDGET (f), selection_atom);

  if (VECTORP (targets))
    {
      gtargets = xzalloc (sizeof *gtargets * ASIZE (targets));
      ntargets = 0;

      for (i = 0; i < ASIZE (targets); ++i)
	{
	  if (SYMBOLP (AREF (targets, i)))
	    gtargets[ntargets++].target
	      = SSDATA (SYMBOL_NAME (AREF (targets, i)));
	}

      gtk_selection_add_targets (FRAME_GTK_WIDGET (f),
				 selection_atom, gtargets,
				 ntargets);

      xfree (gtargets);
    }
  unblock_input ();
}

static Lisp_Object
pgtk_get_local_selection (Lisp_Object selection_symbol, Lisp_Object target_type,
			  bool local_request, struct pgtk_display_info *dpyinfo)
{
  Lisp_Object local_value, tem;
  Lisp_Object handler_fn, value, check;

  local_value = LOCAL_SELECTION (selection_symbol, dpyinfo);

  if (NILP (local_value)) return Qnil;

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
      specpdl_ref count = SPECPDL_INDEX ();
      specbind (Qinhibit_quit, Qt);

      CHECK_SYMBOL (target_type);
      handler_fn = Fcdr (Fassq (target_type, Vselection_converter_alist));

      if (CONSP (handler_fn))
	handler_fn = XCDR (handler_fn);

      tem = XCAR (XCDR (local_value));

      if (STRINGP (tem))
	{
	  local_value = Fget_text_property (make_fixnum (0),
					    target_type, tem);

	  if (!NILP (local_value))
	    tem = local_value;
	}

      if (!NILP (handler_fn))
	value = calln (handler_fn, selection_symbol,
		       (local_request ? Qnil : target_type),
		       tem);
      else
	value = Qnil;
      value = unbind_to (count, value);
    }

  /* Make sure this value is of a type that we could transmit
     to another client.  */

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

static void
pgtk_decline_selection_request (struct selection_input_event *event)
{
  gdk_selection_send_notify (SELECTION_EVENT_REQUESTOR (event),
			     SELECTION_EVENT_SELECTION (event),
			     SELECTION_EVENT_TARGET (event),
			     GDK_NONE, SELECTION_EVENT_TIME (event));
}

struct selection_data
{
  unsigned char *data;
  ptrdiff_t size;
  int format;
  GdkAtom type;
  bool nofree;
  GdkAtom property;

  /* This can be set to non-NULL during x_reply_selection_request, if
     the selection is waiting for an INCR transfer to complete.  Don't
     free these; that's done by unexpect_property_change.  */
  struct prop_location *wait_object;
  struct selection_data *next;
};

struct pgtk_selection_request
{
  /* The last element in this stack.  */
  struct pgtk_selection_request *last;

  /* Its display info.  */
  struct pgtk_display_info *dpyinfo;

  /* Its selection input event.  */
  struct selection_input_event *request;

  /* Linked list of the above (in support of MULTIPLE targets).  */
  struct selection_data *converted_selections;

  /* "Data" to send a requestor for a failed MULTIPLE subtarget.  */
  GdkAtom conversion_fail_tag;

  /* Whether or not conversion was successful.  */
  bool converted;
};

/* Stack of selections currently being processed.
   NULL if all requests have been fully processed.  */

static struct pgtk_selection_request *selection_request_stack;

static void
pgtk_push_current_selection_request (struct selection_input_event *se,
				     struct pgtk_display_info *dpyinfo)
{
  struct pgtk_selection_request *frame;

  frame = xmalloc (sizeof *frame);
  frame->converted = false;
  frame->last = selection_request_stack;
  frame->request = se;
  frame->dpyinfo = dpyinfo;
  frame->converted_selections = NULL;
  frame->conversion_fail_tag = GDK_NONE;

  selection_request_stack = frame;
}

static void
pgtk_pop_current_selection_request (void)
{
  struct pgtk_selection_request *tem;

  tem = selection_request_stack;
  selection_request_stack = selection_request_stack->last;

  xfree (tem);
}

/* Used as an unwind-protect clause so that, if a selection-converter signals
   an error, we tell the requestor that we were unable to do what they wanted
   before we throw to top-level or go into the debugger or whatever.  */

static void
pgtk_selection_request_lisp_error (void)
{
  struct selection_data *cs, *next;
  struct pgtk_selection_request *frame;

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
    pgtk_decline_selection_request (frame->request);
}

/* This stuff is so that INCR selections are reentrant (that is, so we can
   be servicing multiple INCR selection requests simultaneously.)  I haven't
   actually tested that yet.  */

/* Keep a list of the property changes that are awaited.  */

struct prop_location
{
  int identifier;
  GdkDisplay *display;
  GdkWindow *window;
  GdkAtom property;
  int desired_state;
  bool arrived;
  struct prop_location *next;
};

#if 0

static int prop_location_identifier;

#endif

static Lisp_Object property_change_reply;

static struct prop_location *property_change_reply_object;

static struct prop_location *property_change_wait_list;

static void
set_property_change_object (struct prop_location *location)
{
  /* Input must be blocked so we don't get the event before we set these.  */
  if (!input_blocked_p ())
    emacs_abort ();

  XSETCAR (property_change_reply, Qnil);
  property_change_reply_object = location;
}


/* Send the reply to a selection request event EVENT.  */

static void
pgtk_reply_selection_request (struct selection_input_event *event,
			      struct pgtk_display_info *dpyinfo)
{
  GdkDisplay *display = SELECTION_EVENT_DISPLAY (event);
  GdkWindow *window = SELECTION_EVENT_REQUESTOR (event);
  struct selection_data *cs;
  struct pgtk_selection_request *frame;

  frame = selection_request_stack;

  block_input ();
  /* Loop over converted selections, storing them in the requested
     properties.  If data is large, only store the first N bytes
     (section 2.7.2 of ICCCM).  Note that we store the data for a
     MULTIPLE request in the opposite order; the ICCM says only that
     the conversion itself must be done in the same order. */
  for (cs = frame->converted_selections; cs; cs = cs->next)
    {
      if (cs->property == GDK_NONE)
	continue;

      gdk_property_change (window, cs->property,
			   cs->type, cs->format,
			   GDK_PROP_MODE_APPEND,
			   cs->data, cs->size);
    }

  /* Now issue the SelectionNotify event.  */
  gdk_selection_send_notify (window,
			     SELECTION_EVENT_SELECTION (event),
			     SELECTION_EVENT_TARGET (event),
			     SELECTION_EVENT_PROPERTY (event),
			     SELECTION_EVENT_TIME (event));
  gdk_display_flush (display);

  /* Finish sending the rest of each of the INCR values.  This should
     be improved; there's a chance of deadlock if more than one
     subtarget in a MULTIPLE selection requires an INCR transfer, and
     the requestor and Emacs loop waiting on different transfers.  */
  for (cs = frame->converted_selections; cs; cs = cs->next)
    if (cs->wait_object)
      {
        /* Must set this inside block_input ().  unblock_input may read
           events and setting property_change_reply in
           wait_for_property_change is then too late.  */
        set_property_change_object (cs->wait_object);
	unblock_input ();

	/* Wait for the requestor to ack by deleting the property.
	   This can run Lisp code (process handlers) or signal.  */
	wait_for_property_change (cs->wait_object);

	/* Now write a zero-length chunk to the property to tell the
	   requestor that we're done.  */
	block_input ();
	if (! waiting_for_other_props_on_window (display, window))
	  gdk_window_set_events (window, 0);
	gdk_property_change (window, cs->property, cs->type, cs->format,
			     GDK_PROP_MODE_REPLACE, cs->data, 0);
      }

  gdk_display_sync (display);
  unblock_input ();
}



/* Handle a SelectionRequest event EVENT.
   This is called from keyboard.c when such an event is found in the queue.  */

static void
pgtk_handle_selection_request (struct selection_input_event *event)
{
  guint32 local_selection_time;
  struct pgtk_display_info *dpyinfo = SELECTION_EVENT_DPYINFO (event);
  GdkAtom selection = SELECTION_EVENT_SELECTION (event);
  Lisp_Object selection_symbol = gdk_atom_to_symbol (selection);
  GdkAtom target = SELECTION_EVENT_TARGET (event);
  Lisp_Object target_symbol = gdk_atom_to_symbol (target);
  GdkAtom property = SELECTION_EVENT_PROPERTY (event);
  Lisp_Object local_selection_data;
  bool success = false;
  specpdl_ref count = SPECPDL_INDEX ();
  bool pushed;
  Lisp_Object alias, tem;

  alias = Vpgtk_selection_alias_alist;

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
    goto DONE;

  local_selection_data = LOCAL_SELECTION (selection_symbol, dpyinfo);

  /* Decline if we don't own any selections.  */
  if (NILP (local_selection_data)) goto DONE;

  /* Decline requests issued prior to our acquiring the selection.  */
  CONS_TO_INTEGER (XCAR (XCDR (XCDR (local_selection_data))),
		   guint32, local_selection_time);
  if (SELECTION_EVENT_TIME (event) != GDK_CURRENT_TIME
      && local_selection_time > SELECTION_EVENT_TIME (event))
    goto DONE;

  block_input ();
  pushed = true;
  pgtk_push_current_selection_request (event, dpyinfo);
  record_unwind_protect_void (pgtk_pop_current_selection_request);
  record_unwind_protect_void (pgtk_selection_request_lisp_error);
  unblock_input ();

  if (EQ (target_symbol, QMULTIPLE))
    {
      /* For MULTIPLE targets, the event property names a list of atom
	 pairs; the first atom names a target and the second names a
	 non-GDK_NONE property.  */
      GdkWindow *requestor = SELECTION_EVENT_REQUESTOR (event);
      Lisp_Object multprop;
      ptrdiff_t j, nselections;
      struct selection_data cs;

      if (property == GDK_NONE)
	goto DONE;

      multprop = pgtk_get_window_property_as_lisp_data (dpyinfo,
							requestor,
							property,
							QMULTIPLE,
							selection,
							true);

      if (!VECTORP (multprop) || ASIZE (multprop) % 2)
	goto DONE;

      nselections = ASIZE (multprop) / 2;
      /* Perform conversions.  This can signal.  */
      for (j = 0; j < nselections; j++)
	{
	  Lisp_Object subtarget = AREF (multprop, 2*j);
	  GdkAtom subproperty = symbol_to_gdk_atom (AREF (multprop, 2 * j + 1));
	  bool subsuccess = false;

	  if (subproperty != GDK_NONE)
	    subsuccess = pgtk_convert_selection (selection_symbol, subtarget,
						 subproperty, true, dpyinfo);
	  if (!subsuccess)
	    ASET (multprop, 2*j+1, Qnil);
	}
      /* Save conversion results */
      lisp_data_to_selection_data (dpyinfo, multprop, &cs);
      gdk_property_change (requestor, property,
			   cs.type, cs.format,
			   GDK_PROP_MODE_REPLACE,
			   cs.data, cs.size);
      success = true;
    }
  else
    {
      if (property == GDK_NONE)
	property = SELECTION_EVENT_TARGET (event);

      success = pgtk_convert_selection (selection_symbol,
					target_symbol, property,
					false, dpyinfo);
    }

 DONE:

  if (pushed)
    selection_request_stack->converted = true;

  if (success)
    pgtk_reply_selection_request (event, dpyinfo);
  else
    pgtk_decline_selection_request (event);

  /* Run the `pgtk-sent-selection-functions' abnormal hook.  */
  if (!NILP (Vpgtk_sent_selection_functions)
      && !BASE_EQ (Vpgtk_sent_selection_functions, Qunbound))
    CALLN (Frun_hook_with_args, Qpgtk_sent_selection_functions,
	   selection_symbol, target_symbol, success ? Qt : Qnil);

  unbind_to (count, Qnil);
}

/* Perform the requested selection conversion, and write the data to
   the converted_selections linked list, where it can be accessed by
   x_reply_selection_request.  If FOR_MULTIPLE, write out
   the data even if conversion fails, using conversion_fail_tag.

   Return true if (and only if) successful.  */

static bool
pgtk_convert_selection (Lisp_Object selection_symbol,
			Lisp_Object target_symbol, GdkAtom property,
			bool for_multiple, struct pgtk_display_info *dpyinfo)
{
  Lisp_Object lisp_selection;
  struct selection_data *cs;
  struct pgtk_selection_request *frame;

  lisp_selection
    = pgtk_get_local_selection (selection_symbol, target_symbol,
				false, dpyinfo);

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
	  cs->type = GDK_SELECTION_TYPE_ATOM;
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
pgtk_handle_selection_clear (struct selection_input_event *event)
{
  GdkAtom selection = SELECTION_EVENT_SELECTION (event);
  guint32 changed_owner_time = SELECTION_EVENT_TIME (event);

  Lisp_Object selection_symbol, local_selection_data;
  guint32 local_selection_time;
  struct pgtk_display_info *dpyinfo = SELECTION_EVENT_DPYINFO (event);
  Lisp_Object Vselection_alist;

  if (!dpyinfo) return;

  selection_symbol = gdk_atom_to_symbol (selection);
  local_selection_data = LOCAL_SELECTION (selection_symbol, dpyinfo);

  /* Well, we already believe that we don't own it, so that's just fine.  */
  if (NILP (local_selection_data)) return;

  CONS_TO_INTEGER (XCAR (XCDR (XCDR (local_selection_data))),
		   guint32, local_selection_time);

  /* We have reasserted the selection since this SelectionClear was
     generated, so we can disregard it.  */
  if (changed_owner_time != GDK_CURRENT_TIME
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

  /* Run the `pgtk-lost-selection-functions' abnormal hook.  */
  CALLN (Frun_hook_with_args, Qpgtk_lost_selection_functions, selection_symbol);

  redisplay_preserve_echo_area (20);
}

void
pgtk_handle_selection_event (struct selection_input_event *event)
{
  if (event->kind != SELECTION_REQUEST_EVENT)
    pgtk_handle_selection_clear (event);
  else
    pgtk_handle_selection_request (event);
}

/* Clear all selections that were made from frame F.
   We do this when about to delete a frame.  */

void
pgtk_clear_frame_selections (struct frame *f)
{
  Lisp_Object frame, rest, timestamp, symbol;
  guint32 time;
  struct pgtk_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  struct terminal *t = dpyinfo->terminal;

  XSETFRAME (frame, f);

  /* Delete elements from the beginning of Vselection_alist.  */
  while (CONSP (t->Vselection_alist)
	 && EQ (frame, XCAR (XCDR (XCDR (XCDR (XCAR (t->Vselection_alist)))))))
    {
      symbol = Fcar (Fcar (t->Vselection_alist));

      /* Run the `pgtk-lost-selection-functions' abnormal hook.  */
      CALLN (Frun_hook_with_args, Qpgtk_lost_selection_functions,
	     symbol);

      timestamp = Fcar (Fcdr (Fcdr (Fcar (t->Vselection_alist))));
      CONS_TO_INTEGER (timestamp, guint32, time);

      /* On Wayland, GDK will still ask the (now non-existent) frame for
	 selection data, even though we no longer think the selection is
	 owned by us.  Manually relinquish ownership of the selection.  */
      gdk_selection_owner_set_for_display (dpyinfo->display,
					   NULL,
					   symbol_to_gdk_atom (symbol),
					   time, TRUE);

      tset_selection_alist (t, XCDR (t->Vselection_alist));
    }

  /* Delete elements after the beginning of Vselection_alist.  */
  for (rest = t->Vselection_alist; CONSP (rest); rest = XCDR (rest))
    if (CONSP (XCDR (rest))
	&& EQ (frame, XCAR (XCDR (XCDR (XCDR (XCAR (XCDR (rest))))))))
      {
	symbol = XCAR (XCAR (XCDR (rest)));
	CALLN (Frun_hook_with_args, Qpgtk_lost_selection_functions,
	       symbol);

	timestamp = XCAR (XCDR (XCDR (XCAR (XCDR (rest)))));
	CONS_TO_INTEGER (timestamp, guint32, time);

	gdk_selection_owner_set_for_display (dpyinfo->display,
					     NULL,
					     symbol_to_gdk_atom (symbol),
					     time, TRUE);

	XSETCDR (rest, XCDR (XCDR (rest)));
	break;
      }
}

/* True if any properties for DISPLAY and WINDOW
   are on the list of what we are waiting for.  */

static bool
waiting_for_other_props_on_window (GdkDisplay *display, GdkWindow *window)
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

/* Currently unused -- uncomment later if we decide to implement INCR
   transfer for X.  */

#if 0

static struct prop_location *
expect_property_change (GdkDisplay *display, GdkWindow *window,
                        GdkAtom property, int state)
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

#endif

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
      intmax_t timeout = max (0, pgtk_selection_timeout);
      intmax_t secs = timeout / 1000;
      int nsecs = (timeout % 1000) * 1000000;

      wait_reading_process_output (secs, nsecs, 0, false,
				   property_change_reply, NULL, 0);

      if (NILP (XCAR (property_change_reply)))
	error ("Timed out waiting for property-notify event");
    }

  unbind_to (count, Qnil);
}

/* Called from the big filter in response to a PropertyNotify
   event.  */

void
pgtk_handle_property_notify (GdkEventProperty *event)
{
  struct prop_location *rest;
  GdkDisplay *dpy;

  dpy = gdk_window_get_display (event->window);

  for (rest = property_change_wait_list; rest; rest = rest->next)
    {
      if (!rest->arrived
	  && rest->property == event->atom
	  && rest->window == event->window
	  && rest->display == dpy
	  && rest->desired_state == event->state)
	{
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
pgtk_display_selection_waiting_message (struct atimer *timer)
{
  Lisp_Object val;

  val = build_string ("Waiting for reply from selection owner...");
  message3_nolog (val);
}

static void
pgtk_cancel_atimer (void *atimer)
{
  cancel_atimer (atimer);
}


/* Variables for communication with pgtk_handle_selection_notify.  */
static GdkAtom reading_which_selection;
static Lisp_Object reading_selection_reply;
static GdkWindow *reading_selection_window;

/* Do protocol to read selection-data from the window server.
   Converts this to Lisp data and returns it.
   FRAME is the frame whose window shall request the selection.  */

static Lisp_Object
pgtk_get_foreign_selection (Lisp_Object selection_symbol, Lisp_Object target_type,
			    Lisp_Object time_stamp, Lisp_Object frame)
{
  struct frame *f = XFRAME (frame);
  struct pgtk_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  GdkWindow *requestor_window = FRAME_GDK_WINDOW (f);
  guint32 requestor_time = dpyinfo->last_user_time;
  GdkAtom selection_atom = symbol_to_gdk_atom (selection_symbol);
  GdkAtom type_atom = (CONSP (target_type)
		       ? symbol_to_gdk_atom (XCAR (target_type))
		       : symbol_to_gdk_atom (target_type));
  struct atimer *delayed_message;
  struct timespec message_interval;
  specpdl_ref count;

  count = SPECPDL_INDEX ();

  if (!FRAME_LIVE_P (f))
    return unbind_to (count, Qnil);

  if (!NILP (time_stamp))
    CONS_TO_INTEGER (time_stamp, guint32, requestor_time);

  block_input ();
  /* Prepare to block until the reply has been read.  */
  reading_selection_window = requestor_window;
  reading_which_selection = selection_atom;
  XSETCAR (reading_selection_reply, Qnil);

  gdk_selection_convert (requestor_window, selection_atom,
			 type_atom, requestor_time);
  unblock_input ();

  /* It should not be necessary to stop handling selection requests
     during this time.  In fact, the SAVE_TARGETS mechanism requires
     us to handle a clipboard manager's requests before it returns
     GDK_SELECTION_NOTIFY. */

  message_interval = make_timespec (1, 0);
  delayed_message = start_atimer (ATIMER_RELATIVE, message_interval,
				  pgtk_display_selection_waiting_message,
				  NULL);
  record_unwind_protect_ptr (pgtk_cancel_atimer, delayed_message);

  /* This allows quits.  Also, don't wait forever.  */
  intmax_t timeout = max (0, pgtk_selection_timeout);
  intmax_t secs = timeout / 1000;
  int nsecs = (timeout % 1000) * 1000000;

  wait_reading_process_output (secs, nsecs, 0, false,
			       reading_selection_reply, NULL, 0);

  if (NILP (XCAR (reading_selection_reply)))
    error ("Timed out waiting for reply from selection owner");
  if (EQ (XCAR (reading_selection_reply), Qlambda))
    return unbind_to (count, Qnil);

  /* Otherwise, the selection is waiting for us on the requested property.  */
  return unbind_to (count,
		    pgtk_get_window_property_as_lisp_data (dpyinfo,
							   requestor_window,
							   GDK_NONE,
							   target_type,
							   selection_atom,
							   false));
}

/* Subroutines of pgtk_get_window_property_as_lisp_data */

static ptrdiff_t
pgtk_size_for_format (gint format)
{
  switch (format)
    {
    case 8:
      return sizeof (unsigned char);
    case 16:
      return sizeof (unsigned short);
    case 32:
      return sizeof (unsigned long);

    default:
      emacs_abort ();
    }
}

/* Use xfree, not g_free, to free the data obtained with this function.  */

static void
pgtk_get_window_property (GdkWindow *window, unsigned char **data_ret,
			  ptrdiff_t *bytes_ret, GdkAtom *actual_type_ret,
			  int *actual_format_ret, unsigned long *actual_size_ret)
{
  gint length, actual_format;
  unsigned char *data;
  ptrdiff_t element_size;
  void *xdata;
  GdkAtom actual_type;
  unsigned long i;
  unsigned int *idata;
  unsigned long *ldata;

  data = NULL;

  length = gdk_selection_property_get (window, &data,
				       &actual_type,
				       &actual_format);

  if (!data)
    {
      *data_ret = NULL;
      *actual_type_ret = GDK_NONE;
      *bytes_ret = 0;
      *actual_format_ret = 8;
      *actual_size_ret = 0;

      return;
    }

  if (actual_type == GDK_SELECTION_TYPE_ATOM
      || actual_type == gdk_atom_intern_static_string ("ATOM_PAIR"))
    {
      /* GDK should not allow anything else.  */
      eassert (actual_format == 32);

      length = length / sizeof (GdkAtom);
      xdata = pgtk_nalloc (length, sizeof (GdkAtom));
      memcpy (xdata, data, 1 + length * sizeof (GdkAtom));

      g_free (data);

      *data_ret = xdata;
      *actual_type_ret = actual_type;
      *bytes_ret = length * sizeof (GdkAtom);
      *actual_format_ret = 32;
      *actual_size_ret = length;

      return;
    }

  element_size = pgtk_size_for_format (actual_format);
  length = length / element_size;
  xdata = pgtk_nalloc (length, element_size);
  memcpy (xdata, data, 1 + element_size * length);

  if (actual_format == 32 && LONG_WIDTH > 32)
    {
      ldata = (typeof (ldata)) data;
      idata = xdata;

      for (i = 0; i < length; ++i)
	idata[i] = ldata[i];

      /* There is always enough space in idata.  */
      idata[length] = 0;
      *bytes_ret = sizeof *idata * length;
    }
  else
    /* I think GDK itself prevents element_size from exceeding the
       length at which this computation fails.  */
    *bytes_ret = element_size * length;

  /* Now free the original `data' allocated by GDK.  */
  g_free (data);

  *data_ret = xdata;
  *actual_type_ret = GDK_NONE;
  *actual_size_ret = length;
  *actual_format_ret = actual_format;
  *actual_type_ret = actual_type;
}

static Lisp_Object
pgtk_get_window_property_as_lisp_data (struct pgtk_display_info *dpyinfo,
				       GdkWindow *window, GdkAtom property,
				       Lisp_Object target_type, GdkAtom selection_atom,
				       bool for_multiple)
{
  GdkAtom actual_type;
  int actual_format;
  unsigned long actual_size;
  unsigned char *data = 0;
  ptrdiff_t bytes = 0;
  Lisp_Object val;
  GdkDisplay *display = dpyinfo->display;

  pgtk_get_window_property (window, &data, &bytes,
			    &actual_type, &actual_format,
			    &actual_size);

  if (!data)
    {
      if (for_multiple)
	return Qnil;

      if (gdk_selection_owner_get_for_display (display, selection_atom))
	{
	  AUTO_STRING (format, "Selection owner couldn't convert: %s");
	  CALLN (Fmessage, format,
		 actual_type
		 ? list2 (target_type,
			  gdk_atom_to_symbol (actual_type))
		 : target_type);
	  return Qnil;
	}
      else
	{
	  AUTO_STRING (format, "No selection: %s");
	  CALLN (Fmessage, format,
		 gdk_atom_to_symbol (selection_atom));
	  return Qnil;
	}
    }

  if (!for_multiple && property != GDK_NONE)
    gdk_property_delete (window, property);

  /* It's been read.  Now convert it to a lisp object in some semi-rational
     manner.  */
  val = selection_data_to_lisp_data (dpyinfo, data, bytes,
				     actual_type, actual_format);

  /* Use xfree, not g_free, because pgtk_get_window_property calls
     xmalloc itself.  */
  xfree (data);
  return val;
}



/* These functions convert from the selection data read from the
   server into something that we can use from Lisp, and vice versa.

	Type:	Format:	Size:		Lisp Type:
	-----	-------	-----		-----------
	*	8	*		String
	ATOM	32	1		Symbol
	ATOM	32	> 1		Vector of Symbols
	*	16	1		Integer
	*	16	> 1		Vector of Integers
	*	32	1		Integer
	*	32	> 1		Vector of the above

   When converting an object to C, it may be of the form (SYMBOL
   . <data>) where SYMBOL is what we should claim that the type is.
   Format and representation are as above.

   Important: When format is 32, data should contain an array of int,
   not an array of long as GDK returns.  Unless TYPE is also
   GDK_SELECTION_TYPE_ATOM, in which case data should be an array of
   GdkAtom.  This makes a difference when sizeof (long) != sizeof
   (int).  */

static Lisp_Object
selection_data_to_lisp_data (struct pgtk_display_info *dpyinfo,
			     const unsigned char *data,
			     ptrdiff_t size, GdkAtom type, int format)
{
  if (type == gdk_atom_intern_static_string ("NULL"))
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
      if (type == gdk_atom_intern_static_string ("COMPOUND_TEXT"))
	lispy_type = QCOMPOUND_TEXT;
      else if (type == gdk_atom_intern_static_string ("UTF8_STRING"))
	lispy_type = QUTF8_STRING;
      else
	lispy_type = QSTRING;

      Fput_text_property (make_fixnum (0), make_fixnum (size),
			  Qforeign_selection, lispy_type, str);
      return str;
    }
  /* Convert a single atom to a Lisp_Symbol.  Convert a set of atoms to
     a vector of symbols.  */
  else if (format == 32
	   && (type == GDK_SELECTION_TYPE_ATOM
	       /* Treat ATOM_PAIR type similar to list of atoms.  */
	       || type == gdk_atom_intern_static_string ("ATOM_PAIR")))
    {
      ptrdiff_t i;
      GdkAtom *idata = (GdkAtom *) data;

      if (size == sizeof (GdkAtom))
	return gdk_atom_to_symbol (idata[0]);
      else
	{
	  Lisp_Object v = make_nil_vector (size / sizeof (GdkAtom));

	  for (i = 0; i < size / sizeof (GdkAtom); i++)
	    ASET (v, i, gdk_atom_to_symbol (idata[i]));
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
      if (type == GDK_SELECTION_TYPE_INTEGER)
        return INT_TO_INTEGER (((int *) data) [0]);
      else
        return INT_TO_INTEGER (((unsigned int *) data) [0]);
    }
  else if (format == 16 && size == sizeof (short))
    {
      if (type == GDK_SELECTION_TYPE_INTEGER)
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

      if (type == GDK_SELECTION_TYPE_INTEGER)
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
      Lisp_Object v = make_nil_vector (size / sizeof (gint));

      if (type == GDK_SELECTION_TYPE_INTEGER)
        {
          for (i = 0; i < size / sizeof (gint); i++)
            {
              int j = ((gint *) data) [i];
              ASET (v, i, INT_TO_INTEGER (j));
            }
        }
      else
        {
          for (i = 0; i < size / sizeof (gint); i++)
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
cons_to_gdk_long (Lisp_Object obj)
{
  if (G_MAXUINT32 <= INTMAX_MAX
      || NILP (Fnatnump (CONSP (obj) ? XCAR (obj) : obj)))
    return cons_to_signed (obj, 0, min (G_MAXUINT32, INTMAX_MAX));
  else
    return cons_to_unsigned (obj, G_MAXUINT32);
}

/* Use xfree, not XFree, to free the data obtained with this function.  */

static void
lisp_data_to_selection_data (struct pgtk_display_info *dpyinfo,
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
      void *data = pgtk_nalloc (1, sizeof (GdkAtom));
      GdkAtom *x_atom_ptr = data;
      cs->data = data;
      cs->format = 32;
      cs->size = 1;
      cs->data[sizeof (GdkAtom)] = 0;
      *x_atom_ptr = symbol_to_gdk_atom (obj);
      if (NILP (type)) type = QATOM;
    }
  else if (RANGED_FIXNUMP (SHRT_MIN, obj, SHRT_MAX))
    {
      void *data = pgtk_nalloc (1, sizeof (short));
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
      void *data = pgtk_nalloc (1, sizeof (unsigned long));
      unsigned long *x_long_ptr = data;
      cs->data = data;
      cs->format = 32;
      cs->size = 1;
      cs->data[sizeof (unsigned long)] = 0;
      *x_long_ptr = cons_to_gdk_long (obj);
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

      if (SYMBOLP (AREF (obj, 0)))
	/* This vector is an ATOM set */
	{
	  void *data;
	  GdkAtom *x_atoms;
	  if (NILP (type)) type = QATOM;
	  for (i = 0; i < size; i++)
	    if (!SYMBOLP (AREF (obj, i)))
	      signal_error ("All elements of selection vector must have same type", obj);

	  cs->data = data = xnmalloc (size, sizeof *x_atoms);
	  x_atoms = data;
	  cs->format = 32;
	  cs->size = size;
	  for (i = 0; i < size; i++)
	    x_atoms[i] = symbol_to_gdk_atom (AREF (obj, i));
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
	      if (! RANGED_FIXNUMP (SHRT_MIN, AREF (obj, i), SHRT_MAX))
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
		x_atoms[i] = cons_to_gdk_long (AREF (obj, i));
	      else
		shorts[i] = XFIXNUM (AREF (obj, i));
	    }
	}
    }
  else
    signal_error (/* Qselection_error */ "Unrecognized selection data", obj);

  cs->type = symbol_to_gdk_atom (type);
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

DEFUN ("pgtk-own-selection-internal", Fpgtk_own_selection_internal,
       Spgtk_own_selection_internal, 2, 3, 0,
       doc: /* Assert a selection of type SELECTION and value VALUE.
SELECTION is a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what GDK expects.)
VALUE is typically a string, or a cons of two markers, but may be
anything that the functions on `selection-converter-alist' know about.

FRAME should be a frame that should own the selection.  If omitted or
nil, it defaults to the selected frame.  */)
  (Lisp_Object selection, Lisp_Object value, Lisp_Object frame)
{
  if (NILP (frame)) frame = selected_frame;
  if (!FRAME_LIVE_P (XFRAME (frame)) || !FRAME_PGTK_P (XFRAME (frame)))
    error ("GDK selection unavailable for this frame");

  CHECK_SYMBOL (selection);
  if (NILP (value)) error ("VALUE may not be nil");
  pgtk_own_selection (selection, value, frame);
  return value;
}

/* Request the selection value from the owner.  If we are the owner,
   simply return our selection value.  If we are not the owner, this
   will block until all of the data has arrived.  */

DEFUN ("pgtk-get-selection-internal", Fpgtk_get_selection_internal,
       Spgtk_get_selection_internal, 2, 4, 0,
       doc: /* Return text selected from some X window.
SELECTION-SYMBOL is typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)
TARGET-TYPE is the type of data desired, typically `STRING'.

TIME-STAMP is the time to use in the XConvertSelection call for foreign
selections.  If omitted, defaults to the time for the last event.

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.  */)
  (Lisp_Object selection_symbol, Lisp_Object target_type,
   Lisp_Object time_stamp, Lisp_Object terminal)
{
  Lisp_Object val = Qnil;
  Lisp_Object maybe_alias;
  struct frame *f = frame_for_pgtk_selection (terminal);

  CHECK_SYMBOL (selection_symbol);
  CHECK_SYMBOL (target_type);

  if (EQ (target_type, QMULTIPLE))
    error ("Retrieving MULTIPLE selections is currently unimplemented");
  if (!f)
    error ("GDK selection unavailable for this frame");

  /* Quitting inside this function is okay, so we don't have to use
     FOR_EACH_TAIL_SAFE.  */
  maybe_alias = Fassq (selection_symbol, Vpgtk_selection_alias_alist);

  if (!NILP (maybe_alias))
    {
      selection_symbol = XCDR (maybe_alias);
      CHECK_SYMBOL (selection_symbol);
    }

  val = pgtk_get_local_selection (selection_symbol, target_type, true,
				  FRAME_DISPLAY_INFO (f));

  if (NILP (val) && FRAME_LIVE_P (f))
    {
      Lisp_Object frame, val;
      XSETFRAME (frame, f);

      val = pgtk_get_foreign_selection (selection_symbol, target_type,
					time_stamp, frame);

      /* A window property holding just one item is indistinguishable
	 from an array of one element, and is always decoded as the
	 former, producing issues with programs that expect the TARGETS
	 property always to return vectors, even when the toolkit
	 reports just one data type.  Though X sidesteps this ambiguity
	 by defining TARGETS as returning at least two properties
	 TARGETS and MULTIPLE, GTK knows no such scruples, and therefore
	 symbol values (or nil) should be enclosed in vectors when
	 TARGETS is being requested.  (bug#72254) */
      if (EQ (target_type, QTARGETS) && (NILP (val) || SYMBOLP (val)))
	val = make_vector (NILP (val) ? 0 : 1, val);
      return val;
    }

  if (CONSP (val) && SYMBOLP (XCAR (val)))
    {
      val = XCDR (val);
      if (CONSP (val) && NILP (XCDR (val)))
	val = XCAR (val);
    }
  return clean_local_selection_data (val);
}

DEFUN ("pgtk-disown-selection-internal", Fpgtk_disown_selection_internal,
       Spgtk_disown_selection_internal, 1, 3, 0,
       doc: /* If we own the selection SELECTION, disown it.
Disowning it means there is no such selection.

Sets the last-change time for the selection to TIME-OBJECT (by default
the time of the last event).

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.  */)
  (Lisp_Object selection, Lisp_Object time_object, Lisp_Object terminal)
{
  guint32 timestamp;
  GdkAtom selection_atom;
  struct frame *f = frame_for_pgtk_selection (terminal);
  struct pgtk_display_info *dpyinfo;

  if (!f)
    return Qnil;

  dpyinfo = FRAME_DISPLAY_INFO (f);
  CHECK_SYMBOL (selection);

  /* Don't disown the selection when we're not the owner.  */
  if (NILP (LOCAL_SELECTION (selection, dpyinfo)))
    return Qnil;

  selection_atom = symbol_to_gdk_atom (selection);

  block_input ();
  if (NILP (time_object))
    timestamp = dpyinfo->last_user_time;
  else
    CONS_TO_INTEGER (time_object, guint32, timestamp);
  gdk_selection_owner_set_for_display (dpyinfo->display, NULL,
				       selection_atom, timestamp,
				       TRUE);
  unblock_input ();

  return Qt;
}

DEFUN ("pgtk-selection-owner-p", Fpgtk_selection_owner_p, Spgtk_selection_owner_p,
       0, 2, 0,
       doc: /* Whether the current Emacs process owns the given selection.
The arg should be the name of the selection in question, typically one of
the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what GDK expects.)
For convenience, the symbol nil is the same as `PRIMARY',
and t is the same as `SECONDARY'.

TERMINAL should be a terminal object or a frame specifying the GDK
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.  */)
  (Lisp_Object selection, Lisp_Object terminal)
{
  struct frame *f = frame_for_pgtk_selection (terminal);

  CHECK_SYMBOL (selection);
  if (NILP (selection)) selection = QPRIMARY;
  if (EQ (selection, Qt)) selection = QSECONDARY;

  if (f && !NILP (LOCAL_SELECTION (selection, FRAME_DISPLAY_INFO (f))))
    return Qt;
  else
    return Qnil;
}

DEFUN ("pgtk-selection-exists-p", Fpgtk_selection_exists_p, Spgtk_selection_exists_p,
       0, 2, 0,
       doc: /* Whether there is an owner for the given selection.
SELECTION should be the name of the selection in question, typically
one of the symbols `PRIMARY', `SECONDARY', `CLIPBOARD', or
`CLIPBOARD_MANAGER' (GDK expects these literal upper-case names.)  The
symbol nil is the same as `PRIMARY', and t is the same as `SECONDARY'.

TERMINAL should be a terminal object or a frame specifying the GDK
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.  */)
  (Lisp_Object selection, Lisp_Object terminal)
{
  GdkWindow *owner;
  GdkAtom atom;
  struct frame *f = frame_for_pgtk_selection (terminal);
  struct pgtk_display_info *dpyinfo;

  CHECK_SYMBOL (selection);
  if (NILP (selection)) selection = QPRIMARY;
  if (EQ (selection, Qt)) selection = QSECONDARY;

  if (!f)
    return Qnil;

  dpyinfo = FRAME_DISPLAY_INFO (f);

  if (!NILP (LOCAL_SELECTION (selection, dpyinfo)))
    return Qt;

  atom = symbol_to_gdk_atom (selection);
  if (atom == 0) return Qnil;
  block_input ();
  owner = gdk_selection_owner_get_for_display (dpyinfo->display, atom);
  unblock_input ();
  return (owner ? Qt : Qnil);
}

/* Called to handle GDK_SELECTION_NOTIFY events.
   If it's the selection we are waiting for, stop waiting
   by setting the car of reading_selection_reply to non-nil.
   We store t there if the reply is successful, lambda if not.  */

void
pgtk_handle_selection_notify (GdkEventSelection *event)
{
  /* GDK doesn't populate event->requestor, contrary to what the ICCCM
     says should be done with SelectionNotify events.  */

  if (event->selection != reading_which_selection)
    return;

  XSETCAR (reading_selection_reply,
	   (event->property != GDK_NONE ? Qt : Qlambda));
}


/***********************************************************************
                      Drag and drop support
***********************************************************************/

DEFUN ("pgtk-register-dnd-targets", Fpgtk_register_dnd_targets,
       Spgtk_register_dnd_targets, 2, 2, 0,
       doc: /* Register TARGETS on FRAME.
TARGETS should be a list of strings describing data types (selection
targets) that can be dropped on top of FRAME.  */)
  (Lisp_Object frame, Lisp_Object targets)
{
  struct frame *f;
  GtkTargetEntry *entries;
  GtkTargetList *list;
  ptrdiff_t length, n;
  Lisp_Object tem, t;
  char *buf;
  USE_SAFE_ALLOCA;

  f = decode_window_system_frame (frame);
  CHECK_LIST (targets);
  length = list_length (targets);
  n = 0;
  SAFE_NALLOCA (entries, 1, length);
  memset (entries, 0, sizeof *entries * length);
  tem = targets;

  FOR_EACH_TAIL (tem)
    {
      if (!CONSP (tem))
	continue;

      t = XCAR (tem);

      CHECK_STRING (t);
      SAFE_ALLOCA_STRING (buf, t);

      entries[n++].target = buf;
    }
  CHECK_LIST_END (tem, targets);

  if (n != length)
    emacs_abort ();

  list = gtk_target_list_new (entries, n);
  gtk_drag_dest_set_target_list (FRAME_GTK_WIDGET (f), list);
  gtk_target_list_unref (list);

  SAFE_FREE ();

  return Qnil;
}

DEFUN ("pgtk-drop-finish", Fpgtk_drop_finish, Spgtk_drop_finish, 3, 3, 0,
       doc: /* Finish the drag-n-drop event that happened at TIMESTAMP.
SUCCESS is whether or not the drop was successful, i.e. the action
chosen in the last call to `pgtk-update-drop-status' was performed.
TIMESTAMP is the time associated with the drag-n-drop event that is
being finished.
DELETE is whether or not the action was `move'.  */)
  (Lisp_Object success, Lisp_Object timestamp, Lisp_Object delete)
{
  pgtk_finish_drop (success, timestamp, delete);

  return Qnil;
}

DEFUN ("pgtk-update-drop-status", Fpgtk_update_drop_status,
       Spgtk_update_drop_status, 2, 2, 0,
       doc: /* Update the status of the current drag-and-drop operation.
ACTION is the action the drop source should take.
TIMESTAMP is the same as in `pgtk-drop-finish'.  */)
  (Lisp_Object action, Lisp_Object timestamp)
{
  pgtk_update_drop_status (action, timestamp);

  return Qnil;
}

void
syms_of_pgtkselect (void)
{
  DEFSYM (QCLIPBOARD, "CLIPBOARD");
  DEFSYM (QSECONDARY, "SECONDARY");
  DEFSYM (QTEXT, "TEXT");
  DEFSYM (QFILE_NAME, "FILE_NAME");
  DEFSYM (QSTRING, "STRING");
  DEFSYM (QINTEGER, "INTEGER");
  DEFSYM (QTIMESTAMP, "TIMESTAMP");
  DEFSYM (QTEXT, "TEXT");
  DEFSYM (QMULTIPLE, "MULTIPLE");
  DEFSYM (QNULL, "NULL");
  DEFSYM (QATOM, "ATOM");
  DEFSYM (QTARGETS, "TARGETS");
  DEFSYM (QUTF8_STRING, "UTF8_STRING");
  DEFSYM (QCOMPOUND_TEXT, "COMPOUND_TEXT");

  DEFSYM (Qforeign_selection, "foreign-selection");

  DEFSYM (Qpgtk_sent_selection_functions, "pgtk-sent-selection-functions");
  DEFSYM (Qpgtk_lost_selection_functions, "pgtk-lost-selection-functions");

  defsubr (&Spgtk_disown_selection_internal);
  defsubr (&Spgtk_get_selection_internal);
  defsubr (&Spgtk_own_selection_internal);
  defsubr (&Spgtk_selection_exists_p);
  defsubr (&Spgtk_selection_owner_p);
  defsubr (&Spgtk_register_dnd_targets);
  defsubr (&Spgtk_update_drop_status);
  defsubr (&Spgtk_drop_finish);

  DEFVAR_LISP ("selection-converter-alist", Vselection_converter_alist,
	       doc: /* SKIP: real doc in xselect.c.  */);
  Vselection_converter_alist = Qnil;

  DEFVAR_LISP ("pgtk-lost-selection-functions", Vpgtk_lost_selection_functions,
	       doc: /* A list of functions to be called when Emacs loses a selection.
\(This happens when some other client makes its own selection
or when a Lisp program explicitly clears the selection.)
The functions are called with one argument, the selection type
\(a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD').  */);
  Vpgtk_lost_selection_functions = Qnil;

  DEFVAR_LISP ("pgtk-sent-selection-functions", Vpgtk_sent_selection_functions,
	       doc: /* A list of functions to be called when Emacs answers a selection request.
The functions are called with three arguments:
  - the selection name (typically `PRIMARY', `SECONDARY', or `CLIPBOARD');
  - the selection-type which Emacs was asked to convert the
    selection into before sending (for example, `STRING' or `LENGTH');
  - a flag indicating success or failure for responding to the request.
We might have failed (and declined the request) for any number of reasons,
including being asked for a selection that we no longer own, or being asked
to convert into a type that we don't know about or that is inappropriate.
xThis hook doesn't let you change the behavior of Emacs's selection replies,
it merely informs you that they have happened.  */);
  Vpgtk_sent_selection_functions = Qnil;

  DEFVAR_LISP ("pgtk-sent-selection-hooks", Vpgtk_sent_selection_hooks,
	       doc: /* A list of functions to be called when Emacs answers a selection request
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
  Vpgtk_sent_selection_hooks = Qnil;

  DEFVAR_INT ("pgtk-selection-timeout", pgtk_selection_timeout,
	      doc: /* Number of milliseconds to wait for a selection reply.
If the selection owner doesn't reply in this time, we give up.
A value of 0 means wait as long as necessary.  */);
  pgtk_selection_timeout = 0;

  DEFVAR_LISP ("pgtk-selection-alias-alist", Vpgtk_selection_alias_alist,
    doc: /* List of selections to alias to another.
It should be an alist of a selection name to another.  When a
selection request arrives for the first selection, Emacs will respond
as if the request was meant for the other.

Note that this does not affect setting or owning selections.  */);
  Vpgtk_selection_alias_alist = Qnil;

  reading_selection_reply = Fcons (Qnil, Qnil);
  staticpro (&reading_selection_reply);

  property_change_reply = Fcons (Qnil, Qnil);
  staticpro (&property_change_reply);
}
