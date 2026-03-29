/* Support for embedding graphical components in a buffer.

Copyright (C) 2011-2026 Free Software Foundation, Inc.

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

#include "buffer.h"
#include "coding.h"
#include "xwidget.h"
#include "lisp.h"
#include "blockinput.h"
#include "dispextern.h"
#include "frame.h"
#include "keyboard.h"
#include "gtkutil.h"
#include "sysstdio.h"
#include "termhooks.h"
#include "window.h"
#include "process.h"

/* Include xwidget bottom end headers.  */
#ifdef USE_GTK
#include <webkit2/webkit2.h>
#include <JavaScriptCore/JavaScript.h>
#include <cairo.h>
#ifndef HAVE_PGTK
#include <cairo-xlib.h>
#include <X11/Xlib.h>
#else
#include <gtk/gtk.h>
#endif
#ifdef HAVE_XINPUT2
#include <X11/extensions/XInput2.h>
#endif
#elif defined NS_IMPL_COCOA
#include "nsxwidget.h"
#endif

#include <math.h>

static Lisp_Object id_to_xwidget_map;
static Lisp_Object internal_xwidget_view_list;
static Lisp_Object internal_xwidget_list;
static uint32_t xwidget_counter = 0;

#ifdef USE_GTK
#ifdef HAVE_X_WINDOWS
static Lisp_Object x_window_to_xwv_map;
#if WEBKIT_CHECK_VERSION (2, 34, 0)
static Lisp_Object dummy_tooltip_string;
#endif
#endif
static gboolean offscreen_damage_event (GtkWidget *, GdkEvent *, gpointer);
static void synthesize_focus_in_event (GtkWidget *);
static GdkDevice *find_suitable_keyboard (struct frame *);
static gboolean webkit_script_dialog_cb (WebKitWebView *, WebKitScriptDialog *,
					 gpointer);
static void record_osr_embedder (struct xwidget_view *);
static void from_embedder (GdkWindow *, double, double, gpointer, gpointer, gpointer);
static void to_embedder (GdkWindow *, double, double, gpointer, gpointer, gpointer);
static GdkWindow *pick_embedded_child (GdkWindow *, double, double, gpointer);
#endif

static struct xwidget *
allocate_xwidget (void)
{
  return ALLOCATE_PSEUDOVECTOR (struct xwidget, script_callbacks, PVEC_XWIDGET);
}

static struct xwidget_view *
allocate_xwidget_view (void)
{
  return ALLOCATE_PSEUDOVECTOR (struct xwidget_view, w, PVEC_XWIDGET_VIEW);
}

#define XSETXWIDGET(a, b) XSETPSEUDOVECTOR (a, b, PVEC_XWIDGET)
#define XSETXWIDGET_VIEW(a, b) XSETPSEUDOVECTOR (a, b, PVEC_XWIDGET_VIEW)

static struct xwidget_view *xwidget_view_lookup (struct xwidget *,
						 struct window *);
static void kill_xwidget (struct xwidget *);

#ifdef USE_GTK
static void webkit_view_load_changed_cb (WebKitWebView *,
                                         WebKitLoadEvent,
                                         gpointer);
static void webkit_javascript_finished_cb (GObject *,
                                           GAsyncResult *,
                                           gpointer);
static gboolean webkit_download_cb (WebKitWebContext *, WebKitDownload *, gpointer);
static GtkWidget *webkit_create_cb (WebKitWebView *, WebKitNavigationAction *, gpointer);
static gboolean
webkit_decide_policy_cb (WebKitWebView *,
                         WebKitPolicyDecision *,
                         WebKitPolicyDecisionType,
                         gpointer);
static GtkWidget *find_widget_at_pos (GtkWidget *, int, int, int *, int *, bool,
				      struct xwidget_view *);
static gboolean run_file_chooser_cb (WebKitWebView *,
				     WebKitFileChooserRequest *,
				     gpointer);

struct widget_search_data
{
  int x;
  int y;
  bool foundp;
  bool first;
  GtkWidget *data;
};

static void find_widget (GtkWidget *t, struct widget_search_data *);
#endif

#ifdef HAVE_PGTK
static void mouse_target_changed (WebKitWebView *, WebKitHitTestResult *, guint,
				  gpointer);

static int
xw_forward_event_translate (GdkEvent *event, struct xwidget_view *xv,
			    struct xwidget *xw)
{
  GtkWidget *widget;
  int new_x, new_y;

  switch (event->type)
    {
    case GDK_BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
    case GDK_2BUTTON_PRESS:
    case GDK_3BUTTON_PRESS:
      widget = find_widget_at_pos (xw->widgetwindow_osr,
				   lrint (event->button.x - xv->clip_left),
				   lrint (event->button.y - xv->clip_top),
				   &new_x, &new_y, false, NULL);
      if (widget)
	{
	  event->any.window = gtk_widget_get_window (widget);
	  event->button.x = new_x;
	  event->button.y = new_y;
	  return 1;
	}
      return 0;
    case GDK_SCROLL:
      widget = find_widget_at_pos (xw->widgetwindow_osr,
				   lrint (event->scroll.x - xv->clip_left),
				   lrint (event->scroll.y - xv->clip_top),
				   &new_x, &new_y, false, NULL);
      if (widget)
	{
	  event->any.window = gtk_widget_get_window (widget);
	  event->scroll.x = new_x;
	  event->scroll.y = new_y;
	  return 1;
	}
      return 0;
    case GDK_MOTION_NOTIFY:
      widget = find_widget_at_pos (xw->widgetwindow_osr,
				   lrint (event->motion.x - xv->clip_left),
				   lrint (event->motion.y - xv->clip_top),
				   &new_x, &new_y, false, NULL);
      if (widget)
	{
	  event->any.window = gtk_widget_get_window (widget);
	  event->motion.x = new_x;
	  event->motion.y = new_y;
	  return 1;
	}
      return 0;
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      widget = find_widget_at_pos (xw->widgetwindow_osr,
				   lrint (event->crossing.x - xv->clip_left),
				   lrint (event->crossing.y - xv->clip_top),
				   &new_x, &new_y, false, NULL);
      if (widget)
	{
	  event->any.window = gtk_widget_get_window (widget);
	  event->crossing.x = new_x;
	  event->crossing.y = new_y;
	  return 1;
	}
      return 0;
    default:
      return 0;
    }
}

static gboolean
xw_forward_event_from_view (GtkWidget *widget, GdkEvent *event,
			    gpointer user_data)
{
  struct xwidget_view *xv = user_data;
  struct xwidget *xw = XXWIDGET (xv->model);
  GdkEvent *eventcopy;
  bool translated_p;

  if (NILP (xw->buffer))
    return TRUE;

  eventcopy = gdk_event_copy (event);
  translated_p = xw_forward_event_translate (eventcopy, xv, xw);
  record_osr_embedder (xv);

  g_object_ref (eventcopy->any.window);
  if (translated_p)
    gtk_main_do_event (eventcopy);
  gdk_event_free (eventcopy);

  /* Don't propagate this event further.  */
  return TRUE;
}
#endif

#ifdef HAVE_X_WINDOWS
enum xw_crossing_mode
  {
    XW_CROSSING_LEFT,
    XW_CROSSING_ENTERED,
    XW_CROSSING_NONE
  };

static guint
xw_translate_x_modifiers (struct x_display_info *dpyinfo,
			  unsigned int modifiers)
{
  guint mods = 0;

  if (modifiers & dpyinfo->meta_mod_mask)
    {
      /* GDK always assumes Mod1 is alt, but that's no reason for
	 us to make that mistake as well.  */
      if (!dpyinfo->alt_mod_mask)
	mods |= GDK_MOD1_MASK;
      else
	mods |= GDK_META_MASK;
    }

  if (modifiers & dpyinfo->alt_mod_mask)
    mods |= GDK_MOD1_MASK;
  if (modifiers & dpyinfo->super_mod_mask)
    mods |= GDK_SUPER_MASK;
  if (modifiers & dpyinfo->hyper_mod_mask)
    mods |= GDK_HYPER_MASK;
  if (modifiers & ControlMask)
    mods |= GDK_CONTROL_MASK;
  if (modifiers & ShiftMask)
    mods |= GDK_SHIFT_MASK;

  return mods;
}

static bool xw_maybe_synthesize_crossing (struct xwidget_view *,
					  GdkWindow *, int, int, int,
					  Time, unsigned int,
					  GdkCrossingMode, GdkCrossingMode);
static void xw_notify_virtual_upwards_until (struct xwidget_view *, GdkWindow *,
					     GdkWindow *, GdkWindow *, unsigned int,
					     int, int, Time, GdkEventType, bool,
					     GdkCrossingMode);
static void window_coords_from_toplevel (GdkWindow *, GdkWindow *, int,
					 int, int *, int *);
#endif

DEFUN ("make-xwidget",
       Fmake_xwidget, Smake_xwidget,
       4, 7, 0,
       doc: /* Make an xwidget of TYPE.
If BUFFER is nil, use the current buffer.
If BUFFER is a string and no such buffer exists, create it.
TYPE is a symbol which can take one of the following values:

- webkit

RELATED is nil, or an xwidget.  When constructing a WebKit widget, it
will share the same settings and internal subprocess as RELATED.
Returns the newly constructed xwidget, or nil if construction
fails.  */)
  (Lisp_Object type,
   Lisp_Object title, Lisp_Object width, Lisp_Object height,
   Lisp_Object arguments, Lisp_Object buffer, Lisp_Object related)
{
#ifdef USE_GTK
  if (!xg_gtk_initialized)
    error ("make-xwidget: GTK has not been initialized");
#endif
  CHECK_SYMBOL (type);
  CHECK_FIXNAT (width);
  CHECK_FIXNAT (height);

  if (!EQ (type, Qwebkit))
    error ("Bad xwidget type");

  Frequire (Qxwidget, Qnil, Qnil);

  struct xwidget *xw = allocate_xwidget ();
  Lisp_Object val;
  xw->type = type;
  xw->title = title;
  xw->buffer = (NILP (buffer) ? Fcurrent_buffer ()
		: Fget_buffer_create (buffer, Qnil));
  xw->height = XFIXNAT (height);
  xw->width = XFIXNAT (width);
  xw->kill_without_query = false;
  XSETXWIDGET (val, xw);
  internal_xwidget_list = Fcons (val, internal_xwidget_list);
  Vxwidget_list = Fcopy_sequence (internal_xwidget_list);
  xw->plist = Qnil;
  xw->xwidget_id = ++xwidget_counter;
  xw->find_text = NULL;

  Fputhash (make_fixnum (xw->xwidget_id), val, id_to_xwidget_map);

#ifdef USE_GTK
  xw->widgetwindow_osr = NULL;
  xw->widget_osr = NULL;
  xw->hit_result = 0;
  if (EQ (xw->type, Qwebkit))
    {
      block_input ();
      WebKitSettings *settings;
      WebKitWebContext *webkit_context = webkit_web_context_get_default ();

# if WEBKIT_CHECK_VERSION (2, 26, 0)
      if (!webkit_web_context_get_sandbox_enabled (webkit_context))
	webkit_web_context_set_sandbox_enabled (webkit_context, TRUE);
# endif

      xw->widgetwindow_osr = gtk_offscreen_window_new ();
      gtk_window_resize (GTK_WINDOW (xw->widgetwindow_osr), xw->width,
                         xw->height);
      gtk_container_check_resize (GTK_CONTAINER (xw->widgetwindow_osr));

      if (EQ (xw->type, Qwebkit))
        {
	  WebKitWebView *related_view;

	  if (NILP (related)
	      || !XWIDGETP (related)
	      || !EQ (XXWIDGET (related)->type, Qwebkit))
	    {
	      WebKitWebContext *ctx = webkit_web_context_new ();
	      xw->widget_osr = webkit_web_view_new_with_context (ctx);
	      g_object_unref (ctx);

	      g_signal_connect (G_OBJECT (ctx),
				"download-started",
				G_CALLBACK (webkit_download_cb), xw);

	      webkit_web_view_load_uri (WEBKIT_WEB_VIEW (xw->widget_osr),
					"about:blank");
	      /* webkitgtk uses GSubprocess which sets sigaction causing
		 Emacs to not catch SIGCHLD with its usual handle setup in
		 'catch_child_signal'.  This resets the SIGCHLD sigaction.  */
	      catch_child_signal ();
	    }
	  else
	    {
	      related_view = WEBKIT_WEB_VIEW (XXWIDGET (related)->widget_osr);
	      xw->widget_osr = webkit_web_view_new_with_related_view (related_view);
	    }

	  /* Enable the developer extras.  */
	  settings = webkit_web_view_get_settings (WEBKIT_WEB_VIEW (xw->widget_osr));
	  g_object_set (G_OBJECT (settings), "enable-developer-extras", TRUE, NULL);
	  g_object_set (G_OBJECT (settings), "enable-javascript",
		        (gboolean) (!xwidget_webkit_disable_javascript), NULL);
	}

      gtk_widget_set_size_request (GTK_WIDGET (xw->widget_osr), xw->width,
                                   xw->height);
      gtk_widget_queue_allocate (GTK_WIDGET (xw->widget_osr));

      if (EQ (xw->type, Qwebkit))
        {
          gtk_container_add (GTK_CONTAINER (xw->widgetwindow_osr),
                             GTK_WIDGET (WEBKIT_WEB_VIEW (xw->widget_osr)));
        }
      else
        {
          gtk_container_add (GTK_CONTAINER (xw->widgetwindow_osr),
                             xw->widget_osr);
        }

      gtk_widget_show (xw->widget_osr);
      gtk_widget_show (xw->widgetwindow_osr);
#if !defined HAVE_XINPUT2 && !defined HAVE_PGTK
      synthesize_focus_in_event (xw->widgetwindow_osr);
#endif

      g_signal_connect (G_OBJECT (gtk_widget_get_window (xw->widgetwindow_osr)),
			"from-embedder", G_CALLBACK (from_embedder), NULL);
      g_signal_connect (G_OBJECT (gtk_widget_get_window (xw->widgetwindow_osr)),
			"to-embedder", G_CALLBACK (to_embedder), NULL);
      g_signal_connect (G_OBJECT (gtk_widget_get_window (xw->widgetwindow_osr)),
			"pick-embedded-child", G_CALLBACK (pick_embedded_child), NULL);

      /* Store some xwidget data in the gtk widgets for convenient
         retrieval in the event handlers.  */
      g_object_set_data (G_OBJECT (xw->widget_osr), XG_XWIDGET, xw);
      g_object_set_data (G_OBJECT (xw->widgetwindow_osr), XG_XWIDGET, xw);

      /* signals */
      if (EQ (xw->type, Qwebkit))
        {
          g_signal_connect (G_OBJECT (xw->widget_osr),
                            "load-changed",
                            G_CALLBACK (webkit_view_load_changed_cb), xw);

          g_signal_connect (G_OBJECT (xw->widget_osr),
                            "decide-policy",
                            G_CALLBACK
                            (webkit_decide_policy_cb),
                            xw);
#ifdef HAVE_PGTK
	  g_signal_connect (G_OBJECT (xw->widget_osr),
			    "mouse-target-changed",
			    G_CALLBACK (mouse_target_changed),
			    xw);
#endif
	  g_signal_connect (G_OBJECT (xw->widget_osr),
			    "create",
			    G_CALLBACK (webkit_create_cb),
			    xw);
	  g_signal_connect (G_OBJECT (xw->widget_osr),
			    "script-dialog",
			    G_CALLBACK (webkit_script_dialog_cb),
			    NULL);
	  g_signal_connect (G_OBJECT (xw->widget_osr),
			    "run-file-chooser",
			    G_CALLBACK (run_file_chooser_cb),
			    NULL);
        }

      g_signal_connect (G_OBJECT (xw->widgetwindow_osr), "damage-event",
			G_CALLBACK (offscreen_damage_event), xw);

      unblock_input ();
    }
#elif defined NS_IMPL_COCOA
  nsxwidget_init (xw);
#endif

  return val;
}

DEFUN ("xwidget-live-p", Fxwidget_live_p, Sxwidget_live_p,
       1, 1, 0, doc: /* Return t if OBJECT is an xwidget that has not been killed.
Value is nil if OBJECT is not an xwidget or if it has been killed.  */)
  (Lisp_Object object)
{
  return ((XWIDGETP (object)
	   && !NILP (XXWIDGET (object)->buffer))
	  ? Qt : Qnil);
}

#ifdef USE_GTK
static void
set_widget_if_text_view (GtkWidget *widget, void *data)
{
  GtkWidget **pointer = data;

  if (GTK_IS_TEXT_VIEW (widget))
    *pointer = widget;
}
#endif

DEFUN ("xwidget-perform-lispy-event",
       Fxwidget_perform_lispy_event, Sxwidget_perform_lispy_event,
       2, 3, 0, doc: /* Send a lispy event to XWIDGET.
EVENT should be the event that will be sent.  FRAME should be the
frame which generated the event, and defaults to the selected frame.
On X11, modifier keys will not be processed if FRAME is nil and the
selected frame is not an X-Windows frame.  */)
  (Lisp_Object xwidget, Lisp_Object event, Lisp_Object frame)
{
#ifdef USE_GTK
  struct frame *f = NULL;
  GdkEvent *xg_event;
  GtkContainerClass *klass;
  GtkWidget *widget;
  GtkWidget *temp = NULL;
#ifdef HAVE_XINPUT2
  GdkWindow *embedder;
  GdkWindow *osw;
#endif
#endif

  CHECK_LIVE_XWIDGET (xwidget);

#ifdef USE_GTK
  if (!NILP (frame))
    f = decode_window_system_frame (frame);
  else if (FRAME_WINDOW_P (SELECTED_FRAME ()))
    f = SELECTED_FRAME ();

  int character = -1, keycode = -1;
  int modifiers = 0;
  struct xwidget *xw = XXWIDGET (xwidget);

#ifdef HAVE_XINPUT2
  /* XI2 GDK devices crash if we try this without an embedder set.  */
  if (!f)
    return Qnil;

  block_input ();
  osw = gtk_widget_get_window (xw->widgetwindow_osr);
  embedder = gtk_widget_get_window (FRAME_GTK_OUTER_WIDGET (f));

  gdk_offscreen_window_set_embedder (osw, embedder);
  unblock_input ();
#endif	/* HAVE_XINPUT2 */
  widget = gtk_window_get_focus (GTK_WINDOW (xw->widgetwindow_osr));

  if (!widget)
    widget = xw->widget_osr;

  if (RANGED_FIXNUMP (0, event, INT_MAX))
    {
      character = XFIXNUM (event);

      if (character < 32)
	modifiers |= ctrl_modifier;

      modifiers |= character & meta_modifier;
      modifiers |= character & hyper_modifier;
      modifiers |= character & super_modifier;
      modifiers |= character & shift_modifier;
      modifiers |= character & ctrl_modifier;

      character = character & ~(1 << 21);

      if (character < 32)
	character += '_';

#ifndef HAVE_PGTK
      if (f)
	modifiers = x_emacs_to_x_modifiers (FRAME_DISPLAY_INFO (f), modifiers);
      else
	modifiers = 0;
#else
      if (f)
	modifiers = pgtk_emacs_to_gtk_modifiers (FRAME_DISPLAY_INFO (f), modifiers);
      else
	modifiers = 0;
#endif	/* HAVE_PGTK */
    }
  else if (SYMBOLP (event))
    {
      Lisp_Object decoded = parse_modifiers (event);
      Lisp_Object decoded_name = SYMBOL_NAME (XCAR (decoded));

      int off = 0;
      bool found = false;

      while (off < 256)
	{
	  if (lispy_function_keys[off]
	      && !strcmp (lispy_function_keys[off],
			  SSDATA (decoded_name)))
	    {
	      found = true;
	      break;
	    }
	  ++off;
	}

#ifndef HAVE_PGTK
      if (f)
	modifiers = x_emacs_to_x_modifiers (FRAME_DISPLAY_INFO (f),
					    XFIXNUM (XCAR (XCDR (decoded))));
      else
	modifiers = 0;
#else
      if (f)
	modifiers = pgtk_emacs_to_gtk_modifiers (FRAME_DISPLAY_INFO (f),
						 XFIXNUM (XCAR (XCDR (decoded))));
      else
	modifiers = 0;
#endif	/* HAVE_PGTK */

      if (found)
	keycode = off + 0xff00;
    }

  if (character == -1 && keycode == -1)
    {
#ifdef HAVE_XINPUT2
      block_input ();
      if (xw->embedder_view)
	record_osr_embedder (xw->embedder_view);
      else
	gdk_offscreen_window_set_embedder (osw, NULL);
      unblock_input ();
#endif	/* HAVE_XINPUT2 */
      return Qnil;
    }

  block_input ();
  xg_event = gdk_event_new (GDK_KEY_PRESS);
  xg_event->any.window = gtk_widget_get_window (xw->widget_osr);
  g_object_ref (xg_event->any.window);

  if (character > -1)
    keycode = gdk_unicode_to_keyval (character);

  xg_event->key.keyval = keycode;
#ifndef HAVE_X_WINDOWS
  xg_event->key.state = modifiers;
#else
  if (f)
    xg_event->key.state = xw_translate_x_modifiers (FRAME_DISPLAY_INFO (f),
						    modifiers);
#endif	/* !HAVE_X_WINDOWS */

  if (keycode > -1)
    {
      /* WebKitGTK internals abuse follows.  */
      if (WEBKIT_IS_WEB_VIEW (widget))
	{
	  /* WebKitGTK relies on an internal GtkTextView object to
	     "translate" keys such as backspace.  We must find that
	     widget and activate its binding to this key if any.  */
	  klass = GTK_CONTAINER_CLASS (G_OBJECT_GET_CLASS (widget));

	  klass->forall (GTK_CONTAINER (xw->widget_osr), TRUE,
			 set_widget_if_text_view, &temp);

	  if (GTK_IS_WIDGET (temp))
	    {
	      if (!gtk_widget_get_realized (temp))
		gtk_widget_realize (temp);

	      gtk_bindings_activate (G_OBJECT (temp), keycode, modifiers);
	    }
	}
    }

  if (f)
    gdk_event_set_device (xg_event,
			  find_suitable_keyboard (SELECTED_FRAME ()));

  gtk_main_do_event (xg_event);
  xg_event->type = GDK_KEY_RELEASE;
  gtk_main_do_event (xg_event);
  gdk_event_free (xg_event);

#ifdef HAVE_XINPUT2
  if (xw->embedder_view)
    record_osr_embedder (xw->embedder_view);
  else
    gdk_offscreen_window_set_embedder (osw, NULL);
#endif	/* HAVE_XINPUT2 */
  unblock_input ();
#endif	/* USE_GTK */

  return Qnil;
}

DEFUN ("get-buffer-xwidgets", Fget_buffer_xwidgets, Sget_buffer_xwidgets,
       1, 1, 0,
       doc: /* Return a list of xwidgets associated with BUFFER.
BUFFER may be a buffer or the name of one.  */)
  (Lisp_Object buffer)
{
  Lisp_Object xw, tail, xw_list;

  if (NILP (buffer))
    return Qnil;
  buffer = Fget_buffer (buffer);
  if (NILP (buffer))
    return Qnil;

  xw_list = Qnil;

  for (tail = internal_xwidget_list; CONSP (tail); tail = XCDR (tail))
    {
      xw = XCAR (tail);
      if (XWIDGETP (xw) && EQ (Fxwidget_buffer (xw), buffer))
        xw_list = Fcons (xw, xw_list);
    }
  return xw_list;
}

static bool
xwidget_hidden (struct xwidget_view *xv)
{
  return xv->hidden;
}

struct xwidget *
xwidget_from_id (uint32_t id)
{
  Lisp_Object key = make_fixnum (id);
  Lisp_Object xwidget = Fgethash (key, id_to_xwidget_map, Qnil);

  if (NILP (xwidget))
    emacs_abort ();

  return XXWIDGET (xwidget);
}

#ifdef USE_GTK
static GdkWindow *
pick_embedded_child (GdkWindow *window, double x, double y,
		     gpointer user_data)
{
  GtkWidget *widget;
  GtkWidget *child;
  GdkEvent event;
  int xout, yout;

  event.any.window = window;
  event.any.type = GDK_NOTHING;

  widget = gtk_get_event_widget (&event);

  if (!widget)
    return NULL;

  child = find_widget_at_pos (widget, lrint (x), lrint (y),
			      &xout, &yout, false, NULL);

  if (!child)
    return NULL;

  return gtk_widget_get_window (child);
}

static void
record_osr_embedder (struct xwidget_view *view)
{
  struct xwidget *xw;
  GdkWindow *window, *embedder;

  xw = XXWIDGET (view->model);
  window = gtk_widget_get_window (xw->widgetwindow_osr);
#ifndef HAVE_PGTK
  embedder = gtk_widget_get_window (FRAME_GTK_OUTER_WIDGET (view->frame));
#else
  embedder = gtk_widget_get_window (view->widget);
#endif
  gdk_offscreen_window_set_embedder (window, embedder);
  xw->embedder = view->frame;
  xw->embedder_view = view;
}

static struct xwidget *
find_xwidget_for_offscreen_window (GdkWindow *window)
{
  Lisp_Object tem;
  struct xwidget *xw;
  GdkWindow *w;

  for (tem = internal_xwidget_list; CONSP (tem); tem = XCDR (tem))
    {
      if (XWIDGETP (XCAR (tem)))
	{
	  xw = XXWIDGET (XCAR (tem));
	  w = gtk_widget_get_window (xw->widgetwindow_osr);

	  if (w == window)
	    return xw;
	}
    }

  return NULL;
}

static void
from_embedder (GdkWindow *window, double x, double y,
	       gpointer x_out_ptr, gpointer y_out_ptr,
	       gpointer user_data)
{
  double *xout = x_out_ptr;
  double *yout = y_out_ptr;
#ifndef HAVE_PGTK
  struct xwidget *xw = find_xwidget_for_offscreen_window (window);
  struct xwidget_view *xvw;
  gint xoff, yoff;

  if (!xw)
    emacs_abort ();

  xvw = xw->embedder_view;

  if (!xvw)
    {
      *xout = x;
      *yout = y;
    }
  else
    {
      gtk_widget_translate_coordinates (FRAME_GTK_WIDGET (xvw->frame),
					FRAME_GTK_OUTER_WIDGET (xvw->frame),
					0, 0, &xoff, &yoff);

      *xout = x - xvw->x - xoff;
      *yout = y - xvw->y - yoff;
    }
#else
  *xout = x;
  *yout = y;
#endif
}

static void
to_embedder (GdkWindow *window, double x, double y,
	     gpointer x_out_ptr, gpointer y_out_ptr,
	     gpointer user_data)
{
  double *xout = x_out_ptr;
  double *yout = y_out_ptr;
#ifndef HAVE_PGTK
  struct xwidget *xw = find_xwidget_for_offscreen_window (window);
  struct xwidget_view *xvw;
  gint xoff, yoff;

  if (!xw)
    emacs_abort ();

  xvw = xw->embedder_view;

  if (!xvw)
    {
      *xout = x;
      *yout = y;
    }
  else
    {
      gtk_widget_translate_coordinates (FRAME_GTK_WIDGET (xvw->frame),
					FRAME_GTK_OUTER_WIDGET (xvw->frame),
					0, 0, &xoff, &yoff);

      *xout = x + xvw->x + xoff;
      *yout = y + xvw->y + yoff;
    }
#else
  *xout = x;
  *yout = y;
#endif
}

static GdkDevice *
find_suitable_pointer (struct frame *f, bool need_smooth)
{
  GdkSeat *seat = gdk_display_get_default_seat
    (gtk_widget_get_display (FRAME_GTK_WIDGET (f)));
  GList *devices, *tem;
  GdkDevice *device;

  if (!seat)
    return NULL;

  devices = gdk_seat_get_slaves (seat, GDK_SEAT_CAPABILITY_ALL_POINTING);
  device = NULL;
  tem = NULL;

  if (need_smooth)
    {
      for (tem = devices; tem; tem = tem->next)
	{
	  device = GDK_DEVICE (tem->data);

	  if (gdk_device_get_source (device) == GDK_SOURCE_TOUCHPAD)
	    break;
	}
    }

  g_list_free (devices);

  return !tem ? gdk_seat_get_pointer (seat) : device;
}

static GdkDevice *
find_suitable_keyboard (struct frame *f)
{
  GdkSeat *seat = gdk_display_get_default_seat
    (gtk_widget_get_display (FRAME_GTK_WIDGET (f)));

  if (!seat)
    return NULL;

  return gdk_seat_get_keyboard (seat);
}

static void
find_widget_cb (GtkWidget *widget, void *user)
{
  find_widget (widget, user);
}

static void
find_widget (GtkWidget *widget,
	     struct widget_search_data *data)
{
  GtkAllocation new_allocation;
  GdkWindow *window;
  int x_offset = 0;
  int y_offset = 0;

  gtk_widget_get_allocation (widget, &new_allocation);

  if (gtk_widget_get_has_window (widget))
    {
      new_allocation.x = 0;
      new_allocation.y = 0;
    }

  if (gtk_widget_get_parent (widget) && !data->first)
    {
      window = gtk_widget_get_window (widget);
      while (window != gtk_widget_get_window (gtk_widget_get_parent (widget)))
	{
	  gint tx, ty, twidth, theight;

	  if (!window)
	    return;

	  twidth = gdk_window_get_width (window);
	  theight = gdk_window_get_height (window);

	  if (new_allocation.x < 0)
	    {
	      new_allocation.width += new_allocation.x;
	      new_allocation.x = 0;
	    }

	  if (new_allocation.y < 0)
	    {
	      new_allocation.height += new_allocation.y;
	      new_allocation.y = 0;
	    }

	  if (new_allocation.x + new_allocation.width > twidth)
	    new_allocation.width = twidth - new_allocation.x;
	  if (new_allocation.y + new_allocation.height > theight)
	    new_allocation.height = theight - new_allocation.y;

	  gdk_window_get_position (window, &tx, &ty);
	  new_allocation.x += tx;
	  x_offset += tx;
	  new_allocation.y += ty;
	  y_offset += ty;

	  window = gdk_window_get_parent (window);
	}
    }

  if ((data->x >= new_allocation.x) && (data->y >= new_allocation.y)
      && (data->x < new_allocation.x + new_allocation.width)
      && (data->y < new_allocation.y + new_allocation.height))
    {
      /* First, check if the drag is in a valid drop site in one of
	 our children.	*/
      if (GTK_IS_CONTAINER (widget))
	{
	  struct widget_search_data new_data = *data;

	  new_data.x -= x_offset;
	  new_data.y -= y_offset;
	  new_data.foundp = false;
	  new_data.first = false;

	  gtk_container_forall (GTK_CONTAINER (widget),
				find_widget_cb, &new_data);

	  data->foundp = new_data.foundp;
	  if (data->foundp)
	    data->data = new_data.data;
	}

      /* If not, and this widget is registered as a drop site, check
	 to emit "drag_motion" to check if we are actually in a drop
	 site.	*/
      if (!data->foundp)
	{
	  data->foundp = true;
	  data->data = widget;
	}
    }
}

static GtkWidget *
find_widget_at_pos (GtkWidget *w, int x, int y,
		    int *new_x, int *new_y,
		    bool pointer_grabs,
		    struct xwidget_view *vw)
{
  struct widget_search_data data;
#ifdef HAVE_X_WINDOWS
  GtkWidget *grab = NULL;

  if (pointer_grabs)
    {
      grab = vw->passive_grab;

      if (grab && gtk_widget_get_window (grab))
	{
	  gtk_widget_translate_coordinates (w, grab, x,
					    y, new_x, new_y);

	  return grab;
	}
    }
#endif

  data.x = x;
  data.y = y;
  data.foundp = false;
  data.first = true;

  find_widget (w, &data);

  if (data.foundp)
    {
      gtk_widget_translate_coordinates (w, data.data, x,
					y, new_x, new_y);
      return data.data;
    }

  *new_x = x;
  *new_y = y;

  return NULL;
}

#ifdef HAVE_PGTK
static Emacs_Cursor
cursor_for_hit (guint result, struct frame *frame)
{
  Emacs_Cursor cursor = FRAME_OUTPUT_DATA (frame)->nontext_cursor;

  if ((result & WEBKIT_HIT_TEST_RESULT_CONTEXT_EDITABLE)
      || (result & WEBKIT_HIT_TEST_RESULT_CONTEXT_SELECTION)
      || (result & WEBKIT_HIT_TEST_RESULT_CONTEXT_DOCUMENT))
    cursor = FRAME_X_OUTPUT (frame)->text_cursor;

  if (result & WEBKIT_HIT_TEST_RESULT_CONTEXT_SCROLLBAR)
    cursor = FRAME_X_OUTPUT (frame)->vertical_drag_cursor;

  if (result & WEBKIT_HIT_TEST_RESULT_CONTEXT_LINK)
    cursor = FRAME_X_OUTPUT (frame)->hand_cursor;

  return cursor;
}

static void
define_cursors (struct xwidget *xw, WebKitHitTestResult *res)
{
  struct xwidget_view *xvw;
  GdkWindow *wdesc;

  xw->hit_result = webkit_hit_test_result_get_context (res);

  for (Lisp_Object tem = internal_xwidget_view_list; CONSP (tem);
       tem = XCDR (tem))
    {
      if (XWIDGET_VIEW_P (XCAR (tem)))
	{
	  xvw = XXWIDGET_VIEW (XCAR (tem));

	  if (XXWIDGET (xvw->model) == xw)
	    {
	      xvw->cursor = cursor_for_hit (xw->hit_result, xvw->frame);

	      if (gtk_widget_get_realized (xvw->widget))
		{
		  wdesc = gtk_widget_get_window (xvw->widget);
		  gdk_window_set_cursor (wdesc, xvw->cursor);
		}
	    }
	}
    }
}

static void
mouse_target_changed (WebKitWebView *webview,
		      WebKitHitTestResult *hitresult,
		      guint modifiers, gpointer xw)
{
  define_cursors (xw, hitresult);
}
#endif

static gboolean
run_file_chooser_cb (WebKitWebView *webview,
		     WebKitFileChooserRequest *request,
		     gpointer user_data)
{
  struct frame *f = SELECTED_FRAME ();
  GtkFileChooserNative *chooser;
  GtkFileFilter *filter;
  bool select_multiple_p;
  guint response;
  GSList *filenames;
  GSList *tem;
  int i, len;
  gchar **files;

  /* Return TRUE to prevent WebKit from showing the default script
     dialog in the offscreen window, which runs a nested main loop
     Emacs can't respond to, and as such can't pass X events to.  */
  if (!FRAME_WINDOW_P (f))
    return TRUE;

  chooser = gtk_file_chooser_native_new ("Select file",
					 GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
					 GTK_FILE_CHOOSER_ACTION_OPEN, "Select",
					 "Cancel");
  filter = webkit_file_chooser_request_get_mime_types_filter (request);
  select_multiple_p = webkit_file_chooser_request_get_select_multiple (request);

  gtk_file_chooser_set_select_multiple (GTK_FILE_CHOOSER (chooser),
					select_multiple_p);
  gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (chooser), filter);
  response = gtk_native_dialog_run (GTK_NATIVE_DIALOG (chooser));

  if (response != GTK_RESPONSE_ACCEPT)
    {
      gtk_native_dialog_destroy (GTK_NATIVE_DIALOG (chooser));
      webkit_file_chooser_request_cancel (request);

      return TRUE;
    }

  filenames = gtk_file_chooser_get_filenames (GTK_FILE_CHOOSER (chooser));
  len = g_slist_length (filenames);
  files = alloca (sizeof *files * (len + 1));

  for (tem = filenames, i = 0; tem; tem = tem->next, ++i)
    files[i] = tem->data;
  files[len] = NULL;

  g_slist_free (filenames);
  webkit_file_chooser_request_select_files (request, (const gchar **) files);

  for (i = 0; i < len; ++i)
    g_free (files[i]);

  gtk_native_dialog_destroy (GTK_NATIVE_DIALOG (chooser));

  return TRUE;
}

#ifdef HAVE_X_WINDOWS

static void
xv_drag_begin_cb (GtkWidget *widget,
		  GdkDragContext *context,
		  gpointer user_data)
{
  struct xwidget_view *view = user_data;

  if (view->passive_grab)
    {
      g_signal_handler_disconnect (view->passive_grab,
				   view->passive_grab_destruction_signal);
      g_signal_handler_disconnect (view->passive_grab,
				   view->passive_grab_drag_signal);
      view->passive_grab = NULL;
    }
}

static void
xwidget_button_1 (struct xwidget_view *view,
		  bool down_p, int x, int y, int button,
		  int modifier_state, Time time)
{
  GdkEvent *xg_event;
  struct xwidget *model = XXWIDGET (view->model);
  GtkWidget *target;
  GtkWidget *ungrab_target;
  GdkWindow *toplevel, *target_window;
  int view_x, view_y;

  /* X and Y should be relative to the origin of view->wdesc.  */
  x += view->clip_left;
  y += view->clip_top;

  view_x = x;
  view_y = y;

  target = find_widget_at_pos (model->widgetwindow_osr, x, y, &x, &y,
			       true, view);

  if (!target)
    target = model->widget_osr;

  xg_event = gdk_event_new (down_p ? GDK_BUTTON_PRESS : GDK_BUTTON_RELEASE);

  xg_event->any.window = gtk_widget_get_window (target);
  g_object_ref (xg_event->any.window); /* The window will be unrefed
					  later by gdk_event_free.  */

  xg_event->button.x = x;
  xg_event->button.x_root = x;
  xg_event->button.y = y;
  xg_event->button.y_root = y;
  xg_event->button.button = button;
  xg_event->button.state = modifier_state;
  xg_event->button.time = time;
  xg_event->button.device = find_suitable_pointer (view->frame, false);

  gtk_main_do_event (xg_event);
  gdk_event_free (xg_event);


  if (down_p && !view->passive_grab)
    {
      view->passive_grab = target;
      view->passive_grab_destruction_signal
	= g_signal_connect (G_OBJECT (view->passive_grab),
			    "destroy", G_CALLBACK (gtk_widget_destroyed),
			    &view->passive_grab);
      view->passive_grab_drag_signal
	= g_signal_connect (G_OBJECT (view->passive_grab),
			    "drag-begin", G_CALLBACK (xv_drag_begin_cb),
			    view);
    }
  else
    {
      ungrab_target = find_widget_at_pos (model->widgetwindow_osr,
					  view_x, view_y, &x, &y,
					  false, NULL);

      if (view->last_crossing_window && ungrab_target)
	{
	  xw_maybe_synthesize_crossing (view, gtk_widget_get_window (ungrab_target),
					view_x, view_y, XW_CROSSING_NONE,
					time, modifier_state, GDK_CROSSING_UNGRAB,
					GDK_CROSSING_UNGRAB);
	}
      else
	{
	  toplevel = gtk_widget_get_window (model->widgetwindow_osr);
	  xg_event = gdk_event_new (GDK_LEAVE_NOTIFY);
	  target_window = gtk_widget_get_window (target);
	  window_coords_from_toplevel (target_window, toplevel, view_x,
				       view_y, &x, &y);

	  xg_event->crossing.x = x;
	  xg_event->crossing.y = y;
	  xg_event->crossing.time = time;
	  xg_event->crossing.focus = FALSE;
	  xg_event->crossing.detail = GDK_NOTIFY_ANCESTOR;
	  xg_event->crossing.mode = GDK_CROSSING_UNGRAB;
	  xg_event->crossing.window = g_object_ref (target_window);
	  gdk_event_set_device (xg_event,
				find_suitable_pointer (view->frame, false));

	  gtk_main_do_event (xg_event);
	  gdk_event_free (xg_event);

	  xw_notify_virtual_upwards_until (view, target_window, toplevel, toplevel,
					   modifier_state, view_x, view_y, time,
					   GDK_LEAVE_NOTIFY, false,
					   GDK_CROSSING_UNGRAB);

	  if (target_window != toplevel)
	    {
	      xg_event = gdk_event_new (GDK_LEAVE_NOTIFY);

	      xg_event->crossing.x = view_y;
	      xg_event->crossing.y = view_y;
	      xg_event->crossing.time = time;
	      xg_event->crossing.focus = FALSE;
	      xg_event->crossing.detail = GDK_NOTIFY_VIRTUAL;
	      xg_event->crossing.mode = GDK_CROSSING_UNGRAB;
	      xg_event->crossing.window = g_object_ref (toplevel);

	      gdk_event_set_device (xg_event,
				    find_suitable_pointer (view->frame, false));
	      gtk_main_do_event (xg_event);
	      gdk_event_free (xg_event);
	    }

	}

      if (view->passive_grab)
	{
	  g_signal_handler_disconnect (view->passive_grab,
				       view->passive_grab_destruction_signal);
	  g_signal_handler_disconnect (view->passive_grab,
				       view->passive_grab_drag_signal);
	  view->passive_grab = NULL;
	}
    }
}

void
xwidget_button (struct xwidget_view *view,
		bool down_p, int x, int y, int button,
		int modifier_state, Time time)
{
  if (NILP (XXWIDGET (view->model)->buffer))
    return;

  record_osr_embedder (view);

  if (button < 4 || button > 8)
    xwidget_button_1 (view, down_p, x, y, button, modifier_state, time);
  else
    {
      if (!down_p)
	{
	  GdkEvent *xg_event = gdk_event_new (GDK_SCROLL);
	  struct xwidget *model = XXWIDGET (view->model);
	  GtkWidget *target;

	  x += view->clip_left;
	  y += view->clip_top;

	  target = find_widget_at_pos (model->widgetwindow_osr, x, y, &x, &y,
				       true, view);

	  if (!target)
	    target = model->widget_osr;

	  xg_event->any.window = gtk_widget_get_window (target);
	  g_object_ref (xg_event->any.window); /* The window will be unrefed
						  later by gdk_event_free.  */
	  if (button == 4)
	    xg_event->scroll.direction = GDK_SCROLL_UP;
	  else if (button == 5)
	    xg_event->scroll.direction = GDK_SCROLL_DOWN;
	  else if (button == 6)
	    xg_event->scroll.direction = GDK_SCROLL_LEFT;
	  else
	    xg_event->scroll.direction = GDK_SCROLL_RIGHT;

	  xg_event->scroll.device = find_suitable_pointer (view->frame,
							   false);

	  xg_event->scroll.x = x;
	  xg_event->scroll.x_root = x;
	  xg_event->scroll.y = y;
	  xg_event->scroll.y_root = y;
	  xg_event->scroll.state = modifier_state;
	  xg_event->scroll.time = time;

	  xg_event->scroll.delta_x = 0;
	  xg_event->scroll.delta_y = 0;

	  gtk_main_do_event (xg_event);
	  gdk_event_free (xg_event);
	}
    }
}

#ifdef HAVE_XINPUT2
void
xwidget_motion_notify (struct xwidget_view *view,
		       double x, double y,
		       double root_x, double root_y,
		       uint state, Time time)
{
  GdkEvent *xg_event;
  GtkWidget *target;
  struct xwidget *model = XXWIDGET (view->model);
  int target_x, target_y;

  if (NILP (model->buffer))
    return;

  record_osr_embedder (view);

  target = find_widget_at_pos (model->widgetwindow_osr,
			       lrint (x + view->clip_left),
			       lrint (y + view->clip_top),
			       &target_x, &target_y,
			       true, view);

  if (!target)
    {
      target_x = lrint (x + view->clip_left);
      target_y = lrint (y + view->clip_top);
      target = model->widget_osr;
    }
  else if (xw_maybe_synthesize_crossing (view, gtk_widget_get_window (target),
					 x + view->clip_left, y + view->clip_top,
					 XW_CROSSING_NONE, time, state,
					 (view->passive_grab
					  ? GDK_CROSSING_GRAB
					  : GDK_CROSSING_NORMAL),
					 GDK_CROSSING_NORMAL))
    return;

  xg_event = gdk_event_new (GDK_MOTION_NOTIFY);
  xg_event->any.window = gtk_widget_get_window (target);
  xg_event->motion.x = target_x;
  xg_event->motion.y = target_y;
  xg_event->motion.x_root = root_x;
  xg_event->motion.y_root = root_y;
  xg_event->motion.time = time;
  xg_event->motion.state = state;
  xg_event->motion.device = find_suitable_pointer (view->frame, false);

  g_object_ref (xg_event->any.window);

  gtk_main_do_event (xg_event);
  gdk_event_free (xg_event);
}

void
xwidget_scroll (struct xwidget_view *view, double x, double y,
		double dx, double dy, uint state, Time time,
		bool stop_p)
{
  GdkEvent *xg_event;
  GtkWidget *target;
  struct xwidget *model = XXWIDGET (view->model);
  int target_x, target_y;

  if (NILP (model->buffer))
    return;

  record_osr_embedder (view);

  target = find_widget_at_pos (model->widgetwindow_osr,
			       lrint (x + view->clip_left),
			       lrint (y + view->clip_top),
			       &target_x, &target_y,
			       true, view);

  if (!target)
    {
      target_x = lrint (x);
      target_y = lrint (y);
      target = model->widget_osr;
    }

  xg_event = gdk_event_new (GDK_SCROLL);
  xg_event->any.window = gtk_widget_get_window (target);
  xg_event->scroll.direction = GDK_SCROLL_SMOOTH;
  xg_event->scroll.x = target_x;
  xg_event->scroll.y = target_y;
  xg_event->scroll.x_root = lrint (x);
  xg_event->scroll.y_root = lrint (y);
  xg_event->scroll.time = time;
  xg_event->scroll.state = state;
  xg_event->scroll.delta_x = dx;
  xg_event->scroll.delta_y = dy;
  xg_event->scroll.device = find_suitable_pointer (view->frame, true);
  xg_event->scroll.is_stop = stop_p;

  g_object_ref (xg_event->any.window);

  gtk_main_do_event (xg_event);
  gdk_event_free (xg_event);
}

#ifdef HAVE_XINPUT2_4
void
xwidget_pinch (struct xwidget_view *view, XIGesturePinchEvent *xev)
{
#if GTK_CHECK_VERSION (3, 18, 0)
  GdkEvent *xg_event;
  GtkWidget *target;
  struct xwidget *model = XXWIDGET (view->model);
  int target_x, target_y;
  double x = xev->event_x;
  double y = xev->event_y;

  if (NILP (model->buffer))
    return;

  record_osr_embedder (view);

  target = find_widget_at_pos (model->widgetwindow_osr,
			       lrint (x + view->clip_left),
			       lrint (y + view->clip_top),
			       &target_x, &target_y,
			       true, view);

  if (!target)
    {
      target_x = lrint (x);
      target_y = lrint (y);
      target = model->widget_osr;
    }

  xg_event = gdk_event_new (GDK_TOUCHPAD_PINCH);
  xg_event->any.window = gtk_widget_get_window (target);
  xg_event->touchpad_pinch.x = target_x;
  xg_event->touchpad_pinch.y = target_y;
  xg_event->touchpad_pinch.dx = xev->delta_x;
  xg_event->touchpad_pinch.dy = xev->delta_y;
  xg_event->touchpad_pinch.angle_delta = xev->delta_angle;
  xg_event->touchpad_pinch.scale = xev->scale;
  xg_event->touchpad_pinch.x_root = xev->root_x;
  xg_event->touchpad_pinch.y_root = xev->root_y;
  xg_event->touchpad_pinch.state = xev->mods.effective;
  xg_event->touchpad_pinch.n_fingers = 2;

  switch (xev->evtype)
    {
    case XI_GesturePinchBegin:
      xg_event->touchpad_pinch.phase = GDK_TOUCHPAD_GESTURE_PHASE_BEGIN;
      break;
    case XI_GesturePinchUpdate:
      xg_event->touchpad_pinch.phase = GDK_TOUCHPAD_GESTURE_PHASE_UPDATE;
      break;
    case XI_GesturePinchEnd:
      xg_event->touchpad_pinch.phase = GDK_TOUCHPAD_GESTURE_PHASE_END;
      break;
    }

  gdk_event_set_device (xg_event, find_suitable_pointer (view->frame, false));

  g_object_ref (xg_event->any.window);
  gtk_main_do_event (xg_event);
  gdk_event_free (xg_event);
#endif
}
#endif
#endif

#ifdef HAVE_XINPUT2
static GdkNotifyType
xi_translate_notify_detail (int detail)
{
  switch (detail)
    {
    case XINotifyInferior:
      return GDK_NOTIFY_INFERIOR;
    case XINotifyAncestor:
      return GDK_NOTIFY_ANCESTOR;
    case XINotifyVirtual:
      return GDK_NOTIFY_VIRTUAL;
    case XINotifyNonlinear:
      return GDK_NOTIFY_NONLINEAR;
    case XINotifyNonlinearVirtual:
      return GDK_NOTIFY_NONLINEAR_VIRTUAL;
    default:
      emacs_abort ();
    }
}
#endif

static void
window_coords_from_toplevel (GdkWindow *window, GdkWindow *toplevel,
			     int x, int y, int *out_x, int *out_y)
{
  GdkWindow *parent;
  GList *children, *l;
  gdouble x_out, y_out;

  if (window == toplevel)
    {
      *out_x = x;
      *out_y = y;
      return;
    }

  children = NULL;
  while ((parent = gdk_window_get_parent (window)) != toplevel)
    {
      children = g_list_prepend (children, window);
      window = parent;
    }

  for (l = children; l != NULL; l = l->next)
    gdk_window_coords_from_parent (l->data, x, y, &x_out, &y_out);

  g_list_free (children);

  *out_x = x_out;
  *out_y = y_out;
}

static GdkWindow *
xw_find_common_ancestor (GdkWindow *window,
			 GdkWindow *other,
			 GdkWindow *toplevel)
{
  GdkWindow *tem;
  GList *l1 = NULL;
  GList *l2 = NULL;
  GList *i1, *i2;

  tem = window;
  while (tem && tem != toplevel)
    {
      l1 = g_list_prepend (l1, tem);
      tem = gdk_window_get_parent (tem);
    }

  tem = other;
  while (tem && tem != toplevel)
    {
      l2 = g_list_prepend (l2, tem);
      tem = gdk_window_get_parent (tem);
    }

  tem = NULL;
  i1 = l1;
  i2 = l2;

  while (i1 && i2 && (i1->data == i2->data))
    {
      tem = i1->data;
      i1 = i1->next;
      i2 = i2->next;
    }

  g_list_free (l1);
  g_list_free (l2);

  return tem;
}

static void
xw_notify_virtual_upwards_until (struct xwidget_view *xv,
				 GdkWindow *window,
				 GdkWindow *until,
				 GdkWindow *toplevel,
				 unsigned int state,
				 int x, int y, Time time,
				 GdkEventType type,
				 bool nonlinear_p,
				 GdkCrossingMode crossing)
{
  GdkEvent *xg_event;
  GdkWindow *tem;
  int cx, cy;

  for (tem = gdk_window_get_parent (window);
       tem && (tem != until);
       tem = gdk_window_get_parent (tem))
    {
      xg_event = gdk_event_new (type);

      gdk_event_set_device (xg_event,
			    find_suitable_pointer (xv->frame, false));
      window_coords_from_toplevel (tem, toplevel, x, y, &cx, &cy);
      xg_event->crossing.x = cx;
      xg_event->crossing.y = cy;
      xg_event->crossing.time = time;
      xg_event->crossing.focus = FALSE;
      xg_event->crossing.detail = (nonlinear_p
				   ? GDK_NOTIFY_NONLINEAR_VIRTUAL
				   : GDK_NOTIFY_VIRTUAL);
      xg_event->crossing.mode = crossing;
      xg_event->crossing.window = g_object_ref (tem);

      gtk_main_do_event (xg_event);
      gdk_event_free (xg_event);
    }
}

static void
xw_notify_virtual_downwards_until (struct xwidget_view *xv,
				   GdkWindow *window,
				   GdkWindow *until,
				   GdkWindow *toplevel,
				   unsigned int state,
				   int x, int y, Time time,
				   GdkEventType type,
				   bool nonlinear_p,
				   GdkCrossingMode crossing)
{
  GdkEvent *xg_event;
  GdkWindow *tem;
  int cx, cy;
  GList *path = NULL, *it;

  tem = gdk_window_get_parent (window);

  while (tem && tem != until)
    {
      path = g_list_prepend (path, tem);
      tem = gdk_window_get_parent (tem);
    }

  for (it = path; it; it = it->next)
    {
      tem = it->data;
      xg_event = gdk_event_new (type);

      gdk_event_set_device (xg_event,
			    find_suitable_pointer (xv->frame, false));
      window_coords_from_toplevel (tem, toplevel, x, y, &cx, &cy);
      xg_event->crossing.x = cx;
      xg_event->crossing.y = cy;
      xg_event->crossing.time = time;
      xg_event->crossing.focus = FALSE;
      xg_event->crossing.detail = (nonlinear_p
				   ? GDK_NOTIFY_NONLINEAR_VIRTUAL
				   : GDK_NOTIFY_VIRTUAL);
      xg_event->crossing.mode = crossing;
      xg_event->crossing.window = g_object_ref (tem);

      gtk_main_do_event (xg_event);
      gdk_event_free (xg_event);
    }

  g_list_free (path);
}

static void
xw_update_cursor_for_view (struct xwidget_view *xv,
			   GdkWindow *crossing_window)
{
  GdkCursor *xg_cursor;
  Cursor cursor;

  xg_cursor = gdk_window_get_cursor (crossing_window);

  if (xg_cursor)
    {
      cursor = gdk_x11_cursor_get_xcursor (xg_cursor);

      if (gdk_x11_cursor_get_xdisplay (xg_cursor) == xv->dpy)
	xv->cursor = cursor;
    }
  else
    xv->cursor = FRAME_OUTPUT_DATA (xv->frame)->nontext_cursor;

  if (xv->wdesc != None)
    XDefineCursor (xv->dpy, xv->wdesc, xv->cursor);
}

static void
xw_last_crossing_cursor_cb (GdkWindow *window,
			    GParamSpec *spec,
			    gpointer user_data)
{
  xw_update_cursor_for_view (user_data, window);
}

static bool
xw_maybe_synthesize_crossing (struct xwidget_view *view,
			      GdkWindow *current_window,
			      int x, int y, int crossing,
			      Time time, unsigned int state,
			      GdkCrossingMode entry_crossing,
			      GdkCrossingMode exit_crossing)
{
  GdkWindow *last_crossing, *toplevel, *ancestor;
  GdkEvent *xg_event;
  int cx, cy;
  bool nonlinear_p;
  bool retention_flag;

#if WEBKIT_CHECK_VERSION (2, 34, 0)
  /* Work around a silly bug in WebKitGTK+ that tries to make tooltip
     windows transient for our offscreen window.  */
  int tooltip_width, tooltip_height;

  xg_prepare_tooltip (view->frame, dummy_tooltip_string,
		      &tooltip_width, &tooltip_height);
#endif

  toplevel = gtk_widget_get_window (XXWIDGET (view->model)->widgetwindow_osr);
  retention_flag = false;

  if (crossing == XW_CROSSING_LEFT
      && (view->last_crossing_window
	  && !gdk_window_is_destroyed (view->last_crossing_window)))
    {
      xw_notify_virtual_upwards_until (view, view->last_crossing_window,
				       toplevel, toplevel,
				       state, x, y, time,
				       GDK_LEAVE_NOTIFY, false,
				       exit_crossing);
    }

  if (view->last_crossing_window
      && (gdk_window_is_destroyed (view->last_crossing_window)
	  || crossing == XW_CROSSING_LEFT))
    {
      if (!gdk_window_is_destroyed (view->last_crossing_window)
	  && view->last_crossing_window != toplevel)
	{
	  xg_event = gdk_event_new (GDK_LEAVE_NOTIFY);
	  window_coords_from_toplevel (view->last_crossing_window,
				       toplevel, x, y, &cx, &cy);

	  xg_event->crossing.x = cx;
	  xg_event->crossing.y = cy;
	  xg_event->crossing.time = time;
	  xg_event->crossing.focus = FALSE;
	  xg_event->crossing.detail = GDK_NOTIFY_ANCESTOR;
	  xg_event->crossing.mode = exit_crossing;
	  xg_event->crossing.window = g_object_ref (view->last_crossing_window);
	  gdk_event_set_device (xg_event,
				find_suitable_pointer (view->frame, false));

	  gtk_main_do_event (xg_event);
	  gdk_event_free (xg_event);

	  xw_notify_virtual_upwards_until (view, view->last_crossing_window,
					   gdk_window_get_parent (toplevel),
					   toplevel, state, x, y, time,
					   GDK_LEAVE_NOTIFY, false, exit_crossing);
	  retention_flag = true;
	}

      g_signal_handler_disconnect (view->last_crossing_window,
				   view->last_crossing_cursor_signal);
      g_clear_pointer (&view->last_crossing_window,
		       g_object_unref);
    }
  last_crossing = view->last_crossing_window;

  if (!last_crossing)
    {
      if (current_window)
	{
	  view->last_crossing_window = g_object_ref (current_window);
	  xw_update_cursor_for_view (view, current_window);
	  view->last_crossing_cursor_signal
	    = g_signal_connect (G_OBJECT (current_window), "notify::cursor",
				G_CALLBACK (xw_last_crossing_cursor_cb), view);

	  xw_notify_virtual_downwards_until (view, current_window,
					     toplevel, toplevel,
					     state, x, y, time,
					     GDK_ENTER_NOTIFY,
					     false, entry_crossing);
	}
      return retention_flag;
    }

  if (last_crossing != current_window)
    {
      view->last_crossing_window = g_object_ref (current_window);
      g_signal_handler_disconnect (last_crossing, view->last_crossing_cursor_signal);

      xw_update_cursor_for_view (view, current_window);
      view->last_crossing_cursor_signal
	= g_signal_connect (G_OBJECT (current_window), "notify::cursor",
			    G_CALLBACK (xw_last_crossing_cursor_cb), view);

      ancestor = xw_find_common_ancestor (last_crossing, current_window, toplevel);

      if (!ancestor)
	emacs_abort ();

      nonlinear_p = (last_crossing != ancestor) && (current_window != ancestor);

      if (nonlinear_p || (last_crossing != ancestor))
	xw_notify_virtual_upwards_until (view, last_crossing,
					 ancestor, toplevel,
					 state, x, y, time,
					 GDK_LEAVE_NOTIFY,
					 nonlinear_p,
					 exit_crossing);

      xg_event = gdk_event_new (GDK_LEAVE_NOTIFY);
      gdk_event_set_device (xg_event,
			    find_suitable_pointer (view->frame, false));
      window_coords_from_toplevel (last_crossing, toplevel,
				   x, y, &cx, &cy);
      xg_event->crossing.x = cx;
      xg_event->crossing.y = cy;
      xg_event->crossing.time = time;
      xg_event->crossing.focus = FALSE;
      xg_event->crossing.state = state;
      xg_event->crossing.detail = (nonlinear_p
				   ? GDK_NOTIFY_NONLINEAR
				   : (last_crossing == ancestor
				      ? GDK_NOTIFY_INFERIOR
				      : GDK_NOTIFY_ANCESTOR));
      xg_event->crossing.mode = exit_crossing;
      xg_event->crossing.window = g_object_ref (last_crossing);

      gtk_main_do_event (xg_event);
      gdk_event_free (xg_event);

      if (nonlinear_p || (current_window != ancestor))
	xw_notify_virtual_downwards_until (view, current_window,
					   ancestor, toplevel,
					   state, x, y, time,
					   GDK_ENTER_NOTIFY,
					   nonlinear_p,
					   entry_crossing);

      xg_event = gdk_event_new (GDK_ENTER_NOTIFY);
      gdk_event_set_device (xg_event,
			    find_suitable_pointer (view->frame, false));
      window_coords_from_toplevel (current_window, toplevel,
				   x, y, &cx, &cy);
      xg_event->crossing.x = cx;
      xg_event->crossing.y = cy;
      xg_event->crossing.time = time;
      xg_event->crossing.focus = FALSE;
      xg_event->crossing.state = state;
      xg_event->crossing.detail = (nonlinear_p
				   ? GDK_NOTIFY_NONLINEAR
				   : (current_window == ancestor
				      ? GDK_NOTIFY_INFERIOR
				      : GDK_NOTIFY_ANCESTOR));
      xg_event->crossing.mode = entry_crossing;
      xg_event->crossing.window = g_object_ref (current_window);

      gtk_main_do_event (xg_event);
      gdk_event_free (xg_event);
      g_object_unref (last_crossing);

      return true;
    }

  return false;
}

void
xwidget_motion_or_crossing (struct xwidget_view *view, const XEvent *event)
{
  GdkEvent *xg_event;
  struct xwidget *model = XXWIDGET (view->model);
  int x, y, toplevel_x, toplevel_y;
  GtkWidget *target;
#ifdef HAVE_XINPUT2
  XIEnterEvent *xev = NULL;
#endif

  if (NILP (model->buffer))
    return;

#ifdef HAVE_XINPUT2
  if (event->type != GenericEvent)
#endif
    {
      xg_event = gdk_event_new (event->type == MotionNotify
				? GDK_MOTION_NOTIFY
				: (event->type == LeaveNotify
				   ? GDK_LEAVE_NOTIFY
				   : GDK_ENTER_NOTIFY));
      toplevel_x = (event->type == MotionNotify
		    ? event->xmotion.x + view->clip_left
		    : event->xcrossing.x + view->clip_left);
      toplevel_y = (event->type == MotionNotify
		    ? event->xmotion.y + view->clip_top
		    : event->xcrossing.y + view->clip_top);
      target = find_widget_at_pos (model->widgetwindow_osr,
				   toplevel_x, toplevel_y, &x, &y,
				   true, view);
    }
#ifdef HAVE_XINPUT2
  else
    {
      eassert (event->xcookie.evtype == XI_Enter
	       || event->xcookie.evtype == XI_Leave);

      xev = (XIEnterEvent *) event->xcookie.data;
      xg_event = gdk_event_new (event->type == XI_Enter
				? GDK_ENTER_NOTIFY
				: GDK_LEAVE_NOTIFY);
      target = find_widget_at_pos (model->widgetwindow_osr,
				   (toplevel_x
				    = lrint (xev->event_x + view->clip_left)),
				   (toplevel_y
				    = lrint (xev->event_y + view->clip_top)),
				   &x, &y, true, view);
    }
#endif

  if (!target)
    target = model->widget_osr;

  record_osr_embedder (view);
  xg_event->any.window = gtk_widget_get_window (target);
  g_object_ref (xg_event->any.window); /* The window will be unrefed
					  later by gdk_event_free.  */

  if (event->type == MotionNotify)
    {
      if (!xw_maybe_synthesize_crossing (view, xg_event->any.window,
					 toplevel_x, toplevel_y,
					 XW_CROSSING_NONE, event->xmotion.time,
					 event->xmotion.state,
					 (view->passive_grab
					  ? GDK_CROSSING_GRAB
					  : GDK_CROSSING_NORMAL),
					 GDK_CROSSING_NORMAL))
	{
	  xg_event->motion.x = x;
	  xg_event->motion.y = y;
	  xg_event->motion.x_root = event->xmotion.x_root;
	  xg_event->motion.y_root = event->xmotion.y_root;
	  xg_event->motion.time = event->xmotion.time;
	  xg_event->motion.state = event->xmotion.state;
	  xg_event->motion.device
	    = find_suitable_pointer (view->frame, false);
	}
      else
	{
	  gdk_event_free (xg_event);
	  return;
	}
    }
#ifdef HAVE_XINPUT2
  else if (event->type == GenericEvent)
    {
      xg_event->crossing.x = x;
      xg_event->crossing.y = y;
      xg_event->crossing.x_root = (gdouble) xev->root_x;
      xg_event->crossing.y_root = (gdouble) xev->root_y;
      xg_event->crossing.time = xev->time;
      xg_event->crossing.focus = xev->focus;
      xg_event->crossing.mode = xev->mode;
      xg_event->crossing.detail = xi_translate_notify_detail (xev->detail);
      xg_event->crossing.state = xev->mods.effective;

      if (xev->buttons.mask_len)
	{
	  if (XIMaskIsSet (xev->buttons.mask, 1))
	    xg_event->crossing.state |= GDK_BUTTON1_MASK;
	  if (XIMaskIsSet (xev->buttons.mask, 2))
	    xg_event->crossing.state |= GDK_BUTTON2_MASK;
	  if (XIMaskIsSet (xev->buttons.mask, 3))
	    xg_event->crossing.state |= GDK_BUTTON3_MASK;
	}

      if (view->passive_grab
	  || xw_maybe_synthesize_crossing (view, xg_event->any.window,
					   toplevel_x, toplevel_y,
					   (xev->type == XI_Enter
					    ? XW_CROSSING_ENTERED
					    : XW_CROSSING_LEFT),
					   xev->time, xg_event->crossing.state,
					   (view->passive_grab
					    ? GDK_CROSSING_GRAB
					    : GDK_CROSSING_NORMAL),
					   GDK_CROSSING_NORMAL))
	{
	  gdk_event_free (xg_event);
	  return;
	}

      gdk_event_set_device (xg_event,
			    find_suitable_pointer (view->frame, false));
    }
#endif
  else
    {
      if (view->passive_grab
	  || xw_maybe_synthesize_crossing (view, xg_event->any.window,
					   toplevel_x, toplevel_y,
					   (event->type == EnterNotify
					    ? XW_CROSSING_ENTERED
					    : XW_CROSSING_LEFT),
					   event->xcrossing.time,
					   event->xcrossing.state,
					   (view->passive_grab
					    ? GDK_CROSSING_GRAB
					    : GDK_CROSSING_NORMAL),
					   GDK_CROSSING_NORMAL))
	{
	  gdk_event_free (xg_event);
	  return;
	}

      xg_event->crossing.detail = min (5, event->xcrossing.detail);
      xg_event->crossing.time = event->xcrossing.time;
      xg_event->crossing.x = x;
      xg_event->crossing.y = y;
      xg_event->crossing.x_root = event->xcrossing.x_root;
      xg_event->crossing.y_root = event->xcrossing.y_root;
      xg_event->crossing.focus = event->xcrossing.focus;
      gdk_event_set_device (xg_event,
			    find_suitable_pointer (view->frame, false));
    }

  gtk_main_do_event (xg_event);
  gdk_event_free (xg_event);
}

#endif /* HAVE_X_WINDOWS */

static void
synthesize_focus_in_event (GtkWidget *offscreen_window)
{
  GdkWindow *wnd;
  GdkEvent *focus_event;

  if (!gtk_widget_get_realized (offscreen_window))
    gtk_widget_realize (offscreen_window);

  wnd = gtk_widget_get_window (offscreen_window);

  focus_event = gdk_event_new (GDK_FOCUS_CHANGE);
  focus_event->focus_change.window = wnd;
  focus_event->focus_change.in = TRUE;

  if (FRAME_WINDOW_P (SELECTED_FRAME ()))
    gdk_event_set_device (focus_event,
			  find_suitable_pointer (SELECTED_FRAME (),
						 false));

  g_object_ref (wnd);

  gtk_main_do_event (focus_event);
  gdk_event_free (focus_event);
}

#ifdef HAVE_X_WINDOWS
struct xwidget_view *
xwidget_view_from_window (Window wdesc)
{
  Lisp_Object key = make_fixnum (wdesc);
  Lisp_Object xwv = Fgethash (key, x_window_to_xwv_map, Qnil);

  if (NILP (xwv))
    return NULL;

  return XXWIDGET_VIEW (xwv);
}
#endif

static void
xwidget_show_view (struct xwidget_view *xv)
{
  xv->hidden = false;
#ifdef HAVE_X_WINDOWS
  XMoveWindow (xv->dpy, xv->wdesc,
	       xv->x + xv->clip_left,
	       xv->y + xv->clip_top);
  XMapWindow (xv->dpy, xv->wdesc);
  XFlush (xv->dpy);
#else
  gtk_fixed_move (GTK_FIXED (FRAME_GTK_WIDGET (xv->frame)),
		  xv->widget, xv->x + xv->clip_left,
		  xv->y + xv->clip_top);
  gtk_widget_show_all (xv->widget);
#endif
}

/* Hide an xwidget view.  */
static void
xwidget_hide_view (struct xwidget_view *xv)
{
  xv->hidden = true;
#ifdef HAVE_X_WINDOWS
  XUnmapWindow (xv->dpy, xv->wdesc);
  XFlush (xv->dpy);
#else
  gtk_widget_hide (xv->widget);
#endif
}

#ifndef HAVE_PGTK
static void
xv_do_draw (struct xwidget_view *xw, struct xwidget *w)
{
  GtkOffscreenWindow *wnd;
  cairo_surface_t *surface;

  if (xw->just_resized)
    return;

  if (NILP (w->buffer))
    {
      XClearWindow (xw->dpy, xw->wdesc);
      return;
    }

  block_input ();
  wnd = GTK_OFFSCREEN_WINDOW (w->widgetwindow_osr);
  surface = gtk_offscreen_window_get_surface (wnd);

  cairo_save (xw->cr_context);
  if (surface)
    {
      cairo_translate (xw->cr_context, -xw->clip_left, -xw->clip_top);
      cairo_set_source_surface (xw->cr_context, surface, 0, 0);
      cairo_set_operator (xw->cr_context, CAIRO_OPERATOR_SOURCE);
      cairo_paint (xw->cr_context);
    }
  cairo_restore (xw->cr_context);

  unblock_input ();
}
#else
static void
xwidget_view_draw_cb (GtkWidget *widget, cairo_t *cr,
		      gpointer data)
{
  struct xwidget_view *view = data;
  struct xwidget *w = XXWIDGET (view->model);
  GtkOffscreenWindow *wnd;
  cairo_surface_t *surface;

  if (NILP (w->buffer))
    return;

  block_input ();
  wnd = GTK_OFFSCREEN_WINDOW (w->widgetwindow_osr);
  surface = gtk_offscreen_window_get_surface (wnd);

  cairo_save (cr);
  if (surface)
    {
      cairo_translate (cr, -view->clip_left,
		       -view->clip_top);
      cairo_set_source_surface (cr, surface, 0, 0);
      cairo_set_operator (cr, CAIRO_OPERATOR_SOURCE);
      cairo_paint (cr);
    }
  cairo_restore (cr);

  unblock_input ();
}
#endif

/* When the off-screen webkit master view changes this signal is called.
   It copies the bitmap from the off-screen instance.  */
static gboolean
offscreen_damage_event (GtkWidget *widget, GdkEvent *event,
                        gpointer xwidget)
{
  block_input ();

  for (Lisp_Object tail = internal_xwidget_view_list; CONSP (tail);
       tail = XCDR (tail))
    {
      if (XWIDGET_VIEW_P (XCAR (tail)))
	{
	  struct xwidget_view *view = XXWIDGET_VIEW (XCAR (tail));
#ifdef HAVE_X_WINDOWS
	  if (view->wdesc && XXWIDGET (view->model) == xwidget)
	    xv_do_draw (view, XXWIDGET (view->model));
#else
	  gtk_widget_queue_draw (view->widget);
#endif
	}
    }

  unblock_input ();

  return FALSE;
}

#ifdef HAVE_X_WINDOWS
void
xwidget_expose (struct xwidget_view *xv)
{
  struct xwidget *xw = XXWIDGET (xv->model);

  xv_do_draw (xv, xw);
}
#endif
#endif /* USE_GTK */

void
store_xwidget_event_string (struct xwidget *xw, const char *eventname,
                            const char *eventstr)
{
  struct input_event event;
  Lisp_Object xwl;
  XSETXWIDGET (xwl, xw);
  EVENT_INIT (event);
  event.kind = XWIDGET_EVENT;
  event.frame_or_window = Qnil;
  event.arg = list3 (intern (eventname), xwl, build_string (eventstr));
  kbd_buffer_store_event (&event);
}

void
store_xwidget_download_callback_event (struct xwidget *xw,
                                       const char *url,
                                       const char *mimetype,
                                       const char *filename)
{
  struct input_event event;
  Lisp_Object xwl;
  XSETXWIDGET (xwl, xw);
  EVENT_INIT (event);
  event.kind = XWIDGET_EVENT;
  event.frame_or_window = Qnil;
  event.arg = list5 (Qdownload_callback,
                     xwl,
                     build_string (url),
                     build_string (mimetype),
                     build_string (filename));
  kbd_buffer_store_event (&event);
}

void
store_xwidget_js_callback_event (struct xwidget *xw,
                                 Lisp_Object proc,
                                 Lisp_Object argument)
{
  struct input_event event;
  Lisp_Object xwl;
  XSETXWIDGET (xwl, xw);
  EVENT_INIT (event);
  event.kind = XWIDGET_EVENT;
  event.frame_or_window = Qnil;
  event.arg = list4 (Qjavascript_callback, xwl, proc, argument);
  kbd_buffer_store_event (&event);
}


#ifdef USE_GTK
static void
store_xwidget_display_event (struct xwidget *xw,
			     struct xwidget *src)
{
  struct input_event evt;
  Lisp_Object val, src_val;

  XSETXWIDGET (val, xw);
  XSETXWIDGET (src_val, src);
  EVENT_INIT (evt);
  evt.kind = XWIDGET_DISPLAY_EVENT;
  evt.frame_or_window = Qnil;
  evt.arg = list2 (val, src_val);
  kbd_buffer_store_event (&evt);
}

static void
webkit_ready_to_show (WebKitWebView *new_view,
		      gpointer user_data)
{
  Lisp_Object tem;
  struct xwidget *xw;
  struct xwidget *src;

  src = find_xwidget_for_offscreen_window (GDK_WINDOW (user_data));

  for (tem = internal_xwidget_list; CONSP (tem); tem = XCDR (tem))
    {
      if (XWIDGETP (XCAR (tem)))
	{
	  xw = XXWIDGET (XCAR (tem));

	  if (EQ (xw->type, Qwebkit)
	      && WEBKIT_WEB_VIEW (xw->widget_osr) == new_view)
	    {
	      /* The source widget was destroyed before we had a
		 chance to display the new widget.  */
	      if (!src)
		kill_xwidget (xw);
	      else
		store_xwidget_display_event (xw, src);
	    }
	}
    }
}

static GtkWidget *
webkit_create_cb_1 (WebKitWebView *webview,
		    struct xwidget *xv)
{
  Lisp_Object related;
  Lisp_Object xwidget;
  GtkWidget *widget;

  XSETXWIDGET (related, xv);
  xwidget = Fmake_xwidget (Qwebkit, Qnil, make_fixnum (0),
			   make_fixnum (0), Qnil,
			   build_string (" *detached xwidget buffer*"),
			   related);

  if (NILP (xwidget))
    return NULL;

  widget = XXWIDGET (xwidget)->widget_osr;

  g_signal_connect (G_OBJECT (widget), "ready-to-show",
		    G_CALLBACK (webkit_ready_to_show),
		    gtk_widget_get_window (xv->widgetwindow_osr));

  return widget;
}

static GtkWidget *
webkit_create_cb (WebKitWebView *webview,
		  WebKitNavigationAction *nav_action,
		  gpointer user_data)
{
  switch (webkit_navigation_action_get_navigation_type (nav_action))
    {
    case WEBKIT_NAVIGATION_TYPE_OTHER:
      return webkit_create_cb_1 (webview, user_data);

    case WEBKIT_NAVIGATION_TYPE_BACK_FORWARD:
    case WEBKIT_NAVIGATION_TYPE_RELOAD:
    case WEBKIT_NAVIGATION_TYPE_FORM_SUBMITTED:
    case WEBKIT_NAVIGATION_TYPE_FORM_RESUBMITTED:
    case WEBKIT_NAVIGATION_TYPE_LINK_CLICKED:
    default:
      return NULL;
    }
}

void
webkit_view_load_changed_cb (WebKitWebView *webkitwebview,
                             WebKitLoadEvent load_event,
                             gpointer data)
{
  struct xwidget *xw = g_object_get_data (G_OBJECT (webkitwebview),
					  XG_XWIDGET);

  switch (load_event)
    {
    case WEBKIT_LOAD_FINISHED:
      store_xwidget_event_string (xw, "load-changed", "load-finished");
      break;
    case WEBKIT_LOAD_STARTED:
      store_xwidget_event_string (xw, "load-changed", "load-started");
      break;
    case WEBKIT_LOAD_REDIRECTED:
      store_xwidget_event_string (xw, "load-changed", "load-redirected");
      break;
    case WEBKIT_LOAD_COMMITTED:
      store_xwidget_event_string (xw, "load-changed", "load-committed");
      break;
    }
}

/* Recursively convert a JavaScript value to a Lisp value. */
static Lisp_Object
webkit_js_to_lisp (JSCValue *value)
{
  if (jsc_value_is_string (value))
    {
      gchar *str_value = jsc_value_to_string (value);
      Lisp_Object ret = build_string (str_value);
      g_free (str_value);

      return ret;
    }
  else if (jsc_value_is_boolean (value))
    {
      return (jsc_value_to_boolean (value)) ? Qt : Qnil;
    }
  else if (jsc_value_is_number (value))
    {
      return make_fixnum (jsc_value_to_int32 (value));
    }
  else if (jsc_value_is_array (value))
    {
      JSCValue *len = jsc_value_object_get_property (value, "length");
      const gint32 dlen = jsc_value_to_int32 (len);

      Lisp_Object obj;
      if (! (0 <= dlen && dlen < G_MAXINT32))
	memory_full (SIZE_MAX);

      ptrdiff_t n = dlen;
      struct Lisp_Vector *p = allocate_nil_vector (n);

      for (ptrdiff_t i = 0; i < n; ++i)
	{
	  p->contents[i] =
	    webkit_js_to_lisp (jsc_value_object_get_property_at_index (value, i));
	}
      XSETVECTOR (obj, p);
      return obj;
    }
  else if (jsc_value_is_object (value))
    {
      char **properties_names = jsc_value_object_enumerate_properties (value);
      guint n = g_strv_length (properties_names);

      Lisp_Object obj;
      if (PTRDIFF_MAX < n)
	memory_full (n);
      struct Lisp_Vector *p = allocate_nil_vector (n);

      for (ptrdiff_t i = 0; i < n; ++i)
	{
	  const char *name = properties_names[i];
	  JSCValue *property = jsc_value_object_get_property (value, name);

	  p->contents[i] =
	    Fcons (build_string (name), webkit_js_to_lisp (property));
	}

      g_strfreev (properties_names);

      XSETVECTOR (obj, p);
      return obj;
    }

  return Qnil;
}

static void
webkit_javascript_finished_cb (GObject      *webview,
                               GAsyncResult *result,
                               gpointer      arg)
{
  GError *error = NULL;
  struct xwidget *xw = g_object_get_data (G_OBJECT (webview), XG_XWIDGET);

  ptrdiff_t script_idx = (intptr_t) arg;
  Lisp_Object script_callback = AREF (xw->script_callbacks, script_idx);
  ASET (xw->script_callbacks, script_idx, Qnil);
  if (!NILP (script_callback))
    xfree (xmint_pointer (XCAR (script_callback)));

  WebKitJavascriptResult *js_result =
    webkit_web_view_run_javascript_finish
    (WEBKIT_WEB_VIEW (webview), result, &error);

  if (!js_result)
    {
      if (error)
	g_error_free (error);
      return;
    }

  if (!NILP (script_callback) && !NILP (XCDR (script_callback)))
    {
      JSCValue *value = webkit_javascript_result_get_js_value (js_result);

      Lisp_Object lisp_value = webkit_js_to_lisp (value);

      /* Register an xwidget event here, which then runs the callback.
	 This ensures that the callback runs in sync with the Emacs
	 event loop.  */
      store_xwidget_js_callback_event (xw, XCDR (script_callback), lisp_value);
    }

  webkit_javascript_result_unref (js_result);
}


gboolean
webkit_download_cb (WebKitWebContext *webkitwebcontext,
                    WebKitDownload *arg1,
                    gpointer data)
{
  WebKitWebView *view = webkit_download_get_web_view(arg1);
  WebKitURIRequest *request = webkit_download_get_request(arg1);
  struct xwidget *xw = g_object_get_data (G_OBJECT (view),
                                          XG_XWIDGET);

  store_xwidget_event_string (xw, "download-started",
                              webkit_uri_request_get_uri(request));
  return FALSE;
}

static gboolean
webkit_decide_policy_cb (WebKitWebView *webView,
                         WebKitPolicyDecision *decision,
                         WebKitPolicyDecisionType type,
                         gpointer user_data)
{
  switch (type) {
  case WEBKIT_POLICY_DECISION_TYPE_RESPONSE:
    /* This function makes webkit send a download signal for all unknown
       mime types.  TODO: Defer the decision to Lisp, so that it's
       possible to make Emacs handle mime text for instance.  */
    {
      WebKitResponsePolicyDecision *response =
        WEBKIT_RESPONSE_POLICY_DECISION (decision);
      if (!webkit_response_policy_decision_is_mime_type_supported (response))
        {
          webkit_policy_decision_download (decision);
          return TRUE;
        }
      else
        return FALSE;
      break;
    }
  case WEBKIT_POLICY_DECISION_TYPE_NEW_WINDOW_ACTION:
    {
      WebKitNavigationPolicyDecision *navigation_decision =
        WEBKIT_NAVIGATION_POLICY_DECISION (decision);
      WebKitNavigationAction *navigation_action =
        webkit_navigation_policy_decision_get_navigation_action (navigation_decision);
      WebKitURIRequest *request =
        webkit_navigation_action_get_request (navigation_action);
      WebKitWebView *newview;
      struct xwidget *xw = g_object_get_data (G_OBJECT (webView), XG_XWIDGET);
      Lisp_Object val, new_xwidget;

      XSETXWIDGET (val, xw);

      new_xwidget = Fmake_xwidget (Qwebkit, Qnil, make_fixnum (0),
				   make_fixnum (0), Qnil,
				   build_string (" *detached xwidget buffer*"),
				   val);

      if (NILP (new_xwidget))
	return FALSE;

      newview = WEBKIT_WEB_VIEW (XXWIDGET (new_xwidget)->widget_osr);
      webkit_web_view_load_request (newview, request);

      store_xwidget_display_event (XXWIDGET (new_xwidget), xw);
      return TRUE;
    }
  case WEBKIT_POLICY_DECISION_TYPE_NAVIGATION_ACTION:
    {
      WebKitNavigationPolicyDecision *navigation_decision =
        WEBKIT_NAVIGATION_POLICY_DECISION (decision);
      WebKitNavigationAction *navigation_action =
        webkit_navigation_policy_decision_get_navigation_action (navigation_decision);
      WebKitURIRequest *request =
        webkit_navigation_action_get_request (navigation_action);

      struct xwidget *xw = g_object_get_data (G_OBJECT (webView), XG_XWIDGET);
      store_xwidget_event_string (xw, "decide-policy",
                                  webkit_uri_request_get_uri (request));
      return FALSE;
      break;
    }
  default:
    return FALSE;
  }
}

static gboolean
webkit_script_dialog_cb (WebKitWebView *webview,
			 WebKitScriptDialog *script_dialog,
			 gpointer user)
{
  struct frame *f = SELECTED_FRAME ();
  WebKitScriptDialogType type;
  GtkWidget *widget;
  GtkWidget *dialog;
  GtkWidget *entry;
  GtkWidget *content_area;
  GtkWidget *box;
  GtkWidget *label;
  const gchar *content;
  const gchar *message;
  gint result;

  /* Return TRUE to prevent WebKit from showing the default script
     dialog in the offscreen window, which runs a nested main loop
     Emacs can't respond to, and as such can't pass X events to.  */
  if (!FRAME_WINDOW_P (f))
    return TRUE;

  type = webkit_script_dialog_get_dialog_type (script_dialog);;
  widget = FRAME_GTK_OUTER_WIDGET (f);
  content = webkit_script_dialog_get_message (script_dialog);

  if (type == WEBKIT_SCRIPT_DIALOG_ALERT)
    dialog = gtk_dialog_new_with_buttons ("Alert", GTK_WINDOW (widget),
					  GTK_DIALOG_MODAL,
					  "Dismiss", 1, NULL);
  else
    dialog = gtk_dialog_new_with_buttons ("Question", GTK_WINDOW (widget),
					  GTK_DIALOG_MODAL,
					  "OK", 0, "Cancel", 1, NULL);

  box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 8);
  label = gtk_label_new (content);
  content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  gtk_container_add (GTK_CONTAINER (content_area), box);

  gtk_widget_show (box);
  gtk_widget_show (label);

  gtk_box_pack_start (GTK_BOX (box), label, TRUE, TRUE, 0);

  if (type == WEBKIT_SCRIPT_DIALOG_PROMPT)
    {
      entry = gtk_entry_new ();
      message = webkit_script_dialog_prompt_get_default_text (script_dialog);

      gtk_widget_show (entry);
      gtk_entry_set_text (GTK_ENTRY (entry), message);
      gtk_box_pack_end (GTK_BOX (box), entry, TRUE, TRUE, 0);
    }

  result = gtk_dialog_run (GTK_DIALOG (dialog));

  if (type == WEBKIT_SCRIPT_DIALOG_CONFIRM
      || type == WEBKIT_SCRIPT_DIALOG_BEFORE_UNLOAD_CONFIRM)
    webkit_script_dialog_confirm_set_confirmed (script_dialog, result == 0);

  if (type == WEBKIT_SCRIPT_DIALOG_PROMPT)
    webkit_script_dialog_prompt_set_text (script_dialog,
					  gtk_entry_get_text (GTK_ENTRY (entry)));

  gtk_widget_destroy (GTK_WIDGET (dialog));

  return TRUE;
}
#endif /* USE_GTK */


/* Initializes and does initial placement of an xwidget view on screen.  */
static struct xwidget_view *
xwidget_init_view (struct xwidget *xww,
                   struct glyph_string *s,
                   int x, int y)
{

#ifdef USE_GTK
  if (!xg_gtk_initialized)
    error ("xwidget_init_view: GTK has not been initialized");
#endif

  struct xwidget_view *xv = allocate_xwidget_view ();
  Lisp_Object val;

  XSETXWIDGET_VIEW (val, xv);
  internal_xwidget_view_list = Fcons (val, internal_xwidget_view_list);
  Vxwidget_view_list = Fcopy_sequence (internal_xwidget_view_list);

  XSETWINDOW (xv->w, s->w);
  XSETXWIDGET (xv->model, xww);

#ifdef HAVE_X_WINDOWS
  xv->dpy = FRAME_X_DISPLAY (s->f);

  xv->x = x;
  xv->y = y;

  xv->clip_left = 0;
  xv->clip_right = xww->width;
  xv->clip_top = 0;
  xv->clip_bottom = xww->height;

  xv->wdesc = None;
  xv->frame = s->f;
  xv->cursor = FRAME_OUTPUT_DATA (s->f)->nontext_cursor;
  xv->just_resized = false;
  xv->last_crossing_window = NULL;
  xv->passive_grab = NULL;
#elif defined HAVE_PGTK
  xv->dpyinfo = FRAME_DISPLAY_INFO (s->f);
  xv->widget = gtk_drawing_area_new ();
  gtk_widget_set_app_paintable (xv->widget, TRUE);
  gtk_widget_add_events (xv->widget, GDK_ALL_EVENTS_MASK);
  gtk_container_add (GTK_CONTAINER (FRAME_GTK_WIDGET (s->f)),
		     xv->widget);

  g_signal_connect (xv->widget, "draw",
		    G_CALLBACK (xwidget_view_draw_cb), xv);
  g_signal_connect (xv->widget, "event",
		    G_CALLBACK (xw_forward_event_from_view), xv);

  g_object_set_data (G_OBJECT (xv->widget), XG_XWIDGET_VIEW, xv);

  xv->x = x;
  xv->y = y;

  xv->clip_left = 0;
  xv->clip_right = xww->width;
  xv->clip_top = 0;
  xv->clip_bottom = xww->height;

  xv->frame = s->f;
  xv->cursor = cursor_for_hit (xww->hit_result, s->f);
  xv->just_resized = false;
#elif defined NS_IMPL_COCOA
  nsxwidget_init_view (xv, xww, s, x, y);
  nsxwidget_resize_view(xv, xww->width, xww->height);
#endif

  return xv;
}

void
x_draw_xwidget_glyph_string (struct glyph_string *s)
{
  /* This method is called by the redisplay engine and places the
     xwidget on screen.  Moving and clipping is done here.  Also view
     initialization.  */
  struct xwidget *xww = s->xwidget;
  struct xwidget_view *xv = xwidget_view_lookup (xww, s->w);
  int text_area_x, text_area_y, text_area_width, text_area_height;
  int clip_right;
  int clip_bottom;
  int clip_top;
  int clip_left;

  int x = s->x;
  int y = s->y + (s->height / 2) - (xww->height / 2);

  /* Do initialization here in the display loop because there is no
     other time to know things like window placement etc.  Do not
     create a new view if we have found one that is usable.  */
#ifdef USE_GTK
  if (!xv)
    xv = xwidget_init_view (xww, s, x, y);

  xv->just_resized = false;
#elif defined NS_IMPL_COCOA
  if (!xv)
    {
      /* Enforce 1 to 1, model and view for macOS Cocoa webkit2.  */
      if (xww->xv)
        {
          if (xwidget_hidden (xww->xv))
            {
              Lisp_Object xvl;
              XSETXWIDGET_VIEW (xvl, xww->xv);
              Fdelete_xwidget_view (xvl);
            }
          else
            {
              message ("You can't share an xwidget (webkit2) among windows.");
              return;
            }
        }
      xv = xwidget_init_view (xww, s, x, y);
    }
#endif

  xv->area = s->area;

  window_box (s->w, xv->area, &text_area_x, &text_area_y,
              &text_area_width, &text_area_height);

  clip_left = max (0, text_area_x - x);
  clip_right = max (clip_left,
		    min (xww->width, text_area_x + text_area_width - x));
  clip_top = max (0, text_area_y - y);
  clip_bottom = max (clip_top,
		     min (xww->height, text_area_y + text_area_height - y));

  /* We are concerned with movement of the onscreen area.  The area
     might sit still when the widget actually moves.  This happens
     when an Emacs window border moves across a widget window.  So, if
     any corner of the outer widget clipping window moves, that counts
     as movement here, even if it looks like no movement happens
     because the widget sits still inside the clipping area.  The
     widget can also move inside the clipping area, which happens
     later.  */
  bool moved = (xv->x + xv->clip_left != x + clip_left
		|| xv->y + xv->clip_top != y + clip_top);

#ifdef HAVE_X_WINDOWS
  bool wdesc_was_none = xv->wdesc == None;
#endif
  xv->x = x;
  xv->y = y;

#ifdef HAVE_X_WINDOWS
  block_input ();
  if (xv->wdesc == None)
    {
      Lisp_Object xvw;
      XSETXWIDGET_VIEW (xvw, xv);
      XSetWindowAttributes a;
      a.event_mask = (ExposureMask | ButtonPressMask | ButtonReleaseMask
		      | PointerMotionMask | EnterWindowMask | LeaveWindowMask);

      if (clip_right - clip_left <= 0
	  || clip_bottom - clip_top <= 0)
	{
	  unblock_input ();
	  return;
	}

      xv->wdesc = XCreateWindow (xv->dpy, FRAME_X_WINDOW (s->f),
				 x + clip_left, y + clip_top,
				 clip_right - clip_left,
				 clip_bottom - clip_top, 0,
				 CopyFromParent, CopyFromParent,
				 CopyFromParent, CWEventMask, &a);
#ifdef HAVE_XINPUT2
      XIEventMask mask;
      ptrdiff_t l = XIMaskLen (XI_LASTEVENT);
      unsigned char *m;

      if (FRAME_DISPLAY_INFO (s->f)->supports_xi2)
	{
	  mask.mask = m = alloca (l);
	  memset (m, 0, l);
	  mask.mask_len = l;
	  mask.deviceid = XIAllMasterDevices;

	  XISetMask (m, XI_Motion);
	  XISetMask (m, XI_ButtonPress);
	  XISetMask (m, XI_ButtonRelease);
	  XISetMask (m, XI_Enter);
	  XISetMask (m, XI_Leave);
#ifdef HAVE_XINPUT2_4
	  if (FRAME_DISPLAY_INFO (s->f)->xi2_version >= 4)
	    {
	      XISetMask (m, XI_GesturePinchBegin);
	      XISetMask (m, XI_GesturePinchUpdate);
	      XISetMask (m, XI_GesturePinchEnd);
	    }
#endif
	  XISelectEvents (xv->dpy, xv->wdesc, &mask, 1);
	}
#endif
      XLowerWindow (xv->dpy, xv->wdesc);
      XDefineCursor (xv->dpy, xv->wdesc, xv->cursor);
      xv->cr_surface = cairo_xlib_surface_create (xv->dpy,
						  xv->wdesc,
						  FRAME_DISPLAY_INFO (s->f)->visual,
						  clip_right - clip_left,
						  clip_bottom - clip_top);
      xv->cr_context = cairo_create (xv->cr_surface);
      Fputhash (make_fixnum (xv->wdesc), xvw, x_window_to_xwv_map);

      moved = false;
    }
#endif
#ifdef HAVE_PGTK
  block_input ();
#endif

  /* Has it moved?  */
  if (moved)
    {
#ifdef HAVE_X_WINDOWS
      XMoveResizeWindow (xv->dpy, xv->wdesc, x + clip_left, y + clip_top,
			 clip_right - clip_left, clip_bottom - clip_top);
      XFlush (xv->dpy);
      cairo_xlib_surface_set_size (xv->cr_surface, clip_right - clip_left,
				   clip_bottom - clip_top);
#elif defined HAVE_PGTK
      gtk_widget_set_size_request (xv->widget, clip_right - clip_left,
				   clip_bottom - clip_top);
      gtk_fixed_move (GTK_FIXED (FRAME_GTK_WIDGET (xv->frame)),
		      xv->widget, x + clip_left, y + clip_top);
      gtk_widget_queue_allocate (xv->widget);
#elif defined NS_IMPL_COCOA
      nsxwidget_move_view (xv, x + clip_left, y + clip_top);
#endif
    }

  /* Clip the widget window if some parts happen to be outside
     drawable area.  An Emacs window is not a gtk window.  A gtk window
     covers the entire frame.  Clipping might have changed even if we
     haven't actually moved; try to figure out when we need to reclip
     for real.  */
#ifndef HAVE_PGTK
  if (xv->clip_right != clip_right
      || xv->clip_bottom != clip_bottom
      || xv->clip_top != clip_top || xv->clip_left != clip_left)
#endif
    {
#ifdef USE_GTK
#ifdef HAVE_X_WINDOWS
      if (!wdesc_was_none && !moved)
	{
	  if (clip_right - clip_left <= 0
	      || clip_bottom - clip_top <= 0)
	    {
	      XUnmapWindow (xv->dpy, xv->wdesc);
	      xv->hidden = true;
	    }
	  else
	    {
	      XResizeWindow (xv->dpy, xv->wdesc, clip_right - clip_left,
			     clip_bottom - clip_top);
	    }
	  XFlush (xv->dpy);
	  cairo_xlib_surface_set_size (xv->cr_surface, clip_right - clip_left,
				       clip_bottom - clip_top);
	}
#else
      gtk_widget_set_size_request (xv->widget, clip_right - clip_left,
				   clip_bottom - clip_top);
      gtk_fixed_move (GTK_FIXED (FRAME_GTK_WIDGET (xv->frame)),
		      xv->widget, x + clip_left, y + clip_top);
      gtk_widget_queue_allocate (xv->widget);
#endif
#elif defined NS_IMPL_COCOA
      nsxwidget_resize_view (xv, clip_right - clip_left,
                             clip_bottom - clip_top);
      nsxwidget_move_widget_in_view (xv, -clip_left, -clip_top);
#endif

      xv->clip_right = clip_right;
      xv->clip_bottom = clip_bottom;
      xv->clip_top = clip_top;
      xv->clip_left = clip_left;
    }

  /* If emacs wants to repaint the area where the widget lives, queue
     a redraw.  It seems its possible to get out of sync with emacs
     redraws so emacs background sometimes shows up instead of the
     xwidgets background.  It's just a visual glitch though.  */
  /* When xww->buffer is nil, that means the xwidget has been killed.  */
  if (!NILP (xww->buffer))
    {
      if (!xwidget_hidden (xv))
	{
#ifdef USE_GTK
	  gtk_widget_queue_draw (xww->widget_osr);
#elif defined NS_IMPL_COCOA
	  nsxwidget_set_needsdisplay (xv);
#endif
	}
    }
#ifdef HAVE_X_WINDOWS
  else
    {
      XSetWindowBackground (xv->dpy, xv->wdesc,
			    FRAME_BACKGROUND_PIXEL (s->f));
    }
#endif

#if defined HAVE_XINPUT2 || defined HAVE_PGTK
  if (!NILP (xww->buffer))
    {
      record_osr_embedder (xv);
      synthesize_focus_in_event (xww->widget_osr);
    }
#endif

#ifdef USE_GTK
  unblock_input ();
#endif
}

#define CHECK_WEBKIT_WIDGET(xw)				\
  if (NILP (xw->buffer) || !EQ (xw->type, Qwebkit))	\
    error ("Not a WebKit widget")

/* Macro that checks xwidget hold webkit web view first.  */
#define WEBKIT_FN_INIT()						\
  CHECK_LIVE_XWIDGET (xwidget);						\
  struct xwidget *xw = XXWIDGET (xwidget);				\
  CHECK_WEBKIT_WIDGET (xw)

DEFUN ("xwidget-webkit-uri",
       Fxwidget_webkit_uri, Sxwidget_webkit_uri,
       1, 1, 0,
       doc: /* Get the current URL of XWIDGET webkit.  */)
  (Lisp_Object xwidget)
{
  WEBKIT_FN_INIT ();
#ifdef USE_GTK
  WebKitWebView *wkwv = WEBKIT_WEB_VIEW (xw->widget_osr);
  const gchar *uri = webkit_web_view_get_uri (wkwv);
  if (!uri)
    return build_string ("");
  return build_string (uri);
#elif defined NS_IMPL_COCOA
  return nsxwidget_webkit_uri (xw);
#endif
}

DEFUN ("xwidget-webkit-title",
       Fxwidget_webkit_title, Sxwidget_webkit_title,
       1, 1, 0,
       doc: /* Get the current title of XWIDGET webkit.  */)
  (Lisp_Object xwidget)
{
  WEBKIT_FN_INIT ();
#ifdef USE_GTK
  WebKitWebView *wkwv = WEBKIT_WEB_VIEW (xw->widget_osr);
  const gchar *title = webkit_web_view_get_title (wkwv);

  return build_string (title ? title : "");
#elif defined NS_IMPL_COCOA
  return nsxwidget_webkit_title (xw);
#endif
}

DEFUN ("xwidget-webkit-estimated-load-progress",
       Fxwidget_webkit_estimated_load_progress, Sxwidget_webkit_estimated_load_progress,
       1, 1, 0, doc: /* Get the estimated load progress of XWIDGET, a WebKit widget.
Return a value ranging from 0.0 to 1.0, based on how close XWIDGET
is to completely loading its page.  */)
  (Lisp_Object xwidget)
{
  struct xwidget *xw;
#ifdef USE_GTK
  WebKitWebView *webview;
#endif
  double value;

  CHECK_LIVE_XWIDGET (xwidget);
  xw = XXWIDGET (xwidget);
  CHECK_WEBKIT_WIDGET (xw);

  block_input ();
#ifdef USE_GTK
  webview = WEBKIT_WEB_VIEW (xw->widget_osr);
  value = webkit_web_view_get_estimated_load_progress (webview);
#elif defined NS_IMPL_COCOA
  value = nsxwidget_webkit_estimated_load_progress (xw);
#endif

  unblock_input ();

  return make_float (value);
}

DEFUN ("xwidget-webkit-goto-uri",
       Fxwidget_webkit_goto_uri, Sxwidget_webkit_goto_uri,
       2, 2, 0,
       doc: /* Make the xwidget webkit instance referenced by XWIDGET browse URI.  */)
  (Lisp_Object xwidget, Lisp_Object uri)
{
  WEBKIT_FN_INIT ();
  CHECK_STRING (uri);
  uri = ENCODE_FILE (uri);
#ifdef USE_GTK
  webkit_web_view_load_uri (WEBKIT_WEB_VIEW (xw->widget_osr), SSDATA (uri));
  catch_child_signal ();
#elif defined NS_IMPL_COCOA
  nsxwidget_webkit_goto_uri (xw, SSDATA (uri));
#endif
  return Qnil;
}

DEFUN ("xwidget-webkit-goto-history",
       Fxwidget_webkit_goto_history, Sxwidget_webkit_goto_history,
       2, 2, 0,
       doc: /* Make the XWIDGET webkit the REL-POSth element in load history.

If REL-POS is 0, the widget will be just reload the current element in
history.  If REL-POS is more or less than 0, the widget will load the
REL-POSth element around the current spot in the load history. */)
  (Lisp_Object xwidget, Lisp_Object rel_pos)
{
  WEBKIT_FN_INIT ();
  CHECK_FIXNUM (rel_pos);

#ifdef USE_GTK
  WebKitWebView *wkwv = WEBKIT_WEB_VIEW (xw->widget_osr);
  WebKitBackForwardList *list;
  WebKitBackForwardListItem *it;

  if (XFIXNUM (rel_pos) == 0)
    webkit_web_view_reload (wkwv);
  else
    {
      list = webkit_web_view_get_back_forward_list (wkwv);
      it = webkit_back_forward_list_get_nth_item (list, XFIXNUM (rel_pos));

      if (!it)
	error ("There is no item at this index");

      webkit_web_view_go_to_back_forward_list_item (wkwv, it);
    }
#elif defined NS_IMPL_COCOA
  nsxwidget_webkit_goto_history (xw, XFIXNAT (rel_pos));
#endif
  return Qnil;
}

DEFUN ("xwidget-webkit-zoom",
       Fxwidget_webkit_zoom, Sxwidget_webkit_zoom,
       2, 2, 0,
       doc: /* Change the zoom factor of the xwidget webkit instance referenced by XWIDGET.  */)
  (Lisp_Object xwidget, Lisp_Object factor)
{
  WEBKIT_FN_INIT ();
  if (FLOATP (factor))
    {
      double zoom_change = XFLOAT_DATA (factor);
#ifdef USE_GTK
      webkit_web_view_set_zoom_level
        (WEBKIT_WEB_VIEW (xw->widget_osr),
         webkit_web_view_get_zoom_level
         (WEBKIT_WEB_VIEW (xw->widget_osr)) + zoom_change);
#elif defined NS_IMPL_COCOA
      nsxwidget_webkit_zoom (xw, zoom_change);
#endif
    }
  return Qnil;
}

#ifdef USE_GTK
/* Save script and fun in the script/callback save vector and return
   its index.  */
static ptrdiff_t
save_script_callback (struct xwidget *xw, Lisp_Object script, Lisp_Object fun)
{
  Lisp_Object cbs = xw->script_callbacks;
  if (NILP (cbs))
    xw->script_callbacks = cbs = make_nil_vector (32);

  /* Find first free index.  */
  ptrdiff_t idx;
  for (idx = 0; !NILP (AREF (cbs, idx)); idx++)
    if (idx + 1 == ASIZE (cbs))
      {
	xw->script_callbacks = cbs = larger_vector (cbs, 1, -1);
	break;
      }

  ASET (cbs, idx, Fcons (make_mint_ptr (xlispstrdup (script)), fun));
  return idx;
}
#endif

DEFUN ("xwidget-webkit-execute-script",
       Fxwidget_webkit_execute_script, Sxwidget_webkit_execute_script,
       2, 3, 0,
       doc: /* Make the Webkit XWIDGET execute JavaScript SCRIPT.
If FUN is provided, feed the JavaScript return value to the single
argument procedure FUN.*/)
  (Lisp_Object xwidget, Lisp_Object script, Lisp_Object fun)
{
  WEBKIT_FN_INIT ();
  CHECK_STRING (script);
  if (!NILP (fun) && !FUNCTIONP (fun))
    wrong_type_argument (Qinvalid_function, fun);

  script = ENCODE_SYSTEM (script);

#ifdef USE_GTK
  /* Protect script and fun during GC.  */
  intptr_t idx = save_script_callback (xw, script, fun);

  /* JavaScript execution happens asynchronously.  If an elisp
     callback function is provided we pass it to the C callback
     procedure that retrieves the return value.  */
  gchar *script_string
    = xmint_pointer (XCAR (AREF (xw->script_callbacks, idx)));
  webkit_web_view_run_javascript (WEBKIT_WEB_VIEW (xw->widget_osr),
				  script_string,
                                  NULL, /* cancelable */
                                  webkit_javascript_finished_cb,
				  (gpointer) idx);
#elif defined NS_IMPL_COCOA
  nsxwidget_webkit_execute_script (xw, SSDATA (script), fun);
#endif
  return Qnil;
}

DEFUN ("xwidget-resize", Fxwidget_resize, Sxwidget_resize, 3, 3, 0,
       doc: /* Resize XWIDGET to NEW_WIDTH, NEW_HEIGHT.  */ )
  (Lisp_Object xwidget, Lisp_Object new_width, Lisp_Object new_height)
{
  CHECK_LIVE_XWIDGET (xwidget);
  int w = check_integer_range (new_width, 0, INT_MAX);
  int h = check_integer_range (new_height, 0, INT_MAX);
  struct xwidget *xw = XXWIDGET (xwidget);

  xw->width = w;
  xw->height = h;

  block_input ();

  for (Lisp_Object tail = internal_xwidget_view_list; CONSP (tail);
       tail = XCDR (tail))
    {
      if (XWIDGET_VIEW_P (XCAR (tail)))
        {
          struct xwidget_view *xv = XXWIDGET_VIEW (XCAR (tail));
          if (XXWIDGET (xv->model) == xw)
            {
#ifdef USE_GTK
	      xv->just_resized = true;
	      SET_FRAME_GARBAGED (xv->frame);
#else
	      wset_redisplay (XWINDOW (xv->w));
#endif
            }
        }
    }

  redisplay ();

  /* If there is an offscreen widget resize it first.  */
#ifdef USE_GTK
  if (xw->widget_osr)
    {
      gtk_window_resize (GTK_WINDOW (xw->widgetwindow_osr), xw->width,
                         xw->height);
      gtk_widget_set_size_request (GTK_WIDGET (xw->widget_osr), xw->width,
                                   xw->height);

      gtk_widget_queue_allocate (GTK_WIDGET (xw->widget_osr));
    }
#elif defined NS_IMPL_COCOA
  nsxwidget_resize (xw);
#endif
  unblock_input ();

  return Qnil;
}




DEFUN ("xwidget-size-request",
       Fxwidget_size_request, Sxwidget_size_request,
       1, 1, 0,
       doc: /* Return the desired size of the XWIDGET.
This can be used to read the xwidget desired size, and resizes the
Emacs allocated area accordingly.  */)
  (Lisp_Object xwidget)
{
  CHECK_LIVE_XWIDGET (xwidget);
#ifdef USE_GTK
  GtkRequisition requisition;
  gtk_widget_size_request (XXWIDGET (xwidget)->widget_osr, &requisition);
  return list2i (requisition.width, requisition.height);
#elif defined NS_IMPL_COCOA
  return nsxwidget_get_size (XXWIDGET (xwidget));
#endif
}

DEFUN ("xwidgetp",
       Fxwidgetp, Sxwidgetp,
       1, 1, 0,
       doc: /* Return t if OBJECT is an xwidget.  */)
  (Lisp_Object object)
{
  return XWIDGETP (object) ? Qt : Qnil;
}

DEFUN ("xwidget-view-p",
       Fxwidget_view_p, Sxwidget_view_p,
       1, 1, 0,
       doc: /* Return t if OBJECT is an xwidget-view.  */)
  (Lisp_Object object)
{
  return XWIDGET_VIEW_P (object) ? Qt : Qnil;
}

DEFUN ("xwidget-info",
       Fxwidget_info, Sxwidget_info,
       1, 1, 0,
       doc: /* Return XWIDGET properties in a vector.
Currently [TYPE TITLE WIDTH HEIGHT].  */)
  (Lisp_Object xwidget)
{
  CHECK_LIVE_XWIDGET (xwidget);
  struct xwidget *xw = XXWIDGET (xwidget);
  return CALLN (Fvector, xw->type, xw->title,
		make_fixed_natnum (xw->width), make_fixed_natnum (xw->height));
}

DEFUN ("xwidget-view-info",
       Fxwidget_view_info, Sxwidget_view_info,
       1, 1, 0,
       doc: /* Return properties of XWIDGET-VIEW in a vector.
Currently [X Y CLIP_RIGHT CLIP_BOTTOM CLIP_TOP CLIP_LEFT].  */)
  (Lisp_Object xwidget_view)
{
  CHECK_XWIDGET_VIEW (xwidget_view);
  struct xwidget_view *xv = XXWIDGET_VIEW (xwidget_view);
  return CALLN (Fvector, make_fixnum (xv->x), make_fixnum (xv->y),
		make_fixnum (xv->clip_right), make_fixnum (xv->clip_bottom),
		make_fixnum (xv->clip_top), make_fixnum (xv->clip_left));
}

DEFUN ("xwidget-view-model",
       Fxwidget_view_model, Sxwidget_view_model,
       1, 1, 0,
       doc:  /* Return the model associated with XWIDGET-VIEW.  */)
  (Lisp_Object xwidget_view)
{
  CHECK_XWIDGET_VIEW (xwidget_view);
  return XXWIDGET_VIEW (xwidget_view)->model;
}

DEFUN ("xwidget-view-window",
       Fxwidget_view_window, Sxwidget_view_window,
       1, 1, 0,
       doc:  /* Return the window of XWIDGET-VIEW.  */)
  (Lisp_Object xwidget_view)
{
  CHECK_XWIDGET_VIEW (xwidget_view);
  return XXWIDGET_VIEW (xwidget_view)->w;
}


DEFUN ("delete-xwidget-view",
       Fdelete_xwidget_view, Sdelete_xwidget_view,
       1, 1, 0,
       doc:  /* Delete the XWIDGET-VIEW.  */)
  (Lisp_Object xwidget_view)
{
  CHECK_XWIDGET_VIEW (xwidget_view);
  struct xwidget_view *xv = XXWIDGET_VIEW (xwidget_view);

  block_input ();
#ifdef USE_GTK
  struct xwidget *xw = XXWIDGET (xv->model);
  GdkWindow *w;
#ifdef HAVE_X_WINDOWS
  if (xv->wdesc != None)
    {
      cairo_destroy (xv->cr_context);
      cairo_surface_destroy (xv->cr_surface);
      XDestroyWindow (xv->dpy, xv->wdesc);
      Fremhash (make_fixnum (xv->wdesc), x_window_to_xwv_map);
    }

  if (xv->last_crossing_window)
    g_signal_handler_disconnect (xv->last_crossing_window,
				 xv->last_crossing_cursor_signal);
  g_clear_pointer (&xv->last_crossing_window,
		   g_object_unref);

  if (xv->passive_grab)
    {
      g_signal_handler_disconnect (xv->passive_grab,
				   xv->passive_grab_destruction_signal);
      g_signal_handler_disconnect (xv->passive_grab,
				   xv->passive_grab_drag_signal);
      xv->passive_grab = NULL;
    }

#else
  gtk_widget_destroy (xv->widget);
#endif

  if (xw->embedder_view == xv && !NILP (xw->buffer))
    {
      w = gtk_widget_get_window (xw->widgetwindow_osr);

      XXWIDGET (xv->model)->embedder_view = NULL;
      XXWIDGET (xv->model)->embedder = NULL;

      gdk_offscreen_window_set_embedder (w, NULL);
    }
#elif defined NS_IMPL_COCOA
  nsxwidget_delete_view (xv);
#endif

  internal_xwidget_view_list = Fdelq (xwidget_view, internal_xwidget_view_list);
  Vxwidget_view_list = Fcopy_sequence (internal_xwidget_view_list);
  unblock_input ();
  return Qnil;
}

DEFUN ("xwidget-view-lookup",
       Fxwidget_view_lookup, Sxwidget_view_lookup,
       1, 2, 0,
       doc: /* Return the xwidget-view associated with XWIDGET in WINDOW.
If WINDOW is unspecified or nil, use the selected window.
Return nil if no association is found.  */)
  (Lisp_Object xwidget, Lisp_Object window)
{
  CHECK_XWIDGET (xwidget);

  if (NILP (window))
    window = Fselected_window ();
  CHECK_WINDOW (window);

  for (Lisp_Object tail = internal_xwidget_view_list; CONSP (tail);
       tail = XCDR (tail))
    {
      Lisp_Object xwidget_view = XCAR (tail);
      if (EQ (Fxwidget_view_model (xwidget_view), xwidget)
          && EQ (Fxwidget_view_window (xwidget_view), window))
        return xwidget_view;
    }

  return Qnil;
}

DEFUN ("xwidget-plist",
       Fxwidget_plist, Sxwidget_plist,
       1, 1, 0,
       doc: /* Return the plist of XWIDGET.  */)
  (Lisp_Object xwidget)
{
  CHECK_LIVE_XWIDGET (xwidget);
  return XXWIDGET (xwidget)->plist;
}

DEFUN ("xwidget-buffer",
       Fxwidget_buffer, Sxwidget_buffer,
       1, 1, 0,
       doc: /* Return the buffer of XWIDGET.  */)
  (Lisp_Object xwidget)
{
  CHECK_XWIDGET (xwidget);
  return XXWIDGET (xwidget)->buffer;
}

DEFUN ("set-xwidget-buffer",
       Fset_xwidget_buffer, Sset_xwidget_buffer,
       2, 2, 0,
       doc: /* Set XWIDGET's buffer to BUFFER.  */)
  (Lisp_Object xwidget, Lisp_Object buffer)
{
  CHECK_LIVE_XWIDGET (xwidget);
  CHECK_BUFFER (buffer);

  XXWIDGET (xwidget)->buffer = buffer;
  return Qnil;
}

DEFUN ("set-xwidget-plist",
       Fset_xwidget_plist, Sset_xwidget_plist,
       2, 2, 0,
       doc: /* Replace the plist of XWIDGET with PLIST.
Returns PLIST.  */)
  (Lisp_Object xwidget, Lisp_Object plist)
{
  CHECK_LIVE_XWIDGET (xwidget);
  CHECK_LIST (plist);

  XXWIDGET (xwidget)->plist = plist;
  return plist;
}

DEFUN ("set-xwidget-query-on-exit-flag",
       Fset_xwidget_query_on_exit_flag, Sset_xwidget_query_on_exit_flag,
       2, 2, 0,
       doc: /* Specify if query is needed for XWIDGET when Emacs is exited.
If the second argument FLAG is non-nil, Emacs will query the user before
exiting or killing a buffer if XWIDGET is running.
This function returns FLAG.  */)
  (Lisp_Object xwidget, Lisp_Object flag)
{
  CHECK_LIVE_XWIDGET (xwidget);
  XXWIDGET (xwidget)->kill_without_query = NILP (flag);
  return flag;
}

DEFUN ("xwidget-query-on-exit-flag",
       Fxwidget_query_on_exit_flag, Sxwidget_query_on_exit_flag,
       1, 1, 0,
       doc: /* Return the current value of the query-on-exit flag for XWIDGET.  */)
  (Lisp_Object xwidget)
{
  CHECK_LIVE_XWIDGET (xwidget);
  return (XXWIDGET (xwidget)->kill_without_query ? Qnil : Qt);
}

DEFUN ("xwidget-webkit-search", Fxwidget_webkit_search, Sxwidget_webkit_search,
       2, 5, 0,
       doc: /* Begin an incremental search operation in an xwidget.
QUERY should be a string containing the text to search for.  XWIDGET
should be a WebKit xwidget where the search will take place.  When the
search operation is complete, callers should also call
`xwidget-webkit-finish-search' to complete the search operation.

CASE-INSENSITIVE, when non-nil, will cause the search to ignore the
case of characters inside QUERY.  BACKWARDS, when non-nil, will cause
the search to proceed towards the beginning of the widget's contents.
WRAP-AROUND, when nil, will cause the search to stop upon hitting the
end of the widget's contents.

It is OK to call this function even when a search is already in
progress.  In that case, the previous search query will be replaced
with QUERY.  */)
  (Lisp_Object query, Lisp_Object xwidget, Lisp_Object case_insensitive,
   Lisp_Object backwards, Lisp_Object wrap_around)
{
#ifdef USE_GTK
  WebKitWebView *webview;
  WebKitFindController *controller;
  WebKitFindOptions opt;
  struct xwidget *xw;
  gchar *g_query;
#endif

  CHECK_STRING (query);
  CHECK_LIVE_XWIDGET (xwidget);

#ifdef USE_GTK
  xw = XXWIDGET (xwidget);
  CHECK_WEBKIT_WIDGET (xw);

  webview = WEBKIT_WEB_VIEW (xw->widget_osr);
  query = ENCODE_UTF_8 (query);
  opt = WEBKIT_FIND_OPTIONS_NONE;
  g_query = xstrdup (SSDATA (query));

  if (!NILP (case_insensitive))
    opt |= WEBKIT_FIND_OPTIONS_CASE_INSENSITIVE;
  if (!NILP (backwards))
    opt |= WEBKIT_FIND_OPTIONS_BACKWARDS;
  if (!NILP (wrap_around))
    opt |= WEBKIT_FIND_OPTIONS_WRAP_AROUND;

  if (xw->find_text)
    xfree (xw->find_text);
  xw->find_text = g_query;

  block_input ();
  controller = webkit_web_view_get_find_controller (webview);
  webkit_find_controller_search (controller, g_query, opt, G_MAXUINT);
  unblock_input ();
#endif

  return Qnil;
}

DEFUN ("xwidget-webkit-next-result", Fxwidget_webkit_next_result,
       Sxwidget_webkit_next_result, 1, 1, 0,
       doc: /* Show the next result matching the current search query.

XWIDGET should be an xwidget that currently has a search query.
Before calling this function, you should start a search operation
using `xwidget-webkit-search'.  */)
  (Lisp_Object xwidget)
{
  struct xwidget *xw;
#ifdef USE_GTK
  WebKitWebView *webview;
  WebKitFindController *controller;
#endif

  CHECK_LIVE_XWIDGET (xwidget);
  xw = XXWIDGET (xwidget);
  CHECK_WEBKIT_WIDGET (xw);

  if (!xw->find_text)
    error ("Widget has no ongoing search operation");

#ifdef USE_GTK
  block_input ();
  webview = WEBKIT_WEB_VIEW (xw->widget_osr);
  controller = webkit_web_view_get_find_controller (webview);
  webkit_find_controller_search_next (controller);
  unblock_input ();
#endif

  return Qnil;
}

DEFUN ("xwidget-webkit-previous-result", Fxwidget_webkit_previous_result,
       Sxwidget_webkit_previous_result, 1, 1, 0,
       doc: /* Show the previous result matching the current search query.

XWIDGET should be an xwidget that currently has a search query.
Before calling this function, you should start a search operation
using `xwidget-webkit-search'.  */)
  (Lisp_Object xwidget)
{
  struct xwidget *xw;
#ifdef USE_GTK
  WebKitWebView *webview;
  WebKitFindController *controller;
#endif

  CHECK_LIVE_XWIDGET (xwidget);
  xw = XXWIDGET (xwidget);
  CHECK_WEBKIT_WIDGET (xw);

  if (!xw->find_text)
    error ("Widget has no ongoing search operation");

#ifdef USE_GTK
  block_input ();
  webview = WEBKIT_WEB_VIEW (xw->widget_osr);
  controller = webkit_web_view_get_find_controller (webview);
  webkit_find_controller_search_previous (controller);
  unblock_input ();
#endif

  return Qnil;
}

DEFUN ("xwidget-webkit-finish-search", Fxwidget_webkit_finish_search,
       Sxwidget_webkit_finish_search, 1, 1, 0,
       doc: /* Finish XWIDGET's search operation.

XWIDGET should be an xwidget that currently has a search query.
Before calling this function, you should start a search operation
using `xwidget-webkit-search'.  */)
  (Lisp_Object xwidget)
{
  struct xwidget *xw;
#ifdef USE_GTK
  WebKitWebView *webview;
  WebKitFindController *controller;
#endif

  CHECK_LIVE_XWIDGET (xwidget);
  xw = XXWIDGET (xwidget);
  CHECK_WEBKIT_WIDGET (xw);

  if (!xw->find_text)
    error ("Widget has no ongoing search operation");

#ifdef USE_GTK
  block_input ();
  webview = WEBKIT_WEB_VIEW (xw->widget_osr);
  controller = webkit_web_view_get_find_controller (webview);
  webkit_find_controller_search_finish (controller);

  if (xw->find_text)
    {
      xfree (xw->find_text);
      xw->find_text = NULL;
    }
  unblock_input ();
#endif

  return Qnil;
}

DEFUN ("kill-xwidget", Fkill_xwidget, Skill_xwidget,
       1, 1, 0,
       doc: /* Kill the specified XWIDGET.
This releases all window system resources associated with XWIDGET,
removes it from `xwidget-list', and detaches it from its buffer.  */)
  (Lisp_Object xwidget)
{
  struct xwidget *xw;

  CHECK_LIVE_XWIDGET (xwidget);
  xw = XXWIDGET (xwidget);

  block_input ();
  kill_xwidget (xw);
  unblock_input ();

  return Qnil;
}

#ifdef USE_GTK
DEFUN ("xwidget-webkit-load-html", Fxwidget_webkit_load_html,
       Sxwidget_webkit_load_html, 2, 3, 0,
       doc: /* Make XWIDGET's WebKit widget render TEXT.
XWIDGET should be a WebKit xwidget, that will receive TEXT.  TEXT
should be a string that will be displayed by XWIDGET as HTML markup.
BASE-URI should be a string containing a URI that is used to locate
resources with relative URLs, and if not specified, defaults
to "about:blank".  */)
  (Lisp_Object xwidget, Lisp_Object text, Lisp_Object base_uri)
{
  struct xwidget *xw;
  WebKitWebView *webview;
  char *data, *uri;

  CHECK_LIVE_XWIDGET (xwidget);
  CHECK_STRING (text);
  if (NILP (base_uri))
    base_uri = build_string ("about:blank");
  else
    CHECK_STRING (base_uri);

  base_uri = ENCODE_UTF_8 (base_uri);
  text = ENCODE_UTF_8 (text);
  xw = XXWIDGET (xwidget);
  CHECK_WEBKIT_WIDGET (xw);

  data = SSDATA (text);
  uri = SSDATA (base_uri);
  webview = WEBKIT_WEB_VIEW (xw->widget_osr);

  block_input ();
  webkit_web_view_load_html (webview, data, uri);
  unblock_input ();

  return Qnil;
}

DEFUN ("xwidget-webkit-back-forward-list", Fxwidget_webkit_back_forward_list,
       Sxwidget_webkit_back_forward_list, 1, 2, 0,
       doc: /* Return the navigation history of XWIDGET, a WebKit xwidget.

Return the history as a list of the form (BACK HERE FORWARD), where
HERE is the current navigation item, while BACK and FORWARD are lists
of history items of the form (IDX TITLE URI).  Here, IDX is an index
that can be passed to `xwidget-webkit-goto-history', TITLE is a string
containing the human-readable title of the history item, and URI is
the URI of the history item.

BACK, HERE, and FORWARD can all be nil depending on the state of the
navigation history.

BACK and FORWARD will each not contain more elements than LIMIT.  If
LIMIT is not specified or nil, it is treated as `50'.  */)
  (Lisp_Object xwidget, Lisp_Object limit)
{
  struct xwidget *xw;
  Lisp_Object back, here, forward;
  WebKitWebView *webview;
  WebKitBackForwardList *list;
  WebKitBackForwardListItem *item;
  GList *parent, *tem;
  int i;
  unsigned int lim;
  Lisp_Object title, uri;
  const gchar *item_title, *item_uri;

  back = Qnil;
  here = Qnil;
  forward = Qnil;

  if (NILP (limit))
    limit = make_fixnum (50);
  else
    CHECK_FIXNAT (limit);

  CHECK_LIVE_XWIDGET (xwidget);
  xw = XXWIDGET (xwidget);

  webview = WEBKIT_WEB_VIEW (xw->widget_osr);
  list = webkit_web_view_get_back_forward_list (webview);
  item = webkit_back_forward_list_get_current_item (list);
  lim = XFIXNAT (limit);

  if (item)
    {
      item_title = webkit_back_forward_list_item_get_title (item);
      item_uri = webkit_back_forward_list_item_get_uri (item);
      here = list3 (make_fixnum (0),
		    build_string_from_utf8 (item_title ? item_title : ""),
		    build_string_from_utf8 (item_uri ? item_uri : ""));
    }
  parent = webkit_back_forward_list_get_back_list_with_limit (list, lim);

  if (parent)
    {
      for (i = 1, tem = parent; tem; tem = tem->next, ++i)
	{
	  item = tem->data;
	  item_title = webkit_back_forward_list_item_get_title (item);
	  item_uri = webkit_back_forward_list_item_get_uri (item);
	  title = build_string_from_utf8 (item_title ? item_title : "");
	  uri = build_string_from_utf8 (item_uri ? item_uri : "");
	  back = Fcons (list3 (make_fixnum (-i), title, uri), back);
	}
    }

  back = Fnreverse (back);
  g_list_free (parent);

  parent = webkit_back_forward_list_get_forward_list_with_limit (list, lim);

  if (parent)
    {
      for (i = 1, tem = parent; tem; tem = tem->next, ++i)
	{
	  item = tem->data;
	  item_title = webkit_back_forward_list_item_get_title (item);
	  item_uri = webkit_back_forward_list_item_get_uri (item);
	  title = build_string_from_utf8 (item_title ? item_title : "");
	  uri = build_string_from_utf8 (item_uri ? item_uri : "");
	  forward = Fcons (list3 (make_fixnum (i), title, uri), forward);
	}
    }

  forward = Fnreverse (forward);
  g_list_free (parent);

  return list3 (back, here, forward);
}

#endif

DEFUN ("xwidget-webkit-set-cookie-storage-file",
       Fxwidget_webkit_set_cookie_storage_file, Sxwidget_webkit_set_cookie_storage_file,
       2, 2, 0, doc: /* Make the WebKit widget XWIDGET load and store cookies in FILE.

Cookies will be stored as plain text in FILE, which must be an
absolute file name.  All xwidgets related to XWIDGET will also
store cookies in FILE and load them from there.  */)
  (Lisp_Object xwidget, Lisp_Object file)
{
#ifdef USE_GTK
  struct xwidget *xw;
  WebKitWebView *webview;
  WebKitWebContext *context;
  WebKitCookieManager *manager;

  CHECK_LIVE_XWIDGET (xwidget);
  xw = XXWIDGET (xwidget);
  CHECK_WEBKIT_WIDGET (xw);
  CHECK_STRING (file);

  block_input ();
  webview = WEBKIT_WEB_VIEW (xw->widget_osr);
  context = webkit_web_view_get_context (webview);
  manager = webkit_web_context_get_cookie_manager (context);
  webkit_cookie_manager_set_persistent_storage (manager,
						SSDATA (ENCODE_UTF_8 (file)),
						WEBKIT_COOKIE_PERSISTENT_STORAGE_TEXT);
  unblock_input ();
#endif

  return Qnil;
}

DEFUN ("xwidget-webkit-stop-loading", Fxwidget_webkit_stop_loading,
       Sxwidget_webkit_stop_loading,
       1, 1, 0, doc: /* Stop loading data in the WebKit widget XWIDGET.
This will stop any data transfer that may still be in progress inside
XWIDGET as part of loading a page.  */)
  (Lisp_Object xwidget)
{
  struct xwidget *xw;
#ifdef USE_GTK
  WebKitWebView *webview;
#endif

  CHECK_LIVE_XWIDGET (xwidget);
  xw = XXWIDGET (xwidget);
  CHECK_WEBKIT_WIDGET (xw);

  block_input ();
#ifdef USE_GTK
  webview = WEBKIT_WEB_VIEW (xw->widget_osr);
  webkit_web_view_stop_loading (webview);
#elif defined NS_IMPL_COCOA
  nsxwidget_webkit_stop_loading (xw);
#endif
  unblock_input ();

  return Qnil;
}

void
syms_of_xwidget (void)
{
  defsubr (&Smake_xwidget);
  defsubr (&Sxwidgetp);
  defsubr (&Sxwidget_live_p);
  DEFSYM (Qxwidgetp, "xwidgetp");
  DEFSYM (Qxwidget_live_p, "xwidget-live-p");
  defsubr (&Sxwidget_view_p);
  DEFSYM (Qxwidget_view_p, "xwidget-view-p");
  defsubr (&Sxwidget_info);
  defsubr (&Sxwidget_view_info);
  defsubr (&Sxwidget_resize);
  defsubr (&Sget_buffer_xwidgets);
  defsubr (&Sxwidget_view_model);
  defsubr (&Sxwidget_view_window);
  defsubr (&Sxwidget_view_lookup);
  defsubr (&Sxwidget_query_on_exit_flag);
  defsubr (&Sset_xwidget_query_on_exit_flag);

  defsubr (&Sxwidget_webkit_uri);
  defsubr (&Sxwidget_webkit_title);
  defsubr (&Sxwidget_webkit_goto_uri);
  defsubr (&Sxwidget_webkit_goto_history);
  defsubr (&Sxwidget_webkit_zoom);
  defsubr (&Sxwidget_webkit_execute_script);
  DEFSYM (Qwebkit, "webkit");

  defsubr (&Sxwidget_size_request);
  defsubr (&Sdelete_xwidget_view);

  defsubr (&Sxwidget_plist);
  defsubr (&Sxwidget_buffer);
  defsubr (&Sset_xwidget_plist);
  defsubr (&Sxwidget_perform_lispy_event);
  defsubr (&Sxwidget_webkit_search);
  defsubr (&Sxwidget_webkit_finish_search);
  defsubr (&Sxwidget_webkit_next_result);
  defsubr (&Sxwidget_webkit_previous_result);
  defsubr (&Sset_xwidget_buffer);
  defsubr (&Sxwidget_webkit_set_cookie_storage_file);
  defsubr (&Sxwidget_webkit_stop_loading);
#ifdef USE_GTK
  defsubr (&Sxwidget_webkit_load_html);
  defsubr (&Sxwidget_webkit_back_forward_list);
#endif

  defsubr (&Sxwidget_webkit_estimated_load_progress);
  defsubr (&Skill_xwidget);

  DEFSYM (QCxwidget, ":xwidget");
  DEFSYM (QCtitle, ":title");

  /* Do not forget to update the docstring of make-xwidget if you add
     new types.  */

  DEFSYM (Qvertical, "vertical");
  DEFSYM (Qhorizontal, "horizontal");

  DEFSYM (QCplist, ":plist");

  DEFVAR_LISP ("xwidget-list", Vxwidget_list,
               doc: /* List of all xwidgets that have not been killed.  */);
  Vxwidget_list = Qnil;

  DEFVAR_LISP ("xwidget-view-list", Vxwidget_view_list,
	       doc: /* List of all xwidget views.  */);
  Vxwidget_view_list = Qnil;

  DEFVAR_BOOL ("xwidget-webkit-disable-javascript", xwidget_webkit_disable_javascript,
    doc: /* If non-nil, disable execution of JavaScript in xwidget WebKit widgets.
Modifications to this setting do not take effect in existing WebKit
widgets; kill all xwidget-webkit buffers for changes in this setting
to take effect.  */);
  xwidget_webkit_disable_javascript = false;

  Fprovide (intern ("xwidget-internal"), Qnil);

  id_to_xwidget_map = CALLN (Fmake_hash_table, QCtest, Qeq,
			     QCweakness, Qvalue);
  staticpro (&id_to_xwidget_map);

  internal_xwidget_list = Qnil;
  staticpro (&internal_xwidget_list);
  internal_xwidget_view_list = Qnil;
  staticpro (&internal_xwidget_view_list);

#ifdef HAVE_X_WINDOWS
  x_window_to_xwv_map = CALLN (Fmake_hash_table, QCtest, Qeq);

  staticpro (&x_window_to_xwv_map);

#if WEBKIT_CHECK_VERSION (2, 34, 0)
  dummy_tooltip_string = build_string ("");
  staticpro (&dummy_tooltip_string);
#endif
#endif
  DEFSYM (Qdownload_callback, "download-callback");
  DEFSYM (Qjavascript_callback, "javascript-callback");
}


/* Value is non-zero if OBJECT is a valid Lisp xwidget specification.  A
   valid xwidget specification is a list whose car is the symbol
   `xwidget', and whose rest is a property list.  The property list must
   contain a value for key `:type'.  That value must be the name of a
   supported xwidget type.  The rest of the property list depends on the
   xwidget type.  */

bool
valid_xwidget_spec_p (Lisp_Object object)
{
  return CONSP (object) && EQ (XCAR (object), Qxwidget);
}


/* Find a value associated with key in spec.  */
static Lisp_Object
xwidget_spec_value (Lisp_Object spec, Lisp_Object key)
{
  Lisp_Object tail;

  eassert (valid_xwidget_spec_p (spec));

  for (tail = XCDR (spec);
       CONSP (tail) && CONSP (XCDR (tail)); tail = XCDR (XCDR (tail)))
    {
      if (EQ (XCAR (tail), key))
	return XCAR (XCDR (tail));
    }

  return Qnil;
}


void
xwidget_view_delete_all_in_window (struct window *w)
{
  struct xwidget_view *xv = NULL;
  for (Lisp_Object tail = internal_xwidget_view_list; CONSP (tail);
       tail = XCDR (tail))
    {
      if (XWIDGET_VIEW_P (XCAR (tail)))
        {
          xv = XXWIDGET_VIEW (XCAR (tail));
          if (XWINDOW (xv->w) == w)
            {
              Fdelete_xwidget_view (XCAR (tail));
            }
        }
    }
}

static struct xwidget_view *
xwidget_view_lookup (struct xwidget *xw, struct window *w)
{
  Lisp_Object xwidget, window, ret;
  XSETXWIDGET (xwidget, xw);
  XSETWINDOW (window, w);

  ret = Fxwidget_view_lookup (xwidget, window);

  return NILP (ret) ? NULL : XXWIDGET_VIEW (ret);
}

struct xwidget *
lookup_xwidget (Lisp_Object spec)
{
  /* When a xwidget lisp spec is found initialize the C struct that is
     used in the C code.  This is done by redisplay so values change
     if the spec changes.  So, take special care of one-shot events.  */
  Lisp_Object value;
  struct xwidget *xw;

  value = xwidget_spec_value (spec, QCxwidget);
  xw = XXWIDGET (value);

  return xw;
}

/* Set up detection of touched xwidget.  */
static void
xwidget_start_redisplay (void)
{
  for (Lisp_Object tail = internal_xwidget_view_list; CONSP (tail);
       tail = XCDR (tail))
    {
      if (XWIDGET_VIEW_P (XCAR (tail)))
        XXWIDGET_VIEW (XCAR (tail))->redisplayed = false;
    }
}

/* The xwidget was touched during redisplay, so it isn't a candidate
   for hiding.  */
static void
xwidget_touch (struct xwidget_view *xv)
{
  xv->redisplayed = true;
}

static bool
xwidget_touched (struct xwidget_view *xv)
{
  return xv->redisplayed;
}

/* Redisplay has ended, now we should hide untouched xwidgets.  */
void
xwidget_end_redisplay (struct window *w, struct glyph_matrix *matrix)
{
  int i;
  int area;

  xwidget_start_redisplay ();
  /* Iterate desired glyph matrix of window here, hide gtk widgets
     not in the desired matrix.

     This only takes care of xwidgets in active windows.  If a window
     goes away from the screen, xwidget views must be deleted.

     dump_glyph_matrix (matrix, 2);  */
  for (i = 0; i < matrix->nrows; ++i)
    {
      /* dump_glyph_row (MATRIX_ROW (matrix, i), i, glyphs); */
      struct glyph_row *row;
      row = MATRIX_ROW (matrix, i);
      if (row->enabled_p)
	for (area = LEFT_MARGIN_AREA; area < LAST_AREA; ++area)
	  {
	    struct glyph *glyph = row->glyphs[area];
	    struct glyph *glyph_end = glyph + row->used[area];
	    for (; glyph < glyph_end; ++glyph)
	      if (glyph->type == XWIDGET_GLYPH)
		{
		  /* The only call to xwidget_end_redisplay is in dispnew.
		     xwidget_end_redisplay (w->current_matrix);  */
		  struct xwidget_view *xv
		    = xwidget_view_lookup (xwidget_from_id (glyph->u.xwidget), w);

		  /* In NS xwidget, xv can be NULL for the second or
                     later views for a model, the result of 1 to 1
                     model view relation enforcement.  `xwidget_view_lookup'
		     has also been observed to return NULL here on X-Windows
		     at least once, so stay safe and only touch it if it's
		     not NULL.  */

                  if (xv)
                    xwidget_touch (xv);
		}
	  }
    }

  for (Lisp_Object tail = internal_xwidget_view_list; CONSP (tail);
       tail = XCDR (tail))
    {
      if (XWIDGET_VIEW_P (XCAR (tail)))
        {
          struct xwidget_view *xv = XXWIDGET_VIEW (XCAR (tail));

          /* "touched" is only meaningful for the current window, so
             disregard other views.  */
          if (XWINDOW (xv->w) == w)
            {
              if (xwidget_touched (xv))
                {
#ifdef USE_GTK
                  xwidget_show_view (xv);
#elif defined NS_IMPL_COCOA
                  nsxwidget_show_view (xv);
#endif
                }
              else
                {
#ifdef USE_GTK
                  xwidget_hide_view (xv);
#elif defined NS_IMPL_COCOA
                  nsxwidget_hide_view (xv);
#endif
                }
            }
        }
    }
}

#ifdef HAVE_X_WINDOWS
void
lower_frame_xwidget_views (struct frame *f)
{
  struct xwidget_view *xv;

  for (Lisp_Object tail = internal_xwidget_view_list; CONSP (tail);
       tail = XCDR (tail))
    {
      xv = XXWIDGET_VIEW (XCAR (tail));
      if (xv->frame == f && xv->wdesc != None)
	XLowerWindow (xv->dpy, xv->wdesc);
    }
}
#endif

#ifndef NS_IMPL_COCOA
void
kill_frame_xwidget_views (struct frame *f)
{
  Lisp_Object rem = Qnil;

  for (Lisp_Object tail = internal_xwidget_view_list; CONSP (tail);
       tail = XCDR (tail))
    {
      if (XWIDGET_VIEW_P (XCAR (tail))
	  && XXWIDGET_VIEW (XCAR (tail))->frame == f)
	rem = Fcons (XCAR (tail), rem);
    }

  for (; CONSP (rem); rem = XCDR (rem))
    Fdelete_xwidget_view (XCAR (rem));
}
#endif

static void
kill_xwidget (struct xwidget *xw)
{
  Lisp_Object val;
  XSETXWIDGET (val, xw);

  internal_xwidget_list = Fdelq (val, internal_xwidget_list);
  Vxwidget_list = Fcopy_sequence (internal_xwidget_list);
#ifdef USE_GTK
  xw->buffer = Qnil;

  if (xw->widget_osr && xw->widgetwindow_osr)
    {
      gtk_widget_destroy (xw->widget_osr);
      gtk_widget_destroy (xw->widgetwindow_osr);
    }

  if (xw->find_text)
    xfree (xw->find_text);

  if (!NILP (xw->script_callbacks))
    {
      for (ptrdiff_t idx = 0; idx < ASIZE (xw->script_callbacks); idx++)
	{
	  Lisp_Object cb = AREF (xw->script_callbacks, idx);
	  if (!NILP (cb))
	    xfree (xmint_pointer (XCAR (cb)));
	  ASET (xw->script_callbacks, idx, Qnil);
	}
    }

  xw->widget_osr = NULL;
  xw->widgetwindow_osr = NULL;
  xw->find_text = NULL;

  catch_child_signal ();
#elif defined NS_IMPL_COCOA
  nsxwidget_kill (xw);
#endif
}

/* Kill all xwidget in BUFFER.  */
void
kill_buffer_xwidgets (Lisp_Object buffer)
{
  Lisp_Object tail, xwidget;
  for (tail = Fget_buffer_xwidgets (buffer); CONSP (tail); tail = XCDR (tail))
    {
      xwidget = XCAR (tail);
      {
        CHECK_LIVE_XWIDGET (xwidget);
        struct xwidget *xw = XXWIDGET (xwidget);

	kill_xwidget (xw);
      }
    }

  catch_child_signal ();
}
