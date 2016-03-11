/* Support for embedding graphical components in a buffer.

Copyright (C) 2011-2016 Free Software Foundation, Inc.

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
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>

#include "xwidget.h"

#include <signal.h>

#include <stdio.h>
#include <setjmp.h>
#ifdef HAVE_X_WINDOWS

#include "lisp.h"
#include "blockinput.h"
#include "syssignal.h"

#include "xterm.h"
#include <X11/cursorfont.h>

#ifndef makedev
# include <sys/types.h>
#endif

#ifdef BSD_SYSTEM
# include <sys/ioctl.h>
#endif

#include "systime.h"

#ifndef INCLUDED_FCNTL
# include <fcntl.h>
#endif
#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <sys/stat.h>

#include "charset.h"
#include "character.h"
#include "coding.h"
#include "ccl.h"
#include "frame.h"
#include "dispextern.h"
#include "fontset.h"
#include "termhooks.h"
#include "termopts.h"
#include "termchar.h"
#include "disptab.h"
#include "buffer.h"
#include "window.h"
#include "keyboard.h"
#include "intervals.h"
#include "process.h"
#include "atimer.h"
#include "keymap.h"


#ifdef USE_X_TOOLKIT
#include <X11/Shell.h>
#endif
#include <X11/extensions/Xcomposite.h>
#include <X11/extensions/Xrender.h>
#include <cairo.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "gtkutil.h"
#include "font.h"
#endif /* HAVE_X_WINDOWS */

#include <gtk/gtk.h>
#include <gdk/gdk.h>

#include <gtk/gtkx.h>

#include "emacsgtkfixed.h"

#include <wchar.h>

#include <webkit/webkitwebview.h>
#include <webkit/webkitwebplugindatabase.h>
#include <webkit/webkitwebplugin.h>
#include <webkit/webkitglobals.h>
#include <webkit/webkitwebnavigationaction.h>
#include <webkit/webkitdownload.h>
#include <webkit/webkitwebpolicydecision.h>

static struct xwidget *
allocate_xwidget (void)
{
  return ALLOCATE_PSEUDOVECTOR (struct xwidget, height, PVEC_XWIDGET);
}

static struct xwidget_view *
allocate_xwidget_view (void)
{
  return ALLOCATE_PSEUDOVECTOR (struct xwidget_view, redisplayed,
                                PVEC_XWIDGET_VIEW);
}

#define XSETXWIDGET(a, b) XSETPSEUDOVECTOR (a, b, PVEC_XWIDGET)
#define XSETXWIDGET_VIEW(a, b) XSETPSEUDOVECTOR (a, b, PVEC_XWIDGET_VIEW)

static struct xwidget_view *xwidget_view_lookup (struct xwidget *,
						 struct window *);
static void webkit_document_load_finished_cb (WebKitWebView *, WebKitWebFrame *,
					      gpointer);
static gboolean webkit_download_cb (WebKitWebView *, WebKitDownload *, gpointer);

static gboolean
webkit_mime_type_policy_typedecision_requested_cb (WebKitWebView *,
                                                   WebKitWebFrame *,
                                                   WebKitNetworkRequest *,
                                                   gchar *,
                                                   WebKitWebPolicyDecision *,
                                                   gpointer);

static gboolean
webkit_new_window_policy_decision_requested_cb (WebKitWebView *,
                                                WebKitWebFrame *,
                                                WebKitNetworkRequest *,
                                                WebKitWebNavigationAction *,
                                                WebKitWebPolicyDecision *,
                                                gpointer);

static gboolean
webkit_navigation_policy_decision_requested_cb (WebKitWebView *,
                                                WebKitWebFrame *,
                                                WebKitNetworkRequest *,
                                                WebKitWebNavigationAction *,
                                                WebKitWebPolicyDecision *,
                                                gpointer);



DEFUN ("make-xwidget",
       Fmake_xwidget, Smake_xwidget,
       7, 8, 0,
       doc: /* Make an xwidget from BEG to END of TYPE.
If BUFFER is nil, use the current buffer.
If BUFFER is a string and no such buffer exists, create it.
TYPE is a symbol which can take one of the following values:

- webkit-osr

Returns the newly constructed xwidget, or nil if construction fails.  */)
  (Lisp_Object beg, Lisp_Object end, Lisp_Object type,
   Lisp_Object title, Lisp_Object width, Lisp_Object height,
   Lisp_Object arguments, Lisp_Object buffer)
{
  CHECK_SYMBOL (type);
  CHECK_NATNUM (width);
  CHECK_NATNUM (height);
  /* This should work a bit like "make-button"
     (make-button BEG END &rest PROPERTIES)
     TYPE etc. should be keyword args eventually.
     (make-xwidget 3 3 'button "oei" 31 31 nil)
     (xwidget-info (car xwidget-list))  */
  struct xwidget *xw = allocate_xwidget ();
  Lisp_Object val;
  xw->type = type;
  xw->title = title;
  xw->buffer = NILP (buffer) ? Fcurrent_buffer () : Fget_buffer_create (buffer);
  xw->height = XFASTINT (height);
  xw->width = XFASTINT (width);
  xw->kill_without_query = false;
  XSETXWIDGET (val, xw);
  Vxwidget_list = Fcons (val, Vxwidget_list);
  xw->widgetwindow_osr = NULL;
  xw->widget_osr = NULL;
  xw->plist = Qnil;

  if (EQ (xw->type, Qwebkit_osr))
    {
      block_input ();
      xw->widgetwindow_osr = gtk_offscreen_window_new ();
      gtk_window_resize (GTK_WINDOW (xw->widgetwindow_osr), xw->width,
                         xw->height);

      /* WebKit OSR is the only scrolled component at the moment.  */
      xw->widgetscrolledwindow_osr = NULL;

      if (EQ (xw->type, Qwebkit_osr))
        {
          xw->widgetscrolledwindow_osr = gtk_scrolled_window_new (NULL, NULL);
          gtk_scrolled_window_set_min_content_height
	    (GTK_SCROLLED_WINDOW (xw->widgetscrolledwindow_osr),
	     xw->height);
          gtk_scrolled_window_set_min_content_width
	    (GTK_SCROLLED_WINDOW (xw->widgetscrolledwindow_osr),
	     xw->width);
          gtk_scrolled_window_set_policy
	    (GTK_SCROLLED_WINDOW (xw->widgetscrolledwindow_osr),
	     GTK_POLICY_ALWAYS, GTK_POLICY_ALWAYS);

          xw->widget_osr = webkit_web_view_new ();
          gtk_container_add (GTK_CONTAINER (xw->widgetscrolledwindow_osr),
                             GTK_WIDGET (WEBKIT_WEB_VIEW (xw->widget_osr)));
        }

      gtk_widget_set_size_request (GTK_WIDGET (xw->widget_osr), xw->width,
                                   xw->height);

      if (EQ (xw->type, Qwebkit_osr))
        {
          gtk_container_add (GTK_CONTAINER (xw->widgetwindow_osr),
                             xw->widgetscrolledwindow_osr);
        }
      else
        {
          gtk_container_add (GTK_CONTAINER (xw->widgetwindow_osr),
                             xw->widget_osr);
        }

      gtk_widget_show (xw->widget_osr);
      gtk_widget_show (xw->widgetwindow_osr);
      gtk_widget_show (xw->widgetscrolledwindow_osr);

      /* Store some xwidget data in the gtk widgets for convenient
         retrieval in the event handlers.  */
      g_object_set_data (G_OBJECT (xw->widget_osr), XG_XWIDGET, xw);
      g_object_set_data (G_OBJECT (xw->widgetwindow_osr), XG_XWIDGET, xw);

      /* signals */
      if (EQ (xw->type, Qwebkit_osr))
        {
          g_signal_connect (G_OBJECT (xw->widget_osr),
                            "document-load-finished",
                            G_CALLBACK (webkit_document_load_finished_cb), xw);

          g_signal_connect (G_OBJECT (xw->widget_osr),
                            "download-requested",
                            G_CALLBACK (webkit_download_cb), xw);

          g_signal_connect (G_OBJECT (xw->widget_osr),
                            "mime-type-policy-decision-requested",
                            G_CALLBACK
                            (webkit_mime_type_policy_typedecision_requested_cb),
                            xw);

          g_signal_connect (G_OBJECT (xw->widget_osr),
                            "new-window-policy-decision-requested",
                            G_CALLBACK
                            (webkit_new_window_policy_decision_requested_cb),
                            xw);

          g_signal_connect (G_OBJECT (xw->widget_osr),
                            "navigation-policy-decision-requested",
                            G_CALLBACK
                            (webkit_navigation_policy_decision_requested_cb),
                            xw);
        }

      unblock_input ();
    }

  return val;
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

  for (tail = Vxwidget_list; CONSP (tail); tail = XCDR (tail))
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

static void
xwidget_show_view (struct xwidget_view *xv)
{
  xv->hidden = false;
  gtk_widget_show (xv->widgetwindow);
  gtk_fixed_move (GTK_FIXED (xv->emacswindow),
                  xv->widgetwindow,
                  xv->x + xv->clip_left,
                  xv->y + xv->clip_top);
}

/* Hide an xwidget view.  */
static void
xwidget_hide_view (struct xwidget_view *xv)
{
  xv->hidden = true;
  gtk_fixed_move (GTK_FIXED (xv->emacswindow), xv->widgetwindow,
                  10000, 10000);
}

/* When the off-screen webkit master view changes this signal is called.
   It copies the bitmap from the off-screen instance.  */
static gboolean
offscreen_damage_event (GtkWidget *widget, GdkEvent *event,
                        gpointer xv_widget)
{
  /* Queue a redraw of onscreen widget.
     There is a guard against receiving an invalid widget,
     which should only happen if we failed to remove the
     specific signal handler for the damage event.  */
  if (GTK_IS_WIDGET (xv_widget))
    gtk_widget_queue_draw (GTK_WIDGET (xv_widget));
  else
    printf ("Warning, offscreen_damage_event received invalid xv pointer:%p\n",
            xv_widget);

  return FALSE;
}

static void
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

/* TODO deprecated, use load-status.  */
void
webkit_document_load_finished_cb (WebKitWebView *webkitwebview,
                                  WebKitWebFrame *arg1,
                                  gpointer data)
{
  struct xwidget *xw = g_object_get_data (G_OBJECT (webkitwebview),
                                          XG_XWIDGET);

  store_xwidget_event_string (xw, "document-load-finished", "");
}

gboolean
webkit_download_cb (WebKitWebView *webkitwebview,
                    WebKitDownload *arg1,
                    gpointer data)
{
  struct xwidget *xw = g_object_get_data (G_OBJECT (webkitwebview),
                                          XG_XWIDGET);
  store_xwidget_event_string (xw, "download-requested",
                              webkit_download_get_uri (arg1));
  return FALSE;
}

static gboolean
webkit_mime_type_policy_typedecision_requested_cb (WebKitWebView *webView,
						   WebKitWebFrame *frame,
						   WebKitNetworkRequest *request,
						   gchar *mimetype,
						   WebKitWebPolicyDecision *policy_decision,
						   gpointer user_data)
{
  /* This function makes webkit send a download signal for all unknown
     mime types.  TODO: Defer the decision to Lisp, so that it's
     possible to make Emacs handle mime text for instance.  */
  if (!webkit_web_view_can_show_mime_type (webView, mimetype))
    {
      webkit_web_policy_decision_download (policy_decision);
      return TRUE;
    }
  else
    return FALSE;
}

static gboolean
webkit_new_window_policy_decision_requested_cb (WebKitWebView *webView,
						WebKitWebFrame *frame,
						WebKitNetworkRequest *request,
						WebKitWebNavigationAction *navigation_action,
						WebKitWebPolicyDecision *policy_decision,
						gpointer user_data)
{
  struct xwidget *xw = g_object_get_data (G_OBJECT (webView), XG_XWIDGET);
  webkit_web_navigation_action_get_original_uri (navigation_action);

  store_xwidget_event_string (xw, "new-window-policy-decision-requested",
                              webkit_web_navigation_action_get_original_uri
                              (navigation_action));
  return FALSE;
}

static gboolean
webkit_navigation_policy_decision_requested_cb (WebKitWebView *webView,
						WebKitWebFrame *frame,
						WebKitNetworkRequest *request,
						WebKitWebNavigationAction *navigation_action,
						WebKitWebPolicyDecision *policy_decision,
						gpointer user_data)
{
  struct xwidget *xw = g_object_get_data (G_OBJECT (webView), XG_XWIDGET);
  store_xwidget_event_string (xw, "navigation-policy-decision-requested",
                              webkit_web_navigation_action_get_original_uri
                              (navigation_action));
  return FALSE;
}

/* For gtk3 offscreen rendered widgets.  */
static gboolean
xwidget_osr_draw_cb (GtkWidget *widget, cairo_t *cr, gpointer data)
{
  struct xwidget *xw = g_object_get_data (G_OBJECT (widget), XG_XWIDGET);
  struct xwidget_view *xv = g_object_get_data (G_OBJECT (widget),
                                               XG_XWIDGET_VIEW);

  cairo_rectangle (cr, 0, 0, xv->clip_right, xv->clip_bottom);
  cairo_clip (cr);

  if (xw->widgetscrolledwindow_osr != NULL)
    gtk_widget_draw (xw->widgetscrolledwindow_osr, cr);
  else
    gtk_widget_draw (xw->widget_osr, cr);
  return FALSE;
}

static gboolean
xwidget_osr_event_forward (GtkWidget *widget, GdkEvent *event,
			   gpointer user_data)
{
  /* Copy events that arrive at the outer widget to the offscreen widget.  */
  struct xwidget *xw = g_object_get_data (G_OBJECT (widget), XG_XWIDGET);
  GdkEvent *eventcopy = gdk_event_copy (event);
  eventcopy->any.window = gtk_widget_get_window (xw->widget_osr);

  /* TODO: This might leak events.  They should be deallocated later,
     perhaps in xwgir_event_cb.  */
  gtk_main_do_event (eventcopy);

  /* Don't propagate this event further.  */
  return TRUE;
}

static gboolean
xwidget_osr_event_set_embedder (GtkWidget *widget, GdkEvent *event,
				gpointer data)
{
  struct xwidget_view *xv = data;
  struct xwidget *xww = XXWIDGET (xv->model);
  gdk_offscreen_window_set_embedder (gtk_widget_get_window
				     (xww->widgetwindow_osr),
                                     gtk_widget_get_window (xv->widget));
  return FALSE;
}


/* Initializes and does initial placement of an xwidget view on screen.  */
static struct xwidget_view *
xwidget_init_view (struct xwidget *xww,
                   struct glyph_string *s,
                   int x, int y)
{
  struct xwidget_view *xv = allocate_xwidget_view ();
  Lisp_Object val;

  XSETXWIDGET_VIEW (val, xv);
  Vxwidget_view_list = Fcons (val, Vxwidget_view_list);

  XSETWINDOW (xv->w, s->w);
  XSETXWIDGET (xv->model, xww);

  if (EQ (xww->type, Qwebkit_osr))
    {
      xv->widget = gtk_drawing_area_new ();
      /* Expose event handling.  */
      gtk_widget_set_app_paintable (xv->widget, TRUE);
      gtk_widget_add_events (xv->widget, GDK_ALL_EVENTS_MASK);

      /* Draw the view on damage-event.  */
      g_signal_connect (G_OBJECT (xww->widgetwindow_osr), "damage-event",
                        G_CALLBACK (offscreen_damage_event), xv->widget);

      if (EQ (xww->type, Qwebkit_osr))
        {
          g_signal_connect (G_OBJECT (xv->widget), "button-press-event",
                            G_CALLBACK (xwidget_osr_event_forward), NULL);
          g_signal_connect (G_OBJECT (xv->widget), "button-release-event",
                            G_CALLBACK (xwidget_osr_event_forward), NULL);
          g_signal_connect (G_OBJECT (xv->widget), "motion-notify-event",
                            G_CALLBACK (xwidget_osr_event_forward), NULL);
        }
      else
        {
          /* xwgir debug, orthogonal to forwarding.  */
          g_signal_connect (G_OBJECT (xv->widget), "enter-notify-event",
                            G_CALLBACK (xwidget_osr_event_set_embedder), xv);
        }
      g_signal_connect (G_OBJECT (xv->widget), "draw",
                        G_CALLBACK (xwidget_osr_draw_cb), NULL);
    }

  /* Widget realization.

     Make container widget first, and put the actual widget inside the
     container later.  Drawing should crop container window if necessary
     to handle case where xwidget is partially obscured by other Emacs
     windows.  Other containers than gtk_fixed where explored, but
     gtk_fixed had the most predictable behavior so far.  */

  xv->emacswindow = FRAME_GTK_WIDGET (s->f);
  xv->widgetwindow = gtk_fixed_new ();
  gtk_widget_set_has_window (xv->widgetwindow, TRUE);
  gtk_container_add (GTK_CONTAINER (xv->widgetwindow), xv->widget);

  /* Store some xwidget data in the gtk widgets.  */
  g_object_set_data (G_OBJECT (xv->widget), XG_FRAME_DATA, s->f);
  g_object_set_data (G_OBJECT (xv->widget), XG_XWIDGET, xww);
  g_object_set_data (G_OBJECT (xv->widget), XG_XWIDGET_VIEW, xv);
  g_object_set_data (G_OBJECT (xv->widgetwindow), XG_XWIDGET, xww);
  g_object_set_data (G_OBJECT (xv->widgetwindow), XG_XWIDGET_VIEW, xv);

  gtk_widget_set_size_request (GTK_WIDGET (xv->widget), xww->width,
                               xww->height);
  gtk_widget_set_size_request (xv->widgetwindow, xww->width, xww->height);
  gtk_fixed_put (GTK_FIXED (FRAME_GTK_WIDGET (s->f)), xv->widgetwindow, x, y);
  xv->x = x;
  xv->y = y;
  gtk_widget_show_all (xv->widgetwindow);

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
  int clip_right;
  int clip_bottom;
  int clip_top;
  int clip_left;

  int x = s->x;
  int y = s->y + (s->height / 2) - (xww->height / 2);

  /* Do initialization here in the display loop because there is no
     other time to know things like window placement etc.  */
  xv = xwidget_init_view (xww, s, x, y);

  /* Calculate clipping, which is used for all manner of onscreen
     xwidget views.  Each widget border can get clipped by other emacs
     objects so there are four clipping variables.  */
  clip_right =
    min (xww->width,
         WINDOW_RIGHT_EDGE_X (s->w) - x -
         WINDOW_RIGHT_SCROLL_BAR_AREA_WIDTH (s->w) -
         WINDOW_RIGHT_FRINGE_WIDTH (s->w));
  clip_left =
    max (0,
         WINDOW_LEFT_EDGE_X (s->w) - x +
         WINDOW_LEFT_SCROLL_BAR_AREA_WIDTH (s->w) +
         WINDOW_LEFT_FRINGE_WIDTH (s->w));

  clip_bottom =
    min (xww->height,
         WINDOW_BOTTOM_EDGE_Y (s->w) - WINDOW_MODE_LINE_HEIGHT (s->w) - y);
  clip_top = max (0, WINDOW_TOP_EDGE_Y (s->w) - y);

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
  xv->x = x;
  xv->y = y;

  /* Has it moved?  */
  if (moved)
    gtk_fixed_move (GTK_FIXED (FRAME_GTK_WIDGET (s->f)),
		    xv->widgetwindow, x + clip_left, y + clip_top);

  /* Clip the widget window if some parts happen to be outside
     drawable area.  An Emacs window is not a gtk window.  A gtk window
     covers the entire frame.  Clipping might have changed even if we
     haven't actually moved; try to figure out when we need to reclip
     for real.  */
  if (xv->clip_right != clip_right
      || xv->clip_bottom != clip_bottom
      || xv->clip_top != clip_top || xv->clip_left != clip_left)
    {
      gtk_widget_set_size_request (xv->widgetwindow, clip_right + clip_left,
                                   clip_bottom + clip_top);
      gtk_fixed_move (GTK_FIXED (xv->widgetwindow), xv->widget, -clip_left,
                      -clip_top);

      xv->clip_right = clip_right;
      xv->clip_bottom = clip_bottom;
      xv->clip_top = clip_top;
      xv->clip_left = clip_left;
    }

  /* If emacs wants to repaint the area where the widget lives, queue
     a redraw.  It seems its possible to get out of sync with emacs
     redraws so emacs background sometimes shows up instead of the
     xwidgets background.  It's just a visual glitch though.  */
  if (!xwidget_hidden (xv))
    {
      gtk_widget_queue_draw (xv->widgetwindow);
      gtk_widget_queue_draw (xv->widget);
    }
}

/* Macro that checks WEBKIT_IS_WEB_VIEW (xw->widget_osr) first.  */
#define WEBKIT_FN_INIT()						\
  CHECK_XWIDGET (xwidget);						\
  struct xwidget *xw = XXWIDGET (xwidget);				\
  if (!xw->widget_osr || !WEBKIT_IS_WEB_VIEW (xw->widget_osr))		\
    {									\
      printf ("ERROR xw->widget_osr does not hold a webkit instance\n"); \
      return Qnil;							\
    }

DEFUN ("xwidget-webkit-goto-uri",
       Fxwidget_webkit_goto_uri, Sxwidget_webkit_goto_uri,
       2, 2, 0,
       doc: /* Make the xwidget webkit instance referenced by XWIDGET browse URI.  */)
  (Lisp_Object xwidget, Lisp_Object uri)
{
  WEBKIT_FN_INIT ();
  CHECK_STRING (uri);
  webkit_web_view_load_uri (WEBKIT_WEB_VIEW (xw->widget_osr), SSDATA (uri));
  return Qnil;
}


DEFUN ("xwidget-webkit-execute-script",
       Fxwidget_webkit_execute_script, Sxwidget_webkit_execute_script,
       2, 2, 0,
       doc: /* Make the Webkit XWIDGET execute JavaScript SCRIPT.  */)
  (Lisp_Object xwidget, Lisp_Object script)
{
  WEBKIT_FN_INIT ();
  CHECK_STRING (script);
  webkit_web_view_execute_script (WEBKIT_WEB_VIEW (xw->widget_osr),
                                  SSDATA (script));
  return Qnil;
}

DEFUN ("xwidget-webkit-get-title",
       Fxwidget_webkit_get_title, Sxwidget_webkit_get_title,
       1, 1, 0,
       doc: /* Return the title from the Webkit instance in XWIDGET.
This can be used to work around the lack of a return value from the
exec method.  */ )
  (Lisp_Object xwidget)
{
  /* TODO support multibyte strings.  */
  WEBKIT_FN_INIT ();
  const gchar *str =
    webkit_web_view_get_title (WEBKIT_WEB_VIEW (xw->widget_osr));
  if (str == 0)
    {
      /* TODO maybe return Qnil instead.  I suppose webkit returns
	 null pointer when doc is not properly loaded or something.  */
      return build_string ("");
    }
  return build_string (str);
}

DEFUN ("xwidget-resize", Fxwidget_resize, Sxwidget_resize, 3, 3, 0,
       doc: /* Resize XWIDGET.  NEW_WIDTH, NEW_HEIGHT define the new size.  */ )
  (Lisp_Object xwidget, Lisp_Object new_width, Lisp_Object new_height)
{
  CHECK_XWIDGET (xwidget);
  CHECK_NATNUM (new_width);
  CHECK_NATNUM (new_height);
  struct xwidget *xw = XXWIDGET (xwidget);
  int w = XFASTINT (new_width);
  int h = XFASTINT (new_height);

  xw->width = w;
  xw->height = h;

  /* If there is an offscreen widget resize it first.  */
  if (xw->widget_osr)
    {
      /* Use minimum size.  */
      gtk_widget_set_size_request (GTK_WIDGET (xw->widget_osr),
                                   xw->width, xw->height);

      gtk_window_resize (GTK_WINDOW (xw->widgetwindow_osr), xw->width,
                         xw->height);
      gtk_scrolled_window_set_min_content_height
	(GTK_SCROLLED_WINDOW (xw->widgetscrolledwindow_osr),
	 xw->height);
      gtk_scrolled_window_set_min_content_width
	(GTK_SCROLLED_WINDOW (xw->widgetscrolledwindow_osr),
	 xw->width);

      gtk_container_resize_children (GTK_CONTAINER (xw->widgetwindow_osr));

    }

  for (Lisp_Object tail = Vxwidget_view_list; CONSP (tail); tail = XCDR (tail))
    {
      if (XWIDGET_VIEW_P (XCAR (tail)))
        {
          struct xwidget_view *xv = XXWIDGET_VIEW (XCAR (tail));
          if (XXWIDGET (xv->model) == xw)
              gtk_widget_set_size_request (GTK_WIDGET (xv->widget), xw->width,
                                           xw->height);
        }
    }

  return Qnil;
}



DEFUN ("xwidget-set-adjustment",
       Fxwidget_set_adjustment, Sxwidget_set_adjustment, 4, 4, 0,
       doc: /* Set native scrolling for XWIDGET.
AXIS can be `vertical' or `horizontal'.
If RELATIVE is t, scroll relative, otherwise absolutely.
VALUE is the amount to scroll, either relatively or absolutely.  */)
  (Lisp_Object xwidget, Lisp_Object axis, Lisp_Object relative,
   Lisp_Object value)
{
  CHECK_XWIDGET (xwidget);
  CHECK_NATNUM (value);
  struct xwidget *xw = XXWIDGET (xwidget);
  GtkAdjustment *adjustment
    = ((EQ (Qhorizontal, axis)
	? gtk_scrolled_window_get_hadjustment
	: gtk_scrolled_window_get_vadjustment)
       (GTK_SCROLLED_WINDOW (xw->widgetscrolledwindow_osr)));
  double final_value = XFASTINT (value);
  if (EQ (Qt, relative))
    final_value += gtk_adjustment_get_value (adjustment);
  gtk_adjustment_set_value (adjustment, final_value);
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
  CHECK_XWIDGET (xwidget);
  GtkRequisition requisition;
  gtk_widget_size_request (XXWIDGET (xwidget)->widget_osr, &requisition);
  return list2 (make_number (requisition.width),
		make_number (requisition.height));
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
  CHECK_XWIDGET (xwidget);
  struct xwidget *xw = XXWIDGET (xwidget);
  return CALLN (Fvector, xw->type, xw->title,
		make_natnum (xw->width), make_natnum (xw->height));
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
  return CALLN (Fvector, make_number (xv->x), make_number (xv->y),
		make_number (xv->clip_right), make_number (xv->clip_bottom),
		make_number (xv->clip_top), make_number (xv->clip_left));
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
  gtk_widget_destroy (xv->widgetwindow);
  Vxwidget_view_list = Fdelq (xwidget_view, Vxwidget_view_list);
  /* xv->model still has signals pointing to the view.  There can be
     several views.  Find the matching signals and delete them all.  */
  g_signal_handlers_disconnect_matched  (XXWIDGET (xv->model)->widgetwindow_osr,
                                         G_SIGNAL_MATCH_DATA,
                                         0, 0, 0, 0,
                                         xv->widget);
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

  for (Lisp_Object tail = Vxwidget_view_list; CONSP (tail);
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
  CHECK_XWIDGET (xwidget);
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

DEFUN ("set-xwidget-plist",
       Fset_xwidget_plist, Sset_xwidget_plist,
       2, 2, 0,
       doc: /* Replace the plist of XWIDGET with PLIST.
Returns PLIST.  */)
  (Lisp_Object xwidget, Lisp_Object plist)
{
  CHECK_XWIDGET (xwidget);
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
  CHECK_XWIDGET (xwidget);
  XXWIDGET (xwidget)->kill_without_query = NILP (flag);
  return flag;
}

DEFUN ("xwidget-query-on-exit-flag",
       Fxwidget_query_on_exit_flag, Sxwidget_query_on_exit_flag,
       1, 1, 0,
       doc: /* Return the current value of the query-on-exit flag for XWIDGET.  */)
  (Lisp_Object xwidget)
{
  CHECK_XWIDGET (xwidget);
  return (XXWIDGET (xwidget)->kill_without_query ? Qnil : Qt);
}

void
syms_of_xwidget (void)
{
  defsubr (&Smake_xwidget);
  defsubr (&Sxwidgetp);
  DEFSYM (Qxwidgetp, "xwidgetp");
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

  defsubr (&Sxwidget_webkit_goto_uri);
  defsubr (&Sxwidget_webkit_execute_script);
  defsubr (&Sxwidget_webkit_get_title);
  DEFSYM (Qwebkit_osr, "webkit-osr");

  defsubr (&Sxwidget_size_request);
  defsubr (&Sdelete_xwidget_view);

  defsubr (&Sxwidget_plist);
  defsubr (&Sxwidget_buffer);
  defsubr (&Sset_xwidget_plist);

  defsubr (&Sxwidget_set_adjustment);

  DEFSYM (Qxwidget, "xwidget");

  DEFSYM (QCxwidget, ":xwidget");
  DEFSYM (QCtitle, ":title");

  /* Do not forget to update the docstring of make-xwidget if you add
     new types.  */

  DEFSYM (Qvertical, "vertical");
  DEFSYM (Qhorizontal, "horizontal");

  DEFSYM (QCplist, ":plist");

  DEFVAR_LISP ("xwidget-list", Vxwidget_list,
               doc:	/* xwidgets list.  */);
  Vxwidget_list = Qnil;

  DEFVAR_LISP ("xwidget-view-list", Vxwidget_view_list,
             doc:	/* xwidget views list.  */);
  Vxwidget_view_list = Qnil;

  Fprovide (intern ("xwidget-internal"), Qnil);
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
  for (Lisp_Object tail = Vxwidget_view_list; CONSP (tail);
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

  return EQ (ret, Qnil) ? NULL : XXWIDGET_VIEW (ret);
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
  for (Lisp_Object tail = Vxwidget_view_list; CONSP (tail);
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
		  xwidget_touch (xwidget_view_lookup (glyph->u.xwidget, w));
		}
	  }
    }

  for (Lisp_Object tail = Vxwidget_view_list; CONSP (tail);
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
                xwidget_show_view (xv);
              else
                xwidget_hide_view (xv);
            }
        }
    }
}

/* Kill all xwidget in BUFFER.  */
void
kill_buffer_xwidgets (Lisp_Object buffer)
{
  Lisp_Object tail, xwidget;
  for (tail = Fget_buffer_xwidgets (buffer); CONSP (tail); tail = XCDR (tail))
    {
      xwidget = XCAR (tail);
      Vxwidget_list = Fdelq (xwidget, Vxwidget_list);
      /* TODO free the GTK things in xw.  */
      {
        CHECK_XWIDGET (xwidget);
        struct xwidget *xw = XXWIDGET (xwidget);
        if (xw->widget_osr && xw->widgetwindow_osr)
          {
            gtk_widget_destroy (xw->widget_osr);
            gtk_widget_destroy (xw->widgetwindow_osr);
          }
      }
    }
}
