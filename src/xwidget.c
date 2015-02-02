#include <config.h>
#ifdef HAVE_XWIDGETS

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
#include <sys/types.h>
#endif /* makedev */

#ifdef BSD_SYSTEM
#include <sys/ioctl.h>
#endif /* ! defined (BSD_SYSTEM) */

#include "systime.h"

#ifndef INCLUDED_FCNTL
#include <fcntl.h>
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
#include "emacs-icon.h"
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
#endif  /* HAVE_X_WINDOWS */

#include <gtk/gtk.h>
#include <gdk/gdk.h>

#ifdef HAVE_GTK3
//for gtk3; sockets and plugs
#include <gtk/gtkx.h>
#include <gtk/gtkscrolledwindow.h>
#include "emacsgtkfixed.h"
#endif

#include <wchar.h>

#ifdef HAVE_WEBKIT_OSR
#include <webkit/webkitwebview.h>
#include <webkit/webkitwebplugindatabase.h>
#include <webkit/webkitwebplugin.h>
#include <webkit/webkitglobals.h>
#include <webkit/webkitwebnavigationaction.h>
#include <webkit/webkitdownload.h>
#include <webkit/webkitwebpolicydecision.h>
#endif

//for GIR
#include <girepository.h>

#include "xwidget.h"

//TODO embryo of lisp allocators for xwidgets
//TODO xwidget* should be Lisp_xwidget*
struct xwidget*
allocate_xwidget (void)
{
  return ALLOCATE_PSEUDOVECTOR (struct xwidget, height, PVEC_XWIDGET);
}

//TODO xwidget_view* should be Lisp_xwidget_view*
struct xwidget_view*
allocate_xwidget_view (void)
{
  return ALLOCATE_PSEUDOVECTOR (struct xwidget_view, redisplayed, PVEC_XWIDGET_VIEW);
}
#define XSETXWIDGET(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_XWIDGET))
#define XSETXWIDGET_VIEW(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_XWIDGET_VIEW))

struct xwidget_view* xwidget_view_lookup(struct xwidget* xw,     struct window *w);
Lisp_Object xwidget_spec_value ( Lisp_Object spec, Lisp_Object  key,  int *found);
gboolean offscreen_damage_event (GtkWidget *widget, GdkEvent *event, gpointer data);
void     webkit_osr_document_load_finished_callback (WebKitWebView  *webkitwebview,
                                                     WebKitWebFrame *arg1,
                                                     gpointer        user_data);
gboolean     webkit_osr_download_callback (WebKitWebView  *webkitwebview,
                                       WebKitDownload *arg1,
                                       gpointer        data);

gboolean  webkit_osr_mime_type_policy_typedecision_requested_callback(WebKitWebView           *webView,
                                                                      WebKitWebFrame          *frame,
                                                                      WebKitNetworkRequest    *request,
                                                                      gchar                   *mimetype,
                                                                      WebKitWebPolicyDecision *policy_decision,
                                                                      gpointer                 user_data);

gboolean webkit_osr_new_window_policy_decision_requested_callback(WebKitWebView             *webView,
                                                                  WebKitWebFrame            *frame,
                                                                  WebKitNetworkRequest      *request,
                                                                  WebKitWebNavigationAction *navigation_action,
                                                                  WebKitWebPolicyDecision   *policy_decision,
                                                                  gpointer                   user_data);


gboolean webkit_osr_navigation_policy_decision_requested_callback(WebKitWebView             *webView,
                                                        WebKitWebFrame            *frame,
                                                        WebKitNetworkRequest      *request,
                                                        WebKitWebNavigationAction *navigation_action,
                                                        WebKitWebPolicyDecision   *policy_decision,
                                                                  gpointer                   user_data);

GtkWidget* xwgir_create(char* class, char* namespace);



static void
send_xembed_ready_event (struct xwidget* xw, int xembedid);
DEFUN ("make-xwidget", Fmake_xwidget, Smake_xwidget, 7, 8, 0,
         doc: /* Make an xwidget from BEG to END of TYPE.

If BUFFER is nil it uses the current buffer. If BUFFER is a string and
no such buffer exists, it is created.

TYPE is a symbol which can take one of the following values:
- Button
- ToggleButton
- slider
- socket
- socket-osr
- cairo
*/
         )
  (Lisp_Object beg, Lisp_Object end,
   Lisp_Object type,
   Lisp_Object title,
   Lisp_Object width, Lisp_Object height,
   Lisp_Object data,
   Lisp_Object buffer)
{
  //should work a bit like "make-button"(make-button BEG END &rest PROPERTIES)
  // arg "type" and fwd should be keyword args eventually
  //(make-xwidget 3 3 'button "oei" 31 31 nil)
  //(xwidget-info (car xwidget-list))
  struct xwidget* xw = allocate_xwidget();
  Lisp_Object val;
  xw->type = type;
  xw->title = title;
  if (NILP (buffer))
      buffer = Fcurrent_buffer(); // no need to gcpro because Fcurrent_buffer doesn't call Feval/eval_sub.
  else
      buffer = Fget_buffer_create (buffer);
  xw->buffer = buffer;

  xw->height = XFASTINT(height);
  xw->width = XFASTINT(width);
  xw->kill_without_query = 0;
  XSETXWIDGET (val, xw); // set the vectorlike_header of VAL with the correct value
  Vxwidget_list = Fcons (val, Vxwidget_list);
  xw->widgetwindow_osr = NULL;
  xw->widget_osr = NULL;
  xw->plist = Qnil;




#ifdef HAVE_WEBKIT_OSR
  /* DIY mvc. widget is rendered offscreen,
     later bitmap copied to the views.
   */
  if (EQ(xw->type, Qwebkit_osr)||
      EQ(xw->type, Qsocket_osr)||
      (!NILP (Fget(xw->type, QCxwgir_class)))) {
      block_input();
      xw->widgetwindow_osr = gtk_offscreen_window_new ();
      gtk_window_resize(GTK_WINDOW(xw->widgetwindow_osr), xw->width, xw->height);
      xw->widgetscrolledwindow_osr = NULL; //webkit osr is the only scrolled component atm

      if (EQ(xw->type, Qwebkit_osr)){
        xw->widgetscrolledwindow_osr =   gtk_scrolled_window_new(NULL, NULL);
        gtk_scrolled_window_set_min_content_height(GTK_SCROLLED_WINDOW(xw->widgetscrolledwindow_osr ),xw->height);
        gtk_scrolled_window_set_min_content_width(GTK_SCROLLED_WINDOW(xw->widgetscrolledwindow_osr ),xw->width);
        gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(xw->widgetscrolledwindow_osr ), GTK_POLICY_ALWAYS, GTK_POLICY_ALWAYS);

        xw->widget_osr=webkit_web_view_new();
        gtk_container_add(GTK_CONTAINER(xw->widgetscrolledwindow_osr ), GTK_WIDGET( WEBKIT_WEB_VIEW(xw->widget_osr)));
      }
      if(EQ(xw->type, Qsocket_osr))
          xw->widget_osr = gtk_socket_new();
      if(!NILP (Fget(xw->type, QCxwgir_class)))
          xw->widget_osr = xwgir_create(SDATA(Fcar(Fcdr(Fget(xw->type, QCxwgir_class)))),
                                        SDATA(Fcar(Fget(xw->type, QCxwgir_class))));

      gtk_widget_set_size_request (GTK_WIDGET (xw->widget_osr), xw->width, xw->height);

      if (EQ(xw->type, Qwebkit_osr)){
        gtk_container_add (GTK_CONTAINER (xw->widgetwindow_osr), xw->widgetscrolledwindow_osr);
      }else{
        gtk_container_add (GTK_CONTAINER (xw->widgetwindow_osr), xw->widget_osr);
      }

      gtk_widget_show (xw->widget_osr);
      gtk_widget_show (xw->widgetwindow_osr);
      gtk_widget_show (xw->widgetscrolledwindow_osr);

      /* store some xwidget data in the gtk widgets for convenient retrieval in the event handlers. */
      g_object_set_data (G_OBJECT (xw->widget_osr), XG_XWIDGET, (gpointer) (xw));
      g_object_set_data (G_OBJECT (xw->widgetwindow_osr), XG_XWIDGET, (gpointer) (xw));

      /* signals */
      if (EQ(xw->type, Qwebkit_osr)) {
          g_signal_connect (G_OBJECT (xw->widget_osr),
                            "document-load-finished",
                            G_CALLBACK (webkit_osr_document_load_finished_callback),
                            xw);

          g_signal_connect (G_OBJECT (xw->widget_osr),
                            "download-requested",
                            G_CALLBACK (webkit_osr_download_callback),
                            xw);

          g_signal_connect (G_OBJECT (xw->widget_osr),
                            "mime-type-policy-decision-requested",
                            G_CALLBACK (webkit_osr_mime_type_policy_typedecision_requested_callback),
                            xw);

          g_signal_connect (G_OBJECT (xw->widget_osr),
                            "new-window-policy-decision-requested",
                            G_CALLBACK (webkit_osr_new_window_policy_decision_requested_callback),
                            xw);

          g_signal_connect (G_OBJECT (xw->widget_osr),
                            "navigation-policy-decision-requested",
                            G_CALLBACK (webkit_osr_navigation_policy_decision_requested_callback),
                            xw);
      }

      if (EQ(xw->type, Qsocket_osr)) {
          send_xembed_ready_event (xw, gtk_socket_get_id (GTK_SOCKET (xw->widget_osr)));
          //gtk_widget_realize(xw->widget);
      }


      unblock_input();

  }
#endif  /* HAVE_WEBKIT_OSR */

  return val;
}

DEFUN ("get-buffer-xwidgets", Fget_buffer_xwidgets, Sget_buffer_xwidgets, 1, 1, 0,
       doc: /* Return a list of xwidgets associated with BUFFER.
BUFFER may be a buffer or the name of one.
       */
       )
     (Lisp_Object buffer)
{
    Lisp_Object xw, tail, xw_list;

    if (NILP (buffer)) return Qnil;
    buffer = Fget_buffer (buffer);
    if (NILP (buffer)) return Qnil;

    xw_list = Qnil;

    for (tail = Vxwidget_list; CONSP (tail); tail = XCDR (tail))
        {
            xw = XCAR (tail);
            if (XWIDGETP (xw) && EQ (Fxwidget_buffer (xw), buffer))
                xw_list = Fcons (xw, xw_list);
        }
    return xw_list;
}

int
xwidget_hidden(struct xwidget_view *xv)
{
  return  xv->hidden;
}


static void
buttonclick_handler (GtkWidget * widget, gpointer data)
{
  Lisp_Object xwidget_view, xwidget;
  XSETXWIDGET_VIEW (xwidget_view, (struct xwidget_view *) data);
  xwidget = Fxwidget_view_model (xwidget_view);

  struct input_event event;
  Lisp_Object frame = Fwindow_frame (Fxwidget_view_window (xwidget_view));
  struct frame *f = XFRAME (frame);
  printf ("button clicked xw:%d '%s'\n", XXWIDGET (xwidget), XXWIDGET (xwidget)->title);

  EVENT_INIT (event);
  event.kind = XWIDGET_EVENT;

  event.frame_or_window = frame;

  event.arg = Qnil;
  event.arg = Fcons (xwidget, event.arg);
  event.arg = Fcons (intern ("buttonclick"), event.arg);

  kbd_buffer_store_event (&event);
}


static void
send_xembed_ready_event (struct xwidget* xw, int xembedid)
{
  Lisp_Object xw_lo;
  XSETXWIDGET(xw_lo, xw);
  struct input_event event;
  EVENT_INIT (event);
  event.kind = XWIDGET_EVENT;
  event.frame_or_window = Qnil;	//frame; //how to get the frame here? //TODO i store it in the xwidget now

  event.arg = Qnil;
  event.arg = Fcons (make_number (xembedid), event.arg);
  event.arg = Fcons (xw_lo, event.arg);
  event.arg = Fcons (intern ("xembed-ready"), event.arg);


  kbd_buffer_store_event (&event);

}

void
xwidget_show_view (struct xwidget_view *xv)
{
  xv->hidden = 0;
  gtk_widget_show(xv->widgetwindow);
  gtk_fixed_move (GTK_FIXED (xv->emacswindow), xv->widgetwindow,  xv->x  + xv->clip_left, xv->y + xv->clip_top); //TODO refactor
}


/* hide an xvidget view */
void
xwidget_hide_view (struct xwidget_view *xv)
{
  xv->hidden = 1;
  //gtk_widget_hide(xw->widgetwindow);
  gtk_fixed_move (GTK_FIXED (xv->emacswindow), xv->widgetwindow,
                  10000, 10000);
}


void
xwidget_plug_added(GtkSocket *socket,
                   gpointer   user_data)
{
  //hmm this doesnt seem to get called for foreign windows
  printf("xwidget_plug_added\n");
}

gboolean
xwidget_plug_removed(GtkSocket *socket,
                     gpointer   user_data)
{
  printf("xwidget_plug_removed\n");
  return TRUE; /* dont run the default handler because that kills the socket and we want to reuse it*/
}


void
xwidget_slider_changed (GtkRange *range,
                        gpointer  user_data)
{
  //slider value changed. change value of siblings
  //correspondingly. but remember that changing value will again
  //trigger signal

  //TODO MVC view storage wont be an array futureish so the loop needs to change eventually
  //TODO MVC it would be nice if this code could be reusable but, alas, C is not a functional language
  //issues are:
  // - the type of the controllers value (double, boolean etc)
  // - the getter and setter (but they can be func pointers)
  // a behemoth macro is always an option.
  double v=gtk_range_get_value(range);
  struct xwidget_view* xvp = g_object_get_data (G_OBJECT (range), XG_XWIDGET_VIEW);
  struct xwidget_view* xv;

  printf("slider changed val:%f\n", v);

  for (Lisp_Object tail = Vxwidget_view_list; CONSP (tail); tail = XCDR (tail))
    {
      if (XWIDGET_VIEW_P (XCAR (tail))) {
        xv = XXWIDGET_VIEW (XCAR (tail));
        if (EQ (xvp->model, xv->model)) {
          //block sibling views signal handlers
          g_signal_handler_block(xv->widget, xv->handler_id);

          //set values of sibling views and unblock
          gtk_range_set_value(GTK_RANGE(xv->widget), v);
          g_signal_handler_unblock(xv->widget,xv->handler_id);
        }
      }
    }
}


/* when the off-screen webkit master view changes this signal is called.
   it copies the bitmap from the off-screen webkit instance */
gboolean
offscreen_damage_event (GtkWidget *widget, GdkEvent *event, gpointer data)
{
  //TODO this is wrong! should just queu a redraw of onscreen widget
  gtk_widget_queue_draw (GTK_WIDGET (data));
  return FALSE;
}

void
store_xwidget_event_string(struct xwidget* xw, char* eventname, const char* eventstr)
{
  //refactor attempt
  struct input_event event;
  Lisp_Object xwl;
  XSETXWIDGET(xwl,xw);
  EVENT_INIT (event);
  event.kind = XWIDGET_EVENT;
  event.frame_or_window = Qnil;	//frame; //how to get the frame here? //TODO i store it in the xwidget now

  event.arg = Qnil;
  event.arg = Fcons (build_string(eventstr), event.arg);  //string so dont intern
  event.arg = Fcons (xwl, event.arg); //TODO
  event.arg = Fcons (intern (eventname), event.arg);//interning should be ok
  kbd_buffer_store_event (&event);

}

//TODO deprecated, use load-status
void
webkit_osr_document_load_finished_callback (WebKitWebView  *webkitwebview,
                                                     WebKitWebFrame *arg1,
                                                     gpointer        data)
{
  //TODO this event sending code should be refactored
  //  struct xwidget *xw = (struct xwidget *) data;
  struct xwidget* xw = (struct xwidget*) g_object_get_data (G_OBJECT (webkitwebview), XG_XWIDGET);
  printf("webkit finished loading\n");

  store_xwidget_event_string(xw,
                             "document-load-finished", "");
}

gboolean
webkit_osr_download_callback (WebKitWebView  *webkitwebview,
                                       WebKitDownload *arg1,
                                       gpointer        data)
{
  //TODO this event sending code should be refactored
  struct input_event event;
  //  struct xwidget *xw = (struct xwidget *) data;
  struct xwidget* xw = (struct xwidget*) g_object_get_data (G_OBJECT (webkitwebview), XG_XWIDGET);
  printf("download requested %s\n", webkit_download_get_uri (arg1));


  printf("webkit finished loading\n");

  store_xwidget_event_string(xw, "download-requested", webkit_download_get_uri (arg1));

  return FALSE;
}

gboolean
webkit_osr_mime_type_policy_typedecision_requested_callback(WebKitWebView           *webView,
                                                            WebKitWebFrame          *frame,
                                                            WebKitNetworkRequest    *request,
                                                            gchar                   *mimetype,
                                                            WebKitWebPolicyDecision *policy_decision,
                                                            gpointer                 user_data)
{
  printf("mime policy requested\n");
  // this function makes webkit send a download signal for all unknown mime types
  // TODO defer the decision to lisp, so that its possible to make Emacs handle text mime for instance
  if(!webkit_web_view_can_show_mime_type(webView, mimetype)){
    webkit_web_policy_decision_download (policy_decision);
    return TRUE;
  }else{
    return FALSE;
  }
}


gboolean
webkit_osr_new_window_policy_decision_requested_callback(WebKitWebView             *webView,
                                                         WebKitWebFrame            *frame,
                                                         WebKitNetworkRequest      *request,
                                                         WebKitWebNavigationAction *navigation_action,
                                                         WebKitWebPolicyDecision   *policy_decision,
                                                         gpointer                   user_data)
{
  struct xwidget* xw = (struct xwidget*) g_object_get_data (G_OBJECT (webView), XG_XWIDGET);
  printf("webkit_osr_new_window_policy_decision_requested_callback %s\n",
         webkit_web_navigation_action_get_original_uri (navigation_action));

  store_xwidget_event_string(xw,  "new-window-policy-decision-requested", webkit_web_navigation_action_get_original_uri (navigation_action)
                            );
  return FALSE;
}

gboolean
webkit_osr_navigation_policy_decision_requested_callback(WebKitWebView             *webView,
                                                         WebKitWebFrame            *frame,
                                                         WebKitNetworkRequest      *request,
                                                         WebKitWebNavigationAction *navigation_action,
                                                         WebKitWebPolicyDecision   *policy_decision,
                                                         gpointer                   user_data)
{
  struct xwidget* xw = (struct xwidget*) g_object_get_data (G_OBJECT (webView), XG_XWIDGET);
  printf("webkit_osr_navigation_policy_decision_requested_callback %s\n",
         webkit_web_navigation_action_get_original_uri (navigation_action));
  store_xwidget_event_string(xw,  "navigation-policy-decision-requested", webkit_web_navigation_action_get_original_uri (navigation_action)
                            );
  return FALSE;
}

//for gtk3 offscreen rendered widgets
gboolean
xwidget_osr_draw_callback (GtkWidget *widget, cairo_t *cr, gpointer data)
{
  struct xwidget* xw = (struct xwidget*) g_object_get_data (G_OBJECT (widget), XG_XWIDGET);
  struct xwidget_view* xv = (struct xwidget_view*) g_object_get_data (G_OBJECT (widget), XG_XWIDGET_VIEW);

  cairo_rectangle(cr, 0,0, xv->clip_right, xv->clip_bottom);//xw->width, xw->height);
  cairo_clip(cr);

  //
  if(xw->widgetscrolledwindow_osr !=  NULL)
    gtk_widget_draw (xw->widgetscrolledwindow_osr, cr);
  else
    gtk_widget_draw (xw->widget_osr, cr);
  return FALSE;
}

GtkWidget* xwgir_create_debug;



gboolean
xwidget_osr_event_forward (GtkWidget *widget,
                           GdkEvent  *event,
                           gpointer   user_data)
{
  /* copy events that arrive at the outer widget to the offscreen widget */
  struct xwidget* xw = (struct xwidget*) g_object_get_data (G_OBJECT (widget), XG_XWIDGET);
  GdkEvent* eventcopy =  gdk_event_copy(event);
  eventcopy->any.window = gtk_widget_get_window(xw->widget_osr);// works

  /* printf("xwidget_osr_event_forward redirect event to window:%d\n",   ((GdkEventAny*)eventcopy)->window); */
  /* printf("A type:%d x:%f y:%f \n",   event->type, event->button.x, event->button.y); */
  /* printf("B type:%d x:%f y:%f \n",   eventcopy->type, eventcopy->button.x, eventcopy->button.y); */
    //gtk_button_get_event_window(xwgir_create_debug);
  gtk_main_do_event(eventcopy); //TODO this will leak events. they should be deallocated later, perhaps in xwgir_event_callback
  return TRUE; //dont propagate this event furter
}

GIRepository *girepository ;

DEFUN ("xwgir-require-namespace", Fxwgir_require_namespace, Sxwgir_require_namespace, 2,2,0,
       doc: /* Require a GObject Introspection namespace.
               This must be done for all namespaces we want to use, before using other xwgir functions.*/)
  (Lisp_Object lnamespace, Lisp_Object lnamespace_version)
{
  char* namespace = SDATA(lnamespace);
  char* namespace_version = SDATA(lnamespace_version);
  GError *error = NULL;

  girepository = g_irepository_get_default();
  g_irepository_require(girepository, namespace, namespace_version, 0, &error);
  if (error) {
    g_error("ERROR: %s\n", error->message);
    return Qnil;
  }
  return Qt;
}

GtkWidget* xwgir_create(char* class, char* namespace){
  //TODO this is more or less the same as xwgir-call-method, so should be refactored
  //create a gtk widget, given its name
  //find the constructor
  //call it
  //also figure out how to pass args

  GError *error = NULL;
  GIArgument return_value;

  GIObjectInfo* obj_info = g_irepository_find_by_name(girepository, namespace, class);
  GIFunctionInfo* f_info = g_object_info_find_method (obj_info, "new");
  g_function_info_invoke(f_info,
                         NULL, 0,
                         NULL, 0,
                         &return_value,
                         NULL);
  xwgir_create_debug = return_value.v_pointer;
  return return_value.v_pointer;

}

int
xwgir_convert_lisp_to_gir_arg(GIArgument* giarg,
                              GIArgInfo* arginfo,
                              Lisp_Object lisparg )
{

  GITypeTag   tag;
  gboolean    is_pointer;
  gboolean    is_enum;
  tag =  g_type_info_get_tag (g_arg_info_get_type (arginfo));

  switch (tag)
    {
    case GI_TYPE_TAG_BOOLEAN:
      giarg->v_boolean = XFASTINT(lisparg);
      break;
    case GI_TYPE_TAG_INT8:
      giarg->v_int8 = XFASTINT(lisparg);
      break;
    case GI_TYPE_TAG_UINT8:
      giarg->v_uint8 = XFASTINT(lisparg);
      break;
    case GI_TYPE_TAG_INT16:
      giarg->v_int16 = XFASTINT(lisparg);
      break;
    case GI_TYPE_TAG_UINT16:
      giarg->v_uint16 = XFASTINT(lisparg);
      break;
    case GI_TYPE_TAG_INT32:
      giarg->v_int32 = XFASTINT(lisparg);
      break;
    case GI_TYPE_TAG_UINT32:
      giarg->v_uint32 = XFASTINT(lisparg);
      break;

    case GI_TYPE_TAG_INT64:
      giarg->v_int64 = XFASTINT(lisparg);
      break;
    case GI_TYPE_TAG_UINT64:
      giarg->v_uint64 = XFASTINT(lisparg);
      break;


    case GI_TYPE_TAG_FLOAT:
      giarg->v_float = XFLOAT_DATA(lisparg);
      break;

    case GI_TYPE_TAG_DOUBLE:
      giarg->v_double = XFLOAT_DATA(lisparg);
      break;

    case GI_TYPE_TAG_UTF8:
    case GI_TYPE_TAG_FILENAME:
      //giarg->v_string = SDATA(lisparg);
      giarg->v_pointer = SDATA(lisparg);
      break;

    case GI_TYPE_TAG_ARRAY:
    case GI_TYPE_TAG_GLIST:
    case GI_TYPE_TAG_GSLIST:
    case GI_TYPE_TAG_GHASH:
    case GI_TYPE_TAG_ERROR:
    case GI_TYPE_TAG_INTERFACE:
    case GI_TYPE_TAG_VOID:
    case GI_TYPE_TAG_UNICHAR:
    case GI_TYPE_TAG_GTYPE:
      //?? i dont know how to handle these yet TODO
      printf("failed in my lisp to gir arg conversion duties. sob!\n");
      return -1;
      break;
    }
  return 0;
}

#if 0
void
refactor_attempt(){
  //this methhod should be called from xwgir-xwidget-call-method and from xwgir xwidget construction
  char* class = SDATA(Fcar(Fcdr(Fget(xw->type, QCxwgir_class))));

  GIObjectInfo* obj_info = g_irepository_find_by_name(girepository, namespace, class);
  GIFunctionInfo* f_info = g_object_info_find_method (obj_info, SDATA(method));

  //loop over args, convert from lisp to primitive type, given arg introspection data
  //TODO g_callable_info_get_n_args(f_info) should match
  int argscount = XFASTINT(Flength(arguments));
  if(argscount !=  g_callable_info_get_n_args(f_info)){
    printf("xwgir call method arg count doesn match! \n");
    return Qnil;
  }
  int i;
  for (i = 1; i < argscount + 1; ++i)
    {
      xwgir_convert_lisp_to_gir_arg(&in_args[i], g_callable_info_get_arg(f_info, i - 1), Fnth(i - 1, arguments));
    }

  in_args[0].v_pointer = widget;
  if(g_function_info_invoke(f_info,
                            in_args, argscount + 1,
                            NULL, 0,
                            &return_value,
                            &error)) {
    //g_error("ERROR: %s\n", error->message);
    printf("invokation error\n");
     return Qnil;
   }
  return Qt;
}
#endif  /* 0 */

DEFUN ("xwgir-xwidget-call-method", Fxwgir_xwidget_call_method,  Sxwgir_xwidget_call_method,       3, 3, 0,
       doc:	/* Call Xwidget object method using GObject Introspection.
                   XWIDGET is the xwidget instance to act upon.
                    METHOD is the Gobject intrsopsection method name.
                    ARGUMENTS is a list of arguments for the call. They will be converted to GObject types from Lisp types.
                 */)
  (Lisp_Object xwidget, Lisp_Object method, Lisp_Object arguments)
{
  CHECK_XWIDGET (xwidget);
  GError *error = NULL;
  GIArgument return_value;
  GIArgument in_args[20];


  struct xwidget* xw;
  if (NILP (xwidget)) { printf("ERROR xwidget nil\n"); return Qnil; };
  xw = XXWIDGET(xwidget);
  if(NULL == xw) printf("ERROR xw is 0\n");
  char* namespace = SDATA(Fcar(Fget(xw->type, QCxwgir_class)));
  //we need the concrete widget, which happens in 2 ways depending on OSR or not TODO
  GtkWidget* widget = NULL;
  if(NULL == xw->widget_osr) {
    widget = xwidget_view_lookup (xw, XWINDOW(FRAME_SELECTED_WINDOW (SELECTED_FRAME ()))) -> widget;
  } else {
    widget = xw->widget_osr;
  }

  //char* class = SDATA(SYMBOL_NAME(xw->type)); //this works but is unflexible
  //figure out the class from the widget instead
  /* printf("type class: %s %s\n", G_OBJECT_TYPE_NAME(widget), G_OBJECT_CLASS_NAME(G_OBJECT_GET_CLASS(widget))); */
  /* char* class = G_OBJECT_TYPE_NAME(widget); //gives "GtkButton"(I want "Button") */
  /* class += strlen(namespace);  //TODO check for corresponding api method. but this seems to work. */

  char* class = SDATA(Fcar(Fcdr(Fget(xw->type, QCxwgir_class))));

  GIObjectInfo* obj_info = g_irepository_find_by_name(girepository, namespace, class);
  GIFunctionInfo* f_info = g_object_info_find_method (obj_info, SDATA(method));

  //loop over args, convert from lisp to primitive type, given arg introspection data
  //TODO g_callable_info_get_n_args(f_info) should match
  int argscount = XFASTINT(Flength(arguments));
  if(argscount !=  g_callable_info_get_n_args(f_info)){
    printf("xwgir call method arg count doesn match! \n");
    return Qnil;
  }
  int i;
  Lisp_Object n;
  for (i = 1; i < argscount + 1; ++i)
    {
        XSETFASTINT (n, i - 1);
        xwgir_convert_lisp_to_gir_arg(&in_args[i], g_callable_info_get_arg(f_info, i - 1), Fnth(n, arguments));
    }

  in_args[0].v_pointer = widget;
  if(g_function_info_invoke(f_info,
                            in_args, argscount + 1,
                            NULL, 0,
                            &return_value,
                            &error)) {
    //g_error("ERROR: %s\n", error->message);
    printf("invokation error\n");
     return Qnil;
   }
  return Qt;
}

 void
to_child (GtkWidget *bin,
          double         widget_x,
          double         widget_y,
          double        *x_out,
          double        *y_out)
{
  *x_out = widget_x;
  *y_out = widget_y;
}


GdkWindow *
offscreen_pick_embedded_child (GdkWindow *window,
                               double x,
                               double y,
                               gpointer *data)
{
  //in this simple case we assume the window contains a single widget. easy.
  //but then we get the problem that the widget cant be embedded in several windows
  return gtk_widget_get_window (GTK_WIDGET (data));
}

void
offscreen_to_embedder (GdkWindow *window,
                       gdouble offscreen_x,
                       gdouble offscreen_y,
                       gpointer embedder_x,
                       gpointer embedder_y,
                       gpointer data)
{
  * (gdouble *) embedder_x = offscreen_x;
  * (gdouble *) embedder_y = offscreen_y;
}

void
offscreen_from_embedder (GdkWindow *window,
                         gdouble embedder_x,
                         gdouble embedder_y,
                         gpointer offscreen_x,
                         gpointer offscreen_y,
                         gpointer user_data)
{
  * (gdouble *) offscreen_x = embedder_x;
  * (gdouble *) offscreen_y = embedder_y;
}

gboolean
xwidget_osr_event_set_embedder (GtkWidget *widget,
                                GdkEvent *event,
                                gpointer data)
{
  struct xwidget_view *xv = (struct xwidget_view *) data;
  struct xwidget *xww = XXWIDGET (xv->model);
  printf("gdk_offscreen_window_set_embedder %d %d\n",
         GDK_IS_WINDOW(gtk_widget_get_window (xww->widget_osr)),
         GDK_IS_WINDOW(gtk_widget_get_window (GTK_WIDGET (xv->widget))));
  gdk_offscreen_window_set_embedder (gtk_widget_get_window (xww->widgetwindow_osr),
                                     gtk_widget_get_window (xv->widget));
}


/* initializes and does initial placement of an xwidget view on screen */
struct xwidget_view*
xwidget_init_view (struct xwidget *xww,
                   struct glyph_string *s,
                   int x, int y)
{
  struct xwidget_view *xv = allocate_xwidget_view();
  Lisp_Object val;
  GdkColor color;

  XSETXWIDGET_VIEW (val, xv)  ;
  Vxwidget_view_list = Fcons (val, Vxwidget_view_list);

  XSETWINDOW(xv->w, s->w);
  XSETXWIDGET(xv->model, xww);

  //widget creation
  if(EQ(xww->type, Qbutton))
    {
      xv->widget = gtk_button_new_with_label (XSTRING(xww->title)->data);
      g_signal_connect (G_OBJECT (xv->widget), "clicked",
                        G_CALLBACK (buttonclick_handler), xv); // the view rather than the model
    } else if (EQ(xww->type, Qtoggle)) {
    xv->widget = gtk_toggle_button_new_with_label (XSTRING(xww->title)->data);
    //xv->widget = gtk_entry_new ();//temp hack to experiment with key propagation TODO entry widget is useful for testing
  } else if (EQ(xww->type, Qsocket)) {
    xv->widget = gtk_socket_new ();
    g_signal_connect_after(xv->widget, "plug-added", G_CALLBACK(xwidget_plug_added), "plug added");
    g_signal_connect_after(xv->widget, "plug-removed", G_CALLBACK(xwidget_plug_removed), "plug removed");
    //TODO these doesnt help
    gtk_widget_add_events(xv->widget, GDK_KEY_PRESS);
    gtk_widget_add_events(xv->widget, GDK_KEY_RELEASE);
  } else if (EQ(xww->type, Qslider)) {
    xv->widget =
      //gtk_hscale_new (GTK_ADJUSTMENT(gtk_adjustment_new (0.0, 0.0, 100.0, 1.0, 10.0, 10.0)));
      gtk_hscale_new_with_range ( 0.0, 100.0, 10.0);
    gtk_scale_set_draw_value (GTK_SCALE (xv->widget), FALSE);	//i think its emacs role to show text and stuff, so disable the widgets own text
    xv->handler_id = g_signal_connect_after(xv->widget, "value-changed", G_CALLBACK(xwidget_slider_changed), "slider changed");
  } else if (EQ(xww->type, Qcairo)) {
    //Cairo view
    //uhm cairo is differentish in gtk 3.
    //gdk_cairo_create (gtk_widget_get_window (FRAME_GTK_WIDGET (s->f)));
    xv->widget = gtk_drawing_area_new();
    g_signal_connect (G_OBJECT (    xv->widget), "draw",
                      G_CALLBACK (xwidget_osr_draw_callback), NULL);

  } else if (EQ(xww->type, Qwebkit_osr)||
             EQ(xww->type, Qsocket_osr)||
             (!NILP (Fget(xww->type, QCxwgir_class))))//xwgir widgets are OSR
  {
    printf("osr init:%s\n",SDATA(SYMBOL_NAME(xww->type)));
    xv->widget = gtk_drawing_area_new();
    gtk_widget_set_app_paintable ( xv->widget, TRUE); //because expose event handling
    gtk_widget_add_events(xv->widget, GDK_ALL_EVENTS_MASK);

    /* Draw the view on damage-event */
    g_signal_connect (G_OBJECT (xww->widgetwindow_osr), "damage-event",
                      G_CALLBACK (offscreen_damage_event), xv->widget);

    if (EQ(xww->type, Qwebkit_osr)){
      /* ///xwgir debug */
      /* //forward events. this isnt compatible with the set_embedded strategy */
      g_signal_connect (G_OBJECT (    xv->widget), "button-press-event",
                        G_CALLBACK (xwidget_osr_event_forward), NULL);
      g_signal_connect (G_OBJECT (    xv->widget), "button-release-event",
                        G_CALLBACK (xwidget_osr_event_forward), NULL);
      g_signal_connect (G_OBJECT (    xv->widget), "motion-notify-event",
                        G_CALLBACK (xwidget_osr_event_forward), NULL);
    }else{
      //xwgir debug , orthogonal to forwarding
      g_signal_connect (G_OBJECT (xv->widget), "enter-notify-event",
                        G_CALLBACK (xwidget_osr_event_set_embedder), xv);
    }

    //draw
    g_signal_connect (G_OBJECT (xv->widget), "draw",
                      G_CALLBACK (xwidget_osr_draw_callback), NULL);

  }
  //else return NULL;

  //widget realization
  //make container widget 1st, and put the actual widget inside the container
  //later, drawing should crop container window if necessary to handle case where xwidget
  //is partially obscured by other emacs windows
  //other containers than gtk_fixed where explored, but gtk_fixed had the most predictable behaviour so far.
  xv->emacswindow = FRAME_GTK_WIDGET (s->f);
  xv->widgetwindow = gtk_fixed_new ();
  gtk_widget_set_has_window(xv->widgetwindow, TRUE);
  gtk_container_add (GTK_CONTAINER (xv->widgetwindow), xv->widget);

  //store some xwidget data in the gtk widgets
  g_object_set_data (G_OBJECT (xv->widget), XG_FRAME_DATA, (gpointer) (s->f)); //the emacs frame
  g_object_set_data (G_OBJECT (xv->widget), XG_XWIDGET, (gpointer) (xww)); //the xwidget
  g_object_set_data (G_OBJECT (xv->widget), XG_XWIDGET_VIEW, (gpointer) (xv)); //the xwidget
  g_object_set_data (G_OBJECT (xv->widgetwindow), XG_XWIDGET, (gpointer) (xww)); //the xwidget window
  g_object_set_data (G_OBJECT (xv->widgetwindow), XG_XWIDGET_VIEW, (gpointer) (xv)); //the xwidget window


  gtk_widget_set_size_request (GTK_WIDGET (xv->widget), xww->width, xww->height);
  gtk_widget_set_size_request (xv->widgetwindow, xww->width, xww->height);
  gtk_fixed_put (GTK_FIXED (FRAME_GTK_WIDGET (s->f)), xv->widgetwindow, x, y);
  xv->x = x;  xv->y = y;
  gtk_widget_show_all (xv->widgetwindow);



  //widgettype specific initialization only possible after realization
  if (EQ(xww->type, Qsocket)) {
    printf ("xwid:%d socket id:%x %d\n",
            xww,
            gtk_socket_get_id (GTK_SOCKET (xv->widget)),
            gtk_socket_get_id (GTK_SOCKET (xv->widget)));
    send_xembed_ready_event (xww,
                             gtk_socket_get_id (GTK_SOCKET (xv->widget)));
    //gtk_widget_realize(xw->widget);
  }

  //////////////////////////////////////////////////////////////
  // xwgir debug
  if (//EQ(xww->type, Qwebkit_osr)|| //TODO should be able to choose compile time which method to use with webkit
      EQ(xww->type, Qsocket_osr)||
      (!NILP (Fget(xww->type, QCxwgir_class))))//xwgir widgets are OSR
    {
      printf("gdk_offscreen_window_set_embedder %d %d\n",
             GDK_IS_WINDOW(gtk_widget_get_window (xww->widget_osr)),
             GDK_IS_WINDOW(gtk_widget_get_window (GTK_WIDGET (xv->widget))));
      // set_embedder needs to be called after xv->widget realization
      gdk_offscreen_window_set_embedder (gtk_widget_get_window (xww->widgetwindow_osr),
                                         gtk_widget_get_window (xv->widget));
      g_signal_connect (gtk_widget_get_window (xv->widget), "pick-embedded-child",
                        G_CALLBACK (offscreen_pick_embedded_child), xww->widgetwindow_osr);

      g_signal_connect (gtk_widget_get_window (xww->widgetwindow_osr), "from-embedder",
                        G_CALLBACK (offscreen_from_embedder), NULL);
      g_signal_connect (gtk_widget_get_window (xww->widgetwindow_osr), "to-embedder",
                        G_CALLBACK (offscreen_to_embedder), NULL);
    }
  ////////////////////////////////////////

  return xv;
}


void
x_draw_xwidget_glyph_string (struct glyph_string *s)
{
  /*
    this method is called by the redisplay engine and places the xwidget on screen.
    moving and clipping is done here. also view init.

  */
  int box_line_hwidth = eabs (s->face->box_line_width);
  int box_line_vwidth = max (s->face->box_line_width, 0);
  int height = s->height;
  struct xwidget *xww = s->xwidget;
  struct xwidget_view *xv = xwidget_view_lookup(xww, s->w);
  int clip_right; int clip_bottom; int clip_top; int clip_left;

  int x = s->x;
  int y = s->y + (s->height / 2) - (xww->height / 2);
  int moved=0;

  /* We do it here in the display loop because there is no other
     time to know things like window placement etc.
  */
  printf ("xv init for xw %d\n", xww);
  xv = xwidget_init_view (xww, s, x, y);

  //calculate clipping, which is used for all manner of onscreen xwidget views
  //each widget border can get clipped by other emacs objects so there are four clipping variables
  clip_right = min (xww->width, WINDOW_RIGHT_EDGE_X (s->w) - x - WINDOW_RIGHT_SCROLL_BAR_AREA_WIDTH(s->w) - WINDOW_RIGHT_FRINGE_WIDTH(s->w));
  clip_left = max (0, WINDOW_LEFT_EDGE_X (s->w) - x + WINDOW_LEFT_SCROLL_BAR_AREA_WIDTH(s->w) + WINDOW_LEFT_FRINGE_WIDTH(s->w));

  clip_bottom = min (xww->height, WINDOW_BOTTOM_EDGE_Y (s->w) - WINDOW_MODE_LINE_HEIGHT (s->w) - y);
  clip_top = max(0, WINDOW_TOP_EDGE_Y(s->w) -y );

  //we are conserned with movement of the onscreen area. the area might sit still when the widget actually moves
  //this happens when an emacs window border moves across a widget window
  //so, if any corner of the outer widget clippng window moves, that counts as movement here, even
  //if it looks like no movement happens because the widget sits still inside the clipping area.
  //the widget can also move inside the clipping area, which happens later
  moved = (xv->x  + xv->clip_left != x+clip_left)
    || ((xv->y + xv->clip_top)!= (y+clip_top));
  xv->x = x;
  xv->y = y;
  if (moved)	//has it moved?
    {
      if (1)//!xwidget_hidden(xv))	//hidden equals not being seen during redisplay
        {
          //TODO should be possible to use xwidget_show_view here
          gtk_fixed_move (GTK_FIXED (FRAME_GTK_WIDGET (s->f)),
                          xv->widgetwindow,
                          x + clip_left, y + clip_top);
        }
    }
  //clip the widget window if some parts happen to be outside drawable area
  //an emacs window is not a gtk window, a gtk window covers the entire frame
  //cliping might have changed even if we havent actualy moved, we try figure out when we need to reclip for real
  if((xv->clip_right != clip_right)
     || (xv->clip_bottom != clip_bottom)
     || (xv->clip_top != clip_top)
     || (xv->clip_left != clip_left)){
    gtk_widget_set_size_request (xv->widgetwindow,  clip_right + clip_left, clip_bottom + clip_top);
    gtk_fixed_move(GTK_FIXED(xv->widgetwindow), xv->widget, -clip_left, -clip_top);

    xv->clip_right = clip_right; xv->clip_bottom = clip_bottom; xv->clip_top = clip_top;xv->clip_left = clip_left;
  }
  //if emacs wants to repaint the area where the widget lives, queue a redraw
  //TODO it seems its possible to get out of sync with emacs redraws so emacs bg sometimes shows up instead of xwidget
  //its just a visual glitch though
  if (!xwidget_hidden(xv)){
    gtk_widget_queue_draw (xv->widgetwindow);
    gtk_widget_queue_draw (xv->widget);
  }
}


#ifdef HAVE_WEBKIT_OSR

//FUGLY macro that checks WEBKIT_IS_WEB_VIEW(xw->widget_osr) first
#define WEBKIT_FN_INIT()                        \
  struct xwidget* xw; \
  CHECK_XWIDGET (xwidget); \
 if(NILP (xwidget)) {printf("ERROR xwidget nil\n"); return Qnil;};    \
  xw = XXWIDGET(xwidget);                                                    \
  if(NULL == xw) printf("ERROR xw is 0\n");                               \
  if((NULL == xw->widget_osr) || !WEBKIT_IS_WEB_VIEW(xw->widget_osr)){  \
    printf("ERROR xw->widget_osr does not hold a webkit instance\n");\
    return Qnil;\
  };


DEFUN ("xwidget-webkit-goto-uri", Fxwidget_webkit_goto_uri,  Sxwidget_webkit_goto_uri,
       2, 2, 0,
       doc:	/* Make the webkit instance referenced by XWIDGET browse URI. */)
  (Lisp_Object xwidget, Lisp_Object uri)
{
  WEBKIT_FN_INIT();
  CHECK_STRING(uri);
  webkit_web_view_load_uri ( WEBKIT_WEB_VIEW(xw->widget_osr), SDATA(uri));
  return Qnil;
}


DEFUN ("xwidget-webkit-execute-script", Fxwidget_webkit_execute_script,  Sxwidget_webkit_execute_script,
       2, 2, 0,
       doc:	/* webkit exec js.*/)
  (Lisp_Object xwidget, Lisp_Object script)
{
  WEBKIT_FN_INIT();
  CHECK_STRING(script);
  webkit_web_view_execute_script( WEBKIT_WEB_VIEW(xw->widget_osr), SDATA(script));
  return Qnil;
}

DEFUN ("xwidget-webkit-get-title", Fxwidget_webkit_get_title,  Sxwidget_webkit_get_title,
       1, 1, 0,
       doc:	/* Get the title from the Webkit instance in XWIDGET.
                   This can be used to work around the lack of a return value from the exec method.
                */)
  (Lisp_Object xwidget)
{
  //TODO support multibyte strings
  WEBKIT_FN_INIT();
  const gchar* str=webkit_web_view_get_title( WEBKIT_WEB_VIEW(xw->widget_osr));
  //return make_string_from_bytes(str, wcslen((const wchar_t *)str), strlen(str));
  if(str == 0){
    //TODO maybe return Qnil instead. I suppose webkit returns nullpointer when doc is not properly loaded or something
    printf("xwidget-webkit-get-title null webkit title\n");
    return build_string("");
  }
  return build_string(str);
}

//TODO missnamed
DEFUN ("xwidget-disable-plugin-for-mime", Fxwidget_disable_plugin_for_mime , Sxwidget_disable_plugin_for_mime,
      1,1,0, doc: /* */)
  (Lisp_Object mime)
{
  WebKitWebPlugin *wp = webkit_web_plugin_database_get_plugin_for_mimetype
    (webkit_get_web_plugin_database(),  SDATA(mime));
  if(wp == NULL) return Qnil;
  if(webkit_web_plugin_get_enabled (wp)){
    webkit_web_plugin_set_enabled  (wp, FALSE);
    return Qt;
  }
  return Qnil;
}


void
xwidget_webkit_dom_dump(WebKitDOMNode* parent)
{
  WebKitDOMNodeList* list;
  int i;
  int length;
  WebKitDOMNode* attribute;
  WebKitDOMNamedNodeMap* attrs;
  WebKitDOMNode* child;
  printf("node:%d type:%d name:%s content:%s\n",
         parent,
         webkit_dom_node_get_node_type(parent),//1 element 3 text 8 comment 2 attribute
         webkit_dom_node_get_local_name(parent),
         webkit_dom_node_get_text_content(parent));

  if(webkit_dom_node_has_attributes(parent)){
    attrs = webkit_dom_node_get_attributes(parent);

    length = webkit_dom_named_node_map_get_length(attrs);
    for (int i = 0; i < length; i++) {
      attribute = webkit_dom_named_node_map_item(attrs,i);
      printf(" attr node:%d type:%d name:%s content:%s\n",
             attribute,
             webkit_dom_node_get_node_type(attribute),//1 element 3 text 8 comment
             webkit_dom_node_get_local_name(attribute),
             webkit_dom_node_get_text_content(attribute));
    }
  }
  list = webkit_dom_node_get_child_nodes(parent);
  length = webkit_dom_node_list_get_length(list);

  for (int i = 0; i < length; i++) {
    child = webkit_dom_node_list_item(list, i);
    //if(webkit_dom_node_has_child_nodes(child))
    xwidget_webkit_dom_dump(child);
  }
}


DEFUN ("xwidget-webkit-dom-dump", Fxwidget_webkit_dom_dump,  Sxwidget_webkit_dom_dump,
       1, 1, 0,
       doc:	/*Dump the DOM contained in the webkit instance in XWIDGET.*/)
  (Lisp_Object xwidget)
{
  WEBKIT_FN_INIT();
  xwidget_webkit_dom_dump(WEBKIT_DOM_NODE(webkit_web_view_get_dom_document( WEBKIT_WEB_VIEW(xw->widget_osr))));
  return Qnil;
}



#endif  /* HAVE_WEBKIT_OSR */



DEFUN ("xwidget-resize", Fxwidget_resize, Sxwidget_resize, 3, 3, 0, doc:
       /* Resize XWIDGET.
          NEW_WIDTH NEW_HEIGHT defines the new size.)
        */)
  (Lisp_Object xwidget, Lisp_Object new_width, Lisp_Object new_height)
{
  CHECK_XWIDGET (xwidget);
  struct xwidget* xw = XXWIDGET(xwidget);
  struct xwidget_view *xv;
  int  w, h;

  CHECK_NUMBER (new_width);
  CHECK_NUMBER (new_height);
  w = XFASTINT (new_width);
  h = XFASTINT (new_height);


  printf("resize xwidget %d (%d,%d)->(%d,%d)\n",xw, xw->width,xw->height,w,h);
  xw->width=w;
  xw->height=h;
  //if theres a osr resize it 1st
  if(xw->widget_osr){
    printf("resize xwidget_osr\n");
    //gtk_container_set_resize_mode ( GTK_WINDOW(xw->widgetwindow_osr), GTK_RESIZE_QUEUE);
    //gtk_container_set_resize_mode ( GTK_WINDOW(xw->widget_osr), GTK_RESIZE_QUEUE);


    //gtk_layout_set_size (GTK_LAYOUT (xw->widgetwindow_osr), xw->width, xw->height);
    gtk_widget_set_size_request (GTK_WIDGET (xw->widget_osr), xw->width, xw->height); //minimum size
    //gtk_window_resize(    GTK_WINDOW(xw->widget_osr), xw->width, xw->height);
    gtk_window_resize(    GTK_WINDOW(xw->widgetwindow_osr), xw->width, xw->height);
    gtk_window_resize(    GTK_WINDOW(xw->widgetscrolledwindow_osr), xw->width, xw->height);
    gtk_scrolled_window_set_min_content_height(GTK_SCROLLED_WINDOW(xw->widgetscrolledwindow_osr ),xw->height);
    gtk_scrolled_window_set_min_content_width(GTK_SCROLLED_WINDOW(xw->widgetscrolledwindow_osr ),xw->width);

    //gtk_container_resize_children ( GTK_WINDOW(xw->widgetwindow_osr));
    gtk_container_resize_children (GTK_CONTAINER(xw->widgetwindow_osr));

  }

  for (Lisp_Object tail = Vxwidget_view_list; CONSP (tail); tail = XCDR (tail)) //TODO MVC refactor lazy linear search
    {
      if (XWIDGET_VIEW_P (XCAR (tail))) {
        xv = XXWIDGET_VIEW (XCAR (tail));
        if(XXWIDGET (xv->model) == xw) {
          gtk_layout_set_size (GTK_LAYOUT (xv->widgetwindow), xw->width, xw->height);
          gtk_widget_set_size_request (GTK_WIDGET (xv->widget), xw->width, xw->height);
        }
      }
    }

  return Qnil;
}



DEFUN ("xwidget-set-adjustment", Fxwidget_set_adjustment, Sxwidget_set_adjustment, 4, 4, 0, doc:
       /* set scrolling  */)
  (Lisp_Object xwidget, Lisp_Object axis, Lisp_Object relative, Lisp_Object value)
{
  CHECK_XWIDGET (xwidget);
  struct xwidget* xw = XXWIDGET(xwidget);
  GtkAdjustment* adjustment;
  float final_value=0.0;

  if(EQ(Qvertical, axis)){
    adjustment = gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(xw->widgetscrolledwindow_osr));
  }
  if(EQ(Qhorizontal, axis)){
    adjustment = gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(xw->widgetscrolledwindow_osr));
  }

  if(EQ(Qt, relative)){
    final_value=gtk_adjustment_get_value(adjustment)+XFASTINT(value);
  }else{
     final_value=0.0+XFASTINT(value);
  }

  gtk_adjustment_set_value(adjustment, final_value);

  return Qnil;
}


DEFUN ("xwidget-size-request", Fxwidget_size_request, Sxwidget_size_request, 1, 1, 0, doc:
       /* Desired size of the XWIDGET.

  This can be used to read the xwidget desired size,  and resizes the Emacs allocated area accordingly.

(TODO crashes if arg not osr widget)*/)
  (Lisp_Object xwidget)
{
  CHECK_XWIDGET (xwidget);
  GtkRequisition requisition;
  Lisp_Object rv;
  gtk_widget_size_request(XXWIDGET(xwidget)->widget_osr, &requisition);
  rv = Qnil;
  rv = Fcons (make_number(requisition.height), rv);
  rv = Fcons (make_number(requisition.width), rv);
  return rv;

}

DEFUN ("xwidgetp", Fxwidgetp, Sxwidgetp, 1, 1, 0,
       doc: /* Return t if OBJECT is a xwidget.  */)
  (Lisp_Object object)
{
  return XWIDGETP (object) ? Qt : Qnil;
}

DEFUN ("xwidget-view-p", Fxwidget_view_p, Sxwidget_view_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a xwidget-view.  */)
  (Lisp_Object object)
{
  return XWIDGET_VIEW_P (object) ? Qt : Qnil;
}

DEFUN ("xwidget-info", Fxwidget_info , Sxwidget_info, 1,1,0,
       doc: /* Get XWIDGET properties.
             Currently type, title, width, height.*/)
  (Lisp_Object xwidget)
{
  CHECK_XWIDGET (xwidget);
  Lisp_Object info, n;
  struct xwidget* xw = XXWIDGET(xwidget);

  info = Fmake_vector (make_number (4), Qnil);
  ASET (info, 0, xw->type);
  ASET (info, 1, xw->title);
  XSETFASTINT(n, xw->width);
  ASET (info, 2, n);
  XSETFASTINT(n, xw->height);
  ASET (info, 3, n);

  return info;
}

DEFUN ("xwidget-view-info", Fxwidget_view_info , Sxwidget_view_info, 1, 1, 0, doc:
       /* Get XWIDGET-VIEW properties.
        Currently x,y clip right, clip bottom, clip top, clip left*/)
  (Lisp_Object xwidget_view)
{
  CHECK_XWIDGET_VIEW (xwidget_view);
  struct xwidget_view *xv = XXWIDGET_VIEW (xwidget_view);
  Lisp_Object info;

  info = Fmake_vector (make_number (6), Qnil);
  ASET (info, 0,  make_number(xv->x));
  ASET (info, 1,  make_number(xv->y));
  ASET (info, 2,  make_number(xv->clip_right));
  ASET (info, 3,  make_number(xv->clip_bottom));
  ASET (info, 4,  make_number(xv->clip_top));
  ASET (info, 5,  make_number(xv->clip_left));

  return info;
}

DEFUN ("xwidget-view-model", Fxwidget_view_model, Sxwidget_view_model,
       1, 1, 0,
       doc: /* Get XWIDGET-VIEW model. */)
  (Lisp_Object xwidget_view)
{
  CHECK_XWIDGET_VIEW (xwidget_view);
  return XXWIDGET_VIEW (xwidget_view)->model;
}

DEFUN ("xwidget-view-window", Fxwidget_view_window, Sxwidget_view_window,
       1, 1, 0,
       doc: /* Get XWIDGET-VIEW window. */)
  (Lisp_Object xwidget_view)
{
  CHECK_XWIDGET_VIEW (xwidget_view);
  return XXWIDGET_VIEW (xwidget_view)->w;
}

DEFUN ("xwidget-send-keyboard-event", Fxwidget_send_keyboard_event, Sxwidget_send_keyboard_event, 2, 2, 0,
       doc:/* Synthesize a kbd event for  XWIDGET. TODO crashes atm.. */
       )
  (Lisp_Object  xwidget, Lisp_Object keydescriptor)
{
  //TODO this code crashes for offscreen widgets and ive tried many different strategies
  //int keyval = 0x058; //X
  int keyval = XFASTINT(keydescriptor); //X
  char *keystring = "";
  GdkKeymapKey* keys;
  gint n_keys;
  GdkDeviceManager* manager;
  struct xwidget *xw;
  GtkWidget* widget;
  GdkEventKey* ev;
  Lisp_Object window;
  //popup_activated_flag = 1; //TODO just a hack
  gdk_keymap_get_entries_for_keyval(gdk_keymap_get_default(), keyval, &keys, &n_keys);

  xw = XXWIDGET(xwidget);

  ev = (GdkEventKey*)gdk_event_new(GDK_KEY_PRESS);


  //todo what about windowless widgets?

  window = FRAME_SELECTED_WINDOW (SELECTED_FRAME ());


  //TODO maybe we also need to special case sockets by picking up the plug rather than the socket
  if(xw->widget_osr)
    widget = xw->widget_osr;
  else
    widget = xwidget_view_lookup(xw, XWINDOW(window))->widget;

  ev->window = gtk_widget_get_window(widget);
  gtk_widget_grab_focus(widget);
  ev->send_event = FALSE;

  ev->hardware_keycode = keys[0].keycode;
  ev->group = keys[0].group;

  ev->keyval = keyval;
  ev->time = GDK_CURRENT_TIME;

  //ev->device = gdk_device_get_core_pointer();
  manager = gdk_display_get_device_manager(gdk_window_get_display(ev->window));
  gdk_event_set_device ((GdkEvent*)ev,   gdk_device_manager_get_client_pointer(manager));
  gdk_event_put((GdkEvent*)ev);
  //g_signal_emit_by_name(ev->window,"key-press-event", ev);

  ev->type = GDK_KEY_RELEASE;
  gdk_event_put((GdkEvent*)ev);
  //g_signal_emit_by_name(ev->window,"key-release-event", ev);
  //gtk_main_do_event(ev);

  //TODO
  //if I delete the event the receiving component eventually crashes.
  //it ough TDTRT since event_put is supposed to copy the event
  //so probably this leaks events now
  //gdk_event_free((GdkEvent*)ev);

  return Qnil;
}

DEFUN ("delete-xwidget-view", Fdelete_xwidget_view, Sdelete_xwidget_view,
       1, 1, 0,
       doc: /* Delete the XWIDGET-VIEW. */)
  (Lisp_Object xwidget_view)
{
  CHECK_XWIDGET_VIEW (xwidget_view);
  struct xwidget_view *xv = XXWIDGET_VIEW (xwidget_view);
  gtk_widget_destroy(xv->widgetwindow);
  Vxwidget_view_list = Fdelq (xwidget_view, Vxwidget_view_list);
}

DEFUN ("xwidget-view-lookup", Fxwidget_view_lookup, Sxwidget_view_lookup,
       1, 2, 0,
       doc: /* Return the xwidget-view associated to XWIDGET in
WINDOW if specified, otherwise it uses the selected window. */)
  (Lisp_Object xwidget, Lisp_Object window)
{
  CHECK_XWIDGET (xwidget);

  if (NILP (window))
    window = Fselected_window();
  CHECK_WINDOW (window);

  for (Lisp_Object tail = Vxwidget_view_list; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object xwidget_view = XCAR (tail);
      if (EQ (Fxwidget_view_model (xwidget_view), xwidget)
          && EQ (Fxwidget_view_window (xwidget_view), window))
        return xwidget_view;
    }

  return Qnil;
}

DEFUN ("set-frame-visible", Fset_frame_visible, Sset_frame_visible,
       2, 2, 0,
       doc: /* HACKY */)
  (Lisp_Object frame, Lisp_Object flag)
{
  CHECK_FRAME (frame);
  struct frame *f = XFRAME (frame);
  SET_FRAME_VISIBLE (f, !NILP (flag));
  return flag;
}

DEFUN ("xwidget-plist", Fxwidget_plist, Sxwidget_plist,
       1, 1, 0,
       doc: /* Return the plist of XWIDGET.  */)
  (register Lisp_Object xwidget)
{
  CHECK_XWIDGET (xwidget);
  return XXWIDGET (xwidget)->plist;
}

DEFUN ("xwidget-buffer", Fxwidget_buffer, Sxwidget_buffer,
       1, 1, 0,
       doc: /* Return the buffer of XWIDGET.  */)
  (register Lisp_Object xwidget)
{
  CHECK_XWIDGET (xwidget);
  return XXWIDGET (xwidget)->buffer;
}

DEFUN ("set-xwidget-plist", Fset_xwidget_plist, Sset_xwidget_plist,
       2, 2, 0,
       doc: /* Replace the plist of XWIDGET with PLIST.  Returns PLIST.  */)
  (register Lisp_Object xwidget, Lisp_Object plist)
{
  CHECK_XWIDGET (xwidget);
  CHECK_LIST (plist);

  XXWIDGET (xwidget)->plist = plist;
  return plist;
}

DEFUN ("set-xwidget-query-on-exit-flag",
       Fset_xwidget_query_on_exit_flag, Sset_xwidget_query_on_exit_flag,
       2, 2, 0,
       doc: /* Specify if query is needed for XWIDGET when Emacs is
exited.  If the second argument FLAG is non-nil, Emacs will query the
user before exiting or killing a buffer if XWIDGET is running.  This
function returns FLAG. */)
  (Lisp_Object xwidget, Lisp_Object flag)
{
  CHECK_XWIDGET (xwidget);
  XXWIDGET (xwidget)->kill_without_query = NILP (flag);
  return flag;
}

DEFUN ("xwidget-query-on-exit-flag",
       Fxwidget_query_on_exit_flag, Sxwidget_query_on_exit_flag,
       1, 1, 0,
       doc: /* Return the current value of query-on-exit flag for XWIDGET. */)
  (Lisp_Object xwidget)
{
  CHECK_XWIDGET (xwidget);
  return (XXWIDGET (xwidget)->kill_without_query ? Qnil : Qt);
}

void
syms_of_xwidget (void)
{
  int i;

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
  defsubr (&Sset_frame_visible);

  #ifdef HAVE_WEBKIT_OSR
  defsubr (&Sxwidget_webkit_goto_uri);
  defsubr (&Sxwidget_webkit_execute_script);
  defsubr (&Sxwidget_webkit_get_title);
  DEFSYM (Qwebkit_osr, "webkit-osr");
  #endif

  defsubr (&Sxwgir_xwidget_call_method  );
  defsubr (&Sxwgir_require_namespace);
  defsubr (&Sxwidget_size_request  );
  defsubr (&Sdelete_xwidget_view);
  defsubr (&Sxwidget_disable_plugin_for_mime);

  defsubr (&Sxwidget_send_keyboard_event);
  defsubr (&Sxwidget_webkit_dom_dump);
  defsubr (&Sxwidget_plist);
  defsubr (&Sxwidget_buffer);
  defsubr (&Sset_xwidget_plist);

  defsubr (&Sxwidget_set_adjustment);

  DEFSYM (Qxwidget, "xwidget");

  DEFSYM (QCxwidget, ":xwidget");
  DEFSYM (QCxwgir_class, ":xwgir-class");
  DEFSYM (QCtitle, ":title");

  /* Do not forget to update the docstring of make-xwidget if you add
     new types. */
  DEFSYM (Qbutton, "Button"); //changed to match the gtk class because xwgir(experimental and not really needed)
  DEFSYM (Qtoggle, "ToggleButton");
  DEFSYM (Qslider, "slider");
  DEFSYM (Qsocket, "socket");
  DEFSYM (Qsocket_osr, "socket-osr");
  DEFSYM (Qcairo, "cairo");

  DEFSYM (Qvertical, "vertical");
  DEFSYM (Qhorizontal, "horizontal");

  DEFSYM (QCplist, ":plist");

  DEFVAR_LISP ("xwidget-list", Vxwidget_list, doc: /*xwidgets list*/);
  Vxwidget_list = Qnil;

  DEFVAR_LISP ("xwidget-view-list", Vxwidget_view_list, doc: /*xwidget views list*/);
  Vxwidget_view_list = Qnil;

  Fprovide (intern ("xwidget-internal"), Qnil);

}


/* Value is non-zero if OBJECT is a valid Lisp xwidget specification.  A
   valid xwidget specification is a list whose car is the symbol
   `xwidget', and whose rest is a property list.  The property list must
   contain a value for key `:type'.  That value must be the name of a
   supported xwidget type.  The rest of the property list depends on the
   xwidget type.  */

int
valid_xwidget_spec_p (Lisp_Object object)
{
  int valid_p = 0;

  if (CONSP (object) && EQ (XCAR (object), Qxwidget))
    {
      /* Lisp_Object tem; */

      /* for (tem = XCDR (object); CONSP (tem); tem = XCDR (tem)) */
      /*   if (EQ (XCAR (tem), QCtype)) */
      /*     { */
      /*       tem = XCDR (tem); */
      /*       if (CONSP (tem) && SYMBOLP (XCAR (tem))) */
      /*         { */
      /*        struct xwidget_type *type; */
      /*        type = lookup_xwidget_type (XCAR (tem)); */
      /*        if (type) */
      /*          valid_p = type->valid_p (object); */
      /*         } */

      /*       break; */
      /*     } */
      //never mind type support for now
      valid_p = 1;
    }

  return valid_p;
}



/* find a value associated with key in spec */
Lisp_Object
xwidget_spec_value ( Lisp_Object spec, Lisp_Object  key,
                     int *found)
{
  Lisp_Object tail;

  eassert (valid_xwidget_spec_p (spec));

  for (tail = XCDR (spec);
       CONSP (tail) && CONSP (XCDR (tail)); tail = XCDR (XCDR (tail)))
    {
      if (EQ (XCAR (tail), key))
        {
          if (found)
            *found = 1;
          return XCAR (XCDR (tail));
        }
    }

  if (found)
    *found = 0;
  return Qnil;
}


void
xwidget_view_delete_all_in_window (struct window *w)
{
  struct xwidget_view* xv = NULL;
  for (Lisp_Object tail = Vxwidget_view_list; CONSP (tail); tail = XCDR (tail))
    {
      if (XWIDGET_VIEW_P (XCAR (tail))) {
        xv = XXWIDGET_VIEW (XCAR (tail));
        if(XWINDOW (xv->w) == w) {
          gtk_widget_destroy(xv->widgetwindow);
          Vxwidget_view_list = Fdelq (XCAR (tail), Vxwidget_view_list);
        }
      }
    }
}

struct xwidget_view*
xwidget_view_lookup (struct xwidget* xw, struct window *w)
{
  Lisp_Object xwidget, window, ret;
  XSETXWIDGET (xwidget, xw);
  XSETWINDOW (window, w);

  ret = Fxwidget_view_lookup (xwidget, window);

  return EQ (ret, Qnil) ? NULL : XXWIDGET_VIEW (ret);
}

struct xwidget*
lookup_xwidget (Lisp_Object  spec)
{
  /* When a xwidget lisp spec is found initialize the C struct that is used in the C code.
     This is done by redisplay so values change if the spec changes.
     So, take special care of one-shot events

     TODO remove xwidget init from display spec. simply store an xwidget reference only and set
     size etc when creating the xwidget, which should happen before insertion into buffer
  */
  int found = 0, found1 = 0, found2 = 0;
  Lisp_Object value;
  struct xwidget *xw;

  value = xwidget_spec_value (spec, QCxwidget, &found1);
  xw = XXWIDGET(value);

  return xw;
}

/*set up detection of touched xwidget*/
void
xwidget_start_redisplay (void)
{
  for (Lisp_Object tail = Vxwidget_view_list; CONSP (tail); tail = XCDR (tail))
    {
      if (XWIDGET_VIEW_P (XCAR (tail)))
        XXWIDGET_VIEW (XCAR (tail))->redisplayed = 0;
    }
}

/* the xwidget was touched during redisplay, so it isnt a candidate for hiding*/
void
xwidget_touch (struct xwidget_view *xv)
{
  xv->redisplayed = 1;
}

int
xwidget_touched (struct xwidget_view *xv)
{
  return  xv->redisplayed;
}

/* redisplay has ended, now we should hide untouched xwidgets
*/
void
xwidget_end_redisplay (struct window *w, struct glyph_matrix *matrix)
{

  int i;
  struct xwidget *xw;
  int area;


  xwidget_start_redisplay ();
  //iterate desired glyph matrix of window here, hide gtk widgets
  //not in the desired matrix.

  //this only takes care of xwidgets in active windows.
  //if a window goes away from screen xwidget views wust be deleted

  //  dump_glyph_matrix(matrix, 2);
  for (i = 0; i < matrix->nrows; ++i)
    {
      //    dump_glyph_row (MATRIX_ROW (matrix, i), i, glyphs);
      struct glyph_row *row;
      row = MATRIX_ROW (matrix, i);
      if (row->enabled_p != 0)
        {
          for (area = LEFT_MARGIN_AREA; area < LAST_AREA; ++area)
            {
              struct glyph *glyph = row->glyphs[area];
              struct glyph *glyph_end = glyph + row->used[area];
              for (; glyph < glyph_end; ++glyph)
                {
                  if (glyph->type == XWIDGET_GLYPH)
                    {
                      /*
                        the only call to xwidget_end_redisplay is in dispnew
                        xwidget_end_redisplay(w->current_matrix);
                      */
                      xwidget_touch (xwidget_view_lookup(glyph->u.xwidget,
                                                         w));
                    }
                }
            }
        }
    }

  for (Lisp_Object tail = Vxwidget_view_list; CONSP (tail); tail = XCDR (tail))
    {
      if (XWIDGET_VIEW_P (XCAR (tail))) {
        struct xwidget_view* xv = XXWIDGET_VIEW (XCAR (tail));

        //"touched" is only meaningful for the current window, so disregard other views
        if (XWINDOW (xv->w) == w) {
          if (xwidget_touched(xv))
            xwidget_show_view (xv);
          else
            xwidget_hide_view (xv);
        }
      }
    }
}

/* Kill all xwidget in BUFFER. */
void
kill_buffer_xwidgets (Lisp_Object buffer)
{
  Lisp_Object tail, xwidget;
  for (tail = Fget_buffer_xwidgets (buffer); CONSP (tail); tail = XCDR (tail))
    {
      xwidget = XCAR (tail);
      Vxwidget_list = Fdelq (xwidget, Vxwidget_list);
      /* TODO free the GTK things in xw */
      {
        CHECK_XWIDGET (xwidget);
        struct xwidget *xw = XXWIDGET (xwidget);
        if (xw->widget_osr && xw->widgetwindow_osr)
          {
            gtk_widget_destroy(xw->widget_osr);
            gtk_widget_destroy(xw->widgetwindow_osr);
          }
      }
    }
}

#endif  /* HAVE_XWIDGETS */
