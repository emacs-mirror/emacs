/* X Communication module for terminals which understand the X protocol.

Copyright (C) 1986, 1988, 1993-1994, 1996, 1999-2020 Free Software
Foundation, Inc.

Author: Jon Arnold
	Roman Budzianowski
	Robert Krawitz

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

/* X pop-up deck-of-cards menu facility for GNU Emacs.
 *
 */

/* Modified by Fred Pierresteguy on December 93
   to make the popup menus and menubar use the Xt.  */

/* Rewritten for clarity and GC protection by rms in Feb 94.  */

#include <config.h>

#include <stdio.h>

#include "lisp.h"
#include "keyboard.h"
#include "frame.h"
#include "systime.h"
#include "termhooks.h"
#include "window.h"
#include "blockinput.h"
#include "buffer.h"
#include "coding.h"
#include "sysselect.h"
#include "pdumper.h"

#ifdef MSDOS
#include "msdos.h"
#endif

#ifdef HAVE_X_WINDOWS
/* This may include sys/types.h, and that somehow loses
   if this is not done before the other system files.  */
#include "xterm.h"
#endif

/* Load sys/types.h if not already loaded.
   In some systems loading it twice is suicidal.  */
#ifndef makedev
#include <sys/types.h>
#endif

#ifdef HAVE_X_WINDOWS
/*  Defining HAVE_MULTILINGUAL_MENU would mean that the toolkit menu
    code accepts the Emacs internal encoding.  */
#undef HAVE_MULTILINGUAL_MENU
#ifdef USE_X_TOOLKIT
#include "widget.h"
#include <X11/Xlib.h>
#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#ifdef USE_LUCID
#include "xsettings.h"
#include "../lwlib/xlwmenu.h"
#ifdef HAVE_XAW3D
#include <X11/Xaw3d/Paned.h>
#else /* !HAVE_XAW3D */
#include <X11/Xaw/Paned.h>
#endif /* HAVE_XAW3D */
#endif /* USE_LUCID */
#ifdef USE_MOTIF
#include "../lwlib/lwlib.h"
#endif
#else /* not USE_X_TOOLKIT */
#ifndef USE_GTK
#include "../oldXMenu/XMenu.h"
#endif
#endif /* not USE_X_TOOLKIT */
#endif /* HAVE_X_WINDOWS */

#ifdef USE_GTK
#include "gtkutil.h"
#ifdef HAVE_GTK3
#include "xgselect.h"
#endif
#endif

#include "menu.h"


/* Flag which when set indicates a dialog or menu has been posted by
   Xt on behalf of one of the widget sets.  */
static int popup_activated_flag;


#ifdef USE_X_TOOLKIT

static LWLIB_ID next_menubar_widget_id;

/* Return the frame whose ->output_data.x->id equals ID, or 0 if none.  */

static struct frame *
menubar_id_to_frame (LWLIB_ID id)
{
  Lisp_Object tail, frame;
  struct frame *f;

  FOR_EACH_FRAME (tail, frame)
    {
      f = XFRAME (frame);
      if (!FRAME_WINDOW_P (f))
	continue;
      if (f->output_data.x->id == id)
	return f;
    }
  return 0;
}

#endif

#ifndef MSDOS

#if defined USE_GTK || defined USE_MOTIF

/* Set menu_items_inuse so no other popup menu or dialog is created.  */

void
x_menu_set_in_use (bool in_use)
{
  Lisp_Object frames, frame;

  menu_items_inuse = in_use;
  popup_activated_flag = in_use;
#ifdef USE_X_TOOLKIT
  if (popup_activated_flag)
    x_activate_timeout_atimer ();
#endif

  /* Don't let frames in `above' z-group obscure popups.  */
  FOR_EACH_FRAME (frames, frame)
    {
      struct frame *f = XFRAME (frame);

      if (in_use && FRAME_Z_GROUP_ABOVE (f))
	x_set_z_group (f, Qabove_suspended, Qabove);
      else if (!in_use && FRAME_Z_GROUP_ABOVE_SUSPENDED (f))
	x_set_z_group (f, Qabove, Qabove_suspended);
    }
}
#endif

/* Wait for an X event to arrive or for a timer to expire.  */

void
x_menu_wait_for_event (void *data)
{
  /* Another way to do this is to register a timer callback, that can be
     done in GTK and Xt.  But we have to do it like this when using only X
     anyway, and with callbacks we would have three variants for timer handling
     instead of the small ifdefs below.  */

  while (
#ifdef USE_X_TOOLKIT
         ! XtAppPending (Xt_app_con)
#elif defined USE_GTK
         ! gtk_events_pending ()
#else
         ! XPending (data)
#endif
         )
    {
      struct timespec next_time = timer_check (), *ntp;
      fd_set read_fds;
      struct x_display_info *dpyinfo;
      int n = 0;

      FD_ZERO (&read_fds);
      for (dpyinfo = x_display_list; dpyinfo; dpyinfo = dpyinfo->next)
        {
          int fd = ConnectionNumber (dpyinfo->display);
          FD_SET (fd, &read_fds);
          if (fd > n) n = fd;
          XFlush (dpyinfo->display);
        }

      if (! timespec_valid_p (next_time))
        ntp = 0;
      else
        ntp = &next_time;

#if defined USE_GTK && defined HAVE_GTK3
      /* Gtk3 have arrows on menus when they don't fit.  When the
	 pointer is over an arrow, a timeout scrolls it a bit.  Use
	 xg_select so that timeout gets triggered.  */
      xg_select (n + 1, &read_fds, NULL, NULL, ntp, NULL);
#else
      pselect (n + 1, &read_fds, NULL, NULL, ntp, NULL);
#endif
    }
}
#endif /* ! MSDOS */


#if defined (USE_X_TOOLKIT) || defined (USE_GTK)

#ifdef USE_X_TOOLKIT

/* Loop in Xt until the menu pulldown or dialog popup has been
   popped down (deactivated).  This is used for x-popup-menu
   and x-popup-dialog; it is not used for the menu bar.

   NOTE: All calls to popup_get_selection should be protected
   with BLOCK_INPUT, UNBLOCK_INPUT wrappers.  */

static void
popup_get_selection (XEvent *initial_event, struct x_display_info *dpyinfo,
		     LWLIB_ID id, bool do_timers)
{
  XEvent event;

  while (popup_activated_flag)
    {
      if (initial_event)
        {
          event = *initial_event;
          initial_event = 0;
        }
      else
        {
          if (do_timers) x_menu_wait_for_event (0);
          XtAppNextEvent (Xt_app_con, &event);
        }

      /* Make sure we don't consider buttons grabbed after menu goes.
         And make sure to deactivate for any ButtonRelease,
         even if XtDispatchEvent doesn't do that.  */
      if (event.type == ButtonRelease
          && dpyinfo->display == event.xbutton.display)
        {
          dpyinfo->grabbed &= ~(1 << event.xbutton.button);
#ifdef USE_MOTIF /* Pretending that the event came from a
                    Btn1Down seems the only way to convince Motif to
                    activate its callbacks; setting the XmNmenuPost
                    isn't working. --marcus@sysc.pdx.edu.  */
          event.xbutton.button = 1;
          /*  Motif only pops down menus when no Ctrl, Alt or Mod
              key is pressed and the button is released.  So reset key state
              so Motif thinks this is the case.  */
          event.xbutton.state = 0;
#endif
        }
      /* Pop down on C-g and Escape.  */
      else if (event.type == KeyPress
               && dpyinfo->display == event.xbutton.display)
        {
          KeySym keysym = XLookupKeysym (&event.xkey, 0);

          if ((keysym == XK_g && (event.xkey.state & ControlMask) != 0)
              || keysym == XK_Escape) /* Any escape, ignore modifiers.  */
            popup_activated_flag = 0;
        }

      x_dispatch_event (&event, event.xany.display);
    }
}

DEFUN ("x-menu-bar-open-internal", Fx_menu_bar_open_internal, Sx_menu_bar_open_internal, 0, 1, "i",
       doc: /* SKIP: real doc in USE_GTK definition in xmenu.c.  */)
  (Lisp_Object frame)
{
  XEvent ev;
  struct frame *f = decode_window_system_frame (frame);
  Widget menubar;
  block_input ();

  if (FRAME_EXTERNAL_MENU_BAR (f))
    set_frame_menubar (f, false, true);

  menubar = FRAME_X_OUTPUT (f)->menubar_widget;
  if (menubar)
    {
      Window child;
      bool error_p = false;

      x_catch_errors (FRAME_X_DISPLAY (f));
      memset (&ev, 0, sizeof ev);
      ev.xbutton.display = FRAME_X_DISPLAY (f);
      ev.xbutton.window = XtWindow (menubar);
      ev.xbutton.root = FRAME_DISPLAY_INFO (f)->root_window;
      ev.xbutton.time = XtLastTimestampProcessed (FRAME_X_DISPLAY (f));
      ev.xbutton.button = Button1;
      ev.xbutton.x = ev.xbutton.y = FRAME_MENUBAR_HEIGHT (f) / 2;
      ev.xbutton.same_screen = True;

#ifdef USE_MOTIF
      {
        Arg al[2];
        WidgetList list;
        Cardinal nr;
        XtSetArg (al[0], XtNchildren, &list);
        XtSetArg (al[1], XtNnumChildren, &nr);
        XtGetValues (menubar, al, 2);
        ev.xbutton.window = XtWindow (list[0]);
      }
#endif

      XTranslateCoordinates (FRAME_X_DISPLAY (f),
                             /* From-window, to-window.  */
                             ev.xbutton.window, ev.xbutton.root,

                             /* From-position, to-position.  */
                             ev.xbutton.x, ev.xbutton.y,
                             &ev.xbutton.x_root, &ev.xbutton.y_root,

                             /* Child of win.  */
                             &child);
      error_p = x_had_errors_p (FRAME_X_DISPLAY (f));
      x_uncatch_errors_after_check ();

      if (! error_p)
        {
          ev.type = ButtonPress;
          ev.xbutton.state = 0;

          XtDispatchEvent (&ev);
          ev.xbutton.type = ButtonRelease;
          ev.xbutton.state = Button1Mask;
          XtDispatchEvent (&ev);
        }
    }

  unblock_input ();

  return Qnil;
}
#endif /* USE_X_TOOLKIT */


#ifdef USE_GTK
DEFUN ("x-menu-bar-open-internal", Fx_menu_bar_open_internal, Sx_menu_bar_open_internal, 0, 1, "i",
       doc: /* Start key navigation of the menu bar in FRAME.
This initially opens the first menu bar item and you can then navigate with the
arrow keys, select a menu entry with the return key or cancel with the
escape key.  If FRAME has no menu bar this function does nothing.

If FRAME is nil or not given, use the selected frame.  */)
  (Lisp_Object frame)
{
  GtkWidget *menubar;
  struct frame *f;

  block_input ();
  f = decode_window_system_frame (frame);

  if (FRAME_EXTERNAL_MENU_BAR (f))
    set_frame_menubar (f, false, true);

  menubar = FRAME_X_OUTPUT (f)->menubar_widget;
  if (menubar)
    {
      /* Activate the first menu.  */
      GList *children = gtk_container_get_children (GTK_CONTAINER (menubar));

      if (children)
        {
          g_signal_emit_by_name (children->data, "activate_item");
          popup_activated_flag = 1;
          g_list_free (children);
        }
    }
  unblock_input ();

  return Qnil;
}

/* Loop util popup_activated_flag is set to zero in a callback.
   Used for popup menus and dialogs. */

static void
popup_widget_loop (bool do_timers, GtkWidget *widget)
{
  ++popup_activated_flag;

  /* Process events in the Gtk event loop until done.  */
  while (popup_activated_flag)
    {
      if (do_timers) x_menu_wait_for_event (0);
      gtk_main_iteration ();
    }
}
#endif

/* Activate the menu bar of frame F.
   This is called from keyboard.c when it gets the
   MENU_BAR_ACTIVATE_EVENT out of the Emacs event queue.

   To activate the menu bar, we use the X button-press event
   that was saved in saved_menu_event.
   That makes the toolkit do its thing.

   But first we recompute the menu bar contents (the whole tree).

   The reason for saving the button event until here, instead of
   passing it to the toolkit right away, is that we can safely
   execute Lisp code.  */

void
x_activate_menubar (struct frame *f)
{
  eassert (FRAME_X_P (f));

  if (!f->output_data.x->saved_menu_event->type)
    return;

#ifdef USE_GTK
  if (! xg_win_to_widget (FRAME_X_DISPLAY (f),
                          f->output_data.x->saved_menu_event->xany.window))
    return;
#endif

  set_frame_menubar (f, false, true);
  block_input ();
  popup_activated_flag = 1;
#ifdef USE_GTK
  XPutBackEvent (f->output_data.x->display_info->display,
                 f->output_data.x->saved_menu_event);
#else
  XtDispatchEvent (f->output_data.x->saved_menu_event);
#endif
  unblock_input ();

  /* Ignore this if we get it a second time.  */
  f->output_data.x->saved_menu_event->type = 0;
}

/* This callback is invoked when the user selects a menubar cascade
   pushbutton, but before the pulldown menu is posted.  */

#ifndef USE_GTK
static void
popup_activate_callback (Widget widget, LWLIB_ID id, XtPointer client_data)
{
  popup_activated_flag = 1;
  x_activate_timeout_atimer ();
}
#endif

/* This callback is invoked when a dialog or menu is finished being
   used and has been unposted.  */

static void
popup_deactivate_callback (
#ifdef USE_GTK
			   GtkWidget *widget, gpointer client_data
#else
			   Widget widget, LWLIB_ID id, XtPointer client_data
#endif
			   )
{
  popup_activated_flag = 0;
}


/* Function that finds the frame for WIDGET and shows the HELP text
   for that widget.
   F is the frame if known, or NULL if not known.  */
static void
show_help_event (struct frame *f, xt_or_gtk_widget widget, Lisp_Object help)
{
  Lisp_Object frame;

  if (f)
    {
      XSETFRAME (frame, f);
      kbd_buffer_store_help_event (frame, help);
    }
  else
    show_help_echo (help, Qnil, Qnil, Qnil);
}

/* Callback called when menu items are highlighted/unhighlighted
   while moving the mouse over them.  WIDGET is the menu bar or menu
   popup widget.  ID is its LWLIB_ID.  CALL_DATA contains a pointer to
   the data structure for the menu item, or null in case of
   unhighlighting.  */

#ifdef USE_GTK
static void
menu_highlight_callback (GtkWidget *widget, gpointer call_data)
{
  xg_menu_item_cb_data *cb_data;
  Lisp_Object help;

  cb_data = g_object_get_data (G_OBJECT (widget), XG_ITEM_DATA);
  if (! cb_data) return;

  help = call_data ? cb_data->help : Qnil;

  /* If popup_activated_flag is greater than 1 we are in a popup menu.
     Don't pass the frame to show_help_event for those.
     Passing frame creates an Emacs event.  As we are looping in
     popup_widget_loop, it won't be handled.  Passing NULL shows the tip
     directly without using an Emacs event.  This is what the Lucid code
     does below.  */
  show_help_event (popup_activated_flag <= 1 ? cb_data->cl_data->f : NULL,
                   widget, help);
}
#else
static void
menu_highlight_callback (Widget widget, LWLIB_ID id, void *call_data)
{
  widget_value *wv = call_data;
  Lisp_Object help = wv ? wv->help : Qnil;

  /* Determine the frame for the help event.  */
  struct frame *f = menubar_id_to_frame (id);

  show_help_event (f, widget, help);
}
#endif

#ifdef USE_GTK
/* Gtk calls callbacks just because we tell it what item should be
   selected in a radio group.  If this variable is set to a non-zero
   value, we are creating menus and don't want callbacks right now.
*/
static bool xg_crazy_callback_abort;

/* This callback is called from the menu bar pulldown menu
   when the user makes a selection.
   Figure out what the user chose
   and put the appropriate events into the keyboard buffer.  */
static void
menubar_selection_callback (GtkWidget *widget, gpointer client_data)
{
  xg_menu_item_cb_data *cb_data = client_data;

  if (xg_crazy_callback_abort)
    return;

  if (! cb_data || ! cb_data->cl_data || ! cb_data->cl_data->f)
    return;

  /* For a group of radio buttons, GTK calls the selection callback first
     for the item that was active before the selection and then for the one that
     is active after the selection.  For C-h k this means we get the help on
     the deselected item and then the selected item is executed.  Prevent that
     by ignoring the non-active item.  */
  if (GTK_IS_RADIO_MENU_ITEM (widget)
      && ! gtk_check_menu_item_get_active (GTK_CHECK_MENU_ITEM (widget)))
    return;

  /* When a menu is popped down, X generates a focus event (i.e. focus
     goes back to the frame below the menu).  Since GTK buffers events,
     we force it out here before the menu selection event.  Otherwise
     sit-for will exit at once if the focus event follows the menu selection
     event.  */

  block_input ();
  while (gtk_events_pending ())
    gtk_main_iteration ();
  unblock_input ();

  find_and_call_menu_selection (cb_data->cl_data->f,
                                cb_data->cl_data->menu_bar_items_used,
                                cb_data->cl_data->menu_bar_vector,
                                cb_data->call_data);
}

#else /* not USE_GTK */

/* This callback is called from the menu bar pulldown menu
   when the user makes a selection.
   Figure out what the user chose
   and put the appropriate events into the keyboard buffer.  */
static void
menubar_selection_callback (Widget widget, LWLIB_ID id, XtPointer client_data)
{
  struct frame *f;

  f = menubar_id_to_frame (id);
  if (!f)
    return;
  find_and_call_menu_selection (f, f->menu_bar_items_used,
                                f->menu_bar_vector, client_data);
}
#endif /* not USE_GTK */

/* Recompute all the widgets of frame F, when the menu bar has been
   changed.  */

static void
update_frame_menubar (struct frame *f)
{
#ifdef USE_GTK
  xg_update_frame_menubar (f);
#else
  struct x_output *x;

  eassert (FRAME_X_P (f));

  x = f->output_data.x;

  if (!x->menubar_widget || XtIsManaged (x->menubar_widget))
    return;

  block_input ();

  /* Do the voodoo which means "I'm changing lots of things, don't try
     to refigure sizes until I'm done."  */
  lw_refigure_widget (x->column_widget, False);

  /* The order in which children are managed is the top to bottom
     order in which they are displayed in the paned window.  First,
     remove the text-area widget.  */
  XtUnmanageChild (x->edit_widget);

  /* Remove the menubar that is there now, and put up the menubar that
     should be there.  */
  XtManageChild (x->menubar_widget);
  XtMapWidget (x->menubar_widget);
  XtVaSetValues (x->menubar_widget, XtNmappedWhenManaged, 1, NULL);

  /* Re-manage the text-area widget, and then thrash the sizes.  */
  XtManageChild (x->edit_widget);
  lw_refigure_widget (x->column_widget, True);

  /* Force the pane widget to resize itself.  */
  adjust_frame_size (f, -1, -1, 2, false, Qupdate_frame_menubar);
  unblock_input ();
#endif /* USE_GTK */
}

#ifdef USE_LUCID
static void
apply_systemfont_to_dialog (Widget w)
{
  const char *fn = xsettings_get_system_normal_font ();
  if (fn)
    {
      XrmDatabase db = XtDatabase (XtDisplay (w));
      if (db)
        XrmPutStringResource (&db, "*dialog.font", fn);
    }
}

static void
apply_systemfont_to_menu (struct frame *f, Widget w)
{
  const char *fn = xsettings_get_system_normal_font ();

  if (fn)
    {
      XrmDatabase db = XtDatabase (XtDisplay (w));
      if (db)
        {
          XrmPutStringResource (&db, "*menubar*font", fn);
          XrmPutStringResource (&db, "*popup*font", fn);
        }
    }
}

#endif

/* Set the contents of the menubar widgets of frame F.
   The argument FIRST_TIME is currently ignored;
   it is set the first time this is called, from initialize_frame_menubar.  */

void
set_frame_menubar (struct frame *f, bool first_time, bool deep_p)
{
  xt_or_gtk_widget menubar_widget, old_widget;
#ifdef USE_X_TOOLKIT
  LWLIB_ID id;
#endif
  Lisp_Object items;
  widget_value *wv, *first_wv, *prev_wv = 0;
  int i;
  int *submenu_start, *submenu_end;
  bool *submenu_top_level_items;
  int *submenu_n_panes;

  eassert (FRAME_X_P (f));

  menubar_widget = old_widget = f->output_data.x->menubar_widget;

  XSETFRAME (Vmenu_updating_frame, f);

#ifdef USE_X_TOOLKIT
  if (f->output_data.x->id == 0)
    f->output_data.x->id = next_menubar_widget_id++;
  id = f->output_data.x->id;
#endif

  if (! menubar_widget)
    deep_p = true;
  /* Make the first call for any given frame always go deep.  */
  else if (!f->output_data.x->saved_menu_event && !deep_p)
    {
      deep_p = true;
      f->output_data.x->saved_menu_event = xmalloc (sizeof (XEvent));
      f->output_data.x->saved_menu_event->type = 0;
    }

  if (deep_p)
    {
      /* Make a widget-value tree representing the entire menu trees.  */

      struct buffer *prev = current_buffer;
      Lisp_Object buffer;
      ptrdiff_t specpdl_count = SPECPDL_INDEX ();
      int previous_menu_items_used = f->menu_bar_items_used;
      Lisp_Object *previous_items
	= alloca (previous_menu_items_used * sizeof *previous_items);
      int subitems;

      /* If we are making a new widget, its contents are empty,
	 do always reinitialize them.  */
      if (! menubar_widget)
	previous_menu_items_used = 0;

      buffer = XWINDOW (FRAME_SELECTED_WINDOW (f))->contents;
      specbind (Qinhibit_quit, Qt);
      /* Don't let the debugger step into this code
	 because it is not reentrant.  */
      specbind (Qdebug_on_next_call, Qnil);

      record_unwind_save_match_data ();
      if (NILP (Voverriding_local_map_menu_flag))
	{
	  specbind (Qoverriding_terminal_local_map, Qnil);
	  specbind (Qoverriding_local_map, Qnil);
	}

      set_buffer_internal_1 (XBUFFER (buffer));

      /* Run the Lucid hook.  */
      safe_run_hooks (Qactivate_menubar_hook);

      /* If it has changed current-menubar from previous value,
	 really recompute the menubar from the value.  */
      if (! NILP (Vlucid_menu_bar_dirty_flag))
	call0 (Qrecompute_lucid_menubar);
      safe_run_hooks (Qmenu_bar_update_hook);
      fset_menu_bar_items (f, menu_bar_items (FRAME_MENU_BAR_ITEMS (f)));

      items = FRAME_MENU_BAR_ITEMS (f);

      /* Save the frame's previous menu bar contents data.  */
      if (previous_menu_items_used)
	memcpy (previous_items, XVECTOR (f->menu_bar_vector)->contents,
		previous_menu_items_used * word_size);

      /* Fill in menu_items with the current menu bar contents.
	 This can evaluate Lisp code.  */
      save_menu_items ();

      menu_items = f->menu_bar_vector;
      menu_items_allocated = VECTORP (menu_items) ? ASIZE (menu_items) : 0;
      subitems = ASIZE (items) / 4;
      submenu_start = alloca ((subitems + 1) * sizeof *submenu_start);
      submenu_end = alloca (subitems * sizeof *submenu_end);
      submenu_n_panes = alloca (subitems * sizeof *submenu_n_panes);
      submenu_top_level_items = alloca (subitems
					* sizeof *submenu_top_level_items);
      init_menu_items ();
      for (i = 0; i < subitems; i++)
	{
	  Lisp_Object key, string, maps;

	  key = AREF (items, 4 * i);
	  string = AREF (items, 4 * i + 1);
	  maps = AREF (items, 4 * i + 2);
	  if (NILP (string))
	    break;

	  submenu_start[i] = menu_items_used;

	  menu_items_n_panes = 0;
	  submenu_top_level_items[i]
	    = parse_single_submenu (key, string, maps);
	  submenu_n_panes[i] = menu_items_n_panes;

	  submenu_end[i] = menu_items_used;
	}

      submenu_start[i] = -1;
      finish_menu_items ();

      /* Convert menu_items into widget_value trees
	 to display the menu.  This cannot evaluate Lisp code.  */

      wv = make_widget_value ("menubar", NULL, true, Qnil);
      wv->button_type = BUTTON_TYPE_NONE;
      first_wv = wv;

      for (i = 0; submenu_start[i] >= 0; i++)
	{
	  menu_items_n_panes = submenu_n_panes[i];
	  wv = digest_single_submenu (submenu_start[i], submenu_end[i],
				      submenu_top_level_items[i]);
	  if (prev_wv)
	    prev_wv->next = wv;
	  else
	    first_wv->contents = wv;
	  /* Don't set wv->name here; GC during the loop might relocate it.  */
	  wv->enabled = true;
	  wv->button_type = BUTTON_TYPE_NONE;
	  prev_wv = wv;
	}

      set_buffer_internal_1 (prev);

      /* If there has been no change in the Lisp-level contents
	 of the menu bar, skip redisplaying it.  Just exit.  */

      /* Compare the new menu items with the ones computed last time.  */
      for (i = 0; i < previous_menu_items_used; i++)
	if (menu_items_used == i
	    || (!EQ (previous_items[i], AREF (menu_items, i))))
	  break;
      if (i == menu_items_used && i == previous_menu_items_used && i != 0)
	{
	  /* The menu items have not changed.  Don't bother updating
	     the menus in any form, since it would be a no-op.  */
	  free_menubar_widget_value_tree (first_wv);
	  discard_menu_items ();
	  unbind_to (specpdl_count, Qnil);
	  return;
	}

      /* The menu items are different, so store them in the frame.  */
      fset_menu_bar_vector (f, menu_items);
      f->menu_bar_items_used = menu_items_used;

      /* This undoes save_menu_items.  */
      unbind_to (specpdl_count, Qnil);

      /* Now GC cannot happen during the lifetime of the widget_value,
	 so it's safe to store data from a Lisp_String.  */
      wv = first_wv->contents;
      for (i = 0; i < ASIZE (items); i += 4)
	{
	  Lisp_Object string;
	  string = AREF (items, i + 1);
	  if (NILP (string))
            break;
          wv->name = SSDATA (string);
          update_submenu_strings (wv->contents);
          wv = wv->next;
	}

    }
  else
    {
      /* Make a widget-value tree containing
	 just the top level menu bar strings.  */

      wv = make_widget_value ("menubar", NULL, true, Qnil);
      wv->button_type = BUTTON_TYPE_NONE;
      first_wv = wv;

      items = FRAME_MENU_BAR_ITEMS (f);
      for (i = 0; i < ASIZE (items); i += 4)
	{
	  Lisp_Object string;

	  string = AREF (items, i + 1);
	  if (NILP (string))
	    break;

	  wv = make_widget_value (SSDATA (string), NULL, true, Qnil);
	  wv->button_type = BUTTON_TYPE_NONE;
	  /* This prevents lwlib from assuming this
	     menu item is really supposed to be empty.  */
	  /* The intptr_t cast avoids a warning.
	     This value just has to be different from small integers.  */
	  wv->call_data = (void *) (intptr_t) (-1);

	  if (prev_wv)
	    prev_wv->next = wv;
	  else
	    first_wv->contents = wv;
	  prev_wv = wv;
	}

      /* Forget what we thought we knew about what is in the
	 detailed contents of the menu bar menus.
	 Changing the top level always destroys the contents.  */
      f->menu_bar_items_used = 0;
    }

  /* Create or update the menu bar widget.  */

  block_input ();

#ifdef USE_GTK
  xg_crazy_callback_abort = true;
  if (menubar_widget)
    {
      /* The fourth arg is DEEP_P, which says to consider the entire
	 menu trees we supply, rather than just the menu bar item names.  */
      xg_modify_menubar_widgets (menubar_widget,
                                 f,
                                 first_wv,
                                 deep_p,
                                 G_CALLBACK (menubar_selection_callback),
                                 G_CALLBACK (popup_deactivate_callback),
                                 G_CALLBACK (menu_highlight_callback));
    }
  else
    {
      menubar_widget
        = xg_create_widget ("menubar", "menubar", f, first_wv,
                            G_CALLBACK (menubar_selection_callback),
                            G_CALLBACK (popup_deactivate_callback),
                            G_CALLBACK (menu_highlight_callback));

      f->output_data.x->menubar_widget = menubar_widget;
    }


#else /* not USE_GTK */
  if (menubar_widget)
    {
      /* Disable resizing (done for Motif!) */
      lw_allow_resizing (f->output_data.x->widget, False);

      /* The third arg is DEEP_P, which says to consider the entire
	 menu trees we supply, rather than just the menu bar item names.  */
      lw_modify_all_widgets (id, first_wv, deep_p);

      /* Re-enable the edit widget to resize.  */
      lw_allow_resizing (f->output_data.x->widget, True);
    }
  else
    {
      char menuOverride[] = "Ctrl<KeyPress>g: MenuGadgetEscape()";
      XtTranslations  override = XtParseTranslationTable (menuOverride);

#ifdef USE_LUCID
      apply_systemfont_to_menu (f, f->output_data.x->column_widget);
#endif
      menubar_widget = lw_create_widget ("menubar", "menubar", id,
                                         first_wv,
					 f->output_data.x->column_widget,
					 false,
					 popup_activate_callback,
					 menubar_selection_callback,
					 popup_deactivate_callback,
					 menu_highlight_callback);
      f->output_data.x->menubar_widget = menubar_widget;

      /* Make menu pop down on C-g.  */
      XtOverrideTranslations (menubar_widget, override);
    }

  {
    int menubar_size;
    if (f->output_data.x->menubar_widget)
      XtRealizeWidget (f->output_data.x->menubar_widget);

    menubar_size
      = (f->output_data.x->menubar_widget
	 ? (f->output_data.x->menubar_widget->core.height
#ifndef USE_LUCID
	    /* Damn me...  With Lucid I get a core.border_width of 1
	       only the first time this is called and an ibw of 1 every
	       time this is called.  So the first time this is called I
	       was off by one.  Fix that here by never adding
	       core.border_width for Lucid.  */
	    + f->output_data.x->menubar_widget->core.border_width
#endif /* USE_LUCID */
	    )
	 : 0);

#ifdef USE_LUCID
      /* Experimentally, we now get the right results
	 for -geometry -0-0 without this.  24 Aug 96, rms.
         Maybe so, but the menu bar size is missing the pixels so the
         WM size hints are off by these pixels.  Jan D, oct 2009.  */
    if (FRAME_EXTERNAL_MENU_BAR (f))
      {
        Dimension ibw = 0;

        XtVaGetValues (f->output_data.x->column_widget,
		       XtNinternalBorderWidth, &ibw, NULL);
	menubar_size += ibw;
      }
#endif /* USE_LUCID */

    FRAME_MENUBAR_HEIGHT (f) = menubar_size;
  }
#endif /* not USE_GTK */

  free_menubar_widget_value_tree (first_wv);
  update_frame_menubar (f);

#ifdef USE_GTK
  xg_crazy_callback_abort = false;
#endif

  unblock_input ();
}

/* Called from Fx_create_frame to create the initial menubar of a frame
   before it is mapped, so that the window is mapped with the menubar already
   there instead of us tacking it on later and thrashing the window after it
   is visible.  */

void
initialize_frame_menubar (struct frame *f)
{
  /* This function is called before the first chance to redisplay
     the frame.  It has to be, so the frame will have the right size.  */
  fset_menu_bar_items (f, menu_bar_items (FRAME_MENU_BAR_ITEMS (f)));
  set_frame_menubar (f, true, true);
}


/* Get rid of the menu bar of frame F, and free its storage.
   This is used when deleting a frame, and when turning off the menu bar.
   For GTK this function is in gtkutil.c.  */

#ifndef USE_GTK
void
free_frame_menubar (struct frame *f)
{
  Widget menubar_widget;
#ifdef USE_MOTIF
  /* Motif automatically shrinks the frame in lw_destroy_all_widgets.
     If we want to preserve the old height, calculate it now so we can
     restore it below.  */
  int old_height = FRAME_TEXT_HEIGHT (f) + FRAME_MENUBAR_HEIGHT (f);
#endif

  eassert (FRAME_X_P (f));

  menubar_widget = f->output_data.x->menubar_widget;

  FRAME_MENUBAR_HEIGHT (f) = 0;

  if (menubar_widget)
    {
#ifdef USE_MOTIF
      /* Removing the menu bar magically changes the shell widget's x
	 and y position of (0, 0) which, when the menu bar is turned
	 on again, leads to pull-down menus appearing in strange
	 positions near the upper-left corner of the display.  This
	 happens only with some window managers like twm and ctwm,
	 but not with other like Motif's mwm or kwm, because the
	 latter generate ConfigureNotify events when the menu bar
	 is switched off, which fixes the shell position.  */
      Position x0, y0, x1, y1;
#endif

      block_input ();

#ifdef USE_MOTIF
      if (f->output_data.x->widget)
	XtVaGetValues (f->output_data.x->widget, XtNx, &x0, XtNy, &y0, NULL);
#endif

      lw_destroy_all_widgets ((LWLIB_ID) f->output_data.x->id);
      f->output_data.x->menubar_widget = NULL;

      if (f->output_data.x->widget)
	{
#ifdef USE_MOTIF
	  XtVaGetValues (f->output_data.x->widget, XtNx, &x1, XtNy, &y1, NULL);
	  if (x1 == 0 && y1 == 0)
	    XtVaSetValues (f->output_data.x->widget, XtNx, x0, XtNy, y0, NULL);
	  if (frame_inhibit_resize (f, false, Qmenu_bar_lines))
	    adjust_frame_size (f, -1, old_height, 1, false, Qfree_frame_menubar_1);
	  else
	    adjust_frame_size (f, -1, -1, 2, false, Qfree_frame_menubar_1);
#else
	  adjust_frame_size (f, -1, -1, 2, false, Qfree_frame_menubar_1);
#endif /* USE_MOTIF */
	}
      else
	{
#ifdef USE_MOTIF
	  if (WINDOWP (FRAME_ROOT_WINDOW (f))
	      && frame_inhibit_resize (f, false, Qmenu_bar_lines))
	    adjust_frame_size (f, -1, old_height, 1, false, Qfree_frame_menubar_2);
#endif
	}

      unblock_input ();
    }
}
#endif /* not USE_GTK */

#endif /* USE_X_TOOLKIT || USE_GTK */

/* x_menu_show actually displays a menu using the panes and items in menu_items
   and returns the value selected from it.
   There are two versions of x_menu_show, one for Xt and one for Xlib.
   Both assume input is blocked by the caller.  */

/* F is the frame the menu is for.
   X and Y are the frame-relative specified position,
   relative to the inside upper left corner of the frame F.
   Bitfield MENUFLAGS bits are:
   MENU_FOR_CLICK is set if this menu was invoked for a mouse click.
   MENU_KEYMAPS is set if this menu was specified with keymaps;
    in that case, we return a list containing the chosen item's value
    and perhaps also the pane's prefix.
   TITLE is the specified menu title.
   ERROR is a place to store an error message string in case of failure.
   (We return nil on failure, but the value doesn't actually matter.)  */

#if defined (USE_X_TOOLKIT) || defined (USE_GTK)

/* The item selected in the popup menu.  */
static Lisp_Object *volatile menu_item_selection;

#ifdef USE_GTK

/* Used when position a popup menu.  See menu_position_func and
   create_and_show_popup_menu below.  */
struct next_popup_x_y
{
  struct frame *f;
  int x;
  int y;
};

/* The menu position function to use if we are not putting a popup
   menu where the pointer is.
   MENU is the menu to pop up.
   X and Y shall on exit contain x/y where the menu shall pop up.
   PUSH_IN is not documented in the GTK manual.
   USER_DATA is any data passed in when calling gtk_menu_popup.
   Here it points to a struct next_popup_x_y where the coordinates
   to store in *X and *Y are as well as the frame for the popup.

   Here only X and Y are used.  */
static void
menu_position_func (GtkMenu *menu, gint *x, gint *y, gboolean *push_in, gpointer user_data)
{
  struct next_popup_x_y *data = user_data;
  GtkRequisition req;
  int max_x = -1;
  int max_y = -1;
#ifdef HAVE_GTK3
  int scale;
#endif

  Lisp_Object frame, workarea;

  XSETFRAME (frame, data->f);

#ifdef HAVE_GTK3
  scale = xg_get_scale (data->f);
#endif
  /* TODO: Get the monitor workarea directly without calculating other
     items in x-display-monitor-attributes-list. */
  workarea = call3 (Qframe_monitor_workarea,
                    Qnil,
                    make_fixnum (data->x),
                    make_fixnum (data->y));

  if (CONSP (workarea))
    {
      int min_x, min_y;

      min_x = XFIXNUM (XCAR (workarea));
      min_y = XFIXNUM (Fnth (make_fixnum (1), workarea));
      max_x = min_x + XFIXNUM (Fnth (make_fixnum (2), workarea));
      max_y = min_y + XFIXNUM (Fnth (make_fixnum (3), workarea));
    }

  if (max_x < 0 || max_y < 0)
    {
      struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (data->f);

      max_x = x_display_pixel_width (dpyinfo);
      max_y = x_display_pixel_height (dpyinfo);
    }

  /* frame-monitor-workarea and {x,y}_display_pixel_width/height all
     return device pixels, but GTK wants scaled pixels.  The positions
     passed in via data were already scaled for us.  */
#ifdef HAVE_GTK3
  max_x /= scale;
  max_y /= scale;
#endif
  *x = data->x;
  *y = data->y;

  /* Check if there is room for the menu.  If not, adjust x/y so that
     the menu is fully visible.  gtk_widget_get_preferred_size returns
     scaled pixels, so there is no need to apply the scaling
     factor.  */
  gtk_widget_get_preferred_size (GTK_WIDGET (menu), NULL, &req);
  if (data->x + req.width > max_x)
    *x -= data->x + req.width - max_x;
  if (data->y + req.height > max_y)
    *y -= data->y + req.height - max_y;
}

static void
popup_selection_callback (GtkWidget *widget, gpointer client_data)
{
  xg_menu_item_cb_data *cb_data = client_data;

  if (xg_crazy_callback_abort) return;
  if (cb_data) menu_item_selection = cb_data->call_data;
}

static void
pop_down_menu (void *arg)
{
  popup_activated_flag = 0;
  block_input ();
  gtk_widget_destroy (GTK_WIDGET (arg));
  unblock_input ();
}

/* Pop up the menu for frame F defined by FIRST_WV at X/Y and loop until the
   menu pops down.
   menu_item_selection will be set to the selection.  */
static void
create_and_show_popup_menu (struct frame *f, widget_value *first_wv,
			    int x, int y, bool for_click)
{
  int i;
  GtkWidget *menu;
  GtkMenuPositionFunc pos_func = 0;  /* Pop up at pointer.  */
  struct next_popup_x_y popup_x_y;
  ptrdiff_t specpdl_count = SPECPDL_INDEX ();
  bool use_pos_func = ! for_click;

#ifdef HAVE_GTK3
  /* Always use position function for Gtk3.  Otherwise menus may become
     too small to show anything.  */
  use_pos_func = true;
#endif

  eassert (FRAME_X_P (f));

  xg_crazy_callback_abort = true;
  menu = xg_create_widget ("popup", first_wv->name, f, first_wv,
                           G_CALLBACK (popup_selection_callback),
                           G_CALLBACK (popup_deactivate_callback),
                           G_CALLBACK (menu_highlight_callback));
  xg_crazy_callback_abort = false;

  if (use_pos_func)
    {
      Window dummy_window;

      /* Not invoked by a click.  pop up at x/y.  */
      pos_func = menu_position_func;

      /* Adjust coordinates to be root-window-relative.  */
      block_input ();
      XTranslateCoordinates (FRAME_X_DISPLAY (f),

                             /* From-window, to-window.  */
                             FRAME_X_WINDOW (f),
                             FRAME_DISPLAY_INFO (f)->root_window,

                             /* From-position, to-position.  */
                             x, y, &x, &y,

                             /* Child of win.  */
                             &dummy_window);
#ifdef HAVE_GTK3
      /* Use window scaling factor to adjust position for hidpi screens. */
      x /= xg_get_scale (f);
      y /= xg_get_scale (f);
#endif
      unblock_input ();
      popup_x_y.x = x;
      popup_x_y.y = y;
      popup_x_y.f = f;

      i = 0;  /* gtk_menu_popup needs this to be 0 for a non-button popup.  */
    }

  if (for_click)
    {
      for (i = 0; i < 5; i++)
        if (FRAME_DISPLAY_INFO (f)->grabbed & (1 << i))
          break;
      /* If keys aren't grabbed (i.e., a mouse up event), use 0.  */
      if (i == 5) i = 0;
    }

  /* Display the menu.  */
  gtk_widget_show_all (menu);

  gtk_menu_popup (GTK_MENU (menu), 0, 0, pos_func, &popup_x_y, i,
		  FRAME_DISPLAY_INFO (f)->last_user_time);

  record_unwind_protect_ptr (pop_down_menu, menu);

  if (gtk_widget_get_mapped (menu))
    {
      /* Set this to one.  popup_widget_loop increases it by one, so it becomes
         two.  show_help_echo uses this to detect popup menus.  */
      popup_activated_flag = 1;
      /* Process events that apply to the menu.  */
      popup_widget_loop (true, menu);
    }

  unbind_to (specpdl_count, Qnil);

  /* Must reset this manually because the button release event is not passed
     to Emacs event loop. */
  FRAME_DISPLAY_INFO (f)->grabbed = 0;
}

#else /* not USE_GTK */

/* We need a unique id for each widget handled by the Lucid Widget
   library.

   For the main windows, and popup menus, we use this counter, which we
   increment each time after use.  This starts from WIDGET_ID_TICK_START.

   For menu bars, we use numbers starting at 0, counted in
   next_menubar_widget_id.  */
LWLIB_ID widget_id_tick;

static void
popup_selection_callback (Widget widget, LWLIB_ID id, XtPointer client_data)
{
  menu_item_selection = client_data;
}

/* ID is the LWLIB ID of the dialog box.  */

static void
pop_down_menu (int id)
{
  block_input ();
  lw_destroy_all_widgets ((LWLIB_ID) id);
  unblock_input ();
  popup_activated_flag = 0;
}

/* Pop up the menu for frame F defined by FIRST_WV at X/Y and loop until the
   menu pops down.
   menu_item_selection will be set to the selection.  */
static void
create_and_show_popup_menu (struct frame *f, widget_value *first_wv,
			    int x, int y, bool for_click)
{
  int i;
  Arg av[2];
  int ac = 0;
  XEvent dummy;
  XButtonPressedEvent *event = &(dummy.xbutton);
  LWLIB_ID menu_id;
  Widget menu;
  Window dummy_window;

  eassert (FRAME_X_P (f));

#ifdef USE_LUCID
  apply_systemfont_to_menu (f, f->output_data.x->widget);
#endif

  menu_id = widget_id_tick++;
  menu = lw_create_widget ("popup", first_wv->name, menu_id, first_wv,
                           f->output_data.x->widget, true, 0,
                           popup_selection_callback,
                           popup_deactivate_callback,
                           menu_highlight_callback);

  event->type = ButtonPress;
  event->serial = 0;
  event->send_event = false;
  event->display = FRAME_X_DISPLAY (f);
  event->time = CurrentTime;
  event->root = FRAME_DISPLAY_INFO (f)->root_window;
  event->window = event->subwindow = event->root;
  event->x = x;
  event->y = y;

  /* Adjust coordinates to be root-window-relative.  */
  block_input ();
  x += FRAME_LEFT_SCROLL_BAR_AREA_WIDTH (f);
  XTranslateCoordinates (FRAME_X_DISPLAY (f),

                         /* From-window, to-window.  */
                         FRAME_X_WINDOW (f),
                         FRAME_DISPLAY_INFO (f)->root_window,

                         /* From-position, to-position.  */
                         x, y, &x, &y,

                         /* Child of win.  */
                         &dummy_window);
  unblock_input ();

  event->x_root = x;
  event->y_root = y;

  event->state = 0;
  event->button = 0;
  for (i = 0; i < 5; i++)
    if (FRAME_DISPLAY_INFO (f)->grabbed & (1 << i))
      event->button = i;

  /* Don't allow any geometry request from the user.  */
  XtSetArg (av[ac], (char *) XtNgeometry, 0); ac++;
  XtSetValues (menu, av, ac);

  /* Display the menu.  */
  lw_popup_menu (menu, &dummy);
  popup_activated_flag = 1;
  x_activate_timeout_atimer ();

  {
    ptrdiff_t specpdl_count = SPECPDL_INDEX ();

    record_unwind_protect_int (pop_down_menu, (int) menu_id);

    /* Process events that apply to the menu.  */
    popup_get_selection (0, FRAME_DISPLAY_INFO (f), menu_id, true);

    unbind_to (specpdl_count, Qnil);
  }
}

#endif /* not USE_GTK */

static void
cleanup_widget_value_tree (void *arg)
{
  free_menubar_widget_value_tree (arg);
}

Lisp_Object
x_menu_show (struct frame *f, int x, int y, int menuflags,
	     Lisp_Object title, const char **error_name)
{
  int i;
  widget_value *wv, *save_wv = 0, *first_wv = 0, *prev_wv = 0;
  widget_value **submenu_stack
    = alloca (menu_items_used * sizeof *submenu_stack);
  Lisp_Object *subprefix_stack
    = alloca (menu_items_used * sizeof *subprefix_stack);
  int submenu_depth = 0;

  ptrdiff_t specpdl_count = SPECPDL_INDEX ();

  eassert (FRAME_X_P (f));

  *error_name = NULL;

  if (menu_items_used <= MENU_ITEMS_PANE_LENGTH)
    {
      *error_name = "Empty menu";
      return Qnil;
    }

  block_input ();

  /* Create a tree of widget_value objects
     representing the panes and their items.  */
  wv = make_widget_value ("menu", NULL, true, Qnil);
  wv->button_type = BUTTON_TYPE_NONE;
  first_wv = wv;
  bool first_pane = true;

  /* Loop over all panes and items, filling in the tree.  */
  i = 0;
  while (i < menu_items_used)
    {
      if (NILP (AREF (menu_items, i)))
	{
	  submenu_stack[submenu_depth++] = save_wv;
	  save_wv = prev_wv;
	  prev_wv = 0;
	  first_pane = true;
	  i++;
	}
      else if (EQ (AREF (menu_items, i), Qlambda))
	{
	  prev_wv = save_wv;
	  save_wv = submenu_stack[--submenu_depth];
	  first_pane = false;
	  i++;
	}
      else if (EQ (AREF (menu_items, i), Qt)
	       && submenu_depth != 0)
	i += MENU_ITEMS_PANE_LENGTH;
      /* Ignore a nil in the item list.
	 It's meaningful only for dialog boxes.  */
      else if (EQ (AREF (menu_items, i), Qquote))
	i += 1;
      else if (EQ (AREF (menu_items, i), Qt))
	{
	  /* Create a new pane.  */
	  Lisp_Object pane_name, prefix;
	  const char *pane_string;

	  pane_name = AREF (menu_items, i + MENU_ITEMS_PANE_NAME);
	  prefix = AREF (menu_items, i + MENU_ITEMS_PANE_PREFIX);

#ifndef HAVE_MULTILINGUAL_MENU
	  if (STRINGP (pane_name) && STRING_MULTIBYTE (pane_name))
	    {
	      pane_name = ENCODE_MENU_STRING (pane_name);
	      ASET (menu_items, i + MENU_ITEMS_PANE_NAME, pane_name);
	    }
#endif
	  pane_string = (NILP (pane_name)
			 ? "" : SSDATA (pane_name));
	  /* If there is just one top-level pane, put all its items directly
	     under the top-level menu.  */
	  if (menu_items_n_panes == 1)
	    pane_string = "";

	  /* If the pane has a meaningful name,
	     make the pane a top-level menu item
	     with its items as a submenu beneath it.  */
	  if (!(menuflags & MENU_KEYMAPS) && strcmp (pane_string, ""))
	    {
	      wv = make_widget_value (pane_string, NULL, true, Qnil);
	      if (save_wv)
		save_wv->next = wv;
	      else
		first_wv->contents = wv;
	      if ((menuflags & MENU_KEYMAPS) && !NILP (prefix))
		wv->name++;
	      wv->button_type = BUTTON_TYPE_NONE;
	      save_wv = wv;
	      prev_wv = 0;
	    }
	  else if (first_pane)
	    {
	      save_wv = wv;
	      prev_wv = 0;
	    }
	  first_pane = false;
	  i += MENU_ITEMS_PANE_LENGTH;
	}
      else
	{
	  /* Create a new item within current pane.  */
	  Lisp_Object item_name, enable, descrip, def, type, selected, help;
	  item_name = AREF (menu_items, i + MENU_ITEMS_ITEM_NAME);
	  enable = AREF (menu_items, i + MENU_ITEMS_ITEM_ENABLE);
	  descrip = AREF (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY);
	  def = AREF (menu_items, i + MENU_ITEMS_ITEM_DEFINITION);
	  type = AREF (menu_items, i + MENU_ITEMS_ITEM_TYPE);
	  selected = AREF (menu_items, i + MENU_ITEMS_ITEM_SELECTED);
	  help = AREF (menu_items, i + MENU_ITEMS_ITEM_HELP);

#ifndef HAVE_MULTILINGUAL_MENU
          if (STRINGP (item_name) && STRING_MULTIBYTE (item_name))
	    {
	      item_name = ENCODE_MENU_STRING (item_name);
	      ASET (menu_items, i + MENU_ITEMS_ITEM_NAME, item_name);
	    }

          if (STRINGP (descrip) && STRING_MULTIBYTE (descrip))
	    {
	      descrip = ENCODE_MENU_STRING (descrip);
	      ASET (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY, descrip);
	    }
#endif /* not HAVE_MULTILINGUAL_MENU */

	  wv = make_widget_value (SSDATA (item_name), NULL, !NILP (enable),
				  STRINGP (help) ? help : Qnil);
	  if (prev_wv)
	    prev_wv->next = wv;
	  else
	    save_wv->contents = wv;
	  if (!NILP (descrip))
	    wv->key = SSDATA (descrip);
	  /* If this item has a null value,
	     make the call_data null so that it won't display a box
	     when the mouse is on it.  */
	  wv->call_data = !NILP (def) ? aref_addr (menu_items, i) : 0;

	  if (NILP (type))
	    wv->button_type = BUTTON_TYPE_NONE;
	  else if (EQ (type, QCtoggle))
	    wv->button_type = BUTTON_TYPE_TOGGLE;
	  else if (EQ (type, QCradio))
	    wv->button_type = BUTTON_TYPE_RADIO;
	  else
	    emacs_abort ();

	  wv->selected = !NILP (selected);

	  prev_wv = wv;

	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }

  /* Deal with the title, if it is non-nil.  */
  if (!NILP (title))
    {
      widget_value *wv_title;
      widget_value *wv_sep1 = make_widget_value ("--", NULL, false, Qnil);
      widget_value *wv_sep2 = make_widget_value ("--", NULL, false, Qnil);

      wv_sep2->next = first_wv->contents;
      wv_sep1->next = wv_sep2;

#ifndef HAVE_MULTILINGUAL_MENU
      if (STRING_MULTIBYTE (title))
	title = ENCODE_MENU_STRING (title);
#endif

      wv_title = make_widget_value (SSDATA (title), NULL, true, Qnil);
      wv_title->button_type = BUTTON_TYPE_NONE;
      wv_title->next = wv_sep1;
      first_wv->contents = wv_title;
    }

  /* No selection has been chosen yet.  */
  menu_item_selection = 0;

  /* Make sure to free the widget_value objects we used to specify the
     contents even with longjmp.  */
  record_unwind_protect_ptr (cleanup_widget_value_tree, first_wv);

  /* Actually create and show the menu until popped down.  */
  create_and_show_popup_menu (f, first_wv, x, y,
			      menuflags & MENU_FOR_CLICK);

  unbind_to (specpdl_count, Qnil);

  /* Find the selected item, and its pane, to return
     the proper value.  */
  if (menu_item_selection != 0)
    {
      Lisp_Object prefix, entry;

      prefix = entry = Qnil;
      i = 0;
      while (i < menu_items_used)
	{
	  if (NILP (AREF (menu_items, i)))
	    {
	      subprefix_stack[submenu_depth++] = prefix;
	      prefix = entry;
	      i++;
	    }
	  else if (EQ (AREF (menu_items, i), Qlambda))
	    {
	      prefix = subprefix_stack[--submenu_depth];
	      i++;
	    }
	  else if (EQ (AREF (menu_items, i), Qt))
	    {
	      prefix
		= AREF (menu_items, i + MENU_ITEMS_PANE_PREFIX);
	      i += MENU_ITEMS_PANE_LENGTH;
	    }
	  /* Ignore a nil in the item list.
	     It's meaningful only for dialog boxes.  */
	  else if (EQ (AREF (menu_items, i), Qquote))
	    i += 1;
	  else
	    {
	      entry
		= AREF (menu_items, i + MENU_ITEMS_ITEM_VALUE);
	      if (menu_item_selection == aref_addr (menu_items, i))
		{
		  if (menuflags & MENU_KEYMAPS)
		    {
		      int j;

		      entry = list1 (entry);
		      if (!NILP (prefix))
			entry = Fcons (prefix, entry);
		      for (j = submenu_depth - 1; j >= 0; j--)
			if (!NILP (subprefix_stack[j]))
			  entry = Fcons (subprefix_stack[j], entry);
		    }
		  unblock_input ();
		  return entry;
		}
	      i += MENU_ITEMS_ITEM_LENGTH;
	    }
	}
    }
  else if (!(menuflags & MENU_FOR_CLICK))
    {
      unblock_input ();
      /* Make "Cancel" equivalent to C-g.  */
      quit ();
    }

  unblock_input ();
  return Qnil;
}

#ifdef USE_GTK
static void
dialog_selection_callback (GtkWidget *widget, gpointer client_data)
{
  /* Treat the pointer as an integer.  There's no problem
     as long as pointers have enough bits to hold small integers.  */
  if ((intptr_t) client_data != -1)
    menu_item_selection = client_data;

  popup_activated_flag = 0;
}

/* Pop up the dialog for frame F defined by FIRST_WV and loop until the
   dialog pops down.
   menu_item_selection will be set to the selection.  */
static void
create_and_show_dialog (struct frame *f, widget_value *first_wv)
{
  GtkWidget *menu;

  eassert (FRAME_X_P (f));

  menu = xg_create_widget ("dialog", first_wv->name, f, first_wv,
                           G_CALLBACK (dialog_selection_callback),
                           G_CALLBACK (popup_deactivate_callback),
                           0);

  if (menu)
    {
      ptrdiff_t specpdl_count = SPECPDL_INDEX ();
      record_unwind_protect_ptr (pop_down_menu, menu);

      /* Display the menu.  */
      gtk_widget_show_all (menu);

      /* Process events that apply to the menu.  */
      popup_widget_loop (true, menu);

      unbind_to (specpdl_count, Qnil);
    }
}

#else /* not USE_GTK */
static void
dialog_selection_callback (Widget widget, LWLIB_ID id, XtPointer client_data)
{
  /* Treat the pointer as an integer.  There's no problem
     as long as pointers have enough bits to hold small integers.  */
  if ((intptr_t) client_data != -1)
    menu_item_selection = client_data;

  block_input ();
  lw_destroy_all_widgets (id);
  unblock_input ();
  popup_activated_flag = 0;
}


/* Pop up the dialog for frame F defined by FIRST_WV and loop until the
   dialog pops down.
   menu_item_selection will be set to the selection.  */
static void
create_and_show_dialog (struct frame *f, widget_value *first_wv)
{
  LWLIB_ID dialog_id;

  eassert (FRAME_X_P (f));

  dialog_id = widget_id_tick++;
#ifdef USE_LUCID
  apply_systemfont_to_dialog (f->output_data.x->widget);
#endif
  lw_create_widget (first_wv->name, "dialog", dialog_id, first_wv,
                    f->output_data.x->widget, true, 0,
                    dialog_selection_callback, 0, 0);
  lw_modify_all_widgets (dialog_id, first_wv->contents, True);
  /* Display the dialog box.  */
  lw_pop_up_all_widgets (dialog_id);
  popup_activated_flag = 1;
  x_activate_timeout_atimer ();

  /* Process events that apply to the dialog box.
     Also handle timers.  */
  {
    ptrdiff_t count = SPECPDL_INDEX ();

    /* xdialog_show_unwind is responsible for popping the dialog box down.  */

    record_unwind_protect_int (pop_down_menu, (int) dialog_id);

    popup_get_selection (0, FRAME_DISPLAY_INFO (f), dialog_id, true);

    unbind_to (count, Qnil);
  }
}

#endif /* not USE_GTK */

static const char * button_names [] = {
  "button1", "button2", "button3", "button4", "button5",
  "button6", "button7", "button8", "button9", "button10" };

static Lisp_Object
x_dialog_show (struct frame *f, Lisp_Object title,
	       Lisp_Object header, const char **error_name)
{
  int i, nb_buttons=0;
  char dialog_name[6];

  widget_value *wv, *first_wv = 0, *prev_wv = 0;

  /* Number of elements seen so far, before boundary.  */
  int left_count = 0;
  /* Whether we've seen the boundary between left-hand elts and right-hand.  */
  bool boundary_seen = false;

  ptrdiff_t specpdl_count = SPECPDL_INDEX ();

  eassert (FRAME_X_P (f));

  *error_name = NULL;

  if (menu_items_n_panes > 1)
    {
      *error_name = "Multiple panes in dialog box";
      return Qnil;
    }

  /* Create a tree of widget_value objects
     representing the text label and buttons.  */
  {
    Lisp_Object pane_name;
    const char *pane_string;
    pane_name = AREF (menu_items, MENU_ITEMS_PANE_NAME);
    pane_string = (NILP (pane_name)
		   ? "" : SSDATA (pane_name));
    prev_wv = make_widget_value ("message", (char *) pane_string, true, Qnil);
    first_wv = prev_wv;

    /* Loop over all panes and items, filling in the tree.  */
    i = MENU_ITEMS_PANE_LENGTH;
    while (i < menu_items_used)
      {

	/* Create a new item within current pane.  */
	Lisp_Object item_name, enable, descrip;
	item_name = AREF (menu_items, i + MENU_ITEMS_ITEM_NAME);
	enable = AREF (menu_items, i + MENU_ITEMS_ITEM_ENABLE);
	descrip
	  = AREF (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY);

	if (NILP (item_name))
	  {
	    free_menubar_widget_value_tree (first_wv);
	    *error_name = "Submenu in dialog items";
	    return Qnil;
	  }
	if (EQ (item_name, Qquote))
	  {
	    /* This is the boundary between left-side elts
	       and right-side elts.  Stop incrementing right_count.  */
	    boundary_seen = true;
	    i++;
	    continue;
	  }
	if (nb_buttons >= 9)
	  {
	    free_menubar_widget_value_tree (first_wv);
	    *error_name = "Too many dialog items";
	    return Qnil;
	  }

	wv = make_widget_value (button_names[nb_buttons],
				SSDATA (item_name),
				!NILP (enable), Qnil);
	prev_wv->next = wv;
	if (!NILP (descrip))
	  wv->key = SSDATA (descrip);
	wv->call_data = aref_addr (menu_items, i);
	prev_wv = wv;

	if (! boundary_seen)
	  left_count++;

	nb_buttons++;
	i += MENU_ITEMS_ITEM_LENGTH;
      }

    /* If the boundary was not specified,
       by default put half on the left and half on the right.  */
    if (! boundary_seen)
      left_count = nb_buttons - nb_buttons / 2;

    wv = make_widget_value (dialog_name, NULL, false, Qnil);

    /*  Frame title: 'Q' = Question, 'I' = Information.
        Can also have 'E' = Error if, one day, we want
        a popup for errors. */
    if (NILP (header))
      dialog_name[0] = 'Q';
    else
      dialog_name[0] = 'I';

    /* Dialog boxes use a really stupid name encoding
       which specifies how many buttons to use
       and how many buttons are on the right. */
    dialog_name[1] = '0' + nb_buttons;
    dialog_name[2] = 'B';
    dialog_name[3] = 'R';
    /* Number of buttons to put on the right.  */
    dialog_name[4] = '0' + nb_buttons - left_count;
    dialog_name[5] = 0;
    wv->contents = first_wv;
    first_wv = wv;
  }

  /* No selection has been chosen yet.  */
  menu_item_selection = 0;

  /* Make sure to free the widget_value objects we used to specify the
     contents even with longjmp.  */
  record_unwind_protect_ptr (cleanup_widget_value_tree, first_wv);

  /* Actually create and show the dialog.  */
  create_and_show_dialog (f, first_wv);

  unbind_to (specpdl_count, Qnil);

  /* Find the selected item, and its pane, to return
     the proper value.  */
  if (menu_item_selection != 0)
    {
      i = 0;
      while (i < menu_items_used)
	{
	  Lisp_Object entry;

	  if (EQ (AREF (menu_items, i), Qt))
	    i += MENU_ITEMS_PANE_LENGTH;
	  else if (EQ (AREF (menu_items, i), Qquote))
	    {
	      /* This is the boundary between left-side elts and
		 right-side elts.  */
	      ++i;
	    }
	  else
	    {
	      entry
		= AREF (menu_items, i + MENU_ITEMS_ITEM_VALUE);
	      if (menu_item_selection == aref_addr (menu_items, i))
		return entry;
	      i += MENU_ITEMS_ITEM_LENGTH;
	    }
	}
    }
  else
    /* Make "Cancel" equivalent to C-g.  */
    quit ();

  return Qnil;
}

Lisp_Object
xw_popup_dialog (struct frame *f, Lisp_Object header, Lisp_Object contents)
{
  Lisp_Object title;
  const char *error_name;
  Lisp_Object selection;
  ptrdiff_t specpdl_count = SPECPDL_INDEX ();

  check_window_system (f);

  /* Decode the dialog items from what was specified.  */
  title = Fcar (contents);
  CHECK_STRING (title);
  record_unwind_protect_void (unuse_menu_items);

  if (NILP (Fcar (Fcdr (contents))))
    /* No buttons specified, add an "Ok" button so users can pop down
       the dialog.  Also, the lesstif/motif version crashes if there are
       no buttons.  */
    contents = list2 (title, Fcons (build_string ("Ok"), Qt));

  list_of_panes (list1 (contents));

  /* Display them in a dialog box.  */
  block_input ();
  selection = x_dialog_show (f, title, header, &error_name);
  unblock_input ();

  unbind_to (specpdl_count, Qnil);
  discard_menu_items ();

  if (error_name) error ("%s", error_name);
  return selection;
}

#else /* not USE_X_TOOLKIT && not USE_GTK */

/* The frame of the last activated non-toolkit menu bar.
   Used to generate menu help events.  */

static struct frame *menu_help_frame;


/* Show help HELP_STRING, or clear help if HELP_STRING is null.

   PANE is the pane number, and ITEM is the menu item number in
   the menu (currently not used).

   This cannot be done with generating a HELP_EVENT because
   XMenuActivate contains a loop that doesn't let Emacs process
   keyboard events.  */

static void
menu_help_callback (char const *help_string, int pane, int item)
{
  Lisp_Object *first_item;
  Lisp_Object pane_name;
  Lisp_Object menu_object;

  first_item = XVECTOR (menu_items)->contents;
  if (EQ (first_item[0], Qt))
    pane_name = first_item[MENU_ITEMS_PANE_NAME];
  else if (EQ (first_item[0], Qquote))
    /* This shouldn't happen, see x_menu_show.  */
    pane_name = empty_unibyte_string;
  else
    pane_name = first_item[MENU_ITEMS_ITEM_NAME];

  /* (menu-item MENU-NAME PANE-NUMBER)  */
  menu_object = list3 (Qmenu_item, pane_name, make_fixnum (pane));
  show_help_echo (help_string ? build_string (help_string) : Qnil,
 		  Qnil, menu_object, make_fixnum (item));
}

struct pop_down_menu
{
  struct frame *frame;
  XMenu *menu;
};

static void
pop_down_menu (void *arg)
{
  struct pop_down_menu *data = arg;
  struct frame *f = data->frame;
  XMenu *menu = data->menu;

  block_input ();
#ifndef MSDOS
  XUngrabPointer (FRAME_X_DISPLAY (f), CurrentTime);
  XUngrabKeyboard (FRAME_X_DISPLAY (f), CurrentTime);
#endif
  XMenuDestroy (FRAME_X_DISPLAY (f), menu);

#ifdef HAVE_X_WINDOWS
  /* Assume the mouse has moved out of the X window.
     If it has actually moved in, we will get an EnterNotify.  */
  x_mouse_leave (FRAME_DISPLAY_INFO (f));

  /* State that no mouse buttons are now held.
     (The oldXMenu code doesn't track this info for us.)
     That is not necessarily true, but the fiction leads to reasonable
     results, and it is a pain to ask which are actually held now.  */
  FRAME_DISPLAY_INFO (f)->grabbed = 0;

#endif /* HAVE_X_WINDOWS */

  unblock_input ();
}


Lisp_Object
x_menu_show (struct frame *f, int x, int y, int menuflags,
	     Lisp_Object title, const char **error_name)
{
  Window root;
  XMenu *menu;
  int pane, selidx, lpane, status;
  Lisp_Object entry = Qnil;
  Lisp_Object pane_prefix;
  char *datap;
  int ulx, uly, width, height;
  int dispwidth, dispheight;
  int i, j, lines, maxlines;
  int maxwidth;
  int dummy_int;
  unsigned int dummy_uint;
  ptrdiff_t specpdl_count = SPECPDL_INDEX ();

  eassert (FRAME_X_P (f) || FRAME_MSDOS_P (f));

  *error_name = 0;
  if (menu_items_n_panes == 0)
    return Qnil;

  if (menu_items_used <= MENU_ITEMS_PANE_LENGTH)
    {
      *error_name = "Empty menu";
      return Qnil;
    }

  USE_SAFE_ALLOCA;
  block_input ();

  /* Figure out which root window F is on.  */
  XGetGeometry (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), &root,
		&dummy_int, &dummy_int, &dummy_uint, &dummy_uint,
		&dummy_uint, &dummy_uint);

  /* Make the menu on that window.  */
  menu = XMenuCreate (FRAME_X_DISPLAY (f), root, "emacs");
  if (menu == NULL)
    {
      *error_name = "Can't create menu";
      goto return_entry;
    }

  /* Don't GC while we prepare and show the menu,
     because we give the oldxmenu library pointers to the
     contents of strings.  */
  inhibit_garbage_collection ();

#ifdef HAVE_X_WINDOWS
  {
    /* Adjust coordinates to relative to the outer (window manager) window. */
    int left_off, top_off;

    x_real_pos_and_offsets (f, &left_off, NULL, &top_off, NULL,
                            NULL, NULL, NULL, NULL, NULL);

    x += left_off;
    y += top_off;
  }
#endif /* HAVE_X_WINDOWS */

  x += f->left_pos;
  y += f->top_pos;

  /* Create all the necessary panes and their items.  */
  maxwidth = maxlines = lines = i = 0;
  lpane = XM_FAILURE;
  while (i < menu_items_used)
    {
      if (EQ (AREF (menu_items, i), Qt))
	{
	  /* Create a new pane.  */
	  Lisp_Object pane_name, prefix;
	  const char *pane_string;

          maxlines = max (maxlines, lines);
          lines = 0;
	  pane_name = AREF (menu_items, i + MENU_ITEMS_PANE_NAME);
	  prefix = AREF (menu_items, i + MENU_ITEMS_PANE_PREFIX);
	  pane_string = (NILP (pane_name)
			 ? "" : SSDATA (pane_name));
	  if ((menuflags & MENU_KEYMAPS) && !NILP (prefix))
	    pane_string++;

	  lpane = XMenuAddPane (FRAME_X_DISPLAY (f), menu, pane_string, true);
	  if (lpane == XM_FAILURE)
	    {
	      XMenuDestroy (FRAME_X_DISPLAY (f), menu);
	      *error_name = "Can't create pane";
	      goto return_entry;
	    }
	  i += MENU_ITEMS_PANE_LENGTH;

	  /* Find the width of the widest item in this pane.  */
	  j = i;
	  while (j < menu_items_used)
	    {
	      Lisp_Object item;
	      item = AREF (menu_items, j);
	      if (EQ (item, Qt))
		break;
	      if (NILP (item))
		{
		  j++;
		  continue;
		}
	      width = SBYTES (item);
	      if (width > maxwidth)
		maxwidth = width;

	      j += MENU_ITEMS_ITEM_LENGTH;
	    }
	}
      /* Ignore a nil in the item list.
	 It's meaningful only for dialog boxes.  */
      else if (EQ (AREF (menu_items, i), Qquote))
	i += 1;
      else
	{
	  /* Create a new item within current pane.  */
	  Lisp_Object item_name, enable, descrip, help;
	  char *item_data;
	  char const *help_string;

	  item_name = AREF (menu_items, i + MENU_ITEMS_ITEM_NAME);
	  enable = AREF (menu_items, i + MENU_ITEMS_ITEM_ENABLE);
	  descrip
	    = AREF (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY);
	  help = AREF (menu_items, i + MENU_ITEMS_ITEM_HELP);
	  help_string = STRINGP (help) ? SSDATA (help) : NULL;

	  if (!NILP (descrip))
	    {
	      item_data = SAFE_ALLOCA (maxwidth + SBYTES (descrip) + 1);
	      memcpy (item_data, SSDATA (item_name), SBYTES (item_name));
	      for (j = SCHARS (item_name); j < maxwidth; j++)
		item_data[j] = ' ';
	      memcpy (item_data + j, SSDATA (descrip), SBYTES (descrip));
	      item_data[j + SBYTES (descrip)] = 0;
	    }
	  else
	    item_data = SSDATA (item_name);

	  if (lpane == XM_FAILURE
	      || (XMenuAddSelection (FRAME_X_DISPLAY (f),
				     menu, lpane, 0, item_data,
				     !NILP (enable), help_string)
		  == XM_FAILURE))
	    {
	      XMenuDestroy (FRAME_X_DISPLAY (f), menu);
	      *error_name = "Can't add selection to menu";
	      goto return_entry;
	    }
	  i += MENU_ITEMS_ITEM_LENGTH;
          lines++;
	}
    }

  maxlines = max (maxlines, lines);

  /* All set and ready to fly.  */
  XMenuRecompute (FRAME_X_DISPLAY (f), menu);
  dispwidth = DisplayWidth (FRAME_X_DISPLAY (f), FRAME_X_SCREEN_NUMBER (f));
  dispheight = DisplayHeight (FRAME_X_DISPLAY (f), FRAME_X_SCREEN_NUMBER (f));
  x = min (x, dispwidth);
  y = min (y, dispheight);
  x = max (x, 1);
  y = max (y, 1);
  XMenuLocate (FRAME_X_DISPLAY (f), menu, 0, 0, x, y,
	       &ulx, &uly, &width, &height);
  if (ulx+width > dispwidth)
    {
      x -= (ulx + width) - dispwidth;
      ulx = dispwidth - width;
    }
  if (uly+height > dispheight)
    {
      y -= (uly + height) - dispheight;
      uly = dispheight - height;
    }
#ifndef HAVE_X_WINDOWS
  if (FRAME_HAS_MINIBUF_P (f) && uly+height > dispheight - 1)
    {
      /* Move the menu away of the echo area, to avoid overwriting the
	 menu with help echo messages or vice versa.  */
      if (BUFFERP (echo_area_buffer[0]) && WINDOWP (echo_area_window))
	{
	  y -= WINDOW_TOTAL_LINES (XWINDOW (echo_area_window));
	  uly -= WINDOW_TOTAL_LINES (XWINDOW (echo_area_window));
	}
      else
	{
	  y--;
	  uly--;
	}
    }
#endif
  if (ulx < 0) x -= ulx;
  if (uly < 0) y -= uly;

  if (!(menuflags & MENU_FOR_CLICK))
    {
      /* If position was not given by a mouse click, adjust so upper left
         corner of the menu as a whole ends up at given coordinates.  This
         is what x-popup-menu says in its documentation.  */
      x += width/2;
      y += 1.5*height/(maxlines+2);
    }

  XMenuSetAEQ (menu, true);
  XMenuSetFreeze (menu, true);
  pane = selidx = 0;

#ifndef MSDOS
  XMenuActivateSetWaitFunction (x_menu_wait_for_event, FRAME_X_DISPLAY (f));
#endif

  record_unwind_protect_ptr (pop_down_menu,
			     &(struct pop_down_menu) {f, menu});

  /* Help display under X won't work because XMenuActivate contains
     a loop that doesn't give Emacs a chance to process it.  */
  menu_help_frame = f;
  status = XMenuActivate (FRAME_X_DISPLAY (f), menu, &pane, &selidx,
                          x, y, ButtonReleaseMask, &datap,
                          menu_help_callback);
  pane_prefix = Qnil;

  switch (status)
    {
    case XM_SUCCESS:
#ifdef XDEBUG
      fprintf (stderr, "pane= %d line = %d\n", panes, selidx);
#endif

      /* Find the item number SELIDX in pane number PANE.  */
      i = 0;
      while (i < menu_items_used)
	{
	  if (EQ (AREF (menu_items, i), Qt))
	    {
	      if (pane == 0)
		pane_prefix
		  = AREF (menu_items, i + MENU_ITEMS_PANE_PREFIX);
	      pane--;
	      i += MENU_ITEMS_PANE_LENGTH;
	    }
	  else
	    {
	      if (pane == -1)
		{
		  if (selidx == 0)
		    {
		      entry
			= AREF (menu_items, i + MENU_ITEMS_ITEM_VALUE);
		      if (menuflags & MENU_KEYMAPS)
			{
			  entry = list1 (entry);
			  if (!NILP (pane_prefix))
			    entry = Fcons (pane_prefix, entry);
			}
		      break;
		    }
		  selidx--;
		}
	      i += MENU_ITEMS_ITEM_LENGTH;
	    }
	}
      break;

    case XM_FAILURE:
      *error_name = "Can't activate menu";
    case XM_IA_SELECT:
      break;
    case XM_NO_SELECT:
      /* Make "Cancel" equivalent to C-g unless FOR_CLICK (which means
	 the menu was invoked with a mouse event as POSITION).  */
      if (!(menuflags & MENU_FOR_CLICK))
	{
	  unblock_input ();
	  quit ();
	}
      break;
    }

 return_entry:
  unblock_input ();
  return SAFE_FREE_UNBIND_TO (specpdl_count, entry);
}

#endif /* not USE_X_TOOLKIT */

#ifndef MSDOS
/* Detect if a dialog or menu has been posted.  MSDOS has its own
   implementation on msdos.c.  */

int
popup_activated (void)
{
  return popup_activated_flag;
}
#endif	/* not MSDOS */

/* The following is used by delayed window autoselection.  */

DEFUN ("menu-or-popup-active-p", Fmenu_or_popup_active_p, Smenu_or_popup_active_p, 0, 0, 0,
       doc: /* Return t if a menu or popup dialog is active.
\(On MS Windows, this refers to the selected frame.)  */)
  (void)
{
  return (popup_activated ()) ? Qt : Qnil;
}


static void syms_of_xmenu_for_pdumper (void);

void
syms_of_xmenu (void)
{
  DEFSYM (Qdebug_on_next_call, "debug-on-next-call");
  defsubr (&Smenu_or_popup_active_p);

#ifdef USE_GTK
  DEFSYM (Qframe_monitor_workarea, "frame-monitor-workarea");
#endif

#if defined (USE_GTK) || defined (USE_X_TOOLKIT)
  defsubr (&Sx_menu_bar_open_internal);
  Ffset (intern_c_string ("accelerate-menu"),
	 intern_c_string (Sx_menu_bar_open_internal.s.symbol_name));
#endif

  pdumper_do_now_and_after_load (syms_of_xmenu_for_pdumper);
}

static void
syms_of_xmenu_for_pdumper (void)
{
#ifdef USE_X_TOOLKIT
  enum { WIDGET_ID_TICK_START = 1 << 16 };
  widget_id_tick = WIDGET_ID_TICK_START;
  next_menubar_widget_id = 1;
#endif
}
