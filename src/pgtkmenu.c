/* Pure GTK3 menu and toolbar module.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.

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

/*
 */


/* This should be the first include, as it may set up #defines affecting
   interpretation of even the system includes.  */
#include <config.h>

#include "lisp.h"
#include "frame.h"
#include "window.h"
#include "character.h"
#include "buffer.h"
#include "keymap.h"
#include "coding.h"
#include "commands.h"
#include "blockinput.h"
#include "termhooks.h"
#include "keyboard.h"
#include "menu.h"
#include "pdumper.h"
#include "xgselect.h"

#include "gtkutil.h"
#include <gtk/gtk.h>

/* Flag which when set indicates a dialog or menu has been posted by
   GTK on behalf of one of the widget sets.  */
static int popup_activated_flag;

/* Set menu_items_inuse so no other popup menu or dialog is created.  */

void
pgtk_menu_set_in_use (bool in_use)
{
  Lisp_Object frames, frame;

  menu_items_inuse = in_use;
  popup_activated_flag = in_use;

  /* Don't let frames in `above' z-group obscure popups.  */
  FOR_EACH_FRAME (frames, frame)
  {
    struct frame *f = XFRAME (frame);

    if (in_use && FRAME_Z_GROUP_ABOVE (f))
      pgtk_set_z_group (f, Qabove_suspended, Qabove);
    else if (!in_use && FRAME_Z_GROUP_ABOVE_SUSPENDED (f))
      pgtk_set_z_group (f, Qabove, Qabove_suspended);
  }
}

DEFUN ("x-menu-bar-open-internal", Fx_menu_bar_open_internal, Sx_menu_bar_open_internal, 0, 1, "i",
       doc: /* SKIP: real doc in USE_GTK definition in xmenu.c.  */)
  (Lisp_Object frame)
{
  GtkWidget *menubar;
  struct frame *f;

  block_input ();
  f = decode_window_system_frame (frame);

  if (FRAME_EXTERNAL_MENU_BAR (f))
    set_frame_menubar (f, true);

  menubar = FRAME_X_OUTPUT (f)->menubar_widget;
  if (menubar)
    {
      /* Activate the first menu.  */
      GList *children = gtk_container_get_children (GTK_CONTAINER (menubar));

      if (children)
	{
	  g_signal_emit_by_name (children->data, "activate_item");
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
    gtk_main_iteration ();
}

void
pgtk_activate_menubar (struct frame *f)
{
  set_frame_menubar (f, true);

  popup_activated_flag = 1;

  /* f->output_data.pgtk->menubar_active = 1; */
}

/* This callback is invoked when a dialog or menu is finished being
   used and has been unposted.  */

static void
popup_deactivate_callback (GtkWidget *widget, gpointer client_data)
{
  pgtk_menu_set_in_use (false);
}

/* Function that finds the frame for WIDGET and shows the HELP text
   for that widget.
   F is the frame if known, or NULL if not known.  */
static void
show_help_event (struct frame *f, GtkWidget *widget, Lisp_Object help)
{
  /* Don't show help echo on PGTK, as tooltips are always transient
     for the main widget, so on Wayland the menu will display above
     and obscure the tooltip.  FIXME: this is some low hanging fruit
     for fixing.  After you fix Fx_show_tip in pgtkterm.c so that it
     can display tooltips above menus, copy the definition of this
     function from xmenu.c.

     As a workaround, GTK is used to display menu tooltips, outside
     the Emacs help echo machinery.  */
}

/* Callback called when menu items are highlighted/unhighlighted
   while moving the mouse over them.  WIDGET is the menu bar or menu
   popup widget.  ID is its LWLIB_ID.  CALL_DATA contains a pointer to
   the data structure for the menu item, or null in case of
   unhighlighting.  */

static void
menu_highlight_callback (GtkWidget *widget, gpointer call_data)
{
  xg_menu_item_cb_data *cb_data;
  Lisp_Object help;

  cb_data = g_object_get_data (G_OBJECT (widget), XG_ITEM_DATA);
  if (!cb_data)
    return;

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

  if (!cb_data || !cb_data->cl_data || !cb_data->cl_data->f)
    return;

  /* For a group of radio buttons, GTK calls the selection callback first
     for the item that was active before the selection and then for the one that
     is active after the selection.  For C-h k this means we get the help on
     the deselected item and then the selected item is executed.  Prevent that
     by ignoring the non-active item.  */
  if (GTK_IS_RADIO_MENU_ITEM (widget)
      && !gtk_check_menu_item_get_active (GTK_CHECK_MENU_ITEM (widget)))
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

/* Recompute all the widgets of frame F, when the menu bar has been
   changed.  */

static void
update_frame_menubar (struct frame *f)
{
  xg_update_frame_menubar (f);
}

/* Set the contents of the menubar widgets of frame F.
   The argument FIRST_TIME is currently ignored;
   it is set the first time this is called, from initialize_frame_menubar.  */

void
set_frame_menubar (struct frame *f, bool deep_p)
{
  GtkWidget *menubar_widget;
  Lisp_Object items;
  widget_value *wv, *first_wv, *prev_wv = 0;
  int i;
  int *submenu_start, *submenu_end;
  bool *submenu_top_level_items;
  int *submenu_n_panes;


  menubar_widget = f->output_data.pgtk->menubar_widget;

  XSETFRAME (Vmenu_updating_frame, f);

  if (!menubar_widget)
    deep_p = true;

  if (deep_p)
    {
      struct buffer *prev = current_buffer;
      Lisp_Object buffer;
      specpdl_ref specpdl_count = SPECPDL_INDEX ();
      int previous_menu_items_used = f->menu_bar_items_used;
      Lisp_Object *previous_items
	= alloca (previous_menu_items_used * sizeof *previous_items);
      int subitems;

      /* If we are making a new widget, its contents are empty,
         do always reinitialize them.  */
      if (!menubar_widget)
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
      safe_run_hooks (Qmenu_bar_update_hook);
      fset_menu_bar_items (f, menu_bar_items (FRAME_MENU_BAR_ITEMS (f)));

      items = FRAME_MENU_BAR_ITEMS (f);

      /* Save the frame's previous menu bar contents data.  */
      if (previous_menu_items_used)
	memcpy (previous_items, xvector_contents (f->menu_bar_vector),
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

  block_input ();

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

      f->output_data.pgtk->menubar_widget = menubar_widget;
    }

  free_menubar_widget_value_tree (first_wv);
  update_frame_menubar (f);

  xg_crazy_callback_abort = false;

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
  set_frame_menubar (f, true);
}


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

/* The item selected in the popup menu.  */
static Lisp_Object *volatile menu_item_selection;

static void
popup_selection_callback (GtkWidget *widget, gpointer client_data)
{
  xg_menu_item_cb_data *cb_data = client_data;

  if (xg_crazy_callback_abort)
    return;
  if (cb_data)
    menu_item_selection = cb_data->call_data;
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
create_and_show_popup_menu (struct frame *f, widget_value * first_wv,
			    int x, int y, bool for_click)
{
  GtkWidget *menu;
  specpdl_ref specpdl_count = SPECPDL_INDEX ();

  eassert (FRAME_PGTK_P (f));

  xg_crazy_callback_abort = true;
  menu = xg_create_widget ("popup", first_wv->name, f, first_wv,
			   G_CALLBACK (popup_selection_callback),
			   G_CALLBACK (popup_deactivate_callback),
			   G_CALLBACK (menu_highlight_callback));
  xg_crazy_callback_abort = false;

  /* Display the menu.  */
  gtk_widget_show_all (menu);

  if (for_click)
    gtk_menu_popup_at_pointer (GTK_MENU (menu),
			       FRAME_DISPLAY_INFO (f)->last_click_event);
  else
    {
      GdkRectangle rect;
      rect.x = x;
      rect.y = y;
      rect.width = 1;
      rect.height = 1;
      gtk_menu_popup_at_rect (GTK_MENU (menu),
			      gtk_widget_get_window (FRAME_GTK_WIDGET (f)),
			      &rect,
			      GDK_GRAVITY_NORTH_WEST, GDK_GRAVITY_NORTH_WEST,
			      FRAME_DISPLAY_INFO (f)->last_click_event);
    }

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

static void
cleanup_widget_value_tree (void *arg)
{
  free_menubar_widget_value_tree (arg);
}

Lisp_Object
pgtk_menu_show (struct frame *f, int x, int y, int menuflags,
		Lisp_Object title, const char **error_name)
{
  int i;
  widget_value *wv, *save_wv = 0, *first_wv = 0, *prev_wv = 0;
  widget_value **submenu_stack
    = alloca (menu_items_used * sizeof *submenu_stack);
  Lisp_Object *subprefix_stack
    = alloca (menu_items_used * sizeof *subprefix_stack);
  int submenu_depth = 0;

  specpdl_ref specpdl_count = SPECPDL_INDEX ();

  eassert (FRAME_PGTK_P (f));

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
      else if (EQ (AREF (menu_items, i), Qt) && submenu_depth != 0)
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
	  pane_string = (NILP (pane_name) ? "" : SSDATA (pane_name));
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
  create_and_show_popup_menu (f, first_wv, x, y, menuflags & MENU_FOR_CLICK);

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
	      prefix = AREF (menu_items, i + MENU_ITEMS_PANE_PREFIX);
	      i += MENU_ITEMS_PANE_LENGTH;
	    }
	  /* Ignore a nil in the item list.
	     It's meaningful only for dialog boxes.  */
	  else if (EQ (AREF (menu_items, i), Qquote))
	    i += 1;
	  else
	    {
	      entry = AREF (menu_items, i + MENU_ITEMS_ITEM_VALUE);
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

  eassert (FRAME_PGTK_P (f));

  menu = xg_create_widget ("dialog", first_wv->name, f, first_wv,
			   G_CALLBACK (dialog_selection_callback),
			   G_CALLBACK (popup_deactivate_callback), 0);

  if (menu)
    {
      specpdl_ref specpdl_count = SPECPDL_INDEX ();
      record_unwind_protect_ptr (pop_down_menu, menu);

      /* Display the menu.  */
      gtk_widget_show_all (menu);

      /* Process events that apply to the menu.  */
      popup_widget_loop (true, menu);

      unbind_to (specpdl_count, Qnil);
    }
}

static const char *button_names[] = {
  "button1", "button2", "button3", "button4", "button5",
  "button6", "button7", "button8", "button9", "button10"
};

Lisp_Object
pgtk_dialog_show (struct frame *f, Lisp_Object title,
		  Lisp_Object header, const char **error_name)
{
  int i, nb_buttons = 0;
  char dialog_name[6];

  widget_value *wv, *first_wv = 0, *prev_wv = 0;

  /* Number of elements seen so far, before boundary.  */
  int left_count = 0;
  /* Whether we've seen the boundary between left-hand elts and right-hand.  */
  bool boundary_seen = false;

  specpdl_ref specpdl_count = SPECPDL_INDEX ();

  eassert (FRAME_PGTK_P (f));

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
    pane_string = (NILP (pane_name) ? "" : SSDATA (pane_name));
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
	descrip = AREF (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY);

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
				SSDATA (item_name), !NILP (enable), Qnil);
	prev_wv->next = wv;
	if (!NILP (descrip))
	  wv->key = SSDATA (descrip);
	wv->call_data = aref_addr (menu_items, i);
	prev_wv = wv;

	if (!boundary_seen)
	  left_count++;

	nb_buttons++;
	i += MENU_ITEMS_ITEM_LENGTH;
      }

    /* If the boundary was not specified,
       by default put half on the left and half on the right.  */
    if (!boundary_seen)
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
	      entry = AREF (menu_items, i + MENU_ITEMS_ITEM_VALUE);
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
pgtk_popup_dialog (struct frame *f, Lisp_Object header, Lisp_Object contents)
{
  Lisp_Object title;
  const char *error_name;
  Lisp_Object selection;
  specpdl_ref specpdl_count = SPECPDL_INDEX ();

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
  selection = pgtk_dialog_show (f, title, header, &error_name);
  unblock_input ();

  unbind_to (specpdl_count, Qnil);
  discard_menu_items ();

  if (error_name)
    error ("%s", error_name);
  return selection;
}

/* Detect if a dialog or menu has been posted.  MSDOS has its own
   implementation on msdos.c.  */

int
popup_activated (void)
{
  return popup_activated_flag;
}

/* The following is used by delayed window autoselection.  */

DEFUN ("menu-or-popup-active-p", Fmenu_or_popup_active_p, Smenu_or_popup_active_p, 0, 0, 0,
       doc: /* Return t if a menu or popup dialog is active.
\(On MS Windows, this refers to the selected frame.)  */)
  (void)
{
  return (popup_activated ())? Qt : Qnil;
}

static void syms_of_pgtkmenu_for_pdumper (void);

void
syms_of_pgtkmenu (void)
{
  DEFSYM (Qdebug_on_next_call, "debug-on-next-call");
  defsubr (&Smenu_or_popup_active_p);

  DEFSYM (Qframe_monitor_workarea, "frame-monitor-workarea");

  defsubr (&Sx_menu_bar_open_internal);
  Ffset (intern_c_string ("accelerate-menu"),
	 intern_c_string (Sx_menu_bar_open_internal.s.symbol_name));

  pdumper_do_now_and_after_load (syms_of_pgtkmenu_for_pdumper);
}

static void
syms_of_pgtkmenu_for_pdumper (void)
{
}
