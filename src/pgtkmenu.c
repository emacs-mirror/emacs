/* Pure GTK3 menu and toolbar module.
   Copyright (C) 2019 Free Software Foundation, Inc.

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

#include "gtkutil.h"
#include <gtk/gtk.h>


Lisp_Object
pgtk_popup_dialog (struct frame *f, Lisp_Object header, Lisp_Object contents)
{
  return Qnil;
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

static void
menu_highlight_callback (GtkWidget *widget, gpointer call_data)
{
  xg_menu_item_cb_data *cb_data;
  Lisp_Object help;

  cb_data = g_object_get_data (G_OBJECT (widget), XG_ITEM_DATA);
  if (! cb_data) return;

  help = call_data ? cb_data->help : Qnil;
}


/* This callback is invoked when a dialog or menu is finished being
   used and has been unposted.  */

static void
popup_deactivate_callback (GtkWidget *widget, gpointer client_data)
{
}




/* Set the contents of the menubar widgets of frame F.
   The argument FIRST_TIME is currently ignored;
   it is set the first time this is called, from initialize_frame_menubar.  */

void
set_frame_menubar (struct frame *f, bool first_time, bool deep_p)
{
  GtkWidget * menubar_widget;
  Lisp_Object items;
  widget_value *wv, *first_wv, *prev_wv = 0;
  int i;
  int *submenu_start, *submenu_end;
  bool *submenu_top_level_items;
  int *submenu_n_panes;


  menubar_widget = f->output_data.pgtk->menubar_widget;

  XSETFRAME(Vmenu_updating_frame, f);

  if (! menubar_widget)
    deep_p = true;

  if (deep_p)
    {
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

  block_input();

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
  xg_update_frame_menubar (f);

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
  set_frame_menubar (f, true, true);
}


void pgtk_activate_menubar (struct frame *f)
{
  set_frame_menubar(f, false, false);

  /* f->output_data.pgtk->menubar_active = 1; */
}


Lisp_Object
pgtk_menu_show (struct frame *f, int x, int y, int menuflags,
	     Lisp_Object title, const char **error_name)
{
  Lisp_Object tem;

  block_input ();


  unblock_input ();

  // not implemented.
  return Qnil;
}

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
          g_list_free (children);
        }
    }
  unblock_input ();

  return Qnil;
}


static const char * button_names [] = {
  "button1", "button2", "button3", "button4", "button5",
  "button6", "button7", "button8", "button9", "button10" };

extern Lisp_Object
pgtk_dialog_show (struct frame *f, Lisp_Object title,
		 Lisp_Object header, char **error)
{
  return Qnil;
}

/* The following is used by delayed window autoselection.  */

DEFUN ("menu-or-popup-active-p", Fmenu_or_popup_active_p, Smenu_or_popup_active_p, 0, 0, 0,
       doc: /* SKIP: real doc in xmenu.c.  */)
  (void)
{
  struct frame *f;
  f = SELECTED_FRAME ();
  //  return (f->output_data.pgtk->menubar_active > 0) ? Qt : Qnil;
  return Qnil;
}

void
syms_of_pgtkmenu (void)
{
  // current_popup_menu = NULL;
  // PDUMPER_IGNORE (current_popup_menu);

  DEFSYM (Qdebug_on_next_call, "debug-on-next-call");
  DEFSYM (Qunsupported__w32_dialog, "unsupported--w32-dialog");

  defsubr (&Smenu_or_popup_active_p);
}
