/* Haiku window system support
   Copyright (C) 2021-2024 Free Software Foundation, Inc.

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
#include "frame.h"
#include "keyboard.h"
#include "menu.h"
#include "buffer.h"
#include "blockinput.h"

#include "haikuterm.h"
#include "haiku_support.h"

static Lisp_Object *volatile menu_item_selection;
static struct timespec menu_timer_timespec;

int popup_activated_p = 0;

static void
digest_menu_items (void *first_menu, int start, int menu_items_used,
		   bool is_menu_bar)
{
  void **menus, **panes;
  ssize_t menu_len;
  ssize_t pane_len;
  int i, menu_depth;
  void *menu, *window, *view;
  Lisp_Object pane_name, prefix;
  const char *pane_string;
  Lisp_Object item_name, enable, descrip, def, selected, help;

  USE_SAFE_ALLOCA;

  menu_len = (menu_items_used + 1 - start) * sizeof *menus;
  pane_len = (menu_items_used + 1 - start) * sizeof *panes;
  menu = first_menu;

  i = start;
  menu_depth = 0;

  menus = SAFE_ALLOCA (menu_len);
  panes = SAFE_ALLOCA (pane_len);
  memset (menus, 0, menu_len);
  memset (panes, 0, pane_len);
  menus[0] = first_menu;

  window = NULL;
  view = NULL;

  if (FRAMEP (Vmenu_updating_frame) &&
      FRAME_LIVE_P (XFRAME (Vmenu_updating_frame)) &&
      FRAME_HAIKU_P (XFRAME (Vmenu_updating_frame)))
    {
      window = FRAME_HAIKU_WINDOW (XFRAME (Vmenu_updating_frame));
      view = FRAME_HAIKU_VIEW (XFRAME (Vmenu_updating_frame));
    }

  if (view)
    BView_draw_lock (view, false, 0, 0, 0, 0);

  while (i < menu_items_used)
    {
      if (NILP (AREF (menu_items, i)))
	{
	  menus[++menu_depth] = menu;
	  i++;
	}
      else if (EQ (AREF (menu_items, i), Qlambda))
	{
	  panes[menu_depth] = NULL;
	  menu = panes[--menu_depth] ? panes[menu_depth] : menus[menu_depth];
	  i++;
	}
      else if (EQ (AREF (menu_items, i), Qquote))
	i += 1;
      else if (EQ (AREF (menu_items, i), Qt))
	{
	  if (menu_items_n_panes == 1)
	    {
	      i += MENU_ITEMS_PANE_LENGTH;
	      continue;
	    }

	  pane_name = AREF (menu_items, i + MENU_ITEMS_PANE_NAME);
	  prefix = AREF (menu_items, i + MENU_ITEMS_PANE_PREFIX);

	  if (STRINGP (pane_name) && STRING_MULTIBYTE (pane_name))
	    {
	      pane_name = ENCODE_UTF_8 (pane_name);
	      ASET (menu_items, i + MENU_ITEMS_PANE_NAME, pane_name);
	    }

	  pane_string = (NILP (pane_name)
			 ? "" : SSDATA (pane_name));
	  if (!NILP (prefix))
	    pane_string++;

	  if (strcmp (pane_string, ""))
	    {
	      panes[menu_depth] =
		menu = BMenu_new_submenu (menus[menu_depth], pane_string, 1);
	    }

	  i += MENU_ITEMS_PANE_LENGTH;
	}
      else
	{
	  item_name = AREF (menu_items, i + MENU_ITEMS_ITEM_NAME);
	  enable = AREF (menu_items, i + MENU_ITEMS_ITEM_ENABLE);
	  descrip = AREF (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY);
	  def = AREF (menu_items, i + MENU_ITEMS_ITEM_DEFINITION);
	  selected = AREF (menu_items, i + MENU_ITEMS_ITEM_SELECTED);
	  help = AREF (menu_items, i + MENU_ITEMS_ITEM_HELP);

	  if (STRINGP (item_name) && STRING_MULTIBYTE (item_name))
	    {
	      item_name = ENCODE_UTF_8 (item_name);
	      ASET (menu_items, i + MENU_ITEMS_ITEM_NAME, item_name);
	    }

	  if (STRINGP (descrip) && STRING_MULTIBYTE (descrip))
	    {
	      descrip = ENCODE_UTF_8 (descrip);
	      ASET (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY, descrip);
	    }

	  if (STRINGP (help) && STRING_MULTIBYTE (help))
	    help = ENCODE_UTF_8 (help);

	  if (i + MENU_ITEMS_ITEM_LENGTH < menu_items_used &&
	      NILP (AREF (menu_items, i + MENU_ITEMS_ITEM_LENGTH)))
	    menu = BMenu_new_submenu (menu, SSDATA (item_name), !NILP (enable));
	  else if (NILP (def) && menu_separator_name_p (SSDATA (item_name)))
	    BMenu_add_separator (menu);
	  else if (!is_menu_bar)
	    {
	      if (!use_system_tooltips || NILP (Fsymbol_value (Qtooltip_mode)))
		BMenu_add_item (menu, SSDATA (item_name),
				!NILP (def) ? aref_addr (menu_items, i) : NULL,
				!NILP (enable), !NILP (selected), 0, window,
				!NILP (descrip) ? SSDATA (descrip) : NULL,
				NULL);
	      else
		BMenu_add_item (menu, SSDATA (item_name),
				!NILP (def) ? aref_addr (menu_items, i) : NULL,
				!NILP (enable), !NILP (selected), 0, window,
				!NILP (descrip) ? SSDATA (descrip) : NULL,
				STRINGP (help) ? SSDATA (help) : NULL);
	    }
	  else if (!use_system_tooltips || NILP (Fsymbol_value (Qtooltip_mode)))
	    BMenu_add_item (menu, SSDATA (item_name),
			    !NILP (def) ? (void *) (intptr_t) i : NULL,
			    !NILP (enable), !NILP (selected), 1, window,
			    !NILP (descrip) ? SSDATA (descrip) : NULL,
			    NULL);
	  else
	    BMenu_add_item (menu, SSDATA (item_name),
			    !NILP (def) ? (void *) (intptr_t) i : NULL,
			    !NILP (enable), !NILP (selected), 1, window,
			    !NILP (descrip) ? SSDATA (descrip) : NULL,
			    STRINGP (help) ? SSDATA (help) : NULL);

	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }

  if (view)
    BView_draw_unlock (view);

  SAFE_FREE ();
}

static Lisp_Object
haiku_dialog_show (struct frame *f, Lisp_Object title,
		   Lisp_Object header, const char **error_name)
{
  int i, nb_buttons = 0;
  bool boundary_seen = false;
  Lisp_Object pane_name, vals[10];
  void *alert, *button;
  bool enabled_item_seen_p = false;
  int32 val;

  *error_name = NULL;

  if (menu_items_n_panes > 1)
    {
      *error_name = "Multiple panes in dialog box";
      return Qnil;
    }

  pane_name = AREF (menu_items, MENU_ITEMS_PANE_NAME);
  i = MENU_ITEMS_PANE_LENGTH;

  if (STRING_MULTIBYTE (pane_name))
    pane_name = ENCODE_UTF_8 (pane_name);

  block_input ();
  alert = BAlert_new (SSDATA (pane_name), NILP (header) ? HAIKU_INFO_ALERT :
		      HAIKU_IDEA_ALERT);

  while (i < menu_items_used)
    {
      Lisp_Object item_name, enable, descrip, value;
      item_name = AREF (menu_items, i + MENU_ITEMS_ITEM_NAME);
      enable = AREF (menu_items, i + MENU_ITEMS_ITEM_ENABLE);
      descrip = AREF (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY);
      value = AREF (menu_items, i + MENU_ITEMS_ITEM_VALUE);

      if (NILP (item_name))
	{
	  BAlert_delete (alert);
	  *error_name = "Submenu in dialog items";
	  unblock_input ();
	  return Qnil;
	}

      if (EQ (item_name, Qquote))
	{
	  if (nb_buttons)
	    boundary_seen = true;

	  i++;
	  continue;
	}

      if (nb_buttons >= 9)
	{
	  BAlert_delete (alert);
	  *error_name = "Too many dialog items";
	  unblock_input ();
	  return Qnil;
	}

      if (STRING_MULTIBYTE (item_name))
	item_name = ENCODE_UTF_8 (item_name);
      if (!NILP (descrip) && STRING_MULTIBYTE (descrip))
	descrip = ENCODE_UTF_8 (descrip);

      button = BAlert_add_button (alert, SSDATA (item_name));

      BButton_set_enabled (button, !NILP (enable));
      enabled_item_seen_p |= !NILP (enable);

      if (!NILP (descrip))
	BView_set_tooltip (button, SSDATA (descrip));

      vals[nb_buttons] = value;
      ++nb_buttons;
      i += MENU_ITEMS_ITEM_LENGTH;
    }

  /* Haiku only lets us specify a single button to place on the
     left.  */
  if (boundary_seen)
    BAlert_set_offset_spacing (alert);

  /* If there isn't a single enabled item, add an "Ok" button so the
     popup can be dismissed.  */
  if (!enabled_item_seen_p)
    BAlert_add_button (alert, "Ok");
  unblock_input ();

  unrequest_sigio ();
  ++popup_activated_p;
  val = BAlert_go (alert, block_input, unblock_input,
		   process_pending_signals);
  --popup_activated_p;
  request_sigio ();

  if (val < 0)
    quit ();
  else if (val < nb_buttons)
    return vals[val];

  /* The dialog was dismissed via the button appended to dismiss popup
     dialogs without a single enabled item.  */
  if (nb_buttons)
    quit ();
  /* Otherwise, the Ok button was added because no buttons were seen
     at all.  */
  else
    return Qt;

  emacs_abort ();
}

Lisp_Object
haiku_popup_dialog (struct frame *f, Lisp_Object header, Lisp_Object contents)
{
  Lisp_Object title;
  const char *error_name = NULL;
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
  selection = haiku_dialog_show (f, title, header, &error_name);

  unbind_to (specpdl_count, Qnil);
  discard_menu_items ();

  if (error_name)
    error ("%s", error_name);
  return selection;
}

static void
haiku_menu_show_help (void *help, void *data)
{
  Lisp_Object *id = (Lisp_Object *) help;

  if (help)
    show_help_echo (id[MENU_ITEMS_ITEM_HELP],
		    Qnil, Qnil, Qnil);
  else
    show_help_echo (Qnil, Qnil, Qnil, Qnil);
}

static Lisp_Object
haiku_process_pending_signals_for_menu_1 (void *ptr)
{
  menu_timer_timespec = timer_check ();

  return Qnil;
}

static Lisp_Object
haiku_process_pending_signals_for_menu_2 (enum nonlocal_exit exit, Lisp_Object error)
{
  menu_timer_timespec.tv_sec = 0;
  menu_timer_timespec.tv_nsec = -1;

  return Qnil;
}

static struct timespec
haiku_process_pending_signals_for_menu (void)
{
  process_pending_signals ();

  /* The original idea was to let timers throw so that timeouts can
     work correctly, but there's no way to pop down a BPopupMenu
     that's currently popped up.  */
  internal_catch_all (haiku_process_pending_signals_for_menu_1, NULL,
		      haiku_process_pending_signals_for_menu_2);

  return menu_timer_timespec;
}

Lisp_Object
haiku_menu_show (struct frame *f, int x, int y, int menuflags,
		 Lisp_Object title, const char **error_name)
{
  int i, submenu_depth, j;
  void *view, *menu;
  Lisp_Object *subprefix_stack;
  Lisp_Object prefix, entry;

  USE_SAFE_ALLOCA;

  view = FRAME_HAIKU_VIEW (f);
  i = 0;
  submenu_depth = 0;
  subprefix_stack
    = SAFE_ALLOCA (menu_items_used * sizeof (Lisp_Object));

  eassert (FRAME_HAIKU_P (f));

  *error_name = NULL;

  if (menu_items_used <= MENU_ITEMS_PANE_LENGTH)
    {
      *error_name = "Empty menu";

      SAFE_FREE ();
      return Qnil;
    }

  block_input ();
  if (STRINGP (title) && STRING_MULTIBYTE (title))
    title = ENCODE_UTF_8 (title);

  menu = BPopUpMenu_new (STRINGP (title) ? SSDATA (title) : NULL);
  if (STRINGP (title))
    {
      BMenu_add_title (menu, SSDATA (title));
      BMenu_add_separator (menu);
    }
  digest_menu_items (menu, 0, menu_items_used, 0);
  BView_convert_to_screen (view, &x, &y);
  unblock_input ();

  unrequest_sigio ();
  popup_activated_p++;
  menu_item_selection = BMenu_run (menu, x, y,  haiku_menu_show_help,
				   block_input, unblock_input,
				   haiku_process_pending_signals_for_menu, NULL);
  popup_activated_p--;
  request_sigio ();

  FRAME_DISPLAY_INFO (f)->grabbed = 0;

  /* Clear the grab view manually.  There is a race condition here if
     the window thread receives a button press between here and the
     end of BMenu_run.  */
  be_clear_grab_view ();

  if (menu_item_selection)
    {
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
		      entry = list1 (entry);
		      if (!NILP (prefix))
			entry = Fcons (prefix, entry);
		      for (j = submenu_depth - 1; j >= 0; j--)
			if (!NILP (subprefix_stack[j]))
			  entry = Fcons (subprefix_stack[j], entry);
		    }
		  block_input ();
		  BPopUpMenu_delete (menu);
		  unblock_input ();

		  SAFE_FREE ();
		  return entry;
		}
	      i += MENU_ITEMS_ITEM_LENGTH;
	    }
	}
    }
  else if (!(menuflags & MENU_FOR_CLICK))
    {
      block_input ();
      BPopUpMenu_delete (menu);
      unblock_input ();
      quit ();
    }
  block_input ();
  BPopUpMenu_delete (menu);
  unblock_input ();

  SAFE_FREE ();
  return Qnil;
}

void
free_frame_menubar (struct frame *f)
{
  void *mbar;

  FRAME_MENU_BAR_LINES (f) = 0;
  FRAME_MENU_BAR_HEIGHT (f) = 0;
  FRAME_EXTERNAL_MENU_BAR (f) = 0;

  block_input ();
  mbar = FRAME_HAIKU_MENU_BAR (f);
  FRAME_HAIKU_MENU_BAR (f) = NULL;

  if (mbar)
    BMenuBar_delete (mbar);

  if (FRAME_OUTPUT_DATA (f)->menu_bar_open_p)
    --popup_activated_p;
  FRAME_OUTPUT_DATA (f)->menu_bar_open_p = 0;
  unblock_input ();

  adjust_frame_size (f, -1, -1, 2, false, Qmenu_bar_lines);
}

void
initialize_frame_menubar (struct frame *f)
{
  /* This function is called before the first chance to redisplay
     the frame.  It has to be, so the frame will have the right size.  */
  fset_menu_bar_items (f, menu_bar_items (FRAME_MENU_BAR_ITEMS (f)));
  set_frame_menubar (f, true);
}

void
set_frame_menubar (struct frame *f, bool deep_p)
{
  void *mbar = FRAME_HAIKU_MENU_BAR (f);
  void *view = FRAME_HAIKU_VIEW (f);
  bool first_time_p = false;

  if (!mbar)
    {
      block_input ();
      mbar = FRAME_HAIKU_MENU_BAR (f) = BMenuBar_new (view);
      first_time_p = 1;

      /* Now wait for the MENU_BAR_RESIZE event informing us of the
	 initial dimensions of that menu bar.  */
      if (FRAME_VISIBLE_P (f))
	haiku_wait_for_event (f, MENU_BAR_RESIZE);

      unblock_input ();
    }

  Lisp_Object items;
  struct buffer *prev = current_buffer;
  Lisp_Object buffer;
  specpdl_ref specpdl_count = SPECPDL_INDEX ();
  int previous_menu_items_used = f->menu_bar_items_used;
  Lisp_Object *previous_items
    = alloca (previous_menu_items_used * sizeof *previous_items);
  int count;
  ptrdiff_t subitems, i;
  int *submenu_start, *submenu_end, *submenu_n_panes;
  Lisp_Object *submenu_names;

  XSETFRAME (Vmenu_updating_frame, f);

  if (!deep_p)
    {
      items = FRAME_MENU_BAR_ITEMS (f);
      Lisp_Object string;

      block_input ();
      int count = BMenu_count_items (mbar);

      int i;
      for (i = 0; i < ASIZE (items); i += 4)
	{
	  string = AREF (items, i + 1);

	  if (!STRINGP (string))
	    break;

	  if (STRING_MULTIBYTE (string))
	    string = ENCODE_UTF_8 (string);

	  if (i / 4 < count)
	    {
	      void *it = BMenu_item_at (mbar, i / 4);
	      BMenu_item_set_label (it, SSDATA (string));
	    }
	  else
	    BMenu_new_menu_bar_submenu (mbar, SSDATA (string));
	}

      if (i / 4 < count)
	BMenu_delete_from (mbar, i / 4, count - i / 4 + 1);
      unblock_input ();

      f->menu_bar_items_used = 0;
    }
  else
    {
      /* If we are making a new widget, its contents are empty,
	 do always reinitialize them.  */
      if (first_time_p)
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
      submenu_names = alloca (subitems * sizeof (Lisp_Object));

      init_menu_items ();
      for (i = 0; i < subitems; i++)
	{
	  Lisp_Object key, string, maps;

	  key = AREF (items, 4 * i);
	  string = AREF (items, 4 * i + 1);
	  maps = AREF (items, 4 * i + 2);
	  if (NILP (string))
	    break;

	  if (STRINGP (string) && STRING_MULTIBYTE (string))
	    string = ENCODE_UTF_8 (string);

	  submenu_start[i] = menu_items_used;

	  menu_items_n_panes = 0;
	  parse_single_submenu (key, string, maps);
	  submenu_n_panes[i] = menu_items_n_panes;

	  submenu_end[i] = menu_items_used;
	  submenu_names[i] = string;
	}

      submenu_start[i] = -1;
      finish_menu_items ();

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
	  discard_menu_items ();
	  unbind_to (specpdl_count, Qnil);
	  return;
	}

      /* Convert menu_items into widget_value trees
	 to display the menu.  This cannot evaluate Lisp code.  */

      block_input ();
      count = BMenu_count_items (mbar);
      for (i = 0; submenu_start[i] >= 0; ++i)
	{
	  void *mn = NULL;
	  if (i < count)
	    mn = BMenu_item_get_menu (BMenu_item_at (mbar, i));
	  if (mn)
	    BMenu_delete_all (mn);
	  else
	    mn = BMenu_new_menu_bar_submenu (mbar, SSDATA (submenu_names[i]));

	  menu_items_n_panes = submenu_n_panes[i];
	  digest_menu_items (mn, submenu_start[i], submenu_end[i], 1);
	}
      unblock_input ();

      /* The menu items are different, so store them in the frame.  */
      fset_menu_bar_vector (f, menu_items);
      f->menu_bar_items_used = menu_items_used;
    }

  /* This undoes save_menu_items.  */
  unbind_to (specpdl_count, Qnil);
}

void
run_menu_bar_help_event (struct frame *f, int mb_idx)
{
  Lisp_Object frame, vec, help;

  XSETFRAME (frame, f);

  block_input ();
  if (mb_idx < 0)
    {
      kbd_buffer_store_help_event (frame, Qnil);
      unblock_input ();
      return;
    }

  vec = f->menu_bar_vector;
  if ((mb_idx + MENU_ITEMS_ITEM_HELP) >= ASIZE (vec))
    return;

  help = AREF (vec, mb_idx + MENU_ITEMS_ITEM_HELP);
  if (STRINGP (help) || NILP (help))
    kbd_buffer_store_help_event (frame, help);
  unblock_input ();
}

DEFUN ("menu-or-popup-active-p", Fmenu_or_popup_active_p, Smenu_or_popup_active_p,
       0, 0, 0, doc: /* SKIP: real doc in xmenu.c. */)
  (void)
{
  return popup_activated_p ? Qt : Qnil;
}

DEFUN ("haiku-menu-bar-open", Fhaiku_menu_bar_open, Shaiku_menu_bar_open, 0, 1, "i",
       doc: /* Show and start key navigation of the menu bar in FRAME.
This initially opens the first menu bar item and you can then navigate
with the arrow keys, select a menu entry with the return key, or
cancel with the escape key.  If FRAME is nil or not given, use the
selected frame.  If FRAME has no menu bar, a pop-up is displayed at
the position of the last non-menu event instead.  */)
  (Lisp_Object frame)
{
  struct frame *f = decode_window_system_frame (frame);
  int rc;

  if (FRAME_EXTERNAL_MENU_BAR (f))
    {
      block_input ();
      set_frame_menubar (f, 1);
      rc = BMenuBar_start_tracking (FRAME_HAIKU_MENU_BAR (f));
      unblock_input ();

      if (!rc)
	return Qnil;

      FRAME_OUTPUT_DATA (f)->menu_bar_open_p = 1;
      popup_activated_p += 1;
    }
  else
    return call2 (Qpopup_menu, call0 (Qmouse_menu_bar_map),
		  last_nonmenu_event);

  return Qnil;
}

void
haiku_activate_menubar (struct frame *f)
{
  int rc;

  if (!FRAME_HAIKU_MENU_BAR (f))
    return;

  set_frame_menubar (f, true);

  if (FRAME_OUTPUT_DATA (f)->saved_menu_event)
    {
      block_input ();
      rc = be_replay_menu_bar_event (FRAME_HAIKU_MENU_BAR (f),
				     FRAME_OUTPUT_DATA (f)->saved_menu_event);
      xfree (FRAME_OUTPUT_DATA (f)->saved_menu_event);
      FRAME_OUTPUT_DATA (f)->saved_menu_event = NULL;
      unblock_input ();

      if (!rc)
	return;

      FRAME_OUTPUT_DATA (f)->menu_bar_open_p = 1;
      popup_activated_p += 1;
    }
  else
    {
      block_input ();
      rc = BMenuBar_start_tracking (FRAME_HAIKU_MENU_BAR (f));
      unblock_input ();

      if (!rc)
	return;

      FRAME_OUTPUT_DATA (f)->menu_bar_open_p = 1;
      popup_activated_p += 1;
    }
}

void
syms_of_haikumenu (void)
{
  DEFSYM (Qdebug_on_next_call, "debug-on-next-call");
  DEFSYM (Qpopup_menu, "popup-menu");
  DEFSYM (Qmouse_menu_bar_map, "mouse-menu-bar-map");
  DEFSYM (Qtooltip_mode, "tooltip-mode");

  defsubr (&Smenu_or_popup_active_p);
  defsubr (&Shaiku_menu_bar_open);
  return;
}
