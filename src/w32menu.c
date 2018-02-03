/* Menu support for GNU Emacs on the Microsoft Windows API.
   Copyright (C) 1986, 1988, 1993-1994, 1996, 1998-1999, 2001-2018 Free
   Software Foundation, Inc.

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

#include <signal.h>
#include <stdio.h>
#include <setjmp.h>

#include "lisp.h"
#include "keyboard.h"
#include "frame.h"
#include "blockinput.h"
#include "buffer.h"
#include "coding.h"	/* for ENCODE_SYSTEM */
#include "menu.h"

/* This may include sys/types.h, and that somehow loses
   if this is not done before the other system files.  */
#include "w32term.h"

/* Cygwin does not support the multibyte string functions declared in
 * mbstring.h below --- but that's okay: because Cygwin is
 * UNICODE-only, we don't need to use these functions anyway.  */

#ifndef NTGUI_UNICODE
#include <mbstring.h>
#endif /* !NTGUI_UNICODE */

/* Load sys/types.h if not already loaded.
   In some systems loading it twice is suicidal.  */
#ifndef makedev
#include <sys/types.h>
#endif

#include "w32common.h"	/* for osinfo_cache */

#undef HAVE_DIALOGS /* TODO: Implement native dialogs.  */

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif /* no TRUE */

HMENU current_popup_menu;

typedef BOOL (WINAPI * GetMenuItemInfoA_Proc) (
    IN HMENU,
    IN UINT,
    IN BOOL,
    IN OUT LPMENUITEMINFOA);
typedef BOOL (WINAPI * SetMenuItemInfoA_Proc) (
    IN HMENU,
    IN UINT,
    IN BOOL,
    IN LPCMENUITEMINFOA);
typedef int (WINAPI * MessageBoxW_Proc) (
    IN HWND window,
    IN const WCHAR *text,
    IN const WCHAR *caption,
    IN UINT type);

#ifdef NTGUI_UNICODE
GetMenuItemInfoA_Proc get_menu_item_info = GetMenuItemInfoA;
SetMenuItemInfoA_Proc set_menu_item_info = SetMenuItemInfoA;
AppendMenuW_Proc unicode_append_menu = AppendMenuW;
MessageBoxW_Proc unicode_message_box = MessageBoxW;
#else /* !NTGUI_UNICODE */
GetMenuItemInfoA_Proc get_menu_item_info = NULL;
SetMenuItemInfoA_Proc set_menu_item_info = NULL;
AppendMenuW_Proc unicode_append_menu = NULL;
MessageBoxW_Proc unicode_message_box = NULL;
#endif /* NTGUI_UNICODE */

#ifdef HAVE_DIALOGS
static Lisp_Object w32_dialog_show (struct frame *, Lisp_Object, Lisp_Object, char **);
#else
static bool is_simple_dialog (Lisp_Object);
static Lisp_Object simple_dialog_show (struct frame *, Lisp_Object, Lisp_Object);
#endif

static void utf8to16 (unsigned char *, int, WCHAR *);
static int fill_in_menu (HMENU, widget_value *);

void w32_free_menu_strings (HWND);

Lisp_Object
w32_popup_dialog (struct frame *f, Lisp_Object header, Lisp_Object contents)
{

  check_window_system (f);

#ifndef HAVE_DIALOGS

  /* Handle simple Yes/No choices as MessageBox popups.  */
  if (is_simple_dialog (contents))
    return simple_dialog_show (f, contents, header);
  else
    return Qunsupported__w32_dialog;
#else  /* HAVE_DIALOGS */
    {
      Lisp_Object title;
      char *error_name;
      Lisp_Object selection;

      /* Decode the dialog items from what was specified.  */
      title = Fcar (contents);
      CHECK_STRING (title);

      list_of_panes (Fcons (contents, Qnil));

      /* Display them in a dialog box.  */
      block_input ();
      selection = w32_dialog_show (f, title, header, &error_name);
      unblock_input ();

      discard_menu_items ();
      FRAME_DISPLAY_INFO (f)->grabbed = 0;

      if (error_name) error (error_name);
      return selection;
    }
#endif /* HAVE_DIALOGS */
}

/* Activate the menu bar of frame F.
   This is called from keyboard.c when it gets the
   MENU_BAR_ACTIVATE_EVENT out of the Emacs event queue.

   To activate the menu bar, we signal to the input thread that it can
   return from the WM_INITMENU message, allowing the normal Windows
   processing of the menus.

   But first we recompute the menu bar contents (the whole tree).

   This way we can safely execute Lisp code.  */

void
x_activate_menubar (struct frame *f)
{
  set_frame_menubar (f, false, true);

  /* Lock out further menubar changes while active.  */
  f->output_data.w32->menubar_active = 1;

  /* Signal input thread to return from WM_INITMENU.  */
  complete_deferred_msg (FRAME_W32_WINDOW (f), WM_INITMENU, 0);
}

/* This callback is called from the menu bar pulldown menu
   when the user makes a selection.
   Figure out what the user chose
   and put the appropriate events into the keyboard buffer.  */
void menubar_selection_callback (struct frame *, void *);

void
menubar_selection_callback (struct frame *f, void * client_data)
{
  Lisp_Object prefix, entry;
  Lisp_Object vector;
  Lisp_Object *subprefix_stack;
  int submenu_depth = 0;
  int i;

  if (!f)
    return;
  entry = Qnil;
  subprefix_stack = (Lisp_Object *) alloca (f->menu_bar_items_used * word_size);
  vector = f->menu_bar_vector;
  prefix = Qnil;
  i = 0;
  while (i < f->menu_bar_items_used)
    {
      if (EQ (AREF (vector, i), Qnil))
	{
	  subprefix_stack[submenu_depth++] = prefix;
	  prefix = entry;
	  i++;
	}
      else if (EQ (AREF (vector, i), Qlambda))
	{
	  prefix = subprefix_stack[--submenu_depth];
	  i++;
	}
      else if (EQ (AREF (vector, i), Qt))
	{
	  prefix = AREF (vector, i + MENU_ITEMS_PANE_PREFIX);
	  i += MENU_ITEMS_PANE_LENGTH;
	}
      else
	{
	  entry = AREF (vector, i + MENU_ITEMS_ITEM_VALUE);
	  /* The UINT_PTR cast avoids a warning.  There's no problem
	     as long as pointers have enough bits to hold small integers.  */
	  if ((int) (UINT_PTR) client_data == i)
	    {
	      int j;
	      struct input_event buf;
	      Lisp_Object frame;
	      EVENT_INIT (buf);

	      XSETFRAME (frame, f);
	      buf.kind = MENU_BAR_EVENT;
	      buf.frame_or_window = frame;
	      buf.arg = frame;
	      kbd_buffer_store_event (&buf);

	      for (j = 0; j < submenu_depth; j++)
		if (!NILP (subprefix_stack[j]))
		  {
		    buf.kind = MENU_BAR_EVENT;
		    buf.frame_or_window = frame;
		    buf.arg = subprefix_stack[j];
		    kbd_buffer_store_event (&buf);
		  }

	      if (!NILP (prefix))
		{
		  buf.kind = MENU_BAR_EVENT;
		  buf.frame_or_window = frame;
		  buf.arg = prefix;
		  kbd_buffer_store_event (&buf);
		}

	      buf.kind = MENU_BAR_EVENT;
	      buf.frame_or_window = frame;
	      buf.arg = entry;
	      /* Free memory used by owner-drawn and help-echo strings.  */
	      w32_free_menu_strings (FRAME_W32_WINDOW (f));
	      kbd_buffer_store_event (&buf);

	      f->output_data.w32->menubar_active = 0;
	      return;
	    }
	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }
  /* Free memory used by owner-drawn and help-echo strings.  */
  w32_free_menu_strings (FRAME_W32_WINDOW (f));
  f->output_data.w32->menubar_active = 0;
}


/* Set the contents of the menubar widgets of frame F.
   The argument FIRST_TIME is currently ignored;
   it is set the first time this is called, from initialize_frame_menubar.  */

void
set_frame_menubar (struct frame *f, bool first_time, bool deep_p)
{
  HMENU menubar_widget = f->output_data.w32->menubar_widget;
  Lisp_Object items;
  widget_value *wv, *first_wv, *prev_wv = 0;
  int i, last_i = 0;
  int *submenu_start, *submenu_end;
  int *submenu_top_level_items, *submenu_n_panes;

  /* We must not change the menubar when actually in use.  */
  if (f->output_data.w32->menubar_active)
    return;

  XSETFRAME (Vmenu_updating_frame, f);

  if (! menubar_widget)
    deep_p = true;

  if (deep_p)
    {
      /* Make a widget-value tree representing the entire menu trees.  */

      struct buffer *prev = current_buffer;
      Lisp_Object buffer;
      ptrdiff_t specpdl_count = SPECPDL_INDEX ();
      int previous_menu_items_used = f->menu_bar_items_used;
      Lisp_Object *previous_items
	= (Lisp_Object *) alloca (previous_menu_items_used
				  * word_size);

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

      /* Run the hooks.  */
      safe_run_hooks (Qactivate_menubar_hook);
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
      submenu_start = (int *) alloca (ASIZE (items) * sizeof (int));
      submenu_end = (int *) alloca (ASIZE (items) * sizeof (int));
      submenu_n_panes = (int *) alloca (ASIZE (items) * sizeof (int));
      submenu_top_level_items = (int *) alloca (ASIZE (items) * sizeof (int));
      init_menu_items ();
      for (i = 0; i < ASIZE (items); i += 4)
	{
	  Lisp_Object key, string, maps;

	  last_i = i;

	  key = AREF (items, i);
	  string = AREF (items, i + 1);
	  maps = AREF (items, i + 2);
	  if (NILP (string))
	    break;

	  submenu_start[i] = menu_items_used;

	  menu_items_n_panes = 0;
	  submenu_top_level_items[i]
	    = parse_single_submenu (key, string, maps);
	  submenu_n_panes[i] = menu_items_n_panes;

	  submenu_end[i] = menu_items_used;
	}

      finish_menu_items ();

      /* Convert menu_items into widget_value trees
	 to display the menu.  This cannot evaluate Lisp code.  */

      wv = make_widget_value ("menubar", NULL, true, Qnil);
      wv->button_type = BUTTON_TYPE_NONE;
      first_wv = wv;

      for (i = 0; i < last_i; i += 4)
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

      for (i = 0; i < previous_menu_items_used; i++)
	if (menu_items_used == i
	    || (!EQ (previous_items[i], AREF (menu_items, i))))
	  break;
      if (i == menu_items_used && i == previous_menu_items_used && i != 0)
	{
	  free_menubar_widget_value_tree (first_wv);
	  discard_menu_items ();
          unbind_to (specpdl_count, Qnil);
	  return;
	}

      fset_menu_bar_vector (f, menu_items);
      f->menu_bar_items_used = menu_items_used;

      /* This undoes save_menu_items.  */
      unbind_to (specpdl_count, Qnil);

      /* Now GC cannot happen during the lifetime of the widget_value,
	 so it's safe to store data from a Lisp_String, as long as
	 local copies are made when the actual menu is created.
	 Windows takes care of this for normal string items, but
	 not for owner-drawn items or additional item-info.  */
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
	  /* The EMACS_INT cast avoids a warning.
	     This value just has to be different from small integers.  */
	  wv->call_data = (void *) (EMACS_INT) (-1);

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

  if (menubar_widget)
    {
      /* Empty current menubar, rather than creating a fresh one.  */
      while (DeleteMenu (menubar_widget, 0, MF_BYPOSITION))
	;
    }
  else
    {
      menubar_widget = CreateMenu ();
    }
  fill_in_menu (menubar_widget, first_wv->contents);

  free_menubar_widget_value_tree (first_wv);

  {
    HMENU old_widget = f->output_data.w32->menubar_widget;

    f->output_data.w32->menubar_widget = menubar_widget;
    SetMenu (FRAME_W32_WINDOW (f), f->output_data.w32->menubar_widget);
    /* Causes flicker when menu bar is updated
    DrawMenuBar (FRAME_W32_WINDOW (f)); */

    /* Force the window size to be recomputed so that the frame's text
       area remains the same, if menubar has just been created.  */
    if (old_widget == NULL)
      {
	windows_or_buffers_changed = 23;
	adjust_frame_size (f, -1, -1, 2, false, Qmenu_bar_lines);
      }
  }

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
   This is used when deleting a frame, and when turning off the menu bar.  */

void
free_frame_menubar (struct frame *f)
{
  block_input ();

  {
    HMENU old = GetMenu (FRAME_W32_WINDOW (f));
    SetMenu (FRAME_W32_WINDOW (f), NULL);
    f->output_data.w32->menubar_widget = NULL;
    DestroyMenu (old);
  }

  unblock_input ();
}


/* w32_menu_show actually displays a menu using the panes and items in
   menu_items and returns the value selected from it; we assume input
   is blocked by the caller.  */

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

Lisp_Object
w32_menu_show (struct frame *f, int x, int y, int menuflags,
	       Lisp_Object title, const char **error)
{
  int i;
  int menu_item_selection;
  HMENU menu;
  POINT pos;
  widget_value *wv, *save_wv = 0, *first_wv = 0, *prev_wv = 0;
  widget_value **submenu_stack
    = (widget_value **) alloca (menu_items_used * sizeof (widget_value *));
  Lisp_Object *subprefix_stack
    = (Lisp_Object *) alloca (menu_items_used * word_size);
  int submenu_depth = 0;
  bool first_pane;

  *error = NULL;

  if (menu_items_n_panes == 0)
    return Qnil;

  if (menu_items_used <= MENU_ITEMS_PANE_LENGTH)
    {
      *error = "Empty menu";
      return Qnil;
    }

  block_input ();

  /* Create a tree of widget_value objects
     representing the panes and their items.  */
  wv = make_widget_value ("menu", NULL, true, Qnil);
  wv->button_type = BUTTON_TYPE_NONE;
  first_wv = wv;
  first_pane = true;

  /* Loop over all panes and items, filling in the tree.  */
  i = 0;
  while (i < menu_items_used)
    {
      if (EQ (AREF (menu_items, i), Qnil))
	{
	  submenu_stack[submenu_depth++] = save_wv;
	  save_wv = prev_wv;
	  prev_wv = 0;
	  first_pane = false;
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

	  if (STRINGP (pane_name))
	    {
	      if (unicode_append_menu)
		pane_name = ENCODE_UTF_8 (pane_name);
	      else if (STRING_MULTIBYTE (pane_name))
		pane_name = ENCODE_SYSTEM (pane_name);

	      ASET (menu_items, i + MENU_ITEMS_PANE_NAME, pane_name);
	    }

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

          if (STRINGP (item_name))
	    {
	      if (unicode_append_menu)
		item_name = ENCODE_UTF_8 (item_name);
	      else if (STRING_MULTIBYTE (item_name))
		item_name = ENCODE_SYSTEM (item_name);

	      ASET (menu_items, i + MENU_ITEMS_ITEM_NAME, item_name);
	    }

	  if (STRINGP (descrip) && STRING_MULTIBYTE (descrip))
            {
	      descrip = ENCODE_SYSTEM (descrip);
	      ASET (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY, descrip);
	    }

	  wv = make_widget_value (SSDATA (item_name), NULL, !NILP (enable),
				  STRINGP (help) ? help : Qnil);
	  if (prev_wv)
	    prev_wv->next = wv;
	  else
	    save_wv->contents = wv;
	  if (!NILP (descrip))
	    wv->key = SSDATA (descrip);
	  /* Use the contents index as call_data, since we are
             restricted to 16-bits.  */
	  wv->call_data = !NILP (def) ? (void *) (UINT_PTR) i : 0;

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
      widget_value *wv_sep = make_widget_value ("--", NULL, false, Qnil);

      /* Maybe replace this separator with a bitmap or owner-draw item
	 so that it looks better.  Having two separators looks odd.  */
      wv_sep->next = first_wv->contents;

      if (unicode_append_menu)
	title = ENCODE_UTF_8 (title);
      else if (STRING_MULTIBYTE (title))
	title = ENCODE_SYSTEM (title);

      wv_title = make_widget_value (SSDATA (title), NULL, true, Qnil);
      wv_title->title = TRUE;
      wv_title->button_type = BUTTON_TYPE_NONE;
      wv_title->next = wv_sep;
      first_wv->contents = wv_title;
    }

  /* No selection has been chosen yet.  */
  menu_item_selection = 0;

  /* Actually create the menu.  */
  current_popup_menu = menu = CreatePopupMenu ();
  fill_in_menu (menu, first_wv->contents);

  /* Adjust coordinates to be root-window-relative.  */
  pos.x = x;
  pos.y = y;
  ClientToScreen (FRAME_W32_WINDOW (f), &pos);

  /* Display the menu.  */
  menu_item_selection = SendMessage (FRAME_W32_WINDOW (f),
				     WM_EMACS_TRACKPOPUPMENU,
				     (WPARAM)menu, (LPARAM)&pos);

  /* Clean up extraneous mouse events which might have been generated
     during the call. */
  discard_mouse_events ();
  FRAME_DISPLAY_INFO (f)->grabbed = 0;

  /* Free the widget_value objects we used to specify the contents.  */
  free_menubar_widget_value_tree (first_wv);

  DestroyMenu (menu);

  /* Free the owner-drawn and help-echo menu strings.  */
  w32_free_menu_strings (FRAME_W32_WINDOW (f));
  f->output_data.w32->menubar_active = 0;

  /* Find the selected item, and its pane, to return
     the proper value.  */
  if (menu_item_selection != 0)
    {
      Lisp_Object prefix, entry;

      prefix = entry = Qnil;
      i = 0;
      while (i < menu_items_used)
	{
	  if (EQ (AREF (menu_items, i), Qnil))
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
	      if (menu_item_selection == i)
		{
		  if (menuflags & MENU_KEYMAPS)
		    {
		      int j;

		      entry = Fcons (entry, Qnil);
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


#ifdef HAVE_DIALOGS
/* TODO: On Windows, there are two ways of defining a dialog.

   1. Create a predefined dialog resource and include it in nt/emacs.rc.
      Using this method, we could then set the titles and make unneeded
      buttons invisible before displaying the dialog.  Everything would
      be a fixed size though, so there is a risk that text does not
      fit on a button.
   2. Create the dialog template in memory on the fly.  This allows us
      to size the dialog and buttons dynamically, probably giving more
      natural looking results for dialogs with few buttons, and eliminating
      the problem of text overflowing the buttons.  But the API for this is
      quite complex - structures have to be allocated in particular ways,
      text content is tacked onto the end of structures in variable length
      arrays with further structures tacked on after these, there are
      certain alignment requirements for all this, and we have to
      measure all the text and convert to "dialog coordinates" to figure
      out how big to make everything.

      For now, we'll just stick with menus for dialogs that are more
      complicated than simple yes/no type questions for which we can use
      the MessageBox function.
*/

static char * button_names [] = {
  "button1", "button2", "button3", "button4", "button5",
  "button6", "button7", "button8", "button9", "button10" };

static Lisp_Object
w32_dialog_show (struct frame *f, Lisp_Object title,
		 Lisp_Object header, char **error)
{
  int i, nb_buttons = 0;
  char dialog_name[6];
  int menu_item_selection;

  widget_value *wv, *first_wv = 0, *prev_wv = 0;

  /* Number of elements seen so far, before boundary.  */
  int left_count = 0;
  /* true means we've seen the boundary between left-hand elts and
     right-hand.  */
  bool boundary_seen = false;

  *error = NULL;

  if (menu_items_n_panes > 1)
    {
      *error = "Multiple panes in dialog box";
      return Qnil;
    }

  /* Create a tree of widget_value objects
     representing the text label and buttons.  */
  {
    Lisp_Object pane_name;
    char *pane_string;
    pane_name = AREF (menu_items, MENU_ITEMS_PANE_NAME);
    pane_string = (NILP (pane_name)
		   ? "" : SSDATA (pane_name));
    prev_wv = make_widget_value ("message", pane_string, true, Qnil);
    first_wv = prev_wv;

    /* Loop over all panes and items, filling in the tree.  */
    i = MENU_ITEMS_PANE_LENGTH;
    while (i < menu_items_used)
      {

	/* Create a new item within current pane.  */
	Lisp_Object item_name, enable, descrip, help;

	item_name = AREF (menu_items, i + MENU_ITEMS_ITEM_NAME);
	enable = AREF (menu_items, i + MENU_ITEMS_ITEM_ENABLE);
	descrip = AREF (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY);
        help = AREF (menu_items, i + MENU_ITEMS_ITEM_HELP);

	if (NILP (item_name))
	  {
	    free_menubar_widget_value_tree (first_wv);
	    *error = "Submenu in dialog items";
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
	    *error = "Too many dialog items";
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

  /* Actually create the dialog.  */
  dialog_id = widget_id_tick++;
  menu = lw_create_widget (first_wv->name, "dialog", dialog_id, first_wv,
			   f->output_data.w32->widget, true, 0,
			   dialog_selection_callback, 0);
  lw_modify_all_widgets (dialog_id, first_wv->contents, TRUE);

  /* Free the widget_value objects we used to specify the contents.  */
  free_menubar_widget_value_tree (first_wv);

  /* No selection has been chosen yet.  */
  menu_item_selection = 0;

  /* Display the menu.  */
  lw_pop_up_all_widgets (dialog_id);

  /* Process events that apply to the menu.  */
  popup_get_selection ((XEvent *) 0, FRAME_DISPLAY_INFO (f), dialog_id);

  lw_destroy_all_widgets (dialog_id);

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
	  else
	    {
	      entry = AREF (menu_items, i + MENU_ITEMS_ITEM_VALUE);
	      if (menu_item_selection == i)
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
#else /* !HAVE_DIALOGS  */

/* Currently we only handle Yes No dialogs (y-or-n-p and yes-or-no-p) as
   simple dialogs.  We could handle a few more, but I'm not aware of
   anywhere in Emacs that uses the other specific dialog choices that
   MessageBox provides.  */

static bool
is_simple_dialog (Lisp_Object contents)
{
  Lisp_Object options;
  Lisp_Object name, yes, no, other;

  if (!CONSP (contents))
    return false;
  options = XCDR (contents);

  yes = build_string ("Yes");
  no = build_string ("No");

  if (!CONSP (options))
    return false;

  name = XCAR (options);
  if (!CONSP (name))
    return false;
  name = XCAR (name);

  if (!NILP (Fstring_equal (name, yes)))
    other = no;
  else if (!NILP (Fstring_equal (name, no)))
    other = yes;
  else
    return false;

  options = XCDR (options);
  if (!CONSP (options))
    return false;

  name = XCAR (options);
  if (!CONSP (name))
    return false;
  name = XCAR (name);
  if (NILP (Fstring_equal (name, other)))
    return false;

  /* Check there are no more options.  */
  options = XCDR (options);
  return !(CONSP (options));
}

static Lisp_Object
simple_dialog_show (struct frame *f, Lisp_Object contents, Lisp_Object header)
{
  int answer;
  UINT type;
  Lisp_Object lispy_answer = Qnil, temp = XCAR (contents);

  type = MB_YESNO;

  /* Since we only handle Yes/No dialogs, and we already checked
     is_simple_dialog, we don't need to worry about checking contents
     to see what type of dialog to use.  */

  /* Use Unicode if possible, so any language can be displayed.  */
  if (unicode_message_box)
    {
      WCHAR *text;
      const WCHAR *title;
      USE_SAFE_ALLOCA;

      if (STRINGP (temp))
	{
	  char *utf8_text = SSDATA (ENCODE_UTF_8 (temp));
	  /* Be pessimistic about the number of characters needed.
	     Remember characters outside the BMP will take more than
	     one utf16 word, so we cannot simply use the character
	     length of temp.  */
	  int utf8_len = strlen (utf8_text);
	  text = SAFE_ALLOCA ((utf8_len + 1) * sizeof (WCHAR));
	  utf8to16 ((unsigned char *)utf8_text, utf8_len, text);
	}
      else
	{
	  text = (WCHAR *)L"";
	}

      if (NILP (header))
	{
	  title = L"Question";
	  type |= MB_ICONQUESTION;
	}
      else
	{
	  title = L"Information";
	  type |= MB_ICONINFORMATION;
	}

      answer = unicode_message_box (FRAME_W32_WINDOW (f), text, title, type);
      SAFE_FREE ();
    }
  else
    {
      const char *text, *title;

      /* Fall back on ANSI message box, but at least use system
	 encoding so questions representable by the system codepage
	 are encoded properly.  */
      if (STRINGP (temp))
	text = SSDATA (ENCODE_SYSTEM (temp));
      else
	text = "";

      if (NILP (header))
	{
	  title = "Question";
	  type |= MB_ICONQUESTION;
	}
      else
	{
	  title = "Information";
	  type |= MB_ICONINFORMATION;
	}

      answer = MessageBox (FRAME_W32_WINDOW (f), text, title, type);
    }

  if (answer == IDYES)
    lispy_answer = build_string ("Yes");
  else if (answer == IDNO)
    lispy_answer = build_string ("No");
  else
    quit ();

  for (temp = XCDR (contents); CONSP (temp); temp = XCDR (temp))
    {
      Lisp_Object item, name, value;
      item = XCAR (temp);
      if (CONSP (item))
	{
	  name = XCAR (item);
	  value = XCDR (item);
	}
      else
	{
	  name = item;
	  value = Qnil;
	}

      if (!NILP (Fstring_equal (name, lispy_answer)))
	{
	  return value;
	}
    }
  return quit ();
}
#endif  /* !HAVE_DIALOGS  */


/* UTF8: 0xxxxxxx, 110xxxxx 10xxxxxx, 1110xxxx, 10xxxxxx, 10xxxxxx */
static void
utf8to16 (unsigned char * src, int len, WCHAR * dest)
{
  while (len > 0)
    {
      if (*src < 0x80)
	{
	  *dest = (WCHAR) *src;
	  dest++; src++; len--;
	}
      /* Since we might get >3 byte sequences which we don't handle, ignore the extra parts.  */
      else if (*src < 0xC0)
	{
	  src++; len--;
	}
      /* 2 char UTF-8 sequence.  */
      else if (*src <  0xE0)
	{
	  *dest = (WCHAR) (((*src & 0x1f) << 6)
			   | (*(src + 1) & 0x3f));
	  src += 2; len -= 2; dest++;
	}
      else if (*src < 0xF0)
	{
	  *dest = (WCHAR) (((*src & 0x0f) << 12)
			   | ((*(src + 1) & 0x3f) << 6)
			   | (*(src + 2) & 0x3f));
	  src += 3; len -= 3; dest++;
	}
      else /* Not encodable. Insert Unicode Substitution char.  */
	{
	  *dest = (WCHAR) 0xfffd;
	  src++; len--; dest++;
	}
    }
  *dest = 0;
}

static int
add_menu_item (HMENU menu, widget_value *wv, HMENU item)
{
  UINT fuFlags;
  char *out_string, *p, *q;
  int return_value;
  size_t nlen, orig_len;
  USE_SAFE_ALLOCA;

  if (menu_separator_name_p (wv->name))
    {
      fuFlags = MF_SEPARATOR;
      out_string = NULL;
    }
  else
    {
      if (wv->enabled)
	fuFlags = MF_STRING;
      else
	fuFlags = MF_STRING | MF_GRAYED;

      if (wv->key != NULL)
	{
	  out_string = SAFE_ALLOCA (strlen (wv->name) + strlen (wv->key) + 2);
	  p = stpcpy (out_string, wv->name);
	  p = stpcpy (p, "\t");
	  strcpy (p, wv->key);
	}
      else
	out_string = (char *)wv->name;

      /* Quote any special characters within the menu item's text and
	 key binding.  */
      nlen = orig_len = strlen (out_string);
      if (unicode_append_menu)
        {
          /* With UTF-8, & cannot be part of a multibyte character.  */
          for (p = out_string; *p; p++)
            {
              if (*p == '&')
                nlen++;
            }
        }
#ifndef NTGUI_UNICODE
      else
        {
          /* If encoded with the system codepage, use multibyte string
             functions in case of multibyte characters that contain '&'.  */
          for (p = out_string; *p; p = _mbsinc (p))
            {
              if (_mbsnextc (p) == '&')
                nlen++;
            }
        }
#endif /* !NTGUI_UNICODE */

      if (nlen > orig_len)
        {
          p = out_string;
          out_string = SAFE_ALLOCA (nlen + 1);
          q = out_string;
          while (*p)
            {
              if (unicode_append_menu)
                {
                  if (*p == '&')
                    *q++ = *p;
                  *q++ = *p++;
                }
#ifndef NTGUI_UNICODE
              else
                {
                  if (_mbsnextc (p) == '&')
                    {
                      _mbsncpy (q, p, 1);
                      q = _mbsinc (q);
                    }
                  _mbsncpy (q, p, 1);
                  p = _mbsinc (p);
                  q = _mbsinc (q);
                }
#endif /* !NTGUI_UNICODE */
            }
          *q = '\0';
        }

      if (item != NULL)
	fuFlags = MF_POPUP;
      else if (wv->title || wv->call_data == 0)
	{
	  /* Only use MF_OWNERDRAW if GetMenuItemInfo is usable, since
	     we can't deallocate the memory otherwise.  */
	  if (get_menu_item_info)
	    {
              out_string = (char *) local_alloc (strlen (wv->name) + 1);
              strcpy (out_string, wv->name);
#ifdef MENU_DEBUG
	      DebPrint ("Menu: allocating %ld for owner-draw", out_string);
#endif
	      fuFlags = MF_OWNERDRAW | MF_DISABLED;
	    }
	  else
	    fuFlags = MF_DISABLED;
	}

      /* Draw radio buttons and tickboxes. */
      else if (wv->selected && (wv->button_type == BUTTON_TYPE_TOGGLE ||
				wv->button_type == BUTTON_TYPE_RADIO))
	fuFlags |= MF_CHECKED;
      else
	fuFlags |= MF_UNCHECKED;
    }

  if (unicode_append_menu && out_string)
    {
      /* Convert out_string from UTF-8 to UTF-16-LE.  */
      int utf8_len = strlen (out_string);
      WCHAR * utf16_string;
      if (fuFlags & MF_OWNERDRAW)
	utf16_string = local_alloc ((utf8_len + 1) * sizeof (WCHAR));
      else
	utf16_string = SAFE_ALLOCA ((utf8_len + 1) * sizeof (WCHAR));

      utf8to16 ((unsigned char *)out_string, utf8_len, utf16_string);
      return_value = unicode_append_menu (menu, fuFlags,
					  item != NULL ? (UINT_PTR) item
					    : (UINT_PTR) wv->call_data,
					  utf16_string);

#ifndef NTGUI_UNICODE /* Fallback does not apply when always UNICODE */
      if (!return_value)
	{
	  /* On W9x/ME, Unicode menus are not supported, though AppendMenuW
	     apparently does exist at least in some cases and appears to be
	     stubbed out to do nothing.  out_string is UTF-8, but since
	     our standard menus are in English and this is only going to
	     happen the first time a menu is used, the encoding is
	     of minor importance compared with menus not working at all.  */
	  return_value =
	    AppendMenu (menu, fuFlags,
			item != NULL ? (UINT_PTR) item: (UINT_PTR) wv->call_data,
			out_string);
	  /* Don't use Unicode menus in future, unless this is Windows
	     NT or later, where a failure of AppendMenuW does NOT mean
	     Unicode menus are unsupported.  */
	  if (osinfo_cache.dwPlatformId != VER_PLATFORM_WIN32_NT)
	    unicode_append_menu = NULL;
	}
#endif /* NTGUI_UNICODE */

      if (unicode_append_menu && (fuFlags & MF_OWNERDRAW))
	local_free (out_string);
    }
  else
    {
      return_value =
	AppendMenu (menu,
		    fuFlags,
		    item != NULL ? (UINT_PTR) item : (UINT_PTR) wv->call_data,
		    out_string );
    }

  /* This must be done after the menu item is created.  */
  if (!wv->title && wv->call_data != 0)
    {
      if (set_menu_item_info)
	{
	  MENUITEMINFO info;
	  memset (&info, 0, sizeof (info));
	  info.cbSize = sizeof (info);
	  info.fMask = MIIM_DATA;

	  /* Set help string for menu item.  Leave it as a pointer to
	     a Lisp_String until it is ready to be displayed, since GC
	     can happen while menus are active.  */
	  if (!NILP (wv->help))
	    {
	      /* We use XUNTAG below because in a 32-bit build
		 --with-wide-int we cannot pass a Lisp_Object
		 via a DWORD member of MENUITEMINFO.  */
	      /* As of Jul-2012, w32api headers say that dwItemData
		 has DWORD type, but that's a bug: it should actually
		 be ULONG_PTR, which is correct for 32-bit and 64-bit
		 Windows alike.  MSVC headers get it right; hopefully,
		 MinGW headers will, too.  */
	      eassert (STRINGP (wv->help));
	      info.dwItemData = (ULONG_PTR) XUNTAG (wv->help, Lisp_String);
	    }
	  if (wv->button_type == BUTTON_TYPE_RADIO)
	    {
	      /* CheckMenuRadioItem allows us to differentiate TOGGLE and
		 RADIO items, but is not available on NT 3.51 and earlier.  */
	      info.fMask |= MIIM_TYPE | MIIM_STATE;
	      info.fType = MFT_RADIOCHECK | MFT_STRING;
	      info.dwTypeData = out_string;
	      info.fState = wv->selected ? MFS_CHECKED : MFS_UNCHECKED;
	    }

	  set_menu_item_info (menu,
			      item != NULL ? (UINT_PTR) item : (UINT_PTR) wv->call_data,
			      FALSE, &info);
	}
    }
  SAFE_FREE ();
  return return_value;
}

/* Construct native Windows menu(bar) based on widget_value tree.  */
static int
fill_in_menu (HMENU menu, widget_value *wv)
{
  for ( ; wv != NULL; wv = wv->next)
    {
      if (wv->contents)
	{
	  HMENU sub_menu = CreatePopupMenu ();

	  if (sub_menu == NULL)
	    return 0;

	  if (!fill_in_menu (sub_menu, wv->contents) ||
	      !add_menu_item (menu, wv, sub_menu))
	    {
	      DestroyMenu (sub_menu);
	      return 0;
	    }
	}
      else
	{
	  if (!add_menu_item (menu, wv, NULL))
	    return 0;
	}
    }
  return 1;
}

/* Display help string for currently pointed to menu item. Not
   supported on NT 3.51 and earlier, as GetMenuItemInfo is not
   available. */
void w32_menu_display_help (HWND, HMENU, UINT, UINT);

void
w32_menu_display_help (HWND owner, HMENU menu, UINT item, UINT flags)
{
  if (get_menu_item_info)
    {
      struct frame *f = x_window_to_frame (&one_w32_display_info, owner);
      Lisp_Object frame, help;

      /* No help echo on owner-draw menu items, or when the keyboard
	 is used to navigate the menus, since tooltips are distracting
	 if they pop up elsewhere.  */
      if ((flags & MF_OWNERDRAW) || (flags & MF_POPUP)
	  || !(flags & MF_MOUSESELECT)
	  /* Ignore any dwItemData for menu items whose flags don't
	     have the MF_HILITE bit set.  These are dwItemData that
	     Windows sends our way, but they aren't pointers to our
	     Lisp_String objects, so trying to create Lisp_Strings out
	     of them below and pass that to the keyboard queue will
	     crash Emacs when we try to display those "strings".  It
	     is unclear why we get these dwItemData, or what they are:
	     sometimes they point to a wchar_t string that is the menu
	     title, sometimes to someting that doesn't look like text
	     at all.  (The problematic data also comes with the 0x0800
	     bit set, but this bit is not documented, so we don't want
	     to depend on it.)  */
	  || !(flags & MF_HILITE))
	help = Qnil;
      else
	{
	  MENUITEMINFO info;

	  memset (&info, 0, sizeof (info));
	  info.cbSize = sizeof (info);
	  info.fMask = MIIM_DATA;
	  get_menu_item_info (menu, item, FALSE, &info);

	  help =
	    info.dwItemData
	    ? make_lisp_ptr ((void *) info.dwItemData, Lisp_String)
	    : Qnil;
	}

      /* Store the help echo in the keyboard buffer as the X toolkit
	 version does, rather than directly showing it. This seems to
	 solve the GC problems that were present when we based the
	 Windows code on the non-toolkit version.  */
      if (f)
	{
	  XSETFRAME (frame, f);
	  kbd_buffer_store_help_event (frame, help);
	}
      else
	/* X version has a loop through frames here, which doesn't
	   appear to do anything, unless it has some side effect.  */
	show_help_echo (help, Qnil, Qnil, Qnil);
    }
}

/* Free memory used by owner-drawn strings.  */
static void
w32_free_submenu_strings (HMENU menu)
{
  int i, num = GetMenuItemCount (menu);
  for (i = 0; i < num; i++)
    {
      MENUITEMINFO info;
      memset (&info, 0, sizeof (info));
      info.cbSize = sizeof (info);
      info.fMask = MIIM_DATA | MIIM_TYPE | MIIM_SUBMENU;

      get_menu_item_info (menu, i, TRUE, &info);

      /* Owner-drawn names are held in dwItemData.  */
      if ((info.fType & MF_OWNERDRAW) && info.dwItemData)
	{
#ifdef MENU_DEBUG
	  DebPrint ("Menu: freeing %ld for owner-draw", info.dwItemData);
#endif
	  local_free (info.dwItemData);
	}

      /* Recurse down submenus.  */
      if (info.hSubMenu)
	w32_free_submenu_strings (info.hSubMenu);
    }
}

void
w32_free_menu_strings (HWND hwnd)
{
  HMENU menu = current_popup_menu;

  if (get_menu_item_info)
    {
      /* If there is no popup menu active, free the strings from the frame's
	 menubar.  */
      if (!menu)
	menu = GetMenu (hwnd);

      if (menu)
	w32_free_submenu_strings (menu);
    }

  current_popup_menu = NULL;
}

/* The following is used by delayed window autoselection.  */

DEFUN ("menu-or-popup-active-p", Fmenu_or_popup_active_p, Smenu_or_popup_active_p, 0, 0, 0,
       doc: /* Return t if a menu or popup dialog is active on selected frame.  */)
  (void)
{
  struct frame *f;
  f = SELECTED_FRAME ();
  return (f->output_data.w32->menubar_active > 0) ? Qt : Qnil;
}

void
syms_of_w32menu (void)
{
  globals_of_w32menu ();

  current_popup_menu = NULL;

  DEFSYM (Qdebug_on_next_call, "debug-on-next-call");
  DEFSYM (Qunsupported__w32_dialog, "unsupported--w32-dialog");

  defsubr (&Smenu_or_popup_active_p);
}

/*
	globals_of_w32menu is used to initialize those global variables that
	must always be initialized on startup even when the global variable
	initialized is non zero (see the function main in emacs.c).
	globals_of_w32menu is called from syms_of_w32menu when the global
	variable initialized is 0 and directly from main when initialized
	is non zero.
 */
void
globals_of_w32menu (void)
{
#ifndef NTGUI_UNICODE
  /* See if Get/SetMenuItemInfo functions are available.  */
  HMODULE user32 = GetModuleHandle ("user32.dll");
  get_menu_item_info = (GetMenuItemInfoA_Proc) GetProcAddress (user32, "GetMenuItemInfoA");
  set_menu_item_info = (SetMenuItemInfoA_Proc) GetProcAddress (user32, "SetMenuItemInfoA");
  unicode_append_menu = (AppendMenuW_Proc) GetProcAddress (user32, "AppendMenuW");
  unicode_message_box = (MessageBoxW_Proc) GetProcAddress (user32, "MessageBoxW");
#endif /* !NTGUI_UNICODE */
}
