/* Menu support for GNU Emacs on the Microsoft W32 API.
   Copyright (C) 1986, 1988, 1993, 1994, 1996, 1998, 1999, 2001, 2002,
                 2003, 2004, 2005, 2006, 2007  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include <config.h>
#include <signal.h>

#include <stdio.h>
#include "lisp.h"
#include "termhooks.h"
#include "keyboard.h"
#include "keymap.h"
#include "frame.h"
#include "window.h"
#include "blockinput.h"
#include "buffer.h"
#include "charset.h"
#include "coding.h"

/* This may include sys/types.h, and that somehow loses
   if this is not done before the other system files.  */
#include "w32term.h"

/* Load sys/types.h if not already loaded.
   In some systems loading it twice is suicidal.  */
#ifndef makedev
#include <sys/types.h>
#endif

#include "dispextern.h"

#undef HAVE_DIALOGS /* TODO: Implement native dialogs.  */

/******************************************************************/
/* Definitions copied from lwlib.h */

typedef void * XtPointer;
typedef char Boolean;

enum button_type
{
  BUTTON_TYPE_NONE,
  BUTTON_TYPE_TOGGLE,
  BUTTON_TYPE_RADIO
};

/* This structure is based on the one in ../lwlib/lwlib.h, modified
   for Windows.  */
typedef struct _widget_value
{
  /* name of widget */
  Lisp_Object   lname;
  char*		name;
  /* value (meaning depend on widget type) */
  char*		value;
  /* keyboard equivalent. no implications for XtTranslations */
  Lisp_Object   lkey;
  char*		key;
  /* Help string or nil if none.
     GC finds this string through the frame's menu_bar_vector
     or through menu_items.  */
  Lisp_Object	help;
  /* true if enabled */
  Boolean	enabled;
  /* true if selected */
  Boolean	selected;
  /* The type of a button.  */
  enum button_type button_type;
  /* true if menu title */
  Boolean       title;
#if 0
  /* true if was edited (maintained by get_value) */
  Boolean	edited;
  /* true if has changed (maintained by lw library) */
  change_type	change;
  /* true if this widget itself has changed,
     but not counting the other widgets found in the `next' field.  */
  change_type   this_one_change;
#endif
  /* Contents of the sub-widgets, also selected slot for checkbox */
  struct _widget_value*	contents;
  /* data passed to callback */
  XtPointer	call_data;
  /* next one in the list */
  struct _widget_value*	next;
#if 0
  /* slot for the toolkit dependent part.  Always initialize to NULL. */
  void* toolkit_data;
  /* tell us if we should free the toolkit data slot when freeing the
     widget_value itself. */
  Boolean free_toolkit_data;

  /* we resource the widget_value structures; this points to the next
     one on the free list if this one has been deallocated.
   */
  struct _widget_value *free_list;
#endif
} widget_value;

/* Local memory management */
#define local_heap (GetProcessHeap ())
#define local_alloc(n) (HeapAlloc (local_heap, HEAP_ZERO_MEMORY, (n)))
#define local_free(p) (HeapFree (local_heap, 0, ((LPVOID) (p))))

#define malloc_widget_value() ((widget_value *) local_alloc (sizeof (widget_value)))
#define free_widget_value(wv) (local_free ((wv)))

/******************************************************************/

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif /* no TRUE */

HMENU current_popup_menu;

void syms_of_w32menu ();
void globals_of_w32menu ();

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

GetMenuItemInfoA_Proc get_menu_item_info = NULL;
SetMenuItemInfoA_Proc set_menu_item_info = NULL;
AppendMenuW_Proc unicode_append_menu = NULL;

Lisp_Object Qdebug_on_next_call;

extern Lisp_Object Vmenu_updating_frame;

extern Lisp_Object Qmenu_bar;

extern Lisp_Object QCtoggle, QCradio;

extern Lisp_Object Voverriding_local_map;
extern Lisp_Object Voverriding_local_map_menu_flag;

extern Lisp_Object Qoverriding_local_map, Qoverriding_terminal_local_map;

extern Lisp_Object Qmenu_bar_update_hook;

void set_frame_menubar ();

static void push_menu_item P_ ((Lisp_Object, Lisp_Object, Lisp_Object,
				Lisp_Object, Lisp_Object, Lisp_Object,
				Lisp_Object, Lisp_Object));
#ifdef HAVE_DIALOGS
static Lisp_Object w32_dialog_show ();
#endif
static Lisp_Object w32_menu_show ();

static void keymap_panes ();
static void single_keymap_panes ();
static void single_menu_item ();
static void list_of_panes ();
static void list_of_items ();
void w32_free_menu_strings (HWND);

/* This holds a Lisp vector that holds the results of decoding
   the keymaps or alist-of-alists that specify a menu.

   It describes the panes and items within the panes.

   Each pane is described by 3 elements in the vector:
   t, the pane name, the pane's prefix key.
   Then follow the pane's items, with 5 elements per item:
   the item string, the enable flag, the item's value,
   the definition, and the equivalent keyboard key's description string.

   In some cases, multiple levels of menus may be described.
   A single vector slot containing nil indicates the start of a submenu.
   A single vector slot containing lambda indicates the end of a submenu.
   The submenu follows a menu item which is the way to reach the submenu.

   A single vector slot containing quote indicates that the
   following items should appear on the right of a dialog box.

   Using a Lisp vector to hold this information while we decode it
   takes care of protecting all the data from GC.  */

#define MENU_ITEMS_PANE_NAME 1
#define MENU_ITEMS_PANE_PREFIX 2
#define MENU_ITEMS_PANE_LENGTH 3

enum menu_item_idx
{
  MENU_ITEMS_ITEM_NAME = 0,
  MENU_ITEMS_ITEM_ENABLE,
  MENU_ITEMS_ITEM_VALUE,
  MENU_ITEMS_ITEM_EQUIV_KEY,
  MENU_ITEMS_ITEM_DEFINITION,
  MENU_ITEMS_ITEM_TYPE,
  MENU_ITEMS_ITEM_SELECTED,
  MENU_ITEMS_ITEM_HELP,
  MENU_ITEMS_ITEM_LENGTH
};

static Lisp_Object menu_items;

/* Number of slots currently allocated in menu_items.  */
static int menu_items_allocated;

/* This is the index in menu_items of the first empty slot.  */
static int menu_items_used;

/* The number of panes currently recorded in menu_items,
   excluding those within submenus.  */
static int menu_items_n_panes;

/* Current depth within submenus.  */
static int menu_items_submenu_depth;

static int next_menubar_widget_id;

/* This is set nonzero after the user activates the menu bar, and set
   to zero again after the menu bars are redisplayed by prepare_menu_bar.
   While it is nonzero, all calls to set_frame_menubar go deep.

   I don't understand why this is needed, but it does seem to be
   needed on Motif, according to Marcus Daniels <marcus@sysc.pdx.edu>.  */

int pending_menu_activation;


/* Return the frame whose ->output_data.w32->menubar_widget equals
   ID, or 0 if none.  */

static struct frame *
menubar_id_to_frame (id)
     HMENU id;
{
  Lisp_Object tail, frame;
  FRAME_PTR f;

  for (tail = Vframe_list; GC_CONSP (tail); tail = XCDR (tail))
    {
      frame = XCAR (tail);
      if (!GC_FRAMEP (frame))
        continue;
      f = XFRAME (frame);
      if (!FRAME_WINDOW_P (f))
	continue;
      if (f->output_data.w32->menubar_widget == id)
	return f;
    }
  return 0;
}

/* Initialize the menu_items structure if we haven't already done so.
   Also mark it as currently empty.  */

static void
init_menu_items ()
{
  if (NILP (menu_items))
    {
      menu_items_allocated = 60;
      menu_items = Fmake_vector (make_number (menu_items_allocated), Qnil);
    }

  menu_items_used = 0;
  menu_items_n_panes = 0;
  menu_items_submenu_depth = 0;
}

/* Call at the end of generating the data in menu_items.
   This fills in the number of items in the last pane.  */

static void
finish_menu_items ()
{
}

/* Call when finished using the data for the current menu
   in menu_items.  */

static void
discard_menu_items ()
{
  /* Free the structure if it is especially large.
     Otherwise, hold on to it, to save time.  */
  if (menu_items_allocated > 200)
    {
      menu_items = Qnil;
      menu_items_allocated = 0;
    }
}

/* Make the menu_items vector twice as large.  */

static void
grow_menu_items ()
{
  Lisp_Object old;
  int old_size = menu_items_allocated;
  old = menu_items;

  menu_items_allocated *= 2;
  menu_items = Fmake_vector (make_number (menu_items_allocated), Qnil);
  bcopy (XVECTOR (old)->contents, XVECTOR (menu_items)->contents,
	 old_size * sizeof (Lisp_Object));
}

/* Begin a submenu.  */

static void
push_submenu_start ()
{
  if (menu_items_used + 1 > menu_items_allocated)
    grow_menu_items ();

  ASET (menu_items, menu_items_used++, Qnil);
  menu_items_submenu_depth++;
}

/* End a submenu.  */

static void
push_submenu_end ()
{
  if (menu_items_used + 1 > menu_items_allocated)
    grow_menu_items ();

  ASET (menu_items, menu_items_used++, Qlambda);
  menu_items_submenu_depth--;
}

/* Indicate boundary between left and right.  */

static void
push_left_right_boundary ()
{
  if (menu_items_used + 1 > menu_items_allocated)
    grow_menu_items ();

  ASET (menu_items, menu_items_used++, Qquote);
}

/* Start a new menu pane in menu_items.
   NAME is the pane name.  PREFIX_VEC is a prefix key for this pane.  */

static void
push_menu_pane (name, prefix_vec)
     Lisp_Object name, prefix_vec;
{
  if (menu_items_used + MENU_ITEMS_PANE_LENGTH > menu_items_allocated)
    grow_menu_items ();

  if (menu_items_submenu_depth == 0)
    menu_items_n_panes++;
  ASET (menu_items, menu_items_used++, Qt);
  ASET (menu_items, menu_items_used++, name);
  ASET (menu_items, menu_items_used++, prefix_vec);
}

/* Push one menu item into the current pane.  NAME is the string to
   display.  ENABLE if non-nil means this item can be selected.  KEY
   is the key generated by choosing this item, or nil if this item
   doesn't really have a definition.  DEF is the definition of this
   item.  EQUIV is the textual description of the keyboard equivalent
   for this item (or nil if none).  TYPE is the type of this menu
   item, one of nil, `toggle' or `radio'. */

static void
push_menu_item (name, enable, key, def, equiv, type, selected, help)
     Lisp_Object name, enable, key, def, equiv, type, selected, help;
{
  if (menu_items_used + MENU_ITEMS_ITEM_LENGTH > menu_items_allocated)
    grow_menu_items ();

  ASET (menu_items, menu_items_used++, name);
  ASET (menu_items, menu_items_used++, enable);
  ASET (menu_items, menu_items_used++, key);
  ASET (menu_items, menu_items_used++, equiv);
  ASET (menu_items, menu_items_used++, def);
  ASET (menu_items, menu_items_used++, type);
  ASET (menu_items, menu_items_used++, selected);
  ASET (menu_items, menu_items_used++, help);
}

/* Look through KEYMAPS, a vector of keymaps that is NMAPS long,
   and generate menu panes for them in menu_items.
   If NOTREAL is nonzero,
   don't bother really computing whether an item is enabled.  */

static void
keymap_panes (keymaps, nmaps, notreal)
     Lisp_Object *keymaps;
     int nmaps;
     int notreal;
{
  int mapno;

  init_menu_items ();

  /* Loop over the given keymaps, making a pane for each map.
     But don't make a pane that is empty--ignore that map instead.
     P is the number of panes we have made so far.  */
  for (mapno = 0; mapno < nmaps; mapno++)
    single_keymap_panes (keymaps[mapno],
                         Fkeymap_prompt (keymaps[mapno]), Qnil, notreal, 10);

  finish_menu_items ();
}

/* This is a recursive subroutine of keymap_panes.
   It handles one keymap, KEYMAP.
   The other arguments are passed along
   or point to local variables of the previous function.
   If NOTREAL is nonzero, only check for equivalent key bindings, don't
   evaluate expressions in menu items and don't make any menu.

   If we encounter submenus deeper than MAXDEPTH levels, ignore them.  */

static void
single_keymap_panes (keymap, pane_name, prefix, notreal, maxdepth)
     Lisp_Object keymap;
     Lisp_Object pane_name;
     Lisp_Object prefix;
     int notreal;
     int maxdepth;
{
  Lisp_Object pending_maps = Qnil;
  Lisp_Object tail, item;
  struct gcpro gcpro1, gcpro2;

  if (maxdepth <= 0)
    return;

  push_menu_pane (pane_name, prefix);

  for (tail = keymap; CONSP (tail); tail = XCDR (tail))
    {
      GCPRO2 (keymap, pending_maps);
      /* Look at each key binding, and if it is a menu item add it
	 to this menu.  */
      item = XCAR (tail);
      if (CONSP (item))
	single_menu_item (XCAR (item), XCDR (item),
			  &pending_maps, notreal, maxdepth);
      else if (VECTORP (item))
	{
	  /* Loop over the char values represented in the vector.  */
	  int len = ASIZE (item);
	  int c;
	  for (c = 0; c < len; c++)
	    {
	      Lisp_Object character;
	      XSETFASTINT (character, c);
	      single_menu_item (character, AREF (item, c),
				&pending_maps, notreal, maxdepth);
	    }
	}
      UNGCPRO;
    }

  /* Process now any submenus which want to be panes at this level.  */
  while (!NILP (pending_maps))
    {
      Lisp_Object elt, eltcdr, string;
      elt = Fcar (pending_maps);
      eltcdr = XCDR (elt);
      string = XCAR (eltcdr);
      /* We no longer discard the @ from the beginning of the string here.
	 Instead, we do this in w32_menu_show.  */
      single_keymap_panes (Fcar (elt), string,
			   XCDR (eltcdr), notreal, maxdepth - 1);
      pending_maps = Fcdr (pending_maps);
    }
}

/* This is a subroutine of single_keymap_panes that handles one
   keymap entry.
   KEY is a key in a keymap and ITEM is its binding.
   PENDING_MAPS_PTR points to a list of keymaps waiting to be made into
   separate panes.
   If NOTREAL is nonzero, only check for equivalent key bindings, don't
   evaluate expressions in menu items and don't make any menu.
   If we encounter submenus deeper than MAXDEPTH levels, ignore them.  */

static void
single_menu_item (key, item, pending_maps_ptr, notreal, maxdepth)
     Lisp_Object key, item;
     Lisp_Object *pending_maps_ptr;
     int maxdepth, notreal;
{
  Lisp_Object map, item_string, enabled;
  struct gcpro gcpro1, gcpro2;
  int res;

  /* Parse the menu item and leave the result in item_properties.  */
  GCPRO2 (key, item);
  res = parse_menu_item (item, notreal, 0);
  UNGCPRO;
  if (!res)
    return;			/* Not a menu item.  */

  map = AREF (item_properties, ITEM_PROPERTY_MAP);

  if (notreal)
    {
      /* We don't want to make a menu, just traverse the keymaps to
	 precompute equivalent key bindings.  */
      if (!NILP (map))
	single_keymap_panes (map, Qnil, key, 1, maxdepth - 1);
      return;
    }

  enabled = AREF (item_properties, ITEM_PROPERTY_ENABLE);
  item_string = AREF (item_properties, ITEM_PROPERTY_NAME);

  if (!NILP (map) && SREF (item_string, 0) == '@')
    {
      if (!NILP (enabled))
	/* An enabled separate pane. Remember this to handle it later.  */
	*pending_maps_ptr = Fcons (Fcons (map, Fcons (item_string, key)),
				   *pending_maps_ptr);
      return;
    }

  push_menu_item (item_string, enabled, key,
		  AREF (item_properties, ITEM_PROPERTY_DEF),
		  AREF (item_properties, ITEM_PROPERTY_KEYEQ),
		  AREF (item_properties, ITEM_PROPERTY_TYPE),
                  AREF (item_properties, ITEM_PROPERTY_SELECTED),
                  AREF (item_properties, ITEM_PROPERTY_HELP));

  /* Display a submenu using the toolkit.  */
  if (! (NILP (map) || NILP (enabled)))
    {
      push_submenu_start ();
      single_keymap_panes (map, Qnil, key, 0, maxdepth - 1);
      push_submenu_end ();
    }
}

/* Push all the panes and items of a menu described by the
   alist-of-alists MENU.
   This handles old-fashioned calls to x-popup-menu.  */

static void
list_of_panes (menu)
     Lisp_Object menu;
{
  Lisp_Object tail;

  init_menu_items ();

  for (tail = menu; !NILP (tail); tail = Fcdr (tail))
    {
      Lisp_Object elt, pane_name, pane_data;
      elt = Fcar (tail);
      pane_name = Fcar (elt);
      CHECK_STRING (pane_name);
      push_menu_pane (pane_name, Qnil);
      pane_data = Fcdr (elt);
      CHECK_CONS (pane_data);
      list_of_items (pane_data);
    }

  finish_menu_items ();
}

/* Push the items in a single pane defined by the alist PANE.  */

static void
list_of_items (pane)
     Lisp_Object pane;
{
  Lisp_Object tail, item, item1;

  for (tail = pane; !NILP (tail); tail = Fcdr (tail))
    {
      item = Fcar (tail);
      if (STRINGP (item))
	push_menu_item (item, Qnil, Qnil, Qt, Qnil, Qnil, Qnil, Qnil);
      else if (NILP (item))
	push_left_right_boundary ();
      else
	{
	  CHECK_CONS (item);
	  item1 = Fcar (item);
	  CHECK_STRING (item1);
	  push_menu_item (item1, Qt, Fcdr (item), Qt, Qnil, Qnil, Qnil, Qnil);
	}
    }
}

DEFUN ("x-popup-menu", Fx_popup_menu, Sx_popup_menu, 2, 2, 0,
       doc: /* Pop up a deck-of-cards menu and return user's selection.
POSITION is a position specification.  This is either a mouse button
event or a list ((XOFFSET YOFFSET) WINDOW) where XOFFSET and YOFFSET
are positions in pixels from the top left corner of WINDOW's frame
\(WINDOW may be a frame object instead of a window).  This controls the
position of the center of the first line in the first pane of the
menu, not the top left of the menu as a whole.  If POSITION is t, it
means to use the current mouse position.

MENU is a specifier for a menu.  For the simplest case, MENU is a keymap.
The menu items come from key bindings that have a menu string as well as
a definition; actually, the \"definition\" in such a key binding looks like
\(STRING . REAL-DEFINITION).  To give the menu a title, put a string into
the keymap as a top-level element.

If REAL-DEFINITION is nil, that puts a nonselectable string in the menu.
Otherwise, REAL-DEFINITION should be a valid key binding definition.

You can also use a list of keymaps as MENU.  Then each keymap makes a
separate pane.  When MENU is a keymap or a list of keymaps, the return
value is a list of events.

Alternatively, you can specify a menu of multiple panes with a list of
the form (TITLE PANE1 PANE2...), where each pane is a list of
form (TITLE ITEM1 ITEM2...).
Each ITEM is normally a cons cell (STRING . VALUE); but a string can
appear as an item--that makes a nonselectable line in the menu.
With this form of menu, the return value is VALUE from the chosen item.

If POSITION is nil, don't display the menu at all, just precalculate the
cached information about equivalent key sequences.  */)
  (position, menu)
     Lisp_Object position, menu;
{
  Lisp_Object keymap, tem;
  int xpos = 0, ypos = 0;
  Lisp_Object title;
  char *error_name;
  Lisp_Object selection;
  FRAME_PTR f = NULL;
  Lisp_Object x, y, window;
  int keymaps = 0;
  int for_click = 0;
  struct gcpro gcpro1;

#ifdef HAVE_MENUS
  if (! NILP (position))
    {
      check_w32 ();

      /* Decode the first argument: find the window and the coordinates.  */
      if (EQ (position, Qt)
	  || (CONSP (position) && (EQ (XCAR (position), Qmenu_bar)
                                   || EQ (XCAR (position), Qtool_bar))))
	{
	  /* Use the mouse's current position.  */
	  FRAME_PTR new_f = SELECTED_FRAME ();
	  Lisp_Object bar_window;
	  enum scroll_bar_part part;
	  unsigned long time;

	  if (mouse_position_hook)
	    (*mouse_position_hook) (&new_f, 1, &bar_window,
				    &part, &x, &y, &time);
	  if (new_f != 0)
	    XSETFRAME (window, new_f);
	  else
	    {
	      window = selected_window;
	      XSETFASTINT (x, 0);
	      XSETFASTINT (y, 0);
	    }
	}
      else
	{
	  tem = Fcar (position);
	  if (CONSP (tem))
	    {
	      window = Fcar (Fcdr (position));
	      x = Fcar (tem);
	      y = Fcar (Fcdr (tem));
	    }
	  else
	    {
	      for_click = 1;
	      tem = Fcar (Fcdr (position));  /* EVENT_START (position) */
	      window = Fcar (tem);	     /* POSN_WINDOW (tem) */
	      tem = Fcar (Fcdr (Fcdr (tem))); /* POSN_WINDOW_POSN (tem) */
	      x = Fcar (tem);
	      y = Fcdr (tem);
	    }
	}

      CHECK_NUMBER (x);
      CHECK_NUMBER (y);

      /* Decode where to put the menu.  */

      if (FRAMEP (window))
	{
	  f = XFRAME (window);
	  xpos = 0;
	  ypos = 0;
	}
      else if (WINDOWP (window))
	{
	  CHECK_LIVE_WINDOW (window);
	  f = XFRAME (WINDOW_FRAME (XWINDOW (window)));

	  xpos = WINDOW_LEFT_EDGE_X (XWINDOW (window));
	  ypos = WINDOW_TOP_EDGE_Y (XWINDOW (window));
	}
      else
	/* ??? Not really clean; should be CHECK_WINDOW_OR_FRAME,
	   but I don't want to make one now.  */
	CHECK_WINDOW (window);

      xpos += XINT (x);
      ypos += XINT (y);

      XSETFRAME (Vmenu_updating_frame, f);
    }
  else
    Vmenu_updating_frame = Qnil;
#endif /* HAVE_MENUS */

  title = Qnil;
  GCPRO1 (title);

  /* Decode the menu items from what was specified.  */

  keymap = get_keymap (menu, 0, 0);
  if (CONSP (keymap))
    {
      /* We were given a keymap.  Extract menu info from the keymap.  */
      Lisp_Object prompt;

      /* Extract the detailed info to make one pane.  */
      keymap_panes (&menu, 1, NILP (position));

      /* Search for a string appearing directly as an element of the keymap.
	 That string is the title of the menu.  */
      prompt = Fkeymap_prompt (keymap);
      if (NILP (title) && !NILP (prompt))
	title = prompt;

      /* Make that be the pane title of the first pane.  */
      if (!NILP (prompt) && menu_items_n_panes >= 0)
	ASET (menu_items, MENU_ITEMS_PANE_NAME, prompt);

      keymaps = 1;
    }
  else if (CONSP (menu) && KEYMAPP (XCAR (menu)))
    {
      /* We were given a list of keymaps.  */
      int nmaps = XFASTINT (Flength (menu));
      Lisp_Object *maps
	= (Lisp_Object *) alloca (nmaps * sizeof (Lisp_Object));
      int i;

      title = Qnil;

      /* The first keymap that has a prompt string
	 supplies the menu title.  */
      for (tem = menu, i = 0; CONSP (tem); tem = Fcdr (tem))
	{
	  Lisp_Object prompt;

	  maps[i++] = keymap = get_keymap (Fcar (tem), 1, 0);

	  prompt = Fkeymap_prompt (keymap);
	  if (NILP (title) && !NILP (prompt))
	    title = prompt;
	}

      /* Extract the detailed info to make one pane.  */
      keymap_panes (maps, nmaps, NILP (position));

      /* Make the title be the pane title of the first pane.  */
      if (!NILP (title) && menu_items_n_panes >= 0)
	ASET (menu_items, MENU_ITEMS_PANE_NAME, title);

      keymaps = 1;
    }
  else
    {
      /* We were given an old-fashioned menu.  */
      title = Fcar (menu);
      CHECK_STRING (title);

      list_of_panes (Fcdr (menu));

      keymaps = 0;
    }

  if (NILP (position))
    {
      discard_menu_items ();
      UNGCPRO;
      return Qnil;
    }

#ifdef HAVE_MENUS
  /* If resources from a previous popup menu still exist, does nothing
     until the `menu_free_timer' has freed them (see w32fns.c). This
     can occur if you press ESC or click outside a menu without selecting
     a menu item.
  */
  if (current_popup_menu)
    {
      discard_menu_items ();
      UNGCPRO;
      return Qnil;
    }

  /* Display them in a menu.  */
  BLOCK_INPUT;

  selection = w32_menu_show (f, xpos, ypos, for_click,
			     keymaps, title, &error_name);
  UNBLOCK_INPUT;

  discard_menu_items ();

#endif /* HAVE_MENUS */

  UNGCPRO;

  if (error_name) error (error_name);
  return selection;
}

#ifdef HAVE_MENUS

DEFUN ("x-popup-dialog", Fx_popup_dialog, Sx_popup_dialog, 2, 3, 0,
       doc: /* Pop up a dialog box and return user's selection.
POSITION specifies which frame to use.
This is normally a mouse button event or a window or frame.
If POSITION is t, it means to use the frame the mouse is on.
The dialog box appears in the middle of the specified frame.

CONTENTS specifies the alternatives to display in the dialog box.
It is a list of the form (TITLE ITEM1 ITEM2...).
Each ITEM is a cons cell (STRING . VALUE).
The return value is VALUE from the chosen item.

An ITEM may also be just a string--that makes a nonselectable item.
An ITEM may also be nil--that means to put all preceding items
on the left of the dialog box and all following items on the right.
\(By default, approximately half appear on each side.)

If HEADER is non-nil, the frame title for the box is "Information",
otherwise it is "Question". */)
  (position, contents, header)
     Lisp_Object position, contents, header;
{
  FRAME_PTR f = NULL;
  Lisp_Object window;

  check_w32 ();

  /* Decode the first argument: find the window or frame to use.  */
  if (EQ (position, Qt)
      || (CONSP (position) && (EQ (XCAR (position), Qmenu_bar)
                               || EQ (XCAR (position), Qtool_bar))))
    {
#if 0 /* Using the frame the mouse is on may not be right.  */
      /* Use the mouse's current position.  */
      FRAME_PTR new_f = SELECTED_FRAME ();
      Lisp_Object bar_window;
      enum scroll_bar_part part;
      unsigned long time;
      Lisp_Object x, y;

      (*mouse_position_hook) (&new_f, 1, &bar_window, &part, &x, &y, &time);

      if (new_f != 0)
	XSETFRAME (window, new_f);
      else
	window = selected_window;
#endif
      window = selected_window;
    }
  else if (CONSP (position))
    {
      Lisp_Object tem;
      tem = Fcar (position);
      if (CONSP (tem))
	window = Fcar (Fcdr (position));
      else
	{
	  tem = Fcar (Fcdr (position));  /* EVENT_START (position) */
	  window = Fcar (tem);	     /* POSN_WINDOW (tem) */
	}
    }
  else if (WINDOWP (position) || FRAMEP (position))
    window = position;
  else
    window = Qnil;

  /* Decode where to put the menu.  */

  if (FRAMEP (window))
    f = XFRAME (window);
  else if (WINDOWP (window))
    {
      CHECK_LIVE_WINDOW (window);
      f = XFRAME (WINDOW_FRAME (XWINDOW (window)));
    }
  else
    /* ??? Not really clean; should be CHECK_WINDOW_OR_FRAME,
       but I don't want to make one now.  */
    CHECK_WINDOW (window);

#ifndef HAVE_DIALOGS
  /* Display a menu with these alternatives
     in the middle of frame F.  */
  {
    Lisp_Object x, y, frame, newpos;
    XSETFRAME (frame, f);
    XSETINT (x, x_pixel_width (f) / 2);
    XSETINT (y, x_pixel_height (f) / 2);
    newpos = Fcons (Fcons (x, Fcons (y, Qnil)), Fcons (frame, Qnil));

    return Fx_popup_menu (newpos,
			  Fcons (Fcar (contents), Fcons (contents, Qnil)));
  }
#else /* HAVE_DIALOGS */
  {
    Lisp_Object title;
    char *error_name;
    Lisp_Object selection;

    /* Decode the dialog items from what was specified.  */
    title = Fcar (contents);
    CHECK_STRING (title);

    list_of_panes (Fcons (contents, Qnil));

    /* Display them in a dialog box.  */
    BLOCK_INPUT;
    selection = w32_dialog_show (f, 0, title, header, &error_name);
    UNBLOCK_INPUT;

    discard_menu_items ();

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
x_activate_menubar (f)
     FRAME_PTR f;
{
  set_frame_menubar (f, 0, 1);

  /* Lock out further menubar changes while active.  */
  f->output_data.w32->menubar_active = 1;

  /* Signal input thread to return from WM_INITMENU.  */
  complete_deferred_msg (FRAME_W32_WINDOW (f), WM_INITMENU, 0);
}

/* This callback is called from the menu bar pulldown menu
   when the user makes a selection.
   Figure out what the user chose
   and put the appropriate events into the keyboard buffer.  */

void
menubar_selection_callback (FRAME_PTR f, void * client_data)
{
  Lisp_Object prefix, entry;
  Lisp_Object vector;
  Lisp_Object *subprefix_stack;
  int submenu_depth = 0;
  int i;

  if (!f)
    return;
  entry = Qnil;
  subprefix_stack = (Lisp_Object *) alloca (f->menu_bar_items_used * sizeof (Lisp_Object));
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
	  /* The EMACS_INT cast avoids a warning.  There's no problem
	     as long as pointers have enough bits to hold small integers.  */
	  if ((int) (EMACS_INT) client_data == i)
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

/* Allocate a widget_value, blocking input.  */

widget_value *
xmalloc_widget_value ()
{
  widget_value *value;

  BLOCK_INPUT;
  value = malloc_widget_value ();
  UNBLOCK_INPUT;

  return value;
}

/* This recursively calls free_widget_value on the tree of widgets.
   It must free all data that was malloc'ed for these widget_values.
   In Emacs, many slots are pointers into the data of Lisp_Strings, and
   must be left alone.  */

void
free_menubar_widget_value_tree (wv)
     widget_value *wv;
{
  if (! wv) return;

  wv->name = wv->value = wv->key = (char *) 0xDEADBEEF;

  if (wv->contents && (wv->contents != (widget_value*)1))
    {
      free_menubar_widget_value_tree (wv->contents);
      wv->contents = (widget_value *) 0xDEADBEEF;
    }
  if (wv->next)
    {
      free_menubar_widget_value_tree (wv->next);
      wv->next = (widget_value *) 0xDEADBEEF;
    }
  BLOCK_INPUT;
  free_widget_value (wv);
  UNBLOCK_INPUT;
}

/* Set up data i menu_items for a menu bar item
   whose event type is ITEM_KEY (with string ITEM_NAME)
   and whose contents come from the list of keymaps MAPS.  */

static int
parse_single_submenu (item_key, item_name, maps)
     Lisp_Object item_key, item_name, maps;
{
  Lisp_Object length;
  int len;
  Lisp_Object *mapvec;
  int i;
  int top_level_items = 0;

  length = Flength (maps);
  len = XINT (length);

  /* Convert the list MAPS into a vector MAPVEC.  */
  mapvec = (Lisp_Object *) alloca (len * sizeof (Lisp_Object));
  for (i = 0; i < len; i++)
    {
      mapvec[i] = Fcar (maps);
      maps = Fcdr (maps);
    }

  /* Loop over the given keymaps, making a pane for each map.
     But don't make a pane that is empty--ignore that map instead.  */
  for (i = 0; i < len; i++)
    {
      if (SYMBOLP (mapvec[i])
	  || (CONSP (mapvec[i]) && !KEYMAPP (mapvec[i])))
	{
	  /* Here we have a command at top level in the menu bar
	     as opposed to a submenu.  */
	  top_level_items = 1;
	  push_menu_pane (Qnil, Qnil);
	  push_menu_item (item_name, Qt, item_key, mapvec[i],
                          Qnil, Qnil, Qnil, Qnil);
	}
      else
	{
	  Lisp_Object prompt;
	  prompt = Fkeymap_prompt (mapvec[i]);
	  single_keymap_panes (mapvec[i],
			       !NILP (prompt) ? prompt : item_name,
			       item_key, 0, 10);
	}
    }

  return top_level_items;
}


/* Create a tree of widget_value objects
   representing the panes and items
   in menu_items starting at index START, up to index END.  */

static widget_value *
digest_single_submenu (start, end, top_level_items)
     int start, end, top_level_items;
{
  widget_value *wv, *prev_wv, *save_wv, *first_wv;
  int i;
  int submenu_depth = 0;
  widget_value **submenu_stack;

  submenu_stack
    = (widget_value **) alloca (menu_items_used * sizeof (widget_value *));
  wv = xmalloc_widget_value ();
  wv->name = "menu";
  wv->value = 0;
  wv->enabled = 1;
  wv->button_type = BUTTON_TYPE_NONE;
  wv->help = Qnil;
  first_wv = wv;
  save_wv = 0;
  prev_wv = 0;

  /* Loop over all panes and items made by the preceding call
     to parse_single_submenu and construct a tree of widget_value objects.
     Ignore the panes and items used by previous calls to
     digest_single_submenu, even though those are also in menu_items.  */
  i = start;
  while (i < end)
    {
      if (EQ (AREF (menu_items, i), Qnil))
	{
	  submenu_stack[submenu_depth++] = save_wv;
	  save_wv = prev_wv;
	  prev_wv = 0;
	  i++;
	}
      else if (EQ (AREF (menu_items, i), Qlambda))
	{
	  prev_wv = save_wv;
	  save_wv = submenu_stack[--submenu_depth];
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
	  char *pane_string;

	  pane_name = AREF (menu_items, i + MENU_ITEMS_PANE_NAME);
	  prefix = AREF (menu_items, i + MENU_ITEMS_PANE_PREFIX);

	  if (STRINGP (pane_name))
	    {
	      if (unicode_append_menu)
		/* Encode as UTF-8 for now.  */
		pane_name = ENCODE_UTF_8 (pane_name);
	      else if (STRING_MULTIBYTE (pane_name))
		pane_name = ENCODE_SYSTEM (pane_name);

	      ASET (menu_items, i + MENU_ITEMS_PANE_NAME, pane_name);
	    }

	  pane_string = (NILP (pane_name)
			 ? "" : (char *) SDATA (pane_name));
	  /* If there is just one top-level pane, put all its items directly
	     under the top-level menu.  */
	  if (menu_items_n_panes == 1)
	    pane_string = "";

	  /* If the pane has a meaningful name,
	     make the pane a top-level menu item
	     with its items as a submenu beneath it.  */
	  if (strcmp (pane_string, ""))
	    {
	      wv = xmalloc_widget_value ();
	      if (save_wv)
		save_wv->next = wv;
	      else
		first_wv->contents = wv;
	      wv->lname = pane_name;
	      /* Set value to 1 so update_submenu_strings can handle '@'  */
	      wv->value = (char *) 1;
	      wv->enabled = 1;
	      wv->button_type = BUTTON_TYPE_NONE;
	      wv->help = Qnil;
	    }
	  save_wv = wv;
	  prev_wv = 0;
	  i += MENU_ITEMS_PANE_LENGTH;
	}
      else
	{
	  /* Create a new item within current pane.  */
	  Lisp_Object item_name, enable, descrip, def, type, selected;
          Lisp_Object help;

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

	  wv = xmalloc_widget_value ();
	  if (prev_wv)
	    prev_wv->next = wv;
	  else
	    save_wv->contents = wv;

	  wv->lname = item_name;
	  if (!NILP (descrip))
	    wv->lkey = descrip;
	  wv->value = 0;
	  /* The EMACS_INT cast avoids a warning.  There's no problem
	     as long as pointers have enough bits to hold small integers.  */
	  wv->call_data = (!NILP (def) ? (void *) (EMACS_INT) i : 0);
	  wv->enabled = !NILP (enable);

	  if (NILP (type))
	    wv->button_type = BUTTON_TYPE_NONE;
	  else if (EQ (type, QCradio))
	    wv->button_type = BUTTON_TYPE_RADIO;
	  else if (EQ (type, QCtoggle))
	    wv->button_type = BUTTON_TYPE_TOGGLE;
	  else
	    abort ();

	  wv->selected = !NILP (selected);
	  if (!STRINGP (help))
	    help = Qnil;

	  wv->help = help;

	  prev_wv = wv;

	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }

  /* If we have just one "menu item"
     that was originally a button, return it by itself.  */
  if (top_level_items && first_wv->contents && first_wv->contents->next == 0)
    {
      wv = first_wv->contents;
      free_widget_value (first_wv);
      return wv;
    }

  return first_wv;
}


/* Walk through the widget_value tree starting at FIRST_WV and update
   the char * pointers from the corresponding lisp values.
   We do this after building the whole tree, since GC may happen while the
   tree is constructed, and small strings are relocated.  So we must wait
   until no GC can happen before storing pointers into lisp values.  */
static void
update_submenu_strings (first_wv)
     widget_value *first_wv;
{
  widget_value *wv;

  for (wv = first_wv; wv; wv = wv->next)
    {
      if (wv->lname && ! NILP (wv->lname))
        {
          wv->name = SDATA (wv->lname);

          /* Ignore the @ that means "separate pane".
             This is a kludge, but this isn't worth more time.  */
          if (wv->value == (char *)1)
            {
              if (wv->name[0] == '@')
		wv->name++;
              wv->value = 0;
            }
        }

      if (wv->lkey && ! NILP (wv->lkey))
        wv->key = SDATA (wv->lkey);

      if (wv->contents)
        update_submenu_strings (wv->contents);
    }
}


/* Set the contents of the menubar widgets of frame F.
   The argument FIRST_TIME is currently ignored;
   it is set the first time this is called, from initialize_frame_menubar.  */

void
set_frame_menubar (f, first_time, deep_p)
     FRAME_PTR f;
     int first_time;
     int deep_p;
{
  HMENU menubar_widget = f->output_data.w32->menubar_widget;
  Lisp_Object items;
  widget_value *wv, *first_wv, *prev_wv = 0;
  int i, last_i;
  int *submenu_start, *submenu_end;
  int *submenu_top_level_items, *submenu_n_panes;

  /* We must not change the menubar when actually in use.  */
  if (f->output_data.w32->menubar_active)
    return;

  XSETFRAME (Vmenu_updating_frame, f);

  if (! menubar_widget)
    deep_p = 1;
  else if (pending_menu_activation && !deep_p)
    deep_p = 1;

  if (deep_p)
    {
      /* Make a widget-value tree representing the entire menu trees.  */

      struct buffer *prev = current_buffer;
      Lisp_Object buffer;
      int specpdl_count = SPECPDL_INDEX ();
      int previous_menu_items_used = f->menu_bar_items_used;
      Lisp_Object *previous_items
	= (Lisp_Object *) alloca (previous_menu_items_used
				  * sizeof (Lisp_Object));

      /* If we are making a new widget, its contents are empty,
	 do always reinitialize them.  */
      if (! menubar_widget)
	previous_menu_items_used = 0;

      buffer = XWINDOW (FRAME_SELECTED_WINDOW (f))->buffer;
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
      FRAME_MENU_BAR_ITEMS (f) = menu_bar_items (FRAME_MENU_BAR_ITEMS (f));

      items = FRAME_MENU_BAR_ITEMS (f);

      /* Save the frame's previous menu bar contents data.  */
      if (previous_menu_items_used)
	bcopy (XVECTOR (f->menu_bar_vector)->contents, previous_items,
	       previous_menu_items_used * sizeof (Lisp_Object));

      /* Fill in menu_items with the current menu bar contents.
	 This can evaluate Lisp code.  */
      menu_items = f->menu_bar_vector;
      menu_items_allocated = VECTORP (menu_items) ? ASIZE (menu_items) : 0;
      submenu_start = (int *) alloca (XVECTOR (items)->size * sizeof (int *));
      submenu_end = (int *) alloca (XVECTOR (items)->size * sizeof (int *));
      submenu_n_panes = (int *) alloca (XVECTOR (items)->size * sizeof (int));
      submenu_top_level_items
	= (int *) alloca (XVECTOR (items)->size * sizeof (int *));
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

      wv = xmalloc_widget_value ();
      wv->name = "menubar";
      wv->value = 0;
      wv->enabled = 1;
      wv->button_type = BUTTON_TYPE_NONE;
      wv->help = Qnil;
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
	  wv->enabled = 1;
	  wv->button_type = BUTTON_TYPE_NONE;
	  prev_wv = wv;
	}

      set_buffer_internal_1 (prev);
      unbind_to (specpdl_count, Qnil);

      /* If there has been no change in the Lisp-level contents
	 of the menu bar, skip redisplaying it.  Just exit.  */

      for (i = 0; i < previous_menu_items_used; i++)
	if (menu_items_used == i
	    || (!EQ (previous_items[i], AREF (menu_items, i))))
	  break;
      if (i == menu_items_used && i == previous_menu_items_used && i != 0)
	{
	  free_menubar_widget_value_tree (first_wv);
	  menu_items = Qnil;

	  return;
	}

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
	  wv->name = (char *) SDATA (string);
	  update_submenu_strings (wv->contents);
	  wv = wv->next;
	}

      f->menu_bar_vector = menu_items;
      f->menu_bar_items_used = menu_items_used;
      menu_items = Qnil;
    }
  else
    {
      /* Make a widget-value tree containing
	 just the top level menu bar strings.  */

      wv = xmalloc_widget_value ();
      wv->name = "menubar";
      wv->value = 0;
      wv->enabled = 1;
      wv->button_type = BUTTON_TYPE_NONE;
      wv->help = Qnil;
      first_wv = wv;

      items = FRAME_MENU_BAR_ITEMS (f);
      for (i = 0; i < ASIZE (items); i += 4)
	{
	  Lisp_Object string;

	  string = AREF (items, i + 1);
	  if (NILP (string))
	    break;

	  wv = xmalloc_widget_value ();
	  wv->name = (char *) SDATA (string);
	  wv->value = 0;
	  wv->enabled = 1;
	  wv->button_type = BUTTON_TYPE_NONE;
	  wv->help = Qnil;
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

  BLOCK_INPUT;

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
      x_set_window_size (f, 0, FRAME_COLS (f), FRAME_LINES (f));
  }

  UNBLOCK_INPUT;
}

/* Called from Fx_create_frame to create the initial menubar of a frame
   before it is mapped, so that the window is mapped with the menubar already
   there instead of us tacking it on later and thrashing the window after it
   is visible.  */

void
initialize_frame_menubar (f)
     FRAME_PTR f;
{
  /* This function is called before the first chance to redisplay
     the frame.  It has to be, so the frame will have the right size.  */
  FRAME_MENU_BAR_ITEMS (f) = menu_bar_items (FRAME_MENU_BAR_ITEMS (f));
  set_frame_menubar (f, 1, 1);
}

/* Get rid of the menu bar of frame F, and free its storage.
   This is used when deleting a frame, and when turning off the menu bar.  */

void
free_frame_menubar (f)
     FRAME_PTR f;
{
  BLOCK_INPUT;

  {
    HMENU old = GetMenu (FRAME_W32_WINDOW (f));
    SetMenu (FRAME_W32_WINDOW (f), NULL);
    f->output_data.w32->menubar_widget = NULL;
    DestroyMenu (old);
  }

  UNBLOCK_INPUT;
}


/* w32_menu_show actually displays a menu using the panes and items in
   menu_items and returns the value selected from it; we assume input
   is blocked by the caller.  */

/* F is the frame the menu is for.
   X and Y are the frame-relative specified position,
   relative to the inside upper left corner of the frame F.
   FOR_CLICK is nonzero if this menu was invoked for a mouse click.
   KEYMAPS is 1 if this menu was specified with keymaps;
    in that case, we return a list containing the chosen item's value
    and perhaps also the pane's prefix.
   TITLE is the specified menu title.
   ERROR is a place to store an error message string in case of failure.
   (We return nil on failure, but the value doesn't actually matter.)  */

static Lisp_Object
w32_menu_show (f, x, y, for_click, keymaps, title, error)
     FRAME_PTR f;
     int x;
     int y;
     int for_click;
     int keymaps;
     Lisp_Object title;
     char **error;
{
  int i;
  int menu_item_selection;
  HMENU menu;
  POINT pos;
  widget_value *wv, *save_wv = 0, *first_wv = 0, *prev_wv = 0;
  widget_value **submenu_stack
    = (widget_value **) alloca (menu_items_used * sizeof (widget_value *));
  Lisp_Object *subprefix_stack
    = (Lisp_Object *) alloca (menu_items_used * sizeof (Lisp_Object));
  int submenu_depth = 0;
  int first_pane;

  *error = NULL;

  if (menu_items_used <= MENU_ITEMS_PANE_LENGTH)
    {
      *error = "Empty menu";
      return Qnil;
    }

  /* Create a tree of widget_value objects
     representing the panes and their items.  */
  wv = xmalloc_widget_value ();
  wv->name = "menu";
  wv->value = 0;
  wv->enabled = 1;
  wv->button_type = BUTTON_TYPE_NONE;
  wv->help = Qnil;
  first_wv = wv;
  first_pane = 1;

  /* Loop over all panes and items, filling in the tree.  */
  i = 0;
  while (i < menu_items_used)
    {
      if (EQ (AREF (menu_items, i), Qnil))
	{
	  submenu_stack[submenu_depth++] = save_wv;
	  save_wv = prev_wv;
	  prev_wv = 0;
	  first_pane = 1;
	  i++;
	}
      else if (EQ (AREF (menu_items, i), Qlambda))
	{
	  prev_wv = save_wv;
	  save_wv = submenu_stack[--submenu_depth];
	  first_pane = 0;
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
	  char *pane_string;
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
			 ? "" : (char *) SDATA (pane_name));
	  /* If there is just one top-level pane, put all its items directly
	     under the top-level menu.  */
	  if (menu_items_n_panes == 1)
	    pane_string = "";

	  /* If the pane has a meaningful name,
	     make the pane a top-level menu item
	     with its items as a submenu beneath it.  */
	  if (!keymaps && strcmp (pane_string, ""))
	    {
	      wv = xmalloc_widget_value ();
	      if (save_wv)
		save_wv->next = wv;
	      else
		first_wv->contents = wv;
	      wv->name = pane_string;
	      if (keymaps && !NILP (prefix))
		wv->name++;
	      wv->value = 0;
	      wv->enabled = 1;
	      wv->button_type = BUTTON_TYPE_NONE;
	      wv->help = Qnil;
	      save_wv = wv;
	      prev_wv = 0;
	    }
	  else if (first_pane)
	    {
	      save_wv = wv;
	      prev_wv = 0;
	    }
	  first_pane = 0;
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

	  wv = xmalloc_widget_value ();
	  if (prev_wv)
	    prev_wv->next = wv;
	  else
	    save_wv->contents = wv;
	  wv->name = (char *) SDATA (item_name);
	  if (!NILP (descrip))
	    wv->key = (char *) SDATA (descrip);
	  wv->value = 0;
	  /* Use the contents index as call_data, since we are
             restricted to 16-bits.  */
	  wv->call_data = !NILP (def) ? (void *) (EMACS_INT) i : 0;
	  wv->enabled = !NILP (enable);

	  if (NILP (type))
	    wv->button_type = BUTTON_TYPE_NONE;
	  else if (EQ (type, QCtoggle))
	    wv->button_type = BUTTON_TYPE_TOGGLE;
	  else if (EQ (type, QCradio))
	    wv->button_type = BUTTON_TYPE_RADIO;
	  else
	    abort ();

	  wv->selected = !NILP (selected);
          if (!STRINGP (help))
	    help = Qnil;

	  wv->help = help;

	  prev_wv = wv;

	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }

  /* Deal with the title, if it is non-nil.  */
  if (!NILP (title))
    {
      widget_value *wv_title = xmalloc_widget_value ();
      widget_value *wv_sep = xmalloc_widget_value ();

      /* Maybe replace this separator with a bitmap or owner-draw item
	 so that it looks better.  Having two separators looks odd.  */
      wv_sep->name = "--";
      wv_sep->next = first_wv->contents;
      wv_sep->help = Qnil;

      if (unicode_append_menu)
	title = ENCODE_UTF_8 (title);
      else if (STRING_MULTIBYTE (title))
	title = ENCODE_SYSTEM (title);

      wv_title->name = (char *) SDATA (title);
      wv_title->enabled = TRUE;
      wv_title->title = TRUE;
      wv_title->button_type = BUTTON_TYPE_NONE;
      wv_title->help = Qnil;
      wv_title->next = wv_sep;
      first_wv->contents = wv_title;
    }

  /* Actually create the menu.  */
  current_popup_menu = menu = CreatePopupMenu ();
  fill_in_menu (menu, first_wv->contents);

  /* Adjust coordinates to be root-window-relative.  */
  pos.x = x;
  pos.y = y;
  ClientToScreen (FRAME_W32_WINDOW (f), &pos);

  /* No selection has been chosen yet.  */
  menu_item_selection = 0;

  /* Display the menu.  */
  menu_item_selection = SendMessage (FRAME_W32_WINDOW (f),
				     WM_EMACS_TRACKPOPUPMENU,
				     (WPARAM)menu, (LPARAM)&pos);

  /* Clean up extraneous mouse events which might have been generated
     during the call. */
  discard_mouse_events ();

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
	      entry	= AREF (menu_items, i + MENU_ITEMS_ITEM_VALUE);
	      if (menu_item_selection == i)
		{
		  if (keymaps != 0)
		    {
		      int j;

		      entry = Fcons (entry, Qnil);
		      if (!NILP (prefix))
			entry = Fcons (prefix, entry);
		      for (j = submenu_depth - 1; j >= 0; j--)
			if (!NILP (subprefix_stack[j]))
			  entry = Fcons (subprefix_stack[j], entry);
		    }
		  return entry;
		}
	      i += MENU_ITEMS_ITEM_LENGTH;
	    }
	}
    }
  else if (!for_click)
    /* Make "Cancel" equivalent to C-g.  */
    Fsignal (Qquit, Qnil);

  return Qnil;
}


#ifdef HAVE_DIALOGS
static char * button_names [] = {
  "button1", "button2", "button3", "button4", "button5",
  "button6", "button7", "button8", "button9", "button10" };

static Lisp_Object
w32_dialog_show (f, keymaps, title, header, error)
     FRAME_PTR f;
     int keymaps;
     Lisp_Object title, header;
     char **error;
{
  int i, nb_buttons=0;
  char dialog_name[6];
  int menu_item_selection;

  widget_value *wv, *first_wv = 0, *prev_wv = 0;

  /* Number of elements seen so far, before boundary.  */
  int left_count = 0;
  /* 1 means we've seen the boundary between left-hand elts and right-hand.  */
  int boundary_seen = 0;

  *error = NULL;

  if (menu_items_n_panes > 1)
    {
      *error = "Multiple panes in dialog box";
      return Qnil;
    }

  /* Create a tree of widget_value objects
     representing the text label and buttons.  */
  {
    Lisp_Object pane_name, prefix;
    char *pane_string;
    pane_name = AREF (menu_items, MENU_ITEMS_PANE_NAME);
    prefix = AREF (menu_items, MENU_ITEMS_PANE_PREFIX);
    pane_string = (NILP (pane_name)
		   ? "" : (char *) SDATA (pane_name));
    prev_wv = xmalloc_widget_value ();
    prev_wv->value = pane_string;
    if (keymaps && !NILP (prefix))
      prev_wv->name++;
    prev_wv->enabled = 1;
    prev_wv->name = "message";
    prev_wv->help = Qnil;
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
	    boundary_seen = 1;
	    i++;
	    continue;
	  }
	if (nb_buttons >= 9)
	  {
	    free_menubar_widget_value_tree (first_wv);
	    *error = "Too many dialog items";
	    return Qnil;
	  }

	wv = xmalloc_widget_value ();
	prev_wv->next = wv;
	wv->name = (char *) button_names[nb_buttons];
	if (!NILP (descrip))
	  wv->key = (char *) SDATA (descrip);
	wv->value = (char *) SDATA (item_name);
	wv->call_data = (void *) &AREF (menu_items, i);
	wv->enabled = !NILP (enable);
	wv->help = Qnil;
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

    wv = xmalloc_widget_value ();
    wv->name = dialog_name;
    wv->help = Qnil;

    /*  Frame title: 'Q' = Question, 'I' = Information.
        Can also have 'E' = Error if, one day, we want
        a popup for errors. */
    if (NILP(header))
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
			   f->output_data.w32->widget, 1, 0,
			   dialog_selection_callback, 0);
  lw_modify_all_widgets (dialog_id, first_wv->contents, TRUE);

  /* Free the widget_value objects we used to specify the contents.  */
  free_menubar_widget_value_tree (first_wv);

  /* No selection has been chosen yet.  */
  menu_item_selection = 0;

  /* Display the menu.  */
  lw_pop_up_all_widgets (dialog_id);

  /* Process events that apply to the menu.  */
  popup_get_selection ((XEvent *) 0, FRAME_X_DISPLAY_INFO (f), dialog_id);

  lw_destroy_all_widgets (dialog_id);

  /* Find the selected item, and its pane, to return
     the proper value.  */
  if (menu_item_selection != 0)
    {
      Lisp_Object prefix;

      prefix = Qnil;
      i = 0;
      while (i < menu_items_used)
	{
	  Lisp_Object entry;

	  if (EQ (AREF (menu_items, i), Qt))
	    {
	      prefix = AREF (menu_items, i + MENU_ITEMS_PANE_PREFIX);
	      i += MENU_ITEMS_PANE_LENGTH;
	    }
	  else
	    {
	      entry	= AREF (menu_items, i + MENU_ITEMS_ITEM_VALUE);
	      if (menu_item_selection == i)
		{
		  if (keymaps != 0)
		    {
		      entry = Fcons (entry, Qnil);
		      if (!NILP (prefix))
			entry = Fcons (prefix, entry);
		    }
		  return entry;
		}
	      i += MENU_ITEMS_ITEM_LENGTH;
	    }
	}
    }
  else
    /* Make "Cancel" equivalent to C-g.  */
    Fsignal (Qquit, Qnil);

  return Qnil;
}
#endif  /* HAVE_DIALOGS  */


/* Is this item a separator? */
static int
name_is_separator (name)
     char *name;
{
  char *start = name;

  /* Check if name string consists of only dashes ('-').  */
  while (*name == '-') name++;
  /* Separators can also be of the form "--:TripleSuperMegaEtched"
     or "--deep-shadow".  We don't implement them yet, se we just treat
     them like normal separators.  */
  return (*name == '\0' || start + 2 == name);
}


/* Indicate boundary between left and right.  */
static int
add_left_right_boundary (HMENU menu)
{
  return AppendMenu (menu, MF_MENUBARBREAK, 0, NULL);
}

/* UTF8: 0xxxxxxx, 110xxxxx 10xxxxxx, 1110xxxx, 10xxxxxx, 10xxxxxx */
static void
utf8to16 (unsigned char * src, int len, WCHAR * dest)
{
  while (len > 0)
    {
      int utf16;
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
  char *out_string;
  int return_value;

  if (name_is_separator (wv->name))
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
	  out_string = alloca (strlen (wv->name) + strlen (wv->key) + 2);
	  strcpy (out_string, wv->name);
	  strcat (out_string, "\t");
	  strcat (out_string, wv->key);
	}
      else
	out_string = wv->name;

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
	      DebPrint ("Menu: allocing %ld for owner-draw", out_string);
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
	utf16_string = alloca ((utf8_len + 1) * sizeof (WCHAR));

      utf8to16 (out_string, utf8_len, utf16_string);
      return_value = unicode_append_menu (menu, fuFlags,
					  item != NULL ? (UINT) item
					    : (UINT) wv->call_data,
					  utf16_string);
      if (!return_value)
	{
	  /* On W9x/ME, unicode menus are not supported, though AppendMenuW
	     apparently does exist at least in some cases and appears to be
	     stubbed out to do nothing.  out_string is UTF-8, but since
	     our standard menus are in English and this is only going to
	     happen the first time a menu is used, the encoding is
	     of minor importance compared with menus not working at all.  */
	  return_value =
	    AppendMenu (menu, fuFlags,
			item != NULL ? (UINT) item: (UINT) wv->call_data,
			out_string);
	  /* Don't use unicode menus in future.  */
	  unicode_append_menu = NULL;
	}

      if (unicode_append_menu && (fuFlags & MF_OWNERDRAW))
	local_free (out_string);
    }
  else
    {
      return_value =
	AppendMenu (menu,
		    fuFlags,
		    item != NULL ? (UINT) item : (UINT) wv->call_data,
		    out_string );
    }

  /* This must be done after the menu item is created.  */
  if (!wv->title && wv->call_data != 0)
    {
      if (set_menu_item_info)
	{
	  MENUITEMINFO info;
	  bzero (&info, sizeof (info));
	  info.cbSize = sizeof (info);
	  info.fMask = MIIM_DATA;

	  /* Set help string for menu item.  Leave it as a Lisp_Object
	     until it is ready to be displayed, since GC can happen while
	     menus are active.  */
	  if (!NILP (wv->help))
#ifdef USE_LISP_UNION_TYPE
	    info.dwItemData = (DWORD) (wv->help).i;
#else
	    info.dwItemData = (DWORD) (wv->help);
#endif
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
			      item != NULL ? (UINT) item : (UINT) wv->call_data,
			      FALSE, &info);
	}
    }
  return return_value;
}

/* Construct native Windows menu(bar) based on widget_value tree.  */
int
fill_in_menu (HMENU menu, widget_value *wv)
{
  int items_added = 0;

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
void
w32_menu_display_help (HWND owner, HMENU menu, UINT item, UINT flags)
{
  if (get_menu_item_info)
    {
      struct frame *f = x_window_to_frame (&one_w32_display_info, owner);
      Lisp_Object frame, help;

      /* No help echo on owner-draw menu items, or when the keyboard is used
	 to navigate the menus, since tooltips are distracting if they pop
	 up elsewhere.  */
      if (flags & MF_OWNERDRAW || flags & MF_POPUP
	  || !(flags & MF_MOUSESELECT))
	help = Qnil;
      else
	{
	  MENUITEMINFO info;

	  bzero (&info, sizeof (info));
	  info.cbSize = sizeof (info);
	  info.fMask = MIIM_DATA;
	  get_menu_item_info (menu, item, FALSE, &info);

#ifdef USE_LISP_UNION_TYPE
	  help = info.dwItemData ? (Lisp_Object) ((EMACS_INT) info.dwItemData)
	                         : Qnil;
#else
	  help = info.dwItemData ? (Lisp_Object) info.dwItemData : Qnil;
#endif
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
	show_help_echo (help, Qnil, Qnil, Qnil, 1);
    }
}

/* Free memory used by owner-drawn strings.  */
static void
w32_free_submenu_strings (menu)
     HMENU menu;
{
  int i, num = GetMenuItemCount (menu);
  for (i = 0; i < num; i++)
    {
      MENUITEMINFO info;
      bzero (&info, sizeof (info));
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
w32_free_menu_strings (hwnd)
     HWND hwnd;
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

#endif /* HAVE_MENUS */

/* The following is used by delayed window autoselection.  */

DEFUN ("menu-or-popup-active-p", Fmenu_or_popup_active_p, Smenu_or_popup_active_p, 0, 0, 0,
       doc: /* Return t if a menu or popup dialog is active on selected frame.  */)
     ()
{
#ifdef HAVE_MENUS
  FRAME_PTR f;
  f = SELECTED_FRAME ();
  return (f->output_data.w32->menubar_active > 0) ? Qt : Qnil;
#else
  return Qnil;
#endif /* HAVE_MENUS */
}

void syms_of_w32menu ()
{
	globals_of_w32menu ();
  staticpro (&menu_items);
  menu_items = Qnil;

  current_popup_menu = NULL;

  Qdebug_on_next_call = intern ("debug-on-next-call");
  staticpro (&Qdebug_on_next_call);

  defsubr (&Sx_popup_menu);
  defsubr (&Smenu_or_popup_active_p);
#ifdef HAVE_MENUS
  defsubr (&Sx_popup_dialog);
#endif
}

/*
	globals_of_w32menu is used to initialize those global variables that
	must always be initialized on startup even when the global variable
	initialized is non zero (see the function main in emacs.c).
	globals_of_w32menu is called from syms_of_w32menu when the global
	variable initialized is 0 and directly from main when initialized
	is non zero.
 */
void globals_of_w32menu ()
{
	/* See if Get/SetMenuItemInfo functions are available.  */
  HMODULE user32 = GetModuleHandle ("user32.dll");
  get_menu_item_info = (GetMenuItemInfoA_Proc) GetProcAddress (user32, "GetMenuItemInfoA");
  set_menu_item_info = (SetMenuItemInfoA_Proc) GetProcAddress (user32, "SetMenuItemInfoA");
  unicode_append_menu = (AppendMenuW_Proc) GetProcAddress (user32, "AppendMenuW");
}

/* arch-tag: 0eaed431-bb4e-4aac-a527-95a1b4f1fed0
   (do not change this comment) */
