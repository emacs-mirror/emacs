/* X Communication module for terminals which understand the X protocol.
   Copyright (C) 1986, 1988, 1993, 1994 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* X pop-up deck-of-cards menu facility for gnuemacs.
 *
 * Written by Jon Arnold and Roman Budzianowski
 * Mods and rewrite by Robert Krawitz
 *
 */

/* Modified by Fred Pierresteguy on December 93
   to make the popup menus and menubar use the Xt.  */

/* Rewritten for clarity and GC protection by rms in Feb 94.  */

/* On 4.3 this loses if it comes after xterm.h.  */
#include <signal.h>
#include <config.h>

#include <stdio.h>
#include "lisp.h"
#include "termhooks.h"
#include "frame.h"
#include "window.h"
#include "keyboard.h"
#include "blockinput.h"
#include "puresize.h"

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

#include "dispextern.h"

#ifdef HAVE_X_WINDOWS
#ifdef USE_X_TOOLKIT
#include <X11/Xlib.h>
#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Paned.h>
#include "../lwlib/lwlib.h"
#else /* not USE_X_TOOLKIT */
#include "../oldXMenu/XMenu.h"
#endif /* not USE_X_TOOLKIT */
#endif /* HAVE_X_WINDOWS */

#define min(x,y) (((x) < (y)) ? (x) : (y))
#define max(x,y) (((x) > (y)) ? (x) : (y))

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif /* no TRUE */

extern Lisp_Object Qmenu_enable;
extern Lisp_Object Qmenu_bar;
extern Lisp_Object Qmouse_click, Qevent_kind;

#ifdef USE_X_TOOLKIT
extern void process_expose_from_menu ();
extern XtAppContext Xt_app_con;

static Lisp_Object xdialog_show ();
void popup_get_selection ();
#endif

static Lisp_Object xmenu_show ();
static void keymap_panes ();
static void single_keymap_panes ();
static void list_of_panes ();
static void list_of_items ();

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

#define MENU_ITEMS_ITEM_NAME 0
#define MENU_ITEMS_ITEM_ENABLE 1
#define MENU_ITEMS_ITEM_VALUE 2
#define MENU_ITEMS_ITEM_EQUIV_KEY 3
#define MENU_ITEMS_ITEM_DEFINITION 4
#define MENU_ITEMS_ITEM_LENGTH 5

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

/* Flag which when set indicates a dialog or menu has been posted by
   Xt on behalf of one of the widget sets.  */
static int popup_activated_flag;

/* This holds a Lisp vector
   which contains frames that have menu bars.
   Each frame that has a menu bar is found at some index in this vector
   and the menu bar widget refers to the frame through that index.  */
static Lisp_Object frame_vector;

/* Return the index of FRAME in frame_vector.
   If FRAME isn't in frame_vector yet, put it in,
   lengthening the vector if necessary.  */

static int
frame_vector_add_frame (f)
     FRAME_PTR *f;
{
  int length = XVECTOR (frame_vector)->size;
  int i, empty = -1;
  Lisp_Object new, frame;

  XSETFRAME (frame, f);

  for (i = 0; i < length; i++)
    {
      if (EQ (frame, XVECTOR (frame_vector)->contents[i]))
	return i;
      if (NILP (XVECTOR (frame_vector)->contents[i]))
	empty = i;
    }

  if (empty >= 0)
    {
      XVECTOR (frame_vector)->contents[empty] = frame;
      return empty;
    }

  new = Fmake_vector (make_number (length * 2), Qnil);
  bcopy (XVECTOR (frame_vector)->contents,
	 XVECTOR (new)->contents, sizeof (Lisp_Object) * length);
  
  XVECTOR (frame_vector)->contents[length] = frame;
  return length;
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

  XVECTOR (menu_items)->contents[menu_items_used++] = Qnil;
  menu_items_submenu_depth++;
}

/* End a submenu.  */

static void
push_submenu_end ()
{
  if (menu_items_used + 1 > menu_items_allocated)
    grow_menu_items ();

  XVECTOR (menu_items)->contents[menu_items_used++] = Qlambda;
  menu_items_submenu_depth--;
}

/* Indicate boundary between left and right.  */

static void
push_left_right_boundary ()
{
  if (menu_items_used + 1 > menu_items_allocated)
    grow_menu_items ();

  XVECTOR (menu_items)->contents[menu_items_used++] = Qquote;
}

/* Start a new menu pane in menu_items..
   NAME is the pane name.  PREFIX_VEC is a prefix key for this pane.  */

static void
push_menu_pane (name, prefix_vec)
     Lisp_Object name, prefix_vec;
{
  if (menu_items_used + MENU_ITEMS_PANE_LENGTH > menu_items_allocated)
    grow_menu_items ();

  if (menu_items_submenu_depth == 0)
    menu_items_n_panes++;
  XVECTOR (menu_items)->contents[menu_items_used++] = Qt;
  XVECTOR (menu_items)->contents[menu_items_used++] = name;
  XVECTOR (menu_items)->contents[menu_items_used++] = prefix_vec;
}

/* Push one menu item into the current pane.
   NAME is the string to display.  ENABLE if non-nil means
   this item can be selected.  KEY is the key generated by
   choosing this item, or nil if this item doesn't really have a definition.
   DEF is the definition of this item.
   EQUIV is the textual description of the keyboard equivalent for
   this item (or nil if none).  */

static void
push_menu_item (name, enable, key, def, equiv)
     Lisp_Object name, enable, key, def, equiv;
{
  if (menu_items_used + MENU_ITEMS_ITEM_LENGTH > menu_items_allocated)
    grow_menu_items ();

  XVECTOR (menu_items)->contents[menu_items_used++] = name;
  XVECTOR (menu_items)->contents[menu_items_used++] = enable;
  XVECTOR (menu_items)->contents[menu_items_used++] = key;
  XVECTOR (menu_items)->contents[menu_items_used++] = equiv;
  XVECTOR (menu_items)->contents[menu_items_used++] = def;
}

/* Figure out the current keyboard equivalent of a menu item ITEM1.
   The item string for menu display should be ITEM_STRING.
   Store the equivalent keyboard key sequence's
   textual description into *DESCRIP_PTR.
   Also cache them in the item itself.
   Return the real definition to execute.  */

static Lisp_Object
menu_item_equiv_key (item_string, item1, descrip_ptr)
     Lisp_Object item_string;
     Lisp_Object item1;
     Lisp_Object *descrip_ptr;
{
  /* This is the real definition--the function to run.  */
  Lisp_Object def;
  /* This is the sublist that records cached equiv key data
     so we can save time.  */
  Lisp_Object cachelist;
  /* These are the saved equivalent keyboard key sequence
     and its key-description.  */
  Lisp_Object savedkey, descrip;
  Lisp_Object def1;
  int changed = 0;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  /* If a help string follows the item string, skip it.  */
  if (CONSP (XCONS (item1)->cdr)
      && STRINGP (XCONS (XCONS (item1)->cdr)->car))
    item1 = XCONS (item1)->cdr;

  def = Fcdr (item1);

  /* Get out the saved equivalent-keyboard-key info.  */
  cachelist = savedkey = descrip = Qnil;
  if (CONSP (def) && CONSP (XCONS (def)->car)
      && (NILP (XCONS (XCONS (def)->car)->car)
	  || VECTORP (XCONS (XCONS (def)->car)->car)))
    {
      cachelist = XCONS (def)->car;
      def = XCONS (def)->cdr;
      savedkey = XCONS (cachelist)->car;
      descrip = XCONS (cachelist)->cdr;
    }

  GCPRO4 (def, def1, savedkey, descrip);

  /* Is it still valid?  */
  def1 = Qnil;
  if (!NILP (savedkey))
    def1 = Fkey_binding (savedkey, Qnil);
  /* If not, update it.  */
  if (! EQ (def1, def)
      /* If the command is an alias for another
         (such as easymenu.el and lmenu.el set it up),
         check if the original command matches the cached command.  */
      && !(SYMBOLP (def) && SYMBOLP (XSYMBOL (def)->function)
           && EQ (def1, XSYMBOL (def)->function))
      /* If something had no key binding before, don't recheck it--
	 doing that takes too much time and makes menus too slow.  */
      && !(!NILP (cachelist) && NILP (savedkey)))
    {
      changed = 1;
      descrip = Qnil;
      /* If the command is an alias for another
	 (such as easymenu.el and lmenu.el set it up),
	 see if the original command name has equivalent keys.  */
      if (SYMBOLP (def) && SYMBOLP (XSYMBOL (def)->function))
	savedkey = Fwhere_is_internal (XSYMBOL (def)->function,
				       Qnil, Qt, Qnil);
      else
	/* Otherwise look up the specified command itself.
	   We don't try both, because that makes easymenu menus slow.  */
	savedkey = Fwhere_is_internal (def, Qnil, Qt, Qnil);

      if (!NILP (savedkey))
	{
	  descrip = Fkey_description (savedkey);
	  descrip = concat2 (make_string ("  (", 3), descrip);
	  descrip = concat2 (descrip, make_string (")", 1));
	}
    }

  /* Cache the data we just got in a sublist of the menu binding.  */
  if (NILP (cachelist))
    {
      CHECK_IMPURE (item1);
      XCONS (item1)->cdr = Fcons (Fcons (savedkey, descrip), def);
    }
  else if (changed)
    {
      XCONS (cachelist)->car = savedkey;
      XCONS (cachelist)->cdr = descrip;
    }

  UNGCPRO;
  *descrip_ptr = descrip;
  return def;
}

/* This is used as the handler when calling internal_condition_case_1.  */

static Lisp_Object
menu_item_enabled_p_1 (arg)
     Lisp_Object arg;
{
  return Qnil;
}

/* Return non-nil if the command DEF is enabled when used as a menu item.
   This is based on looking for a menu-enable property.
   If NOTREAL is set, don't bother really computing this.  */

static Lisp_Object
menu_item_enabled_p (def, notreal)
     Lisp_Object def;
     int notreal;
{
  Lisp_Object enabled, tem;

  enabled = Qt;
  if (notreal)
    return enabled;
  if (SYMBOLP (def))
    {
      /* No property, or nil, means enable.
	 Otherwise, enable if value is not nil.  */
      tem = Fget (def, Qmenu_enable);
      if (!NILP (tem))
	/* (condition-case nil (eval tem)
	   (error nil))  */
	enabled = internal_condition_case_1 (Feval, tem, Qerror,
					     menu_item_enabled_p_1);
    }
  return enabled;
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
    single_keymap_panes (keymaps[mapno], Qnil, Qnil, notreal);

  finish_menu_items ();
}

/* This is a recursive subroutine of keymap_panes.
   It handles one keymap, KEYMAP.
   The other arguments are passed along
   or point to local variables of the previous function.
   If NOTREAL is nonzero,
   don't bother really computing whether an item is enabled.  */

static void
single_keymap_panes (keymap, pane_name, prefix, notreal)
     Lisp_Object keymap;
     Lisp_Object pane_name;
     Lisp_Object prefix;
     int notreal;
{
  Lisp_Object pending_maps;
  Lisp_Object tail, item, item1, item_string, table;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  pending_maps = Qnil;

  push_menu_pane (pane_name, prefix);

  for (tail = keymap; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      /* Look at each key binding, and if it has a menu string,
	 make a menu item from it.  */
      item = XCONS (tail)->car;
      if (CONSP (item))
	{
	  item1 = XCONS (item)->cdr;
	  if (CONSP (item1))
	    {
	      item_string = XCONS (item1)->car;
	      if (STRINGP (item_string))
		{
		  /* This is the real definition--the function to run.  */
		  Lisp_Object def;
		  /* These are the saved equivalent keyboard key sequence
		     and its key-description.  */
		  Lisp_Object descrip;
		  Lisp_Object tem, enabled;

		  /* GCPRO because ...enabled_p will call eval
		     and ..._equiv_key may autoload something.
		     Protecting KEYMAP preserves everything we use;
		     aside from that, must protect whatever might be
		     a string.  Since there's no GCPRO5, we refetch
		     item_string instead of protecting it.  */
		  descrip = def = Qnil;
		  GCPRO4 (keymap, pending_maps, def, descrip);

		  def = menu_item_equiv_key (item_string, item1, &descrip);
		  enabled = menu_item_enabled_p (def, notreal);

		  UNGCPRO;

		  item_string = XCONS (item1)->car;

		  tem = Fkeymapp (def);
		  if (XSTRING (item_string)->data[0] == '@' && !NILP (tem))
		    pending_maps = Fcons (Fcons (def, Fcons (item_string, XCONS (item)->car)),
					  pending_maps);
		  else
		    {
		      Lisp_Object submap;
		      GCPRO4 (keymap, pending_maps, descrip, item_string);
		      submap = get_keymap_1 (def, 0, 1);
		      UNGCPRO;
#ifndef USE_X_TOOLKIT
		      /* Indicate visually that this is a submenu.  */
		      if (!NILP (submap))
			item_string = concat2 (item_string,
					       build_string (" >"));
#endif
		      /* If definition is nil, pass nil as the key.  */
		      push_menu_item (item_string, enabled,
				      XCONS (item)->car, def,
				      descrip);
#ifdef USE_X_TOOLKIT
		      /* Display a submenu using the toolkit.  */
		      if (! NILP (submap))
			{
			  push_submenu_start ();
			  single_keymap_panes (submap, Qnil,
					       XCONS (item)->car, notreal);
			  push_submenu_end ();
			}
#endif
		    }
		}
	    }
	}
      else if (VECTORP (item))
	{
	  /* Loop over the char values represented in the vector.  */
	  int len = XVECTOR (item)->size;
	  int c;
	  for (c = 0; c < len; c++)
	    {
	      Lisp_Object character;
	      XSETFASTINT (character, c);
	      item1 = XVECTOR (item)->contents[c];
	      if (CONSP (item1))
		{
		  item_string = XCONS (item1)->car;
		  if (STRINGP (item_string))
		    {
		      Lisp_Object def;

		      /* These are the saved equivalent keyboard key sequence
			 and its key-description.  */
		      Lisp_Object descrip;
		      Lisp_Object tem, enabled;

		      /* GCPRO because ...enabled_p will call eval
			 and ..._equiv_key may autoload something.
			 Protecting KEYMAP preserves everything we use;
			 aside from that, must protect whatever might be
			 a string.  Since there's no GCPRO5, we refetch
			 item_string instead of protecting it.  */
		      GCPRO4 (keymap, pending_maps, def, descrip);
		      descrip = def = Qnil;

		      def = menu_item_equiv_key (item_string, item1, &descrip);
		      enabled = menu_item_enabled_p (def, notreal);

		      UNGCPRO;

		      item_string = XCONS (item1)->car;

		      tem = Fkeymapp (def);
		      if (XSTRING (item_string)->data[0] == '@' && !NILP (tem))
			pending_maps = Fcons (Fcons (def, Fcons (item_string, character)),
					      pending_maps);
		      else
			{
			  Lisp_Object submap;
			  GCPRO4 (keymap, pending_maps, descrip, item_string);
			  submap = get_keymap_1 (def, 0, 1);
			  UNGCPRO;
#ifndef USE_X_TOOLKIT
			  if (!NILP (submap))
			    item_string = concat2 (item_string,
						   build_string (" >"));
#endif
			  /* If definition is nil, pass nil as the key.  */
			  push_menu_item (item_string, enabled, character,
					  def, descrip);
#ifdef USE_X_TOOLKIT
			  if (! NILP (submap))
			    {
			      push_submenu_start ();
			      single_keymap_panes (submap, Qnil,
						   character, notreal);
			      push_submenu_end ();
			    }
#endif
			}
		    }
		}
	    }
	}
    }

  /* Process now any submenus which want to be panes at this level.  */
  while (!NILP (pending_maps))
    {
      Lisp_Object elt, eltcdr, string;
      elt = Fcar (pending_maps);
      eltcdr = XCONS (elt)->cdr;
      string = XCONS (eltcdr)->car;
      /* We no longer discard the @ from the beginning of the string here.
	 Instead, we do this in xmenu_show.  */
      single_keymap_panes (Fcar (elt), string,
			   XCONS (eltcdr)->cdr, notreal);
      pending_maps = Fcdr (pending_maps);
    }
}

/* Push all the panes and items of a menu decsribed by the
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
      CHECK_STRING (pane_name, 0);
      push_menu_pane (pane_name, Qnil);
      pane_data = Fcdr (elt);
      CHECK_CONS (pane_data, 0);
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
	push_menu_item (item, Qnil, Qnil, Qt, Qnil);
      else if (NILP (item))
	push_left_right_boundary ();
      else
	{
	  CHECK_CONS (item, 0);
	  item1 = Fcar (item);
	  CHECK_STRING (item1, 1);
	  push_menu_item (item1, Qt, Fcdr (item), Qt, Qnil);
	}
    }
}

DEFUN ("x-popup-menu", Fx_popup_menu, Sx_popup_menu, 2, 2, 0,
  "Pop up a deck-of-cards menu and return user's selection.\n\
POSITION is a position specification.  This is either a mouse button event\n\
or a list ((XOFFSET YOFFSET) WINDOW)\n\
where XOFFSET and YOFFSET are positions in pixels from the top left\n\
corner of WINDOW's frame.  (WINDOW may be a frame object instead of a window.)\n\
This controls the position of the center of the first line\n\
in the first pane of the menu, not the top left of the menu as a whole.\n\
If POSITION is t, it means to use the current mouse position.\n\
\n\
MENU is a specifier for a menu.  For the simplest case, MENU is a keymap.\n\
The menu items come from key bindings that have a menu string as well as\n\
a definition; actually, the \"definition\" in such a key binding looks like\n\
\(STRING . REAL-DEFINITION).  To give the menu a title, put a string into\n\
the keymap as a top-level element.\n\n\
You can also use a list of keymaps as MENU.\n\
  Then each keymap makes a separate pane.\n\
When MENU is a keymap or a list of keymaps, the return value\n\
is a list of events.\n\n\
Alternatively, you can specify a menu of multiple panes\n\
  with a list of the form (TITLE PANE1 PANE2...),\n\
where each pane is a list of form (TITLE ITEM1 ITEM2...).\n\
Each ITEM is normally a cons cell (STRING . VALUE);\n\
but a string can appear as an item--that makes a nonselectable line\n\
in the menu.\n\
With this form of menu, the return value is VALUE from the chosen item.\n\
\n\
If POSITION is nil, don't display the menu at all, just precalculate the\n\
cached information about equivalent key sequences.")
  (position, menu)
     Lisp_Object position, menu;
{
  int number_of_panes, panes;
  Lisp_Object keymap, tem;
  int xpos, ypos;
  Lisp_Object title;
  char *error_name;
  Lisp_Object selection;
  int i, j;
  FRAME_PTR f;
  Lisp_Object x, y, window;
  int keymaps = 0;
  int for_click = 0;
  struct gcpro gcpro1;

  if (! NILP (position))
    {
      check_x ();

      /* Decode the first argument: find the window and the coordinates.  */
      if (EQ (position, Qt)
	  || (CONSP (position) && EQ (XCONS (position)->car, Qmenu_bar)))
	{
	  /* Use the mouse's current position.  */
	  FRAME_PTR new_f = selected_frame;
	  Lisp_Object bar_window;
	  int part;
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

      CHECK_NUMBER (x, 0);
      CHECK_NUMBER (y, 0);

      /* Decode where to put the menu.  */

      if (FRAMEP (window))
	{
	  f = XFRAME (window);
	  xpos = 0;
	  ypos = 0;
	}
      else if (WINDOWP (window))
	{
	  CHECK_LIVE_WINDOW (window, 0);
	  f = XFRAME (WINDOW_FRAME (XWINDOW (window)));

	  xpos = (FONT_WIDTH (f->display.x->font) * XWINDOW (window)->left);
	  ypos = (f->display.x->line_height * XWINDOW (window)->top);
	}
      else
	/* ??? Not really clean; should be CHECK_WINDOW_OR_FRAME,
	   but I don't want to make one now.  */
	CHECK_WINDOW (window, 0);

      xpos += XINT (x);
      ypos += XINT (y);
    }

  title = Qnil;
  GCPRO1 (title);

  /* Decode the menu items from what was specified.  */

  keymap = Fkeymapp (menu);
  tem = Qnil;
  if (CONSP (menu))
    tem = Fkeymapp (Fcar (menu));
  if (!NILP (keymap))
    {
      /* We were given a keymap.  Extract menu info from the keymap.  */
      Lisp_Object prompt;
      keymap = get_keymap (menu);

      /* Extract the detailed info to make one pane.  */
      keymap_panes (&menu, 1, NILP (position));

      /* Search for a string appearing directly as an element of the keymap.
	 That string is the title of the menu.  */
      prompt = map_prompt (keymap);

      /* Make that be the pane title of the first pane.  */
      if (!NILP (prompt) && menu_items_n_panes >= 0)
	XVECTOR (menu_items)->contents[MENU_ITEMS_PANE_NAME] = prompt;

      keymaps = 1;
    }
  else if (!NILP (tem))
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

	  maps[i++] = keymap = get_keymap (Fcar (tem));

	  prompt = map_prompt (keymap);
	  if (NILP (title) && !NILP (prompt))
	    title = prompt;
	}

      /* Extract the detailed info to make one pane.  */
      keymap_panes (maps, nmaps, NILP (position));

      /* Make the title be the pane title of the first pane.  */
      if (!NILP (title) && menu_items_n_panes >= 0)
	XVECTOR (menu_items)->contents[MENU_ITEMS_PANE_NAME] = title;

      keymaps = 1;
    }
  else
    {
      /* We were given an old-fashioned menu.  */
      title = Fcar (menu);
      CHECK_STRING (title, 1);

      list_of_panes (Fcdr (menu));

      keymaps = 0;
    }
  
  if (NILP (position))
    {
      discard_menu_items ();
      UNGCPRO;
      return Qnil;
    }

  /* Display them in a menu.  */
  BLOCK_INPUT;

  selection = xmenu_show (f, xpos, ypos, for_click,
			  keymaps, title, &error_name);
  UNBLOCK_INPUT;

  discard_menu_items ();

  UNGCPRO;

  if (error_name) error (error_name);
  return selection;
}

DEFUN ("x-popup-dialog", Fx_popup_dialog, Sx_popup_dialog, 2, 2, 0,
  "Pop up a dialog box and return user's selection.\n\
POSITION specifies which frame to use.\n\
This is normally a mouse button event or a window or frame.\n\
If POSITION is t, it means to use the frame the mouse is on.\n\
The dialog box appears in the middle of the specified frame.\n\
\n\
CONTENTS specifies the alternatives to display in the dialog box.\n\
It is a list of the form (TITLE ITEM1 ITEM2...).\n\
Each ITEM is a cons cell (STRING . VALUE).\n\
The return value is VALUE from the chosen item.\n\n\
An ITEM may also be just a string--that makes a nonselectable item.\n\
An ITEM may also be nil--that means to put all preceding items\n\
on the left of the dialog box and all following items on the right.\n\
\(By default, approximately half appear on each side.)")
  (position, contents)
     Lisp_Object position, contents;
{
  FRAME_PTR f;
  Lisp_Object window;

  check_x ();

  /* Decode the first argument: find the window or frame to use.  */
  if (EQ (position, Qt)
      || (CONSP (position) && EQ (XCONS (position)->car, Qmenu_bar)))
    {
#if 0 /* Using the frame the mouse is on may not be right.  */
      /* Use the mouse's current position.  */
      FRAME_PTR new_f = selected_frame;
      Lisp_Object bar_window;
      int part;
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

  /* Decode where to put the menu.  */

  if (FRAMEP (window))
    f = XFRAME (window);
  else if (WINDOWP (window))
    {
      CHECK_LIVE_WINDOW (window, 0);
      f = XFRAME (WINDOW_FRAME (XWINDOW (window)));
    }
  else
    /* ??? Not really clean; should be CHECK_WINDOW_OR_FRAME,
       but I don't want to make one now.  */
    CHECK_WINDOW (window, 0);

#ifndef USE_X_TOOLKIT
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
#else
  {
    Lisp_Object title;
    char *error_name;
    Lisp_Object selection;

    /* Decode the dialog items from what was specified.  */
    title = Fcar (contents);
    CHECK_STRING (title, 1);

    list_of_panes (Fcons (contents, Qnil));

    /* Display them in a dialog box.  */
    BLOCK_INPUT;
    selection = xdialog_show (f, 0, title, &error_name);
    UNBLOCK_INPUT;

    discard_menu_items ();

    if (error_name) error (error_name);
    return selection;
  }
#endif
}

#ifdef USE_X_TOOLKIT

/* Loop in Xt until the menu pulldown or dialog popup has been
   popped down (deactivated).

   NOTE: All calls to popup_get_selection() should be protected
   with BLOCK_INPUT, UNBLOCK_INPUT wrappers.  */

void
popup_get_selection (initial_event, dpyinfo)
     XEvent *initial_event;
     struct x_display_info *dpyinfo;
{
  XEvent event;

  /* Define a queue to save up for later unreading
     all X events that don't pertain to the menu.  */
  struct event_queue
    {
      XEvent event;
      struct event_queue *next;
    };
  
  struct event_queue *queue = NULL;
  struct event_queue *queue_tmp;

  if (initial_event)
    event = *initial_event;
  else
    XtAppNextEvent (Xt_app_con, &event);

  while (1)
    {
      /* Handle expose events for editor frames right away.  */
      if (event.type == Expose)
	process_expose_from_menu (event);
      /* Make sure we don't consider buttons grabbed after menu goes.  */
      else if (event.type == ButtonRelease
	       && dpyinfo->display == event.xbutton.display)
	dpyinfo->grabbed &= ~(1 << event.xbutton.button);

      /* Queue all events not for this popup,
	 except for Expose, which we've already handled.
	 Note that the X window is associated with the frame if this
	 is a menu bar popup, but not if it's a dialog box.  So we use
	 x_non_menubar_window_to_frame, not x_any_window_to_frame.  */
      if (event.type != Expose
	  && (event.xany.display != dpyinfo->display
	      || x_non_menubar_window_to_frame (dpyinfo, event.xany.window)))
	{
	  queue_tmp = (struct event_queue *) malloc (sizeof (struct event_queue));

	  if (queue_tmp != NULL) 
	    {
	      queue_tmp->event = event;
	      queue_tmp->next = queue;
	      queue = queue_tmp;
	    }
	}
      else
	XtDispatchEvent (&event);

      if (!popup_activated ())
	break;
      XtAppNextEvent (Xt_app_con, &event);
    }

  /* Unread any events that we got but did not handle.  */
  while (queue != NULL) 
    {
      queue_tmp = queue;
      XPutBackEvent (queue_tmp->event.xany.display, &queue_tmp->event);
      queue = queue_tmp->next;
      free ((char *)queue_tmp);
      /* Cause these events to get read as soon as we UNBLOCK_INPUT.  */
      interrupt_input_pending = 1;
    }
}

/* Detect if a dialog or menu has been posted.  */

int
popup_activated ()
{
  return popup_activated_flag;
}


/* This callback is invoked when the user selects a menubar cascade
   pushbutton, but before the pulldown menu is posted.  */

static void
popup_activate_callback (widget, id, client_data)
     Widget widget;
     LWLIB_ID id;
     XtPointer client_data;
{
  popup_activated_flag = 1;
}

/* This callback is called from the menu bar pulldown menu
   when the user makes a selection.
   Figure out what the user chose
   and put the appropriate events into the keyboard buffer.  */

static void
menubar_selection_callback (widget, id, client_data)
     Widget widget;
     LWLIB_ID id;
     XtPointer client_data;
{
  Lisp_Object prefix, entry;
  FRAME_PTR f = XFRAME (XVECTOR (frame_vector)->contents[id]);
  Lisp_Object vector;
  Lisp_Object *subprefix_stack;
  int submenu_depth = 0;
  int i;

  if (!f)
    return;
  subprefix_stack = (Lisp_Object *) alloca (f->menu_bar_items_used * sizeof (Lisp_Object));
  vector = f->menu_bar_vector;
  prefix = Qnil;
  i = 0;
  while (i < f->menu_bar_items_used)
    {
      if (EQ (XVECTOR (vector)->contents[i], Qnil))
	{
	  subprefix_stack[submenu_depth++] = prefix;
	  prefix = entry;
	  i++;
	}
      else if (EQ (XVECTOR (vector)->contents[i], Qlambda))
	{
	  prefix = subprefix_stack[--submenu_depth];
	  i++;
	}
      else if (EQ (XVECTOR (vector)->contents[i], Qt))
	{
	  prefix = XVECTOR (vector)->contents[i + MENU_ITEMS_PANE_PREFIX];
	  i += MENU_ITEMS_PANE_LENGTH;
	}
      else
	{
	  entry = XVECTOR (vector)->contents[i + MENU_ITEMS_ITEM_VALUE];
	  /* The EMACS_INT cast avoids a warning.  There's no problem
	     as long as pointers have enough bits to hold small integers.  */
	  if ((int) (EMACS_INT) client_data == i)
	    {
	      int j;
	      struct input_event buf;
	      Lisp_Object frame;

	      XSETFRAME (frame, f);
	      buf.kind = menu_bar_event;
	      buf.frame_or_window = Fcons (frame, Fcons (Qmenu_bar, Qnil));
	      kbd_buffer_store_event (&buf);

	      for (j = 0; j < submenu_depth; j++)
		if (!NILP (subprefix_stack[j]))
		  {
		    buf.kind = menu_bar_event;
		    buf.frame_or_window = Fcons (frame, subprefix_stack[j]);
		    kbd_buffer_store_event (&buf);
		  }

	      if (!NILP (prefix))
		{
		  buf.kind = menu_bar_event;
		  buf.frame_or_window = Fcons (frame, prefix);
		  kbd_buffer_store_event (&buf);
		}

	      buf.kind = menu_bar_event;
	      buf.frame_or_window = Fcons (frame, entry);
	      kbd_buffer_store_event (&buf);

	      return;
	    }
	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }
}

/* This callback is invoked when a dialog or menu is finished being
   used and has been unposted.  */

static void
popup_deactivate_callback (widget, id, client_data)
     Widget widget;
     LWLIB_ID id;
     XtPointer client_data;
{
  popup_activated_flag = 0;
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

/* Return a tree of widget_value structures for a menu bar item
   whose event type is ITEM_KEY (with string ITEM_NAME)
   and whose contents come from the list of keymaps MAPS.  */

static widget_value *
single_submenu (item_key, item_name, maps)
     Lisp_Object item_key, item_name, maps;
{
  widget_value *wv, *prev_wv, *save_wv, *first_wv;
  int i;
  int submenu_depth = 0;
  Lisp_Object length;
  int len;
  Lisp_Object *mapvec;
  widget_value **submenu_stack;
  int mapno;
  int previous_items = menu_items_used;

  length = Flength (maps);
  len = XINT (length);

  /* Convert the list MAPS into a vector MAPVEC.  */
  mapvec = (Lisp_Object *) alloca (len * sizeof (Lisp_Object));
  for (i = 0; i < len; i++)
    {
      mapvec[i] = Fcar (maps);
      maps = Fcdr (maps);
    }

  menu_items_n_panes = 0;

  /* Loop over the given keymaps, making a pane for each map.
     But don't make a pane that is empty--ignore that map instead.  */
  for (i = 0; i < len; i++)
    single_keymap_panes (mapvec[i], item_name, item_key, 0);

  /* Create a tree of widget_value objects
     representing the panes and their items.  */

  submenu_stack
    = (widget_value **) alloca (menu_items_used * sizeof (widget_value *));
  wv = malloc_widget_value ();
  wv->name = "menu";
  wv->value = 0;
  wv->enabled = 1;
  first_wv = wv;
  save_wv = 0;
 
  /* Loop over all panes and items made during this call
     and construct a tree of widget_value objects.
     Ignore the panes and items made by previous calls to
     single_submenu, even though those are also in menu_items.  */
  i = previous_items;
  while (i < menu_items_used)
    {
      if (EQ (XVECTOR (menu_items)->contents[i], Qnil))
	{
	  submenu_stack[submenu_depth++] = save_wv;
	  save_wv = prev_wv;
	  prev_wv = 0;
	  i++;
	}
      else if (EQ (XVECTOR (menu_items)->contents[i], Qlambda))
	{
	  prev_wv = save_wv;
	  save_wv = submenu_stack[--submenu_depth];
	  i++;
	}
      else if (EQ (XVECTOR (menu_items)->contents[i], Qt)
	       && submenu_depth != 0)
	i += MENU_ITEMS_PANE_LENGTH;
      /* Ignore a nil in the item list.
	 It's meaningful only for dialog boxes.  */
      else if (EQ (XVECTOR (menu_items)->contents[i], Qquote))
	i += 1;
      else if (EQ (XVECTOR (menu_items)->contents[i], Qt))
	{
	  /* Create a new pane.  */
	  Lisp_Object pane_name, prefix;
	  char *pane_string;
	  pane_name = XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_NAME];
	  prefix = XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_PREFIX];
	  pane_string = (NILP (pane_name)
			 ? "" : (char *) XSTRING (pane_name)->data);
	  /* If there is just one top-level pane, put all its items directly
	     under the top-level menu.  */
	  if (menu_items_n_panes == 1)
	    pane_string = "";

	  /* If the pane has a meaningful name,
	     make the pane a top-level menu item
	     with its items as a submenu beneath it.  */
	  if (strcmp (pane_string, ""))
	    {
	      wv = malloc_widget_value ();
	      if (save_wv)
		save_wv->next = wv;
	      else
		first_wv->contents = wv;
	      wv->name = pane_string;
	      if (!NILP (prefix))
		wv->name++;
	      wv->value = 0;
	      wv->enabled = 1;
	    }
	  save_wv = wv;
	  prev_wv = 0;
	  i += MENU_ITEMS_PANE_LENGTH;
	}
      else
	{
	  /* Create a new item within current pane.  */
	  Lisp_Object item_name, enable, descrip, def;
	  item_name = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_NAME];
	  enable = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_ENABLE];
	  descrip
	    = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_EQUIV_KEY];
	  def = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_DEFINITION];

	  wv = malloc_widget_value ();
	  if (prev_wv) 
	    prev_wv->next = wv;
	  else 
	    save_wv->contents = wv;
	  wv->name = (char *) XSTRING (item_name)->data;
	  if (!NILP (descrip))
	    wv->key = (char *) XSTRING (descrip)->data;
	  wv->value = 0;
	  /* The EMACS_INT cast avoids a warning.  There's no problem
	     as long as pointers have enough bits to hold small integers.  */
	  wv->call_data = (!NILP (def) ? (void *) (EMACS_INT) i : 0);
	  wv->enabled = !NILP (enable);
	  prev_wv = wv;

	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }

  return first_wv;
}

extern void EmacsFrameSetCharSize ();

/* Recompute the menu bar of frame F.  */

static void
update_frame_menubar (f)
     FRAME_PTR f;
{
  struct x_display *x = f->display.x;
  int columns, rows;
  int menubar_changed;
  
  Dimension shell_height;

  /* We assume the menubar contents has changed if the global flag is set,
     or if the current buffer has changed, or if the menubar has never
     been updated before.
   */
  menubar_changed = (x->menubar_widget
		     && !XtIsManaged (x->menubar_widget));

  if (! (menubar_changed))
    return;

  BLOCK_INPUT;
  /* Save the size of the frame because the pane widget doesn't accept to
     resize itself. So force it.  */
  columns = f->width;
  rows = f->height;

  /* Do the voodoo which means "I'm changing lots of things, don't try to
     refigure sizes until I'm done." */
  lw_refigure_widget (x->column_widget, False);

  /* the order in which children are managed is the top to
     bottom order in which they are displayed in the paned window.
     First, remove the text-area widget.
   */
  XtUnmanageChild (x->edit_widget);

  /* remove the menubar that is there now, and put up the menubar that
     should be there.
   */
  if (menubar_changed)
    {
      XtManageChild (x->menubar_widget);
      XtMapWidget (x->menubar_widget);
      XtVaSetValues (x->menubar_widget, XtNmappedWhenManaged, 1, 0);
    }

  /* Re-manage the text-area widget, and then thrash the sizes.  */
  XtManageChild (x->edit_widget);
  lw_refigure_widget (x->column_widget, True);

  /* Force the pane widget to resize itself with the right values.  */
  EmacsFrameSetCharSize (x->edit_widget, columns, rows);

  UNBLOCK_INPUT;
}

void
set_frame_menubar (f, first_time)
     FRAME_PTR f;
     int first_time;
{
  Widget menubar_widget = f->display.x->menubar_widget;
  Lisp_Object tail, items, frame;
  widget_value *wv, *first_wv, *prev_wv = 0;
  int i;
  int id;
  int count;

  count = inhibit_garbage_collection ();

  id = frame_vector_add_frame (f);

  wv = malloc_widget_value ();
  wv->name = "menubar";
  wv->value = 0;
  wv->enabled = 1;
  first_wv = wv;
  items = FRAME_MENU_BAR_ITEMS (f);
  menu_items = f->menu_bar_vector;
  menu_items_allocated = XVECTOR (menu_items)->size;
  init_menu_items ();

  for (i = 0; i < XVECTOR (items)->size; i += 3)
    {
      Lisp_Object key, string, maps;

      key = XVECTOR (items)->contents[i];
      string = XVECTOR (items)->contents[i + 1];
      maps = XVECTOR (items)->contents[i + 2];
      if (NILP (string))
	break;

      wv = single_submenu (key, string, maps);
      if (prev_wv) 
	prev_wv->next = wv;
      else
	first_wv->contents = wv;
      /* Don't set wv->name here; GC during the loop might relocate it.  */
      wv->enabled = 1;
      prev_wv = wv;
    }

  /* Now GC cannot happen during the lifetime of the widget_value,
     so it's safe to store data from a Lisp_String.  */
  wv = first_wv->contents;
  for (i = 0; i < XVECTOR (items)->size; i += 3)
    {
      Lisp_Object string;
      string = XVECTOR (items)->contents[i + 1];
      if (NILP (string))
	break;
      wv->name = (char *) XSTRING (string)->data;
      wv = wv->next;
    }

  finish_menu_items ();

  f->menu_bar_vector = menu_items;
  f->menu_bar_items_used = menu_items_used;
  menu_items = Qnil;

  unbind_to (count, Qnil);

  BLOCK_INPUT;

  if (menubar_widget)
    {
      /* Disable resizing (done for Motif!) */
      lw_allow_resizing (f->display.x->widget, False);

      /* The third arg is DEEP_P, which says to consider the entire
	 menu trees we supply, rather than just the menu bar item names.  */
      lw_modify_all_widgets ((LWLIB_ID) id, first_wv, 1);

      /* Re-enable the edit widget to resize.  */
      lw_allow_resizing (f->display.x->widget, True);
    }
  else
    {
      menubar_widget = lw_create_widget ("menubar", "menubar", 
					 (LWLIB_ID) id, first_wv, 
					 f->display.x->column_widget,
					 0,
					 popup_activate_callback,
					 menubar_selection_callback,
					 popup_deactivate_callback);
      f->display.x->menubar_widget = menubar_widget;
    }

  {
    int menubar_size 
      = (f->display.x->menubar_widget
	 ? (f->display.x->menubar_widget->core.height
	    + f->display.x->menubar_widget->core.border_width)
	 : 0);

    if (FRAME_EXTERNAL_MENU_BAR (f))
      {
        Dimension ibw = 0;
        XtVaGetValues (f->display.x->column_widget,
		       XtNinternalBorderWidth, &ibw, NULL);
        menubar_size += ibw;
      }

    f->display.x->menubar_height = menubar_size;
  }
  
  free_menubar_widget_value_tree (first_wv);

  /* Don't update the menubar the first time it is created via x_window.  */
  if (!first_time)
    update_frame_menubar (f);

  UNBLOCK_INPUT;
}

/* Called from Fx_create_frame to create the inital menubar of a frame
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
  set_frame_menubar (f, 1);
}

/* Get rid of the menu bar of frame F, and free its storage.
   This is used when deleting a frame, and when turning off the menu bar.  */

void
free_frame_menubar (f)
     FRAME_PTR f;
{
  Widget menubar_widget;
  int id;

  menubar_widget = f->display.x->menubar_widget;
  
  if (menubar_widget)
    {
      id = frame_vector_add_frame (f);
      BLOCK_INPUT;
      lw_destroy_all_widgets ((LWLIB_ID) id);
      XVECTOR (frame_vector)->contents[id] = Qnil;
      UNBLOCK_INPUT;
    }
}

#endif /* USE_X_TOOLKIT */

/* xmenu_show actually displays a menu using the panes and items in menu_items
   and returns the value selected from it.
   There are two versions of xmenu_show, one for Xt and one for Xlib.
   Both assume input is blocked by the caller.  */

/* F is the frame the menu is for.
   X and Y are the frame-relative specified position,
   relative to the inside upper left corner of the frame F.
   FOR_CLICK if this menu was invoked for a mouse click.
   KEYMAPS is 1 if this menu was specified with keymaps;
    in that case, we return a list containing the chosen item's value
    and perhaps also the pane's prefix.
   TITLE is the specified menu title.
   ERROR is a place to store an error message string in case of failure.
   (We return nil on failure, but the value doesn't actually matter.)  */

#ifdef USE_X_TOOLKIT

/* We need a unique id for each widget handled by the Lucid Widget
   library.

   For the main windows, and popup menus, we use this counter,
   which we increment each time after use.

   For menu bars, we use the index of the frame in frame_vector
   as the id.  */
LWLIB_ID widget_id_tick;

#ifdef __STDC__
static Lisp_Object *volatile menu_item_selection;
#else
static Lisp_Object *menu_item_selection;
#endif

static void
popup_selection_callback (widget, id, client_data)
     Widget widget;
     LWLIB_ID id;
     XtPointer client_data;
{
  menu_item_selection = (Lisp_Object *) client_data;
}

static Lisp_Object
xmenu_show (f, x, y, for_click, keymaps, title, error)
     FRAME_PTR f;
     int x;
     int y;
     int for_click;
     int keymaps;
     Lisp_Object title;
     char **error;
{
  int i;
  LWLIB_ID menu_id;
  Widget menu;
  Arg av [2];
  int ac = 0;
  widget_value *wv, *save_wv = 0, *first_wv = 0, *prev_wv = 0;
  widget_value **submenu_stack
    = (widget_value **) alloca (menu_items_used * sizeof (widget_value *));
  Lisp_Object *subprefix_stack
    = (Lisp_Object *) alloca (menu_items_used * sizeof (Lisp_Object));
  int submenu_depth = 0;

  Position root_x, root_y;

  int first_pane;
  int next_release_must_exit = 0;

  *error = NULL;

  if (menu_items_used <= MENU_ITEMS_PANE_LENGTH)
    {
      *error = "Empty menu";
      return Qnil;
    }

  /* Create a tree of widget_value objects
     representing the panes and their items.  */
  wv = malloc_widget_value ();
  wv->name = "menu";
  wv->value = 0;
  wv->enabled = 1;
  first_wv = wv;
  first_pane = 1;
 
  /* Loop over all panes and items, filling in the tree.  */
  i = 0;
  while (i < menu_items_used)
    {
      if (EQ (XVECTOR (menu_items)->contents[i], Qnil))
	{
	  submenu_stack[submenu_depth++] = save_wv;
	  save_wv = prev_wv;
	  prev_wv = 0;
	  first_pane = 1;
	  i++;
	}
      else if (EQ (XVECTOR (menu_items)->contents[i], Qlambda))
	{
	  prev_wv = save_wv;
	  save_wv = submenu_stack[--submenu_depth];
	  first_pane = 0;
	  i++;
	}
      else if (EQ (XVECTOR (menu_items)->contents[i], Qt)
	       && submenu_depth != 0)
	i += MENU_ITEMS_PANE_LENGTH;
      /* Ignore a nil in the item list.
	 It's meaningful only for dialog boxes.  */
      else if (EQ (XVECTOR (menu_items)->contents[i], Qquote))
	i += 1;
      else if (EQ (XVECTOR (menu_items)->contents[i], Qt))
	{
	  /* Create a new pane.  */
	  Lisp_Object pane_name, prefix;
	  char *pane_string;
	  pane_name = XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_NAME];
	  prefix = XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_PREFIX];
	  pane_string = (NILP (pane_name)
			 ? "" : (char *) XSTRING (pane_name)->data);
	  /* If there is just one top-level pane, put all its items directly
	     under the top-level menu.  */
	  if (menu_items_n_panes == 1)
	    pane_string = "";

	  /* If the pane has a meaningful name,
	     make the pane a top-level menu item
	     with its items as a submenu beneath it.  */
	  if (!keymaps && strcmp (pane_string, ""))
	    {
	      wv = malloc_widget_value ();
	      if (save_wv)
		save_wv->next = wv;
	      else
		first_wv->contents = wv;
	      wv->name = pane_string;
	      if (keymaps && !NILP (prefix))
		wv->name++;
	      wv->value = 0;
	      wv->enabled = 1;
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
	  Lisp_Object item_name, enable, descrip, def;
	  item_name = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_NAME];
	  enable = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_ENABLE];
	  descrip
	    = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_EQUIV_KEY];
	  def = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_DEFINITION];

	  wv = malloc_widget_value ();
	  if (prev_wv) 
	    prev_wv->next = wv;
	  else 
	    save_wv->contents = wv;
	  wv->name = (char *) XSTRING (item_name)->data;
	  if (!NILP (descrip))
	    wv->key = (char *) XSTRING (descrip)->data;
	  wv->value = 0;
	  /* If this item has a null value,
	     make the call_data null so that it won't display a box
	     when the mouse is on it.  */
	  wv->call_data
	    = (!NILP (def) ? (void *) &XVECTOR (menu_items)->contents[i] : 0);
	  wv->enabled = !NILP (enable);
	  prev_wv = wv;

	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }

  /* Deal with the title, if it is non-nil.  */
  if (!NILP (title))
    {
      widget_value *wv_title = malloc_widget_value ();
      widget_value *wv_sep1 = malloc_widget_value ();
      widget_value *wv_sep2 = malloc_widget_value ();

      wv_sep2->name = "--";
      wv_sep2->next = first_wv->contents;

      wv_sep1->name = "--";
      wv_sep1->next = wv_sep2;

      wv_title->name = (char *) XSTRING (title)->data;
      wv_title->enabled = True;
      wv_title->next = wv_sep1;
      first_wv->contents = wv_title;
    }

  /* Actually create the menu.  */
  menu_id = widget_id_tick++;
  menu = lw_create_widget ("popup", first_wv->name, menu_id, first_wv,
			   f->display.x->widget, 1, 0,
			   popup_selection_callback,
			   popup_deactivate_callback);

  /* Don't allow any geometry request from the user.  */
  XtSetArg (av[ac], XtNgeometry, 0); ac++;
  XtSetValues (menu, av, ac);

  /* Free the widget_value objects we used to specify the contents.  */
  free_menubar_widget_value_tree (first_wv);

  /* No selection has been chosen yet.  */
  menu_item_selection = 0;

  /* Display the menu.  */
  lw_popup_menu (menu);
  popup_activated_flag = 1;

  /* Process events that apply to the menu.  */
  popup_get_selection ((XEvent *) 0, FRAME_X_DISPLAY_INFO (f));

#if 0
  /* fp turned off the following statement and wrote a comment
     that it is unnecessary--that the menu has already disappeared.
     I observer that is not so. -- rms.  */
  /* Make sure the menu disappears.  */
  lw_destroy_all_widgets (menu_id); 
#endif

  /* Find the selected item, and its pane, to return
     the proper value.  */
  if (menu_item_selection != 0)
    {
      Lisp_Object prefix, entry;

      prefix = Qnil;
      i = 0;
      while (i < menu_items_used)
	{
	  if (EQ (XVECTOR (menu_items)->contents[i], Qnil))
	    {
	      subprefix_stack[submenu_depth++] = prefix;
	      prefix = entry;
	      i++;
	    }
	  else if (EQ (XVECTOR (menu_items)->contents[i], Qlambda))
	    {
	      prefix = subprefix_stack[--submenu_depth];
	      i++;
	    }
	  else if (EQ (XVECTOR (menu_items)->contents[i], Qt))
	    {
	      prefix
		= XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_PREFIX];
	      i += MENU_ITEMS_PANE_LENGTH;
	    }
	  else
	    {
	      entry
		= XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_VALUE];
	      if (menu_item_selection == &XVECTOR (menu_items)->contents[i])
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

  return Qnil;
}

static void
dialog_selection_callback (widget, id, client_data)
     Widget widget;
     LWLIB_ID id;
     XtPointer client_data;
{
  /* The EMACS_INT cast avoids a warning.  There's no problem
     as long as pointers have enough bits to hold small integers.  */
  if ((int) (EMACS_INT) client_data != -1)
    menu_item_selection = (Lisp_Object *) client_data;
  BLOCK_INPUT;
  lw_destroy_all_widgets (id);
  UNBLOCK_INPUT;
  popup_activated_flag = 0;
}

static char * button_names [] = {
  "button1", "button2", "button3", "button4", "button5",
  "button6", "button7", "button8", "button9", "button10" };

static Lisp_Object
xdialog_show (f, keymaps, title, error)
     FRAME_PTR f;
     int keymaps;
     Lisp_Object title;
     char **error;
{
  int i, nb_buttons=0;
  LWLIB_ID dialog_id;
  Widget menu;
  char dialog_name[6];

  widget_value *wv, *save_wv = 0, *first_wv = 0, *prev_wv = 0;

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
    pane_name = XVECTOR (menu_items)->contents[MENU_ITEMS_PANE_NAME];
    prefix = XVECTOR (menu_items)->contents[MENU_ITEMS_PANE_PREFIX];
    pane_string = (NILP (pane_name)
		   ? "" : (char *) XSTRING (pane_name)->data);  
    prev_wv = malloc_widget_value ();
    prev_wv->value = pane_string;
    if (keymaps && !NILP (prefix))
      prev_wv->name++;
    prev_wv->enabled = 1;
    prev_wv->name = "message";
    first_wv = prev_wv;
 
    /* Loop over all panes and items, filling in the tree.  */
    i = MENU_ITEMS_PANE_LENGTH;
    while (i < menu_items_used)
      {
	
	/* Create a new item within current pane.  */
	Lisp_Object item_name, enable, descrip;
	item_name = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_NAME];
	enable = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_ENABLE];
	descrip
	  = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_EQUIV_KEY];
	
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
	if (nb_buttons >= 10)
	  {
	    free_menubar_widget_value_tree (first_wv);
	    *error = "Too many dialog items";
	    return Qnil;
	  }

	wv = malloc_widget_value ();
	prev_wv->next = wv;
	wv->name = (char *) button_names[nb_buttons];
	if (!NILP (descrip))
	  wv->key = (char *) XSTRING (descrip)->data;
	wv->value = (char *) XSTRING (item_name)->data;
	wv->call_data = (void *) &XVECTOR (menu_items)->contents[i];
	wv->enabled = !NILP (enable);
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

    wv = malloc_widget_value ();
    wv->name = dialog_name;

    /* Dialog boxes use a really stupid name encoding
       which specifies how many buttons to use
       and how many buttons are on the right.
       The Q means something also.  */
    dialog_name[0] = 'Q';
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
			   f->display.x->widget, 1, 0,
			   dialog_selection_callback, 0);
  lw_modify_all_widgets (dialog_id, first_wv->contents, True);
  /* Free the widget_value objects we used to specify the contents.  */
  free_menubar_widget_value_tree (first_wv);

  /* No selection has been chosen yet.  */
  menu_item_selection = 0;

  /* Display the menu.  */
  lw_pop_up_all_widgets (dialog_id);
  popup_activated_flag = 1;

  /* Process events that apply to the menu.  */
  popup_get_selection ((XEvent *) 0, FRAME_X_DISPLAY_INFO (f));

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

	  if (EQ (XVECTOR (menu_items)->contents[i], Qt))
	    {
	      prefix
		= XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_PREFIX];
	      i += MENU_ITEMS_PANE_LENGTH;
	    }
	  else
	    {
	      entry
		= XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_VALUE];
	      if (menu_item_selection == &XVECTOR (menu_items)->contents[i])
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

  return Qnil;
}
#else /* not USE_X_TOOLKIT */

static Lisp_Object
xmenu_show (f, x, y, for_click, keymaps, title, error)
     FRAME_PTR f;
     int x, y;
     int for_click;
     int keymaps;
     Lisp_Object title;
     char **error;
{
  Window root;		
  XMenu *menu;
  int pane, selidx, lpane, status;
  Lisp_Object entry, pane_prefix;
  char *datap;
  int ulx, uly, width, height;
  int dispwidth, dispheight;
  int i, j;
  int maxwidth;
  int dummy_int;
  unsigned int dummy_uint;

  *error = 0;
  if (menu_items_n_panes == 0)
    return Qnil;

  if (menu_items_used <= MENU_ITEMS_PANE_LENGTH)
    {
      *error = "Empty menu";
      return Qnil;
    }

  /* Figure out which root window F is on.  */
  XGetGeometry (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), &root,
		&dummy_int, &dummy_int, &dummy_uint, &dummy_uint,
		&dummy_uint, &dummy_uint);

  /* Make the menu on that window.  */
  menu = XMenuCreate (FRAME_X_DISPLAY (f), root, "emacs");
  if (menu == NULL)
    {
      *error = "Can't create menu";
      return Qnil;
    }

#ifdef HAVE_X_WINDOWS
  /* Adjust coordinates to relative to the outer (window manager) window.  */
  {
    Window child;
    int win_x = 0, win_y = 0;

    /* Find the position of the outside upper-left corner of
       the inner window, with respect to the outer window.  */
    if (f->display.x->parent_desc != FRAME_X_DISPLAY_INFO (f)->root_window)
      {
	BLOCK_INPUT;
	XTranslateCoordinates (FRAME_X_DISPLAY (f),

			       /* From-window, to-window.  */
			       f->display.x->window_desc,
			       f->display.x->parent_desc,

			       /* From-position, to-position.  */
			       0, 0, &win_x, &win_y,

			       /* Child of window.  */
			       &child);
	UNBLOCK_INPUT;
	x += win_x;
	y += win_y;
      }
  }
#endif /* HAVE_X_WINDOWS */

  /* Adjust coordinates to be root-window-relative.  */
  x += f->display.x->left_pos;
  y += f->display.x->top_pos;
 
  /* Create all the necessary panes and their items.  */
  i = 0;
  while (i < menu_items_used)
    {
      if (EQ (XVECTOR (menu_items)->contents[i], Qt))
	{
	  /* Create a new pane.  */
	  Lisp_Object pane_name, prefix;
	  char *pane_string;

	  pane_name = XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_NAME];
	  prefix = XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_PREFIX];
	  pane_string = (NILP (pane_name)
			 ? "" : (char *) XSTRING (pane_name)->data);
	  if (keymaps && !NILP (prefix))
	    pane_string++;

	  lpane = XMenuAddPane (FRAME_X_DISPLAY (f), menu, pane_string, TRUE);
	  if (lpane == XM_FAILURE)
	    {
	      XMenuDestroy (FRAME_X_DISPLAY (f), menu);
	      *error = "Can't create pane";
	      return Qnil;
	    }
	  i += MENU_ITEMS_PANE_LENGTH;

	  /* Find the width of the widest item in this pane.  */
	  maxwidth = 0;
	  j = i;
	  while (j < menu_items_used)
	    {
	      Lisp_Object item;
	      item = XVECTOR (menu_items)->contents[j];
	      if (EQ (item, Qt))
		break;
	      if (NILP (item))
		{
		  j++;
		  continue;
		}
	      width = XSTRING (item)->size;
	      if (width > maxwidth)
		maxwidth = width;

	      j += MENU_ITEMS_ITEM_LENGTH;
	    }
	}
      /* Ignore a nil in the item list.
	 It's meaningful only for dialog boxes.  */
      else if (EQ (XVECTOR (menu_items)->contents[i], Qquote))
	i += 1;
      else
	{
	  /* Create a new item within current pane.  */
	  Lisp_Object item_name, enable, descrip;
	  unsigned char *item_data;

	  item_name = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_NAME];
	  enable = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_ENABLE];
	  descrip
	    = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_EQUIV_KEY];
	  if (!NILP (descrip))
	    {
	      int gap = maxwidth - XSTRING (item_name)->size;
#ifdef C_ALLOCA
	      Lisp_Object spacer;
	      spacer = Fmake_string (make_number (gap), make_number (' '));
	      item_name = concat2 (item_name, spacer);
	      item_name = concat2 (item_name, descrip);
	      item_data = XSTRING (item_name)->data;
#else
	      /* if alloca is fast, use that to make the space,
		 to reduce gc needs.  */
	      item_data
		= (unsigned char *) alloca (maxwidth
					    + XSTRING (descrip)->size + 1);
	      bcopy (XSTRING (item_name)->data, item_data,
		     XSTRING (item_name)->size);
	      for (j = XSTRING (item_name)->size; j < maxwidth; j++)
		item_data[j] = ' ';
	      bcopy (XSTRING (descrip)->data, item_data + j,
		     XSTRING (descrip)->size);
	      item_data[j + XSTRING (descrip)->size] = 0;
#endif
	    }
	  else
	    item_data = XSTRING (item_name)->data;

	  if (XMenuAddSelection (FRAME_X_DISPLAY (f),
				 menu, lpane, 0, item_data,
				 !NILP (enable))
	      == XM_FAILURE)
	    {
	      XMenuDestroy (FRAME_X_DISPLAY (f), menu);
	      *error = "Can't add selection to menu";
	      return Qnil;
	    }
	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }

  /* All set and ready to fly.  */
  XMenuRecompute (FRAME_X_DISPLAY (f), menu);
  dispwidth = DisplayWidth (FRAME_X_DISPLAY (f),
			    XScreenNumberOfScreen (FRAME_X_SCREEN (f)));
  dispheight = DisplayHeight (FRAME_X_DISPLAY (f),
			      XScreenNumberOfScreen (FRAME_X_SCREEN (f)));
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
  if (ulx < 0) x -= ulx;
  if (uly < 0) y -= uly;

  XMenuSetAEQ (menu, TRUE);
  XMenuSetFreeze (menu, TRUE);
  pane = selidx = 0;
  
  status = XMenuActivate (FRAME_X_DISPLAY (f), menu, &pane, &selidx,
			  x, y, ButtonReleaseMask, &datap);


  /* Assume the mouse has moved out of the X window.
     If it has actually moved in, we will get an EnterNotify.  */
  x_mouse_leave (FRAME_X_DISPLAY_INFO (f));

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
	  if (EQ (XVECTOR (menu_items)->contents[i], Qt))
	    {
	      if (pane == 0)
		pane_prefix
		  = XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_PREFIX];
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
			= XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_VALUE];
		      if (keymaps != 0)
			{
			  entry = Fcons (entry, Qnil);
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
      *error = "Can't activate menu";
    case XM_IA_SELECT:
    case XM_NO_SELECT:
      entry = Qnil;
      break;
    }
  XMenuDestroy (FRAME_X_DISPLAY (f), menu);

#ifdef HAVE_X_WINDOWS
  /* State that no mouse buttons are now held.
     (The oldXMenu code doesn't track this info for us.)
     That is not necessarily true, but the fiction leads to reasonable
     results, and it is a pain to ask which are actually held now.  */
  FRAME_X_DISPLAY_INFO (f)->grabbed = 0;
#endif

  return entry;
}

#endif /* not USE_X_TOOLKIT */

syms_of_xmenu ()
{
  staticpro (&menu_items);
  menu_items = Qnil;

#ifdef USE_X_TOOLKIT
  widget_id_tick = (1<<16);	
#endif

  staticpro (&frame_vector);
  frame_vector = Fmake_vector (make_number (10), Qnil);

  defsubr (&Sx_popup_menu);
  defsubr (&Sx_popup_dialog);
}
