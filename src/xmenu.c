/* X Communication module for terminals which understand the X protocol.
   Copyright (C) 1986, 1988, 1993, 1994, 1996, 1999, 2000, 2001, 2002, 2003,
                 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* X pop-up deck-of-cards menu facility for GNU Emacs.
 *
 * Written by Jon Arnold and Roman Budzianowski
 * Mods and rewrite by Robert Krawitz
 *
 */

/* Modified by Fred Pierresteguy on December 93
   to make the popup menus and menubar use the Xt.  */

/* Rewritten for clarity and GC protection by rms in Feb 94.  */

#include <config.h>

#if 0  /* Why was this included?  And without syssignal.h?  */
/* On 4.3 this loses if it comes after xterm.h.  */
#include <signal.h>
#endif

#include <stdio.h>

#include "lisp.h"
#include "keyboard.h"
#include "keymap.h"
#include "frame.h"
#include "termhooks.h"
#include "window.h"
#include "blockinput.h"
#include "buffer.h"
#include "charset.h"
#include "coding.h"
#include "sysselect.h"

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
#ifdef HAVE_XAW3D
#include <X11/Xaw3d/Paned.h>
#else /* !HAVE_XAW3D */
#include <X11/Xaw/Paned.h>
#endif /* HAVE_XAW3D */
#endif /* USE_LUCID */
#include "../lwlib/lwlib.h"
#else /* not USE_X_TOOLKIT */
#ifndef USE_GTK
#include "../oldXMenu/XMenu.h"
#endif
#endif /* not USE_X_TOOLKIT */
#endif /* HAVE_X_WINDOWS */

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif /* no TRUE */

Lisp_Object Qdebug_on_next_call;

extern Lisp_Object Vmenu_updating_frame;

extern Lisp_Object Qmenu_bar;

extern Lisp_Object QCtoggle, QCradio;

extern Lisp_Object Voverriding_local_map;
extern Lisp_Object Voverriding_local_map_menu_flag;

extern Lisp_Object Qoverriding_local_map, Qoverriding_terminal_local_map;

extern Lisp_Object Qmenu_bar_update_hook;

#ifdef USE_X_TOOLKIT
extern void set_frame_menubar P_ ((FRAME_PTR, int, int));
extern XtAppContext Xt_app_con;

static Lisp_Object xdialog_show P_ ((FRAME_PTR, int, Lisp_Object, Lisp_Object,
				     char **));
static void popup_get_selection P_ ((XEvent *, struct x_display_info *,
                                     LWLIB_ID, int));

/* Define HAVE_BOXES if menus can handle radio and toggle buttons.  */

#define HAVE_BOXES 1
#endif /* USE_X_TOOLKIT */

#ifdef USE_GTK
#include "gtkutil.h"
#define HAVE_BOXES 1
extern void set_frame_menubar P_ ((FRAME_PTR, int, int));
static Lisp_Object xdialog_show P_ ((FRAME_PTR, int, Lisp_Object, Lisp_Object,
				     char **));
#endif

/* This is how to deal with multibyte text if HAVE_MULTILINGUAL_MENU
   isn't defined.  The use of HAVE_MULTILINGUAL_MENU could probably be
   confined to an extended version of this with sections of code below
   using it unconditionally.  */
#ifdef USE_GTK
/* gtk just uses utf-8.  */
# define ENCODE_MENU_STRING(str) ENCODE_UTF_8 (str)
#elif defined HAVE_X_I18N
# define ENCODE_MENU_STRING(str) ENCODE_SYSTEM (str)
#else
# define ENCODE_MENU_STRING(str) string_make_unibyte (str)
#endif

static void push_menu_item P_ ((Lisp_Object, Lisp_Object, Lisp_Object,
				Lisp_Object, Lisp_Object, Lisp_Object,
				Lisp_Object, Lisp_Object));
static int update_frame_menubar P_ ((struct frame *));
static Lisp_Object xmenu_show P_ ((struct frame *, int, int, int, int,
				   Lisp_Object, char **));
static void keymap_panes P_ ((Lisp_Object *, int, int));
static void single_keymap_panes P_ ((Lisp_Object, Lisp_Object, Lisp_Object,
				     int, int));
static void list_of_panes P_ ((Lisp_Object));
static void list_of_items P_ ((Lisp_Object));


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

/* If non-nil, means that the global vars defined here are already in use.
   Used to detect cases where we try to re-enter this non-reentrant code.  */
static Lisp_Object menu_items_inuse;

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

static int next_menubar_widget_id;

/* This is set nonzero after the user activates the menu bar, and set
   to zero again after the menu bars are redisplayed by prepare_menu_bar.
   While it is nonzero, all calls to set_frame_menubar go deep.

   I don't understand why this is needed, but it does seem to be
   needed on Motif, according to Marcus Daniels <marcus@sysc.pdx.edu>.  */

int pending_menu_activation;

#ifdef USE_X_TOOLKIT

/* Return the frame whose ->output_data.x->id equals ID, or 0 if none.  */

static struct frame *
menubar_id_to_frame (id)
     LWLIB_ID id;
{
  Lisp_Object tail, frame;
  FRAME_PTR f;

  for (tail = Vframe_list; CONSP (tail); tail = XCDR (tail))
    {
      frame = XCAR (tail);
      if (!FRAMEP (frame))
        continue;
      f = XFRAME (frame);
      if (!FRAME_WINDOW_P (f))
	continue;
      if (f->output_data.x->id == id)
	return f;
    }
  return 0;
}

#endif

/* Initialize the menu_items structure if we haven't already done so.
   Also mark it as currently empty.  */

static void
init_menu_items ()
{
  if (!NILP (menu_items_inuse))
    error ("Trying to use a menu from within a menu-entry");

  if (NILP (menu_items))
    {
      menu_items_allocated = 60;
      menu_items = Fmake_vector (make_number (menu_items_allocated), Qnil);
    }

  menu_items_inuse = Qt;
  menu_items_used = 0;
  menu_items_n_panes = 0;
  menu_items_submenu_depth = 0;
}

/* Call at the end of generating the data in menu_items.  */

static void
finish_menu_items ()
{
}

static Lisp_Object
unuse_menu_items (dummy)
     Lisp_Object dummy;
{
  return menu_items_inuse = Qnil;
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
  xassert (NILP (menu_items_inuse));
}

/* This undoes save_menu_items, and it is called by the specpdl unwind
   mechanism.  */

static Lisp_Object
restore_menu_items (saved)
     Lisp_Object saved;
{
  menu_items = XCAR (saved);
  menu_items_inuse = (! NILP (menu_items) ? Qt : Qnil);
  menu_items_allocated = (VECTORP (menu_items) ? ASIZE (menu_items) : 0);
  saved = XCDR (saved);
  menu_items_used = XINT (XCAR (saved));
  saved = XCDR (saved);
  menu_items_n_panes = XINT (XCAR (saved));
  saved = XCDR (saved);
  menu_items_submenu_depth = XINT (XCAR (saved));
  return Qnil;
}

/* Push the whole state of menu_items processing onto the specpdl.
   It will be restored when the specpdl is unwound.  */

static void
save_menu_items ()
{
  Lisp_Object saved = list4 (!NILP (menu_items_inuse) ? menu_items : Qnil,
			     make_number (menu_items_used),
			     make_number (menu_items_n_panes),
			     make_number (menu_items_submenu_depth));
  record_unwind_protect (restore_menu_items, saved);
  menu_items_inuse = Qnil;
  menu_items = Qnil;
}

/* Make the menu_items vector twice as large.  */

static void
grow_menu_items ()
{
  menu_items_allocated *= 2;
  menu_items = larger_vector (menu_items, menu_items_allocated, Qnil);
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
  XVECTOR (menu_items)->contents[menu_items_used++] = Qt;
  XVECTOR (menu_items)->contents[menu_items_used++] = name;
  XVECTOR (menu_items)->contents[menu_items_used++] = prefix_vec;
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

  XVECTOR (menu_items)->contents[menu_items_used++] = name;
  XVECTOR (menu_items)->contents[menu_items_used++] = enable;
  XVECTOR (menu_items)->contents[menu_items_used++] = key;
  XVECTOR (menu_items)->contents[menu_items_used++] = equiv;
  XVECTOR (menu_items)->contents[menu_items_used++] = def;
  XVECTOR (menu_items)->contents[menu_items_used++] = type;
  XVECTOR (menu_items)->contents[menu_items_used++] = selected;
  XVECTOR (menu_items)->contents[menu_items_used++] = help;
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

/* Args passed between single_keymap_panes and single_menu_item.  */
struct skp
  {
     Lisp_Object pending_maps;
     int maxdepth, notreal;
     int notbuttons;
  };

static void single_menu_item P_ ((Lisp_Object, Lisp_Object, Lisp_Object,
				  void *));

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
  struct skp skp;
  struct gcpro gcpro1;

  skp.pending_maps = Qnil;
  skp.maxdepth = maxdepth;
  skp.notreal = notreal;
  skp.notbuttons = 0;

  if (maxdepth <= 0)
    return;

  push_menu_pane (pane_name, prefix);

#ifndef HAVE_BOXES
  /* Remember index for first item in this pane so we can go back and
     add a prefix when (if) we see the first button.  After that, notbuttons
     is set to 0, to mark that we have seen a button and all non button
     items need a prefix.  */
  skp.notbuttons = menu_items_used;
#endif

  GCPRO1 (skp.pending_maps);
  map_keymap (keymap, single_menu_item, Qnil, &skp, 1);
  UNGCPRO;

  /* Process now any submenus which want to be panes at this level.  */
  while (CONSP (skp.pending_maps))
    {
      Lisp_Object elt, eltcdr, string;
      elt = XCAR (skp.pending_maps);
      eltcdr = XCDR (elt);
      string = XCAR (eltcdr);
      /* We no longer discard the @ from the beginning of the string here.
	 Instead, we do this in xmenu_show.  */
      single_keymap_panes (Fcar (elt), string,
			   XCDR (eltcdr), notreal, maxdepth - 1);
      skp.pending_maps = XCDR (skp.pending_maps);
    }
}

/* This is a subroutine of single_keymap_panes that handles one
   keymap entry.
   KEY is a key in a keymap and ITEM is its binding.
   SKP->PENDING_MAPS_PTR is a list of keymaps waiting to be made into
   separate panes.
   If SKP->NOTREAL is nonzero, only check for equivalent key bindings, don't
   evaluate expressions in menu items and don't make any menu.
   If we encounter submenus deeper than SKP->MAXDEPTH levels, ignore them.
   SKP->NOTBUTTONS is only used when simulating toggle boxes and radio
   buttons.  It keeps track of if we have seen a button in this menu or
   not.  */

static void
single_menu_item (key, item, dummy, skp_v)
     Lisp_Object key, item, dummy;
     void *skp_v;
{
  Lisp_Object map, item_string, enabled;
  struct gcpro gcpro1, gcpro2;
  int res;
  struct skp *skp = skp_v;

  /* Parse the menu item and leave the result in item_properties.  */
  GCPRO2 (key, item);
  res = parse_menu_item (item, skp->notreal, 0);
  UNGCPRO;
  if (!res)
    return;			/* Not a menu item.  */

  map = XVECTOR (item_properties)->contents[ITEM_PROPERTY_MAP];

  if (skp->notreal)
    {
      /* We don't want to make a menu, just traverse the keymaps to
	 precompute equivalent key bindings.  */
      if (!NILP (map))
	single_keymap_panes (map, Qnil, key, 1, skp->maxdepth - 1);
      return;
    }

  enabled = XVECTOR (item_properties)->contents[ITEM_PROPERTY_ENABLE];
  item_string = XVECTOR (item_properties)->contents[ITEM_PROPERTY_NAME];

  if (!NILP (map) && SREF (item_string, 0) == '@')
    {
      if (!NILP (enabled))
	/* An enabled separate pane. Remember this to handle it later.  */
	skp->pending_maps = Fcons (Fcons (map, Fcons (item_string, key)),
				   skp->pending_maps);
      return;
    }

#ifndef HAVE_BOXES
  /* Simulate radio buttons and toggle boxes by putting a prefix in
     front of them.  */
  {
    Lisp_Object prefix = Qnil;
    Lisp_Object type = XVECTOR (item_properties)->contents[ITEM_PROPERTY_TYPE];
    if (!NILP (type))
      {
	Lisp_Object selected
	  = XVECTOR (item_properties)->contents[ITEM_PROPERTY_SELECTED];

	if (skp->notbuttons)
	  /* The first button. Line up previous items in this menu.  */
	  {
	    int index = skp->notbuttons; /* Index for first item this menu.  */
	    int submenu = 0;
	    Lisp_Object tem;
	    while (index < menu_items_used)
	      {
		tem
		  = XVECTOR (menu_items)->contents[index + MENU_ITEMS_ITEM_NAME];
		if (NILP (tem))
		  {
		    index++;
		    submenu++;		/* Skip sub menu.  */
		  }
		else if (EQ (tem, Qlambda))
		  {
		    index++;
		    submenu--;		/* End sub menu.  */
		  }
		else if (EQ (tem, Qt))
		  index += 3;		/* Skip new pane marker. */
		else if (EQ (tem, Qquote))
		  index++;		/* Skip a left, right divider. */
		else
		  {
		    if (!submenu && SREF (tem, 0) != '\0'
			&& SREF (tem, 0) != '-')
		      XVECTOR (menu_items)->contents[index + MENU_ITEMS_ITEM_NAME]
			= concat2 (build_string ("    "), tem);
		    index += MENU_ITEMS_ITEM_LENGTH;
		  }
	      }
	    skp->notbuttons = 0;
	  }

	/* Calculate prefix, if any, for this item.  */
	if (EQ (type, QCtoggle))
	  prefix = build_string (NILP (selected) ? "[ ] " : "[X] ");
	else if (EQ (type, QCradio))
	  prefix = build_string (NILP (selected) ? "( ) " : "(*) ");
      }
    /* Not a button. If we have earlier buttons, then we need a prefix.  */
    else if (!skp->notbuttons && SREF (item_string, 0) != '\0'
	     && SREF (item_string, 0) != '-')
      prefix = build_string ("    ");

    if (!NILP (prefix))
      item_string = concat2 (prefix, item_string);
  }
#endif /* not HAVE_BOXES */

#if ! defined (USE_X_TOOLKIT) && ! defined (USE_GTK)
  if (!NILP (map))
    /* Indicate visually that this is a submenu.  */
    item_string = concat2 (item_string, build_string (" >"));
#endif

  push_menu_item (item_string, enabled, key,
		  XVECTOR (item_properties)->contents[ITEM_PROPERTY_DEF],
		  XVECTOR (item_properties)->contents[ITEM_PROPERTY_KEYEQ],
		  XVECTOR (item_properties)->contents[ITEM_PROPERTY_TYPE],
		  XVECTOR (item_properties)->contents[ITEM_PROPERTY_SELECTED],
		  XVECTOR (item_properties)->contents[ITEM_PROPERTY_HELP]);

#if defined (USE_X_TOOLKIT) || defined (USE_GTK)
  /* Display a submenu using the toolkit.  */
  if (! (NILP (map) || NILP (enabled)))
    {
      push_submenu_start ();
      single_keymap_panes (map, Qnil, key, 0, skp->maxdepth - 1);
      push_submenu_end ();
    }
#endif
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

  for (tail = menu; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object elt, pane_name, pane_data;
      elt = XCAR (tail);
      pane_name = Fcar (elt);
      CHECK_STRING (pane_name);
      push_menu_pane (ENCODE_MENU_STRING (pane_name), Qnil);
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

  for (tail = pane; CONSP (tail); tail = XCDR (tail))
    {
      item = XCAR (tail);
      if (STRINGP (item))
	push_menu_item (ENCODE_MENU_STRING (item), Qnil, Qnil, Qt,
			Qnil, Qnil, Qnil, Qnil);
      else if (CONSP (item))
	{
	  item1 = XCAR (item);
	  CHECK_STRING (item1);
	  push_menu_item (ENCODE_MENU_STRING (item1), Qt, XCDR (item),
			  Qt, Qnil, Qnil, Qnil, Qnil);
	}
      else
	push_left_right_boundary ();

    }
}

#ifdef HAVE_X_WINDOWS
/* Return the mouse position in *X and *Y.  The coordinates are window
   relative for the edit window in frame F.
   This is for Fx_popup_menu.  The mouse_position_hook can not
   be used for X, as it returns window relative coordinates
   for the window where the mouse is in.  This could be the menu bar,
   the scroll bar or the edit window.  Fx_popup_menu needs to be
   sure it is the edit window.  */
static void
mouse_position_for_popup (f, x, y)
     FRAME_PTR f;
     int *x;
     int *y;
{
  Window root, dummy_window;
  int dummy;

  if (! FRAME_X_P (f))
    abort ();

  BLOCK_INPUT;

  XQueryPointer (FRAME_X_DISPLAY (f),
                 DefaultRootWindow (FRAME_X_DISPLAY (f)),

                 /* The root window which contains the pointer.  */
                 &root,

                 /* Window pointer is on, not used  */
                 &dummy_window,

                 /* The position on that root window.  */
                 x, y,

                 /* x/y in dummy_window coordinates, not used.  */
                 &dummy, &dummy,

                 /* Modifier keys and pointer buttons, about which
                    we don't care.  */
                 (unsigned int *) &dummy);

  UNBLOCK_INPUT;

  /* xmenu_show expects window coordinates, not root window
     coordinates.  Translate.  */
  *x -= f->left_pos + FRAME_OUTER_TO_INNER_DIFF_X (f);
  *y -= f->top_pos + FRAME_OUTER_TO_INNER_DIFF_Y (f);
}

#endif /* HAVE_X_WINDOWS */

DEFUN ("x-popup-menu", Fx_popup_menu, Sx_popup_menu, 2, 2, 0,
       doc: /* Pop up a deck-of-cards menu and return user's selection.
POSITION is a position specification.  This is either a mouse button event
or a list ((XOFFSET YOFFSET) WINDOW)
where XOFFSET and YOFFSET are positions in pixels from the top left
corner of WINDOW.  (WINDOW may be a window or a frame object.)
This controls the position of the top left of the menu as a whole.
If POSITION is t, it means to use the current mouse position.

MENU is a specifier for a menu.  For the simplest case, MENU is a keymap.
The menu items come from key bindings that have a menu string as well as
a definition; actually, the "definition" in such a key binding looks like
\(STRING . REAL-DEFINITION).  To give the menu a title, put a string into
the keymap as a top-level element.

If REAL-DEFINITION is nil, that puts a nonselectable string in the menu.
Otherwise, REAL-DEFINITION should be a valid key binding definition.

You can also use a list of keymaps as MENU.
  Then each keymap makes a separate pane.

When MENU is a keymap or a list of keymaps, the return value is the
list of events corresponding to the user's choice. Note that
`x-popup-menu' does not actually execute the command bound to that
sequence of events.

Alternatively, you can specify a menu of multiple panes
  with a list of the form (TITLE PANE1 PANE2...),
where each pane is a list of form (TITLE ITEM1 ITEM2...).
Each ITEM is normally a cons cell (STRING . VALUE);
but a string can appear as an item--that makes a nonselectable line
in the menu.
With this form of menu, the return value is VALUE from the chosen item.

If POSITION is nil, don't display the menu at all, just precalculate the
cached information about equivalent key sequences.

If the user gets rid of the menu without making a valid choice, for
instance by clicking the mouse away from a valid choice or by typing
keyboard input, then this normally results in a quit and
`x-popup-menu' does not return.  But if POSITION is a mouse button
event (indicating that the user invoked the menu with the mouse) then
no quit occurs and `x-popup-menu' returns nil.  */)
     (position, menu)
     Lisp_Object position, menu;
{
  Lisp_Object keymap, tem;
  int xpos = 0, ypos = 0;
  Lisp_Object title;
  char *error_name = NULL;
  Lisp_Object selection = Qnil;
  FRAME_PTR f = NULL;
  Lisp_Object x, y, window;
  int keymaps = 0;
  int for_click = 0;
  int specpdl_count = SPECPDL_INDEX ();
  struct gcpro gcpro1;

#ifdef HAVE_MENUS
  if (! NILP (position))
    {
      int get_current_pos_p = 0;
      check_x ();

      /* Decode the first argument: find the window and the coordinates.  */
      if (EQ (position, Qt)
	  || (CONSP (position) && (EQ (XCAR (position), Qmenu_bar)
				   || EQ (XCAR (position), Qtool_bar))))
	{
          get_current_pos_p = 1;
        }
      else
	{
	  tem = Fcar (position);
	  if (CONSP (tem))
	    {
	      window = Fcar (Fcdr (position));
	      x = XCAR (tem);
	      y = Fcar (XCDR (tem));
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

          /* If a click happens in an external tool bar or a detached
             tool bar, x and y is NIL.  In that case, use the current
             mouse position.  This happens for the help button in the
             tool bar.  Ideally popup-menu should pass NIL to
             this function, but it doesn't.  */
          if (NILP (x) && NILP (y))
            get_current_pos_p = 1;
	}

      if (get_current_pos_p)
        {
	  /* Use the mouse's current position.  */
	  FRAME_PTR new_f = SELECTED_FRAME ();
#ifdef HAVE_X_WINDOWS
          /* Can't use mouse_position_hook for X since it returns
             coordinates relative to the window the mouse is in,
             we need coordinates relative to the edit widget always.  */
          if (new_f != 0)
            {
              int cur_x, cur_y;

              mouse_position_for_popup (new_f, &cur_x, &cur_y);
              /* cur_x/y may be negative, so use make_number.  */
              x = make_number (cur_x);
              y = make_number (cur_y);
            }

#else /* not HAVE_X_WINDOWS */
	  Lisp_Object bar_window;
	  enum scroll_bar_part part;
	  unsigned long time;

	  if (mouse_position_hook)
	    (*mouse_position_hook) (&new_f, 1, &bar_window,
				    &part, &x, &y, &time);
#endif /* not HAVE_X_WINDOWS */

	  if (new_f != 0)
	    XSETFRAME (window, new_f);
	  else
	    {
	      window = selected_window;
	      XSETFASTINT (x, 0);
	      XSETFASTINT (y, 0);
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

      if (! FRAME_X_P (f))
        error ("Can not put X menu on non-X terminal");

      XSETFRAME (Vmenu_updating_frame, f);
    }
  else
    Vmenu_updating_frame = Qnil;
#endif /* HAVE_MENUS */

  record_unwind_protect (unuse_menu_items, Qnil);
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
	XVECTOR (menu_items)->contents[MENU_ITEMS_PANE_NAME] = prompt;

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
      for (tem = menu, i = 0; CONSP (tem); tem = XCDR (tem))
	{
	  Lisp_Object prompt;

	  maps[i++] = keymap = get_keymap (XCAR (tem), 1, 0);

	  prompt = Fkeymap_prompt (keymap);
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
      CHECK_STRING (title);

      list_of_panes (Fcdr (menu));

      keymaps = 0;
    }

  unbind_to (specpdl_count, Qnil);

  if (NILP (position))
    {
      discard_menu_items ();
      UNGCPRO;
      return Qnil;
    }

#ifdef HAVE_MENUS
  /* Display them in a menu.  */
  BLOCK_INPUT;

  selection = xmenu_show (f, xpos, ypos, for_click,
			  keymaps, title, &error_name);
  UNBLOCK_INPUT;

  discard_menu_items ();

  UNGCPRO;
#endif /* HAVE_MENUS */

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
It is a list of the form (DIALOG ITEM1 ITEM2...).
Each ITEM is a cons cell (STRING . VALUE).
The return value is VALUE from the chosen item.

An ITEM may also be just a string--that makes a nonselectable item.
An ITEM may also be nil--that means to put all preceding items
on the left of the dialog box and all following items on the right.
\(By default, approximately half appear on each side.)

If HEADER is non-nil, the frame title for the box is "Information",
otherwise it is "Question".

If the user gets rid of the dialog box without making a valid choice,
for instance using the window manager, then this produces a quit and
`x-popup-dialog' does not return.  */)
     (position, contents, header)
     Lisp_Object position, contents, header;
{
  FRAME_PTR f = NULL;
  Lisp_Object window;

  check_x ();

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

  if (! FRAME_X_P (f))
    error ("Can not put X dialog on non-X terminal");

#if ! defined (USE_X_TOOLKIT) && ! defined (USE_GTK)
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
    int specpdl_count = SPECPDL_INDEX ();

    /* Decode the dialog items from what was specified.  */
    title = Fcar (contents);
    CHECK_STRING (title);
    record_unwind_protect (unuse_menu_items, Qnil);

    if (NILP (Fcar (Fcdr (contents))))
      /* No buttons specified, add an "Ok" button so users can pop down
         the dialog.  Also, the lesstif/motif version crashes if there are
         no buttons.  */
      contents = Fcons (title, Fcons (Fcons (build_string ("Ok"), Qt), Qnil));

    list_of_panes (Fcons (contents, Qnil));

    /* Display them in a dialog box.  */
    BLOCK_INPUT;
    selection = xdialog_show (f, 0, title, header, &error_name);
    UNBLOCK_INPUT;

    unbind_to (specpdl_count, Qnil);
    discard_menu_items ();

    if (error_name) error (error_name);
    return selection;
  }
#endif
}


#ifndef MSDOS

/* Set menu_items_inuse so no other popup menu or dialog is created.  */

void
x_menu_set_in_use (in_use)
     int in_use;
{
  menu_items_inuse = in_use ? Qt : Qnil;
  popup_activated_flag = in_use;
#ifdef USE_X_TOOLKIT
  if (popup_activated_flag)
    x_activate_timeout_atimer ();
#endif
}

/* Wait for an X event to arrive or for a timer to expire.  */

void
x_menu_wait_for_event (void *data)
{
  extern EMACS_TIME timer_check P_ ((int));

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
         ! XPending ((Display*) data)
#endif
         )
    {
      EMACS_TIME next_time = timer_check (1);
      long secs = EMACS_SECS (next_time);
      long usecs = EMACS_USECS (next_time);
      SELECT_TYPE read_fds;
      struct x_display_info *dpyinfo;
      int n = 0;

      FD_ZERO (&read_fds);
      for (dpyinfo = x_display_list; dpyinfo; dpyinfo = dpyinfo->next)
        {
          int fd = ConnectionNumber (dpyinfo->display);
          FD_SET (fd, &read_fds);
          if (fd > n) n = fd;
        }

      if (secs < 0 || (secs == 0 && usecs == 0))
        {
          /* Sometimes timer_check returns -1 (no timers) even if there are
             timers.  So do a timeout anyway.  */
          EMACS_SET_SECS (next_time, 1);
          EMACS_SET_USECS (next_time, 0);
        }

      select (n + 1, &read_fds, (SELECT_TYPE *)0, (SELECT_TYPE *)0, &next_time);
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
popup_get_selection (initial_event, dpyinfo, id, do_timers)
     XEvent *initial_event;
     struct x_display_info *dpyinfo;
     LWLIB_ID id;
     int do_timers;
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
       doc: /* Start key navigation of the menu bar in FRAME.
This initially opens the first menu bar item and you can then navigate with the
arrow keys, select a menu entry with the return key or cancel with the
escape key.  If FRAME has no menu bar this function does nothing.

If FRAME is nil or not given, use the selected frame.  */)
     (frame)
     Lisp_Object frame;
{
  XEvent ev;
  FRAME_PTR f = check_x_frame (frame);
  Widget menubar;
  BLOCK_INPUT;

  if (FRAME_EXTERNAL_MENU_BAR (f))
    set_frame_menubar (f, 0, 1);

  menubar = FRAME_X_OUTPUT (f)->menubar_widget;
  if (menubar)
    {
      Window child;
      int error_p = 0;

      x_catch_errors (FRAME_X_DISPLAY (f));
      memset (&ev, 0, sizeof ev);
      ev.xbutton.display = FRAME_X_DISPLAY (f);
      ev.xbutton.window = XtWindow (menubar);
      ev.xbutton.root = FRAME_X_DISPLAY_INFO (f)->root_window;
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
      x_uncatch_errors ();

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

  UNBLOCK_INPUT;

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
     (frame)
     Lisp_Object frame;
{
  GtkWidget *menubar;
  FRAME_PTR f;

  /* gcc 2.95 doesn't accept the FRAME_PTR declaration after
     BLOCK_INPUT.  */

  BLOCK_INPUT;
  f = check_x_frame (frame);

  if (FRAME_EXTERNAL_MENU_BAR (f))
    set_frame_menubar (f, 0, 1);

  menubar = FRAME_X_OUTPUT (f)->menubar_widget;
  if (menubar)
    {
      /* Activate the first menu.  */
      GList *children = gtk_container_get_children (GTK_CONTAINER (menubar));

      gtk_menu_shell_select_item (GTK_MENU_SHELL (menubar),
                                  GTK_WIDGET (children->data));

      popup_activated_flag = 1;
      g_list_free (children);
    }
  UNBLOCK_INPUT;

  return Qnil;
}

/* Loop util popup_activated_flag is set to zero in a callback.
   Used for popup menus and dialogs. */

static void
popup_widget_loop (do_timers, widget)
     int do_timers;
     GtkWidget *widget;
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
x_activate_menubar (f)
     FRAME_PTR f;
{
  if (! FRAME_X_P (f))
    abort ();

  if (!f->output_data.x->saved_menu_event->type)
    return;

#ifdef USE_GTK
  if (! xg_win_to_widget (FRAME_X_DISPLAY (f),
                          f->output_data.x->saved_menu_event->xany.window))
    return;
#endif

  set_frame_menubar (f, 0, 1);
  BLOCK_INPUT;
#ifdef USE_GTK
  XPutBackEvent (f->output_data.x->display_info->display,
                 f->output_data.x->saved_menu_event);
  popup_activated_flag = 1;
#else
  XtDispatchEvent (f->output_data.x->saved_menu_event);
#endif
  UNBLOCK_INPUT;
#ifdef USE_MOTIF
  if (f->output_data.x->saved_menu_event->type == ButtonRelease)
    pending_menu_activation = 1;
#endif

  /* Ignore this if we get it a second time.  */
  f->output_data.x->saved_menu_event->type = 0;
}

/* This callback is invoked when the user selects a menubar cascade
   pushbutton, but before the pulldown menu is posted.  */

#ifndef USE_GTK
static void
popup_activate_callback (widget, id, client_data)
     Widget widget;
     LWLIB_ID id;
     XtPointer client_data;
{
  popup_activated_flag = 1;
#ifdef USE_X_TOOLKIT
  x_activate_timeout_atimer ();
#endif
}
#endif

/* This callback is invoked when a dialog or menu is finished being
   used and has been unposted.  */

#ifdef USE_GTK
static void
popup_deactivate_callback (widget, client_data)
     GtkWidget *widget;
     gpointer client_data;
{
  popup_activated_flag = 0;
}
#else
static void
popup_deactivate_callback (widget, id, client_data)
     Widget widget;
     LWLIB_ID id;
     XtPointer client_data;
{
  popup_activated_flag = 0;
}
#endif


/* Function that finds the frame for WIDGET and shows the HELP text
   for that widget.
   F is the frame if known, or NULL if not known.  */
static void
show_help_event (f, widget, help)
     FRAME_PTR f;
     xt_or_gtk_widget widget;
     Lisp_Object help;
{
  Lisp_Object frame;

  if (f)
    {
      XSETFRAME (frame, f);
      kbd_buffer_store_help_event (frame, help);
    }
  else
    {
#if 0  /* This code doesn't do anything useful.  ++kfs */
      /* WIDGET is the popup menu.  It's parent is the frame's
	 widget.  See which frame that is.  */
      xt_or_gtk_widget frame_widget = XtParent (widget);
      Lisp_Object tail;

      for (tail = Vframe_list; CONSP (tail); tail = XCDR (tail))
	{
	  frame = XCAR (tail);
	  if (FRAMEP (frame)
	      && (f = XFRAME (frame),
		  FRAME_X_P (f) && f->output_data.x->widget == frame_widget))
	    break;
	}
#endif
      show_help_echo (help, Qnil, Qnil, Qnil, 1);
    }
}

/* Callback called when menu items are highlighted/unhighlighted
   while moving the mouse over them.  WIDGET is the menu bar or menu
   popup widget.  ID is its LWLIB_ID.  CALL_DATA contains a pointer to
   the data structure for the menu item, or null in case of
   unhighlighting.  */

#ifdef USE_GTK
void
menu_highlight_callback (widget, call_data)
     GtkWidget *widget;
     gpointer call_data;
{
  xg_menu_item_cb_data *cb_data;
  Lisp_Object help;

  cb_data = (xg_menu_item_cb_data*) g_object_get_data (G_OBJECT (widget),
                                                       XG_ITEM_DATA);
  if (! cb_data) return;

  help = call_data ? cb_data->help : Qnil;

  /* If popup_activated_flag is greater than 1 we are in a popup menu.
     Don't show help for them, they won't appear before the
     popup is popped down.  */
  if (popup_activated_flag <= 1)
    show_help_event (cb_data->cl_data->f, widget, help);
}
#else
void
menu_highlight_callback (widget, id, call_data)
     Widget widget;
     LWLIB_ID id;
     void *call_data;
{
  struct frame *f;
  Lisp_Object help;

  widget_value *wv = (widget_value *) call_data;

  help = wv ? wv->help : Qnil;

  /* Determine the frame for the help event.  */
  f = menubar_id_to_frame (id);

  show_help_event (f, widget, help);
}
#endif

/* Find the menu selection and store it in the keyboard buffer.
   F is the frame the menu is on.
   MENU_BAR_ITEMS_USED is the length of VECTOR.
   VECTOR is an array of menu events for the whole menu.  */

static void
find_and_call_menu_selection (f, menu_bar_items_used, vector, client_data)
     FRAME_PTR f;
     EMACS_INT menu_bar_items_used;
     Lisp_Object vector;
     void *client_data;
{
  Lisp_Object prefix, entry;
  Lisp_Object *subprefix_stack;
  int submenu_depth = 0;
  int i;

  entry = Qnil;
  subprefix_stack = (Lisp_Object *) alloca (menu_bar_items_used * sizeof (Lisp_Object));
  prefix = Qnil;
  i = 0;

  while (i < menu_bar_items_used)
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
	      kbd_buffer_store_event (&buf);

	      return;
	    }
	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }
}


#ifdef USE_GTK
/* Gtk calls callbacks just because we tell it what item should be
   selected in a radio group.  If this variable is set to a non-zero
   value, we are creating menus and don't want callbacks right now.
*/
static int xg_crazy_callback_abort;

/* This callback is called from the menu bar pulldown menu
   when the user makes a selection.
   Figure out what the user chose
   and put the appropriate events into the keyboard buffer.  */
static void
menubar_selection_callback (widget, client_data)
     GtkWidget *widget;
     gpointer client_data;
{
  xg_menu_item_cb_data *cb_data = (xg_menu_item_cb_data*) client_data;

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

  BLOCK_INPUT;
  while (gtk_events_pending ())
    gtk_main_iteration ();
  UNBLOCK_INPUT;

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
menubar_selection_callback (widget, id, client_data)
     Widget widget;
     LWLIB_ID id;
     XtPointer client_data;
{
  FRAME_PTR f;

  f = menubar_id_to_frame (id);
  if (!f)
    return;
  find_and_call_menu_selection (f, f->menu_bar_items_used,
                                f->menu_bar_vector, client_data);
}
#endif /* not USE_GTK */

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

/* Set up data in menu_items for a menu bar item
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
      if (!KEYMAPP (mapvec[i]))
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
  int panes_seen = 0;

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

	  panes_seen++;

	  pane_name = XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_NAME];
	  prefix = XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_PREFIX];

#ifndef HAVE_MULTILINGUAL_MENU
	  if (STRINGP (pane_name) && STRING_MULTIBYTE (pane_name))
	    {
	      pane_name = ENCODE_MENU_STRING (pane_name);
	      AREF (menu_items, i + MENU_ITEMS_PANE_NAME) = pane_name;
	    }
#endif
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
	      wv->value = (char *)1;
	      wv->enabled = 1;
	      wv->button_type = BUTTON_TYPE_NONE;
	      wv->help = Qnil;
	      save_wv = wv;
	    }
	  else
	    save_wv = first_wv;

	  prev_wv = 0;
	  i += MENU_ITEMS_PANE_LENGTH;
	}
      else
	{
	  /* Create a new item within current pane.  */
	  Lisp_Object item_name, enable, descrip, def, type, selected;
	  Lisp_Object help;

	  /* All items should be contained in panes.  */
	  if (panes_seen == 0)
	    abort ();

	  item_name = AREF (menu_items, i + MENU_ITEMS_ITEM_NAME);
	  enable = AREF (menu_items, i + MENU_ITEMS_ITEM_ENABLE);
	  descrip = AREF (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY);
	  def = AREF (menu_items, i + MENU_ITEMS_ITEM_DEFINITION);
	  type = AREF (menu_items, i + MENU_ITEMS_ITEM_TYPE);
	  selected = AREF (menu_items, i + MENU_ITEMS_ITEM_SELECTED);
	  help = AREF (menu_items, i + MENU_ITEMS_ITEM_HELP);

#ifndef HAVE_MULTILINGUAL_MENU
          if (STRING_MULTIBYTE (item_name))
	    {
	      item_name = ENCODE_MENU_STRING (item_name);
	      AREF (menu_items, i + MENU_ITEMS_ITEM_NAME) = item_name;
	    }

          if (STRINGP (descrip) && STRING_MULTIBYTE (descrip))
	    {
	      descrip = ENCODE_MENU_STRING (descrip);
	      AREF (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY) = descrip;
	    }
#endif /* not HAVE_MULTILINGUAL_MENU */

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
	  if (! STRINGP (help))
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
      if (STRINGP (wv->lname))
        {
          wv->name = (char *) SDATA (wv->lname);

          /* Ignore the @ that means "separate pane".
             This is a kludge, but this isn't worth more time.  */
          if (wv->value == (char *)1)
            {
              if (wv->name[0] == '@')
		wv->name++;
              wv->value = 0;
            }
        }

      if (STRINGP (wv->lkey))
        wv->key = (char *) SDATA (wv->lkey);

      if (wv->contents)
        update_submenu_strings (wv->contents);
    }
}


/* Recompute all the widgets of frame F, when the menu bar has been
   changed.  Value is non-zero if widgets were updated.  */

static int
update_frame_menubar (f)
     FRAME_PTR f;
{
#ifdef USE_GTK
  return xg_update_frame_menubar (f);
#else
  struct x_output *x;
  int columns, rows;

  if (! FRAME_X_P (f))
    abort ();

  x = f->output_data.x;

  if (!x->menubar_widget || XtIsManaged (x->menubar_widget))
    return 0;

  BLOCK_INPUT;
  /* Save the size of the frame because the pane widget doesn't accept
     to resize itself. So force it.  */
  columns = FRAME_COLS (f);
  rows = FRAME_LINES (f);

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

  /* Force the pane widget to resize itself with the right values.  */
  EmacsFrameSetCharSize (x->edit_widget, columns, rows);
  UNBLOCK_INPUT;
#endif
  return 1;
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
  xt_or_gtk_widget menubar_widget;
#ifdef USE_X_TOOLKIT
  LWLIB_ID id;
#endif
  Lisp_Object items;
  widget_value *wv, *first_wv, *prev_wv = 0;
  int i, last_i = 0;
  int *submenu_start, *submenu_end;
  int *submenu_top_level_items, *submenu_n_panes;

  if (! FRAME_X_P (f))
    abort ();

  menubar_widget = f->output_data.x->menubar_widget;

  XSETFRAME (Vmenu_updating_frame, f);

#ifdef USE_X_TOOLKIT
  if (f->output_data.x->id == 0)
    f->output_data.x->id = next_menubar_widget_id++;
  id = f->output_data.x->id;
#endif

  if (! menubar_widget)
    deep_p = 1;
  else if (pending_menu_activation && !deep_p)
    deep_p = 1;
  /* Make the first call for any given frame always go deep.  */
  else if (!f->output_data.x->saved_menu_event && !deep_p)
    {
      deep_p = 1;
      f->output_data.x->saved_menu_event = (XEvent*)xmalloc (sizeof (XEvent));
      f->output_data.x->saved_menu_event->type = 0;
    }

#ifdef USE_GTK
  /* If we have detached menus, we must update deep so detached menus
     also gets updated.  */
  deep_p = deep_p || xg_have_tear_offs ();
#endif

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
      save_menu_items ();

      menu_items = f->menu_bar_vector;
      menu_items_allocated = VECTORP (menu_items) ? ASIZE (menu_items) : 0;
      submenu_start = (int *) alloca (XVECTOR (items)->size * sizeof (int *));
      submenu_end = (int *) alloca (XVECTOR (items)->size * sizeof (int *));
      submenu_n_panes = (int *) alloca (XVECTOR (items)->size * sizeof (int));
      submenu_top_level_items
	= (int *) alloca (XVECTOR (items)->size * sizeof (int *));
      init_menu_items ();
      for (i = 0; i < XVECTOR (items)->size; i += 4)
	{
	  Lisp_Object key, string, maps;

	  last_i = i;

	  key = XVECTOR (items)->contents[i];
	  string = XVECTOR (items)->contents[i + 1];
	  maps = XVECTOR (items)->contents[i + 2];
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

      /* If there has been no change in the Lisp-level contents
	 of the menu bar, skip redisplaying it.  Just exit.  */

      /* Compare the new menu items with the ones computed last time.  */
      for (i = 0; i < previous_menu_items_used; i++)
	if (menu_items_used == i
	    || (!EQ (previous_items[i], XVECTOR (menu_items)->contents[i])))
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
      f->menu_bar_vector = menu_items;
      f->menu_bar_items_used = menu_items_used;

      /* This calls restore_menu_items to restore menu_items, etc.,
	 as they were outside.  */
      unbind_to (specpdl_count, Qnil);

      /* Now GC cannot happen during the lifetime of the widget_value,
	 so it's safe to store data from a Lisp_String.  */
      wv = first_wv->contents;
      for (i = 0; i < XVECTOR (items)->size; i += 4)
	{
	  Lisp_Object string;
	  string = XVECTOR (items)->contents[i + 1];
	  if (NILP (string))
            break;
          wv->name = (char *) SDATA (string);
          update_submenu_strings (wv->contents);
          wv = wv->next;
	}

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
      for (i = 0; i < XVECTOR (items)->size; i += 4)
	{
	  Lisp_Object string;

	  string = XVECTOR (items)->contents[i + 1];
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

#ifdef USE_GTK
  xg_crazy_callback_abort = 1;
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
      GtkWidget *wvbox = f->output_data.x->vbox_widget;

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

      menubar_widget = lw_create_widget ("menubar", "menubar", id, first_wv,
					 f->output_data.x->column_widget,
					 0,
					 popup_activate_callback,
					 menubar_selection_callback,
					 popup_deactivate_callback,
					 menu_highlight_callback);
      f->output_data.x->menubar_widget = menubar_widget;

      /* Make menu pop down on C-g.  */
      XtOverrideTranslations (menubar_widget, override);
    }

  {
    int menubar_size
      = (f->output_data.x->menubar_widget
	 ? (f->output_data.x->menubar_widget->core.height
	    + f->output_data.x->menubar_widget->core.border_width)
	 : 0);

#if 0 /* Experimentally, we now get the right results
	 for -geometry -0-0 without this.  24 Aug 96, rms.  */
#ifdef USE_LUCID
    if (FRAME_EXTERNAL_MENU_BAR (f))
      {
        Dimension ibw = 0;
        XtVaGetValues (f->output_data.x->column_widget,
		       XtNinternalBorderWidth, &ibw, NULL);
        menubar_size += ibw;
      }
#endif /* USE_LUCID */
#endif /* 0 */

    f->output_data.x->menubar_height = menubar_size;
  }
#endif /* not USE_GTK */

  free_menubar_widget_value_tree (first_wv);
  update_frame_menubar (f);

#ifdef USE_GTK
  xg_crazy_callback_abort = 0;
#endif

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
   This is used when deleting a frame, and when turning off the menu bar.
   For GTK this function is in gtkutil.c.  */

#ifndef USE_GTK
void
free_frame_menubar (f)
     FRAME_PTR f;
{
  Widget menubar_widget;

  if (! FRAME_X_P (f))
    abort ();

  menubar_widget = f->output_data.x->menubar_widget;

  f->output_data.x->menubar_height = 0;

  if (menubar_widget)
    {
#ifdef USE_MOTIF
      /* Removing the menu bar magically changes the shell widget's x
	 and y position of (0, 0) which, when the menu bar is turned
	 on again, leads to pull-down menuss appearing in strange
	 positions near the upper-left corner of the display.  This
	 happens only with some window managers like twm and ctwm,
	 but not with other like Motif's mwm or kwm, because the
	 latter generate ConfigureNotify events when the menu bar
	 is switched off, which fixes the shell position.  */
      Position x0, y0, x1, y1;
#endif

      BLOCK_INPUT;

#ifdef USE_MOTIF
      if (f->output_data.x->widget)
	XtVaGetValues (f->output_data.x->widget, XtNx, &x0, XtNy, &y0, NULL);
#endif

      lw_destroy_all_widgets ((LWLIB_ID) f->output_data.x->id);
      f->output_data.x->menubar_widget = NULL;

#ifdef USE_MOTIF
      if (f->output_data.x->widget)
	{
	  XtVaGetValues (f->output_data.x->widget, XtNx, &x1, XtNy, &y1, NULL);
	  if (x1 == 0 && y1 == 0)
	    XtVaSetValues (f->output_data.x->widget, XtNx, x0, XtNy, y0, NULL);
	}
#endif

      UNBLOCK_INPUT;
    }
}
#endif /* not USE_GTK */

#endif /* USE_X_TOOLKIT || USE_GTK */

/* xmenu_show actually displays a menu using the panes and items in menu_items
   and returns the value selected from it.
   There are two versions of xmenu_show, one for Xt and one for Xlib.
   Both assume input is blocked by the caller.  */

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

#if defined (USE_X_TOOLKIT) || defined (USE_GTK)

/* The item selected in the popup menu.  */
static Lisp_Object *volatile menu_item_selection;

#ifdef USE_GTK

/* Used when position a popup menu.  See menu_position_func and
   create_and_show_popup_menu below.  */
struct next_popup_x_y
{
  FRAME_PTR f;
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
menu_position_func (menu, x, y, push_in, user_data)
     GtkMenu *menu;
     gint *x;
     gint *y;
     gboolean *push_in;
     gpointer user_data;
{
  struct next_popup_x_y* data = (struct next_popup_x_y*)user_data;
  GtkRequisition req;
  int disp_width = FRAME_X_DISPLAY_INFO (data->f)->width;
  int disp_height = FRAME_X_DISPLAY_INFO (data->f)->height;

  *x = data->x;
  *y = data->y;

  /* Check if there is room for the menu.  If not, adjust x/y so that
     the menu is fully visible.  */
  gtk_widget_size_request (GTK_WIDGET (menu), &req);
  if (data->x + req.width > disp_width)
    *x -= data->x + req.width - disp_width;
  if (data->y + req.height > disp_height)
    *y -= data->y + req.height - disp_height;
}

static void
popup_selection_callback (widget, client_data)
     GtkWidget *widget;
     gpointer client_data;
{
  xg_menu_item_cb_data *cb_data = (xg_menu_item_cb_data*) client_data;

  if (xg_crazy_callback_abort) return;
  if (cb_data) menu_item_selection = (Lisp_Object *) cb_data->call_data;
}

static Lisp_Object
pop_down_menu (arg)
     Lisp_Object arg;
{
  struct Lisp_Save_Value *p = XSAVE_VALUE (arg);

  popup_activated_flag = 0;
  BLOCK_INPUT;
  gtk_widget_destroy (GTK_WIDGET (p->pointer));
  UNBLOCK_INPUT;
  return Qnil;
}

/* Pop up the menu for frame F defined by FIRST_WV at X/Y and loop until the
   menu pops down.
   menu_item_selection will be set to the selection.  */
static void
create_and_show_popup_menu (f, first_wv, x, y, for_click)
     FRAME_PTR f;
     widget_value *first_wv;
     int x;
     int y;
     int for_click;
{
  int i;
  GtkWidget *menu;
  GtkMenuPositionFunc pos_func = 0;  /* Pop up at pointer.  */
  struct next_popup_x_y popup_x_y;
  int specpdl_count = SPECPDL_INDEX ();

  if (! FRAME_X_P (f))
    abort ();

  xg_crazy_callback_abort = 1;
  menu = xg_create_widget ("popup", first_wv->name, f, first_wv,
                           G_CALLBACK (popup_selection_callback),
                           G_CALLBACK (popup_deactivate_callback),
                           G_CALLBACK (menu_highlight_callback));
  xg_crazy_callback_abort = 0;

  if (! for_click)
    {
      /* Not invoked by a click.  pop up at x/y.  */
      pos_func = menu_position_func;

      /* Adjust coordinates to be root-window-relative.  */
      x += f->left_pos + FRAME_OUTER_TO_INNER_DIFF_X (f);
      y += f->top_pos + FRAME_OUTER_TO_INNER_DIFF_Y (f);

      popup_x_y.x = x;
      popup_x_y.y = y;
      popup_x_y.f = f;

      i = 0;  /* gtk_menu_popup needs this to be 0 for a non-button popup.  */
    }
  else
    {
      for (i = 0; i < 5; i++)
        if (FRAME_X_DISPLAY_INFO (f)->grabbed & (1 << i))
          break;
    }

  /* Display the menu.  */
  gtk_widget_show_all (menu);
  gtk_menu_popup (GTK_MENU (menu), 0, 0, pos_func, &popup_x_y, i, 0);

  record_unwind_protect (pop_down_menu, make_save_value (menu, 0));

  if (GTK_WIDGET_MAPPED (menu))
    {
      /* Set this to one.  popup_widget_loop increases it by one, so it becomes
         two.  show_help_echo uses this to detect popup menus.  */
      popup_activated_flag = 1;
      /* Process events that apply to the menu.  */
      popup_widget_loop (1, menu);
    }

  unbind_to (specpdl_count, Qnil);

  /* Must reset this manually because the button release event is not passed
     to Emacs event loop. */
  FRAME_X_DISPLAY_INFO (f)->grabbed = 0;
}

#else /* not USE_GTK */

/* We need a unique id for each widget handled by the Lucid Widget
   library.

   For the main windows, and popup menus, we use this counter,
   which we increment each time after use.  This starts from 1<<16.

   For menu bars, we use numbers starting at 0, counted in
   next_menubar_widget_id.  */
LWLIB_ID widget_id_tick;

static void
popup_selection_callback (widget, id, client_data)
     Widget widget;
     LWLIB_ID id;
     XtPointer client_data;
{
  menu_item_selection = (Lisp_Object *) client_data;
}

/* ARG is the LWLIB ID of the dialog box, represented
   as a Lisp object as (HIGHPART . LOWPART).  */

static Lisp_Object
pop_down_menu (arg)
     Lisp_Object arg;
{
  LWLIB_ID id = (XINT (XCAR (arg)) << 4 * sizeof (LWLIB_ID)
                 | XINT (XCDR (arg)));

  BLOCK_INPUT;
  lw_destroy_all_widgets (id);
  UNBLOCK_INPUT;
  popup_activated_flag = 0;

  return Qnil;
}

/* Pop up the menu for frame F defined by FIRST_WV at X/Y and loop until the
   menu pops down.
   menu_item_selection will be set to the selection.  */
static void
create_and_show_popup_menu (f, first_wv, x, y, for_click)
     FRAME_PTR f;
     widget_value *first_wv;
     int x;
     int y;
     int for_click;
{
  int i;
  Arg av[2];
  int ac = 0;
  XButtonPressedEvent dummy;
  LWLIB_ID menu_id;
  Widget menu;

  if (! FRAME_X_P (f))
    abort ();

  menu_id = widget_id_tick++;
  menu = lw_create_widget ("popup", first_wv->name, menu_id, first_wv,
                           f->output_data.x->widget, 1, 0,
                           popup_selection_callback,
                           popup_deactivate_callback,
                           menu_highlight_callback);

  dummy.type = ButtonPress;
  dummy.serial = 0;
  dummy.send_event = 0;
  dummy.display = FRAME_X_DISPLAY (f);
  dummy.time = CurrentTime;
  dummy.root = FRAME_X_DISPLAY_INFO (f)->root_window;
  dummy.window = dummy.root;
  dummy.subwindow = dummy.root;
  dummy.x = x;
  dummy.y = y;

  /* Adjust coordinates to be root-window-relative.  */
  x += f->left_pos + FRAME_OUTER_TO_INNER_DIFF_X (f);
  y += f->top_pos + FRAME_OUTER_TO_INNER_DIFF_Y (f);

  dummy.x_root = x;
  dummy.y_root = y;

  dummy.state = 0;
  dummy.button = 0;
  for (i = 0; i < 5; i++)
    if (FRAME_X_DISPLAY_INFO (f)->grabbed & (1 << i))
      dummy.button = i;

  /* Don't allow any geometry request from the user.  */
  XtSetArg (av[ac], XtNgeometry, 0); ac++;
  XtSetValues (menu, av, ac);

  /* Display the menu.  */
  lw_popup_menu (menu, (XEvent *) &dummy);
  popup_activated_flag = 1;
  x_activate_timeout_atimer ();

  {
    int fact = 4 * sizeof (LWLIB_ID);
    int specpdl_count = SPECPDL_INDEX ();
    record_unwind_protect (pop_down_menu,
                           Fcons (make_number (menu_id >> (fact)),
                                  make_number (menu_id & ~(-1 << (fact)))));

    /* Process events that apply to the menu.  */
    popup_get_selection ((XEvent *) 0, FRAME_X_DISPLAY_INFO (f), menu_id, 1);

    unbind_to (specpdl_count, Qnil);
  }
}

#endif /* not USE_GTK */

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
  widget_value *wv, *save_wv = 0, *first_wv = 0, *prev_wv = 0;
  widget_value **submenu_stack
    = (widget_value **) alloca (menu_items_used * sizeof (widget_value *));
  Lisp_Object *subprefix_stack
    = (Lisp_Object *) alloca (menu_items_used * sizeof (Lisp_Object));
  int submenu_depth = 0;

  int first_pane;

  if (! FRAME_X_P (f))
    abort ();

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
  wv->help =Qnil;
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

	  pane_name = AREF (menu_items, i + MENU_ITEMS_PANE_NAME);
	  prefix = AREF (menu_items, i + MENU_ITEMS_PANE_PREFIX);

#ifndef HAVE_MULTILINGUAL_MENU
	  if (STRINGP (pane_name) && STRING_MULTIBYTE (pane_name))
	    {
	      pane_name = ENCODE_MENU_STRING (pane_name);
	      AREF (menu_items, i + MENU_ITEMS_PANE_NAME) = pane_name;
	    }
#endif
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

#ifndef HAVE_MULTILINGUAL_MENU
          if (STRINGP (item_name) && STRING_MULTIBYTE (item_name))
	    {
	      item_name = ENCODE_MENU_STRING (item_name);
	      AREF (menu_items, i + MENU_ITEMS_ITEM_NAME) = item_name;
	    }

          if (STRINGP (descrip) && STRING_MULTIBYTE (descrip))
	    {
	      descrip = ENCODE_MENU_STRING (descrip);
	      AREF (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY) = descrip;
	    }
#endif /* not HAVE_MULTILINGUAL_MENU */

	  wv = xmalloc_widget_value ();
	  if (prev_wv)
	    prev_wv->next = wv;
	  else
	    save_wv->contents = wv;
	  wv->name = (char *) SDATA (item_name);
	  if (!NILP (descrip))
	    wv->key = (char *) SDATA (descrip);
	  wv->value = 0;
	  /* If this item has a null value,
	     make the call_data null so that it won't display a box
	     when the mouse is on it.  */
	  wv->call_data
	    = (!NILP (def) ? (void *) &XVECTOR (menu_items)->contents[i] : 0);
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

          if (! STRINGP (help))
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
      widget_value *wv_sep1 = xmalloc_widget_value ();
      widget_value *wv_sep2 = xmalloc_widget_value ();

      wv_sep2->name = "--";
      wv_sep2->next = first_wv->contents;
      wv_sep2->help = Qnil;

      wv_sep1->name = "--";
      wv_sep1->next = wv_sep2;
      wv_sep1->help = Qnil;

#ifndef HAVE_MULTILINGUAL_MENU
      if (STRING_MULTIBYTE (title))
	title = ENCODE_MENU_STRING (title);
#endif

      wv_title->name = (char *) SDATA (title);
      wv_title->enabled = TRUE;
      wv_title->button_type = BUTTON_TYPE_NONE;
      wv_title->next = wv_sep1;
      wv_title->help = Qnil;
      first_wv->contents = wv_title;
    }

  /* No selection has been chosen yet.  */
  menu_item_selection = 0;

  /* Actually create and show the menu until popped down.  */
  create_and_show_popup_menu (f, first_wv, x, y, for_click);

  /* Free the widget_value objects we used to specify the contents.  */
  free_menubar_widget_value_tree (first_wv);

  /* Find the selected item, and its pane, to return
     the proper value.  */
  if (menu_item_selection != 0)
    {
      Lisp_Object prefix, entry;

      prefix = entry = Qnil;
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
	  /* Ignore a nil in the item list.
	     It's meaningful only for dialog boxes.  */
	  else if (EQ (XVECTOR (menu_items)->contents[i], Qquote))
	    i += 1;
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
  else if (!for_click)
    /* Make "Cancel" equivalent to C-g.  */
    Fsignal (Qquit, Qnil);

  return Qnil;
}

#ifdef USE_GTK
static void
dialog_selection_callback (widget, client_data)
     GtkWidget *widget;
     gpointer client_data;
{
  /* The EMACS_INT cast avoids a warning.  There's no problem
     as long as pointers have enough bits to hold small integers.  */
  if ((int) (EMACS_INT) client_data != -1)
    menu_item_selection = (Lisp_Object *) client_data;

  popup_activated_flag = 0;
}

/* Pop up the dialog for frame F defined by FIRST_WV and loop until the
   dialog pops down.
   menu_item_selection will be set to the selection.  */
static void
create_and_show_dialog (f, first_wv)
     FRAME_PTR f;
     widget_value *first_wv;
{
  GtkWidget *menu;

  if (! FRAME_X_P (f))
    abort ();

  menu = xg_create_widget ("dialog", first_wv->name, f, first_wv,
                           G_CALLBACK (dialog_selection_callback),
                           G_CALLBACK (popup_deactivate_callback),
                           0);

  if (menu)
    {
      int specpdl_count = SPECPDL_INDEX ();
      record_unwind_protect (pop_down_menu, make_save_value (menu, 0));

      /* Display the menu.  */
      gtk_widget_show_all (menu);

      /* Process events that apply to the menu.  */
      popup_widget_loop (1, menu);

      unbind_to (specpdl_count, Qnil);
    }
}

#else /* not USE_GTK */
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


/* Pop up the dialog for frame F defined by FIRST_WV and loop until the
   dialog pops down.
   menu_item_selection will be set to the selection.  */
static void
create_and_show_dialog (f, first_wv)
     FRAME_PTR f;
     widget_value *first_wv;
{
  LWLIB_ID dialog_id;

  if (!FRAME_X_P (f))
    abort();

  dialog_id = widget_id_tick++;
  lw_create_widget (first_wv->name, "dialog", dialog_id, first_wv,
                    f->output_data.x->widget, 1, 0,
                    dialog_selection_callback, 0, 0);
  lw_modify_all_widgets (dialog_id, first_wv->contents, True);

  /* Display the dialog box.  */
  lw_pop_up_all_widgets (dialog_id);
  popup_activated_flag = 1;
  x_activate_timeout_atimer ();

  /* Process events that apply to the dialog box.
     Also handle timers.  */
  {
    int count = SPECPDL_INDEX ();
    int fact = 4 * sizeof (LWLIB_ID);

    /* xdialog_show_unwind is responsible for popping the dialog box down.  */
    record_unwind_protect (pop_down_menu,
                           Fcons (make_number (dialog_id >> (fact)),
                                  make_number (dialog_id & ~(-1 << (fact)))));

    popup_get_selection ((XEvent *) 0, FRAME_X_DISPLAY_INFO (f),
                         dialog_id, 1);

    unbind_to (count, Qnil);
  }
}

#endif /* not USE_GTK */

static char * button_names [] = {
  "button1", "button2", "button3", "button4", "button5",
  "button6", "button7", "button8", "button9", "button10" };

static Lisp_Object
xdialog_show (f, keymaps, title, header, error_name)
     FRAME_PTR f;
     int keymaps;
     Lisp_Object title, header;
     char **error_name;
{
  int i, nb_buttons=0;
  char dialog_name[6];

  widget_value *wv, *first_wv = 0, *prev_wv = 0;

  /* Number of elements seen so far, before boundary.  */
  int left_count = 0;
  /* 1 means we've seen the boundary between left-hand elts and right-hand.  */
  int boundary_seen = 0;

  if (! FRAME_X_P (f))
    abort ();

  *error_name = NULL;

  if (menu_items_n_panes > 1)
    {
      *error_name = "Multiple panes in dialog box";
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
	Lisp_Object item_name, enable, descrip;
	item_name = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_NAME];
	enable = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_ENABLE];
	descrip
	  = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_EQUIV_KEY];

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
	    boundary_seen = 1;
	    i++;
	    continue;
	  }
	if (nb_buttons >= 9)
	  {
	    free_menubar_widget_value_tree (first_wv);
	    *error_name = "Too many dialog items";
	    return Qnil;
	  }

	wv = xmalloc_widget_value ();
	prev_wv->next = wv;
	wv->name = (char *) button_names[nb_buttons];
	if (!NILP (descrip))
	  wv->key = (char *) SDATA (descrip);
	wv->value = (char *) SDATA (item_name);
	wv->call_data = (void *) &XVECTOR (menu_items)->contents[i];
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

  /* No selection has been chosen yet.  */
  menu_item_selection = 0;

  /* Force a redisplay before showing the dialog.  If a frame is created
     just before showing the dialog, its contents may not have been fully
     drawn, as this depends on timing of events from the X server.  Redisplay
     is not done when a dialog is shown.  If redisplay could be done in the
     X event loop (i.e. the X event loop does not run in a signal handler)
     this would not be needed.  */
  Fredisplay (Qt);

  /* Actually create and show the dialog.  */
  create_and_show_dialog (f, first_wv);

  /* Free the widget_value objects we used to specify the contents.  */
  free_menubar_widget_value_tree (first_wv);

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
	  else if (EQ (XVECTOR (menu_items)->contents[i], Qquote))
	    {
	      /* This is the boundary between left-side elts and
		 right-side elts.  */
	      ++i;
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
  else
    /* Make "Cancel" equivalent to C-g.  */
    Fsignal (Qquit, Qnil);

  return Qnil;
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
menu_help_callback (help_string, pane, item)
     char *help_string;
     int pane, item;
{
  extern Lisp_Object Qmenu_item;
  Lisp_Object *first_item;
  Lisp_Object pane_name;
  Lisp_Object menu_object;

  first_item = XVECTOR (menu_items)->contents;
  if (EQ (first_item[0], Qt))
    pane_name = first_item[MENU_ITEMS_PANE_NAME];
  else if (EQ (first_item[0], Qquote))
    /* This shouldn't happen, see xmenu_show.  */
    pane_name = empty_unibyte_string;
  else
    pane_name = first_item[MENU_ITEMS_ITEM_NAME];

  /* (menu-item MENU-NAME PANE-NUMBER)  */
  menu_object = Fcons (Qmenu_item,
 		       Fcons (pane_name,
 			      Fcons (make_number (pane), Qnil)));
  show_help_echo (help_string ? build_string (help_string) : Qnil,
 		  Qnil, menu_object, make_number (item), 1);
}

static Lisp_Object
pop_down_menu (arg)
     Lisp_Object arg;
{
  struct Lisp_Save_Value *p1 = XSAVE_VALUE (Fcar (arg));
  struct Lisp_Save_Value *p2 = XSAVE_VALUE (Fcdr (arg));

  FRAME_PTR f = p1->pointer;
  XMenu *menu = p2->pointer;

  BLOCK_INPUT;
#ifndef MSDOS
  XUngrabPointer (FRAME_X_DISPLAY (f), CurrentTime);
  XUngrabKeyboard (FRAME_X_DISPLAY (f), CurrentTime);
#endif
  XMenuDestroy (FRAME_X_DISPLAY (f), menu);

#ifdef HAVE_X_WINDOWS
  /* Assume the mouse has moved out of the X window.
     If it has actually moved in, we will get an EnterNotify.  */
  x_mouse_leave (FRAME_X_DISPLAY_INFO (f));

  /* State that no mouse buttons are now held.
     (The oldXMenu code doesn't track this info for us.)
     That is not necessarily true, but the fiction leads to reasonable
     results, and it is a pain to ask which are actually held now.  */
  FRAME_X_DISPLAY_INFO (f)->grabbed = 0;

#endif /* HAVE_X_WINDOWS */

  UNBLOCK_INPUT;

  return Qnil;
}


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
  int i, j, lines, maxlines;
  int maxwidth;
  int dummy_int;
  unsigned int dummy_uint;
  int specpdl_count = SPECPDL_INDEX ();

  if (! FRAME_X_P (f))
    abort ();

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

  /* Don't GC while we prepare and show the menu,
     because we give the oldxmenu library pointers to the
     contents of strings.  */
  inhibit_garbage_collection ();

#ifdef HAVE_X_WINDOWS
  /* Adjust coordinates to relative to the outer (window manager) window.  */
  x += FRAME_OUTER_TO_INNER_DIFF_X (f);
  y += FRAME_OUTER_TO_INNER_DIFF_Y (f);
#endif /* HAVE_X_WINDOWS */

  /* Adjust coordinates to be root-window-relative.  */
  x += f->left_pos;
  y += f->top_pos;

  /* Create all the necessary panes and their items.  */
  maxlines = lines = i = 0;
  while (i < menu_items_used)
    {
      if (EQ (XVECTOR (menu_items)->contents[i], Qt))
	{
	  /* Create a new pane.  */
	  Lisp_Object pane_name, prefix;
	  char *pane_string;

          maxlines = max (maxlines, lines);
          lines = 0;
	  pane_name = XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_NAME];
	  prefix = XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_PREFIX];
	  pane_string = (NILP (pane_name)
			 ? "" : (char *) SDATA (pane_name));
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
	      width = SBYTES (item);
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
	  Lisp_Object item_name, enable, descrip, help;
	  unsigned char *item_data;
	  char *help_string;

	  item_name = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_NAME];
	  enable = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_ENABLE];
	  descrip
	    = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_EQUIV_KEY];
	  help = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_HELP];
	  help_string = STRINGP (help) ? SDATA (help) : NULL;

	  if (!NILP (descrip))
	    {
	      int gap = maxwidth - SBYTES (item_name);
	      /* if alloca is fast, use that to make the space,
		 to reduce gc needs.  */
	      item_data
		= (unsigned char *) alloca (maxwidth
					    + SBYTES (descrip) + 1);
	      bcopy (SDATA (item_name), item_data,
		     SBYTES (item_name));
	      for (j = SCHARS (item_name); j < maxwidth; j++)
		item_data[j] = ' ';
	      bcopy (SDATA (descrip), item_data + j,
		     SBYTES (descrip));
	      item_data[j + SBYTES (descrip)] = 0;
	    }
	  else
	    item_data = SDATA (item_name);

	  if (XMenuAddSelection (FRAME_X_DISPLAY (f),
				 menu, lpane, 0, item_data,
				 !NILP (enable), help_string)
	      == XM_FAILURE)
	    {
	      XMenuDestroy (FRAME_X_DISPLAY (f), menu);
	      *error = "Can't add selection to menu";
	      return Qnil;
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
  if (ulx < 0) x -= ulx;
  if (uly < 0) y -= uly;

  if (! for_click)
    {
      /* If position was not given by a mouse click, adjust so upper left
         corner of the menu as a whole ends up at given coordinates.  This
         is what x-popup-menu says in its documentation.  */
      x += width/2;
      y += 1.5*height/(maxlines+2);
    }

  XMenuSetAEQ (menu, TRUE);
  XMenuSetFreeze (menu, TRUE);
  pane = selidx = 0;

#ifndef MSDOS
  XMenuActivateSetWaitFunction (x_menu_wait_for_event, FRAME_X_DISPLAY (f));
#endif

  record_unwind_protect (pop_down_menu,
                         Fcons (make_save_value (f, 0),
                                make_save_value (menu, 0)));

  /* Help display under X won't work because XMenuActivate contains
     a loop that doesn't give Emacs a chance to process it.  */
  menu_help_frame = f;
  status = XMenuActivate (FRAME_X_DISPLAY (f), menu, &pane, &selidx,
                          x, y, ButtonReleaseMask, &datap,
                          menu_help_callback);

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
      entry = Qnil;
      break;
    case XM_NO_SELECT:
      /* Make "Cancel" equivalent to C-g unless FOR_CLICK (which means
	 the menu was invoked with a mouse event as POSITION).  */
      if (! for_click)
        Fsignal (Qquit, Qnil);
      entry = Qnil;
      break;
    }

  unbind_to (specpdl_count, Qnil);

  return entry;
}

#endif /* not USE_X_TOOLKIT */

#endif /* HAVE_MENUS */

/* Detect if a dialog or menu has been posted.  */

int
popup_activated ()
{
  return popup_activated_flag;
}

/* The following is used by delayed window autoselection.  */

DEFUN ("menu-or-popup-active-p", Fmenu_or_popup_active_p, Smenu_or_popup_active_p, 0, 0, 0,
       doc: /* Return t if a menu or popup dialog is active.  */)
     ()
{
#ifdef HAVE_MENUS
  return (popup_activated ()) ? Qt : Qnil;
#else
  return Qnil;
#endif /* HAVE_MENUS */
}

void
syms_of_xmenu ()
{
  staticpro (&menu_items);
  menu_items = Qnil;
  menu_items_inuse = Qnil;

  Qdebug_on_next_call = intern ("debug-on-next-call");
  staticpro (&Qdebug_on_next_call);

#ifdef USE_X_TOOLKIT
  widget_id_tick = (1<<16);
  next_menubar_widget_id = 1;
#endif

  defsubr (&Sx_popup_menu);
  defsubr (&Smenu_or_popup_active_p);

#if defined (USE_GTK) || defined (USE_X_TOOLKIT)
  defsubr (&Sx_menu_bar_open_internal);
  Ffset (intern ("accelerate-menu"),
	 intern (Sx_menu_bar_open_internal.symbol_name));
#endif

#ifdef HAVE_MENUS
  defsubr (&Sx_popup_dialog);
#endif
}

/* arch-tag: 92ea573c-398e-496e-ac73-2436f7d63242
   (do not change this comment) */
