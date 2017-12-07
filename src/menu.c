/* Platform-independent code for terminal communications.

Copyright (C) 1986, 1988, 1993-1994, 1996, 1999-2017 Free Software
Foundation, Inc.

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
#include <stdio.h>
#include <limits.h> /* for INT_MAX */

#include "lisp.h"
#include "character.h"
#include "coding.h"
#include "keyboard.h"
#include "keymap.h"
#include "frame.h"
#include "window.h"
#include "termhooks.h"
#include "blockinput.h"
#include "buffer.h"

#ifdef USE_X_TOOLKIT
#include "../lwlib/lwlib.h"
#endif

#ifdef HAVE_WINDOW_SYSTEM
#include TERM_HEADER
#endif /* HAVE_WINDOW_SYSTEM */

#ifdef HAVE_NTGUI
extern AppendMenuW_Proc unicode_append_menu;
#endif /* HAVE_NTGUI  */

#include "menu.h"

/* Return non-zero if menus can handle radio and toggle buttons.  */
static bool
have_boxes (void)
{
#if defined (USE_X_TOOLKIT) || defined (USE_GTK) || defined (HAVE_NTGUI) || defined(HAVE_NS)
  if (FRAME_WINDOW_P (XFRAME (Vmenu_updating_frame)))
    return 1;
#endif
  return 0;
}

Lisp_Object menu_items;

/* If non-nil, means that the global vars defined here are already in use.
   Used to detect cases where we try to re-enter this non-reentrant code.  */
Lisp_Object menu_items_inuse;

/* Number of slots currently allocated in menu_items.  */
int menu_items_allocated;

/* This is the index in menu_items of the first empty slot.  */
int menu_items_used;

/* The number of panes currently recorded in menu_items,
   excluding those within submenus.  */
int menu_items_n_panes;

/* Current depth within submenus.  */
static int menu_items_submenu_depth;

void
init_menu_items (void)
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

void
finish_menu_items (void)
{
}

void
unuse_menu_items (void)
{
  menu_items_inuse = Qnil;
}

/* Call when finished using the data for the current menu
   in menu_items.  */

void
discard_menu_items (void)
{
  /* Free the structure if it is especially large.
     Otherwise, hold on to it, to save time.  */
  if (menu_items_allocated > 200)
    {
      menu_items = Qnil;
      menu_items_allocated = 0;
    }
  eassert (NILP (menu_items_inuse));
}

/* This undoes save_menu_items, and it is called by the specpdl unwind
   mechanism.  */

static void
restore_menu_items (Lisp_Object saved)
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
}

/* Push the whole state of menu_items processing onto the specpdl.
   It will be restored when the specpdl is unwound.  */

void
save_menu_items (void)
{
  Lisp_Object saved = list4 (!NILP (menu_items_inuse) ? menu_items : Qnil,
			     make_number (menu_items_used),
			     make_number (menu_items_n_panes),
			     make_number (menu_items_submenu_depth));
  record_unwind_protect (restore_menu_items, saved);
  menu_items_inuse = Qnil;
  menu_items = Qnil;
}


/* Ensure that there is room for ITEMS items in the menu_items vector.  */

static void
ensure_menu_items (int items)
{
  int incr = items - (menu_items_allocated - menu_items_used);
  if (incr > 0)
    {
      menu_items = larger_vector (menu_items, incr, INT_MAX);
      menu_items_allocated = ASIZE (menu_items);
    }
}

#if (defined USE_X_TOOLKIT || defined USE_GTK || defined HAVE_NS \
     || defined HAVE_NTGUI)

/* Begin a submenu.  */

static void
push_submenu_start (void)
{
  ensure_menu_items (1);
  ASET (menu_items, menu_items_used, Qnil);
  menu_items_used++;
  menu_items_submenu_depth++;
}

/* End a submenu.  */

static void
push_submenu_end (void)
{
  ensure_menu_items (1);
  ASET (menu_items, menu_items_used, Qlambda);
  menu_items_used++;
  menu_items_submenu_depth--;
}

#endif /* USE_X_TOOLKIT || USE_GTK || HAVE_NS || defined HAVE_NTGUI */

/* Indicate boundary between left and right.  */

static void
push_left_right_boundary (void)
{
  ensure_menu_items (1);
  ASET (menu_items, menu_items_used, Qquote);
  menu_items_used++;
}

/* Start a new menu pane in menu_items.
   NAME is the pane name.  PREFIX_VEC is a prefix key for this pane.  */

static void
push_menu_pane (Lisp_Object name, Lisp_Object prefix_vec)
{
  ensure_menu_items (MENU_ITEMS_PANE_LENGTH);
  if (menu_items_submenu_depth == 0)
    menu_items_n_panes++;
  ASET (menu_items, menu_items_used, Qt);
  menu_items_used++;
  ASET (menu_items, menu_items_used, name);
  menu_items_used++;
  ASET (menu_items, menu_items_used, prefix_vec);
  menu_items_used++;
}

/* Push one menu item into the current pane.  NAME is the string to
   display.  ENABLE if non-nil means this item can be selected.  KEY
   is the key generated by choosing this item, or nil if this item
   doesn't really have a definition.  DEF is the definition of this
   item.  EQUIV is the textual description of the keyboard equivalent
   for this item (or nil if none).  TYPE is the type of this menu
   item, one of nil, `toggle' or `radio'. */

static void
push_menu_item (Lisp_Object name, Lisp_Object enable, Lisp_Object key, Lisp_Object def, Lisp_Object equiv, Lisp_Object type, Lisp_Object selected, Lisp_Object help)
{
  ensure_menu_items (MENU_ITEMS_ITEM_LENGTH);

  ASET (menu_items, menu_items_used + MENU_ITEMS_ITEM_NAME,	name);
  ASET (menu_items, menu_items_used + MENU_ITEMS_ITEM_ENABLE,	enable);
  ASET (menu_items, menu_items_used + MENU_ITEMS_ITEM_VALUE,	key);
  ASET (menu_items, menu_items_used + MENU_ITEMS_ITEM_EQUIV_KEY, equiv);
  ASET (menu_items, menu_items_used + MENU_ITEMS_ITEM_DEFINITION, def);
  ASET (menu_items, menu_items_used + MENU_ITEMS_ITEM_TYPE,	type);
  ASET (menu_items, menu_items_used + MENU_ITEMS_ITEM_SELECTED,	selected);
  ASET (menu_items, menu_items_used + MENU_ITEMS_ITEM_HELP,	help);

  menu_items_used += MENU_ITEMS_ITEM_LENGTH;
}

/* Args passed between single_keymap_panes and single_menu_item.  */
struct skp
  {
     Lisp_Object pending_maps;
     int maxdepth;
     int notbuttons;
  };

static void single_menu_item (Lisp_Object, Lisp_Object, Lisp_Object,
                              void *);

/* This is a recursive subroutine of keymap_panes.
   It handles one keymap, KEYMAP.
   The other arguments are passed along
   or point to local variables of the previous function.

   If we encounter submenus deeper than MAXDEPTH levels, ignore them.  */

static void
single_keymap_panes (Lisp_Object keymap, Lisp_Object pane_name,
		     Lisp_Object prefix, int maxdepth)
{
  struct skp skp;

  skp.pending_maps = Qnil;
  skp.maxdepth = maxdepth;
  skp.notbuttons = 0;

  if (maxdepth <= 0)
    return;

  push_menu_pane (pane_name, prefix);

  if (!have_boxes ())
    {
      /* Remember index for first item in this pane so we can go back
	 and add a prefix when (if) we see the first button.  After
	 that, notbuttons is set to 0, to mark that we have seen a
	 button and all non button items need a prefix.  */
      skp.notbuttons = menu_items_used;
    }

  map_keymap_canonical (keymap, single_menu_item, Qnil, &skp);

  /* Process now any submenus which want to be panes at this level.  */
  while (CONSP (skp.pending_maps))
    {
      Lisp_Object elt, eltcdr, string;
      elt = XCAR (skp.pending_maps);
      eltcdr = XCDR (elt);
      string = XCAR (eltcdr);
      /* We no longer discard the @ from the beginning of the string here.
	 Instead, we do this in *menu_show.  */
      single_keymap_panes (Fcar (elt), string, XCDR (eltcdr), maxdepth - 1);
      skp.pending_maps = XCDR (skp.pending_maps);
    }
}

/* This is a subroutine of single_keymap_panes that handles one
   keymap entry.
   KEY is a key in a keymap and ITEM is its binding.
   SKP->PENDING_MAPS_PTR is a list of keymaps waiting to be made into
   separate panes.
   If we encounter submenus deeper than SKP->MAXDEPTH levels, ignore them.  */

static void
single_menu_item (Lisp_Object key, Lisp_Object item, Lisp_Object dummy, void *skp_v)
{
  Lisp_Object map, item_string, enabled;
  bool res;
  struct skp *skp = skp_v;

  /* Parse the menu item and leave the result in item_properties.  */
  res = parse_menu_item (item, 0);
  if (!res)
    return;			/* Not a menu item.  */

  map = AREF (item_properties, ITEM_PROPERTY_MAP);

  enabled = AREF (item_properties, ITEM_PROPERTY_ENABLE);
  item_string = AREF (item_properties, ITEM_PROPERTY_NAME);

  if (!NILP (map) && SREF (item_string, 0) == '@')
    {
      if (!NILP (enabled))
	/* An enabled separate pane. Remember this to handle it later.  */
	skp->pending_maps = Fcons (Fcons (map, Fcons (item_string, key)),
				   skp->pending_maps);
      return;
    }

  /* Simulate radio buttons and toggle boxes by putting a prefix in
     front of them.  */
  if (!have_boxes ())
    {
      char const *prefix = 0;
      Lisp_Object type = AREF (item_properties, ITEM_PROPERTY_TYPE);
      if (!NILP (type))
	{
	  Lisp_Object selected
	    = AREF (item_properties, ITEM_PROPERTY_SELECTED);

	  if (skp->notbuttons)
	    /* The first button. Line up previous items in this menu.  */
	    {
	      int idx = skp->notbuttons; /* Index for first item this menu.  */
	      int submenu = 0;
	      Lisp_Object tem;
	      while (idx < menu_items_used)
		{
		  tem
		    = AREF (menu_items, idx + MENU_ITEMS_ITEM_NAME);
		  if (NILP (tem))
		    {
		      idx++;
		      submenu++;		/* Skip sub menu.  */
		    }
		  else if (EQ (tem, Qlambda))
		    {
		      idx++;
		      submenu--;		/* End sub menu.  */
		    }
		  else if (EQ (tem, Qt))
		    idx += 3;		/* Skip new pane marker. */
		  else if (EQ (tem, Qquote))
		    idx++;		/* Skip a left, right divider. */
		  else
		    {
		      if (!submenu && SREF (tem, 0) != '\0'
			  && SREF (tem, 0) != '-')
			{
			  AUTO_STRING (spaces, "    ");
			  ASET (menu_items, idx + MENU_ITEMS_ITEM_NAME,
				concat2 (spaces, tem));
			}
		      idx += MENU_ITEMS_ITEM_LENGTH;
		    }
		}
	      skp->notbuttons = 0;
	    }

	  /* Calculate prefix, if any, for this item.  */
	  if (EQ (type, QCtoggle))
	    prefix = NILP (selected) ? "[ ] " : "[X] ";
	  else if (EQ (type, QCradio))
	    prefix = NILP (selected) ? "( ) " : "(*) ";
	}
      /* Not a button. If we have earlier buttons, then we need a prefix.  */
      else if (!skp->notbuttons && SREF (item_string, 0) != '\0'
	       && SREF (item_string, 0) != '-')
	prefix = "    ";

      if (prefix)
	{
	  AUTO_STRING_WITH_LEN (prefix_obj, prefix, 4);
	  item_string = concat2 (prefix_obj, item_string);
	}
  }

  if ((FRAME_TERMCAP_P (XFRAME (Vmenu_updating_frame))
       || FRAME_MSDOS_P (XFRAME (Vmenu_updating_frame)))
      && !NILP (map))
    /* Indicate visually that this is a submenu.  */
    {
      AUTO_STRING (space_gt, " >");
      item_string = concat2 (item_string, space_gt);
    }

  push_menu_item (item_string, enabled, key,
		  AREF (item_properties, ITEM_PROPERTY_DEF),
		  AREF (item_properties, ITEM_PROPERTY_KEYEQ),
		  AREF (item_properties, ITEM_PROPERTY_TYPE),
		  AREF (item_properties, ITEM_PROPERTY_SELECTED),
		  AREF (item_properties, ITEM_PROPERTY_HELP));

#if defined (USE_X_TOOLKIT) || defined (USE_GTK) || defined (HAVE_NS) || defined (HAVE_NTGUI)
  /* Display a submenu using the toolkit.  */
  if (FRAME_WINDOW_P (XFRAME (Vmenu_updating_frame))
      && ! (NILP (map) || NILP (enabled)))
    {
      push_submenu_start ();
      single_keymap_panes (map, Qnil, key, skp->maxdepth - 1);
      push_submenu_end ();
    }
#endif
}

/* Look through KEYMAPS, a vector of keymaps that is NMAPS long,
   and generate menu panes for them in menu_items.  */

static void
keymap_panes (Lisp_Object *keymaps, ptrdiff_t nmaps)
{
  ptrdiff_t mapno;

  init_menu_items ();

  /* Loop over the given keymaps, making a pane for each map.
     But don't make a pane that is empty--ignore that map instead.
     P is the number of panes we have made so far.  */
  for (mapno = 0; mapno < nmaps; mapno++)
    single_keymap_panes (keymaps[mapno],
			 Fkeymap_prompt (keymaps[mapno]), Qnil, 10);

  finish_menu_items ();
}

/* Encode a menu string as appropriate for menu-updating-frame's type.  */
static Lisp_Object
encode_menu_string (Lisp_Object str)
{
  /* TTY menu strings are encoded by write_glyphs, when they are
     delivered to the glass, so no need to encode them here.  */
  if (FRAME_TERMCAP_P (XFRAME (Vmenu_updating_frame)))
    return str;
  return ENCODE_MENU_STRING (str);
}

/* Push the items in a single pane defined by the alist PANE.  */
static void
list_of_items (Lisp_Object pane)
{
  Lisp_Object tail, item, item1;

  for (tail = pane; CONSP (tail); tail = XCDR (tail))
    {
      item = XCAR (tail);
      if (STRINGP (item))
	push_menu_item (encode_menu_string (item), Qnil, Qnil, Qt,
			Qnil, Qnil, Qnil, Qnil);
      else if (CONSP (item))
	{
	  item1 = XCAR (item);
	  CHECK_STRING (item1);
	  push_menu_item (encode_menu_string (item1), Qt, XCDR (item),
			  Qt, Qnil, Qnil, Qnil, Qnil);
	}
      else
	push_left_right_boundary ();

    }
}

/* Push all the panes and items of a menu described by the
   alist-of-alists MENU.
   This handles old-fashioned calls to x-popup-menu.  */
void
list_of_panes (Lisp_Object menu)
{
  Lisp_Object tail;

  init_menu_items ();

  for (tail = menu; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object elt, pane_name, pane_data;
      elt = XCAR (tail);
      pane_name = Fcar (elt);
      CHECK_STRING (pane_name);
      push_menu_pane (encode_menu_string (pane_name), Qnil);
      pane_data = Fcdr (elt);
      CHECK_CONS (pane_data);
      list_of_items (pane_data);
    }

  finish_menu_items ();
}

/* Set up data in menu_items for a menu bar item
   whose event type is ITEM_KEY (with string ITEM_NAME)
   and whose contents come from the list of keymaps MAPS.  */
bool
parse_single_submenu (Lisp_Object item_key, Lisp_Object item_name,
		      Lisp_Object maps)
{
  Lisp_Object length;
  EMACS_INT len;
  Lisp_Object *mapvec;
  ptrdiff_t i;
  bool top_level_items = 0;
  USE_SAFE_ALLOCA;

  length = Flength (maps);
  len = XINT (length);

  /* Convert the list MAPS into a vector MAPVEC.  */
  SAFE_ALLOCA_LISP (mapvec, len);
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
			       item_key, 10);
	}
    }

  SAFE_FREE ();
  return top_level_items;
}


#if defined (USE_X_TOOLKIT) || defined (USE_GTK) || defined (HAVE_NS) || defined (HAVE_NTGUI)

/* Allocate and basically initialize widget_value, blocking input.  */

widget_value *
make_widget_value (const char *name, char *value,
		   bool enabled, Lisp_Object help)
{
  widget_value *wv;

  block_input ();
  wv = xzalloc (sizeof (widget_value));
  unblock_input ();

  wv->name = (char *) name;
  wv->value = value;
  wv->enabled = enabled;
  wv->help = help;
  return wv;
}

/* This recursively calls xfree on the tree of widgets.
   It must free all data that was malloc'ed for these widget_values.
   In Emacs, many slots are pointers into the data of Lisp_Strings, and
   must be left alone.  */

void
free_menubar_widget_value_tree (widget_value *wv)
{
  if (! wv) return;

  wv->name = wv->value = wv->key = (char *) 0xDEADBEEF;

  if (wv->contents && (wv->contents != (widget_value *) 1))
    {
      free_menubar_widget_value_tree (wv->contents);
      wv->contents = (widget_value *) 0xDEADBEEF;
    }
  if (wv->next)
    {
      free_menubar_widget_value_tree (wv->next);
      wv->next = (widget_value *) 0xDEADBEEF;
    }
  block_input ();
  xfree (wv);
  unblock_input ();
}

/* Create a tree of widget_value objects
   representing the panes and items
   in menu_items starting at index START, up to index END.  */

widget_value *
digest_single_submenu (int start, int end, bool top_level_items)
{
  widget_value *wv, *prev_wv, *save_wv, *first_wv;
  int i;
  int submenu_depth = 0;
  widget_value **submenu_stack;
  bool panes_seen = 0;
  struct frame *f = XFRAME (Vmenu_updating_frame);
  USE_SAFE_ALLOCA;

  SAFE_NALLOCA (submenu_stack, 1, menu_items_used);
  wv = make_widget_value ("menu", NULL, true, Qnil);
  wv->button_type = BUTTON_TYPE_NONE;
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
	  Lisp_Object pane_name;
	  const char *pane_string;

	  panes_seen = 1;

	  pane_name = AREF (menu_items, i + MENU_ITEMS_PANE_NAME);

	  /* TTY menus display menu items via tty_write_glyphs, which
	     will encode the strings as appropriate.  */
	  if (!FRAME_TERMCAP_P (f))
	    {
#ifdef HAVE_NTGUI
	      if (STRINGP (pane_name))
		{
		  if (unicode_append_menu)
		    /* Encode as UTF-8 for now.  */
		    pane_name = ENCODE_UTF_8 (pane_name);
		  else if (STRING_MULTIBYTE (pane_name))
		    pane_name = ENCODE_SYSTEM (pane_name);

		  ASET (menu_items, i + MENU_ITEMS_PANE_NAME, pane_name);
		}
#elif defined (USE_LUCID) && defined (HAVE_XFT)
	      if (STRINGP (pane_name))
		{
		  pane_name = ENCODE_UTF_8 (pane_name);
		  ASET (menu_items, i + MENU_ITEMS_PANE_NAME, pane_name);
		}
#elif !defined (HAVE_MULTILINGUAL_MENU)
	      if (STRINGP (pane_name) && STRING_MULTIBYTE (pane_name))
		{
		  pane_name = ENCODE_MENU_STRING (pane_name);
		  ASET (menu_items, i + MENU_ITEMS_PANE_NAME, pane_name);
		}
#endif
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
	  if (strcmp (pane_string, ""))
	    {
	      /* Set value to 1 so update_submenu_strings can handle '@'.  */
	      wv = make_widget_value (NULL, (char *) 1, true, Qnil);
	      if (save_wv)
		save_wv->next = wv;
	      else
		first_wv->contents = wv;
	      wv->lname = pane_name;
	      wv->button_type = BUTTON_TYPE_NONE;
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
	  if (! panes_seen)
	    emacs_abort ();

	  item_name = AREF (menu_items, i + MENU_ITEMS_ITEM_NAME);
	  enable = AREF (menu_items, i + MENU_ITEMS_ITEM_ENABLE);
	  descrip = AREF (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY);
	  def = AREF (menu_items, i + MENU_ITEMS_ITEM_DEFINITION);
	  type = AREF (menu_items, i + MENU_ITEMS_ITEM_TYPE);
	  selected = AREF (menu_items, i + MENU_ITEMS_ITEM_SELECTED);
	  help = AREF (menu_items, i + MENU_ITEMS_ITEM_HELP);

	  /* TTY menu items and their descriptions will be encoded by
	     tty_write_glyphs.  */
	  if (!FRAME_TERMCAP_P (f))
	    {
#ifdef HAVE_NTGUI
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
#elif USE_LUCID
	      if (STRINGP (item_name))
		{
		  item_name = ENCODE_UTF_8 (item_name);
		  ASET (menu_items, i + MENU_ITEMS_ITEM_NAME, item_name);
		}

	      if (STRINGP (descrip))
		{
		  descrip = ENCODE_UTF_8 (descrip);
		  ASET (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY, descrip);
		}
#elif !defined (HAVE_MULTILINGUAL_MENU)
	      if (STRING_MULTIBYTE (item_name))
		{
		  item_name = ENCODE_MENU_STRING (item_name);
		  ASET (menu_items, i + MENU_ITEMS_ITEM_NAME, item_name);
		}

	      if (STRINGP (descrip) && STRING_MULTIBYTE (descrip))
		{
		  descrip = ENCODE_MENU_STRING (descrip);
		  ASET (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY, descrip);
		}
#endif
	    }

	  wv = make_widget_value (NULL, NULL, !NILP (enable),
				  STRINGP (help) ? help : Qnil);
	  if (prev_wv)
	    prev_wv->next = wv;
	  else
	    save_wv->contents = wv;

	  wv->lname = item_name;
	  if (!NILP (descrip))
	    wv->lkey = descrip;
	  /* The intptr_t cast avoids a warning.  There's no problem
	     as long as pointers have enough bits to hold small integers.  */
	  wv->call_data = (!NILP (def) ? (void *) (intptr_t) i : 0);

	  if (NILP (type))
	    wv->button_type = BUTTON_TYPE_NONE;
	  else if (EQ (type, QCradio))
	    wv->button_type = BUTTON_TYPE_RADIO;
	  else if (EQ (type, QCtoggle))
	    wv->button_type = BUTTON_TYPE_TOGGLE;
	  else
	    emacs_abort ();

	  wv->selected = !NILP (selected);

	  prev_wv = wv;

	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }

  /* If we have just one "menu item"
     that was originally a button, return it by itself.  */
  if (top_level_items && first_wv->contents && first_wv->contents->next == 0)
    {
      wv = first_wv;
      first_wv = first_wv->contents;
      xfree (wv);
    }

  SAFE_FREE ();
  return first_wv;
}

/* Walk through the widget_value tree starting at FIRST_WV and update
   the char * pointers from the corresponding lisp values.
   We do this after building the whole tree, since GC may happen while the
   tree is constructed, and small strings are relocated.  So we must wait
   until no GC can happen before storing pointers into lisp values.  */
void
update_submenu_strings (widget_value *first_wv)
{
  widget_value *wv;

  for (wv = first_wv; wv; wv = wv->next)
    {
      if (STRINGP (wv->lname))
        {
          wv->name = SSDATA (wv->lname);

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
        wv->key = SSDATA (wv->lkey);

      if (wv->contents)
        update_submenu_strings (wv->contents);
    }
}

/* Find the menu selection and store it in the keyboard buffer.
   F is the frame the menu is on.
   MENU_BAR_ITEMS_USED is the length of VECTOR.
   VECTOR is an array of menu events for the whole menu.  */

void
find_and_call_menu_selection (struct frame *f, int menu_bar_items_used,
			      Lisp_Object vector, void *client_data)
{
  Lisp_Object prefix, entry;
  Lisp_Object *subprefix_stack;
  int submenu_depth = 0;
  int i;
  USE_SAFE_ALLOCA;

  entry = Qnil;
  SAFE_NALLOCA (subprefix_stack, 1, menu_bar_items_used);
  prefix = Qnil;
  i = 0;

  while (i < menu_bar_items_used)
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
	  /* Treat the pointer as an integer.  There's no problem
	     as long as pointers have enough bits to hold small integers.  */
	  if ((intptr_t) client_data == i)
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

	      break;
	    }
	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }

  SAFE_FREE ();
}

#endif /* USE_X_TOOLKIT || USE_GTK || HAVE_NS || HAVE_NTGUI */

#ifdef HAVE_NS
/* As above, but return the menu selection instead of storing in kb buffer.
   If KEYMAPS, return full prefixes to selection. */
Lisp_Object
find_and_return_menu_selection (struct frame *f, bool keymaps, void *client_data)
{
  Lisp_Object prefix, entry;
  int i;
  Lisp_Object *subprefix_stack;
  int submenu_depth = 0;
  USE_SAFE_ALLOCA;

  prefix = entry = Qnil;
  i = 0;
  SAFE_ALLOCA_LISP (subprefix_stack, menu_items_used);

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
          if (aref_addr (menu_items, i) == client_data)
            {
              if (keymaps)
                {
                  int j;

                  entry = list1 (entry);
                  if (!NILP (prefix))
                    entry = Fcons (prefix, entry);
                  for (j = submenu_depth - 1; j >= 0; j--)
                    if (!NILP (subprefix_stack[j]))
                      entry = Fcons (subprefix_stack[j], entry);
                }
	      SAFE_FREE ();
              return entry;
            }
          i += MENU_ITEMS_ITEM_LENGTH;
        }
    }
  SAFE_FREE ();
  return Qnil;
}
#endif  /* HAVE_NS */

ptrdiff_t
menu_item_width (const unsigned char *str)
{
  ptrdiff_t len;
  const unsigned char *p;

  for (len = 0, p = str; *p; )
    {
      int ch_len;
      int ch = STRING_CHAR_AND_LENGTH (p, ch_len);

      len += CHARACTER_WIDTH (ch);
      p += ch_len;
    }
  return len;
}

DEFUN ("menu-bar-menu-at-x-y", Fmenu_bar_menu_at_x_y, Smenu_bar_menu_at_x_y,
       2, 3, 0,
       doc: /* Return the menu-bar menu on FRAME at pixel coordinates X, Y.
X and Y are frame-relative pixel coordinates, assumed to define
a location within the menu bar.
If FRAME is nil or omitted, it defaults to the selected frame.

Value is the symbol of the menu at X/Y, or nil if the specified
coordinates are not within the FRAME's menu bar.  The symbol can
be used to look up the menu like this:

     (lookup-key MAP [menu-bar SYMBOL])

where MAP is either the current global map or the current local map,
since menu-bar items come from both.

This function can return non-nil only on a text-terminal frame
or on an X frame that doesn't use any GUI toolkit.  Otherwise,
Emacs does not manage the menu bar and cannot convert coordinates
into menu items.  */)
  (Lisp_Object x, Lisp_Object y, Lisp_Object frame)
{
  int row, col;
  struct frame *f = decode_any_frame (frame);

  if (!FRAME_LIVE_P (f))
    return Qnil;

  pixel_to_glyph_coords (f, XINT (x), XINT (y), &col, &row, NULL, 1);
  if (0 <= row && row < FRAME_MENU_BAR_LINES (f))
    {
      Lisp_Object items, item;
      int i;

      /* Find the menu bar item under `col'.  */
      item = Qnil;
      items = FRAME_MENU_BAR_ITEMS (f);
      /* This loop assumes a single menu-bar line, and will fail to
	 find an item if it is not in the first line.  Note that
	 make_lispy_event in keyboard.c makes the same assumption.  */
      for (i = 0; i < ASIZE (items); i += 4)
	{
	  Lisp_Object pos, str;

	  str = AREF (items, i + 1);
	  pos = AREF (items, i + 3);
	  if (NILP (str))
	    return item;
	  if (XINT (pos) <= col
	      /* We use <= so the blank between 2 items on a TTY is
		 considered part of the previous item.  */
	      && col <= XINT (pos) + menu_item_width (SDATA (str)))
	    {
	      item = AREF (items, i);
	      return item;
	    }
	}
    }
  return Qnil;
}

Lisp_Object
x_popup_menu_1 (Lisp_Object position, Lisp_Object menu)
{
  Lisp_Object keymap, tem, tem2;
  int xpos = 0, ypos = 0;
  Lisp_Object title;
  const char *error_name = NULL;
  Lisp_Object selection = Qnil;
  struct frame *f = NULL;
  Lisp_Object x, y, window;
  int menuflags = 0;
  ptrdiff_t specpdl_count = SPECPDL_INDEX ();

  if (NILP (position))
    /* This is an obsolete call, which wants us to precompute the
       keybinding equivalents, but we don't do that any more anyway.  */
    return Qnil;

  {
    bool get_current_pos_p = 0;

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
	    menuflags |= MENU_FOR_CLICK;
	    tem = Fcar (Fcdr (position));  /* EVENT_START (position) */
	    window = Fcar (tem);	     /* POSN_WINDOW (tem) */
	    tem2 = Fcar (Fcdr (tem));	     /* POSN_POSN (tem) */
	    /* The MENU_KBD_NAVIGATION field is set when the menu
	       was invoked by F10, which probably means they have no
	       mouse.  In that case, we let them switch between
	       top-level menu-bar menus by using C-f/C-b and
	       horizontal arrow keys, since they cannot click the
	       mouse to open a different submenu.  This flag is only
	       supported by tty_menu_show.  We set it when POSITION
	       and last_nonmenu_event are different, which means we
	       constructed POSITION by hand (in popup-menu, see
	       menu-bar.el) to look like a mouse click on the menu bar
	       event.  */
	    if (!EQ (POSN_POSN (last_nonmenu_event),
		     POSN_POSN (position))
		&& CONSP (tem2) && EQ (Fcar (tem2), Qmenu_bar))
	      menuflags |= MENU_KBD_NAVIGATION;
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
	struct frame *new_f = SELECTED_FRAME ();

	XSETFASTINT (x, 0);
	XSETFASTINT (y, 0);
#ifdef HAVE_X_WINDOWS
	if (FRAME_X_P (new_f))
	  {
	    /* Can't use mouse_position_hook for X since it returns
	       coordinates relative to the window the mouse is in,
	       we need coordinates relative to the edit widget always.  */
	    if (new_f != 0)
	      {
		int cur_x, cur_y;

		x_relative_mouse_position (new_f, &cur_x, &cur_y);
		/* cur_x/y may be negative, so use make_number.  */
		x = make_number (cur_x);
		y = make_number (cur_y);
	      }
	  }
	else
#endif /* HAVE_X_WINDOWS */
	  {
	    Lisp_Object bar_window;
	    enum scroll_bar_part part;
	    Time time;
	    void (*mouse_position_hook) (struct frame **, int,
					 Lisp_Object *,
					 enum scroll_bar_part *,
					 Lisp_Object *,
					 Lisp_Object *,
					 Time *) =
	      FRAME_TERMINAL (new_f)->mouse_position_hook;

	    if (mouse_position_hook)
	      (*mouse_position_hook) (&new_f, 1, &bar_window,
				      &part, &x, &y, &time);
	  }

	if (new_f != 0)
	  XSETFRAME (window, new_f);
	else
	  {
	    window = selected_window;
	    XSETFASTINT (x, 0);
	    XSETFASTINT (y, 0);
	  }
      }

    /* Decode where to put the menu.  */

    if (FRAMEP (window))
      {
	f = XFRAME (window);
	xpos = 0;
	ypos = 0;
      }
    else if (WINDOWP (window))
      {
	struct window *win = XWINDOW (window);
	CHECK_LIVE_WINDOW (window);
	f = XFRAME (WINDOW_FRAME (win));

	xpos = WINDOW_LEFT_EDGE_X (win);
	ypos = WINDOW_TOP_EDGE_Y (win);
      }
    else
      /* ??? Not really clean; should be CHECK_WINDOW_OR_FRAME,
	 but I don't want to make one now.  */
      CHECK_WINDOW (window);

    CHECK_RANGED_INTEGER (x,
			  (xpos < INT_MIN - MOST_NEGATIVE_FIXNUM
			   ? (EMACS_INT) INT_MIN - xpos
			   : MOST_NEGATIVE_FIXNUM),
			  INT_MAX - xpos);
    CHECK_RANGED_INTEGER (y,
			  (ypos < INT_MIN - MOST_NEGATIVE_FIXNUM
			   ? (EMACS_INT) INT_MIN - ypos
			   : MOST_NEGATIVE_FIXNUM),
			  INT_MAX - ypos);
    xpos += XINT (x);
    ypos += XINT (y);

    XSETFRAME (Vmenu_updating_frame, f);
  }

  /* Now parse the lisp menus.  */
  record_unwind_protect_void (unuse_menu_items);

  title = Qnil;

  /* Decode the menu items from what was specified.  */

  keymap = get_keymap (menu, 0, 0);
  if (CONSP (keymap))
    {
      /* We were given a keymap.  Extract menu info from the keymap.  */
      Lisp_Object prompt;

      /* Extract the detailed info to make one pane.  */
      keymap_panes (&menu, 1);

      /* Search for a string appearing directly as an element of the keymap.
	 That string is the title of the menu.  */
      prompt = Fkeymap_prompt (keymap);
      if (!NILP (prompt))
	title = prompt;
#ifdef HAVE_NS		/* Is that needed and NS-specific?  --Stef  */
      else
	title = build_string ("Select");
#endif

      /* Make that be the pane title of the first pane.  */
      if (!NILP (prompt) && menu_items_n_panes >= 0)
	ASET (menu_items, MENU_ITEMS_PANE_NAME, prompt);

      menuflags |= MENU_KEYMAPS;
    }
  else if (CONSP (menu) && KEYMAPP (XCAR (menu)))
    {
      /* We were given a list of keymaps.  */
      EMACS_INT nmaps = XFASTINT (Flength (menu));
      Lisp_Object *maps;
      ptrdiff_t i;
      USE_SAFE_ALLOCA;

      SAFE_ALLOCA_LISP (maps, nmaps);
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
      keymap_panes (maps, nmaps);

      /* Make the title be the pane title of the first pane.  */
      if (!NILP (title) && menu_items_n_panes >= 0)
	ASET (menu_items, MENU_ITEMS_PANE_NAME, title);

      menuflags |= MENU_KEYMAPS;

      SAFE_FREE ();
    }
  else
    {
      /* We were given an old-fashioned menu.  */
      title = Fcar (menu);
      CHECK_STRING (title);

      list_of_panes (Fcdr (menu));

      menuflags &= ~MENU_KEYMAPS;
    }

  unbind_to (specpdl_count, Qnil);

#ifdef HAVE_WINDOW_SYSTEM
  /* Hide a previous tip, if any.  */
  if (!FRAME_TERMCAP_P (f))
    Fx_hide_tip ();
#endif

#ifdef HAVE_NTGUI     /* FIXME: Is it really w32-specific?  --Stef  */
  /* If resources from a previous popup menu still exist, does nothing
     until the `menu_free_timer' has freed them (see w32fns.c). This
     can occur if you press ESC or click outside a menu without selecting
     a menu item.
  */
  if (current_popup_menu && FRAME_W32_P (f))
    {
      discard_menu_items ();
      FRAME_DISPLAY_INFO (f)->grabbed = 0;
      return Qnil;
    }
#endif

#ifdef HAVE_NS			/* FIXME: ns-specific, why? --Stef  */
  record_unwind_protect_void (discard_menu_items);
#endif

  /* Display them in a menu, but not if F is the initial frame that
     doesn't have its hooks set (e.g., in a batch session), because
     such a frame cannot display menus.  */
  if (!FRAME_INITIAL_P (f))
    selection = FRAME_TERMINAL (f)->menu_show_hook (f, xpos, ypos, menuflags,
						    title, &error_name);

#ifdef HAVE_NS
  unbind_to (specpdl_count, Qnil);
#else
  discard_menu_items ();
#endif

#ifdef HAVE_NTGUI     /* FIXME: Is it really w32-specific?  --Stef  */
  if (FRAME_W32_P (f))
    FRAME_DISPLAY_INFO (f)->grabbed = 0;
#endif

  if (error_name) error ("%s", error_name);
  return selection;
}

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
  (Lisp_Object position, Lisp_Object menu)
{
  init_raw_keybuf_count ();
  return x_popup_menu_1 (position, menu);
}

/* If F's terminal is not capable of displaying a popup dialog,
   emulate it with a menu.  */

static Lisp_Object
emulate_dialog_with_menu (struct frame *f, Lisp_Object contents)
{
  Lisp_Object x, y, frame, newpos, prompt = Fcar (contents);
  int x_coord, y_coord;

  if (FRAME_WINDOW_P (f))
    {
      x_coord = FRAME_PIXEL_WIDTH (f);
      y_coord = FRAME_PIXEL_HEIGHT (f);
    }
  else
    {
      x_coord = FRAME_COLS (f);
      /* Center the title at frame middle.  (TTY menus have
	 their upper-left corner at the given position.)  */
      if (STRINGP (prompt))
	x_coord -= SCHARS (prompt);
      y_coord = FRAME_TOTAL_LINES (f);
    }

  XSETFRAME (frame, f);
  XSETINT (x, x_coord / 2);
  XSETINT (y, y_coord / 2);
  newpos = list2 (list2 (x, y), frame);

  return Fx_popup_menu (newpos, list2 (prompt, contents));
}

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
  (Lisp_Object position, Lisp_Object contents, Lisp_Object header)
{
  struct frame *f = NULL;
  Lisp_Object window;

  /* Decode the first argument: find the window or frame to use.  */
  if (EQ (position, Qt)
      || (CONSP (position) && (EQ (XCAR (position), Qmenu_bar)
			       || EQ (XCAR (position), Qtool_bar))))
    window = selected_window;
  else if (CONSP (position))
    {
      Lisp_Object tem = XCAR (position);
      if (CONSP (tem))
	window = Fcar (XCDR (position));
      else
	{
	  tem = Fcar (XCDR (position));  /* EVENT_START (position) */
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

  /* Note that xw_popup_dialog can call menu code, so
     Vmenu_updating_frame should be set (Bug#17891).  */
  eassume (f && FRAME_LIVE_P (f));
  XSETFRAME (Vmenu_updating_frame, f);

  /* Force a redisplay before showing the dialog.  If a frame is created
     just before showing the dialog, its contents may not have been fully
     drawn, as this depends on timing of events from the X server.  Redisplay
     is not done when a dialog is shown.  If redisplay could be done in the
     X event loop (i.e. the X event loop does not run in a signal handler)
     this would not be needed.

     Do this before creating the widget value that points to Lisp
     string contents, because Fredisplay may GC and relocate them.  */
  Fredisplay (Qt);

  /* Display the popup dialog by a terminal-specific hook ... */
  if (FRAME_TERMINAL (f)->popup_dialog_hook)
    {
      Lisp_Object selection
	= FRAME_TERMINAL (f)->popup_dialog_hook (f, header, contents);
#ifdef HAVE_NTGUI
      /* NTGUI supports only simple dialogs with Yes/No choices.  For
	 other dialogs, it returns the symbol 'unsupported--w32-dialog',
	 as a signal for the caller to fall back to the emulation code.  */
      if (!EQ (selection, Qunsupported__w32_dialog))
#endif
	return selection;
    }
  /* ... or emulate it with a menu.  */
  return emulate_dialog_with_menu (f, contents);
}

void
syms_of_menu (void)
{
  staticpro (&menu_items);
  menu_items = Qnil;
  menu_items_inuse = Qnil;

  defsubr (&Sx_popup_menu);
  defsubr (&Sx_popup_dialog);
  defsubr (&Smenu_bar_menu_at_x_y);
}
