/* X Communication module for terminals which understand the X protocol.
   Copyright (C) 1986, 1988, 1993 Free Software Foundation, Inc.

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

#ifdef XDEBUG
#include <stdio.h>
#endif

/* On 4.3 this loses if it comes after xterm.h.  */
#include <signal.h>
#include <config.h>
#include "lisp.h"
#include "frame.h"
#include "window.h"
#include "keyboard.h"
#include "blockinput.h"

/* This may include sys/types.h, and that somehow loses
   if this is not done before the other system files.  */
#include "xterm.h"

/* Load sys/types.h if not already loaded.
   In some systems loading it twice is suicidal.  */
#ifndef makedev
#include <sys/types.h>
#endif

#include "dispextern.h"

#ifdef HAVE_X11
#include "../oldXMenu/XMenu.h"
#else
#include <X/XMenu.h>
#endif

#define min(x,y) (((x) < (y)) ? (x) : (y))
#define max(x,y) (((x) > (y)) ? (x) : (y))

#define NUL 0

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif /* TRUE */

#ifdef HAVE_X11
extern Display *x_current_display;
#else
#define	ButtonReleaseMask ButtonReleased
#endif /* not HAVE_X11 */

extern Lisp_Object Qmenu_enable;
Lisp_Object xmenu_show ();
extern int x_error_handler ();

/*************************************************************/

#if 0
/* Ignoring the args is easiest.  */
xmenu_quit ()
{
  error ("Unknown XMenu error");
}
#endif

DEFUN ("x-popup-menu",Fx_popup_menu, Sx_popup_menu, 1, 2, 0,
  "Pop up a deck-of-cards menu and return user's selection.\n\
POSITION is a position specification.  This is either a mouse button event\n\
or a list ((XOFFSET YOFFSET) WINDOW)\n\
where XOFFSET and YOFFSET are positions in characters from the top left\n\
corner of WINDOW's frame.  (WINDOW may be a frame object instead of a window.)\n\
This controls the position of the center of the first line\n\
in the first pane of the menu, not the top left of the menu as a whole.\n\
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
With this form of menu, the return value is VALUE from the chosen item.")
  (position, menu)
     Lisp_Object position, menu;
{
  int number_of_panes;
  Lisp_Object XMenu_return, keymap, tem;
  int XMenu_xpos, XMenu_ypos;
  char **menus;
  char ***names;
  int **enables;
  Lisp_Object **obj_list;
  Lisp_Object *prefixes;
  int *items;
  char *title;
  char *error_name;
  Lisp_Object ltitle, selection;
  int i, j;
  FRAME_PTR f;
  Lisp_Object x, y, window;

  /* Decode the first argument: find the window and the coordinates.  */
  tem = Fcar (position);
  if (XTYPE (tem) == Lisp_Cons)
    {
      window = Fcar (Fcdr (position));
      x = Fcar (tem);
      y = Fcar (Fcdr (tem));
    }
  else
    {
      tem = Fcar (Fcdr (position));  /* EVENT_START (position) */
      window = Fcar (tem);	     /* POSN_WINDOW (tem) */
      tem = Fcar (Fcdr (Fcdr (tem))); /* POSN_WINDOW_POSN (tem) */
      x = Fcar (tem);
      y = Fcdr (tem);
    }
  CHECK_NUMBER (x, 0);
  CHECK_NUMBER (y, 0);

  if (XTYPE (window) == Lisp_Frame)
    {
      f = XFRAME (window);

      XMenu_xpos = 0;
      XMenu_ypos = 0;
    }
  else if (XTYPE (window) == Lisp_Window)
    {
      CHECK_LIVE_WINDOW (window, 0);
      f = XFRAME (WINDOW_FRAME (XWINDOW (window)));

      XMenu_xpos = FONT_WIDTH (f->display.x->font) * XWINDOW (window)->left;
      XMenu_ypos = FONT_HEIGHT (f->display.x->font) * XWINDOW (window)->top;
    }
  else
    /* ??? Not really clean; should be CHECK_WINDOW_OR_FRAME,
       but I don't want to make one now.  */
    CHECK_WINDOW (window, 0);

#ifdef HAVE_X11
  {
    Window child;
    int win_x = 0, win_y = 0;

    /* Find the position of the outside upper-left corner of
       the inner window, with respect to the outer window.  */
    if (f->display.x->parent_desc != ROOT_WINDOW)
      {
	BLOCK_INPUT;
	XTranslateCoordinates (x_current_display,

			       /* From-window, to-window.  */
			       f->display.x->window_desc,
			       f->display.x->parent_desc,

			       /* From-position, to-position.  */
			       0, 0, &win_x, &win_y,

			       /* Child of window.  */
			       &child);
	UNBLOCK_INPUT;
	XMenu_xpos += win_x;
	XMenu_ypos += win_y;
      }
  }
#endif

  XMenu_xpos += FONT_WIDTH (f->display.x->font) * XINT (x);
  XMenu_ypos += FONT_HEIGHT (f->display.x->font) * XINT (y);

  XMenu_xpos += f->display.x->left_pos;
  XMenu_ypos += f->display.x->top_pos;

  keymap = Fkeymapp (menu);
  tem = Qnil;
  if (XTYPE (menu) == Lisp_Cons)
    tem = Fkeymapp (Fcar (menu));
  if (!NILP (keymap))
    {
      /* We were given a keymap.  Extract menu info from the keymap.  */
      Lisp_Object prompt;
      keymap = get_keymap (menu);

      /* Search for a string appearing directly as an element of the keymap.
	 That string is the title of the menu.  */
      prompt = map_prompt (keymap);
      if (!NILP (prompt))
	title = (char *) XSTRING (prompt)->data;

      /* Extract the detailed info to make one pane.  */
      number_of_panes = keymap_panes (&obj_list, &menus, &names, &enables,
				      &items, &prefixes, &menu, 1);
      /* The menu title seems to be ignored,
	 so put it in the pane title.  */
      if (menus[0] == 0)
	menus[0] = title;
    }
  else if (!NILP (tem))
    {
      /* We were given a list of keymaps.  */
      Lisp_Object prompt;
      int nmaps = XFASTINT (Flength (menu));
      Lisp_Object *maps
	= (Lisp_Object *) alloca (nmaps * sizeof (Lisp_Object));
      int i;
      title = 0;

      /* The first keymap that has a prompt string
	 supplies the menu title.  */
      for (tem = menu, i = 0; XTYPE (tem) == Lisp_Cons; tem = Fcdr (tem))
	{
	  maps[i++] = keymap = get_keymap (Fcar (tem));

	  prompt = map_prompt (keymap);
	  if (title == 0 && !NILP (prompt))
	    title = (char *) XSTRING (prompt)->data;
	}

      /* Extract the detailed info to make one pane.  */
      number_of_panes = keymap_panes (&obj_list, &menus, &names, &enables,
				      &items, &prefixes, maps, nmaps);
      /* The menu title seems to be ignored,
	 so put it in the pane title.  */
      if (menus[0] == 0)
	menus[0] = title;
    }
  else
    {
      /* We were given an old-fashioned menu.  */
      ltitle = Fcar (menu);
      CHECK_STRING (ltitle, 1);
      title = (char *) XSTRING (ltitle)->data;
      prefixes = 0;
      number_of_panes = list_of_panes (&obj_list, &menus, &names, &enables,
				       &items, Fcdr (menu));
    }
#ifdef XDEBUG
  fprintf (stderr, "Panes = %d\n", number_of_panes);
  for (i = 0; i < number_of_panes; i++)
    {
      fprintf (stderr, "Pane %d has lines %d title %s\n",
	       i, items[i], menus[i]);
      for (j = 0; j < items[i]; j++)
	fprintf (stderr, "    Item %d %s\n", j, names[i][j]);
    }
#endif
  BLOCK_INPUT;
  {
    Window root;
    int root_x, root_y;
    int dummy_int;
    unsigned int dummy_uint;
    Window dummy_window;

    /* Figure out which root window F is on.  */
    XGetGeometry (x_current_display, FRAME_X_WINDOW (f), &root,
		  &dummy_int, &dummy_int, &dummy_uint, &dummy_uint,
		  &dummy_uint, &dummy_uint);

    /* Translate the menu co-ordinates within f to menu co-ordinates
       on that root window.  */
    if (! XTranslateCoordinates (x_current_display,
				 FRAME_X_WINDOW (f), root,
				 XMenu_xpos, XMenu_ypos, &root_x, &root_y,
				 &dummy_window))
      /* But XGetGeometry said root was the root window of f's screen!  */ 
      abort ();

    selection = xmenu_show (root, XMenu_xpos, XMenu_ypos, names, enables,
			    menus, prefixes, items, number_of_panes, obj_list,
			    title, &error_name);
  }
  UNBLOCK_INPUT;
  /* fprintf (stderr, "selection = %x\n", selection);  */
  if (selection != NUL)
    {				/* selected something */
      XMenu_return = selection;
    }
  else
    {				/* nothing selected */
      XMenu_return = Qnil;
    }
  /* now free up the strings */
  for (i = 0; i < number_of_panes; i++)
    {
      xfree (names[i]);
      xfree (enables[i]);
      xfree (obj_list[i]);
    }
  xfree (menus);
  xfree (obj_list);
  xfree (names);
  xfree (enables);
  xfree (items);
  /* free (title); */
  if (error_name) error (error_name);
  return XMenu_return;
}

struct indices {
  int pane;
  int line;
};

Lisp_Object
xmenu_show (parent, startx, starty, line_list, enable_list, pane_list,
	    prefixes, line_cnt, pane_cnt, item_list, title, error)
     Window parent;		
     int startx, starty;	/* upper left corner position BROKEN */
     char **line_list[];   	/* list of strings for items */
     int *enable_list[];   	/* list of strings for items */
     char *pane_list[];		/* list of pane titles */
     Lisp_Object *prefixes;	/* Prefix key for each pane */
     char *title;
     int pane_cnt;		/* total number of panes */
     Lisp_Object *item_list[];	/* All items */
     int line_cnt[];		/* Lines in each pane */
     char **error;		/* Error returned */
{
  XMenu *GXMenu;
  int last, panes, selidx, lpane, status;
  int lines, sofar;
  Lisp_Object entry;
  /* struct indices *datap, *datap_save; */
  char *datap;
  int ulx, uly, width, height;
  int dispwidth, dispheight;

  *error = 0;
  if (pane_cnt == 0)
    return 0;

  BLOCK_INPUT;
  *error = (char *) 0;		/* Initialize error pointer to null */
  GXMenu = XMenuCreate (XDISPLAY parent, "emacs");
  if (GXMenu == NUL)
    {
      *error = "Can't create menu";
      UNBLOCK_INPUT;
      return (0);
    }
  
  for (panes = 0, lines = 0; panes < pane_cnt;
       lines += line_cnt[panes], panes++)
    ;
  /* datap = (struct indices *) xmalloc (lines * sizeof (struct indices)); */
  /* datap = (char *) xmalloc (lines * sizeof (char));
    datap_save = datap;*/
  
  for (panes = 0, sofar = 0; panes < pane_cnt;
       sofar += line_cnt[panes], panes++)
    {
      /* create all the necessary panes */
      lpane = XMenuAddPane (XDISPLAY GXMenu, pane_list[panes], TRUE);
      if (lpane == XM_FAILURE)
	{
	  XMenuDestroy (XDISPLAY GXMenu);
	  *error = "Can't create pane";
	  UNBLOCK_INPUT;
	  return (0);
	}
      for (selidx = 0; selidx < line_cnt[panes]; selidx++)
	{
	  /* add the selection stuff to the menus */
	  /* datap[selidx+sofar].pane = panes;
	     datap[selidx+sofar].line = selidx; */
	  if (XMenuAddSelection (XDISPLAY GXMenu, lpane, 0,
				 line_list[panes][selidx],
				 enable_list[panes][selidx])
	      == XM_FAILURE)
	    {
	      XMenuDestroy (XDISPLAY GXMenu);
	      /* free (datap); */
	      *error = "Can't add selection to menu";
	      /* error ("Can't add selection to menu"); */
	      UNBLOCK_INPUT;
	      return (0);
	    }
	}
    }
  /* all set and ready to fly */
  XMenuRecompute (XDISPLAY GXMenu);
  dispwidth = DisplayWidth (x_current_display, XDefaultScreen (x_current_display));
  dispheight = DisplayHeight (x_current_display, XDefaultScreen (x_current_display));
  startx = min (startx, dispwidth);
  starty = min (starty, dispheight);
  startx = max (startx, 1);
  starty = max (starty, 1);
  XMenuLocate (XDISPLAY GXMenu, 0, 0, startx, starty,
	       &ulx, &uly, &width, &height);
  if (ulx+width > dispwidth)
    {
      startx -= (ulx + width) - dispwidth;
      ulx = dispwidth - width;
    }
  if (uly+height > dispheight)
    {
      starty -= (uly + height) - dispheight;
      uly = dispheight - height;
    }
  if (ulx < 0) startx -= ulx;
  if (uly < 0) starty -= uly;
    
  XMenuSetFreeze (GXMenu, TRUE);
  panes = selidx = 0;
  
  status = XMenuActivate (XDISPLAY GXMenu, &panes, &selidx,
			  startx, starty, ButtonReleaseMask, &datap);
  switch (status)
    {
    case XM_SUCCESS:
#ifdef XDEBUG
      fprintf (stderr, "pane= %d line = %d\n", panes, selidx);
#endif
      entry = item_list[panes][selidx];
      if (prefixes != 0)
	{
	  entry = Fcons (entry, Qnil);
	  if (!NILP (prefixes[panes]))
	    entry = Fcons (prefixes[panes], entry);
	}
      break;
    case XM_FAILURE:
      /* free (datap_save); */
      XMenuDestroy (XDISPLAY GXMenu);
      *error = "Can't activate menu";
      /* error ("Can't activate menu"); */
    case XM_IA_SELECT:
    case XM_NO_SELECT:
      entry = Qnil;
      break;
    }
  XMenuDestroy (XDISPLAY GXMenu);
  UNBLOCK_INPUT;
  /* free (datap_save);*/
  return (entry);
}

syms_of_xmenu ()
{
  defsubr (&Sx_popup_menu);
}

/* Construct the vectors that describe a menu
   and store them in *VECTOR, *PANES, *NAMES, *ENABLES and *ITEMS.
   Each of those four values is a vector indexed by pane number.
   Return the number of panes.

   KEYMAPS is a vector of keymaps.  NMAPS gives the length of KEYMAPS.  */

int
keymap_panes (vector, panes, names, enables, items, prefixes, keymaps, nmaps)
     Lisp_Object ***vector;	/* RETURN all menu objects */
     char ***panes;		/* RETURN pane names */
     char ****names;		/* RETURN all line names */
     int ***enables;		/* RETURN enable-flags of lines */
     int **items;		/* RETURN number of items per pane */
     Lisp_Object **prefixes;	/* RETURN vector of prefix keys, per pane */
     Lisp_Object *keymaps;
     int nmaps;
{
  /* Number of panes we have made.  */
  int p = 0;
  /* Number of panes we have space for.  */
  int npanes_allocated = nmaps;
  int mapno;

  if (npanes_allocated < 4)
    npanes_allocated = 4;

  /* Make space for an estimated number of panes.  */
  *vector = (Lisp_Object **) xmalloc (npanes_allocated * sizeof (Lisp_Object *));
  *panes = (char **) xmalloc (npanes_allocated * sizeof (char *));
  *items = (int *) xmalloc (npanes_allocated * sizeof (int));
  *names = (char ***) xmalloc (npanes_allocated * sizeof (char **));
  *enables = (int **) xmalloc (npanes_allocated * sizeof (int *));
  *prefixes = (Lisp_Object *) xmalloc (npanes_allocated * sizeof (Lisp_Object));

  /* Loop over the given keymaps, making a pane for each map.
     But don't make a pane that is empty--ignore that map instead.
     P is the number of panes we have made so far.  */
  for (mapno = 0; mapno < nmaps; mapno++)
    single_keymap_panes (keymaps[mapno], panes, vector, names, enables, items,
			 prefixes, &p, &npanes_allocated, "");

  /* Return the number of panes.  */
  return p;
}

/* This is a recursive subroutine of the previous function.
   It handles one keymap, KEYMAP.
   The other arguments are passed along
   or point to local variables of the previous function.  */

single_keymap_panes (keymap, panes, vector, names, enables, items, prefixes,
		     p_ptr, npanes_allocated_ptr, pane_name)
     Lisp_Object keymap;
     Lisp_Object ***vector;	/* RETURN all menu objects */
     char ***panes;		/* RETURN pane names */
     char ****names;		/* RETURN all line names */
     int ***enables;		/* RETURN enable flags of lines */
     int **items;		/* RETURN number of items per pane */
     Lisp_Object **prefixes;	/* RETURN vector of prefix keys, per pane */
     int *p_ptr;
     int *npanes_allocated_ptr;
     char *pane_name;
{
  int i;
  Lisp_Object pending_maps;
  Lisp_Object tail, item, item1, item2, table;

  pending_maps = Qnil;

  /* Make sure we have room for another pane.  */
  if (*p_ptr == *npanes_allocated_ptr)
    {
      *npanes_allocated_ptr *= 2;

      *vector
	= (Lisp_Object **) xrealloc (*vector,
				     *npanes_allocated_ptr * sizeof (Lisp_Object *));
      *panes
	= (char **) xrealloc (*panes,
			      *npanes_allocated_ptr * sizeof (char *));
      *items
	= (int *) xrealloc (*items,
			    *npanes_allocated_ptr * sizeof (int));
      *prefixes
	= (Lisp_Object *) xrealloc (*prefixes,
				    (*npanes_allocated_ptr
				     * sizeof (Lisp_Object)));
      *names
	= (char ***) xrealloc (*names,
			       *npanes_allocated_ptr * sizeof (char **));
      *enables
	= (int **) xrealloc (*enables,
			     *npanes_allocated_ptr * sizeof (int *));
    }

  /* When a menu comes from keymaps, don't give names to the panes.  */
  (*panes)[*p_ptr] = pane_name;

  /* Normally put nil as pane's prefix key.
     Caller will override this if appropriate.  */
  (*prefixes)[*p_ptr] = Qnil;

  /* Get the length of the list level of the keymap.  */
  i = XFASTINT (Flength (keymap));

  /* Add in lengths of any arrays.  */
  for (tail = keymap; XTYPE (tail) == Lisp_Cons; tail = XCONS (tail)->cdr)
    if (XTYPE (XCONS (tail)->car) == Lisp_Vector)
      i += XVECTOR (XCONS (tail)->car)->size;

  /* Create vectors for the names and values of the items in the pane.
     I is an upper bound for the number of items.  */
  (*vector)[*p_ptr] = (Lisp_Object *) xmalloc (i * sizeof (Lisp_Object));
  (*names)[*p_ptr] = (char **) xmalloc (i * sizeof (char *));
  (*enables)[*p_ptr] = (int *) xmalloc (i * sizeof (int));

  /* I is now the index of the next unused slots.  */
  i = 0;
  for (tail = keymap; XTYPE (tail) == Lisp_Cons; tail = XCONS (tail)->cdr)
    {
      /* Look at each key binding, and if it has a menu string,
	 make a menu item from it.  */
      item = XCONS (tail)->car;
      if (XTYPE (item) == Lisp_Cons)
	{
	  item1 = XCONS (item)->cdr;
	  if (XTYPE (item1) == Lisp_Cons)
	    {
	      item2 = XCONS (item1)->car;
	      if (XTYPE (item2) == Lisp_String)
		{
		  Lisp_Object def, tem;
		  Lisp_Object enabled;

		  def = Fcdr (item1);
		  enabled = Qt;
		  if (XTYPE (def) == Lisp_Symbol)
		    {
		      /* No property, or nil, means enable.
			 Otherwise, enable if value is not nil.  */
		      tem = Fget (def, Qmenu_enable);
		      if (!NILP (tem))
			enabled = Feval (tem);
		    }
		  tem = Fkeymapp (def);
		  if (XSTRING (item2)->data[0] == '@' && !NILP (tem))
		    pending_maps = Fcons (Fcons (def, Fcons (item2, XCONS (item)->car)),
					  pending_maps);
		  else
		    {
		      (*names)[*p_ptr][i] = (char *) XSTRING (item2)->data;
		      /* The menu item "value" is the key bound here.  */
		      (*vector)[*p_ptr][i] = XCONS (item)->car;
		      (*enables)[*p_ptr][i]
			= (NILP (def) ? -1 : !NILP (enabled) ? 1 : 0);
		      i++;
		    }
		}
	    }
	}
      else if (XTYPE (item) == Lisp_Vector)
	{
	  /* Loop over the char values represented in the vector.  */
	  int len = XVECTOR (item)->size;
	  int c;
	  for (c = 0; c < len; c++)
	    {
	      Lisp_Object character;
	      XFASTINT (character) = c;
	      item1 = XVECTOR (item)->contents[c];
	      if (XTYPE (item1) == Lisp_Cons)
		{
		  item2 = XCONS (item1)->car;
		  if (XTYPE (item2) == Lisp_String)
		    {
		      Lisp_Object tem;
		      Lisp_Object def;
		      Lisp_Object enabled;

		      def = Fcdr (item1);
		      enabled = Qt;
		      if (XTYPE (def) == Lisp_Symbol)
			{
			  tem = Fget (def, Qmenu_enable);
			  /* No property, or nil, means enable.
			     Otherwise, enable if value is not nil.  */
			  if (!NILP (tem))
			    enabled = Feval (tem);
			}

		      tem = Fkeymapp (def);
		      if (XSTRING (item2)->data[0] == '@' && !NILP (tem))
			pending_maps = Fcons (Fcons (def, Fcons (item2, character)),
					      pending_maps);
		      else
			{
			  (*names)[*p_ptr][i] = (char *) XSTRING (item2)->data;
			  /* The menu item "value" is the key bound here.  */
			  (*vector)[*p_ptr][i] = character;
			  (*enables)[*p_ptr][i]
			    = (NILP (def) ? -1 : !NILP (enabled) ? 1 : 0);
			  i++;
			}
		    }
		}
	    }
	}
    }
  /* Record the number of items in the pane.  */
  (*items)[*p_ptr] = i;

  /* If we just made an empty pane, get rid of it.  */
  if (i == 0)
    {
      xfree ((*vector)[*p_ptr]);
      xfree ((*names)[*p_ptr]);
      xfree ((*enables)[*p_ptr]);
    }
  /* Otherwise, advance past it.  */
  else
    (*p_ptr)++;

  /* Process now any submenus which want to be panes at this level.  */
  while (!NILP (pending_maps))
    {
      Lisp_Object elt, eltcdr;
      int panenum = *p_ptr;
      elt = Fcar (pending_maps);
      eltcdr = XCONS (elt)->cdr;
      single_keymap_panes (Fcar (elt), panes, vector, names, enables, items,
			   prefixes, p_ptr, npanes_allocated_ptr,
			   /* Add 1 to discard the @.  */
			   (char *) XSTRING (XCONS (eltcdr)->car)->data + 1);
      (*prefixes)[panenum] = XCONS (eltcdr)->cdr;
      pending_maps = Fcdr (pending_maps);
    }
}

/* Construct the vectors that describe a menu
   and store them in *VECTOR, *PANES, *NAMES, *ENABLES and *ITEMS.
   Each of those four values is a vector indexed by pane number.
   Return the number of panes.

   MENU is the argument that was given to Fx_popup_menu.  */

int
list_of_panes (vector, panes, names, enables, items, menu)
     Lisp_Object ***vector;	/* RETURN all menu objects */
     char ***panes;		/* RETURN pane names */
     char ****names;		/* RETURN all line names */
     int ***enables;		/* RETURN enable flags of lines */
     int **items;		/* RETURN number of items per pane */
     Lisp_Object menu;
{
  Lisp_Object tail, item, item1;
  int i;
  
  if (XTYPE (menu) != Lisp_Cons) menu = wrong_type_argument (Qlistp, menu);

  i = XFASTINT (Flength (menu));

  *vector = (Lisp_Object **) xmalloc (i * sizeof (Lisp_Object *));
  *panes = (char **) xmalloc (i * sizeof (char *));
  *items = (int *) xmalloc (i * sizeof (int));
  *names = (char ***) xmalloc (i * sizeof (char **));
  *enables = (int **) xmalloc (i * sizeof (int *));

  for (i = 0, tail = menu; !NILP (tail); tail = Fcdr (tail), i++)
    {
      item = Fcdr (Fcar (tail));
      if (XTYPE (item) != Lisp_Cons) (void) wrong_type_argument (Qlistp, item);
#ifdef XDEBUG
      fprintf (stderr, "list_of_panes check tail, i=%d\n", i);
#endif
      item1 = Fcar (Fcar (tail));
      CHECK_STRING (item1, 1);
#ifdef XDEBUG
      fprintf (stderr, "list_of_panes check pane, i=%d%s\n", i,
	       XSTRING (item1)->data);
#endif
      (*panes)[i] = (char *) XSTRING (item1)->data;
      (*items)[i] = list_of_items ((*vector)+i, (*names)+i, (*enables)+i, item);
      /* (*panes)[i] = (char *) xmalloc ((XSTRING (item1)->size)+1);
	 bcopy (XSTRING (item1)->data, (*panes)[i], XSTRING (item1)->size + 1)
	 ; */
    }
  return i;
}

/* Construct the lists of values and names for a single pane, from the
   alist PANE.  Put them in *VECTOR and *NAMES.  Put the enable flags
   int *ENABLES.   Return the number of items.  */

int
list_of_items (vector, names, enables, pane)
     Lisp_Object **vector;	/* RETURN menu "objects" */
     char ***names;		/* RETURN line names */
     int **enables;		/* RETURN enable flags of lines */
     Lisp_Object pane;
{
  Lisp_Object tail, item, item1;
  int i;

  if (XTYPE (pane) != Lisp_Cons) pane = wrong_type_argument (Qlistp, pane);

  i = XFASTINT (Flength (pane));

  *vector = (Lisp_Object *) xmalloc (i * sizeof (Lisp_Object));
  *names = (char **) xmalloc (i * sizeof (char *));
  *enables = (int *) xmalloc (i * sizeof (int));

  for (i = 0, tail = pane; !NILP (tail); tail = Fcdr (tail), i++)
    {
      item = Fcar (tail);
      if (STRINGP (item))
	{
	  (*vector)[i] = Qnil;
	  (*names)[i] = (char *) XSTRING (item)->data;
	  (*enables)[i] = -1;
	}
      else
	{
	  CHECK_CONS (item, 0);
	  (*vector)[i] = Fcdr (item);
	  item1 = Fcar (item);
	  CHECK_STRING (item1, 1);
	  (*names)[i] = (char *) XSTRING (item1)->data;
	  (*enables)[i] = 1;
	}
    }
  return i;
}
