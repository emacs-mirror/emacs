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

/* Modified by Fred Pierresteguy on December 93
   to make the popup menus and menubar use the Xt.  */

#include <stdio.h>

/* On 4.3 this loses if it comes after xterm.h.  */
#include <signal.h>
#include <config.h>
#include "lisp.h"
#include "termhooks.h"
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

#ifdef USE_X_TOOLKIT
#include <X11/Xlib.h>
#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Paned.h>
#include "../lwlib/lwlib.h"
#include "../lwlib/xlwmenuP.h"
#endif /* USE_X_TOOLKIT */

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
extern Lisp_Object Qmenu_bar;
Lisp_Object xmenu_show ();
extern int x_error_handler ();
#ifdef USE_X_TOOLKIT
static widget_value *set_menu_items ();
static int string_width ();
static void free_menu_items ();
#endif

/* we need a unique id for each popup menu and dialog box */
unsigned int popup_id_tick;

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
  int number_of_panes, panes;
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
  int i, j, menubarp = 0;
  FRAME_PTR f;
  Lisp_Object x, y, window;
#ifdef USE_X_TOOLKIT
  widget_value *val, *vw = 0;
#endif /* USE_X_TOOLKIT */

  check_x ();
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
      
      XMenu_xpos = FONT_WIDTH (f->display.x->font) 
	* XWINDOW (window)->left;
      XMenu_ypos = FONT_HEIGHT (f->display.x->font) 
	* XWINDOW (window)->top;
    }
  else
    /* ??? Not really clean; should be CHECK_WINDOW_OR_FRAME,
       but I don't want to make one now.  */
    CHECK_WINDOW (window, 0);

#ifdef USE_X_TOOLKIT
  tem = Fcar (Fcdr (Fcar (Fcdr (position))));
  if (XTYPE (Fcar (position)) != Lisp_Cons
      && CONSP (tem)
      && EQ (Fcar (tem), Qmenu_bar))
    {
      /* We are in the menubar */
      XlwMenuWidget mw;
      int w1 = 0, w2;
    
      mw = (XlwMenuWidget)f->display.x->menubar_widget;
      menubarp = 1;
      for (vw = mw->menu.old_stack [0]->contents; vw; vw = vw->next)
	{
	  w2 = w1;
	  w1 += string_width (mw, vw->name) 
	    + 2 * (mw->menu.horizontal_spacing + 
		   mw->menu.shadow_thickness);
	  if (XINT (x) < w1)
	    {
	      XMenu_xpos = w2 + 4;
	      XMenu_ypos = 0;
	      break;
	    }
	}
    }
  else
    {
      XMenu_xpos += FONT_WIDTH (f->display.x->font) * XINT (x);
      XMenu_ypos += FONT_HEIGHT (f->display.x->font) * XINT (y);
    }

  BLOCK_INPUT;
  XMenu_xpos += (f->display.x->widget->core.x
		 + f->display.x->widget->core.border_width);
  XMenu_ypos += (f->display.x->widget->core.y 
		 + f->display.x->widget->core.border_width
		 + f->display.x->menubar_widget->core.height);
  UNBLOCK_INPUT;

  val = set_menu_items (menu, &prefixes, &panes, &names, 
			&enables, &menus, &items, &number_of_panes, &obj_list, 
			&title, &error_name);
  selection = xmenu_show (f, val, XMenu_xpos, XMenu_ypos,
			  menubarp, vw);

  free_menu_items (names, enables, menus, items, number_of_panes, obj_list, 
		   title, error_name);

  if (selection != NUL)
    {				/* selected something */
      XMenu_return = selection;
    }
  else
    {				/* nothing selected */
      XMenu_return = Qnil;
    }

  return XMenu_return;

#else /* not USE_X_TOOLKIT */
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
#endif /* HAVE_X11 */

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
#endif /* not USE_X_TOOLKIT */
}

#ifdef USE_X_TOOLKIT

static void
dispatch_dummy_expose (w, x, y)
     Widget w;
     int x;
     int y;
{
  XExposeEvent dummy;
	
  dummy.type = Expose;
  dummy.window = XtWindow (w);
  dummy.count = 0;
  dummy.serial = 0;
  dummy.send_event = 0;
  dummy.display = XtDisplay (w);
  dummy.x = x;
  dummy.y = y;

  XtDispatchEvent (&dummy);
}

static int
string_width (mw, s)
     XlwMenuWidget mw;
     char* s;
{
  XCharStruct xcs;
  int drop;
  
  XTextExtents (mw->menu.font, s, strlen (s), &drop, &drop, &drop, &xcs);
  return xcs.width;
}

static int
event_is_in_menu_item (mw, event, name, string_w)
     XlwMenuWidget mw;
     struct input_event *event;
     char *name;
     int *string_w;
{
  *string_w += string_width (mw, name) 
    + 2 * (mw->menu.horizontal_spacing + mw->menu.shadow_thickness);
  return (XINT (event->x) < *string_w);
}


Lisp_Object
map_event_to_object (event, f)
     struct input_event *event;
     FRAME_PTR f;
{
  int i,j, string_w;
  window_state*	ws;
  XlwMenuWidget mw = (XlwMenuWidget) f->display.x->menubar_widget;
  widget_value *val;


  string_w = 0;
  /* Find the window */
  for (val = mw->menu.old_stack [0]->contents; val; val = val->next)
    {
      ws = &mw->menu.windows [0];
      if (ws && event_is_in_menu_item (mw, event, val->name, &string_w))
	{
	  Lisp_Object items;
	  items = FRAME_MENU_BAR_ITEMS (f);
	  for (; CONSP (items); items = XCONS (items)->cdr)
	    if (!strcmp (val->name,
			XSTRING (Fcar (Fcdr (Fcar (items))))->data))
	      return items;
	}
    }
  return Qnil;
}

static widget_value *
set_menu_items (menu, prefixes, panes, names, enables, menus,
		items, number_of_panes, obj_list, title, error_name)
     Lisp_Object menu;
     Lisp_Object **prefixes;
     int *panes;
     char ***names[];
     int ***enables;
     char ***menus;
     int **items;
     int *number_of_panes;
     Lisp_Object ***obj_list;
     char **title;
     char **error_name;
{
  Lisp_Object keymap, tem;
  Lisp_Object ltitle, selection;
  int i, j;
  widget_value *wv, *save_wv = 0, *first_wv = 0, *prev_wv = 0;
  int last, selidx, lpane, status;
  int lines, sofar;

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
	*title = (char *) XSTRING (prompt)->data;

      /* Extract the detailed info to make one pane.  */
      *number_of_panes = keymap_panes (obj_list, menus, names, enables,
				      items, prefixes, menu, 1);
      /* The menu title seems to be ignored,
	 so put it in the pane title.  */
      if ((*menus)[0] == 0)
	(*menus)[0] = *title;
    }
  else if (!NILP (tem))
    {
      /* We were given a list of keymaps.  */
      Lisp_Object prompt;
      int nmaps = XFASTINT (Flength (menu));
      Lisp_Object *maps
	= (Lisp_Object *) alloca (nmaps * sizeof (Lisp_Object));
      int i;
      *title = 0;

      /* The first keymap that has a prompt string
	 supplies the menu title.  */
      for (tem = menu, i = 0; XTYPE (tem) == Lisp_Cons; tem = Fcdr (tem))
	{
	  maps[i++] = keymap = get_keymap (Fcar (tem));

	  prompt = map_prompt (keymap);
	  if (*title == 0 && !NILP (prompt))
	    *title = (char *) XSTRING (prompt)->data;
	}

      /* Extract the detailed info to make one pane.  */
      *number_of_panes = keymap_panes (obj_list, menus, names, enables,
				      items, prefixes, maps, nmaps);
      /* The menu title seems to be ignored,
	 so put it in the pane title.  */
      if ((*menus)[0] == 0)
	(*menus)[0] = *title;
    }
  else
    {
      /* We were given an old-fashioned menu.  */
      ltitle = Fcar (menu);
      CHECK_STRING (ltitle, 1);
      *title = (char *) XSTRING (ltitle)->data;
      *prefixes = 0;
      *number_of_panes = list_of_panes (obj_list, menus, names, enables,
				       items, Fcdr (menu));
    }

  *error_name = 0;
  if (*number_of_panes == 0)
    return 0;

  *error_name = (char *) 0;		/* Initialize error pointer to null */

  wv = malloc_widget_value ();
  wv->name = "menu";
  wv->value = 0;
  wv->enabled = 1;
  first_wv = wv;
 
  for (*panes = 0, lines = 0; *panes < *number_of_panes;
       lines += (*items)[*panes], (*panes)++)
    ;
  /* datap = (struct indices *) xmalloc (lines * sizeof (struct indices)); */
  /* datap = (char *) xmalloc (lines * sizeof (char));
    datap_save = datap;*/
  
  for (*panes = 0, sofar = 0; *panes < *number_of_panes;
       sofar += (*items)[*panes], (*panes)++)
    {
      if (strcmp((*menus)[*panes], ""))
	{
	  wv = malloc_widget_value ();
	  if (save_wv)
	    save_wv->next = wv;
	  else
	    first_wv->contents = wv;
	  wv->name = (*menus)[*panes];
	  wv->value = 0;
	  wv->enabled = 1;
	}
      prev_wv = 0;
      save_wv = wv;

      for (selidx = 0; selidx < (*items)[*panes]; selidx++)
	{
	  wv = malloc_widget_value ();
	  if (prev_wv) 
	    prev_wv->next = wv;
	  else 
	    save_wv->contents = wv;
	  wv->name = (*names)[*panes][selidx];
	  wv->value = 0;
	  selection = (*obj_list)[*panes][selidx];
	  if (*prefixes != 0)
	    {
	      selection = Fcons (selection, Qnil);
	      if (!NILP ((*prefixes)[*panes]))
		selection = Fcons ((*prefixes)[*panes], selection);
	    }
	  wv->call_data = LISP_TO_VOID(selection);
	  wv->enabled = (*enables)[*panes][selidx];
	  prev_wv = wv;
	}
    }

  return (first_wv);
}

static void
free_menu_items (names, enables, menus, items, number_of_panes, 
		 obj_list, title, error_name)
     char **names[];
     int *enables[];
     char **menus;
     int *items;
     int number_of_panes;
     Lisp_Object **obj_list;
     char *title;
     char *error_name;
{
  int i;
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

}

static Lisp_Object menu_item_selection;

static void
popup_selection_callback (widget, id, client_data)
     Widget widget;
     LWLIB_ID id;
     XtPointer client_data;
{
  VOID_TO_LISP (menu_item_selection, client_data);
}

static void
popup_down_callback (widget, id, client_data)
     Widget widget;
     LWLIB_ID id;
     XtPointer client_data;
{
  BLOCK_INPUT;
  lw_destroy_all_widgets (id);
  UNBLOCK_INPUT;
}

/* This recursively calls free_widget_value() on the tree of widgets.
   It must free all data that was malloc'ed for these widget_values.
   Currently, emacs only allocates new storage for the `key' slot.
   All other slots are pointers into the data of Lisp_Strings, and
   must be left alone.
 */
void
free_menubar_widget_value_tree (wv)
     widget_value *wv;
{
  if (! wv) return;
  if (wv->key) xfree (wv->key);

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

static void
update_one_frame_psheets (f)
     FRAME_PTR f;
{
  struct x_display *x = f->display.x;
  
  int menubar_changed;
  
  menubar_changed = (x->menubar_widget
		     && !XtIsManaged (x->menubar_widget));

  if (! (menubar_changed))
    return;

  BLOCK_INPUT;
  XawPanedSetRefigureMode (x->column_widget, 0);
  
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


  /* Re-manage the text-area widget */
  XtManageChild (x->edit_widget);

  /* and now thrash the sizes */
  XawPanedSetRefigureMode (x->column_widget, 1);
  UNBLOCK_INPUT;
}

void
set_frame_menubar (f)
     FRAME_PTR f;
{
  Widget menubar_widget = f->display.x->menubar_widget;
  int id = (int) f;
  Lisp_Object tail;
  widget_value *wv, *save_wv, *first_wv, *prev_wv = 0;

  BLOCK_INPUT;

  wv = malloc_widget_value ();
  wv->name = "menubar";
  wv->value = 0;
  wv->enabled = 1;
  save_wv = first_wv = wv;


  for (tail = FRAME_MENU_BAR_ITEMS (f); CONSP (tail); tail = XCONS (tail)->cdr)
    {
      Lisp_Object string;

      string = Fcar (Fcdr (Fcar (tail)));

      wv = malloc_widget_value ();
      if (prev_wv) 
	prev_wv->next = wv;
      else 
	save_wv->contents = wv;
      wv->name = XSTRING (string)->data;
      wv->value = 0;
      wv->enabled = 1;
      prev_wv = wv;
    }

  if (menubar_widget)
    lw_modify_all_widgets (id, first_wv, False);
  else
    {
      menubar_widget = lw_create_widget ("menubar", "menubar", 
					 id, first_wv, 
					 f->display.x->column_widget, 
					 0, 0,
					 0, 0);
      f->display.x->menubar_widget = menubar_widget;
      XtVaSetValues (menubar_widget,
		     XtNshowGrip, 0,
		     XtNresizeToPreferred, 1,
		     XtNallowResize, 1,
		     0);
    }
  
  free_menubar_widget_value_tree (first_wv);

  update_one_frame_psheets (f);

  UNBLOCK_INPUT;
}

void
free_frame_menubar (f)
     FRAME_PTR f;
{
  Widget menubar_widget;
  int id;

  menubar_widget = f->display.x->menubar_widget;
  id = (int) f;
  
  if (menubar_widget)
    {
      BLOCK_INPUT;
      lw_destroy_all_widgets (id);
      UNBLOCK_INPUT;
    }
}
#endif /* USE_X_TOOLKIT */

struct indices {
  int pane;
  int line;
};

extern void process_expose_from_menu ();

#ifdef USE_X_TOOLKIT
extern XtAppContext Xt_app_con;

Lisp_Object
xmenu_show (f, val, x, y, menubarp, vw)
     FRAME_PTR f;
     widget_value *val;
     int x;
     int y;
     int menubarp;
     widget_value *vw;
{
  int menu_id, item_length;
  Lisp_Object selection;
  Widget menu;
  XlwMenuWidget menuw = (XlwMenuWidget) f->display.x->menubar_widget;

  /*
   * Define and allocate a foreign event queue to hold events
   * that don't belong to XMenu.  These events are later restored
   * to the X event queue.
   */
  typedef struct _xmeventque 
    {
      XEvent event;
      struct _xmeventque *next;
    } XMEventQue;
  
  XMEventQue *feq = NULL;    		/* Foreign event queue. */
  XMEventQue *feq_tmp;		/* Foreign event queue temporary. */
  
  BLOCK_INPUT;
  if (val == 0) return Qnil;

  menu_id = ++popup_id_tick;
  menu = lw_create_widget ("popup", val->name, menu_id, val, 
			   f->display.x->widget, 1, 0,
			   popup_selection_callback, popup_down_callback);
  free_menubar_widget_value_tree (val);

  /* reset the selection */
  menu_item_selection = Qnil;

  {
    XButtonPressedEvent dummy;
    XlwMenuWidget mw;
    
    mw = ((XlwMenuWidget)
	  ((CompositeWidget)menu)->composite.children[0]);

    dummy.type = ButtonPress;
    dummy.serial = 0;
    dummy.send_event = 0;
    dummy.display = XtDisplay (menu);
    dummy.window = XtWindow (XtParent (menu));
    dummy.time = CurrentTime;
    dummy.button = 0;
    dummy.x_root = x;
    dummy.y_root = y;

    if (menubarp)
      {
	vw->call_data = (XtPointer) 1;
	dispatch_dummy_expose (f->display.x->menubar_widget, x, y);
      }


    /* We activate directly the lucid implementation */
    pop_up_menu (mw, &dummy);
  }

  if (menubarp)
    {
      item_length = (x + string_width (menuw, vw->name) 
		     + (2 * (menuw->menu.horizontal_spacing 
			     + menuw->menu.shadow_thickness))
		     - 4);
    }

  /* Enters XEvent loop */
  while (1)
    {

      XEvent event;
      XtAppNextEvent (Xt_app_con, &event);
      if (event.type == ButtonRelease)
	{
	  XtDispatchEvent (&event);
	  break;
	}
      else
	if (event.type == Expose)
	  process_expose_from_menu (event);
      else 
	if (event.type == MotionNotify 
	    && menubarp
	    && ((event.xmotion.y_root 
		 >= (f->display.x->widget->core.y 
		     + f->display.x->widget->core.border_width))
		&& (event.xmotion.y_root
		    < (f->display.x->widget->core.y
		       + f->display.x->widget->core.border_width
		       + f->display.x->menubar_widget->core.height)))
	    && ((event.xmotion.x_root
		 >= (f->display.x->widget->core.x
		     + f->display.x->widget->core.border_width))
		&& (event.xmotion.x_root
		    < (f->display.x->widget->core.x
		       + f->display.x->widget->core.border_width
		       + f->display.x->widget->core.width)))
	    && (event.xmotion.x_root >= item_length
		|| event.xmotion.x_root < (x - 4)))
	  {
	    BLOCK_INPUT;
	    XtUngrabPointer ((Widget)
			     ((XlwMenuWidget)
			      ((CompositeWidget)menu)->composite.children[0]),
			     event.xbutton.time);
	    lw_destroy_all_widgets (menu_id); 
	    UNBLOCK_INPUT;

	    event.type = ButtonPress;
	        event.xbutton.time = CurrentTime;
	        event.xbutton.button = Button1;
	    event.xbutton.window = XtWindow (f->display.x->menubar_widget);
	    event.xbutton.x = (event.xbutton.x_root 
			       - (f->display.x->widget->core.x
				  + f->display.x->widget->core.border_width));
	    XPutBackEvent (XDISPLAY &event);
	    break;
	  }

      XtDispatchEvent (&event);
      feq_tmp = (XMEventQue *) malloc (sizeof (XMEventQue));

      if (feq_tmp == NULL) 
	return(Qnil);

      feq_tmp->event = event;
      feq_tmp->next = feq;
      feq = feq_tmp;
    }
      
  if (menubarp)
    {
      vw->call_data = (XtPointer) 0;
      dispatch_dummy_expose (f->display.x->menubar_widget, x, y);
    }

  /* Return any foreign events that were queued to the X event queue.  */
  while (feq != NULL) 
    {
      feq_tmp = feq;
      XPutBackEvent (XDISPLAY &feq_tmp->event);
      feq = feq_tmp->next;
      free ((char *)feq_tmp);
    }

  UNBLOCK_INPUT;

  return menu_item_selection;
}

#else /* not USE_X_TOOLKIT */
xmenu_show (parent, startx, starty, line_list, enable_list, pane_list,
	    prefixes, line_cnt, pane_cnt, item_list, title, error)
     Window parent;		
     int startx, starty;	/* upper left corner position BROKEN */
     char **line_list[];   	/* list of strings for items */
     int *enable_list[];   	/* enable flags of lines */
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
#endif /* not USE_X_TOOLKIT */

syms_of_xmenu ()
{
  popup_id_tick = (1<<16);	
  defsubr (&Sx_popup_menu);
}

/* Figure out the current keyboard equivalent of a menu item ITEM1.
   Store the equivalent key sequence in *SAVEDKEY_PTR
   and the textual description (to use in the menu display) in *DESCRIP_PTR.
   Also cache them in the item itself.
   Return the real definition to execute.  */

static Lisp_Object
menu_item_equiv_key (item1, savedkey_ptr, descrip_ptr)
     Lisp_Object item1;
     Lisp_Object *savedkey_ptr, *descrip_ptr;
{
  /* This is what is left after the menu item name.  */
  Lisp_Object overdef;
  /* This is the real definition--the function to run.  */
  Lisp_Object def;
  /* These are the saved equivalent keyboard key sequence
     and its key-description.  */
  Lisp_Object savedkey, descrip;
  Lisp_Object def1;
  int changed = 0;

  overdef = def = Fcdr (item1);

  /* Get out the saved equivalent-keyboard-key info.  */
  savedkey = descrip = Qnil;
  if (CONSP (overdef)
      && (STRINGP (XCONS (overdef)->car)
	  || VECTORP (XCONS (overdef)->car)))
    {
      savedkey = XCONS (overdef)->car;
      def = XCONS (def)->cdr;
      if (CONSP (def)
	  && (STRINGP (XCONS (def)->car)
	      || VECTORP (XCONS (def)->car)))
	{
	  descrip = XCONS (def)->car;
	  def = XCONS (def)->cdr;
	}
    }

  /* Is it still valid?  */
  def1 = Qnil;
  if (!NILP (savedkey))
    def1 = Fkey_binding (savedkey, Qnil);
  /* If not, update it.  */
  if (! EQ (def1, def))
    {
      changed = 1;
      descrip = Qnil;
      savedkey = Fwhere_is_internal (def, Qnil, Qt, Qnil);
      if (VECTORP (savedkey)
	  && EQ (XVECTOR (savedkey)->contents[0], Qmenu_bar))
	savedkey = Qnil;
      if (!NILP (savedkey))
	{
	  descrip = Fkey_description (savedkey);
	  descrip = concat2 (make_string ("  (", 3), descrip);
	  descrip = concat2 (descrip, make_string (")", 1));
	}
    }

  /* Store back the recorded keyboard key sequence
     if we changed it.  */
  if (!NILP (savedkey)
      && CONSP (overdef)
      && (STRINGP (XCONS (overdef)->car)
	  || VECTORP (XCONS (overdef)->car)))
    {
      if (changed)
	{
	  XCONS (overdef)->car = savedkey;
	  def1 = XCONS (overdef)->cdr;
	  if (CONSP (def1)
	      && (STRINGP (XCONS (def1)->car)
		  || VECTORP (XCONS (def1)->car)))
	    XCONS (def1)->car = descrip;
	}
    }
  /* If we had none but need one now, add it.  */
  else if (!NILP (savedkey))
    XCONS (item1)->cdr
      = overdef = Fcons (savedkey, Fcons (descrip, def));
  /* If we had one but no longer should have one,
     delete it.  */
  else if (CONSP (overdef)
	   && (STRINGP (XCONS (overdef)->car)
	       || VECTORP (XCONS (overdef)->car)))
    {
      XCONS (item1)->cdr = overdef = XCONS (overdef)->cdr;
      if (CONSP (overdef)
	  && (STRINGP (XCONS (overdef)->car)
	      || VECTORP (XCONS (overdef)->car)))
	XCONS (item1)->cdr = overdef = XCONS (overdef)->cdr;
    }

  *savedkey_ptr = savedkey;
  *descrip_ptr = descrip;
  return def;
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

/* This is used as the handler when calling internal_condition_case_1.  */

static Lisp_Object
single_keymap_panes_1 (arg)
     Lisp_Object arg;
{
  return Qnil;
}

/* This is a recursive subroutine of keymap_panes.
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
		  /* This is the real definition--the function to run.  */
		  Lisp_Object def;
		  /* These are the saved equivalent keyboard key sequence
		     and its key-description.  */
		  Lisp_Object savedkey, descrip;
		  Lisp_Object tem, enabled;

		  def = menu_item_equiv_key (item1, &savedkey, &descrip);

		  enabled = Qt;
		  if (XTYPE (def) == Lisp_Symbol)
		    {
		      /* No property, or nil, means enable.
			 Otherwise, enable if value is not nil.  */
		      tem = Fget (def, Qmenu_enable);
		      if (!NILP (tem))
			/* (condition-case nil (eval tem)
			   (error nil))  */
			enabled = internal_condition_case_1 (Feval, tem,
							     Qerror,
							     single_keymap_panes_1);
		    }
		  tem = Fkeymapp (def);
		  if (XSTRING (item2)->data[0] == '@' && !NILP (tem))
		    pending_maps = Fcons (Fcons (def, Fcons (item2, XCONS (item)->car)),
					  pending_maps);
		  else
		    {
		      Lisp_Object concat;
		      if (!NILP (descrip))
			concat = concat2 (item2, descrip);
		      else
			concat = item2;
		      (*names)[*p_ptr][i] = (char *) XSTRING (concat)->data;
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
		      Lisp_Object def;

		      /* These are the saved equivalent keyboard key sequence
			 and its key-description.  */
		      Lisp_Object savedkey, descrip;
		      Lisp_Object tem, enabled;

		      def = menu_item_equiv_key (item1, &savedkey, &descrip);

		      enabled = Qt;
		      if (XTYPE (def) == Lisp_Symbol)
			{
			  tem = Fget (def, Qmenu_enable);
			  /* No property, or nil, means enable.
			     Otherwise, enable if value is not nil.  */
			  if (!NILP (tem))
			    /* (condition-case nil (eval tem)
			       (error nil))  */
			    enabled = internal_condition_case_1 (Feval, tem,
								 Qerror,
								 single_keymap_panes_1);
			}

		      tem = Fkeymapp (def);
		      if (XSTRING (item2)->data[0] == '@' && !NILP (tem))
			pending_maps = Fcons (Fcons (def, Fcons (item2, character)),
					      pending_maps);
		      else
			{
			  Lisp_Object concat;
			  if (!NILP (descrip))
			    concat = concat2 (item2, descrip);
			  else
			    concat = item2;
			  (*names)[*p_ptr][i]
			    = (char *) XSTRING (concat)->data;
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
