/* X Communication module for terminals which understand the X protocol.
   Copyright (C) 1986, 1988 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
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

/* $Source: /u2/third_party/gnuemacs.chow/src/RCS/xmenu.c,v $
 * $Author: rlk $
 * $Locker:  $
 * $Header: xmenu.c,v 1.6 86/08/26 17:23:26 rlk Exp $
 *
 */

#ifndef lint
static char *rcsid_GXMenu_c = "$Header: xmenu.c,v 1.6 86/08/26 17:23:26 rlk Exp $";
#endif	lint
#ifdef XDEBUG
#include <stdio.h>
#endif

/* On 4.3 this loses if it comes after xterm.h.  */
#include <signal.h>
#include "config.h"
#include "lisp.h"
#include "screen.h"
#include "window.h"

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
#endif TRUE

#ifdef HAVE_X11
extern Display *x_current_display;
#else
#define	ButtonReleaseMask ButtonReleased
#endif /* not HAVE_X11 */

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
ARG is a position specification: a list ((XOFFSET YOFFSET) WINDOW)\n\
where XOFFSET and YOFFSET are positions in characters from the top left\n\
corner of WINDOW's screen.  A mouse-event list will serve for this.\n\
This controls the position of the center of the first line\n\
in the first pane of the menu, not the top left of the menu as a whole.\n\
\n\
MENU is a specifier for a menu.  It is a list of the form\n\
\(TITLE PANE1 PANE2...), and each pane is a list of form\n\
\(TITLE (LINE ITEM)...).  Each line should be a string, and item should\n\
be the return value for that line (i.e. if it is selected.")
       (arg, menu)
     Lisp_Object arg, menu;
{
  int number_of_panes;
  Lisp_Object XMenu_return;
  int XMenu_xpos, XMenu_ypos;
  char **menus;
  char ***names;
  Lisp_Object **obj_list;
  int *items;
  char *title;
  char *error_name;
  Lisp_Object ltitle, selection;
  int i, j;
  SCREEN_PTR s;
  Lisp_Object x, y, window;

  window = Fcar (Fcdr (arg));
  x = Fcar (Fcar (arg));
  y = Fcar (Fcdr (Fcar (arg)));
  CHECK_WINDOW (window, 0);
  CHECK_NUMBER (x, 0);
  CHECK_NUMBER (y, 0);
  s = XSCREEN (WINDOW_SCREEN (XWINDOW (window)));

  XMenu_xpos = FONT_WIDTH (s->display.x->font) * XINT (x);
  XMenu_ypos = FONT_HEIGHT (s->display.x->font) * XINT (y);
  XMenu_xpos += s->display.x->left_pos;
  XMenu_ypos += s->display.x->top_pos;

  ltitle = Fcar (menu);
  CHECK_STRING (ltitle, 1);
  title = (char *) XSTRING (ltitle)->data;
  number_of_panes=list_of_panes (&obj_list, &menus, &names, &items, Fcdr (menu));
#ifdef XDEBUG
  fprintf (stderr, "Panes= %d\n", number_of_panes);
  for (i=0; i < number_of_panes; i++)
    {
      fprintf (stderr, "Pane %d lines %d title %s\n", i, items[i], menus[i]);
      for (j=0; j < items[i]; j++)
	{
	  fprintf (stderr, "    Item %d %s\n", j, names[i][j]);
	}
    }
#endif
  BLOCK_INPUT;
  selection = xmenu_show (ROOT_WINDOW, XMenu_xpos, XMenu_ypos, names, menus,
			  items, number_of_panes, obj_list, title, &error_name);
  UNBLOCK_INPUT;
  /** fprintf (stderr, "selection = %x\n", selection);  **/
  if (selection != NUL)
    {				/* selected something */
      XMenu_return = selection;
    }
  else
    {				/* nothing selected */
      XMenu_return = Qnil;
    }
  /* now free up the strings */
  for (i=0; i < number_of_panes; i++)
    {
      free (names[i]);
      free (obj_list[i]);
    }
  free (menus);
  free (obj_list);
  free (names);
  free (items);
  /*   free (title); */
  if (error_name) error (error_name);
  return XMenu_return;
}

struct indices {
  int pane;
  int line;
};

Lisp_Object
xmenu_show (parent, startx, starty, line_list, pane_list, line_cnt,
		      pane_cnt, item_list, title, error)
     Window parent;		
     int startx, starty;	/* upper left corner position BROKEN */
     char **line_list[];   	/* list of strings for items */
     char *pane_list[];		/* list of pane titles */
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
  
  *error = (char *) 0;		/* Initialize error pointer to null */
  GXMenu = XMenuCreate (XDISPLAY parent, "emacs");
  if (GXMenu == NUL)
    {
      *error = "Can't create menu";
      return (0);
    }
  
  for (panes=0, lines=0; panes < pane_cnt; lines += line_cnt[panes], panes++)
    ;
  /* datap = (struct indices *) xmalloc (lines * sizeof (struct indices)); */
  /*datap = (char *) xmalloc (lines * sizeof (char));
    datap_save = datap;*/
  
  for (panes = 0, sofar=0;panes < pane_cnt;sofar +=line_cnt[panes], panes++)
    {
      /* create all the necessary panes */
      lpane = XMenuAddPane (XDISPLAY GXMenu, pane_list[panes], TRUE);
      if (lpane == XM_FAILURE)
	{
	  XMenuDestroy (XDISPLAY GXMenu);
	  *error = "Can't create pane";
	  return (0);
	}
      for (selidx = 0; selidx < line_cnt[panes] ; selidx++)
	{
	  /* add the selection stuff to the menus */
	  /* datap[selidx+sofar].pane = panes;
	     datap[selidx+sofar].line = selidx; */
	  if (XMenuAddSelection (XDISPLAY GXMenu, lpane, 0,
				 line_list[panes][selidx], TRUE)
	      == XM_FAILURE)
	    {
	      XMenuDestroy (XDISPLAY GXMenu);
	      /* free (datap); */
	      *error = "Can't add selection to menu";
	      /* error ("Can't add selection to menu"); */
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
      break;
    case XM_FAILURE:
      /*free (datap_save); */
      XMenuDestroy (XDISPLAY GXMenu);
      *error = "Can't activate menu";
      /* error ("Can't activate menu"); */
    case XM_IA_SELECT:
    case XM_NO_SELECT:
      entry = Qnil;
      break;
    }
  XMenuDestroy (XDISPLAY GXMenu);
  /*free (datap_save);*/
  return (entry);
}

syms_of_xmenu ()
{
  defsubr (&Sx_popup_menu);
}

list_of_panes (vector, panes, names, items, menu)
     Lisp_Object ***vector;	/* RETURN all menu objects */
     char ***panes;		/* RETURN pane names */
     char ****names;		/* RETURN all line names */
     int **items;		/* RETURN number of items per pane */
     Lisp_Object menu;
{
  Lisp_Object tail, item, item1;
  int i;
  
  if (XTYPE (menu) != Lisp_Cons) menu = wrong_type_argument (Qlistp, menu);

  i= XFASTINT (Flength (menu, 1));

  *vector = (Lisp_Object **) xmalloc (i * sizeof (Lisp_Object *));
  *panes = (char **) xmalloc (i * sizeof (char *));
  *items = (int *) xmalloc (i * sizeof (int));
  *names = (char ***) xmalloc (i * sizeof (char **));

  for (i=0, tail = menu; !NILP (tail); tail = Fcdr (tail), i++)
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
       (*items)[i] = list_of_items ((*vector)+i, (*names)+i, item);
       /* (*panes)[i] = (char *) xmalloc ((XSTRING (item1)->size)+1);
	  bcopy (XSTRING (item1)->data, (*panes)[i], XSTRING (item1)->size + 1)
	  ; */
    }
  return i;
}
     

list_of_items (vector, names, pane)  /* get list from emacs and put to vector */
     Lisp_Object **vector;	/* RETURN menu "objects" */
     char ***names;		/* RETURN line names */
     Lisp_Object pane;
{
  Lisp_Object tail, item, item1;
  int i;

  if (XTYPE (pane) != Lisp_Cons) pane = wrong_type_argument (Qlistp, pane);

  i= XFASTINT (Flength (pane, 1));

  *vector = (Lisp_Object *) xmalloc (i * sizeof (Lisp_Object));
  *names = (char **) xmalloc (i * sizeof (char *));

  for (i=0, tail = pane; !NILP (tail); tail = Fcdr (tail), i++)
    {
       item = Fcar (tail);
       if (XTYPE (item) != Lisp_Cons) (void) wrong_type_argument (Qlistp, item);
#ifdef XDEBUG
       fprintf (stderr, "list_of_items check tail, i=%d\n", i);
#endif
       (*vector)[i] =  Fcdr (item);
       item1 = Fcar (item);
       CHECK_STRING (item1, 1);
#ifdef XDEBUG
       fprintf (stderr, "list_of_items check item, i=%d%s\n", i,
		XSTRING (item1)->data);
#endif
       (*names)[i] = (char *) XSTRING (item1)->data;
    }
  return i;
}
