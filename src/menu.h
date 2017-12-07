/* Functions to manipulate menus.
   Copyright (C) 2008-2017 Free Software Foundation, Inc.

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

#ifndef MENU_H
#define MENU_H

#include "../lwlib/lwlib-widget.h"

/* Bit fields used by terminal-specific menu_show_hook.  */

enum {
  MENU_KEYMAPS = 0x1,
  MENU_FOR_CLICK = 0x2,
  MENU_KBD_NAVIGATION = 0x4
};

extern void init_menu_items (void);
extern void finish_menu_items (void) ATTRIBUTE_CONST;
extern void discard_menu_items (void);
extern void save_menu_items (void);
extern bool parse_single_submenu (Lisp_Object, Lisp_Object, Lisp_Object);
extern void list_of_panes (Lisp_Object);
#if defined (USE_X_TOOLKIT) || defined (USE_GTK) || defined (HAVE_NTGUI) \
  || defined (HAVE_NS)
extern void free_menubar_widget_value_tree (widget_value *);
extern void update_submenu_strings (widget_value *);
extern void find_and_call_menu_selection (struct frame *, int,
                                          Lisp_Object, void *);
extern widget_value *make_widget_value (const char *, char *, bool, Lisp_Object);
extern widget_value *digest_single_submenu (int, int, bool);
#endif

#if defined (HAVE_X_WINDOWS) || defined (MSDOS)
extern Lisp_Object x_menu_show (struct frame *, int, int, int,
				Lisp_Object, const char **);
#endif
#ifdef HAVE_NTGUI
extern Lisp_Object w32_menu_show (struct frame *, int, int, int,
				  Lisp_Object, const char **);
#endif
#ifdef HAVE_NS
extern Lisp_Object ns_menu_show (struct frame *, int, int, int,
				 Lisp_Object, const char **);
#endif
extern Lisp_Object tty_menu_show (struct frame *, int, int, int,
				  Lisp_Object, const char **);
extern ptrdiff_t menu_item_width (const unsigned char *);
extern Lisp_Object x_popup_menu_1 (Lisp_Object position, Lisp_Object menu);
#endif /* MENU_H */
