/*
Copyright (C) 1992, 1993 Lucid, Inc.
Copyright (C) 1994, 1999-2018 Free Software Foundation, Inc.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

The Lucid Widget Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

/* This part is separate from lwlib.h because it does not need X,
   and thus can be used by non-X code in Emacs proper.  */

#ifndef LWLIB_WIDGET_H
#define LWLIB_WIDGET_H

#include "../src/lisp.h"

typedef enum
{
  NO_CHANGE = 0,
  INVISIBLE_CHANGE = 1,
  VISIBLE_CHANGE = 2,
  STRUCTURAL_CHANGE = 3
} change_type;

enum button_type
{
  BUTTON_TYPE_NONE,
  BUTTON_TYPE_TOGGLE,
  BUTTON_TYPE_RADIO
};

typedef struct _widget_value
{
  /* Name of widget.  */
  Lisp_Object lname;
  char *name;

  /* Value (meaning depend on widget type).  */
  char *value;

  /* Keyboard equivalent. no implications for XtTranslations.  */
  Lisp_Object lkey;
  char *key;

  /* Help string or nil if none.
     GC finds this string through the frame's menu_bar_vector
     or through menu_items.  */
  Lisp_Object help;

  /* True if enabled.  */
  bool enabled;

  /* True if selected.  */
  bool selected;

  /* True if was edited (maintained by get_value).  */
  bool edited;

#ifdef HAVE_NTGUI
  /* True if menu title.  */
  bool title;
#endif

  /* The type of a button.  */
  enum button_type button_type;

  /* Contents of the sub-widgets, also selected slot for checkbox.  */
  struct _widget_value *contents;

  /* Data passed to callback.  */
  void *call_data;

  /* Next one in the list.  */
  struct _widget_value *next;

#ifdef USE_X_TOOLKIT
  /* Type of change (maintained by lw library).  */
  change_type change;

  /* Type of this widget's change, but not counting the other widgets
     found in the `next' field.  */
  change_type this_one_change;

  /* Slot for the toolkit dependent part.  Always initialize to NULL.  */
  void *toolkit_data;

  /* Whether we should free the toolkit data slot when freeing the
     widget_value itself.  */
  bool free_toolkit_data;
#endif

} widget_value;

#endif
