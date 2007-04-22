/*
Copyright (C) 1992 Lucid, Inc.
Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006,
  2007 Free Software Foundation, Inc.

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
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */


#ifndef LWLIB_INTERNAL_H
#define LWLIB_INTERNAL_H

#include "lwlib.h"

extern char *safe_strdup __P ((const char *));

typedef struct _widget_instance
{
  Widget		widget;
  Widget		parent;
  Boolean		pop_up_p;
  struct _widget_info*		info;
  struct _widget_instance*	next;
} widget_instance;

typedef struct _widget_info
{
  char*			type;
  char*			name;
  LWLIB_ID		id;
  widget_value*		val;
  Boolean		busy;
  lw_callback		pre_activate_cb;
  lw_callback		selection_cb;
  lw_callback		post_activate_cb;
  lw_callback		highlight_cb;
  struct _widget_instance*	instances;
  struct _widget_info*		next;
} widget_info;

typedef Widget
(*widget_creation_function) ();

typedef struct _widget_creation_entry
{
  char*				type;
  widget_creation_function	function;
} widget_creation_entry;

/* update all other instances of a widget.  Can be used in a callback when
   a widget has been used by the user */
void
lw_internal_update_other_instances __P ((Widget, XtPointer, XtPointer));

/* get the widget_value for a widget in a given instance */
widget_value*
lw_get_widget_value_for_widget __P ((widget_instance *, Widget));

widget_info *lw_get_widget_info __P ((LWLIB_ID));
widget_instance * lw_get_widget_instance __P ((Widget));

#endif /* LWLIB_INTERNAL_H */

/* arch-tag: ae02f67d-ef25-421c-b956-b01a4b0aac76
   (do not change this comment) */
