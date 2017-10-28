/* Definitions and headers for selection of pure Gtk+3.
   Copyright (C) 1989, 1993, 2005, 2008-2017 Free Software Foundation,
   Inc.

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


#include "dispextern.h"
#include "frame.h"

#ifdef HAVE_PGTK

#include <gtk/gtk.h>

extern void pgtk_selection_init(void);
void pgtk_selection_lost(GtkWidget *widget, GdkEventSelection *event, gpointer user_data);

#endif	/* HAVE_PGTK */
