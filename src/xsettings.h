/* Functions for handle font changes dynamically.
   Copyright (C) 2009, 2010, 2011, 2012
                 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#ifndef XSETTINGS_H
#define XSETTINGS_H

EXFUN (Ffont_get_system_font, 0);
extern void xsettings_initialize P_ ((struct x_display_info *dpyinfo));
extern void xft_settings_event P_ ((struct x_display_info *dpyinfo,
                                    XEvent *));
extern const char *xsettings_get_system_font P_ ((void));


#endif /* XSETTINGS_H */

/* arch-tag: 2c0f5c49-e925-4452-b778-4c082da6dd72
   (do not change this comment) */
