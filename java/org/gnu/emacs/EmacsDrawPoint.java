/* Communication module for Android terminals.  -*- c-file-style: "GNU" -*-

Copyright (C) 2023-2026 Free Software Foundation, Inc.

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

package org.gnu.emacs;

public final class EmacsDrawPoint
{
  public static void
  perform (EmacsDrawable drawable,
	   EmacsGC immutableGC, int x, int y)
  {
    /* Use EmacsFillRectangle instead of EmacsDrawRectangle, as the
       latter actually draws a rectangle one pixel wider than
       specified.  */
    EmacsFillRectangle.perform (drawable, immutableGC,
				x, y, 1, 1);
  }
}
