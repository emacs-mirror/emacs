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

import android.view.PointerIcon;
import android.os.Build;

/* Cursor wrapper.  Note that pointer icons are not supported prior to
   Android 24.  */

public final class EmacsCursor extends EmacsHandleObject
{
  /* The pointer icon associated with this cursor.  */
  public final PointerIcon icon;

  public
  EmacsCursor (int glyph)
  {
    super ();

    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.N)
      {
	icon = null;
	return;
      }

    icon = PointerIcon.getSystemIcon (EmacsService.SERVICE,
				      glyph);
  }
};
