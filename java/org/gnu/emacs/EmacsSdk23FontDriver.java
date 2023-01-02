/* Font backend for Android terminals.  -*- c-file-style: "GNU" -*-

Copyright (C) 2023 Free Software Foundation, Inc.

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

import android.graphics.Paint;

public class EmacsSdk23FontDriver extends EmacsSdk7FontDriver
{
  @Override
  public int
  hasChar (FontSpec font, char charCode)
  {
    Sdk7FontObject fontObject;
    Paint paint;

    if (font instanceof Sdk7FontObject)
      {
	fontObject = (Sdk7FontObject) font;
	paint = fontObject.typeface.typefacePaint;
      }
    else
      paint = ((Sdk7FontEntity) font).typeface.typefacePaint;

    return paint.hasGlyph (String.valueOf (charCode)) ? 1 : 0;
  }
};
