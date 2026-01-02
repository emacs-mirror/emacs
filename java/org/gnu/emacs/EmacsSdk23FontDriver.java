/* Font backend for Android terminals.  -*- c-file-style: "GNU" -*-

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

import android.graphics.Paint;
import android.graphics.Rect;

public final class EmacsSdk23FontDriver extends EmacsSdk7FontDriver
{
  private void
  textExtents1 (Sdk7FontObject font, int code, FontMetrics metrics,
		Paint paint, Rect bounds)
  {
    char[] text;

    text = new char[2];
    text[0] = (char) code;
    text[1] = 'c';

    paint.getTextBounds (text, 0, 1, bounds);

    metrics.lbearing = (short) bounds.left;
    metrics.rbearing = (short) bounds.right;
    metrics.ascent = (short) -bounds.top;
    metrics.descent = (short) bounds.bottom;
    metrics.width
      = (short) paint.getRunAdvance (text, 0, 1, 0, 1, false, 1);
  }

  @Override
  public void
  textExtents (FontObject font, int code[], FontMetrics fontMetrics)
  {
    int i;
    Paint paintCache;
    Rect boundsCache;
    Sdk7FontObject fontObject;
    char[] text;
    float width;

    fontObject = (Sdk7FontObject) font;
    paintCache = fontObject.typeface.typefacePaint;
    paintCache.setTextSize (fontObject.pixelSize);
    boundsCache = new Rect ();

    if (code.length == 0)
      {
	fontMetrics.lbearing = 0;
	fontMetrics.rbearing = 0;
	fontMetrics.ascent = 0;
	fontMetrics.descent = 0;
	fontMetrics.width = 0;
      }
    else if (code.length == 1)
      textExtents1 ((Sdk7FontObject) font, code[0], fontMetrics,
		    paintCache, boundsCache);
    else
      {
	text = new char[code.length + 1];

	for (i = 0; i < code.length; ++i)
	  text[i] = (char) code[i];

	text[code.length] = 'c';

	paintCache.getTextBounds (text, 0, code.length,
				  boundsCache);
	width = paintCache.getRunAdvance (text, 0, code.length, 0,
					  code.length,
					  false, code.length);

	fontMetrics.lbearing = (short) boundsCache.left;
	fontMetrics.rbearing = (short) boundsCache.right;
	fontMetrics.ascent = (short) -boundsCache.top;
	fontMetrics.descent = (short) boundsCache.bottom;
	fontMetrics.width = (short) width;
      }
  }

  @Override
  public int
  hasChar (FontSpec font, int charCode)
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

    /* If the character falls within the confines of the BMP, return
       1.  */
    if (charCode < 65536)
      return paint.hasGlyph (String.valueOf ((char) charCode)) ? 1 : 0;

    /* Otherwise return 0.  */
    return 0;
  }
};
