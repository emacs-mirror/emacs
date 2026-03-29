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

import android.os.Build;

/* This code is mostly unused.  See sfntfont-android.c for the code
   that is actually used.  */

public abstract class EmacsFontDriver
{
  /* Font weights.  */
  public static final int THIN		= 0;
  public static final int ULTRA_LIGHT	= 40;
  public static final int LIGHT		= 50;
  public static final int SEMI_LIGHT	= 55;
  public static final int REGULAR	= 80;
  public static final int MEDIUM	= 100;
  public static final int SEMI_BOLD	= 180;
  public static final int BOLD		= 200;
  public static final int EXTRA_BOLD	= 205;
  public static final int BLACK		= 210;
  public static final int ULTRA_HEAVY	= 250;

  /* Font slants.  */
  public static final int REVERSE_OBLIQUE	= 0;
  public static final int REVERSE_ITALIC	= 10;
  public static final int NORMAL		= 100;
  public static final int ITALIC		= 200;
  public static final int OBLIQUE		= 210;

  /* Font widths.  */
  public static final int ULTRA_CONDENSED	= 50;
  public static final int EXTRA_CONDENSED	= 63;
  public static final int CONDENSED		= 75;
  public static final int SEMI_CONDENSED	= 87;
  public static final int UNSPECIFIED		= 100;
  public static final int SEMI_EXPANDED		= 113;
  public static final int EXPANDED		= 125;
  public static final int EXTRA_EXPANDED	= 150;
  public static final int ULTRA_EXPANDED	= 200;

  /* Font spacings.  */
  public static final int PROPORTIONAL	= 0;
  public static final int DUAL		= 90;
  public static final int MONO		= 100;
  public static final int CHARCELL	= 110;

  /* Special glyph codes.  */
  public static final int FONT_INVALID_CODE = 0xFFFFFFFF;



  public static class FontSpec
  {
    /* The fields below mean the same as they do in enum
       font_property_index in font.h.  */

    public String foundry;
    public String family;
    public String adstyle;
    public String registry;
    public Integer width;
    public Integer weight;
    public Integer slant;
    public Integer size;
    public Integer spacing;
    public Integer avgwidth;
    public Integer dpi;

    @Override
    public String
    toString ()
    {
      return ("foundry: " + foundry
	      + " family: " + family
	      + " adstyle: " + adstyle
	      + " registry: " + registry
	      + " width: " + width
	      + " weight: " + weight
	      + " slant: " + slant
	      + " spacing: " + spacing
	      + " avgwidth: " + avgwidth
	      + " dpi: " + dpi);
    }
  };

  public static class FontMetrics
  {
    public short lbearing;
    public short rbearing;
    public short width;
    public short ascent;
    public short descent;

    @Override
    public String
    toString ()
    {
      return ("lbearing " + lbearing
	      + " rbearing " + rbearing
	      + " width " + width
	      + " ascent " + ascent
	      + " descent " + descent);
    }
  }

  public static class FontEntity extends FontSpec
  {
    /* No extra fields here.  */
  };

  public abstract class FontObject extends FontSpec
  {
    public int minWidth;
    public int maxWidth;
    public int pixelSize;
    public int height;
    public int spaceWidth;
    public int averageWidth;
    public int ascent;
    public int descent;
    public int underlineThickness;
    public int underlinePosition;
    public int baselineOffset;
    public int relativeCompose;
    public int defaultAscent;
    public int encodingCharset;
    public int repertoryCharset;

    public
    FontObject ()
    {
      encodingCharset = -1;
      repertoryCharset = -1;
    }
  };



  /* These mean the same as they do in struct font_driver.  */
  public abstract FontEntity[] list (FontSpec fontSpec);
  public abstract FontEntity match (FontSpec fontSpec);
  public abstract String[] listFamilies ();
  public abstract FontObject openFont (FontEntity fontEntity, int pixelSize);
  public abstract int hasChar (FontSpec font, int charCode);
  public abstract void textExtents (FontObject font, int code[],
				    FontMetrics fontMetrics);
  public abstract int encodeChar (FontObject fontObject, int charCode);
  public abstract int draw (FontObject fontObject, EmacsGC gc,
			    EmacsDrawable drawable, int[] chars,
			    int x, int y, int backgroundWidth,
			    boolean withBackground);

  public static EmacsFontDriver
  createFontDriver ()
  {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M)
      return new EmacsSdk23FontDriver ();

    return new EmacsSdk7FontDriver ();
  }
};
