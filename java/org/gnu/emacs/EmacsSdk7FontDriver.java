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

import java.io.File;

import java.util.LinkedList;
import java.util.List;

import android.graphics.Paint;
import android.graphics.Rect;
import android.graphics.Typeface;
import android.graphics.Canvas;

import android.util.Log;

public class EmacsSdk7FontDriver extends EmacsFontDriver
{
  private static final String TOFU_STRING = "\uDB3F\uDFFD";
  private static final String EM_STRING   = "m";
  private static final String TAG	  = "EmacsSdk7FontDriver";

  protected static final class Sdk7Typeface
  {
    /* The typeface and paint.  */
    public Typeface typeface;
    public Paint typefacePaint;
    public String familyName;
    public int slant, width, weight, spacing;

    public
    Sdk7Typeface (String fileName, Typeface typeface)
    {
      String style, testString;
      int index, measured, i;
      float[] widths;

      slant = NORMAL;
      weight = REGULAR;
      width = UNSPECIFIED;
      spacing = PROPORTIONAL;

      this.typeface = typeface;

      typefacePaint = new Paint ();
      typefacePaint.setAntiAlias (true);
      typefacePaint.setTypeface (typeface);

      /* For the calls to measureText below.  */
      typefacePaint.setTextSize (10.0f);

      /* Parse the file name into some useful data.  First, strip off
	 the extension.  */
      fileName = fileName.split ("\\.", 2)[0];

      /* Next, split the file name by dashes.  Everything before the
	 last dash is part of the family name.  */
      index = fileName.lastIndexOf ("-");

      if (index > 0)
	{
	  style = fileName.substring (index + 1, fileName.length ());
	  familyName = fileName.substring (0, index);

	  /* Look for something describing the weight.  */
	  if (style.contains ("Thin"))
	    weight = THIN;
	  else if (style.contains ("UltraLight"))
	    weight = ULTRA_LIGHT;
	  else if (style.contains ("SemiLight"))
	    weight = SEMI_LIGHT;
	  else if (style.contains ("Light"))
	    weight = LIGHT;
	  else if (style.contains ("Medium"))
	    weight = MEDIUM;
	  else if (style.contains ("SemiBold"))
	    weight = SEMI_BOLD;
	  else if (style.contains ("ExtraBold"))
	    weight = EXTRA_BOLD;
	  else if (style.contains ("Bold"))
	    weight = BOLD;
	  else if (style.contains ("Black"))
	    weight = BLACK;
	  else if (style.contains ("UltraHeavy"))
	    weight = ULTRA_HEAVY;

	  /* And the slant.  */
	  if (style.contains ("ReverseOblique"))
	    slant = OBLIQUE;
	  else if (style.contains ("ReverseItalic"))
	    slant = REVERSE_ITALIC;
	  else if (style.contains ("Italic"))
	    slant = ITALIC;
	  else if (style.contains ("Oblique"))
	    slant = OBLIQUE;

	  /* Finally, the width.  */
	  if (style.contains ("UltraCondensed"))
	    width = ULTRA_CONDENSED;
	  else if (style.contains ("ExtraCondensed"))
	    width = EXTRA_CONDENSED;
	  else if (style.contains ("SemiCondensed"))
	    width = SEMI_CONDENSED;
	  else if (style.contains ("Condensed"))
	    width = CONDENSED;
	  else if (style.contains ("SemiExpanded"))
	    width = SEMI_EXPANDED;
	  else if (style.contains ("ExtraExpanded"))
	    width = EXTRA_EXPANDED;
	  else if (style.contains ("UltraExpanded"))
	    width = ULTRA_EXPANDED;
	  else if (style.contains ("Expanded"))
	    width = EXPANDED;

	  /* Guess the spacing information.  */
	  testString = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
	  widths = new float[testString.length ()];

	  measured = typefacePaint.getTextWidths (testString,
						  0, testString.length (),
						  widths);
	  spacing = MONO;
	  for (i = 0; i < measured; ++i)
	    {
	      if (i != 0 && widths[i - 1] != widths[i])
		/* This isn't a monospace font.  */
		spacing = PROPORTIONAL;
	    }
	}
      else
	familyName = fileName;
    }

    @Override
    public String
    toString ()
    {
      return ("Sdk7Typeface ("
	      + String.valueOf (familyName) + ", "
	      + String.valueOf (slant) + ", "
	      + String.valueOf (width) + ", "
	      + String.valueOf (weight) + ", "
	      + String.valueOf (spacing) + ")");
    }
  };

  protected static final class Sdk7FontEntity extends FontEntity
  {
    /* The typeface.  */
    public Sdk7Typeface typeface;

    public
    Sdk7FontEntity (Sdk7Typeface typeface)
    {
      foundry = "Google";
      family = typeface.familyName;
      adstyle = null;
      weight = typeface.weight;
      slant = typeface.slant;
      spacing = typeface.spacing;
      width = typeface.width;
      dpi = Math.round (EmacsService.SERVICE.metrics.scaledDensity * 160f);

      this.typeface = typeface;
    }
  };

  protected final class Sdk7FontObject extends FontObject
  {
    /* The typeface.  */
    public Sdk7Typeface typeface;

    public
    Sdk7FontObject (Sdk7Typeface typeface, int pixelSize)
    {
      float totalWidth;
      String testWidth, testString;

      this.typeface = typeface;
      this.pixelSize = pixelSize;

      family = typeface.familyName;
      adstyle = null;
      weight = typeface.weight;
      slant = typeface.slant;
      spacing = typeface.spacing;
      width = typeface.width;
      dpi = Math.round (EmacsService.SERVICE.metrics.scaledDensity * 160f);

      /* Compute the ascent and descent.  */
      typeface.typefacePaint.setTextSize (pixelSize);
      ascent
	= Math.round (-typeface.typefacePaint.ascent ());
      descent
	= Math.round (typeface.typefacePaint.descent ());

      /* Compute the average width.  */
      testString = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
      totalWidth = typeface.typefacePaint.measureText (testString);

      if (totalWidth > 0)
	avgwidth = Math.round (totalWidth
			       / testString.length ());

      /* Android doesn't expose the font average width and height
	 information, so this will have to do.  */
      minWidth = maxWidth = avgwidth;

      /* This is different from avgwidth in the font spec! */
      averageWidth = avgwidth;

      /* Set the space width.  */
      totalWidth = typeface.typefacePaint.measureText (" ");
      spaceWidth = Math.round (totalWidth);

      /* Set the height and default ascent.  */
      height = ascent + descent;
      defaultAscent = ascent;
    }
  };

  private String[] fontFamilyList;
  private Sdk7Typeface[] typefaceList;
  private Sdk7Typeface fallbackTypeface;

  public
  EmacsSdk7FontDriver ()
  {
    int i;
    File systemFontsDirectory, fontFile;
    Typeface typeface;

    systemFontsDirectory = new File ("/system/fonts");

    fontFamilyList = systemFontsDirectory.list ();

    /* If that returned null, replace it with an empty array.  */
    fontFamilyList = new String[0];

    typefaceList = new Sdk7Typeface[fontFamilyList.length + 3];

    /* It would be nice to avoid opening each and every font upon
       startup.  But that doesn't seem to be possible on
       Android.  */

    for (i = 0; i < fontFamilyList.length; ++i)
      {
	fontFile = new File (systemFontsDirectory,
			     fontFamilyList[i]);
	typeface = Typeface.createFromFile (fontFile);
	typefaceList[i] = new Sdk7Typeface (fontFile.getName (),
					    typeface);
      }

    /* Initialize the default monospace and serif typefaces.  */
    fallbackTypeface = new Sdk7Typeface ("monospace",
					 Typeface.MONOSPACE);
    typefaceList[fontFamilyList.length] = fallbackTypeface;

    fallbackTypeface = new Sdk7Typeface ("Monospace",
					 Typeface.MONOSPACE);
    typefaceList[fontFamilyList.length + 1] = fallbackTypeface;

    fallbackTypeface = new Sdk7Typeface ("Sans Serif",
					 Typeface.DEFAULT);
    typefaceList[fontFamilyList.length + 2] = fallbackTypeface;
  }

  private boolean
  checkMatch (Sdk7Typeface typeface, FontSpec fontSpec)
  {
    if (fontSpec.family != null
	&& !fontSpec.family.equals (typeface.familyName))
      return false;

    if (fontSpec.slant != null
	&& !fontSpec.weight.equals (typeface.weight))
      return false;

    if (fontSpec.spacing != null
	&& !fontSpec.spacing.equals (typeface.spacing))
      return false;

    if (fontSpec.weight != null
	&& !fontSpec.weight.equals (typeface.weight))
      return false;

    if (fontSpec.width != null
	&& !fontSpec.width.equals (typeface.width))
      return false;

    return true;
  }

  @Override
  public FontEntity[]
  list (FontSpec fontSpec)
  {
    LinkedList<FontEntity> list;
    int i;

    list = new LinkedList<FontEntity> ();

    for (i = 0; i < typefaceList.length; ++i)
      {
	if (checkMatch (typefaceList[i], fontSpec))
	  list.add (new Sdk7FontEntity (typefaceList[i]));
      }

    return list.toArray (new FontEntity[0]);
  }

  @Override
  public FontEntity
  match (FontSpec fontSpec)
  {
    FontEntity[] entities;
    int i;

    entities = this.list (fontSpec);

    if (entities.length == 0)
      return new Sdk7FontEntity (fallbackTypeface);

    return entities[0];
  }

  @Override
  public String[]
  listFamilies ()
  {
    return fontFamilyList;
  }

  @Override
  public FontObject
  openFont (FontEntity fontEntity, int pixelSize)
  {
    return new Sdk7FontObject (((Sdk7FontEntity) fontEntity).typeface,
			       pixelSize);
  }

  @Override
  public int
  hasChar (FontSpec font, char charCode)
  {
    float missingGlyphWidth, width;
    Rect rect1, rect2;
    Paint paint;
    Sdk7FontObject fontObject;

    if (font instanceof Sdk7FontObject)
      {
	fontObject = (Sdk7FontObject) font;
	paint = fontObject.typeface.typefacePaint;
      }
    else
      paint = ((Sdk7FontEntity) font).typeface.typefacePaint;

    paint.setTextSize (10);

    if (Character.isWhitespace (charCode))
      return 1;

    missingGlyphWidth = paint.measureText (TOFU_STRING);
    width = paint.measureText ("" + charCode);

    if (width == 0f)
      return 0;

    if (width != missingGlyphWidth)
      return 1;

    rect1 = new Rect ();
    rect2 = new Rect ();

    paint.getTextBounds (TOFU_STRING, 0, TOFU_STRING.length (),
			 rect1);
    paint.getTextBounds ("" + charCode, 0, 1, rect2);
    return rect1.equals (rect2) ? 0 : 1;
  }

  private void
  textExtents1 (Sdk7FontObject font, int code, FontMetrics metrics,
		Paint paint, Rect bounds)
  {
    char[] text;

    text = new char[1];
    text[0] = (char) code;

    paint.getTextBounds (text, 0, 1, bounds);

    /* bounds is the bounding box of the glyph corresponding to CODE.
       Translate these into XCharStruct values.

       The origin is at 0, 0, and lbearing is the distance counting
       rightwards from the origin to the left most pixel in the glyph
       raster.  rbearing is the distance between the origin and the
       rightmost pixel in the glyph raster.  ascent is the distance
       counting upwards between the the topmost pixel in the glyph
       raster.  descent is the distance (once again counting
       downwards) between the origin and the bottommost pixel in the
       glyph raster.

       width is the distance between the origin and the origin of any
       character to the right.  */

    metrics.lbearing = (short) bounds.left;
    metrics.rbearing = (short) bounds.right;
    metrics.ascent = (short) -bounds.top;
    metrics.descent = (short) bounds.bottom;
    metrics.width = (short) paint.measureText ("" + text[0]);
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
	text = new char[code.length];

	for (i = 0; i < code.length; ++i)
	  text[i] = (char) code[i];

	paintCache.getTextBounds (text, 0, code.length,
				  boundsCache);
	width = paintCache.measureText (text, 0, code.length);

	fontMetrics.lbearing = (short) boundsCache.left;
	fontMetrics.rbearing = (short) boundsCache.right;
	fontMetrics.ascent = (short) -boundsCache.top;
	fontMetrics.descent = (short) boundsCache.bottom;
	fontMetrics.width = (short) Math.round (width);
      }
  }

  @Override
  public int
  encodeChar (FontObject fontObject, char charCode)
  {
    return charCode;
  }

  @Override
  public int
  draw (FontObject fontObject, EmacsGC gc, EmacsDrawable drawable,
	int[] chars, int x, int y, int backgroundWidth,
	boolean withBackground)
  {
    Rect backgroundRect, bounds;
    Sdk7FontObject sdk7FontObject;
    char[] charsArray;
    int i;
    Canvas canvas;
    Paint paint;

    sdk7FontObject = (Sdk7FontObject) fontObject;
    charsArray = new char[chars.length];

    for (i = 0; i < chars.length; ++i)
      charsArray[i] = (char) chars[i];

    backgroundRect = new Rect ();
    backgroundRect.top = y - sdk7FontObject.ascent;
    backgroundRect.left = x;
    backgroundRect.right = x + backgroundWidth;
    backgroundRect.bottom = y + sdk7FontObject.descent;

    canvas = drawable.lockCanvas (gc);

    if (canvas == null)
      return 0;

    paint = gc.gcPaint;
    paint.setStyle (Paint.Style.FILL);

    if (withBackground)
      {
	paint.setColor (gc.background | 0xff000000);
	canvas.drawRect (backgroundRect, paint);
	paint.setColor (gc.foreground | 0xff000000);
      }

    paint.setTextSize (sdk7FontObject.pixelSize);
    paint.setTypeface (sdk7FontObject.typeface.typeface);
    paint.setAntiAlias (true);
    canvas.drawText (charsArray, 0, chars.length, x, y, paint);

    bounds = new Rect ();
    paint.getTextBounds (charsArray, 0, chars.length, bounds);
    bounds.offset (x, y);
    bounds.union (backgroundRect);
    drawable.damageRect (bounds);
    paint.setAntiAlias (false);
    return 1;
  }
};
