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

import java.io.File;

import java.util.LinkedList;
import java.util.List;

import android.graphics.Paint;
import android.graphics.Rect;
import android.graphics.Typeface;
import android.graphics.Canvas;

import android.util.DisplayMetrics;
import android.util.Log;



/* EmacsSdk7FontDriver implements a fallback font driver under
   Android.  This font driver is enabled when the SFNT font driver (in
   sfntfont-android.c) proves incapable of locating any fonts, which
   has hitherto not been observed in practice.

   This font driver does not supply each font installed on the system,
   in lieu of which it provides a list of fonts for each conceivable
   style and sub-type of the system's own Typefaces, which arises from
   Android's absence of suitable APIs for loading individual font
   files.  */

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
    Sdk7Typeface (String familyName, Typeface typeface)
    {
      String style, testString;
      int index, measured, i;
      float[] widths;

      /* Initialize the font style fields and create a paint object
	 linked with that typeface.  */

      slant = NORMAL;
      weight = REGULAR;
      width = UNSPECIFIED;
      spacing = PROPORTIONAL;

      this.typeface = typeface;
      this.familyName = familyName;

      typefacePaint = new Paint ();
      typefacePaint.setAntiAlias (true);
      typefacePaint.setTypeface (typeface);
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

    @SuppressWarnings ("deprecation")
    public
    Sdk7FontEntity (Sdk7Typeface typeface)
    {
      DisplayMetrics metrics;

      foundry = "Google";
      family = typeface.familyName;
      adstyle = null;
      weight = typeface.weight;
      slant = typeface.slant;
      spacing = typeface.spacing;
      width = typeface.width;
      metrics = EmacsService.SERVICE.getResources ().getDisplayMetrics ();
      dpi = Math.round (metrics.scaledDensity * 160f);

      this.typeface = typeface;
    }
  };

  protected final class Sdk7FontObject extends FontObject
  {
    /* The typeface.  */
    public Sdk7Typeface typeface;

    @SuppressWarnings ("deprecation")
    public
    Sdk7FontObject (Sdk7Typeface typeface, int pixelSize)
    {
      float totalWidth;
      String testWidth, testString;
      DisplayMetrics metrics;

      this.typeface = typeface;
      this.pixelSize = pixelSize;

      family = typeface.familyName;
      adstyle = null;
      weight = typeface.weight;
      slant = typeface.slant;
      spacing = typeface.spacing;
      width = typeface.width;
      metrics = EmacsService.SERVICE.getResources ().getDisplayMetrics ();
      dpi = Math.round (metrics.scaledDensity * 160f);

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
    Typeface typeface;

    typefaceList = new Sdk7Typeface[5];

    /* Initialize the default monospace and Sans Serif typefaces.
       Initialize the same typeface with various distinct styles.  */
    fallbackTypeface = new Sdk7Typeface ("Sans Serif",
					 Typeface.DEFAULT);
    typefaceList[1] = fallbackTypeface;

    fallbackTypeface = new Sdk7Typeface ("Sans Serif",
					 Typeface.create (Typeface.DEFAULT,
							  Typeface.BOLD));
    fallbackTypeface.weight = BOLD;
    typefaceList[2] = fallbackTypeface;

    fallbackTypeface = new Sdk7Typeface ("Sans Serif",
					 Typeface.create (Typeface.DEFAULT,
							  Typeface.ITALIC));
    fallbackTypeface.slant = ITALIC;
    typefaceList[3] = fallbackTypeface;

    fallbackTypeface
      = new Sdk7Typeface ("Sans Serif",
			  Typeface.create (Typeface.DEFAULT,
					   Typeface.BOLD_ITALIC));
    fallbackTypeface.weight = BOLD;
    fallbackTypeface.slant = ITALIC;
    typefaceList[4] = fallbackTypeface;

    fallbackTypeface = new Sdk7Typeface ("Monospace",
					 Typeface.MONOSPACE);
    fallbackTypeface.spacing = MONO;
    typefaceList[0] = fallbackTypeface;

    fontFamilyList = new String[] { "Monospace", "Sans Serif", };
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
  hasChar (FontSpec font, int charCode)
  {
    float missingGlyphWidth, width;
    Rect rect1, rect2;
    Paint paint;
    Sdk7FontObject fontObject;

    /* Ignore characters outside the BMP.  */

    if (charCode > 65535)
      return 0;

    if (font instanceof Sdk7FontObject)
      {
	fontObject = (Sdk7FontObject) font;
	paint = fontObject.typeface.typefacePaint;
      }
    else
      paint = ((Sdk7FontEntity) font).typeface.typefacePaint;

    paint.setTextSize (10);

    if (Character.isWhitespace ((char) charCode))
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
    paint.getTextBounds ("" + (char) charCode, 0, 1, rect2);
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
       counting upwards between the topmost pixel in the glyph
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
  encodeChar (FontObject fontObject, int charCode)
  {
    if (charCode > 65535)
      return FONT_INVALID_CODE;

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
    int i;
    Canvas canvas;
    Paint paint;
    char[] array;

    sdk7FontObject = (Sdk7FontObject) fontObject;

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

    /* Android applies kerning to non-monospaced fonts by default,
       which brings the dimensions of strings drawn via `drawText' out
       of agreement with measurements previously provided to redisplay
       by textExtents.  To avert such disaster, draw each character
       individually, advancing the origin point by hand.  */

    bounds = new Rect ();
    array = new char[1];

    for (i = 0; i < chars.length; ++i)
      {
	/* Retrieve the text bounds for this character so as to
	   compute the damage rectangle.  */
	array[0] = (char) chars[i];
	paint.getTextBounds (array, 0, 1, bounds);
	bounds.offset (x, y);
	backgroundRect.union (bounds);

	/* Draw this character.  */
	canvas.drawText (array, 0, 1, x, y, paint);

	/* Advance the origin point by that much.  */
	x += paint.measureText ("" + array[0]);
      }

    drawable.damageRect (backgroundRect);
    paint.setAntiAlias (false);
    return 1;
  }
};
