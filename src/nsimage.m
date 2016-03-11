/* Image support for the NeXT/Open/GNUstep and MacOSX window system.
   Copyright (C) 1989, 1992-1994, 2005-2006, 2008-2016 Free Software
   Foundation, Inc.

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
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

/*
Originally by Carl Edman
Updated by Christian Limpach (chris@nice.ch)
OpenStep/Rhapsody port by Scott Bender (sbender@harmony-ds.com)
MacOSX/Aqua port by Christophe de Dinechin (descubes@earthlink.net)
GNUstep port and post-20 update by Adrian Robert (arobert@cogsci.ucsd.edu)
*/

/* This should be the first include, as it may set up #defines affecting
   interpretation of even the system includes. */
#include <config.h>

#include "lisp.h"
#include "dispextern.h"
#include "nsterm.h"
#include "frame.h"
#include "coding.h"



/* ==========================================================================

   C interface.  This allows easy calling from C files.  We could just
   compile everything as Objective-C, but that might mean slower
   compilation and possible difficulties on some platforms..

   ========================================================================== */

void *
ns_image_from_XBM (unsigned char *bits, int width, int height,
                   unsigned long fg, unsigned long bg)
{
  NSTRACE ("ns_image_from_XBM");
  return [[EmacsImage alloc] initFromXBM: bits
                                   width: width height: height
                                      fg: fg bg: bg];
}

void *
ns_image_for_XPM (int width, int height, int depth)
{
  NSTRACE ("ns_image_for_XPM");
  return [[EmacsImage alloc] initForXPMWithDepth: depth
                                           width: width height: height];
}

void *
ns_image_from_file (Lisp_Object file)
{
  NSTRACE ("ns_image_from_file");
  return [EmacsImage allocInitFromFile: file];
}

bool
ns_load_image (struct frame *f, struct image *img,
               Lisp_Object spec_file, Lisp_Object spec_data)
{
  EmacsImage *eImg = nil;
  NSSize size;

  NSTRACE ("ns_load_image");

  if (STRINGP (spec_file))
    {
      eImg = [EmacsImage allocInitFromFile: spec_file];
    }
  else if (STRINGP (spec_data))
    {
      NSData *data;

      data = [NSData dataWithBytes: SSDATA (spec_data)
			    length: SBYTES (spec_data)];
      eImg = [[EmacsImage alloc] initWithData: data];
      [eImg setPixmapData];
    }

  if (eImg == nil)
    {
      add_to_log ("Unable to load image %s", img->spec);
      return 0;
    }

  size = [eImg size];
  img->width = size.width;
  img->height = size.height;

  /* 4) set img->pixmap = emacsimage */
  img->pixmap = eImg;
  return 1;
}


int
ns_image_width (void *img)
{
  return [(id)img size].width;
}

int
ns_image_height (void *img)
{
  return [(id)img size].height;
}

unsigned long
ns_get_pixel (void *img, int x, int y)
{
  return [(EmacsImage *)img getPixelAtX: x Y: y];
}

void
ns_put_pixel (void *img, int x, int y, unsigned long argb)
{
  unsigned char alpha = (argb >> 24) & 0xFF;
  if (alpha == 0)
    alpha = 0xFF;
  [(EmacsImage *)img setPixelAtX: x Y: y toRed: (argb >> 16) & 0xFF
   green: (argb >> 8) & 0xFF blue: (argb & 0xFF) alpha: alpha];
}

void
ns_set_alpha (void *img, int x, int y, unsigned char a)
{
  [(EmacsImage *)img setAlphaAtX: x Y: y to: a];
}


/* ==========================================================================

   Class supporting bitmaps and images of various sorts.

   ========================================================================== */

@implementation EmacsImage

+ allocInitFromFile: (Lisp_Object)file
{
  NSImageRep *imgRep;
  Lisp_Object found;
  EmacsImage *image;

  /* Search bitmap-file-path for the file, if appropriate.  */
  found = x_find_image_file (file);
  if (!STRINGP (found))
    return nil;
  found = ENCODE_FILE (found);

  image = [[EmacsImage alloc] initByReferencingFile:
                     [NSString stringWithUTF8String: SSDATA (found)]];

  image->bmRep = nil;
#ifdef NS_IMPL_COCOA
  imgRep = [NSBitmapImageRep imageRepWithData:[image TIFFRepresentation]];
#else
  imgRep = [image bestRepresentationForDevice: nil];
#endif
  if (imgRep == nil)
    {
      [image release];
      return nil;
    }

  /* The next two lines cause the DPI of the image to be ignored.
     This seems to be the behavior users expect. */
#ifdef NS_IMPL_COCOA
#if MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_6
  [image setScalesWhenResized: YES];
#endif
#endif
  [image setSize: NSMakeSize([imgRep pixelsWide], [imgRep pixelsHigh])];

  [image setName: [NSString stringWithUTF8String: SSDATA (file)]];

  return image;
}


- (void)dealloc
{
  [stippleMask release];
  [bmRep release];
  [super dealloc];
}


/* Create image from monochrome bitmap. If both FG and BG are 0
   (black), set the background to white and make it transparent. */
- initFromXBM: (unsigned char *)bits width: (int)w height: (int)h
           fg: (unsigned long)fg bg: (unsigned long)bg
{
  unsigned char *planes[5];
  unsigned char bg_alpha = 0xff;

  [self initWithSize: NSMakeSize (w, h)];

  bmRep = [[NSBitmapImageRep alloc] initWithBitmapDataPlanes: NULL
                                    pixelsWide: w pixelsHigh: h
                                    bitsPerSample: 8 samplesPerPixel: 4
                                    hasAlpha: YES isPlanar: YES
                                    colorSpaceName: NSCalibratedRGBColorSpace
                                    bytesPerRow: w bitsPerPixel: 0];

  [bmRep getBitmapDataPlanes: planes];

  if (fg == 0 && bg == 0)
    {
      bg = 0xffffff;
      bg_alpha = 0;
    }

  {
    /* pull bits out to set the (bytewise) alpha mask */
    int i, j, k;
    unsigned char *s = bits;
    unsigned char *rr = planes[0];
    unsigned char *gg = planes[1];
    unsigned char *bb = planes[2];
    unsigned char *alpha = planes[3];
    unsigned char fgr = (fg >> 16) & 0xff;
    unsigned char fgg = (fg >> 8) & 0xff;
    unsigned char fgb = fg & 0xff;
    unsigned char bgr = (bg >> 16) & 0xff;
    unsigned char bgg = (bg >> 8) & 0xff;
    unsigned char bgb = bg & 0xff;
    unsigned char c;

    int idx = 0;
    for (j = 0; j < h; ++j)
      for (i = 0; i < w; )
        {
          c = *s++;
          for (k = 0; i < w && k < 8; ++k, ++i)
            {
              if (c & 0x80)
                {
                  *rr++ = fgr;
                  *gg++ = fgg;
                  *bb++ = fgb;
                  *alpha++ = 0xff;
                }
              else
                {
                  *rr++ = bgr;
                  *gg++ = bgg;
                  *bb++ = bgb;
                  *alpha++ = bg_alpha;
                }
              idx++;
              c <<= 1;
            }
        }
  }

  xbm_fg = fg;
  [self addRepresentation: bmRep];
  return self;
}

/* Set color for a bitmap image.  */
- setXBMColor: (NSColor *)color
{
  NSSize s = [self size];
  unsigned char *planes[5];
  EmacsCGFloat r, g, b, a;
  NSColor *rgbColor;

  if (bmRep == nil || color == nil)
    return self;

  if ([color colorSpaceName] != NSCalibratedRGBColorSpace)
    rgbColor = [color colorUsingColorSpaceName: NSCalibratedRGBColorSpace];
  else
    rgbColor = color;

  [rgbColor getRed: &r green: &g blue: &b alpha: &a];

  [bmRep getBitmapDataPlanes: planes];

  {
    int i, len = s.width*s.height;
    int rr = r * 0xff, gg = g * 0xff, bb = b * 0xff;
    unsigned char fgr = (xbm_fg >> 16) & 0xff;
    unsigned char fgg = (xbm_fg >> 8) & 0xff;
    unsigned char fgb = xbm_fg & 0xff;

    for (i = 0; i < len; ++i)
      if (planes[0][i] == fgr && planes[1][i] == fgg && planes[2][i] == fgb)
        {
          planes[0][i] = rr;
          planes[1][i] = gg;
          planes[2][i] = bb;
        }
    xbm_fg = ((rr << 16) & 0xff) + ((gg << 8) & 0xff) + (bb & 0xff);
  }

  return self;
}


- initForXPMWithDepth: (int)depth width: (int)width height: (int)height
{
  NSSize s = {width, height};
  int i;

  [self initWithSize: s];

  bmRep = [[NSBitmapImageRep alloc] initWithBitmapDataPlanes: NULL
                                  pixelsWide: width pixelsHigh: height
                                  /* keep things simple for now */
                                  bitsPerSample: 8 samplesPerPixel: 4 /*RGB+A*/
                                  hasAlpha: YES isPlanar: YES
                                  colorSpaceName: NSCalibratedRGBColorSpace
                                  bytesPerRow: width bitsPerPixel: 0];

  [bmRep getBitmapDataPlanes: pixmapData];
  for (i =0; i<4; i++)
    memset (pixmapData[i], 0, width*height);
  [self addRepresentation: bmRep];
  return self;
}


/* attempt to pull out pixmap data from a BitmapImageRep; returns NO if fails */
- (void) setPixmapData
{
  NSEnumerator *reps;
  NSImageRep *rep;

  reps = [[self representations] objectEnumerator];
  while ((rep = (NSImageRep *) [reps nextObject]))
    {
      if ([rep respondsToSelector: @selector (getBitmapDataPlanes:)])
        {
          NSBitmapImageRep *bmr = (NSBitmapImageRep *) rep;

          if ([bmr numberOfPlanes] >= 3)
              [bmr getBitmapDataPlanes: pixmapData];

          /* The next two lines cause the DPI of the image to be ignored.
             This seems to be the behavior users expect. */
#ifdef NS_IMPL_COCOA
#if MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_6
          [self setScalesWhenResized: YES];
#endif
#endif
          [self setSize: NSMakeSize([bmr pixelsWide], [bmr pixelsHigh])];

          break;
        }
    }
}


/* note; this and next work only for image created with initForXPMWithDepth,
         initFromSkipXBM, or where setPixmapData was called successfully */
/* return ARGB */
- (unsigned long) getPixelAtX: (int)x Y: (int)y
{
  if (bmRep == nil)
    return 0;

  /* this method is faster but won't work for bitmaps */
  if (pixmapData[0] != NULL)
    {
      int loc = x + y * [self size].width;
      return (pixmapData[3][loc] << 24) /* alpha */
       | (pixmapData[0][loc] << 16) | (pixmapData[1][loc] << 8)
       | (pixmapData[2][loc]);
    }
  else
    {
      NSColor *color = [bmRep colorAtX: x y: y];
      EmacsCGFloat r, g, b, a;
      [color getRed: &r green: &g blue: &b alpha: &a];
      return ((int)(a * 255.0) << 24)
        | ((int)(r * 255.0) << 16) | ((int)(g * 255.0) << 8)
        | ((int)(b * 255.0));

    }
}

- (void) setPixelAtX: (int)x Y: (int)y toRed: (unsigned char)r
               green: (unsigned char)g blue: (unsigned char)b
               alpha:(unsigned char)a;
{
  if (bmRep == nil)
    return;

  if (pixmapData[0] != NULL)
    {
      int loc = x + y * [self size].width;
      pixmapData[0][loc] = r;
      pixmapData[1][loc] = g;
      pixmapData[2][loc] = b;
      pixmapData[3][loc] = a;
    }
  else
    {
      [bmRep setColor:
               [NSColor colorWithCalibratedRed: (r/255.0) green: (g/255.0)
                                          blue: (b/255.0) alpha: (a/255.0)]
                  atX: x y: y];
    }
}

- (void) setAlphaAtX: (int) x Y: (int) y to: (unsigned char) a
{
  if (bmRep == nil)
    return;

  if (pixmapData[0] != NULL)
    {
      int loc = x + y * [self size].width;

      pixmapData[3][loc] = a;
    }
  else
    {
      NSColor *color = [bmRep colorAtX: x y: y];
      color = [color colorWithAlphaComponent: (a / 255.0)];
      [bmRep setColor: color atX: x y: y];
    }
}

/* returns a pattern color, which is cached here */
- (NSColor *)stippleMask
{
  if (stippleMask == nil)
      stippleMask = [[NSColor colorWithPatternImage: self] retain];
  return stippleMask;
}

@end
