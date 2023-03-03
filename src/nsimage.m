/* Image support for the NeXT/Open/GNUstep and macOS window system.
   Copyright (C) 1989, 1992-1994, 2005-2006, 2008-2023 Free Software
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
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

/*
Originally by Carl Edman
Updated by Christian Limpach (chris@nice.ch)
OpenStep/Rhapsody port by Scott Bender (sbender@harmony-ds.com)
macOS/Aqua port by Christophe de Dinechin (descubes@earthlink.net)
GNUstep port and post-20 update by Adrian Robert (arobert@cogsci.ucsd.edu)
*/

/* This should be the first include, as it may set up #defines affecting
   interpretation of even the system includes.  */
#include <config.h>

#include "lisp.h"
#include "dispextern.h"
#include "nsterm.h"
#include "frame.h"
#include "coding.h"


#if defined (NS_IMPL_GNUSTEP) || MAC_OS_X_VERSION_MAX_ALLOWED < 1070
# define COLORSPACE_NAME NSCalibratedRGBColorSpace
#else
# define COLORSPACE_NAME                                                \
  ((ns_use_srgb_colorspace && NSAppKitVersionNumber >= NSAppKitVersionNumber10_7) \
   ? NSDeviceRGBColorSpace : NSCalibratedRGBColorSpace)
#endif


/* ==========================================================================

   C interface.  This allows easy calling from C files.  We could just
   compile everything as Objective-C, but that might mean slower
   compilation and possible difficulties on some platforms.

   ========================================================================== */

bool
ns_can_use_native_image_api (Lisp_Object type)
{
  NSString *imageType = @"unknown";
  NSArray *types;

  NSTRACE ("ns_can_use_native_image_api");

  if (EQ (type, Qnative_image))
    return YES;

#ifdef NS_IMPL_COCOA
  /* Work out the UTI of the image type.  */
  if (EQ (type, Qjpeg))
    imageType = @"public.jpeg";
  else if (EQ (type, Qpng))
    imageType = @"public.png";
  else if (EQ (type, Qgif))
    imageType = @"com.compuserve.gif";
  else if (EQ (type, Qtiff))
    imageType = @"public.tiff";
#ifndef HAVE_RSVG
  else if (EQ (type, Qsvg))
    imageType = @"public.svg-image";
#endif
  else if (EQ (type, Qheic))
    imageType = @"public.heic";

  /* NSImage also supports a host of other types such as PDF and BMP,
     but we don't yet support these in image.c.  */

  types = [NSImage imageTypes];
#else
  /* Work out the image type.  */
  if (EQ (type, Qjpeg))
    imageType = @"jpeg";
  else if (EQ (type, Qpng))
    imageType = @"png";
  else if (EQ (type, Qgif))
    imageType = @"gif";
  else if (EQ (type, Qtiff))
    imageType = @"tiff";

  types = [NSImage imageFileTypes];
#endif

  /* Check if the type is supported on this system.  */
  if ([types indexOfObject:imageType] != NSNotFound)
    return YES;
  else
    return NO;
}

void *
ns_image_from_XBM (char *bits, int width, int height,
                   unsigned long fg, unsigned long bg)
{
  NSTRACE ("ns_image_from_XBM");
  return [[EmacsImage alloc] initFromXBM: (unsigned char *) bits
                                   width: width height: height
                                      fg: fg bg: bg reverseBytes: YES];
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
  Lisp_Object lisp_index;
  unsigned int index;

  NSTRACE ("ns_load_image");

  eassert (valid_image_p (img->spec));

  lisp_index = plist_get (XCDR (img->spec), QCindex);
  index = FIXNUMP (lisp_index) ? XFIXNAT (lisp_index) : 0;

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

  if (![eImg setFrame: index])
    {
      add_to_log ("Unable to set index %d for image %s",
                  make_fixnum (index), img->spec);
      return 0;
    }

  img->lisp_data = [eImg getMetadata];

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

void
ns_image_set_size (void *img, int width, int height)
{
  [(EmacsImage *)img setSize:NSMakeSize (width, height)];
}

void
ns_image_set_transform (void *img, double m[3][3])
{
  [(EmacsImage *)img setTransform:m];
}

void
ns_image_set_smoothing (void *img, bool smooth)
{
  [(EmacsImage *)img setSmoothing:smooth];
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

size_t
ns_image_size_in_bytes (void *img)
{
  return [(EmacsImage *)img sizeInBytes];
}

/* ==========================================================================

   Class supporting bitmaps and images of various sorts.

   ========================================================================== */

@implementation EmacsImage

+ (instancetype)allocInitFromFile: (Lisp_Object)file
{
  NSImageRep *imgRep;
  Lisp_Object found;
  EmacsImage *image;
  NSString *filename;

  /* Search bitmap-file-path for the file, if appropriate.  */
  found = image_find_image_file (file);
  if (!STRINGP (found))
    return nil;
  filename = [NSString stringWithLispString:found];

  image = [[EmacsImage alloc] initByReferencingFile:filename];

  image->bmRep = nil;
  if (![image isValid])
    {
      [image release];
      return nil;
    }
  imgRep = [[image representations] firstObject];

  [image setSize: NSMakeSize([imgRep pixelsWide], [imgRep pixelsHigh])];
  [image setName:filename];

  return image;
}


- (void)dealloc
{
  [stippleMask release];
  [bmRep release];
  [transform release];
  [super dealloc];
}


- (id)copyWithZone:(NSZone *)zone
{
  EmacsImage *copy = [super copyWithZone:zone];

  copy->stippleMask = [stippleMask copyWithZone:zone];
  copy->bmRep = [bmRep copyWithZone:zone];
  copy->transform = [transform copyWithZone:zone];

  return copy;
}


/* Create image from monochrome bitmap. If both FG and BG are 0
   (black), set the background to white and make it transparent.  */
- (instancetype)initFromXBM: (unsigned char *)bits width: (int)w height: (int)h
                         fg: (unsigned long)fg bg: (unsigned long)bg
               reverseBytes: (BOOL)reverse
{
  unsigned char *planes[5];
  unsigned char bg_alpha = 0xff;

  [self initWithSize: NSMakeSize (w, h)];

  bmRep = [[NSBitmapImageRep alloc] initWithBitmapDataPlanes: NULL
                                    pixelsWide: w pixelsHigh: h
                                    bitsPerSample: 8 samplesPerPixel: 4
                                    hasAlpha: YES isPlanar: YES
                                    colorSpaceName: COLORSPACE_NAME
                                    bytesPerRow: w bitsPerPixel: 0];

  [bmRep getBitmapDataPlanes: planes];

  if (fg == 0 && bg == 0)
    {
      bg = 0xffffff;
      bg_alpha = 0;
    }

  {
    /* Pull bits out to set the (bytewise) alpha mask.  */
    unsigned char swt[16] = {0, 8, 4, 12, 2, 10, 6, 14,
                             1, 9, 5, 13, 3, 11, 7, 15};
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

    for (j = 0; j < h; ++j)
      for (i = 0; i < w; )
        {
          c = *s++;

          /* XBM files have the bits in reverse order within each byte
             as compared to our fringe bitmaps.  This function deals
             with both so has to be able to handle the bytes in either
             order.  */
          if (reverse)
            c = swt[c >> 4] | (swt[c & 0xf] << 4);

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
              c <<= 1;
            }
        }
  }

  [self addRepresentation: bmRep];
  return self;
}


- (instancetype)initForXPMWithDepth: (int)depth width: (int)width height: (int)height
{
  NSSize s = {width, height};
  int i;

  [self initWithSize: s];

  bmRep = [[NSBitmapImageRep alloc] initWithBitmapDataPlanes: NULL
                                  pixelsWide: width pixelsHigh: height
                                  /* keep things simple for now */
                                  bitsPerSample: 8 samplesPerPixel: 4 /*RGB+A*/
                                  hasAlpha: YES isPlanar: YES
                                  colorSpaceName: COLORSPACE_NAME
                                  bytesPerRow: width bitsPerPixel: 0];

  [bmRep getBitmapDataPlanes: pixmapData];
  for (i =0; i<4; i++)
    memset (pixmapData[i], 0, width*height);
  [self addRepresentation: bmRep];
  return self;
}


/* Attempt to pull out pixmap data from a BitmapImageRep; returns NO if fails.  */
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

          [self setSize: NSMakeSize([bmr pixelsWide], [bmr pixelsHigh])];

          break;
        }
    }
}


/* Note: this and next work only for image created with initForXPMWithDepth,
         initFromSkipXBM, or where setPixmapData was called successfully.  */
/* return ARGB */
- (unsigned long) getPixelAtX: (int)x Y: (int)y
{
  if (bmRep == nil)
    return 0;

  /* This method is faster but won't work for bitmaps.  */
  if (pixmapData[0] != NULL)
    {
      int loc = x + y * [self size].width;
      return (((unsigned long) pixmapData[3][loc] << 24) /* alpha */
              | ((unsigned long) pixmapData[0][loc] << 16)
              | ((unsigned long) pixmapData[1][loc] << 8)
              | (unsigned long) pixmapData[2][loc]);
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
               alpha:(unsigned char)a
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

/* Returns a pattern color, which is cached here.  */
- (NSColor *)stippleMask
{
  if (stippleMask == nil)
      stippleMask = [[NSColor colorWithPatternImage: self] retain];
  return stippleMask;
}

/* Find the first NSBitmapImageRep which has multiple frames.  */
- (NSBitmapImageRep *)getAnimatedBitmapImageRep
{
  for (NSImageRep * r in [self representations])
    {
      if ([r isKindOfClass:[NSBitmapImageRep class]])
        {
          NSBitmapImageRep * bm = (NSBitmapImageRep *)r;
          if ([[bm valueForProperty:NSImageFrameCount] intValue] > 0)
            return bm;
        }
    }
  return nil;
}

/* If the image has multiple frames, get a count of them and the
   animation delay, if available.  */
- (Lisp_Object)getMetadata
{
  Lisp_Object metadata = Qnil;

  NSBitmapImageRep * bm = [self getAnimatedBitmapImageRep];

  if (bm != nil)
    {
      int frames = [[bm valueForProperty:NSImageFrameCount] intValue];
      float delay = [[bm valueForProperty:NSImageCurrentFrameDuration]
                      floatValue];

      if (frames > 1)
        metadata = Fcons (Qcount, Fcons (make_fixnum (frames), metadata));
      if (delay > 0)
        metadata = Fcons (Qdelay, Fcons (make_float (delay), metadata));
    }
  return metadata;
}

/* Attempt to set the animation frame to be displayed.  */
- (BOOL)setFrame: (unsigned int) index
{
  NSBitmapImageRep * bm = [self getAnimatedBitmapImageRep];

  if (bm != nil)
    {
      int frames = [[bm valueForProperty:NSImageFrameCount] intValue];

      /* If index is invalid, give up.  */
      if (index < 0 || index > frames)
        return NO;

      [bm setProperty: NSImageCurrentFrame
            withValue: [NSNumber numberWithUnsignedInt:index]];
    }

  /* Setting the frame has succeeded, or the image doesn't have
     multiple frames.  */
  return YES;
}

- (void)setTransform: (double[3][3]) m
{
  transform = [[NSAffineTransform transform] retain];
  NSAffineTransformStruct tm
    = { m[0][0], m[0][1], m[1][0], m[1][1], m[2][0], m[2][1]};
  [transform setTransformStruct:tm];
}

- (void)setSmoothing: (BOOL) s
{
  smoothing = s;
}

/* Approximate allocated size of image in bytes.  */
- (size_t) sizeInBytes
{
  size_t bytes = 0;
  NSImageRep *rep;
  NSEnumerator *reps = [[self representations] objectEnumerator];
  while ((rep = (NSImageRep *) [reps nextObject]))
    {
      if ([rep respondsToSelector: @selector (bytesPerRow)])
        {
          NSBitmapImageRep *bmr = (NSBitmapImageRep *) rep;
          bytes += [bmr bytesPerRow] * [bmr numberOfPlanes] * [bmr pixelsHigh];
        }
    }
  return bytes;
}


@end
