/* xfaces.c -- "Face" primitives.

Copyright (C) 1993-1994, 1998-2023 Free Software Foundation, Inc.

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

/* New face implementation by Gerd Moellmann <gerd@gnu.org>.  */

/* Faces.

   When using Emacs with X, the display style of characters can be
   changed by defining `faces'.  Each face can specify the following
   display attributes:

   1. Font family name.

   2. Font foundry name.

   3. Relative proportionate width, aka character set width or set
   width (swidth), e.g. `semi-compressed'.

   4. Font height in 1/10pt.

   5. Font weight, e.g. `bold'.

   6. Font slant, e.g. `italic'.

   7. Foreground color.

   8. Background color.

   9. Whether or not characters should be underlined, and in what color.

   10. Whether or not characters should be displayed in inverse video.

   11. A background stipple, a bitmap.

   12. Whether or not characters should be overlined, and in what color.

   13. Whether or not characters should be strike-through, and in what
   color.

   14. Whether or not a box should be drawn around characters, the box
   type, and, for simple boxes, in what color.

   15. Font-spec, or nil.  This is a special attribute.

   A font-spec is a collection of font attributes (specs).

   When this attribute is specified, the face uses a font matching
   with the specs as is except for what overwritten by the specs in
   the fontset (see below).  In addition, the other font-related
   attributes (1st thru 5th) are updated from the spec.

   On the other hand, if one of the other font-related attributes are
   specified, the corresponding specs in this attribute is set to nil.

   16. A face name or list of face names from which to inherit attributes.

   17. A fontset name.  This is another special attribute.

   A fontset is a mappings from characters to font-specs, and the
   specs overwrite the font-spec in the 14th attribute.

   18. A "distant background" color, to be used when the foreground is
   too close to the background and is hard to read.

   19. Whether to extend the face to end of line when the face
   "covers" the newline that ends the line.

   On the C level, a Lisp face is completely represented by its array
   of attributes.  In that array, the zeroth element is Qface, and the
   rest are the 19 face attributes described above.  The
   lface_attribute_index enumeration, defined on dispextern.h, with
   values given by the LFACE_*_INDEX constants, is used to reference
   the individual attributes.

   Faces are frame-local by nature because Emacs allows you to define the
   same named face (face names are symbols) differently for different
   frames.  Each frame has an alist of face definitions for all named
   faces.  The value of a named face in such an alist is a Lisp vector
   with the symbol `face' in slot 0, and a slot for each of the face
   attributes mentioned above.

   There is also a global face map `Vface_new_frame_defaults',
   containing conses of (FACE_ID . FACE_DEFINITION).  Face definitions
   from this table are used to initialize faces of newly created
   frames.

   A face doesn't have to specify all attributes.  Those not specified
   have a value of `unspecified'.  Faces specifying all attributes but
   the 14th are called `fully-specified'.


   Face merging.

   The display style of a given character in the text is determined by
   combining several faces.  This process is called `face merging'.
   Face merging combines the attributes of each of the faces being
   merged such that the attributes of the face that is merged later
   override those of a face merged earlier in the process.  In
   particular, this replaces any 'unspecified' attributes with
   non-'unspecified' values.  Also, if a face inherits from another
   (via the :inherit attribute), the attributes of the parent face,
   recursively, are applied where the inheriting face doesn't specify
   non-'unspecified' values.  Any aspect of the display style that
   isn't specified by overlays or text properties is taken from the
   'default' face.  Since it is made sure that the default face is
   always fully-specified, face merging always results in a
   fully-specified face.


   Face realization.

   After all face attributes for a character have been determined by
   merging faces of that character, that face is `realized'.  The
   realization process maps face attributes to what is physically
   available on the system where Emacs runs.  The result is a
   `realized face' in the form of a struct face which is stored in the
   face cache of the frame on which it was realized.

   Face realization is done in the context of the character to display
   because different fonts may be used for different characters.  In
   other words, for characters that have different font
   specifications, different realized faces are needed to display
   them.

   Font specification is done by fontsets.  See the comment in
   fontset.c for the details.  In the current implementation, all ASCII
   characters share the same font in a fontset.

   Faces are at first realized for ASCII characters, and, at that
   time, assigned a specific realized fontset.  Hereafter, we call
   such a face as `ASCII face'.  When a face for a multibyte character
   is realized, it inherits (thus shares) a fontset of an ASCII face
   that has the same attributes other than font-related ones.

   Thus, all realized faces have a realized fontset.


   Unibyte text.

   Unibyte text (i.e. raw 8-bit characters) is displayed with the same
   font as ASCII characters.  That is because it is expected that
   unibyte text users specify a font that is suitable both for ASCII
   and raw 8-bit characters.


   Font selection.

   Font selection tries to find the best available matching font for a
   given (character, face) combination.

   If the face specifies a fontset name, that fontset determines a
   pattern for fonts of the given character.  If the face specifies a
   font name or the other font-related attributes, a fontset is
   realized from the default fontset.  In that case, that
   specification determines a pattern for ASCII characters and the
   default fontset determines a pattern for multibyte characters.

   Available fonts on the system on which Emacs runs are then matched
   against the font pattern.  The result of font selection is the best
   match for the given face attributes in this font list.

   Font selection can be influenced by the user.

   1. The user can specify the relative importance he gives the face
   attributes width, height, weight, and slant by setting
   face-font-selection-order (faces.el) to a list of face attribute
   names.  The default is '(:width :height :weight :slant), and means
   that font selection first tries to find a good match for the font
   width specified by a face, then---within fonts with that
   width---tries to find a best match for the specified font height,
   etc.

   2. Setting face-font-family-alternatives allows the user to
   specify alternative font families to try if a family specified by a
   face doesn't exist.

   3. Setting face-font-registry-alternatives allows the user to
   specify all alternative font registries to try for a face
   specifying a registry.

   4. Setting face-ignored-fonts allows the user to ignore specific
   fonts.


   Character composition.

   Usually, the realization process is already finished when Emacs
   actually reflects the desired glyph matrix on the screen.  However,
   on displaying a composition (sequence of characters to be composed
   on the screen), a suitable font for the components of the
   composition is selected and realized while drawing them on the
   screen, i.e.  the realization process is delayed but in principle
   the same.


   Initialization of basic faces.

   The faces `default', `modeline' are considered `basic faces'.
   When redisplay happens the first time for a newly created frame,
   basic faces are realized for CHARSET_ASCII.  Frame parameters are
   used to fill in unspecified attributes of the default face.  */

#include <config.h>
#include <stdlib.h>
#include "sysstdio.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <math.h>

#include "lisp.h"
#include "character.h"
#include "frame.h"

#ifdef USE_MOTIF
#include <Xm/Xm.h>
#include <Xm/XmStrDefs.h>
#endif /* USE_MOTIF */

#ifdef MSDOS
#include "dosfns.h"
#endif

#ifdef HAVE_WINDOW_SYSTEM
#include TERM_HEADER
#include "fontset.h"
#ifdef HAVE_NTGUI
#define GCGraphicsExposures 0
#endif /* HAVE_NTGUI */

#ifdef HAVE_NS
#define GCGraphicsExposures 0
#endif /* HAVE_NS */

#ifdef HAVE_PGTK
#define GCGraphicsExposures 0
#endif /* HAVE_PGTK */

#ifdef HAVE_HAIKU
#define GCGraphicsExposures 0
#endif /* HAVE_HAIKU */
#endif /* HAVE_WINDOW_SYSTEM */

#include "buffer.h"
#include "dispextern.h"
#include "blockinput.h"
#include "window.h"
#include "termchar.h"

#include "font.h"

#ifdef HAVE_X_WINDOWS

/* Compensate for a bug in Xos.h on some systems, on which it requires
   time.h.  On some such systems, Xos.h tries to redefine struct
   timeval and struct timezone if USG is #defined while it is
   #included.  */

#ifdef XOS_NEEDS_TIME_H
#include <time.h>
#undef USG
#include <X11/Xos.h>
#define USG
#define __TIMEVAL__
#if defined USG || defined __TIMEVAL__ /* Don't warn about unused macros.  */
#endif
#else /* not XOS_NEEDS_TIME_H */
#include <X11/Xos.h>
#endif /* not XOS_NEEDS_TIME_H */

#endif /* HAVE_X_WINDOWS */

#include <c-ctype.h>

/* True if face attribute ATTR is unspecified.  */

#define UNSPECIFIEDP(ATTR) EQ ((ATTR), Qunspecified)

/* True if face attribute ATTR is `ignore-defface'.  */

#define IGNORE_DEFFACE_P(ATTR) EQ ((ATTR), QCignore_defface)

/* True if face attribute ATTR is `reset'.  */

#define RESET_P(ATTR) EQ ((ATTR), Qreset)

/* Size of hash table of realized faces in face caches (should be a
   prime number).  */

#define FACE_CACHE_BUCKETS_SIZE 1009

char unspecified_fg[] = "unspecified-fg", unspecified_bg[] = "unspecified-bg";

/* Alist of alternative font families.  Each element is of the form
   (FAMILY FAMILY1 FAMILY2 ...).  If fonts of FAMILY can't be loaded,
   try FAMILY1, then FAMILY2, ...  */

Lisp_Object Vface_alternative_font_family_alist;

/* Alist of alternative font registries.  Each element is of the form
   (REGISTRY REGISTRY1 REGISTRY2...).  If fonts of REGISTRY can't be
   loaded, try REGISTRY1, then REGISTRY2, ...  */

Lisp_Object Vface_alternative_font_registry_alist;

/* The next ID to assign to Lisp faces.  */

static int next_lface_id;

/* A vector mapping Lisp face Id's to face names.  */

static Lisp_Object *lface_id_to_name;
static ptrdiff_t lface_id_to_name_size;

#ifdef HAVE_WINDOW_SYSTEM

/* Counter for calls to clear_face_cache.  If this counter reaches
   CLEAR_FONT_TABLE_COUNT, and a frame has more than
   CLEAR_FONT_TABLE_NFONTS load, unused fonts are freed.  */

static int clear_font_table_count;
#define CLEAR_FONT_TABLE_COUNT	100
#define CLEAR_FONT_TABLE_NFONTS	10

#endif /* HAVE_WINDOW_SYSTEM */

/* True means face attributes have been changed since the last
   redisplay.  Used in redisplay_internal.  */

bool face_change;

/* True means don't display bold text if a face's foreground
   and background colors are the inverse of the default colors of the
   display.   This is a kluge to suppress `bold black' foreground text
   which is hard to read on an LCD monitor.  */

static bool tty_suppress_bold_inverse_default_colors_p;

/* The total number of colors currently allocated.  */

#ifdef GLYPH_DEBUG
static int ncolors_allocated;
static int npixmaps_allocated;
static int ngcs;
#endif

/* True means the definition of the `menu' face for new frames has
   been changed.  */

static bool menu_face_changed_default;

struct named_merge_point;

static struct face *realize_face (struct face_cache *,
				  Lisp_Object [LFACE_VECTOR_SIZE],
				  int);
static struct face *realize_gui_face (struct face_cache *,
				      Lisp_Object [LFACE_VECTOR_SIZE]);
static struct face *realize_tty_face (struct face_cache *,
				      Lisp_Object [LFACE_VECTOR_SIZE]);
static bool realize_basic_faces (struct frame *);
static bool realize_default_face (struct frame *);
static void realize_named_face (struct frame *, Lisp_Object, int);
static struct face_cache *make_face_cache (struct frame *);
static void free_face_cache (struct face_cache *);
static bool merge_face_ref (struct window *w,
                            struct frame *, Lisp_Object, Lisp_Object *,
                            bool, struct named_merge_point *,
                            enum lface_attribute_index);
static int color_distance (Emacs_Color *x, Emacs_Color *y);

#ifdef HAVE_WINDOW_SYSTEM
static void set_font_frame_param (Lisp_Object, Lisp_Object);
static void clear_face_gcs (struct face_cache *);
static struct face *realize_non_ascii_face (struct frame *, Lisp_Object,
					    struct face *);
#endif /* HAVE_WINDOW_SYSTEM */

/***********************************************************************
			      Utilities
 ***********************************************************************/

#ifdef HAVE_X_WINDOWS

#ifdef DEBUG_X_COLORS

/* The following is a poor mans infrastructure for debugging X color
   allocation problems on displays with PseudoColor-8.  Some X servers
   like 3.3.5 XF86_SVGA with Matrox cards apparently don't implement
   color reference counts completely so that they don't signal an
   error when a color is freed whose reference count is already 0.
   Other X servers do.  To help me debug this, the following code
   implements a simple reference counting schema of its own, for a
   single display/screen.  --gerd.  */

/* Reference counts for pixel colors.  */

int color_count[256];

/* Register color PIXEL as allocated.  */

void
register_color (unsigned long pixel)
{
  eassert (pixel < 256);
  ++color_count[pixel];
}


/* Register color PIXEL as deallocated.  */

void
unregister_color (unsigned long pixel)
{
  eassert (pixel < 256);
  if (color_count[pixel] > 0)
    --color_count[pixel];
  else
    emacs_abort ();
}


/* Register N colors from PIXELS as deallocated.  */

void
unregister_colors (unsigned long *pixels, int n)
{
  int i;
  for (i = 0; i < n; ++i)
    unregister_color (pixels[i]);
}


DEFUN ("dump-colors", Fdump_colors, Sdump_colors, 0, 0, 0,
       doc: /* Dump currently allocated colors to stderr.  */)
  (void)
{
  int i, n;

  putc ('\n', stderr);

  for (i = n = 0; i < ARRAYELTS (color_count); ++i)
    if (color_count[i])
      {
	fprintf (stderr, "%3d: %5d", i, color_count[i]);
	++n;
	putc (n % 5 == 0 ? '\n' : '\t', stderr);
      }

  if (n % 5 != 0)
    putc ('\n', stderr);
  return Qnil;
}

#endif /* DEBUG_X_COLORS */


/* Free colors used on frame F.  PIXELS is an array of NPIXELS pixel
   color values.  Interrupt input must be blocked when this function
   is called.  */

void
x_free_colors (struct frame *f, unsigned long *pixels, int npixels)
{
  /* If display has an immutable color map, freeing colors is not
     necessary and some servers don't allow it.  So don't do it.  */
  if (x_mutable_colormap (FRAME_X_VISUAL_INFO (f)))
    {
#ifdef DEBUG_X_COLORS
      unregister_colors (pixels, npixels);
#endif
      XFreeColors (FRAME_X_DISPLAY (f), FRAME_X_COLORMAP (f),
		   pixels, npixels, 0);
    }
}


#ifdef USE_X_TOOLKIT

/* Free colors used on display DPY.  PIXELS is an array of NPIXELS pixel
   color values.  Interrupt input must be blocked when this function
   is called.  */

void
x_free_dpy_colors (Display *dpy, Screen *screen, Colormap cmap,
		   unsigned long *pixels, int npixels)
{
  struct x_display_info *dpyinfo = x_display_info_for_display (dpy);

  /* If display has an immutable color map, freeing colors is not
     necessary and some servers don't allow it.  So don't do it.  */
  if (x_mutable_colormap (&dpyinfo->visual_info))
    {
#ifdef DEBUG_X_COLORS
      unregister_colors (pixels, npixels);
#endif
      XFreeColors (dpy, cmap, pixels, npixels, 0);
    }
}
#endif /* USE_X_TOOLKIT */

/* Create and return a GC for use on frame F.  GC values and mask
   are given by XGCV and MASK.  */

static GC
x_create_gc (struct frame *f, unsigned long mask, XGCValues *xgcv)
{
  GC gc;
  block_input ();
  gc = XCreateGC (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f), mask, xgcv);
  unblock_input ();
  IF_DEBUG (++ngcs);
  return gc;
}


/* Free GC which was used on frame F.  */

static void
x_free_gc (struct frame *f, GC gc)
{
  eassert (input_blocked_p ());
  IF_DEBUG ((--ngcs, eassert (ngcs >= 0)));
  XFreeGC (FRAME_X_DISPLAY (f), gc);
}

#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_NTGUI
/* W32 emulation of GCs */

static Emacs_GC *
x_create_gc (struct frame *f, unsigned long mask, Emacs_GC *egc)
{
  Emacs_GC *gc;
  block_input ();
  gc = XCreateGC (NULL, FRAME_W32_WINDOW (f), mask, egc);
  unblock_input ();
  IF_DEBUG (++ngcs);
  return gc;
}


/* Free GC which was used on frame F.  */

static void
x_free_gc (struct frame *f, Emacs_GC *gc)
{
  IF_DEBUG ((--ngcs, eassert (ngcs >= 0)));
  xfree (gc);
}

#endif  /* HAVE_NTGUI */

#if defined (HAVE_NS) || defined (HAVE_HAIKU)
/* NS and Haiku emulation of GCs */

static Emacs_GC *
x_create_gc (struct frame *f,
	     unsigned long mask,
	     Emacs_GC *egc)
{
  Emacs_GC *gc = xmalloc (sizeof *gc);
  *gc = *egc;
  return gc;
}

static void
x_free_gc (struct frame *f, Emacs_GC *gc)
{
  xfree (gc);
}
#endif  /* HAVE_NS */

#ifdef HAVE_PGTK
/* PGTK emulation of GCs */

static Emacs_GC *
x_create_gc (struct frame *f,
	     unsigned long mask,
	     Emacs_GC *xgcv)
{
  Emacs_GC *gc = xmalloc (sizeof *gc);
  *gc = *xgcv;
  return gc;
}

static void
x_free_gc (struct frame *f, Emacs_GC *gc)
{
  xfree (gc);
}
#endif  /* HAVE_NS */

/***********************************************************************
			   Frames and faces
 ***********************************************************************/

/* Initialize face cache and basic faces for frame F.  */

void
init_frame_faces (struct frame *f)
{
  /* Make a face cache, if F doesn't have one.  */
  if (FRAME_FACE_CACHE (f) == NULL)
    FRAME_FACE_CACHE (f) = make_face_cache (f);

#ifdef HAVE_WINDOW_SYSTEM
  /* Make the image cache.  */
  if (FRAME_WINDOW_P (f))
    {
      /* We initialize the image cache when creating the first frame
	 on a terminal, and not during terminal creation.  This way,
	 `x-open-connection' on a tty won't create an image cache.  */
      if (FRAME_IMAGE_CACHE (f) == NULL)
	FRAME_IMAGE_CACHE (f) = make_image_cache ();
      ++FRAME_IMAGE_CACHE (f)->refcount;
    }
#endif /* HAVE_WINDOW_SYSTEM */

  /* Realize faces early (Bug#17889).  */
  if (!realize_basic_faces (f))
    emacs_abort ();
}


/* Free face cache of frame F.  Called from frame-dependent
   resource freeing function, e.g. (x|tty)_free_frame_resources.  */

void
free_frame_faces (struct frame *f)
{
  struct face_cache *face_cache = FRAME_FACE_CACHE (f);

  if (face_cache)
    {
      free_face_cache (face_cache);
      FRAME_FACE_CACHE (f) = NULL;
    }

#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    {
      struct image_cache *image_cache = FRAME_IMAGE_CACHE (f);
      if (image_cache)
	{
	  --image_cache->refcount;
	  if (image_cache->refcount == 0)
	    free_image_cache (f);
	}
    }
#endif /* HAVE_WINDOW_SYSTEM */
}


/* Clear face caches, and recompute basic faces for frame F.  Call
   this after changing frame parameters on which those faces depend,
   or when realized faces have been freed due to changing attributes
   of named faces.  */

void
recompute_basic_faces (struct frame *f)
{
  if (FRAME_FACE_CACHE (f))
    {
      clear_face_cache (false);
      if (!realize_basic_faces (f))
	emacs_abort ();
    }
}


/* Clear the face caches of all frames.  CLEAR_FONTS_P means
   try to free unused fonts, too.  */

void
clear_face_cache (bool clear_fonts_p)
{
#ifdef HAVE_WINDOW_SYSTEM
  Lisp_Object tail, frame;

  if (clear_fonts_p
      || ++clear_font_table_count == CLEAR_FONT_TABLE_COUNT)
    {
      /* From time to time see if we can unload some fonts.  This also
	 frees all realized faces on all frames.  Fonts needed by
	 faces will be loaded again when faces are realized again.  */
      clear_font_table_count = 0;

      FOR_EACH_FRAME (tail, frame)
	{
	  struct frame *f = XFRAME (frame);
	  if (FRAME_WINDOW_P (f)
	      && FRAME_DISPLAY_INFO (f)->n_fonts > CLEAR_FONT_TABLE_NFONTS
	      && !f->inhibit_clear_image_cache)
	    {
	      clear_font_cache (f);
	      free_all_realized_faces (frame);
	    }
	}
    }
  else
    {
      /* Clear GCs of realized faces.  */
      FOR_EACH_FRAME (tail, frame)
	{
	  struct frame *f = XFRAME (frame);
	  if (FRAME_WINDOW_P (f))
	      clear_face_gcs (FRAME_FACE_CACHE (f));
	}
      clear_image_caches (Qnil);
    }
#endif /* HAVE_WINDOW_SYSTEM */
}

DEFUN ("clear-face-cache", Fclear_face_cache, Sclear_face_cache, 0, 1, 0,
       doc: /* Clear face caches on all frames.
Optional THOROUGHLY non-nil means try to free unused fonts, too.  */)
  (Lisp_Object thoroughly)
{
  clear_face_cache (!NILP (thoroughly));
  face_change = true;
  windows_or_buffers_changed = 53;
  return Qnil;
}


/***********************************************************************
			      X Pixmaps
 ***********************************************************************/

#ifdef HAVE_WINDOW_SYSTEM

DEFUN ("bitmap-spec-p", Fbitmap_spec_p, Sbitmap_spec_p, 1, 1, 0,
       doc: /* Value is non-nil if OBJECT is a valid bitmap specification.
A bitmap specification is either a string, a file name, or a list
\(WIDTH HEIGHT DATA) where WIDTH is the pixel width of the bitmap,
HEIGHT is its height, and DATA is a string containing the bits of
the pixmap.  Bits are stored row by row, each row occupies
\(WIDTH + 7)/8 bytes.  */)
  (Lisp_Object object)
{
  bool pixmap_p = false;

  if (STRINGP (object))
    /* If OBJECT is a string, it's a file name.  */
    pixmap_p = true;
  else if (CONSP (object))
    {
      /* Otherwise OBJECT must be (WIDTH HEIGHT DATA), WIDTH and
	 HEIGHT must be ints > 0, and DATA must be string large
	 enough to hold a bitmap of the specified size.  */
      Lisp_Object width, height, data;

      height = width = data = Qnil;

      if (CONSP (object))
	{
	  width = XCAR (object);
	  object = XCDR (object);
	  if (CONSP (object))
	    {
	      height = XCAR (object);
	      object = XCDR (object);
	      if (CONSP (object))
		data = XCAR (object);
	    }
	}

      if (STRINGP (data)
	  && RANGED_FIXNUMP (1, width, INT_MAX)
	  && RANGED_FIXNUMP (1, height, INT_MAX))
	{
	  int bytes_per_row = (XFIXNUM (width) + CHAR_BIT - 1) / CHAR_BIT;
	  if (XFIXNUM (height) <= SBYTES (data) / bytes_per_row)
	    pixmap_p = true;
	}
    }

  return pixmap_p ? Qt : Qnil;
}


/* Load a bitmap according to NAME (which is either a file name or a
   pixmap spec) for use on frame F.  Value is the bitmap_id (see
   xfns.c).  If NAME is nil, return with a bitmap id of zero.  If
   bitmap cannot be loaded, display a message saying so, and return
   zero.  */

static ptrdiff_t
load_pixmap (struct frame *f, Lisp_Object name)
{
  ptrdiff_t bitmap_id;

  if (NILP (name))
    return 0;

  CHECK_TYPE (!NILP (Fbitmap_spec_p (name)), Qbitmap_spec_p, name);

  block_input ();
  if (CONSP (name))
    {
      /* Decode a bitmap spec into a bitmap.  */

      int h, w;
      Lisp_Object bits;

      w = XFIXNUM (Fcar (name));
      h = XFIXNUM (Fcar (Fcdr (name)));
      bits = Fcar (Fcdr (Fcdr (name)));

      bitmap_id = image_create_bitmap_from_data (f, SSDATA (bits),
                                                 w, h);
    }
  else
    {
      /* It must be a string -- a file name.  */
      bitmap_id = image_create_bitmap_from_file (f, name);
    }
  unblock_input ();

  if (bitmap_id < 0)
    {
      add_to_log ("Invalid or undefined bitmap `%s'", name);
      bitmap_id = 0;
    }
  else
    {
#ifdef GLYPH_DEBUG
      ++npixmaps_allocated;
#endif
    }

  return bitmap_id;
}

#endif /* HAVE_WINDOW_SYSTEM */



/***********************************************************************
                            Color Handling
 ***********************************************************************/

/* Parse hex color component specification that starts at S and ends
   right before E.  Set *DST to the parsed value normalized so that
   the maximum value for the number of hex digits given becomes 65535,
   and return true on success, false otherwise.  */
static bool
parse_hex_color_comp (const char *s, const char *e, unsigned short *dst)
{
  int n = e - s;
  if (n <= 0 || n > 4)
    return false;
  int val = 0;
  for (; s < e; s++)
    {
      int digit;
      if (*s >= '0' && *s <= '9')
        digit = *s - '0';
      else if (*s >= 'A' && *s <= 'F')
        digit = *s - 'A' + 10;
      else if (*s >= 'a' && *s <= 'f')
        digit = *s - 'a' + 10;
      else
        return false;
      val = (val << 4) | digit;
    }
  int maxval = (1 << (n * 4)) - 1;
  *dst = (unsigned)val * 65535 / maxval;
  return true;
}

/* Parse floating-point color component specification that starts at S
   and ends right before E.  Return the parsed number if in the range
   [0,1]; otherwise return -1.  */
static double
parse_float_color_comp (const char *s, const char *e)
{
  /* Only allow decimal float literals without whitespace.  */
  for (const char *p = s; p < e; p++)
    if (!((*p >= '0' && *p <= '9')
	  || *p == '.' || *p == '+' || *p == '-' || *p == 'e' || *p == 'E'))
      return -1;
  char *end;
  double x = strtod (s, &end);
  return (end == e && x >= 0 && x <= 1) ? x : -1;
}

/* Parse SPEC as a numeric color specification and set *R, *G and *B.
   Return true on success, false on failure.

   Recognized formats of SPEC:

    "#RGB", with R, G and B hex strings of equal length, 1-4 digits each.
    "rgb:R/G/B", with R, G and B hex strings, 1-4 digits each.
    "rgbi:R/G/B", with R, G and B numbers in [0,1].

   If the function succeeds, it assigns to each of the components *R,
   *G, and *B a value normalized to be in the [0, 65535] range.  If
   the function fails, some or all of the components remain unassigned.  */
bool
parse_color_spec (const char *spec,
                  unsigned short *r, unsigned short *g, unsigned short *b)
{
  int len = strlen (spec);
  if (spec[0] == '#')
    {
      if ((len - 1) % 3 == 0)
        {
          int n = (len - 1) / 3;
          return (   parse_hex_color_comp (spec + 1 + 0 * n,
					   spec + 1 + 1 * n, r)
                  && parse_hex_color_comp (spec + 1 + 1 * n,
					   spec + 1 + 2 * n, g)
                  && parse_hex_color_comp (spec + 1 + 2 * n,
					   spec + 1 + 3 * n, b));
        }
    }
  else if (strncmp (spec, "rgb:", 4) == 0)
    {
      char *sep1, *sep2;
      return ((sep1 = strchr (spec + 4, '/')) != NULL
              && (sep2 = strchr (sep1 + 1, '/')) != NULL
              && parse_hex_color_comp (spec + 4, sep1, r)
              && parse_hex_color_comp (sep1 + 1, sep2, g)
              && parse_hex_color_comp (sep2 + 1, spec + len, b));
    }
  else if (strncmp (spec, "rgbi:", 5) == 0)
    {
      char *sep1, *sep2;
      double red, green, blue;
      if ((sep1 = strchr (spec + 5, '/')) != NULL
          && (sep2 = strchr (sep1 + 1, '/')) != NULL
          && (red = parse_float_color_comp (spec + 5, sep1)) >= 0
          && (green = parse_float_color_comp (sep1 + 1, sep2)) >= 0
          && (blue = parse_float_color_comp (sep2 + 1, spec + len)) >= 0)
        {
          *r = lrint (red * 65535);
          *g = lrint (green * 65535);
          *b = lrint (blue * 65535);
          return true;
        }
    }
  return false;
}

DEFUN ("color-values-from-color-spec",
       Fcolor_values_from_color_spec,
       Scolor_values_from_color_spec,
       1, 1, 0,
       doc: /* Parse color SPEC as a numeric color and return (RED GREEN BLUE).
This function recognizes the following formats for SPEC:

 #RGB, where R, G and B are hex numbers of equal length, 1-4 digits each.
 rgb:R/G/B, where R, G, and B are hex numbers, 1-4 digits each.
 rgbi:R/G/B, where R, G and B are floating-point numbers in [0,1].

If SPEC is not in one of the above forms, return nil.

Each of the 3 integer members of the resulting list, RED, GREEN, and BLUE,
is normalized to have its value in [0,65535].  */)
  (Lisp_Object spec)
{
  CHECK_STRING (spec);
  unsigned short r, g, b;
  return (parse_color_spec (SSDATA (spec), &r, &g, &b)
          ? list3i (r, g, b)
          : Qnil);
}

/* Parse RGB_LIST, and fill in the RGB fields of COLOR.
   RGB_LIST should contain (at least) 3 lisp integers.
   Return true iff RGB_LIST is OK.  */

static bool
parse_rgb_list (Lisp_Object rgb_list, Emacs_Color *color)
{
#define PARSE_RGB_LIST_FIELD(field)					\
  if (CONSP (rgb_list) && FIXNUMP (XCAR (rgb_list)))			\
    {									\
      color->field = XFIXNUM (XCAR (rgb_list));				\
      rgb_list = XCDR (rgb_list);					\
    }									\
  else									\
    return false;

  PARSE_RGB_LIST_FIELD (red);
  PARSE_RGB_LIST_FIELD (green);
  PARSE_RGB_LIST_FIELD (blue);

  return true;
}


/* Lookup on frame F the color described by the lisp string COLOR.
   The resulting tty color is returned in TTY_COLOR; if STD_COLOR is
   non-zero, then the `standard' definition of the same color is
   returned in it.  */

static bool
tty_lookup_color (struct frame *f, Lisp_Object color, Emacs_Color *tty_color,
		  Emacs_Color *std_color)
{
  Lisp_Object frame, color_desc;

  if (!STRINGP (color) || NILP (Ffboundp (Qtty_color_desc)))
    return false;

  XSETFRAME (frame, f);

  color_desc = call2 (Qtty_color_desc, color, frame);
  if (CONSP (color_desc) && CONSP (XCDR (color_desc)))
    {
      Lisp_Object rgb;

      if (! FIXNUMP (XCAR (XCDR (color_desc))))
	return false;

      tty_color->pixel = XFIXNUM (XCAR (XCDR (color_desc)));

      rgb = XCDR (XCDR (color_desc));
      if (! parse_rgb_list (rgb, tty_color))
	return false;

      /* Should we fill in STD_COLOR too?  */
      if (std_color)
	{
	  /* Default STD_COLOR to the same as TTY_COLOR.  */
	  *std_color = *tty_color;

	  /* Do a quick check to see if the returned descriptor is
	     actually _exactly_ equal to COLOR, otherwise we have to
	     lookup STD_COLOR separately.  If it's impossible to lookup
	     a standard color, we just give up and use TTY_COLOR.  */
	  if ((!STRINGP (XCAR (color_desc))
	       || NILP (Fstring_equal (color, XCAR (color_desc))))
	      && !NILP (Ffboundp (Qtty_color_standard_values)))
	    {
	      /* Look up STD_COLOR separately.  */
	      rgb = call1 (Qtty_color_standard_values, color);
	      if (! parse_rgb_list (rgb, std_color))
		return false;
	    }
	}

      return true;
    }
  else if (NILP (Fsymbol_value (intern ("tty-defined-color-alist"))))
    /* We were called early during startup, and the colors are not
       yet set up in tty-defined-color-alist.  Don't return a failure
       indication, since this produces the annoying "Unable to
       load color" messages in the *Messages* buffer.  */
    return true;
  else
    /* tty-color-desc seems to have returned a bad value.  */
    return false;
}

/* An implementation of defined_color_hook for tty frames.  */

bool
tty_defined_color (struct frame *f, const char *color_name,
		   Emacs_Color *color_def, bool alloc, bool _makeIndex)
{
  bool status = true;

  /* Defaults.  */
  color_def->pixel = FACE_TTY_DEFAULT_COLOR;
  color_def->red = 0;
  color_def->blue = 0;
  color_def->green = 0;

  if (*color_name)
    status = tty_lookup_color (f, build_string (color_name), color_def, NULL);

  if (color_def->pixel == FACE_TTY_DEFAULT_COLOR && *color_name)
    {
      if (strcmp (color_name, "unspecified-fg") == 0)
	color_def->pixel = FACE_TTY_DEFAULT_FG_COLOR;
      else if (strcmp (color_name, "unspecified-bg") == 0)
	color_def->pixel = FACE_TTY_DEFAULT_BG_COLOR;
    }

  if (color_def->pixel != FACE_TTY_DEFAULT_COLOR)
    status = true;

  return status;
}

/* Given the index IDX of a tty color on frame F, return its name, a
   Lisp string.  */

Lisp_Object
tty_color_name (struct frame *f, int idx)
{
  if (idx >= 0 && !NILP (Ffboundp (Qtty_color_by_index)))
    {
      Lisp_Object frame;
      Lisp_Object coldesc;

      XSETFRAME (frame, f);
      coldesc = call2 (Qtty_color_by_index, make_fixnum (idx), frame);

      if (!NILP (coldesc))
	return XCAR (coldesc);
    }
#ifdef MSDOS
  /* We can have an MS-DOS frame under -nw for a short window of
     opportunity before internal_terminal_init is called.  DTRT.  */
  if (FRAME_MSDOS_P (f) && !inhibit_window_system)
    return msdos_stdcolor_name (idx);
#endif

  if (idx == FACE_TTY_DEFAULT_FG_COLOR)
    return build_string (unspecified_fg);
  if (idx == FACE_TTY_DEFAULT_BG_COLOR)
    return build_string (unspecified_bg);

  return Qunspecified;
}


/* Return true if COLOR_NAME is a shade of gray (or white or
   black) on frame F.

   The criterion implemented here is not a terribly sophisticated one.  */

static bool
face_color_gray_p (struct frame *f, const char *color_name)
{
  Emacs_Color color;
  bool gray_p;

  if (FRAME_TERMINAL (f)->defined_color_hook
      (f, color_name, &color, false, true))
    gray_p = (/* Any color sufficiently close to black counts as gray.  */
	      (color.red < 5000 && color.green < 5000 && color.blue < 5000)
	      ||
	      ((eabs (color.red - color.green)
		< max (color.red, color.green) / 20)
	       && (eabs (color.green - color.blue)
		   < max (color.green, color.blue) / 20)
	       && (eabs (color.blue - color.red)
		   < max (color.blue, color.red) / 20)));
  else
    gray_p = false;

  return gray_p;
}


/* Return true if color COLOR_NAME can be displayed on frame F.
   BACKGROUND_P means the color will be used as background color.  */

static bool
face_color_supported_p (struct frame *f, const char *color_name,
			bool background_p)
{
  Lisp_Object frame;
  Emacs_Color not_used;

  XSETFRAME (frame, f);
  return
#ifdef HAVE_WINDOW_SYSTEM
    FRAME_WINDOW_P (f)
    ? (!NILP (Fxw_display_color_p (frame))
       || xstrcasecmp (color_name, "black") == 0
       || xstrcasecmp (color_name, "white") == 0
       || (background_p
	   && face_color_gray_p (f, color_name))
       || (!NILP (Fx_display_grayscale_p (frame))
	   && face_color_gray_p (f, color_name)))
    :
#endif
    tty_defined_color (f, color_name, &not_used, false, false);
}


DEFUN ("color-gray-p", Fcolor_gray_p, Scolor_gray_p, 1, 2, 0,
       doc: /* Return non-nil if COLOR is a shade of gray (or white or black).
FRAME specifies the frame and thus the display for interpreting COLOR.
If FRAME is nil or omitted, use the selected frame.  */)
  (Lisp_Object color, Lisp_Object frame)
{
  CHECK_STRING (color);
  return (face_color_gray_p (decode_any_frame (frame), SSDATA (color))
	  ? Qt : Qnil);
}


DEFUN ("color-supported-p", Fcolor_supported_p,
       Scolor_supported_p, 1, 3, 0,
       doc: /* Return non-nil if COLOR can be displayed on FRAME.
BACKGROUND-P non-nil means COLOR is used as a background.
Otherwise, this function tells whether it can be used as a foreground.
If FRAME is nil or omitted, use the selected frame.
COLOR must be a valid color name.  */)
  (Lisp_Object color, Lisp_Object frame, Lisp_Object background_p)
{
  CHECK_STRING (color);
  return (face_color_supported_p (decode_any_frame (frame),
				  SSDATA (color), !NILP (background_p))
	  ? Qt : Qnil);
}


static unsigned long
load_color2 (struct frame *f, struct face *face, Lisp_Object name,
             enum lface_attribute_index target_index, Emacs_Color *color)
{
  eassert (STRINGP (name));
  eassert (target_index == LFACE_FOREGROUND_INDEX
	   || target_index == LFACE_BACKGROUND_INDEX
	   || target_index == LFACE_UNDERLINE_INDEX
	   || target_index == LFACE_OVERLINE_INDEX
	   || target_index == LFACE_STRIKE_THROUGH_INDEX
	   || target_index == LFACE_BOX_INDEX);

  /* if the color map is full, defined_color_hook will return a best match
     to the values in an existing cell. */
  if (!FRAME_TERMINAL (f)->defined_color_hook
      (f, SSDATA (name), color, true, true))
    {
      add_to_log ("Unable to load color \"%s\"", name);

      switch (target_index)
	{
	case LFACE_FOREGROUND_INDEX:
	  face->foreground_defaulted_p = true;
	  color->pixel = FRAME_FOREGROUND_PIXEL (f);
	  break;

	case LFACE_BACKGROUND_INDEX:
	  face->background_defaulted_p = true;
	  color->pixel = FRAME_BACKGROUND_PIXEL (f);
	  break;

	case LFACE_UNDERLINE_INDEX:
	  face->underline_defaulted_p = true;
	  color->pixel = FRAME_FOREGROUND_PIXEL (f);
	  break;

	case LFACE_OVERLINE_INDEX:
	  face->overline_color_defaulted_p = true;
	  color->pixel = FRAME_FOREGROUND_PIXEL (f);
	  break;

	case LFACE_STRIKE_THROUGH_INDEX:
	  face->strike_through_color_defaulted_p = true;
	  color->pixel = FRAME_FOREGROUND_PIXEL (f);
	  break;

	case LFACE_BOX_INDEX:
	  face->box_color_defaulted_p = true;
	  color->pixel = FRAME_FOREGROUND_PIXEL (f);
	  break;

	default:
	  emacs_abort ();
	}
    }
#ifdef GLYPH_DEBUG
  else
    ++ncolors_allocated;
#endif

  return color->pixel;
}

/* Load color with name NAME for use by face FACE on frame F.
   TARGET_INDEX must be one of LFACE_FOREGROUND_INDEX,
   LFACE_BACKGROUND_INDEX, LFACE_UNDERLINE_INDEX, LFACE_OVERLINE_INDEX,
   LFACE_STRIKE_THROUGH_INDEX, or LFACE_BOX_INDEX.  Value is the
   pixel color.  If color cannot be loaded, display a message, and
   return the foreground, background or underline color of F, but
   record that fact in flags of the face so that we don't try to free
   these colors.  */

unsigned long
load_color (struct frame *f, struct face *face, Lisp_Object name,
	    enum lface_attribute_index target_index)
{
  Emacs_Color color;
  return load_color2 (f, face, name, target_index, &color);
}


#ifdef HAVE_WINDOW_SYSTEM

/* Load colors for face FACE which is used on frame F.  Colors are
   specified by slots LFACE_BACKGROUND_INDEX and LFACE_FOREGROUND_INDEX
   of ATTRS.  If the background color specified is not supported on F,
   try to emulate gray colors with a stipple from Vface_default_stipple.  */

static void
load_face_colors (struct frame *f, struct face *face,
		  Lisp_Object attrs[LFACE_VECTOR_SIZE])
{
  Lisp_Object fg, bg, dfg;
  Emacs_Color xfg, xbg;

  bg = attrs[LFACE_BACKGROUND_INDEX];
  fg = attrs[LFACE_FOREGROUND_INDEX];

  /* Swap colors if face is inverse-video.  */
  if (EQ (attrs[LFACE_INVERSE_INDEX], Qt))
    {
      Lisp_Object tmp;
      tmp = fg;
      fg = bg;
      bg = tmp;
    }

  /* Check for support for foreground, not for background because
     face_color_supported_p is smart enough to know that grays are
     "supported" as background because we are supposed to use stipple
     for them.  */
  if (!face_color_supported_p (f, SSDATA (bg), false)
      && !NILP (Fbitmap_spec_p (Vface_default_stipple)))
    {
      image_destroy_bitmap (f, face->stipple);
      face->stipple = load_pixmap (f, Vface_default_stipple);
    }

  face->background = load_color2 (f, face, bg, LFACE_BACKGROUND_INDEX, &xbg);
  face->foreground = load_color2 (f, face, fg, LFACE_FOREGROUND_INDEX, &xfg);

  dfg = attrs[LFACE_DISTANT_FOREGROUND_INDEX];
  if (!NILP (dfg) && !UNSPECIFIEDP (dfg)
      && color_distance (&xbg, &xfg) < face_near_same_color_threshold)
    {
      if (EQ (attrs[LFACE_INVERSE_INDEX], Qt))
        face->background = load_color (f, face, dfg, LFACE_BACKGROUND_INDEX);
      else
        face->foreground = load_color (f, face, dfg, LFACE_FOREGROUND_INDEX);
    }
}

#ifdef HAVE_X_WINDOWS

/* Free color PIXEL on frame F.  */

void
unload_color (struct frame *f, unsigned long pixel)
{
  if (pixel != -1)
    {
      block_input ();
      x_free_colors (f, &pixel, 1);
      unblock_input ();
    }
}

/* Free colors allocated for FACE.  */

static void
free_face_colors (struct frame *f, struct face *face)
{
  /* PENDING(NS): need to do something here? */

  if (face->colors_copied_bitwise_p)
    return;

  block_input ();

  if (!face->foreground_defaulted_p)
    {
      x_free_colors (f, &face->foreground, 1);
      IF_DEBUG (--ncolors_allocated);
    }

  if (!face->background_defaulted_p)
    {
      x_free_colors (f, &face->background, 1);
      IF_DEBUG (--ncolors_allocated);
    }

  if (face->underline
      && !face->underline_defaulted_p)
    {
      x_free_colors (f, &face->underline_color, 1);
      IF_DEBUG (--ncolors_allocated);
    }

  if (face->overline_p
      && !face->overline_color_defaulted_p)
    {
      x_free_colors (f, &face->overline_color, 1);
      IF_DEBUG (--ncolors_allocated);
    }

  if (face->strike_through_p
      && !face->strike_through_color_defaulted_p)
    {
      x_free_colors (f, &face->strike_through_color, 1);
      IF_DEBUG (--ncolors_allocated);
    }

  if (face->box != FACE_NO_BOX
      && !face->box_color_defaulted_p)
    {
      x_free_colors (f, &face->box_color, 1);
      IF_DEBUG (--ncolors_allocated);
    }

  unblock_input ();
}

#endif /* HAVE_X_WINDOWS */

#endif /* HAVE_WINDOW_SYSTEM */



/***********************************************************************
			   XLFD Font Names
 ***********************************************************************/

/* An enumerator for each field of an XLFD font name.  */

enum xlfd_field
{
  XLFD_FOUNDRY,
  XLFD_FAMILY,
  XLFD_WEIGHT,
  XLFD_SLANT,
  XLFD_SWIDTH,
  XLFD_ADSTYLE,
  XLFD_PIXEL_SIZE,
  XLFD_POINT_SIZE,
  XLFD_RESX,
  XLFD_RESY,
  XLFD_SPACING,
  XLFD_AVGWIDTH,
  XLFD_REGISTRY,
  XLFD_ENCODING,
  XLFD_LAST
};

/* Order by which font selection chooses fonts.  The default values
   mean "first, find a best match for the font width, then for the
   font height, then for weight, then for slant."  This variable can be
   set via 'internal-set-font-selection-order'.  */

static int font_sort_order[4];

#ifdef HAVE_WINDOW_SYSTEM

static enum font_property_index font_props_for_sorting[FONT_SIZE_INDEX];

static int
compare_fonts_by_sort_order (const void *v1, const void *v2)
{
  Lisp_Object const *p1 = v1;
  Lisp_Object const *p2 = v2;
  Lisp_Object font1 = *p1;
  Lisp_Object font2 = *p2;
  int i;

  for (i = 0; i < FONT_SIZE_INDEX; i++)
    {
      enum font_property_index idx = font_props_for_sorting[i];
      Lisp_Object val1 = AREF (font1, idx), val2 = AREF (font2, idx);
      int result;

      if (idx <= FONT_REGISTRY_INDEX)
	{
	  if (STRINGP (val1))
	    result = STRINGP (val2) ? strcmp (SSDATA (val1), SSDATA (val2)) : -1;
	  else
	    result = STRINGP (val2) ? 1 : 0;
	}
      else
	{
	  if (FIXNUMP (val1))
	    result = (FIXNUMP (val2) && XFIXNUM (val1) >= XFIXNUM (val2)
		      ? XFIXNUM (val1) > XFIXNUM (val2)
		      : -1);
	  else
	    result = FIXNUMP (val2) ? 1 : 0;
	}
      if (result)
	return result;
    }
  return 0;
}

DEFUN ("x-family-fonts", Fx_family_fonts, Sx_family_fonts, 0, 2, 0,
       doc: /* Return a list of available fonts of family FAMILY on FRAME.
If FAMILY is omitted or nil, list all families.
Otherwise, FAMILY must be a string, possibly containing wildcards
`?' and `*'.
If FRAME is omitted or nil, use the selected frame.

Each element of the result is a vector [FAMILY WIDTH POINT-SIZE WEIGHT
SLANT FIXED-P FULL REGISTRY-AND-ENCODING].

FAMILY is the font family name.
POINT-SIZE is the size of the font in 1/10 pt.
WIDTH, WEIGHT, and SLANT are symbols describing the width, weight
  and slant of the font.  These symbols are the same as for face
  attributes, see `set-face-attribute'.
FIXED-P is non-nil if the font is fixed-pitch.
FULL is the full name of the font.
REGISTRY-AND-ENCODING is a string giving the registry and encoding of
  the font.

The resulting list is sorted according to the current setting of
the face font sort order, see `face-font-selection-order'.  */)
  (Lisp_Object family, Lisp_Object frame)
{
  Lisp_Object font_spec, list, *drivers, vec;
  struct frame *f = decode_live_frame (frame);
  ptrdiff_t i, nfonts;
  Lisp_Object result;
  USE_SAFE_ALLOCA;

  font_spec = Ffont_spec (0, NULL);
  if (!NILP (family))
    {
      CHECK_STRING (family);
      font_parse_family_registry (family, Qnil, font_spec);
    }

  list = font_list_entities (f, font_spec);
  if (NILP (list))
    return Qnil;

  /* Sort the font entities.  */
  for (i = 0; i < 4; i++)
    switch (font_sort_order[i])
      {
      case XLFD_SWIDTH:
	font_props_for_sorting[i] = FONT_WIDTH_INDEX; break;
      case XLFD_POINT_SIZE:
	font_props_for_sorting[i] = FONT_SIZE_INDEX; break;
      case XLFD_WEIGHT:
	font_props_for_sorting[i] = FONT_WEIGHT_INDEX; break;
      default:
	font_props_for_sorting[i] = FONT_SLANT_INDEX; break;
      }
  font_props_for_sorting[i++] = FONT_FAMILY_INDEX;
  font_props_for_sorting[i++] = FONT_FOUNDRY_INDEX;
  font_props_for_sorting[i++] = FONT_ADSTYLE_INDEX;
  font_props_for_sorting[i++] = FONT_REGISTRY_INDEX;

  ptrdiff_t ndrivers = list_length (list);
  SAFE_ALLOCA_LISP (drivers, ndrivers);
  for (i = 0; i < ndrivers; i++, list = XCDR (list))
    drivers[i] = XCAR (list);
  vec = Fvconcat (ndrivers, drivers);
  nfonts = ASIZE (vec);

  qsort (XVECTOR (vec)->contents, nfonts, word_size,
	 compare_fonts_by_sort_order);

  result = Qnil;
  for (i = nfonts - 1; i >= 0; --i)
    {
      Lisp_Object font = AREF (vec, i);
      int point = PIXEL_TO_POINT (XFIXNUM (AREF (font, FONT_SIZE_INDEX)) * 10,
				  FRAME_RES_Y (f));
      Lisp_Object spacing = Ffont_get (font, QCspacing);
      Lisp_Object v = CALLN (Fvector,
			     AREF (font, FONT_FAMILY_INDEX),
			     FONT_WIDTH_SYMBOLIC (font),
			     make_fixnum (point),
			     FONT_WEIGHT_SYMBOLIC (font),
			     FONT_SLANT_SYMBOLIC (font),
			     (NILP (spacing)
			      || EQ (spacing, Qp)
			      /* If the font was specified in a way
				 different from XLFD (e.g., on MS-Windows),
				 we will have a number there, not 'p'.  */
			      || BASE_EQ (spacing,
					  make_fixnum
					  (FONT_SPACING_PROPORTIONAL)))
			     ? Qnil : Qt,
			     Ffont_xlfd_name (font, Qnil),
			     AREF (font, FONT_REGISTRY_INDEX));
      result = Fcons (v, result);
    }

  SAFE_FREE ();
  return result;
}

DEFUN ("x-list-fonts", Fx_list_fonts, Sx_list_fonts, 1, 5, 0,
       doc: /* Return a list of the names of available fonts matching PATTERN.
If optional arguments FACE and FRAME are specified, return only fonts
the same size as FACE on FRAME.

PATTERN should be a string containing a font name in the XLFD,
Fontconfig, or GTK format.  A font name given in the XLFD format may
contain wildcard characters:
  the * character matches any substring, and
  the ? character matches any single character.
  PATTERN is case-insensitive.

The return value is a list of strings, suitable as arguments to
`set-face-font'.

Fonts Emacs can't use may or may not be excluded
even if they match PATTERN and FACE.
The optional fourth argument MAXIMUM sets a limit on how many
fonts to match.  The first MAXIMUM fonts are reported.
The optional fifth argument WIDTH, if specified, is a number of columns
occupied by a character of a font.  In that case, return only fonts
the WIDTH times as wide as FACE on FRAME.  */)
  (Lisp_Object pattern, Lisp_Object face, Lisp_Object frame,
   Lisp_Object maximum, Lisp_Object width)
{
  struct frame *f;
  int size, avgwidth;

  check_window_system (NULL);
  CHECK_STRING (pattern);

  if (! NILP (maximum))
    CHECK_FIXNAT (maximum);

  if (!NILP (width))
    CHECK_FIXNUM (width);

  /* We can't simply call decode_window_system_frame because
     this function may be called before any frame is created.  */
  f = decode_live_frame (frame);
  if (! FRAME_WINDOW_P (f))
    {
      /* Perhaps we have not yet created any frame.  */
      f = NULL;
      frame = Qnil;
      face = Qnil;
    }
  else
    XSETFRAME (frame, f);

  /* Determine the width standard for comparison with the fonts we find.  */

  if (NILP (face))
    size = 0;
  else
    {
      /* This is of limited utility since it works with character
	 widths.  Keep it for compatibility.  --gerd.  */
      int face_id = lookup_named_face (NULL, f, face, false);
      struct face *width_face = FACE_FROM_ID_OR_NULL (f, face_id);

      if (width_face && width_face->font)
	{
	  size = width_face->font->pixel_size;
	  avgwidth = width_face->font->average_width;
	}
      else
	{
	  size = FRAME_FONT (f)->pixel_size;
	  avgwidth = FRAME_FONT (f)->average_width;
	}
      if (!NILP (width))
	avgwidth *= XFIXNUM (width);
    }

  Lisp_Object font_spec = font_spec_from_name (pattern);
  if (!FONTP (font_spec))
    signal_error ("Invalid font name", pattern);

  if (size)
    {
      Ffont_put (font_spec, QCsize, make_fixnum (size));
      Ffont_put (font_spec, QCavgwidth, make_fixnum (avgwidth));
    }
  Lisp_Object fonts = Flist_fonts (font_spec, frame, maximum, font_spec);
  for (Lisp_Object tail = fonts; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object font_entity;

      font_entity = XCAR (tail);
      if ((NILP (AREF (font_entity, FONT_SIZE_INDEX))
	   || XFIXNUM (AREF (font_entity, FONT_SIZE_INDEX)) == 0)
	  && ! NILP (AREF (font_spec, FONT_SIZE_INDEX)))
	{
	  /* This is a scalable font.  For backward compatibility,
	     we set the specified size. */
	  font_entity = copy_font_spec (font_entity);
	  ASET (font_entity, FONT_SIZE_INDEX,
		AREF (font_spec, FONT_SIZE_INDEX));
	}
      XSETCAR (tail, Ffont_xlfd_name (font_entity, Qnil));
    }
  if (NILP (frame))
    /* We don't have to check fontsets.  */
    return fonts;
  Lisp_Object fontsets = list_fontsets (f, pattern, size);
  return nconc2 (fonts, fontsets);
}

#endif /* HAVE_WINDOW_SYSTEM */


/***********************************************************************
			      Lisp Faces
 ***********************************************************************/

/* Access face attributes of face LFACE, a Lisp vector.  */

#define LFACE_FAMILY(LFACE)	    AREF ((LFACE), LFACE_FAMILY_INDEX)
#define LFACE_FOUNDRY(LFACE)	    AREF ((LFACE), LFACE_FOUNDRY_INDEX)
#define LFACE_HEIGHT(LFACE)	    AREF ((LFACE), LFACE_HEIGHT_INDEX)
#define LFACE_WEIGHT(LFACE)	    AREF ((LFACE), LFACE_WEIGHT_INDEX)
#define LFACE_SLANT(LFACE)	    AREF ((LFACE), LFACE_SLANT_INDEX)
#define LFACE_UNDERLINE(LFACE)      AREF ((LFACE), LFACE_UNDERLINE_INDEX)
#define LFACE_INVERSE(LFACE)	    AREF ((LFACE), LFACE_INVERSE_INDEX)
#define LFACE_FOREGROUND(LFACE)     AREF ((LFACE), LFACE_FOREGROUND_INDEX)
#define LFACE_BACKGROUND(LFACE)     AREF ((LFACE), LFACE_BACKGROUND_INDEX)
#define LFACE_STIPPLE(LFACE)	    AREF ((LFACE), LFACE_STIPPLE_INDEX)
#define LFACE_SWIDTH(LFACE)	    AREF ((LFACE), LFACE_SWIDTH_INDEX)
#define LFACE_OVERLINE(LFACE)	    AREF ((LFACE), LFACE_OVERLINE_INDEX)
#define LFACE_STRIKE_THROUGH(LFACE) AREF ((LFACE), LFACE_STRIKE_THROUGH_INDEX)
#define LFACE_BOX(LFACE)	    AREF ((LFACE), LFACE_BOX_INDEX)
#define LFACE_FONT(LFACE)	    AREF ((LFACE), LFACE_FONT_INDEX)
#define LFACE_INHERIT(LFACE)	    AREF ((LFACE), LFACE_INHERIT_INDEX)
#define LFACE_FONTSET(LFACE)	    AREF ((LFACE), LFACE_FONTSET_INDEX)
#define LFACE_EXTEND(LFACE)	    AREF ((LFACE), LFACE_EXTEND_INDEX)
#define LFACE_DISTANT_FOREGROUND(LFACE) \
  AREF ((LFACE), LFACE_DISTANT_FOREGROUND_INDEX)

/* True if LFACE is a Lisp face.  A Lisp face is a vector of size
   LFACE_VECTOR_SIZE which has the symbol `face' in slot 0.  */

#define LFACEP(LFACE)					\
     (VECTORP (LFACE)					\
      && ASIZE (LFACE) == LFACE_VECTOR_SIZE		\
      && EQ (AREF (LFACE, 0), Qface))


/* Face attribute symbols for each value of LFACE_*_INDEX.  */
static Lisp_Object face_attr_sym[LFACE_VECTOR_SIZE];

#ifdef GLYPH_DEBUG

/* Check consistency of Lisp face attribute vector ATTRS.  */

static void
check_lface_attrs (Lisp_Object attrs[LFACE_VECTOR_SIZE])
{
  eassert (UNSPECIFIEDP (attrs[LFACE_FAMILY_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_FAMILY_INDEX])
	   || RESET_P (attrs[LFACE_FAMILY_INDEX])
	   || STRINGP (attrs[LFACE_FAMILY_INDEX]));
  eassert (UNSPECIFIEDP (attrs[LFACE_FOUNDRY_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_FOUNDRY_INDEX])
	   || RESET_P (attrs[LFACE_FOUNDRY_INDEX])
	   || STRINGP (attrs[LFACE_FOUNDRY_INDEX]));
  eassert (UNSPECIFIEDP (attrs[LFACE_SWIDTH_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_SWIDTH_INDEX])
	   || RESET_P (attrs[LFACE_SWIDTH_INDEX])
	   || SYMBOLP (attrs[LFACE_SWIDTH_INDEX]));
  eassert (UNSPECIFIEDP (attrs[LFACE_HEIGHT_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_HEIGHT_INDEX])
	   || RESET_P (attrs[LFACE_HEIGHT_INDEX])
	   || NUMBERP (attrs[LFACE_HEIGHT_INDEX])
	   || FUNCTIONP (attrs[LFACE_HEIGHT_INDEX]));
  eassert (UNSPECIFIEDP (attrs[LFACE_WEIGHT_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_WEIGHT_INDEX])
	   || RESET_P (attrs[LFACE_WEIGHT_INDEX])
	   || SYMBOLP (attrs[LFACE_WEIGHT_INDEX]));
  eassert (UNSPECIFIEDP (attrs[LFACE_SLANT_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_SLANT_INDEX])
	   || RESET_P (attrs[LFACE_SLANT_INDEX])
	   || SYMBOLP (attrs[LFACE_SLANT_INDEX]));
  eassert (UNSPECIFIEDP (attrs[LFACE_UNDERLINE_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_UNDERLINE_INDEX])
	   || RESET_P (attrs[LFACE_UNDERLINE_INDEX])
	   || SYMBOLP (attrs[LFACE_UNDERLINE_INDEX])
	   || STRINGP (attrs[LFACE_UNDERLINE_INDEX])
	   || CONSP (attrs[LFACE_UNDERLINE_INDEX]));
  eassert (UNSPECIFIEDP (attrs[LFACE_EXTEND_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_EXTEND_INDEX])
	   || RESET_P (attrs[LFACE_EXTEND_INDEX])
	   || SYMBOLP (attrs[LFACE_EXTEND_INDEX])
	   || STRINGP (attrs[LFACE_EXTEND_INDEX]));
  eassert (UNSPECIFIEDP (attrs[LFACE_OVERLINE_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_OVERLINE_INDEX])
	   || RESET_P (attrs[LFACE_OVERLINE_INDEX])
	   || SYMBOLP (attrs[LFACE_OVERLINE_INDEX])
	   || STRINGP (attrs[LFACE_OVERLINE_INDEX]));
  eassert (UNSPECIFIEDP (attrs[LFACE_STRIKE_THROUGH_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_STRIKE_THROUGH_INDEX])
	   || RESET_P (attrs[LFACE_STRIKE_THROUGH_INDEX])
	   || SYMBOLP (attrs[LFACE_STRIKE_THROUGH_INDEX])
	   || STRINGP (attrs[LFACE_STRIKE_THROUGH_INDEX]));
  eassert (UNSPECIFIEDP (attrs[LFACE_BOX_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_BOX_INDEX])
	   || RESET_P (attrs[LFACE_BOX_INDEX])
	   || SYMBOLP (attrs[LFACE_BOX_INDEX])
	   || STRINGP (attrs[LFACE_BOX_INDEX])
	   || FIXNUMP (attrs[LFACE_BOX_INDEX])
	   || CONSP (attrs[LFACE_BOX_INDEX]));
  eassert (UNSPECIFIEDP (attrs[LFACE_INVERSE_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_INVERSE_INDEX])
	   || RESET_P (attrs[LFACE_INVERSE_INDEX])
	   || SYMBOLP (attrs[LFACE_INVERSE_INDEX]));
  eassert (UNSPECIFIEDP (attrs[LFACE_FOREGROUND_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_FOREGROUND_INDEX])
	   || RESET_P (attrs[LFACE_FOREGROUND_INDEX])
	   || STRINGP (attrs[LFACE_FOREGROUND_INDEX]));
  eassert (UNSPECIFIEDP (attrs[LFACE_DISTANT_FOREGROUND_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_DISTANT_FOREGROUND_INDEX])
	   || RESET_P (attrs[LFACE_DISTANT_FOREGROUND_INDEX])
	   || STRINGP (attrs[LFACE_DISTANT_FOREGROUND_INDEX]));
  eassert (UNSPECIFIEDP (attrs[LFACE_BACKGROUND_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_BACKGROUND_INDEX])
	   || RESET_P (attrs[LFACE_BACKGROUND_INDEX])
	   || STRINGP (attrs[LFACE_BACKGROUND_INDEX]));
  eassert (UNSPECIFIEDP (attrs[LFACE_INHERIT_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_INHERIT_INDEX])
	   || NILP (attrs[LFACE_INHERIT_INDEX])
	   || SYMBOLP (attrs[LFACE_INHERIT_INDEX])
	   || CONSP (attrs[LFACE_INHERIT_INDEX]));
#ifdef HAVE_WINDOW_SYSTEM
  eassert (UNSPECIFIEDP (attrs[LFACE_STIPPLE_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_STIPPLE_INDEX])
	   || RESET_P (attrs[LFACE_STIPPLE_INDEX])
	   || SYMBOLP (attrs[LFACE_STIPPLE_INDEX])
	   || !NILP (Fbitmap_spec_p (attrs[LFACE_STIPPLE_INDEX])));
  eassert (UNSPECIFIEDP (attrs[LFACE_FONT_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_FONT_INDEX])
	   || RESET_P (attrs[LFACE_FONT_INDEX])
	   || FONTP (attrs[LFACE_FONT_INDEX]));
  eassert (UNSPECIFIEDP (attrs[LFACE_FONTSET_INDEX])
	   || STRINGP (attrs[LFACE_FONTSET_INDEX])
	   || RESET_P (attrs[LFACE_FONTSET_INDEX])
	   || NILP (attrs[LFACE_FONTSET_INDEX]));
#endif
}


/* Check consistency of attributes of Lisp face LFACE (a Lisp vector).  */

static void
check_lface (Lisp_Object lface)
{
  if (!NILP (lface))
    {
      eassert (LFACEP (lface));
      check_lface_attrs (XVECTOR (lface)->contents);
    }
}

#else /* not GLYPH_DEBUG */

#define check_lface_attrs(attrs)	(void) 0
#define check_lface(lface)		(void) 0

#endif /* GLYPH_DEBUG */



/* Face-merge cycle checking.  */

enum named_merge_point_kind
{
  NAMED_MERGE_POINT_NORMAL,
  NAMED_MERGE_POINT_REMAP
};

/* A `named merge point' is simply a point during face-merging where we
   look up a face by name.  We keep a stack of which named lookups we're
   currently processing so that we can easily detect cycles, using a
   linked- list of struct named_merge_point structures, typically
   allocated on the stack frame of the named lookup functions which are
   active (so no consing is required).  */
struct named_merge_point
{
  Lisp_Object face_name;
  enum named_merge_point_kind named_merge_point_kind;
  struct named_merge_point *prev;
};


/* If a face merging cycle is detected for FACE_NAME, return false,
   otherwise add NEW_NAMED_MERGE_POINT, which is initialized using
   FACE_NAME and NAMED_MERGE_POINT_KIND, as the head of the linked list
   pointed to by NAMED_MERGE_POINTS, and return true.  */

static bool
push_named_merge_point (struct named_merge_point *new_named_merge_point,
			Lisp_Object face_name,
			enum named_merge_point_kind named_merge_point_kind,
			struct named_merge_point **named_merge_points)
{
  struct named_merge_point *prev;

  for (prev = *named_merge_points; prev; prev = prev->prev)
    if (EQ (face_name, prev->face_name))
      {
	if (prev->named_merge_point_kind == named_merge_point_kind)
	  /* A cycle, so fail.  */
	  return false;
	else if (prev->named_merge_point_kind == NAMED_MERGE_POINT_REMAP)
	  /* A remap `hides ' any previous normal merge points
	     (because the remap means that it's actually different face),
	     so as we know the current merge point must be normal, we
	     can just assume it's OK.  */
	  break;
      }

  new_named_merge_point->face_name = face_name;
  new_named_merge_point->named_merge_point_kind = named_merge_point_kind;
  new_named_merge_point->prev = *named_merge_points;

  *named_merge_points = new_named_merge_point;

  return true;
}


/* Resolve face name FACE_NAME.  If FACE_NAME is a string, intern it
   to make it a symbol.  If FACE_NAME is an alias for another face,
   return that face's name.

   Return default face in case of errors.  */

static Lisp_Object
resolve_face_name (Lisp_Object face_name, bool signal_p)
{
  Lisp_Object orig_face;
  Lisp_Object tortoise, hare;

  if (STRINGP (face_name))
    face_name = Fintern (face_name, Qnil);

  if (NILP (face_name) || !SYMBOLP (face_name))
    return face_name;

  orig_face = face_name;
  tortoise = hare = face_name;

  while (true)
    {
      face_name = hare;
      hare = Fget (hare, Qface_alias);
      if (NILP (hare) || !SYMBOLP (hare))
	break;

      face_name = hare;
      hare = Fget (hare, Qface_alias);
      if (NILP (hare) || !SYMBOLP (hare))
	break;

      tortoise = Fget (tortoise, Qface_alias);
      if (BASE_EQ (hare, tortoise))
	{
	  if (signal_p)
	    circular_list (orig_face);
	  return Qdefault;
	}
    }

  return face_name;
}


/* Return the face definition of FACE_NAME on frame F.  F null means
   return the definition for new frames.  FACE_NAME may be a string or
   a symbol (apparently Emacs 20.2 allowed strings as face names in
   face text properties; Ediff uses that).
   If SIGNAL_P, signal an error if FACE_NAME is not a valid face name.
   Otherwise, value is nil if FACE_NAME is not a valid face name.  */
static Lisp_Object
lface_from_face_name_no_resolve (struct frame *f, Lisp_Object face_name,
				 bool signal_p)
{
  Lisp_Object lface;

  if (f)
    lface = Fgethash (face_name, f->face_hash_table, Qnil);
  else
    lface = CDR (Fgethash (face_name, Vface_new_frame_defaults, Qnil));

  if (signal_p && NILP (lface))
    signal_error ("Invalid face", face_name);

  check_lface (lface);

  return lface;
}

/* Return the face definition of FACE_NAME on frame F.  F null means
   return the definition for new frames.  FACE_NAME may be a string or
   a symbol (apparently Emacs 20.2 allowed strings as face names in
   face text properties; Ediff uses that).  If FACE_NAME is an alias
   for another face, return that face's definition.
   If SIGNAL_P, signal an error if FACE_NAME is not a valid face name.
   Otherwise, value is nil if FACE_NAME is not a valid face name.  */
static Lisp_Object
lface_from_face_name (struct frame *f, Lisp_Object face_name, bool signal_p)
{
  face_name = resolve_face_name (face_name, signal_p);
  return lface_from_face_name_no_resolve (f, face_name, signal_p);
}


/* Get face attributes of face FACE_NAME from frame-local faces on
   frame F.  Store the resulting attributes in ATTRS which must point
   to a vector of Lisp_Objects of size LFACE_VECTOR_SIZE.
   If SIGNAL_P, signal an error if FACE_NAME does not name a face.
   Otherwise, return true iff FACE_NAME is a face.  */

static bool
get_lface_attributes_no_remap (struct frame *f, Lisp_Object face_name,
			       Lisp_Object attrs[LFACE_VECTOR_SIZE],
			       bool signal_p)
{
  Lisp_Object lface;

  lface = lface_from_face_name_no_resolve (f, face_name, signal_p);

  if (! NILP (lface))
    memcpy (attrs, xvector_contents (lface),
	    LFACE_VECTOR_SIZE * sizeof *attrs);

  return !NILP (lface);
}

/* Get face attributes of face FACE_NAME from frame-local faces on
   frame F.  Store the resulting attributes in ATTRS which must point
   to a vector of Lisp_Objects of size LFACE_VECTOR_SIZE.
   If FACE_NAME is an alias for another face, use that face's
   definition.  If SIGNAL_P, signal an error if FACE_NAME does not
   name a face.  Otherwise, return true iff FACE_NAME is a face.  If W
   is non-NULL, also consider remappings attached to the window.
   */
static bool
get_lface_attributes (struct window *w,
                      struct frame *f, Lisp_Object face_name,
		      Lisp_Object attrs[LFACE_VECTOR_SIZE], bool signal_p,
		      struct named_merge_point *named_merge_points)
{
  Lisp_Object face_remapping;
  eassert (w == NULL || WINDOW_XFRAME (w) == f);

  face_name = resolve_face_name (face_name, signal_p);

  /* See if SYMBOL has been remapped to some other face (usually this
     is done buffer-locally).  */
  face_remapping = assq_no_quit (face_name, Vface_remapping_alist);
  if (CONSP (face_remapping))
    {
      struct named_merge_point named_merge_point;

      if (push_named_merge_point (&named_merge_point,
				  face_name, NAMED_MERGE_POINT_REMAP,
				  &named_merge_points))
	{
	  int i;

	  for (i = 1; i < LFACE_VECTOR_SIZE; ++i)
	    attrs[i] = Qunspecified;

	  return merge_face_ref (w, f, XCDR (face_remapping), attrs,
	                         signal_p, named_merge_points,
	                         0);
	}
    }

  /* Default case, no remapping.  */
  return get_lface_attributes_no_remap (f, face_name, attrs, signal_p);
}


/* True iff all attributes in face attribute vector ATTRS are
   specified, i.e. are non-nil.  */

static bool
lface_fully_specified_p (Lisp_Object attrs[LFACE_VECTOR_SIZE])
{
  int i;

  for (i = 1; i < LFACE_VECTOR_SIZE; ++i)
    if (i != LFACE_FONT_INDEX && i != LFACE_INHERIT_INDEX
        && i != LFACE_DISTANT_FOREGROUND_INDEX)
      if ((UNSPECIFIEDP (attrs[i]) || IGNORE_DEFFACE_P (attrs[i])))
	break;

  return i == LFACE_VECTOR_SIZE;
}

#ifdef HAVE_WINDOW_SYSTEM

/* Set font-related attributes of Lisp face LFACE from FONT-OBJECT.
   If FORCE_P is zero, set only unspecified attributes of LFACE.  The
   exception is `font' attribute.  It is set to FONT_OBJECT regardless
   of FORCE_P.  */

static void
set_lface_from_font (struct frame *f, Lisp_Object lface,
		     Lisp_Object font_object, bool force_p)
{
  Lisp_Object val;
  struct font *font = XFONT_OBJECT (font_object);

  /* Set attributes only if unspecified, otherwise face defaults for
     new frames would never take effect.  If the font doesn't have a
     specific property, set a normal value for that.  */

  if (force_p || UNSPECIFIEDP (LFACE_FAMILY (lface)))
    {
      Lisp_Object family = AREF (font_object, FONT_FAMILY_INDEX);

      ASET (lface, LFACE_FAMILY_INDEX, SYMBOL_NAME (family));
    }

  if (force_p || UNSPECIFIEDP (LFACE_FOUNDRY (lface)))
    {
      Lisp_Object foundry = AREF (font_object, FONT_FOUNDRY_INDEX);

      ASET (lface, LFACE_FOUNDRY_INDEX, SYMBOL_NAME (foundry));
    }

  if (force_p || UNSPECIFIEDP (LFACE_HEIGHT (lface)))
    {
      int pt = PIXEL_TO_POINT (font->pixel_size * 10, FRAME_RES_Y (f));

      eassert (pt > 0);
      ASET (lface, LFACE_HEIGHT_INDEX, make_fixnum (pt));
    }

  if (force_p || UNSPECIFIEDP (LFACE_WEIGHT (lface)))
    {
      val = FONT_WEIGHT_FOR_FACE (font_object);
      ASET (lface, LFACE_WEIGHT_INDEX, ! NILP (val) ? val :Qnormal);
    }
  if (force_p || UNSPECIFIEDP (LFACE_SLANT (lface)))
    {
      val = FONT_SLANT_FOR_FACE (font_object);
      ASET (lface, LFACE_SLANT_INDEX, ! NILP (val) ? val : Qnormal);
    }
  if (force_p || UNSPECIFIEDP (LFACE_SWIDTH (lface)))
    {
      val = FONT_WIDTH_FOR_FACE (font_object);
      ASET (lface, LFACE_SWIDTH_INDEX, ! NILP (val) ? val : Qnormal);
    }

  ASET (lface, LFACE_FONT_INDEX, font_object);
}

#endif /* HAVE_WINDOW_SYSTEM */


/* Merges the face height FROM with the face height TO, and returns the
   merged height.  If FROM is an invalid height, then INVALID is
   returned instead.  FROM and TO may be either absolute face heights or
   `relative' heights; the returned value is always an absolute height
   unless both FROM and TO are relative.  */

static Lisp_Object
merge_face_heights (Lisp_Object from, Lisp_Object to, Lisp_Object invalid)
{
  Lisp_Object result = invalid;

  if (FIXNUMP (from))
    /* FROM is absolute, just use it as is.  */
    result = from;
  else if (FLOATP (from))
    /* FROM is a scale, use it to adjust TO.  */
    {
      if (FIXNUMP (to))
	/* relative X absolute => absolute */
	result = make_fixnum (XFLOAT_DATA (from) * XFIXNUM (to));
      else if (FLOATP (to))
	/* relative X relative => relative */
	result = make_float (XFLOAT_DATA (from) * XFLOAT_DATA (to));
      else if (UNSPECIFIEDP (to))
	result = from;
    }
  else if (FUNCTIONP (from))
    /* FROM is a function, which use to adjust TO.  */
    {
      /* Call function with current height as argument.
	 From is the new height.  */
      result = safe_call1 (from, to);

      /* Ensure that if TO was absolute, so is the result.  */
      if (FIXNUMP (to) && !FIXNUMP (result))
	result = invalid;
    }

  return result;
}


/* Merge two Lisp face attribute vectors on frame F, FROM and TO, and
   store the resulting attributes in TO, which must be already be
   completely specified and contain only absolute attributes.
   Every specified attribute of FROM overrides the corresponding
   attribute of TO; relative attributes in FROM are merged with the
   absolute value in TO and replace it.  NAMED_MERGE_POINTS is used
   internally to detect loops in face inheritance/remapping; it should
   be 0 when called from other places.  If window W is non-NULL, use W
   to interpret face specifications. */
static void
merge_face_vectors (struct window *w,
		    struct frame *f, const Lisp_Object *from, Lisp_Object *to,
                    struct named_merge_point *named_merge_points)
{
  int i;
  Lisp_Object font = Qnil;

  /* If FROM inherits from some other faces, merge their attributes into
     TO before merging FROM's direct attributes.  Note that an :inherit
     attribute of `unspecified' is the same as one of nil; we never
     merge :inherit attributes, so nil is more correct, but lots of
     other code uses `unspecified' as a generic value for face attributes. */
  if (!UNSPECIFIEDP (from[LFACE_INHERIT_INDEX])
      && !NILP (from[LFACE_INHERIT_INDEX]))
    merge_face_ref (w, f, from[LFACE_INHERIT_INDEX],
		    to, false, named_merge_points,
		    0);

  if (FONT_SPEC_P (from[LFACE_FONT_INDEX]))
    {
      if (!UNSPECIFIEDP (to[LFACE_FONT_INDEX]))
	font = merge_font_spec (from[LFACE_FONT_INDEX], to[LFACE_FONT_INDEX]);
      else
	font = copy_font_spec (from[LFACE_FONT_INDEX]);
      to[LFACE_FONT_INDEX] = font;
    }

  for (i = 1; i < LFACE_VECTOR_SIZE; ++i)
    if (!UNSPECIFIEDP (from[i]))
      {
	if (i == LFACE_HEIGHT_INDEX && !FIXNUMP (from[i]))
	  {
	    to[i] = merge_face_heights (from[i], to[i], to[i]);
	    font_clear_prop (to, FONT_SIZE_INDEX);
	  }
	else if (i != LFACE_FONT_INDEX && ! EQ (to[i], from[i]))
	  {
	    to[i] = from[i];
	    if (i >= LFACE_FAMILY_INDEX && i <= LFACE_SLANT_INDEX)
	      font_clear_prop (to,
	                       (i == LFACE_FAMILY_INDEX ? FONT_FAMILY_INDEX
			        : i == LFACE_FOUNDRY_INDEX ? FONT_FOUNDRY_INDEX
				: i == LFACE_SWIDTH_INDEX ? FONT_WIDTH_INDEX
				: i == LFACE_HEIGHT_INDEX ? FONT_SIZE_INDEX
				: i == LFACE_WEIGHT_INDEX ? FONT_WEIGHT_INDEX
				: FONT_SLANT_INDEX));
	  }
      }

  /* If FROM specifies a font spec, make its contents take precedence
     over :family and other attributes.  This is needed for face
     remapping using :font to work.  */

  if (!NILP (font))
    {
      if (! NILP (AREF (font, FONT_FOUNDRY_INDEX)))
	to[LFACE_FOUNDRY_INDEX] = SYMBOL_NAME (AREF (font, FONT_FOUNDRY_INDEX));
      if (! NILP (AREF (font, FONT_FAMILY_INDEX)))
	to[LFACE_FAMILY_INDEX] = SYMBOL_NAME (AREF (font, FONT_FAMILY_INDEX));
      if (! NILP (AREF (font, FONT_WEIGHT_INDEX)))
	to[LFACE_WEIGHT_INDEX] = FONT_WEIGHT_FOR_FACE (font);
      if (! NILP (AREF (font, FONT_SLANT_INDEX)))
	to[LFACE_SLANT_INDEX] = FONT_SLANT_FOR_FACE (font);
      if (! NILP (AREF (font, FONT_WIDTH_INDEX)))
	to[LFACE_SWIDTH_INDEX] = FONT_WIDTH_FOR_FACE (font);
      ASET (font, FONT_SIZE_INDEX, Qnil);
    }

  /* TO is always an absolute face, which should inherit from nothing.
     We blindly copy the :inherit attribute above and fix it up here.  */
  to[LFACE_INHERIT_INDEX] = Qnil;
}

/* Chase the chain of face inheritance of frame F's face whose
   attributes are in ATTRS, for a non-'unspecified' value of face
   attribute whose index is ATTR_IDX, and return that value.  Window
   W, if non-NULL, is used to filter face specifications.  */
static Lisp_Object
face_inherited_attr (struct window *w, struct frame *f,
		     Lisp_Object attrs[LFACE_VECTOR_SIZE],
		     enum lface_attribute_index attr_idx,
		     struct named_merge_point *named_merge_points)
{
  Lisp_Object inherited_attrs[LFACE_VECTOR_SIZE];
  Lisp_Object attr_val = attrs[attr_idx];

  memcpy (inherited_attrs, attrs, LFACE_VECTOR_SIZE * sizeof (attrs[0]));
  while (UNSPECIFIEDP (attr_val)
	 && !NILP (inherited_attrs[LFACE_INHERIT_INDEX])
	 && !UNSPECIFIEDP (inherited_attrs[LFACE_INHERIT_INDEX]))
    {
      Lisp_Object parent_face = inherited_attrs[LFACE_INHERIT_INDEX];
      bool ok;

      if (CONSP (parent_face))
	{
	  Lisp_Object tail;
	  ok = false;
	  for (tail = parent_face; !NILP (tail); tail = XCDR (tail))
	    {
	      ok = get_lface_attributes (w, f, XCAR (tail), inherited_attrs,
					 false, named_merge_points);
	      if (!ok)
		break;
	      attr_val = face_inherited_attr (w, f, inherited_attrs, attr_idx,
					      named_merge_points);
	      if (!UNSPECIFIEDP (attr_val))
		break;
	    }
	  if (!ok)	/* bad face? */
	    break;
	}
      else
	{
	  ok = get_lface_attributes (w, f, parent_face, inherited_attrs,
				     false, named_merge_points);
	  if (!ok)
	    break;
	  attr_val = inherited_attrs[attr_idx];
	}
    }
  return attr_val;
}

/* Merge the named face FACE_NAME on frame F, into the vector of face
   attributes TO.  Use NAMED_MERGE_POINTS to detect loops in face
   inheritance.  Return true if FACE_NAME is a valid face name and
   merging succeeded.  Window W, if non-NULL, is used to filter face
   specifications. */

static bool
merge_named_face (struct window *w,
                  struct frame *f, Lisp_Object face_name, Lisp_Object *to,
                  struct named_merge_point *named_merge_points,
                  enum lface_attribute_index attr_filter)
{
  struct named_merge_point named_merge_point;

  if (push_named_merge_point (&named_merge_point,
                              face_name, NAMED_MERGE_POINT_NORMAL,
                              &named_merge_points))
    {
      Lisp_Object from[LFACE_VECTOR_SIZE], val;
      bool ok = get_lface_attributes (w, f, face_name, from, false,
                                      named_merge_points);
      if (ok && !EQ (face_name, Qdefault))
	{
	  struct face *deflt = FACE_FROM_ID (f, DEFAULT_FACE_ID);
	  int i;
	  for (i = 1; i < LFACE_VECTOR_SIZE; i++)
	    if (EQ (from[i], Qreset))
	      from[i] = deflt->lface[i];
	}

      if (ok && (attr_filter == 0	       /* No filter.  */
                 || (!NILP (from[attr_filter]) /* Filter, but specified.  */
		     && !UNSPECIFIEDP (from[attr_filter]))
		 /* Filter, unspecified, but inherited.  */
		 || (!NILP (from[LFACE_INHERIT_INDEX])
		     && !UNSPECIFIEDP (from[LFACE_INHERIT_INDEX])
		     && (val = face_inherited_attr (w, f, from, attr_filter,
						    named_merge_points),
			 (!NILP (val) && !UNSPECIFIEDP (val))))))
        merge_face_vectors (w, f, from, to, named_merge_points);

      return ok;
    }
  else
    return false;
}

/* Determine whether the face filter FILTER evaluated in window W
   matches.  W can be NULL if the window context is unknown.

   A face filter is either nil, which always matches, or a list
   (:window PARAMETER VALUE), which matches if the current window has
   a PARAMETER EQ to VALUE.

   This function returns true if the face filter matches, and false if
   it doesn't or if the function encountered an error.  If the filter
   is invalid, set *OK to false and, if ERR_MSGS is true, log an error
   message.  On success, *OK is untouched.  */
static bool
evaluate_face_filter (Lisp_Object filter, struct window *w,
                      bool *ok, bool err_msgs)
{
  Lisp_Object orig_filter = filter;

  /* Inner braces keep compiler happy about the goto skipping variable
     initialization.  */
  {
    if (NILP (filter))
      return true;

    if (face_filters_always_match)
      return true;

    if (!CONSP (filter))
      goto err;

    if (!EQ (XCAR (filter), QCwindow))
      goto err;
    filter = XCDR (filter);

    Lisp_Object parameter = XCAR (filter);
    filter = XCDR (filter);
    if (!CONSP (filter))
      goto err;

    Lisp_Object value = XCAR (filter);
    filter = XCDR (filter);
    if (!NILP (filter))
      goto err;

    bool match = false;
    if (w)
      {
        Lisp_Object found = assq_no_quit (parameter, w->window_parameters);
        if (!NILP (found) && EQ (XCDR (found), value))
          match = true;
      }

    return match;
  }

 err:
  if (err_msgs)
    add_to_log ("Invalid face filter %S", orig_filter);
  *ok = false;
  return false;
}

/* Determine whether FACE_REF is a "filter" face specification (case
   #4 in merge_face_ref).  If it is, evaluate the filter, and if the
   filter matches, return the filtered face spec.  If the filter does
   not match, return nil.  If FACE_REF is not a filtered face
   specification, return FACE_REF.

   On error, set *OK to false, having logged an error message if
   ERR_MSGS is true, and return nil.  Otherwise, *OK is not touched.

   W is either NULL or a window used to evaluate filters.  If W is
   NULL, no window-based face specification filter matches.
*/
static Lisp_Object
filter_face_ref (Lisp_Object face_ref,
                 struct window *w,
                 bool *ok,
                 bool err_msgs)
{
  Lisp_Object orig_face_ref = face_ref;
  if (!CONSP (face_ref))
    return face_ref;

  /* Inner braces keep compiler happy about the goto skipping variable
     initialization.  */
  {
    if (!EQ (XCAR (face_ref), QCfiltered))
      return face_ref;
    face_ref = XCDR (face_ref);

    if (!CONSP (face_ref))
      goto err;
    Lisp_Object filter = XCAR (face_ref);
    face_ref = XCDR (face_ref);

    if (!CONSP (face_ref))
      goto err;
    Lisp_Object filtered_face_ref = XCAR (face_ref);
    face_ref = XCDR (face_ref);

    if (!NILP (face_ref))
      goto err;

    return evaluate_face_filter (filter, w, ok, err_msgs)
      ? filtered_face_ref : Qnil;
  }

 err:
  if (err_msgs)
    add_to_log ("Invalid face ref %S", orig_face_ref);
  *ok = false;
  return Qnil;
}

/* Merge face attributes from the lisp `face reference' FACE_REF on
   frame F into the face attribute vector TO as appropriate for
   window W; W is used only for filtering face specs.  If ERR_MSGS
   is non-zero, problems with FACE_REF cause an error message to be
   shown.  Return true if no errors occurred (regardless of the value
   of ERR_MSGS).  Use NAMED_MERGE_POINTS to detect loops in face
   inheritance or list structure; it may be 0 for most callers.

   ATTR_FILTER is the index of a parameter that conditions the merging
   for named faces (case 1) to only the face_ref where
   lface[merge_face_ref] is non-nil.  To merge unconditionally set this
   value to 0.

   FACE_REF may be a single face specification or a list of such
   specifications.  Each face specification can be:

   1. A symbol or string naming a Lisp face.

   2. A property list of the form (KEYWORD VALUE ...) where each
   KEYWORD is a face attribute name, and value is an appropriate value
   for that attribute.

   3. Conses or the form (FOREGROUND-COLOR . COLOR) or
   (BACKGROUND-COLOR . COLOR) where COLOR is a color name.  This is
   for compatibility with 20.2.

   4. Conses of the form
   (:filtered (:window PARAMETER VALUE) FACE-SPECIFICATION),
   which applies FACE-SPECIFICATION only if the given face attributes
   are being evaluated in the context of a window with a parameter
   named PARAMETER being EQ VALUE.  In this case, W specifies the window
   for which the filtered face spec is to be evaluated.

   5. nil, which means to merge nothing.

   Face specifications earlier in lists take precedence over later
   specifications.  */

static bool
merge_face_ref (struct window *w,
                struct frame *f, Lisp_Object face_ref, Lisp_Object *to,
                bool err_msgs, struct named_merge_point *named_merge_points,
                enum lface_attribute_index attr_filter)
{
  bool ok = true;		/* Succeed without an error? */
  Lisp_Object filtered_face_ref;
  bool attr_filter_passed = false;

  filtered_face_ref = face_ref;
  do
    {
      face_ref = filtered_face_ref;
      filtered_face_ref = filter_face_ref (face_ref, w, &ok, err_msgs);
    }
  while (ok && !EQ (face_ref, filtered_face_ref));

  if (!ok)
    return false;

  if (NILP (face_ref))
    return true;

  if (CONSP (face_ref))
    {
      Lisp_Object first = XCAR (face_ref);

      if (EQ (first, Qforeground_color)
	  || EQ (first, Qbackground_color))
	{
	  /* One of (FOREGROUND-COLOR . COLOR) or (BACKGROUND-COLOR
	     . COLOR).  COLOR must be a string.  */
	  Lisp_Object color_name = XCDR (face_ref);
	  Lisp_Object color = first;

	  if (STRINGP (color_name))
	    {
	      if (EQ (color, Qforeground_color))
		to[LFACE_FOREGROUND_INDEX] = color_name;
	      else
		to[LFACE_BACKGROUND_INDEX] = color_name;
	    }
	  else
	    {
	      if (err_msgs)
		add_to_log ("Invalid face color %S", color_name);
	      ok = false;
	    }
	}
      else if (SYMBOLP (first)
	       && *SDATA (SYMBOL_NAME (first)) == ':')
	{
	  /* Assume this is the property list form.  */
	  if (attr_filter > 0)
	    {
	      eassert (attr_filter < LFACE_VECTOR_SIZE);
	      /* ATTR_FILTER positive means don't merge this face if
		 the corresponding attribute is nil, or not mentioned,
		 or if it's unspecified and the face doesn't inherit
		 from a face whose attribute is non-nil.  The code
		 below determines whether a face given as a property
		 list shall be merged.  */
	      Lisp_Object parent_face = Qnil;
	      bool attr_filter_seen = false;
	      Lisp_Object face_ref_tem = face_ref;
	      while (CONSP (face_ref_tem) && CONSP (XCDR (face_ref_tem)))
		{
		  Lisp_Object keyword = XCAR (face_ref_tem);
		  Lisp_Object value = XCAR (XCDR (face_ref_tem));

		  if (EQ (keyword, face_attr_sym[attr_filter])
		      || (attr_filter == LFACE_INVERSE_INDEX
			  && EQ (keyword, QCreverse_video)))
		    {
		      attr_filter_seen = true;
		      if (NILP (value))
			return true;
		    }
		  else if (EQ (keyword, QCinherit))
		    parent_face = value;
		  face_ref_tem = XCDR (XCDR (face_ref_tem));
		}
	      if (!attr_filter_seen)
		{
		  if (NILP (parent_face))
		    return true;

		  Lisp_Object scratch_attrs[LFACE_VECTOR_SIZE];
		  int i;

		  scratch_attrs[0] = Qface;
		  for (i = 1; i < LFACE_VECTOR_SIZE; i++)
		    scratch_attrs[i] = Qunspecified;
		  if (!merge_face_ref (w, f, parent_face, scratch_attrs,
				       err_msgs, named_merge_points, 0))
		    {
		      add_to_log ("Invalid face attribute %S %S",
				  QCinherit, parent_face);
		      return false;
		    }
		  if (NILP (scratch_attrs[attr_filter])
		      || UNSPECIFIEDP (scratch_attrs[attr_filter]))
		    return true;
		}
	      attr_filter_passed = true;
	    }
	  while (CONSP (face_ref) && CONSP (XCDR (face_ref)))
	    {
	      Lisp_Object keyword = XCAR (face_ref);
	      Lisp_Object value = XCAR (XCDR (face_ref));
	      bool err = false;

	      /* Specifying `unspecified' is a no-op.  */
	      if (EQ (value, Qunspecified))
		;
	      else if (EQ (keyword, QCfamily))
		{
		  if (STRINGP (value))
		    {
		      to[LFACE_FAMILY_INDEX] = value;
		      font_clear_prop (to, FONT_FAMILY_INDEX);
		    }
		  else
		    err = true;
		}
	      else if (EQ (keyword, QCfoundry))
		{
		  if (STRINGP (value))
		    {
		      to[LFACE_FOUNDRY_INDEX] = value;
		      font_clear_prop (to, FONT_FOUNDRY_INDEX);
		    }
		  else
		    err = true;
		}
	      else if (EQ (keyword, QCheight))
		{
		  Lisp_Object new_height =
		    merge_face_heights (value, to[LFACE_HEIGHT_INDEX], Qnil);

		  if (! NILP (new_height))
		    {
		      to[LFACE_HEIGHT_INDEX] = new_height;
		      font_clear_prop (to, FONT_SIZE_INDEX);
		    }
		  else
		    err = true;
		}
	      else if (EQ (keyword, QCweight))
		{
		  if (SYMBOLP (value) && FONT_WEIGHT_NAME_NUMERIC (value) >= 0)
		    {
		      to[LFACE_WEIGHT_INDEX] = value;
		      font_clear_prop (to, FONT_WEIGHT_INDEX);
		    }
		  else
		    err = true;
		}
	      else if (EQ (keyword, QCslant))
		{
		  if (SYMBOLP (value) && FONT_SLANT_NAME_NUMERIC (value) >= 0)
		    {
		      to[LFACE_SLANT_INDEX] = value;
		      font_clear_prop (to, FONT_SLANT_INDEX);
		    }
		  else
		    err = true;
		}
	      else if (EQ (keyword, QCunderline))
		{
		  if (EQ (value, Qt)
		      || NILP (value)
		      || STRINGP (value)
		      || CONSP (value))
		    to[LFACE_UNDERLINE_INDEX] = value;
		  else
		    err = true;
		}
	      else if (EQ (keyword, QCoverline))
		{
		  if (EQ (value, Qt)
		      || NILP (value)
		      || STRINGP (value))
		    to[LFACE_OVERLINE_INDEX] = value;
		  else
		    err = true;
		}
	      else if (EQ (keyword, QCstrike_through))
		{
		  if (EQ (value, Qt)
		      || NILP (value)
		      || STRINGP (value))
		    to[LFACE_STRIKE_THROUGH_INDEX] = value;
		  else
		    err = true;
		}
	      else if (EQ (keyword, QCbox))
		{
		  if (EQ (value, Qt))
		    value = make_fixnum (1);
		  if ((FIXNUMP (value) && XFIXNUM (value) != 0)
		      || STRINGP (value)
		      || CONSP (value)
		      || NILP (value))
		    to[LFACE_BOX_INDEX] = value;
		  else
		    err = true;
		}
	      else if (EQ (keyword, QCinverse_video)
		       || EQ (keyword, QCreverse_video))
		{
		  if (EQ (value, Qt) || NILP (value))
		    to[LFACE_INVERSE_INDEX] = value;
		  else
		    err = true;
		}
	      else if (EQ (keyword, QCforeground))
		{
		  if (STRINGP (value))
		    to[LFACE_FOREGROUND_INDEX] = value;
		  else
		    err = true;
		}
	      else if (EQ (keyword, QCdistant_foreground))
		{
		  if (STRINGP (value))
		    to[LFACE_DISTANT_FOREGROUND_INDEX] = value;
		  else
		    err = true;
		}
	      else if (EQ (keyword, QCbackground))
		{
		  if (STRINGP (value))
		    to[LFACE_BACKGROUND_INDEX] = value;
		  else
		    err = true;
		}
	      else if (EQ (keyword, QCstipple))
		{
#if defined (HAVE_WINDOW_SYSTEM)
		  if (NILP (value) || !NILP (Fbitmap_spec_p (value)))
		    to[LFACE_STIPPLE_INDEX] = value;
		  else
		    err = true;
#endif /* HAVE_WINDOW_SYSTEM */
		}
	      else if (EQ (keyword, QCwidth))
		{
		  if (SYMBOLP (value) && FONT_WIDTH_NAME_NUMERIC (value) >= 0)
		    {
		      to[LFACE_SWIDTH_INDEX] = value;
		      font_clear_prop (to, FONT_WIDTH_INDEX);
		    }
		  else
		    err = true;
		}
	      else if (EQ (keyword, QCfont))
		{
		  if (FONTP (value))
		    to[LFACE_FONT_INDEX] = value;
		  else
		    err = true;
		}
	      else if (EQ (keyword, QCinherit))
		{
		  /* This is not really very useful; it's just like a
		     normal face reference.  */
		  if (attr_filter_passed)
		    {
		      /* We already know that this face was tested
			 against attr_filter and was found applicable,
			 so don't pass attr_filter to merge_face_ref.
			 This is for when a face is specified like
			 (:inherit FACE :extend t), but the parent
			 FACE itself doesn't specify :extend.  */
		      if (! merge_face_ref (w, f, value, to,
					    err_msgs, named_merge_points, 0))
			err = true;
		    }
		  else if (! merge_face_ref (w, f, value, to,
					     err_msgs, named_merge_points,
					     attr_filter))
		    err = true;
		}
	      else if (EQ (keyword, QCextend))
		{
		  if (EQ (value, Qt) || NILP (value))
		    to[LFACE_EXTEND_INDEX] = value;
		  else
		    err = true;
		}
	      else
		err = true;

	      if (err)
		{
		  add_to_log ("Invalid face attribute %S %S", keyword, value);
		  ok = false;
		}

	      face_ref = XCDR (XCDR (face_ref));
	    }
	}
      else
	{
	  /* This is a list of face refs.  Those at the beginning of the
	     list take precedence over what follows, so we have to merge
	     from the end backwards.  */
	  Lisp_Object next = XCDR (face_ref);

	  if (! NILP (next))
	    ok = merge_face_ref (w, f, next, to, err_msgs,
	                         named_merge_points, attr_filter);

	  if (! merge_face_ref (w, f, first, to, err_msgs,
	                        named_merge_points, attr_filter))
	    ok = false;
	}
    }
  else
    {
      /* FACE_REF ought to be a face name.  */
      ok = merge_named_face (w, f, face_ref, to, named_merge_points,
                             attr_filter);
      if (!ok && err_msgs)
	add_to_log ("Invalid face reference: %s", face_ref);
    }

  return ok;
}


DEFUN ("internal-make-lisp-face", Finternal_make_lisp_face,
       Sinternal_make_lisp_face, 1, 2, 0,
       doc: /* Make FACE, a symbol, a Lisp face with all attributes nil.
If FACE was not known as a face before, create a new one.
If optional argument FRAME is specified, make a frame-local face
for that frame.  Otherwise operate on the global face definition.
Value is a vector of face attributes.  */)
  (Lisp_Object face, Lisp_Object frame)
{
  Lisp_Object global_lface, lface;
  struct frame *f;
  int i;

  CHECK_SYMBOL (face);
  global_lface = lface_from_face_name (NULL, face, false);

  if (!NILP (frame))
    {
      CHECK_LIVE_FRAME (frame);
      f = XFRAME (frame);
      lface = lface_from_face_name (f, face, false);
    }
  else
    f = NULL, lface = Qnil;

  /* Add a global definition if there is none.  */
  if (NILP (global_lface))
    {
      /* Assign the new Lisp face a unique ID.  The mapping from Lisp
	 face id to Lisp face is given by the vector lface_id_to_name.
	 The mapping from Lisp face to Lisp face id is given by the
	 property `face' of the Lisp face name.  */
      if (next_lface_id == lface_id_to_name_size)
	lface_id_to_name =
	  xpalloc (lface_id_to_name, &lface_id_to_name_size, 1, MAX_FACE_ID,
		   sizeof *lface_id_to_name);

      Lisp_Object face_id = make_fixnum (next_lface_id);
      lface_id_to_name[next_lface_id] = face;
      Fput (face, Qface, face_id);
      ++next_lface_id;

      global_lface = make_vector (LFACE_VECTOR_SIZE, Qunspecified);
      ASET (global_lface, 0, Qface);
      Fputhash (face, Fcons (face_id, global_lface), Vface_new_frame_defaults);
    }
  else if (f == NULL)
    for (i = 1; i < LFACE_VECTOR_SIZE; ++i)
      ASET (global_lface, i, Qunspecified);

  /* Add a frame-local definition.  */
  if (f)
    {
      if (NILP (lface))
	{
	  lface = make_vector (LFACE_VECTOR_SIZE, Qunspecified);
	  ASET (lface, 0, Qface);
          Fputhash (face, lface, f->face_hash_table);
	}
      else
	for (i = 1; i < LFACE_VECTOR_SIZE; ++i)
	  ASET (lface, i, Qunspecified);
    }
  else
    lface = global_lface;

  /* Changing a named face means that all realized faces depending on
     that face are invalid.  Since we cannot tell which realized faces
     depend on the face, make sure they are all removed.  This is done
     by setting face_change.  The next call to init_iterator will then
     free realized faces.  */
  if (NILP (Fget (face, Qface_no_inherit)))
    {
      if (f)
	{
	  f->face_change = true;
	  fset_redisplay (f);
	}
      else
	{
	  face_change = true;
	  windows_or_buffers_changed = 54;
	}
    }

  eassert (LFACEP (lface));
  check_lface (lface);
  return lface;
}


DEFUN ("internal-lisp-face-p", Finternal_lisp_face_p,
       Sinternal_lisp_face_p, 1, 2, 0,
       doc: /* Return non-nil if FACE names a face.
FACE should be a symbol or string.
If optional second argument FRAME is non-nil, check for the
existence of a frame-local face with name FACE on that frame.
Otherwise check for the existence of a global face.  */)
  (Lisp_Object face, Lisp_Object frame)
{
  Lisp_Object lface;

  face = resolve_face_name (face, true);

  if (!NILP (frame))
    {
      CHECK_LIVE_FRAME (frame);
      lface = lface_from_face_name (XFRAME (frame), face, false);
    }
  else
    lface = lface_from_face_name (NULL, face, false);

  return lface;
}


DEFUN ("internal-copy-lisp-face", Finternal_copy_lisp_face,
       Sinternal_copy_lisp_face, 4, 4, 0,
       doc: /* Copy face FROM to TO.
If FRAME is t, copy the global face definition of FROM.
Otherwise, copy the frame-local definition of FROM on FRAME.
If NEW-FRAME is a frame, copy that data into the frame-local
definition of TO on NEW-FRAME.  If NEW-FRAME is nil,
FRAME controls where the data is copied to.

The value is TO.  */)
  (Lisp_Object from, Lisp_Object to, Lisp_Object frame, Lisp_Object new_frame)
{
  Lisp_Object lface, copy;
  struct frame *f;

  CHECK_SYMBOL (from);
  CHECK_SYMBOL (to);

  if (EQ (frame, Qt))
    {
      /* Copy global definition of FROM.  We don't make copies of
	 strings etc. because 20.2 didn't do it either.  */
      lface = lface_from_face_name (NULL, from, true);
      copy = Finternal_make_lisp_face (to, Qnil);
      f = NULL;
    }
  else
    {
      /* Copy frame-local definition of FROM.  */
      if (NILP (new_frame))
	new_frame = frame;
      CHECK_LIVE_FRAME (frame);
      CHECK_LIVE_FRAME (new_frame);
      lface = lface_from_face_name (XFRAME (frame), from, true);
      copy = Finternal_make_lisp_face (to, new_frame);
      f = XFRAME (new_frame);
    }

  vcopy (copy, 0, xvector_contents (lface), LFACE_VECTOR_SIZE);

  /* Changing a named face means that all realized faces depending on
     that face are invalid.  Since we cannot tell which realized faces
     depend on the face, make sure they are all removed.  This is done
     by setting face_change.  The next call to init_iterator will then
     free realized faces.  */
  if (NILP (Fget (to, Qface_no_inherit)))
    {
      if (f)
	{
	  f->face_change = true;
	  fset_redisplay (f);
	}
      else
	{
	  face_change = true;
	  windows_or_buffers_changed = 55;
	}
    }

  return to;
}


#define HANDLE_INVALID_NIL_VALUE(A,F)					\
  if (NILP (value))							\
    {									\
      add_to_log ("Warning: setting attribute `%s' of face `%s': nil "	\
		  "value is invalid, use `unspecified' instead.", A, F); \
      /* Compatibility with 20.x.  */					\
      value = Qunspecified;						\
    }

DEFUN ("internal-set-lisp-face-attribute", Finternal_set_lisp_face_attribute,
       Sinternal_set_lisp_face_attribute, 3, 4, 0,
       doc: /* Set attribute ATTR of FACE to VALUE.
FRAME being a frame means change the face on that frame.
FRAME nil means change the face of the selected frame.
FRAME t means change the default for new frames.
FRAME 0 means change the face on all frames, and change the default
  for new frames.  */)
  (Lisp_Object face, Lisp_Object attr, Lisp_Object value, Lisp_Object frame)
{
  Lisp_Object lface;
  Lisp_Object old_value = Qnil;
  /* Set one of enum font_property_index (> 0) if ATTR is one of
     font-related attributes other than QCfont and QCfontset.  */
  enum font_property_index prop_index = 0;
  struct frame *f;

  CHECK_SYMBOL (face);
  CHECK_SYMBOL (attr);

  face = resolve_face_name (face, true);

  /* If FRAME is 0, change face on all frames, and change the
     default for new frames.  */
  if (FIXNUMP (frame) && XFIXNUM (frame) == 0)
    {
      Lisp_Object tail;
      Finternal_set_lisp_face_attribute (face, attr, value, Qt);
      FOR_EACH_FRAME (tail, frame)
	Finternal_set_lisp_face_attribute (face, attr, value, frame);
      return face;
    }

  /* Set lface to the Lisp attribute vector of FACE.  */
  if (EQ (frame, Qt))
    {
      f = NULL;
      lface = lface_from_face_name (NULL, face, true);

      /* When updating face--new-frame-defaults, we put :ignore-defface
	 where the caller wants `unspecified'.  This forces the frame
	 defaults to ignore the defface value.  Otherwise, the defface
	 will take effect, which is generally not what is intended.
	 The value of that attribute will be inherited from some other
	 face during face merging.  See internal_merge_in_global_face. */
      if (UNSPECIFIEDP (value))
	value = QCignore_defface;
    }
  else
    {
      if (NILP (frame))
	frame = selected_frame;

      CHECK_LIVE_FRAME (frame);
      f = XFRAME (frame);

      lface = lface_from_face_name (f, face, false);

      /* If a frame-local face doesn't exist yet, create one.  */
      if (NILP (lface))
	lface = Finternal_make_lisp_face (face, frame);
    }

  if (EQ (attr, QCfamily))
    {
      if (!UNSPECIFIEDP (value)
	  && !IGNORE_DEFFACE_P (value)
	  && !RESET_P (value))
	{
	  CHECK_STRING (value);
	  if (SCHARS (value) == 0)
	    signal_error ("Invalid face family", value);
	}
      old_value = LFACE_FAMILY (lface);
      ASET (lface, LFACE_FAMILY_INDEX, value);
      prop_index = FONT_FAMILY_INDEX;
    }
  else if (EQ (attr, QCfoundry))
    {
      if (!UNSPECIFIEDP (value)
	  && !IGNORE_DEFFACE_P (value)
	  && !RESET_P (value))
	{
	  CHECK_STRING (value);
	  if (SCHARS (value) == 0)
	    signal_error ("Invalid face foundry", value);
	}
      old_value = LFACE_FOUNDRY (lface);
      ASET (lface, LFACE_FOUNDRY_INDEX, value);
      prop_index = FONT_FOUNDRY_INDEX;
    }
  else if (EQ (attr, QCheight))
    {
      if (!UNSPECIFIEDP (value)
	  && !IGNORE_DEFFACE_P (value)
	  && !RESET_P (value))
	{
	  if (EQ (face, Qdefault))
	    {
	      /* The default face must have an absolute size.  */
	      if (!FIXNUMP (value) || XFIXNUM (value) <= 0)
		signal_error ("Default face height not absolute and positive",
			      value);
	    }
	  else
	    {
	      /* For non-default faces, do a test merge with a random
		 height to see if VALUE's ok. */
	      Lisp_Object test = merge_face_heights (value,
						     make_fixnum (10),
						     Qnil);
	      if (!FIXNUMP (test) || XFIXNUM (test) <= 0)
		signal_error ("Face height does not produce a positive integer",
			      value);
	    }
	}

      old_value = LFACE_HEIGHT (lface);
      ASET (lface, LFACE_HEIGHT_INDEX, value);
      prop_index = FONT_SIZE_INDEX;
    }
  else if (EQ (attr, QCweight))
    {
      if (!UNSPECIFIEDP (value)
	  && !IGNORE_DEFFACE_P (value)
	  && !RESET_P (value))
	{
	  CHECK_SYMBOL (value);
	  if (FONT_WEIGHT_NAME_NUMERIC (value) < 0)
	    signal_error ("Invalid face weight", value);
	}
      old_value = LFACE_WEIGHT (lface);
      ASET (lface, LFACE_WEIGHT_INDEX, value);
      prop_index = FONT_WEIGHT_INDEX;
    }
  else if (EQ (attr, QCslant))
    {
      if (!UNSPECIFIEDP (value)
	  && !IGNORE_DEFFACE_P (value)
	  && !RESET_P (value))
	{
	  CHECK_SYMBOL (value);
	  if (FONT_SLANT_NAME_NUMERIC (value) < 0)
	    signal_error ("Invalid face slant", value);
	}
      old_value = LFACE_SLANT (lface);
      ASET (lface, LFACE_SLANT_INDEX, value);
      prop_index = FONT_SLANT_INDEX;
    }
  else if (EQ (attr, QCunderline))
    {
      bool valid_p = false;

      if (UNSPECIFIEDP (value) || IGNORE_DEFFACE_P (value) || RESET_P (value))
	valid_p = true;
      else if (NILP (value) || EQ (value, Qt))
        valid_p = true;
      else if (STRINGP (value) && SCHARS (value) > 0)
        valid_p = true;
      else if (CONSP (value))
        {
          Lisp_Object key, val, list;

          list = value;
          /* FIXME?  This errs on the side of acceptance.  Eg it accepts:
               (defface foo '((t :underline 'foo) "doc")
             Maybe this is intentional, maybe it isn't.
             Non-nil symbols other than t are not documented as being valid.
             Eg compare with inverse-video, which explicitly rejects them.
          */
          valid_p = true;

          while (!NILP (CAR_SAFE (list)))
            {
              key = CAR_SAFE (list);
              list = CDR_SAFE (list);
              val = CAR_SAFE (list);
              list = CDR_SAFE (list);

              if (NILP (key) || (NILP (val)
				 && !EQ (key, QCposition)))
                {
                  valid_p = false;
                  break;
                }

              else if (EQ (key, QCcolor)
                       && !(EQ (val, Qforeground_color)
                            || (STRINGP (val) && SCHARS (val) > 0)))
                {
                  valid_p = false;
                  break;
                }

              else if (EQ (key, QCstyle)
                       && !(EQ (val, Qline) || EQ (val, Qwave)))
                {
                  valid_p = false;
                  break;
                }
            }
        }

      if (!valid_p)
        signal_error ("Invalid face underline", value);

      old_value = LFACE_UNDERLINE (lface);
      ASET (lface, LFACE_UNDERLINE_INDEX, value);
    }
  else if (EQ (attr, QCoverline))
    {
      if (!UNSPECIFIEDP (value)
	  && !IGNORE_DEFFACE_P (value)
	  && !RESET_P (value))
	if ((SYMBOLP (value)
	     && !EQ (value, Qt)
	     && !NILP (value))
	    /* Overline color.  */
	    || (STRINGP (value)
		&& SCHARS (value) == 0))
	  signal_error ("Invalid face overline", value);

      old_value = LFACE_OVERLINE (lface);
      ASET (lface, LFACE_OVERLINE_INDEX, value);
    }
  else if (EQ (attr, QCstrike_through))
    {
      if (!UNSPECIFIEDP (value)
	  && !IGNORE_DEFFACE_P (value)
	  && !RESET_P (value))
	if ((SYMBOLP (value)
	     && !EQ (value, Qt)
	     && !NILP (value))
	    /* Strike-through color.  */
	    || (STRINGP (value)
		&& SCHARS (value) == 0))
	  signal_error ("Invalid face strike-through", value);

      old_value = LFACE_STRIKE_THROUGH (lface);
      ASET (lface, LFACE_STRIKE_THROUGH_INDEX, value);
    }
  else if (EQ (attr, QCbox))
    {
      bool valid_p;

      /* Allow t meaning a simple box of width 1 in foreground color
	 of the face.  */
      if (EQ (value, Qt))
	value = make_fixnum (1);

      if (UNSPECIFIEDP (value) || IGNORE_DEFFACE_P (value) || RESET_P (value))
	valid_p = true;
      else if (NILP (value))
	valid_p = true;
      else if (FIXNUMP (value))
	valid_p = XFIXNUM (value) != 0;
      else if (STRINGP (value))
	valid_p = SCHARS (value) > 0;
      else if (CONSP (value) && FIXNUMP (XCAR (value)) && FIXNUMP (XCDR (value)))
	valid_p = true;
      else if (CONSP (value))
	{
	  Lisp_Object tem;

	  tem = value;
	  while (CONSP (tem))
	    {
	      Lisp_Object k, v;

	      k = XCAR (tem);
	      tem = XCDR (tem);
	      if (!CONSP (tem))
		break;
	      v = XCAR (tem);
	      tem = XCDR (tem);

	      if (EQ (k, QCline_width))
		{
		  if ((!CONSP(v) || !FIXNUMP (XCAR (v)) || XFIXNUM (XCAR (v)) == 0
		                 || !FIXNUMP (XCDR (v)) || XFIXNUM (XCDR (v)) == 0)
		      && (!FIXNUMP (v) || XFIXNUM (v) == 0))
		    break;
		}
	      else if (EQ (k, QCcolor))
		{
		  if (!NILP (v) && (!STRINGP (v) || SCHARS (v) == 0))
		    break;
		}
	      else if (EQ (k, QCstyle))
		{
		  if (!EQ (v, Qpressed_button) && !EQ (v, Qreleased_button)
		      && !EQ(v, Qflat_button))
		    break;
		}
	      else
		break;
	    }

	  valid_p = NILP (tem);
	}
      else
	valid_p = false;

      if (!valid_p)
	signal_error ("Invalid face box", value);

      old_value = LFACE_BOX (lface);
      ASET (lface, LFACE_BOX_INDEX, value);
    }
  else if (EQ (attr, QCinverse_video)
	   || EQ (attr, QCreverse_video))
    {
      if (!UNSPECIFIEDP (value)
	  && !IGNORE_DEFFACE_P (value)
	  && !RESET_P (value))
	{
	  CHECK_SYMBOL (value);
	  if (!EQ (value, Qt) && !NILP (value))
	    signal_error ("Invalid inverse-video face attribute value", value);
	}
      old_value = LFACE_INVERSE (lface);
      ASET (lface, LFACE_INVERSE_INDEX, value);
    }
  else if (EQ (attr, QCextend))
    {
      if (!UNSPECIFIEDP (value)
	  && !IGNORE_DEFFACE_P (value)
	  && !RESET_P (value))
	{
	  CHECK_SYMBOL (value);
	  if (!EQ (value, Qt) && !NILP (value))
	    signal_error ("Invalid extend face attribute value", value);
	}
      old_value = LFACE_EXTEND (lface);
      ASET (lface, LFACE_EXTEND_INDEX, value);
    }
  else if (EQ (attr, QCforeground))
    {
      HANDLE_INVALID_NIL_VALUE (QCforeground, face);
      if (!UNSPECIFIEDP (value)
	  && !IGNORE_DEFFACE_P (value)
	  && !RESET_P (value))
	{
	  /* Don't check for valid color names here because it depends
	     on the frame (display) whether the color will be valid
	     when the face is realized.  */
	  CHECK_STRING (value);
	  if (SCHARS (value) == 0)
	    signal_error ("Empty foreground color value", value);
	}
      old_value = LFACE_FOREGROUND (lface);
      ASET (lface, LFACE_FOREGROUND_INDEX, value);
    }
  else if (EQ (attr, QCdistant_foreground))
    {
      HANDLE_INVALID_NIL_VALUE (QCdistant_foreground, face);
      if (!UNSPECIFIEDP (value)
	  && !IGNORE_DEFFACE_P (value)
	  && !RESET_P (value))
	{
	  /* Don't check for valid color names here because it depends
	     on the frame (display) whether the color will be valid
	     when the face is realized.  */
	  CHECK_STRING (value);
	  if (SCHARS (value) == 0)
	    signal_error ("Empty distant-foreground color value", value);
	}
      old_value = LFACE_DISTANT_FOREGROUND (lface);
      ASET (lface, LFACE_DISTANT_FOREGROUND_INDEX, value);
    }
  else if (EQ (attr, QCbackground))
    {
      HANDLE_INVALID_NIL_VALUE (QCbackground, face);
      if (!UNSPECIFIEDP (value)
	  && !IGNORE_DEFFACE_P (value)
	  && !RESET_P (value))
	{
	  /* Don't check for valid color names here because it depends
	     on the frame (display) whether the color will be valid
	     when the face is realized.  */
	  CHECK_STRING (value);
	  if (SCHARS (value) == 0)
	    signal_error ("Empty background color value", value);
	}
      old_value = LFACE_BACKGROUND (lface);
      ASET (lface, LFACE_BACKGROUND_INDEX, value);
    }
  else if (EQ (attr, QCstipple))
    {
#if defined (HAVE_WINDOW_SYSTEM)
      if (!UNSPECIFIEDP (value)
	  && !IGNORE_DEFFACE_P (value)
	  && !RESET_P (value)
	  && !NILP (value)
	  && NILP (Fbitmap_spec_p (value)))
	signal_error ("Invalid stipple attribute", value);
      old_value = LFACE_STIPPLE (lface);
      ASET (lface, LFACE_STIPPLE_INDEX, value);
#endif /* HAVE_WINDOW_SYSTEM */
    }
  else if (EQ (attr, QCwidth))
    {
      if (!UNSPECIFIEDP (value)
	  && !IGNORE_DEFFACE_P (value)
	  && !RESET_P (value))
	{
	  CHECK_SYMBOL (value);
	  if (FONT_WIDTH_NAME_NUMERIC (value) < 0)
	    signal_error ("Invalid face width", value);
	}
      old_value = LFACE_SWIDTH (lface);
      ASET (lface, LFACE_SWIDTH_INDEX, value);
      prop_index = FONT_WIDTH_INDEX;
    }
  else if (EQ (attr, QCfont))
    {
#ifdef HAVE_WINDOW_SYSTEM
      if (EQ (frame, Qt) || FRAME_WINDOW_P (f))
	{
	  if (!UNSPECIFIEDP (value)
	      && !IGNORE_DEFFACE_P (value)
	      && !RESET_P (value))
	    {
	      struct frame *f1;

	      old_value = LFACE_FONT (lface);
	      if (! FONTP (value))
		{
		  if (STRINGP (value))
		    {
		      Lisp_Object name = value;
		      int fontset = fs_query_fontset (name, 0);

		      if (fontset >= 0)
			name = fontset_ascii (fontset);
		      value = font_spec_from_name (name);
		      if (!FONTP (value))
			signal_error ("Invalid font name", name);
		    }
		  else
		    signal_error ("Invalid font or font-spec", value);
		}
	      if (EQ (frame, Qt))
		f1 = XFRAME (selected_frame);
	      else
		f1 = XFRAME (frame);

              /* FIXME:
                 If frame is t, and selected frame is a tty frame, the font
                 can't be realized.  An improvement would be to loop over frames
                 for a non-tty frame and use that.  See discussion in Bug#18573.
                 For a daemon, frame may be an initial frame (Bug#18869).  */
              if (FRAME_WINDOW_P (f1))
                {
                  if (! FONT_OBJECT_P (value))
                    {
                      Lisp_Object *attrs = XVECTOR (lface)->contents;
                      Lisp_Object font_object;

                      font_object = font_load_for_lface (f1, attrs, value);
                      if (NILP (font_object))
                        signal_error ("Font not available", value);
                      value = font_object;
                    }
                  set_lface_from_font (f1, lface, value, true);
		  f1->face_change = 1;
                }
	    }
	  else
	    ASET (lface, LFACE_FONT_INDEX, value);
	}
#endif /* HAVE_WINDOW_SYSTEM */
    }
  else if (EQ (attr, QCfontset))
    {
#ifdef HAVE_WINDOW_SYSTEM
      if (EQ (frame, Qt) || FRAME_WINDOW_P (f))
	{
	  Lisp_Object tmp = value;

	  old_value = LFACE_FONTSET (lface);
	  if (!RESET_P (value))
	    {
	      tmp = Fquery_fontset (value, Qnil);
	      if (NILP (tmp))
		signal_error ("Invalid fontset name", value);
	    }
	  ASET (lface, LFACE_FONTSET_INDEX, value = tmp);
	}
#endif /* HAVE_WINDOW_SYSTEM */
    }
  else if (EQ (attr, QCinherit))
    {
      Lisp_Object tail;
      if (SYMBOLP (value))
	tail = Qnil;
      else
	for (tail = value; CONSP (tail); tail = XCDR (tail))
	  if (!SYMBOLP (XCAR (tail)))
	    break;
      if (NILP (tail))
	ASET (lface, LFACE_INHERIT_INDEX, value);
      else
	signal_error ("Invalid face inheritance", value);
    }
  else if (EQ (attr, QCbold))
    {
      old_value = LFACE_WEIGHT (lface);
      if (RESET_P (value))
	ASET (lface, LFACE_WEIGHT_INDEX, value);
      else
	ASET (lface, LFACE_WEIGHT_INDEX, NILP (value) ? Qnormal : Qbold);
      prop_index = FONT_WEIGHT_INDEX;
    }
  else if (EQ (attr, QCitalic))
    {
      attr = QCslant;
      old_value = LFACE_SLANT (lface);
      if (RESET_P (value))
	ASET (lface, LFACE_SLANT_INDEX, value);
      else
	ASET (lface, LFACE_SLANT_INDEX, NILP (value) ? Qnormal : Qitalic);
      prop_index = FONT_SLANT_INDEX;
    }
  else
    signal_error ("Invalid face attribute name", attr);

  if (prop_index)
    {
      /* If a font-related attribute other than QCfont and QCfontset
	 is specified, and if the original QCfont attribute has a font
	 (font-spec or font-object), set the corresponding property in
	 the font to nil so that the font selector doesn't think that
	 the attribute is mandatory.  Also, clear the average
	 width.  */
      font_clear_prop (XVECTOR (lface)->contents, prop_index);
    }

  /* Changing a named face means that all realized faces depending on
     that face are invalid.  Since we cannot tell which realized faces
     depend on the face, make sure they are all removed.  This is done
     by setting face_change.  The next call to init_iterator will then
     free realized faces.  */
  if (!EQ (frame, Qt)
      && NILP (Fget (face, Qface_no_inherit))
      && NILP (Fequal (old_value, value)))
    {
      f->face_change = true;
      fset_redisplay (f);
    }

  if (!UNSPECIFIEDP (value) && !IGNORE_DEFFACE_P (value)
      && NILP (Fequal (old_value, value)))
    {
      Lisp_Object param;

      param = Qnil;

      if (EQ (face, Qdefault))
	{
#ifdef HAVE_WINDOW_SYSTEM
	  /* Changed font-related attributes of the `default' face are
	     reflected in changed `font' frame parameters.  */
	  if (FRAMEP (frame)
	      && (prop_index || EQ (attr, QCfont))
	      && lface_fully_specified_p (XVECTOR (lface)->contents))
	    set_font_frame_param (frame, lface);
	  else
#endif /* HAVE_WINDOW_SYSTEM */

	  if (EQ (attr, QCforeground))
	    param = Qforeground_color;
	  else if (EQ (attr, QCbackground))
	    param = Qbackground_color;
	}
#ifdef HAVE_WINDOW_SYSTEM
#ifndef HAVE_NTGUI
      else if (EQ (face, Qscroll_bar))
	{
	  /* Changing the colors of `scroll-bar' sets frame parameters
	     `scroll-bar-foreground' and `scroll-bar-background'. */
	  if (EQ (attr, QCforeground))
	    param = Qscroll_bar_foreground;
	  else if (EQ (attr, QCbackground))
	    param = Qscroll_bar_background;
	}
#endif /* not HAVE_NTGUI */
      else if (EQ (face, Qborder))
	{
	  /* Changing background color of `border' sets frame parameter
	     `border-color'.  */
	  if (EQ (attr, QCbackground))
	    param = Qborder_color;
	}
      else if (EQ (face, Qcursor))
	{
	  /* Changing background color of `cursor' sets frame parameter
	     `cursor-color'.  */
	  if (EQ (attr, QCbackground))
	    param = Qcursor_color;
	}
      else if (EQ (face, Qmouse))
	{
	  /* Changing background color of `mouse' sets frame parameter
	     `mouse-color'.  */
	  if (EQ (attr, QCbackground))
	    param = Qmouse_color;
	}
#endif /* HAVE_WINDOW_SYSTEM */
      else if (EQ (face, Qmenu))
	{
	  /* Indicate that we have to update the menu bar when realizing
	     faces on FRAME.  FRAME t change the default for new frames.
	     We do this by setting the flag in new face caches.  */
	  if (FRAMEP (frame))
	    {
	      struct frame *f = XFRAME (frame);
	      if (FRAME_FACE_CACHE (f) == NULL)
		FRAME_FACE_CACHE (f) = make_face_cache (f);
	      FRAME_FACE_CACHE (f)->menu_face_changed_p = true;
	    }
	  else
	    menu_face_changed_default = true;
	}

      if (!NILP (param))
	{
	  if (EQ (frame, Qt))
	    /* Update `default-frame-alist', which is used for new frames.  */
	    {
	      store_in_alist (&Vdefault_frame_alist, param, value);
	    }
	  else
	    /* Update the current frame's parameters.  */
	    {
	      AUTO_FRAME_ARG (arg, param, value);
	      Fmodify_frame_parameters (frame, arg);
	    }
	}
    }

  return face;
}


/* Update the corresponding face when frame parameter PARAM on frame F
   has been assigned the value NEW_VALUE.  */

void
update_face_from_frame_parameter (struct frame *f, Lisp_Object param,
				  Lisp_Object new_value)
{
  Lisp_Object face = Qnil;
  Lisp_Object lface;

  /* If there are no faces yet, give up.  This is the case when called
     from Fx_create_frame, and we do the necessary things later in
     face-set-after-frame-defaults.  */
  if (XFIXNAT (Fhash_table_count (f->face_hash_table)) == 0)
    return;

  if (EQ (param, Qforeground_color))
    {
      face = Qdefault;
      lface = lface_from_face_name (f, face, true);
      ASET (lface, LFACE_FOREGROUND_INDEX,
	    (STRINGP (new_value) ? new_value : Qunspecified));
      realize_basic_faces (f);
    }
  else if (EQ (param, Qbackground_color))
    {
      Lisp_Object frame;

      /* Changing the background color might change the background
	 mode, so that we have to load new defface specs.
	 Call frame-set-background-mode to do that.  */
      XSETFRAME (frame, f);
      call1 (Qframe_set_background_mode, frame);

      face = Qdefault;
      lface = lface_from_face_name (f, face, true);
      ASET (lface, LFACE_BACKGROUND_INDEX,
	    (STRINGP (new_value) ? new_value : Qunspecified));
      realize_basic_faces (f);
    }
#ifdef HAVE_WINDOW_SYSTEM
  else if (EQ (param, Qborder_color))
    {
      face = Qborder;
      lface = lface_from_face_name (f, face, true);
      ASET (lface, LFACE_BACKGROUND_INDEX,
	    (STRINGP (new_value) ? new_value : Qunspecified));
    }
  else if (EQ (param, Qcursor_color))
    {
      face = Qcursor;
      lface = lface_from_face_name (f, face, true);
      ASET (lface, LFACE_BACKGROUND_INDEX,
	    (STRINGP (new_value) ? new_value : Qunspecified));
    }
  else if (EQ (param, Qmouse_color))
    {
      face = Qmouse;
      lface = lface_from_face_name (f, face, true);
      ASET (lface, LFACE_BACKGROUND_INDEX,
	    (STRINGP (new_value) ? new_value : Qunspecified));
    }
#endif

  /* Changing a named face means that all realized faces depending on
     that face are invalid.  Since we cannot tell which realized faces
     depend on the face, make sure they are all removed.  This is done
     by setting face_change.  The next call to init_iterator will then
     free realized faces.  */
  if (!NILP (face)
      && NILP (Fget (face, Qface_no_inherit)))
    {
      f->face_change = true;
      fset_redisplay (f);
    }
}


#ifdef HAVE_WINDOW_SYSTEM

/* Set the `font' frame parameter of FRAME determined from the
   font-object set in `default' face attributes LFACE.  */

static void
set_font_frame_param (Lisp_Object frame, Lisp_Object lface)
{
  struct frame *f = XFRAME (frame);
  Lisp_Object font;

  if (FRAME_WINDOW_P (f)
      /* Don't do anything if the font is `unspecified'.  This can
	 happen during frame creation.  */
      && (font = LFACE_FONT (lface),
	  ! UNSPECIFIEDP (font)))
    {
      if (FONT_SPEC_P (font))
	{
	  font = font_load_for_lface (f, XVECTOR (lface)->contents, font);
	  if (NILP (font))
	    return;
	  ASET (lface, LFACE_FONT_INDEX, font);
	}
      f->default_face_done_p = false;
      AUTO_LIST2 (arg, AUTO_CONS_EXPR (Qfont, font),
		  /* Clear the `font-parameter' frame property, as the
		     font is now being specified by a face, not a
		     frame property.  */
		  AUTO_CONS_EXPR (Qfont_parameter, Qnil));
      gui_set_frame_parameters_1 (f, arg, true);
    }
}

DEFUN ("internal-face-x-get-resource", Finternal_face_x_get_resource,
       Sinternal_face_x_get_resource, 2, 3, 0,
       doc: /* Get the value of X resource RESOURCE, class CLASS.
Returned value is for the display of frame FRAME.  If FRAME is not
specified or nil, use selected frame.  This function exists because
ordinary `x-get-resource' doesn't take a frame argument.  */)
  (Lisp_Object resource, Lisp_Object class, Lisp_Object frame)
{
  Lisp_Object value = Qnil;
  struct frame *f;

  CHECK_STRING (resource);
  CHECK_STRING (class);
  f = decode_live_frame (frame);
  block_input ();
  value = gui_display_get_resource (FRAME_DISPLAY_INFO (f),
                                    resource, class, Qnil, Qnil);
  unblock_input ();
  return value;
}


/* Return resource string VALUE as a boolean value, i.e. nil, or t.
   If VALUE is "on" or "true", return t.  If VALUE is "off" or
   "false", return nil.  Otherwise, if SIGNAL_P, signal an
   error; if !SIGNAL_P, return 0.  */

static Lisp_Object
face_boolean_x_resource_value (Lisp_Object value, bool signal_p)
{
  Lisp_Object result = make_fixnum (0);

  eassert (STRINGP (value));

  if (xstrcasecmp (SSDATA (value), "on") == 0
      || xstrcasecmp (SSDATA (value), "true") == 0)
    result = Qt;
  else if (xstrcasecmp (SSDATA (value), "off") == 0
	   || xstrcasecmp (SSDATA (value), "false") == 0)
    result = Qnil;
  else if (xstrcasecmp (SSDATA (value), "unspecified") == 0)
    result = Qunspecified;
  else if (signal_p)
    signal_error ("Invalid face attribute value from X resource", value);

  return result;
}


DEFUN ("internal-set-lisp-face-attribute-from-resource",
       Finternal_set_lisp_face_attribute_from_resource,
       Sinternal_set_lisp_face_attribute_from_resource,
       3, 4, 0, doc: /* */)
  (Lisp_Object face, Lisp_Object attr, Lisp_Object value, Lisp_Object frame)
{
  CHECK_SYMBOL (face);
  CHECK_SYMBOL (attr);
  CHECK_STRING (value);

  if (xstrcasecmp (SSDATA (value), "unspecified") == 0)
    value = Qunspecified;
  else if (EQ (attr, QCheight))
    {
      value = Fstring_to_number (value, Qnil);
      if (!FIXNUMP (value) || XFIXNUM (value) <= 0)
	signal_error ("Invalid face height from X resource", value);
    }
  else if (EQ (attr, QCbold) || EQ (attr, QCitalic))
    value = face_boolean_x_resource_value (value, true);
  else if (EQ (attr, QCweight) || EQ (attr, QCslant) || EQ (attr, QCwidth))
    value = intern (SSDATA (value));
  else if (EQ (attr, QCreverse_video)
           || EQ (attr, QCinverse_video)
           || EQ (attr, QCextend))
    value = face_boolean_x_resource_value (value, true);
  else if (EQ (attr, QCunderline)
	   || EQ (attr, QCoverline)
	   || EQ (attr, QCstrike_through))
    {
      Lisp_Object boolean_value;

      /* If the result of face_boolean_x_resource_value is t or nil,
	 VALUE does NOT specify a color. */
      boolean_value = face_boolean_x_resource_value (value, false);
      if (SYMBOLP (boolean_value))
	value = boolean_value;
    }
  else if (EQ (attr, QCbox) || EQ (attr, QCinherit))
    value = Fcar (Fread_from_string (value, Qnil, Qnil));

  return Finternal_set_lisp_face_attribute (face, attr, value, frame);
}

#endif /* HAVE_WINDOW_SYSTEM */


/***********************************************************************
			      Menu face
 ***********************************************************************/

#if defined HAVE_X_WINDOWS && defined USE_X_TOOLKIT

/* Make menus on frame F appear as specified by the `menu' face.  */

static void
x_update_menu_appearance (struct frame *f)
{
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  XrmDatabase rdb;

  if (dpyinfo
      && (rdb = XrmGetDatabase (FRAME_X_DISPLAY (f)),
	  rdb != NULL))
    {
      char line[512];
      char *buf = line;
      ptrdiff_t bufsize = sizeof line;
      Lisp_Object lface = lface_from_face_name (f, Qmenu, true);
      struct face *face = FACE_FROM_ID (f, MENU_FACE_ID);
      const char *myname = SSDATA (Vx_resource_name);
      bool changed_p = false;
#ifdef USE_MOTIF
      const char *popup_path = "popup_menu";
#else
      const char *popup_path = "menu.popup";
#endif

      if (STRINGP (LFACE_FOREGROUND (lface)))
	{
	  exprintf (&buf, &bufsize, line, -1, "%s.%s*foreground: %s",
		    myname, popup_path,
		    SDATA (LFACE_FOREGROUND (lface)));
	  XrmPutLineResource (&rdb, line);
	  exprintf (&buf, &bufsize, line, -1, "%s.pane.menubar*foreground: %s",
		    myname, SDATA (LFACE_FOREGROUND (lface)));
	  XrmPutLineResource (&rdb, line);
	  changed_p = true;
	}

      if (STRINGP (LFACE_BACKGROUND (lface)))
	{
	  exprintf (&buf, &bufsize, line, -1, "%s.%s*background: %s",
		    myname, popup_path,
		    SDATA (LFACE_BACKGROUND (lface)));
	  XrmPutLineResource (&rdb, line);

	  exprintf (&buf, &bufsize, line, -1, "%s.pane.menubar*background: %s",
		    myname, SDATA (LFACE_BACKGROUND (lface)));
	  XrmPutLineResource (&rdb, line);
	  changed_p = true;
	}

      if (face->font
	  /* On Solaris 5.8, it's been reported that the `menu' face
	     can be unspecified here, during startup.  Why this
	     happens remains unknown.  -- cyd  */
	  && FONTP (LFACE_FONT (lface))
	  && (!UNSPECIFIEDP (LFACE_FAMILY (lface))
	      || !UNSPECIFIEDP (LFACE_FOUNDRY (lface))
	      || !UNSPECIFIEDP (LFACE_SWIDTH (lface))
	      || !UNSPECIFIEDP (LFACE_WEIGHT (lface))
	      || !UNSPECIFIEDP (LFACE_SLANT (lface))
	      || !UNSPECIFIEDP (LFACE_HEIGHT (lface))))
	{
	  Lisp_Object xlfd = Ffont_xlfd_name (LFACE_FONT (lface), Qnil);
#ifdef USE_MOTIF
	  const char *suffix = "List";
	  bool motif = true;
#else
#if defined HAVE_X_I18N

	  const char *suffix = "Set";
#else
	  const char *suffix = "";
#endif
	  bool motif = false;
#endif

	  if (! NILP (xlfd))
	    {
#if defined HAVE_X_I18N
	      char *fontsetname = xic_create_fontsetname (SSDATA (xlfd), motif);
#else
	      char *fontsetname = SSDATA (xlfd);
#endif
	      exprintf (&buf, &bufsize, line, -1, "%s.pane.menubar*font%s: %s",
			myname, suffix, fontsetname);
	      XrmPutLineResource (&rdb, line);

	      exprintf (&buf, &bufsize, line, -1, "%s.%s*font%s: %s",
			myname, popup_path, suffix, fontsetname);
	      XrmPutLineResource (&rdb, line);
	      changed_p = true;
	      if (fontsetname != SSDATA (xlfd))
		xfree (fontsetname);
	    }
	}

      if (changed_p && f->output_data.x->menubar_widget)
	free_frame_menubar (f);

      if (buf != line)
	xfree (buf);
    }
}

#endif /* HAVE_X_WINDOWS && USE_X_TOOLKIT */


DEFUN ("face-attribute-relative-p", Fface_attribute_relative_p,
       Sface_attribute_relative_p,
       2, 2, 0,
       doc: /* Check whether a face attribute value is relative.
Specifically, this function returns t if the attribute ATTRIBUTE
with the value VALUE is relative.

A relative value is one that doesn't entirely override whatever is
inherited from another face.  For most possible attributes,
the only relative value that users see is `unspecified'.
However, for :height, floating point values are also relative.  */
       attributes: const)
  (Lisp_Object attribute, Lisp_Object value)
{
  if (EQ (value, Qunspecified) || (EQ (value, QCignore_defface)))
    return Qt;
  else if (EQ (attribute, QCheight))
    return FIXNUMP (value) ? Qnil : Qt;
  else
    return Qnil;
}

DEFUN ("merge-face-attribute", Fmerge_face_attribute, Smerge_face_attribute,
       3, 3, 0,
       doc: /* Return face ATTRIBUTE VALUE1 merged with VALUE2.
If VALUE1 or VALUE2 are absolute (see `face-attribute-relative-p'), then
the result will be absolute, otherwise it will be relative.  */)
  (Lisp_Object attribute, Lisp_Object value1, Lisp_Object value2)
{
  if (EQ (value1, Qunspecified) || EQ (value1, QCignore_defface))
    return value2;
  else if (EQ (attribute, QCheight))
    return merge_face_heights (value1, value2, value1);
  else
    return value1;
}


DEFUN ("internal-get-lisp-face-attribute", Finternal_get_lisp_face_attribute,
       Sinternal_get_lisp_face_attribute,
       2, 3, 0,
       doc: /* Return face attribute KEYWORD of face SYMBOL.
If SYMBOL does not name a valid Lisp face or KEYWORD isn't a valid
face attribute name, signal an error.
If the optional argument FRAME is given, report on face SYMBOL in that
frame.  If FRAME is t, report on the defaults for face SYMBOL (for new
frames).  If FRAME is omitted or nil, use the selected frame.  */)
  (Lisp_Object symbol, Lisp_Object keyword, Lisp_Object frame)
{
  struct frame *f = EQ (frame, Qt) ? NULL : decode_live_frame (frame);
  Lisp_Object lface = lface_from_face_name (f, symbol, true), value = Qnil;

  CHECK_SYMBOL (symbol);
  CHECK_SYMBOL (keyword);

  if (EQ (keyword, QCfamily))
    value = LFACE_FAMILY (lface);
  else if (EQ (keyword, QCfoundry))
    value = LFACE_FOUNDRY (lface);
  else if (EQ (keyword, QCheight))
    value = LFACE_HEIGHT (lface);
  else if (EQ (keyword, QCweight))
    value = LFACE_WEIGHT (lface);
  else if (EQ (keyword, QCslant))
    value = LFACE_SLANT (lface);
  else if (EQ (keyword, QCunderline))
    value = LFACE_UNDERLINE (lface);
  else if (EQ (keyword, QCoverline))
    value = LFACE_OVERLINE (lface);
  else if (EQ (keyword, QCstrike_through))
    value = LFACE_STRIKE_THROUGH (lface);
  else if (EQ (keyword, QCbox))
    value = LFACE_BOX (lface);
  else if (EQ (keyword, QCinverse_video)
	   || EQ (keyword, QCreverse_video))
    value = LFACE_INVERSE (lface);
  else if (EQ (keyword, QCforeground))
    value = LFACE_FOREGROUND (lface);
  else if (EQ (keyword, QCdistant_foreground))
    value = LFACE_DISTANT_FOREGROUND (lface);
  else if (EQ (keyword, QCbackground))
    value = LFACE_BACKGROUND (lface);
  else if (EQ (keyword, QCstipple))
    value = LFACE_STIPPLE (lface);
  else if (EQ (keyword, QCwidth))
    value = LFACE_SWIDTH (lface);
  else if (EQ (keyword, QCinherit))
    value = LFACE_INHERIT (lface);
  else if (EQ (keyword, QCextend))
    value = LFACE_EXTEND (lface);
  else if (EQ (keyword, QCfont))
    value = LFACE_FONT (lface);
  else if (EQ (keyword, QCfontset))
    value = LFACE_FONTSET (lface);
  else
    signal_error ("Invalid face attribute name", keyword);

  if (IGNORE_DEFFACE_P (value))
    return Qunspecified;

  return value;
}


DEFUN ("internal-lisp-face-attribute-values",
       Finternal_lisp_face_attribute_values,
       Sinternal_lisp_face_attribute_values, 1, 1, 0,
       doc: /* Return a list of valid discrete values for face attribute ATTR.
Value is nil if ATTR doesn't have a discrete set of valid values.  */)
  (Lisp_Object attr)
{
  Lisp_Object result = Qnil;

  CHECK_SYMBOL (attr);

  if (EQ (attr, QCunderline) || EQ (attr, QCoverline)
      || EQ (attr, QCstrike_through)
      || EQ (attr, QCinverse_video)
      || EQ (attr, QCreverse_video)
      || EQ (attr, QCextend))
    result = list2 (Qt, Qnil);

  return result;
}


DEFUN ("internal-merge-in-global-face", Finternal_merge_in_global_face,
       Sinternal_merge_in_global_face, 2, 2, 0,
       doc: /* Add attributes from frame-default definition of FACE to FACE on FRAME.
Default face attributes override any local face attributes.  */)
  (Lisp_Object face, Lisp_Object frame)
{
  int i;
  Lisp_Object global_lface, local_lface, *gvec, *lvec;
  struct frame *f = XFRAME (frame);

  CHECK_LIVE_FRAME (frame);
  global_lface = lface_from_face_name (NULL, face, true);
  local_lface = lface_from_face_name (f, face, false);
  if (NILP (local_lface))
    local_lface = Finternal_make_lisp_face (face, frame);

  /* Make every specified global attribute override the local one.
     BEWARE!! This is only used from `face-set-after-frame-default' where
     the local frame is defined from default specs in `face-defface-spec'
     and those should be overridden by global settings.  Hence the strange
     "global before local" priority.  */
  lvec = XVECTOR (local_lface)->contents;
  gvec = XVECTOR (global_lface)->contents;
  for (i = 1; i < LFACE_VECTOR_SIZE; ++i)
    if (IGNORE_DEFFACE_P (gvec[i]))
      ASET (local_lface, i, Qunspecified);
    else if (! UNSPECIFIEDP (gvec[i]))
      ASET (local_lface, i, AREF (global_lface, i));

  /* If the default face was changed, update the face cache and the
     `font' frame parameter.  */
  if (EQ (face, Qdefault))
    {
      struct face_cache *c = FRAME_FACE_CACHE (f);
      struct face *newface;
      struct face *oldface =
	c ? FACE_FROM_ID_OR_NULL (f, DEFAULT_FACE_ID) : NULL;
      Lisp_Object attrs[LFACE_VECTOR_SIZE];

      /* This can be NULL (e.g., in batch mode).  */
      if (oldface)
	{
	  /* Ensure that the face vector is fully specified by merging
	     the previously-cached vector.  */
	  memcpy (attrs, oldface->lface, sizeof attrs);

	  merge_face_vectors (NULL, f, lvec, attrs, 0);
	  vcopy (local_lface, 0, attrs, LFACE_VECTOR_SIZE);
	  newface = realize_face (c, lvec, DEFAULT_FACE_ID);

	  if ((! UNSPECIFIEDP (gvec[LFACE_FAMILY_INDEX])
	       || ! UNSPECIFIEDP (gvec[LFACE_FOUNDRY_INDEX])
	       || ! UNSPECIFIEDP (gvec[LFACE_HEIGHT_INDEX])
	       || ! UNSPECIFIEDP (gvec[LFACE_WEIGHT_INDEX])
	       || ! UNSPECIFIEDP (gvec[LFACE_SLANT_INDEX])
	       || ! UNSPECIFIEDP (gvec[LFACE_SWIDTH_INDEX])
	       || ! UNSPECIFIEDP (gvec[LFACE_FONT_INDEX]))
	      && newface->font)
	    {
	      Lisp_Object name = newface->font->props[FONT_NAME_INDEX];
	      AUTO_FRAME_ARG (arg, Qfont, name);

#ifdef HAVE_WINDOW_SYSTEM
	      if (FRAME_WINDOW_P (f))
		/* This is a window-system frame.  Prevent changes of
		   the `font' parameter here from messing with the
		   `font-parameter' frame property, as the frame
		   parameter is not being changed by the user.  */
	        gui_set_frame_parameters_1 (f, arg, true);
	      else
#endif
		Fmodify_frame_parameters (frame, arg);
	    }

	  if (STRINGP (gvec[LFACE_FOREGROUND_INDEX]))
	    {
	      AUTO_FRAME_ARG (arg, Qforeground_color,
			      gvec[LFACE_FOREGROUND_INDEX]);
	      Fmodify_frame_parameters (frame, arg);
	    }

	  if (STRINGP (gvec[LFACE_BACKGROUND_INDEX]))
	    {
	      AUTO_FRAME_ARG (arg, Qbackground_color,
			      gvec[LFACE_BACKGROUND_INDEX]);
	      Fmodify_frame_parameters (frame, arg);
	    }
	}
    }

  return Qnil;
}


/* The following function is implemented for compatibility with 20.2.
   The function is used in x-resolve-fonts when it is asked to
   return fonts with the same size as the font of a face.  This is
   done in fontset.el.  */

DEFUN ("face-font", Fface_font, Sface_font, 1, 3, 0,
       doc: /* Return the font name of face FACE, or nil if it is unspecified.
The font name is, by default, for ASCII characters.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
  The font default for a face is either nil, or a list
  of the form (bold), (italic) or (bold italic).
If FRAME is omitted or nil, use the selected frame.
If FRAME is anything but t, and the optional third argument CHARACTER
is given, return the font name used by FACE for CHARACTER on FRAME.  */)
  (Lisp_Object face, Lisp_Object frame, Lisp_Object character)
{
  if (EQ (frame, Qt))
    {
      Lisp_Object result = Qnil;
      Lisp_Object lface = lface_from_face_name (NULL, face, true);

      if (!UNSPECIFIEDP (LFACE_WEIGHT (lface))
	  && !EQ (LFACE_WEIGHT (lface), Qnormal))
	result = Fcons (Qbold, result);

      if (!UNSPECIFIEDP (LFACE_SLANT (lface))
	  && !EQ (LFACE_SLANT (lface), Qnormal))
	result = Fcons (Qitalic, result);

      return result;
    }
  else
    {
      struct frame *f = decode_live_frame (frame);
      int face_id = lookup_named_face (NULL, f, face, true);
      struct face *fface = FACE_FROM_ID_OR_NULL (f, face_id);

      if (! fface)
	return Qnil;
#ifdef HAVE_WINDOW_SYSTEM
      if (FRAME_WINDOW_P (f) && !NILP (character))
	{
	  CHECK_CHARACTER (character);
	  face_id = FACE_FOR_CHAR (f, fface, XFIXNUM (character), -1, Qnil);
	  fface = FACE_FROM_ID_OR_NULL (f, face_id);
	}
      return ((fface && fface->font)
	      ? fface->font->props[FONT_NAME_INDEX]
	      : Qnil);
#else  /* !HAVE_WINDOW_SYSTEM */
      return build_string (FRAME_MSDOS_P (f)
			   ? "ms-dos"
			   : FRAME_W32_P (f) ? "w32term"
			   :"tty");
#endif
    }
}


/* Compare face-attribute values v1 and v2 for equality.  Value is true if
   all attributes are `equal'.  Tries to be fast because this function
   is called quite often.  */

static bool
face_attr_equal_p (Lisp_Object v1, Lisp_Object v2)
{
  /* Type can differ, e.g. when one attribute is unspecified, i.e. nil,
     and the other is specified.  */
  if (XTYPE (v1) != XTYPE (v2))
    return false;

  if (EQ (v1, v2))
    return true;

  switch (XTYPE (v1))
    {
    case Lisp_String:
      if (SBYTES (v1) != SBYTES (v2))
	return false;

      return memcmp (SDATA (v1), SDATA (v2), SBYTES (v1)) == 0;

    case_Lisp_Int:
    case Lisp_Symbol:
      return false;

    default:
      return !NILP (Fequal (v1, v2));
    }
}


/* Compare face vectors V1 and V2 for equality.  Value is true if
   all attributes are `equal'.  Tries to be fast because this function
   is called quite often.  */

static bool
lface_equal_p (Lisp_Object *v1, Lisp_Object *v2)
{
  int i;
  bool equal_p = true;

  for (i = 1; i < LFACE_VECTOR_SIZE && equal_p; ++i)
    equal_p = face_attr_equal_p (v1[i], v2[i]);

  return equal_p;
}


DEFUN ("internal-lisp-face-equal-p", Finternal_lisp_face_equal_p,
       Sinternal_lisp_face_equal_p, 2, 3, 0,
       doc: /* True if FACE1 and FACE2 are equal.
If the optional argument FRAME is given, report on FACE1 and FACE2 in that frame.
If FRAME is t, report on the defaults for FACE1 and FACE2 (for new frames).
If FRAME is omitted or nil, use the selected frame.  */)
  (Lisp_Object face1, Lisp_Object face2, Lisp_Object frame)
{
  bool equal_p;
  struct frame *f;
  Lisp_Object lface1, lface2;

  /* Don't use decode_window_system_frame here because this function
     is called before X frames exist.  At that time, if FRAME is nil,
     selected_frame will be used which is the frame dumped with
     Emacs.  That frame is not an X frame.  */
  f = EQ (frame, Qt) ? NULL : decode_live_frame (frame);

  lface1 = lface_from_face_name (f, face1, true);
  lface2 = lface_from_face_name (f, face2, true);
  equal_p = lface_equal_p (XVECTOR (lface1)->contents,
			   XVECTOR (lface2)->contents);
  return equal_p ? Qt : Qnil;
}


DEFUN ("internal-lisp-face-empty-p", Finternal_lisp_face_empty_p,
       Sinternal_lisp_face_empty_p, 1, 2, 0,
       doc: /* True if FACE has no attribute specified.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame.  */)
  (Lisp_Object face, Lisp_Object frame)
{
  struct frame *f = EQ (frame, Qt) ? NULL : decode_live_frame (frame);
  Lisp_Object lface = lface_from_face_name (f, face, true);
  int i;

  for (i = 1; i < LFACE_VECTOR_SIZE; ++i)
    if (!UNSPECIFIEDP (AREF (lface, i)))
      break;

  return i == LFACE_VECTOR_SIZE ? Qt : Qnil;
}

DEFUN ("frame--face-hash-table", Fframe_face_hash_table, Sframe_face_hash_table,
       0, 1, 0,
       doc: /* Return a hash table of frame-local faces defined on FRAME.
For internal use only.  */)
  (Lisp_Object frame)
{
  return decode_live_frame (frame)->face_hash_table;
}


/* Return a hash code for Lisp string STRING with case ignored.  Used
   below in computing a hash value for a Lisp face.  */

static uintptr_t
hash_string_case_insensitive (Lisp_Object string)
{
  const unsigned char *s;
  uintptr_t hash = 0;
  eassert (STRINGP (string));
  for (s = SDATA (string); *s; ++s)
    hash = (hash << 1) ^ c_tolower (*s);
  return hash;
}


/* Return a hash code for face attribute vector V.  */

static uintptr_t
lface_hash (Lisp_Object *v)
{
  return (hash_string_case_insensitive (v[LFACE_FAMILY_INDEX])
	  ^ hash_string_case_insensitive (v[LFACE_FOUNDRY_INDEX])
	  ^ hash_string_case_insensitive (v[LFACE_FOREGROUND_INDEX])
	  ^ hash_string_case_insensitive (v[LFACE_BACKGROUND_INDEX])
	  ^ XHASH (v[LFACE_WEIGHT_INDEX])
	  ^ XHASH (v[LFACE_SLANT_INDEX])
	  ^ XHASH (v[LFACE_SWIDTH_INDEX])
	  ^ XHASH (v[LFACE_HEIGHT_INDEX]));
}

#ifdef HAVE_WINDOW_SYSTEM

/* Return true if LFACE1 and LFACE2 specify the same font (without
   considering charsets/registries).  They do if they specify the same
   family, point size, weight, width, slant, and font.  Both
   LFACE1 and LFACE2 must be fully-specified.  */

static bool
lface_same_font_attributes_p (Lisp_Object *lface1, Lisp_Object *lface2)
{
  eassert (lface_fully_specified_p (lface1)
	   && lface_fully_specified_p (lface2));
  return (xstrcasecmp (SSDATA (lface1[LFACE_FAMILY_INDEX]),
		       SSDATA (lface2[LFACE_FAMILY_INDEX])) == 0
	  && xstrcasecmp (SSDATA (lface1[LFACE_FOUNDRY_INDEX]),
			  SSDATA (lface2[LFACE_FOUNDRY_INDEX])) == 0
	  && EQ (lface1[LFACE_HEIGHT_INDEX], lface2[LFACE_HEIGHT_INDEX])
	  && EQ (lface1[LFACE_SWIDTH_INDEX], lface2[LFACE_SWIDTH_INDEX])
	  && EQ (lface1[LFACE_WEIGHT_INDEX], lface2[LFACE_WEIGHT_INDEX])
	  && EQ (lface1[LFACE_SLANT_INDEX], lface2[LFACE_SLANT_INDEX])
	  && EQ (lface1[LFACE_FONT_INDEX], lface2[LFACE_FONT_INDEX])
	  && (EQ (lface1[LFACE_FONTSET_INDEX], lface2[LFACE_FONTSET_INDEX])
	      || (STRINGP (lface1[LFACE_FONTSET_INDEX])
		  && STRINGP (lface2[LFACE_FONTSET_INDEX])
		  && ! xstrcasecmp (SSDATA (lface1[LFACE_FONTSET_INDEX]),
				    SSDATA (lface2[LFACE_FONTSET_INDEX]))))
	  );
}

#endif /* HAVE_WINDOW_SYSTEM */

/***********************************************************************
			    Realized Faces
 ***********************************************************************/

/* Allocate and return a new realized face for Lisp face attribute
   vector ATTR.  */

static struct face *
make_realized_face (Lisp_Object *attr)
{
  enum { off = offsetof (struct face, id) };
  struct face *face = xmalloc (sizeof *face);

  memcpy (face->lface, attr, sizeof face->lface);
  memset (&face->id, 0, sizeof *face - off);
  face->ascii_face = face;

  return face;
}


/* Free realized face FACE, including its X resources.  FACE may
   be null.  */

static void
free_realized_face (struct frame *f, struct face *face)
{
  if (face)
    {
#ifdef HAVE_WINDOW_SYSTEM
      if (FRAME_WINDOW_P (f))
	{
	  /* Free fontset of FACE if it is ASCII face.  */
	  if (face->fontset >= 0 && face == face->ascii_face)
	    free_face_fontset (f, face);
	  if (face->gc)
	    {
	      block_input ();
	      if (face->font)
		font_done_for_face (f, face);
	      x_free_gc (f, face->gc);
	      face->gc = 0;
	      unblock_input ();
	    }
#ifdef HAVE_X_WINDOWS
	  free_face_colors (f, face);
#endif /* HAVE_X_WINDOWS */
	  image_destroy_bitmap (f, face->stipple);
	}
#endif /* HAVE_WINDOW_SYSTEM */

      xfree (face);
    }
}

#ifdef HAVE_WINDOW_SYSTEM

/* Prepare face FACE for subsequent display on frame F.  This must be called
   before using X resources of FACE to allocate GCs if they haven't been
   allocated yet or have been freed by clearing the face cache.  */

void
prepare_face_for_display (struct frame *f, struct face *face)
{
  Emacs_GC egc;
  unsigned long mask;

  eassert (FRAME_WINDOW_P (f));

  if (face->gc == 0)
    {
      mask = GCForeground | GCBackground | GCGraphicsExposures;

      egc.foreground = face->foreground;
      egc.background = face->background;
#ifdef HAVE_X_WINDOWS
      egc.graphics_exposures = False;

      /* While this was historically slower than a line_width of 0,
	 the difference no longer matters on modern X servers, so set
	 it to 1 in order for PolyLine requests to behave consistently
	 everywhere.  */
      mask |= GCLineWidth;
      egc.line_width = 1;
#endif

      block_input ();
#ifdef HAVE_X_WINDOWS
      if (face->stipple)
	{
	  egc.fill_style = FillOpaqueStippled;
	  egc.stipple = image_bitmap_pixmap (f, face->stipple);
	  mask |= GCFillStyle | GCStipple;
	}
#endif
      face->gc = x_create_gc (f, mask, &egc);
      if (face->font)
	font_prepare_for_face (f, face);
      unblock_input ();
    }
}

#endif /* HAVE_WINDOW_SYSTEM */

/* Returns the `distance' between the colors X and Y.  */

static int
color_distance (Emacs_Color *x, Emacs_Color *y)
{
  /* This formula is from a paper titled `Colour metric' by Thiadmer Riemersma.
     Quoting from that paper:

	 This formula has results that are very close to L*u*v* (with the
	 modified lightness curve) and, more importantly, it is a more even
	 algorithm: it does not have a range of colors where it suddenly
	 gives far from optimal results.

     See <https://www.compuphase.com/cmetric.htm> for more info.  */

  long long r = x->red   - y->red;
  long long g = x->green - y->green;
  long long b = x->blue  - y->blue;
  long long r_mean = (x->red + y->red) >> 1;

  return (((((2 * 65536 + r_mean) * r * r) >> 16)
           + 4 * g * g
           + (((2 * 65536 + 65535 - r_mean) * b * b) >> 16))
          >> 16);
}


DEFUN ("color-distance", Fcolor_distance, Scolor_distance, 2, 4, 0,
       doc: /* Return an integer distance between COLOR1 and COLOR2 on FRAME.
COLOR1 and COLOR2 may be either strings containing the color name,
or lists of the form (RED GREEN BLUE), each in the range 0 to 65535 inclusive.
If FRAME is unspecified or nil, the current frame is used.
If METRIC is specified, it should be a function that accepts
two lists of the form (RED GREEN BLUE) aforementioned.
Despite the name, this is not a true distance metric as it does not satisfy
the triangle inequality.  */)
  (Lisp_Object color1, Lisp_Object color2, Lisp_Object frame,
   Lisp_Object metric)
{
  struct frame *f = decode_live_frame (frame);
  Emacs_Color cdef1, cdef2;

  if (!(CONSP (color1) && parse_rgb_list (color1, &cdef1))
      && !(STRINGP (color1)
           && FRAME_TERMINAL (f)->defined_color_hook (f,
                                                      SSDATA (color1),
                                                      &cdef1,
                                                      false,
                                                      true)))
    signal_error ("Invalid color", color1);
  if (!(CONSP (color2) && parse_rgb_list (color2, &cdef2))
      && !(STRINGP (color2)
           && FRAME_TERMINAL (f)->defined_color_hook (f,
                                                      SSDATA (color2),
                                                      &cdef2,
                                                      false,
                                                      true)))
    signal_error ("Invalid color", color2);

  if (NILP (metric))
    return make_fixnum (color_distance (&cdef1, &cdef2));
  else
    return call2 (metric,
		  list3i (cdef1.red, cdef1.green, cdef1.blue),
		  list3i (cdef2.red, cdef2.green, cdef2.blue));
}


/***********************************************************************
			      Face Cache
 ***********************************************************************/

/* Return a new face cache for frame F.  */

static struct face_cache *
make_face_cache (struct frame *f)
{
  struct face_cache *c = xmalloc (sizeof *c);

  c->buckets = xzalloc (FACE_CACHE_BUCKETS_SIZE * sizeof *c->buckets);
  c->size = 50;
  c->used = 0;
  c->faces_by_id = xmalloc (c->size * sizeof *c->faces_by_id);
  c->f = f;
  c->menu_face_changed_p = menu_face_changed_default;
  return c;
}

#ifdef HAVE_WINDOW_SYSTEM

/* Clear out all graphics contexts for all realized faces, except for
   the basic faces.  This should be done from time to time just to avoid
   keeping too many graphics contexts that are no longer needed.  */

static void
clear_face_gcs (struct face_cache *c)
{
  if (c && FRAME_WINDOW_P (c->f))
    {
      int i;
      for (i = BASIC_FACE_ID_SENTINEL; i < c->used; ++i)
	{
	  struct face *face = c->faces_by_id[i];
	  if (face && face->gc)
	    {
	      block_input ();
	      if (face->font)
		font_done_for_face (c->f, face);
	      x_free_gc (c->f, face->gc);
	      face->gc = 0;
	      unblock_input ();
	    }
	}
    }
}

#endif /* HAVE_WINDOW_SYSTEM */

/* Free all realized faces in face cache C, including basic faces.
   C may be null.  If faces are freed, make sure the frame's current
   matrix is marked invalid, so that a display caused by an expose
   event doesn't try to use faces we destroyed.  */

static void
free_realized_faces (struct face_cache *c)
{
  if (c && c->used)
    {
      int i, size;
      struct frame *f = c->f;

      /* We must block input here because we can't process X events
	 safely while only some faces are freed, or when the frame's
	 current matrix still references freed faces.  */
      block_input ();

      for (i = 0; i < c->used; ++i)
	{
	  free_realized_face (f, c->faces_by_id[i]);
	  c->faces_by_id[i] = NULL;
	}

      /* Forget the escape-glyph and glyphless-char faces.  */
      forget_escape_and_glyphless_faces ();
      c->used = 0;
      size = FACE_CACHE_BUCKETS_SIZE * sizeof *c->buckets;
      memset (c->buckets, 0, size);

      /* Must do a thorough redisplay the next time.  Mark current
	 matrices as invalid because they will reference faces freed
	 above.  This function is also called when a frame is
	 destroyed.  In this case, the root window of F is nil.  */
      if (WINDOWP (f->root_window))
	{
	  clear_current_matrices (f);
	  fset_redisplay (f);
	}

      unblock_input ();
    }
}


/* Free all realized faces on FRAME or on all frames if FRAME is nil.
   This is done after attributes of a named face have been changed,
   because we can't tell which realized faces depend on that face.  */

void
free_all_realized_faces (Lisp_Object frame)
{
  if (NILP (frame))
    {
      Lisp_Object rest;
      FOR_EACH_FRAME (rest, frame)
	free_realized_faces (FRAME_FACE_CACHE (XFRAME (frame)));
      windows_or_buffers_changed = 58;
    }
  else
    free_realized_faces (FRAME_FACE_CACHE (XFRAME (frame)));
}


/* Free face cache C and faces in it, including their X resources.  */

static void
free_face_cache (struct face_cache *c)
{
  if (c)
    {
      free_realized_faces (c);
      xfree (c->buckets);
      xfree (c->faces_by_id);
      xfree (c);
    }
}


/* Cache realized face FACE in face cache C.  HASH is the hash value
   of FACE.  If FACE is for ASCII characters (i.e. FACE->ascii_face ==
   FACE), insert the new face to the beginning of the collision list
   of the face hash table of C.  Otherwise, add the new face to the
   end of the collision list.  This way, lookup_face can quickly find
   that a requested face is not cached.  */

static void
cache_face (struct face_cache *c, struct face *face, uintptr_t hash)
{
  int i = hash % FACE_CACHE_BUCKETS_SIZE;

  face->hash = hash;

  if (face->ascii_face != face)
    {
      struct face *last = c->buckets[i];
      if (last)
	{
	  while (last->next)
	    last = last->next;
	  last->next = face;
	  face->prev = last;
	  face->next = NULL;
	}
      else
	{
	  c->buckets[i] = face;
	  face->prev = face->next = NULL;
	}
    }
  else
    {
      face->prev = NULL;
      face->next = c->buckets[i];
      if (face->next)
	face->next->prev = face;
      c->buckets[i] = face;
    }

  /* Find a free slot in C->faces_by_id and use the index of the free
     slot as FACE->id.  */
  for (i = 0; i < c->used; ++i)
    if (c->faces_by_id[i] == NULL)
      break;
  face->id = i;

#ifdef GLYPH_DEBUG
  /* Check that FACE got a unique id.  */
  {
    int j, n;
    struct face *face1;

    for (j = n = 0; j < FACE_CACHE_BUCKETS_SIZE; ++j)
      for (face1 = c->buckets[j]; face1; face1 = face1->next)
	if (face1->id == i)
	  ++n;

    eassert (n == 1);
  }
#endif /* GLYPH_DEBUG */

  /* Maybe enlarge C->faces_by_id.  */
  if (i == c->used)
    {
      if (c->used == c->size)
	c->faces_by_id = xpalloc (c->faces_by_id, &c->size, 1, MAX_FACE_ID,
				  sizeof *c->faces_by_id);
      c->used++;
    }

  c->faces_by_id[i] = face;
}


/* Remove face FACE from cache C.  */

static void
uncache_face (struct face_cache *c, struct face *face)
{
  int i = face->hash % FACE_CACHE_BUCKETS_SIZE;

  if (face->prev)
    face->prev->next = face->next;
  else
    c->buckets[i] = face->next;

  if (face->next)
    face->next->prev = face->prev;

  c->faces_by_id[face->id] = NULL;
  if (face->id == c->used)
    --c->used;
}


/* Look up a realized face with face attributes ATTR in the face cache
   of frame F.  The face will be used to display ASCII characters.
   Value is the ID of the face found.  If no suitable face is found,
   realize a new one.  */

static int
lookup_face (struct frame *f, Lisp_Object *attr)
{
  struct face_cache *cache = FRAME_FACE_CACHE (f);
  struct face *face;

  eassert (cache != NULL);
  check_lface_attrs (attr);

  /* Look up ATTR in the face cache.  */
  uintptr_t hash = lface_hash (attr);
  int i = hash % FACE_CACHE_BUCKETS_SIZE;

  for (face = cache->buckets[i]; face; face = face->next)
    {
      if (face->ascii_face != face)
	{
	  /* There's no more ASCII face.  */
	  face = NULL;
	  break;
	}
      if (face->hash == hash
	  && lface_equal_p (face->lface, attr))
	break;
    }

  /* If not found, realize a new face.  */
  if (face == NULL)
    face = realize_face (cache, attr, -1);

#ifdef GLYPH_DEBUG
  eassert (face == FACE_FROM_ID_OR_NULL (f, face->id));
#endif /* GLYPH_DEBUG */

  return face->id;
}

#ifdef HAVE_WINDOW_SYSTEM
/* Look up a realized face that has the same attributes as BASE_FACE
   except for the font in the face cache of frame F.  If FONT-OBJECT
   is not nil, it is an already opened font.  If FONT-OBJECT is nil,
   the face has no font.  Value is the ID of the face found.  If no
   suitable face is found, realize a new one.  */

int
face_for_font (struct frame *f, Lisp_Object font_object,
               struct face *base_face)
{
  struct face_cache *cache = FRAME_FACE_CACHE (f);
  uintptr_t hash;
  int i;
  struct face *face;

  eassert (cache != NULL);
  base_face = base_face->ascii_face;
  hash = lface_hash (base_face->lface);
  i = hash % FACE_CACHE_BUCKETS_SIZE;

  for (face = cache->buckets[i]; face; face = face->next)
    {
      if (face->ascii_face == face)
	continue;
      if (face->ascii_face == base_face
	  && face->font == (NILP (font_object) ? NULL
			    : XFONT_OBJECT (font_object))
	  && lface_equal_p (face->lface, base_face->lface))
	return face->id;
    }

  /* If not found, realize a new face.  */
  face = realize_non_ascii_face (f, font_object, base_face);
  return face->id;
}
#endif	/* HAVE_WINDOW_SYSTEM */

/* Return the face id of the realized face for named face SYMBOL on
   frame F suitable for displaying ASCII characters.  Value is -1 if
   the face couldn't be determined, which might happen if the default
   face isn't realized and cannot be realized.  If window W is given,
   consider face remappings specified for W or for W's buffer.  If W
   is NULL, consider only frame-level face configuration.  */
int
lookup_named_face (struct window *w, struct frame *f,
                   Lisp_Object symbol, bool signal_p)
{
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  Lisp_Object symbol_attrs[LFACE_VECTOR_SIZE];
  struct face *default_face = FACE_FROM_ID_OR_NULL (f, DEFAULT_FACE_ID);

  if (default_face == NULL)
    {
      if (!realize_basic_faces (f))
	return -1;
      default_face = FACE_FROM_ID (f, DEFAULT_FACE_ID);
    }

  if (! get_lface_attributes (w, f, symbol, symbol_attrs, signal_p, 0))
    return -1;

  memcpy (attrs, default_face->lface, sizeof attrs);

  /* Make explicit any attributes whose value is 'reset'.  */
  int i;
  for (i = 1; i < LFACE_VECTOR_SIZE; i++)
    if (EQ (symbol_attrs[i], Qreset))
      symbol_attrs[i] = attrs[i];

  merge_face_vectors (w, f, symbol_attrs, attrs, 0);

  return lookup_face (f, attrs);
}


/* Return the display face-id of the basic face whose canonical face-id
   is FACE_ID.  The return value will usually simply be FACE_ID, unless that
   basic face has been remapped via Vface_remapping_alist.  This function is
   conservative: if something goes wrong, it will simply return FACE_ID
   rather than signal an error.  Window W, if non-NULL, is used to filter
   face specifications for remapping.  */
int
lookup_basic_face (struct window *w, struct frame *f, int face_id)
{
  Lisp_Object name, mapping;
  int remapped_face_id;

  if (NILP (Vface_remapping_alist))
    return face_id;		/* Nothing to do.  */

  switch (face_id)
    {
    case DEFAULT_FACE_ID:		name = Qdefault;		break;
    case MODE_LINE_ACTIVE_FACE_ID:	name = Qmode_line_active;      	break;
    case MODE_LINE_INACTIVE_FACE_ID:	name = Qmode_line_inactive;	break;
    case HEADER_LINE_FACE_ID:		name = Qheader_line;		break;
    case TAB_LINE_FACE_ID:		name = Qtab_line;		break;
    case TAB_BAR_FACE_ID:		name = Qtab_bar;		break;
    case TOOL_BAR_FACE_ID:		name = Qtool_bar;		break;
    case FRINGE_FACE_ID:		name = Qfringe;			break;
    case SCROLL_BAR_FACE_ID:		name = Qscroll_bar;		break;
    case BORDER_FACE_ID:		name = Qborder;			break;
    case CURSOR_FACE_ID:		name = Qcursor;			break;
    case MOUSE_FACE_ID:			name = Qmouse;			break;
    case MENU_FACE_ID:			name = Qmenu;			break;
    case WINDOW_DIVIDER_FACE_ID:	name = Qwindow_divider;		break;
    case VERTICAL_BORDER_FACE_ID: 	name = Qvertical_border; 	break;
    case WINDOW_DIVIDER_FIRST_PIXEL_FACE_ID:	name = Qwindow_divider_first_pixel;	break;
    case WINDOW_DIVIDER_LAST_PIXEL_FACE_ID:	name = Qwindow_divider_last_pixel;	break;
    case INTERNAL_BORDER_FACE_ID:	name = Qinternal_border; 	break;
    case CHILD_FRAME_BORDER_FACE_ID:	name = Qchild_frame_border; 	break;

    default:
      emacs_abort (); /* the caller is supposed to pass us a basic face id */
    }

  /* Do a quick scan through Vface_remapping_alist, and return immediately
     if there is no remapping for face NAME.  This is just an optimization
     for the very common no-remapping case.  */
  mapping = assq_no_quit (name, Vface_remapping_alist);
  if (NILP (mapping))
    return face_id;		/* Give up.  */

  /* If there is a remapping entry, lookup the face using NAME, which will
     handle the remapping too.  */
  remapped_face_id = lookup_named_face (w, f, name, false);
  if (remapped_face_id < 0)
    return face_id;		/* Give up. */

  return remapped_face_id;
}


/* Return a face for charset ASCII that is like the face with id
   FACE_ID on frame F, but has a font that is STEPS steps smaller.
   STEPS < 0 means larger.  Value is the id of the face.  */

int
smaller_face (struct frame *f, int face_id, int steps)
{
#ifdef HAVE_WINDOW_SYSTEM
  struct face *face;
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  int pt, last_pt, last_height;
  int delta;
  int new_face_id;
  struct face *new_face;

  /* If not called for an X frame, just return the original face.  */
  if (FRAME_TERMCAP_P (f))
    return face_id;

  /* Try in increments of 1/2 pt.  */
  delta = steps < 0 ? 5 : -5;
  steps = eabs (steps);

  face = FACE_FROM_ID (f, face_id);
  memcpy (attrs, face->lface, sizeof attrs);
  pt = last_pt = XFIXNAT (attrs[LFACE_HEIGHT_INDEX]);
  new_face_id = face_id;
  last_height = FONT_HEIGHT (face->font);

  while (steps
	 && pt + delta > 0
	 /* Give up if we cannot find a font within 10pt.  */
	 && eabs (last_pt - pt) < 100)
    {
      /* Look up a face for a slightly smaller/larger font.  */
      pt += delta;
      attrs[LFACE_HEIGHT_INDEX] = make_fixnum (pt);
      new_face_id = lookup_face (f, attrs);
      new_face = FACE_FROM_ID (f, new_face_id);

      /* If height changes, count that as one step.  */
      if ((delta < 0 && FONT_HEIGHT (new_face->font) < last_height)
	  || (delta > 0 && FONT_HEIGHT (new_face->font) > last_height))
	{
	  --steps;
	  last_height = FONT_HEIGHT (new_face->font);
	  last_pt = pt;
	}
    }

  return new_face_id;

#else /* not HAVE_WINDOW_SYSTEM */

  return face_id;

#endif /* not HAVE_WINDOW_SYSTEM */
}


/* Return a face for charset ASCII that is like the face with id
   FACE_ID on frame F, but has height HEIGHT.  */

int
face_with_height (struct frame *f, int face_id, int height)
{
#ifdef HAVE_WINDOW_SYSTEM
  struct face *face;
  Lisp_Object attrs[LFACE_VECTOR_SIZE];

  if (FRAME_TERMCAP_P (f)
      || height <= 0)
    return face_id;

  face = FACE_FROM_ID (f, face_id);
  memcpy (attrs, face->lface, sizeof attrs);
  attrs[LFACE_HEIGHT_INDEX] = make_fixnum (height);
  font_clear_prop (attrs, FONT_SIZE_INDEX);
  face_id = lookup_face (f, attrs);
#endif /* HAVE_WINDOW_SYSTEM */

  return face_id;
}


/* Return the face id of the realized face for named face SYMBOL on
   frame F suitable for displaying ASCII characters, and use
   attributes of the face FACE_ID for attributes that aren't
   completely specified by SYMBOL.  This is like lookup_named_face,
   except that the default attributes come from FACE_ID, not from the
   default face.  FACE_ID is assumed to be already realized.
   Window W, if non-NULL, filters face specifications.  */
int
lookup_derived_face (struct window *w,
                     struct frame *f, Lisp_Object symbol, int face_id,
		     bool signal_p)
{
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  Lisp_Object symbol_attrs[LFACE_VECTOR_SIZE];
  struct face *default_face;

  if (!get_lface_attributes (w, f, symbol, symbol_attrs, signal_p, 0))
    return -1;

  default_face = FACE_FROM_ID (f, face_id);
  memcpy (attrs, default_face->lface, sizeof attrs);

  /* Make explicit any attributes whose value is 'reset'.  */
  int i;
  for (i = 1; i < LFACE_VECTOR_SIZE; i++)
    if (EQ (symbol_attrs[i], Qreset))
      symbol_attrs[i] = attrs[i];

  merge_face_vectors (w, f, symbol_attrs, attrs, 0);
  return lookup_face (f, attrs);
}

DEFUN ("face-attributes-as-vector", Fface_attributes_as_vector,
       Sface_attributes_as_vector, 1, 1, 0,
       doc: /* Return a vector of face attributes corresponding to PLIST.  */)
  (Lisp_Object plist)
{
  Lisp_Object lface = make_vector (LFACE_VECTOR_SIZE, Qunspecified);
  merge_face_ref (NULL, XFRAME (selected_frame),
                  plist, XVECTOR (lface)->contents,
                  true, NULL, 0);
  return lface;
}



/***********************************************************************
			Face capability testing
 ***********************************************************************/


/* If the distance (as returned by color_distance) between two colors is
   less than this, then they are considered the same, for determining
   whether a color is supported or not.  */

#define TTY_SAME_COLOR_THRESHOLD  10000

#ifdef HAVE_WINDOW_SYSTEM

/* Return true if all the face attributes in ATTRS are supported
   on the window-system frame F.

   The definition of `supported' is somewhat heuristic, but basically means
   that a face containing all the attributes in ATTRS, when merged with the
   default face for display, can be represented in a way that's

    (1) different in appearance from the default face, and
    (2) `close in spirit' to what the attributes specify, if not exact.  */

static bool
gui_supports_face_attributes_p (struct frame *f,
                                Lisp_Object attrs[LFACE_VECTOR_SIZE],
                                struct face *def_face)
{
  Lisp_Object *def_attrs = def_face->lface;
  Lisp_Object lattrs[LFACE_VECTOR_SIZE];

  /* Make explicit any attributes whose value is 'reset'.  */
  int i;
  for (i = 1; i < LFACE_VECTOR_SIZE; i++)
    {
      if (EQ (attrs[i], Qreset))
	lattrs[i] = def_attrs[i];
      else
	lattrs[i] = attrs[i];
    }

  /* Check that other specified attributes are different from the
     default face.  */
  if ((!UNSPECIFIEDP (lattrs[LFACE_UNDERLINE_INDEX])
       && face_attr_equal_p (lattrs[LFACE_UNDERLINE_INDEX],
			     def_attrs[LFACE_UNDERLINE_INDEX]))
      || (!UNSPECIFIEDP (lattrs[LFACE_INVERSE_INDEX])
	  && face_attr_equal_p (lattrs[LFACE_INVERSE_INDEX],
				def_attrs[LFACE_INVERSE_INDEX]))
      || (!UNSPECIFIEDP (lattrs[LFACE_EXTEND_INDEX])
	  && face_attr_equal_p (lattrs[LFACE_EXTEND_INDEX],
				def_attrs[LFACE_EXTEND_INDEX]))
      || (!UNSPECIFIEDP (lattrs[LFACE_FOREGROUND_INDEX])
	  && face_attr_equal_p (lattrs[LFACE_FOREGROUND_INDEX],
				def_attrs[LFACE_FOREGROUND_INDEX]))
      || (!UNSPECIFIEDP (lattrs[LFACE_DISTANT_FOREGROUND_INDEX])
	  && face_attr_equal_p (lattrs[LFACE_DISTANT_FOREGROUND_INDEX],
				def_attrs[LFACE_DISTANT_FOREGROUND_INDEX]))
      || (!UNSPECIFIEDP (lattrs[LFACE_BACKGROUND_INDEX])
	  && face_attr_equal_p (lattrs[LFACE_BACKGROUND_INDEX],
				def_attrs[LFACE_BACKGROUND_INDEX]))
      || (!UNSPECIFIEDP (lattrs[LFACE_STIPPLE_INDEX])
	  && face_attr_equal_p (lattrs[LFACE_STIPPLE_INDEX],
				def_attrs[LFACE_STIPPLE_INDEX]))
      || (!UNSPECIFIEDP (lattrs[LFACE_OVERLINE_INDEX])
	  && face_attr_equal_p (lattrs[LFACE_OVERLINE_INDEX],
				def_attrs[LFACE_OVERLINE_INDEX]))
      || (!UNSPECIFIEDP (lattrs[LFACE_STRIKE_THROUGH_INDEX])
	  && face_attr_equal_p (lattrs[LFACE_STRIKE_THROUGH_INDEX],
				def_attrs[LFACE_STRIKE_THROUGH_INDEX]))
      || (!UNSPECIFIEDP (lattrs[LFACE_BOX_INDEX])
	  && face_attr_equal_p (lattrs[LFACE_BOX_INDEX],
				def_attrs[LFACE_BOX_INDEX])))
    return false;

  /* Check font-related attributes, as those are the most commonly
     "unsupported" on a window-system (because of missing fonts).  */
  if (!UNSPECIFIEDP (lattrs[LFACE_FAMILY_INDEX])
      || !UNSPECIFIEDP (lattrs[LFACE_FOUNDRY_INDEX])
      || !UNSPECIFIEDP (lattrs[LFACE_HEIGHT_INDEX])
      || !UNSPECIFIEDP (lattrs[LFACE_WEIGHT_INDEX])
      || !UNSPECIFIEDP (lattrs[LFACE_SLANT_INDEX])
      || !UNSPECIFIEDP (lattrs[LFACE_SWIDTH_INDEX]))
    {
      int face_id;
      struct face *face;
      Lisp_Object merged_attrs[LFACE_VECTOR_SIZE];
      int i;

      memcpy (merged_attrs, def_attrs, sizeof merged_attrs);

      merge_face_vectors (NULL, f, attrs, merged_attrs, 0);

      face_id = lookup_face (f, merged_attrs);
      face = FACE_FROM_ID_OR_NULL (f, face_id);

      if (! face)
	error ("Cannot make face");

      /* If the font is the same, or no font is found, then not
	 supported.  */
      if (face->font == def_face->font
	  || ! face->font)
	return false;
      for (i = FONT_TYPE_INDEX; i <= FONT_SIZE_INDEX; i++)
	if (! EQ (face->font->props[i], def_face->font->props[i]))
	  {
	    Lisp_Object s1, s2;

	    if (i < FONT_FOUNDRY_INDEX || i > FONT_REGISTRY_INDEX
		|| face->font->driver->case_sensitive)
	      return true;
	    s1 = SYMBOL_NAME (face->font->props[i]);
	    s2 = SYMBOL_NAME (def_face->font->props[i]);
	    if (! BASE_EQ (Fcompare_strings (s1, make_fixnum (0), Qnil,
					     s2, make_fixnum (0), Qnil, Qt),
			   Qt))
	      return true;
	  }
      return false;
    }

  /* Everything checks out, this face is supported.  */
  return true;
}

#endif	/* HAVE_WINDOW_SYSTEM */

/* Return true if all the face attributes in ATTRS are supported
   on the tty frame F.

   The definition of `supported' is somewhat heuristic, but basically means
   that a face containing all the attributes in ATTRS, when merged
   with the default face for display, can be represented in a way that's

    (1) different in appearance from the default face, and
    (2) `close in spirit' to what the attributes specify, if not exact.

   Point (2) implies that a `:weight black' attribute will be satisfied
   by any terminal that can display bold, and a `:foreground "yellow"' as
   long as the terminal can display a yellowish color, but `:slant italic'
   will _not_ be satisfied by the tty display code's automatic
   substitution of a `dim' face for italic.  */

static bool
tty_supports_face_attributes_p (struct frame *f,
				Lisp_Object attrs[LFACE_VECTOR_SIZE],
				struct face *def_face)
{
  int weight, slant;
  Lisp_Object val, fg, bg;
  Emacs_Color fg_tty_color, fg_std_color;
  Emacs_Color bg_tty_color, bg_std_color;
  unsigned test_caps = 0;
  Lisp_Object *def_attrs = def_face->lface;

  /* First check some easy-to-check stuff; ttys support none of the
     following attributes, so we can just return false if any are requested
     (even if `nominal' values are specified, we should still return false,
     as that will be the same value that the default face uses).  We
     consider :slant unsupportable on ttys, even though the face code
     actually `fakes' them using a dim attribute if possible.  This is
     because the faked result is too different from what the face
     specifies.  */
  if (!UNSPECIFIEDP (attrs[LFACE_FAMILY_INDEX])
      || !UNSPECIFIEDP (attrs[LFACE_FOUNDRY_INDEX])
      || !UNSPECIFIEDP (attrs[LFACE_STIPPLE_INDEX])
      || !UNSPECIFIEDP (attrs[LFACE_HEIGHT_INDEX])
      || !UNSPECIFIEDP (attrs[LFACE_SWIDTH_INDEX])
      || !UNSPECIFIEDP (attrs[LFACE_OVERLINE_INDEX])
      || !UNSPECIFIEDP (attrs[LFACE_BOX_INDEX]))
    return false;

  /* Test for terminal `capabilities' (non-color character attributes).  */

  /* font weight (bold/dim) */
  val = attrs[LFACE_WEIGHT_INDEX];
  if (!UNSPECIFIEDP (val)
      && (weight = FONT_WEIGHT_NAME_NUMERIC (val), weight >= 0))
    {
      int def_weight = FONT_WEIGHT_NAME_NUMERIC (def_attrs[LFACE_WEIGHT_INDEX]);

      if (weight > 100)
	{
	  if (def_weight > 100)
	    return false;	/* same as default */
	  test_caps = TTY_CAP_BOLD;
	}
      else if (weight < 100)
	{
	  if (def_weight < 100)
	    return false;	/* same as default */
	  test_caps = TTY_CAP_DIM;
	}
      else if (def_weight == 100)
	return false;		/* same as default */
    }

  /* font slant */
  val = attrs[LFACE_SLANT_INDEX];
  if (!UNSPECIFIEDP (val)
      && (slant = FONT_SLANT_NAME_NUMERIC (val), slant >= 0))
    {
      int def_slant = FONT_SLANT_NAME_NUMERIC (def_attrs[LFACE_SLANT_INDEX]);
      if (slant == 100 || slant == def_slant)
	return false;		/* same as default */
      else
	test_caps |= TTY_CAP_ITALIC;
    }

  /* underlining */
  val = attrs[LFACE_UNDERLINE_INDEX];
  if (!UNSPECIFIEDP (val))
    {
      if (STRINGP (val))
	return false;		/* ttys can't use colored underlines */
      else if (EQ (CAR_SAFE (val), QCstyle) && EQ (CAR_SAFE (CDR_SAFE (val)), Qwave))
	return false;		/* ttys can't use wave underlines */
      else if (face_attr_equal_p (val, def_attrs[LFACE_UNDERLINE_INDEX]))
	return false;		/* same as default */
      else
	test_caps |= TTY_CAP_UNDERLINE;
    }

  /* inverse video */
  val = attrs[LFACE_INVERSE_INDEX];
  if (!UNSPECIFIEDP (val))
    {
      if (face_attr_equal_p (val, def_attrs[LFACE_INVERSE_INDEX]))
	return false;		/* same as default */
      else
	test_caps |= TTY_CAP_INVERSE;
    }

  /* strike through */
  val = attrs[LFACE_STRIKE_THROUGH_INDEX];
  if (!UNSPECIFIEDP (val))
    {
      if (face_attr_equal_p (val, def_attrs[LFACE_STRIKE_THROUGH_INDEX]))
	return false;		/* same as default */
      else
	test_caps |= TTY_CAP_STRIKE_THROUGH;
    }

  /* Color testing.  */

  /* Check if foreground color is close enough.  */
  fg = attrs[LFACE_FOREGROUND_INDEX];
  if (STRINGP (fg))
    {
      Lisp_Object def_fg = def_attrs[LFACE_FOREGROUND_INDEX];

      if (face_attr_equal_p (fg, def_fg))
	return false;		/* same as default */
      else if (! tty_lookup_color (f, fg, &fg_tty_color, &fg_std_color))
	return false;		/* not a valid color */
      else if (color_distance (&fg_tty_color, &fg_std_color)
	       > TTY_SAME_COLOR_THRESHOLD)
	return false;		/* displayed color is too different */
      else
	/* Make sure the color is really different from the default.  */
	{
	  Emacs_Color def_fg_color;
	  if (tty_lookup_color (f, def_fg, &def_fg_color, 0)
	      && (color_distance (&fg_tty_color, &def_fg_color)
		  <= TTY_SAME_COLOR_THRESHOLD))
	    return false;
	}
    }

  /* Check if background color is close enough.  */
  bg = attrs[LFACE_BACKGROUND_INDEX];
  if (STRINGP (bg))
    {
      Lisp_Object def_bg = def_attrs[LFACE_BACKGROUND_INDEX];

      if (face_attr_equal_p (bg, def_bg))
	return false;		/* same as default */
      else if (! tty_lookup_color (f, bg, &bg_tty_color, &bg_std_color))
	return false;		/* not a valid color */
      else if (color_distance (&bg_tty_color, &bg_std_color)
	       > TTY_SAME_COLOR_THRESHOLD)
	return false;		/* displayed color is too different */
      else
	/* Make sure the color is really different from the default.  */
	{
	  Emacs_Color def_bg_color;
	  if (tty_lookup_color (f, def_bg, &def_bg_color, 0)
	      && (color_distance (&bg_tty_color, &def_bg_color)
		  <= TTY_SAME_COLOR_THRESHOLD))
	    return false;
	}
    }

  /* If both foreground and background are requested, see if the
     distance between them is OK.  We just check to see if the distance
     between the tty's foreground and background is close enough to the
     distance between the standard foreground and background.  */
  if (STRINGP (fg) && STRINGP (bg))
    {
      int delta_delta
	= (color_distance (&fg_std_color, &bg_std_color)
	   - color_distance (&fg_tty_color, &bg_tty_color));
      if (delta_delta > TTY_SAME_COLOR_THRESHOLD
	  || delta_delta < -TTY_SAME_COLOR_THRESHOLD)
	return false;
    }


  /* See if the capabilities we selected above are supported, with the
     given colors.  */
  return tty_capable_p (FRAME_TTY (f), test_caps);
}


DEFUN ("display-supports-face-attributes-p",
       Fdisplay_supports_face_attributes_p, Sdisplay_supports_face_attributes_p,
       1, 2, 0,
       doc: /* Return non-nil if all the face attributes in ATTRIBUTES are supported.
The optional argument DISPLAY can be a display name, a frame, or
nil (meaning the selected frame's display).

For instance, to check whether the display supports underlining:

  (display-supports-face-attributes-p \\='(:underline t))

The definition of `supported' is somewhat heuristic, but basically means
that a face containing all the attributes in ATTRIBUTES, when merged
with the default face for display, can be represented in a way that's

 (1) different in appearance from the default face, and
 (2) `close in spirit' to what the attributes specify, if not exact.

Point (2) implies that a `:weight black' attribute will be satisfied by
any display that can display bold, and a `:foreground \"yellow\"' as long
as it can display a yellowish color, but `:slant italic' will _not_ be
satisfied by the tty display code's automatic substitution of a `dim'
face for italic.  */)
  (Lisp_Object attributes, Lisp_Object display)
{
  bool supports = false;
  int i;
  Lisp_Object frame;
  struct frame *f;
  struct face *def_face;
  Lisp_Object attrs[LFACE_VECTOR_SIZE];

  if (noninteractive || !initialized)
    /* We may not be able to access low-level face information in batch
       mode, or before being dumped, and this function is not going to
       be very useful in those cases anyway, so just give up.  */
    return Qnil;

  if (NILP (display))
    frame = selected_frame;
  else if (FRAMEP (display))
    frame = display;
  else
    {
      /* Find any frame on DISPLAY.  */
      Lisp_Object tail;

      frame = Qnil;
      FOR_EACH_FRAME (tail, frame)
	if (!NILP (Fequal (Fcdr (Fassq (Qdisplay,
					XFRAME (frame)->param_alist)),
			   display)))
	  break;
    }

  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);

  for (i = 0; i < LFACE_VECTOR_SIZE; i++)
    attrs[i] = Qunspecified;
  merge_face_ref (NULL, f, attributes, attrs, true, NULL, 0);

  def_face = FACE_FROM_ID_OR_NULL (f, DEFAULT_FACE_ID);
  if (def_face == NULL)
    {
      if (! realize_basic_faces (f))
	error ("Cannot realize default face");
      def_face = FACE_FROM_ID (f, DEFAULT_FACE_ID);
    }

  /* Dispatch to the appropriate handler.  */
  if (FRAME_TERMCAP_P (f) || FRAME_MSDOS_P (f))
    supports = tty_supports_face_attributes_p (f, attrs, def_face);
#ifdef HAVE_WINDOW_SYSTEM
  else
    supports = gui_supports_face_attributes_p (f, attrs, def_face);
#endif

  return supports ? Qt : Qnil;
}


/***********************************************************************
			    Font selection
 ***********************************************************************/

DEFUN ("internal-set-font-selection-order",
       Finternal_set_font_selection_order,
       Sinternal_set_font_selection_order, 1, 1, 0,
       doc: /* Set font selection order for face font selection to ORDER.
ORDER must be a list of length 4 containing the symbols `:width',
`:height', `:weight', and `:slant'.  Face attributes appearing
first in ORDER are matched first, e.g. if `:height' appears before
`:weight' in ORDER, font selection first tries to find a font with
a suitable height, and then tries to match the font weight.
Value is ORDER.  */)
  (Lisp_Object order)
{
  Lisp_Object list;
  int i;
  int indices[ARRAYELTS (font_sort_order)];

  CHECK_LIST (order);
  memset (indices, 0, sizeof indices);
  i = 0;

  for (list = order;
       CONSP (list) && i < ARRAYELTS (indices);
       list = XCDR (list), ++i)
    {
      Lisp_Object attr = XCAR (list);
      int xlfd;

      if (EQ (attr, QCwidth))
	xlfd = XLFD_SWIDTH;
      else if (EQ (attr, QCheight))
	xlfd = XLFD_POINT_SIZE;
      else if (EQ (attr, QCweight))
	xlfd = XLFD_WEIGHT;
      else if (EQ (attr, QCslant))
	xlfd = XLFD_SLANT;
      else
	break;

      if (indices[i] != 0)
	break;
      indices[i] = xlfd;
    }

  if (!NILP (list) || i != ARRAYELTS (indices))
    signal_error ("Invalid font sort order", order);
  for (i = 0; i < ARRAYELTS (font_sort_order); ++i)
    if (indices[i] == 0)
      signal_error ("Invalid font sort order", order);

  if (memcmp (indices, font_sort_order, sizeof indices) != 0)
    {
      memcpy (font_sort_order, indices, sizeof font_sort_order);
      free_all_realized_faces (Qnil);
    }

  font_update_sort_order (font_sort_order);

  return Qnil;
}


DEFUN ("internal-set-alternative-font-family-alist",
       Finternal_set_alternative_font_family_alist,
       Sinternal_set_alternative_font_family_alist, 1, 1, 0,
       doc: /* Define alternative font families to try in face font selection.
ALIST is an alist of (FAMILY ALTERNATIVE1 ALTERNATIVE2 ...) entries.
Each ALTERNATIVE is tried in order if no fonts of font family FAMILY can
be found.  Value is ALIST.  */)
  (Lisp_Object alist)
{
  Lisp_Object entry, tail, tail2;

  CHECK_LIST (alist);
  alist = Fcopy_sequence (alist);
  for (tail = alist; CONSP (tail); tail = XCDR (tail))
    {
      entry = XCAR (tail);
      CHECK_LIST (entry);
      entry = Fcopy_sequence (entry);
      XSETCAR (tail, entry);
      for (tail2 = entry; CONSP (tail2); tail2 = XCDR (tail2))
	XSETCAR (tail2, Fintern (XCAR (tail2), Qnil));
    }

  Vface_alternative_font_family_alist = alist;
  free_all_realized_faces (Qnil);
  return alist;
}


DEFUN ("internal-set-alternative-font-registry-alist",
       Finternal_set_alternative_font_registry_alist,
       Sinternal_set_alternative_font_registry_alist, 1, 1, 0,
       doc: /* Define alternative font registries to try in face font selection.
ALIST is an alist of (REGISTRY ALTERNATIVE1 ALTERNATIVE2 ...) entries.
Each ALTERNATIVE is tried in order if no fonts of font registry REGISTRY can
be found.  Value is ALIST.  */)
  (Lisp_Object alist)
{
  Lisp_Object entry, tail, tail2;

  CHECK_LIST (alist);
  alist = Fcopy_sequence (alist);
  for (tail = alist; CONSP (tail); tail = XCDR (tail))
    {
      entry = XCAR (tail);
      CHECK_LIST (entry);
      entry = Fcopy_sequence (entry);
      XSETCAR (tail, entry);
      for (tail2 = entry; CONSP (tail2); tail2 = XCDR (tail2))
	XSETCAR (tail2, Fdowncase (XCAR (tail2)));
    }
  Vface_alternative_font_registry_alist = alist;
  free_all_realized_faces (Qnil);
  return alist;
}


#ifdef HAVE_WINDOW_SYSTEM

/* Return the fontset id of the base fontset name or alias name given
   by the fontset attribute of ATTRS.  Value is -1 if the fontset
   attribute of ATTRS doesn't name a fontset.  */

static int
face_fontset (Lisp_Object attrs[LFACE_VECTOR_SIZE])
{
  Lisp_Object name;

  name = attrs[LFACE_FONTSET_INDEX];
  if (!STRINGP (name))
    return -1;
  return fs_query_fontset (name, 0);
}

#endif /* HAVE_WINDOW_SYSTEM */



/***********************************************************************
			   Face Realization
 ***********************************************************************/

/* Realize basic faces on frame F.  Value is zero if frame parameters
   of F don't contain enough information needed to realize the default
   face.  */

static bool
realize_basic_faces (struct frame *f)
{
  bool success_p = false;

  /* Block input here so that we won't be surprised by an X expose
     event, for instance, without having the faces set up.  */
  block_input ();

  if (realize_default_face (f))
    {
      realize_named_face (f, Qmode_line_active, MODE_LINE_ACTIVE_FACE_ID);
      realize_named_face (f, Qmode_line_inactive, MODE_LINE_INACTIVE_FACE_ID);
      realize_named_face (f, Qtool_bar, TOOL_BAR_FACE_ID);
      realize_named_face (f, Qfringe, FRINGE_FACE_ID);
      realize_named_face (f, Qheader_line, HEADER_LINE_FACE_ID);
      realize_named_face (f, Qscroll_bar, SCROLL_BAR_FACE_ID);
      realize_named_face (f, Qborder, BORDER_FACE_ID);
      realize_named_face (f, Qcursor, CURSOR_FACE_ID);
      realize_named_face (f, Qmouse, MOUSE_FACE_ID);
      realize_named_face (f, Qmenu, MENU_FACE_ID);
      realize_named_face (f, Qvertical_border, VERTICAL_BORDER_FACE_ID);
      realize_named_face (f, Qwindow_divider, WINDOW_DIVIDER_FACE_ID);
      realize_named_face (f, Qwindow_divider_first_pixel,
			  WINDOW_DIVIDER_FIRST_PIXEL_FACE_ID);
      realize_named_face (f, Qwindow_divider_last_pixel,
			  WINDOW_DIVIDER_LAST_PIXEL_FACE_ID);
      realize_named_face (f, Qinternal_border, INTERNAL_BORDER_FACE_ID);
      realize_named_face (f, Qchild_frame_border, CHILD_FRAME_BORDER_FACE_ID);
      realize_named_face (f, Qtab_bar, TAB_BAR_FACE_ID);
      realize_named_face (f, Qtab_line, TAB_LINE_FACE_ID);

      /* Reflect changes in the `menu' face in menu bars.  */
      if (FRAME_FACE_CACHE (f)->menu_face_changed_p)
	{
	  FRAME_FACE_CACHE (f)->menu_face_changed_p = false;
#ifdef USE_X_TOOLKIT
	  if (FRAME_WINDOW_P (f))
	    x_update_menu_appearance (f);
#endif
	}

      success_p = true;
    }

  unblock_input ();
  return success_p;
}


/* Realize the default face on frame F.  If the face is not fully
   specified, make it fully-specified.  Attributes of the default face
   that are not explicitly specified are taken from frame parameters.  */

static bool
realize_default_face (struct frame *f)
{
  struct face_cache *c = FRAME_FACE_CACHE (f);
  Lisp_Object lface;
  Lisp_Object attrs[LFACE_VECTOR_SIZE];

  /* If the `default' face is not yet known, create it.  */
  lface = lface_from_face_name (f, Qdefault, false);
  if (NILP (lface))
    {
       Lisp_Object frame;
       XSETFRAME (frame, f);
       lface = Finternal_make_lisp_face (Qdefault, frame);
    }

#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    {
      Lisp_Object font_object;

      XSETFONT (font_object, FRAME_FONT (f));
      set_lface_from_font (f, lface, font_object, f->default_face_done_p);
      ASET (lface, LFACE_FONTSET_INDEX, fontset_name (FRAME_FONTSET (f)));
      f->default_face_done_p = true;
    }
#endif /* HAVE_WINDOW_SYSTEM */

  if (!FRAME_WINDOW_P (f))
    {
      ASET (lface, LFACE_FAMILY_INDEX, build_string ("default"));
      ASET (lface, LFACE_FOUNDRY_INDEX, LFACE_FAMILY (lface));
      ASET (lface, LFACE_SWIDTH_INDEX, Qnormal);
      ASET (lface, LFACE_HEIGHT_INDEX, make_fixnum (1));
      if (UNSPECIFIEDP (LFACE_WEIGHT (lface)))
	ASET (lface, LFACE_WEIGHT_INDEX, Qnormal);
      if (UNSPECIFIEDP (LFACE_SLANT (lface)))
	ASET (lface, LFACE_SLANT_INDEX, Qnormal);
      if (UNSPECIFIEDP (LFACE_FONTSET (lface)))
	ASET (lface, LFACE_FONTSET_INDEX, Qnil);
    }

  if (UNSPECIFIEDP (LFACE_EXTEND (lface)))
    ASET (lface, LFACE_EXTEND_INDEX, Qnil);

  if (UNSPECIFIEDP (LFACE_UNDERLINE (lface)))
    ASET (lface, LFACE_UNDERLINE_INDEX, Qnil);

  if (UNSPECIFIEDP (LFACE_OVERLINE (lface)))
    ASET (lface, LFACE_OVERLINE_INDEX, Qnil);

  if (UNSPECIFIEDP (LFACE_STRIKE_THROUGH (lface)))
    ASET (lface, LFACE_STRIKE_THROUGH_INDEX, Qnil);

  if (UNSPECIFIEDP (LFACE_BOX (lface)))
    ASET (lface, LFACE_BOX_INDEX, Qnil);

  if (UNSPECIFIEDP (LFACE_INVERSE (lface)))
    ASET (lface, LFACE_INVERSE_INDEX, Qnil);

  if (UNSPECIFIEDP (LFACE_FOREGROUND (lface)))
    {
      /* This function is called so early that colors are not yet
	 set in the frame parameter list.  */
      Lisp_Object color = Fassq (Qforeground_color, f->param_alist);

      if (CONSP (color) && STRINGP (XCDR (color)))
	ASET (lface, LFACE_FOREGROUND_INDEX, XCDR (color));
      else if (FRAME_WINDOW_P (f))
	return false;
      else if (FRAME_INITIAL_P (f) || FRAME_TERMCAP_P (f) || FRAME_MSDOS_P (f))
	ASET (lface, LFACE_FOREGROUND_INDEX, build_string (unspecified_fg));
      else
	emacs_abort ();
    }

  if (UNSPECIFIEDP (LFACE_BACKGROUND (lface)))
    {
      /* This function is called so early that colors are not yet
	 set in the frame parameter list.  */
      Lisp_Object color = Fassq (Qbackground_color, f->param_alist);
      if (CONSP (color) && STRINGP (XCDR (color)))
	ASET (lface, LFACE_BACKGROUND_INDEX, XCDR (color));
      else if (FRAME_WINDOW_P (f))
	return false;
      else if (FRAME_INITIAL_P (f) || FRAME_TERMCAP_P (f) || FRAME_MSDOS_P (f))
	ASET (lface, LFACE_BACKGROUND_INDEX, build_string (unspecified_bg));
      else
	emacs_abort ();
    }

  if (UNSPECIFIEDP (LFACE_STIPPLE (lface)))
    ASET (lface, LFACE_STIPPLE_INDEX, Qnil);

  /* Realize the face; it must be fully-specified now.  */
  eassert (lface_fully_specified_p (XVECTOR (lface)->contents));
  check_lface (lface);
  memcpy (attrs, xvector_contents (lface), sizeof attrs);
  struct face *face = realize_face (c, attrs, DEFAULT_FACE_ID);

#ifndef HAVE_WINDOW_SYSTEM
  (void) face;
#else
  if (FRAME_X_P (f) && face->font != FRAME_FONT (f))
    {
      /* This can happen when making a frame on a display that does
	 not support the default font.  */
      if (!face->font)
	return false;

      /* Otherwise, the font specified for the frame was not
	 acceptable as a font for the default face (perhaps because
	 auto-scaled fonts are rejected), so we must adjust the frame
	 font.  */
      gui_set_font (f, LFACE_FONT (lface), Qnil);
    }
#endif
  return true;
}


/* Realize basic faces other than the default face in face cache C.
   SYMBOL is the face name, ID is the face id the realized face must
   have.  The default face must have been realized already.  */

static void
realize_named_face (struct frame *f, Lisp_Object symbol, int id)
{
  struct face_cache *c = FRAME_FACE_CACHE (f);
  Lisp_Object lface = lface_from_face_name (f, symbol, false);
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  Lisp_Object symbol_attrs[LFACE_VECTOR_SIZE];

  /* The default face must exist and be fully specified.  */
  get_lface_attributes_no_remap (f, Qdefault, attrs, true);
  check_lface_attrs (attrs);
  eassert (lface_fully_specified_p (attrs));

  /* If SYMBOL isn't know as a face, create it.  */
  if (NILP (lface))
    {
      Lisp_Object frame;
      XSETFRAME (frame, f);
      lface = Finternal_make_lisp_face (symbol, frame);
    }


  get_lface_attributes_no_remap (f, symbol, symbol_attrs, true);

  /* Handle the 'reset' pseudo-value of any attribute by replacing it
     with the corresponding value of the default face.  */
  int i;
  for (i = 1; i < LFACE_VECTOR_SIZE; i++)
    if (EQ (symbol_attrs[i], Qreset))
      symbol_attrs[i] = attrs[i];
  /* Merge SYMBOL's face with the default face.  */
  merge_face_vectors (NULL, f, symbol_attrs, attrs, 0);

  /* Realize the face.  */
  realize_face (c, attrs, id);
}


/* Realize the fully-specified face with attributes ATTRS in face
   cache CACHE for ASCII characters.  If FORMER_FACE_ID is
   non-negative, it is an ID of face to remove before caching the new
   face.  Value is a pointer to the newly created realized face.  */

static struct face *
realize_face (struct face_cache *cache, Lisp_Object attrs[LFACE_VECTOR_SIZE],
	      int former_face_id)
{
  struct face *face;

  /* LFACE must be fully specified.  */
  eassert (cache != NULL);
  check_lface_attrs (attrs);

  if (former_face_id >= 0 && cache->used > former_face_id)
    {
      /* Remove the former face.  */
      struct face *former_face = cache->faces_by_id[former_face_id];
      uncache_face (cache, former_face);
      free_realized_face (cache->f, former_face);
      SET_FRAME_GARBAGED (cache->f);
    }

  if (FRAME_WINDOW_P (cache->f))
    face = realize_gui_face (cache, attrs);
  else if (FRAME_TERMCAP_P (cache->f) || FRAME_MSDOS_P (cache->f))
    face = realize_tty_face (cache, attrs);
  else if (FRAME_INITIAL_P (cache->f))
    {
      /* Create a dummy face. */
      face = make_realized_face (attrs);
    }
  else
    emacs_abort ();

  /* Insert the new face.  */
  cache_face (cache, face, lface_hash (attrs));
  return face;
}


#ifdef HAVE_WINDOW_SYSTEM
/* Realize the fully-specified face that uses FONT-OBJECT and has the
   same attributes as BASE_FACE except for the font on frame F.
   FONT-OBJECT may be nil, in which case, realized a face of
   no-font.  */

static struct face *
realize_non_ascii_face (struct frame *f, Lisp_Object font_object,
			struct face *base_face)
{
  struct face_cache *cache = FRAME_FACE_CACHE (f);
  struct face *face;

  face = xmalloc (sizeof *face);
  *face = *base_face;
  face->gc = 0;
  face->overstrike
    = (! NILP (font_object)
       && FONT_WEIGHT_NAME_NUMERIC (face->lface[LFACE_WEIGHT_INDEX]) > 100
       && FONT_WEIGHT_NUMERIC (font_object) <= 100);

  /* Don't try to free the colors copied bitwise from BASE_FACE.  */
  face->colors_copied_bitwise_p = true;
  face->font = NILP (font_object) ? NULL : XFONT_OBJECT (font_object);
  face->gc = 0;

  cache_face (cache, face, face->hash);

  return face;
}

/* Remove the attribute at INDEX from the font object if SYMBOL
   appears in `font-fallback-ignored-attributes'.  */

static void
font_maybe_unset_attribute (Lisp_Object font_object,
			    enum font_property_index index, Lisp_Object symbol)
{
  Lisp_Object tail = Vface_font_lax_matched_attributes;

  eassert (CONSP (tail));

  FOR_EACH_TAIL_SAFE (tail)
    {
      if (EQ (XCAR (tail), symbol))
	ASET (font_object, index, Qnil);
    }
}
#endif /* HAVE_WINDOW_SYSTEM */

/* Realize the fully-specified face with attributes ATTRS in face
   cache CACHE for ASCII characters.  Do it for GUI frame CACHE->f.
   If the new face doesn't share font with the default face, a
   fontname is allocated from the heap and set in `font_name' of the
   new face, but it is not yet loaded here.  Value is a pointer to the
   newly created realized face.  */

static struct face *
realize_gui_face (struct face_cache *cache, Lisp_Object attrs[LFACE_VECTOR_SIZE])
{
  struct face *face = NULL;
#ifdef HAVE_WINDOW_SYSTEM
  struct face *default_face;
  struct frame *f;
  Lisp_Object stipple, underline, overline, strike_through, box;

  eassert (FRAME_WINDOW_P (cache->f));

  /* Allocate a new realized face.  */
  face = make_realized_face (attrs);
  face->ascii_face = face;

  f = cache->f;

  /* Determine the font to use.  Most of the time, the font will be
     the same as the font of the default face, so try that first.  */
  default_face = FACE_FROM_ID_OR_NULL (f, DEFAULT_FACE_ID);
  if (default_face
      && lface_same_font_attributes_p (default_face->lface, attrs))
    {
      face->font = default_face->font;
      face->fontset
	= make_fontset_for_ascii_face (f, default_face->fontset, face);
    }
  else
    {
      /* If the face attribute ATTRS specifies a fontset, use it as
	 the base of a new realized fontset.  Otherwise, use the same
	 base fontset as of the default face.  The base determines
	 registry and encoding of a font.  It may also determine
	 foundry and family.  The other fields of font name pattern
	 are constructed from ATTRS.  */
      int fontset = face_fontset (attrs);

      /* If we are realizing the default face, ATTRS should specify a
	 fontset.  In other words, if FONTSET is -1, we are not
	 realizing the default face, thus the default face should have
	 already been realized.  */
      if (fontset == -1)
	{
	  if (default_face)
	    fontset = default_face->fontset;
	  if (fontset == -1)
	    emacs_abort ();
	}
      if (! FONT_OBJECT_P (attrs[LFACE_FONT_INDEX]))
	{
	  Lisp_Object spec = copy_font_spec (attrs[LFACE_FONT_INDEX]);

	  /* Maybe unset several values in SPEC, usually the width,
	     slant, and weight.  The best possible values for these
	     attributes are determined in font_find_for_lface, called
	     by font_load_for_lface, when the list of candidate fonts
	     returned by font_list_entities is sorted by font_select_entity
	     (which calls font_sort_entities, which calls font_score).
	     If these attributes are not unset here, the candidate
	     font list returned by font_list_entities only contains
	     fonts that are exact matches for these weight, slant, and
	     width attributes, which could lead to suboptimal or wrong
	     font selection.  (bug#5934) */
	  if (EQ (Vface_font_lax_matched_attributes, Qt))
	    {
	      /* The default case: clear the font attributes that
		 affect its appearance the least, to try to find some
		 font that is close, if not exact, match.  */
	      ASET (spec, FONT_WEIGHT_INDEX, Qnil);
	      ASET (spec, FONT_SLANT_INDEX, Qnil);
	      ASET (spec, FONT_WIDTH_INDEX, Qnil);
	    }
	  else if (!NILP (Vface_font_lax_matched_attributes))
	    {
	      /* Also allow unsetting specific attributes for
		 debugging purposes.  */
	      font_maybe_unset_attribute (spec, FONT_WEIGHT_INDEX, QCweight);
	      font_maybe_unset_attribute (spec, FONT_SLANT_INDEX, QCslant);
	      font_maybe_unset_attribute (spec, FONT_WIDTH_INDEX, QCwidth);
	      font_maybe_unset_attribute (spec, FONT_FAMILY_INDEX, QCfamily);
	      font_maybe_unset_attribute (spec, FONT_FOUNDRY_INDEX, QCfoundry);
	      font_maybe_unset_attribute (spec, FONT_REGISTRY_INDEX, QCregistry);
	      font_maybe_unset_attribute (spec, FONT_ADSTYLE_INDEX, QCadstyle);
	      font_maybe_unset_attribute (spec, FONT_SIZE_INDEX, QCsize);
	      font_maybe_unset_attribute (spec, FONT_DPI_INDEX, QCdpi);
	      font_maybe_unset_attribute (spec, FONT_SPACING_INDEX, QCspacing);
	      font_maybe_unset_attribute (spec, FONT_AVGWIDTH_INDEX, QCavgwidth);
	    }

	  attrs[LFACE_FONT_INDEX] = font_load_for_lface (f, attrs, spec);
	}
      if (FONT_OBJECT_P (attrs[LFACE_FONT_INDEX]))
	{
	  face->font = XFONT_OBJECT (attrs[LFACE_FONT_INDEX]);
	  face->fontset = make_fontset_for_ascii_face (f, fontset, face);
	}
      else
	{
	  face->font = NULL;
	  face->fontset = -1;
	}
    }

  if (face->font
      && FONT_WEIGHT_NAME_NUMERIC (attrs[LFACE_WEIGHT_INDEX]) > 100
      && FONT_WEIGHT_NUMERIC (attrs[LFACE_FONT_INDEX]) <= 100)
    face->overstrike = true;

  /* Load colors, and set remaining attributes.  */

  load_face_colors (f, face, attrs);

  /* Set up box.  */
  box = attrs[LFACE_BOX_INDEX];
  if (STRINGP (box))
    {
      /* A simple box of line width 1 drawn in color given by
	 the string.  */
      face->box_color = load_color (f, face, attrs[LFACE_BOX_INDEX],
				    LFACE_BOX_INDEX);
      face->box = FACE_SIMPLE_BOX;
      face->box_vertical_line_width = face->box_horizontal_line_width = 1;
    }
  else if (FIXNUMP (box))
    {
      /* Simple box of specified line width in foreground color of the
	 face.  */
      eassert (XFIXNUM (box) != 0);
      face->box = FACE_SIMPLE_BOX;
      face->box_vertical_line_width = eabs(XFIXNUM (box));
      face->box_horizontal_line_width = XFIXNUM (box);
      face->box_color = face->foreground;
      face->box_color_defaulted_p = true;
    }
  else if (CONSP (box) && FIXNUMP (XCAR (box)) && FIXNUMP (XCDR (box)))
    {
	/* `(VWIDTH . HWIDTH)'.  */
      face->box = FACE_SIMPLE_BOX;
      face->box_color = face->foreground;
      face->box_color_defaulted_p = true;
      face->box_vertical_line_width = XFIXNUM (XCAR (box));
      face->box_horizontal_line_width = XFIXNUM (XCDR (box));
    }
  else if (CONSP (box))
    {
      bool set_color = false;

      /* `(:width WIDTH :color COLOR :shadow SHADOW)'.  SHADOW
	 being one of `raised' or `sunken'.  */
      face->box = FACE_SIMPLE_BOX;
      face->box_color = face->foreground;
      face->box_color_defaulted_p = true;
      face->box_vertical_line_width = face->box_horizontal_line_width = 1;

      while (CONSP (box))
	{
	  Lisp_Object keyword, value;

	  keyword = XCAR (box);
	  box = XCDR (box);

	  if (!CONSP (box))
	    break;
	  value = XCAR (box);
	  box = XCDR (box);

	  if (EQ (keyword, QCline_width))
	    {
	      if (CONSP (value) && FIXNUMP (XCAR (value)) && FIXNUMP (XCDR (value))) {
		  face->box_vertical_line_width = XFIXNUM (XCAR (value));
		  face->box_horizontal_line_width = XFIXNUM (XCDR (value));
	      }
	      else if (FIXNUMP (value) && XFIXNUM (value) != 0) {
		face->box_vertical_line_width = eabs (XFIXNUM (value));
		face->box_horizontal_line_width = XFIXNUM (value);
	      }
	    }
	  else if (EQ (keyword, QCcolor))
	    {
	      if (STRINGP (value))
		{
		  face->box_color = load_color (f, face, value,
						LFACE_BOX_INDEX);
		  face->use_box_color_for_shadows_p = true;
		  set_color = true;
		}
	    }
	  else if (EQ (keyword, QCstyle))
	    {
	      if (EQ (value, Qreleased_button))
		face->box = FACE_RAISED_BOX;
	      else if (EQ (value, Qpressed_button))
		face->box = FACE_SUNKEN_BOX;
	      else if (EQ (value, Qflat_button))
		{
		  face->box = FACE_SIMPLE_BOX;
		  /* Don't override colors set in this box. */
		  if (!set_color)
		    face->box_color = face->background;
		}
	    }
	}
    }

  /* Text underline, overline, strike-through.  */

  underline = attrs[LFACE_UNDERLINE_INDEX];
  if (EQ (underline, Qt))
    {
      /* Use default color (same as foreground color).  */
      face->underline = FACE_UNDER_LINE;
      face->underline_defaulted_p = true;
      face->underline_color = 0;
      face->underline_at_descent_line_p = false;
      face->underline_pixels_above_descent_line = 0;
    }
  else if (STRINGP (underline))
    {
      /* Use specified color.  */
      face->underline = FACE_UNDER_LINE;
      face->underline_defaulted_p = false;
      face->underline_color
	= load_color (f, face, underline,
		      LFACE_UNDERLINE_INDEX);
      face->underline_at_descent_line_p = false;
      face->underline_pixels_above_descent_line = 0;
    }
  else if (NILP (underline))
    {
      face->underline = FACE_NO_UNDERLINE;
      face->underline_defaulted_p = false;
      face->underline_color = 0;
      face->underline_at_descent_line_p = false;
      face->underline_pixels_above_descent_line = 0;
    }
  else if (CONSP (underline))
    {
      /* `(:color COLOR :style STYLE)'.
         STYLE being one of `line' or `wave'. */
      face->underline = FACE_UNDER_LINE;
      face->underline_color = 0;
      face->underline_defaulted_p = true;
      face->underline_at_descent_line_p = false;
      face->underline_pixels_above_descent_line = 0;

      /* FIXME?  This is also not robust about checking the precise form.
         See comments in Finternal_set_lisp_face_attribute.  */
      while (CONSP (underline))
        {
          Lisp_Object keyword, value;

          keyword = XCAR (underline);
          underline = XCDR (underline);

          if (!CONSP (underline))
            break;
          value = XCAR (underline);
          underline = XCDR (underline);

          if (EQ (keyword, QCcolor))
            {
              if (EQ (value, Qforeground_color))
                {
                  face->underline_defaulted_p = true;
                  face->underline_color = 0;
                }
              else if (STRINGP (value))
                {
                  face->underline_defaulted_p = false;
                  face->underline_color = load_color (f, face, value,
                                                      LFACE_UNDERLINE_INDEX);
                }
            }
          else if (EQ (keyword, QCstyle))
            {
              if (EQ (value, Qline))
                face->underline = FACE_UNDER_LINE;
              else if (EQ (value, Qwave))
                face->underline = FACE_UNDER_WAVE;
            }
	  else if (EQ (keyword, QCposition))
	    {
	      face->underline_at_descent_line_p = !NILP (value);

	      if (FIXNATP (value))
		face->underline_pixels_above_descent_line = XFIXNAT (value);
	    }
        }
    }

  overline = attrs[LFACE_OVERLINE_INDEX];
  if (STRINGP (overline))
    {
      face->overline_color
	= load_color (f, face, attrs[LFACE_OVERLINE_INDEX],
		      LFACE_OVERLINE_INDEX);
      face->overline_p = true;
    }
  else if (EQ (overline, Qt))
    {
      face->overline_color = face->foreground;
      face->overline_color_defaulted_p = true;
      face->overline_p = true;
    }

  strike_through = attrs[LFACE_STRIKE_THROUGH_INDEX];
  if (STRINGP (strike_through))
    {
      face->strike_through_color
	= load_color (f, face, attrs[LFACE_STRIKE_THROUGH_INDEX],
		      LFACE_STRIKE_THROUGH_INDEX);
      face->strike_through_p = true;
    }
  else if (EQ (strike_through, Qt))
    {
      face->strike_through_color = face->foreground;
      face->strike_through_color_defaulted_p = true;
      face->strike_through_p = true;
    }

  stipple = attrs[LFACE_STIPPLE_INDEX];
  if (!NILP (stipple))
    face->stipple = load_pixmap (f, stipple);
#endif /* HAVE_WINDOW_SYSTEM */

  return face;
}


/* Map a specified color of face FACE on frame F to a tty color index.
   IDX is either LFACE_FOREGROUND_INDEX or LFACE_BACKGROUND_INDEX, and
   specifies which color to map.  Set *DEFAULTED to true if mapping to the
   default foreground/background colors.  */

static void
map_tty_color (struct frame *f, struct face *face,
	       enum lface_attribute_index idx, bool *defaulted)
{
  Lisp_Object frame, color, def;
  bool foreground_p = idx == LFACE_FOREGROUND_INDEX;
  unsigned long default_pixel =
    foreground_p ? FACE_TTY_DEFAULT_FG_COLOR : FACE_TTY_DEFAULT_BG_COLOR;
  unsigned long pixel = default_pixel;
#ifdef MSDOS
  unsigned long default_other_pixel =
    foreground_p ? FACE_TTY_DEFAULT_BG_COLOR : FACE_TTY_DEFAULT_FG_COLOR;
#endif

  eassert (idx == LFACE_FOREGROUND_INDEX || idx == LFACE_BACKGROUND_INDEX);

  XSETFRAME (frame, f);
  color = face->lface[idx];

  if (STRINGP (color)
      && SCHARS (color)
      && CONSP (Vtty_defined_color_alist)
      && (def = assoc_no_quit (color, call1 (Qtty_color_alist, frame)),
	  CONSP (def)))
    {
      /* Associations in tty-defined-color-alist are of the form
	 (NAME INDEX R G B).  We need the INDEX part.  */
      pixel = XFIXNUM (XCAR (XCDR (def)));
    }

  if (pixel == default_pixel && STRINGP (color))
    {
      pixel = load_color (f, face, color, idx);

#ifdef MSDOS
      /* If the foreground of the default face is the default color,
	 use the foreground color defined by the frame.  */
      if (FRAME_MSDOS_P (f))
	{
	  if (pixel == default_pixel
	      || pixel == FACE_TTY_DEFAULT_COLOR)
	    {
	      if (foreground_p)
		pixel = FRAME_FOREGROUND_PIXEL (f);
	      else
		pixel = FRAME_BACKGROUND_PIXEL (f);
	      face->lface[idx] = tty_color_name (f, pixel);
	      *defaulted = true;
	    }
	  else if (pixel == default_other_pixel)
	    {
	      if (foreground_p)
		pixel = FRAME_BACKGROUND_PIXEL (f);
	      else
		pixel = FRAME_FOREGROUND_PIXEL (f);
	      face->lface[idx] = tty_color_name (f, pixel);
	      *defaulted = true;
	    }
	}
#endif /* MSDOS */
    }

  if (foreground_p)
    face->foreground = pixel;
  else
    face->background = pixel;
}


/* Realize the fully-specified face with attributes ATTRS in face
   cache CACHE for ASCII characters.  Do it for TTY frame CACHE->f.
   Value is a pointer to the newly created realized face.  */

static struct face *
realize_tty_face (struct face_cache *cache,
		  Lisp_Object attrs[LFACE_VECTOR_SIZE])
{
  struct face *face;
  int weight, slant;
  bool face_colors_defaulted = false;
  struct frame *f = cache->f;

  /* Frame must be a termcap frame.  */
  eassert (FRAME_TERMCAP_P (cache->f) || FRAME_MSDOS_P (cache->f));

  /* Allocate a new realized face.  */
  face = make_realized_face (attrs);
#if false
  face->font_name = FRAME_MSDOS_P (cache->f) ? "ms-dos" : "tty";
#endif

  /* Map face attributes to TTY appearances.  */
  weight = FONT_WEIGHT_NAME_NUMERIC (attrs[LFACE_WEIGHT_INDEX]);
  slant = FONT_SLANT_NAME_NUMERIC (attrs[LFACE_SLANT_INDEX]);
  if (weight > 100)
    face->tty_bold_p = true;
  if (slant != 100)
    face->tty_italic_p = true;
  if (!NILP (attrs[LFACE_UNDERLINE_INDEX]))
    face->tty_underline_p = true;
  if (!NILP (attrs[LFACE_INVERSE_INDEX]))
    face->tty_reverse_p = true;
  if (!NILP (attrs[LFACE_STRIKE_THROUGH_INDEX]))
    face->tty_strike_through_p = true;

  /* Map color names to color indices.  */
  map_tty_color (f, face, LFACE_FOREGROUND_INDEX, &face_colors_defaulted);
  map_tty_color (f, face, LFACE_BACKGROUND_INDEX, &face_colors_defaulted);

  /* Swap colors if face is inverse-video.  If the colors are taken
     from the frame colors, they are already inverted, since the
     frame-creation function calls x-handle-reverse-video.  */
  if (face->tty_reverse_p && !face_colors_defaulted)
    {
      unsigned long tem = face->foreground;
      face->foreground = face->background;
      face->background = tem;
    }

  if (tty_suppress_bold_inverse_default_colors_p
      && face->tty_bold_p
      && face->background == FACE_TTY_DEFAULT_FG_COLOR
      && face->foreground == FACE_TTY_DEFAULT_BG_COLOR)
    face->tty_bold_p = false;

  return face;
}


DEFUN ("tty-suppress-bold-inverse-default-colors",
       Ftty_suppress_bold_inverse_default_colors,
       Stty_suppress_bold_inverse_default_colors, 1, 1, 0,
       doc: /* Suppress/allow boldness of faces with inverse default colors.
SUPPRESS non-nil means suppress it.
This affects bold faces on TTYs whose foreground is the default background
color of the display and whose background is the default foreground color.
For such faces, the bold face attribute is ignored if this variable
is non-nil.  */)
  (Lisp_Object suppress)
{
  tty_suppress_bold_inverse_default_colors_p = !NILP (suppress);
  face_change = true;
  return suppress;
}



/***********************************************************************
			   Computing Faces
 ***********************************************************************/

/* Return the ID of the face to use to display character CH with face
   property PROP on frame F in current_buffer.  */

int
compute_char_face (struct frame *f, int ch, Lisp_Object prop)
{
  int face_id;

  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    ch = 0;

  if (NILP (prop))
    {
      struct face *face = FACE_FROM_ID (f, DEFAULT_FACE_ID);
      face_id = FACE_FOR_CHAR (f, face, ch, -1, Qnil);
    }
  else
    {
      Lisp_Object attrs[LFACE_VECTOR_SIZE];
      struct face *default_face = FACE_FROM_ID (f, DEFAULT_FACE_ID);
      memcpy (attrs, default_face->lface, sizeof attrs);
      merge_face_ref (NULL, f, prop, attrs, true, NULL, 0);
      face_id = lookup_face (f, attrs);
    }

  return face_id;
}

/* Return the face ID associated with buffer position POS for
   displaying ASCII characters.  Return in *ENDPTR the position at
   which a different face is needed, as far as text properties and
   overlays are concerned.  W is a window displaying current_buffer.

   ATTR_FILTER is passed merge_face_ref.

   REGION_BEG, REGION_END delimit the region, so it can be
   highlighted.

   LIMIT is a position not to scan beyond.  That is to limit the time
   this function can take.

   If MOUSE, use the character's mouse-face, not its face, and only
   consider the highest-priority source of mouse-face at POS,
   i.e. don't merge different mouse-face values if more than one
   source specifies it.

   BASE_FACE_ID, if non-negative, specifies a base face id to use
   instead of DEFAULT_FACE_ID.

   The face returned is suitable for displaying ASCII characters.  */

int
face_at_buffer_position (struct window *w, ptrdiff_t pos,
			 ptrdiff_t *endptr, ptrdiff_t limit,
                         bool mouse, int base_face_id,
                         enum lface_attribute_index attr_filter)
{
  struct frame *f = XFRAME (w->frame);
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  Lisp_Object prop, position;
  ptrdiff_t i, noverlays;
  Lisp_Object *overlay_vec;
  ptrdiff_t endpos;
  Lisp_Object propname = mouse ? Qmouse_face : Qface;
  Lisp_Object limit1, end;
  struct face *default_face;

  /* W must display the current buffer.  We could write this function
     to use the frame and buffer of W, but right now it doesn't.  */
  /* eassert (XBUFFER (w->contents) == current_buffer); */

  XSETFASTINT (position, pos);

  endpos = ZV;

  /* Get the `face' or `mouse_face' text property at POS, and
     determine the next position at which the property changes.  */
  prop = Fget_text_property (position, propname, w->contents);
  XSETFASTINT (limit1, (limit < endpos ? limit : endpos));
  end = Fnext_single_property_change (position, propname, w->contents, limit1);
  if (FIXNUMP (end))
    endpos = XFIXNUM (end);

  /* Look at properties from overlays.  */
  USE_SAFE_ALLOCA;
  {
    ptrdiff_t next_overlay;
    GET_OVERLAYS_AT (pos, overlay_vec, noverlays, &next_overlay);
    if (next_overlay < endpos)
      endpos = next_overlay;
  }

  *endptr = endpos;

  {
    int face_id;

    if (base_face_id >= 0)
      face_id = base_face_id;
    else if (NILP (Vface_remapping_alist))
      face_id = DEFAULT_FACE_ID;
    else
      face_id = lookup_basic_face (w, f, DEFAULT_FACE_ID);

    default_face = FACE_FROM_ID_OR_NULL (f, face_id);
    /* Make sure the default face ID is usable: if someone freed the
       cached faces since we've looked up these faces, we need to look
       them up again.  */
    if (!default_face)
      {
	if (FRAME_FACE_CACHE (f)->used == 0)
	  recompute_basic_faces (f);
	default_face = FACE_FROM_ID (f,
				     lookup_basic_face (w, f, DEFAULT_FACE_ID));
      }
  }

  /* Optimize common cases where we can use the default face.  */
  if (noverlays == 0
      && NILP (prop))
    {
      SAFE_FREE ();
      return default_face->id;
    }

  /* Begin with attributes from the default face.  */
  memcpy (attrs, default_face->lface, sizeof(attrs));

  /* Merge in attributes specified via text properties.  */
  if (!NILP (prop))
    merge_face_ref (w, f, prop, attrs, true, NULL, attr_filter);

  /* Now merge the overlay data.  */
  noverlays = sort_overlays (overlay_vec, noverlays, w);
  /* For mouse-face, we need only the single highest-priority face
     from the overlays, if any.  */
  if (mouse)
    {
      for (prop = Qnil, i = noverlays - 1; i >= 0 && NILP (prop); --i)
	{
	  ptrdiff_t oendpos;

	  prop = Foverlay_get (overlay_vec[i], propname);
	  if (!NILP (prop))
	    {
	      /* Overlays always take priority over text properties,
		 so discard the mouse-face text property, if any, and
		 use the overlay property instead.  */
	      memcpy (attrs, default_face->lface, sizeof attrs);
	      merge_face_ref (w, f, prop, attrs, true, NULL, attr_filter);
	    }

	  oendpos = OVERLAY_END (overlay_vec[i]);
	  if (oendpos < endpos)
	    endpos = oendpos;
	}
    }
  else
    {
      for (i = 0; i < noverlays; i++)
	{
	  ptrdiff_t oendpos;

	  prop = Foverlay_get (overlay_vec[i], propname);

	  if (!NILP (prop))
	    merge_face_ref (w, f, prop, attrs, true, NULL, attr_filter);

          oendpos = OVERLAY_END (overlay_vec[i]);
          if (oendpos < endpos)
            endpos = oendpos;
        }
    }

  *endptr = endpos;

  SAFE_FREE ();

  /* Look up a realized face with the given face attributes,
     or realize a new one for ASCII characters.  */
  return lookup_face (f, attrs);
}

/* Return the face ID at buffer position POS for displaying ASCII
   characters associated with overlay strings for overlay OVERLAY.

   Like face_at_buffer_position except for OVERLAY.  Currently it
   simply disregards the `face' properties of all overlays.  */

int
face_for_overlay_string (struct window *w, ptrdiff_t pos,
			 ptrdiff_t *endptr, ptrdiff_t limit,
			 bool mouse, Lisp_Object overlay,
			 enum lface_attribute_index attr_filter)
{
  struct frame *f = XFRAME (w->frame);
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  Lisp_Object prop, position;
  ptrdiff_t endpos;
  Lisp_Object propname = mouse ? Qmouse_face : Qface;
  Lisp_Object limit1, end;
  struct face *default_face;

  /* W must display the current buffer.  We could write this function
     to use the frame and buffer of W, but right now it doesn't.  */
  /* eassert (XBUFFER (w->contents) == current_buffer); */

  XSETFASTINT (position, pos);

  endpos = ZV;

  /* Get the `face' or `mouse_face' text property at POS, and
     determine the next position at which the property changes.  */
  prop = Fget_text_property (position, propname, w->contents);
  XSETFASTINT (limit1, (limit < endpos ? limit : endpos));
  end = Fnext_single_property_change (position, propname, w->contents, limit1);
  if (FIXNUMP (end))
    endpos = XFIXNUM (end);

  *endptr = endpos;

  /* Optimize common case where we can use the default face.  */
  if (NILP (prop)
      && NILP (Vface_remapping_alist))
    return DEFAULT_FACE_ID;

  /* Begin with attributes from the default face.  */
  default_face = FACE_FROM_ID (f, lookup_basic_face (w, f, DEFAULT_FACE_ID));
  memcpy (attrs, default_face->lface, sizeof attrs);

  /* Merge in attributes specified via text properties.  */
  if (!NILP (prop))
    merge_face_ref (w, f, prop, attrs, true, NULL, attr_filter);

  *endptr = endpos;

  /* Look up a realized face with the given face attributes,
     or realize a new one for ASCII characters.  */
  return lookup_face (f, attrs);
}


/* Compute the face at character position POS in Lisp string STRING on
   window W, for ASCII characters.

   If STRING is an overlay string, it comes from position BUFPOS in
   current_buffer, otherwise BUFPOS is zero to indicate that STRING is
   not an overlay string.  W must display the current buffer.
   REGION_BEG and REGION_END give the start and end positions of the
   region; both are -1 if no region is visible.

   BASE_FACE_ID is the id of a face to merge with.  For strings coming
   from overlays or the `display' property it is the face at BUFPOS.

   If MOUSE_P, use the character's mouse-face, not its face.

   Set *ENDPTR to the next position where to check for faces in
   STRING; -1 if the face is constant from POS to the end of the
   string.

   Value is the id of the face to use.  The face returned is suitable
   for displaying ASCII characters.  */

int
face_at_string_position (struct window *w, Lisp_Object string,
                         ptrdiff_t pos, ptrdiff_t bufpos,
                         ptrdiff_t *endptr, enum face_id base_face_id,
                         bool mouse_p,
                         enum lface_attribute_index attr_filter)
{
  Lisp_Object prop, position, end, limit;
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  struct face *base_face;
  bool multibyte_p = STRING_MULTIBYTE (string);
  Lisp_Object prop_name = mouse_p ? Qmouse_face : Qface;

  /* Get the value of the face property at the current position within
     STRING.  Value is nil if there is no face property.  */
  XSETFASTINT (position, pos);
  prop = Fget_text_property (position, prop_name, string);

  /* Get the next position at which to check for faces.  Value of end
     is nil if face is constant all the way to the end of the string.
     Otherwise it is a string position where to check faces next.
     Limit is the maximum position up to which to check for property
     changes in Fnext_single_property_change.  Strings are usually
     short, so set the limit to the end of the string.  */
  XSETFASTINT (limit, SCHARS (string));
  end = Fnext_single_property_change (position, prop_name, string, limit);
  if (FIXNUMP (end))
    *endptr = XFIXNAT (end);
  else
    *endptr = -1;

  base_face = FACE_FROM_ID_OR_NULL (f, base_face_id);
  if (!base_face)
    base_face = FACE_FROM_ID (f, lookup_basic_face (w, f, DEFAULT_FACE_ID));

  /* Optimize the default case that there is no face property.  */
  if (NILP (prop)
      && (multibyte_p
	  /* We can't realize faces for different charsets differently
	     if we don't have fonts, so we can stop here if not working
	     on a window-system frame.  */
	  || !FRAME_WINDOW_P (f)
	  || FACE_SUITABLE_FOR_ASCII_CHAR_P (base_face)))
    return base_face->id;

  /* Begin with attributes from the base face.  */
  memcpy (attrs, base_face->lface, sizeof attrs);

  /* Merge in attributes specified via text properties.  */
  if (!NILP (prop))
    merge_face_ref (w, f, prop, attrs, true, NULL, attr_filter);

  /* Look up a realized face with the given face attributes,
     or realize a new one for ASCII characters.  */
  return lookup_face (f, attrs);
}


/* Merge a face into a realized face.

   W is a window in the frame where faces are (to be) realized.

   FACE_NAME is named face to merge.

   If FACE_NAME is nil, FACE_ID is face_id of realized face to merge.

   If FACE_NAME is t, FACE_ID is lface_id of face to merge.

   BASE_FACE_ID is realized face to merge into.

   Return new face id.
*/

int
merge_faces (struct window *w, Lisp_Object face_name, int face_id,
	     int base_face_id)
{
  struct frame *f = WINDOW_XFRAME (w);
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  struct face *base_face = FACE_FROM_ID_OR_NULL (f, base_face_id);

  if (!base_face)
    return base_face_id;

  if (EQ (face_name, Qt))
    {
      if (face_id < 0 || face_id >= lface_id_to_name_size)
	return base_face_id;
      face_name = lface_id_to_name[face_id];
      /* When called during make-frame, lookup_derived_face may fail
	 if the faces are uninitialized.  Don't signal an error.  */
      face_id = lookup_derived_face (w, f, face_name, base_face_id, 0);
      return (face_id >= 0 ? face_id : base_face_id);
    }

  /* Begin with attributes from the base face.  */
  memcpy (attrs, base_face->lface, sizeof attrs);

  if (!NILP (face_name))
    {
      if (!merge_named_face (w, f, face_name, attrs, NULL, 0))
	return base_face_id;
    }
  else
    {
      if (face_id < 0)
	return base_face_id;

      struct face *face = FACE_FROM_ID_OR_NULL (f, face_id);

      if (!face)
	return base_face_id;

      if (face_id != DEFAULT_FACE_ID)
	{
	  struct face *deflt = FACE_FROM_ID (f, DEFAULT_FACE_ID);
	  Lisp_Object lface_attrs[LFACE_VECTOR_SIZE];
	  int i;

	  memcpy (lface_attrs, face->lface, LFACE_VECTOR_SIZE);
	  /* Make explicit any attributes whose value is 'reset'.  */
	  for (i = 1; i < LFACE_VECTOR_SIZE; i++)
	    if (EQ (lface_attrs[i], Qreset))
	      lface_attrs[i] = deflt->lface[i];
	  merge_face_vectors (w, f, lface_attrs, attrs, 0);
	}
      else
	merge_face_vectors (w, f, face->lface, attrs, 0);
    }

  /* Look up a realized face with the given face attributes,
     or realize a new one for ASCII characters.  */
  return lookup_face (f, attrs);
}



#ifndef HAVE_X_WINDOWS
DEFUN ("x-load-color-file", Fx_load_color_file,
       Sx_load_color_file, 1, 1, 0,
       doc: /* Create an alist of color entries from an external file.

The file should define one named RGB color per line like so:
  R G B   name
where R,G,B are numbers between 0 and 255 and name is an arbitrary string.  */)
  (Lisp_Object filename)
{
  FILE *fp;
  Lisp_Object cmap = Qnil;
  Lisp_Object abspath;

  CHECK_STRING (filename);
  abspath = Fexpand_file_name (filename, Qnil);

  block_input ();
  fp = emacs_fopen (SSDATA (abspath), "r" FOPEN_TEXT);
  if (fp)
    {
      char buf[512];
      int red, green, blue;
      int num;

      while (fgets (buf, sizeof (buf), fp) != NULL)
	if (sscanf (buf, "%d %d %d %n", &red, &green, &blue, &num) == 3)
	  {
#ifdef HAVE_NTGUI
	    int color = RGB (red, green, blue);
#else
	    int color = (red << 16) | (green << 8) | blue;
#endif
	    char *name = buf + num;
	    ptrdiff_t len = strlen (name);
	    len -= 0 < len && name[len - 1] == '\n';
	    cmap = Fcons (Fcons (make_string (name, len), make_fixnum (color)),
			  cmap);
	  }
      fclose (fp);
    }
  unblock_input ();
  return cmap;
}
#endif


/***********************************************************************
				Tests
 ***********************************************************************/

#ifdef GLYPH_DEBUG

/* Print the contents of the realized face FACE to stderr.  */

static void
dump_realized_face (struct face *face)
{
  fprintf (stderr, "ID: %d\n", face->id);
#ifdef HAVE_X_WINDOWS
  fprintf (stderr, "gc: %p\n", face->gc);
#endif
  fprintf (stderr, "foreground: 0x%lx (%s)\n",
	   face->foreground,
	   SDATA (face->lface[LFACE_FOREGROUND_INDEX]));
  fprintf (stderr, "background: 0x%lx (%s)\n",
	   face->background,
	   SDATA (face->lface[LFACE_BACKGROUND_INDEX]));
  if (face->font)
    fprintf (stderr, "font_name: %s (%s)\n",
	     SDATA (face->font->props[FONT_NAME_INDEX]),
	     SDATA (face->lface[LFACE_FAMILY_INDEX]));
#ifdef HAVE_X_WINDOWS
  fprintf (stderr, "font = %p\n", face->font);
#endif
  fprintf (stderr, "fontset: %d\n", face->fontset);
  fprintf (stderr, "underline: %d (%s)\n",
	   face->underline,
	   SDATA (Fsymbol_name (face->lface[LFACE_UNDERLINE_INDEX])));
  fprintf (stderr, "hash: %" PRIuPTR "\n", face->hash);
}


DEFUN ("dump-face", Fdump_face, Sdump_face, 0, 1, 0, doc: /* */)
  (Lisp_Object n)
{
  if (NILP (n))
    {
      int i;

      fputs ("font selection order: ", stderr);
      for (i = 0; i < ARRAYELTS (font_sort_order); ++i)
	fprintf (stderr, "%d ", font_sort_order[i]);
      putc ('\n', stderr);

      fputs ("alternative fonts: ", stderr);
      debug_print (Vface_alternative_font_family_alist);
      putc ('\n', stderr);

      for (i = 0; i < FRAME_FACE_CACHE (SELECTED_FRAME ())->used; ++i)
	Fdump_face (make_fixnum (i));
    }
  else
    {
      struct face *face;
      CHECK_FIXNUM (n);
      face = FACE_FROM_ID_OR_NULL (SELECTED_FRAME (), XFIXNUM (n));
      if (face == NULL)
	error ("Not a valid face");
      dump_realized_face (face);
    }

  return Qnil;
}


DEFUN ("show-face-resources", Fshow_face_resources, Sshow_face_resources,
       0, 0, 0, doc: /* */)
  (void)
{
  fprintf (stderr, "number of colors = %d\n", ncolors_allocated);
  fprintf (stderr, "number of pixmaps = %d\n", npixmaps_allocated);
  fprintf (stderr, "number of GCs = %d\n", ngcs);
  return Qnil;
}

#endif /* GLYPH_DEBUG */



/***********************************************************************
			    Initialization
 ***********************************************************************/

/* All the faces defined during loadup are recorded in
   face-new-frame-defaults.  We need to set next_lface_id to the next
   face ID number, so that any new faces defined in this session will
   have face IDs different from those defined during loadup.  We also
   need to set up the lface_id_to_name[] array for the faces that were
   defined during loadup.  */
void
init_xfaces (void)
{
#ifdef HAVE_PDUMPER
  int nfaces;

  if (dumped_with_pdumper_p ())
    {
      nfaces = XFIXNAT (Fhash_table_count (Vface_new_frame_defaults));
      if (nfaces > 0)
	{
	  /* Allocate the lface_id_to_name[] array.  */
	  lface_id_to_name_size = next_lface_id = nfaces;
	  lface_id_to_name = xnmalloc (next_lface_id, sizeof *lface_id_to_name);

	  /* Store the faces.  */
	  struct Lisp_Hash_Table* table = XHASH_TABLE (Vface_new_frame_defaults);
	  for (ptrdiff_t idx = 0; idx < nfaces; ++idx)
	    {
	      Lisp_Object lface = HASH_KEY (table, idx);
	      Lisp_Object face_id = CAR (HASH_VALUE (table, idx));
	      if (FIXNATP (face_id))
		{
		  int id = XFIXNAT (face_id);
		  eassert (id >= 0);
		  lface_id_to_name[id] = lface;
		}
	    }
	}
    }
#endif

  face_attr_sym[0] = Qface;
  face_attr_sym[LFACE_FOUNDRY_INDEX] = QCfoundry;
  face_attr_sym[LFACE_SWIDTH_INDEX] = QCwidth;
  face_attr_sym[LFACE_HEIGHT_INDEX] = QCheight;
  face_attr_sym[LFACE_WEIGHT_INDEX] = QCweight;
  face_attr_sym[LFACE_SLANT_INDEX] = QCslant;
  face_attr_sym[LFACE_UNDERLINE_INDEX] = QCunderline;
  face_attr_sym[LFACE_INVERSE_INDEX] = QCinverse_video;
  face_attr_sym[LFACE_FOREGROUND_INDEX] = QCforeground;
  face_attr_sym[LFACE_BACKGROUND_INDEX] = QCbackground;
  face_attr_sym[LFACE_STIPPLE_INDEX] = QCstipple;
  face_attr_sym[LFACE_OVERLINE_INDEX] = QCoverline;
  face_attr_sym[LFACE_STRIKE_THROUGH_INDEX] = QCstrike_through;
  face_attr_sym[LFACE_BOX_INDEX] = QCbox;
  face_attr_sym[LFACE_FONT_INDEX] = QCfont;
  face_attr_sym[LFACE_INHERIT_INDEX] = QCinherit;
  face_attr_sym[LFACE_FONTSET_INDEX] = QCfontset;
  face_attr_sym[LFACE_DISTANT_FOREGROUND_INDEX] = QCdistant_foreground;
  face_attr_sym[LFACE_EXTEND_INDEX] = QCextend;
}

void
syms_of_xfaces (void)
{
  /* The symbols `face' and `mouse-face' used as text properties.  */
  DEFSYM (Qface, "face");

  /* Property for basic faces which other faces cannot inherit.  */
  DEFSYM (Qface_no_inherit, "face-no-inherit");

  /* Error symbol for wrong_type_argument in load_pixmap.  */
  DEFSYM (Qbitmap_spec_p, "bitmap-spec-p");

  /* The name of the function to call when the background of the frame
     has changed, frame_set_background_mode.  */
  DEFSYM (Qframe_set_background_mode, "frame-set-background-mode");

  /* Lisp face attribute keywords.  */
  DEFSYM (QCfamily, ":family");
  DEFSYM (QCheight, ":height");
  DEFSYM (QCweight, ":weight");
  DEFSYM (QCslant, ":slant");
  DEFSYM (QCunderline, ":underline");
  DEFSYM (QCinverse_video, ":inverse-video");
  DEFSYM (QCreverse_video, ":reverse-video");
  DEFSYM (QCforeground, ":foreground");
  DEFSYM (QCbackground, ":background");
  DEFSYM (QCstipple, ":stipple");
  DEFSYM (QCwidth, ":width");
  DEFSYM (QCfont, ":font");
  DEFSYM (QCfontset, ":fontset");
  DEFSYM (QCdistant_foreground, ":distant-foreground");
  DEFSYM (QCbold, ":bold");
  DEFSYM (QCitalic, ":italic");
  DEFSYM (QCoverline, ":overline");
  DEFSYM (QCstrike_through, ":strike-through");
  DEFSYM (QCbox, ":box");
  DEFSYM (QCinherit, ":inherit");
  DEFSYM (QCextend, ":extend");

  /* Symbols used for Lisp face attribute values.  */
  DEFSYM (QCcolor, ":color");
  DEFSYM (QCline_width, ":line-width");
  DEFSYM (QCstyle, ":style");
  DEFSYM (QCposition, ":position");
  DEFSYM (Qline, "line");
  DEFSYM (Qwave, "wave");
  DEFSYM (Qreleased_button, "released-button");
  DEFSYM (Qpressed_button, "pressed-button");
  DEFSYM (Qflat_button, "flat-button");
  DEFSYM (Qnormal, "normal");
  DEFSYM (Qthin, "thin");
  DEFSYM (Qextra_light, "extra-light");
  DEFSYM (Qultra_light, "ultra-light");
  DEFSYM (Qlight, "light");
  DEFSYM (Qsemi_light, "semi-light");
  DEFSYM (Qmedium, "medium");
  DEFSYM (Qsemi_bold, "semi-bold");
  DEFSYM (Qbook, "book");
  DEFSYM (Qbold, "bold");
  DEFSYM (Qextra_bold, "extra-bold");
  DEFSYM (Qultra_bold, "ultra-bold");
  DEFSYM (Qheavy, "heavy");
  DEFSYM (Qultra_heavy, "ultra-heavy");
  DEFSYM (Qblack, "black");
  DEFSYM (Qoblique, "oblique");
  DEFSYM (Qitalic, "italic");
  DEFSYM (Qreset, "reset");

  /* The symbols `foreground-color' and `background-color' which can be
     used as part of a `face' property.  This is for compatibility with
     Emacs 20.2.  */
  DEFSYM (Qbackground_color, "background-color");
  DEFSYM (Qforeground_color, "foreground-color");

  DEFSYM (Qunspecified, "unspecified");
  DEFSYM (QCignore_defface, ":ignore-defface");

  /* Used for limiting character attributes to windows with specific
     characteristics.  */
  DEFSYM (QCwindow, ":window");
  DEFSYM (QCfiltered, ":filtered");

  /* The symbol `face-alias'.  A symbol having that property is an
     alias for another face.  Value of the property is the name of
     the aliased face.  */
  DEFSYM (Qface_alias, "face-alias");

  /* Names of basic faces.  */
  DEFSYM (Qdefault, "default");
  DEFSYM (Qtool_bar, "tool-bar");
  DEFSYM (Qtab_bar, "tab-bar");
  DEFSYM (Qfringe, "fringe");
  DEFSYM (Qtab_line, "tab-line");
  DEFSYM (Qheader_line, "header-line");
  DEFSYM (Qscroll_bar, "scroll-bar");
  DEFSYM (Qmenu, "menu");
  DEFSYM (Qcursor, "cursor");
  DEFSYM (Qborder, "border");
  DEFSYM (Qmouse, "mouse");
  DEFSYM (Qmode_line_inactive, "mode-line-inactive");
  DEFSYM (Qmode_line_active, "mode-line-active");
  DEFSYM (Qvertical_border, "vertical-border");
  DEFSYM (Qwindow_divider, "window-divider");
  DEFSYM (Qwindow_divider_first_pixel, "window-divider-first-pixel");
  DEFSYM (Qwindow_divider_last_pixel, "window-divider-last-pixel");
  DEFSYM (Qinternal_border, "internal-border");
  DEFSYM (Qchild_frame_border, "child-frame-border");

  /* TTY color-related functions (defined in tty-colors.el).  */
  DEFSYM (Qtty_color_desc, "tty-color-desc");
  DEFSYM (Qtty_color_standard_values, "tty-color-standard-values");
  DEFSYM (Qtty_color_by_index, "tty-color-by-index");

  /* The name of the function used to compute colors on TTYs.  */
  DEFSYM (Qtty_color_alist, "tty-color-alist");

  Vface_alternative_font_family_alist = Qnil;
  staticpro (&Vface_alternative_font_family_alist);
  Vface_alternative_font_registry_alist = Qnil;
  staticpro (&Vface_alternative_font_registry_alist);

  defsubr (&Sinternal_make_lisp_face);
  defsubr (&Sinternal_lisp_face_p);
  defsubr (&Sinternal_set_lisp_face_attribute);
#ifdef HAVE_WINDOW_SYSTEM
  defsubr (&Sinternal_set_lisp_face_attribute_from_resource);
#endif
  defsubr (&Scolor_gray_p);
  defsubr (&Scolor_supported_p);
#ifndef HAVE_X_WINDOWS
  defsubr (&Sx_load_color_file);
#endif
  defsubr (&Sface_attribute_relative_p);
  defsubr (&Smerge_face_attribute);
  defsubr (&Sinternal_get_lisp_face_attribute);
  defsubr (&Sinternal_lisp_face_attribute_values);
  defsubr (&Sinternal_lisp_face_equal_p);
  defsubr (&Sinternal_lisp_face_empty_p);
  defsubr (&Sinternal_copy_lisp_face);
  defsubr (&Sinternal_merge_in_global_face);
  defsubr (&Sface_font);
  defsubr (&Sframe_face_hash_table);
  defsubr (&Sdisplay_supports_face_attributes_p);
  defsubr (&Scolor_distance);
  defsubr (&Sinternal_set_font_selection_order);
  defsubr (&Sinternal_set_alternative_font_family_alist);
  defsubr (&Sinternal_set_alternative_font_registry_alist);
  defsubr (&Sface_attributes_as_vector);
#ifdef GLYPH_DEBUG
  defsubr (&Sdump_face);
  defsubr (&Sshow_face_resources);
#endif /* GLYPH_DEBUG */
  defsubr (&Sclear_face_cache);
  defsubr (&Stty_suppress_bold_inverse_default_colors);

#if defined DEBUG_X_COLORS && defined HAVE_X_WINDOWS
  defsubr (&Sdump_colors);
#endif

  DEFVAR_BOOL ("face-filters-always-match", face_filters_always_match,
    doc: /* Non-nil means that face filters are always deemed to match.
This variable is intended for use only by code that evaluates
the "specificity" of a face specification and should be let-bound
only for this purpose.  */);

  DEFVAR_LISP ("face--new-frame-defaults", Vface_new_frame_defaults,
    doc: /* Hash table of global face definitions (for internal use only.)  */);
  Vface_new_frame_defaults =
    /* 33 entries is enough to fit all basic faces */
    make_hash_table (hashtest_eq, 33, DEFAULT_REHASH_SIZE,
                     DEFAULT_REHASH_THRESHOLD, Qnil, false);

  DEFVAR_LISP ("face-default-stipple", Vface_default_stipple,
    doc: /* Default stipple pattern used on monochrome displays.
This stipple pattern is used on monochrome displays
instead of shades of gray for a face background color.
See `set-face-stipple' for possible values for this variable.  */);
  Vface_default_stipple = build_pure_c_string ("gray3");

  DEFVAR_LISP ("tty-defined-color-alist", Vtty_defined_color_alist,
   doc: /* An alist of defined terminal colors and their RGB values.
See the docstring of `tty-color-alist' for the details.  */);
  Vtty_defined_color_alist = Qnil;

  DEFVAR_LISP ("scalable-fonts-allowed", Vscalable_fonts_allowed,
	       doc: /* Allowed scalable fonts.
A value of nil means don't allow any scalable fonts.
A value of t means allow any scalable font.
Otherwise, value must be a list of regular expressions.  A font may be
scaled if its name matches a regular expression in the list.
Note that if value is nil, a scalable font might still be used, if no
other font of the appropriate family and registry is available.  */);
  Vscalable_fonts_allowed = Qnil;

  DEFVAR_LISP ("face-ignored-fonts", Vface_ignored_fonts,
	       doc: /* List of ignored fonts.
Each element is a regular expression that matches names of fonts to
ignore.  */);
#ifdef HAVE_XFT
  /* This font causes libXft crashes, so ignore it by default.  Bug#37786.  */
  Vface_ignored_fonts = list1 (build_string ("Noto Color Emoji"));
#else
  Vface_ignored_fonts = Qnil;
#endif
#ifdef HAVE_OTF_KANNADA_BUG
  /* This font causes libotf crashes, so ignore it when we know we're
     using a vulnerable version.  https://debbugs.gnu.org/30193  */
  Vface_ignored_fonts = Fcons (build_string ("Noto Serif Kannada"), Vface_ignored_fonts);
#endif

  DEFVAR_LISP ("face-remapping-alist", Vface_remapping_alist,
	       doc: /* Alist of face remappings.
Each element is of the form:

   (FACE . REPLACEMENT),

which causes display of the face FACE to use REPLACEMENT instead.
REPLACEMENT is a face specification, i.e. one of the following:

  (1) a face name
  (2) a property list of attribute/value pairs, or
  (3) a list in which each element has one of the above forms.

List values for REPLACEMENT are merged to form the final face
specification, with earlier entries taking precedence, in the same way
as with the `face' text property.

Face-name remapping cycles are suppressed; recursive references use
the underlying face instead of the remapped face.  So a remapping of
the form:

   (FACE EXTRA-FACE... FACE)

or:

   (FACE (FACE-ATTR VAL ...) FACE)

causes EXTRA-FACE... or (FACE-ATTR VAL ...) to be _merged_ with the
existing definition of FACE.  Note that this isn't necessary for the
default face, since every face inherits from the default face.

An entry in the list can also be a filtered face expression of the
form:

  (:filtered FILTER FACE-SPECIFICATION)

This construct applies FACE-SPECIFICATION (which can have any of the
forms allowed for face specifications generally) only if FILTER
matches at the moment Emacs wants to draw text with the combined face.

The only filters currently defined are NIL (which always matches) and
(:window PARAMETER VALUE), which matches only in the context of a
window with a parameter EQ-equal to VALUE.

An entry in the face list can also be nil, which does nothing.

If `face-remapping-alist' is made buffer-local, the face remapping
takes effect only in that buffer.  For instance, the mode my-mode
could define a face `my-mode-default', and then in the mode setup
function, do:

   (set (make-local-variable \\='face-remapping-alist)
        (copy-tree \\='((default my-mode-default)))).

You probably want to use the face-remap package included in Emacs
instead of manipulating face-remapping-alist directly.  Note that many
of the functions in that package modify the list destructively, so make
sure you set it to a fresh value (for instance, use `copy-tree' as in
the example above) before modifying.

Because Emacs normally only redraws screen areas when the underlying
buffer contents change, you may need to call `redraw-display' after
changing this variable for it to take effect.  */);
  Vface_remapping_alist = Qnil;
  DEFSYM (Qface_remapping_alist,"face-remapping-alist");

  DEFVAR_LISP ("face-font-rescale-alist", Vface_font_rescale_alist,
	       doc: /* Alist of fonts vs the rescaling factors.
Each element is a cons (FONT-PATTERN . RESCALE-RATIO), where
FONT-PATTERN is a font-spec or a regular expression matching a font name, and
RESCALE-RATIO is a floating point number to specify how much larger
\(or smaller) font we should use.  For instance, if a face requests
a font of 10 point, we actually use a font of 10 * RESCALE-RATIO point.  */);
  Vface_font_rescale_alist = Qnil;

  DEFVAR_INT ("face-near-same-color-threshold", face_near_same_color_threshold,
	      doc: /* Threshold for using distant-foreground color instead of foreground.

The value should be an integer number providing the minimum distance
between two colors that will still qualify them to be used as foreground
and background.  If the value of `color-distance', invoked with a nil
METRIC argument, for the foreground and background colors of a face is
less than this threshold, the distant-foreground color, if defined,
will be used for the face instead of the foreground color.

Lisp programs that change the value of this variable should also
clear the face cache, see `clear-face-cache'.  */);
  face_near_same_color_threshold = 30000;

  DEFVAR_LISP ("face-font-lax-matched-attributes",
	       Vface_font_lax_matched_attributes,
	       doc: /* Whether to match some face attributes in lax manner when realizing faces.

If non-nil, some font-related face attributes will be matched in a lax
manner when looking for candidate fonts.
If the value is t, the default, the search for fonts will not insist
on exact match for 3 font attributes: weight, width, and slant.
Instead, it will examine the available fonts with various values of
these attributes, and select the font that is the closest possible
match.  (If an exact match is available, it will still be selected,
as that is the closest match.)  For example, looking for a semi-bold
font might select a bold or a medium-weight font if no semi-bold font
matching other attributes can be found.  This is especially important
when the `default' face specifies unusual values for one or more of
these 3 attributes, which other installed fonts don't support.

The value can also be a list of font-related face attribute symbols;
see `set-face-attribute' for the full list of attributes.  Then the
corresponding face attributes will be treated as "soft" constraints
in the manner described above, instead of the default 3 attributes.

If the value is nil, candidate fonts might be rejected if the don't
have exactly the same values of attributes as the face requests.

This variable exists for debugging of the font-selection process,
and we advise not to change it otherwise.  */);
  Vface_font_lax_matched_attributes = Qt;

#ifdef HAVE_WINDOW_SYSTEM
  defsubr (&Sbitmap_spec_p);
  defsubr (&Sx_list_fonts);
  defsubr (&Sinternal_face_x_get_resource);
  defsubr (&Sx_family_fonts);
#endif
  defsubr (&Scolor_values_from_color_spec);
}
