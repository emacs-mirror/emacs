/* xfaces.c -- "Face" primitives.
   Copyright (C) 1993, 1994, 1998, 1999, 2000, 2001
   Free Software Foundation.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* New face implementation by Gerd Moellmann <gerd@gnu.org>.  */

/* Faces.

   When using Emacs with X, the display style of characters can be
   changed by defining `faces'.  Each face can specify the following
   display attributes:

   1. Font family name.

   2. Relative proportionate width, aka character set width or set
   width (swidth), e.g. `semi-compressed'.

   3. Font height in 1/10pt.

   4. Font weight, e.g. `bold'.

   5. Font slant, e.g. `italic'.

   6. Foreground color.

   7. Background color.

   8. Whether or not characters should be underlined, and in what color.

   9. Whether or not characters should be displayed in inverse video.

   10. A background stipple, a bitmap.

   11. Whether or not characters should be overlined, and in what color.

   12. Whether or not characters should be strike-through, and in what
   color.

   13. Whether or not a box should be drawn around characters, the box
   type, and, for simple boxes, in what color.

   14. Font or fontset pattern, or nil.  This is a special attribute.
   When this attribute is specified, the face uses a font opened by
   that pattern as is.  In addition, all the other font-related
   attributes (1st thru 5th) are generated from the opened font name.
   On the other hand, if one of the other font-related attributes are
   specified, this attribute is set to nil.  In that case, the face
   doesn't inherit this attribute from the `default' face, and uses a
   font determined by the other attributes (those may be inherited
   from the `default' face).

   15. A face name or list of face names from which to inherit attributes.

   16. A specified average font width, which is invisible from Lisp,
   and is used to ensure that a font specified on the command line,
   for example, can be matched exactly.

   Faces are frame-local by nature because Emacs allows to define the
   same named face (face names are symbols) differently for different
   frames.  Each frame has an alist of face definitions for all named
   faces.  The value of a named face in such an alist is a Lisp vector
   with the symbol `face' in slot 0, and a slot for each of the face
   attributes mentioned above.

   There is also a global face alist `Vface_new_frame_defaults'.  Face
   definitions from this list are used to initialize faces of newly
   created frames.

   A face doesn't have to specify all attributes.  Those not specified
   have a value of `unspecified'.  Faces specifying all attributes but
   the 14th are called `fully-specified'.


   Face merging.

   The display style of a given character in the text is determined by
   combining several faces.  This process is called `face merging'.
   Any aspect of the display style that isn't specified by overlays or
   text properties is taken from the `default' face.  Since it is made
   sure that the default face is always fully-specified, face merging
   always results in a fully-specified face.


   Face realization.

   After all face attributes for a character have been determined by
   merging faces of that character, that face is `realized'.  The
   realization process maps face attributes to what is physically
   available on the system where Emacs runs.  The result is a
   `realized face' in form of a struct face which is stored in the
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

   Thus, all realized face have a realized fontset.


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
#include <sys/types.h>
#include <sys/stat.h>

#include "lisp.h"
#include "charset.h"
#include "keyboard.h"
#include "frame.h"

#ifdef HAVE_WINDOW_SYSTEM
#include "fontset.h"
#endif /* HAVE_WINDOW_SYSTEM */

#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#ifdef USE_MOTIF
#include <Xm/Xm.h>
#include <Xm/XmStrDefs.h>
#endif /* USE_MOTIF */
#endif /* HAVE_X_WINDOWS */

#ifdef MSDOS
#include "dosfns.h"
#endif

#ifdef WINDOWSNT
#include "w32term.h"
#include "fontset.h"
/* Redefine X specifics to W32 equivalents to avoid cluttering the
   code with #ifdef blocks. */
#undef FRAME_X_DISPLAY_INFO
#define FRAME_X_DISPLAY_INFO FRAME_W32_DISPLAY_INFO
#define x_display_info w32_display_info
#define FRAME_X_FONT_TABLE FRAME_W32_FONT_TABLE
#define check_x check_w32
#define x_list_fonts w32_list_fonts
#define GCGraphicsExposures 0
/* For historic reasons, FONT_WIDTH refers to average width on W32,
   not maximum as on X. Redefine here. */
#undef FONT_WIDTH
#define FONT_WIDTH FONT_MAX_WIDTH
#endif /* WINDOWSNT */

#ifdef MAC_OS
#include "macterm.h"
#define x_display_info mac_display_info
#define check_x check_mac
#endif /* MAC_OS */

#include "buffer.h"
#include "dispextern.h"
#include "blockinput.h"
#include "window.h"
#include "intervals.h"

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
#else /* not XOS_NEEDS_TIME_H */
#include <X11/Xos.h>
#endif /* not XOS_NEEDS_TIME_H */

#endif /* HAVE_X_WINDOWS */

#include <stdio.h>
#include <ctype.h>

#define abs(X)		((X) < 0 ? -(X) : (X))

/* Number of pt per inch (from the TeXbook).  */

#define PT_PER_INCH 72.27

/* Non-zero if face attribute ATTR is unspecified.  */

#define UNSPECIFIEDP(ATTR) EQ ((ATTR), Qunspecified)

/* Value is the number of elements of VECTOR.  */

#define DIM(VECTOR) (sizeof (VECTOR) / sizeof *(VECTOR))

/* Make a copy of string S on the stack using alloca.  Value is a pointer
   to the copy.  */

#define STRDUPA(S) strcpy ((char *) alloca (strlen ((S)) + 1), (S))

/* Make a copy of the contents of Lisp string S on the stack using
   alloca.  Value is a pointer to the copy.  */

#define LSTRDUPA(S) STRDUPA (XSTRING ((S))->data)

/* Size of hash table of realized faces in face caches (should be a
   prime number).  */

#define FACE_CACHE_BUCKETS_SIZE 1001

/* A definition of XColor for non-X frames.  */

#ifndef HAVE_X_WINDOWS

typedef struct
{
  unsigned long pixel;
  unsigned short red, green, blue;
  char flags;
  char pad;
}
XColor;

#endif /* not HAVE_X_WINDOWS */

/* Keyword symbols used for face attribute names.  */

Lisp_Object QCfamily, QCheight, QCweight, QCslant, QCunderline;
Lisp_Object QCinverse_video, QCforeground, QCbackground, QCstipple;
Lisp_Object QCwidth, QCfont, QCbold, QCitalic;
Lisp_Object QCreverse_video;
Lisp_Object QCoverline, QCstrike_through, QCbox, QCinherit;

/* Symbols used for attribute values.  */

Lisp_Object Qnormal, Qbold, Qultra_light, Qextra_light, Qlight;
Lisp_Object Qsemi_light, Qsemi_bold, Qextra_bold, Qultra_bold;
Lisp_Object Qoblique, Qitalic, Qreverse_oblique, Qreverse_italic;
Lisp_Object Qultra_condensed, Qextra_condensed, Qcondensed;
Lisp_Object Qsemi_condensed, Qsemi_expanded, Qexpanded, Qextra_expanded;
Lisp_Object Qultra_expanded;
Lisp_Object Qreleased_button, Qpressed_button;
Lisp_Object QCstyle, QCcolor, QCline_width;
Lisp_Object Qunspecified;

char unspecified_fg[] = "unspecified-fg", unspecified_bg[] = "unspecified-bg";

/* The name of the function to call when the background of the frame
   has changed, frame_update_face_colors.  */

Lisp_Object Qframe_update_face_colors;

/* Names of basic faces.  */

Lisp_Object Qdefault, Qtool_bar, Qregion, Qfringe;
Lisp_Object Qheader_line, Qscroll_bar, Qcursor, Qborder, Qmouse, Qmenu;
Lisp_Object Qmode_line_inactive;
extern Lisp_Object Qmode_line;

/* The symbol `face-alias'.  A symbols having that property is an
   alias for another face.  Value of the property is the name of
   the aliased face.  */

Lisp_Object Qface_alias;

/* Names of frame parameters related to faces.  */

extern Lisp_Object Qscroll_bar_foreground, Qscroll_bar_background;
extern Lisp_Object Qborder_color, Qcursor_color, Qmouse_color;

/* Default stipple pattern used on monochrome displays.  This stipple
   pattern is used on monochrome displays instead of shades of gray
   for a face background color.  See `set-face-stipple' for possible
   values for this variable.  */

Lisp_Object Vface_default_stipple;

/* Alist of alternative font families.  Each element is of the form
   (FAMILY FAMILY1 FAMILY2 ...).  If fonts of FAMILY can't be loaded,
   try FAMILY1, then FAMILY2, ...  */

Lisp_Object Vface_alternative_font_family_alist;

/* Alist of alternative font registries.  Each element is of the form
   (REGISTRY REGISTRY1 REGISTRY2...).  If fonts of REGISTRY can't be
   loaded, try REGISTRY1, then REGISTRY2, ...  */

Lisp_Object Vface_alternative_font_registry_alist;

/* Allowed scalable fonts.  A value of nil means don't allow any
   scalable fonts.  A value of t means allow the use of any scalable
   font.  Otherwise, value must be a list of regular expressions.  A
   font may be scaled if its name matches a regular expression in the
   list.  */

Lisp_Object Vscalable_fonts_allowed, Qscalable_fonts_allowed;

/* List of regular expressions that matches names of fonts to ignore. */

Lisp_Object Vface_ignored_fonts;

/* Maximum number of fonts to consider in font_list.  If not an
   integer > 0, DEFAULT_FONT_LIST_LIMIT is used instead.  */

Lisp_Object Vfont_list_limit;
#define DEFAULT_FONT_LIST_LIMIT 100

/* The symbols `foreground-color' and `background-color' which can be
   used as part of a `face' property.  This is for compatibility with
   Emacs 20.2.  */

Lisp_Object Qforeground_color, Qbackground_color;

/* The symbols `face' and `mouse-face' used as text properties.  */

Lisp_Object Qface;
extern Lisp_Object Qmouse_face;

/* Error symbol for wrong_type_argument in load_pixmap.  */

Lisp_Object Qbitmap_spec_p;

/* Alist of global face definitions.  Each element is of the form
   (FACE . LFACE) where FACE is a symbol naming a face and LFACE
   is a Lisp vector of face attributes.  These faces are used
   to initialize faces for new frames.  */

Lisp_Object Vface_new_frame_defaults;

/* The next ID to assign to Lisp faces.  */

static int next_lface_id;

/* A vector mapping Lisp face Id's to face names.  */

static Lisp_Object *lface_id_to_name;
static int lface_id_to_name_size;

/* TTY color-related functions (defined in tty-colors.el).  */

Lisp_Object Qtty_color_desc, Qtty_color_by_index;

/* The name of the function used to compute colors on TTYs.  */

Lisp_Object Qtty_color_alist;

/* An alist of defined terminal colors and their RGB values.  */

Lisp_Object Vtty_defined_color_alist;

/* Counter for calls to clear_face_cache.  If this counter reaches
   CLEAR_FONT_TABLE_COUNT, and a frame has more than
   CLEAR_FONT_TABLE_NFONTS load, unused fonts are freed.  */

static int clear_font_table_count;
#define CLEAR_FONT_TABLE_COUNT	100
#define CLEAR_FONT_TABLE_NFONTS	10

/* Non-zero means face attributes have been changed since the last
   redisplay.  Used in redisplay_internal.  */

int face_change_count;

/* Non-zero means don't display bold text if a face's foreground
   and background colors are the inverse of the default colors of the
   display.   This is a kluge to suppress `bold black' foreground text
   which is hard to read on an LCD monitor.  */

int tty_suppress_bold_inverse_default_colors_p;

/* A list of the form `((x . y))' used to avoid consing in
   Finternal_set_lisp_face_attribute.  */

static Lisp_Object Vparam_value_alist;

/* The total number of colors currently allocated.  */

#if GLYPH_DEBUG
static int ncolors_allocated;
static int npixmaps_allocated;
static int ngcs;
#endif

/* Non-zero means the definition of the `menu' face for new frames has
   been changed.  */

int menu_face_changed_default;


/* Function prototypes.  */

struct font_name;
struct table_entry;

static void map_tty_color P_ ((struct frame *, struct face *,
			       enum lface_attribute_index, int *));
static Lisp_Object resolve_face_name P_ ((Lisp_Object));
static int may_use_scalable_font_p P_ ((char *));
static void set_font_frame_param P_ ((Lisp_Object, Lisp_Object));
static int better_font_p P_ ((int *, struct font_name *, struct font_name *,
			      int, int));
static int x_face_list_fonts P_ ((struct frame *, char *,
				  struct font_name *, int, int));
static int font_scalable_p P_ ((struct font_name *));
static int get_lface_attributes P_ ((struct frame *, Lisp_Object, Lisp_Object *, int));
static int load_pixmap P_ ((struct frame *, Lisp_Object, unsigned *, unsigned *));
static unsigned char *xstrlwr P_ ((unsigned char *));
static void signal_error P_ ((char *, Lisp_Object));
static struct frame *frame_or_selected_frame P_ ((Lisp_Object, int));
static void load_face_font P_ ((struct frame *, struct face *, int));
static void load_face_colors P_ ((struct frame *, struct face *, Lisp_Object *));
static void free_face_colors P_ ((struct frame *, struct face *));
static int face_color_gray_p P_ ((struct frame *, char *));
static char *build_font_name P_ ((struct font_name *));
static void free_font_names P_ ((struct font_name *, int));
static int sorted_font_list P_ ((struct frame *, char *,
				 int (*cmpfn) P_ ((const void *, const void *)),
				 struct font_name **));
static int font_list_1 P_ ((struct frame *, Lisp_Object, Lisp_Object,
			    Lisp_Object, struct font_name **));
static int font_list P_ ((struct frame *, Lisp_Object, Lisp_Object,
			  Lisp_Object, struct font_name **));
static int try_font_list P_ ((struct frame *, Lisp_Object *, 
			      Lisp_Object, Lisp_Object, struct font_name **));
static int try_alternative_families P_ ((struct frame *f, Lisp_Object,
					 Lisp_Object, struct font_name **));
static int cmp_font_names P_ ((const void *, const void *));
static struct face *realize_face P_ ((struct face_cache *, Lisp_Object *, int,
				      struct face *, int));
static struct face *realize_x_face P_ ((struct face_cache *,
					Lisp_Object *, int, struct face *));
static struct face *realize_tty_face P_ ((struct face_cache *,
					  Lisp_Object *, int));
static int realize_basic_faces P_ ((struct frame *));
static int realize_default_face P_ ((struct frame *));
static void realize_named_face P_ ((struct frame *, Lisp_Object, int));
static int lface_fully_specified_p P_ ((Lisp_Object *));
static int lface_equal_p P_ ((Lisp_Object *, Lisp_Object *));
static unsigned hash_string_case_insensitive P_ ((Lisp_Object));
static unsigned lface_hash P_ ((Lisp_Object *));
static int lface_same_font_attributes_p P_ ((Lisp_Object *, Lisp_Object *));
static struct face_cache *make_face_cache P_ ((struct frame *));
static void free_realized_face P_ ((struct frame *, struct face *));
static void clear_face_gcs P_ ((struct face_cache *));
static void free_face_cache P_ ((struct face_cache *));
static int face_numeric_weight P_ ((Lisp_Object));
static int face_numeric_slant P_ ((Lisp_Object));
static int face_numeric_swidth P_ ((Lisp_Object));
static int face_fontset P_ ((Lisp_Object *));
static char *choose_face_font P_ ((struct frame *, Lisp_Object *, int, int));
static void merge_face_vectors P_ ((struct frame *, Lisp_Object *, Lisp_Object*, Lisp_Object));
static void merge_face_inheritance P_ ((struct frame *f, Lisp_Object,
					Lisp_Object *, Lisp_Object));
static void merge_face_vector_with_property P_ ((struct frame *, Lisp_Object *,
						 Lisp_Object));
static int set_lface_from_font_name P_ ((struct frame *, Lisp_Object,
					 Lisp_Object, int, int));
static Lisp_Object lface_from_face_name P_ ((struct frame *, Lisp_Object, int));
static struct face *make_realized_face P_ ((Lisp_Object *));
static void free_realized_faces P_ ((struct face_cache *));
static char *best_matching_font P_ ((struct frame *, Lisp_Object *,
				     struct font_name *, int, int));
static void cache_face P_ ((struct face_cache *, struct face *, unsigned));
static void uncache_face P_ ((struct face_cache *, struct face *));
static int xlfd_numeric_slant P_ ((struct font_name *));
static int xlfd_numeric_weight P_ ((struct font_name *));
static int xlfd_numeric_swidth P_ ((struct font_name *));
static Lisp_Object xlfd_symbolic_slant P_ ((struct font_name *));
static Lisp_Object xlfd_symbolic_weight P_ ((struct font_name *));
static Lisp_Object xlfd_symbolic_swidth P_ ((struct font_name *));
static int xlfd_fixed_p P_ ((struct font_name *));
static int xlfd_numeric_value P_ ((struct table_entry *, int, struct font_name *,
				   int, int));
static Lisp_Object xlfd_symbolic_value P_ ((struct table_entry *, int,
					    struct font_name *, int,
					    Lisp_Object));
static struct table_entry *xlfd_lookup_field_contents P_ ((struct table_entry *, int,
							   struct font_name *, int));

#ifdef HAVE_WINDOW_SYSTEM

static int split_font_name P_ ((struct frame *, struct font_name *, int));
static int xlfd_point_size P_ ((struct frame *, struct font_name *));
static void sort_fonts P_ ((struct frame *, struct font_name *, int,
			       int (*cmpfn) P_ ((const void *, const void *))));
static GC x_create_gc P_ ((struct frame *, unsigned long, XGCValues *));
static void x_free_gc P_ ((struct frame *, GC));
static void clear_font_table P_ ((struct x_display_info *));

#ifdef WINDOWSNT
extern Lisp_Object w32_list_fonts P_ ((struct frame *, Lisp_Object, int, int));
#endif /* WINDOWSNT */

#ifdef USE_X_TOOLKIT
static void x_update_menu_appearance P_ ((struct frame *));

extern void free_frame_menubar P_ ((struct frame *));
#endif /* USE_X_TOOLKIT */

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
register_color (pixel)
     unsigned long pixel;
{
  xassert (pixel < 256);
  ++color_count[pixel];
}


/* Register color PIXEL as deallocated.  */

void
unregister_color (pixel)
     unsigned long pixel;
{
  xassert (pixel < 256);
  if (color_count[pixel] > 0)
    --color_count[pixel];
  else
    abort ();
}


/* Register N colors from PIXELS as deallocated.  */

void
unregister_colors (pixels, n)
     unsigned long *pixels;
     int n;
{
  int i;
  for (i = 0; i < n; ++i)
    unregister_color (pixels[i]);
}


DEFUN ("dump-colors", Fdump_colors, Sdump_colors, 0, 0, 0,
       doc: /* Dump currently allocated colors to stderr.  */)
     ()
{
  int i, n;

  fputc ('\n', stderr);

  for (i = n = 0; i < sizeof color_count / sizeof color_count[0]; ++i)
    if (color_count[i])
      {
	fprintf (stderr, "%3d: %5d", i, color_count[i]);
	++n;
	if (n % 5 == 0)
	  fputc ('\n', stderr);
	else
	  fputc ('\t', stderr);
      }

  if (n % 5 != 0)
    fputc ('\n', stderr);
  return Qnil;
}

#endif /* DEBUG_X_COLORS */


/* Free colors used on frame F.  PIXELS is an array of NPIXELS pixel
   color values.  Interrupt input must be blocked when this function
   is called.  */

void
x_free_colors (f, pixels, npixels)
     struct frame *f;
     unsigned long *pixels;
     int npixels;
{
  int class = FRAME_X_DISPLAY_INFO (f)->visual->class;

  /* If display has an immutable color map, freeing colors is not
     necessary and some servers don't allow it.  So don't do it.  */
  if (class != StaticColor && class != StaticGray && class != TrueColor)
    {
#ifdef DEBUG_X_COLORS
      unregister_colors (pixels, npixels);
#endif
      XFreeColors (FRAME_X_DISPLAY (f), FRAME_X_COLORMAP (f),
		   pixels, npixels, 0);
    }
}


/* Free colors used on frame F.  PIXELS is an array of NPIXELS pixel
   color values.  Interrupt input must be blocked when this function
   is called.  */

void
x_free_dpy_colors (dpy, screen, cmap, pixels, npixels)
     Display *dpy;
     Screen *screen;
     Colormap cmap;
     unsigned long *pixels;
     int npixels;
{
  struct x_display_info *dpyinfo = x_display_info_for_display (dpy);
  int class = dpyinfo->visual->class;

  /* If display has an immutable color map, freeing colors is not
     necessary and some servers don't allow it.  So don't do it.  */
  if (class != StaticColor && class != StaticGray && class != TrueColor)
    {
#ifdef DEBUG_X_COLORS
      unregister_colors (pixels, npixels);
#endif
      XFreeColors (dpy, cmap, pixels, npixels, 0);
    }
}


/* Create and return a GC for use on frame F.  GC values and mask
   are given by XGCV and MASK.  */

static INLINE GC
x_create_gc (f, mask, xgcv)
     struct frame *f;
     unsigned long mask;
     XGCValues *xgcv;
{
  GC gc;
  BLOCK_INPUT;
  gc = XCreateGC (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), mask, xgcv);
  UNBLOCK_INPUT;
  IF_DEBUG (++ngcs);
  return gc;
}


/* Free GC which was used on frame F.  */

static INLINE void
x_free_gc (f, gc)
     struct frame *f;
     GC gc;
{
  BLOCK_INPUT;
  xassert (--ngcs >= 0);
  XFreeGC (FRAME_X_DISPLAY (f), gc);
  UNBLOCK_INPUT;
}

#endif /* HAVE_X_WINDOWS */

#ifdef WINDOWSNT
/* W32 emulation of GCs */

static INLINE GC
x_create_gc (f, mask, xgcv)
     struct frame *f;
     unsigned long mask;
     XGCValues *xgcv;
{
  GC gc;
  BLOCK_INPUT;
  gc = XCreateGC (NULL, FRAME_W32_WINDOW (f), mask, xgcv);
  UNBLOCK_INPUT;
  IF_DEBUG (++ngcs);
  return gc;
}


/* Free GC which was used on frame F.  */

static INLINE void
x_free_gc (f, gc)
     struct frame *f;
     GC gc;
{
  BLOCK_INPUT;
  xassert (--ngcs >= 0);
  xfree (gc);
  UNBLOCK_INPUT;
}

#endif  /* WINDOWSNT */

#ifdef MAC_OS
/* Mac OS emulation of GCs */

extern XGCValues *XCreateGC (void *, Window, unsigned long, XGCValues *);

static INLINE GC
x_create_gc (f, mask, xgcv)
     struct frame *f;
     unsigned long mask;
     XGCValues *xgcv;
{
  GC gc;
  gc = XCreateGC (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f), mask, xgcv);
  return gc;
}

static INLINE void
x_free_gc (f, gc)
     struct frame *f;
     GC gc;
{
  XFreeGC (FRAME_MAC_DISPLAY (f), gc);
}

#endif  /* MAC_OS */

/* Like stricmp.  Used to compare parts of font names which are in
   ISO8859-1.  */

int
xstricmp (s1, s2)
     unsigned char *s1, *s2;
{
  while (*s1 && *s2)
    {
      unsigned char c1 = tolower (*s1);
      unsigned char c2 = tolower (*s2);
      if (c1 != c2)
	return c1 < c2 ? -1 : 1;
      ++s1, ++s2;
    }

  if (*s1 == 0)
    return *s2 == 0 ? 0 : -1;
  return 1;
}


/* Like strlwr, which might not always be available.  */

static unsigned char *
xstrlwr (s)
     unsigned char *s;
{
  unsigned char *p = s;

  for (p = s; *p; ++p)
    *p = tolower (*p);

  return s;
}


/* Signal `error' with message S, and additional argument ARG.  */

static void
signal_error (s, arg)
     char *s;
     Lisp_Object arg;
{
  Fsignal (Qerror, Fcons (build_string (s), Fcons (arg, Qnil)));
}


/* If FRAME is nil, return a pointer to the selected frame.
   Otherwise, check that FRAME is a live frame, and return a pointer
   to it.  NPARAM is the parameter number of FRAME, for
   CHECK_LIVE_FRAME.  This is here because it's a frequent pattern in
   Lisp function definitions.  */

static INLINE struct frame *
frame_or_selected_frame (frame, nparam)
     Lisp_Object frame;
     int nparam;
{
  if (NILP (frame))
    frame = selected_frame;

  CHECK_LIVE_FRAME (frame);
  return XFRAME (frame);
}


/***********************************************************************
			   Frames and faces
 ***********************************************************************/

/* Initialize face cache and basic faces for frame F.  */

void
init_frame_faces (f)
     struct frame *f;
{
  /* Make a face cache, if F doesn't have one.  */
  if (FRAME_FACE_CACHE (f) == NULL)
    FRAME_FACE_CACHE (f) = make_face_cache (f);

#ifdef HAVE_WINDOW_SYSTEM
  /* Make the image cache.  */
  if (FRAME_WINDOW_P (f))
    {
      if (FRAME_X_IMAGE_CACHE (f) == NULL)
	FRAME_X_IMAGE_CACHE (f) = make_image_cache ();
      ++FRAME_X_IMAGE_CACHE (f)->refcount;
    }
#endif /* HAVE_WINDOW_SYSTEM */

  /* Realize basic faces.  Must have enough information in frame
     parameters to realize basic faces at this point.  */
#ifdef HAVE_X_WINDOWS
  if (!FRAME_X_P (f) || FRAME_X_WINDOW (f))
#endif
#ifdef WINDOWSNT
  if (!FRAME_WINDOW_P (f) || FRAME_W32_WINDOW (f))
#endif
    if (!realize_basic_faces (f))
      abort ();
}


/* Free face cache of frame F.  Called from Fdelete_frame.  */

void
free_frame_faces (f)
     struct frame *f;
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
      struct image_cache *image_cache = FRAME_X_IMAGE_CACHE (f);
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
   of named faces. */

void
recompute_basic_faces (f)
     struct frame *f;
{
  if (FRAME_FACE_CACHE (f))
    {
      clear_face_cache (0);
      if (!realize_basic_faces (f))
	abort ();
    }
}


/* Clear the face caches of all frames.  CLEAR_FONTS_P non-zero means
   try to free unused fonts, too.  */

void
clear_face_cache (clear_fonts_p)
     int clear_fonts_p;
{
#ifdef HAVE_WINDOW_SYSTEM
  Lisp_Object tail, frame;
  struct frame *f;

  if (clear_fonts_p
      || ++clear_font_table_count == CLEAR_FONT_TABLE_COUNT)
    {
      struct x_display_info *dpyinfo;
      
      /* Fonts are common for frames on one display, i.e. on
	 one X screen.  */
      for (dpyinfo = x_display_list; dpyinfo; dpyinfo = dpyinfo->next)
	if (dpyinfo->n_fonts > CLEAR_FONT_TABLE_NFONTS)
	  clear_font_table (dpyinfo);
      
      /* From time to time see if we can unload some fonts.  This also
	 frees all realized faces on all frames.  Fonts needed by
	 faces will be loaded again when faces are realized again.  */
      clear_font_table_count = 0;

      FOR_EACH_FRAME (tail, frame)
	{
	  struct frame *f = XFRAME (frame);
	  if (FRAME_WINDOW_P (f)
	      && FRAME_X_DISPLAY_INFO (f)->n_fonts > CLEAR_FONT_TABLE_NFONTS)
	    free_all_realized_faces (frame);
	}
    }
  else
    {
      /* Clear GCs of realized faces.  */
      FOR_EACH_FRAME (tail, frame)
	{
	  f = XFRAME (frame);
	  if (FRAME_WINDOW_P (f))
	    {
	      clear_face_gcs (FRAME_FACE_CACHE (f));
	      clear_image_cache (f, 0);
	    }
	}
    }
#endif /* HAVE_WINDOW_SYSTEM */
}


DEFUN ("clear-face-cache", Fclear_face_cache, Sclear_face_cache, 0, 1, 0,
       doc: /* Clear face caches on all frames.
Optional THOROUGHLY non-nil means try to free unused fonts, too.  */)
     (thoroughly)
     Lisp_Object thoroughly;
{
  clear_face_cache (!NILP (thoroughly));
  ++face_change_count;
  ++windows_or_buffers_changed;
  return Qnil;
}



#ifdef HAVE_WINDOW_SYSTEM


/* Remove fonts from the font table of DPYINFO except for the default
   ASCII fonts of frames on that display.  Called from clear_face_cache
   from time to time.  */

static void
clear_font_table (dpyinfo)
     struct x_display_info *dpyinfo;
{
  int i;

  /* Free those fonts that are not used by frames on DPYINFO.  */
  for (i = 0; i < dpyinfo->n_fonts; ++i)
    {
      struct font_info *font_info = dpyinfo->font_table + i;
      Lisp_Object tail, frame;

      /* Check if slot is already free.  */
      if (font_info->name == NULL)
	continue;

      /* Don't free a default font of some frame.  */
      FOR_EACH_FRAME (tail, frame)
	{
	  struct frame *f = XFRAME (frame);
	  if (FRAME_WINDOW_P (f)
	      && font_info->font == FRAME_FONT (f))
	    break;
	}

      if (!NILP (tail))
	continue;

      /* Free names.  */
      if (font_info->full_name != font_info->name)
	xfree (font_info->full_name);
      xfree (font_info->name);

      /* Free the font.  */
      BLOCK_INPUT;
#ifdef HAVE_X_WINDOWS
      XFreeFont (dpyinfo->display, font_info->font);
#endif
#ifdef WINDOWSNT
      w32_unload_font (dpyinfo, font_info->font);
#endif
      UNBLOCK_INPUT;

      /* Mark font table slot free.  */
      font_info->font = NULL;
      font_info->name = font_info->full_name = NULL;
    }
}

#endif /* HAVE_WINDOW_SYSTEM */



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
     (object)
     Lisp_Object object;
{
  int pixmap_p = 0;

  if (STRINGP (object))
    /* If OBJECT is a string, it's a file name.  */
    pixmap_p = 1;
  else if (CONSP (object))
    {
      /* Otherwise OBJECT must be (WIDTH HEIGHT DATA), WIDTH and
	 HEIGHT must be integers > 0, and DATA must be string large
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

      if (NATNUMP (width) && NATNUMP (height) && STRINGP (data))
	{
	  int bytes_per_row = ((XFASTINT (width) + BITS_PER_CHAR - 1)
			       / BITS_PER_CHAR);
	  if (STRING_BYTES (XSTRING (data)) >= bytes_per_row * XINT (height))
	    pixmap_p = 1;
	}
    }

  return pixmap_p ? Qt : Qnil;
}


/* Load a bitmap according to NAME (which is either a file name or a
   pixmap spec) for use on frame F.  Value is the bitmap_id (see
   xfns.c).  If NAME is nil, return with a bitmap id of zero.  If
   bitmap cannot be loaded, display a message saying so, and return
   zero.  Store the bitmap width in *W_PTR and its height in *H_PTR,
   if these pointers are not null.  */

static int
load_pixmap (f, name, w_ptr, h_ptr)
     FRAME_PTR f;
     Lisp_Object name;
     unsigned int *w_ptr, *h_ptr;
{
  int bitmap_id;
  Lisp_Object tem;

  if (NILP (name))
    return 0;

  tem = Fbitmap_spec_p (name);
  if (NILP (tem))
    wrong_type_argument (Qbitmap_spec_p, name);

  BLOCK_INPUT;
  if (CONSP (name))
    {
      /* Decode a bitmap spec into a bitmap.  */

      int h, w;
      Lisp_Object bits;

      w = XINT (Fcar (name));
      h = XINT (Fcar (Fcdr (name)));
      bits = Fcar (Fcdr (Fcdr (name)));

      bitmap_id = x_create_bitmap_from_data (f, XSTRING (bits)->data,
					     w, h);
    }
  else
    {
      /* It must be a string -- a file name.  */
      bitmap_id = x_create_bitmap_from_file (f, name);
    }
  UNBLOCK_INPUT;

  if (bitmap_id < 0)
    {
      add_to_log ("Invalid or undefined bitmap %s", name, Qnil);
      bitmap_id = 0;

      if (w_ptr)
	*w_ptr = 0;
      if (h_ptr)
	*h_ptr = 0;
    }
  else
    {
#if GLYPH_DEBUG
      ++npixmaps_allocated;
#endif
      if (w_ptr)
	*w_ptr = x_bitmap_width (f, bitmap_id);

      if (h_ptr)
	*h_ptr = x_bitmap_height (f, bitmap_id);
    }

  return bitmap_id;
}

#endif /* HAVE_WINDOW_SYSTEM */



/***********************************************************************
			 Minimum font bounds
 ***********************************************************************/

#ifdef HAVE_WINDOW_SYSTEM

/* Update the line_height of frame F.  Return non-zero if line height
   changes.  */

int
frame_update_line_height (f)
     struct frame *f;
{
  int line_height, changed_p;

  line_height = FONT_HEIGHT (FRAME_FONT (f));
  changed_p = line_height != FRAME_LINE_HEIGHT (f);
  FRAME_LINE_HEIGHT (f) = line_height;
  return changed_p;
}

#endif /* HAVE_WINDOW_SYSTEM */


/***********************************************************************
				Fonts
 ***********************************************************************/

#ifdef HAVE_WINDOW_SYSTEM

/* Load font of face FACE which is used on frame F to display
   character C.  The name of the font to load is determined by lface
   and fontset of FACE.  */

static void
load_face_font (f, face, c)
     struct frame *f;
     struct face *face;
     int c;
{
  struct font_info *font_info = NULL;
  char *font_name;

  face->font_info_id = -1;
  face->font = NULL;

  font_name = choose_face_font (f, face->lface, face->fontset, c);
  if (!font_name)
    return;

  BLOCK_INPUT;
  font_info = FS_LOAD_FACE_FONT (f, c, font_name, face);
  UNBLOCK_INPUT;

  if (font_info)
    {
      face->font_info_id = font_info->font_idx;
      face->font = font_info->font;
      face->font_name = font_info->full_name;
      if (face->gc)
	{
	  x_free_gc (f, face->gc);
	  face->gc = 0;
	}
    }
  else
    add_to_log ("Unable to load font %s",
		build_string (font_name), Qnil);
  xfree (font_name);
}

#endif /* HAVE_WINDOW_SYSTEM */



/***********************************************************************
				X Colors
 ***********************************************************************/

/* A version of defined_color for non-X frames.  */

int
tty_defined_color (f, color_name, color_def, alloc)
     struct frame *f;
     char *color_name;
     XColor *color_def;
     int alloc;
{
  Lisp_Object color_desc;
  unsigned long color_idx = FACE_TTY_DEFAULT_COLOR;
  unsigned long red = 0, green = 0, blue = 0;
  int status = 1;

  if (*color_name && !NILP (Ffboundp (Qtty_color_desc)))
    {
      Lisp_Object frame;

      XSETFRAME (frame, f);
      status = 0;
      color_desc = call2 (Qtty_color_desc, build_string (color_name), frame);
      if (CONSP (color_desc) && CONSP (XCDR (color_desc)))
	{
	  color_idx = XINT (XCAR (XCDR (color_desc)));
	  if (CONSP (XCDR (XCDR (color_desc))))
	    {
	      red = XINT (XCAR (XCDR (XCDR (color_desc))));
	      green = XINT (XCAR (XCDR (XCDR (XCDR (color_desc)))));
	      blue = XINT (XCAR (XCDR (XCDR (XCDR (XCDR (color_desc))))));
	    }
	  status = 1;
	}
      else if (NILP (Fsymbol_value (intern ("tty-defined-color-alist"))))
	/* We were called early during startup, and the colors are not
	   yet set up in tty-defined-color-alist.  Don't return a failure
	   indication, since this produces the annoying "Unable to
	   load color" messages in the *Messages* buffer.  */
	status = 1;
    }
  if (color_idx == FACE_TTY_DEFAULT_COLOR && *color_name)
    {
      if (strcmp (color_name, "unspecified-fg") == 0)
	color_idx = FACE_TTY_DEFAULT_FG_COLOR;
      else if (strcmp (color_name, "unspecified-bg") == 0)
	color_idx = FACE_TTY_DEFAULT_BG_COLOR;
    }

  if (color_idx != FACE_TTY_DEFAULT_COLOR)
    status = 1;

  color_def->pixel = color_idx;
  color_def->red = red;
  color_def->green = green;
  color_def->blue = blue;

  return status;
}


/* Decide if color named COLOR_NAME is valid for the display
   associated with the frame F; if so, return the rgb values in
   COLOR_DEF.  If ALLOC is nonzero, allocate a new colormap cell.

   This does the right thing for any type of frame.  */

int
defined_color (f, color_name, color_def, alloc)
     struct frame *f;
     char *color_name;
     XColor *color_def;
     int alloc;
{
  if (!FRAME_WINDOW_P (f))
    return tty_defined_color (f, color_name, color_def, alloc);
#ifdef HAVE_X_WINDOWS
  else if (FRAME_X_P (f))
    return x_defined_color (f, color_name, color_def, alloc);
#endif
#ifdef WINDOWSNT
  else if (FRAME_W32_P (f))
    return w32_defined_color (f, color_name, color_def, alloc);
#endif
#ifdef MAC_OS
  else if (FRAME_MAC_P (f))
    return mac_defined_color (f, color_name, color_def, alloc);
#endif
  else
    abort ();
}


/* Given the index IDX of a tty color on frame F, return its name, a
   Lisp string.  */

Lisp_Object
tty_color_name (f, idx)
     struct frame *f;
     int idx;
{
  if (idx >= 0 && !NILP (Ffboundp (Qtty_color_by_index)))
    {
      Lisp_Object frame;
      Lisp_Object coldesc;

      XSETFRAME (frame, f);
      coldesc = call2 (Qtty_color_by_index, make_number (idx), frame);

      if (!NILP (coldesc))
	return XCAR (coldesc);
    }
#ifdef MSDOS
  /* We can have an MSDOG frame under -nw for a short window of
     opportunity before internal_terminal_init is called.  DTRT.  */
  if (FRAME_MSDOS_P (f) && !inhibit_window_system)
    return msdos_stdcolor_name (idx);
#endif

  if (idx == FACE_TTY_DEFAULT_FG_COLOR)
    return build_string (unspecified_fg);
  if (idx == FACE_TTY_DEFAULT_BG_COLOR)
    return build_string (unspecified_bg);

#ifdef WINDOWSNT
  return vga_stdcolor_name (idx);
#endif

  return Qunspecified;
}


/* Return non-zero if COLOR_NAME is a shade of gray (or white or
   black) on frame F.  The algorithm is taken from 20.2 faces.el.  */

static int
face_color_gray_p (f, color_name)
     struct frame *f;
     char *color_name;
{
  XColor color;
  int gray_p;

  if (defined_color (f, color_name, &color, 0))
    gray_p = ((abs (color.red - color.green)
	       < max (color.red, color.green) / 20)
	      && (abs (color.green - color.blue)
		  < max (color.green, color.blue) / 20)
	      && (abs (color.blue - color.red)
		  < max (color.blue, color.red) / 20));
  else
    gray_p = 0;

  return gray_p;
}


/* Return non-zero if color COLOR_NAME can be displayed on frame F.
   BACKGROUND_P non-zero means the color will be used as background
   color.  */

static int
face_color_supported_p (f, color_name, background_p)
     struct frame *f;
     char *color_name;
     int background_p;
{
  Lisp_Object frame;
  XColor not_used;

  XSETFRAME (frame, f);
  return (FRAME_WINDOW_P (f)
	  ? (!NILP (Fxw_display_color_p (frame))
	     || xstricmp (color_name, "black") == 0
	     || xstricmp (color_name, "white") == 0
	     || (background_p
		 && face_color_gray_p (f, color_name))
	     || (!NILP (Fx_display_grayscale_p (frame))
		 && face_color_gray_p (f, color_name)))
	  : tty_defined_color (f, color_name, &not_used, 0));
}


DEFUN ("color-gray-p", Fcolor_gray_p, Scolor_gray_p, 1, 2, 0,
       doc: /* Return non-nil if COLOR is a shade of gray (or white or black).
FRAME specifies the frame and thus the display for interpreting COLOR.
If FRAME is nil or omitted, use the selected frame.  */)
     (color, frame)
     Lisp_Object color, frame;
{
  struct frame *f;

  CHECK_FRAME (frame);
  CHECK_STRING (color);
  f = XFRAME (frame);
  return face_color_gray_p (f, XSTRING (color)->data) ? Qt : Qnil;
}


DEFUN ("color-supported-p", Fcolor_supported_p,
       Scolor_supported_p, 2, 3, 0,
       doc: /* Return non-nil if COLOR can be displayed on FRAME.
BACKGROUND-P non-nil means COLOR is used as a background.
If FRAME is nil or omitted, use the selected frame.
COLOR must be a valid color name.  */)
     (color, frame, background_p)
     Lisp_Object frame, color, background_p;
{
  struct frame *f;

  CHECK_FRAME (frame);
  CHECK_STRING (color);
  f = XFRAME (frame);
  if (face_color_supported_p (f, XSTRING (color)->data, !NILP (background_p)))
    return Qt;
  return Qnil;
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
load_color (f, face, name, target_index)
     struct frame *f;
     struct face *face;
     Lisp_Object name;
     enum lface_attribute_index target_index;
{
  XColor color;

  xassert (STRINGP (name));
  xassert (target_index == LFACE_FOREGROUND_INDEX
	   || target_index == LFACE_BACKGROUND_INDEX
	   || target_index == LFACE_UNDERLINE_INDEX
	   || target_index == LFACE_OVERLINE_INDEX
	   || target_index == LFACE_STRIKE_THROUGH_INDEX
	   || target_index == LFACE_BOX_INDEX);

  /* if the color map is full, defined_color will return a best match
     to the values in an existing cell. */
  if (!defined_color (f, XSTRING (name)->data, &color, 1))
    {
      add_to_log ("Unable to load color \"%s\"", name, Qnil);

      switch (target_index)
	{
	case LFACE_FOREGROUND_INDEX:
	  face->foreground_defaulted_p = 1;
	  color.pixel = FRAME_FOREGROUND_PIXEL (f);
	  break;

	case LFACE_BACKGROUND_INDEX:
	  face->background_defaulted_p = 1;
	  color.pixel = FRAME_BACKGROUND_PIXEL (f);
	  break;

	case LFACE_UNDERLINE_INDEX:
	  face->underline_defaulted_p = 1;
	  color.pixel = FRAME_FOREGROUND_PIXEL (f);
	  break;

	case LFACE_OVERLINE_INDEX:
	  face->overline_color_defaulted_p = 1;
	  color.pixel = FRAME_FOREGROUND_PIXEL (f);
	  break;

	case LFACE_STRIKE_THROUGH_INDEX:
	  face->strike_through_color_defaulted_p = 1;
	  color.pixel = FRAME_FOREGROUND_PIXEL (f);
	  break;

	case LFACE_BOX_INDEX:
	  face->box_color_defaulted_p = 1;
	  color.pixel = FRAME_FOREGROUND_PIXEL (f);
	  break;

	default:
	  abort ();
	}
    }
#if GLYPH_DEBUG
  else
    ++ncolors_allocated;
#endif

  return color.pixel;
}


#ifdef HAVE_WINDOW_SYSTEM

/* Load colors for face FACE which is used on frame F.  Colors are
   specified by slots LFACE_BACKGROUND_INDEX and LFACE_FOREGROUND_INDEX
   of ATTRS.  If the background color specified is not supported on F,
   try to emulate gray colors with a stipple from Vface_default_stipple.  */

static void
load_face_colors (f, face, attrs)
     struct frame *f;
     struct face *face;
     Lisp_Object *attrs;
{
  Lisp_Object fg, bg;

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
  if (!face_color_supported_p (f, XSTRING (bg)->data, 0)
      && !NILP (Fbitmap_spec_p (Vface_default_stipple)))
    {
      x_destroy_bitmap (f, face->stipple);
      face->stipple = load_pixmap (f, Vface_default_stipple,
				   &face->pixmap_w, &face->pixmap_h);
    }

  face->background = load_color (f, face, bg, LFACE_BACKGROUND_INDEX);
  face->foreground = load_color (f, face, fg, LFACE_FOREGROUND_INDEX);
}


/* Free color PIXEL on frame F.  */

void
unload_color (f, pixel)
     struct frame *f;
     unsigned long pixel;
{
#ifdef HAVE_X_WINDOWS
  if (pixel != -1)
    {
      BLOCK_INPUT;
      x_free_colors (f, &pixel, 1);
      UNBLOCK_INPUT;
    }
#endif
}


/* Free colors allocated for FACE.  */

static void
free_face_colors (f, face)
     struct frame *f;
     struct face *face;
{
#ifdef HAVE_X_WINDOWS
  if (face->colors_copied_bitwise_p)
    return;

  BLOCK_INPUT;

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

  if (face->underline_p
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

  UNBLOCK_INPUT;
#endif /* HAVE_X_WINDOWS */
}

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

/* An enumerator for each possible slant value of a font.  Taken from
   the XLFD specification.  */

enum xlfd_slant
{
  XLFD_SLANT_UNKNOWN,
  XLFD_SLANT_ROMAN,
  XLFD_SLANT_ITALIC,
  XLFD_SLANT_OBLIQUE,
  XLFD_SLANT_REVERSE_ITALIC,
  XLFD_SLANT_REVERSE_OBLIQUE,
  XLFD_SLANT_OTHER
};

/* Relative font weight according to XLFD documentation.  */

enum xlfd_weight
{
  XLFD_WEIGHT_UNKNOWN,
  XLFD_WEIGHT_ULTRA_LIGHT,	/* 10 */
  XLFD_WEIGHT_EXTRA_LIGHT,	/* 20 */
  XLFD_WEIGHT_LIGHT,		/* 30 */
  XLFD_WEIGHT_SEMI_LIGHT,	/* 40: SemiLight, Book, ...  */
  XLFD_WEIGHT_MEDIUM,		/* 50: Medium, Normal, Regular, ...  */
  XLFD_WEIGHT_SEMI_BOLD,	/* 60: SemiBold, DemiBold, ...  */
  XLFD_WEIGHT_BOLD,		/* 70: Bold, ... */
  XLFD_WEIGHT_EXTRA_BOLD,	/* 80: ExtraBold, Heavy, ...  */
  XLFD_WEIGHT_ULTRA_BOLD	/* 90: UltraBold, Black, ...  */
};

/* Relative proportionate width.  */

enum xlfd_swidth
{
  XLFD_SWIDTH_UNKNOWN,
  XLFD_SWIDTH_ULTRA_CONDENSED,	/* 10 */
  XLFD_SWIDTH_EXTRA_CONDENSED,	/* 20 */
  XLFD_SWIDTH_CONDENSED,	/* 30: Condensed, Narrow, Compressed, ... */
  XLFD_SWIDTH_SEMI_CONDENSED,	/* 40: semicondensed */
  XLFD_SWIDTH_MEDIUM,		/* 50: Medium, Normal, Regular, ... */
  XLFD_SWIDTH_SEMI_EXPANDED,	/* 60: SemiExpanded, DemiExpanded, ... */
  XLFD_SWIDTH_EXPANDED,		/* 70: Expanded... */
  XLFD_SWIDTH_EXTRA_EXPANDED,	/* 80: ExtraExpanded, Wide...  */
  XLFD_SWIDTH_ULTRA_EXPANDED	/* 90: UltraExpanded... */
};

/* Structure used for tables mapping XLFD weight, slant, and width
   names to numeric and symbolic values.  */

struct table_entry
{
  char *name;
  int numeric;
  Lisp_Object *symbol;
};

/* Table of XLFD slant names and their numeric and symbolic
   representations.  This table must be sorted by slant names in
   ascending order.  */

static struct table_entry slant_table[] =
{
  {"i",			XLFD_SLANT_ITALIC,		&Qitalic},
  {"o",			XLFD_SLANT_OBLIQUE,		&Qoblique},
  {"ot",		XLFD_SLANT_OTHER,		&Qitalic},
  {"r",			XLFD_SLANT_ROMAN,		&Qnormal},
  {"ri",		XLFD_SLANT_REVERSE_ITALIC,	&Qreverse_italic},
  {"ro",		XLFD_SLANT_REVERSE_OBLIQUE,	&Qreverse_oblique}
};

/* Table of XLFD weight names.  This table must be sorted by weight
   names in ascending order.  */

static struct table_entry weight_table[] =
{
  {"black",		XLFD_WEIGHT_ULTRA_BOLD,		&Qultra_bold},
  {"bold",		XLFD_WEIGHT_BOLD,		&Qbold},
  {"book",		XLFD_WEIGHT_SEMI_LIGHT,		&Qsemi_light},
  {"demi",		XLFD_WEIGHT_SEMI_BOLD,		&Qsemi_bold},
  {"demibold",		XLFD_WEIGHT_SEMI_BOLD,		&Qsemi_bold},
  {"extralight",	XLFD_WEIGHT_EXTRA_LIGHT,	&Qextra_light},
  {"extrabold",		XLFD_WEIGHT_EXTRA_BOLD,		&Qextra_bold},
  {"heavy",		XLFD_WEIGHT_EXTRA_BOLD,		&Qextra_bold},
  {"light",		XLFD_WEIGHT_LIGHT,		&Qlight},
  {"medium",		XLFD_WEIGHT_MEDIUM,		&Qnormal},
  {"normal",		XLFD_WEIGHT_MEDIUM,		&Qnormal},
  {"regular",		XLFD_WEIGHT_MEDIUM,		&Qnormal},
  {"semibold",		XLFD_WEIGHT_SEMI_BOLD,		&Qsemi_bold},
  {"semilight",		XLFD_WEIGHT_SEMI_LIGHT,		&Qsemi_light},
  {"ultralight",	XLFD_WEIGHT_ULTRA_LIGHT,	&Qultra_light},
  {"ultrabold",		XLFD_WEIGHT_ULTRA_BOLD,		&Qultra_bold}
};

/* Table of XLFD width names.  This table must be sorted by width
   names in ascending order.  */

static struct table_entry swidth_table[] =
{
  {"compressed",	XLFD_SWIDTH_CONDENSED,		&Qcondensed},
  {"condensed",		XLFD_SWIDTH_CONDENSED,		&Qcondensed},
  {"demiexpanded",	XLFD_SWIDTH_SEMI_EXPANDED,	&Qsemi_expanded},
  {"expanded",		XLFD_SWIDTH_EXPANDED,		&Qexpanded},
  {"extracondensed",	XLFD_SWIDTH_EXTRA_CONDENSED,	&Qextra_condensed},
  {"extraexpanded",	XLFD_SWIDTH_EXTRA_EXPANDED,	&Qextra_expanded},
  {"medium",		XLFD_SWIDTH_MEDIUM,		&Qnormal},
  {"narrow",		XLFD_SWIDTH_CONDENSED,		&Qcondensed},
  {"normal",		XLFD_SWIDTH_MEDIUM,		&Qnormal},
  {"regular",		XLFD_SWIDTH_MEDIUM,		&Qnormal},
  {"semicondensed",	XLFD_SWIDTH_SEMI_CONDENSED,	&Qsemi_condensed},
  {"semiexpanded",	XLFD_SWIDTH_SEMI_EXPANDED,	&Qsemi_expanded},
  {"ultracondensed",	XLFD_SWIDTH_ULTRA_CONDENSED,	&Qultra_condensed},
  {"ultraexpanded",	XLFD_SWIDTH_ULTRA_EXPANDED,	&Qultra_expanded},
  {"wide",		XLFD_SWIDTH_EXTRA_EXPANDED,	&Qextra_expanded}
};

/* Structure used to hold the result of splitting font names in XLFD
   format into their fields.  */

struct font_name
{
  /* The original name which is modified destructively by
     split_font_name.  The pointer is kept here to be able to free it
     if it was allocated from the heap.  */
  char *name;

  /* Font name fields.  Each vector element points into `name' above.
     Fields are NUL-terminated.  */
  char *fields[XLFD_LAST];

  /* Numeric values for those fields that interest us.  See
     split_font_name for which these are.  */
  int numeric[XLFD_LAST];

  /* Lower value mean higher priority.  */
  int registry_priority;
};

/* The frame in effect when sorting font names.  Set temporarily in
   sort_fonts so that it is available in font comparison functions.  */

static struct frame *font_frame;

/* Order by which font selection chooses fonts.  The default values
   mean `first, find a best match for the font width, then for the
   font height, then for weight, then for slant.'  This variable can be
   set via set-face-font-sort-order.  */

#ifdef MAC_OS
static int font_sort_order[4] = {
  XLFD_SWIDTH, XLFD_POINT_SIZE, XLFD_WEIGHT, XLFD_SLANT
};
#else
static int font_sort_order[4];
#endif

/* Look up FONT.fields[FIELD_INDEX] in TABLE which has DIM entries.
   TABLE must be sorted by TABLE[i]->name in ascending order.  Value
   is a pointer to the matching table entry or null if no table entry
   matches.  */

static struct table_entry *
xlfd_lookup_field_contents (table, dim, font, field_index)
     struct table_entry *table;
     int dim;
     struct font_name *font;
     int field_index;
{
  /* Function split_font_name converts fields to lower-case, so there
     is no need to use xstrlwr or xstricmp here.  */
  char *s = font->fields[field_index];
  int low, mid, high, cmp;

  low = 0;
  high = dim - 1;

  while (low <= high)
    {
      mid = (low + high) / 2;
      cmp = strcmp (table[mid].name, s);

      if (cmp < 0)
	low = mid + 1;
      else if (cmp > 0)
	high = mid - 1;
      else
	return table + mid;
    }

  return NULL;
}


/* Return a numeric representation for font name field
   FONT.fields[FIELD_INDEX].  The field is looked up in TABLE which
   has DIM entries.  Value is the numeric value found or DFLT if no
   table entry matches.  This function is used to translate weight,
   slant, and swidth names of XLFD font names to numeric values.  */

static INLINE int
xlfd_numeric_value (table, dim, font, field_index, dflt)
     struct table_entry *table;
     int dim;
     struct font_name *font;
     int field_index;
     int dflt;
{
  struct table_entry *p;
  p = xlfd_lookup_field_contents (table, dim, font, field_index);
  return p ? p->numeric : dflt;
}


/* Return a symbolic representation for font name field
   FONT.fields[FIELD_INDEX].  The field is looked up in TABLE which
   has DIM entries.  Value is the symbolic value found or DFLT if no
   table entry matches.  This function is used to translate weight,
   slant, and swidth names of XLFD font names to symbols.  */

static INLINE Lisp_Object
xlfd_symbolic_value (table, dim, font, field_index, dflt)
     struct table_entry *table;
     int dim;
     struct font_name *font;
     int field_index;
     Lisp_Object dflt;
{
  struct table_entry *p;
  p = xlfd_lookup_field_contents (table, dim, font, field_index);
  return p ? *p->symbol : dflt;
}


/* Return a numeric value for the slant of the font given by FONT.  */

static INLINE int
xlfd_numeric_slant (font)
     struct font_name *font;
{
  return xlfd_numeric_value (slant_table, DIM (slant_table),
			     font, XLFD_SLANT, XLFD_SLANT_ROMAN);
}


/* Return a symbol representing the weight of the font given by FONT.  */

static INLINE Lisp_Object
xlfd_symbolic_slant (font)
     struct font_name *font;
{
  return xlfd_symbolic_value (slant_table, DIM (slant_table),
			      font, XLFD_SLANT, Qnormal);
}


/* Return a numeric value for the weight of the font given by FONT.  */

static INLINE int
xlfd_numeric_weight (font)
     struct font_name *font;
{
  return xlfd_numeric_value (weight_table, DIM (weight_table),
			     font, XLFD_WEIGHT, XLFD_WEIGHT_MEDIUM);
}


/* Return a symbol representing the slant of the font given by FONT.  */

static INLINE Lisp_Object
xlfd_symbolic_weight (font)
     struct font_name *font;
{
  return xlfd_symbolic_value (weight_table, DIM (weight_table),
			      font, XLFD_WEIGHT, Qnormal);
}


/* Return a numeric value for the swidth of the font whose XLFD font
   name fields are found in FONT.  */

static INLINE int
xlfd_numeric_swidth (font)
     struct font_name *font;
{
  return xlfd_numeric_value (swidth_table, DIM (swidth_table),
			     font, XLFD_SWIDTH, XLFD_SWIDTH_MEDIUM);
}


/* Return a symbolic value for the swidth of FONT.  */

static INLINE Lisp_Object
xlfd_symbolic_swidth (font)
     struct font_name *font;
{
  return xlfd_symbolic_value (swidth_table, DIM (swidth_table),
			      font, XLFD_SWIDTH, Qnormal);
}


/* Look up the entry of SYMBOL in the vector TABLE which has DIM
   entries.  Value is a pointer to the matching table entry or null if
   no element of TABLE contains SYMBOL.  */

static struct table_entry *
face_value (table, dim, symbol)
     struct table_entry *table;
     int dim;
     Lisp_Object symbol;
{
  int i;

  xassert (SYMBOLP (symbol));

  for (i = 0; i < dim; ++i)
    if (EQ (*table[i].symbol, symbol))
      break;

  return i < dim ? table + i : NULL;
}


/* Return a numeric value for SYMBOL in the vector TABLE which has DIM
   entries.  Value is -1 if SYMBOL is not found in TABLE.  */

static INLINE int
face_numeric_value (table, dim, symbol)
     struct table_entry *table;
     int dim;
     Lisp_Object symbol;
{
  struct table_entry *p = face_value (table, dim, symbol);
  return p ? p->numeric : -1;
}


/* Return a numeric value representing the weight specified by Lisp
   symbol WEIGHT.  Value is one of the enumerators of enum
   xlfd_weight.  */

static INLINE int
face_numeric_weight (weight)
     Lisp_Object weight;
{
  return face_numeric_value (weight_table, DIM (weight_table), weight);
}


/* Return a numeric value representing the slant specified by Lisp
   symbol SLANT.  Value is one of the enumerators of enum xlfd_slant.  */

static INLINE int
face_numeric_slant (slant)
     Lisp_Object slant;
{
  return face_numeric_value (slant_table, DIM (slant_table), slant);
}


/* Return a numeric value representing the swidth specified by Lisp
   symbol WIDTH.  Value is one of the enumerators of enum xlfd_swidth.  */

static int
face_numeric_swidth (width)
     Lisp_Object width;
{
  return face_numeric_value (swidth_table, DIM (swidth_table), width);
}


#ifdef HAVE_WINDOW_SYSTEM

/* Return non-zero if FONT is the name of a fixed-pitch font.  */

static INLINE int
xlfd_fixed_p (font)
     struct font_name *font;
{
  /* Function split_font_name converts fields to lower-case, so there
     is no need to use tolower here.  */
  return *font->fields[XLFD_SPACING] != 'p';
}


/* Return the point size of FONT on frame F, measured in 1/10 pt.

   The actual height of the font when displayed on F depends on the
   resolution of both the font and frame.  For example, a 10pt font
   designed for a 100dpi display will display larger than 10pt on a
   75dpi display.  (It's not unusual to use fonts not designed for the
   display one is using.  For example, some intlfonts are available in
   72dpi versions, only.)

   Value is the real point size of FONT on frame F, or 0 if it cannot
   be determined.  */

static INLINE int
xlfd_point_size (f, font)
     struct frame *f;
     struct font_name *font;
{
  double resy = FRAME_X_DISPLAY_INFO (f)->resy;
  char *pixel_field = font->fields[XLFD_PIXEL_SIZE];
  double pixel;
  int real_pt;

  if (*pixel_field == '[')
    {
      /* The pixel size field is `[A B C D]' which specifies
	 a transformation matrix.

	 A  B  0
	 C  D  0
	 0  0  1

	 by which all glyphs of the font are transformed.  The spec
	 says that s scalar value N for the pixel size is equivalent
	 to A = N * resx/resy, B = C = 0, D = N.  */
      char *start = pixel_field + 1, *end;
      double matrix[4];
      int i;

      for (i = 0; i < 4; ++i)
	{
	  matrix[i] = strtod (start, &end);
	  start = end;
	}

      pixel = matrix[3];
    }
  else
    pixel = atoi (pixel_field);
  
  if (pixel == 0)
    real_pt = 0;
  else
    real_pt = PT_PER_INCH * 10.0 * pixel / resy + 0.5;

  return real_pt;
}


/* Return point size of PIXEL dots while considering Y-resultion (DPI)
   of frame F.  This function is used to guess a point size of font
   when only the pixel height of the font is available.  */

static INLINE int
pixel_point_size (f, pixel)
     struct frame *f;
     int pixel;
{
  double resy = FRAME_X_DISPLAY_INFO (f)->resy;
  double real_pt;
  int int_pt;

  /* As one inch is PT_PER_INCH points, PT_PER_INCH/RESY gives the
     point size of one dot.  */
  real_pt = pixel * PT_PER_INCH / resy;
  int_pt = real_pt + 0.5;

  return int_pt;
}


/* Split XLFD font name FONT->name destructively into NUL-terminated,
   lower-case fields in FONT->fields.  NUMERIC_P non-zero means
   compute numeric values for fields XLFD_POINT_SIZE, XLFD_SWIDTH,
   XLFD_RESY, XLFD_SLANT, and XLFD_WEIGHT in FONT->numeric.  Value is
   zero if the font name doesn't have the format we expect.  The
   expected format is a font name that starts with a `-' and has
   XLFD_LAST fields separated by `-'.  */

static int
split_font_name (f, font, numeric_p)
     struct frame *f;
     struct font_name *font;
     int numeric_p;
{
  int i = 0;
  int success_p;

  if (*font->name == '-')
    {
      char *p = xstrlwr (font->name) + 1;

      while (i < XLFD_LAST)
	{
	  font->fields[i] = p;
	  ++i;

	  /* Pixel and point size may be of the form `[....]'.  For
	     BNF, see XLFD spec, chapter 4.  Negative values are
	     indicated by tilde characters which we replace with
	     `-' characters, here.  */
	  if (*p == '['
	      && (i - 1 == XLFD_PIXEL_SIZE
		  || i - 1 == XLFD_POINT_SIZE))
	    {
	      char *start, *end;
	      int j;
	      
	      for (++p; *p && *p != ']'; ++p)
		if (*p == '~')
		  *p = '-';

	      /* Check that the matrix contains 4 floating point
		 numbers.  */
	      for (j = 0, start = font->fields[i - 1] + 1;
		   j < 4;
		   ++j, start = end)
		if (strtod (start, &end) == 0 && start == end)
		  break;

	      if (j < 4)
		break;
	    }

	  while (*p && *p != '-')
	    ++p;

	  if (*p != '-')
	    break;

	  *p++ = 0;
	}
    }

  success_p = i == XLFD_LAST;

  /* If requested, and font name was in the expected format,
     compute numeric values for some fields.  */
  if (numeric_p && success_p)
    {
      font->numeric[XLFD_POINT_SIZE] = xlfd_point_size (f, font);
      font->numeric[XLFD_RESY] = atoi (font->fields[XLFD_RESY]);
      font->numeric[XLFD_SLANT] = xlfd_numeric_slant (font);
      font->numeric[XLFD_WEIGHT] = xlfd_numeric_weight (font);
      font->numeric[XLFD_SWIDTH] = xlfd_numeric_swidth (font);
      font->numeric[XLFD_AVGWIDTH] = atoi (font->fields[XLFD_AVGWIDTH]);
    }

  /* Initialize it to zero.  It will be overridden by font_list while
     trying alternate registries.  */
  font->registry_priority = 0;

  return success_p;
}


/* Build an XLFD font name from font name fields in FONT.  Value is a
   pointer to the font name, which is allocated via xmalloc.  */

static char *
build_font_name (font)
     struct font_name *font;
{
  int i;
  int size = 100;
  char *font_name = (char *) xmalloc (size);
  int total_length = 0;

  for (i = 0; i < XLFD_LAST; ++i)
    {
      /* Add 1 because of the leading `-'.  */
      int len = strlen (font->fields[i]) + 1;

      /* Reallocate font_name if necessary.  Add 1 for the final
         NUL-byte.  */
      if (total_length + len + 1 >= size)
	{
	  int new_size = max (2 * size, size + len + 1);
	  int sz = new_size * sizeof *font_name;
	  font_name = (char *) xrealloc (font_name, sz);
	  size = new_size;
	}

      font_name[total_length] = '-';
      bcopy (font->fields[i], font_name + total_length + 1, len - 1);
      total_length += len;
    }

  font_name[total_length] = 0;
  return font_name;
}


/* Free an array FONTS of N font_name structures.  This frees FONTS
   itself and all `name' fields in its elements.  */

static INLINE void
free_font_names (fonts, n)
     struct font_name *fonts;
     int n;
{
  while (n)
    xfree (fonts[--n].name);
  xfree (fonts);
}


/* Sort vector FONTS of font_name structures which contains NFONTS
   elements using qsort and comparison function CMPFN.  F is the frame
   on which the fonts will be used.  The global variable font_frame
   is temporarily set to F to make it available in CMPFN.  */

static INLINE void
sort_fonts (f, fonts, nfonts, cmpfn)
     struct frame *f;
     struct font_name *fonts;
     int nfonts;
     int (*cmpfn) P_ ((const void *, const void *));
{
  font_frame = f;
  qsort (fonts, nfonts, sizeof *fonts, cmpfn);
  font_frame = NULL;
}


/* Get fonts matching PATTERN on frame F.  If F is null, use the first
   display in x_display_list.  FONTS is a pointer to a vector of
   NFONTS font_name structures.  TRY_ALTERNATIVES_P non-zero means try
   alternative patterns from Valternate_fontname_alist if no fonts are
   found matching PATTERN.

   For all fonts found, set FONTS[i].name to the name of the font,
   allocated via xmalloc, and split font names into fields.  Ignore
   fonts that we can't parse.  Value is the number of fonts found.  */

static int
x_face_list_fonts (f, pattern, fonts, nfonts, try_alternatives_p)
     struct frame *f;
     char *pattern;
     struct font_name *fonts;
     int nfonts, try_alternatives_p;
{
  int n, nignored;

  /* NTEMACS_TODO : currently this uses w32_list_fonts, but it may be
     better to do it the other way around. */
  Lisp_Object lfonts;
  Lisp_Object lpattern, tem;

  lpattern = build_string (pattern);

  /* Get the list of fonts matching PATTERN.  */
#ifdef WINDOWSNT
  BLOCK_INPUT;
  lfonts = w32_list_fonts (f, lpattern, 0, nfonts);
  UNBLOCK_INPUT;
#else
  lfonts = x_list_fonts (f, lpattern, -1, nfonts);
#endif

  /* Make a copy of the font names we got from X, and
     split them into fields.  */
  n = nignored = 0;
  for (tem = lfonts; CONSP (tem) && n < nfonts; tem = XCDR (tem))
    {
      Lisp_Object elt, tail;
      char *name = XSTRING (XCAR (tem))->data;

      /* Ignore fonts matching a pattern from face-ignored-fonts.  */
      for (tail = Vface_ignored_fonts; CONSP (tail); tail = XCDR (tail))
	{
	  elt = XCAR (tail);
	  if (STRINGP (elt)
	      && fast_c_string_match_ignore_case (elt, name) >= 0)
	    break;
	}
      if (!NILP (tail))
	{
	  ++nignored;
	  continue;
	}

      /* Make a copy of the font name.  */
      fonts[n].name = xstrdup (name);

      if (split_font_name (f, fonts + n, 1))
	{
	  if (font_scalable_p (fonts + n)
	      && !may_use_scalable_font_p (name))
	    {
	      ++nignored;
	      xfree (fonts[n].name);
	    }
	  else
	    ++n;
	}
      else
	xfree (fonts[n].name);
    }

  /* If no fonts found, try patterns from Valternate_fontname_alist.  */
  if (n == 0 && try_alternatives_p)
    {
      Lisp_Object list = Valternate_fontname_alist;

      while (CONSP (list))
	{
	  Lisp_Object entry = XCAR (list);
	  if (CONSP (entry)
	      && STRINGP (XCAR (entry))
	      && strcmp (XSTRING (XCAR (entry))->data, pattern) == 0)
	    break;
	  list = XCDR (list);
	}

      if (CONSP (list))
	{
	  Lisp_Object patterns = XCAR (list);
	  Lisp_Object name;

	  while (CONSP (patterns)
		 /* If list is screwed up, give up.  */
		 && (name = XCAR (patterns),
		     STRINGP (name))
		 /* Ignore patterns equal to PATTERN because we tried that
		    already with no success.  */
		 && (strcmp (XSTRING (name)->data, pattern) == 0
		     || (n = x_face_list_fonts (f, XSTRING (name)->data,
						fonts, nfonts, 0),
			 n == 0)))
	    patterns = XCDR (patterns);
	}
    }

  return n;
}


/* Determine fonts matching PATTERN on frame F.  Sort resulting fonts
   using comparison function CMPFN.  Value is the number of fonts
   found.  If value is non-zero, *FONTS is set to a vector of
   font_name structures allocated from the heap containing matching
   fonts.  Each element of *FONTS contains a name member that is also
   allocated from the heap.  Font names in these structures are split
   into fields.  Use free_font_names to free such an array.  */

static int
sorted_font_list (f, pattern, cmpfn, fonts)
     struct frame *f;
     char *pattern;
     int (*cmpfn) P_ ((const void *, const void *));
     struct font_name **fonts;
{
  int nfonts;

  /* Get the list of fonts matching pattern.  100 should suffice.  */
  nfonts = DEFAULT_FONT_LIST_LIMIT;
  if (INTEGERP (Vfont_list_limit) && XINT (Vfont_list_limit) > 0)
    nfonts = XFASTINT (Vfont_list_limit);

  *fonts = (struct font_name *) xmalloc (nfonts * sizeof **fonts);
  nfonts = x_face_list_fonts (f, pattern, *fonts, nfonts, 1);

  /* Sort the resulting array and return it in *FONTS.  If no
     fonts were found, make sure to set *FONTS to null.  */
  if (nfonts)
    sort_fonts (f, *fonts, nfonts, cmpfn);
  else
    {
      xfree (*fonts);
      *fonts = NULL;
    }

  return nfonts;
}


/* Compare two font_name structures *A and *B.  Value is analogous to
   strcmp.  Sort order is given by the global variable
   font_sort_order.  Font names are sorted so that, everything else
   being equal, fonts with a resolution closer to that of the frame on
   which they are used are listed first.  The global variable
   font_frame is the frame on which we operate.  */

static int
cmp_font_names (a, b)
     const void *a, *b;
{
  struct font_name *x = (struct font_name *) a;
  struct font_name *y = (struct font_name *) b;
  int cmp;

  /* All strings have been converted to lower-case by split_font_name,
     so we can use strcmp here.  */
  cmp = strcmp (x->fields[XLFD_FAMILY], y->fields[XLFD_FAMILY]);
  if (cmp == 0)
    {
      int i;

      for (i = 0; i < DIM (font_sort_order) && cmp == 0; ++i)
	{
	  int j = font_sort_order[i];
	  cmp = x->numeric[j] - y->numeric[j];
	}

      if (cmp == 0)
	{
	  /* Everything else being equal, we prefer fonts with an
	     y-resolution closer to that of the frame.  */
	  int resy = FRAME_X_DISPLAY_INFO (font_frame)->resy;
	  int x_resy = x->numeric[XLFD_RESY];
	  int y_resy = y->numeric[XLFD_RESY];
	  cmp = abs (resy - x_resy) - abs (resy - y_resy);
	}
    }

  return cmp;
}


/* Get a sorted list of fonts of family FAMILY on frame F.  If PATTERN
   is non-nil list fonts matching that pattern.  Otherwise, if
   REGISTRY is non-nil return only fonts with that registry, otherwise
   return fonts of any registry.  Set *FONTS to a vector of font_name
   structures allocated from the heap containing the fonts found.
   Value is the number of fonts found.  */

static int
font_list_1 (f, pattern, family, registry, fonts)
     struct frame *f;
     Lisp_Object pattern, family, registry;
     struct font_name **fonts;
{
  char *pattern_str, *family_str, *registry_str;

  if (NILP (pattern))
    {
      family_str = (NILP (family) ? "*" : (char *) XSTRING (family)->data);
      registry_str = (NILP (registry) ? "*" : (char *) XSTRING (registry)->data);

      pattern_str = (char *) alloca (strlen (family_str)
				     + strlen (registry_str)
				     + 10);
      strcpy (pattern_str, index (family_str, '-') ? "-" : "-*-");
      strcat (pattern_str, family_str);
      strcat (pattern_str, "-*-");
      strcat (pattern_str, registry_str);
      if (!index (registry_str, '-'))
	{
	  if (registry_str[strlen (registry_str) - 1] == '*')
	    strcat (pattern_str, "-*");
	  else
	    strcat (pattern_str, "*-*");
	}
    }
  else
    pattern_str = (char *) XSTRING (pattern)->data;

  return sorted_font_list (f, pattern_str, cmp_font_names, fonts);
}


/* Concatenate font list FONTS1 and FONTS2.  FONTS1 and FONTS2
   contains NFONTS1 fonts and NFONTS2 fonts respectively.  Return a
   pointer to a newly allocated font list.  FONTS1 and FONTS2 are
   freed.  */

static struct font_name *
concat_font_list (fonts1, nfonts1, fonts2, nfonts2)
     struct font_name *fonts1, *fonts2;
     int nfonts1, nfonts2;
{
  int new_nfonts = nfonts1 + nfonts2;
  struct font_name *new_fonts;

  new_fonts = (struct font_name *) xmalloc (sizeof *new_fonts * new_nfonts);
  bcopy (fonts1, new_fonts, sizeof *new_fonts * nfonts1);
  bcopy (fonts2, new_fonts + nfonts1, sizeof *new_fonts * nfonts2);
  xfree (fonts1);
  xfree (fonts2);
  return new_fonts;
}


/* Get a sorted list of fonts of family FAMILY on frame F.

   If PATTERN is non-nil list fonts matching that pattern.

   If REGISTRY is non-nil, return fonts with that registry and the
   alternative registries from Vface_alternative_font_registry_alist.
   
   If REGISTRY is nil return fonts of any registry.

   Set *FONTS to a vector of font_name structures allocated from the
   heap containing the fonts found.  Value is the number of fonts
   found.  */

static int
font_list (f, pattern, family, registry, fonts)
     struct frame *f;
     Lisp_Object pattern, family, registry;
     struct font_name **fonts;
{
  int nfonts = font_list_1 (f, pattern, family, registry, fonts);
  
  if (!NILP (registry)
      && CONSP (Vface_alternative_font_registry_alist))
    {
      Lisp_Object alter;

      alter = Fassoc (registry, Vface_alternative_font_registry_alist);
      if (CONSP (alter))
	{
	  int reg_prio, i;

	  for (alter = XCDR (alter), reg_prio = 1;
	       CONSP (alter);
	       alter = XCDR (alter), reg_prio++)
	    if (STRINGP (XCAR (alter)))
	      {
		int nfonts2;
		struct font_name *fonts2;

		nfonts2 = font_list_1 (f, pattern, family, XCAR (alter),
				       &fonts2);
		for (i = 0; i < nfonts2; i++)
		  fonts2[i].registry_priority = reg_prio;
		*fonts = (nfonts > 0
			  ? concat_font_list (*fonts, nfonts, fonts2, nfonts2)
			  : fonts2);
		nfonts += nfonts2;
	      }
	}
    }

  return nfonts;
}


/* Remove elements from LIST whose cars are `equal'.  Called from
   x-family-fonts and x-font-family-list to remove duplicate font
   entries.  */

static void
remove_duplicates (list)
     Lisp_Object list;
{
  Lisp_Object tail = list;

  while (!NILP (tail) && !NILP (XCDR (tail)))
    {
      Lisp_Object next = XCDR (tail);
      if (!NILP (Fequal (XCAR (next), XCAR (tail))))
	XSETCDR (tail, XCDR (next));
      else
	tail = XCDR (tail);
    }
}


DEFUN ("x-family-fonts", Fx_family_fonts, Sx_family_fonts, 0, 2, 0,
       doc: /* Return a list of available fonts of family FAMILY on FRAME.
If FAMILY is omitted or nil, list all families.
Otherwise, FAMILY must be a string, possibly containing wildcards
`?' and `*'.
If FRAME is omitted or nil, use the selected frame.
Each element of the result is a vector [FAMILY WIDTH POINT-SIZE WEIGHT
SLANT FIXED-P FULL REGISTRY-AND-ENCODING].
FAMILY is the font family name.  POINT-SIZE is the size of the
font in 1/10 pt.  WIDTH, WEIGHT, and SLANT are symbols describing the
width, weight and slant of the font.  These symbols are the same as for
face attributes.  FIXED-P is non-nil if the font is fixed-pitch.
FULL is the full name of the font, and REGISTRY-AND-ENCODING is a string
giving the registry and encoding of the font.
The result list is sorted according to the current setting of
the face font sort order.  */)
     (family, frame)
     Lisp_Object family, frame;
{
  struct frame *f = check_x_frame (frame);
  struct font_name *fonts;
  int i, nfonts;
  Lisp_Object result;
  struct gcpro gcpro1;

  if (!NILP (family))
    CHECK_STRING (family);

  result = Qnil;
  GCPRO1 (result);
  nfonts = font_list (f, Qnil, family, Qnil, &fonts);
  for (i = nfonts - 1; i >= 0; --i)
    {
      Lisp_Object v = Fmake_vector (make_number (8), Qnil);
      char *tem;

      ASET (v, 0, build_string (fonts[i].fields[XLFD_FAMILY]));
      ASET (v, 1, xlfd_symbolic_swidth (fonts + i));
      ASET (v, 2, make_number (xlfd_point_size (f, fonts + i)));
      ASET (v, 3, xlfd_symbolic_weight (fonts + i));
      ASET (v, 4, xlfd_symbolic_slant (fonts + i));
      ASET (v, 5, xlfd_fixed_p (fonts + i) ? Qt : Qnil);
      tem = build_font_name (fonts + i);
      ASET (v, 6, build_string (tem));
      sprintf (tem, "%s-%s", fonts[i].fields[XLFD_REGISTRY],
	       fonts[i].fields[XLFD_ENCODING]);
      ASET (v, 7, build_string (tem));
      xfree (tem);

      result = Fcons (v, result);
    }

  remove_duplicates (result);
  free_font_names (fonts, nfonts);
  UNGCPRO;
  return result;
}


DEFUN ("x-font-family-list", Fx_font_family_list, Sx_font_family_list,
       0, 1, 0,
       doc: /* Return a list of available font families on FRAME.
If FRAME is omitted or nil, use the selected frame.
Value is a list of conses (FAMILY . FIXED-P) where FAMILY
is a font family, and FIXED-P is non-nil if fonts of that family
are fixed-pitch.  */)
     (frame)
     Lisp_Object frame;
{
  struct frame *f = check_x_frame (frame);
  int nfonts, i;
  struct font_name *fonts;
  Lisp_Object result;
  struct gcpro gcpro1;
  int count = specpdl_ptr - specpdl;
  int limit;

  /* Let's consider all fonts.  Increase the limit for matching
     fonts until we have them all.  */
  for (limit = 500;;)
    {
      specbind (intern ("font-list-limit"), make_number (limit));
      nfonts = font_list (f, Qnil, Qnil, Qnil, &fonts);

      if (nfonts == limit)
	{
	  free_font_names (fonts, nfonts);
	  limit *= 2;
	}
      else
	break;
    }

  result = Qnil;
  GCPRO1 (result);
  for (i = nfonts - 1; i >= 0; --i)
    result = Fcons (Fcons (build_string (fonts[i].fields[XLFD_FAMILY]),
			   xlfd_fixed_p (fonts + i) ? Qt : Qnil),
		    result);

  remove_duplicates (result);
  free_font_names (fonts, nfonts);
  UNGCPRO;
  return unbind_to (count, result);
}


DEFUN ("x-list-fonts", Fx_list_fonts, Sx_list_fonts, 1, 5, 0,
       doc: /* Return a list of the names of available fonts matching PATTERN.
If optional arguments FACE and FRAME are specified, return only fonts
the same size as FACE on FRAME.
PATTERN is a string, perhaps with wildcard characters;
  the * character matches any substring, and
  the ? character matches any single character.
  PATTERN is case-insensitive.
FACE is a face name--a symbol.

The return value is a list of strings, suitable as arguments to
set-face-font.

Fonts Emacs can't use may or may not be excluded
even if they match PATTERN and FACE.
The optional fourth argument MAXIMUM sets a limit on how many
fonts to match.  The first MAXIMUM fonts are reported.
The optional fifth argument WIDTH, if specified, is a number of columns
occupied by a character of a font.  In that case, return only fonts
the WIDTH times as wide as FACE on FRAME.  */)
     (pattern, face, frame, maximum, width)
    Lisp_Object pattern, face, frame, maximum, width;
{
  struct frame *f;
  int size;
  int maxnames;

  check_x ();
  CHECK_STRING (pattern);

  if (NILP (maximum))
    maxnames = 2000;
  else
    {
      CHECK_NATNUM (maximum);
      maxnames = XINT (maximum);
    }

  if (!NILP (width))
    CHECK_NUMBER (width);

  /* We can't simply call check_x_frame because this function may be
     called before any frame is created.  */
  f = frame_or_selected_frame (frame, 2);
  if (!FRAME_WINDOW_P (f))
    {
      /* Perhaps we have not yet created any frame.  */
      f = NULL;
      face = Qnil;
    }

  /* Determine the width standard for comparison with the fonts we find.  */

  if (NILP (face))
    size = 0;
  else
    {
      /* This is of limited utility since it works with character
	 widths.  Keep it for compatibility.  --gerd.  */
      int face_id = lookup_named_face (f, face, 0);
      struct face *face = (face_id < 0
			   ? NULL
			   : FACE_FROM_ID (f, face_id));

      if (face && face->font)
	size = FONT_WIDTH (face->font);
      else
	size = FONT_WIDTH (FRAME_FONT (f));

      if (!NILP (width))
	size *= XINT (width);
    }

  {
    Lisp_Object args[2];

    args[0] = x_list_fonts (f, pattern, size, maxnames);
    if (f == NULL)
      /* We don't have to check fontsets.  */
      return args[0];
    args[1] = list_fontsets (f, pattern, size);
    return Fnconc (2, args);
  }
}

#endif /* HAVE_WINDOW_SYSTEM */



/***********************************************************************
			      Lisp Faces
 ***********************************************************************/

/* Access face attributes of face LFACE, a Lisp vector.  */

#define LFACE_FAMILY(LFACE)	    AREF ((LFACE), LFACE_FAMILY_INDEX)
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
#define LFACE_AVGWIDTH(LFACE)	    AREF ((LFACE), LFACE_AVGWIDTH_INDEX)

/* Non-zero if LFACE is a Lisp face.  A Lisp face is a vector of size
   LFACE_VECTOR_SIZE which has the symbol `face' in slot 0.  */

#define LFACEP(LFACE)					\
     (VECTORP (LFACE)					\
      && XVECTOR (LFACE)->size == LFACE_VECTOR_SIZE	\
      && EQ (AREF (LFACE, 0), Qface))


#if GLYPH_DEBUG

/* Check consistency of Lisp face attribute vector ATTRS.  */

static void
check_lface_attrs (attrs)
     Lisp_Object *attrs;
{
  xassert (UNSPECIFIEDP (attrs[LFACE_FAMILY_INDEX])
	   || STRINGP (attrs[LFACE_FAMILY_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_SWIDTH_INDEX])
	   || SYMBOLP (attrs[LFACE_SWIDTH_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_AVGWIDTH_INDEX])
	   || INTEGERP (attrs[LFACE_AVGWIDTH_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_HEIGHT_INDEX])
	   || INTEGERP (attrs[LFACE_HEIGHT_INDEX])
	   || FLOATP (attrs[LFACE_HEIGHT_INDEX])
	   || FUNCTIONP (attrs[LFACE_HEIGHT_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_WEIGHT_INDEX])
	   || SYMBOLP (attrs[LFACE_WEIGHT_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_SLANT_INDEX])
	   || SYMBOLP (attrs[LFACE_SLANT_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_UNDERLINE_INDEX])
	   || SYMBOLP (attrs[LFACE_UNDERLINE_INDEX])
	   || STRINGP (attrs[LFACE_UNDERLINE_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_OVERLINE_INDEX])
	   || SYMBOLP (attrs[LFACE_OVERLINE_INDEX])
	   || STRINGP (attrs[LFACE_OVERLINE_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_STRIKE_THROUGH_INDEX])
	   || SYMBOLP (attrs[LFACE_STRIKE_THROUGH_INDEX])
	   || STRINGP (attrs[LFACE_STRIKE_THROUGH_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_BOX_INDEX])
	   || SYMBOLP (attrs[LFACE_BOX_INDEX])
	   || STRINGP (attrs[LFACE_BOX_INDEX])
	   || INTEGERP (attrs[LFACE_BOX_INDEX])
	   || CONSP (attrs[LFACE_BOX_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_INVERSE_INDEX])
	   || SYMBOLP (attrs[LFACE_INVERSE_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_FOREGROUND_INDEX])
	   || STRINGP (attrs[LFACE_FOREGROUND_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_BACKGROUND_INDEX])
	   || STRINGP (attrs[LFACE_BACKGROUND_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_INHERIT_INDEX])
	   || NILP (attrs[LFACE_INHERIT_INDEX])
	   || SYMBOLP (attrs[LFACE_INHERIT_INDEX])
	   || CONSP (attrs[LFACE_INHERIT_INDEX]));
#ifdef HAVE_WINDOW_SYSTEM
  xassert (UNSPECIFIEDP (attrs[LFACE_STIPPLE_INDEX])
	   || SYMBOLP (attrs[LFACE_STIPPLE_INDEX])
	   || !NILP (Fbitmap_spec_p (attrs[LFACE_STIPPLE_INDEX])));
  xassert (UNSPECIFIEDP (attrs[LFACE_FONT_INDEX])
	   || NILP (attrs[LFACE_FONT_INDEX])
	   || STRINGP (attrs[LFACE_FONT_INDEX]));
#endif
}


/* Check consistency of attributes of Lisp face LFACE (a Lisp vector).  */

static void
check_lface (lface)
     Lisp_Object lface;
{
  if (!NILP (lface))
    {
      xassert (LFACEP (lface));
      check_lface_attrs (XVECTOR (lface)->contents);
    }
}

#else /* GLYPH_DEBUG == 0 */

#define check_lface_attrs(attrs)	(void) 0
#define check_lface(lface)		(void) 0

#endif /* GLYPH_DEBUG == 0 */


/* Resolve face name FACE_NAME.  If FACE_NAME is a string, intern it
   to make it a symvol.  If FACE_NAME is an alias for another face,
   return that face's name.  */

static Lisp_Object
resolve_face_name (face_name)
     Lisp_Object face_name;
{
  Lisp_Object aliased;

  if (STRINGP (face_name))
    face_name = intern (XSTRING (face_name)->data);

  while (SYMBOLP (face_name))
    {
      aliased = Fget (face_name, Qface_alias);
      if (NILP (aliased))
	break;
      else
	face_name = aliased;
    }

  return face_name;
}


/* Return the face definition of FACE_NAME on frame F.  F null means
   return the definition for new frames.  FACE_NAME may be a string or
   a symbol (apparently Emacs 20.2 allowed strings as face names in
   face text properties; Ediff uses that).  If FACE_NAME is an alias
   for another face, return that face's definition.  If SIGNAL_P is
   non-zero, signal an error if FACE_NAME is not a valid face name.
   If SIGNAL_P is zero, value is nil if FACE_NAME is not a valid face
   name.  */

static INLINE Lisp_Object
lface_from_face_name (f, face_name, signal_p)
     struct frame *f;
     Lisp_Object face_name;
     int signal_p;
{
  Lisp_Object lface;

  face_name = resolve_face_name (face_name);

  if (f)
    lface = assq_no_quit (face_name, f->face_alist);
  else
    lface = assq_no_quit (face_name, Vface_new_frame_defaults);

  if (CONSP (lface))
    lface = XCDR (lface);
  else if (signal_p)
    signal_error ("Invalid face", face_name);

  check_lface (lface);
  return lface;
}


/* Get face attributes of face FACE_NAME from frame-local faces on
   frame F.  Store the resulting attributes in ATTRS which must point
   to a vector of Lisp_Objects of size LFACE_VECTOR_SIZE.  If SIGNAL_P
   is non-zero, signal an error if FACE_NAME does not name a face.
   Otherwise, value is zero if FACE_NAME is not a face.  */

static INLINE int
get_lface_attributes (f, face_name, attrs, signal_p)
     struct frame *f;
     Lisp_Object face_name;
     Lisp_Object *attrs;
     int signal_p;
{
  Lisp_Object lface;
  int success_p;

  lface = lface_from_face_name (f, face_name, signal_p);
  if (!NILP (lface))
    {
      bcopy (XVECTOR (lface)->contents, attrs,
	     LFACE_VECTOR_SIZE * sizeof *attrs);
      success_p = 1;
    }
  else
    success_p = 0;

  return success_p;
}


/* Non-zero if all attributes in face attribute vector ATTRS are
   specified, i.e. are non-nil.  */

static int
lface_fully_specified_p (attrs)
     Lisp_Object *attrs;
{
  int i;

  for (i = 1; i < LFACE_VECTOR_SIZE; ++i)
    if (i != LFACE_FONT_INDEX && i != LFACE_INHERIT_INDEX
	&& i != LFACE_AVGWIDTH_INDEX)
      if (UNSPECIFIEDP (attrs[i])) 
        break;

  return i == LFACE_VECTOR_SIZE;
}

#ifdef HAVE_WINDOW_SYSTEM

/* Set font-related attributes of Lisp face LFACE from the fullname of
   the font opened by FONTNAME.  If FORCE_P is zero, set only
   unspecified attributes of LFACE.  The exception is `font'
   attribute.  It is set to FONTNAME as is regardless of FORCE_P.

   If FONTNAME is not available on frame F,
	return 0 if MAY_FAIL_P is non-zero, otherwise abort.
   If the fullname is not in a valid XLFD format,
   	return 0 if MAY_FAIL_P is non-zero, otherwise set normal values
	in LFACE and return 1.
   Otherwise, return 1.  */

static int
set_lface_from_font_name (f, lface, fontname, force_p, may_fail_p)
     struct frame *f;
     Lisp_Object lface;
     Lisp_Object fontname;
     int force_p, may_fail_p;
{
  struct font_name font;
  char *buffer;
  int pt;
  int have_xlfd_p;
  int fontset;
  char *font_name = XSTRING (fontname)->data;
  struct font_info *font_info;

  /* If FONTNAME is actually a fontset name, get ASCII font name of it.  */
  fontset = fs_query_fontset (fontname, 0);
  if (fontset >= 0)
    font_name = XSTRING (fontset_ascii (fontset))->data;

  /* Check if FONT_NAME is surely available on the system.  Usually
     FONT_NAME is already cached for the frame F and FS_LOAD_FONT
     returns quickly.  But, even if FONT_NAME is not yet cached,
     caching it now is not futail because we anyway load the font
     later.  */
  BLOCK_INPUT;
  font_info = FS_LOAD_FONT (f, 0, font_name, -1);
  UNBLOCK_INPUT;

  if (!font_info)
    {
      if (may_fail_p)
	return 0;
      abort ();
    }

  font.name = STRDUPA (font_info->full_name);
  have_xlfd_p = split_font_name (f, &font, 1);

  /* Set attributes only if unspecified, otherwise face defaults for
     new frames would never take effect.  If we couldn't get a font
     name conforming to XLFD, set normal values.  */

  if (force_p || UNSPECIFIEDP (LFACE_FAMILY (lface)))
    {
      Lisp_Object val;
      if (have_xlfd_p)
	{
	  buffer = (char *) alloca (strlen (font.fields[XLFD_FAMILY])
				    + strlen (font.fields[XLFD_FOUNDRY])
				    + 2);
	  sprintf (buffer, "%s-%s", font.fields[XLFD_FOUNDRY],
		   font.fields[XLFD_FAMILY]);
	  val = build_string (buffer);
	}
      else
	val = build_string ("*");
      LFACE_FAMILY (lface) = val;
    }

  if (force_p || UNSPECIFIEDP (LFACE_HEIGHT (lface)))
    {
      if (have_xlfd_p)
	pt = xlfd_point_size (f, &font);
      else
	pt = pixel_point_size (f, font_info->height * 10);
      xassert (pt > 0);
      LFACE_HEIGHT (lface) = make_number (pt);
    }

  if (force_p || UNSPECIFIEDP (LFACE_SWIDTH (lface)))
    LFACE_SWIDTH (lface)
      = have_xlfd_p ? xlfd_symbolic_swidth (&font) : Qnormal;

  if (force_p || UNSPECIFIEDP (LFACE_AVGWIDTH (lface)))
    LFACE_AVGWIDTH (lface)
      = (have_xlfd_p
	 ? make_number (font.numeric[XLFD_AVGWIDTH])
	 : Qunspecified);

  if (force_p || UNSPECIFIEDP (LFACE_WEIGHT (lface)))
    LFACE_WEIGHT (lface)
      = have_xlfd_p ? xlfd_symbolic_weight (&font) : Qnormal;

  if (force_p || UNSPECIFIEDP (LFACE_SLANT (lface)))
    LFACE_SLANT (lface)
      = have_xlfd_p ? xlfd_symbolic_slant (&font) : Qnormal;

  LFACE_FONT (lface) = fontname;

  return 1;
}

#endif /* HAVE_WINDOW_SYSTEM */


/* Merges the face height FROM with the face height TO, and returns the
   merged height.  If FROM is an invalid height, then INVALID is
   returned instead.  FROM and TO may be either absolute face heights or
   `relative' heights; the returned value is always an absolute height
   unless both FROM and TO are relative.  GCPRO is a lisp value that
   will be protected from garbage-collection if this function makes a
   call into lisp.  */

Lisp_Object
merge_face_heights (from, to, invalid, gcpro)
     Lisp_Object from, to, invalid, gcpro;
{
  Lisp_Object result = invalid;

  if (INTEGERP (from))
    /* FROM is absolute, just use it as is.  */
    result = from;
  else if (FLOATP (from))
    /* FROM is a scale, use it to adjust TO.  */
    {
      if (INTEGERP (to))
	/* relative X absolute => absolute */
	result = make_number ((EMACS_INT)(XFLOAT_DATA (from) * XINT (to)));
      else if (FLOATP (to))
	/* relative X relative => relative */
	result = make_float (XFLOAT_DATA (from) * XFLOAT_DATA (to));
    }
  else if (FUNCTIONP (from))
    /* FROM is a function, which use to adjust TO.  */
    {
      /* Call function with current height as argument.
	 From is the new height.  */
      Lisp_Object args[2];
      struct gcpro gcpro1;

      GCPRO1 (gcpro);

      args[0] = from;
      args[1] = to;
      result = safe_call (2, args);

      UNGCPRO;

      /* Ensure that if TO was absolute, so is the result.  */
      if (INTEGERP (to) && !INTEGERP (result))
	result = invalid;
    }

  return result;
}


/* Merge two Lisp face attribute vectors on frame F, FROM and TO, and
   store the resulting attributes in TO, which must be already be
   completely specified and contain only absolute attributes.  Every
   specified attribute of FROM overrides the corresponding attribute of
   TO; relative attributes in FROM are merged with the absolute value in
   TO and replace it.  CYCLE_CHECK is used internally to detect loops in
   face inheritance; it should be Qnil when called from other places.  */

static INLINE void
merge_face_vectors (f, from, to, cycle_check)
     struct frame *f;
     Lisp_Object *from, *to;
     Lisp_Object cycle_check;
{
  int i;

  /* If FROM inherits from some other faces, merge their attributes into
     TO before merging FROM's direct attributes.  Note that an :inherit
     attribute of `unspecified' is the same as one of nil; we never
     merge :inherit attributes, so nil is more correct, but lots of
     other code uses `unspecified' as a generic value for face attributes. */
  if (!UNSPECIFIEDP (from[LFACE_INHERIT_INDEX])
      && !NILP (from[LFACE_INHERIT_INDEX]))
    merge_face_inheritance (f, from[LFACE_INHERIT_INDEX], to, cycle_check);

  /* If TO specifies a :font attribute, and FROM specifies some
     font-related attribute, we need to clear TO's :font attribute
     (because it will be inconsistent with whatever FROM specifies, and
     FROM takes precedence).  */
  if (!NILP (to[LFACE_FONT_INDEX])
      && (!UNSPECIFIEDP (from[LFACE_FAMILY_INDEX])
	  || !UNSPECIFIEDP (from[LFACE_HEIGHT_INDEX])
	  || !UNSPECIFIEDP (from[LFACE_WEIGHT_INDEX])
	  || !UNSPECIFIEDP (from[LFACE_SLANT_INDEX])
	  || !UNSPECIFIEDP (from[LFACE_SWIDTH_INDEX])
	  || !UNSPECIFIEDP (from[LFACE_AVGWIDTH_INDEX])))
    to[LFACE_FONT_INDEX] = Qnil;

  for (i = 1; i < LFACE_VECTOR_SIZE; ++i)
    if (!UNSPECIFIEDP (from[i]))
      {
	if (i == LFACE_HEIGHT_INDEX && !INTEGERP (from[i]))
	  to[i] = merge_face_heights (from[i], to[i], to[i], cycle_check);
	else
	  to[i] = from[i];
      }

  /* TO is always an absolute face, which should inherit from nothing.
     We blindly copy the :inherit attribute above and fix it up here.  */
  to[LFACE_INHERIT_INDEX] = Qnil;
}


/* Checks the `cycle check' variable CHECK to see if it indicates that
   EL is part of a cycle; CHECK must be either Qnil or a value returned
   by an earlier use of CYCLE_CHECK.  SUSPICIOUS is the number of
   elements after which a cycle might be suspected; after that many
   elements, this macro begins consing in order to keep more precise
   track of elements.

   Returns nil if a cycle was detected, otherwise a new value for CHECK
   that includes EL.

   CHECK is evaluated multiple times, EL and SUSPICIOUS 0 or 1 times, so
   the caller should make sure that's ok.  */

#define CYCLE_CHECK(check, el, suspicious)	\
  (NILP (check)					\
   ? make_number (0)				\
   : (INTEGERP (check)				\
      ? (XFASTINT (check) < (suspicious)	\
	 ? make_number (XFASTINT (check) + 1)	\
	 : Fcons (el, Qnil))			\
      : (!NILP (Fmemq ((el), (check)))		\
	 ? Qnil					\
	 : Fcons ((el), (check)))))


/* Merge face attributes from the face on frame F whose name is
   INHERITS, into the vector of face attributes TO; INHERITS may also be
   a list of face names, in which case they are applied in order.
   CYCLE_CHECK is used to detect loops in face inheritance.
   Returns true if any of the inherited attributes are `font-related'.  */

static void
merge_face_inheritance (f, inherit, to, cycle_check)
     struct frame *f;
     Lisp_Object inherit;
     Lisp_Object *to;
     Lisp_Object cycle_check;
{
  if (SYMBOLP (inherit) && !EQ (inherit, Qunspecified))
    /* Inherit from the named face INHERIT.  */
    {
      Lisp_Object lface;

      /* Make sure we're not in an inheritance loop.  */
      cycle_check = CYCLE_CHECK (cycle_check, inherit, 15);
      if (NILP (cycle_check))
	/* Cycle detected, ignore any further inheritance.  */
	return;

      lface = lface_from_face_name (f, inherit, 0);
      if (!NILP (lface))
	merge_face_vectors (f, XVECTOR (lface)->contents, to, cycle_check);
    }
  else if (CONSP (inherit))
    /* Handle a list of inherited faces by calling ourselves recursively
       on each element.  Note that we only do so for symbol elements, so
       it's not possible to infinitely recurse.  */
    {
      while (CONSP (inherit))
	{
	  if (SYMBOLP (XCAR (inherit)))
	    merge_face_inheritance (f, XCAR (inherit), to, cycle_check);

	  /* Check for a circular inheritance list.  */
	  cycle_check = CYCLE_CHECK (cycle_check, inherit, 15);
	  if (NILP (cycle_check))
	    /* Cycle detected.  */
	    break;

	  inherit = XCDR (inherit);
	}
    }
}


/* Given a Lisp face attribute vector TO and a Lisp object PROP that
   is a face property, determine the resulting face attributes on
   frame F, and store them in TO.  PROP may be a single face
   specification or a list of such specifications.  Each face
   specification can be

   1. A symbol or string naming a Lisp face.

   2. A property list of the form (KEYWORD VALUE ...) where each
   KEYWORD is a face attribute name, and value is an appropriate value
   for that attribute.

   3. Conses or the form (FOREGROUND-COLOR . COLOR) or
   (BACKGROUND-COLOR . COLOR) where COLOR is a color name.  This is
   for compatibility with 20.2.

   Face specifications earlier in lists take precedence over later
   specifications.  */

static void
merge_face_vector_with_property (f, to, prop)
     struct frame *f;
     Lisp_Object *to;
     Lisp_Object prop;
{
  if (CONSP (prop))
    {
      Lisp_Object first = XCAR (prop);

      if (EQ (first, Qforeground_color)
	  || EQ (first, Qbackground_color))
	{
	  /* One of (FOREGROUND-COLOR . COLOR) or (BACKGROUND-COLOR
	     . COLOR).  COLOR must be a string.  */
	  Lisp_Object color_name = XCDR (prop);
	  Lisp_Object color = first;

	  if (STRINGP (color_name))
	    {
	      if (EQ (color, Qforeground_color))
		to[LFACE_FOREGROUND_INDEX] = color_name;
	      else
		to[LFACE_BACKGROUND_INDEX] = color_name;
	    }
	  else
	    add_to_log ("Invalid face color", color_name, Qnil);
	}
      else if (SYMBOLP (first)
	       && *XSTRING (SYMBOL_NAME (first))->data == ':')
	{
	  /* Assume this is the property list form.  */
	  while (CONSP (prop) && CONSP (XCDR (prop)))
	    {
	      Lisp_Object keyword = XCAR (prop);
	      Lisp_Object value = XCAR (XCDR (prop));

	      if (EQ (keyword, QCfamily))
		{
		  if (STRINGP (value))
		    to[LFACE_FAMILY_INDEX] = value;
		  else
		    add_to_log ("Invalid face font family", value, Qnil);
		}
	      else if (EQ (keyword, QCheight))
		{
		  Lisp_Object new_height =
		    merge_face_heights (value, to[LFACE_HEIGHT_INDEX],
					Qnil, Qnil);

		  if (NILP (new_height))
		    add_to_log ("Invalid face font height", value, Qnil);
		  else
		    to[LFACE_HEIGHT_INDEX] = new_height;
		}
	      else if (EQ (keyword, QCweight))
		{
		  if (SYMBOLP (value)
		      && face_numeric_weight (value) >= 0)
		    to[LFACE_WEIGHT_INDEX] = value;
		  else
		    add_to_log ("Invalid face weight", value, Qnil);
		}
	      else if (EQ (keyword, QCslant))
		{
		  if (SYMBOLP (value)
		      && face_numeric_slant (value) >= 0)
		    to[LFACE_SLANT_INDEX] = value;
		  else
		    add_to_log ("Invalid face slant", value, Qnil);
		}
	      else if (EQ (keyword, QCunderline))
		{
		  if (EQ (value, Qt)
		      || NILP (value)
		      || STRINGP (value))
		    to[LFACE_UNDERLINE_INDEX] = value;
		  else
		    add_to_log ("Invalid face underline", value, Qnil);
		}
	      else if (EQ (keyword, QCoverline))
		{
		  if (EQ (value, Qt)
		      || NILP (value)
		      || STRINGP (value))
		    to[LFACE_OVERLINE_INDEX] = value;
		  else
		    add_to_log ("Invalid face overline", value, Qnil);
		}
	      else if (EQ (keyword, QCstrike_through))
		{
		  if (EQ (value, Qt)
		      || NILP (value)
		      || STRINGP (value))
		    to[LFACE_STRIKE_THROUGH_INDEX] = value;
		  else
		    add_to_log ("Invalid face strike-through", value, Qnil);
		}
	      else if (EQ (keyword, QCbox))
		{
		  if (EQ (value, Qt))
		    value = make_number (1);
		  if (INTEGERP (value)
		      || STRINGP (value)
		      || CONSP (value)
		      || NILP (value))
		    to[LFACE_BOX_INDEX] = value;
		  else
		    add_to_log ("Invalid face box", value, Qnil);
		}
	      else if (EQ (keyword, QCinverse_video)
		       || EQ (keyword, QCreverse_video))
		{
		  if (EQ (value, Qt) || NILP (value))
		    to[LFACE_INVERSE_INDEX] = value;
		  else
		    add_to_log ("Invalid face inverse-video", value, Qnil);
		}
	      else if (EQ (keyword, QCforeground))
		{
		  if (STRINGP (value))
		    to[LFACE_FOREGROUND_INDEX] = value;
		  else
		    add_to_log ("Invalid face foreground", value, Qnil);
		}
	      else if (EQ (keyword, QCbackground))
		{
		  if (STRINGP (value))
		    to[LFACE_BACKGROUND_INDEX] = value;
		  else
		    add_to_log ("Invalid face background", value, Qnil);
		}
	      else if (EQ (keyword, QCstipple))
		{
#ifdef HAVE_X_WINDOWS
		  Lisp_Object pixmap_p = Fbitmap_spec_p (value);
		  if (!NILP (pixmap_p))
		    to[LFACE_STIPPLE_INDEX] = value;
		  else
		    add_to_log ("Invalid face stipple", value, Qnil);
#endif
		}
	      else if (EQ (keyword, QCwidth))
		{
		  if (SYMBOLP (value)
		      && face_numeric_swidth (value) >= 0)
		    to[LFACE_SWIDTH_INDEX] = value;
		  else
		    add_to_log ("Invalid face width", value, Qnil);
		}
	      else if (EQ (keyword, QCinherit))
		{
		  if (SYMBOLP (value))
		    to[LFACE_INHERIT_INDEX] = value;
		  else
		    {
		      Lisp_Object tail;
		      for (tail = value; CONSP (tail); tail = XCDR (tail))
			if (!SYMBOLP (XCAR (tail)))
			  break;
		      if (NILP (tail))
			to[LFACE_INHERIT_INDEX] = value;
		      else
			add_to_log ("Invalid face inherit", value, Qnil);
		    }
		}
	      else
		add_to_log ("Invalid attribute %s in face property",
			    keyword, Qnil);

	      prop = XCDR (XCDR (prop));
	    }
	}
      else
	{
	  /* This is a list of face specs.  Specifications at the
	     beginning of the list take precedence over later
	     specifications, so we have to merge starting with the
	     last specification.  */
	  Lisp_Object next = XCDR (prop);
	  if (!NILP (next))
	    merge_face_vector_with_property (f, to, next);
	  merge_face_vector_with_property (f, to, first);
	}
    }
  else
    {
      /* PROP ought to be a face name.  */
      Lisp_Object lface = lface_from_face_name (f, prop, 0);
      if (NILP (lface))
	add_to_log ("Invalid face text property value: %s", prop, Qnil);
      else
	merge_face_vectors (f, XVECTOR (lface)->contents, to, Qnil);
    }
}


DEFUN ("internal-make-lisp-face", Finternal_make_lisp_face,
       Sinternal_make_lisp_face, 1, 2, 0,
       doc: /* Make FACE, a symbol, a Lisp face with all attributes nil.
If FACE was not known as a face before, create a new one.
If optional argument FRAME is specified, make a frame-local face
for that frame.  Otherwise operate on the global face definition.
Value is a vector of face attributes.  */)
     (face, frame)
     Lisp_Object face, frame;
{
  Lisp_Object global_lface, lface;
  struct frame *f;
  int i;

  CHECK_SYMBOL (face);
  global_lface = lface_from_face_name (NULL, face, 0);

  if (!NILP (frame))
    {
      CHECK_LIVE_FRAME (frame);
      f = XFRAME (frame);
      lface = lface_from_face_name (f, face, 0);
    }
  else
    f = NULL, lface = Qnil;

  /* Add a global definition if there is none.  */
  if (NILP (global_lface))
    {
      global_lface = Fmake_vector (make_number (LFACE_VECTOR_SIZE),
				   Qunspecified);
      AREF (global_lface, 0) = Qface;
      Vface_new_frame_defaults = Fcons (Fcons (face, global_lface),
					Vface_new_frame_defaults);

      /* Assign the new Lisp face a unique ID.  The mapping from Lisp
	 face id to Lisp face is given by the vector lface_id_to_name.
	 The mapping from Lisp face to Lisp face id is given by the
	 property `face' of the Lisp face name.  */
      if (next_lface_id == lface_id_to_name_size)
	{
	  int new_size = max (50, 2 * lface_id_to_name_size);
	  int sz = new_size * sizeof *lface_id_to_name;
	  lface_id_to_name = (Lisp_Object *) xrealloc (lface_id_to_name, sz);
	  lface_id_to_name_size = new_size;
	}

      lface_id_to_name[next_lface_id] = face;
      Fput (face, Qface, make_number (next_lface_id));
      ++next_lface_id;
    }
  else if (f == NULL)
    for (i = 1; i < LFACE_VECTOR_SIZE; ++i)
      AREF (global_lface, i) = Qunspecified;

  /* Add a frame-local definition.  */
  if (f)
    {
      if (NILP (lface))
	{
	  lface = Fmake_vector (make_number (LFACE_VECTOR_SIZE),
				Qunspecified);
	  AREF (lface, 0) = Qface;
	  f->face_alist = Fcons (Fcons (face, lface), f->face_alist);
	}
      else
	for (i = 1; i < LFACE_VECTOR_SIZE; ++i)
	  AREF (lface, i) = Qunspecified;
    }
  else
    lface = global_lface;

  /* Changing a named face means that all realized faces depending on
     that face are invalid.  Since we cannot tell which realized faces
     depend on the face, make sure they are all removed.  This is done
     by incrementing face_change_count.  The next call to
     init_iterator will then free realized faces.  */
  ++face_change_count;
  ++windows_or_buffers_changed;

  xassert (LFACEP (lface));
  check_lface (lface);
  return lface;
}


DEFUN ("internal-lisp-face-p", Finternal_lisp_face_p,
       Sinternal_lisp_face_p, 1, 2, 0,
       doc: /* Return non-nil if FACE names a face.
If optional second parameter FRAME is non-nil, check for the
existence of a frame-local face with name FACE on that frame.
Otherwise check for the existence of a global face.  */)
     (face, frame)
     Lisp_Object face, frame;
{
  Lisp_Object lface;

  if (!NILP (frame))
    {
      CHECK_LIVE_FRAME (frame);
      lface = lface_from_face_name (XFRAME (frame), face, 0);
    }
  else
    lface = lface_from_face_name (NULL, face, 0);

  return lface;
}


DEFUN ("internal-copy-lisp-face", Finternal_copy_lisp_face,
       Sinternal_copy_lisp_face, 4, 4, 0,
       doc: /* Copy face FROM to TO.
If FRAME it t, copy the global face definition of FROM to the
global face definition of TO.  Otherwise, copy the frame-local
definition of FROM on FRAME to the frame-local definition of TO
on NEW-FRAME, or FRAME if NEW-FRAME is nil.

Value is TO.  */)
     (from, to, frame, new_frame)
     Lisp_Object from, to, frame, new_frame;
{
  Lisp_Object lface, copy;

  CHECK_SYMBOL (from);
  CHECK_SYMBOL (to);
  if (NILP (new_frame))
    new_frame = frame;

  if (EQ (frame, Qt))
    {
      /* Copy global definition of FROM.  We don't make copies of
	 strings etc. because 20.2 didn't do it either.  */
      lface = lface_from_face_name (NULL, from, 1);
      copy = Finternal_make_lisp_face (to, Qnil);
    }
  else
    {
      /* Copy frame-local definition of FROM.  */
      CHECK_LIVE_FRAME (frame);
      CHECK_LIVE_FRAME (new_frame);
      lface = lface_from_face_name (XFRAME (frame), from, 1);
      copy = Finternal_make_lisp_face (to, new_frame);
    }

  bcopy (XVECTOR (lface)->contents, XVECTOR (copy)->contents,
	 LFACE_VECTOR_SIZE * sizeof (Lisp_Object));

  /* Changing a named face means that all realized faces depending on
     that face are invalid.  Since we cannot tell which realized faces
     depend on the face, make sure they are all removed.  This is done
     by incrementing face_change_count.  The next call to
     init_iterator will then free realized faces.  */
  ++face_change_count;
  ++windows_or_buffers_changed;

  return to;
}


DEFUN ("internal-set-lisp-face-attribute", Finternal_set_lisp_face_attribute,
       Sinternal_set_lisp_face_attribute, 3, 4, 0,
       doc: /* Set attribute ATTR of FACE to VALUE.
FRAME being a frame means change the face on that frame.
FRAME nil means change the face of the selected frame.
FRAME t means change the default for new frames.
FRAME 0 means change the face on all frames, and change the default
  for new frames.  */)
     (face, attr, value, frame)
     Lisp_Object face, attr, value, frame;
{
  Lisp_Object lface;
  Lisp_Object old_value = Qnil;
  /* Set 1 if ATTR is QCfont.  */
  int font_attr_p = 0;
  /* Set 1 if ATTR is one of font-related attributes other than QCfont.  */
  int font_related_attr_p = 0;

  CHECK_SYMBOL (face);
  CHECK_SYMBOL (attr);

  face = resolve_face_name (face);

  /* If FRAME is 0, change face on all frames, and change the
     default for new frames.  */
  if (INTEGERP (frame) && XINT (frame) == 0)
    {
      Lisp_Object tail;
      Finternal_set_lisp_face_attribute (face, attr, value, Qt);
      FOR_EACH_FRAME (tail, frame)
	Finternal_set_lisp_face_attribute (face, attr, value, frame);
      return face;
    }

  /* Set lface to the Lisp attribute vector of FACE.  */
  if (EQ (frame, Qt))
    lface = lface_from_face_name (NULL, face, 1);
  else
    {
      if (NILP (frame))
	frame = selected_frame;

      CHECK_LIVE_FRAME (frame);
      lface = lface_from_face_name (XFRAME (frame), face, 0);

      /* If a frame-local face doesn't exist yet, create one.  */
      if (NILP (lface))
	lface = Finternal_make_lisp_face (face, frame);
    }

  if (EQ (attr, QCfamily))
    {
      if (!UNSPECIFIEDP (value))
	{
	  CHECK_STRING (value);
	  if (XSTRING (value)->size == 0)
	    signal_error ("Invalid face family", value);
	}
      old_value = LFACE_FAMILY (lface);
      LFACE_FAMILY (lface) = value;
      font_related_attr_p = 1;
    }
  else if (EQ (attr, QCheight))
    {
      if (!UNSPECIFIEDP (value))
	{
	  Lisp_Object test;

	  test = (EQ (face, Qdefault)
		  ? value
		  /* The default face must have an absolute size,
		     otherwise, we do a test merge with a random
		     height to see if VALUE's ok. */
		  : merge_face_heights (value, make_number (10), Qnil, Qnil));

	  if (!INTEGERP (test) || XINT (test) <= 0)
	    signal_error ("Invalid face height", value);
	}

      old_value = LFACE_HEIGHT (lface);
      LFACE_HEIGHT (lface) = value;
      font_related_attr_p = 1;
    }
  else if (EQ (attr, QCweight))
    {
      if (!UNSPECIFIEDP (value))
	{
	  CHECK_SYMBOL (value);
	  if (face_numeric_weight (value) < 0)
	    signal_error ("Invalid face weight", value);
	}
      old_value = LFACE_WEIGHT (lface);
      LFACE_WEIGHT (lface) = value;
      font_related_attr_p = 1;
    }
  else if (EQ (attr, QCslant))
    {
      if (!UNSPECIFIEDP (value))
	{
	  CHECK_SYMBOL (value);
	  if (face_numeric_slant (value) < 0)
	    signal_error ("Invalid face slant", value);
	}
      old_value = LFACE_SLANT (lface);
      LFACE_SLANT (lface) = value;
      font_related_attr_p = 1;
    }
  else if (EQ (attr, QCunderline))
    {
      if (!UNSPECIFIEDP (value))
	if ((SYMBOLP (value)
	     && !EQ (value, Qt)
	     && !EQ (value, Qnil))
	    /* Underline color.  */
	    || (STRINGP (value)
		&& XSTRING (value)->size == 0))
	  signal_error ("Invalid face underline", value);

      old_value = LFACE_UNDERLINE (lface);
      LFACE_UNDERLINE (lface) = value;
    }
  else if (EQ (attr, QCoverline))
    {
      if (!UNSPECIFIEDP (value))
	if ((SYMBOLP (value)
	     && !EQ (value, Qt)
	     && !EQ (value, Qnil))
	    /* Overline color.  */
	    || (STRINGP (value)
		&& XSTRING (value)->size == 0))
	  signal_error ("Invalid face overline", value);

      old_value = LFACE_OVERLINE (lface);
      LFACE_OVERLINE (lface) = value;
    }
  else if (EQ (attr, QCstrike_through))
    {
      if (!UNSPECIFIEDP (value))
	if ((SYMBOLP (value)
	     && !EQ (value, Qt)
	     && !EQ (value, Qnil))
	    /* Strike-through color.  */
	    || (STRINGP (value)
		&& XSTRING (value)->size == 0))
	  signal_error ("Invalid face strike-through", value);

      old_value = LFACE_STRIKE_THROUGH (lface);
      LFACE_STRIKE_THROUGH (lface) = value;
    }
  else if (EQ (attr, QCbox))
    {
      int valid_p;

      /* Allow t meaning a simple box of width 1 in foreground color
         of the face.  */
      if (EQ (value, Qt))
	value = make_number (1);

      if (UNSPECIFIEDP (value))
	valid_p = 1;
      else if (NILP (value))
	valid_p = 1;
      else if (INTEGERP (value))
	valid_p = XINT (value) != 0;
      else if (STRINGP (value))
	valid_p = XSTRING (value)->size > 0;
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
		  if (!INTEGERP (v) || XINT (v) == 0)
		    break;
		}
	      else if (EQ (k, QCcolor))
		{
		  if (!STRINGP (v) || XSTRING (v)->size == 0)
		    break;
		}
	      else if (EQ (k, QCstyle))
		{
		  if (!EQ (v, Qpressed_button) && !EQ (v, Qreleased_button))
		    break;
		}
	      else
		break;
	    }

	  valid_p = NILP (tem);
	}
      else
	valid_p = 0;

      if (!valid_p)
	signal_error ("Invalid face box", value);

      old_value = LFACE_BOX (lface);
      LFACE_BOX (lface) = value;
    }
  else if (EQ (attr, QCinverse_video)
	   || EQ (attr, QCreverse_video))
    {
      if (!UNSPECIFIEDP (value))
	{
	  CHECK_SYMBOL (value);
	  if (!EQ (value, Qt) && !NILP (value))
	    signal_error ("Invalid inverse-video face attribute value", value);
	}
      old_value = LFACE_INVERSE (lface);
      LFACE_INVERSE (lface) = value;
    }
  else if (EQ (attr, QCforeground))
    {
      if (!UNSPECIFIEDP (value))
	{
	  /* Don't check for valid color names here because it depends
	     on the frame (display) whether the color will be valid
	     when the face is realized.  */
	  CHECK_STRING (value);
	  if (XSTRING (value)->size == 0)
	    signal_error ("Empty foreground color value", value);
	}
      old_value = LFACE_FOREGROUND (lface);
      LFACE_FOREGROUND (lface) = value;
    }
  else if (EQ (attr, QCbackground))
    {
      if (!UNSPECIFIEDP (value))
	{
	  /* Don't check for valid color names here because it depends
	     on the frame (display) whether the color will be valid
	     when the face is realized.  */
	  CHECK_STRING (value);
	  if (XSTRING (value)->size == 0)
	    signal_error ("Empty background color value", value);
	}
      old_value = LFACE_BACKGROUND (lface);
      LFACE_BACKGROUND (lface) = value;
    }
  else if (EQ (attr, QCstipple))
    {
#ifdef HAVE_X_WINDOWS
      if (!UNSPECIFIEDP (value)
	  && !NILP (value)
	  && NILP (Fbitmap_spec_p (value)))
	signal_error ("Invalid stipple attribute", value);
      old_value = LFACE_STIPPLE (lface);
      LFACE_STIPPLE (lface) = value;
#endif /* HAVE_X_WINDOWS */
    }
  else if (EQ (attr, QCwidth))
    {
      if (!UNSPECIFIEDP (value))
	{
	  CHECK_SYMBOL (value);
	  if (face_numeric_swidth (value) < 0)
	    signal_error ("Invalid face width", value);
	}
      old_value = LFACE_SWIDTH (lface);
      LFACE_SWIDTH (lface) = value;
      font_related_attr_p = 1;
    }
  else if (EQ (attr, QCfont))
    {
#ifdef HAVE_WINDOW_SYSTEM
      if (EQ (frame, Qt) || FRAME_WINDOW_P (XFRAME (frame)))
	{
	  /* Set font-related attributes of the Lisp face from an XLFD
	     font name.  */
	  struct frame *f;
	  Lisp_Object tmp;

	  CHECK_STRING (value);
	  if (EQ (frame, Qt))
	    f = SELECTED_FRAME ();
	  else
	    f = check_x_frame (frame);

	  /* VALUE may be a fontset name or an alias of fontset.  In
	     such a case, use the base fontset name.  */
	  tmp = Fquery_fontset (value, Qnil);
	  if (!NILP (tmp))
	    value = tmp;

	  if (!set_lface_from_font_name (f, lface, value, 1, 1))
	    signal_error ("Invalid font or fontset name", value);

	  font_attr_p = 1;
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
	LFACE_INHERIT (lface) = value;
      else
	signal_error ("Invalid face inheritance", value);
    }
  else if (EQ (attr, QCbold))
    {
      old_value = LFACE_WEIGHT (lface);
      LFACE_WEIGHT (lface) = NILP (value) ? Qnormal : Qbold;
      font_related_attr_p = 1;
    }
  else if (EQ (attr, QCitalic))
    {
      old_value = LFACE_SLANT (lface);
      LFACE_SLANT (lface) = NILP (value) ? Qnormal : Qitalic;
      font_related_attr_p = 1;
    }
  else
    signal_error ("Invalid face attribute name", attr);

  if (font_related_attr_p
      && !UNSPECIFIEDP (value))
    /* If a font-related attribute other than QCfont is specified, the
       original `font' attribute nor that of default face is useless
       to determine a new font.  Thus, we set it to nil so that font
       selection mechanism doesn't use it.  */
    LFACE_FONT (lface) = Qnil;

  /* Changing a named face means that all realized faces depending on
     that face are invalid.  Since we cannot tell which realized faces
     depend on the face, make sure they are all removed.  This is done
     by incrementing face_change_count.  The next call to
     init_iterator will then free realized faces.  */
  if (!EQ (frame, Qt)
      && (EQ (attr, QCfont)
	  || NILP (Fequal (old_value, value))))
    {
      ++face_change_count;
      ++windows_or_buffers_changed;
    }

  if (!UNSPECIFIEDP (value)
      && NILP (Fequal (old_value, value)))
    {
      Lisp_Object param;

      param = Qnil;

      if (EQ (face, Qdefault))
	{
#ifdef HAVE_WINDOW_SYSTEM
	  /* Changed font-related attributes of the `default' face are
	     reflected in changed `font' frame parameters. */
	  if (FRAMEP (frame)
	      && (font_related_attr_p || font_attr_p)
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
#ifndef WINDOWSNT
      else if (EQ (face, Qscroll_bar))
	{
	  /* Changing the colors of `scroll-bar' sets frame parameters
	     `scroll-bar-foreground' and `scroll-bar-background'. */
	  if (EQ (attr, QCforeground))
	    param = Qscroll_bar_foreground;
	  else if (EQ (attr, QCbackground))
	    param = Qscroll_bar_background;
	}
#endif /* not WINDOWSNT */
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
	  /* Indicate that we have to update the menu bar when
	     realizing faces on FRAME.  FRAME t change the
	     default for new frames.  We do this by setting
	     setting the flag in new face caches   */
	  if (FRAMEP (frame))
	    {
	      struct frame *f = XFRAME (frame);
	      if (FRAME_FACE_CACHE (f) == NULL)
		FRAME_FACE_CACHE (f) = make_face_cache (f);
	      FRAME_FACE_CACHE (f)->menu_face_changed_p = 1;
	    }
	  else
	    menu_face_changed_default = 1;
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
	      Lisp_Object cons;
	      cons = XCAR (Vparam_value_alist);
	      XSETCAR (cons, param);
	      XSETCDR (cons, value);
	      Fmodify_frame_parameters (frame, Vparam_value_alist);
	    }
	}
    }

  return face;
}


#ifdef HAVE_WINDOW_SYSTEM

/* Set the `font' frame parameter of FRAME determined from `default'
   face attributes LFACE.  If a face or fontset name is explicitely
   specfied in LFACE, use it as is.  Otherwise, determine a font name
   from the other font-related atrributes of LFACE.  In that case, if
   there's no matching font, signals an error.  */

static void
set_font_frame_param (frame, lface)
     Lisp_Object frame, lface;
{
  struct frame *f = XFRAME (frame);

  if (FRAME_WINDOW_P (f))
    {
      Lisp_Object font_name;
      char *font;
      
      if (STRINGP (LFACE_FONT (lface)))
	font_name = LFACE_FONT (lface);
      else
	{
	  /* Choose a font name that reflects LFACE's attributes and has
	     the registry and encoding pattern specified in the default
	     fontset (3rd arg: -1) for ASCII characters (4th arg: 0).  */
	  font = choose_face_font (f, XVECTOR (lface)->contents, -1, 0);
	  if (!font)
	    error ("No font matches the specified attribute");
	  font_name = build_string (font);
	  xfree (font);
	}
  
      Fmodify_frame_parameters (frame, Fcons (Fcons (Qfont, font_name), Qnil));
    }
}


/* Update the corresponding face when frame parameter PARAM on frame F
   has been assigned the value NEW_VALUE.  */

void
update_face_from_frame_parameter (f, param, new_value)
     struct frame *f;
     Lisp_Object param, new_value;
{
  Lisp_Object lface;

  /* If there are no faces yet, give up.  This is the case when called
     from Fx_create_frame, and we do the necessary things later in
     face-set-after-frame-defaults.  */
  if (NILP (f->face_alist))
    return;

  /* Changing a named face means that all realized faces depending on
     that face are invalid.  Since we cannot tell which realized faces
     depend on the face, make sure they are all removed.  This is done
     by incrementing face_change_count.  The next call to
     init_iterator will then free realized faces.  */
  ++face_change_count;
  ++windows_or_buffers_changed;

  if (EQ (param, Qforeground_color))
    {
      lface = lface_from_face_name (f, Qdefault, 1);
      LFACE_FOREGROUND (lface) = (STRINGP (new_value)
				  ? new_value : Qunspecified);
      realize_basic_faces (f);
    }
  else if (EQ (param, Qbackground_color))
    {
      Lisp_Object frame;

      /* Changing the background color might change the background
	 mode, so that we have to load new defface specs.  Call
	 frame-update-face-colors to do that.  */
      XSETFRAME (frame, f);
      call1 (Qframe_update_face_colors, frame);

      lface = lface_from_face_name (f, Qdefault, 1);
      LFACE_BACKGROUND (lface) = (STRINGP (new_value)
				  ? new_value : Qunspecified);
      realize_basic_faces (f);
    }
  if (EQ (param, Qborder_color))
    {
      lface = lface_from_face_name (f, Qborder, 1);
      LFACE_BACKGROUND (lface) = (STRINGP (new_value)
				  ? new_value : Qunspecified);
    }
  else if (EQ (param, Qcursor_color))
    {
      lface = lface_from_face_name (f, Qcursor, 1);
      LFACE_BACKGROUND (lface) = (STRINGP (new_value)
				  ? new_value : Qunspecified);
    }
  else if (EQ (param, Qmouse_color))
    {
      lface = lface_from_face_name (f, Qmouse, 1);
      LFACE_BACKGROUND (lface) = (STRINGP (new_value)
				  ? new_value : Qunspecified);
    }
}


/* Get the value of X resource RESOURCE, class CLASS for the display
   of frame FRAME.  This is here because ordinary `x-get-resource'
   doesn't take a frame argument.  */

DEFUN ("internal-face-x-get-resource", Finternal_face_x_get_resource,
       Sinternal_face_x_get_resource, 3, 3, 0, doc: /* */)
     (resource, class, frame)
     Lisp_Object resource, class, frame;
{
  Lisp_Object value = Qnil;
#ifndef WINDOWSNT
#ifndef MAC_OS
  CHECK_STRING (resource);
  CHECK_STRING (class);
  CHECK_LIVE_FRAME (frame);
  BLOCK_INPUT;
  value = display_x_get_resource (FRAME_X_DISPLAY_INFO (XFRAME (frame)),
				  resource, class, Qnil, Qnil);
  UNBLOCK_INPUT;
#endif /* not MAC_OS */
#endif /* not WINDOWSNT */
  return value;
}


/* Return resource string VALUE as a boolean value, i.e. nil, or t.
   If VALUE is "on" or "true", return t.  If VALUE is "off" or
   "false", return nil.  Otherwise, if SIGNAL_P is non-zero, signal an
   error; if SIGNAL_P is zero, return 0.  */

static Lisp_Object
face_boolean_x_resource_value (value, signal_p)
     Lisp_Object value;
     int signal_p;
{
  Lisp_Object result = make_number (0);

  xassert (STRINGP (value));

  if (xstricmp (XSTRING (value)->data, "on") == 0
      || xstricmp (XSTRING (value)->data, "true") == 0)
    result = Qt;
  else if (xstricmp (XSTRING (value)->data, "off") == 0
	   || xstricmp (XSTRING (value)->data, "false") == 0)
    result = Qnil;
  else if (xstricmp (XSTRING (value)->data, "unspecified") == 0)
    result = Qunspecified;
  else if (signal_p)
    signal_error ("Invalid face attribute value from X resource", value);

  return result;
}


DEFUN ("internal-set-lisp-face-attribute-from-resource",
       Finternal_set_lisp_face_attribute_from_resource,
       Sinternal_set_lisp_face_attribute_from_resource,
       3, 4, 0, doc: /* */)
     (face, attr, value, frame)
     Lisp_Object face, attr, value, frame;
{
  CHECK_SYMBOL (face);
  CHECK_SYMBOL (attr);
  CHECK_STRING (value);

  if (xstricmp (XSTRING (value)->data, "unspecified") == 0)
    value = Qunspecified;
  else if (EQ (attr, QCheight))
    {
      value = Fstring_to_number (value, make_number (10));
      if (XINT (value) <= 0)
	signal_error ("Invalid face height from X resource", value);
    }
  else if (EQ (attr, QCbold) || EQ (attr, QCitalic))
    value = face_boolean_x_resource_value (value, 1);
  else if (EQ (attr, QCweight) || EQ (attr, QCslant) || EQ (attr, QCwidth))
    value = intern (XSTRING (value)->data);
  else if (EQ (attr, QCreverse_video) || EQ (attr, QCinverse_video))
    value = face_boolean_x_resource_value (value, 1);
  else if (EQ (attr, QCunderline)
	   || EQ (attr, QCoverline)
	   || EQ (attr, QCstrike_through))
    {
      Lisp_Object boolean_value;

      /* If the result of face_boolean_x_resource_value is t or nil,
	 VALUE does NOT specify a color. */
      boolean_value = face_boolean_x_resource_value (value, 0);
      if (SYMBOLP (boolean_value))
	value = boolean_value;
    }
  else if (EQ (attr, QCbox))
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
x_update_menu_appearance (f)
     struct frame *f;
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  XrmDatabase rdb;

  if (dpyinfo
      && (rdb = XrmGetDatabase (FRAME_X_DISPLAY (f)),
	  rdb != NULL))
    {
      char line[512];
      Lisp_Object lface = lface_from_face_name (f, Qmenu, 1);
      struct face *face = FACE_FROM_ID (f, MENU_FACE_ID);
      char *myname = XSTRING (Vx_resource_name)->data;
      int changed_p = 0;
#ifdef USE_MOTIF
      const char *popup_path = "popup_menu";
#else
      const char *popup_path = "menu.popup";
#endif
      
      if (STRINGP (LFACE_FOREGROUND (lface)))
	{
	  sprintf (line, "%s.%s*foreground: %s",
		   myname, popup_path,
		   XSTRING (LFACE_FOREGROUND (lface))->data);
	  XrmPutLineResource (&rdb, line);
	  sprintf (line, "%s.pane.menubar*foreground: %s",
		   myname, XSTRING (LFACE_FOREGROUND (lface))->data);
	  XrmPutLineResource (&rdb, line);
	  changed_p = 1;
	}

      if (STRINGP (LFACE_BACKGROUND (lface)))
	{
	  sprintf (line, "%s.%s*background: %s",
		   myname, popup_path,
		   XSTRING (LFACE_BACKGROUND (lface))->data);
	  XrmPutLineResource (&rdb, line);
	  sprintf (line, "%s.pane.menubar*background: %s",
		   myname, XSTRING (LFACE_BACKGROUND (lface))->data);
	  XrmPutLineResource (&rdb, line);
	  changed_p = 1;
	}
	  
      if (face->font_name
	  && (!UNSPECIFIEDP (LFACE_FAMILY (lface))
	      || !UNSPECIFIEDP (LFACE_SWIDTH (lface))
	      || !UNSPECIFIEDP (LFACE_AVGWIDTH (lface))
	      || !UNSPECIFIEDP (LFACE_WEIGHT (lface))
	      || !UNSPECIFIEDP (LFACE_SLANT (lface))
	      || !UNSPECIFIEDP (LFACE_HEIGHT (lface))))
	{
#ifdef USE_MOTIF
	  const char *suffix = "List";
#else
	  const char *suffix = "";
#endif
	  sprintf (line, "%s.pane.menubar*font%s: %s",
		   myname, suffix, face->font_name);
	  XrmPutLineResource (&rdb, line);
	  sprintf (line, "%s.%s*font%s: %s",
		   myname, popup_path, suffix, face->font_name);
	  XrmPutLineResource (&rdb, line);
	  changed_p = 1;
	}

      if (changed_p && f->output_data.x->menubar_widget)
	free_frame_menubar (f);
    }
}

#endif /* HAVE_X_WINDOWS && USE_X_TOOLKIT */


DEFUN ("face-attribute-relative-p", Fface_attribute_relative_p, 
       Sface_attribute_relative_p,
       2, 2, 0,
       doc: /* Return non-nil if face ATTRIBUTE VALUE is relative.  */)
     (attribute, value)
     Lisp_Object attribute, value;
{
  if (EQ (value, Qunspecified))
    return Qt;
  else if (EQ (attribute, QCheight))
    return INTEGERP (value) ? Qnil : Qt;
  else
    return Qnil;
}

DEFUN ("merge-face-attribute", Fmerge_face_attribute, Smerge_face_attribute,
       3, 3, 0,
       doc: /* Return face ATTRIBUTE VALUE1 merged with VALUE2.
If VALUE1 or VALUE2 are absolute (see `face-attribute-relative-p'), then
the result will be absolute, otherwise it will be relative.  */)
     (attribute, value1, value2)
     Lisp_Object attribute, value1, value2;
{
  if (EQ (value1, Qunspecified))
    return value2;
  else if (EQ (attribute, QCheight))
    return merge_face_heights (value1, value2, value1, Qnil);
  else
    return value1;
}


DEFUN ("internal-get-lisp-face-attribute", Finternal_get_lisp_face_attribute,
       Sinternal_get_lisp_face_attribute,
       2, 3, 0,
       doc: /* Return face attribute KEYWORD of face SYMBOL.
If SYMBOL does not name a valid Lisp face or KEYWORD isn't a valid
face attribute name, signal an error.
If the optional argument FRAME is given, report on face FACE in that
frame.  If FRAME is t, report on the defaults for face FACE (for new
frames).  If FRAME is omitted or nil, use the selected frame.  */)
     (symbol, keyword, frame)
     Lisp_Object symbol, keyword, frame;
{
  Lisp_Object lface, value = Qnil;

  CHECK_SYMBOL (symbol);
  CHECK_SYMBOL (keyword);

  if (EQ (frame, Qt))
    lface = lface_from_face_name (NULL, symbol, 1);
  else
    {
      if (NILP (frame))
	frame = selected_frame;
      CHECK_LIVE_FRAME (frame);
      lface = lface_from_face_name (XFRAME (frame), symbol, 1);
    }

  if (EQ (keyword, QCfamily))
    value = LFACE_FAMILY (lface);
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
  else if (EQ (keyword, QCbackground))
    value = LFACE_BACKGROUND (lface);
  else if (EQ (keyword, QCstipple))
    value = LFACE_STIPPLE (lface);
  else if (EQ (keyword, QCwidth))
    value = LFACE_SWIDTH (lface);
  else if (EQ (keyword, QCinherit))
    value = LFACE_INHERIT (lface);
  else if (EQ (keyword, QCfont))
    value = LFACE_FONT (lface);
  else
    signal_error ("Invalid face attribute name", keyword);

  return value;
}


DEFUN ("internal-lisp-face-attribute-values",
       Finternal_lisp_face_attribute_values,
       Sinternal_lisp_face_attribute_values, 1, 1, 0,
       doc: /* Return a list of valid discrete values for face attribute ATTR.
Value is nil if ATTR doesn't have a discrete set of valid values.  */)
     (attr)
     Lisp_Object attr;
{
  Lisp_Object result = Qnil;

  CHECK_SYMBOL (attr);

  if (EQ (attr, QCweight)
      || EQ (attr, QCslant)
      || EQ (attr, QCwidth))
    {
      /* Extract permissible symbols from tables.  */
      struct table_entry *table;
      int i, dim;

      if (EQ (attr, QCweight))
	table = weight_table, dim = DIM (weight_table);
      else if (EQ (attr, QCslant))
	table = slant_table, dim = DIM (slant_table);
      else
	table = swidth_table, dim = DIM (swidth_table);

      for (i = 0; i < dim; ++i)
	{
	  Lisp_Object symbol = *table[i].symbol;
	  Lisp_Object tail = result;

	  while (!NILP (tail)
		 && !EQ (XCAR (tail), symbol))
	    tail = XCDR (tail);

	  if (NILP (tail))
	    result = Fcons (symbol, result);
	}
    }
  else if (EQ (attr, QCunderline))
    result = Fcons (Qt, Fcons (Qnil, Qnil));
  else if (EQ (attr, QCoverline))
    result = Fcons (Qt, Fcons (Qnil, Qnil));
  else if (EQ (attr, QCstrike_through))
    result = Fcons (Qt, Fcons (Qnil, Qnil));
  else if (EQ (attr, QCinverse_video) || EQ (attr, QCreverse_video))
    result = Fcons (Qt, Fcons (Qnil, Qnil));

  return result;
}


DEFUN ("internal-merge-in-global-face", Finternal_merge_in_global_face,
       Sinternal_merge_in_global_face, 2, 2, 0,
       doc: /* Add attributes from frame-default definition of FACE to FACE on FRAME.
Default face attributes override any local face attributes.  */)
     (face, frame)
     Lisp_Object face, frame;
{
  int i;
  Lisp_Object global_lface, local_lface, *gvec, *lvec;

  CHECK_LIVE_FRAME (frame);
  global_lface = lface_from_face_name (NULL, face, 1);
  local_lface = lface_from_face_name (XFRAME (frame), face, 0);
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
    if (! UNSPECIFIEDP (gvec[i]))
      lvec[i] = gvec[i];

  return Qnil;
}


/* The following function is implemented for compatibility with 20.2.
   The function is used in x-resolve-fonts when it is asked to
   return fonts with the same size as the font of a face.  This is
   done in fontset.el.  */

DEFUN ("face-font", Fface_font, Sface_font, 1, 2, 0,
       doc: /* Return the font name of face FACE, or nil if it is unspecified.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
  The font default for a face is either nil, or a list
  of the form (bold), (italic) or (bold italic).
If FRAME is omitted or nil, use the selected frame.  */)
     (face, frame)
     Lisp_Object face, frame;
{
  if (EQ (frame, Qt))
    {
      Lisp_Object result = Qnil;
      Lisp_Object lface = lface_from_face_name (NULL, face, 1);

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
      struct frame *f = frame_or_selected_frame (frame, 1);
      int face_id = lookup_named_face (f, face, 0);
      struct face *face = FACE_FROM_ID (f, face_id);
      return face ? build_string (face->font_name) : Qnil;
    }
}


/* Compare face vectors V1 and V2 for equality.  Value is non-zero if
   all attributes are `equal'.  Tries to be fast because this function
   is called quite often.  */

static INLINE int
lface_equal_p (v1, v2)
     Lisp_Object *v1, *v2;
{
  int i, equal_p = 1;

  for (i = 1; i < LFACE_VECTOR_SIZE && equal_p; ++i)
    {
      Lisp_Object a = v1[i];
      Lisp_Object b = v2[i];

      /* Type can differ, e.g. when one attribute is unspecified, i.e. nil,
	 and the other is specified.  */
      equal_p = XTYPE (a) == XTYPE (b);
      if (!equal_p)
	break;

      if (!EQ (a, b))
	{
	  switch (XTYPE (a))
	    {
	    case Lisp_String:
	      equal_p = ((STRING_BYTES (XSTRING (a))
			  == STRING_BYTES (XSTRING (b)))
			 && bcmp (XSTRING (a)->data, XSTRING (b)->data,
				  STRING_BYTES (XSTRING (a))) == 0);
	      break;

	    case Lisp_Int:
	    case Lisp_Symbol:
	      equal_p = 0;
	      break;

	    default:
	      equal_p = !NILP (Fequal (a, b));
	      break;
	    }
	}
    }

  return equal_p;
}


DEFUN ("internal-lisp-face-equal-p", Finternal_lisp_face_equal_p,
       Sinternal_lisp_face_equal_p, 2, 3, 0,
       doc: /* True if FACE1 and FACE2 are equal.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame.  */)
     (face1, face2, frame)
     Lisp_Object face1, face2, frame;
{
  int equal_p;
  struct frame *f;
  Lisp_Object lface1, lface2;

  if (EQ (frame, Qt))
    f = NULL;
  else
    /* Don't use check_x_frame here because this function is called
       before X frames exist.  At that time, if FRAME is nil,
       selected_frame will be used which is the frame dumped with
       Emacs.  That frame is not an X frame.  */
    f = frame_or_selected_frame (frame, 2);

  lface1 = lface_from_face_name (NULL, face1, 1);
  lface2 = lface_from_face_name (NULL, face2, 1);
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
     (face, frame)
     Lisp_Object face, frame;
{
  struct frame *f;
  Lisp_Object lface;
  int i;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);

  if (EQ (frame, Qt))
    lface = lface_from_face_name (NULL, face, 1);
  else
    lface = lface_from_face_name (f, face, 1);

  for (i = 1; i < LFACE_VECTOR_SIZE; ++i)
    if (!UNSPECIFIEDP (AREF (lface, i)))
      break;

  return i == LFACE_VECTOR_SIZE ? Qt : Qnil;
}


DEFUN ("frame-face-alist", Fframe_face_alist, Sframe_face_alist,
       0, 1, 0,
       doc: /* Return an alist of frame-local faces defined on FRAME.
For internal use only.  */)
     (frame)
     Lisp_Object frame;
{
  struct frame *f = frame_or_selected_frame (frame, 0);
  return f->face_alist;
}


/* Return a hash code for Lisp string STRING with case ignored.  Used
   below in computing a hash value for a Lisp face.  */

static INLINE unsigned
hash_string_case_insensitive (string)
     Lisp_Object string;
{
  unsigned char *s;
  unsigned hash = 0;
  xassert (STRINGP (string));
  for (s = XSTRING (string)->data; *s; ++s)
    hash = (hash << 1) ^ tolower (*s);
  return hash;
}


/* Return a hash code for face attribute vector V.  */

static INLINE unsigned
lface_hash (v)
     Lisp_Object *v;
{
  return (hash_string_case_insensitive (v[LFACE_FAMILY_INDEX])
	  ^ hash_string_case_insensitive (v[LFACE_FOREGROUND_INDEX])
	  ^ hash_string_case_insensitive (v[LFACE_BACKGROUND_INDEX])
	  ^ XFASTINT (v[LFACE_WEIGHT_INDEX])
	  ^ XFASTINT (v[LFACE_SLANT_INDEX])
	  ^ XFASTINT (v[LFACE_SWIDTH_INDEX])
	  ^ XFASTINT (v[LFACE_HEIGHT_INDEX]));
}


/* Return non-zero if LFACE1 and LFACE2 specify the same font (without
   considering charsets/registries).  They do if they specify the same
   family, point size, weight, width, slant, and fontset.  Both LFACE1
   and LFACE2 must be fully-specified.  */

static INLINE int
lface_same_font_attributes_p (lface1, lface2)
     Lisp_Object *lface1, *lface2;
{
  xassert (lface_fully_specified_p (lface1)
	   && lface_fully_specified_p (lface2));
  return (xstricmp (XSTRING (lface1[LFACE_FAMILY_INDEX])->data,
		    XSTRING (lface2[LFACE_FAMILY_INDEX])->data) == 0
	  && EQ (lface1[LFACE_HEIGHT_INDEX], lface2[LFACE_HEIGHT_INDEX])
	  && EQ (lface1[LFACE_SWIDTH_INDEX], lface2[LFACE_SWIDTH_INDEX])
	  && EQ (lface1[LFACE_AVGWIDTH_INDEX], lface2[LFACE_AVGWIDTH_INDEX])
	  && EQ (lface1[LFACE_WEIGHT_INDEX], lface2[LFACE_WEIGHT_INDEX])
	  && EQ (lface1[LFACE_SLANT_INDEX], lface2[LFACE_SLANT_INDEX])
	  && (EQ (lface1[LFACE_FONT_INDEX], lface2[LFACE_FONT_INDEX])
	      || (STRINGP (lface1[LFACE_FONT_INDEX])
		  && STRINGP (lface2[LFACE_FONT_INDEX])
		  && xstricmp (XSTRING (lface1[LFACE_FONT_INDEX])->data,
			       XSTRING (lface2[LFACE_FONT_INDEX])->data))));
}



/***********************************************************************
			    Realized Faces
 ***********************************************************************/

/* Allocate and return a new realized face for Lisp face attribute
   vector ATTR.  */

static struct face *
make_realized_face (attr)
     Lisp_Object *attr;
{
  struct face *face = (struct face *) xmalloc (sizeof *face);
  bzero (face, sizeof *face);
  face->ascii_face = face;
  bcopy (attr, face->lface, sizeof face->lface);
  return face;
}


/* Free realized face FACE, including its X resources.  FACE may
   be null.  */

static void
free_realized_face (f, face)
     struct frame *f;
     struct face *face;
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
	      x_free_gc (f, face->gc);
	      face->gc = 0;
	    }

	  free_face_colors (f, face);
	  x_destroy_bitmap (f, face->stipple);
	}
#endif /* HAVE_WINDOW_SYSTEM */

      xfree (face);
    }
}


/* Prepare face FACE for subsequent display on frame F.  This
   allocated GCs if they haven't been allocated yet or have been freed
   by clearing the face cache.  */

void
prepare_face_for_display (f, face)
     struct frame *f;
     struct face *face;
{
#ifdef HAVE_WINDOW_SYSTEM
  xassert (FRAME_WINDOW_P (f));

  if (face->gc == 0)
    {
      XGCValues xgcv;
      unsigned long mask = GCForeground | GCBackground | GCGraphicsExposures;

      xgcv.foreground = face->foreground;
      xgcv.background = face->background;
#ifdef HAVE_X_WINDOWS
      xgcv.graphics_exposures = False;
#endif
      /* The font of FACE may be null if we couldn't load it.  */
      if (face->font)
	{
#ifdef HAVE_X_WINDOWS
	  xgcv.font = face->font->fid;
#endif
#ifdef WINDOWSNT
	  xgcv.font = face->font;
#endif
#ifdef MAC_OS
	  xgcv.font = face->font;
#endif
	  mask |= GCFont;
	}

      BLOCK_INPUT;
#ifdef HAVE_X_WINDOWS
      if (face->stipple)
	{
	  xgcv.fill_style = FillOpaqueStippled;
	  xgcv.stipple = x_bitmap_pixmap (f, face->stipple);
	  mask |= GCFillStyle | GCStipple;
	}
#endif
      face->gc = x_create_gc (f, mask, &xgcv);
      UNBLOCK_INPUT;
    }
#endif /* HAVE_WINDOW_SYSTEM */
}


/***********************************************************************
			      Face Cache
 ***********************************************************************/

/* Return a new face cache for frame F.  */

static struct face_cache *
make_face_cache (f)
     struct frame *f;
{
  struct face_cache *c;
  int size;

  c = (struct face_cache *) xmalloc (sizeof *c);
  bzero (c, sizeof *c);
  size = FACE_CACHE_BUCKETS_SIZE * sizeof *c->buckets;
  c->buckets = (struct face **) xmalloc (size);
  bzero (c->buckets, size);
  c->size = 50;
  c->faces_by_id = (struct face **) xmalloc (c->size * sizeof *c->faces_by_id);
  c->f = f;
  c->menu_face_changed_p = menu_face_changed_default;
  return c;
}


/* Clear out all graphics contexts for all realized faces, except for
   the basic faces.  This should be done from time to time just to avoid
   keeping too many graphics contexts that are no longer needed.  */

static void
clear_face_gcs (c)
     struct face_cache *c;
{
  if (c && FRAME_WINDOW_P (c->f))
    {
#ifdef HAVE_WINDOW_SYSTEM
      int i;
      for (i = BASIC_FACE_ID_SENTINEL; i < c->used; ++i)
	{
	  struct face *face = c->faces_by_id[i];
	  if (face && face->gc)
	    {
	      x_free_gc (c->f, face->gc);
	      face->gc = 0;
	    }
	}
#endif /* HAVE_WINDOW_SYSTEM */
    }
}


/* Free all realized faces in face cache C, including basic faces.  C
   may be null.  If faces are freed, make sure the frame's current
   matrix is marked invalid, so that a display caused by an expose
   event doesn't try to use faces we destroyed.  */

static void
free_realized_faces (c)
     struct face_cache *c;
{
  if (c && c->used)
    {
      int i, size;
      struct frame *f = c->f;

      /* We must block input here because we can't process X events
	 safely while only some faces are freed, or when the frame's
	 current matrix still references freed faces.  */
      BLOCK_INPUT;

      for (i = 0; i < c->used; ++i)
	{
	  free_realized_face (f, c->faces_by_id[i]);
	  c->faces_by_id[i] = NULL;
	}

      c->used = 0;
      size = FACE_CACHE_BUCKETS_SIZE * sizeof *c->buckets;
      bzero (c->buckets, size);

      /* Must do a thorough redisplay the next time.  Mark current
	 matrices as invalid because they will reference faces freed
	 above.  This function is also called when a frame is
	 destroyed.  In this case, the root window of F is nil.  */
      if (WINDOWP (f->root_window))
	{
	  clear_current_matrices (f);
	  ++windows_or_buffers_changed;
	}

      UNBLOCK_INPUT;
    }
}


/* Free all faces realized for multibyte characters on frame F that
   has FONTSET.  */

void
free_realized_multibyte_face (f, fontset)
     struct frame *f;
     int fontset;
{
  struct face_cache *cache = FRAME_FACE_CACHE (f);
  struct face *face;
  int i;

  /* We must block input here because we can't process X events safely
     while only some faces are freed, or when the frame's current
     matrix still references freed faces.  */
  BLOCK_INPUT;

  for (i = 0; i < cache->used; i++)
    {
      face = cache->faces_by_id[i];
      if (face
	  && face != face->ascii_face
	  && face->fontset == fontset)
	{
	  uncache_face (cache, face);
	  free_realized_face (f, face);
	}
    }

  /* Must do a thorough redisplay the next time.  Mark current
     matrices as invalid because they will reference faces freed
     above.  This function is also called when a frame is destroyed.
     In this case, the root window of F is nil.  */
  if (WINDOWP (f->root_window))
    {
      clear_current_matrices (f);
      ++windows_or_buffers_changed;
    }

  UNBLOCK_INPUT;
}


/* Free all realized faces on FRAME or on all frames if FRAME is nil.
   This is done after attributes of a named face have been changed,
   because we can't tell which realized faces depend on that face.  */

void
free_all_realized_faces (frame)
     Lisp_Object frame;
{
  if (NILP (frame))
    {
      Lisp_Object rest;
      FOR_EACH_FRAME (rest, frame)
	free_realized_faces (FRAME_FACE_CACHE (XFRAME (frame)));
    }
  else
    free_realized_faces (FRAME_FACE_CACHE (XFRAME (frame)));
}


/* Free face cache C and faces in it, including their X resources.  */

static void
free_face_cache (c)
     struct face_cache *c;
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
   of FACE.  If FACE->fontset >= 0, add the new face to the end of the
   collision list of the face hash table of C.  This is done because
   otherwise lookup_face would find FACE for every character, even if
   faces with the same attributes but for specific characters exist.  */

static void
cache_face (c, face, hash)
     struct face_cache *c;
     struct face *face;
     unsigned hash;
{
  int i = hash % FACE_CACHE_BUCKETS_SIZE;

  face->hash = hash;

  if (face->fontset >= 0)
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

  /* Maybe enlarge C->faces_by_id.  */
  if (i == c->used && c->used == c->size)
    {
      int new_size = 2 * c->size;
      int sz = new_size * sizeof *c->faces_by_id;
      c->faces_by_id = (struct face **) xrealloc (c->faces_by_id, sz);
      c->size = new_size;
    }

#if GLYPH_DEBUG
  /* Check that FACE got a unique id.  */
  {
    int j, n;
    struct face *face;

    for (j = n = 0; j < FACE_CACHE_BUCKETS_SIZE; ++j)
      for (face = c->buckets[j]; face; face = face->next)
	if (face->id == i)
	  ++n;

    xassert (n == 1);
  }
#endif /* GLYPH_DEBUG */

  c->faces_by_id[i] = face;
  if (i == c->used)
    ++c->used;
}


/* Remove face FACE from cache C.  */

static void
uncache_face (c, face)
     struct face_cache *c;
     struct face *face;
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
   of frame F.  The face will be used to display character C.  Value
   is the ID of the face found.  If no suitable face is found, realize
   a new one.  In that case, if C is a multibyte character, BASE_FACE
   is a face that has the same attributes.  */

INLINE int
lookup_face (f, attr, c, base_face)
     struct frame *f;
     Lisp_Object *attr;
     int c;
     struct face *base_face;
{
  struct face_cache *cache = FRAME_FACE_CACHE (f);
  unsigned hash;
  int i;
  struct face *face;

  xassert (cache != NULL);
  check_lface_attrs (attr);

  /* Look up ATTR in the face cache.  */
  hash = lface_hash (attr);
  i = hash % FACE_CACHE_BUCKETS_SIZE;

  for (face = cache->buckets[i]; face; face = face->next)
    if (face->hash == hash
	&& (!FRAME_WINDOW_P (f)
	    || FACE_SUITABLE_FOR_CHAR_P (face, c))
	&& lface_equal_p (face->lface, attr))
      break;

  /* If not found, realize a new face.  */
  if (face == NULL)
    face = realize_face (cache, attr, c, base_face, -1);

#if GLYPH_DEBUG
  xassert (face == FACE_FROM_ID (f, face->id));

/* When this function is called from face_for_char (in this case, C is
   a multibyte character), a fontset of a face returned by
   realize_face is not yet set, i.e. FACE_SUITABLE_FOR_CHAR_P (FACE,
   C) is not sutisfied.  The fontset is set for this face by
   face_for_char later.  */
#if 0
  if (FRAME_WINDOW_P (f))
    xassert (FACE_SUITABLE_FOR_CHAR_P (face, c));
#endif
#endif /* GLYPH_DEBUG */

  return face->id;
}


/* Return the face id of the realized face for named face SYMBOL on
   frame F suitable for displaying character C.  Value is -1 if the
   face couldn't be determined, which might happen if the default face
   isn't realized and cannot be realized.  */

int
lookup_named_face (f, symbol, c)
     struct frame *f;
     Lisp_Object symbol;
     int c;
{
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  Lisp_Object symbol_attrs[LFACE_VECTOR_SIZE];
  struct face *default_face = FACE_FROM_ID (f, DEFAULT_FACE_ID);

  if (default_face == NULL)
    {
      if (!realize_basic_faces (f))
	return -1;
      default_face = FACE_FROM_ID (f, DEFAULT_FACE_ID);
    }

  get_lface_attributes (f, symbol, symbol_attrs, 1);
  bcopy (default_face->lface, attrs, sizeof attrs);
  merge_face_vectors (f, symbol_attrs, attrs, Qnil);
  return lookup_face (f, attrs, c, NULL);
}


/* Return the ID of the realized ASCII face of Lisp face with ID
   LFACE_ID on frame F.  Value is -1 if LFACE_ID isn't valid.  */

int
ascii_face_of_lisp_face (f, lface_id)
     struct frame *f;
     int lface_id;
{
  int face_id;

  if (lface_id >= 0 && lface_id < lface_id_to_name_size)
    {
      Lisp_Object face_name = lface_id_to_name[lface_id];
      face_id = lookup_named_face (f, face_name, 0);
    }
  else
    face_id = -1;

  return face_id;
}


/* Return a face for charset ASCII that is like the face with id
   FACE_ID on frame F, but has a font that is STEPS steps smaller.
   STEPS < 0 means larger.  Value is the id of the face.  */

int
smaller_face (f, face_id, steps)
     struct frame *f;
     int face_id, steps;
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
  steps = abs (steps);

  face = FACE_FROM_ID (f, face_id);
  bcopy (face->lface, attrs, sizeof attrs);
  pt = last_pt = XFASTINT (attrs[LFACE_HEIGHT_INDEX]);
  new_face_id = face_id;
  last_height = FONT_HEIGHT (face->font);

  while (steps
	 && pt + delta > 0
	 /* Give up if we cannot find a font within 10pt.  */
	 && abs (last_pt - pt) < 100)
    {
      /* Look up a face for a slightly smaller/larger font.  */
      pt += delta;
      attrs[LFACE_HEIGHT_INDEX] = make_number (pt);
      new_face_id = lookup_face (f, attrs, 0, NULL);
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
face_with_height (f, face_id, height)
     struct frame *f;
     int face_id;
     int height;
{
#ifdef HAVE_WINDOW_SYSTEM
  struct face *face;
  Lisp_Object attrs[LFACE_VECTOR_SIZE];

  if (FRAME_TERMCAP_P (f)
      || height <= 0)
    return face_id;

  face = FACE_FROM_ID (f, face_id);
  bcopy (face->lface, attrs, sizeof attrs);
  attrs[LFACE_HEIGHT_INDEX] = make_number (height);
  face_id = lookup_face (f, attrs, 0, NULL);
#endif /* HAVE_WINDOW_SYSTEM */

  return face_id;
}


/* Return the face id of the realized face for named face SYMBOL on
   frame F suitable for displaying character C, and use attributes of
   the face FACE_ID for attributes that aren't completely specified by
   SYMBOL.  This is like lookup_named_face, except that the default
   attributes come from FACE_ID, not from the default face.  FACE_ID
   is assumed to be already realized.  */

int
lookup_derived_face (f, symbol, c, face_id)
     struct frame *f;
     Lisp_Object symbol;
     int c;
     int face_id;
{
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  Lisp_Object symbol_attrs[LFACE_VECTOR_SIZE];
  struct face *default_face = FACE_FROM_ID (f, face_id);

  if (!default_face)
    abort ();

  get_lface_attributes (f, symbol, symbol_attrs, 1);
  bcopy (default_face->lface, attrs, sizeof attrs);
  merge_face_vectors (f, symbol_attrs, attrs, Qnil);
  return lookup_face (f, attrs, c, default_face);
}

DEFUN ("face-attributes-as-vector", Fface_attributes_as_vector,
       Sface_attributes_as_vector, 1, 1, 0,
       doc: /* Return a vector of face attributes corresponding to PLIST.  */)
     (plist)
     Lisp_Object plist;
{
  Lisp_Object lface;
  lface = Fmake_vector (make_number (LFACE_VECTOR_SIZE),
			Qunspecified);
  merge_face_vector_with_property (XFRAME (selected_frame),
				   XVECTOR (lface)->contents,
				   plist);
  return lface;
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
     (order)
     Lisp_Object order;
{
  Lisp_Object list;
  int i;
  int indices[DIM (font_sort_order)];

  CHECK_LIST (order);
  bzero (indices, sizeof indices);
  i = 0;

  for (list = order;
       CONSP (list) && i < DIM (indices);
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

  if (!NILP (list) || i != DIM (indices))
    signal_error ("Invalid font sort order", order);
  for (i = 0; i < DIM (font_sort_order); ++i)
    if (indices[i] == 0)
      signal_error ("Invalid font sort order", order);

  if (bcmp (indices, font_sort_order, sizeof indices) != 0)
    {
      bcopy (indices, font_sort_order, sizeof font_sort_order);
      free_all_realized_faces (Qnil);
    }

  return Qnil;
}


DEFUN ("internal-set-alternative-font-family-alist",
       Finternal_set_alternative_font_family_alist,
       Sinternal_set_alternative_font_family_alist, 1, 1, 0,
       doc: /* Define alternative font families to try in face font selection.
ALIST is an alist of (FAMILY ALTERNATIVE1 ALTERNATIVE2 ...) entries.
Each ALTERNATIVE is tried in order if no fonts of font family FAMILY can
be found.  Value is ALIST.  */)
     (alist)
     Lisp_Object alist;
{
  CHECK_LIST (alist);
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
     (alist)
     Lisp_Object alist;
{
  CHECK_LIST (alist);
  Vface_alternative_font_registry_alist = alist;
  free_all_realized_faces (Qnil);
  return alist;
}


#ifdef HAVE_WINDOW_SYSTEM

/* Value is non-zero if FONT is the name of a scalable font.  The
   X11R6 XLFD spec says that point size, pixel size, and average width
   are zero for scalable fonts.  Intlfonts contain at least one
   scalable font ("*-muleindian-1") for which this isn't true, so we
   just test average width.  */

static int
font_scalable_p (font)
     struct font_name *font;
{
  char *s = font->fields[XLFD_AVGWIDTH];
  return (*s == '0' && *(s + 1) == '\0')
#ifdef WINDOWSNT
  /* Windows implementation of XLFD is slightly broken for backward
     compatibility with previous broken versions, so test for
     wildcards as well as 0. */
  || *s == '*'
#endif
    ;
}


/* Ignore the difference of font point size less than this value.  */

#define FONT_POINT_SIZE_QUANTUM 5

/* Value is non-zero if FONT1 is a better match for font attributes
   VALUES than FONT2.  VALUES is an array of face attribute values in
   font sort order.  COMPARE_PT_P zero means don't compare point
   sizes.  AVGWIDTH, if not zero, is a specified font average width
   to compare with.  */

static int
better_font_p (values, font1, font2, compare_pt_p, avgwidth)
     int *values;
     struct font_name *font1, *font2;
     int compare_pt_p, avgwidth;
{
  int i;

  for (i = 0; i < DIM (font_sort_order); ++i)
    {
      int xlfd_idx = font_sort_order[i];

      if (compare_pt_p || xlfd_idx != XLFD_POINT_SIZE)
	{
	  int delta1 = abs (values[i] - font1->numeric[xlfd_idx]);
	  int delta2 = abs (values[i] - font2->numeric[xlfd_idx]);

	  if (xlfd_idx == XLFD_POINT_SIZE
	      && abs (delta1 - delta2) < FONT_POINT_SIZE_QUANTUM)
	    continue;
	  if (delta1 > delta2)
	    return 0;
	  else if (delta1 < delta2)
	    return 1;
	  else
	    {
	      /* The difference may be equal because, e.g., the face
		 specifies `italic' but we have only `regular' and
		 `oblique'.  Prefer `oblique' in this case.  */
	      if ((xlfd_idx == XLFD_WEIGHT || xlfd_idx == XLFD_SLANT)
		  && font1->numeric[xlfd_idx] > values[i]
		  && font2->numeric[xlfd_idx] < values[i])
		return 1;
	    }
	}
    }

  if (avgwidth)
    {
      int delta1 = abs (avgwidth - font1->numeric[XLFD_AVGWIDTH]);
      int delta2 = abs (avgwidth - font2->numeric[XLFD_AVGWIDTH]);
      if (delta1 > delta2)
	return 0;
      else if (delta1 < delta2)
	return 1;
    }

  return font1->registry_priority < font2->registry_priority;
}


/* Value is non-zero if FONT is an exact match for face attributes in
   SPECIFIED.  SPECIFIED is an array of face attribute values in font
   sort order.  AVGWIDTH, if non-zero, is an average width to compare
   with.  */

static int
exact_face_match_p (specified, font, avgwidth)
     int *specified;
     struct font_name *font;
     int avgwidth;
{
  int i;

  for (i = 0; i < DIM (font_sort_order); ++i)
    if (specified[i] != font->numeric[font_sort_order[i]])
      break;

  return (i == DIM (font_sort_order)
	  && (avgwidth <= 0
	      || avgwidth == font->numeric[XLFD_AVGWIDTH]));
}


/* Value is the name of a scaled font, generated from scalable font
   FONT on frame F.  SPECIFIED_PT is the point-size to scale FONT to.
   Value is allocated from heap.  */

static char *
build_scalable_font_name (f, font, specified_pt)
     struct frame *f;
     struct font_name *font;
     int specified_pt;
{
  char point_size[20], pixel_size[20];
  int pixel_value;
  double resy = FRAME_X_DISPLAY_INFO (f)->resy;
  double pt;

  /* If scalable font is for a specific resolution, compute
     the point size we must specify from the resolution of
     the display and the specified resolution of the font.  */
  if (font->numeric[XLFD_RESY] != 0)
    {
      pt = resy / font->numeric[XLFD_RESY] * specified_pt + 0.5;
      pixel_value = font->numeric[XLFD_RESY] / (PT_PER_INCH * 10.0) * pt;
    }
  else
    {
      pt = specified_pt;
      pixel_value = resy / (PT_PER_INCH * 10.0) * pt;
    }

  /* Set point size of the font.  */
  sprintf (point_size, "%d", (int) pt);
  font->fields[XLFD_POINT_SIZE] = point_size;
  font->numeric[XLFD_POINT_SIZE] = pt;

  /* Set pixel size.  */
  sprintf (pixel_size, "%d", pixel_value);
  font->fields[XLFD_PIXEL_SIZE] = pixel_size;
  font->numeric[XLFD_PIXEL_SIZE] = pixel_value;

  /* If font doesn't specify its resolution, use the
     resolution of the display.  */
  if (font->numeric[XLFD_RESY] == 0)
    {
      char buffer[20];
      sprintf (buffer, "%d", (int) resy);
      font->fields[XLFD_RESY] = buffer;
      font->numeric[XLFD_RESY] = resy;
    }

  if (strcmp (font->fields[XLFD_RESX], "0") == 0)
    {
      char buffer[20];
      int resx = FRAME_X_DISPLAY_INFO (f)->resx;
      sprintf (buffer, "%d", resx);
      font->fields[XLFD_RESX] = buffer;
      font->numeric[XLFD_RESX] = resx;
    }

  return build_font_name (font);
}


/* Value is non-zero if we are allowed to use scalable font FONT.  We
   can't run a Lisp function here since this function may be called
   with input blocked.  */

static int
may_use_scalable_font_p (font)
     char *font;
{
  if (EQ (Vscalable_fonts_allowed, Qt))
    return 1;
  else if (CONSP (Vscalable_fonts_allowed))
    {
      Lisp_Object tail, regexp;

      for (tail = Vscalable_fonts_allowed; CONSP (tail); tail = XCDR (tail))
	{
	  regexp = XCAR (tail);
	  if (STRINGP (regexp)
	      && fast_c_string_match_ignore_case (regexp, font) >= 0)
	    return 1;
	}
    }

  return 0;
}



/* Return the name of the best matching font for face attributes ATTRS
   in the array of font_name structures FONTS which contains NFONTS
   elements.  WIDTH_RATIO is a factor with which to multiply average
   widths if ATTRS specifies such a width.

   Value is a font name which is allocated from the heap.  FONTS is
   freed by this function.  */

static char *
best_matching_font (f, attrs, fonts, nfonts, width_ratio)
     struct frame *f;
     Lisp_Object *attrs;
     struct font_name *fonts;
     int nfonts;
     int width_ratio;
{
  char *font_name;
  struct font_name *best;
  int i, pt = 0;
  int specified[5];
  int exact_p, avgwidth;

  if (nfonts == 0)
    return NULL;

  /* Make specified font attributes available in `specified',
     indexed by sort order.  */
  for (i = 0; i < DIM (font_sort_order); ++i)
    {
      int xlfd_idx = font_sort_order[i];

      if (xlfd_idx == XLFD_SWIDTH)
	specified[i] = face_numeric_swidth (attrs[LFACE_SWIDTH_INDEX]);
      else if (xlfd_idx == XLFD_POINT_SIZE)
	specified[i] = pt = XFASTINT (attrs[LFACE_HEIGHT_INDEX]);
      else if (xlfd_idx == XLFD_WEIGHT)
	specified[i] = face_numeric_weight (attrs[LFACE_WEIGHT_INDEX]);
      else if (xlfd_idx == XLFD_SLANT)
	specified[i] = face_numeric_slant (attrs[LFACE_SLANT_INDEX]);
      else
	abort ();
    }

  avgwidth = (UNSPECIFIEDP (attrs[LFACE_AVGWIDTH_INDEX])
	      ? 0
	      : XFASTINT (attrs[LFACE_AVGWIDTH_INDEX]) * width_ratio);

  exact_p = 0;

  /* Start with the first non-scalable font in the list.  */
  for (i = 0; i < nfonts; ++i)
    if (!font_scalable_p (fonts + i))
      break;

  /* Find the best match among the non-scalable fonts.  */
  if (i < nfonts)
    {
      best = fonts + i;

      for (i = 1; i < nfonts; ++i)
	if (!font_scalable_p (fonts + i)
	    && better_font_p (specified, fonts + i, best, 1, avgwidth))
	  {
	    best = fonts + i;

	    exact_p = exact_face_match_p (specified, best, avgwidth);
	    if (exact_p)
	      break;
	  }

    }
  else
    best = NULL;

  /* Unless we found an exact match among non-scalable fonts, see if
     we can find a better match among scalable fonts.  */
  if (!exact_p)
    {
      /* A scalable font is better if

	 1. its weight, slant, swidth attributes are better, or.

	 2. the best non-scalable font doesn't have the required
	 point size, and the scalable fonts weight, slant, swidth
	 isn't worse.  */

      int non_scalable_has_exact_height_p;

      if (best && best->numeric[XLFD_POINT_SIZE] == pt)
	non_scalable_has_exact_height_p = 1;
      else
	non_scalable_has_exact_height_p = 0;

      for (i = 0; i < nfonts; ++i)
	if (font_scalable_p (fonts + i))
	  {
	    if (best == NULL
		|| better_font_p (specified, fonts + i, best, 0, 0)
		|| (!non_scalable_has_exact_height_p
		    && !better_font_p (specified, best, fonts + i, 0, 0)))
	      best = fonts + i;
	  }
    }

  if (font_scalable_p (best))
    font_name = build_scalable_font_name (f, best, pt);
  else
    font_name = build_font_name (best);

  /* Free font_name structures.  */
  free_font_names (fonts, nfonts);

  return font_name;
}


/* Get a list of matching fonts on frame F, considering FAMILY
   and alternative font families from Vface_alternative_font_registry_alist.

   FAMILY is the font family whose alternatives are considered.

   REGISTRY, if a string, specifies a font registry and encoding to
   match.  A value of nil means include fonts of any registry and
   encoding.
   
   Return in *FONTS a pointer to a vector of font_name structures for
   the fonts matched.  Value is the number of fonts found.  */

static int
try_alternative_families (f, family, registry, fonts)
     struct frame *f;
     Lisp_Object family, registry;
     struct font_name **fonts;
{
  Lisp_Object alter;
  int nfonts = 0;

  nfonts = font_list (f, Qnil, family, registry, fonts);
  if (nfonts == 0)
    {
      /* Try alternative font families.  */
      alter = Fassoc (family, Vface_alternative_font_family_alist);
      if (CONSP (alter))
	{
	  for (alter = XCDR (alter);
	       CONSP (alter) && nfonts == 0;
	       alter = XCDR (alter))
	    {
	      if (STRINGP (XCAR (alter)))
		nfonts = font_list (f, Qnil, XCAR (alter), registry, fonts);
	    }
	}
      
      /* Try scalable fonts before giving up.  */
      if (nfonts == 0 && NILP (Vscalable_fonts_allowed))
	{
	  int count = BINDING_STACK_SIZE ();
	  specbind (Qscalable_fonts_allowed, Qt);
	  nfonts = try_alternative_families (f, family, registry, fonts);
	  unbind_to (count, Qnil);
	}
    }
  return nfonts;
}


/* Get a list of matching fonts on frame F.

   FAMILY, if a string, specifies a font family derived from the fontset.
   It is only used if the face does not specify any family in ATTRS or
   if we cannot find any font of the face's family.

   REGISTRY, if a string, specifies a font registry and encoding to
   match.  A value of nil means include fonts of any registry and
   encoding.
   
   Return in *FONTS a pointer to a vector of font_name structures for
   the fonts matched.  Value is the number of fonts found.  */

static int
try_font_list (f, attrs, family, registry, fonts)
     struct frame *f;
     Lisp_Object *attrs;
     Lisp_Object family, registry;
     struct font_name **fonts;
{
  int nfonts = 0;
  Lisp_Object face_family = attrs[LFACE_FAMILY_INDEX];

  if (STRINGP (face_family))
    nfonts = try_alternative_families (f, face_family, registry, fonts);

  if (nfonts == 0 && !NILP (family))
    nfonts = try_alternative_families (f, family, registry, fonts);

  /* Try font family of the default face or "fixed".  */
  if (nfonts == 0)
    {
      struct face *default_face = FACE_FROM_ID (f, DEFAULT_FACE_ID);
      if (default_face)
	family = default_face->lface[LFACE_FAMILY_INDEX];
      else
	family = build_string ("fixed");
      nfonts = font_list (f, Qnil, family, registry, fonts);
    }
      
  /* Try any family with the given registry.  */
  if (nfonts == 0)
    nfonts = font_list (f, Qnil, Qnil, registry, fonts);

  return nfonts;
}


/* Return the fontset id of the base fontset name or alias name given
   by the fontset attribute of ATTRS.  Value is -1 if the fontset
   attribute of ATTRS doesn't name a fontset.  */

static int
face_fontset (attrs)
     Lisp_Object *attrs;
{
  Lisp_Object name;

  name = attrs[LFACE_FONT_INDEX];
  if (!STRINGP (name))
    return -1;
  return fs_query_fontset (name, 0);
}


/* Choose a name of font to use on frame F to display character C with
   Lisp face attributes specified by ATTRS.  The font name is
   determined by the font-related attributes in ATTRS and the name
   pattern for C in FONTSET.  Value is the font name which is
   allocated from the heap and must be freed by the caller, or NULL if
   we can get no information about the font name of C.  It is assured
   that we always get some information for a single byte
   character.  */

static char *
choose_face_font (f, attrs, fontset, c)
     struct frame *f;
     Lisp_Object *attrs;
     int fontset, c;
{
  Lisp_Object pattern;
  char *font_name = NULL;
  struct font_name *fonts;
  int nfonts, width_ratio;

  /* Get (foundry and) family name and registry (and encoding) name of
     a font for C.  */
  pattern = fontset_font_pattern (f, fontset, c);
  if (NILP (pattern))
    {
      xassert (!SINGLE_BYTE_CHAR_P (c));
      return NULL;
    }
  
  /* If what we got is a name pattern, return it.  */
  if (STRINGP (pattern))
    return xstrdup (XSTRING (pattern)->data);

  /* Get a list of fonts matching that pattern and choose the
     best match for the specified face attributes from it.  */
  nfonts = try_font_list (f, attrs, XCAR (pattern), XCDR (pattern), &fonts);
  width_ratio = (SINGLE_BYTE_CHAR_P (c)
		 ? 1
		 : CHARSET_WIDTH (CHAR_CHARSET (c)));
  font_name = best_matching_font (f, attrs, fonts, nfonts, width_ratio);
  return font_name;
}

#endif /* HAVE_WINDOW_SYSTEM */



/***********************************************************************
			   Face Realization
 ***********************************************************************/

/* Realize basic faces on frame F.  Value is zero if frame parameters
   of F don't contain enough information needed to realize the default
   face.  */

static int
realize_basic_faces (f)
     struct frame *f;
{
  int success_p = 0;
  int count = BINDING_STACK_SIZE ();

  /* Block input here so that we won't be surprised by an X expose
     event, for instance, without having the faces set up.  */
  BLOCK_INPUT;
  specbind (Qscalable_fonts_allowed, Qt);

  if (realize_default_face (f))
    {
      realize_named_face (f, Qmode_line, MODE_LINE_FACE_ID);
      realize_named_face (f, Qmode_line_inactive, MODE_LINE_INACTIVE_FACE_ID);
      realize_named_face (f, Qtool_bar, TOOL_BAR_FACE_ID);
      realize_named_face (f, Qfringe, FRINGE_FACE_ID);
      realize_named_face (f, Qheader_line, HEADER_LINE_FACE_ID);
      realize_named_face (f, Qscroll_bar, SCROLL_BAR_FACE_ID);
      realize_named_face (f, Qborder, BORDER_FACE_ID);
      realize_named_face (f, Qcursor, CURSOR_FACE_ID);
      realize_named_face (f, Qmouse, MOUSE_FACE_ID);
      realize_named_face (f, Qmenu, MENU_FACE_ID);

      /* Reflect changes in the `menu' face in menu bars.  */
      if (FRAME_FACE_CACHE (f)->menu_face_changed_p)
	{
	  FRAME_FACE_CACHE (f)->menu_face_changed_p = 0;
#ifdef USE_X_TOOLKIT
	  x_update_menu_appearance (f);
#endif
	}
      
      success_p = 1;
    }

  unbind_to (count, Qnil);
  UNBLOCK_INPUT;
  return success_p;
}


/* Realize the default face on frame F.  If the face is not fully
   specified, make it fully-specified.  Attributes of the default face
   that are not explicitly specified are taken from frame parameters.  */

static int
realize_default_face (f)
     struct frame *f;
{
  struct face_cache *c = FRAME_FACE_CACHE (f);
  Lisp_Object lface;
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  Lisp_Object frame_font;
  struct face *face;

  /* If the `default' face is not yet known, create it.  */
  lface = lface_from_face_name (f, Qdefault, 0);
  if (NILP (lface))
    {
      Lisp_Object frame;
      XSETFRAME (frame, f);
      lface = Finternal_make_lisp_face (Qdefault, frame);
    }

#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    {
      /* Set frame_font to the value of the `font' frame parameter.  */
      frame_font = Fassq (Qfont, f->param_alist);
      xassert (CONSP (frame_font) && STRINGP (XCDR (frame_font)));
      frame_font = XCDR (frame_font);
      set_lface_from_font_name (f, lface, frame_font, 1, 1);
    }
#endif /* HAVE_WINDOW_SYSTEM */

  if (!FRAME_WINDOW_P (f))
    {
      LFACE_FAMILY (lface) = build_string ("default");
      LFACE_SWIDTH (lface) = Qnormal;
      LFACE_HEIGHT (lface) = make_number (1);
      if (UNSPECIFIEDP (LFACE_WEIGHT (lface)))
	LFACE_WEIGHT (lface) = Qnormal;
      if (UNSPECIFIEDP (LFACE_SLANT (lface)))
	LFACE_SLANT (lface) = Qnormal;
      LFACE_AVGWIDTH (lface) = Qunspecified;
    }

  if (UNSPECIFIEDP (LFACE_UNDERLINE (lface)))
    LFACE_UNDERLINE (lface) = Qnil;

  if (UNSPECIFIEDP (LFACE_OVERLINE (lface)))
    LFACE_OVERLINE (lface) = Qnil;

  if (UNSPECIFIEDP (LFACE_STRIKE_THROUGH (lface)))
    LFACE_STRIKE_THROUGH (lface) = Qnil;

  if (UNSPECIFIEDP (LFACE_BOX (lface)))
    LFACE_BOX (lface) = Qnil;

  if (UNSPECIFIEDP (LFACE_INVERSE (lface)))
    LFACE_INVERSE (lface) = Qnil;

  if (UNSPECIFIEDP (LFACE_FOREGROUND (lface)))
    {
      /* This function is called so early that colors are not yet
	 set in the frame parameter list.  */
      Lisp_Object color = Fassq (Qforeground_color, f->param_alist);

      if (CONSP (color) && STRINGP (XCDR (color)))
	LFACE_FOREGROUND (lface) = XCDR (color);
      else if (FRAME_WINDOW_P (f))
	return 0;
      else if (FRAME_TERMCAP_P (f) || FRAME_MSDOS_P (f))
	LFACE_FOREGROUND (lface) = build_string (unspecified_fg);
      else
	abort ();
    }

  if (UNSPECIFIEDP (LFACE_BACKGROUND (lface)))
    {
      /* This function is called so early that colors are not yet
	 set in the frame parameter list.  */
      Lisp_Object color = Fassq (Qbackground_color, f->param_alist);
      if (CONSP (color) && STRINGP (XCDR (color)))
	LFACE_BACKGROUND (lface) = XCDR (color);
      else if (FRAME_WINDOW_P (f))
	return 0;
      else if (FRAME_TERMCAP_P (f) || FRAME_MSDOS_P (f))
	LFACE_BACKGROUND (lface) = build_string (unspecified_bg);
      else
	abort ();
    }

  if (UNSPECIFIEDP (LFACE_STIPPLE (lface)))
    LFACE_STIPPLE (lface) = Qnil;

  /* Realize the face; it must be fully-specified now.  */
  xassert (lface_fully_specified_p (XVECTOR (lface)->contents));
  check_lface (lface);
  bcopy (XVECTOR (lface)->contents, attrs, sizeof attrs);
  face = realize_face (c, attrs, 0, NULL, DEFAULT_FACE_ID);
  return 1;
}


/* Realize basic faces other than the default face in face cache C.
   SYMBOL is the face name, ID is the face id the realized face must
   have.  The default face must have been realized already.  */

static void
realize_named_face (f, symbol, id)
     struct frame *f;
     Lisp_Object symbol;
     int id;
{
  struct face_cache *c = FRAME_FACE_CACHE (f);
  Lisp_Object lface = lface_from_face_name (f, symbol, 0);
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  Lisp_Object symbol_attrs[LFACE_VECTOR_SIZE];
  struct face *new_face;

  /* The default face must exist and be fully specified.  */
  get_lface_attributes (f, Qdefault, attrs, 1);
  check_lface_attrs (attrs);
  xassert (lface_fully_specified_p (attrs));

  /* If SYMBOL isn't know as a face, create it.  */
  if (NILP (lface))
    {
      Lisp_Object frame;
      XSETFRAME (frame, f);
      lface = Finternal_make_lisp_face (symbol, frame);
    }

  /* Merge SYMBOL's face with the default face.  */
  get_lface_attributes (f, symbol, symbol_attrs, 1);
  merge_face_vectors (f, symbol_attrs, attrs, Qnil);

  /* Realize the face.  */
  new_face = realize_face (c, attrs, 0, NULL, id);
}


/* Realize the fully-specified face with attributes ATTRS in face
   cache CACHE for character C.  If C is a multibyte character,
   BASE_FACE is a face that has the same attributes.  Otherwise,
   BASE_FACE is ignored.  If FORMER_FACE_ID is non-negative, it is an
   ID of face to remove before caching the new face.  Value is a
   pointer to the newly created realized face.  */

static struct face *
realize_face (cache, attrs, c, base_face, former_face_id)
     struct face_cache *cache;
     Lisp_Object *attrs;
     int c;
     struct face *base_face;
     int former_face_id;
{
  struct face *face;

  /* LFACE must be fully specified.  */
  xassert (cache != NULL);
  check_lface_attrs (attrs);

  if (former_face_id >= 0 && cache->used > former_face_id)
    {
      /* Remove the former face.  */
      struct face *former_face = cache->faces_by_id[former_face_id];
      uncache_face (cache, former_face);
      free_realized_face (cache->f, former_face);
    }

  if (FRAME_WINDOW_P (cache->f))
    face = realize_x_face (cache, attrs, c, base_face);
  else if (FRAME_TERMCAP_P (cache->f) || FRAME_MSDOS_P (cache->f))
    face = realize_tty_face (cache, attrs, c);
  else
    abort ();

  /* Insert the new face.  */
  cache_face (cache, face, lface_hash (attrs));
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (cache->f) && face->font == NULL)
    load_face_font (cache->f, face, c);
#endif  /* HAVE_WINDOW_SYSTEM */
  return face;
}


/* Realize the fully-specified face with attributes ATTRS in face
   cache CACHE for character C.  Do it for X frame CACHE->f.  If C is
   a multibyte character, BASE_FACE is a face that has the same
   attributes.  Otherwise, BASE_FACE is ignored.  If the new face
   doesn't share font with the default face, a fontname is allocated
   from the heap and set in `font_name' of the new face, but it is not
   yet loaded here.  Value is a pointer to the newly created realized
   face.  */

static struct face *
realize_x_face (cache, attrs, c, base_face)
     struct face_cache *cache;
     Lisp_Object *attrs;
     int c;
     struct face *base_face;
{
#ifdef HAVE_WINDOW_SYSTEM
  struct face *face, *default_face;
  struct frame *f;
  Lisp_Object stipple, overline, strike_through, box;

  xassert (FRAME_WINDOW_P (cache->f));
  xassert (SINGLE_BYTE_CHAR_P (c)
	   || base_face);

  /* Allocate a new realized face.  */
  face = make_realized_face (attrs);

  f = cache->f;

  /* If C is a multibyte character, we share all face attirbutes with
     BASE_FACE including the realized fontset.  But, we must load a
     different font.  */
  if (!SINGLE_BYTE_CHAR_P (c))
    {
      bcopy (base_face, face, sizeof *face);
      face->gc = 0;

      /* Don't try to free the colors copied bitwise from BASE_FACE.  */
      face->colors_copied_bitwise_p = 1;

      /* to force realize_face to load font */
      face->font = NULL;
      return face;
    }

  /* Now we are realizing a face for ASCII (and unibyte) characters.  */

  /* Determine the font to use.  Most of the time, the font will be
     the same as the font of the default face, so try that first.  */
  default_face = FACE_FROM_ID (f, DEFAULT_FACE_ID);
  if (default_face
      && FACE_SUITABLE_FOR_CHAR_P (default_face, c)
      && lface_same_font_attributes_p (default_face->lface, attrs))
    {
      face->font = default_face->font;
      face->fontset = default_face->fontset;
      face->font_info_id = default_face->font_info_id;
      face->font_name = default_face->font_name;
      face->ascii_face = face;

      /* But, as we can't share the fontset, make a new realized
	 fontset that has the same base fontset as of the default
	 face.  */
      face->fontset
	= make_fontset_for_ascii_face (f, default_face->fontset);
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

      if ((fontset == -1) && default_face)
	fontset = default_face->fontset;
      face->fontset = make_fontset_for_ascii_face (f, fontset);
      face->font = NULL;	/* to force realize_face to load font */

#ifdef MAC_OS
      /* Load the font if it is specified in ATTRS.  This fixes
         changing frame font on the Mac.  */
      if (STRINGP (attrs[LFACE_FONT_INDEX]))
        {
          struct font_info *font_info =
            FS_LOAD_FONT (f, 0, XSTRING (attrs[LFACE_FONT_INDEX])->data, -1);
          if (font_info)
            face->font = font_info->font;
        }
#endif
    }

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
      face->box_line_width = 1;
    }
  else if (INTEGERP (box))
    {
      /* Simple box of specified line width in foreground color of the
         face.  */
      xassert (XINT (box) != 0);
      face->box = FACE_SIMPLE_BOX;
      face->box_line_width = XINT (box);
      face->box_color = face->foreground;
      face->box_color_defaulted_p = 1;
    }
  else if (CONSP (box))
    {
      /* `(:width WIDTH :color COLOR :shadow SHADOW)'.  SHADOW
	 being one of `raised' or `sunken'.  */
      face->box = FACE_SIMPLE_BOX;
      face->box_color = face->foreground;
      face->box_color_defaulted_p = 1;
      face->box_line_width = 1;

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
	      if (INTEGERP (value) && XINT (value) != 0)
		face->box_line_width = XINT (value);
	    }
	  else if (EQ (keyword, QCcolor))
	    {
	      if (STRINGP (value))
		{
		  face->box_color = load_color (f, face, value,
						LFACE_BOX_INDEX);
		  face->use_box_color_for_shadows_p = 1;
		}
	    }
	  else if (EQ (keyword, QCstyle))
	    {
	      if (EQ (value, Qreleased_button))
		face->box = FACE_RAISED_BOX;
	      else if (EQ (value, Qpressed_button))
		face->box = FACE_SUNKEN_BOX;
	    }
	}
    }

  /* Text underline, overline, strike-through.  */

  if (EQ (attrs[LFACE_UNDERLINE_INDEX], Qt))
    {
      /* Use default color (same as foreground color).  */
      face->underline_p = 1;
      face->underline_defaulted_p = 1;
      face->underline_color = 0;
    }
  else if (STRINGP (attrs[LFACE_UNDERLINE_INDEX]))
    {
      /* Use specified color.  */
      face->underline_p = 1;
      face->underline_defaulted_p = 0;
      face->underline_color
	= load_color (f, face, attrs[LFACE_UNDERLINE_INDEX],
		      LFACE_UNDERLINE_INDEX);
    }
  else if (NILP (attrs[LFACE_UNDERLINE_INDEX]))
    {
      face->underline_p = 0;
      face->underline_defaulted_p = 0;
      face->underline_color = 0;
    }

  overline = attrs[LFACE_OVERLINE_INDEX];
  if (STRINGP (overline))
    {
      face->overline_color
	= load_color (f, face, attrs[LFACE_OVERLINE_INDEX],
		      LFACE_OVERLINE_INDEX);
      face->overline_p = 1;
    }
  else if (EQ (overline, Qt))
    {
      face->overline_color = face->foreground;
      face->overline_color_defaulted_p = 1;
      face->overline_p = 1;
    }

  strike_through = attrs[LFACE_STRIKE_THROUGH_INDEX];
  if (STRINGP (strike_through))
    {
      face->strike_through_color
	= load_color (f, face, attrs[LFACE_STRIKE_THROUGH_INDEX],
		      LFACE_STRIKE_THROUGH_INDEX);
      face->strike_through_p = 1;
    }
  else if (EQ (strike_through, Qt))
    {
      face->strike_through_color = face->foreground;
      face->strike_through_color_defaulted_p = 1;
      face->strike_through_p = 1;
    }

  stipple = attrs[LFACE_STIPPLE_INDEX];
  if (!NILP (stipple))
    face->stipple = load_pixmap (f, stipple, &face->pixmap_w, &face->pixmap_h);

  xassert (FACE_SUITABLE_FOR_CHAR_P (face, c));
  return face;
#endif /* HAVE_WINDOW_SYSTEM */
}


/* Map a specified color of face FACE on frame F to a tty color index.
   IDX is either LFACE_FOREGROUND_INDEX or LFACE_BACKGROUND_INDEX, and
   specifies which color to map.  Set *DEFAULTED to 1 if mapping to the
   default foreground/background colors.  */

static void
map_tty_color (f, face, idx, defaulted)
     struct frame *f;
     struct face *face;
     enum lface_attribute_index idx;
     int *defaulted;
{
  Lisp_Object frame, color, def;
  int foreground_p = idx == LFACE_FOREGROUND_INDEX;
  unsigned long default_pixel, default_other_pixel, pixel;

  xassert (idx == LFACE_FOREGROUND_INDEX || idx == LFACE_BACKGROUND_INDEX);

  if (foreground_p)
    {
      pixel = default_pixel = FACE_TTY_DEFAULT_FG_COLOR;
      default_other_pixel = FACE_TTY_DEFAULT_BG_COLOR;
    }
  else
    {
      pixel = default_pixel = FACE_TTY_DEFAULT_BG_COLOR;
      default_other_pixel = FACE_TTY_DEFAULT_FG_COLOR;
    }
  
  XSETFRAME (frame, f);
  color = face->lface[idx];
  
  if (STRINGP (color)
      && XSTRING (color)->size
      && CONSP (Vtty_defined_color_alist)
      && (def = assq_no_quit (color, call1 (Qtty_color_alist, frame)),
	  CONSP (def)))
    {
      /* Associations in tty-defined-color-alist are of the form
	 (NAME INDEX R G B).  We need the INDEX part.  */
      pixel = XINT (XCAR (XCDR (def)));
    }

  if (pixel == default_pixel && STRINGP (color))
    {
      pixel = load_color (f, face, color, idx);

#if defined (MSDOS) || defined (WINDOWSNT)
      /* If the foreground of the default face is the default color,
	 use the foreground color defined by the frame.  */
#ifdef MSDOS
      if (FRAME_MSDOS_P (f))
	{
#endif /* MSDOS */
	  if (pixel == default_pixel
	      || pixel == FACE_TTY_DEFAULT_COLOR)
	    {
	      if (foreground_p)
		pixel = FRAME_FOREGROUND_PIXEL (f);
	      else
		pixel = FRAME_BACKGROUND_PIXEL (f);
	      face->lface[idx] = tty_color_name (f, pixel);
	      *defaulted = 1;
	    }
	  else if (pixel == default_other_pixel)
	    {
	      if (foreground_p)
		pixel = FRAME_BACKGROUND_PIXEL (f);
	      else
		pixel = FRAME_FOREGROUND_PIXEL (f);
	      face->lface[idx] = tty_color_name (f, pixel);
	      *defaulted = 1;
	    }
#ifdef MSDOS
	}
#endif
#endif /* MSDOS or WINDOWSNT */
    }

  if (foreground_p)
    face->foreground = pixel;
  else
    face->background = pixel;
}


/* Realize the fully-specified face with attributes ATTRS in face
   cache CACHE for character C.  Do it for TTY frame CACHE->f.  Value is a
   pointer to the newly created realized face.  */

static struct face *
realize_tty_face (cache, attrs, c)
     struct face_cache *cache;
     Lisp_Object *attrs;
     int c;
{
  struct face *face;
  int weight, slant;
  int face_colors_defaulted = 0;
  struct frame *f = cache->f;

  /* Frame must be a termcap frame.  */
  xassert (FRAME_TERMCAP_P (cache->f) || FRAME_MSDOS_P (cache->f));

  /* Allocate a new realized face.  */
  face = make_realized_face (attrs);
  face->font_name = FRAME_MSDOS_P (cache->f) ? "ms-dos" : "tty";

  /* Map face attributes to TTY appearances.  We map slant to
     dimmed text because we want italic text to appear differently
     and because dimmed text is probably used infrequently.  */
  weight = face_numeric_weight (attrs[LFACE_WEIGHT_INDEX]);
  slant = face_numeric_slant (attrs[LFACE_SLANT_INDEX]);

  if (weight > XLFD_WEIGHT_MEDIUM)
    face->tty_bold_p = 1;
  if (weight < XLFD_WEIGHT_MEDIUM || slant != XLFD_SLANT_ROMAN)
    face->tty_dim_p = 1;
  if (!NILP (attrs[LFACE_UNDERLINE_INDEX]))
    face->tty_underline_p = 1;
  if (!NILP (attrs[LFACE_INVERSE_INDEX]))
    face->tty_reverse_p = 1;

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
    face->tty_bold_p = 0;

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
     (suppress)
     Lisp_Object suppress;
{
  tty_suppress_bold_inverse_default_colors_p = !NILP (suppress);
  ++face_change_count;
  return suppress;
}



/***********************************************************************
			   Computing Faces
 ***********************************************************************/

/* Return the ID of the face to use to display character CH with face
   property PROP on frame F in current_buffer.  */

int
compute_char_face (f, ch, prop)
     struct frame *f;
     int ch;
     Lisp_Object prop;
{
  int face_id;

  if (NILP (current_buffer->enable_multibyte_characters))
    ch = 0;

  if (NILP (prop))
    {
      struct face *face = FACE_FROM_ID (f, DEFAULT_FACE_ID);
      face_id = FACE_FOR_CHAR (f, face, ch);
    }
  else
    {
      Lisp_Object attrs[LFACE_VECTOR_SIZE];
      struct face *default_face = FACE_FROM_ID (f, DEFAULT_FACE_ID);
      bcopy (default_face->lface, attrs, sizeof attrs);
      merge_face_vector_with_property (f, attrs, prop);
      face_id = lookup_face (f, attrs, ch, NULL);
    }

  return face_id;
}

/* Return the face ID associated with buffer position POS for
   displaying ASCII characters.  Return in *ENDPTR the position at
   which a different face is needed, as far as text properties and
   overlays are concerned.  W is a window displaying current_buffer.

   REGION_BEG, REGION_END delimit the region, so it can be
   highlighted.

   LIMIT is a position not to scan beyond.  That is to limit the time
   this function can take.

   If MOUSE is non-zero, use the character's mouse-face, not its face.

   The face returned is suitable for displaying ASCII characters.  */

int
face_at_buffer_position (w, pos, region_beg, region_end,
			 endptr, limit, mouse)
     struct window *w;
     int pos;
     int region_beg, region_end;
     int *endptr;
     int limit;
     int mouse;
{
  struct frame *f = XFRAME (w->frame);
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  Lisp_Object prop, position;
  int i, noverlays;
  Lisp_Object *overlay_vec;
  Lisp_Object frame;
  int endpos;
  Lisp_Object propname = mouse ? Qmouse_face : Qface;
  Lisp_Object limit1, end;
  struct face *default_face;

  /* W must display the current buffer.  We could write this function
     to use the frame and buffer of W, but right now it doesn't.  */
  /* xassert (XBUFFER (w->buffer) == current_buffer); */

  XSETFRAME (frame, f);
  XSETFASTINT (position, pos);

  endpos = ZV;
  if (pos < region_beg && region_beg < endpos)
    endpos = region_beg;

  /* Get the `face' or `mouse_face' text property at POS, and
     determine the next position at which the property changes.  */
  prop = Fget_text_property (position, propname, w->buffer);
  XSETFASTINT (limit1, (limit < endpos ? limit : endpos));
  end = Fnext_single_property_change (position, propname, w->buffer, limit1);
  if (INTEGERP (end))
    endpos = XINT (end);

  /* Look at properties from overlays.  */
  {
    int next_overlay;
    int len;

    /* First try with room for 40 overlays.  */
    len = 40;
    overlay_vec = (Lisp_Object *) alloca (len * sizeof (Lisp_Object));
    noverlays = overlays_at (pos, 0, &overlay_vec, &len,
			     &next_overlay, NULL, 0);

    /* If there are more than 40, make enough space for all, and try
       again.  */
    if (noverlays > len)
      {
	len = noverlays;
	overlay_vec = (Lisp_Object *) alloca (len * sizeof (Lisp_Object));
	noverlays = overlays_at (pos, 0, &overlay_vec, &len,
				 &next_overlay, NULL, 0);
      }

    if (next_overlay < endpos)
      endpos = next_overlay;
  }

  *endptr = endpos;

  default_face = FACE_FROM_ID (f, DEFAULT_FACE_ID);

  /* Optimize common cases where we can use the default face.  */
  if (noverlays == 0
      && NILP (prop)
      && !(pos >= region_beg && pos < region_end))
    return DEFAULT_FACE_ID;

  /* Begin with attributes from the default face.  */
  bcopy (default_face->lface, attrs, sizeof attrs);

  /* Merge in attributes specified via text properties.  */
  if (!NILP (prop))
    merge_face_vector_with_property (f, attrs, prop);

  /* Now merge the overlay data.  */
  noverlays = sort_overlays (overlay_vec, noverlays, w);
  for (i = 0; i < noverlays; i++)
    {
      Lisp_Object oend;
      int oendpos;

      prop = Foverlay_get (overlay_vec[i], propname);
      if (!NILP (prop))
	merge_face_vector_with_property (f, attrs, prop);

      oend = OVERLAY_END (overlay_vec[i]);
      oendpos = OVERLAY_POSITION (oend);
      if (oendpos < endpos)
	endpos = oendpos;
    }

  /* If in the region, merge in the region face.  */
  if (pos >= region_beg && pos < region_end)
    {
      Lisp_Object region_face = lface_from_face_name (f, Qregion, 0);
      merge_face_vectors (f, XVECTOR (region_face)->contents, attrs, Qnil);

      if (region_end < endpos)
	endpos = region_end;
    }

  *endptr = endpos;

  /* Look up a realized face with the given face attributes,
     or realize a new one for ASCII characters.  */
  return lookup_face (f, attrs, 0, NULL);
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

   If MOUSE_P is non-zero, use the character's mouse-face, not its face.

   Set *ENDPTR to the next position where to check for faces in
   STRING; -1 if the face is constant from POS to the end of the
   string.

   Value is the id of the face to use.  The face returned is suitable
   for displaying ASCII characters.  */

int
face_at_string_position (w, string, pos, bufpos, region_beg,
			 region_end, endptr, base_face_id, mouse_p)
     struct window *w;
     Lisp_Object string;
     int pos, bufpos;
     int region_beg, region_end;
     int *endptr;
     enum face_id base_face_id;
     int mouse_p;
{
  Lisp_Object prop, position, end, limit;
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  struct face *base_face;
  int multibyte_p = STRING_MULTIBYTE (string);
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
  XSETFASTINT (limit, XSTRING (string)->size);
  end = Fnext_single_property_change (position, prop_name, string, limit);
  if (INTEGERP (end))
    *endptr = XFASTINT (end);
  else
    *endptr = -1;

  base_face = FACE_FROM_ID (f, base_face_id);
  xassert (base_face);

  /* Optimize the default case that there is no face property and we
     are not in the region.  */
  if (NILP (prop)
      && (base_face_id != DEFAULT_FACE_ID
	  /* BUFPOS <= 0 means STRING is not an overlay string, so
	     that the region doesn't have to be taken into account.  */
	  || bufpos <= 0
	  || bufpos < region_beg
	  || bufpos >= region_end)
      && (multibyte_p
	  /* We can't realize faces for different charsets differently
	     if we don't have fonts, so we can stop here if not working
	     on a window-system frame.  */
	  || !FRAME_WINDOW_P (f)
	  || FACE_SUITABLE_FOR_CHAR_P (base_face, 0)))
    return base_face->id;

  /* Begin with attributes from the base face.  */
  bcopy (base_face->lface, attrs, sizeof attrs);

  /* Merge in attributes specified via text properties.  */
  if (!NILP (prop))
    merge_face_vector_with_property (f, attrs, prop);

  /* If in the region, merge in the region face.  */
  if (bufpos
      && bufpos >= region_beg
      && bufpos < region_end)
    {
      Lisp_Object region_face = lface_from_face_name (f, Qregion, 0);
      merge_face_vectors (f, XVECTOR (region_face)->contents, attrs, Qnil);
    }

  /* Look up a realized face with the given face attributes,
     or realize a new one for ASCII characters.  */
  return lookup_face (f, attrs, 0, NULL);
}



/***********************************************************************
				Tests
 ***********************************************************************/

#if GLYPH_DEBUG

/* Print the contents of the realized face FACE to stderr.  */

static void
dump_realized_face (face)
     struct face *face;
{
  fprintf (stderr, "ID: %d\n", face->id);
#ifdef HAVE_X_WINDOWS
  fprintf (stderr, "gc: %d\n", (int) face->gc);
#endif
  fprintf (stderr, "foreground: 0x%lx (%s)\n",
	   face->foreground,
	   XSTRING (face->lface[LFACE_FOREGROUND_INDEX])->data);
  fprintf (stderr, "background: 0x%lx (%s)\n",
	   face->background,
	   XSTRING (face->lface[LFACE_BACKGROUND_INDEX])->data);
  fprintf (stderr, "font_name: %s (%s)\n",
	   face->font_name,
	   XSTRING (face->lface[LFACE_FAMILY_INDEX])->data);
#ifdef HAVE_X_WINDOWS
  fprintf (stderr, "font = %p\n", face->font);
#endif
  fprintf (stderr, "font_info_id = %d\n", face->font_info_id);
  fprintf (stderr, "fontset: %d\n", face->fontset);
  fprintf (stderr, "underline: %d (%s)\n",
	   face->underline_p,
	   XSTRING (Fsymbol_name (face->lface[LFACE_UNDERLINE_INDEX]))->data);
  fprintf (stderr, "hash: %d\n", face->hash);
  fprintf (stderr, "charset: %d\n", face->charset);
}


DEFUN ("dump-face", Fdump_face, Sdump_face, 0, 1, 0, doc: /* */)
     (n)
     Lisp_Object n;
{
  if (NILP (n))
    {
      int i;

      fprintf (stderr, "font selection order: ");
      for (i = 0; i < DIM (font_sort_order); ++i)
	fprintf (stderr, "%d ", font_sort_order[i]);
      fprintf (stderr, "\n");

      fprintf (stderr, "alternative fonts: ");
      debug_print (Vface_alternative_font_family_alist);
      fprintf (stderr, "\n");

      for (i = 0; i < FRAME_FACE_CACHE (SELECTED_FRAME ())->used; ++i)
	Fdump_face (make_number (i));
    }
  else
    {
      struct face *face;
      CHECK_NUMBER (n);
      face = FACE_FROM_ID (SELECTED_FRAME (), XINT (n));
      if (face == NULL)
	error ("Not a valid face");
      dump_realized_face (face);
    }

  return Qnil;
}


DEFUN ("show-face-resources", Fshow_face_resources, Sshow_face_resources,
       0, 0, 0, doc: /* */)
     ()
{
  fprintf (stderr, "number of colors = %d\n", ncolors_allocated);
  fprintf (stderr, "number of pixmaps = %d\n", npixmaps_allocated);
  fprintf (stderr, "number of GCs = %d\n", ngcs);
  return Qnil;
}

#endif /* GLYPH_DEBUG != 0 */



/***********************************************************************
			    Initialization
 ***********************************************************************/

void
syms_of_xfaces ()
{
  Qface = intern ("face");
  staticpro (&Qface);
  Qbitmap_spec_p = intern ("bitmap-spec-p");
  staticpro (&Qbitmap_spec_p);
  Qframe_update_face_colors = intern ("frame-update-face-colors");
  staticpro (&Qframe_update_face_colors);

  /* Lisp face attribute keywords.  */
  QCfamily = intern (":family");
  staticpro (&QCfamily);
  QCheight = intern (":height");
  staticpro (&QCheight);
  QCweight = intern (":weight");
  staticpro (&QCweight);
  QCslant = intern (":slant");
  staticpro (&QCslant);
  QCunderline = intern (":underline");
  staticpro (&QCunderline);
  QCinverse_video = intern (":inverse-video");
  staticpro (&QCinverse_video);
  QCreverse_video = intern (":reverse-video");
  staticpro (&QCreverse_video);
  QCforeground = intern (":foreground");
  staticpro (&QCforeground);
  QCbackground = intern (":background");
  staticpro (&QCbackground);
  QCstipple = intern (":stipple");;
  staticpro (&QCstipple);
  QCwidth = intern (":width");
  staticpro (&QCwidth);
  QCfont = intern (":font");
  staticpro (&QCfont);
  QCbold = intern (":bold");
  staticpro (&QCbold);
  QCitalic = intern (":italic");
  staticpro (&QCitalic);
  QCoverline = intern (":overline");
  staticpro (&QCoverline);
  QCstrike_through = intern (":strike-through");
  staticpro (&QCstrike_through);
  QCbox = intern (":box");
  staticpro (&QCbox);
  QCinherit = intern (":inherit");
  staticpro (&QCinherit);

  /* Symbols used for Lisp face attribute values.  */
  QCcolor = intern (":color");
  staticpro (&QCcolor);
  QCline_width = intern (":line-width");
  staticpro (&QCline_width);
  QCstyle = intern (":style");
  staticpro (&QCstyle);
  Qreleased_button = intern ("released-button");
  staticpro (&Qreleased_button);
  Qpressed_button = intern ("pressed-button");
  staticpro (&Qpressed_button);
  Qnormal = intern ("normal");
  staticpro (&Qnormal);
  Qultra_light = intern ("ultra-light");
  staticpro (&Qultra_light);
  Qextra_light = intern ("extra-light");
  staticpro (&Qextra_light);
  Qlight = intern ("light");
  staticpro (&Qlight);
  Qsemi_light = intern ("semi-light");
  staticpro (&Qsemi_light);
  Qsemi_bold = intern ("semi-bold");
  staticpro (&Qsemi_bold);
  Qbold = intern ("bold");
  staticpro (&Qbold);
  Qextra_bold = intern ("extra-bold");
  staticpro (&Qextra_bold);
  Qultra_bold = intern ("ultra-bold");
  staticpro (&Qultra_bold);
  Qoblique = intern ("oblique");
  staticpro (&Qoblique);
  Qitalic = intern ("italic");
  staticpro (&Qitalic);
  Qreverse_oblique = intern ("reverse-oblique");
  staticpro (&Qreverse_oblique);
  Qreverse_italic = intern ("reverse-italic");
  staticpro (&Qreverse_italic);
  Qultra_condensed = intern ("ultra-condensed");
  staticpro (&Qultra_condensed);
  Qextra_condensed = intern ("extra-condensed");
  staticpro (&Qextra_condensed);
  Qcondensed = intern ("condensed");
  staticpro (&Qcondensed);
  Qsemi_condensed = intern ("semi-condensed");
  staticpro (&Qsemi_condensed);
  Qsemi_expanded = intern ("semi-expanded");
  staticpro (&Qsemi_expanded);
  Qexpanded = intern ("expanded");
  staticpro (&Qexpanded);
  Qextra_expanded = intern ("extra-expanded");
  staticpro (&Qextra_expanded);
  Qultra_expanded = intern ("ultra-expanded");
  staticpro (&Qultra_expanded);
  Qbackground_color = intern ("background-color");
  staticpro (&Qbackground_color);
  Qforeground_color = intern ("foreground-color");
  staticpro (&Qforeground_color);
  Qunspecified = intern ("unspecified");
  staticpro (&Qunspecified);

  Qface_alias = intern ("face-alias");
  staticpro (&Qface_alias);
  Qdefault = intern ("default");
  staticpro (&Qdefault);
  Qtool_bar = intern ("tool-bar");
  staticpro (&Qtool_bar);
  Qregion = intern ("region");
  staticpro (&Qregion);
  Qfringe = intern ("fringe");
  staticpro (&Qfringe);
  Qheader_line = intern ("header-line");
  staticpro (&Qheader_line);
  Qscroll_bar = intern ("scroll-bar");
  staticpro (&Qscroll_bar);
  Qmenu = intern ("menu");
  staticpro (&Qmenu);
  Qcursor = intern ("cursor");
  staticpro (&Qcursor);
  Qborder = intern ("border");
  staticpro (&Qborder);
  Qmouse = intern ("mouse");
  staticpro (&Qmouse);
  Qmode_line_inactive = intern ("mode-line-inactive");
  staticpro (&Qmode_line_inactive);
  Qtty_color_desc = intern ("tty-color-desc");
  staticpro (&Qtty_color_desc);
  Qtty_color_by_index = intern ("tty-color-by-index");
  staticpro (&Qtty_color_by_index);
  Qtty_color_alist = intern ("tty-color-alist");
  staticpro (&Qtty_color_alist);
  Qscalable_fonts_allowed = intern ("scalable-fonts-allowed");
  staticpro (&Qscalable_fonts_allowed);

  Vparam_value_alist = Fcons (Fcons (Qnil, Qnil), Qnil);
  staticpro (&Vparam_value_alist);
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
  defsubr (&Sface_attribute_relative_p);
  defsubr (&Smerge_face_attribute);
  defsubr (&Sinternal_get_lisp_face_attribute);
  defsubr (&Sinternal_lisp_face_attribute_values);
  defsubr (&Sinternal_lisp_face_equal_p);
  defsubr (&Sinternal_lisp_face_empty_p);
  defsubr (&Sinternal_copy_lisp_face);
  defsubr (&Sinternal_merge_in_global_face);
  defsubr (&Sface_font);
  defsubr (&Sframe_face_alist);
  defsubr (&Sinternal_set_font_selection_order);
  defsubr (&Sinternal_set_alternative_font_family_alist);
  defsubr (&Sinternal_set_alternative_font_registry_alist);
  defsubr (&Sface_attributes_as_vector);
#if GLYPH_DEBUG
  defsubr (&Sdump_face);
  defsubr (&Sshow_face_resources);
#endif /* GLYPH_DEBUG */
  defsubr (&Sclear_face_cache);
  defsubr (&Stty_suppress_bold_inverse_default_colors);

#if defined DEBUG_X_COLORS && defined HAVE_X_WINDOWS
  defsubr (&Sdump_colors);
#endif

  DEFVAR_LISP ("font-list-limit", &Vfont_list_limit,
	       doc: /* *Limit for font matching.
If an integer > 0, font matching functions won't load more than
that number of fonts when searching for a matching font.  */);
  Vfont_list_limit = make_number (DEFAULT_FONT_LIST_LIMIT);

  DEFVAR_LISP ("face-new-frame-defaults", &Vface_new_frame_defaults,
    doc: /* List of global face definitions (for internal use only.)  */);
  Vface_new_frame_defaults = Qnil;

  DEFVAR_LISP ("face-default-stipple", &Vface_default_stipple,
    doc: /* *Default stipple pattern used on monochrome displays.
This stipple pattern is used on monochrome displays
instead of shades of gray for a face background color.
See `set-face-stipple' for possible values for this variable.  */);
  Vface_default_stipple = build_string ("gray3");

  DEFVAR_LISP ("tty-defined-color-alist", &Vtty_defined_color_alist,
   doc: /* An alist of defined terminal colors and their RGB values.  */);
  Vtty_defined_color_alist = Qnil;

  DEFVAR_LISP ("scalable-fonts-allowed", &Vscalable_fonts_allowed,
	       doc: /* Allowed scalable fonts.
A value of nil means don't allow any scalable fonts.
A value of t means allow any scalable font.
Otherwise, value must be a list of regular expressions.  A font may be
scaled if its name matches a regular expression in the list.
Note that if value is nil, a scalable font might still be used, if no
other font of the appropriate family and registry is available.  */);
  Vscalable_fonts_allowed = Qnil;

  DEFVAR_LISP ("face-ignored-fonts", &Vface_ignored_fonts,
	       doc: /* List of ignored fonts.
Each element is a regular expression that matches names of fonts to
ignore.  */);
  Vface_ignored_fonts = Qnil;

#ifdef HAVE_WINDOW_SYSTEM
  defsubr (&Sbitmap_spec_p);
  defsubr (&Sx_list_fonts);
  defsubr (&Sinternal_face_x_get_resource);
  defsubr (&Sx_family_fonts);
  defsubr (&Sx_font_family_list);
#endif /* HAVE_WINDOW_SYSTEM */
}
