/* xfaces.c -- "Face" primitives.
   Copyright (C) 1993, 1994, 1998, 1999 Free Software Foundation.

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

   1. Font family or fontset alias name.
   
   2. Relative proportionate width, aka character set width or set
   width (swidth), e.g. `semi-compressed'.
   
   3. Font height in 1/10pt
   
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

   Faces are frame-local by nature because Emacs allows to define the
   same named face (face names are symbols) differently for different
   frames.  Each frame has an alist of face definitions for all named
   faces.  The value of a named face in such an alist is a Lisp vector
   with the symbol `face' in slot 0, and a slot for each each of the
   face attributes mentioned above.

   There is also a global face alist `Vface_new_frame_defaults'.  Face
   definitions from this list are used to initialize faces of newly
   created frames.
   
   A face doesn't have to specify all attributes.  Those not specified
   have a value of `unspecified'.  Faces specifying all attributes are
   called `fully-specified'.


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

   Face realization is done in the context of the charset of the
   character to display because different fonts and encodings are used
   for different charsets.  In other words, for characters of
   different charsets, different realized faces are needed to display
   them.

   Faces are always realized for a specific character set and contain
   a specific font, even if the face being realized specifies a
   fontset (see `font selection' below).  The reason is that the
   result of the new font selection stage is better than what can be
   done with statically defined font name patterns in fontsets.


   Unibyte text.

   In unibyte text, Emacs' charsets aren't applicable; function
   `char-charset' reports CHARSET_ASCII for all characters, including
   those > 0x7f.  The X registry and encoding of fonts to use is
   determined from the variable `x-unibyte-registry-and-encoding' in
   this case.  The variable is initialized at Emacs startup time from
   the font the user specified for Emacs.

   Currently all unibyte text, i.e. all buffers with
   enable_multibyte_characters nil are displayed with fonts of the
   same registry and encoding `x-unibyte-registry-and-encoding'.  This
   is consistent with the fact that languages can also be set
   globally, only.
   

   Font selection.

   Font selection tries to find the best available matching font for a
   given (charset, face) combination.  This is done slightly
   differently for faces specifying a fontset, or a font family name.

   If the face specifies a fontset alias name, that fontset determines
   a pattern for fonts of the given charset.  If the face specifies a
   font family, a font pattern is constructed.  Charset symbols have a
   property `x-charset-registry' for that purpose that maps a charset
   to an XLFD registry and encoding in the font pattern constructed.

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

   2. Setting face-alternative-font-family-alist allows the user to
   specify alternative font families to try if a family specified by a
   face doesn't exist.


   Composite characters.  
   
   Realized faces for composite characters are the only ones having a
   fontset id >= 0.  When a composite character is encoded into a
   sequence of non-composite characters (in xterm.c), a suitable font
   for the non-composite characters is then selected and realized,
   i.e.  the realization process is delayed but in principle the same.

   
   Initialization of basic faces.

   The faces `default', `modeline' are considered `basic faces'.
   When redisplay happens the first time for a newly created frame,
   basic faces are realized for CHARSET_ASCII.  Frame parameters are
   used to fill in unspecified attributes of the default face.  */

/* Define SCALABLE_FONTS to a non-zero value to enable scalable
   font use. Define it to zero to disable scalable font use.

   Use of too many or too large scalable fonts can crash XFree86
   servers.  That's why I've put the code dealing with scalable fonts
   in #if's.  */

#define SCALABLE_FONTS 1

#include <config.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "lisp.h"
#include "charset.h"
#include "frame.h"

#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#include "fontset.h"
#ifdef USE_MOTIF
#include <Xm/Xm.h>
#include <Xm/XmStrDefs.h>
#endif /* USE_MOTIF */
#endif

#ifdef MSDOS
#include "dosfns.h"
#endif

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
#include "keyboard.h"

#ifndef max
#define max(A, B)	((A) > (B) ? (A) : (B))
#define min(A, B)	((A) < (B) ? (A) : (B))
#define abs(X)		((X) < 0 ? -(X) : (X))
#endif

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
typedef struct {
  unsigned long pixel;
  unsigned short red, green, blue;
  char flags;
  char pad;
} XColor;
#endif

/* Keyword symbols used for face attribute names.  */

Lisp_Object QCfamily, QCheight, QCweight, QCslant, QCunderline;
Lisp_Object QCinverse_video, QCforeground, QCbackground, QCstipple;
Lisp_Object QCwidth, QCfont, QCbold, QCitalic;
Lisp_Object QCreverse_video;
Lisp_Object QCoverline, QCstrike_through, QCbox;

/* Symbols used for attribute values.  */

Lisp_Object Qnormal, Qbold, Qultra_light, Qextra_light, Qlight;
Lisp_Object Qsemi_light, Qsemi_bold, Qextra_bold, Qultra_bold;
Lisp_Object Qoblique, Qitalic, Qreverse_oblique, Qreverse_italic;
Lisp_Object Qultra_condensed, Qextra_condensed, Qcondensed;
Lisp_Object Qsemi_condensed, Qsemi_expanded, Qexpanded, Qextra_expanded;
Lisp_Object Qultra_expanded;
Lisp_Object Qreleased_button, Qpressed_button;
Lisp_Object QCstyle, QCcolor, QCline_width;
Lisp_Object Qunspecified, Qunspecified_fg, Qunspecified_bg;

/* The symbol `x-charset-registry'.  This property of charsets defines
   the X registry and encoding that fonts should have that are used to
   display characters of that charset.  */

Lisp_Object Qx_charset_registry;

/* The name of the function to call when the background of the frame
   has changed, frame_update_face_colors.  */

Lisp_Object Qframe_update_face_colors;

/* Names of basic faces.  */

Lisp_Object Qdefault, Qtool_bar, Qregion, Qfringe;
Lisp_Object Qheader_line, Qscroll_bar, Qcursor, Qborder, Qmouse, Qmenu;
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

/* Default registry and encoding to use for charsets whose charset
   symbols don't specify one.  */

Lisp_Object Vface_default_registry;

/* Alist of alternative font families.  Each element is of the form
   (FAMILY FAMILY1 FAMILY2 ...).  If fonts of FAMILY can't be loaded,
   try FAMILY1, then FAMILY2, ...  */

Lisp_Object Vface_alternative_font_family_alist;

/* Allowed scalable fonts.  A value of nil means don't allow any
   scalable fonts.  A value of t means allow the use of any scalable
   font.  Otherwise, value must be a list of regular expressions.  A
   font may be scaled if its name matches a regular expression in the
   list.  */

#if SCALABLE_FONTS
Lisp_Object Vscalable_fonts_allowed;
#endif

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

/* tty color-related functions (defined on lisp/term/tty-colors.el).  */
Lisp_Object Qtty_color_desc, Qtty_color_by_index;

/* Counter for calls to clear_face_cache.  If this counter reaches
   CLEAR_FONT_TABLE_COUNT, and a frame has more than
   CLEAR_FONT_TABLE_NFONTS load, unused fonts are freed.  */

static int clear_font_table_count;
#define CLEAR_FONT_TABLE_COUNT	100
#define CLEAR_FONT_TABLE_NFONTS	10

/* Non-zero means face attributes have been changed since the last
   redisplay.  Used in redisplay_internal.  */

int face_change_count;

/* The total number of colors currently allocated.  */

#if GLYPH_DEBUG
static int ncolors_allocated;
static int npixmaps_allocated;
static int ngcs;
#endif



/* Function prototypes.  */

struct font_name;
struct table_entry;

static Lisp_Object resolve_face_name P_ ((Lisp_Object));
static int may_use_scalable_font_p P_ ((struct font_name *, char *));
static void set_font_frame_param P_ ((Lisp_Object, Lisp_Object));
static int better_font_p P_ ((int *, struct font_name *, struct font_name *,
			      int));
static int first_font_matching P_ ((struct frame *f, char *,
				    struct font_name *));
static int x_face_list_fonts P_ ((struct frame *, char *,
				  struct font_name *, int, int, int));
static int font_scalable_p P_ ((struct font_name *));
static Lisp_Object deduce_unibyte_registry P_ ((struct frame *, char *));
static int get_lface_attributes P_ ((struct frame *, Lisp_Object, Lisp_Object *, int));
static int load_pixmap P_ ((struct frame *, Lisp_Object, unsigned *, unsigned *));
static char *xstrdup P_ ((char *));
static unsigned char *xstrlwr P_ ((unsigned char *));
static void signal_error P_ ((char *, Lisp_Object));
static struct frame *frame_or_selected_frame P_ ((Lisp_Object, int));
static void load_face_font_or_fontset P_ ((struct frame *, struct face *, char *, int));
static void load_face_colors P_ ((struct frame *, struct face *, Lisp_Object *));
static void free_face_colors P_ ((struct frame *, struct face *));
static int face_color_gray_p P_ ((struct frame *, char *));
static char *build_font_name P_ ((struct font_name *));
static void free_font_names P_ ((struct font_name *, int));
static int sorted_font_list P_ ((struct frame *, char *,
				 int (*cmpfn) P_ ((const void *, const void *)),
				 struct font_name **));
static int font_list P_ ((struct frame *, char *, char *, char *, struct font_name **));
static int try_font_list P_ ((struct frame *, Lisp_Object *, char *, char *, char *,
			      struct font_name **));
static int cmp_font_names P_ ((const void *, const void *));
static struct face *realize_face P_ ((struct face_cache *,
				      Lisp_Object *, int));
static struct face *realize_x_face P_ ((struct face_cache *,
					Lisp_Object *, int));
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
static int face_fontset P_ ((struct frame *, Lisp_Object *));
static char *choose_face_font P_ ((struct frame *, Lisp_Object *, int,
				   Lisp_Object));
static char *choose_face_fontset_font P_ ((struct frame *, Lisp_Object *,
					   int, int));
static void merge_face_vectors P_ ((Lisp_Object *from, Lisp_Object *));
static void merge_face_vector_with_property P_ ((struct frame *, Lisp_Object *,
						 Lisp_Object));
static int set_lface_from_font_name P_ ((struct frame *, Lisp_Object, char *,
					 int, int));
static Lisp_Object lface_from_face_name P_ ((struct frame *, Lisp_Object, int));
static struct face *make_realized_face P_ ((Lisp_Object *, int, Lisp_Object));
static void free_realized_faces P_ ((struct face_cache *));
static char *best_matching_font P_ ((struct frame *, Lisp_Object *,
				     struct font_name *, int));
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
					    struct font_name *, int, int));
static struct table_entry *xlfd_lookup_field_contents P_ ((struct table_entry *, int,
							   struct font_name *, int));

#ifdef HAVE_X_WINDOWS

static int split_font_name P_ ((struct frame *, struct font_name *, int));
static int xlfd_point_size P_ ((struct frame *, struct font_name *));
static void sort_fonts P_ ((struct frame *, struct font_name *, int,
			       int (*cmpfn) P_ ((const void *, const void *))));
static GC x_create_gc P_ ((struct frame *, unsigned long, XGCValues *));
static void x_free_gc P_ ((struct frame *, GC));
static void clear_font_table P_ ((struct frame *));

#endif /* HAVE_X_WINDOWS */


/***********************************************************************
			      Utilities
 ***********************************************************************/

#ifdef HAVE_X_WINDOWS

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


/* Like strdup, but uses xmalloc.  */

static char *
xstrdup (s)
     char *s;
{
  int len = strlen (s) + 1;
  char *p = (char *) xmalloc (len);
  bcopy (s, p, len);
  return p;
}


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
  
  CHECK_LIVE_FRAME (frame, nparam);
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
      
#ifdef HAVE_X_WINDOWS
  /* Make the image cache.  */
  if (FRAME_X_P (f))
    {
      if (FRAME_X_IMAGE_CACHE (f) == NULL)
	FRAME_X_IMAGE_CACHE (f) = make_image_cache ();
      ++FRAME_X_IMAGE_CACHE (f)->refcount;
    }
#endif /* HAVE_X_WINDOWS */

  /* Realize basic faces.  Must have enough information in frame 
     parameters to realize basic faces at this point.  */
#ifdef HAVE_X_WINDOWS
  if (!FRAME_X_P (f) || FRAME_X_WINDOW (f))
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

#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (f))
    {
      struct image_cache *image_cache = FRAME_X_IMAGE_CACHE (f);
      if (image_cache)
	{
	  --image_cache->refcount;
	  if (image_cache->refcount == 0)
	    free_image_cache (f);
	}
    }
#endif /* HAVE_X_WINDOWS */
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
#ifdef HAVE_X_WINDOWS
  Lisp_Object tail, frame;
  struct frame *f;

  if (clear_fonts_p
      || ++clear_font_table_count == CLEAR_FONT_TABLE_COUNT)
    {
      /* From time to time see if we can unload some fonts.  This also
	 frees all realized faces on all frames.  Fonts needed by
	 faces will be loaded again when faces are realized again.  */
      clear_font_table_count = 0;

      FOR_EACH_FRAME (tail, frame)
	{
	  f = XFRAME (frame);
	  if (FRAME_X_P (f)
	      && FRAME_X_DISPLAY_INFO (f)->n_fonts > CLEAR_FONT_TABLE_NFONTS)
	    {
	      free_all_realized_faces (frame);
	      clear_font_table (f);
	    }
	}
    }
  else
    {
      /* Clear GCs of realized faces.  */
      FOR_EACH_FRAME (tail, frame)
	{
	  f = XFRAME (frame);
	  if (FRAME_X_P (f))
	    {
	      clear_face_gcs (FRAME_FACE_CACHE (f));
	      clear_image_cache (f, 0);
	    }
	}
    }
#endif /* HAVE_X_WINDOWS */
}


DEFUN ("clear-face-cache", Fclear_face_cache, Sclear_face_cache, 0, 1, 0,
  "Clear face caches on all frames.\n\
Optional THOROUGHLY non-nil means try to free unused fonts, too.")
  (thorougly)
     Lisp_Object thorougly;
{
  clear_face_cache (!NILP (thorougly));
  return Qnil;
}



#ifdef HAVE_X_WINDOWS


/* Remove those fonts from the font table of frame F that are not used
   by fontsets.  Called from clear_face_cache from time to time.  */

static void
clear_font_table (f)
     struct frame *f;
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  char *used;
  Lisp_Object rest, frame;
  int i;

  xassert (FRAME_X_P (f));

  used = (char *) alloca (dpyinfo->n_fonts * sizeof *used);
  bzero (used, dpyinfo->n_fonts * sizeof *used);

  /* For all frames with the same x_display_info as F, record
     in `used' those fonts that are in use by fontsets.  */
  FOR_EACH_FRAME (rest, frame)
    if (FRAME_X_DISPLAY_INFO (XFRAME (frame)) == dpyinfo)
      {
	struct frame *f = XFRAME (frame);
	struct fontset_data *fontset_data = FRAME_FONTSET_DATA (f);

	for (i = 0; i < fontset_data->n_fontsets; ++i)
	  {
	    struct fontset_info *info = fontset_data->fontset_table[i];
	    int j;
	    
	    for (j = 0; j <= MAX_CHARSET; ++j)
	      {
		int idx = info->font_indexes[j];
		if (idx >= 0)
		  used[idx] = 1;
	      }
	  }
      }

  /* Free those fonts that are not used by fontsets.  */
  for (i = 0; i < dpyinfo->n_fonts; ++i)
    if (used[i] == 0 && dpyinfo->font_table[i].name)
      {
	struct font_info *font_info = dpyinfo->font_table + i;

	/* Free names.  In xfns.c there is a comment that full_name
	   should never be freed because it is always shared with
	   something else.  I don't think this is true anymore---see
	   x_load_font.  It's either equal to font_info->name or
	   allocated via xmalloc, and there seems to be no place in
	   the source files where full_name is transferred to another
	   data structure.  */
	if (font_info->full_name != font_info->name)
	  xfree (font_info->full_name);
	xfree (font_info->name);

	/* Free the font.  */
	BLOCK_INPUT;
	XFreeFont (dpyinfo->display, font_info->font);
	UNBLOCK_INPUT;

	/* Mark font table slot free.  */
	font_info->font = NULL;
	font_info->name = font_info->full_name = NULL;
      }
}


#endif /* HAVE_X_WINDOWS */



/***********************************************************************
			      X Pixmaps
 ***********************************************************************/

#ifdef HAVE_X_WINDOWS

DEFUN ("bitmap-spec-p", Fbitmap_spec_p, Sbitmap_spec_p, 1, 1, 0,
  "Value is non-nil if OBJECT is a valid bitmap specification.\n\
A bitmap specification is either a string, a file name, or a list\n\
(WIDTH HEIGHT DATA) where WIDTH is the pixel width of the bitmap,\n\
HEIGHT is its height, and DATA is a string containing the bits of\n\
the pixmap.  Bits are stored row by row, each row occupies\n\
(WIDTH + 7)/8 bytes.")
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
	  if (STRING_BYTES (XSTRING (data)) >= bytes_per_row * height)
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

#endif /* HAVE_X_WINDOWS */



/***********************************************************************
			 Minimum font bounds
 ***********************************************************************/

#ifdef HAVE_X_WINDOWS

/* Update the line_height of frame F.  Return non-zero if line height
   changes.  */

int
frame_update_line_height (f)
     struct frame *f;
{
  int fontset, line_height, changed_p;
  
  fontset = f->output_data.x->fontset;
  if (fontset > 0)
    line_height = FRAME_FONTSET_DATA (f)->fontset_table[fontset]->height;
  else
    line_height = FONT_HEIGHT (f->output_data.x->font);
  
  changed_p = line_height != f->output_data.x->line_height;
  f->output_data.x->line_height = line_height;
  return changed_p;
}

#endif /* HAVE_X_WINDOWS */


/***********************************************************************
				Fonts
 ***********************************************************************/

#ifdef HAVE_X_WINDOWS

/* Load font or fontset of face FACE which is used on frame F.
   FONTSET is the fontset FACE should use or -1, if FACE doesn't use a
   fontset.  FONT_NAME is the name of the font to load, if no fontset
   is used.  It is null if no suitable font name could be determined
   for the face.  */

static void
load_face_font_or_fontset (f, face, font_name, fontset)
     struct frame *f;
     struct face *face;
     char *font_name;
     int fontset;
{
  struct font_info *font_info = NULL;

  face->font_info_id = -1;
  face->fontset = fontset;
  face->font = NULL;
  
  BLOCK_INPUT;
  if (fontset >= 0)
    font_info = FS_LOAD_FONT (f, FRAME_X_FONT_TABLE (f), CHARSET_ASCII,
			      NULL, fontset);
  else if (font_name)
    font_info = FS_LOAD_FONT (f, FRAME_X_FONT_TABLE (f), face->charset,
			      font_name, -1);
  UNBLOCK_INPUT;

  if (font_info)
    {
      char *s;
      int i;
      
      face->font_info_id = FONT_INFO_ID (f, font_info);
      face->font = font_info->font;
      face->font_name = font_info->full_name;

      /* Make the registry part of the font name readily accessible.
	 The registry is used to find suitable faces for unibyte text.  */
      s = font_info->full_name + strlen (font_info->full_name);
      i = 0;
      while (i < 2 && --s >= font_info->full_name)
	if (*s == '-')
	  ++i;

      if (!STRINGP (face->registry)
	  || xstricmp (XSTRING (face->registry)->data, s + 1) != 0)
	{
	  if (STRINGP (Vface_default_registry)
	      && !xstricmp (XSTRING (Vface_default_registry)->data, s + 1))
	    face->registry = Vface_default_registry;
	  else
	    face->registry = build_string (s + 1);
	}
    }
  else if (fontset >= 0)
    add_to_log ("Unable to load ASCII font of fontset %d",
		make_number (fontset), Qnil);
  else if (font_name)
    add_to_log ("Unable to load font %s",
		build_string (font_name), Qnil);
}

#endif /* HAVE_X_WINDOWS */



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
  int color_idx = FACE_TTY_DEFAULT_COLOR, red = 0, green = 0, blue = 0;
  int status = 1;

  if (*color_name && !NILP (Ffboundp (Qtty_color_desc)))
    {
      status = 0;
      color_desc = call1 (Qtty_color_desc, build_string (color_name));
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
      else if (NILP (Fsymbol_value (intern ("tty-color-alist"))))
	/* We were called early during startup, and the colors are not
	   yet set up in tty-color-alist.  Don't return a failure
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

  color_def->pixel = (unsigned long) color_idx;
  color_def->red = red;
  color_def->green = green;
  color_def->blue = blue;

  return status;
}

/* Decide if color named COLOR is valid for the display associated
   with the frame F; if so, return the rgb values in COLOR_DEF.  If
   ALLOC is nonzero, allocate a new colormap cell.

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
    /* FIXME: w32_defined_color doesn't exist!  w32fns.c defines
       defined_color which needs to be renamed, and the declaration
       of color_def therein should be changed.  */
    return w32_defined_color (f, color_name, color_def, alloc);
#endif
#ifdef macintosh
  else if (FRAME_MAC_P (f))
    /* FIXME: mac_defined_color doesn't exist!  */
    return mac_defined_color (f, color_name, color_def, alloc);
#endif
  else
    abort ();
}

/* Given the index of the tty color, return its name, a Lisp string.  */

Lisp_Object
tty_color_name (f, idx)
     struct frame *f;
     int idx;
{
  char *color;

  if (idx >= 0 && !NILP (Ffboundp (Qtty_color_by_index)))
    {
      Lisp_Object coldesc = call1 (Qtty_color_by_index, make_number (idx));

      if (!NILP (coldesc))
	return XCAR (coldesc);
    }
#ifdef MSDOS
  /* We can have an MSDOG frame under -nw for a short window of
     opportunity before internal_terminal_init is called.  DTRT.  */
  if (FRAME_MSDOS_P (f) && !inhibit_window_system)
    return msdos_stdcolor_name (idx);
#endif

#ifdef WINDOWSNT
  /* FIXME: When/if w32 supports colors in non-window mode, there should
     be a call here to a w32-specific function that returns the color
     by index using the default color mapping on a Windows console.  */
#endif

  return
    idx == FACE_TTY_DEFAULT_FG_COLOR ? Qunspecified_fg
    : idx == FACE_TTY_DEFAULT_BG_COLOR ? Qunspecified_bg
    : Qunspecified;
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
  return ((FRAME_WINDOW_P (f)
	  && (!NILP (Fxw_display_color_p (frame))
	      || xstricmp (color_name, "black") == 0
	      || xstricmp (color_name, "white") == 0
	      || (background_p
		  && face_color_gray_p (f, color_name))
	      || (!NILP (Fx_display_grayscale_p (frame))
		  && face_color_gray_p (f, color_name))))
	  || tty_defined_color (f, color_name, &not_used, 0));
}


DEFUN ("face-color-gray-p", Fface_color_gray_p, Sface_color_gray_p, 1, 2, 0,
  "Return non-nil if COLOR is a shade of gray (or white or black).\n\
FRAME specifies the frame and thus the display for interpreting COLOR.\n\
If FRAME is nil or omitted, use the selected frame.")
   (color, frame)
     Lisp_Object color, frame;
{
  struct frame *f;

  CHECK_FRAME (frame, 0);
  CHECK_STRING (color, 0);
  f = XFRAME (frame);
  return face_color_gray_p (f, XSTRING (color)->data) ? Qt : Qnil;
}


DEFUN ("face-color-supported-p", Fface_color_supported_p,
       Sface_color_supported_p, 2, 3, 0,
  "Return non-nil if COLOR can be displayed on FRAME.\n\
BACKGROUND-P non-nil means COLOR is used as a background.\n\
If FRAME is nil or omitted, use the selected frame.\n\
COLOR must be a valid color name.")
   (frame, color, background_p)
     Lisp_Object frame, color, background_p;
{
  struct frame *f;

  CHECK_FRAME (frame, 0);
  CHECK_STRING (color, 0);
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

#ifdef HAVE_X_WINDOWS

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
  Display *dpy = FRAME_X_DISPLAY (f);
  int class = FRAME_X_DISPLAY_INFO (f)->visual->class;

  if (pixel == BLACK_PIX_DEFAULT (f)
      || pixel == WHITE_PIX_DEFAULT (f))
    return;

  BLOCK_INPUT;
  
  /* If display has an immutable color map, freeing colors is not
     necessary and some servers don't allow it.  So don't do it.  */
  if (! (class == StaticColor || class == StaticGray || class == TrueColor))
    {
      Colormap cmap = DefaultColormapOfScreen (FRAME_X_SCREEN (f));
      XFreeColors (dpy, cmap, &pixel, 1, 0);
    }
  
  UNBLOCK_INPUT;
}


/* Free colors allocated for FACE.  */

static void
free_face_colors (f, face)
     struct frame *f;
     struct face *face;
{
  int class = FRAME_X_DISPLAY_INFO (f)->visual->class;
  
  /* If display has an immutable color map, freeing colors is not
     necessary and some servers don't allow it.  So don't do it.  */
  if (class != StaticColor
      && class != StaticGray
      && class != TrueColor)
    {
      Display *dpy;
      Colormap cmap;
      
      BLOCK_INPUT;
      dpy = FRAME_X_DISPLAY (f);
      cmap = DefaultColormapOfScreen (FRAME_X_SCREEN (f));
      
      if (face->foreground != BLACK_PIX_DEFAULT (f)
	  && face->foreground != WHITE_PIX_DEFAULT (f)
	  && !face->foreground_defaulted_p)
	{
	  XFreeColors (dpy, cmap, &face->foreground, 1, 0);
	  IF_DEBUG (--ncolors_allocated);
	}
      
      if (face->background != BLACK_PIX_DEFAULT (f)
	  && face->background != WHITE_PIX_DEFAULT (f)
	  && !face->background_defaulted_p)
	{
	  XFreeColors (dpy, cmap, &face->background, 1, 0);
	  IF_DEBUG (--ncolors_allocated);
	}

      if (face->underline_p
	  && !face->underline_defaulted_p
	  && face->underline_color != BLACK_PIX_DEFAULT (f)
	  && face->underline_color != WHITE_PIX_DEFAULT (f))
	{
	  XFreeColors (dpy, cmap, &face->underline_color, 1, 0);
	  IF_DEBUG (--ncolors_allocated);
	}

      if (face->overline_p
	  && !face->overline_color_defaulted_p
	  && face->overline_color != BLACK_PIX_DEFAULT (f)
	  && face->overline_color != WHITE_PIX_DEFAULT (f))
	{
	  XFreeColors (dpy, cmap, &face->overline_color, 1, 0);
	  IF_DEBUG (--ncolors_allocated);
	}

      if (face->strike_through_p
	  && !face->strike_through_color_defaulted_p
	  && face->strike_through_color != BLACK_PIX_DEFAULT (f)
	  && face->strike_through_color != WHITE_PIX_DEFAULT (f))
	{
	  XFreeColors (dpy, cmap, &face->strike_through_color, 1, 0);
	  IF_DEBUG (--ncolors_allocated);
	}

      if (face->box != FACE_NO_BOX
	  && !face->box_color_defaulted_p
	  && face->box_color != BLACK_PIX_DEFAULT (f)
	  && face->box_color != WHITE_PIX_DEFAULT (f))
	{
	  XFreeColors (dpy, cmap, &face->box_color, 1, 0);
	  IF_DEBUG (--ncolors_allocated);
	}

      UNBLOCK_INPUT;
    }
}
#endif /* HAVE_X_WINDOWS */



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
};

/* The frame in effect when sorting font names.  Set temporarily in
   sort_fonts so that it is available in font comparison functions.  */

static struct frame *font_frame;

/* Order by which font selection chooses fonts.  The default values
   mean `first, find a best match for the font width, then for the
   font height, then for weight, then for slant.'  This variable can be
   set via set-face-font-sort-order.  */

static int font_sort_order[4];


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
     int dflt;
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


#ifdef HAVE_X_WINDOWS

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
  double font_resy = atoi (font->fields[XLFD_RESY]);
  double font_pt = atoi (font->fields[XLFD_POINT_SIZE]);
  int real_pt;

  if (font_resy == 0 || font_pt == 0)
    real_pt = 0;
  else
    real_pt = (font_resy / resy) * font_pt + 0.5;

  return real_pt;
}


/* Split XLFD font name FONT->name destructively into NUL-terminated,
   lower-case fields in FONT->fields.  NUMERIC_P non-zero means
   compute numeric values for fields XLFD_POINT_SIZE, XLFD_SWIDTH,
   XLFD_RESY, XLFD_SLANT, and XLFD_WEIGHT in FONT->numeric.  Value is
   zero if the font name doesn't have the format we expect.  The
   expected format is a font name that starts with a `-' and has
   XLFD_LAST fields separated by `-'.  (The XLFD specification allows
   forms of font names where certain field contents are enclosed in
   square brackets.  We don't support that, for now.  */

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
    }

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
   found matching PATTERN.  SCALABLE_FONTS_P non-zero means include
   scalable fonts.

   For all fonts found, set FONTS[i].name to the name of the font,
   allocated via xmalloc, and split font names into fields.  Ignore
   fonts that we can't parse.  Value is the number of fonts found.
   
   This is similar to x_list_fonts.  The differences are:

   1. It avoids consing.
   2. It never calls XLoadQueryFont.  */

static int
x_face_list_fonts (f, pattern, fonts, nfonts, try_alternatives_p,
		   scalable_fonts_p)
     struct frame *f;
     char *pattern;
     struct font_name *fonts;
     int nfonts, try_alternatives_p;
     int scalable_fonts_p;
{
  Display *dpy = f ? FRAME_X_DISPLAY (f) : x_display_list->display;
  int n, i, j;
  char **names;

  /* Get the list of fonts matching PATTERN from the X server.  */
  BLOCK_INPUT;
  names = XListFonts (dpy, pattern, nfonts, &n);
  UNBLOCK_INPUT;

  if (names)
    {
      /* Make a copy of the font names we got from X, and
	 split them into fields.  */
      for (i = j = 0; i < n; ++i)
	{
	  /* Make a copy of the font name.  */
	  fonts[j].name = xstrdup (names[i]);

	  /* Ignore fonts having a name that we can't parse.  */
	  if (!split_font_name (f, fonts + j, 1))
	    xfree (fonts[j].name);
	  else if (font_scalable_p (fonts + j))
	    {
#if SCALABLE_FONTS
	      if (!scalable_fonts_p
		  || !may_use_scalable_font_p (fonts + j, names[i]))
		xfree (fonts[j].name);
	      else
		++j;
#else /* !SCALABLE_FONTS */
	      /* Always ignore scalable fonts.  */
	      xfree (fonts[j].name);
#endif /* !SCALABLE_FONTS */
	    }
	  else
	    ++j;
	}

      n = j;

      /* Free font names.  */
      BLOCK_INPUT;
      XFreeFontNames (names);
      UNBLOCK_INPUT;
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
						fonts, nfonts, 0,
						scalable_fonts_p),
			 n == 0)))
	    patterns = XCDR (patterns);
	}
    }
  
  return n;
}
  

/* Determine the first font matching PATTERN on frame F.  Return in
   *FONT the matching font name, split into fields.  Value is non-zero
   if a match was found.  */

static int
first_font_matching (f, pattern, font)
     struct frame *f;
     char *pattern;
     struct font_name *font;
{
  int nfonts = 100;
  struct font_name *fonts;

  fonts = (struct font_name *) xmalloc (nfonts * sizeof *fonts);
  nfonts = x_face_list_fonts (f, pattern, fonts, nfonts, 1, 0);

  if (nfonts > 0)
    {
      bcopy (&fonts[0], font, sizeof *font);
      
      fonts[0].name = NULL;
      free_font_names (fonts, nfonts);
    }

  return nfonts > 0;
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
#if SCALABLE_FONTS
  nfonts = x_face_list_fonts (f, pattern, *fonts, nfonts, 1, 1);
#else
  nfonts = x_face_list_fonts (f, pattern, *fonts, nfonts, 1, 0);
#endif
  
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
   is non-null list fonts matching that pattern.  Otherwise, if
   REGISTRY_AND_ENCODING is non-null return only fonts with that
   registry and encoding, otherwise return fonts of any registry and
   encoding.  Set *FONTS to a vector of font_name structures allocated
   from the heap containing the fonts found.  Value is the number of
   fonts found.  */

static int
font_list (f, pattern, family, registry_and_encoding, fonts)
     struct frame *f;
     char *pattern;
     char *family;
     char *registry_and_encoding;
     struct font_name **fonts;
{
  if (pattern == NULL)
    {
      if (family == NULL)
	family = "*";
      
      if (registry_and_encoding == NULL)
	registry_and_encoding = "*";
      
      pattern = (char *) alloca (strlen (family)
				 + strlen (registry_and_encoding)
				 + 10);
      if (index (family, '-'))
	sprintf (pattern, "-%s-*-%s", family, registry_and_encoding);
      else
	sprintf (pattern, "-*-%s-*-%s", family, registry_and_encoding);
    }
  
  return sorted_font_list (f, pattern, cmp_font_names, fonts);
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
	XCDR (tail) = XCDR (next);
      else
	tail = XCDR (tail);
    }
}


DEFUN ("x-family-fonts", Fx_family_fonts, Sx_family_fonts, 0, 2, 0,
  "Return a list of available fonts of family FAMILY on FRAME.\n\
If FAMILY is omitted or nil, list all families.\n\
Otherwise, FAMILY must be a string, possibly containing wildcards\n\
`?' and `*'.\n\
If FRAME is omitted or nil, use the selected frame.\n\
Each element of the result is a vector [FAMILY WIDTH POINT-SIZE WEIGHT\n\
SLANT FIXED-P FULL REGISTRY-AND-ENCODING].\n\
FAMILY is the font family name.  POINT-SIZE is the size of the\n\
font in 1/10 pt.  WIDTH, WEIGHT, and SLANT are symbols describing the\n\
width, weight and slant of the font.  These symbols are the same as for\n\
face attributes.  FIXED-P is non-nil if the font is fixed-pitch.\n\
FULL is the full name of the font, and REGISTRY-AND-ENCODING is a string\n\
giving the registry and encoding of the font.\n\
The result list is sorted according to the current setting of\n\
the face font sort order.")
  (family, frame)
     Lisp_Object family, frame;
{
  struct frame *f = check_x_frame (frame);
  struct font_name *fonts;
  int i, nfonts;
  Lisp_Object result;
  struct gcpro gcpro1;
  char *family_pattern;

  if (NILP (family))
    family_pattern = "*";
  else
    {
      CHECK_STRING (family, 1);
      family_pattern = LSTRDUPA (family);
    }
  
  result = Qnil;
  GCPRO1 (result);
  nfonts = font_list (f, NULL, family_pattern, NULL, &fonts);
  for (i = nfonts - 1; i >= 0; --i)
    {
      Lisp_Object v = Fmake_vector (make_number (8), Qnil);
      char *tem;

#define ASET(VECTOR, IDX, VAL) (XVECTOR (VECTOR)->contents[IDX] = (VAL))
      
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
      
#undef ASET
    }

  remove_duplicates (result);
  free_font_names (fonts, nfonts);
  UNGCPRO;
  return result;
}


DEFUN ("x-font-family-list", Fx_font_family_list, Sx_font_family_list,
       0, 1, 0,
  "Return a list of available font families on FRAME.\n\
If FRAME is omitted or nil, use the selected frame.\n\
Value is a list of conses (FAMILY . FIXED-P) where FAMILY\n\
is a font family, and FIXED-P is non-nil if fonts of that family\n\
are fixed-pitch.")
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
      nfonts = font_list (f, NULL, "*", NULL, &fonts);
      
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
  "Return a list of the names of available fonts matching PATTERN.\n\
If optional arguments FACE and FRAME are specified, return only fonts\n\
the same size as FACE on FRAME.\n\
PATTERN is a string, perhaps with wildcard characters;\n\
  the * character matches any substring, and\n\
  the ? character matches any single character.\n\
  PATTERN is case-insensitive.\n\
FACE is a face name--a symbol.\n\
\n\
The return value is a list of strings, suitable as arguments to\n\
set-face-font.\n\
\n\
Fonts Emacs can't use may or may not be excluded\n\
even if they match PATTERN and FACE.\n\
The optional fourth argument MAXIMUM sets a limit on how many\n\
fonts to match.  The first MAXIMUM fonts are reported.\n\
The optional fifth argument WIDTH, if specified, is a number of columns\n\
occupied by a character of a font.  In that case, return only fonts\n\
the WIDTH times as wide as FACE on FRAME.")
  (pattern, face, frame, maximum, width)
    Lisp_Object pattern, face, frame, maximum, width;
{
  struct frame *f;
  int size;
  int maxnames;

  check_x ();
  CHECK_STRING (pattern, 0);
  
  if (NILP (maximum))
    maxnames = 2000;
  else
    {
      CHECK_NATNUM (maximum, 0);
      maxnames = XINT (maximum);
    }

  if (!NILP (width))
    CHECK_NUMBER (width, 4);

  /* We can't simply call check_x_frame because this function may be
     called before any frame is created.  */
  f = frame_or_selected_frame (frame, 2);
  if (!FRAME_X_P (f))
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
      int face_id = lookup_named_face (f, face, CHARSET_ASCII);
      struct face *face = FACE_FROM_ID (f, face_id);

      if (face->font)
	size = face->font->max_bounds.width;
      else
	size = FRAME_FONT (f)->max_bounds.width;

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

#endif /* HAVE_X_WINDOWS */



/***********************************************************************
			      Lisp Faces
 ***********************************************************************/

/* Access face attributes of face FACE, a Lisp vector.  */

#define LFACE_FAMILY(LFACE) \
     XVECTOR (LFACE)->contents[LFACE_FAMILY_INDEX]
#define LFACE_HEIGHT(LFACE) \
     XVECTOR (LFACE)->contents[LFACE_HEIGHT_INDEX]
#define LFACE_WEIGHT(LFACE) \
     XVECTOR (LFACE)->contents[LFACE_WEIGHT_INDEX]
#define LFACE_SLANT(LFACE) \
     XVECTOR (LFACE)->contents[LFACE_SLANT_INDEX]
#define LFACE_UNDERLINE(LFACE) \
     XVECTOR (LFACE)->contents[LFACE_UNDERLINE_INDEX]
#define LFACE_INVERSE(LFACE) \
     XVECTOR (LFACE)->contents[LFACE_INVERSE_INDEX]
#define LFACE_FOREGROUND(LFACE) \
     XVECTOR (LFACE)->contents[LFACE_FOREGROUND_INDEX]
#define LFACE_BACKGROUND(LFACE) \
     XVECTOR (LFACE)->contents[LFACE_BACKGROUND_INDEX]
#define LFACE_STIPPLE(LFACE) \
     XVECTOR (LFACE)->contents[LFACE_STIPPLE_INDEX]
#define LFACE_SWIDTH(LFACE) \
     XVECTOR (LFACE)->contents[LFACE_SWIDTH_INDEX]
#define LFACE_OVERLINE(LFACE) \
     XVECTOR (LFACE)->contents[LFACE_OVERLINE_INDEX]
#define LFACE_STRIKE_THROUGH(LFACE) \
     XVECTOR (LFACE)->contents[LFACE_STRIKE_THROUGH_INDEX]
#define LFACE_BOX(LFACE) \
     XVECTOR (LFACE)->contents[LFACE_BOX_INDEX]

/* Non-zero if LFACE is a Lisp face.  A Lisp face is a vector of size
   LFACE_VECTOR_SIZE which has the symbol `face' in slot 0.  */

#define LFACEP(LFACE)					\
     (VECTORP (LFACE)					\
      && XVECTOR (LFACE)->size == LFACE_VECTOR_SIZE	\
      && EQ (XVECTOR (LFACE)->contents[0], Qface))

     
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
  xassert (UNSPECIFIEDP (attrs[LFACE_HEIGHT_INDEX])
	   || INTEGERP (attrs[LFACE_HEIGHT_INDEX]));
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
#ifdef HAVE_WINDOW_SYSTEM
  xassert (UNSPECIFIEDP (attrs[LFACE_STIPPLE_INDEX])
	   || SYMBOLP (attrs[LFACE_STIPPLE_INDEX])
	   || !NILP (Fbitmap_spec_p (attrs[LFACE_STIPPLE_INDEX])));
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


/* Resolve face name FACE_NAME.  If FACE_NAME Is a string, intern it
   to make it a symvol.  If FACE_NAME is an alias for another face,
   return that face's name.  */

static Lisp_Object
resolve_face_name (face_name)
     Lisp_Object face_name;
{
  Lisp_Object aliased;
  
  if (STRINGP (face_name))
    face_name = intern (XSTRING (face_name)->data);

  for (;;)
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
   return the global definition.  FACE_NAME may be a string or a
   symbol (apparently Emacs 20.2 allows strings as face names in face
   text properties; ediff uses that).  If FACE_NAME is an alias for
   another face, return that face's definition.  If SIGNAL_P is
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
    if (UNSPECIFIEDP (attrs[i]))
      break;

  return i == LFACE_VECTOR_SIZE;
}


#ifdef HAVE_X_WINDOWS

/* Set font-related attributes of Lisp face LFACE from XLFD font name
   FONT_NAME.  If FORCE_P is zero, set only unspecified attributes of
   LFACE.  MAY_FAIL_P non-zero means return 0 if FONT_NAME isn't a
   valid font name; otherwise this function tries to use a reasonable
   default font.

   Ignore fields of FONT_NAME containing wildcards.  Value is zero if
   not successful because FONT_NAME was not in a valid format and
   MAY_FAIL_P was non-zero.  A valid format is one that is suitable
   for split_font_name, see the comment there.  */
   
static int
set_lface_from_font_name (f, lface, font_name, force_p, may_fail_p)
     struct frame *f;
     Lisp_Object lface;
     char *font_name;
     int force_p, may_fail_p;
{
  struct font_name font;
  char *buffer;
  int pt;
  int free_font_name_p = 0;
  int have_font_p = 0;

  /* If FONT_NAME contains wildcards, use the first matching font.  */
  if (index (font_name, '*') || index (font_name, '?'))
    {
      if (first_font_matching (f, font_name, &font))
	free_font_name_p = have_font_p = 1;
    }
  else
    {
      font.name = STRDUPA (font_name);
      if (split_font_name (f, &font, 1))
	have_font_p = 1;
      else
	{
	  /* The font name may be something like `6x13'.  Make
	     sure we use the full name.  */
	  struct font_info *font_info;

	  BLOCK_INPUT;
	  font_info = fs_load_font (f, FRAME_X_FONT_TABLE (f),
				    CHARSET_ASCII, font_name, -1);
	  if (font_info)
	    {
	      font.name = STRDUPA (font_info->full_name);
	      split_font_name (f, &font, 1);
	      have_font_p = 1;
	    }
	  UNBLOCK_INPUT;
	}
    }

  /* If FONT_NAME is completely bogus try to use something reasonable
     if this function must succeed.  Otherwise, give up.  */
  if (!have_font_p)
    {
      if (may_fail_p)
	return 0;
      else if (first_font_matching (f, "-adobe-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-1",
				    &font)
	       || first_font_matching (f, "-misc-fixed-medium-r-normal-*-*-140-*-*-c-*-iso8859-1",
				       &font)
	       || first_font_matching (f, "-*-*-medium-r-normal-*-*-140-*-*-c-*-iso8859-1",
				       &font)
	       || first_font_matching (f, "-*-*-medium-r-*-*-*-*-*-*-c-*-iso8859-1",
				       &font)
	       || first_font_matching (f, "-*-fixed-*-*-*-*-*-140-*-*-c-*-iso8859-1",
				       &font)
	       || first_font_matching (f, "fixed", &font))
	free_font_name_p = 1;
      else
	abort ();
    }
      

  /* Set attributes only if unspecified, otherwise face defaults for
     new frames would never take effect.  */
  
  if (force_p || UNSPECIFIEDP (LFACE_FAMILY (lface)))
    {
      buffer = (char *) alloca (strlen (font.fields[XLFD_FAMILY])
				+ strlen (font.fields[XLFD_FOUNDRY])
				+ 2);
      sprintf (buffer, "%s-%s", font.fields[XLFD_FOUNDRY],
	       font.fields[XLFD_FAMILY]);
      LFACE_FAMILY (lface) = build_string (buffer);
    }

  if (force_p || UNSPECIFIEDP (LFACE_HEIGHT (lface)))
    {
      pt = xlfd_point_size (f, &font);
      xassert (pt > 0);
      LFACE_HEIGHT (lface) = make_number (pt);
    }

  if (force_p || UNSPECIFIEDP (LFACE_SWIDTH (lface)))
    LFACE_SWIDTH (lface) = xlfd_symbolic_swidth (&font);

  if (force_p || UNSPECIFIEDP (LFACE_WEIGHT (lface)))
    LFACE_WEIGHT (lface) = xlfd_symbolic_weight (&font);

  if (force_p || UNSPECIFIEDP (LFACE_SLANT (lface)))
    LFACE_SLANT (lface) = xlfd_symbolic_slant (&font);

  if (free_font_name_p)
    xfree (font.name);
  
  return 1;
}

#endif /* HAVE_X_WINDOWS */


/* Merge two Lisp face attribute vectors FROM and TO and store the
   resulting attributes in TO.  Every non-nil attribute of FROM
   overrides the corresponding attribute of TO.  */

static INLINE void
merge_face_vectors (from, to)
     Lisp_Object *from, *to;
{
  int i;
  for (i = 1; i < LFACE_VECTOR_SIZE; ++i)
    if (!UNSPECIFIEDP (from[i]))
      to[i] = from[i];
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
	       && *XSYMBOL (first)->name->data == ':')
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
		    add_to_log ("Illegal face font family", value, Qnil);
		}
	      else if (EQ (keyword, QCheight))
		{
		  if (INTEGERP (value))
		    to[LFACE_HEIGHT_INDEX] = value;
		  else
		    add_to_log ("Illegal face font height", value, Qnil);
		}
	      else if (EQ (keyword, QCweight))
		{
		  if (SYMBOLP (value)
		      && face_numeric_weight (value) >= 0)
		    to[LFACE_WEIGHT_INDEX] = value;
		  else
		    add_to_log ("Illegal face weight", value, Qnil);
		}
	      else if (EQ (keyword, QCslant))
		{
		  if (SYMBOLP (value)
		      && face_numeric_slant (value) >= 0)
		    to[LFACE_SLANT_INDEX] = value;
		  else
		    add_to_log ("Illegal face slant", value, Qnil);
		}
	      else if (EQ (keyword, QCunderline))
		{
		  if (EQ (value, Qt)
		      || NILP (value)
		      || STRINGP (value))
		    to[LFACE_UNDERLINE_INDEX] = value;
		  else
		    add_to_log ("Illegal face underline", value, Qnil);
		}
	      else if (EQ (keyword, QCoverline))
		{
		  if (EQ (value, Qt)
		      || NILP (value)
		      || STRINGP (value))
		    to[LFACE_OVERLINE_INDEX] = value;
		  else
		    add_to_log ("Illegal face overline", value, Qnil);
		}
	      else if (EQ (keyword, QCstrike_through))
		{
		  if (EQ (value, Qt)
		      || NILP (value)
		      || STRINGP (value))
		    to[LFACE_STRIKE_THROUGH_INDEX] = value;
		  else
		    add_to_log ("Illegal face strike-through", value, Qnil);
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
		    add_to_log ("Illegal face box", value, Qnil);
		}
	      else if (EQ (keyword, QCinverse_video)
		       || EQ (keyword, QCreverse_video))
		{
		  if (EQ (value, Qt) || NILP (value))
		    to[LFACE_INVERSE_INDEX] = value;
		  else
		    add_to_log ("Illegal face inverse-video", value, Qnil);
		}
	      else if (EQ (keyword, QCforeground))
		{
		  if (STRINGP (value))
		    to[LFACE_FOREGROUND_INDEX] = value;
		  else
		    add_to_log ("Illegal face foreground", value, Qnil);
		}
	      else if (EQ (keyword, QCbackground))
		{
		  if (STRINGP (value))
		    to[LFACE_BACKGROUND_INDEX] = value;
		  else
		    add_to_log ("Illegal face background", value, Qnil);
		}
	      else if (EQ (keyword, QCstipple))
		{
#ifdef HAVE_X_WINDOWS
		  Lisp_Object pixmap_p = Fbitmap_spec_p (value);
		  if (!NILP (pixmap_p))
		    to[LFACE_STIPPLE_INDEX] = value;
		  else
		    add_to_log ("Illegal face stipple", value, Qnil);
#endif
		}
	      else if (EQ (keyword, QCwidth))
		{
		  if (SYMBOLP (value)
		      && face_numeric_swidth (value) >= 0)
		    to[LFACE_SWIDTH_INDEX] = value;
		  else
		    add_to_log ("Illegal face width", value, Qnil);
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
	merge_face_vectors (XVECTOR (lface)->contents, to);
    }
}


DEFUN ("internal-make-lisp-face", Finternal_make_lisp_face,
       Sinternal_make_lisp_face, 1, 2, 0,
  "Make FACE, a symbol, a Lisp face with all attributes nil.\n\
If FACE was not known as a face before, create a new one.\n\
If optional argument FRAME is specified, make a frame-local face\n\
for that frame.  Otherwise operate on the global face definition.\n\
Value is a vector of face attributes.")
  (face, frame)
     Lisp_Object face, frame;
{
  Lisp_Object global_lface, lface;
  struct frame *f;
  int i;

  CHECK_SYMBOL (face, 0);
  global_lface = lface_from_face_name (NULL, face, 0);
  
  if (!NILP (frame))
    {
      CHECK_LIVE_FRAME (frame, 1);
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
      XVECTOR (global_lface)->contents[0] = Qface;
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
      XVECTOR (global_lface)->contents[i] = Qunspecified;
    
  /* Add a frame-local definition.  */
  if (f)
    {
      if (NILP (lface))
	{
	  lface = Fmake_vector (make_number (LFACE_VECTOR_SIZE),
				Qunspecified);
	  XVECTOR (lface)->contents[0] = Qface;
	  f->face_alist = Fcons (Fcons (face, lface), f->face_alist);
	}
      else
	for (i = 1; i < LFACE_VECTOR_SIZE; ++i)
	  XVECTOR (lface)->contents[i] = Qunspecified;
    }
  else
    lface = global_lface;

  xassert (LFACEP (lface));
  check_lface (lface);
  return lface;
}


DEFUN ("internal-lisp-face-p", Finternal_lisp_face_p,
       Sinternal_lisp_face_p, 1, 2, 0,
  "Return non-nil if FACE names a face.\n\
If optional second parameter FRAME is non-nil, check for the\n\
existence of a frame-local face with name FACE on that frame.\n\
Otherwise check for the existence of a global face.")
  (face, frame)
     Lisp_Object face, frame;
{
  Lisp_Object lface;
  
  if (!NILP (frame))
    {
      CHECK_LIVE_FRAME (frame, 1);
      lface = lface_from_face_name (XFRAME (frame), face, 0);
    }
  else
    lface = lface_from_face_name (NULL, face, 0);

  return lface;
}


DEFUN ("internal-copy-lisp-face", Finternal_copy_lisp_face,
       Sinternal_copy_lisp_face, 4, 4, 0,
  "Copy face FROM to TO.\n\
If FRAME it t, copy the global face definition of FROM to the\n\
global face definition of TO.  Otherwise, copy the frame-local\n\
definition of FROM on FRAME to the frame-local definition of TO\n\
on NEW-FRAME, or FRAME if NEW-FRAME is nil.\n\
\n\
Value is TO.")
  (from, to, frame, new_frame)
     Lisp_Object from, to, frame, new_frame;
{
  Lisp_Object lface, copy;
  
  CHECK_SYMBOL (from, 0);
  CHECK_SYMBOL (to, 1);
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
      CHECK_LIVE_FRAME (frame, 2);
      CHECK_LIVE_FRAME (new_frame, 3);
      lface = lface_from_face_name (XFRAME (frame), from, 1);
      copy = Finternal_make_lisp_face (to, new_frame);
    }
  
  bcopy (XVECTOR (lface)->contents, XVECTOR (copy)->contents,
	 LFACE_VECTOR_SIZE * sizeof (Lisp_Object));
  
  return to;
}


DEFUN ("internal-set-lisp-face-attribute", Finternal_set_lisp_face_attribute,
       Sinternal_set_lisp_face_attribute, 3, 4, 0,
  "Set attribute ATTR of FACE to VALUE.\n\
If optional argument FRAME is given, set the face attribute of face FACE\n\
on that frame.  If FRAME is t, set the attribute of the default for face\n\
FACE (for new frames).  If FRAME is omitted or nil, use the selected\n\
frame.")
  (face, attr, value, frame)
     Lisp_Object face, attr, value, frame;
{
  Lisp_Object lface;
  Lisp_Object old_value = Qnil;
  int font_related_attr_p = 0;
  
  CHECK_SYMBOL (face, 0);
  CHECK_SYMBOL (attr, 1);

  face = resolve_face_name (face);

  /* Set lface to the Lisp attribute vector of FACE.  */
  if (EQ (frame, Qt))
    lface = lface_from_face_name (NULL, face, 1);
  else
    {
      if (NILP (frame))
	frame = selected_frame;
      
      CHECK_LIVE_FRAME (frame, 3);
      lface = lface_from_face_name (XFRAME (frame), face, 0);
      
      /* If a frame-local face doesn't exist yet, create one.  */
      if (NILP (lface))
	lface = Finternal_make_lisp_face (face, frame);
    }

  if (EQ (attr, QCfamily))
    {
      if (!UNSPECIFIEDP (value))
	{
	  CHECK_STRING (value, 3);
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
	  CHECK_NUMBER (value, 3);
	  if (XINT (value) <= 0)
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
	  CHECK_SYMBOL (value, 3);
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
	  CHECK_SYMBOL (value, 3);
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
	valid_p = XINT (value) > 0;
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
		  if (!INTEGERP (v) || XINT (v) <= 0)
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
	  CHECK_SYMBOL (value, 3);
	  if (!EQ (value, Qt) && !NILP (value))
	    signal_error ("Invalid inverse-video face attribute value", value);
	}
      old_value = LFACE_INVERSE (lface);
      LFACE_INVERSE (lface) = value;
    }
  else if (EQ (attr, QCforeground))
    {
      if (!UNSPECIFIEDP (value)
	  && !EQ (value, Qunspecified_fg) && !EQ (value, Qunspecified_bg))
	{
	  /* Don't check for valid color names here because it depends
	     on the frame (display) whether the color will be valid
	     when the face is realized.  */
	  CHECK_STRING (value, 3);
	  if (XSTRING (value)->size == 0)
	    signal_error ("Empty foreground color value", value);
	}
      old_value = LFACE_FOREGROUND (lface);
      LFACE_FOREGROUND (lface) = value;
    }
  else if (EQ (attr, QCbackground))
    {
      if (!UNSPECIFIEDP (value)
	  && !EQ (value, Qunspecified_bg) && !EQ (value, Qunspecified_fg))
	{
	  /* Don't check for valid color names here because it depends
	     on the frame (display) whether the color will be valid
	     when the face is realized.  */
	  CHECK_STRING (value, 3);
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
	  CHECK_SYMBOL (value, 3);
	  if (face_numeric_swidth (value) < 0)
	    signal_error ("Invalid face width", value);
	}
      old_value = LFACE_SWIDTH (lface);
      LFACE_SWIDTH (lface) = value;
      font_related_attr_p = 1;
    }
  else if (EQ (attr, QCfont))
    {
#ifdef HAVE_X_WINDOWS
      /* Set font-related attributes of the Lisp face from an
	 XLFD font name.  */
      struct frame *f;

      CHECK_STRING (value, 3);
      if (EQ (frame, Qt))
	f = SELECTED_FRAME ();
      else
	f = check_x_frame (frame);
      
      if (!set_lface_from_font_name (f, lface, XSTRING (value)->data, 1, 1))
	signal_error ("Invalid font name", value);
      
      font_related_attr_p = 1;
#endif /* HAVE_X_WINDOWS */
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

#ifdef HAVE_X_WINDOWS

  if (!EQ (frame, Qt)
      && !UNSPECIFIEDP (value)
      && NILP (Fequal (old_value, value)))
    {
      Lisp_Object param;

      param = Qnil;
      
      if (EQ (face, Qdefault))
	{
	  /* Changed font-related attributes of the `default' face are
	     reflected in changed `font' frame parameters. */
	  if (font_related_attr_p
	      && lface_fully_specified_p (XVECTOR (lface)->contents))
	    set_font_frame_param (frame, lface);
	  else if (EQ (attr, QCforeground))
	    param = Qforeground_color;
	  else if (EQ (attr, QCbackground))
	    param = Qbackground_color;
	}
      else if (EQ (face, Qscroll_bar))
	{
	  /* Changing the colors of `scroll-bar' sets frame parameters
	     `scroll-bar-foreground' and `scroll-bar-background'. */
	  if (EQ (attr, QCforeground))
	    param = Qscroll_bar_foreground;
	  else if (EQ (attr, QCbackground))
	    param = Qscroll_bar_background;
	}
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

      if (SYMBOLP (param))
	Fmodify_frame_parameters (frame, Fcons (Fcons (param, value), Qnil));
    }

#endif /* HAVE_X_WINDOWS */
  
  return face;
}


#ifdef HAVE_X_WINDOWS

/* Set the `font' frame parameter of FRAME according to `default' face
   attributes LFACE.  */

static void
set_font_frame_param (frame, lface)
     Lisp_Object frame, lface;
{
  struct frame *f = XFRAME (frame);
  Lisp_Object frame_font;
  int fontset;
  char *font;

  /* Get FRAME's font parameter.  */
  frame_font = Fassq (Qfont, f->param_alist);
  xassert (CONSP (frame_font) && STRINGP (XCDR (frame_font)));
  frame_font = XCDR (frame_font);

  fontset = fs_query_fontset (f, XSTRING (frame_font)->data);
  if (fontset >= 0)
    {
      /* Frame parameter is a fontset name.  Modify the fontset so
         that all its fonts reflect face attributes LFACE.  */
      int charset;
      struct fontset_info *fontset_info;

      fontset_info = FRAME_FONTSET_DATA (f)->fontset_table[fontset];
      
      for (charset = 0; charset < MAX_CHARSET; ++charset)
	if (fontset_info->fontname[charset])
	  {
	    font = choose_face_fontset_font (f, XVECTOR (lface)->contents,
					     fontset, charset);
	    Fset_fontset_font (frame_font, CHARSET_SYMBOL (charset),
			       build_string (font), frame);
	    xfree (font);
	  }
    }
  else
    {
      /* Frame parameter is an X font name.  I believe this can 
         only happen in unibyte mode.  */
      font = choose_face_font (f, XVECTOR (lface)->contents,
			       -1, Vface_default_registry);
      if (font)
	{
	  store_frame_param (f, Qfont, build_string (font));
	  xfree (font);
	}
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
       Sinternal_face_x_get_resource, 3, 3, 0, "")
  (resource, class, frame)
     Lisp_Object resource, class, frame;
{
  Lisp_Object value;
  CHECK_STRING (resource, 0);
  CHECK_STRING (class, 1);
  CHECK_LIVE_FRAME (frame, 2);
  BLOCK_INPUT;
  value = display_x_get_resource (FRAME_X_DISPLAY_INFO (XFRAME (frame)),
				  resource, class, Qnil, Qnil);
  UNBLOCK_INPUT;
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
       3, 4, 0, "")
  (face, attr, value, frame)
     Lisp_Object face, attr, value, frame;
{
  CHECK_SYMBOL (face, 0);
  CHECK_SYMBOL (attr, 1);
  CHECK_STRING (value, 2);

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
	   || EQ (attr, QCstrike_through)
	   || EQ (attr, QCbox))
    {
      Lisp_Object boolean_value;

      /* If the result of face_boolean_x_resource_value is t or nil,
	 VALUE does NOT specify a color. */
      boolean_value = face_boolean_x_resource_value (value, 0);
      if (SYMBOLP (boolean_value))
	value = boolean_value;
    }

  return Finternal_set_lisp_face_attribute (face, attr, value, frame);
}



/***********************************************************************
			      Menu face
 ***********************************************************************/

#ifdef USE_X_TOOLKIT

/* Structure used to pass X resources to functions called via
   XtApplyToWidgets.  */

struct x_resources
{
  Arg *av;
  int ac;
};


#ifdef USE_MOTIF

static void xm_apply_resources P_ ((Widget, XtPointer));
static void xm_set_menu_resources_from_menu_face P_ ((struct frame *, Widget));


/* Set widget W's X resources from P which points to an x_resources
   structure.  If W is a cascade button, apply resources to W's
   submenu.  */

static void
xm_apply_resources (w, p)
     Widget w;
     XtPointer p;
{
  Widget submenu = 0;
  struct x_resources *res = (struct x_resources *) p;
  
  XtSetValues (w, res->av, res->ac);
  XtVaGetValues (w, XmNsubMenuId, &submenu, NULL);
  if (submenu)
    {
      XtSetValues (submenu, res->av, res->ac);
      XtApplyToWidgets (submenu, xm_apply_resources, p);
    }
}


/* Set X resources of menu-widget WIDGET on frame F from face `menu'.
   This is the LessTif/Motif version.  As of LessTif 0.88 it has the
   following problems:

   1. Setting the XmNfontList resource leads to an infinite loop
   somewhere in LessTif.  */

static void
xm_set_menu_resources_from_menu_face (f, widget)
     struct frame *f;
     Widget widget;
{
  struct face *face;
  Lisp_Object lface;
  Arg av[3];
  int ac = 0;
  XmFontList fl = 0;

  lface = lface_from_face_name (f, Qmenu, 1);
  face = FACE_FROM_ID (f, MENU_FACE_ID);

  if (!UNSPECIFIEDP (LFACE_FOREGROUND (lface)))
    {
      XtSetArg (av[ac], XmNforeground, face->foreground);
      ++ac;
    }

  if (!UNSPECIFIEDP (LFACE_BACKGROUND (lface)))
    {
      XtSetArg (av[ac], XmNbackground, face->background);
      ++ac;
    }

  /* If any font-related attribute of `menu' is set, set the font.  */
  if (face->font
      && (!UNSPECIFIEDP (LFACE_FAMILY (lface))
	  || !UNSPECIFIEDP (LFACE_SWIDTH (lface))
	  || !UNSPECIFIEDP (LFACE_WEIGHT (lface))
	  || !UNSPECIFIEDP (LFACE_SLANT (lface))
	  || !UNSPECIFIEDP (LFACE_HEIGHT (lface))))
    {
#if 0 /* Setting the font leads to an infinite loop somewhere
	 in LessTif during geometry computation.  */
      XmFontListEntry fe;
      fe = XmFontListEntryCreate ("menu_font", XmFONT_IS_FONT, face->font);
      fl = XmFontListAppendEntry (NULL, fe);
      XtSetArg (av[ac], XmNfontList, fl);
      ++ac;
#endif
    }

  xassert (ac <= sizeof av / sizeof *av);
  
  if (ac)
    {
      struct x_resources res;
      
      XtSetValues (widget, av, ac);
      res.av = av, res.ac = ac;
      XtApplyToWidgets (widget, xm_apply_resources, &res);
      if (fl)
	XmFontListFree (fl);
    }
}


#endif /* USE_MOTIF */

#ifdef USE_LUCID

static void xl_apply_resources P_ ((Widget, XtPointer));
static void xl_set_menu_resources_from_menu_face P_ ((struct frame *, Widget));


/* Set widget W's resources from P which points to an x_resources
   structure.  */

static void
xl_apply_resources (widget, p)
     Widget widget;
     XtPointer p;
{
  struct x_resources *res = (struct x_resources *) p;
  XtSetValues (widget, res->av, res->ac);
}


/* On frame F, set X resources of menu-widget WIDGET from face `menu'.
   This is the Lucid version.  */

static void
xl_set_menu_resources_from_menu_face (f, widget)
     struct frame *f;
     Widget widget;
{
  struct face *face;
  Lisp_Object lface;
  Arg av[3];
  int ac = 0;

  lface = lface_from_face_name (f, Qmenu, 1);
  face = FACE_FROM_ID (f, MENU_FACE_ID);

  if (!UNSPECIFIEDP (LFACE_FOREGROUND (lface)))
    {
      XtSetArg (av[ac], XtNforeground, face->foreground);
      ++ac;
    }

  if (!UNSPECIFIEDP (LFACE_BACKGROUND (lface)))
    {
      XtSetArg (av[ac], XtNbackground, face->background);
      ++ac;
    }

  if (face->font
      && (!UNSPECIFIEDP (LFACE_FAMILY (lface))
	  || !UNSPECIFIEDP (LFACE_SWIDTH (lface))
	  || !UNSPECIFIEDP (LFACE_WEIGHT (lface))
	  || !UNSPECIFIEDP (LFACE_SLANT (lface))
	  || !UNSPECIFIEDP (LFACE_HEIGHT (lface))))
    {
      XtSetArg (av[ac], XtNfont, face->font);
      ++ac;
    }

  if (ac)
    {
      struct x_resources res;
      
      XtSetValues (widget, av, ac);

      /* We must do children here in case we're handling a pop-up menu
	 in which case WIDGET is a popup shell.  XtApplyToWidgets
	 is a function from lwlib.  */
      res.av = av, res.ac = ac;
      XtApplyToWidgets (widget, xl_apply_resources, &res);
    }
}

#endif /* USE_LUCID */


/* On frame F, set X resources of menu-widget WIDGET from face `menu'.  */

void
x_set_menu_resources_from_menu_face (f, widget)
     struct frame *f;
     Widget widget;
{
  /* Realized faces may have been removed on frame F, e.g. because of
     face attribute changes.  Recompute them, if necessary, since we
     will need the `menu' face.  */
  if (f->face_cache->used == 0)
    recompute_basic_faces (f);
  
#ifdef USE_LUCID
  xl_set_menu_resources_from_menu_face (f, widget);
#endif
#ifdef USE_MOTIF
  xm_set_menu_resources_from_menu_face (f, widget);
#endif
}

#endif /* USE_X_TOOLKIT */

#endif /* HAVE_X_WINDOWS */



DEFUN ("internal-get-lisp-face-attribute", Finternal_get_lisp_face_attribute,
       Sinternal_get_lisp_face_attribute,
       2, 3, 0,
  "Return face attribute KEYWORD of face SYMBOL.\n\
If SYMBOL does not name a valid Lisp face or KEYWORD isn't a valid\n\
face attribute name, signal an error.\n\
If the optional argument FRAME is given, report on face FACE in that\n\
frame.  If FRAME is t, report on the defaults for face FACE (for new\n\
frames).  If FRAME is omitted or nil, use the selected frame.")
  (symbol, keyword, frame)
     Lisp_Object symbol, keyword, frame;
{
  Lisp_Object lface, value = Qnil;
  
  CHECK_SYMBOL (symbol, 0);
  CHECK_SYMBOL (keyword, 1);

  if (EQ (frame, Qt))
    lface = lface_from_face_name (NULL, symbol, 1);
  else
    {
      if (NILP (frame))
	frame = selected_frame;
      CHECK_LIVE_FRAME (frame, 2);
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
  else
    signal_error ("Invalid face attribute name", keyword);

  return value;
}


DEFUN ("internal-lisp-face-attribute-values",
       Finternal_lisp_face_attribute_values,
       Sinternal_lisp_face_attribute_values, 1, 1, 0,
  "Return a list of valid discrete values for face attribute ATTR.\n\
Value is nil if ATTR doesn't have a discrete set of valid values.")
  (attr)
     Lisp_Object attr;
{
  Lisp_Object result = Qnil;
  
  CHECK_SYMBOL (attr, 0);
  
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
  "Add attributes from frame-default definition of FACE to FACE on FRAME.")
  (face, frame)
     Lisp_Object face, frame;
{
  Lisp_Object global_lface, local_lface;
  CHECK_LIVE_FRAME (frame, 1);
  global_lface = lface_from_face_name (NULL, face, 1);
  local_lface = lface_from_face_name (XFRAME (frame), face, 0);
  if (NILP (local_lface))
    local_lface = Finternal_make_lisp_face (face, frame);
  merge_face_vectors (XVECTOR (global_lface)->contents,
		      XVECTOR (local_lface)->contents);
  return face;
}


/* The following function is implemented for compatibility with 20.2.
   The function is used in x-resolve-fonts when it is asked to
   return fonts with the same size as the font of a face.  This is
   done in fontset.el.  */

DEFUN ("face-font", Fface_font, Sface_font, 1, 2, 0, 
  "Return the font name of face FACE, or nil if it is unspecified.\n\
If the optional argument FRAME is given, report on face FACE in that frame.\n\
If FRAME is t, report on the defaults for face FACE (for new frames).\n\
  The font default for a face is either nil, or a list\n\
  of the form (bold), (italic) or (bold italic).\n\
If FRAME is omitted or nil, use the selected frame.")
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
      
      if (!NILP (LFACE_SLANT (lface))
	  && !EQ (LFACE_SLANT (lface), Qnormal))
	result = Fcons (Qitalic, result);
      
      return result;
    }
  else
    {
      struct frame *f = frame_or_selected_frame (frame, 1);
      int face_id = lookup_named_face (f, face, CHARSET_ASCII);
      struct face *face = FACE_FROM_ID (f, face_id);
      return build_string (face->font_name);
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
	      equal_p = (XSTRING (a)->size == XSTRING (b)->size
			 && bcmp (XSTRING (a)->data, XSTRING (b)->data,
				  XSTRING (a)->size) == 0);
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
  "True if FACE1 and FACE2 are equal.\n\
If the optional argument FRAME is given, report on face FACE in that frame.\n\
If FRAME is t, report on the defaults for face FACE (for new frames).\n\
If FRAME is omitted or nil, use the selected frame.")
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
  "True if FACE has no attribute specified.\n\
If the optional argument FRAME is given, report on face FACE in that frame.\n\
If FRAME is t, report on the defaults for face FACE (for new frames).\n\
If FRAME is omitted or nil, use the selected frame.")
  (face, frame)
     Lisp_Object face, frame;
{
  struct frame *f;
  Lisp_Object lface;
  int i;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame, 0);
  f = XFRAME (frame);
  
  if (EQ (frame, Qt))
    lface = lface_from_face_name (NULL, face, 1);
  else
    lface = lface_from_face_name (f, face, 1);

  for (i = 1; i < LFACE_VECTOR_SIZE; ++i)
    if (!UNSPECIFIEDP (XVECTOR (lface)->contents[i]))
      break;
  
  return i == LFACE_VECTOR_SIZE ? Qt : Qnil;
}


DEFUN ("frame-face-alist", Fframe_face_alist, Sframe_face_alist,
       0, 1, 0, 
  "Return an alist of frame-local faces defined on FRAME.\n\
For internal use only.")
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
	  ^ (unsigned) v[LFACE_WEIGHT_INDEX]
	  ^ (unsigned) v[LFACE_SLANT_INDEX]
	  ^ (unsigned) v[LFACE_SWIDTH_INDEX]
	  ^ XFASTINT (v[LFACE_HEIGHT_INDEX]));
}


/* Return non-zero if LFACE1 and LFACE2 specify the same font (without
   considering charsets/registries).  They do if they specify the same
   family, point size, weight, width and slant.  Both LFACE1 and
   LFACE2 must be fully-specified.  */

static INLINE int
lface_same_font_attributes_p (lface1, lface2)
     Lisp_Object *lface1, *lface2;
{
  xassert (lface_fully_specified_p (lface1)
	   && lface_fully_specified_p (lface2));
  return (xstricmp (XSTRING (lface1[LFACE_FAMILY_INDEX])->data,
		    XSTRING (lface2[LFACE_FAMILY_INDEX])->data) == 0
	  && (XFASTINT (lface1[LFACE_HEIGHT_INDEX])
	      == XFASTINT (lface2[LFACE_HEIGHT_INDEX]))
	  && EQ (lface1[LFACE_SWIDTH_INDEX], lface2[LFACE_SWIDTH_INDEX])
	  && EQ (lface1[LFACE_WEIGHT_INDEX], lface2[LFACE_WEIGHT_INDEX])
	  && EQ (lface1[LFACE_SLANT_INDEX], lface2[LFACE_SLANT_INDEX]));
}



/***********************************************************************
			    Realized Faces
 ***********************************************************************/

/* Allocate and return a new realized face for Lisp face attribute
   vector ATTR, charset CHARSET, and registry REGISTRY.  */

static struct face *
make_realized_face (attr, charset, registry)
     Lisp_Object *attr;
     int charset;
     Lisp_Object registry;
{
  struct face *face = (struct face *) xmalloc (sizeof *face);
  bzero (face, sizeof *face);
  face->charset = charset;
  face->registry = registry;
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
#ifdef HAVE_X_WINDOWS
      if (FRAME_X_P (f))
	{
	  if (face->gc)
	    {
	      x_free_gc (f, face->gc);
	      face->gc = 0;
	    }
	  
	  free_face_colors (f, face);
	  x_destroy_bitmap (f, face->stipple);
	}
#endif /* HAVE_X_WINDOWS */

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
#ifdef HAVE_X_WINDOWS
  xassert (FRAME_X_P (f));
  
  if (face->gc == 0)
    {
      XGCValues xgcv;
      unsigned long mask = GCForeground | GCBackground | GCGraphicsExposures;

      xgcv.foreground = face->foreground;
      xgcv.background = face->background;
      xgcv.graphics_exposures = False;

      /* The font of FACE may be null if we couldn't load it.  */
      if (face->font)
	{
	  xgcv.font = face->font->fid;
	  mask |= GCFont;
	}

      BLOCK_INPUT;
      if (face->stipple)
	{
	  xgcv.fill_style = FillOpaqueStippled;
	  xgcv.stipple = x_bitmap_pixmap (f, face->stipple);
	  mask |= GCFillStyle | GCStipple;
	}

      face->gc = x_create_gc (f, mask, &xgcv);
      UNBLOCK_INPUT;
    }
#endif
}


/* Non-zero if FACE is suitable for displaying ISO8859-1.  Used in
   macro FACE_SUITABLE_FOR_CHARSET_P to avoid realizing a new face for
   ISO8859-1 if the ASCII face suffices.  */

int
face_suitable_for_iso8859_1_p (face)
     struct face *face;
{
  int len = strlen (face->font_name);
  return len >= 9 && xstricmp (face->font_name + len - 9, "iso8859-1") == 0;
}


/* Value is non-zero if FACE is suitable for displaying characters
   of CHARSET.  CHARSET < 0 means unibyte text.  */

INLINE int
face_suitable_for_charset_p (face, charset)
     struct face *face;
     int charset;
{
  int suitable_p = 0;
  
  if (charset < 0)
    {
      if (EQ (face->registry, Vface_default_registry)
	  || !NILP (Fequal (face->registry, Vface_default_registry)))
	suitable_p = 1;
    }
  else if (face->charset == charset)
    suitable_p = 1;
  else if (face->charset == CHARSET_ASCII
	   && charset == charset_latin_iso8859_1)
    suitable_p = face_suitable_for_iso8859_1_p (face);
  else if (face->charset == charset_latin_iso8859_1
	   && charset == CHARSET_ASCII)
    suitable_p = 1;

  return suitable_p;
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
  return c;
}


/* Clear out all graphics contexts for all realized faces, except for
   the basic faces.  This should be done from time to time just to avoid
   keeping too many graphics contexts that are no longer needed.  */

static void
clear_face_gcs (c)
     struct face_cache *c;
{
  if (c && FRAME_X_P (c->f))
    {
#ifdef HAVE_X_WINDOWS
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
#endif /* HAVE_X_WINDOWS */
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
    }
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
   otherwise lookup_face would find FACE for every charset, even if
   faces with the same attributes but for specific charsets exist.  */

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
   of frame F.  The face will be used to display characters of
   CHARSET.  CHARSET < 0 means the face will be used to display
   unibyte text.  The value of face-default-registry is used to choose
   a font for the face in that case.  Value is the ID of the face
   found.  If no suitable face is found, realize a new one.  */

INLINE int
lookup_face (f, attr, charset)
     struct frame *f;
     Lisp_Object *attr;
     int charset;
{
  struct face_cache *c = FRAME_FACE_CACHE (f);
  unsigned hash;
  int i;
  struct face *face;

  xassert (c != NULL);
  check_lface_attrs (attr);

  /* Look up ATTR in the face cache.  */
  hash = lface_hash (attr);
  i = hash % FACE_CACHE_BUCKETS_SIZE;
  
  for (face = c->buckets[i]; face; face = face->next)
    if (face->hash == hash
	&& (!FRAME_WINDOW_P (f)
	    || FACE_SUITABLE_FOR_CHARSET_P (face, charset))
	&& lface_equal_p (face->lface, attr))
      break;

  /* If not found, realize a new face.  */
  if (face == NULL)
    {
      face = realize_face (c, attr, charset);
      cache_face (c, face, hash);
    }

#if GLYPH_DEBUG
  xassert (face == FACE_FROM_ID (f, face->id));
  if (FRAME_X_P (f))
    xassert (charset < 0 || FACE_SUITABLE_FOR_CHARSET_P (face, charset));
#endif /* GLYPH_DEBUG */
  
  return face->id;
}


/* Return the face id of the realized face for named face SYMBOL on
   frame F suitable for displaying characters from CHARSET.  CHARSET <
   0 means unibyte text.  */

int
lookup_named_face (f, symbol, charset)
     struct frame *f;
     Lisp_Object symbol;
     int charset;
{
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  Lisp_Object symbol_attrs[LFACE_VECTOR_SIZE];
  struct face *default_face = FACE_FROM_ID (f, DEFAULT_FACE_ID);

  get_lface_attributes (f, symbol, symbol_attrs, 1);
  bcopy (default_face->lface, attrs, sizeof attrs);
  merge_face_vectors (symbol_attrs, attrs);
  return lookup_face (f, attrs, charset);
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
      face_id = lookup_named_face (f, face_name, CHARSET_ASCII);
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
#ifdef HAVE_X_WINDOWS
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
      new_face_id = lookup_face (f, attrs, CHARSET_ASCII);
      new_face = FACE_FROM_ID (f, new_face_id);

      /* If height changes, count that as one step.  */
      if (FONT_HEIGHT (new_face->font) != last_height)
	{
	  --steps;
	  last_height = FONT_HEIGHT (new_face->font);
	  last_pt = pt;
	}
    }

  return new_face_id;

#else /* not HAVE_X_WINDOWS */

  return face_id;
  
#endif /* not HAVE_X_WINDOWS */
}


/* Return a face for charset ASCII that is like the face with id
   FACE_ID on frame F, but has height HEIGHT.  */

int
face_with_height (f, face_id, height)
     struct frame *f;
     int face_id;
     int height;
{
#ifdef HAVE_X_WINDOWS
  struct face *face;
  Lisp_Object attrs[LFACE_VECTOR_SIZE];

  if (FRAME_TERMCAP_P (f)
      || height <= 0)
    return face_id;

  face = FACE_FROM_ID (f, face_id);
  bcopy (face->lface, attrs, sizeof attrs);
  attrs[LFACE_HEIGHT_INDEX] = make_number (height);
  face_id = lookup_face (f, attrs, CHARSET_ASCII);
#endif /* HAVE_X_WINDOWS */
  
  return face_id;
}

/* Return the face id of the realized face for named face SYMBOL on
   frame F suitable for displaying characters from CHARSET (CHARSET <
   0 means unibyte text), and use attributes of the face FACE_ID for
   attributes that aren't completely specified by SYMBOL.  This is
   like lookup_named_face, except that the default attributes come
   from FACE_ID, not from the default face.  FACE_ID is assumed to
   be already realized.  */

int
lookup_derived_face (f, symbol, charset, face_id)
     struct frame *f;
     Lisp_Object symbol;
     int charset;
     int face_id;
{
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  Lisp_Object symbol_attrs[LFACE_VECTOR_SIZE];
  struct face *default_face = FACE_FROM_ID (f, face_id);

  if (!default_face)
    abort ();

  get_lface_attributes (f, symbol, symbol_attrs, 1);
  bcopy (default_face->lface, attrs, sizeof attrs);
  merge_face_vectors (symbol_attrs, attrs);
  return lookup_face (f, attrs, charset);
}



/***********************************************************************
			    Font selection
 ***********************************************************************/

DEFUN ("internal-set-font-selection-order",
       Finternal_set_font_selection_order,
       Sinternal_set_font_selection_order, 1, 1, 0,
  "Set font selection order for face font selection to ORDER.\n\
ORDER must be a list of length 4 containing the symbols `:width',\n\
`:height', `:weight', and `:slant'.  Face attributes appearing\n\
first in ORDER are matched first, e.g. if `:height' appears before\n\
`:weight' in ORDER, font selection first tries to find a font with\n\
a suitable height, and then tries to match the font weight.\n\
Value is ORDER.")
  (order)
       Lisp_Object order;
{
  Lisp_Object list;
  int i;
  int indices[4];
  
  CHECK_LIST (order, 0);
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

  if (!NILP (list)
      || i != DIM (indices)
      || indices[0] == 0
      || indices[1] == 0
      || indices[2] == 0
      || indices[3] == 0)
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
  "Define alternative font families to try in face font selection.\n\
ALIST is an alist of (FAMILY ALTERNATIVE1 ALTERNATIVE2 ...) entries.\n\
Each ALTERNATIVE is tried in order if no fonts of font family FAMILY can\n\
be found.  Value is ALIST.")
  (alist)
     Lisp_Object alist;
{
  CHECK_LIST (alist, 0);
  Vface_alternative_font_family_alist = alist;
  free_all_realized_faces (Qnil);
  return alist;
}


#ifdef HAVE_X_WINDOWS

/* Return the X registry and encoding of font name FONT_NAME on frame F.
   Value is nil if not successful.  */

static Lisp_Object
deduce_unibyte_registry (f, font_name)
     struct frame *f;
     char *font_name;
{
  struct font_name font;
  Lisp_Object registry = Qnil;
  
  font.name = STRDUPA (font_name);
  if (split_font_name (f, &font, 0))
    {
      char *buffer;

      /* Extract registry and encoding.  */
      buffer = (char *) alloca (strlen (font.fields[XLFD_REGISTRY])
				+ strlen (font.fields[XLFD_ENCODING])
				+ 10);
      strcpy (buffer, font.fields[XLFD_REGISTRY]);
      strcat (buffer, "-");
      strcat (buffer, font.fields[XLFD_ENCODING]);
      registry = build_string (buffer);
    }

  return registry;
}


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
  return *s == '0' && *(s + 1) == '\0';
}


/* Value is non-zero if FONT1 is a better match for font attributes
   VALUES than FONT2.  VALUES is an array of face attribute values in
   font sort order.  COMPARE_PT_P zero means don't compare point
   sizes.  */

static int
better_font_p (values, font1, font2, compare_pt_p)
     int *values;
     struct font_name *font1, *font2;
     int compare_pt_p;
{
  int i;
  
  for (i = 0; i < 4; ++i)
    {
      int xlfd_idx = font_sort_order[i];

      if (compare_pt_p || xlfd_idx != XLFD_POINT_SIZE)
	{
	  int delta1 = abs (values[i] - font1->numeric[xlfd_idx]);
	  int delta2 = abs (values[i] - font2->numeric[xlfd_idx]);
      
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
  
  return 0;
}


#if SCALABLE_FONTS

/* Value is non-zero if FONT is an exact match for face attributes in
   SPECIFIED.  SPECIFIED is an array of face attribute values in font
   sort order.  */

static int
exact_face_match_p (specified, font)
     int *specified;
     struct font_name *font;
{
  int i;
  
  for (i = 0; i < 4; ++i)
    if (specified[i] != font->numeric[font_sort_order[i]])
      break;

  return i == 4;
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
      pixel_value = font->numeric[XLFD_RESY] / 720.0 * pt;
    }
  else
    {
      pt = specified_pt;
      pixel_value = resy / 720.0 * pt;
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
may_use_scalable_font_p (font, name)
     struct font_name *font;
     char *name;
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
	      && fast_c_string_match_ignore_case (regexp, name) >= 0)
	    return 1;
	}
    }
  
  return 0;
}

#endif /* SCALABLE_FONTS != 0 */


/* Return the name of the best matching font for face attributes
   ATTRS in the array of font_name structures FONTS which contains
   NFONTS elements.  Value is a font name which is allocated from
   the heap.  FONTS is freed by this function.  */

static char *
best_matching_font (f, attrs, fonts, nfonts)
     struct frame *f;
     Lisp_Object *attrs;
     struct font_name *fonts;
     int nfonts;
{
  char *font_name;
  struct font_name *best;
  int i, pt;
  int specified[4];
  int exact_p;

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

#if SCALABLE_FONTS

  /* Set to 1 */
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
	    && better_font_p (specified, fonts + i, best, 1))
	  {
	    best = fonts + i;

	    exact_p = exact_face_match_p (specified, best);
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
		|| better_font_p (specified, fonts + i, best, 0)
		|| (!non_scalable_has_exact_height_p
		    && !better_font_p (specified, best, fonts + i, 0)))
	      best = fonts + i;
	  }
    }

  if (font_scalable_p (best))
    font_name = build_scalable_font_name (f, best, pt);
  else
    font_name = build_font_name (best);
  
#else /* !SCALABLE_FONTS */
  
  /* Find the best non-scalable font.  */
  best = fonts;
  
  for (i = 1; i < nfonts; ++i)
    {
      xassert (!font_scalable_p (fonts + i));
      if (better_font_p (specified, fonts + i, best, 1))
	best = fonts + i;
    }
  
  font_name = build_font_name (best);

#endif /* !SCALABLE_FONTS */

  /* Free font_name structures.  */
  free_font_names (fonts, nfonts);
  
  return font_name;
}


/* Try to get a list of fonts on frame F with font family FAMILY and
   registry/encoding REGISTRY.  Return in *FONTS a pointer to a vector
   of font_name structures for the fonts matched.  Value is the number
   of fonts found.  */

static int
try_font_list (f, attrs, pattern, family, registry, fonts)
     struct frame *f;
     Lisp_Object *attrs;
     char *pattern, *family, *registry;
     struct font_name **fonts;
{
  int nfonts;

  if (family == NULL)
    family = LSTRDUPA (attrs[LFACE_FAMILY_INDEX]);
  
  nfonts = font_list (f, pattern, family, registry, fonts);
  
  if (nfonts == 0)
    {
      Lisp_Object alter;
	  
      /* Try alternative font families from
	 Vface_alternative_font_family_alist.  */
      alter = Fassoc (build_string (family),
		      Vface_alternative_font_family_alist);
      if (CONSP (alter))
	for (alter = XCDR (alter);
	     CONSP (alter) && nfonts == 0;
	     alter = XCDR (alter))
	  {
	    if (STRINGP (XCAR (alter)))
	      {
		family = LSTRDUPA (XCAR (alter));
		nfonts = font_list (f, NULL, family, registry, fonts);
	      }
	  }
	  
      /* Try font family of the default face or "fixed".  */
      if (nfonts == 0)
	{
	  struct face *dflt = FACE_FROM_ID (f, DEFAULT_FACE_ID);
	  if (dflt)
	    family = LSTRDUPA (dflt->lface[LFACE_FAMILY_INDEX]);
	  else
	    family = "fixed";
	  nfonts = font_list (f, NULL, family, registry, fonts);
	}
	  
      /* Try any family with the given registry.  */
      if (nfonts == 0)
	nfonts = font_list (f, NULL, "*", registry, fonts);
    }

  return nfonts;
}

  
/* Return the registry and encoding pattern that fonts for CHARSET
   should match.  Value is allocated from the heap.  */

char *
x_charset_registry (charset)
     int charset;
{
  Lisp_Object prop, charset_plist;
  char *registry;

  /* Get registry and encoding from the charset's plist.  */
  charset_plist = CHARSET_TABLE_INFO (charset, CHARSET_PLIST_IDX);
  prop = Fplist_get (charset_plist, Qx_charset_registry);
  
  if (STRINGP (prop))
    {
      if (index (XSTRING (prop)->data, '-'))
	registry = xstrdup (XSTRING (prop)->data);
      else
	{
	  /* If registry doesn't contain a `-', make it a pattern.  */
	  registry = (char *) xmalloc (STRING_BYTES (XSTRING (prop)) + 5);
	  strcpy (registry, XSTRING (prop)->data);
	  strcat (registry, "*-*");
	}
    }
  else if (STRINGP (Vface_default_registry))
    registry = xstrdup (XSTRING (Vface_default_registry)->data);
  else
    registry = xstrdup ("iso8859-1");

  return registry;
}


/* Return the fontset id of the fontset name or alias name given by
   the family attribute of ATTRS on frame F.  Value is -1 if the
   family attribute of ATTRS doesn't name a fontset.  */

static int
face_fontset (f, attrs)
     struct frame *f;
     Lisp_Object *attrs;
{
  Lisp_Object name = attrs[LFACE_FAMILY_INDEX];
  int fontset;
  
  name = Fquery_fontset (name, Qnil);
  if (NILP (name))
    fontset = -1;
  else
    fontset = fs_query_fontset (f, XSTRING (name)->data);

  return fontset;
}


/* Get the font to use for the face realizing the fully-specified Lisp
   face ATTRS for charset CHARSET on frame F.  CHARSET < 0 means
   unibyte text; UNIBYTE_REGISTRY is the registry and encoding to use
   in this case.  Value is the font name which is allocated from the
   heap (which means that it must be freed eventually).  */

static char *
choose_face_font (f, attrs, charset, unibyte_registry)
     struct frame *f;
     Lisp_Object *attrs;
     int charset;
     Lisp_Object unibyte_registry;
{
  struct font_name *fonts;
  int nfonts;
  char *registry;

  /* ATTRS must be fully-specified.  */
  xassert (lface_fully_specified_p (attrs));

  if (STRINGP (unibyte_registry))
    registry = xstrdup (XSTRING (unibyte_registry)->data);
  else
    registry = x_charset_registry (charset);

  nfonts = try_font_list (f, attrs, NULL, NULL, registry, &fonts);
  xfree (registry);
  return best_matching_font (f, attrs, fonts, nfonts);
}


/* Choose a font to use on frame F to display CHARSET using FONTSET
   with Lisp face attributes specified by ATTRS.  CHARSET may be any
   valid charset.  CHARSET < 0 means unibyte text.  If the fontset
   doesn't contain a font pattern for charset, use the pattern for
   CHARSET_ASCII.  Value is the font name which is allocated from the
   heap and must be freed by the caller.  */

static char *
choose_face_fontset_font (f, attrs, fontset, charset)
     struct frame *f;
     Lisp_Object *attrs;
     int fontset, charset;
{
  char *pattern;
  char *font_name = NULL;
  struct fontset_info *fontset_info;
  struct font_name *fonts;
  int nfonts;
  
  xassert (fontset >= 0 && fontset < FRAME_FONTSET_DATA (f)->n_fontsets);

  /* For unibyte text, use the ASCII font of the fontset.  Using the
     ASCII font seems to be the most reasonable thing we can do in
     this case.  */
  if (charset < 0)
    charset = CHARSET_ASCII;

  /* Get the font name pattern to use for CHARSET from the fontset.  */
  fontset_info = FRAME_FONTSET_DATA (f)->fontset_table[fontset];
  pattern = fontset_info->fontname[charset];
  if (!pattern)
    pattern = fontset_info->fontname[CHARSET_ASCII];
  xassert (pattern);

  /* Get a list of fonts matching that pattern and choose the 
     best match for the specified face attributes from it.  */
  nfonts = try_font_list (f, attrs, pattern, NULL, NULL, &fonts);
  font_name = best_matching_font (f, attrs, fonts, nfonts);
  return font_name;
}

#endif /* HAVE_X_WINDOWS */



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
  
  if (realize_default_face (f))
    {
      realize_named_face (f, Qmode_line, MODE_LINE_FACE_ID);
      realize_named_face (f, Qtool_bar, TOOL_BAR_FACE_ID);
      realize_named_face (f, Qfringe, BITMAP_AREA_FACE_ID);
      realize_named_face (f, Qheader_line, HEADER_LINE_FACE_ID);
      realize_named_face (f, Qscroll_bar, SCROLL_BAR_FACE_ID);
      realize_named_face (f, Qborder, BORDER_FACE_ID);
      realize_named_face (f, Qcursor, CURSOR_FACE_ID);
      realize_named_face (f, Qmouse, MOUSE_FACE_ID);
      realize_named_face (f, Qmenu, MENU_FACE_ID);
      success_p = 1;
    }

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
  Lisp_Object unibyte_registry;
  Lisp_Object frame_font;
  struct face *face;
  int fontset;

  /* If the `default' face is not yet known, create it.  */
  lface = lface_from_face_name (f, Qdefault, 0);
  if (NILP (lface))
    {
      Lisp_Object frame;
      XSETFRAME (frame, f);
      lface = Finternal_make_lisp_face (Qdefault, frame);
    }

#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (f))
    {
      /* Set frame_font to the value of the `font' frame parameter.  */
      frame_font = Fassq (Qfont, f->param_alist);
      xassert (CONSP (frame_font) && STRINGP (XCDR (frame_font)));
      frame_font = XCDR (frame_font);
  
      fontset = fs_query_fontset (f, XSTRING (frame_font)->data);
      if (fontset >= 0)
	{
	  /* If frame_font is a fontset name, don't use that for
	     determining font-related attributes of the default face
	     because it is just an artificial name.  Use the ASCII font of
	     the fontset, instead.  */
	  struct font_info *font_info;
	  struct font_name font;

	  BLOCK_INPUT;
	  font_info = FS_LOAD_FONT (f, FRAME_X_FONT_TABLE (f), CHARSET_ASCII,
				    NULL, fontset);
	  UNBLOCK_INPUT;
	  
	  /* Set weight etc. from the ASCII font.  */
	  if (!set_lface_from_font_name (f, lface, font_info->full_name, 0, 0))
	    return 0;
	  
	  /* Remember registry and encoding of the frame font.  */
	  unibyte_registry = deduce_unibyte_registry (f, font_info->full_name);
	  if (STRINGP (unibyte_registry))
	    Vface_default_registry = unibyte_registry;
	  else
	    Vface_default_registry = build_string ("iso8859-1");
	  
	  /* But set the family to the fontset alias name.  Implementation
	     note: When a font is passed to Emacs via `-fn FONT', a
	     fontset is created in `x-win.el' whose name ends in
	     `fontset-startup'.  This fontset has an alias name that is
	     equal to frame_font.  */
	  xassert (STRINGP (frame_font));
	  font.name = LSTRDUPA (frame_font);
	  
	  if (!split_font_name (f, &font, 1)
	      || xstricmp (font.fields[XLFD_REGISTRY], "fontset") != 0
	      || xstricmp (font.fields[XLFD_ENCODING], "startup") != 0)
	    LFACE_FAMILY (lface) = frame_font;
	}
      else
	{
	  /* Frame parameters contain a real font.  Fill default face
	     attributes from that font.  */
	  if (!set_lface_from_font_name (f, lface,
					 XSTRING (frame_font)->data, 0, 0))
	    return 0;
	  
	  /* Remember registry and encoding of the frame font.  */
	  unibyte_registry
	    = deduce_unibyte_registry (f, XSTRING (frame_font)->data);
	  if (STRINGP (unibyte_registry))
	    Vface_default_registry = unibyte_registry;
	  else
	    Vface_default_registry = build_string ("iso8859-1");
	}
    }
#endif /* HAVE_X_WINDOWS */

  if (!FRAME_WINDOW_P (f))
    {
      LFACE_FAMILY (lface) = build_string ("default");
      LFACE_SWIDTH (lface) = Qnormal;
      LFACE_HEIGHT (lface) = make_number (1);
      LFACE_WEIGHT (lface) = Qnormal;
      LFACE_SLANT (lface) = Qnormal;
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
      else if (FRAME_X_P (f))
	return 0;
      else if (FRAME_TERMCAP_P (f) || FRAME_MSDOS_P (f))
	LFACE_FOREGROUND (lface) = Qunspecified_fg;
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
      else if (FRAME_X_P (f))
	return 0;
      else if (FRAME_TERMCAP_P (f) || FRAME_MSDOS_P (f))
	LFACE_BACKGROUND (lface) = Qunspecified_bg;
      else
	abort ();
    }
  
  if (UNSPECIFIEDP (LFACE_STIPPLE (lface)))
    LFACE_STIPPLE (lface) = Qnil;

  /* Realize the face; it must be fully-specified now.  */
  xassert (lface_fully_specified_p (XVECTOR (lface)->contents));
  check_lface (lface);
  bcopy (XVECTOR (lface)->contents, attrs, sizeof attrs);
  face = realize_face (c, attrs, CHARSET_ASCII);

  /* Remove the former default face.  */
  if (c->used > DEFAULT_FACE_ID)
    {
      struct face *default_face = FACE_FROM_ID (f, DEFAULT_FACE_ID);
      uncache_face (c, default_face);
      free_realized_face (f, default_face);
    }

  /* Insert the new default face.  */
  cache_face (c, face, lface_hash (attrs));
  xassert (face->id == DEFAULT_FACE_ID);
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
  merge_face_vectors (symbol_attrs, attrs);

  /* Realize the face.  */
  new_face = realize_face (c, attrs, CHARSET_ASCII);

  /* Remove the former face.  */
  if (c->used > id)
    {
      struct face *old_face = c->faces_by_id[id];
      uncache_face (c, old_face);
      free_realized_face (f, old_face);
    }

  /* Insert the new face.  */
  cache_face (c, new_face, lface_hash (attrs));
  xassert (new_face->id == id);
}


/* Realize the fully-specified face with attributes ATTRS in face
   cache C for character set CHARSET or for unibyte text if CHARSET <
   0.  Value is a pointer to the newly created realized face.  */

static struct face *
realize_face (c, attrs, charset)
     struct face_cache *c;
     Lisp_Object *attrs;
     int charset;
{
  struct face *face;
  
  /* LFACE must be fully specified.  */
  xassert (c != NULL);
  check_lface_attrs (attrs);

  if (FRAME_X_P (c->f))
    face = realize_x_face (c, attrs, charset);
  else if (FRAME_TERMCAP_P (c->f) || FRAME_MSDOS_P (c->f))
    face = realize_tty_face (c, attrs, charset);
  else
    abort ();

  return face;
}


/* Realize the fully-specified face with attributes ATTRS in face
   cache C for character set CHARSET or for unibyte text if CHARSET <
   0.  Do it for X frame C->f.  Value is a pointer to the newly
   created realized face.  */

static struct face *
realize_x_face (c, attrs, charset)
     struct face_cache *c;
     Lisp_Object *attrs;
     int charset;
{
#ifdef HAVE_X_WINDOWS
  struct face *face, *default_face;
  struct frame *f;
  Lisp_Object stipple, overline, strike_through, box;
  Lisp_Object unibyte_registry;
  struct gcpro gcpro1;

  xassert (FRAME_X_P (f));

  /* If realizing a face for use in unibyte text, get the X registry
     and encoding to use from Vface_default_registry.  */
  if (charset < 0)
    unibyte_registry = (STRINGP (Vface_default_registry)
			? Vface_default_registry
			: build_string ("iso8859-1"));
  else
    unibyte_registry = Qnil;
  GCPRO1 (unibyte_registry);

  /* Allocate a new realized face.  */
  face = make_realized_face (attrs, charset, unibyte_registry);

  f = c->f;
  /* Determine the font to use.  Most of the time, the font will be
     the same as the font of the default face, so try that first.  */
  default_face = FACE_FROM_ID (f, DEFAULT_FACE_ID);
  if (default_face
      && FACE_SUITABLE_FOR_CHARSET_P (default_face, charset)
      && lface_same_font_attributes_p (default_face->lface, attrs))
    {
      face->font = default_face->font;
      face->fontset = default_face->fontset;
      face->font_info_id = default_face->font_info_id;
      face->font_name = default_face->font_name;
      face->registry = default_face->registry;
    }
  else if (charset >= 0)
    {
      /* For all charsets, we use our own font selection functions to
	 choose a best matching font for the specified face
	 attributes.  If the face specifies a fontset alias name, the
	 fontset determines the font name pattern, otherwise we
	 construct a font pattern from face attributes and charset.  */
      
      char *font_name = NULL;
      int fontset = face_fontset (f, attrs);

      if (fontset < 0)
	font_name = choose_face_font (f, attrs, charset, Qnil);
      else
	{
	  font_name = choose_face_fontset_font (f, attrs, fontset, charset);
	  fontset = -1;
	}
      
      load_face_font_or_fontset (f, face, font_name, fontset);
      xfree (font_name);
    }
  else
    {
      /* Unibyte case, and font is not equal to that of the default
	 face.  UNIBYTE_REGISTRY is the X registry and encoding the
	 font should have.  What is a reasonable thing to do if the
	 user specified a fontset alias name for the face in this
	 case?  We choose a font by taking the ASCII font of the
	 fontset, but using UNIBYTE_REGISTRY for its registry and
	 encoding.  */
      
      char *font_name = NULL;
      int fontset = face_fontset (f, attrs);
      
      if (fontset < 0)
	font_name = choose_face_font (f, attrs, charset, unibyte_registry);
      else
	font_name = choose_face_fontset_font (f, attrs, fontset, charset);
      
      load_face_font_or_fontset (f, face, font_name, -1);
      xfree (font_name);
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
      xassert (XINT (box) > 0);
      face->box = FACE_SIMPLE_BOX;
      face->box_line_width = XFASTINT (box);
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
	      if (INTEGERP (value) && XINT (value) > 0)
		face->box_line_width = XFASTINT (value);
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

  UNGCPRO;
  xassert (face->fontset < 0);
  xassert (FACE_SUITABLE_FOR_CHARSET_P (face, charset));
  return face;
#endif /* HAVE_X_WINDOWS */
}


/* Realize the fully-specified face with attributes ATTRS in face
   cache C for character set CHARSET or for unibyte text if CHARSET <
   0.  Do it for TTY frame C->f.  Value is a pointer to the newly
   created realized face.  */

static struct face *
realize_tty_face (c, attrs, charset)
     struct face_cache *c;
     Lisp_Object *attrs;
     int charset;
{
  struct face *face;
  int weight, slant;
  Lisp_Object color;
  Lisp_Object tty_color_alist = Fsymbol_value (intern ("tty-color-alist"));
  int face_colors_defaulted = 0;

  /* Frame must be a termcap frame.  */
  xassert (FRAME_TERMCAP_P (c->f) || FRAME_MSDOS_P (c->f));
  
  /* Allocate a new realized face.  */
  face = make_realized_face (attrs, charset, Qnil);
  face->font_name = FRAME_MSDOS_P (c->f) ? "ms-dos" : "tty";

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
  face->foreground = FACE_TTY_DEFAULT_FG_COLOR;
  face->background = FACE_TTY_DEFAULT_BG_COLOR;

  color = attrs[LFACE_FOREGROUND_INDEX];
  if (STRINGP (color)
      && XSTRING (color)->size
      && !NILP (tty_color_alist)
      && (color = Fassoc (color, tty_color_alist),
	  CONSP (color)))
    /* Associations in tty-color-alist are of the form
       (NAME INDEX R G B).  We need the INDEX part.  */
    face->foreground = XINT (XCAR (XCDR (color)));

  if (face->foreground == FACE_TTY_DEFAULT_FG_COLOR
      && STRINGP (attrs[LFACE_FOREGROUND_INDEX]))
    {
      face->foreground = load_color (c->f, face,
				     attrs[LFACE_FOREGROUND_INDEX],
				     LFACE_FOREGROUND_INDEX);
#ifdef MSDOS
      /* If the foreground of the default face is the default color,
	 use the foreground color defined by the frame.  */
      if (FRAME_MSDOS_P (c->f))
	{
	  if (face->foreground == FACE_TTY_DEFAULT_FG_COLOR
	      || face->foreground == FACE_TTY_DEFAULT_COLOR)
	    {
	      face->foreground = FRAME_FOREGROUND_PIXEL (f);
	      attrs[LFACE_FOREGROUND_INDEX] =
		msdos_stdcolor_name (face->foreground);
	      face_colors_defaulted = 1;
	    }
	  else if (face->foreground == FACE_TTY_DEFAULT_BG_COLOR)
	    {
	      face->foreground = FRAME_BACKGROUND_PIXEL (f);
	      attrs[LFACE_FOREGROUND_INDEX] =
		msdos_stdcolor_name (face->foreground);
	      face_colors_defaulted = 1;
	    }
	}
#endif
    }

  color = attrs[LFACE_BACKGROUND_INDEX];
  if (STRINGP (color)
      && XSTRING (color)->size
      && !NILP (tty_color_alist)
      && (color = Fassoc (color, tty_color_alist),
	  CONSP (color)))
    /* Associations in tty-color-alist are of the form
       (NAME INDEX R G B).  We need the INDEX part.  */
    face->background = XINT (XCAR (XCDR (color)));

  if (face->background == FACE_TTY_DEFAULT_BG_COLOR
      && STRINGP (attrs[LFACE_BACKGROUND_INDEX]))
    {
      face->background = load_color (c->f, face,
				     attrs[LFACE_BACKGROUND_INDEX],
				     LFACE_BACKGROUND_INDEX);
#ifdef MSDOS
      /* If the background of the default face is the default color,
	 use the background color defined by the frame.  */
      if (FRAME_MSDOS_P (c->f))
	{
	  if (face->background == FACE_TTY_DEFAULT_BG_COLOR
	      || face->background == FACE_TTY_DEFAULT_COLOR)
	    {
	      face->background = FRAME_BACKGROUND_PIXEL (f);
	      attrs[LFACE_BACKGROUND_INDEX] =
		msdos_stdcolor_name (face->background);
	      face_colors_defaulted = 1;
	    }
	  else if (face->background == FACE_TTY_DEFAULT_FG_COLOR)
	    {
	      face->background = FRAME_FOREGROUND_PIXEL (f);
	      attrs[LFACE_BACKGROUND_INDEX] =
		msdos_stdcolor_name (face->background);
	      face_colors_defaulted = 1;
	    }
	}
#endif
    }

  /* Swap colors if face is inverse-video.  If the colors are taken
     from the frame colors, they are already inverted, since the
     frame-creation function calls x-handle-reverse-video.  */
  if (face->tty_reverse_p && !face_colors_defaulted)
    {
      unsigned long tem = face->foreground;

      face->foreground = face->background;
      face->background = tem;
    }

  return face;
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
  int charset = (NILP (current_buffer->enable_multibyte_characters)
		 ? -1
		 : CHAR_CHARSET (ch));
  
  if (NILP (prop))
    face_id = FACE_FOR_CHARSET (f, DEFAULT_FACE_ID, charset);
  else
    {
      Lisp_Object attrs[LFACE_VECTOR_SIZE];
      struct face *default_face = FACE_FROM_ID (f, DEFAULT_FACE_ID);
      bcopy (default_face->lface, attrs, sizeof attrs);
      merge_face_vector_with_property (f, attrs, prop);
      face_id = lookup_face (f, attrs, charset);
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

   The face returned is suitable for displaying CHARSET_ASCII if
   current_buffer->enable_multibyte_characters is non-nil.  Otherwise,
   the face is suitable for displaying unibyte text.  */

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
  int multibyte_p = !NILP (current_buffer->enable_multibyte_characters);

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
			     &next_overlay, NULL);

    /* If there are more than 40, make enough space for all, and try
       again.  */
    if (noverlays > len)
      {
	len = noverlays;
	overlay_vec = (Lisp_Object *) alloca (len * sizeof (Lisp_Object));
	noverlays = overlays_at (pos, 0, &overlay_vec, &len,
				 &next_overlay, NULL);
      }

    if (next_overlay < endpos)
      endpos = next_overlay;
  }

  *endptr = endpos;

  default_face = FACE_FROM_ID (f, DEFAULT_FACE_ID);
  
  /* Optimize common cases where we can use the default face.  */
  if (noverlays == 0
      && NILP (prop)
      && !(pos >= region_beg && pos < region_end)
      && (multibyte_p
	  || !FRAME_WINDOW_P (f)
	  || FACE_SUITABLE_FOR_CHARSET_P (default_face, -1)))
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
      merge_face_vectors (XVECTOR (region_face)->contents, attrs);
 
      if (region_end < endpos)
	endpos = region_end;
    }

  *endptr = endpos;

  /* Look up a realized face with the given face attributes,
     or realize a new one.  Charset is ignored for tty frames.  */
  return lookup_face (f, attrs, multibyte_p ? CHARSET_ASCII : -1);
}


/* Compute the face at character position POS in Lisp string STRING on
   window W, for charset CHARSET_ASCII.

   If STRING is an overlay string, it comes from position BUFPOS in
   current_buffer, otherwise BUFPOS is zero to indicate that STRING is
   not an overlay string.  W must display the current buffer.
   REGION_BEG and REGION_END give the start and end positions of the
   region; both are -1 if no region is visible.  BASE_FACE_ID is the
   id of the basic face to merge with.  It is usually equal to
   DEFAULT_FACE_ID but can be MODE_LINE_FACE_ID or HEADER_LINE_FACE_ID
   for strings displayed in the mode or top line.
   
   Set *ENDPTR to the next position where to check for faces in
   STRING; -1 if the face is constant from POS to the end of the
   string.

   Value is the id of the face to use.  The face returned is suitable
   for displaying CHARSET_ASCII if STRING is multibyte.  Otherwise,
   the face is suitable for displaying unibyte text.  */

int
face_at_string_position (w, string, pos, bufpos, region_beg,
			 region_end, endptr, base_face_id)
     struct window *w;
     Lisp_Object string;
     int pos, bufpos;
     int region_beg, region_end;
     int *endptr;
     enum face_id base_face_id;
{
  Lisp_Object prop, position, end, limit;
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  struct face *base_face;
  int multibyte_p = STRING_MULTIBYTE (string);

  /* Get the value of the face property at the current position within
     STRING.  Value is nil if there is no face property.  */
  XSETFASTINT (position, pos);
  prop = Fget_text_property (position, Qface, string);

  /* Get the next position at which to check for faces.  Value of end
     is nil if face is constant all the way to the end of the string.
     Otherwise it is a string position where to check faces next.
     Limit is the maximum position up to which to check for property
     changes in Fnext_single_property_change.  Strings are usually
     short, so set the limit to the end of the string.  */
  XSETFASTINT (limit, XSTRING (string)->size);
  end = Fnext_single_property_change (position, Qface, string, limit);
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
	  || FACE_SUITABLE_FOR_CHARSET_P (base_face, -1)))
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
      merge_face_vectors (XVECTOR (region_face)->contents, attrs);
    }

  /* Look up a realized face with the given face attributes,
     or realize a new one.  */
  return lookup_face (f, attrs, multibyte_p ? CHARSET_ASCII : -1);
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


DEFUN ("dump-face", Fdump_face, Sdump_face, 0, 1, 0, "")
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
      CHECK_NUMBER (n, 0);
      face = FACE_FROM_ID (SELECTED_FRAME (), XINT (n));
      if (face == NULL)
	error ("Not a valid face");
      dump_realized_face (face);
    }
  
  return Qnil;
}


DEFUN ("show-face-resources", Fshow_face_resources, Sshow_face_resources,
       0, 0, 0, "")
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
  Qunspecified_fg = intern ("unspecified-fg");
  staticpro (&Qunspecified_fg);
  Qunspecified_bg = intern ("unspecified-bg");
  staticpro (&Qunspecified_bg);

  Qx_charset_registry = intern ("x-charset-registry");
  staticpro (&Qx_charset_registry);
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
  Qtty_color_desc = intern ("tty-color-desc");
  staticpro (&Qtty_color_desc);
  Qtty_color_by_index = intern ("tty-color-by-index");
  staticpro (&Qtty_color_by_index);

  defsubr (&Sinternal_make_lisp_face);
  defsubr (&Sinternal_lisp_face_p);
  defsubr (&Sinternal_set_lisp_face_attribute);
#ifdef HAVE_X_WINDOWS
  defsubr (&Sinternal_set_lisp_face_attribute_from_resource);
  defsubr (&Sface_color_gray_p);
  defsubr (&Sface_color_supported_p);
#endif
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
#if GLYPH_DEBUG
  defsubr (&Sdump_face);
  defsubr (&Sshow_face_resources);
#endif /* GLYPH_DEBUG */
  defsubr (&Sclear_face_cache);

  DEFVAR_LISP ("font-list-limit", &Vfont_list_limit,
    "*Limit for font matching.\n\
If an integer > 0, font matching functions won't load more than\n\
that number of fonts when searching for a matching font.");
  Vfont_list_limit = make_number (DEFAULT_FONT_LIST_LIMIT);

  DEFVAR_LISP ("face-new-frame-defaults", &Vface_new_frame_defaults,
    "List of global face definitions (for internal use only.)");
  Vface_new_frame_defaults = Qnil;
  
  DEFVAR_LISP ("face-default-stipple", &Vface_default_stipple,
    "*Default stipple pattern used on monochrome displays.\n\
This stipple pattern is used on monochrome displays\n\
instead of shades of gray for a face background color.\n\
See `set-face-stipple' for possible values for this variable.");
  Vface_default_stipple = build_string ("gray3");

  DEFVAR_LISP ("face-default-registry", &Vface_default_registry,
    "Default registry and encoding to use.\n\
This registry and encoding is used for unibyte text.  It is set up\n\
from the specified frame font when Emacs starts. (For internal use only.)");
  Vface_default_registry = Qnil;

  DEFVAR_LISP ("face-alternative-font-family-alist",
	       &Vface_alternative_font_family_alist, "");
  Vface_alternative_font_family_alist = Qnil;

#if SCALABLE_FONTS
  
  DEFVAR_LISP ("scalable-fonts-allowed", &Vscalable_fonts_allowed,
    "Allowed scalable fonts.\n\
A value of nil means don't allow any scalable fonts.\n\
A value of t means allow any scalable font.\n\
Otherwise, value must be a list of regular expressions.  A font may be\n\
scaled if its name matches a regular expression in the list.");
  Vscalable_fonts_allowed = Qnil;
  
#endif /* SCALABLE_FONTS */

#ifdef HAVE_X_WINDOWS
  defsubr (&Sbitmap_spec_p);
  defsubr (&Sx_list_fonts);
  defsubr (&Sinternal_face_x_get_resource);
  defsubr (&Sx_family_fonts);
  defsubr (&Sx_font_family_list);
#endif /* HAVE_X_WINDOWS */
}
