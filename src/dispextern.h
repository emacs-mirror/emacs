/* Interface definitions for display code.

Copyright (C) 1985, 1993-1994, 1997-2020 Free Software Foundation, Inc.

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

/* New redisplay written by Gerd Moellmann <gerd@gnu.org>.  */

#ifndef DISPEXTERN_H_INCLUDED
#define DISPEXTERN_H_INCLUDED

#include "character.h"

#ifdef HAVE_X_WINDOWS

#include <X11/Xlib.h>
#ifdef USE_X_TOOLKIT
#include <X11/Intrinsic.h>
#endif /* USE_X_TOOLKIT */

#ifdef HAVE_XRENDER
# include <X11/extensions/Xrender.h>
#endif

typedef XColor Emacs_Color;
typedef Cursor Emacs_Cursor;
#define No_Cursor (None)
#ifndef USE_CAIRO
typedef Pixmap Emacs_Pixmap;
#endif
typedef XRectangle Emacs_Rectangle;
typedef XGCValues Emacs_GC;
#else /* !HAVE_X_WINDOWS */

/* XColor-like struct used by non-X code.  */

typedef struct
{
  unsigned long pixel;
  unsigned short red, green, blue;
} Emacs_Color;

/* Accommodate X's usage of None as a null resource ID.  */
#define No_Cursor (NULL)

/* XRectangle-like struct used by non-X GUI code.  */
typedef struct
{
  int x, y;
  unsigned width, height;
} Emacs_Rectangle;

/* XGCValues-like struct used by non-X GUI code.  */
typedef struct
{
  unsigned long foreground;
  unsigned long background;
} Emacs_GC;

/* Mask values to select foreground/background.  */
/* FIXME: The GC handling in w32 really should be redesigned as to not
   need these.  */
#define GCForeground 0x01
#define GCBackground 0x02

#endif /* HAVE_X_WINDOWS */

#ifdef MSDOS
#include "msdos.h"
#endif

INLINE_HEADER_BEGIN

#include <c-strcase.h>
INLINE int
xstrcasecmp (char const *a, char const *b)
{
  return c_strcasecmp (a, b);
}

#ifdef HAVE_X_WINDOWS
#include <X11/Xresource.h> /* for XrmDatabase */
typedef struct x_display_info Display_Info;
#ifndef USE_CAIRO
typedef XImage *Emacs_Pix_Container;
typedef XImage *Emacs_Pix_Context;
#endif	/* !USE_CAIRO */
#define NativeRectangle XRectangle
#endif

#ifdef USE_CAIRO
/* Mininal version of XImage.  */
typedef struct
{
  int width, height;		/* size of image */
  char *data;			/* pointer to image data */
  int bytes_per_line;		/* accelarator to next line */
  int bits_per_pixel;		/* bits per pixel (ZPixmap) */
} *Emacs_Pix_Container;
typedef Emacs_Pix_Container Emacs_Pixmap;
typedef Emacs_Pix_Container Emacs_Pix_Context;
#endif

#ifdef HAVE_NTGUI
#include "w32gui.h"
typedef struct w32_display_info Display_Info;
typedef XImage *Emacs_Pix_Container;
typedef HDC Emacs_Pix_Context;
#endif

#ifdef HAVE_NS
#include "nsgui.h"
#define FACE_COLOR_TO_PIXEL(face_color, frame) (FRAME_NS_P (frame) \
                                                ? ns_color_index_to_rgba (face_color, frame) \
                                                : face_color)
/* Following typedef needed to accommodate the MSDOS port, believe it or not.  */
typedef struct ns_display_info Display_Info;
typedef Emacs_Pixmap Emacs_Pix_Container;
typedef Emacs_Pixmap Emacs_Pix_Context;
#else
#define FACE_COLOR_TO_PIXEL(face_color, frame) face_color
#endif

#ifdef HAVE_WINDOW_SYSTEM
# include <time.h>
# include "fontset.h"
#endif

#ifndef HAVE_WINDOW_SYSTEM
typedef void *Emacs_Cursor;
#endif

#ifndef NativeRectangle
#define NativeRectangle int
#endif

/* Text cursor types.  */

enum text_cursor_kinds
{
  DEFAULT_CURSOR = -2,
  NO_CURSOR = -1,
  FILLED_BOX_CURSOR,
  HOLLOW_BOX_CURSOR,
  BAR_CURSOR,
  HBAR_CURSOR
};

/* Values returned from coordinates_in_window.  */

enum window_part
{
  ON_NOTHING,
  ON_TEXT,
  ON_MODE_LINE,
  ON_VERTICAL_BORDER,
  ON_HEADER_LINE,
  ON_TAB_LINE,
  ON_LEFT_FRINGE,
  ON_RIGHT_FRINGE,
  ON_LEFT_MARGIN,
  ON_RIGHT_MARGIN,
  ON_VERTICAL_SCROLL_BAR,
  ON_HORIZONTAL_SCROLL_BAR,
  ON_RIGHT_DIVIDER,
  ON_BOTTOM_DIVIDER
};

/* Number of bits allocated to store fringe bitmap numbers.  */
#define FRINGE_ID_BITS  16

/* Number of bits allocated to store fringe bitmap height.  */
#define FRINGE_HEIGHT_BITS 8


/***********************************************************************
			      Debugging
 ***********************************************************************/

/* If GLYPH_DEBUG is defined, additional checks are activated.  */

/* Macros to include code only if GLYPH_DEBUG is defined.  */

#ifdef GLYPH_DEBUG
#define IF_DEBUG(X)	((void) (X))
#else
#define IF_DEBUG(X)	((void) 0)
#endif

/***********************************************************************
			    Text positions
 ***********************************************************************/

/* Starting with Emacs 20.3, characters from strings and buffers have
   both a character and a byte position associated with them.  The
   following structure holds such a pair of positions.  */

struct text_pos
{
  /* Character position.  */
  ptrdiff_t charpos;

  /* Corresponding byte position.  */
  ptrdiff_t bytepos;
};

/* Access character and byte position of POS in a functional form.  */

#define BYTEPOS(POS)	(POS).bytepos
#define CHARPOS(POS)	(POS).charpos

/* Set character position of POS to CHARPOS, byte position to BYTEPOS.  */

#define SET_TEXT_POS(POS, CHARPOS, BYTEPOS) \
     ((POS).charpos = (CHARPOS), (POS).bytepos = BYTEPOS)

/* Increment text position POS.  */

#define INC_TEXT_POS(POS, MULTIBYTE_P)		\
     do						\
       {					\
	 ++(POS).charpos;			\
         if (MULTIBYTE_P)			\
	   INC_POS ((POS).bytepos);		\
	 else					\
	   ++(POS).bytepos;			\
       }					\
     while (false)

/* Decrement text position POS.  */

#define DEC_TEXT_POS(POS, MULTIBYTE_P)		\
     do						\
       {					\
	 --(POS).charpos;			\
         if (MULTIBYTE_P)			\
	   DEC_POS ((POS).bytepos);		\
	 else					\
	   --(POS).bytepos;			\
       }					\
     while (false)

/* Set text position POS from marker MARKER.  */

#define SET_TEXT_POS_FROM_MARKER(POS, MARKER)		\
  (CHARPOS (POS) = marker_position (MARKER),		\
   BYTEPOS (POS) = marker_byte_position (MARKER))

/* Like above, but clip POS within accessible range.  */

#define CLIP_TEXT_POS_FROM_MARKER(POS, MARKER)		\
  (CHARPOS (POS) = clip_to_bounds			\
   (BEGV, marker_position (MARKER), ZV),		\
   BYTEPOS (POS) = clip_to_bounds			\
   (BEGV_BYTE, marker_byte_position (MARKER), ZV_BYTE))

/* Set marker MARKER from text position POS.  */

#define SET_MARKER_FROM_TEXT_POS(MARKER, POS) \
     set_marker_both ((MARKER), Qnil, CHARPOS ((POS)), BYTEPOS ((POS)))

/* Value is non-zero if character and byte positions of POS1 and POS2
   are equal.  */

#define TEXT_POS_EQUAL_P(POS1, POS2)		\
     ((POS1).charpos == (POS2).charpos		\
      && (POS1).bytepos == (POS2).bytepos)

/* When rendering glyphs, redisplay scans string or buffer text,
   overlay strings in that text, and does display table or control
   character translations.  The following structure captures a
   position taking all this into account.  */

struct display_pos
{
  /* Buffer or string position.  */
  struct text_pos pos;

  /* If this is a position in an overlay string, overlay_string_index
     is the index of that overlay string in the sequence of overlay
     strings at `pos' in the order redisplay processes them.  A value
     < 0 means that this is not a position in an overlay string.  */
  ptrdiff_t overlay_string_index;

  /* If this is a position in an overlay string, string_pos is the
     position within that string.  */
  struct text_pos string_pos;

  /* If the character at the position above is a control character or
     has a display table entry, dpvec_index is an index in the display
     table or control character translation of that character.  A
     value < 0 means this is not a position in such a translation.  */
  int dpvec_index;
};



/***********************************************************************
				Glyphs
 ***********************************************************************/

/* The glyph datatype, used to represent characters on the display.
   It consists of a char code and a face id.  */

typedef struct {
  int ch;
  int face_id;
} GLYPH;

/* Return a glyph's character code.  */
INLINE int GLYPH_CHAR (GLYPH glyph) { return glyph.ch; }

/* Return a glyph's face ID.  */
INLINE int GLYPH_FACE (GLYPH glyph) { return glyph.face_id; }

#define SET_GLYPH_CHAR(glyph, char) ((glyph).ch = (char))
#define SET_GLYPH_FACE(glyph, face) ((glyph).face_id = (face))
#define SET_GLYPH(glyph, char, face) \
  ((glyph).ch = (char), (glyph).face_id = (face))

/* The following are valid only if GLYPH_CODE_P (gc).  */

INLINE int
GLYPH_CODE_CHAR (Lisp_Object gc)
{
  return (CONSP (gc)
	  ? XFIXNUM (XCAR (gc))
	  : XFIXNUM (gc) & MAX_CHAR);
}

INLINE int
GLYPH_CODE_FACE (Lisp_Object gc)
{
  return CONSP (gc) ? XFIXNUM (XCDR (gc)) : XFIXNUM (gc) >> CHARACTERBITS;
}

#define SET_GLYPH_FROM_GLYPH_CODE(glyph, gc)				\
  do									\
    {									\
      if (CONSP (gc))							\
	SET_GLYPH (glyph, XFIXNUM (XCAR (gc)), XFIXNUM (XCDR (gc)));		\
      else								\
	SET_GLYPH (glyph, (XFIXNUM (gc) & ((1 << CHARACTERBITS)-1)),	\
		   (XFIXNUM (gc) >> CHARACTERBITS));			\
    }									\
  while (false)

/* The ID of the mode line highlighting face.  */
enum { GLYPH_MODE_LINE_FACE = 1 };

/* Enumeration of glyph types.  Glyph structures contain a type field
   containing one of the enumerators defined here.  */

enum glyph_type
{
  /* Glyph describes a character.  */
  CHAR_GLYPH,

  /* Glyph describes a static composition.  */
  COMPOSITE_GLYPH,

  /* Glyph describes a glyphless character.  */
  GLYPHLESS_GLYPH,

  /* Glyph describes an image.  */
  IMAGE_GLYPH,

  /* Glyph is a space of fractional width and/or height.  */
  STRETCH_GLYPH,

  /* Glyph is an external widget drawn by the GUI toolkit.  */
  XWIDGET_GLYPH
};


/* Structure describing how to use partial glyphs (images slicing) */

struct glyph_slice
{
  unsigned x : 16;
  unsigned y : 16;
  unsigned width : 16;
  unsigned height : 16;
};


/* Glyphs.

   Be extra careful when changing this structure!  Esp. make sure that
   functions producing glyphs, like append_glyph, fill ALL of the
   glyph structure, and that GLYPH_EQUAL_P compares all
   display-relevant members of glyphs (not to imply that these are the
   only things to check when you add a member).  */

struct glyph
{
  /* Position from which this glyph was drawn.  If `object' below is a
     Lisp string, this is an index into that string.  If it is a
     buffer, this is a position in that buffer.  In addition, some
     special glyphs have special values for this:

      glyph standing for newline at end of line    0
      empty space after the end of the line       -1
      overlay arrow on a TTY                      -1
      glyph displaying line number                -1
      glyph at EOB that ends in a newline         -1
      left truncation glyphs:                     -1
      right truncation/continuation glyphs        next buffer position
      glyph standing for newline of an empty line buffer position of newline
      stretch glyph at left edge of R2L lines     buffer position of newline  */
  ptrdiff_t charpos;

  /* Lisp object source of this glyph.  Currently either a buffer or a
     string, if the glyph was produced from characters which came from
     a buffer or a string; or nil if the glyph was inserted by
     redisplay for its own purposes, such as padding, truncation, or
     continuation glyphs, or the overlay-arrow glyphs on TTYs.  */
  Lisp_Object object;

  /* Width in pixels.  */
  short pixel_width;

  /* Ascent and descent in pixels.  */
  short ascent, descent;

  /* Vertical offset.  If < 0, the glyph is displayed raised, if > 0
     the glyph is displayed lowered.  */
  short voffset;

  /* Which kind of glyph this is---character, image etc.  Value
     should be an enumerator of type enum glyph_type.  */
  unsigned type : 3;

  /* True means this glyph was produced from multibyte text.  False
     means it was produced from unibyte text, i.e. charsets aren't
     applicable, and encoding is not performed.  */
  bool_bf multibyte_p : 1;

  /* True means draw a box line at the left or right side of this
     glyph.  This is part of the implementation of the face attribute
     `:box'.  */
  bool_bf left_box_line_p : 1;
  bool_bf right_box_line_p : 1;

  /* True means this glyph's physical ascent or descent is greater
     than its logical ascent/descent, i.e. it may potentially overlap
     glyphs above or below it.  */
  bool_bf overlaps_vertically_p : 1;

  /* For terminal frames, true means glyph is a padding glyph.  Padding
     glyphs are used for characters whose visual shape consists of
     more than one glyph (e.g. Asian characters).  All but the first
     glyph of such a glyph sequence have the padding_p flag set.  This
     flag is used only to minimize code changes.  A better way would
     probably be to use the width field of glyphs to express padding.

     For graphic frames, true means the pixel width of the glyph in a
     font is 0, but 1-pixel is padded on displaying for correct cursor
     displaying.  The member `pixel_width' above is set to 1.  */
  bool_bf padding_p : 1;

  /* True means the actual glyph is not available, draw using `struct
     glyphless' below instead.  This can happen when a font couldn't
     be loaded, or a character doesn't have a glyph in a font.  */
  bool_bf glyph_not_available_p : 1;

  /* True means don't display cursor here.  */
  bool_bf avoid_cursor_p : 1;

  /* Resolved bidirectional level of this character [0..127].  */
  unsigned resolved_level : 7;

  /* Resolved bidirectional type of this character, see enum
     bidi_type_t below.  Note that according to UAX#9, only some
     values (STRONG_L, STRONG_R, WEAK_AN, WEAK_EN, WEAK_BN, and
     NEUTRAL_B) can appear in the resolved type, so we only reserve
     space for those that can.  */
  unsigned bidi_type : 3;

#define FACE_ID_BITS	20

  /* Face of the glyph.  This is a realized face ID,
     an index in the face cache of the frame.  */
  unsigned face_id : FACE_ID_BITS;

  /* Type of font used to display the character glyph.  May be used to
     determine which set of functions to use to obtain font metrics
     for the glyph.  On W32, value should be an enumerator of the type
     w32_char_font_type.  Otherwise it equals FONT_TYPE_UNKNOWN.  */
  unsigned font_type : 3;

  /* A union of sub-structures for different glyph types.  */
  union
  {
    /* Metrics of a partial glyph of an image (type == IMAGE_GLYPH).  */
    struct glyph_slice img;
    /* Start and end indices of glyphs of a grapheme cluster of a
       composition (type == COMPOSITE_GLYPH).  */
    struct { int from, to; } cmp;
    /* Pixel offsets for upper and lower part of the acronym.  */
    struct {
      short upper_xoff, upper_yoff;
      short lower_xoff, lower_yoff;
    } glyphless;
  } slice;

  /* A union of sub-structures for different glyph types.  */
  union
  {
    /* Character code for character glyphs (type == CHAR_GLYPH).  */
    unsigned ch;

    /* Sub-structures for type == COMPOSITE_GLYPH.  */
    struct
    {
      /* Flag to tell if the composition is automatic or not.  */
      bool_bf automatic : 1;
      /* ID of the composition.  */
      unsigned id    : 31;
    } cmp;

    /* Image ID for image glyphs (type == IMAGE_GLYPH).  */
    int img_id;

#ifdef HAVE_XWIDGETS
    /* Xwidget reference (type == XWIDGET_GLYPH).  */
    struct xwidget *xwidget;
#endif

    /* Sub-structure for type == STRETCH_GLYPH.  */
    struct
    {
      /* The height of the glyph.  */
      unsigned height  : 16;

      /* The ascent of the glyph.  */
      unsigned ascent  : 16;
    }
    stretch;

    /* Sub-stretch for type == GLYPHLESS_GLYPH.  */
    struct
    {
      /* Value is an enum of the type glyphless_display_method.  */
      unsigned method : 2;
      /* True iff this glyph is for a character of no font. */
      bool_bf for_no_font : 1;
      /* Length of acronym or hexadecimal code string (at most 8).  */
      unsigned len : 4;
      /* Character to display.  Actually we need only 22 bits.  */
      unsigned ch : 25;
    } glyphless;

    /* Used to compare all bit-fields above in one step.  */
    unsigned val;
  } u;
};


/* Default value of the glyph font_type field.  */

#define FONT_TYPE_UNKNOWN	0

/* Is GLYPH a space?  */

#define CHAR_GLYPH_SPACE_P(GLYPH) \
  ((GLYPH).u.ch == SPACEGLYPH && (GLYPH).face_id == DEFAULT_FACE_ID)

/* Are glyph slices of glyphs *X and *Y equal?  It assumes that both
   glyphs have the same type.

   Note: for composition glyphs, we don't have to compare slice.cmp.to
   because they should be the same if and only if slice.cmp.from are
   the same.  */

#define GLYPH_SLICE_EQUAL_P(X, Y)				\
  ((X)->type == IMAGE_GLYPH					\
   ? ((X)->slice.img.x == (Y)->slice.img.x			\
      && (X)->slice.img.y == (Y)->slice.img.y			\
      && (X)->slice.img.width == (Y)->slice.img.width		\
      && (X)->slice.img.height == (Y)->slice.img.height)	\
   : ((X)->type != COMPOSITE_GLYPH				\
      || (X)->slice.cmp.from == (Y)->slice.cmp.from))

/* Are glyphs *X and *Y displayed equal?  */

#define GLYPH_EQUAL_P(X, Y)					\
     ((X)->type == (Y)->type					\
      && (X)->u.val == (Y)->u.val				\
      && GLYPH_SLICE_EQUAL_P (X, Y)				\
      && (X)->face_id == (Y)->face_id				\
      && (X)->padding_p == (Y)->padding_p			\
      && (X)->left_box_line_p == (Y)->left_box_line_p		\
      && (X)->right_box_line_p == (Y)->right_box_line_p		\
      && (X)->voffset == (Y)->voffset				\
      && (X)->pixel_width == (Y)->pixel_width)

/* Are character codes, faces, padding_ps of glyphs *X and *Y equal?  */

#define GLYPH_CHAR_AND_FACE_EQUAL_P(X, Y)	\
  ((X)->u.ch == (Y)->u.ch			\
   && (X)->face_id == (Y)->face_id		\
   && (X)->padding_p == (Y)->padding_p)

/* Fill a character glyph GLYPH.  CODE, FACE_ID, PADDING_P correspond
   to the bits defined for the typedef `GLYPH' in lisp.h.  */

#define SET_CHAR_GLYPH(GLYPH, CODE, FACE_ID, PADDING_P)	\
     do							\
       {						\
         (GLYPH).u.ch = (CODE);				\
         (GLYPH).face_id = (FACE_ID);			\
         (GLYPH).padding_p = (PADDING_P);		\
       }						\
     while (false)

/* Fill a character type glyph GLYPH from a glyph typedef FROM as
   defined in lisp.h.  */

#define SET_CHAR_GLYPH_FROM_GLYPH(GLYPH, FROM)			\
     SET_CHAR_GLYPH ((GLYPH),					\
	 	     GLYPH_CHAR ((FROM)),			\
		     GLYPH_FACE ((FROM)),			\
		     false)

/* Construct a glyph code from a character glyph GLYPH.  If the
   character is multibyte, return -1 as we can't use glyph table for a
   multibyte character.  */

#define SET_GLYPH_FROM_CHAR_GLYPH(G, GLYPH)			\
  do								\
    {								\
      if ((GLYPH).u.ch < 256)					\
	SET_GLYPH ((G), (GLYPH).u.ch, ((GLYPH).face_id));	\
      else							\
	SET_GLYPH ((G), -1, 0);					\
    }								\
  while (false)

#define GLYPH_INVALID_P(GLYPH) (GLYPH_CHAR (GLYPH) < 0)

/* Is GLYPH a padding glyph?  */

#define CHAR_GLYPH_PADDING_P(GLYPH) (GLYPH).padding_p




/***********************************************************************
			     Glyph Pools
 ***********************************************************************/

/* Glyph Pool.

   Glyph memory for frame-based redisplay is allocated from the heap
   in one vector kept in a glyph pool structure which is stored with
   the frame.  The size of the vector is made large enough to cover
   all windows on the frame.

   Both frame and window glyph matrices reference memory from a glyph
   pool in frame-based redisplay.

   In window-based redisplay, no glyphs pools exist; windows allocate
   and free their glyph memory themselves.  */

struct glyph_pool
{
  /* Vector of glyphs allocated from the heap.  */
  struct glyph *glyphs;

  /* Allocated size of `glyphs'.  */
  ptrdiff_t nglyphs;

  /* Number of rows and columns in a matrix.  */
  int nrows, ncolumns;
};



/***********************************************************************
			     Glyph Matrix
 ***********************************************************************/

/* Glyph Matrix.

   Three kinds of glyph matrices exist:

   1. Frame glyph matrices.  These are used for terminal frames whose
   redisplay needs a view of the whole screen due to limited terminal
   capabilities.  Frame matrices are used only in the update phase
   of redisplay.  They are built in update_frame and not used after
   the update has been performed.

   2. Window glyph matrices on frames having frame glyph matrices.
   Such matrices are sub-matrices of their corresponding frame matrix,
   i.e., frame glyph matrices and window glyph matrices share the same
   glyph memory, which is allocated in the form of a glyph_pool structure.
   Glyph rows in such a window matrix are slices of frame matrix rows.

   3. Free-standing window glyph matrices managing their own glyph
   storage.  This form is used in window-based redisplay which
   includes variable width and height fonts etc.

   The size of a window's row vector depends on the height of fonts
   defined on its frame.  It is chosen so that the vector is large
   enough to describe all lines in a window when it is displayed in
   the smallest possible character size.  When new fonts are loaded,
   or window sizes change, the row vector is adjusted accordingly.  */

struct glyph_matrix
{
  /* The pool from which glyph memory is allocated, if any.  This is
     null for frame matrices and for window matrices managing their
     own storage.  */
  struct glyph_pool *pool;

  /* Vector of glyph row structures.  The row at nrows - 1 is reserved
     for the mode line.  */
  struct glyph_row *rows;

  /* Number of elements allocated for the vector rows above.  */
  ptrdiff_t rows_allocated;

  /* The number of rows used by the window if all lines were displayed
     with the smallest possible character height.  */
  int nrows;

  /* Origin within the frame matrix if this is a window matrix on a
     frame having a frame matrix.  Both values are zero for
     window-based redisplay.  */
  int matrix_x, matrix_y;

  /* Width and height of the matrix in columns and rows.  */
  int matrix_w, matrix_h;

  /* If this structure describes a window matrix of window W,
     window_pixel_left is the value of W->pixel_left, window_pixel_top
     the value of W->pixel_top, window_height and window_width are width
     and height of W, as returned by window_box, and window_vscroll is
     the value of W->vscroll at the time the matrix was last adjusted.
     Only set for window-based redisplay.  */
  int window_pixel_left, window_pixel_top;
  int window_height, window_width;
  int window_vscroll;

  /* Number of glyphs reserved for left and right marginal areas when
     the matrix was last adjusted.  */
  int left_margin_glyphs, right_margin_glyphs;

  /* Flag indicating that scrolling should not be tried in
     update_window.  This flag is set by functions like try_window_id
     which do their own scrolling.  */
  bool_bf no_scrolling_p : 1;

  /* True means window displayed in this matrix has a tab line.  */
  bool_bf tab_line_p : 1;

  /* True means window displayed in this matrix has a header
     line.  */
  bool_bf header_line_p : 1;

#ifdef GLYPH_DEBUG
  /* A string identifying the method used to display the matrix.  */
  char method[512];
#endif

  /* The buffer this matrix displays.  Set in
     mark_window_display_accurate_1.  */
  struct buffer *buffer;

  /* Values of BEGV and ZV as of last redisplay.  Set in
     mark_window_display_accurate_1.  */
  ptrdiff_t begv, zv;
};


/* Check that glyph pointers stored in glyph rows of MATRIX are okay.
   This aborts if any pointer is found twice.  */

#ifdef GLYPH_DEBUG
void check_matrix_pointer_lossage (struct glyph_matrix *);
#define CHECK_MATRIX(MATRIX) check_matrix_pointer_lossage ((MATRIX))
#else
#define CHECK_MATRIX(MATRIX) ((void) 0)
#endif



/***********************************************************************
			     Glyph Rows
 ***********************************************************************/

/* Area in window glyph matrix.  If values are added or removed,
   the function mark_glyph_matrix in alloc.c may need to be changed.  */

enum glyph_row_area
{
  ANY_AREA = -1,
  LEFT_MARGIN_AREA,
  TEXT_AREA,
  RIGHT_MARGIN_AREA,
  LAST_AREA
};


/* Rows of glyphs in a windows or frame glyph matrix.

   Each row is partitioned into three areas.  The start and end of
   each area is recorded in a pointer as shown below.

   +--------------------+-------------+---------------------+
   |  left margin area  |  text area  |  right margin area  |
   +--------------------+-------------+---------------------+
   |                    |             |                     |
   glyphs[LEFT_MARGIN_AREA]           glyphs[RIGHT_MARGIN_AREA]
			|                                   |
			glyphs[TEXT_AREA]                   |
			                      glyphs[LAST_AREA]

   Rows in frame matrices reference glyph memory allocated in a frame
   glyph pool (see the description of struct glyph_pool).  Rows in
   window matrices on frames having frame matrices reference slices of
   the glyphs of corresponding rows in the frame matrix.

   Rows in window matrices on frames having no frame matrices point to
   glyphs allocated from the heap via xmalloc;
   glyphs[LEFT_MARGIN_AREA] is the start address of the allocated
   glyph structure array.

   NOTE: layout of first four members of this structure is important,
   see clear_glyph_row and copy_row_except_pointers to check why.  */

struct glyph_row
{
  /* Pointers to beginnings of areas.  The end of an area A is found at
     A + 1 in the vector.  The last element of the vector is the end
     of the whole row.

     Kludge alert: Even if used[TEXT_AREA] == 0, glyphs[TEXT_AREA][0]'s
     position field is used.  It is -1 if this row does not correspond
     to any text; it is some buffer position if the row corresponds to
     an empty display line that displays a line end.  This is what old
     redisplay used to do.  (Except in code for terminal frames, this
     kludge is no longer used, I believe. --gerd).

     See also start, end, displays_text_p and ends_at_zv_p for cleaner
     ways to do it.  The special meaning of positions 0 and -1 will be
     removed some day, so don't use it in new code.  */
  struct glyph *glyphs[1 + LAST_AREA];

  /* Number of glyphs actually filled in areas.  This could have size
     LAST_AREA, but it's 1 + LAST_AREA to simplify offset calculations.  */
  short used[1 + LAST_AREA];

  /* Hash code.  This hash code is available as soon as the row
     is constructed, i.e. after a call to display_line.  */
  unsigned hash;

  /* Window-relative x and y-position of the top-left corner of this
     row.  If y < 0, this means that eabs (y) pixels of the row are
     invisible because it is partially visible at the top of a window.
     If x < 0, this means that eabs (x) pixels of the first glyph of
     the text area of the row are invisible because the glyph is
     partially visible.  */
  int x, y;

  /* Width of the row in pixels without taking face extension at the
     end of the row into account, and without counting truncation
     and continuation glyphs at the end of a row on ttys.  */
  int pixel_width;

  /* Logical ascent/height of this line.  The value of ascent is zero
     and height is 1 on terminal frames.  */
  int ascent, height;

  /* Physical ascent/height of this line.  If max_ascent > ascent,
     this line overlaps the line above it on the display.  Otherwise,
     if max_height > height, this line overlaps the line beneath it.  */
  int phys_ascent, phys_height;

  /* Portion of row that is visible.  Partially visible rows may be
     found at the top and bottom of a window.  This is 1 for tty
     frames.  It may be < 0 in case of completely invisible rows.  */
  int visible_height;

  /* Extra line spacing added after this row.  Do not consider this
     in last row when checking if row is fully visible.  */
  int extra_line_spacing;

  /* First position in this row.  This is the text position, including
     overlay position information etc, where the display of this row
     started, and can thus be less than the position of the first
     glyph (e.g. due to invisible text or horizontal scrolling).
     BIDI Note: In R2L rows, that have its reversed_p flag set, this
     position is at or beyond the right edge of the row.  */
  struct display_pos start;

  /* Text position at the end of this row.  This is the position after
     the last glyph on this row.  It can be greater than the last
     glyph position + 1, due to a newline that ends the line,
     truncation, invisible text etc.  In an up-to-date display, this
     should always be equal to the start position of the next row.
     BIDI Note: In R2L rows, this position is at or beyond the left
     edge of the row.  */
  struct display_pos end;

  /* The smallest and the largest buffer positions that contributed to
     glyphs in this row.  Note that due to bidi reordering, these are
     in general different from the text positions stored in `start'
     and `end' members above, and also different from the buffer
     positions recorded in the glyphs displayed the leftmost and
     rightmost on the screen.  */
  struct text_pos minpos, maxpos;

  /* Non-zero means the overlay arrow bitmap is on this line.
     -1 means use default overlay arrow bitmap, else
     it specifies actual fringe bitmap number.  */
  int overlay_arrow_bitmap;

  /* Left fringe bitmap number (enum fringe_bitmap_type).  */
  unsigned left_user_fringe_bitmap : FRINGE_ID_BITS;

  /* Right fringe bitmap number (enum fringe_bitmap_type).  */
  unsigned right_user_fringe_bitmap : FRINGE_ID_BITS;

  /* Left fringe bitmap number (enum fringe_bitmap_type).  */
  unsigned left_fringe_bitmap : FRINGE_ID_BITS;

  /* Right fringe bitmap number (enum fringe_bitmap_type).  */
  unsigned right_fringe_bitmap : FRINGE_ID_BITS;

  /* Face of the left fringe glyph.  */
  unsigned left_user_fringe_face_id : FACE_ID_BITS;

  /* Face of the right fringe glyph.  */
  unsigned right_user_fringe_face_id : FACE_ID_BITS;

  /* Face of the left fringe glyph.  */
  unsigned left_fringe_face_id : FACE_ID_BITS;

  /* Face of the right fringe glyph.  */
  unsigned right_fringe_face_id : FACE_ID_BITS;

  /* Vertical offset of the left fringe bitmap.  */
  signed left_fringe_offset : FRINGE_HEIGHT_BITS;

  /* Vertical offset of the right fringe bitmap.  */
  signed right_fringe_offset : FRINGE_HEIGHT_BITS;

  /* True means that at least one of the left and right fringe bitmaps is
     periodic and thus depends on the y-position of the row.  */
  bool_bf fringe_bitmap_periodic_p : 1;

  /* True means that we must draw the bitmaps of this row.  */
  bool_bf redraw_fringe_bitmaps_p : 1;

  /* In a desired matrix, true means that this row must be updated.  In a
     current matrix, false means that the row has been invalidated, i.e.
     the row's contents do not agree with what is visible on the
     screen.  */
  bool_bf enabled_p : 1;

  /* True means row displays a text line that is truncated on the left or
     right side.  */
  bool_bf truncated_on_left_p : 1;
  bool_bf truncated_on_right_p : 1;

  /* True means that this row displays a continued line, i.e. it has a
     continuation mark at the right side.  */
  bool_bf continued_p : 1;

  /* False means that this row does not contain any text, i.e., it is
     a blank line at the window and buffer end.  */
  bool_bf displays_text_p : 1;

  /* True means that this line ends at ZV.  */
  bool_bf ends_at_zv_p : 1;

  /* True means the face of the last glyph in the text area is drawn to
     the right end of the window.  This flag is used in
     update_text_area to optimize clearing to the end of the area.  */
  bool_bf fill_line_p : 1;

  /* True means display a bitmap on X frames indicating that this
     line contains no text and ends in ZV.  */
  bool_bf indicate_empty_line_p : 1;

  /* True means this row contains glyphs that overlap each other because
     of lbearing or rbearing.  */
  bool_bf contains_overlapping_glyphs_p : 1;

  /* True means this row is as wide as the window it is displayed in, including
     scroll bars, fringes, and internal borders.  This also
     implies that the row doesn't have marginal areas.  */
  bool_bf full_width_p : 1;

  /* True means row is a mode or header/tab-line.  */
  bool_bf mode_line_p : 1;

  /* True means row is a tab-line.  */
  bool_bf tab_line_p : 1;

  /* True in a current row means this row is overlapped by another row.  */
  bool_bf overlapped_p : 1;

  /* True means this line ends in the middle of a character consisting
     of more than one glyph.  Some glyphs have been put in this row,
     the rest are put in rows below this one.  */
  bool_bf ends_in_middle_of_char_p : 1;

  /* True means this line starts in the middle of a character consisting
     of more than one glyph.  Some glyphs have been put in the
     previous row, the rest are put in this row.  */
  bool_bf starts_in_middle_of_char_p : 1;

  /* True in a current row means this row overlaps others.  */
  bool_bf overlapping_p : 1;

  /* True means some glyphs in this row are displayed in mouse-face.  */
  bool_bf mouse_face_p : 1;

  /* True means this row was ended by a newline from a string.  */
  bool_bf ends_in_newline_from_string_p : 1;

  /* True means this row width is exactly the width of the window, and the
     final newline character is hidden in the right fringe.  */
  bool_bf exact_window_width_line_p : 1;

  /* True means this row currently shows the cursor in the right fringe.  */
  bool_bf cursor_in_fringe_p : 1;

  /* True means the last glyph in the row is part of an ellipsis.  */
  bool_bf ends_in_ellipsis_p : 1;

  /* True means display a bitmap on X frames indicating that this
     the first line of the buffer.  */
  bool_bf indicate_bob_p : 1;

  /* True means display a bitmap on X frames indicating that this
     the top line of the window, but not start of the buffer.  */
  bool_bf indicate_top_line_p : 1;

  /* True means display a bitmap on X frames indicating that this
     the last line of the buffer.  */
  bool_bf indicate_eob_p : 1;

  /* True means display a bitmap on X frames indicating that this
     the bottom line of the window, but not end of the buffer.  */
  bool_bf indicate_bottom_line_p : 1;

  /* True means the row was reversed to display text in a
     right-to-left paragraph.  */
  bool_bf reversed_p : 1;

  /* Continuation lines width at the start of the row.  */
  int continuation_lines_width;

#ifdef HAVE_WINDOW_SYSTEM
  /* Non-NULL means the current clipping area.  This is temporarily
     set while exposing a region.  Coordinates are frame-relative.  */
  const Emacs_Rectangle *clip;
#endif
};


/* Get a pointer to row number ROW in matrix MATRIX.  If GLYPH_DEBUG
   is defined, the function matrix_row checks that we don't try to
   access rows that are out of bounds.  */

#ifdef GLYPH_DEBUG
struct glyph_row *matrix_row (struct glyph_matrix *, int);
#define MATRIX_ROW(MATRIX, ROW)   matrix_row ((MATRIX), (ROW))
#else
#define MATRIX_ROW(MATRIX, ROW)	  ((MATRIX)->rows + (ROW))
#endif

/* Return a pointer to the row reserved for the mode line in MATRIX.
   Row MATRIX->nrows - 1 is always reserved for the mode line.  */

#define MATRIX_MODE_LINE_ROW(MATRIX) \
     ((MATRIX)->rows + (MATRIX)->nrows - 1)

/* Return a pointer to the row reserved for the tab line in MATRIX.
   This is always the first row in MATRIX because that's the only
   way that works in frame-based redisplay.  */

#define MATRIX_TAB_LINE_ROW(MATRIX) (MATRIX)->rows

/* Return a pointer to the row reserved for the header line in MATRIX.
   This is always the second row in MATRIX because that's the only
   way that works in frame-based redisplay.  */

#define MATRIX_HEADER_LINE_ROW(MATRIX) \
     ((MATRIX)->tab_line_p ? ((MATRIX)->rows + 1) : (MATRIX)->rows)

/* Return a pointer to first row in MATRIX used for text display.  */

#define MATRIX_FIRST_TEXT_ROW(MATRIX) \
  ((MATRIX)->rows->mode_line_p ?                                        \
   (((MATRIX)->rows + 1)->mode_line_p ?                                 \
    (MATRIX)->rows + 2 : (MATRIX)->rows + 1) : (MATRIX)->rows)

/* Return a pointer to the first glyph in the text area of a row.
   MATRIX is the glyph matrix accessed, and ROW is the row index in
   MATRIX.  */

#define MATRIX_ROW_GLYPH_START(MATRIX, ROW) \
     (MATRIX_ROW ((MATRIX), (ROW))->glyphs[TEXT_AREA])

/* Return the number of used glyphs in the text area of a row.  */

#define MATRIX_ROW_USED(MATRIX, ROW) \
     (MATRIX_ROW ((MATRIX), (ROW))->used[TEXT_AREA])

/* Return the character/ byte position at which the display of ROW
   starts.  BIDI Note: this is the smallest character/byte position
   among characters in ROW, i.e. the first logical-order character
   displayed by ROW, which is not necessarily the smallest horizontal
   position.  */

#define MATRIX_ROW_START_CHARPOS(ROW) ((ROW)->minpos.charpos)
#define MATRIX_ROW_START_BYTEPOS(ROW) ((ROW)->minpos.bytepos)

/* Return the character/ byte position at which ROW ends.  BIDI Note:
   this is the largest character/byte position among characters in
   ROW, i.e. the last logical-order character displayed by ROW, which
   is not necessarily the largest horizontal position.  */

#define MATRIX_ROW_END_CHARPOS(ROW) ((ROW)->maxpos.charpos)
#define MATRIX_ROW_END_BYTEPOS(ROW) ((ROW)->maxpos.bytepos)

/* Return the vertical position of ROW in MATRIX.  */

#define MATRIX_ROW_VPOS(ROW, MATRIX) ((ROW) - (MATRIX)->rows)

/* Return the last glyph row + 1 in MATRIX on window W reserved for
   text.  If W has a mode line, the last row in the matrix is reserved
   for it.  */

#define MATRIX_BOTTOM_TEXT_ROW(MATRIX, W)		\
     ((MATRIX)->rows					\
      + (MATRIX)->nrows					\
      - (window_wants_mode_line ((W)) ? 1 : 0))

/* Non-zero if the face of the last glyph in ROW's text area has
   to be drawn to the end of the text area.  */

#define MATRIX_ROW_EXTENDS_FACE_P(ROW) ((ROW)->fill_line_p)

/* Set and query the enabled_p flag of glyph row ROW in MATRIX.  */

#define SET_MATRIX_ROW_ENABLED_P(MATRIX, ROW, VALUE) \
     (MATRIX_ROW (MATRIX, ROW)->enabled_p = (VALUE))

#define MATRIX_ROW_ENABLED_P(MATRIX, ROW) \
     (MATRIX_ROW (MATRIX, ROW)->enabled_p)

/* Non-zero if ROW displays text.  Value is non-zero if the row is
   blank but displays a line end.  */

#define MATRIX_ROW_DISPLAYS_TEXT_P(ROW) ((ROW)->displays_text_p)


/* Helper macros */

#define MR_PARTIALLY_VISIBLE(ROW)	\
  ((ROW)->height != (ROW)->visible_height)

#define MR_PARTIALLY_VISIBLE_AT_TOP(W, ROW)  \
  ((ROW)->y < WINDOW_TAB_LINE_HEIGHT (W) + WINDOW_HEADER_LINE_HEIGHT (W))

#define MR_PARTIALLY_VISIBLE_AT_BOTTOM(W, ROW)  \
  (((ROW)->y + (ROW)->height - (ROW)->extra_line_spacing) \
   > WINDOW_BOX_HEIGHT_NO_MODE_LINE ((W)))

/* Non-zero if ROW is not completely visible in window W.  */

#define MATRIX_ROW_PARTIALLY_VISIBLE_P(W, ROW)		\
  (MR_PARTIALLY_VISIBLE ((ROW))				\
   && (MR_PARTIALLY_VISIBLE_AT_TOP ((W), (ROW))		\
       || MR_PARTIALLY_VISIBLE_AT_BOTTOM ((W), (ROW))))



/* Non-zero if ROW is partially visible at the top of window W.  */

#define MATRIX_ROW_PARTIALLY_VISIBLE_AT_TOP_P(W, ROW)		\
  (MR_PARTIALLY_VISIBLE ((ROW))					\
   && MR_PARTIALLY_VISIBLE_AT_TOP ((W), (ROW)))

/* Non-zero if ROW is partially visible at the bottom of window W.  */

#define MATRIX_ROW_PARTIALLY_VISIBLE_AT_BOTTOM_P(W, ROW)	\
  (MR_PARTIALLY_VISIBLE ((ROW))					\
   && MR_PARTIALLY_VISIBLE_AT_BOTTOM ((W), (ROW)))

/* Return the bottom Y + 1 of ROW.   */

#define MATRIX_ROW_BOTTOM_Y(ROW) ((ROW)->y + (ROW)->height)

/* Is ROW the last visible one in the display described by the
   iterator structure pointed to by IT?.  */

#define MATRIX_ROW_LAST_VISIBLE_P(ROW, IT) \
     (MATRIX_ROW_BOTTOM_Y ((ROW)) >= (IT)->last_visible_y)

/* Non-zero if ROW displays a continuation line.  */

#define MATRIX_ROW_CONTINUATION_LINE_P(ROW) \
     ((ROW)->continuation_lines_width > 0)

/* Non-zero if ROW ends in the middle of a character.  This is the
   case for continued lines showing only part of a display table entry
   or a control char, or an overlay string.  */

#define MATRIX_ROW_ENDS_IN_MIDDLE_OF_CHAR_P(ROW)	\
     ((ROW)->end.dpvec_index > 0			\
      || (ROW)->end.overlay_string_index >= 0		\
      || (ROW)->ends_in_middle_of_char_p)

/* Non-zero if ROW ends in the middle of an overlay string.  */

#define MATRIX_ROW_ENDS_IN_OVERLAY_STRING_P(ROW) \
     ((ROW)->end.overlay_string_index >= 0)

/* Non-zero if ROW starts in the middle of a character.  See above.  */

#define MATRIX_ROW_STARTS_IN_MIDDLE_OF_CHAR_P(ROW)	\
     ((ROW)->start.dpvec_index > 0			\
      || (ROW)->starts_in_middle_of_char_p		\
      || ((ROW)->start.overlay_string_index >= 0	\
	  && (ROW)->start.string_pos.charpos > 0))

/* True means ROW overlaps its predecessor.  */

#define MATRIX_ROW_OVERLAPS_PRED_P(ROW) \
     ((ROW)->phys_ascent > (ROW)->ascent)

/* True means ROW overlaps its successor.  */

#define MATRIX_ROW_OVERLAPS_SUCC_P(ROW)		\
      ((ROW)->phys_height - (ROW)->phys_ascent	\
       > (ROW)->height - (ROW)->ascent)

/* A glyph for a space.  */

extern struct glyph space_glyph;

/* True means last display completed.  False means it was preempted.  */

extern bool display_completed;



/************************************************************************
			  Glyph Strings
 ************************************************************************/

/* Enumeration for overriding/changing the face to use for drawing
   glyphs in draw_glyphs.  */

enum draw_glyphs_face
{
  DRAW_NORMAL_TEXT,
  DRAW_INVERSE_VIDEO,
  DRAW_CURSOR,
  DRAW_MOUSE_FACE,
  DRAW_IMAGE_RAISED,
  DRAW_IMAGE_SUNKEN
};

#ifdef HAVE_WINDOW_SYSTEM

/* A sequence of glyphs to be drawn in the same face.  */

struct glyph_string
{
  /* X-origin of the string.  */
  int x;

  /* Y-origin and y-position of the base line of this string.  */
  int y, ybase;

  /* The width of the string, not including a face extension.  */
  int width;

  /* The width of the string, including a face extension.  */
  int background_width;

  /* The height of this string.  This is the height of the line this
     string is drawn in, and can be different from the height of the
     font the string is drawn in.  */
  int height;

  /* Number of pixels this string overwrites in front of its x-origin.
     This number is zero if the string has an lbearing >= 0; it is
     -lbearing, if the string has an lbearing < 0.  */
  int left_overhang;

  /* Number of pixels this string overwrites past its right-most
     nominal x-position, i.e. x + width.  Zero if the string's
     rbearing is <= its nominal width, rbearing - width otherwise.  */
  int right_overhang;

  /* The frame on which the glyph string is drawn.  */
  struct frame *f;

  /* The window on which the glyph string is drawn.  */
  struct window *w;

  /* The glyph row for which this string was built.  It determines the
     y-origin and height of the string.  */
  struct glyph_row *row;

  /* The area within row.  */
  enum glyph_row_area area;

  /* Characters to be drawn, and number of characters.  */
  unsigned *char2b;
  int nchars;

  /* A face-override for drawing cursors, mouse face and similar.  */
  enum draw_glyphs_face hl;

  /* Face in which this string is to be drawn.  */
  struct face *face;

  /* Font in which this string is to be drawn.  */
  struct font *font;

  /* Non-null means this string describes (part of) a static
     composition.  */
  struct composition *cmp;

  /* If not negative, this string describes a compos.  */
  ptrdiff_t cmp_id;

  /* Start and end glyph indices in a glyph-string.  */
  int cmp_from, cmp_to;

  /* True means this glyph strings face has to be drawn to the right end
     of the window's drawing area.  */
  bool_bf extends_to_end_of_line_p : 1;

  /* True means the background of this string has been drawn.  */
  bool_bf background_filled_p : 1;

  /* True means that the original font determined for drawing this glyph
     string could not be loaded.  The member `font' has been set to
     the frame's default font in this case.  */
  bool_bf font_not_found_p : 1;

  /* True means that the face in which this glyph string is drawn has a
     stipple pattern.  */
  bool_bf stippled_p : 1;

#define OVERLAPS_PRED		(1 << 0)
#define OVERLAPS_SUCC		(1 << 1)
#define OVERLAPS_BOTH		(OVERLAPS_PRED | OVERLAPS_SUCC)
#define OVERLAPS_ERASED_CURSOR 	(1 << 2)
  /* Non-zero means only the foreground of this glyph string must be
     drawn, and we should use the physical height of the line this
     glyph string appears in as clip rect.  If the value is
     OVERLAPS_ERASED_CURSOR, the clip rect is restricted to the rect
     of the erased cursor.  OVERLAPS_PRED and OVERLAPS_SUCC mean we
     draw overlaps with the preceding and the succeeding rows,
     respectively.  */
  unsigned for_overlaps : 3;

  /* True means that all glyphs in this glyph string has the flag
     padding_p set, and thus must be drawn one by one to have 1-pixel
     width even though the logical width in the font is zero.  */
  bool_bf padding_p : 1;

  /* The GC to use for drawing this glyph string.  */
#if defined (HAVE_X_WINDOWS)
  GC gc;
#endif
#if defined (HAVE_NTGUI)
  Emacs_GC *gc;
  HDC hdc;
#endif

  /* A pointer to the first glyph in the string.  This glyph
     corresponds to char2b[0].  Needed to draw rectangles if
     font_not_found_p is true.  */
  struct glyph *first_glyph;

  /* Image, if any.  */
  struct image *img;

  /* Xwidget.  */
  struct xwidget *xwidget;

  /* Slice */
  struct glyph_slice slice;

  /* Non-null means the horizontal clipping region starts from the
     left edge of *clip_head, and ends with the right edge of
     *clip_tail, not including their overhangs.  */
  struct glyph_string *clip_head, *clip_tail;

  /* The current clipping areas.  */
  NativeRectangle clip[2];

  /* Number of clipping areas. */
  int num_clips;

  int underline_position;

  int underline_thickness;

  struct glyph_string *next, *prev;
};

#endif /* HAVE_WINDOW_SYSTEM */


/************************************************************************
			  Display Dimensions
 ************************************************************************/

/* Return the height of the mode line in glyph matrix MATRIX, or zero
   if not known.  This macro is called under circumstances where
   MATRIX might not have been allocated yet.  */

#define MATRIX_MODE_LINE_HEIGHT(MATRIX)		\
     ((MATRIX) && (MATRIX)->rows		\
      ? MATRIX_MODE_LINE_ROW (MATRIX)->height	\
      : 0)

/* Return the height of the header line in glyph matrix MATRIX, or zero
   if not known.  This macro is called under circumstances where
   MATRIX might not have been allocated yet.  */

#define MATRIX_HEADER_LINE_HEIGHT(MATRIX)	\
     ((MATRIX) && (MATRIX)->rows		\
      ? MATRIX_HEADER_LINE_ROW (MATRIX)->height	\
      : 0)

/* Return the height of the tab line in glyph matrix MATRIX, or zero
   if not known.  This macro is called under circumstances where
   MATRIX might not have been allocated yet.  */

#define MATRIX_TAB_LINE_HEIGHT(MATRIX)	\
     ((MATRIX) && (MATRIX)->rows		\
      ? MATRIX_TAB_LINE_ROW (MATRIX)->height	\
      : 0)

/* Return the desired face id for the mode line of a window, depending
   on whether the window is selected or not, or if the window is the
   scrolling window for the currently active minibuffer window.

   Due to the way display_mode_lines manipulates with the contents of
   selected_window, this macro needs three arguments: SELW which is
   compared against the current value of selected_window, MBW which is
   compared against minibuf_window (if SELW doesn't match), and SCRW
   which is compared against minibuf_selected_window (if MBW matches).  */

#define CURRENT_MODE_LINE_FACE_ID_3(SELW, MBW, SCRW)		\
     ((!mode_line_in_non_selected_windows			\
       || (SELW) == XWINDOW (selected_window)			\
       || (minibuf_level > 0					\
           && !NILP (minibuf_selected_window)			\
           && (MBW) == XWINDOW (minibuf_window)			\
           && (SCRW) == XWINDOW (minibuf_selected_window)))	\
      ? MODE_LINE_FACE_ID					\
      : MODE_LINE_INACTIVE_FACE_ID)


/* Return the desired face id for the mode line of window W.  */

#define CURRENT_MODE_LINE_FACE_ID(W)		\
	(CURRENT_MODE_LINE_FACE_ID_3((W), XWINDOW (selected_window), (W)))

/* Return the current height of the mode line of window W.  If not known
   from W->mode_line_height, look at W's current glyph matrix, or return
   a default based on the height of the font of the face `mode-line'.  */

#define CURRENT_MODE_LINE_HEIGHT(W)					\
  ((W)->mode_line_height >= 0						\
   ? (W)->mode_line_height						\
   : ((W)->mode_line_height						\
      = (MATRIX_MODE_LINE_HEIGHT ((W)->current_matrix)			\
	 ? MATRIX_MODE_LINE_HEIGHT ((W)->current_matrix)		\
	 : estimate_mode_line_height					\
	     (XFRAME ((W)->frame), CURRENT_MODE_LINE_FACE_ID (W)))))

/* Return the current height of the header line of window W.  If not known
   from W->header_line_height, look at W's current glyph matrix, or return
   an estimation based on the height of the font of the face `header-line'.  */

#define CURRENT_HEADER_LINE_HEIGHT(W)				\
  ((W)->header_line_height >= 0					\
   ? (W)->header_line_height					\
   : ((W)->header_line_height					\
      = (MATRIX_HEADER_LINE_HEIGHT ((W)->current_matrix)	\
	 ? MATRIX_HEADER_LINE_HEIGHT ((W)->current_matrix)	\
	 : estimate_mode_line_height				\
	     (XFRAME ((W)->frame), HEADER_LINE_FACE_ID))))

/* Return the current height of the tab line of window W.  If not known
   from W->tab_line_height, look at W's current glyph matrix, or return
   an estimation based on the height of the font of the face `tab-line'.  */

#define CURRENT_TAB_LINE_HEIGHT(W)				\
  ((W)->tab_line_height >= 0					\
   ? (W)->tab_line_height					\
   : ((W)->tab_line_height					\
      = (MATRIX_TAB_LINE_HEIGHT ((W)->current_matrix)		\
	 ? MATRIX_TAB_LINE_HEIGHT ((W)->current_matrix)		\
	 : estimate_mode_line_height				\
	     (XFRAME ((W)->frame), TAB_LINE_FACE_ID))))

/* Return the height of the desired mode line of window W.  */

#define DESIRED_MODE_LINE_HEIGHT(W) \
     MATRIX_MODE_LINE_HEIGHT ((W)->desired_matrix)

/* Return the height of the desired header line of window W.  */

#define DESIRED_HEADER_LINE_HEIGHT(W) \
     MATRIX_HEADER_LINE_HEIGHT ((W)->desired_matrix)

/* Return the height of the desired tab line of window W.  */

#define DESIRED_TAB_LINE_HEIGHT(W) \
     MATRIX_TAB_LINE_HEIGHT ((W)->desired_matrix)

/* Return proper value to be used as baseline offset of font that has
   ASCENT and DESCENT to draw characters by the font at the vertical
   center of the line of frame F.

   Here, our task is to find the value of BOFF in the following figure;

	-------------------------+-----------+-
	 -+-+---------+-+        |           |
	  | |         | |        |           |
	  | |         | |        F_ASCENT    F_HEIGHT
	  | |         | ASCENT   |           |
     HEIGHT |         | |        |           |
	  | |         |-|-+------+-----------|------- baseline
	  | |         | | BOFF   |           |
	  | |---------|-+-+      |           |
	  | |         | DESCENT  |           |
	 -+-+---------+-+        F_DESCENT   |
	-------------------------+-----------+-

	-BOFF + DESCENT + (F_HEIGHT - HEIGHT) / 2 = F_DESCENT
	BOFF = DESCENT +  (F_HEIGHT - HEIGHT) / 2 - F_DESCENT
	DESCENT = FONT->descent
	HEIGHT = FONT_HEIGHT (FONT)
	F_DESCENT = (FRAME_FONT (F)->descent
		     - F->terminal->output_data.x->baseline_offset)
	F_HEIGHT = FRAME_LINE_HEIGHT (F)
*/

#define VCENTER_BASELINE_OFFSET(FONT, F)			\
  (FONT_DESCENT (FONT)						\
   + (FRAME_LINE_HEIGHT ((F)) - FONT_HEIGHT ((FONT))		\
      + (FRAME_LINE_HEIGHT ((F)) > FONT_HEIGHT ((FONT)))) / 2	\
   - (FONT_DESCENT (FRAME_FONT (F)) - FRAME_BASELINE_OFFSET (F)))

/* A heuristic test for fonts that claim they need a preposterously
   large vertical space.  The heuristics is in the factor of 3.  We
   ignore the ascent and descent values reported by such fonts, and
   instead go by the values reported for individual glyphs.  */
#define FONT_TOO_HIGH(ft)						\
  ((ft)->pixel_size > 0 && (ft)->ascent + (ft)->descent > 3*(ft)->pixel_size)


/***********************************************************************
				Faces
 ***********************************************************************/

/* Indices of face attributes in Lisp face vectors.  Slot zero is the
   symbol `face'.  */

enum lface_attribute_index
{
  LFACE_FAMILY_INDEX = 1,
  LFACE_FOUNDRY_INDEX,
  LFACE_SWIDTH_INDEX,
  LFACE_HEIGHT_INDEX,
  LFACE_WEIGHT_INDEX,
  LFACE_SLANT_INDEX,
  LFACE_UNDERLINE_INDEX,
  LFACE_INVERSE_INDEX,
  LFACE_FOREGROUND_INDEX,
  LFACE_BACKGROUND_INDEX,
  LFACE_STIPPLE_INDEX,
  LFACE_OVERLINE_INDEX,
  LFACE_STRIKE_THROUGH_INDEX,
  LFACE_BOX_INDEX,
  LFACE_FONT_INDEX,
  LFACE_INHERIT_INDEX,
  LFACE_FONTSET_INDEX,
  LFACE_DISTANT_FOREGROUND_INDEX,
  LFACE_EXTEND_INDEX,
  LFACE_VECTOR_SIZE
};


/* Box types of faces.  */

enum face_box_type
{
  /* No box around text.  */
  FACE_NO_BOX,

  /* Simple box of specified width and color.  Default width is 1, and
     default color is the foreground color of the face.  */
  FACE_SIMPLE_BOX,

  /* Boxes with 3D shadows.  Color equals the background color of the
     face.  Width is specified.  */
  FACE_RAISED_BOX,
  FACE_SUNKEN_BOX
};

/* Underline type. */

enum face_underline_type
{
  FACE_NO_UNDERLINE = 0,
  FACE_UNDER_LINE,
  FACE_UNDER_WAVE
};

/* Structure describing a realized face.

   For each Lisp face, 0..N realized faces can exist for different
   frames and different charsets.  Realized faces are built from Lisp
   faces and text properties/overlays by merging faces and adding
   unspecified attributes from the `default' face.  */

struct face
{
  /* The Lisp face attributes this face realizes.  All attributes
     in this vector are non-nil.  */
  Lisp_Object lface[LFACE_VECTOR_SIZE];

  /* The id of this face.  The id equals the index of this face in the
     vector faces_by_id of its face cache.  */
  int id;

#ifdef HAVE_WINDOW_SYSTEM

  /* If non-zero, this is a GC that we can use without modification for
     drawing the characters in this face.  */
# ifdef HAVE_X_WINDOWS
  GC gc;
# else
  Emacs_GC *gc;
# endif
  /* Background stipple or bitmap used for this face.  This is
     an id as returned from load_pixmap.  */
  ptrdiff_t stipple;

#endif /* not HAVE_WINDOW_SYSTEM */

  /* Pixel value of foreground color for X frames.  Color index
     for tty frames.  */
  unsigned long foreground;

  /* Pixel value or color index of background color.  */
  unsigned long background;

  /* Pixel value or color index of underline, overlined,
     strike-through, or box color.  */
  unsigned long underline_color;
  unsigned long overline_color;
  unsigned long strike_through_color;
  unsigned long box_color;

  struct font *font;

  /* Fontset ID if for this face's fontset.  Non-ASCII faces derived
     from the same ASCII face have the same fontset.  */
  int fontset;

  /* Non-zero means characters in this face have a box of that
     thickness around them.  If this value is negative, its absolute
     value indicates the thickness, and the horizontal (top and
     bottom) borders of box are drawn inside of the character glyphs'
     area.  The vertical (left and right) borders of the box are drawn
     in the same way as when this value is positive.  */
  int box_line_width;

  /* Type of box drawn.  A value of FACE_NO_BOX means no box is drawn
     around text in this face.  A value of FACE_SIMPLE_BOX means a box
     of width box_line_width is drawn in color box_color.  A value of
     FACE_RAISED_BOX or FACE_SUNKEN_BOX means a 3D box is drawn with
     shadow colors derived from the background color of the face.  */
  ENUM_BF (face_box_type) box : 2;

  /* Style of underlining. */
  ENUM_BF (face_underline_type) underline : 2;

  /* If `box' above specifies a 3D type, true means use box_color for
     drawing shadows.  */
  bool_bf use_box_color_for_shadows_p : 1;

  /* Non-zero if text in this face should be underlined, overlined,
     strike-through or have a box drawn around it.  */
  bool_bf overline_p : 1;
  bool_bf strike_through_p : 1;

  /* True means that the colors specified for this face could not be
     loaded, and were replaced by default colors, so they shouldn't be
     freed.  */
  bool_bf foreground_defaulted_p : 1;
  bool_bf background_defaulted_p : 1;

  /* True means that either no color is specified for the corresponding
     attribute or that the specified color couldn't be loaded.
     Use the foreground color when drawing in that case. */
  bool_bf underline_defaulted_p : 1;
  bool_bf overline_color_defaulted_p : 1;
  bool_bf strike_through_color_defaulted_p : 1;
  bool_bf box_color_defaulted_p : 1;

  /* TTY appearances.  Colors are found in `lface' with empty color
     string meaning the default color of the TTY.  */
  bool_bf tty_bold_p : 1;
  bool_bf tty_italic_p : 1;
  bool_bf tty_underline_p : 1;
  bool_bf tty_reverse_p : 1;

  /* True means that colors of this face may not be freed because they
     have been copied bitwise from a base face (see
     realize_gui_face).  */
  bool_bf colors_copied_bitwise_p : 1;

  /* If non-zero, use overstrike (to simulate bold-face).  */
  bool_bf overstrike : 1;

/* NOTE: this is not used yet, but eventually this impl should be done
         similarly to overstrike */
#ifdef HAVE_NS
  /* If non-zero, use geometric rotation (to simulate italic).  */
  bool_bf synth_ital : 1;
#endif

  /* The hash value of this face.  */
  uintptr_t hash;

  /* Next and previous face in hash collision list of face cache.  */
  struct face *next, *prev;

  /* If this face is an ASCII face, this points to this face itself.
     Otherwise, this points to an ASCII face that has the same
     attributes except the font.  */
  struct face *ascii_face;

#if defined HAVE_XFT || defined HAVE_FREETYPE
/* Extra member that a font-driver uses privately.  */
  void *extra;
#endif
};


/* Color index indicating that face uses a terminal's default color.  */

#define FACE_TTY_DEFAULT_COLOR ((unsigned long) -1)

/* Color index indicating that face uses an unknown foreground color.  */

#define FACE_TTY_DEFAULT_FG_COLOR ((unsigned long) -2)

/* Color index indicating that face uses an unknown background color.  */

#define FACE_TTY_DEFAULT_BG_COLOR ((unsigned long) -3)

/* True if COLOR is a specified (i.e., nondefault) foreground or
   background color for a tty face.  */

INLINE bool
face_tty_specified_color (unsigned long color)
{
  return color < FACE_TTY_DEFAULT_BG_COLOR;
}

/* Non-zero if FACE was realized for unibyte use.  */

#define FACE_UNIBYTE_P(FACE) ((FACE)->charset < 0)


/* IDs of important faces known by the C face code.  These are the IDs
   of the faces for CHARSET_ASCII.  */

enum face_id
{
  DEFAULT_FACE_ID,
  MODE_LINE_FACE_ID,
  MODE_LINE_INACTIVE_FACE_ID,
  TOOL_BAR_FACE_ID,
  FRINGE_FACE_ID,
  HEADER_LINE_FACE_ID,
  SCROLL_BAR_FACE_ID,
  BORDER_FACE_ID,
  CURSOR_FACE_ID,
  MOUSE_FACE_ID,
  MENU_FACE_ID,
  VERTICAL_BORDER_FACE_ID,
  WINDOW_DIVIDER_FACE_ID,
  WINDOW_DIVIDER_FIRST_PIXEL_FACE_ID,
  WINDOW_DIVIDER_LAST_PIXEL_FACE_ID,
  INTERNAL_BORDER_FACE_ID,
  TAB_BAR_FACE_ID,
  TAB_LINE_FACE_ID,
  BASIC_FACE_ID_SENTINEL
};

#define MAX_FACE_ID  ((1 << FACE_ID_BITS) - 1)

/* A cache of realized faces.  Each frame has its own cache because
   Emacs allows different frame-local face definitions.  */

struct face_cache
{
  /* Hash table of cached realized faces.  */
  struct face **buckets;

  /* Back-pointer to the frame this cache belongs to.  */
  struct frame *f;

  /* A vector of faces so that faces can be referenced by an ID.  */
  struct face **faces_by_id;

  /* The allocated size, and number of used slots of faces_by_id.  */
  ptrdiff_t size;
  int used;

  /* Flag indicating that attributes of the `menu' face have been
     changed.  */
  bool_bf menu_face_changed_p : 1;
};

/* Return a non-null pointer to the cached face with ID on frame F.  */

#define FACE_FROM_ID(F, ID)					\
  (eassert (UNSIGNED_CMP (ID, <, FRAME_FACE_CACHE (F)->used)),	\
   FRAME_FACE_CACHE (F)->faces_by_id[ID])

/* Return a pointer to the face with ID on frame F, or null if such a
   face doesn't exist.  */

#define FACE_FROM_ID_OR_NULL(F, ID)			\
  (UNSIGNED_CMP (ID, <, FRAME_FACE_CACHE (F)->used)	\
   ? FRAME_FACE_CACHE (F)->faces_by_id[ID]		\
   : NULL)

#define FACE_EXTENSIBLE_P(F)			\
  (!NILP (F->lface[LFACE_EXTEND_INDEX]))

/* True if FACE is suitable for displaying ASCII characters.  */
INLINE bool
FACE_SUITABLE_FOR_ASCII_CHAR_P (struct face *face)
{
#ifdef HAVE_WINDOW_SYSTEM
  return face == face->ascii_face;
#else
  return true;
#endif
}

/* Return the id of the realized face on frame F that is like the face
   FACE, but is suitable for displaying character CHARACTER at buffer or
   string position POS.  OBJECT is the string object, or nil for
   buffer.  This macro is only meaningful for multibyte character
   CHAR.  */
INLINE int
FACE_FOR_CHAR (struct frame *f, struct face *face, int character,
	       ptrdiff_t pos, Lisp_Object object)
{
#ifdef HAVE_WINDOW_SYSTEM
  return face_for_char (f, face, character, pos, object);
#else
  return face->id;
#endif
}

/* Return true if G contains a valid character code.  */
INLINE bool
GLYPH_CHAR_VALID_P (GLYPH g)
{
  return CHAR_VALID_P (GLYPH_CHAR (g));
}

/* The glyph code from a display vector may either be an integer which
   encodes a char code in the lower CHARACTERBITS bits and a (very small)
   face-id in the upper bits, or it may be a cons (CHAR . FACE-ID).  */

INLINE bool
GLYPH_CODE_P (Lisp_Object gc)
{
  return (CONSP (gc)
	  ? (CHARACTERP (XCAR (gc))
	     && RANGED_FIXNUMP (0, XCDR (gc), MAX_FACE_ID))
	  : (RANGED_FIXNUMP
	     (0, gc,
	      (MAX_FACE_ID < TYPE_MAXIMUM (EMACS_INT) >> CHARACTERBITS
	       ? ((EMACS_INT) MAX_FACE_ID << CHARACTERBITS) | MAX_CHAR
	       : TYPE_MAXIMUM (EMACS_INT)))));
}

/* True means face attributes have been changed since the last
   redisplay.  Used in redisplay_internal.  */

extern bool face_change;

/* For reordering of bidirectional text.  */

/* UAX#9's max_depth value.  */
#define BIDI_MAXDEPTH 125

/* Data type for describing the bidirectional character types.  The
   first 7 must be at the beginning, because they are the only values
   valid in the `bidi_type' member of `struct glyph'; we only reserve
   3 bits for it, so we cannot use there values larger than 7.

   The order of members must be in sync with the 8th element of the
   member of unidata-prop-alist (in admin/unidata/unidata-gen.el) for
   Unicode character property `bidi-class'.  */
typedef enum {
  UNKNOWN_BT = 0,
  STRONG_L,	/* strong left-to-right */
  STRONG_R,	/* strong right-to-left */
  WEAK_EN,	/* european number */
  WEAK_AN,	/* arabic number */
  WEAK_BN,	/* boundary neutral */
  NEUTRAL_B,	/* paragraph separator */
  STRONG_AL,	/* arabic right-to-left letter */
  LRE,		/* left-to-right embedding */
  LRO,		/* left-to-right override */
  RLE,		/* right-to-left embedding */
  RLO,		/* right-to-left override */
  PDF,		/* pop directional format */
  LRI,		/* left-to-right isolate */
  RLI,		/* right-to-left isolate */
  FSI,		/* first strong isolate */
  PDI,		/* pop directional isolate */
  WEAK_ES,	/* european number separator */
  WEAK_ET,	/* european number terminator */
  WEAK_CS,	/* common separator */
  WEAK_NSM,	/* non-spacing mark */
  NEUTRAL_S,	/* segment separator */
  NEUTRAL_WS,	/* whitespace */
  NEUTRAL_ON	/* other neutrals */
} bidi_type_t;

/* Data type for describing the Bidi Paired Bracket Type of a character.

   The order of members must be in sync with the 8th element of the
   member of unidata-prop-alist (in admin/unidata/unidata-gen.el) for
   Unicode character property `bracket-type'.  */
typedef enum {
  BIDI_BRACKET_NONE = 1,
  BIDI_BRACKET_OPEN,
  BIDI_BRACKET_CLOSE
} bidi_bracket_type_t;

/* The basic directionality data type.  */
typedef enum { NEUTRAL_DIR = 0, L2R, R2L } bidi_dir_t;

/* Data type for storing information about characters we need to
   remember.  */
struct bidi_saved_info {
  ptrdiff_t charpos;		/* character's buffer position */
  bidi_type_t type;		/* character's resolved bidi type */
  bidi_type_t orig_type;	/* bidi type as we found it in the buffer */
};

/* Data type for keeping track of information about saved embedding
   levels, override status, isolate status, and isolating sequence
   runs.  This should be as tightly packed as possible, because there
   are 127 such entries in each iterator state, and so the size of
   cache is directly affected by the size of this struct.  */
struct bidi_stack {
  ptrdiff_t next_for_neutral_pos;
  unsigned next_for_neutral_type : 3;
  unsigned last_strong_type : 3;
  unsigned prev_for_neutral_type : 3;
  unsigned char level;
  unsigned char flags;		/* sos, override, isolate_status */
};

/* Data type for storing information about a string being iterated on.  */
struct bidi_string_data {
  Lisp_Object lstring;		/* Lisp string to reorder, or nil */
  const unsigned char *s;	/* string data, or NULL if reordering buffer */
  ptrdiff_t schars;		/* the number of characters in the string,
				   excluding the terminating NUL */
  ptrdiff_t bufpos;		/* buffer position of lstring, or 0 if N/A */
  bool_bf from_disp_str : 1;	/* True means the string comes from a
				   display property */
  bool_bf unibyte : 1;		/* True means the string is unibyte */
};

/* Data type for reordering bidirectional text.  */
struct bidi_it {
  ptrdiff_t bytepos;		/* iterator's position in buffer/string */
  ptrdiff_t charpos;
  int ch;			/* character at that position, or u+FFFC
				   ("object replacement character") for a run
				   of characters covered by a display string */
  ptrdiff_t nchars;		/* its "length", usually 1; it's > 1 for a run
				   of characters covered by a display string */
  ptrdiff_t ch_len;		/* its length in bytes */
  bidi_type_t type;		/* final bidi type of this character, after
				   resolving weak and neutral types */
  bidi_type_t type_after_wn;	/* bidi type after overrides and Wn */
  bidi_type_t orig_type;	/* original bidi type, as found in the buffer */
  signed char resolved_level;	/* final resolved level of this character */
  signed char isolate_level;	/* count of isolate initiators unmatched by PDI */
  ptrdiff_t invalid_levels;	/* how many PDFs to ignore */
  ptrdiff_t invalid_isolates;	/* how many PDIs to ignore */
  struct bidi_saved_info prev;	/* info about previous character */
  struct bidi_saved_info last_strong; /* last-seen strong directional char */
  struct bidi_saved_info next_for_neutral; /* surrounding characters for... */
  struct bidi_saved_info prev_for_neutral; /* ...resolving neutrals */
  struct bidi_saved_info next_for_ws; /* character after sequence of ws */
  ptrdiff_t bracket_pairing_pos;	/* position of pairing bracket */
  bidi_type_t bracket_enclosed_type;	/* type for bracket resolution */
  ptrdiff_t next_en_pos;	/* pos. of next char for determining ET type */
  bidi_type_t next_en_type;	/* type of char at next_en_pos */
  bidi_dir_t sos;		/* direction of start-of-sequence in effect */
  int scan_dir;			/* direction of text scan, 1: forw, -1: back */
  ptrdiff_t disp_pos;		/* position of display string after ch */
  int disp_prop;		/* if non-zero, there really is a
				   `display' property/string at disp_pos;
				   if 2, the property is a `space' spec */
  int stack_idx;		/* index of current data on the stack */
  /* Note: Everything from here on is not copied/saved when the bidi
     iterator state is saved, pushed, or popped.  So only put here
     stuff that is not part of the bidi iterator's state!  */
  struct bidi_stack level_stack[BIDI_MAXDEPTH+2+1]; /* directional status stack */
  struct bidi_string_data string;	/* string to reorder */
  struct window *w;		/* the window being displayed */
  bidi_dir_t paragraph_dir;	/* current paragraph direction */
  ptrdiff_t separator_limit;	/* where paragraph separator should end */
  bool_bf first_elt : 1;	/* if true, examine current char first */
  bool_bf new_paragraph : 1;	/* if true, we expect a new paragraph */
  bool_bf frame_window_p : 1;	/* true if displaying on a GUI frame */
};

/* Value is non-zero when the bidi iterator is at base paragraph
   embedding level.  */
#define BIDI_AT_BASE_LEVEL(BIDI_IT) \
  ((BIDI_IT).resolved_level == (BIDI_IT).level_stack[0].level)


/***********************************************************************
			       Fringes
 ***********************************************************************/

/* Structure used to describe where and how to draw a fringe bitmap.
   WHICH is the fringe bitmap to draw.  WD and H is the (adjusted)
   width and height of the bitmap, DH is the height adjustment (if
   bitmap is periodic).  X and Y are frame coordinates of the area to
   display the bitmap, DY is relative offset of the bitmap into that
   area.  BX, NX, BY, NY specifies the area to clear if the bitmap
   does not fill the entire area.  FACE is the fringe face.  */

struct draw_fringe_bitmap_params
{
  int which;  /* enum fringe_bitmap_type */
  unsigned short *bits;
  int wd, h, dh;
  int x, y;
  int bx, nx, by, ny;
  bool_bf cursor_p : 1;
  bool_bf overlay_p : 1;
  struct face *face;
};

#define MAX_FRINGE_BITMAPS (1<<FRINGE_ID_BITS)


/***********************************************************************
			    Display Iterator
 ***********************************************************************/

/* Iteration over things to display in current_buffer or in a string.

   The iterator handles:

   1. Overlay strings (after-string, before-string).
   2. Face properties.
   3. Invisible text properties.
   4. Selective display.
   5. Translation of characters via display tables.
   6. Translation of control characters to the forms `\003' or `^C'.
   7. `glyph' and `space-width' properties.

   Iterators are initialized by calling init_iterator or one of the
   equivalent functions below.  A call to get_next_display_element
   loads the iterator structure with information about what next to
   display.  A call to set_iterator_to_next increments the iterator's
   position.

   Characters from overlay strings, display table entries or control
   character translations are returned one at a time.  For example, if
   we have a text of `a\x01' where `a' has a display table definition
   of `cd' and the control character is displayed with a leading
   arrow, then the iterator will return:

   Call		Return  Source		Call next
   -----------------------------------------------------------------
   next		c	display table	move
   next		d	display table	move
   next		^	control char	move
   next		A	control char	move

   The same mechanism is also used to return characters for ellipses
   displayed at the end of invisible text.

   CAVEAT: Under some circumstances, move_.* functions can be called
   asynchronously, e.g. when computing a buffer position from an x and
   y pixel position.  This means that these functions and functions
   called from them SHOULD NOT USE xmalloc and alike.  See also the
   comment at the start of xdisp.c.  */

/* Enumeration describing what kind of display element an iterator is
   loaded with after a call to get_next_display_element.  */

enum display_element_type
{
  /* A normal character.  */
  IT_CHARACTER,

  /* A composition (static and automatic).  */
  IT_COMPOSITION,

  /* A glyphless character (e.g. ZWNJ, LRE).  */
  IT_GLYPHLESS,

  /* An image.  */
  IT_IMAGE,

  /* A flexible width and height space.  */
  IT_STRETCH,

  /* End of buffer or string.  */
  IT_EOB,

  /* Truncation glyphs.  Never returned by get_next_display_element.
     Used to get display information about truncation glyphs via
     produce_glyphs.  */
  IT_TRUNCATION,

  /* Continuation glyphs.  See the comment for IT_TRUNCATION.  */
  IT_CONTINUATION,

  /* Xwidget.  */
  IT_XWIDGET
};


/* An enumerator for each text property that has a meaning for display
   purposes.  */

enum prop_idx
{
  FONTIFIED_PROP_IDX,
  FACE_PROP_IDX,
  INVISIBLE_PROP_IDX,
  DISPLAY_PROP_IDX,
  COMPOSITION_PROP_IDX,

  /* Not a property.  Used to indicate changes in overlays.  */
  OVERLAY_PROP_IDX,

  /* Sentinel.  */
  LAST_PROP_IDX
};

/* An enumerator for the method of wrapping long lines.  */

enum line_wrap_method
{
  TRUNCATE,
  WORD_WRAP,
  WINDOW_WRAP
};

/* An enumerator for the method of displaying glyphless characters.  */

enum glyphless_display_method
  {
    /* Display a thin (1-pixel width) space.  On a TTY, display a
       1-character width space.  */
    GLYPHLESS_DISPLAY_THIN_SPACE,
    /* Display an empty box of proper width.  */
    GLYPHLESS_DISPLAY_EMPTY_BOX,
    /* Display an acronym string in a box.  */
    GLYPHLESS_DISPLAY_ACRONYM,
    /* Display the hexadecimal code of the character in a box.  */
    GLYPHLESS_DISPLAY_HEX_CODE
  };

struct it_slice
{
  Lisp_Object x;
  Lisp_Object y;
  Lisp_Object width;
  Lisp_Object height;
};

/* Input sources for fetching characters or data to display.
   The input source is found in the `method' field.  */

enum it_method {
  GET_FROM_BUFFER = 0,
  GET_FROM_DISPLAY_VECTOR,
  GET_FROM_STRING,
  GET_FROM_C_STRING,
  GET_FROM_IMAGE,
  GET_FROM_STRETCH,
  GET_FROM_XWIDGET,
  NUM_IT_METHODS
};

/* FIXME: What is this?  Why 5?  */
#define IT_STACK_SIZE 5

/* Iterator for composition (both for static and automatic).  */
struct composition_it
{
  /* Next position at which to check the composition.  */
  ptrdiff_t stop_pos;
  /* ID number of the composition or glyph-string.  If negative, we
     are not iterating over a composition now.  */
  ptrdiff_t id;
  /* If non-negative, character that triggers the automatic
     composition at `stop_pos', and this is an automatic composition.
     If negative, this is a static composition.  This is set to -2
     temporarily if searching of composition reach a limit or a
     newline.  */
  int ch;
  /* If this is an automatic composition, index of a rule for making
     the automatic composition.  Provided that ELT is an element of
     Vcomposition_function_table for CH, (nth ELT RULE_IDX) is the
     rule for the composition.  */
  EMACS_INT rule_idx;
  /* If this is an automatic composition, how many characters to look
     back from the position where a character triggering the
     composition exists.  */
  ptrdiff_t lookback;
  /* If non-negative, number of glyphs of the glyph-string.  */
  int nglyphs;
  /* True iff the composition is created while buffer is scanned in
     reverse order, and thus the grapheme clusters must be rendered
     from the last to the first.  */
  bool reversed_p;

  /** The following members contain information about the current
      grapheme cluster.  */
  /* Position of the first character of the current grapheme cluster.  */
  ptrdiff_t charpos;
  /* Number of characters and bytes of the current grapheme cluster.  */
  int nchars, nbytes;
  /* Indices of the glyphs for the current grapheme cluster.  */
  int from, to;
  /* Width of the current grapheme cluster in units of columns it will
     occupy on display; see CHARACTER_WIDTH.  */
  int width;
};

struct it
{
  /* The window in which we iterate over current_buffer (or a string).  */
  Lisp_Object window;
  struct window *w;

  /* The window's frame.  */
  struct frame *f;

  /* Method to use to load this structure with the next display element.  */
  enum it_method method;

  /* The next position at which to check for face changes, invisible
     text, overlay strings, end of text etc., which see.  */
  ptrdiff_t stop_charpos;

  /* Previous stop position, i.e. the last one before the current
     iterator position in `current'.  */
  ptrdiff_t prev_stop;

  /* Last stop position iterated across whose bidi embedding level is
     equal to the current paragraph's base embedding level.  */
  ptrdiff_t base_level_stop;

  /* Maximum string or buffer position + 1.  ZV when iterating over
     current_buffer.  When iterating over a string in display_string,
     this can be smaller or greater than the number of string
     characters, depending on the values of PRECISION and FIELD_WIDTH
     with which display_string was called.  */
  ptrdiff_t end_charpos;

  /* C string to iterate over.  Non-null means get characters from
     this string, otherwise characters are read from current_buffer
     or it->string.  */
  const unsigned char *s;

  /* Number of characters in the string (s, or it->string) we iterate
     over.  Used only in display_string and its subroutines; never
     used for overlay strings and strings from display properties.  */
  ptrdiff_t string_nchars;

  /* Position at which redisplay end trigger functions should be run.  */
  ptrdiff_t redisplay_end_trigger_charpos;

  /* True means multibyte characters are enabled.  */
  bool_bf multibyte_p : 1;

  /* True means window has a tab line at its top.  */
  bool_bf tab_line_p : 1;

  /* True means window has a mode line at its top.  */
  bool_bf header_line_p : 1;

  /* True means `string' is the value of a `display' property.
     Don't handle some `display' properties in these strings.  */
  bool_bf string_from_display_prop_p : 1;

  /* True means `string' comes from a `line-prefix' or `wrap-prefix'
     property.  */
  bool_bf string_from_prefix_prop_p : 1;

  /* True means we are iterating an object that came from a value of a
     `display' property.  */
  bool_bf from_disp_prop_p : 1;

  /* When METHOD == next_element_from_display_vector,
     this is true if we're doing an ellipsis.  Otherwise meaningless.  */
  bool_bf ellipsis_p : 1;

  /* True means cursor shouldn't be displayed here.  */
  bool_bf avoid_cursor_p : 1;

  /* Display table in effect or null for none.  */
  struct Lisp_Char_Table *dp;

  /* Current display table vector to return characters from and its
     end.  dpvec null means we are not returning characters from a
     display table entry; current.dpvec_index gives the current index
     into dpvec.  This same mechanism is also used to return
     characters from translated control characters, i.e. `\003' or
     `^C'.  */
  Lisp_Object *dpvec, *dpend;

  /* Length in bytes of the char that filled dpvec.  A value of zero
     means that no such character is involved.  A negative value means
     the rest of the line from the current iterator position onwards
     is hidden by selective display or ellipsis.  */
  int dpvec_char_len;

  /* Face id to use for all characters in display vector.  -1 if unused. */
  int dpvec_face_id;

  /* Face id of the iterator saved in case a glyph from dpvec contains
     a face.  The face is restored when all glyphs from dpvec have
     been delivered.  */
  int saved_face_id;

  /* Vector of glyphs for control character translation.  The pointer
     dpvec is set to ctl_chars when a control character is translated.
     This vector is also used for incomplete multibyte character
     translation (e.g \222\244).  Such a character is at most 4 bytes,
     thus we need at most 16 bytes here.  */
  Lisp_Object ctl_chars[16];

  /* Initial buffer or string position of the iterator, before skipping
     over display properties and invisible text.  */
  struct display_pos start;

  /* Current buffer or string position of the iterator, including
     position in overlay strings etc.  */
  struct display_pos current;

  /* Total number of overlay strings to process.  This can be >
     OVERLAY_STRING_CHUNK_SIZE.  Value is dependable only when
     current.overlay_string_index >= 0.  Use the latter to determine
     whether an overlay string is being iterated over, because
     n_overlay_strings can be positive even when we are not rendering
     an overlay string.  */
  ptrdiff_t n_overlay_strings;

  /* The charpos where n_overlay_strings was calculated.  This should
     be set at the same time as n_overlay_strings.  It is needed
     because we show before-strings at the start of invisible text;
     see handle_invisible_prop in xdisp.c.  */
  ptrdiff_t overlay_strings_charpos;

  /* Vector of overlays to process.  Overlay strings are processed
     OVERLAY_STRING_CHUNK_SIZE at a time.  */
#define OVERLAY_STRING_CHUNK_SIZE 16
  Lisp_Object overlay_strings[OVERLAY_STRING_CHUNK_SIZE];

  /* For each overlay string, the overlay it came from.  */
  Lisp_Object string_overlays[OVERLAY_STRING_CHUNK_SIZE];

  /* If non-nil, a Lisp string being processed.  If
     current.overlay_string_index >= 0, this is an overlay string from
     pos.  Use STRINGP (it.string) to test whether we are rendering a
     string or something else; do NOT use BUFFERP (it.object).  */
  Lisp_Object string;

  /* If non-nil, we are processing a string that came
     from a `display' property given by an overlay.  */
  Lisp_Object from_overlay;

  /* Stack of saved values.  New entries are pushed when we begin to
     process an overlay string or a string from a `glyph' property.
     Entries are popped when we return to deliver display elements
     from what we previously had.  */
  struct iterator_stack_entry
  {
    Lisp_Object string;
    int string_nchars;
    ptrdiff_t end_charpos;
    ptrdiff_t stop_charpos;
    ptrdiff_t prev_stop;
    ptrdiff_t base_level_stop;
    struct composition_it cmp_it;
    int face_id;

    /* Save values specific to a given method.  */
    union {
      /* method == GET_FROM_IMAGE */
      struct {
	Lisp_Object object;
	struct it_slice slice;
	ptrdiff_t image_id;
      } image;
      /* method == GET_FROM_STRETCH */
      struct {
	Lisp_Object object;
      } stretch;
      /* method == GET_FROM_XWIDGET */
      struct {
	Lisp_Object object;
      } xwidget;
    } u;

    /* Current text and display positions.  */
    struct text_pos position;
    struct display_pos current;
    Lisp_Object from_overlay;
    enum glyph_row_area area;
    enum it_method method;
    bidi_dir_t paragraph_embedding;
    bool_bf multibyte_p : 1;
    bool_bf string_from_display_prop_p : 1;
    bool_bf string_from_prefix_prop_p : 1;
    bool_bf display_ellipsis_p : 1;
    bool_bf avoid_cursor_p : 1;
    bool_bf bidi_p : 1;
    bool_bf from_disp_prop_p : 1;
    enum line_wrap_method line_wrap;

    /* Properties from display property that are reset by another display
       property.  */
    short voffset;
    Lisp_Object space_width;
    Lisp_Object font_height;
  }
  stack[IT_STACK_SIZE];

  /* Stack pointer.  */
  int sp;

  /* -1 means selective display hides everything between a \r and the
     next newline; > 0 means hide lines indented more than that value.  */
  ptrdiff_t selective;

  /* An enumeration describing what the next display element is
     after a call to get_next_display_element.  */
  enum display_element_type what;

  /* Face to use.  */
  int face_id;

  /* Setting of buffer-local variable selective-display-ellipses.  */
  bool_bf selective_display_ellipsis_p : 1;

  /* True means control characters are translated into the form `^C'
     where the `^' can be replaced by a display table entry.  */
  bool_bf ctl_arrow_p : 1;

  /* True means that the current face has a box.  */
  bool_bf face_box_p : 1;

  /* Non-null means that the current character is the first in a run
     of characters with box face.  */
  bool_bf start_of_box_run_p : 1;

  /* True means that the current character is the last in a run
     of characters with box face.  */
  bool_bf end_of_box_run_p : 1;

  /* True means overlay strings at end_charpos have been processed.  */
  bool_bf overlay_strings_at_end_processed_p : 1;

  /* True means to ignore overlay strings at current pos, as they have
     already been processed.  */
  bool_bf ignore_overlay_strings_at_pos_p : 1;

  /* True means the actual glyph is not available in the current
     system.  */
  bool_bf glyph_not_available_p : 1;

  /* True means the next line in display_line continues a character
     consisting of more than one glyph, and some glyphs of this
     character have been put on the previous line.  */
  bool_bf starts_in_middle_of_char_p : 1;

  /* If true, saved_face_id contains the id of the face in front of text
     skipped due to selective display.  */
  bool_bf face_before_selective_p : 1;

  /* If true, adjust current glyph so it does not increase current row
     descent/ascent (line-height property).  Reset after this glyph.  */
  bool_bf constrain_row_ascent_descent_p : 1;

  /* If true, glyphs for line number display were already produced for
     the current row.  */
  bool_bf line_number_produced_p : 1;

  enum line_wrap_method line_wrap;

  /* The ID of the default face to use.  One of DEFAULT_FACE_ID,
     MODE_LINE_FACE_ID, etc, depending on what we are displaying.  */
  int base_face_id;

  /* If `what' == IT_CHARACTER, the character and the length in bytes
     of its multibyte sequence.  The character comes from a buffer or
     a string.  It may be different from the character displayed in
     case that unibyte_display_via_language_environment is set.

     If `what' == IT_COMPOSITION, the first component of a composition
     and length in bytes of the composition.

     If `what' is anything else, these two are undefined (will
     probably hold values for the last IT_CHARACTER or IT_COMPOSITION
     traversed by the iterator).

     The values are updated by get_next_display_element, so they are
     out of sync with the value returned by IT_CHARPOS between the
     time set_iterator_to_next advances the position and the time
     get_next_display_element loads the new values into c and len.  */
  int c, len;

  /* If what == IT_COMPOSITION, iterator substructure for the
     composition.  */
  struct composition_it cmp_it;

  /* The character to display, possibly translated to multibyte if
     multibyte_p is zero or unibyte_display_via_language_environment
     is set.  This is set after get_next_display_element has been
     called.  If we are setting it->C directly before calling
     PRODUCE_GLYPHS, this should be set beforehand too.  */
  int char_to_display;

  /* If what == IT_GLYPHLESS, the method to display such a
     character.  */
  enum glyphless_display_method glyphless_method;

  /* If what == IT_IMAGE, the id of the image to display.  */
  ptrdiff_t image_id;

  /* If what == IT_XWIDGET.  */
  struct xwidget *xwidget;

  /* Values from `slice' property.  */
  struct it_slice slice;

  /* Value of the `space-width' property, if any; nil if none.  */
  Lisp_Object space_width;

  /* Computed from the value of the `raise' property.  */
  short voffset;

  /* Number of columns per \t.  */
  short tab_width;

  /* Value of the `height' property, if any; nil if none.  */
  Lisp_Object font_height;

  /* Object and position where the current display element came from.
     Object is normally the buffer which is being rendered, but it can
     also be a Lisp string in case the current display element comes
     from an overlay string or from a display string (before- or
     after-string).  It may also be a zero-valued Lisp integer when a
     C string is being rendered, e.g., during mode-line or header-line
     update.  It can also be a cons cell of the form `(space ...)',
     when we produce a stretch glyph from a `display' specification.
     Finally, it can be nil, but only temporarily, when we are
     producing special glyphs for display purposes, like truncation
     and continuation glyphs, or blanks that extend each line to the
     edge of the window on a TTY.

     Do NOT use !BUFFERP (it.object) as a test whether we are
     iterating over a string; use STRINGP (it.string) instead.

     Position is the current iterator position in object.

     The 'position's CHARPOS is copied to glyph->charpos of the glyph
     produced by PRODUCE_GLYPHS, so any artificial value documented
     under 'struct glyph's 'charpos' member can also be found in the
     'position' member here.  */
  Lisp_Object object;
  struct text_pos position;

  /* Width in pixels of truncation and continuation glyphs.  */
  short truncation_pixel_width, continuation_pixel_width;

  /* First and last visible x-position in the display area.  If window
     is hscrolled by n columns, first_visible_x == n * FRAME_COLUMN_WIDTH
     (f), and last_visible_x == pixel width of W + first_visible_x.
     When truncation or continuation glyphs are produced due to lack of
     fringes, last_visible_x excludes the space required for these glyphs.  */
  int first_visible_x, last_visible_x;

  /* Last visible y-position + 1 in the display area without a mode
     line, if the window has one.  */
  int last_visible_y;

  /* Default amount of additional space in pixels between lines (for
     window systems only.)  */
  int extra_line_spacing;

  /* Max extra line spacing added in this row.  */
  int max_extra_line_spacing;

  /* Override font height information for this glyph.
     Used if override_ascent >= 0.  Cleared after this glyph.  */
  int override_ascent, override_descent, override_boff;

  /* If non-null, glyphs are produced in glyph_row with each call to
     produce_glyphs.  */
  struct glyph_row *glyph_row;

  /* The area of glyph_row to which glyphs are added.  */
  enum glyph_row_area area;

  /* Number of glyphs needed for the last character requested via
     produce_glyphs.  This is 1 except for tabs.  */
  int nglyphs;

  /* Width of the display element in pixels.  Result of
     produce_glyphs.  */
  int pixel_width;

  /* Current, maximum logical, and maximum physical line height
     information.  Result of produce_glyphs.  */
  int ascent, descent, max_ascent, max_descent;
  int phys_ascent, phys_descent, max_phys_ascent, max_phys_descent;

  /* Current x pixel position within the display line.  This value
     does not include the width of continuation lines in front of the
     line.  The value of current_x is automatically incremented by
     pixel_width with each call to produce_glyphs.  */
  int current_x;

  /* Accumulated width of continuation lines.  If > 0, this means we
     are currently in a continuation line.  This is initially zero and
     incremented/reset by display_line, move_it_to etc.  */
  int continuation_lines_width;

  /* Buffer position that ends the buffer text line being iterated.
     This is normally the position after the newline at EOL.  If this
     is the last line of the buffer and it doesn't have a newline,
     value is ZV/ZV_BYTE.  Set and used only if IT->bidi_p, for
     setting the end position of glyph rows produced for continuation
     lines, see display_line.  */
  struct text_pos eol_pos;

  /* Current y-position.  Automatically incremented by the height of
     glyph_row in move_it_to and display_line.  */
  int current_y;

  /* Vertical matrix position of first text line in window.  */
  int first_vpos;

  /* Current vertical matrix position, or line number.  Automatically
     incremented by move_it_to and display_line.  */
  int vpos;

  /* Horizontal matrix position reached in move_it_in_display_line.
     Only set there, not in display_line, and only when the X
     coordinate is past first_visible_x.  */
  int hpos;

  /* Current line number, zero-based.  */
  ptrdiff_t lnum;

  /* The byte position corresponding to lnum.  */
  ptrdiff_t lnum_bytepos;

  /* The width, in columns and in pixels, needed for display of the
     line numbers, or zero if not computed.  */
  int lnum_width;
  int lnum_pixel_width;

  /* The line number of point's line, or zero if not computed yet.  */
  ptrdiff_t pt_lnum;

  /* Number of pixels to offset tab stops due to width fixup of the
     first glyph that crosses first_visible_x.  This is only needed on
     GUI frames, only when display-line-numbers is in effect, and only
     in hscrolled windows.  */
  int tab_offset;

  /* Left fringe bitmap number (enum fringe_bitmap_type).  */
  unsigned left_user_fringe_bitmap : FRINGE_ID_BITS;

  /* Right fringe bitmap number (enum fringe_bitmap_type).  */
  unsigned right_user_fringe_bitmap : FRINGE_ID_BITS;

  /* Face of the left fringe glyph.  */
  unsigned left_user_fringe_face_id : FACE_ID_BITS;

  /* Face of the right fringe glyph.  */
  unsigned right_user_fringe_face_id : FACE_ID_BITS;

  /* True means we need to reorder bidirectional text for display
     in the visual order.  */
  bool_bf bidi_p : 1;

  /* For iterating over bidirectional text.  */
  struct bidi_it bidi_it;
  bidi_dir_t paragraph_embedding;
};


/* Access to positions of iterator IT.  */

#define IT_CHARPOS(IT)		CHARPOS ((IT).current.pos)
#define IT_BYTEPOS(IT)		BYTEPOS ((IT).current.pos)
#define IT_STRING_CHARPOS(IT)	CHARPOS ((IT).current.string_pos)
#define IT_STRING_BYTEPOS(IT)	BYTEPOS ((IT).current.string_pos)

/* Test if IT has reached the end of its buffer or string.  This will
   only work after get_next_display_element has been called.  */

#define ITERATOR_AT_END_P(IT) ((IT)->what == IT_EOB)

/* True means IT is at the end of a line.  This is the case if it
   is either on a newline or on a carriage return and selective
   display hides the rest of the line.  */

#define ITERATOR_AT_END_OF_LINE_P(IT)			\
     ((IT)->what == IT_CHARACTER			\
      && ((IT)->c == '\n'				\
	  || ((IT)->c == '\r' && (IT)->selective)))

/* Call produce_glyphs or FRAME_RIF->produce_glyphs, if set.  Shortcut
   to avoid the function call overhead.  */

#define PRODUCE_GLYPHS(IT)                              \
  do {                                                  \
    if ((IT)->glyph_row != NULL && (IT)->bidi_p)	\
      (IT)->glyph_row->reversed_p = (IT)->bidi_it.paragraph_dir == R2L; \
    if (FRAME_RIF ((IT)->f) != NULL)                    \
      FRAME_RIF ((IT)->f)->produce_glyphs ((IT));       \
    else                                                \
      produce_glyphs ((IT));                            \
    if ((IT)->glyph_row != NULL)                        \
      inhibit_free_realized_faces = true;		\
  } while (false)

/* Bit-flags indicating what operation move_it_to should perform.  */

enum move_operation_enum
{
  /* Stop if specified x-position is reached.  */
  MOVE_TO_X = 0x01,

  /* Stop if specified y-position is reached.  */
  MOVE_TO_Y = 0x02,

  /* Stop if specified vpos is reached.  */
  MOVE_TO_VPOS = 0x04,

  /* Stop if specified buffer or string position is reached.  */
  MOVE_TO_POS = 0x08
};

/***********************************************************************
			    Mouse Highlight
 ***********************************************************************/

/* Structure to hold mouse highlight data.  */

typedef struct {
  /* These variables describe the range of text currently shown in its
     mouse-face, together with the window they apply to.  As long as
     the mouse stays within this range, we need not redraw anything on
     its account.  Rows and columns are glyph matrix positions in
     MOUSE_FACE_WINDOW.  */
  int mouse_face_beg_row, mouse_face_beg_col, mouse_face_beg_x;
  int mouse_face_end_row, mouse_face_end_col, mouse_face_end_x;
  Lisp_Object mouse_face_window;
  int mouse_face_face_id;
  Lisp_Object mouse_face_overlay;

  /* FRAME and X, Y position of mouse when last checked for
     highlighting.  X and Y can be negative or out of range for the frame.  */
  struct frame *mouse_face_mouse_frame;
  int mouse_face_mouse_x, mouse_face_mouse_y;

  /* Nonzero if part of the text currently shown in
     its mouse-face is beyond the window end.  */
  bool_bf mouse_face_past_end : 1;

  /* True means defer mouse-motion highlighting.  */
  bool_bf mouse_face_defer : 1;

  /* True means that the mouse highlight should not be shown.  */
  bool_bf mouse_face_hidden : 1;
} Mouse_HLInfo;

INLINE void
reset_mouse_highlight (Mouse_HLInfo *hlinfo)
{

    hlinfo->mouse_face_beg_row = hlinfo->mouse_face_beg_col = -1;
    hlinfo->mouse_face_end_row = hlinfo->mouse_face_end_col = -1;
    hlinfo->mouse_face_mouse_x = hlinfo->mouse_face_mouse_y = 0;
    hlinfo->mouse_face_beg_x = hlinfo->mouse_face_end_x = 0;
    hlinfo->mouse_face_face_id = DEFAULT_FACE_ID;
    hlinfo->mouse_face_mouse_frame = NULL;
    hlinfo->mouse_face_window = Qnil;
    hlinfo->mouse_face_overlay = Qnil;
    hlinfo->mouse_face_past_end = false;
    hlinfo->mouse_face_hidden = false;
    hlinfo->mouse_face_defer = false;
}

/***********************************************************************
		   Window-based redisplay interface
 ***********************************************************************/

/* Structure used to describe runs of lines that must be scrolled.  */

struct run
{
  /* Source and destination y pixel position.  */
  int desired_y, current_y;

  /* Source and destination vpos in matrix.  */
  int desired_vpos, current_vpos;

  /* Height in pixels, number of glyph rows.  */
  int height, nrows;
};


/* Handlers for setting frame parameters.  */

typedef void (*frame_parm_handler) (struct frame *, Lisp_Object, Lisp_Object);


/* Structure holding system-dependent interface functions needed
   for window-based redisplay.  */

struct redisplay_interface
{
  /* Handlers for setting frame parameters.  */
  frame_parm_handler *frame_parm_handlers;

  /* Produce glyphs/get display metrics for the display element IT is
     loaded with.  */
  void (*produce_glyphs) (struct it *it);

  /* Write or insert LEN glyphs from STRING at the nominal output
     position.  */
  void (*write_glyphs) (struct window *w, struct glyph_row *row,
			struct glyph *string, enum glyph_row_area area,
			int len);
  void (*insert_glyphs) (struct window *w, struct glyph_row *row,
			 struct glyph *start, enum glyph_row_area area,
			 int len);

  /* Clear from nominal output position to X.  X < 0 means clear
     to right end of display.  */
  void (*clear_end_of_line) (struct window *w, struct glyph_row *row,
			     enum glyph_row_area area, int x);

  /* Function to call to scroll the display as described by RUN on
     window W.  */
  void (*scroll_run_hook) (struct window *w, struct run *run);

  /* Function to call after a line in a display has been completely
     updated.  Used to draw truncation marks and alike.  DESIRED_ROW
     is the desired row which has been updated.  */
  void (*after_update_window_line_hook) (struct window *w,
					 struct glyph_row *desired_row);

  /* Function to call before beginning to update window W in
     window-based redisplay.  */
  void (*update_window_begin_hook) (struct window *w);

  /* Function to call after window W has been updated in window-based
     redisplay.  CURSOR_ON_P true means switch cursor on.
     MOUSE_FACE_OVERWRITTEN_P true means that some lines in W
     that contained glyphs in mouse-face were overwritten, so we
     have to update the mouse highlight.  */
  void (*update_window_end_hook) (struct window *w, bool cursor_on_p,
                                  bool mouse_face_overwritten_p);

  /* Flush the display of frame F.  For X, this is XFlush.  */
  void (*flush_display) (struct frame *f);

  /* Clear the mouse highlight in window W, if there is any.  */
  void (*clear_window_mouse_face) (struct window *w);

  /* Set *LEFT and *RIGHT to the left and right overhang of GLYPH on
     frame F.  */
  void (*get_glyph_overhangs) (struct glyph *glyph, struct frame *f,
                               int *left, int *right);

  /* Fix the display of AREA of ROW in window W for overlapping rows.
     This function is called from redraw_overlapping_rows after
     desired rows have been made current.  */
  void (*fix_overlapping_area) (struct window *w, struct glyph_row *row,
                                enum glyph_row_area area, int);

#ifdef HAVE_WINDOW_SYSTEM

  /* Draw a fringe bitmap in window W of row ROW using parameters P.  */
  void (*draw_fringe_bitmap) (struct window *w, struct glyph_row *row,
                              struct draw_fringe_bitmap_params *p);

  /* Define and destroy fringe bitmap no. WHICH.  */
  void (*define_fringe_bitmap) (int which, unsigned short *bits,
                                int h, int wd);
  void (*destroy_fringe_bitmap) (int which);

  /* Compute left and right overhang of glyph string S.
     A NULL pointer if platform does not support this. */
  void (*compute_glyph_string_overhangs) (struct glyph_string *s);

  /* Draw a glyph string S.  */
  void (*draw_glyph_string) (struct glyph_string *s);

  /* Define cursor CURSOR on frame F.  */
  void (*define_frame_cursor) (struct frame *f, Emacs_Cursor cursor);

  /* Clear the area at (X,Y,WIDTH,HEIGHT) of frame F.  */
  void (*clear_frame_area) (struct frame *f, int x, int y,
                            int width, int height);

 /* Clear area of frame F's internal border.  If the internal border
    face of F has been specified (is not null), fill the area with
    that face.  */
  void (*clear_under_internal_border) (struct frame *f);

  /* Draw specified cursor CURSOR_TYPE of width CURSOR_WIDTH
     at row GLYPH_ROW on window W if ON_P is true.  If ON_P is
     false, don't draw cursor.  If ACTIVE_P is true, system caret
     should track this cursor (when applicable).  */
  void (*draw_window_cursor) (struct window *w,
			      struct glyph_row *glyph_row,
			      int x, int y,
			      enum text_cursor_kinds cursor_type,
			      int cursor_width, bool on_p, bool active_p);

  /* Draw vertical border for window W from (X,Y_0) to (X,Y_1).  */
  void (*draw_vertical_window_border) (struct window *w,
                                       int x, int y_0, int y_1);

  /* Draw window divider for window W from (X_0, Y_0) to (X_1, ,Y_1).  */
  void (*draw_window_divider) (struct window *w,
			       int x_0, int x_1, int y_0, int y_1);

  /* Shift display of frame F to make room for inserted glyphs.
     The area at pixel (X,Y) of width WIDTH and height HEIGHT is
     shifted right by SHIFT_BY pixels.  */
  void (*shift_glyphs_for_insert) (struct frame *f,
                                   int x, int y, int width,
                                   int height, int shift_by);

  /* Start display hourglass cursor on frame F.  */
  void (*show_hourglass) (struct frame *f);

  /* Cancel hourglass cursor on frame F.  */
  void (*hide_hourglass) (struct frame *f);

  /* Called to (re)calculate the default face when changing the font
     backend.  */
  void (*default_font_parameter) (struct frame *f, Lisp_Object parms);
#endif /* HAVE_WINDOW_SYSTEM */
};


/***********************************************************************
				Images
 ***********************************************************************/

#ifdef HAVE_WINDOW_SYSTEM

# if (defined USE_CAIRO || defined HAVE_XRENDER \
      || defined HAVE_NS || defined HAVE_NTGUI)
#  define HAVE_NATIVE_TRANSFORMS
# endif

/* Structure describing an image.  Specific image formats like XBM are
   converted into this form, so that display only has to deal with
   this type of image.  */

struct image
{
  /* The time in seconds at which the image was last displayed.  Set
     in prepare_image_for_display.  */
  struct timespec timestamp;

  /* Pixmaps of the image.  */
  Emacs_Pixmap pixmap, mask;

#ifdef USE_CAIRO
  void *cr_data;
#endif
#ifdef HAVE_X_WINDOWS
  /* X images of the image, corresponding to the above Pixmaps.
     Non-NULL means it and its Pixmap counterpart may be out of sync
     and the latter is outdated.  NULL means the X image has been
     synchronized to Pixmap.  */
  XImage *ximg, *mask_img;

# if !defined USE_CAIRO && defined HAVE_XRENDER
  /* Picture versions of pixmap and mask for compositing.  */
  Picture picture, mask_picture;
# endif
#endif	/* HAVE_X_WINDOWS */
#ifdef HAVE_NTGUI
  XFORM xform;
#endif

  /* Colors allocated for this image, if any.  Allocated via xmalloc.  */
  unsigned long *colors;
  int ncolors;

  /* A single `background color' for this image, for the use of anyone that
     cares about such a thing.  Only valid if the `background_valid' field
     is true.  This should generally be accessed by calling the accessor
     macro `IMAGE_BACKGROUND', which will heuristically calculate a value
     if necessary.  */
  unsigned long background;

  /* Foreground and background colors of the frame on which the image
     is created.  */
  unsigned long frame_foreground, frame_background;

  /* True if this image has a `transparent' background -- that is, is
     uses an image mask.  The accessor macro for this is
     `IMAGE_BACKGROUND_TRANSPARENT'.  */
  bool_bf background_transparent : 1;

  /* True if the `background' and `background_transparent' fields are
     valid, respectively. */
  bool_bf background_valid : 1, background_transparent_valid : 1;

  /* Width and height of the image.  */
  int width, height;

  /* These values are used for the rectangles displayed for images
     that can't be loaded.  */
#define DEFAULT_IMAGE_WIDTH 30
#define DEFAULT_IMAGE_HEIGHT 30

  /* Top/left and bottom/right corner pixel of actual image data.
     Used by four_corners_best to consider the real image data,
     rather than looking at the optional image margin.  */
  int corners[4];
#define TOP_CORNER 0
#define LEFT_CORNER 1
#define BOT_CORNER 2
#define RIGHT_CORNER 3

  /* Percent of image height used as ascent.  A value of
     CENTERED_IMAGE_ASCENT means draw the image centered on the
     line.  */
  int ascent;
#define DEFAULT_IMAGE_ASCENT 50
#define CENTERED_IMAGE_ASCENT -1

  /* Lisp specification of this image.  */
  Lisp_Object spec;

  /* List of "references" followed to build the image.
     Typically will just contain the name of the image file.
     Used to allow fine-grained cache flushing.  */
  Lisp_Object dependencies;

  /* Relief to draw around the image.  */
  int relief;

  /* Optional margins around the image.  This includes the relief.  */
  int hmargin, vmargin;

  /* Reference to the type of the image.  */
  struct image_type const *type;

  /* True means that loading the image failed.  Don't try again.  */
  bool load_failed_p;

  /* A place for image types to store additional data.  It is marked
     during GC.  */
  Lisp_Object lisp_data;

  /* Hash value of image specification to speed up comparisons.  */
  EMACS_UINT hash;

  /* Image id of this image.  */
  ptrdiff_t id;

  /* Hash collision chain.  */
  struct image *next, *prev;
};


/* Cache of images.  Each frame has a cache.  X frames with the same
   x_display_info share their caches.  */

struct image_cache
{
  /* Hash table of images.  */
  struct image **buckets;

  /* Vector mapping image ids to images.  */
  struct image **images;

  /* Allocated size of `images'.  */
  ptrdiff_t size;

  /* Number of images in the cache.  */
  ptrdiff_t used;

  /* Reference count (number of frames sharing this cache).  */
  ptrdiff_t refcount;
};


/* A non-null pointer to the image with id ID on frame F.  */

#define IMAGE_FROM_ID(F, ID)					\
  (eassert (UNSIGNED_CMP (ID, <, FRAME_IMAGE_CACHE (F)->used)),	\
   FRAME_IMAGE_CACHE (F)->images[ID])

/* Value is a pointer to the image with id ID on frame F, or null if
   no image with that id exists.  */

#define IMAGE_OPT_FROM_ID(F, ID)				\
  (UNSIGNED_CMP (ID, <, FRAME_IMAGE_CACHE (F)->used)		\
   ? FRAME_IMAGE_CACHE (F)->images[ID]				\
   : NULL)

/* Size of bucket vector of image caches.  Should be prime.  */

#define IMAGE_CACHE_BUCKETS_SIZE 1001

#endif /* HAVE_WINDOW_SYSTEM */



/***********************************************************************
			       Tab-bars
 ***********************************************************************/

/* Enumeration defining where to find tab-bar item information in
   tab-bar items vectors stored with frames.  Each tab-bar item
   occupies TAB_BAR_ITEM_NSLOTS elements in such a vector.  */

enum tab_bar_item_idx
{
  /* The key of the tab-bar item.  Used to remove items when a binding
     for `undefined' is found.  */
  TAB_BAR_ITEM_KEY,

  /* Non-nil if item is enabled.  */
  TAB_BAR_ITEM_ENABLED_P,

  /* Non-nil if item is selected (pressed).  */
  TAB_BAR_ITEM_SELECTED_P,

  /* Caption.  */
  TAB_BAR_ITEM_CAPTION,

  /* The binding.  */
  TAB_BAR_ITEM_BINDING,

  /* Help string.  */
  TAB_BAR_ITEM_HELP,

  /* Sentinel = number of slots in tab_bar_items occupied by one
     tab-bar item.  */
  TAB_BAR_ITEM_NSLOTS
};

/* Default values of the above variables.  */

#define DEFAULT_TAB_BAR_BUTTON_MARGIN 4
#define DEFAULT_TAB_BAR_BUTTON_RELIEF 1

/* The height in pixels of the default tab-bar images.  */

#define DEFAULT_TAB_BAR_IMAGE_HEIGHT 18


/***********************************************************************
			       Tool-bars
 ***********************************************************************/

/* Enumeration defining where to find tool-bar item information in
   tool-bar items vectors stored with frames.  Each tool-bar item
   occupies TOOL_BAR_ITEM_NSLOTS elements in such a vector.  */

enum tool_bar_item_idx
{
  /* The key of the tool-bar item.  Used to remove items when a binding
     for `undefined' is found.  */
  TOOL_BAR_ITEM_KEY,

  /* Non-nil if item is enabled.  */
  TOOL_BAR_ITEM_ENABLED_P,

  /* Non-nil if item is selected (pressed).  */
  TOOL_BAR_ITEM_SELECTED_P,

  /* Caption.  */
  TOOL_BAR_ITEM_CAPTION,

  /* Image(s) to display.  This is either a single image specification
     or a vector of specifications.  */
  TOOL_BAR_ITEM_IMAGES,

  /* The binding.  */
  TOOL_BAR_ITEM_BINDING,

  /* Button type.  One of nil (default button), t (a separator),
     `:radio', or `:toggle'.  The latter two currently do nothing.  */
  TOOL_BAR_ITEM_TYPE,

  /* Help string.  */
  TOOL_BAR_ITEM_HELP,

  /* Icon file name of right to left image when an RTL locale is used.  */
  TOOL_BAR_ITEM_RTL_IMAGE,

  /* Label to show when text labels are enabled.  */
  TOOL_BAR_ITEM_LABEL,

  /* If we shall show the label only below the icon and not beside it.  */
  TOOL_BAR_ITEM_VERT_ONLY,

  /* Sentinel = number of slots in tool_bar_items occupied by one
     tool-bar item.  */
  TOOL_BAR_ITEM_NSLOTS
};


/* An enumeration for the different images that can be specified
   for a tool-bar item.  */

enum tool_bar_item_image
{
  TOOL_BAR_IMAGE_ENABLED_SELECTED,
  TOOL_BAR_IMAGE_ENABLED_DESELECTED,
  TOOL_BAR_IMAGE_DISABLED_SELECTED,
  TOOL_BAR_IMAGE_DISABLED_DESELECTED
};

#define DEFAULT_TOOL_BAR_LABEL_SIZE 14

/* Default values of the above variables.  */

#define DEFAULT_TOOL_BAR_BUTTON_MARGIN 4
#define DEFAULT_TOOL_BAR_BUTTON_RELIEF 1

/* The height in pixels of the default tool-bar images.  */

#define DEFAULT_TOOL_BAR_IMAGE_HEIGHT 24


/***********************************************************************
			 Terminal Capabilities
 ***********************************************************************/

/* Each of these is a bit representing a terminal `capability' (bold,
   inverse, etc).  They are or'd together to specify the set of
   capabilities being queried for when calling `tty_capable_p' (which
   returns true if the terminal supports all of them).  */

#define TTY_CAP_INVERSE		0x01
#define TTY_CAP_UNDERLINE	0x02
#define TTY_CAP_BOLD		0x04
#define TTY_CAP_DIM		0x08
#define TTY_CAP_ITALIC  	0x10


/***********************************************************************
			  Function Prototypes
 ***********************************************************************/

/* Defined in bidi.c */

extern void bidi_init_it (ptrdiff_t, ptrdiff_t, bool, struct bidi_it *);
extern void bidi_move_to_visually_next (struct bidi_it *);
extern void bidi_paragraph_init (bidi_dir_t, struct bidi_it *, bool);
extern int  bidi_mirror_char (int);
extern void bidi_push_it (struct bidi_it *);
extern void bidi_pop_it (struct bidi_it *);
extern void *bidi_shelve_cache (void);
extern void bidi_unshelve_cache (void *, bool);
extern ptrdiff_t bidi_find_first_overridden (struct bidi_it *);

/* Defined in xdisp.c */

struct glyph_row *row_containing_pos (struct window *, ptrdiff_t,
                                      struct glyph_row *,
                                      struct glyph_row *, int);
int line_bottom_y (struct it *);
int default_line_pixel_height (struct window *);
bool display_prop_intangible_p (Lisp_Object, Lisp_Object, ptrdiff_t, ptrdiff_t);
void resize_echo_area_exactly (void);
bool resize_mini_window (struct window *, bool);
void set_vertical_scroll_bar (struct window *);
void set_horizontal_scroll_bar (struct window *);
int try_window (Lisp_Object, struct text_pos, int);
void window_box (struct window *, enum glyph_row_area,
		 int *, int *, int *, int *);
int window_box_height (struct window *);
int window_text_bottom_y (struct window *);
int window_box_width (struct window *, enum glyph_row_area);
int window_box_left (struct window *, enum glyph_row_area);
int window_box_left_offset (struct window *, enum glyph_row_area);
int window_box_right (struct window *, enum glyph_row_area);
int estimate_mode_line_height (struct frame *, enum face_id);
int move_it_to (struct it *, ptrdiff_t, int, int, int, int);
void pixel_to_glyph_coords (struct frame *, int, int, int *, int *,
                            NativeRectangle *, bool);
void remember_mouse_glyph (struct frame *, int, int, NativeRectangle *);

void mark_window_display_accurate (Lisp_Object, bool);
void redisplay_preserve_echo_area (int);
void init_iterator (struct it *, struct window *, ptrdiff_t,
                    ptrdiff_t, struct glyph_row *, enum face_id);
void init_iterator_to_row_start (struct it *, struct window *,
                                 struct glyph_row *);
void start_display (struct it *, struct window *, struct text_pos);
void move_it_vertically (struct it *, int);
void move_it_vertically_backward (struct it *, int);
void move_it_by_lines (struct it *, ptrdiff_t);
void move_it_past_eol (struct it *);
void move_it_in_display_line (struct it *it,
			      ptrdiff_t to_charpos, int to_x,
			      enum move_operation_enum op);
int partial_line_height (struct it *it_origin);
bool in_display_vector_p (struct it *);
int frame_mode_line_height (struct frame *);
extern bool redisplaying_p;
extern bool help_echo_showing_p;
extern Lisp_Object help_echo_string, help_echo_window;
extern Lisp_Object help_echo_object, previous_help_echo_string;
extern ptrdiff_t help_echo_pos;
extern int last_tab_bar_item;
extern int last_tool_bar_item;
extern void reseat_at_previous_visible_line_start (struct it *);
extern Lisp_Object lookup_glyphless_char_display (int, struct it *);
extern ptrdiff_t compute_display_string_pos (struct text_pos *,
					     struct bidi_string_data *,
					     struct window *, bool, int *);
extern ptrdiff_t compute_display_string_end (ptrdiff_t,
					     struct bidi_string_data *);
extern void produce_stretch_glyph (struct it *);
extern int merge_glyphless_glyph_face (struct it *);
extern void forget_escape_and_glyphless_faces (void);

extern void get_font_ascent_descent (struct font *, int *, int *);

#ifdef HAVE_WINDOW_SYSTEM

extern void gui_get_glyph_overhangs (struct glyph *, struct frame *,
                                     int *, int *);
extern struct font *font_for_underline_metrics (struct glyph_string *);
extern void gui_produce_glyphs (struct it *);

extern void gui_write_glyphs (struct window *, struct glyph_row *,
                              struct glyph *, enum glyph_row_area, int);
extern void gui_insert_glyphs (struct window *, struct glyph_row *,
                               struct glyph *, enum glyph_row_area, int);
extern void gui_clear_end_of_line (struct window *, struct glyph_row *,
                                   enum glyph_row_area, int);
extern void gui_fix_overlapping_area (struct window *, struct glyph_row *,
                                      enum glyph_row_area, int);
extern void draw_phys_cursor_glyph (struct window *,
                                    struct glyph_row *,
                                    enum draw_glyphs_face);
extern void get_phys_cursor_geometry (struct window *, struct glyph_row *,
                                      struct glyph *, int *, int *, int *);
extern void erase_phys_cursor (struct window *);
extern void display_and_set_cursor (struct window *, bool, int, int, int, int);
extern void gui_update_cursor (struct frame *, bool);
extern void gui_clear_cursor (struct window *);
extern void gui_draw_vertical_border (struct window *w);
extern void gui_draw_right_divider (struct window *w);

extern int get_glyph_string_clip_rects (struct glyph_string *,
                                        NativeRectangle *, int);
extern void get_glyph_string_clip_rect (struct glyph_string *,
                                        NativeRectangle *nr);
extern Lisp_Object find_hot_spot (Lisp_Object, int, int);

extern void handle_tab_bar_click (struct frame *,
                                   int, int, bool, int);
extern void handle_tool_bar_click (struct frame *,
                                   int, int, bool, int);

extern void expose_frame (struct frame *, int, int, int, int);
extern bool gui_intersect_rectangles (const Emacs_Rectangle *,
                                      const Emacs_Rectangle *,
                                      Emacs_Rectangle *);
#endif	/* HAVE_WINDOW_SYSTEM */

extern void note_mouse_highlight (struct frame *, int, int);
extern void gui_clear_window_mouse_face (struct window *);
extern void cancel_mouse_face (struct frame *);
extern bool clear_mouse_face (Mouse_HLInfo *);
extern bool cursor_in_mouse_face_p (struct window *w);
extern void tty_draw_row_with_mouse_face (struct window *, struct glyph_row *,
					  int, int, enum draw_glyphs_face);
extern void display_tty_menu_item (const char *, int, int, int, int, bool);

/* Flags passed to try_window.  */
#define TRY_WINDOW_CHECK_MARGINS	(1 << 0)
#define TRY_WINDOW_IGNORE_FONTS_CHANGE	(1 << 1)

int lookup_fringe_bitmap (Lisp_Object);
void draw_fringe_bitmap (struct window *, struct glyph_row *, int);
void draw_row_fringe_bitmaps (struct window *, struct glyph_row *);
bool draw_window_fringes (struct window *, bool);
bool update_window_fringes (struct window *, bool);

void gui_init_fringe (struct redisplay_interface *);

#ifdef HAVE_NTGUI
void w32_reset_fringes (void);
#endif

extern unsigned row_hash (struct glyph_row *);

extern bool buffer_flipping_blocked_p (void);

/* Defined in image.c */

#ifdef HAVE_WINDOW_SYSTEM

extern ptrdiff_t image_bitmap_pixmap (struct frame *, ptrdiff_t);
extern void image_reference_bitmap (struct frame *, ptrdiff_t);
extern ptrdiff_t image_create_bitmap_from_data (struct frame *, char *,
                                                unsigned int, unsigned int);
extern ptrdiff_t image_create_bitmap_from_file (struct frame *, Lisp_Object);
#if defined HAVE_XPM && defined HAVE_X_WINDOWS && !defined USE_GTK
extern ptrdiff_t x_create_bitmap_from_xpm_data (struct frame *, const char **);
#endif
#ifndef image_destroy_bitmap
extern void image_destroy_bitmap (struct frame *, ptrdiff_t);
#endif
extern void image_destroy_all_bitmaps (Display_Info *);
#ifdef HAVE_X_WINDOWS
extern void x_create_bitmap_mask (struct frame *, ptrdiff_t);
#ifndef USE_CAIRO
extern void x_kill_gs_process (Pixmap, struct frame *);
#endif	/* !USE_CAIRO */
#endif
extern Lisp_Object image_find_image_file (Lisp_Object);

struct image_cache *make_image_cache (void);
void free_image_cache (struct frame *);
void clear_image_caches (Lisp_Object);
void mark_image_cache (struct image_cache *);
bool valid_image_p (Lisp_Object);
void prepare_image_for_display (struct frame *, struct image *);
ptrdiff_t lookup_image (struct frame *, Lisp_Object);

#if defined HAVE_X_WINDOWS || defined USE_CAIRO || defined HAVE_NS
#define RGB_PIXEL_COLOR unsigned long
#endif

#ifdef HAVE_NTGUI
#define RGB_PIXEL_COLOR COLORREF
#endif

RGB_PIXEL_COLOR image_background (struct image *, struct frame *,
                                  Emacs_Pix_Context img);
int image_background_transparent (struct image *, struct frame *,
                                  Emacs_Pix_Context mask);

int image_ascent (struct image *, struct face *, struct glyph_slice *);

#endif

/* Defined in sysdep.c */

void get_tty_size (int, int *, int *);
void request_sigio (void);
void unrequest_sigio (void);
bool tabs_safe_p (int);
void init_baud_rate (int);
void init_sigio (int);

/* Defined in xfaces.c.  */

#ifdef HAVE_X_WINDOWS
void unload_color (struct frame *, unsigned long);
void x_free_colors (struct frame *, unsigned long *, int);
#endif

void update_face_from_frame_parameter (struct frame *, Lisp_Object,
                                       Lisp_Object);
extern bool tty_defined_color (struct frame *, const char *, Emacs_Color *,
                               bool, bool);

Lisp_Object tty_color_name (struct frame *, int);
void clear_face_cache (bool);
unsigned long load_color (struct frame *, struct face *, Lisp_Object,
                          enum lface_attribute_index);
char *choose_face_font (struct frame *, Lisp_Object *, Lisp_Object,
                        int *);
#ifdef HAVE_WINDOW_SYSTEM
void prepare_face_for_display (struct frame *, struct face *);
#endif
int lookup_named_face (struct window *, struct frame *, Lisp_Object, bool);
int lookup_basic_face (struct window *, struct frame *, int);
int smaller_face (struct frame *, int, int);
int face_with_height (struct frame *, int, int);
int lookup_derived_face (struct window *, struct frame *,
                         Lisp_Object, int, bool);
void init_frame_faces (struct frame *);
void free_frame_faces (struct frame *);
void recompute_basic_faces (struct frame *);
int face_at_buffer_position (struct window *, ptrdiff_t, ptrdiff_t *,
                             ptrdiff_t, bool, int, enum lface_attribute_index);
int face_for_overlay_string (struct window *, ptrdiff_t, ptrdiff_t *, ptrdiff_t,
                             bool, Lisp_Object);
int face_at_string_position (struct window *, Lisp_Object, ptrdiff_t, ptrdiff_t,
                             ptrdiff_t *, enum face_id, bool,
                             enum lface_attribute_index);
int merge_faces (struct window *, Lisp_Object, int, int);
int compute_char_face (struct frame *, int, Lisp_Object);
void free_all_realized_faces (Lisp_Object);
extern char unspecified_fg[], unspecified_bg[];

/* Defined in xfns.c.  */

#ifdef HAVE_X_WINDOWS
void gamma_correct (struct frame *, XColor *);
#endif
#ifdef HAVE_NTGUI
void gamma_correct (struct frame *, COLORREF *);
#endif

#ifdef HAVE_WINDOW_SYSTEM

extern void start_hourglass (void);
extern void cancel_hourglass (void);

/* Returns the background color of IMG, calculating one heuristically if
   necessary.  If non-zero, XIMG is an existing XImage object to use for
   the heuristic.  */

#define IMAGE_BACKGROUND(img, f, ximg)					      \
   ((img)->background_valid						      \
    ? (img)->background							      \
    : image_background (img, f, ximg))

/* Returns true if IMG has a `transparent' background, using heuristics
   to decide if necessary.  If non-zero, MASK is an existing XImage
   object to use for the heuristic.  */

#define IMAGE_BACKGROUND_TRANSPARENT(img, f, mask)			      \
   ((img)->background_transparent_valid					      \
    ? (img)->background_transparent					      \
    : image_background_transparent (img, f, mask))

#endif /* HAVE_WINDOW_SYSTEM */


/* Defined in xmenu.c.  */

int popup_activated (void);

/* Defined in dispnew.c.  */

extern Lisp_Object buffer_posn_from_coords (struct window *,
                                            int *, int *,
                                            struct display_pos *,
                                            Lisp_Object *,
                                            int *, int *, int *, int *);
extern Lisp_Object mode_line_string (struct window *, enum window_part,
                                     int *, int *, ptrdiff_t *,
                                     Lisp_Object *,
                                     int *, int *, int *, int *);
extern Lisp_Object marginal_area_string (struct window *, enum window_part,
                                         int *, int *, ptrdiff_t *,
                                         Lisp_Object *,
                                         int *, int *, int *, int *);
extern void redraw_frame (struct frame *);
extern bool update_frame (struct frame *, bool, bool);
extern void update_frame_with_menu (struct frame *, int, int);
extern void bitch_at_user (void);
extern void adjust_frame_glyphs (struct frame *);
void free_glyphs (struct frame *);
void free_window_matrices (struct window *);
void check_glyph_memory (void);
void mirrored_line_dance (struct glyph_matrix *, int, int, int *, char *);
void clear_glyph_matrix (struct glyph_matrix *);
void clear_current_matrices (struct frame *f);
void clear_desired_matrices (struct frame *);
void shift_glyph_matrix (struct window *, struct glyph_matrix *,
                         int, int, int);
void rotate_matrix (struct glyph_matrix *, int, int, int);
void increment_matrix_positions (struct glyph_matrix *,
                                 int, int, ptrdiff_t, ptrdiff_t);
void blank_row (struct window *, struct glyph_row *, int);
void clear_glyph_matrix_rows (struct glyph_matrix *, int, int);
void clear_glyph_row (struct glyph_row *);
void prepare_desired_row (struct window *, struct glyph_row *, bool);
void update_single_window (struct window *);
#ifdef HAVE_WINDOW_SYSTEM
extern void gui_update_window_begin (struct window *);
extern void gui_update_window_end (struct window *, bool, bool);
#endif
void do_pending_window_change (bool);
void change_frame_size (struct frame *, int, int, bool, bool, bool, bool);
void init_display (void);
void syms_of_display (void);
extern void spec_glyph_lookup_face (struct window *, GLYPH *);
extern void fill_up_frame_row_with_spaces (struct glyph_row *, int);

/* Defined in terminal.c.  */

extern void ring_bell (struct frame *);
extern void update_begin (struct frame *);
extern void update_end (struct frame *);
extern void set_terminal_window (struct frame *, int);
extern void cursor_to (struct frame *, int, int);
extern void raw_cursor_to (struct frame *, int, int);
extern void clear_to_end (struct frame *);
extern void clear_frame (struct frame *);
extern void clear_end_of_line (struct frame *, int);
extern void write_glyphs (struct frame *, struct glyph *, int);
extern void insert_glyphs (struct frame *, struct glyph *, int);
extern void delete_glyphs (struct frame *, int);
extern void ins_del_lines (struct frame *, int, int);

extern struct terminal *init_initial_terminal (void);


/* Defined in term.c */

extern void tty_turn_off_insert (struct tty_display_info *);
extern int string_cost (const char *);
extern int per_line_cost (const char *);
extern void calculate_costs (struct frame *);
extern void produce_glyphs (struct it *);
extern bool tty_capable_p (struct tty_display_info *, unsigned);
extern void set_tty_color_mode (struct tty_display_info *, struct frame *);
extern void create_tty_output (struct frame *);
extern struct terminal *init_tty (const char *, const char *, bool);
extern void tty_append_glyph (struct it *);

/* All scrolling costs measured in characters.
   So no cost can exceed the area of a frame, measured in characters.
   Let's hope this is never more than 1000000 characters.  */
enum { SCROLL_INFINITY = 1000000 };

/* Defined in scroll.c */

extern int scrolling_max_lines_saved (int, int, unsigned *, unsigned *, int *);
extern void do_line_insertion_deletion_costs (struct frame *, const char *,
                                              const char *, const char *,
					      const char *, const char *,
					      const char *, int);
void scrolling_1 (struct frame *, int, int, int, int *, int *, unsigned *,
                  unsigned *, int);

/* Defined in frame.c */

#ifdef HAVE_WINDOW_SYSTEM

/* Types we might convert a resource string into.  */
enum resource_types
{
  RES_TYPE_NUMBER,
  RES_TYPE_FLOAT,
  RES_TYPE_BOOLEAN,
  RES_TYPE_STRING,
  RES_TYPE_SYMBOL,
  RES_TYPE_BOOLEAN_NUMBER
};

extern Display_Info *check_x_display_info (Lisp_Object);
extern Lisp_Object gui_display_get_arg (Display_Info *, Lisp_Object,
                                        Lisp_Object, const char *, const char *,
                                        enum resource_types);
extern Lisp_Object gui_frame_get_and_record_arg (struct frame *, Lisp_Object,
                                                 Lisp_Object,
                                                 const char *, const char *,
                                                 enum resource_types);
extern Lisp_Object gui_default_parameter (struct frame *, Lisp_Object,
                                          Lisp_Object, Lisp_Object,
                                          const char *, const char *,
                                          enum resource_types);

#ifndef HAVE_NS /* These both used on W32 and X only.  */
extern bool gui_mouse_grabbed (Display_Info *);
extern void gui_redo_mouse_highlight (Display_Info *);
#endif /* HAVE_NS */

#endif /* HAVE_WINDOW_SYSTEM */

INLINE_HEADER_END

#endif /* not DISPEXTERN_H_INCLUDED */
