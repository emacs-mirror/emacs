/* Interface definitions for display code.
   Copyright (C) 1985, 1993, 1994, 1997, 1998, 1999, 2000, 2001, 2002
     Free Software Foundation, Inc.

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

/* New redisplay written by Gerd Moellmann <gerd@gnu.org>.  */

#ifndef DISPEXTERN_H_INCLUDED
#define DISPEXTERN_H_INCLUDED

#ifdef HAVE_X_WINDOWS
#include <X11/Xlib.h>
#ifdef USE_X_TOOLKIT
#include <X11/Intrinsic.h>
#endif /* USE_X_TOOLKIT */
#endif /* HAVE_X_WINDOWS */

#ifdef MSDOS
#include "msdos.h"
#endif

#ifdef HAVE_NTGUI
#include "w32gui.h"
#endif

#ifdef MAC_OS
#include "macgui.h"
#endif

/* Structure forward declarations.  Some are here because function
   prototypes below reference structure types before their definition
   in this file.  Some are here because not every file including
   dispextern.h also includes frame.h and windows.h.  */

struct glyph;
struct glyph_row;
struct glyph_matrix;
struct glyph_pool;
struct frame;
struct window;



/***********************************************************************
			      Debugging
 ***********************************************************************/

/* If GLYPH_DEBUG is non-zero, additional checks are activated.  Turn
   it off by defining the macro GLYPH_DEBUG to zero.  */

#ifndef GLYPH_DEBUG
#define GLYPH_DEBUG 0
#endif

/* Macros to include code only if GLYPH_DEBUG != 0.  */

#if GLYPH_DEBUG
#define IF_DEBUG(X)	X
#define xassert(X)	do {if (!(X)) abort ();} while (0)
#else
#define IF_DEBUG(X)	(void) 0
#define xassert(X)	(void) 0
#endif

/* Macro for displaying traces of redisplay.  If Emacs was compiled
   with GLYPH_DEBUG != 0, the variable trace_redisplay_p can be set to
   a non-zero value in debugging sessions to activate traces.  */

#if GLYPH_DEBUG

extern int trace_redisplay_p;
#include <stdio.h>

#define TRACE(X)				\
     if (trace_redisplay_p)			\
       fprintf X;				\
     else					\
       (void) 0

#else /* GLYPH_DEBUG == 0 */

#define TRACE(X)	(void) 0

#endif /* GLYPH_DEBUG == 0 */

     

/***********************************************************************
			    Text positions
 ***********************************************************************/

/* Starting with Emacs 20.3, characters from strings and buffers have
   both a character and a byte position associated with them.  The
   following structure holds such a pair of positions.  */
     
struct text_pos
{
  /* Character position.  */
  int charpos;

  /* Corresponding byte position.  */
  int bytepos;
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
     while (0)

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
     while (0)

/* Set text position POS from marker MARKER.  */

#define SET_TEXT_POS_FROM_MARKER(POS, MARKER)		\
     (CHARPOS (POS) = marker_position ((MARKER)),	\
      BYTEPOS (POS) = marker_byte_position ((MARKER)))

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
  int overlay_string_index;

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

/* Enumeration of glyph types.  Glyph structures contain a type field
   containing one of the enumerators defined here.  */

enum glyph_type
{
  /* Glyph describes a character.  */
  CHAR_GLYPH, 	

  /* Glyph describes a composition sequence.  */
  COMPOSITE_GLYPH,

  /* Glyph describes an image.  */
  IMAGE_GLYPH,

  /* Glyph is a space of fractional width and/or height.  */
  STRETCH_GLYPH
};


/* Glyphs.

   Be extra careful when changing this structure!  Esp. make sure that
   functions producing glyphs, like x_append_glyph, fill ALL of the
   glyph structure, and that GLYPH_EQUAL_P compares all
   display-relevant members of glyphs (not to imply that these are the
   only things to check when you add a member).  */

struct glyph
{
  /* Position from which this glyph was drawn.  If `object' below is a
     Lisp string, this is a position in that string.  If it is a
     buffer, this is a position in that buffer.  A value of -1
     together with a null object means glyph is a truncation glyph at
     the start of a row.  */
  int charpos;

  /* Lisp object source of this glyph.  Currently either a buffer or
     a string, if the glyph was produced from characters which came from 
     a buffer or a string; or 0 if the glyph was inserted by redisplay
     for its own purposes such as padding.  */
  Lisp_Object object;

  /* Width in pixels.  */
  short pixel_width;

  /* Vertical offset.  If < 0, the glyph is displayed raised, if > 0
     the glyph is displayed lowered.  */
  short voffset;

  /* Which kind of glyph this is---character, image etc.  Value
     should be an enumerator of type enum glyph_type.  */
  unsigned type : 2;

  /* 1 means this glyph was produced from multibyte text.  Zero
     means it was produced from unibyte text, i.e. charsets aren't
     applicable, and encoding is not performed.  */
  unsigned multibyte_p : 1;

  /* Non-zero means draw a box line at the left or right side of this
     glyph.  This is part of the implementation of the face attribute
     `:box'.  */
  unsigned left_box_line_p : 1;
  unsigned right_box_line_p : 1;

  /* Non-zero means this glyph's physical ascent or descent is greater
     than its logical ascent/descent, i.e. it may potentially overlap
     glyphs above or below it.  */
  unsigned overlaps_vertically_p : 1;

  /* 1 means glyph is a padding glyph.  Padding glyphs are used for
     characters whose visual shape consists of more than one glyph
     (e.g. Asian characters).  All but the first glyph of such a glyph
     sequence have the padding_p flag set.  Only used for terminal
     frames, and there only to minimize code changes.  A better way
     would probably be to use the width field of glyphs to express
     padding. */
  unsigned padding_p : 1;

  /* 1 means the actual glyph is not available, draw a box instead.
     This can happen when a font couldn't be loaded, or a character
     doesn't have a glyph in a font.  */
  unsigned glyph_not_available_p : 1;

  /* Face of the glyph.  */
  unsigned face_id : 22;

#ifdef WINDOWSNT
  /* Type of font used to display the character glyph. Used to
     determine which set of functions to use to obtain font metrics
     for the glyph. Value should be an enumerator of the type
     w32_char_font_type.  */
  unsigned w32_font_type : 2;
#endif

  /* A union of sub-structures for different glyph types.  */
  union
  {
    /* Character code for character glyphs (type == CHAR_GLYPH).  */
    unsigned ch;

    /* Composition ID for composition glyphs (type == COMPOSITION_GLYPH)  */
    unsigned cmp_id;

    /* Image ID for image glyphs (type == IMAGE_GLYPH).  */
    unsigned img_id;

    /* Sub-structure for type == STRETCH_GLYPH.  */
    struct
    {
      /* The height of the glyph.  */
      unsigned height  : 16;

      /* The ascent of the glyph.  */
      unsigned ascent  : 16;
    }
    stretch;
    
    /* Used to compare all bit-fields above in one step.  */
    unsigned val;
  } u;
};


/* Is GLYPH a space?  */

#define CHAR_GLYPH_SPACE_P(GLYPH) \
     (GLYPH_FROM_CHAR_GLYPH ((GLYPH)) == SPACEGLYPH)

/* Are glyphs *X and *Y displayed equal?  */
     
#define GLYPH_EQUAL_P(X, Y)					\
     ((X)->type == (Y)->type					\
      && (X)->u.val == (Y)->u.val				\
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
     while (0)

/* Fill a character type glyph GLYPH from a glyph typedef FROM as
   defined in lisp.h.  */
     
#define SET_CHAR_GLYPH_FROM_GLYPH(GLYPH, FROM)			\
     SET_CHAR_GLYPH ((GLYPH),					\
	 	     FAST_GLYPH_CHAR ((FROM)),			\
		     FAST_GLYPH_FACE ((FROM)),			\
		     0)

/* Construct a glyph code from a character glyph GLYPH.  If the
   character is multibyte, return -1 as we can't use glyph table for a
   multibyte character.  */
     
#define GLYPH_FROM_CHAR_GLYPH(GLYPH)				\
  ((GLYPH).u.ch < 256						\
   ? ((GLYPH).u.ch | ((GLYPH).face_id << CHARACTERBITS))	\
   : -1)

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
  int nglyphs;

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
   i.e. frame glyph matrices and window glyph matrices share the same
   glyph memory which is allocated in form of a glyph_pool structure.
   Glyph rows in such a window matrix are slices of frame matrix rows.

   2. Free-standing window glyph matrices managing their own glyph
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
  int rows_allocated;

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
     window_left_x is the value of W->left, window_top_y the value of
     W->top, window_height and window_width are width and height of W,
     as returned by window_box, and window_vscroll is the value of
     W->vscroll at the time the matrix was last adjusted.  Only set
     for window-based redisplay.  */
  int window_left_x, window_top_y, window_height, window_width, window_vscroll;

  /* Number of glyphs reserved for left and right marginal areas when
     the matrix was last adjusted.  */
  int left_margin_glyphs, right_margin_glyphs;

  /* Flag indicating that scrolling should not be tried in
     update_window.  This flag is set by functions like try_window_id
     which do their own scrolling.  */
  unsigned no_scrolling_p : 1;

  /* Non-zero means window displayed in this matrix has a top mode
     line.  */
  unsigned header_line_p : 1;

#ifdef GLYPH_DEBUG
  /* A string identifying the method used to display the matrix.  */
  char method[512];
#endif

  /* The buffer this matrix displays.  Set in
     mark_window_display_accurate_1.  */
  struct buffer *buffer;

  /* Values of BEGV and ZV as of last redisplay.  Set in
     mark_window_display_accurate_1.  */
  int begv, zv;
};


/* Check that glyph pointers stored in glyph rows of MATRIX are okay.
   This aborts if any pointer is found twice.  */

#if GLYPH_DEBUG
void check_matrix_pointer_lossage P_ ((struct glyph_matrix *));
#define CHECK_MATRIX(MATRIX) check_matrix_pointer_lossage ((MATRIX))
#else
#define CHECK_MATRIX(MATRIX) (void) 0
#endif



/***********************************************************************
			     Glyph Rows
 ***********************************************************************/

/* Area in window glyph matrix.  If values are added or removed, the
   function mark_object in alloc.c has to be changed.  */

enum glyph_row_area
{
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
   glyph structure array.  */

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

  /* Number of glyphs actually filled in areas.  */
  short used[LAST_AREA];

  /* Window-relative x and y-position of the top-left corner of this
     row.  If y < 0, this means that abs (y) pixels of the row are
     invisible because it is partially visible at the top of a window.
     If x < 0, this means that abs (x) pixels of the first glyph of
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

  /* Hash code.  This hash code is available as soon as the row
     is constructed, i.e. after a call to display_line.  */
  unsigned hash;

  /* First position in this row.  This is the text position, including
     overlay position information etc, where the display of this row
     started, and can thus be less the position of the first glyph
     (e.g. due to invisible text or horizontal scrolling).  */
  struct display_pos start;

  /* Text position at the end of this row.  This is the position after
     the last glyph on this row.  It can be greater than the last
     glyph position + 1, due to truncation, invisible text etc.  In an
     up-to-date display, this should always be equal to the start
     position of the next row.  */
  struct display_pos end;

  /* In a desired matrix, 1 means that this row must be updated.  In a
     current matrix, 0 means that the row has been invalidated, i.e.
     the row's contents do not agree with what is visible on the
     screen.  */
  unsigned enabled_p : 1;

  /* 1 means row displays a text line that is truncated on the left or
     right side.  */
  unsigned truncated_on_left_p : 1;
  unsigned truncated_on_right_p : 1;

  /* 1 means the overlay arrow is on this line.  */
  unsigned overlay_arrow_p : 1;
  
  /* 1 means that this row displays a continued line, i.e. it has a
     continuation mark at the right side.  */
  unsigned continued_p : 1;

  /* 0 means that this row does not contain any text, i.e. it is
     a blank line at the window and buffer end.  */
  unsigned displays_text_p : 1;

  /* 1 means that this line ends at ZV.  */
  unsigned ends_at_zv_p : 1;

  /* 1 means the face of the last glyph in the text area is drawn to
     the right end of the window.  This flag is used in
     update_text_area to optimize clearing to the end of the area.  */
  unsigned fill_line_p : 1;

  /* Non-zero means display a bitmap on X frames indicating that this
     line contains no text and ends in ZV.  */
  unsigned indicate_empty_line_p : 1;

  /* 1 means this row contains glyphs that overlap each other because
     of lbearing or rbearing.  */
  unsigned contains_overlapping_glyphs_p : 1;

  /* 1 means this row is as wide as the window it is displayed in, including
     scroll bars, fringes, and internal borders.  This also
     implies that the row doesn't have marginal areas.  */
  unsigned full_width_p : 1;

  /* Non-zero means row is a mode or header-line.  */
  unsigned mode_line_p : 1;

  /* 1 in a current row means this row is overlapped by another row.  */
  unsigned overlapped_p : 1;

  /* 1 means this line ends in the middle of a character consisting
     of more than one glyph.  Some glyphs have been put in this row,
     the rest are put in rows below this one.  */
  unsigned ends_in_middle_of_char_p : 1;
  
  /* 1 means this line starts in the middle of a character consisting
     of more than one glyph.  Some glyphs have been put in the
     previous row, the rest are put in this row.  */
  unsigned starts_in_middle_of_char_p : 1;

  /* 1 in a current row means this row overlaps others.  */
  unsigned overlapping_p : 1;

  /* 1 means some glyphs in this row are displayed in mouse-face.  */
  unsigned mouse_face_p : 1;

  /* 1 means this row was ended by a newline from a string.  */
  unsigned ends_in_newline_from_string_p : 1;

  /* Continuation lines width at the start of the row.  */
  int continuation_lines_width;
};


/* Get a pointer to row number ROW in matrix MATRIX.  If GLYPH_DEBUG
   is defined to a non-zero value, the function matrix_row checks that
   we don't try to access rows that are out of bounds.  */

#if GLYPH_DEBUG
struct glyph_row *matrix_row P_ ((struct glyph_matrix *, int));
#define MATRIX_ROW(MATRIX, ROW)   matrix_row ((MATRIX), (ROW))
#else
#define MATRIX_ROW(MATRIX, ROW)	  ((MATRIX)->rows + (ROW))
#endif

/* Return a pointer to the row reserved for the mode line in MATRIX.  
   Row MATRIX->nrows - 1 is always reserved for the mode line.  */

#define MATRIX_MODE_LINE_ROW(MATRIX) \
     ((MATRIX)->rows + (MATRIX)->nrows - 1)

/* Return a pointer to the row reserved for the header line in MATRIX.
   This is always the first row in MATRIX because that's the only
   way that works in frame-based redisplay.  */

#define MATRIX_HEADER_LINE_ROW(MATRIX) (MATRIX)->rows

/* Return a pointer to first row in MATRIX used for text display.  */

#define MATRIX_FIRST_TEXT_ROW(MATRIX) \
     ((MATRIX)->rows->mode_line_p ? (MATRIX)->rows + 1 : (MATRIX)->rows)

/* Return a pointer to the first glyph in the text area of a row.
   MATRIX is the glyph matrix accessed, and ROW is the row index in
   MATRIX.  */

#define MATRIX_ROW_GLYPH_START(MATRIX, ROW) \
     (MATRIX_ROW ((MATRIX), (ROW))->glyphs[TEXT_AREA])

/* Return the number of used glyphs in the text area of a row.  */
     
#define MATRIX_ROW_USED(MATRIX, ROW) \
     (MATRIX_ROW ((MATRIX), (ROW))->used[TEXT_AREA])

/* Return the character/ byte position at which the display of ROW
   starts.  */
     
#define MATRIX_ROW_START_CHARPOS(ROW) ((ROW)->start.pos.charpos)
#define MATRIX_ROW_START_BYTEPOS(ROW) ((ROW)->start.pos.bytepos)

/* Return the character/ byte position at which ROW ends.  */
     
#define MATRIX_ROW_END_CHARPOS(ROW) ((ROW)->end.pos.charpos)
#define MATRIX_ROW_END_BYTEPOS(ROW) ((ROW)->end.pos.bytepos)

/* Return the vertical position of ROW in MATRIX.  */
     
#define MATRIX_ROW_VPOS(ROW, MATRIX) ((ROW) - (MATRIX)->rows)

/* Return the last glyph row + 1 in MATRIX on window W reserved for
   text.  If W has a mode line, the last row in the matrix is reserved
   for it.  */
     
#define MATRIX_BOTTOM_TEXT_ROW(MATRIX, W)		\
     ((MATRIX)->rows					\
      + (MATRIX)->nrows					\
      - (WINDOW_WANTS_MODELINE_P ((W)) ? 1 : 0))

/* Non-zero if the face of the last glyph in ROW's text area has
   to be drawn to the end of the text area.  */
     
#define MATRIX_ROW_EXTENDS_FACE_P(ROW) ((ROW)->fill_line_p)

/* Set and query the enabled_p flag of glyph row ROW in MATRIX.  */
     
#define SET_MATRIX_ROW_ENABLED_P(MATRIX, ROW, VALUE) \
     (MATRIX_ROW ((MATRIX), (ROW))->enabled_p = (VALUE) != 0)
     
#define MATRIX_ROW_ENABLED_P(MATRIX, ROW) \
     (MATRIX_ROW ((MATRIX), (ROW))->enabled_p)

/* Non-zero if ROW displays text.  Value is non-zero if the row is
   blank but displays a line end.  */
     
#define MATRIX_ROW_DISPLAYS_TEXT_P(ROW) ((ROW)->displays_text_p)

/* Non-zero if ROW is not completely visible in window W.  */
     
#define MATRIX_ROW_PARTIALLY_VISIBLE_P(ROW)	\
     ((ROW)->height != (ROW)->visible_height)

/* Non-zero if ROW is partially visible at the top of window W.  */
     
#define MATRIX_ROW_PARTIALLY_VISIBLE_AT_TOP_P(W, ROW)		\
     (MATRIX_ROW_PARTIALLY_VISIBLE_P ((ROW))			\
      && (ROW)->y < WINDOW_DISPLAY_HEADER_LINE_HEIGHT ((W)))

/* Non-zero if ROW is partially visible at the bottom of window W.  */
     
#define MATRIX_ROW_PARTIALLY_VISIBLE_AT_BOTTOM_P(W, ROW)		      \
     (MATRIX_ROW_PARTIALLY_VISIBLE_P ((ROW))				      \
      && (ROW)->y + (ROW)->height > WINDOW_DISPLAY_HEIGHT_NO_MODE_LINE ((W)))

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
     ((ROW)->end.dpvec_index >= 0			\
      || (ROW)->end.overlay_string_index >= 0		\
      || (ROW)->ends_in_middle_of_char_p)

/* Non-zero if ROW ends in the middle of an overlay string.  */

#define MATRIX_ROW_ENDS_IN_OVERLAY_STRING_P(ROW) \
     ((ROW)->end.overlay_string_index >= 0)

/* Non-zero if ROW starts in the middle of a character.  See above.  */
     
#define MATRIX_ROW_STARTS_IN_MIDDLE_OF_CHAR_P(ROW)	\
     ((ROW)->start.dpvec_index >= 0			\
      || (ROW)->starts_in_middle_of_char_p		\
      || ((ROW)->start.overlay_string_index >= 0	\
	  && (ROW)->start.string_pos.charpos > 0))

/* Non-zero means ROW overlaps its predecessor.  */

#define MATRIX_ROW_OVERLAPS_PRED_P(ROW) \
     ((ROW)->phys_ascent > (ROW)->ascent)

/* Non-zero means ROW overlaps its successor.  */

#define MATRIX_ROW_OVERLAPS_SUCC_P(ROW)		\
      ((ROW)->phys_height - (ROW)->phys_ascent	\
       > (ROW)->height - (ROW)->ascent)

/* Non-zero means that fonts have been loaded since the last glyph
   matrix adjustments.  The function redisplay_internal adjusts glyph
   matrices when this flag is non-zero.  */

extern int fonts_changed_p;

/* A glyph for a space.  */

extern struct glyph space_glyph;

/* Frame being updated by update_window/update_frame.  */

extern struct frame *updating_frame;

/* Window being updated by update_window.  This is non-null as long as
   update_window has not finished, and null otherwise.  It's role is
   analogous to updating_frame.  */

extern struct window *updated_window;

/* Glyph row and area updated by update_window_line.  */

extern struct glyph_row *updated_row;
extern int updated_area;

/* Non-zero means reading single-character input with prompt so put
   cursor on mini-buffer after the prompt.  Positive means at end of
   text in echo area; negative means at beginning of line.  */

extern int cursor_in_echo_area;

/* Non-zero means last display completed.  Zero means it was
   preempted.  */

extern int display_completed;

/* Non-zero means redisplay has been performed directly (see also
   direct_output_for_insert and direct_output_forward_char), so that
   no further updating has to be performed.  The function
   redisplay_internal checks this flag, and does nothing but reset it
   to zero if it is non-zero.  */

extern int redisplay_performed_directly_p;

/* A temporary storage area, including a row of glyphs.  Initialized
   in xdisp.c.  Used for various purposes, as an example see
   direct_output_for_insert.  */

extern struct glyph_row scratch_glyph_row;



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

/* Return the current height of the mode line of window W.  If not
   known from current_mode_line_height, look at W's current glyph
   matrix, or return a default based on the height of the font of the
   face `mode-line'.  */

#define CURRENT_MODE_LINE_HEIGHT(W)				\
     (current_mode_line_height >= 0				\
      ? current_mode_line_height				\
      : (MATRIX_MODE_LINE_HEIGHT ((W)->current_matrix)		\
	 ? MATRIX_MODE_LINE_HEIGHT ((W)->current_matrix)	\
	 : estimate_mode_line_height (XFRAME ((W)->frame),	\
				      CURRENT_MODE_LINE_FACE_ID (W))))

/* Return the current height of the header line of window W.  If not
   known from current_header_line_height, look at W's current glyph
   matrix, or return an estimation based on the height of the font of
   the face `header-line'.  */

#define CURRENT_HEADER_LINE_HEIGHT(W)				\
      (current_header_line_height >= 0				\
       ? current_header_line_height				\
       : (MATRIX_HEADER_LINE_HEIGHT ((W)->current_matrix)	\
	  ? MATRIX_HEADER_LINE_HEIGHT ((W)->current_matrix)	\
	  : estimate_mode_line_height (XFRAME ((W)->frame),	\
				       HEADER_LINE_FACE_ID)))

/* Return the height of the desired mode line of window W.  */

#define DESIRED_MODE_LINE_HEIGHT(W) \
     MATRIX_MODE_LINE_HEIGHT ((W)->desired_matrix)

/* Return the height of the desired header line of window W.  */

#define DESIRED_HEADER_LINE_HEIGHT(W) \
     MATRIX_HEADER_LINE_HEIGHT ((W)->desired_matrix)

/* Like FRAME_INTERNAL_BORDER_WIDTH but checks whether frame F is a
   window-system frame.  */

#define FRAME_INTERNAL_BORDER_WIDTH_SAFE(F) \
     (FRAME_WINDOW_P (F) ? FRAME_INTERNAL_BORDER_WIDTH (F) : 0)

/* Width of display region of window W.  For terminal frames, this
   equals the width of W since there are no vertical scroll bars.  For
   window system frames, the value has to be corrected by the pixel
   width of vertical scroll bars, and fringes.  */

#define WINDOW_DISPLAY_PIXEL_WIDTH(W)					\
     (((XFASTINT ((W)->width)						\
        - FRAME_SCROLL_BAR_WIDTH (XFRAME (WINDOW_FRAME ((W))))		\
	- FRAME_FRINGE_COLS (XFRAME (WINDOW_FRAME ((W)))))		\
       * CANON_X_UNIT (XFRAME (WINDOW_FRAME ((W))))))

/* Height of the display region of W, including a mode line, if any.  */
     
#define WINDOW_DISPLAY_PIXEL_HEIGHT(W)					\
     (XFASTINT ((W)->height)						\
      * CANON_Y_UNIT (XFRAME (WINDOW_FRAME ((W)))))

/* Height in pixels of the mode line.  May be zero if W doesn't have a
   mode line.  */
     
#define WINDOW_DISPLAY_MODE_LINE_HEIGHT(W)	\
     (WINDOW_WANTS_MODELINE_P ((W))		\
      ? CURRENT_MODE_LINE_HEIGHT (W)		\
      : 0)

/* Height in pixels of the header line.  Zero if W doesn't have a header
   line.  */
     
#define WINDOW_DISPLAY_HEADER_LINE_HEIGHT(W)	\
     (WINDOW_WANTS_HEADER_LINE_P ((W))		\
      ? CURRENT_HEADER_LINE_HEIGHT (W)		\
      : 0)

/* Pixel height of window W without mode line.  */
     
#define WINDOW_DISPLAY_HEIGHT_NO_MODE_LINE(W)	\
     (WINDOW_DISPLAY_PIXEL_HEIGHT ((W))		\
      - WINDOW_DISPLAY_MODE_LINE_HEIGHT ((W)))

/* Pixel height of window W without mode and header line.  */
     
#define WINDOW_DISPLAY_TEXT_HEIGHT(W)		\
     (WINDOW_DISPLAY_PIXEL_HEIGHT ((W))		\
      - WINDOW_DISPLAY_MODE_LINE_HEIGHT ((W))	\
      - WINDOW_DISPLAY_HEADER_LINE_HEIGHT ((W)))

/* Left edge of W in pixels relative to its frame.  */
     
#define WINDOW_DISPLAY_LEFT_EDGE_PIXEL_X(W)				\
     (FRAME_INTERNAL_BORDER_WIDTH_SAFE (XFRAME (WINDOW_FRAME ((W))))	\
      + (WINDOW_LEFT_MARGIN ((W))					\
         * CANON_X_UNIT (XFRAME (WINDOW_FRAME ((W)))))			\
      + FRAME_LEFT_FRINGE_WIDTH (XFRAME (WINDOW_FRAME ((W)))))

/* Right edge of window W in pixels, relative to its frame.  */
     
#define WINDOW_DISPLAY_RIGHT_EDGE_PIXEL_X(W)		\
     (WINDOW_DISPLAY_LEFT_EDGE_PIXEL_X ((W))		\
      + WINDOW_DISPLAY_PIXEL_WIDTH ((W)))

/* Top edge of W in pixels relative to its frame.  */
     
#define WINDOW_DISPLAY_TOP_EDGE_PIXEL_Y(W)				\
     (FRAME_INTERNAL_BORDER_WIDTH_SAFE (XFRAME (WINDOW_FRAME ((W))))	\
      + (XFASTINT ((W)->top)						\
         * CANON_Y_UNIT (XFRAME (WINDOW_FRAME ((W))))))

/* Bottom edge of window W relative to its frame.  */
     
#define WINDOW_DISPLAY_BOTTOM_EDGE_PIXEL_Y(W)		\
     (WINDOW_DISPLAY_TOP_EDGE_PIXEL_Y ((W))		\
      + WINDOW_DISPLAY_PIXEL_HEIGHT ((W)))
     
/* Convert window W relative pixel X to frame pixel coordinates.  */
     
#define WINDOW_TO_FRAME_PIXEL_X(W, X) \
     ((X) + WINDOW_DISPLAY_LEFT_EDGE_PIXEL_X ((W)))

/* Convert window W relative pixel Y to frame pixel coordinates.  */
     
#define WINDOW_TO_FRAME_PIXEL_Y(W, Y) \
     ((Y) + WINDOW_DISPLAY_TOP_EDGE_PIXEL_Y ((W)))

/* Convert frame relative pixel X to window relative pixel X.  */
     
#define FRAME_TO_WINDOW_PIXEL_X(W, X) \
     ((X) - WINDOW_DISPLAY_LEFT_EDGE_PIXEL_X ((W)))

/* Convert frame relative pixel Y to window relative pixel Y.  */
     
#define FRAME_TO_WINDOW_PIXEL_Y(W, Y) \
     ((Y) - WINDOW_DISPLAY_TOP_EDGE_PIXEL_Y ((W)))

/* Width of left margin area in pixels.  */
     
#define WINDOW_DISPLAY_LEFT_AREA_PIXEL_WIDTH(W)		\
     (NILP ((W)->left_margin_width)			\
      ? 0						\
      : (XINT ((W)->left_margin_width)			\
	 * CANON_X_UNIT (XFRAME (WINDOW_FRAME ((W))))))
	    
/* Width of right marginal area in pixels.  */
     
#define WINDOW_DISPLAY_RIGHT_AREA_PIXEL_WIDTH(W)	\
     (NILP ((W)->right_margin_width)			\
      ? 0						\
      : (XINT ((W)->right_margin_width)			\
	 * CANON_X_UNIT (XFRAME (WINDOW_FRAME ((W))))))

/* Width of text area in pixels.  */
     
#define WINDOW_DISPLAY_TEXT_AREA_PIXEL_WIDTH(W)		\
     (WINDOW_DISPLAY_PIXEL_WIDTH ((W))			\
      - WINDOW_DISPLAY_LEFT_AREA_PIXEL_WIDTH ((W))	\
      - WINDOW_DISPLAY_RIGHT_AREA_PIXEL_WIDTH ((W)))

/* Convert a text area relative x-position in window W to frame X
   pixel coordinates.  */

#define WINDOW_TEXT_TO_FRAME_PIXEL_X(W, X)		\
     (WINDOW_TO_FRAME_PIXEL_X ((W), (X))		\
      + WINDOW_DISPLAY_LEFT_AREA_PIXEL_WIDTH ((W)))

/* Translate an x-position relative to AREA in window W to frame pixel
   coordinates.  */

#define WINDOW_AREA_TO_FRAME_PIXEL_X(W, AREA, X)	\
     (WINDOW_TO_FRAME_PIXEL_X ((W), (X))		\
      + (((AREA) > LEFT_MARGIN_AREA)			\
	 ? WINDOW_DISPLAY_LEFT_AREA_PIXEL_WIDTH ((W))	\
	 : 0)						\
      + (((AREA) > TEXT_AREA)				\
	 ? WINDOW_DISPLAY_TEXT_AREA_PIXEL_WIDTH ((W))	\
	 : 0))

/* Return the pixel width of AREA in W.  */

#define WINDOW_AREA_PIXEL_WIDTH(W, AREA)		\
     (((AREA) == TEXT_AREA)				\
      ? WINDOW_DISPLAY_TEXT_AREA_PIXEL_WIDTH ((W))	\
      : (((AREA) == LEFT_MARGIN_AREA)			\
	 ? WINDOW_DISPLAY_LEFT_AREA_PIXEL_WIDTH ((W))	\
	 : WINDOW_DISPLAY_RIGHT_AREA_PIXEL_WIDTH ((W))))
     
/* Value is non-zero if window W wants a mode line.  */

#define WINDOW_WANTS_MODELINE_P(W)					\
     (!MINI_WINDOW_P ((W))						\
      && !(W)->pseudo_window_p						\
      && FRAME_WANTS_MODELINE_P (XFRAME (WINDOW_FRAME ((W))))		\
      && BUFFERP ((W)->buffer)						\
      && !NILP (XBUFFER ((W)->buffer)->mode_line_format)		\
      && XFASTINT ((W)->height) > 1)

/* Value is non-zero if window W wants a header line.  */

#define WINDOW_WANTS_HEADER_LINE_P(W)					\
     (!MINI_WINDOW_P ((W))						\
      && !(W)->pseudo_window_p						\
      && FRAME_WANTS_MODELINE_P (XFRAME (WINDOW_FRAME ((W))))		\
      && BUFFERP ((W)->buffer)						\
      && !NILP (XBUFFER ((W)->buffer)->header_line_format)		\
      && XFASTINT ((W)->height) > 1 + !NILP (XBUFFER ((W)->buffer)->mode_line_format))

     
/***********************************************************************
				Faces
 ***********************************************************************/
     
/* Indices of face attributes in Lisp face vectors.  Slot zero is the
   symbol `face'.  */

enum lface_attribute_index
{
  LFACE_FAMILY_INDEX = 1,
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
  LFACE_AVGWIDTH_INDEX,
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


/* Structure describing a realized face.

   For each Lisp face, 0..N realized faces can exist for different
   frames and different charsets.  Realized faces are built from Lisp
   faces and text properties/overlays by merging faces and adding
   unspecified attributes from the `default' face.  */

struct face
{
  /* The id of this face.  The id equals the index of this face in the
     vector faces_by_id of its face cache.  */
  int id;

#ifdef HAVE_WINDOW_SYSTEM
  
  /* If non-zero, this is a GC that we can use without modification for
     drawing the characters in this face.  */
  GC gc;
  
  /* Font used for this face, or null if the font could not be loaded
     for some reason.  This points to a `font' slot of a struct
     font_info, and we should not call XFreeFont on it because the
     font may still be used somewhere else.  */
  XFontStruct *font;

  /* Background stipple or bitmap used for this face.  This is
     an id as returned from load_pixmap.  */
  int stipple;

#else /* not HAVE_WINDOW_SYSTEM */

  /* Dummy.  */
  int stipple;

#endif /* not HAVE_WINDOW_SYSTEM */

  /* Pixel value of foreground color for X frames.  Color index
     for tty frames.  */
  unsigned long foreground;
  
  /* Pixel value or color index of background color.  */
  unsigned long background;

  /* Pixel value or color index of underline color.  */
  unsigned long underline_color;

  /* Pixel value or color index of overlined, strike-through, or box
     color.  */
  unsigned long overline_color;
  unsigned long strike_through_color;
  unsigned long box_color;

  /* The font's name.  This points to a `name' of a font_info, and it
     must not be freed.  */
  char *font_name;

  /* Font info ID for this face's font.  An ID is stored here because
     pointers to font_info structures may change.  The reason is that
     they are pointers into a font table vector that is itself
     reallocated.  */
  int font_info_id;

  /* Fontset ID if this face uses a fontset, or -1.  This is only >= 0
     if the face was realized for a composition sequence.
     Otherwise, a specific font is loaded from the set of fonts
     specified by the fontset given by the family attribute of the face.  */
  int fontset;
  
  /* Pixmap width and height.  */
  unsigned int pixmap_w, pixmap_h;
  
  /* Non-zero means characters in this face have a box that thickness
     around them.  If it is negative, the absolute value indicates the
     thickness, and the horizontal lines of box (top and bottom) are
     drawn inside of characters glyph area.  The vertical lines of box
     (left and right) are drawn as the same way as the case that this
     value is positive.  */
  int box_line_width;

  /* Type of box drawn.  A value of FACE_NO_BOX means no box is drawn
     around text in this face.  A value of FACE_SIMPLE_BOX means a box
     of width box_line_width is drawn in color box_color.  A value of
     FACE_RAISED_BOX or FACE_SUNKEN_BOX means a 3D box is drawn with
     shadow colors derived from the background color of the face.  */
  enum face_box_type box;

  /* If `box' above specifies a 3D type, 1 means use box_color for
     drawing shadows.  */
  unsigned use_box_color_for_shadows_p : 1;

  /* The Lisp face attributes this face realizes.  All attributes
     in this vector are non-nil.  */
  Lisp_Object lface[LFACE_VECTOR_SIZE];

  /* The hash value of this face.  */
  unsigned hash;

  /* The charset for which this face was realized if it was realized
     for use in multibyte text.  If fontset >= 0, this is the charset
     of the first character of the composition sequence.  A value of
     charset < 0 means the face was realized for use in unibyte text
     where the idea of Emacs charsets isn't applicable.  */
  int charset;

  /* Non-zero if text in this face should be underlined, overlined,
     strike-through or have a box drawn around it.  */
  unsigned underline_p : 1;
  unsigned overline_p : 1;
  unsigned strike_through_p : 1;

  /* 1 means that the colors specified for this face could not be
     loaded, and were replaced by default colors, so they shouldn't be
     freed.  */
  unsigned foreground_defaulted_p : 1;
  unsigned background_defaulted_p : 1;

  /* 1 means that either no color is specified for underlining or that
     the specified color couldn't be loaded.  Use the foreground
     color when drawing in that case. */
  unsigned underline_defaulted_p : 1; 

  /* 1 means that either no color is specified for the corresponding
     attribute or that the specified color couldn't be loaded.
     Use the foreground color when drawing in that case. */
  unsigned overline_color_defaulted_p : 1;
  unsigned strike_through_color_defaulted_p : 1;
  unsigned box_color_defaulted_p : 1;

  /* TTY appearances.  Blinking is not yet implemented.  Colors are
     found in `lface' with empty color string meaning the default
     color of the TTY.  */
  unsigned tty_bold_p : 1;
  unsigned tty_dim_p : 1;
  unsigned tty_underline_p : 1;
  unsigned tty_alt_charset_p : 1;
  unsigned tty_reverse_p : 1;
  unsigned tty_blinking_p : 1;

  /* 1 means that colors of this face may not be freed because they
     have been copied bitwise from a base face (see
     realize_x_face).  */
  unsigned colors_copied_bitwise_p : 1;

  /* Next and previous face in hash collision list of face cache.  */
  struct face *next, *prev;

  /* If this face is for ASCII characters, this points this face
     itself.  Otherwise, this points a face for ASCII characters.  */
  struct face *ascii_face;
};


/* Color index indicating that face uses a terminal's default color.  */

#define FACE_TTY_DEFAULT_COLOR ((unsigned long) -1)

/* Color index indicating that face uses an unknown foreground color.  */

#define FACE_TTY_DEFAULT_FG_COLOR ((unsigned long) -2)

/* Color index indicating that face uses an unknown background color.  */

#define FACE_TTY_DEFAULT_BG_COLOR ((unsigned long) -3)

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
  BASIC_FACE_ID_SENTINEL
};


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
  int size, used;

  /* Flag indicating that attributes of the `menu' face have been
     changed.  */
  unsigned menu_face_changed_p : 1;
};


/* Prepare face FACE for use on frame F.  This must be called before
   using X resources of FACE.  */

#define PREPARE_FACE_FOR_DISPLAY(F, FACE)	\
     if ((FACE)->gc == 0)			\
       prepare_face_for_display ((F), (FACE));	\
     else					\
       (void) 0

/* Return a pointer to the face with ID on frame F, or null if such a
   face doesn't exist.  */

#define FACE_FROM_ID(F, ID)				\
     (((unsigned) (ID) < FRAME_FACE_CACHE (F)->used)	\
      ? FRAME_FACE_CACHE (F)->faces_by_id[ID]		\
      : NULL)

#ifdef HAVE_WINDOW_SYSTEM

/* Non-zero if FACE is suitable for displaying character CHAR.  */

#define FACE_SUITABLE_FOR_CHAR_P(FACE, CHAR)	\
  (SINGLE_BYTE_CHAR_P (CHAR)			\
   ? (FACE) == (FACE)->ascii_face		\
   : face_suitable_for_char_p ((FACE), (CHAR)))

/* Return the id of the realized face on frame F that is like the face
   with id ID but is suitable for displaying character CHAR.
   This macro is only meaningful for multibyte character CHAR.  */
   
#define FACE_FOR_CHAR(F, FACE, CHAR)	\
  (SINGLE_BYTE_CHAR_P (CHAR)		\
   ? (FACE)->ascii_face->id		\
   : face_for_char ((F), (FACE), (CHAR)))

#else /* not HAVE_WINDOW_SYSTEM */

#define FACE_SUITABLE_FOR_CHAR_P(FACE, CHAR) 1
#define FACE_FOR_CHAR(F, FACE, CHAR) ((FACE)->id)

#endif /* not HAVE_WINDOW_SYSTEM */

/* Non-zero means face attributes have been changed since the last
   redisplay.  Used in redisplay_internal.  */

extern int face_change_count;




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

  /* A composition sequence.  */
  IT_COMPOSITION,

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
  IT_CONTINUATION
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


struct it
{
  /* The window in which we iterate over current_buffer (or a string).  */
  Lisp_Object window;
  struct window *w;

  /* The window's frame.  */
  struct frame *f;

  /* Function to call to load this structure with the next display
     element.  */
  int (* method) P_ ((struct it *it));

  /* The next position at which to check for face changes, invisible
     text, overlay strings, end of text etc., which see.  */
  int stop_charpos;

  /* Maximum string or buffer position + 1.  ZV when iterating over
     current_buffer.  */
  int end_charpos;

  /* C string to iterate over.  Non-null means get characters from
     this string, otherwise characters are read from current_buffer
     or it->string.  */
  unsigned char *s;

  /* Number of characters in the string (s, or it->string) we iterate
     over.  */
  int string_nchars;

  /* Start and end of a visible region; -1 if the region is not
     visible in the window.  */
  int region_beg_charpos, region_end_charpos;

  /* Position at which redisplay end trigger functions should be run.  */
  int redisplay_end_trigger_charpos;

  /* 1 means multibyte characters are enabled.  */
  unsigned multibyte_p : 1;

  /* 1 means window has a mode line at its top.  */
  unsigned header_line_p : 1;

  /* 1 means `string' is the value of a `display' property.
     Don't handle some `display' properties in these strings.  */
  unsigned string_from_display_prop_p : 1;

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
     means that no such character is involved.  */
  int dpvec_char_len;

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

  /* Current buffer or string position of the iterator, including
     position in overlay strings etc.  */
  struct display_pos current;

  /* Vector of overlays to process.  Overlay strings are processed
     OVERLAY_STRING_CHUNK_SIZE at a time.  */
#define OVERLAY_STRING_CHUNK_SIZE 3
  Lisp_Object overlay_strings[OVERLAY_STRING_CHUNK_SIZE];

  /* Total number of overlay strings to process.  This can be >
     OVERLAY_STRING_CHUNK_SIZE.  */
  int n_overlay_strings;

  /* If non-nil, a Lisp string being processed.  If
     current.overlay_string_index >= 0, this is an overlay string from
     pos.  */
  Lisp_Object string;

  /* Stack of saved values.  New entries are pushed when we begin to
     process an overlay string or a string from a `glyph' property.
     Entries are popped when we return to deliver display elements
     from what we previously had.  */
  struct iterator_stack_entry
  {
    int stop_charpos;
    int face_id;
    Lisp_Object string;
    struct display_pos pos;
    int end_charpos;
    int string_nchars;
    enum glyph_row_area area;
    unsigned multibyte_p : 1;
    unsigned string_from_display_prop_p : 1;
    unsigned display_ellipsis_p : 1;
    Lisp_Object space_width;
    short voffset;
    Lisp_Object font_height;
  }
  stack[2];

  /* Stack pointer.  */
  int sp;
  
  /* Setting of buffer-local variable selective-display-ellipsis.  */
  unsigned selective_display_ellipsis_p : 1;

  /* 1 means control characters are translated into the form `^C'
     where the `^' can be replaced by a display table entry.  */
  unsigned ctl_arrow_p : 1;

  /* -1 means selective display hides everything between a \r and the
     next newline; > 0 means hide lines indented more than that value.  */
  int selective;

  /* An enumeration describing what the next display element is
     after a call to get_next_display_element.  */
  enum display_element_type what;

  /* Face to use.  */
  int face_id;

  /* Non-zero means that the current face has a box.  */
  unsigned face_box_p : 1;

  /* Non-null means that the current character is the first in a run
     of characters with box face.  */
  unsigned start_of_box_run_p : 1;
  
  /* Non-zero means that the current character is the last in a run
     of characters with box face.  */
  unsigned end_of_box_run_p : 1;

  /* 1 means overlay strings at end_charpos have been processed.  */
  unsigned overlay_strings_at_end_processed_p : 1;

  /* 1 means the actual glyph is not available in the current
     system.  */
  unsigned glyph_not_available_p : 1;

  /* 1 means the next line in display_line continues a character
     consisting of more than one glyph, and some glyphs of this
     character have been put on the previous line.  */
  unsigned starts_in_middle_of_char_p : 1;

  /* If 1, saved_face_id contains the id of the face in front of text
     skipped due to selective display.  */
  unsigned face_before_selective_p : 1;

  /* The ID of the default face to use.  One of DEFAULT_FACE_ID,
     MODE_LINE_FACE_ID, etc, depending on what we are displaying.  */
  int base_face_id;

  /* If what == IT_CHARACTER, character and length in bytes.  This is
     a character from a buffer or string.  It may be different from
     the character displayed in case that
     unibyte_display_via_language_environment is set.

     If what == IT_COMPOSITION, the first component of a composition
     and length in bytes of the composition.  */
  int c, len;

  /* If what == IT_COMPOSITION, identification number and length in
     chars of a composition.  */
  int cmp_id, cmp_len;

  /* The character to display, possibly translated to multibyte
     if unibyte_display_via_language_environment is set.  This
     is set after x_produce_glyphs has been called.  */
  int char_to_display;

  /* If what == IT_IMAGE, the id of the image to display.  */
  int image_id;

  /* Value of the `space-width' property, if any; nil if none.  */
  Lisp_Object space_width;

  /* Computed from the value of the `raise' property.  */
  short voffset;

  /* Value of the `height' property, if any; nil if none.  */
  Lisp_Object font_height;

  /* Object and position where the current display element came from.
     Object can be a Lisp string in case the current display element
     comes from an overlay string, or it is buffer.  Position is
     a position in object.  */
  Lisp_Object object;
  struct text_pos position;

  /* 1 means lines are truncated.  */
  unsigned truncate_lines_p : 1;

  /* Number of columns per \t.  */
  short tab_width;

  /* Width in pixels of truncation and continuation glyphs.  */
  short truncation_pixel_width, continuation_pixel_width;

  /* First and last visible x-position in the display area.  If window
     is hscrolled by n columns, first_visible_x == n * CANON_X_UNIT
     (f), and last_visible_x == pixel width of W + first_visible_x.  */
  int first_visible_x, last_visible_x;

  /* Last visible y-position + 1 in the display area without a mode
     line, if the window has one.  */
  int last_visible_y;

  /* Additional space in pixels between lines (for window systems
     only.)  */
  int extra_line_spacing;

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

  /* Current y-position.  Automatically incremented by the height of
     glyph_row in move_it_to and display_line.  */
  int current_y;

  /* Current vertical matrix position, or line number.  Automatically
     incremented by move_it_to and display_line.  */
  int vpos;

  /* Horizontal matrix position reached in move_it_in_display_line.
     Only set there, not in display_line.  */
  int hpos;
};


/* Access to positions of iterator IT.  */

#define IT_CHARPOS(IT)		CHARPOS ((IT).current.pos)
#define IT_BYTEPOS(IT)		BYTEPOS ((IT).current.pos)
#define IT_STRING_CHARPOS(IT)	CHARPOS ((IT).current.string_pos)
#define IT_STRING_BYTEPOS(IT)	BYTEPOS ((IT).current.string_pos)

/* Test if IT has reached the end of its buffer or string.  This will
   only work after get_next_display_element has been called.  */

#define ITERATOR_AT_END_P(IT) ((IT)->what == IT_EOB)

/* Non-zero means IT is at the end of a line.  This is the case if it
   is either on a newline or on a carriage return and selective
   display hides the rest of the line.  */

#define ITERATOR_AT_END_OF_LINE_P(IT)			\
     ((IT)->what == IT_CHARACTER			\
      && ((IT)->c == '\n'				\
	  || ((IT)->c == '\r' && (IT)->selective)))

/* Call produce_glyphs or produce_glyphs_hook, if set.  Shortcut to
   avoid the function call overhead.  */

#define PRODUCE_GLYPHS(IT)			\
     (rif					\
      ? rif->produce_glyphs ((IT))		\
      : produce_glyphs ((IT)))

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


/* Structure holding system-dependent interface functions needed
   for window-based redisplay.  */

struct redisplay_interface
{
  /* Produce glyphs/get display metrics for the display element IT is
     loaded with.  */
  void (*produce_glyphs) P_ ((struct it *it));
  
  /* Write or insert LEN glyphs from STRING at the nominal output
     position.  */
  void (*write_glyphs) P_ ((struct glyph *string, int len));
  void (*insert_glyphs) P_ ((struct glyph *start, int len));

  /* Clear from nominal output position to X.  X < 0 means clear
     to right end of display.  */
  void (*clear_end_of_line) P_ ((int x));
  
  /* Function to call to scroll the display as described by RUN on
     window W.  */
  void (*scroll_run_hook) P_ ((struct window *w, struct run *run));

  /* Function to call after a line in a display has been completely
     updated.  Used to draw truncation marks and alike.  DESIRED_ROW
     is the desired row which has been updated.  */
  void (*after_update_window_line_hook) P_ ((struct glyph_row *desired_row));

  /* Function to call before beginning to update window W in
     window-based redisplay.  */
  void (*update_window_begin_hook) P_ ((struct window *w));

  /* Function to call after window W has been updated in window-based
     redisplay.  CURSOR_ON_P non-zero means switch cursor on.
     MOUSE_FACE_OVERWRITTEN_P non-zero means that some lines in W
     that contained glyphs in mouse-face were overwritten, so we
     have to update the mouse highlight.  */
  void (*update_window_end_hook) P_ ((struct window *w, int cursor_on_p,
				      int mouse_face_overwritten_p));
  
  /* Move cursor to row/column position VPOS/HPOS, pixel coordinates
     Y/X. HPOS/VPOS are window-relative row and column numbers and X/Y
     are window-relative pixel positions.  */
  void (*cursor_to) P_ ((int vpos, int hpos, int y, int x));

  /* Flush the display of frame F.  For X, this is XFlush.  */
  void (*flush_display) P_ ((struct frame *f));

  /* Clear the mouse hightlight in window W, if there is any.  */
  void (*clear_mouse_face) P_ ((struct window *w));

  /* Set *LEFT and *RIGHT to the left and right overhang of GLYPH on
     frame F.  */
  void (*get_glyph_overhangs) P_ ((struct glyph *glyph, struct frame *f,
				   int *left, int *right));

  /* Fix the display of AREA of ROW in window W for overlapping rows.
     This function is called from redraw_overlapping_rows after
     desired rows have been made current.  */
  void (*fix_overlapping_area) P_ ((struct window *w, struct glyph_row *row,
				    enum glyph_row_area area));
};

/* The current interface for window-based redisplay.  */

extern struct redisplay_interface *rif;

/* Hook to call in estimate_mode_line_height.  */

extern int (* estimate_mode_line_height_hook) P_ ((struct frame *,
                                                   enum face_id));


/***********************************************************************
				Images
 ***********************************************************************/

#ifdef HAVE_WINDOW_SYSTEM

/* Structure forward declarations.  */

struct image;


/* Each image format (JPEG, TIFF, ...) supported is described by
   a structure of the type below.  */

struct image_type
{
  /* A symbol uniquely identifying the image type, .e.g `jpeg'.  */
  Lisp_Object *type;

  /* Check that SPEC is a valid image specification for the given
     image type.  Value is non-zero if SPEC is valid.  */
  int (* valid_p) P_ ((Lisp_Object spec));

  /* Load IMG which is used on frame F from information contained in
     IMG->spec.  Value is non-zero if successful.  */
  int (* load) P_ ((struct frame *f, struct image *img));

  /* Free resources of image IMG which is used on frame F.  */
  void (* free) P_ ((struct frame *f, struct image *img));

  /* Next in list of all supported image types.  */
  struct image_type *next;
};


/* Structure describing an image.  Specific image formats like XBM are
   converted into this form, so that display only has to deal with
   this type of image.  */

struct image
{
  /* The time in seconds at which the image was last displayed.  Set
     in prepare_image_for_display.  */
  unsigned long timestamp;

  /* Pixmaps of the image.  */
  Pixmap pixmap, mask;

  /* Colors allocated for this image, if any.  Allocated via xmalloc.  */
  unsigned long *colors;
  int ncolors;

  /* A single `background color' for this image, for the use of anyone that
     cares about such a thing.  Only valid if the `background_valid' field
     is true.  This should generally be accessed by calling the accessor
     macro `IMAGE_BACKGROUND', which will heuristically calculate a value
     if necessary.  */
  unsigned long background;

  /* True if this image has a `transparent' background -- that is, is
     uses an image mask.  The accessor macro for this is
     `IMAGE_BACKGROUND_TRANSPARENT'.  */
  unsigned background_transparent : 1;

  /* True if the `background' and `background_transparent' fields are
     valid, respectively. */
  unsigned background_valid : 1, background_transparent_valid : 1;

  /* Width and height of the image.  */
  int width, height;

  /* These values are used for the rectangles displayed for images
     that can't be loaded.  */
#define DEFAULT_IMAGE_WIDTH 30
#define DEFAULT_IMAGE_HEIGHT 30

  /* Percent of image height used as ascent.  A value of
     CENTERED_IMAGE_ASCENT means draw the image centered on the
     line.  */
  int ascent;
#define DEFAULT_IMAGE_ASCENT 50
#define CENTERED_IMAGE_ASCENT -1

  /* Lisp specification of this image.  */
  Lisp_Object spec;

  /* Relief to draw around the image.  */
  int relief;

  /* Optional margins around the image.  This includes the relief.  */
  int hmargin, vmargin;

  /* Reference to the type of the image.  */
  struct image_type *type;

  /* 1 means that loading the image failed.  Don't try again.  */
  unsigned load_failed_p;

  /* A place for image types to store additional data.  The member
     data.lisp_val is marked during GC, so it's safe to store Lisp data
     there.  Image types should free this data when their `free'
     function is called.  */
  struct
  {
    int int_val;
    void *ptr_val;
    Lisp_Object lisp_val;
  } data;

  /* Hash value of image specification to speed up comparisons.  */
  unsigned hash;

  /* Image id of this image.  */
  int id;

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
  unsigned size;

  /* Number of images in the cache.  */
  unsigned used;

  /* Reference count (number of frames sharing this cache).  */
  int refcount;
};


/* Value is a pointer to the image with id ID on frame F, or null if
   no image with that id exists.  */

#define IMAGE_FROM_ID(F, ID)					\
     (((ID) >= 0 && (ID) < (FRAME_X_IMAGE_CACHE (F)->used))	\
      ? FRAME_X_IMAGE_CACHE (F)->images[ID]			\
      : NULL)

/* Size of bucket vector of image caches.  Should be prime.  */

#define IMAGE_CACHE_BUCKETS_SIZE 1001

#endif /* HAVE_WINDOW_SYSTEM */



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

  /* Button type.  One of nil, `:radio' or `:toggle'.  */
  TOOL_BAR_ITEM_TYPE,

  /* Help string.  */
  TOOL_BAR_ITEM_HELP,

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

/* Non-zero means raise tool-bar buttons when the mouse moves over them.  */

extern int auto_raise_tool_bar_buttons_p;

/* Margin around tool-bar buttons in pixels.  */

extern Lisp_Object Vtool_bar_button_margin;

/* Thickness of relief to draw around tool-bar buttons.  */

extern EMACS_INT tool_bar_button_relief;

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
#define TTY_CAP_BLINK		0x10
#define TTY_CAP_ALT_CHARSET	0x20


/***********************************************************************
			  Function Prototypes
 ***********************************************************************/

/* Defined in xdisp.c */

struct glyph_row *row_containing_pos P_ ((struct window *, int,
					  struct glyph_row *,
					  struct glyph_row *, int));
int string_buffer_position P_ ((struct window *, Lisp_Object, int));
int line_bottom_y P_ ((struct it *));
int display_prop_intangible_p P_ ((Lisp_Object));
void resize_echo_area_exactly P_ ((void));
int resize_mini_window P_ ((struct window *, int));
int try_window P_ ((Lisp_Object, struct text_pos));
void window_box P_ ((struct window *, int, int *, int *, int *, int *));
int window_box_height P_ ((struct window *));
int window_text_bottom_y P_ ((struct window *));
int window_box_width P_ ((struct window *, int));
int window_box_left P_ ((struct window *, int));
int window_box_right P_ ((struct window *, int));
void window_box_edges P_ ((struct window *, int, int *, int *, int *, int *));
void mark_window_display_accurate P_ ((Lisp_Object, int));
void redisplay_preserve_echo_area P_ ((int));
void set_cursor_from_row P_ ((struct window *, struct glyph_row *,
			      struct glyph_matrix *, int, int, int, int));
void init_iterator P_ ((struct it *, struct window *, int,
			int, struct glyph_row *, enum face_id));
void init_iterator_to_row_start P_ ((struct it *, struct window *,
				     struct glyph_row *));
int get_next_display_element P_ ((struct it *));
void set_iterator_to_next P_ ((struct it *, int));
void produce_glyphs P_ ((struct it *));
void produce_special_glyphs P_ ((struct it *, enum display_element_type));
void start_display P_ ((struct it *, struct window *, struct text_pos));
void move_it_to P_ ((struct it *, int, int, int, int, int));
void move_it_vertically P_ ((struct it *, int));
void move_it_vertically_backward P_ ((struct it *, int));
void move_it_by_lines P_ ((struct it *, int, int));
void move_it_past_eol P_ ((struct it *));
int frame_mode_line_height P_ ((struct frame *));
void highlight_trailing_whitespace P_ ((struct frame *, struct glyph_row *));
int tool_bar_item_info P_ ((struct frame *, struct glyph *, int *));
extern Lisp_Object Qtool_bar;
extern Lisp_Object Vshow_trailing_whitespace;
extern int mode_line_in_non_selected_windows;
extern int redisplaying_p;
extern Lisp_Object Vimage_types;
extern void add_to_log P_ ((char *, Lisp_Object, Lisp_Object));
extern int help_echo_showing_p;
extern int current_mode_line_height, current_header_line_height;
extern int cursor_in_non_selected_windows;

/* Defined in sysdep.c */

void get_frame_size P_ ((int *, int *));
void request_sigio P_ ((void));
void unrequest_sigio P_ ((void));
int tabs_safe_p P_ ((void));
void init_baud_rate P_ ((void));
void init_sigio P_ ((int));

/* Defined in xfaces.c */

#ifdef HAVE_X_WINDOWS
void x_free_colors P_ ((struct frame *, unsigned long *, int));
#endif

void update_face_from_frame_parameter P_ ((struct frame *, Lisp_Object,
					   Lisp_Object));
Lisp_Object tty_color_name P_ ((struct frame *, int));
void clear_face_cache P_ ((int));
unsigned long load_color P_ ((struct frame *, struct face *, Lisp_Object,
			      enum lface_attribute_index));
void unload_color P_ ((struct frame *, unsigned long));
int frame_update_line_height P_ ((struct frame *));
int ascii_face_of_lisp_face P_ ((struct frame *, int));
void prepare_face_for_display P_ ((struct frame *, struct face *));
int xstricmp P_ ((unsigned char *, unsigned char *));
int lookup_face P_ ((struct frame *, Lisp_Object *, int, struct face *));
int lookup_named_face P_ ((struct frame *, Lisp_Object, int));
int smaller_face P_ ((struct frame *, int, int));
int face_with_height P_ ((struct frame *, int, int));
int lookup_derived_face P_ ((struct frame *, Lisp_Object, int, int));
void init_frame_faces P_ ((struct frame *));
void free_frame_faces P_ ((struct frame *));
void recompute_basic_faces P_ ((struct frame *));
int face_at_buffer_position P_ ((struct window *, int, int, int, int *,
				 int, int));
int face_at_string_position P_ ((struct window *, Lisp_Object, int, int, int,
				 int, int *, enum face_id, int));
int compute_char_face P_ ((struct frame *, int, Lisp_Object));
void free_all_realized_faces P_ ((Lisp_Object));
extern Lisp_Object Qforeground_color, Qbackground_color;
extern char unspecified_fg[], unspecified_bg[];
void free_realized_multibyte_face P_ ((struct frame *, int));

/* Defined in xfns.c  */

#ifdef HAVE_X_WINDOWS
void gamma_correct P_ ((struct frame *, XColor *));
#endif
#ifdef WINDOWSNT
void gamma_correct P_ ((struct frame *, COLORREF *));
#endif

#ifdef HAVE_WINDOW_SYSTEM

void x_kill_gs_process P_ ((Pixmap, struct frame *));
int x_screen_planes P_ ((struct frame *));
void x_implicitly_set_name P_ ((struct frame *, Lisp_Object, Lisp_Object));
struct image_cache *make_image_cache P_ ((void));
void free_image_cache P_ ((struct frame *));
void clear_image_cache P_ ((struct frame *, int));
void forall_images_in_image_cache P_ ((struct frame *,
				       void (*) P_ ((struct image *))));
int valid_image_p P_ ((Lisp_Object));
void prepare_image_for_display P_ ((struct frame *, struct image *));
int lookup_image P_ ((struct frame *, Lisp_Object));

#ifdef HAVE_X_WINDOWS
unsigned long image_background P_ ((struct image *, struct frame *,
				    XImage *ximg));
int image_background_transparent P_ ((struct image *, struct frame *,
				      XImage *mask));
#endif /* HAVE_X_WINDOWS */

extern Lisp_Object tip_frame;
extern Window tip_window;
EXFUN (Fx_show_tip, 6);
EXFUN (Fx_hide_tip, 0);
extern void start_hourglass P_ ((void));
extern void cancel_hourglass P_ ((void));
extern int display_hourglass_p;

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


/* Defined in xmenu.c  */

int popup_activated P_ ((void));

/* Defined in dispnew.c  */

extern int inverse_video;
extern int required_matrix_width P_ ((struct window *));
extern int required_matrix_height P_ ((struct window *));
extern int estimate_mode_line_height P_ ((struct frame *, enum face_id));
extern Lisp_Object mode_line_string P_ ((struct window *, int, int, int, int *));
extern Lisp_Object marginal_area_string P_ ((struct window *, int, int, int, int *));
extern void redraw_frame P_ ((struct frame *));
extern void redraw_garbaged_frames P_ ((void));
extern void cancel_line P_ ((int, struct frame *));
extern void init_desired_glyphs P_ ((struct frame *));
extern int scroll_frame_lines P_ ((struct frame *, int, int, int, int));
extern int direct_output_for_insert P_ ((int));
extern int direct_output_forward_char P_ ((int));
extern int update_frame P_ ((struct frame *, int, int));
extern int scrolling P_ ((struct frame *));
extern void bitch_at_user P_ ((void));
void adjust_glyphs P_ ((struct frame *));
void free_glyphs P_ ((struct frame *));
void free_window_matrices P_ ((struct window *));
void check_glyph_memory P_ ((void));
void mirrored_line_dance P_ ((struct glyph_matrix *, int, int, int *, char *));
void clear_glyph_matrix P_ ((struct glyph_matrix *));
void clear_current_matrices P_ ((struct frame *f));
void clear_desired_matrices P_ ((struct frame *));
void shift_glyph_matrix P_ ((struct window *, struct glyph_matrix *,
			     int, int, int));
void rotate_matrix P_ ((struct glyph_matrix *, int, int, int));
void increment_matrix_positions P_ ((struct glyph_matrix *,
				     int, int, int, int));
void blank_row P_ ((struct window *, struct glyph_row *, int));
void increment_row_positions P_ ((struct glyph_row *, int, int));
void enable_glyph_matrix_rows P_ ((struct glyph_matrix *, int, int, int));
void clear_glyph_row P_ ((struct glyph_row *));
void prepare_desired_row P_ ((struct glyph_row *));
int line_hash_code P_ ((struct glyph_row *));
void set_window_update_flags P_ ((struct window *, int));
void write_glyphs P_ ((struct glyph *, int));
void insert_glyphs P_ ((struct glyph *, int));
void redraw_frame P_ ((struct frame *));
void redraw_garbaged_frames P_ ((void));
int scroll_cost P_ ((struct frame *, int, int, int));
int direct_output_for_insert P_ ((int));
int direct_output_forward_char P_ ((int));
int update_frame P_ ((struct frame *, int, int));
void update_single_window P_ ((struct window *, int));
int scrolling P_ ((struct frame *));
void buffer_posn_from_coords P_ ((struct window *, int *, int *,
				  Lisp_Object *, struct display_pos *));
void do_pending_window_change P_ ((int));
void change_frame_size P_ ((struct frame *, int, int, int, int, int));
void bitch_at_user P_ ((void));
Lisp_Object sit_for P_ ((int, int, int, int, int));
void init_display P_ ((void));
void syms_of_display P_ ((void));
extern Lisp_Object Qredisplay_dont_pause;

/* Defined in term.c */

extern void ring_bell P_ ((void));
extern void set_terminal_modes P_ ((void));
extern void reset_terminal_modes P_ ((void));
extern void update_begin P_ ((struct frame *));
extern void update_end P_ ((struct frame *));
extern void set_terminal_window P_ ((int));
extern void set_scroll_region P_ ((int, int));
extern void turn_off_insert P_ ((void));
extern void turn_off_highlight P_ ((void));
extern void background_highlight P_ ((void));
extern void clear_frame P_ ((void));
extern void clear_end_of_line P_ ((int));
extern void clear_end_of_line_raw P_ ((int));
extern void delete_glyphs P_ ((int));
extern void ins_del_lines P_ ((int, int));
extern int string_cost P_ ((char *));
extern int per_line_cost P_ ((char *));
extern void calculate_costs P_ ((struct frame *));
extern void set_tty_color_mode P_ ((struct frame *, Lisp_Object));
extern void tty_setup_colors P_ ((int));
extern void term_init P_ ((char *));
extern void fatal P_ ((/* char *, ... */));
void cursor_to P_ ((int, int));
extern int tty_capable_p P_ ((struct frame *, unsigned, unsigned long, unsigned long));

/* Defined in scroll.c */

extern int scrolling_max_lines_saved P_ ((int, int, int *, int *, int *));
extern int scroll_cost P_ ((struct frame *, int, int, int));
extern void do_line_insertion_deletion_costs P_ ((struct frame *, char *,
						  char *, char *, char *,
						  char *, char *, int));
void scrolling_1 P_ ((struct frame *, int, int, int, int *, int *, int *,
		      int *, int));

#endif /* not DISPEXTERN_H_INCLUDED */
