/* Interface definitions for display code.
   Copyright (C) 1985, 1993, 1994 Free Software Foundation, Inc.

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

#ifndef _DISPEXTERN_H_
#define _DISPEXTERN_H_

/* Nonzero means last display completed and cursor is really at
   cursX, cursY.  Zero means it was preempted. */
extern int display_completed;

#ifdef HAVE_X_WINDOWS
#include <X11/Xlib.h>
#endif

#ifdef MSDOS
#include "msdos.h"
#endif

#ifdef HAVE_NTGUI
#include "win32.h"
#endif

#ifdef HAVE_FACES
struct face
  {
    /* If this is non-zero, it is a GC we can use without modification
       to represent this face.  */
    GC gc;
  
    /* Pixel value for foreground color.  */
    EMACS_UINT foreground;
  
    /* Pixel value for background color.  */
    EMACS_UINT background;
  
    /* Font used for this face.  */
    XFontStruct *font;
  
    /* Background stipple or bitmap used for this face.  */
    Pixmap stipple;

    /* Pixmap_depth.  */
    unsigned int pixmap_w, pixmap_h;
  
    /* Whether or not to underline text in this face.  */
    char underline;
  };

/* Let's stop using this and get rid of it.  */
typedef struct face *FACE;

#define NORMAL_FACE ((struct face *) 0)

#define FACE_HAS_GC(f) ((f)->gc)
#define FACE_GC(f) ((f)->gc)
#define FACE_FOREGROUND(f) ((f)->foreground)
#define FACE_BACKGROUND(f) ((f)->background)
#define FACE_FONT(f) ((f)->font)
#define FACE_STIPPLE(f) ((f)->stipple)
#define FACE_UNDERLINE_P(f) ((f)->underline)

#else /* not HAVE_FACES */

typedef int FACE;

#define NORMAL_FACE 0x0
#define HIGHLIGHT_FACE 0x1
#define UNDERLINE_FACE 0x2
#define HIGHLIGHT_UNDERLINE_FACE 0x3

#define FACE_HIGHLIGHT(f) ((f) & 0x1)
#define FACE_UNDERLINE(f) ((f) & 0x2)

#endif /* not HAVE_FACES */


/* This structure is used for the actual display of text on a frame.

   There are two instantiations of it:  the glyphs currently displayed,
   and the glyphs we desire to display.  The latter object is generated
   from buffers being displayed.  */

struct frame_glyphs
  {
#ifdef MULTI_FRAME
    struct  frame *frame;	/* Frame these glyphs belong to.  */
#endif /* MULTI_FRAME */
    int height;
    int width;

    /* Contents of the frame.
       glyphs[V][H] is the glyph at position V, H.
       Note that glyphs[V][-1],
                 glyphs[V][used[V]],
	     and glyphs[V][frame_width] are always '\0'.  */
    GLYPH **glyphs;
    /* long vector from which the strings in `glyphs' are taken.  */
    GLYPH *total_contents;

    /* When representing a desired frame,
         enable[n] == 0 means that line n is same as current frame.
	 Between updates, all lines should be disabled.
       When representing current frame contents,
         enable[n] == 0 means that line n is blank.  */
    char *enable;

    /* Everything on line n after column used[n] is considered blank.  */
    int *used;

    /* highlight[n] != 0 iff line n is highlighted.  */
    char *highlight;

    /* Buffer offset of this line's first char.
       This is not really implemented, and cannot be,
       and should be deleted.  */
    int   *bufp;

#ifdef HAVE_WINDOW_SYSTEM
    /* Pixel position of top left corner of line.  */
    short *top_left_x;
    short *top_left_y;

    /* Pixel width of line.  */
    short *pix_width;

    /* Pixel height of line.  */
    short *pix_height;

    /* Largest font ascent on this line.  */
    short *max_ascent;
#endif	/* HAVE_WINDOW_SYSTEM */

    /* Mapping of coordinate pairs to buffer positions.
       This field holds a vector indexed by row number.
       Its elements are vectors indexed by column number.
       Each element of these vectors is a buffer position, 0, or -1.

       For a column where the image of a text character starts,
       the element value is the buffer position of that character.
       When a window's screen line starts in mid character,
       the element for the line's first column (at the window's left margin)
       is that character's position.
       For successive columns within a multicolumn character,
       the element is -1.
       For the column just beyond the last glyph on a line,
       the element is the buffer position of the end of the line.
       For following columns within the same window, the element is 0.
       For rows past the end of the accessible buffer text,
       the window's first column has ZV and other columns have 0.

       Mode lines and vertical separator lines have 0.

       The column of a window's left margin
       always has a positive value (a buffer position), not 0 or -1,
       for each line in that window's interior.  */

    int **charstarts;

    /* This holds all the space in the subvectors of the charstarts field.  */
    int *total_charstarts;
  };

extern void get_display_line ();
extern Lisp_Object sit_for ();

#endif /* not _DISPEXTERN_H_ */
