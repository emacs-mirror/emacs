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
#include "w32gui.h"
#endif

#ifdef HAVE_FACES
struct face
  {
    /* If this is non-zero, it is a GC we can use without modification
       to represent this face.  Used only for ASCII characters.  */
    GC gc;

    /* GC used for non-ASCII characters.  */
    GC non_ascii_gc;

    /* Pixel value for foreground color.  */
    EMACS_UINT foreground;
  
    /* Pixel value for background color.  */
    EMACS_UINT background;
  
    /* Font used for this face.  If any fontset is set for this face,
       this points to a `font' slot of the struct `font_info' for an
       ASCII font of the fontset.  In that case, we should not call
       XFreeFont on it because the font may still be used somewhere
       else.  */
    XFontStruct *font;

    /* Fontset ID if any fontset is set for this face, else -1.  */
    int fontset;
  
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
#define FACE_NON_ASCII_GC(f) ((f)->non_ascii_gc)
#define FACE_FOREGROUND(f) ((f)->foreground)
#define FACE_BACKGROUND(f) ((f)->background)
#define FACE_FONT(f) ((f)->font)
#define FACE_FONTSET(f) ((f)->fontset)
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
    struct  frame *frame;	/* Frame these glyphs belong to.  */
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

extern void redraw_frame P_ ((struct frame *));
extern void redraw_garbaged_frames P_ ((void));
extern void free_frame_glyphs P_ ((struct frame *, struct frame_glyphs *));
extern void remake_frame_glyphs P_ ((struct frame *));
extern void cancel_line P_ ((int, struct frame *));
extern void clear_frame_records P_ ((struct frame *));
extern void init_desired_glyphs P_ ((struct frame *));
extern void get_display_line P_ ((struct frame *, int, int));
extern int scroll_frame_lines P_ ((struct frame *, int, int, int, int));
extern void preserve_other_columns P_ ((struct window *));
extern void adjust_window_charstarts P_ ((struct window *, int, int));
extern void verify_charstarts P_ ((struct window *));
extern void cancel_my_columns P_ ((struct window *));
extern int direct_output_for_insert P_ ((int));
extern int direct_output_forward_char P_ ((int));
extern int update_frame P_ ((struct frame *, int, int));
extern void quit_error_check P_ ((void));
extern int scrolling P_ ((struct frame *));
extern int buffer_posn_from_coords P_ ((struct window *, int, int));
extern void do_pending_window_change P_ ((void));
extern void change_frame_size P_ ((struct frame *, int, int, int, int));
extern void bitch_at_user P_ ((void));

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
extern void reassert_line_highlight P_ ((int, int));
extern void change_line_highlight P_ ((int, int, int));
extern void cursor_to P_ ((int, int));
extern void clear_frame P_ ((void));
extern void clear_end_of_line P_ ((int));
extern void clear_end_of_line_raw P_ ((int));
extern void write_glyphs P_ ((GLYPH *, int));
extern void insert_glyphs P_ ((GLYPH *, int));
extern void delete_glyphs P_ ((int));
extern void ins_del_lines P_ ((int, int));
extern int string_cost P_ ((char *));
extern int per_line_cost P_ ((char *));
extern void calculate_costs P_ ((struct frame *));
extern void term_init P_ ((char *));
extern void fatal P_ ((/* char *, ... */));

/* Defined in scroll.c */
extern int scrolling_max_lines_saved P_ ((int, int, int *, int *, int *));
extern int scroll_cost P_ ((struct frame *, int, int, int));
extern void do_line_insertion_deletion_costs P_ ((struct frame *, char *,
						  char *, char *, char *,
						  char *, char *, int));

#endif /* not _DISPEXTERN_H_ */
