/* Interface definitions for display code.
   Copyright (C) 1985 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Nonzero means don't assume anything about current
   contents of actual terminal screen */
extern int screen_garbaged;

/* Nonzero means last display completed and cursor is really at
   cursX, cursY.  Zero means it was preempted. */
extern int display_completed;

#ifdef HAVE_X_WINDOWS
struct run
{
  int begin_run;
  int len;
  int face_code;		/* Also handles underlining. */
};
#endif

/* This structure is used for the actual display of text on a screen.

   There are two instantiations of it:  the glyphs currently displayed,
   and the glyphs we desire to display.  The latter object is generated
   from buffers being displayed. */

struct screen_glyphs
  {
#ifdef MULTI_SCREEN
    struct  screen *screen;	/* Screen these glyphs belong to. */
#endif /* MULTI_SCREEN */
    int height;
    int width;

    int *used;			/* Vector of widths (in chars) of lines. */
    /* Vector of line contents.
       m->glyphs[V][H] is the glyph at position V, H.
       Note that ->glyphs[...][screen_width] is always 0
       and so is ->glyphs[...][-1].  */
    GLYPH **glyphs;
    /* long vector from which the strings in `glyphs' are taken.  */
    GLYPH *total_contents;

    /* highlight[n] != 0 iff line n is highlighted.  */
    char *highlight;

    /* When representing a desired screen, enable[n] == 0 implies line
       n is same as current screen.  When representing current screen
       contents, enable[n] == 0 implies line n is blank.  */
    char *enable;

    /* Buffer offset of this line's first char. */
    int   *bufp;

#ifdef HAVE_X_WINDOWS
    int *nruns;			/* N runs of differently displayed text. */
    struct run **face_list;
    short *top_left_x;		/* Pixel position of top left corner */
    short *top_left_y;
    short *pix_width;		/* Pixel width of line. */
    short *pix_height;		/* Pixel height of line. */
#endif	/* HAVE_X_WINDOWS */
  };

#if 0
#define LINE_HEIGHT(s,n) (current_glyphs->pix_height[n])
#define LINE_WIDTH(s,n) (current_glyphs->pix_width[n])
#endif

#define LINE_HEIGHT(s,n) (FONT_HEIGHT((s)->display.x->font))
#define LINE_WIDTH(s,n) (FONT_HEIGHT((s)->display.x->font) \
			 * SCREEN_CURRENT_GLYPHS(s)->enable[(n)])

extern void get_display_line ();

/* Buffer used by `message' for formatting a message.  */
extern char *message_buf;
extern int message_buf_size;
