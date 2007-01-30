/* MS-DOS specific C utilities, interface.
   Copyright (C) 1993, 2001, 2002, 2003, 2004,
                 2005, 2006, 2007 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#ifndef EMACS_MSDOS_H
#define EMACS_MSDOS_H

#include <dpmi.h>

int dos_ttraw ();
int dos_ttcooked ();
int dos_get_saved_screen (char **, int *, int *);
int dos_set_keyboard (int, int);
void dos_set_window_size (int *, int *);

int getdefdir (int, char*);
void unixtodos_filename (char *);
void dostounix_filename (char *);
char *rootrelativepath (char *);
void init_environment ();
void internal_terminal_init ();
void ctrl_break_func (_go32_dpmi_registers *);
void install_ctrl_break_check ();

extern int have_mouse;
void mouse_init ();
void mouse_on ();
void mouse_off ();
void mouse_moveto (int, int);

#ifndef HAVE_X_WINDOWS
/* Dummy types.  */
typedef int XFontStruct;
typedef int GC;
typedef int Pixmap;
typedef int Display;
typedef int Window;
typedef int XRectangle;
#define PIX_TYPE unsigned long
#define XDISPLAY

/* A stripped version of struct x_display_info in xterm.h, which see.  */
struct display_info
{
  /* These variables describe the range of text currently shown in its
     mouse-face, together with the window they apply to.  As long as
     the mouse stays within this range, we need not redraw anything on
     its account.  Rows and columns are glyph matrix positions in
     MOUSE_FACE_WINDOW.  */
  int mouse_face_beg_row, mouse_face_beg_col;
  int mouse_face_end_row, mouse_face_end_col;
  int mouse_face_past_end;
  Lisp_Object mouse_face_window;
  int mouse_face_face_id;

  /* 1 if a mouse motion event came and we didn't handle it right away because
     gc was in progress.  */
  int mouse_face_deferred_gc;

  /* FRAME and X, Y position of mouse when last checked for
     highlighting.  X and Y can be negative or out of range for the frame.  */
  struct frame *mouse_face_mouse_frame;
  int mouse_face_mouse_x, mouse_face_mouse_y;

  /* Nonzero means defer mouse-motion highlighting.  */
  int mouse_face_defer;

  /* Nonzero means that the mouse highlight should not be shown.  */
  int mouse_face_hidden;
};

typedef struct display_info Display_Info;

/* This is a cut-down version of the one in xterm.h, which see.  */
struct x_output
{
  PIX_TYPE background_pixel;	/* used in xfaces.c and lots of other places */
  PIX_TYPE foreground_pixel;	/* ditto */
  XFontStruct *font;		/* used in x-popup-menu (xmenu.c) */
  Window hourglass_window;	/* currently unused (but maybe some day) */
  unsigned hourglass_p : 1;	/* ditto */
  struct display_info display_info; /* used for drawing mouse highlight */
};

extern struct x_output the_only_x_display;

#define FRAME_X_DISPLAY(f) ((Display *) 0)
#define FRAME_FOREGROUND_PIXEL(f) (the_only_x_display.foreground_pixel)
#define FRAME_BACKGROUND_PIXEL(f) (the_only_x_display.background_pixel)
#define FRAME_FONT(f) (the_only_x_display.font)
#define FRAME_X_DISPLAY_INFO(f) (&the_only_x_display.display_info)

/* Prototypes.  */

/* Forward declarations for prototypes.  */
struct frame;
struct window;

/* Defined in xfns.c; emulated on msdos.c */

extern int have_menus_p P_ ((void));
extern void x_set_menu_bar_lines P_ ((struct frame *, Lisp_Object, Lisp_Object));
extern int x_pixel_width P_ ((struct frame *));
extern int x_pixel_height P_ ((struct frame *));

#define XFreeGC (void)
#define x_destroy_bitmap(p1,p2)
#define load_pixmap(p1,p2,p3,p4) (0)
#define XGetGeometry(p1,p2,p3,p4,p5,p6,p7,p8,p9)
#define DisplayWidth(p1,p2) (SELECTED_FRAME()->text_cols)
#define DisplayHeight(p1,p2) (SELECTED_FRAME()->text_lines)
#define XMenuSetAEQ (void)
#define XMenuSetFreeze (void)
#define XMenuRecompute (void)
#define FONT_WIDTH(foo) 1
#define XM_FAILURE -1
#define XM_SUCCESS 1
#define XM_NO_SELECT 2
#define XM_IA_SELECT 3
#define ButtonReleaseMask 0

typedef struct x_menu_struct
{
  int count;
  char **text;
  struct x_menu_struct **submenu;
  int *panenumber; /* Also used as enable.  */
  int allocated;
  int panecount;
  int width;
  char **help_text;
} XMenu;

XMenu *XMenuCreate (Display *, Window, char *);
int XMenuAddPane (Display *, XMenu *, char *, int);
int XMenuAddSelection (Display *, XMenu *, int, int, char *, int, char *);
void XMenuLocate (Display *, XMenu *, int, int, int, int,
		  int *, int *, int *, int *);
int XMenuActivate (Display *, XMenu *, int *, int *, int, int, unsigned,
		   char **, void (*callback)(char *, int, int));
void XMenuDestroy (Display *, XMenu *);

#endif /* not HAVE_X_WINDOWS */

#endif /* not EMACS_MSDOS_H */

/* arch-tag: ad21eeed-8fdb-4357-8007-36368a6bdbf3
   (do not change this comment) */
