/* MS-DOS specific C utilities, interface.
   Copyright (C) 1993 Free Software Foundation, Inc.

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

#ifndef _MSDOS_H_
#define _MSDOS_H_

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
#define PIX_TYPE int
#define XDISPLAY

/* This is a cut-down version of the one in xterm.h, which see.  */
struct x_output
{
  int left_pos;
  int top_pos;
  int line_height;
  PIX_TYPE background_pixel;
  PIX_TYPE foreground_pixel;
  XFontStruct *font;
  Window busy_window;
  unsigned busy_p : 1;
  struct face **param_faces;
  int n_param_faces;
  struct face **computed_faces;
  int n_computed_faces;
  int size_computed_faces;
};

extern struct x_output the_only_x_display;
extern Display *x_current_display;

#define FRAME_PARAM_FACES(f) (the_only_x_display.param_faces)
#define FRAME_N_PARAM_FACES(f) (the_only_x_display.n_param_faces)
#define FRAME_DEFAULT_PARAM_FACE(f) (FRAME_PARAM_FACES (f)[0])
#define FRAME_MODE_LINE_PARAM_FACE(f) (FRAME_PARAM_FACES (f)[1])
#define FRAME_COMPUTED_FACES(f) (the_only_x_display.computed_faces)
#define FRAME_N_COMPUTED_FACES(f) (the_only_x_display.n_computed_faces)
#define FRAME_SIZE_COMPUTED_FACES(f) (the_only_x_display.size_computed_faces)
#define FRAME_DEFAULT_FACE(f) (the_only_x_display.computed_faces[0])
#define FRAME_MODE_LINE_FACE(f) (the_only_x_display.computed_faces[1])
#define FRAME_X_DISPLAY(f) ((Display *) 0)
#define FRAME_FOREGROUND_PIXEL(f) (the_only_x_display.foreground_pixel)
#define FRAME_BACKGROUND_PIXEL(f) (the_only_x_display.background_pixel)
#define FRAME_FONT(f) (the_only_x_display.font)

/* Prototypes.  */

/* Forward declarations for prototypes.  */
struct frame;
struct window;
#if 0
extern int face_name_id_number P_ ((struct frame *, Lisp_Object)); /* !!! */
extern int compute_glyph_face P_ ((struct frame *, int, int)); /* !!! */
#endif

/* From xterm.c; emulated on msdos.c */

extern void pixel_to_glyph_coords P_ ((struct frame *f, int pix_x, int pix_y,
				       int *x, int *y, XRectangle *bounds,
				       int noclip));
extern void glyph_to_pixel_coords P_ ((struct frame *f, int x, int y,
				       int *pix_x, int *pix_y));

/* Defined in xfns.c; emulated on msdos.c */

extern int have_menus_p P_ ((void));
extern void x_set_menu_bar_lines P_ ((struct frame *, Lisp_Object, Lisp_Object));
extern int x_pixel_width P_ ((struct frame *));
extern int x_pixel_height P_ ((struct frame *));

#define XFreeGC (void)
#define unload_color(p1,p2)
#define x_destroy_bitmap(p1,p2)
#define load_pixmap(p1,p2,p3,p4) (0)
#define XGetGeometry(p1,p2,p3,p4,p5,p6,p7,p8,p9)
#define DisplayWidth(p1,p2) (selected_frame->width)
#define DisplayHeight(p1,p2) (selected_frame->height)
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
} XMenu;

XMenu *XMenuCreate (Display *, Window, char *);
int XMenuAddPane (Display *, XMenu *, char *, int);
int XMenuAddSelection (Display *, XMenu *, int, int, char *, int);
void XMenuLocate (Display *, XMenu *, int, int, int, int,
		  int *, int *, int *, int *);
int XMenuActivate (Display *, XMenu *, int *, int *, int, int, unsigned, char **);
void XMenuDestroy (Display *, XMenu *);

#endif /* not HAVE_X_WINDOWS */

#endif /* not _MSDOS_H_ */
