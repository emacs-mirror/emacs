/* MS-DOS specific C utilities, interface.
   Copyright (C) 1993, 2001-2017 Free Software Foundation, Inc.

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

#ifndef EMACS_MSDOS_H
#define EMACS_MSDOS_H

#include <dpmi.h>

#include "termhooks.h"		/* struct terminal */

int dos_ttraw (struct tty_display_info *);
int dos_ttcooked (void);
int dos_get_saved_screen (char **, int *, int *);
int dos_set_keyboard (int, int);
void dos_set_window_size (int *, int *);

int getdefdir (int, char*);
void unixtodos_filename (char *);
void dostounix_filename (char *);
char *rootrelativepath (char *);
void init_environment (int, char **, int);
void internal_terminal_init (void);
void initialize_msdos_display (struct terminal *);

extern int have_mouse;
void mouse_init (void);
void mouse_on (void);
void mouse_off (void);
void mouse_moveto (int, int);

void IT_set_frame_parameters (struct frame *, Lisp_Object);

#include <sys/types.h>
#include <sys/stat.h>
#include <pc.h>
#include <signal.h>

#if __DJGPP__ == 2 && __DJGPP_MINOR__ < 4
int readlink (const char *, char *, size_t);
#endif
ssize_t readlinkat (int, const char *, char *, size_t);
int fstatat (int, char const *, struct stat *, int);
int unsetenv (const char *);
int faccessat (int, const char *, int, int);
void msdos_fatal_signal (int);
void syms_of_msdos (void);
int pthread_sigmask (int, const sigset_t *, sigset_t *);
int dos_keysns (void);
int dos_keyread (void);
int run_msdos_command (char **, const char *, int, int, int, char **);

void syms_of_win16select (void);


/* Constants.  */
#define EINPROGRESS 112
#define ENOTSUP     ENOSYS
/* Gnulib sets O_CLOEXEC to O_NOINHERIT, which gets in the way when we
   need to redirect standard handles for subprocesses using temporary
   files created by mkostemp, see callproc.c.  */
#ifdef O_CLOEXEC
# undef O_CLOEXEC
#endif
#define O_CLOEXEC 0


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

typedef struct tty_display_info Display_Info;

extern struct tty_display_info the_only_display_info;

#define FRAME_X_DISPLAY(f) ((Display *) 0)
#define FRAME_FONT(f) ((f)->output_data.tty->font)
#define FRAME_DISPLAY_INFO(f) (&the_only_display_info)

/* Prototypes.  */

/* Forward declarations for prototypes.  */
struct frame;
struct window;

/* Defined in xfns.c; emulated on msdos.c */

extern void x_set_menu_bar_lines (struct frame *, Lisp_Object, Lisp_Object);

#define XFreeGC (void)
#define x_destroy_bitmap(p1,p2)
#define XGetGeometry(p1,p2,p3,p4,p5,p6,p7,p8,p9)
#define DisplayWidth(p1,p2) (SELECTED_FRAME()->text_cols)
#define DisplayHeight(p1,p2) (SELECTED_FRAME()->text_lines)
#define XMenuSetAEQ (void)
#define XMenuSetFreeze (void)
#define XMenuRecompute (void)
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
  const char **help_text;
} XMenu;

XMenu *XMenuCreate (Display *, Window, char *);
int XMenuAddPane (Display *, XMenu *, char const *, int);
int XMenuAddSelection (Display *, XMenu *, int, int, char *, int, char const *);
void XMenuLocate (Display *, XMenu *, int, int, int, int,
		  int *, int *, int *, int *);
int XMenuActivate (Display *, XMenu *, int *, int *, int, int, unsigned,
		   char **, void (*callback)(char const *, int, int));
void XMenuDestroy (Display *, XMenu *);

#endif /* not HAVE_X_WINDOWS */

#endif /* not EMACS_MSDOS_H */
