/* MS-DOS specific C utilities, interface.
   Copyright (C) 1993 Free Software Foundation, Inc.

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

#include <time.h>
#include <dpmi.h>

int dos_ttraw ();
int dos_ttcooked ();
int getdefdir (int, char*);
void unixtodos_filename (char *);
void dostounix_filename (char *);
void sleep_or_kbd_hit (int, int);
void init_environment ();
void internal_terminal_init ();
#ifndef _stdio_h_
int internal_flush (FILE *);
#endif
void ctrl_break_func (_go32_dpmi_registers *);
void install_ctrl_break_check ();

extern int have_mouse;
int mouse_init1 ();
void mouse_init ();
void mouse_on ();
void mouse_off ();
void mouse_moveto (int, int);
void mouse_check_moved ();
int mouse_pressed (int, int *, int *);
int mouse_released (int, int *, int *);
