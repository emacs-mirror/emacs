/* MS-DOS specific Lisp utilities interface.
   Coded by Manabu Higashida, 1991.
   Modified by Morten Welinder, 1993-1994.

   Copyright (C) 1991, 1994, 1995, 1997, 1999
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

extern EMACS_INT dos_hyper_key;
extern EMACS_INT dos_super_key;
extern EMACS_INT dos_decimal_point;
extern EMACS_INT dos_keypad_mode;

extern EMACS_INT dos_keyboard_layout;
extern EMACS_INT dos_country_code;
extern EMACS_INT dos_codepage;
extern EMACS_INT dos_timezone_offset;

#define DOS_COUNTRY_INFO 34	/* no of bytes returned by dos int 38h */
extern unsigned char dos_country_info[DOS_COUNTRY_INFO];

extern Lisp_Object Vdos_version;
extern Lisp_Object Vdos_windows_version;
#ifndef HAVE_X_WINDOWS
extern Lisp_Object Vdos_display_scancodes;

extern int         msdos_stdcolor_idx  P_ ((const char *));
extern Lisp_Object msdos_stdcolor_name P_ ((int));
#endif
