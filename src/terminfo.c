/* Interface from Emacs to terminfo.
   Copyright (C) 1985, 1986, 2001, 2002, 2003, 2004,
                 2005, 2006, 2007  Free Software Foundation, Inc.

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

#include <config.h>
#include "lisp.h"

/* Define these variables that serve as global parameters to termcap,
   so that we do not need to conditionalize the places in Emacs
   that set them.  */

char *UP, *BC, PC;

/* Interface to curses/terminfo library.
   Turns out that all of the terminfo-level routines look
   like their termcap counterparts except for tparm, which replaces
   tgoto.  Not only is the calling sequence different, but the string
   format is different too.
*/

char *
tparam (string, outstring, len, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
     char *string;
     char *outstring;
     int len, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9;
{
  char *temp;
  extern char *tparm();

  temp = tparm (string, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
  if (outstring == 0)
    outstring = ((char *) (xmalloc ((strlen (temp)) + 1)));
  strcpy (outstring, temp);
  return outstring;
}

/* arch-tag: a6f96a69-e68f-4e9d-a223-f0b0da26ead5
   (do not change this comment) */
