/* s- file for building Emacs on AIX 3.2.

   Copyright (C) 1999, 2001, 2002, 2003, 2004,
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


#include "aix3-1.h"

#define AIX3_2

/* No need to define this--the header files indicate X11R4,
   and that's supposedly what 3.2 will come with.  */
#undef SPECIFY_X11R4

#ifndef __GNUC__
/* Some programs in src produce warnings saying certain subprograms
   are to comples and need a MAXMEM value greater than 2000 for
   additional optimization.  --nils@exp-math.uni-essen.de */
#define C_SWITCH_SYSTEM -ma -qmaxmem=4000
#endif

/* Adrian Colley <Adrian.Colley@three.serpentine.com> says this is needed.  */
#ifndef NOT_C_CODE
#ifndef AIX4
 #pragma alloca
#endif
#endif

#undef rindex
#undef index

/* With this defined, a gcc-compiled Emacs crashed in realloc under AIX
   3.2, and a cc-compiled Emacs works with this undefined.
   --karl@cs.umb.edu.  */
#undef SYSTEM_MALLOC

/* For AIX, it turns out compiling emacs under AIX 3.2.4 REQUIRES "cc -g"
   because "cc -O" crashes. Under AIX 3.2.5, "cc -O" is required because
   "cc -g" crashes. Go figure.  --floppy@merlin.mit.edu */
/* See comments about this in aix3-2-5.h.  -- fx */
#ifndef __GNUC__
#define C_DEBUG_SWITCH -g -O
#endif

/* The character-composition stuff is broken in X11R5.
   Even with XIMStatusNothing aliased to XIMStatusNone,
   tranle@intellicorp.com (Minh Tran-Le) reports that enabling
   the internationalization code causes the modifier keys C, M and Shift
   to beep after a mouse click.  */
#define X11R5_INHIBIT_I18N

/* string.h defines rindex as a macro, at least with native cc, so we
   lose declaring char * rindex without this.
   It is just a guess which versions of AIX need this definition.  */
#undef HAVE_STRING_H

/* arch-tag: 0935754d-67e1-4697-978a-3e9976da05c3
   (do not change this comment) */
