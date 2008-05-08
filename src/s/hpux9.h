/* System description file for hpux version 9.

   Copyright (C) 1999, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
                 2008  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */


#include "hpux8.h"

#define HPUX9

/* If Emacs doesn't seem to work when built to use GNU malloc, you
   probably need to get the latest patches to the HP/UX compiler.
   See `etc/MACHINES' for more information.  */
#if 0
#define SYSTEM_MALLOC 1
#undef GNU_MALLOC
#undef REL_ALLOC
#endif

#ifndef __GNUC__
/* Make room for enough symbols, so dispnew.c does not fail.  */
#define C_SWITCH_SYSTEM -Wp,-H200000 -D_BSD
#else
#define C_SWITCH_SYSTEM -D_BSD
#endif

#if 0 /* These definitions run into a bug in hpux
	 whereby trying to disable the vdsusp character has no effect.
	 supposedly there is no particular need for this.  */
/* neal@ctd.comsat.com */
#undef HAVE_TERMIO
#define HAVE_TERMIOS
#define NO_TERMIO
#endif

/* According to ngorelic@speclab.cr.usgs.gov,
   references to the X11R4 directories in these variables
   (inherited from hpux8.h)
   cause the wrong libraries to be found,
   and the options to specify the X11R5 directories are unnecessary
   since the R5 files are found without them.  */
#undef LIB_X11_LIB
#undef C_SWITCH_X_SYSTEM
#undef LD_SWITCH_X_DEFAULT
/* However, HPUX 9 has Motif includes in a strange place.
   So search that place.  These definitions assume that X11R5 is being
   used -- if X11R4 is used, "s/hpux9-x11r4.h" gets loaded instead.  */
/* horst@tkm.physik.uni-karlsruhe.de says that the /usr/contrib/... dirs
   are needed to find the Xmu and Xaw libraries.  */
#define C_SWITCH_X_SYSTEM -I/usr/include/X11R5 -I/usr/contrib/X11R5/include -I/usr/include/Motif1.2
#define LD_SWITCH_X_DEFAULT -L/usr/lib/X11R5 -L/usr/contrib/X11R5/lib -L/usr/lib/Motif1.2

#ifndef HAVE_LIBXMU
/* HP-UX doesn't supply Xmu.  */
#define LIBXMU

#endif

/* Assar Westerlund <assar@sics.se> says this is necessary for
   HP-UX 10.20, and that it works for HP-UX 0 as well.  */
#define NO_EDITRES

/* zoo@armadillo.com says we don't need -lXext in HPUX 9.  */
#undef LIBX11_SYSTEM

/* Tested in getloadavg.c.  */
#define HAVE_PSTAT_GETDYNAMIC

/* Eric Backus <ericb@lsid.hp.com> says, HP-UX 9.x on HP 700 machines
   has a broken `rint' in some library versions including math library
   version number A.09.05.

   You can fix the math library by installing patch number PHSS_4630.
   But we can fix it more reliably for Emacs like this. */
#undef HAVE_RINT


/* arch-tag: 0a5e9f05-012c-4962-a222-a7a3a7fe0ab7
   (do not change this comment) */
