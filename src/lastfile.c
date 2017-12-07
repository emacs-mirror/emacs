/* Mark end of data space to dump as pure, for GNU Emacs.
   Copyright (C) 1985, 2001-2017 Free Software Foundation, Inc.

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


/* How this works:

 Fdump_emacs dumps everything up to my_edata as text space (pure).

 The files of Emacs are written so as to have no initialized
 data that can ever need to be altered except at the first startup.
 This is so that those words can be dumped as shareable text.

 It is not possible to exercise such control over library files.
 So it is necessary to refrain from making their data areas shared.
 Therefore, this file is loaded following all the files of Emacs
 but before library files.
 As a result, the symbol my_edata indicates the point
 in data space between data coming from Emacs and data
 coming from libraries.
*/

#include <config.h>

#include "lisp.h"

#if ((!defined SYSTEM_MALLOC && !defined HYBRID_MALLOC) \
     || defined WINDOWSNT || defined CYGWIN || defined DARWIN_OS)
char my_edata[] = "End of Emacs initialized data";
#endif

#ifndef CANNOT_DUMP

/* Help unexec locate the end of the .bss area used by Emacs (which
   isn't always a separate section in NT executables).  */
char my_endbss[1];

static char _my_endbss[1];
char * my_endbss_static = _my_endbss;

#endif
