/* Definitions file for GNU Emacs running on bsd 4.3

Copyright (C) 1985-1986, 2001-2012  Free Software Foundation, Inc.

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


/* Define symbols to identify the version of Unix this is.
   Define all the symbols that apply correctly.   */

/* We give these symbols the numeric values found in <sys/param.h> to
   avoid warnings about redefined macros.  */

/* Nothing in Emacs uses this any more.
   ifndef BSD4_3
   define BSD4_3 1
   endif
*/

#ifndef BSD_SYSTEM
#define BSD_SYSTEM 43
#endif /* BSD_SYSTEM */

/* For mem-limits.h.  */
#define BSD4_2

/* First pty name is /dev/ptyp0.  */
#define FIRST_PTY_LETTER 'p'
