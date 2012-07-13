/* Template for system description header files.
   This file describes the parameters that system description files
   should define or not.

Copyright (C) 1985-1986, 1992, 1999, 2001-2012  Free Software Foundation, Inc.

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

/* subprocesses should be undefined if you do NOT want to
   have code for asynchronous subprocesses
   (as used in M-x compile and M-x shell).
   Currently only MSDOS does not support this.  */

/* #undef subprocesses */

/* If the character used to separate elements of the executable path
   is not ':', #define this to be the appropriate character constant.  */
/* #define SEPCHAR ':' */

/* ============================================================ */

/* Here, add any special hacks needed to make Emacs work on this
   system.  For example, you might define certain system call names
   that don't exist on your system, or that do different things on
   your system and must be used only through an encapsulation (which
   you should place, by convention, in sysdep.c).  */

/* ============================================================ */

/* After adding support for a new system, modify the large case
   statement in configure.ac to recognize reasonable
   configuration names, and add a description of the system to
   `etc/MACHINES'.

   Check for any tests of $opsys in configure.ac, and add an entry
   for the new system if needed.

   If you've just fixed a problem in an existing configuration file,
   you should also check `etc/MACHINES' to make sure its descriptions
   of known problems in that configuration should be updated.  */
