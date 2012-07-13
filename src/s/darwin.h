/* System description header file for Darwin (Mac OS X).

Copyright (C) 2001-2012  Free Software Foundation, Inc.

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

/* Definitions for how to compile & link.  */
#ifdef emacs
#define malloc unexec_malloc
#define realloc unexec_realloc
#define free unexec_free
/* Don't use posix_memalign because it is not compatible with unexmacosx.c.  */
#undef HAVE_POSIX_MEMALIGN
#endif

/* The following solves the problem that Emacs hangs when evaluating
   (make-comint "test0" "/nodir/nofile" nil "") when /nodir/nofile
   does not exist.  Also, setsid is not allowed in the vfork child's
   context as of Darwin 9/Mac OS X 10.5.  */
#undef HAVE_WORKING_VFORK
#define vfork fork
