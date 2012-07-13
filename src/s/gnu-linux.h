/* This file is the configuration file for Linux-based GNU systems

Copyright (C) 1985-1986, 1992, 1994, 1996, 1999, 2001-2012
  Free Software Foundation, Inc.

This file was put together by Michael K. Johnson and Rik Faith.

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

/* Tell that garbage collector that setjmp is known to save all
   registers relevant for conservative garbage collection in the jmp_buf.
   Not all the architectures are tested, but there are Debian packages
   for SCM and/or Guile on them, so the technique must work.  See also
   comments in alloc.c concerning setjmp and gcc.  Fixme:  it's
   probably safe to just let the GCC conditional in AH_BOTTOM handle this.
*/
#if defined __i386__ || defined __sparc__ || defined __mc68000__ \
    || defined __alpha__ || defined __mips__ || defined __s390__ \
    || defined __arm__ || defined __powerpc__ || defined __amd64__ \
    || defined __ia64__ || defined __sh__
#define GC_SETJMP_WORKS 1
#else
#define GC_MARK_STACK GC_USE_GCPROS_AS_BEFORE
#endif
