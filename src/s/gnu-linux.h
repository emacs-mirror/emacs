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

/* Define symbols to identify the version of Unix this is.
   Define all the symbols that apply correctly.  */
#define USG
#define GNU_LINUX

/* Here, on a separate page, add any special hacks needed
   to make Emacs work on this system.  For example,
   you might define certain system call names that don't
   exist on your system, or that do different things on
   your system and must be used only through an encapsulation
   (Which you should place, by convention, in sysdep.c).  */

#ifdef __ia64__
#define GC_MARK_SECONDARY_STACK()				\
  do {								\
    extern void *__libc_ia64_register_backing_store_base;	\
    __builtin_ia64_flushrs ();					\
    mark_memory (__libc_ia64_register_backing_store_base,	\
		 __builtin_ia64_bsp ());			\
  } while (0)
#endif

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

#ifdef __i386__
/* libc-linux/sysdeps/linux/i386/ulimit.c says that due to shared library, */
/* we cannot get the maximum address for brk */
# define ULIMIT_BREAK_VALUE (32*1024*1024)
#endif
