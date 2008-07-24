/* C code startup routine.
   Copyright (C) 1985, 1986, 1992, 2001, 2002, 2003, 2004,
                 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

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


/* The standard Vax 4.2 Unix crt0.c cannot be used for Emacs
   because it makes `environ' an initialized variable.
   It is easiest to have a special crt0.c on all machines
   though I don't know whether other machines actually need it.  */

/* On the vax and 68000, in BSD4.2 and USG5.2,
   this is the data format on startup:
  (vax) ap and fp are unpredictable as far as I know; don't use them.
  sp ->  word containing argc
         word pointing to first arg string
	 [word pointing to next arg string]... 0 or more times
	 0
Optionally:
	 [word pointing to environment variable]... 1 or more times
	 ...
	 0
And always:
	 first arg string
	 [next arg string]... 0 or more times
*/

#ifdef emacs
#include <config.h>
#endif

/*		********  WARNING ********
    Do not insert any data definitions before data_start!
    Since this is the first file linked, the address of the following
    variable should correspond to the start of initialized data space.
    On some systems this is a constant that is independent of the text
    size for shared executables.  On others, it is a function of the
    text size. In short, this seems to be the most portable way to
    discover the start of initialized data space dynamically at runtime,
    for either shared or unshared executables, on either swapping or
    virtual systems.  It only requires that the linker allocate objects
    in the order encountered, a reasonable model for most Unix systems.
    Similarly, note that the address of _start() should be the start
    of text space.   Fred Fish, UniSoft Systems Inc.  */

int data_start = 0;

#ifdef NEED_ERRNO
int errno;
#endif

#ifndef MSDOS
char **environ;
#endif

#ifndef static
/* On systems where the static storage class is usable, this function
   should be declared as static.  Otherwise, the static keyword has
   been defined to be something else, and code for those systems must
   take care of this declaration appropriately.  */
static start1 ();
#endif

#ifdef CRT0_DUMMIES

/* Define symbol "start": here; some systems want that symbol.  */
asm("	.text		");
asm("	.globl start	");
asm("	start:		");

_start ()
{
/* On vax, nothing is pushed here  */
  start1 ();
}

static
start1 (CRT0_DUMMIES argc, xargv)
     int argc;
     char *xargv;
{
  register char **argv = &xargv;
  environ = argv + argc + 1;

  if ((char *)environ == xargv)
    environ--;
  exit (main (argc, argv, environ));

  /* Refer to `start1' so GCC will not think it is never called
     and optimize it out.  */
  (void) &start1;
}
#else /* not CRT0_DUMMIES */

/* This is a kludge.  Now that the CRT0_DUMMIES mechanism above exists,
   most of these machines could use the vax code above
   with some suitable definition of CRT0_DUMMIES.
   Then the symbol m68k could be flushed.
   But I don't want to risk breaking these machines
   in a version 17 patch release, so that change is being put off.  */

#ifdef m68k			/* Can't do it all from C */
	asm ("	global	_start");
	asm ("	text");
	asm ("_start:");
	asm ("  comm	splimit%,4");
	asm ("	global	exit");
	asm ("	text");
  	asm ("	mov.l	%d0,splimit%");
	asm ("	jsr	start1");
	asm ("	mov.l	%d0,(%sp)");
	asm ("	jsr	exit");
	asm ("	mov.l	&1,%d0");	/* d0 = 1 => exit */
	asm ("	trap	&0");

/* ignore takes care of skipping the a6 value pushed in start.  */
static
start1 (argc, xargv)
     int argc;
     char *xargv;
{
  register char **argv = &xargv;
  environ = argv + argc + 1;

  if ((char *)environ == xargv)
    environ--;
  exit (main (argc, argv, environ));
}

#endif /* m68k */

#endif /* not CRT0_DUMMIES */

#ifdef __sparc__
asm (".global __start");
asm (".text");
asm ("__start:");
asm ("	mov	0, %fp");
asm ("	ld	[%sp + 64], %o0");
asm ("	add	%sp, 68, %o1");
asm ("	sll	%o0, 2,	%o2");
asm ("	add	%o2, 4,	%o2");
asm ("	add	%o1, %o2, %o2");
asm ("	sethi	%hi(_environ), %o3");
asm ("	st	%o2, [%o3+%lo(_environ)]");
asm ("	andn	%sp, 7,	%sp");
asm ("	call	_main");
asm ("	sub	%sp, 24, %sp");
asm ("	call	__exit");
asm ("	nop");

#endif /* __sparc__ */

#if __FreeBSD__ == 2
char *__progname;
#endif

/* arch-tag: 4025c2fb-d6b1-4d29-b1b6-8100b6bd1e74
   (do not change this comment) */
