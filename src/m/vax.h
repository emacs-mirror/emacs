/* machine description file for vax.
   Copyright (C) 1985, 1986, 2002 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="note"

NOTE-START
The vax (-machine=vax) runs zillions of different operating systems.

Vax running Berkeley Unix (-opsystem=bsd4-1, -opsystem=bsd4-2 or
			   -opsystem=bsd4-3)

  Works.

Vax running Ultrix (-opsystem=bsd4-2)

  Works.  See under Ultrix in share-lib/MACHINES for problems using X
  windows on Ultrix.

Vax running System V rel 2 (-opsystem=usg5-2)

  18.27 Works.

Vax running System V rel 0 (-opsystem=usg5-0)

  Works as of 18.36.

Vax running VMS (-opsystem=vms)

  18.36 believed to work.  Addition of features is necessary to make
  this Emacs version more usable.

NOTE-END  */

/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#undef WORDS_BIG_ENDIAN

/* #define vax    -- appears to be done automatically  */

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* crt0.c should use the vax-bsd style of entry, with no dummy args.  */

#define CRT0_DUMMIES

/* crt0.c should define a symbol `start' and do .globl with a dot.  */

#define DOT_GLOBAL_START

#ifdef BSD_SYSTEM
/* USG systems I know of running on Vaxes do not actually
   support the load average, so disable it for them.  */

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE double

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) ((int) ((x) * 100.0))

#endif /* BSD_SYSTEM */

#ifdef VMS

/* Data type of load average, as read out of driver.  */

#define LOAD_AVE_TYPE float

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) ((int) ((x) * 100.0))

#endif /* VMS */

/* Vax sysV has alloca in the PW library.  */

#ifdef USG
#define LIB_STANDARD -lPW -lc

/* There is some bug in unexec in for usg 5.2 on a vax
   which nobody who runs such a system has yet tracked down. */
#ifndef USG5_0
#define NO_REMAP
#endif /* USG 5_0 */

#define TEXT_START 0
#endif /* USG */

#ifdef BSD4_2
#define HAVE_FTIME
#endif
