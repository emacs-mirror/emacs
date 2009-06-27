/* Machine description file for DEC MIPS machines.

   Copyright (C) 1992, 1999, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
                 2008, 2009  Free Software Foundation, Inc.

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

#include "mips.h"

/* The following line tells the configuration script what sort of
   operating system this machine is likely to run.
   USUAL-OPSYS="netbsd"

NOTE-START
The only operating system is NetBSD (osf1 and ultrix removed after 22.3).
NOTE-END  */

#ifndef __MIPSEB__
#undef WORDS_BIG_ENDIAN
#endif

#define BROKEN_NOCOMBRELOC
#undef COFF
#undef TERMINFO
#define MAIL_USE_FLOCK
#define HAVE_UNION_WAIT

#ifdef MACH
#define START_FILES pre-crt0.o /usr/lib/crt0.o
#endif

/* Supposedly the following will overcome a kernel bug.  */
#undef LD_SWITCH_MACHINE
#undef DATA_START
#define DATA_START 0x10000000
#define DATA_SEG_BITS 0x10000000

/* Enable a fix in process.c.  */
#define SET_CHILD_PTY_PGRP

/* arch-tag: 45d5070e-d2b7-479f-b336-3fd497c36e15
   (do not change this comment) */
