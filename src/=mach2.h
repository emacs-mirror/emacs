/* Definitions for Emacs running on Mach version 2 (non-kernelized system).
   Copyright (C) 1990 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "bsd4-3.h"

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  We'll need to undo the bsd one. */

#undef SYSTEM_TYPE
#define SYSTEM_TYPE "next-mach"

#define LD_SWITCH_SYSTEM -X -noseglinkedit

/* Don't use -lc on the NeXT.  */
#define LIB_STANDARD -lsys_s
#define LIB_MATH -lm

#define environ _environ

#define START_FILES pre-crt0.o
#define UNEXEC unexnext.o

/* start_of_text isn't actually used, so make it compile without error.  */
#define TEXT_START 0
/* This seems to be right for end_of_text, but it may not be used anyway.  */
#define TEXT_END get_etext ()
/* This seems to be right for end_of_data, but it may not be used anyway.  */
#define DATA_END get_edata ()

/* Defining KERNEL_FILE causes lossage because sys/file.h
   stupidly gets confused by it.  */
#undef KERNEL_FILE
