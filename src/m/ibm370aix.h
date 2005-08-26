/* m/ file for IBM 370 running AIX.
   Copyright (C) 1993, 1994, 2001, 2002, 2003, 2004,
                 2005 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "ibmps2-aix.h"

#define AIX

/* Include unistd.h, even though we don't define POSIX.  */
#define NEED_UNISTD_H

/* these were defined in "ibmps2-aix.h" */
#undef INTEL386
#undef aix386

/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define WORDS_BIG_ENDIAN

#undef TEXT_START
#undef SEGMENT_MASK
#undef DATA_SECTION_ALIGNMENT

#define TEXT_START 0
#define SEGMENT_MASK 0
#define DATA_SECTION_ALIGNMENT 0x00001000

#undef LOAD_AVE_CVT
#undef LOAD_AVE_TYPE
/* Data type of load average, as read out of kmem.  */
#define LOAD_AVE_CVT(x) (int)(((double) (x)) * 100.0 / 1.0)
#define LOAD_AVE_TYPE double

#undef LIBS_MACHINE
#define LIBS_MACHINE

#undef LD_SWITCH_MACHINE
#define LD_SWITCH_MACHINE -xa

/* arch-tag: 8605b600-0580-4e49-9ba9-8b4a977f860a
   (do not change this comment) */
