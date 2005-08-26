/* m- file for NEC EWS4800 RISC series.
   Copyright (C) 1998, 2000, 2001 Free Software Foundation, Inc.

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

#undef LIB_STANDARD
#undef C_DEBUG_SWITCH
#define INHIBIT_BSD_TIME
#undef USG
#include "mips.h"
#define USG
#undef UNEXEC
#define UNEXEC unexelf.o
#undef LIBS_MACHINE
#undef LD_SWITCH_MACHINE
#undef START_FILES
#undef DATA_START
#undef LIB_STANDARD
#undef C_SWITCH_MACHINE
#ifndef __GNUC__
#undef C_DEBUG_SWITCH
#define C_DEBUG_SWITCH -O  -KOlimit=3000 -ZXNd=5000
#endif  /* !__GNUC__ */

/* arch-tag: 27f72f54-45cd-40a3-b182-345127f04955
   (do not change this comment) */
