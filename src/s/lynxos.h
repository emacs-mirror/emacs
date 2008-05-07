/* Definitions file for GNU Emacs running on LynxOS-3.0.1
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
                 2008 Free Software Foundation, Inc.

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

/* LynxOS is almost a bsd 4.2 system */
#include "s/bsd4-2.h"

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

/* override the bsd definition */
#undef SYSTEM_TYPE
#define SYSTEM_TYPE "lynxos 3.0.1"

/* System stuff redefined from bsd4-2.h */
#undef KERNEL_FILE
#define KERNEL_FILE "/lynx.os"
#undef LDAV_SYMBOL
#define LDAV_SYMBOL "load_average"

/* misc defines */
#define GETPGRP_NO_ARG
#define LNOFLSH 0

/* COFF related */
#define COFF
#define NO_REMAP
#define SECTION_ALIGNMENT 0x1
#define COFF_BSD_SYMBOLS
#define etext __etext
#define edata __edata
#define _start __text

/* Compilation options */
#define LIBS_DEBUG
#define ORDINARY_LINK
/* we define following to prevent all the lynxos's stupid compilation */
/* warning messages */
#define C_SWITCH_SYSTEM -D__NO_INCLUDE_WARN__
#define LIBS_SYSTEM -lbsd

/* arch-tag: fbc81ec9-1c45-416b-a368-799ae7c094a1
   (do not change this comment) */
