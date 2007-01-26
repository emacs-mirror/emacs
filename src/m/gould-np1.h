/* machine description file for Gould NP1 with UTX/32 3.0 (first release for NP1)
   Copyright (C) 1986, 2001, 2002, 2003, 2004, 2005,
                 2006, 2007  Free Software Foundation, Inc.

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

/* The following line tells the configuration script what sort of
   operating system this machine is likely to run.
   USUAL-OPSYS="bsd4-3"  */

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */
/* UTX 3.0 uses a cross between COFF and a.out format, but closer to COFF. */
/* at least currently, already defined by cpp, but make sure */
#ifndef COFF
#define COFF
#endif COFF

#include "gould.h"

/* undefine what gould.h defined */
#undef ADJUST_EXEC_HEADER

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */
/* UTX 3.0 uses a cross between COFF and a.out format, but closer to COFF. */
#ifndef COFF	/* at least currently, already defined by cpp */
#define COFF
#endif COFF

/* make Gould NP1 and PN COFF look like USG COFF */
/* NP1 COFF */
#undef aouthdr /* Since gould.h already defined these */
#undef a_dtbase

#ifdef IN_UNEXEC
#define aouthdr exec
#define ADJUST_TEXT_SCNHDR_SIZE

/* Gould COFF - these are already defined in gould.h */
/*
 * #define COFF_WITH_BSD_SYMTAB
 * #define HEADER_INCL_IN_TEXT
 * #define magic a_magic
 * #define tsize a_text
 * #define dsize a_data
 * #define bsize a_bss
 * #define entry a_entry
 * #define text_start a_txbase
 * #define data_start a_dtbase
*/
/* End Gould COFF */
#endif /* IN_UNEXEC */

/* NP1 supports a slightly different set than PowerNode */
#define BAUD_CONVERT	{ 0, 50, 75, 110, 134, 150, 300, 450, 600, 1200, \
			      1800, 2000, 2400, 3600, 4800, 7200, 9600, \
				  19200, 38400 }

#define LD_SWITCH_SYSTEM -BS -e start

/* Undef C_DEBUG_SWITCH because it may have been set in gould.h */
/* It will compile and load and works with dbx. Runs under an incomplete
   port of gdb, but gdb doesn't always find things correctly. */
#undef C_DEBUG_SWITCH
#define C_DEBUG_SWITCH -g
#define LIBS_DEBUG -lg


/* The data segment in this machine always starts at address 0x1000000 = 16M.
   An address of data cannot be stored correctly in a Lisp object;
   we always lose the high bits.  We must tell XPNTR to add them back.  */

#define DATA_SEG_BITS 0x1000000
#define DATA_START    0x1000000

/* The text segment always starts at 0.
   This way we don't need to have a label _start defined.  */
#define TEXT_START 0

/* Data isn't right next to text on an NP1 */
#define NO_REMAP

/* The bcopy bug has reappeared */
#undef BSTRING

#ifndef GOULD_NP1
#define GOULD_NP1
#endif


/* arch-tag: cdfd3dbf-a5e4-464d-8cef-985fb7872873
   (do not change this comment) */
