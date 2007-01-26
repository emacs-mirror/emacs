/*
 * External symbol setup file for GNU Emacs on CX/UX
 * Copyright (C) 1990, 2002, 2003, 2004, 2005,
 *               2006, 2007 Free Software Foundation, Inc.
 *
 * This file is part of GNU Emacs.
 *
 * GNU Emacs is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY.  No author or distributor
 * accepts responsibility to anyone for the consequences of using it
 * or for whether it serves any particular purpose or works at all,
 * unless he says so in writing.  Refer to the GNU Emacs General Public
 * License for full details.
 *
 * Everyone is granted permission to copy, modify and redistribute
 * GNU Emacs, but only under the conditions described in the
 * GNU Emacs General Public License.   A copy of this license is
 * supposed to have been given to you along with GNU Emacs so you
 * can know your rights and responsibilities.  It should be in a
 * file named COPYING.  Among other things, the copyright notice
 * and this notice must be preserved on all copies.
 */

/*
 * This file makes the start of the text and data regions of the program
 * clearly visible to the GNU Emacs C source code, without any dependencies
 * on any changes made to the standard C runtime startup module, crt0.o.
 * It depends, however, on this file being passed down to the linker (ld)
 * before any others, and the linker's behavior of assigning increasing
 * addresses as it finds symbols.
 */
	/* C symbol _start marks beginning of text region. */
	.text
	.globl __start
__start:
	/* C symbol data_start marks beginning of data region. */
	.data
	.globl _data_start
_data_start:	.space 4

/* arch-tag: ba84e4dc-615d-4a81-898c-f5b98ec71c9d
   (do not change this comment) */
