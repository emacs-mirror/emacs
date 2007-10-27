/*
 * External symbol setup file for GNU Emacs on CX/UX
 * Copyright (C) 1990, 2002, 2003, 2004, 2005,
 *               2006, 2007 Free Software Foundation, Inc.
 *
 * This file is part of GNU Emacs.
 * 
 * GNU Emacs is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 * 
 * GNU Emacs is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with GNU Emacs; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
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
