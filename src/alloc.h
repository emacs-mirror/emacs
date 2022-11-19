/* Allocation-related definitions, used by comp.c

 Copyright (C) 2019-2022 Free Software Foundation, Inc.

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
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#ifndef ALLOC_H
#define ALLOC_H

#ifdef HAVE_NATIVE_COMP

extern const size_t block_align;

extern const size_t float_block_floats_length;
extern const size_t float_block_gcmarkbits_length;

extern const size_t cons_block_conses_length;
extern const size_t cons_block_gcmarkbits_length;

#endif /* #ifndef HAVE_NATIVE_COMP */

#endif /* #ifndef ALLOC_H */
