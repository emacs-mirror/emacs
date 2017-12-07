/* Static heap allocation for GNU Emacs.

Copyright 2016-2017 Free Software Foundation, Inc.

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

#include <stddef.h>
#include "lisp.h"

/* Size of the static heap.  Guess a value that is probably too large,
   by up to a factor of four or so.  Typically the unused part is not
   paged in and so does not cost much.  */
enum { STATIC_HEAP_SIZE = sizeof (Lisp_Object) << 22 };

extern char bss_sbrk_buffer[STATIC_HEAP_SIZE];
extern char *max_bss_sbrk_ptr;
extern bool bss_sbrk_did_unexec;
extern void *bss_sbrk (ptrdiff_t);
