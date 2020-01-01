/* Tailor mini-gmp.c for GNU Emacs

Copyright 2018-2020 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>

#include <stddef.h>

/* Pacify GCC -Wsuggest-attribute=malloc.  */
static void *gmp_default_alloc (size_t) ATTRIBUTE_MALLOC;

/* Pacify GCC -Wunused-variable for variables used only in 'assert' calls.  */
#if defined NDEBUG && GNUC_PREREQ (4, 6, 0)
# pragma GCC diagnostic ignored "-Wunused-variable"
#endif

#include "mini-gmp.c"
