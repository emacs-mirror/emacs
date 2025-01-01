/* Tailor mini-gmp.c for Gnulib-using applications.

   Copyright 2018-2025 Free Software Foundation, Inc.

   This file is free software.
   It is dual-licensed under "the GNU LGPLv3+ or the GNU GPLv2+".
   You can redistribute it and/or modify it under either
     - the terms of the GNU Lesser General Public License as published
       by the Free Software Foundation, either version 3, or (at your
       option) any later version, or
     - the terms of the GNU General Public License as published by the
       Free Software Foundation; either version 2, or (at your option)
       any later version, or
     - the same dual license "the GNU LGPLv3+ or the GNU GPLv2+".

   This file is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License and the GNU General Public License
   for more details.

   You should have received a copy of the GNU Lesser General Public
   License and of the GNU General Public License along with this
   program.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>

#include <stddef.h>
#include <stdio.h>

#include "mini-gmp.h"

/* Pacify GCC -Wsuggest-attribute=const, pure, malloc.  */
#if _GL_GNUC_PREREQ (4, 6)
# pragma GCC diagnostic ignored "-Wsuggest-attribute=const"
# pragma GCC diagnostic ignored "-Wsuggest-attribute=pure"
#endif
#if _GL_GNUC_PREREQ (8, 0)
# pragma GCC diagnostic ignored "-Wsuggest-attribute=malloc"
#endif

/* Pacify GCC -Wunused-variable for variables used only in 'assert' calls.  */
#if (defined NDEBUG \
     && (4 < __GNUC__ + (6 <= __GNUC_MINOR__) || defined __clang__))
# pragma GCC diagnostic ignored "-Wunused-variable"
#endif

#include "mini-gmp.c"
