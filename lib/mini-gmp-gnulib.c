/* Tailor mini-gmp.c for Gnulib-using applications.

   Copyright 2018-2020 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>

#include <stddef.h>
#include <stdio.h>

#include "mini-gmp.h"

/* Pacify GCC -Wsuggest-attribute=const, pure, malloc.  */
#if 4 < __GNUC__ + (6 <= __GNUC_MINOR__)
# pragma GCC diagnostic ignored "-Wsuggest-attribute=const"
# pragma GCC diagnostic ignored "-Wsuggest-attribute=pure"
#endif
#if 8 <= __GNUC__
# pragma GCC diagnostic ignored "-Wsuggest-attribute=malloc"
#endif

/* Pacify GCC -Wunused-variable for variables used only in 'assert' calls.  */
#if defined NDEBUG && 4 < __GNUC__ + (6 <= __GNUC_MINOR__)
# pragma GCC diagnostic ignored "-Wunused-variable"
#endif

#include "mini-gmp.c"
