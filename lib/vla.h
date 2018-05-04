/* vla.h - variable length arrays

   Copyright 2014-2018 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.

   Written by Paul Eggert.  */

/* A function's argument must point to an array with at least N elements.
   Example: 'int main (int argc, char *argv[VLA_ELEMS (argc)]);'.  */

#ifdef __STDC_NO_VLA__
# define VLA_ELEMS(n)
#else
# define VLA_ELEMS(n) static n
#endif
