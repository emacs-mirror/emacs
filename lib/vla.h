/* vla.h - variable length arrays

   Copyright 2014-2020 Free Software Foundation, Inc.

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

/* The VLA_ELEMS macro does not allocate variable-length arrays (VLAs),
   so it does not have the security or performance issues commonly
   associated with VLAs.  VLA_ELEMS is for exploiting a C11 feature
   where a function can start like this:

     double scan_array (int n, double v[static n])

   to require a caller to pass a vector V with at least N elements;
   this allows better static checking and performance in some cases.
   In C11 this feature means that V is a VLA, so the feature is
   supported only if __STDC_NO_VLA__ is defined, and for compatibility
   to platforms that do not support VLAs, VLA_ELEMS (n) expands to
   nothing when __STDC_NO_VLA__ is not defined.  */

/* A function's argument must point to an array with at least N elements.
   Example: 'int main (int argc, char *argv[VLA_ELEMS (argc)]);'.  */

#ifdef __STDC_NO_VLA__
# define VLA_ELEMS(n)
#else
# define VLA_ELEMS(n) static n
#endif

/* Although C99 requires support for variable-length arrays (VLAs),
   some C compilers never supported VLAs and VLAs are optional in C11.
   VLAs are controversial because their allocation may be unintended
   or awkward to support, and large VLAs might cause security or
   performance problems.  GCC can diagnose the use of VLAs via the
   -Wvla and -Wvla-larger-than warnings options, and defining the
   macro GNULIB_NO_VLA disables the allocation of VLAs in Gnulib code.

   The VLA_ELEMS macro is unaffected by GNULIB_NO_VLA, since it does
   not allocate VLAs.  Programs that use VLA_ELEMS should be compiled
   with 'gcc -Wvla-larger-than' instead of with 'gcc -Wvla'.  */
