/* Pointer bounds checking for GNU Emacs

Copyright 2017-2020 Free Software Foundation, Inc.

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

/* Pointer bounds checking is a no-op unless running on hardware
   supporting Intel MPX (Intel Skylake or better).  Also, it requires
   GCC 5 and Linux kernel 3.19, or later.  Configure with
   CFLAGS='-fcheck-pointer-bounds -mmpx', perhaps with
   -fchkp-first-field-has-own-bounds thrown in.

   Although pointer bounds checking can help during debugging, it is
   disabled by default because it hurts performance significantly.
   The checking does not detect all pointer errors.  For example, a
   dumped Emacs might not detect a bounds violation of a pointer that
   was created before Emacs was dumped.  */

#ifndef PTR_BOUNDS_H
#define PTR_BOUNDS_H

#include <stddef.h>

/* When not checking pointer bounds, the following macros simply
   return their first argument.  These macros return either void *, or
   the same type as their first argument.  */

INLINE_HEADER_BEGIN

/* Return a copy of P, with bounds narrowed to [P, P + N).  */
#ifdef __CHKP__
INLINE void *
ptr_bounds_clip (void const *p, size_t n)
{
  return __builtin___bnd_narrow_ptr_bounds (p, p, n);
}
#else
# define ptr_bounds_clip(p, n) ((void) (size_t) {n}, p)
#endif

/* Return a copy of P, but with the bounds of Q.  */
#ifdef __CHKP__
# define ptr_bounds_copy(p, q) __builtin___bnd_copy_ptr_bounds (p, q)
#else
# define ptr_bounds_copy(p, q) ((void) (void const *) {q}, p)
#endif

/* Return a copy of P, but with infinite bounds.
   This is a loophole in pointer bounds checking.  */
#ifdef __CHKP__
# define ptr_bounds_init(p) __builtin___bnd_init_ptr_bounds (p)
#else
# define ptr_bounds_init(p) (p)
#endif

/* Return a copy of P, but with bounds [P, P + N).
   This is a loophole in pointer bounds checking.  */
#ifdef __CHKP__
# define ptr_bounds_set(p, n) __builtin___bnd_set_ptr_bounds (p, n)
#else
# define ptr_bounds_set(p, n) ((void) (size_t) {n}, p)
#endif

INLINE_HEADER_END

#endif /* PTR_BOUNDS_H */
