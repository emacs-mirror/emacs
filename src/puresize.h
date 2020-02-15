/* How much read-only Lisp storage a dumped Emacs needs.
   Copyright (C) 1993, 2001-2020 Free Software Foundation, Inc.

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

#ifndef EMACS_PURESIZE_H
#define EMACS_PURESIZE_H

#include "lisp.h"

INLINE_HEADER_BEGIN

/* Define PURESIZE, the number of bytes of pure Lisp code to leave space for.

   At one point, this was defined in config.h, meaning that changing
   PURESIZE would make Make recompile all of Emacs.  But only a few
   files actually use PURESIZE, so we split it out to its own .h file.

   Make sure to include this file after config.h, since that tells us
   whether we are running X windows, which tells us how much pure
   storage to allocate.  */

/* First define a measure of the amount of data we have.  */

/* A system configuration file may set this to request a certain extra
   amount of storage.  This is a lot more update-robust that defining
   BASE_PURESIZE or even PURESIZE directly.  */
#ifndef SYSTEM_PURESIZE_EXTRA
#define SYSTEM_PURESIZE_EXTRA 0
#endif

#ifndef SITELOAD_PURESIZE_EXTRA
#define SITELOAD_PURESIZE_EXTRA 0
#endif

#ifndef BASE_PURESIZE
#define BASE_PURESIZE (2000000 + SYSTEM_PURESIZE_EXTRA + SITELOAD_PURESIZE_EXTRA)
#endif

/* Increase BASE_PURESIZE by a ratio depending on the machine's word size.  */
#ifndef PURESIZE_RATIO
#if EMACS_INT_MAX >> 31 != 0
#if PTRDIFF_MAX >> 31 != 0
#define PURESIZE_RATIO 10 / 6	/* Don't surround with `()'.  */
#else
#define PURESIZE_RATIO 8 / 6	/* Don't surround with `()'.  */
#endif
#else
#define PURESIZE_RATIO 1
#endif
#endif

#ifdef ENABLE_CHECKING
/* ENABLE_CHECKING somehow increases the purespace used, probably because
   it tends to cause some macro arguments to be evaluated twice.  This is
   a bug, but it's difficult to track it down.  */
#define PURESIZE_CHECKING_RATIO 12 / 10	/* Don't surround with `()'.  */
#else
#define PURESIZE_CHECKING_RATIO 1
#endif

/* This is the actual size in bytes to allocate.  */
#ifndef PURESIZE
#define PURESIZE  (BASE_PURESIZE * PURESIZE_RATIO * PURESIZE_CHECKING_RATIO)
#endif

extern AVOID pure_write_error (Lisp_Object);

extern EMACS_INT pure[];

/* The puresize_h_* macros are private to this include file.  */

/* True if PTR is pure.  */

#define puresize_h_PURE_P(ptr) \
  ((uintptr_t) (ptr) - (uintptr_t) pure <= PURESIZE)

INLINE bool
PURE_P (void *ptr)
{
  return puresize_h_PURE_P (ptr);
}

/* Signal an error if OBJ is pure.  PTR is OBJ untagged.  */

#define puresize_h_CHECK_IMPURE(obj, ptr) \
  (PURE_P (ptr) ? pure_write_error (obj) : (void) 0)

INLINE void
CHECK_IMPURE (Lisp_Object obj, void *ptr)
{
  puresize_h_CHECK_IMPURE (obj, ptr);
}

#if DEFINE_KEY_OPS_AS_MACROS
# define PURE_P(ptr) puresize_h_PURE_P (ptr)
# define CHECK_IMPURE(obj, ptr) puresize_h_CHECK_IMPURE (obj, ptr)
#endif

INLINE_HEADER_END

#endif /* EMACS_PURESIZE_H */
