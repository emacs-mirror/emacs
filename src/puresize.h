/* How much read-only Lisp storage a dumped Emacs needs.
   Copyright (C) 1993 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* # bytes of pure Lisp code to leave space for.

   At one point, this was defined in config.h, meaning that changing
   PURESIZE would make Make recompile all of Emacs.  But only a few
   files actually use PURESIZE, so we split it out to its own .h file.

   Make sure to include this file after config.h, since that tells us
   whether we are running X windows, which tells us how much pure
   storage to allocate.  */

#ifndef PURESIZE
#ifdef MULTI_FRAME
#define PURESIZE 220000
#else
#define PURESIZE 190000
#endif
#endif

#ifdef VIRT_ADDR_VARIES

/* For machines like APOLLO where text and data can go anywhere
   in virtual memory.  */
#define CHECK_IMPURE(obj) \
  { extern int pure[]; \
    if ((PNTR_COMPARISON_TYPE) XPNTR (obj) < (PNTR_COMPARISON_TYPE) ((char *) pure + PURESIZE) \
	&& (PNTR_COMPARISON_TYPE) XPNTR (obj) >= (PNTR_COMPARISON_TYPE) pure) \
      pure_write_error (); }

#else /* not VIRT_ADDR_VARIES */
#ifdef PNTR_COMPARISON_TYPE

/* when PNTR_COMPARISON_TYPE is not the default (unsigned int) */
#define CHECK_IMPURE(obj) \
  { extern int my_edata; \
    if ((PNTR_COMPARISON_TYPE) XPNTR (obj) < (PNTR_COMPARISON_TYPE) &my_edata) \
      pure_write_error (); }

#else /* not VIRT_ADDRESS_VARIES, not PNTR_COMPARISON_TYPE */

#define CHECK_IMPURE(obj) \
  { extern int my_edata; \
    if (XPNTR (obj) < (unsigned int) &my_edata) \
      pure_write_error (); }

#endif /* PNTR_COMPARISON_TYPE */
#endif /* VIRT_ADDRESS_VARIES */

