/* How much read-only Lisp storage a dumped Emacs needs.
   Copyright (C) 1993, 2002 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

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
#define BASE_PURESIZE (900000 + SYSTEM_PURESIZE_EXTRA + SITELOAD_PURESIZE_EXTRA)
#endif

/* Increase BASE_PURESIZE by a ratio depending on the machine's word size.  */
#ifndef PURESIZE_RATIO
#if VALBITS + GCTYPEBITS + 1 > 32
#define PURESIZE_RATIO 9/5	/* Don't surround with `()'. */
#else
#define PURESIZE_RATIO 1
#endif
#endif

/* This is the actual size in bytes to allocate.  */
#ifndef PURESIZE
#define PURESIZE  (BASE_PURESIZE * PURESIZE_RATIO)
#endif

/* Signal an error if OBJ is pure.  */
#define CHECK_IMPURE(obj) \
  { if (PURE_P (obj))	  \
      pure_write_error (); }

extern void pure_write_error P_ ((void));

/* Define PURE_P.  */

#ifdef VIRT_ADDR_VARIES
/* For machines like APOLLO where text and data can go anywhere
   in virtual memory.  */

extern EMACS_INT pure[];

#define PURE_P(obj) \
 ((PNTR_COMPARISON_TYPE) XPNTR (obj) < (PNTR_COMPARISON_TYPE) ((char *) pure + PURESIZE) \
  && (PNTR_COMPARISON_TYPE) XPNTR (obj) >= (PNTR_COMPARISON_TYPE) pure)

#else /* not VIRT_ADDR_VARIES */
#ifdef PNTR_COMPARISON_TYPE
/* When PNTR_COMPARISON_TYPE is not the default (unsigned int).  */

extern char my_edata[];

#define PURE_P(obj) \
  ((PNTR_COMPARISON_TYPE) XPNTR (obj) < (PNTR_COMPARISON_TYPE) my_edata)

#else /* not VIRT_ADDRESS_VARIES, not PNTR_COMPARISON_TYPE */

extern char my_edata[];
    
#define PURE_P(obj) \
  (XPNTR (obj) < (unsigned int) my_edata)

#endif /* PNTR_COMPARISON_TYPE */
#endif /* VIRT_ADDRESS_VARIES */
