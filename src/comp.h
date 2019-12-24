/* Elisp native compiler definitions
Copyright (C) 2012-2019 Free Software Foundation, Inc.

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

#ifndef COMP_H
#define COMP_H

#ifdef HAVE_NATIVE_COMP

#include <dynlib.h>

struct Lisp_Native_Comp_Unit
{
  union vectorlike_header header;
  /* Original eln file loaded. */
  Lisp_Object file;
  /* Analogous to the constant vector but per compilation unit.  */
  Lisp_Object data_vec;
  dynlib_handle_ptr handle;
};

INLINE bool
NATIVE_COMP_UNITP (Lisp_Object a)
{
  return PSEUDOVECTORP (a, PVEC_NATIVE_COMP_UNIT);
}

INLINE struct Lisp_Native_Comp_Unit *
XNATIVE_COMP_UNIT (Lisp_Object a)
{
  eassert (NATIVE_COMP_UNITP (a));
  return XUNTAG (a, Lisp_Vectorlike, struct Lisp_Native_Comp_Unit);
}

/* Defined in comp.c.  */
extern void load_comp_unit (struct Lisp_Native_Comp_Unit *);
extern void syms_of_comp (void);
/* Fill the freloc structure. Must be called before any eln is loaded.  */
extern void fill_freloc (void);
/* Return 1 if freloc is filled or 0 otherwise.  */
extern int filled_freloc (void);

#endif
#endif
