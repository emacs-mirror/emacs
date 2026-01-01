/* Elisp native compiler definitions

Copyright (C) 2019-2026 Free Software Foundation, Inc.

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

#include <dynlib.h>

struct Lisp_Native_Comp_Unit
{
  union vectorlike_header header;
  /* The original eln file loaded.  In the pdumper file this is stored
     as a cons cell of 2 alternative file names: the car is the
     filename relative to the directory of an installed binary, the
     cdr is the filename relative to the directory of an uninstalled
     binary.  This is arranged in loadup.el.  */
  Lisp_Object file;
  Lisp_Object optimize_qualities;
  /* Guard anonymous lambdas against Garbage Collection and serve
     sanity checks.  */
  Lisp_Object lambda_gc_guard_h;
  /* Hash c_name -> d_reloc index.  */
  Lisp_Object lambda_c_name_idx_h;
  /* Hash doc-idx -> function documentation.  */
  Lisp_Object data_fdoc_v;
  /* Analogous to the constant vector but per compilation unit.  Must be
     last.  */
  Lisp_Object data_vec;
  /* STUFFS WE DO NOT DUMP!!  */
  Lisp_Object *data_relocs;
  bool loaded_once;
  bool load_ongoing;
  dynlib_handle_ptr handle;
} GCALIGNED_STRUCT;

#ifdef HAVE_NATIVE_COMP

INLINE_HEADER_BEGIN

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

extern void hash_native_abi (void);

extern Lisp_Object load_comp_unit (struct Lisp_Native_Comp_Unit *comp_u,
				   bool loading_dump, bool late_load);

extern void unload_comp_unit (struct Lisp_Native_Comp_Unit *);

extern Lisp_Object native_function_doc (Lisp_Object function);

extern void syms_of_comp (void);

extern void maybe_defer_native_compilation (Lisp_Object function_name,
					    Lisp_Object definition);

extern void eln_load_path_final_clean_up (void);

extern void fixup_eln_load_path (Lisp_Object directory);

#else /* #ifdef HAVE_NATIVE_COMP */

static inline void
maybe_defer_native_compilation (Lisp_Object function_name,
				Lisp_Object definition)
{}

static inline
void unload_comp_unit (struct Lisp_Native_Comp_Unit *cu)
{}

extern void syms_of_comp (void);

INLINE_HEADER_END

#endif /* #ifdef HAVE_NATIVE_COMP */

#endif /* #ifndef COMP_H */
