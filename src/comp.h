/* Elisp native compiler definitions

Copyright (C) 2019-2025 Free Software Foundation, Inc.

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

// clang-format off

#include <dynlib.h>
# include "lisp.h"

#ifdef HAVE_MPS
typedef struct igc_root_list *gc_root_t;
#else
typedef void *gc_root_t;
#endif

/* Shared .eln objects contain printed representations of Lisp vectors
   representing constant Lisp objects used in native-compiled
   code. There are two of these vectors: one for top-level code, which
   is called "ephemeral", and one for other code.

   When an .eln is loaded, the Lisp reader is used to construct Lisp
   vectors from these printed representations (which are C strings in
   the text segment). This Lisp vector is then saved to protect the
   constant objects from GC, and its contents are additionally copied to
   vectors in the data segment of the .eln to avoid an additional
   indirection when accessing them.

   With igc, the vectors in the data segment are made exact roots, and
   there are quite a lot of them. With my init file, this add up to
   around 1.5 MB. To avoid these roots, an alternative can be used:

   Compile the shared object to contain a pointer to the contents member
   of the Lisp vectors that has been read, instead of copying the
   vector's contents to the data segment. Access to constants then
   requires an additional indirection, but the GC latency is less. It's
   something like the difference between

   static Lisp_Object constants[42];
   Lisp_Object constant = constants[17];

   and

   static Lisp_Object **constants;
   Lisp_Object constant = (*constants)[17];

   The advantage of the first method is slightly faster native code, the
   advantage of the second method is slightly less latency of
   incremental GC.

   Define USE_POINTER_TO_CONSTANTS to use the second method. */

#ifdef USE_POINTER_TO_CONSTANTS
typedef Lisp_Object **comp_data_vector_t;
#else
typedef Lisp_Object *comp_data_vector_t;
#endif

struct Lisp_Native_Comp_Unit
{
  struct vectorlike_header header;

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

  /* Temporarily used to GC protect a vector of constants used
     during the execution of top-level code.  */
  Lisp_Object data_eph_vec;

  /* A Lisp vector read from a string contained in the text segment of
     the .eln (TEXT_DATA_RELOC_SYM). The elements of the vector are
     constants used in the native code. */
  Lisp_Object data_vec;

  /* STUFF WE DO NOT DUMP!!  */

  /* Pointer into the data segment where the compilation unit is
     stored (COMP_UNIT_SYM), and an exact root for it.  */
  Lisp_Object *comp_unit;
  gc_root_t comp_unit_root;

  /* Pointers into data segment where constant vectors are found. */
  comp_data_vector_t data_relocs;
  comp_data_vector_t data_eph_relocs;

  /* Size of the vectors above. 1 in the USE_POINTER_TO_CONSTANTS
     case. */
  size_t n_data_relocs;
  size_t n_data_eph_relocs;

  /* Exact roots for the vectors. */
  gc_root_t data_relocs_root;
  gc_root_t data_eph_relocs_root;

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
