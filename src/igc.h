/* Fundamental definitions for GNU Emacs Lisp interpreter. -*- coding: utf-8 -*-

Copyright (C) 2024 Free Software Foundation, Inc.

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

#ifndef EMACS_IGC_H
#define EMACS_IGC_H

#include "config.h"
#include "lisp.h"

#ifdef HAVE_MPS

/* Assertions.  */
# define IGC_DEBUG 1

/* If defined, allocate conses from MPS.  */
# define IGC_MANAGE_CONS 1
# define IGC_MANAGE_SYMBOLS 1

/* If defined, use a debug AMS pool, and check fenceposts etc.
   See MPS docs.  Can be slow.  */
# define IGC_DEBUG_POOL 1

void igc_break (void);
void init_igc (void);
void syms_of_igc (void);
void *igc_thread_add (struct thread_state *ts);
void igc_thread_remove (void *info);
void igc_on_idle (void);
void igc_on_pdump_loaded (void *start, void *end);
void igc_on_face_cache_change (void *face_cache);

void igc_process_messages (void);
Lisp_Object igc_make_cons (Lisp_Object car, Lisp_Object cdr);
Lisp_Object igc_alloc_symbol (void);

struct Lisp_Buffer_Local_Value *igc_alloc_blv (void);
void *igc_xzalloc_ambig (size_t size);
void igc_xfree (void *p);
Lisp_Object *igc_xalloc_lisp_objs_exact (size_t n);
void *igc_xpalloc_ambig (void *pa, ptrdiff_t *nitems, ptrdiff_t nitems_incr_min,
		   ptrdiff_t nitems_max, ptrdiff_t item_size);
void *igc_xnrealloc_ambig (void *pa, ptrdiff_t nitems, ptrdiff_t item_size);

struct Lisp_Vector *igc_alloc_pseudovector (size_t nwords_mem,
					    size_t nwords_lisp,
					    size_t nwords_zero,
					    enum pvec_type tag);
struct Lisp_Vector *igc_alloc_vector (ptrdiff_t len);
struct Lisp_Vector *igc_alloc_record (ptrdiff_t len);
struct itree_node *igc_make_itree_node (void);
struct itree_tree *igc_make_itree_tree (void);
struct image *igc_make_image (void);
struct face *igc_make_face (void);
struct face_cache *igc_make_face_cache (void);
struct interval *igc_make_interval (void);

Lisp_Object igc_make_string (size_t nchars, size_t nbytes, bool unibyte,
			     bool clear);
Lisp_Object igc_make_multibyte_string (size_t nchars, size_t nbytes,
				       bool clear);
Lisp_Object igc_make_unibyte_string (size_t nchars, size_t nbytes, bool clear);
Lisp_Object igc_make_float (double val);
int igc_valid_lisp_object_p (Lisp_Object obj);
unsigned char *igc_replace_char (Lisp_Object string, ptrdiff_t at_byte_pos,
				 ptrdiff_t old_char_len,
				 ptrdiff_t new_char_len);
size_t igc_hash (Lisp_Object key);
void igc_create_charset_root (void *table, size_t size);
specpdl_ref igc_park_arena (void);
void igc_check_vector (const struct Lisp_Vector *v);
void igc_postmortem (void);
void igc_on_grow_specpdl (void);
void igc_on_alloc_main_thread_specpdl (void);
void igc_on_alloc_main_thread_bc (void);
void igc_check_symbol (void *p);
void igc_collect (void);
void igc_root_create_ambig (void *start, void *end);
void igc_root_create_exact (Lisp_Object *start, Lisp_Object *end);
void igc_root_create_exact_ptr (void *var_addr);

# define eassert_not_mps() eassert (false)
#else
# define igc_break() (void) 0
# define eassert_not_mps() (void) 0
#endif // HAVE_MPS

#endif // EMACS_IGC_H
