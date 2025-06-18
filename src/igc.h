/* Fundamental definitions for GNU Emacs Lisp interpreter. -*- coding: utf-8 -*-

Copyright (C) 2024-2025 Free Software Foundation, Inc.

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

enum igc_obj_type
{
  IGC_OBJ_INVALID,
  IGC_OBJ_PAD,
  IGC_OBJ_FWD,
  IGC_OBJ_CONS,
  IGC_OBJ_SYMBOL,
  IGC_OBJ_INTERVAL,
  IGC_OBJ_STRING,
  IGC_OBJ_STRING_DATA,
  IGC_OBJ_VECTOR,
  IGC_OBJ_MARKER_VECTOR,
  IGC_OBJ_ITREE_TREE,
  IGC_OBJ_ITREE_NODE,
  IGC_OBJ_IMAGE,
  IGC_OBJ_IMAGE_CACHE,
  IGC_OBJ_FACE,
  IGC_OBJ_FACE_CACHE,
  IGC_OBJ_FLOAT,
  IGC_OBJ_BLV,
  IGC_OBJ_HANDLER,
  IGC_OBJ_BYTES,
  IGC_OBJ_BUILTIN_SYMBOL,
  IGC_OBJ_BUILTIN_THREAD,
  IGC_OBJ_BUILTIN_SUBR,
  IGC_OBJ_DUMPED_CHARSET_TABLE,
  IGC_OBJ_DUMPED_CODE_SPACE_MASKS,
  IGC_OBJ_DUMPED_BUFFER_TEXT,
  IGC_OBJ_DUMPED_BIGNUM_DATA,
  IGC_OBJ_DUMPED_BYTES,
  IGC_OBJ_WEAK_HASH_TABLE_WEAK_PART,
  IGC_OBJ_WEAK_HASH_TABLE_STRONG_PART,
  IGC_OBJ_NUM_TYPES
};

#ifdef HAVE_MPS

void igc_break (void);
void init_igc (void);
void syms_of_igc (void);
void *igc_thread_add (struct thread_state *ts);
void igc_thread_remove (void **info);
void igc_on_idle (void);
void igc_on_pdump_loaded (void *dump_base,
			  void *hot_start, void *hot_end,
			  void *cold_start, void *cold_end,
			  void *cold_user_data_start,
			  void *heap_end);
void igc_on_face_cache_change (void *face_cache);

void igc_process_messages (void);
Lisp_Object igc_make_cons (Lisp_Object car, Lisp_Object cdr);
void igc_add_marker (struct buffer *b, struct Lisp_Marker *m);
void igc_remove_marker (struct buffer *b, struct Lisp_Marker *m);
void igc_remove_all_markers (struct buffer *b);
void igc_resurrect_markers (struct buffer *b);
Lisp_Object igc_alloc_symbol (void);
void *igc_alloc_global_ref (void);

struct Lisp_Buffer_Local_Value *igc_alloc_blv (void);
void *igc_alloc_handler (void);
void *igc_xzalloc_ambig (size_t size);
void *igc_realloc_ambig (void *block, size_t size);
#ifdef ENABLE_CHECKING
void igc_check_freeable (void *p);
#endif
Lisp_Object *igc_xalloc_lisp_objs_exact (size_t n);
void * igc_xalloc_raw_exact (size_t n);

void *igc_xpalloc_ambig (void *pa, ptrdiff_t *nitems,
			 ptrdiff_t nitems_incr_min, ptrdiff_t nitems_max,
			 ptrdiff_t item_size);

typedef int igc_scan_result_t; /* zero means success */
struct igc_ss;
typedef igc_scan_result_t (*igc_scan_area_t) (struct igc_ss *ss, void *start,
					      void *end, void *closure);
igc_scan_result_t igc_fix12_obj (struct igc_ss *ss, Lisp_Object *addr);
void igc_xpalloc_exact (void **pa_cell, ptrdiff_t *nitems,
			ptrdiff_t nitems_incr_min, ptrdiff_t nitems_max,
			ptrdiff_t item_size, igc_scan_area_t scan,
			void *closure);

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
void igc_grow_rdstack (struct read_stack *rs);
Lisp_Object *igc_make_hash_table_vec (size_t n);
struct Lisp_Weak_Hash_Table_Strong_Part *igc_alloc_weak_hash_table_strong_part(hash_table_weakness_t, size_t, size_t);
struct Lisp_Weak_Hash_Table_Weak_Part *igc_alloc_weak_hash_table_weak_part(hash_table_weakness_t, size_t, size_t);
void *igc_alloc_bytes (size_t nbytes);
struct image_cache *igc_make_image_cache (void);
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
specpdl_ref igc_park_arena (void);
void igc_postmortem (void);
void igc_replace_specpdl (volatile union specbinding *old_specpdl, ptrdiff_t old_nitems,
			  volatile union specbinding *new_specpdl, ptrdiff_t new_nitems);
void igc_on_alloc_main_thread_specpdl (void);
void igc_on_alloc_main_thread_bc (void);
void igc_on_staticpros_complete (void);
void igc_collect (void);
void igc_root_create_ambig (void *start, void *end, const char *debug_name);
void igc_root_create_exact (Lisp_Object *start, Lisp_Object *end);
void igc_root_create_exact_ptr (void *var_addr);
void igc_root_destroy_comp_unit (struct Lisp_Native_Comp_Unit *u);
void igc_root_destroy_comp_unit_eph (struct Lisp_Native_Comp_Unit *u);
void *igc_root_create_n (Lisp_Object start[], size_t n);
void igc_destroy_root_with_start (void *start);
size_t igc_header_size (void);
char *igc_dump_finish_obj (void *client, enum igc_obj_type type,
			   char *base, char *end);
void igc_dump_check_object_starts (Lisp_Object relocs, void *dump_base,
				   void *hot_start, void *hot_end,
				   void *cold_start, void *heap_end);
void *igc_alloc_dump (size_t nbytes);
bool igc_busy_p (void);
Lisp_Object igc_discard_killed_buffers (Lisp_Object list);

# define eassert_not_mps() eassert (false)
#else
# define igc_break() (void) 0
# define eassert_not_mps() (void) 0
#endif // HAVE_MPS

#endif // EMACS_IGC_H
