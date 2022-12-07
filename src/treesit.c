/* Tree-sitter integration for GNU Emacs.

Copyright (C) 2021-2022 Free Software Foundation, Inc.

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

#include <config.h>
#include "lisp.h"
#include "buffer.h"

#include "treesit.h"

#if HAVE_TREE_SITTER


/* Dynamic loading of libtree-sitter.  */

#ifdef WINDOWSNT
# include "w32common.h"

/* In alphabetical order.  */
#undef ts_language_version
#undef ts_node_child
#undef ts_node_child_by_field_name
#undef ts_node_child_count
#undef ts_node_descendant_for_byte_range
#undef ts_node_end_byte
#undef ts_node_eq
#undef ts_node_field_name_for_child
#undef ts_node_first_child_for_byte
#undef ts_node_first_named_child_for_byte
#undef ts_node_has_error
#undef ts_node_is_extra
#undef ts_node_is_missing
#undef ts_node_is_named
#undef ts_node_is_null
#undef ts_node_named_child
#undef ts_node_named_child_count
#undef ts_node_named_descendant_for_byte_range
#undef ts_node_next_named_sibling
#undef ts_node_next_sibling
#undef ts_node_parent
#undef ts_node_prev_named_sibling
#undef ts_node_prev_sibling
#undef ts_node_start_byte
#undef ts_node_string
#undef ts_node_type
#undef ts_parser_delete
#undef ts_parser_included_ranges
#undef ts_parser_language
#undef ts_parser_new
#undef ts_parser_parse
#undef ts_parser_set_included_ranges
#undef ts_parser_set_language
#undef ts_query_capture_name_for_id
#undef ts_query_cursor_delete
#undef ts_query_cursor_exec
#undef ts_query_cursor_new
#undef ts_query_cursor_next_match
#undef ts_query_cursor_set_byte_range
#undef ts_query_delete
#undef ts_query_new
#undef ts_query_predicates_for_pattern
#undef ts_query_string_value_for_id
#undef ts_set_allocator
#undef ts_tree_cursor_current_node
#undef ts_tree_cursor_goto_first_child
#undef ts_tree_cursor_goto_next_sibling
#undef ts_tree_cursor_goto_parent
#undef ts_tree_cursor_new
#undef ts_tree_delete
#undef ts_tree_edit
#undef ts_tree_get_changed_ranges
#undef ts_tree_root_node

DEF_DLL_FN (uint32_t, ts_language_version, (const TSLanguage *));
DEF_DLL_FN (TSNode, ts_node_child, (TSNode, uint32_t));
DEF_DLL_FN (TSNode, ts_node_child_by_field_name,
	    (TSNode, const char *, uint32_t));
DEF_DLL_FN (uint32_t, ts_node_child_count, (TSNode));
DEF_DLL_FN (TSNode, ts_node_descendant_for_byte_range,
	    (TSNode, uint32_t, uint32_t));
DEF_DLL_FN (uint32_t, ts_node_end_byte, (TSNode));
DEF_DLL_FN (bool, ts_node_eq, (TSNode, TSNode));
DEF_DLL_FN (const char *, ts_node_field_name_for_child, (TSNode, uint32_t));
DEF_DLL_FN (TSNode, ts_node_first_child_for_byte, (TSNode, uint32_t));
DEF_DLL_FN (TSNode, ts_node_first_named_child_for_byte, (TSNode, uint32_t));
DEF_DLL_FN (bool, ts_node_has_error, (TSNode));
DEF_DLL_FN (bool, ts_node_is_extra, (TSNode));
DEF_DLL_FN (bool, ts_node_is_missing, (TSNode));
DEF_DLL_FN (bool, ts_node_is_named, (TSNode));
DEF_DLL_FN (bool, ts_node_is_null, (TSNode));
DEF_DLL_FN (TSNode, ts_node_named_child, (TSNode, uint32_t));
DEF_DLL_FN (uint32_t, ts_node_named_child_count, (TSNode));
DEF_DLL_FN (TSNode, ts_node_named_descendant_for_byte_range,
	    (TSNode, uint32_t, uint32_t));
DEF_DLL_FN (TSNode, ts_node_next_named_sibling, (TSNode));
DEF_DLL_FN (TSNode, ts_node_next_sibling, (TSNode));
DEF_DLL_FN (TSNode, ts_node_parent, (TSNode));
DEF_DLL_FN (TSNode, ts_node_prev_named_sibling, (TSNode));
DEF_DLL_FN (TSNode, ts_node_prev_sibling, (TSNode));
DEF_DLL_FN (uint32_t, ts_node_start_byte, (TSNode));
DEF_DLL_FN (char *, ts_node_string, (TSNode));
DEF_DLL_FN (const char *, ts_node_type, (TSNode));
DEF_DLL_FN (void, ts_parser_delete, (TSParser *));
DEF_DLL_FN (const TSRange *, ts_parser_included_ranges,
	    (const TSParser *, uint32_t *));
DEF_DLL_FN (const TSLanguage *, ts_parser_language, (const TSParser *));
DEF_DLL_FN (TSParser *, ts_parser_new, (void));
DEF_DLL_FN (TSTree *, ts_parser_parse, (TSParser *, const TSTree *, TSInput));
DEF_DLL_FN (bool, ts_parser_set_included_ranges,
	    (TSParser *, const TSRange *, uint32_t));
DEF_DLL_FN (bool, ts_parser_set_language, (TSParser *, const TSLanguage *));
DEF_DLL_FN (const char *, ts_query_capture_name_for_id,
	    (const TSQuery *, uint32_t, uint32_t *));
DEF_DLL_FN (void, ts_query_cursor_delete, (TSQueryCursor *));
DEF_DLL_FN (void, ts_query_cursor_exec,
	    (TSQueryCursor *, const TSQuery *, TSNode));
DEF_DLL_FN (TSQueryCursor *, ts_query_cursor_new, (void));
DEF_DLL_FN (bool, ts_query_cursor_next_match,
	    (TSQueryCursor *, TSQueryMatch *));
DEF_DLL_FN (void, ts_query_cursor_set_byte_range,
	    (TSQueryCursor *, uint32_t, uint32_t));
DEF_DLL_FN (void, ts_query_delete, (TSQuery *));
DEF_DLL_FN (TSQuery *, ts_query_new,
	    (const TSLanguage *, const char *, uint32_t, uint32_t *, TSQueryError *));
DEF_DLL_FN (const TSQueryPredicateStep *, ts_query_predicates_for_pattern,
	    ( const TSQuery *, uint32_t, uint32_t *));
DEF_DLL_FN (const char *, ts_query_string_value_for_id,
	    (const TSQuery *, uint32_t, uint32_t *));
DEF_DLL_FN (void, ts_set_allocator,
	    (void *(*)(size_t), void *(*)(size_t, size_t), void *(*)(void *, size_t), void (*)(void *)));
DEF_DLL_FN (TSNode, ts_tree_cursor_current_node, (const TSTreeCursor *));
DEF_DLL_FN (bool, ts_tree_cursor_goto_first_child, (TSTreeCursor *));
DEF_DLL_FN (bool, ts_tree_cursor_goto_next_sibling, (TSTreeCursor *));
DEF_DLL_FN (bool, ts_tree_cursor_goto_parent, (TSTreeCursor *));
DEF_DLL_FN (TSTreeCursor, ts_tree_cursor_new, (TSNode));
DEF_DLL_FN (void, ts_tree_delete, (TSTree *));
DEF_DLL_FN (void, ts_tree_edit, (TSTree *, const TSInputEdit *));
DEF_DLL_FN (TSRange *, ts_tree_get_changed_ranges,
	    (const TSTree *, const TSTree *, uint32_t *));
DEF_DLL_FN (TSNode, ts_tree_root_node, (const TSTree *));

static bool
init_treesit_functions (void)
{
  HMODULE library = w32_delayed_load (Qtree_sitter);

  if (!library)
    return false;

  LOAD_DLL_FN (library, ts_language_version);
  LOAD_DLL_FN (library, ts_node_child);
  LOAD_DLL_FN (library, ts_node_child_by_field_name);
  LOAD_DLL_FN (library, ts_node_child_count);
  LOAD_DLL_FN (library, ts_node_descendant_for_byte_range);
  LOAD_DLL_FN (library, ts_node_end_byte);
  LOAD_DLL_FN (library, ts_node_eq);
  LOAD_DLL_FN (library, ts_node_field_name_for_child);
  LOAD_DLL_FN (library, ts_node_first_child_for_byte);
  LOAD_DLL_FN (library, ts_node_first_named_child_for_byte);
  LOAD_DLL_FN (library, ts_node_has_error);
  LOAD_DLL_FN (library, ts_node_is_extra);
  LOAD_DLL_FN (library, ts_node_is_missing);
  LOAD_DLL_FN (library, ts_node_is_named);
  LOAD_DLL_FN (library, ts_node_is_null);
  LOAD_DLL_FN (library, ts_node_named_child);
  LOAD_DLL_FN (library, ts_node_named_child_count);
  LOAD_DLL_FN (library, ts_node_named_descendant_for_byte_range);
  LOAD_DLL_FN (library, ts_node_next_named_sibling);
  LOAD_DLL_FN (library, ts_node_next_sibling);
  LOAD_DLL_FN (library, ts_node_parent);
  LOAD_DLL_FN (library, ts_node_prev_named_sibling);
  LOAD_DLL_FN (library, ts_node_prev_sibling);
  LOAD_DLL_FN (library, ts_node_start_byte);
  LOAD_DLL_FN (library, ts_node_string);
  LOAD_DLL_FN (library, ts_node_type);
  LOAD_DLL_FN (library, ts_parser_delete);
  LOAD_DLL_FN (library, ts_parser_included_ranges);
  LOAD_DLL_FN (library, ts_parser_language);
  LOAD_DLL_FN (library, ts_parser_new);
  LOAD_DLL_FN (library, ts_parser_parse);
  LOAD_DLL_FN (library, ts_parser_set_included_ranges);
  LOAD_DLL_FN (library, ts_parser_set_language);
  LOAD_DLL_FN (library, ts_query_capture_name_for_id);
  LOAD_DLL_FN (library, ts_query_cursor_delete);
  LOAD_DLL_FN (library, ts_query_cursor_exec);
  LOAD_DLL_FN (library, ts_query_cursor_new);
  LOAD_DLL_FN (library, ts_query_cursor_next_match);
  LOAD_DLL_FN (library, ts_query_cursor_set_byte_range);
  LOAD_DLL_FN (library, ts_query_delete);
  LOAD_DLL_FN (library, ts_query_new);
  LOAD_DLL_FN (library, ts_query_predicates_for_pattern);
  LOAD_DLL_FN (library, ts_query_string_value_for_id);
  LOAD_DLL_FN (library, ts_set_allocator);
  LOAD_DLL_FN (library, ts_tree_cursor_current_node);
  LOAD_DLL_FN (library, ts_tree_cursor_goto_first_child);
  LOAD_DLL_FN (library, ts_tree_cursor_goto_next_sibling);
  LOAD_DLL_FN (library, ts_tree_cursor_goto_parent);
  LOAD_DLL_FN (library, ts_tree_cursor_new);
  LOAD_DLL_FN (library, ts_tree_delete);
  LOAD_DLL_FN (library, ts_tree_edit);
  LOAD_DLL_FN (library, ts_tree_get_changed_ranges);
  LOAD_DLL_FN (library, ts_tree_root_node);

  return true;
}

#define ts_language_version fn_ts_language_version
#define ts_node_child fn_ts_node_child
#define ts_node_child_by_field_name fn_ts_node_child_by_field_name
#define ts_node_child_count fn_ts_node_child_count
#define ts_node_descendant_for_byte_range fn_ts_node_descendant_for_byte_range
#define ts_node_end_byte fn_ts_node_end_byte
#define ts_node_eq fn_ts_node_eq
#define ts_node_field_name_for_child fn_ts_node_field_name_for_child
#define ts_node_first_child_for_byte fn_ts_node_first_child_for_byte
#define ts_node_first_named_child_for_byte fn_ts_node_first_named_child_for_byte
#define ts_node_has_error fn_ts_node_has_error
#define ts_node_is_extra fn_ts_node_is_extra
#define ts_node_is_missing fn_ts_node_is_missing
#define ts_node_is_named fn_ts_node_is_named
#define ts_node_is_null fn_ts_node_is_null
#define ts_node_named_child fn_ts_node_named_child
#define ts_node_named_child_count fn_ts_node_named_child_count
#define ts_node_named_descendant_for_byte_range fn_ts_node_named_descendant_for_byte_range
#define ts_node_next_named_sibling fn_ts_node_next_named_sibling
#define ts_node_next_sibling fn_ts_node_next_sibling
#define ts_node_parent fn_ts_node_parent
#define ts_node_prev_named_sibling fn_ts_node_prev_named_sibling
#define ts_node_prev_sibling fn_ts_node_prev_sibling
#define ts_node_start_byte fn_ts_node_start_byte
#define ts_node_string fn_ts_node_string
#define ts_node_type fn_ts_node_type
#define ts_parser_delete fn_ts_parser_delete
#define ts_parser_included_ranges fn_ts_parser_included_ranges
#define ts_parser_language fn_ts_parser_language
#define ts_parser_new fn_ts_parser_new
#define ts_parser_parse fn_ts_parser_parse
#define ts_parser_set_included_ranges fn_ts_parser_set_included_ranges
#define ts_parser_set_language fn_ts_parser_set_language
#define ts_query_capture_name_for_id fn_ts_query_capture_name_for_id
#define ts_query_cursor_delete fn_ts_query_cursor_delete
#define ts_query_cursor_exec fn_ts_query_cursor_exec
#define ts_query_cursor_new fn_ts_query_cursor_new
#define ts_query_cursor_next_match fn_ts_query_cursor_next_match
#define ts_query_cursor_set_byte_range fn_ts_query_cursor_set_byte_range
#define ts_query_delete fn_ts_query_delete
#define ts_query_new fn_ts_query_new
#define ts_query_predicates_for_pattern fn_ts_query_predicates_for_pattern
#define ts_query_string_value_for_id fn_ts_query_string_value_for_id
#define ts_set_allocator fn_ts_set_allocator
#define ts_tree_cursor_current_node fn_ts_tree_cursor_current_node
#define ts_tree_cursor_goto_first_child fn_ts_tree_cursor_goto_first_child
#define ts_tree_cursor_goto_next_sibling fn_ts_tree_cursor_goto_next_sibling
#define ts_tree_cursor_goto_parent fn_ts_tree_cursor_goto_parent
#define ts_tree_cursor_new fn_ts_tree_cursor_new
#define ts_tree_delete fn_ts_tree_delete
#define ts_tree_edit fn_ts_tree_edit
#define ts_tree_get_changed_ranges fn_ts_tree_get_changed_ranges
#define ts_tree_root_node fn_ts_tree_root_node

#endif	/* WINDOWSNT */


/* Commentary

   The Emacs wrapper of tree-sitter does not expose everything the C
   API provides, most notably:

   - It doesn't expose a syntax tree.  The syntax tree is part of the
     parser object, and updating the tree is handled on the C level.

   - It doesn't expose the tree cursor, either.  Presumably, Lisp is
     slow enough to make insignificant any performance advantages from
     using the cursor.  Not exposing the cursor also minimizes the
     number of new types this adds to Emacs Lisp; currently, this adds
     only the parser and node types.

   - Because updating the change is handled on the C level as each
     change is made in the buffer, there is no way for Lisp to update
     a node.  But since we can just retrieve a new node, it shouldn't
     be a limitation.

   - I didn't expose setting timeout and cancellation flag for a
     parser, mainly because I don't think they are really necessary
     in Emacs's use cases.

   - Many tree-sitter functions take a TSPoint, which is basically a
     row and column.  Emacs uses a gap buffer and does not keep
     information about the row and column position of a buffer.
     According to the author of tree-sitter, those functions only take
     a TSPoint so that it can be moved alongside the byte position and
     returned to the caller afterwards, and the position actually used
     is the specified byte position.  He also said that he _thinks_
     that just passing a byte position will also work.  As a result, a
     dummy value is used in place of each TSPoint.  Judging by the
     nature of parsing algorithms, I think it is safe to use only the
     byte position, and I don't think this will change in the future.

     See: https://github.com/tree-sitter/tree-sitter/issues/445

   treesit.h has some commentary on the two main data structure for
   the parser and node.  treesit_sync_visible_region has some
   commentary on how we make tree-sitter play well with narrowing (the
   tree-sitter parser only sees the visible region, so we need to
   translate positions back and forth).  Most action happens in
   treesit_ensure_parsed, treesit_read_buffer and
   treesit_record_change.

   A complete correspondence list between tree-sitter functions and
   exposed Lisp functions can be found in the manual node (elisp)API
   Correspondence.

   Placement of CHECK_xxx functions: call CHECK_xxx before using any
   unchecked Lisp values; these include arguments of Lisp functions,
   the return value of Fsymbol_value, and that of Fcar or Fcdr on
   user-specified conses.

   Initializing tree-sitter: there are two entry points to tree-sitter
   functions: 'treesit-parser-create' and
   'treesit-language-available-p'.  Technically we only need to call
   initialization function in those two functions, but in reality we
   check at the beginning of every Lisp function.  That should be more
   fool-proof.

   Tree-sitter offset (0-based) and buffer position (1-based):
     tree-sitter offset + buffer position = buffer position
     buffer position - buffer position = tree-sitter offset

   Tree-sitter-related code in other files:
   - src/alloc.c for gc for parser and node
   - src/casefiddle.c & src/insdel.c for notifying tree-sitter
     parser of buffer changes.
   - lisp/emacs-lisp/cl-preloaded.el & data.c & lisp.h for parser and
     node type.
   - print.c for printing tree-sitter objects (node, parser, query).

   Regarding signals: only raise signals in Lisp functions.

   Casts from EMACS_INT and ptrdiff_t to uint32_t: We install checks
   for buffer size and range and thus able to assume these casts never
   overflow.

   We don't parse at every keystroke.  Instead we only record the
   changes at each keystroke, and only parse when requested.  It is
   possible that lazy parsing is worse: instead of dispersed little
   pauses, now you have less frequent but larger pauses.  I doubt
   there will be any perceived difference, as the lazy parsing is
   going to be pretty frequent anyway.  Also this (lazy parsing) is
   what the mailing list guys wanted.

   Because it is pretty slow (comparing to other tree-sitter
   operations) for tree-sitter to parse the query and produce a query
   object, it is very wasteful to reparse the query every time
   treesit-query-capture is called, and it completely kills the
   performance of querying in a loop for a moderate amount of times
   (hundreds of queries takes seconds rather than milliseconds to
   complete).  Therefore we want some caching.  We can either use a
   search.c style transparent caching, or simply expose a new type,
   compiled-ts-query and let the user to manually compile AOT.  I
   believe AOT compiling gives users more control, makes the
   performance stable and easy to understand (compiled -> fast,
   uncompiled -> slow), and avoids some edge cases transparent cache
   could have (see below).  So I implemented the AOT compilation.

   Problems a transparent cache could have: Suppose we store cache
   entries in a fixed-length linked-list, and compare with EQ.  1)
   One-off query could kick out useful cache.  2) if the user messed
   up and the query doesn't EQ to the cache anymore, the performance
   mysteriously drops.  3) what if a user uses so many stuff that the
   default cache size (20) is not enough and we end up thrashing?
   These are all imaginary scenarios but they are not impossible
   :-)

   Parsers in indirect buffers: We make indirect buffers to share the
   parser of its base buffer.  Indirect buffers and their base buffer
   share the same buffer content but not other buffer attributes.  If
   they have separate parser lists, changes made in an indirect buffer
   will only update parsers of that indirect buffer, and not parsers
   in the base buffer or other indirect buffers, and vice versa.  We
   could keep track of all the base and indirect buffers, and update
   all of their parsers, but ultimately decide to take a simpler
   approach, which is to make indirect buffers share their base
   buffer's parser list.  The discussion can be found in bug#59693.  */


/*** Initialization */

bool treesit_initialized = false;

static bool
load_tree_sitter_if_necessary (bool required)
{
#ifdef WINDOWSNT
  static bool tried_to_initialize_once;
  static bool tree_sitter_initialized;

  if (!tried_to_initialize_once)
    {
      Lisp_Object status;

      tried_to_initialize_once = true;
      tree_sitter_initialized = init_treesit_functions ();
      status = tree_sitter_initialized ? Qt : Qnil;
      Vlibrary_cache = Fcons (Fcons (Qtree_sitter, status), Vlibrary_cache);
    }

  if (required && !tree_sitter_initialized)
    xsignal1 (Qtreesit_error,
	      build_string ("tree-sitter library not found or failed to load"));

  return tree_sitter_initialized;
#else
  return true;
#endif
}

static void *
treesit_calloc_wrapper (size_t n, size_t size)
{
  return xzalloc (n * size);
}

static void
treesit_initialize (void)
{
  if (!treesit_initialized)
    {
      load_tree_sitter_if_necessary (true);
      ts_set_allocator (xmalloc, treesit_calloc_wrapper, xrealloc, xfree);
      treesit_initialized = true;
    }
}


/*** Loading language library */

/* Translates a symbol treesit-<lang> to a C name
   treesit_<lang>.  */
static void
treesit_symbol_to_c_name (char *symbol_name)
{
  for (int idx = 0; idx < strlen (symbol_name); idx++)
    {
      if (symbol_name[idx] == '-')
	symbol_name[idx] = '_';
    }
}

static bool
treesit_find_override_name (Lisp_Object language_symbol, Lisp_Object *name,
			    Lisp_Object *c_symbol)
{
  Lisp_Object tem;

  CHECK_LIST (Vtreesit_load_name_override_list);

  tem = Vtreesit_load_name_override_list;

  FOR_EACH_TAIL (tem)
    {
      Lisp_Object lang = XCAR (XCAR (tem));
      CHECK_SYMBOL (lang);

      if (EQ (lang, language_symbol))
	{
	  *name = Fnth (make_fixnum (1), XCAR (tem));
	  CHECK_STRING (*name);
	  *c_symbol = Fnth (make_fixnum (2), XCAR (tem));
	  CHECK_STRING (*c_symbol);

	  return true;
	}
    }

  CHECK_LIST_END (tem, Vtreesit_load_name_override_list);

  return false;
}

/* For example, if Vdynamic_library_suffixes is (".so", ".dylib"),
   this function pushes "lib_base_name.so" and "lib_base_name.dylib"
   into *path_candidates.  Obviously path_candidates should be a Lisp
   list of Lisp strings.  */
static void
treesit_load_language_push_for_each_suffix (Lisp_Object lib_base_name,
					    Lisp_Object *path_candidates)
{
  Lisp_Object suffixes;

  suffixes = Vdynamic_library_suffixes;

  FOR_EACH_TAIL (suffixes)
    *path_candidates = Fcons (concat2 (lib_base_name, XCAR (suffixes)),
			      *path_candidates);
}

/* Load the dynamic library of LANGUAGE_SYMBOL and return the pointer
   to the language definition.

   If error occurs, return NULL and fill SIGNAL_SYMBOL and SIGNAL_DATA
   with values suitable for xsignal.  */
static TSLanguage *
treesit_load_language (Lisp_Object language_symbol,
		       Lisp_Object *signal_symbol, Lisp_Object *signal_data)
{
  Lisp_Object symbol_name = Fsymbol_name (language_symbol);

  CHECK_LIST (Vtreesit_extra_load_path);

  /* Figure out the library name and C name.  */
  Lisp_Object lib_base_name
    = concat2 (build_pure_c_string ("libtree-sitter-"), symbol_name);
  Lisp_Object base_name
    = concat2 (build_pure_c_string ("tree-sitter-"), symbol_name);

  /* Override the library name and C name, if appropriate.  */
  Lisp_Object override_name;
  Lisp_Object override_c_name;
  bool found_override = treesit_find_override_name (language_symbol,
						    &override_name,
						    &override_c_name);
  if (found_override)
    lib_base_name = override_name;

  /* Now we generate a list of possible library paths.  */
  Lisp_Object path_candidates = Qnil;
  /* First push just the filenames to the candidate list, which will
     make dynlib_open look under standard system load paths.  */
  treesit_load_language_push_for_each_suffix (lib_base_name, &path_candidates);
  /* This is used for reporting errors (i.e., just filenames).  */
  Lisp_Object base_candidates = path_candidates;
  /* Then push ~/.emacs.d/tree-sitter paths.  */
  Lisp_Object lib_name
    = Fexpand_file_name (concat2 (build_string ("tree-sitter/"), lib_base_name),
			 Fsymbol_value (Quser_emacs_directory));
  treesit_load_language_push_for_each_suffix (lib_name, &path_candidates);
  /* Then push paths from treesit-extra-load-path.  */
  Lisp_Object tail;

  tail = Freverse (Vtreesit_extra_load_path);

  FOR_EACH_TAIL (tail)
    {
      Lisp_Object expanded_lib = Fexpand_file_name (lib_base_name, XCAR (tail));
      treesit_load_language_push_for_each_suffix (expanded_lib,
						  &path_candidates);
    }

  /* Try loading the dynamic library by each path candidate.  Stop
     when succeed, record the error message and try the next one when
     fail.  */
  dynlib_handle_ptr handle;
  const char *error;

  tail = path_candidates;
  error = NULL;
  handle = NULL;

  FOR_EACH_TAIL (tail)
    {
      char *library_name = SSDATA (XCAR (tail));
      dynlib_error ();
      handle = dynlib_open (library_name);
      error = dynlib_error ();
      if (error == NULL)
	break;
    }

  if (error != NULL)
    {
      *signal_symbol = Qtreesit_load_language_error;
      *signal_data = list3 (Qnot_found, base_candidates,
			    build_string ("No such file or directory"));
      return NULL;
    }

  /* Load TSLanguage.  */
  eassume (handle != NULL);
  dynlib_error ();
  TSLanguage *(*langfn) (void);
  char *c_name = xstrdup (SSDATA (base_name));
  treesit_symbol_to_c_name (c_name);
  if (found_override)
    c_name = SSDATA (override_c_name);
  langfn = dynlib_sym (handle, c_name);
  xfree (c_name);
  error = dynlib_error ();
  if (error != NULL)
    {
      *signal_symbol = Qtreesit_load_language_error;
      *signal_data = list2 (Qsymbol_error, build_string (error));
      return NULL;
    }
  TSLanguage *lang = (*langfn) ();

  /* Check if language version matches tree-sitter version.  */
  TSParser *parser = ts_parser_new ();
  bool success = ts_parser_set_language (parser, lang);
  ts_parser_delete (parser);
  if (!success)
    {
      *signal_symbol = Qtreesit_load_language_error;
      *signal_data = list2 (Qversion_mismatch,
			    make_fixnum (ts_language_version (lang)));
      return NULL;
    }
  return lang;
}

DEFUN ("treesit-language-available-p", Ftreesit_langauge_available_p,
       Streesit_language_available_p,
       1, 2, 0,
       doc: /* Return non-nil if LANGUAGE exists and is loadable.

If DETAIL is non-nil, return (t . nil) when LANGUAGE is available,
(nil . DATA) when unavailable.  DATA is the signal data of
`treesit-load-language-error'.  */)
  (Lisp_Object language, Lisp_Object detail)
{
  CHECK_SYMBOL (language);
  treesit_initialize ();
  Lisp_Object signal_symbol = Qnil;
  Lisp_Object signal_data = Qnil;
  if (treesit_load_language (language, &signal_symbol, &signal_data) == NULL)
    {
      if (NILP (detail))
	return Qnil;
      else
	return Fcons (Qnil, signal_data);
    }
  else
    {
      if (NILP (detail))
	return Qt;
      else
	return Fcons (Qt, Qnil);
    }
}

DEFUN ("treesit-language-version",
       Ftreesit_language_version,
       Streesit_language_version,
       0, 1, 0,
       doc: /* Return the language ABI version of the tree-sitter library.

By default, report the latest ABI version supported by the library for
loading language support modules.  The library is backward-compatible
with language modules which use older ABI versions; if MIN-COMPATIBLE
is non-nil, return the oldest compatible ABI version.  */)
  (Lisp_Object min_compatible)
{
  if (NILP (min_compatible))
    return make_fixnum (TREE_SITTER_LANGUAGE_VERSION);
  else
    return make_fixnum (TREE_SITTER_MIN_COMPATIBLE_LANGUAGE_VERSION);
}

/*** Parsing functions */

static void
treesit_check_parser (Lisp_Object obj)
{
  CHECK_TS_PARSER (obj);
  if (XTS_PARSER (obj)->deleted)
    xsignal1 (Qtreesit_parser_deleted, obj);
}

/* An auxiliary function that saves a few lines of code.  Assumes TREE
   is not NULL.  START_BYTE, OLD_END_BYTE, NEW_END_BYTE must not be
   larger than UINT32_MAX.  */
static inline void
treesit_tree_edit_1 (TSTree *tree, ptrdiff_t start_byte,
		     ptrdiff_t old_end_byte, ptrdiff_t new_end_byte)
{
  eassert (start_byte >= 0);
  eassert (start_byte <= old_end_byte);
  eassert (start_byte <= new_end_byte);
  TSPoint dummy_point = {0, 0};
  eassert (start_byte <= UINT32_MAX);
  eassert (old_end_byte <= UINT32_MAX);
  eassert (new_end_byte <= UINT32_MAX);
  TSInputEdit edit = {(uint32_t) start_byte,
		      (uint32_t) old_end_byte,
		      (uint32_t) new_end_byte,
		      dummy_point, dummy_point, dummy_point};
  ts_tree_edit (tree, &edit);
}

/* Update each parser's tree after the user made an edit.  This
   function does not parse the buffer and only updates the tree, so it
   should be very fast.  */
void
treesit_record_change (ptrdiff_t start_byte, ptrdiff_t old_end_byte,
		       ptrdiff_t new_end_byte)
{
  struct buffer *base_buffer = current_buffer;
  if (current_buffer->base_buffer)
    base_buffer = current_buffer->base_buffer;
  Lisp_Object parser_list = BVAR (base_buffer, ts_parser_list);

  FOR_EACH_TAIL_SAFE (parser_list)
    {
      CHECK_CONS (parser_list);
      Lisp_Object lisp_parser = XCAR (parser_list);
      treesit_check_parser (lisp_parser);
      TSTree *tree = XTS_PARSER (lisp_parser)->tree;
      /* See comment (ref:visible-beg-null) if you wonder why we don't
      update visible_beg/end when tree is NULL.  */
      if (tree != NULL)
	{
	  eassert (start_byte <= old_end_byte);
	  eassert (start_byte <= new_end_byte);
	  /* Think the recorded change as a delete followed by an
	     insert, and think of them as moving unchanged text back
	     and forth.  After all, the whole point of updating the
	     tree is to update the position of unchanged text.  */
	  ptrdiff_t visible_beg = XTS_PARSER (lisp_parser)->visible_beg;
	  ptrdiff_t visible_end = XTS_PARSER (lisp_parser)->visible_end;
	  eassert (visible_beg >= 0);
	  eassert (visible_beg <= visible_end);

	  /* AFFECTED_START/OLD_END/NEW_END are (0-based) offsets from
	     VISIBLE_BEG.  min(visi_end, max(visi_beg, value)) clips
	     value into [visi_beg, visi_end], and subtracting visi_beg
	     gives the offset from visi_beg.  */
	  ptrdiff_t start_offset = (min (visible_end,
					 max (visible_beg, start_byte))
				    - visible_beg);
	  ptrdiff_t old_end_offset = (min (visible_end,
					   max (visible_beg, old_end_byte))
				      - visible_beg);
	  ptrdiff_t new_end_offset = (min (visible_end,
					   max (visible_beg, new_end_byte))
				      - visible_beg);
	  eassert (start_offset <= old_end_offset);
	  eassert (start_offset <= new_end_offset);

	  treesit_tree_edit_1 (tree, start_offset, old_end_offset,
			       new_end_offset);
	  XTS_PARSER (lisp_parser)->need_reparse = true;
	  XTS_PARSER (lisp_parser)->timestamp++;

	  /* VISIBLE_BEG/END records tree-sitter's range of view in
	     the buffer.  We need to adjust them when tree-sitter's
	     view changes.  */
	  ptrdiff_t visi_beg_delta;
	  if (old_end_byte > new_end_byte)
	    /* Move backward.  */
	    visi_beg_delta = (min (visible_beg, new_end_byte)
			      - min (visible_beg, old_end_byte));
	  else
	    /* Move forward.  */
	    visi_beg_delta = (old_end_byte < visible_beg
			      ? new_end_byte - old_end_byte : 0);
	  XTS_PARSER (lisp_parser)->visible_beg = visible_beg + visi_beg_delta;
	  XTS_PARSER (lisp_parser)->visible_end = (visible_end
						   + visi_beg_delta
						   + (new_end_offset
						      - old_end_offset));
	  eassert (XTS_PARSER (lisp_parser)->visible_beg >= 0);
	  eassert (XTS_PARSER (lisp_parser)->visible_beg
		   <= XTS_PARSER (lisp_parser)->visible_end);
	}
    }
}

/* Comment (ref:visible-beg-null) The purpose of visible_beg/end is to
   keep track of "which part of the buffer does the tree-sitter tree
   see", in order to update the tree correctly.  Visible_beg/end have
   two purposes: they "clip" buffer changes within them, and they
   translate positions in the buffer to positions in the tree
   (buffer position - visible_beg = tree position).

   Conceptually, visible_beg/end hold the visible region of the buffer
   when we last reparsed.  In-between two reparses, we don't really
   care if the visible region of the buffer changes.

   Right before we reparse, we make tree-sitter's visible region
   match that of the buffer, and update visible_beg/end.

   That is, the whole purpose of visible_beg/end (and also of
   treesit_record_change and treesit_sync_visible_region) is to update
   the tree (by ts_tree_edit).  So if the tree is NULL,
   visible_beg/end are considered uninitialized.  Only when we already
   have a tree, do we need to keep track of position changes and
   update it correctly, so it can be fed into ts_parser_parse as the
   old tree, so that tree-sitter will only parse the changed part,
   incrementally.

   In a nutshell, tree-sitter incremental parsing in Emacs looks like:

   treesit_record_change (tree)  \
   treesit_record_change (tree)  | user edits buffer
   ...                           /

   treesit_sync_visible_region (tree) \ treesit_ensure_parsed
   ts_parser_parse(tree) -> tree      /

   treesit_record_change (tree)  \
   treesit_record_change (tree)  | user edits buffer
   ...                           /

   and so on.  */

/* Make sure the tree's visible range is in sync with the buffer's
   visible range, and PARSER's visible_beg and visible_end are in sync
   with BUF_BEGV_BYTE and BUG_ZV_BYTE.  When calling this function you
   must make sure the current buffer's size in bytes is not larger than
   UINT32_MAX.  Basically, always call treesit_check_buffer_size before
   this function.

   If buffer range changed since last parse (visible_beg/end doesn't
   match buffer visible beginning/end), set need_reparse to true.  */
static void
treesit_sync_visible_region (Lisp_Object parser)
{
  TSTree *tree = XTS_PARSER (parser)->tree;
  struct buffer *buffer = XBUFFER (XTS_PARSER (parser)->buffer);

  /* If we are setting visible_beg/end for the first time, we can skip
  the offset acrobatics and updating the tree below.  */
  if (tree == NULL)
    {
      XTS_PARSER (parser)->visible_beg = BUF_BEGV_BYTE (buffer);
      XTS_PARSER (parser)->visible_end = BUF_ZV_BYTE (buffer);
      return;
    }

  ptrdiff_t visible_beg = XTS_PARSER (parser)->visible_beg;
  ptrdiff_t visible_end = XTS_PARSER (parser)->visible_end;
  eassert (0 <= visible_beg);
  eassert (visible_beg <= visible_end);

  eassert (BUF_BEGV_BYTE (buffer) <= UINT32_MAX);
  eassert (BUF_ZV_BYTE (buffer) <= UINT32_MAX);

  /* If buffer restriction changed and user requests for a node (hence
     this function is called), we need to reparse.  */
  if (visible_beg != BUF_BEGV_BYTE (buffer)
      || visible_end != BUF_ZV_BYTE (buffer))
    XTS_PARSER (parser)->need_reparse = true;

  /* Before we parse or set ranges, catch up with the narrowing
     situation.  We change visible_beg and visible_end to match
     BUF_BEGV_BYTE and BUF_ZV_BYTE, and inform tree-sitter of the
     change.  We want to move the visible range of tree-sitter to
     match the narrowed range.  For example,
     from ________|xxxx|__
     to   |xxxx|__________ */

  /* 1. Make sure visible_beg <= BUF_BEGV_BYTE.  */
  if (visible_beg > BUF_BEGV_BYTE (buffer))
    {
      /* Tree-sitter sees: insert at the beginning.  */
      treesit_tree_edit_1 (tree, 0, 0, visible_beg - BUF_BEGV_BYTE (buffer));
      visible_beg = BUF_BEGV_BYTE (buffer);
      eassert (visible_beg <= visible_end);
    }
  /* 2. Make sure visible_end = BUF_ZV_BYTE.  */
  if (visible_end < BUF_ZV_BYTE (buffer))
    {
      /* Tree-sitter sees: insert at the end.  */
      treesit_tree_edit_1 (tree, visible_end - visible_beg,
			   visible_end - visible_beg,
			   BUF_ZV_BYTE (buffer) - visible_beg);
      visible_end = BUF_ZV_BYTE (buffer);
      eassert (visible_beg <= visible_end);
    }
  else if (visible_end > BUF_ZV_BYTE (buffer))
    {
      /* Tree-sitter sees: delete at the end.  */
      treesit_tree_edit_1 (tree, BUF_ZV_BYTE (buffer) - visible_beg,
			   visible_end - visible_beg,
			   BUF_ZV_BYTE (buffer) - visible_beg);
      visible_end = BUF_ZV_BYTE (buffer);
      eassert (visible_beg <= visible_end);
    }
  /* 3. Make sure visible_beg = BUF_BEGV_BYTE.  */
  if (visible_beg < BUF_BEGV_BYTE (buffer))
    {
      /* Tree-sitter sees: delete at the beginning.  */
      treesit_tree_edit_1 (tree, 0, BUF_BEGV_BYTE (buffer) - visible_beg, 0);
      visible_beg = BUF_BEGV_BYTE (buffer);
      eassert (visible_beg <= visible_end);
    }
  eassert (0 <= visible_beg);
  eassert (visible_beg <= visible_end);
  eassert (visible_beg == BUF_BEGV_BYTE (buffer));
  eassert (visible_end == BUF_ZV_BYTE (buffer));

  XTS_PARSER (parser)->visible_beg = visible_beg;
  XTS_PARSER (parser)->visible_end = visible_end;
}

static void
treesit_check_buffer_size (struct buffer *buffer)
{
  ptrdiff_t buffer_size_bytes = (BUF_Z_BYTE (buffer) - BUF_BEG_BYTE (buffer));
  if (buffer_size_bytes > UINT32_MAX)
    xsignal2 (Qtreesit_buffer_too_large,
	      build_pure_c_string ("Buffer size cannot be larger than 4GB"),
	      make_fixnum (buffer_size_bytes));
}

static Lisp_Object treesit_make_ranges (const TSRange *, uint32_t, struct buffer *);

static void
treesit_call_after_change_functions (TSTree *old_tree, TSTree *new_tree,
				     Lisp_Object parser)
{
  uint32_t len;
  TSRange *ranges = ts_tree_get_changed_ranges (old_tree, new_tree, &len);
  struct buffer *buf = XBUFFER (XTS_PARSER (parser)->buffer);
  Lisp_Object lisp_ranges = treesit_make_ranges (ranges, len, buf);
  xfree (ranges);

  specpdl_ref count = SPECPDL_INDEX ();

  /* let's trust the after change functions and not clone a new ranges
     for each of them.  */
  Lisp_Object functions = XTS_PARSER (parser)->after_change_functions;
  FOR_EACH_TAIL (functions)
    safe_call2 (XCAR (functions), lisp_ranges, parser);

  unbind_to (count, Qnil);
}

/* Parse the buffer.  We don't parse until we have to.  When we have
   to, we call this function to parse and update the tree.  */
static void
treesit_ensure_parsed (Lisp_Object parser)
{
  struct buffer *buffer = XBUFFER (XTS_PARSER (parser)->buffer);

  /* Before we parse, catch up with the narrowing situation.  */
  treesit_check_buffer_size (buffer);
  /* This function has to run before we check for need_reparse flag,
     because it might set the flag to true.  */
  treesit_sync_visible_region (parser);

  if (!XTS_PARSER (parser)->need_reparse)
    return;
  TSParser *treesit_parser = XTS_PARSER (parser)->parser;
  TSTree *tree = XTS_PARSER (parser)->tree;
  TSInput input = XTS_PARSER (parser)->input;

  TSTree *new_tree = ts_parser_parse (treesit_parser, tree, input);
  /* This should be very rare (impossible, really): it only happens
     when 1) language is not set (impossible in Emacs because the user
     has to supply a language to create a parser), 2) parse canceled
     due to timeout (impossible because we don't set a timeout), 3)
     parse canceled due to cancellation flag (impossible because we
     don't set the flag).  (See comments for ts_parser_parse in
     tree_sitter/api.h.)  */
  if (new_tree == NULL)
    {
      Lisp_Object buf;
      XSETBUFFER (buf, buffer);
      xsignal1 (Qtreesit_parse_error, buf);
    }

  if (tree != NULL)
    {
      treesit_call_after_change_functions (tree, new_tree, parser);
      ts_tree_delete (tree);
    }

  XTS_PARSER (parser)->tree = new_tree;
  XTS_PARSER (parser)->need_reparse = false;
}

/* This is the read function provided to tree-sitter to read from a
   buffer.  It reads one character at a time and automatically skips
   the gap.  */
static const char*
treesit_read_buffer (void *parser, uint32_t byte_index,
		     TSPoint position, uint32_t *bytes_read)
{
  struct buffer *buffer = XBUFFER (((struct Lisp_TS_Parser *) parser)->buffer);
  ptrdiff_t visible_beg = ((struct Lisp_TS_Parser *) parser)->visible_beg;
  ptrdiff_t visible_end = ((struct Lisp_TS_Parser *) parser)->visible_end;
  ptrdiff_t byte_pos = byte_index + visible_beg;
  /* We will make sure visible_beg = BUF_BEGV_BYTE before re-parse (in
     treesit_ensure_parsed), so byte_pos will never be smaller than
     BUF_BEG_BYTE.  */
  eassert (visible_beg = BUF_BEGV_BYTE (buffer));
  eassert (visible_end = BUF_ZV_BYTE (buffer));

  /* Read one character.  Tree-sitter wants us to set bytes_read to 0
     if it reads to the end of buffer.  It doesn't say what it wants
     for the return value in that case, so we just give it an empty
     string.  */
  char *beg;
  int len;
  /* This function could run from a user command, so it is better to
     do nothing instead of raising an error.  (It was a pain in the a**
     to decrypt mega-if-conditions in Emacs source, so I wrote the two
     branches separately, you are welcome.)  */
  if (!BUFFER_LIVE_P (buffer))
    {
      beg = NULL;
      len = 0;
    }
  /* Reached visible end-of-buffer, tell tree-sitter to read no more.  */
  else if (byte_pos >= visible_end)
    {
      beg = NULL;
      len = 0;
    }
  /* Normal case, read a character.  */
  else
    {
      beg = (char *) BUF_BYTE_ADDRESS (buffer, byte_pos);
      len = BYTES_BY_CHAR_HEAD ((int) *beg);
    }
  /* We never let tree-sitter to parse buffers that large so this
     assertion should never hit.  */
  eassert (len < UINT32_MAX);
  *bytes_read = (uint32_t) len;
  return beg;
}

/*** Functions for parser and node object */

/* Wrap the parser in a Lisp_Object to be used in the Lisp
   machine.  */
Lisp_Object
make_treesit_parser (Lisp_Object buffer, TSParser *parser,
		     TSTree *tree, Lisp_Object language_symbol)
{
  struct Lisp_TS_Parser *lisp_parser;

  lisp_parser = ALLOCATE_PSEUDOVECTOR (struct Lisp_TS_Parser,
				       buffer, PVEC_TS_PARSER);

  lisp_parser->language_symbol = language_symbol;
  lisp_parser->after_change_functions = Qnil;
  lisp_parser->buffer = buffer;
  lisp_parser->parser = parser;
  lisp_parser->tree = tree;
  TSInput input = {lisp_parser, treesit_read_buffer, TSInputEncodingUTF8};
  lisp_parser->input = input;
  lisp_parser->need_reparse = true;
  lisp_parser->visible_beg = BUF_BEGV_BYTE (XBUFFER (buffer));
  lisp_parser->visible_end = BUF_ZV_BYTE (XBUFFER (buffer));
  lisp_parser->timestamp = 0;
  lisp_parser->deleted = false;
  lisp_parser->has_range = false;
  eassert (lisp_parser->visible_beg <= lisp_parser->visible_end);
  return make_lisp_ptr (lisp_parser, Lisp_Vectorlike);
}

/* Wrap the node in a Lisp_Object to be used in the Lisp machine.  */
Lisp_Object
make_treesit_node (Lisp_Object parser, TSNode node)
{
  struct Lisp_TS_Node *lisp_node;

  lisp_node = ALLOCATE_PSEUDOVECTOR (struct Lisp_TS_Node,
				     parser, PVEC_TS_NODE);
  lisp_node->parser = parser;
  lisp_node->node = node;
  lisp_node->timestamp = XTS_PARSER (parser)->timestamp;
  return make_lisp_ptr (lisp_node, Lisp_Vectorlike);
}

/* Make a compiled query.  QUERY has to be either a cons or a
   string.  */
static Lisp_Object
make_treesit_query (Lisp_Object query, Lisp_Object language)
{
  TSQueryCursor *treesit_cursor = ts_query_cursor_new ();
  struct Lisp_TS_Query *lisp_query;

  lisp_query = ALLOCATE_PSEUDOVECTOR (struct Lisp_TS_Query,
				      source, PVEC_TS_COMPILED_QUERY);

  lisp_query->language = language;
  lisp_query->source = query;
  lisp_query->query = NULL;
  lisp_query->cursor = treesit_cursor;
  return make_lisp_ptr (lisp_query, Lisp_Vectorlike);
}

/* The following two functions are called from alloc.c:cleanup_vector.  */
void
treesit_delete_parser (struct Lisp_TS_Parser *lisp_parser)
{
  ts_tree_delete (lisp_parser->tree);
  ts_parser_delete (lisp_parser->parser);
}

void
treesit_delete_query (struct Lisp_TS_Query *lisp_query)
{
  ts_query_delete (lisp_query->query);
  ts_query_cursor_delete (lisp_query->cursor);
}

/* The following function is called from print.c:print_vectorlike.  */
bool
treesit_named_node_p (TSNode node)
{
  return ts_node_is_named (node);
}

static const char*
treesit_query_error_to_string (TSQueryError error)
{
  switch (error)
    {
    case TSQueryErrorNone:
      return "None";
    case TSQueryErrorSyntax:
      return "Syntax error at";
    case TSQueryErrorNodeType:
      return "Node type error at";
    case TSQueryErrorField:
      return "Field error at";
    case TSQueryErrorCapture:
      return "Capture error at";
    case TSQueryErrorStructure:
      return "Structure error at";
    default:
      return "Unknown error";
    }
}

static Lisp_Object
treesit_compose_query_signal_data (uint32_t error_offset,
				   TSQueryError error_type)
{
  return list3 (build_string (treesit_query_error_to_string (error_type)),
		make_fixnum (error_offset + 1),
		build_pure_c_string ("Debug the query with `treesit-query-validate'"));
}

/* Ensure the QUERY is compiled.  Return the TSQuery.  It could be
   NULL if error occurs, in which case ERROR_OFFSET and ERROR_TYPE are
   bound.  If error occurs, return NULL, and assign SIGNAL_SYMBOL and
   SIGNAL_DATA accordingly.  */
static TSQuery *
treesit_ensure_query_compiled (Lisp_Object query, Lisp_Object *signal_symbol,
			       Lisp_Object *signal_data)
{
  /* If query is already compiled (not null), return that, otherwise
     compile and return it.  */
  TSQuery *treesit_query = XTS_COMPILED_QUERY (query)->query;
  if (treesit_query != NULL)
    return treesit_query;

  /* Get query source and TSLanguage ready.  */
  Lisp_Object source = XTS_COMPILED_QUERY (query)->source;
  Lisp_Object language = XTS_COMPILED_QUERY (query)->language;
  /* This is the main reason why we compile query lazily: to avoid
     loading languages early.  */
  TSLanguage *treesit_lang = treesit_load_language (language, signal_symbol,
						    signal_data);
  if (treesit_lang == NULL)
    return NULL;

  if (CONSP (source))
    source = Ftreesit_query_expand (source);

  /* Create TSQuery.  */
  uint32_t error_offset;
  TSQueryError error_type;
  char *treesit_source = SSDATA (source);
  treesit_query = ts_query_new (treesit_lang, treesit_source,
				strlen (treesit_source),
				&error_offset, &error_type);
  if (treesit_query == NULL)
    {
      *signal_symbol = Qtreesit_query_error;
      *signal_data = treesit_compose_query_signal_data (error_offset,
							error_type);
    }
  XTS_COMPILED_QUERY (query)->query = treesit_query;
  return treesit_query;
}

DEFUN ("treesit-parser-p",
       Ftreesit_parser_p, Streesit_parser_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a tree-sitter parser.  */)
  (Lisp_Object object)
{
  if (TS_PARSERP (object))
    return Qt;
  else
    return Qnil;
}

DEFUN ("treesit-node-p",
       Ftreesit_node_p, Streesit_node_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a tree-sitter node.  */)
  (Lisp_Object object)
{
  if (TS_NODEP (object))
    return Qt;
  else
    return Qnil;
}

DEFUN ("treesit-compiled-query-p",
       Ftreesit_compiled_query_p, Streesit_compiled_query_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a compiled tree-sitter query.  */)
  (Lisp_Object object)
{
  if (TS_COMPILED_QUERY_P (object))
    return Qt;
  else
    return Qnil;
}

DEFUN ("treesit-query-p",
       Ftreesit_query_p, Streesit_query_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a generic tree-sitter query.  */)
  (Lisp_Object object)
{
  if (TS_COMPILED_QUERY_P (object)
      || CONSP (object) || STRINGP (object))
    return Qt;
  else
    return Qnil;
}

DEFUN ("treesit-query-language",
       Ftreesit_query_language, Streesit_query_language, 1, 1, 0,
       doc: /* Return the language of QUERY.
QUERY has to be a compiled query.  */)
  (Lisp_Object query)
{
  CHECK_TS_COMPILED_QUERY (query);
  return XTS_COMPILED_QUERY (query)->language;
}

DEFUN ("treesit-node-parser",
       Ftreesit_node_parser, Streesit_node_parser,
       1, 1, 0,
       doc: /* Return the parser to which NODE belongs.  */)
  (Lisp_Object node)
{
  CHECK_TS_NODE (node);
  return XTS_NODE (node)->parser;
}

DEFUN ("treesit-parser-create",
       Ftreesit_parser_create, Streesit_parser_create,
       1, 3, 0,
       doc: /* Create and return a parser in BUFFER for LANGUAGE.

The parser is automatically added to BUFFER's parser list, as returned
by `treesit-parser-list'.  LANGUAGE is a language symbol.  If BUFFER
is nil or omitted, it defaults to the current buffer.  If BUFFER
already has a parser for LANGUAGE, return that parser, but if NO-REUSE
is non-nil, always create a new parser.

If that buffer is an indirect buffer, its base buffer is used instead.
That is, indirect buffers use their base buffer's parsers.  Lisp
programs should widen as necessary should they want to use a parser in
an indirect buffer.  */)
  (Lisp_Object language, Lisp_Object buffer, Lisp_Object no_reuse)
{
  treesit_initialize ();

  CHECK_SYMBOL (language);
  struct buffer *buf;
  if (NILP (buffer))
    buf = current_buffer;
  else
    {
      CHECK_BUFFER (buffer);
      buf = XBUFFER (buffer);
    }
  if (buf->base_buffer)
    buf = buf->base_buffer;

  treesit_check_buffer_size (buf);

  /* See if we can reuse a parser.  */
  if (NILP (no_reuse))
    {
      Lisp_Object tail = BVAR (buf, ts_parser_list);
      FOR_EACH_TAIL (tail)
      {
	struct Lisp_TS_Parser *parser = XTS_PARSER (XCAR (tail));
	if (EQ (parser->language_symbol, language))
	  return XCAR (tail);
      }
    }

  /* Load language.  */
  Lisp_Object signal_symbol = Qnil;
  Lisp_Object signal_data = Qnil;
  TSParser *parser = ts_parser_new ();
  TSLanguage *lang = treesit_load_language (language, &signal_symbol,
					    &signal_data);
  if (lang == NULL)
    xsignal (signal_symbol, signal_data);
  /* We check language version when loading a language, so this should
     always succeed.  */
  ts_parser_set_language (parser, lang);

  /* Create parser.  */
  Lisp_Object lisp_parser = make_treesit_parser (Fcurrent_buffer (),
						 parser, NULL,
						 language);

  /* Update parser-list.  */
  BVAR (buf, ts_parser_list) = Fcons (lisp_parser, BVAR (buf, ts_parser_list));

  return lisp_parser;
}

DEFUN ("treesit-parser-delete",
       Ftreesit_parser_delete, Streesit_parser_delete,
       1, 1, 0,
       doc: /* Delete PARSER from its buffer's parser list.
See `treesit-parser-list' for the buffer's parser list.  */)
  (Lisp_Object parser)
{
  treesit_check_parser (parser);

  Lisp_Object buffer = XTS_PARSER (parser)->buffer;
  struct buffer *buf = XBUFFER (buffer);

  BVAR (buf, ts_parser_list)
    = Fdelete (parser, BVAR (buf, ts_parser_list));

  XTS_PARSER (parser)->deleted = true;
  return Qnil;
}

DEFUN ("treesit-parser-list",
       Ftreesit_parser_list, Streesit_parser_list,
       0, 1, 0,
       doc: /* Return BUFFER's parser list.

BUFFER defaults to the current buffer.  If that buffer is an indirect
buffer, its base buffer is used instead.  That is, indirect buffers
use their base buffer's parsers.  */)
  (Lisp_Object buffer)
{
  struct buffer *buf;
  if (NILP (buffer))
    buf = current_buffer;
  else
    {
      CHECK_BUFFER (buffer);
      buf = XBUFFER (buffer);
    }
  if (buf->base_buffer)
    buf = buf->base_buffer;

  /* Return a fresh list so messing with that list doesn't affect our
     internal data.  */
  Lisp_Object return_list = Qnil;
  Lisp_Object tail;

  tail = BVAR (buf, ts_parser_list);

  FOR_EACH_TAIL (tail)
    return_list = Fcons (XCAR (tail), return_list);

  return Freverse (return_list);
}

DEFUN ("treesit-parser-buffer",
       Ftreesit_parser_buffer, Streesit_parser_buffer,
       1, 1, 0,
       doc: /* Return the buffer of PARSER.  */)
  (Lisp_Object parser)
{
  treesit_check_parser (parser);
  Lisp_Object buf;
  XSETBUFFER (buf, XBUFFER (XTS_PARSER (parser)->buffer));
  return buf;
}

DEFUN ("treesit-parser-language",
       Ftreesit_parser_language, Streesit_parser_language,
       1, 1, 0,
       doc: /* Return PARSER's language symbol.
This symbol is the one used to create the parser.  */)
  (Lisp_Object parser)
{
  treesit_check_parser (parser);
  return XTS_PARSER (parser)->language_symbol;
}

/*** Parser API */

DEFUN ("treesit-parser-root-node",
       Ftreesit_parser_root_node, Streesit_parser_root_node,
       1, 1, 0,
       doc: /* Return the root node of PARSER.  */)
  (Lisp_Object parser)
{
  treesit_check_parser (parser);
  treesit_initialize ();
  treesit_ensure_parsed (parser);
  TSNode root_node = ts_tree_root_node (XTS_PARSER (parser)->tree);
  return make_treesit_node (parser, root_node);
}

/* Checks that the RANGES argument of
   treesit-parser-set-included-ranges is valid.  */
static void
treesit_check_range_argument (Lisp_Object ranges)
{
  struct buffer *buffer = current_buffer;
  ptrdiff_t point_min = BUF_BEGV (buffer);
  ptrdiff_t point_max = BUF_ZV (buffer);
  EMACS_INT last_point = point_min;
  Lisp_Object tail;

  tail = ranges;

  CHECK_LIST (tail);

  FOR_EACH_TAIL (tail)
    {
      CHECK_CONS (tail);
      Lisp_Object range = XCAR (tail);
      CHECK_CONS (range);
      CHECK_FIXNUM (XCAR (range));
      CHECK_FIXNUM (XCDR (range));
      EMACS_INT beg = XFIXNUM (XCAR (range));
      EMACS_INT end = XFIXNUM (XCDR (range));
      if (!(last_point <= beg && beg <= end && end <= point_max))
	xsignal2 (Qtreesit_range_invalid,
		  build_pure_c_string ("RANGE is either overlapping,"
				       " out-of-order or out-of-range"),
		  ranges);
      last_point = end;
    }

  CHECK_LIST_END (tail, ranges);
}

/* Generate a list of ranges in Lisp from RANGES.  Assumes tree-sitter
   tree and the buffer has the same visible region (wrt narrowing).
   This function doesn't take ownership of RANGES.  BUFFER is used to
   convert between tree-sitter buffer offset and buffer position.  */
static Lisp_Object
treesit_make_ranges (const TSRange *ranges, uint32_t len,
		     struct buffer *buffer)
{
  Lisp_Object list = Qnil;
  for (int idx = 0; idx < len; idx++)
    {
      TSRange range = ranges[idx];
      uint32_t beg_byte = range.start_byte + BUF_BEGV_BYTE (buffer);
      uint32_t end_byte = range.end_byte + BUF_BEGV_BYTE (buffer);
      eassert (BUF_BEGV_BYTE (buffer) <= beg_byte);
      eassert (beg_byte <= end_byte);
      eassert (end_byte <= BUF_ZV_BYTE (buffer));

      Lisp_Object lisp_range
	= Fcons (make_fixnum (buf_bytepos_to_charpos (buffer, beg_byte)),
		 make_fixnum (buf_bytepos_to_charpos (buffer, end_byte)));
      list = Fcons (lisp_range, list);
    }
  return Fnreverse (list);
}

DEFUN ("treesit-parser-set-included-ranges",
       Ftreesit_parser_set_included_ranges,
       Streesit_parser_set_included_ranges,
       2, 2, 0,
       doc: /* Limit PARSER to RANGES.

RANGES is a list of (BEG . END), each (BEG . END) defines a region in
which the parser should operate.  Regions must not overlap, and the
regions should come in order in the list.  Signal
`treesit-set-range-error' if the argument is invalid, or something
else went wrong.  If RANGES is nil, the PARSER is to parse the whole
buffer.  */)
  (Lisp_Object parser, Lisp_Object ranges)
{
  treesit_check_parser (parser);
  if (!NILP (ranges))
    CHECK_CONS (ranges);
  treesit_check_range_argument (ranges);

  treesit_initialize ();
  /* Before we parse, catch up with narrowing/widening.  */
  treesit_check_buffer_size (XBUFFER (XTS_PARSER (parser)->buffer));
  treesit_sync_visible_region (parser);

  bool success;
  if (NILP (ranges))
    {
      XTS_PARSER (parser)->has_range = false;
      /* If RANGES is nil, make parser to parse the whole document.
	 To do that we give tree-sitter a 0 length, the range is a
	 dummy.  */
      TSRange treesit_range = {{0, 0}, {0, 0}, 0, 0};
      success = ts_parser_set_included_ranges (XTS_PARSER (parser)->parser,
					       &treesit_range , 0);
    }
  else
    {
      /* Set ranges for PARSER.  */
      XTS_PARSER (parser)->has_range = true;

      if (list_length (ranges) > UINT32_MAX)
	xsignal (Qargs_out_of_range, list2 (ranges, Flength (ranges)));
      uint32_t len = (uint32_t) list_length (ranges);
      TSRange *treesit_ranges = xmalloc (sizeof (TSRange) * len);
      struct buffer *buffer = XBUFFER (XTS_PARSER (parser)->buffer);

      for (int idx = 0; !NILP (ranges); idx++, ranges = XCDR (ranges))
	{
	  Lisp_Object range = XCAR (ranges);
	  ptrdiff_t beg_byte = buf_charpos_to_bytepos (buffer,
						       XFIXNUM (XCAR (range)));
	  ptrdiff_t end_byte = buf_charpos_to_bytepos (buffer,
						       XFIXNUM (XCDR (range)));
	  /* Shouldn't violate assertion since we just checked for
	     buffer size at the beginning of this function.  */
	  eassert (beg_byte - BUF_BEGV_BYTE (buffer) <= UINT32_MAX);
	  eassert (end_byte - BUF_BEGV_BYTE (buffer) <= UINT32_MAX);
	  /* We don't care about start and end points, put in dummy
	     values.  */
	  TSRange rg = {{0, 0}, {0, 0},
			(uint32_t) beg_byte - BUF_BEGV_BYTE (buffer),
			(uint32_t) end_byte - BUF_BEGV_BYTE (buffer)};
	  treesit_ranges[idx] = rg;
	}
      success = ts_parser_set_included_ranges (XTS_PARSER (parser)->parser,
					       treesit_ranges, len);
      /* Although XFIXNUM could signal, it should be impossible
	 because we have checked the input by treesit_check_range_argument.
	 So there is no need for unwind-protect.  */
      xfree (treesit_ranges);
    }

  if (!success)
    xsignal2 (Qtreesit_range_invalid,
	      build_pure_c_string ("Something went wrong when setting ranges"),
	      ranges);

  XTS_PARSER (parser)->need_reparse = true;
  return Qnil;
}

DEFUN ("treesit-parser-included-ranges",
       Ftreesit_parser_included_ranges,
       Streesit_parser_included_ranges,
       1, 1, 0,
       doc: /* Return the ranges set for PARSER.
If no ranges are set for PARSER, return nil.
See also `treesit-parser-set-included-ranges'.  */)
  (Lisp_Object parser)
{
  treesit_check_parser (parser);
  treesit_initialize ();

  /* When the parser doesn't have a range set and we call
     ts_parser_included_ranges on it, it doesn't return an empty list,
     but rather return some garbled data. (A single range where
     start_byte = 0, end_byte = UINT32_MAX).  So we need to track
     whether the parser is ranged ourselves.  */
  if (!XTS_PARSER (parser)->has_range)
    return Qnil;

  uint32_t len;
  const TSRange *ranges
    = ts_parser_included_ranges (XTS_PARSER (parser)->parser, &len);

  /* Our return value depends on the buffer state (BUF_BEGV_BYTE,
     etc), so we need to sync up.  */
  treesit_check_buffer_size (XBUFFER (XTS_PARSER (parser)->buffer));
  treesit_sync_visible_region (parser);

  struct buffer *buffer = XBUFFER (XTS_PARSER (parser)->buffer);
  return treesit_make_ranges (ranges, len, buffer);
}

DEFUN ("treesit-parser-notifiers", Ftreesit_parser_notifiers,
       Streesit_parser_notifiers,
       1, 1, 0,
       doc: /* Return the list of after-change notifier functions for PARSER.  */)
  (Lisp_Object parser)
{
  treesit_check_parser (parser);

  Lisp_Object return_list = Qnil;
  Lisp_Object tail = XTS_PARSER (parser)->after_change_functions;
  FOR_EACH_TAIL (tail)
    return_list = Fcons (XCAR (tail), return_list);

  return return_list;
}

DEFUN ("treesit-parser-add-notifier", Ftreesit_parser_add_notifier,
       Streesit_parser_add_notifier,
       2, 2, 0,
       doc: /* Add FUNCTION to the list of PARSER's after-change notifiers.
FUNCTION must be a function symbol, rather than a lambda form.
FUNCTION should take 2 arguments, RANGES and PARSER.  RANGES is a list
of cons cells of the form (START . END), where START and END are buffer
positions.  PARSER is the parser issuing the notification.  */)
  (Lisp_Object parser, Lisp_Object function)
{
  treesit_check_parser (parser);
  /* For simplicity we don't accept lambda functions.  */
  CHECK_SYMBOL (function);

  Lisp_Object functions = XTS_PARSER (parser)->after_change_functions;
  if (NILP (Fmemq (function, functions)))
    XTS_PARSER (parser)->after_change_functions = Fcons (function, functions);
  return Qnil;
}

DEFUN ("treesit-parser-remove-notifier", Ftreesit_parser_remove_notifier,
       Streesit_parser_remove_notifier,
       2, 2, 0,
       doc: /* Remove FUNCTION from the list of PARSER's after-change notifiers.
  FUNCTION must be a function symbol, rather than a lambda form.
FUNCTION should take 2 arguments, RANGES and PARSER.  RANGES is a list
of cons of the form (START . END), where START and END are buffer
positions.  PARSER is the parser issuing the notification.   */)
  (Lisp_Object parser, Lisp_Object function)
{
  treesit_check_parser (parser);
  /* For simplicity we don't accept lambda functions.  */
  CHECK_SYMBOL (function);

  Lisp_Object functions = XTS_PARSER (parser)->after_change_functions;
  if (!NILP (Fmemq (function, functions)))
    XTS_PARSER (parser)->after_change_functions = Fdelq (function, functions);
  return Qnil;
}

/*** Node API  */

/* Check that OBJ is a positive integer and signal an error if
   otherwise.  */
static void
treesit_check_positive_integer (Lisp_Object obj)
{
  CHECK_INTEGER (obj);
  if (XFIXNUM (obj) < 0)
    xsignal1 (Qargs_out_of_range, obj);
}

static void
treesit_check_node (Lisp_Object obj)
{
  CHECK_TS_NODE (obj);
  if (!treesit_node_uptodate_p (obj))
    xsignal1 (Qtreesit_node_outdated, obj);
}

/* Checks that OBJ is a positive integer and it is within the visible
   portion of BUF. */
static void
treesit_check_position (Lisp_Object obj, struct buffer *buf)
{
  treesit_check_positive_integer (obj);
  ptrdiff_t pos = XFIXNUM (obj);
  if (pos < BUF_BEGV (buf) || pos > BUF_ZV (buf))
    xsignal1 (Qargs_out_of_range, obj);
}

bool
treesit_node_uptodate_p (Lisp_Object obj)
{
  Lisp_Object lisp_parser = XTS_NODE (obj)->parser;
  return XTS_NODE (obj)->timestamp == XTS_PARSER (lisp_parser)->timestamp;
}

DEFUN ("treesit-node-type",
       Ftreesit_node_type, Streesit_node_type, 1, 1, 0,
       doc: /* Return the NODE's type as a string.
If NODE is nil, return nil.  */)
  (Lisp_Object node)
{
  if (NILP (node)) return Qnil;
  treesit_check_node (node);
  treesit_initialize ();

  TSNode treesit_node = XTS_NODE (node)->node;
  const char *type = ts_node_type (treesit_node);
  return build_string (type);
}

DEFUN ("treesit-node-start",
       Ftreesit_node_start, Streesit_node_start, 1, 1, 0,
       doc: /* Return the NODE's start position in its buffer.
If NODE is nil, return nil.  */)
  (Lisp_Object node)
{
  if (NILP (node)) return Qnil;
  treesit_check_node (node);
  treesit_initialize ();

  TSNode treesit_node = XTS_NODE (node)->node;
  ptrdiff_t visible_beg = XTS_PARSER (XTS_NODE (node)->parser)->visible_beg;
  uint32_t start_byte_offset = ts_node_start_byte (treesit_node);
  struct buffer *buffer
    = XBUFFER (XTS_PARSER (XTS_NODE (node)->parser)->buffer);
  ptrdiff_t start_pos
    = buf_bytepos_to_charpos (buffer,
			      start_byte_offset + visible_beg);
  return make_fixnum (start_pos);
}

DEFUN ("treesit-node-end",
       Ftreesit_node_end, Streesit_node_end, 1, 1, 0,
       doc: /* Return the NODE's end position in its buffer.
If NODE is nil, return nil.  */)
  (Lisp_Object node)
{
  if (NILP (node)) return Qnil;
  treesit_check_node (node);
  treesit_initialize ();

  TSNode treesit_node = XTS_NODE (node)->node;
  ptrdiff_t visible_beg = XTS_PARSER (XTS_NODE (node)->parser)->visible_beg;
  uint32_t end_byte_offset = ts_node_end_byte (treesit_node);
  struct buffer *buffer
    = XBUFFER (XTS_PARSER (XTS_NODE (node)->parser)->buffer);
  ptrdiff_t end_pos
    = buf_bytepos_to_charpos (buffer, end_byte_offset + visible_beg);
  return make_fixnum (end_pos);
}

DEFUN ("treesit-node-string",
       Ftreesit_node_string, Streesit_node_string, 1, 1, 0,
       doc: /* Return the string representation of NODE.
If NODE is nil, return nil.  */)
  (Lisp_Object node)
{
  if (NILP (node)) return Qnil;
  treesit_check_node (node);
  treesit_initialize ();

  TSNode treesit_node = XTS_NODE (node)->node;
  char *string = ts_node_string (treesit_node);
  return build_string (string);
}

DEFUN ("treesit-node-parent",
       Ftreesit_node_parent, Streesit_node_parent, 1, 1, 0,
       doc: /* Return the immediate parent of NODE.
Return nil if NODE has no parent.  If NODE is nil, return nil.  */)
  (Lisp_Object node)
{
  if (NILP (node)) return Qnil;
  treesit_check_node (node);
  treesit_initialize ();

  TSNode treesit_node = XTS_NODE (node)->node;
  TSNode parent = ts_node_parent (treesit_node);

  if (ts_node_is_null (parent))
    return Qnil;

  return make_treesit_node (XTS_NODE (node)->parser, parent);
}

DEFUN ("treesit-node-child",
       Ftreesit_node_child, Streesit_node_child, 2, 3, 0,
       doc: /* Return the Nth child of NODE.

Return nil if there is no Nth child.  If NAMED is non-nil, look for
named child only.  NAMED defaults to nil.  If NODE is nil, return
nil.

N could be negative, e.g., -1 represents the last child.  */)
  (Lisp_Object node, Lisp_Object n, Lisp_Object named)
{
  if (NILP (node)) return Qnil;
  treesit_check_node (node);
  CHECK_INTEGER (n);
  EMACS_INT idx = XFIXNUM (n);

  treesit_initialize ();

  TSNode treesit_node = XTS_NODE (node)->node;
  TSNode child;

  /* Process negative index.  */
  if (idx < 0)
    {
      if (NILP (named))
	idx = ts_node_child_count (treesit_node) + idx;
      else
	idx = ts_node_named_child_count (treesit_node) + idx;
    }
  if (idx < 0)
    return Qnil;
  if (idx > UINT32_MAX)
    xsignal1 (Qargs_out_of_range, n);

  if (NILP (named))
    child = ts_node_child (treesit_node, (uint32_t) idx);
  else
    child = ts_node_named_child (treesit_node, (uint32_t) idx);

  if (ts_node_is_null (child))
    return Qnil;

  return make_treesit_node (XTS_NODE (node)->parser, child);
}

DEFUN ("treesit-node-check",
       Ftreesit_node_check, Streesit_node_check, 2, 2, 0,
       doc: /* Return non-nil if NODE has PROPERTY, nil otherwise.

PROPERTY could be `named', `missing', `extra', `outdated', or `has-error'.

Named nodes correspond to named rules in the language definition,
whereas "anonymous" nodes correspond to string literals in the
language definition.

Missing nodes are inserted by the parser in order to recover from
certain kinds of syntax errors, i.e., should be there but not there.

Extra nodes represent things like comments, which are not required the
language definition, but can appear anywhere.

A node is "outdated" if the parser has reparsed at least once after
the node was created.

A node "has error" if itself is a syntax error or contains any syntax
errors.  */)
  (Lisp_Object node, Lisp_Object property)
{
  if (NILP (node)) return Qnil;
  CHECK_TS_NODE (node);
  CHECK_SYMBOL (property);
  treesit_initialize ();

  TSNode treesit_node = XTS_NODE (node)->node;
  bool result;

  if (EQ (property, Qoutdated))
    return treesit_node_uptodate_p (node) ? Qnil : Qt;

  treesit_check_node (node);
  if (EQ (property, Qnamed))
    result = ts_node_is_named (treesit_node);
  else if (EQ (property, Qmissing))
    result = ts_node_is_missing (treesit_node);
  else if (EQ (property, Qextra))
    result = ts_node_is_extra (treesit_node);
  else if (EQ (property, Qhas_error))
    result = ts_node_has_error (treesit_node);
  else
    signal_error ("Expecting `named', `missing', `extra', "
		  "`outdated', or `has-error', but got",
		  property);
  return result ? Qt : Qnil;
}

DEFUN ("treesit-node-field-name-for-child",
       Ftreesit_node_field_name_for_child,
       Streesit_node_field_name_for_child, 2, 2, 0,
       doc: /* Return the field name of the Nth child of NODE.

Return nil if there's no Nth child, or if it has no field.
If NODE is nil, return nil.

N counts all children, i.e., named ones and anonymous ones.

N could be negative, e.g., -1 represents the last child.  */)
  (Lisp_Object node, Lisp_Object n)
{
  if (NILP (node))
    return Qnil;
  treesit_check_node (node);
  CHECK_INTEGER (n);
  EMACS_INT idx = XFIXNUM (n);
  treesit_initialize ();

  TSNode treesit_node = XTS_NODE (node)->node;

  /* Process negative index.  */
  if (idx < 0)
    idx = ts_node_child_count (treesit_node) + idx;
  if (idx < 0)
    return Qnil;
  if (idx > UINT32_MAX)
    xsignal1 (Qargs_out_of_range, n);

  const char *name
    = ts_node_field_name_for_child (treesit_node, (uint32_t) idx);

  if (name == NULL)
    return Qnil;

  return build_string (name);
}

DEFUN ("treesit-node-child-count",
       Ftreesit_node_child_count,
       Streesit_node_child_count, 1, 2, 0,
       doc: /* Return the number of children of NODE.

If NAMED is non-nil, count named children only.  NAMED defaults to
nil.  If NODE is nil, return nil.  */)
  (Lisp_Object node, Lisp_Object named)
{
  if (NILP (node))
    return Qnil;
  treesit_check_node (node);
  treesit_initialize ();

  TSNode treesit_node = XTS_NODE (node)->node;
  uint32_t count;
  if (NILP (named))
    count = ts_node_child_count (treesit_node);
  else
    count = ts_node_named_child_count (treesit_node);
  return make_fixnum (count);
}

DEFUN ("treesit-node-child-by-field-name",
       Ftreesit_node_child_by_field_name,
       Streesit_node_child_by_field_name, 2, 2, 0,
       doc: /* Return the child of NODE with FIELD-NAME.
Return nil if there is no such child.  If NODE is nil, return nil.  */)
  (Lisp_Object node, Lisp_Object field_name)
{
  if (NILP (node))
    return Qnil;
  treesit_check_node (node);
  CHECK_STRING (field_name);
  treesit_initialize ();

  char *name_str = SSDATA (field_name);
  TSNode treesit_node = XTS_NODE (node)->node;
  TSNode child
    = ts_node_child_by_field_name (treesit_node, name_str,
				   strlen (name_str));

  if (ts_node_is_null (child))
    return Qnil;

  return make_treesit_node (XTS_NODE (node)->parser, child);
}

DEFUN ("treesit-node-next-sibling",
       Ftreesit_node_next_sibling,
       Streesit_node_next_sibling, 1, 2, 0,
       doc: /* Return the next sibling of NODE.

Return nil if there is no next sibling.  If NAMED is non-nil, look for named
siblings only.  NAMED defaults to nil.  If NODE is nil, return nil.  */)
  (Lisp_Object node, Lisp_Object named)
{
  if (NILP (node)) return Qnil;
  treesit_check_node (node);
  treesit_initialize ();

  TSNode treesit_node = XTS_NODE (node)->node;
  TSNode sibling;
  if (NILP (named))
    sibling = ts_node_next_sibling (treesit_node);
  else
    sibling = ts_node_next_named_sibling (treesit_node);

  if (ts_node_is_null (sibling))
    return Qnil;

  return make_treesit_node (XTS_NODE (node)->parser, sibling);
}

DEFUN ("treesit-node-prev-sibling",
       Ftreesit_node_prev_sibling,
       Streesit_node_prev_sibling, 1, 2, 0,
       doc: /* Return the previous sibling of NODE.

Return nil if there is no previous sibling.  If NAMED is non-nil, look
for named siblings only.  NAMED defaults to nil.  If NODE is nil,
return nil.  */)
  (Lisp_Object node, Lisp_Object named)
{
  if (NILP (node)) return Qnil;
  treesit_check_node (node);
  treesit_initialize ();

  TSNode treesit_node = XTS_NODE (node)->node;
  TSNode sibling;

  if (NILP (named))
    sibling = ts_node_prev_sibling (treesit_node);
  else
    sibling = ts_node_prev_named_sibling (treesit_node);

  if (ts_node_is_null (sibling))
    return Qnil;

  return make_treesit_node (XTS_NODE (node)->parser, sibling);
}

DEFUN ("treesit-node-first-child-for-pos",
       Ftreesit_node_first_child_for_pos,
       Streesit_node_first_child_for_pos, 2, 3, 0,
       doc: /* Return the first child of NODE for buffer position POS.

Specifically, return the first child that extends beyond POS.
Return nil if there is no such child.
If NAMED is non-nil, look for named children only.  NAMED defaults to nil.
Note that this function returns an immediate child, not the smallest
(grand)child.  If NODE is nil, return nil.  */)
  (Lisp_Object node, Lisp_Object pos, Lisp_Object named)
{
  if (NILP (node))
    return Qnil;
  treesit_check_node (node);

  struct buffer *buf = XBUFFER (XTS_PARSER (XTS_NODE (node)->parser)->buffer);
  ptrdiff_t visible_beg = XTS_PARSER (XTS_NODE (node)->parser)->visible_beg;
  ptrdiff_t byte_pos = buf_charpos_to_bytepos (buf, XFIXNUM (pos));

  treesit_check_position (pos, buf);

  treesit_initialize ();

  TSNode treesit_node = XTS_NODE (node)->node;
  TSNode child;
  if (NILP (named))
    child = ts_node_first_child_for_byte (treesit_node, byte_pos - visible_beg);
  else
    child = ts_node_first_named_child_for_byte (treesit_node,
						byte_pos - visible_beg);

  if (ts_node_is_null (child))
    return Qnil;

  return make_treesit_node (XTS_NODE (node)->parser, child);
}

DEFUN ("treesit-node-descendant-for-range",
       Ftreesit_node_descendant_for_range,
       Streesit_node_descendant_for_range, 3, 4, 0,
       doc: /* Return the smallest node that covers buffer positions BEG to END.

The returned node is a descendant of NODE.
Return nil if there is no such node.
If NAMED is non-nil, look for named child only.  NAMED defaults to nil.
If NODE is nil, return nil.  */)
  (Lisp_Object node, Lisp_Object beg, Lisp_Object end, Lisp_Object named)
{
  if (NILP (node)) return Qnil;
  treesit_check_node (node);

  struct buffer *buf = XBUFFER (XTS_PARSER (XTS_NODE (node)->parser)->buffer);
  ptrdiff_t visible_beg = XTS_PARSER (XTS_NODE (node)->parser)->visible_beg;
  ptrdiff_t byte_beg = buf_charpos_to_bytepos (buf, XFIXNUM (beg));
  ptrdiff_t byte_end = buf_charpos_to_bytepos (buf, XFIXNUM (end));

  treesit_check_position (beg, buf);
  treesit_check_position (end, buf);

  treesit_initialize ();

  TSNode treesit_node = XTS_NODE (node)->node;
  TSNode child;
  if (NILP (named))
    child = ts_node_descendant_for_byte_range (treesit_node, byte_beg - visible_beg,
					       byte_end - visible_beg);
  else
    child = ts_node_named_descendant_for_byte_range (treesit_node,
						     byte_beg - visible_beg,
						     byte_end - visible_beg);

  if (ts_node_is_null (child))
    return Qnil;

  return make_treesit_node (XTS_NODE (node)->parser, child);
}

DEFUN ("treesit-node-eq",
       Ftreesit_node_eq,
       Streesit_node_eq, 2, 2, 0,
       doc: /* Return non-nil if NODE1 and NODE2 are the same node.
If any one of NODE1 and NODE2 is nil, return nil.  */)
  (Lisp_Object node1, Lisp_Object node2)
{
  if (NILP (node1) || NILP (node2))
    return Qnil;
  CHECK_TS_NODE (node1);
  CHECK_TS_NODE (node2);

  treesit_initialize ();

  TSNode treesit_node_1 = XTS_NODE (node1)->node;
  TSNode treesit_node_2 = XTS_NODE (node2)->node;

  bool same_node = ts_node_eq (treesit_node_1, treesit_node_2);
  return same_node ? Qt : Qnil;
}

/*** Query functions */

DEFUN ("treesit-pattern-expand",
       Ftreesit_pattern_expand,
       Streesit_pattern_expand, 1, 1, 0,
       doc: /* Expand PATTERN to its string form.

PATTERN can be

    :anchor
    :?
    :*
    :+
    :equal
    :match
    (TYPE PATTERN...)
    [PATTERN...]
    FIELD-NAME:
    @CAPTURE-NAME
    (_)
    _
    \"TYPE\"

See Info node `(elisp)Pattern Matching' for detailed explanation.  */)
  (Lisp_Object pattern)
{
  if (EQ (pattern, QCanchor))
    return build_pure_c_string (".");
  if (EQ (pattern, intern_c_string (":?")))
    return build_pure_c_string ("?");
  if (EQ (pattern, intern_c_string (":*")))
    return build_pure_c_string ("*");
  if (EQ (pattern, intern_c_string (":+")))
    return build_pure_c_string ("+");
  if (EQ (pattern, QCequal))
    return build_pure_c_string ("#equal");
  if (EQ (pattern, QCmatch))
    return build_pure_c_string ("#match");
  Lisp_Object opening_delimeter
    = build_pure_c_string (VECTORP (pattern) ? "[" : "(");
  Lisp_Object closing_delimiter
    = build_pure_c_string (VECTORP (pattern) ? "]" : ")");
  if (VECTORP (pattern) || CONSP (pattern))
    return concat3 (opening_delimeter,
		    Fmapconcat (Qtreesit_pattern_expand,
				pattern,
				build_pure_c_string (" ")),
		    closing_delimiter);
  return CALLN (Fformat, build_pure_c_string ("%S"), pattern);
}

DEFUN ("treesit-query-expand",
       Ftreesit_query_expand,
       Streesit_query_expand, 1, 1, 0,
       doc: /* Expand sexp QUERY to its string form.

A PATTERN in QUERY can be

    :anchor
    :?
    :*
    :+
    :equal
    :match
    (TYPE PATTERN...)
    [PATTERN...]
    FIELD-NAME:
    @CAPTURE-NAME
    (_)
    _
    \"TYPE\"

See Info node `(elisp)Pattern Matching' for detailed explanation.  */)
  (Lisp_Object query)
{
  return Fmapconcat (Qtreesit_pattern_expand,
		     query, build_pure_c_string (" "));
}

/* This struct is used for passing captures to be check against
   predicates.  Captures we check for are the ones in START before
   END.  For example, if START and END are

   START       END
    v              v
   (1 . (2 . (3 . (4 . (5 . (6 . nil))))))

   We only look at captures 1 2 3.  */
struct capture_range
{
  Lisp_Object start;
  Lisp_Object end;
};

/* Collect predicates for this match and return them in a list.  Each
   predicate is a list of strings and symbols.  */
static Lisp_Object
treesit_predicates_for_pattern (TSQuery *query, uint32_t pattern_index)
{
  uint32_t len;
  const TSQueryPredicateStep *predicate_list
    = ts_query_predicates_for_pattern (query, pattern_index, &len);
  Lisp_Object result = Qnil;
  Lisp_Object predicate = Qnil;
  for (int idx = 0; idx < len; idx++)
    {
      TSQueryPredicateStep step = predicate_list[idx];
      switch (step.type)
	{
	case TSQueryPredicateStepTypeCapture:
	  {
	    uint32_t str_len;
	    const char *str = ts_query_capture_name_for_id (query,
							    step.value_id,
							    &str_len);
	    predicate = Fcons (intern_c_string_1 (str, str_len),
			       predicate);
	    break;
	  }
	case TSQueryPredicateStepTypeString:
	  {
	    uint32_t str_len;
	    const char *str = ts_query_string_value_for_id (query,
							    step.value_id,
							    &str_len);
	    predicate = Fcons (make_string (str, str_len), predicate);
	    break;
	  }
	case TSQueryPredicateStepTypeDone:
	  result = Fcons (Fnreverse (predicate), result);
	  predicate = Qnil;
	  break;
	}
    }
  return Fnreverse (result);
}

/* Translate a capture NAME (symbol) to the text of the captured node.
   Signals treesit-query-error if such node is not captured.  */
static Lisp_Object
treesit_predicate_capture_name_to_text (Lisp_Object name,
					struct capture_range captures)
{
  Lisp_Object node = Qnil;
  for (Lisp_Object tail = captures.start; !EQ (tail, captures.end);
       tail = XCDR (tail))
    {
      if (EQ (XCAR (XCAR (tail)), name))
	{
	  node = XCDR (XCAR (tail));
	  break;
	}
    }

  if (NILP (node))
    xsignal3 (Qtreesit_query_error,
	      build_pure_c_string ("Cannot find captured node"),
	      name, build_pure_c_string ("A predicate can only refer"
					 " to captured nodes in the "
					 "same pattern"));

  struct buffer *old_buffer = current_buffer;
  set_buffer_internal (XBUFFER (XTS_PARSER (XTS_NODE (node)->parser)->buffer));
  Lisp_Object text = Fbuffer_substring (Ftreesit_node_start (node),
					Ftreesit_node_end (node));
  set_buffer_internal (old_buffer);
  return text;
}

/* Handles predicate (#equal A B).  Return true if A equals B; return
   false otherwise.  A and B can be either string, or a capture name.
   The capture name evaluates to the text its captured node spans in
   the buffer.  */
static bool
treesit_predicate_equal (Lisp_Object args, struct capture_range captures)
{
  if (XFIXNUM (Flength (args)) != 2)
    xsignal2 (Qtreesit_query_error,
	      build_pure_c_string ("Predicate `equal' requires "
				   "two arguments but only given"),
	      Flength (args));

  Lisp_Object arg1 = XCAR (args);
  Lisp_Object arg2 = XCAR (XCDR (args));
  Lisp_Object text1 = (STRINGP (arg1)
		       ? arg1
		       : treesit_predicate_capture_name_to_text (arg1,
								 captures));
  Lisp_Object text2 = (STRINGP (arg2)
		       ? arg2
		       : treesit_predicate_capture_name_to_text (arg2,
								 captures));

  return !NILP (Fstring_equal (text1, text2));
}

/* Handles predicate (#match "regexp" @node).  Return true if "regexp"
   matches the text spanned by @node; return false otherwise.  Matching
   is case-sensitive.  */
static bool
treesit_predicate_match (Lisp_Object args, struct capture_range captures)
{
  if (XFIXNUM (Flength (args)) != 2)
    xsignal2 (Qtreesit_query_error,
	      build_pure_c_string ("Predicate `equal' requires two "
				   "arguments but only given"),
	      Flength (args));

  Lisp_Object regexp = XCAR (args);
  Lisp_Object capture_name = XCAR (XCDR (args));

  /* It's probably common to get the argument order backwards.  Catch
     this mistake early and show helpful explanation, because Emacs
     loves you.  (We put the regexp first because that's what
     string-match does.)  */
  if (!STRINGP (regexp))
    xsignal1 (Qtreesit_query_error,
	      build_pure_c_string ("The first argument to `match' should "
				   "be a regexp string, not a capture name"));
  if (!SYMBOLP (capture_name))
    xsignal1 (Qtreesit_query_error,
	      build_pure_c_string ("The second argument to `match' should "
				   "be a capture name, not a string"));

  Lisp_Object text = treesit_predicate_capture_name_to_text (capture_name,
							     captures);

  if (fast_string_match (regexp, text) >= 0)
    return true;
  else
    return false;
}

/* About predicates: I decide to hard-code predicates in C instead of
   implementing an extensible system where predicates are translated
   to Lisp functions, and new predicates can be added by extending a
   list of functions, because I really couldn't imagine any useful
   predicates besides equal and match.  If we later found out that
   such system is indeed useful and necessary, it can be easily
   added.  */

/* If all predicates in PREDICATES passes, return true; otherwise
   return false.  */
static bool
treesit_eval_predicates (struct capture_range captures, Lisp_Object predicates)
{
  bool pass = true;
  /* Evaluate each predicates.  */
  for (Lisp_Object tail = predicates;
       !NILP (tail); tail = XCDR (tail))
    {
      Lisp_Object predicate = XCAR (tail);
      Lisp_Object fn = XCAR (predicate);
      Lisp_Object args = XCDR (predicate);
      if (!NILP (Fstring_equal (fn, build_pure_c_string ("equal"))))
	pass = treesit_predicate_equal (args, captures);
      else if (!NILP (Fstring_equal (fn, build_pure_c_string ("match"))))
	pass = treesit_predicate_match (args, captures);
      else
	xsignal3 (Qtreesit_query_error,
		  build_pure_c_string ("Invalid predicate"),
		  fn, build_pure_c_string ("Currently Emacs only supports"
					   " equal and match predicate"));
    }
  /* If all predicates passed, add captures to result list.  */
  return pass;
}

DEFUN ("treesit-query-compile",
       Ftreesit_query_compile,
       Streesit_query_compile, 2, 3, 0,
       doc: /* Compile QUERY to a compiled query.

Querying with a compiled query is much faster than an uncompiled one.
LANGUAGE is the language this query is for.

If EAGER is non-nil, immediately load LANGUAGE and compile the query.
Otherwise defer the compilation until the query is first used.

Signal `treesit-query-error' if QUERY is malformed or something else
goes wrong.  (This only happens if EAGER is non-nil.)
You can use `treesit-query-validate' to validate and debug a query.  */)
  (Lisp_Object language, Lisp_Object query, Lisp_Object eager)
{
  if (NILP (Ftreesit_query_p (query)))
    wrong_type_argument (Qtreesit_query_p, query);
  CHECK_SYMBOL (language);
  if (TS_COMPILED_QUERY_P (query))
    return query;

  treesit_initialize ();

  Lisp_Object lisp_query = make_treesit_query (query, language);

  /* Maybe actually compile.  */
  if (NILP (eager))
    return lisp_query;
  else
    {
      Lisp_Object signal_symbol = Qnil;
      Lisp_Object signal_data = Qnil;
      TSQuery *treesit_query = treesit_ensure_query_compiled (lisp_query,
							 &signal_symbol,
							 &signal_data);

      if (treesit_query == NULL)
	xsignal (signal_symbol, signal_data);

      return lisp_query;
    }
}

DEFUN ("treesit-query-capture",
       Ftreesit_query_capture,
       Streesit_query_capture, 2, 5, 0,
       doc: /* Query NODE with patterns in QUERY.

Return a list of (CAPTURE_NAME . NODE).  CAPTURE_NAME is the name
assigned to the node in PATTERN.  NODE is the captured node.

QUERY is either a string query, a sexp query, or a compiled query.
See Info node `(elisp)Pattern Matching' for how to write a query in
either string or sexp form.  When using repeatedly, a compiled query
is much faster than a string or sexp one, so it is recommend to
compile your query if it will be used repeatedly.

BEG and END, if both non-nil, specify the region of buffer positions
in which the query is executed.  Any matching node whose span overlaps
with the region between BEG and END are captured, it doesn't have to
be completely in the region.

If NODE-ONLY is non-nil, return a list of nodes.

Besides a node, NODE can also be a parser, in which case the root node
of that parser is used.
NODE can also be a language symbol, in which case the root node of a
parser for that language is used.  If such a parser doesn't exist, it
is created.

Signal `treesit-query-error' if QUERY is malformed or something else
goes wrong.  You can use `treesit-query-validate' to validate and debug
the query.  */)
  (Lisp_Object node, Lisp_Object query,
   Lisp_Object beg, Lisp_Object end, Lisp_Object node_only)
{
  if (!(TS_COMPILED_QUERY_P (query)
	|| CONSP (query) || STRINGP (query)))
    wrong_type_argument (Qtreesit_query_p, query);

  treesit_initialize ();

  /* Resolve NODE into an actual node.  */
  Lisp_Object lisp_node;
  if (TS_NODEP (node))
    {
      treesit_check_node (node); /* Check if up-to-date.  */
      lisp_node = node;
    }
  else if (TS_PARSERP (node))
    {
      treesit_check_parser (node); /* Check if deleted.  */
      lisp_node = Ftreesit_parser_root_node (node);
    }
  else if (SYMBOLP (node))
    {
      Lisp_Object parser
	= Ftreesit_parser_create (node, Fcurrent_buffer (), Qnil);
      lisp_node = Ftreesit_parser_root_node (parser);
    }
  else
    xsignal2 (Qwrong_type_argument,
	      list4 (Qor, Qtreesit_node_p, Qtreesit_parser_p, Qsymbolp),
	      node);

  /* Extract C values from Lisp objects.  */
  TSNode treesit_node
    = XTS_NODE (lisp_node)->node;
  Lisp_Object lisp_parser
    = XTS_NODE (lisp_node)->parser;
  ptrdiff_t visible_beg
    = XTS_PARSER (XTS_NODE (lisp_node)->parser)->visible_beg;
  const TSLanguage *lang
    = ts_parser_language (XTS_PARSER (lisp_parser)->parser);

  /* Check BEG and END.  */
  struct buffer *buf = XBUFFER (XTS_PARSER (lisp_parser)->buffer);
  if (!NILP (beg))
    treesit_check_position (beg, buf);
  if (!NILP (end))
    treesit_check_position (end, buf);

  /* Initialize query objects.  At the end of this block, we should
     have a working TSQuery and a TSQueryCursor.  */
  TSQuery *treesit_query;
  TSQueryCursor *cursor;
  bool needs_to_free_query_and_cursor;
  if (TS_COMPILED_QUERY_P (query))
    {
      Lisp_Object signal_symbol = Qnil;
      Lisp_Object signal_data = Qnil;
      treesit_query = treesit_ensure_query_compiled (query, &signal_symbol,
						     &signal_data);
      cursor = XTS_COMPILED_QUERY (query)->cursor;
      /* We don't need to free ts_query and cursor because they
	 are stored in a lisp object, which is tracked by gc.  */
      needs_to_free_query_and_cursor = false;
      if (treesit_query == NULL)
	xsignal (signal_symbol, signal_data);
    }
  else
    {
      /* Since query is not TS_COMPILED_QUERY, it can only be a string
	 or a cons.  */
      if (CONSP (query))
	query = Ftreesit_query_expand (query);
      char *query_string = SSDATA (query);
      uint32_t error_offset;
      TSQueryError error_type;
      treesit_query = ts_query_new (lang, query_string, strlen (query_string),
				    &error_offset, &error_type);
      if (treesit_query == NULL)
	xsignal (Qtreesit_query_error,
		 treesit_compose_query_signal_data (error_offset,
						    error_type));
      cursor = ts_query_cursor_new ();
      needs_to_free_query_and_cursor = true;
    }

  /* WARN: After this point, free treesit_query and cursor before every
     signal and return.  */

  /* Set query range.  */
  if (!NILP (beg) && !NILP (end))
    {
      ptrdiff_t beg_byte = CHAR_TO_BYTE (XFIXNUM (beg));
      ptrdiff_t end_byte = CHAR_TO_BYTE (XFIXNUM (end));
      /* We never let tree-sitter run on buffers too large, so these
	 assertion should never hit.  */
      eassert (beg_byte - visible_beg <= UINT32_MAX);
      eassert (end_byte - visible_beg <= UINT32_MAX);
      ts_query_cursor_set_byte_range (cursor,
				      (uint32_t) (beg_byte - visible_beg),
				      (uint32_t) (end_byte - visible_beg));
    }

  /* Execute query.  */
  ts_query_cursor_exec (cursor, treesit_query, treesit_node);
  TSQueryMatch match;

  /* Go over each match, collect captures and predicates.  Include the
     captures in the RESULT list unconditionally as we get them, then
     test for predicates.  If predicates pass, then all good, if
     predicates don't pass, revert the result back to the result
     before this loop (PREV_RESULT).  (Predicates control the entire
     match.) This way we don't need to create a list of captures in
     every for loop and nconc it to RESULT every time.  That is indeed
     the initial implementation in which Yoav found nconc being the
     bottleneck (98.4% of the running time spent on nconc).  */
  Lisp_Object result = Qnil;
  Lisp_Object prev_result = result;
  while (ts_query_cursor_next_match (cursor, &match))
    {
      /* Record the checkpoint that we may roll back to.  */
      prev_result = result;
      /* Get captured nodes.  */
      const TSQueryCapture *captures = match.captures;
      for (int idx = 0; idx < match.capture_count; idx++)
	{
	  uint32_t capture_name_len;
	  TSQueryCapture capture = captures[idx];
	  Lisp_Object captured_node = make_treesit_node (lisp_parser,
							 capture.node);

	  Lisp_Object cap;
	  if (NILP (node_only))
	    {
	      const char *capture_name
		= ts_query_capture_name_for_id (treesit_query, capture.index,
						&capture_name_len);
	      cap = Fcons (intern_c_string_1 (capture_name, capture_name_len),
			   captured_node);
	    }
	  else
	    cap = captured_node;

	  result = Fcons (cap, result);
	}
      /* Get predicates.  */
      Lisp_Object predicates
	= treesit_predicates_for_pattern (treesit_query,
					  match.pattern_index);

      /* captures_lisp = Fnreverse (captures_lisp); */
      struct capture_range captures_range = { result, prev_result };
      if (!treesit_eval_predicates (captures_range, predicates))
	/* Predicates didn't pass, roll back.  */
	result = prev_result;
    }
  if (needs_to_free_query_and_cursor)
    {
      ts_query_delete (treesit_query);
      ts_query_cursor_delete (cursor);
    }
  return Fnreverse (result);
}

/*** Navigation */

/* Return the next/previous named/unnamed sibling of NODE.  FORWARD
   controls the direction and NAMED controls the nameness.  */
static TSNode
treesit_traverse_sibling_helper (TSNode node, bool forward, bool named)
{
  if (forward)
    {
      if (named)
	return ts_node_next_named_sibling (node);
      else
	return ts_node_next_sibling (node);
    }
  else
    {
      if (named)
	return ts_node_prev_named_sibling (node);
      else
	return ts_node_prev_sibling (node);
    }
}

/* Return the first/last named/unnamed child of NODE.  FORWARD controls
   the direction and NAMED controls the nameness.  */
static TSNode
treesit_traverse_child_helper (TSNode node, bool forward, bool named)
{
  if (forward)
    {
      if (named)
	return ts_node_named_child (node, 0);
      else
	return ts_node_child (node, 0);
    }
  else
    {
      if (named)
	{
	  uint32_t count = ts_node_named_child_count (node);
	  uint32_t idx = count == 0 ? 0 : count - 1;
	  return ts_node_named_child (node, idx);
	}
      else
	{
	  uint32_t count = ts_node_child_count (node);
	  uint32_t idx = count == 0 ? 0 : count - 1;
	  return ts_node_child (node, idx);
	}
    }
}

/* Return true if NODE matches PRED.  PRED can be a string or a
   function.  This function assumes PRED is either a string or a
   function.  */
static bool
treesit_traverse_match_predicate (TSNode node, Lisp_Object pred,
				  Lisp_Object parser)
{
  if (STRINGP (pred))
    {
      const char *type = ts_node_type (node);
      return fast_c_string_match (pred, type, strlen (type)) >= 0;
    }
  else
    {
      Lisp_Object lisp_node = make_treesit_node (parser, node);
      return !NILP (CALLN (Ffuncall, pred, lisp_node));
    }

}

/* Traverse the parse tree starting from ROOT (but ROOT is not
   matches against PRED).  PRED can be a function (takes a node and
   returns nil/non-nil),or a string (treated as regexp matching the
   node's type, ignores case, must be all single byte characters).  If
   the node satisfies PRED , terminate, set ROOT to that node, and
   return true.  If no node satisfies PRED, return FALSE.  PARSER is
   the parser of ROOT.

   LIMIT is the number of levels we descend in the tree.  FORWARD
   controls the direction in which we traverse the tree, true means
   forward, false backward.  If NAMED is true, only traverse named
   nodes, if false, all nodes.  If SKIP_ROOT is true, don't match
   ROOT.  */
static bool
treesit_search_dfs (TSNode *root, Lisp_Object pred, Lisp_Object parser,
		    bool named, bool forward, ptrdiff_t limit,
		    bool skip_root)
{
  /* TSTreeCursor doesn't allow us to move backward, so we can't use
     it.  */
  TSNode node = *root;

  if (!skip_root && treesit_traverse_match_predicate (node, pred, parser))
    {
      *root = node;
      return true;
    }

  if (limit <= 0)
    return false;
  else
    {
      int count = (named
		   ? ts_node_named_child_count (node)
		   : ts_node_child_count (node));
      for (int offset = 0; offset < count; offset++)
	{
	  uint32_t idx = forward ? offset : count - offset - 1;
	  TSNode child = (named
			  ? ts_node_named_child (node, idx)
			  : ts_node_child (node, idx));

	  if (!ts_node_is_null (child)
	      && treesit_search_dfs (&child, pred, parser, named,
				     forward, limit - 1, false))
	    {
	      *root = child;
	      return true;
	    }
	}
      return false;
    }
}

/* Go through the whole tree linearly, leaf-first, starting from
   START.  PRED, PARSER, NAMED, FORWARD are the same as in
   ts_search_subtre.  If UP_ONLY is true, never go to children, only
   sibling and parents.  */
static bool
treesit_search_forward (TSNode *start, Lisp_Object pred, Lisp_Object parser,
			bool named, bool forward)
{
  TSNode node = *start;

  /* We don't search for subtree and always search from the leaf
     nodes.  This way repeated call of this function traverses each
     node in the tree once and only once:

     (while node (setq node (treesit-search-forward node)))
  */
  bool initial = true;
  while (true)
    {
      if (!initial /* We don't match START.  */
	  && treesit_traverse_match_predicate (node, pred, parser))
	{
	  *start = node;
	  return true;
	}
      initial = false;

      TSNode next = treesit_traverse_sibling_helper (node, forward, named);
      while (ts_node_is_null (next))
	{
	  /* There is no next sibling, go to parent.  */
	  node = ts_node_parent (node);
	  if (ts_node_is_null (node))
	    return false;

	  if (treesit_traverse_match_predicate (node, pred, parser))
	    {
	      *start = node;
	      return true;
	    }
	  next = treesit_traverse_sibling_helper (node, forward, named);
	}
      /* We are at the next sibling, deep dive into the first leaf
	 node.  */
      TSNode next_next = treesit_traverse_child_helper (next, forward, named);
      while (!ts_node_is_null (next_next))
	{
	  next = next_next;
	  next_next = treesit_traverse_child_helper (next, forward, named);
	}
      /* At this point NEXT is a leaf node.  */
      node = next;
    }
}

DEFUN ("treesit-search-subtree",
       Ftreesit_search_subtree,
       Streesit_search_subtree, 2, 5, 0,
       doc: /* Traverse the parse tree of NODE depth-first using PREDICATE.

Traverse the subtree of NODE, and match PREDICATE with each node along
the way.  PREDICATE is a regexp string that matches against each
node's type, or a function that takes a node and returns nil/non-nil.

By default, only traverse named nodes, but if ALL is non-nil, traverse
all nodes.  If BACKWARD is non-nil, traverse backwards.  If LIMIT is
non-nil, only traverse nodes up to that number of levels down in the
tree.  If LIMIT is nil, default to 1000.

Return the first matched node, or nil if none matches.  */)
  (Lisp_Object node, Lisp_Object predicate, Lisp_Object backward,
   Lisp_Object all, Lisp_Object limit)
{
  CHECK_TS_NODE (node);
  CHECK_TYPE (STRINGP (predicate) || FUNCTIONP (predicate),
	      list3 (Qor, Qstringp, Qfunctionp), predicate);
  CHECK_SYMBOL (all);
  CHECK_SYMBOL (backward);

  /* We use a default limit to 1000.  See bug#59426 for the
     discussion.  */
  ptrdiff_t the_limit = 1000;
  if (!NILP (limit))
    {
      CHECK_FIXNUM (limit);
      the_limit = XFIXNUM (limit);
    }

  treesit_initialize ();

  TSNode treesit_node = XTS_NODE (node)->node;
  Lisp_Object parser = XTS_NODE (node)->parser;
  if (treesit_search_dfs (&treesit_node, predicate, parser, NILP (all),
			  NILP (backward), the_limit, false))
    return make_treesit_node (parser, treesit_node);
  else
    return Qnil;
}

DEFUN ("treesit-search-forward",
       Ftreesit_search_forward,
       Streesit_search_forward, 2, 4, 0,
       doc: /* Search for node matching PREDICATE in the parse tree of START.

Start traversing the tree from node START, and match PREDICATE with
each node (except START itself) along the way.  PREDICATE is a regexp
string that matches against each node's type, or a function that takes
a node and returns non-nil if it matches.

By default, only search for named nodes, but if ALL is non-nil, search
for all nodes.  If BACKWARD is non-nil, search backwards.

Return the first matched node, or nil if none matches.

For a tree like below, where START is marked by S, traverse as
numbered from 1 to 12:

                12
                |
       S--------3----------11
       |        |          |
  o--o-+--o  1--+--2    6--+-----10
  |  |                  |        |
  o  o                +-+-+   +--+--+
                      |   |   |  |  |
                      4   5   7  8  9

Note that this function doesn't traverse the subtree of START, and it
always traverse leaf nodes first, then upwards.  */)
  (Lisp_Object start, Lisp_Object predicate, Lisp_Object backward,
   Lisp_Object all)
{
  CHECK_TS_NODE (start);
  CHECK_TYPE (STRINGP (predicate) || FUNCTIONP (predicate),
	      list3 (Qor, Qstringp, Qfunctionp), predicate);
  CHECK_SYMBOL (all);
  CHECK_SYMBOL (backward);

  treesit_initialize ();

  TSNode treesit_start = XTS_NODE (start)->node;
  Lisp_Object parser = XTS_NODE (start)->parser;
  if (treesit_search_forward (&treesit_start, predicate, parser, NILP (all),
			      NILP (backward)))
    return make_treesit_node (parser, treesit_start);
  else
    return Qnil;
}

/* Recursively traverse the tree under CURSOR, and append the result
   subtree to PARENT's cdr.  See more in Ftreesit_induce_sparse_tree.
   Note that the top-level children list is reversed, because
   reasons.  */
static void
treesit_build_sparse_tree (TSTreeCursor *cursor, Lisp_Object parent,
			   Lisp_Object pred, Lisp_Object process_fn,
			   ptrdiff_t limit, Lisp_Object parser)
{

  TSNode node = ts_tree_cursor_current_node (cursor);
  bool match = treesit_traverse_match_predicate (node, pred, parser);
  if (match)
    {
      /* If this node matches pred, add a new node to the parent's
	 children list.  */
      Lisp_Object lisp_node = make_treesit_node (parser, node);
      if (!NILP (process_fn))
	lisp_node = CALLN (Ffuncall, process_fn, lisp_node);

      Lisp_Object this = Fcons (lisp_node, Qnil);
      Fsetcdr (parent, Fcons (this, Fcdr (parent)));
      /* Now for children nodes, this is the new parent.  */
      parent = this;
    }
  /* Go through each child.  */
  if (limit > 0 && ts_tree_cursor_goto_first_child (cursor))
    {
      do
	{
	  /* Make sure not to use node after the recursive funcall.
	     Then C compilers should be smart enough not to copy NODE
	     to stack.  */
	  treesit_build_sparse_tree (cursor, parent, pred, process_fn,
				     limit - 1, parser);
	}
      while (ts_tree_cursor_goto_next_sibling (cursor));
      /* Don't forget to come back to this node.  */
      ts_tree_cursor_goto_parent (cursor);
    }
  /* Before we go, reverse children in the sparse tree.  */
  if (match)
    /* When match == true, "parent" is actually the node we added in
       this layer (parent = this).  */
    Fsetcdr (parent, Fnreverse (Fcdr (parent)));
}

DEFUN ("treesit-induce-sparse-tree",
       Ftreesit_induce_sparse_tree,
       Streesit_induce_sparse_tree, 2, 4, 0,
       doc: /* Create a sparse tree of ROOT's subtree.

This takes the subtree under ROOT, and combs it so only the nodes
that match PREDICATE are left, like picking out grapes on the vine.
PREDICATE is a regexp string that matches against each node's type.

For a subtree on the left that consist of both numbers and letters, if
PREDICATE is "is letter", the returned tree is the one on the right.

	a                 a              a
	|                 |              |
    +---+---+         +---+---+      +---+---+
    |   |   |         |   |   |      |   |   |
    b   1   2         b   |   |      b   c   d
	|   |     =>      |   |  =>      |
	c   +--+          c   +          e
	|   |  |          |   |
     +--+   d  4       +--+   d
     |  |              |
     e  5              e

If PROCESS-FN is non-nil, it should be a function of one argument.  In
that case, instead of returning the matched nodes, pass each node to
PROCESS-FN, and use its return value instead.

If non-nil, LIMIT is the number of levels to go down the tree from
ROOT.  If LIMIT is nil or omitted, it defaults to 1000.

Each node in the returned tree looks like (NODE . (CHILD ...)).  The
root of this tree might be nil, if ROOT doesn't match PREDICATE.

If no node matches PREDICATE, return nil.

PREDICATE can also be a function that takes a node and returns
nil/non-nil, but it is slower and more memory consuming than using
a regexp.  */)
  (Lisp_Object root, Lisp_Object predicate, Lisp_Object process_fn,
   Lisp_Object limit)
{
  CHECK_TS_NODE (root);
  CHECK_TYPE (STRINGP (predicate) || FUNCTIONP (predicate),
	      list3 (Qor, Qstringp, Qfunctionp), predicate);

  if (!NILP (process_fn))
    CHECK_TYPE (FUNCTIONP (process_fn), Qfunctionp, process_fn);

  /* We use a default limit to 1000.  See bug#59426 for the
     discussion.  */
  ptrdiff_t the_limit = 1000;
  if (!NILP (limit))
    {
      CHECK_FIXNUM (limit);
      the_limit = XFIXNUM (limit);
    }

  treesit_initialize ();

  TSTreeCursor cursor = ts_tree_cursor_new (XTS_NODE (root)->node);
  Lisp_Object parser = XTS_NODE (root)->parser;
  Lisp_Object parent = Fcons (Qnil, Qnil);
  treesit_build_sparse_tree (&cursor, parent, predicate, process_fn,
			     the_limit, parser);
  Fsetcdr (parent, Fnreverse (Fcdr (parent)));
  if (NILP (Fcdr (parent)))
    return Qnil;
  else
    return parent;
}

#endif	/* HAVE_TREE_SITTER */

DEFUN ("treesit-available-p", Ftreesit_available_p,
       Streesit_available_p, 0, 0, 0,
       doc: /* Return non-nil if tree-sitter support is built-in and available.  */)
  (void)
{
#if HAVE_TREE_SITTER
  return load_tree_sitter_if_necessary (false) ? Qt : Qnil;
#else
  return Qnil;
#endif
}


/*** Initialization */

/* Initialize the tree-sitter routines.  */
void
syms_of_treesit (void)
{
#if HAVE_TREE_SITTER
  DEFSYM (Qtreesit_parser_p, "treesit-parser-p");
  DEFSYM (Qtreesit_node_p, "treesit-node-p");
  DEFSYM (Qtreesit_compiled_query_p, "treesit-compiled-query-p");
  DEFSYM (Qtreesit_query_p, "treesit-query-p");
  DEFSYM (Qnamed, "named");
  DEFSYM (Qmissing, "missing");
  DEFSYM (Qextra, "extra");
  DEFSYM (Qoutdated, "outdated");
  DEFSYM (Qhas_error, "has-error");

  DEFSYM (QCanchor, ":anchor");
  DEFSYM (QCequal, ":equal");
  DEFSYM (QCmatch, ":match");

  DEFSYM (Qnot_found, "not-found");
  DEFSYM (Qsymbol_error, "symbol-error");
  DEFSYM (Qversion_mismatch, "version-mismatch");

  DEFSYM (Qtreesit_error, "treesit-error");
  DEFSYM (Qtreesit_query_error, "treesit-query-error");
  DEFSYM (Qtreesit_parse_error, "treesit-parse-error");
  DEFSYM (Qtreesit_range_invalid, "treesit-range-invalid");
  DEFSYM (Qtreesit_buffer_too_large,
	  "treesit-buffer-too-large");
  DEFSYM (Qtreesit_load_language_error,
	  "treesit-load-language-error");
  DEFSYM (Qtreesit_node_outdated,
	  "treesit-node-outdated");
  DEFSYM (Quser_emacs_directory,
	  "user-emacs-directory");
  DEFSYM (Qtreesit_parser_deleted, "treesit-parser-deleted");
  DEFSYM (Qtreesit_pattern_expand, "treesit-pattern-expand");

  DEFSYM (Qor, "or");

#ifdef WINDOWSNT
  DEFSYM (Qtree_sitter, "tree-sitter");
#endif

  define_error (Qtreesit_error, "Generic tree-sitter error", Qerror);
  define_error (Qtreesit_query_error, "Query pattern is malformed",
		Qtreesit_error);
  /* Should be impossible, no need to document this error.  */
  define_error (Qtreesit_parse_error, "Parse failed",
		Qtreesit_error);
  define_error (Qtreesit_range_invalid,
		"RANGES are invalid, they have to be ordered and not overlapping",
		Qtreesit_error);
  define_error (Qtreesit_buffer_too_large, "Buffer too large (> 4GB)",
		Qtreesit_error);
  define_error (Qtreesit_load_language_error,
		"Cannot load language definition",
		Qtreesit_error);
  define_error (Qtreesit_node_outdated,
		"This node is outdated, please retrieve a new one",
		Qtreesit_error);
  define_error (Qtreesit_parser_deleted,
		"This parser is deleted and cannot be used",
		Qtreesit_error);

  DEFVAR_LISP ("treesit-load-name-override-list",
	       Vtreesit_load_name_override_list,
	       doc:
	       /* An override list for unconventional tree-sitter libraries.

By default, Emacs assumes the dynamic library for LANG is
libtree-sitter-LANG.EXT, where EXT is the OS specific extension for
dynamic libraries.  Emacs also assumes that the name of the C function
the library provides is tree_sitter_LANG.  If that is not the case,
you can add an entry

    (LANG LIBRARY-BASE-NAME FUNCTION-NAME)

to this list, where LIBRARY-BASE-NAME is the filename of the dynamic
library without the file-name extension, and FUNCTION-NAME is the
function provided by the library.  */);
  Vtreesit_load_name_override_list = Qnil;

  DEFVAR_LISP ("treesit-extra-load-path",
	       Vtreesit_extra_load_path,
	       doc:
	       /* Additional directories to look for tree-sitter language definitions.
The value should be a list of directories.
When trying to load a tree-sitter language definition,
Emacs first looks in the directories mentioned in this variable,
then in the `tree-sitter' subdirectory of `user-emacs-directory', and
then in the system default locations for dynamic libraries, in that order.  */);
  Vtreesit_extra_load_path = Qnil;

  defsubr (&Streesit_language_available_p);
  defsubr (&Streesit_language_version);

  defsubr (&Streesit_parser_p);
  defsubr (&Streesit_node_p);
  defsubr (&Streesit_compiled_query_p);
  defsubr (&Streesit_query_p);
  defsubr (&Streesit_query_language);

  defsubr (&Streesit_node_parser);

  defsubr (&Streesit_parser_create);
  defsubr (&Streesit_parser_delete);
  defsubr (&Streesit_parser_list);
  defsubr (&Streesit_parser_buffer);
  defsubr (&Streesit_parser_language);

  defsubr (&Streesit_parser_root_node);
  /* defsubr (&Streesit_parse_string); */

  defsubr (&Streesit_parser_set_included_ranges);
  defsubr (&Streesit_parser_included_ranges);

  defsubr (&Streesit_parser_notifiers);
  defsubr (&Streesit_parser_add_notifier);
  defsubr (&Streesit_parser_remove_notifier);

  defsubr (&Streesit_node_type);
  defsubr (&Streesit_node_start);
  defsubr (&Streesit_node_end);
  defsubr (&Streesit_node_string);
  defsubr (&Streesit_node_parent);
  defsubr (&Streesit_node_child);
  defsubr (&Streesit_node_check);
  defsubr (&Streesit_node_field_name_for_child);
  defsubr (&Streesit_node_child_count);
  defsubr (&Streesit_node_child_by_field_name);
  defsubr (&Streesit_node_next_sibling);
  defsubr (&Streesit_node_prev_sibling);
  defsubr (&Streesit_node_first_child_for_pos);
  defsubr (&Streesit_node_descendant_for_range);
  defsubr (&Streesit_node_eq);

  defsubr (&Streesit_pattern_expand);
  defsubr (&Streesit_query_expand);
  defsubr (&Streesit_query_compile);
  defsubr (&Streesit_query_capture);

  defsubr (&Streesit_search_subtree);
  defsubr (&Streesit_search_forward);
  defsubr (&Streesit_induce_sparse_tree);
#endif /* HAVE_TREE_SITTER */
  defsubr (&Streesit_available_p);
}
