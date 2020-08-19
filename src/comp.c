/* Compile elisp into native code.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.

Author: Andrea Corallo <akrl@sdf.org>

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

#ifdef HAVE_NATIVE_COMP

#include <setjmp.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <libgccjit.h>
#include <epaths.h>

#include "puresize.h"
#include "window.h"
#include "dynlib.h"
#include "buffer.h"
#include "blockinput.h"
#include "sha512.h"


/********************************/
/* Dynamic loading of libgccjit */
/********************************/

#ifdef WINDOWSNT
# include "w32common.h"

#undef gcc_jit_block_add_assignment
#undef gcc_jit_block_add_comment
#undef gcc_jit_block_add_eval
#undef gcc_jit_block_end_with_conditional
#undef gcc_jit_block_end_with_jump
#undef gcc_jit_block_end_with_return
#undef gcc_jit_block_end_with_void_return
#undef gcc_jit_context_acquire
#undef gcc_jit_context_add_driver_option
#undef gcc_jit_context_compile_to_file
#undef gcc_jit_context_dump_reproducer_to_file
#undef gcc_jit_context_dump_to_file
#undef gcc_jit_context_get_builtin_function
#undef gcc_jit_context_get_first_error
#undef gcc_jit_context_get_int_type
#undef gcc_jit_context_get_type
#undef gcc_jit_context_new_array_access
#undef gcc_jit_context_new_array_type
#undef gcc_jit_context_new_binary_op
#undef gcc_jit_context_new_call
#undef gcc_jit_context_new_call_through_ptr
#undef gcc_jit_context_new_comparison
#undef gcc_jit_context_new_field
#undef gcc_jit_context_new_function
#undef gcc_jit_context_new_function_ptr_type
#undef gcc_jit_context_new_global
#undef gcc_jit_context_new_opaque_struct
#undef gcc_jit_context_new_param
#undef gcc_jit_context_new_rvalue_from_int
#undef gcc_jit_context_new_rvalue_from_long
#undef gcc_jit_context_new_rvalue_from_ptr
#undef gcc_jit_context_new_string_literal
#undef gcc_jit_context_new_struct_type
#undef gcc_jit_context_new_unary_op
#undef gcc_jit_context_new_union_type
#undef gcc_jit_context_release
#undef gcc_jit_context_set_bool_option
#undef gcc_jit_context_set_int_option
#undef gcc_jit_context_set_logfile
#undef gcc_jit_function_get_param
#undef gcc_jit_function_new_block
#undef gcc_jit_function_new_local
#undef gcc_jit_lvalue_access_field
#undef gcc_jit_lvalue_as_rvalue
#undef gcc_jit_lvalue_get_address
#undef gcc_jit_param_as_lvalue
#undef gcc_jit_param_as_rvalue
#undef gcc_jit_rvalue_access_field
#undef gcc_jit_rvalue_dereference
#undef gcc_jit_rvalue_dereference_field
#undef gcc_jit_rvalue_get_type
#undef gcc_jit_struct_as_type
#undef gcc_jit_struct_set_fields
#undef gcc_jit_type_get_pointer
#undef gcc_jit_version_major
#undef gcc_jit_version_minor
#undef gcc_jit_version_patchlevel

/* In alphabetical order */
DEF_DLL_FN (gcc_jit_rvalue *, gcc_jit_context_new_rvalue_from_int,
            (gcc_jit_context *ctxt, gcc_jit_type *numeric_type, int value));
DEF_DLL_FN (gcc_jit_rvalue *, gcc_jit_lvalue_as_rvalue,
            (gcc_jit_lvalue *lvalue));
DEF_DLL_FN (gcc_jit_rvalue *, gcc_jit_rvalue_access_field,
            (gcc_jit_rvalue *struct_or_union, gcc_jit_location *loc,
             gcc_jit_field *field));
DEF_DLL_FN (void, gcc_jit_block_add_comment,
            (gcc_jit_block *block, gcc_jit_location *loc, const char *text));
DEF_DLL_FN (void, gcc_jit_context_release, (gcc_jit_context *ctxt));
DEF_DLL_FN (const char *, gcc_jit_context_get_first_error,
            (gcc_jit_context *ctxt));
DEF_DLL_FN (gcc_jit_block *, gcc_jit_function_new_block,
            (gcc_jit_function *func, const char *name));
DEF_DLL_FN (gcc_jit_context *, gcc_jit_context_acquire, (void));
DEF_DLL_FN (void, gcc_jit_context_add_driver_option,
            (gcc_jit_context *ctxt, const char *optname));
DEF_DLL_FN (gcc_jit_field *, gcc_jit_context_new_field,
            (gcc_jit_context *ctxt, gcc_jit_location *loc, gcc_jit_type *type,
             const char *name));
DEF_DLL_FN (gcc_jit_function *, gcc_jit_context_get_builtin_function,
            (gcc_jit_context *ctxt, const char *name));
DEF_DLL_FN (gcc_jit_function *, gcc_jit_context_new_function,
            (gcc_jit_context *ctxt, gcc_jit_location *loc,
             enum gcc_jit_function_kind kind, gcc_jit_type *return_type,
             const char *name, int num_params, gcc_jit_param **params,
             int is_variadic));
DEF_DLL_FN (gcc_jit_lvalue *, gcc_jit_context_new_array_access,
            (gcc_jit_context *ctxt, gcc_jit_location *loc, gcc_jit_rvalue *ptr,
             gcc_jit_rvalue *index));
DEF_DLL_FN (gcc_jit_lvalue *, gcc_jit_context_new_global,
            (gcc_jit_context *ctxt, gcc_jit_location *loc,
             enum gcc_jit_global_kind kind, gcc_jit_type *type,
             const char *name));
DEF_DLL_FN (gcc_jit_lvalue *, gcc_jit_function_new_local,
            (gcc_jit_function *func, gcc_jit_location *loc, gcc_jit_type *type,
             const char *name));
DEF_DLL_FN (gcc_jit_lvalue *, gcc_jit_lvalue_access_field,
            (gcc_jit_lvalue *struct_or_union, gcc_jit_location *loc,
             gcc_jit_field *field));
DEF_DLL_FN (gcc_jit_lvalue *, gcc_jit_param_as_lvalue, (gcc_jit_param *param));
DEF_DLL_FN (gcc_jit_lvalue *, gcc_jit_rvalue_dereference,
            (gcc_jit_rvalue *rvalue, gcc_jit_location *loc));
DEF_DLL_FN (gcc_jit_lvalue *, gcc_jit_rvalue_dereference_field,
            (gcc_jit_rvalue *ptr, gcc_jit_location *loc, gcc_jit_field *field));
DEF_DLL_FN (gcc_jit_param *, gcc_jit_context_new_param,
            (gcc_jit_context *ctxt, gcc_jit_location *loc, gcc_jit_type *type,
             const char *name));
DEF_DLL_FN (gcc_jit_param *, gcc_jit_function_get_param,
            (gcc_jit_function *func, int index));
DEF_DLL_FN (gcc_jit_rvalue *, gcc_jit_context_new_binary_op,
            (gcc_jit_context *ctxt, gcc_jit_location *loc,
             enum gcc_jit_binary_op op, gcc_jit_type *result_type,
             gcc_jit_rvalue *a, gcc_jit_rvalue *b));
DEF_DLL_FN (gcc_jit_rvalue *, gcc_jit_context_new_call,
            (gcc_jit_context *ctxt, gcc_jit_location *loc,
             gcc_jit_function *func, int numargs , gcc_jit_rvalue **args));
DEF_DLL_FN (gcc_jit_rvalue *, gcc_jit_context_new_call_through_ptr,
            (gcc_jit_context *ctxt, gcc_jit_location *loc,
             gcc_jit_rvalue *fn_ptr, int numargs, gcc_jit_rvalue **args));
DEF_DLL_FN (gcc_jit_rvalue *, gcc_jit_context_new_comparison,
            (gcc_jit_context *ctxt, gcc_jit_location *loc,
             enum gcc_jit_comparison op, gcc_jit_rvalue *a, gcc_jit_rvalue *b));
DEF_DLL_FN (gcc_jit_rvalue *, gcc_jit_context_new_rvalue_from_long,
            (gcc_jit_context *ctxt, gcc_jit_type *numeric_type, long value));
DEF_DLL_FN (gcc_jit_rvalue *, gcc_jit_context_new_rvalue_from_ptr,
            (gcc_jit_context *ctxt, gcc_jit_type *pointer_type, void *value));
DEF_DLL_FN (gcc_jit_rvalue *, gcc_jit_context_new_string_literal,
            (gcc_jit_context *ctxt, const char *value));
DEF_DLL_FN (gcc_jit_rvalue *, gcc_jit_context_new_unary_op,
            (gcc_jit_context *ctxt, gcc_jit_location *loc,
             enum gcc_jit_unary_op op, gcc_jit_type *result_type,
             gcc_jit_rvalue *rvalue));
DEF_DLL_FN (gcc_jit_rvalue *, gcc_jit_lvalue_get_address,
            (gcc_jit_lvalue *lvalue, gcc_jit_location *loc));
DEF_DLL_FN (gcc_jit_rvalue *, gcc_jit_param_as_rvalue, (gcc_jit_param *param));
DEF_DLL_FN (gcc_jit_struct *, gcc_jit_context_new_opaque_struct,
            (gcc_jit_context *ctxt, gcc_jit_location *loc, const char *name));
DEF_DLL_FN (gcc_jit_struct *, gcc_jit_context_new_struct_type,
            (gcc_jit_context *ctxt, gcc_jit_location *loc, const char *name,
             int num_fields, gcc_jit_field **fields));
DEF_DLL_FN (gcc_jit_type *, gcc_jit_context_get_int_type,
            (gcc_jit_context *ctxt, int num_bytes, int is_signed));
DEF_DLL_FN (gcc_jit_type *, gcc_jit_context_get_type,
            (gcc_jit_context *ctxt, enum gcc_jit_types type_));
DEF_DLL_FN (gcc_jit_type *, gcc_jit_context_new_array_type,
            (gcc_jit_context *ctxt, gcc_jit_location *loc,
             gcc_jit_type *element_type, int num_elements));
DEF_DLL_FN (gcc_jit_type *, gcc_jit_context_new_function_ptr_type,
            (gcc_jit_context *ctxt, gcc_jit_location *loc,
             gcc_jit_type *return_type, int num_params,
             gcc_jit_type **param_types, int is_variadic));
DEF_DLL_FN (gcc_jit_type *, gcc_jit_context_new_union_type,
            (gcc_jit_context *ctxt, gcc_jit_location *loc, const char *name,
             int num_fields, gcc_jit_field **fields));
DEF_DLL_FN (gcc_jit_type *, gcc_jit_rvalue_get_type, (gcc_jit_rvalue *rvalue));
DEF_DLL_FN (gcc_jit_type *, gcc_jit_struct_as_type,
            (gcc_jit_struct *struct_type));
DEF_DLL_FN (gcc_jit_type *, gcc_jit_type_get_pointer, (gcc_jit_type *type));
DEF_DLL_FN (void, gcc_jit_block_add_assignment,
            (gcc_jit_block *block, gcc_jit_location *loc, gcc_jit_lvalue *lvalue,
             gcc_jit_rvalue *rvalue));
DEF_DLL_FN (void, gcc_jit_block_add_eval,
            (gcc_jit_block *block, gcc_jit_location *loc,
             gcc_jit_rvalue *rvalue));
DEF_DLL_FN (void, gcc_jit_block_end_with_conditional,
            (gcc_jit_block *block, gcc_jit_location *loc,
             gcc_jit_rvalue *boolval, gcc_jit_block *on_true,
             gcc_jit_block *on_false));
DEF_DLL_FN (void, gcc_jit_block_end_with_jump,
            (gcc_jit_block *block, gcc_jit_location *loc,
             gcc_jit_block *target));
DEF_DLL_FN (void, gcc_jit_block_end_with_return,
            (gcc_jit_block *block, gcc_jit_location *loc,
             gcc_jit_rvalue *rvalue));
DEF_DLL_FN (void, gcc_jit_block_end_with_void_return,
            (gcc_jit_block *block, gcc_jit_location *loc));
DEF_DLL_FN (void, gcc_jit_context_compile_to_file,
            (gcc_jit_context *ctxt, enum gcc_jit_output_kind output_kind,
             const char *output_path));
DEF_DLL_FN (void, gcc_jit_context_dump_reproducer_to_file,
            (gcc_jit_context *ctxt, const char *path));
DEF_DLL_FN (void, gcc_jit_context_dump_to_file,
            (gcc_jit_context *ctxt, const char *path, int update_locations));
DEF_DLL_FN (void, gcc_jit_context_set_bool_option,
            (gcc_jit_context *ctxt, enum gcc_jit_bool_option opt, int value));
DEF_DLL_FN (void, gcc_jit_context_set_int_option,
            (gcc_jit_context *ctxt, enum gcc_jit_int_option opt, int value));
DEF_DLL_FN (void, gcc_jit_context_set_logfile,
            (gcc_jit_context *ctxt, FILE *logfile, int flags, int verbosity));
DEF_DLL_FN (void, gcc_jit_struct_set_fields,
            (gcc_jit_struct *struct_type, gcc_jit_location *loc, int num_fields,
             gcc_jit_field **fields));
DEF_DLL_FN (int, gcc_jit_version_major, (void));
DEF_DLL_FN (int, gcc_jit_version_minor, (void));
DEF_DLL_FN (int, gcc_jit_version_patchlevel, (void));

static bool
init_gccjit_functions (void)
{
  HMODULE library = w32_delayed_load (Qgccjit);

  if (!library)
    return false;

  /* In alphabetical order */
  LOAD_DLL_FN (library, gcc_jit_block_add_assignment);
  LOAD_DLL_FN (library, gcc_jit_block_add_comment);
  LOAD_DLL_FN (library, gcc_jit_block_add_eval);
  LOAD_DLL_FN (library, gcc_jit_block_end_with_conditional);
  LOAD_DLL_FN (library, gcc_jit_block_end_with_jump);
  LOAD_DLL_FN (library, gcc_jit_block_end_with_return);
  LOAD_DLL_FN (library, gcc_jit_block_end_with_void_return);
  LOAD_DLL_FN (library, gcc_jit_context_acquire);
  LOAD_DLL_FN (library, gcc_jit_context_add_driver_option);
  LOAD_DLL_FN (library, gcc_jit_context_compile_to_file);
  LOAD_DLL_FN (library, gcc_jit_context_dump_reproducer_to_file);
  LOAD_DLL_FN (library, gcc_jit_context_dump_to_file);
  LOAD_DLL_FN (library, gcc_jit_context_get_builtin_function);
  LOAD_DLL_FN (library, gcc_jit_context_get_first_error);
  LOAD_DLL_FN (library, gcc_jit_context_get_int_type);
  LOAD_DLL_FN (library, gcc_jit_context_get_type);
  LOAD_DLL_FN (library, gcc_jit_context_new_array_access);
  LOAD_DLL_FN (library, gcc_jit_context_new_array_type);
  LOAD_DLL_FN (library, gcc_jit_context_new_binary_op);
  LOAD_DLL_FN (library, gcc_jit_context_new_call);
  LOAD_DLL_FN (library, gcc_jit_context_new_call_through_ptr);
  LOAD_DLL_FN (library, gcc_jit_context_new_comparison);
  LOAD_DLL_FN (library, gcc_jit_context_new_field);
  LOAD_DLL_FN (library, gcc_jit_context_new_function);
  LOAD_DLL_FN (library, gcc_jit_context_new_function_ptr_type);
  LOAD_DLL_FN (library, gcc_jit_context_new_global);
  LOAD_DLL_FN (library, gcc_jit_context_new_opaque_struct);
  LOAD_DLL_FN (library, gcc_jit_context_new_param);
  LOAD_DLL_FN (library, gcc_jit_context_new_rvalue_from_int);
  LOAD_DLL_FN (library, gcc_jit_context_new_rvalue_from_long);
  LOAD_DLL_FN (library, gcc_jit_context_new_rvalue_from_ptr);
  LOAD_DLL_FN (library, gcc_jit_context_new_string_literal);
  LOAD_DLL_FN (library, gcc_jit_context_new_struct_type);
  LOAD_DLL_FN (library, gcc_jit_context_new_unary_op);
  LOAD_DLL_FN (library, gcc_jit_context_new_union_type);
  LOAD_DLL_FN (library, gcc_jit_context_release);
  LOAD_DLL_FN (library, gcc_jit_context_set_bool_option);
  LOAD_DLL_FN (library, gcc_jit_context_set_int_option);
  LOAD_DLL_FN (library, gcc_jit_context_set_logfile);
  LOAD_DLL_FN (library, gcc_jit_function_get_param);
  LOAD_DLL_FN (library, gcc_jit_function_new_block);
  LOAD_DLL_FN (library, gcc_jit_function_new_local);
  LOAD_DLL_FN (library, gcc_jit_lvalue_access_field);
  LOAD_DLL_FN (library, gcc_jit_lvalue_as_rvalue);
  LOAD_DLL_FN (library, gcc_jit_lvalue_get_address);
  LOAD_DLL_FN (library, gcc_jit_param_as_lvalue);
  LOAD_DLL_FN (library, gcc_jit_param_as_rvalue);
  LOAD_DLL_FN (library, gcc_jit_rvalue_access_field);
  LOAD_DLL_FN (library, gcc_jit_rvalue_dereference);
  LOAD_DLL_FN (library, gcc_jit_rvalue_dereference_field);
  LOAD_DLL_FN (library, gcc_jit_rvalue_get_type);
  LOAD_DLL_FN (library, gcc_jit_struct_as_type);
  LOAD_DLL_FN (library, gcc_jit_struct_set_fields);
  LOAD_DLL_FN (library, gcc_jit_type_get_pointer);
  LOAD_DLL_FN_OPT (library, gcc_jit_version_major);
  LOAD_DLL_FN_OPT (library, gcc_jit_version_minor);
  LOAD_DLL_FN_OPT (library, gcc_jit_version_patchlevel);

  return true;
}

/* In alphabetical order */
#define gcc_jit_block_add_assignment fn_gcc_jit_block_add_assignment
#define gcc_jit_block_add_comment fn_gcc_jit_block_add_comment
#define gcc_jit_block_add_eval fn_gcc_jit_block_add_eval
#define gcc_jit_block_end_with_conditional fn_gcc_jit_block_end_with_conditional
#define gcc_jit_block_end_with_jump fn_gcc_jit_block_end_with_jump
#define gcc_jit_block_end_with_return fn_gcc_jit_block_end_with_return
#define gcc_jit_block_end_with_void_return fn_gcc_jit_block_end_with_void_return
#define gcc_jit_context_acquire fn_gcc_jit_context_acquire
#define gcc_jit_context_add_driver_option fn_gcc_jit_context_add_driver_option
#define gcc_jit_context_compile_to_file fn_gcc_jit_context_compile_to_file
#define gcc_jit_context_dump_reproducer_to_file fn_gcc_jit_context_dump_reproducer_to_file
#define gcc_jit_context_dump_to_file fn_gcc_jit_context_dump_to_file
#define gcc_jit_context_get_builtin_function fn_gcc_jit_context_get_builtin_function
#define gcc_jit_context_get_first_error fn_gcc_jit_context_get_first_error
#define gcc_jit_context_get_int_type fn_gcc_jit_context_get_int_type
#define gcc_jit_context_get_type fn_gcc_jit_context_get_type
#define gcc_jit_context_new_array_access fn_gcc_jit_context_new_array_access
#define gcc_jit_context_new_array_type fn_gcc_jit_context_new_array_type
#define gcc_jit_context_new_binary_op fn_gcc_jit_context_new_binary_op
#define gcc_jit_context_new_call fn_gcc_jit_context_new_call
#define gcc_jit_context_new_call_through_ptr fn_gcc_jit_context_new_call_through_ptr
#define gcc_jit_context_new_comparison fn_gcc_jit_context_new_comparison
#define gcc_jit_context_new_field fn_gcc_jit_context_new_field
#define gcc_jit_context_new_function fn_gcc_jit_context_new_function
#define gcc_jit_context_new_function_ptr_type fn_gcc_jit_context_new_function_ptr_type
#define gcc_jit_context_new_global fn_gcc_jit_context_new_global
#define gcc_jit_context_new_opaque_struct fn_gcc_jit_context_new_opaque_struct
#define gcc_jit_context_new_param fn_gcc_jit_context_new_param
#define gcc_jit_context_new_rvalue_from_int fn_gcc_jit_context_new_rvalue_from_int
#define gcc_jit_context_new_rvalue_from_long fn_gcc_jit_context_new_rvalue_from_long
#define gcc_jit_context_new_rvalue_from_ptr fn_gcc_jit_context_new_rvalue_from_ptr
#define gcc_jit_context_new_string_literal fn_gcc_jit_context_new_string_literal
#define gcc_jit_context_new_struct_type fn_gcc_jit_context_new_struct_type
#define gcc_jit_context_new_unary_op fn_gcc_jit_context_new_unary_op
#define gcc_jit_context_new_union_type fn_gcc_jit_context_new_union_type
#define gcc_jit_context_release fn_gcc_jit_context_release
#define gcc_jit_context_set_bool_option fn_gcc_jit_context_set_bool_option
#define gcc_jit_context_set_int_option fn_gcc_jit_context_set_int_option
#define gcc_jit_context_set_logfile fn_gcc_jit_context_set_logfile
#define gcc_jit_function_get_param fn_gcc_jit_function_get_param
#define gcc_jit_function_new_block fn_gcc_jit_function_new_block
#define gcc_jit_function_new_local fn_gcc_jit_function_new_local
#define gcc_jit_lvalue_access_field fn_gcc_jit_lvalue_access_field
#define gcc_jit_lvalue_as_rvalue fn_gcc_jit_lvalue_as_rvalue
#define gcc_jit_lvalue_get_address fn_gcc_jit_lvalue_get_address
#define gcc_jit_param_as_lvalue fn_gcc_jit_param_as_lvalue
#define gcc_jit_param_as_rvalue fn_gcc_jit_param_as_rvalue
#define gcc_jit_rvalue_access_field fn_gcc_jit_rvalue_access_field
#define gcc_jit_rvalue_dereference fn_gcc_jit_rvalue_dereference
#define gcc_jit_rvalue_dereference_field fn_gcc_jit_rvalue_dereference_field
#define gcc_jit_rvalue_get_type fn_gcc_jit_rvalue_get_type
#define gcc_jit_struct_as_type fn_gcc_jit_struct_as_type
#define gcc_jit_struct_set_fields fn_gcc_jit_struct_set_fields
#define gcc_jit_type_get_pointer fn_gcc_jit_type_get_pointer
#define gcc_jit_version_major fn_gcc_jit_version_major
#define gcc_jit_version_minor fn_gcc_jit_version_minor
#define gcc_jit_version_patchlevel fn_gcc_jit_version_patchlevel

#endif

static bool
load_gccjit_if_necessary (bool mandatory)
{
#ifdef WINDOWSNT
  static bool tried_to_initialize_once;
  static bool gccjit_initialized;

  if (!tried_to_initialize_once)
    {
      tried_to_initialize_once = true;
      Lisp_Object status;
      gccjit_initialized = init_gccjit_functions ();
      status = gccjit_initialized ? Qt : Qnil;
      Vlibrary_cache = Fcons (Fcons (Qgccjit, status), Vlibrary_cache);
    }

  if (mandatory && !gccjit_initialized)
    xsignal1 (Qnative_compiler_error, build_string ("libgccjit not found"));

  return gccjit_initialized;
#else
  return true;
#endif
}


#define ELN_FILENAME_HASH_LEN 64

/* C symbols emitted for the load relocation mechanism.  */
#define CURRENT_THREAD_RELOC_SYM "current_thread_reloc"
#define PURE_RELOC_SYM "pure_reloc"
#define DATA_RELOC_SYM "d_reloc"
#define DATA_RELOC_IMPURE_SYM "d_reloc_imp"
#define DATA_RELOC_EPHEMERAL_SYM "d_reloc_eph"

#define FUNC_LINK_TABLE_SYM "freloc_link_table"
#define LINK_TABLE_HASH_SYM "freloc_hash"
#define COMP_UNIT_SYM "comp_unit"
#define TEXT_DATA_RELOC_SYM "text_data_reloc"
#define TEXT_DATA_RELOC_IMPURE_SYM "text_data_reloc_imp"
#define TEXT_DATA_RELOC_EPHEMERAL_SYM "text_data_reloc_eph"

#define TEXT_OPTIM_QLY_SYM "text_optim_qly"
#define TEXT_FDOC_SYM "text_data_fdoc"


#define COMP_SPEED XFIXNUM (Fsymbol_value (Qcomp_speed))
#define COMP_DEBUG XFIXNUM (Fsymbol_value (Qcomp_debug))

#define STR_VALUE(s) #s
#define STR(s) STR_VALUE (s)

#define FIRST(x)				\
  XCAR(x)
#define SECOND(x)				\
  XCAR (XCDR (x))
#define THIRD(x)				\
  XCAR (XCDR (XCDR (x)))

/* Like call1 but stringify and intern.  */
#define CALL1I(fun, arg)				\
  CALLN (Ffuncall, intern_c_string (STR (fun)), arg)

/* Like call2 but stringify and intern.  */
#define CALL2I(fun, arg1, arg2)				\
  CALLN (Ffuncall, intern_c_string (STR (fun)), arg1, arg2)

#define DECL_BLOCK(name, func)				\
  gcc_jit_block *(name) =				\
    gcc_jit_function_new_block ((func), STR (name))

#ifndef WINDOWSNT
# ifdef HAVE__SETJMP
#  define SETJMP _setjmp
# else
#  define SETJMP setjmp
# endif
#else
/* snippet from MINGW-64 setjmp.h */
# define SETJMP _setjmp
#endif
#define SETJMP_NAME SETJMP

/* Max number function importable by native compiled code.  */
#define F_RELOC_MAX_SIZE 1500

typedef struct {
  void *link_table[F_RELOC_MAX_SIZE];
  ptrdiff_t size;
} f_reloc_t;

sigset_t saved_sigset;

static f_reloc_t freloc;

#define NUM_CAST_TYPES 15

enum cast_kind_of_type
  {
    kind_unsigned,
    kind_signed,
    kind_pointer
  };

/* C side of the compiler context.  */

typedef struct {
  gcc_jit_context *ctxt;
  gcc_jit_type *void_type;
  gcc_jit_type *bool_type;
  gcc_jit_type *char_type;
  gcc_jit_type *int_type;
  gcc_jit_type *unsigned_type;
  gcc_jit_type *long_type;
  gcc_jit_type *unsigned_long_type;
  gcc_jit_type *long_long_type;
  gcc_jit_type *unsigned_long_long_type;
  gcc_jit_type *emacs_int_type;
  gcc_jit_type *emacs_uint_type;
  gcc_jit_type *void_ptr_type;
  gcc_jit_type *char_ptr_type;
  gcc_jit_type *ptrdiff_type;
  gcc_jit_type *uintptr_type;
  gcc_jit_type *size_t_type;
  gcc_jit_type *lisp_word_type;
  gcc_jit_type *lisp_word_tag_type;
#ifdef LISP_OBJECT_IS_STRUCT
  gcc_jit_field *lisp_obj_i;
  gcc_jit_struct *lisp_obj_s;
#endif
  gcc_jit_type *lisp_obj_type;
  gcc_jit_type *lisp_obj_ptr_type;
  /* struct Lisp_Cons */
  gcc_jit_struct *lisp_cons_s;
  gcc_jit_field *lisp_cons_u;
  gcc_jit_field *lisp_cons_u_s;
  gcc_jit_field *lisp_cons_u_s_car;
  gcc_jit_field *lisp_cons_u_s_u;
  gcc_jit_field *lisp_cons_u_s_u_cdr;
  gcc_jit_type *lisp_cons_type;
  gcc_jit_type *lisp_cons_ptr_type;
  /* struct jmp_buf.  */
  gcc_jit_struct *jmp_buf_s;
  /* struct handler.  */
  gcc_jit_struct *handler_s;
  gcc_jit_field *handler_jmp_field;
  gcc_jit_field *handler_val_field;
  gcc_jit_field *handler_next_field;
  gcc_jit_type *handler_ptr_type;
  gcc_jit_lvalue *loc_handler;
  /* struct thread_state.  */
  gcc_jit_struct *thread_state_s;
  gcc_jit_field *m_handlerlist;
  gcc_jit_type *thread_state_ptr_type;
  gcc_jit_rvalue *current_thread_ref;
  /* Other globals.  */
  gcc_jit_rvalue *pure_ptr;
  /* libgccjit has really limited support for casting therefore this union will
     be used for the scope.  */
  gcc_jit_type *cast_union_type;
  gcc_jit_function *cast_functions_from_to[NUM_CAST_TYPES][NUM_CAST_TYPES];
  /*  We add one to make space for the last member which is the "biggest_type"
      member.  */
  gcc_jit_type *cast_types[NUM_CAST_TYPES + 1];
  size_t cast_type_sizes[NUM_CAST_TYPES + 1];
  enum cast_kind_of_type cast_type_kind[NUM_CAST_TYPES + 1];
  const char *cast_type_names[NUM_CAST_TYPES + 1];
  gcc_jit_field *cast_union_fields[NUM_CAST_TYPES + 1];
  size_t cast_union_field_biggest_type;
  gcc_jit_function *func; /* Current function being compiled.  */
  bool func_has_non_local; /* From comp-func has-non-local slot.  */
  EMACS_INT func_speed; /* From comp-func speed slot.  */
  gcc_jit_block *block;  /* Current basic block being compiled.  */
  gcc_jit_lvalue *scratch; /* Used as scratch slot for some code sequence (switch).  */
  gcc_jit_lvalue **frame; /* Frame slot n -> gcc_jit_lvalue *.  */
  gcc_jit_rvalue *zero;
  gcc_jit_rvalue *one;
  gcc_jit_rvalue *inttypebits;
  gcc_jit_rvalue *lisp_int0;
  gcc_jit_function *pseudovectorp;
  gcc_jit_function *bool_to_lisp_obj;
  gcc_jit_function *add1;
  gcc_jit_function *sub1;
  gcc_jit_function *negate;
  gcc_jit_function *car;
  gcc_jit_function *cdr;
  gcc_jit_function *setcar;
  gcc_jit_function *setcdr;
  gcc_jit_function *check_type;
  gcc_jit_function *check_impure;
  gcc_jit_function *maybe_gc_or_quit;
  Lisp_Object func_blocks_h; /* blk_name -> gcc_block.  */
  Lisp_Object exported_funcs_h; /* c-func-name -> gcc_jit_function *.  */
  Lisp_Object imported_funcs_h; /* subr_name -> gcc_jit_field *reloc_field.  */
  Lisp_Object emitter_dispatcher;
  /* Synthesized struct holding data relocs.  */
  gcc_jit_rvalue *data_relocs;
  /* Same as before but can't go in pure space. */
  gcc_jit_rvalue *data_relocs_impure;
  /* Same as before but content does not survive load phase. */
  gcc_jit_rvalue *data_relocs_ephemeral;
  /* Synthesized struct holding func relocs.  */
  gcc_jit_lvalue *func_relocs;
  gcc_jit_function *memcpy;
  Lisp_Object d_default_idx;
  Lisp_Object d_impure_idx;
  Lisp_Object d_ephemeral_idx;
} comp_t;

static comp_t comp;

FILE *logfile = NULL;

/* This is used for serialized objects by the reload mechanism.  */
typedef struct {
  ptrdiff_t len;
  const char data[];
} static_obj_t;

typedef struct {
  gcc_jit_rvalue *array;
  gcc_jit_rvalue *idx;
} imm_reloc_t;


/*
   Helper functions called by the run-time.
*/

void helper_unwind_protect (Lisp_Object handler);
Lisp_Object helper_temp_output_buffer_setup (Lisp_Object x);
Lisp_Object helper_unbind_n (Lisp_Object n);
void helper_save_restriction (void);
bool helper_PSEUDOVECTOR_TYPEP_XUNTAG (Lisp_Object a, enum pvec_type code);

void *helper_link_table[] =
  { wrong_type_argument,
    helper_PSEUDOVECTOR_TYPEP_XUNTAG,
    pure_write_error,
    push_handler,
    SETJMP_NAME,
    record_unwind_protect_excursion,
    helper_unbind_n,
    helper_save_restriction,
    record_unwind_current_buffer,
    set_internal,
    helper_unwind_protect,
    specbind,
    maybe_gc,
    maybe_quit };


static char * ATTRIBUTE_FORMAT_PRINTF (1, 2)
format_string (const char *format, ...)
{
  static char scratch_area[512];
  va_list va;
  va_start (va, format);
  int res = vsnprintf (scratch_area, sizeof (scratch_area), format, va);
  if (res >= sizeof (scratch_area))
    {
      scratch_area[sizeof (scratch_area) - 4] = '.';
      scratch_area[sizeof (scratch_area) - 3] = '.';
      scratch_area[sizeof (scratch_area) - 2] = '.';
    }
  va_end (va);
  return scratch_area;
}

static Lisp_Object
comp_hash_string (Lisp_Object string)
{
  Lisp_Object digest = make_uninit_string (SHA512_DIGEST_SIZE * 2);
  sha512_buffer (SSDATA (string), SCHARS (string), SSDATA (digest));
  hexbuf_digest (SSDATA (digest), SDATA (digest), SHA512_DIGEST_SIZE);

  return digest;
}

/* Produce a key hashing Vcomp_subr_list.  */

void
hash_native_abi (void)
{
  Lisp_Object string = Fmapconcat (intern_c_string ("subr-name"),
				   Vcomp_subr_list, build_string (" "));
  Lisp_Object digest = comp_hash_string (string);

  /* Check runs once.  */
  eassert (NILP (Vcomp_abi_hash));
  Vcomp_abi_hash = digest;
  /* If 10 characters are usually sufficient for git I guess 16 are
     fine for us here.  */
  Vcomp_native_path_postfix =
    concat2 (Vsystem_configuration,
	     concat2 (make_string ("-", 1),
		      Fsubstring_no_properties (Vcomp_abi_hash,
						make_fixnum (0),
						make_fixnum (16))));
}

static void
freloc_check_fill (void)
{
  if (freloc.size)
    return;

  eassert (!NILP (Vcomp_subr_list));

  if (ARRAYELTS (helper_link_table) > F_RELOC_MAX_SIZE)
    goto overflow;
  memcpy (freloc.link_table, helper_link_table, sizeof (helper_link_table));
  freloc.size = ARRAYELTS (helper_link_table);

  Lisp_Object subr_l = Vcomp_subr_list;
  FOR_EACH_TAIL (subr_l)
    {
      if (freloc.size == F_RELOC_MAX_SIZE)
	goto overflow;
      struct Lisp_Subr *subr = XSUBR (XCAR (subr_l));
      freloc.link_table[freloc.size] = subr->function.a0;
      freloc.size++;
    }
  return;

 overflow:
  fatal ("Overflowing function relocation table, increase F_RELOC_MAX_SIZE");
}

static void
bcall0 (Lisp_Object f)
{
  Ffuncall (1, &f);
}

static gcc_jit_block *
retrive_block (Lisp_Object block_name)
{
  Lisp_Object value = Fgethash (block_name, comp.func_blocks_h, Qnil);

  if (NILP (value))
    xsignal1 (Qnative_ice, build_string ("missing basic block"));

  return (gcc_jit_block *) xmint_pointer (value);
}

static void
declare_block (Lisp_Object block_name)
{
  char *name_str = SSDATA (SYMBOL_NAME (block_name));
  gcc_jit_block *block = gcc_jit_function_new_block (comp.func, name_str);
  Lisp_Object value = make_mint_ptr (block);

  if (!NILP (Fgethash (block_name, comp.func_blocks_h, Qnil)))
    xsignal1 (Qnative_ice, build_string ("double basic block declaration"));

  Fputhash (block_name, value, comp.func_blocks_h);
}

static gcc_jit_lvalue *
emit_mvar_lval (Lisp_Object mvar)
{
  Lisp_Object mvar_slot = CALL1I (comp-mvar-slot, mvar);

  if (EQ (mvar_slot, Qscratch))
    {
      if (!comp.scratch)
	comp.scratch = gcc_jit_function_new_local (comp.func,
						   NULL,
						   comp.lisp_obj_type,
						   "scratch");
      return comp.scratch;
    }

  return comp.frame[XFIXNUM (mvar_slot)];
}

static void
register_emitter (Lisp_Object key, void *func)
{
  Lisp_Object value = make_mint_ptr (func);
  Fputhash (key, value, comp.emitter_dispatcher);
}

static imm_reloc_t
obj_to_reloc (Lisp_Object obj)
{
  imm_reloc_t reloc;
  Lisp_Object idx;

  idx = Fgethash (obj, comp.d_default_idx, Qnil);
  if (!NILP (idx)) {
      reloc.array = comp.data_relocs;
      goto found;
  }

  idx = Fgethash (obj, comp.d_impure_idx, Qnil);
  if (!NILP (idx))
    {
      reloc.array = comp.data_relocs_impure;
      goto found;
    }

  idx = Fgethash (obj, comp.d_ephemeral_idx, Qnil);
  if (!NILP (idx))
    {
      reloc.array = comp.data_relocs_ephemeral;
      goto found;
    }

  xsignal1 (Qnative_ice,
	    build_string ("cant't find data in relocation containers"));
  assume (false);
 found:
  if (!FIXNUMP (idx))
    xsignal1 (Qnative_ice,
	      build_string ("inconsistent data relocation container"));
  reloc.idx = gcc_jit_context_new_rvalue_from_int (comp.ctxt,
						   comp.ptrdiff_type,
						   XFIXNUM (idx));
  return reloc;
}

static void
emit_comment (const char *str)
{
  if (COMP_DEBUG)
    gcc_jit_block_add_comment (comp.block,
			       NULL,
			       str);
}

/*
  Declare an imported function.
  When nargs is MANY (ptrdiff_t nargs, Lisp_Object *args) signature is assumed.
  When types is NULL args are assumed to be all Lisp_Objects.
*/
static gcc_jit_field *
declare_imported_func (Lisp_Object subr_sym, gcc_jit_type *ret_type,
		       int nargs, gcc_jit_type **types)
{
  USE_SAFE_ALLOCA;
  /* Don't want to declare the same function two times.  */
  if (!NILP (Fgethash (subr_sym, comp.imported_funcs_h, Qnil)))
    xsignal2 (Qnative_ice,
	      build_string ("unexpected double function declaration"),
	      subr_sym);

  if (nargs == MANY)
    {
      nargs = 2;
      types = SAFE_ALLOCA (nargs * sizeof (* types));
      types[0] = comp.ptrdiff_type;
      types[1] = comp.lisp_obj_ptr_type;
    }
  else if (nargs == UNEVALLED)
    {
      nargs = 1;
      types = SAFE_ALLOCA (nargs * sizeof (* types));
      types[0] = comp.lisp_obj_type;
    }
  else if (!types)
    {
      types = SAFE_ALLOCA (nargs * sizeof (* types));
      for (ptrdiff_t i = 0; i < nargs; i++)
	types[i] = comp.lisp_obj_type;
    }

  /* String containing the function ptr name.  */
  Lisp_Object f_ptr_name =
    CALLN (Ffuncall, intern_c_string ("comp-c-func-name"),
	   subr_sym, make_string ("R", 1));

  gcc_jit_type *f_ptr_type =
    gcc_jit_context_new_function_ptr_type (comp.ctxt,
					   NULL,
					   ret_type,
					   nargs,
					   types,
					   0);
  gcc_jit_field *field =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       f_ptr_type,
			       SSDATA (f_ptr_name));

  Fputhash (subr_sym, make_mint_ptr (field), comp.imported_funcs_h);
  SAFE_FREE ();
  return field;
}

/* Emit calls fetching from existing declarations.  */

static gcc_jit_rvalue *
emit_call (Lisp_Object func, gcc_jit_type *ret_type, ptrdiff_t nargs,
	   gcc_jit_rvalue **args, bool direct)
{
  Lisp_Object gcc_func =
    Fgethash (func,
	      direct ? comp.exported_funcs_h : comp.imported_funcs_h,
	      Qnil);

  if (NILP (gcc_func))
      xsignal2 (Qnative_ice,
		build_string ("missing function declaration"),
		func);

  if (direct)
    {
      emit_comment (format_string ("direct call to: %s",
				   SSDATA (func)));
      return gcc_jit_context_new_call (comp.ctxt,
				       NULL,
				       xmint_pointer (gcc_func),
				       nargs,
				       args);
    }
  else
    {
      gcc_jit_lvalue *f_ptr =
	gcc_jit_rvalue_dereference_field (
	  gcc_jit_lvalue_as_rvalue (comp.func_relocs),
	  NULL,
	  (gcc_jit_field *) xmint_pointer (gcc_func));

      if (!f_ptr)
	xsignal2 (Qnative_ice,
		  build_string ("missing function relocation"),
		  func);
      emit_comment (format_string ("calling subr: %s",
				   SSDATA (SYMBOL_NAME (func))));
      return gcc_jit_context_new_call_through_ptr (comp.ctxt,
						   NULL,
						   gcc_jit_lvalue_as_rvalue (f_ptr),
						   nargs,
						   args);
    }
}

static gcc_jit_rvalue *
emit_call_ref (Lisp_Object func, ptrdiff_t nargs,
	       gcc_jit_lvalue *base_arg, bool direct)
{
  gcc_jit_rvalue *args[] =
    { gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					   comp.ptrdiff_type,
					   nargs),
      gcc_jit_lvalue_get_address (base_arg, NULL) };
  return emit_call (func, comp.lisp_obj_type, 2, args, direct);
}

/* Close current basic block emitting a conditional.  */

static void
emit_cond_jump (gcc_jit_rvalue *test,
		gcc_jit_block *then_target, gcc_jit_block *else_target)
{
  if (gcc_jit_rvalue_get_type (test) == comp.bool_type)
    gcc_jit_block_end_with_conditional (comp.block,
				      NULL,
				      test,
				      then_target,
				      else_target);
  else
    /* In case test is not bool we do a logical negation to obtain a bool as
       result.  */
    gcc_jit_block_end_with_conditional (
      comp.block,
      NULL,
      gcc_jit_context_new_unary_op (comp.ctxt,
				    NULL,
				    GCC_JIT_UNARY_OP_LOGICAL_NEGATE,
				    comp.bool_type,
				    test),
      else_target,
      then_target);

}

static int
type_to_cast_index (gcc_jit_type * type)
{
  for (int i = 0; i < NUM_CAST_TYPES; ++i)
    if (type == comp.cast_types[i])
      return i;

  xsignal1 (Qnative_ice, build_string ("unsupported cast"));
}

static gcc_jit_rvalue *
emit_coerce (gcc_jit_type *new_type, gcc_jit_rvalue *obj)
{
  gcc_jit_type *old_type = gcc_jit_rvalue_get_type (obj);

  if (new_type == old_type)
    return obj;

#ifdef LISP_OBJECT_IS_STRUCT
  if (old_type == comp.lisp_obj_type)
    {
      gcc_jit_rvalue *lwordobj =
        gcc_jit_rvalue_access_field (obj, NULL, comp.lisp_obj_i);
      return emit_coerce (new_type, lwordobj);
    }

  if (new_type == comp.lisp_obj_type)
    {
      gcc_jit_rvalue *lwordobj =
        emit_coerce (comp.lisp_word_type, obj);

      static ptrdiff_t i;
      gcc_jit_lvalue *tmp_s =
	gcc_jit_function_new_local (comp.func, NULL, comp.lisp_obj_type,
				    format_string ("lisp_obj_%td", i++));

      gcc_jit_block_add_assignment (
	comp.block, NULL,
	gcc_jit_lvalue_access_field (tmp_s, NULL,
				     comp.lisp_obj_i),
	lwordobj);
      return gcc_jit_lvalue_as_rvalue (tmp_s);
    }
#endif

  int old_index = type_to_cast_index (old_type);
  int new_index = type_to_cast_index (new_type);

  if (comp.cast_type_sizes[old_index] < comp.cast_type_sizes[new_index]
      && comp.cast_type_kind[new_index] == kind_signed)
    xsignal3 (Qnative_ice,
              build_string ("FIXME: sign extension not implemented"),
              build_string (comp.cast_type_names[old_index]),
              build_string (comp.cast_type_names[new_index]));

  /* Lookup the appropriate cast function in the cast matrix.  */
  return gcc_jit_context_new_call (comp.ctxt,
           NULL,
           comp.cast_functions_from_to[old_index][new_index],
           1, &obj);
}

static gcc_jit_rvalue *
emit_binary_op (enum gcc_jit_binary_op op,
		gcc_jit_type *result_type,
		gcc_jit_rvalue *a, gcc_jit_rvalue *b)
{
  /* FIXME Check here for possible UB.  */
  return gcc_jit_context_new_binary_op (comp.ctxt, NULL,
					op,
					result_type,
					emit_coerce (result_type, a),
					emit_coerce (result_type, b));
}

/* Should come with libgccjit.  */

static gcc_jit_rvalue *
emit_rvalue_from_long_long (gcc_jit_type *type, long long n)
{
  emit_comment (format_string ("emit long long: %lld", n));

  gcc_jit_rvalue *high =
    gcc_jit_context_new_rvalue_from_long (comp.ctxt,
					  comp.unsigned_long_long_type,
					  (unsigned long long)n >> 32);
  gcc_jit_rvalue *low =
    emit_binary_op (GCC_JIT_BINARY_OP_RSHIFT,
		    comp.unsigned_long_long_type,
		    emit_binary_op (GCC_JIT_BINARY_OP_LSHIFT,
				    comp.unsigned_long_long_type,
				    gcc_jit_context_new_rvalue_from_long (
				      comp.ctxt,
				      comp.unsigned_long_long_type,
				      n),
				    gcc_jit_context_new_rvalue_from_int (
				      comp.ctxt,
				      comp.unsigned_long_long_type,
				      32)),
		    gcc_jit_context_new_rvalue_from_int (
		      comp.ctxt,
		      comp.unsigned_long_long_type,
		      32));

  return
    emit_coerce (type,
      emit_binary_op (
	GCC_JIT_BINARY_OP_BITWISE_OR,
	comp.unsigned_long_long_type,
	emit_binary_op (
	  GCC_JIT_BINARY_OP_LSHIFT,
	  comp.unsigned_long_long_type,
	  high,
	  gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					       comp.unsigned_long_long_type,
					       32)),
	low));
}

static gcc_jit_rvalue *
emit_rvalue_from_unsigned_long_long (gcc_jit_type *type, unsigned long long n)
{
  emit_comment (format_string ("emit unsigned long long: %llu", n));

  gcc_jit_rvalue *high =
    gcc_jit_context_new_rvalue_from_long (comp.ctxt,
					  comp.unsigned_long_long_type,
					  n >> 32);
  gcc_jit_rvalue *low =
    emit_binary_op (GCC_JIT_BINARY_OP_RSHIFT,
		    comp.unsigned_long_long_type,
		    emit_binary_op (GCC_JIT_BINARY_OP_LSHIFT,
				    comp.unsigned_long_long_type,
				    gcc_jit_context_new_rvalue_from_long (
				      comp.ctxt,
				      comp.unsigned_long_long_type,
				      n),
				    gcc_jit_context_new_rvalue_from_int (
				      comp.ctxt,
				      comp.unsigned_long_long_type,
				      32)),
		    gcc_jit_context_new_rvalue_from_int (
		      comp.ctxt,
		      comp.unsigned_long_long_type,
		      32));

  return emit_coerce (
           type,
           emit_binary_op (
             GCC_JIT_BINARY_OP_BITWISE_OR,
             comp.unsigned_long_long_type,
             emit_binary_op (
               GCC_JIT_BINARY_OP_LSHIFT,
               comp.unsigned_long_long_type,
               high,
               gcc_jit_context_new_rvalue_from_int (comp.ctxt,
                                                    comp.unsigned_long_long_type,
                                                    32)),
             low));
}

static gcc_jit_rvalue *
emit_rvalue_from_emacs_uint (EMACS_UINT val)
{
  if (val != (long) val)
    return emit_rvalue_from_unsigned_long_long (comp.emacs_uint_type, val);
  else
    return gcc_jit_context_new_rvalue_from_long (comp.ctxt,
						 comp.emacs_uint_type,
						 val);
}

static gcc_jit_rvalue *
emit_rvalue_from_emacs_int (EMACS_INT val)
{
  if (val != (long) val)
    return emit_rvalue_from_long_long (comp.emacs_int_type, val);
  else
    return gcc_jit_context_new_rvalue_from_long (comp.ctxt,
						 comp.emacs_int_type, val);
}

static gcc_jit_rvalue *
emit_rvalue_from_lisp_word_tag (Lisp_Word_tag val)
{
  if (val != (long) val)
    return emit_rvalue_from_unsigned_long_long (comp.lisp_word_tag_type, val);
  else
    return gcc_jit_context_new_rvalue_from_long (comp.ctxt,
						 comp.lisp_word_tag_type,
						 val);
}

static gcc_jit_rvalue *
emit_rvalue_from_lisp_word (Lisp_Word val)
{
#if LISP_WORDS_ARE_POINTERS
  return gcc_jit_context_new_rvalue_from_ptr (comp.ctxt,
                                              comp.lisp_word_type,
                                              val);
#else
  if (val != (long) val)
    return emit_rvalue_from_unsigned_long_long (comp.lisp_word_type, val);
  else
    return gcc_jit_context_new_rvalue_from_long (comp.ctxt,
						 comp.lisp_word_type,
						 val);
#endif
}

static gcc_jit_rvalue *
emit_rvalue_from_lisp_obj (Lisp_Object obj)
{
#ifdef LISP_OBJECT_IS_STRUCT
  return emit_coerce (comp.lisp_obj_type,
                      emit_rvalue_from_lisp_word (obj.i));
#else
  return emit_rvalue_from_lisp_word (obj);
#endif
}

/*
   Emit the equivalent of:
   (typeof_ptr) ((uintptr) ptr + size_of_ptr_ref * i)
*/

static gcc_jit_rvalue *
emit_ptr_arithmetic (gcc_jit_rvalue *ptr, gcc_jit_type *ptr_type,
		     int size_of_ptr_ref, gcc_jit_rvalue *i)
{
  emit_comment ("ptr_arithmetic");

  gcc_jit_rvalue *offset =
    emit_binary_op (
      GCC_JIT_BINARY_OP_MULT,
      comp.uintptr_type,
      gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					   comp.uintptr_type,
					   size_of_ptr_ref),
       i);

  return
    emit_coerce (
      ptr_type,
      emit_binary_op (
	GCC_JIT_BINARY_OP_PLUS,
	comp.uintptr_type,
	ptr,
	offset));
}

static gcc_jit_rvalue *
emit_XLI (gcc_jit_rvalue *obj)
{
  emit_comment ("XLI");
  return emit_coerce (comp.emacs_int_type, obj);
}

static gcc_jit_rvalue *
emit_XLP (gcc_jit_rvalue *obj)
{
  emit_comment ("XLP");

  return emit_coerce (comp.void_ptr_type, obj);
}

static gcc_jit_rvalue *
emit_XUNTAG (gcc_jit_rvalue *a, gcc_jit_type *type, Lisp_Word_tag lisp_word_tag)
{
  /* #define XUNTAG(a, type, ctype) ((ctype *)
     ((char *) XLP (a) - LISP_WORD_TAG (type))) */
  emit_comment ("XUNTAG");

  return emit_coerce (
	   gcc_jit_type_get_pointer (type),
	   emit_binary_op (
	     GCC_JIT_BINARY_OP_MINUS,
	     comp.uintptr_type,
	     emit_XLP (a),
	     emit_rvalue_from_lisp_word_tag (lisp_word_tag)));
}

static gcc_jit_rvalue *
emit_XCONS (gcc_jit_rvalue *a)
{
  emit_comment ("XCONS");

  return emit_XUNTAG (a,
		      gcc_jit_struct_as_type (comp.lisp_cons_s),
		      LISP_WORD_TAG (Lisp_Cons));
}

static gcc_jit_rvalue *
emit_EQ (gcc_jit_rvalue *x, gcc_jit_rvalue *y)
{
  emit_comment ("EQ");

  return gcc_jit_context_new_comparison (
	   comp.ctxt,
	   NULL,
	   GCC_JIT_COMPARISON_EQ,
	   emit_XLI (x),
	   emit_XLI (y));
}

static gcc_jit_rvalue *
emit_TAGGEDP (gcc_jit_rvalue *obj, Lisp_Word_tag tag)
{
   /* (! (((unsigned) (XLI (a) >> (USE_LSB_TAG ? 0 : VALBITS)) \
	- (unsigned) (tag)) \
	& ((1 << GCTYPEBITS) - 1))) */
  emit_comment ("TAGGEDP");

  gcc_jit_rvalue *sh_res =
    emit_binary_op (
      GCC_JIT_BINARY_OP_RSHIFT,
      comp.emacs_int_type,
      emit_XLI (obj),
      gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					   comp.emacs_int_type,
					   (USE_LSB_TAG ? 0 : VALBITS)));

  gcc_jit_rvalue *minus_res =
    emit_binary_op (
      GCC_JIT_BINARY_OP_MINUS,
	   comp.unsigned_type,
	   sh_res,
	   gcc_jit_context_new_rvalue_from_int (
	     comp.ctxt,
	     comp.unsigned_type,
	     tag));

  gcc_jit_rvalue *res =
   gcc_jit_context_new_unary_op (
     comp.ctxt,
     NULL,
     GCC_JIT_UNARY_OP_LOGICAL_NEGATE,
     comp.int_type,
     emit_binary_op (
       GCC_JIT_BINARY_OP_BITWISE_AND,
       comp.unsigned_type,
       minus_res,
       gcc_jit_context_new_rvalue_from_int (
	 comp.ctxt,
	 comp.unsigned_type,
	 ((1 << GCTYPEBITS) - 1))));

  return res;
}

static gcc_jit_rvalue *
emit_VECTORLIKEP (gcc_jit_rvalue *obj)
{
  emit_comment ("VECTORLIKEP");

  return emit_TAGGEDP (obj, Lisp_Vectorlike);
}

static gcc_jit_rvalue *
emit_CONSP (gcc_jit_rvalue *obj)
{
  emit_comment ("CONSP");

  return emit_TAGGEDP (obj, Lisp_Cons);
}

static gcc_jit_rvalue *
emit_FLOATP (gcc_jit_rvalue *obj)
{
  emit_comment ("FLOATP");

  return emit_TAGGEDP (obj, Lisp_Float);
}

static gcc_jit_rvalue *
emit_BIGNUMP (gcc_jit_rvalue *obj)
{
  /* PSEUDOVECTORP (x, PVEC_BIGNUM); */
  emit_comment ("BIGNUMP");

  gcc_jit_rvalue *args[] =
    { obj,
      gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					   comp.int_type,
					   PVEC_BIGNUM) };

  return gcc_jit_context_new_call (comp.ctxt,
				   NULL,
				   comp.pseudovectorp,
				   2,
				   args);
}

static gcc_jit_rvalue *
emit_FIXNUMP (gcc_jit_rvalue *obj)
{
  /* (! (((unsigned) (XLI (x) >> (USE_LSB_TAG ? 0 : FIXNUM_BITS))
	- (unsigned) (Lisp_Int0 >> !USE_LSB_TAG))
	& ((1 << INTTYPEBITS) - 1)))  */
  emit_comment ("FIXNUMP");

  gcc_jit_rvalue *sh_res =
    USE_LSB_TAG ? obj
    : emit_binary_op (GCC_JIT_BINARY_OP_RSHIFT,
		      comp.emacs_int_type,
		      emit_XLI (obj),
		      gcc_jit_context_new_rvalue_from_int (
			comp.ctxt,
			comp.emacs_int_type,
			FIXNUM_BITS));

  gcc_jit_rvalue *minus_res =
    emit_binary_op (
      GCC_JIT_BINARY_OP_MINUS,
	   comp.unsigned_type,
	   sh_res,
	   gcc_jit_context_new_rvalue_from_int (
	     comp.ctxt,
	     comp.unsigned_type,
	     (Lisp_Int0 >> !USE_LSB_TAG)));

  gcc_jit_rvalue *res =
   gcc_jit_context_new_unary_op (
     comp.ctxt,
     NULL,
     GCC_JIT_UNARY_OP_LOGICAL_NEGATE,
     comp.int_type,
     emit_binary_op (
       GCC_JIT_BINARY_OP_BITWISE_AND,
       comp.unsigned_type,
       minus_res,
       gcc_jit_context_new_rvalue_from_int (
	 comp.ctxt,
	 comp.unsigned_type,
	 ((1 << INTTYPEBITS) - 1))));

  return res;
}

static gcc_jit_rvalue *
emit_XFIXNUM (gcc_jit_rvalue *obj)
{
  emit_comment ("XFIXNUM");
  gcc_jit_rvalue *i = emit_coerce (comp.emacs_uint_type, emit_XLI (obj));

  /* FIXME: Implementation dependent (both RSHIFT are arithmetics).  */

  if (!USE_LSB_TAG)
    {
      i = emit_binary_op (GCC_JIT_BINARY_OP_LSHIFT,
			  comp.emacs_uint_type,
			  i,
			  comp.inttypebits);

      return emit_binary_op (GCC_JIT_BINARY_OP_RSHIFT,
			     comp.emacs_int_type,
			     i,
			     comp.inttypebits);
    }
  else
    return emit_coerce (comp.emacs_int_type,
			emit_binary_op (GCC_JIT_BINARY_OP_RSHIFT,
					comp.emacs_int_type,
					i,
					comp.inttypebits));
}

static gcc_jit_rvalue *
emit_INTEGERP (gcc_jit_rvalue *obj)
{
  emit_comment ("INTEGERP");

  return emit_binary_op (GCC_JIT_BINARY_OP_LOGICAL_OR,
			 comp.bool_type,
			 emit_FIXNUMP (obj),
			 emit_BIGNUMP (obj));
}

static gcc_jit_rvalue *
emit_NUMBERP (gcc_jit_rvalue *obj)
{
  emit_comment ("NUMBERP");

  return emit_binary_op (GCC_JIT_BINARY_OP_LOGICAL_OR,
			 comp.bool_type,
			 emit_INTEGERP (obj),
			 emit_FLOATP (obj));
}

static gcc_jit_rvalue *
emit_make_fixnum_LSB_TAG (gcc_jit_rvalue *n)
{
  /*
    EMACS_UINT u = n;
    n = u << INTTYPEBITS;
    n += int0;
  */

  gcc_jit_rvalue *tmp =
    emit_binary_op (GCC_JIT_BINARY_OP_LSHIFT,
		    comp.emacs_int_type,
		    n, comp.inttypebits);

  tmp = emit_binary_op (GCC_JIT_BINARY_OP_PLUS,
			comp.emacs_int_type,
			tmp, comp.lisp_int0);

  return emit_coerce (comp.lisp_obj_type, tmp);
}

static gcc_jit_rvalue *
emit_make_fixnum_MSB_TAG (gcc_jit_rvalue *n)
{
  /*
    n &= INTMASK;
    n += (int0 << VALBITS);
    return XIL (n);
  */

  gcc_jit_rvalue *intmask = emit_rvalue_from_emacs_uint (INTMASK);

  n = emit_binary_op (GCC_JIT_BINARY_OP_BITWISE_AND,
		      comp.emacs_uint_type,
		      intmask, n);

  n =
    emit_binary_op (GCC_JIT_BINARY_OP_PLUS,
		    comp.emacs_uint_type,
		    emit_binary_op (GCC_JIT_BINARY_OP_LSHIFT,
				    comp.emacs_uint_type,
				    comp.lisp_int0,
                                    emit_rvalue_from_emacs_uint (VALBITS)),
		    n);

  return emit_coerce (comp.lisp_obj_type, n);
}


static gcc_jit_rvalue *
emit_make_fixnum (gcc_jit_rvalue *obj)
{
  emit_comment ("make_fixnum");
  return USE_LSB_TAG
    ? emit_make_fixnum_LSB_TAG (obj)
    : emit_make_fixnum_MSB_TAG (obj);
}

static gcc_jit_lvalue *
emit_lisp_obj_reloc_lval (Lisp_Object obj)
{
  emit_comment (format_string ("l-value for lisp obj: %s",
			       SSDATA (Fprin1_to_string (obj, Qnil))));

  imm_reloc_t reloc = obj_to_reloc (obj);
  return gcc_jit_context_new_array_access (comp.ctxt,
					   NULL,
					   reloc.array,
					   reloc.idx);
}

static gcc_jit_rvalue *
emit_lisp_obj_rval (Lisp_Object obj)
{
  emit_comment (format_string ("const lisp obj: %s",
			       SSDATA (Fprin1_to_string (obj, Qnil))));

  if (EQ (obj, Qnil))
    {
      gcc_jit_rvalue *n;
      n = emit_rvalue_from_lisp_word ((Lisp_Word) iQnil);
      return emit_coerce (comp.lisp_obj_type, n);
    }

  return gcc_jit_lvalue_as_rvalue (emit_lisp_obj_reloc_lval (obj));
}

static gcc_jit_rvalue *
emit_NILP (gcc_jit_rvalue *x)
{
  emit_comment ("NILP");
  return emit_EQ (x, emit_lisp_obj_rval (Qnil));
}

static gcc_jit_rvalue *
emit_XCAR (gcc_jit_rvalue *c)
{
  emit_comment ("XCAR");

  /* XCONS (c)->u.s.car */
  return
    gcc_jit_rvalue_access_field (
      /* XCONS (c)->u.s */
      gcc_jit_rvalue_access_field (
	/* XCONS (c)->u */
	gcc_jit_lvalue_as_rvalue (
	  gcc_jit_rvalue_dereference_field (
	    emit_XCONS (c),
	    NULL,
	    comp.lisp_cons_u)),
	NULL,
	comp.lisp_cons_u_s),
      NULL,
      comp.lisp_cons_u_s_car);
}

static gcc_jit_lvalue *
emit_lval_XCAR (gcc_jit_rvalue *c)
{
  emit_comment ("lval_XCAR");

  /* XCONS (c)->u.s.car */
  return
    gcc_jit_lvalue_access_field (
      /* XCONS (c)->u.s */
      gcc_jit_lvalue_access_field (
	/* XCONS (c)->u */
	gcc_jit_rvalue_dereference_field (
	  emit_XCONS (c),
	  NULL,
	  comp.lisp_cons_u),
	NULL,
	comp.lisp_cons_u_s),
      NULL,
      comp.lisp_cons_u_s_car);
}

static gcc_jit_rvalue *
emit_XCDR (gcc_jit_rvalue *c)
{
  emit_comment ("XCDR");
  /* XCONS (c)->u.s.u.cdr */
  return
    gcc_jit_rvalue_access_field (
      /* XCONS (c)->u.s.u */
      gcc_jit_rvalue_access_field (
	/* XCONS (c)->u.s */
	gcc_jit_rvalue_access_field (
	  /* XCONS (c)->u */
	  gcc_jit_lvalue_as_rvalue (
	    gcc_jit_rvalue_dereference_field (
	      emit_XCONS (c),
	      NULL,
	      comp.lisp_cons_u)),
	  NULL,
	  comp.lisp_cons_u_s),
	NULL,
	comp.lisp_cons_u_s_u),
      NULL,
      comp.lisp_cons_u_s_u_cdr);
}

static gcc_jit_lvalue *
emit_lval_XCDR (gcc_jit_rvalue *c)
{
  emit_comment ("lval_XCDR");

  /* XCONS (c)->u.s.u.cdr */
  return
    gcc_jit_lvalue_access_field (
      /* XCONS (c)->u.s.u */
      gcc_jit_lvalue_access_field (
	/* XCONS (c)->u.s */
	gcc_jit_lvalue_access_field (
	  /* XCONS (c)->u */
	  gcc_jit_rvalue_dereference_field (
	    emit_XCONS (c),
	    NULL,
	    comp.lisp_cons_u),
	  NULL,
	  comp.lisp_cons_u_s),
	NULL,
	comp.lisp_cons_u_s_u),
      NULL,
      comp.lisp_cons_u_s_u_cdr);
}

static void
emit_CHECK_CONS (gcc_jit_rvalue *x)
{
  emit_comment ("CHECK_CONS");

  gcc_jit_rvalue *args[] =
    { emit_CONSP (x),
      emit_lisp_obj_rval (Qconsp),
      x };

  gcc_jit_block_add_eval (
    comp.block,
    NULL,
    gcc_jit_context_new_call (comp.ctxt,
			      NULL,
			      comp.check_type,
			      3,
			      args));
}

static gcc_jit_rvalue *
emit_car_addr (gcc_jit_rvalue *c)
{
  emit_comment ("car_addr");

  return gcc_jit_lvalue_get_address (emit_lval_XCAR (c), NULL);
}

static gcc_jit_rvalue *
emit_cdr_addr (gcc_jit_rvalue *c)
{
  emit_comment ("cdr_addr");

  return gcc_jit_lvalue_get_address (emit_lval_XCDR (c), NULL);
}

static void
emit_XSETCAR (gcc_jit_rvalue *c, gcc_jit_rvalue *n)
{
  emit_comment ("XSETCAR");

  gcc_jit_block_add_assignment (
    comp.block,
    NULL,
    gcc_jit_rvalue_dereference (
      emit_car_addr (c),
      NULL),
    n);
}

static void
emit_XSETCDR (gcc_jit_rvalue *c, gcc_jit_rvalue *n)
{
  emit_comment ("XSETCDR");

  gcc_jit_block_add_assignment (
    comp.block,
    NULL,
    gcc_jit_rvalue_dereference (
      emit_cdr_addr (c),
      NULL),
    n);
}

static gcc_jit_rvalue *
emit_PURE_P (gcc_jit_rvalue *ptr)
{

  emit_comment ("PURE_P");

  return
    gcc_jit_context_new_comparison (
      comp.ctxt,
      NULL,
      GCC_JIT_COMPARISON_LE,
      emit_binary_op (
	GCC_JIT_BINARY_OP_MINUS,
	comp.uintptr_type,
	ptr,
        comp.pure_ptr),
      gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					   comp.uintptr_type,
					   PURESIZE));
}


/*************************************/
/* Code emitted by LIMPLE statemes.  */
/*************************************/

/* Emit an r-value from an mvar meta variable.
   In case this is a constant that was propagated return it otherwise load it
   from frame.  */

static gcc_jit_rvalue *
emit_mvar_rval (Lisp_Object mvar)
{
  Lisp_Object const_vld = CALL1I (comp-mvar-const-vld, mvar);
  Lisp_Object constant = CALL1I (comp-mvar-constant, mvar);

  if (!NILP (const_vld))
    {
      if (COMP_DEBUG > 1)
	{
	  Lisp_Object func =
	    Fgethash (constant,
		      CALL1I (comp-ctxt-byte-func-to-func-h, Vcomp_ctxt),
		      Qnil);

	  emit_comment (
	    SSDATA (
	      Fprin1_to_string (
		NILP (func) ? constant : CALL1I (comp-func-c-name, func),
		Qnil)));
	}
      if (FIXNUMP (constant))
	{
	  /* We can still emit directly objects that are self-contained in a
	     word (read fixnums).  */
          return emit_rvalue_from_lisp_obj (constant);
	}
      /* Other const objects are fetched from the reloc array.  */
      return emit_lisp_obj_rval (constant);
    }

  return gcc_jit_lvalue_as_rvalue (emit_mvar_lval (mvar));
}

static void
emit_frame_assignment (Lisp_Object dst_mvar, gcc_jit_rvalue *val)
{

  gcc_jit_block_add_assignment (
    comp.block,
    NULL,
    emit_mvar_lval (dst_mvar),
    val);
}

static gcc_jit_rvalue *
emit_set_internal (Lisp_Object args)
{
  /*
    Ex: (set_internal #s(comp-mvar nil nil t comp-test-up-val nil nil)
                      #s(comp-mvar 1 4 t nil symbol nil)).
  */
  /* TODO: Inline the most common case.  */
  if (list_length (args) != 3)
    xsignal2 (Qnative_ice,
	      build_string ("unexpected arg length for insns"),
	      args);

  args = XCDR (args);
  int i = 0;
  gcc_jit_rvalue *gcc_args[4];
  FOR_EACH_TAIL (args)
    gcc_args[i++] = emit_mvar_rval (XCAR (args));
  gcc_args[2] = emit_lisp_obj_rval (Qnil);
  gcc_args[3] = gcc_jit_context_new_rvalue_from_int (comp.ctxt,
						     comp.int_type,
						     SET_INTERNAL_SET);
  return emit_call (intern_c_string ("set_internal"), comp.void_type , 4,
		    gcc_args, false);
}

/* This is for a regular function with arguments as m-var.  */

static gcc_jit_rvalue *
emit_simple_limple_call (Lisp_Object args, gcc_jit_type *ret_type, bool direct)
{
  USE_SAFE_ALLOCA;
  int i = 0;
  Lisp_Object callee = FIRST (args);
  args = XCDR (args);
  ptrdiff_t nargs = list_length (args);
  gcc_jit_rvalue **gcc_args = SAFE_ALLOCA (nargs * sizeof (*gcc_args));
  FOR_EACH_TAIL (args)
    gcc_args[i++] = emit_mvar_rval (XCAR (args));

  SAFE_FREE ();
  return emit_call (callee, ret_type, nargs, gcc_args, direct);
}

static gcc_jit_rvalue *
emit_simple_limple_call_lisp_ret (Lisp_Object args)
{
  /*
    Ex: (call Fcons #s(comp-mvar 3 0 t 1 nil) #s(comp-mvar 4 nil t nil nil)).
  */
  return emit_simple_limple_call (args, comp.lisp_obj_type, false);
}

static gcc_jit_rvalue *
emit_simple_limple_call_void_ret (Lisp_Object args)
{
  return emit_simple_limple_call (args, comp.void_type, false);
}

/* Entry point to dispatch emitting (call fun ...).  */

static gcc_jit_rvalue *
emit_limple_call (Lisp_Object insn)
{
  Lisp_Object callee_sym = FIRST (insn);
  Lisp_Object emitter = Fgethash (callee_sym, comp.emitter_dispatcher, Qnil);

  if (!NILP (emitter))
    {
      gcc_jit_rvalue * (* emitter_ptr) (Lisp_Object) = xmint_pointer (emitter);
      return emitter_ptr (insn);
    }

  return emit_simple_limple_call_lisp_ret (insn);
}

static gcc_jit_rvalue *
emit_limple_call_ref (Lisp_Object insn, bool direct)
{
  /* Ex: (funcall #s(comp-mvar 1 5 t eql symbol t)
                  #s(comp-mvar 2 6 nil nil nil t)
		  #s(comp-mvar 3 7 t 0 fixnum t)).  */
  static int i = 0;
  Lisp_Object callee = FIRST (insn);
  EMACS_INT nargs = XFIXNUM (Flength (CDR (insn)));

  if (!nargs)
    return emit_call_ref (callee, 0, comp.frame[0], direct);

  if (comp.func_has_non_local || !comp.func_speed)
    {
      /* FIXME: See bug#42360.  */
      Lisp_Object first_arg = SECOND (insn);
      EMACS_INT first_slot = XFIXNUM (CALL1I (comp-mvar-slot, first_arg));
      return emit_call_ref (callee, nargs, comp.frame[first_slot], direct);
    }

  gcc_jit_lvalue *tmp_arr =
    gcc_jit_function_new_local (
      comp.func,
      NULL,
      gcc_jit_context_new_array_type (comp.ctxt,
				      NULL,
				      comp.lisp_obj_type,
				      nargs),
      format_string ("call_arr_%d", i++));

  ptrdiff_t j = 0;
  Lisp_Object arg = CDR (insn);
  FOR_EACH_TAIL (arg)
    {
      gcc_jit_block_add_assignment (
        comp.block,
	NULL,
	gcc_jit_context_new_array_access (
	  comp.ctxt,
	  NULL,
	  gcc_jit_lvalue_as_rvalue (tmp_arr),
	  gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					       comp.int_type,
					       j)),
	emit_mvar_rval (XCAR (arg)));
      ++j;
    }

  return emit_call_ref (
	   callee,
	   nargs,
	   gcc_jit_context_new_array_access (comp.ctxt,
					     NULL,
					     gcc_jit_lvalue_as_rvalue (tmp_arr),
					     comp.zero),
	   direct);
}

static gcc_jit_rvalue *
emit_setjmp (gcc_jit_rvalue *buf)
{
#ifndef WINDOWSNT
  gcc_jit_rvalue *args[] = {buf};
  return emit_call (intern_c_string (STR (SETJMP_NAME)), comp.int_type, 1, args,
                   false);
#else
  /* _setjmp (buf, __builtin_frame_address (0)) */
  gcc_jit_rvalue *args[2];

  args[0] =
    gcc_jit_context_new_rvalue_from_int (comp.ctxt, comp.unsigned_type, 0);

  args[1] =
    gcc_jit_context_new_call (
      comp.ctxt,
      NULL,
      gcc_jit_context_get_builtin_function (comp.ctxt,
					    "__builtin_frame_address"),
      1, args);
  args[0] = buf;
  return emit_call (intern_c_string (STR (SETJMP_NAME)), comp.int_type, 2, args,
                    false);
#endif
}

/* Register an handler for a non local exit.  */

static void
emit_limple_push_handler (gcc_jit_rvalue *handler, gcc_jit_rvalue *handler_type,
			  gcc_jit_block *handler_bb, gcc_jit_block *guarded_bb,
			  Lisp_Object clobbered_mvar)
{
   /* struct handler *c = push_handler (POP, type);  */

  gcc_jit_rvalue *args[] = { handler, handler_type };
  gcc_jit_block_add_assignment (
    comp.block,
    NULL,
    comp.loc_handler,
    emit_call (intern_c_string ("push_handler"),
	       comp.handler_ptr_type, 2, args, false));

  args[0] =
    gcc_jit_lvalue_get_address (
	gcc_jit_rvalue_dereference_field (
	  gcc_jit_lvalue_as_rvalue (comp.loc_handler),
	  NULL,
	  comp.handler_jmp_field),
	NULL);

  gcc_jit_rvalue *res;
  res = emit_setjmp (args[0]);
  emit_cond_jump (res, handler_bb, guarded_bb);
}

static void
emit_limple_insn (Lisp_Object insn)
{
  Lisp_Object op = XCAR (insn);
  Lisp_Object args = XCDR (insn);
  gcc_jit_rvalue *res;
  Lisp_Object arg[6];

  Lisp_Object p = XCDR (insn);
  ptrdiff_t i = 0;
  FOR_EACH_TAIL (p)
    {
      if (i == sizeof (arg) / sizeof (Lisp_Object))
	break;
      arg[i++] = XCAR (p);
    }

  if (EQ (op, Qjump))
    {
      /* Unconditional branch.  */
      gcc_jit_block *target = retrive_block (arg[0]);
      gcc_jit_block_end_with_jump (comp.block, NULL, target);
    }
  else if (EQ (op, Qcond_jump))
    {
      /* Conditional branch.  */
      gcc_jit_rvalue *a = emit_mvar_rval (arg[0]);
      gcc_jit_rvalue *b = emit_mvar_rval (arg[1]);
      gcc_jit_block *target1 = retrive_block (arg[2]);
      gcc_jit_block *target2 = retrive_block (arg[3]);

      emit_cond_jump (emit_EQ (a, b), target2, target1);
    }
  else if (EQ (op, Qcond_jump_narg_leq))
    {
      /*
	 Limple: (cond-jump-narg-less 2 entry_2 entry_fallback_2)
	 C: if (nargs < 2) goto entry2_fallback; else goto entry_2;
      */
      gcc_jit_lvalue *nargs =
	gcc_jit_param_as_lvalue (gcc_jit_function_get_param (comp.func, 0));
      gcc_jit_rvalue *n =
	gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					     comp.ptrdiff_type,
					     XFIXNUM (arg[0]));
      gcc_jit_block *target1 = retrive_block (arg[1]);
      gcc_jit_block *target2 = retrive_block (arg[2]);
      gcc_jit_rvalue *test = gcc_jit_context_new_comparison (
			       comp.ctxt,
			       NULL,
			       GCC_JIT_COMPARISON_LE,
			       gcc_jit_lvalue_as_rvalue (nargs),
			       n);
      emit_cond_jump (test, target2, target1);
    }
  else if (EQ (op, Qphi))
    {
      /* Nothing to do for phis into the backend.  */
    }
  else if (EQ (op, Qpush_handler))
    {
      /* (push-handler condition-case #s(comp-mvar 0 3 t (arith-error) cons nil) 1 bb_2 bb_1) */
      int h_num UNINIT;
      Lisp_Object handler_spec = arg[0];
      gcc_jit_rvalue *handler = emit_mvar_rval (arg[1]);
      if (EQ (handler_spec, Qcatcher))
	h_num = CATCHER;
      else if (EQ (handler_spec, Qcondition_case))
	h_num = CONDITION_CASE;
      else
	xsignal2 (Qnative_ice, build_string ("incoherent insn"), insn);
      gcc_jit_rvalue *handler_type =
	gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					     comp.int_type,
					     h_num);
      gcc_jit_block *handler_bb = retrive_block (arg[2]);
      gcc_jit_block *guarded_bb = retrive_block (arg[3]);
      emit_limple_push_handler (handler, handler_type, handler_bb, guarded_bb,
				arg[0]);
    }
  else if (EQ (op, Qpop_handler))
    {
      /*
	C: current_thread->m_handlerlist =
	     current_thread->m_handlerlist->next;
      */
      gcc_jit_lvalue *m_handlerlist =
	gcc_jit_rvalue_dereference_field (
	  gcc_jit_lvalue_as_rvalue (
	    gcc_jit_rvalue_dereference (comp.current_thread_ref, NULL)),
	  NULL,
	  comp.m_handlerlist);

      gcc_jit_block_add_assignment (
	comp.block,
	NULL,
	m_handlerlist,
	gcc_jit_lvalue_as_rvalue (
	  gcc_jit_rvalue_dereference_field (
	    gcc_jit_lvalue_as_rvalue (m_handlerlist),
	    NULL,
	    comp.handler_next_field)));

    }
  else if (EQ (op, Qfetch_handler))
    {
      gcc_jit_lvalue *m_handlerlist =
	gcc_jit_rvalue_dereference_field (
	  gcc_jit_lvalue_as_rvalue (
	    gcc_jit_rvalue_dereference (comp.current_thread_ref, NULL)),
	  NULL,
	  comp.m_handlerlist);
      gcc_jit_block_add_assignment (comp.block,
				    NULL,
				    comp.loc_handler,
				    gcc_jit_lvalue_as_rvalue (m_handlerlist));

      gcc_jit_block_add_assignment (
	comp.block,
	NULL,
	m_handlerlist,
	gcc_jit_lvalue_as_rvalue (
	  gcc_jit_rvalue_dereference_field (
	    gcc_jit_lvalue_as_rvalue (comp.loc_handler),
	    NULL,
	    comp.handler_next_field)));
      emit_frame_assignment (
	arg[0],
	gcc_jit_lvalue_as_rvalue (
	  gcc_jit_rvalue_dereference_field (
	    gcc_jit_lvalue_as_rvalue (comp.loc_handler),
	    NULL,
	    comp.handler_val_field)));
    }
  else if (EQ (op, Qcall))
    {
      gcc_jit_block_add_eval (comp.block, NULL,
			      emit_limple_call (args));
    }
  else if (EQ (op, Qcallref))
    {
      gcc_jit_block_add_eval (comp.block, NULL,
			      emit_limple_call_ref (args, false));
    }
  else if (EQ (op, Qdirect_call))
    {
      gcc_jit_block_add_eval (
        comp.block, NULL,
	emit_simple_limple_call (XCDR (insn), comp.lisp_obj_type, true));
    }
  else if (EQ (op, Qdirect_callref))
    {
      gcc_jit_block_add_eval (comp.block, NULL,
			      emit_limple_call_ref (XCDR (insn), true));
    }
  else if (EQ (op, Qset))
    {
      Lisp_Object arg1 = arg[1];

      if (EQ (Ftype_of (arg1), Qcomp_mvar))
	res = emit_mvar_rval (arg1);
      else if (EQ (FIRST (arg1), Qcall))
	res = emit_limple_call (XCDR (arg1));
      else if (EQ (FIRST (arg1), Qcallref))
	res = emit_limple_call_ref (XCDR (arg1), false);
      else if (EQ (FIRST (arg1), Qdirect_call))
	res = emit_simple_limple_call (XCDR (arg1), comp.lisp_obj_type, true);
      else if (EQ (FIRST (arg1), Qdirect_callref))
	res = emit_limple_call_ref (XCDR (arg1), true);
      else
	xsignal2 (Qnative_ice,
		  build_string ("LIMPLE inconsistent arg1 for insn"),
		  insn);

      if (!res)
	xsignal1 (Qnative_ice,
		  build_string (gcc_jit_context_get_first_error (comp.ctxt)));

      emit_frame_assignment (arg[0], res);
    }
  else if (EQ (op, Qset_par_to_local))
    {
      /* Ex: (set-par-to-local #s(comp-mvar 0 3 nil nil nil nil) 0).  */
      EMACS_INT param_n = XFIXNUM (arg[1]);
      gcc_jit_rvalue *param =
	gcc_jit_param_as_rvalue (gcc_jit_function_get_param (comp.func,
							     param_n));
      emit_frame_assignment (arg[0], param);
    }
  else if (EQ (op, Qset_args_to_local))
    {
      /*
	Ex: (set-args-to-local #s(comp-mvar 1 6 nil nil nil nil))
	C: local[1] = *args;
      */
      gcc_jit_rvalue *gcc_args =
	gcc_jit_lvalue_as_rvalue (
	  gcc_jit_param_as_lvalue (gcc_jit_function_get_param (comp.func, 1)));

      gcc_jit_rvalue *res =
	gcc_jit_lvalue_as_rvalue (gcc_jit_rvalue_dereference (gcc_args, NULL));

      emit_frame_assignment (arg[0], res);
    }
  else if (EQ (op, Qset_rest_args_to_local))
    {
      /*
        Ex: (set-rest-args-to-local #s(comp-mvar 2 9 nil nil nil nil))
        C: local[2] = list (nargs - 2, args);
      */

      EMACS_INT slot_n = XFIXNUM (CALL1I (comp-mvar-slot, arg[0]));
      gcc_jit_rvalue *n =
	gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					     comp.ptrdiff_type,
					     slot_n);
      gcc_jit_lvalue *nargs =
	gcc_jit_param_as_lvalue (gcc_jit_function_get_param (comp.func, 0));
      gcc_jit_lvalue *args =
	gcc_jit_param_as_lvalue (gcc_jit_function_get_param (comp.func, 1));

      gcc_jit_rvalue *list_args[] =
	{ emit_binary_op (GCC_JIT_BINARY_OP_MINUS,
			  comp.ptrdiff_type,
			  gcc_jit_lvalue_as_rvalue (nargs),
			  n),
	  gcc_jit_lvalue_as_rvalue (args) };

      res = emit_call (Qlist, comp.lisp_obj_type, 2,
		       list_args, false);

      emit_frame_assignment (arg[0], res);
    }
  else if (EQ (op, Qinc_args))
    {
      /*
	Ex: (inc-args)
	C: ++args;
      */
      gcc_jit_lvalue *args =
	gcc_jit_param_as_lvalue (gcc_jit_function_get_param (comp.func, 1));

      gcc_jit_block_add_assignment (comp.block,
				    NULL,
				    args,
				    emit_ptr_arithmetic (
				      gcc_jit_lvalue_as_rvalue (args),
				      comp.lisp_obj_ptr_type,
				      sizeof (Lisp_Object),
				      comp.one));
    }
  else if (EQ (op, Qsetimm))
    {
      /* Ex: (setimm #s(comp-mvar 9 1 t 3 nil) a).  */
      emit_comment (SSDATA (Fprin1_to_string (arg[1], Qnil)));
      imm_reloc_t reloc = obj_to_reloc (arg[1]);
      emit_frame_assignment (
	arg[0],
	gcc_jit_lvalue_as_rvalue (
	  gcc_jit_context_new_array_access (comp.ctxt,
					    NULL,
					    reloc.array,
					    reloc.idx)));
    }
  else if (EQ (op, Qcomment))
    {
      /* Ex: (comment "Function: foo").  */
      emit_comment (SSDATA (arg[0]));
    }
  else if (EQ (op, Qreturn))
    {
      gcc_jit_block_end_with_return (comp.block,
				     NULL,
				     emit_mvar_rval (arg[0]));
    }
  else
    {
      xsignal2 (Qnative_ice,
		build_string ("LIMPLE op inconsistent"),
		op);
    }
}


/**************/
/* Inliners.  */
/**************/

static gcc_jit_rvalue *
emit_call_with_type_hint (gcc_jit_function *func, Lisp_Object insn,
			  Lisp_Object type)
{
  bool type_hint = EQ (CALL1I (comp-mvar-type, SECOND (insn)), type);
  gcc_jit_rvalue *args[] =
    { emit_mvar_rval (SECOND (insn)),
      gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					   comp.bool_type,
					   type_hint) };

  return gcc_jit_context_new_call (comp.ctxt, NULL, func, 2, args);
}

/* Same as before but with two args. The type hint is on the 2th.  */
static gcc_jit_rvalue *
emit_call2_with_type_hint (gcc_jit_function *func, Lisp_Object insn,
			   Lisp_Object type)
{
  bool type_hint = EQ (CALL1I (comp-mvar-type, SECOND (insn)), type);
  gcc_jit_rvalue *args[] =
    { emit_mvar_rval (SECOND (insn)),
      emit_mvar_rval (THIRD (insn)),
      gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					   comp.bool_type,
					   type_hint) };

  return gcc_jit_context_new_call (comp.ctxt, NULL, func, 3, args);
}


static gcc_jit_rvalue *
emit_add1 (Lisp_Object insn)
{
  return emit_call_with_type_hint (comp.add1, insn, Qfixnum);
}

static gcc_jit_rvalue *
emit_sub1 (Lisp_Object insn)
{
  return emit_call_with_type_hint (comp.sub1, insn, Qfixnum);
}

static gcc_jit_rvalue *
emit_negate (Lisp_Object insn)
{
  return emit_call_with_type_hint (comp.negate, insn, Qfixnum);
}

static gcc_jit_rvalue *
emit_consp (Lisp_Object insn)
{
  gcc_jit_rvalue *x = emit_mvar_rval (SECOND (insn));
  gcc_jit_rvalue *res = emit_coerce (comp.bool_type,
				   emit_CONSP (x));
  return gcc_jit_context_new_call (comp.ctxt,
				   NULL,
				   comp.bool_to_lisp_obj,
				   1, &res);
}

static gcc_jit_rvalue *
emit_car (Lisp_Object insn)
{
  return emit_call_with_type_hint (comp.car, insn, Qcons);
}

static gcc_jit_rvalue *
emit_cdr (Lisp_Object insn)
{
  return emit_call_with_type_hint (comp.cdr, insn, Qcons);
}

static gcc_jit_rvalue *
emit_setcar (Lisp_Object insn)
{
  return emit_call2_with_type_hint (comp.setcar, insn, Qcons);
}

static gcc_jit_rvalue *
emit_setcdr (Lisp_Object insn)
{
  return emit_call2_with_type_hint (comp.setcdr, insn, Qcons);
}

static gcc_jit_rvalue *
emit_numperp (Lisp_Object insn)
{
  gcc_jit_rvalue *x = emit_mvar_rval (SECOND (insn));
  gcc_jit_rvalue *res = emit_NUMBERP (x);
  return gcc_jit_context_new_call (comp.ctxt, NULL, comp.bool_to_lisp_obj, 1,
				   &res);
}

static gcc_jit_rvalue *
emit_integerp (Lisp_Object insn)
{
  gcc_jit_rvalue *x = emit_mvar_rval (SECOND (insn));
  gcc_jit_rvalue *res = emit_INTEGERP (x);
  return gcc_jit_context_new_call (comp.ctxt, NULL, comp.bool_to_lisp_obj, 1,
				   &res);
}

static gcc_jit_rvalue *
emit_maybe_gc_or_quit (Lisp_Object insn)
{
  return gcc_jit_context_new_call (comp.ctxt, NULL, comp.maybe_gc_or_quit, 0,
				   NULL);
}

/* This is in charge of serializing an object and export a function to
   retrieve it at load time.  */
static void
emit_static_object (const char *name, Lisp_Object obj)
{
  /* libgccjit has no support for initialized static data.
     The mechanism below is certainly not aesthetic but I assume the bottle neck
     in terms of performance at load time will still be the reader.
     NOTE: we can not rely on libgccjit even for valid NULL terminated C
     strings cause of this funny bug that will affect all pre gcc10 era gccs:
     https://gcc.gnu.org/ml/jit/2019-q3/msg00013.html  */

  ptrdiff_t count = SPECPDL_INDEX ();
  /* Preserve uninterned symbols, this is specifically necessary for
     CL macro expansion in dynamic scope code (bug#42088).  See
     `byte-compile-output-file-form'.  */
  specbind (intern_c_string ("print-escape-newlines"), Qt);
  specbind (intern_c_string ("print-length"), Qnil);
  specbind (intern_c_string ("print-level"), Qnil);
  specbind (intern_c_string ("print-quoted"), Qt);
  specbind (intern_c_string ("print-gensym"), Qt);
  specbind (intern_c_string ("print-circle"), Qt);
  Lisp_Object str = Fprin1_to_string (obj, Qnil);
  unbind_to (count, Qnil);

  ptrdiff_t len = SBYTES (str);
  const char *p = SSDATA (str);

  gcc_jit_type *a_type =
    gcc_jit_context_new_array_type (comp.ctxt,
				    NULL,
				    comp.char_type,
				    len + 1);
  gcc_jit_field *fields[] =
    { gcc_jit_context_new_field (comp.ctxt,
				 NULL,
				 comp.ptrdiff_type,
				 "len"),
      gcc_jit_context_new_field (comp.ctxt,
				 NULL,
				 a_type,
				 "data") };

  gcc_jit_type *data_struct_t =
    gcc_jit_struct_as_type (
      gcc_jit_context_new_struct_type (comp.ctxt,
				       NULL,
				       format_string ("%s_struct", name),
				       2, fields));

  gcc_jit_lvalue *data_struct =
    gcc_jit_context_new_global (comp.ctxt,
				NULL,
				GCC_JIT_GLOBAL_INTERNAL,
				data_struct_t,
				format_string ("%s_s", name));

  gcc_jit_function *f =
    gcc_jit_context_new_function (comp.ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  gcc_jit_type_get_pointer (data_struct_t),
				  name,
				  0, NULL, 0);
  DECL_BLOCK (block, f);

  if (COMP_DEBUG > 1)
    {
      char *comment = memcpy (xmalloc (len), p, len);
      for (ptrdiff_t i = 0; i < len - 1; i++)
	if (!comment[i])
	  comment[i] = '\n';
      gcc_jit_block_add_comment (block, NULL, comment);
      xfree (comment);
    }

  gcc_jit_lvalue *arr =
      gcc_jit_lvalue_access_field (data_struct, NULL, fields[1]);

  gcc_jit_lvalue *ptrvar = gcc_jit_function_new_local (f, NULL,
                                                       comp.char_ptr_type,
                                                       "ptr");

  gcc_jit_block_add_assignment (
    block,
    NULL,
    ptrvar,
    gcc_jit_lvalue_get_address (
      gcc_jit_context_new_array_access (
        comp.ctxt,
        NULL,
        gcc_jit_lvalue_as_rvalue (arr),
        gcc_jit_context_new_rvalue_from_int (comp.ctxt, comp.int_type, 0)),
      NULL));

  /* We can't use always string literals longer that 200 bytes because
     they cause a crash in pre GCC 10 libgccjit.
     <https://gcc.gnu.org/ml/jit/2019-q3/msg00013.html>.

     Adjust if possible to reduce the number of function calls.  */
  size_t chunck_size = NILP (Fcomp_libgccjit_version ()) ? 200 : 1024;
  char *buff = xmalloc (chunck_size);
  for (ptrdiff_t i = 0; i < len;)
    {
      strncpy (buff, p, chunck_size);
      buff[chunck_size - 1] = 0;
      uintptr_t l = strlen (buff);

      if (l != 0)
        {
          p += l;
          i += l;

          gcc_jit_rvalue *args[] =
	    { gcc_jit_lvalue_as_rvalue (ptrvar),
	      gcc_jit_context_new_string_literal (comp.ctxt, buff),
	      gcc_jit_context_new_rvalue_from_int (comp.ctxt,
						   comp.size_t_type,
						   l) };

          gcc_jit_block_add_eval (block, NULL,
                                  gcc_jit_context_new_call (comp.ctxt, NULL,
                                                            comp.memcpy,
                                                            ARRAYELTS (args),
							    args));
          gcc_jit_block_add_assignment (block, NULL, ptrvar,
            gcc_jit_lvalue_get_address (
              gcc_jit_context_new_array_access (comp.ctxt, NULL,
                gcc_jit_lvalue_as_rvalue (ptrvar),
                gcc_jit_context_new_rvalue_from_int (comp.ctxt,
                                                     comp.uintptr_type,
                                                     l)),
              NULL));
        }
      else
        {
          /* If strlen returned 0 that means that the static object
             contains a NULL byte.  In that case just move over to the
             next block.  We can rely on the byte being zero because
             of the previous call to bzero and because the dynamic
             linker cleared it.  */
          p++;
          i++;
          gcc_jit_block_add_assignment (
            block, NULL, ptrvar,
            gcc_jit_lvalue_get_address (
              gcc_jit_context_new_array_access (
                comp.ctxt, NULL, gcc_jit_lvalue_as_rvalue (ptrvar),
                gcc_jit_context_new_rvalue_from_int (comp.ctxt,
                                                     comp.uintptr_type, 1)),
              NULL));
        }
    }
  xfree (buff);

  gcc_jit_block_add_assignment (
	block,
	NULL,
	gcc_jit_lvalue_access_field (data_struct, NULL, fields[0]),
	gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					     comp.ptrdiff_type,
					     len));
  gcc_jit_rvalue *res = gcc_jit_lvalue_get_address (data_struct, NULL);
  gcc_jit_block_end_with_return (block, NULL, res);
}

static gcc_jit_rvalue *
declare_imported_data_relocs (Lisp_Object container, const char *code_symbol,
			      const char *text_symbol)
{
  /* Imported objects.  */
  EMACS_INT d_reloc_len =
    XFIXNUM (CALL1I (hash-table-count,
		     CALL1I (comp-data-container-idx, container)));
  Lisp_Object d_reloc = CALL1I (comp-data-container-l, container);
  d_reloc = Fvconcat (1, &d_reloc);

  gcc_jit_rvalue *reloc_struct =
    gcc_jit_lvalue_as_rvalue (
      gcc_jit_context_new_global (
	comp.ctxt,
	NULL,
	GCC_JIT_GLOBAL_EXPORTED,
	gcc_jit_context_new_array_type (comp.ctxt,
					NULL,
					comp.lisp_obj_type,
					d_reloc_len),
	code_symbol));

  emit_static_object (text_symbol, d_reloc);

  return reloc_struct;
}

static void
declare_imported_data (void)
{
  /* Imported objects.  */
  comp.data_relocs =
    declare_imported_data_relocs (CALL1I (comp-ctxt-d-default, Vcomp_ctxt),
				  DATA_RELOC_SYM,
				  TEXT_DATA_RELOC_SYM);
  comp.data_relocs_impure =
    declare_imported_data_relocs (CALL1I (comp-ctxt-d-impure, Vcomp_ctxt),
				  DATA_RELOC_IMPURE_SYM,
				  TEXT_DATA_RELOC_IMPURE_SYM);
  comp.data_relocs_ephemeral =
    declare_imported_data_relocs (CALL1I (comp-ctxt-d-ephemeral, Vcomp_ctxt),
				  DATA_RELOC_EPHEMERAL_SYM,
				  TEXT_DATA_RELOC_EPHEMERAL_SYM);
}

/*
  Declare as imported all the functions that are requested from the runtime.
  These are either subrs or not.
*/
static Lisp_Object
declare_runtime_imported_funcs (void)
{
  Lisp_Object field_list = Qnil;

#define ADD_IMPORTED(f_name, ret_type, nargs, args)			       \
  {									       \
    Lisp_Object name = intern_c_string (STR (f_name));			       \
    Lisp_Object field =							       \
      make_mint_ptr (declare_imported_func (name, ret_type, nargs, args));     \
    Lisp_Object el = Fcons (name, field);				       \
    field_list = Fcons (el, field_list);				       \
  } while (0)

  gcc_jit_type *args[4];

  ADD_IMPORTED (wrong_type_argument, comp.void_type, 2, NULL);

  args[0] = comp.lisp_obj_type;
  args[1] = comp.int_type;
  ADD_IMPORTED (helper_PSEUDOVECTOR_TYPEP_XUNTAG, comp.bool_type, 2, args);

  ADD_IMPORTED (pure_write_error, comp.void_type, 1, NULL);

  args[0] = comp.lisp_obj_type;
  args[1] = comp.int_type;
  ADD_IMPORTED (push_handler, comp.handler_ptr_type, 2, args);

#ifndef WINDOWSNT
  args[0] = gcc_jit_type_get_pointer (gcc_jit_struct_as_type (comp.jmp_buf_s));
  ADD_IMPORTED (SETJMP_NAME, comp.int_type, 1, args);
#else
  args[0] = gcc_jit_type_get_pointer (gcc_jit_struct_as_type (comp.jmp_buf_s));
  args[1] = comp.void_ptr_type;
  ADD_IMPORTED (SETJMP_NAME, comp.int_type, 2, args);
#endif

  ADD_IMPORTED (record_unwind_protect_excursion, comp.void_type, 0, NULL);

  args[0] = comp.lisp_obj_type;
  ADD_IMPORTED (helper_unbind_n, comp.lisp_obj_type, 1, args);

  ADD_IMPORTED (helper_save_restriction, comp.void_type, 0, NULL);

  ADD_IMPORTED (record_unwind_current_buffer, comp.void_type, 0, NULL);

  args[0] = args[1] = args[2] = comp.lisp_obj_type;
  args[3] = comp.int_type;
  ADD_IMPORTED (set_internal, comp.void_type, 4, args);

  args[0] = comp.lisp_obj_type;
  ADD_IMPORTED (helper_unwind_protect, comp.void_type, 1, args);

  args[0] = args[1] = comp.lisp_obj_type;
  ADD_IMPORTED (specbind, comp.void_type, 2, args);

  ADD_IMPORTED (maybe_gc, comp.void_type, 0, NULL);

  ADD_IMPORTED (maybe_quit, comp.void_type, 0, NULL);

#undef ADD_IMPORTED

  return Freverse (field_list);
}

/*
  This emit the code needed by every compilation unit to be loaded.
*/
static void
emit_ctxt_code (void)
{
  /* Emit optimize qualities.  */
  Lisp_Object opt_qly[] =
    { Fcons (Qcomp_speed,
	     Fsymbol_value (Qcomp_speed)),
      Fcons (Qcomp_debug,
	     Fsymbol_value (Qcomp_debug)),
      Fcons (Qgccjit,
	     Fcomp_libgccjit_version ()) };
  emit_static_object (TEXT_OPTIM_QLY_SYM, Flist (ARRAYELTS (opt_qly), opt_qly));

  emit_static_object (TEXT_FDOC_SYM,
		      CALL1I (comp-ctxt-function-docs, Vcomp_ctxt));

  comp.current_thread_ref =
    gcc_jit_lvalue_as_rvalue (
      gcc_jit_context_new_global (
	comp.ctxt,
	NULL,
	GCC_JIT_GLOBAL_EXPORTED,
	gcc_jit_type_get_pointer (comp.thread_state_ptr_type),
	CURRENT_THREAD_RELOC_SYM));

  comp.pure_ptr =
    gcc_jit_lvalue_as_rvalue (
      gcc_jit_context_new_global (
	comp.ctxt,
	NULL,
	GCC_JIT_GLOBAL_EXPORTED,
        comp.void_ptr_type,
	PURE_RELOC_SYM));

  gcc_jit_context_new_global (
	comp.ctxt,
	NULL,
	GCC_JIT_GLOBAL_EXPORTED,
	gcc_jit_type_get_pointer (comp.lisp_obj_ptr_type),
	COMP_UNIT_SYM);

  declare_imported_data ();

  /* Functions imported from Lisp code.	 */
  freloc_check_fill ();
  gcc_jit_field **fields = xmalloc (freloc.size * sizeof (*fields));
  ptrdiff_t n_frelocs = 0;
  Lisp_Object f_runtime = declare_runtime_imported_funcs ();
  FOR_EACH_TAIL (f_runtime)
    {
      Lisp_Object el = XCAR (f_runtime);
      eassert (n_frelocs < freloc.size);
      fields[n_frelocs++] = xmint_pointer (XCDR (el));
    }

  /* Sign the .eln for the exposed ABI it expects at load.  */
  eassert (!NILP (Vcomp_abi_hash));
  emit_static_object (LINK_TABLE_HASH_SYM, Vcomp_abi_hash);

  Lisp_Object subr_l = Vcomp_subr_list;
  FOR_EACH_TAIL (subr_l)
    {
      struct Lisp_Subr *subr = XSUBR (XCAR (subr_l));
      Lisp_Object subr_sym = intern_c_string (subr->symbol_name);
      eassert (n_frelocs < freloc.size);
      fields[n_frelocs++] = declare_imported_func (subr_sym, comp.lisp_obj_type,
						   subr->max_args, NULL);
    }

  gcc_jit_struct *f_reloc_struct =
    gcc_jit_context_new_struct_type (comp.ctxt,
				     NULL,
				     "freloc_link_table",
				     n_frelocs, fields);
  comp.func_relocs =
    gcc_jit_context_new_global (
      comp.ctxt,
      NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      gcc_jit_type_get_pointer (gcc_jit_struct_as_type (f_reloc_struct)),
      FUNC_LINK_TABLE_SYM);

  xfree (fields);
}


/****************************************************************/
/* Inline function definition and lisp data structure follows.  */
/****************************************************************/

/* struct Lisp_Cons definition.  */

static void
define_lisp_cons (void)
{
  /*
    union cdr_u
    {
      Lisp_Object cdr;
      struct Lisp_Cons *chain;
    };

    struct cons_s
    {
      Lisp_Object car;
      union cdr_u u;
    };

    union cons_u
    {
      struct cons_s s;
      char align_pad[sizeof (struct Lisp_Cons)];
    };

    struct Lisp_Cons
    {
      union cons_u u;
    };
  */

  comp.lisp_cons_s =
    gcc_jit_context_new_opaque_struct (comp.ctxt,
				       NULL,
				       "comp_Lisp_Cons");
  comp.lisp_cons_type =
    gcc_jit_struct_as_type (comp.lisp_cons_s);
  comp.lisp_cons_ptr_type =
    gcc_jit_type_get_pointer (comp.lisp_cons_type);

  comp.lisp_cons_u_s_u_cdr =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       comp.lisp_obj_type,
			       "cdr");

  gcc_jit_field *cdr_u_fields[] =
    { comp.lisp_cons_u_s_u_cdr,
      gcc_jit_context_new_field (comp.ctxt,
				 NULL,
				 comp.lisp_cons_ptr_type,
				 "chain") };

  gcc_jit_type *cdr_u =
    gcc_jit_context_new_union_type (comp.ctxt,
				    NULL,
				    "comp_cdr_u",
				    ARRAYELTS (cdr_u_fields),
				    cdr_u_fields);

  comp.lisp_cons_u_s_car = gcc_jit_context_new_field (comp.ctxt,
					    NULL,
					    comp.lisp_obj_type,
					    "car");
  comp.lisp_cons_u_s_u = gcc_jit_context_new_field (comp.ctxt,
						    NULL,
						    cdr_u,
						    "u");
  gcc_jit_field *cons_s_fields[] =
    { comp.lisp_cons_u_s_car,
      comp.lisp_cons_u_s_u };

  gcc_jit_struct *cons_s =
    gcc_jit_context_new_struct_type (comp.ctxt,
				     NULL,
				     "comp_cons_s",
				     ARRAYELTS (cons_s_fields),
				     cons_s_fields);

  comp.lisp_cons_u_s = gcc_jit_context_new_field (comp.ctxt,
				 NULL,
				 gcc_jit_struct_as_type (cons_s),
				 "s");

  gcc_jit_field *cons_u_fields[] =
    { comp.lisp_cons_u_s,
      gcc_jit_context_new_field (
	comp.ctxt,
	NULL,
	gcc_jit_context_new_array_type (comp.ctxt,
					NULL,
					comp.char_type,
					sizeof (struct Lisp_Cons)),
	"align_pad") };

  gcc_jit_type *lisp_cons_u_type =
    gcc_jit_context_new_union_type (comp.ctxt,
				    NULL,
				    "comp_cons_u",
				    ARRAYELTS (cons_u_fields),
				    cons_u_fields);

  comp.lisp_cons_u =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       lisp_cons_u_type,
			       "u");
  gcc_jit_struct_set_fields (comp.lisp_cons_s,
			     NULL, 1, &comp.lisp_cons_u);

}

/* Opaque jmp_buf definition.  */

static void
define_jmp_buf (void)
{
  gcc_jit_field *field =
    gcc_jit_context_new_field (
      comp.ctxt,
      NULL,
      gcc_jit_context_new_array_type (comp.ctxt,
				      NULL,
				      comp.char_type,
				      sizeof (sys_jmp_buf)),
      "stuff");
  comp.jmp_buf_s =
    gcc_jit_context_new_struct_type (comp.ctxt,
				     NULL,
				     "comp_jmp_buf",
				     1, &field);
}

static void
define_memcpy (void)
{

  gcc_jit_param *params[] =
    { gcc_jit_context_new_param (comp.ctxt, NULL, comp.void_ptr_type, "dest"),
      gcc_jit_context_new_param (comp.ctxt, NULL, comp.void_ptr_type, "src"),
      gcc_jit_context_new_param (comp.ctxt, NULL, comp.size_t_type, "n") };

  comp.memcpy =
    gcc_jit_context_new_function (comp.ctxt, NULL, GCC_JIT_FUNCTION_IMPORTED,
				  comp.void_ptr_type, "memcpy",
				  ARRAYELTS (params), params, false);
}

/* struct handler definition  */

static void
define_handler_struct (void)
{
  comp.handler_s =
    gcc_jit_context_new_opaque_struct (comp.ctxt, NULL, "comp_handler");
  comp.handler_ptr_type =
    gcc_jit_type_get_pointer (gcc_jit_struct_as_type (comp.handler_s));

  comp.handler_jmp_field = gcc_jit_context_new_field (comp.ctxt,
						      NULL,
						      gcc_jit_struct_as_type (
							comp.jmp_buf_s),
						      "jmp");
  comp.handler_val_field = gcc_jit_context_new_field (comp.ctxt,
						      NULL,
						      comp.lisp_obj_type,
						      "val");
  comp.handler_next_field = gcc_jit_context_new_field (comp.ctxt,
						       NULL,
						       comp.handler_ptr_type,
						       "next");
  gcc_jit_field *fields[] =
    { gcc_jit_context_new_field (
	comp.ctxt,
	NULL,
	gcc_jit_context_new_array_type (comp.ctxt,
					NULL,
					comp.char_type,
					offsetof (struct handler, val)),
	"pad0"),
      comp.handler_val_field,
      comp.handler_next_field,
      gcc_jit_context_new_field (
	comp.ctxt,
	NULL,
	gcc_jit_context_new_array_type (comp.ctxt,
					NULL,
					comp.char_type,
					offsetof (struct handler, jmp)
					- offsetof (struct handler, next)
					- sizeof (((struct handler *) 0)->next)),
	"pad1"),
      comp.handler_jmp_field,
      gcc_jit_context_new_field (
	comp.ctxt,
	NULL,
	gcc_jit_context_new_array_type (comp.ctxt,
					NULL,
					comp.char_type,
					sizeof (struct handler)
					- offsetof (struct handler, jmp)
					- sizeof (((struct handler *) 0)->jmp)),
	"pad2") };
  gcc_jit_struct_set_fields (comp.handler_s,
			     NULL,
			     ARRAYELTS (fields),
			     fields);

}

static void
define_thread_state_struct (void)
{
  /* Partially opaque definition for `thread_state'.
     Because we need to access just m_handlerlist hopefully this is requires
     less manutention then the full deifnition.	 */

  comp.m_handlerlist = gcc_jit_context_new_field (comp.ctxt,
						  NULL,
						  comp.handler_ptr_type,
						  "m_handlerlist");
  gcc_jit_field *fields[] =
    { gcc_jit_context_new_field (
	comp.ctxt,
	NULL,
	gcc_jit_context_new_array_type (comp.ctxt,
					NULL,
					comp.char_type,
					offsetof (struct thread_state,
						  m_handlerlist)),
	"pad0"),
      comp.m_handlerlist,
      gcc_jit_context_new_field (
	comp.ctxt,
	NULL,
	gcc_jit_context_new_array_type (
	  comp.ctxt,
	  NULL,
	  comp.char_type,
	  sizeof (struct thread_state)
	  - offsetof (struct thread_state,
		      m_handlerlist)
	  - sizeof (((struct thread_state *) 0)->m_handlerlist)),
	"pad1") };

  comp.thread_state_s =
    gcc_jit_context_new_struct_type (comp.ctxt,
				     NULL,
				     "comp_thread_state",
				     ARRAYELTS (fields),
				     fields);
  comp.thread_state_ptr_type =
    gcc_jit_type_get_pointer (gcc_jit_struct_as_type (comp.thread_state_s));
}

struct cast_type
{
  gcc_jit_type *type;
  const char *name;
  size_t bytes_size;
  enum cast_kind_of_type kind;
};

static gcc_jit_function *
define_cast_from_to (struct cast_type from, int from_index, struct cast_type to,
                    int to_index)
{
  /*  FIXME: sign extension not implemented.  */
  if (comp.cast_type_sizes[from_index] < comp.cast_type_sizes[to_index]
      && comp.cast_type_kind[to_index] == kind_signed)
    return NULL;

  char *name = format_string ("cast_from_%s_to_%s", from.name, to.name);
  gcc_jit_param *param = gcc_jit_context_new_param (comp.ctxt, NULL,
                                                    from.type, "arg");
  gcc_jit_function *result = gcc_jit_context_new_function (comp.ctxt,
                               NULL,
                               GCC_JIT_FUNCTION_INTERNAL,
                               to.type,
                               name,
                               1,
                               &param,
                               0);

  DECL_BLOCK (entry_block, result);

  gcc_jit_lvalue *tmp_union
    = gcc_jit_function_new_local (result,
                                  NULL,
                                  comp.cast_union_type,
                                  "union_cast");

  /*  Zero the union first.  */
  gcc_jit_block_add_assignment (entry_block, NULL,
                                gcc_jit_lvalue_access_field (tmp_union, NULL,
                                  comp.cast_union_fields[NUM_CAST_TYPES]),
                                  gcc_jit_context_new_rvalue_from_int (
				    comp.ctxt,
				    comp.cast_types[NUM_CAST_TYPES],
                                    0));

  gcc_jit_block_add_assignment (entry_block, NULL,
                                gcc_jit_lvalue_access_field (tmp_union, NULL,
                                  comp.cast_union_fields[from_index]),
                                gcc_jit_param_as_rvalue (param));

  gcc_jit_block_end_with_return (entry_block,
                                 NULL,
                                 gcc_jit_rvalue_access_field (
                                   gcc_jit_lvalue_as_rvalue (tmp_union),
                                   NULL,
                                   comp.cast_union_fields[to_index]));

  return result;
}

static void
define_cast_functions (void)
{
  struct cast_type cast_types[NUM_CAST_TYPES]
    = { { comp.bool_type, "bool", sizeof (bool), kind_unsigned },
        { comp.char_ptr_type, "char_ptr", sizeof (char *), kind_pointer },
        { comp.int_type, "int", sizeof (int), kind_signed },
        { comp.lisp_cons_ptr_type, "cons_ptr", sizeof (struct Lisp_Cons *),
          kind_pointer },
        { comp.lisp_obj_ptr_type, "lisp_obj_ptr", sizeof (Lisp_Object *),
          kind_pointer },
        { comp.lisp_word_tag_type, "lisp_word_tag", sizeof (Lisp_Word_tag),
          kind_unsigned },
        { comp.lisp_word_type, "lisp_word", sizeof (Lisp_Word),
          LISP_WORDS_ARE_POINTERS ? kind_pointer : kind_signed },
        { comp.long_long_type, "long_long", sizeof (long long), kind_signed },
        { comp.long_type, "long", sizeof (long), kind_signed },
        { comp.ptrdiff_type, "ptrdiff", sizeof (ptrdiff_t), kind_signed },
        { comp.uintptr_type, "uintptr", sizeof (uintptr_t), kind_unsigned },
        { comp.unsigned_long_long_type, "unsigned_long_long",
          sizeof (unsigned long long), kind_unsigned },
        { comp.unsigned_long_type, "unsigned_long", sizeof (unsigned long),
          kind_unsigned },
        { comp.unsigned_type, "unsigned", sizeof (unsigned), kind_unsigned },
        { comp.void_ptr_type, "void_ptr", sizeof (void*), kind_pointer } };

  /* Find the biggest size.  It should be unsigned long long, but to be
     sure we find it programmatically.  */
  size_t biggest_size = 0;
  for (int i = 0; i < NUM_CAST_TYPES; ++i)
    biggest_size = max (biggest_size, cast_types[i].bytes_size);

  /* Define the union used for casting.  */
  for (int i = 0; i < NUM_CAST_TYPES; ++i)
    {
      comp.cast_types[i] = cast_types[i].type;
      comp.cast_union_fields[i] = gcc_jit_context_new_field (comp.ctxt,
                                    NULL,
                                    cast_types[i].type,
                                    cast_types[i].name);
      comp.cast_type_names[i] = cast_types[i].name;
      comp.cast_type_sizes[i] = cast_types[i].bytes_size;
      comp.cast_type_kind[i] = cast_types[i].kind;
    }

  gcc_jit_type *biggest_type = gcc_jit_context_get_int_type (comp.ctxt,
                                                             biggest_size,
                                                             false);
  comp.cast_types[NUM_CAST_TYPES] = biggest_type;
  comp.cast_union_fields[NUM_CAST_TYPES] =
    gcc_jit_context_new_field (comp.ctxt, NULL, biggest_type, "biggest_type");
  comp.cast_type_names[NUM_CAST_TYPES] = "biggest_type";
  comp.cast_type_sizes[NUM_CAST_TYPES] = biggest_size;
  comp.cast_type_kind[NUM_CAST_TYPES] = kind_unsigned;

  comp.cast_union_type =
    gcc_jit_context_new_union_type (comp.ctxt,
				    NULL,
				    "cast_union",
				    NUM_CAST_TYPES + 1,
				    comp.cast_union_fields);

  /* Define the cast functions using a matrix.  */
  for (int i = 0; i < NUM_CAST_TYPES; ++i)
    for (int j = 0; j < NUM_CAST_TYPES; ++j)
        comp.cast_functions_from_to[i][j] =
          define_cast_from_to (cast_types[i], i, cast_types[j], j);
}

static void
define_CHECK_TYPE (void)
{
  gcc_jit_param *param[] =
    { gcc_jit_context_new_param (comp.ctxt,
				 NULL,
				 comp.int_type,
				 "ok"),
      gcc_jit_context_new_param (comp.ctxt,
				 NULL,
				 comp.lisp_obj_type,
				 "predicate"),
      gcc_jit_context_new_param (comp.ctxt,
				 NULL,
				 comp.lisp_obj_type,
				 "x") };
  comp.check_type =
    gcc_jit_context_new_function (comp.ctxt, NULL,
				  GCC_JIT_FUNCTION_INTERNAL,
				  comp.void_type,
				  "CHECK_TYPE",
				  3,
				  param,
				  0);
  gcc_jit_rvalue *ok = gcc_jit_param_as_rvalue (param[0]);
  gcc_jit_rvalue *predicate = gcc_jit_param_as_rvalue (param[1]);
  gcc_jit_rvalue *x = gcc_jit_param_as_rvalue (param[2]);

  DECL_BLOCK (entry_block, comp.check_type);
  DECL_BLOCK (ok_block, comp.check_type);
  DECL_BLOCK (not_ok_block, comp.check_type);

  comp.block = entry_block;
  comp.func = comp.check_type;

  emit_cond_jump (ok, ok_block, not_ok_block);

  gcc_jit_block_end_with_void_return (ok_block, NULL);

  comp.block = not_ok_block;

  gcc_jit_rvalue *wrong_type_args[] = { predicate, x };

  gcc_jit_block_add_eval (comp.block,
			  NULL,
			  emit_call (intern_c_string ("wrong_type_argument"),
				     comp.void_type, 2, wrong_type_args,
				     false));

  gcc_jit_block_end_with_void_return (not_ok_block, NULL);
}

/* Define a substitute for CAR as always inlined function.  */

static void
define_CAR_CDR (void)
{
  gcc_jit_function *func[2];
  char const *f_name[] = { "CAR", "CDR" };
  for (int i = 0; i < 2; i++)
    {
      gcc_jit_param *param[] =
	{ gcc_jit_context_new_param (comp.ctxt,
				     NULL,
				     comp.lisp_obj_type,
				     "c"),
	  gcc_jit_context_new_param (comp.ctxt,
				     NULL,
				     comp.bool_type,
				     "cert_cons") };
      /* TODO: understand why after ipa-prop pass gcc is less keen on inlining
	 and as consequence can refuse to compile these. (see dhrystone.el)
	 Flag this and all the one involved in ipa-prop as
	 GCC_JIT_FUNCTION_INTERNAL not to fail compilation in case.
	 This seems at least to have no perf downside.  */
      func[i] =
	gcc_jit_context_new_function (comp.ctxt, NULL,
				      GCC_JIT_FUNCTION_INTERNAL,
				      comp.lisp_obj_type,
				      f_name[i],
				      2, param, 0);

      gcc_jit_rvalue *c = gcc_jit_param_as_rvalue (param[0]);
      DECL_BLOCK (entry_block, func[i]);
      DECL_BLOCK (is_cons_b, func[i]);
      DECL_BLOCK (not_a_cons_b, func[i]);
      comp.block = entry_block;
      comp.func = func[i];
      emit_cond_jump (emit_binary_op (GCC_JIT_BINARY_OP_LOGICAL_OR,
				      comp.bool_type,
				      gcc_jit_param_as_rvalue (param[1]),
				      emit_CONSP (c)),
		      is_cons_b,
		      not_a_cons_b);
      comp.block = is_cons_b;
      if (i == 0)
	gcc_jit_block_end_with_return (comp.block, NULL, emit_XCAR (c));
      else
	gcc_jit_block_end_with_return (comp.block, NULL, emit_XCDR (c));

      comp.block = not_a_cons_b;

      DECL_BLOCK (is_nil_b, func[i]);
      DECL_BLOCK (not_nil_b, func[i]);

      emit_cond_jump (emit_NILP (c), is_nil_b, not_nil_b);

      comp.block = is_nil_b;
      gcc_jit_block_end_with_return (comp.block,
				     NULL,
				     emit_lisp_obj_rval (Qnil));

      comp.block = not_nil_b;
      gcc_jit_rvalue *wrong_type_args[] =
	{ emit_lisp_obj_rval (Qlistp), c };

      gcc_jit_block_add_eval (comp.block,
			      NULL,
			      emit_call (intern_c_string ("wrong_type_argument"),
					 comp.void_type, 2, wrong_type_args,
					 false));
      gcc_jit_block_end_with_return (comp.block,
				     NULL,
				     emit_lisp_obj_rval (Qnil));
    }
  comp.car = func[0];
  comp.cdr = func[1];
}

static void
define_setcar_setcdr (void)
{
  char const *f_name[] = { "setcar", "setcdr" };
  char const *par_name[] = { "new_car", "new_cdr" };

  for (int i = 0; i < 2; i++)
    {
      gcc_jit_param *cell =
	gcc_jit_context_new_param (comp.ctxt,
				   NULL,
				   comp.lisp_obj_type,
				   "cell");
      gcc_jit_param *new_el =
	gcc_jit_context_new_param (comp.ctxt,
				   NULL,
				   comp.lisp_obj_type,
				   par_name[i]);

      gcc_jit_param *param[] =
	{ cell,
	  new_el,
	  gcc_jit_context_new_param (comp.ctxt,
				     NULL,
				     comp.bool_type,
				     "cert_cons") };

      gcc_jit_function **f_ref = !i ? &comp.setcar : &comp.setcdr;
      *f_ref = gcc_jit_context_new_function (comp.ctxt, NULL,
					     GCC_JIT_FUNCTION_INTERNAL,
					     comp.lisp_obj_type,
					     f_name[i],
					     3, param, 0);
      DECL_BLOCK (entry_block, *f_ref);
      comp.func = *f_ref;
      comp.block = entry_block;

      /* CHECK_CONS (cell);  */
      emit_CHECK_CONS (gcc_jit_param_as_rvalue (cell));

      /* CHECK_IMPURE (cell, XCONS (cell));  */
      gcc_jit_rvalue *args[] =
	{ gcc_jit_param_as_rvalue (cell),
	  emit_XCONS (gcc_jit_param_as_rvalue (cell)) };

      gcc_jit_block_add_eval (entry_block,
			      NULL,
			      gcc_jit_context_new_call (comp.ctxt,
							NULL,
							comp.check_impure,
							2,
							args));

      /* XSETCDR (cell, newel);  */
      if (!i)
	emit_XSETCAR (gcc_jit_param_as_rvalue (cell),
		      gcc_jit_param_as_rvalue (new_el));
      else
	emit_XSETCDR (gcc_jit_param_as_rvalue (cell),
		      gcc_jit_param_as_rvalue (new_el));

      /* return newel;  */
      gcc_jit_block_end_with_return (entry_block,
				     NULL,
				     gcc_jit_param_as_rvalue (new_el));
    }
}

/*
   Define a substitute for Fadd1 Fsub1.
   Currently expose just fixnum arithmetic.
*/

static void
define_add1_sub1 (void)
{
  gcc_jit_block *bb_orig = comp.block;
  gcc_jit_function *func[2];
  char const *f_name[] = { "add1", "sub1" };
  char const *fall_back_func[] = { "1+", "1-" };
  enum gcc_jit_binary_op op[] =
    { GCC_JIT_BINARY_OP_PLUS, GCC_JIT_BINARY_OP_MINUS };
  for (ptrdiff_t i = 0; i < 2; i++)
    {
      gcc_jit_param *param[] =
	{ gcc_jit_context_new_param (comp.ctxt,
				     NULL,
				     comp.lisp_obj_type,
				     "n"),
	  gcc_jit_context_new_param (comp.ctxt,
				     NULL,
				     comp.bool_type,
				     "cert_fixnum") };
      comp.func = func[i] =
	gcc_jit_context_new_function (comp.ctxt, NULL,
				      GCC_JIT_FUNCTION_INTERNAL,
				      comp.lisp_obj_type,
				      f_name[i],
				      2,
				      param, 0);
      DECL_BLOCK (entry_block, func[i]);
      DECL_BLOCK (inline_block, func[i]);
      DECL_BLOCK (fcall_block, func[i]);

      comp.block = entry_block;

      /* cert_fixnum ||
	 ((FIXNUMP (n) && XFIXNUM (n) != MOST_POSITIVE_FIXNUM
	 ? (XFIXNUM (n) + 1)
	 : Fadd1 (n)) */

      gcc_jit_rvalue *n = gcc_jit_param_as_rvalue (param[0]);
      gcc_jit_rvalue *n_fixnum = emit_XFIXNUM (n);
      gcc_jit_rvalue *sure_fixnum =
	emit_binary_op (GCC_JIT_BINARY_OP_LOGICAL_OR,
			comp.bool_type,
			gcc_jit_param_as_rvalue (param[1]),
			emit_FIXNUMP (n));
      emit_cond_jump (
	emit_binary_op (
	  GCC_JIT_BINARY_OP_LOGICAL_AND,
	  comp.bool_type,
	  sure_fixnum,
	  gcc_jit_context_new_comparison (
	    comp.ctxt,
	    NULL,
	    GCC_JIT_COMPARISON_NE,
	    n_fixnum,
	    i == 0
	    ? emit_rvalue_from_emacs_int (MOST_POSITIVE_FIXNUM)
	    : emit_rvalue_from_emacs_int (MOST_NEGATIVE_FIXNUM))),
	inline_block,
	fcall_block);

      comp.block = inline_block;
      gcc_jit_rvalue *inline_res =
	emit_binary_op (op[i], comp.emacs_int_type, n_fixnum, comp.one);

      gcc_jit_block_end_with_return (inline_block,
				     NULL,
				     emit_make_fixnum (inline_res));

      comp.block = fcall_block;
      gcc_jit_rvalue *call_res = emit_call (intern_c_string (fall_back_func[i]),
					    comp.lisp_obj_type, 1, &n, false);
      gcc_jit_block_end_with_return (fcall_block,
				     NULL,
				     call_res);
    }
  comp.block = bb_orig;
  comp.add1 = func[0];
  comp.sub1 = func[1];
}

static void
define_negate (void)
{
  gcc_jit_block *bb_orig = comp.block;
  gcc_jit_param *param[] =
	{ gcc_jit_context_new_param (comp.ctxt,
				     NULL,
				     comp.lisp_obj_type,
				     "n"),
	  gcc_jit_context_new_param (comp.ctxt,
				     NULL,
				     comp.bool_type,
				     "cert_fixnum") };

  comp.func = comp.negate =
    gcc_jit_context_new_function (comp.ctxt, NULL,
				  GCC_JIT_FUNCTION_INTERNAL,
				  comp.lisp_obj_type,
				  "negate",
				  2, param, 0);

  DECL_BLOCK (entry_block, comp.negate);
  DECL_BLOCK (inline_block, comp.negate);
  DECL_BLOCK (fcall_block, comp.negate);

  comp.block = entry_block;

  /* (cert_fixnum || FIXNUMP (TOP)) && XFIXNUM (TOP) != MOST_NEGATIVE_FIXNUM
     ? make_fixnum (- XFIXNUM (TOP)) : Fminus (1, &TOP))  */

  gcc_jit_lvalue *n = gcc_jit_param_as_lvalue (param[0]);
  gcc_jit_rvalue *n_fixnum = emit_XFIXNUM (gcc_jit_lvalue_as_rvalue (n));
  gcc_jit_rvalue *sure_fixnum =
    emit_binary_op (GCC_JIT_BINARY_OP_LOGICAL_OR,
		    comp.bool_type,
		    gcc_jit_param_as_rvalue (param[1]),
		    emit_FIXNUMP (gcc_jit_lvalue_as_rvalue (n)));

  emit_cond_jump (emit_binary_op (GCC_JIT_BINARY_OP_LOGICAL_AND,
				  comp.bool_type,
				  sure_fixnum,
				  gcc_jit_context_new_comparison (
				    comp.ctxt,
				    NULL,
				    GCC_JIT_COMPARISON_NE,
				    n_fixnum,
				    emit_rvalue_from_emacs_int (
                                      MOST_NEGATIVE_FIXNUM))),
		  inline_block,
		  fcall_block);

  comp.block = inline_block;
  gcc_jit_rvalue *inline_res =
    gcc_jit_context_new_unary_op (comp.ctxt,
				  NULL,
				  GCC_JIT_UNARY_OP_MINUS,
				  comp.emacs_int_type,
				  n_fixnum);

  gcc_jit_block_end_with_return (inline_block,
				 NULL,
				 emit_make_fixnum (inline_res));

  comp.block = fcall_block;
  gcc_jit_rvalue *call_res = emit_call_ref (Qminus, 1, n, false);
  gcc_jit_block_end_with_return (fcall_block,
				 NULL,
				 call_res);
  comp.block = bb_orig;
}

/* Define a substitute for PSEUDOVECTORP as always inlined function.  */

static void
define_PSEUDOVECTORP (void)
{
  gcc_jit_param *param[] =
    { gcc_jit_context_new_param (comp.ctxt,
				 NULL,
				 comp.lisp_obj_type,
				 "a"),
      gcc_jit_context_new_param (comp.ctxt,
				 NULL,
				 comp.int_type,
				 "code") };

  comp.pseudovectorp =
    gcc_jit_context_new_function (comp.ctxt, NULL,
				  GCC_JIT_FUNCTION_INTERNAL,
				  comp.bool_type,
				  "PSEUDOVECTORP",
				  2,
				  param,
				  0);

  DECL_BLOCK (entry_block, comp.pseudovectorp);
  DECL_BLOCK (ret_false_b, comp.pseudovectorp);
  DECL_BLOCK (call_pseudovector_typep_b, comp.pseudovectorp);

  comp.block = entry_block;
  comp.func = comp.pseudovectorp;

  emit_cond_jump (emit_VECTORLIKEP (gcc_jit_param_as_rvalue (param[0])),
		  call_pseudovector_typep_b,
		  ret_false_b);

  comp.block = ret_false_b;
  gcc_jit_block_end_with_return (ret_false_b,
				 NULL,
				 gcc_jit_context_new_rvalue_from_int (
				   comp.ctxt,
				   comp.bool_type,
				   false));

  gcc_jit_rvalue *args[] =
    { gcc_jit_param_as_rvalue (param[0]),
      gcc_jit_param_as_rvalue (param[1]) };
  comp.block = call_pseudovector_typep_b;
  /* FIXME use XUNTAG now that's available.  */
  gcc_jit_block_end_with_return (
    call_pseudovector_typep_b,
    NULL,
    emit_call (intern_c_string ("helper_PSEUDOVECTOR_TYPEP_XUNTAG"),
	       comp.bool_type, 2, args, false));
}

static void
define_CHECK_IMPURE (void)
{
  gcc_jit_param *param[] =
    { gcc_jit_context_new_param (comp.ctxt,
				 NULL,
				 comp.lisp_obj_type,
				 "obj"),
      gcc_jit_context_new_param (comp.ctxt,
				 NULL,
				 comp.void_ptr_type,
				 "ptr") };
  comp.check_impure =
    gcc_jit_context_new_function (comp.ctxt, NULL,
				  GCC_JIT_FUNCTION_INTERNAL,
				  comp.void_type,
				  "CHECK_IMPURE",
				  2,
				  param,
				  0);

    DECL_BLOCK (entry_block, comp.check_impure);
    DECL_BLOCK (err_block, comp.check_impure);
    DECL_BLOCK (ok_block, comp.check_impure);

    comp.block = entry_block;
    comp.func = comp.check_impure;

    emit_cond_jump (emit_PURE_P (gcc_jit_param_as_rvalue (param[0])), /* FIXME */
		    err_block,
		    ok_block);
    gcc_jit_block_end_with_void_return (ok_block, NULL);

    gcc_jit_rvalue *pure_write_error_arg =
      gcc_jit_param_as_rvalue (param[0]);

    comp.block = err_block;
    gcc_jit_block_add_eval (comp.block,
			    NULL,
			    emit_call (intern_c_string ("pure_write_error"),
				       comp.void_type, 1,&pure_write_error_arg,
				       false));

    gcc_jit_block_end_with_void_return (err_block, NULL);
}

static void
define_maybe_gc_or_quit (void)
{

  /*
    void
    maybe_gc_or_quit (void)
    {
      static unsigned quitcounter;
     inc:
      quitcounter++;
      if (quitcounter >> 14) goto maybe_do_it else goto pass;
     maybe_do_it:
          quitcounter = 0;
          maybe_gc ();
          maybe_quit ();
          return;
     pass:
          return;
    }
  */

  gcc_jit_block *bb_orig = comp.block;

  gcc_jit_lvalue *quitcounter =
    gcc_jit_context_new_global (
      comp.ctxt,
      NULL,
      GCC_JIT_GLOBAL_INTERNAL,
      comp.unsigned_type,
      "quitcounter");

  comp.func = comp.maybe_gc_or_quit =
    gcc_jit_context_new_function (comp.ctxt, NULL,
				  GCC_JIT_FUNCTION_INTERNAL,
				  comp.void_type,
				  "maybe_gc_quit",
				  0, NULL, 0);
  DECL_BLOCK (increment_block, comp.maybe_gc_or_quit);
  DECL_BLOCK (maybe_do_it_block, comp.maybe_gc_or_quit);
  DECL_BLOCK (pass_block, comp.maybe_gc_or_quit);

  comp.block = increment_block;

  gcc_jit_block_add_assignment (
    comp.block,
    NULL,
    quitcounter,
    emit_binary_op (GCC_JIT_BINARY_OP_PLUS,
		    comp.unsigned_type,
		    gcc_jit_lvalue_as_rvalue (quitcounter),
		    gcc_jit_context_new_rvalue_from_int (comp.ctxt,
							 comp.unsigned_type,
							 1)));
  emit_cond_jump (
    emit_binary_op (GCC_JIT_BINARY_OP_RSHIFT,
		    comp.unsigned_type,
		    gcc_jit_lvalue_as_rvalue (quitcounter),
		    gcc_jit_context_new_rvalue_from_int (comp.ctxt,
							 comp.unsigned_type,
							 9)),
    /* 9 translates into checking for GC or quit every 512 calls to
       'maybe_gc_quit'.  This is the smallest value I could find with
       no performance impact running elisp-banechmarks and the same
       used by the byte intepreter (see 'exec_byte_code').  */
    maybe_do_it_block,
    pass_block);

  comp.block = maybe_do_it_block;

  gcc_jit_block_add_assignment (
    comp.block,
    NULL,
    quitcounter,
    gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					 comp.unsigned_type,
					 0));
  gcc_jit_block_add_eval (comp.block, NULL,
			  emit_call (intern_c_string ("maybe_gc"),
				     comp.void_type, 0, NULL, false));
  gcc_jit_block_add_eval (comp.block, NULL,
			  emit_call (intern_c_string ("maybe_quit"),
				     comp.void_type, 0, NULL, false));
  gcc_jit_block_end_with_void_return (comp.block, NULL);

  gcc_jit_block_end_with_void_return (pass_block, NULL);

  comp.block = bb_orig;
}

/* Define a function to convert boolean into t or nil */

static void
define_bool_to_lisp_obj (void)
{
  /* x ? Qt : Qnil */
  gcc_jit_param *param = gcc_jit_context_new_param (comp.ctxt,
						    NULL,
						    comp.bool_type,
						    "x");
  comp.bool_to_lisp_obj =
    gcc_jit_context_new_function (comp.ctxt, NULL,
				  GCC_JIT_FUNCTION_INTERNAL,
				  comp.lisp_obj_type,
				  "bool_to_lisp_obj",
				  1,
				  &param,
				  0);
  DECL_BLOCK (entry_block, comp.bool_to_lisp_obj);
  DECL_BLOCK (ret_t_block, comp.bool_to_lisp_obj);
  DECL_BLOCK (ret_nil_block, comp.bool_to_lisp_obj);
  comp.block = entry_block;
  comp.func = comp.bool_to_lisp_obj;

  emit_cond_jump (gcc_jit_param_as_rvalue (param),
		  ret_t_block,
		  ret_nil_block);

  comp.block = ret_t_block;
  gcc_jit_block_end_with_return (ret_t_block,
				 NULL,
				 emit_lisp_obj_rval (Qt));

  comp.block = ret_nil_block;
  gcc_jit_block_end_with_return (ret_nil_block,
				 NULL,
				 emit_lisp_obj_rval (Qnil));
}

static gcc_jit_function *
declare_lex_function (Lisp_Object func)
{
  gcc_jit_function *res;
  char *c_name = SSDATA (CALL1I (comp-func-c-name, func));
  Lisp_Object args = CALL1I (comp-func-l-args, func);
  bool nargs = !NILP (CALL1I (comp-nargs-p, args));
  USE_SAFE_ALLOCA;

  if (!nargs)
    {
      EMACS_INT max_args = XFIXNUM (CALL1I (comp-args-max, args));
      gcc_jit_type **type = SAFE_ALLOCA (max_args * sizeof (*type));
      for (ptrdiff_t i = 0; i < max_args; i++)
	type[i] = comp.lisp_obj_type;

      gcc_jit_param **params = SAFE_ALLOCA (max_args * sizeof (*params));
      for (int i = 0; i < max_args; ++i)
	params[i] = gcc_jit_context_new_param (comp.ctxt,
					      NULL,
					      type[i],
					      format_string ("par_%d", i));
      res = gcc_jit_context_new_function (comp.ctxt, NULL,
					  GCC_JIT_FUNCTION_EXPORTED,
					  comp.lisp_obj_type,
					  c_name,
					  max_args,
					  params,
					  0);
    }
  else
    {
      gcc_jit_param *params[] =
	{ gcc_jit_context_new_param (comp.ctxt,
				     NULL,
				     comp.ptrdiff_type,
				     "nargs"),
	  gcc_jit_context_new_param (comp.ctxt,
				     NULL,
				     comp.lisp_obj_ptr_type,
				     "args") };
      res =
	gcc_jit_context_new_function (comp.ctxt,
				      NULL,
				      GCC_JIT_FUNCTION_EXPORTED,
				      comp.lisp_obj_type,
				      c_name, ARRAYELTS (params), params, 0);
    }
  SAFE_FREE ();
  return res;
}

/* Declare a function being compiled and add it to comp.exported_funcs_h.  */

static void
declare_function (Lisp_Object func)
{
  gcc_jit_function *gcc_func =
    !NILP (CALL1I (comp-func-l-p, func))
    ? declare_lex_function (func)
    : gcc_jit_context_new_function (comp.ctxt,
				    NULL,
				    GCC_JIT_FUNCTION_EXPORTED,
				    comp.lisp_obj_type,
				    SSDATA (CALL1I (comp-func-c-name, func)),
				    0, NULL, 0);
  Fputhash (CALL1I (comp-func-c-name, func),
	    make_mint_ptr (gcc_func),
	    comp.exported_funcs_h);
}

static void
compile_function (Lisp_Object func)
{
  USE_SAFE_ALLOCA;
  EMACS_INT frame_size = XFIXNUM (CALL1I (comp-func-frame-size, func));

  comp.func = xmint_pointer (Fgethash (CALL1I (comp-func-c-name, func),
				       comp.exported_funcs_h, Qnil));

  comp.func_has_non_local = !NILP (CALL1I (comp-func-has-non-local, func));
  comp.func_speed = XFIXNUM (CALL1I (comp-func-speed, func));

  comp.frame = SAFE_ALLOCA (frame_size * sizeof (*comp.frame));
  if (comp.func_has_non_local || !comp.func_speed)
    {
      /* FIXME: See bug#42360.  */
      gcc_jit_lvalue *arr =
        gcc_jit_function_new_local (
          comp.func,
          NULL,
          gcc_jit_context_new_array_type (comp.ctxt,
                                          NULL,
                                          comp.lisp_obj_type,
                                          frame_size),
          "frame");

      for (ptrdiff_t i = 0; i < frame_size; ++i)
	comp.frame[i] =
          gcc_jit_context_new_array_access (
            comp.ctxt,
            NULL,
            gcc_jit_lvalue_as_rvalue (arr),
            gcc_jit_context_new_rvalue_from_int (comp.ctxt,
                                                 comp.int_type,
                                                 i));
    }
  else
    for (ptrdiff_t i = 0; i < frame_size; ++i)
      comp.frame[i] =
	gcc_jit_function_new_local (comp.func,
				    NULL,
				    comp.lisp_obj_type,
				    format_string ("slot_%td", i));

  comp.scratch = NULL;

  comp.loc_handler =  gcc_jit_function_new_local (comp.func,
						  NULL,
						  comp.handler_ptr_type,
						  "c");

  comp.func_blocks_h = CALLN (Fmake_hash_table);

  /* Pre-declare all basic blocks to gcc.
     The "entry" block must be declared as first.  */
  declare_block (Qentry);
  Lisp_Object blocks = CALL1I (comp-func-blocks, func);
  Lisp_Object entry_block = Fgethash (Qentry, blocks, Qnil);
  struct Lisp_Hash_Table *ht = XHASH_TABLE (blocks);
  for (ptrdiff_t i = 0; i < ht->count; i++)
    {
      Lisp_Object block = HASH_VALUE (ht, i);
      if (!EQ (block, entry_block))
	declare_block (HASH_KEY (ht, i));
    }

  for (ptrdiff_t i = 0; i < ht->count; i++)
    {
      Lisp_Object block_name = HASH_KEY (ht, i);
      Lisp_Object block = HASH_VALUE (ht, i);
      Lisp_Object insns = CALL1I (comp-block-insns, block);
      if (NILP (block) || NILP (insns))
	xsignal1 (Qnative_ice,
		  build_string ("basic block is missing or empty"));

      comp.block = retrive_block (block_name);
      while (CONSP (insns))
	{
	  Lisp_Object insn = XCAR (insns);
	  emit_limple_insn (insn);
	  insns = XCDR (insns);
	}
    }
  const char *err =  gcc_jit_context_get_first_error (comp.ctxt);
  if (err)
    xsignal3 (Qnative_ice,
	      build_string ("failing to compile function"),
	      CALL1I (comp-func-name, func),
	      build_string (err));
  SAFE_FREE ();
}


/**********************************/
/* Entry points exposed to lisp.  */
/**********************************/

/* In use by Fcomp_el_to_eln_filename.  */
static Lisp_Object loadsearch_re_list;

DEFUN ("comp-el-to-eln-filename", Fcomp_el_to_eln_filename,
       Scomp_el_to_eln_filename, 1, 2, 0,
       doc: /* Given a source file return the corresponding .eln true filename.
If BASE-DIR is nil use the first entry in `comp-eln-load-path'.  */)
  (Lisp_Object filename, Lisp_Object base_dir)
{
  CHECK_STRING (filename);

  if (NILP (Ffile_exists_p (filename)))
    xsignal1 (Qfile_missing, filename);

  Lisp_Object last_mod_time =
    Fnth (make_fixnum (5), Ffile_attributes (filename, Qnil));

  if (suffix_p (filename, ".gz"))
    filename = Fsubstring (filename, Qnil, make_fixnum (-3));
  filename = Fexpand_file_name (filename, Qnil);

  /* We create eln filenames with an hash in order to look-up these
     starting from the source filename, IOW have a relation

     /absolute/path/filename.el + last_mod_time ->
     eln-cache/filename-hash.eln.

     'dlopen' can return the same handle if two shared with the same
     filename are loaded in two different times (even if the first was
     deleted!).  To prevent this scenario the last modification time
     of the source file is included in the hashing algorithm.

     As installing .eln files compiled during the build changes their
     absolute path we need an hashing mechanism that is not sensitive
     to that.  For this we replace if match PATH_DUMPLOADSEARCH or
     PATH_LOADSEARCH with '//' before generating the hash.

     Another approach would be to hash using the source file content
     but this may have a measurable performance impact.  */

  if (NILP (loadsearch_re_list))
    {
      Lisp_Object loadsearch_list =
	Fcons (build_string (PATH_DUMPLOADSEARCH),
	       Fcons (build_string (PATH_LOADSEARCH), Qnil));
      FOR_EACH_TAIL (loadsearch_list)
	loadsearch_re_list =
	  Fcons (Fregexp_quote (XCAR (loadsearch_list)), loadsearch_re_list);
    }
  Lisp_Object loadsearch_res = loadsearch_re_list;
  FOR_EACH_TAIL (loadsearch_res)
    {
      Lisp_Object match_idx =
	Fstring_match (XCAR (loadsearch_res), filename, Qnil);
      if (EQ (match_idx, make_fixnum (0)))
	{
	  filename =
	    Freplace_match (build_string ("//"), Qt, Qt, filename, Qnil);
	  break;
	}
    }

  Lisp_Object hash_input =
    concat2 (filename, Fprin1_to_string (last_mod_time, Qnil));
  Lisp_Object hash = Fsubstring (comp_hash_string (hash_input), Qnil,
				 make_fixnum (ELN_FILENAME_HASH_LEN));
  filename = concat2 (Ffile_name_nondirectory (Fsubstring (filename, Qnil,
							   make_fixnum (-3))),
		       build_string ("-"));
  filename = concat3 (filename, hash, build_string (NATIVE_ELISP_SUFFIX));
  if (NILP (base_dir))
    base_dir = XCAR (Vcomp_eln_load_path);

  if (!file_name_absolute_p (SSDATA (base_dir)))
    base_dir = Fexpand_file_name (base_dir, Vinvocation_directory);

  return Fexpand_file_name (filename,
			    concat2 (base_dir, Vcomp_native_path_postfix));
}

DEFUN ("comp--init-ctxt", Fcomp__init_ctxt, Scomp__init_ctxt,
       0, 0, 0,
       doc: /* Initialize the native compiler context. Return t on success.  */)
  (void)
{
  load_gccjit_if_necessary (true);

  if (comp.ctxt)
    {
      xsignal1 (Qnative_ice,
		build_string ("compiler context already taken"));
      return Qnil;
    }

  if (NILP (comp.emitter_dispatcher))
    {
      /* Move this into syms_of_comp the day will be dumpable.  */
      comp.emitter_dispatcher = CALLN (Fmake_hash_table);
      register_emitter (Qset_internal, emit_set_internal);
      register_emitter (Qhelper_unbind_n, emit_simple_limple_call_lisp_ret);
      register_emitter (Qhelper_unwind_protect,
			emit_simple_limple_call_void_ret);
      register_emitter (Qrecord_unwind_current_buffer,
			emit_simple_limple_call_lisp_ret);
      register_emitter (Qrecord_unwind_protect_excursion,
			emit_simple_limple_call_void_ret);
      register_emitter (Qhelper_save_restriction,
			emit_simple_limple_call_void_ret);
      /* Inliners.  */
      register_emitter (Qadd1, emit_add1);
      register_emitter (Qsub1, emit_sub1);
      register_emitter (Qconsp, emit_consp);
      register_emitter (Qcar, emit_car);
      register_emitter (Qcdr, emit_cdr);
      register_emitter (Qsetcar, emit_setcar);
      register_emitter (Qsetcdr, emit_setcdr);
      register_emitter (Qnegate, emit_negate);
      register_emitter (Qnumberp, emit_numperp);
      register_emitter (Qintegerp, emit_integerp);
      register_emitter (Qcomp_maybe_gc_or_quit, emit_maybe_gc_or_quit);
    }

  comp.ctxt = gcc_jit_context_acquire ();

  if (COMP_DEBUG)
    {
      gcc_jit_context_set_bool_option (comp.ctxt,
				       GCC_JIT_BOOL_OPTION_DEBUGINFO,
				       1);
    }
  if (COMP_DEBUG > 2)
    {
      logfile = fopen ("libgccjit.log", "w");
      gcc_jit_context_set_logfile (comp.ctxt,
				   logfile,
				   0, 0);
      gcc_jit_context_set_bool_option (comp.ctxt,
				       GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES,
				       1);
      gcc_jit_context_set_bool_option (comp.ctxt,
				       GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING,
				       1);
    }

  comp.void_type = gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_VOID);
  comp.void_ptr_type =
    gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_VOID_PTR);
  comp.bool_type = gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_BOOL);
  comp.char_type = gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_CHAR);
  comp.int_type = gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_INT);
  comp.unsigned_type = gcc_jit_context_get_type (comp.ctxt,
						 GCC_JIT_TYPE_UNSIGNED_INT);
  comp.long_type = gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_LONG);
  comp.unsigned_long_type =
    gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_UNSIGNED_LONG);
  comp.long_long_type =
    gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_LONG_LONG);
  comp.unsigned_long_long_type =
    gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_UNSIGNED_LONG_LONG);
  comp.char_ptr_type = gcc_jit_type_get_pointer (comp.char_type);
  comp.emacs_int_type = gcc_jit_context_get_int_type (comp.ctxt,
						      sizeof (EMACS_INT),
						      true);
  comp.emacs_uint_type = gcc_jit_context_get_int_type (comp.ctxt,
						       sizeof (EMACS_UINT),
						       false);
#if LISP_WORDS_ARE_POINTERS
  comp.lisp_word_type =
    gcc_jit_type_get_pointer (
      gcc_jit_struct_as_type (
	gcc_jit_context_new_opaque_struct (comp.ctxt,
					   NULL,
					   "Lisp_X")));
#else
  comp.lisp_word_type = comp.emacs_int_type;
#endif
  comp.lisp_word_tag_type
    = gcc_jit_context_get_int_type (comp.ctxt, sizeof (Lisp_Word_tag), false);
#ifdef LISP_OBJECT_IS_STRUCT
  comp.lisp_obj_i = gcc_jit_context_new_field (comp.ctxt,
                                               NULL,
                                               comp.lisp_word_type,
                                               "i");
  comp.lisp_obj_s = gcc_jit_context_new_struct_type (comp.ctxt,
                                                     NULL,
                                                     "Lisp_Object",
                                                     1,
                                                     &comp.lisp_obj_i);
  comp.lisp_obj_type = gcc_jit_struct_as_type (comp.lisp_obj_s);
#else
  comp.lisp_obj_type = comp.lisp_word_type;
#endif
  comp.lisp_obj_ptr_type = gcc_jit_type_get_pointer (comp.lisp_obj_type);
  comp.zero =
    gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					 comp.emacs_int_type,
					 0);
  comp.one =
    gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					 comp.emacs_int_type,
					 1);
  comp.inttypebits =
    gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					 comp.emacs_uint_type,
					 INTTYPEBITS);
  comp.lisp_int0 =
    gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					 comp.emacs_int_type,
					 Lisp_Int0);
  comp.ptrdiff_type = gcc_jit_context_get_int_type (comp.ctxt,
						    sizeof (void *),
						    true);
  comp.uintptr_type = gcc_jit_context_get_int_type (comp.ctxt,
						    sizeof (void *),
						    false);
  comp.size_t_type = gcc_jit_context_get_int_type (comp.ctxt,
						   sizeof (size_t),
						   false);

  comp.exported_funcs_h = CALLN (Fmake_hash_table, QCtest, Qequal);
  /*
    Always reinitialize this cause old function definitions are garbage
    collected by libgccjit when the ctxt is released.
  */
  comp.imported_funcs_h = CALLN (Fmake_hash_table);

  define_memcpy ();

  /* Define data structures.  */

  define_lisp_cons ();
  define_jmp_buf ();
  define_handler_struct ();
  define_thread_state_struct ();
  define_cast_functions ();

  return Qt;
}

DEFUN ("comp--release-ctxt", Fcomp__release_ctxt, Scomp__release_ctxt,
       0, 0, 0,
       doc: /* Release the native compiler context.  */)
  (void)
{
  load_gccjit_if_necessary (true);

  if (comp.ctxt)
    gcc_jit_context_release (comp.ctxt);

  if (logfile)
    fclose (logfile);
  comp.ctxt = NULL;

  return Qt;
}

static void
add_driver_options (void)
{
  Lisp_Object options = Fsymbol_value (Qcomp_native_driver_options);

#if defined (LIBGCCJIT_HAVE_gcc_jit_context_add_driver_option) \
  || defined (WINDOWSNT)
#pragma GCC diagnostic ignored "-Waddress"
  if (gcc_jit_context_add_driver_option)
    {
      while (CONSP (options))
        {
          gcc_jit_context_add_driver_option (comp.ctxt,
					     SSDATA (XCAR (options)));
          options = XCDR (options);
        }

      return;
    }
#pragma GCC diagnostic pop
#endif
  if (CONSP (options))
    {
      xsignal1 (Qnative_compiler_error,
                build_string ("Customizing native compiler options"
                              " via `comp-native-driver-options' is"
                              " only available on libgccjit version 9"
                              " and above."));
    }
}

static void
restore_sigmask (void)
{
  pthread_sigmask (SIG_SETMASK, &saved_sigset, 0);
  unblock_input ();
}

DEFUN ("comp--compile-ctxt-to-file", Fcomp__compile_ctxt_to_file,
       Scomp__compile_ctxt_to_file,
       1, 1, 0,
       doc: /* Compile as native code the current context to file.  */)
  (Lisp_Object file_name)
{
  load_gccjit_if_necessary (true);

  CHECK_STRING (file_name);
  Lisp_Object base_name = Fsubstring (file_name, Qnil, make_fixnum (-4));

  gcc_jit_context_set_int_option (comp.ctxt,
				  GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL,
				  COMP_SPEED < 0 ? 0
				  : (COMP_SPEED > 3 ? 3 : COMP_SPEED));
  comp.d_default_idx =
    CALL1I (comp-data-container-idx, CALL1I (comp-ctxt-d-default, Vcomp_ctxt));
  comp.d_impure_idx =
    CALL1I (comp-data-container-idx, CALL1I (comp-ctxt-d-impure, Vcomp_ctxt));
  comp.d_ephemeral_idx =
    CALL1I (comp-data-container-idx, CALL1I (comp-ctxt-d-ephemeral, Vcomp_ctxt));

  sigset_t oldset;
  ptrdiff_t count = 0;

  if (!noninteractive)
    {
      sigset_t blocked;
      /* Gcc doesn't like being interrupted at all.  */
      block_input ();
      sigemptyset (&blocked);
      sigaddset (&blocked, SIGALRM);
      sigaddset (&blocked, SIGINT);
#ifdef USABLE_SIGIO
      sigaddset (&blocked, SIGIO);
#endif
      pthread_sigmask (SIG_BLOCK, &blocked, &oldset);
      count = SPECPDL_INDEX ();
      record_unwind_protect_void (restore_sigmask);
    }
  emit_ctxt_code ();

  /* Define inline functions.  */
  define_CAR_CDR ();
  define_PSEUDOVECTORP ();
  define_CHECK_TYPE ();
  define_CHECK_IMPURE ();
  define_bool_to_lisp_obj ();
  define_setcar_setcdr ();
  define_add1_sub1 ();
  define_negate ();
  define_maybe_gc_or_quit ();

  struct Lisp_Hash_Table *func_h =
    XHASH_TABLE (CALL1I (comp-ctxt-funcs-h, Vcomp_ctxt));
  for (ptrdiff_t i = 0; i < func_h->count; i++)
    declare_function (HASH_VALUE (func_h, i));
  /* Compile all functions. Can't be done before because the
     relocation structs has to be already defined.  */
  for (ptrdiff_t i = 0; i < func_h->count; i++)
    compile_function (HASH_VALUE (func_h, i));

  add_driver_options ();

  if (COMP_DEBUG)
      gcc_jit_context_dump_to_file (comp.ctxt,
				    format_string ("%s.c", SSDATA (base_name)),
				    1);
  if (COMP_DEBUG > 2)
    gcc_jit_context_dump_reproducer_to_file (comp.ctxt, "comp_reproducer.c");

  AUTO_STRING (dot_so, NATIVE_ELISP_SUFFIX);

  Lisp_Object tmp_file =
    Fmake_temp_file_internal (base_name, Qnil, dot_so, Qnil);
  gcc_jit_context_compile_to_file (comp.ctxt,
				   GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY,
				   SSDATA (tmp_file));

  CALL2I (comp--replace-output-file, file_name, tmp_file);

  if (!noninteractive)
    unbind_to (count, Qnil);

  return file_name;
}

DEFUN ("comp-libgccjit-version", Fcomp_libgccjit_version,
       Scomp_libgccjit_version, 0, 0, 0,
       doc: /* Return the libgccjit version in use in the form
(MAJOR MINOR PATCHLEVEL) or nil if unknown (pre GCC10).  */)
  (void)
{
#if defined (LIBGCCJIT_HAVE_gcc_jit_version) || defined (WINDOWSNT)
  load_gccjit_if_necessary (true);

  /* FIXME this kludge is quite bad.  Can we dynamically load on all
     operating systems?  */
#pragma GCC diagnostic ignored "-Waddress"
  return gcc_jit_version_major
    ? list3 (make_fixnum (gcc_jit_version_major ()),
	     make_fixnum (gcc_jit_version_minor ()),
	     make_fixnum (gcc_jit_version_patchlevel ()))
    : Qnil;
#pragma GCC diagnostic pop
#else
  return Qnil;
#endif
}


/******************************************************************************/
/* Helper functions called from the run-time.				      */
/* These can't be statics till shared mechanism is used to solve relocations. */
/* Note: this are all potentially definable directly to gcc and are here just */
/* for laziness. Change this if a performance impact is measured.             */
/******************************************************************************/

void
helper_unwind_protect (Lisp_Object handler)
{
  /* Support for a function here is new in 24.4.  */
  record_unwind_protect (FUNCTIONP (handler) ? bcall0 : prog_ignore,
			 handler);
}

Lisp_Object
helper_temp_output_buffer_setup (Lisp_Object x)
{
  CHECK_STRING (x);
  temp_output_buffer_setup (SSDATA (x));
  return Vstandard_output;
}

Lisp_Object
helper_unbind_n (Lisp_Object n)
{
  return unbind_to (SPECPDL_INDEX () - XFIXNUM (n), Qnil);
}

void
helper_save_restriction (void)
{
  record_unwind_protect (save_restriction_restore,
			 save_restriction_save ());
}

bool
helper_PSEUDOVECTOR_TYPEP_XUNTAG (Lisp_Object a, enum pvec_type code)
{
  return PSEUDOVECTOR_TYPEP (XUNTAG (a, Lisp_Vectorlike,
				     union vectorlike_header),
			     code);
}


/*********************************/
/* Disposal of compilation units */
/*********************************/

/*
  The problem: Windows does not let us delete an .eln file that has
  been loaded by a process.  This has two implications in Emacs:

  1) It is not possible to recompile a lisp file if the corresponding
  .eln file has been loaded.  This is because we'd like to use the same
  filename, but we can't delete the old .eln file.

  2) It is not possible to delete a package using `package-delete'
  if an .eln file has been loaded.

  * General idea

  The solution to these two problems is to move the foo.eln file
  somewhere else and have the last Emacs instance using it delete it.
  To make it easy to find what files need to be removed we use two approaches.

  In the 1) case we rename foo.eln to fooXXXXXX.eln.old in the same
  folder.  When Emacs is unloading "foo" (either GC'd the native
  compilation unit or Emacs is closing (see below)) we delete all the
  .eln.old files in the folder where the original foo.eln was stored.

  Ideally we'd figure out the new name of foo.eln and delete it if it
  ends in .eln.old.  There is no simple API to do this in Windows.
  GetModuleFileName () returns the original filename, not the current
  one.  This forces us to put .eln.old files in an agreed upon path.
  We cannot use %TEMP% because it may be in another drive and then the
  rename operation would fail.

  In the 2) case we can't use the same folder where the .eln file
  resided, as we are trying to completely remove the package.  Since we
  are removing packages we can safely move the .eln.old file to
  `package-user-dir' as we are sure that that would not mean changing
  drives.

  * Implementation details

  The concept of disposal of a native compilation unit refers to
  unloading the shared library and deleting all the .eln.old files in
  the directory.  These are two separate steps.  We'll call them
  early-disposal and late-disposal.

  There are two data structures used:

  - The `all_loaded_comp_units_h` hashtable.

  This hashtable is used like an array of weak references to native
  compilation units.  This hash table is filled by load_comp_unit ()
  and dispose_all_remaining_comp_units () iterates over all values
  that were not disposed by the GC and performs all disposal steps
  when Emacs is closing.

  - The `delayed_comp_unit_disposal_list` list.

  This is were the dispose_comp_unit () function, when called by the
  GC sweep stage, stores the original filenames of the disposed native
  compilation units.  This is an ad-hoc C structure instead of a Lisp
  cons because we need to allocate instances of this structure during
  the GC.

  The finish_delayed_disposal_of_comp_units () function will iterate
  over this list and perform the late-disposal step when Emacs is
  closing.

*/

#ifdef WINDOWSNT
#define OLD_ELN_SUFFIX_REGEXP build_string ("\\.eln\\.old\\'")

static Lisp_Object all_loaded_comp_units_h;

/* We need to allocate instances of this struct during a GC sweep.
   This is why it can't be transformed into a simple cons.  */
struct delayed_comp_unit_disposal
{
  struct delayed_comp_unit_disposal *next;
  char *filename;
};

struct delayed_comp_unit_disposal *delayed_comp_unit_disposal_list;

static Lisp_Object
return_nil (Lisp_Object arg)
{
  return Qnil;
}

/* Tries to remove all *.eln.old files in DIRNAME.

   Any error is ignored because it may be due to the file being loaded
   in another Emacs instance.  */
static void
clean_comp_unit_directory (Lisp_Object dirpath)
{
  if (NILP (dirpath))
    return;
  Lisp_Object files_in_dir;
  files_in_dir = internal_condition_case_4 (Fdirectory_files, dirpath, Qt,
                                            OLD_ELN_SUFFIX_REGEXP, Qnil, Qt,
                                            return_nil);
  FOR_EACH_TAIL (files_in_dir) { DeleteFile (SSDATA (XCAR (files_in_dir))); }
}

/* Tries to remove all *.eln.old files in `package-user-dir'.

   This is called when Emacs is closing to clean any *.eln left from a
   deleted package.  */
void
clean_package_user_dir_of_old_comp_units (void)
{
  Lisp_Object package_user_dir
      = find_symbol_value (intern ("package-user-dir"));
  if (EQ (package_user_dir, Qunbound) || !STRINGP (package_user_dir))
    return;

  clean_comp_unit_directory (package_user_dir);
}

/* This function disposes all compilation units that are still loaded.

   It is important that this function is called only right before
   Emacs is closed, otherwise we risk running a subr that is
   implemented in an unloaded dynamic library.  */
void
dispose_all_remaining_comp_units (void)
{
  struct Lisp_Hash_Table *h = XHASH_TABLE (all_loaded_comp_units_h);

  for (ptrdiff_t i = 0; i < HASH_TABLE_SIZE (h); ++i)
    {
      Lisp_Object k = HASH_KEY (h, i);
      if (!EQ (k, Qunbound))
        {
          Lisp_Object val = HASH_VALUE (h, i);
          struct Lisp_Native_Comp_Unit *cu = XNATIVE_COMP_UNIT (val);
          dispose_comp_unit (cu, false);
        }
    }
}

/* This function finishes the disposal of compilation units that were
   passed to `dispose_comp_unit` with DELAY == true.

   This function is called when Emacs is idle and when it is about to
   close.  */
void
finish_delayed_disposal_of_comp_units (void)
{
  for (struct delayed_comp_unit_disposal *item
       = delayed_comp_unit_disposal_list;
       delayed_comp_unit_disposal_list; item = delayed_comp_unit_disposal_list)
    {
      delayed_comp_unit_disposal_list = item->next;
      Lisp_Object dirname = internal_condition_case_1 (
          Ffile_name_directory, build_string (item->filename), Qt, return_nil);
      clean_comp_unit_directory (dirname);
      xfree (item->filename);
      xfree (item);
    }
}
#endif

/* This function puts the compilation unit in the
  `all_loaded_comp_units_h` hashmap.  */
static void
register_native_comp_unit (Lisp_Object comp_u)
{
#ifdef WINDOWSNT
  /* We have to do this since we can't use `gensym'. This function is
     called early when loading a dump file and subr.el may not have
     been loaded yet.  */
  static intmax_t count;

  Fputhash (make_int (count++), comp_u, all_loaded_comp_units_h);
#endif
}

/* This function disposes compilation units.  It is called during the GC sweep
   stage and when Emacs is closing.

   On Windows the the DELAY parameter specifies whether the native
   compilation file will be deleted right away (if necessary) or put
   on a list.  That list will be dealt with by
   `finish_delayed_disposal_of_comp_units`.  */
void
dispose_comp_unit (struct Lisp_Native_Comp_Unit *comp_handle, bool delay)
{
  eassert (comp_handle->handle);
  dynlib_close (comp_handle->handle);
#ifdef WINDOWSNT
  if (!delay)
    {
      Lisp_Object dirname = internal_condition_case_1 (
          Ffile_name_directory, build_string (comp_handle->cfile), Qt,
          return_nil);
      if (!NILP (dirname))
        clean_comp_unit_directory (dirname);
      xfree (comp_handle->cfile);
      comp_handle->cfile = NULL;
    }
  else
    {
      struct delayed_comp_unit_disposal *head;
      head = xmalloc (sizeof (struct delayed_comp_unit_disposal));
      head->next = delayed_comp_unit_disposal_list;
      head->filename = comp_handle->cfile;
      comp_handle->cfile = NULL;
      delayed_comp_unit_disposal_list = head;
    }
#endif
}


/***********************************/
/* Deferred compilation mechanism. */
/***********************************/

/* List of sources we'll compile and load after having conventionally
   loaded the compiler and its dependencies.  */
static Lisp_Object delayed_sources;


/* Queue an asyncronous compilation for the source file defining
   FUNCTION_NAME and perform a late load.

   NOTE: ideally would be nice to move its call simply into Fload but
   we need DEFINITION to guard against function redefinition while
   async compilation happen.  */

void
maybe_defer_native_compilation (Lisp_Object function_name,
				Lisp_Object definition)
{
#if 0
#include <sys/types.h>
#include <unistd.h>
  if (!NILP (function_name) &&
      STRINGP (Vload_true_file_name))
    {
      static FILE *f;
      if (!f)
	{
	  char str[128];
	  sprintf (str, "log_%d", getpid ());
	  f = fopen (str, "w");
	}
      if (!f)
	exit (1);
      fprintf (f, "function %s file %s\n",
	       SSDATA (Fsymbol_name (function_name)),
	       SSDATA (Vload_true_file_name));
      fflush (f);
    }
#endif
  if (!load_gccjit_if_necessary (false))
    return;

  if (!comp_deferred_compilation
      || noninteractive
      || !NILP (Vpurify_flag)
      || !COMPILEDP (definition)
      || !STRINGP (Vload_true_file_name)
      || !suffix_p (Vload_true_file_name, ".elc"))
    return;

  Lisp_Object src =
    concat2 (CALL1I (file-name-sans-extension, Vload_true_file_name),
	     build_pure_c_string (".el"));
  if (NILP (Ffile_exists_p (src)))
    {
      src = concat2 (src, build_pure_c_string (".gz"));
      if (NILP (Ffile_exists_p (src)))
	return;
    }

  /* This is to have deferred compilaiton able to compile comp
     dependecies breaking circularity.  */
  if (!NILP (Ffeaturep (Qcomp, Qnil)))
    {
      /* Comp already loaded.  */
      if (!NILP (delayed_sources))
	{
	  CALLN (Ffuncall, intern_c_string ("native-compile-async"),
		 delayed_sources, Qnil, Qlate);
	  delayed_sources = Qnil;
	}
      Fputhash (function_name, definition, Vcomp_deferred_pending_h);
      CALLN (Ffuncall, intern_c_string ("native-compile-async"), src, Qnil,
	     Qlate);
    }
  else
    {
      delayed_sources = Fcons (src, delayed_sources);
      /* Require comp only once.  */
      static bool comp_required = false;
      if (!comp_required)
	{
	  comp_required = true;
	  Frequire (Qcomp, Qnil, Qnil);
	}
    }
}


/**************************************/
/* Functions used to load eln files.  */
/**************************************/

/* Fixup the system eln-cache dir.  This is the last entry in
   `comp-eln-load-path'.  */
void
fixup_eln_load_path (Lisp_Object directory)
{
  Lisp_Object last_cell = Qnil;
  Lisp_Object tmp = Vcomp_eln_load_path;
  FOR_EACH_TAIL (tmp)
    if (CONSP (tmp))
      last_cell = tmp;

  Lisp_Object eln_cache_sys =
    Ffile_name_directory (concat2 (Vinvocation_directory,
				   directory));
  /* One directory up...  */
  eln_cache_sys =
    Ffile_name_directory (Fsubstring (eln_cache_sys, Qnil,
				      make_fixnum (-1)));
  Fsetcar (last_cell, eln_cache_sys);
}

typedef char *(*comp_lit_str_func) (void);

/* Deserialize read and return static object.  */
static Lisp_Object
load_static_obj (struct Lisp_Native_Comp_Unit *comp_u, const char *name)
{
  static_obj_t *(*f)(void) = dynlib_sym (comp_u->handle, name);
  if (!f)
    xsignal1 (Qnative_lisp_file_inconsistent, comp_u->file);

  static_obj_t *res = f ();
  return Fread (make_string (res->data, res->len));
}

/* Return false when something is wrong or true otherwise.  */

static bool
check_comp_unit_relocs (struct Lisp_Native_Comp_Unit *comp_u)
{
  dynlib_handle_ptr handle = comp_u->handle;
  Lisp_Object *data_relocs = dynlib_sym (handle, DATA_RELOC_SYM);
  Lisp_Object *data_imp_relocs = dynlib_sym (handle, DATA_RELOC_IMPURE_SYM);

  EMACS_INT d_vec_len = XFIXNUM (Flength (comp_u->data_vec));
  for (EMACS_INT i = 0; i < d_vec_len; i++)
    if (!EQ (data_relocs[i],  AREF (comp_u->data_vec, i)))
      return false;

  d_vec_len = XFIXNUM (Flength (comp_u->data_impure_vec));
  for (EMACS_INT i = 0; i < d_vec_len; i++)
    {
      Lisp_Object x = data_imp_relocs[i];
      if (EQ (x, Qlambda_fixup))
	return false;
      else if (SUBR_NATIVE_COMPILEDP (x))
	{
	  if (NILP (Fgethash (x, comp_u->lambda_gc_guard_h, Qnil)))
	    return false;
	}
      else if (!EQ (data_imp_relocs[i], AREF (comp_u->data_impure_vec, i)))
	return false;
    }
  return true;
}

static void
unset_cu_load_ongoing (Lisp_Object comp_u)
{
  XNATIVE_COMP_UNIT (comp_u)->load_ongoing = false;
}

void
load_comp_unit (struct Lisp_Native_Comp_Unit *comp_u, bool loading_dump,
		bool late_load)
{
  dynlib_handle_ptr handle = comp_u->handle;
  Lisp_Object comp_u_lisp_obj;
  XSETNATIVE_COMP_UNIT (comp_u_lisp_obj, comp_u);

  Lisp_Object *saved_cu = dynlib_sym (handle, COMP_UNIT_SYM);
  if (!saved_cu)
    xsignal1 (Qnative_lisp_file_inconsistent, comp_u->file);
  comp_u->loaded_once = !NILP (*saved_cu);
  Lisp_Object *data_eph_relocs =
    dynlib_sym (handle, DATA_RELOC_EPHEMERAL_SYM);

  /* While resurrecting from an image dump loading more than once the
     same compilation unit does not make any sense.  */
  eassert (!(loading_dump && comp_u->loaded_once));

  if (comp_u->loaded_once)
    /* 'dlopen' returns the same handle when trying to load two times
       the same shared.  In this case touching 'd_reloc' etc leads to
       fails in case a frame with a reference to it in a live reg is
       active (comp-speed >= 0).

       We must *never* mess with static pointers in an already loaded
       eln.  */
    {
      comp_u_lisp_obj = *saved_cu;
      comp_u = XNATIVE_COMP_UNIT (comp_u_lisp_obj);
      comp_u->loaded_once = true;
    }
  else
    *saved_cu = comp_u_lisp_obj;

  /* Once we are sure to have the right compilation unit we want to
     identify is we have at least another load active on it.  */
  bool recursive_load = comp_u->load_ongoing;
  comp_u->load_ongoing = true;
  ptrdiff_t count = SPECPDL_INDEX ();
  if (!recursive_load)
    record_unwind_protect (unset_cu_load_ongoing, comp_u_lisp_obj);

  freloc_check_fill ();

  Lisp_Object (*top_level_run)(Lisp_Object)
    = dynlib_sym (handle,
		  late_load ? "late_top_level_run" : "top_level_run");

  /* Always set data_imp_relocs pointer in the compilation unit (in can be
     used in 'dump_do_dump_relocation').  */
  comp_u->data_imp_relocs = dynlib_sym (handle, DATA_RELOC_IMPURE_SYM);

  if (!comp_u->loaded_once)
    {
      struct thread_state ***current_thread_reloc =
	dynlib_sym (handle, CURRENT_THREAD_RELOC_SYM);
      void **pure_reloc = dynlib_sym (handle, PURE_RELOC_SYM);
      Lisp_Object *data_relocs = dynlib_sym (handle, DATA_RELOC_SYM);
      Lisp_Object *data_imp_relocs = comp_u->data_imp_relocs;
      void **freloc_link_table = dynlib_sym (handle, FUNC_LINK_TABLE_SYM);

      if (!(current_thread_reloc
	    && pure_reloc
	    && data_relocs
	    && data_imp_relocs
	    && data_eph_relocs
	    && freloc_link_table
	    && top_level_run)
	  || NILP (Fstring_equal (load_static_obj (comp_u, LINK_TABLE_HASH_SYM),
				  Vcomp_abi_hash)))
	xsignal1 (Qnative_lisp_file_inconsistent, comp_u->file);

      *current_thread_reloc = &current_thread;
      *pure_reloc = pure;

      /* Imported functions.  */
      *freloc_link_table = freloc.link_table;

      /* Imported data.  */
      if (!loading_dump)
	{
	  comp_u->optimize_qualities =
	    load_static_obj (comp_u, TEXT_OPTIM_QLY_SYM);
	  comp_u->data_vec = load_static_obj (comp_u, TEXT_DATA_RELOC_SYM);
	  comp_u->data_impure_vec =
	    load_static_obj (comp_u, TEXT_DATA_RELOC_IMPURE_SYM);

	  if (!NILP (Vpurify_flag))
	    /* Non impure can be copied into pure space.  */
	    comp_u->data_vec = Fpurecopy (comp_u->data_vec);
	}

      EMACS_INT d_vec_len = XFIXNUM (Flength (comp_u->data_vec));
      for (EMACS_INT i = 0; i < d_vec_len; i++)
	data_relocs[i] = AREF (comp_u->data_vec, i);

      d_vec_len = XFIXNUM (Flength (comp_u->data_impure_vec));
      for (EMACS_INT i = 0; i < d_vec_len; i++)
	data_imp_relocs[i] = AREF (comp_u->data_impure_vec, i);

      /* If we register them while dumping we will get some entries in
	 the hash table that will be duplicated when pdumper calls
	 load_comp_unit.  */
      if (!will_dump_p ())
	register_native_comp_unit (comp_u_lisp_obj);
    }

  if (!loading_dump)
    {
      /* Note: data_ephemeral_vec is not GC protected except than by
	 this function frame.  After this functions will be
	 deactivated GC will be free to collect it, but it MUST
	 survive till 'top_level_run' has finished his job.  We store
	 into the ephemeral allocation class only objects that we know
	 are necessary exclusively during the first load.  Once these
	 are collected we don't have to maintain them in the heap
	 forever.  */
      Lisp_Object volatile data_ephemeral_vec;
      /* In case another load of the same CU is active on the stack
	 all ephemeral data is hold by that frame.  Re-writing
	 'data_ephemeral_vec' would be not only a waste of cycles but
	 more importanly would lead to crashed if the contained data
	 is not cons hashed.  */
      if (!recursive_load)
	{
	  Lisp_Object volatile data_ephemeral_vec  =
	    load_static_obj (comp_u, TEXT_DATA_RELOC_EPHEMERAL_SYM);

	  EMACS_INT d_vec_len = XFIXNUM (Flength (data_ephemeral_vec));
	  for (EMACS_INT i = 0; i < d_vec_len; i++)
	    data_eph_relocs[i] = AREF (data_ephemeral_vec, i);
	}
      /* Executing this will perform all the expected environment
	 modifications.  */
      top_level_run (comp_u_lisp_obj);
      /* Make sure data_ephemeral_vec still exists after top_level_run has run.
	 Guard against sibling call optimization (or any other).  */
      data_ephemeral_vec = data_ephemeral_vec;
      eassert (check_comp_unit_relocs (comp_u));
    }

  if (!recursive_load)
    /* Clean-up the load ongoing flag in case.  */
    unbind_to (count, Qnil);

  return;
}

Lisp_Object
native_function_doc (Lisp_Object function)
{
  struct Lisp_Native_Comp_Unit *cu =
    XNATIVE_COMP_UNIT (Fsubr_native_comp_unit (function));

  if (NILP (cu->data_fdoc_v))
    cu->data_fdoc_v = load_static_obj (cu, TEXT_FDOC_SYM);
  if (!VECTORP (cu->data_fdoc_v))
    xsignal2 (Qnative_lisp_file_inconsistent, cu->file,
	      build_string ("missing documentation vector"));
  return AREF (cu->data_fdoc_v, XSUBR (function)->doc);
}

static Lisp_Object
make_subr (Lisp_Object symbol_name, Lisp_Object minarg, Lisp_Object maxarg,
	   Lisp_Object c_name, Lisp_Object doc_idx, Lisp_Object intspec,
	   Lisp_Object comp_u)
{
  struct Lisp_Native_Comp_Unit *cu = XNATIVE_COMP_UNIT (comp_u);
  dynlib_handle_ptr handle = cu->handle;
  if (!handle)
    xsignal0 (Qwrong_register_subr_call);

  void *func = dynlib_sym (handle, SSDATA (c_name));
  eassert (func);
  union Aligned_Lisp_Subr *x =
    (union Aligned_Lisp_Subr *) allocate_pseudovector (
				  VECSIZE (union Aligned_Lisp_Subr),
				  0, VECSIZE (union Aligned_Lisp_Subr),
				  PVEC_SUBR);
  if (CONSP (minarg))
    {
      /* Dynamic code.  */
      x->s.lambda_list[0] = maxarg;
      maxarg = XCDR (minarg);
      minarg = XCAR (minarg);
    }
  else
    x->s.lambda_list[0] = Qnil;
  x->s.function.a0 = func;
  x->s.min_args = XFIXNUM (minarg);
  x->s.max_args = FIXNUMP (maxarg) ? XFIXNUM (maxarg) : MANY;
  x->s.symbol_name = xstrdup (SSDATA (symbol_name));
  x->s.native_intspec = intspec;
  x->s.doc = XFIXNUM (doc_idx);
  x->s.native_comp_u[0] = comp_u;
  x->s.native_c_name[0] = xstrdup (SSDATA (c_name));
  Lisp_Object tem;
  XSETSUBR (tem, &x->s);

  return tem;
}

DEFUN ("comp--register-lambda", Fcomp__register_lambda, Scomp__register_lambda,
       7, 7, 0,
       doc: /* This gets called by top_level_run during load phase to register
	       anonymous lambdas.  */)
  (Lisp_Object reloc_idx, Lisp_Object minarg, Lisp_Object maxarg,
   Lisp_Object c_name, Lisp_Object doc_idx, Lisp_Object intspec,
   Lisp_Object comp_u)
{
  struct Lisp_Native_Comp_Unit *cu = XNATIVE_COMP_UNIT (comp_u);
  if (cu->loaded_once)
    return Qnil;

  Lisp_Object tem =
    make_subr (c_name, minarg, maxarg, c_name, doc_idx, intspec, comp_u);

  /* We must protect it against GC because the function is not
     reachable through symbols.  */
  Fputhash (tem, Qt, cu->lambda_gc_guard_h);
  /* This is for fixing up the value in d_reloc while resurrecting
     from dump.  See 'dump_do_dump_relocation'.  */
  eassert (NILP (Fgethash (c_name, cu->lambda_c_name_idx_h, Qnil)));
  Fputhash (c_name, reloc_idx, cu->lambda_c_name_idx_h);
  /* Do the real relocation fixup.  */
  cu->data_imp_relocs[XFIXNUM (reloc_idx)] = tem;

  return tem;
}

DEFUN ("comp--register-subr", Fcomp__register_subr, Scomp__register_subr,
       7, 7, 0,
       doc: /* This gets called by top_level_run during load phase to register
	       each exported subr.  */)
  (Lisp_Object name, Lisp_Object minarg, Lisp_Object maxarg,
   Lisp_Object c_name, Lisp_Object doc_idx, Lisp_Object intspec,
   Lisp_Object comp_u)
{
  Lisp_Object tem =
    make_subr (SYMBOL_NAME (name), minarg, maxarg, c_name, doc_idx, intspec,
	       comp_u);

  LOADHIST_ATTACH (Fcons (Qdefun, name));

  { /* Handle automatic advice activation (bug#42038).
       See `defalias'.  */
    Lisp_Object hook = Fget (name, Qdefalias_fset_function);
    if (!NILP (hook))
      call2 (hook, name, tem);
    else
      Ffset (name, tem);
  }

  return tem;
}

DEFUN ("comp--late-register-subr", Fcomp__late_register_subr,
       Scomp__late_register_subr, 7, 7, 0,
       doc: /* This gets called by late_top_level_run during load
	       phase to register each exported subr.  */)
  (Lisp_Object name, Lisp_Object minarg, Lisp_Object maxarg,
   Lisp_Object c_name, Lisp_Object doc, Lisp_Object intspec,
   Lisp_Object comp_u)
{
  if (!NILP (Fequal (Fsymbol_function (name),
		     Fgethash (name, Vcomp_deferred_pending_h, Qnil))))
    Fcomp__register_subr (name, minarg, maxarg, c_name, doc, intspec, comp_u);
  Fremhash (name, Vcomp_deferred_pending_h);
  return Qnil;
}

/* Load related routines.  */
DEFUN ("native-elisp-load", Fnative_elisp_load, Snative_elisp_load, 1, 2, 0,
       doc: /* Load native elisp code FILE.
	       LATE_LOAD has to be non nil when loading for deferred
	       compilation.  */)
  (Lisp_Object file, Lisp_Object late_load)
{
  CHECK_STRING (file);
  if (NILP (Ffile_exists_p (file)))
    xsignal2 (Qnative_lisp_load_failed, build_string ("file does not exists"),
	      file);
  struct Lisp_Native_Comp_Unit *comp_u = allocate_native_comp_unit ();
  comp_u->handle = dynlib_open (SSDATA (file));
  if (!comp_u->handle)
    xsignal2 (Qnative_lisp_load_failed, file, build_string (dynlib_error ()));
  comp_u->file = file;
#ifdef WINDOWSNT
  comp_u->cfile = xlispstrdup (file);
#endif
  comp_u->data_vec = Qnil;
  comp_u->lambda_gc_guard_h = CALLN (Fmake_hash_table, QCtest, Qeq);
  comp_u->lambda_c_name_idx_h = CALLN (Fmake_hash_table, QCtest, Qequal);
  load_comp_unit (comp_u, false, !NILP (late_load));

  return Qt;
}

#endif /* HAVE_NATIVE_COMP */

DEFUN ("native-comp-available-p", Fnative_comp_available_p,
       Snative_comp_available_p, 0, 0, 0,
       doc: /* Returns t if native compilation of Lisp files is available in
this instance of Emacs. */)
  (void)
{
#ifdef HAVE_NATIVE_COMP
  return load_gccjit_if_necessary (false) ? Qt : Qnil;
#else
  return Qnil;
#endif
}


void
syms_of_comp (void)
{
#ifdef HAVE_NATIVE_COMP
  /* Compiler control customizes.  */
  DEFVAR_BOOL ("comp-deferred-compilation", comp_deferred_compilation,
	       doc: /* If non-nil compile asyncronously all .elc files
being loaded.

Once compilation happened each function definition is updated to the
native compiled one.  */);
  comp_deferred_compilation = true;

  DEFSYM (Qcomp_speed, "comp-speed");
  DEFSYM (Qcomp_debug, "comp-debug");
  DEFSYM (Qcomp_native_driver_options, "comp-native-driver-options");

  /* Limple instruction set.  */
  DEFSYM (Qcomment, "comment");
  DEFSYM (Qjump, "jump");
  DEFSYM (Qcall, "call");
  DEFSYM (Qcallref, "callref");
  DEFSYM (Qdirect_call, "direct-call");
  DEFSYM (Qdirect_callref, "direct-callref");
  DEFSYM (Qsetimm, "setimm");
  DEFSYM (Qreturn, "return");
  DEFSYM (Qcomp_mvar, "comp-mvar");
  DEFSYM (Qcond_jump, "cond-jump");
  DEFSYM (Qphi, "phi");
  /* Ops in use for prologue emission.  */
  DEFSYM (Qset_par_to_local, "set-par-to-local");
  DEFSYM (Qset_args_to_local, "set-args-to-local");
  DEFSYM (Qset_rest_args_to_local, "set-rest-args-to-local");
  DEFSYM (Qinc_args, "inc-args");
  DEFSYM (Qcond_jump_narg_leq, "cond-jump-narg-leq");
  /* Others.  */
  DEFSYM (Qpush_handler, "push-handler");
  DEFSYM (Qpop_handler, "pop-handler");
  DEFSYM (Qfetch_handler, "fetch-handler");
  DEFSYM (Qcondition_case, "condition-case");
  /* call operands.  */
  DEFSYM (Qcatcher, "catcher");
  DEFSYM (Qentry, "entry");
  DEFSYM (Qset_internal, "set_internal");
  DEFSYM (Qrecord_unwind_current_buffer, "record_unwind_current_buffer");
  DEFSYM (Qrecord_unwind_protect_excursion, "record_unwind_protect_excursion");
  DEFSYM (Qhelper_unbind_n, "helper_unbind_n");
  DEFSYM (Qhelper_unwind_protect, "helper_unwind_protect");
  DEFSYM (Qhelper_save_restriction, "helper_save_restriction");
  /* Inliners.  */
  DEFSYM (Qadd1, "1+");
  DEFSYM (Qsub1, "1-");
  DEFSYM (Qconsp, "consp");
  DEFSYM (Qcar, "car");
  DEFSYM (Qcdr, "cdr");
  DEFSYM (Qsetcar, "setcar");
  DEFSYM (Qsetcdr, "setcdr");
  DEFSYM (Qnegate, "negate");
  DEFSYM (Qnumberp, "numberp");
  DEFSYM (Qintegerp, "integerp");
  DEFSYM (Qcomp_maybe_gc_or_quit, "comp-maybe-gc-or-quit");

  /* Allocation classes. */
  DEFSYM (Qd_default, "d-default");
  DEFSYM (Qd_impure, "d-impure");
  DEFSYM (Qd_ephemeral, "d-ephemeral");

  /* Others.  */
  DEFSYM (Qcomp, "comp");
  DEFSYM (Qfixnum, "fixnum");
  DEFSYM (Qscratch, "scratch");
  DEFSYM (Qlate, "late");
  DEFSYM (Qlambda_fixup, "lambda-fixup");
  DEFSYM (Qgccjit, "gccjit");

  /* To be signaled by the compiler.  */
  DEFSYM (Qnative_compiler_error, "native-compiler-error");
  Fput (Qnative_compiler_error, Qerror_conditions,
	pure_list (Qnative_compiler_error, Qerror));
  Fput (Qnative_compiler_error, Qerror_message,
        build_pure_c_string ("Native compiler error"));

  DEFSYM (Qnative_ice, "native-ice");
  Fput (Qnative_ice, Qerror_conditions,
	pure_list (Qnative_ice, Qnative_compiler_error, Qerror));
  Fput (Qnative_ice, Qerror_message,
        build_pure_c_string ("Internal native compiler error"));

  /* By the load machinery.  */
  DEFSYM (Qnative_lisp_load_failed, "native-lisp-load-failed");
  Fput (Qnative_lisp_load_failed, Qerror_conditions,
	pure_list (Qnative_lisp_load_failed, Qerror));
  Fput (Qnative_lisp_load_failed, Qerror_message,
        build_pure_c_string ("Native elisp load failed"));

  DEFSYM (Qnative_lisp_wrong_reloc, "native-lisp-wrong-reloc");
  Fput (Qnative_lisp_wrong_reloc, Qerror_conditions,
	pure_list (Qnative_lisp_wrong_reloc, Qnative_lisp_load_failed, Qerror));
  Fput (Qnative_lisp_wrong_reloc, Qerror_message,
        build_pure_c_string ("Primitive redefined or wrong relocation"));

  DEFSYM (Qwrong_register_subr_call, "wrong-register-subr-call");
  Fput (Qwrong_register_subr_call, Qerror_conditions,
	pure_list (Qwrong_register_subr_call, Qnative_lisp_load_failed, Qerror));
  Fput (Qwrong_register_subr_call, Qerror_message,
        build_pure_c_string ("comp--register-subr can only be called during "
			    "native lisp load phase."));

  DEFSYM (Qnative_lisp_file_inconsistent, "native-lisp-file-inconsistent");
  Fput (Qnative_lisp_file_inconsistent, Qerror_conditions,
	pure_list (Qnative_lisp_file_inconsistent, Qnative_lisp_load_failed, Qerror));
  Fput (Qnative_lisp_file_inconsistent, Qerror_message,
        build_pure_c_string ("eln file inconsistent with current runtime "
			     "configuration, please recompile"));

  defsubr (&Scomp_el_to_eln_filename);
  defsubr (&Scomp__init_ctxt);
  defsubr (&Scomp__release_ctxt);
  defsubr (&Scomp__compile_ctxt_to_file);
  defsubr (&Scomp_libgccjit_version);
  defsubr (&Scomp__register_lambda);
  defsubr (&Scomp__register_subr);
  defsubr (&Scomp__late_register_subr);
  defsubr (&Snative_elisp_load);

  staticpro (&comp.exported_funcs_h);
  comp.exported_funcs_h = Qnil;
  staticpro (&comp.imported_funcs_h);
  comp.imported_funcs_h = Qnil;
  staticpro (&comp.func_blocks_h);
  staticpro (&comp.emitter_dispatcher);
  comp.emitter_dispatcher = Qnil;
  staticpro (&delayed_sources);
  delayed_sources = Qnil;
  staticpro (&loadsearch_re_list);
  loadsearch_re_list = Qnil;

#ifdef WINDOWSNT
  staticpro (&all_loaded_comp_units_h);
  all_loaded_comp_units_h = CALLN (Fmake_hash_table, QCweakness, Qvalue);
#endif

  DEFVAR_LISP ("comp-ctxt", Vcomp_ctxt,
	       doc: /* The compiler context.  */);
  Vcomp_ctxt = Qnil;

  /* FIXME should be initialized but not here...  Plus this don't have
     to be necessarily exposed to lisp but can easy debug for now.  */
  DEFVAR_LISP ("comp-subr-list", Vcomp_subr_list,
	       doc: /* List of all defined subrs.  */);
  DEFVAR_LISP ("comp-abi-hash", Vcomp_abi_hash,
	       doc: /* String signing the ABI exposed to .eln files.  */);
  Vcomp_abi_hash = Qnil;
  DEFVAR_LISP ("comp-native-path-postfix", Vcomp_native_path_postfix,
	       doc: /* Postifix to be added to the .eln compilation path.  */);
  Vcomp_native_path_postfix = Qnil;

  DEFVAR_LISP ("comp-deferred-pending-h", Vcomp_deferred_pending_h,
	       doc: /* Hash table symbol-name -> function-value.  For
		       internal use during  */);
  Vcomp_deferred_pending_h = CALLN (Fmake_hash_table, QCtest, Qeq);

  DEFVAR_LISP ("comp-eln-to-el-h", Vcomp_eln_to_el_h,
	       doc: /* Hash table eln-filename -> el-filename.  */);
  Vcomp_eln_to_el_h = CALLN (Fmake_hash_table, QCtest, Qequal);

  DEFVAR_LISP ("comp-eln-load-path", Vcomp_eln_load_path,
	       doc: /* List of eln cache directories.

If a directory is non absolute is assumed to be relative to
`invocation-directory'.
The last directory of this list is assumed to be the system one.  */);

  /* Temporary value in use for boostrap.  We can't do better as
     `invocation-directory' is still unset, will be fixed up during
     dump reload.  */
  Vcomp_eln_load_path = Fcons (build_string ("../eln-cache/"), Qnil);

#endif /* #ifdef HAVE_NATIVE_COMP */

  defsubr (&Snative_comp_available_p);
}
