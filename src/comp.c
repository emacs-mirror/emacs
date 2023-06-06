/* Compile Emacs Lisp into native code.
   Copyright (C) 2019-2023 Free Software Foundation, Inc.

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
#include "coding.h"
#include "md5.h"
#include "sysstdio.h"
#include "zlib.h"


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
#undef gcc_jit_context_add_command_line_option
#undef gcc_jit_context_add_driver_option
#undef gcc_jit_context_compile_to_file
#undef gcc_jit_context_dump_reproducer_to_file
#undef gcc_jit_context_dump_to_file
#undef gcc_jit_context_get_builtin_function
#undef gcc_jit_context_get_first_error
#undef gcc_jit_context_get_int_type
#undef gcc_jit_context_get_type
#undef gcc_jit_context_new_array_access
#undef gcc_jit_context_new_array_constructor
#undef gcc_jit_context_new_array_type
#undef gcc_jit_context_new_bitcast
#undef gcc_jit_context_new_binary_op
#undef gcc_jit_context_new_call
#undef gcc_jit_context_new_call_through_ptr
#undef gcc_jit_context_new_cast
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
#undef gcc_jit_context_new_struct_constructor
#undef gcc_jit_context_new_struct_type
#undef gcc_jit_context_new_union_constructor
#undef gcc_jit_context_new_unary_op
#undef gcc_jit_context_new_union_type
#undef gcc_jit_context_release
#undef gcc_jit_context_set_bool_option
#undef gcc_jit_context_set_int_option
#undef gcc_jit_context_set_logfile
#undef gcc_jit_context_set_str_option
#undef gcc_jit_function_get_param
#undef gcc_jit_function_new_block
#undef gcc_jit_function_new_local
#undef gcc_jit_global_set_initializer
#undef gcc_jit_global_set_initializer_rvalue
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
#undef gcc_jit_struct_get_field_count
#undef gcc_jit_struct_set_fields
#undef gcc_jit_type_get_aligned
#undef gcc_jit_type_get_const
#undef gcc_jit_type_get_pointer
#undef gcc_jit_type_is_pointer
#undef gcc_jit_type_is_struct
#undef gcc_jit_type_unqualified
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
DEF_DLL_FN (void, gcc_jit_context_add_command_line_option,
            (gcc_jit_context *ctxt, const char *optname));
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
#ifdef LIBGCCJIT_HAVE_CTORS
DEF_DLL_FN (gcc_jit_rvalue *, gcc_jit_context_new_array_constructor,
            (gcc_jit_context * ctxt, gcc_jit_location *loc,
             gcc_jit_type *type, size_t num_values,
             gcc_jit_rvalue **values));
#endif
DEF_DLL_FN (gcc_jit_lvalue *, gcc_jit_context_new_global,
            (gcc_jit_context *ctxt, gcc_jit_location *loc,
             enum gcc_jit_global_kind kind, gcc_jit_type *type,
             const char *name));
DEF_DLL_FN (gcc_jit_lvalue *, gcc_jit_function_new_local,
            (gcc_jit_function *func, gcc_jit_location *loc, gcc_jit_type *type,
             const char *name));
#if defined (LIBGCCJIT_HAVE_gcc_jit_global_set_initializer)
DEF_DLL_FN (gcc_jit_lvalue *, gcc_jit_global_set_initializer,
	    (gcc_jit_lvalue *global, const void *blob, size_t num_bytes));
#endif
#ifdef LIBGCCJIT_HAVE_CTORS
DEF_DLL_FN (gcc_jit_lvalue *, gcc_jit_global_set_initializer_rvalue,
	    (gcc_jit_lvalue *global, gcc_jit_rvalue *init_value));
#endif
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
DEF_DLL_FN (gcc_jit_rvalue *, gcc_jit_context_new_cast,
            (gcc_jit_context * ctxt, gcc_jit_location *loc,
             gcc_jit_rvalue *rvalue, gcc_jit_type *type));
#ifdef LIBGCCJIT_HAVE_gcc_jit_context_new_bitcast
DEF_DLL_FN (gcc_jit_rvalue *, gcc_jit_context_new_bitcast,
            (gcc_jit_context *ctxt, gcc_jit_location *loc,
             gcc_jit_rvalue *rvalue, gcc_jit_type *type));
#endif
DEF_DLL_FN (gcc_jit_rvalue *, gcc_jit_context_new_comparison,
            (gcc_jit_context *ctxt, gcc_jit_location *loc,
             enum gcc_jit_comparison op, gcc_jit_rvalue *a, gcc_jit_rvalue *b));
DEF_DLL_FN (gcc_jit_rvalue *, gcc_jit_context_new_rvalue_from_long,
            (gcc_jit_context *ctxt, gcc_jit_type *numeric_type, long value));
#if LISP_WORDS_ARE_POINTERS
DEF_DLL_FN (gcc_jit_rvalue *, gcc_jit_context_new_rvalue_from_ptr,
            (gcc_jit_context *ctxt, gcc_jit_type *pointer_type, void *value));
#endif
DEF_DLL_FN (gcc_jit_rvalue *, gcc_jit_context_new_string_literal,
            (gcc_jit_context *ctxt, const char *value));
#ifdef LIBGCCJIT_HAVE_CTORS
DEF_DLL_FN (gcc_jit_rvalue *, gcc_jit_context_new_struct_constructor,
            (gcc_jit_context * ctxt, gcc_jit_location *loc,
             gcc_jit_type *type, size_t num_values,
             gcc_jit_field **fields, gcc_jit_rvalue **values));
DEF_DLL_FN (gcc_jit_rvalue *, gcc_jit_context_new_union_constructor,
            (gcc_jit_context * ctxt, gcc_jit_location *loc,
             gcc_jit_type *type, gcc_jit_field *field,
             gcc_jit_rvalue *value));
#endif
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
DEF_DLL_FN (gcc_jit_type *, gcc_jit_type_get_const, (gcc_jit_type *type));
DEF_DLL_FN (gcc_jit_type *, gcc_jit_type_get_pointer, (gcc_jit_type *type));
#ifdef LIBGCCJIT_HAVE_REFLECTION
DEF_DLL_FN (size_t, gcc_jit_struct_get_field_count, (gcc_jit_struct *struct_type));
DEF_DLL_FN (gcc_jit_type *, gcc_jit_type_is_pointer, (gcc_jit_type *type));
DEF_DLL_FN (gcc_jit_struct *, gcc_jit_type_is_struct, (gcc_jit_type *type));
DEF_DLL_FN (gcc_jit_type *, gcc_jit_type_unqualified, (gcc_jit_type *type));
#endif
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
DEF_DLL_FN (void, gcc_jit_context_set_str_option,
	    (gcc_jit_context *ctxt, enum gcc_jit_str_option opt,
	     const char *value));
DEF_DLL_FN (void, gcc_jit_struct_set_fields,
            (gcc_jit_struct *struct_type, gcc_jit_location *loc, int num_fields,
             gcc_jit_field **fields));
#ifdef LIBGCCJIT_HAVE_gcc_jit_type_get_aligned
DEF_DLL_FN (gcc_jit_type *, gcc_jit_type_get_aligned,
	    (gcc_jit_type *type, size_t alignment_in_bytes));
#endif
#if defined (LIBGCCJIT_HAVE_gcc_jit_version)
DEF_DLL_FN (int, gcc_jit_version_major, (void));
DEF_DLL_FN (int, gcc_jit_version_minor, (void));
DEF_DLL_FN (int, gcc_jit_version_patchlevel, (void));
#endif

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
  LOAD_DLL_FN (library, gcc_jit_context_compile_to_file);
  LOAD_DLL_FN (library, gcc_jit_context_dump_reproducer_to_file);
  LOAD_DLL_FN (library, gcc_jit_context_dump_to_file);
  LOAD_DLL_FN (library, gcc_jit_context_get_builtin_function);
  LOAD_DLL_FN (library, gcc_jit_context_get_first_error);
  LOAD_DLL_FN (library, gcc_jit_context_get_int_type);
  LOAD_DLL_FN (library, gcc_jit_context_get_type);
  LOAD_DLL_FN (library, gcc_jit_context_new_array_access);
#ifdef LIBGCCJIT_HAVE_CTORS
  LOAD_DLL_FN (library, gcc_jit_context_new_array_constructor);
#endif
  LOAD_DLL_FN (library, gcc_jit_context_new_array_type);
#ifdef LIBGCCJIT_HAVE_gcc_jit_context_new_bitcast
  LOAD_DLL_FN (library, gcc_jit_context_new_bitcast);
#endif
  LOAD_DLL_FN (library, gcc_jit_context_new_binary_op);
  LOAD_DLL_FN (library, gcc_jit_context_new_call);
  LOAD_DLL_FN (library, gcc_jit_context_new_call_through_ptr);
  LOAD_DLL_FN (library, gcc_jit_context_new_cast);
  LOAD_DLL_FN (library, gcc_jit_context_new_comparison);
  LOAD_DLL_FN (library, gcc_jit_context_new_field);
  LOAD_DLL_FN (library, gcc_jit_context_new_function);
  LOAD_DLL_FN (library, gcc_jit_context_new_function_ptr_type);
  LOAD_DLL_FN (library, gcc_jit_context_new_global);
  LOAD_DLL_FN (library, gcc_jit_context_new_opaque_struct);
  LOAD_DLL_FN (library, gcc_jit_context_new_param);
  LOAD_DLL_FN (library, gcc_jit_context_new_rvalue_from_int);
  LOAD_DLL_FN (library, gcc_jit_context_new_rvalue_from_long);
#if LISP_WORDS_ARE_POINTERS
  LOAD_DLL_FN (library, gcc_jit_context_new_rvalue_from_ptr);
#endif
  LOAD_DLL_FN (library, gcc_jit_context_new_string_literal);
#ifdef LIBGCCJIT_HAVE_CTORS
  LOAD_DLL_FN (library, gcc_jit_context_new_struct_constructor);
#endif
  LOAD_DLL_FN (library, gcc_jit_context_new_struct_type);
#ifdef LIBGCCJIT_HAVE_CTORS
  LOAD_DLL_FN (library, gcc_jit_context_new_union_constructor);
#endif
  LOAD_DLL_FN (library, gcc_jit_context_new_unary_op);
  LOAD_DLL_FN (library, gcc_jit_context_new_union_type);
  LOAD_DLL_FN (library, gcc_jit_context_release);
  LOAD_DLL_FN (library, gcc_jit_context_set_bool_option);
  LOAD_DLL_FN (library, gcc_jit_context_set_int_option);
  LOAD_DLL_FN (library, gcc_jit_context_set_logfile);
  LOAD_DLL_FN (library, gcc_jit_context_set_str_option);
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
#ifdef LIBGCCJIT_HAVE_REFLECTION
  LOAD_DLL_FN (library, gcc_jit_struct_get_field_count);
#endif
  LOAD_DLL_FN (library, gcc_jit_struct_set_fields);
#ifdef LIBGCCJIT_HAVE_gcc_jit_type_get_aligned
  LOAD_DLL_FN (library, gcc_jit_type_get_aligned);
#endif
  LOAD_DLL_FN (library, gcc_jit_type_get_const);
  LOAD_DLL_FN (library, gcc_jit_type_get_pointer);
#ifdef LIBGCCJIT_HAVE_REFLECTION
  LOAD_DLL_FN (library, gcc_jit_type_is_pointer);
  LOAD_DLL_FN (library, gcc_jit_type_is_struct);
  LOAD_DLL_FN (library, gcc_jit_type_unqualified);
#endif
  LOAD_DLL_FN_OPT (library, gcc_jit_context_add_command_line_option);
  LOAD_DLL_FN_OPT (library, gcc_jit_context_add_driver_option);
#if defined (LIBGCCJIT_HAVE_gcc_jit_global_set_initializer)
  LOAD_DLL_FN_OPT (library, gcc_jit_global_set_initializer);
#endif
#ifdef LIBGCCJIT_HAVE_CTORS
  LOAD_DLL_FN (gcc_jit_global_set_initializer_rvalue);
#endif

#if defined (LIBGCCJIT_HAVE_gcc_jit_version)
  LOAD_DLL_FN_OPT (library, gcc_jit_version_major);
  LOAD_DLL_FN_OPT (library, gcc_jit_version_minor);
  LOAD_DLL_FN_OPT (library, gcc_jit_version_patchlevel);
#endif

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
#define gcc_jit_context_add_command_line_option fn_gcc_jit_context_add_command_line_option
#define gcc_jit_context_add_driver_option fn_gcc_jit_context_add_driver_option
#define gcc_jit_context_compile_to_file fn_gcc_jit_context_compile_to_file
#define gcc_jit_context_dump_reproducer_to_file fn_gcc_jit_context_dump_reproducer_to_file
#define gcc_jit_context_dump_to_file fn_gcc_jit_context_dump_to_file
#define gcc_jit_context_get_builtin_function fn_gcc_jit_context_get_builtin_function
#define gcc_jit_context_get_first_error fn_gcc_jit_context_get_first_error
#define gcc_jit_context_get_int_type fn_gcc_jit_context_get_int_type
#define gcc_jit_context_get_type fn_gcc_jit_context_get_type
#define gcc_jit_context_new_array_access fn_gcc_jit_context_new_array_access
#ifdef LIBGCCJIT_HAVE_CTORS
#define gcc_jit_context_new_array_constructor fn_gcc_jit_context_new_array_constructor
#endif
#define gcc_jit_context_new_array_type fn_gcc_jit_context_new_array_type
#ifdef LIBGCCJIT_HAVE_gcc_jit_context_new_bitcast
# define gcc_jit_context_new_bitcast fn_gcc_jit_context_new_bitcast
#endif
#define gcc_jit_context_new_binary_op fn_gcc_jit_context_new_binary_op
#define gcc_jit_context_new_call fn_gcc_jit_context_new_call
#define gcc_jit_context_new_call_through_ptr fn_gcc_jit_context_new_call_through_ptr
#define gcc_jit_context_new_cast fn_gcc_jit_context_new_cast
#define gcc_jit_context_new_comparison fn_gcc_jit_context_new_comparison
#define gcc_jit_context_new_field fn_gcc_jit_context_new_field
#define gcc_jit_context_new_function fn_gcc_jit_context_new_function
#define gcc_jit_context_new_function_ptr_type fn_gcc_jit_context_new_function_ptr_type
#define gcc_jit_context_new_global fn_gcc_jit_context_new_global
#define gcc_jit_context_new_opaque_struct fn_gcc_jit_context_new_opaque_struct
#define gcc_jit_context_new_param fn_gcc_jit_context_new_param
#define gcc_jit_context_new_rvalue_from_int fn_gcc_jit_context_new_rvalue_from_int
#define gcc_jit_context_new_rvalue_from_long fn_gcc_jit_context_new_rvalue_from_long
#if LISP_WORDS_ARE_POINTERS
# define gcc_jit_context_new_rvalue_from_ptr fn_gcc_jit_context_new_rvalue_from_ptr
#endif
#define gcc_jit_context_new_string_literal fn_gcc_jit_context_new_string_literal
#ifdef LIBGCCJIT_HAVE_CTORS
#define gcc_jit_context_new_struct_constructor fn_gcc_jit_context_new_struct_constructor
#endif
#define gcc_jit_context_new_struct_type fn_gcc_jit_context_new_struct_type
#ifdef LIBGCCJIT_HAVE_CTORS
#define gcc_jit_context_new_union_constructor fn_gcc_jit_context_new_union_constructor
#endif
#define gcc_jit_context_new_unary_op fn_gcc_jit_context_new_unary_op
#define gcc_jit_context_new_union_type fn_gcc_jit_context_new_union_type
#define gcc_jit_context_release fn_gcc_jit_context_release
#define gcc_jit_context_set_bool_option fn_gcc_jit_context_set_bool_option
#define gcc_jit_context_set_int_option fn_gcc_jit_context_set_int_option
#define gcc_jit_context_set_logfile fn_gcc_jit_context_set_logfile
#define gcc_jit_context_set_str_option fn_gcc_jit_context_set_str_option
#define gcc_jit_function_get_param fn_gcc_jit_function_get_param
#define gcc_jit_function_new_block fn_gcc_jit_function_new_block
#define gcc_jit_function_new_local fn_gcc_jit_function_new_local
#if defined (LIBGCCJIT_HAVE_gcc_jit_global_set_initializer)
 #define gcc_jit_global_set_initializer fn_gcc_jit_global_set_initializer
#endif
#ifdef LIBGCCJIT_HAVE_CTORS
#define gcc_jit_global_set_initializer_rvalue fn_gcc_jit_global_set_initializer_rvalue
#endif
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
#ifdef LIBGCCJIT_HAVE_REFLECTION
# define gcc_jit_type_is_pointer fn_gcc_jit_type_is_pointer
# define gcc_jit_type_unqualified fn_gcc_jit_type_unqualified
# define gcc_jit_type_is_struct fn_gcc_jit_type_is_struct
# define gcc_jit_struct_get_field_count fn_gcc_jit_struct_get_field_count
#endif
#ifdef LIBGCCJIT_HAVE_gcc_jit_type_get_aligned
# define gcc_jit_type_get_aligned fn_gcc_jit_type_get_aligned
#endif
#define gcc_jit_type_get_const fn_gcc_jit_type_get_const
#define gcc_jit_type_get_pointer fn_gcc_jit_type_get_pointer
#if defined (LIBGCCJIT_HAVE_gcc_jit_version)
 #define gcc_jit_version_major fn_gcc_jit_version_major
 #define gcc_jit_version_minor fn_gcc_jit_version_minor
 #define gcc_jit_version_patchlevel fn_gcc_jit_version_patchlevel
#endif

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



/* Increase this number to force a new Vcomp_abi_hash to be generated.  */
#define ABI_VERSION "5"

/* Length of the hashes used for eln file naming.  */
#define HASH_LENGTH 8

/* C symbols emitted for the load relocation mechanism.  */
#define CURRENT_THREAD_RELOC_SYM "current_thread_reloc"
#define F_SYMBOLS_WITH_POS_ENABLED_RELOC_SYM "f_symbols_with_pos_enabled_reloc"
#define PURE_RELOC_SYM "pure_reloc"

#define DATA_RELOC_SYM "d_reloc"
#define DATA_RELOC_IMPURE_SYM "d_reloc_imp"
#define DATA_RELOC_EPHEMERAL_SYM "d_reloc_eph"

#define FUNC_LINK_TABLE_SYM "freloc_link_table"
#define LINK_TABLE_HASH_SYM "freloc_hash"
#define COMP_UNIT_SYM "comp_unit"

#if USE_COMP_STATIC_LISP_OBJECTS
# define DATA_STATICPRO_SYM "d_staticpro"
# define DATA_EPHEMERAL_SYM "d_ephemeral"
# define HAVE_STATIC_LISP_DATA_SYM "comp_have_static_lisp_data"
#endif
#define TEXT_DATA_RELOC_SYM "text_data_reloc"
#define TEXT_DATA_RELOC_IMPURE_SYM "text_data_reloc_imp"
#define TEXT_DATA_RELOC_EPHEMERAL_SYM "text_data_reloc_eph"

#define TEXT_OPTIM_QLY_SYM "text_optim_qly"
#define TEXT_FDOC_SYM "text_data_fdoc"

#define STR_VALUE(s) #s
#define STR(s) STR_VALUE (s)

#define FIRST(x)				\
  XCAR(x)
#define SECOND(x)				\
  XCAR (XCDR (x))
#define THIRD(x)				\
  XCAR (XCDR (XCDR (x)))

/* Like call0 but stringify and intern.  */
#define CALL0I(fun)				\
  CALLN (Ffuncall, intern_c_string (STR (fun)))

/* Like call1 but stringify and intern.  */
#define CALL1I(fun, arg)				\
  CALLN (Ffuncall, intern_c_string (STR (fun)), arg)

/* Like call2 but stringify and intern.  */
#define CALL2I(fun, arg1, arg2)				\
  CALLN (Ffuncall, intern_c_string (STR (fun)), arg1, arg2)

/* Like call4 but stringify and intern.  */
#define CALL4I(fun, arg1, arg2, arg3, arg4)				\
  CALLN (Ffuncall, intern_c_string (STR (fun)), arg1, arg2, arg3, arg4)

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
#define F_RELOC_MAX_SIZE 1600

typedef struct {
  void *link_table[F_RELOC_MAX_SIZE];
  ptrdiff_t size;
} f_reloc_t;

static f_reloc_t freloc;

#ifndef LIBGCCJIT_HAVE_gcc_jit_context_new_bitcast
# define NUM_CAST_TYPES 15
#endif

typedef struct {
  EMACS_INT len;
  gcc_jit_rvalue *r_val;
} reloc_array_t;

/* C side of the compiler context.  */

typedef struct {
  EMACS_INT speed;
  EMACS_INT debug;
  Lisp_Object compiler_options;
  Lisp_Object driver_options;
  gcc_jit_context *ctxt;
  gcc_jit_type *void_type;
  gcc_jit_type *bool_type;
  gcc_jit_type *unsigned_char_type;
  gcc_jit_type *char_type;
  gcc_jit_type *int_type;
  gcc_jit_type *double_type;
  gcc_jit_type *unsigned_type;
  gcc_jit_type *long_type;
  gcc_jit_type *unsigned_long_type;
  gcc_jit_type *long_long_type;
  gcc_jit_type *unsigned_long_long_type;
  gcc_jit_type *emacs_int_type;
  gcc_jit_type *emacs_uint_type;
  gcc_jit_type *void_ptr_type;
  gcc_jit_type *bool_ptr_type;
  gcc_jit_type *unsigned_char_ptr_type;
  gcc_jit_type *char_ptr_type;
  gcc_jit_type *short_type;
  gcc_jit_type *ptrdiff_type;
  gcc_jit_type *uintptr_type;
  gcc_jit_type *size_t_type;
  gcc_jit_type *lisp_word_type;
  gcc_jit_type *lisp_word_tag_type;
  gcc_jit_type *untagged_ptr_type;
#if USE_COMP_STATIC_LISP_OBJECTS
  gcc_jit_type *bits_word_type;
#endif
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
  gcc_jit_type *lisp_cons_u_type;
  gcc_jit_type *lisp_cons_u_s_type;
  gcc_jit_type *lisp_cons_u_s_u_type;
#if USE_COMP_STATIC_LISP_OBJECTS
  /* struct cons_block */
  gcc_jit_struct *cons_block_s;
  gcc_jit_field *cons_block_conses;
  gcc_jit_field *cons_block_gcmarkbits;
  gcc_jit_field *cons_block_next;
  gcc_jit_type *cons_block_type;
  gcc_jit_type *cons_block_aligned_type;
  gcc_jit_type *cons_block_aligned_ptr_type;
  /* struct float_block */
  gcc_jit_struct *float_block_s;
  gcc_jit_field *float_block_floats;
  gcc_jit_field *float_block_gcmarkbits;
  gcc_jit_field *float_block_next;
  gcc_jit_type *float_block_type;
  gcc_jit_type *float_block_aligned_type;
  gcc_jit_type *float_block_aligned_ptr_type;
#endif
  /* struct Lisp_Symbol_With_Position */
  gcc_jit_rvalue *f_symbols_with_pos_enabled_ref;
  gcc_jit_struct *lisp_symbol_with_position;
  gcc_jit_field *lisp_symbol_with_position_header;
  gcc_jit_field *lisp_symbol_with_position_sym;
  gcc_jit_field *lisp_symbol_with_position_pos;
  gcc_jit_type *lisp_symbol_with_position_type;
  gcc_jit_type *lisp_symbol_with_position_ptr_type;
  gcc_jit_function *get_symbol_with_position;
  gcc_jit_function *symbol_with_pos_sym;
  /* struct interval */
  gcc_jit_struct *interval_s;
  gcc_jit_type *interval_type;
  gcc_jit_type *interval_ptr_type;
  /* struct Lisp_Vector */
  gcc_jit_struct *lisp_vector_s;
  gcc_jit_field *lisp_vector_header;
  gcc_jit_field *lisp_vector_contents;
  gcc_jit_type *lisp_vector_type;
  gcc_jit_type *lisp_vector_gcaligned_type;
  gcc_jit_type *lisp_vector_ptr_type;
  /* struct Lisp_String */
  gcc_jit_struct *lisp_string_s;
  gcc_jit_field *lisp_string_u;
  gcc_jit_field *lisp_string_u_s;
  gcc_jit_field *lisp_string_u_s_size;
  gcc_jit_field *lisp_string_u_s_size_bytes;
  gcc_jit_field *lisp_string_u_s_intervals;
  gcc_jit_field *lisp_string_u_s_data;
  gcc_jit_field *lisp_string_u_next;
  gcc_jit_type *lisp_string_type;
  gcc_jit_type *lisp_string_ptr_type;
  gcc_jit_type *lisp_string_u_type;
  gcc_jit_type *lisp_string_u_s_type;
  /* struct Lisp_Float */
  gcc_jit_struct *lisp_float_s;
  gcc_jit_field *lisp_float_u;
  gcc_jit_field *lisp_float_u_data;
  gcc_jit_field *lisp_float_u_chain;
  gcc_jit_type *lisp_float_type;
  gcc_jit_type *lisp_float_ptr_type;
  gcc_jit_type *lisp_float_u_type;
#if USE_COMP_STATIC_LISP_OBJECTS
  /* struct Lisp_Subr */
  gcc_jit_struct *lisp_subr_s;
  gcc_jit_field *lisp_subr_header;
  gcc_jit_field *lisp_subr_function;
  gcc_jit_field *lisp_subr_min_args;
  gcc_jit_field *lisp_subr_max_args;
  gcc_jit_field *lisp_subr_symbol_name;
  gcc_jit_field *lisp_subr_intspec;
  gcc_jit_field *lisp_subr_intspec_string;
  gcc_jit_field *lisp_subr_intspec_native;
  gcc_jit_field *lisp_subr_command_modes;
  gcc_jit_field *lisp_subr_doc;
  gcc_jit_field *lisp_subr_native_comp_u;
  gcc_jit_field *lisp_subr_native_c_name;
  gcc_jit_field *lisp_subr_lambda_list;
  gcc_jit_field *lisp_subr_type;
  gcc_jit_type *lisp_subr_intspec_type;
  gcc_jit_type *lisp_subr_s_type;
  gcc_jit_type *lisp_subr_s_gcaligned_type;
  /* struct Aligned_Lisp_Subr */
  gcc_jit_type *aligned_lisp_subr_type;
  gcc_jit_type *aligned_lisp_subr_ptr_type;
  gcc_jit_field *aligned_lisp_subr_s;
  gcc_jit_field *aligned_lisp_subr_pvec_subr;
  gcc_jit_type *aligned_lisp_subr_pvec_type;
#endif
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
  Lisp_Object lisp_vector_structs_h; /* h -> Lisp_Vector struct with n members.  */
  /* Other globals.  */
  gcc_jit_rvalue *pure_ptr;
#ifndef LIBGCCJIT_HAVE_gcc_jit_context_new_bitcast
  /* This version of libgccjit has really limited support for casting
     therefore this union will be used for the scope.  */
  gcc_jit_type *cast_union_type;
  gcc_jit_function *cast_functions_from_to[NUM_CAST_TYPES][NUM_CAST_TYPES];
  gcc_jit_function *cast_ptr_to_int;
  gcc_jit_function *cast_int_to_ptr;
  gcc_jit_type *cast_types[NUM_CAST_TYPES];
#endif
  gcc_jit_function *func; /* Current function being compiled.  */
  bool func_has_non_local; /* From comp-func has-non-local slot.  */
  EMACS_INT func_speed; /* From comp-func speed slot.  */
  gcc_jit_block *block;  /* Current basic block being compiled.  */
  gcc_jit_lvalue *scratch; /* Used as scratch slot for some code sequence (switch).  */
  ptrdiff_t frame_size; /* Size of the following array in elements. */
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
  reloc_array_t data_relocs;
  /* Same as before but can't go in pure space. */
  reloc_array_t data_relocs_impure;
  /* Same as before but content does not survive load phase. */
  reloc_array_t data_relocs_ephemeral;

  /* Global structure holding function relocations.  */
  gcc_jit_lvalue *func_relocs;
  gcc_jit_type *func_relocs_ptr_type;
  /* Pointer to this structure local to each function.  */
  gcc_jit_lvalue *func_relocs_local;
  gcc_jit_function *memcpy;

  Lisp_Object d_default_idx;
  Lisp_Object d_impure_idx;
  Lisp_Object d_ephemeral_idx;

#if USE_COMP_STATIC_LISP_OBJECTS
  /* If true, compile lisp constants statically into the eln file.  */
  bool compile_static_data;
  /* A list of
     [cons-block-global-val last-idx [cons-rvalue-initializer * cons_block_size]]
  vectors. */
  Lisp_Object cons_block_list;
  /* Same as above, but for Lisp_Float values.  */
  Lisp_Object float_block_list;
  /* Hash table holding Lisp_Object -> compiled constant rvalue.  */
  Lisp_Object d_default_rvals;
  /* Same as before but can't go in pure space.  */
  Lisp_Object d_impure_rvals;
  /* Same as before but contents dont survive load phase.  */
  Lisp_Object d_ephemeral_rvals;

  gcc_jit_lvalue *d_staticvec_ptr_var;
  ptrdiff_t d_staticvec_entries;

  gcc_jit_lvalue *d_ephemeral_ptr_var;
  ptrdiff_t d_ephemeral_entries;

  ptrdiff_t static_lisp_data_count;
  Lisp_Object static_hash_cons_h;
  /* A list of lvalues that need to be dynamically initialized at load
     time. Each entry is a vector of the form [lvalue lisp_obj alloc_class]. */
  Lisp_Object lisp_consts_init_lvals;
  /* A list of lvalues to Lisp_Vector variables that need to be initialized
     to anonymous lambdas (Aligned_Lisp_Subr) at load time. Each entry is a
     vector of the form [lvalue comp-func] */
  Lisp_Object lambda_init_lvals;
#endif

} comp_t;

static comp_t comp;

static FILE *logfile;

/* This is used for serialized objects by the reload mechanism.  */
typedef struct {
  ptrdiff_t len;
  char data[];
} static_obj_t;

typedef struct {
  reloc_array_t array;
  gcc_jit_rvalue *idx;
} imm_reloc_t;

#if USE_COMP_STATIC_LISP_OBJECTS
/* Represents a JIT compiled const static Lisp object.  */
typedef struct
{
  union
  {
    struct
    {
      gcc_jit_rvalue *init;
      enum Lisp_Type type;
    } with_type;
    gcc_jit_rvalue *lisp_obj;
  } expr;
  enum
  {
    /* A constant initializer expression for the underlying Lisp value
       (struct Lisp_Cons, Lisp_String, Lisp_Vector, etc), with its
       type.  Represented in lisp as (const-p expr_type (type .
       rval)).  */
    COMP_LISP_CONST_INIT_WITH_TYPE = 0,
    /* A Lisp_Object expression, either a self representing form
       (integers, nil), or a tagged pointer to a static
       variable.
       Repersented in lisp as (const-p expr_type lisp-obj).  */
    COMP_LISP_CONST_SELF_REPR = 1,
    COMP_LISP_CONST_VAR = 2
  } expr_type;
  /* True if is this can be emitted with a constant expression
     initializer.  This is usually the case with self representing
     objects, and forms consisting of self representing objects.  To
     see the criteria for an object to be emitted statically, see
     emit_comp_lisp_obj.  */
  bool const_expr_p;
} comp_lisp_const_t;

static gcc_jit_rvalue *
comp_lisp_const_get_lisp_obj_rval (Lisp_Object obj,
				   comp_lisp_const_t expr);
static comp_lisp_const_t emit_comp_lisp_obj (Lisp_Object obj,
					     Lisp_Object alloc_class);
#endif

/*
   Helper functions called by the run-time.
*/

static void helper_unwind_protect (Lisp_Object);
static Lisp_Object helper_unbind_n (Lisp_Object);
static void helper_save_restriction (void);
static bool helper_PSEUDOVECTOR_TYPEP_XUNTAG (Lisp_Object, enum pvec_type);
static struct Lisp_Symbol_With_Pos *
  helper_GET_SYMBOL_WITH_POSITION (Lisp_Object);
#if USE_COMP_STATIC_LISP_OBJECTS
static bool helper_static_comp_object_p (Lisp_Object);
#endif

/* Note: helper_link_table must match the list created by
   `declare_runtime_imported_funcs'.  */
static void *helper_link_table[] = {
  wrong_type_argument,
  helper_PSEUDOVECTOR_TYPEP_XUNTAG,
  pure_write_error,
  push_handler,
  record_unwind_protect_excursion,
  helper_unbind_n,
  helper_save_restriction,
  helper_GET_SYMBOL_WITH_POSITION,
  record_unwind_current_buffer,
  set_internal,
  helper_unwind_protect,
  specbind,
  maybe_gc,
  maybe_quit,
#if USE_COMP_STATIC_LISP_OBJECTS
  helper_static_comp_object_p,
#endif
};


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
  Lisp_Object digest = make_uninit_string (MD5_DIGEST_SIZE * 2);
  md5_buffer (SSDATA (string), SCHARS (string), SSDATA (digest));
  hexbuf_digest (SSDATA (digest), SDATA (digest), MD5_DIGEST_SIZE);

  return Fsubstring (digest, Qnil, make_fixnum (HASH_LENGTH));
}

static Lisp_Object
comp_hash_source_file (Lisp_Object filename)
{
  /* Can't use Finsert_file_contents + Fbuffer_hash as this is called
     by Fcomp_el_to_eln_filename too early during bootstrap.  */
  bool is_gz = suffix_p (filename, ".gz");
#ifndef HAVE_ZLIB
  if (is_gz)
    xsignal2 (Qfile_notify_error,
	      build_string ("Cannot natively compile compressed *.el files without zlib support"),
	      filename);
#endif
  Lisp_Object encoded_filename = ENCODE_FILE (filename);
  FILE *f = emacs_fopen (SSDATA (encoded_filename), is_gz ? "rb" : "r");

  if (!f)
    report_file_error ("Opening source file", filename);

  Lisp_Object digest = make_uninit_string (MD5_DIGEST_SIZE * 2);

#ifdef HAVE_ZLIB
  int res = is_gz
    ? md5_gz_stream (f, SSDATA (digest))
    : md5_stream (f, SSDATA (digest));
#else
  int res = md5_stream (f, SSDATA (digest));
#endif
  fclose (f);

  if (res)
    xsignal2 (Qfile_notify_error, build_string ("hashing failed"), filename);

  hexbuf_digest (SSDATA (digest), SSDATA (digest), MD5_DIGEST_SIZE);

  return Fsubstring (digest, Qnil, make_fixnum (HASH_LENGTH));
}

DEFUN ("comp--subr-signature", Fcomp__subr_signature,
       Scomp__subr_signature, 1, 1, 0,
       doc: /* Support function to hash_native_abi.
For internal use.  */)
  (Lisp_Object subr)
{
  return concat2 (Fsubr_name (subr),
		  Fprin1_to_string (Fsubr_arity (subr), Qnil, Qnil));
}

/* Produce a key hashing Vcomp_subr_list.  */

void
hash_native_abi (void)
{
  /* Check runs once.  */
  eassert (NILP (Vcomp_abi_hash));

#if USE_COMP_STATIC_LISP_OBJECTS
  Lisp_Object builtin_syms = Qnil;
  AUTO_STRING (sep, " ");

  for (ptrdiff_t i = 0; i < ARRAYELTS (lispsym) - 1; i++)
    builtin_syms
      = concat3 (builtin_syms, SYMBOL_NAME (builtin_lisp_symbol (i)),
		 sep);
  builtin_syms
    = concat2 (builtin_syms, SYMBOL_NAME (builtin_lisp_symbol (
			       ARRAYELTS (lispsym) - 1)));
#endif

  Vcomp_abi_hash = comp_hash_string (
    CALLN (Fconcat, build_string (ABI_VERSION),
	   concat3 (Vemacs_version, Vsystem_configuration,
		    Vsystem_configuration_options),
	   Fmapconcat (intern_c_string ("comp--subr-signature"),
		       Vcomp_subr_list, build_string (""))
#if USE_COMP_STATIC_LISP_OBJECTS
	   ,builtin_syms
#endif
	   ));


  Lisp_Object version = Vemacs_version;

#ifdef NS_SELF_CONTAINED
  /* MacOS self contained app bundles do not like having dots in the
     directory names under the Contents/Frameworks directory, so
     convert them to underscores.  */
  version = STRING_MULTIBYTE (Vemacs_version)
    ? make_uninit_multibyte_string (SCHARS (Vemacs_version),
				    SBYTES (Vemacs_version))
    : make_uninit_string (SBYTES (Vemacs_version));

  const unsigned char *from = SDATA (Vemacs_version);
  unsigned char *to = SDATA (version);

  while (from < SDATA (Vemacs_version) + SBYTES (Vemacs_version))
    {
      unsigned char c = *from++;

      if (c == '.')
	c = '_';

      *to++ = c;
    }
#endif

  Vcomp_native_version_dir =
    concat3 (version, build_string ("-"), Vcomp_abi_hash);
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
    xsignal2 (Qnative_ice, build_string ("missing basic block"), block_name);

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

  EMACS_INT slot_n = XFIXNUM (mvar_slot);
  eassert (slot_n < comp.frame_size);
  return comp.frame[slot_n];
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
	    build_string ("can't find data in relocation containers"));
  assume (false);

 found:
  eassert (XFIXNUM (idx) < reloc.array.len);
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
  if (comp.debug && comp.block)
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
    gcc_jit_type_get_const (
      gcc_jit_context_new_function_ptr_type (comp.ctxt,
					     NULL,
					     ret_type,
					     nargs,
					     types,
					     0));
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
      /* Inline functions so far don't have a local variable for
	 function reloc table so we fall back to the global one.  Even
	 if this is not aesthetic calling into C from open-code is
	 always a fallback and therefore not be performance critical.
	 To fix this could think do the inline our-self without
	 relying on GCC. */
      gcc_jit_lvalue *f_ptr =
	gcc_jit_rvalue_dereference_field (
	  gcc_jit_lvalue_as_rvalue (comp.func_relocs_local
				    ? comp.func_relocs_local
				    : comp.func_relocs),
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

#ifndef LIBGCCJIT_HAVE_gcc_jit_context_new_bitcast
static int
type_to_cast_index (gcc_jit_type * type)
{
  for (int i = 0; i < NUM_CAST_TYPES; ++i)
    if (type == comp.cast_types[i])
      return i;

  xsignal1 (Qnative_ice, build_string ("unsupported cast"));
}
#endif

static gcc_jit_rvalue *
emit_coerce (gcc_jit_type *new_type, gcc_jit_rvalue *obj)
{
  gcc_jit_type *old_type
    = gcc_jit_type_unqualified (gcc_jit_rvalue_get_type (obj));

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

#ifdef LIBGCCJIT_HAVE_CTORS
      gcc_jit_rvalue *s
	= gcc_jit_context_new_struct_constructor (comp.ctxt, NULL,
						  comp.lisp_obj_type,
						  1, &comp.lisp_obj_i,
						  &lwordobj);
      return s;
#else /* !LIBGCCJIT_HAVE_CTORS */
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
#endif
    }
#endif

#ifdef LIBGCCJIT_HAVE_gcc_jit_context_new_bitcast
  bool old_is_ptr = gcc_jit_type_is_pointer (old_type) != NULL;
  bool new_is_ptr = gcc_jit_type_is_pointer (new_type) != NULL;

  gcc_jit_rvalue *tmp = obj;

  /* `gcc_jit_context_new_bitcast` requires that the types being converted
     between have the same layout and as such, doesn't allow converting
     between an arbitrarily sized integer/boolean and a pointer. Casting it
     to a uintptr/void* is still necessary, to ensure that it can be bitcast
     into a (void *)/uintptr respectively.  */
  if (old_is_ptr != new_is_ptr)
    {
      if (old_is_ptr)
	{
	  tmp = gcc_jit_context_new_cast (comp.ctxt, NULL, tmp,
					  comp.void_ptr_type);
	  tmp = gcc_jit_context_new_bitcast (comp.ctxt, NULL, tmp,
					     comp.uintptr_type);
	}
      else
	{
	  tmp = gcc_jit_context_new_cast (comp.ctxt, NULL, tmp,
					  comp.uintptr_type);
	  tmp = gcc_jit_context_new_bitcast (comp.ctxt, NULL, tmp,
					     comp.void_ptr_type);
	}
    }
  return gcc_jit_context_new_cast (comp.ctxt, NULL, tmp, new_type);

#else /* !LIBGCCJIT_HAVE_gcc_jit_context_new_bitcast */

  int old_index = type_to_cast_index (old_type);
  int new_index = type_to_cast_index (new_type);

  /* Lookup the appropriate cast function in the cast matrix.  */
  return gcc_jit_context_new_call (comp.ctxt,
				   NULL,
				   comp.cast_functions_from_to
				   [old_index][new_index],
				   1, &obj);
#endif
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
emit_rvalue_from_emacs_uint (EMACS_UINT val)
{
#ifdef WIDE_EMACS_INT
  if (val > ULONG_MAX)
    return emit_rvalue_from_long_long (comp.emacs_uint_type, val);
#endif
  return gcc_jit_context_new_rvalue_from_long (comp.ctxt,
					       comp.emacs_uint_type,
					       val);
}

static gcc_jit_rvalue *
emit_rvalue_from_emacs_int (EMACS_INT val)
{
  if (val > LONG_MAX || val < LONG_MIN)
    return emit_rvalue_from_long_long (comp.emacs_int_type, val);
  else
    return gcc_jit_context_new_rvalue_from_long (comp.ctxt,
						 comp.emacs_int_type, val);
}

static gcc_jit_rvalue *
emit_rvalue_from_lisp_word_tag (Lisp_Word_tag val)
{
#ifdef WIDE_EMACS_INT
  if (val > ULONG_MAX)
    return emit_rvalue_from_long_long (comp.lisp_word_tag_type, val);
#endif
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
  if (val > LONG_MAX || val < LONG_MIN)
    return emit_rvalue_from_long_long (comp.lisp_word_type, val);
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
emit_XVECTOR (gcc_jit_rvalue *o)
{
  emit_comment ("XVECTOR");

  return emit_XUNTAG (o, comp.lisp_vector_gcaligned_type,
                      LISP_WORD_TAG (Lisp_Vectorlike));
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
emit_BASE_EQ (gcc_jit_rvalue *x, gcc_jit_rvalue *y)
{
  emit_comment ("BASE_EQ");

  return gcc_jit_context_new_comparison (
	   comp.ctxt,
	   NULL,
	   GCC_JIT_COMPARISON_EQ,
	   emit_XLI (x),
	   emit_XLI (y));
}

static gcc_jit_rvalue *
emit_AND (gcc_jit_rvalue *x, gcc_jit_rvalue *y)
{
  return gcc_jit_context_new_binary_op (
    comp.ctxt,
    NULL,
    GCC_JIT_BINARY_OP_LOGICAL_AND,
    comp.bool_type,
    x,
    y);
}

static gcc_jit_rvalue *
emit_OR (gcc_jit_rvalue *x, gcc_jit_rvalue *y)
{
  return gcc_jit_context_new_binary_op (
    comp.ctxt,
    NULL,
    GCC_JIT_BINARY_OP_LOGICAL_OR,
    comp.bool_type,
    x,
    y);
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
emit_BARE_SYMBOL_P (gcc_jit_rvalue *obj)
{
  emit_comment ("BARE_SYMBOL_P");

  return gcc_jit_context_new_cast (comp.ctxt,
				   NULL,
				   emit_TAGGEDP (obj, Lisp_Symbol),
				   comp.bool_type);
}

static gcc_jit_rvalue *
emit_SYMBOL_WITH_POS_P (gcc_jit_rvalue *obj)
{
  emit_comment ("SYMBOL_WITH_POS_P");

  gcc_jit_rvalue *args[] =
    { obj,
      gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					   comp.int_type,
					   PVEC_SYMBOL_WITH_POS)
    };

  return gcc_jit_context_new_call (comp.ctxt,
				   NULL,
				   comp.pseudovectorp,
				   2,
				   args);
}

static gcc_jit_rvalue *
emit_SYMBOL_WITH_POS_SYM (gcc_jit_rvalue *obj)
{
  emit_comment ("SYMBOL_WITH_POS_SYM");

  gcc_jit_rvalue *arg [] = { obj };
  return gcc_jit_context_new_call (comp.ctxt,
				   NULL,
				   comp.symbol_with_pos_sym,
				   1,
				   arg);
}

static gcc_jit_rvalue *
emit_EQ (gcc_jit_rvalue *x, gcc_jit_rvalue *y)
{
  return
    emit_OR (
      gcc_jit_context_new_comparison (
        comp.ctxt, NULL,
        GCC_JIT_COMPARISON_EQ,
        emit_XLI (x), emit_XLI (y)),
      emit_AND (
	gcc_jit_lvalue_as_rvalue (
	  gcc_jit_rvalue_dereference (comp.f_symbols_with_pos_enabled_ref,
				      NULL)),
        emit_OR (
          emit_AND (
            emit_SYMBOL_WITH_POS_P (x),
            emit_OR (
              emit_AND (
                emit_SYMBOL_WITH_POS_P (y),
                emit_BASE_EQ (
                  emit_XLI (emit_SYMBOL_WITH_POS_SYM (x)),
                  emit_XLI (emit_SYMBOL_WITH_POS_SYM (y)))),
              emit_AND (
                emit_BARE_SYMBOL_P (y),
                emit_BASE_EQ (
                  emit_XLI (emit_SYMBOL_WITH_POS_SYM (x)),
                  emit_XLI (y))))),
          emit_AND (
            emit_BARE_SYMBOL_P (x),
            emit_AND (
              emit_SYMBOL_WITH_POS_P (y),
              emit_BASE_EQ (
                emit_XLI (x),
                emit_XLI (emit_SYMBOL_WITH_POS_SYM (y))))))));
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

  /* FIXME: Implementation dependent (both RSHIFT are arithmetic).  */

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

  n = emit_binary_op (GCC_JIT_BINARY_OP_PLUS, comp.emacs_uint_type,
		      emit_binary_op (GCC_JIT_BINARY_OP_LSHIFT,
				      comp.emacs_uint_type,
				      comp.lisp_int0,
				      emit_rvalue_from_emacs_uint (
					VALBITS)),
		      n);

  return emit_coerce (comp.lisp_obj_type, n);
}


static gcc_jit_rvalue *
emit_make_fixnum (gcc_jit_rvalue *obj)
{
  emit_comment ("make_fixnum");
  return USE_LSB_TAG ? emit_make_fixnum_LSB_TAG (obj)
		     : emit_make_fixnum_MSB_TAG (obj);
}

#if USE_COMP_STATIC_LISP_OBJECTS

/* Emits a Lisp_Cons struct with the given car and cdr values. */
static gcc_jit_rvalue *
emit_cons_struct (gcc_jit_rvalue *car, gcc_jit_rvalue *cdr)
{
  gcc_jit_rvalue *cons_u_s_u
    = gcc_jit_context_new_union_constructor (comp.ctxt, NULL,
					     comp
					       .lisp_cons_u_s_u_type,
					     comp.lisp_cons_u_s_u_cdr,
					     cdr);

  gcc_jit_field *u_s_fields[]
    = { comp.lisp_cons_u_s_car, comp.lisp_cons_u_s_u };
  gcc_jit_rvalue *u_s_values[] = { car, cons_u_s_u };

  gcc_jit_rvalue *cons_u_s
    = gcc_jit_context_new_struct_constructor (comp.ctxt, NULL,
					      comp.lisp_cons_u_s_type,
					      2, u_s_fields,
					      u_s_values);

  gcc_jit_rvalue *u
    = gcc_jit_context_new_union_constructor (comp.ctxt, NULL,
					     comp.lisp_cons_u_type,
					     comp.lisp_cons_u_s,
					     cons_u_s);
  gcc_jit_rvalue *cons
    = gcc_jit_context_new_struct_constructor (comp.ctxt, NULL,
					      gcc_jit_struct_as_type (
						comp.lisp_cons_s),
					      1, &comp.lisp_cons_u,
					      &u);
  return cons;
}

typedef struct
{
  ptrdiff_t size;
  gcc_jit_field *header;
  gcc_jit_field *contents;
  gcc_jit_type *lisp_vector_type;
  gcc_jit_type *contents_type;
} jit_vector_type_t;

/* Because vectors are implemented as variable length arrays,
 and libgccjit doesn't seem to support them, we need to create a
 new struct type for every Lisp_Vector.  */
static jit_vector_type_t
make_lisp_vector_struct_type (ptrdiff_t n)
{
  Lisp_Object cached;
  jit_vector_type_t vec;
  Lisp_Object lisp_n = make_fixnum (n);

  cached = Fgethash (lisp_n, comp.lisp_vector_structs_h, Qnil);

  if (NILP (cached))
    {
      vec.size = n;
      vec.header
	= gcc_jit_context_new_field (comp.ctxt, NULL,
				     comp.ptrdiff_type, "header");
      vec.contents_type
	= gcc_jit_context_new_array_type (comp.ctxt, NULL,
					  comp.lisp_obj_type,
					  vec.size);
      vec.contents
	= gcc_jit_context_new_field (comp.ctxt, NULL,
				     vec.contents_type, "contents");
      gcc_jit_field *fields[] = { vec.header, vec.contents };

      gcc_jit_struct *lisp_vector_struct
	= gcc_jit_context_new_struct_type (
	  comp.ctxt, NULL,
	  format_string ("comp_Lisp_Vector_%td", vec.size), 2,
	  fields);

      vec.lisp_vector_type
	= gcc_jit_struct_as_type (lisp_vector_struct);

      Lisp_Object entry = CALLN (Fvector, make_mint_ptr (vec.header),
				 make_mint_ptr (vec.contents),
				 make_mint_ptr (vec.lisp_vector_type),
				 make_mint_ptr (vec.contents_type));
      Fputhash (lisp_n, entry, comp.lisp_vector_structs_h);
    }
  else
    {
      vec.size = n;
      vec.header = xmint_pointer (AREF (cached, 0));
      vec.contents = xmint_pointer (AREF (cached, 1));
      vec.lisp_vector_type = xmint_pointer (AREF (cached, 2));
      vec.contents_type = xmint_pointer (AREF (cached, 3));
    }

  return vec;
}

/* Returns whether the given type represents a Lisp_Vector struct.  */
static bool
type_lisp_vector_p (gcc_jit_type *type)
{
  if (NILP (comp.lisp_vector_structs_h))
    return false;

  type = gcc_jit_type_unqualified (type);
  gcc_jit_struct *s = gcc_jit_type_is_struct (type);

  if (s == NULL)
    return false;
  if (gcc_jit_struct_get_field_count (s) != 2)
    return false;

  struct Lisp_Hash_Table *h
    = XHASH_TABLE (comp.lisp_vector_structs_h);

  for (ptrdiff_t i = 0; i < HASH_TABLE_SIZE (h); ++i)
    {
      Lisp_Object k = HASH_KEY (h, i);
      if (!BASE_EQ (k, Qunbound))
	{
	  Lisp_Object val = HASH_VALUE (h, i);
	  if (xmint_pointer (AREF (val, 2)) == type)
	    return true;
	}
    }

  return false;
}

/* Emit a Lisp_String struct rvalue from a given string.  */
static gcc_jit_rvalue *
emit_lisp_string_constructor_rval (Lisp_Object str)
{
  eassert (STRINGP (str));

  gcc_jit_rvalue *str_data;

  /* libgccjit has a bug where creating psuedo-code for a string
     initializer can SIGSEGV if the string contains any format
     strings, so we avoid using gcc_jit_context_new_string_literal
     when debugging options are enabled. Additionally, if the string
     contains a NULL byte, use a regular char array.  */
  if (memchr (SDATA (str), '\0', SBYTES (str)) == NULL && !comp.debug)
    str_data
      = gcc_jit_context_new_string_literal (comp.ctxt, SSDATA (str));
  else
    {
      static ptrdiff_t i;
      ptrdiff_t str_size = SBYTES (str) + 1;

      gcc_jit_type *arr_type
	= gcc_jit_context_new_array_type (comp.ctxt, NULL,
					  comp.unsigned_char_type,
					  str_size);
      gcc_jit_lvalue *var
	= gcc_jit_context_new_global (comp.ctxt, NULL,
				      GCC_JIT_GLOBAL_INTERNAL,
				      arr_type,
				      format_string ("str_data_%td",
						     i++));
      gcc_jit_global_set_initializer (var, SDATA (str), str_size);
      str_data = gcc_jit_lvalue_get_address (var, NULL);
    }

  gcc_jit_rvalue *size_bytes
    = STRING_MULTIBYTE (str)
      ? gcc_jit_context_new_rvalue_from_long (comp.ctxt,
					      comp.ptrdiff_type,
					      SBYTES (str))
      // Mark unibyte strings as immovable, so that pin_string does
      // not attempt to modify them.
      : gcc_jit_context_new_rvalue_from_long (comp.ctxt,
					      comp.ptrdiff_type, -3);
  // Perma-mark all string constants, which lets us declare them as
  // constants.
  gcc_jit_rvalue *size
    = gcc_jit_context_new_rvalue_from_long (comp.ctxt,
					    comp.ptrdiff_type,
					    SCHARS (str)
					      | ARRAY_MARK_FLAG);
  gcc_jit_field *u_s_fields[]
    = { comp.lisp_string_u_s_size, comp.lisp_string_u_s_size_bytes,
	comp.lisp_string_u_s_intervals, comp.lisp_string_u_s_data };
  gcc_jit_rvalue *u_s_values[] = {
    size,
    size_bytes,
    gcc_jit_context_null (comp.ctxt, comp.interval_ptr_type),
    gcc_jit_context_new_cast (comp.ctxt, NULL,
			      str_data,
			      comp.unsigned_char_ptr_type),
  };
  gcc_jit_rvalue *u_s
    = gcc_jit_context_new_struct_constructor (comp.ctxt, NULL,
					      comp
						.lisp_string_u_s_type,
					      4, u_s_fields,
					      u_s_values);
  gcc_jit_rvalue *u
    = gcc_jit_context_new_union_constructor (comp.ctxt, NULL,
					     comp.lisp_string_u_type,
					     comp.lisp_string_u_s,
					     u_s);
  gcc_jit_rvalue *s
    = gcc_jit_context_new_struct_constructor (comp.ctxt, NULL,
					      comp.lisp_string_type,
					      1, &comp.lisp_string_u,
					      &u);

  return s;
}

static gcc_jit_rvalue *
emit_lisp_float_constructor_rval (Lisp_Object f)
{
  eassert (FLOATP (f));

  gcc_jit_rvalue *u = gcc_jit_context_new_union_constructor (
    comp.ctxt, NULL, comp.lisp_float_u_type, comp.lisp_float_u_data,
    gcc_jit_context_new_rvalue_from_double (comp.ctxt,
					    comp.double_type,
					    XFLOAT (f)->u.data));
  gcc_jit_rvalue *float_s
    = gcc_jit_context_new_struct_constructor (comp.ctxt, NULL,
					      comp.lisp_float_type, 1,
					      &comp.lisp_float_u, &u);
  return float_s;
}

static bool
comp_func_l_p (Lisp_Object func)
{
  return !NILP (CALL1I (comp-func-l-p, func));
}

static bool
comp_func_d_p (Lisp_Object func)
{
  return !NILP (CALL1I (comp-func-d-p, func));
}

static gcc_jit_rvalue *emit_lisp_obj_rval (Lisp_Object obj);

static gcc_jit_rvalue *emit_mvar_rval (Lisp_Object mvar);

/* Return the docstring index for the given function.  */
static EMACS_INT
get_comp_func_doc_idx (Lisp_Object func)
{
  Lisp_Object func_doc = CALL1I (comp-func-doc, func);
  Lisp_Object docs = CALL1I (comp-ctxt-function-docs, Vcomp_ctxt);
  eassert (VECTORP (docs));

  for (ptrdiff_t i = 0; i < ASIZE (docs); i++)
    {
      Lisp_Object el = AREF (docs, i);
      if (!NILP (Fstring_equal (el, func_doc)))
	return i;
    }

  xsignal2 (Qnative_ice,
	    build_string (
	      "could not find documentation index for function"),
	    CALL1I (comp-func-c-name, func));
}

static gcc_jit_rvalue *
emit_aligned_lisp_subr_constructor_rval (
  const char *symbol_name, gcc_jit_rvalue *native_comp_u,
  Lisp_Object func, bool *const_p)
{
  bool is_const = true;
  gcc_jit_rvalue *sym_name
    = gcc_jit_context_new_string_literal (comp.ctxt, symbol_name);
  Lisp_Object c_name_l = CALL1I (comp-func-c-name, func);

  short minargs;
  short maxargs;
  gcc_jit_rvalue *lambda_list = NULL;

  if (comp_func_l_p (func))
    {
      Lisp_Object args = CALL1I (comp-func-l-args, func);
      minargs = XFIXNUM (CALL1I (comp-args-base-min, args));
      if (NILP (CALL1I (comp-args-p, args)))
	maxargs = MANY;
      else
	maxargs = XFIXNUM (CALL1I (comp-args-max, args));
      lambda_list = emit_rvalue_from_lisp_obj (Qnil);
    }
  else if (comp_func_d_p (func))
    {
      Lisp_Object args
	= Ffunc_arity (CALL1I (comp-func-byte-func, func));
      minargs = XFIXNUM (XCAR (args));

      if (FIXNUMP (XCDR (args)))
	maxargs = XFIXNUM (XCDR (args));
      else
	maxargs = MANY;

      Lisp_Object l = CALL1I (comp-func-d-lambda-list, func);
      comp_lisp_const_t expr = emit_comp_lisp_obj (l, Qd_default);
      is_const &= expr.const_expr_p;
      lambda_list = comp_lisp_const_get_lisp_obj_rval (l, expr);
    }
  else
    xsignal2 (Qnative_ice, build_string ("invalid function"), func);

  eassert (lambda_list != NULL);

  gcc_jit_rvalue *type
    = emit_mvar_rval (CALL1I (comp-func-type, func));
  gcc_jit_rvalue *doc
    = gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					   comp.emacs_int_type,
					   get_comp_func_doc_idx (
					     func));
  Lisp_Object int_spec_l = CALL1I (comp-func-int-spec, func);
  comp_lisp_const_t intspec_expr
    = emit_comp_lisp_obj (int_spec_l, Qd_default);
  is_const &= intspec_expr.const_expr_p;
  gcc_jit_rvalue *intspec = gcc_jit_context_new_union_constructor (
    comp.ctxt, NULL, comp.lisp_subr_intspec_type,
    comp.lisp_subr_intspec_native,
    comp_lisp_const_get_lisp_obj_rval (int_spec_l, intspec_expr));

  Lisp_Object command_modes_l
    = CALL1I (comp-func-command-modes, func);
  comp_lisp_const_t command_modes_expr
    = emit_comp_lisp_obj (command_modes_l, Qd_default);
  is_const &= command_modes_expr.const_expr_p;
  gcc_jit_rvalue *command_modes
    = comp_lisp_const_get_lisp_obj_rval (command_modes_l,
					 command_modes_expr);

  Lisp_Object gcc_func
    = Fgethash (c_name_l, comp.exported_funcs_h, Qnil);
  if (NILP (gcc_func))
    xsignal2 (Qnative_ice, build_string ("missing function"),
	      gcc_func);
  gcc_jit_rvalue *function = gcc_jit_context_new_cast (
    comp.ctxt, NULL,
    gcc_jit_function_get_address (xmint_pointer (gcc_func), NULL),
    comp.void_ptr_type);
  ptrdiff_t header_val
    = (PVEC_SUBR << PSEUDOVECTOR_AREA_BITS) | PSEUDOVECTOR_FLAG;
  gcc_jit_rvalue *header
    = gcc_jit_context_new_rvalue_from_long (comp.ctxt,
					    comp.ptrdiff_type,
					    header_val);

  gcc_jit_rvalue *values[] = {
    header,
    function,
    gcc_jit_context_new_rvalue_from_int (comp.ctxt, comp.short_type,
					 minargs),
    gcc_jit_context_new_rvalue_from_int (comp.ctxt, comp.short_type,
					 maxargs),
    sym_name,
    intspec,
    command_modes,
    doc,
    native_comp_u,
    gcc_jit_context_new_string_literal (comp.ctxt, SSDATA (c_name_l)),
    lambda_list,
    type
  };

  gcc_jit_field *fields[]
    = { comp.lisp_subr_header,	      comp.lisp_subr_function,
	comp.lisp_subr_min_args,      comp.lisp_subr_max_args,
	comp.lisp_subr_symbol_name,   comp.lisp_subr_intspec,
	comp.lisp_subr_command_modes, comp.lisp_subr_doc,
	comp.lisp_subr_native_comp_u, comp.lisp_subr_native_c_name,
	comp.lisp_subr_lambda_list,   comp.lisp_subr_type };

  gcc_jit_rvalue *subr
    = gcc_jit_context_new_struct_constructor (comp.ctxt, NULL,
					      comp.lisp_subr_s_type,
					      12, fields, values);
  *const_p = is_const;
  return gcc_jit_context_new_union_constructor (
    comp.ctxt, NULL, comp.aligned_lisp_subr_type,
    comp.aligned_lisp_subr_s, subr);
}

static gcc_jit_rvalue *
emit_lisp_obj_static_rval (Lisp_Object obj)
{
  Lisp_Object ptr;

  ptr = Fgethash (obj, comp.d_default_rvals, Qnil);
  if (!NILP (ptr))
    return xmint_pointer (ptr);

  ptr = Fgethash (obj, comp.d_impure_rvals, Qnil);
  if (!NILP (ptr))
    return xmint_pointer (ptr);

  ptr = Fgethash (obj, comp.d_ephemeral_rvals, Qnil);
  if (!NILP (ptr))
    return xmint_pointer (ptr);

  xsignal1 (Qnative_ice,
	    build_string ("cant't find static data in relocation containers"));
}
#endif


static gcc_jit_rvalue *emit_make_lisp_ptr (gcc_jit_rvalue *ptr,
					   enum Lisp_Type type);
static gcc_jit_lvalue *
emit_lval_access_cons_car (gcc_jit_lvalue *cons);
static gcc_jit_lvalue *
emit_lval_access_cons_cdr (gcc_jit_lvalue *cons);

#define INIT_EXPR_CONST_P_IDX (make_fixnum (0))
#define INIT_EXPR_TYPE_IDX (make_fixnum (1))
#define INIT_EXPR_EXPR_IDX (make_fixnum (2))

static comp_lisp_const_t
init_expr_from_lisp (Lisp_Object entry)
{
  eassert (CONSP (entry));
  eassert (list_length (entry) == 3);

  comp_lisp_const_t expr;
  expr.const_expr_p = !NILP (Fnth (INIT_EXPR_CONST_P_IDX, entry));
  Lisp_Object expr_type = Fnth (INIT_EXPR_TYPE_IDX, entry);
  eassert (SYMBOLP (expr_type));

  Lisp_Object lexpr = Fnth (INIT_EXPR_EXPR_IDX, entry);

  if (EQ (expr_type, Qinit_expr_type_val))
    {
      eassert (CONSP (lexpr));

      Lisp_Object pred = XCAR (lexpr);
      eassert (SYMBOLP (pred));

      enum Lisp_Type type;
      if (EQ (pred, Qstring))
	type = Lisp_String;
      else if (EQ (pred, Qvector))
	type = Lisp_Vectorlike;
      else if (EQ (pred, Qcons))
	type = Lisp_Cons;
      else if (EQ (pred, Qfloat))
	type = Lisp_Float;
      else if (EQ (pred, Qsymbol))
	type = Lisp_Symbol;
      else
	emacs_abort ();

      expr.expr.with_type.type = type;
      expr.expr.with_type.init = xmint_pointer (XCDR (lexpr));
      expr.expr_type = COMP_LISP_CONST_INIT_WITH_TYPE;
    }
  else if (EQ (expr_type, Qinit_expr_type_self_repr))
    {
      eassert (expr.const_expr_p);
      expr.expr.lisp_obj = xmint_pointer (lexpr);
      expr.expr_type = COMP_LISP_CONST_SELF_REPR;
    }
  else if (EQ (expr_type, Qinit_expr_type_var))
    {
      expr.expr.lisp_obj = xmint_pointer (lexpr);
      expr.expr_type = COMP_LISP_CONST_VAR;
    }

  return expr;
}

static Lisp_Object
init_expr_to_lisp (comp_lisp_const_t expr)
{
  switch (expr.expr_type)
    {
    case COMP_LISP_CONST_INIT_WITH_TYPE:
      eassert (expr.expr.with_type.init != NULL);
      eassert (expr.const_expr_p);

      Lisp_Object pred;
      switch (expr.expr.with_type.type)
	{
	case Lisp_String:
	  pred = Qstring;
	  break;
	case Lisp_Vectorlike:
	  pred = Qvector;
	  break;
	case Lisp_Cons:
	  pred = Qcons;
	  break;
	case Lisp_Float:
	  pred = Qfloat;
	  break;
	case Lisp_Symbol:
	  pred = Qsymbol;
	  break;
	default:
	  emacs_abort ();
	}

      return list3 (Qt, Qinit_expr_type_val,
		    Fcons (pred,
			   make_mint_ptr (expr.expr.with_type.init)));
    case COMP_LISP_CONST_SELF_REPR:
      eassert (expr.const_expr_p);
      eassert (expr.expr.lisp_obj != NULL);

      return list3 (Qt, Qinit_expr_type_self_repr,
		    make_mint_ptr (expr.expr.lisp_obj));
    case COMP_LISP_CONST_VAR:
      eassert (expr.expr.lisp_obj != NULL);

      return list3 (expr.const_expr_p ? Qt : Qnil,
		    Qinit_expr_type_var,
		    make_mint_ptr (expr.expr.lisp_obj));
    default:
      emacs_abort ();
    }
}

/* Export a global Lisp_Object constant variable with the the provided
   name and value.  */
static gcc_jit_lvalue *
emit_export_const_lisp_obj_var (const char *name, gcc_jit_rvalue *val)
{
  gcc_jit_lvalue *global
    = gcc_jit_context_new_global (comp.ctxt, NULL,
					   GCC_JIT_GLOBAL_EXPORTED,
					   gcc_jit_type_get_const (
					     comp.lisp_obj_type),
					   name);
  gcc_jit_global_set_initializer_rvalue (global, val);
  return global;
}

static void
alloc_class_check (Lisp_Object alloc_class)
{
  if ((EQ (alloc_class, Qd_default)
       || EQ (alloc_class, Qd_impure)
       || EQ (alloc_class, Qd_ephemeral)))
    return;

  xsignal2 (Qnative_ice,
	    build_string ("invalid lisp data allocation class"),
	    alloc_class);
}

static gcc_jit_lvalue *
emit_static_lisp_obj_var (Lisp_Object alloc_class)
{
  alloc_class_check (alloc_class);
  gcc_jit_rvalue *array;
  ptrdiff_t idx;

  if (EQ (alloc_class, Qd_ephemeral))
    {
      array = gcc_jit_lvalue_as_rvalue (comp.d_ephemeral_ptr_var);
      idx = comp.d_ephemeral_entries++;
    }
  else
    {
      array = gcc_jit_lvalue_as_rvalue (comp.d_staticvec_ptr_var);
      idx = comp.d_staticvec_entries++;
    }

  return gcc_jit_context_new_array_access (
    comp.ctxt, NULL, array,
    gcc_jit_context_new_rvalue_from_int (comp.ctxt, comp.ptrdiff_type,
					 idx));
}

static Lisp_Object
push_cons_block (void)
{
  char *name
    = format_string ("cons_block_%ld",
		     XFIXNUM (Flength (comp.cons_block_list)));
  gcc_jit_lvalue *var
    = gcc_jit_context_new_global (comp.ctxt, NULL,
				  GCC_JIT_GLOBAL_INTERNAL,
				  comp.cons_block_aligned_type,
				  name);
  Lisp_Object entry
    = CALLN (Fvector, make_mint_ptr (var), make_fixnum (-1),
	     Fmake_vector (make_fixnum (cons_block_conses_length),
			   Qnil));
  comp.cons_block_list = Fcons (entry, comp.cons_block_list);
  return entry;
}

static Lisp_Object
push_float_block (void)
{
  char *name
    = format_string ("float_block_%ld",
		     XFIXNUM (Flength (comp.float_block_list)));
  gcc_jit_lvalue *var
    = gcc_jit_context_new_global (comp.ctxt, NULL,
				  GCC_JIT_GLOBAL_INTERNAL,
				  comp.float_block_aligned_type,
				  name);
  Lisp_Object entry
    = CALLN (Fvector, make_mint_ptr (var), make_fixnum (-1),
	     Fmake_vector (make_fixnum (float_block_floats_length),
			   Qnil));
  comp.float_block_list = Fcons (entry, comp.float_block_list);
  return entry;
}

static gcc_jit_lvalue *
alloc_block_var (Lisp_Object block)
{
  return xmint_pointer (AREF (block, 0));
}

static ptrdiff_t
alloc_block_last_idx (Lisp_Object block)
{
  return XFIXNUM (AREF (block, 1));
}

static void
alloc_block_set_last_idx (Lisp_Object block, ptrdiff_t idx)
{
  ASET (block, 1, make_fixnum (idx));
}

static void
alloc_block_put_cons (Lisp_Object block, gcc_jit_rvalue *init_rval,
		     ptrdiff_t idx)
{
  ASET (AREF (block, 2), idx, make_mint_ptr (init_rval));
}


static gcc_jit_rvalue *
cons_block_emit_constructor (Lisp_Object block)
{
  USE_SAFE_ALLOCA;
  ptrdiff_t last_idx = alloc_block_last_idx (block);
  Lisp_Object conses_vec = AREF (block, 2);
  ptrdiff_t conses_n = last_idx + 1;
  gcc_jit_rvalue **conses;

  SAFE_NALLOCA (conses, 1,  conses_n);
  for (ptrdiff_t i = 0; i < conses_n; i++)
    conses[i] = xmint_pointer (AREF (conses_vec, i));

  gcc_jit_rvalue **gcmarkbits;
  SAFE_NALLOCA (gcmarkbits, 1, cons_block_gcmarkbits_length);
  for (ptrdiff_t i = 0; i < cons_block_gcmarkbits_length; i++)
    gcmarkbits[i]
      = gcc_jit_context_new_rvalue_from_long (comp.ctxt,
					      comp.ptrdiff_type,
					      BITS_WORD_MAX);

  gcc_jit_field *fields[] = {
    comp.cons_block_conses,
    comp.cons_block_gcmarkbits,
    comp.cons_block_next,
  };

  gcc_jit_rvalue *values[] = {
    gcc_jit_context_new_array_constructor (
      comp.ctxt, NULL,
      gcc_jit_context_new_array_type (comp.ctxt, NULL,
				      comp.lisp_cons_type,
				      cons_block_conses_length),
      conses_n, conses),
    gcc_jit_context_new_array_constructor (
      comp.ctxt, NULL,
      gcc_jit_context_new_array_type (comp.ctxt, NULL,
				      comp.bits_word_type,
				      cons_block_gcmarkbits_length),
      cons_block_gcmarkbits_length, gcmarkbits),
    gcc_jit_context_null (comp.ctxt,
			  comp.cons_block_aligned_ptr_type),
  };

  gcc_jit_rvalue *value
    = gcc_jit_context_new_struct_constructor (comp.ctxt, NULL,
					      comp.cons_block_type, 3,
					      fields, values);
  SAFE_FREE();
  return value;
}

static gcc_jit_rvalue *
float_block_emit_constructor (Lisp_Object block)
{
  USE_SAFE_ALLOCA;
  ptrdiff_t last_idx = alloc_block_last_idx (block);
  Lisp_Object floats_vec = AREF (block, 2);
  ptrdiff_t floats_n = last_idx + 1;
  gcc_jit_rvalue **floats;

  SAFE_NALLOCA (floats, 1,  floats_n);
  for (ptrdiff_t i = 0; i < floats_n; i++)
    floats[i] = xmint_pointer (AREF (floats_vec, i));

  gcc_jit_rvalue **gcmarkbits;
  SAFE_NALLOCA (gcmarkbits, 1, float_block_gcmarkbits_length);
  for (ptrdiff_t i = 0; i < float_block_gcmarkbits_length; i++)
    gcmarkbits[i]
      = gcc_jit_context_new_rvalue_from_long (comp.ctxt,
					      comp.ptrdiff_type,
					      BITS_WORD_MAX);

  gcc_jit_field *fields[] = {
    comp.float_block_floats,
    comp.float_block_gcmarkbits,
    comp.float_block_next,
  };

  gcc_jit_rvalue *values[] = {
    gcc_jit_context_new_array_constructor (
      comp.ctxt, NULL,
      gcc_jit_context_new_array_type (comp.ctxt, NULL,
				      comp.lisp_float_type,
				      float_block_floats_length),
      floats_n, floats),
    gcc_jit_context_new_array_constructor (
      comp.ctxt, NULL,
      gcc_jit_context_new_array_type (comp.ctxt, NULL,
				      comp.bits_word_type,
				      float_block_gcmarkbits_length),
      float_block_gcmarkbits_length, gcmarkbits),
    gcc_jit_context_null (comp.ctxt,
			  comp.float_block_aligned_ptr_type),
  };

  gcc_jit_rvalue *value
    = gcc_jit_context_new_struct_constructor (comp.ctxt, NULL,
					      comp.float_block_type, 3,
					      fields, values);
  SAFE_FREE();
  return value;
}

typedef struct
{
  ptrdiff_t cons_block_list_idx;
  ptrdiff_t cons_block_conses_idx;
} cons_block_entry_t;

static Lisp_Object
cons_block_list_get_block_entry (cons_block_entry_t entry)
{
  ptrdiff_t list_idx = (XFIXNUM (Flength (comp.cons_block_list)) - 1
			- entry.cons_block_list_idx);
  return Fnth (make_fixnum (list_idx), comp.cons_block_list);
}

static void
cons_block_entry_set_init_rval (cons_block_entry_t entry, gcc_jit_rvalue *init)
{
  Lisp_Object block = cons_block_list_get_block_entry (entry);
  alloc_block_put_cons (block, init, entry.cons_block_conses_idx);
}

static gcc_jit_lvalue *
cons_block_entry_emit_cons_lval (cons_block_entry_t entry)
{
  Lisp_Object block = cons_block_list_get_block_entry (entry);
  gcc_jit_lvalue *var = alloc_block_var (block);
  gcc_jit_lvalue *conses
    = gcc_jit_lvalue_access_field (var, NULL, comp.cons_block_conses);
  return gcc_jit_context_new_array_access (
    comp.ctxt, NULL, gcc_jit_lvalue_as_rvalue (conses),
    gcc_jit_context_new_rvalue_from_int (comp.ctxt, comp.ptrdiff_type,
					 entry.cons_block_conses_idx));
}

static cons_block_entry_t
cons_block_new_cons (gcc_jit_rvalue *init_val)
{
  Lisp_Object block;
  if (NILP (comp.cons_block_list))
    block = push_cons_block ();
  else
    block = XCAR (comp.cons_block_list);

  ptrdiff_t idx = alloc_block_last_idx (block);
  if (++idx == cons_block_conses_length)
    {
      block = push_cons_block ();
      eassert (alloc_block_last_idx (block) == -1);
      idx = 0;
    }

  alloc_block_set_last_idx (block, idx);

  if (init_val != NULL)
    alloc_block_put_cons (block, init_val, idx);

  ptrdiff_t list_idx = XFIXNUM (Flength (comp.cons_block_list)) - 1;

  cons_block_entry_t entry
    = { .cons_block_list_idx = list_idx, .cons_block_conses_idx = idx };
  return entry;
}

static gcc_jit_lvalue *
float_block_new_float (gcc_jit_rvalue *init_val)
{
  eassert (init_val != NULL);

  Lisp_Object block;
  if (NILP (comp.float_block_list))
    block = push_float_block ();
  else
    block = XCAR (comp.float_block_list);

  ptrdiff_t idx = alloc_block_last_idx (block);
  if (++idx == float_block_floats_length)
    {
      block = push_float_block ();
      eassert (alloc_block_last_idx (block) == -1);
    }

  alloc_block_set_last_idx (block, idx);
  alloc_block_put_cons (block, init_val, idx);

  gcc_jit_lvalue *var = alloc_block_var (block);
  gcc_jit_lvalue *floats
    = gcc_jit_lvalue_access_field (var, NULL,
				   comp.float_block_floats);
  return gcc_jit_context_new_array_access (
    comp.ctxt, NULL, gcc_jit_lvalue_as_rvalue (floats),
    gcc_jit_context_new_rvalue_from_int (comp.ctxt, comp.ptrdiff_type,
					 idx));
}

static gcc_jit_lvalue *
emit_lisp_data_var (gcc_jit_type *type, enum gcc_jit_global_kind kind)
{
  Lisp_Object final_name;
  AUTO_STRING (lisp_format, "lisp_data_%d");

  final_name = CALLN (Fformat, lisp_format,
		      make_fixnum (comp.static_lisp_data_count++));
  return gcc_jit_context_new_global (comp.ctxt, NULL,
				     kind, type,
				     SSDATA (final_name));
}

/* Get the actual Lisp_Object rvalue for a given compiled constant.
   For types COMP_LISP_CONST_VAR and COMP_LISP_CONST_SELF_REPR, this
   does nothing but return the lisp_obj field.
   For COMP_LISP_CONST_INIT_WITH_TYPE values, create a global constant
   static to store the underlying data and return a pointer to it,
   tagged with the corresponding Lisp_Type value.
*/
static gcc_jit_rvalue *
comp_lisp_const_get_lisp_obj_rval (Lisp_Object lobj,
				   comp_lisp_const_t expr)
{
  if (expr.expr_type == COMP_LISP_CONST_SELF_REPR
      || expr.expr_type == COMP_LISP_CONST_VAR)
    return expr.expr.lisp_obj;

  eassert (expr.const_expr_p);

  gcc_jit_type *type = gcc_jit_type_unqualified (
    gcc_jit_rvalue_get_type (expr.expr.with_type.init));

  gcc_jit_rvalue *ptr = NULL;

  if (type == comp.lisp_cons_type)
    {
      eassert (expr.expr.with_type.type == Lisp_Cons);
      cons_block_entry_t entry
	= cons_block_new_cons (expr.expr.with_type.init);
      ptr = gcc_jit_lvalue_get_address (
	cons_block_entry_emit_cons_lval (entry), NULL);
    }
  else if (type == comp.lisp_float_type)
    {
      eassert (expr.expr.with_type.type == Lisp_Float);
      ptr = gcc_jit_lvalue_get_address (float_block_new_float (
					  expr.expr.with_type.init),
					NULL);
    }
  else
    {
      if (type_lisp_vector_p (type))
	type = gcc_jit_type_get_aligned (type, GCALIGNMENT);
      type = gcc_jit_type_get_const (type);

      gcc_jit_lvalue *var
	= emit_lisp_data_var (type, GCC_JIT_GLOBAL_INTERNAL);
      if (gcc_jit_lvalue_get_alignment (var) % GCALIGNMENT != 0)
	xsignal1 (Qnative_ice,
		  build_string ("misaligned lisp data variable"));

      gcc_jit_global_set_initializer_rvalue (var, expr.expr.with_type
						    .init);
      ptr = gcc_jit_lvalue_get_address (var, NULL);
    }

  gcc_jit_rvalue *obj
    = emit_make_lisp_ptr (ptr, expr.expr.with_type.type);

  comp_lisp_const_t new = { .const_expr_p = true,
			    .expr_type = COMP_LISP_CONST_VAR,
			    .expr.lisp_obj = obj };
  Fputhash (lobj, init_expr_to_lisp (new), comp.static_hash_cons_h);
  return obj;
}

static void
add_static_initializer_lisp (gcc_jit_lvalue *accessor,
			     Lisp_Object obj, Lisp_Object alloc_class)
{
  alloc_class_check (alloc_class);
  Lisp_Object entry
    = CALLN (Fvector, make_mint_ptr (accessor), obj, alloc_class);
  comp.lisp_consts_init_lvals
    = Fcons (entry, comp.lisp_consts_init_lvals);
}

static void
add_lisp_const_lvalue_init_rval (gcc_jit_lvalue *accessor,
				 gcc_jit_rvalue *init,
				 Lisp_Object alloc_class)
{
  add_static_initializer_lisp (accessor, make_mint_ptr (init),
			       alloc_class);
}

static gcc_jit_lvalue *
lisp_const_init_lvalue (Lisp_Object l)
{
  return xmint_pointer (AREF (l, 0));
}

static Lisp_Object
lisp_const_init_obj (Lisp_Object l)
{
  return AREF (l, 1);
}

static Lisp_Object
lisp_const_init_alloc_class (Lisp_Object l)
{
  return AREF (l, 2);
}

static comp_lisp_const_t
emit_lambda_subr (Lisp_Object obj, Lisp_Object func)
{
  comp_lisp_const_t expr;

  gcc_jit_lvalue *subr_var
    = emit_lisp_data_var (comp.aligned_lisp_subr_pvec_type,
			  GCC_JIT_GLOBAL_INTERNAL);
  comp.lambda_init_lvals
    = Fcons (CALLN (Fvector, make_mint_ptr (subr_var), func),
	     comp.lambda_init_lvals);
  expr.const_expr_p = false;
  expr.expr_type = COMP_LISP_CONST_VAR;
  expr.expr.lisp_obj
    = emit_make_lisp_ptr (gcc_jit_lvalue_get_address (subr_var, NULL),
			  Lisp_Vectorlike);

  return expr;
}


/* Returns whether the provided Lisp_Object can be statically
   constructed during compilation.  */
static bool
lisp_object_constructed_p (Lisp_Object obj)
{
  return FIXNUMP (obj)
	 || (BARE_SYMBOL_P (obj) && c_symbol_p (XBARE_SYMBOL (obj)))
	 || (STRINGP (obj) && XSTRING (obj)->u.s.intervals == NULL)
	 || FLOATP (obj)
	 || (VECTORP (obj) || RECORDP (obj) || COMPILEDP (obj))
	 || CONSP (obj);
}

/* Recursively create a comp_lisp_const_t for the given Lisp_Object
   obj, with the given allocation class. We are able to statically
   construct constant rvalues for the following objects:

   * Fixnums (they're self-evaluating, so its trivial)
   * Builtin bare symbols in lispsym (also self-evaluting)
   * Strings without any intervals
   * Floats
   * Vectors, records and bytecode vectors
   * Conses

   For other kinds of object, emit_comp_lisp_obj will simply create a
   static Lisp_Object global, and set it up to be initialized in
   `comp_init_objs`.  Conses and (pseudo)vectors containing them are
   still statically initialized, but with space left for them for
   `comp_init_objs` to fill.  For instance, a form like
   '#[1 non-c-symbol "foo"]' is compiled as:

   static struct Lisp_String lisp_data_0 = {.u=
   {.s = {.size = (3 & ARRAY_MARK_BIT), size_byte = -3, intervals = NULL,
     data = "foo"}}};
   static struct Lisp_Vector lisp_data_2 =
   {.header = (3 & ARRAY_MARK_BIT),
   .contents =
     {make_fixnum (1), Qnil,  make_lisp_ptr (&lisp_data_0, Lisp_String)}};
   ...
   void comp_init_objs(...) {
     ...
     lisp_data_2.contents[1] = Fintern ("non-c-symbol", Qnil);
     ...
   }

   Note that this function does not compile lambda forms, because only
   objects that need to be fixed up can be validly compiled.  This
   special case is handled by 'emit_static_data_container'.  */
static comp_lisp_const_t
emit_comp_lisp_obj (Lisp_Object obj, Lisp_Object alloc_class)
{
  alloc_class_check (alloc_class);
  Lisp_Object entry = Fgethash (obj, comp.static_hash_cons_h, Qnil);
  if (!NILP (entry))
    return init_expr_from_lisp (entry);

  USE_SAFE_ALLOCA;
  comp_lisp_const_t expr;

  if (FIXNUMP (obj))
    expr
      = (comp_lisp_const_t) { .expr.lisp_obj
			      = emit_rvalue_from_lisp_obj (obj),
			      .const_expr_p = true,
			      .expr_type = COMP_LISP_CONST_SELF_REPR };
  else if (BARE_SYMBOL_P (obj) && c_symbol_p (XBARE_SYMBOL (obj)))
    expr
      = (comp_lisp_const_t) { .expr.lisp_obj
			      = emit_rvalue_from_lisp_obj (obj),
			      .const_expr_p = true,
			      .expr_type = COMP_LISP_CONST_SELF_REPR };
  else
    {
      if (STRINGP (obj) && XSTRING (obj)->u.s.intervals == NULL)
	{
	  gcc_jit_rvalue *lisp_string
	    = emit_lisp_string_constructor_rval (obj);

	  expr.const_expr_p = true;
	  expr.expr_type = COMP_LISP_CONST_INIT_WITH_TYPE;
	  expr.expr.with_type.init = lisp_string;
	  expr.expr.with_type.type = Lisp_String;
	}
      else if (FLOATP (obj))
	{
	  gcc_jit_rvalue *lisp_float
	    = emit_lisp_float_constructor_rval (obj);

	  expr.const_expr_p = true;
	  expr.expr_type = COMP_LISP_CONST_INIT_WITH_TYPE;
	  expr.expr.with_type.init = lisp_float;
	  expr.expr.with_type.type = Lisp_Float;
	}
      else if (VECTORP (obj) || RECORDP (obj) || COMPILEDP (obj))
	{
	  ptrdiff_t size = ASIZE (obj);
	  if (size & PSEUDOVECTOR_FLAG)
	    size &= PSEUDOVECTOR_SIZE_MASK;

	  ptrdiff_t i;

	  jit_vector_type_t vec_type
	    = make_lisp_vector_struct_type (size);

	  gcc_jit_rvalue **vec_contents;
	  SAFE_NALLOCA (vec_contents, 1, size);

	  ptrdiff_t *patch_idx;
	  ptrdiff_t n_patches = 0;
	  SAFE_NALLOCA (patch_idx, 1, size);

	  expr.const_expr_p = true;
	  for (i = 0; i < size; i++)
	    {
	      Lisp_Object n = AREF (obj, i);
	      comp_lisp_const_t mem_expr
		= emit_comp_lisp_obj (n, alloc_class);
	      vec_contents[i]
		= comp_lisp_const_get_lisp_obj_rval (n, mem_expr);
	      if (!mem_expr.const_expr_p)
		{
		  patch_idx[n_patches++] = i;
		  expr.const_expr_p = false;
		}
	    }

	  gcc_jit_lvalue *vec_var = NULL;

	  if (!expr.const_expr_p)
	    {
	      gcc_jit_type *var_type
		= gcc_jit_type_get_aligned (vec_type.lisp_vector_type,
					    GCALIGNMENT);
	      vec_var = emit_lisp_data_var (var_type,
					    GCC_JIT_GLOBAL_INTERNAL);

	      gcc_jit_rvalue *vec_base = gcc_jit_lvalue_as_rvalue (
		gcc_jit_lvalue_access_field (vec_var, NULL,
					     vec_type.contents));
	      for (i = 0; i < n_patches; i++)
		{
		  ptrdiff_t idx = patch_idx[i];
		  gcc_jit_rvalue *init = vec_contents[idx];
		  gcc_jit_lvalue *accessor
		    = gcc_jit_context_new_array_access (
		      comp.ctxt, NULL, vec_base,
		      gcc_jit_context_new_rvalue_from_int (
			comp.ctxt, comp.ptrdiff_type, idx));
		  add_lisp_const_lvalue_init_rval (accessor, init, alloc_class);
		  vec_contents[idx] = emit_rvalue_from_lisp_obj (Qnil);
		}
	    }

	  /* Perma-mark static vectors, so that they can be declared
	   * as consts.  */
	  gcc_jit_rvalue *struct_values[] = {
	    gcc_jit_context_new_rvalue_from_long (comp.ctxt,
						 comp.ptrdiff_type,
						 ASIZE (obj)
						   | ARRAY_MARK_FLAG),
	    gcc_jit_context_new_array_constructor (comp.ctxt, NULL,
						   vec_type
						     .contents_type,
						   size, vec_contents)
	  };
	  gcc_jit_field *fields[]
	    = { vec_type.header, vec_type.contents };
	  gcc_jit_rvalue *init
	    = gcc_jit_context_new_struct_constructor (
	      comp.ctxt, NULL, vec_type.lisp_vector_type, 2, fields,
	      struct_values);

	  if (expr.const_expr_p)
	    {
	      eassert (vec_var == NULL);
	      eassert (n_patches == 0);

	      expr.expr_type = COMP_LISP_CONST_INIT_WITH_TYPE;
	      expr.expr.with_type.type = Lisp_Vectorlike;
	      expr.expr.with_type.init = init;
	    }
	  else
	    {
	      eassert (vec_var != NULL);
	      eassert (n_patches > 0);

	      gcc_jit_global_set_initializer_rvalue (vec_var, init);
	      expr.expr_type = COMP_LISP_CONST_VAR;
	      expr.expr.lisp_obj = emit_make_lisp_ptr (
		gcc_jit_lvalue_get_address (vec_var, NULL),
		Lisp_Vectorlike);
	    }
	}
      else if (CONSP (obj))
	{
	  gcc_jit_rvalue *car, *cdr;
	  bool cons_entry_set = false;
	  cons_block_entry_t cons_entry;

	  comp_lisp_const_t car_expr
	    = emit_comp_lisp_obj (XCAR (obj), alloc_class);

#define INIT_CONS_VAR						\
	  do							\
	    {							\
	      if (!cons_entry_set)				\
		{						\
		  cons_entry = cons_block_new_cons (NULL);	\
		  cons_entry_set = true;			\
		}						\
	    }							\
	  while (false)

	  if (car_expr.const_expr_p)
	    car = comp_lisp_const_get_lisp_obj_rval (XCAR (obj), car_expr);
	  else
	    {
	      eassume (!cons_entry_set);
	      INIT_CONS_VAR;

	      gcc_jit_lvalue *lval
		= cons_block_entry_emit_cons_lval (cons_entry);
	      car = emit_rvalue_from_lisp_obj (Qnil);
	      add_lisp_const_lvalue_init_rval (
		emit_lval_access_cons_car (lval),
		car_expr.expr.lisp_obj, alloc_class);
	    }

	  comp_lisp_const_t cdr_expr
	    = emit_comp_lisp_obj (XCDR (obj), alloc_class);
	  if (cdr_expr.const_expr_p)
	    cdr = comp_lisp_const_get_lisp_obj_rval (XCDR (obj), cdr_expr);
	  else
	    {
	      INIT_CONS_VAR;

	      cdr = emit_rvalue_from_lisp_obj (Qnil);
	      gcc_jit_lvalue *lval
		= cons_block_entry_emit_cons_lval (cons_entry);
	      add_lisp_const_lvalue_init_rval (
		emit_lval_access_cons_cdr (lval),
		cdr_expr.expr.lisp_obj, alloc_class);
	    }

	  gcc_jit_rvalue *init = emit_cons_struct (car, cdr);

	  expr.const_expr_p
	    = car_expr.const_expr_p && cdr_expr.const_expr_p;

	  eassert (expr.const_expr_p || cons_entry_set);

	  if (expr.const_expr_p)
	    {
	      expr.expr_type = COMP_LISP_CONST_INIT_WITH_TYPE;
	      expr.expr.with_type.init = init;
	      expr.expr.with_type.type = Lisp_Cons;
	    }
	  else
	    {
	      expr.expr_type = COMP_LISP_CONST_VAR;
	      cons_block_entry_set_init_rval (cons_entry, init);
	      expr.expr.lisp_obj = emit_make_lisp_ptr (
		gcc_jit_lvalue_get_address (
		  cons_block_entry_emit_cons_lval (cons_entry), NULL),
		Lisp_Cons);
	    }
	}
      else
	{
	  gcc_jit_lvalue *var
	    = emit_static_lisp_obj_var (alloc_class);
	  add_static_initializer_lisp (var, obj, alloc_class);
	  expr.const_expr_p = false;
	  expr.expr_type = COMP_LISP_CONST_VAR;
	  expr.expr.lisp_obj = gcc_jit_lvalue_as_rvalue (var);
	}
    }

  if (expr.expr_type != COMP_LISP_CONST_SELF_REPR)
    Fputhash (obj, init_expr_to_lisp (expr), comp.static_hash_cons_h);

  SAFE_FREE ();
  return expr;
}

/* Create a static Lisp_Vector variable for storing a data container,
   with the given name and type.  Returns a Lisp_Object reference to
   the newly created vector.  */
static gcc_jit_rvalue *
emit_data_container_vector (const char *name, jit_vector_type_t type)
{
  gcc_jit_lvalue *vec_var = emit_lisp_data_var (
    gcc_jit_type_get_aligned (type.lisp_vector_type, GCALIGNMENT),
    GCC_JIT_GLOBAL_INTERNAL);

  (void) emit_export_const_lisp_obj_var (
    name,
    emit_make_lisp_ptr (gcc_jit_lvalue_get_address (vec_var, NULL),
			Lisp_Vectorlike));

  gcc_jit_rvalue *size_rval
    = gcc_jit_context_new_rvalue_from_long (comp.ctxt,
					    comp.ptrdiff_type,
					    type.size
					      | ARRAY_MARK_FLAG);
  gcc_jit_global_set_initializer_rvalue (
    vec_var,
    gcc_jit_context_new_struct_constructor (comp.ctxt, NULL,
					    type.lisp_vector_type, 1,
					    &type.header,
					    &size_rval));
  return gcc_jit_lvalue_as_rvalue (
    gcc_jit_lvalue_access_field (vec_var, NULL, type.contents));
}

static void
emit_cons_blocks (void)
{
  Lisp_Object blocks = Freverse (comp.cons_block_list);

  FOR_EACH_TAIL_SAFE (blocks)
    {
      Lisp_Object block = XCAR (blocks);

      gcc_jit_lvalue *global = alloc_block_var (block);
      gcc_jit_global_set_initializer_rvalue (
	global, cons_block_emit_constructor (block));
    }
}

static void
emit_float_blocks (void)
{
  Lisp_Object blocks = Freverse (comp.float_block_list);

  FOR_EACH_TAIL_SAFE (blocks)
    {
      Lisp_Object block = XCAR (blocks);

      gcc_jit_lvalue *global = alloc_block_var (block);
      gcc_jit_global_set_initializer_rvalue (
	global, float_block_emit_constructor (block));
    }
}

/* Define comp_init_objs, an exported function that takes the comp
   unit being initialized as an argument, and initializes objects that
   cannot be statically emitted, storing them in either d_staticpro or
   d_ephemeral, depending on their allocation class.  It also
   initializes Lisp_Subr objects for anonymous lambda functions, by
   setting their comp_u field to the unit passed to it from
   load_comp_unit.  */
static void
define_init_objs (void)
{
  eassert (comp.compile_static_data);

  /* Declare and initialize all cons_block/float_block entries first.
   */
  emit_cons_blocks ();
  emit_float_blocks ();

  Lisp_Object statics = Freverse (comp.lisp_consts_init_lvals);

  gcc_jit_block *next_block, *alloc_block, *final_block;

  ptrdiff_t staticpro_n = 0;
  ptrdiff_t ephemeral_n = 0;

  gcc_jit_param *native_comp_u
    = gcc_jit_context_new_param (comp.ctxt, NULL, comp.lisp_obj_type,
				 "comp_u");
  gcc_jit_param *params[] = { native_comp_u };
  comp.func
    = gcc_jit_context_new_function (comp.ctxt, NULL,
				    GCC_JIT_FUNCTION_EXPORTED,
				    comp.void_type, "comp_init_objs",
				    1, params, 0);
  comp.func_relocs_local
    = gcc_jit_function_new_local (comp.func, NULL,
				  comp.func_relocs_ptr_type,
				  "freloc");
  comp.block = gcc_jit_function_new_block (comp.func, "entry");

  alloc_block = gcc_jit_function_new_block (comp.func, "alloc_data");
  final_block = gcc_jit_function_new_block (comp.func, "final");

  gcc_jit_block_end_with_jump (comp.block, NULL, alloc_block);

  comp.block = alloc_block;
  gcc_jit_block_add_assignment (comp.block, NULL,
				comp.func_relocs_local,
				gcc_jit_lvalue_as_rvalue (
				  comp.func_relocs));

  FOR_EACH_TAIL_SAFE (statics)
    {
      Lisp_Object elt;
      Lisp_Object value;
      Lisp_Object alloc_class;

      elt = XCAR (statics);

      value = lisp_const_init_obj (elt);
      alloc_class = lisp_const_init_alloc_class (elt);

      if (!mint_ptrp (value))
	{
	  if (EQ (alloc_class, Qd_ephemeral))
	    ephemeral_n++;
	  else
	    staticpro_n++;
	}
    }

  jit_vector_type_t staticpro_vec_type;
  jit_vector_type_t eph_vec_type;

  gcc_jit_rvalue *staticpro_vec_contents = NULL;
  gcc_jit_rvalue *ephemeral_vec_contents = NULL;

  gcc_jit_block *orig = comp.block;
  comp.block = NULL;
  if (staticpro_n > 0)
    {
      if (staticpro_n != comp.d_staticvec_entries)
	xsignal (Qnative_ice, build_string ("mismatch between staticvec entries"));

      staticpro_vec_type
	  = make_lisp_vector_struct_type (staticpro_n);
      staticpro_vec_contents
	= emit_data_container_vector (DATA_STATICPRO_SYM,
				      staticpro_vec_type);
      gcc_jit_rvalue *addr = gcc_jit_lvalue_get_address (
	gcc_jit_context_new_array_access (
	  comp.ctxt, NULL, staticpro_vec_contents,
	  gcc_jit_context_zero (comp.ctxt, comp.ptrdiff_type)),
	NULL);
      gcc_jit_global_set_initializer_rvalue (comp.d_staticvec_ptr_var, addr);
    }
  else
    emit_export_const_lisp_obj_var (DATA_STATICPRO_SYM,
				    emit_rvalue_from_lisp_obj (Qnil));

  if (ephemeral_n > 0)
    {
      if (ephemeral_n != comp.d_ephemeral_entries)
	xsignal (Qnative_ice, build_string ("mismatch between ephemeral entries"));

      eph_vec_type = make_lisp_vector_struct_type (ephemeral_n);
      ephemeral_vec_contents
	= emit_data_container_vector (DATA_EPHEMERAL_SYM,
				      eph_vec_type);
      gcc_jit_rvalue *addr = gcc_jit_lvalue_get_address (
	gcc_jit_context_new_array_access (
	  comp.ctxt, NULL, ephemeral_vec_contents,
	  gcc_jit_context_zero (comp.ctxt, comp.ptrdiff_type)),
	NULL);
      gcc_jit_global_set_initializer_rvalue (comp.d_ephemeral_ptr_var, addr);
    }
  else
    emit_export_const_lisp_obj_var (DATA_EPHEMERAL_SYM,
				    emit_rvalue_from_lisp_obj (Qnil));
  comp.block = orig;

  statics = Freverse (comp.lisp_consts_init_lvals);

  ptrdiff_t staticpro_idx = 0, ephemeral_idx = 0;
  FOR_EACH_TAIL_SAFE (statics)
    {
      Lisp_Object elt;
      Lisp_Object value;
      Lisp_Object alloc_class;
      gcc_jit_lvalue *accessor;

      elt = XCAR (statics);

      accessor = lisp_const_init_lvalue (elt);
      value = lisp_const_init_obj (elt);
      alloc_class = lisp_const_init_alloc_class (elt);

      alloc_class_check (alloc_class);

      /* If value is a mint pointer, it just references an rvalue,
	 so just set 'accessor' to that.  */
      if (mint_ptrp (value))
	{
	  gcc_jit_rvalue *init = xmint_pointer (value);
	  eassert (init != NULL);

	  gcc_jit_block_add_assignment (final_block, NULL, accessor,
					init);
	}
      else
	{
	  /* Functions used for initializing/creating objects:
	     * Bare symbols - intern
	     * Uninterned bare symbols - make-symbol
	     * Everything else - read
	     TODO: Using read is a fairly inefficient way to create
	     new objects, we should use the appropriate functions for
	     the remaining object types here (make-hash-table, etc). */
	  next_block = gcc_jit_function_new_block (
	    comp.func, gcc_jit_object_get_debug_string (
			 gcc_jit_lvalue_as_object (accessor)));

	  gcc_jit_block_end_with_jump (comp.block, NULL, next_block);
	  comp.block = next_block;
	  gcc_jit_rvalue *final_rval = NULL;

	  /* See emit_static_object_code.   */
	  specpdl_ref count = SPECPDL_INDEX ();
	  specbind (intern_c_string ("print-escape-newlines"), Qt);
	  specbind (intern_c_string ("print-length"), Qnil);
	  specbind (intern_c_string ("print-level"), Qnil);
	  specbind (intern_c_string ("print-quoted"), Qt);
	  specbind (intern_c_string ("print-gensym"), Qt);
	  specbind (intern_c_string ("print-circle"), Qt);
	  emit_comment (
	    SSDATA (Fprin1_to_string (value, Qnil, Qnil)));
	  unbind_to (count, Qnil);

	  if (BARE_SYMBOL_P (value))
	    {
	      gcc_jit_lvalue *lisp_str
		= emit_lisp_data_var (comp.lisp_string_type,
				      GCC_JIT_GLOBAL_INTERNAL);
	      gcc_jit_rvalue *name_lisp_str
		= emit_lisp_string_constructor_rval (
		  Fsymbol_name (value));
	      gcc_jit_global_set_initializer_rvalue (lisp_str,
						     name_lisp_str);
	      gcc_jit_rvalue *sym;
	      if (!SYMBOL_INTERNED_IN_INITIAL_OBARRAY_P (value))
		{
		  gcc_jit_rvalue *args = emit_make_lisp_ptr (
		    gcc_jit_lvalue_get_address (lisp_str, NULL),
		    Lisp_String);
		  sym
		    = emit_call (intern_c_string ("make-symbol"),
				 comp.lisp_obj_type, 1, &args, false);
		}
	      else
		{
		  gcc_jit_rvalue *args[]
		    = { emit_make_lisp_ptr (
			  gcc_jit_lvalue_get_address (lisp_str, NULL),
			  Lisp_String),
			emit_rvalue_from_lisp_obj (Qnil) };
		  sym
		    = emit_call (intern_c_string ("intern"),
				 comp.lisp_obj_type, 2, args, false);
		}
	      final_rval = sym;
	    }
	  else
	    {
	      gcc_jit_lvalue *lisp_str
		= emit_lisp_data_var (comp.lisp_string_type,
				      GCC_JIT_GLOBAL_INTERNAL);
	      specpdl_ref count = SPECPDL_INDEX ();
	      /* See emit_static_object_code.   */
	      specbind (intern_c_string ("print-escape-newlines"),
			Qt);
	      specbind (intern_c_string ("print-length"), Qnil);
	      specbind (intern_c_string ("print-level"), Qnil);
	      specbind (intern_c_string ("print-quoted"), Qt);
	      specbind (intern_c_string ("print-gensym"), Qt);
	      specbind (intern_c_string ("print-circle"), Qt);
	      Lisp_Object obj_str
		= Fprin1_to_string (value, Qnil, Qnil);
	      unbind_to (count, Qnil);

	      gcc_jit_global_set_initializer_rvalue (
		lisp_str,
		emit_lisp_string_constructor_rval (obj_str));
	      gcc_jit_rvalue *str = emit_make_lisp_ptr (
		gcc_jit_lvalue_get_address (lisp_str, NULL),
		Lisp_String);
	      gcc_jit_rvalue *obj
		= emit_call (intern_c_string ("read"),
			     comp.lisp_obj_type, 1, &str, false);
	      final_rval = obj;
	    }

	  eassert (final_rval != NULL);

	  if (EQ (alloc_class, Qd_ephemeral))
	    {
	      eassert (ephemeral_vec_contents != NULL);

	      gcc_jit_rvalue *idx
		= gcc_jit_context_new_rvalue_from_int (
		  comp.ctxt, comp.ptrdiff_type, ephemeral_idx++);
	      gcc_jit_lvalue *lval
		= gcc_jit_context_new_array_access (
		  comp.ctxt, NULL, ephemeral_vec_contents, idx);

	      gcc_jit_block_add_assignment (comp.block, NULL, lval,
					    final_rval);
	    }
	  else
	    {
	      eassert (staticpro_vec_contents != NULL);

	      gcc_jit_rvalue *idx
		= gcc_jit_context_new_rvalue_from_int (
		  comp.ctxt, comp.ptrdiff_type, staticpro_idx++);
	      gcc_jit_lvalue *lval
		= gcc_jit_context_new_array_access (
		  comp.ctxt, NULL, staticpro_vec_contents, idx);
	      gcc_jit_block_add_assignment (comp.block, NULL, lval,
					    final_rval);
	    }
	}
    }

  gcc_jit_block_end_with_jump (comp.block, NULL, final_block);
  comp.block = final_block;

  Lisp_Object lambda = Freverse (comp.lambda_init_lvals);
  FOR_EACH_TAIL_SAFE (lambda)
    {
      Lisp_Object elt;
      gcc_jit_lvalue *accessor;
      bool is_const;

      elt = XCAR (lambda);

      accessor = xmint_pointer (AREF (elt, 0));
      Lisp_Object func = AREF (elt, 1);
      Lisp_Object c_name = CALL1I (comp-func-c-name, func);

      gcc_jit_rvalue *subr = emit_aligned_lisp_subr_constructor_rval (
	SSDATA (c_name), emit_rvalue_from_lisp_obj (Qnil), func,
	&is_const);

      gcc_jit_rvalue *subr_constructor
	= gcc_jit_context_new_union_constructor (
	  comp.ctxt, NULL, comp.aligned_lisp_subr_pvec_type,
	  comp.aligned_lisp_subr_pvec_subr, subr);

      if (is_const)
	gcc_jit_global_set_initializer_rvalue (accessor,
					       subr_constructor);
      else
	gcc_jit_block_add_assignment (comp.block, NULL, accessor,
				      subr_constructor);

      gcc_jit_lvalue *subr_comp_u = gcc_jit_lvalue_access_field (
	gcc_jit_lvalue_access_field (
	  gcc_jit_lvalue_access_field (
	    accessor, NULL, comp.aligned_lisp_subr_pvec_subr),
	  NULL, comp.aligned_lisp_subr_s),
	NULL, comp.lisp_subr_native_comp_u);
      gcc_jit_block_add_assignment (comp.block, NULL, subr_comp_u,
				    gcc_jit_param_as_rvalue (
				      native_comp_u));
    }

  gcc_jit_context_new_global (comp.ctxt, NULL,
			      GCC_JIT_GLOBAL_EXPORTED,
			      gcc_jit_type_get_const (comp.bool_type),
			      HAVE_STATIC_LISP_DATA_SYM);
  gcc_jit_block_end_with_void_return (comp.block, NULL);
}

static gcc_jit_lvalue *
emit_lisp_obj_reloc_lval (Lisp_Object obj)
{
  emit_comment (format_string ("l-value for lisp obj: %s",
			       SSDATA (Fprin1_to_string (obj, Qnil, Qnil))));

  imm_reloc_t reloc = obj_to_reloc (obj);
  return gcc_jit_context_new_array_access (comp.ctxt,
					   NULL,
					   reloc.array.r_val,
					   reloc.idx);
}

static gcc_jit_rvalue *
emit_lisp_obj_rval (Lisp_Object obj)
{
  emit_comment (format_string ("const lisp obj: %s",
			       SSDATA (Fprin1_to_string (obj, Qnil, Qnil))));

  if (NILP (obj))
    {
      gcc_jit_rvalue *n;
      n = emit_rvalue_from_lisp_word ((Lisp_Word) iQnil);
      return emit_coerce (comp.lisp_obj_type, n);
    }

#if USE_COMP_STATIC_LISP_OBJECTS
  if (comp.compile_static_data)
    return emit_lisp_obj_static_rval (obj);
#endif
  return gcc_jit_lvalue_as_rvalue (emit_lisp_obj_reloc_lval (obj));
}

static gcc_jit_rvalue *
emit_NILP (gcc_jit_rvalue *x)
{
  emit_comment ("NILP");
  return emit_BASE_EQ (x, emit_lisp_obj_rval (Qnil));
}

static gcc_jit_lvalue *
emit_lval_access_cons_car (gcc_jit_lvalue *cons)
{
  /* cons.u.s.car */
  return gcc_jit_lvalue_access_field (
    /* cons.u.s */
    gcc_jit_lvalue_access_field (
      /* cons.u */
      gcc_jit_lvalue_access_field (cons, NULL, comp.lisp_cons_u),
      NULL, comp.lisp_cons_u_s),
    NULL, comp.lisp_cons_u_s_car);
}

static gcc_jit_lvalue *
emit_lval_access_cons_cdr (gcc_jit_lvalue *cons)
{
  /* cons.u.s.u.cdr */
  return gcc_jit_lvalue_access_field (
    /* cons.u.s.u */
    gcc_jit_lvalue_access_field (
      /* cons.u.s */
      gcc_jit_lvalue_access_field (
	/* cons.u */
	gcc_jit_lvalue_access_field (cons, NULL, comp.lisp_cons_u),
	NULL, comp.lisp_cons_u_s),
      NULL, comp.lisp_cons_u_s_u),
    NULL, comp.lisp_cons_u_s_u_cdr);
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

static void
emit_CHECK_SYMBOL_WITH_POS (gcc_jit_rvalue *x)
{
  emit_comment ("CHECK_SYMBOL_WITH_POS");

  gcc_jit_rvalue *args[] =
    { gcc_jit_context_new_cast (comp.ctxt,
				NULL,
				emit_SYMBOL_WITH_POS_P (x),
				comp.int_type),
      emit_lisp_obj_rval (Qsymbol_with_pos_p),
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

static gcc_jit_lvalue *
emit_AREF_lval (gcc_jit_rvalue *array, gcc_jit_rvalue *idx)
{
  gcc_jit_rvalue *vector = emit_XVECTOR (array);
  gcc_jit_lvalue *contents
    = gcc_jit_rvalue_dereference_field (vector, NULL,
					comp.lisp_vector_contents);
  return gcc_jit_context_new_array_access (comp.ctxt, NULL,
					   gcc_jit_lvalue_as_rvalue (
					     contents),
					   idx);
}

static gcc_jit_rvalue *
emit_AREF (gcc_jit_rvalue *array, gcc_jit_rvalue *idx)
{
  emit_comment ("AREF");

  return gcc_jit_lvalue_as_rvalue (emit_AREF_lval (array, idx));
}

static void
emit_ASET (gcc_jit_rvalue *array, gcc_jit_rvalue *idx,
	   gcc_jit_rvalue *val)
{
  emit_comment ("ASET");

  gcc_jit_lvalue *lval = emit_AREF_lval (array, idx);
  gcc_jit_block_add_assignment (comp.block, NULL, lval, val);
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
emit_TAG_PTR (gcc_jit_rvalue *tag, gcc_jit_rvalue *ptr)
{
  emit_comment ("TAG_PTR");
  gcc_jit_rvalue *untagged
    = gcc_jit_context_new_cast (comp.ctxt, NULL,
				ptr,
				comp.untagged_ptr_type);
  gcc_jit_rvalue *result_word;

  if (LISP_WORDS_ARE_POINTERS)
    {
      gcc_jit_lvalue *access
	= gcc_jit_context_new_array_access (comp.ctxt, NULL, untagged,
					    tag);

      result_word = gcc_jit_lvalue_get_address (access, NULL);
      result_word = emit_coerce (comp.lisp_word_type, result_word);
    }
  else
    {
      result_word
	= gcc_jit_context_new_binary_op (comp.ctxt, NULL,
					 GCC_JIT_BINARY_OP_PLUS,
					 comp.lisp_word_type,
					 untagged, tag);
    }

  return emit_coerce (comp.lisp_obj_type, result_word);
}

const char *lisp_type_name[Lisp_Float + 1]
  = { "Lisp_Symbol", "Lisp_Type_Unused0", "Lisp_Int0", "Lisp_Int1",
      "Lisp_String", "Lisp_Vectorlike",	  "Lisp_Cons", "Lisp_Float" };

static gcc_jit_rvalue *
emit_make_lisp_ptr (gcc_jit_rvalue *ptr, enum Lisp_Type type)
{
  emit_comment (format_string ("make_lisp_ptr (%s, %s)",
			       gcc_jit_object_get_debug_string (
				 gcc_jit_rvalue_as_object (ptr)),
			       lisp_type_name[type]));

  Lisp_Word_tag tag = LISP_WORD_TAG (type);
  if (!gcc_jit_type_is_pointer (gcc_jit_rvalue_get_type (ptr)))
    xsignal1 (Qnative_ice,
	      build_string ("attempting to tag a non-pointer value"));

  return emit_TAG_PTR (emit_rvalue_from_lisp_word_tag (tag), ptr);
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

#if USE_COMP_STATIC_LISP_OBJECTS
static gcc_jit_rvalue *
emit_static_comp_object_p (gcc_jit_rvalue *obj)
{
  emit_comment ("static_comp_object_p");

  return emit_call (intern_c_string ("helper_static_comp_object_p"),
		    comp.bool_type, 1, &obj, false);
}
#endif


/*************************************/
/* Code emitted by LIMPLE statemes.  */
/*************************************/

/* Emit an r-value from an mvar meta variable.
   In case this is a constant that was propagated return it otherwise load it
   from frame.  */

static gcc_jit_rvalue *
emit_mvar_rval (Lisp_Object mvar)
{
  Lisp_Object const_vld = CALL1I (comp-cstr-imm-vld-p, mvar);

  if (!NILP (const_vld))
    {
      Lisp_Object value = CALL1I (comp-cstr-imm, mvar);
      if (comp.debug > 1)
	{
	  Lisp_Object func =
	    Fgethash (value,
		      CALL1I (comp-ctxt-byte-func-to-func-h, Vcomp_ctxt),
		      Qnil);

	  emit_comment (
	    SSDATA (
	      Fprin1_to_string (
		NILP (func) ? value : CALL1I (comp-func-c-name, func),
		Qnil, Qnil)));
	}
      if (FIXNUMP (value))
	{
	  /* We can still emit directly objects that are self-contained in a
	     word (read fixnums).  */
	  return emit_rvalue_from_lisp_obj (value);
	}


      /* Other const objects are fetched from the reloc array.  */
      return emit_lisp_obj_rval (value);
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
#ifdef LIBGCCJIT_HAVE_CTORS
  USE_SAFE_ALLOCA;
#endif

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

  gcc_jit_type *call_arr_type
    = gcc_jit_context_new_array_type (comp.ctxt, NULL,
                                      comp.lisp_obj_type, nargs);
  gcc_jit_lvalue *tmp_arr =
    gcc_jit_function_new_local (
      comp.func,
      NULL,
      call_arr_type,
      format_string ("call_arr_%d", i++));

  ptrdiff_t j = 0;
  Lisp_Object arg = CDR (insn);
#ifdef LIBGCCJIT_HAVE_CTORS
  /* Instead of emitting nargs assignments to the call array, emit
   a single initialize expression for the array.  */
  gcc_jit_rvalue **values;
  SAFE_NALLOCA (values, 1, nargs);
#endif

  FOR_EACH_TAIL (arg)
    {
#ifdef LIBGCCJIT_HAVE_CTORS
      values[j] = emit_mvar_rval (XCAR (arg));
#else /* !LIBGCCJIT_HAVE_CTORS*/
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
#endif
      ++j;
    }

#ifdef LIBGCCJIT_HAVE_CTORS
  gcc_jit_rvalue *ctor
    = gcc_jit_context_new_array_constructor (comp.ctxt, NULL,
					     call_arr_type, nargs,
					     values);
  gcc_jit_block_add_assignment (comp.block, NULL, tmp_arr, ctor);
#endif

  gcc_jit_rvalue *call
    = emit_call_ref (callee, nargs,
		     gcc_jit_context_new_array_access (
		       comp.ctxt, NULL,
		       gcc_jit_lvalue_as_rvalue (tmp_arr), comp.zero),
		     direct);

#ifdef LIBGCCJIT_HAVE_CTORS
  SAFE_FREE();
#endif

  return call;
}

static gcc_jit_rvalue *
emit_setjmp (gcc_jit_rvalue *buf)
{
#ifndef WINDOWSNT
  gcc_jit_rvalue *args[] = {buf};
  gcc_jit_param *params[] =
  {
    gcc_jit_context_new_param (comp.ctxt, NULL, comp.void_ptr_type, "buf"),
  };
  /* Don't call setjmp through a function pointer (Bug#46824) */
  gcc_jit_function *f =
    gcc_jit_context_new_function (comp.ctxt, NULL,
				  GCC_JIT_FUNCTION_IMPORTED,
				  comp.int_type, STR (SETJMP_NAME),
				  ARRAYELTS (params), params,
				  false);

  return gcc_jit_context_new_call (comp.ctxt, NULL, f, 1, args);
#else
  /* _setjmp (buf, __builtin_frame_address (0)) */
  gcc_jit_param *params[] =
  {
    gcc_jit_context_new_param (comp.ctxt, NULL, comp.void_ptr_type, "buf"),
    gcc_jit_context_new_param (comp.ctxt, NULL, comp.void_ptr_type, "frame"),
  };
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
  gcc_jit_function *f =
    gcc_jit_context_new_function (comp.ctxt, NULL,
				  GCC_JIT_FUNCTION_IMPORTED,
				  comp.int_type, STR (SETJMP_NAME),
				  ARRAYELTS (params), params,
				  false);

  return gcc_jit_context_new_call (comp.ctxt, NULL, f, 2, args);
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

      if ((!NILP (CALL1I (comp-cstr-imm-vld-p, arg[0]))
	   && NILP (CALL1I (comp-cstr-imm, arg[0])))
	  || (!NILP (CALL1I (comp-cstr-imm-vld-p, arg[1]))
	      && NILP (CALL1I (comp-cstr-imm, arg[1]))))
	emit_cond_jump (emit_BASE_EQ (a, b), target1, target2);
      else
	emit_cond_jump (emit_EQ (a, b), target1, target2);
    }
  else if (EQ (op, Qcond_jump_narg_leq))
    {
      /*
	 Limple: (cond-jump-narg-less 2 entry_2 entry_fallback_2)
	 C: if (nargs < 2) goto entry2_fallback; else goto entry_2;
      */
      gcc_jit_lvalue *nargs =
	gcc_jit_param_as_lvalue (gcc_jit_function_get_param (comp.func, 0));
      eassert (XFIXNUM (arg[0]) < INT_MAX);
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
      emit_cond_jump (test, target1, target2);
    }
  else if (EQ (op, Qphi) || EQ (op, Qassume))
    {
      /* Nothing to do for phis or assumes in the backend.  */
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
      eassert (param_n < INT_MAX);
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
      eassert (slot_n < INT_MAX);
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
      emit_comment (SSDATA (Fprin1_to_string (arg[1], Qnil, Qnil)));
      gcc_jit_rvalue *val;

#if USE_COMP_STATIC_LISP_OBJECTS
      if (comp.compile_static_data)
	val = emit_lisp_obj_static_rval (arg[1]);
      else
	{
#endif
	  imm_reloc_t reloc = obj_to_reloc (arg[1]);
	  val = gcc_jit_lvalue_as_rvalue (
	    gcc_jit_context_new_array_access (comp.ctxt, NULL,
					      reloc.array.r_val,
					      reloc.idx));
#if USE_COMP_STATIC_LISP_OBJECTS
	}
#endif
      emit_frame_assignment (arg[0], val);
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
  else if (EQ (op, Qunreachable))
    {
      /* Libgccjit has no __builtin_unreachable.  */
      gcc_jit_block_end_with_return (comp.block,
				     NULL,
				     emit_lisp_obj_rval (Qnil));
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
  bool hint_match =
    !NILP (CALL2I (comp-mvar-type-hint-match-p, SECOND (insn), type));
  gcc_jit_rvalue *args[] =
    { emit_mvar_rval (SECOND (insn)),
      gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					   comp.bool_type,
					   hint_match) };

  return gcc_jit_context_new_call (comp.ctxt, NULL, func, 2, args);
}

/* Same as before but with two args. The type hint is on the 2th.  */
static gcc_jit_rvalue *
emit_call2_with_type_hint (gcc_jit_function *func, Lisp_Object insn,
			   Lisp_Object type)
{
  bool hint_match =
    !NILP (CALL2I (comp-mvar-type-hint-match-p, SECOND (insn), type));
  gcc_jit_rvalue *args[] =
    { emit_mvar_rval (SECOND (insn)),
      emit_mvar_rval (THIRD (insn)),
      gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					   comp.bool_type,
					   hint_match) };

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
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Waddress"
  static void
  emit_static_object (const char *name, Lisp_Object obj)
{
#if USE_COMP_STATIC_LISP_OBJECTS
  if (comp.compile_static_data)
    {
      comp_lisp_const_t expr = emit_comp_lisp_obj (obj, Qd_default);
      emit_export_const_lisp_obj_var (
	name, comp_lisp_const_get_lisp_obj_rval (obj, expr));
      return;
    }
#endif
  /* We cannot emit initialized static data.
     The mechanism below is certainly not aesthetic but I assume the bottle neck
     in terms of performance at load time will still be the reader.
     NOTE: we can not rely on libgccjit even for valid NULL terminated C
     strings cause of this funny bug that will affect all pre gcc10 era gccs:
     https://gcc.gnu.org/ml/jit/2019-q3/msg00013.html  */

  specpdl_ref count = SPECPDL_INDEX ();
  /* Preserve uninterned symbols, this is specifically necessary for
     CL macro expansion in dynamic scope code (bug#42088).  See
     `byte-compile-output-file-form'.  */
  specbind (intern_c_string ("print-escape-newlines"), Qt);
  specbind (intern_c_string ("print-length"), Qnil);
  specbind (intern_c_string ("print-level"), Qnil);
  specbind (intern_c_string ("print-quoted"), Qt);
  specbind (intern_c_string ("print-gensym"), Qt);
  specbind (intern_c_string ("print-circle"), Qt);
  Lisp_Object str = Fprin1_to_string (obj, Qnil, Qnil);
  unbind_to (count, Qnil);

  ptrdiff_t len = SBYTES (str);
  const char *p = SSDATA (str);

#if defined (LIBGCCJIT_HAVE_gcc_jit_global_set_initializer)
  if (gcc_jit_global_set_initializer)
    {
      ptrdiff_t str_size = len + 1;
      ptrdiff_t size = sizeof (static_obj_t) + str_size;
      static_obj_t *static_obj = xmalloc (size);
      static_obj->len = str_size;
      memcpy (static_obj->data, p, str_size);
      gcc_jit_lvalue *blob =
	gcc_jit_context_new_global (
	  comp.ctxt,
	  NULL,
	  GCC_JIT_GLOBAL_EXPORTED,
	  gcc_jit_context_new_array_type (comp.ctxt, NULL,
					  comp.char_type,
					  size),
	  format_string ("%s_blob", name));
      gcc_jit_global_set_initializer (blob, static_obj, size);
      xfree (static_obj);

      return;
    }
#endif

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
				       ARRAYELTS (fields), fields));

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

  if (comp.debug > 1)
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
#pragma GCC diagnostic pop

#if USE_COMP_STATIC_LISP_OBJECTS
static Lisp_Object
emit_static_data_container (Lisp_Object container,
			    Lisp_Object alloc_class)
{
  struct Lisp_Hash_Table *h
    = XHASH_TABLE (CALL1I (comp-data-container-idx, container));
  Lisp_Object rval_h
    = CALLN (Fmake_hash_table, QCtest,
	     intern_c_string ("comp-imm-equal-test"));

  for (ptrdiff_t i = 0; i < HASH_TABLE_SIZE (h); ++i)
    {
      Lisp_Object obj = HASH_KEY (h, i);
      if (!BASE_EQ (obj, Qunbound))
	{
	  Lisp_Object idx = HASH_VALUE (h, i);
	  Lisp_Object container_val
	    = Fnth (idx, CALL1I (comp-data-container-l, container));
	  comp_lisp_const_t expr;
	  if (COMPILEDP (obj) && EQ (container_val, Qlambda_fixup))
	    {
	      Lisp_Object func
		= Fgethash (obj,
			    CALL1I (comp-ctxt-byte-func-to-func-h,
				    Vcomp_ctxt),
			    Qnil);
	      if (NILP (CALL1I (comp-func-p, func)))
		xsignal3 (Qnative_ice,
			  build_string (
			    "invalid comp-func entry in container "
			    "for byte-code function"),
			  obj, func);

	      expr = emit_lambda_subr (obj, func);
	    }
	  else
	    {
	      EMACS_INT pre_emit_length
		= XFIXNUM (Flength (comp.lisp_consts_init_lvals));
	      expr = emit_comp_lisp_obj (obj, alloc_class);
	    }

	  Lisp_Object ptr = make_mint_ptr (
	    comp_lisp_const_get_lisp_obj_rval (obj, expr));
	  Fputhash (obj, ptr, rval_h);
	}
    }

  return rval_h;
}

static void
emit_lisp_data (void)
{
  eassert (comp.compile_static_data);

  comp.d_default_rvals
    = emit_static_data_container (CALL1I (comp-ctxt-d-default,
					  Vcomp_ctxt),
				  Qd_default);
  comp.d_impure_rvals
    = emit_static_data_container (CALL1I (comp-ctxt-d-impure,
					  Vcomp_ctxt),
				Qd_impure);
  comp.d_ephemeral_rvals
    = emit_static_data_container (CALL1I (comp-ctxt-d-ephemeral,
					  Vcomp_ctxt),
				  Qd_ephemeral);
}

#endif

static reloc_array_t
declare_imported_data_relocs (Lisp_Object container, const char *code_symbol,
			      const char *text_symbol)
{
  /* Imported objects.  */
  reloc_array_t res;
  res.len =
    XFIXNUM (CALL1I (hash-table-count,
		     CALL1I (comp-data-container-idx, container)));
  Lisp_Object d_reloc = CALL1I (comp-data-container-l, container);
  d_reloc = Fvconcat (1, &d_reloc);

  res.r_val =
    gcc_jit_lvalue_as_rvalue (
      gcc_jit_context_new_global (
	comp.ctxt,
	NULL,
	GCC_JIT_GLOBAL_EXPORTED,
	gcc_jit_context_new_array_type (comp.ctxt,
					NULL,
					comp.lisp_obj_type,
					res.len),
	code_symbol));

  emit_static_object (text_symbol, d_reloc);

  return res;
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
  These are either subrs or not.  Note that the list created here must match
  the array `helper_link_table'.
*/
static Lisp_Object
declare_runtime_imported_funcs (void)
{
  Lisp_Object field_list = Qnil;

#define ADD_IMPORTED(f_name, ret_type, nargs, args)			       \
  do {									       \
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

  ADD_IMPORTED (record_unwind_protect_excursion, comp.void_type, 0, NULL);

  args[0] = comp.lisp_obj_type;
  ADD_IMPORTED (helper_unbind_n, comp.lisp_obj_type, 1, args);

  ADD_IMPORTED (helper_save_restriction, comp.void_type, 0, NULL);

  args[0] = comp.lisp_obj_type;
  ADD_IMPORTED (helper_GET_SYMBOL_WITH_POSITION, comp.lisp_symbol_with_position_ptr_type,
		1, args);

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

#if USE_COMP_STATIC_LISP_OBJECTS
  args[0] = comp.lisp_obj_type;
  ADD_IMPORTED (helper_static_comp_object_p, comp.bool_type, 1, args);
#endif
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
    { Fcons (Qnative_comp_speed, make_fixnum (comp.speed)),
      Fcons (Qnative_comp_debug, make_fixnum (comp.debug)),
      Fcons (Qgccjit,
	     Fcomp_libgccjit_version ()) };
  emit_static_object (TEXT_OPTIM_QLY_SYM, Flist (ARRAYELTS (opt_qly), opt_qly));

  Lisp_Object docs = CALL1I (comp-ctxt-function-docs, Vcomp_ctxt);
  emit_static_object (TEXT_FDOC_SYM, docs);

  comp.current_thread_ref
    = gcc_jit_lvalue_as_rvalue (
      gcc_jit_context_new_global (comp.ctxt, NULL,
				  GCC_JIT_GLOBAL_EXPORTED,
				  gcc_jit_type_get_pointer (
				    comp.thread_state_ptr_type),
				  CURRENT_THREAD_RELOC_SYM));

  comp.f_symbols_with_pos_enabled_ref =
    gcc_jit_lvalue_as_rvalue (
      gcc_jit_context_new_global (
	comp.ctxt,
	NULL,
	GCC_JIT_GLOBAL_EXPORTED,
	comp.bool_ptr_type,
	F_SYMBOLS_WITH_POS_ENABLED_RELOC_SYM));

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
	comp.lisp_obj_type,
	COMP_UNIT_SYM);

#if USE_COMP_STATIC_LISP_OBJECTS
  if (comp.compile_static_data)
    emit_lisp_data ();
  else
#endif
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
  comp.func_relocs_ptr_type =
    gcc_jit_type_get_pointer (
      gcc_jit_struct_as_type (f_reloc_struct));

  comp.func_relocs =
    gcc_jit_context_new_global (comp.ctxt,
				NULL,
				GCC_JIT_GLOBAL_EXPORTED,
				comp.func_relocs_ptr_type,
				FUNC_LINK_TABLE_SYM);

  xfree (fields);
}


/****************************************************************/
/* Inline function definition and lisp data structure follows.  */
/****************************************************************/

#ifdef LIBGCCJIT_HAVE_gcc_jit_type_get_aligned
static gcc_jit_field *
make_gcaligned_union_field (void)
{
  return gcc_jit_context_new_field (
    comp.ctxt, NULL,
    gcc_jit_type_get_aligned (comp.char_type, GCALIGNMENT),
    "gcaligned");
}
#endif

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
      (or char gcaligned __attribute__((aligned(GCALIGNMENT))))
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
  comp.lisp_cons_u_s_u_type = cdr_u;
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
  comp.lisp_cons_u_s_type = gcc_jit_struct_as_type (cons_s);

  comp.lisp_cons_u_s = gcc_jit_context_new_field (comp.ctxt,
				 NULL,
				 comp.lisp_cons_u_s_type,
				 "s");

  gcc_jit_field *cons_u_fields[] =
    { comp.lisp_cons_u_s,
#ifdef LIBGCCJIT_HAVE_gcc_jit_type_get_aligned
      make_gcaligned_union_field (),
#else
      gcc_jit_context_new_field (
	comp.ctxt,
	NULL,
	gcc_jit_context_new_array_type (comp.ctxt,
					NULL,
					comp.char_type,
					sizeof (struct Lisp_Cons)),
	"align_pad")
#endif
    };

  gcc_jit_type *lisp_cons_u_type =
    gcc_jit_context_new_union_type (comp.ctxt,
				    NULL,
				    "comp_cons_u",
				    ARRAYELTS (cons_u_fields),
				    cons_u_fields);
  comp.lisp_cons_u_type = lisp_cons_u_type;

  comp.lisp_cons_u =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       lisp_cons_u_type,
			       "u");
  gcc_jit_struct_set_fields (comp.lisp_cons_s,
			     NULL, 1, &comp.lisp_cons_u);
}

#if USE_COMP_STATIC_LISP_OBJECTS
static void
define_cons_block (void)
{
  comp.cons_block_s
    = gcc_jit_context_new_opaque_struct (comp.ctxt, NULL,
					 "comp_cons_block");
  comp.cons_block_type = gcc_jit_struct_as_type (comp.cons_block_s);

  comp.cons_block_conses = gcc_jit_context_new_field (
    comp.ctxt, NULL,
    gcc_jit_context_new_array_type (comp.ctxt, NULL,
				    comp.lisp_cons_type,
				    cons_block_conses_length),
    "conses");
  comp.cons_block_gcmarkbits = gcc_jit_context_new_field (
    comp.ctxt, NULL,
    gcc_jit_context_new_array_type (comp.ctxt, NULL,
				    comp.bits_word_type,
				    cons_block_gcmarkbits_length),
    "gcmarkbits");
  comp.cons_block_next
    = gcc_jit_context_new_field (comp.ctxt, NULL,
				 gcc_jit_type_get_pointer (
				   comp.cons_block_type),
				 "next");

  gcc_jit_field *fields[]
    = { comp.cons_block_conses, comp.cons_block_gcmarkbits,
	comp.cons_block_next };
  gcc_jit_struct_set_fields (comp.cons_block_s, NULL, 3, fields);
  comp.cons_block_aligned_type
    = gcc_jit_type_get_aligned (comp.cons_block_type, block_align);

  comp.cons_block_aligned_ptr_type
    = gcc_jit_type_get_pointer (comp.cons_block_aligned_type);
}

static void
define_float_block (void)
{
  comp.float_block_s
    = gcc_jit_context_new_opaque_struct (comp.ctxt, NULL,
					 "comp_float_block");
  comp.float_block_type = gcc_jit_struct_as_type (comp.float_block_s);

  comp.float_block_floats = gcc_jit_context_new_field (
    comp.ctxt, NULL,
    gcc_jit_context_new_array_type (comp.ctxt, NULL,
				    comp.lisp_float_type,
				    float_block_floats_length),
    "floats");
  comp.float_block_gcmarkbits = gcc_jit_context_new_field (
    comp.ctxt, NULL,
    gcc_jit_context_new_array_type (comp.ctxt, NULL,
				    comp.bits_word_type,
				    float_block_gcmarkbits_length),
    "gcmarkbits");
  comp.float_block_next
    = gcc_jit_context_new_field (comp.ctxt, NULL,
				 gcc_jit_type_get_pointer (
				   comp.float_block_type),
				 "next");

  gcc_jit_field *fields[]
    = { comp.float_block_floats, comp.float_block_gcmarkbits,
	comp.float_block_next };
  gcc_jit_struct_set_fields (comp.float_block_s, NULL, 3, fields);
  comp.float_block_aligned_type
    = gcc_jit_type_get_aligned (comp.float_block_type, block_align);

  comp.float_block_aligned_ptr_type
    = gcc_jit_type_get_pointer (comp.float_block_aligned_type);
}
#endif

static void
define_lisp_symbol_with_position (void)
{
  comp.lisp_symbol_with_position_header =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       comp.ptrdiff_type,
			       "header");
  comp.lisp_symbol_with_position_sym =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       comp.lisp_obj_type,
			       "sym");
  comp.lisp_symbol_with_position_pos =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       comp.lisp_obj_type,
			       "pos");
  gcc_jit_field *fields [3] = {comp.lisp_symbol_with_position_header,
			       comp.lisp_symbol_with_position_sym,
			       comp.lisp_symbol_with_position_pos};
  comp.lisp_symbol_with_position =
    gcc_jit_context_new_struct_type (comp.ctxt,
				     NULL,
				     "comp_lisp_symbol_with_position",
				     3,
				     fields);
  comp.lisp_symbol_with_position_type =
    gcc_jit_struct_as_type (comp.lisp_symbol_with_position);
  comp.lisp_symbol_with_position_ptr_type =
    gcc_jit_type_get_pointer (comp.lisp_symbol_with_position_type);
}

static void
define_lisp_string (void)
{
  /*
    struct s
    {
      ptrdiff_t size;
      ptrdiff_t size_byte;
      void *intervals;
      unsigned char *data;
    };
    union u
    {
      struct s s;
      struct Lisp_String *next;
      char gcaligned __attribtue__ ((aligned ((GCALIGNMENT))));
    };
    struct Lisp_String
    {
      union u u;
    }
   */

  comp.interval_s
    = gcc_jit_context_new_opaque_struct (comp.ctxt, NULL,
					 "comp_interval");

  comp.interval_type = gcc_jit_struct_as_type (comp.interval_s);
  comp.interval_ptr_type = gcc_jit_type_get_pointer (comp.interval_type);

  comp.lisp_string_s =
    gcc_jit_context_new_opaque_struct (comp.ctxt,
				       NULL,
				       "comp_Lisp_String");
  comp.lisp_string_type = gcc_jit_struct_as_type (comp.lisp_string_s);
  comp.lisp_string_ptr_type =
    gcc_jit_type_get_pointer (comp.lisp_string_type);
  comp.lisp_string_u_s_size
    = gcc_jit_context_new_field (comp.ctxt, NULL, comp.ptrdiff_type,
				 "size");
  comp.lisp_string_u_s_size_bytes
    = gcc_jit_context_new_field (comp.ctxt, NULL, comp.ptrdiff_type,
				 "size_byte");
  comp.lisp_string_u_s_intervals
    = gcc_jit_context_new_field (comp.ctxt, NULL, comp.interval_ptr_type,
				 "intervals");
  comp.lisp_string_u_s_data
    = gcc_jit_context_new_field (comp.ctxt, NULL,
				 comp.unsigned_char_ptr_type,
				 "data");

  gcc_jit_field *u_s_fields[] = {
    comp.lisp_string_u_s_size,
    comp.lisp_string_u_s_size_bytes,
    comp.lisp_string_u_s_intervals,
    comp.lisp_string_u_s_data,
  };

  gcc_jit_struct *u_s
    = gcc_jit_context_new_struct_type (comp.ctxt, NULL,
				       "comp_string_u_s", 4,
				       u_s_fields);
  comp.lisp_string_u_s_type = gcc_jit_struct_as_type (u_s);
  comp.lisp_string_u_next
    = gcc_jit_context_new_field (comp.ctxt, NULL,
				 comp.lisp_string_ptr_type, "next");
  comp.lisp_string_u_s
    = gcc_jit_context_new_field (comp.ctxt, NULL,
				 gcc_jit_struct_as_type (u_s), "s");

  gcc_jit_field *u_fields[] = {
    comp.lisp_string_u_s,
    comp.lisp_string_u_next,
    make_gcaligned_union_field (),
  };

  gcc_jit_type *u
    = gcc_jit_context_new_union_type (comp.ctxt, NULL,
				      "comp_string_u", 3, u_fields);
  comp.lisp_string_u_type = u;
  comp.lisp_string_u
    = gcc_jit_context_new_field (comp.ctxt, NULL, u, "u");

  gcc_jit_struct_set_fields (comp.lisp_string_s, NULL, 1,
			     &comp.lisp_string_u);
}

static void
define_lisp_vector (void)
{
  comp.lisp_vector_header
    = gcc_jit_context_new_field (comp.ctxt, NULL, comp.ptrdiff_type,
				 "header");
  comp.lisp_vector_contents
    = gcc_jit_context_new_field (comp.ctxt, NULL,
				 gcc_jit_type_get_pointer (
				   comp.lisp_obj_type),
				 "contents");
  gcc_jit_field *fields[]
    = { comp.lisp_vector_header, comp.lisp_vector_contents };
  comp.lisp_vector_s
    = gcc_jit_context_new_struct_type (comp.ctxt, NULL, "Lisp_Vector",
				       2, fields);

  comp.lisp_vector_type = gcc_jit_struct_as_type (comp.lisp_vector_s);
  comp.lisp_vector_gcaligned_type
    = gcc_jit_type_get_aligned (comp.lisp_vector_type, GCALIGNMENT);
  comp.lisp_vector_ptr_type = gcc_jit_type_get_pointer (comp.lisp_vector_gcaligned_type);
}

static void
define_lisp_float (void)
{
  /*
    union u
    {
      double data;
      struct Lisp_Float *chain;
      char gcaligned __attribute__((aligned (GCALIGNMENT)));
    }

    struct Lisp_Float
    {
      union u u;
    }
   */

  comp.lisp_float_s
    = gcc_jit_context_new_opaque_struct (comp.ctxt, NULL,
					 "comp_Lisp_Float");

  comp.lisp_float_type = gcc_jit_struct_as_type (comp.lisp_float_s);
  comp.lisp_float_ptr_type = gcc_jit_type_get_pointer (comp.lisp_float_type);

  comp.lisp_float_u_data
    = gcc_jit_context_new_field (comp.ctxt, NULL, comp.double_type,
				 "data");
  comp.lisp_float_u_chain
    = gcc_jit_context_new_field (comp.ctxt, NULL,
				 comp.lisp_float_ptr_type, "chain");
  gcc_jit_field *u_fields[] = {
    comp.lisp_float_u_data,
    comp.lisp_float_u_chain,
    make_gcaligned_union_field (),
  };
  comp.lisp_float_u_type
    = gcc_jit_context_new_union_type (comp.ctxt, NULL,
				      "comp_Lisp_Float_u", 3, u_fields);

  comp.lisp_float_u
    = gcc_jit_context_new_field (comp.ctxt, NULL,
				 comp.lisp_float_u_type, "u");
  gcc_jit_struct_set_fields (comp.lisp_float_s, NULL, 1, &comp.lisp_float_u);
}

static void
define_lisp_subr (void)
{
  comp.lisp_subr_header
    = gcc_jit_context_new_field (comp.ctxt, NULL, comp.ptrdiff_type,
				 "header");
  comp.lisp_subr_function
    = gcc_jit_context_new_field (comp.ctxt, NULL, comp.void_ptr_type,
				 "function");
  comp.lisp_subr_min_args
    = gcc_jit_context_new_field (comp.ctxt, NULL, comp.short_type,
				 "min_args");
  comp.lisp_subr_max_args
    = gcc_jit_context_new_field (comp.ctxt, NULL, comp.short_type,
				 "max_args");
  comp.lisp_subr_symbol_name
    = gcc_jit_context_new_field (comp.ctxt, NULL,
				 gcc_jit_type_get_const (
				   comp.char_ptr_type),
				 "symbol_name");

  comp.lisp_subr_intspec_string = gcc_jit_context_new_field (comp.ctxt, NULL,
				 gcc_jit_type_get_const (
				   comp.char_ptr_type),
				 "string");
  comp.lisp_subr_intspec_native
    = gcc_jit_context_new_field (comp.ctxt, NULL, comp.lisp_obj_type,
				 "native");
  gcc_jit_field *intspec_fields[] = { comp.lisp_subr_intspec_string,
				      comp.lisp_subr_intspec_native };

  comp.lisp_subr_intspec_type
    = gcc_jit_context_new_union_type (comp.ctxt, NULL, "comp_intspec",
				      2, intspec_fields);
  comp.lisp_subr_intspec
    = gcc_jit_context_new_field (comp.ctxt, NULL,
				 comp.lisp_subr_intspec_type,
				 "intspec");

  comp.lisp_subr_command_modes
    = gcc_jit_context_new_field (comp.ctxt, NULL, comp.lisp_obj_type,
				 "command_modes");
  comp.lisp_subr_doc
    = gcc_jit_context_new_field (comp.ctxt, NULL, comp.emacs_int_type,
				 "doc");
  comp.lisp_subr_native_comp_u
    = gcc_jit_context_new_field (comp.ctxt, NULL, comp.lisp_obj_type,
				 "native_comp_u");
  comp.lisp_subr_native_c_name
    = gcc_jit_context_new_field (comp.ctxt, NULL, comp.char_ptr_type,
				 "native_c_name");
  comp.lisp_subr_lambda_list
    = gcc_jit_context_new_field (comp.ctxt, NULL, comp.lisp_obj_type,
				 "lamdba_list");
  comp.lisp_subr_type
    = gcc_jit_context_new_field (comp.ctxt, NULL, comp.lisp_obj_type,
				 "type");

  gcc_jit_field *lisp_subr_fields[]
    = { comp.lisp_subr_header,        comp.lisp_subr_function,
	comp.lisp_subr_min_args,      comp.lisp_subr_max_args,
	comp.lisp_subr_symbol_name,   comp.lisp_subr_intspec,
	comp.lisp_subr_command_modes, comp.lisp_subr_doc,
	comp.lisp_subr_native_comp_u, comp.lisp_subr_native_c_name,
	comp.lisp_subr_lambda_list,   comp.lisp_subr_type };

  comp.lisp_subr_s
    = gcc_jit_context_new_struct_type (comp.ctxt, NULL,
				       "comp_Lisp_Subr", 12,
				       lisp_subr_fields);
  comp.lisp_subr_s_type = gcc_jit_struct_as_type (comp.lisp_subr_s);
  comp.lisp_subr_s_gcaligned_type
    = gcc_jit_type_get_aligned (comp.lisp_subr_s_type, GCALIGNMENT);
}

static void
define_aligned_lisp_subr (void)
{
  comp.aligned_lisp_subr_s
    = gcc_jit_context_new_field (comp.ctxt, NULL,
				 comp.lisp_subr_s_gcaligned_type,
				 "s");
  gcc_jit_field *gcaligned = make_gcaligned_union_field ();
  gcc_jit_field *fields[] = { comp.aligned_lisp_subr_s, gcaligned };
  comp.aligned_lisp_subr_type
    = gcc_jit_context_new_union_type (comp.ctxt, NULL,
				      "comp_Aligned_Subr", 2, fields);
  comp.aligned_lisp_subr_ptr_type
    = gcc_jit_type_get_pointer (comp.aligned_lisp_subr_type);

  jit_vector_type_t pvec_type = make_lisp_vector_struct_type (
    VECSIZE (union Aligned_Lisp_Subr));
  gcc_jit_field *subr_pvec_vec
    = gcc_jit_context_new_field (comp.ctxt, NULL,
				 pvec_type.lisp_vector_type,
				 "vector");
  comp.aligned_lisp_subr_pvec_subr
    = gcc_jit_context_new_field (comp.ctxt, NULL,
				 comp.aligned_lisp_subr_type,
				 "aligned_subr");
  gcc_jit_field *pvec_fields[]
    = { subr_pvec_vec, comp.aligned_lisp_subr_pvec_subr };
  comp.aligned_lisp_subr_pvec_type
    = gcc_jit_context_new_union_type (comp.ctxt, NULL,
				      "comp_Lisp_Pseudovector_Subr",
				      2, pvec_fields);
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

#ifndef LIBGCCJIT_HAVE_gcc_jit_context_new_bitcast
static gcc_jit_function *
define_type_punning (const char *name,
		     gcc_jit_type *from, gcc_jit_field *from_field,
		     gcc_jit_type *to, gcc_jit_field *to_field)
{
  gcc_jit_param *param = gcc_jit_context_new_param (comp.ctxt, NULL,
                                                    from, "arg");
  gcc_jit_function *result = gcc_jit_context_new_function (comp.ctxt,
                               NULL,
                               GCC_JIT_FUNCTION_INTERNAL,
                               to,
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

  gcc_jit_block_add_assignment (entry_block, NULL,
                                gcc_jit_lvalue_access_field (tmp_union, NULL,
							     from_field),
                                gcc_jit_param_as_rvalue (param));

  gcc_jit_block_end_with_return (entry_block,
                                 NULL,
                                 gcc_jit_rvalue_access_field (
                                   gcc_jit_lvalue_as_rvalue (tmp_union),
                                   NULL, to_field));

  return result;
}

struct cast_type
{
  gcc_jit_type *type;
  const char *name;
  bool is_ptr;
};

static gcc_jit_function *
define_cast_from_to (struct cast_type from, struct cast_type to)
{
  char *name = format_string ("cast_from_%s_to_%s", from.name, to.name);
  gcc_jit_param *param = gcc_jit_context_new_param (comp.ctxt, NULL,
						    from.type, "arg");
  gcc_jit_function *result
    = gcc_jit_context_new_function (comp.ctxt,
				    NULL,
				    GCC_JIT_FUNCTION_INTERNAL,
				    to.type, name,
				    1, &param, 0);
  DECL_BLOCK (entry_block, result);

  gcc_jit_rvalue *tmp = gcc_jit_param_as_rvalue (param);
  if (from.is_ptr != to.is_ptr)
    {
      if (from.is_ptr)
	{
	  tmp = gcc_jit_context_new_cast (comp.ctxt, NULL,
					  tmp, comp.void_ptr_type);
	  tmp = gcc_jit_context_new_call (comp.ctxt, NULL,
					  comp.cast_ptr_to_int, 1, &tmp);
	}
      else
	{
	  tmp = gcc_jit_context_new_cast (comp.ctxt, NULL,
					  tmp, comp.uintptr_type);
	  tmp = gcc_jit_context_new_call (comp.ctxt, NULL,
					  comp.cast_int_to_ptr, 1, &tmp);
	}
    }

  tmp = gcc_jit_context_new_cast (comp.ctxt, NULL, tmp, to.type);

  gcc_jit_block_end_with_return (entry_block, NULL, tmp);

  return result;
}

static void
define_cast_functions (void)
{
  struct cast_type cast_types[NUM_CAST_TYPES]
    = { { comp.bool_type, "bool", false },
        { comp.char_ptr_type, "char_ptr", true },
        { comp.int_type, "int", false },
        { comp.lisp_cons_ptr_type, "lisp_cons_ptr", true },
        { comp.lisp_obj_ptr_type, "lisp_obj_ptr", true },
        { comp.lisp_word_tag_type, "lisp_word_tag", false },
        { comp.lisp_word_type, "lisp_word", LISP_WORDS_ARE_POINTERS },
        { comp.long_long_type, "long_long", false },
        { comp.long_type, "long", false },
        { comp.ptrdiff_type, "ptrdiff", false },
        { comp.uintptr_type, "uintptr", false },
        { comp.unsigned_long_long_type, "unsigned_long_long", false },
        { comp.unsigned_long_type, "unsigned_long", false },
        { comp.unsigned_type, "unsigned", false },
        { comp.void_ptr_type, "void_ptr", true } };
  gcc_jit_field *cast_union_fields[2];

  /* Define the union used for type punning.  */
  cast_union_fields[0] = gcc_jit_context_new_field (comp.ctxt,
						    NULL,
						    comp.void_ptr_type,
						    "void_ptr");
  cast_union_fields[1] = gcc_jit_context_new_field (comp.ctxt,
						    NULL,
						    comp.uintptr_type,
						    "uintptr");

  comp.cast_union_type
    = gcc_jit_context_new_union_type (comp.ctxt,
				      NULL,
				      "cast_union",
				      2, cast_union_fields);

  comp.cast_ptr_to_int = define_type_punning ("cast_pointer_to_uintptr_t",
					      comp.void_ptr_type,
					      cast_union_fields[0],
					      comp.uintptr_type,
					      cast_union_fields[1]);
  comp.cast_int_to_ptr = define_type_punning ("cast_uintptr_t_to_pointer",
					      comp.uintptr_type,
					      cast_union_fields[1],
					      comp.void_ptr_type,
					      cast_union_fields[0]);


  for (int i = 0; i < NUM_CAST_TYPES; ++i)
    comp.cast_types[i] = cast_types[i].type;

  /* Define the cast functions using a matrix.  */
  for (int i = 0; i < NUM_CAST_TYPES; ++i)
    for (int j = 0; j < NUM_CAST_TYPES; ++j)
        comp.cast_functions_from_to[i][j] =
          define_cast_from_to (cast_types[i], cast_types[j]);
}
#endif /* !LIBGCCJIT_HAVE_gcc_jit_context_new_bitcast */

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
define_GET_SYMBOL_WITH_POSITION (void)
{
  gcc_jit_param *param[] =
    { gcc_jit_context_new_param (comp.ctxt,
				 NULL,
				 comp.lisp_obj_type,
				 "a") };

  comp.get_symbol_with_position =
    gcc_jit_context_new_function (comp.ctxt, NULL,
				  GCC_JIT_FUNCTION_INTERNAL,
				  comp.lisp_symbol_with_position_ptr_type,
				  "GET_SYMBOL_WITH_POSITION",
				  1,
				  param,
				  0);

  DECL_BLOCK (entry_block, comp.get_symbol_with_position);

  comp.block = entry_block;
  comp.func = comp.get_symbol_with_position;

  gcc_jit_rvalue *args[] =
    { gcc_jit_param_as_rvalue (param[0]) };
  /* FIXME use XUNTAG now that's available.  */
  gcc_jit_block_end_with_return (
    entry_block,
    NULL,
    emit_call (intern_c_string ("helper_GET_SYMBOL_WITH_POSITION"),
	       comp.lisp_symbol_with_position_ptr_type,
	       1, args, false));
}

static void define_SYMBOL_WITH_POS_SYM (void)
{
  gcc_jit_rvalue *tmpr, *swp;
  gcc_jit_lvalue *tmpl;

  gcc_jit_param *param [] =
    { gcc_jit_context_new_param (comp.ctxt,
				 NULL,
				 comp.lisp_obj_type,
				 "a") };
  comp.symbol_with_pos_sym =
    gcc_jit_context_new_function (comp.ctxt, NULL,
				  GCC_JIT_FUNCTION_INTERNAL,
				  comp.lisp_obj_type,
				  "SYMBOL_WITH_POS_SYM",
				  1,
				  param,
				  0);

  DECL_BLOCK (entry_block, comp.symbol_with_pos_sym);
  comp.func = comp.symbol_with_pos_sym;
  comp.block = entry_block;

  emit_CHECK_SYMBOL_WITH_POS (gcc_jit_param_as_rvalue (param [0]));

  gcc_jit_rvalue *args[] = { gcc_jit_param_as_rvalue (param [0]) };

  swp = gcc_jit_context_new_call (comp.ctxt,
				  NULL,
				  comp.get_symbol_with_position,
				  1,
				  args);
  tmpl = gcc_jit_rvalue_dereference (swp, NULL);
  tmpr = gcc_jit_lvalue_as_rvalue (tmpl);
  gcc_jit_block_end_with_return (entry_block,
				 NULL,
				 gcc_jit_rvalue_access_field (
				   tmpr,
				   NULL,
				   comp.lisp_symbol_with_position_sym));
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

  gcc_jit_rvalue *pure_write_p;
  gcc_jit_rvalue *pure_p
    = emit_PURE_P (gcc_jit_param_as_rvalue (param[0]));

#if USE_COMP_STATIC_LISP_OBJECTS
  gcc_jit_rvalue *static_const_p
    = emit_static_comp_object_p (gcc_jit_param_as_rvalue (param[0]));
  pure_write_p
    = emit_binary_op (GCC_JIT_BINARY_OP_LOGICAL_OR, comp.bool_type,
		      pure_p, static_const_p);
#else
  pure_write_p = pure_p;
#endif

  emit_cond_jump (pure_write_p, /* FIXME */
		  err_block, ok_block);
  gcc_jit_block_end_with_void_return (ok_block, NULL);

  gcc_jit_rvalue *pure_write_error_arg
    = gcc_jit_param_as_rvalue (param[0]);

  comp.block = err_block;
  gcc_jit_block_add_eval (comp.block, NULL,
			  emit_call (intern_c_string (
				       "pure_write_error"),
				     comp.void_type, 1,
				     &pure_write_error_arg, false));

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
       used by the byte interpreter (see 'exec_byte_code').  */
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
  Lisp_Object c_name = CALL1I (comp-func-c-name, func);
  Lisp_Object args = CALL1I (comp-func-l-args, func);
  bool nargs = !NILP (CALL1I (comp-nargs-p, args));
  USE_SAFE_ALLOCA;

  if (!nargs)
    {
      EMACS_INT max_args = XFIXNUM (CALL1I (comp-args-max, args));
      eassert (max_args < INT_MAX);
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
					  SSDATA (c_name),
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
				      SSDATA (c_name),
				      ARRAYELTS (params), params, 0);
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
  comp.frame_size = XFIXNUM (CALL1I (comp-func-frame-size, func));
  eassert (comp.frame_size < INT_MAX);

  comp.func = xmint_pointer (Fgethash (CALL1I (comp-func-c-name, func),
				       comp.exported_funcs_h, Qnil));

  comp.func_has_non_local = !NILP (CALL1I (comp-func-has-non-local, func));
  comp.func_speed = XFIXNUM (CALL1I (comp-func-speed, func));

  comp.func_relocs_local =
    gcc_jit_function_new_local (comp.func,
				NULL,
				comp.func_relocs_ptr_type,
				"freloc");

  comp.frame = SAFE_ALLOCA (comp.frame_size * sizeof (*comp.frame));
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
                                          comp.frame_size),
          "frame");

      for (ptrdiff_t i = 0; i < comp.frame_size; ++i)
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
    for (ptrdiff_t i = 0; i < comp.frame_size; ++i)
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
  struct Lisp_Hash_Table *ht = XHASH_TABLE (blocks);
  for (ptrdiff_t i = 0; i < HASH_TABLE_SIZE (ht); i++)
    {
      Lisp_Object block_name = HASH_KEY (ht, i);
      if (!EQ (block_name, Qentry)
	  && !BASE_EQ (block_name, Qunbound))
	declare_block (block_name);
    }

  gcc_jit_block_add_assignment (retrive_block (Qentry),
				NULL,
				comp.func_relocs_local,
				gcc_jit_lvalue_as_rvalue (comp.func_relocs));


  for (ptrdiff_t i = 0; i < HASH_TABLE_SIZE (ht); i++)
    {
      Lisp_Object block_name = HASH_KEY (ht, i);
      if (!BASE_EQ (block_name, Qunbound))
	{
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

static Lisp_Object
make_directory_wrapper (Lisp_Object directory)
{
  CALL2I (make-directory, directory, Qt);
  return Qnil;
}

static Lisp_Object
make_directory_wrapper_1 (Lisp_Object ignore)
{
  return Qt;
}

DEFUN ("comp-el-to-eln-rel-filename", Fcomp_el_to_eln_rel_filename,
       Scomp_el_to_eln_rel_filename, 1, 1, 0,
       doc: /* Return the relative name of the .eln file for FILENAME.
FILENAME must exist, and if it's a symlink, the target must exist.
If FILENAME is compressed, it must have the \".gz\" extension,
and Emacs must have been compiled with zlib; the file will be
uncompressed on the fly to hash its contents.
Value includes the original base name, followed by 2 hash values,
one for the file name and another for its contents, followed by .eln.  */)
  (Lisp_Object filename)
{
  CHECK_STRING (filename);

  /* Resolve possible symlinks in FILENAME, so that path_hash below
     always compares equal. (Bug#44701).  */
  filename = Fexpand_file_name (filename, Qnil);
  char *file_normalized = realpath (SSDATA (ENCODE_FILE (filename)), NULL);
  if (file_normalized)
    {
      filename = DECODE_FILE (make_unibyte_string (file_normalized,
						   strlen (file_normalized)));
      xfree (file_normalized);
    }

  if (NILP (Ffile_exists_p (filename)))
    xsignal1 (Qfile_missing, filename);

#ifdef WINDOWSNT
  filename = Fw32_long_file_name (filename);
#endif

  Lisp_Object content_hash = comp_hash_source_file (filename);

  if (suffix_p (filename, ".gz"))
    filename = Fsubstring (filename, Qnil, make_fixnum (-3));

  /* We create eln filenames with an hash in order to look-up these
     starting from the source filename, IOW have a relation

     /absolute/path/filename.el + content ->
     eln-cache/filename-path_hash-content_hash.eln.

     'dlopen' can return the same handle if two shared with the same
     filename are loaded in two different times (even if the first was
     deleted!).  To prevent this scenario the source file content is
     included in the hashing algorithm.

     As at any point in time no more then one file can exist with the
     same filename, should be possible to clean up all
     filename-path_hash-* except the most recent one (or the new one
     being recompiled).

     As installing .eln files compiled during the build changes their
     absolute path we need an hashing mechanism that is not sensitive
     to that.  For this we replace if match PATH_DUMPLOADSEARCH or
     *PATH_REL_LOADSEARCH with '//' before computing the hash.  */

  if (NILP (loadsearch_re_list))
    {
      Lisp_Object sys_re =
	concat2 (build_string ("\\`[[:ascii:]]+"),
		 Fregexp_quote (build_string ("/" PATH_REL_LOADSEARCH "/")));
      Lisp_Object dump_load_search =
	Fexpand_file_name (build_string (PATH_DUMPLOADSEARCH "/"), Qnil);
#ifdef WINDOWSNT
      dump_load_search = Fw32_long_file_name (dump_load_search);
#endif
      loadsearch_re_list = list2 (sys_re, Fregexp_quote (dump_load_search));
    }

  Lisp_Object lds_re_tail = loadsearch_re_list;
  FOR_EACH_TAIL (lds_re_tail)
    {
      Lisp_Object match_idx =
	Fstring_match (XCAR (lds_re_tail), filename, Qnil, Qnil);
      if (BASE_EQ (match_idx, make_fixnum (0)))
	{
	  filename =
	    Freplace_match (build_string ("//"), Qt, Qt, filename, Qnil);
	  break;
	}
    }
  Lisp_Object separator = build_string ("-");
  Lisp_Object path_hash = comp_hash_string (filename);
  filename = concat2 (Ffile_name_nondirectory (Fsubstring (filename, Qnil,
							   make_fixnum (-3))),
		      separator);
  Lisp_Object hash = concat3 (path_hash, separator, content_hash);
  return concat3 (filename, hash, build_string (NATIVE_ELISP_SUFFIX));
}

DEFUN ("comp-el-to-eln-filename", Fcomp_el_to_eln_filename,
       Scomp_el_to_eln_filename, 1, 2, 0,
       doc: /* Return the absolute .eln file name for source FILENAME.
The resulting .eln file name is intended to be used for natively
compiling FILENAME.  FILENAME must exist and be readable, but other
than that, its leading directories are ignored when constructing
the name of the .eln file.
If BASE-DIR is non-nil, use it as the directory for the .eln file;
non-absolute BASE-DIR is interpreted as relative to `invocation-directory'.
If BASE-DIR is omitted or nil, look for the first writable directory
in `native-comp-eln-load-path', and use as BASE-DIR its subdirectory
whose name is given by `comp-native-version-dir'.
If FILENAME specifies a preloaded file, the directory for the .eln
file is the \"preloaded/\" subdirectory of the directory determined
as described above.  FILENAME is considered to be a preloaded file if
the value of `comp-file-preloaded-p' is non-nil, or if FILENAME
appears in the value of the environment variable LISP_PRELOADED;
the latter is supposed to be used by the Emacs build procedure.  */)
  (Lisp_Object filename, Lisp_Object base_dir)
{
  Lisp_Object source_filename = filename;
  filename = Fcomp_el_to_eln_rel_filename (filename);

  /* If base_dir was not specified search inside Vnative_comp_eln_load_path
     for the first directory where we have write access.  */
  if (NILP (base_dir))
    {
      Lisp_Object eln_load_paths = Vnative_comp_eln_load_path;
      FOR_EACH_TAIL (eln_load_paths)
	{
	  Lisp_Object dir = XCAR (eln_load_paths);
	  if (!NILP (Ffile_exists_p (dir)))
	    {
	      if (!NILP (Ffile_writable_p (dir)))
		{
		  base_dir = dir;
		  break;
		}
	    }
	  else
	    {
	      /* Try to create the directory and if succeeds use it.  */
	      if (NILP (internal_condition_case_1 (make_directory_wrapper,
						   dir, Qt,
						   make_directory_wrapper_1)))
		{
		  base_dir = dir;
		  break;
		}
	    }
	}
      if (NILP (base_dir))
	error ("Cannot find suitable directory for output in "
	       "`native-comp-eln-load-path'.");
    }

  if (!file_name_absolute_p (SSDATA (base_dir)))
    base_dir = Fexpand_file_name (base_dir, Vinvocation_directory);

  /* In case the file being compiled is found in 'LISP_PRELOADED' or
     `comp-file-preloaded-p' is non-nil target for output the
     'preloaded' subfolder.  */
  Lisp_Object lisp_preloaded =
    Fgetenv_internal (build_string ("LISP_PRELOADED"), Qnil);
  base_dir = Fexpand_file_name (Vcomp_native_version_dir, base_dir);
  if (comp_file_preloaded_p
      || (!NILP (lisp_preloaded)
	  && !NILP (Fmember (CALL1I (file-name-base, source_filename),
			     Fmapcar (intern_c_string ("file-name-base"),
				      CALL1I (split-string, lisp_preloaded))))))
    base_dir = Fexpand_file_name (build_string ("preloaded"), base_dir);

  return Fexpand_file_name (filename, base_dir);
}

DEFUN ("comp--install-trampoline", Fcomp__install_trampoline,
       Scomp__install_trampoline, 2, 2, 0,
       doc: /* Install a TRAMPOLINE for primitive SUBR-NAME.  */)
  (Lisp_Object subr_name, Lisp_Object trampoline)
{
  CHECK_SYMBOL (subr_name);
  CHECK_SUBR (trampoline);
  Lisp_Object orig_subr = Fsymbol_function (subr_name);
  CHECK_SUBR (orig_subr);

  /* FIXME: add a post dump load trampoline machinery to remove this
     check.  */
  if (will_dump_p ())
    signal_error ("Trying to advice unexpected primitive before dumping",
		  subr_name);

  Lisp_Object subr_l = Vcomp_subr_list;
  ptrdiff_t i = ARRAYELTS (helper_link_table);
  FOR_EACH_TAIL (subr_l)
    {
      Lisp_Object subr = XCAR (subr_l);
      if (EQ (subr, orig_subr))
	{
	  freloc.link_table[i] = XSUBR (trampoline)->function.a0;
	  Fputhash (subr_name, trampoline, Vcomp_installed_trampolines_h);
	  return Qt;
	}
      i++;
    }
    signal_error ("Trying to install trampoline for non existent subr",
		  subr_name);
    return Qnil;
}

DEFUN ("comp--init-ctxt", Fcomp__init_ctxt, Scomp__init_ctxt,
       0, 0, 0,
       doc: /* Initialize the native compiler context.
Return t on success.  */)
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

  comp.void_type = gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_VOID);
  comp.void_ptr_type =
    gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_VOID_PTR);
  comp.bool_type = gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_BOOL);
  comp.unsigned_char_type
    = gcc_jit_context_get_type (comp.ctxt,
				GCC_JIT_TYPE_UNSIGNED_CHAR);
  comp.char_type = gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_CHAR);
  comp.int_type = gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_INT);
  comp.double_type = gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_DOUBLE);
  comp.unsigned_type = gcc_jit_context_get_type (comp.ctxt,
						 GCC_JIT_TYPE_UNSIGNED_INT);
  comp.long_type = gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_LONG);
  comp.unsigned_long_type =
    gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_UNSIGNED_LONG);
  comp.long_long_type =
    gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_LONG_LONG);
  comp.unsigned_long_long_type =
    gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_UNSIGNED_LONG_LONG);
  comp.bool_ptr_type = gcc_jit_type_get_pointer (comp.bool_type);
  comp.unsigned_char_ptr_type
    = gcc_jit_type_get_pointer (comp.unsigned_char_type);
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
  comp.untagged_ptr_type = comp.char_ptr_type;
#else
  comp.lisp_word_type = comp.emacs_int_type;
  comp.untagged_ptr_type = comp.uintptr_type;
#endif
  comp.lisp_word_tag_type
    = gcc_jit_context_get_int_type (comp.ctxt, sizeof (Lisp_Word_tag), false);
  comp.bits_word_type
    = gcc_jit_context_get_int_type (comp.ctxt, sizeof (bits_word),
				    BITS_WORD_IS_SIGNED);
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
  comp.short_type
    = gcc_jit_context_get_int_type (comp.ctxt, sizeof (short), true);
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

#if USE_COMP_STATIC_LISP_OBJECTS
  comp.lisp_vector_structs_h = CALLN (Fmake_hash_table);
#endif

  define_lisp_cons ();
#if USE_COMP_STATIC_LISP_OBJECTS
  define_cons_block ();
#endif
  define_lisp_symbol_with_position ();
  define_lisp_string ();
  define_lisp_float ();
#if USE_COMP_STATIC_LISP_OBJECTS
  define_float_block ();
#endif
  define_lisp_vector ();
  define_lisp_subr ();
  define_aligned_lisp_subr ();
  define_jmp_buf ();
  define_handler_struct ();
  define_thread_state_struct ();
#ifndef LIBGCCJIT_HAVE_gcc_jit_context_new_bitcast
  define_cast_functions ();
#endif

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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Waddress"
DEFUN ("comp-native-driver-options-effective-p",
       Fcomp_native_driver_options_effective_p,
       Scomp_native_driver_options_effective_p,
       0, 0, 0,
       doc: /* Return t if `comp-native-driver-options' is effective.  */)
  (void)
{
#if defined (LIBGCCJIT_HAVE_gcc_jit_context_add_driver_option)
  if (gcc_jit_context_add_driver_option)
    return Qt;
#endif
  return Qnil;
}
#pragma GCC diagnostic pop

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Waddress"
DEFUN ("comp-native-compiler-options-effective-p",
       Fcomp_native_compiler_options_effective_p,
       Scomp_native_compiler_options_effective_p,
       0, 0, 0,
       doc: /* Return t if `comp-native-compiler-options' is effective.  */)
  (void)
{
#if defined (LIBGCCJIT_HAVE_gcc_jit_context_add_command_line_option)
  if (gcc_jit_context_add_command_line_option)
    return Qt;
#endif
  return Qnil;
}
#pragma GCC diagnostic pop

static void
add_driver_options (void)
{
  Lisp_Object options = Fsymbol_value (Qnative_comp_driver_options);

#if defined (LIBGCCJIT_HAVE_gcc_jit_context_add_driver_option)
  load_gccjit_if_necessary (true);
  if (!NILP (Fcomp_native_driver_options_effective_p ()))
    FOR_EACH_TAIL (options)
      gcc_jit_context_add_driver_option (comp.ctxt,
					 /* FIXME: Need to encode
					    this, but how? either
					    ENCODE_FILE or
					    ENCODE_SYSTEM.  */
					 SSDATA (XCAR (options)));
#endif
  if (CONSP (options))
    xsignal1 (Qnative_compiler_error,
	      build_string ("Customizing native compiler options"
			    " via `comp-native-driver-options' is"
			    " only available on libgccjit version 9"
			    " and above."));

  /* Captured `comp-native-driver-options' because file-local.  */
#if defined (LIBGCCJIT_HAVE_gcc_jit_context_add_driver_option)
  options = comp.driver_options;
  if (!NILP (Fcomp_native_driver_options_effective_p ()))
    FOR_EACH_TAIL (options)
      gcc_jit_context_add_driver_option (comp.ctxt,
					 /* FIXME: Need to encode
					    this, but how? either
					    ENCODE_FILE or
					    ENCODE_SYSTEM.  */
					 SSDATA (XCAR (options)));
#endif
}

static void
add_compiler_options (void)
{
  Lisp_Object options = Fsymbol_value (Qnative_comp_compiler_options);

#if defined (LIBGCCJIT_HAVE_gcc_jit_context_add_command_line_option)
  load_gccjit_if_necessary (true);
  if (!NILP (Fcomp_native_compiler_options_effective_p ()))
    FOR_EACH_TAIL (options)
        gcc_jit_context_add_command_line_option (comp.ctxt,
                                                 /* FIXME: Need to encode
                                                    this, but how? either
                                                    ENCODE_FILE or
                                                    ENCODE_SYSTEM.  */
                                                 SSDATA (XCAR (options)));
#endif
  if (CONSP (options))
    xsignal1 (Qnative_compiler_error,
	      build_string ("Customizing native compiler options"
			    " via `comp-native-compiler-options' is"
			    " only available on libgccjit version 9"
			    " and above."));

  /* Captured `comp-native-compiler-options' because file-local.  */
#if defined (LIBGCCJIT_HAVE_gcc_jit_context_add_command_line_option)
  options = comp.compiler_options;
  if (!NILP (Fcomp_native_compiler_options_effective_p ()))
    FOR_EACH_TAIL (options)
      gcc_jit_context_add_command_line_option (comp.ctxt,
                                               /* FIXME: Need to encode
                                                  this, but how? either
                                                  ENCODE_FILE or
                                                  ENCODE_SYSTEM.  */
                                               SSDATA (XCAR (options)));
#endif
}

DEFUN ("comp--compile-ctxt-to-file", Fcomp__compile_ctxt_to_file,
       Scomp__compile_ctxt_to_file,
       1, 1, 0,
       doc: /* Compile the current context as native code to file FILENAME.  */)
  (Lisp_Object filename)
{
  load_gccjit_if_necessary (true);

  CHECK_STRING (filename);
  Lisp_Object base_name = Fsubstring (filename, Qnil, make_fixnum (-4));
  Lisp_Object ebase_name = ENCODE_FILE (base_name);

  comp.func_relocs_local = NULL;

#ifdef WINDOWSNT
  ebase_name = ansi_encode_filename (ebase_name);
  /* Tell libgccjit the actual file name of the loaded DLL, otherwise
     it will use 'libgccjit.so', which is not useful.  */
  Lisp_Object libgccjit_loaded_from = Fget (Qgccjit, QCloaded_from);
  Lisp_Object libgccjit_fname;

  if (CONSP (libgccjit_loaded_from))
    {
      /* Use the absolute file name if available, otherwise the name
	 we looked for in w32_delayed_load.  */
      libgccjit_fname = XCDR (libgccjit_loaded_from);
      if (NILP (libgccjit_fname))
	libgccjit_fname = XCAR (libgccjit_loaded_from);
      /* Must encode to ANSI, as libgccjit will not be able to handle
	 UTF-8 encoded file names.  */
      libgccjit_fname = ENCODE_FILE (libgccjit_fname);
      libgccjit_fname = ansi_encode_filename (libgccjit_fname);
      gcc_jit_context_set_str_option (comp.ctxt, GCC_JIT_STR_OPTION_PROGNAME,
				      SSDATA (libgccjit_fname));
    }
  else	/* this should never happen */
    gcc_jit_context_set_str_option (comp.ctxt, GCC_JIT_STR_OPTION_PROGNAME,
				    "libgccjit-0.dll");
#endif

  comp.speed = XFIXNUM (CALL1I (comp-ctxt-speed, Vcomp_ctxt));
  eassert (comp.speed < INT_MAX);
  comp.debug = XFIXNUM (CALL1I (comp-ctxt-debug, Vcomp_ctxt));
  eassert (comp.debug < INT_MAX);
  comp.driver_options = CALL1I (comp-ctxt-driver-options, Vcomp_ctxt);
  comp.compiler_options = CALL1I (comp-ctxt-compiler-options, Vcomp_ctxt);

# if USE_COMP_STATIC_LISP_OBJECTS
  comp.compile_static_data
    = !NILP (CALL1I (comp-ctxt-with-static-data,
					    Vcomp_ctxt));
# endif

  if (comp.debug)
      gcc_jit_context_set_bool_option (comp.ctxt,
				       GCC_JIT_BOOL_OPTION_DEBUGINFO,
				       1);
  if (comp.debug >= 3)
    {
      logfile = emacs_fopen ("libgccjit.log", "w");
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

  gcc_jit_context_set_int_option (comp.ctxt,
				  GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL,
				  comp.speed < 0 ? 0
				  : (comp.speed > 3 ? 3 : comp.speed));

  /* On MacOS set a unique dylib ID.  */
#if defined (LIBGCCJIT_HAVE_gcc_jit_context_add_driver_option)	\
  && defined (DARWIN_OS)
  gcc_jit_context_add_driver_option (comp.ctxt, "-install_name");
  gcc_jit_context_add_driver_option (
         comp.ctxt, SSDATA (Ffile_name_nondirectory (filename)));
#endif

#if USE_COMP_STATIC_LISP_OBJECTS
  if (comp.compile_static_data)
    {
      comp.static_lisp_data_count = 0;
      comp.static_hash_cons_h
	= CALLN (Fmake_hash_table, QCtest,
		 intern_c_string ("comp-imm-equal-test"));
      comp.lisp_consts_init_lvals = Qnil;
      comp.lambda_init_lvals = Qnil;
      comp.cons_block_list = Qnil;
      comp.float_block_list = Qnil;

#  define INIT_STORAGE_PTR(field, name)                         \
   do                                                           \
    {                                                           \
     (field)                                                    \
       = gcc_jit_context_new_global (comp.ctxt, NULL,           \
				     GCC_JIT_GLOBAL_INTERNAL,   \
				     gcc_jit_type_get_const (   \
				       comp.lisp_obj_ptr_type), \
				     (name));                   \
    }                                                           \
   while (0)

      INIT_STORAGE_PTR (comp.d_staticvec_ptr_var, "d_default_ptr");
      INIT_STORAGE_PTR (comp.d_ephemeral_ptr_var, "d_ephemeral_ptr");
#  undef INIT_STORAGE_PTR
    }
  else
    {
#endif
      comp.d_default_idx =
	CALL1I (comp-data-container-idx,
		CALL1I (comp-ctxt-d-default, Vcomp_ctxt));
      comp.d_impure_idx =
	CALL1I (comp-data-container-idx,
		CALL1I (comp-ctxt-d-impure, Vcomp_ctxt));
      comp.d_ephemeral_idx =
	CALL1I (comp-data-container-idx,
		CALL1I (comp-ctxt-d-ephemeral, Vcomp_ctxt));

#if USE_COMP_STATIC_LISP_OBJECTS
    }
#endif

  comp.block = NULL;
  emit_ctxt_code ();

  /* Define inline functions.  */
  define_CAR_CDR ();
  define_PSEUDOVECTORP ();
  define_GET_SYMBOL_WITH_POSITION ();
  define_CHECK_TYPE ();
  define_SYMBOL_WITH_POS_SYM ();
  define_CHECK_IMPURE ();
  define_bool_to_lisp_obj ();
  define_setcar_setcdr ();
  define_add1_sub1 ();
  define_negate ();
  define_maybe_gc_or_quit ();

  struct Lisp_Hash_Table *func_h =
    XHASH_TABLE (CALL1I (comp-ctxt-funcs-h, Vcomp_ctxt));
  for (ptrdiff_t i = 0; i < HASH_TABLE_SIZE (func_h); i++)
    if (!BASE_EQ (HASH_VALUE (func_h, i), Qunbound))
      declare_function (HASH_VALUE (func_h, i));
  /* Compile all functions. Can't be done before because the
     relocation structs has to be already defined.  */
  for (ptrdiff_t i = 0; i < HASH_TABLE_SIZE (func_h); i++)
    if (!BASE_EQ (HASH_VALUE (func_h, i), Qunbound))
      compile_function (HASH_VALUE (func_h, i));

#if USE_COMP_STATIC_LISP_OBJECTS
  if (comp.compile_static_data)
    define_init_objs ();
#endif

  /* Work around bug#46495 (GCC PR99126). */
#if defined (WIDE_EMACS_INT)						\
  && defined (LIBGCCJIT_HAVE_gcc_jit_context_add_command_line_option)
  Lisp_Object version = Fcomp_libgccjit_version ();
  if (NILP (version)
      || XFIXNUM (XCAR (version)) < 11)
    gcc_jit_context_add_command_line_option (comp.ctxt,
					     "-fdisable-tree-isolate-paths");
#endif

  add_compiler_options ();
  add_driver_options ();

  if (comp.debug > 1)
      gcc_jit_context_dump_to_file (comp.ctxt,
				    format_string ("%s.c", SSDATA (ebase_name)),
				    1);
  if (!NILP (Fsymbol_value (Qcomp_libgccjit_reproducer)))
    gcc_jit_context_dump_reproducer_to_file (
      comp.ctxt,
      format_string ("%s_libgccjit_repro.c", SSDATA (ebase_name)));

  Lisp_Object tmp_file =
    CALL4I (make-temp-file, base_name, Qnil, build_string (".eln.tmp"), Qnil);

  Lisp_Object encoded_tmp_file = ENCODE_FILE (tmp_file);
#ifdef WINDOWSNT
  encoded_tmp_file = ansi_encode_filename (encoded_tmp_file);
#endif
  gcc_jit_context_compile_to_file (comp.ctxt,
				   GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY,
				   SSDATA (encoded_tmp_file));

  const char *err =  gcc_jit_context_get_first_error (comp.ctxt);
  if (err)
    xsignal3 (Qnative_ice,
	      build_string ("failed to compile"),
	      filename,
	      build_string (err));

  CALL1I (comp-clean-up-stale-eln, filename);
  CALL2I (comp-delete-or-replace-file, filename, tmp_file);

  return filename;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Waddress"
DEFUN ("comp-libgccjit-version", Fcomp_libgccjit_version,
       Scomp_libgccjit_version, 0, 0, 0,
       doc: /* Return libgccjit version in use.

The return value has the form (MAJOR MINOR PATCHLEVEL) or nil if
unknown (before GCC version 10).  */)
  (void)
{
#if defined (LIBGCCJIT_HAVE_gcc_jit_version)
  load_gccjit_if_necessary (true);

  return gcc_jit_version_major
    ? list3 (make_fixnum (gcc_jit_version_major ()),
	     make_fixnum (gcc_jit_version_minor ()),
	     make_fixnum (gcc_jit_version_patchlevel ()))
    : Qnil;
#else
  return Qnil;
#endif
}
#pragma GCC diagnostic pop


/******************************************************************************/
/* Helper functions called from the run-time.				      */
/* Note: this are all potentially definable directly to gcc and are here just */
/* for laziness. Change this if a performance impact is measured.             */
/******************************************************************************/

static void
helper_unwind_protect (Lisp_Object handler)
{
  /* Support for a function here is new in 24.4.  */
  record_unwind_protect (FUNCTIONP (handler) ? bcall0 : prog_ignore,
			 handler);
}

static Lisp_Object
helper_unbind_n (Lisp_Object n)
{
  return unbind_to (specpdl_ref_add (SPECPDL_INDEX (), -XFIXNUM (n)), Qnil);
}

static void
helper_save_restriction (void)
{
  record_unwind_protect (save_restriction_restore,
			 save_restriction_save ());
}

static bool
helper_PSEUDOVECTOR_TYPEP_XUNTAG (Lisp_Object a, enum pvec_type code)
{
  return PSEUDOVECTOR_TYPEP (XUNTAG (a, Lisp_Vectorlike,
				     union vectorlike_header),
			     code);
}

static struct Lisp_Symbol_With_Pos *
helper_GET_SYMBOL_WITH_POSITION (Lisp_Object a)
{
  if (!SYMBOL_WITH_POS_P (a))
    wrong_type_argument (Qwrong_type_argument, a);
  return XUNTAG (a, Lisp_Vectorlike, struct Lisp_Symbol_With_Pos);
}

#if USE_COMP_STATIC_LISP_OBJECTS
static bool
helper_static_comp_object_p (Lisp_Object o)
{
  return static_comp_object_p (o);
}
#endif


/* `native-comp-eln-load-path' clean-up support code.  */

#ifdef WINDOWSNT
static Lisp_Object
return_nil (Lisp_Object arg)
{
  return Qnil;
}

static Lisp_Object
directory_files_matching (Lisp_Object name, Lisp_Object match)
{
  return Fdirectory_files (name, Qt, match, Qnil, Qnil);
}
#endif

/* Windows does not let us delete a .eln file that is currently loaded
   by a process.  The strategy is to rename .eln files into .old.eln
   instead of removing them when this is not possible and clean-up
   `native-comp-eln-load-path' when exiting.

   Any error is ignored because it may be due to the file being loaded
   in another Emacs instance.  */
void
eln_load_path_final_clean_up (void)
{
#ifdef WINDOWSNT
  Lisp_Object dir_tail = Vnative_comp_eln_load_path;
  FOR_EACH_TAIL (dir_tail)
    {
      Lisp_Object files_in_dir =
	internal_condition_case_2 (directory_files_matching,
				   Fexpand_file_name (Vcomp_native_version_dir,
						      XCAR (dir_tail)),
				   build_string ("\\.eln\\.old\\'"),
				   Qt, return_nil);
      FOR_EACH_TAIL (files_in_dir)
	internal_delete_file (XCAR (files_in_dir));
    }
#endif
}

/* This function puts the compilation unit in the
  `Vcomp_loaded_comp_units_h` hashmap.  */
static void
register_native_comp_unit (Lisp_Object comp_u)
{
  Fputhash (
    XNATIVE_COMP_UNIT (comp_u)->file, comp_u, Vcomp_loaded_comp_units_h);
  /* TODO: This is a hack to avoid use-after-free issues with
     heap-allocated objects being in static objects not getting marked
     after a native comp unit is freed.  */
  if (XNATIVE_COMP_UNIT (comp_u)->have_static_lisp_data)
    pin_object (comp_u);
}


/***********************************/
/* Deferred compilation mechanism. */
/***********************************/

/* Queue an asynchronous compilation for the source file defining
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

  if (!native_comp_jit_compilation
      || noninteractive
      || !NILP (Vpurify_flag)
      || !COMPILEDP (definition)
      || !STRINGP (Vload_true_file_name)
      || !suffix_p (Vload_true_file_name, ".elc")
      || !NILP (Fgethash (Vload_true_file_name, V_comp_no_native_file_h, Qnil)))
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

  Fputhash (function_name, definition, Vcomp_deferred_pending_h);

  /* This is so deferred compilation is able to compile comp
     dependencies breaking circularity.  */
  if (comp__compilable)
    {
      /* Startup is done, comp is usable.  */
      CALL0I (startup--require-comp-safely);
      CALLN (Ffuncall, intern_c_string ("native--compile-async"),
	     src, Qnil, Qlate);
    }
  else
    Vcomp__delayed_sources = Fcons (src, Vcomp__delayed_sources);
}


/**************************************/
/* Functions used to load eln files.  */
/**************************************/

/* Fixup the system eln-cache directory, which is the last entry in
   `native-comp-eln-load-path'.  Argument is a .eln file in that directory.  */
void
fixup_eln_load_path (Lisp_Object eln_filename)
{
  Lisp_Object last_cell = Qnil;
  Lisp_Object tem = Vnative_comp_eln_load_path;
  FOR_EACH_TAIL (tem)
    if (CONSP (tem))
      last_cell = tem;

  const char preloaded[] = "/preloaded/";
  Lisp_Object eln_cache_sys = Ffile_name_directory (eln_filename);
  const char *p_preloaded =
    SSDATA (eln_cache_sys) + SBYTES (eln_cache_sys) - sizeof (preloaded) + 1;
  bool preloaded_p = strcmp (p_preloaded, preloaded) == 0;

  /* One or two directories up...  */
  for (int i = 0; i < (preloaded_p ? 2 : 1); i++)
    eln_cache_sys =
      Ffile_name_directory (Fsubstring_no_properties (eln_cache_sys, Qnil,
						      make_fixnum (-1)));
  Fsetcar (last_cell, eln_cache_sys);
}

typedef char *(*comp_lit_str_func) (void);

/* Deserialize read and return static object.  */
static Lisp_Object
load_static_obj (struct Lisp_Native_Comp_Unit *comp_u, const char *name)
{
#if USE_COMP_STATIC_LISP_OBJECTS
  if (comp_u->have_static_lisp_data)
    {
      Lisp_Object *obj = dynlib_sym (comp_u->handle, name);
      if (obj)
	return *obj;

      xsignal1 (Qnative_lisp_file_inconsistent, comp_u->file);
    }
#endif

  static_obj_t *blob =
    dynlib_sym (comp_u->handle, format_string ("%s_blob", name));
  if (blob)
    /* New blob format.  */
    return Fread (make_string (blob->data, blob->len));

  static_obj_t *(*f)(void) = dynlib_sym (comp_u->handle, name);
  if (!f)
    xsignal1 (Qnative_lisp_file_inconsistent, comp_u->file);

  blob = f ();
  return Fread (make_string (blob->data, blob->len));

}

/* Return false when something is wrong or true otherwise.  */
static bool
check_comp_unit_relocs (struct Lisp_Native_Comp_Unit *comp_u)
{
  if (comp_u->have_static_lisp_data)
    return false;

  dynlib_handle_ptr handle = comp_u->handle;
  Lisp_Object *data_relocs = dynlib_sym (handle, DATA_RELOC_SYM);
  Lisp_Object *data_imp_relocs = dynlib_sym (handle, DATA_RELOC_IMPURE_SYM);

  EMACS_INT d_vec_len = XFIXNUM (Flength (comp_u->data_vec));
  for (ptrdiff_t i = 0; i < d_vec_len; i++)
    if (!EQ (data_relocs[i],  AREF (comp_u->data_vec, i)))
      return false;

  d_vec_len = XFIXNUM (Flength (comp_u->data_impure_vec));
  for (ptrdiff_t i = 0; i < d_vec_len; i++)
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

Lisp_Object
load_comp_unit (struct Lisp_Native_Comp_Unit *comp_u, bool loading_dump,
		bool late_load)
{
  Lisp_Object res = Qnil;
  dynlib_handle_ptr handle = comp_u->handle;
  Lisp_Object comp_u_lisp_obj;
  XSETNATIVE_COMP_UNIT (comp_u_lisp_obj, comp_u);

  Lisp_Object *saved_cu = dynlib_sym (handle, COMP_UNIT_SYM);
  if (!saved_cu)
    xsignal1 (Qnative_lisp_file_inconsistent, comp_u->file);
  comp_u->loaded_once = !NILP (*saved_cu);

  Lisp_Object *data_eph_relocs
    = comp_u->have_static_lisp_data
      ? NULL
      : dynlib_sym (handle, DATA_RELOC_EPHEMERAL_SYM);

  /* While resurrecting from an image dump loading more than once the
     same compilation unit does not make any sense.  */
  eassert (!(loading_dump && comp_u->loaded_once));

  if (comp_u->loaded_once)
    /* 'dlopen' returns the same handle when trying to load two times
       the same shared.  In this case touching 'd_reloc' etc leads to
       fails in case a frame with a reference to it in a live reg is
       active (native-comp-speed > 0).

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
  specpdl_ref count = SPECPDL_INDEX ();
  if (!recursive_load)
    record_unwind_protect (unset_cu_load_ongoing, comp_u_lisp_obj);

  freloc_check_fill ();

  Lisp_Object (*top_level_run)(Lisp_Object)
    = dynlib_sym (handle,
		  late_load ? "late_top_level_run" : "top_level_run");

  if (!comp_u->have_static_lisp_data)
    /* Always set data_imp_relocs pointer in the compilation unit (in
       can be used in 'dump_do_dump_relocation').  */
    comp_u->data_imp_relocs
      = dynlib_sym (handle, DATA_RELOC_IMPURE_SYM);

  if (!comp_u->loaded_once)
    {
      struct thread_state ***current_thread_reloc =
	dynlib_sym (handle, CURRENT_THREAD_RELOC_SYM);
      bool **f_symbols_with_pos_enabled_reloc =
	dynlib_sym (handle, F_SYMBOLS_WITH_POS_ENABLED_RELOC_SYM);
      void **pure_reloc = dynlib_sym (handle, PURE_RELOC_SYM);
      bool data_valid = false;

#if USE_COMP_STATIC_LISP_OBJECTS
      Lisp_Object *data_staticpro;
      Lisp_Object (*comp_init_objs) (Lisp_Object);
      if (comp_u->have_static_lisp_data)
	{
	  data_staticpro = dynlib_sym (handle, DATA_STATICPRO_SYM);
	  comp_init_objs = dynlib_sym (handle, "comp_init_objs");
	  data_valid = data_staticpro && comp_init_objs;
	}
#endif
      Lisp_Object *data_relocs;
      Lisp_Object *data_imp_relocs;
      if (!comp_u->have_static_lisp_data)
	{
	  eassert (!data_valid);

          data_relocs = dynlib_sym (handle, DATA_RELOC_SYM);
          data_imp_relocs = comp_u->data_imp_relocs;
	  data_valid = data_relocs && data_imp_relocs && data_eph_relocs;
	}

      void **freloc_link_table = dynlib_sym (handle, FUNC_LINK_TABLE_SYM);

      if (!(current_thread_reloc
	    && f_symbols_with_pos_enabled_reloc
	    && pure_reloc
	    && data_valid
	    && freloc_link_table
	    && top_level_run)
	  || NILP (Fstring_equal (load_static_obj (comp_u, LINK_TABLE_HASH_SYM),
				  Vcomp_abi_hash)))
	xsignal1 (Qnative_lisp_file_inconsistent, comp_u->file);

      *current_thread_reloc = &current_thread;
      *f_symbols_with_pos_enabled_reloc = &symbols_with_pos_enabled;
      *pure_reloc = pure;

      /* Imported functions.  */
      *freloc_link_table = freloc.link_table;

#if USE_COMP_STATIC_LISP_OBJECTS
      if (comp_u->have_static_lisp_data)
	{
	  comp_u->staticpro = *data_staticpro;
	  comp_u->ephemeral
	    = load_static_obj (comp_u, DATA_EPHEMERAL_SYM);
	  if (!recursive_load)
	    comp_init_objs (comp_u_lisp_obj);
	}
#endif

      /* Imported data.  */
      if (!loading_dump)
	{
	  comp_u->optimize_qualities =
	    load_static_obj (comp_u, TEXT_OPTIM_QLY_SYM);
	  if (!comp_u->have_static_lisp_data)
	    {
	      comp_u->data_vec = load_static_obj (comp_u, TEXT_DATA_RELOC_SYM);
	      comp_u->data_impure_vec =
		load_static_obj (comp_u, TEXT_DATA_RELOC_IMPURE_SYM);

	      if (!NILP (Vpurify_flag))
		/* Non impure can be copied into pure space.  */
		comp_u->data_vec = Fpurecopy (comp_u->data_vec);
	    }
	}

      if (!comp_u->have_static_lisp_data)
	{
	  EMACS_INT d_vec_len = XFIXNUM (Flength (comp_u->data_vec));
	  for (EMACS_INT i = 0; i < d_vec_len; i++)
	    data_relocs[i] = AREF (comp_u->data_vec, i);

	  d_vec_len = XFIXNUM (Flength (comp_u->data_impure_vec));
	  for (EMACS_INT i = 0; i < d_vec_len; i++)
	    data_imp_relocs[i] = AREF (comp_u->data_impure_vec, i);
	}
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
      Lisp_Object volatile data_ephemeral_vec = Qnil;
      /* In case another load of the same CU is active on the stack
	 all ephemeral data is hold by that frame.  Re-writing
	 'data_ephemeral_vec' would be not only a waste of cycles but
	 more importantly would lead to crashes if the contained data
	 is not cons hashed.  */
      if (!recursive_load && !comp_u->have_static_lisp_data)
	{
	  data_ephemeral_vec =
	    load_static_obj (comp_u, TEXT_DATA_RELOC_EPHEMERAL_SYM);

	  EMACS_INT d_vec_len = XFIXNUM (Flength (data_ephemeral_vec));
	  for (EMACS_INT i = 0; i < d_vec_len; i++)
	    data_eph_relocs[i] = AREF (data_ephemeral_vec, i);
	}
      /* Executing this will perform all the expected environment
	 modifications.  */
      res = top_level_run (comp_u_lisp_obj);


      if (!comp_u->have_static_lisp_data)
	{
	  /* Make sure data_ephemeral_vec still exists after
	     top_level_run has run. Guard against sibling call
	     optimization (or any other).  */
	  data_ephemeral_vec = data_ephemeral_vec;
	  eassert (check_comp_unit_relocs (comp_u));
	}
    }

  if (!recursive_load)
    /* Clean-up the load ongoing flag in case.  */
    unbind_to (count, Qnil);

  register_native_comp_unit (comp_u_lisp_obj);

#if USE_COMP_STATIC_LISP_OBJECTS
  if (comp_u->have_static_lisp_data)
    comp_u->ephemeral = Qnil;
#endif
  return res;
}

void
unload_comp_unit (struct Lisp_Native_Comp_Unit *cu)
{
  if (cu->handle == NULL)
    return;

  if (cu->have_static_lisp_data)
    return;

  Lisp_Object *saved_cu = dynlib_sym (cu->handle, COMP_UNIT_SYM);
  Lisp_Object this_cu;
  XSETNATIVE_COMP_UNIT (this_cu, cu);
  if (EQ (this_cu, *saved_cu))
    *saved_cu = Qnil;
  dynlib_close (cu->handle);
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
	   Lisp_Object c_name, Lisp_Object type, Lisp_Object doc_idx,
	   Lisp_Object intspec, Lisp_Object command_modes, Lisp_Object comp_u)
{
  struct Lisp_Native_Comp_Unit *cu = XNATIVE_COMP_UNIT (comp_u);
  dynlib_handle_ptr handle = cu->handle;
  if (!handle)
    xsignal0 (Qwrong_register_subr_call);

  void *func = dynlib_sym (handle, SSDATA (c_name));
  eassert (func);
  union Aligned_Lisp_Subr *x = (union Aligned_Lisp_Subr *)
    allocate_pseudovector (VECSIZE (union Aligned_Lisp_Subr), 0,
                           VECSIZE (union Aligned_Lisp_Subr),
                           PVEC_SUBR);
  if (CONSP (minarg))
    {
      /* Dynamic code.  */
#ifdef HAVE_NATIVE_COMP
      x->s.lambda_list = maxarg;
#endif
      maxarg = XCDR (minarg);
      minarg = XCAR (minarg);
    }
  else
    {
#ifdef HAVE_NATIVE_COMP
      x->s.lambda_list = Qnil;
#endif
    }
  x->s.function.a0 = func;
  x->s.min_args = XFIXNUM (minarg);
  x->s.max_args = FIXNUMP (maxarg) ? XFIXNUM (maxarg) : MANY;
  x->s.symbol_name = xstrdup (SSDATA (symbol_name));
  x->s.intspec.native = intspec;
  x->s.command_modes = command_modes;
  x->s.doc = XFIXNUM (doc_idx);
#ifdef HAVE_NATIVE_COMP
  x->s.native_comp_u = comp_u;
  x->s.native_c_name = xstrdup (SSDATA (c_name));
  x->s.type = type;
#endif
  Lisp_Object tem;
  XSETSUBR (tem, &x->s);

  return tem;

}

DEFUN ("comp--register-lambda", Fcomp__register_lambda, Scomp__register_lambda,
       7, 7, 0,
       doc: /* Register anonymous lambda.
This gets called by top_level_run during the load phase.  */)
  (Lisp_Object reloc_idx, Lisp_Object c_name, Lisp_Object minarg,
   Lisp_Object maxarg, Lisp_Object type, Lisp_Object rest,
   Lisp_Object comp_u)
{
#if USE_COMP_STATIC_LISP_OBJECTS
  eassert (!XNATIVE_COMP_UNIT (comp_u)->have_static_lisp_data);
#endif
  Lisp_Object doc_idx = FIRST (rest);
  Lisp_Object intspec = SECOND (rest);
  Lisp_Object command_modes = THIRD (rest);

  struct Lisp_Native_Comp_Unit *cu = XNATIVE_COMP_UNIT (comp_u);
  if (cu->loaded_once)
    return Qnil;

  Lisp_Object tem =
    make_subr (c_name, minarg, maxarg, c_name, type, doc_idx, intspec,
	       command_modes, comp_u);

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
       doc: /* Register exported subr.
This gets called by top_level_run during the load phase.  */)
  (Lisp_Object name, Lisp_Object c_name, Lisp_Object minarg,
   Lisp_Object maxarg, Lisp_Object type, Lisp_Object rest,
   Lisp_Object comp_u)
{
  Lisp_Object doc_idx = FIRST (rest);
  Lisp_Object intspec = SECOND (rest);
  Lisp_Object command_modes = THIRD (rest);

  Lisp_Object tem =
    make_subr (SYMBOL_NAME (name), minarg, maxarg, c_name, type, doc_idx,
	       intspec, command_modes, comp_u);

  defalias (name, tem);

  return tem;
}

DEFUN ("comp--late-register-subr", Fcomp__late_register_subr,
       Scomp__late_register_subr, 7, 7, 0,
       doc: /* Register exported subr.
This gets called by late_top_level_run during the load phase.  */)
  (Lisp_Object name, Lisp_Object c_name, Lisp_Object minarg,
   Lisp_Object maxarg, Lisp_Object type, Lisp_Object rest,
   Lisp_Object comp_u)
{
  if (!NILP (Fequal (Fsymbol_function (name),
		     Fgethash (name, Vcomp_deferred_pending_h, Qnil))))
    Fcomp__register_subr (name, c_name, minarg, maxarg, type, rest, comp_u);
  Fremhash (name, Vcomp_deferred_pending_h);
  return Qnil;
}

static bool
file_in_eln_sys_dir (Lisp_Object filename)
{
  Lisp_Object eln_sys_dir = Qnil;
  Lisp_Object tmp = Vnative_comp_eln_load_path;
  FOR_EACH_TAIL (tmp)
    eln_sys_dir = XCAR (tmp);
  return !NILP (Fstring_match (Fregexp_quote (Fexpand_file_name (eln_sys_dir,
								 Qnil)),
			       Fexpand_file_name (filename, Qnil),
			       Qnil, Qnil));
}

/* Load related routines.  */
DEFUN ("native-elisp-load", Fnative_elisp_load, Snative_elisp_load, 1, 2, 0,
       doc: /* Load native elisp code FILENAME.
LATE-LOAD has to be non-nil when loading for deferred compilation.  */)
  (Lisp_Object filename, Lisp_Object late_load)
{
  CHECK_STRING (filename);
  if (NILP (Ffile_exists_p (filename)))
    xsignal2 (Qnative_lisp_load_failed, build_string ("file does not exists"),
	      filename);
  struct Lisp_Native_Comp_Unit *comp_u = allocate_native_comp_unit ();
  Lisp_Object encoded_filename = ENCODE_FILE (filename);

  if (!NILP (Fgethash (filename, Vcomp_loaded_comp_units_h, Qnil))
      && !file_in_eln_sys_dir (filename)
      && !NILP (Ffile_writable_p (filename)))
    {
      /* If in this session there was ever a file loaded with this
	 name, rename it before loading, to make sure we always get a
	 new handle!  */
      Lisp_Object tmp_filename =
	Fmake_temp_file_internal (filename, Qnil, build_string (".eln.tmp"),
				  Qnil);
      if (NILP (Ffile_writable_p (tmp_filename)))
	comp_u->handle = dynlib_open_for_eln (SSDATA (encoded_filename));
      else
	{
	  Frename_file (filename, tmp_filename, Qt);
	  comp_u->handle = dynlib_open_for_eln (SSDATA (ENCODE_FILE (tmp_filename)));
	  Frename_file (tmp_filename, filename, Qnil);
	}
    }
  else
    comp_u->handle = dynlib_open_for_eln (SSDATA (encoded_filename));

  if (!comp_u->handle)
    xsignal2 (Qnative_lisp_load_failed, filename,
	      build_string (dynlib_error ()));
  comp_u->file = filename;

  comp_u->have_static_lisp_data =
#if USE_COMP_STATIC_LISP_OBJECTS
    dynlib_sym (comp_u->handle, HAVE_STATIC_LISP_DATA_SYM) != NULL;
#else
  false;
#endif

#if USE_COMP_STATIC_LISP_OBJECTS
  comp_u->staticpro = Qnil;
  comp_u->ephemeral = Qnil;
#endif

  if (!comp_u->have_static_lisp_data)
    {
      comp_u->data_vec = Qnil;
      comp_u->lambda_gc_guard_h
	= CALLN (Fmake_hash_table, QCtest, Qeq);
      comp_u->lambda_c_name_idx_h
	= CALLN (Fmake_hash_table, QCtest, Qequal);
    }
  return load_comp_unit (comp_u, false, !NILP (late_load));
}

#endif /* HAVE_NATIVE_COMP */

DEFUN ("native-comp-available-p", Fnative_comp_available_p,
       Snative_comp_available_p, 0, 0, 0,
       doc: /* Return non-nil if native compilation support is built-in.  */)
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
  DEFVAR_LISP ("comp--delayed-sources", Vcomp__delayed_sources,
    doc: /* List of sources to be native-compiled when startup is finished.
For internal use.  */);
  DEFVAR_BOOL ("comp--compilable", comp__compilable,
    doc: /* Non-nil when comp.el can be native compiled.
For internal use. */);
  /* Compiler control customizes.  */
  DEFVAR_BOOL ("native-comp-jit-compilation", native_comp_jit_compilation,
    doc: /* If non-nil, compile loaded .elc files asynchronously.

After compilation, each function definition is updated to use the
natively-compiled one.  */);
  native_comp_jit_compilation = true;

  DEFSYM (Qnative_comp_speed, "native-comp-speed");
  DEFSYM (Qnative_comp_debug, "native-comp-debug");
  DEFSYM (Qnative_comp_driver_options, "native-comp-driver-options");
  DEFSYM (Qnative_comp_compiler_options, "native-comp-compiler-options");
  DEFSYM (Qcomp_libgccjit_reproducer, "comp-libgccjit-reproducer");

  /* Limple instruction set.  */
  DEFSYM (Qcomment, "comment");
  DEFSYM (Qjump, "jump");
  DEFSYM (Qcall, "call");
  DEFSYM (Qcallref, "callref");
  DEFSYM (Qdirect_call, "direct-call");
  DEFSYM (Qdirect_callref, "direct-callref");
  DEFSYM (Qassume, "assume");
  DEFSYM (Qsetimm, "setimm");
  DEFSYM (Qreturn, "return");
  DEFSYM (Qunreachable, "unreachable");
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
  DEFSYM (Qsymbol_with_pos_p, "symbol-with-pos-p");

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
  DEFSYM (Qcomp_subr_trampoline_install, "comp-subr-trampoline-install");
  DEFSYM (Qnative_comp_warning_on_missing_source,
	  "native-comp-warning-on-missing-source");

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

#if USE_COMP_STATIC_LISP_OBJECTS
  DEFSYM (Qinit_expr_type_val, "init-expr-type-val");
  DEFSYM (Qinit_expr_type_self_repr, "init-expr-type-self-repr");
  DEFSYM (Qinit_expr_type_var, "init-expr-type-var");
#endif

  defsubr (&Scomp__subr_signature);
  defsubr (&Scomp_el_to_eln_rel_filename);
  defsubr (&Scomp_el_to_eln_filename);
  defsubr (&Scomp_native_driver_options_effective_p);
  defsubr (&Scomp_native_compiler_options_effective_p);
  defsubr (&Scomp__install_trampoline);
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
  staticpro (&loadsearch_re_list);
  loadsearch_re_list = Qnil;

#if USE_COMP_STATIC_LISP_OBJECTS
  staticpro (&comp.d_default_rvals);
  comp.d_default_rvals = Qnil;
  staticpro (&comp.d_impure_rvals);
  comp.d_impure_rvals = Qnil;
  staticpro (&comp.d_ephemeral_rvals);
  comp.d_ephemeral_rvals = Qnil;
  staticpro (&comp.static_hash_cons_h);
  comp.static_hash_cons_h = Qnil;
  staticpro (&comp.lisp_consts_init_lvals);
  comp.lisp_consts_init_lvals = Qnil;
  staticpro (&comp.lambda_init_lvals);
  comp.lambda_init_lvals = Qnil;
  staticpro (&comp.lisp_vector_structs_h);
  comp.lisp_vector_structs_h = Qnil;
  staticpro (&comp.cons_block_list);
  comp.cons_block_list = Qnil;
  staticpro (&comp.float_block_list);
  comp.float_block_list = Qnil;

  Fprovide (intern_c_string ("comp--static-lisp-consts"), Qnil);
#endif

  DEFVAR_LISP ("comp-ctxt", Vcomp_ctxt,
	       doc: /* The compiler context.  */);
  Vcomp_ctxt = Qnil;

  /* FIXME should be initialized but not here...  Plus this don't have
     to be necessarily exposed to lisp but can easy debug for now.  */
  DEFVAR_LISP ("comp-subr-list", Vcomp_subr_list,
    doc: /* List of all defined subrs.  */);
  DEFVAR_LISP ("comp-abi-hash", Vcomp_abi_hash,
    doc: /* String signing the .eln files ABI.  */);
  Vcomp_abi_hash = Qnil;
  DEFVAR_LISP ("comp-native-version-dir", Vcomp_native_version_dir,
    doc: /* Directory in use to disambiguate eln compatibility.  */);
  Vcomp_native_version_dir = Qnil;

  DEFVAR_LISP ("comp-deferred-pending-h", Vcomp_deferred_pending_h,
    doc: /* Hash table symbol-name -> function-value.
For internal use.  */);
  Vcomp_deferred_pending_h = CALLN (Fmake_hash_table, QCtest, Qeq);

  DEFVAR_LISP ("comp-eln-to-el-h", Vcomp_eln_to_el_h,
    doc: /* Hash table eln-filename -> el-filename.  */);
  Vcomp_eln_to_el_h = CALLN (Fmake_hash_table, QCtest, Qequal);

  DEFVAR_LISP ("native-comp-eln-load-path", Vnative_comp_eln_load_path,
    doc: /* List of directories to look for natively-compiled *.eln files.

The *.eln files are actually looked for in a version-specific
subdirectory of each directory in this list.  That subdirectory
is determined by the value of `comp-native-version-dir'.
If the name of a directory in this list is not absolute, it is
assumed to be relative to `invocation-directory'.
The last directory of this list is assumed to be the one holding
the system *.eln files, which are the files produced when building
Emacs.  */);

  /* Temporary value in use for bootstrap.  We can't do better as
     `invocation-directory' is still unset, will be fixed up during
     dump reload.  */
  Vnative_comp_eln_load_path = Fcons (build_string ("../native-lisp/"), Qnil);

  DEFVAR_LISP ("native-comp-enable-subr-trampolines",
	       Vnative_comp_enable_subr_trampolines,
    doc: /* If non-nil, enable generation of trampolines for calling primitives.
Trampolines are needed so that Emacs respects redefinition or advice of
primitive functions when they are called from Lisp code natively-compiled
at `native-comp-speed' of 2.

By default, the value is t, and when Emacs sees a redefined or advised
primitive called from natively-compiled Lisp, it generates a trampoline
for it on-the-fly.

If the value is a file name (a string), it specifies the directory in
which to deposit the generated trampolines, overriding the directories
in `native-comp-eln-load-path'.

When this variable is nil, generation of trampolines is disabled.

Disabling the generation of trampolines, when a trampoline for a redefined
or advised primitive is not already available from previous compilations,
means that such redefinition or advice will not have effect when calling
primitives from natively-compiled Lisp code.  That is, calls to primitives
without existing trampolines from natively-compiled Lisp will behave as if
the primitive was called directly from C, and will ignore its redefinition
and advice.  */);

  DEFVAR_LISP ("comp-installed-trampolines-h", Vcomp_installed_trampolines_h,
    doc: /* Hash table subr-name -> installed trampoline.
This is used to prevent double trampoline instantiation, and also to
protect the trampolines against GC.  */);
  Vcomp_installed_trampolines_h = CALLN (Fmake_hash_table);

  DEFVAR_LISP ("comp-no-native-file-h", V_comp_no_native_file_h,
    doc: /* Files for which no deferred compilation should be performed.
These files' compilation should not be deferred because the bytecode
version was explicitly requested by the user during load.
For internal use.  */);
  V_comp_no_native_file_h = CALLN (Fmake_hash_table, QCtest, Qequal);

  DEFVAR_BOOL ("comp-file-preloaded-p", comp_file_preloaded_p,
    doc: /* When non-nil, assume the file being compiled to be preloaded.  */);

  DEFVAR_LISP ("comp-loaded-comp-units-h", Vcomp_loaded_comp_units_h,
    doc: /* Hash table recording all loaded compilation units, file -> CU.  */);
  Vcomp_loaded_comp_units_h =
    CALLN (Fmake_hash_table, QCweakness, Qvalue, QCtest, Qequal);

  DEFVAR_LISP ("comp-subr-arities-h", Vcomp_subr_arities_h,
    doc: /* Hash table recording the arity of Lisp primitives.
This is in case they are redefined so the compiler still knows how to
compile calls to them.
subr-name -> arity
For internal use.  */);
  Vcomp_subr_arities_h = CALLN (Fmake_hash_table, QCtest, Qequal);

  Fprovide (intern_c_string ("native-compile"), Qnil);
#endif /* #ifdef HAVE_NATIVE_COMP */

  defsubr (&Snative_comp_available_p);
}
