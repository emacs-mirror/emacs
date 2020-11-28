/* emacs-module.c - Module loading and runtime implementation

Copyright (C) 2015-2020 Free Software Foundation, Inc.

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

/*
The public module API is defined in the header emacs-module.h.  The
configure script generates emacs-module.h from emacs-module.h.in and
the version-specific environment fragments in module-env-*.h.

If you want to change the module API, please abide to the following
rules:

- Don't remove publicly documented declarations from the headers.

- Don't remove, reorder, or rename structure fields, as such changes
  break ABI compatibility.

- Don't change the types of structure fields.

- Likewise, the presence, order, and type of structure fields may not
  depend on preprocessor macros.

- Add structure fields only at the end of structures.

- For every Emacs major version there is a new fragment file
  module-env-VER.h.  Add functions solely at the end of the fragment
  file for the next (not yet released) major version of Emacs.  For
  example, if the current Emacs release is 26.2, add functions only to
  emacs-env-27.h.

- emacs-module.h should only depend on standard C headers.  In
  particular, don't include config.h or lisp.h from emacs-module.h.

- The contents of emacs-module.h should be the same on all platforms
  and architectures.

- emacs-module.h may not depend on Emacs configuration options.

- Prefix all names in emacs-module.h with "emacs_" or "EMACS_".

To add a new module function, proceed as follows:

1. Add a new function pointer field at the end of the emacs-env-*.h
   file for the next major version of Emacs.

2. Run config.status or configure to regenerate emacs-module.h.

3. Create a corresponding implementation function in this file.  See
   "Implementation of runtime and environment functions" below for
   further rules.

4. Assign the new field in the initialize_environment function.

5. Add a test function that calls your new function to
   test/data/emacs-module/mod-test.c.  Add a unit test that invokes
   your new test function to test/src/emacs-module-tests.el.

6. Document your new function in the manual and in etc/NEWS.
*/

#include <config.h>

#include "emacs-module.h"

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>

#include "lisp.h"
#include "bignum.h"
#include "dynlib.h"
#include "coding.h"
#include "keyboard.h"
#include "syssignal.h"
#include "sysstdio.h"
#include "thread.h"

#include <intprops.h>
#include <verify.h>

/* Work around GCC bug 83162.  */
#if GNUC_PREREQ (4, 3, 0)
# pragma GCC diagnostic ignored "-Wclobbered"
#endif

/* We use different strategies for allocating the user-visible objects
   (struct emacs_runtime, emacs_env, emacs_value), depending on
   whether the user supplied the -module-assertions flag.  If
   assertions are disabled, all objects are allocated from the stack.
   If assertions are enabled, all objects are allocated from the free
   store, and objects are never freed; this guarantees that they all
   have different addresses.  We use that for checking which objects
   are live.  Without unique addresses, we might consider some dead
   objects live because their addresses would have been reused in the
   meantime.  */


/* Feature tests.  */

#ifdef WINDOWSNT
#include <windows.h>
#include "w32term.h"
#endif

/* Function prototype for the module init function.  */
typedef int (*emacs_init_function) (struct emacs_runtime *);

/* Function prototype for module user-pointer finalizers.  These
   should not throw C++ exceptions, so emacs-module.h declares the
   corresponding interfaces with EMACS_NOEXCEPT.  There is only C code
   in this module, though, so this constraint is not enforced here.  */
typedef void (*emacs_finalizer_function) (void *);


/* Memory management.  */

/* An `emacs_value' is just a pointer to a structure holding an
   internal Lisp object.  */
struct emacs_value_tag { Lisp_Object v; };

/* Local value objects use a simple fixed-sized block allocation
   scheme without explicit deallocation.  All local values are
   deallocated when the lifetime of their environment ends.  Keep
   track of a current frame from which new values are allocated,
   appending further dynamically-allocated frames if necessary.  */

enum { value_frame_size = 512 };

/* A block from which `emacs_value' object can be allocated.  */
struct emacs_value_frame
{
  /* Storage for values.  */
  struct emacs_value_tag objects[value_frame_size];

  /* Index of the next free value in `objects'.  */
  int offset;

  /* Pointer to next frame, if any.  */
  struct emacs_value_frame *next;
};

/* A structure that holds an initial frame (so that the first local
   values require no dynamic allocation) and keeps track of the
   current frame.  */
struct emacs_value_storage
{
  struct emacs_value_frame initial;
  struct emacs_value_frame *current;
};


/* Private runtime and environment members.  */

/* The private part of an environment stores the current non local exit state
   and holds the `emacs_value' objects allocated during the lifetime
   of the environment.  */
struct emacs_env_private
{
  enum emacs_funcall_exit pending_non_local_exit;

  /* Dedicated storage for non-local exit symbol and data so that
     storage is always available for them, even in an out-of-memory
     situation.  */
  struct emacs_value_tag non_local_exit_symbol, non_local_exit_data;

  struct emacs_value_storage storage;
};

/* The private parts of an `emacs_runtime' object contain the initial
   environment.  */
struct emacs_runtime_private
{
  emacs_env *env;
};


/* Forward declarations.  */

static Lisp_Object value_to_lisp (emacs_value);
static emacs_value allocate_emacs_value (emacs_env *, struct emacs_value_storage *, Lisp_Object);
static emacs_value lisp_to_value (emacs_env *, Lisp_Object);
static enum emacs_funcall_exit module_non_local_exit_check (emacs_env *);
static void module_assert_thread (void);
static void module_assert_runtime (struct emacs_runtime *);
static void module_assert_env (emacs_env *);
static AVOID module_abort (const char *, ...) ATTRIBUTE_FORMAT_PRINTF (1, 2);
static emacs_env *initialize_environment (emacs_env *,
					  struct emacs_env_private *);
static void finalize_environment (emacs_env *);
static void finalize_environment_unwind (void *);
static void finalize_runtime_unwind (void *);
static void module_handle_nonlocal_exit (emacs_env *, enum nonlocal_exit,
                                         Lisp_Object);
static void module_non_local_exit_signal_1 (emacs_env *,
					    Lisp_Object, Lisp_Object);
static void module_non_local_exit_throw_1 (emacs_env *,
					   Lisp_Object, Lisp_Object);
static void module_out_of_memory (emacs_env *);
static void module_reset_handlerlist (struct handler **);
static bool value_storage_contains_p (const struct emacs_value_storage *,
                                      emacs_value, ptrdiff_t *);

static bool module_assertions = false;

/* Convenience macros for non-local exit handling.  */

/* FIXME: The following implementation for non-local exit handling
   does not support recovery from stack overflow, see sysdep.c.  */

/* Emacs uses setjmp and longjmp for non-local exits, but
   module frames cannot be skipped because they are in general
   not prepared for long jumps (e.g., the behavior in C++ is undefined
   if objects with nontrivial destructors would be skipped).
   Therefore, catch all non-local exits.  There are two kinds of
   non-local exits: `signal' and `throw'.  The macro in this section
   can be used to catch both.  Use a macro to avoid additional variants
   of `internal_condition_case' etc., and to avoid worrying about
   passing information to the handler functions.  */

#if !__has_attribute (cleanup)
 #error "__attribute__ ((cleanup)) not supported by this compiler; try GCC"
#endif

/* Place this macro at the beginning of a function returning a number
   or a pointer to handle non-local exits.  The function must have an
   ENV parameter.  The function will return the specified value if a
   signal or throw is caught.  */

/* It is very important that pushing the handler doesn't itself raise
   a signal.  Install the cleanup only after the handler has been
   pushed.  Use __attribute__ ((cleanup)) to avoid
   non-local-exit-prone manual cleanup.

   The do-while forces uses of the macro to be followed by a semicolon.
   This macro cannot enclose its entire body inside a do-while, as the
   code after the macro may longjmp back into the macro, which means
   its local variable INTERNAL_CLEANUP must stay live in later code.  */

/* TODO: Make backtraces work if this macro is used.  */

#define MODULE_HANDLE_NONLOCAL_EXIT(retval)                             \
  if (module_non_local_exit_check (env) != emacs_funcall_exit_return)	\
    return retval;							\
  struct handler *internal_handler =                                    \
    push_handler_nosignal (Qt, CATCHER_ALL);                            \
  if (!internal_handler)                                                \
    {									\
      module_out_of_memory (env);					\
      return retval;							\
    }									\
  struct handler *internal_cleanup                                      \
    __attribute__ ((cleanup (module_reset_handlerlist)))                \
    = internal_handler;                                                 \
  if (sys_setjmp (internal_cleanup->jmp))                               \
    {									\
      module_handle_nonlocal_exit (env,                                 \
                                   internal_cleanup->nonlocal_exit,     \
                                   internal_cleanup->val);              \
      return retval;							\
    }									\
  do { } while (false)


/* Implementation of runtime and environment functions.

   These should abide by the following rules:

   1. The first argument should always be a pointer to emacs_env.

   2. Each function should first call check_thread.  Note that
      this function is a no-op unless Emacs was built with
      --enable-checking.

   3. The very next thing each function should do is check that the
      emacs_env object does not have a non-local exit indication set,
      by calling module_non_local_exit_check.  If that returns
      anything but emacs_funcall_exit_return, the function should do
      nothing and return immediately with an error indication, without
      clobbering the existing error indication in emacs_env.  This is
      needed for correct reporting of Lisp errors to the Emacs Lisp
      interpreter.

   4. Any function that needs to call Emacs facilities, such as
      encoding or decoding functions, or 'intern', or 'make_string',
      should protect itself from signals and 'throw' in the called
      Emacs functions, by placing the macro
      MODULE_HANDLE_NONLOCAL_EXIT right after the above 2 tests.

   5. Do NOT use 'eassert' for checking validity of user code in the
      module.  Instead, make those checks part of the code, and if the
      check fails, call 'module_non_local_exit_signal_1' or
      'module_non_local_exit_throw_1' to report the error.  This is
      because using 'eassert' in these situations will abort Emacs
      instead of reporting the error back to Lisp, and also because
      'eassert' is compiled to nothing in the release version.  */

/* Use MODULE_FUNCTION_BEGIN_NO_CATCH to implement steps 2 and 3 for
   environment functions that are known to never exit non-locally.  On
   error it will return its argument, which can be a sentinel
   value.  */

#define MODULE_FUNCTION_BEGIN_NO_CATCH(error_retval)                    \
  do {                                                                  \
    module_assert_thread ();                                            \
    module_assert_env (env);                                            \
    if (module_non_local_exit_check (env) != emacs_funcall_exit_return) \
      return error_retval;                                              \
  } while (false)

/* Use MODULE_FUNCTION_BEGIN to implement steps 2 through 4 for most
   environment functions.  On error it will return its argument, which
   can be a sentinel value.  */

#define MODULE_FUNCTION_BEGIN(error_retval)      \
  MODULE_FUNCTION_BEGIN_NO_CATCH (error_retval); \
  MODULE_HANDLE_NONLOCAL_EXIT (error_retval)

static void
CHECK_USER_PTR (Lisp_Object obj)
{
  CHECK_TYPE (USER_PTRP (obj), Quser_ptrp, obj);
}

/* Catch signals and throws only if the code can actually signal or
   throw.  If checking is enabled, abort if the current thread is not
   the Emacs main thread.  */

static emacs_env *
module_get_environment (struct emacs_runtime *ert)
{
  module_assert_thread ();
  module_assert_runtime (ert);
  return ert->private_members->env;
}

/* To make global refs (GC-protected global values) keep a hash that
   maps global Lisp objects to 'struct module_global_reference'
   objects.  We store the 'emacs_value' in the hash table so that it
   is automatically garbage-collected (Bug#42482).  */

static Lisp_Object Vmodule_refs_hash;

/* Pseudovector type for global references.  The pseudovector tag is
   PVEC_OTHER since these values are never printed and don't need to
   be special-cased for garbage collection.  */

struct module_global_reference {
  /* Pseudovector header, must come first. */
  union vectorlike_header header;

  /* Holds the emacs_value for the object.  The Lisp_Object stored
     therein must be the same as the hash key.  */
  struct emacs_value_tag value;

  /* Reference count, always positive.  */
  ptrdiff_t refcount;
};

static struct module_global_reference *
XMODULE_GLOBAL_REFERENCE (Lisp_Object o)
{
  eassert (PSEUDOVECTORP (o, PVEC_OTHER));
  return XUNTAG (o, Lisp_Vectorlike, struct module_global_reference);
}

/* Returns whether V is a global reference.  Only used to check module
   assertions.  If V is not a global reference, increment *N by the
   number of global references (for debugging output).  */

static bool
module_global_reference_p (emacs_value v, ptrdiff_t *n)
{
  struct Lisp_Hash_Table *h = XHASH_TABLE (Vmodule_refs_hash);
  /* Note that we can't use `hash_lookup' because V might be a local
     reference that's identical to some global reference.  */
  for (ptrdiff_t i = 0; i < HASH_TABLE_SIZE (h); ++i)
    {
      if (!EQ (HASH_KEY (h, i), Qunbound)
          && &XMODULE_GLOBAL_REFERENCE (HASH_VALUE (h, i))->value == v)
        return true;
    }
  /* Only used for debugging, so we don't care about overflow, just
     make sure the operation is defined.  */
  INT_ADD_WRAPV (*n, h->count, n);
  return false;
}

static emacs_value
module_make_global_ref (emacs_env *env, emacs_value ref)
{
  MODULE_FUNCTION_BEGIN (NULL);
  struct Lisp_Hash_Table *h = XHASH_TABLE (Vmodule_refs_hash);
  Lisp_Object new_obj = value_to_lisp (ref), hashcode;
  ptrdiff_t i = hash_lookup (h, new_obj, &hashcode);

  /* Note: This approach requires the garbage collector to never move
     objects.  */

  if (i >= 0)
    {
      Lisp_Object value = HASH_VALUE (h, i);
      struct module_global_reference *ref = XMODULE_GLOBAL_REFERENCE (value);
      bool overflow = INT_ADD_WRAPV (ref->refcount, 1, &ref->refcount);
      if (overflow)
	overflow_error ();
      return &ref->value;
    }
  else
    {
      struct module_global_reference *ref
        = ALLOCATE_PLAIN_PSEUDOVECTOR (struct module_global_reference,
                                       PVEC_OTHER);
      ref->value.v = new_obj;
      ref->refcount = 1;
      Lisp_Object value;
      XSETPSEUDOVECTOR (value, ref, PVEC_OTHER);
      hash_put (h, new_obj, value, hashcode);
      return &ref->value;
    }
}

static void
module_free_global_ref (emacs_env *env, emacs_value ref)
{
  /* TODO: This probably never signals.  */
  /* FIXME: Wait a minute.  Shouldn't this function report an error if
     the hash lookup fails?  */
  MODULE_FUNCTION_BEGIN ();
  struct Lisp_Hash_Table *h = XHASH_TABLE (Vmodule_refs_hash);
  Lisp_Object obj = value_to_lisp (ref);
  ptrdiff_t i = hash_lookup (h, obj, NULL);

  if (module_assertions)
    {
      ptrdiff_t n = 0;
      if (! module_global_reference_p (ref, &n))
        module_abort ("Global value was not found in list of %"pD"d globals",
                      n);
    }

  if (i >= 0)
    {
      Lisp_Object value = HASH_VALUE (h, i);
      struct module_global_reference *ref = XMODULE_GLOBAL_REFERENCE (value);
      eassert (0 < ref->refcount);
      if (--ref->refcount == 0)
        hash_remove_from_table (h, obj);
    }
}

static enum emacs_funcall_exit
module_non_local_exit_check (emacs_env *env)
{
  module_assert_thread ();
  module_assert_env (env);
  return env->private_members->pending_non_local_exit;
}

static void
module_non_local_exit_clear (emacs_env *env)
{
  module_assert_thread ();
  module_assert_env (env);
  env->private_members->pending_non_local_exit = emacs_funcall_exit_return;
}

static enum emacs_funcall_exit
module_non_local_exit_get (emacs_env *env, emacs_value *sym, emacs_value *data)
{
  module_assert_thread ();
  module_assert_env (env);
  struct emacs_env_private *p = env->private_members;
  if (p->pending_non_local_exit != emacs_funcall_exit_return)
    {
      *sym = &p->non_local_exit_symbol;
      *data = &p->non_local_exit_data;
    }
  return p->pending_non_local_exit;
}

/* Like for `signal', DATA must be a list.  */
static void
module_non_local_exit_signal (emacs_env *env, emacs_value sym, emacs_value data)
{
  module_assert_thread ();
  module_assert_env (env);
  if (module_non_local_exit_check (env) == emacs_funcall_exit_return)
    module_non_local_exit_signal_1 (env, value_to_lisp (sym),
				    value_to_lisp (data));
}

static void
module_non_local_exit_throw (emacs_env *env, emacs_value tag, emacs_value value)
{
  module_assert_thread ();
  module_assert_env (env);
  if (module_non_local_exit_check (env) == emacs_funcall_exit_return)
    module_non_local_exit_throw_1 (env, value_to_lisp (tag),
				   value_to_lisp (value));
}

/* Function prototype for the module Lisp functions.  */
typedef emacs_value (*emacs_subr) (emacs_env *, ptrdiff_t,
				   emacs_value [], void *);

/* Module function.  */

/* A function environment is an auxiliary structure returned by
   `module_make_function' to store information about a module
   function.  It is stored in a pseudovector.  Its members correspond
   to the arguments given to `module_make_function'.  */

struct Lisp_Module_Function
{
  union vectorlike_header header;

  /* Fields traced by GC; these must come first.  */
  Lisp_Object documentation;

  /* Fields ignored by GC.  */
  ptrdiff_t min_arity, max_arity;
  emacs_subr subr;
  void *data;
} GCALIGNED_STRUCT;

static struct Lisp_Module_Function *
allocate_module_function (void)
{
  return ALLOCATE_PSEUDOVECTOR (struct Lisp_Module_Function,
                                documentation, PVEC_MODULE_FUNCTION);
}

#define XSET_MODULE_FUNCTION(var, ptr) \
  XSETPSEUDOVECTOR (var, ptr, PVEC_MODULE_FUNCTION)

/* A module function is a pseudovector of subtype
   PVEC_MODULE_FUNCTION; see lisp.h for the definition.  */

static emacs_value
module_make_function (emacs_env *env, ptrdiff_t min_arity, ptrdiff_t max_arity,
		      emacs_subr subr, const char *documentation,
		      void *data)
{
  MODULE_FUNCTION_BEGIN (NULL);

  if (! (0 <= min_arity
	 && (max_arity < 0
	     ? (min_arity <= MOST_POSITIVE_FIXNUM
		&& max_arity == emacs_variadic_function)
	     : min_arity <= max_arity && max_arity <= MOST_POSITIVE_FIXNUM)))
    xsignal2 (Qinvalid_arity, make_fixnum (min_arity), make_fixnum (max_arity));

  struct Lisp_Module_Function *function = allocate_module_function ();
  function->min_arity = min_arity;
  function->max_arity = max_arity;
  function->subr = subr;
  function->data = data;

  if (documentation)
    function->documentation = build_string_from_utf8 (documentation);

  Lisp_Object result;
  XSET_MODULE_FUNCTION (result, function);
  eassert (MODULE_FUNCTIONP (result));

  return lisp_to_value (env, result);
}

static emacs_value
module_funcall (emacs_env *env, emacs_value fun, ptrdiff_t nargs,
		emacs_value args[])
{
  MODULE_FUNCTION_BEGIN (NULL);

  /* Make a new Lisp_Object array starting with the function as the
     first arg, because that's what Ffuncall takes.  */
  Lisp_Object *newargs;
  USE_SAFE_ALLOCA;
  ptrdiff_t nargs1;
  if (INT_ADD_WRAPV (nargs, 1, &nargs1))
    overflow_error ();
  SAFE_ALLOCA_LISP (newargs, nargs1);
  newargs[0] = value_to_lisp (fun);
  for (ptrdiff_t i = 0; i < nargs; i++)
    newargs[1 + i] = value_to_lisp (args[i]);
  emacs_value result = lisp_to_value (env, Ffuncall (nargs1, newargs));
  SAFE_FREE ();
  return result;
}

static emacs_value
module_intern (emacs_env *env, const char *name)
{
  MODULE_FUNCTION_BEGIN (NULL);
  return lisp_to_value (env, intern (name));
}

static emacs_value
module_type_of (emacs_env *env, emacs_value value)
{
  MODULE_FUNCTION_BEGIN (NULL);
  return lisp_to_value (env, Ftype_of (value_to_lisp (value)));
}

static bool
module_is_not_nil (emacs_env *env, emacs_value value)
{
  MODULE_FUNCTION_BEGIN_NO_CATCH (false);
  return ! NILP (value_to_lisp (value));
}

static bool
module_eq (emacs_env *env, emacs_value a, emacs_value b)
{
  MODULE_FUNCTION_BEGIN_NO_CATCH (false);
  return EQ (value_to_lisp (a), value_to_lisp (b));
}

static intmax_t
module_extract_integer (emacs_env *env, emacs_value n)
{
  MODULE_FUNCTION_BEGIN (0);
  Lisp_Object l = value_to_lisp (n);
  CHECK_INTEGER (l);
  intmax_t i;
  if (! integer_to_intmax (l, &i))
    xsignal1 (Qoverflow_error, l);
  return i;
}

static emacs_value
module_make_integer (emacs_env *env, intmax_t n)
{
  MODULE_FUNCTION_BEGIN (NULL);
  return lisp_to_value (env, make_int (n));
}

static double
module_extract_float (emacs_env *env, emacs_value f)
{
  MODULE_FUNCTION_BEGIN (0);
  Lisp_Object lisp = value_to_lisp (f);
  CHECK_TYPE (FLOATP (lisp), Qfloatp, lisp);
  return XFLOAT_DATA (lisp);
}

static emacs_value
module_make_float (emacs_env *env, double d)
{
  MODULE_FUNCTION_BEGIN (NULL);
  return lisp_to_value (env, make_float (d));
}

static bool
module_copy_string_contents (emacs_env *env, emacs_value value, char *buffer,
			     ptrdiff_t *length)
{
  MODULE_FUNCTION_BEGIN (false);
  Lisp_Object lisp_str = value_to_lisp (value);
  CHECK_STRING (lisp_str);

  /* We can set NOCOPY to true here because we only use the byte
     sequence starting at SDATA and don't modify the original string
     before copying out the data.

     We set HANDLE-8-BIT and HANDLE-OVER-UNI to nil to signal an error
     if the argument is not a valid Unicode string.  While it isn't
     documented how copy_string_contents behaves in this case,
     signaling an error is the most defensive and obvious reaction. */
  Lisp_Object lisp_str_utf8
    = encode_string_utf_8 (lisp_str, Qnil, true, Qnil, Qnil);

  /* Since we set HANDLE-8-BIT and HANDLE-OVER-UNI to nil, the return
     value can be nil, and we have to check for that. */
  CHECK_TYPE (!NILP (lisp_str_utf8), Qunicode_string_p, lisp_str);

  ptrdiff_t raw_size = SBYTES (lisp_str_utf8);
  ptrdiff_t required_buf_size = raw_size + 1;

  if (buffer == NULL)
    {
      *length = required_buf_size;
      return true;
    }

  if (*length < required_buf_size)
    {
      ptrdiff_t actual = *length;
      *length = required_buf_size;
      args_out_of_range_3 (INT_TO_INTEGER (actual),
                           INT_TO_INTEGER (required_buf_size),
                           INT_TO_INTEGER (PTRDIFF_MAX));
    }

  *length = required_buf_size;
  memcpy (buffer, SDATA (lisp_str_utf8), raw_size + 1);

  return true;
}

static emacs_value
module_make_string (emacs_env *env, const char *str, ptrdiff_t length)
{
  MODULE_FUNCTION_BEGIN (NULL);
  if (! (0 <= length && length <= STRING_BYTES_BOUND))
    overflow_error ();
  Lisp_Object lstr = make_string_from_utf8 (str, length);
  return lisp_to_value (env, lstr);
}

static emacs_value
module_make_user_ptr (emacs_env *env, emacs_finalizer_function fin, void *ptr)
{
  MODULE_FUNCTION_BEGIN (NULL);
  return lisp_to_value (env, make_user_ptr (fin, ptr));
}

static void *
module_get_user_ptr (emacs_env *env, emacs_value uptr)
{
  MODULE_FUNCTION_BEGIN (NULL);
  Lisp_Object lisp = value_to_lisp (uptr);
  CHECK_USER_PTR (lisp);
  return XUSER_PTR (lisp)->p;
}

static void
module_set_user_ptr (emacs_env *env, emacs_value uptr, void *ptr)
{
  MODULE_FUNCTION_BEGIN ();
  Lisp_Object lisp = value_to_lisp (uptr);
  CHECK_USER_PTR (lisp);
  XUSER_PTR (lisp)->p = ptr;
}

static emacs_finalizer_function
module_get_user_finalizer (emacs_env *env, emacs_value uptr)
{
  MODULE_FUNCTION_BEGIN (NULL);
  Lisp_Object lisp = value_to_lisp (uptr);
  CHECK_USER_PTR (lisp);
  return XUSER_PTR (lisp)->finalizer;
}

static void
module_set_user_finalizer (emacs_env *env, emacs_value uptr,
			   emacs_finalizer_function fin)
{
  MODULE_FUNCTION_BEGIN ();
  Lisp_Object lisp = value_to_lisp (uptr);
  CHECK_USER_PTR (lisp);
  XUSER_PTR (lisp)->finalizer = fin;
}

static void
check_vec_index (Lisp_Object lvec, ptrdiff_t i)
{
  CHECK_VECTOR (lvec);
  if (! (0 <= i && i < ASIZE (lvec)))
    args_out_of_range_3 (INT_TO_INTEGER (i),
			 make_fixnum (0), make_fixnum (ASIZE (lvec) - 1));
}

static void
module_vec_set (emacs_env *env, emacs_value vec, ptrdiff_t i, emacs_value val)
{
  MODULE_FUNCTION_BEGIN ();
  Lisp_Object lvec = value_to_lisp (vec);
  check_vec_index (lvec, i);
  ASET (lvec, i, value_to_lisp (val));
}

static emacs_value
module_vec_get (emacs_env *env, emacs_value vec, ptrdiff_t i)
{
  MODULE_FUNCTION_BEGIN (NULL);
  Lisp_Object lvec = value_to_lisp (vec);
  check_vec_index (lvec, i);
  return lisp_to_value (env, AREF (lvec, i));
}

static ptrdiff_t
module_vec_size (emacs_env *env, emacs_value vec)
{
  MODULE_FUNCTION_BEGIN (0);
  Lisp_Object lvec = value_to_lisp (vec);
  CHECK_VECTOR (lvec);
  return ASIZE (lvec);
}

/* This function should return true if and only if maybe_quit would
   quit.  */
static bool
module_should_quit (emacs_env *env)
{
  MODULE_FUNCTION_BEGIN_NO_CATCH (false);
  return QUITP;
}

static enum emacs_process_input_result
module_process_input (emacs_env *env)
{
  MODULE_FUNCTION_BEGIN (emacs_process_input_quit);
  maybe_quit ();
  return emacs_process_input_continue;
}

static struct timespec
module_extract_time (emacs_env *env, emacs_value value)
{
  MODULE_FUNCTION_BEGIN ((struct timespec) {0});
  return lisp_time_argument (value_to_lisp (value));
}

static emacs_value
module_make_time (emacs_env *env, struct timespec time)
{
  MODULE_FUNCTION_BEGIN (NULL);
  return lisp_to_value (env, timespec_to_lisp (time));
}

/*
Big integer support.

There are two possible ways to support big integers in the module API
that have been discussed:

1. Exposing GMP numbers (mpz_t) directly in the API.

2. Isolating the API from GMP by converting to/from a custom
   sign-magnitude representation.

Approach (1) has the advantage of being faster (no import/export
required) and requiring less code in Emacs and in modules that would
use GMP anyway.  However, (1) also couples big integer support
directly to the current implementation in Emacs (GMP).  Also (1)
requires each module author to ensure that their module is linked to
the same GMP library as Emacs itself; in particular, module authors
can't link GMP statically.  (1) also requires conditional compilation
and workarounds to ensure the module interface still works if GMP
isn't available while including emacs-module.h.  It also means that
modules written in languages such as Go and Java that support big
integers without GMP now have to carry an otherwise unnecessary GMP
dependency.  Approach (2), on the other hand, neatly decouples the
module interface from the GMP-based implementation.  It's not
significantly more complex than (1) either: the additional code is
mostly straightforward.  Over all, the benefits of (2) over (1) are
large enough to prefer it here.

We use a simple sign-magnitude representation for the big integers.
For the magnitude we pick an array of an unsigned integer type similar
to mp_limb_t instead of e.g. unsigned char.  This matches in most
cases the representation of a GMP limb.  In such cases GMP picks an
optimized algorithm for mpz_import and mpz_export that boils down to a
single memcpy to convert the magnitude.  This way we largely avoid the
import/export overhead on most platforms.
*/

enum
{
  /* Documented maximum count of magnitude elements. */
  module_bignum_count_max = min (SIZE_MAX, PTRDIFF_MAX) / sizeof (emacs_limb_t)
};

/* Verify that emacs_limb_t indeed has unique object
   representations.  */
verify (CHAR_BIT == 8);
verify ((sizeof (emacs_limb_t) == 4 && EMACS_LIMB_MAX == 0xFFFFFFFF)
        || (sizeof (emacs_limb_t) == 8
            && EMACS_LIMB_MAX == 0xFFFFFFFFFFFFFFFF));

static bool
module_extract_big_integer (emacs_env *env, emacs_value arg, int *sign,
                            ptrdiff_t *count, emacs_limb_t *magnitude)
{
  MODULE_FUNCTION_BEGIN (false);
  Lisp_Object o = value_to_lisp (arg);
  CHECK_INTEGER (o);
  int dummy;
  if (sign == NULL)
    sign = &dummy;
  /* See
     https://gmplib.org/manual/Integer-Import-and-Export.html#index-Export. */
  enum
  {
    order = -1,
    size = sizeof *magnitude,
    bits = size * CHAR_BIT,
    endian = 0,
    nails = 0,
    numb = 8 * size - nails
  };
  if (FIXNUMP (o))
    {
      EMACS_INT x = XFIXNUM (o);
      *sign = (0 < x) - (x < 0);
      if (x == 0 || count == NULL)
        return true;
      /* As a simplification we don't check how many array elements
         are exactly required, but use a reasonable static upper
         bound.  For most architectures exactly one element should
         suffice.  */
      EMACS_UINT u;
      enum { required = (sizeof u + size - 1) / size };
      verify (0 < required && +required <= module_bignum_count_max);
      if (magnitude == NULL)
        {
          *count = required;
          return true;
        }
      if (*count < required)
        {
          ptrdiff_t actual = *count;
          *count = required;
          args_out_of_range_3 (INT_TO_INTEGER (actual),
                               INT_TO_INTEGER (required),
                               INT_TO_INTEGER (module_bignum_count_max));
        }
      /* Set u = abs(x).  See https://stackoverflow.com/a/17313717. */
      if (0 < x)
        u = (EMACS_UINT) x;
      else
        u = -(EMACS_UINT) x;
      verify (required * bits < PTRDIFF_MAX);
      for (ptrdiff_t i = 0; i < required; ++i)
        magnitude[i] = (emacs_limb_t) (u >> (i * bits));
      return true;
    }
  const mpz_t *x = xbignum_val (o);
  *sign = mpz_sgn (*x);
  if (count == NULL)
    return true;
  size_t required_size = (mpz_sizeinbase (*x, 2) + numb - 1) / numb;
  eassert (required_size <= PTRDIFF_MAX);
  ptrdiff_t required = (ptrdiff_t) required_size;
  eassert (required <= module_bignum_count_max);
  if (magnitude == NULL)
    {
      *count = required;
      return true;
    }
  if (*count < required)
    {
      ptrdiff_t actual = *count;
      *count = required;
      args_out_of_range_3 (INT_TO_INTEGER (actual), INT_TO_INTEGER (required),
                           INT_TO_INTEGER (module_bignum_count_max));
    }
  size_t written;
  mpz_export (magnitude, &written, order, size, endian, nails, *x);
  eassert (written == required_size);
  return true;
}

static emacs_value
module_make_big_integer (emacs_env *env, int sign,
                         ptrdiff_t count, const emacs_limb_t *magnitude)
{
  MODULE_FUNCTION_BEGIN (NULL);
  if (sign == 0)
    return lisp_to_value (env, make_fixed_natnum (0));
  enum { order = -1, size = sizeof *magnitude, endian = 0, nails = 0 };
  mpz_import (mpz[0], count, order, size, endian, nails, magnitude);
  if (sign < 0)
    mpz_neg (mpz[0], mpz[0]);
  return lisp_to_value (env, make_integer_mpz ());
}


/* Subroutines.  */

static void
module_signal_or_throw (struct emacs_env_private *env)
{
  switch (env->pending_non_local_exit)
    {
    case emacs_funcall_exit_return:
      return;
    case emacs_funcall_exit_signal:
      xsignal (value_to_lisp (&env->non_local_exit_symbol),
               value_to_lisp (&env->non_local_exit_data));
    case emacs_funcall_exit_throw:
      Fthrow (value_to_lisp (&env->non_local_exit_symbol),
              value_to_lisp (&env->non_local_exit_data));
    default:
      eassume (false);
    }
}

/* Live runtime and environment objects, for assertions.  */
static Lisp_Object Vmodule_runtimes;
static Lisp_Object Vmodule_environments;

DEFUN ("module-load", Fmodule_load, Smodule_load, 1, 1, 0,
       doc: /* Load module FILE.  */)
  (Lisp_Object file)
{
  dynlib_handle_ptr handle;
  emacs_init_function module_init;
  void *gpl_sym;

  CHECK_STRING (file);
  handle = dynlib_open (SSDATA (file));
  if (!handle)
    xsignal2 (Qmodule_open_failed, file, build_string (dynlib_error ()));

  gpl_sym = dynlib_sym (handle, "plugin_is_GPL_compatible");
  if (!gpl_sym)
    xsignal1 (Qmodule_not_gpl_compatible, file);

  module_init = (emacs_init_function) dynlib_func (handle, "emacs_module_init");
  if (!module_init)
    xsignal1 (Qmissing_module_init_function, file);

  struct emacs_runtime rt_pub;
  struct emacs_runtime_private rt_priv;
  emacs_env env_pub;
  struct emacs_env_private env_priv;
  rt_priv.env = initialize_environment (&env_pub, &env_priv);

  /* If we should use module assertions, reallocate the runtime object
     from the free store, but never free it.  That way the addresses
     for two different runtime objects are guaranteed to be distinct,
     which we can use for checking the liveness of runtime
     pointers.  */
  struct emacs_runtime *rt = module_assertions ? xmalloc (sizeof *rt) : &rt_pub;
  rt->size = sizeof *rt;
  rt->private_members = &rt_priv;
  rt->get_environment = module_get_environment;

  Vmodule_runtimes = Fcons (make_mint_ptr (rt), Vmodule_runtimes);
  ptrdiff_t count = SPECPDL_INDEX ();
  record_unwind_protect_ptr (finalize_runtime_unwind, rt);

  int r = module_init (rt);

  /* Process the quit flag first, so that quitting doesn't get
     overridden by other non-local exits.  */
  maybe_quit ();

  if (r != 0)
    xsignal2 (Qmodule_init_failed, file, INT_TO_INTEGER (r));

  module_signal_or_throw (&env_priv);
  return unbind_to (count, Qt);
}

Lisp_Object
funcall_module (Lisp_Object function, ptrdiff_t nargs, Lisp_Object *arglist)
{
  const struct Lisp_Module_Function *func = XMODULE_FUNCTION (function);
  eassume (0 <= func->min_arity);
  if (! (func->min_arity <= nargs
	 && (func->max_arity < 0 || nargs <= func->max_arity)))
    xsignal2 (Qwrong_number_of_arguments, function, make_fixnum (nargs));

  emacs_env pub;
  struct emacs_env_private priv;
  emacs_env *env = initialize_environment (&pub, &priv);
  ptrdiff_t count = SPECPDL_INDEX ();
  record_unwind_protect_ptr (finalize_environment_unwind, env);

  USE_SAFE_ALLOCA;
  emacs_value *args = nargs > 0 ? SAFE_ALLOCA (nargs * sizeof *args) : NULL;
  for (ptrdiff_t i = 0; i < nargs; ++i)
    {
      args[i] = lisp_to_value (env, arglist[i]);
      if (! args[i])
	memory_full (sizeof *args[i]);
    }

  /* The only possibility of getting an error until here is failure to
     allocate memory for the arguments, but then we already should
     have signaled an error before.  */
  eassert (priv.pending_non_local_exit == emacs_funcall_exit_return);

  emacs_value ret = func->subr (env, nargs, args, func->data);

  eassert (&priv == env->private_members);

  /* Process the quit flag first, so that quitting doesn't get
     overridden by other non-local exits.  */
  maybe_quit ();

  module_signal_or_throw (&priv);
  return SAFE_FREE_UNBIND_TO (count, value_to_lisp (ret));
}

Lisp_Object
module_function_arity (const struct Lisp_Module_Function *const function)
{
  ptrdiff_t minargs = function->min_arity;
  ptrdiff_t maxargs = function->max_arity;
  return Fcons (make_fixnum (minargs),
		maxargs == MANY ? Qmany : make_fixnum (maxargs));
}

Lisp_Object
module_function_documentation (const struct Lisp_Module_Function *function)
{
  return function->documentation;
}

module_funcptr
module_function_address (const struct Lisp_Module_Function *function)
{
  return (module_funcptr) function->subr;
}


/* Helper functions.  */

static void
module_assert_thread (void)
{
  if (!module_assertions)
    return;
  if (!in_current_thread ())
    module_abort ("Module function called from outside "
                  "the current Lisp thread");
  if (gc_in_progress)
    module_abort ("Module function called during garbage collection");
}

static void
module_assert_runtime (struct emacs_runtime *ert)
{
  if (! module_assertions)
    return;
  ptrdiff_t count = 0;
  for (Lisp_Object tail = Vmodule_runtimes; CONSP (tail); tail = XCDR (tail))
    {
      if (xmint_pointer (XCAR (tail)) == ert)
        return;
      ++count;
    }
  module_abort ("Runtime pointer not found in list of %"pD"d runtimes",
		count);
}

static void
module_assert_env (emacs_env *env)
{
  if (! module_assertions)
    return;
  ptrdiff_t count = 0;
  for (Lisp_Object tail = Vmodule_environments; CONSP (tail);
       tail = XCDR (tail))
    {
      if (xmint_pointer (XCAR (tail)) == env)
        return;
      ++count;
    }
  module_abort ("Environment pointer not found in list of %"pD"d environments",
                count);
}

static void
module_non_local_exit_signal_1 (emacs_env *env, Lisp_Object sym,
				Lisp_Object data)
{
  struct emacs_env_private *p = env->private_members;
  if (p->pending_non_local_exit == emacs_funcall_exit_return)
    {
      p->pending_non_local_exit = emacs_funcall_exit_signal;
      p->non_local_exit_symbol.v = sym;
      p->non_local_exit_data.v = data;
    }
}

static void
module_non_local_exit_throw_1 (emacs_env *env, Lisp_Object tag,
			       Lisp_Object value)
{
  struct emacs_env_private *p = env->private_members;
  if (p->pending_non_local_exit == emacs_funcall_exit_return)
    {
      p->pending_non_local_exit = emacs_funcall_exit_throw;
      p->non_local_exit_symbol.v = tag;
      p->non_local_exit_data.v = value;
    }
}

/* Signal an out-of-memory condition to the caller.  */
static void
module_out_of_memory (emacs_env *env)
{
  /* TODO: Reimplement this so it works even if memory-signal-data has
     been modified.  */
  module_non_local_exit_signal_1 (env, XCAR (Vmemory_signal_data),
				  XCDR (Vmemory_signal_data));
}


/* Value conversion.  */

/* Convert an `emacs_value' to the corresponding internal object.
   Never fails.  */

/* If V was computed from lisp_to_value (O), then return O.
   Exits non-locally only if the stack overflows.  */
static Lisp_Object
value_to_lisp (emacs_value v)
{
  if (module_assertions)
    {
      /* Check the liveness of the value by iterating over all live
         environments.  */
      ptrdiff_t num_environments = 0;
      ptrdiff_t num_values = 0;
      for (Lisp_Object environments = Vmodule_environments;
           CONSP (environments); environments = XCDR (environments))
        {
          emacs_env *env = xmint_pointer (XCAR (environments));
          struct emacs_env_private *priv = env->private_members;
          /* The value might be one of the nonlocal exit values.  Note
             that we don't check whether a nonlocal exit is currently
             pending, because the module might have cleared the flag
             in the meantime.  */
          if (&priv->non_local_exit_symbol == v
              || &priv->non_local_exit_data == v)
            goto ok;
          if (value_storage_contains_p (&priv->storage, v, &num_values))
            goto ok;
          ++num_environments;
        }
      /* Also check global values.  */
      if (module_global_reference_p (v, &num_values))
        goto ok;
      module_abort (("Emacs value not found in %"pD"d values "
		     "of %"pD"d environments"),
                    num_values, num_environments);
    }

 ok: return v->v;
}

/* Convert an internal object to an `emacs_value'.  Allocate storage
   from the environment; return NULL if allocation fails.  */
static emacs_value
lisp_to_value (emacs_env *env, Lisp_Object o)
{
  struct emacs_env_private *p = env->private_members;
  if (p->pending_non_local_exit != emacs_funcall_exit_return)
    return NULL;
  return allocate_emacs_value (env, &p->storage, o);
}

/* Must be called for each frame before it can be used for allocation.  */
static void
initialize_frame (struct emacs_value_frame *frame)
{
  frame->offset = 0;
  frame->next = NULL;
}

/* Must be called for any storage object before it can be used for
   allocation.  */
static void
initialize_storage (struct emacs_value_storage *storage)
{
  initialize_frame (&storage->initial);
  storage->current = &storage->initial;
}

/* Must be called for any initialized storage object before its
   lifetime ends.  Free all dynamically-allocated frames.  */
static void
finalize_storage (struct emacs_value_storage *storage)
{
  struct emacs_value_frame *next = storage->initial.next;
  while (next != NULL)
    {
      struct emacs_value_frame *current = next;
      next = current->next;
      free (current);
    }
}

/* Allocate a new value from STORAGE and stores OBJ in it.  Return
   NULL if allocation fails and use ENV for non local exit reporting.  */
static emacs_value
allocate_emacs_value (emacs_env *env, struct emacs_value_storage *storage,
		      Lisp_Object obj)
{
  eassert (storage->current);
  eassert (storage->current->offset < value_frame_size);
  eassert (! storage->current->next);
  if (storage->current->offset == value_frame_size - 1)
    {
      storage->current->next = malloc (sizeof *storage->current->next);
      if (! storage->current->next)
        {
          module_out_of_memory (env);
          return NULL;
        }
      initialize_frame (storage->current->next);
      storage->current = storage->current->next;
    }
  emacs_value value = storage->current->objects + storage->current->offset;
  value->v = obj;
  ++storage->current->offset;
  return value;
}

/* Mark all objects allocated from local environments so that they
   don't get garbage-collected.  */
void
mark_modules (void)
{
  for (Lisp_Object tem = Vmodule_environments; CONSP (tem); tem = XCDR (tem))
    {
      emacs_env *env = xmint_pointer (XCAR (tem));
      struct emacs_env_private *priv = env->private_members;
      for (struct emacs_value_frame *frame = &priv->storage.initial;
	   frame != NULL;
	   frame = frame->next)
        for (int i = 0; i < frame->offset; ++i)
          mark_object (frame->objects[i].v);
    }
}


/* Environment lifetime management.  */

/* Must be called before the environment can be used.  Returns another
   pointer that callers should use instead of the ENV argument.  If
   module assertions are disabled, the return value is ENV.  If module
   assertions are enabled, the return value points to a heap-allocated
   object.  That object is never freed to guarantee unique
   addresses.  */
static emacs_env *
initialize_environment (emacs_env *env, struct emacs_env_private *priv)
{
  if (module_assertions)
      env = xmalloc (sizeof *env);

  priv->pending_non_local_exit = emacs_funcall_exit_return;
  initialize_storage (&priv->storage);
  env->size = sizeof *env;
  env->private_members = priv;
  env->make_global_ref = module_make_global_ref;
  env->free_global_ref = module_free_global_ref;
  env->non_local_exit_check = module_non_local_exit_check;
  env->non_local_exit_clear = module_non_local_exit_clear;
  env->non_local_exit_get = module_non_local_exit_get;
  env->non_local_exit_signal = module_non_local_exit_signal;
  env->non_local_exit_throw = module_non_local_exit_throw;
  env->make_function = module_make_function;
  env->funcall = module_funcall;
  env->intern = module_intern;
  env->type_of = module_type_of;
  env->is_not_nil = module_is_not_nil;
  env->eq = module_eq;
  env->extract_integer = module_extract_integer;
  env->make_integer = module_make_integer;
  env->extract_float = module_extract_float;
  env->make_float = module_make_float;
  env->copy_string_contents = module_copy_string_contents;
  env->make_string = module_make_string;
  env->make_user_ptr = module_make_user_ptr;
  env->get_user_ptr = module_get_user_ptr;
  env->set_user_ptr = module_set_user_ptr;
  env->get_user_finalizer = module_get_user_finalizer;
  env->set_user_finalizer = module_set_user_finalizer;
  env->vec_set = module_vec_set;
  env->vec_get = module_vec_get;
  env->vec_size = module_vec_size;
  env->should_quit = module_should_quit;
  env->process_input = module_process_input;
  env->extract_time = module_extract_time;
  env->make_time = module_make_time;
  env->extract_big_integer = module_extract_big_integer;
  env->make_big_integer = module_make_big_integer;
  Vmodule_environments = Fcons (make_mint_ptr (env), Vmodule_environments);
  return env;
}

/* Must be called before the lifetime of the environment object
   ends.  */
static void
finalize_environment (emacs_env *env)
{
  finalize_storage (&env->private_members->storage);
  eassert (xmint_pointer (XCAR (Vmodule_environments)) == env);
  Vmodule_environments = XCDR (Vmodule_environments);
}

static void
finalize_environment_unwind (void *env)
{
  finalize_environment (env);
}

static void
finalize_runtime_unwind (void *raw_ert)
{
  struct emacs_runtime *ert = raw_ert;
  eassert (xmint_pointer (XCAR (Vmodule_runtimes)) == ert);
  Vmodule_runtimes = XCDR (Vmodule_runtimes);
  finalize_environment (ert->private_members->env);
}


/* Non-local exit handling.  */

/* Must be called after setting up a handler immediately before
   returning from the function.  See the comments in lisp.h and the
   code in eval.c for details.  The macros below arrange for this
   function to be called automatically.  PHANDLERLIST points to a word
   containing the handler list, for sanity checking.  */
static void
module_reset_handlerlist (struct handler **phandlerlist)
{
  eassert (handlerlist == *phandlerlist);
  handlerlist = handlerlist->next;
}

/* Called on `signal' and `throw'.  DATA is a pair
   (ERROR-SYMBOL . ERROR-DATA) or (TAG . VALUE), which gets stored in
   the environment.  Set the pending non-local exit flag.  */
static void
module_handle_nonlocal_exit (emacs_env *env, enum nonlocal_exit type,
                             Lisp_Object data)
{
  switch (type)
    {
    case NONLOCAL_EXIT_SIGNAL:
      module_non_local_exit_signal_1 (env, XCAR (data), XCDR (data));
      break;
    case NONLOCAL_EXIT_THROW:
      module_non_local_exit_throw_1 (env, XCAR (data), XCDR (data));
      break;
    }
}


/* Support for assertions.  */
void
init_module_assertions (bool enable)
{
  module_assertions = enable;
}

/* Return whether STORAGE contains VALUE.  Used to check module
   assertions.  Increment *COUNT by the number of values searched.  */

static bool
value_storage_contains_p (const struct emacs_value_storage *storage,
                          emacs_value value, ptrdiff_t *count)
{
  for (const struct emacs_value_frame *frame = &storage->initial; frame != NULL;
       frame = frame->next)
    {
      for (int i = 0; i < frame->offset; ++i)
        {
          if (&frame->objects[i] == value)
            return true;
          ++*count;
        }
    }
  return false;
}

static AVOID ATTRIBUTE_FORMAT_PRINTF (1, 2)
module_abort (const char *format, ...)
{
  fputs ("Emacs module assertion: ", stderr);
  va_list args;
  va_start (args, format);
  vfprintf (stderr, format, args);
  va_end (args);
  putc ('\n', stderr);
  fflush (NULL);
  emacs_abort ();
}


/* Segment initializer.  */

void
syms_of_module (void)
{
  staticpro (&Vmodule_refs_hash);
  Vmodule_refs_hash
    = make_hash_table (hashtest_eq, DEFAULT_HASH_SIZE,
		       DEFAULT_REHASH_SIZE, DEFAULT_REHASH_THRESHOLD,
		       Qnil, false);

  staticpro (&Vmodule_runtimes);
  Vmodule_runtimes = Qnil;

  staticpro (&Vmodule_environments);
  Vmodule_environments = Qnil;

  DEFSYM (Qmodule_load_failed, "module-load-failed");
  Fput (Qmodule_load_failed, Qerror_conditions,
	pure_list (Qmodule_load_failed, Qerror));
  Fput (Qmodule_load_failed, Qerror_message,
        build_pure_c_string ("Module load failed"));

  DEFSYM (Qmodule_open_failed, "module-open-failed");
  Fput (Qmodule_open_failed, Qerror_conditions,
	pure_list (Qmodule_open_failed, Qmodule_load_failed, Qerror));
  Fput (Qmodule_open_failed, Qerror_message,
        build_pure_c_string ("Module could not be opened"));

  DEFSYM (Qmodule_not_gpl_compatible, "module-not-gpl-compatible");
  Fput (Qmodule_not_gpl_compatible, Qerror_conditions,
	pure_list (Qmodule_not_gpl_compatible, Qmodule_load_failed, Qerror));
  Fput (Qmodule_not_gpl_compatible, Qerror_message,
        build_pure_c_string ("Module is not GPL compatible"));

  DEFSYM (Qmissing_module_init_function, "missing-module-init-function");
  Fput (Qmissing_module_init_function, Qerror_conditions,
	pure_list (Qmissing_module_init_function, Qmodule_load_failed,
		   Qerror));
  Fput (Qmissing_module_init_function, Qerror_message,
        build_pure_c_string ("Module does not export an "
                             "initialization function"));

  DEFSYM (Qmodule_init_failed, "module-init-failed");
  Fput (Qmodule_init_failed, Qerror_conditions,
	pure_list (Qmodule_init_failed, Qmodule_load_failed, Qerror));
  Fput (Qmodule_init_failed, Qerror_message,
        build_pure_c_string ("Module initialization failed"));

  DEFSYM (Qinvalid_arity, "invalid-arity");
  Fput (Qinvalid_arity, Qerror_conditions, pure_list (Qinvalid_arity, Qerror));
  Fput (Qinvalid_arity, Qerror_message,
        build_pure_c_string ("Invalid function arity"));

  DEFSYM (Qmodule_function_p, "module-function-p");
  DEFSYM (Qunicode_string_p, "unicode-string-p");

  defsubr (&Smodule_load);
}
