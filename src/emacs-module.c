/* emacs-module.c - Module loading and runtime implementation

Copyright (C) 2015-2017 Free Software Foundation, Inc.

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

#include "emacs-module.h"

#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#include "lisp.h"
#include "dynlib.h"
#include "coding.h"
#include "keyboard.h"
#include "syssignal.h"
#include "thread.h"

#include <intprops.h>
#include <verify.h>

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

/* True if Lisp_Object and emacs_value have the same representation.
   This is typically true unless WIDE_EMACS_INT.  In practice, having
   the same sizes and alignments and maximums should be a good enough
   proxy for equality of representation.  */
enum
  {
    plain_values
      = (sizeof (Lisp_Object) == sizeof (emacs_value)
	 && alignof (Lisp_Object) == alignof (emacs_value)
	 && INTPTR_MAX == EMACS_INT_MAX)
  };

/* Function prototype for the module init function.  */
typedef int (*emacs_init_function) (struct emacs_runtime *);

/* Function prototype for module user-pointer finalizers.  These
   should not throw C++ exceptions, so emacs-module.h declares the
   corresponding interfaces with EMACS_NOEXCEPT.  There is only C code
   in this module, though, so this constraint is not enforced here.  */
typedef void (*emacs_finalizer_function) (void *);


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
  Lisp_Object non_local_exit_symbol, non_local_exit_data;

  /* List of values allocated from this environment.  The code uses
     this only if the user gave the -module-assertions command-line
     option.  */
  Lisp_Object values;
};

/* The private parts of an `emacs_runtime' object contain the initial
   environment.  */
struct emacs_runtime_private
{
  emacs_env *env;
};


/* Forward declarations.  */

static Lisp_Object value_to_lisp (emacs_value);
static emacs_value lisp_to_value (emacs_env *, Lisp_Object);
static enum emacs_funcall_exit module_non_local_exit_check (emacs_env *);
static void module_assert_thread (void);
static void module_assert_runtime (struct emacs_runtime *);
static void module_assert_env (emacs_env *);
static _Noreturn void module_abort (const char *format, ...)
  ATTRIBUTE_FORMAT_PRINTF(1, 2);
static emacs_env *initialize_environment (emacs_env *,
					  struct emacs_env_private *);
static void finalize_environment (emacs_env *);
static void finalize_environment_unwind (void *);
static void finalize_runtime_unwind (void *);
static void module_handle_signal (emacs_env *, Lisp_Object);
static void module_handle_throw (emacs_env *, Lisp_Object);
static void module_non_local_exit_signal_1 (emacs_env *,
					    Lisp_Object, Lisp_Object);
static void module_non_local_exit_throw_1 (emacs_env *,
					   Lisp_Object, Lisp_Object);
static void module_out_of_memory (emacs_env *);
static void module_reset_handlerlist (struct handler **);

/* We used to return NULL when emacs_value was a different type from
   Lisp_Object, but nowadays we just use Qnil instead.  Although they
   happen to be the same thing in the current implementation, module
   code should not assume this.  */
verify (NIL_IS_ZERO);
static emacs_value const module_nil = 0;

static bool module_assertions = false;
static emacs_env *global_env;
static struct emacs_env_private global_env_private;

/* Convenience macros for non-local exit handling.  */

/* FIXME: The following implementation for non-local exit handling
   does not support recovery from stack overflow, see sysdep.c.  */

/* Emacs uses setjmp and longjmp for non-local exits, but
   module frames cannot be skipped because they are in general
   not prepared for long jumps (e.g., the behavior in C++ is undefined
   if objects with nontrivial destructors would be skipped).
   Therefore, catch all non-local exits.  There are two kinds of
   non-local exits: `signal' and `throw'.  The macros in this section
   can be used to catch both.  Use macros to avoid additional variants
   of `internal_condition_case' etc., and to avoid worrying about
   passing information to the handler functions.  */

/* Place this macro at the beginning of a function returning a number
   or a pointer to handle non-local exits.  The function must have an
   ENV parameter.  The function will return the specified value if a
   signal or throw is caught.  */
/* TODO: Have Fsignal check for CATCHER_ALL so we only have to install
   one handler.  */
#define MODULE_HANDLE_NONLOCAL_EXIT(retval)                     \
  MODULE_SETJMP (CONDITION_CASE, module_handle_signal, retval); \
  MODULE_SETJMP (CATCHER_ALL, module_handle_throw, retval)

#define MODULE_SETJMP(handlertype, handlerfunc, retval)			       \
  MODULE_SETJMP_1 (handlertype, handlerfunc, retval,			       \
		   internal_handler_##handlertype,			       \
		   internal_cleanup_##handlertype)

#if !__has_attribute (cleanup)
 #error "__attribute__ ((cleanup)) not supported by this compiler; try GCC"
#endif

/* It is very important that pushing the handler doesn't itself raise
   a signal.  Install the cleanup only after the handler has been
   pushed.  Use __attribute__ ((cleanup)) to avoid
   non-local-exit-prone manual cleanup.

   The do-while forces uses of the macro to be followed by a semicolon.
   This macro cannot enclose its entire body inside a do-while, as the
   code after the macro may longjmp back into the macro, which means
   its local variable C must stay live in later code.  */

/* TODO: Make backtraces work if this macros is used.  */

#define MODULE_SETJMP_1(handlertype, handlerfunc, retval, c0, c)	\
  if (module_non_local_exit_check (env) != emacs_funcall_exit_return)	\
    return retval;							\
  struct handler *c0 = push_handler_nosignal (Qt, handlertype);		\
  if (!c0)								\
    {									\
      module_out_of_memory (env);					\
      return retval;							\
    }									\
  struct handler *c __attribute__ ((cleanup (module_reset_handlerlist))) \
    = c0;								\
  if (sys_setjmp (c->jmp))						\
    {									\
      (handlerfunc) (env, c->val);					\
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
   maps global Lisp objects to reference counts.  */

static emacs_value
module_make_global_ref (emacs_env *env, emacs_value ref)
{
  MODULE_FUNCTION_BEGIN (module_nil);
  struct Lisp_Hash_Table *h = XHASH_TABLE (Vmodule_refs_hash);
  Lisp_Object new_obj = value_to_lisp (ref);
  EMACS_UINT hashcode;
  ptrdiff_t i = hash_lookup (h, new_obj, &hashcode);

  if (i >= 0)
    {
      Lisp_Object value = HASH_VALUE (h, i);
      EMACS_INT refcount = XFASTINT (value) + 1;
      if (MOST_POSITIVE_FIXNUM < refcount)
	xsignal0 (Qoverflow_error);
      value = make_natnum (refcount);
      set_hash_value_slot (h, i, value);
    }
  else
    {
      hash_put (h, new_obj, make_natnum (1), hashcode);
    }

  return lisp_to_value (module_assertions ? global_env : env, new_obj);
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

  if (i >= 0)
    {
      EMACS_INT refcount = XFASTINT (HASH_VALUE (h, i)) - 1;
      if (refcount > 0)
        set_hash_value_slot (h, i, make_natnum (refcount));
      else
        {
          eassert (refcount == 0);
          hash_remove_from_table (h, obj);
        }
    }

  if (module_assertions)
    {
      Lisp_Object globals = global_env_private.values;
      Lisp_Object prev = Qnil;
      ptrdiff_t count = 0;
      for (Lisp_Object tail = global_env_private.values; CONSP (tail);
           tail = XCDR (tail))
        {
          emacs_value global = XSAVE_POINTER (XCAR (globals), 0);
          if (global == ref)
            {
              if (NILP (prev))
                global_env_private.values = XCDR (globals);
              else
                XSETCDR (prev, XCDR (globals));
              return;
            }
          ++count;
          prev = globals;
        }
      module_abort ("Global value was not found in list of %"pD"d globals",
                    count);
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
      /* FIXME: lisp_to_value can exit non-locally.  */
      *sym = lisp_to_value (env, p->non_local_exit_symbol);
      *data = lisp_to_value (env, p->non_local_exit_data);
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

static struct Lisp_Module_Function *
allocate_module_function (void)
{
  return ALLOCATE_PSEUDOVECTOR (struct Lisp_Module_Function,
                                min_arity, PVEC_MODULE_FUNCTION);
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
  MODULE_FUNCTION_BEGIN (module_nil);

  if (! (0 <= min_arity
	 && (max_arity < 0
	     ? (min_arity <= MOST_POSITIVE_FIXNUM
		&& max_arity == emacs_variadic_function)
	     : min_arity <= max_arity && max_arity <= MOST_POSITIVE_FIXNUM)))
    xsignal2 (Qinvalid_arity, make_number (min_arity), make_number (max_arity));

  struct Lisp_Module_Function *function = allocate_module_function ();
  function->min_arity = min_arity;
  function->max_arity = max_arity;
  function->subr = subr;
  function->data = data;

  if (documentation)
    {
      AUTO_STRING (unibyte_doc, documentation);
      function->documentation =
        code_convert_string_norecord (unibyte_doc, Qutf_8, false);
    }

  Lisp_Object result;
  XSET_MODULE_FUNCTION (result, function);
  eassert (MODULE_FUNCTIONP (result));

  return lisp_to_value (env, result);
}

static emacs_value
module_funcall (emacs_env *env, emacs_value fun, ptrdiff_t nargs,
		emacs_value args[])
{
  MODULE_FUNCTION_BEGIN (module_nil);

  /* Make a new Lisp_Object array starting with the function as the
     first arg, because that's what Ffuncall takes.  */
  Lisp_Object *newargs;
  USE_SAFE_ALLOCA;
  ptrdiff_t nargs1;
  if (INT_ADD_WRAPV (nargs, 1, &nargs1))
    xsignal0 (Qoverflow_error);
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
  MODULE_FUNCTION_BEGIN (module_nil);
  return lisp_to_value (env, intern (name));
}

static emacs_value
module_type_of (emacs_env *env, emacs_value value)
{
  MODULE_FUNCTION_BEGIN (module_nil);
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
  CHECK_NUMBER (l);
  return XINT (l);
}

static emacs_value
module_make_integer (emacs_env *env, intmax_t n)
{
  MODULE_FUNCTION_BEGIN (module_nil);
  if (FIXNUM_OVERFLOW_P (n))
    xsignal0 (Qoverflow_error);
  return lisp_to_value (env, make_number (n));
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
  MODULE_FUNCTION_BEGIN (module_nil);
  return lisp_to_value (env, make_float (d));
}

static bool
module_copy_string_contents (emacs_env *env, emacs_value value, char *buffer,
			     ptrdiff_t *length)
{
  MODULE_FUNCTION_BEGIN (false);
  Lisp_Object lisp_str = value_to_lisp (value);
  CHECK_STRING (lisp_str);

  Lisp_Object lisp_str_utf8 = ENCODE_UTF_8 (lisp_str);
  ptrdiff_t raw_size = SBYTES (lisp_str_utf8);
  ptrdiff_t required_buf_size = raw_size + 1;

  if (buffer == NULL)
    {
      *length = required_buf_size;
      return true;
    }

  if (*length < required_buf_size)
    {
      *length = required_buf_size;
      xsignal0 (Qargs_out_of_range);
    }

  *length = required_buf_size;
  memcpy (buffer, SDATA (lisp_str_utf8), raw_size + 1);

  return true;
}

static emacs_value
module_make_string (emacs_env *env, const char *str, ptrdiff_t length)
{
  MODULE_FUNCTION_BEGIN (module_nil);
  if (! (0 <= length && length <= STRING_BYTES_BOUND))
    xsignal0 (Qoverflow_error);
  /* FIXME: AUTO_STRING_WITH_LEN requires STR to be null-terminated,
     but we shouldn’t require that.  */
  AUTO_STRING_WITH_LEN (lstr, str, length);
  return lisp_to_value (env,
                        code_convert_string_norecord (lstr, Qutf_8, false));
}

static emacs_value
module_make_user_ptr (emacs_env *env, emacs_finalizer_function fin, void *ptr)
{
  MODULE_FUNCTION_BEGIN (module_nil);
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
    args_out_of_range_3 (make_fixnum_or_float (i),
			 make_number (0), make_number (ASIZE (lvec) - 1));
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
  MODULE_FUNCTION_BEGIN (module_nil);
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

/* This function should return true if and only if maybe_quit would do
   anything.  */
static bool
module_should_quit (emacs_env *env)
{
  MODULE_FUNCTION_BEGIN_NO_CATCH (false);
  return (! NILP (Vquit_flag) && NILP (Vinhibit_quit)) || pending_signals;
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
      xsignal (env->non_local_exit_symbol, env->non_local_exit_data);
    case emacs_funcall_exit_throw:
      Fthrow (env->non_local_exit_symbol, env->non_local_exit_data);
    default:
      eassume (false);
    }
}

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

  Vmodule_runtimes = Fcons (make_save_ptr (rt), Vmodule_runtimes);
  ptrdiff_t count = SPECPDL_INDEX ();
  record_unwind_protect_ptr (finalize_runtime_unwind, rt);

  int r = module_init (rt);

  /* Process the quit flag first, so that quitting doesn't get
     overridden by other non-local exits.  */
  maybe_quit ();

  if (r != 0)
    {
      if (FIXNUM_OVERFLOW_P (r))
        xsignal0 (Qoverflow_error);
      xsignal2 (Qmodule_init_failed, file, make_number (r));
    }

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
    xsignal2 (Qwrong_number_of_arguments, function, make_number (nargs));

  emacs_env pub;
  struct emacs_env_private priv;
  emacs_env *env = initialize_environment (&pub, &priv);
  ptrdiff_t count = SPECPDL_INDEX ();
  record_unwind_protect_ptr (finalize_environment_unwind, env);

  USE_SAFE_ALLOCA;
  ATTRIBUTE_MAY_ALIAS emacs_value *args;
  if (plain_values && ! module_assertions)
    /* FIXME: The cast below is incorrect because the argument array
       is not declared as const, so module functions can modify it.
       Either declare it as const, or remove this branch.  */
    args = (emacs_value *) arglist;
  else
    {
      args = SAFE_ALLOCA (nargs * sizeof *args);
      for (ptrdiff_t i = 0; i < nargs; i++)
	args[i] = lisp_to_value (env, arglist[i]);
    }

  emacs_value ret = func->subr (env, nargs, args, func->data);
  SAFE_FREE ();

  eassert (&priv == env->private_members);

  /* Process the quit flag first, so that quitting doesn't get
     overridden by other non-local exits.  */
  maybe_quit ();

  module_signal_or_throw (&priv);
  return unbind_to (count, value_to_lisp (ret));
}

Lisp_Object
module_function_arity (const struct Lisp_Module_Function *const function)
{
  ptrdiff_t minargs = function->min_arity;
  ptrdiff_t maxargs = function->max_arity;
  return Fcons (make_number (minargs),
		maxargs == MANY ? Qmany : make_number (maxargs));
}


/* Helper functions.  */

static bool
in_current_thread (void)
{
  if (current_thread == NULL)
    return false;
#ifdef HAVE_PTHREAD
  return pthread_equal (pthread_self (), current_thread->thread_id);
#elif defined WINDOWSNT
  return GetCurrentThreadId () == current_thread->thread_id;
#endif
}

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
      if (XSAVE_POINTER (XCAR (tail), 0) == ert)
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
      if (XSAVE_POINTER (XCAR (tail), 0) == env)
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
      p->non_local_exit_symbol = sym;
      p->non_local_exit_data = data;
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
      p->non_local_exit_symbol = tag;
      p->non_local_exit_data = value;
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

/* We represent Lisp objects differently depending on whether the user
   gave -module-assertions.  If assertions are disabled, emacs_value
   objects are Lisp_Objects cast to emacs_value.  If assertions are
   enabled, emacs_value objects are pointers to Lisp_Object objects
   allocated from the free store; they are never freed, which ensures
   that their addresses are unique and can be used for liveness
   checking.  */

/* Unique Lisp_Object used to mark those emacs_values which are really
   just containers holding a Lisp_Object that does not fit as an emacs_value,
   either because it is an integer out of range, or is not properly aligned.
   Used only if !plain_values.  */
static Lisp_Object ltv_mark;

/* Convert V to the corresponding internal object O, such that
   V == lisp_to_value_bits (O).  Never fails.  */
static Lisp_Object
value_to_lisp_bits (emacs_value v)
{
  intptr_t i = (intptr_t) v;
  if (plain_values || USE_LSB_TAG)
    return XIL (i);

  /* With wide EMACS_INT and when tag bits are the most significant,
     reassembling integers differs from reassembling pointers in two
     ways.  First, save and restore the least-significant bits of the
     integer, not the most-significant bits.  Second, sign-extend the
     integer when restoring, but zero-extend pointers because that
     makes TAG_PTR faster.  */

  EMACS_UINT tag = i & (GCALIGNMENT - 1);
  EMACS_UINT untagged = i - tag;
  switch (tag)
    {
    case_Lisp_Int:
      {
	bool negative = tag & 1;
	EMACS_UINT sign_extension
	  = negative ? VALMASK & ~(INTPTR_MAX >> INTTYPEBITS): 0;
	uintptr_t u = i;
	intptr_t all_but_sign = u >> GCTYPEBITS;
	untagged = sign_extension + all_but_sign;
	break;
      }
    }

  return XIL ((tag << VALBITS) + untagged);
}

/* If V was computed from lisp_to_value (O), then return O.
   Exits non-locally only if the stack overflows.  */
static Lisp_Object
value_to_lisp (emacs_value v)
{
  if (module_assertions)
    {
      /* Check the liveness of the value by iterating over all live
         environments.  */
      void *vptr = v;
      ATTRIBUTE_MAY_ALIAS Lisp_Object *optr = vptr;
      ptrdiff_t num_environments = 0;
      ptrdiff_t num_values = 0;
      for (Lisp_Object environments = Vmodule_environments;
           CONSP (environments); environments = XCDR (environments))
        {
          emacs_env *env = XSAVE_POINTER (XCAR (environments), 0);
          for (Lisp_Object values = env->private_members->values;
               CONSP (values); values = XCDR (values))
            {
              Lisp_Object *p = XSAVE_POINTER (XCAR (values), 0);
              if (p == optr)
                return *p;
              ++num_values;
            }
          ++num_environments;
        }
      module_abort (("Emacs value not found in %"pD"d values "
		     "of %"pD"d environments"),
                    num_values, num_environments);
    }

  Lisp_Object o = value_to_lisp_bits (v);
  if (! plain_values && CONSP (o) && EQ (XCDR (o), ltv_mark))
    o = XCAR (o);
  return o;
}

/* Attempt to convert O to an emacs_value.  Do not do any checking
   or allocate any storage; the caller should prevent or detect
   any resulting bit pattern that is not a valid emacs_value.  */
static emacs_value
lisp_to_value_bits (Lisp_Object o)
{
  EMACS_UINT u = XLI (o);

  /* Compress U into the space of a pointer, possibly losing information.  */
  uintptr_t p = (plain_values || USE_LSB_TAG
		 ? u
		 : (INTEGERP (o) ? u << VALBITS : u & VALMASK) + XTYPE (o));
  return (emacs_value) p;
}

/* Convert O to an emacs_value.  Allocate storage if needed; this can
   signal if memory is exhausted.  Must be an injective function.  */
static emacs_value
lisp_to_value (emacs_env *env, Lisp_Object o)
{
  if (module_assertions)
    {
      /* Add the new value to the list of values allocated from this
         environment.  The value is actually a pointer to the
         Lisp_Object cast to emacs_value.  We make a copy of the
         object on the free store to guarantee unique addresses.  */
      ATTRIBUTE_MAY_ALIAS Lisp_Object *optr = xmalloc (sizeof o);
      *optr = o;
      void *vptr = optr;
      ATTRIBUTE_MAY_ALIAS emacs_value ret = vptr;
      struct emacs_env_private *priv = env->private_members;
      priv->values = Fcons (make_save_ptr (ret), priv->values);
      return ret;
    }

  emacs_value v = lisp_to_value_bits (o);

  if (! EQ (o, value_to_lisp_bits (v)))
    {
      /* Package the incompressible object pointer inside a pair
	 that is compressible.  */
      Lisp_Object pair = Fcons (o, ltv_mark);
      v = (emacs_value) ((intptr_t) XCONS (pair) + Lisp_Cons);
    }

  eassert (EQ (o, value_to_lisp (v)));
  return v;
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
  priv->values = priv->non_local_exit_symbol = priv->non_local_exit_data = Qnil;
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
  Vmodule_environments = Fcons (make_save_ptr (env), Vmodule_environments);
  return env;
}

/* Must be called before the lifetime of the environment object
   ends.  */
static void
finalize_environment (emacs_env *env)
{
  eassert (XSAVE_POINTER (XCAR (Vmodule_environments), 0) == env);
  Vmodule_environments = XCDR (Vmodule_environments);
  if (module_assertions)
    /* There is always at least the global environment.  */
    eassert (CONSP (Vmodule_environments));
}

static void
finalize_environment_unwind (void *env)
{
  finalize_environment (env);
}

static void
finalize_runtime_unwind (void* raw_ert)
{
  struct emacs_runtime *ert = raw_ert;
  eassert (XSAVE_POINTER (XCAR (Vmodule_runtimes), 0) == ert);
  Vmodule_runtimes = XCDR (Vmodule_runtimes);
  finalize_environment (ert->private_members->env);
}

void
mark_modules (void)
{
  for (Lisp_Object tail = Vmodule_environments; CONSP (tail);
       tail = XCDR (tail))
    {
      emacs_env *env = XSAVE_POINTER (XCAR (tail), 0);
      struct emacs_env_private *priv = env->private_members;
      mark_object (priv->non_local_exit_symbol);
      mark_object (priv->non_local_exit_data);
      mark_object (priv->values);
    }
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

/* Called on `signal'.  ERR is a pair (SYMBOL . DATA), which gets
   stored in the environment.  Set the pending non-local exit flag.  */
static void
module_handle_signal (emacs_env *env, Lisp_Object err)
{
  module_non_local_exit_signal_1 (env, XCAR (err), XCDR (err));
}

/* Called on `throw'.  TAG_VAL is a pair (TAG . VALUE), which gets
   stored in the environment.  Set the pending non-local exit flag.  */
static void
module_handle_throw (emacs_env *env, Lisp_Object tag_val)
{
  module_non_local_exit_throw_1 (env, XCAR (tag_val), XCDR (tag_val));
}


/* Support for assertions.  */
void
init_module_assertions (bool enable)
{
  module_assertions = enable;
  if (enable)
    {
      /* We use a hidden environment for storing the globals.  This
         environment is never freed.  */
      emacs_env env;
      global_env = initialize_environment (&env, &global_env_private);
      eassert (global_env != &env);
    }
}

static _Noreturn void
ATTRIBUTE_FORMAT_PRINTF(1, 2)
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
  if (!plain_values)
    ltv_mark = Fcons (Qnil, Qnil);
  eassert (NILP (value_to_lisp (module_nil)));

  DEFSYM (Qmodule_refs_hash, "module-refs-hash");
  DEFVAR_LISP ("module-refs-hash", Vmodule_refs_hash,
	       doc: /* Module global reference table.  */);

  Vmodule_refs_hash
    = make_hash_table (hashtest_eq, DEFAULT_HASH_SIZE,
		       DEFAULT_REHASH_SIZE, DEFAULT_REHASH_THRESHOLD,
		       Qnil, false);
  Funintern (Qmodule_refs_hash, Qnil);

  DEFSYM (Qmodule_runtimes, "module-runtimes");
  DEFVAR_LISP ("module-runtimes", Vmodule_runtimes,
               doc: /* List of active module runtimes.  */);
  Vmodule_runtimes = Qnil;
  /* Unintern `module-runtimes' because it is only used
     internally.  */
  Funintern (Qmodule_runtimes, Qnil);

  DEFSYM (Qmodule_environments, "module-environments");
  DEFVAR_LISP ("module-environments", Vmodule_environments,
               doc: /* List of active module environments.  */);
  Vmodule_environments = Qnil;
  /* Unintern `module-environments' because it is only used
     internally.  */
  Funintern (Qmodule_environments, Qnil);

  DEFSYM (Qmodule_load_failed, "module-load-failed");
  Fput (Qmodule_load_failed, Qerror_conditions,
        listn (CONSTYPE_PURE, 2, Qmodule_load_failed, Qerror));
  Fput (Qmodule_load_failed, Qerror_message,
        build_pure_c_string ("Module load failed"));

  DEFSYM (Qmodule_open_failed, "module-open-failed");
  Fput (Qmodule_open_failed, Qerror_conditions,
        listn (CONSTYPE_PURE, 3,
               Qmodule_open_failed, Qmodule_load_failed, Qerror));
  Fput (Qmodule_open_failed, Qerror_message,
        build_pure_c_string ("Module could not be opened"));

  DEFSYM (Qmodule_not_gpl_compatible, "module-not-gpl-compatible");
  Fput (Qmodule_not_gpl_compatible, Qerror_conditions,
        listn (CONSTYPE_PURE, 3,
               Qmodule_not_gpl_compatible, Qmodule_load_failed, Qerror));
  Fput (Qmodule_not_gpl_compatible, Qerror_message,
        build_pure_c_string ("Module is not GPL compatible"));

  DEFSYM (Qmissing_module_init_function, "missing-module-init-function");
  Fput (Qmissing_module_init_function, Qerror_conditions,
        listn (CONSTYPE_PURE, 3,
               Qmissing_module_init_function, Qmodule_load_failed, Qerror));
  Fput (Qmissing_module_init_function, Qerror_message,
        build_pure_c_string ("Module does not export an "
                             "initialization function"));

  DEFSYM (Qmodule_init_failed, "module-init-failed");
  Fput (Qmodule_init_failed, Qerror_conditions,
        listn (CONSTYPE_PURE, 3,
               Qmodule_init_failed, Qmodule_load_failed, Qerror));
  Fput (Qmodule_init_failed, Qerror_message,
        build_pure_c_string ("Module initialization failed"));

  DEFSYM (Qinvalid_arity, "invalid-arity");
  Fput (Qinvalid_arity, Qerror_conditions,
        listn (CONSTYPE_PURE, 2, Qinvalid_arity, Qerror));
  Fput (Qinvalid_arity, Qerror_message,
        build_pure_c_string ("Invalid function arity"));

  /* Unintern `module-refs-hash' because it is internal-only and Lisp
     code or modules should not access it.  */
  Funintern (Qmodule_refs_hash, Qnil);

  DEFSYM (Qmodule_function_p, "module-function-p");

  defsubr (&Smodule_load);
}
