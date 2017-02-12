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
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>

#include "emacs-module.h"

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#include "lisp.h"
#include "dynlib.h"
#include "coding.h"
#include "syssignal.h"

#include <intprops.h>
#include <verify.h>


/* Feature tests.  */

#if __has_attribute (cleanup)
enum { module_has_cleanup = true };
#else
enum { module_has_cleanup = false };
#endif

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

/* Function prototype for the module Lisp functions.  */
typedef emacs_value (*emacs_subr) (emacs_env *, ptrdiff_t,
				   emacs_value [], void *);

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
};

/* The private parts of an `emacs_runtime' object contain the initial
   environment.  */
struct emacs_runtime_private
{
  /* FIXME: Ideally, we would just define "struct emacs_runtime_private"
     as a synonym of "emacs_env", but I don't know how to do that in C.  */
  emacs_env pub;
};


/* Forward declarations.  */

struct module_fun_env;

static Lisp_Object module_format_fun_env (const struct module_fun_env *);
static Lisp_Object value_to_lisp (emacs_value);
static emacs_value lisp_to_value (Lisp_Object);
static enum emacs_funcall_exit module_non_local_exit_check (emacs_env *);
static void check_main_thread (void);
static void finalize_environment (struct emacs_env_private *);
static void initialize_environment (emacs_env *, struct emacs_env_private *priv);
static void module_handle_signal (emacs_env *, Lisp_Object);
static void module_handle_throw (emacs_env *, Lisp_Object);
static void module_non_local_exit_signal_1 (emacs_env *, Lisp_Object, Lisp_Object);
static void module_non_local_exit_throw_1 (emacs_env *, Lisp_Object, Lisp_Object);
static void module_out_of_memory (emacs_env *);
static void module_reset_handlerlist (const int *);

/* We used to return NULL when emacs_value was a different type from
   Lisp_Object, but nowadays we just use Qnil instead.  Although they
   happen to be the same thing in the current implementation, module
   code should not assume this.  */
verify (NIL_IS_ZERO);
static emacs_value const module_nil = 0;

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

/* It is very important that pushing the handler doesn't itself raise
   a signal.  Install the cleanup only after the handler has been
   pushed.  Use __attribute__ ((cleanup)) to avoid
   non-local-exit-prone manual cleanup.

   The do-while forces uses of the macro to be followed by a semicolon.
   This macro cannot enclose its entire body inside a do-while, as the
   code after the macro may longjmp back into the macro, which means
   its local variable C must stay live in later code.  */

/* TODO: Make backtraces work if this macros is used.  */

#define MODULE_SETJMP_1(handlertype, handlerfunc, retval, c, dummy)	\
  if (module_non_local_exit_check (env) != emacs_funcall_exit_return)	\
    return retval;							\
  struct handler *c = push_handler_nosignal (Qt, handlertype);		\
  if (!c)								\
    {									\
      module_out_of_memory (env);					\
      return retval;							\
    }									\
  verify (module_has_cleanup);						\
  int dummy __attribute__ ((cleanup (module_reset_handlerlist)));	\
  if (sys_setjmp (c->jmp))						\
    {									\
      (handlerfunc) (env, c->val);					\
      return retval;							\
    }									\
  do { } while (false)


/* Function environments.  */

/* A function environment is an auxiliary structure used by
   `module_make_function' to store information about a module
   function.  It is stored in a save pointer and retrieved by
   `internal--module-call'.  Its members correspond to the arguments
   given to `module_make_function'.  */

struct module_fun_env
{
  ptrdiff_t min_arity, max_arity;
  emacs_subr subr;
  void *data;
};


/* Implementation of runtime and environment functions.

   These should abide by the following rules:

   1. The first argument should always be a pointer to emacs_env.

   2. Each function should first call check_main_thread.  Note that
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

/* Use MODULE_FUNCTION_BEGIN to implement steps 2 through 4 for most
   environment functions.  On error it will return its argument, which
   should be a sentinel value.  */

#define MODULE_FUNCTION_BEGIN(error_retval)                             \
  check_main_thread ();                                                 \
  if (module_non_local_exit_check (env) != emacs_funcall_exit_return)   \
    return error_retval;                                                \
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
  check_main_thread ();
  return &ert->private_members->pub;
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

  return lisp_to_value (new_obj);
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
  EMACS_UINT hashcode;
  ptrdiff_t i = hash_lookup (h, obj, &hashcode);

  if (i >= 0)
    {
      Lisp_Object value = HASH_VALUE (h, i);
      EMACS_INT refcount = XFASTINT (value) - 1;
      if (refcount > 0)
        {
          value = make_natnum (refcount);
          set_hash_value_slot (h, i, value);
        }
      else
	hash_remove_from_table (h, value);
    }
}

static enum emacs_funcall_exit
module_non_local_exit_check (emacs_env *env)
{
  check_main_thread ();
  return env->private_members->pending_non_local_exit;
}

static void
module_non_local_exit_clear (emacs_env *env)
{
  check_main_thread ();
  env->private_members->pending_non_local_exit = emacs_funcall_exit_return;
}

static enum emacs_funcall_exit
module_non_local_exit_get (emacs_env *env, emacs_value *sym, emacs_value *data)
{
  check_main_thread ();
  struct emacs_env_private *p = env->private_members;
  if (p->pending_non_local_exit != emacs_funcall_exit_return)
    {
      /* FIXME: lisp_to_value can exit non-locally.  */
      *sym = lisp_to_value (p->non_local_exit_symbol);
      *data = lisp_to_value (p->non_local_exit_data);
    }
  return p->pending_non_local_exit;
}

/* Like for `signal', DATA must be a list.  */
static void
module_non_local_exit_signal (emacs_env *env, emacs_value sym, emacs_value data)
{
  check_main_thread ();
  if (module_non_local_exit_check (env) == emacs_funcall_exit_return)
    module_non_local_exit_signal_1 (env, value_to_lisp (sym),
				    value_to_lisp (data));
}

static void
module_non_local_exit_throw (emacs_env *env, emacs_value tag, emacs_value value)
{
  check_main_thread ();
  if (module_non_local_exit_check (env) == emacs_funcall_exit_return)
    module_non_local_exit_throw_1 (env, value_to_lisp (tag),
				   value_to_lisp (value));
}

/* A module function is lambda function that calls
   `internal--module-call', passing the function pointer of the module
   function along with the module emacs_env pointer as arguments.

	(function (lambda (&rest arglist)
		    (internal--module-call envobj arglist)))  */

static emacs_value
module_make_function (emacs_env *env, ptrdiff_t min_arity, ptrdiff_t max_arity,
		      emacs_subr subr, const char *documentation,
		      void *data)
{
  MODULE_FUNCTION_BEGIN (module_nil);

  if (! (0 <= min_arity
	 && (max_arity < 0
	     ? max_arity == emacs_variadic_function
	     : min_arity <= max_arity)))
    xsignal2 (Qinvalid_arity, make_number (min_arity), make_number (max_arity));

  /* FIXME: This should be freed when envobj is GC'd.  */
  struct module_fun_env *envptr = xmalloc (sizeof *envptr);
  envptr->min_arity = min_arity;
  envptr->max_arity = max_arity;
  envptr->subr = subr;
  envptr->data = data;

  Lisp_Object envobj = make_save_ptr (envptr);
  Lisp_Object doc = Qnil;
  if (documentation)
    {
      AUTO_STRING (unibyte_doc, documentation);
      doc = code_convert_string_norecord (unibyte_doc, Qutf_8, false);
    }

  /* FIXME: Use a bytecompiled object, or even better a subr.  */
  Lisp_Object ret = list4 (Qlambda,
                           list2 (Qand_rest, Qargs),
                           doc,
                           list4 (Qapply,
                                  list2 (Qfunction, Qinternal__module_call),
                                  envobj,
                                  Qargs));

  return lisp_to_value (ret);
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
  emacs_value result = lisp_to_value (Ffuncall (nargs1, newargs));
  SAFE_FREE ();
  return result;
}

static emacs_value
module_intern (emacs_env *env, const char *name)
{
  MODULE_FUNCTION_BEGIN (module_nil);
  return lisp_to_value (intern (name));
}

static emacs_value
module_type_of (emacs_env *env, emacs_value value)
{
  MODULE_FUNCTION_BEGIN (module_nil);
  return lisp_to_value (Ftype_of (value_to_lisp (value)));
}

static bool
module_is_not_nil (emacs_env *env, emacs_value value)
{
  check_main_thread ();
  if (module_non_local_exit_check (env) != emacs_funcall_exit_return)
    return false;
  return ! NILP (value_to_lisp (value));
}

static bool
module_eq (emacs_env *env, emacs_value a, emacs_value b)
{
  check_main_thread ();
  if (module_non_local_exit_check (env) != emacs_funcall_exit_return)
    return false;
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
  return lisp_to_value (make_number (n));
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
  return lisp_to_value (make_float (d));
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

  eassert (length != NULL);

  if (buffer == NULL)
    {
      *length = required_buf_size;
      return true;
    }

  eassert (*length >= 0);

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
  AUTO_STRING_WITH_LEN (lstr, str, length);
  return lisp_to_value (code_convert_string_norecord (lstr, Qutf_8, false));
}

static emacs_value
module_make_user_ptr (emacs_env *env, emacs_finalizer_function fin, void *ptr)
{
  MODULE_FUNCTION_BEGIN (module_nil);
  return lisp_to_value (make_user_ptr (fin, ptr));
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
  /* FIXME: This function should return bool because it can fail.  */
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
  /* FIXME: This function should return bool because it can fail.  */
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
  /* FIXME: This function should return bool because it can fail.  */
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
  return lisp_to_value (AREF (lvec, i));
}

static ptrdiff_t
module_vec_size (emacs_env *env, emacs_value vec)
{
  /* FIXME: Return a sentinel value (e.g., -1) on error.  */
  MODULE_FUNCTION_BEGIN (0);
  Lisp_Object lvec = value_to_lisp (vec);
  CHECK_VECTOR (lvec);
  return ASIZE (lvec);
}


/* Subroutines.  */

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
    error ("Cannot load file %s: %s", SDATA (file), dynlib_error ());

  gpl_sym = dynlib_sym (handle, "plugin_is_GPL_compatible");
  if (!gpl_sym)
    error ("Module %s is not GPL compatible", SDATA (file));

  module_init = (emacs_init_function) dynlib_func (handle, "emacs_module_init");
  if (!module_init)
    error ("Module %s does not have an init function.", SDATA (file));

  struct emacs_runtime_private rt; /* Includes the public emacs_env.  */
  struct emacs_env_private priv;
  initialize_environment (&rt.pub, &priv);
  struct emacs_runtime pub =
    {
      .size = sizeof pub,
      .private_members = &rt,
      .get_environment = module_get_environment
    };
  int r = module_init (&pub);
  finalize_environment (&priv);

  if (r != 0)
    {
      if (FIXNUM_OVERFLOW_P (r))
        xsignal0 (Qoverflow_error);
      xsignal2 (Qmodule_load_failed, file, make_number (r));
    }

  return Qt;
}

DEFUN ("internal--module-call", Finternal_module_call, Sinternal_module_call, 1, MANY, 0,
       doc: /* Internal function to call a module function.
ENVOBJ is a save pointer to a module_fun_env structure.
ARGLIST is a list of arguments passed to SUBRPTR.
usage: (module-call ENVOBJ &rest ARGLIST)   */)
  (ptrdiff_t nargs, Lisp_Object *arglist)
{
  Lisp_Object envobj = arglist[0];
  /* FIXME: Rather than use a save_value, we should create a new object type.
     Making save_value visible to Lisp is wrong.  */
  CHECK_TYPE (SAVE_VALUEP (envobj), Qsave_value_p, envobj);
  struct Lisp_Save_Value *save_value = XSAVE_VALUE (envobj);
  CHECK_TYPE (save_type (save_value, 0) == SAVE_POINTER, Qsave_pointer_p, envobj);
  /* FIXME: We have no reason to believe that XSAVE_POINTER (envobj, 0)
     is a module_fun_env pointer.  If some other part of Emacs also
     exports save_value objects to Elisp, than we may be getting here this
     other kind of save_value which will likely hold something completely
     different in this field.  */
  struct module_fun_env *envptr = XSAVE_POINTER (envobj, 0);
  EMACS_INT len = nargs - 1;
  eassume (0 <= envptr->min_arity);
  if (! (envptr->min_arity <= len
	 && len <= (envptr->max_arity < 0 ? PTRDIFF_MAX : envptr->max_arity)))
    xsignal2 (Qwrong_number_of_arguments, module_format_fun_env (envptr),
	      make_number (len));

  emacs_env pub;
  struct emacs_env_private priv;
  initialize_environment (&pub, &priv);

  USE_SAFE_ALLOCA;
  emacs_value *args;
  if (plain_values)
    args = (emacs_value *) arglist + 1;
  else
    {
      args = SAFE_ALLOCA (len * sizeof *args);
      for (ptrdiff_t i = 0; i < len; i++)
	args[i] = lisp_to_value (arglist[i + 1]);
    }

  emacs_value ret = envptr->subr (&pub, len, args, envptr->data);
  SAFE_FREE ();

  eassert (&priv == pub.private_members);

  switch (priv.pending_non_local_exit)
    {
    case emacs_funcall_exit_return:
      finalize_environment (&priv);
      return value_to_lisp (ret);
    case emacs_funcall_exit_signal:
      {
        Lisp_Object symbol = priv.non_local_exit_symbol;
        Lisp_Object data = priv.non_local_exit_data;
        finalize_environment (&priv);
        xsignal (symbol, data);
      }
    case emacs_funcall_exit_throw:
      {
        Lisp_Object tag = priv.non_local_exit_symbol;
        Lisp_Object value = priv.non_local_exit_data;
        finalize_environment (&priv);
        Fthrow (tag, value);
      }
    default:
      eassume (false);
    }
}


/* Helper functions.  */

static void
check_main_thread (void)
{
#ifdef HAVE_PTHREAD
  eassert (pthread_equal (pthread_self (), main_thread_id));
#elif defined WINDOWSNT
  eassert (GetCurrentThreadId () == dwMainThreadId);
#endif
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
  Lisp_Object o = value_to_lisp_bits (v);
  if (! plain_values && CONSP (o) && EQ (XCDR (o), ltv_mark))
    o = XCAR (o);
  return o;
}

/* Attempt to convert O to an emacs_value.  Do not do any checking or
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

#ifndef HAVE_STRUCT_ATTRIBUTE_ALIGNED
enum { HAVE_STRUCT_ATTRIBUTE_ALIGNED = 0 };
#endif

/* Convert O to an emacs_value.  Allocate storage if needed; this can
   signal if memory is exhausted.  Must be an injective function.  */
static emacs_value
lisp_to_value (Lisp_Object o)
{
  emacs_value v = lisp_to_value_bits (o);

  if (! EQ (o, value_to_lisp_bits (v)))
    {
      /* Package the incompressible object pointer inside a pair
	 that is compressible.  */
      Lisp_Object pair = Fcons (o, ltv_mark);

      if (! HAVE_STRUCT_ATTRIBUTE_ALIGNED)
	{
	  /* Keep calling Fcons until it returns a compressible pair.
	     This shouldn't take long.  */
	  while ((intptr_t) XCONS (pair) & (GCALIGNMENT - 1))
	    pair = Fcons (o, pair);

	  /* Plant the mark.  The garbage collector will eventually
	     reclaim any just-allocated incompressible pairs.  */
	  XSETCDR (pair, ltv_mark);
	}

      v = (emacs_value) ((intptr_t) XCONS (pair) + Lisp_Cons);
    }

  eassert (EQ (o, value_to_lisp (v)));
  return v;
}


/* Environment lifetime management.  */

/* Must be called before the environment can be used.  */
static void
initialize_environment (emacs_env *env, struct emacs_env_private *priv)
{
  priv->pending_non_local_exit = emacs_funcall_exit_return;
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
  Vmodule_environments = Fcons (make_save_ptr (env), Vmodule_environments);
}

/* Must be called before the lifetime of the environment object
   ends.  */
static void
finalize_environment (struct emacs_env_private *env)
{
  Vmodule_environments = XCDR (Vmodule_environments);
}


/* Non-local exit handling.  */

/* Must be called after setting up a handler immediately before
   returning from the function.  See the comments in lisp.h and the
   code in eval.c for details.  The macros below arrange for this
   function to be called automatically.  DUMMY is ignored.  */
static void
module_reset_handlerlist (const int *dummy)
{
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


/* Function environments.  */

/* Return a string object that contains a user-friendly
   representation of the function environment.  */
static Lisp_Object
module_format_fun_env (const struct module_fun_env *env)
{
  /* Try to print a function name if possible.  */
  const char *path, *sym;
  static char const noaddr_format[] = "#<module function at %p>";
  char buffer[sizeof noaddr_format + INT_STRLEN_BOUND (intptr_t) + 256];
  char *buf = buffer;
  ptrdiff_t bufsize = sizeof buffer;
  ptrdiff_t size
    = (dynlib_addr (env->subr, &path, &sym)
       ? exprintf (&buf, &bufsize, buffer, -1,
		   "#<module function %s from %s>", sym, path)
       : sprintf (buffer, noaddr_format, env->subr));
  AUTO_STRING_WITH_LEN (unibyte_result, buffer, size);
  Lisp_Object result = code_convert_string_norecord (unibyte_result,
						     Qutf_8, false);
  if (buf != buffer)
    xfree (buf);
  return result;
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
    = make_hash_table (hashtest_eq, make_number (DEFAULT_HASH_SIZE),
		       make_float (DEFAULT_REHASH_SIZE),
		       make_float (DEFAULT_REHASH_THRESHOLD),
		       Qnil, Qnil);
  Funintern (Qmodule_refs_hash, Qnil);

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

  DEFSYM (Qinvalid_module_call, "invalid-module-call");
  Fput (Qinvalid_module_call, Qerror_conditions,
        listn (CONSTYPE_PURE, 2, Qinvalid_module_call, Qerror));
  Fput (Qinvalid_module_call, Qerror_message,
        build_pure_c_string ("Invalid module call"));

  DEFSYM (Qinvalid_arity, "invalid-arity");
  Fput (Qinvalid_arity, Qerror_conditions,
        listn (CONSTYPE_PURE, 2, Qinvalid_arity, Qerror));
  Fput (Qinvalid_arity, Qerror_message,
        build_pure_c_string ("Invalid function arity"));

  /* Unintern `module-refs-hash' because it is internal-only and Lisp
     code or modules should not access it.  */
  Funintern (Qmodule_refs_hash, Qnil);

  DEFSYM (Qsave_value_p, "save-value-p");
  DEFSYM (Qsave_pointer_p, "save-pointer-p");

  defsubr (&Smodule_load);

  DEFSYM (Qinternal__module_call, "internal--module-call");
  defsubr (&Sinternal_module_call);
}
