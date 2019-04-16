/* Test GNU Emacs modules.

Copyright 2015-2019 Free Software Foundation, Inc.

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

#include "config.h"

#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <emacs-module.h>

#include "timespec.h"

int plugin_is_GPL_compatible;

#if INTPTR_MAX <= 0
# error "INTPTR_MAX misconfigured"
#elif INTPTR_MAX <= INT_MAX || INTPTR_MAX <= LONG_MAX
# define pT "ld"
# define pZ "lu"
# define T_TYPE long
# define Z_TYPE unsigned long
#elif INTPTR_MAX <= INT64_MAX
# ifdef __MINGW32__
#  define pT "lld"
#  define pZ "llu"
#  define T_TYPE long long
#  define Z_TYPE unsigned long long
# else
#  define pT "ld"
#  define pZ "lu"
#  define T_TYPE long
#  define Z_TYPE unsigned long
# endif
#else
# error "INTPTR_MAX too large"
#endif


/* Always return symbol 't'.  */
static emacs_value
Fmod_test_return_t (emacs_env *env, ptrdiff_t nargs, emacs_value args[],
		    void *data)
{
  return env->intern (env, "t");
}

/* Expose simple sum function.  */
static intmax_t
sum (intmax_t a, intmax_t b)
{
  return a + b;
}

static emacs_value
Fmod_test_sum (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  assert (nargs == 2);

  intmax_t a = env->extract_integer (env, args[0]);
  intmax_t b = env->extract_integer (env, args[1]);

  intmax_t r = sum (a, b);

  return env->make_integer (env, r);
}


/* Signal '(error 56).  */
static emacs_value
Fmod_test_signal (emacs_env *env, ptrdiff_t nargs, emacs_value args[],
		  void *data)
{
  assert (env->non_local_exit_check (env) == emacs_funcall_exit_return);
  env->non_local_exit_signal (env, env->intern (env, "error"),
			      env->make_integer (env, 56));
  return NULL;
}


/* Throw '(tag 65).  */
static emacs_value
Fmod_test_throw (emacs_env *env, ptrdiff_t nargs, emacs_value args[],
		 void *data)
{
  assert (env->non_local_exit_check (env) == emacs_funcall_exit_return);
  env->non_local_exit_throw (env, env->intern (env, "tag"),
			     env->make_integer (env, 65));
  return NULL;
}


/* Call argument function, catch all non-local exists and return
   either normal result or a list describing the non-local exit.  */
static emacs_value
Fmod_test_non_local_exit_funcall (emacs_env *env, ptrdiff_t nargs,
				  emacs_value args[], void *data)
{
  assert (nargs == 1);
  emacs_value result = env->funcall (env, args[0], 0, NULL);
  emacs_value non_local_exit_symbol, non_local_exit_data;
  enum emacs_funcall_exit code
    = env->non_local_exit_get (env, &non_local_exit_symbol,
			       &non_local_exit_data);
  switch (code)
    {
    case emacs_funcall_exit_return:
      return result;
    case emacs_funcall_exit_signal:
      {
        env->non_local_exit_clear (env);
        emacs_value Flist = env->intern (env, "list");
        emacs_value list_args[] = {env->intern (env, "signal"),
				   non_local_exit_symbol, non_local_exit_data};
        return env->funcall (env, Flist, 3, list_args);
      }
    case emacs_funcall_exit_throw:
      {
        env->non_local_exit_clear (env);
        emacs_value Flist = env->intern (env, "list");
        emacs_value list_args[] = {env->intern (env, "throw"),
				   non_local_exit_symbol, non_local_exit_data};
        return env->funcall (env, Flist, 3, list_args);
      }
    }

  /* Never reached.  */
  return env->intern (env, "nil");;
}


/* Return a global reference.  */
static emacs_value
Fmod_test_globref_make (emacs_env *env, ptrdiff_t nargs, emacs_value args[],
			void *data)
{
  /* Make a big string and make it global.  */
  char str[26 * 100];
  for (int i = 0; i < sizeof str; i++)
    str[i] = 'a' + (i % 26);

  /* We don't need to null-terminate str.  */
  emacs_value lisp_str = env->make_string (env, str, sizeof str);
  return env->make_global_ref (env, lisp_str);
}

/* Create a few global references from arguments and free them.  */
static emacs_value
Fmod_test_globref_free (emacs_env *env, ptrdiff_t nargs, emacs_value args[],
			void *data)
{
  emacs_value refs[10];
  for (int i = 0; i < 10; i++)
    {
      refs[i] = env->make_global_ref (env, args[i % nargs]);
    }
  for (int i = 0; i < 10; i++)
    {
      env->free_global_ref (env, refs[i]);
    }
  return env->intern (env, "ok");
}



/* Return a copy of the argument string where every 'a' is replaced
   with 'b'.  */
static emacs_value
Fmod_test_string_a_to_b (emacs_env *env, ptrdiff_t nargs, emacs_value args[],
			 void *data)
{
  emacs_value lisp_str = args[0];
  ptrdiff_t size = 0;
  char * buf = NULL;

  env->copy_string_contents (env, lisp_str, buf, &size);
  buf = malloc (size);
  env->copy_string_contents (env, lisp_str, buf, &size);

  for (ptrdiff_t i = 0; i + 1 < size; i++)
    if (buf[i] == 'a')
      buf[i] = 'b';

  return env->make_string (env, buf, size - 1);
}


/* Embedded pointers in lisp objects.  */

/* C struct (pointer to) that will be embedded.  */
struct super_struct
{
  int amazing_int;
  char large_unused_buffer[512];
};

/* Return a new user-pointer to a super_struct, with amazing_int set
   to the passed parameter.  */
static emacs_value
Fmod_test_userptr_make (emacs_env *env, ptrdiff_t nargs, emacs_value args[],
			void *data)
{
  struct super_struct *p = calloc (1, sizeof *p);
  p->amazing_int = env->extract_integer (env, args[0]);
  return env->make_user_ptr (env, free, p);
}

/* Return the amazing_int of a passed 'user-pointer to a super_struct'.  */
static emacs_value
Fmod_test_userptr_get (emacs_env *env, ptrdiff_t nargs, emacs_value args[],
		       void *data)
{
  struct super_struct *p = env->get_user_ptr (env, args[0]);
  return env->make_integer (env, p->amazing_int);
}


/* Fill vector in args[0] with value in args[1].  */
static emacs_value
Fmod_test_vector_fill (emacs_env *env, ptrdiff_t nargs, emacs_value args[],
		       void *data)
{
  emacs_value vec = args[0];
  emacs_value val = args[1];
  ptrdiff_t size = env->vec_size (env, vec);
  for (ptrdiff_t i = 0; i < size; i++)
    env->vec_set (env, vec, i, val);
  return env->intern (env, "t");
}


/* Return whether all elements of vector in args[0] are 'eq' to value
   in args[1].  */
static emacs_value
Fmod_test_vector_eq (emacs_env *env, ptrdiff_t nargs, emacs_value args[],
		     void *data)
{
  emacs_value vec = args[0];
  emacs_value val = args[1];
  ptrdiff_t size = env->vec_size (env, vec);
  for (ptrdiff_t i = 0; i < size; i++)
    if (!env->eq (env, env->vec_get (env, vec, i), val))
        return env->intern (env, "nil");
  return env->intern (env, "t");
}

static emacs_value invalid_stored_value;

/* The next two functions perform a possibly-invalid operation: they
   store a value in a static variable and load it.  This causes
   undefined behavior if the environment that the value was created
   from is no longer live.  The module assertions check for this
   error.  */

static emacs_value
Fmod_test_invalid_store (emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                         void *data)
{
  return invalid_stored_value = env->make_integer (env, 123);
}

static emacs_value
Fmod_test_invalid_load (emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                        void *data)
{
  return invalid_stored_value;
}

/* An invalid finalizer: Finalizers are run during garbage collection,
   where Lisp code can’t be executed.  -module-assertions tests for
   this case.  */

static emacs_env *current_env;

static void
invalid_finalizer (void *ptr)
{
  current_env->intern (current_env, "nil");
}

static emacs_value
Fmod_test_invalid_finalizer (emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                             void *data)
{
  current_env = env;
  env->make_user_ptr (env, invalid_finalizer, NULL);
  return env->intern (env, "nil");
}

static void
signal_wrong_type_argument (emacs_env *env, const char *predicate,
                            emacs_value arg)
{
  emacs_value symbol = env->intern (env, "wrong-type-argument");
  emacs_value elements[2] = {env->intern (env, predicate), arg};
  emacs_value data = env->funcall (env, env->intern (env, "list"), 2, elements);
  env->non_local_exit_signal (env, symbol, data);
}

static void
signal_errno (emacs_env *env, const char *function)
{
  const char *message = strerror (errno);
  emacs_value message_value = env->make_string (env, message, strlen (message));
  emacs_value symbol = env->intern (env, "file-error");
  emacs_value elements[2]
    = {env->make_string (env, function, strlen (function)), message_value};
  emacs_value data = env->funcall (env, env->intern (env, "list"), 2, elements);
  env->non_local_exit_signal (env, symbol, data);
}

/* A long-running operation that occasionally calls `should_quit' or
   `process_input'.  */

static emacs_value
Fmod_test_sleep_until (emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                       void *data)
{
  assert (nargs == 2);
  const double until_seconds = env->extract_float (env, args[0]);
  if (env->non_local_exit_check (env))
    return NULL;
  if (until_seconds <= 0)
    {
      signal_wrong_type_argument (env, "cl-plusp", args[0]);
      return NULL;
    }
  const bool process_input = env->is_not_nil (env, args[1]);
  const struct timespec until = dtotimespec (until_seconds);
  const struct timespec amount = make_timespec(0,  10000000);
  while (true)
    {
      const struct timespec now = current_timespec ();
      if (timespec_cmp (now, until) >= 0)
        break;
      if (nanosleep (&amount, NULL) && errno != EINTR)
        {
          signal_errno (env, "nanosleep");
          return NULL;
        }
      if ((process_input
           && env->process_input (env) == emacs_process_input_quit)
          || env->should_quit (env))
        return NULL;
    }
  return env->intern (env, "finished");
}

/* Lisp utilities for easier readability (simple wrappers).  */

/* Provide FEATURE to Emacs.  */
static void
provide (emacs_env *env, const char *feature)
{
  emacs_value Qfeat = env->intern (env, feature);
  emacs_value Qprovide = env->intern (env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall (env, Qprovide, 1, args);
}

/* Bind NAME to FUN.  */
static void
bind_function (emacs_env *env, const char *name, emacs_value Sfun)
{
  emacs_value Qdefalias = env->intern (env, "defalias");
  emacs_value Qsym = env->intern (env, name);
  emacs_value args[] = { Qsym, Sfun };

  env->funcall (env, Qdefalias, 2, args);
}

/* Module init function.  */
int
emacs_module_init (struct emacs_runtime *ert)
{
  if (ert->size < sizeof *ert)
    {
      fprintf (stderr, "Runtime size of runtime structure (%"pT" bytes) "
               "smaller than compile-time size (%"pZ" bytes)",
               (T_TYPE) ert->size, (Z_TYPE) sizeof (*ert));
      return 1;
    }

  emacs_env *env = ert->get_environment (ert);

  if (env->size < sizeof *env)
    {
      fprintf (stderr, "Runtime size of environment structure (%"pT" bytes) "
               "smaller than compile-time size (%"pZ" bytes)",
               (T_TYPE) env->size, (Z_TYPE) sizeof (*env));
      return 2;
    }

#define DEFUN(lsym, csym, amin, amax, doc, data) \
  bind_function (env, lsym, \
		 env->make_function (env, amin, amax, csym, doc, data))

  DEFUN ("mod-test-return-t", Fmod_test_return_t, 1, 1, NULL, NULL);
  DEFUN ("mod-test-sum", Fmod_test_sum, 2, 2, "Return A + B\n\n(fn a b)", NULL);
  DEFUN ("mod-test-signal", Fmod_test_signal, 0, 0, NULL, NULL);
  DEFUN ("mod-test-throw", Fmod_test_throw, 0, 0, NULL, NULL);
  DEFUN ("mod-test-non-local-exit-funcall", Fmod_test_non_local_exit_funcall,
	 1, 1, NULL, NULL);
  DEFUN ("mod-test-globref-make", Fmod_test_globref_make, 0, 0, NULL, NULL);
  DEFUN ("mod-test-globref-free", Fmod_test_globref_free, 4, 4, NULL, NULL);
  DEFUN ("mod-test-string-a-to-b", Fmod_test_string_a_to_b, 1, 1, NULL, NULL);
  DEFUN ("mod-test-userptr-make", Fmod_test_userptr_make, 1, 1, NULL, NULL);
  DEFUN ("mod-test-userptr-get", Fmod_test_userptr_get, 1, 1, NULL, NULL);
  DEFUN ("mod-test-vector-fill", Fmod_test_vector_fill, 2, 2, NULL, NULL);
  DEFUN ("mod-test-vector-eq", Fmod_test_vector_eq, 2, 2, NULL, NULL);
  DEFUN ("mod-test-invalid-store", Fmod_test_invalid_store, 0, 0, NULL, NULL);
  DEFUN ("mod-test-invalid-load", Fmod_test_invalid_load, 0, 0, NULL, NULL);
  DEFUN ("mod-test-invalid-finalizer", Fmod_test_invalid_finalizer, 0, 0,
         NULL, NULL);
  DEFUN ("mod-test-sleep-until", Fmod_test_sleep_until, 2, 2, NULL, NULL);

#undef DEFUN

  provide (env, "mod-test");
  return 0;
}
