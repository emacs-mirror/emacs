/* Mocklisp compatibility functions for GNU Emacs Lisp interpreter.
   Copyright (C) 1985, 1986, 1995 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* Compatibility for mocklisp */

#include <config.h>
#include "lisp.h"
#include "buffer.h"

DEFUN ("ml-if", Fml_if, Sml_if, 0, UNEVALLED, 0,
       doc: /* Mocklisp version of `if'.
usage: (ml-if COND THEN ELSE...)  */)
     (args)
     Lisp_Object args;
{
  register Lisp_Object val;
  struct gcpro gcpro1;

  val = Qnil;
  GCPRO1 (args);
  while (!NILP (args))
    {
      val = Feval (Fcar (args));
      args = Fcdr (args);
      if (NILP (args)) break;
      if (XINT (val))
	{
	  val = Feval (Fcar (args));
	  break;
	}
      args = Fcdr (args);
    }
  UNGCPRO;
  return val;
}


/* This is the main entry point to mocklisp execution.
   When eval sees a mocklisp function being called, it calls here
   with the unevaluated argument list.  */

Lisp_Object
ml_apply (function, args)
     Lisp_Object function, args;
{
  register int count = specpdl_ptr - specpdl;
  register Lisp_Object val;

  specbind (Qmocklisp_arguments, args);
  val = Fprogn (Fcdr (function));
  return unbind_to (count, val);
}

DEFUN ("ml-nargs", Fml_nargs, Sml_nargs, 0, 0, 0,
       doc: /* Number of arguments to currently executing mocklisp function.  */)
     ()
{
  if (EQ (Vmocklisp_arguments, Qinteractive))
    return make_number (0);
  return Flength (Vmocklisp_arguments);
}

DEFUN ("ml-arg", Fml_arg, Sml_arg, 1, 2, 0,
       doc: /* Argument number N to currently executing mocklisp function.  */)
     (n, prompt)
     Lisp_Object n, prompt;
{
  if (EQ (Vmocklisp_arguments, Qinteractive))
    return Fread_string (prompt, Qnil, Qnil, Qnil, Qnil);
  CHECK_NUMBER (n);
  XSETINT (n, XINT (n) - 1);	/* Mocklisp likes to be origin-1 */
  return Fcar (Fnthcdr (n, Vmocklisp_arguments));
}

DEFUN ("ml-interactive", Fml_interactive, Sml_interactive, 0, 0, 0,
       doc: /* True if currently executing mocklisp function was called interactively.  */)
     ()
{
  return (EQ (Vmocklisp_arguments, Qinteractive)) ? Qt : Qnil;
}

DEFUN ("ml-provide-prefix-argument", Fml_provide_prefix_argument, Sml_provide_prefix_argument,
       2, UNEVALLED, 0,
       doc: /* Evaluate second argument, using first argument as prefix arg value.
usage: (ml-provide-prefix-argument ARG1 ARG2)  */)
     (args)
     Lisp_Object args;
{
  struct gcpro gcpro1;
  GCPRO1 (args);
  Vcurrent_prefix_arg = Feval (Fcar (args));
  UNGCPRO;
  return Feval (Fcar (Fcdr (args)));
}

DEFUN ("ml-prefix-argument-loop", Fml_prefix_argument_loop, Sml_prefix_argument_loop,
       0, UNEVALLED, 0,
       doc: /* usage: (ml-prefix-argument-loop ...)  */)
     (args)
     Lisp_Object args;
{
  register Lisp_Object tem;
  register int i;
  struct gcpro gcpro1;

  /* Set `arg' in case we call a built-in function that looks at it.  Still are a few. */
  if (NILP (Vcurrent_prefix_arg))
    i = 1;
  else
    {
      tem = Vcurrent_prefix_arg;
      if (CONSP (tem))
	tem = Fcar (tem);
      if (EQ (tem, Qminus))
	i = -1;
      else i = XINT (tem);
    }

  GCPRO1 (args);
  while (i-- > 0)
    Fprogn (args);
  UNGCPRO;
  return Qnil;
}

DEFUN ("insert-string", Finsert_string, Sinsert_string, 0, MANY, 0,
       doc: /* Mocklisp-compatibility insert function.
Like the function `insert' except that any argument that is a number
is converted into a string by expressing it in decimal.
usage: (insert-string &rest ARGS)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  register int argnum;
  register Lisp_Object tem;

  for (argnum = 0; argnum < nargs; argnum++)
    {
      tem = args[argnum];
    retry:
      if (INTEGERP (tem))
	tem = Fnumber_to_string (tem);
      if (STRINGP (tem))
	insert1 (tem);
      else
	{
	  tem = wrong_type_argument (Qstringp, tem);
	  goto retry;
	}
    }

  return Qnil;
}


void
syms_of_mocklisp ()
{
  Qmocklisp = intern ("mocklisp");
  staticpro (&Qmocklisp);

  defsubr (&Sml_if);
  defsubr (&Sml_arg);
  defsubr (&Sml_nargs);
  defsubr (&Sml_interactive);
  defsubr (&Sml_provide_prefix_argument);
  defsubr (&Sml_prefix_argument_loop);
  defsubr (&Sinsert_string);
}
