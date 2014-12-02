#include <config.h>
#include <lisp.h>

#include <math.h>

/* emacs checks for this symbol before running the module */

int plugin_is_GPL_compatible;

/* module feature name */
static Lisp_Object Qfmod;

/* define a new lisp function */

EXFUN (Ffmod, 2);
DEFUN ("fmod", Ffmod, Sfmod, 2, 2, 0,
       doc: "Returns the floating-point remainder of NUMER/DENOM")
  (Lisp_Object numer, Lisp_Object denom)
{
  return make_float (fmod (extract_float (numer), extract_float (denom)));
}

EXFUN (Ffmod_test1, 0);
DEFUN ("fmod-test1", Ffmod_test1, Sfmod_test1, 0, 0, 0,
       doc: "Return 1")
  (void)
{
  return make_float (1.);
}

EXFUN (Ffmod_test2, 0);
DEFUN ("fmod-test2", Ffmod_test2, Sfmod_test2, 0, 0, 0,
       doc: "Return 2")
  (void)
{
  return make_float (2.);
}


EXFUN (Ffmod_test3, 0);
DEFUN ("fmod-test3", Ffmod_test3, Sfmod_test3, 0, 0, 0,
       doc: "Return 3")
  (void)
{
  return make_float (3.);
}

/* entry point of the module */

void init ()
{
  DEFSYM (Qfmod, "fmod");

  defsubr (&Sfmod);
  defsubr (&Sfmod_test1);
  defsubr (&Sfmod_test2);
  defsubr (&Sfmod_test3);

  Fprovide (Qfmod, Qnil);
}
