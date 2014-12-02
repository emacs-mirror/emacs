#include <string.h>
#include <config.h>
#include <lisp.h>

int plugin_is_GPL_compatible;

static Lisp_Object Qelisp, Qreplace_regexp_in_string;

#define MAKE_STRING(s) (make_string (s, sizeof(s)-1))

EXFUN (Felisp_test, 0);
DEFUN ("elisp-test", Felisp_test, Selisp_test, 0, 0, 0,
       doc: "Eval some lisp.")
    (void)
{
    Lisp_Object string  = MAKE_STRING ("no-more-dash");
    Lisp_Object regex   = MAKE_STRING ("[-]");
    Lisp_Object replace = MAKE_STRING (" ");
    Lisp_Object res;

    struct gcpro gcpro1, gcpro2, gcpro3;
    GCPRO3 (string, regex, replace);
    res = call3 (Qreplace_regexp_in_string, regex, replace, string);
    UNGCPRO;

    return res;
}


void init ()
{
  DEFSYM (Qelisp, "elisp");
  DEFSYM (Qreplace_regexp_in_string, "replace-regexp-in-string");

  defsubr (&Selisp_test);

  Fprovide (Qelisp, Qnil);
}
