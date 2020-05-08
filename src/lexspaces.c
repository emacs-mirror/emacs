/* Copyright (C) 2020 Free Software Foundation, Inc.

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

EMACS_INT curr_lexspace;

/* Store lexnumber in closure + set lexspace calling subrs.  */

static void
lexspace_copy (EMACS_INT dst, EMACS_INT src)
{
  Lisp_Object tail;
  for (ptrdiff_t i = ASIZE (Vobarray) - 1; i >= 0; i--)
    {
      tail = AREF (Vobarray, i);
      if (SYMBOLP (tail))
	while (1)
	  {
	    struct Lisp_Symbol *sym = XSYMBOL (tail);
	    if (sym->u.s.redirect == SYMBOL_PLAINVAL
		&& !EQ (sym->u.s.val.value, Qunbound))
	      {
		struct Lisp_Binding *binding = XBINDING (sym->u.s.val.value);
		binding->b[dst] = binding->b[src];
	      }
	    if (!NILP (sym->u.s._function))
	      {
		struct Lisp_Binding *binding = XBINDING (sym->u.s._function);
		binding->b[dst] = binding->b[src];
	      }
	    if (sym->u.s.next == 0)
	      break;
	    XSETSYMBOL (tail, sym->u.s.next);
	  }
    }
}


/**********************************/
/* Entry points exposed to Lisp.  */
/**********************************/

DEFUN ("lexspace-make-from", Flexspace_make_from, Slexspace_make_from, 2, 2, 0,
       doc: /* Make lexspace NAME from SRC.   */)
  (Lisp_Object name, Lisp_Object src)
{
  CHECK_SYMBOL (name);
  CHECK_SYMBOL (src);
  EMACS_INT lexspace_num = XFIXNUM (Fhash_table_count (Vlexspaces));
  if (lexspace_num == MAX_LEXSPACES)
    error ("Max number of lexspaces reached");
  Lisp_Object src_lex_n = Fgethash (src, Vlexspaces, Qnil);
  if (NILP (src_lex_n))
    error ("lexspace %s does not exists", SSDATA (SYMBOL_NAME (src)));

  Fputhash (name, make_fixnum (lexspace_num), Vlexspaces);
  lexspace_copy (lexspace_num, XFIXNUM (src_lex_n));

  return name;
}

DEFUN ("in-lexspace", Fin_lexspace, Sin_lexspace, 1, 1, 0,
       doc: /* Set NAME as current lexspace.  Create it in case.   */)
  (Lisp_Object name)
{
  CHECK_SYMBOL (name);
  return name;
}

void
syms_of_lexspaces (void)
{
  DEFSYM (Qbinding, "binding");
  DEFSYM (Qel, "el");

  /* Internal use!  */
  DEFVAR_LISP ("current-lexspace-name", Vcurrent_lexspace_name,
	       doc: /* Internal use.  */);
  Vcurrent_lexspace_name = Qel;
  DEFVAR_LISP ("lexspaces", Vlexspaces,
	       doc: /* Internal use.  */);
  Vlexspaces = CALLN (Fmake_hash_table, QCtest, Qeq);
  Fputhash (Qel, make_fixnum (0), Vlexspaces);

  defsubr (&Sin_lexspace);
  defsubr (&Slexspace_make_from);
}
