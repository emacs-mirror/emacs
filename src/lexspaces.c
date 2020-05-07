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
  DEFVAR_LISP ("current-lexspace-name", Vcurrent_lexspace_name,
	       doc: /* Internal use.  */);
  Vcurrent_lexspace_name = Qel;

  defsubr (&Sin_lexspace);
}
