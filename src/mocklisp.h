/* Fundamental definitions for emulating mocklisp.
   Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.

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

extern Lisp_Object ml_apply ();
extern Lisp_Object Fml_if ();
extern Lisp_Object Fml_nargs ();
extern Lisp_Object Fml_arg ();
extern Lisp_Object Fml_interactive ();
extern Lisp_Object Fml_provide_prefix_argument ();
extern Lisp_Object Fml_prefix_argument_loop ();
extern Lisp_Object Finsert_string ();
