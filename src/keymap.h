/* Functions to manipulate keymaps.
   Copyright (C) 2001  Free Software Foundation, Inc.

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

#ifndef KEYMAP_H
#define KEYMAP_H

#define KEYMAPP(m) (!NILP (get_keymap (m, 0, 0)))
extern Lisp_Object Qkeymap, Qmenu_bar;
extern Lisp_Object current_global_map;
EXFUN (Fmake_sparse_keymap, 1);
EXFUN (Fkeymap_prompt, 1);
EXFUN (Fdefine_key, 3);
EXFUN (Flookup_key, 3);
EXFUN (Fkey_binding, 2);
EXFUN (Fkey_description, 1);
EXFUN (Fsingle_key_description, 2);
EXFUN (Fwhere_is_internal, 4);
extern Lisp_Object access_keymap P_ ((Lisp_Object, Lisp_Object, int, int, int));
extern Lisp_Object get_keyelt P_ ((Lisp_Object, int));
extern Lisp_Object get_keymap P_ ((Lisp_Object, int, int));
extern void describe_vector P_ ((Lisp_Object, Lisp_Object, Lisp_Object,
				 void (*) (Lisp_Object, Lisp_Object), int,
				 Lisp_Object, Lisp_Object, int *, int));
extern void describe_map_tree P_ ((Lisp_Object, int, Lisp_Object, Lisp_Object,
				   char *, int, int, int));
extern int current_minor_maps P_ ((Lisp_Object **, Lisp_Object **));
extern void initial_define_key P_ ((Lisp_Object, int, char *));
extern void initial_define_lispy_key P_ ((Lisp_Object, char *, char *));
extern void syms_of_keymap P_ ((void));
extern void keys_of_keymap P_ ((void));


#endif
