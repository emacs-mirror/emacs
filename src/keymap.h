/* Functions to manipulate keymaps.
   Copyright (C) 2001-2018 Free Software Foundation, Inc.

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

#ifndef KEYMAP_H
#define KEYMAP_H

#include "lisp.h"

/* The maximum byte size consumed by push_key_description.
   All callers should assure that at least this size of memory is
   allocated at the place pointed by the second argument.

   There are 6 modifiers, each consumes 2 chars.
   The octal form of a character code consumes
   (1 + CHARACTERBITS / 3 + 1) chars (including backslash at the head).
   We need one more byte for string terminator `\0'.  */
#define KEY_DESCRIPTION_SIZE ((2 * 6) + 1 + (CHARACTERBITS / 3) + 1 + 1)

#define KEYMAPP(m) (!NILP (get_keymap (m, false, false)))
extern Lisp_Object current_global_map;
extern char *push_key_description (EMACS_INT, char *);
extern Lisp_Object access_keymap (Lisp_Object, Lisp_Object, bool, bool, bool);
extern Lisp_Object get_keymap (Lisp_Object, bool, bool);
extern void describe_map_tree (Lisp_Object, bool, Lisp_Object, Lisp_Object,
			       const char *, bool, bool, bool, bool);
extern ptrdiff_t current_minor_maps (Lisp_Object **, Lisp_Object **);
extern void initial_define_key (Lisp_Object, int, const char *);
extern void initial_define_lispy_key (Lisp_Object, const char *, const char *);
extern void syms_of_keymap (void);
extern void keys_of_keymap (void);

typedef void (*map_keymap_function_t)
     (Lisp_Object key, Lisp_Object val, Lisp_Object args, void *data);
extern void map_keymap (Lisp_Object, map_keymap_function_t, Lisp_Object,
			void *, bool);
extern void map_keymap_canonical (Lisp_Object map,
				  map_keymap_function_t fun,
				  Lisp_Object args, void *data);

#endif
