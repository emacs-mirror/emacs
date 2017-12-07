/* Header for fontset handler.
   Copyright (C) 1998, 2001-2017 Free Software Foundation, Inc.
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
     2005, 2006, 2007, 2008, 2009, 2010, 2011
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H14PRO021
   Copyright (C) 2003, 2006
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H13PRO009

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

#ifndef EMACS_FONTSET_H
#define EMACS_FONTSET_H

#include "lisp.h"

struct face;

extern void free_face_fontset (struct frame *, struct face *);
extern int face_for_char (struct frame *, struct face *, int,
                          ptrdiff_t, Lisp_Object);
extern Lisp_Object font_for_char (struct face *, int, ptrdiff_t, Lisp_Object);

extern int make_fontset_for_ascii_face (struct frame *, int, struct face *);
extern int fontset_from_font (Lisp_Object);
extern int fs_query_fontset (Lisp_Object, int);
extern Lisp_Object list_fontsets (struct frame *, Lisp_Object, int);

extern Lisp_Object fontset_name (int);
extern Lisp_Object fontset_ascii (int);

extern int face_for_font (struct frame *, Lisp_Object, struct face *);

#endif /* EMACS_FONTSET_H */
