/* Header for fontset handler.
   Copyright (C) 1998, 2001, 2002, 2003, 2004, 2005,
                 2006, 2007, 2008  Free Software Foundation, Inc.
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
     2005, 2006, 2007, 2008
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H14PRO021
   Copyright (C) 2003, 2006
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H13PRO009

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#ifndef EMACS_FONTSET_H
#define EMACS_FONTSET_H

/* Check if any window system is used now.  */
extern void (*check_window_system_func) P_ ((void));

struct face;

extern void free_face_fontset P_ ((FRAME_PTR, struct face *));
extern Lisp_Object fontset_font_pattern P_ ((FRAME_PTR, struct face *, int));
extern int face_suitable_for_char_p P_ ((struct face *, int));
extern int face_for_char P_ ((FRAME_PTR, struct face *, int,
			      int, Lisp_Object));
extern int make_fontset_for_ascii_face P_ ((FRAME_PTR, int, struct face *));
extern int fontset_from_font_name P_ ((Lisp_Object));
extern void set_default_ascii_font P_ ((Lisp_Object));
extern int fs_query_fontset P_ ((Lisp_Object, int));
EXFUN (Fquery_fontset, 2);
extern Lisp_Object list_fontsets P_ ((struct frame *, Lisp_Object, int));

extern Lisp_Object Vuse_default_ascent;
extern Lisp_Object Vignore_relative_composition;
extern Lisp_Object Valternate_fontname_alist;
extern Lisp_Object Vfontset_alias_alist;
extern Lisp_Object Vvertical_centering_font_regexp;
extern Lisp_Object Votf_script_alist;

extern Lisp_Object fontset_name P_ ((int));
extern Lisp_Object fontset_ascii P_ ((int));
extern int fontset_height P_ ((int));

struct font;
extern int face_for_font P_ ((struct frame *, Lisp_Object, struct face *));
extern int new_fontset_from_font P_ ((Lisp_Object));

#endif /* EMACS_FONTSET_H */

/* arch-tag: c27cef7b-3cab-488a-8398-7a4daa96bb77
   (do not change this comment) */
