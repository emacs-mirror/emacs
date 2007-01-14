/* Definitions for interface to indent.c
   Copyright (C) 1985, 1986, 2001, 2002, 2003, 2004,
                 2005, 2006, 2007  Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* We introduce new member `tab_offset'.  We need it because of the
   existence of wide-column characters.  There is a case that the
   line-break occurs at a wide-column character and the number of
   colums of the line gets less than width.

   Example (where W_ stands for a wide-column character):
	     ----------
	     abcdefgh\\
	     W_
	     ----------

   To handle this case, we should not calculate the tab offset by
  	tab_offset += width;

   Instead, we must remember tab_offset of the line.

 */

struct position
  {
    int bufpos;
    int bytepos;
    int hpos;
    int vpos;
    int prevhpos;
    int contin;
    /* Number of characters we have already handled
       from the before and after strings at this position.  */
    int ovstring_chars_done;
    int tab_offset;
  };

struct position *compute_motion P_ ((int, int, int, int, int, int, int,
				     int, int, int, struct window *));
struct position *vmotion P_ ((int, int, struct window *));
int skip_invisible P_ ((int, int *, int, Lisp_Object));

/* Value of point when current_column was called */
extern int last_known_column_point;

/* Functions for dealing with the column cache.  */

/* Return true iff the display table DISPTAB specifies the same widths
   for characters as WIDTHTAB.  We use this to decide when to
   invalidate the buffer's column_cache.  */
int disptab_matches_widthtab P_ ((struct Lisp_Char_Table *disptab,
				  struct Lisp_Vector *widthtab));

/* Recompute BUF's width table, using the display table DISPTAB.  */
void recompute_width_table P_ ((struct buffer *buf,
				struct Lisp_Char_Table *disptab));


/* arch-tag: f9feb373-5bff-4f4f-9198-94805d00cfd7
   (do not change this comment) */
