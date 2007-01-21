/* Header for fontset handler.
   Copyright (C) 1998, 2001, 2002, 2003, 2004, 2005,
                 2006, 2007  Free Software Foundation, Inc.
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
     2005, 2006, 2007
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H14PRO021

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

#ifndef EMACS_FONTSET_H
#define EMACS_FONTSET_H

/* This data type is used for the font_table field of window system
   depending data area (e.g. struct x_display_info on X window).  */

struct font_info
{
  /* Pointer to window system dependent font structure.  On X window,
     this value should be coerced to (XFontStruct *).  */
  void *font;

  /* Index number of the font.  */
  int font_idx;

  /* Name to be used to find the font.  */
  char *name;

  /* Full name of the font given by a window system.  */
  char *full_name;

  /* Charset of characters displayed by the font.  */
  int charset;

#ifdef WINDOWSNT
  /* Codepage of characters that will be displayed by the font.  */
  int codepage;
#endif

  /* Maximum bound width over all existing characters of the font.  On
     X window, this is same as (font->max_bounds.width) */
  int size;

  /* Height of the font.  On X window, this is the same as
     (font->ascent + font->descent).  */
  int height;

  /* Width of the space glyph of the font.  */
  int space_width;

  /* Average width of glyphs in the font.  */
  int average_width;

  /* 1 iff `vertical-centering-font-regexp' matches this font name.
     In this case, we render characters at vartical center positions
     of lines.  */
  int vertical_centering;

  /* Encodings of the font indexed by CHARSET.  The value is one of
     0, 1, 2, or 3:
	0: code points 0x20..0x7F or 0x2020..0x7F7F are used
	1: code points 0xA0..0xFF or 0xA0A0..0xFFFF are used
	2: code points 0x20A0..0x7FFF are used
	3: code points 0xA020..0xFF7F are used
     For instance, ASCII and Latin-1 characters may use the same font
     but different code points (ASCII uses 0x20..0x7F and Latin-1 uses
     0xA0..0xFF).

     If the value can't be decided from information of the font, we
     consult `font-encoding-alist' to get of the corresponding charset
     whose default value is defined in lisp/fontset.el.  Since there's
     no charset whose id is 1, we use encoding[1] to store the
     encoding information decided by the font itself.

     If the member `font_encoder' is not NULL, this member is ignored.
  */
  unsigned char encoding[MAX_CHARSET + 1];

  /* The baseline position of a font is normally `ascent' value of the
     font.  However, there exists many fonts which don't set `ascent'
     an appropriate value to be used as baseline position.  This is
     typical in such ASCII fonts which are designed to be used with
     Chinese, Japanese, Korean characters.  When we use mixture of
     such fonts and normal fonts (having correct `ascent' value), a
     display line gets very ugly.  Since we have no way to fix it
     automatically, it is users responsibility to supply well designed
     fonts or correct `ascent' value of fonts.  But, the latter
     requires heavy work (modifying all bitmap data in BDF files).
     So, Emacs accepts a private font property
     `_MULE_BASELINE_OFFSET'.  If a font has this property, we
     calculate the baseline position by subtracting the value from
     `ascent'.  In other words, the value indicates how many bits
     higher we should draw a character of the font than normal ASCII
     text for a better looking.

     We also have to consider the fact that the concept of `baseline'
     differs among languages to which each character belongs.  For
     instance, baseline should be at the bottom most position of all
     glyphs for Chinese, Japanese, and Korean.  But, many of existing
     fonts for those characters doesn't have correct `ascent' values
     because they are designed to be used with ASCII fonts.  To
     display characters of different language on the same line, the
     best way will be to arrange them in the middle of the line.  So,
     in such a case, again, we utilize the font property
     `_MULE_BASELINE_OFFSET'.  If the value is larger than `ascent' we
     calculate baseline so that a character is arranged in the middle
     of a line.  */

  int baseline_offset;

  /* Non zero means a character should be composed at a position
     relative to the height (or depth) of previous glyphs in the
     following cases:
	(1) The bottom of the character is higher than this value.  In
	this case, the character is drawn above the previous glyphs.
	(2) The top of the character is lower than 0 (i.e. baseline
	height).  In this case, the character is drawn beneath the
	previous glyphs.

     This value is taken from a private font property
     `_MULE_RELATIVE_COMPOSE' which is introduced by Emacs.  */
  int relative_compose;

  /* Non zero means an ascent value to be used for a character
     registered in char-table `use-default-ascent'.  */
  int default_ascent;

  /* CCL program to calculate code points of the font.  */
  struct ccl_program *font_encoder;
};

/* A value which may appear in the member `encoding' of struch
   font_info indicating that a font itself doesn't tell which encoding
   to be used.  */
#define FONT_ENCODING_NOT_DECIDED 255

/* Forward declaration for prototypes.  */
struct frame;

/* The following six are window system dependent functions.
   Initialization routine of each window system should set appropriate
   functions to these variables.  For instance, in case of X window,
   x_term_init does this.  */

/* Return a pointer to struct font_info of font FONT_IDX of frame F.  */
extern struct font_info *(*get_font_info_func) P_ ((struct frame *f,
						    int font_idx));

/* Return a list of font names which matches PATTERN.  See the document of
   `x-list-fonts' for more detail.  */
extern Lisp_Object (*list_fonts_func) P_ ((struct frame *f,
					   Lisp_Object pattern,
					   int size,
					   int maxnames));

/* Load a font named NAME for frame F and return a pointer to the
   information of the loaded font.  If loading is failed, return -1.  */
extern struct font_info *(*load_font_func) P_ ((struct frame *f,
						char *name, int));

/* Return a pointer to struct font_info of a font named NAME for frame F.
   If no such font is loaded, return NULL.  */
extern struct font_info *(*query_font_func) P_ ((struct frame *f, char *name));

/* Additional function for setting fontset or changing fontset
   contents of frame F.  This function may change the coordinate of
   the frame.  */
extern void (*set_frame_fontset_func) P_ ((struct frame *f, Lisp_Object arg,
					   Lisp_Object oldval));

/* To find a CCL program, fs_load_font calls this function.
   The argument is a pointer to the struct font_info.
   This function set the memer `encoder' of the structure.  */
extern void (*find_ccl_program_func) P_ ((struct font_info *));

/* Check if any window system is used now.  */
extern void (*check_window_system_func) P_ ((void));

struct face;

extern void free_face_fontset P_ ((FRAME_PTR, struct face *));
extern Lisp_Object fontset_font_pattern P_ ((FRAME_PTR, int, int));
extern int face_suitable_for_char_p P_ ((struct face *, int));
extern int face_for_char P_ ((FRAME_PTR, struct face *, int));
extern int make_fontset_for_ascii_face P_ ((FRAME_PTR, int));
extern void set_default_ascii_font P_ ((Lisp_Object));
extern struct font_info *fs_load_font P_ ((struct frame *, int, char *, int,
					   struct face *));
extern int fs_query_fontset P_ ((Lisp_Object, int));
EXFUN (Fquery_fontset, 2);
extern Lisp_Object list_fontsets P_ ((struct frame *, Lisp_Object, int));

extern Lisp_Object Qfontset;
extern Lisp_Object Vuse_default_ascent;
extern Lisp_Object Vignore_relative_composition;
extern Lisp_Object Valternate_fontname_alist;
extern Lisp_Object Vfontset_alias_alist;
extern Lisp_Object Vvertical_centering_font_regexp;

/* Load a font named FONTNAME for displaying character C.  All fonts
   for frame F is stored in a table pointed by FONT_TABLE.  Return a
   pointer to the struct font_info of the loaded font.  If loading
   fails, return 0; If FONTNAME is NULL, the name is taken from the
   information of FONTSET.  If FONTSET is given, try to load a font
   whose size matches that of FONTSET, and, the font index is stored
   in the table for FONTSET.  */

#define FS_LOAD_FONT(f, c, fontname, fontset)  \
  fs_load_font (f, c, fontname, fontset, NULL)

#define FS_LOAD_FACE_FONT(f, c, fontname, face) \
  fs_load_font (f, c, fontname, -1, face)

/* Return an immutable id for font_info FONT_INFO on frame F.  The
   reason for this macro is hat one cannot hold pointers to font_info
   structures in other data structures, because the table is
   reallocated in x_list_fonts.  */

#define FONT_INFO_ID(F, FONT_INFO) \
     (FONT_INFO) - (FRAME_X_DISPLAY_INFO ((F))->font_table)

/* Given a font_info id ID, return a pointer to the font_info
   structure on frame F.  If ID is invalid, return null.  */

#define FONT_INFO_FROM_ID(F, ID)					\
     (((ID) >= 0 && (ID) < FRAME_X_DISPLAY_INFO ((F))->font_table_size)	\
      ? (FRAME_X_DISPLAY_INFO ((F))->font_table + (ID))			\
      : 0)

extern Lisp_Object fontset_name P_ ((int));
extern Lisp_Object fontset_ascii P_ ((int));
extern int fontset_height P_ ((int));

#endif /* EMACS_FONTSET_H */

/* arch-tag: c27cef7b-3cab-488a-8398-7a4daa96bb77
   (do not change this comment) */
