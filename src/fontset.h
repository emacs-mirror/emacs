/* Header for fontset handler.
   Copyright (C) 1995, 1997 Electrotechnical Laboratory, JAPAN.
   Licensed to the Free Software Foundation.

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

#ifndef _FONTSET_H
#define _FONTSET_H

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

  /* Maximum bound width over all existing characters of the font.  On
     X window, this is same as (font->max_bounds.width) */
  int size;

  /* Height of the font.  On X window, this is same as (font->ascent
     + font->descent).  */
  int height;

  /* Encodings of the font indexed by CHARSET.  The value an integer
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
     encoding information decided by the font itself.  */
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

/* A value which may appear in the member encoding of struch font_info
   indicating that a font itself doesn't tell which encoding to be
   used.  */
#define FONT_ENCODING_NOT_DECIDED 255

#define FONT_NOT_OPENED -1
#define FONT_NOT_FOUND  -2

struct fontset_info
{
  /* Name of the fontset.  */
  char *name;

  /* Size of the fontset.  This is the same as the size of ASCII font
     of this fontset.  */
  int size;

  /* Height of the tallest font in the fontset.  */
  int height;

  /* Table of font name for each character set.  */
  char *fontname[MAX_CHARSET + 1];

  /* Table of index numbers of fonts indexed by charset.  If a font is
     not yet loaded, the value is -1 (FONT_NOT_OPENED).  If font
     loading is failed, the value is -2 (FONT_NOT_FOUND).  */
  int font_indexes[MAX_CHARSET + 1];
};

/* This data type is used for the fontset_data field of struct frame.  */

struct fontset_data
{
  /* A table of pointers to all the fontsets.  */
  struct fontset_info **fontset_table;

  /* The current capacity of fontset_table.  */
  int fontset_table_size;

  /* The number of fontsets actually stored in fontset_table.
     fontset_table[n] is used and valid iff 0 <= n < n_fontsets.
     0 <= n_fontsets <= fontset_table_size.  */
  int n_fontsets;
};

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

extern struct fontset_data *alloc_fontset_data P_ ((void));
extern void free_fontset_data P_ ((struct fontset_data *));
extern struct font_info *fs_load_font P_ ((struct frame *, struct font_info *,
					   int, char *, int));
extern int fs_query_fontset P_ ((struct frame *, char *));
extern int fs_register_fontset P_ ((struct frame *, Lisp_Object));
EXFUN (Fquery_fontset, 2);
extern Lisp_Object list_fontsets P_ ((struct frame *, Lisp_Object, int));
extern Lisp_Object Vglobal_fontset_alist;

extern Lisp_Object Qfontset;
extern Lisp_Object Vuse_default_ascent; 
extern Lisp_Object Vignore_relative_composition;
extern Lisp_Object Valternate_fontname_alist;
extern Lisp_Object Vhighlight_wrong_size_font;
extern Lisp_Object Vclip_large_size_font;

extern int font_idx_temp;

/* Load a font named FONTNAME for displaying CHARSET on frame F.
   All fonts for frame F is stored in a table pointed by FONT_TABLE.
   Return a pointer to the struct font_info of the loaded font.
   If loading fails, return 0;
   If FONTNAME is NULL, the name is taken from the information of FONTSET.
   If FONTSET is given, try to load a font whose size matches that of
   FONTSET, and, the font index is stored in the table for FONTSET.  */

#define FS_LOAD_FONT(f, font_table, charset, fontname, fontset)		  \
  (fontset >= 0 && fontset < FRAME_FONTSET_DATA (f)->n_fontsets		  \
   && (font_idx_temp = (FRAME_FONTSET_DATA (f)				  \
			->fontset_table[fontset]->font_indexes[charset]), \
       font_idx_temp >= 0)						  \
   ? font_table + font_idx_temp						  \
   : fs_load_font (f, font_table, charset, fontname, fontset))

#endif /* _FONTSET_H */
