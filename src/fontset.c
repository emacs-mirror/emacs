/* Fontset handler.
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

#include <config.h>
#if HAVE_ALLOCA_H
#include <alloca.h>
#endif /* HAVE_ALLOCA_H */ 
#include "lisp.h"
#include "charset.h"
#include "ccl.h"
#include "frame.h"
#include "fontset.h"

Lisp_Object Vglobal_fontset_alist;
Lisp_Object Vfont_encoding_alist;
Lisp_Object Vuse_default_ascent;
Lisp_Object Vignore_relative_composition;
Lisp_Object Valternate_fontname_alist;
Lisp_Object Vfontset_alias_alist;
Lisp_Object Vhighlight_wrong_size_font;
Lisp_Object Vclip_large_size_font;
Lisp_Object Vvertical_centering_font_regexp;

/* Used as a temporary in macro FS_LOAD_FONT.  */
int font_idx_temp;

/* We had better have our own strcasecmp function because some system
   doesn't have it.  */
static char my_strcasetbl[256];

/* Compare two strings S0 and S1 while ignoring differences in case.
   Return 1 if they differ, else return 0.  */
static int
my_strcasecmp (s0, s1)
     unsigned char *s0, *s1;
{
  while (*s0)
    if (my_strcasetbl[*s0++] != my_strcasetbl[*s1++]) return 1;
  return (int) *s1;
}

/* The following six are window system dependent functions.  See
   the comments in src/fontset.h for more detail.  */

/* Return a pointer to struct font_info of font FONT_IDX of frame F.  */
struct font_info *(*get_font_info_func) P_ ((FRAME_PTR f, int font_idx));

/* Return a list of font names which matches PATTERN.  See the document of
   `x-list-fonts' for more detail.  */
Lisp_Object (*list_fonts_func) P_ ((struct frame *f,
				    Lisp_Object pattern,
				    int size,
				    int maxnames));

/* Load a font named NAME for frame F and return a pointer to the
   information of the loaded font.  If loading is failed, return 0.  */
struct font_info *(*load_font_func) P_ ((FRAME_PTR f, char *name, int));

/* Return a pointer to struct font_info of a font named NAME for frame F.  */
struct font_info *(*query_font_func) P_ ((FRAME_PTR f, char *name));

/* Additional function for setting fontset or changing fontset
   contents of frame F.  */
void (*set_frame_fontset_func) P_ ((FRAME_PTR f, Lisp_Object arg,
				    Lisp_Object oldval));

/* To find a CCL program, fs_load_font calls this function.
   The argument is a pointer to the struct font_info.
   This function set the memer `encoder' of the structure.  */
void (*find_ccl_program_func) P_ ((struct font_info *));

/* Check if any window system is used now.  */
void (*check_window_system_func) P_ ((void));

struct fontset_data *
alloc_fontset_data ()
{
  struct fontset_data *fontset_data
    = (struct fontset_data *) xmalloc (sizeof (struct fontset_data));

  bzero (fontset_data, sizeof (struct fontset_data));

  return fontset_data;
}

void
free_fontset_data (fontset_data)
     struct fontset_data *fontset_data;
{
  if (fontset_data->fontset_table)
    {
      int i;

      for (i = 0; i < fontset_data->n_fontsets; i++)
	{
	  int j;
	  
	  xfree (fontset_data->fontset_table[i]->name);
	  for (j = 0; j <= MAX_CHARSET; j++)
	    if (fontset_data->fontset_table[i]->fontname[j])
	      xfree (fontset_data->fontset_table[i]->fontname[j]);
	  xfree (fontset_data->fontset_table[i]);
	}
      xfree (fontset_data->fontset_table);
    }

  xfree (fontset_data);
}

/* Load a font named FONTNAME for displaying CHARSET on frame F.
   All fonts for frame F is stored in a table pointed by FONT_TABLE.
   Return a pointer to the struct font_info of the loaded font.
   If loading fails, return 0;
   If FONTNAME is NULL, the name is taken from the information of FONTSET.
   If FONTSET is given, try to load a font whose size matches that of
   FONTSET, and, the font index is stored in the table for FONTSET.

   If you give FONTSET argument, don't call this function directry,
   instead call macro FS_LOAD_FONT with the same argument.  */

struct font_info *
fs_load_font (f, font_table, charset, fontname, fontset)
     FRAME_PTR f;
     struct font_info *font_table;
     int charset, fontset;
     char *fontname;
{
  Lisp_Object font_list;
  Lisp_Object list, elt;
  int font_idx;
  int size = 0;
  struct fontset_info *fontsetp = 0;
  struct font_info *fontp;

  if (fontset >= 0 && fontset < FRAME_FONTSET_DATA (f)->n_fontsets)
    {
      fontsetp = FRAME_FONTSET_DATA (f)->fontset_table[fontset];
      font_idx = fontsetp->font_indexes[charset];
      if (font_idx >= 0)
	/* We have already loaded a font.  */
	return font_table + font_idx;
      else if (font_idx == FONT_NOT_FOUND)
	/* We have already tried loading a font and failed.  */
	return 0;
      if (!fontname)
	fontname = fontsetp->fontname[charset];
    }

  if (!fontname)
    /* No way to get fontname.  */
    return 0;

  /* If CHARSET is not ASCII and FONTSET is specified, we must load a
     font of appropriate size to be used with other fonts in this
     fontset.  */
  if (charset != CHARSET_ASCII && fontsetp)
    {
      /* If we have not yet loaded ASCII font of FONTSET, we must load
	 it now to decided the size and height of this fontset.  */
      if (fontsetp->size == 0)
	{
	  fontp = fs_load_font (f, font_table, CHARSET_ASCII, 0, fontset);
	  if (!fontp)
	    /* Any fontset should contain available ASCII.  */
	    return 0;
	}
      /* Now we have surely decided the size of this fontset.  */
      size = fontsetp->size * CHARSET_WIDTH (charset);
    }

  fontp = (*load_font_func) (f, fontname, size);

  if (!fontp)
    {
      if (fontsetp)
	fontsetp->font_indexes[charset] = FONT_NOT_FOUND;
      return 0;
    }

  /* Fill in fields (charset, vertical_centering, encoding, and
     font_encoder) which are not set by (*load_font_func).  */
  fontp->charset = charset;

  fontp->vertical_centering
    = (STRINGP (Vvertical_centering_font_regexp)
       && (fast_c_string_match_ignore_case 
	   (Vvertical_centering_font_regexp, fontp->full_name) >= 0));

  if (fontp->encoding[1] != FONT_ENCODING_NOT_DECIDED)
    {
      /* The font itself tells which code points to be used.  Use this
	 encoding for all other charsets.  */
      int i;

      fontp->encoding[0] = fontp->encoding[1];
      for (i = MIN_CHARSET_OFFICIAL_DIMENSION1; i <= MAX_CHARSET; i++)
	fontp->encoding[i] = fontp->encoding[1];
    }
  else
    {
      /* The font itself doesn't tell which code points to be used.  */
      int i;

      /* At first, set 1 (means 0xA0..0xFF) as the default.  */
      fontp->encoding[0] = 1;
      for (i = MIN_CHARSET_OFFICIAL_DIMENSION1; i <= MAX_CHARSET; i++)
	fontp->encoding[i] = 1;
      /* Then override them by a specification in Vfont_encoding_alist.  */
      for (list = Vfont_encoding_alist; CONSP (list); list = XCDR (list))
	{
	  elt = XCAR (list);
	  if (CONSP (elt)
	      && STRINGP (XCAR (elt)) && CONSP (XCDR (elt))
	      && (fast_c_string_match_ignore_case (XCAR (elt), fontname)
		  >= 0))
	    {
	      Lisp_Object tmp;

	      for (tmp = XCDR (elt); CONSP (tmp); tmp = XCDR (tmp))
		if (CONSP (XCAR (tmp))
		    && ((i = get_charset_id (XCAR (XCAR (tmp))))
			>= 0)
		    && INTEGERP (XCDR (XCAR (tmp)))
		    && XFASTINT (XCDR (XCAR (tmp))) < 4)
		  fontp->encoding[i]
		    = XFASTINT (XCDR (XCAR (tmp)));
	    }
	}
    }

  fontp->font_encoder = (struct ccl_program *) 0;

  if (find_ccl_program_func)
    (*find_ccl_program_func) (fontp);

  /* If FONTSET is specified, setup various fields of it.  */
  if (fontsetp)
    {
      fontsetp->font_indexes[charset] = fontp->font_idx;
      if (charset == CHARSET_ASCII)
	{
	  /* Decide or change the size and height of this fontset.  */
	  if (fontsetp->size == 0)
	    {
	      fontsetp->size = fontp->size;
	      fontsetp->height = fontp->height;
	    }
	  else if (fontsetp->size != fontp->size
		   || fontsetp->height != fontp->height)
	    {
	      /* When loading ASCII font of the different size from
		 the size of FONTSET, we have to update the size of
		 FONTSET.  Since changing the size of FONTSET may make
		 some fonts already loaded inappropriate to be used in
		 FONTSET, we must delete the record of such fonts.  In
		 that case, we also have to calculate the height of
		 FONTSET from the remaining fonts.  */
	      int i;

	      fontsetp->size = fontp->size;
	      fontsetp->height = fontp->height;
	      for (i = CHARSET_ASCII + 1; i <= MAX_CHARSET; i++)
		{
		  font_idx = fontsetp->font_indexes[i];
		  if (font_idx >= 0)
		    {
		      struct font_info *fontp2 = font_table + font_idx;

		      if (fontp2->size != fontp->size * CHARSET_WIDTH (i))
			fontsetp->font_indexes[i] = FONT_NOT_OPENED;
		      /* The following code should be disabled until
			 Emacs supports variable height lines.  */
#if 0
		      else if (fontsetp->height < fontp->height)
			fontsetp->height = fontp->height;
#endif
		    }
		}
	    }
	}
    }

  return fontp;
}

/* Return ID of the fontset named NAME on frame F.  */

int
fs_query_fontset (f, name)
     FRAME_PTR f;
     char *name;
{
  struct fontset_data *fontset_data = FRAME_FONTSET_DATA (f);
  int i;

  for (i = 0; i < fontset_data->n_fontsets; i++)
    if (!my_strcasecmp(name, fontset_data->fontset_table[i]->name))
      return i;
  return -1;
}

/* Register a fontset specified by FONTSET_INFO for frame FRAME.
   Return the fontset ID if successfully registered, else return -1.
   FONTSET_INFO is a cons of name of the fontset and FONTLIST, where
   FONTLIST is an alist of charsets vs fontnames.  */

int
fs_register_fontset (f, fontset_info)
     FRAME_PTR f;
     Lisp_Object fontset_info;
{
  struct fontset_data *fontset_data = FRAME_FONTSET_DATA (f);
  Lisp_Object name, fontlist;
  int fontset;
  struct fontset_info *fontsetp;
  int i;

  if (!CONSP (fontset_info)
      || !STRINGP (XCAR (fontset_info))
      || !CONSP (XCDR (fontset_info)))
    /* Invalid data in FONTSET_INFO.  */
    return -1;

  name = XCAR (fontset_info);
  if ((fontset = fs_query_fontset (f, XSTRING (name)->data)) >= 0)
    /* This fontset already exists on frame F.  */
    return fontset;

  fontsetp = (struct fontset_info *) xmalloc (sizeof (struct fontset_info));

  fontsetp->name = (char *) xmalloc (XSTRING (name)->size + 1);
  bcopy(XSTRING (name)->data, fontsetp->name, XSTRING (name)->size + 1);

  fontsetp->size = fontsetp->height = 0;

  for (i = 0; i <= MAX_CHARSET; i++)
    {
      fontsetp->fontname[i] = (char *) 0;
      fontsetp->font_indexes[i] = FONT_NOT_OPENED;
    }

  for (fontlist = XCDR (fontset_info); CONSP (fontlist);
       fontlist = XCDR (fontlist))
    {
      Lisp_Object tem = Fcar (fontlist);
      int charset;

      if (CONSP (tem)
	  && (charset = get_charset_id (XCAR (tem))) >= 0
	  && STRINGP (XCDR (tem)))
	{
	  fontsetp->fontname[charset]
	     = (char *) xmalloc (XSTRING (XCDR (tem))->size + 1);
	  bcopy (XSTRING (XCDR (tem))->data,
		 fontsetp->fontname[charset],
		 XSTRING (XCDR (tem))->size + 1);
	}
      else
	/* Broken or invalid data structure.  */
	return -1;
    }

  /* Do we need to create the table?  */
  if (fontset_data->fontset_table_size == 0)
    {
      fontset_data->fontset_table_size = 8;
      fontset_data->fontset_table
	= (struct fontset_info **) xmalloc (fontset_data->fontset_table_size
					    * sizeof (struct fontset_info *));
    }
  /* Do we need to grow the table?  */
  else if (fontset_data->n_fontsets >= fontset_data->fontset_table_size)
    {
      fontset_data->fontset_table_size += 8;
      fontset_data->fontset_table
	= (struct fontset_info **) xrealloc (fontset_data->fontset_table,
					     fontset_data->fontset_table_size
					     * sizeof (struct fontset_info *));
    }
  fontset = fontset_data->n_fontsets++;
  fontset_data->fontset_table[fontset] = fontsetp;

  return fontset;
}

/* Cache data used by fontset_pattern_regexp.  The car part is a
   pattern string containing at least one wild card, the cdr part is
   the corresponding regular expression.  */
static Lisp_Object Vcached_fontset_data;

#define CACHED_FONTSET_NAME (XSTRING (XCAR (Vcached_fontset_data))->data)
#define CACHED_FONTSET_REGEX (XCDR (Vcached_fontset_data))

/* If fontset name PATTERN contains any wild card, return regular
   expression corresponding to PATTERN.  */

Lisp_Object
fontset_pattern_regexp (pattern)
     Lisp_Object pattern;
{
  if (!index (XSTRING (pattern)->data, '*')
      && !index (XSTRING (pattern)->data, '?'))
    /* PATTERN does not contain any wild cards.  */
    return Qnil;

  if (!CONSP (Vcached_fontset_data)
      || strcmp (XSTRING (pattern)->data, CACHED_FONTSET_NAME))
    {
      /* We must at first update the cached data.  */
      char *regex = (char *) alloca (XSTRING (pattern)->size * 2);
      char *p0, *p1 = regex;

      /* Convert "*" to ".*", "?" to ".".  */
      *p1++ = '^';
      for (p0 = (char *) XSTRING (pattern)->data; *p0; p0++)
	{
	  if (*p0 == '*')
	    {
	      *p1++ = '.';
	      *p1++ = '*';
	    }
	  else if (*p0 == '?')
	    *p1++ = '.';
	  else
	    *p1++ = *p0;
	}
      *p1++ = '$';
      *p1++ = 0;

      Vcached_fontset_data = Fcons (build_string (XSTRING (pattern)->data),
				    build_string (regex));
    }

  return CACHED_FONTSET_REGEX;
}

DEFUN ("query-fontset", Fquery_fontset, Squery_fontset, 1, 2, 0,
  "Return the name of an existing fontset which matches PATTERN.\n\
The value is nil if there is no matching fontset.\n\
PATTERN can contain `*' or `?' as a wildcard\n\
just as X font name matching algorithm allows.\n\
If REGEXPP is non-nil, PATTERN is a regular expression.")
  (pattern, regexpp)
     Lisp_Object pattern, regexpp;
{
  Lisp_Object regexp, tem;

  (*check_window_system_func) ();

  CHECK_STRING (pattern, 0);

  if (XSTRING (pattern)->size == 0)
    return Qnil;

  tem = Frassoc (pattern, Vfontset_alias_alist);
  if (!NILP (tem))
    return Fcar (tem);

  if (NILP (regexpp))
    regexp = fontset_pattern_regexp (pattern);
  else
    regexp = pattern;

  for (tem = Vglobal_fontset_alist; CONSP (tem); tem = XCDR (tem))
    {
      Lisp_Object fontset_name = XCAR (XCAR (tem));
      if (!NILP (regexp))
	{
	  if (fast_c_string_match_ignore_case (regexp,
					       XSTRING (fontset_name)->data)
	      >= 0)
	    return fontset_name;
	}
      else
	{
	  if (!my_strcasecmp (XSTRING (pattern)->data,
			      XSTRING (fontset_name)->data))
	    return fontset_name;
	}
    }

  return Qnil;
}

/* Return a list of names of available fontsets matching PATTERN on
   frame F.  If SIZE is not 0, it is the size (maximum bound width) of
   fontsets to be listed. */

Lisp_Object
list_fontsets (f, pattern, size)
     FRAME_PTR f;
     Lisp_Object pattern;
     int size;
{
  int i;
  Lisp_Object regexp, val;

  regexp = fontset_pattern_regexp (pattern);

  val = Qnil;
  for (i = 0; i < FRAME_FONTSET_DATA (f)->n_fontsets; i++)
    {
      struct fontset_info *fontsetp = FRAME_FONTSET_DATA (f)->fontset_table[i];
      int name_matched = 0;
      int size_matched = 0;

      if (!NILP (regexp))
	{
	  if (fast_c_string_match_ignore_case (regexp, fontsetp->name) >= 0)
	    name_matched = 1;
	}
      else
	{
	  if (!my_strcasecmp (XSTRING (pattern)->data, fontsetp->name))
	    name_matched = 1;	  
	}

      if (name_matched)
	{
	  if (!size || fontsetp->size == size)
	    size_matched = 1;
	  else if (fontsetp->size == 0)
	    {
	      /* No font of this fontset has loaded yet.  Try loading
		 one with SIZE.  */
	      int j;

	      for (j = 0; j <= MAX_CHARSET; j++)
		if (fontsetp->fontname[j])
		  {
		    if ((*load_font_func) (f, fontsetp->fontname[j], size))
		      size_matched = 1;
		    break;
		  }
	    }

	  if (size_matched)
	    val = Fcons (build_string (fontsetp->name), val);
	}
    }

  return val;
}

DEFUN ("new-fontset", Fnew_fontset, Snew_fontset, 2, 2, 0,
  "Create a new fontset NAME which contains fonts in FONTLIST.\n\
FONTLIST is an alist of charsets vs corresponding font names.")
  (name, fontlist)
     Lisp_Object name, fontlist;
{
  Lisp_Object fullname, fontset_info;
  Lisp_Object tail;

  (*check_window_system_func) ();

  CHECK_STRING (name, 0);
  CHECK_LIST (fontlist, 1);

  fullname = Fquery_fontset (name, Qnil);
  if (!NILP (fullname))
    error ("Fontset `%s' matches the existing fontset `%s'",
	   XSTRING (name)->data, XSTRING (fullname)->data);

  /* Check the validity of FONTLIST.  */
  for (tail = fontlist; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object tem = XCAR (tail);
      int charset;

      if (!CONSP (tem)
	  || (charset = get_charset_id (XCAR (tem))) < 0
	  || !STRINGP (XCDR (tem)))
	error ("Elements of fontlist must be a cons of charset and font name");
    }

  fontset_info = Fcons (name, fontlist);
  Vglobal_fontset_alist  = Fcons (fontset_info, Vglobal_fontset_alist);

  /* Register this fontset for all existing frames.  */
  {
    Lisp_Object framelist, frame;
    
    FOR_EACH_FRAME (framelist, frame)
      if (!FRAME_TERMCAP_P (XFRAME (frame)))
	fs_register_fontset (XFRAME (frame), fontset_info);
  }

  return Qnil;
}

extern Lisp_Object Qfont;
Lisp_Object Qfontset;

DEFUN ("set-fontset-font", Fset_fontset_font, Sset_fontset_font, 3, 4, 0,
  "Set FONTNAME for a font of CHARSET in fontset NAME on frame FRAME.\n\
If FRAME is omitted or nil, all frames are affected.")
  (name, charset_symbol, fontname, frame)
     Lisp_Object name, charset_symbol, fontname, frame;
{
  int charset;
  Lisp_Object fullname, fontlist;

  (*check_window_system_func) ();

  CHECK_STRING (name, 0);
  CHECK_SYMBOL (charset_symbol, 1);
  CHECK_STRING (fontname, 2);
  if (!NILP (frame))
    CHECK_LIVE_FRAME (frame, 3);

  if ((charset = get_charset_id (charset_symbol)) < 0)
    error ("Invalid charset: %s", XSYMBOL (charset_symbol)->name->data);

  fullname = Fquery_fontset (name, Qnil);
  if (NILP (fullname))
    error ("Fontset `%s' does not exist", XSTRING (name)->data);

  /* If FRAME is not specified, we must, at first, update contents of
     `global-fontset-alist' for a frame created in the future.  */
  if (NILP (frame))
    {
      Lisp_Object fontset_info = Fassoc (fullname, Vglobal_fontset_alist);
      Lisp_Object tem = Fassq (charset_symbol, XCDR (fontset_info));

      if (NILP (tem))
	XCDR (fontset_info)
	  = Fcons (Fcons (charset_symbol, fontname),
		   XCDR (fontset_info));
      else
	XCDR (tem) = fontname;
    }

  /* Then, update information in the specified frame or all existing
     frames.  */
  {
    Lisp_Object framelist, tem;

    FOR_EACH_FRAME (framelist, tem)
      if (!FRAME_TERMCAP_P (XFRAME (tem))
	  && (NILP (frame) || EQ (frame, tem)))
	{
	  FRAME_PTR f = XFRAME (tem);
	  int fontset = fs_query_fontset (f, XSTRING (fullname)->data);
	  struct fontset_info *fontsetp
	    = FRAME_FONTSET_DATA (f)->fontset_table[fontset];

	  if (fontsetp->fontname[charset])
	    xfree (fontsetp->fontname[charset]);
	  fontsetp->fontname[charset]
	    = (char *) xmalloc (XSTRING (fontname)->size + 1);
	  bcopy (XSTRING (fontname)->data, fontsetp->fontname[charset],
		 XSTRING (fontname)->size + 1);
	  fontsetp->font_indexes[charset] = FONT_NOT_OPENED;

	  if (charset == CHARSET_ASCII)
	    {
	      Lisp_Object font_param = Fassq (Qfont, Fframe_parameters (tem));

	      if (set_frame_fontset_func
		  && !NILP (font_param)
		  && !strcmp (XSTRING (fullname)->data,
			      XSTRING (XCDR (font_param))->data))
		/* This fontset is the default fontset on frame TEM.
		   We may have to resize this frame because of new
		   ASCII font.  */
		(*set_frame_fontset_func) (f, fullname, Qnil);
	    }
	}
  }

  return Qnil;
}

DEFUN ("font-info", Ffont_info, Sfont_info, 1, 2, 0,
  "Return information about a font named NAME on frame FRAME.\n\
If FRAME is omitted or nil, use the selected frame.\n\
The returned value is a vector of OPENED-NAME, FULL-NAME, CHARSET, SIZE,\n\
  HEIGHT, BASELINE-OFFSET, RELATIVE-COMPOSE, and DEFAULT-ASCENT,\n\
where\n\
  OPENED-NAME is the name used for opening the font,\n\
  FULL-NAME is the full name of the font,\n\
  CHARSET is the charset displayed by the font,\n\
  SIZE is the minimum bound width of the font,\n\
  HEIGHT is the height of the font,\n\
  BASELINE-OFFSET is the upward offset pixels from ASCII baseline,\n\
  RELATIVE-COMPOSE and DEFAULT-ASCENT are the numbers controlling\n\
    how to compose characters.\n\
If the named font is not yet loaded, return nil.")
  (name, frame)
     Lisp_Object name, frame;
{
  FRAME_PTR f;
  struct font_info *fontp;
  Lisp_Object info;

  (*check_window_system_func) ();

  CHECK_STRING (name, 0);
  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame, 1);
  f = XFRAME (frame);

  if (!query_font_func)
    error ("Font query function is not supported");

  fontp = (*query_font_func) (f, XSTRING (name)->data);
  if (!fontp)
    return Qnil;

  info = Fmake_vector (make_number (8), Qnil);

  XVECTOR (info)->contents[0] = build_string (fontp->name);
  XVECTOR (info)->contents[1] = build_string (fontp->full_name);
  XVECTOR (info)->contents[2] = CHARSET_SYMBOL (fontp->charset);
  XVECTOR (info)->contents[3] = make_number (fontp->size);
  XVECTOR (info)->contents[4] = make_number (fontp->height);
  XVECTOR (info)->contents[5] = make_number (fontp->baseline_offset);
  XVECTOR (info)->contents[6] = make_number (fontp->relative_compose);
  XVECTOR (info)->contents[7] = make_number (fontp->default_ascent);

  return info;
}

DEFUN ("fontset-info", Ffontset_info, Sfontset_info, 1, 2, 0,
  "Return information about a fontset named NAME on frame FRAME.\n\
If FRAME is omitted or nil, use the selected frame.\n\
The returned value is a vector of SIZE, HEIGHT, and FONT-LIST,\n\
where\n\
  SIZE is the minimum bound width of ASCII font of the fontset,\n\
  HEIGHT is the height of the tallest font in the fontset, and\n\
  FONT-LIST is an alist of the format:\n\
    (CHARSET REQUESTED-FONT-NAME LOADED-FONT-NAME).\n\
LOADED-FONT-NAME t means the font is not yet loaded, nil means the\n\
loading failed.")
  (name, frame)
     Lisp_Object name, frame;
{
  FRAME_PTR f;
  int fontset;
  struct fontset_info *fontsetp;
  Lisp_Object info, val;
  int i;
  
  (*check_window_system_func) ();

  CHECK_STRING(name, 0);
  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame, 1);
  f = XFRAME (frame);

  fontset = fs_query_fontset (f, XSTRING (name)->data);
  if (fontset < 0)
    error ("Fontset `%s' does not exist", XSTRING (name)->data);

  info = Fmake_vector (make_number (3), Qnil);

  fontsetp = FRAME_FONTSET_DATA (f)->fontset_table[fontset];

  XVECTOR (info)->contents[0] = make_number (fontsetp->size);
  XVECTOR (info)->contents[1] = make_number (fontsetp->height);
  val = Qnil;
  for (i = 0; i <= MAX_CHARSET; i++)
    if (fontsetp->fontname[i])
      {
	int font_idx = fontsetp->font_indexes[i];
	Lisp_Object loaded;

	if (font_idx == FONT_NOT_OPENED)
	  loaded = Qt;
	else if (font_idx == FONT_NOT_FOUND)
	  loaded = Qnil;
	else
	  loaded
	    = build_string ((*get_font_info_func) (f, font_idx)->full_name);
	val = Fcons (Fcons (CHARSET_SYMBOL (i),
			    Fcons (build_string (fontsetp->fontname[i]),
				   Fcons (loaded, Qnil))),
		     val);
      }
  XVECTOR (info)->contents[2] = val;
  return info;
}

void
syms_of_fontset ()
{
  int i;

  for (i = 0; i < 256; i++)
    my_strcasetbl[i] = (i >= 'A' && i <= 'Z') ? i + 'a' - 'A' : i;

  if (!load_font_func)
    /* Window system initializer should have set proper functions.  */
    abort ();

  Qfontset = intern ("fontset");
  staticpro (&Qfontset);

  Vcached_fontset_data = Qnil;
  staticpro (&Vcached_fontset_data);

  DEFVAR_LISP ("global-fontset-alist", &Vglobal_fontset_alist,
    "Internal data for fontset.  Not for external use.\n\
This is an alist associating fontset names with the lists of fonts\n\
 contained in them.\n\
Newly created frames make their own fontset database from here.");
  Vglobal_fontset_alist = Qnil;

  DEFVAR_LISP ("font-encoding-alist", &Vfont_encoding_alist,
    "Alist of fontname patterns vs corresponding encoding info.\n\
Each element looks like (REGEXP . ENCODING-INFO),\n\
 where ENCODING-INFO is an alist of CHARSET vs ENCODING.\n\
ENCODING is one of the following integer values:\n\
	0: code points 0x20..0x7F or 0x2020..0x7F7F are used,\n\
	1: code points 0xA0..0xFF or 0xA0A0..0xFFFF are used,\n\
	2: code points 0x20A0..0x7FFF are used,\n\
	3: code points 0xA020..0xFF7F are used.");
  Vfont_encoding_alist = Qnil;

  DEFVAR_LISP ("use-default-ascent", &Vuse_default_ascent,
     "Char table of characters whose ascent values should be ignored.\n\
If an entry for a character is non-nil, the ascent value of the glyph\n\
is assumed to be what specified by _MULE_DEFAULT_ASCENT property of a font.\n\
\n\
This affects how a composite character which contains\n\
such a character is displayed on screen.");
  Vuse_default_ascent = Qnil;

  DEFVAR_LISP ("ignore-relative-composition", &Vignore_relative_composition,
     "Char table of characters which is not composed relatively.\n\
If an entry for a character is non-nil, a composition sequence\n\
which contains that character is displayed so that\n\
the glyph of that character is put without considering\n\
an ascent and descent value of a previous character.");
  Vignore_relative_composition = Qnil;

  DEFVAR_LISP ("alternate-fontname-alist", &Valternate_fontname_alist,
     "Alist of fontname vs list of the alternate fontnames.\n\
When a specified font name is not found, the corresponding\n\
alternate fontnames (if any) are tried instead.");
  Valternate_fontname_alist = Qnil;

  DEFVAR_LISP ("fontset-alias-alist", &Vfontset_alias_alist,
     "Alist of fontset names vs the aliases.");
  Vfontset_alias_alist = Qnil;

  DEFVAR_LISP ("highlight-wrong-size-font", &Vhighlight_wrong_size_font,
     "*Non-nil means highlight characters shown in wrong size fonts somehow.\n\
The way to highlight them depends on window system on which Emacs runs.\n\
On X11, a rectangle is shown around each such character.");
  Vhighlight_wrong_size_font = Qnil;

  DEFVAR_LISP ("clip-large-size-font", &Vclip_large_size_font,
     "*Non-nil means characters shown in overlarge fonts are clipped.\n\
The height of clipping area is the same as that of an ASCII character.\n\
The width of the area is the same as that of an ASCII character,\n\
or twice as wide, depending on the character set's column-width.\n\
\n\
If the only font you have for a specific character set is too large,\n\
and clipping these characters makes them hard to read,\n\
you can set this variable to nil to display the characters without clipping.\n\
The drawback is that you will get some garbage left on your screen.");
  Vclip_large_size_font = Qt;

  DEFVAR_LISP ("vertical-centering-font-regexp",
	       &Vvertical_centering_font_regexp,
    "*Regexp matching font names that require vertical centering on display.\n\
When a character is displayed with such fonts, the character is displayed\n\
at the vertival center of lines.");
  Vvertical_centering_font_regexp = Qnil;

  defsubr (&Squery_fontset);
  defsubr (&Snew_fontset);
  defsubr (&Sset_fontset_font);
  defsubr (&Sfont_info);
  defsubr (&Sfontset_info);
}
