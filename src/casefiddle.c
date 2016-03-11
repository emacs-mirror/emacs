/* GNU Emacs case conversion functions.

Copyright (C) 1985, 1994, 1997-1999, 2001-2016 Free Software Foundation,
Inc.

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
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */


#include <config.h>

#include "lisp.h"
#include "character.h"
#include "buffer.h"
#include "commands.h"
#include "syntax.h"
#include "composite.h"
#include "keymap.h"

enum case_action {CASE_UP, CASE_DOWN, CASE_CAPITALIZE, CASE_CAPITALIZE_UP};

static Lisp_Object
casify_object (enum case_action flag, Lisp_Object obj)
{
  int c, c1;
  bool inword = flag == CASE_DOWN;

  /* If the case table is flagged as modified, rescan it.  */
  if (NILP (XCHAR_TABLE (BVAR (current_buffer, downcase_table))->extras[1]))
    Fset_case_table (BVAR (current_buffer, downcase_table));

  if (INTEGERP (obj))
    {
      int flagbits = (CHAR_ALT | CHAR_SUPER | CHAR_HYPER
		      | CHAR_SHIFT | CHAR_CTL | CHAR_META);
      int flags = XINT (obj) & flagbits;
      bool multibyte = ! NILP (BVAR (current_buffer,
				     enable_multibyte_characters));

      /* If the character has higher bits set
	 above the flags, return it unchanged.
	 It is not a real character.  */
      if (UNSIGNED_CMP (XFASTINT (obj), >, flagbits))
	return obj;

      c1 = XFASTINT (obj) & ~flagbits;
      /* FIXME: Even if enable-multibyte-characters is nil, we may
	 manipulate multibyte chars.  This means we have a bug for latin-1
	 chars since when we receive an int 128-255 we can't tell whether
	 it's an eight-bit byte or a latin-1 char.  */
      if (c1 >= 256)
	multibyte = 1;
      if (! multibyte)
	MAKE_CHAR_MULTIBYTE (c1);
      c = downcase (c1);
      if (inword)
	XSETFASTINT (obj, c | flags);
      else if (c == (XFASTINT (obj) & ~flagbits))
	{
	  if (! inword)
	    c = upcase1 (c1);
	  if (! multibyte)
	    MAKE_CHAR_UNIBYTE (c);
	  XSETFASTINT (obj, c | flags);
	}
      return obj;
    }

  if (!STRINGP (obj))
    wrong_type_argument (Qchar_or_string_p, obj);
  else if (!STRING_MULTIBYTE (obj))
    {
      ptrdiff_t i;
      ptrdiff_t size = SCHARS (obj);

      obj = Fcopy_sequence (obj);
      for (i = 0; i < size; i++)
	{
	  c = SREF (obj, i);
	  MAKE_CHAR_MULTIBYTE (c);
	  c1 = c;
	  if (inword && flag != CASE_CAPITALIZE_UP)
	    c = downcase (c);
	  else if (!uppercasep (c)
		   && (!inword || flag != CASE_CAPITALIZE_UP))
	    c = upcase1 (c1);
	  if ((int) flag >= (int) CASE_CAPITALIZE)
	    inword = (SYNTAX (c) == Sword);
	  if (c != c1)
	    {
	      MAKE_CHAR_UNIBYTE (c);
	      /* If the char can't be converted to a valid byte, just don't
		 change it.  */
	      if (c >= 0 && c < 256)
		SSET (obj, i, c);
	    }
	}
      return obj;
    }
  else
    {
      ptrdiff_t i, i_byte, size = SCHARS (obj);
      int len;
      USE_SAFE_ALLOCA;
      ptrdiff_t o_size;
      if (INT_MULTIPLY_WRAPV (size, MAX_MULTIBYTE_LENGTH, &o_size))
	o_size = PTRDIFF_MAX;
      unsigned char *dst = SAFE_ALLOCA (o_size);
      unsigned char *o = dst;

      for (i = i_byte = 0; i < size; i++, i_byte += len)
	{
	  if (o_size - MAX_MULTIBYTE_LENGTH < o - dst)
	    string_overflow ();
	  c = STRING_CHAR_AND_LENGTH (SDATA (obj) + i_byte, len);
	  if (inword && flag != CASE_CAPITALIZE_UP)
	    c = downcase (c);
	  else if (!uppercasep (c)
		   && (!inword || flag != CASE_CAPITALIZE_UP))
	    c = upcase1 (c);
	  if ((int) flag >= (int) CASE_CAPITALIZE)
	    inword = (SYNTAX (c) == Sword);
	  o += CHAR_STRING (c, o);
	}
      eassert (o - dst <= o_size);
      obj = make_multibyte_string ((char *) dst, size, o - dst);
      SAFE_FREE ();
      return obj;
    }
}

DEFUN ("upcase", Fupcase, Supcase, 1, 1, 0,
       doc: /* Convert argument to upper case and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.
See also `capitalize', `downcase' and `upcase-initials'.  */)
  (Lisp_Object obj)
{
  return casify_object (CASE_UP, obj);
}

DEFUN ("downcase", Fdowncase, Sdowncase, 1, 1, 0,
       doc: /* Convert argument to lower case and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.  */)
  (Lisp_Object obj)
{
  return casify_object (CASE_DOWN, obj);
}

DEFUN ("capitalize", Fcapitalize, Scapitalize, 1, 1, 0,
       doc: /* Convert argument to capitalized form and return that.
This means that each word's first character is upper case
and the rest is lower case.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.  */)
  (Lisp_Object obj)
{
  return casify_object (CASE_CAPITALIZE, obj);
}

/* Like Fcapitalize but change only the initials.  */

DEFUN ("upcase-initials", Fupcase_initials, Supcase_initials, 1, 1, 0,
       doc: /* Convert the initial of each word in the argument to upper case.
Do not change the other letters of each word.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.  */)
  (Lisp_Object obj)
{
  return casify_object (CASE_CAPITALIZE_UP, obj);
}

/* flag is CASE_UP, CASE_DOWN or CASE_CAPITALIZE or CASE_CAPITALIZE_UP.
   b and e specify range of buffer to operate on. */

static void
casify_region (enum case_action flag, Lisp_Object b, Lisp_Object e)
{
  int c;
  bool inword = flag == CASE_DOWN;
  bool multibyte = !NILP (BVAR (current_buffer, enable_multibyte_characters));
  ptrdiff_t start, end;
  ptrdiff_t start_byte;

  /* Position of first and last changes.  */
  ptrdiff_t first = -1, last IF_LINT (= 0);

  ptrdiff_t opoint = PT;
  ptrdiff_t opoint_byte = PT_BYTE;

  if (EQ (b, e))
    /* Not modifying because nothing marked */
    return;

  /* If the case table is flagged as modified, rescan it.  */
  if (NILP (XCHAR_TABLE (BVAR (current_buffer, downcase_table))->extras[1]))
    Fset_case_table (BVAR (current_buffer, downcase_table));

  validate_region (&b, &e);
  start = XFASTINT (b);
  end = XFASTINT (e);
  modify_text (start, end);
  record_change (start, end - start);
  start_byte = CHAR_TO_BYTE (start);

  SETUP_BUFFER_SYNTAX_TABLE ();	/* For syntax_prefix_flag_p.  */

  while (start < end)
    {
      int c2, len;

      if (multibyte)
	{
	  c = FETCH_MULTIBYTE_CHAR (start_byte);
	  len = CHAR_BYTES (c);
	}
      else
	{
	  c = FETCH_BYTE (start_byte);
	  MAKE_CHAR_MULTIBYTE (c);
	  len = 1;
	}
      c2 = c;
      if (inword && flag != CASE_CAPITALIZE_UP)
	c = downcase (c);
      else if (!uppercasep (c)
	       && (!inword || flag != CASE_CAPITALIZE_UP))
	c = upcase1 (c);
      if ((int) flag >= (int) CASE_CAPITALIZE)
	inword = ((SYNTAX (c) == Sword)
		  && (inword || !syntax_prefix_flag_p (c)));
      if (c != c2)
	{
	  last = start;
	  if (first < 0)
	    first = start;

	  if (! multibyte)
	    {
	      MAKE_CHAR_UNIBYTE (c);
	      FETCH_BYTE (start_byte) = c;
	    }
	  else if (ASCII_CHAR_P (c2) && ASCII_CHAR_P (c))
	    FETCH_BYTE (start_byte) = c;
	  else
	    {
	      int tolen = CHAR_BYTES (c);
	      int j;
	      unsigned char str[MAX_MULTIBYTE_LENGTH];

	      CHAR_STRING (c, str);
	      if (len == tolen)
		{
		  /* Length is unchanged.  */
		  for (j = 0; j < len; ++j)
		    FETCH_BYTE (start_byte + j) = str[j];
		}
	      else
		{
		  /* Replace one character with the other,
		     keeping text properties the same.  */
		  replace_range_2 (start, start_byte,
				   start + 1, start_byte + len,
				   (char *) str, 1, tolen,
				   0);
		  len = tolen;
		}
	    }
	}
      start++;
      start_byte += len;
    }

  if (PT != opoint)
    TEMP_SET_PT_BOTH (opoint, opoint_byte);

  if (first >= 0)
    {
      signal_after_change (first, last + 1 - first, last + 1 - first);
      update_compositions (first, last + 1, CHECK_ALL);
    }
}

DEFUN ("upcase-region", Fupcase_region, Supcase_region, 2, 2, "r",
       doc: /* Convert the region to upper case.  In programs, wants two arguments.
These arguments specify the starting and ending character numbers of
the region to operate on.  When used as a command, the text between
point and the mark is operated on.
See also `capitalize-region'.  */)
  (Lisp_Object beg, Lisp_Object end)
{
  casify_region (CASE_UP, beg, end);
  return Qnil;
}

DEFUN ("downcase-region", Fdowncase_region, Sdowncase_region, 2, 3,
       "(list (region-beginning) (region-end) (region-noncontiguous-p))",
       doc: /* Convert the region to lower case.  In programs, wants two arguments.
These arguments specify the starting and ending character numbers of
the region to operate on.  When used as a command, the text between
point and the mark is operated on.  */)
  (Lisp_Object beg, Lisp_Object end, Lisp_Object region_noncontiguous_p)
{
  Lisp_Object bounds = Qnil;

  if (!NILP (region_noncontiguous_p))
    {
      bounds = call1 (Fsymbol_value (intern ("region-extract-function")),
		      intern ("bounds"));

      while (CONSP (bounds))
	{
	  casify_region (CASE_DOWN, XCAR (XCAR (bounds)), XCDR (XCAR (bounds)));
	  bounds = XCDR (bounds);
	}
    }
  else
    casify_region (CASE_DOWN, beg, end);

  return Qnil;
}

DEFUN ("capitalize-region", Fcapitalize_region, Scapitalize_region, 2, 2, "r",
       doc: /* Convert the region to capitalized form.
Capitalized form means each word's first character is upper case
and the rest of it is lower case.
In programs, give two arguments, the starting and ending
character positions to operate on.  */)
  (Lisp_Object beg, Lisp_Object end)
{
  casify_region (CASE_CAPITALIZE, beg, end);
  return Qnil;
}

/* Like Fcapitalize_region but change only the initials.  */

DEFUN ("upcase-initials-region", Fupcase_initials_region,
       Supcase_initials_region, 2, 2, "r",
       doc: /* Upcase the initial of each word in the region.
Subsequent letters of each word are not changed.
In programs, give two arguments, the starting and ending
character positions to operate on.  */)
  (Lisp_Object beg, Lisp_Object end)
{
  casify_region (CASE_CAPITALIZE_UP, beg, end);
  return Qnil;
}

static Lisp_Object
operate_on_word (Lisp_Object arg, ptrdiff_t *newpoint)
{
  Lisp_Object val;
  ptrdiff_t farend;
  EMACS_INT iarg;

  CHECK_NUMBER (arg);
  iarg = XINT (arg);
  farend = scan_words (PT, iarg);
  if (!farend)
    farend = iarg > 0 ? ZV : BEGV;

  *newpoint = PT > farend ? PT : farend;
  XSETFASTINT (val, farend);

  return val;
}

DEFUN ("upcase-word", Fupcase_word, Supcase_word, 1, 1, "p",
       doc: /* Convert to upper case from point to end of word, moving over.

If point is in the middle of a word, the part of that word before point
is ignored when moving forward.

With negative argument, convert previous words but do not move.
See also `capitalize-word'.  */)
  (Lisp_Object arg)
{
  Lisp_Object beg, end;
  ptrdiff_t newpoint;
  XSETFASTINT (beg, PT);
  end = operate_on_word (arg, &newpoint);
  casify_region (CASE_UP, beg, end);
  SET_PT (newpoint);
  return Qnil;
}

DEFUN ("downcase-word", Fdowncase_word, Sdowncase_word, 1, 1, "p",
       doc: /* Convert to lower case from point to end of word, moving over.

If point is in the middle of a word, the part of that word before point
is ignored when moving forward.

With negative argument, convert previous words but do not move.  */)
  (Lisp_Object arg)
{
  Lisp_Object beg, end;
  ptrdiff_t newpoint;
  XSETFASTINT (beg, PT);
  end = operate_on_word (arg, &newpoint);
  casify_region (CASE_DOWN, beg, end);
  SET_PT (newpoint);
  return Qnil;
}

DEFUN ("capitalize-word", Fcapitalize_word, Scapitalize_word, 1, 1, "p",
       doc: /* Capitalize from point to the end of word, moving over.
With numerical argument ARG, capitalize the next ARG-1 words as well.
This gives the word(s) a first character in upper case
and the rest lower case.

If point is in the middle of a word, the part of that word before point
is ignored when moving forward.

With negative argument, capitalize previous words but do not move.  */)
  (Lisp_Object arg)
{
  Lisp_Object beg, end;
  ptrdiff_t newpoint;
  XSETFASTINT (beg, PT);
  end = operate_on_word (arg, &newpoint);
  casify_region (CASE_CAPITALIZE, beg, end);
  SET_PT (newpoint);
  return Qnil;
}

void
syms_of_casefiddle (void)
{
  DEFSYM (Qidentity, "identity");
  defsubr (&Supcase);
  defsubr (&Sdowncase);
  defsubr (&Scapitalize);
  defsubr (&Supcase_initials);
  defsubr (&Supcase_region);
  defsubr (&Sdowncase_region);
  defsubr (&Scapitalize_region);
  defsubr (&Supcase_initials_region);
  defsubr (&Supcase_word);
  defsubr (&Sdowncase_word);
  defsubr (&Scapitalize_word);
}

void
keys_of_casefiddle (void)
{
  initial_define_key (control_x_map, Ctl ('U'), "upcase-region");
  Fput (intern ("upcase-region"), Qdisabled, Qt);
  initial_define_key (control_x_map, Ctl ('L'), "downcase-region");
  Fput (intern ("downcase-region"), Qdisabled, Qt);

  initial_define_key (meta_map, 'u', "upcase-word");
  initial_define_key (meta_map, 'l', "downcase-word");
  initial_define_key (meta_map, 'c', "capitalize-word");
}
