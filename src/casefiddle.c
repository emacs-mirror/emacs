/* GNU Emacs case conversion functions.
   Copyright (C) 1985, 1994, 1997, 1998, 1999, 2001, 2002, 2003, 2004,
                 2005 Free Software Foundation, Inc.

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


#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "character.h"
#include "commands.h"
#include "syntax.h"
#include "composite.h"
#include "keymap.h"

enum case_action {CASE_UP, CASE_DOWN, CASE_CAPITALIZE, CASE_CAPITALIZE_UP};

Lisp_Object Qidentity;

Lisp_Object
casify_object (flag, obj)
     enum case_action flag;
     Lisp_Object obj;
{
  register int c, c1;
  register int inword = flag == CASE_DOWN;

  /* If the case table is flagged as modified, rescan it.  */
  if (NILP (XCHAR_TABLE (current_buffer->downcase_table)->extras[1]))
    Fset_case_table (current_buffer->downcase_table);

  while (1)
    {
      if (INTEGERP (obj))
	{
	  int flagbits = (CHAR_ALT | CHAR_SUPER | CHAR_HYPER
			  | CHAR_SHIFT | CHAR_CTL | CHAR_META);
	  int flags = XINT (obj) & flagbits;
	  int multibyte = ! NILP (current_buffer->enable_multibyte_characters);

	  /* If the character has higher bits set
	     above the flags, return it unchanged.
	     It is not a real character.  */
	  if ((unsigned) XFASTINT (obj) > (unsigned) flagbits)
	    return obj;

	  c1 = XFASTINT (obj) & ~flagbits;
	  if (! multibyte)
	    MAKE_CHAR_MULTIBYTE (c1);
	  c = DOWNCASE (c1);
	  if (inword)
	    XSETFASTINT (obj, c | flags);
	  else if (c == (XFASTINT (obj) & ~flagbits))
	    {
	      if (! inword)
		c = UPCASE1 (c1);
	      if (! multibyte)
		MAKE_CHAR_UNIBYTE (c);
	      XSETFASTINT (obj, c | flags);
	    }
	  return obj;
	}

      if (STRINGP (obj))
	{
	  int multibyte = STRING_MULTIBYTE (obj);
	  int i, i_byte, len;
	  int size = SCHARS (obj);

	  obj = Fcopy_sequence (obj);
	  for (i = i_byte = 0; i < size; i++, i_byte += len)
	    {
	      if (multibyte)
		c = STRING_CHAR_AND_LENGTH (SDATA (obj) + i_byte, 0, len);
	      else
		{
		  c = SREF (obj, i_byte);
		  len = 1;
		  MAKE_CHAR_MULTIBYTE (c);
		}
	      c1 = c;
	      if (inword && flag != CASE_CAPITALIZE_UP)
		c = DOWNCASE (c);
	      else if (!UPPERCASEP (c)
		       && (!inword || flag != CASE_CAPITALIZE_UP))
		c = UPCASE1 (c1);
	      if ((int) flag >= (int) CASE_CAPITALIZE)
		inword = (SYNTAX (c) == Sword);
	      if (c != c1)
		{
		  if (! multibyte)
		    {
		      MAKE_CHAR_UNIBYTE (c);
		      SSET (obj, i_byte, c);
		    }
		  else if (ASCII_CHAR_P (c1) && ASCII_CHAR_P (c))
		    SSET (obj, i_byte,  c);
		  else
		    {
		      Faset (obj, make_number (i), make_number (c));
		      i_byte += CHAR_BYTES (c) - len;
		    }
		}
	    }
	  return obj;
	}
      obj = wrong_type_argument (Qchar_or_string_p, obj);
    }
}

DEFUN ("upcase", Fupcase, Supcase, 1, 1, 0,
       doc: /* Convert argument to upper case and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.
See also `capitalize', `downcase' and `upcase-initials'.  */)
     (obj)
     Lisp_Object obj;
{
  return casify_object (CASE_UP, obj);
}

DEFUN ("downcase", Fdowncase, Sdowncase, 1, 1, 0,
       doc: /* Convert argument to lower case and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.  */)
     (obj)
     Lisp_Object obj;
{
  return casify_object (CASE_DOWN, obj);
}

DEFUN ("capitalize", Fcapitalize, Scapitalize, 1, 1, 0,
       doc: /* Convert argument to capitalized form and return that.
This means that each word's first character is upper case
and the rest is lower case.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.  */)
     (obj)
     Lisp_Object obj;
{
  return casify_object (CASE_CAPITALIZE, obj);
}

/* Like Fcapitalize but change only the initials.  */

DEFUN ("upcase-initials", Fupcase_initials, Supcase_initials, 1, 1, 0,
       doc: /* Convert the initial of each word in the argument to upper case.
Do not change the other letters of each word.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.  */)
     (obj)
     Lisp_Object obj;
{
  return casify_object (CASE_CAPITALIZE_UP, obj);
}

/* flag is CASE_UP, CASE_DOWN or CASE_CAPITALIZE or CASE_CAPITALIZE_UP.
   b and e specify range of buffer to operate on. */

void
casify_region (flag, b, e)
     enum case_action flag;
     Lisp_Object b, e;
{
  register int c;
  register int inword = flag == CASE_DOWN;
  register int multibyte = !NILP (current_buffer->enable_multibyte_characters);
  int start, end;
  int start_byte, end_byte;
  int changed = 0;
  int opoint = PT;
  int opoint_byte = PT_BYTE;

  if (EQ (b, e))
    /* Not modifying because nothing marked */
    return;

  /* If the case table is flagged as modified, rescan it.  */
  if (NILP (XCHAR_TABLE (current_buffer->downcase_table)->extras[1]))
    Fset_case_table (current_buffer->downcase_table);

  validate_region (&b, &e);
  start = XFASTINT (b);
  end = XFASTINT (e);
  modify_region (current_buffer, start, end);
  record_change (start, end - start);
  start_byte = CHAR_TO_BYTE (start);
  end_byte = CHAR_TO_BYTE (end);

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
	c = DOWNCASE (c);
      else if (!UPPERCASEP (c)
	       && (!inword || flag != CASE_CAPITALIZE_UP))
	c = UPCASE1 (c);
      if ((int) flag >= (int) CASE_CAPITALIZE)
	inword = ((SYNTAX (c) == Sword) && (inword || !SYNTAX_PREFIX (c)));
      if (c != c2)
	{
	  changed = 1;
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
				   str, 1, tolen,
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

  if (changed)
    {
      start = XFASTINT (b);
      signal_after_change (start, end - start, end - start);
      update_compositions (start, end, CHECK_ALL);
    }
}

DEFUN ("upcase-region", Fupcase_region, Supcase_region, 2, 2, "r",
       doc: /* Convert the region to upper case.  In programs, wants two arguments.
These arguments specify the starting and ending character numbers of
the region to operate on.  When used as a command, the text between
point and the mark is operated on.
See also `capitalize-region'.  */)
     (beg, end)
     Lisp_Object beg, end;
{
  casify_region (CASE_UP, beg, end);
  return Qnil;
}

DEFUN ("downcase-region", Fdowncase_region, Sdowncase_region, 2, 2, "r",
       doc: /* Convert the region to lower case.  In programs, wants two arguments.
These arguments specify the starting and ending character numbers of
the region to operate on.  When used as a command, the text between
point and the mark is operated on.  */)
     (beg, end)
     Lisp_Object beg, end;
{
  casify_region (CASE_DOWN, beg, end);
  return Qnil;
}

DEFUN ("capitalize-region", Fcapitalize_region, Scapitalize_region, 2, 2, "r",
       doc: /* Convert the region to capitalized form.
Capitalized form means each word's first character is upper case
and the rest of it is lower case.
In programs, give two arguments, the starting and ending
character positions to operate on.  */)
     (beg, end)
     Lisp_Object beg, end;
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
     (beg, end)
     Lisp_Object beg, end;
{
  casify_region (CASE_CAPITALIZE_UP, beg, end);
  return Qnil;
}

Lisp_Object
operate_on_word (arg, newpoint)
     Lisp_Object arg;
     int *newpoint;
{
  Lisp_Object val;
  int farend;
  int iarg;

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
       doc: /* Convert following word (or ARG words) to upper case, moving over.
With negative argument, convert previous words but do not move.
See also `capitalize-word'.  */)
     (arg)
     Lisp_Object arg;
{
  Lisp_Object beg, end;
  int newpoint;
  XSETFASTINT (beg, PT);
  end = operate_on_word (arg, &newpoint);
  casify_region (CASE_UP, beg, end);
  SET_PT (newpoint);
  return Qnil;
}

DEFUN ("downcase-word", Fdowncase_word, Sdowncase_word, 1, 1, "p",
       doc: /* Convert following word (or ARG words) to lower case, moving over.
With negative argument, convert previous words but do not move.  */)
     (arg)
     Lisp_Object arg;
{
  Lisp_Object beg, end;
  int newpoint;
  XSETFASTINT (beg, PT);
  end = operate_on_word (arg, &newpoint);
  casify_region (CASE_DOWN, beg, end);
  SET_PT (newpoint);
  return Qnil;
}

DEFUN ("capitalize-word", Fcapitalize_word, Scapitalize_word, 1, 1, "p",
       doc: /* Capitalize the following word (or ARG words), moving over.
This gives the word(s) a first character in upper case
and the rest lower case.
With negative argument, capitalize previous words but do not move.  */)
     (arg)
     Lisp_Object arg;
{
  Lisp_Object beg, end;
  int newpoint;
  XSETFASTINT (beg, PT);
  end = operate_on_word (arg, &newpoint);
  casify_region (CASE_CAPITALIZE, beg, end);
  SET_PT (newpoint);
  return Qnil;
}

void
syms_of_casefiddle ()
{
  Qidentity = intern ("identity");
  staticpro (&Qidentity);
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
keys_of_casefiddle ()
{
  initial_define_key (control_x_map, Ctl('U'), "upcase-region");
  Fput (intern ("upcase-region"), Qdisabled, Qt);
  initial_define_key (control_x_map, Ctl('L'), "downcase-region");
  Fput (intern ("downcase-region"), Qdisabled, Qt);

  initial_define_key (meta_map, 'u', "upcase-word");
  initial_define_key (meta_map, 'l', "downcase-word");
  initial_define_key (meta_map, 'c', "capitalize-word");
}

/* arch-tag: 60a73c66-5489-47e7-a81f-cead4057c526
   (do not change this comment) */
