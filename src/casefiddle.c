/* GNU Emacs case conversion functions.
   Copyright (C) 1985, 1994, 1997 Free Software Foundation, Inc.

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
  register int i, c, len;
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

	  c = DOWNCASE (XFASTINT (obj) & ~flagbits);
	  if (inword)
	    XSETFASTINT (obj, c | flags);
	  else if (c == (XFASTINT (obj) & ~flagbits))
	    {
	      c = UPCASE1 ((XFASTINT (obj) & ~flagbits));
	      XSETFASTINT (obj, c | flags);
	    }
	  return obj;
	}

      if (STRINGP (obj))
	{
	  int multibyte = STRING_MULTIBYTE (obj);

	  obj = Fcopy_sequence (obj);
	  len = STRING_BYTES (XSTRING (obj));

	  /* Scan all single-byte characters from start of string.  */
	  for (i = 0; i < len;)
	    {
	      c = XSTRING (obj)->data[i];

	      if (multibyte && c >= 0x80)
		/* A multibyte character can't be handled in this
                   simple loop.  */
		break;
	      if (inword && flag != CASE_CAPITALIZE_UP)
		c = DOWNCASE (c);
	      else if (!UPPERCASEP (c)
		       && (!inword || flag != CASE_CAPITALIZE_UP))
		c = UPCASE1 (c);
	      /* If this char won't fit in a single-byte string.
		 fall out to the multibyte case.  */
	      if (multibyte ? ! ASCII_BYTE_P (c)
		  : ! SINGLE_BYTE_CHAR_P (c))
		break;

	      XSTRING (obj)->data[i] = c;
	      if ((int) flag >= (int) CASE_CAPITALIZE)
		inword = SYNTAX (c) == Sword;
	      i++;
	    }

	  /* If we didn't do the whole string as single-byte,
	     scan the rest in a more complex way.  */
	  if (i < len)
	    {
	      /* The work is not yet finished because of a multibyte
		 character just encountered.  */
	      int fromlen, j_byte = i;
	      char *buf
		= (char *) alloca ((len - i) * MAX_MULTIBYTE_LENGTH + i);

	      /* Copy data already handled.  */
	      bcopy (XSTRING (obj)->data, buf, i);

	      /* From now on, I counts bytes.  */
	      while (i < len)
		{
		  c = STRING_CHAR_AND_LENGTH (XSTRING (obj)->data + i,
					      len - i, fromlen);
		  if (inword && flag != CASE_CAPITALIZE_UP)
		    c = DOWNCASE (c);
		  else if (!UPPERCASEP (c)
			   && (!inword || flag != CASE_CAPITALIZE_UP))
		    c = UPCASE1 (c);
		  i += fromlen;
		  j_byte += CHAR_STRING (c, buf + j_byte);
		  if ((int) flag >= (int) CASE_CAPITALIZE)
		    inword = SYNTAX (c) == Sword;
		}
	      obj = make_multibyte_string (buf, XSTRING (obj)->size,
					   j_byte);
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
  register int i;
  register int c;
  register int inword = flag == CASE_DOWN;
  register int multibyte = !NILP (current_buffer->enable_multibyte_characters);
  int start, end;
  int start_byte, end_byte;
  int changed = 0;

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

  for (i = start_byte; i < end_byte; i++, start++)
    {
      int c2;
      c = c2 = FETCH_BYTE (i);
      if (multibyte && c >= 0x80)
	/* A multibyte character can't be handled in this simple loop.  */
	break;
      if (inword && flag != CASE_CAPITALIZE_UP)
	c = DOWNCASE (c);
      else if (!UPPERCASEP (c)
	       && (!inword || flag != CASE_CAPITALIZE_UP))
	c = UPCASE1 (c);
      FETCH_BYTE (i) = c;
      if (c != c2)
	changed = 1;
      if ((int) flag >= (int) CASE_CAPITALIZE)
	inword = SYNTAX (c) == Sword;
    }
  if (i < end_byte)
    {
      /* The work is not yet finished because of a multibyte character
	 just encountered.  */
      int opoint = PT;
      int opoint_byte = PT_BYTE;
      int c2;

      while (i < end_byte)
	{
	  if ((c = FETCH_BYTE (i)) >= 0x80)
	    c = FETCH_MULTIBYTE_CHAR (i);
	  c2 = c;
	  if (inword && flag != CASE_CAPITALIZE_UP)
	    c2 = DOWNCASE (c);
	  else if (!UPPERCASEP (c)
		   && (!inword || flag != CASE_CAPITALIZE_UP))
	    c2 = UPCASE1 (c);
	  if (c != c2)
	    {
	      int fromlen, tolen, j;
	      unsigned char str[MAX_MULTIBYTE_LENGTH];

	      changed = 1;
	      /* Handle the most likely case */
	      if (multibyte ? (c < 0200 && c2 < 0200)
		  : (c < 0400 && c2 < 0400))
		FETCH_BYTE (i) = c2;
	      else if (fromlen = CHAR_STRING (c, str),
		       tolen = CHAR_STRING (c2, str),
		       fromlen == tolen)
		{
		  for (j = 0; j < tolen; ++j)
		    FETCH_BYTE (i + j) = str[j];
		}
	      else
		{
		  error ("Can't casify letters that change length");
#if 0 /* This is approximately what we'd like to be able to do here */
		  if (tolen < fromlen)
		    del_range_1 (i + tolen, i + fromlen, 0, 0);
		  else if (tolen > fromlen)
		    {
		      TEMP_SET_PT (i + fromlen);
		      insert_1 (str + fromlen, tolen - fromlen, 1, 0, 0);
		    }
#endif
		}
	    }
	  if ((int) flag >= (int) CASE_CAPITALIZE)
	    inword = SYNTAX (c2) == Sword;
	  INC_BOTH (start, i);
	}
      TEMP_SET_PT_BOTH (opoint, opoint_byte);
    }

  start = XFASTINT (b);
  if (changed)
    {
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
