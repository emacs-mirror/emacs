/* Random utility Lisp functions.
   Copyright (C) 1985, 86, 87, 93, 94, 95, 97, 1998 Free Software Foundation, Inc.

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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <time.h>

/* Note on some machines this defines `vector' as a typedef,
   so make sure we don't use that name in this file.  */
#undef vector
#define vector *****

#include "lisp.h"
#include "commands.h"
#include "charset.h"

#include "buffer.h"
#include "keyboard.h"
#include "intervals.h"
#include "frame.h"
#include "window.h"
#if defined (HAVE_MENUS) && defined (HAVE_X_WINDOWS)
#include "xterm.h"
#endif

#ifndef NULL
#define NULL (void *)0
#endif

/* Nonzero enables use of dialog boxes for questions
   asked by mouse commands.  */
int use_dialog_box;

extern int minibuffer_auto_raise;
extern Lisp_Object minibuf_window;

Lisp_Object Qstring_lessp, Qprovide, Qrequire;
Lisp_Object Qyes_or_no_p_history;
Lisp_Object Qcursor_in_echo_area;
Lisp_Object Qwidget_type;

extern Lisp_Object Qinput_method_function;

static int internal_equal ();

extern long get_random ();
extern void seed_random ();

#ifndef HAVE_UNISTD_H
extern long time ();
#endif

DEFUN ("identity", Fidentity, Sidentity, 1, 1, 0,
  "Return the argument unchanged.")
  (arg)
     Lisp_Object arg;
{
  return arg;
}

DEFUN ("random", Frandom, Srandom, 0, 1, 0,
  "Return a pseudo-random number.\n\
All integers representable in Lisp are equally likely.\n\
  On most systems, this is 28 bits' worth.\n\
With positive integer argument N, return random number in interval [0,N).\n\
With argument t, set the random number seed from the current time and pid.")
  (n)
     Lisp_Object n;
{
  EMACS_INT val;
  Lisp_Object lispy_val;
  unsigned long denominator;

  if (EQ (n, Qt))
    seed_random (getpid () + time (NULL));
  if (NATNUMP (n) && XFASTINT (n) != 0)
    {
      /* Try to take our random number from the higher bits of VAL,
	 not the lower, since (says Gentzel) the low bits of `random'
	 are less random than the higher ones.  We do this by using the
	 quotient rather than the remainder.  At the high end of the RNG
	 it's possible to get a quotient larger than n; discarding
	 these values eliminates the bias that would otherwise appear
	 when using a large n.  */
      denominator = ((unsigned long)1 << VALBITS) / XFASTINT (n);
      do
	val = get_random () / denominator;
      while (val >= XFASTINT (n));
    }
  else
    val = get_random ();
  XSETINT (lispy_val, val);
  return lispy_val;
}

/* Random data-structure functions */

DEFUN ("length", Flength, Slength, 1, 1, 0,
  "Return the length of vector, list or string SEQUENCE.\n\
A byte-code function object is also allowed.\n\
If the string contains multibyte characters, this is not the necessarily\n\
the number of bytes in the string; it is the number of characters.\n\
To get the number of bytes, use `string-bytes'")
  (sequence)
     register Lisp_Object sequence;
{
  register Lisp_Object tail, val;
  register int i;

 retry:
  if (STRINGP (sequence))
    XSETFASTINT (val, XSTRING (sequence)->size);
  else if (VECTORP (sequence))
    XSETFASTINT (val, XVECTOR (sequence)->size);
  else if (CHAR_TABLE_P (sequence))
    XSETFASTINT (val, (MIN_CHAR_COMPOSITION
		       + (CHAR_FIELD2_MASK | CHAR_FIELD3_MASK)
		       - 1));
  else if (BOOL_VECTOR_P (sequence))
    XSETFASTINT (val, XBOOL_VECTOR (sequence)->size);
  else if (COMPILEDP (sequence))
    XSETFASTINT (val, XVECTOR (sequence)->size & PSEUDOVECTOR_SIZE_MASK);
  else if (CONSP (sequence))
    {
      for (i = 0, tail = sequence; !NILP (tail); i++)
	{
	  QUIT;
	  tail = Fcdr (tail);
	}

      XSETFASTINT (val, i);
    }
  else if (NILP (sequence))
    XSETFASTINT (val, 0);
  else
    {
      sequence = wrong_type_argument (Qsequencep, sequence);
      goto retry;
    }
  return val;
}

/* This does not check for quits.  That is safe
   since it must terminate.  */

DEFUN ("safe-length", Fsafe_length, Ssafe_length, 1, 1, 0,
  "Return the length of a list, but avoid error or infinite loop.\n\
This function never gets an error.  If LIST is not really a list,\n\
it returns 0.  If LIST is circular, it returns a finite value\n\
which is at least the number of distinct elements.")
  (list)
     Lisp_Object list;
{
  Lisp_Object tail, halftail, length;
  int len = 0;

  /* halftail is used to detect circular lists.  */
  halftail = list;
  for (tail = list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      if (EQ (tail, halftail) && len != 0)
	break;
      len++;
      if ((len & 1) == 0)
	halftail = XCONS (halftail)->cdr;
    }

  XSETINT (length, len);
  return length;
}

DEFUN ("string-bytes", Fstring_bytes, Sstring_bytes, 1, 1, 0,
  "Return the number of bytes in STRING.\n\
If STRING is a multibyte string, this is greater than the length of STRING.")
  (string)
     Lisp_Object string;
{
  CHECK_STRING (string, 1);
  return make_number (STRING_BYTES (XSTRING (string)));
}

DEFUN ("string-equal", Fstring_equal, Sstring_equal, 2, 2, 0,
  "Return t if two strings have identical contents.\n\
Case is significant, but text properties are ignored.\n\
Symbols are also allowed; their print names are used instead.")
  (s1, s2)
     register Lisp_Object s1, s2;
{
  if (SYMBOLP (s1))
    XSETSTRING (s1, XSYMBOL (s1)->name);
  if (SYMBOLP (s2))
    XSETSTRING (s2, XSYMBOL (s2)->name);
  CHECK_STRING (s1, 0);
  CHECK_STRING (s2, 1);

  if (XSTRING (s1)->size != XSTRING (s2)->size
      || STRING_BYTES (XSTRING (s1)) != STRING_BYTES (XSTRING (s2))
      || bcmp (XSTRING (s1)->data, XSTRING (s2)->data, STRING_BYTES (XSTRING (s1))))
    return Qnil;
  return Qt;
}

DEFUN ("compare-strings", Fcompare_strings,
       Scompare_strings, 6, 7, 0,
  "Compare the contents of two strings, converting to multibyte if needed.\n\
In string STR1, skip the first START1 characters and stop at END1.\n\
In string STR2, skip the first START2 characters and stop at END2.\n\
END1 and END2 default to the full lengths of the respective strings.\n\
\n\
Case is significant in this comparison if IGNORE-CASE is nil.\n\
Unibyte strings are converted to multibyte for comparison.\n\
\n\
The value is t if the strings (or specified portions) match.\n\
If string STR1 is less, the value is a negative number N;\n\
  - 1 - N is the number of characters that match at the beginning.\n\
If string STR1 is greater, the value is a positive number N;\n\
  N - 1 is the number of characters that match at the beginning.")
  (str1, start1, end1, str2, start2, end2, ignore_case)
     Lisp_Object str1, start1, end1, start2, str2, end2, ignore_case;
{
  register int end1_char, end2_char;
  register int i1, i1_byte, i2, i2_byte;

  CHECK_STRING (str1, 0);
  CHECK_STRING (str2, 1);
  if (NILP (start1))
    start1 = make_number (0);
  if (NILP (start2))
    start2 = make_number (0);
  CHECK_NATNUM (start1, 2);
  CHECK_NATNUM (start2, 3);
  if (! NILP (end1))
    CHECK_NATNUM (end1, 4);
  if (! NILP (end2))
    CHECK_NATNUM (end2, 4);

  i1 = XINT (start1);
  i2 = XINT (start2);

  i1_byte = string_char_to_byte (str1, i1);
  i2_byte = string_char_to_byte (str2, i2);

  end1_char = XSTRING (str1)->size;
  if (! NILP (end1) && end1_char > XINT (end1))
    end1_char = XINT (end1);

  end2_char = XSTRING (str2)->size;
  if (! NILP (end2) && end2_char > XINT (end2))
    end2_char = XINT (end2);

  while (i1 < end1_char && i2 < end2_char)
    {
      /* When we find a mismatch, we must compare the
	 characters, not just the bytes.  */
      int c1, c2;

      if (STRING_MULTIBYTE (str1))
	FETCH_STRING_CHAR_ADVANCE (c1, str1, i1, i1_byte);
      else
	{
	  c1 = XSTRING (str1)->data[i1++];
	  c1 = unibyte_char_to_multibyte (c1);
	}

      if (STRING_MULTIBYTE (str2))
	FETCH_STRING_CHAR_ADVANCE (c2, str2, i2, i2_byte);
      else
	{
	  c2 = XSTRING (str2)->data[i2++];
	  c2 = unibyte_char_to_multibyte (c2);
	}

      if (c1 == c2)
	continue;

      if (! NILP (ignore_case))
	{
	  Lisp_Object tem;

	  tem = Fupcase (make_number (c1));
	  c1 = XINT (tem);
	  tem = Fupcase (make_number (c2));
	  c2 = XINT (tem);
	}

      if (c1 == c2)
	continue;

      /* Note that I1 has already been incremented
	 past the character that we are comparing;
	 hence we don't add or subtract 1 here.  */
      if (c1 < c2)
	return make_number (- i1);
      else
	return make_number (i1);
    }

  if (i1 < end1_char)
    return make_number (i1 - XINT (start1) + 1);
  if (i2 < end2_char)
    return make_number (- i1 + XINT (start1) - 1);

  return Qt;
}

DEFUN ("string-lessp", Fstring_lessp, Sstring_lessp, 2, 2, 0,
  "Return t if first arg string is less than second in lexicographic order.\n\
Case is significant.\n\
Symbols are also allowed; their print names are used instead.")
  (s1, s2)
     register Lisp_Object s1, s2;
{
  register int end;
  register int i1, i1_byte, i2, i2_byte;

  if (SYMBOLP (s1))
    XSETSTRING (s1, XSYMBOL (s1)->name);
  if (SYMBOLP (s2))
    XSETSTRING (s2, XSYMBOL (s2)->name);
  CHECK_STRING (s1, 0);
  CHECK_STRING (s2, 1);

  i1 = i1_byte = i2 = i2_byte = 0;

  end = XSTRING (s1)->size;
  if (end > XSTRING (s2)->size)
    end = XSTRING (s2)->size;

  while (i1 < end)
    {
      /* When we find a mismatch, we must compare the
	 characters, not just the bytes.  */
      int c1, c2;

      if (STRING_MULTIBYTE (s1))
	FETCH_STRING_CHAR_ADVANCE (c1, s1, i1, i1_byte);
      else
	c1 = XSTRING (s1)->data[i1++];

      if (STRING_MULTIBYTE (s2))
	FETCH_STRING_CHAR_ADVANCE (c2, s2, i2, i2_byte);
      else
	c2 = XSTRING (s2)->data[i2++];

      if (c1 != c2)
	return c1 < c2 ? Qt : Qnil;
    }
  return i1 < XSTRING (s2)->size ? Qt : Qnil;
}

static Lisp_Object concat ();

/* ARGSUSED */
Lisp_Object
concat2 (s1, s2)
     Lisp_Object s1, s2;
{
#ifdef NO_ARG_ARRAY
  Lisp_Object args[2];
  args[0] = s1;
  args[1] = s2;
  return concat (2, args, Lisp_String, 0);
#else
  return concat (2, &s1, Lisp_String, 0);
#endif /* NO_ARG_ARRAY */
}

/* ARGSUSED */
Lisp_Object
concat3 (s1, s2, s3)
     Lisp_Object s1, s2, s3;
{
#ifdef NO_ARG_ARRAY
  Lisp_Object args[3];
  args[0] = s1;
  args[1] = s2;
  args[2] = s3;
  return concat (3, args, Lisp_String, 0);
#else
  return concat (3, &s1, Lisp_String, 0);
#endif /* NO_ARG_ARRAY */
}

DEFUN ("append", Fappend, Sappend, 0, MANY, 0,
  "Concatenate all the arguments and make the result a list.\n\
The result is a list whose elements are the elements of all the arguments.\n\
Each argument may be a list, vector or string.\n\
The last argument is not copied, just used as the tail of the new list.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return concat (nargs, args, Lisp_Cons, 1);
}

DEFUN ("concat", Fconcat, Sconcat, 0, MANY, 0,
  "Concatenate all the arguments and make the result a string.\n\
The result is a string whose elements are the elements of all the arguments.\n\
Each argument may be a string or a list or vector of characters (integers).\n\
\n\
Do not use individual integers as arguments!\n\
The behavior of `concat' in that case will be changed later!\n\
If your program passes an integer as an argument to `concat',\n\
you should change it right away not to do so.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return concat (nargs, args, Lisp_String, 0);
}

DEFUN ("vconcat", Fvconcat, Svconcat, 0, MANY, 0,
  "Concatenate all the arguments and make the result a vector.\n\
The result is a vector whose elements are the elements of all the arguments.\n\
Each argument may be a list, vector or string.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return concat (nargs, args, Lisp_Vectorlike, 0);
}

/* Retrun a copy of a sub char table ARG.  The elements except for a
   nested sub char table are not copied.  */
static Lisp_Object
copy_sub_char_table (arg)
     Lisp_Object arg;
{
  Lisp_Object copy = make_sub_char_table (XCHAR_TABLE (arg)->defalt);
  int i;

  /* Copy all the contents.  */
  bcopy (XCHAR_TABLE (arg)->contents, XCHAR_TABLE (copy)->contents,
	 SUB_CHAR_TABLE_ORDINARY_SLOTS * sizeof (Lisp_Object));
  /* Recursively copy any sub char-tables in the ordinary slots.  */
  for (i = 32; i < SUB_CHAR_TABLE_ORDINARY_SLOTS; i++)
    if (SUB_CHAR_TABLE_P (XCHAR_TABLE (arg)->contents[i]))
      XCHAR_TABLE (copy)->contents[i]
	= copy_sub_char_table (XCHAR_TABLE (copy)->contents[i]);

  return copy;
}


DEFUN ("copy-sequence", Fcopy_sequence, Scopy_sequence, 1, 1, 0,
  "Return a copy of a list, vector or string.\n\
The elements of a list or vector are not copied; they are shared\n\
with the original.")
  (arg)
     Lisp_Object arg;
{
  if (NILP (arg)) return arg;

  if (CHAR_TABLE_P (arg))
    {
      int i;
      Lisp_Object copy;

      copy = Fmake_char_table (XCHAR_TABLE (arg)->purpose, Qnil);
      /* Copy all the slots, including the extra ones.  */
      bcopy (XVECTOR (arg)->contents, XVECTOR (copy)->contents,
	     ((XCHAR_TABLE (arg)->size & PSEUDOVECTOR_SIZE_MASK)
	      * sizeof (Lisp_Object)));

      /* Recursively copy any sub char tables in the ordinary slots
         for multibyte characters.  */
      for (i = CHAR_TABLE_SINGLE_BYTE_SLOTS;
	   i < CHAR_TABLE_ORDINARY_SLOTS; i++)
	if (SUB_CHAR_TABLE_P (XCHAR_TABLE (arg)->contents[i]))
	  XCHAR_TABLE (copy)->contents[i]
	    = copy_sub_char_table (XCHAR_TABLE (copy)->contents[i]);

      return copy;
    }

  if (BOOL_VECTOR_P (arg))
    {
      Lisp_Object val;
      int size_in_chars
	= (XBOOL_VECTOR (arg)->size + BITS_PER_CHAR - 1) / BITS_PER_CHAR;

      val = Fmake_bool_vector (Flength (arg), Qnil);
      bcopy (XBOOL_VECTOR (arg)->data, XBOOL_VECTOR (val)->data,
	     size_in_chars);
      return val;
    }

  if (!CONSP (arg) && !VECTORP (arg) && !STRINGP (arg))
    arg = wrong_type_argument (Qsequencep, arg);
  return concat (1, &arg, CONSP (arg) ? Lisp_Cons : XTYPE (arg), 0);
}

static Lisp_Object
concat (nargs, args, target_type, last_special)
     int nargs;
     Lisp_Object *args;
     enum Lisp_Type target_type;
     int last_special;
{
  Lisp_Object val;
  register Lisp_Object tail;
  register Lisp_Object this;
  int toindex;
  int toindex_byte;
  register int result_len;
  register int result_len_byte;
  register int argnum;
  Lisp_Object last_tail;
  Lisp_Object prev;
  int some_multibyte;
  /* When we make a multibyte string, we must pay attention to the
     byte combining problem, i.e., a byte may be combined with a
     multibyte charcter of the previous string.  This flag tells if we
     must consider such a situation or not.  */
  int maybe_combine_byte;

  /* In append, the last arg isn't treated like the others */
  if (last_special && nargs > 0)
    {
      nargs--;
      last_tail = args[nargs];
    }
  else
    last_tail = Qnil;

  /* Canonicalize each argument.  */
  for (argnum = 0; argnum < nargs; argnum++)
    {
      this = args[argnum];
      if (!(CONSP (this) || NILP (this) || VECTORP (this) || STRINGP (this)
	    || COMPILEDP (this) || BOOL_VECTOR_P (this)))
	{
	  if (INTEGERP (this))
            args[argnum] = Fnumber_to_string (this);
	  else
	    args[argnum] = wrong_type_argument (Qsequencep, this);
	}
    }

  /* Compute total length in chars of arguments in RESULT_LEN.
     If desired output is a string, also compute length in bytes
     in RESULT_LEN_BYTE, and determine in SOME_MULTIBYTE
     whether the result should be a multibyte string.  */
  result_len_byte = 0;
  result_len = 0;
  some_multibyte = 0;
  for (argnum = 0; argnum < nargs; argnum++)
    {
      int len;
      this = args[argnum];
      len = XFASTINT (Flength (this));
      if (target_type == Lisp_String)
	{
	  /* We must count the number of bytes needed in the string
	     as well as the number of characters.  */
	  int i;
	  Lisp_Object ch;
	  int this_len_byte;

	  if (VECTORP (this))
	    for (i = 0; i < len; i++)
	      {
		ch = XVECTOR (this)->contents[i];
		if (! INTEGERP (ch))
		  wrong_type_argument (Qintegerp, ch);
		this_len_byte = CHAR_BYTES (XINT (ch));
		result_len_byte += this_len_byte;
		if (this_len_byte > 1)
		  some_multibyte = 1;
	      }
	  else if (BOOL_VECTOR_P (this) && XBOOL_VECTOR (this)->size > 0)
	    wrong_type_argument (Qintegerp, Faref (this, make_number (0)));
	  else if (CONSP (this))
	    for (; CONSP (this); this = XCONS (this)->cdr)
	      {
		ch = XCONS (this)->car;
		if (! INTEGERP (ch))
		  wrong_type_argument (Qintegerp, ch);
		this_len_byte = CHAR_BYTES (XINT (ch));
		result_len_byte += this_len_byte;
		if (this_len_byte > 1)
		  some_multibyte = 1;
	      }
	  else if (STRINGP (this))
	    {
	      if (STRING_MULTIBYTE (this))
		{
		  some_multibyte = 1;
		  result_len_byte += STRING_BYTES (XSTRING (this));
		}
	      else
		result_len_byte += count_size_as_multibyte (XSTRING (this)->data,
							    XSTRING (this)->size);
	    }
	}

      result_len += len;
    }

  if (! some_multibyte)
    result_len_byte = result_len;

  /* Create the output object.  */
  if (target_type == Lisp_Cons)
    val = Fmake_list (make_number (result_len), Qnil);
  else if (target_type == Lisp_Vectorlike)
    val = Fmake_vector (make_number (result_len), Qnil);
  else if (some_multibyte)
    val = make_uninit_multibyte_string (result_len, result_len_byte);
  else
    val = make_uninit_string (result_len);

  /* In `append', if all but last arg are nil, return last arg.  */
  if (target_type == Lisp_Cons && EQ (val, Qnil))
    return last_tail;

  /* Copy the contents of the args into the result.  */
  if (CONSP (val))
    tail = val, toindex = -1;		/* -1 in toindex is flag we are making a list */
  else
    toindex = 0, toindex_byte = 0;

  prev = Qnil;

  maybe_combine_byte = 0;
  for (argnum = 0; argnum < nargs; argnum++)
    {
      Lisp_Object thislen;
      int thisleni;
      register unsigned int thisindex = 0;
      register unsigned int thisindex_byte = 0;

      this = args[argnum];
      if (!CONSP (this))
	thislen = Flength (this), thisleni = XINT (thislen);

      if (STRINGP (this) && STRINGP (val)
	  && ! NULL_INTERVAL_P (XSTRING (this)->intervals))
	copy_text_properties (make_number (0), thislen, this,
			      make_number (toindex), val, Qnil);

      /* Between strings of the same kind, copy fast.  */
      if (STRINGP (this) && STRINGP (val)
	  && STRING_MULTIBYTE (this) == some_multibyte)
	{
	  int thislen_byte = STRING_BYTES (XSTRING (this));
	  bcopy (XSTRING (this)->data, XSTRING (val)->data + toindex_byte,
		 STRING_BYTES (XSTRING (this)));
	  if (some_multibyte
	      && toindex_byte > 0
	      && !ASCII_BYTE_P (XSTRING (val)->data[toindex_byte - 1])
	      && !CHAR_HEAD_P (XSTRING (this)->data[0]))
	    maybe_combine_byte = 1;
	  toindex_byte += thislen_byte;
	  toindex += thisleni;
	}
      /* Copy a single-byte string to a multibyte string.  */
      else if (STRINGP (this) && STRINGP (val))
	{
	  toindex_byte += copy_text (XSTRING (this)->data,
				     XSTRING (val)->data + toindex_byte,
				     XSTRING (this)->size, 0, 1);
	  toindex += thisleni;
	}
      else
	/* Copy element by element.  */
	while (1)
	  {
	    register Lisp_Object elt;

	    /* Fetch next element of `this' arg into `elt', or break if
	       `this' is exhausted. */
	    if (NILP (this)) break;
	    if (CONSP (this))
	      elt = XCONS (this)->car, this = XCONS (this)->cdr;
	    else if (thisindex >= thisleni)
	      break;
	    else if (STRINGP (this))
	      {
		int c;
		if (STRING_MULTIBYTE (this))
		  {
		    FETCH_STRING_CHAR_ADVANCE (c, this,
					       thisindex,
					       thisindex_byte);
		    XSETFASTINT (elt, c);
		  }
		else
		  {
		    XSETFASTINT (elt, XSTRING (this)->data[thisindex++]);
		    if (some_multibyte
			&& (XINT (elt) >= 0240
			    || (XINT (elt) >= 0200
				&& ! NILP (Vnonascii_translation_table)))
			&& XINT (elt) < 0400)
		      {
			c = unibyte_char_to_multibyte (XINT (elt));
			XSETINT (elt, c);
		      }
		  }
	      }
	    else if (BOOL_VECTOR_P (this))
	      {
		int byte;
		byte = XBOOL_VECTOR (this)->data[thisindex / BITS_PER_CHAR];
		if (byte & (1 << (thisindex % BITS_PER_CHAR)))
		  elt = Qt;
		else
		  elt = Qnil;
		thisindex++;
	      }
	    else
	      elt = XVECTOR (this)->contents[thisindex++];

	    /* Store this element into the result.  */
	    if (toindex < 0)
	      {
		XCONS (tail)->car = elt;
		prev = tail;
		tail = XCONS (tail)->cdr;
	      }
	    else if (VECTORP (val))
	      XVECTOR (val)->contents[toindex++] = elt;
	    else
	      {
		CHECK_NUMBER (elt, 0);
		if (SINGLE_BYTE_CHAR_P (XINT (elt)))
		  {
		    if (some_multibyte
			&& toindex_byte > 0
			&& !ASCII_BYTE_P (XSTRING (val)->data[toindex_byte - 1])
			&& !CHAR_HEAD_P (XINT (elt)))
		      maybe_combine_byte = 1;
		    XSTRING (val)->data[toindex_byte++] = XINT (elt);
		    toindex++;
		  }
		else
		  /* If we have any multibyte characters,
		     we already decided to make a multibyte string.  */
		  {
		    int c = XINT (elt);
		    unsigned char work[4], *str;
		    int i = CHAR_STRING (c, work, str);

		    /* P exists as a variable
		       to avoid a bug on the Masscomp C compiler.  */
		    unsigned char *p = & XSTRING (val)->data[toindex_byte];
		    bcopy (str, p, i);
		    toindex_byte += i;
		    toindex++;
		  }
	      }
	  }
    }
  if (!NILP (prev))
    XCONS (prev)->cdr = last_tail;

  if (maybe_combine_byte)
    /* Character counter of the multibyte string VAL may be wrong
       because of byte combining problem.  We must re-calculate it.  */
    XSTRING (val)->size = multibyte_chars_in_text (XSTRING (val)->data,
						   XSTRING (val)->size_byte);

  return val;
}

static Lisp_Object string_char_byte_cache_string;
static int string_char_byte_cache_charpos;
static int string_char_byte_cache_bytepos;

void
clear_string_char_byte_cache ()
{
  string_char_byte_cache_string = Qnil;
}

/* Return the character index corresponding to CHAR_INDEX in STRING.  */

int
string_char_to_byte (string, char_index)
     Lisp_Object string;
     int char_index;
{
  int i, i_byte;
  int best_below, best_below_byte;
  int best_above, best_above_byte;

  if (! STRING_MULTIBYTE (string))
    return char_index;

  best_below = best_below_byte = 0;
  best_above = XSTRING (string)->size;
  best_above_byte = STRING_BYTES (XSTRING (string));

  if (EQ (string, string_char_byte_cache_string))
    {
      if (string_char_byte_cache_charpos < char_index)
	{
	  best_below = string_char_byte_cache_charpos;
	  best_below_byte = string_char_byte_cache_bytepos;
	}
      else
	{
	  best_above = string_char_byte_cache_charpos;
	  best_above_byte = string_char_byte_cache_bytepos;
	}
    }

  if (char_index - best_below < best_above - char_index)
    {
      while (best_below < char_index)
	{
	  int c;
	  FETCH_STRING_CHAR_ADVANCE (c, string, best_below, best_below_byte);
	}
      i = best_below;
      i_byte = best_below_byte;
    }
  else
    {
      while (best_above > char_index)
	{
	  int best_above_byte_saved = --best_above_byte;

	  while (best_above_byte > 0
		 && !CHAR_HEAD_P (XSTRING (string)->data[best_above_byte]))
	    best_above_byte--;
	  if (!BASE_LEADING_CODE_P (XSTRING (string)->data[best_above_byte]))
	    best_above_byte = best_above_byte_saved;
	  best_above--;
	}
      i = best_above;
      i_byte = best_above_byte;
    }

  string_char_byte_cache_bytepos = i_byte;
  string_char_byte_cache_charpos = i;
  string_char_byte_cache_string = string;

  return i_byte;
}

/* Return the character index corresponding to BYTE_INDEX in STRING.  */

int
string_byte_to_char (string, byte_index)
     Lisp_Object string;
     int byte_index;
{
  int i, i_byte;
  int best_below, best_below_byte;
  int best_above, best_above_byte;

  if (! STRING_MULTIBYTE (string))
    return byte_index;

  best_below = best_below_byte = 0;
  best_above = XSTRING (string)->size;
  best_above_byte = STRING_BYTES (XSTRING (string));

  if (EQ (string, string_char_byte_cache_string))
    {
      if (string_char_byte_cache_bytepos < byte_index)
	{
	  best_below = string_char_byte_cache_charpos;
	  best_below_byte = string_char_byte_cache_bytepos;
	}
      else
	{
	  best_above = string_char_byte_cache_charpos;
	  best_above_byte = string_char_byte_cache_bytepos;
	}
    }

  if (byte_index - best_below_byte < best_above_byte - byte_index)
    {
      while (best_below_byte < byte_index)
	{
	  int c;
	  FETCH_STRING_CHAR_ADVANCE (c, string, best_below, best_below_byte);
	}
      i = best_below;
      i_byte = best_below_byte;
    }
  else
    {
      while (best_above_byte > byte_index)
	{
	  int best_above_byte_saved = --best_above_byte;

	  while (best_above_byte > 0
		 && !CHAR_HEAD_P (XSTRING (string)->data[best_above_byte]))
	    best_above_byte--;
	  if (!BASE_LEADING_CODE_P (XSTRING (string)->data[best_above_byte]))
	    best_above_byte = best_above_byte_saved;
	  best_above--;
	}
      i = best_above;
      i_byte = best_above_byte;
    }

  string_char_byte_cache_bytepos = i_byte;
  string_char_byte_cache_charpos = i;
  string_char_byte_cache_string = string;

  return i;
}

/* Convert STRING to a multibyte string.
   Single-byte characters 0240 through 0377 are converted
   by adding nonascii_insert_offset to each.  */

Lisp_Object
string_make_multibyte (string)
     Lisp_Object string;
{
  unsigned char *buf;
  int nbytes;

  if (STRING_MULTIBYTE (string))
    return string;

  nbytes = count_size_as_multibyte (XSTRING (string)->data,
				    XSTRING (string)->size);
  /* If all the chars are ASCII, they won't need any more bytes
     once converted.  In that case, we can return STRING itself.  */
  if (nbytes == STRING_BYTES (XSTRING (string)))
    return string;

  buf = (unsigned char *) alloca (nbytes);
  copy_text (XSTRING (string)->data, buf, STRING_BYTES (XSTRING (string)),
	     0, 1);

  return make_multibyte_string (buf, XSTRING (string)->size, nbytes);
}

/* Convert STRING to a single-byte string.  */

Lisp_Object
string_make_unibyte (string)
     Lisp_Object string;
{
  unsigned char *buf;

  if (! STRING_MULTIBYTE (string))
    return string;

  buf = (unsigned char *) alloca (XSTRING (string)->size);

  copy_text (XSTRING (string)->data, buf, STRING_BYTES (XSTRING (string)),
	     1, 0);

  return make_unibyte_string (buf, XSTRING (string)->size);
}

DEFUN ("string-make-multibyte", Fstring_make_multibyte, Sstring_make_multibyte,
       1, 1, 0,
  "Return the multibyte equivalent of STRING.\n\
The function `unibyte-char-to-multibyte' is used to convert\n\
each unibyte character to a multibyte character.")
  (string)
     Lisp_Object string;
{
  CHECK_STRING (string, 0);

  return string_make_multibyte (string);
}

DEFUN ("string-make-unibyte", Fstring_make_unibyte, Sstring_make_unibyte,
       1, 1, 0,
  "Return the unibyte equivalent of STRING.\n\
Multibyte character codes are converted to unibyte\n\
by using just the low 8 bits.")
  (string)
     Lisp_Object string;
{
  CHECK_STRING (string, 0);

  return string_make_unibyte (string);
}

DEFUN ("string-as-unibyte", Fstring_as_unibyte, Sstring_as_unibyte,
       1, 1, 0,
  "Return a unibyte string with the same individual bytes as STRING.\n\
If STRING is unibyte, the result is STRING itself.\n\
Otherwise it is a newly created string, with no text properties.")
  (string)
     Lisp_Object string;
{
  CHECK_STRING (string, 0);

  if (STRING_MULTIBYTE (string))
    {
      string = Fcopy_sequence (string);
      XSTRING (string)->size = STRING_BYTES (XSTRING (string));
      XSTRING (string)->intervals = NULL_INTERVAL;
      SET_STRING_BYTES (XSTRING (string), -1);
    }
  return string;
}

DEFUN ("string-as-multibyte", Fstring_as_multibyte, Sstring_as_multibyte,
       1, 1, 0,
  "Return a multibyte string with the same individual bytes as STRING.\n\
If STRING is multibyte, the result is STRING itself.\n\
Otherwise it is a newly created string, with no text properties.")
  (string)
     Lisp_Object string;
{
  CHECK_STRING (string, 0);

  if (! STRING_MULTIBYTE (string))
    {
      int nbytes = STRING_BYTES (XSTRING (string));
      int newlen = multibyte_chars_in_text (XSTRING (string)->data, nbytes);

      string = Fcopy_sequence (string);
      XSTRING (string)->size = newlen;
      XSTRING (string)->size_byte = nbytes;
      XSTRING (string)->intervals = NULL_INTERVAL;
    }
  return string;
}

DEFUN ("copy-alist", Fcopy_alist, Scopy_alist, 1, 1, 0,
  "Return a copy of ALIST.\n\
This is an alist which represents the same mapping from objects to objects,\n\
but does not share the alist structure with ALIST.\n\
The objects mapped (cars and cdrs of elements of the alist)\n\
are shared, however.\n\
Elements of ALIST that are not conses are also shared.")
  (alist)
     Lisp_Object alist;
{
  register Lisp_Object tem;

  CHECK_LIST (alist, 0);
  if (NILP (alist))
    return alist;
  alist = concat (1, &alist, Lisp_Cons, 0);
  for (tem = alist; CONSP (tem); tem = XCONS (tem)->cdr)
    {
      register Lisp_Object car;
      car = XCONS (tem)->car;

      if (CONSP (car))
	XCONS (tem)->car = Fcons (XCONS (car)->car, XCONS (car)->cdr);
    }
  return alist;
}

DEFUN ("substring", Fsubstring, Ssubstring, 2, 3, 0,
  "Return a substring of STRING, starting at index FROM and ending before TO.\n\
TO may be nil or omitted; then the substring runs to the end of STRING.\n\
If FROM or TO is negative, it counts from the end.\n\
\n\
This function allows vectors as well as strings.")
  (string, from, to)
     Lisp_Object string;
     register Lisp_Object from, to;
{
  Lisp_Object res;
  int size;
  int size_byte;
  int from_char, to_char;
  int from_byte, to_byte;

  if (! (STRINGP (string) || VECTORP (string)))
    wrong_type_argument (Qarrayp, string);

  CHECK_NUMBER (from, 1);

  if (STRINGP (string))
    {
      size = XSTRING (string)->size;
      size_byte = STRING_BYTES (XSTRING (string));
    }
  else
    size = XVECTOR (string)->size;

  if (NILP (to))
    {
      to_char = size;
      to_byte = size_byte;
    }
  else
    {
      CHECK_NUMBER (to, 2);

      to_char = XINT (to);
      if (to_char < 0)
	to_char += size;

      if (STRINGP (string))
	to_byte = string_char_to_byte (string, to_char);
    }

  from_char = XINT (from);
  if (from_char < 0)
    from_char += size;
  if (STRINGP (string))
    from_byte = string_char_to_byte (string, from_char);

  if (!(0 <= from_char && from_char <= to_char && to_char <= size))
    args_out_of_range_3 (string, make_number (from_char),
			 make_number (to_char));

  if (STRINGP (string))
    {
      res = make_specified_string (XSTRING (string)->data + from_byte,
				   to_char - from_char, to_byte - from_byte,
				   STRING_MULTIBYTE (string));
      copy_text_properties (make_number (from_char), make_number (to_char),
			    string, make_number (0), res, Qnil);
    }
  else
    res = Fvector (to_char - from_char,
		   XVECTOR (string)->contents + from_char);

  return res;
}

/* Extract a substring of STRING, giving start and end positions
   both in characters and in bytes.  */

Lisp_Object
substring_both (string, from, from_byte, to, to_byte)
     Lisp_Object string;
     int from, from_byte, to, to_byte;
{
  Lisp_Object res;
  int size;
  int size_byte;

  if (! (STRINGP (string) || VECTORP (string)))
    wrong_type_argument (Qarrayp, string);

  if (STRINGP (string))
    {
      size = XSTRING (string)->size;
      size_byte = STRING_BYTES (XSTRING (string));
    }
  else
    size = XVECTOR (string)->size;

  if (!(0 <= from && from <= to && to <= size))
    args_out_of_range_3 (string, make_number (from), make_number (to));

  if (STRINGP (string))
    {
      res = make_specified_string (XSTRING (string)->data + from_byte,
				   to - from, to_byte - from_byte,
				   STRING_MULTIBYTE (string));
      copy_text_properties (make_number (from), make_number (to),
			    string, make_number (0), res, Qnil);
    }
  else
    res = Fvector (to - from,
		   XVECTOR (string)->contents + from);

  return res;
}

DEFUN ("nthcdr", Fnthcdr, Snthcdr, 2, 2, 0,
  "Take cdr N times on LIST, returns the result.")
  (n, list)
     Lisp_Object n;
     register Lisp_Object list;
{
  register int i, num;
  CHECK_NUMBER (n, 0);
  num = XINT (n);
  for (i = 0; i < num && !NILP (list); i++)
    {
      QUIT;
      list = Fcdr (list);
    }
  return list;
}

DEFUN ("nth", Fnth, Snth, 2, 2, 0,
  "Return the Nth element of LIST.\n\
N counts from zero.  If LIST is not that long, nil is returned.")
  (n, list)
     Lisp_Object n, list;
{
  return Fcar (Fnthcdr (n, list));
}

DEFUN ("elt", Felt, Selt, 2, 2, 0,
  "Return element of SEQUENCE at index N.")
  (sequence, n)
     register Lisp_Object sequence, n;
{
  CHECK_NUMBER (n, 0);
  while (1)
    {
      if (CONSP (sequence) || NILP (sequence))
	return Fcar (Fnthcdr (n, sequence));
      else if (STRINGP (sequence) || VECTORP (sequence)
	       || BOOL_VECTOR_P (sequence) || CHAR_TABLE_P (sequence))
	return Faref (sequence, n);
      else
	sequence = wrong_type_argument (Qsequencep, sequence);
    }
}

DEFUN ("member", Fmember, Smember, 2, 2, 0,
  "Return non-nil if ELT is an element of LIST.  Comparison done with `equal'.\n\
The value is actually the tail of LIST whose car is ELT.")
  (elt, list)
     register Lisp_Object elt;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NILP (tail); tail = XCONS (tail)->cdr)
    {
      register Lisp_Object tem;
      tem = Fcar (tail);
      if (! NILP (Fequal (elt, tem)))
	return tail;
      QUIT;
    }
  return Qnil;
}

DEFUN ("memq", Fmemq, Smemq, 2, 2, 0,
  "Return non-nil if ELT is an element of LIST.  Comparison done with EQ.\n\
The value is actually the tail of LIST whose car is ELT.")
  (elt, list)
     register Lisp_Object elt;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NILP (tail); tail = XCONS (tail)->cdr)
    {
      register Lisp_Object tem;
      tem = Fcar (tail);
      if (EQ (elt, tem)) return tail;
      QUIT;
    }
  return Qnil;
}

DEFUN ("assq", Fassq, Sassq, 2, 2, 0,
  "Return non-nil if KEY is `eq' to the car of an element of LIST.\n\
The value is actually the element of LIST whose car is KEY.\n\
Elements of LIST that are not conses are ignored.")
  (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NILP (tail); tail = XCONS (tail)->cdr)
    {
      register Lisp_Object elt, tem;
      elt = Fcar (tail);
      if (!CONSP (elt)) continue;
      tem = XCONS (elt)->car;
      if (EQ (key, tem)) return elt;
      QUIT;
    }
  return Qnil;
}

/* Like Fassq but never report an error and do not allow quits.
   Use only on lists known never to be circular.  */

Lisp_Object
assq_no_quit (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      register Lisp_Object elt, tem;
      elt = Fcar (tail);
      if (!CONSP (elt)) continue;
      tem = XCONS (elt)->car;
      if (EQ (key, tem)) return elt;
    }
  return Qnil;
}

DEFUN ("assoc", Fassoc, Sassoc, 2, 2, 0,
  "Return non-nil if KEY is `equal' to the car of an element of LIST.\n\
The value is actually the element of LIST whose car equals KEY.")
  (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NILP (tail); tail = XCONS (tail)->cdr)
    {
      register Lisp_Object elt, tem;
      elt = Fcar (tail);
      if (!CONSP (elt)) continue;
      tem = Fequal (XCONS (elt)->car, key);
      if (!NILP (tem)) return elt;
      QUIT;
    }
  return Qnil;
}

DEFUN ("rassq", Frassq, Srassq, 2, 2, 0,
  "Return non-nil if ELT is `eq' to the cdr of an element of LIST.\n\
The value is actually the element of LIST whose cdr is ELT.")
  (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NILP (tail); tail = XCONS (tail)->cdr)
    {
      register Lisp_Object elt, tem;
      elt = Fcar (tail);
      if (!CONSP (elt)) continue;
      tem = XCONS (elt)->cdr;
      if (EQ (key, tem)) return elt;
      QUIT;
    }
  return Qnil;
}

DEFUN ("rassoc", Frassoc, Srassoc, 2, 2, 0,
  "Return non-nil if KEY is `equal' to the cdr of an element of LIST.\n\
The value is actually the element of LIST whose cdr equals KEY.")
  (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NILP (tail); tail = XCONS (tail)->cdr)
    {
      register Lisp_Object elt, tem;
      elt = Fcar (tail);
      if (!CONSP (elt)) continue;
      tem = Fequal (XCONS (elt)->cdr, key);
      if (!NILP (tem)) return elt;
      QUIT;
    }
  return Qnil;
}

DEFUN ("delq", Fdelq, Sdelq, 2, 2, 0,
  "Delete by side effect any occurrences of ELT as a member of LIST.\n\
The modified LIST is returned.  Comparison is done with `eq'.\n\
If the first member of LIST is ELT, there is no way to remove it by side effect;\n\
therefore, write `(setq foo (delq element foo))'\n\
to be sure of changing the value of `foo'.")
  (elt, list)
     register Lisp_Object elt;
     Lisp_Object list;
{
  register Lisp_Object tail, prev;
  register Lisp_Object tem;

  tail = list;
  prev = Qnil;
  while (!NILP (tail))
    {
      tem = Fcar (tail);
      if (EQ (elt, tem))
	{
	  if (NILP (prev))
	    list = XCONS (tail)->cdr;
	  else
	    Fsetcdr (prev, XCONS (tail)->cdr);
	}
      else
	prev = tail;
      tail = XCONS (tail)->cdr;
      QUIT;
    }
  return list;
}

DEFUN ("delete", Fdelete, Sdelete, 2, 2, 0,
  "Delete by side effect any occurrences of ELT as a member of LIST.\n\
The modified LIST is returned.  Comparison is done with `equal'.\n\
If the first member of LIST is ELT, deleting it is not a side effect;\n\
it is simply using a different list.\n\
Therefore, write `(setq foo (delete element foo))'\n\
to be sure of changing the value of `foo'.")
  (elt, list)
     register Lisp_Object elt;
     Lisp_Object list;
{
  register Lisp_Object tail, prev;
  register Lisp_Object tem;

  tail = list;
  prev = Qnil;
  while (!NILP (tail))
    {
      tem = Fcar (tail);
      if (! NILP (Fequal (elt, tem)))
	{
	  if (NILP (prev))
	    list = XCONS (tail)->cdr;
	  else
	    Fsetcdr (prev, XCONS (tail)->cdr);
	}
      else
	prev = tail;
      tail = XCONS (tail)->cdr;
      QUIT;
    }
  return list;
}

DEFUN ("nreverse", Fnreverse, Snreverse, 1, 1, 0,
  "Reverse LIST by modifying cdr pointers.\n\
Returns the beginning of the reversed list.")
  (list)
     Lisp_Object list;
{
  register Lisp_Object prev, tail, next;

  if (NILP (list)) return list;
  prev = Qnil;
  tail = list;
  while (!NILP (tail))
    {
      QUIT;
      next = Fcdr (tail);
      Fsetcdr (tail, prev);
      prev = tail;
      tail = next;
    }
  return prev;
}

DEFUN ("reverse", Freverse, Sreverse, 1, 1, 0,
  "Reverse LIST, copying.  Returns the beginning of the reversed list.\n\
See also the function `nreverse', which is used more often.")
  (list)
     Lisp_Object list;
{
  Lisp_Object new;

  for (new = Qnil; CONSP (list); list = XCONS (list)->cdr)
    new = Fcons (XCONS (list)->car, new);
  if (!NILP (list))
    wrong_type_argument (Qconsp, list);
  return new;
}

Lisp_Object merge ();

DEFUN ("sort", Fsort, Ssort, 2, 2, 0,
  "Sort LIST, stably, comparing elements using PREDICATE.\n\
Returns the sorted list.  LIST is modified by side effects.\n\
PREDICATE is called with two elements of LIST, and should return T\n\
if the first element is \"less\" than the second.")
  (list, predicate)
     Lisp_Object list, predicate;
{
  Lisp_Object front, back;
  register Lisp_Object len, tem;
  struct gcpro gcpro1, gcpro2;
  register int length;

  front = list;
  len = Flength (list);
  length = XINT (len);
  if (length < 2)
    return list;

  XSETINT (len, (length / 2) - 1);
  tem = Fnthcdr (len, list);
  back = Fcdr (tem);
  Fsetcdr (tem, Qnil);

  GCPRO2 (front, back);
  front = Fsort (front, predicate);
  back = Fsort (back, predicate);
  UNGCPRO;
  return merge (front, back, predicate);
}

Lisp_Object
merge (org_l1, org_l2, pred)
     Lisp_Object org_l1, org_l2;
     Lisp_Object pred;
{
  Lisp_Object value;
  register Lisp_Object tail;
  Lisp_Object tem;
  register Lisp_Object l1, l2;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  l1 = org_l1;
  l2 = org_l2;
  tail = Qnil;
  value = Qnil;

  /* It is sufficient to protect org_l1 and org_l2.
     When l1 and l2 are updated, we copy the new values
     back into the org_ vars.  */
  GCPRO4 (org_l1, org_l2, pred, value);

  while (1)
    {
      if (NILP (l1))
	{
	  UNGCPRO;
	  if (NILP (tail))
	    return l2;
	  Fsetcdr (tail, l2);
	  return value;
	}
      if (NILP (l2))
	{
	  UNGCPRO;
	  if (NILP (tail))
	    return l1;
	  Fsetcdr (tail, l1);
	  return value;
	}
      tem = call2 (pred, Fcar (l2), Fcar (l1));
      if (NILP (tem))
	{
	  tem = l1;
	  l1 = Fcdr (l1);
	  org_l1 = l1;
	}
      else
	{
	  tem = l2;
	  l2 = Fcdr (l2);
	  org_l2 = l2;
	}
      if (NILP (tail))
	value = tem;
      else
	Fsetcdr (tail, tem);
      tail = tem;
    }
}


DEFUN ("plist-get", Fplist_get, Splist_get, 2, 2, 0,
  "Extract a value from a property list.\n\
PLIST is a property list, which is a list of the form\n\
\(PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value\n\
corresponding to the given PROP, or nil if PROP is not\n\
one of the properties on the list.")
  (plist, prop)
     Lisp_Object plist;
     register Lisp_Object prop;
{
  register Lisp_Object tail;
  for (tail = plist; !NILP (tail); tail = Fcdr (XCONS (tail)->cdr))
    {
      register Lisp_Object tem;
      tem = Fcar (tail);
      if (EQ (prop, tem))
	return Fcar (XCONS (tail)->cdr);
    }
  return Qnil;
}

DEFUN ("get", Fget, Sget, 2, 2, 0,
  "Return the value of SYMBOL's PROPNAME property.\n\
This is the last value stored with `(put SYMBOL PROPNAME VALUE)'.")
  (symbol, propname)
     Lisp_Object symbol, propname;
{
  CHECK_SYMBOL (symbol, 0);
  return Fplist_get (XSYMBOL (symbol)->plist, propname);
}

DEFUN ("plist-put", Fplist_put, Splist_put, 3, 3, 0,
  "Change value in PLIST of PROP to VAL.\n\
PLIST is a property list, which is a list of the form\n\
\(PROP1 VALUE1 PROP2 VALUE2 ...).  PROP is a symbol and VAL is any object.\n\
If PROP is already a property on the list, its value is set to VAL,\n\
otherwise the new PROP VAL pair is added.  The new plist is returned;\n\
use `(setq x (plist-put x prop val))' to be sure to use the new value.\n\
The PLIST is modified by side effects.")
  (plist, prop, val)
     Lisp_Object plist;
     register Lisp_Object prop;
     Lisp_Object val;
{
  register Lisp_Object tail, prev;
  Lisp_Object newcell;
  prev = Qnil;
  for (tail = plist; CONSP (tail) && CONSP (XCONS (tail)->cdr);
       tail = XCONS (XCONS (tail)->cdr)->cdr)
    {
      if (EQ (prop, XCONS (tail)->car))
	{
	  Fsetcar (XCONS (tail)->cdr, val);
	  return plist;
	}
      prev = tail;
    }
  newcell = Fcons (prop, Fcons (val, Qnil));
  if (NILP (prev))
    return newcell;
  else
    Fsetcdr (XCONS (prev)->cdr, newcell);
  return plist;
}

DEFUN ("put", Fput, Sput, 3, 3, 0,
  "Store SYMBOL's PROPNAME property with value VALUE.\n\
It can be retrieved with `(get SYMBOL PROPNAME)'.")
  (symbol, propname, value)
     Lisp_Object symbol, propname, value;
{
  CHECK_SYMBOL (symbol, 0);
  XSYMBOL (symbol)->plist
    = Fplist_put (XSYMBOL (symbol)->plist, propname, value);
  return value;
}

DEFUN ("equal", Fequal, Sequal, 2, 2, 0,
  "Return t if two Lisp objects have similar structure and contents.\n\
They must have the same data type.\n\
Conses are compared by comparing the cars and the cdrs.\n\
Vectors and strings are compared element by element.\n\
Numbers are compared by value, but integers cannot equal floats.\n\
 (Use `=' if you want integers and floats to be able to be equal.)\n\
Symbols must match exactly.")
  (o1, o2)
     register Lisp_Object o1, o2;
{
  return internal_equal (o1, o2, 0) ? Qt : Qnil;
}

static int
internal_equal (o1, o2, depth)
     register Lisp_Object o1, o2;
     int depth;
{
  if (depth > 200)
    error ("Stack overflow in equal");

 tail_recurse:
  QUIT;
  if (EQ (o1, o2))
    return 1;
  if (XTYPE (o1) != XTYPE (o2))
    return 0;

  switch (XTYPE (o1))
    {
#ifdef LISP_FLOAT_TYPE
    case Lisp_Float:
      return (extract_float (o1) == extract_float (o2));
#endif

    case Lisp_Cons:
      if (!internal_equal (XCONS (o1)->car, XCONS (o2)->car, depth + 1))
	return 0;
      o1 = XCONS (o1)->cdr;
      o2 = XCONS (o2)->cdr;
      goto tail_recurse;

    case Lisp_Misc:
      if (XMISCTYPE (o1) != XMISCTYPE (o2))
	return 0;
      if (OVERLAYP (o1))
	{
	  if (!internal_equal (OVERLAY_START (o1), OVERLAY_START (o1),
			       depth + 1)
	      || !internal_equal (OVERLAY_END (o1), OVERLAY_END (o1),
				  depth + 1))
	    return 0;
	  o1 = XOVERLAY (o1)->plist;
	  o2 = XOVERLAY (o2)->plist;
	  goto tail_recurse;
	}
      if (MARKERP (o1))
	{
	  return (XMARKER (o1)->buffer == XMARKER (o2)->buffer
		  && (XMARKER (o1)->buffer == 0
		      || XMARKER (o1)->bytepos == XMARKER (o2)->bytepos));
	}
      break;

    case Lisp_Vectorlike:
      {
	register int i, size;
	size = XVECTOR (o1)->size;
	/* Pseudovectors have the type encoded in the size field, so this test
	   actually checks that the objects have the same type as well as the
	   same size.  */
	if (XVECTOR (o2)->size != size)
	  return 0;
	/* Boolvectors are compared much like strings.  */
	if (BOOL_VECTOR_P (o1))
	  {
	    int size_in_chars
	      = (XBOOL_VECTOR (o1)->size + BITS_PER_CHAR - 1) / BITS_PER_CHAR;

	    if (XBOOL_VECTOR (o1)->size != XBOOL_VECTOR (o2)->size)
	      return 0;
	    if (bcmp (XBOOL_VECTOR (o1)->data, XBOOL_VECTOR (o2)->data,
		      size_in_chars))
	      return 0;
	    return 1;
	  }
	if (WINDOW_CONFIGURATIONP (o1))
	  return compare_window_configurations (o1, o2, 0);

	/* Aside from them, only true vectors, char-tables, and compiled
	   functions are sensible to compare, so eliminate the others now.  */
	if (size & PSEUDOVECTOR_FLAG)
	  {
	    if (!(size & (PVEC_COMPILED | PVEC_CHAR_TABLE)))
	      return 0;
	    size &= PSEUDOVECTOR_SIZE_MASK;
	  }
	for (i = 0; i < size; i++)
	  {
	    Lisp_Object v1, v2;
	    v1 = XVECTOR (o1)->contents [i];
	    v2 = XVECTOR (o2)->contents [i];
	    if (!internal_equal (v1, v2, depth + 1))
	      return 0;
	  }
	return 1;
      }
      break;

    case Lisp_String:
      if (XSTRING (o1)->size != XSTRING (o2)->size)
	return 0;
      if (STRING_BYTES (XSTRING (o1)) != STRING_BYTES (XSTRING (o2)))
	return 0;
      if (bcmp (XSTRING (o1)->data, XSTRING (o2)->data,
		STRING_BYTES (XSTRING (o1))))
	return 0;
      return 1;
    }
  return 0;
}

extern Lisp_Object Fmake_char_internal ();

DEFUN ("fillarray", Ffillarray, Sfillarray, 2, 2, 0,
  "Store each element of ARRAY with ITEM.\n\
ARRAY is a vector, string, char-table, or bool-vector.")
  (array, item)
     Lisp_Object array, item;
{
  register int size, index, charval;
 retry:
  if (VECTORP (array))
    {
      register Lisp_Object *p = XVECTOR (array)->contents;
      size = XVECTOR (array)->size;
      for (index = 0; index < size; index++)
	p[index] = item;
    }
  else if (CHAR_TABLE_P (array))
    {
      register Lisp_Object *p = XCHAR_TABLE (array)->contents;
      size = CHAR_TABLE_ORDINARY_SLOTS;
      for (index = 0; index < size; index++)
	p[index] = item;
      XCHAR_TABLE (array)->defalt = Qnil;
    }
  else if (STRINGP (array))
    {
      register unsigned char *p = XSTRING (array)->data;
      CHECK_NUMBER (item, 1);
      charval = XINT (item);
      size = XSTRING (array)->size;
      if (STRING_MULTIBYTE (array))
	{
	  unsigned char workbuf[4], *str;
	  int len = CHAR_STRING (charval, workbuf, str);
	  int size_byte = STRING_BYTES (XSTRING (array));
	  unsigned char *p1 = p, *endp = p + size_byte;
	  int i;

	  if (size != size_byte)
	    while (p1 < endp)
	      {
		int this_len = MULTIBYTE_FORM_LENGTH (p1, endp - p1);
		if (len != this_len)
		  error ("Attempt to change byte length of a string");
		p1 += this_len;
	      }
	  for (i = 0; i < size_byte; i++)
	    *p++ = str[i % len];
	}
      else
	for (index = 0; index < size; index++)
	  p[index] = charval;
    }
  else if (BOOL_VECTOR_P (array))
    {
      register unsigned char *p = XBOOL_VECTOR (array)->data;
      int size_in_chars
	= (XBOOL_VECTOR (array)->size + BITS_PER_CHAR - 1) / BITS_PER_CHAR;

      charval = (! NILP (item) ? -1 : 0);
      for (index = 0; index < size_in_chars; index++)
	p[index] = charval;
    }
  else
    {
      array = wrong_type_argument (Qarrayp, array);
      goto retry;
    }
  return array;
}

DEFUN ("char-table-subtype", Fchar_table_subtype, Schar_table_subtype,
       1, 1, 0,
  "Return the subtype of char-table CHAR-TABLE.   The value is a symbol.")
  (char_table)
     Lisp_Object char_table;
{
  CHECK_CHAR_TABLE (char_table, 0);

  return XCHAR_TABLE (char_table)->purpose;
}

DEFUN ("char-table-parent", Fchar_table_parent, Schar_table_parent,
       1, 1, 0,
  "Return the parent char-table of CHAR-TABLE.\n\
The value is either nil or another char-table.\n\
If CHAR-TABLE holds nil for a given character,\n\
then the actual applicable value is inherited from the parent char-table\n\
\(or from its parents, if necessary).")
  (char_table)
     Lisp_Object char_table;
{
  CHECK_CHAR_TABLE (char_table, 0);

  return XCHAR_TABLE (char_table)->parent;
}

DEFUN ("set-char-table-parent", Fset_char_table_parent, Sset_char_table_parent,
       2, 2, 0,
  "Set the parent char-table of CHAR-TABLE to PARENT.\n\
PARENT must be either nil or another char-table.")
  (char_table, parent)
     Lisp_Object char_table, parent;
{
  Lisp_Object temp;

  CHECK_CHAR_TABLE (char_table, 0);

  if (!NILP (parent))
    {
      CHECK_CHAR_TABLE (parent, 0);

      for (temp = parent; !NILP (temp); temp = XCHAR_TABLE (temp)->parent)
	if (EQ (temp, char_table))
	  error ("Attempt to make a chartable be its own parent");
    }

  XCHAR_TABLE (char_table)->parent = parent;

  return parent;
}

DEFUN ("char-table-extra-slot", Fchar_table_extra_slot, Schar_table_extra_slot,
       2, 2, 0,
  "Return the value of CHAR-TABLE's extra-slot number N.")
  (char_table, n)
     Lisp_Object char_table, n;
{
  CHECK_CHAR_TABLE (char_table, 1);
  CHECK_NUMBER (n, 2);
  if (XINT (n) < 0
      || XINT (n) >= CHAR_TABLE_EXTRA_SLOTS (XCHAR_TABLE (char_table)))
    args_out_of_range (char_table, n);

  return XCHAR_TABLE (char_table)->extras[XINT (n)];
}

DEFUN ("set-char-table-extra-slot", Fset_char_table_extra_slot,
       Sset_char_table_extra_slot,
       3, 3, 0,
  "Set CHAR-TABLE's extra-slot number N to VALUE.")
  (char_table, n, value)
     Lisp_Object char_table, n, value;
{
  CHECK_CHAR_TABLE (char_table, 1);
  CHECK_NUMBER (n, 2);
  if (XINT (n) < 0
      || XINT (n) >= CHAR_TABLE_EXTRA_SLOTS (XCHAR_TABLE (char_table)))
    args_out_of_range (char_table, n);

  return XCHAR_TABLE (char_table)->extras[XINT (n)] = value;
}

DEFUN ("char-table-range", Fchar_table_range, Schar_table_range,
       2, 2, 0,
  "Return the value in CHAR-TABLE for a range of characters RANGE.\n\
RANGE should be nil (for the default value)\n\
a vector which identifies a character set or a row of a character set,\n\
a character set name, or a character code.")
  (char_table, range)
     Lisp_Object char_table, range;
{
  int i;

  CHECK_CHAR_TABLE (char_table, 0);

  if (EQ (range, Qnil))
    return XCHAR_TABLE (char_table)->defalt;
  else if (INTEGERP (range))
    return Faref (char_table, range);
  else if (SYMBOLP (range))
    {
      Lisp_Object charset_info;

      charset_info = Fget (range, Qcharset);
      CHECK_VECTOR (charset_info, 0);

      return Faref (char_table,
		    make_number (XINT (XVECTOR (charset_info)->contents[0])
				 + 128));
    }
  else if (VECTORP (range))
    {
      if (XVECTOR (range)->size == 1)
	return Faref (char_table,
		      make_number (XINT (XVECTOR (range)->contents[0]) + 128));
      else
	{
	  int size = XVECTOR (range)->size;
	  Lisp_Object *val = XVECTOR (range)->contents;
	  Lisp_Object ch = Fmake_char_internal (size <= 0 ? Qnil : val[0],
						size <= 1 ? Qnil : val[1],
						size <= 2 ? Qnil : val[2]);
	  return Faref (char_table, ch);
	}
    }
  else
    error ("Invalid RANGE argument to `char-table-range'");
}

DEFUN ("set-char-table-range", Fset_char_table_range, Sset_char_table_range,
       3, 3, 0,
  "Set the value in CHAR-TABLE for a range of characters RANGE to VALUE.\n\
RANGE should be t (for all characters), nil (for the default value)\n\
a vector which identifies a character set or a row of a character set,\n\
a coding system, or a character code.")
  (char_table, range, value)
     Lisp_Object char_table, range, value;
{
  int i;

  CHECK_CHAR_TABLE (char_table, 0);

  if (EQ (range, Qt))
    for (i = 0; i < CHAR_TABLE_ORDINARY_SLOTS; i++)
      XCHAR_TABLE (char_table)->contents[i] = value;
  else if (EQ (range, Qnil))
    XCHAR_TABLE (char_table)->defalt = value;
  else if (SYMBOLP (range))
    {
      Lisp_Object charset_info;

      charset_info = Fget (range, Qcharset);
      CHECK_VECTOR (charset_info, 0);

      return Faset (char_table,
		    make_number (XINT (XVECTOR (charset_info)->contents[0])
				 + 128),
		    value);
    }
  else if (INTEGERP (range))
    Faset (char_table, range, value);
  else if (VECTORP (range))
    {
      if (XVECTOR (range)->size == 1)
	return Faset (char_table,
		      make_number (XINT (XVECTOR (range)->contents[0]) + 128),
		      value);
      else
	{
	  int size = XVECTOR (range)->size;
	  Lisp_Object *val = XVECTOR (range)->contents;
	  Lisp_Object ch = Fmake_char_internal (size <= 0 ? Qnil : val[0],
						size <= 1 ? Qnil : val[1],
						size <= 2 ? Qnil : val[2]);
	  return Faset (char_table, ch, value);
	}
    }
  else
    error ("Invalid RANGE argument to `set-char-table-range'");

  return value;
}

DEFUN ("set-char-table-default", Fset_char_table_default,
       Sset_char_table_default, 3, 3, 0,
  "Set the default value in CHAR-TABLE for a generic character CHAR to VALUE.\n\
The generic character specifies the group of characters.\n\
See also the documentation of make-char.")
  (char_table, ch, value)
     Lisp_Object char_table, ch, value;
{
  int c, i, charset, code1, code2;
  Lisp_Object temp;

  CHECK_CHAR_TABLE (char_table, 0);
  CHECK_NUMBER (ch, 1);

  c = XINT (ch);
  SPLIT_NON_ASCII_CHAR (c, charset, code1, code2);

  /* Since we may want to set the default value for a character set
     not yet defined, we check only if the character set is in the
     valid range or not, instead of it is already defined or not.  */
  if (! CHARSET_VALID_P (charset))
    invalid_character (c);

  if (charset == CHARSET_ASCII)
    return (XCHAR_TABLE (char_table)->defalt = value);

  /* Even if C is not a generic char, we had better behave as if a
     generic char is specified.  */
  if (charset == CHARSET_COMPOSITION || CHARSET_DIMENSION (charset) == 1)
    code1 = 0;
  temp = XCHAR_TABLE (char_table)->contents[charset + 128];
  if (!code1)
    {
      if (SUB_CHAR_TABLE_P (temp))
	XCHAR_TABLE (temp)->defalt = value;
      else
	XCHAR_TABLE (char_table)->contents[charset + 128] = value;
      return value;
    }
  char_table = temp;
  if (! SUB_CHAR_TABLE_P (char_table))
    char_table = (XCHAR_TABLE (char_table)->contents[charset + 128]
	    = make_sub_char_table (temp));
  temp = XCHAR_TABLE (char_table)->contents[code1];
  if (SUB_CHAR_TABLE_P (temp))
    XCHAR_TABLE (temp)->defalt = value;
  else
    XCHAR_TABLE (char_table)->contents[code1] = value;
  return value;
}

/* Look up the element in TABLE at index CH,
   and return it as an integer.
   If the element is nil, return CH itself.
   (Actually we do that for any non-integer.)  */

int
char_table_translate (table, ch)
     Lisp_Object table;
     int ch;
{
  Lisp_Object value;
  value = Faref (table, make_number (ch));
  if (! INTEGERP (value))
    return ch;
  return XINT (value);
}

/* Map C_FUNCTION or FUNCTION over SUBTABLE, calling it for each
   character or group of characters that share a value.
   DEPTH is the current depth in the originally specified
   chartable, and INDICES contains the vector indices
   for the levels our callers have descended.

   ARG is passed to C_FUNCTION when that is called.  */

void
map_char_table (c_function, function, subtable, arg, depth, indices)
     void (*c_function) P_ ((Lisp_Object, Lisp_Object, Lisp_Object));
     Lisp_Object function, subtable, arg, *indices;
     int depth;
{
  int i, to;

  if (depth == 0)
    {
      /* At first, handle ASCII and 8-bit European characters.  */
      for (i = 0; i < CHAR_TABLE_SINGLE_BYTE_SLOTS; i++)
	{
	  Lisp_Object elt = XCHAR_TABLE (subtable)->contents[i];
	  if (c_function)
	    (*c_function) (arg, make_number (i), elt);
	  else
	    call2 (function, make_number (i), elt);
	}
#if 0 /* If the char table has entries for higher characters,
	 we should report them.  */
      if (NILP (current_buffer->enable_multibyte_characters))
	return;
#endif
      to = CHAR_TABLE_ORDINARY_SLOTS;
    }
  else
    {
      i = 32;
      to = SUB_CHAR_TABLE_ORDINARY_SLOTS;
    }

  for (; i < to; i++)
    {
      Lisp_Object elt = XCHAR_TABLE (subtable)->contents[i];

      XSETFASTINT (indices[depth], i);

      if (SUB_CHAR_TABLE_P (elt))
	{
	  if (depth >= 3)
	    error ("Too deep char table");
	  map_char_table (c_function, function, elt, arg, depth + 1, indices);
	}
      else
	{
	  int charset = XFASTINT (indices[0]) - 128, c1, c2, c;

	  if (CHARSET_DEFINED_P (charset))
	    {
	      c1 = depth >= 1 ? XFASTINT (indices[1]) : 0;
	      c2 = depth >= 2 ? XFASTINT (indices[2]) : 0;
	      c = MAKE_NON_ASCII_CHAR (charset, c1, c2);
	      if (c_function)
		(*c_function) (arg, make_number (c), elt);
	      else
		call2 (function, make_number (c), elt);
	    }
  	}
    }
}

DEFUN ("map-char-table", Fmap_char_table, Smap_char_table,
  2, 2, 0,
  "Call FUNCTION for each (normal and generic) characters in CHAR-TABLE.\n\
FUNCTION is called with two arguments--a key and a value.\n\
The key is always a possible IDX argument to `aref'.")
  (function, char_table)
     Lisp_Object function, char_table;
{
  /* The depth of char table is at most 3. */
  Lisp_Object indices[3];

  CHECK_CHAR_TABLE (char_table, 1);

  map_char_table (NULL, function, char_table, char_table, 0, indices);
  return Qnil;
}

/* ARGSUSED */
Lisp_Object
nconc2 (s1, s2)
     Lisp_Object s1, s2;
{
#ifdef NO_ARG_ARRAY
  Lisp_Object args[2];
  args[0] = s1;
  args[1] = s2;
  return Fnconc (2, args);
#else
  return Fnconc (2, &s1);
#endif /* NO_ARG_ARRAY */
}

DEFUN ("nconc", Fnconc, Snconc, 0, MANY, 0,
  "Concatenate any number of lists by altering them.\n\
Only the last argument is not altered, and need not be a list.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  register int argnum;
  register Lisp_Object tail, tem, val;

  val = Qnil;

  for (argnum = 0; argnum < nargs; argnum++)
    {
      tem = args[argnum];
      if (NILP (tem)) continue;

      if (NILP (val))
	val = tem;

      if (argnum + 1 == nargs) break;

      if (!CONSP (tem))
	tem = wrong_type_argument (Qlistp, tem);

      while (CONSP (tem))
	{
	  tail = tem;
	  tem = Fcdr (tail);
	  QUIT;
	}

      tem = args[argnum + 1];
      Fsetcdr (tail, tem);
      if (NILP (tem))
	args[argnum + 1] = tail;
    }

  return val;
}

/* This is the guts of all mapping functions.
 Apply FN to each element of SEQ, one by one,
 storing the results into elements of VALS, a C vector of Lisp_Objects.
 LENI is the length of VALS, which should also be the length of SEQ.  */

static void
mapcar1 (leni, vals, fn, seq)
     int leni;
     Lisp_Object *vals;
     Lisp_Object fn, seq;
{
  register Lisp_Object tail;
  Lisp_Object dummy;
  register int i;
  struct gcpro gcpro1, gcpro2, gcpro3;

  /* Don't let vals contain any garbage when GC happens.  */
  for (i = 0; i < leni; i++)
    vals[i] = Qnil;

  GCPRO3 (dummy, fn, seq);
  gcpro1.var = vals;
  gcpro1.nvars = leni;
  /* We need not explicitly protect `tail' because it is used only on lists, and
    1) lists are not relocated and 2) the list is marked via `seq' so will not be freed */

  if (VECTORP (seq))
    {
      for (i = 0; i < leni; i++)
	{
	  dummy = XVECTOR (seq)->contents[i];
	  vals[i] = call1 (fn, dummy);
	}
    }
  else if (BOOL_VECTOR_P (seq))
    {
      for (i = 0; i < leni; i++)
	{
	  int byte;
	  byte = XBOOL_VECTOR (seq)->data[i / BITS_PER_CHAR];
	  if (byte & (1 << (i % BITS_PER_CHAR)))
	    dummy = Qt;
	  else
	    dummy = Qnil;

	  vals[i] = call1 (fn, dummy);
	}
    }
  else if (STRINGP (seq) && ! STRING_MULTIBYTE (seq))
    {
      /* Single-byte string.  */
      for (i = 0; i < leni; i++)
	{
	  XSETFASTINT (dummy, XSTRING (seq)->data[i]);
	  vals[i] = call1 (fn, dummy);
	}
    }
  else if (STRINGP (seq))
    {
      /* Multi-byte string.  */
      int len_byte = STRING_BYTES (XSTRING (seq));
      int i_byte;

      for (i = 0, i_byte = 0; i < leni;)
	{
	  int c;
	  int i_before = i;

	  FETCH_STRING_CHAR_ADVANCE (c, seq, i, i_byte);
	  XSETFASTINT (dummy, c);
	  vals[i_before] = call1 (fn, dummy);
	}
    }
  else   /* Must be a list, since Flength did not get an error */
    {
      tail = seq;
      for (i = 0; i < leni; i++)
	{
	  vals[i] = call1 (fn, Fcar (tail));
	  tail = XCONS (tail)->cdr;
	}
    }

  UNGCPRO;
}

DEFUN ("mapconcat", Fmapconcat, Smapconcat, 3, 3, 0,
  "Apply FUNCTION to each element of SEQUENCE, and concat the results as strings.\n\
In between each pair of results, stick in SEPARATOR.  Thus, \" \" as\n\
SEPARATOR results in spaces between the values returned by FUNCTION.\n\
SEQUENCE may be a list, a vector, a bool-vector, or a string.")
  (function, sequence, separator)
     Lisp_Object function, sequence, separator;
{
  Lisp_Object len;
  register int leni;
  int nargs;
  register Lisp_Object *args;
  register int i;
  struct gcpro gcpro1;

  len = Flength (sequence);
  leni = XINT (len);
  nargs = leni + leni - 1;
  if (nargs < 0) return build_string ("");

  args = (Lisp_Object *) alloca (nargs * sizeof (Lisp_Object));

  GCPRO1 (separator);
  mapcar1 (leni, args, function, sequence);
  UNGCPRO;

  for (i = leni - 1; i >= 0; i--)
    args[i + i] = args[i];

  for (i = 1; i < nargs; i += 2)
    args[i] = separator;

  return Fconcat (nargs, args);
}

DEFUN ("mapcar", Fmapcar, Smapcar, 2, 2, 0,
  "Apply FUNCTION to each element of SEQUENCE, and make a list of the results.\n\
The result is a list just as long as SEQUENCE.\n\
SEQUENCE may be a list, a vector, a bool-vector, or a string.")
  (function, sequence)
     Lisp_Object function, sequence;
{
  register Lisp_Object len;
  register int leni;
  register Lisp_Object *args;

  len = Flength (sequence);
  leni = XFASTINT (len);
  args = (Lisp_Object *) alloca (leni * sizeof (Lisp_Object));

  mapcar1 (leni, args, function, sequence);

  return Flist (leni, args);
}

/* Anything that calls this function must protect from GC!  */

DEFUN ("y-or-n-p", Fy_or_n_p, Sy_or_n_p, 1, 1, 0,
  "Ask user a \"y or n\" question.  Return t if answer is \"y\".\n\
Takes one argument, which is the string to display to ask the question.\n\
It should end in a space; `y-or-n-p' adds `(y or n) ' to it.\n\
No confirmation of the answer is requested; a single character is enough.\n\
Also accepts Space to mean yes, or Delete to mean no.")
  (prompt)
     Lisp_Object prompt;
{
  register Lisp_Object obj, key, def, answer_string, map;
  register int answer;
  Lisp_Object xprompt;
  Lisp_Object args[2];
  struct gcpro gcpro1, gcpro2;
  int count = specpdl_ptr - specpdl;

  specbind (Qcursor_in_echo_area, Qt);

  map = Fsymbol_value (intern ("query-replace-map"));

  CHECK_STRING (prompt, 0);
  xprompt = prompt;
  GCPRO2 (prompt, xprompt);

  while (1)
    {

#ifdef HAVE_MENUS
      if ((NILP (last_nonmenu_event) || CONSP (last_nonmenu_event))
	  && use_dialog_box
	  && have_menus_p ())
	{
	  Lisp_Object pane, menu;
	  redisplay_preserve_echo_area ();
	  pane = Fcons (Fcons (build_string ("Yes"), Qt),
			Fcons (Fcons (build_string ("No"), Qnil),
			       Qnil));
	  menu = Fcons (prompt, pane);
	  obj = Fx_popup_dialog (Qt, menu);
	  answer = !NILP (obj);
	  break;
	}
#endif /* HAVE_MENUS */
      cursor_in_echo_area = 1;
      choose_minibuf_frame ();
      message_with_string ("%s(y or n) ", xprompt, 0);

      if (minibuffer_auto_raise)
	{
	  Lisp_Object mini_frame;

	  mini_frame = WINDOW_FRAME (XWINDOW (minibuf_window));

	  Fraise_frame (mini_frame);
	}

      obj = read_filtered_event (1, 0, 0, 0);
      cursor_in_echo_area = 0;
      /* If we need to quit, quit with cursor_in_echo_area = 0.  */
      QUIT;

      key = Fmake_vector (make_number (1), obj);
      def = Flookup_key (map, key, Qt);
      answer_string = Fsingle_key_description (obj);

      if (EQ (def, intern ("skip")))
	{
	  answer = 0;
	  break;
	}
      else if (EQ (def, intern ("act")))
	{
	  answer = 1;
	  break;
	}
      else if (EQ (def, intern ("recenter")))
	{
	  Frecenter (Qnil);
	  xprompt = prompt;
	  continue;
	}
      else if (EQ (def, intern ("quit")))
	Vquit_flag = Qt;
      /* We want to exit this command for exit-prefix,
	 and this is the only way to do it.  */
      else if (EQ (def, intern ("exit-prefix")))
	Vquit_flag = Qt;

      QUIT;

      /* If we don't clear this, then the next call to read_char will
	 return quit_char again, and we'll enter an infinite loop.  */
      Vquit_flag = Qnil;

      Fding (Qnil);
      Fdiscard_input ();
      if (EQ (xprompt, prompt))
	{
	  args[0] = build_string ("Please answer y or n.  ");
	  args[1] = prompt;
	  xprompt = Fconcat (2, args);
	}
    }
  UNGCPRO;

  if (! noninteractive)
    {
      cursor_in_echo_area = -1;
      message_with_string (answer ? "%s(y or n) y" : "%s(y or n) n",
			   xprompt, 0);
    }

  unbind_to (count, Qnil);
  return answer ? Qt : Qnil;
}

/* This is how C code calls `yes-or-no-p' and allows the user
   to redefined it.

   Anything that calls this function must protect from GC!  */

Lisp_Object
do_yes_or_no_p (prompt)
     Lisp_Object prompt;
{
  return call1 (intern ("yes-or-no-p"), prompt);
}

/* Anything that calls this function must protect from GC!  */

DEFUN ("yes-or-no-p", Fyes_or_no_p, Syes_or_no_p, 1, 1, 0,
  "Ask user a yes-or-no question.  Return t if answer is yes.\n\
Takes one argument, which is the string to display to ask the question.\n\
It should end in a space; `yes-or-no-p' adds `(yes or no) ' to it.\n\
The user must confirm the answer with RET,\n\
and can edit it until it has been confirmed.")
  (prompt)
     Lisp_Object prompt;
{
  register Lisp_Object ans;
  Lisp_Object args[2];
  struct gcpro gcpro1;
  Lisp_Object menu;

  CHECK_STRING (prompt, 0);

#ifdef HAVE_MENUS
  if ((NILP (last_nonmenu_event) || CONSP (last_nonmenu_event))
      && use_dialog_box
      && have_menus_p ())
    {
      Lisp_Object pane, menu, obj;
      redisplay_preserve_echo_area ();
      pane = Fcons (Fcons (build_string ("Yes"), Qt),
		    Fcons (Fcons (build_string ("No"), Qnil),
			   Qnil));
      GCPRO1 (pane);
      menu = Fcons (prompt, pane);
      obj = Fx_popup_dialog (Qt, menu);
      UNGCPRO;
      return obj;
    }
#endif /* HAVE_MENUS */

  args[0] = prompt;
  args[1] = build_string ("(yes or no) ");
  prompt = Fconcat (2, args);

  GCPRO1 (prompt);

  while (1)
    {
      ans = Fdowncase (Fread_from_minibuffer (prompt, Qnil, Qnil, Qnil,
					      Qyes_or_no_p_history, Qnil,
					      Qnil));
      if (XSTRING (ans)->size == 3 && !strcmp (XSTRING (ans)->data, "yes"))
	{
	  UNGCPRO;
	  return Qt;
	}
      if (XSTRING (ans)->size == 2 && !strcmp (XSTRING (ans)->data, "no"))
	{
	  UNGCPRO;
	  return Qnil;
	}

      Fding (Qnil);
      Fdiscard_input ();
      message ("Please answer yes or no.");
      Fsleep_for (make_number (2), Qnil);
    }
}

DEFUN ("load-average", Fload_average, Sload_average, 0, 1, 0,
  "Return list of 1 minute, 5 minute and 15 minute load averages.\n\
Each of the three load averages is multiplied by 100,\n\
then converted to integer.\n\
When USE-FLOATS is non-nil, floats will be used instead of integers.\n\
These floats are not multiplied by 100.\n\n\
If the 5-minute or 15-minute load averages are not available, return a\n\
shortened list, containing only those averages which are available.")
  (use_floats)
     Lisp_Object use_floats;
{
  double load_ave[3];
  int loads = getloadavg (load_ave, 3);
  Lisp_Object ret = Qnil;

  if (loads < 0)
    error ("load-average not implemented for this operating system");

  while (loads-- > 0)
    {
      Lisp_Object load = (NILP (use_floats) ?
			  make_number ((int) (100.0 * load_ave[loads]))
			  : make_float (load_ave[loads]));
      ret = Fcons (load, ret);
    }

  return ret;
}

Lisp_Object Vfeatures;

DEFUN ("featurep", Ffeaturep, Sfeaturep, 1, 1, 0,
  "Returns t if FEATURE is present in this Emacs.\n\
Use this to conditionalize execution of lisp code based on the presence or\n\
absence of emacs or environment extensions.\n\
Use `provide' to declare that a feature is available.\n\
This function looks at the value of the variable `features'.")
  (feature)
     Lisp_Object feature;
{
  register Lisp_Object tem;
  CHECK_SYMBOL (feature, 0);
  tem = Fmemq (feature, Vfeatures);
  return (NILP (tem)) ? Qnil : Qt;
}

DEFUN ("provide", Fprovide, Sprovide, 1, 1, 0,
  "Announce that FEATURE is a feature of the current Emacs.")
  (feature)
     Lisp_Object feature;
{
  register Lisp_Object tem;
  CHECK_SYMBOL (feature, 0);
  if (!NILP (Vautoload_queue))
    Vautoload_queue = Fcons (Fcons (Vfeatures, Qnil), Vautoload_queue);
  tem = Fmemq (feature, Vfeatures);
  if (NILP (tem))
    Vfeatures = Fcons (feature, Vfeatures);
  LOADHIST_ATTACH (Fcons (Qprovide, feature));
  return feature;
}

DEFUN ("require", Frequire, Srequire, 1, 3, 0,
  "If feature FEATURE is not loaded, load it from FILENAME.\n\
If FEATURE is not a member of the list `features', then the feature\n\
is not loaded; so load the file FILENAME.\n\
If FILENAME is omitted, the printname of FEATURE is used as the file name,\n\
but in this case `load' insists on adding the suffix `.el' or `.elc'.\n\
If the optional third argument NOERROR is non-nil,\n\
then return nil if the file is not found.\n\
Normally the return value is FEATURE.")
  (feature, file_name, noerror)
     Lisp_Object feature, file_name, noerror;
{
  register Lisp_Object tem;
  CHECK_SYMBOL (feature, 0);
  tem = Fmemq (feature, Vfeatures);
  LOADHIST_ATTACH (Fcons (Qrequire, feature));
  if (NILP (tem))
    {
      int count = specpdl_ptr - specpdl;

      /* Value saved here is to be restored into Vautoload_queue */
      record_unwind_protect (un_autoload, Vautoload_queue);
      Vautoload_queue = Qt;

      tem = Fload (NILP (file_name) ? Fsymbol_name (feature) : file_name,
		     noerror, Qt, Qnil, (NILP (file_name) ? Qt : Qnil));
      /* If load failed entirely, return nil.  */
      if (NILP (tem))
	return unbind_to (count, Qnil);

      tem = Fmemq (feature, Vfeatures);
      if (NILP (tem))
	error ("Required feature %s was not provided",
	       XSYMBOL (feature)->name->data);

      /* Once loading finishes, don't undo it.  */
      Vautoload_queue = Qt;
      feature = unbind_to (count, feature);
    }
  return feature;
}

/* Primitives for work of the "widget" library.
   In an ideal world, this section would not have been necessary.
   However, lisp function calls being as slow as they are, it turns
   out that some functions in the widget library (wid-edit.el) are the
   bottleneck of Widget operation.  Here is their translation to C,
   for the sole reason of efficiency.  */

DEFUN ("widget-plist-member", Fwidget_plist_member, Swidget_plist_member, 2, 2, 0,
  "Return non-nil if PLIST has the property PROP.\n\
PLIST is a property list, which is a list of the form\n\
\(PROP1 VALUE1 PROP2 VALUE2 ...\).  PROP is a symbol.\n\
Unlike `plist-get', this allows you to distinguish between a missing\n\
property and a property with the value nil.\n\
The value is actually the tail of PLIST whose car is PROP.")
  (plist, prop)
     Lisp_Object plist, prop;
{
  while (CONSP (plist) && !EQ (XCAR (plist), prop))
    {
      QUIT;
      plist = XCDR (plist);
      plist = CDR (plist);
    }
  return plist;
}

DEFUN ("widget-put", Fwidget_put, Swidget_put, 3, 3, 0,
  "In WIDGET, set PROPERTY to VALUE.\n\
The value can later be retrieved with `widget-get'.")
  (widget, property, value)
     Lisp_Object widget, property, value;
{
  CHECK_CONS (widget, 1);
  XCDR (widget) = Fplist_put (XCDR (widget), property, value);
  return value;
}

DEFUN ("widget-get", Fwidget_get, Swidget_get, 2, 2, 0,
  "In WIDGET, get the value of PROPERTY.\n\
The value could either be specified when the widget was created, or\n\
later with `widget-put'.")
  (widget, property)
     Lisp_Object widget, property;
{
  Lisp_Object tmp;

  while (1)
    {
      if (NILP (widget))
	return Qnil;
      CHECK_CONS (widget, 1);
      tmp = Fwidget_plist_member (XCDR (widget), property);
      if (CONSP (tmp))
	{
	  tmp = XCDR (tmp);
	  return CAR (tmp);
	}
      tmp = XCAR (widget);
      if (NILP (tmp))
	return Qnil;
      widget = Fget (tmp, Qwidget_type);
    }
}

DEFUN ("widget-apply", Fwidget_apply, Swidget_apply, 2, MANY, 0,
  "Apply the value of WIDGET's PROPERTY to the widget itself.\n\
ARGS are passed as extra arguments to the function.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  /* This function can GC. */
  Lisp_Object newargs[3];
  struct gcpro gcpro1, gcpro2;
  Lisp_Object result;

  newargs[0] = Fwidget_get (args[0], args[1]);
  newargs[1] = args[0];
  newargs[2] = Flist (nargs - 2, args + 2);
  GCPRO2 (newargs[0], newargs[2]);
  result = Fapply (3, newargs);
  UNGCPRO;
  return result;
}

/* base64 encode/decode functions.
   Based on code from GNU recode. */

#define MIME_LINE_LENGTH 76

#define IS_ASCII(Character) \
  ((Character) < 128)
#define IS_BASE64(Character) \
  (IS_ASCII (Character) && base64_char_to_value[Character] >= 0)

/* Don't use alloca for regions larger than this, lest we overflow
   their stack.  */
#define MAX_ALLOCA 16*1024

/* Table of characters coding the 64 values.  */
static char base64_value_to_char[64] =
{
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',	/*  0- 9 */
  'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',	/* 10-19 */
  'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd',	/* 20-29 */
  'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',	/* 30-39 */
  'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x',	/* 40-49 */
  'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7',	/* 50-59 */
  '8', '9', '+', '/'					/* 60-63 */
};

/* Table of base64 values for first 128 characters.  */
static short base64_char_to_value[128] =
{
  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,	/*   0-  9 */
  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,	/*  10- 19 */
  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,	/*  20- 29 */
  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,	/*  30- 39 */
  -1,  -1,  -1,  62,  -1,  -1,  -1,  63,  52,  53,	/*  40- 49 */
  54,  55,  56,  57,  58,  59,  60,  61,  -1,  -1,	/*  50- 59 */
  -1,  -1,  -1,  -1,  -1,  0,   1,   2,   3,   4,	/*  60- 69 */
  5,   6,   7,   8,   9,   10,  11,  12,  13,  14,	/*  70- 79 */
  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,	/*  80- 89 */
  25,  -1,  -1,  -1,  -1,  -1,  -1,  26,  27,  28,	/*  90- 99 */
  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,	/* 100-109 */
  39,  40,  41,  42,  43,  44,  45,  46,  47,  48,	/* 110-119 */
  49,  50,  51,  -1,  -1,  -1,  -1,  -1			/* 120-127 */
};

/* The following diagram shows the logical steps by which three octets
   get transformed into four base64 characters.

		 .--------.  .--------.  .--------.
		 |aaaaaabb|  |bbbbcccc|  |ccdddddd|
		 `--------'  `--------'  `--------'
                    6   2      4   4       2   6
	       .--------+--------+--------+--------.
	       |00aaaaaa|00bbbbbb|00cccccc|00dddddd|
	       `--------+--------+--------+--------'

	       .--------+--------+--------+--------.
	       |AAAAAAAA|BBBBBBBB|CCCCCCCC|DDDDDDDD|
	       `--------+--------+--------+--------'

   The octets are divided into 6 bit chunks, which are then encoded into
   base64 characters.  */


static int base64_encode_1 P_ ((const char *, char *, int, int));
static int base64_decode_1 P_ ((const char *, char *, int));

DEFUN ("base64-encode-region", Fbase64_encode_region, Sbase64_encode_region,
       2, 3, "r",
       "Base64-encode the region between BEG and END.\n\
Return the length of the encoded text.\n\
Optional third argument NO-LINE-BREAK means do not break long lines\n\
into shorter lines.")
     (beg, end, no_line_break)
     Lisp_Object beg, end, no_line_break;
{
  char *encoded;
  int allength, length;
  int ibeg, iend, encoded_length;
  int old_pos = PT;

  validate_region (&beg, &end);

  ibeg = CHAR_TO_BYTE (XFASTINT (beg));
  iend = CHAR_TO_BYTE (XFASTINT (end));
  move_gap_both (XFASTINT (beg), ibeg);

  /* We need to allocate enough room for encoding the text.
     We need 33 1/3% more space, plus a newline every 76
     characters, and then we round up. */
  length = iend - ibeg;
  allength = length + length/3 + 1;
  allength += allength / MIME_LINE_LENGTH + 1 + 6;

  if (allength <= MAX_ALLOCA)
    encoded = (char *) alloca (allength);
  else
    encoded = (char *) xmalloc (allength);
  encoded_length = base64_encode_1 (BYTE_POS_ADDR (ibeg), encoded, length,
				    NILP (no_line_break));
  if (encoded_length > allength)
    abort ();

  /* Now we have encoded the region, so we insert the new contents
     and delete the old.  (Insert first in order to preserve markers.)  */
  SET_PT_BOTH (XFASTINT (beg), ibeg);
  insert (encoded, encoded_length);
  if (allength > MAX_ALLOCA)
    xfree (encoded);
  del_range_byte (ibeg + encoded_length, iend + encoded_length, 1);

  /* If point was outside of the region, restore it exactly; else just
     move to the beginning of the region.  */
  if (old_pos >= XFASTINT (end))
    old_pos += encoded_length - (XFASTINT (end) - XFASTINT (beg));
  else if (old_pos > XFASTINT (beg))
    old_pos = XFASTINT (beg);
  SET_PT (old_pos);

  /* We return the length of the encoded text. */
  return make_number (encoded_length);
}

DEFUN ("base64-encode-string", Fbase64_encode_string, Sbase64_encode_string,
       1, 1, 0,
       "Base64-encode STRING and return the result.")
     (string)
     Lisp_Object string;
{
  int allength, length, encoded_length;
  char *encoded;
  Lisp_Object encoded_string;

  CHECK_STRING (string, 1);

  length = STRING_BYTES (XSTRING (string));
  allength = length + length/3 + 1 + 6;

  /* We need to allocate enough room for decoding the text. */
  if (allength <= MAX_ALLOCA)
    encoded = (char *) alloca (allength);
  else
    encoded = (char *) xmalloc (allength);

  encoded_length = base64_encode_1 (XSTRING (string)->data,
				    encoded, length, 0);
  if (encoded_length > allength)
    abort ();

  encoded_string = make_unibyte_string (encoded, encoded_length);
  if (allength > MAX_ALLOCA)
    xfree (encoded);

  return encoded_string;
}

static int
base64_encode_1 (from, to, length, line_break)
     const char *from;
     char *to;
     int length;
     int line_break;
{
  int counter = 0, i = 0;
  char *e = to;
  unsigned char c;
  unsigned int value;

  while (i < length)
    {
      c = from[i++];

      /* Wrap line every 76 characters.  */

      if (line_break)
	{
	  if (counter < MIME_LINE_LENGTH / 4)
	    counter++;
	  else
	    {
	      *e++ = '\n';
	      counter = 1;
	    }
	}

      /* Process first byte of a triplet.  */

      *e++ = base64_value_to_char[0x3f & c >> 2];
      value = (0x03 & c) << 4;

      /* Process second byte of a triplet.  */

      if (i == length)
	{
	  *e++ = base64_value_to_char[value];
	  *e++ = '=';
	  *e++ = '=';
	  break;
	}

      c = from[i++];

      *e++ = base64_value_to_char[value | (0x0f & c >> 4)];
      value = (0x0f & c) << 2;

      /* Process third byte of a triplet.  */

      if (i == length)
	{
	  *e++ = base64_value_to_char[value];
	  *e++ = '=';
	  break;
	}

      c = from[i++];

      *e++ = base64_value_to_char[value | (0x03 & c >> 6)];
      *e++ = base64_value_to_char[0x3f & c];
    }

  /* Complete last partial line.  */

  if (line_break)
    if (counter > 0)
      *e++ = '\n';

  return e - to;
}


DEFUN ("base64-decode-region", Fbase64_decode_region, Sbase64_decode_region,
  2, 2, "r",
  "Base64-decode the region between BEG and END.\n\
Return the length of the decoded text.\n\
If the region can't be decoded, return nil and don't modify the buffer.")
     (beg, end)
     Lisp_Object beg, end;
{
  int ibeg, iend, length;
  char *decoded;
  int old_pos = PT;
  int decoded_length;
  int inserted_chars;

  validate_region (&beg, &end);

  ibeg = CHAR_TO_BYTE (XFASTINT (beg));
  iend = CHAR_TO_BYTE (XFASTINT (end));

  length = iend - ibeg;
  /* We need to allocate enough room for decoding the text. */
  if (length <= MAX_ALLOCA)
    decoded = (char *) alloca (length);
  else
    decoded = (char *) xmalloc (length);

  move_gap_both (XFASTINT (beg), ibeg);
  decoded_length = base64_decode_1 (BYTE_POS_ADDR (ibeg), decoded, length);
  if (decoded_length > length)
    abort ();

  if (decoded_length < 0)
    {
      /* The decoding wasn't possible. */
      if (length > MAX_ALLOCA)
	xfree (decoded);
      return Qnil;
    }

  /* Now we have decoded the region, so we insert the new contents
     and delete the old.  (Insert first in order to preserve markers.)  */
  /* We insert two spaces, then insert the decoded text in between
     them, at last, delete those extra two spaces.  This is to avoid
     byte combining while inserting.  */
  TEMP_SET_PT_BOTH (XFASTINT (beg), ibeg);
  insert_1_both ("  ", 2, 2, 0, 1, 0);
  TEMP_SET_PT_BOTH (XFASTINT (beg) + 1, ibeg + 1);  
  insert (decoded, decoded_length);
  inserted_chars = PT - (XFASTINT (beg) + 1);
  if (length > MAX_ALLOCA)
    xfree (decoded);
  /* At first delete the original text.  This never cause byte
     combining.  */
  del_range_both (PT + 1, PT_BYTE + 1, XFASTINT (end) + inserted_chars + 2,
		  iend + decoded_length + 2, 1);
  /* Next delete the extra spaces.  This will cause byte combining
     error.  */
  del_range_both (PT, PT_BYTE, PT + 1, PT_BYTE + 1, 0);
  del_range_both (XFASTINT (beg), ibeg, XFASTINT (beg) + 1, ibeg + 1, 0);
  inserted_chars = PT - XFASTINT (beg);

  /* If point was outside of the region, restore it exactly; else just
     move to the beginning of the region.  */
  if (old_pos >= XFASTINT (end))
    old_pos += inserted_chars - (XFASTINT (end) - XFASTINT (beg));
  else if (old_pos > XFASTINT (beg))
    old_pos = XFASTINT (beg);
  SET_PT (old_pos);

  return make_number (inserted_chars);
}

DEFUN ("base64-decode-string", Fbase64_decode_string, Sbase64_decode_string,
       1, 1, 0,
       "Base64-decode STRING and return the result.")
     (string)
     Lisp_Object string;
{
  char *decoded;
  int length, decoded_length;
  Lisp_Object decoded_string;

  CHECK_STRING (string, 1);

  length = STRING_BYTES (XSTRING (string));
  /* We need to allocate enough room for decoding the text. */
  if (length <= MAX_ALLOCA)
    decoded = (char *) alloca (length);
  else
    decoded = (char *) xmalloc (length);

  decoded_length = base64_decode_1 (XSTRING (string)->data, decoded, length);
  if (decoded_length > length)
    abort ();

  if (decoded_length < 0)
    /* The decoding wasn't possible. */
    decoded_string = Qnil;
  else
    decoded_string = make_string (decoded, decoded_length);

  if (length > MAX_ALLOCA)
    xfree (decoded);

  return decoded_string;
}

static int
base64_decode_1 (from, to, length)
     const char *from;
     char *to;
     int length;
{
  int counter = 0, i = 0;
  char *e = to;
  unsigned char c;
  unsigned long value;

  while (i < length)
    {
      /* Accept wrapping lines, reversibly if at each 76 characters.  */

      c = from[i++];
      if (c == '\n')
	{
	  if (i == length)
	    break;
	  c = from[i++];
	  if (i == length)
	    break;
	  counter = 1;
	}
      else
	counter++;

      /* Process first byte of a quadruplet.  */

      if (!IS_BASE64 (c))
	return -1;
      value = base64_char_to_value[c] << 18;

      /* Process second byte of a quadruplet.  */

      if (i == length)
	return -1;
      c = from[i++];

      if (!IS_BASE64 (c))
	return -1;
      value |= base64_char_to_value[c] << 12;

      *e++ = (unsigned char) (value >> 16);

      /* Process third byte of a quadruplet.  */

      if (i == length)
	return -1;
      c = from[i++];

      if (c == '=')
	{
	  c = from[i++];
	  if (c != '=')
	    return -1;
	  continue;
	}

      if (!IS_BASE64 (c))
	return -1;
      value |= base64_char_to_value[c] << 6;

      *e++ = (unsigned char) (0xff & value >> 8);

      /* Process fourth byte of a quadruplet.  */

      if (i == length)
	return -1;
      c = from[i++];

      if (c == '=')
	continue;

      if (!IS_BASE64 (c))
	return -1;
      value |= base64_char_to_value[c];

      *e++ = (unsigned char) (0xff & value);
    }

  return e - to;
}

void
syms_of_fns ()
{
  Qstring_lessp = intern ("string-lessp");
  staticpro (&Qstring_lessp);
  Qprovide = intern ("provide");
  staticpro (&Qprovide);
  Qrequire = intern ("require");
  staticpro (&Qrequire);
  Qyes_or_no_p_history = intern ("yes-or-no-p-history");
  staticpro (&Qyes_or_no_p_history);
  Qcursor_in_echo_area = intern ("cursor-in-echo-area");
  staticpro (&Qcursor_in_echo_area);
  Qwidget_type = intern ("widget-type");
  staticpro (&Qwidget_type);

  staticpro (&string_char_byte_cache_string);
  string_char_byte_cache_string = Qnil;

  Fset (Qyes_or_no_p_history, Qnil);

  DEFVAR_LISP ("features", &Vfeatures,
    "A list of symbols which are the features of the executing emacs.\n\
Used by `featurep' and `require', and altered by `provide'.");
  Vfeatures = Qnil;

  DEFVAR_BOOL ("use-dialog-box", &use_dialog_box,
    "*Non-nil means mouse commands use dialog boxes to ask questions.\n\
This applies to y-or-n and yes-or-no questions asked by commands\n\
invoked by mouse clicks and mouse menu items.");
  use_dialog_box = 1;

  defsubr (&Sidentity);
  defsubr (&Srandom);
  defsubr (&Slength);
  defsubr (&Ssafe_length);
  defsubr (&Sstring_bytes);
  defsubr (&Sstring_equal);
  defsubr (&Scompare_strings);
  defsubr (&Sstring_lessp);
  defsubr (&Sappend);
  defsubr (&Sconcat);
  defsubr (&Svconcat);
  defsubr (&Scopy_sequence);
  defsubr (&Sstring_make_multibyte);
  defsubr (&Sstring_make_unibyte);
  defsubr (&Sstring_as_multibyte);
  defsubr (&Sstring_as_unibyte);
  defsubr (&Scopy_alist);
  defsubr (&Ssubstring);
  defsubr (&Snthcdr);
  defsubr (&Snth);
  defsubr (&Selt);
  defsubr (&Smember);
  defsubr (&Smemq);
  defsubr (&Sassq);
  defsubr (&Sassoc);
  defsubr (&Srassq);
  defsubr (&Srassoc);
  defsubr (&Sdelq);
  defsubr (&Sdelete);
  defsubr (&Snreverse);
  defsubr (&Sreverse);
  defsubr (&Ssort);
  defsubr (&Splist_get);
  defsubr (&Sget);
  defsubr (&Splist_put);
  defsubr (&Sput);
  defsubr (&Sequal);
  defsubr (&Sfillarray);
  defsubr (&Schar_table_subtype);
  defsubr (&Schar_table_parent);
  defsubr (&Sset_char_table_parent);
  defsubr (&Schar_table_extra_slot);
  defsubr (&Sset_char_table_extra_slot);
  defsubr (&Schar_table_range);
  defsubr (&Sset_char_table_range);
  defsubr (&Sset_char_table_default);
  defsubr (&Smap_char_table);
  defsubr (&Snconc);
  defsubr (&Smapcar);
  defsubr (&Smapconcat);
  defsubr (&Sy_or_n_p);
  defsubr (&Syes_or_no_p);
  defsubr (&Sload_average);
  defsubr (&Sfeaturep);
  defsubr (&Srequire);
  defsubr (&Sprovide);
  defsubr (&Swidget_plist_member);
  defsubr (&Swidget_put);
  defsubr (&Swidget_get);
  defsubr (&Swidget_apply);
  defsubr (&Sbase64_encode_region);
  defsubr (&Sbase64_decode_region);
  defsubr (&Sbase64_encode_string);
  defsubr (&Sbase64_decode_string);
}
