/* Random utility Lisp functions.
   Copyright (C) 1985, 86, 87, 93, 94, 95, 97, 98, 99, 2000, 2001
   Free Software Foundation, Inc.

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
#include "keymap.h"
#include "intervals.h"
#include "frame.h"
#include "window.h"
#include "blockinput.h"
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
       doc: /* Return the argument unchanged.  */)
     (arg)
     Lisp_Object arg;
{
  return arg;
}

DEFUN ("random", Frandom, Srandom, 0, 1, 0,
       doc: /* Return a pseudo-random number.
All integers representable in Lisp are equally likely.
  On most systems, this is 28 bits' worth.
With positive integer argument N, return random number in interval [0,N).
With argument t, set the random number seed from the current time and pid. */)
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
       doc: /* Return the length of vector, list or string SEQUENCE.
A byte-code function object is also allowed.
If the string contains multibyte characters, this is not the necessarily
the number of bytes in the string; it is the number of characters.
To get the number of bytes, use `string-bytes'. */)
     (sequence)
     register Lisp_Object sequence;
{
  register Lisp_Object val;
  register int i;

 retry:
  if (STRINGP (sequence))
    XSETFASTINT (val, XSTRING (sequence)->size);
  else if (VECTORP (sequence))
    XSETFASTINT (val, XVECTOR (sequence)->size);
  else if (CHAR_TABLE_P (sequence))
    XSETFASTINT (val, MAX_CHAR);
  else if (BOOL_VECTOR_P (sequence))
    XSETFASTINT (val, XBOOL_VECTOR (sequence)->size);
  else if (COMPILEDP (sequence))
    XSETFASTINT (val, XVECTOR (sequence)->size & PSEUDOVECTOR_SIZE_MASK);
  else if (CONSP (sequence))
    {
      i = 0;
      while (CONSP (sequence))
	{
	  sequence = XCDR (sequence);
	  ++i;

	  if (!CONSP (sequence))
	    break;

	  sequence = XCDR (sequence);
	  ++i;
	  QUIT;
	}

      if (!NILP (sequence))
	wrong_type_argument (Qlistp, sequence);

      val = make_number (i);
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
       doc: /* Return the length of a list, but avoid error or infinite loop.
This function never gets an error.  If LIST is not really a list,
it returns 0.  If LIST is circular, it returns a finite value
which is at least the number of distinct elements. */)
     (list)
     Lisp_Object list;
{
  Lisp_Object tail, halftail, length;
  int len = 0;

  /* halftail is used to detect circular lists.  */
  halftail = list;
  for (tail = list; CONSP (tail); tail = XCDR (tail))
    {
      if (EQ (tail, halftail) && len != 0)
	break;
      len++;
      if ((len & 1) == 0)
	halftail = XCDR (halftail);
    }

  XSETINT (length, len);
  return length;
}

DEFUN ("string-bytes", Fstring_bytes, Sstring_bytes, 1, 1, 0, 
       doc: /* Return the number of bytes in STRING.
If STRING is a multibyte string, this is greater than the length of STRING. */)
     (string)
     Lisp_Object string;
{
  CHECK_STRING (string);
  return make_number (STRING_BYTES (XSTRING (string)));
}

DEFUN ("string-equal", Fstring_equal, Sstring_equal, 2, 2, 0,
       doc: /* Return t if two strings have identical contents.
Case is significant, but text properties are ignored.
Symbols are also allowed; their print names are used instead. */)
     (s1, s2)
     register Lisp_Object s1, s2;
{
  if (SYMBOLP (s1))
    s1 = SYMBOL_NAME (s1);
  if (SYMBOLP (s2))
    s2 = SYMBOL_NAME (s2);
  CHECK_STRING (s1);
  CHECK_STRING (s2);

  if (XSTRING (s1)->size != XSTRING (s2)->size
      || STRING_BYTES (XSTRING (s1)) != STRING_BYTES (XSTRING (s2))
      || bcmp (XSTRING (s1)->data, XSTRING (s2)->data, STRING_BYTES (XSTRING (s1))))
    return Qnil;
  return Qt;
}

DEFUN ("compare-strings", Fcompare_strings,
       Scompare_strings, 6, 7, 0,
doc: /* Compare the contents of two strings, converting to multibyte if needed.
In string STR1, skip the first START1 characters and stop at END1.
In string STR2, skip the first START2 characters and stop at END2.
END1 and END2 default to the full lengths of the respective strings.

Case is significant in this comparison if IGNORE-CASE is nil.
Unibyte strings are converted to multibyte for comparison.

The value is t if the strings (or specified portions) match.
If string STR1 is less, the value is a negative number N;
  - 1 - N is the number of characters that match at the beginning.
If string STR1 is greater, the value is a positive number N;
  N - 1 is the number of characters that match at the beginning. */)
     (str1, start1, end1, str2, start2, end2, ignore_case)
     Lisp_Object str1, start1, end1, start2, str2, end2, ignore_case;
{
  register int end1_char, end2_char;
  register int i1, i1_byte, i2, i2_byte;

  CHECK_STRING (str1);
  CHECK_STRING (str2);
  if (NILP (start1))
    start1 = make_number (0);
  if (NILP (start2))
    start2 = make_number (0);
  CHECK_NATNUM (start1);
  CHECK_NATNUM (start2);
  if (! NILP (end1))
    CHECK_NATNUM (end1);
  if (! NILP (end2))
    CHECK_NATNUM (end2);

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
	FETCH_STRING_CHAR_ADVANCE_NO_CHECK (c1, str1, i1, i1_byte);
      else
	{
	  c1 = XSTRING (str1)->data[i1++];
	  c1 = unibyte_char_to_multibyte (c1);
	}

      if (STRING_MULTIBYTE (str2))
	FETCH_STRING_CHAR_ADVANCE_NO_CHECK (c2, str2, i2, i2_byte);
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
	return make_number (- i1 + XINT (start1));
      else
	return make_number (i1 - XINT (start1));
    }

  if (i1 < end1_char)
    return make_number (i1 - XINT (start1) + 1);
  if (i2 < end2_char)
    return make_number (- i1 + XINT (start1) - 1);

  return Qt;
}

DEFUN ("string-lessp", Fstring_lessp, Sstring_lessp, 2, 2, 0,
       doc: /* Return t if first arg string is less than second in lexicographic order.
Case is significant.
Symbols are also allowed; their print names are used instead. */)
     (s1, s2)
     register Lisp_Object s1, s2;
{
  register int end;
  register int i1, i1_byte, i2, i2_byte;

  if (SYMBOLP (s1))
    s1 = SYMBOL_NAME (s1);
  if (SYMBOLP (s2))
    s2 = SYMBOL_NAME (s2);
  CHECK_STRING (s1);
  CHECK_STRING (s2);

  i1 = i1_byte = i2 = i2_byte = 0;

  end = XSTRING (s1)->size;
  if (end > XSTRING (s2)->size)
    end = XSTRING (s2)->size;

  while (i1 < end)
    {
      /* When we find a mismatch, we must compare the
	 characters, not just the bytes.  */
      int c1, c2;

      FETCH_STRING_CHAR_ADVANCE (c1, s1, i1, i1_byte);
      FETCH_STRING_CHAR_ADVANCE (c2, s2, i2, i2_byte);

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
       doc: /* Concatenate all the arguments and make the result a list.
The result is a list whose elements are the elements of all the arguments.
Each argument may be a list, vector or string.
The last argument is not copied, just used as the tail of the new list.
usage: (append &rest SEQUENCES)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return concat (nargs, args, Lisp_Cons, 1);
}

DEFUN ("concat", Fconcat, Sconcat, 0, MANY, 0,
       doc: /* Concatenate all the arguments and make the result a string.
The result is a string whose elements are the elements of all the arguments.
Each argument may be a string or a list or vector of characters (integers).
usage: (concat &rest SEQUENCES)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return concat (nargs, args, Lisp_String, 0);
}

DEFUN ("vconcat", Fvconcat, Svconcat, 0, MANY, 0,
       doc: /* Concatenate all the arguments and make the result a vector.
The result is a vector whose elements are the elements of all the arguments.
Each argument may be a list, vector or string.
usage: (vconcat &rest SEQUENCES)   */)
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
       doc: /* Return a copy of a list, vector or string.
The elements of a list or vector are not copied; they are shared
with the original. */)
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

/* In string STR of length LEN, see if bytes before STR[I] combine
   with bytes after STR[I] to form a single character.  If so, return
   the number of bytes after STR[I] which combine in this way.
   Otherwize, return 0.  */

static int
count_combining (str, len, i)
     unsigned char *str;
     int len, i;
{
  int j = i - 1, bytes;

  if (i == 0 || i == len || CHAR_HEAD_P (str[i]))
    return 0;
  while (j >= 0 && !CHAR_HEAD_P (str[j])) j--;
  if (j < 0 || ! BASE_LEADING_CODE_P (str[j]))
    return 0;
  PARSE_MULTIBYTE_SEQ (str + j, len - j, bytes);
  return (bytes <= i - j ? 0 : bytes - (i - j));
}

/* This structure holds information of an argument of `concat' that is
   a string and has text properties to be copied.  */
struct textprop_rec
{
  int argnum;			/* refer to ARGS (arguments of `concat') */
  int from;			/* refer to ARGS[argnum] (argument string) */
  int to;			/* refer to VAL (the target string) */
};

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
  int toindex_byte = 0;
  register int result_len;
  register int result_len_byte;
  register int argnum;
  Lisp_Object last_tail;
  Lisp_Object prev;
  int some_multibyte;
  /* When we make a multibyte string, we can't copy text properties
     while concatinating each string because the length of resulting
     string can't be decided until we finish the whole concatination.
     So, we record strings that have text properties to be copied
     here, and copy the text properties after the concatination.  */
  struct textprop_rec  *textprops = NULL;
  /* Number of elments in textprops.  */
  int num_textprops = 0;

  tail = Qnil;

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
		if (!SINGLE_BYTE_CHAR_P (XINT (ch)))
		  some_multibyte = 1;
	      }
	  else if (BOOL_VECTOR_P (this) && XBOOL_VECTOR (this)->size > 0)
	    wrong_type_argument (Qintegerp, Faref (this, make_number (0)));
	  else if (CONSP (this))
	    for (; CONSP (this); this = XCDR (this))
	      {
		ch = XCAR (this);
		if (! INTEGERP (ch))
		  wrong_type_argument (Qintegerp, ch);
		this_len_byte = CHAR_BYTES (XINT (ch));
		result_len_byte += this_len_byte;
		if (!SINGLE_BYTE_CHAR_P (XINT (ch)))
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
    tail = val, toindex = -1; /* -1 in toindex is flag we are making a list */
  else
    toindex = 0, toindex_byte = 0;

  prev = Qnil;
  if (STRINGP (val))
    textprops
      = (struct textprop_rec *) alloca (sizeof (struct textprop_rec) * nargs);

  for (argnum = 0; argnum < nargs; argnum++)
    {
      Lisp_Object thislen;
      int thisleni = 0;
      register unsigned int thisindex = 0;
      register unsigned int thisindex_byte = 0;

      this = args[argnum];
      if (!CONSP (this))
	thislen = Flength (this), thisleni = XINT (thislen);

      /* Between strings of the same kind, copy fast.  */
      if (STRINGP (this) && STRINGP (val)
	  && STRING_MULTIBYTE (this) == some_multibyte)
	{
	  int thislen_byte = STRING_BYTES (XSTRING (this));
	  int combined;

	  bcopy (XSTRING (this)->data, XSTRING (val)->data + toindex_byte,
		 STRING_BYTES (XSTRING (this)));
	  combined =  (some_multibyte && toindex_byte > 0
		       ? count_combining (XSTRING (val)->data,
					  toindex_byte + thislen_byte,
					  toindex_byte)
		       : 0);
	  if (! NULL_INTERVAL_P (XSTRING (this)->intervals))
	    {
	      textprops[num_textprops].argnum = argnum;
	      /* We ignore text properties on characters being combined.  */
	      textprops[num_textprops].from = combined;
	      textprops[num_textprops++].to = toindex;
	    }
	  toindex_byte += thislen_byte;
	  toindex += thisleni - combined;
	  XSTRING (val)->size -= combined;
	}
      /* Copy a single-byte string to a multibyte string.  */
      else if (STRINGP (this) && STRINGP (val))
	{
	  if (! NULL_INTERVAL_P (XSTRING (this)->intervals))
	    {
	      textprops[num_textprops].argnum = argnum;
	      textprops[num_textprops].from = 0;
	      textprops[num_textprops++].to = toindex;
	    }
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
	      elt = XCAR (this), this = XCDR (this);
	    else if (thisindex >= thisleni)
	      break;
	    else if (STRINGP (this))
	      {
		int c;
		if (STRING_MULTIBYTE (this))
		  {
		    FETCH_STRING_CHAR_ADVANCE_NO_CHECK (c, this,
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
		XSETCAR (tail, elt);
		prev = tail;
		tail = XCDR (tail);
	      }
	    else if (VECTORP (val))
	      XVECTOR (val)->contents[toindex++] = elt;
	    else
	      {
		CHECK_NUMBER (elt);
		if (SINGLE_BYTE_CHAR_P (XINT (elt)))
		  {
		    if (some_multibyte)
		      toindex_byte
			+= CHAR_STRING (XINT (elt),
					XSTRING (val)->data + toindex_byte);
		    else
		      XSTRING (val)->data[toindex_byte++] = XINT (elt);
		    if (some_multibyte
			&& toindex_byte > 0
			&& count_combining (XSTRING (val)->data,
					    toindex_byte, toindex_byte - 1))
		      XSTRING (val)->size--;
		    else
		      toindex++;
		  }
		else
		  /* If we have any multibyte characters,
		     we already decided to make a multibyte string.  */
		  {
		    int c = XINT (elt);
		    /* P exists as a variable
		       to avoid a bug on the Masscomp C compiler.  */
		    unsigned char *p = & XSTRING (val)->data[toindex_byte];

		    toindex_byte += CHAR_STRING (c, p);
		    toindex++;
		  }
	      }
	  }
    }
  if (!NILP (prev))
    XSETCDR (prev, last_tail);

  if (num_textprops > 0)
    {
      Lisp_Object props;
      int last_to_end = -1;

      for (argnum = 0; argnum < num_textprops; argnum++)
	{
	  this = args[textprops[argnum].argnum];
	  props = text_property_list (this,
				      make_number (0),
				      make_number (XSTRING (this)->size),
				      Qnil);
	  /* If successive arguments have properites, be sure that the
	     value of `composition' property be the copy.  */
	  if (last_to_end == textprops[argnum].to)
	    make_composition_value_copy (props);
	  add_text_properties_from_list (val, props,
					 make_number (textprops[argnum].to));
	  last_to_end = textprops[argnum].to + XSTRING (this)->size;
	}
    }
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
	  FETCH_STRING_CHAR_ADVANCE_NO_CHECK (c, string,
					      best_below, best_below_byte);
	}
      i = best_below;
      i_byte = best_below_byte;
    }
  else
    {
      while (best_above > char_index)
	{
	  unsigned char *pend = XSTRING (string)->data + best_above_byte;
	  unsigned char *pbeg = pend - best_above_byte;
	  unsigned char *p = pend - 1;
	  int bytes;

	  while (p > pbeg  && !CHAR_HEAD_P (*p)) p--;
	  PARSE_MULTIBYTE_SEQ (p, pend - p, bytes);
	  if (bytes == pend - p)
	    best_above_byte -= bytes;
	  else if (bytes > pend - p)
	    best_above_byte -= (pend - p);
	  else
	    best_above_byte--;
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
	  FETCH_STRING_CHAR_ADVANCE_NO_CHECK (c, string,
					      best_below, best_below_byte);
	}
      i = best_below;
      i_byte = best_below_byte;
    }
  else
    {
      while (best_above_byte > byte_index)
	{
	  unsigned char *pend = XSTRING (string)->data + best_above_byte;
	  unsigned char *pbeg = pend - best_above_byte;
	  unsigned char *p = pend - 1;
	  int bytes;

	  while (p > pbeg  && !CHAR_HEAD_P (*p)) p--;
	  PARSE_MULTIBYTE_SEQ (p, pend - p, bytes);
	  if (bytes == pend - p)
	    best_above_byte -= bytes;
	  else if (bytes > pend - p)
	    best_above_byte -= (pend - p);
	  else
	    best_above_byte--;
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
       doc: /* Return the multibyte equivalent of STRING.
The function `unibyte-char-to-multibyte' is used to convert
each unibyte character to a multibyte character. */)
     (string)
     Lisp_Object string;
{
  CHECK_STRING (string);

  return string_make_multibyte (string);
}

DEFUN ("string-make-unibyte", Fstring_make_unibyte, Sstring_make_unibyte,
       1, 1, 0,
       doc: /* Return the unibyte equivalent of STRING.
Multibyte character codes are converted to unibyte
by using just the low 8 bits. */)
     (string)
     Lisp_Object string;
{
  CHECK_STRING (string);

  return string_make_unibyte (string);
}

DEFUN ("string-as-unibyte", Fstring_as_unibyte, Sstring_as_unibyte,
       1, 1, 0,
       doc: /* Return a unibyte string with the same individual bytes as STRING.
If STRING is unibyte, the result is STRING itself.
Otherwise it is a newly created string, with no text properties.
If STRING is multibyte and contains a character of charset
`eight-bit-control' or `eight-bit-graphic', it is converted to the
corresponding single byte.  */)
     (string)
     Lisp_Object string;
{
  CHECK_STRING (string);

  if (STRING_MULTIBYTE (string))
    {
      int bytes = STRING_BYTES (XSTRING (string));
      unsigned char *str = (unsigned char *) xmalloc (bytes);

      bcopy (XSTRING (string)->data, str, bytes);
      bytes = str_as_unibyte (str, bytes);
      string = make_unibyte_string (str, bytes);
      xfree (str);
    }
  return string;
}

DEFUN ("string-as-multibyte", Fstring_as_multibyte, Sstring_as_multibyte,
       1, 1, 0,
       doc: /* Return a multibyte string with the same individual bytes as STRING.
If STRING is multibyte, the result is STRING itself.
Otherwise it is a newly created string, with no text properties.
If STRING is unibyte and contains an individual 8-bit byte (i.e. not
part of a multibyte form), it is converted to the corresponding
multibyte character of charset `eight-bit-control' or `eight-bit-graphic'.  */)
     (string)
     Lisp_Object string;
{
  CHECK_STRING (string);

  if (! STRING_MULTIBYTE (string))
    {
      Lisp_Object new_string;
      int nchars, nbytes;

      parse_str_as_multibyte (XSTRING (string)->data,
			      STRING_BYTES (XSTRING (string)),
			      &nchars, &nbytes);
      new_string = make_uninit_multibyte_string (nchars, nbytes);
      bcopy (XSTRING (string)->data, XSTRING (new_string)->data,
	     STRING_BYTES (XSTRING (string)));
      if (nbytes != STRING_BYTES (XSTRING (string)))
	str_as_multibyte (XSTRING (new_string)->data, nbytes,
			  STRING_BYTES (XSTRING (string)), NULL);
      string = new_string;
      XSTRING (string)->intervals = NULL_INTERVAL;
    }
  return string;
}

DEFUN ("copy-alist", Fcopy_alist, Scopy_alist, 1, 1, 0,
       doc: /* Return a copy of ALIST.
This is an alist which represents the same mapping from objects to objects,
but does not share the alist structure with ALIST.
The objects mapped (cars and cdrs of elements of the alist)
are shared, however.
Elements of ALIST that are not conses are also shared.  */)
     (alist)
     Lisp_Object alist;
{
  register Lisp_Object tem;

  CHECK_LIST (alist);
  if (NILP (alist))
    return alist;
  alist = concat (1, &alist, Lisp_Cons, 0);
  for (tem = alist; CONSP (tem); tem = XCDR (tem))
    {
      register Lisp_Object car;
      car = XCAR (tem);

      if (CONSP (car))
	XSETCAR (tem, Fcons (XCAR (car), XCDR (car)));
    }
  return alist;
}

DEFUN ("substring", Fsubstring, Ssubstring, 2, 3, 0,
       doc: /* Return a substring of STRING, starting at index FROM and ending before TO.
TO may be nil or omitted; then the substring runs to the end of STRING.
If FROM or TO is negative, it counts from the end.

This function allows vectors as well as strings.  */)
     (string, from, to)
     Lisp_Object string;
     register Lisp_Object from, to;
{
  Lisp_Object res;
  int size;
  int size_byte = 0;
  int from_char, to_char;
  int from_byte = 0, to_byte = 0;

  if (! (STRINGP (string) || VECTORP (string)))
    wrong_type_argument (Qarrayp, string);

  CHECK_NUMBER (from);

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
      CHECK_NUMBER (to);

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


DEFUN ("substring-no-properties", Fsubstring_no_properties, Ssubstring_no_properties, 1, 3, 0,
       doc: /* Return a substring of STRING, without text properties.
It starts at index FROM and ending before TO.
TO may be nil or omitted; then the substring runs to the end of STRING.
If FROM is nil or omitted, the substring starts at the beginning of STRING.
If FROM or TO is negative, it counts from the end.

With one argument, just copy STRING without its properties.  */)
     (string, from, to)
     Lisp_Object string;
     register Lisp_Object from, to;
{
  int size, size_byte;
  int from_char, to_char;
  int from_byte, to_byte;

  CHECK_STRING (string);

  size = XSTRING (string)->size;
  size_byte = STRING_BYTES (XSTRING (string));

  if (NILP (from))
    from_char = from_byte = 0;
  else
    {
      CHECK_NUMBER (from);
      from_char = XINT (from);
      if (from_char < 0)
	from_char += size;

      from_byte = string_char_to_byte (string, from_char);
    }

  if (NILP (to))
    {
      to_char = size;
      to_byte = size_byte;
    }
  else
    {
      CHECK_NUMBER (to);

      to_char = XINT (to);
      if (to_char < 0)
	to_char += size;

      to_byte = string_char_to_byte (string, to_char);
    }

  if (!(0 <= from_char && from_char <= to_char && to_char <= size))
    args_out_of_range_3 (string, make_number (from_char),
			 make_number (to_char));

  return make_specified_string (XSTRING (string)->data + from_byte,
				to_char - from_char, to_byte - from_byte,
				STRING_MULTIBYTE (string));
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
       doc: /* Take cdr N times on LIST, returns the result.  */)
     (n, list)
     Lisp_Object n;
     register Lisp_Object list;
{
  register int i, num;
  CHECK_NUMBER (n);
  num = XINT (n);
  for (i = 0; i < num && !NILP (list); i++)
    {
      QUIT;
      if (! CONSP (list))
	wrong_type_argument (Qlistp, list);
      list = XCDR (list);
    }
  return list;
}

DEFUN ("nth", Fnth, Snth, 2, 2, 0,
       doc: /* Return the Nth element of LIST.
N counts from zero.  If LIST is not that long, nil is returned.  */)
     (n, list)
     Lisp_Object n, list;
{
  return Fcar (Fnthcdr (n, list));
}

DEFUN ("elt", Felt, Selt, 2, 2, 0,
       doc: /* Return element of SEQUENCE at index N.  */)
     (sequence, n)
     register Lisp_Object sequence, n;
{
  CHECK_NUMBER (n);
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
doc: /* Return non-nil if ELT is an element of LIST.  Comparison done with `equal'.
The value is actually the tail of LIST whose car is ELT.  */)
     (elt, list)
     register Lisp_Object elt;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NILP (tail); tail = XCDR (tail))
    {
      register Lisp_Object tem;
      if (! CONSP (tail))
	wrong_type_argument (Qlistp, list);
      tem = XCAR (tail);
      if (! NILP (Fequal (elt, tem)))
	return tail;
      QUIT;
    }
  return Qnil;
}

DEFUN ("memq", Fmemq, Smemq, 2, 2, 0,
       doc: /* Return non-nil if ELT is an element of LIST.
Comparison done with EQ.  The value is actually the tail of LIST
whose car is ELT.  */)
     (elt, list)
     Lisp_Object elt, list;
{
  while (1)
    {
      if (!CONSP (list) || EQ (XCAR (list), elt))
	break;

      list = XCDR (list);
      if (!CONSP (list) || EQ (XCAR (list), elt))
	break;

      list = XCDR (list);
      if (!CONSP (list) || EQ (XCAR (list), elt))
	break;

      list = XCDR (list);
      QUIT;
    }

  if (!CONSP (list) && !NILP (list))
    list = wrong_type_argument (Qlistp, list);

  return list;
}

DEFUN ("assq", Fassq, Sassq, 2, 2, 0,
       doc: /* Return non-nil if KEY is `eq' to the car of an element of LIST.
The value is actually the element of LIST whose car is KEY.
Elements of LIST that are not conses are ignored.  */)
     (key, list)
     Lisp_Object key, list;
{
  Lisp_Object result;

  while (1)
    {
      if (!CONSP (list)
	  || (CONSP (XCAR (list))
	      && EQ (XCAR (XCAR (list)), key)))
	break;

      list = XCDR (list);
      if (!CONSP (list)
	  || (CONSP (XCAR (list))
	      && EQ (XCAR (XCAR (list)), key)))
	break;

      list = XCDR (list);
      if (!CONSP (list)
	  || (CONSP (XCAR (list))
	      && EQ (XCAR (XCAR (list)), key)))
	break;

      list = XCDR (list);
      QUIT;
    }

  if (CONSP (list))
    result = XCAR (list);
  else if (NILP (list))
    result = Qnil;
  else
    result = wrong_type_argument (Qlistp, list);

  return result;
}

/* Like Fassq but never report an error and do not allow quits.
   Use only on lists known never to be circular.  */

Lisp_Object
assq_no_quit (key, list)
     Lisp_Object key, list;
{
  while (CONSP (list)
	 && (!CONSP (XCAR (list))
	     || !EQ (XCAR (XCAR (list)), key)))
    list = XCDR (list);

  return CONSP (list) ? XCAR (list) : Qnil;
}

DEFUN ("assoc", Fassoc, Sassoc, 2, 2, 0,
       doc: /* Return non-nil if KEY is `equal' to the car of an element of LIST.
The value is actually the element of LIST whose car equals KEY.  */)
       (key, list)
     Lisp_Object key, list;
{
  Lisp_Object result, car;

  while (1)
    {
      if (!CONSP (list)
	  || (CONSP (XCAR (list))
	      && (car = XCAR (XCAR (list)),
		  EQ (car, key) || !NILP (Fequal (car, key)))))
	break;

      list = XCDR (list);
      if (!CONSP (list)
	  || (CONSP (XCAR (list))
	      && (car = XCAR (XCAR (list)),
		  EQ (car, key) || !NILP (Fequal (car, key)))))
	break;

      list = XCDR (list);
      if (!CONSP (list)
	  || (CONSP (XCAR (list))
	      && (car = XCAR (XCAR (list)),
		  EQ (car, key) || !NILP (Fequal (car, key)))))
	break;

      list = XCDR (list);
      QUIT;
    }

  if (CONSP (list))
    result = XCAR (list);
  else if (NILP (list))
    result = Qnil;
  else
    result = wrong_type_argument (Qlistp, list);

  return result;
}

DEFUN ("rassq", Frassq, Srassq, 2, 2, 0,
       doc: /* Return non-nil if KEY is `eq' to the cdr of an element of LIST.
The value is actually the element of LIST whose cdr is KEY.  */)
     (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  Lisp_Object result;

  while (1)
    {
      if (!CONSP (list)
	  || (CONSP (XCAR (list))
	      && EQ (XCDR (XCAR (list)), key)))
	break;

      list = XCDR (list);
      if (!CONSP (list)
	  || (CONSP (XCAR (list))
	      && EQ (XCDR (XCAR (list)), key)))
	break;

      list = XCDR (list);
      if (!CONSP (list)
	  || (CONSP (XCAR (list))
	      && EQ (XCDR (XCAR (list)), key)))
	break;

      list = XCDR (list);
      QUIT;
    }

  if (NILP (list))
    result = Qnil;
  else if (CONSP (list))
    result = XCAR (list);
  else
    result = wrong_type_argument (Qlistp, list);

  return result;
}

DEFUN ("rassoc", Frassoc, Srassoc, 2, 2, 0,
       doc: /* Return non-nil if KEY is `equal' to the cdr of an element of LIST.
The value is actually the element of LIST whose cdr equals KEY.  */)
     (key, list)
     Lisp_Object key, list;
{
  Lisp_Object result, cdr;

  while (1)
    {
      if (!CONSP (list)
	  || (CONSP (XCAR (list))
	      && (cdr = XCDR (XCAR (list)),
		  EQ (cdr, key) || !NILP (Fequal (cdr, key)))))
	break;

      list = XCDR (list);
      if (!CONSP (list)
	  || (CONSP (XCAR (list))
	      && (cdr = XCDR (XCAR (list)),
		  EQ (cdr, key) || !NILP (Fequal (cdr, key)))))
	break;

      list = XCDR (list);
      if (!CONSP (list)
	  || (CONSP (XCAR (list))
	      && (cdr = XCDR (XCAR (list)),
		  EQ (cdr, key) || !NILP (Fequal (cdr, key)))))
	break;

      list = XCDR (list);
      QUIT;
    }

  if (CONSP (list))
    result = XCAR (list);
  else if (NILP (list))
    result = Qnil;
  else
    result = wrong_type_argument (Qlistp, list);

  return result;
}

DEFUN ("delq", Fdelq, Sdelq, 2, 2, 0,
       doc: /* Delete by side effect any occurrences of ELT as a member of LIST.
The modified LIST is returned.  Comparison is done with `eq'.
If the first member of LIST is ELT, there is no way to remove it by side effect;
therefore, write `(setq foo (delq element foo))'
to be sure of changing the value of `foo'.  */)
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
      if (! CONSP (tail))
	wrong_type_argument (Qlistp, list);
      tem = XCAR (tail);
      if (EQ (elt, tem))
	{
	  if (NILP (prev))
	    list = XCDR (tail);
	  else
	    Fsetcdr (prev, XCDR (tail));
	}
      else
	prev = tail;
      tail = XCDR (tail);
      QUIT;
    }
  return list;
}

DEFUN ("delete", Fdelete, Sdelete, 2, 2, 0,
       doc: /* Delete by side effect any occurrences of ELT as a member of SEQ.
SEQ must be a list, a vector, or a string.
The modified SEQ is returned.  Comparison is done with `equal'.
If SEQ is not a list, or the first member of SEQ is ELT, deleting it
is not a side effect; it is simply using a different sequence.
Therefore, write `(setq foo (delete element foo))'
to be sure of changing the value of `foo'.  */)
     (elt, seq)
     Lisp_Object elt, seq;
{
  if (VECTORP (seq))
    {
      EMACS_INT i, n;

      for (i = n = 0; i < ASIZE (seq); ++i)
	if (NILP (Fequal (AREF (seq, i), elt)))
	  ++n;

      if (n != ASIZE (seq))
	{
	  struct Lisp_Vector *p = allocate_vector (n);

	  for (i = n = 0; i < ASIZE (seq); ++i)
	    if (NILP (Fequal (AREF (seq, i), elt)))
	      p->contents[n++] = AREF (seq, i);

	  XSETVECTOR (seq, p);
	}
    }
  else if (STRINGP (seq))
    {
      EMACS_INT i, ibyte, nchars, nbytes, cbytes;
      int c;

      for (i = nchars = nbytes = ibyte = 0;
	   i < XSTRING (seq)->size;
	   ++i, ibyte += cbytes)
	{
	  if (STRING_MULTIBYTE (seq))
	    {
	      c = STRING_CHAR (&XSTRING (seq)->data[ibyte],
			       STRING_BYTES (XSTRING (seq)) - ibyte);
	      cbytes = CHAR_BYTES (c);
	    }
	  else
	    {
	      c = XSTRING (seq)->data[i];
	      cbytes = 1;
	    }

	  if (!INTEGERP (elt) || c != XINT (elt))
	    {
	      ++nchars;
	      nbytes += cbytes;
	    }
	}

      if (nchars != XSTRING (seq)->size)
	{
	  Lisp_Object tem;

	  tem = make_uninit_multibyte_string (nchars, nbytes);
	  if (!STRING_MULTIBYTE (seq))
	    SET_STRING_BYTES (XSTRING (tem), -1);

	  for (i = nchars = nbytes = ibyte = 0;
	       i < XSTRING (seq)->size;
	       ++i, ibyte += cbytes)
	    {
	      if (STRING_MULTIBYTE (seq))
		{
		  c = STRING_CHAR (&XSTRING (seq)->data[ibyte],
				   STRING_BYTES (XSTRING (seq)) - ibyte);
		  cbytes = CHAR_BYTES (c);
		}
	      else
		{
		  c = XSTRING (seq)->data[i];
		  cbytes = 1;
		}

	      if (!INTEGERP (elt) || c != XINT (elt))
		{
		  unsigned char *from = &XSTRING (seq)->data[ibyte];
		  unsigned char *to   = &XSTRING (tem)->data[nbytes];
		  EMACS_INT n;

		  ++nchars;
		  nbytes += cbytes;

		  for (n = cbytes; n--; )
		    *to++ = *from++;
		}
	    }

	  seq = tem;
	}
    }
  else
    {
      Lisp_Object tail, prev;

      for (tail = seq, prev = Qnil; !NILP (tail); tail = XCDR (tail))
	{
	  if (!CONSP (tail))
	    wrong_type_argument (Qlistp, seq);

	  if (!NILP (Fequal (elt, XCAR (tail))))
	    {
	      if (NILP (prev))
		seq = XCDR (tail);
	      else
		Fsetcdr (prev, XCDR (tail));
	    }
	  else
	    prev = tail;
	  QUIT;
	}
    }

  return seq;
}

DEFUN ("nreverse", Fnreverse, Snreverse, 1, 1, 0,
       doc: /* Reverse LIST by modifying cdr pointers.
Returns the beginning of the reversed list.  */)
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
      if (! CONSP (tail))
	wrong_type_argument (Qlistp, list);
      next = XCDR (tail);
      Fsetcdr (tail, prev);
      prev = tail;
      tail = next;
    }
  return prev;
}

DEFUN ("reverse", Freverse, Sreverse, 1, 1, 0,
       doc: /* Reverse LIST, copying.  Returns the beginning of the reversed list.
See also the function `nreverse', which is used more often.  */)
     (list)
     Lisp_Object list;
{
  Lisp_Object new;

  for (new = Qnil; CONSP (list); list = XCDR (list))
    new = Fcons (XCAR (list), new);
  if (!NILP (list))
    wrong_type_argument (Qconsp, list);
  return new;
}

Lisp_Object merge ();

DEFUN ("sort", Fsort, Ssort, 2, 2, 0,
       doc: /* Sort LIST, stably, comparing elements using PREDICATE.
Returns the sorted list.  LIST is modified by side effects.
PREDICATE is called with two elements of LIST, and should return t
if the first element is "less" than the second.  */)
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
       doc: /* Extract a value from a property list.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value
corresponding to the given PROP, or nil if PROP is not
one of the properties on the list.  */)
     (plist, prop)
     Lisp_Object plist;
     Lisp_Object prop;
{
  Lisp_Object tail;
  
  for (tail = plist;
       CONSP (tail) && CONSP (XCDR (tail));
       tail = XCDR (XCDR (tail)))
    {
      if (EQ (prop, XCAR (tail)))
	return XCAR (XCDR (tail));

      /* This function can be called asynchronously
	 (setup_coding_system).  Don't QUIT in that case.  */
      if (!interrupt_input_blocked)
	QUIT;
    }

  if (!NILP (tail))
    wrong_type_argument (Qlistp, prop);
  
  return Qnil;
}

DEFUN ("get", Fget, Sget, 2, 2, 0,
       doc: /* Return the value of SYMBOL's PROPNAME property.
This is the last value stored with `(put SYMBOL PROPNAME VALUE)'.  */)
     (symbol, propname)
     Lisp_Object symbol, propname;
{
  CHECK_SYMBOL (symbol);
  return Fplist_get (XSYMBOL (symbol)->plist, propname);
}

DEFUN ("plist-put", Fplist_put, Splist_put, 3, 3, 0,
       doc: /* Change value in PLIST of PROP to VAL.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2 ...).  PROP is a symbol and VAL is any object.
If PROP is already a property on the list, its value is set to VAL,
otherwise the new PROP VAL pair is added.  The new plist is returned;
use `(setq x (plist-put x prop val))' to be sure to use the new value.
The PLIST is modified by side effects.  */)
     (plist, prop, val)
     Lisp_Object plist;
     register Lisp_Object prop;
     Lisp_Object val;
{
  register Lisp_Object tail, prev;
  Lisp_Object newcell;
  prev = Qnil;
  for (tail = plist; CONSP (tail) && CONSP (XCDR (tail));
       tail = XCDR (XCDR (tail)))
    {
      if (EQ (prop, XCAR (tail)))
	{
	  Fsetcar (XCDR (tail), val);
	  return plist;
	}
      
      prev = tail;
      QUIT;
    }
  newcell = Fcons (prop, Fcons (val, Qnil));
  if (NILP (prev))
    return newcell;
  else
    Fsetcdr (XCDR (prev), newcell);
  return plist;
}

DEFUN ("put", Fput, Sput, 3, 3, 0,
       doc: /* Store SYMBOL's PROPNAME property with value VALUE.
It can be retrieved with `(get SYMBOL PROPNAME)'.  */)
     (symbol, propname, value)
     Lisp_Object symbol, propname, value;
{
  CHECK_SYMBOL (symbol);
  XSYMBOL (symbol)->plist
    = Fplist_put (XSYMBOL (symbol)->plist, propname, value);
  return value;
}

DEFUN ("lax-plist-get", Flax_plist_get, Slax_plist_get, 2, 2, 0,
       doc: /* Extract a value from a property list, comparing with `equal'.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value
corresponding to the given PROP, or nil if PROP is not
one of the properties on the list.  */)
     (plist, prop)
     Lisp_Object plist;
     Lisp_Object prop;
{
  Lisp_Object tail;
  
  for (tail = plist;
       CONSP (tail) && CONSP (XCDR (tail));
       tail = XCDR (XCDR (tail)))
    {
      if (! NILP (Fequal (prop, XCAR (tail))))
	return XCAR (XCDR (tail));

      QUIT;
    }

  if (!NILP (tail))
    wrong_type_argument (Qlistp, prop);
  
  return Qnil;
}

DEFUN ("lax-plist-put", Flax_plist_put, Slax_plist_put, 3, 3, 0,
       doc: /* Change value in PLIST of PROP to VAL, comparing with `equal'.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2 ...).  PROP and VAL are any objects.
If PROP is already a property on the list, its value is set to VAL,
otherwise the new PROP VAL pair is added.  The new plist is returned;
use `(setq x (lax-plist-put x prop val))' to be sure to use the new value.
The PLIST is modified by side effects.  */)
     (plist, prop, val)
     Lisp_Object plist;
     register Lisp_Object prop;
     Lisp_Object val;
{
  register Lisp_Object tail, prev;
  Lisp_Object newcell;
  prev = Qnil;
  for (tail = plist; CONSP (tail) && CONSP (XCDR (tail));
       tail = XCDR (XCDR (tail)))
    {
      if (! NILP (Fequal (prop, XCAR (tail))))
	{
	  Fsetcar (XCDR (tail), val);
	  return plist;
	}
      
      prev = tail;
      QUIT;
    }
  newcell = Fcons (prop, Fcons (val, Qnil));
  if (NILP (prev))
    return newcell;
  else
    Fsetcdr (XCDR (prev), newcell);
  return plist;
}

DEFUN ("equal", Fequal, Sequal, 2, 2, 0,
       doc: /* Return t if two Lisp objects have similar structure and contents.
They must have the same data type.
Conses are compared by comparing the cars and the cdrs.
Vectors and strings are compared element by element.
Numbers are compared by value, but integers cannot equal floats.
 (Use `=' if you want integers and floats to be able to be equal.)
Symbols must match exactly.  */)
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
    case Lisp_Float:
      return (extract_float (o1) == extract_float (o2));

    case Lisp_Cons:
      if (!internal_equal (XCAR (o1), XCAR (o2), depth + 1))
	return 0;
      o1 = XCDR (o1);
      o2 = XCDR (o2);
      goto tail_recurse;

    case Lisp_Misc:
      if (XMISCTYPE (o1) != XMISCTYPE (o2))
	return 0;
      if (OVERLAYP (o1))
	{
	  if (!internal_equal (OVERLAY_START (o1), OVERLAY_START (o2),
			       depth + 1)
	      || !internal_equal (OVERLAY_END (o1), OVERLAY_END (o2),
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

    case Lisp_Int:
    case Lisp_Symbol:
    case Lisp_Type_Limit:
      break;
    }
  
  return 0;
}

extern Lisp_Object Fmake_char_internal ();

DEFUN ("fillarray", Ffillarray, Sfillarray, 2, 2, 0,
       doc: /* Store each element of ARRAY with ITEM.
ARRAY is a vector, string, char-table, or bool-vector.  */)
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
      CHECK_NUMBER (item);
      charval = XINT (item);
      size = XSTRING (array)->size;
      if (STRING_MULTIBYTE (array))
	{
	  unsigned char str[MAX_MULTIBYTE_LENGTH];
	  int len = CHAR_STRING (charval, str);
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
       doc: /* Return the subtype of char-table CHAR-TABLE.  The value is a symbol.  */)
     (char_table)
     Lisp_Object char_table;
{
  CHECK_CHAR_TABLE (char_table);

  return XCHAR_TABLE (char_table)->purpose;
}

DEFUN ("char-table-parent", Fchar_table_parent, Schar_table_parent,
       1, 1, 0,
       doc: /* Return the parent char-table of CHAR-TABLE.
The value is either nil or another char-table.
If CHAR-TABLE holds nil for a given character,
then the actual applicable value is inherited from the parent char-table
\(or from its parents, if necessary).  */)
     (char_table)
     Lisp_Object char_table;
{
  CHECK_CHAR_TABLE (char_table);

  return XCHAR_TABLE (char_table)->parent;
}

DEFUN ("set-char-table-parent", Fset_char_table_parent, Sset_char_table_parent,
       2, 2, 0,
       doc: /* Set the parent char-table of CHAR-TABLE to PARENT.
PARENT must be either nil or another char-table.  */)
     (char_table, parent)
     Lisp_Object char_table, parent;
{
  Lisp_Object temp;

  CHECK_CHAR_TABLE (char_table);

  if (!NILP (parent))
    {
      CHECK_CHAR_TABLE (parent);

      for (temp = parent; !NILP (temp); temp = XCHAR_TABLE (temp)->parent)
	if (EQ (temp, char_table))
	  error ("Attempt to make a chartable be its own parent");
    }

  XCHAR_TABLE (char_table)->parent = parent;

  return parent;
}

DEFUN ("char-table-extra-slot", Fchar_table_extra_slot, Schar_table_extra_slot,
       2, 2, 0,
       doc: /* Return the value of CHAR-TABLE's extra-slot number N.  */)
     (char_table, n)
     Lisp_Object char_table, n;
{
  CHECK_CHAR_TABLE (char_table);
  CHECK_NUMBER (n);
  if (XINT (n) < 0
      || XINT (n) >= CHAR_TABLE_EXTRA_SLOTS (XCHAR_TABLE (char_table)))
    args_out_of_range (char_table, n);

  return XCHAR_TABLE (char_table)->extras[XINT (n)];
}

DEFUN ("set-char-table-extra-slot", Fset_char_table_extra_slot,
       Sset_char_table_extra_slot,
       3, 3, 0,
       doc: /* Set CHAR-TABLE's extra-slot number N to VALUE.  */)
     (char_table, n, value)
     Lisp_Object char_table, n, value;
{
  CHECK_CHAR_TABLE (char_table);
  CHECK_NUMBER (n);
  if (XINT (n) < 0
      || XINT (n) >= CHAR_TABLE_EXTRA_SLOTS (XCHAR_TABLE (char_table)))
    args_out_of_range (char_table, n);

  return XCHAR_TABLE (char_table)->extras[XINT (n)] = value;
}

DEFUN ("char-table-range", Fchar_table_range, Schar_table_range,
       2, 2, 0,
       doc: /* Return the value in CHAR-TABLE for a range of characters RANGE.
RANGE should be nil (for the default value)
a vector which identifies a character set or a row of a character set,
a character set name, or a character code.  */)
     (char_table, range)
     Lisp_Object char_table, range;
{
  CHECK_CHAR_TABLE (char_table);

  if (EQ (range, Qnil))
    return XCHAR_TABLE (char_table)->defalt;
  else if (INTEGERP (range))
    return Faref (char_table, range);
  else if (SYMBOLP (range))
    {
      Lisp_Object charset_info;

      charset_info = Fget (range, Qcharset);
      CHECK_VECTOR (charset_info);

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
  return Qt;
}

DEFUN ("set-char-table-range", Fset_char_table_range, Sset_char_table_range,
       3, 3, 0,
       doc: /* Set the value in CHAR-TABLE for a range of characters RANGE to VALUE.
RANGE should be t (for all characters), nil (for the default value)
a vector which identifies a character set or a row of a character set,
a coding system, or a character code.  */)
     (char_table, range, value)
     Lisp_Object char_table, range, value;
{
  int i;

  CHECK_CHAR_TABLE (char_table);

  if (EQ (range, Qt))
    for (i = 0; i < CHAR_TABLE_ORDINARY_SLOTS; i++)
      XCHAR_TABLE (char_table)->contents[i] = value;
  else if (EQ (range, Qnil))
    XCHAR_TABLE (char_table)->defalt = value;
  else if (SYMBOLP (range))
    {
      Lisp_Object charset_info;

      charset_info = Fget (range, Qcharset);
      CHECK_VECTOR (charset_info);

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
       doc: /* Set the default value in CHAR-TABLE for a generic character CHAR to VALUE.
The generic character specifies the group of characters.
See also the documentation of make-char.  */)
     (char_table, ch, value)
     Lisp_Object char_table, ch, value;
{
  int c, charset, code1, code2;
  Lisp_Object temp;

  CHECK_CHAR_TABLE (char_table);
  CHECK_NUMBER (ch);

  c = XINT (ch);
  SPLIT_CHAR (c, charset, code1, code2);

  /* Since we may want to set the default value for a character set
     not yet defined, we check only if the character set is in the
     valid range or not, instead of it is already defined or not.  */
  if (! CHARSET_VALID_P (charset))
    invalid_character (c);

  if (charset == CHARSET_ASCII)
    return (XCHAR_TABLE (char_table)->defalt = value);

  /* Even if C is not a generic char, we had better behave as if a
     generic char is specified.  */
  if (!CHARSET_DEFINED_P (charset) || CHARSET_DIMENSION (charset) == 1)
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
  if (SUB_CHAR_TABLE_P (temp))
    char_table = temp;
  else
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

static void
optimize_sub_char_table (table, chars)
     Lisp_Object *table;
     int chars;
{
  Lisp_Object elt;
  int from, to;

  if (chars == 94)
    from = 33, to = 127;
  else
    from = 32, to = 128;

  if (!SUB_CHAR_TABLE_P (*table))
    return;
  elt = XCHAR_TABLE (*table)->contents[from++];
  for (; from < to; from++)
    if (NILP (Fequal (elt, XCHAR_TABLE (*table)->contents[from])))
      return;
  *table = elt;
}

DEFUN ("optimize-char-table", Foptimize_char_table, Soptimize_char_table,
       1, 1, 0, doc: /* Optimize char table TABLE.  */)
     (table)
     Lisp_Object table;
{
  Lisp_Object elt;
  int dim;
  int i, j;

  CHECK_CHAR_TABLE (table);

  for (i = CHAR_TABLE_SINGLE_BYTE_SLOTS; i < CHAR_TABLE_ORDINARY_SLOTS; i++)
    {
      elt = XCHAR_TABLE (table)->contents[i];
      if (!SUB_CHAR_TABLE_P (elt))
	continue;
      dim = CHARSET_DIMENSION (i - 128);
      if (dim == 2)
	for (j = 32; j < SUB_CHAR_TABLE_ORDINARY_SLOTS; j++)
	  optimize_sub_char_table (XCHAR_TABLE (elt)->contents + j, dim);
      optimize_sub_char_table (XCHAR_TABLE (table)->contents + i, dim);
    }
  return Qnil;
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
      int charset = XFASTINT (indices[0]) - 128;

      i = 32;
      to = SUB_CHAR_TABLE_ORDINARY_SLOTS;
      if (CHARSET_CHARS (charset) == 94)
	i++, to--;
    }

  for (; i < to; i++)
    {
      Lisp_Object elt;
      int charset;

      elt = XCHAR_TABLE (subtable)->contents[i];
      XSETFASTINT (indices[depth], i);
      charset = XFASTINT (indices[0]) - 128;
      if (depth == 0
	  && (!CHARSET_DEFINED_P (charset)
	      || charset == CHARSET_8_BIT_CONTROL
	      || charset == CHARSET_8_BIT_GRAPHIC))
	continue;

      if (SUB_CHAR_TABLE_P (elt))
	{
	  if (depth >= 3)
	    error ("Too deep char table");
	  map_char_table (c_function, function, elt, arg, depth + 1, indices);
	}
      else
	{
	  int c1, c2, c;

	  if (NILP (elt))
	    elt = XCHAR_TABLE (subtable)->defalt;
	  c1 = depth >= 1 ? XFASTINT (indices[1]) : 0;
	  c2 = depth >= 2 ? XFASTINT (indices[2]) : 0;
	  c = MAKE_CHAR (charset, c1, c2);
	  if (c_function)
	    (*c_function) (arg, make_number (c), elt);
	  else
	    call2 (function, make_number (c), elt);
  	}
    }
}

DEFUN ("map-char-table", Fmap_char_table, Smap_char_table,
       2, 2, 0,
       doc: /* Call FUNCTION for each (normal and generic) characters in CHAR-TABLE.
FUNCTION is called with two arguments--a key and a value.
The key is always a possible IDX argument to `aref'.  */)
     (function, char_table)
     Lisp_Object function, char_table;
{
  /* The depth of char table is at most 3. */
  Lisp_Object indices[3];

  CHECK_CHAR_TABLE (char_table);

  map_char_table (NULL, function, char_table, char_table, 0, indices);
  return Qnil;
}

/* Return a value for character C in char-table TABLE.  Store the
   actual index for that value in *IDX.  Ignore the default value of
   TABLE.  */

Lisp_Object
char_table_ref_and_index (table, c, idx)
     Lisp_Object table;
     int c, *idx;
{
  int charset, c1, c2;
  Lisp_Object elt;

  if (SINGLE_BYTE_CHAR_P (c))
    {
      *idx = c;
      return XCHAR_TABLE (table)->contents[c];
    }
  SPLIT_CHAR (c, charset, c1, c2);
  elt = XCHAR_TABLE (table)->contents[charset + 128];
  *idx = MAKE_CHAR (charset, 0, 0);
  if (!SUB_CHAR_TABLE_P (elt))
    return elt;
  if (c1 < 32 || NILP (XCHAR_TABLE (elt)->contents[c1]))
    return XCHAR_TABLE (elt)->defalt;
  elt = XCHAR_TABLE (elt)->contents[c1];
  *idx = MAKE_CHAR (charset, c1, 0);
  if (!SUB_CHAR_TABLE_P (elt))
    return elt;
  if (c2 < 32 || NILP (XCHAR_TABLE (elt)->contents[c2]))
    return XCHAR_TABLE (elt)->defalt;
  *idx = c;
  return XCHAR_TABLE (elt)->contents[c2];
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
       doc: /* Concatenate any number of lists by altering them.
Only the last argument is not altered, and need not be a list.
usage: (nconc &rest LISTS)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  register int argnum;
  register Lisp_Object tail, tem, val;

  val = tail = Qnil;

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

  if (vals)
    {
      /* Don't let vals contain any garbage when GC happens.  */
      for (i = 0; i < leni; i++)
	vals[i] = Qnil;

      GCPRO3 (dummy, fn, seq);
      gcpro1.var = vals;
      gcpro1.nvars = leni;
    }
  else
    GCPRO2 (fn, seq);
  /* We need not explicitly protect `tail' because it is used only on lists, and
    1) lists are not relocated and 2) the list is marked via `seq' so will not be freed */

  if (VECTORP (seq))
    {
      for (i = 0; i < leni; i++)
	{
	  dummy = XVECTOR (seq)->contents[i];
	  dummy = call1 (fn, dummy);
	  if (vals)
	    vals[i] = dummy;
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

	  dummy = call1 (fn, dummy);
	  if (vals)
	    vals[i] = dummy;
	}
    }
  else if (STRINGP (seq))
    {
      int i_byte;

      for (i = 0, i_byte = 0; i < leni;)
	{
	  int c;
	  int i_before = i;

	  FETCH_STRING_CHAR_ADVANCE (c, seq, i, i_byte);
	  XSETFASTINT (dummy, c);
	  dummy = call1 (fn, dummy);
	  if (vals)
	    vals[i_before] = dummy;
	}
    }
  else   /* Must be a list, since Flength did not get an error */
    {
      tail = seq;
      for (i = 0; i < leni; i++)
	{
	  dummy = call1 (fn, Fcar (tail));
	  if (vals)
	    vals[i] = dummy;
	  tail = XCDR (tail);
	}
    }

  UNGCPRO;
}

DEFUN ("mapconcat", Fmapconcat, Smapconcat, 3, 3, 0,
       doc: /* Apply FUNCTION to each element of SEQUENCE, and concat the results as strings.
In between each pair of results, stick in SEPARATOR.  Thus, " " as
SEPARATOR results in spaces between the values returned by FUNCTION.
SEQUENCE may be a list, a vector, a bool-vector, or a string.  */)
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
       doc: /* Apply FUNCTION to each element of SEQUENCE, and make a list of the results.
The result is a list just as long as SEQUENCE.
SEQUENCE may be a list, a vector, a bool-vector, or a string.  */)
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

DEFUN ("mapc", Fmapc, Smapc, 2, 2, 0,
       doc: /* Apply FUNCTION to each element of SEQUENCE for side effects only.
Unlike `mapcar', don't accumulate the results.  Return SEQUENCE.
SEQUENCE may be a list, a vector, a bool-vector, or a string.  */)
     (function, sequence)
     Lisp_Object function, sequence;
{
  register int leni;

  leni = XFASTINT (Flength (sequence));
  mapcar1 (leni, 0, function, sequence);

  return sequence;
}

/* Anything that calls this function must protect from GC!  */

DEFUN ("y-or-n-p", Fy_or_n_p, Sy_or_n_p, 1, 1, 0,
       doc: /* Ask user a "y or n" question.  Return t if answer is "y".
Takes one argument, which is the string to display to ask the question.
It should end in a space; `y-or-n-p' adds `(y or n) ' to it.
No confirmation of the answer is requested; a single character is enough.
Also accepts Space to mean yes, or Delete to mean no.  \(Actually, it uses
the bindings in `query-replace-map'; see the documentation of that variable
for more information.  In this case, the useful bindings are `act', `skip',
`recenter', and `quit'.\)

Under a windowing system a dialog box will be used if `last-nonmenu-event'
is nil and `use-dialog-box' is non-nil.  */)
     (prompt)
     Lisp_Object prompt;
{
  register Lisp_Object obj, key, def, map;
  register int answer;
  Lisp_Object xprompt;
  Lisp_Object args[2];
  struct gcpro gcpro1, gcpro2;
  int count = specpdl_ptr - specpdl;

  specbind (Qcursor_in_echo_area, Qt);

  map = Fsymbol_value (intern ("query-replace-map"));

  CHECK_STRING (prompt);
  xprompt = prompt;
  GCPRO2 (prompt, xprompt);

#ifdef HAVE_X_WINDOWS
  if (display_hourglass_p)
    cancel_hourglass ();
#endif

  while (1)
    {

#ifdef HAVE_MENUS
      if ((NILP (last_nonmenu_event) || CONSP (last_nonmenu_event))
	  && use_dialog_box
	  && have_menus_p ())
	{
	  Lisp_Object pane, menu;
	  redisplay_preserve_echo_area (3);
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

      {
	Lisp_Object pargs[3];

	/* Colorize prompt according to `minibuffer-prompt' face.  */
	pargs[0] = build_string ("%s(y or n) ");
	pargs[1] = intern ("face");
	pargs[2] = intern ("minibuffer-prompt");
	args[0] = Fpropertize (3, pargs);
	args[1] = xprompt;
	Fmessage (2, args);
      }

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
       doc: /* Ask user a yes-or-no question.  Return t if answer is yes.
Takes one argument, which is the string to display to ask the question.
It should end in a space; `yes-or-no-p' adds `(yes or no) ' to it.
The user must confirm the answer with RET,
and can edit it until it has been confirmed.

Under a windowing system a dialog box will be used if `last-nonmenu-event'
is nil, and `use-dialog-box' is non-nil.  */)
     (prompt)
     Lisp_Object prompt;
{
  register Lisp_Object ans;
  Lisp_Object args[2];
  struct gcpro gcpro1;

  CHECK_STRING (prompt);

#ifdef HAVE_MENUS
  if ((NILP (last_nonmenu_event) || CONSP (last_nonmenu_event))
      && use_dialog_box
      && have_menus_p ())
    {
      Lisp_Object pane, menu, obj;
      redisplay_preserve_echo_area (4);
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
       doc: /* Return list of 1 minute, 5 minute and 15 minute load averages.
     
Each of the three load averages is multiplied by 100, then converted
to integer.

When USE-FLOATS is non-nil, floats will be used instead of integers.
These floats are not multiplied by 100.

If the 5-minute or 15-minute load averages are not available, return a
shortened list, containing only those averages which are available.  */)
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

Lisp_Object Vfeatures, Qsubfeatures;
extern Lisp_Object Vafter_load_alist;

DEFUN ("featurep", Ffeaturep, Sfeaturep, 1, 2, 0,
       doc: /* Returns t if FEATURE is present in this Emacs.
     
Use this to conditionalize execution of lisp code based on the
presence or absence of emacs or environment extensions.
Use `provide' to declare that a feature is available.  This function
looks at the value of the variable `features'.  The optional argument
SUBFEATURE can be used to check a specific subfeature of FEATURE.  */)
     (feature, subfeature)
     Lisp_Object feature, subfeature;
{
  register Lisp_Object tem;
  CHECK_SYMBOL (feature);
  tem = Fmemq (feature, Vfeatures);
  if (!NILP (tem) && !NILP (subfeature))
    tem = Fmember (subfeature, Fget (feature, Qsubfeatures));
  return (NILP (tem)) ? Qnil : Qt;
}

DEFUN ("provide", Fprovide, Sprovide, 1, 2, 0,
       doc: /* Announce that FEATURE is a feature of the current Emacs.
The optional argument SUBFEATURES should be a list of symbols listing
particular subfeatures supported in this version of FEATURE.  */)
     (feature, subfeatures)
     Lisp_Object feature, subfeatures;
{
  register Lisp_Object tem;
  CHECK_SYMBOL (feature);
  CHECK_LIST (subfeatures);
  if (!NILP (Vautoload_queue))
    Vautoload_queue = Fcons (Fcons (Vfeatures, Qnil), Vautoload_queue);
  tem = Fmemq (feature, Vfeatures);
  if (NILP (tem))
    Vfeatures = Fcons (feature, Vfeatures);
  if (!NILP (subfeatures))
    Fput (feature, Qsubfeatures, subfeatures);
  LOADHIST_ATTACH (Fcons (Qprovide, feature));

  /* Run any load-hooks for this file.  */
  tem = Fassq (feature, Vafter_load_alist);
  if (!NILP (tem))
    Fprogn (Fcdr (tem));

  return feature;
}

/* `require' and its subroutines.  */

/* List of features currently being require'd, innermost first.  */

Lisp_Object require_nesting_list;

Lisp_Object
require_unwind (old_value)
     Lisp_Object old_value;
{
  return require_nesting_list = old_value;
}

DEFUN ("require", Frequire, Srequire, 1, 3, 0,
       doc: /* If feature FEATURE is not loaded, load it from FILENAME.
If FEATURE is not a member of the list `features', then the feature
is not loaded; so load the file FILENAME.
If FILENAME is omitted, the printname of FEATURE is used as the file name,
and `load' will try to load this name appended with the suffix `.elc',
`.el' or the unmodified name, in that order.
If the optional third argument NOERROR is non-nil,
then return nil if the file is not found instead of signaling an error.
Normally the return value is FEATURE.
The normal messages at start and end of loading FILENAME are suppressed.  */)
     (feature, filename, noerror)
     Lisp_Object feature, filename, noerror;
{
  register Lisp_Object tem;
  struct gcpro gcpro1, gcpro2;

  CHECK_SYMBOL (feature);

  tem = Fmemq (feature, Vfeatures);

  LOADHIST_ATTACH (Fcons (Qrequire, feature));
  
  if (NILP (tem))
    {
      int count = specpdl_ptr - specpdl;
      int nesting = 0;

      /* This is to make sure that loadup.el gives a clear picture
	 of what files are preloaded and when.  */
      if (! NILP (Vpurify_flag))
	error ("(require %s) while preparing to dump",
	       XSTRING (SYMBOL_NAME (feature))->data);
      
      /* A certain amount of recursive `require' is legitimate,
	 but if we require the same feature recursively 3 times,
	 signal an error.  */
      tem = require_nesting_list;
      while (! NILP (tem))
	{
	  if (! NILP (Fequal (feature, XCAR (tem))))
	    nesting++;
	  tem = XCDR (tem);
	}
      if (nesting > 2)
	error ("Recursive `require' for feature `%s'",
	       XSTRING (SYMBOL_NAME (feature))->data);

      /* Update the list for any nested `require's that occur.  */
      record_unwind_protect (require_unwind, require_nesting_list);
      require_nesting_list = Fcons (feature, require_nesting_list);

      /* Value saved here is to be restored into Vautoload_queue */
      record_unwind_protect (un_autoload, Vautoload_queue);
      Vautoload_queue = Qt;

      /* Load the file.  */
      GCPRO2 (feature, filename);
      tem = Fload (NILP (filename) ? Fsymbol_name (feature) : filename,
		   noerror, Qt, Qnil, (NILP (filename) ? Qt : Qnil));
      UNGCPRO;

      /* If load failed entirely, return nil.  */
      if (NILP (tem))
	return unbind_to (count, Qnil);

      tem = Fmemq (feature, Vfeatures);
      if (NILP (tem))
	error ("Required feature `%s' was not provided",
	       XSTRING (SYMBOL_NAME (feature))->data);

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

DEFUN ("plist-member", Fplist_member, Splist_member, 2, 2, 0,
       doc: /* Return non-nil if PLIST has the property PROP.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2 ...\).  PROP is a symbol.
Unlike `plist-get', this allows you to distinguish between a missing
property and a property with the value nil.
The value is actually the tail of PLIST whose car is PROP.  */)
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
       doc: /* In WIDGET, set PROPERTY to VALUE.
The value can later be retrieved with `widget-get'.  */)
     (widget, property, value)
     Lisp_Object widget, property, value;
{
  CHECK_CONS (widget);
  XSETCDR (widget, Fplist_put (XCDR (widget), property, value));
  return value;
}

DEFUN ("widget-get", Fwidget_get, Swidget_get, 2, 2, 0,
       doc: /* In WIDGET, get the value of PROPERTY.
The value could either be specified when the widget was created, or
later with `widget-put'.  */)
     (widget, property)
     Lisp_Object widget, property;
{
  Lisp_Object tmp;

  while (1)
    {
      if (NILP (widget))
	return Qnil;
      CHECK_CONS (widget);
      tmp = Fplist_member (XCDR (widget), property);
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
       doc: /* Apply the value of WIDGET's PROPERTY to the widget itself.
ARGS are passed as extra arguments to the function.
usage: (widget-apply WIDGET PROPERTY &rest ARGS)  */)
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

/* base64 encode/decode functions (RFC 2045).
   Based on code from GNU recode. */

#define MIME_LINE_LENGTH 76

#define IS_ASCII(Character) \
  ((Character) < 128)
#define IS_BASE64(Character) \
  (IS_ASCII (Character) && base64_char_to_value[Character] >= 0)
#define IS_BASE64_IGNORABLE(Character) \
  ((Character) == ' ' || (Character) == '\t' || (Character) == '\n' \
   || (Character) == '\f' || (Character) == '\r')

/* Used by base64_decode_1 to retrieve a non-base64-ignorable
   character or return retval if there are no characters left to
   process. */
#define READ_QUADRUPLET_BYTE(retval)	\
  do					\
    {					\
      if (i == length)			\
	{				\
	  if (nchars_return)		\
	    *nchars_return = nchars;	\
	  return (retval);		\
	}				\
      c = from[i++];			\
    }					\
  while (IS_BASE64_IGNORABLE (c))

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


static int base64_encode_1 P_ ((const char *, char *, int, int, int));
static int base64_decode_1 P_ ((const char *, char *, int, int, int *));

DEFUN ("base64-encode-region", Fbase64_encode_region, Sbase64_encode_region,
       2, 3, "r",
       doc: /* Base64-encode the region between BEG and END.
Return the length of the encoded text.
Optional third argument NO-LINE-BREAK means do not break long lines
into shorter lines.  */)
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
				    NILP (no_line_break),
				    !NILP (current_buffer->enable_multibyte_characters));
  if (encoded_length > allength)
    abort ();

  if (encoded_length < 0)
    {
      /* The encoding wasn't possible. */
      if (length > MAX_ALLOCA)
	xfree (encoded);
      error ("Multibyte character in data for base64 encoding");
    }

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
       1, 2, 0,
       doc: /* Base64-encode STRING and return the result.
Optional second argument NO-LINE-BREAK means do not break long lines
into shorter lines.  */)
     (string, no_line_break)
     Lisp_Object string, no_line_break;
{
  int allength, length, encoded_length;
  char *encoded;
  Lisp_Object encoded_string;

  CHECK_STRING (string);

  /* We need to allocate enough room for encoding the text.
     We need 33 1/3% more space, plus a newline every 76
     characters, and then we round up. */
  length = STRING_BYTES (XSTRING (string));
  allength = length + length/3 + 1;
  allength += allength / MIME_LINE_LENGTH + 1 + 6;

  /* We need to allocate enough room for decoding the text. */
  if (allength <= MAX_ALLOCA)
    encoded = (char *) alloca (allength);
  else
    encoded = (char *) xmalloc (allength);

  encoded_length = base64_encode_1 (XSTRING (string)->data,
				    encoded, length, NILP (no_line_break),
				    STRING_MULTIBYTE (string));
  if (encoded_length > allength)
    abort ();

  if (encoded_length < 0)
    {
      /* The encoding wasn't possible. */
      if (length > MAX_ALLOCA)
	xfree (encoded);
      error ("Multibyte character in data for base64 encoding");
    }

  encoded_string = make_unibyte_string (encoded, encoded_length);
  if (allength > MAX_ALLOCA)
    xfree (encoded);

  return encoded_string;
}

static int
base64_encode_1 (from, to, length, line_break, multibyte)
     const char *from;
     char *to;
     int length;
     int line_break;
     int multibyte;
{
  int counter = 0, i = 0;
  char *e = to;
  int c;
  unsigned int value;
  int bytes;

  while (i < length)
    {
      if (multibyte)
	{
	  c = STRING_CHAR_AND_LENGTH (from + i, length - i, bytes);
	  if (c >= 256)
	    return -1;
	  i += bytes;
	}
      else
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

      if (multibyte)
	{
	  c = STRING_CHAR_AND_LENGTH (from + i, length - i, bytes);
	  if (c >= 256)
	    return -1;
	  i += bytes;
	}
      else
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

      if (multibyte)
	{
	  c = STRING_CHAR_AND_LENGTH (from + i, length - i, bytes);
	  if (c >= 256)
	    return -1;
	  i += bytes;
	}
      else
	c = from[i++];

      *e++ = base64_value_to_char[value | (0x03 & c >> 6)];
      *e++ = base64_value_to_char[0x3f & c];
    }

  return e - to;
}


DEFUN ("base64-decode-region", Fbase64_decode_region, Sbase64_decode_region,
       2, 2, "r",
       doc: /* Base64-decode the region between BEG and END.
Return the length of the decoded text.
If the region can't be decoded, signal an error and don't modify the buffer.  */)
     (beg, end)
     Lisp_Object beg, end;
{
  int ibeg, iend, length, allength;
  char *decoded;
  int old_pos = PT;
  int decoded_length;
  int inserted_chars;
  int multibyte = !NILP (current_buffer->enable_multibyte_characters);

  validate_region (&beg, &end);

  ibeg = CHAR_TO_BYTE (XFASTINT (beg));
  iend = CHAR_TO_BYTE (XFASTINT (end));

  length = iend - ibeg;

  /* We need to allocate enough room for decoding the text.  If we are
     working on a multibyte buffer, each decoded code may occupy at
     most two bytes.  */
  allength = multibyte ? length * 2 : length;
  if (allength <= MAX_ALLOCA)
    decoded = (char *) alloca (allength);
  else
    decoded = (char *) xmalloc (allength);

  move_gap_both (XFASTINT (beg), ibeg);
  decoded_length = base64_decode_1 (BYTE_POS_ADDR (ibeg), decoded, length,
				    multibyte, &inserted_chars);
  if (decoded_length > allength)
    abort ();

  if (decoded_length < 0)
    {
      /* The decoding wasn't possible. */
      if (allength > MAX_ALLOCA)
	xfree (decoded);
      error ("Invalid base64 data");
    }

  /* Now we have decoded the region, so we insert the new contents
     and delete the old.  (Insert first in order to preserve markers.)  */
  TEMP_SET_PT_BOTH (XFASTINT (beg), ibeg);
  insert_1_both (decoded, inserted_chars, decoded_length, 0, 1, 0);
  if (allength > MAX_ALLOCA)
    xfree (decoded);
  /* Delete the original text.  */
  del_range_both (PT, PT_BYTE, XFASTINT (end) + inserted_chars,
		  iend + decoded_length, 1);

  /* If point was outside of the region, restore it exactly; else just
     move to the beginning of the region.  */
  if (old_pos >= XFASTINT (end))
    old_pos += inserted_chars - (XFASTINT (end) - XFASTINT (beg));
  else if (old_pos > XFASTINT (beg))
    old_pos = XFASTINT (beg);
  SET_PT (old_pos > ZV ? ZV : old_pos);

  return make_number (inserted_chars);
}

DEFUN ("base64-decode-string", Fbase64_decode_string, Sbase64_decode_string,
       1, 1, 0,
       doc: /* Base64-decode STRING and return the result.  */)
     (string)
     Lisp_Object string;
{
  char *decoded;
  int length, decoded_length;
  Lisp_Object decoded_string;

  CHECK_STRING (string);

  length = STRING_BYTES (XSTRING (string));
  /* We need to allocate enough room for decoding the text. */
  if (length <= MAX_ALLOCA)
    decoded = (char *) alloca (length);
  else
    decoded = (char *) xmalloc (length);

  /* The decoded result should be unibyte. */
  decoded_length = base64_decode_1 (XSTRING (string)->data, decoded, length,
				    0, NULL);
  if (decoded_length > length)
    abort ();
  else if (decoded_length >= 0)
    decoded_string = make_unibyte_string (decoded, decoded_length);
  else
    decoded_string = Qnil;

  if (length > MAX_ALLOCA)
    xfree (decoded);
  if (!STRINGP (decoded_string))
    error ("Invalid base64 data");

  return decoded_string;
}

/* Base64-decode the data at FROM of LENGHT bytes into TO.  If
   MULTIBYTE is nonzero, the decoded result should be in multibyte
   form.  If NCHARS_RETRUN is not NULL, store the number of produced
   characters in *NCHARS_RETURN.  */

static int
base64_decode_1 (from, to, length, multibyte, nchars_return)
     const char *from;
     char *to;
     int length;
     int multibyte;
     int *nchars_return;
{
  int i = 0;
  char *e = to;
  unsigned char c;
  unsigned long value;
  int nchars = 0;

  while (1)
    {
      /* Process first byte of a quadruplet. */

      READ_QUADRUPLET_BYTE (e-to);

      if (!IS_BASE64 (c))
	return -1;
      value = base64_char_to_value[c] << 18;

      /* Process second byte of a quadruplet.  */

      READ_QUADRUPLET_BYTE (-1);

      if (!IS_BASE64 (c))
	return -1;
      value |= base64_char_to_value[c] << 12;

      c = (unsigned char) (value >> 16);
      if (multibyte)
	e += CHAR_STRING (c, e);
      else
	*e++ = c;
      nchars++;

      /* Process third byte of a quadruplet.  */

      READ_QUADRUPLET_BYTE (-1);

      if (c == '=')
	{
	  READ_QUADRUPLET_BYTE (-1);

	  if (c != '=')
	    return -1;
	  continue;
	}

      if (!IS_BASE64 (c))
	return -1;
      value |= base64_char_to_value[c] << 6;

      c = (unsigned char) (0xff & value >> 8);
      if (multibyte)
	e += CHAR_STRING (c, e);
      else
	*e++ = c;
      nchars++;

      /* Process fourth byte of a quadruplet.  */

      READ_QUADRUPLET_BYTE (-1);

      if (c == '=')
	continue;

      if (!IS_BASE64 (c))
	return -1;
      value |= base64_char_to_value[c];

      c = (unsigned char) (0xff & value);
      if (multibyte)
	e += CHAR_STRING (c, e);
      else
	*e++ = c;
      nchars++;
    }
}



/***********************************************************************
 *****                                                             *****
 *****			     Hash Tables                           *****
 *****                                                             *****
 ***********************************************************************/

/* Implemented by gerd@gnu.org.  This hash table implementation was
   inspired by CMUCL hash tables.  */

/* Ideas:

   1. For small tables, association lists are probably faster than
   hash tables because they have lower overhead.

   For uses of hash tables where the O(1) behavior of table
   operations is not a requirement, it might therefore be a good idea
   not to hash.  Instead, we could just do a linear search in the
   key_and_value vector of the hash table.  This could be done
   if a `:linear-search t' argument is given to make-hash-table.  */


/* Value is the key part of entry IDX in hash table H.  */

#define HASH_KEY(H, IDX)   AREF ((H)->key_and_value, 2 * (IDX))

/* Value is the value part of entry IDX in hash table H.  */

#define HASH_VALUE(H, IDX) AREF ((H)->key_and_value, 2 * (IDX) + 1)

/* Value is the index of the next entry following the one at IDX
   in hash table H.  */

#define HASH_NEXT(H, IDX)  AREF ((H)->next, (IDX))

/* Value is the hash code computed for entry IDX in hash table H.  */

#define HASH_HASH(H, IDX)  AREF ((H)->hash, (IDX))

/* Value is the index of the element in hash table H that is the
   start of the collision list at index IDX in the index vector of H.  */

#define HASH_INDEX(H, IDX)  AREF ((H)->index, (IDX))

/* Value is the size of hash table H.  */

#define HASH_TABLE_SIZE(H) XVECTOR ((H)->next)->size

/* The list of all weak hash tables.  Don't staticpro this one.  */

Lisp_Object Vweak_hash_tables;

/* Various symbols.  */

Lisp_Object Qhash_table_p, Qeq, Qeql, Qequal, Qkey, Qvalue;
Lisp_Object QCtest, QCsize, QCrehash_size, QCrehash_threshold, QCweakness;
Lisp_Object Qhash_table_test, Qkey_or_value, Qkey_and_value;

/* Function prototypes.  */

static struct Lisp_Hash_Table *check_hash_table P_ ((Lisp_Object));
static int get_key_arg P_ ((Lisp_Object, int, Lisp_Object *, char *));
static void maybe_resize_hash_table P_ ((struct Lisp_Hash_Table *));
static int cmpfn_eql P_ ((struct Lisp_Hash_Table *, Lisp_Object, unsigned,
			  Lisp_Object, unsigned));
static int cmpfn_equal P_ ((struct Lisp_Hash_Table *, Lisp_Object, unsigned,
			    Lisp_Object, unsigned));
static int cmpfn_user_defined P_ ((struct Lisp_Hash_Table *, Lisp_Object,
				   unsigned, Lisp_Object, unsigned));
static unsigned hashfn_eq P_ ((struct Lisp_Hash_Table *, Lisp_Object));
static unsigned hashfn_eql P_ ((struct Lisp_Hash_Table *, Lisp_Object));
static unsigned hashfn_equal P_ ((struct Lisp_Hash_Table *, Lisp_Object));
static unsigned hashfn_user_defined P_ ((struct Lisp_Hash_Table *,
					 Lisp_Object));
static unsigned sxhash_string P_ ((unsigned char *, int));
static unsigned sxhash_list P_ ((Lisp_Object, int));
static unsigned sxhash_vector P_ ((Lisp_Object, int));
static unsigned sxhash_bool_vector P_ ((Lisp_Object));
static int sweep_weak_table P_ ((struct Lisp_Hash_Table *, int));



/***********************************************************************
			       Utilities
 ***********************************************************************/

/* If OBJ is a Lisp hash table, return a pointer to its struct
   Lisp_Hash_Table.  Otherwise, signal an error.  */

static struct Lisp_Hash_Table *
check_hash_table (obj)
     Lisp_Object obj;
{
  CHECK_HASH_TABLE (obj);
  return XHASH_TABLE (obj);
}


/* Value is the next integer I >= N, N >= 0 which is "almost" a prime
   number.  */

int
next_almost_prime (n)
     int n;
{
  if (n % 2 == 0)
    n += 1;
  if (n % 3 == 0)
    n += 2;
  if (n % 7 == 0)
    n += 4;
  return n;
}


/* Find KEY in ARGS which has size NARGS.  Don't consider indices for
   which USED[I] is non-zero.  If found at index I in ARGS, set
   USED[I] and USED[I + 1] to 1, and return I + 1.  Otherwise return
   -1.  This function is used to extract a keyword/argument pair from
   a DEFUN parameter list.  */

static int
get_key_arg (key, nargs, args, used)
     Lisp_Object key;
     int nargs;
     Lisp_Object *args;
     char *used;
{
  int i;

  for (i = 0; i < nargs - 1; ++i)
    if (!used[i] && EQ (args[i], key))
      break;

  if (i >= nargs - 1)
    i = -1;
  else
    {
      used[i++] = 1;
      used[i] = 1;
    }

  return i;
}


/* Return a Lisp vector which has the same contents as VEC but has
   size NEW_SIZE, NEW_SIZE >= VEC->size.  Entries in the resulting
   vector that are not copied from VEC are set to INIT.  */

Lisp_Object
larger_vector (vec, new_size, init)
     Lisp_Object vec;
     int new_size;
     Lisp_Object init;
{
  struct Lisp_Vector *v;
  int i, old_size;

  xassert (VECTORP (vec));
  old_size = XVECTOR (vec)->size;
  xassert (new_size >= old_size);

  v = allocate_vector (new_size);
  bcopy (XVECTOR (vec)->contents, v->contents,
	 old_size * sizeof *v->contents);
  for (i = old_size; i < new_size; ++i)
    v->contents[i] = init;
  XSETVECTOR (vec, v);
  return vec;
}


/***********************************************************************
			 Low-level Functions
 ***********************************************************************/

/* Compare KEY1 which has hash code HASH1 and KEY2 with hash code
   HASH2 in hash table H using `eql'.  Value is non-zero if KEY1 and
   KEY2 are the same.  */

static int
cmpfn_eql (h, key1, hash1, key2, hash2)
     struct Lisp_Hash_Table *h;
     Lisp_Object key1, key2;
     unsigned hash1, hash2;
{
  return (FLOATP (key1)
	  && FLOATP (key2)
	  && XFLOAT_DATA (key1) == XFLOAT_DATA (key2));
}


/* Compare KEY1 which has hash code HASH1 and KEY2 with hash code
   HASH2 in hash table H using `equal'.  Value is non-zero if KEY1 and
   KEY2 are the same.  */

static int
cmpfn_equal (h, key1, hash1, key2, hash2)
     struct Lisp_Hash_Table *h;
     Lisp_Object key1, key2;
     unsigned hash1, hash2;
{
  return hash1 == hash2 && !NILP (Fequal (key1, key2));
}


/* Compare KEY1 which has hash code HASH1, and KEY2 with hash code
   HASH2 in hash table H using H->user_cmp_function.  Value is non-zero
   if KEY1 and KEY2 are the same.  */

static int
cmpfn_user_defined (h, key1, hash1, key2, hash2)
     struct Lisp_Hash_Table *h;
     Lisp_Object key1, key2;
     unsigned hash1, hash2;
{
  if (hash1 == hash2)
    {
      Lisp_Object args[3];

      args[0] = h->user_cmp_function;
      args[1] = key1;
      args[2] = key2;
      return !NILP (Ffuncall (3, args));
    }
  else
    return 0;
}


/* Value is a hash code for KEY for use in hash table H which uses
   `eq' to compare keys.  The hash code returned is guaranteed to fit
   in a Lisp integer.  */

static unsigned
hashfn_eq (h, key)
     struct Lisp_Hash_Table *h;
     Lisp_Object key;
{
  unsigned hash = XUINT (key) ^ XGCTYPE (key);
  xassert ((hash & ~VALMASK) == 0);
  return hash;
}


/* Value is a hash code for KEY for use in hash table H which uses
   `eql' to compare keys.  The hash code returned is guaranteed to fit
   in a Lisp integer.  */

static unsigned
hashfn_eql (h, key)
     struct Lisp_Hash_Table *h;
     Lisp_Object key;
{
  unsigned hash;
  if (FLOATP (key))
    hash = sxhash (key, 0);
  else
    hash = XUINT (key) ^ XGCTYPE (key);
  xassert ((hash & ~VALMASK) == 0);
  return hash;
}


/* Value is a hash code for KEY for use in hash table H which uses
   `equal' to compare keys.  The hash code returned is guaranteed to fit
   in a Lisp integer.  */

static unsigned
hashfn_equal (h, key)
     struct Lisp_Hash_Table *h;
     Lisp_Object key;
{
  unsigned hash = sxhash (key, 0);
  xassert ((hash & ~VALMASK) == 0);
  return hash;
}


/* Value is a hash code for KEY for use in hash table H which uses as
   user-defined function to compare keys.  The hash code returned is
   guaranteed to fit in a Lisp integer.  */

static unsigned
hashfn_user_defined (h, key)
     struct Lisp_Hash_Table *h;
     Lisp_Object key;
{
  Lisp_Object args[2], hash;

  args[0] = h->user_hash_function;
  args[1] = key;
  hash = Ffuncall (2, args);
  if (!INTEGERP (hash))
    Fsignal (Qerror,
	     list2 (build_string ("Invalid hash code returned from \
user-supplied hash function"),
		    hash));
  return XUINT (hash);
}


/* Create and initialize a new hash table.

   TEST specifies the test the hash table will use to compare keys.
   It must be either one of the predefined tests `eq', `eql' or
   `equal' or a symbol denoting a user-defined test named TEST with
   test and hash functions USER_TEST and USER_HASH.

   Give the table initial capacity SIZE, SIZE >= 0, an integer.

   If REHASH_SIZE is an integer, it must be > 0, and this hash table's
   new size when it becomes full is computed by adding REHASH_SIZE to
   its old size.  If REHASH_SIZE is a float, it must be > 1.0, and the
   table's new size is computed by multiplying its old size with
   REHASH_SIZE.

   REHASH_THRESHOLD must be a float <= 1.0, and > 0.  The table will
   be resized when the ratio of (number of entries in the table) /
   (table size) is >= REHASH_THRESHOLD.

   WEAK specifies the weakness of the table.  If non-nil, it must be
   one of the symbols `key', `value', `key-or-value', or `key-and-value'.  */

Lisp_Object
make_hash_table (test, size, rehash_size, rehash_threshold, weak,
		 user_test, user_hash)
     Lisp_Object test, size, rehash_size, rehash_threshold, weak;
     Lisp_Object user_test, user_hash;
{
  struct Lisp_Hash_Table *h;
  Lisp_Object table;
  int index_size, i, sz;

  /* Preconditions.  */
  xassert (SYMBOLP (test));
  xassert (INTEGERP (size) && XINT (size) >= 0);
  xassert ((INTEGERP (rehash_size) && XINT (rehash_size) > 0)
	   || (FLOATP (rehash_size) && XFLOATINT (rehash_size) > 1.0));
  xassert (FLOATP (rehash_threshold)
	   && XFLOATINT (rehash_threshold) > 0
	   && XFLOATINT (rehash_threshold) <= 1.0);

  if (XFASTINT (size) == 0)
    size = make_number (1);

  /* Allocate a table and initialize it.  */
  h = allocate_hash_table ();

  /* Initialize hash table slots.  */
  sz = XFASTINT (size);

  h->test = test;
  if (EQ (test, Qeql))
    {
      h->cmpfn = cmpfn_eql;
      h->hashfn = hashfn_eql;
    }
  else if (EQ (test, Qeq))
    {
      h->cmpfn = NULL;
      h->hashfn = hashfn_eq;
    }
  else if (EQ (test, Qequal))
    {
      h->cmpfn = cmpfn_equal;
      h->hashfn = hashfn_equal;
    }
  else
    {
      h->user_cmp_function = user_test;
      h->user_hash_function = user_hash;
      h->cmpfn = cmpfn_user_defined;
      h->hashfn = hashfn_user_defined;
    }

  h->weak = weak;
  h->rehash_threshold = rehash_threshold;
  h->rehash_size = rehash_size;
  h->count = make_number (0);
  h->key_and_value = Fmake_vector (make_number (2 * sz), Qnil);
  h->hash = Fmake_vector (size, Qnil);
  h->next = Fmake_vector (size, Qnil);
  /* Cast to int here avoids losing with gcc 2.95 on Tru64/Alpha...  */
  index_size = next_almost_prime ((int) (sz / XFLOATINT (rehash_threshold)));
  h->index = Fmake_vector (make_number (index_size), Qnil);

  /* Set up the free list.  */
  for (i = 0; i < sz - 1; ++i)
    HASH_NEXT (h, i) = make_number (i + 1);
  h->next_free = make_number (0);

  XSET_HASH_TABLE (table, h);
  xassert (HASH_TABLE_P (table));
  xassert (XHASH_TABLE (table) == h);

  /* Maybe add this hash table to the list of all weak hash tables.  */
  if (NILP (h->weak))
    h->next_weak = Qnil;
  else
    {
      h->next_weak = Vweak_hash_tables;
      Vweak_hash_tables = table;
    }

  return table;
}


/* Return a copy of hash table H1.  Keys and values are not copied,
   only the table itself is.  */

Lisp_Object
copy_hash_table (h1)
     struct Lisp_Hash_Table *h1;
{
  Lisp_Object table;
  struct Lisp_Hash_Table *h2;
  struct Lisp_Vector *next;

  h2 = allocate_hash_table ();
  next = h2->vec_next;
  bcopy (h1, h2, sizeof *h2);
  h2->vec_next = next;
  h2->key_and_value = Fcopy_sequence (h1->key_and_value);
  h2->hash = Fcopy_sequence (h1->hash);
  h2->next = Fcopy_sequence (h1->next);
  h2->index = Fcopy_sequence (h1->index);
  XSET_HASH_TABLE (table, h2);

  /* Maybe add this hash table to the list of all weak hash tables.  */
  if (!NILP (h2->weak))
    {
      h2->next_weak = Vweak_hash_tables;
      Vweak_hash_tables = table;
    }

  return table;
}


/* Resize hash table H if it's too full.  If H cannot be resized
   because it's already too large, throw an error.  */

static INLINE void
maybe_resize_hash_table (h)
     struct Lisp_Hash_Table *h;
{
  if (NILP (h->next_free))
    {
      int old_size = HASH_TABLE_SIZE (h);
      int i, new_size, index_size;

      if (INTEGERP (h->rehash_size))
	new_size = old_size + XFASTINT (h->rehash_size);
      else
	new_size = old_size * XFLOATINT (h->rehash_size);
      new_size = max (old_size + 1, new_size);
      index_size = next_almost_prime ((int)
				      (new_size
				       / XFLOATINT (h->rehash_threshold)));
      if (max (index_size, 2 * new_size) & ~VALMASK)
	error ("Hash table too large to resize");

      h->key_and_value = larger_vector (h->key_and_value, 2 * new_size, Qnil);
      h->next = larger_vector (h->next, new_size, Qnil);
      h->hash = larger_vector (h->hash, new_size, Qnil);
      h->index = Fmake_vector (make_number (index_size), Qnil);

      /* Update the free list.  Do it so that new entries are added at
         the end of the free list.  This makes some operations like
         maphash faster.  */
      for (i = old_size; i < new_size - 1; ++i)
	HASH_NEXT (h, i) = make_number (i + 1);

      if (!NILP (h->next_free))
	{
	  Lisp_Object last, next;

	  last = h->next_free;
	  while (next = HASH_NEXT (h, XFASTINT (last)),
		 !NILP (next))
	    last = next;

	  HASH_NEXT (h, XFASTINT (last)) = make_number (old_size);
	}
      else
	XSETFASTINT (h->next_free, old_size);

      /* Rehash.  */
      for (i = 0; i < old_size; ++i)
	if (!NILP (HASH_HASH (h, i)))
	  {
	    unsigned hash_code = XUINT (HASH_HASH (h, i));
	    int start_of_bucket = hash_code % XVECTOR (h->index)->size;
	    HASH_NEXT (h, i) = HASH_INDEX (h, start_of_bucket);
	    HASH_INDEX (h, start_of_bucket) = make_number (i);
	  }
    }
}


/* Lookup KEY in hash table H.  If HASH is non-null, return in *HASH
   the hash code of KEY.  Value is the index of the entry in H
   matching KEY, or -1 if not found.  */

int
hash_lookup (h, key, hash)
     struct Lisp_Hash_Table *h;
     Lisp_Object key;
     unsigned *hash;
{
  unsigned hash_code;
  int start_of_bucket;
  Lisp_Object idx;

  hash_code = h->hashfn (h, key);
  if (hash)
    *hash = hash_code;

  start_of_bucket = hash_code % XVECTOR (h->index)->size;
  idx = HASH_INDEX (h, start_of_bucket);

  /* We need not gcpro idx since it's either an integer or nil.  */
  while (!NILP (idx))
    {
      int i = XFASTINT (idx);
      if (EQ (key, HASH_KEY (h, i))
	  || (h->cmpfn
	      && h->cmpfn (h, key, hash_code,
			   HASH_KEY (h, i), XUINT (HASH_HASH (h, i)))))
	break;
      idx = HASH_NEXT (h, i);
    }

  return NILP (idx) ? -1 : XFASTINT (idx);
}


/* Put an entry into hash table H that associates KEY with VALUE.
   HASH is a previously computed hash code of KEY.
   Value is the index of the entry in H matching KEY.  */

int
hash_put (h, key, value, hash)
     struct Lisp_Hash_Table *h;
     Lisp_Object key, value;
     unsigned hash;
{
  int start_of_bucket, i;

  xassert ((hash & ~VALMASK) == 0);

  /* Increment count after resizing because resizing may fail.  */
  maybe_resize_hash_table (h);
  h->count = make_number (XFASTINT (h->count) + 1);

  /* Store key/value in the key_and_value vector.  */
  i = XFASTINT (h->next_free);
  h->next_free = HASH_NEXT (h, i);
  HASH_KEY (h, i) = key;
  HASH_VALUE (h, i) = value;

  /* Remember its hash code.  */
  HASH_HASH (h, i) = make_number (hash);

  /* Add new entry to its collision chain.  */
  start_of_bucket = hash % XVECTOR (h->index)->size;
  HASH_NEXT (h, i) = HASH_INDEX (h, start_of_bucket);
  HASH_INDEX (h, start_of_bucket) = make_number (i);
  return i;
}


/* Remove the entry matching KEY from hash table H, if there is one.  */

void
hash_remove (h, key)
     struct Lisp_Hash_Table *h;
     Lisp_Object key;
{
  unsigned hash_code;
  int start_of_bucket;
  Lisp_Object idx, prev;

  hash_code = h->hashfn (h, key);
  start_of_bucket = hash_code % XVECTOR (h->index)->size;
  idx = HASH_INDEX (h, start_of_bucket);
  prev = Qnil;

  /* We need not gcpro idx, prev since they're either integers or nil.  */
  while (!NILP (idx))
    {
      int i = XFASTINT (idx);

      if (EQ (key, HASH_KEY (h, i))
	  || (h->cmpfn
	      && h->cmpfn (h, key, hash_code,
			   HASH_KEY (h, i), XUINT (HASH_HASH (h, i)))))
	{
	  /* Take entry out of collision chain.  */
	  if (NILP (prev))
	    HASH_INDEX (h, start_of_bucket) = HASH_NEXT (h, i);
	  else
	    HASH_NEXT (h, XFASTINT (prev)) = HASH_NEXT (h, i);

	  /* Clear slots in key_and_value and add the slots to
	     the free list.  */
	  HASH_KEY (h, i) = HASH_VALUE (h, i) = HASH_HASH (h, i) = Qnil;
	  HASH_NEXT (h, i) = h->next_free;
	  h->next_free = make_number (i);
	  h->count = make_number (XFASTINT (h->count) - 1);
	  xassert (XINT (h->count) >= 0);
	  break;
	}
      else
	{
	  prev = idx;
	  idx = HASH_NEXT (h, i);
	}
    }
}


/* Clear hash table H.  */

void
hash_clear (h)
     struct Lisp_Hash_Table *h;
{
  if (XFASTINT (h->count) > 0)
    {
      int i, size = HASH_TABLE_SIZE (h);

      for (i = 0; i < size; ++i)
	{
	  HASH_NEXT (h, i) = i < size - 1 ? make_number (i + 1) : Qnil;
	  HASH_KEY (h, i) = Qnil;
	  HASH_VALUE (h, i) = Qnil;
	  HASH_HASH (h, i) = Qnil;
	}

      for (i = 0; i < XVECTOR (h->index)->size; ++i)
	XVECTOR (h->index)->contents[i] = Qnil;

      h->next_free = make_number (0);
      h->count = make_number (0);
    }
}



/************************************************************************
			   Weak Hash Tables
 ************************************************************************/

/* Sweep weak hash table H.  REMOVE_ENTRIES_P non-zero means remove
   entries from the table that don't survive the current GC.
   REMOVE_ENTRIES_P zero means mark entries that are in use.  Value is
   non-zero if anything was marked.  */

static int
sweep_weak_table (h, remove_entries_p)
     struct Lisp_Hash_Table *h;
     int remove_entries_p;
{
  int bucket, n, marked;

  n = XVECTOR (h->index)->size & ~ARRAY_MARK_FLAG;
  marked = 0;

  for (bucket = 0; bucket < n; ++bucket)
    {
      Lisp_Object idx, next, prev;

      /* Follow collision chain, removing entries that
	 don't survive this garbage collection.  */
      prev = Qnil;
      for (idx = HASH_INDEX (h, bucket); !GC_NILP (idx); idx = next)
	{
	  int i = XFASTINT (idx);
	  int key_known_to_survive_p = survives_gc_p (HASH_KEY (h, i));
	  int value_known_to_survive_p = survives_gc_p (HASH_VALUE (h, i));
	  int remove_p;

	  if (EQ (h->weak, Qkey))
	    remove_p = !key_known_to_survive_p;
	  else if (EQ (h->weak, Qvalue))
	    remove_p = !value_known_to_survive_p;
	  else if (EQ (h->weak, Qkey_or_value))
	    remove_p = !(key_known_to_survive_p || value_known_to_survive_p);
	  else if (EQ (h->weak, Qkey_and_value))
	    remove_p = !(key_known_to_survive_p && value_known_to_survive_p);
	  else
	    abort ();

	  next = HASH_NEXT (h, i);

	  if (remove_entries_p)
	    {
	      if (remove_p)
		{
		  /* Take out of collision chain.  */
		  if (GC_NILP (prev))
		    HASH_INDEX (h, bucket) = next;
		  else
		    HASH_NEXT (h, XFASTINT (prev)) = next;

		  /* Add to free list.  */
		  HASH_NEXT (h, i) = h->next_free;
		  h->next_free = idx;

		  /* Clear key, value, and hash.  */
		  HASH_KEY (h, i) = HASH_VALUE (h, i) = Qnil;
		  HASH_HASH (h, i) = Qnil;

		  h->count = make_number (XFASTINT (h->count) - 1);
		}
	    }
	  else
	    {
	      if (!remove_p)
		{
		  /* Make sure key and value survive.  */
		  if (!key_known_to_survive_p)
		    {
		      mark_object (&HASH_KEY (h, i));
		      marked = 1;
		    }

		  if (!value_known_to_survive_p)
		    {
		      mark_object (&HASH_VALUE (h, i));
		      marked = 1;
		    }
		}
	    }
	}
    }

  return marked;
}

/* Remove elements from weak hash tables that don't survive the
   current garbage collection.  Remove weak tables that don't survive
   from Vweak_hash_tables.  Called from gc_sweep.  */

void
sweep_weak_hash_tables ()
{
  Lisp_Object table, used, next;
  struct Lisp_Hash_Table *h;
  int marked;

  /* Mark all keys and values that are in use.  Keep on marking until
     there is no more change.  This is necessary for cases like
     value-weak table A containing an entry X -> Y, where Y is used in a
     key-weak table B, Z -> Y.  If B comes after A in the list of weak
     tables, X -> Y might be removed from A, although when looking at B
     one finds that it shouldn't.  */
  do
    {
      marked = 0;
      for (table = Vweak_hash_tables; !GC_NILP (table); table = h->next_weak)
	{
	  h = XHASH_TABLE (table);
	  if (h->size & ARRAY_MARK_FLAG)
	    marked |= sweep_weak_table (h, 0);
	}
    }
  while (marked);

  /* Remove tables and entries that aren't used.  */
  for (table = Vweak_hash_tables, used = Qnil; !GC_NILP (table); table = next)
    {
      h = XHASH_TABLE (table);
      next = h->next_weak;
      
      if (h->size & ARRAY_MARK_FLAG)
	{
	  /* TABLE is marked as used.  Sweep its contents.  */
	  if (XFASTINT (h->count) > 0)
	    sweep_weak_table (h, 1);

	  /* Add table to the list of used weak hash tables.  */
	  h->next_weak = used;
	  used = table;
	}
    }

  Vweak_hash_tables = used;
}



/***********************************************************************
			Hash Code Computation
 ***********************************************************************/

/* Maximum depth up to which to dive into Lisp structures.  */

#define SXHASH_MAX_DEPTH 3

/* Maximum length up to which to take list and vector elements into
   account.  */

#define SXHASH_MAX_LEN   7

/* Combine two integers X and Y for hashing.  */

#define SXHASH_COMBINE(X, Y)						\
     ((((unsigned)(X) << 4) + (((unsigned)(X) >> 24) & 0x0fffffff))	\
      + (unsigned)(Y))


/* Return a hash for string PTR which has length LEN.  The hash
   code returned is guaranteed to fit in a Lisp integer.  */

static unsigned
sxhash_string (ptr, len)
     unsigned char *ptr;
     int len;
{
  unsigned char *p = ptr;
  unsigned char *end = p + len;
  unsigned char c;
  unsigned hash = 0;

  while (p != end)
    {
      c = *p++;
      if (c >= 0140)
	c -= 40;
      hash = ((hash << 3) + (hash >> 28) + c);
    }

  return hash & VALMASK;
}


/* Return a hash for list LIST.  DEPTH is the current depth in the
   list.  We don't recurse deeper than SXHASH_MAX_DEPTH in it.  */

static unsigned
sxhash_list (list, depth)
     Lisp_Object list;
     int depth;
{
  unsigned hash = 0;
  int i;

  if (depth < SXHASH_MAX_DEPTH)
    for (i = 0;
	 CONSP (list) && i < SXHASH_MAX_LEN;
	 list = XCDR (list), ++i)
      {
	unsigned hash2 = sxhash (XCAR (list), depth + 1);
	hash = SXHASH_COMBINE (hash, hash2);
      }

  return hash;
}


/* Return a hash for vector VECTOR.  DEPTH is the current depth in
   the Lisp structure.  */

static unsigned
sxhash_vector (vec, depth)
     Lisp_Object vec;
     int depth;
{
  unsigned hash = XVECTOR (vec)->size;
  int i, n;

  n = min (SXHASH_MAX_LEN, XVECTOR (vec)->size);
  for (i = 0; i < n; ++i)
    {
      unsigned hash2 = sxhash (XVECTOR (vec)->contents[i], depth + 1);
      hash = SXHASH_COMBINE (hash, hash2);
    }

  return hash;
}


/* Return a hash for bool-vector VECTOR.  */

static unsigned
sxhash_bool_vector (vec)
     Lisp_Object vec;
{
  unsigned hash = XBOOL_VECTOR (vec)->size;
  int i, n;

  n = min (SXHASH_MAX_LEN, XBOOL_VECTOR (vec)->vector_size);
  for (i = 0; i < n; ++i)
    hash = SXHASH_COMBINE (hash, XBOOL_VECTOR (vec)->data[i]);

  return hash;
}


/* Return a hash code for OBJ.  DEPTH is the current depth in the Lisp
   structure.  Value is an unsigned integer clipped to VALMASK.  */

unsigned
sxhash (obj, depth)
     Lisp_Object obj;
     int depth;
{
  unsigned hash;

  if (depth > SXHASH_MAX_DEPTH)
    return 0;

  switch (XTYPE (obj))
    {
    case Lisp_Int:
      hash = XUINT (obj);
      break;

    case Lisp_Symbol:
      hash = sxhash_string (XSTRING (SYMBOL_NAME (obj))->data,
			    XSTRING (SYMBOL_NAME (obj))->size);
      break;

    case Lisp_Misc:
      hash = XUINT (obj);
      break;

    case Lisp_String:
      hash = sxhash_string (XSTRING (obj)->data, XSTRING (obj)->size);
      break;

      /* This can be everything from a vector to an overlay.  */
    case Lisp_Vectorlike:
      if (VECTORP (obj))
	/* According to the CL HyperSpec, two arrays are equal only if
	   they are `eq', except for strings and bit-vectors.  In
	   Emacs, this works differently.  We have to compare element
	   by element.  */
	hash = sxhash_vector (obj, depth);
      else if (BOOL_VECTOR_P (obj))
	hash = sxhash_bool_vector (obj);
      else
	/* Others are `equal' if they are `eq', so let's take their
	   address as hash.  */
	hash = XUINT (obj);
      break;

    case Lisp_Cons:
      hash = sxhash_list (obj, depth);
      break;

    case Lisp_Float:
      {
	unsigned char *p = (unsigned char *) &XFLOAT_DATA (obj);
	unsigned char *e = p + sizeof XFLOAT_DATA (obj);
	for (hash = 0; p < e; ++p)
	  hash = SXHASH_COMBINE (hash, *p);
	break;
      }

    default:
      abort ();
    }

  return hash & VALMASK;
}



/***********************************************************************
			    Lisp Interface
 ***********************************************************************/


DEFUN ("sxhash", Fsxhash, Ssxhash, 1, 1, 0,
       doc: /* Compute a hash code for OBJ and return it as integer.  */)
     (obj)
     Lisp_Object obj;
{
  unsigned hash = sxhash (obj, 0);;
  return make_number (hash);
}


DEFUN ("make-hash-table", Fmake_hash_table, Smake_hash_table, 0, MANY, 0,
       doc: /* Create and return a new hash table.
	  
Arguments are specified as keyword/argument pairs.  The following
arguments are defined:

:test TEST -- TEST must be a symbol that specifies how to compare
keys.  Default is `eql'.  Predefined are the tests `eq', `eql', and
`equal'.  User-supplied test and hash functions can be specified via
`define-hash-table-test'.

:size SIZE -- A hint as to how many elements will be put in the table.
Default is 65.

:rehash-size REHASH-SIZE - Indicates how to expand the table when it
fills up.  If REHASH-SIZE is an integer, add that many space.  If it
is a float, it must be > 1.0, and the new size is computed by
multiplying the old size with that factor.  Default is 1.5.

:rehash-threshold THRESHOLD -- THRESHOLD must a float > 0, and <= 1.0.
Resize the hash table when ratio of the number of entries in the
table.  Default is 0.8.

:weakness WEAK -- WEAK must be one of nil, t, `key', `value',
`key-or-value', or `key-and-value'.  If WEAK is not nil, the table
returned is a weak table.  Key/value pairs are removed from a weak
hash table when there are no non-weak references pointing to their
key, value, one of key or value, or both key and value, depending on
WEAK.  WEAK t is equivalent to `key-and-value'.  Default value of WEAK
is nil.

usage: (make-hash-table &rest KEYWORD-ARGS)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  Lisp_Object test, size, rehash_size, rehash_threshold, weak;
  Lisp_Object user_test, user_hash;
  char *used;
  int i;

  /* The vector `used' is used to keep track of arguments that
     have been consumed.  */
  used = (char *) alloca (nargs * sizeof *used);
  bzero (used, nargs * sizeof *used);

  /* See if there's a `:test TEST' among the arguments.  */
  i = get_key_arg (QCtest, nargs, args, used);
  test = i < 0 ? Qeql : args[i];
  if (!EQ (test, Qeq) && !EQ (test, Qeql) && !EQ (test, Qequal))
    {
      /* See if it is a user-defined test.  */
      Lisp_Object prop;

      prop = Fget (test, Qhash_table_test);
      if (!CONSP (prop) || !CONSP (XCDR (prop)))
	Fsignal (Qerror, list2 (build_string ("Invalid hash table test"),
				test));
      user_test = XCAR (prop);
      user_hash = XCAR (XCDR (prop));
    }
  else
    user_test = user_hash = Qnil;

  /* See if there's a `:size SIZE' argument.  */
  i = get_key_arg (QCsize, nargs, args, used);
  size = i < 0 ? make_number (DEFAULT_HASH_SIZE) : args[i];
  if (!INTEGERP (size) || XINT (size) < 0)
    Fsignal (Qerror,
	     list2 (build_string ("Invalid hash table size"),
		    size));

  /* Look for `:rehash-size SIZE'.  */
  i = get_key_arg (QCrehash_size, nargs, args, used);
  rehash_size = i < 0 ? make_float (DEFAULT_REHASH_SIZE) : args[i];
  if (!NUMBERP (rehash_size)
      || (INTEGERP (rehash_size) && XINT (rehash_size) <= 0)
      || XFLOATINT (rehash_size) <= 1.0)
    Fsignal (Qerror,
	     list2 (build_string ("Invalid hash table rehash size"),
		    rehash_size));

  /* Look for `:rehash-threshold THRESHOLD'.  */
  i = get_key_arg (QCrehash_threshold, nargs, args, used);
  rehash_threshold = i < 0 ? make_float (DEFAULT_REHASH_THRESHOLD) : args[i];
  if (!FLOATP (rehash_threshold)
      || XFLOATINT (rehash_threshold) <= 0.0
      || XFLOATINT (rehash_threshold) > 1.0)
    Fsignal (Qerror,
	     list2 (build_string ("Invalid hash table rehash threshold"),
		    rehash_threshold));

  /* Look for `:weakness WEAK'.  */
  i = get_key_arg (QCweakness, nargs, args, used);
  weak = i < 0 ? Qnil : args[i];
  if (EQ (weak, Qt))
    weak = Qkey_and_value;
  if (!NILP (weak)
      && !EQ (weak, Qkey)
      && !EQ (weak, Qvalue)
      && !EQ (weak, Qkey_or_value)
      && !EQ (weak, Qkey_and_value))
    Fsignal (Qerror, list2 (build_string ("Invalid hash table weakness"),
			    weak));

  /* Now, all args should have been used up, or there's a problem.  */
  for (i = 0; i < nargs; ++i)
    if (!used[i])
      Fsignal (Qerror,
	       list2 (build_string ("Invalid argument list"), args[i]));

  return make_hash_table (test, size, rehash_size, rehash_threshold, weak,
			  user_test, user_hash);
}


DEFUN ("copy-hash-table", Fcopy_hash_table, Scopy_hash_table, 1, 1, 0,
       doc: /* Return a copy of hash table TABLE.  */)
     (table)
     Lisp_Object table;
{
  return copy_hash_table (check_hash_table (table));
}


DEFUN ("makehash", Fmakehash, Smakehash, 0, 1, 0,
       doc: /* Create a new hash table.
	  
Optional first argument TEST specifies how to compare keys in the
table.  Predefined tests are `eq', `eql', and `equal'.  Default is
`eql'.  New tests can be defined with `define-hash-table-test'.  */)
     (test)
     Lisp_Object test;
{
  Lisp_Object args[2];
  args[0] = QCtest;
  args[1] = NILP (test) ? Qeql : test;
  return Fmake_hash_table (2, args);
}


DEFUN ("hash-table-count", Fhash_table_count, Shash_table_count, 1, 1, 0,
       doc: /* Return the number of elements in TABLE.  */)
     (table)
     Lisp_Object table;
{
  return check_hash_table (table)->count;
}


DEFUN ("hash-table-rehash-size", Fhash_table_rehash_size,
       Shash_table_rehash_size, 1, 1, 0,
       doc: /* Return the current rehash size of TABLE.  */)
     (table)
     Lisp_Object table;
{
  return check_hash_table (table)->rehash_size;
}


DEFUN ("hash-table-rehash-threshold", Fhash_table_rehash_threshold,
       Shash_table_rehash_threshold, 1, 1, 0,
       doc: /* Return the current rehash threshold of TABLE.  */)
     (table)
     Lisp_Object table;
{
  return check_hash_table (table)->rehash_threshold;
}


DEFUN ("hash-table-size", Fhash_table_size, Shash_table_size, 1, 1, 0,
       doc: /* Return the size of TABLE.
The size can be used as an argument to `make-hash-table' to create
a hash table than can hold as many elements of TABLE holds
without need for resizing.  */)
     (table)
       Lisp_Object table;
{
  struct Lisp_Hash_Table *h = check_hash_table (table);
  return make_number (HASH_TABLE_SIZE (h));
}


DEFUN ("hash-table-test", Fhash_table_test, Shash_table_test, 1, 1, 0,
       doc: /* Return the test TABLE uses.  */)
     (table)
     Lisp_Object table;
{
  return check_hash_table (table)->test;
}


DEFUN ("hash-table-weakness", Fhash_table_weakness, Shash_table_weakness,
       1, 1, 0,
       doc: /* Return the weakness of TABLE.  */)
     (table)
     Lisp_Object table;
{
  return check_hash_table (table)->weak;
}


DEFUN ("hash-table-p", Fhash_table_p, Shash_table_p, 1, 1, 0,
       doc: /* Return t if OBJ is a Lisp hash table object.  */)
     (obj)
     Lisp_Object obj;
{
  return HASH_TABLE_P (obj) ? Qt : Qnil;
}


DEFUN ("clrhash", Fclrhash, Sclrhash, 1, 1, 0,
       doc: /* Clear hash table TABLE.  */)
     (table)
     Lisp_Object table;
{
  hash_clear (check_hash_table (table));
  return Qnil;
}


DEFUN ("gethash", Fgethash, Sgethash, 2, 3, 0,
       doc: /* Look up KEY in TABLE and return its associated value.
If KEY is not found, return DFLT which defaults to nil.  */)
     (key, table, dflt)
     Lisp_Object key, table, dflt;
{
  struct Lisp_Hash_Table *h = check_hash_table (table);
  int i = hash_lookup (h, key, NULL);
  return i >= 0 ? HASH_VALUE (h, i) : dflt;
}


DEFUN ("puthash", Fputhash, Sputhash, 3, 3, 0,
       doc: /* Associate KEY with VALUE in hash table TABLE.
If KEY is already present in table, replace its current value with
VALUE.  */)
     (key, value, table)
     Lisp_Object key, value, table;
{
  struct Lisp_Hash_Table *h = check_hash_table (table);
  int i;
  unsigned hash;

  i = hash_lookup (h, key, &hash);
  if (i >= 0)
    HASH_VALUE (h, i) = value;
  else
    hash_put (h, key, value, hash);

  return value;
}


DEFUN ("remhash", Fremhash, Sremhash, 2, 2, 0,
       doc: /* Remove KEY from TABLE.  */)
     (key, table)
     Lisp_Object key, table;
{
  struct Lisp_Hash_Table *h = check_hash_table (table);
  hash_remove (h, key);
  return Qnil;
}


DEFUN ("maphash", Fmaphash, Smaphash, 2, 2, 0,
       doc: /* Call FUNCTION for all entries in hash table TABLE.
FUNCTION is called with 2 arguments KEY and VALUE.  */)
     (function, table)
     Lisp_Object function, table;
{
  struct Lisp_Hash_Table *h = check_hash_table (table);
  Lisp_Object args[3];
  int i;

  for (i = 0; i < HASH_TABLE_SIZE (h); ++i)
    if (!NILP (HASH_HASH (h, i)))
      {
	args[0] = function;
	args[1] = HASH_KEY (h, i);
	args[2] = HASH_VALUE (h, i);
	Ffuncall (3, args);
      }

  return Qnil;
}


DEFUN ("define-hash-table-test", Fdefine_hash_table_test,
       Sdefine_hash_table_test, 3, 3, 0,
       doc: /* Define a new hash table test with name NAME, a symbol.
	  
In hash tables created with NAME specified as test, use TEST to
compare keys, and HASH for computing hash codes of keys.

TEST must be a function taking two arguments and returning non-nil if
both arguments are the same.  HASH must be a function taking one
argument and return an integer that is the hash code of the argument.
Hash code computation should use the whole value range of integers,
including negative integers.  */)
     (name, test, hash)
     Lisp_Object name, test, hash;
{
  return Fput (name, Qhash_table_test, list2 (test, hash));
}



/************************************************************************
				 MD5
 ************************************************************************/

#include "md5.h"
#include "coding.h"

DEFUN ("md5", Fmd5, Smd5, 1, 5, 0,
       doc: /* Return MD5 message digest of OBJECT, a buffer or string.
	  
A message digest is a cryptographic checksum of a document, and the
algorithm to calculate it is defined in RFC 1321.

The two optional arguments START and END are character positions
specifying for which part of OBJECT the message digest should be
computed.  If nil or omitted, the digest is computed for the whole
OBJECT.

The MD5 message digest is computed from the result of encoding the
text in a coding system, not directly from the internal Emacs form of
the text.  The optional fourth argument CODING-SYSTEM specifies which
coding system to encode the text with.  It should be the same coding
system that you used or will use when actually writing the text into a
file.

If CODING-SYSTEM is nil or omitted, the default depends on OBJECT.  If
OBJECT is a buffer, the default for CODING-SYSTEM is whatever coding
system would be chosen by default for writing this text into a file.

If OBJECT is a string, the most preferred coding system (see the
command `prefer-coding-system') is used.

If NOERROR is non-nil, silently assume the `raw-text' coding if the
guesswork fails.  Normally, an error is signaled in such case.  */)
     (object, start, end, coding_system, noerror)
     Lisp_Object object, start, end, coding_system, noerror;
{
  unsigned char digest[16];
  unsigned char value[33];
  int i;
  int size;
  int size_byte = 0;
  int start_char = 0, end_char = 0;
  int start_byte = 0, end_byte = 0;
  register int b, e;
  register struct buffer *bp;
  int temp;

  if (STRINGP (object))
    {
      if (NILP (coding_system))
	{
	  /* Decide the coding-system to encode the data with.  */

	  if (STRING_MULTIBYTE (object))
	    /* use default, we can't guess correct value */
	    coding_system = SYMBOL_VALUE (XCAR (Vcoding_category_list));
	  else 
	    coding_system = Qraw_text;
	}
      
      if (NILP (Fcoding_system_p (coding_system)))
	{
	  /* Invalid coding system.  */
	  
	  if (!NILP (noerror))
	    coding_system = Qraw_text;
	  else
	    while (1)
	      Fsignal (Qcoding_system_error, Fcons (coding_system, Qnil));
	}

      if (STRING_MULTIBYTE (object))
	object = code_convert_string1 (object, coding_system, Qnil, 1);

      size = XSTRING (object)->size;
      size_byte = STRING_BYTES (XSTRING (object));

      if (!NILP (start))
	{
	  CHECK_NUMBER (start);

	  start_char = XINT (start);

	  if (start_char < 0)
	    start_char += size;

	  start_byte = string_char_to_byte (object, start_char);
	}

      if (NILP (end))
	{
	  end_char = size;
	  end_byte = size_byte;
	}
      else
	{
	  CHECK_NUMBER (end);
	  
	  end_char = XINT (end);

	  if (end_char < 0)
	    end_char += size;
	  
	  end_byte = string_char_to_byte (object, end_char);
	}
      
      if (!(0 <= start_char && start_char <= end_char && end_char <= size))
	args_out_of_range_3 (object, make_number (start_char),
			     make_number (end_char));
    }
  else
    {
      CHECK_BUFFER (object);

      bp = XBUFFER (object);
	  
      if (NILP (start))
	b = BUF_BEGV (bp);
      else
	{
	  CHECK_NUMBER_COERCE_MARKER (start);
	  b = XINT (start);
	}

      if (NILP (end))
	e = BUF_ZV (bp);
      else
	{
	  CHECK_NUMBER_COERCE_MARKER (end);
	  e = XINT (end);
	}
      
      if (b > e)
	temp = b, b = e, e = temp;
      
      if (!(BUF_BEGV (bp) <= b && e <= BUF_ZV (bp)))
	args_out_of_range (start, end);
      
      if (NILP (coding_system))
	{
	  /* Decide the coding-system to encode the data with. 
	     See fileio.c:Fwrite-region */

	  if (!NILP (Vcoding_system_for_write))
	    coding_system = Vcoding_system_for_write;
	  else
	    {
	      int force_raw_text = 0;

	      coding_system = XBUFFER (object)->buffer_file_coding_system;
	      if (NILP (coding_system)
		  || NILP (Flocal_variable_p (Qbuffer_file_coding_system, Qnil)))
		{
		  coding_system = Qnil;
		  if (NILP (current_buffer->enable_multibyte_characters))
		    force_raw_text = 1;
		}

	      if (NILP (coding_system) && !NILP (Fbuffer_file_name(object)))
		{
		  /* Check file-coding-system-alist.  */
		  Lisp_Object args[4], val;
		  
		  args[0] = Qwrite_region; args[1] = start; args[2] = end;
		  args[3] = Fbuffer_file_name(object);
		  val = Ffind_operation_coding_system (4, args);
		  if (CONSP (val) && !NILP (XCDR (val)))
		    coding_system = XCDR (val);
		}

	      if (NILP (coding_system)
		  && !NILP (XBUFFER (object)->buffer_file_coding_system))
		{
		  /* If we still have not decided a coding system, use the
		     default value of buffer-file-coding-system.  */
		  coding_system = XBUFFER (object)->buffer_file_coding_system;
		}

	      if (!force_raw_text
		  && !NILP (Ffboundp (Vselect_safe_coding_system_function)))
		/* Confirm that VAL can surely encode the current region.  */
		coding_system = call4 (Vselect_safe_coding_system_function,
				       make_number (b), make_number (e),
				       coding_system, Qnil);

	      if (force_raw_text)
		coding_system = Qraw_text;
	    }

	  if (NILP (Fcoding_system_p (coding_system)))
	    {
	      /* Invalid coding system.  */

	      if (!NILP (noerror))
		coding_system = Qraw_text;
	      else
		while (1)
		  Fsignal (Qcoding_system_error, Fcons (coding_system, Qnil));
	    }
	}

      object = make_buffer_string (b, e, 0);

      if (STRING_MULTIBYTE (object))
	object = code_convert_string1 (object, coding_system, Qnil, 1);
    }

  md5_buffer (XSTRING (object)->data + start_byte, 
	      STRING_BYTES(XSTRING (object)) - (size_byte - end_byte), 
	      digest);

  for (i = 0; i < 16; i++)
    sprintf (&value[2 * i], "%02x", digest[i]);
  value[32] = '\0';

  return make_string (value, 32);
}


void
syms_of_fns ()
{
  /* Hash table stuff.  */
  Qhash_table_p = intern ("hash-table-p");
  staticpro (&Qhash_table_p);
  Qeq = intern ("eq");
  staticpro (&Qeq);
  Qeql = intern ("eql");
  staticpro (&Qeql);
  Qequal = intern ("equal");
  staticpro (&Qequal);
  QCtest = intern (":test");
  staticpro (&QCtest);
  QCsize = intern (":size");
  staticpro (&QCsize);
  QCrehash_size = intern (":rehash-size");
  staticpro (&QCrehash_size);
  QCrehash_threshold = intern (":rehash-threshold");
  staticpro (&QCrehash_threshold);
  QCweakness = intern (":weakness");
  staticpro (&QCweakness);
  Qkey = intern ("key");
  staticpro (&Qkey);
  Qvalue = intern ("value");
  staticpro (&Qvalue);
  Qhash_table_test = intern ("hash-table-test");
  staticpro (&Qhash_table_test);
  Qkey_or_value = intern ("key-or-value");
  staticpro (&Qkey_or_value);
  Qkey_and_value = intern ("key-and-value");
  staticpro (&Qkey_and_value);

  defsubr (&Ssxhash);
  defsubr (&Smake_hash_table);
  defsubr (&Scopy_hash_table);
  defsubr (&Smakehash);
  defsubr (&Shash_table_count);
  defsubr (&Shash_table_rehash_size);
  defsubr (&Shash_table_rehash_threshold);
  defsubr (&Shash_table_size);
  defsubr (&Shash_table_test);
  defsubr (&Shash_table_weakness);
  defsubr (&Shash_table_p);
  defsubr (&Sclrhash);
  defsubr (&Sgethash);
  defsubr (&Sputhash);
  defsubr (&Sremhash);
  defsubr (&Smaphash);
  defsubr (&Sdefine_hash_table_test);

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

  require_nesting_list = Qnil;
  staticpro (&require_nesting_list);

  Fset (Qyes_or_no_p_history, Qnil);

  DEFVAR_LISP ("features", &Vfeatures,
    doc: /* A list of symbols which are the features of the executing emacs.
Used by `featurep' and `require', and altered by `provide'.  */);
  Vfeatures = Qnil;
  Qsubfeatures = intern ("subfeatures");
  staticpro (&Qsubfeatures);

  DEFVAR_BOOL ("use-dialog-box", &use_dialog_box,
    doc: /* *Non-nil means mouse commands use dialog boxes to ask questions.
This applies to `y-or-n-p' and `yes-or-no-p' questions asked by commands
invoked by mouse clicks and mouse menu items.  */);
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
  defsubr (&Ssubstring_no_properties);
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
  defsubr (&Slax_plist_get);
  defsubr (&Slax_plist_put);
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
  defsubr (&Soptimize_char_table);
  defsubr (&Smap_char_table);
  defsubr (&Snconc);
  defsubr (&Smapcar);
  defsubr (&Smapc);
  defsubr (&Smapconcat);
  defsubr (&Sy_or_n_p);
  defsubr (&Syes_or_no_p);
  defsubr (&Sload_average);
  defsubr (&Sfeaturep);
  defsubr (&Srequire);
  defsubr (&Sprovide);
  defsubr (&Splist_member);
  defsubr (&Swidget_put);
  defsubr (&Swidget_get);
  defsubr (&Swidget_apply);
  defsubr (&Sbase64_encode_region);
  defsubr (&Sbase64_decode_region);
  defsubr (&Sbase64_encode_string);
  defsubr (&Sbase64_decode_string);
  defsubr (&Smd5);
}


void
init_fns ()
{
  Vweak_hash_tables = Qnil;
}
