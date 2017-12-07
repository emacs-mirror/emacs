/* Random utility Lisp functions.

Copyright (C) 1985-1987, 1993-1995, 1997-2017 Free Software Foundation,
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
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>

#include <stdlib.h>
#include <unistd.h>
#include <filevercmp.h>
#include <intprops.h>
#include <vla.h>
#include <errno.h>

#include "lisp.h"
#include "character.h"
#include "coding.h"
#include "composite.h"
#include "buffer.h"
#include "intervals.h"
#include "window.h"
#include "puresize.h"
#include "gnutls.h"

#if defined WINDOWSNT && defined HAVE_GNUTLS3
# define gnutls_rnd w32_gnutls_rnd
#endif

static void sort_vector_copy (Lisp_Object, ptrdiff_t,
			      Lisp_Object *restrict, Lisp_Object *restrict);
enum equal_kind { EQUAL_NO_QUIT, EQUAL_PLAIN, EQUAL_INCLUDING_PROPERTIES };
static bool internal_equal (Lisp_Object, Lisp_Object,
			    enum equal_kind, int, Lisp_Object);

DEFUN ("identity", Fidentity, Sidentity, 1, 1, 0,
       doc: /* Return the argument unchanged.  */
       attributes: const)
  (Lisp_Object arg)
{
  return arg;
}

DEFUN ("random", Frandom, Srandom, 0, 1, 0,
       doc: /* Return a pseudo-random number.
All integers representable in Lisp, i.e. between `most-negative-fixnum'
and `most-positive-fixnum', inclusive, are equally likely.

With positive integer LIMIT, return random number in interval [0,LIMIT).
With argument t, set the random number seed from the system's entropy
pool if available, otherwise from less-random volatile data such as the time.
With a string argument, set the seed based on the string's contents.
Other values of LIMIT are ignored.

See Info node `(elisp)Random Numbers' for more details.  */)
  (Lisp_Object limit)
{
  EMACS_INT val;

  if (EQ (limit, Qt))
    init_random ();
  else if (STRINGP (limit))
    seed_random (SSDATA (limit), SBYTES (limit));

  val = get_random ();
  if (INTEGERP (limit) && 0 < XINT (limit))
    while (true)
      {
	/* Return the remainder, except reject the rare case where
	   get_random returns a number so close to INTMASK that the
	   remainder isn't random.  */
	EMACS_INT remainder = val % XINT (limit);
	if (val - remainder <= INTMASK - XINT (limit) + 1)
	  return make_number (remainder);
	val = get_random ();
      }
  return make_number (val);
}

/* Random data-structure functions.  */

DEFUN ("length", Flength, Slength, 1, 1, 0,
       doc: /* Return the length of vector, list or string SEQUENCE.
A byte-code function object is also allowed.
If the string contains multibyte characters, this is not necessarily
the number of bytes in the string; it is the number of characters.
To get the number of bytes, use `string-bytes'.  */)
  (register Lisp_Object sequence)
{
  register Lisp_Object val;

  if (STRINGP (sequence))
    XSETFASTINT (val, SCHARS (sequence));
  else if (VECTORP (sequence))
    XSETFASTINT (val, ASIZE (sequence));
  else if (CHAR_TABLE_P (sequence))
    XSETFASTINT (val, MAX_CHAR);
  else if (BOOL_VECTOR_P (sequence))
    XSETFASTINT (val, bool_vector_size (sequence));
  else if (COMPILEDP (sequence) || RECORDP (sequence))
    XSETFASTINT (val, PVSIZE (sequence));
  else if (CONSP (sequence))
    {
      intptr_t i = 0;
      FOR_EACH_TAIL (sequence)
	i++;
      CHECK_LIST_END (sequence, sequence);
      if (MOST_POSITIVE_FIXNUM < i)
	error ("List too long");
      val = make_number (i);
    }
  else if (NILP (sequence))
    XSETFASTINT (val, 0);
  else
    wrong_type_argument (Qsequencep, sequence);

  return val;
}

DEFUN ("safe-length", Fsafe_length, Ssafe_length, 1, 1, 0,
       doc: /* Return the length of a list, but avoid error or infinite loop.
This function never gets an error.  If LIST is not really a list,
it returns 0.  If LIST is circular, it returns a finite value
which is at least the number of distinct elements.  */)
  (Lisp_Object list)
{
  intptr_t len = 0;
  FOR_EACH_TAIL_SAFE (list)
    len++;
  return make_fixnum_or_float (len);
}

DEFUN ("string-bytes", Fstring_bytes, Sstring_bytes, 1, 1, 0,
       doc: /* Return the number of bytes in STRING.
If STRING is multibyte, this may be greater than the length of STRING.  */)
  (Lisp_Object string)
{
  CHECK_STRING (string);
  return make_number (SBYTES (string));
}

DEFUN ("string-equal", Fstring_equal, Sstring_equal, 2, 2, 0,
       doc: /* Return t if two strings have identical contents.
Case is significant, but text properties are ignored.
Symbols are also allowed; their print names are used instead.  */)
  (register Lisp_Object s1, Lisp_Object s2)
{
  if (SYMBOLP (s1))
    s1 = SYMBOL_NAME (s1);
  if (SYMBOLP (s2))
    s2 = SYMBOL_NAME (s2);
  CHECK_STRING (s1);
  CHECK_STRING (s2);

  if (SCHARS (s1) != SCHARS (s2)
      || SBYTES (s1) != SBYTES (s2)
      || memcmp (SDATA (s1), SDATA (s2), SBYTES (s1)))
    return Qnil;
  return Qt;
}

DEFUN ("compare-strings", Fcompare_strings, Scompare_strings, 6, 7, 0,
       doc: /* Compare the contents of two strings, converting to multibyte if needed.
The arguments START1, END1, START2, and END2, if non-nil, are
positions specifying which parts of STR1 or STR2 to compare.  In
string STR1, compare the part between START1 (inclusive) and END1
\(exclusive).  If START1 is nil, it defaults to 0, the beginning of
the string; if END1 is nil, it defaults to the length of the string.
Likewise, in string STR2, compare the part between START2 and END2.
Like in `substring', negative values are counted from the end.

The strings are compared by the numeric values of their characters.
For instance, STR1 is "less than" STR2 if its first differing
character has a smaller numeric value.  If IGNORE-CASE is non-nil,
characters are converted to upper-case before comparing them.  Unibyte
strings are converted to multibyte for comparison.

The value is t if the strings (or specified portions) match.
If string STR1 is less, the value is a negative number N;
  - 1 - N is the number of characters that match at the beginning.
If string STR1 is greater, the value is a positive number N;
  N - 1 is the number of characters that match at the beginning.  */)
  (Lisp_Object str1, Lisp_Object start1, Lisp_Object end1, Lisp_Object str2,
   Lisp_Object start2, Lisp_Object end2, Lisp_Object ignore_case)
{
  ptrdiff_t from1, to1, from2, to2, i1, i1_byte, i2, i2_byte;

  CHECK_STRING (str1);
  CHECK_STRING (str2);

  /* For backward compatibility, silently bring too-large positive end
     values into range.  */
  if (INTEGERP (end1) && SCHARS (str1) < XINT (end1))
    end1 = make_number (SCHARS (str1));
  if (INTEGERP (end2) && SCHARS (str2) < XINT (end2))
    end2 = make_number (SCHARS (str2));

  validate_subarray (str1, start1, end1, SCHARS (str1), &from1, &to1);
  validate_subarray (str2, start2, end2, SCHARS (str2), &from2, &to2);

  i1 = from1;
  i2 = from2;

  i1_byte = string_char_to_byte (str1, i1);
  i2_byte = string_char_to_byte (str2, i2);

  while (i1 < to1 && i2 < to2)
    {
      /* When we find a mismatch, we must compare the
	 characters, not just the bytes.  */
      int c1, c2;

      FETCH_STRING_CHAR_AS_MULTIBYTE_ADVANCE (c1, str1, i1, i1_byte);
      FETCH_STRING_CHAR_AS_MULTIBYTE_ADVANCE (c2, str2, i2, i2_byte);

      if (c1 == c2)
	continue;

      if (! NILP (ignore_case))
	{
	  c1 = XINT (Fupcase (make_number (c1)));
	  c2 = XINT (Fupcase (make_number (c2)));
	}

      if (c1 == c2)
	continue;

      /* Note that I1 has already been incremented
	 past the character that we are comparing;
	 hence we don't add or subtract 1 here.  */
      if (c1 < c2)
	return make_number (- i1 + from1);
      else
	return make_number (i1 - from1);
    }

  if (i1 < to1)
    return make_number (i1 - from1 + 1);
  if (i2 < to2)
    return make_number (- i1 + from1 - 1);

  return Qt;
}

DEFUN ("string-lessp", Fstring_lessp, Sstring_lessp, 2, 2, 0,
       doc: /* Return non-nil if STRING1 is less than STRING2 in lexicographic order.
Case is significant.
Symbols are also allowed; their print names are used instead.  */)
  (register Lisp_Object string1, Lisp_Object string2)
{
  register ptrdiff_t end;
  register ptrdiff_t i1, i1_byte, i2, i2_byte;

  if (SYMBOLP (string1))
    string1 = SYMBOL_NAME (string1);
  if (SYMBOLP (string2))
    string2 = SYMBOL_NAME (string2);
  CHECK_STRING (string1);
  CHECK_STRING (string2);

  i1 = i1_byte = i2 = i2_byte = 0;

  end = SCHARS (string1);
  if (end > SCHARS (string2))
    end = SCHARS (string2);

  while (i1 < end)
    {
      /* When we find a mismatch, we must compare the
	 characters, not just the bytes.  */
      int c1, c2;

      FETCH_STRING_CHAR_ADVANCE (c1, string1, i1, i1_byte);
      FETCH_STRING_CHAR_ADVANCE (c2, string2, i2, i2_byte);

      if (c1 != c2)
	return c1 < c2 ? Qt : Qnil;
    }
  return i1 < SCHARS (string2) ? Qt : Qnil;
}

DEFUN ("string-version-lessp", Fstring_version_lessp,
       Sstring_version_lessp, 2, 2, 0,
       doc: /* Return non-nil if S1 is less than S2, as version strings.

This function compares version strings S1 and S2:
   1) By prefix lexicographically.
   2) Then by version (similarly to version comparison of Debian's dpkg).
      Leading zeros in version numbers are ignored.
   3) If both prefix and version are equal, compare as ordinary strings.

For example, \"foo2.png\" compares less than \"foo12.png\".
Case is significant.
Symbols are also allowed; their print names are used instead.  */)
  (Lisp_Object string1, Lisp_Object string2)
{
  if (SYMBOLP (string1))
    string1 = SYMBOL_NAME (string1);
  if (SYMBOLP (string2))
    string2 = SYMBOL_NAME (string2);
  CHECK_STRING (string1);
  CHECK_STRING (string2);

  char *p1 = SSDATA (string1);
  char *p2 = SSDATA (string2);
  char *lim1 = p1 + SBYTES (string1);
  char *lim2 = p2 + SBYTES (string2);
  int cmp;

  while ((cmp = filevercmp (p1, p2)) == 0)
    {
      /* If the strings are identical through their first null bytes,
	 skip past identical prefixes and try again.  */
      ptrdiff_t size = strlen (p1) + 1;
      p1 += size;
      p2 += size;
      if (lim1 < p1)
	return lim2 < p2 ? Qnil : Qt;
      if (lim2 < p2)
	return Qnil;
    }

  return cmp < 0 ? Qt : Qnil;
}

DEFUN ("string-collate-lessp", Fstring_collate_lessp, Sstring_collate_lessp, 2, 4, 0,
       doc: /* Return t if first arg string is less than second in collation order.
Symbols are also allowed; their print names are used instead.

This function obeys the conventions for collation order in your
locale settings.  For example, punctuation and whitespace characters
might be considered less significant for sorting:

\(sort \\='("11" "12" "1 1" "1 2" "1.1" "1.2") \\='string-collate-lessp)
  => ("11" "1 1" "1.1" "12" "1 2" "1.2")

The optional argument LOCALE, a string, overrides the setting of your
current locale identifier for collation.  The value is system
dependent; a LOCALE \"en_US.UTF-8\" is applicable on POSIX systems,
while it would be, e.g., \"enu_USA.1252\" on MS-Windows systems.

If IGNORE-CASE is non-nil, characters are converted to lower-case
before comparing them.

To emulate Unicode-compliant collation on MS-Windows systems,
bind `w32-collate-ignore-punctuation' to a non-nil value, since
the codeset part of the locale cannot be \"UTF-8\" on MS-Windows.

If your system does not support a locale environment, this function
behaves like `string-lessp'.  */)
  (Lisp_Object s1, Lisp_Object s2, Lisp_Object locale, Lisp_Object ignore_case)
{
#if defined __STDC_ISO_10646__ || defined WINDOWSNT
  /* Check parameters.  */
  if (SYMBOLP (s1))
    s1 = SYMBOL_NAME (s1);
  if (SYMBOLP (s2))
    s2 = SYMBOL_NAME (s2);
  CHECK_STRING (s1);
  CHECK_STRING (s2);
  if (!NILP (locale))
    CHECK_STRING (locale);

  return (str_collate (s1, s2, locale, ignore_case) < 0) ? Qt : Qnil;

#else  /* !__STDC_ISO_10646__, !WINDOWSNT */
  return Fstring_lessp (s1, s2);
#endif /* !__STDC_ISO_10646__, !WINDOWSNT */
}

DEFUN ("string-collate-equalp", Fstring_collate_equalp, Sstring_collate_equalp, 2, 4, 0,
       doc: /* Return t if two strings have identical contents.
Symbols are also allowed; their print names are used instead.

This function obeys the conventions for collation order in your locale
settings.  For example, characters with different coding points but
the same meaning might be considered as equal, like different grave
accent Unicode characters:

\(string-collate-equalp (string ?\\uFF40) (string ?\\u1FEF))
  => t

The optional argument LOCALE, a string, overrides the setting of your
current locale identifier for collation.  The value is system
dependent; a LOCALE \"en_US.UTF-8\" is applicable on POSIX systems,
while it would be \"enu_USA.1252\" on MS Windows systems.

If IGNORE-CASE is non-nil, characters are converted to lower-case
before comparing them.

To emulate Unicode-compliant collation on MS-Windows systems,
bind `w32-collate-ignore-punctuation' to a non-nil value, since
the codeset part of the locale cannot be \"UTF-8\" on MS-Windows.

If your system does not support a locale environment, this function
behaves like `string-equal'.

Do NOT use this function to compare file names for equality.  */)
  (Lisp_Object s1, Lisp_Object s2, Lisp_Object locale, Lisp_Object ignore_case)
{
#if defined __STDC_ISO_10646__ || defined WINDOWSNT
  /* Check parameters.  */
  if (SYMBOLP (s1))
    s1 = SYMBOL_NAME (s1);
  if (SYMBOLP (s2))
    s2 = SYMBOL_NAME (s2);
  CHECK_STRING (s1);
  CHECK_STRING (s2);
  if (!NILP (locale))
    CHECK_STRING (locale);

  return (str_collate (s1, s2, locale, ignore_case) == 0) ? Qt : Qnil;

#else  /* !__STDC_ISO_10646__, !WINDOWSNT */
  return Fstring_equal (s1, s2);
#endif /* !__STDC_ISO_10646__, !WINDOWSNT */
}

static Lisp_Object concat (ptrdiff_t nargs, Lisp_Object *args,
			   enum Lisp_Type target_type, bool last_special);

/* ARGSUSED */
Lisp_Object
concat2 (Lisp_Object s1, Lisp_Object s2)
{
  return concat (2, ((Lisp_Object []) {s1, s2}), Lisp_String, 0);
}

/* ARGSUSED */
Lisp_Object
concat3 (Lisp_Object s1, Lisp_Object s2, Lisp_Object s3)
{
  return concat (3, ((Lisp_Object []) {s1, s2, s3}), Lisp_String, 0);
}

DEFUN ("append", Fappend, Sappend, 0, MANY, 0,
       doc: /* Concatenate all the arguments and make the result a list.
The result is a list whose elements are the elements of all the arguments.
Each argument may be a list, vector or string.
The last argument is not copied, just used as the tail of the new list.
usage: (append &rest SEQUENCES)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return concat (nargs, args, Lisp_Cons, 1);
}

DEFUN ("concat", Fconcat, Sconcat, 0, MANY, 0,
       doc: /* Concatenate all the arguments and make the result a string.
The result is a string whose elements are the elements of all the arguments.
Each argument may be a string or a list or vector of characters (integers).
usage: (concat &rest SEQUENCES)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return concat (nargs, args, Lisp_String, 0);
}

DEFUN ("vconcat", Fvconcat, Svconcat, 0, MANY, 0,
       doc: /* Concatenate all the arguments and make the result a vector.
The result is a vector whose elements are the elements of all the arguments.
Each argument may be a list, vector or string.
usage: (vconcat &rest SEQUENCES)   */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return concat (nargs, args, Lisp_Vectorlike, 0);
}


DEFUN ("copy-sequence", Fcopy_sequence, Scopy_sequence, 1, 1, 0,
       doc: /* Return a copy of a list, vector, string, char-table or record.
The elements of a list, vector or record are not copied; they are
shared with the original.
If the original sequence is empty, this function may return
the same empty object instead of its copy.  */)
  (Lisp_Object arg)
{
  if (NILP (arg)) return arg;

  if (RECORDP (arg))
    {
      return Frecord (PVSIZE (arg), XVECTOR (arg)->contents);
    }

  if (CHAR_TABLE_P (arg))
    {
      return copy_char_table (arg);
    }

  if (BOOL_VECTOR_P (arg))
    {
      EMACS_INT nbits = bool_vector_size (arg);
      ptrdiff_t nbytes = bool_vector_bytes (nbits);
      Lisp_Object val = make_uninit_bool_vector (nbits);
      memcpy (bool_vector_data (val), bool_vector_data (arg), nbytes);
      return val;
    }

  if (!CONSP (arg) && !VECTORP (arg) && !STRINGP (arg))
    wrong_type_argument (Qsequencep, arg);

  return concat (1, &arg, XTYPE (arg), 0);
}

/* This structure holds information of an argument of `concat' that is
   a string and has text properties to be copied.  */
struct textprop_rec
{
  ptrdiff_t argnum;		/* refer to ARGS (arguments of `concat') */
  ptrdiff_t from;		/* refer to ARGS[argnum] (argument string) */
  ptrdiff_t to;			/* refer to VAL (the target string) */
};

static Lisp_Object
concat (ptrdiff_t nargs, Lisp_Object *args,
	enum Lisp_Type target_type, bool last_special)
{
  Lisp_Object val;
  Lisp_Object tail;
  Lisp_Object this;
  ptrdiff_t toindex;
  ptrdiff_t toindex_byte = 0;
  EMACS_INT result_len;
  EMACS_INT result_len_byte;
  ptrdiff_t argnum;
  Lisp_Object last_tail;
  Lisp_Object prev;
  bool some_multibyte;
  /* When we make a multibyte string, we can't copy text properties
     while concatenating each string because the length of resulting
     string can't be decided until we finish the whole concatenation.
     So, we record strings that have text properties to be copied
     here, and copy the text properties after the concatenation.  */
  struct textprop_rec  *textprops = NULL;
  /* Number of elements in textprops.  */
  ptrdiff_t num_textprops = 0;
  USE_SAFE_ALLOCA;

  tail = Qnil;

  /* In append, the last arg isn't treated like the others */
  if (last_special && nargs > 0)
    {
      nargs--;
      last_tail = args[nargs];
    }
  else
    last_tail = Qnil;

  /* Check each argument.  */
  for (argnum = 0; argnum < nargs; argnum++)
    {
      this = args[argnum];
      if (!(CONSP (this) || NILP (this) || VECTORP (this) || STRINGP (this)
	    || COMPILEDP (this) || BOOL_VECTOR_P (this)))
	wrong_type_argument (Qsequencep, this);
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
      EMACS_INT len;
      this = args[argnum];
      len = XFASTINT (Flength (this));
      if (target_type == Lisp_String)
	{
	  /* We must count the number of bytes needed in the string
	     as well as the number of characters.  */
	  ptrdiff_t i;
	  Lisp_Object ch;
	  int c;
	  ptrdiff_t this_len_byte;

	  if (VECTORP (this) || COMPILEDP (this))
	    for (i = 0; i < len; i++)
	      {
		ch = AREF (this, i);
		CHECK_CHARACTER (ch);
		c = XFASTINT (ch);
		this_len_byte = CHAR_BYTES (c);
		if (STRING_BYTES_BOUND - result_len_byte < this_len_byte)
		  string_overflow ();
		result_len_byte += this_len_byte;
		if (! ASCII_CHAR_P (c) && ! CHAR_BYTE8_P (c))
		  some_multibyte = 1;
	      }
	  else if (BOOL_VECTOR_P (this) && bool_vector_size (this) > 0)
	    wrong_type_argument (Qintegerp, Faref (this, make_number (0)));
	  else if (CONSP (this))
	    for (; CONSP (this); this = XCDR (this))
	      {
		ch = XCAR (this);
		CHECK_CHARACTER (ch);
		c = XFASTINT (ch);
		this_len_byte = CHAR_BYTES (c);
		if (STRING_BYTES_BOUND - result_len_byte < this_len_byte)
		  string_overflow ();
		result_len_byte += this_len_byte;
		if (! ASCII_CHAR_P (c) && ! CHAR_BYTE8_P (c))
		  some_multibyte = 1;
	      }
	  else if (STRINGP (this))
	    {
	      if (STRING_MULTIBYTE (this))
		{
		  some_multibyte = 1;
		  this_len_byte = SBYTES (this);
		}
	      else
		this_len_byte = count_size_as_multibyte (SDATA (this),
							 SCHARS (this));
	      if (STRING_BYTES_BOUND - result_len_byte < this_len_byte)
		string_overflow ();
	      result_len_byte += this_len_byte;
	    }
	}

      result_len += len;
      if (MOST_POSITIVE_FIXNUM < result_len)
	memory_full (SIZE_MAX);
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
    SAFE_NALLOCA (textprops, 1, nargs);

  for (argnum = 0; argnum < nargs; argnum++)
    {
      Lisp_Object thislen;
      ptrdiff_t thisleni = 0;
      register ptrdiff_t thisindex = 0;
      register ptrdiff_t thisindex_byte = 0;

      this = args[argnum];
      if (!CONSP (this))
	thislen = Flength (this), thisleni = XINT (thislen);

      /* Between strings of the same kind, copy fast.  */
      if (STRINGP (this) && STRINGP (val)
	  && STRING_MULTIBYTE (this) == some_multibyte)
	{
	  ptrdiff_t thislen_byte = SBYTES (this);

	  memcpy (SDATA (val) + toindex_byte, SDATA (this), SBYTES (this));
	  if (string_intervals (this))
	    {
	      textprops[num_textprops].argnum = argnum;
	      textprops[num_textprops].from = 0;
	      textprops[num_textprops++].to = toindex;
	    }
	  toindex_byte += thislen_byte;
	  toindex += thisleni;
	}
      /* Copy a single-byte string to a multibyte string.  */
      else if (STRINGP (this) && STRINGP (val))
	{
	  if (string_intervals (this))
	    {
	      textprops[num_textprops].argnum = argnum;
	      textprops[num_textprops].from = 0;
	      textprops[num_textprops++].to = toindex;
	    }
	  toindex_byte += copy_text (SDATA (this),
				     SDATA (val) + toindex_byte,
				     SCHARS (this), 0, 1);
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
		  FETCH_STRING_CHAR_ADVANCE_NO_CHECK (c, this,
						      thisindex,
						      thisindex_byte);
		else
		  {
		    c = SREF (this, thisindex); thisindex++;
		    if (some_multibyte && !ASCII_CHAR_P (c))
		      c = BYTE8_TO_CHAR (c);
		  }
		XSETFASTINT (elt, c);
	      }
	    else if (BOOL_VECTOR_P (this))
	      {
		elt = bool_vector_ref (this, thisindex);
		thisindex++;
	      }
	    else
	      {
		elt = AREF (this, thisindex);
		thisindex++;
	      }

	    /* Store this element into the result.  */
	    if (toindex < 0)
	      {
		XSETCAR (tail, elt);
		prev = tail;
		tail = XCDR (tail);
	      }
	    else if (VECTORP (val))
	      {
		ASET (val, toindex, elt);
		toindex++;
	      }
	    else
	      {
		int c;
		CHECK_CHARACTER (elt);
		c = XFASTINT (elt);
		if (some_multibyte)
		  toindex_byte += CHAR_STRING (c, SDATA (val) + toindex_byte);
		else
		  SSET (val, toindex_byte++, c);
		toindex++;
	      }
	  }
    }
  if (!NILP (prev))
    XSETCDR (prev, last_tail);

  if (num_textprops > 0)
    {
      Lisp_Object props;
      ptrdiff_t last_to_end = -1;

      for (argnum = 0; argnum < num_textprops; argnum++)
	{
	  this = args[textprops[argnum].argnum];
	  props = text_property_list (this,
				      make_number (0),
				      make_number (SCHARS (this)),
				      Qnil);
	  /* If successive arguments have properties, be sure that the
	     value of `composition' property be the copy.  */
	  if (last_to_end == textprops[argnum].to)
	    make_composition_value_copy (props);
	  add_text_properties_from_list (val, props,
					 make_number (textprops[argnum].to));
	  last_to_end = textprops[argnum].to + SCHARS (this);
	}
    }

  SAFE_FREE ();
  return val;
}

static Lisp_Object string_char_byte_cache_string;
static ptrdiff_t string_char_byte_cache_charpos;
static ptrdiff_t string_char_byte_cache_bytepos;

void
clear_string_char_byte_cache (void)
{
  string_char_byte_cache_string = Qnil;
}

/* Return the byte index corresponding to CHAR_INDEX in STRING.  */

ptrdiff_t
string_char_to_byte (Lisp_Object string, ptrdiff_t char_index)
{
  ptrdiff_t i_byte;
  ptrdiff_t best_below, best_below_byte;
  ptrdiff_t best_above, best_above_byte;

  best_below = best_below_byte = 0;
  best_above = SCHARS (string);
  best_above_byte = SBYTES (string);
  if (best_above == best_above_byte)
    return char_index;

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
      unsigned char *p = SDATA (string) + best_below_byte;

      while (best_below < char_index)
	{
	  p += BYTES_BY_CHAR_HEAD (*p);
	  best_below++;
	}
      i_byte = p - SDATA (string);
    }
  else
    {
      unsigned char *p = SDATA (string) + best_above_byte;

      while (best_above > char_index)
	{
	  p--;
	  while (!CHAR_HEAD_P (*p)) p--;
	  best_above--;
	}
      i_byte = p - SDATA (string);
    }

  string_char_byte_cache_bytepos = i_byte;
  string_char_byte_cache_charpos = char_index;
  string_char_byte_cache_string = string;

  return i_byte;
}

/* Return the character index corresponding to BYTE_INDEX in STRING.  */

ptrdiff_t
string_byte_to_char (Lisp_Object string, ptrdiff_t byte_index)
{
  ptrdiff_t i, i_byte;
  ptrdiff_t best_below, best_below_byte;
  ptrdiff_t best_above, best_above_byte;

  best_below = best_below_byte = 0;
  best_above = SCHARS (string);
  best_above_byte = SBYTES (string);
  if (best_above == best_above_byte)
    return byte_index;

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
      unsigned char *p = SDATA (string) + best_below_byte;
      unsigned char *pend = SDATA (string) + byte_index;

      while (p < pend)
	{
	  p += BYTES_BY_CHAR_HEAD (*p);
	  best_below++;
	}
      i = best_below;
      i_byte = p - SDATA (string);
    }
  else
    {
      unsigned char *p = SDATA (string) + best_above_byte;
      unsigned char *pbeg = SDATA (string) + byte_index;

      while (p > pbeg)
	{
	  p--;
	  while (!CHAR_HEAD_P (*p)) p--;
	  best_above--;
	}
      i = best_above;
      i_byte = p - SDATA (string);
    }

  string_char_byte_cache_bytepos = i_byte;
  string_char_byte_cache_charpos = i;
  string_char_byte_cache_string = string;

  return i;
}

/* Convert STRING to a multibyte string.  */

static Lisp_Object
string_make_multibyte (Lisp_Object string)
{
  unsigned char *buf;
  ptrdiff_t nbytes;
  Lisp_Object ret;
  USE_SAFE_ALLOCA;

  if (STRING_MULTIBYTE (string))
    return string;

  nbytes = count_size_as_multibyte (SDATA (string),
				    SCHARS (string));
  /* If all the chars are ASCII, they won't need any more bytes
     once converted.  In that case, we can return STRING itself.  */
  if (nbytes == SBYTES (string))
    return string;

  buf = SAFE_ALLOCA (nbytes);
  copy_text (SDATA (string), buf, SBYTES (string),
	     0, 1);

  ret = make_multibyte_string ((char *) buf, SCHARS (string), nbytes);
  SAFE_FREE ();

  return ret;
}


/* Convert STRING (if unibyte) to a multibyte string without changing
   the number of characters.  Characters 0200 trough 0237 are
   converted to eight-bit characters. */

Lisp_Object
string_to_multibyte (Lisp_Object string)
{
  unsigned char *buf;
  ptrdiff_t nbytes;
  Lisp_Object ret;
  USE_SAFE_ALLOCA;

  if (STRING_MULTIBYTE (string))
    return string;

  nbytes = count_size_as_multibyte (SDATA (string), SBYTES (string));
  /* If all the chars are ASCII, they won't need any more bytes once
     converted.  */
  if (nbytes == SBYTES (string))
    return make_multibyte_string (SSDATA (string), nbytes, nbytes);

  buf = SAFE_ALLOCA (nbytes);
  memcpy (buf, SDATA (string), SBYTES (string));
  str_to_multibyte (buf, nbytes, SBYTES (string));

  ret = make_multibyte_string ((char *) buf, SCHARS (string), nbytes);
  SAFE_FREE ();

  return ret;
}


/* Convert STRING to a single-byte string.  */

Lisp_Object
string_make_unibyte (Lisp_Object string)
{
  ptrdiff_t nchars;
  unsigned char *buf;
  Lisp_Object ret;
  USE_SAFE_ALLOCA;

  if (! STRING_MULTIBYTE (string))
    return string;

  nchars = SCHARS (string);

  buf = SAFE_ALLOCA (nchars);
  copy_text (SDATA (string), buf, SBYTES (string),
	     1, 0);

  ret = make_unibyte_string ((char *) buf, nchars);
  SAFE_FREE ();

  return ret;
}

DEFUN ("string-make-multibyte", Fstring_make_multibyte, Sstring_make_multibyte,
       1, 1, 0,
       doc: /* Return the multibyte equivalent of STRING.
If STRING is unibyte and contains non-ASCII characters, the function
`unibyte-char-to-multibyte' is used to convert each unibyte character
to a multibyte character.  In this case, the returned string is a
newly created string with no text properties.  If STRING is multibyte
or entirely ASCII, it is returned unchanged.  In particular, when
STRING is unibyte and entirely ASCII, the returned string is unibyte.
\(When the characters are all ASCII, Emacs primitives will treat the
string the same way whether it is unibyte or multibyte.)  */)
  (Lisp_Object string)
{
  CHECK_STRING (string);

  return string_make_multibyte (string);
}

DEFUN ("string-make-unibyte", Fstring_make_unibyte, Sstring_make_unibyte,
       1, 1, 0,
       doc: /* Return the unibyte equivalent of STRING.
Multibyte character codes are converted to unibyte according to
`nonascii-translation-table' or, if that is nil, `nonascii-insert-offset'.
If the lookup in the translation table fails, this function takes just
the low 8 bits of each character.  */)
  (Lisp_Object string)
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
`eight-bit', it is converted to the corresponding single byte.  */)
  (Lisp_Object string)
{
  CHECK_STRING (string);

  if (STRING_MULTIBYTE (string))
    {
      unsigned char *str = (unsigned char *) xlispstrdup (string);
      ptrdiff_t bytes = str_as_unibyte (str, SBYTES (string));

      string = make_unibyte_string ((char *) str, bytes);
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
part of a correct utf-8 sequence), it is converted to the corresponding
multibyte character of charset `eight-bit'.
See also `string-to-multibyte'.

Beware, this often doesn't really do what you think it does.
It is similar to (decode-coding-string STRING \\='utf-8-emacs).
If you're not sure, whether to use `string-as-multibyte' or
`string-to-multibyte', use `string-to-multibyte'.  */)
  (Lisp_Object string)
{
  CHECK_STRING (string);

  if (! STRING_MULTIBYTE (string))
    {
      Lisp_Object new_string;
      ptrdiff_t nchars, nbytes;

      parse_str_as_multibyte (SDATA (string),
			      SBYTES (string),
			      &nchars, &nbytes);
      new_string = make_uninit_multibyte_string (nchars, nbytes);
      memcpy (SDATA (new_string), SDATA (string), SBYTES (string));
      if (nbytes != SBYTES (string))
	str_as_multibyte (SDATA (new_string), nbytes,
			  SBYTES (string), NULL);
      string = new_string;
      set_string_intervals (string, NULL);
    }
  return string;
}

DEFUN ("string-to-multibyte", Fstring_to_multibyte, Sstring_to_multibyte,
       1, 1, 0,
       doc: /* Return a multibyte string with the same individual chars as STRING.
If STRING is multibyte, the result is STRING itself.
Otherwise it is a newly created string, with no text properties.

If STRING is unibyte and contains an 8-bit byte, it is converted to
the corresponding multibyte character of charset `eight-bit'.

This differs from `string-as-multibyte' by converting each byte of a correct
utf-8 sequence to an eight-bit character, not just bytes that don't form a
correct sequence.  */)
  (Lisp_Object string)
{
  CHECK_STRING (string);

  return string_to_multibyte (string);
}

DEFUN ("string-to-unibyte", Fstring_to_unibyte, Sstring_to_unibyte,
       1, 1, 0,
       doc: /* Return a unibyte string with the same individual chars as STRING.
If STRING is unibyte, the result is STRING itself.
Otherwise it is a newly created string, with no text properties,
where each `eight-bit' character is converted to the corresponding byte.
If STRING contains a non-ASCII, non-`eight-bit' character,
an error is signaled.  */)
  (Lisp_Object string)
{
  CHECK_STRING (string);

  if (STRING_MULTIBYTE (string))
    {
      ptrdiff_t chars = SCHARS (string);
      unsigned char *str = xmalloc (chars);
      ptrdiff_t converted = str_to_unibyte (SDATA (string), str, chars);

      if (converted < chars)
	error ("Can't convert the %"pD"dth character to unibyte", converted);
      string = make_unibyte_string ((char *) str, chars);
      xfree (str);
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
  (Lisp_Object alist)
{
  if (NILP (alist))
    return alist;
  alist = concat (1, &alist, Lisp_Cons, false);
  for (Lisp_Object tem = alist; !NILP (tem); tem = XCDR (tem))
    {
      Lisp_Object car = XCAR (tem);
      if (CONSP (car))
	XSETCAR (tem, Fcons (XCAR (car), XCDR (car)));
    }
  return alist;
}

/* Check that ARRAY can have a valid subarray [FROM..TO),
   given that its size is SIZE.
   If FROM is nil, use 0; if TO is nil, use SIZE.
   Count negative values backwards from the end.
   Set *IFROM and *ITO to the two indexes used.  */

void
validate_subarray (Lisp_Object array, Lisp_Object from, Lisp_Object to,
		   ptrdiff_t size, ptrdiff_t *ifrom, ptrdiff_t *ito)
{
  EMACS_INT f, t;

  if (INTEGERP (from))
    {
      f = XINT (from);
      if (f < 0)
	f += size;
    }
  else if (NILP (from))
    f = 0;
  else
    wrong_type_argument (Qintegerp, from);

  if (INTEGERP (to))
    {
      t = XINT (to);
      if (t < 0)
	t += size;
    }
  else if (NILP (to))
    t = size;
  else
    wrong_type_argument (Qintegerp, to);

  if (! (0 <= f && f <= t && t <= size))
    args_out_of_range_3 (array, from, to);

  *ifrom = f;
  *ito = t;
}

DEFUN ("substring", Fsubstring, Ssubstring, 1, 3, 0,
       doc: /* Return a new string whose contents are a substring of STRING.
The returned string consists of the characters between index FROM
\(inclusive) and index TO (exclusive) of STRING.  FROM and TO are
zero-indexed: 0 means the first character of STRING.  Negative values
are counted from the end of STRING.  If TO is nil, the substring runs
to the end of STRING.

The STRING argument may also be a vector.  In that case, the return
value is a new vector that contains the elements between index FROM
\(inclusive) and index TO (exclusive) of that vector argument.

With one argument, just copy STRING (with properties, if any).  */)
  (Lisp_Object string, Lisp_Object from, Lisp_Object to)
{
  Lisp_Object res;
  ptrdiff_t size, ifrom, ito;

  size = CHECK_VECTOR_OR_STRING (string);
  validate_subarray (string, from, to, size, &ifrom, &ito);

  if (STRINGP (string))
    {
      ptrdiff_t from_byte
	= !ifrom ? 0 : string_char_to_byte (string, ifrom);
      ptrdiff_t to_byte
	= ito == size ? SBYTES (string) : string_char_to_byte (string, ito);
      res = make_specified_string (SSDATA (string) + from_byte,
				   ito - ifrom, to_byte - from_byte,
				   STRING_MULTIBYTE (string));
      copy_text_properties (make_number (ifrom), make_number (ito),
			    string, make_number (0), res, Qnil);
    }
  else
    res = Fvector (ito - ifrom, aref_addr (string, ifrom));

  return res;
}


DEFUN ("substring-no-properties", Fsubstring_no_properties, Ssubstring_no_properties, 1, 3, 0,
       doc: /* Return a substring of STRING, without text properties.
It starts at index FROM and ends before TO.
TO may be nil or omitted; then the substring runs to the end of STRING.
If FROM is nil or omitted, the substring starts at the beginning of STRING.
If FROM or TO is negative, it counts from the end.

With one argument, just copy STRING without its properties.  */)
  (Lisp_Object string, register Lisp_Object from, Lisp_Object to)
{
  ptrdiff_t from_char, to_char, from_byte, to_byte, size;

  CHECK_STRING (string);

  size = SCHARS (string);
  validate_subarray (string, from, to, size, &from_char, &to_char);

  from_byte = !from_char ? 0 : string_char_to_byte (string, from_char);
  to_byte =
    to_char == size ? SBYTES (string) : string_char_to_byte (string, to_char);
  return make_specified_string (SSDATA (string) + from_byte,
				to_char - from_char, to_byte - from_byte,
				STRING_MULTIBYTE (string));
}

/* Extract a substring of STRING, giving start and end positions
   both in characters and in bytes.  */

Lisp_Object
substring_both (Lisp_Object string, ptrdiff_t from, ptrdiff_t from_byte,
		ptrdiff_t to, ptrdiff_t to_byte)
{
  Lisp_Object res;
  ptrdiff_t size = CHECK_VECTOR_OR_STRING (string);

  if (!(0 <= from && from <= to && to <= size))
    args_out_of_range_3 (string, make_number (from), make_number (to));

  if (STRINGP (string))
    {
      res = make_specified_string (SSDATA (string) + from_byte,
				   to - from, to_byte - from_byte,
				   STRING_MULTIBYTE (string));
      copy_text_properties (make_number (from), make_number (to),
			    string, make_number (0), res, Qnil);
    }
  else
    res = Fvector (to - from, aref_addr (string, from));

  return res;
}

DEFUN ("nthcdr", Fnthcdr, Snthcdr, 2, 2, 0,
       doc: /* Take cdr N times on LIST, return the result.  */)
  (Lisp_Object n, Lisp_Object list)
{
  CHECK_NUMBER (n);
  Lisp_Object tail = list;
  for (EMACS_INT num = XINT (n); 0 < num; num--)
    {
      if (! CONSP (tail))
	{
	  CHECK_LIST_END (tail, list);
	  return Qnil;
	}
      tail = XCDR (tail);
      rarely_quit (num);
    }
  return tail;
}

DEFUN ("nth", Fnth, Snth, 2, 2, 0,
       doc: /* Return the Nth element of LIST.
N counts from zero.  If LIST is not that long, nil is returned.  */)
  (Lisp_Object n, Lisp_Object list)
{
  return Fcar (Fnthcdr (n, list));
}

DEFUN ("elt", Felt, Selt, 2, 2, 0,
       doc: /* Return element of SEQUENCE at index N.  */)
  (register Lisp_Object sequence, Lisp_Object n)
{
  CHECK_NUMBER (n);
  if (CONSP (sequence) || NILP (sequence))
    return Fcar (Fnthcdr (n, sequence));

  /* Faref signals a "not array" error, so check here.  */
  CHECK_ARRAY (sequence, Qsequencep);
  return Faref (sequence, n);
}

DEFUN ("member", Fmember, Smember, 2, 2, 0,
       doc: /* Return non-nil if ELT is an element of LIST.  Comparison done with `equal'.
The value is actually the tail of LIST whose car is ELT.  */)
  (Lisp_Object elt, Lisp_Object list)
{
  Lisp_Object tail = list;
  FOR_EACH_TAIL (tail)
    if (! NILP (Fequal (elt, XCAR (tail))))
      return tail;
  CHECK_LIST_END (tail, list);
  return Qnil;
}

DEFUN ("memq", Fmemq, Smemq, 2, 2, 0,
       doc: /* Return non-nil if ELT is an element of LIST.  Comparison done with `eq'.
The value is actually the tail of LIST whose car is ELT.  */)
  (Lisp_Object elt, Lisp_Object list)
{
  Lisp_Object tail = list;
  FOR_EACH_TAIL (tail)
    if (EQ (XCAR (tail), elt))
      return tail;
  CHECK_LIST_END (tail, list);
  return Qnil;
}

DEFUN ("memql", Fmemql, Smemql, 2, 2, 0,
       doc: /* Return non-nil if ELT is an element of LIST.  Comparison done with `eql'.
The value is actually the tail of LIST whose car is ELT.  */)
  (Lisp_Object elt, Lisp_Object list)
{
  if (!FLOATP (elt))
    return Fmemq (elt, list);

  Lisp_Object tail = list;
  FOR_EACH_TAIL (tail)
    {
      Lisp_Object tem = XCAR (tail);
      if (FLOATP (tem) && equal_no_quit (elt, tem))
	return tail;
    }
  CHECK_LIST_END (tail, list);
  return Qnil;
}

DEFUN ("assq", Fassq, Sassq, 2, 2, 0,
       doc: /* Return non-nil if KEY is `eq' to the car of an element of LIST.
The value is actually the first element of LIST whose car is KEY.
Elements of LIST that are not conses are ignored.  */)
  (Lisp_Object key, Lisp_Object list)
{
  Lisp_Object tail = list;
  FOR_EACH_TAIL (tail)
    if (CONSP (XCAR (tail)) && EQ (XCAR (XCAR (tail)), key))
      return XCAR (tail);
  CHECK_LIST_END (tail, list);
  return Qnil;
}

/* Like Fassq but never report an error and do not allow quits.
   Use only on objects known to be non-circular lists.  */

Lisp_Object
assq_no_quit (Lisp_Object key, Lisp_Object list)
{
  for (; ! NILP (list); list = XCDR (list))
    if (CONSP (XCAR (list)) && EQ (XCAR (XCAR (list)), key))
      return XCAR (list);
  return Qnil;
}

DEFUN ("assoc", Fassoc, Sassoc, 2, 3, 0,
       doc: /* Return non-nil if KEY is equal to the car of an element of LIST.
The value is actually the first element of LIST whose car equals KEY.

Equality is defined by TESTFN if non-nil or by `equal' if nil.  */)
     (Lisp_Object key, Lisp_Object list, Lisp_Object testfn)
{
  Lisp_Object tail = list;
  FOR_EACH_TAIL (tail)
    {
      Lisp_Object car = XCAR (tail);
      if (CONSP (car)
	  && (NILP (testfn)
	      ? (EQ (XCAR (car), key) || !NILP (Fequal
						(XCAR (car), key)))
	      : !NILP (call2 (testfn, XCAR (car), key))))
	return car;
    }
  CHECK_LIST_END (tail, list);
  return Qnil;
}

/* Like Fassoc but never report an error and do not allow quits.
   Use only on keys and lists known to be non-circular, and on keys
   that are not too deep and are not window configurations.  */

Lisp_Object
assoc_no_quit (Lisp_Object key, Lisp_Object list)
{
  for (; ! NILP (list); list = XCDR (list))
    {
      Lisp_Object car = XCAR (list);
      if (CONSP (car)
	  && (EQ (XCAR (car), key) || equal_no_quit (XCAR (car), key)))
	return car;
    }
  return Qnil;
}

DEFUN ("rassq", Frassq, Srassq, 2, 2, 0,
       doc: /* Return non-nil if KEY is `eq' to the cdr of an element of LIST.
The value is actually the first element of LIST whose cdr is KEY.  */)
  (Lisp_Object key, Lisp_Object list)
{
  Lisp_Object tail = list;
  FOR_EACH_TAIL (tail)
    if (CONSP (XCAR (tail)) && EQ (XCDR (XCAR (tail)), key))
      return XCAR (tail);
  CHECK_LIST_END (tail, list);
  return Qnil;
}

DEFUN ("rassoc", Frassoc, Srassoc, 2, 2, 0,
       doc: /* Return non-nil if KEY is `equal' to the cdr of an element of LIST.
The value is actually the first element of LIST whose cdr equals KEY.  */)
  (Lisp_Object key, Lisp_Object list)
{
  Lisp_Object tail = list;
  FOR_EACH_TAIL (tail)
    {
      Lisp_Object car = XCAR (tail);
      if (CONSP (car)
	  && (EQ (XCDR (car), key) || !NILP (Fequal (XCDR (car), key))))
	return car;
    }
  CHECK_LIST_END (tail, list);
  return Qnil;
}

DEFUN ("delq", Fdelq, Sdelq, 2, 2, 0,
       doc: /* Delete members of LIST which are `eq' to ELT, and return the result.
More precisely, this function skips any members `eq' to ELT at the
front of LIST, then removes members `eq' to ELT from the remaining
sublist by modifying its list structure, then returns the resulting
list.

Write `(setq foo (delq element foo))' to be sure of correctly changing
the value of a list `foo'.  See also `remq', which does not modify the
argument.  */)
  (Lisp_Object elt, Lisp_Object list)
{
  Lisp_Object prev = Qnil, tail = list;

  FOR_EACH_TAIL (tail)
    {
      Lisp_Object tem = XCAR (tail);
      if (EQ (elt, tem))
	{
	  if (NILP (prev))
	    list = XCDR (tail);
	  else
	    Fsetcdr (prev, XCDR (tail));
	}
      else
	prev = tail;
    }
  CHECK_LIST_END (tail, list);
  return list;
}

DEFUN ("delete", Fdelete, Sdelete, 2, 2, 0,
       doc: /* Delete members of SEQ which are `equal' to ELT, and return the result.
SEQ must be a sequence (i.e. a list, a vector, or a string).
The return value is a sequence of the same type.

If SEQ is a list, this behaves like `delq', except that it compares
with `equal' instead of `eq'.  In particular, it may remove elements
by altering the list structure.

If SEQ is not a list, deletion is never performed destructively;
instead this function creates and returns a new vector or string.

Write `(setq foo (delete element foo))' to be sure of correctly
changing the value of a sequence `foo'.  */)
  (Lisp_Object elt, Lisp_Object seq)
{
  if (VECTORP (seq))
    {
      ptrdiff_t i, n;

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
      ptrdiff_t i, ibyte, nchars, nbytes, cbytes;
      int c;

      for (i = nchars = nbytes = ibyte = 0;
	   i < SCHARS (seq);
	   ++i, ibyte += cbytes)
	{
	  if (STRING_MULTIBYTE (seq))
	    {
	      c = STRING_CHAR (SDATA (seq) + ibyte);
	      cbytes = CHAR_BYTES (c);
	    }
	  else
	    {
	      c = SREF (seq, i);
	      cbytes = 1;
	    }

	  if (!INTEGERP (elt) || c != XINT (elt))
	    {
	      ++nchars;
	      nbytes += cbytes;
	    }
	}

      if (nchars != SCHARS (seq))
	{
	  Lisp_Object tem;

	  tem = make_uninit_multibyte_string (nchars, nbytes);
	  if (!STRING_MULTIBYTE (seq))
	    STRING_SET_UNIBYTE (tem);

	  for (i = nchars = nbytes = ibyte = 0;
	       i < SCHARS (seq);
	       ++i, ibyte += cbytes)
	    {
	      if (STRING_MULTIBYTE (seq))
		{
		  c = STRING_CHAR (SDATA (seq) + ibyte);
		  cbytes = CHAR_BYTES (c);
		}
	      else
		{
		  c = SREF (seq, i);
		  cbytes = 1;
		}

	      if (!INTEGERP (elt) || c != XINT (elt))
		{
		  unsigned char *from = SDATA (seq) + ibyte;
		  unsigned char *to   = SDATA (tem) + nbytes;
		  ptrdiff_t n;

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
      Lisp_Object prev = Qnil, tail = seq;

      FOR_EACH_TAIL (tail)
	{
	  if (!NILP (Fequal (elt, XCAR (tail))))
	    {
	      if (NILP (prev))
		seq = XCDR (tail);
	      else
		Fsetcdr (prev, XCDR (tail));
	    }
	  else
	    prev = tail;
	}
      CHECK_LIST_END (tail, seq);
    }

  return seq;
}

DEFUN ("nreverse", Fnreverse, Snreverse, 1, 1, 0,
       doc: /* Reverse order of items in a list, vector or string SEQ.
If SEQ is a list, it should be nil-terminated.
This function may destructively modify SEQ to produce the value.  */)
  (Lisp_Object seq)
{
  if (NILP (seq))
    return seq;
  else if (STRINGP (seq))
    return Freverse (seq);
  else if (CONSP (seq))
    {
      Lisp_Object prev, tail, next;

      for (prev = Qnil, tail = seq; CONSP (tail); tail = next)
	{
	  next = XCDR (tail);
	  /* If SEQ contains a cycle, attempting to reverse it
	     in-place will inevitably come back to SEQ.  */
	  if (EQ (next, seq))
	    circular_list (seq);
	  Fsetcdr (tail, prev);
	  prev = tail;
	}
      CHECK_LIST_END (tail, seq);
      seq = prev;
    }
  else if (VECTORP (seq))
    {
      ptrdiff_t i, size = ASIZE (seq);

      for (i = 0; i < size / 2; i++)
	{
	  Lisp_Object tem = AREF (seq, i);
	  ASET (seq, i, AREF (seq, size - i - 1));
	  ASET (seq, size - i - 1, tem);
	}
    }
  else if (BOOL_VECTOR_P (seq))
    {
      ptrdiff_t i, size = bool_vector_size (seq);

      for (i = 0; i < size / 2; i++)
	{
	  bool tem = bool_vector_bitref (seq, i);
	  bool_vector_set (seq, i, bool_vector_bitref (seq, size - i - 1));
	  bool_vector_set (seq, size - i - 1, tem);
	}
    }
  else
    wrong_type_argument (Qarrayp, seq);
  return seq;
}

DEFUN ("reverse", Freverse, Sreverse, 1, 1, 0,
       doc: /* Return the reversed copy of list, vector, or string SEQ.
See also the function `nreverse', which is used more often.  */)
  (Lisp_Object seq)
{
  Lisp_Object new;

  if (NILP (seq))
    return Qnil;
  else if (CONSP (seq))
    {
      new = Qnil;
      FOR_EACH_TAIL (seq)
	new = Fcons (XCAR (seq), new);
      CHECK_LIST_END (seq, seq);
    }
  else if (VECTORP (seq))
    {
      ptrdiff_t i, size = ASIZE (seq);

      new = make_uninit_vector (size);
      for (i = 0; i < size; i++)
	ASET (new, i, AREF (seq, size - i - 1));
    }
  else if (BOOL_VECTOR_P (seq))
    {
      ptrdiff_t i;
      EMACS_INT nbits = bool_vector_size (seq);

      new = make_uninit_bool_vector (nbits);
      for (i = 0; i < nbits; i++)
	bool_vector_set (new, i, bool_vector_bitref (seq, nbits - i - 1));
    }
  else if (STRINGP (seq))
    {
      ptrdiff_t size = SCHARS (seq), bytes = SBYTES (seq);

      if (size == bytes)
	{
	  ptrdiff_t i;

	  new = make_uninit_string (size);
	  for (i = 0; i < size; i++)
	    SSET (new, i, SREF (seq, size - i - 1));
	}
      else
	{
	  unsigned char *p, *q;

	  new = make_uninit_multibyte_string (size, bytes);
	  p = SDATA (seq), q = SDATA (new) + bytes;
	  while (q > SDATA (new))
	    {
	      int ch, len;

	      ch = STRING_CHAR_AND_LENGTH (p, len);
	      p += len, q -= len;
	      CHAR_STRING (ch, q);
	    }
	}
    }
  else
    wrong_type_argument (Qsequencep, seq);
  return new;
}

/* Sort LIST using PREDICATE, preserving original order of elements
   considered as equal.  */

static Lisp_Object
sort_list (Lisp_Object list, Lisp_Object predicate)
{
  Lisp_Object front, back;
  Lisp_Object len, tem;
  EMACS_INT length;

  front = list;
  len = Flength (list);
  length = XINT (len);
  if (length < 2)
    return list;

  XSETINT (len, (length / 2) - 1);
  tem = Fnthcdr (len, list);
  back = Fcdr (tem);
  Fsetcdr (tem, Qnil);

  front = Fsort (front, predicate);
  back = Fsort (back, predicate);
  return merge (front, back, predicate);
}

/* Using PRED to compare, return whether A and B are in order.
   Compare stably when A appeared before B in the input.  */
static bool
inorder (Lisp_Object pred, Lisp_Object a, Lisp_Object b)
{
  return NILP (call2 (pred, b, a));
}

/* Using PRED to compare, merge from ALEN-length A and BLEN-length B
   into DEST.  Argument arrays must be nonempty and must not overlap,
   except that B might be the last part of DEST.  */
static void
merge_vectors (Lisp_Object pred,
	       ptrdiff_t alen, Lisp_Object const a[restrict VLA_ELEMS (alen)],
	       ptrdiff_t blen, Lisp_Object const b[VLA_ELEMS (blen)],
	       Lisp_Object dest[VLA_ELEMS (alen + blen)])
{
  eassume (0 < alen && 0 < blen);
  Lisp_Object const *alim = a + alen;
  Lisp_Object const *blim = b + blen;

  while (true)
    {
      if (inorder (pred, a[0], b[0]))
	{
	  *dest++ = *a++;
	  if (a == alim)
	    {
	      if (dest != b)
		memcpy (dest, b, (blim - b) * sizeof *dest);
	      return;
	    }
	}
      else
	{
	  *dest++ = *b++;
	  if (b == blim)
	    {
	      memcpy (dest, a, (alim - a) * sizeof *dest);
	      return;
	    }
	}
    }
}

/* Using PRED to compare, sort LEN-length VEC in place, using TMP for
   temporary storage.  LEN must be at least 2.  */
static void
sort_vector_inplace (Lisp_Object pred, ptrdiff_t len,
		     Lisp_Object vec[restrict VLA_ELEMS (len)],
		     Lisp_Object tmp[restrict VLA_ELEMS (len >> 1)])
{
  eassume (2 <= len);
  ptrdiff_t halflen = len >> 1;
  sort_vector_copy (pred, halflen, vec, tmp);
  if (1 < len - halflen)
    sort_vector_inplace (pred, len - halflen, vec + halflen, vec);
  merge_vectors (pred, halflen, tmp, len - halflen, vec + halflen, vec);
}

/* Using PRED to compare, sort from LEN-length SRC into DST.
   Len must be positive.  */
static void
sort_vector_copy (Lisp_Object pred, ptrdiff_t len,
		  Lisp_Object src[restrict VLA_ELEMS (len)],
		  Lisp_Object dest[restrict VLA_ELEMS (len)])
{
  eassume (0 < len);
  ptrdiff_t halflen = len >> 1;
  if (halflen < 1)
    dest[0] = src[0];
  else
    {
      if (1 < halflen)
	sort_vector_inplace (pred, halflen, src, dest);
      if (1 < len - halflen)
	sort_vector_inplace (pred, len - halflen, src + halflen, dest);
      merge_vectors (pred, halflen, src, len - halflen, src + halflen, dest);
    }
}

/* Sort VECTOR in place using PREDICATE, preserving original order of
   elements considered as equal.  */

static void
sort_vector (Lisp_Object vector, Lisp_Object predicate)
{
  ptrdiff_t len = ASIZE (vector);
  if (len < 2)
    return;
  ptrdiff_t halflen = len >> 1;
  Lisp_Object *tmp;
  USE_SAFE_ALLOCA;
  SAFE_ALLOCA_LISP (tmp, halflen);
  for (ptrdiff_t i = 0; i < halflen; i++)
    tmp[i] = make_number (0);
  sort_vector_inplace (predicate, len, XVECTOR (vector)->contents, tmp);
  SAFE_FREE ();
}

DEFUN ("sort", Fsort, Ssort, 2, 2, 0,
       doc: /* Sort SEQ, stably, comparing elements using PREDICATE.
Returns the sorted sequence.  SEQ should be a list or vector.  SEQ is
modified by side effects.  PREDICATE is called with two elements of
SEQ, and should return non-nil if the first element should sort before
the second.  */)
  (Lisp_Object seq, Lisp_Object predicate)
{
  if (CONSP (seq))
    seq = sort_list (seq, predicate);
  else if (VECTORP (seq))
    sort_vector (seq, predicate);
  else if (!NILP (seq))
    wrong_type_argument (Qsequencep, seq);
  return seq;
}

Lisp_Object
merge (Lisp_Object org_l1, Lisp_Object org_l2, Lisp_Object pred)
{
  Lisp_Object l1 = org_l1;
  Lisp_Object l2 = org_l2;
  Lisp_Object tail = Qnil;
  Lisp_Object value = Qnil;

  while (1)
    {
      if (NILP (l1))
	{
	  if (NILP (tail))
	    return l2;
	  Fsetcdr (tail, l2);
	  return value;
	}
      if (NILP (l2))
	{
	  if (NILP (tail))
	    return l1;
	  Fsetcdr (tail, l1);
	  return value;
	}

      Lisp_Object tem;
      if (inorder (pred, Fcar (l1), Fcar (l2)))
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


/* This does not check for quits.  That is safe since it must terminate.  */

DEFUN ("plist-get", Fplist_get, Splist_get, 2, 2, 0,
       doc: /* Extract a value from a property list.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value
corresponding to the given PROP, or nil if PROP is not one of the
properties on the list.  This function never signals an error.  */)
  (Lisp_Object plist, Lisp_Object prop)
{
  Lisp_Object tail = plist;
  FOR_EACH_TAIL_SAFE (tail)
    {
      if (! CONSP (XCDR (tail)))
	break;
      if (EQ (prop, XCAR (tail)))
	return XCAR (XCDR (tail));
      tail = XCDR (tail);
      if (EQ (tail, li.tortoise))
	break;
    }

  return Qnil;
}

DEFUN ("get", Fget, Sget, 2, 2, 0,
       doc: /* Return the value of SYMBOL's PROPNAME property.
This is the last value stored with `(put SYMBOL PROPNAME VALUE)'.  */)
  (Lisp_Object symbol, Lisp_Object propname)
{
  CHECK_SYMBOL (symbol);
  Lisp_Object propval = Fplist_get (CDR (Fassq (symbol, Voverriding_plist_environment)),
                                    propname);
  if (!NILP (propval))
    return propval;
  return Fplist_get (XSYMBOL (symbol)->u.s.plist, propname);
}

DEFUN ("plist-put", Fplist_put, Splist_put, 3, 3, 0,
       doc: /* Change value in PLIST of PROP to VAL.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2 ...).  PROP is a symbol and VAL is any object.
If PROP is already a property on the list, its value is set to VAL,
otherwise the new PROP VAL pair is added.  The new plist is returned;
use `(setq x (plist-put x prop val))' to be sure to use the new value.
The PLIST is modified by side effects.  */)
  (Lisp_Object plist, Lisp_Object prop, Lisp_Object val)
{
  Lisp_Object prev = Qnil, tail = plist;
  FOR_EACH_TAIL (tail)
    {
      if (! CONSP (XCDR (tail)))
	break;

      if (EQ (prop, XCAR (tail)))
	{
	  Fsetcar (XCDR (tail), val);
	  return plist;
	}

      prev = tail;
      tail = XCDR (tail);
      if (EQ (tail, li.tortoise))
	circular_list (plist);
    }
  CHECK_TYPE (NILP (tail), Qplistp, plist);
  Lisp_Object newcell
    = Fcons (prop, Fcons (val, NILP (prev) ? plist : XCDR (XCDR (prev))));
  if (NILP (prev))
    return newcell;
  Fsetcdr (XCDR (prev), newcell);
  return plist;
}

DEFUN ("put", Fput, Sput, 3, 3, 0,
       doc: /* Store SYMBOL's PROPNAME property with value VALUE.
It can be retrieved with `(get SYMBOL PROPNAME)'.  */)
  (Lisp_Object symbol, Lisp_Object propname, Lisp_Object value)
{
  CHECK_SYMBOL (symbol);
  set_symbol_plist
    (symbol, Fplist_put (XSYMBOL (symbol)->u.s.plist, propname, value));
  return value;
}

DEFUN ("lax-plist-get", Flax_plist_get, Slax_plist_get, 2, 2, 0,
       doc: /* Extract a value from a property list, comparing with `equal'.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value
corresponding to the given PROP, or nil if PROP is not
one of the properties on the list.  */)
  (Lisp_Object plist, Lisp_Object prop)
{
  Lisp_Object tail = plist;
  FOR_EACH_TAIL (tail)
    {
      if (! CONSP (XCDR (tail)))
	break;
      if (! NILP (Fequal (prop, XCAR (tail))))
	return XCAR (XCDR (tail));
      tail = XCDR (tail);
      if (EQ (tail, li.tortoise))
	circular_list (plist);
    }

  CHECK_TYPE (NILP (tail), Qplistp, plist);

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
  (Lisp_Object plist, Lisp_Object prop, Lisp_Object val)
{
  Lisp_Object prev = Qnil, tail = plist;
  FOR_EACH_TAIL (tail)
    {
      if (! CONSP (XCDR (tail)))
	break;

      if (! NILP (Fequal (prop, XCAR (tail))))
	{
	  Fsetcar (XCDR (tail), val);
	  return plist;
	}

      prev = tail;
      tail = XCDR (tail);
      if (EQ (tail, li.tortoise))
	circular_list (plist);
    }
  CHECK_TYPE (NILP (tail), Qplistp, plist);
  Lisp_Object newcell = list2 (prop, val);
  if (NILP (prev))
    return newcell;
  Fsetcdr (XCDR (prev), newcell);
  return plist;
}

DEFUN ("eql", Feql, Seql, 2, 2, 0,
       doc: /* Return t if the two args are the same Lisp object.
Floating-point numbers of equal value are `eql', but they may not be `eq'.  */)
  (Lisp_Object obj1, Lisp_Object obj2)
{
  if (FLOATP (obj1))
    return equal_no_quit (obj1, obj2) ? Qt : Qnil;
  else
    return EQ (obj1, obj2) ? Qt : Qnil;
}

DEFUN ("equal", Fequal, Sequal, 2, 2, 0,
       doc: /* Return t if two Lisp objects have similar structure and contents.
They must have the same data type.
Conses are compared by comparing the cars and the cdrs.
Vectors and strings are compared element by element.
Numbers are compared by value, but integers cannot equal floats.
 (Use `=' if you want integers and floats to be able to be equal.)
Symbols must match exactly.  */)
  (Lisp_Object o1, Lisp_Object o2)
{
  return internal_equal (o1, o2, EQUAL_PLAIN, 0, Qnil) ? Qt : Qnil;
}

DEFUN ("equal-including-properties", Fequal_including_properties, Sequal_including_properties, 2, 2, 0,
       doc: /* Return t if two Lisp objects have similar structure and contents.
This is like `equal' except that it compares the text properties
of strings.  (`equal' ignores text properties.)  */)
  (Lisp_Object o1, Lisp_Object o2)
{
  return (internal_equal (o1, o2, EQUAL_INCLUDING_PROPERTIES, 0, Qnil)
	  ? Qt : Qnil);
}

/* Return true if O1 and O2 are equal.  Do not quit or check for cycles.
   Use this only on arguments that are cycle-free and not too large and
   are not window configurations.  */

bool
equal_no_quit (Lisp_Object o1, Lisp_Object o2)
{
  return internal_equal (o1, o2, EQUAL_NO_QUIT, 0, Qnil);
}

/* Return true if O1 and O2 are equal.  EQUAL_KIND specifies what kind
   of equality test to use: if it is EQUAL_NO_QUIT, do not check for
   cycles or large arguments or quits; if EQUAL_PLAIN, do ordinary
   Lisp equality; and if EQUAL_INCLUDING_PROPERTIES, do
   equal-including-properties.

   If DEPTH is the current depth of recursion; signal an error if it
   gets too deep.  HT is a hash table used to detect cycles; if nil,
   it has not been allocated yet.  But ignore the last two arguments
   if EQUAL_KIND == EQUAL_NO_QUIT.  */

static bool
internal_equal (Lisp_Object o1, Lisp_Object o2, enum equal_kind equal_kind,
		int depth, Lisp_Object ht)
{
 tail_recurse:
  if (depth > 10)
    {
      eassert (equal_kind != EQUAL_NO_QUIT);
      if (depth > 200)
	error ("Stack overflow in equal");
      if (NILP (ht))
	ht = CALLN (Fmake_hash_table, QCtest, Qeq);
      switch (XTYPE (o1))
	{
	case Lisp_Cons: case Lisp_Misc: case Lisp_Vectorlike:
	  {
	    struct Lisp_Hash_Table *h = XHASH_TABLE (ht);
	    EMACS_UINT hash;
	    ptrdiff_t i = hash_lookup (h, o1, &hash);
	    if (i >= 0)
	      { /* `o1' was seen already.  */
		Lisp_Object o2s = HASH_VALUE (h, i);
		if (!NILP (Fmemq (o2, o2s)))
		  return true;
		else
		  set_hash_value_slot (h, i, Fcons (o2, o2s));
	      }
	    else
	      hash_put (h, o1, Fcons (o2, Qnil), hash);
	  }
	default: ;
	}
    }

  if (EQ (o1, o2))
    return true;
  if (XTYPE (o1) != XTYPE (o2))
    return false;

  switch (XTYPE (o1))
    {
    case Lisp_Float:
      {
	double d1 = XFLOAT_DATA (o1);
	double d2 = XFLOAT_DATA (o2);
	/* If d is a NaN, then d != d. Two NaNs should be `equal' even
	   though they are not =.  */
	return d1 == d2 || (d1 != d1 && d2 != d2);
      }

    case Lisp_Cons:
      if (equal_kind == EQUAL_NO_QUIT)
	for (; CONSP (o1); o1 = XCDR (o1))
	  {
	    if (! CONSP (o2))
	      return false;
	    if (! equal_no_quit (XCAR (o1), XCAR (o2)))
	      return false;
	    o2 = XCDR (o2);
	    if (EQ (XCDR (o1), o2))
	      return true;
	  }
      else
	FOR_EACH_TAIL (o1)
	  {
	    if (! CONSP (o2))
	      return false;
	    if (! internal_equal (XCAR (o1), XCAR (o2),
				  equal_kind, depth + 1, ht))
	      return false;
	    o2 = XCDR (o2);
	    if (EQ (XCDR (o1), o2))
	      return true;
	  }
      depth++;
      goto tail_recurse;

    case Lisp_Misc:
      if (XMISCTYPE (o1) != XMISCTYPE (o2))
	return false;
      if (OVERLAYP (o1))
	{
	  if (!internal_equal (OVERLAY_START (o1), OVERLAY_START (o2),
			       equal_kind, depth + 1, ht)
	      || !internal_equal (OVERLAY_END (o1), OVERLAY_END (o2),
				  equal_kind, depth + 1, ht))
	    return false;
	  o1 = XOVERLAY (o1)->plist;
	  o2 = XOVERLAY (o2)->plist;
	  depth++;
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
	register int i;
	ptrdiff_t size = ASIZE (o1);
	/* Pseudovectors have the type encoded in the size field, so this test
	   actually checks that the objects have the same type as well as the
	   same size.  */
	if (ASIZE (o2) != size)
	  return false;
	/* Boolvectors are compared much like strings.  */
	if (BOOL_VECTOR_P (o1))
	  {
	    EMACS_INT size = bool_vector_size (o1);
	    if (size != bool_vector_size (o2))
	      return false;
	    if (memcmp (bool_vector_data (o1), bool_vector_data (o2),
			bool_vector_bytes (size)))
	      return false;
	    return true;
	  }
	if (WINDOW_CONFIGURATIONP (o1))
	  {
	    eassert (equal_kind != EQUAL_NO_QUIT);
	    return compare_window_configurations (o1, o2, false);
	  }

	/* Aside from them, only true vectors, char-tables, compiled
	   functions, and fonts (font-spec, font-entity, font-object)
	   are sensible to compare, so eliminate the others now.  */
	if (size & PSEUDOVECTOR_FLAG)
	  {
	    if (((size & PVEC_TYPE_MASK) >> PSEUDOVECTOR_AREA_BITS)
		< PVEC_COMPILED)
	      return false;
	    size &= PSEUDOVECTOR_SIZE_MASK;
	  }
	for (i = 0; i < size; i++)
	  {
	    Lisp_Object v1, v2;
	    v1 = AREF (o1, i);
	    v2 = AREF (o2, i);
	    if (!internal_equal (v1, v2, equal_kind, depth + 1, ht))
	      return false;
	  }
	return true;
      }
      break;

    case Lisp_String:
      if (SCHARS (o1) != SCHARS (o2))
	return false;
      if (SBYTES (o1) != SBYTES (o2))
	return false;
      if (memcmp (SDATA (o1), SDATA (o2), SBYTES (o1)))
	return false;
      if (equal_kind == EQUAL_INCLUDING_PROPERTIES
	  && !compare_string_intervals (o1, o2))
	return false;
      return true;

    default:
      break;
    }

  return false;
}


DEFUN ("fillarray", Ffillarray, Sfillarray, 2, 2, 0,
       doc: /* Store each element of ARRAY with ITEM.
ARRAY is a vector, string, char-table, or bool-vector.  */)
  (Lisp_Object array, Lisp_Object item)
{
  register ptrdiff_t size, idx;

  if (VECTORP (array))
    for (idx = 0, size = ASIZE (array); idx < size; idx++)
      ASET (array, idx, item);
  else if (CHAR_TABLE_P (array))
    {
      int i;

      for (i = 0; i < (1 << CHARTAB_SIZE_BITS_0); i++)
	set_char_table_contents (array, i, item);
      set_char_table_defalt (array, item);
    }
  else if (STRINGP (array))
    {
      register unsigned char *p = SDATA (array);
      int charval;
      CHECK_CHARACTER (item);
      charval = XFASTINT (item);
      size = SCHARS (array);
      if (STRING_MULTIBYTE (array))
	{
	  unsigned char str[MAX_MULTIBYTE_LENGTH];
	  int len = CHAR_STRING (charval, str);
	  ptrdiff_t size_byte = SBYTES (array);
	  ptrdiff_t product;

	  if (INT_MULTIPLY_WRAPV (size, len, &product) || product != size_byte)
	    error ("Attempt to change byte length of a string");
	  for (idx = 0; idx < size_byte; idx++)
	    *p++ = str[idx % len];
	}
      else
	for (idx = 0; idx < size; idx++)
	  p[idx] = charval;
    }
  else if (BOOL_VECTOR_P (array))
    return bool_vector_fill (array, item);
  else
    wrong_type_argument (Qarrayp, array);
  return array;
}

DEFUN ("clear-string", Fclear_string, Sclear_string,
       1, 1, 0,
       doc: /* Clear the contents of STRING.
This makes STRING unibyte and may change its length.  */)
  (Lisp_Object string)
{
  ptrdiff_t len;
  CHECK_STRING (string);
  len = SBYTES (string);
  memset (SDATA (string), 0, len);
  STRING_SET_CHARS (string, len);
  STRING_SET_UNIBYTE (string);
  return Qnil;
}

/* ARGSUSED */
Lisp_Object
nconc2 (Lisp_Object s1, Lisp_Object s2)
{
  return CALLN (Fnconc, s1, s2);
}

DEFUN ("nconc", Fnconc, Snconc, 0, MANY, 0,
       doc: /* Concatenate any number of lists by altering them.
Only the last argument is not altered, and need not be a list.
usage: (nconc &rest LISTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object val = Qnil;

  for (ptrdiff_t argnum = 0; argnum < nargs; argnum++)
    {
      Lisp_Object tem = args[argnum];
      if (NILP (tem)) continue;

      if (NILP (val))
	val = tem;

      if (argnum + 1 == nargs) break;

      CHECK_CONS (tem);

      Lisp_Object tail;
      FOR_EACH_TAIL (tem)
	tail = tem;

      tem = args[argnum + 1];
      Fsetcdr (tail, tem);
      if (NILP (tem))
	args[argnum + 1] = tail;
    }

  return val;
}

/* This is the guts of all mapping functions.
   Apply FN to each element of SEQ, one by one, storing the results
   into elements of VALS, a C vector of Lisp_Objects.  LENI is the
   length of VALS, which should also be the length of SEQ.  Return the
   number of results; although this is normally LENI, it can be less
   if SEQ is made shorter as a side effect of FN.  */

static EMACS_INT
mapcar1 (EMACS_INT leni, Lisp_Object *vals, Lisp_Object fn, Lisp_Object seq)
{
  Lisp_Object tail, dummy;
  EMACS_INT i;

  if (VECTORP (seq) || COMPILEDP (seq))
    {
      for (i = 0; i < leni; i++)
	{
	  dummy = call1 (fn, AREF (seq, i));
	  if (vals)
	    vals[i] = dummy;
	}
    }
  else if (BOOL_VECTOR_P (seq))
    {
      for (i = 0; i < leni; i++)
	{
	  dummy = call1 (fn, bool_vector_ref (seq, i));
	  if (vals)
	    vals[i] = dummy;
	}
    }
  else if (STRINGP (seq))
    {
      ptrdiff_t i_byte;

      for (i = 0, i_byte = 0; i < leni;)
	{
	  int c;
	  ptrdiff_t i_before = i;

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
	  if (! CONSP (tail))
	    return i;
	  dummy = call1 (fn, XCAR (tail));
	  if (vals)
	    vals[i] = dummy;
	  tail = XCDR (tail);
	}
    }

  return leni;
}

DEFUN ("mapconcat", Fmapconcat, Smapconcat, 3, 3, 0,
       doc: /* Apply FUNCTION to each element of SEQUENCE, and concat the results as strings.
In between each pair of results, stick in SEPARATOR.  Thus, " " as
SEPARATOR results in spaces between the values returned by FUNCTION.
SEQUENCE may be a list, a vector, a bool-vector, or a string.  */)
  (Lisp_Object function, Lisp_Object sequence, Lisp_Object separator)
{
  USE_SAFE_ALLOCA;
  EMACS_INT leni = XFASTINT (Flength (sequence));
  if (CHAR_TABLE_P (sequence))
    wrong_type_argument (Qlistp, sequence);
  EMACS_INT args_alloc = 2 * leni - 1;
  if (args_alloc < 0)
    return empty_unibyte_string;
  Lisp_Object *args;
  SAFE_ALLOCA_LISP (args, args_alloc);
  ptrdiff_t nmapped = mapcar1 (leni, args, function, sequence);
  ptrdiff_t nargs = 2 * nmapped - 1;

  for (ptrdiff_t i = nmapped - 1; i > 0; i--)
    args[i + i] = args[i];

  for (ptrdiff_t i = 1; i < nargs; i += 2)
    args[i] = separator;

  Lisp_Object ret = Fconcat (nargs, args);
  SAFE_FREE ();
  return ret;
}

DEFUN ("mapcar", Fmapcar, Smapcar, 2, 2, 0,
       doc: /* Apply FUNCTION to each element of SEQUENCE, and make a list of the results.
The result is a list just as long as SEQUENCE.
SEQUENCE may be a list, a vector, a bool-vector, or a string.  */)
  (Lisp_Object function, Lisp_Object sequence)
{
  USE_SAFE_ALLOCA;
  EMACS_INT leni = XFASTINT (Flength (sequence));
  if (CHAR_TABLE_P (sequence))
    wrong_type_argument (Qlistp, sequence);
  Lisp_Object *args;
  SAFE_ALLOCA_LISP (args, leni);
  ptrdiff_t nmapped = mapcar1 (leni, args, function, sequence);
  Lisp_Object ret = Flist (nmapped, args);
  SAFE_FREE ();
  return ret;
}

DEFUN ("mapc", Fmapc, Smapc, 2, 2, 0,
       doc: /* Apply FUNCTION to each element of SEQUENCE for side effects only.
Unlike `mapcar', don't accumulate the results.  Return SEQUENCE.
SEQUENCE may be a list, a vector, a bool-vector, or a string.  */)
  (Lisp_Object function, Lisp_Object sequence)
{
  register EMACS_INT leni;

  leni = XFASTINT (Flength (sequence));
  if (CHAR_TABLE_P (sequence))
    wrong_type_argument (Qlistp, sequence);
  mapcar1 (leni, 0, function, sequence);

  return sequence;
}

DEFUN ("mapcan", Fmapcan, Smapcan, 2, 2, 0,
       doc: /* Apply FUNCTION to each element of SEQUENCE, and concatenate
the results by altering them (using `nconc').
SEQUENCE may be a list, a vector, a bool-vector, or a string. */)
     (Lisp_Object function, Lisp_Object sequence)
{
  USE_SAFE_ALLOCA;
  EMACS_INT leni = XFASTINT (Flength (sequence));
  if (CHAR_TABLE_P (sequence))
    wrong_type_argument (Qlistp, sequence);
  Lisp_Object *args;
  SAFE_ALLOCA_LISP (args, leni);
  ptrdiff_t nmapped = mapcar1 (leni, args, function, sequence);
  Lisp_Object ret = Fnconc (nmapped, args);
  SAFE_FREE ();
  return ret;
}

/* This is how C code calls `yes-or-no-p' and allows the user
   to redefine it.  */

Lisp_Object
do_yes_or_no_p (Lisp_Object prompt)
{
  return call1 (intern ("yes-or-no-p"), prompt);
}

DEFUN ("yes-or-no-p", Fyes_or_no_p, Syes_or_no_p, 1, 1, 0,
       doc: /* Ask user a yes-or-no question.
Return t if answer is yes, and nil if the answer is no.
PROMPT is the string to display to ask the question.  It should end in
a space; `yes-or-no-p' adds \"(yes or no) \" to it.

The user must confirm the answer with RET, and can edit it until it
has been confirmed.

If dialog boxes are supported, a dialog box will be used
if `last-nonmenu-event' is nil, and `use-dialog-box' is non-nil.  */)
  (Lisp_Object prompt)
{
  Lisp_Object ans;

  CHECK_STRING (prompt);

  if ((NILP (last_nonmenu_event) || CONSP (last_nonmenu_event))
      && use_dialog_box && ! NILP (last_input_event))
    {
      Lisp_Object pane, menu, obj;
      redisplay_preserve_echo_area (4);
      pane = list2 (Fcons (build_string ("Yes"), Qt),
		    Fcons (build_string ("No"), Qnil));
      menu = Fcons (prompt, pane);
      obj = Fx_popup_dialog (Qt, menu, Qnil);
      return obj;
    }

  AUTO_STRING (yes_or_no, "(yes or no) ");
  prompt = CALLN (Fconcat, prompt, yes_or_no);

  while (1)
    {
      ans = Fdowncase (Fread_from_minibuffer (prompt, Qnil, Qnil, Qnil,
					      Qyes_or_no_p_history, Qnil,
					      Qnil));
      if (SCHARS (ans) == 3 && !strcmp (SSDATA (ans), "yes"))
	return Qt;
      if (SCHARS (ans) == 2 && !strcmp (SSDATA (ans), "no"))
	return Qnil;

      Fding (Qnil);
      Fdiscard_input ();
      message1 ("Please answer yes or no.");
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
shortened list, containing only those averages which are available.

An error is thrown if the load average can't be obtained.  In some
cases making it work would require Emacs being installed setuid or
setgid so that it can read kernel information, and that usually isn't
advisable.  */)
  (Lisp_Object use_floats)
{
  double load_ave[3];
  int loads = getloadavg (load_ave, 3);
  Lisp_Object ret = Qnil;

  if (loads < 0)
    error ("load-average not implemented for this operating system");

  while (loads-- > 0)
    {
      Lisp_Object load = (NILP (use_floats)
			  ? make_number (100.0 * load_ave[loads])
			  : make_float (load_ave[loads]));
      ret = Fcons (load, ret);
    }

  return ret;
}

DEFUN ("featurep", Ffeaturep, Sfeaturep, 1, 2, 0,
       doc: /* Return t if FEATURE is present in this Emacs.

Use this to conditionalize execution of lisp code based on the
presence or absence of Emacs or environment extensions.
Use `provide' to declare that a feature is available.  This function
looks at the value of the variable `features'.  The optional argument
SUBFEATURE can be used to check a specific subfeature of FEATURE.  */)
  (Lisp_Object feature, Lisp_Object subfeature)
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
  (Lisp_Object feature, Lisp_Object subfeatures)
{
  register Lisp_Object tem;
  CHECK_SYMBOL (feature);
  CHECK_LIST (subfeatures);
  if (!NILP (Vautoload_queue))
    Vautoload_queue = Fcons (Fcons (make_number (0), Vfeatures),
			     Vautoload_queue);
  tem = Fmemq (feature, Vfeatures);
  if (NILP (tem))
    Vfeatures = Fcons (feature, Vfeatures);
  if (!NILP (subfeatures))
    Fput (feature, Qsubfeatures, subfeatures);
  LOADHIST_ATTACH (Fcons (Qprovide, feature));

  /* Run any load-hooks for this file.  */
  tem = Fassq (feature, Vafter_load_alist);
  if (CONSP (tem))
    Fmapc (Qfuncall, XCDR (tem));

  return feature;
}

/* `require' and its subroutines.  */

/* List of features currently being require'd, innermost first.  */

static Lisp_Object require_nesting_list;

static void
require_unwind (Lisp_Object old_value)
{
  require_nesting_list = old_value;
}

DEFUN ("require", Frequire, Srequire, 1, 3, 0,
       doc: /* If feature FEATURE is not loaded, load it from FILENAME.
If FEATURE is not a member of the list `features', then the feature is
not loaded; so load the file FILENAME.

If FILENAME is omitted, the printname of FEATURE is used as the file
name, and `load' will try to load this name appended with the suffix
`.elc', `.el', or the system-dependent suffix for dynamic module
files, in that order.  The name without appended suffix will not be
used.  See `get-load-suffixes' for the complete list of suffixes.

The directories in `load-path' are searched when trying to find the
file name.

If the optional third argument NOERROR is non-nil, then return nil if
the file is not found instead of signaling an error.  Normally the
return value is FEATURE.

The normal messages at start and end of loading FILENAME are
suppressed.  */)
  (Lisp_Object feature, Lisp_Object filename, Lisp_Object noerror)
{
  Lisp_Object tem;
  bool from_file = load_in_progress;

  CHECK_SYMBOL (feature);

  /* Record the presence of `require' in this file
     even if the feature specified is already loaded.
     But not more than once in any file,
     and not when we aren't loading or reading from a file.  */
  if (!from_file)
    for (tem = Vcurrent_load_list; CONSP (tem); tem = XCDR (tem))
      if (NILP (XCDR (tem)) && STRINGP (XCAR (tem)))
	from_file = 1;

  if (from_file)
    {
      tem = Fcons (Qrequire, feature);
      if (NILP (Fmember (tem, Vcurrent_load_list)))
	LOADHIST_ATTACH (tem);
    }
  tem = Fmemq (feature, Vfeatures);

  if (NILP (tem))
    {
      ptrdiff_t count = SPECPDL_INDEX ();
      int nesting = 0;

      /* This is to make sure that loadup.el gives a clear picture
	 of what files are preloaded and when.  */
      if (! NILP (Vpurify_flag))
	error ("(require %s) while preparing to dump",
	       SDATA (SYMBOL_NAME (feature)));

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
      if (nesting > 3)
	error ("Recursive `require' for feature `%s'",
	       SDATA (SYMBOL_NAME (feature)));

      /* Update the list for any nested `require's that occur.  */
      record_unwind_protect (require_unwind, require_nesting_list);
      require_nesting_list = Fcons (feature, require_nesting_list);

      /* Value saved here is to be restored into Vautoload_queue */
      record_unwind_protect (un_autoload, Vautoload_queue);
      Vautoload_queue = Qt;

      /* Load the file.  */
      tem = Fload (NILP (filename) ? Fsymbol_name (feature) : filename,
		   noerror, Qt, Qnil, (NILP (filename) ? Qt : Qnil));

      /* If load failed entirely, return nil.  */
      if (NILP (tem))
	return unbind_to (count, Qnil);

      tem = Fmemq (feature, Vfeatures);
      if (NILP (tem))
        {
          unsigned char *tem2 = SDATA (SYMBOL_NAME (feature));
          Lisp_Object tem3 = Fcar (Fcar (Vload_history));

          if (NILP (tem3))
            error ("Required feature `%s' was not provided", tem2);
          else
            /* Cf autoload-do-load.  */
            error ("Loading file %s failed to provide feature `%s'",
                   SDATA (tem3), tem2);
        }

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
\(PROP1 VALUE1 PROP2 VALUE2 ...).  PROP is a symbol.
Unlike `plist-get', this allows you to distinguish between a missing
property and a property with the value nil.
The value is actually the tail of PLIST whose car is PROP.  */)
  (Lisp_Object plist, Lisp_Object prop)
{
  Lisp_Object tail = plist;
  FOR_EACH_TAIL (tail)
    {
      if (EQ (XCAR (tail), prop))
	return tail;
      tail = XCDR (tail);
      if (! CONSP (tail))
	break;
      if (EQ (tail, li.tortoise))
	circular_list (tail);
    }
  CHECK_TYPE (NILP (tail), Qplistp, plist);
  return Qnil;
}

DEFUN ("widget-put", Fwidget_put, Swidget_put, 3, 3, 0,
       doc: /* In WIDGET, set PROPERTY to VALUE.
The value can later be retrieved with `widget-get'.  */)
  (Lisp_Object widget, Lisp_Object property, Lisp_Object value)
{
  CHECK_CONS (widget);
  XSETCDR (widget, Fplist_put (XCDR (widget), property, value));
  return value;
}

DEFUN ("widget-get", Fwidget_get, Swidget_get, 2, 2, 0,
       doc: /* In WIDGET, get the value of PROPERTY.
The value could either be specified when the widget was created, or
later with `widget-put'.  */)
  (Lisp_Object widget, Lisp_Object property)
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
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object widget = args[0];
  Lisp_Object property = args[1];
  Lisp_Object propval = Fwidget_get (widget, property);
  Lisp_Object trailing_args = Flist (nargs - 2, args + 2);
  Lisp_Object result = CALLN (Fapply, propval, widget, trailing_args);
  return result;
}

#ifdef HAVE_LANGINFO_CODESET
#include <langinfo.h>
#endif

DEFUN ("locale-info", Flocale_info, Slocale_info, 1, 1, 0,
       doc: /* Access locale data ITEM for the current C locale, if available.
ITEM should be one of the following:

`codeset', returning the character set as a string (locale item CODESET);

`days', returning a 7-element vector of day names (locale items DAY_n);

`months', returning a 12-element vector of month names (locale items MON_n);

`paper', returning a list (WIDTH HEIGHT) for the default paper size,
  both measured in millimeters (locale items PAPER_WIDTH, PAPER_HEIGHT).

If the system can't provide such information through a call to
`nl_langinfo', or if ITEM isn't from the list above, return nil.

See also Info node `(libc)Locales'.

The data read from the system are decoded using `locale-coding-system'.  */)
  (Lisp_Object item)
{
  char *str = NULL;
#ifdef HAVE_LANGINFO_CODESET
  if (EQ (item, Qcodeset))
    {
      str = nl_langinfo (CODESET);
      return build_string (str);
    }
#ifdef DAY_1
  else if (EQ (item, Qdays))	/* e.g. for calendar-day-name-array */
    {
      Lisp_Object v = Fmake_vector (make_number (7), Qnil);
      const int days[7] = {DAY_1, DAY_2, DAY_3, DAY_4, DAY_5, DAY_6, DAY_7};
      int i;
      synchronize_system_time_locale ();
      for (i = 0; i < 7; i++)
	{
	  str = nl_langinfo (days[i]);
	  AUTO_STRING (val, str);
	  /* Fixme: Is this coding system necessarily right, even if
	     it is consistent with CODESET?  If not, what to do?  */
	  ASET (v, i, code_convert_string_norecord (val, Vlocale_coding_system,
						    0));
	}
      return v;
    }
#endif	/* DAY_1 */
#ifdef MON_1
  else if (EQ (item, Qmonths))	/* e.g. for calendar-month-name-array */
    {
      Lisp_Object v = Fmake_vector (make_number (12), Qnil);
      const int months[12] = {MON_1, MON_2, MON_3, MON_4, MON_5, MON_6, MON_7,
			      MON_8, MON_9, MON_10, MON_11, MON_12};
      int i;
      synchronize_system_time_locale ();
      for (i = 0; i < 12; i++)
	{
	  str = nl_langinfo (months[i]);
	  AUTO_STRING (val, str);
	  ASET (v, i, code_convert_string_norecord (val, Vlocale_coding_system,
						    0));
	}
      return v;
    }
#endif	/* MON_1 */
/* LC_PAPER stuff isn't defined as accessible in glibc as of 2.3.1,
   but is in the locale files.  This could be used by ps-print.  */
#ifdef PAPER_WIDTH
  else if (EQ (item, Qpaper))
    return list2i (nl_langinfo (PAPER_WIDTH), nl_langinfo (PAPER_HEIGHT));
#endif	/* PAPER_WIDTH */
#endif	/* HAVE_LANGINFO_CODESET*/
  return Qnil;
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

/* Table of characters coding the 64 values.  */
static const char base64_value_to_char[64] =
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
static const short base64_char_to_value[128] =
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


static ptrdiff_t base64_encode_1 (const char *, char *, ptrdiff_t, bool, bool);
static ptrdiff_t base64_decode_1 (const char *, char *, ptrdiff_t, bool,
				  ptrdiff_t *);

DEFUN ("base64-encode-region", Fbase64_encode_region, Sbase64_encode_region,
       2, 3, "r",
       doc: /* Base64-encode the region between BEG and END.
Return the length of the encoded text.
Optional third argument NO-LINE-BREAK means do not break long lines
into shorter lines.  */)
  (Lisp_Object beg, Lisp_Object end, Lisp_Object no_line_break)
{
  char *encoded;
  ptrdiff_t allength, length;
  ptrdiff_t ibeg, iend, encoded_length;
  ptrdiff_t old_pos = PT;
  USE_SAFE_ALLOCA;

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

  encoded = SAFE_ALLOCA (allength);
  encoded_length = base64_encode_1 ((char *) BYTE_POS_ADDR (ibeg),
				    encoded, length, NILP (no_line_break),
				    !NILP (BVAR (current_buffer, enable_multibyte_characters)));
  if (encoded_length > allength)
    emacs_abort ();

  if (encoded_length < 0)
    {
      /* The encoding wasn't possible. */
      SAFE_FREE ();
      error ("Multibyte character in data for base64 encoding");
    }

  /* Now we have encoded the region, so we insert the new contents
     and delete the old.  (Insert first in order to preserve markers.)  */
  SET_PT_BOTH (XFASTINT (beg), ibeg);
  insert (encoded, encoded_length);
  SAFE_FREE ();
  del_range_byte (ibeg + encoded_length, iend + encoded_length);

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
  (Lisp_Object string, Lisp_Object no_line_break)
{
  ptrdiff_t allength, length, encoded_length;
  char *encoded;
  Lisp_Object encoded_string;
  USE_SAFE_ALLOCA;

  CHECK_STRING (string);

  /* We need to allocate enough room for encoding the text.
     We need 33 1/3% more space, plus a newline every 76
     characters, and then we round up. */
  length = SBYTES (string);
  allength = length + length/3 + 1;
  allength += allength / MIME_LINE_LENGTH + 1 + 6;

  /* We need to allocate enough room for decoding the text. */
  encoded = SAFE_ALLOCA (allength);

  encoded_length = base64_encode_1 (SSDATA (string),
				    encoded, length, NILP (no_line_break),
				    STRING_MULTIBYTE (string));
  if (encoded_length > allength)
    emacs_abort ();

  if (encoded_length < 0)
    {
      /* The encoding wasn't possible. */
      error ("Multibyte character in data for base64 encoding");
    }

  encoded_string = make_unibyte_string (encoded, encoded_length);
  SAFE_FREE ();

  return encoded_string;
}

static ptrdiff_t
base64_encode_1 (const char *from, char *to, ptrdiff_t length,
		 bool line_break, bool multibyte)
{
  int counter = 0;
  ptrdiff_t i = 0;
  char *e = to;
  int c;
  unsigned int value;
  int bytes;

  while (i < length)
    {
      if (multibyte)
	{
	  c = STRING_CHAR_AND_LENGTH ((unsigned char *) from + i, bytes);
	  if (CHAR_BYTE8_P (c))
	    c = CHAR_TO_BYTE8 (c);
	  else if (c >= 256)
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
	  c = STRING_CHAR_AND_LENGTH ((unsigned char *) from + i, bytes);
	  if (CHAR_BYTE8_P (c))
	    c = CHAR_TO_BYTE8 (c);
	  else if (c >= 256)
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
	  c = STRING_CHAR_AND_LENGTH ((unsigned char *) from + i, bytes);
	  if (CHAR_BYTE8_P (c))
	    c = CHAR_TO_BYTE8 (c);
	  else if (c >= 256)
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
  (Lisp_Object beg, Lisp_Object end)
{
  ptrdiff_t ibeg, iend, length, allength;
  char *decoded;
  ptrdiff_t old_pos = PT;
  ptrdiff_t decoded_length;
  ptrdiff_t inserted_chars;
  bool multibyte = !NILP (BVAR (current_buffer, enable_multibyte_characters));
  USE_SAFE_ALLOCA;

  validate_region (&beg, &end);

  ibeg = CHAR_TO_BYTE (XFASTINT (beg));
  iend = CHAR_TO_BYTE (XFASTINT (end));

  length = iend - ibeg;

  /* We need to allocate enough room for decoding the text.  If we are
     working on a multibyte buffer, each decoded code may occupy at
     most two bytes.  */
  allength = multibyte ? length * 2 : length;
  decoded = SAFE_ALLOCA (allength);

  move_gap_both (XFASTINT (beg), ibeg);
  decoded_length = base64_decode_1 ((char *) BYTE_POS_ADDR (ibeg),
				    decoded, length,
				    multibyte, &inserted_chars);
  if (decoded_length > allength)
    emacs_abort ();

  if (decoded_length < 0)
    {
      /* The decoding wasn't possible. */
      error ("Invalid base64 data");
    }

  /* Now we have decoded the region, so we insert the new contents
     and delete the old.  (Insert first in order to preserve markers.)  */
  TEMP_SET_PT_BOTH (XFASTINT (beg), ibeg);
  insert_1_both (decoded, inserted_chars, decoded_length, 0, 1, 0);
  SAFE_FREE ();

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
  (Lisp_Object string)
{
  char *decoded;
  ptrdiff_t length, decoded_length;
  Lisp_Object decoded_string;
  USE_SAFE_ALLOCA;

  CHECK_STRING (string);

  length = SBYTES (string);
  /* We need to allocate enough room for decoding the text. */
  decoded = SAFE_ALLOCA (length);

  /* The decoded result should be unibyte. */
  decoded_length = base64_decode_1 (SSDATA (string), decoded, length,
				    0, NULL);
  if (decoded_length > length)
    emacs_abort ();
  else if (decoded_length >= 0)
    decoded_string = make_unibyte_string (decoded, decoded_length);
  else
    decoded_string = Qnil;

  SAFE_FREE ();
  if (!STRINGP (decoded_string))
    error ("Invalid base64 data");

  return decoded_string;
}

/* Base64-decode the data at FROM of LENGTH bytes into TO.  If
   MULTIBYTE, the decoded result should be in multibyte
   form.  If NCHARS_RETURN is not NULL, store the number of produced
   characters in *NCHARS_RETURN.  */

static ptrdiff_t
base64_decode_1 (const char *from, char *to, ptrdiff_t length,
		 bool multibyte, ptrdiff_t *nchars_return)
{
  ptrdiff_t i = 0;		/* Used inside READ_QUADRUPLET_BYTE */
  char *e = to;
  unsigned char c;
  unsigned long value;
  ptrdiff_t nchars = 0;

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
      if (multibyte && c >= 128)
	e += BYTE8_STRING (c, e);
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
      if (multibyte && c >= 128)
	e += BYTE8_STRING (c, e);
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
      if (multibyte && c >= 128)
	e += BYTE8_STRING (c, e);
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


/* The list of all weak hash tables.  Don't staticpro this one.  */

static struct Lisp_Hash_Table *weak_hash_tables;


/***********************************************************************
			       Utilities
 ***********************************************************************/

static void
CHECK_HASH_TABLE (Lisp_Object x)
{
  CHECK_TYPE (HASH_TABLE_P (x), Qhash_table_p, x);
}

static void
set_hash_key_and_value (struct Lisp_Hash_Table *h, Lisp_Object key_and_value)
{
  h->key_and_value = key_and_value;
}
static void
set_hash_next (struct Lisp_Hash_Table *h, Lisp_Object next)
{
  h->next = next;
}
static void
set_hash_next_slot (struct Lisp_Hash_Table *h, ptrdiff_t idx, ptrdiff_t val)
{
  gc_aset (h->next, idx, make_number (val));
}
static void
set_hash_hash (struct Lisp_Hash_Table *h, Lisp_Object hash)
{
  h->hash = hash;
}
static void
set_hash_hash_slot (struct Lisp_Hash_Table *h, ptrdiff_t idx, Lisp_Object val)
{
  gc_aset (h->hash, idx, val);
}
static void
set_hash_index (struct Lisp_Hash_Table *h, Lisp_Object index)
{
  h->index = index;
}
static void
set_hash_index_slot (struct Lisp_Hash_Table *h, ptrdiff_t idx, ptrdiff_t val)
{
  gc_aset (h->index, idx, make_number (val));
}

/* If OBJ is a Lisp hash table, return a pointer to its struct
   Lisp_Hash_Table.  Otherwise, signal an error.  */

static struct Lisp_Hash_Table *
check_hash_table (Lisp_Object obj)
{
  CHECK_HASH_TABLE (obj);
  return XHASH_TABLE (obj);
}


/* Value is the next integer I >= N, N >= 0 which is "almost" a prime
   number.  A number is "almost" a prime number if it is not divisible
   by any integer in the range 2 .. (NEXT_ALMOST_PRIME_LIMIT - 1).  */

EMACS_INT
next_almost_prime (EMACS_INT n)
{
  verify (NEXT_ALMOST_PRIME_LIMIT == 11);
  for (n |= 1; ; n += 2)
    if (n % 3 != 0 && n % 5 != 0 && n % 7 != 0)
      return n;
}


/* Find KEY in ARGS which has size NARGS.  Don't consider indices for
   which USED[I] is non-zero.  If found at index I in ARGS, set
   USED[I] and USED[I + 1] to 1, and return I + 1.  Otherwise return
   0.  This function is used to extract a keyword/argument pair from
   a DEFUN parameter list.  */

static ptrdiff_t
get_key_arg (Lisp_Object key, ptrdiff_t nargs, Lisp_Object *args, char *used)
{
  ptrdiff_t i;

  for (i = 1; i < nargs; i++)
    if (!used[i - 1] && EQ (args[i - 1], key))
      {
	used[i - 1] = 1;
	used[i] = 1;
	return i;
      }

  return 0;
}


/* Return a Lisp vector which has the same contents as VEC but has
   at least INCR_MIN more entries, where INCR_MIN is positive.
   If NITEMS_MAX is not -1, do not grow the vector to be any larger
   than NITEMS_MAX.  New entries in the resulting vector are
   uninitialized.  */

static Lisp_Object
larger_vecalloc (Lisp_Object vec, ptrdiff_t incr_min, ptrdiff_t nitems_max)
{
  struct Lisp_Vector *v;
  ptrdiff_t incr, incr_max, old_size, new_size;
  ptrdiff_t C_language_max = min (PTRDIFF_MAX, SIZE_MAX) / sizeof *v->contents;
  ptrdiff_t n_max = (0 <= nitems_max && nitems_max < C_language_max
		     ? nitems_max : C_language_max);
  eassert (VECTORP (vec));
  eassert (0 < incr_min && -1 <= nitems_max);
  old_size = ASIZE (vec);
  incr_max = n_max - old_size;
  incr = max (incr_min, min (old_size >> 1, incr_max));
  if (incr_max < incr)
    memory_full (SIZE_MAX);
  new_size = old_size + incr;
  v = allocate_vector (new_size);
  memcpy (v->contents, XVECTOR (vec)->contents, old_size * sizeof *v->contents);
  XSETVECTOR (vec, v);
  return vec;
}

/* Likewise, except set new entries in the resulting vector to nil.  */

Lisp_Object
larger_vector (Lisp_Object vec, ptrdiff_t incr_min, ptrdiff_t nitems_max)
{
  ptrdiff_t old_size = ASIZE (vec);
  Lisp_Object v = larger_vecalloc (vec, incr_min, nitems_max);
  ptrdiff_t new_size = ASIZE (v);
  memclear (XVECTOR (v)->contents + old_size,
	    (new_size - old_size) * word_size);
  return v;
}


/***********************************************************************
			 Low-level Functions
 ***********************************************************************/

/* Return the index of the next entry in H following the one at IDX,
   or -1 if none.  */

static ptrdiff_t
HASH_NEXT (struct Lisp_Hash_Table *h, ptrdiff_t idx)
{
  return XINT (AREF (h->next, idx));
}

/* Return the index of the element in hash table H that is the start
   of the collision list at index IDX, or -1 if the list is empty.  */

static ptrdiff_t
HASH_INDEX (struct Lisp_Hash_Table *h, ptrdiff_t idx)
{
  return XINT (AREF (h->index, idx));
}

/* Compare KEY1 which has hash code HASH1 and KEY2 with hash code
   HASH2 in hash table H using `eql'.  Value is true if KEY1 and
   KEY2 are the same.  */

static bool
cmpfn_eql (struct hash_table_test *ht,
	   Lisp_Object key1,
	   Lisp_Object key2)
{
  return (FLOATP (key1)
	  && FLOATP (key2)
	  && XFLOAT_DATA (key1) == XFLOAT_DATA (key2));
}


/* Compare KEY1 which has hash code HASH1 and KEY2 with hash code
   HASH2 in hash table H using `equal'.  Value is true if KEY1 and
   KEY2 are the same.  */

static bool
cmpfn_equal (struct hash_table_test *ht,
	     Lisp_Object key1,
	     Lisp_Object key2)
{
  return !NILP (Fequal (key1, key2));
}


/* Compare KEY1 which has hash code HASH1, and KEY2 with hash code
   HASH2 in hash table H using H->user_cmp_function.  Value is true
   if KEY1 and KEY2 are the same.  */

static bool
cmpfn_user_defined (struct hash_table_test *ht,
		    Lisp_Object key1,
		    Lisp_Object key2)
{
  return !NILP (call2 (ht->user_cmp_function, key1, key2));
}

/* Value is a hash code for KEY for use in hash table H which uses
   `eq' to compare keys.  The hash code returned is guaranteed to fit
   in a Lisp integer.  */

static EMACS_UINT
hashfn_eq (struct hash_table_test *ht, Lisp_Object key)
{
  return XHASH (key) ^ XTYPE (key);
}

/* Value is a hash code for KEY for use in hash table H which uses
   `equal' to compare keys.  The hash code returned is guaranteed to fit
   in a Lisp integer.  */

static EMACS_UINT
hashfn_equal (struct hash_table_test *ht, Lisp_Object key)
{
  return sxhash (key, 0);
}

/* Value is a hash code for KEY for use in hash table H which uses
   `eql' to compare keys.  The hash code returned is guaranteed to fit
   in a Lisp integer.  */

static EMACS_UINT
hashfn_eql (struct hash_table_test *ht, Lisp_Object key)
{
  return FLOATP (key) ? hashfn_equal (ht, key) : hashfn_eq (ht, key);
}

/* Value is a hash code for KEY for use in hash table H which uses as
   user-defined function to compare keys.  The hash code returned is
   guaranteed to fit in a Lisp integer.  */

static EMACS_UINT
hashfn_user_defined (struct hash_table_test *ht, Lisp_Object key)
{
  Lisp_Object hash = call1 (ht->user_hash_function, key);
  return hashfn_eq (ht, hash);
}

struct hash_table_test const
  hashtest_eq = { LISPSYM_INITIALLY (Qeq), LISPSYM_INITIALLY (Qnil),
		  LISPSYM_INITIALLY (Qnil), 0, hashfn_eq },
  hashtest_eql = { LISPSYM_INITIALLY (Qeql), LISPSYM_INITIALLY (Qnil),
		   LISPSYM_INITIALLY (Qnil), cmpfn_eql, hashfn_eql },
  hashtest_equal = { LISPSYM_INITIALLY (Qequal), LISPSYM_INITIALLY (Qnil),
		     LISPSYM_INITIALLY (Qnil), cmpfn_equal, hashfn_equal };

/* Allocate basically initialized hash table.  */

static struct Lisp_Hash_Table *
allocate_hash_table (void)
{
  return ALLOCATE_PSEUDOVECTOR (struct Lisp_Hash_Table,
				count, PVEC_HASH_TABLE);
}

/* An upper bound on the size of a hash table index.  It must fit in
   ptrdiff_t and be a valid Emacs fixnum.  */
#define INDEX_SIZE_BOUND \
  ((ptrdiff_t) min (MOST_POSITIVE_FIXNUM, PTRDIFF_MAX / word_size))

/* Create and initialize a new hash table.

   TEST specifies the test the hash table will use to compare keys.
   It must be either one of the predefined tests `eq', `eql' or
   `equal' or a symbol denoting a user-defined test named TEST with
   test and hash functions USER_TEST and USER_HASH.

   Give the table initial capacity SIZE, 0 <= SIZE <= MOST_POSITIVE_FIXNUM.

   If REHASH_SIZE is equal to a negative integer, this hash table's
   new size when it becomes full is computed by subtracting
   REHASH_SIZE from its old size.  Otherwise it must be positive, and
   the table's new size is computed by multiplying its old size by
   REHASH_SIZE + 1.

   REHASH_THRESHOLD must be a float <= 1.0, and > 0.  The table will
   be resized when the approximate ratio of table entries to table
   size exceeds REHASH_THRESHOLD.

   WEAK specifies the weakness of the table.  If non-nil, it must be
   one of the symbols `key', `value', `key-or-value', or `key-and-value'.

   If PURECOPY is non-nil, the table can be copied to pure storage via
   `purecopy' when Emacs is being dumped. Such tables can no longer be
   changed after purecopy.  */

Lisp_Object
make_hash_table (struct hash_table_test test, EMACS_INT size,
		 float rehash_size, float rehash_threshold,
		 Lisp_Object weak, bool pure)
{
  struct Lisp_Hash_Table *h;
  Lisp_Object table;
  EMACS_INT index_size;
  ptrdiff_t i;
  double index_float;

  /* Preconditions.  */
  eassert (SYMBOLP (test.name));
  eassert (0 <= size && size <= MOST_POSITIVE_FIXNUM);
  eassert (rehash_size <= -1 || 0 < rehash_size);
  eassert (0 < rehash_threshold && rehash_threshold <= 1);

  if (size == 0)
    size = 1;

  double threshold = rehash_threshold;
  index_float = size / threshold;
  index_size = (index_float < INDEX_SIZE_BOUND + 1
		? next_almost_prime (index_float)
		: INDEX_SIZE_BOUND + 1);
  if (INDEX_SIZE_BOUND < max (index_size, 2 * size))
    error ("Hash table too large");

  /* Allocate a table and initialize it.  */
  h = allocate_hash_table ();

  /* Initialize hash table slots.  */
  h->test = test;
  h->weak = weak;
  h->rehash_threshold = rehash_threshold;
  h->rehash_size = rehash_size;
  h->count = 0;
  h->key_and_value = Fmake_vector (make_number (2 * size), Qnil);
  h->hash = Fmake_vector (make_number (size), Qnil);
  h->next = Fmake_vector (make_number (size), make_number (-1));
  h->index = Fmake_vector (make_number (index_size), make_number (-1));
  h->pure = pure;

  /* Set up the free list.  */
  for (i = 0; i < size - 1; ++i)
    set_hash_next_slot (h, i, i + 1);
  h->next_free = 0;

  XSET_HASH_TABLE (table, h);
  eassert (HASH_TABLE_P (table));
  eassert (XHASH_TABLE (table) == h);

  /* Maybe add this hash table to the list of all weak hash tables.  */
  if (! NILP (weak))
    {
      h->next_weak = weak_hash_tables;
      weak_hash_tables = h;
    }

  return table;
}


/* Return a copy of hash table H1.  Keys and values are not copied,
   only the table itself is.  */

static Lisp_Object
copy_hash_table (struct Lisp_Hash_Table *h1)
{
  Lisp_Object table;
  struct Lisp_Hash_Table *h2;

  h2 = allocate_hash_table ();
  *h2 = *h1;
  h2->key_and_value = Fcopy_sequence (h1->key_and_value);
  h2->hash = Fcopy_sequence (h1->hash);
  h2->next = Fcopy_sequence (h1->next);
  h2->index = Fcopy_sequence (h1->index);
  XSET_HASH_TABLE (table, h2);

  /* Maybe add this hash table to the list of all weak hash tables.  */
  if (!NILP (h2->weak))
    {
      h2->next_weak = h1->next_weak;
      h1->next_weak = h2;
    }

  return table;
}


/* Resize hash table H if it's too full.  If H cannot be resized
   because it's already too large, throw an error.  */

static void
maybe_resize_hash_table (struct Lisp_Hash_Table *h)
{
  if (h->next_free < 0)
    {
      ptrdiff_t old_size = HASH_TABLE_SIZE (h);
      EMACS_INT new_size, index_size, nsize;
      ptrdiff_t i;
      double rehash_size = h->rehash_size;
      double index_float;

      if (rehash_size < 0)
	new_size = old_size - rehash_size;
      else
	{
	  double float_new_size = old_size * (rehash_size + 1);
	  if (float_new_size < INDEX_SIZE_BOUND + 1)
	    new_size = float_new_size;
	  else
	    new_size = INDEX_SIZE_BOUND + 1;
	}
      if (new_size <= old_size)
	new_size = old_size + 1;
      double threshold = h->rehash_threshold;
      index_float = new_size / threshold;
      index_size = (index_float < INDEX_SIZE_BOUND + 1
		    ? next_almost_prime (index_float)
		    : INDEX_SIZE_BOUND + 1);
      nsize = max (index_size, 2 * new_size);
      if (INDEX_SIZE_BOUND < nsize)
	error ("Hash table too large to resize");

#ifdef ENABLE_CHECKING
      if (HASH_TABLE_P (Vpurify_flag)
	  && XHASH_TABLE (Vpurify_flag) == h)
	message ("Growing hash table to: %"pI"d", new_size);
#endif

      set_hash_key_and_value (h, larger_vector (h->key_and_value,
						2 * (new_size - old_size), -1));
      set_hash_hash (h, larger_vector (h->hash, new_size - old_size, -1));
      set_hash_index (h, Fmake_vector (make_number (index_size),
				       make_number (-1)));
      set_hash_next (h, larger_vecalloc (h->next, new_size - old_size, -1));

      /* Update the free list.  Do it so that new entries are added at
         the end of the free list.  This makes some operations like
         maphash faster.  */
      for (i = old_size; i < new_size - 1; ++i)
	set_hash_next_slot (h, i, i + 1);
      set_hash_next_slot (h, i, -1);

      if (h->next_free < 0)
	h->next_free = old_size;
      else
	{
	  ptrdiff_t last = h->next_free;
	  while (true)
	    {
	      ptrdiff_t next = HASH_NEXT (h, last);
	      if (next < 0)
		break;
	      last = next;
	    }
	  set_hash_next_slot (h, last, old_size);
	}

      /* Rehash.  */
      for (i = 0; i < old_size; ++i)
	if (!NILP (HASH_HASH (h, i)))
	  {
	    EMACS_UINT hash_code = XUINT (HASH_HASH (h, i));
	    ptrdiff_t start_of_bucket = hash_code % ASIZE (h->index);
	    set_hash_next_slot (h, i, HASH_INDEX (h, start_of_bucket));
	    set_hash_index_slot (h, start_of_bucket, i);
	  }
    }
}


/* Lookup KEY in hash table H.  If HASH is non-null, return in *HASH
   the hash code of KEY.  Value is the index of the entry in H
   matching KEY, or -1 if not found.  */

ptrdiff_t
hash_lookup (struct Lisp_Hash_Table *h, Lisp_Object key, EMACS_UINT *hash)
{
  EMACS_UINT hash_code;
  ptrdiff_t start_of_bucket, i;

  hash_code = h->test.hashfn (&h->test, key);
  eassert ((hash_code & ~INTMASK) == 0);
  if (hash)
    *hash = hash_code;

  start_of_bucket = hash_code % ASIZE (h->index);

  for (i = HASH_INDEX (h, start_of_bucket); 0 <= i; i = HASH_NEXT (h, i))
    if (EQ (key, HASH_KEY (h, i))
	|| (h->test.cmpfn
	    && hash_code == XUINT (HASH_HASH (h, i))
	    && h->test.cmpfn (&h->test, key, HASH_KEY (h, i))))
      break;

  return i;
}


/* Put an entry into hash table H that associates KEY with VALUE.
   HASH is a previously computed hash code of KEY.
   Value is the index of the entry in H matching KEY.  */

ptrdiff_t
hash_put (struct Lisp_Hash_Table *h, Lisp_Object key, Lisp_Object value,
	  EMACS_UINT hash)
{
  ptrdiff_t start_of_bucket, i;

  eassert ((hash & ~INTMASK) == 0);

  /* Increment count after resizing because resizing may fail.  */
  maybe_resize_hash_table (h);
  h->count++;

  /* Store key/value in the key_and_value vector.  */
  i = h->next_free;
  h->next_free = HASH_NEXT (h, i);
  set_hash_key_slot (h, i, key);
  set_hash_value_slot (h, i, value);

  /* Remember its hash code.  */
  set_hash_hash_slot (h, i, make_number (hash));

  /* Add new entry to its collision chain.  */
  start_of_bucket = hash % ASIZE (h->index);
  set_hash_next_slot (h, i, HASH_INDEX (h, start_of_bucket));
  set_hash_index_slot (h, start_of_bucket, i);
  return i;
}


/* Remove the entry matching KEY from hash table H, if there is one.  */

void
hash_remove_from_table (struct Lisp_Hash_Table *h, Lisp_Object key)
{
  EMACS_UINT hash_code = h->test.hashfn (&h->test, key);
  eassert ((hash_code & ~INTMASK) == 0);
  ptrdiff_t start_of_bucket = hash_code % ASIZE (h->index);
  ptrdiff_t prev = -1;

  for (ptrdiff_t i = HASH_INDEX (h, start_of_bucket);
       0 <= i;
       i = HASH_NEXT (h, i))
    {
      if (EQ (key, HASH_KEY (h, i))
	  || (h->test.cmpfn
	      && hash_code == XUINT (HASH_HASH (h, i))
	      && h->test.cmpfn (&h->test, key, HASH_KEY (h, i))))
	{
	  /* Take entry out of collision chain.  */
	  if (prev < 0)
	    set_hash_index_slot (h, start_of_bucket, HASH_NEXT (h, i));
	  else
	    set_hash_next_slot (h, prev, HASH_NEXT (h, i));

	  /* Clear slots in key_and_value and add the slots to
	     the free list.  */
	  set_hash_key_slot (h, i, Qnil);
	  set_hash_value_slot (h, i, Qnil);
	  set_hash_hash_slot (h, i, Qnil);
	  set_hash_next_slot (h, i, h->next_free);
	  h->next_free = i;
	  h->count--;
	  eassert (h->count >= 0);
	  break;
	}

      prev = i;
    }
}


/* Clear hash table H.  */

static void
hash_clear (struct Lisp_Hash_Table *h)
{
  if (h->count > 0)
    {
      ptrdiff_t i, size = HASH_TABLE_SIZE (h);

      for (i = 0; i < size; ++i)
	{
	  set_hash_next_slot (h, i, i < size - 1 ? i + 1 : -1);
	  set_hash_key_slot (h, i, Qnil);
	  set_hash_value_slot (h, i, Qnil);
	  set_hash_hash_slot (h, i, Qnil);
	}

      for (i = 0; i < ASIZE (h->index); ++i)
	ASET (h->index, i, make_number (-1));

      h->next_free = 0;
      h->count = 0;
    }
}



/************************************************************************
			   Weak Hash Tables
 ************************************************************************/

/* Sweep weak hash table H.  REMOVE_ENTRIES_P means remove
   entries from the table that don't survive the current GC.
   !REMOVE_ENTRIES_P means mark entries that are in use.  Value is
   true if anything was marked.  */

static bool
sweep_weak_table (struct Lisp_Hash_Table *h, bool remove_entries_p)
{
  ptrdiff_t n = gc_asize (h->index);
  bool marked = false;

  for (ptrdiff_t bucket = 0; bucket < n; ++bucket)
    {
      /* Follow collision chain, removing entries that
	 don't survive this garbage collection.  */
      ptrdiff_t prev = -1;
      ptrdiff_t next;
      for (ptrdiff_t i = HASH_INDEX (h, bucket); 0 <= i; i = next)
	{
	  bool key_known_to_survive_p = survives_gc_p (HASH_KEY (h, i));
	  bool value_known_to_survive_p = survives_gc_p (HASH_VALUE (h, i));
	  bool remove_p;

	  if (EQ (h->weak, Qkey))
	    remove_p = !key_known_to_survive_p;
	  else if (EQ (h->weak, Qvalue))
	    remove_p = !value_known_to_survive_p;
	  else if (EQ (h->weak, Qkey_or_value))
	    remove_p = !(key_known_to_survive_p || value_known_to_survive_p);
	  else if (EQ (h->weak, Qkey_and_value))
	    remove_p = !(key_known_to_survive_p && value_known_to_survive_p);
	  else
	    emacs_abort ();

	  next = HASH_NEXT (h, i);

	  if (remove_entries_p)
	    {
	      if (remove_p)
		{
		  /* Take out of collision chain.  */
		  if (prev < 0)
		    set_hash_index_slot (h, bucket, next);
		  else
		    set_hash_next_slot (h, prev, next);

		  /* Add to free list.  */
		  set_hash_next_slot (h, i, h->next_free);
		  h->next_free = i;

		  /* Clear key, value, and hash.  */
		  set_hash_key_slot (h, i, Qnil);
		  set_hash_value_slot (h, i, Qnil);
		  set_hash_hash_slot (h, i, Qnil);

		  h->count--;
		}
	      else
		{
		  prev = i;
		}
	    }
	  else
	    {
	      if (!remove_p)
		{
		  /* Make sure key and value survive.  */
		  if (!key_known_to_survive_p)
		    {
		      mark_object (HASH_KEY (h, i));
		      marked = 1;
		    }

		  if (!value_known_to_survive_p)
		    {
		      mark_object (HASH_VALUE (h, i));
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

NO_INLINE /* For better stack traces */
void
sweep_weak_hash_tables (void)
{
  struct Lisp_Hash_Table *h, *used, *next;
  bool marked;

  /* Mark all keys and values that are in use.  Keep on marking until
     there is no more change.  This is necessary for cases like
     value-weak table A containing an entry X -> Y, where Y is used in a
     key-weak table B, Z -> Y.  If B comes after A in the list of weak
     tables, X -> Y might be removed from A, although when looking at B
     one finds that it shouldn't.  */
  do
    {
      marked = 0;
      for (h = weak_hash_tables; h; h = h->next_weak)
	{
	  if (h->header.size & ARRAY_MARK_FLAG)
	    marked |= sweep_weak_table (h, 0);
	}
    }
  while (marked);

  /* Remove tables and entries that aren't used.  */
  for (h = weak_hash_tables, used = NULL; h; h = next)
    {
      next = h->next_weak;

      if (h->header.size & ARRAY_MARK_FLAG)
	{
	  /* TABLE is marked as used.  Sweep its contents.  */
	  if (h->count > 0)
	    sweep_weak_table (h, 1);

	  /* Add table to the list of used weak hash tables.  */
	  h->next_weak = used;
	  used = h;
	}
    }

  weak_hash_tables = used;
}



/***********************************************************************
			Hash Code Computation
 ***********************************************************************/

/* Maximum depth up to which to dive into Lisp structures.  */

#define SXHASH_MAX_DEPTH 3

/* Maximum length up to which to take list and vector elements into
   account.  */

#define SXHASH_MAX_LEN   7

/* Return a hash for string PTR which has length LEN.  The hash value
   can be any EMACS_UINT value.  */

EMACS_UINT
hash_string (char const *ptr, ptrdiff_t len)
{
  char const *p = ptr;
  char const *end = p + len;
  unsigned char c;
  EMACS_UINT hash = 0;

  while (p != end)
    {
      c = *p++;
      hash = sxhash_combine (hash, c);
    }

  return hash;
}

/* Return a hash for string PTR which has length LEN.  The hash
   code returned is guaranteed to fit in a Lisp integer.  */

static EMACS_UINT
sxhash_string (char const *ptr, ptrdiff_t len)
{
  EMACS_UINT hash = hash_string (ptr, len);
  return SXHASH_REDUCE (hash);
}

/* Return a hash for the floating point value VAL.  */

static EMACS_UINT
sxhash_float (double val)
{
  EMACS_UINT hash = 0;
  enum {
    WORDS_PER_DOUBLE = (sizeof val / sizeof hash
			+ (sizeof val % sizeof hash != 0))
  };
  union {
    double val;
    EMACS_UINT word[WORDS_PER_DOUBLE];
  } u;
  int i;
  u.val = val;
  memset (&u.val + 1, 0, sizeof u - sizeof u.val);
  for (i = 0; i < WORDS_PER_DOUBLE; i++)
    hash = sxhash_combine (hash, u.word[i]);
  return SXHASH_REDUCE (hash);
}

/* Return a hash for list LIST.  DEPTH is the current depth in the
   list.  We don't recurse deeper than SXHASH_MAX_DEPTH in it.  */

static EMACS_UINT
sxhash_list (Lisp_Object list, int depth)
{
  EMACS_UINT hash = 0;
  int i;

  if (depth < SXHASH_MAX_DEPTH)
    for (i = 0;
	 CONSP (list) && i < SXHASH_MAX_LEN;
	 list = XCDR (list), ++i)
      {
	EMACS_UINT hash2 = sxhash (XCAR (list), depth + 1);
	hash = sxhash_combine (hash, hash2);
      }

  if (!NILP (list))
    {
      EMACS_UINT hash2 = sxhash (list, depth + 1);
      hash = sxhash_combine (hash, hash2);
    }

  return SXHASH_REDUCE (hash);
}


/* Return a hash for (pseudo)vector VECTOR.  DEPTH is the current depth in
   the Lisp structure.  */

static EMACS_UINT
sxhash_vector (Lisp_Object vec, int depth)
{
  EMACS_UINT hash = ASIZE (vec);
  int i, n;

  n = min (SXHASH_MAX_LEN, hash & PSEUDOVECTOR_FLAG ? PVSIZE (vec) : hash);
  for (i = 0; i < n; ++i)
    {
      EMACS_UINT hash2 = sxhash (AREF (vec, i), depth + 1);
      hash = sxhash_combine (hash, hash2);
    }

  return SXHASH_REDUCE (hash);
}

/* Return a hash for bool-vector VECTOR.  */

static EMACS_UINT
sxhash_bool_vector (Lisp_Object vec)
{
  EMACS_INT size = bool_vector_size (vec);
  EMACS_UINT hash = size;
  int i, n;

  n = min (SXHASH_MAX_LEN, bool_vector_words (size));
  for (i = 0; i < n; ++i)
    hash = sxhash_combine (hash, bool_vector_data (vec)[i]);

  return SXHASH_REDUCE (hash);
}


/* Return a hash code for OBJ.  DEPTH is the current depth in the Lisp
   structure.  Value is an unsigned integer clipped to INTMASK.  */

EMACS_UINT
sxhash (Lisp_Object obj, int depth)
{
  EMACS_UINT hash;

  if (depth > SXHASH_MAX_DEPTH)
    return 0;

  switch (XTYPE (obj))
    {
    case_Lisp_Int:
      hash = XUINT (obj);
      break;

    case Lisp_Misc:
    case Lisp_Symbol:
      hash = XHASH (obj);
      break;

    case Lisp_String:
      hash = sxhash_string (SSDATA (obj), SBYTES (obj));
      break;

      /* This can be everything from a vector to an overlay.  */
    case Lisp_Vectorlike:
      if (VECTORP (obj) || RECORDP (obj))
	/* According to the CL HyperSpec, two arrays are equal only if
	   they are `eq', except for strings and bit-vectors.  In
	   Emacs, this works differently.  We have to compare element
	   by element.  Same for records.  */
	hash = sxhash_vector (obj, depth);
      else if (BOOL_VECTOR_P (obj))
	hash = sxhash_bool_vector (obj);
      else
	/* Others are `equal' if they are `eq', so let's take their
	   address as hash.  */
	hash = XHASH (obj);
      break;

    case Lisp_Cons:
      hash = sxhash_list (obj, depth);
      break;

    case Lisp_Float:
      hash = sxhash_float (XFLOAT_DATA (obj));
      break;

    default:
      emacs_abort ();
    }

  return hash;
}



/***********************************************************************
			    Lisp Interface
 ***********************************************************************/

DEFUN ("sxhash-eq", Fsxhash_eq, Ssxhash_eq, 1, 1, 0,
       doc: /* Return an integer hash code for OBJ suitable for `eq'.
If (eq A B), then (= (sxhash-eq A) (sxhash-eq B)).  */)
  (Lisp_Object obj)
{
  return make_number (hashfn_eq (NULL, obj));
}

DEFUN ("sxhash-eql", Fsxhash_eql, Ssxhash_eql, 1, 1, 0,
       doc: /* Return an integer hash code for OBJ suitable for `eql'.
If (eql A B), then (= (sxhash-eql A) (sxhash-eql B)).  */)
  (Lisp_Object obj)
{
  return make_number (hashfn_eql (NULL, obj));
}

DEFUN ("sxhash-equal", Fsxhash_equal, Ssxhash_equal, 1, 1, 0,
       doc: /* Return an integer hash code for OBJ suitable for `equal'.
If (equal A B), then (= (sxhash-equal A) (sxhash-equal B)).  */)
  (Lisp_Object obj)
{
  return make_number (hashfn_equal (NULL, obj));
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
fills up.  If REHASH-SIZE is an integer, increase the size by that
amount.  If it is a float, it must be > 1.0, and the new size is the
old size multiplied by that factor.  Default is 1.5.

:rehash-threshold THRESHOLD -- THRESHOLD must a float > 0, and <= 1.0.
Resize the hash table when the ratio (table entries / table size)
exceeds an approximation to THRESHOLD.  Default is 0.8125.

:weakness WEAK -- WEAK must be one of nil, t, `key', `value',
`key-or-value', or `key-and-value'.  If WEAK is not nil, the table
returned is a weak table.  Key/value pairs are removed from a weak
hash table when there are no non-weak references pointing to their
key, value, one of key or value, or both key and value, depending on
WEAK.  WEAK t is equivalent to `key-and-value'.  Default value of WEAK
is nil.

:purecopy PURECOPY -- If PURECOPY is non-nil, the table can be copied
to pure storage when Emacs is being dumped, making the contents of the
table read only. Any further changes to purified tables will result
in an error.

usage: (make-hash-table &rest KEYWORD-ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object test, weak;
  bool pure;
  struct hash_table_test testdesc;
  ptrdiff_t i;
  USE_SAFE_ALLOCA;

  /* The vector `used' is used to keep track of arguments that
     have been consumed.  */
  char *used = SAFE_ALLOCA (nargs * sizeof *used);
  memset (used, 0, nargs * sizeof *used);

  /* See if there's a `:test TEST' among the arguments.  */
  i = get_key_arg (QCtest, nargs, args, used);
  test = i ? args[i] : Qeql;
  if (EQ (test, Qeq))
    testdesc = hashtest_eq;
  else if (EQ (test, Qeql))
    testdesc = hashtest_eql;
  else if (EQ (test, Qequal))
    testdesc = hashtest_equal;
  else
    {
      /* See if it is a user-defined test.  */
      Lisp_Object prop;

      prop = Fget (test, Qhash_table_test);
      if (!CONSP (prop) || !CONSP (XCDR (prop)))
	signal_error ("Invalid hash table test", test);
      testdesc.name = test;
      testdesc.user_cmp_function = XCAR (prop);
      testdesc.user_hash_function = XCAR (XCDR (prop));
      testdesc.hashfn = hashfn_user_defined;
      testdesc.cmpfn = cmpfn_user_defined;
    }

  /* See if there's a `:purecopy PURECOPY' argument.  */
  i = get_key_arg (QCpurecopy, nargs, args, used);
  pure = i && !NILP (args[i]);
  /* See if there's a `:size SIZE' argument.  */
  i = get_key_arg (QCsize, nargs, args, used);
  Lisp_Object size_arg = i ? args[i] : Qnil;
  EMACS_INT size;
  if (NILP (size_arg))
    size = DEFAULT_HASH_SIZE;
  else if (NATNUMP (size_arg))
    size = XFASTINT (size_arg);
  else
    signal_error ("Invalid hash table size", size_arg);

  /* Look for `:rehash-size SIZE'.  */
  float rehash_size;
  i = get_key_arg (QCrehash_size, nargs, args, used);
  if (!i)
    rehash_size = DEFAULT_REHASH_SIZE;
  else if (INTEGERP (args[i]) && 0 < XINT (args[i]))
    rehash_size = - XINT (args[i]);
  else if (FLOATP (args[i]) && 0 < (float) (XFLOAT_DATA (args[i]) - 1))
    rehash_size = (float) (XFLOAT_DATA (args[i]) - 1);
  else
    signal_error ("Invalid hash table rehash size", args[i]);

  /* Look for `:rehash-threshold THRESHOLD'.  */
  i = get_key_arg (QCrehash_threshold, nargs, args, used);
  float rehash_threshold = (!i ? DEFAULT_REHASH_THRESHOLD
			    : !FLOATP (args[i]) ? 0
			    : (float) XFLOAT_DATA (args[i]));
  if (! (0 < rehash_threshold && rehash_threshold <= 1))
    signal_error ("Invalid hash table rehash threshold", args[i]);

  /* Look for `:weakness WEAK'.  */
  i = get_key_arg (QCweakness, nargs, args, used);
  weak = i ? args[i] : Qnil;
  if (EQ (weak, Qt))
    weak = Qkey_and_value;
  if (!NILP (weak)
      && !EQ (weak, Qkey)
      && !EQ (weak, Qvalue)
      && !EQ (weak, Qkey_or_value)
      && !EQ (weak, Qkey_and_value))
    signal_error ("Invalid hash table weakness", weak);

  /* Now, all args should have been used up, or there's a problem.  */
  for (i = 0; i < nargs; ++i)
    if (!used[i])
      signal_error ("Invalid argument list", args[i]);

  SAFE_FREE ();
  return make_hash_table (testdesc, size, rehash_size, rehash_threshold, weak,
                          pure);
}


DEFUN ("copy-hash-table", Fcopy_hash_table, Scopy_hash_table, 1, 1, 0,
       doc: /* Return a copy of hash table TABLE.  */)
  (Lisp_Object table)
{
  return copy_hash_table (check_hash_table (table));
}


DEFUN ("hash-table-count", Fhash_table_count, Shash_table_count, 1, 1, 0,
       doc: /* Return the number of elements in TABLE.  */)
  (Lisp_Object table)
{
  return make_number (check_hash_table (table)->count);
}


DEFUN ("hash-table-rehash-size", Fhash_table_rehash_size,
       Shash_table_rehash_size, 1, 1, 0,
       doc: /* Return the current rehash size of TABLE.  */)
  (Lisp_Object table)
{
  double rehash_size = check_hash_table (table)->rehash_size;
  if (rehash_size < 0)
    {
      EMACS_INT s = -rehash_size;
      return make_number (min (s, MOST_POSITIVE_FIXNUM));
    }
  else
    return make_float (rehash_size + 1);
}


DEFUN ("hash-table-rehash-threshold", Fhash_table_rehash_threshold,
       Shash_table_rehash_threshold, 1, 1, 0,
       doc: /* Return the current rehash threshold of TABLE.  */)
  (Lisp_Object table)
{
  return make_float (check_hash_table (table)->rehash_threshold);
}


DEFUN ("hash-table-size", Fhash_table_size, Shash_table_size, 1, 1, 0,
       doc: /* Return the size of TABLE.
The size can be used as an argument to `make-hash-table' to create
a hash table than can hold as many elements as TABLE holds
without need for resizing.  */)
  (Lisp_Object table)
{
  struct Lisp_Hash_Table *h = check_hash_table (table);
  return make_number (HASH_TABLE_SIZE (h));
}


DEFUN ("hash-table-test", Fhash_table_test, Shash_table_test, 1, 1, 0,
       doc: /* Return the test TABLE uses.  */)
  (Lisp_Object table)
{
  return check_hash_table (table)->test.name;
}


DEFUN ("hash-table-weakness", Fhash_table_weakness, Shash_table_weakness,
       1, 1, 0,
       doc: /* Return the weakness of TABLE.  */)
  (Lisp_Object table)
{
  return check_hash_table (table)->weak;
}


DEFUN ("hash-table-p", Fhash_table_p, Shash_table_p, 1, 1, 0,
       doc: /* Return t if OBJ is a Lisp hash table object.  */)
  (Lisp_Object obj)
{
  return HASH_TABLE_P (obj) ? Qt : Qnil;
}


DEFUN ("clrhash", Fclrhash, Sclrhash, 1, 1, 0,
       doc: /* Clear hash table TABLE and return it.  */)
  (Lisp_Object table)
{
  struct Lisp_Hash_Table *h = check_hash_table (table);
  CHECK_IMPURE (table, h);
  hash_clear (h);
  /* Be compatible with XEmacs.  */
  return table;
}


DEFUN ("gethash", Fgethash, Sgethash, 2, 3, 0,
       doc: /* Look up KEY in TABLE and return its associated value.
If KEY is not found, return DFLT which defaults to nil.  */)
  (Lisp_Object key, Lisp_Object table, Lisp_Object dflt)
{
  struct Lisp_Hash_Table *h = check_hash_table (table);
  ptrdiff_t i = hash_lookup (h, key, NULL);
  return i >= 0 ? HASH_VALUE (h, i) : dflt;
}


DEFUN ("puthash", Fputhash, Sputhash, 3, 3, 0,
       doc: /* Associate KEY with VALUE in hash table TABLE.
If KEY is already present in table, replace its current value with
VALUE.  In any case, return VALUE.  */)
  (Lisp_Object key, Lisp_Object value, Lisp_Object table)
{
  struct Lisp_Hash_Table *h = check_hash_table (table);
  CHECK_IMPURE (table, h);

  ptrdiff_t i;
  EMACS_UINT hash;
  i = hash_lookup (h, key, &hash);
  if (i >= 0)
    set_hash_value_slot (h, i, value);
  else
    hash_put (h, key, value, hash);

  return value;
}


DEFUN ("remhash", Fremhash, Sremhash, 2, 2, 0,
       doc: /* Remove KEY from TABLE.  */)
  (Lisp_Object key, Lisp_Object table)
{
  struct Lisp_Hash_Table *h = check_hash_table (table);
  CHECK_IMPURE (table, h);
  hash_remove_from_table (h, key);
  return Qnil;
}


DEFUN ("maphash", Fmaphash, Smaphash, 2, 2, 0,
       doc: /* Call FUNCTION for all entries in hash table TABLE.
FUNCTION is called with two arguments, KEY and VALUE.
`maphash' always returns nil.  */)
  (Lisp_Object function, Lisp_Object table)
{
  struct Lisp_Hash_Table *h = check_hash_table (table);

  for (ptrdiff_t i = 0; i < HASH_TABLE_SIZE (h); ++i)
    if (!NILP (HASH_HASH (h, i)))
      call2 (function, HASH_KEY (h, i), HASH_VALUE (h, i));

  return Qnil;
}


DEFUN ("define-hash-table-test", Fdefine_hash_table_test,
       Sdefine_hash_table_test, 3, 3, 0,
       doc: /* Define a new hash table test with name NAME, a symbol.

In hash tables created with NAME specified as test, use TEST to
compare keys, and HASH for computing hash codes of keys.

TEST must be a function taking two arguments and returning non-nil if
both arguments are the same.  HASH must be a function taking one
argument and returning an object that is the hash code of the argument.
It should be the case that if (eq (funcall HASH x1) (funcall HASH x2))
returns nil, then (funcall TEST x1 x2) also returns nil.  */)
  (Lisp_Object name, Lisp_Object test, Lisp_Object hash)
{
  return Fput (name, Qhash_table_test, list2 (test, hash));
}



/************************************************************************
			MD5, SHA-1, and SHA-2
 ************************************************************************/

#include "md5.h"
#include "sha1.h"
#include "sha256.h"
#include "sha512.h"

static Lisp_Object
make_digest_string (Lisp_Object digest, int digest_size)
{
  unsigned char *p = SDATA (digest);

  for (int i = digest_size - 1; i >= 0; i--)
    {
      static char const hexdigit[16] = "0123456789abcdef";
      int p_i = p[i];
      p[2 * i] = hexdigit[p_i >> 4];
      p[2 * i + 1] = hexdigit[p_i & 0xf];
    }
  return digest;
}

DEFUN ("secure-hash-algorithms", Fsecure_hash_algorithms,
       Ssecure_hash_algorithms, 0, 0, 0,
       doc: /* Return a list of all the supported `secure_hash' algorithms. */)
  (void)
{
  return listn (CONSTYPE_HEAP, 6,
                Qmd5,
                Qsha1,
                Qsha224,
                Qsha256,
                Qsha384,
                Qsha512);
}

/* Extract data from a string or a buffer. SPEC is a list of
(BUFFER-OR-STRING-OR-SYMBOL START END CODING-SYSTEM NOERROR) which behave as
specified with `secure-hash' and in Info node
`(elisp)Format of GnuTLS Cryptography Inputs'.  */
char *
extract_data_from_object (Lisp_Object spec,
                          ptrdiff_t *start_byte,
                          ptrdiff_t *end_byte)
{
  Lisp_Object object = XCAR (spec);

  if (CONSP (spec)) spec = XCDR (spec);
  Lisp_Object start = CAR_SAFE (spec);

  if (CONSP (spec)) spec = XCDR (spec);
  Lisp_Object end = CAR_SAFE (spec);

  if (CONSP (spec)) spec = XCDR (spec);
  Lisp_Object coding_system = CAR_SAFE (spec);

  if (CONSP (spec)) spec = XCDR (spec);
  Lisp_Object noerror = CAR_SAFE (spec);

  if (STRINGP (object))
    {
      if (NILP (coding_system))
	{
	  /* Decide the coding-system to encode the data with.  */

	  if (STRING_MULTIBYTE (object))
	    /* use default, we can't guess correct value */
	    coding_system = preferred_coding_system ();
	  else
	    coding_system = Qraw_text;
	}

      if (NILP (Fcoding_system_p (coding_system)))
	{
	  /* Invalid coding system.  */

	  if (!NILP (noerror))
	    coding_system = Qraw_text;
	  else
	    xsignal1 (Qcoding_system_error, coding_system);
	}

      if (STRING_MULTIBYTE (object))
	object = code_convert_string (object, coding_system, Qnil, 1, 0, 1);

      ptrdiff_t size = SCHARS (object), start_char, end_char;
      validate_subarray (object, start, end, size, &start_char, &end_char);

      *start_byte = !start_char ? 0 : string_char_to_byte (object, start_char);
      *end_byte = (end_char == size
                   ? SBYTES (object)
                   : string_char_to_byte (object, end_char));
    }
  else if (BUFFERP (object))
    {
      struct buffer *prev = current_buffer;
      EMACS_INT b, e;

      record_unwind_current_buffer ();

      CHECK_BUFFER (object);

      struct buffer *bp = XBUFFER (object);
      set_buffer_internal (bp);

      if (NILP (start))
	b = BEGV;
      else
	{
	  CHECK_NUMBER_COERCE_MARKER (start);
	  b = XINT (start);
	}

      if (NILP (end))
	e = ZV;
      else
	{
	  CHECK_NUMBER_COERCE_MARKER (end);
	  e = XINT (end);
	}

      if (b > e)
	{
	  EMACS_INT temp = b;
	  b = e;
	  e = temp;
	}

      if (!(BEGV <= b && e <= ZV))
	args_out_of_range (start, end);

      if (NILP (coding_system))
	{
	  /* Decide the coding-system to encode the data with.
	     See fileio.c:Fwrite-region */

	  if (!NILP (Vcoding_system_for_write))
	    coding_system = Vcoding_system_for_write;
	  else
	    {
	      bool force_raw_text = 0;

	      coding_system = BVAR (XBUFFER (object), buffer_file_coding_system);
	      if (NILP (coding_system)
		  || NILP (Flocal_variable_p (Qbuffer_file_coding_system, Qnil)))
		{
		  coding_system = Qnil;
		  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
		    force_raw_text = 1;
		}

	      if (NILP (coding_system) && !NILP (Fbuffer_file_name (object)))
		{
		  /* Check file-coding-system-alist.  */
		  Lisp_Object val = CALLN (Ffind_operation_coding_system,
					   Qwrite_region, start, end,
					   Fbuffer_file_name (object));
		  if (CONSP (val) && !NILP (XCDR (val)))
		    coding_system = XCDR (val);
		}

	      if (NILP (coding_system)
		  && !NILP (BVAR (XBUFFER (object), buffer_file_coding_system)))
		{
		  /* If we still have not decided a coding system, use the
		     default value of buffer-file-coding-system.  */
		  coding_system = BVAR (XBUFFER (object), buffer_file_coding_system);
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
		xsignal1 (Qcoding_system_error, coding_system);
	    }
	}

      object = make_buffer_string (b, e, 0);
      set_buffer_internal (prev);
      /* Discard the unwind protect for recovering the current
	 buffer.  */
      specpdl_ptr--;

      if (STRING_MULTIBYTE (object))
	object = code_convert_string (object, coding_system, Qnil, 1, 0, 0);
      *start_byte = 0;
      *end_byte = SBYTES (object);
    }
  else if (EQ (object, Qiv_auto))
    {
#ifdef HAVE_GNUTLS3
      /* Format: (iv-auto REQUIRED-LENGTH).  */

      if (! NATNUMP (start))
        error ("Without a length, `iv-auto' can't be used; see ELisp manual");
      else
        {
	  EMACS_INT start_hold = XFASTINT (start);
          object = make_uninit_string (start_hold);
          gnutls_rnd (GNUTLS_RND_NONCE, SSDATA (object), start_hold);

          *start_byte = 0;
          *end_byte = start_hold;
        }
#else
      error ("GnuTLS is not available, so `iv-auto' can't be used");
#endif
    }

  return SSDATA (object);
}


/* ALGORITHM is a symbol: md5, sha1, sha224 and so on. */

static Lisp_Object
secure_hash (Lisp_Object algorithm, Lisp_Object object, Lisp_Object start,
	     Lisp_Object end, Lisp_Object coding_system, Lisp_Object noerror,
	     Lisp_Object binary)
{
  ptrdiff_t start_byte, end_byte;
  int digest_size;
  void *(*hash_func) (const char *, size_t, void *);
  Lisp_Object digest;

  CHECK_SYMBOL (algorithm);

  Lisp_Object spec = list5 (object, start, end, coding_system, noerror);

  const char *input = extract_data_from_object (spec, &start_byte, &end_byte);

  if (input == NULL)
    error ("secure_hash: failed to extract data from object, aborting!");

  if (EQ (algorithm, Qmd5))
    {
      digest_size = MD5_DIGEST_SIZE;
      hash_func	  = md5_buffer;
    }
  else if (EQ (algorithm, Qsha1))
    {
      digest_size = SHA1_DIGEST_SIZE;
      hash_func	  = sha1_buffer;
    }
  else if (EQ (algorithm, Qsha224))
    {
      digest_size = SHA224_DIGEST_SIZE;
      hash_func	  = sha224_buffer;
    }
  else if (EQ (algorithm, Qsha256))
    {
      digest_size = SHA256_DIGEST_SIZE;
      hash_func	  = sha256_buffer;
    }
  else if (EQ (algorithm, Qsha384))
    {
      digest_size = SHA384_DIGEST_SIZE;
      hash_func	  = sha384_buffer;
    }
  else if (EQ (algorithm, Qsha512))
    {
      digest_size = SHA512_DIGEST_SIZE;
      hash_func	  = sha512_buffer;
    }
  else
    error ("Invalid algorithm arg: %s", SDATA (Fsymbol_name (algorithm)));

  /* allocate 2 x digest_size so that it can be re-used to hold the
     hexified value */
  digest = make_uninit_string (digest_size * 2);

  hash_func (input + start_byte,
	     end_byte - start_byte,
	     SSDATA (digest));

  if (NILP (binary))
    return make_digest_string (digest, digest_size);
  else
    return make_unibyte_string (SSDATA (digest), digest_size);
}

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
  (Lisp_Object object, Lisp_Object start, Lisp_Object end, Lisp_Object coding_system, Lisp_Object noerror)
{
  return secure_hash (Qmd5, object, start, end, coding_system, noerror, Qnil);
}

DEFUN ("secure-hash", Fsecure_hash, Ssecure_hash, 2, 5, 0,
       doc: /* Return the secure hash of OBJECT, a buffer or string.
ALGORITHM is a symbol specifying the hash to use:
md5, sha1, sha224, sha256, sha384 or sha512.

The two optional arguments START and END are positions specifying for
which part of OBJECT to compute the hash.  If nil or omitted, uses the
whole OBJECT.

The full list of algorithms can be obtained with `secure-hash-algorithms'.

If BINARY is non-nil, returns a string in binary form.  */)
  (Lisp_Object algorithm, Lisp_Object object, Lisp_Object start, Lisp_Object end, Lisp_Object binary)
{
  return secure_hash (algorithm, object, start, end, Qnil, Qnil, binary);
}

DEFUN ("buffer-hash", Fbuffer_hash, Sbuffer_hash, 0, 1, 0,
       doc: /* Return a hash of the contents of BUFFER-OR-NAME.
This hash is performed on the raw internal format of the buffer,
disregarding any coding systems.  If nil, use the current buffer.  */ )
  (Lisp_Object buffer_or_name)
{
  Lisp_Object buffer;
  struct buffer *b;
  struct sha1_ctx ctx;

  if (NILP (buffer_or_name))
    buffer = Fcurrent_buffer ();
  else
    buffer = Fget_buffer (buffer_or_name);
  if (NILP (buffer))
    nsberror (buffer_or_name);

  b = XBUFFER (buffer);
  sha1_init_ctx (&ctx);

  /* Process the first part of the buffer. */
  sha1_process_bytes (BUF_BEG_ADDR (b),
		      BUF_GPT_BYTE (b) - BUF_BEG_BYTE (b),
		      &ctx);

  /* If the gap is before the end of the buffer, process the last half
     of the buffer. */
  if (BUF_GPT_BYTE (b) < BUF_Z_BYTE (b))
    sha1_process_bytes (BUF_GAP_END_ADDR (b),
			BUF_Z_ADDR (b) - BUF_GAP_END_ADDR (b),
			&ctx);

  Lisp_Object digest = make_uninit_string (SHA1_DIGEST_SIZE * 2);
  sha1_finish_ctx (&ctx, SSDATA (digest));
  return make_digest_string (digest, SHA1_DIGEST_SIZE);
}


void
syms_of_fns (void)
{
  /* Hash table stuff.  */
  DEFSYM (Qhash_table_p, "hash-table-p");
  DEFSYM (Qeq, "eq");
  DEFSYM (Qeql, "eql");
  DEFSYM (Qequal, "equal");
  DEFSYM (QCtest, ":test");
  DEFSYM (QCsize, ":size");
  DEFSYM (QCpurecopy, ":purecopy");
  DEFSYM (QCrehash_size, ":rehash-size");
  DEFSYM (QCrehash_threshold, ":rehash-threshold");
  DEFSYM (QCweakness, ":weakness");
  DEFSYM (Qkey, "key");
  DEFSYM (Qvalue, "value");
  DEFSYM (Qhash_table_test, "hash-table-test");
  DEFSYM (Qkey_or_value, "key-or-value");
  DEFSYM (Qkey_and_value, "key-and-value");

  defsubr (&Ssxhash_eq);
  defsubr (&Ssxhash_eql);
  defsubr (&Ssxhash_equal);
  defsubr (&Smake_hash_table);
  defsubr (&Scopy_hash_table);
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

  /* Crypto and hashing stuff.  */
  DEFSYM (Qiv_auto, "iv-auto");

  DEFSYM (Qmd5,    "md5");
  DEFSYM (Qsha1,   "sha1");
  DEFSYM (Qsha224, "sha224");
  DEFSYM (Qsha256, "sha256");
  DEFSYM (Qsha384, "sha384");
  DEFSYM (Qsha512, "sha512");

  /* Miscellaneous stuff.  */

  DEFSYM (Qstring_lessp, "string-lessp");
  DEFSYM (Qprovide, "provide");
  DEFSYM (Qrequire, "require");
  DEFSYM (Qyes_or_no_p_history, "yes-or-no-p-history");
  DEFSYM (Qcursor_in_echo_area, "cursor-in-echo-area");
  DEFSYM (Qwidget_type, "widget-type");

  DEFVAR_LISP ("overriding-plist-environment", Voverriding_plist_environment,
               doc: /* An alist that overrides the plists of the symbols which it lists.
Used by the byte-compiler to apply `define-symbol-prop' during
compilation.  */);
  Voverriding_plist_environment = Qnil;
  DEFSYM (Qoverriding_plist_environment, "overriding-plist-environment");

  staticpro (&string_char_byte_cache_string);
  string_char_byte_cache_string = Qnil;

  require_nesting_list = Qnil;
  staticpro (&require_nesting_list);

  Fset (Qyes_or_no_p_history, Qnil);

  DEFVAR_LISP ("features", Vfeatures,
    doc: /* A list of symbols which are the features of the executing Emacs.
Used by `featurep' and `require', and altered by `provide'.  */);
  Vfeatures = list1 (Qemacs);
  DEFSYM (Qfeatures, "features");
  /* Let people use lexically scoped vars named `features'.  */
  Fmake_var_non_special (Qfeatures);
  DEFSYM (Qsubfeatures, "subfeatures");
  DEFSYM (Qfuncall, "funcall");
  DEFSYM (Qplistp, "plistp");

#ifdef HAVE_LANGINFO_CODESET
  DEFSYM (Qcodeset, "codeset");
  DEFSYM (Qdays, "days");
  DEFSYM (Qmonths, "months");
  DEFSYM (Qpaper, "paper");
#endif	/* HAVE_LANGINFO_CODESET */

  DEFVAR_BOOL ("use-dialog-box", use_dialog_box,
    doc: /* Non-nil means mouse commands use dialog boxes to ask questions.
This applies to `y-or-n-p' and `yes-or-no-p' questions asked by commands
invoked by mouse clicks and mouse menu items.

On some platforms, file selection dialogs are also enabled if this is
non-nil.  */);
  use_dialog_box = 1;

  DEFVAR_BOOL ("use-file-dialog", use_file_dialog,
    doc: /* Non-nil means mouse commands use a file dialog to ask for files.
This applies to commands from menus and tool bar buttons even when
they are initiated from the keyboard.  If `use-dialog-box' is nil,
that disables the use of a file dialog, regardless of the value of
this variable.  */);
  use_file_dialog = 1;

  defsubr (&Sidentity);
  defsubr (&Srandom);
  defsubr (&Slength);
  defsubr (&Ssafe_length);
  defsubr (&Sstring_bytes);
  defsubr (&Sstring_equal);
  defsubr (&Scompare_strings);
  defsubr (&Sstring_lessp);
  defsubr (&Sstring_version_lessp);
  defsubr (&Sstring_collate_lessp);
  defsubr (&Sstring_collate_equalp);
  defsubr (&Sappend);
  defsubr (&Sconcat);
  defsubr (&Svconcat);
  defsubr (&Scopy_sequence);
  defsubr (&Sstring_make_multibyte);
  defsubr (&Sstring_make_unibyte);
  defsubr (&Sstring_as_multibyte);
  defsubr (&Sstring_as_unibyte);
  defsubr (&Sstring_to_multibyte);
  defsubr (&Sstring_to_unibyte);
  defsubr (&Scopy_alist);
  defsubr (&Ssubstring);
  defsubr (&Ssubstring_no_properties);
  defsubr (&Snthcdr);
  defsubr (&Snth);
  defsubr (&Selt);
  defsubr (&Smember);
  defsubr (&Smemq);
  defsubr (&Smemql);
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
  defsubr (&Seql);
  defsubr (&Sequal);
  defsubr (&Sequal_including_properties);
  defsubr (&Sfillarray);
  defsubr (&Sclear_string);
  defsubr (&Snconc);
  defsubr (&Smapcar);
  defsubr (&Smapc);
  defsubr (&Smapcan);
  defsubr (&Smapconcat);
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
  defsubr (&Ssecure_hash_algorithms);
  defsubr (&Ssecure_hash);
  defsubr (&Sbuffer_hash);
  defsubr (&Slocale_info);
}
