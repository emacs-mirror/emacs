/* Basic character set support.
   Copyright (C) 1995, 97, 98, 2000, 2001 Electrotechnical Laboratory, JAPAN.
   Licensed to the Free Software Foundation.
   Copyright (C) 2001 Free Software Foundation, Inc.
   Copyright (C) 2001, 2002
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H13PRO009

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

#ifdef emacs
#include <config.h>
#endif

#include <stdio.h>
#include <unistd.h>
#include <ctype.h>

#ifdef emacs

#include <sys/types.h>
#include "lisp.h"
#include "character.h"
#include "charset.h"
#include "coding.h"
#include "disptab.h"
#include "buffer.h"

#else  /* not emacs */

#include "mulelib.h"

#endif /* emacs */


/*** GENERAL NOTES on CODED CHARACTER SETS (CHARSETS) ***

  A coded character set ("charset" hereafter) is a meaningful
  collection (i.e. language, culture, functionality, etc.) of
  characters.  Emacs handles multiple charsets at once.  In Emacs Lisp
  code, a charset is represented by a symbol.  In C code, a charset is
  represented by its ID number or by a pointer to a struct charset.

  The actual information about each charset is stored in two places.
  Lispy information is stored in the hash table Vcharset_hash_table as
  a vector (charset attributes).  The other information is stored in
  charset_table as a struct charset.

*/

/* List of all charsets.  This variable is used only from Emacs
   Lisp.  */
Lisp_Object Vcharset_list;

/* Hash table that contains attributes of each charset.  Keys are
   charset symbols, and values are vectors of charset attributes.  */
Lisp_Object Vcharset_hash_table;

/* Table of struct charset.  */
struct charset *charset_table;

static int charset_table_size;
int charset_table_used;

Lisp_Object Qcharsetp;

/* Special charset symbols.  */
Lisp_Object Qascii;
Lisp_Object Qeight_bit_control;
Lisp_Object Qeight_bit_graphic;
Lisp_Object Qiso_8859_1;
Lisp_Object Qunicode;

/* The corresponding charsets.  */
int charset_ascii;
int charset_8_bit_control;
int charset_8_bit_graphic;
int charset_iso_8859_1;
int charset_unicode;

/* Value of charset attribute `charset-iso-plane'.  */
Lisp_Object Qgl, Qgr;

/* The primary charset.  It is a charset of unibyte characters.  */
int charset_primary;

/* List of charsets ordered by the priority.  */
Lisp_Object Vcharset_ordered_list;

/* List of iso-2022 charsets.  */
Lisp_Object Viso_2022_charset_list;

/* List of emacs-mule charsets.  */
Lisp_Object Vemacs_mule_charset_list;

struct charset *emacs_mule_charset[256];

/* Mapping table from ISO2022's charset (specified by DIMENSION,
   CHARS, and FINAL-CHAR) to Emacs' charset.  */
int iso_charset_table[ISO_MAX_DIMENSION][ISO_MAX_CHARS][ISO_MAX_FINAL];

Lisp_Object Vcharset_map_directory;

Lisp_Object Vchar_unified_charset_table;

#define CODE_POINT_TO_INDEX(charset, code)				\
  ((charset)->code_linear_p						\
   ? (code) - (charset)->min_code					\
   : (((charset)->code_space_mask[(code) >> 24] & 0x8)			\
      && ((charset)->code_space_mask[((code) >> 16) & 0xFF] & 0x4)	\
      && ((charset)->code_space_mask[((code) >> 8) & 0xFF] & 0x2)	\
      && ((charset)->code_space_mask[(code) & 0xFF] & 0x1))		\
   ? (((((code) >> 24) - (charset)->code_space[12])			\
       * (charset)->code_space[11])					\
      + (((((code) >> 16) & 0xFF) - (charset)->code_space[8])		\
	 * (charset)->code_space[7])					\
      + (((((code) >> 8) & 0xFF) - (charset)->code_space[4])		\
	 * (charset)->code_space[3])					\
      + (((code) & 0xFF) - (charset)->code_space[0]))			\
   : -1)


/* Convert the character index IDX to code-point CODE for CHARSET.
   It is assumed that IDX is in a valid range.  */

#define INDEX_TO_CODE_POINT(charset, idx)				   \
  ((charset)->code_linear_p						   \
   ? (idx) + (charset)->min_code					   \
   : (((charset)->code_space[0] + (idx) % (charset)->code_space[2])	   \
      | (((charset)->code_space[4]					   \
	  + ((idx) / (charset)->code_space[3] % (charset)->code_space[6])) \
	 << 8)								   \
      | (((charset)->code_space[8]					   \
	  + ((idx) / (charset)->code_space[7] % (charset)->code_space[10])) \
	 << 16)								   \
      | (((charset)->code_space[12] + ((idx) / (charset)->code_space[11])) \
	 << 24)))



/* Set to 1 to warn that a charset map is loaded and thus a buffer
   text and a string data may be relocated.  */
int charset_map_loaded;

struct charset_map_entries
{
  struct {
    unsigned from, to;
    int c;
  } entry[0x10000];
  struct charset_map_entries *next;
};

/* Load the mapping information for CHARSET from ENTRIES.

   If CONTROL_FLAG is 0, setup CHARSET->min_char and CHARSET->max_char.

   If CONTROL_FLAG is 1, setup CHARSET->min_char, CHARSET->max_char,
   CHARSET->decoder, and CHARSET->encoder.

   If CONTROL_FLAG is 2, setup CHARSET->deunifier and
   Vchar_unify_table.  If Vchar_unified_charset_table is non-nil,
   setup it too.  */

static void
load_charset_map (charset, entries, n_entries, control_flag)
  struct charset *charset;
  struct charset_map_entries *entries;
  int n_entries;
  int control_flag;
{
  Lisp_Object vec, table;
  unsigned max_code = CHARSET_MAX_CODE (charset);
  int ascii_compatible_p = charset->ascii_compatible_p;
  int min_char, max_char, nonascii_min_char;
  int i;
  unsigned char *fast_map = charset->fast_map;

  if (n_entries <= 0)
    return;

  if (control_flag > 0)
    {
      int n = CODE_POINT_TO_INDEX (charset, max_code) + 1;
      unsigned invalid_code = CHARSET_INVALID_CODE (charset);

      table = Fmake_char_table (Qnil, make_number (invalid_code));
      if (control_flag == 1)
	vec = Fmake_vector (make_number (n), make_number (-1));
      else if (! CHAR_TABLE_P (Vchar_unify_table))
	Vchar_unify_table = Fmake_char_table (Qnil, make_number (-1));

      charset_map_loaded = 1;
    }

  min_char = max_char = entries->entry[0].c;
  nonascii_min_char = MAX_CHAR;
  for (i = 0; i < n_entries; i++)
    {
      unsigned from, to;
      int c;
      int idx = i % 0x10000;

      if (i > 0 && idx == 0)
	entries = entries->next;
      from = entries->entry[idx].from;
      to = entries->entry[idx].to;
      c = entries->entry[idx].c;

      if (control_flag < 2)
	{
	  if (control_flag == 1)
	    {
	      unsigned code = from;
	      int from_index, to_index;

	      from_index = CODE_POINT_TO_INDEX (charset, from);
	      if (from == to)
		to_index = from_index;
	      else
		to_index = CODE_POINT_TO_INDEX (charset, to);
	      if (from_index < 0 || to_index < 0)
		continue;
	      if (CHARSET_COMPACT_CODES_P (charset))
		while (1)
		  {
		    ASET (vec, from_index, make_number (c));
		    CHAR_TABLE_SET (table, c, make_number (code));
		    if (from_index == to_index)
		      break;
		    from_index++, c++;
		    code = INDEX_TO_CODE_POINT (charset, from_index);
		  }
	      else
		for (; from_index <= to_index; from_index++, c++)
		  {
		    ASET (vec, from_index, make_number (c));
		    CHAR_TABLE_SET (table, c, make_number (from_index));
		  }
	    }

	  if (c > max_char)
	    max_char = c;
	  else if (c < min_char)
	    min_char = c;
	  if (ascii_compatible_p && ! ASCII_BYTE_P (c)
	      && c < nonascii_min_char)
	    nonascii_min_char = c;

	  CHARSET_FAST_MAP_SET (c, fast_map);
	}
      else
	{
	  unsigned code = from;
	  int from_index, to_index;

	  from_index = CODE_POINT_TO_INDEX (charset, from);
	  if (from == to)
	    to_index = from_index;
	  else
	    to_index = CODE_POINT_TO_INDEX (charset, to);
	  if (from_index < 0 || to_index < 0)
	    continue;
	  while (1)
	    {
	      int c1 = DECODE_CHAR (charset, code);
	      
	      if (c1 >= 0)
		{
		  CHAR_TABLE_SET (table, c, make_number (c1));
		  CHAR_TABLE_SET (Vchar_unify_table, c1, c);
		  if (CHAR_TABLE_P (Vchar_unified_charset_table))
		    CHAR_TABLE_SET (Vchar_unified_charset_table, c1,
				    CHARSET_NAME (charset));
		}
	      if (from_index == to_index)
		break;
	      from_index++, c++;
	      code = INDEX_TO_CODE_POINT (charset, from_index);
	    }
	}
    }

  if (control_flag < 2)
    {
      CHARSET_MIN_CHAR (charset) = (ascii_compatible_p
				    ? nonascii_min_char : min_char);
      CHARSET_MAX_CHAR (charset) = max_char;
      if (control_flag == 1)
	{
	  CHARSET_DECODER (charset) = vec;
	  CHARSET_ENCODER (charset) = table;
	}
    }
  else
    CHARSET_DEUNIFIER (charset) = table;  
}


/* Read a hexadecimal number (preceded by "0x") from the file FP while
   paying attention to comment charcter '#'.  */

static INLINE unsigned
read_hex (fp, eof)
     FILE *fp;
     int *eof;
{
  int c;
  unsigned n;

  while ((c = getc (fp)) != EOF)
    {
      if (c == '#')
	{
	  while ((c = getc (fp)) != EOF && c != '\n');
	}
      else if (c == '0')
	{
	  if ((c = getc (fp)) == EOF || c == 'x')
	    break;
	}
    }	    
  if (c == EOF)
    {
      *eof = 1;
      return 0;
    }
  *eof = 0;
  n = 0;
  if (c == 'x')
    while ((c = getc (fp)) != EOF && isxdigit (c))
      n = ((n << 4)
	   | (c <= '9' ? c - '0' : c <= 'F' ? c - 'A' + 10 : c - 'a' + 10));
  else
    while ((c = getc (fp)) != EOF && isdigit (c))
      n = (n * 10) + c - '0';
  if (c != EOF)
    ungetc (c, fp);
  return n;
}


/* Return a mapping vector for CHARSET loaded from MAPFILE.
   Each line of MAPFILE has this form
	0xAAAA 0xCCCC
   where 0xAAAA is a code-point and 0xCCCC is the corresponding
   character code, or this form
	0xAAAA-0xBBBB 0xCCCC
   where 0xAAAA and 0xBBBB are code-points specifying a range, and
   0xCCCC is the first character code of the range.

   The returned vector has this form:
	[ CODE1 CHAR1 CODE2 CHAR2 .... ]
   where CODE1 is a code-point or a cons of code-points specifying a
   range.  */

extern void add_to_log P_ ((char *, Lisp_Object, Lisp_Object));

static void
load_charset_map_from_file (charset, mapfile, control_flag)
     struct charset *charset;
     Lisp_Object mapfile;
     int control_flag;
{
  unsigned min_code = CHARSET_MIN_CODE (charset);
  unsigned max_code = CHARSET_MAX_CODE (charset);
  int fd;
  FILE *fp;
  int eof;
  Lisp_Object suffixes;
  struct charset_map_entries *head, *entries;
  int n_entries;

  suffixes = Fcons (build_string (".map"),
		    Fcons (build_string (".TXT"), Qnil));

  fd = openp (Fcons (Vcharset_map_directory, Qnil), mapfile, suffixes,
	      NULL, 0);
  if (fd < 0
      || ! (fp = fdopen (fd, "r")))
    {
      add_to_log ("Failure in loading charset map: %S", mapfile, Qnil);
      return;
    }

  head = entries = ((struct charset_map_entries *)
		    alloca (sizeof (struct charset_map_entries)));
  n_entries = 0;
  eof = 0;
  while (1)
    {
      unsigned from, to;
      int c;
      int idx;

      from = read_hex (fp, &eof);
      if (eof)
	break;
      if (getc (fp) == '-')
	to = read_hex (fp, &eof);
      else
	to = from;
      c = (int) read_hex (fp, &eof);

      if (from < min_code || to > max_code || from > to || c > MAX_CHAR)
	continue;

      if (n_entries > 0 && (n_entries % 0x10000) == 0)
	{
	  entries->next = ((struct charset_map_entries *)
			   alloca (sizeof (struct charset_map_entries)));
	  entries = entries->next;
	}
      idx = n_entries % 0x10000;
      entries->entry[idx].from = from;
      entries->entry[idx].to = to;
      entries->entry[idx].c = c;
      n_entries++;
    }
  fclose (fp);
  close (fd);

  load_charset_map (charset, head, n_entries, control_flag);
}

static void
load_charset_map_from_vector (charset, vec, control_flag)
     struct charset *charset;
     Lisp_Object vec;
     int control_flag;
{
  unsigned min_code = CHARSET_MIN_CODE (charset);
  unsigned max_code = CHARSET_MAX_CODE (charset);
  struct charset_map_entries *head, *entries;
  int n_entries;
  int len = ASIZE (vec);
  int i;

  if (len % 2 == 1)
    {
      add_to_log ("Failure in loading charset map: %V", vec, Qnil);
      return;
    }

  head = entries = ((struct charset_map_entries *)
		    alloca (sizeof (struct charset_map_entries)));
  n_entries = 0;
  for (i = 0; i < len; i += 2)
    {
      Lisp_Object val, val2;
      unsigned from, to;
      int c;
      int idx;

      val = AREF (vec, i);
      if (CONSP (val))
	{
	  val2 = XCDR (val);
	  val = XCAR (val);
	  CHECK_NATNUM (val);
	  CHECK_NATNUM (val2);
	  from = XFASTINT (val);
	  to = XFASTINT (val2);
	}
      else
	{
	  CHECK_NATNUM (val);
	  from = to = XFASTINT (val);
	}
      val = AREF (vec, i + 1);
      CHECK_NATNUM (val);
      c = XFASTINT (val);

      if (from < min_code || to > max_code || from > to || c > MAX_CHAR)
	continue;

      if ((n_entries % 0x10000) == 0)
	{
	  entries->next = ((struct charset_map_entries *)
			   alloca (sizeof (struct charset_map_entries)));
	  entries = entries->next;
	}
      idx = n_entries % 0x10000;
      entries->entry[idx].from = from;
      entries->entry[idx].to = to;
      entries->entry[idx].c = c;
      n_entries++;
    }

  load_charset_map (charset, head, n_entries, control_flag);
}

static void
load_charset (charset)
     struct charset *charset;
{
  if (CHARSET_METHOD (charset) == CHARSET_METHOD_MAP_DEFERRED)
    {
      Lisp_Object map;

      map = CHARSET_MAP (charset);
      if (STRINGP (map))
	load_charset_map_from_file (charset, map, 1);
      else
	load_charset_map_from_vector (charset, map, 1);
      CHARSET_METHOD (charset) = CHARSET_METHOD_MAP;
    }
}


DEFUN ("charsetp", Fcharsetp, Scharsetp, 1, 1, 0,
       doc: /* Return non-nil if and only if OBJECT is a charset.*/)
     (object)
     Lisp_Object object;
{
  return (CHARSETP (object) ? Qt : Qnil);
}


void
map_charset_chars (c_function, function, charset_symbol, arg)
     void (*c_function) (Lisp_Object, Lisp_Object, Lisp_Object);
     Lisp_Object function, charset_symbol, arg;
{
  int id;
  struct charset *charset;
  Lisp_Object range;

  CHECK_CHARSET_GET_ID (charset_symbol, id);
  charset = CHARSET_FROM_ID (id);

  if (CHARSET_METHOD (charset) == CHARSET_METHOD_MAP_DEFERRED)  
    load_charset (charset);

  if (CHARSET_METHOD (charset) == CHARSET_METHOD_OFFSET)
    {
      range = Fcons (make_number (CHARSET_MIN_CHAR (charset)),
		     make_number (CHARSET_MAX_CHAR (charset)));
      if (NILP (function))
	(*c_function) (arg, range, Qnil);
      else
	call2 (function, range, arg);
    }
  else if (CHARSET_METHOD (charset) == CHARSET_METHOD_MAP)
    {
      if (! CHAR_TABLE_P (CHARSET_ENCODER (charset)))
	return;
      if (CHARSET_ASCII_COMPATIBLE_P (charset))
	{
	  range = Fcons (make_number (0), make_number (127));
	  if (NILP (function))
	    (*c_function) (arg, range, Qnil);
	  else
	    call2 (function, range, arg);
	}
      map_char_table (c_function, function, CHARSET_ENCODER (charset), arg,
		      0, NULL);
    }
  else				/* i.e. CHARSET_METHOD_PARENT */
    {
      int from, to, c;
      unsigned code;
      int i, j, k, l;
      int *code_space = CHARSET_CODE_SPACE (charset);
      Lisp_Object val;

      range = Fcons (Qnil, Qnil);
      from = to = -2;
      for (i = code_space[12]; i <= code_space[13]; i++)
	for (j = code_space[8]; j <= code_space[9]; j++)
	  for (k = code_space[4]; k <= code_space[5]; k++)
	    for (l = code_space[0]; l <= code_space[1]; l++)
	      {
		code = (i << 24) | (j << 16) | (k << 8) | l;
		c = DECODE_CHAR (charset, code);
		if (c == to + 1)
		  {
		    to++;
		    continue;
		  }
		if (from >= 0)
		  {
		    if (from < to)
		      {
			XSETCAR (range, make_number (from));
			XSETCDR (range, make_number (to));
			val = range;
		      }
		    else
		      val = make_number (from);
		    if (NILP (function))
		      (*c_function) (arg, val, Qnil);
		    else
		      call2 (function, val, arg);
		  }
		from = to = (c < 0 ? -2 : c);
	      }
      if (from >= 0)
	{
	  if (from < to)
	    {
	      XSETCAR (range, make_number (from));
	      XSETCDR (range, make_number (to));
	      val = range;
	    }
	  else
	    val = make_number (from);
	  if (NILP (function))
	    (*c_function) (arg, val, Qnil);
	  else
	    call2 (function, val, arg);
	}
    }
}
  
DEFUN ("map-charset-chars", Fmap_charset_chars, Smap_charset_chars, 2, 3, 0,
       doc: /* Call FUNCTION for all characters in CHARSET.
FUNCTION is called with an argument RANGE and optional 2nd
argument ARG.

RANGE is either a cons (FROM .  TO), where FROM and TO indicate a range of
characters contained in CHARSET or a single character in the case that
FROM and TO would be equal.  (The charset mapping may have gaps.)*/)
     (function, charset, arg)
       Lisp_Object function, charset, arg;
{
  map_charset_chars (NULL, function, charset, arg);
  return Qnil;
}


/* Define a charset according to the arguments.  The Nth argument is
   the Nth attribute of the charset (the last attribute `charset-id'
   is not included).  See the docstring of `define-charset' for the
   detail.  */

DEFUN ("define-charset-internal", Fdefine_charset_internal,
       Sdefine_charset_internal, charset_arg_max, MANY, 0,
       doc: /* For internal use only.
usage: (define-charset-internal ...)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  /* Charset attr vector.  */
  Lisp_Object attrs;
  Lisp_Object val;
  unsigned hash_code;
  struct Lisp_Hash_Table *hash_table = XHASH_TABLE (Vcharset_hash_table);
  int i, j;
  struct charset charset;
  int id;
  int dimension;
  int new_definition_p;
  int nchars;

  if (nargs != charset_arg_max)
    return Fsignal (Qwrong_number_of_arguments,
		    Fcons (intern ("define-charset-internal"),
			   make_number (nargs)));

  attrs = Fmake_vector (make_number (charset_attr_max), Qnil);

  CHECK_SYMBOL (args[charset_arg_name]);
  ASET (attrs, charset_name, args[charset_arg_name]);

  val = args[charset_arg_code_space];
  for (i = 0, dimension = 0, nchars = 1; i < 4; i++)
    {
      int min_byte, max_byte;

      min_byte = XINT (Faref (val, make_number (i * 2)));
      max_byte = XINT (Faref (val, make_number (i * 2 + 1)));
      if (min_byte < 0 || min_byte > max_byte || max_byte >= 256)
	error ("Invalid :code-space value");
      charset.code_space[i * 4] = min_byte;
      charset.code_space[i * 4 + 1] = max_byte;
      charset.code_space[i * 4 + 2] = max_byte - min_byte + 1;
      nchars *= charset.code_space[i * 4 + 2];
      charset.code_space[i * 4 + 3] = nchars;
      if (max_byte > 0)
	dimension = i + 1;
    }

  val = args[charset_arg_dimension];
  if (NILP (val))
    charset.dimension = dimension;
  else
    {
      CHECK_NATNUM (val);
      charset.dimension = XINT (val);
      if (charset.dimension < 1 || charset.dimension > 4)
	args_out_of_range_3 (val, make_number (1), make_number (4));
    }

  charset.code_linear_p
    = (charset.dimension == 1
       || (charset.code_space[2] == 256
	   && (charset.dimension == 2
	       || (charset.code_space[6] == 256
		   && (charset.dimension == 3
		       || charset.code_space[10] == 256)))));

  if (! charset.code_linear_p)
    {
      charset.code_space_mask = (unsigned char *) xmalloc (256);
      bzero (charset.code_space_mask, 256);
      for (i = 0; i < 4; i++)
	for (j = charset.code_space[i * 4]; j <= charset.code_space[i * 4 + 1];
	     j++)
	  charset.code_space_mask[j] |= (1 << i);
    }

  charset.iso_chars_96 = charset.code_space[2] == 96;

  charset.min_code = (charset.code_space[0]
		      | (charset.code_space[4] << 8)
		      | (charset.code_space[8] << 16)
		      | (charset.code_space[12] << 24));
  charset.max_code = (charset.code_space[1]
		      | (charset.code_space[5] << 8)
		      | (charset.code_space[9] << 16)
		      | (charset.code_space[13] << 24));

  charset.compact_codes_p = charset.max_code < 0x1000000;

  val = args[charset_arg_invalid_code];
  if (NILP (val))
    {
      if (charset.min_code > 0)
	charset.invalid_code = 0;
      else
	{
	  XSETINT (val, charset.max_code + 1);
	  if (XINT (val) == charset.max_code + 1)
	    charset.invalid_code = charset.max_code + 1;
	  else
	    error ("Attribute :invalid-code must be specified");
	}
    }
  else
    {
      CHECK_NATNUM (val);
      charset.invalid_code = XFASTINT (val);
    }

  val = args[charset_arg_iso_final];
  if (NILP (val))
    charset.iso_final = -1;
  else
    {
      CHECK_NUMBER (val);
      if (XINT (val) < '0' || XINT (val) > 127)
	error ("Invalid iso-final-char: %d", XINT (val));
      charset.iso_final = XINT (val);
    }
    
  val = args[charset_arg_iso_revision];
  if (NILP (val))
    charset.iso_revision = -1;
  else
    {
      CHECK_NUMBER (val);
      if (XINT (val) > 63)
	args_out_of_range (make_number (63), val);
      charset.iso_revision = XINT (val);
    }

  val = args[charset_arg_emacs_mule_id];
  if (NILP (val))
    charset.emacs_mule_id = -1;
  else
    {
      CHECK_NATNUM (val);
      if ((XINT (val) > 0 && XINT (val) <= 128) || XINT (val) >= 256)
	error ("Invalid emacs-mule-id: %d", XINT (val));
      charset.emacs_mule_id = XINT (val);
    }

  charset.ascii_compatible_p = ! NILP (args[charset_arg_ascii_compatible_p]);

  charset.supplementary_p = ! NILP (args[charset_arg_supplementary_p]);

  charset.unified_p = 0;

  bzero (charset.fast_map, sizeof (charset.fast_map));

  if (! NILP (args[charset_arg_code_offset]))
    {
      val = args[charset_arg_code_offset];
      CHECK_NUMBER (val);

      charset.method = CHARSET_METHOD_OFFSET;
      charset.code_offset = XINT (val);

      i = CODE_POINT_TO_INDEX (&charset, charset.min_code);
      charset.min_char = i + charset.code_offset;
      i = CODE_POINT_TO_INDEX (&charset, charset.max_code);
      charset.max_char = i + charset.code_offset;
      if (charset.max_char > MAX_CHAR)
	error ("Unsupported max char: %d", charset.max_char);

      for (i = charset.min_char; i < 0x10000 && i <= charset.max_char;
	   i += 128)
	CHARSET_FAST_MAP_SET (i, charset.fast_map);
      for (; i <= charset.max_char; i += 0x1000)
	CHARSET_FAST_MAP_SET (i, charset.fast_map);
    }
  else if (! NILP (args[charset_arg_map]))
    {
      val = args[charset_arg_map];
      ASET (attrs, charset_map, val);
      if (STRINGP (val))
	load_charset_map_from_file (&charset, val, 0);
      else
	load_charset_map_from_vector (&charset, val, 0);
      charset.method = CHARSET_METHOD_MAP_DEFERRED;
    }
  else if (! NILP (args[charset_arg_parents]))
    {
      val = args[charset_arg_parents];
      CHECK_LIST (val);
      charset.method = CHARSET_METHOD_INHERIT;
      val = Fcopy_sequence (val);
      ASET (attrs, charset_parents, val);

      charset.min_char = MAX_CHAR;
      charset.max_char = 0;
      for (; ! NILP (val); val = Fcdr (val))
	{
	  Lisp_Object elt, car_part, cdr_part;
	  int this_id, offset;
	  struct charset *this_charset;

	  elt = Fcar (val);
	  if (CONSP (elt))
	    {
	      car_part = XCAR (elt);
	      cdr_part = XCDR (elt);
	      CHECK_CHARSET_GET_ID (car_part, this_id);
	      CHECK_NUMBER (cdr_part);
	      offset = XINT (cdr_part);
	    }
	  else
	    {
	      CHECK_CHARSET_GET_ID (elt, this_id);
	      offset = 0;
	    }
	  XSETCAR (val, Fcons (make_number (this_id), make_number (offset)));

	  this_charset = CHARSET_FROM_ID (this_id);
	  if (charset.min_char > this_charset->min_char)
	    charset.min_char = this_charset->min_char;
	  if (charset.max_char < this_charset->max_char)
	    charset.max_char = this_charset->max_char;
	  for (i = 0; i < 190; i++)
	    charset.fast_map[i] |= this_charset->fast_map[i];
	}
    }
  else
    error ("None of :code-offset, :map, :parents are specified");

  val = args[charset_arg_unify_map];
  if (! NILP (val) && !STRINGP (val))
    CHECK_VECTOR (val);
  ASET (attrs, charset_unify_map, val);

  CHECK_LIST (args[charset_arg_plist]);
  ASET (attrs, charset_plist, args[charset_arg_plist]);

  charset.hash_index = hash_lookup (hash_table, args[charset_arg_name],
				    &hash_code);
  if (charset.hash_index >= 0)
    {
      new_definition_p = 0;
      id = XFASTINT (CHARSET_SYMBOL_ID (args[charset_arg_name]));
      HASH_VALUE (hash_table, charset.hash_index) = attrs;
    }
  else
    {
      charset.hash_index = hash_put (hash_table, args[charset_arg_name], attrs,
				     hash_code);
      if (charset_table_used == charset_table_size)
	{
	  charset_table_size += 256;
	  charset_table
	    = ((struct charset *)
	       xrealloc (charset_table,
			 sizeof (struct charset) * charset_table_size));
	}
      id = charset_table_used++;
      new_definition_p = 1;
    }

  ASET (attrs, charset_id, make_number (id));
  charset.id = id;
  charset_table[id] = charset;

  if (charset.iso_final >= 0)
    {
      ISO_CHARSET_TABLE (charset.dimension, charset.iso_chars_96,
			 charset.iso_final) = id;
      if (new_definition_p)
	Viso_2022_charset_list = nconc2 (Viso_2022_charset_list,
					 Fcons (make_number (id), Qnil));
    }
	
  if (charset.emacs_mule_id >= 0)
    {
      emacs_mule_charset[charset.emacs_mule_id] = CHARSET_FROM_ID (id);
      if (charset.emacs_mule_id < 0xA0)
	emacs_mule_bytes[charset.emacs_mule_id] = charset.dimension + 1;
      if (new_definition_p)
	Vemacs_mule_charset_list = nconc2 (Vemacs_mule_charset_list,
					   Fcons (make_number (id), Qnil));
    }

  if (new_definition_p)
    {
      Vcharset_list = Fcons (args[charset_arg_name], Vcharset_list);
      Vcharset_ordered_list = nconc2 (Vcharset_ordered_list, 
				      Fcons (make_number (id), Qnil));
    }

  return Qnil;
}


DEFUN ("define-charset-alias", Fdefine_charset_alias,
       Sdefine_charset_alias, 2, 2, 0,
       doc: /* Define ALIAS as an alias for charset CHARSET.  */)
     (alias, charset)
     Lisp_Object alias, charset;
{
  Lisp_Object attr;

  CHECK_CHARSET_GET_ATTR (charset, attr);
  Fputhash (alias, attr, Vcharset_hash_table);
  return Qnil;
}


DEFUN ("primary-charset", Fprimary_charset, Sprimary_charset, 0, 0, 0,
       doc: /* Return the primary charset.  */)
     ()
{
  return CHARSET_NAME (CHARSET_FROM_ID (charset_primary));
}


DEFUN ("set-primary-charset", Fset_primary_charset, Sset_primary_charset,
       1, 1, 0,
       doc: /* Set the primary charset to CHARSET.  */)
     (charset)
     Lisp_Object charset;
{
  int id;

  CHECK_CHARSET_GET_ID (charset, id);
  charset_primary = id;
  return Qnil;
}


DEFUN ("charset-plist", Fcharset_plist, Scharset_plist, 1, 1, 0,
       doc: /* Return a property list of CHARSET.  */)
     (charset)
     Lisp_Object charset;
{
  Lisp_Object attrs;

  CHECK_CHARSET_GET_ATTR (charset, attrs);
  return CHARSET_ATTR_PLIST (attrs);
}


DEFUN ("set-charset-plist", Fset_charset_plist, Sset_charset_plist, 2, 2, 0,
       doc: /* Set CHARSET's property list to PLIST.  */)
     (charset, plist)
     Lisp_Object charset, plist;
{
  Lisp_Object attrs;

  CHECK_CHARSET_GET_ATTR (charset, attrs);
  CHARSET_ATTR_PLIST (attrs) = plist;
  return plist;
}


DEFUN ("unify-charset", Funify_charset, Sunify_charset, 1, 2, 0,
       doc: /* Unify characters of CHARSET with Unicode.   */)
     (charset, unify_map)
     Lisp_Object charset, unify_map;
{
  int id;
  struct charset *cs;
  
  CHECK_CHARSET_GET_ID (charset, id);
  cs = CHARSET_FROM_ID (id);
  if (CHARSET_METHOD (cs) == CHARSET_METHOD_MAP_DEFERRED)
    load_charset (cs);
  if (CHARSET_UNIFIED_P (cs)
      && CHAR_TABLE_P (CHARSET_DEUNIFIER (cs)))
    return Qnil;
  CHARSET_UNIFIED_P (cs) = 0;
  if (NILP (unify_map))
    unify_map = CHARSET_UNIFY_MAP (cs);
  if (STRINGP (unify_map))
    load_charset_map_from_file (cs, unify_map, 2);
  else
    load_charset_map_from_vector (cs, unify_map, 2);
  CHARSET_UNIFIED_P (cs) = 1;
  return Qnil;
}

DEFUN ("get-unused-iso-final-char", Fget_unused_iso_final_char,
       Sget_unused_iso_final_char, 2, 2, 0,
       doc: /*
Return an unsed ISO's final char for a charset of DIMENISION and CHARS.
DIMENSION is the number of bytes to represent a character: 1 or 2.
CHARS is the number of characters in a dimension: 94 or 96.

This final char is for private use, thus the range is `0' (48) .. `?' (63).
If there's no unused final char for the attrified kind of charset,
return nil.  */)
     (dimension, chars)
     Lisp_Object dimension, chars;
{
  int final_char;

  CHECK_NUMBER (dimension);
  CHECK_NUMBER (chars);
  if (XINT (dimension) != 1 && XINT (dimension) != 2 && XINT (dimension) != 3)
    args_out_of_range_3 (dimension, make_number (1), make_number (3));
  if (XINT (chars) != 94 && XINT (chars) != 96)
    args_out_of_range_3 (chars, make_number (94), make_number (96));
  for (final_char = '0'; final_char <= '?'; final_char++)
    if (ISO_CHARSET_TABLE (XINT (dimension), XINT (chars), final_char) < 0)
      break;
  return (final_char <= '?' ? make_number (final_char) : Qnil);
}

static void
check_iso_charset_parameter (dimension, chars, final_char)
     Lisp_Object dimension, chars, final_char;
{
  CHECK_NATNUM (dimension);
  CHECK_NATNUM (chars);
  CHECK_NATNUM (final_char);

  if (XINT (dimension) > 3)
    error ("Invalid DIMENSION %d, it should be 1, 2, or 3", XINT (dimension));
  if (XINT (chars) != 94 && XINT (chars) != 96)
    error ("Invalid CHARS %d, it should be 94 or 96", XINT (chars));
  if (XINT (final_char) < '0' || XINT (final_char) > '~')
    error ("Invalid FINAL-CHAR %c, it should be `0'..`~'", XINT (chars));
}


DEFUN ("declare-equiv-charset", Fdeclare_equiv_charset, Sdeclare_equiv_charset,
       4, 4, 0,
       doc: /*
Declare a charset of DIMENSION, CHARS, FINAL-CHAR is the same as CHARSET.
CHARSET should be defined by `defined-charset' in advance.  */)
     (dimension, chars, final_char, charset)
     Lisp_Object dimension, chars, final_char, charset;
{
  int id;

  CHECK_CHARSET_GET_ID (charset, id);
  check_iso_charset_parameter (dimension, chars, final_char);

  ISO_CHARSET_TABLE (dimension, chars, final_char) = id;
  return Qnil;
}


/* Return information about charsets in the text at PTR of NBYTES
   bytes, which are NCHARS characters.  The value is:

	0: Each character is represented by one byte.  This is always
	   true for a unibyte string.  For a multibyte string, true if
	   it contains only ASCII characters.

	1: No charsets other than ascii, eight-bit-control, and
	latin-1 are found.

	2: Otherwise.
*/

int
string_xstring_p (string)
     Lisp_Object string;
{
  unsigned char *p = XSTRING (string)->data;
  unsigned char *endp = p + STRING_BYTES (XSTRING (string));
  struct charset *charset;

  if (XSTRING (string)->size == STRING_BYTES (XSTRING (string)))
    return 0;

  charset = CHARSET_FROM_ID (charset_iso_8859_1);
  while (p < endp)
    {
      int c = STRING_CHAR_ADVANCE (p);

      if (ENCODE_CHAR (charset, c) < 0)
	return 2;
    }
  return 1;
}


/* Find charsets in the string at PTR of NCHARS and NBYTES.

   CHARSETS is a vector.  Each element is a cons of CHARSET and
   FOUND-FLAG.  CHARSET is a charset id, and FOUND-FLAG is nil or t.
   FOUND-FLAG t (or nil) means that the corresponding charset is
   already found (or not yet found).

   It may lookup a translation table TABLE if supplied.  */

static void
find_charsets_in_text (ptr, nchars, nbytes, charsets, table)
     unsigned char *ptr;
     int nchars, nbytes;
     Lisp_Object charsets, table;
{
  unsigned char *pend = ptr + nbytes;
  int ncharsets = ASIZE (charsets);

  if (nchars == nbytes)
    return;

  while (ptr < pend)
    {
      int c = STRING_CHAR_ADVANCE (ptr);
      int i;
      int all_found = 1;
      Lisp_Object elt;

      if (!NILP (table))
	c = translate_char (table, c);
      for (i = 0; i < ncharsets; i++)
	{
	  elt = AREF (charsets, i);
	  if (NILP (XCDR (elt)))
	    {
	      struct charset *charset = CHARSET_FROM_ID (XINT (XCAR (elt)));

	      if (ENCODE_CHAR (charset, c) != CHARSET_INVALID_CODE (charset))
		XCDR (elt) = Qt;
	      else
		all_found = 0;
	    }
	}
      if (all_found)
	break;
    }
}


DEFUN ("find-charset-region", Ffind_charset_region, Sfind_charset_region,
       2, 3, 0,
       doc: /* Return a list of charsets in the region between BEG and END.
BEG and END are buffer positions.
Optional arg TABLE if non-nil is a translation table to look up.

If the region contains invalid multibyte characters,
`unknown' is included in the returned list.

If the current buffer is unibyte, the returned list may contain
only `ascii', `eight-bit-control', and `eight-bit-graphic'.  */)
     (beg, end, table)
     Lisp_Object beg, end, table;
{
  Lisp_Object charsets;
  int from, from_byte, to, stop, stop_byte, i;
  Lisp_Object val;

  validate_region (&beg, &end);
  from = XFASTINT (beg);
  stop = to = XFASTINT (end);

  if (from < GPT && GPT < to)
    {
      stop = GPT;
      stop_byte = GPT_BYTE;
    }
  else
    stop_byte = CHAR_TO_BYTE (stop);

  from_byte = CHAR_TO_BYTE (from);

  charsets = Fmake_vector (make_number (charset_table_used), Qnil);
  for (i = 0; i < charset_table_used; i++)
    ASET (charsets, i, Fcons (make_number (i), Qnil));

  while (1)
    {
      find_charsets_in_text (BYTE_POS_ADDR (from_byte), stop - from,
			     stop_byte - from_byte, charsets, table);
      if (stop < to)
	{
	  from = stop, from_byte = stop_byte;
	  stop = to, stop_byte = CHAR_TO_BYTE (stop);
	}
      else
	break;
    }

  val = Qnil;
  for (i = charset_table_used - 1; i >= 0; i--)
    if (!NILP (XCDR (AREF (charsets, i))))
      val = Fcons (CHARSET_NAME (charset_table + i), val);
  return val;
}

DEFUN ("find-charset-string", Ffind_charset_string, Sfind_charset_string,
       1, 2, 0,
       doc: /* Return a list of charsets in STR.
Optional arg TABLE if non-nil is a translation table to look up.

If the string contains invalid multibyte characters,
`unknown' is included in the returned list.

If STR is unibyte, the returned list may contain
only `ascii', `eight-bit-control', and `eight-bit-graphic'. */)
     (str, table)
     Lisp_Object str, table;
{
  Lisp_Object charsets;
  int i;
  Lisp_Object val;

  CHECK_STRING (str);

  charsets = Fmake_vector (make_number (charset_table_used), Qnil);
  find_charsets_in_text (XSTRING (str)->data, XSTRING (str)->size,
			 STRING_BYTES (XSTRING (str)), charsets, table);

  val = Qnil;
  for (i = charset_table_used - 1; i >= 0; i--)
    if (!NILP (XCDR (AREF (charsets, i))))
      val = Fcons (CHARSET_NAME (charset_table + i), val);
  return val;
}



/* Return a character correponding to the code-point CODE of
   CHARSET.  */

int
decode_char (charset, code)
     struct charset *charset;
     unsigned code;
{
  int c, char_index;
  enum charset_method method = CHARSET_METHOD (charset);

  if (code < CHARSET_MIN_CODE (charset) || code > CHARSET_MAX_CODE (charset))
    return -1;

  if (method == CHARSET_METHOD_MAP_DEFERRED)
    {
      load_charset (charset);
      method = CHARSET_METHOD (charset);
    }

  if (method == CHARSET_METHOD_INHERIT)
    {
      Lisp_Object parents;

      parents = CHARSET_PARENTS (charset);
      c = -1;
      for (; CONSP (parents); parents = XCDR (parents))
	{
	  int id = XINT (XCAR (XCAR (parents)));
	  int code_offset = XINT (XCDR (XCAR (parents)));
	  unsigned this_code = code + code_offset;

	  charset = CHARSET_FROM_ID (id);
	  if ((c = DECODE_CHAR (charset, this_code)) >= 0)
	    break;
	}
    }
  else
    {
      char_index = CODE_POINT_TO_INDEX (charset, code);
      if (char_index < 0)
	return -1;

      if (method == CHARSET_METHOD_MAP)
	{
	  Lisp_Object decoder;

	  decoder = CHARSET_DECODER (charset);
	  if (! VECTORP (decoder))
	    return -1;
	  c = XINT (AREF (decoder, char_index));
	}
      else
	{
	  c = char_index + CHARSET_CODE_OFFSET (charset);
	}
    }

  if (CHARSET_UNIFIED_P (charset)
      && c >= 0)
    {
      MAYBE_UNIFY_CHAR (c);
    }

  return c;
}


/* Return a code-point of CHAR in CHARSET.  If CHAR doesn't belong to
   CHARSET, return CHARSET_INVALID_CODE (CHARSET).  */

unsigned
encode_char (charset, c)
     struct charset *charset;
     int c;
{
  unsigned code;
  enum charset_method method = CHARSET_METHOD (charset);

  if (CHARSET_UNIFIED_P (charset))
    {
      Lisp_Object deunifier;
      int deunified;

      deunifier = CHARSET_DEUNIFIER (charset);
      if (! CHAR_TABLE_P (deunifier))
	{
	  Funify_charset (CHARSET_NAME (charset), Qnil);
	  deunifier = CHARSET_DEUNIFIER (charset);
	}
      deunified = XINT (CHAR_TABLE_REF (deunifier, c));
      if (deunified > 0)
	c = deunified;
    }

  if (! CHARSET_FAST_MAP_REF ((c), charset->fast_map)
      || c < CHARSET_MIN_CHAR (charset) || c > CHARSET_MAX_CHAR (charset))
    return CHARSET_INVALID_CODE (charset);

  if (method == CHARSET_METHOD_INHERIT)
    {
      Lisp_Object parents;

      parents = CHARSET_PARENTS (charset);
      for (; CONSP (parents); parents = XCDR (parents))
	{
	  int id = XINT (XCAR (XCAR (parents)));
	  int code_offset = XINT (XCDR (XCAR (parents)));
	  struct charset *this_charset = CHARSET_FROM_ID (id);

	  code = ENCODE_CHAR (this_charset, c);
	  if (code != CHARSET_INVALID_CODE (this_charset)
	      && (code_offset < 0 || code >= code_offset))
	    {
	      code -= code_offset;
	      if (code >= charset->min_code && code <= charset->max_code
		  && CODE_POINT_TO_INDEX (charset, code) >= 0)
		return code;
	    }
	}
      return CHARSET_INVALID_CODE (charset);
    }

  if (method == CHARSET_METHOD_MAP_DEFERRED)
    {
      load_charset (charset);
      method = CHARSET_METHOD (charset);
    }

  if (method == CHARSET_METHOD_MAP)
    {
      Lisp_Object encoder;
      Lisp_Object val;

      encoder = CHARSET_ENCODER (charset);
      if (! CHAR_TABLE_P (CHARSET_ENCODER (charset)))
	return CHARSET_INVALID_CODE (charset);
      val = CHAR_TABLE_REF (encoder, c);
      code = XINT (val);
      if (! CHARSET_COMPACT_CODES_P (charset))
	code = INDEX_TO_CODE_POINT (charset, code);
    }
  else
    {
      code = c - CHARSET_CODE_OFFSET (charset);
      code = INDEX_TO_CODE_POINT (charset, code);
    }

  return code;
}


DEFUN ("decode-char", Fdecode_char, Sdecode_char, 2, 3, 0,
       doc: /* Decode the pair of CHARSET and CODE-POINT into a character.
Return nil if CODE-POINT is not valid in CHARSET.

CODE-POINT may be a cons (HIGHER-16-BIT-VALUE . LOWER-16-BIT-VALUE).

Optional argument RESTRICTION specifies a way to map the pair of CCS
and CODE-POINT to a chracter.   Currently not supported and just ignored.  */)
  (charset, code_point, restriction)
     Lisp_Object charset, code_point, restriction;
{
  int c, id;
  unsigned code;
  struct charset *charsetp;

  CHECK_CHARSET_GET_ID (charset, id);
  if (CONSP (code_point))
    {
      CHECK_NATNUM (XCAR (code_point));
      CHECK_NATNUM (XCDR (code_point));
      code = (XINT (XCAR (code_point)) << 16) | (XINT (XCDR (code_point)));
    }
  else
    {
      CHECK_NATNUM (code_point);
      code = XINT (code_point);
    }
  charsetp = CHARSET_FROM_ID (id);
  c = DECODE_CHAR (charsetp, code);
  return (c >= 0 ? make_number (c) : Qnil);
}


DEFUN ("encode-char", Fencode_char, Sencode_char, 2, 3, 0,
       doc: /* Encode the character CH into a code-point of CHARSET.
Return nil if CHARSET doesn't include CH.

Optional argument RESTRICTION specifies a way to map CHAR to a
code-point in CCS.  Currently not supported and just ignored.  */)
     (ch, charset, restriction)
     Lisp_Object ch, charset, restriction;
{
  int c, id;
  unsigned code;
  struct charset *charsetp;

  CHECK_CHARSET_GET_ID (charset, id);
  CHECK_NATNUM (ch);
  c = XINT (ch);
  charsetp = CHARSET_FROM_ID (id);
  code = ENCODE_CHAR (charsetp, ch);
  if (code == CHARSET_INVALID_CODE (charsetp))
    return Qnil;
  if (code > 0x7FFFFFF)
    return Fcons (make_number (code >> 16), make_number (code & 0xFFFF));
  return make_number (code);
}


DEFUN ("make-char", Fmake_char, Smake_char, 1, 5, 0,
       doc:
       /* Return a character of CHARSET whose position codes are CODEn.

CODE1 through CODE4 are optional, but if you don't supply sufficient
position codes, it is assumed that the minimum code in each dimension
is specified.  */)
     (charset, code1, code2, code3, code4)
     Lisp_Object charset, code1, code2, code3, code4;
{
  int id, dimension;
  struct charset *charsetp;
  unsigned code;
  int c;

  CHECK_CHARSET_GET_ID (charset, id);
  charsetp = CHARSET_FROM_ID (id);

  dimension = CHARSET_DIMENSION (charsetp);
  if (NILP (code1))
    code = (CHARSET_ASCII_COMPATIBLE_P (charsetp)
	    ? 0 : CHARSET_MIN_CODE (charsetp));
  else
    {
      CHECK_NATNUM (code1);
      if (XFASTINT (code1) >= 0x100)
	args_out_of_range (make_number (0xFF), code1);
      code = XFASTINT (code1);

      if (dimension > 1)
	{
	  code <<= 8;
	  if (NILP (code2))
	    code |= charsetp->code_space[(dimension - 2) * 4];
	  else
	    {
	      CHECK_NATNUM (code2);
	      if (XFASTINT (code2) >= 0x100)
		args_out_of_range (make_number (0xFF), code2);
	      code |= XFASTINT (code2);
	    }

	  if (dimension > 2)
	    {
	      code <<= 8;
	      if (NILP (code3))
		code |= charsetp->code_space[(dimension - 3) * 4];
	      else
		{
		  CHECK_NATNUM (code3);
		  if (XFASTINT (code3) >= 0x100)
		    args_out_of_range (make_number (0xFF), code3);
		  code |= XFASTINT (code3);
		}

	      if (dimension > 3)
		{
		  code <<= 8;
		  if (NILP (code4))
		    code |= charsetp->code_space[0];
		  else
		    {
		      CHECK_NATNUM (code4);
		      if (XFASTINT (code4) >= 0x100)
			args_out_of_range (make_number (0xFF), code4);
		      code |= XFASTINT (code4);
		    }
		}
	    }
	}
    }

  if (CHARSET_ISO_FINAL (charsetp) >= 0)
    code &= 0x7F7F7F7F;
  c = DECODE_CHAR (charsetp, code);
  if (c < 0)
    error ("Invalid code(s)");
  return make_number (c);
}


/* Return the first charset in CHARSET_LIST that contains C.
   CHARSET_LIST is a list of charset IDs.  If it is nil, use
   Vcharset_ordered_list.  */

struct charset *
char_charset (c, charset_list, code_return)
     int c;
     Lisp_Object charset_list;
     unsigned *code_return;
{
  if (NILP (charset_list))
    charset_list = Vcharset_ordered_list;

  while (CONSP (charset_list))
    {
      struct charset *charset = CHARSET_FROM_ID (XINT (XCAR (charset_list)));
      unsigned code = ENCODE_CHAR (charset, c);

      if (code != CHARSET_INVALID_CODE (charset))
	{
	  if (code_return)
	    *code_return = code;
	  return charset;
	}
      charset_list = XCDR (charset_list);
    }
  return NULL;
}


DEFUN ("split-char", Fsplit_char, Ssplit_char, 1, 1, 0,
       doc: /*Return list of charset and one to three position-codes of CHAR.
If CHAR is invalid as a character code,
return a list of symbol `unknown' and CHAR.  */)
     (ch)
     Lisp_Object ch;
{
  struct charset *charset;
  int c, dimension;
  unsigned code;
  Lisp_Object val;

  CHECK_CHARACTER (ch);
  c = XFASTINT (ch);
  charset = CHAR_CHARSET (c);
  if (! charset)
    return Fcons (intern ("unknown"), Fcons (ch, Qnil));
  
  code = ENCODE_CHAR (charset, c);
  if (code == CHARSET_INVALID_CODE (charset))
    abort ();
  dimension = CHARSET_DIMENSION (charset);
  val = (dimension == 1 ? Fcons (make_number (code), Qnil)
	 : dimension == 2 ? Fcons (make_number (code >> 8),
				   Fcons (make_number (code & 0xFF), Qnil))
	 : Fcons (make_number (code >> 16),
		  Fcons (make_number ((code >> 8) & 0xFF),
			 Fcons (make_number (code & 0xFF), Qnil))));
  return Fcons (CHARSET_NAME (charset), val);
}


DEFUN ("char-charset", Fchar_charset, Schar_charset, 1, 1, 0,
       doc: /* Return the charset of highest priority that contains CHAR.  */)
     (ch)
     Lisp_Object ch;
{
  struct charset *charset;

  CHECK_CHARACTER (ch);
  charset = CHAR_CHARSET (XINT (ch));
  return (CHARSET_NAME (charset));
}


DEFUN ("charset-after", Fcharset_after, Scharset_after, 0, 1, 0,
       doc: /*
Return charset of a character in the current buffer at position POS.
If POS is nil, it defauls to the current point.
If POS is out of range, the value is nil.  */)
     (pos)
     Lisp_Object pos;
{
  Lisp_Object ch;
  struct charset *charset;

  ch = Fchar_after (pos);
  if (! INTEGERP (ch))
    return ch;
  charset = CHAR_CHARSET (XINT (ch));
  return (CHARSET_NAME (charset));
}


DEFUN ("iso-charset", Fiso_charset, Siso_charset, 3, 3, 0,
       doc: /*
Return charset of ISO's specification DIMENSION, CHARS, and FINAL-CHAR.

ISO 2022's designation sequence (escape sequence) distinguishes charsets
by their DIMENSION, CHARS, and FINAL-CHAR,
where as Emacs distinguishes them by charset symbol.
See the documentation of the function `charset-info' for the meanings of
DIMENSION, CHARS, and FINAL-CHAR.  */)
     (dimension, chars, final_char)
     Lisp_Object dimension, chars, final_char;
{
  int id;

  check_iso_charset_parameter (dimension, chars, final_char);
  id = ISO_CHARSET_TABLE (XFASTINT (dimension), XFASTINT (chars),
			  XFASTINT (final_char));
  return (id >= 0 ? CHARSET_NAME (CHARSET_FROM_ID (id)) : Qnil);
}


DEFUN ("clear-charset-maps", Fclear_charset_maps, Sclear_charset_maps,
       0, 0, 0,
       doc: /*
Clear encoder and decoder of charsets that are loaded from mapfiles.  */)
     ()
{
  int i;
  struct charset *charset;
  Lisp_Object attrs;

  for (i = 0; i < charset_table_used; i++)
    {
      charset = CHARSET_FROM_ID (i);
      attrs = CHARSET_ATTRIBUTES (charset);

      if (CHARSET_METHOD (charset) == CHARSET_METHOD_MAP)
	{
	  CHARSET_ATTR_DECODER (attrs) = Qnil;
	  CHARSET_ATTR_ENCODER (attrs) = Qnil;
	  CHARSET_METHOD (charset) = CHARSET_METHOD_MAP_DEFERRED;
	}

      if (CHARSET_UNIFIED_P (charset))
	CHARSET_ATTR_DEUNIFIER (attrs) = Qnil;
    }

  if (CHAR_TABLE_P (Vchar_unified_charset_table))
    {
      Foptimize_char_table (Vchar_unified_charset_table);
      Vchar_unify_table = Vchar_unified_charset_table;
      Vchar_unified_charset_table = Qnil;
    }

  return Qnil;
}


void
init_charset ()
{

}


void
init_charset_once ()
{
  int i, j, k;

  for (i = 0; i < ISO_MAX_DIMENSION; i++)
    for (j = 0; j < ISO_MAX_CHARS; j++)
      for (k = 0; k < ISO_MAX_FINAL; k++)
	iso_charset_table[i][j][k] = -1;

  for (i = 0; i < 255; i++)
    emacs_mule_charset[i] = NULL;

#if 0
  Vchar_charset_set = Fmake_char_table (Qnil, Qnil);
  CHAR_TABLE_SET (Vchar_charset_set, make_number (97), Qnil);

  DEFSYM (Qcharset_encode_table, "charset-encode-table");

  /* Intern this now in case it isn't already done.
     Setting this variable twice is harmless.
     But don't staticpro it here--that is done in alloc.c.  */
  Qchar_table_extra_slots = intern ("char-table-extra-slots");

  /* Now we are ready to set up this property, so we can create syntax
     tables.  */
  Fput (Qcharset_encode_table, Qchar_table_extra_slots, make_number (0));
#endif
}

#ifdef emacs

void
syms_of_charset ()
{
  char *p;

  DEFSYM (Qcharsetp, "charsetp");

  DEFSYM (Qascii, "ascii");
  DEFSYM (Qunicode, "unicode");
  DEFSYM (Qeight_bit_control, "eight-bit-control");
  DEFSYM (Qeight_bit_graphic, "eight-bit-graphic");
  DEFSYM (Qiso_8859_1, "iso-8859-1");

  DEFSYM (Qgl, "gl");
  DEFSYM (Qgr, "gr");

  p = (char *) xmalloc (30000);

  staticpro (&Vcharset_ordered_list);
  Vcharset_ordered_list = Qnil;

  staticpro (&Viso_2022_charset_list);
  Viso_2022_charset_list = Qnil;

  staticpro (&Vemacs_mule_charset_list);
  Vemacs_mule_charset_list = Qnil;

  staticpro (&Vcharset_hash_table);
  Vcharset_hash_table = Fmakehash (Qeq);

  charset_table_size = 128;
  charset_table = ((struct charset *)
		   xmalloc (sizeof (struct charset) * charset_table_size));
  charset_table_used = 0;

  staticpro (&Vchar_unified_charset_table);
  Vchar_unified_charset_table = Fmake_char_table (Qnil, make_number (-1));

  defsubr (&Scharsetp);
  defsubr (&Smap_charset_chars);
  defsubr (&Sdefine_charset_internal);
  defsubr (&Sdefine_charset_alias);
  defsubr (&Sprimary_charset);
  defsubr (&Sset_primary_charset);
  defsubr (&Scharset_plist);
  defsubr (&Sset_charset_plist);
  defsubr (&Sunify_charset);
  defsubr (&Sget_unused_iso_final_char);
  defsubr (&Sdeclare_equiv_charset);
  defsubr (&Sfind_charset_region);
  defsubr (&Sfind_charset_string);
  defsubr (&Sdecode_char);
  defsubr (&Sencode_char);
  defsubr (&Ssplit_char);
  defsubr (&Smake_char);
  defsubr (&Schar_charset);
  defsubr (&Scharset_after);
  defsubr (&Siso_charset);
  defsubr (&Sclear_charset_maps);

  DEFVAR_LISP ("charset-map-directory", &Vcharset_map_directory,
	       doc: /* Directory of charset map files that come with GNU Emacs.
The default value is sub-directory "charsets" of `data-directory'.  */);
  Vcharset_map_directory = Fexpand_file_name (build_string ("charsets"),
					      Vdata_directory);

  DEFVAR_LISP ("charset-list", &Vcharset_list,
	       doc: /* List of charsets ever defined.  */);
  Vcharset_list = Qnil;

  /* Make the prerequisite charset `ascii' and `unicode'.  */
  {
    Lisp_Object args[charset_arg_max];
    Lisp_Object plist[14];
    Lisp_Object val;

    plist[0] = intern (":name");
    plist[1] = args[charset_arg_name] = Qascii;
    plist[2] = intern (":dimension");
    plist[3] = args[charset_arg_dimension] = make_number (1);
    val = Fmake_vector (make_number (8), make_number (0));
    ASET (val, 1, make_number (127));
    plist[4] = intern (":code-space");
    plist[5] = args[charset_arg_code_space] = val;
    plist[6] = intern (":iso-final-char");
    plist[7] = args[charset_arg_iso_final] = make_number ('B');
    args[charset_arg_iso_revision] = Qnil;
    plist[8] = intern (":emacs-mule-id");
    plist[9] = args[charset_arg_emacs_mule_id] = make_number (0);
    plist[10] = intern (":ascii-compatible-p");
    plist[11] = args[charset_arg_ascii_compatible_p] = Qt;
    args[charset_arg_supplementary_p] = Qnil;
    args[charset_arg_invalid_code] = Qnil;
    plist[12] = intern (":code-offset");
    plist[13] = args[charset_arg_code_offset] = make_number (0);
    args[charset_arg_map] = Qnil;
    args[charset_arg_parents] = Qnil;
    args[charset_arg_unify_map] = Qnil;
    /* The actual plist is set by mule-conf.el.  */
    args[charset_arg_plist] = Flist (14, plist);
    Fdefine_charset_internal (charset_arg_max, args);
    charset_ascii = CHARSET_SYMBOL_ID (Qascii);

    plist[1] = args[charset_arg_name] = Qunicode;
    plist[3] = args[charset_arg_dimension] = make_number (3);
    val = Fmake_vector (make_number (8), make_number (0));
    ASET (val, 1, make_number (255));
    ASET (val, 3, make_number (255));
    ASET (val, 5, make_number (16));
    plist[5] = args[charset_arg_code_space] = val;
    plist[7] = args[charset_arg_iso_final] = Qnil;
    args[charset_arg_iso_revision] = Qnil;
    plist[9] = args[charset_arg_emacs_mule_id] = Qnil;
    plist[11] = args[charset_arg_ascii_compatible_p] = Qt;
    args[charset_arg_supplementary_p] = Qnil;
    args[charset_arg_invalid_code] = Qnil;
    plist[13] = args[charset_arg_code_offset] = make_number (0);
    args[charset_arg_map] = Qnil;
    args[charset_arg_parents] = Qnil;
    args[charset_arg_unify_map] = Qnil;
    /* The actual plist is set by mule-conf.el.  */
    args[charset_arg_plist] = Flist (14, plist);
    Fdefine_charset_internal (charset_arg_max, args);
    charset_unicode = CHARSET_SYMBOL_ID (Qunicode);
  }
}

#endif /* emacs */
