/* Composite sequence support.
   Copyright (C) 1999 Electrotechnical Laboratory, JAPAN.
   Licensed to the Free Software Foundation.
   Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

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
#include "charset.h"
#include "intervals.h"

/* Emacs uses special text property `composition' to support character
   composition.  A sequence of characters that have the same (i.e. eq)
   `composition' property value is treated as a single composite
   sequence (we call it just `composition' here after).  Characters in
   a composition are all composed somehow on the screen.

   The property value has this form when the composition is made:
	((LENGTH . COMPONENTS) . MODIFICATION-FUNC)
   then turns to this form:
	(COMPOSITION-ID . (LENGTH COMPONENTS-VEC . MODIFICATION-FUNC))
   when the composition is registered in composition_hash_table and
   composition_table.  These rather peculiar structures were designed
   to make it easy to distinguish them quickly (we can do that by
   checking only the first element) and to extract LENGTH (from the
   former form) and COMPOSITION-ID (from the latter form).

   We register a composition when it is displayed, or when the width
   is required (for instance, to calculate columns).

   LENGTH -- Length of the composition.  This information is used to
	check the validity of the composition.

   COMPONENTS --  Character, string, vector, list, or nil.

	If it is nil, characters in the text are composed relatively
	according to their metrics in font glyphs.

	If it is a character or a string, the character or characters
	in the string are composed relatively.

	If it is a vector or list of integers, the element is a
	character or an encoded composition rule.  The characters are
	composed according to the rules.  (2N)th elements are
	characters to be composed and (2N+1)th elements are
	composition rules to tell how to compose (2N+2)th element with
	the previously composed 2N glyphs.

   COMPONENTS-VEC -- Vector of integers.  In relative composition, the
	elements are characters to be composed.  In rule-base
	composition, the elements are characters or encoded
	composition rules.

   MODIFICATION-FUNC -- If non nil, it is a function to call when the
	composition gets invalid after a modification in a buffer.  If
	it is nil, a function in `composition-function-table' of the
	first character in the sequence is called.

   COMPOSITION-ID --Identification number of the composition.  It is
	used as an index to composition_table for the composition.

   When Emacs has to display a composition or has to know its
   displaying width, the function get_composition_id is called.  It
   returns COMPOSITION-ID so that the caller can access the
   information about the composition through composition_table.  If a
   COMPOSITION-ID has not yet been assigned to the composition,
   get_composition_id checks the validity of `composition' property,
   and, if valid, assigns a new ID, registers the information in
   composition_hash_table and composition_table, and changes the form
   of the property value.  If the property is invalid, return -1
   without changing the property value.

   We use two tables to keep information about composition;
   composition_hash_table and composition_table.

   The former is a hash table in which keys are COMPONENTS-VECs and
   values are the corresponding COMPOSITION-IDs.  This hash table is
   weak, but as each key (COMPONENTS-VEC) is also kept as a value of the
   `composition' property, it won't be collected as garbage until all
   bits of text that have the same COMPONENTS-VEC are deleted.

   The latter is a table of pointers to `struct composition' indexed
   by COMPOSITION-ID.  This structure keeps the other information (see
   composite.h).

   In general, a text property holds information about individual
   characters.  But, a `composition' property holds information about
   a sequence of characters (in this sense, it is like the `intangible'
   property).  That means that we should not share the property value
   in adjacent compositions -- we can't distinguish them if they have the
   same property.  So, after any changes, we call
   `update_compositions' and change a property of one of adjacent
   compositions to a copy of it.  This function also runs a proper
   composition modification function to make a composition that gets
   invalid by the change valid again.

   As the value of the `composition' property holds information about a
   specific range of text, the value gets invalid if we change the
   text in the range.  We treat the `composition' property as always
   rear-nonsticky (currently by setting default-text-properties to
   (rear-nonsticky (composition))) and we never make properties of
   adjacent compositions identical.  Thus, any such changes make the
   range just shorter.  So, we can check the validity of the `composition'
   property by comparing LENGTH information with the actual length of
   the composition.

*/


Lisp_Object Qcomposition;

/* Table of pointers to the structure `composition' indexed by
   COMPOSITION-ID.  This structure is for storing information about
   each composition except for COMPONENTS-VEC.  */
struct composition **composition_table;

/* The current size of `composition_table'.  */
static int composition_table_size;

/* Number of compositions currently made. */
int n_compositions;

/* Hash table for compositions.  The key is COMPONENTS-VEC of
   `composition' property.  The value is the corresponding
   COMPOSITION-ID.  */
Lisp_Object composition_hash_table;

/* Function to call to adjust composition.  */
Lisp_Object Vcompose_chars_after_function;

/* Char-table of patterns and functions to make a composition.  */
Lisp_Object Vcomposition_function_table;
Lisp_Object Qcomposition_function_table;

/* Temporary variable used in macros COMPOSITION_XXX.  */
Lisp_Object composition_temp;

/* Return how many columns C will occupy on the screen.  It always
   returns 1 for control characters and 8-bit characters because those
   are just ignored in a composition.  */
#define CHAR_WIDTH(c) \
  (SINGLE_BYTE_CHAR_P (c) ? 1 : CHARSET_WIDTH (CHAR_CHARSET (c)))

/* Return COMPOSITION-ID of a composition at buffer position
   CHARPOS/BYTEPOS and length NCHARS.  The `composition' property of
   the sequence is PROP.  STRING, if non-nil, is a string that
   contains the composition instead of the current buffer.

   If the composition is invalid, return -1.  */

int
get_composition_id (charpos, bytepos, nchars, prop, string)
     int charpos, bytepos, nchars;
     Lisp_Object prop, string;
{
  Lisp_Object id, length, components, key, *key_contents;
  int glyph_len;
  struct Lisp_Hash_Table *hash_table = XHASH_TABLE (composition_hash_table);
  int hash_index;
  unsigned hash_code;
  struct composition *cmp;
  int i, ch;

  /* PROP should be
	Form-A: ((LENGTH . COMPONENTS) . MODIFICATION-FUNC)
     or
	Form-B: (COMPOSITION-ID . (LENGTH COMPONENTS-VEC . MODIFICATION-FUNC))
  */
  if (nchars == 0 || !CONSP (prop))
    goto invalid_composition;

  id = XCAR (prop);
  if (INTEGERP (id))
    {
      /* PROP should be Form-B.  */
      if (XINT (id) < 0 || XINT (id) >= n_compositions)
	goto invalid_composition;
      return XINT (id);
    }

  /* PROP should be Form-A.
     Thus, ID should be (LENGTH . COMPONENTS).  */
  if (!CONSP (id))
    goto invalid_composition;
  length = XCAR (id);
  if (!INTEGERP (length) || XINT (length) != nchars)
    goto invalid_composition;

  components = XCDR (id);

  /* Check if the same composition has already been registered or not
     by consulting composition_hash_table.  The key for this table is
     COMPONENTS (converted to a vector COMPONENTS-VEC) or, if it is
     nil, vector of characters in the composition range.  */
  if (INTEGERP (components))
    key = Fmake_vector (make_number (1), components);
  else if (STRINGP (components) || CONSP (components))
    key = Fvconcat (1, &components);
  else if (VECTORP (components))
    key = components;
  else if (NILP (components))
    {
      key = Fmake_vector (make_number (nchars), Qnil);
      if (STRINGP (string))
	for (i = 0; i < nchars; i++)
	  {
	    FETCH_STRING_CHAR_ADVANCE (ch, string, charpos, bytepos);
	    XVECTOR (key)->contents[i] = make_number (ch);
	  }
      else
	for (i = 0; i < nchars; i++)
	  {
	    FETCH_CHAR_ADVANCE (ch, charpos, bytepos);
	    XVECTOR (key)->contents[i] = make_number (ch);
	  }
    }
  else
    goto invalid_composition;

  hash_index = hash_lookup (hash_table, key, &hash_code);
  if (hash_index >= 0)
    {
      /* We have already registered the same composition.  Change PROP
	 from Form-A above to Form-B while replacing COMPONENTS with
	 COMPONENTS-VEC stored in the hash table.  We can directly
	 modify the cons cell of PROP because it is not shared.  */
      key = HASH_KEY (hash_table, hash_index);
      id = HASH_VALUE (hash_table, hash_index);
      XSETCAR (prop, id);
      XSETCDR (prop, Fcons (make_number (nchars), Fcons (key, XCDR (prop))));
      return XINT (id);
    }

  /* This composition is a new one.  We must register it.  */

  /* Check if we have sufficient memory to store this information.  */
  if (composition_table_size == 0)
    {
      composition_table_size = 256;
      composition_table
	= (struct composition **) xmalloc (sizeof (composition_table[0])
					   * composition_table_size);
    }
  else if (composition_table_size <= n_compositions)
    {
      composition_table_size += 256;
      composition_table
	= (struct composition **) xrealloc (composition_table,
					    sizeof (composition_table[0])
					    * composition_table_size);
    }

  key_contents = XVECTOR (key)->contents;

  /* Check if the contents of COMPONENTS are valid if COMPONENTS is a
     vector or a list.  It should be a sequence of:
	char1 rule1 char2 rule2 char3 ...    ruleN charN+1  */
  if (VECTORP (components) || CONSP (components))
    {
      int len = XVECTOR (key)->size;

      /* The number of elements should be odd.  */
      if ((len % 2) == 0)
	goto invalid_composition;
      /* All elements should be integers (character or encoded
         composition rule).  */
      for (i = 0; i < len; i++)
	{
	  if (!INTEGERP (key_contents[i]))
	    goto invalid_composition;
	}
    }

  /* Change PROP from Form-A above to Form-B.  We can directly modify
     the cons cell of PROP because it is not shared.  */
  XSETFASTINT (id, n_compositions);
  XSETCAR (prop, id);
  XSETCDR (prop, Fcons (make_number (nchars), Fcons (key, XCDR (prop))));

  /* Register the composition in composition_hash_table.  */
  hash_index = hash_put (hash_table, key, id, hash_code);

  /* Register the composition in composition_table.  */
  cmp = (struct composition *) xmalloc (sizeof (struct composition));

  cmp->method = (NILP (components)
		 ? COMPOSITION_RELATIVE
		 : ((INTEGERP (components) || STRINGP (components))
		    ? COMPOSITION_WITH_ALTCHARS
		    : COMPOSITION_WITH_RULE_ALTCHARS));
  cmp->hash_index = hash_index;
  glyph_len = (cmp->method == COMPOSITION_WITH_RULE_ALTCHARS
	       ? (XVECTOR (key)->size + 1) / 2
	       : XVECTOR (key)->size);
  cmp->glyph_len = glyph_len;
  cmp->offsets = (short *) xmalloc (sizeof (short) * glyph_len * 2);
  cmp->font = NULL;

  /* Calculate the width of overall glyphs of the composition.  */
  if (cmp->method != COMPOSITION_WITH_RULE_ALTCHARS)
    {
      /* Relative composition.  */
      cmp->width = 0;
      for (i = 0; i < glyph_len; i++)
	{
	  int this_width;
	  ch = XINT (key_contents[i]);
	  this_width = CHAR_WIDTH (ch);
	  if (cmp->width < this_width)
	    cmp->width = this_width;
	}
    }
  else
    {
      /* Rule-base composition.  */
      float leftmost = 0.0, rightmost;

      ch = XINT (key_contents[0]);
      rightmost = CHAR_WIDTH (ch);

      for (i = 1; i < glyph_len; i += 2)
	{
	  int rule, gref, nref;
	  int this_width;
	  float this_left;

	  rule = XINT (key_contents[i]);
	  ch = XINT (key_contents[i + 1]);
	  this_width = CHAR_WIDTH (ch);

	  /* A composition rule is specified by an integer value
	     that encodes global and new reference points (GREF and
	     NREF).  GREF and NREF are specified by numbers as
	     below:
		0---1---2 -- ascent
		|       |
		|       |
		|       |
		9--10--11 -- center
		|       |
	     ---3---4---5--- baseline
		|       |
		6---7---8 -- descent
	  */
	  COMPOSITION_DECODE_RULE (rule, gref, nref);
	  this_left = (leftmost
		       + (gref % 3) * (rightmost - leftmost) / 2.0
		       - (nref % 3) * this_width / 2.0);

	  if (this_left < leftmost)
	    leftmost = this_left;
	  if (this_left + this_width > rightmost)
	    rightmost = this_left + this_width;
	}

      cmp->width = rightmost - leftmost;
      if (cmp->width < (rightmost - leftmost))
	/* To get a ceiling integer value.  */
	cmp->width++;
    }

  composition_table[n_compositions] = cmp;

  return n_compositions++;

 invalid_composition:
  /* Would it be better to remove this `composition' property?  */
  return -1;
}


/* Find a composition at or nearest to position POS of OBJECT (buffer
   or string).

   OBJECT defaults to the current buffer.  If there's a composition at
   POS, set *START and *END to the start and end of the sequence,
   *PROP to the `composition' property, and return 1.

   If there's no composition at POS and LIMIT is negative, return 0.

   Otherwise, search for a composition forward (LIMIT > POS) or
   backward (LIMIT < POS).  In this case, LIMIT bounds the search.

   If a composition is found, set *START, *END, and *PROP as above,
   and return 1, else return 0.

   This doesn't check the validity of composition.  */

int
find_composition (pos, limit, start, end, prop, object)
     int pos, limit, *start, *end;
     Lisp_Object *prop, object;
{
  Lisp_Object val;

  if (get_property_and_range (pos, Qcomposition, prop, start, end, object))
    return 1;

  if (limit < 0 || limit == pos)
    return 0;

  if (limit > pos)		/* search forward */
    {
      val = Fnext_single_property_change (make_number (pos), Qcomposition,
					  object, make_number (limit));
      pos = XINT (val);
      if (pos == limit)
	return 0;
    }
  else				/* search backward */
    {
      if (get_property_and_range (pos - 1, Qcomposition, prop, start, end,
				  object))
	return 1;
      val = Fprevious_single_property_change (make_number (pos), Qcomposition,
					      object, make_number (limit));
      pos = XINT (val);
      if (pos == limit)
	return 0;
      pos--;
    }
  get_property_and_range (pos, Qcomposition, prop, start, end, object);
  return 1;
}

/* Run a proper function to adjust the composition sitting between
   FROM and TO with property PROP.  */

static void
run_composition_function (from, to, prop)
     int from, to;
     Lisp_Object prop;
{
  Lisp_Object func;
  int start, end;

  func = COMPOSITION_MODIFICATION_FUNC (prop);
  /* If an invalid composition precedes or follows, try to make them
     valid too.  */
  if (from > BEGV
      && find_composition (from - 1, -1, &start, &end, &prop, Qnil)
      && !COMPOSITION_VALID_P (start, end, prop))
    from = start;
  if (to < ZV
      && find_composition (to, -1, &start, &end, &prop, Qnil)
      && !COMPOSITION_VALID_P (start, end, prop))
    to = end;
  if (!NILP (Ffboundp (func)))
    call2 (func, make_number (from), make_number (to));
  else if (!NILP (Ffboundp (Vcompose_chars_after_function)))
    call3 (Vcompose_chars_after_function,
	   make_number (from), make_number (to), Qnil);
}

/* Make invalid compositions adjacent to or inside FROM and TO valid.
   CHECK_MASK is bitwise `or' of mask bits defined by macros
   CHECK_XXX (see the comment in composite.h).

   This function is called when a buffer text is changed.  If the
   change is deletion, FROM == TO.  Otherwise, FROM < TO.  */

void
update_compositions (from, to, check_mask)
     int from, to, check_mask;
{
  Lisp_Object prop;
  int start, end;

  if (inhibit_modification_hooks)
    return;

  /* If FROM and TO are not in a valid range, do nothing.  */
  if (! (BEGV <= from && from <= to && to <= ZV))
    return;

  if (check_mask & CHECK_HEAD)
    {
      /* FROM should be at composition boundary.  But, insertion or
	 deletion will make two compositions adjacent and
	 indistinguishable when they have same (eq) property.  To
	 avoid it, in such a case, we change the property of the
	 latter to the copy of it.  */
      if (from > BEGV
	  && find_composition (from - 1, -1, &start, &end, &prop, Qnil))
	{
	  if (from < end)
	    Fput_text_property (make_number (from), make_number (end),
				Qcomposition,
				Fcons (XCAR (prop), XCDR (prop)), Qnil);
	  run_composition_function (start, end, prop);
	  from = end;
	}
      else if (from < ZV
	       && find_composition (from, -1, &start, &from, &prop, Qnil))
	run_composition_function (start, from, prop);
    }

  if (check_mask & CHECK_INSIDE)
    {
      /* In this case, we are sure that (check & CHECK_TAIL) is also
         nonzero.  Thus, here we should check only compositions before
         (to - 1).  */
      while (from < to - 1
	     && find_composition (from, to, &start, &from, &prop, Qnil)
	     && from < to - 1)
	run_composition_function (start, from, prop);
    }

  if (check_mask & CHECK_TAIL)
    {
      if (from < to
	  && find_composition (to - 1, -1, &start, &end, &prop, Qnil))
	{
	  /* TO should be also at composition boundary.  But,
	     insertion or deletion will make two compositions adjacent
	     and indistinguishable when they have same (eq) property.
	     To avoid it, in such a case, we change the property of
	     the former to the copy of it.  */
	  if (to < end)
	    Fput_text_property (make_number (start), make_number (to),
				Qcomposition,
				Fcons (XCAR (prop), XCDR (prop)), Qnil);
	  run_composition_function (start, end, prop);
	}
      else if (to < ZV
	       && find_composition (to, -1, &start, &end, &prop, Qnil))
	run_composition_function (start, end, prop);
    }
}


/* Modify composition property values in LIST destructively.  LIST is
   a list as returned from text_property_list.  Change values to the
   top-level copies of them so that none of them are `eq'.  */

void
make_composition_value_copy (list)
     Lisp_Object list;
{
  Lisp_Object plist, val;

  for (; CONSP (list); list = XCDR (list))
    {
      plist = XCAR (XCDR (XCDR (XCAR (list))));
      while (CONSP (plist) && CONSP (XCDR (plist)))
	{
	  if (EQ (XCAR (plist), Qcomposition)
	      && (val = XCAR (XCDR (plist)), CONSP (val)))
	    XSETCAR (XCDR (plist), Fcons (XCAR (val), XCDR (val)));
	  plist = XCDR (XCDR (plist));
	}
    }
}


/* Make text in the region between START and END a composition that
   has COMPONENTS and MODIFICATION-FUNC.

   If STRING is non-nil, then operate on characters contained between
   indices START and END in STRING.  */

void
compose_text (start, end, components, modification_func, string)
     int start, end;
     Lisp_Object components, modification_func, string;
{
  Lisp_Object prop;

  prop = Fcons (Fcons (make_number (end - start), components),
		modification_func);
  Fput_text_property  (make_number (start), make_number (end),
		       Qcomposition, prop, string);
}

/* Compose sequences of characters in the region between START and END
   by functions registered in Vcomposition_function_table.  If STRING
   is non-nil, operate on characters contained between indices START
   and END in STRING.  */

void
compose_chars_in_text (start, end, string)
     int start, end;
     Lisp_Object string;
{
  int count = 0;
  struct gcpro gcpro1;
  Lisp_Object tail, elt, val, to;
  /* Set to nonzero if we don't have to compose ASCII characters.  */
  int skip_ascii;
  int i, len, stop, c;
  const unsigned char *ptr, *pend;

  if (! CHAR_TABLE_P (Vcomposition_function_table))
    return;

  if (STRINGP (string))
    {
      count = SPECPDL_INDEX ();
      GCPRO1 (string);
      stop = end;
      ptr = SDATA (string) + string_char_to_byte (string, start);
      pend = SDATA (string) + SBYTES (string);
    }
  else
    {
      record_unwind_protect (save_excursion_restore, save_excursion_save ());
      TEMP_SET_PT (start);
      stop = (start < GPT && GPT < end ? GPT : end);
      ptr = CHAR_POS_ADDR (start);
      pend = CHAR_POS_ADDR (end);
    }

  /* Preserve the match data.  */
  record_unwind_save_match_data ();

  /* If none of ASCII characters have composition functions, we can
     skip them quickly.  */
  for (i = 0; i < 128; i++)
    if (!NILP (CHAR_TABLE_REF (Vcomposition_function_table, i)))
      break;
  skip_ascii = (i == 128);


  while (1)
    {
      if (skip_ascii)
	while (start < stop && ASCII_BYTE_P (*ptr))
	  start++, ptr++;

      if (start >= stop)
	{
	  if (stop == end || start >= end)
	    break;
	  stop = end;
	  if (STRINGP (string))
	    ptr = SDATA (string) + string_char_to_byte (string, start);
	  else
	    ptr = CHAR_POS_ADDR (start);
	}

      c = STRING_CHAR_AND_LENGTH (ptr, pend - ptr, len);
      tail = CHAR_TABLE_REF (Vcomposition_function_table, c);
      while (CONSP (tail))
	{
	  elt = XCAR (tail);
	  if (CONSP (elt)
	      && STRINGP (XCAR (elt))
	      && !NILP (Ffboundp (XCDR (elt))))
	    {
	      if (STRINGP (string))
		val = Fstring_match (XCAR (elt), string, make_number (start));
	      else
		{
		  val = Flooking_at (XCAR (elt));
		  if (!NILP (val))
		    val = make_number (start);
		}
	      if (INTEGERP (val) && XFASTINT (val) == start)
		{
		  to = Fmatch_end (make_number (0));
		  val = call4 (XCDR (elt), val, to, XCAR (elt), string);
		  if (INTEGERP (val) && XINT (val) > 1)
		    {
		      start += XINT (val);
		      if (STRINGP (string))
			{
			  ptr = SDATA (string) + string_char_to_byte (string, start);
			  pend = SDATA (string) + SBYTES (string);
			}
		      else
			ptr = CHAR_POS_ADDR (start);
		    }
		  else if (STRINGP (string))
		    {
		      start++;
		      ptr = SDATA (string) + string_char_to_byte (string, start);
		      pend = SDATA (string) + SBYTES (string);
		    }
		  else
		    {
		      start++;
		      ptr += len;
		    }
		  break;
		}
	    }
	  tail = XCDR (tail);
	}
      if (!CONSP (tail))
	{
	  /* No composition done.  Try the next character.  */
	  start++;
	  ptr += len;
	}
    }

  unbind_to (count, Qnil);
  if (STRINGP (string))
    UNGCPRO;
}

/* Emacs Lisp APIs.  */

DEFUN ("compose-region-internal", Fcompose_region_internal,
       Scompose_region_internal, 2, 4, 0,
       doc: /* Internal use only.

Compose text in the region between START and END.
Optional 3rd and 4th arguments are COMPONENTS and MODIFICATION-FUNC
for the composition.  See `compose-region' for more detail.  */)
     (start, end, components, modification_func)
     Lisp_Object start, end, components, modification_func;
{
  validate_region (&start, &end);
  if (!NILP (components)
      && !INTEGERP (components)
      && !CONSP (components)
      && !STRINGP (components))
    CHECK_VECTOR (components);

  compose_text (XINT (start), XINT (end), components, modification_func, Qnil);
  return Qnil;
}

DEFUN ("compose-string-internal", Fcompose_string_internal,
       Scompose_string_internal, 3, 5, 0,
       doc: /* Internal use only.

Compose text between indices START and END of STRING.
Optional 4th and 5th arguments are COMPONENTS and MODIFICATION-FUNC
for the composition.  See `compose-string' for more detail.  */)
     (string, start, end, components, modification_func)
     Lisp_Object string, start, end, components, modification_func;
{
  CHECK_STRING (string);
  CHECK_NUMBER (start);
  CHECK_NUMBER (end);

  if (XINT (start) < 0 ||
      XINT (start) > XINT (end)
      || XINT (end) > SCHARS (string))
    args_out_of_range (start, end);

  compose_text (XINT (start), XINT (end), components, modification_func, string);
  return string;
}

DEFUN ("find-composition-internal", Ffind_composition_internal,
       Sfind_composition_internal, 4, 4, 0,
       doc: /* Internal use only.

Return information about composition at or nearest to position POS.
See `find-composition' for more detail.  */)
     (pos, limit, string, detail_p)
     Lisp_Object pos, limit, string, detail_p;
{
  Lisp_Object prop, tail;
  int start, end;
  int id;

  CHECK_NUMBER_COERCE_MARKER (pos);
  start = XINT (pos);
  if (!NILP (limit))
    {
      CHECK_NUMBER_COERCE_MARKER (limit);
      end = XINT (limit);
    }
  else
    end = -1;

  if (!NILP (string))
    {
      CHECK_STRING (string);
      if (XINT (pos) < 0 || XINT (pos) > SCHARS (string))
	args_out_of_range (string, pos);
    }
  else
    {
      if (XINT (pos) < BEGV || XINT (pos) > ZV)
	args_out_of_range (Fcurrent_buffer (), pos);
    }

  if (!find_composition (start, end, &start, &end, &prop, string))
    return Qnil;
  if (!COMPOSITION_VALID_P (start, end, prop))
    return Fcons (make_number (start), Fcons (make_number (end),
					      Fcons (Qnil, Qnil)));
  if (NILP (detail_p))
    return Fcons (make_number (start), Fcons (make_number (end),
					      Fcons (Qt, Qnil)));

  if (COMPOSITION_REGISTERD_P (prop))
    id = COMPOSITION_ID (prop);
  else
    {
      int start_byte = (NILP (string)
			? CHAR_TO_BYTE (start)
			: string_char_to_byte (string, start));
      id = get_composition_id (start, start_byte, end - start, prop, string);
    }

  if (id >= 0)
    {
      Lisp_Object components, relative_p, mod_func;
      enum composition_method method = COMPOSITION_METHOD (prop);
      int width = composition_table[id]->width;

      components = Fcopy_sequence (COMPOSITION_COMPONENTS (prop));
      relative_p = (method == COMPOSITION_WITH_RULE_ALTCHARS
		    ? Qnil : Qt);
      mod_func = COMPOSITION_MODIFICATION_FUNC (prop);
      tail = Fcons (components,
		    Fcons (relative_p,
			   Fcons (mod_func,
				  Fcons (make_number (width), Qnil))));
    }
  else
    tail = Qnil;

  return Fcons (make_number (start), Fcons (make_number (end), tail));
}


void
syms_of_composite ()
{
  Qcomposition = intern ("composition");
  staticpro (&Qcomposition);

  /* Make a hash table for composition.  */
  {
    Lisp_Object args[6];
    extern Lisp_Object QCsize;

    args[0] = QCtest;
    args[1] = Qequal;
    /* We used to make the hash table weak so that unreferenced
       compostions can be garbage-collected.  But, usually once
       created compositions are repeatedly used in an Emacs session,
       and thus it's not worth to save memory in such a way.  So, we
       make the table not weak.  */
    args[2] = QCweakness;
    args[3] = Qnil;
    args[4] = QCsize;
    args[5] = make_number (311);
    composition_hash_table = Fmake_hash_table (6, args);
    staticpro (&composition_hash_table);
  }

  /* Text property `composition' should be nonsticky by default.  */
  Vtext_property_default_nonsticky
    = Fcons (Fcons (Qcomposition, Qt), Vtext_property_default_nonsticky);

  DEFVAR_LISP ("compose-chars-after-function", &Vcompose_chars_after_function,
	       doc: /* Function to adjust composition of buffer text.

The function is called with three arguments FROM, TO, and OBJECT.
FROM and TO specify the range of text of which composition should be
adjusted.  OBJECT, if non-nil, is a string that contains the text.

This function is called after a text with `composition' property is
inserted or deleted to keep `composition' property of buffer text
valid.

The default value is the function `compose-chars-after'.  */);
  Vcompose_chars_after_function = intern ("compose-chars-after");

  Qcomposition_function_table = intern ("composition-function-table");
  staticpro (&Qcomposition_function_table);

  /* Intern this now in case it isn't already done.
     Setting this variable twice is harmless.
     But don't staticpro it here--that is done in alloc.c.  */
  Qchar_table_extra_slots = intern ("char-table-extra-slots");

  Fput (Qcomposition_function_table, Qchar_table_extra_slots, make_number (0));

  DEFVAR_LISP ("composition-function-table", &Vcomposition_function_table,
	       doc: /* Char table of patterns and functions to make a composition.

Each element is nil or an alist of PATTERNs vs FUNCs, where PATTERNs
are regular expressions and FUNCs are functions.  FUNC is responsible
for composing text matching the corresponding PATTERN.  FUNC is called
with three arguments FROM, TO, and PATTERN.  See the function
`compose-chars-after' for more detail.

This table is looked up by the first character of a composition when
the composition gets invalid after a change in a buffer.  */);
  Vcomposition_function_table
    = Fmake_char_table (Qcomposition_function_table, Qnil);

  defsubr (&Scompose_region_internal);
  defsubr (&Scompose_string_internal);
  defsubr (&Sfind_composition_internal);
}

/* arch-tag: 79cefaf8-ca48-4eed-97e5-d5afb290d272
   (do not change this comment) */
