/* Composite sequence support.
   Copyright (C) 2001, 2002, 2003, 2004, 2005,
                 2006, 2007, 2008 Free Software Foundation, Inc.
   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H14PRO021
   Copyright (C) 2003, 2006
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H13PRO009

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "character.h"
#include "intervals.h"
#include "window.h"
#include "frame.h"
#include "dispextern.h"
#include "font.h"

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

Lisp_Object Qauto_composed;
Lisp_Object Vauto_composition_function;
Lisp_Object Qauto_composition_function;
Lisp_Object Vcomposition_function_table;

EXFUN (Fremove_list_of_text_properties, 4);

/* Temporary variable used in macros COMPOSITION_XXX.  */
Lisp_Object composition_temp;


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

  if (VECTORP (components)
      && ASIZE (components) >= 2
      && VECTORP (AREF (components, 0)))
    {
      /* COMPONENTS is a glyph-string.  */
      int len = ASIZE (key);

      for (i = 1; i < len; i++)
	if (! VECTORP (AREF (key, i)))
	  goto invalid_composition;
    }
  else if (VECTORP (components) || CONSP (components))
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

  if (cmp->method != COMPOSITION_WITH_RULE_ALTCHARS)
    {
      /* Relative composition.  */
      cmp->width = 0;
      for (i = 0; i < glyph_len; i++)
	{
	  int this_width;
	  ch = XINT (key_contents[i]);
	  this_width = (ch == '\t' ? 1 : CHAR_WIDTH (ch));
	  if (cmp->width < this_width)
	    cmp->width = this_width;
	}
    }
  else
    {
      /* Rule-base composition.  */
      float leftmost = 0.0, rightmost;

      ch = XINT (key_contents[0]);
      rightmost = ch != '\t' ? CHAR_WIDTH (ch) : 1;

      for (i = 1; i < glyph_len; i += 2)
	{
	  int rule, gref, nref, xoff, yoff;
	  int this_width;
	  float this_left;

	  rule = XINT (key_contents[i]);
	  ch = XINT (key_contents[i + 1]);
	  this_width = ch != '\t' ? CHAR_WIDTH (ch) : 1;

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
	  COMPOSITION_DECODE_RULE (rule, gref, nref, xoff, yoff);
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


/* Find a static composition at or nearest to position POS of OBJECT
   (buffer or string).

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
     int pos, limit;
     EMACS_INT *start, *end;
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
  EMACS_INT start, end;

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
}

/* Make invalid compositions adjacent to or inside FROM and TO valid.
   CHECK_MASK is bitwise `or' of mask bits defined by macros
   CHECK_XXX (see the comment in composite.h).

   It also resets the text-property `auto-composed' to a proper region
   so that automatic character composition works correctly later while
   displaying the region.

   This function is called when a buffer text is changed.  If the
   change is deletion, FROM == TO.  Otherwise, FROM < TO.  */

void
update_compositions (from, to, check_mask)
     EMACS_INT from, to;
     int check_mask;
{
  Lisp_Object prop;
  EMACS_INT start, end;
  /* The beginning and end of the region to set the property
     `auto-composed' to nil.  */
  EMACS_INT min_pos = from, max_pos = to;

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
	  && find_composition (from - 1, -1, &start, &end, &prop, Qnil)
	  && COMPOSITION_VALID_P (start, end, prop))
	{
	  min_pos = start;
	  if (end > to)
	    max_pos = end;
	  if (from < end)
	    Fput_text_property (make_number (from), make_number (end),
				Qcomposition,
				Fcons (XCAR (prop), XCDR (prop)), Qnil);
	  run_composition_function (start, end, prop);
	  from = end;
	}
      else if (from < ZV
	       && find_composition (from, -1, &start, &from, &prop, Qnil)
	       && COMPOSITION_VALID_P (start, from, prop))
	{
	  if (from > to)
	    max_pos = from;
	  run_composition_function (start, from, prop);
	}
    }

  if (check_mask & CHECK_INSIDE)
    {
      /* In this case, we are sure that (check & CHECK_TAIL) is also
         nonzero.  Thus, here we should check only compositions before
         (to - 1).  */
      while (from < to - 1
	     && find_composition (from, to, &start, &from, &prop, Qnil)
	     && COMPOSITION_VALID_P (start, from, prop)
	     && from < to - 1)
	run_composition_function (start, from, prop);
    }

  if (check_mask & CHECK_TAIL)
    {
      if (from < to
	  && find_composition (to - 1, -1, &start, &end, &prop, Qnil)
	  && COMPOSITION_VALID_P (start, end, prop))
	{
	  /* TO should be also at composition boundary.  But,
	     insertion or deletion will make two compositions adjacent
	     and indistinguishable when they have same (eq) property.
	     To avoid it, in such a case, we change the property of
	     the former to the copy of it.  */
	  if (to < end)
	    {
	      Fput_text_property (make_number (start), make_number (to),
				  Qcomposition,
				  Fcons (XCAR (prop), XCDR (prop)), Qnil);
	      max_pos = end;
	    }
	  run_composition_function (start, end, prop);
	}
      else if (to < ZV
	       && find_composition (to, -1, &start, &end, &prop, Qnil)
	       && COMPOSITION_VALID_P (start, end, prop))
	{
	  run_composition_function (start, end, prop);
	  max_pos = end;
	}
    }
  if (min_pos < max_pos)
    {
      int count = SPECPDL_INDEX ();

      specbind (Qinhibit_read_only, Qt);
      specbind (Qinhibit_modification_hooks, Qt);
      specbind (Qinhibit_point_motion_hooks, Qt);
      Fremove_list_of_text_properties (make_number (min_pos),
				       make_number (max_pos),
				       Fcons (Qauto_composed, Qnil), Qnil);
      unbind_to (count, Qnil);
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


static Lisp_Object autocmp_chars P_ ((Lisp_Object, EMACS_INT, EMACS_INT,
				      EMACS_INT, struct window *,
				      struct face *, Lisp_Object));


/* Lisp glyph-string handlers */

/* Hash table for automatic composition.  The key is a header of a
   lgstring (Lispy glyph-string), and the value is a body of a
   lgstring.  */

static Lisp_Object gstring_hash_table;

static Lisp_Object gstring_lookup_cache P_ ((Lisp_Object));

static Lisp_Object
gstring_lookup_cache (header)
     Lisp_Object header;
{
  struct Lisp_Hash_Table *h = XHASH_TABLE (gstring_hash_table);
  int i = hash_lookup (h, header, NULL);

  return (i >= 0 ? HASH_VALUE (h, i) : Qnil);
}

Lisp_Object
composition_gstring_put_cache (gstring, len)
     Lisp_Object gstring;
     int len;
{
  struct Lisp_Hash_Table *h = XHASH_TABLE (gstring_hash_table);
  unsigned hash;
  Lisp_Object header, copy;
  int i;

  header = LGSTRING_HEADER (gstring);
  hash = h->hashfn (h, header);
  if (len < 0)
    {
      len = LGSTRING_GLYPH_LEN (gstring);
      for (i = 0; i < len; i++)
	if (NILP (LGSTRING_GLYPH (gstring, i)))
	  break;
      len = i;
    }
      
  copy = Fmake_vector (make_number (len + 2), Qnil);
  LGSTRING_SET_HEADER (copy, Fcopy_sequence (header));
  for (i = 0; i < len; i++)
    LGSTRING_SET_GLYPH (copy, i, Fcopy_sequence (LGSTRING_GLYPH (gstring, i)));
  i = hash_put (h, LGSTRING_HEADER (copy), copy, hash);
  LGSTRING_SET_ID (copy, make_number (i));
  return copy;
}

Lisp_Object
composition_gstring_from_id (id)
     int id;
{
  struct Lisp_Hash_Table *h = XHASH_TABLE (gstring_hash_table);

  return HASH_VALUE (h, id);
}

static Lisp_Object fill_gstring_header P_ ((Lisp_Object, Lisp_Object,
					    Lisp_Object, Lisp_Object,
					    Lisp_Object));

int
composition_gstring_p (gstring)
     Lisp_Object gstring;
{
  Lisp_Object header;
  int i;

  if (! VECTORP (gstring) || ASIZE (gstring) < 2)
    return 0;
  header = LGSTRING_HEADER (gstring);
  if (! VECTORP (header) || ASIZE (header) < 2)
    return 0;
  if (! NILP (LGSTRING_FONT (gstring))
      && ! FONT_OBJECT_P (LGSTRING_FONT (gstring)))
    return 0;
  for (i = 1; i < ASIZE (LGSTRING_HEADER (gstring)); i++)
    if (! NATNUMP (AREF (LGSTRING_HEADER (gstring), i)))
      return 0;
  if (! NILP (LGSTRING_ID (gstring)) && ! NATNUMP (LGSTRING_ID (gstring)))
    return 0;
  for (i = 0; i < LGSTRING_GLYPH_LEN (gstring); i++)
    {
      Lisp_Object glyph = LGSTRING_GLYPH (gstring, i);
      if (NILP (glyph))
	break;
      if (! VECTORP (glyph) || ASIZE (glyph) != LGLYPH_SIZE)
	return 0;
    }
  return 1;
}

int
composition_gstring_width (gstring, from, to, metrics)
     Lisp_Object gstring;
     int from, to;
     struct font_metrics *metrics;
{
  Lisp_Object *glyph;
  int width = 0;

  if (metrics)
    {
      Lisp_Object font_object = LGSTRING_FONT (gstring);
      struct font *font = XFONT_OBJECT (font_object);

      metrics->ascent = font->ascent;
      metrics->descent = font->descent;
      metrics->width = metrics->lbearing = metrics->rbearing = 0;
    }
  for (glyph = &LGSTRING_GLYPH (gstring, from); from < to; from++, glyph++)
    {
      int x;

      if (NILP (LGLYPH_ADJUSTMENT (*glyph)))
	width += LGLYPH_WIDTH (*glyph);
      else
	width += LGLYPH_WADJUST (*glyph);
      if (metrics)
	{
	  x = metrics->width + LGLYPH_LBEARING (*glyph) + LGLYPH_XOFF (*glyph);
	  if (metrics->lbearing > x)
	    metrics->lbearing = x;
	  x = metrics->width + LGLYPH_RBEARING (*glyph) + LGLYPH_XOFF (*glyph);
	  if (metrics->rbearing < x)
	    metrics->rbearing = x;
	  metrics->width = width;
	  x = LGLYPH_ASCENT (*glyph) - LGLYPH_YOFF (*glyph);
	  if (metrics->ascent < x)
	    metrics->ascent = x;
	  x = LGLYPH_DESCENT (*glyph) - LGLYPH_YOFF (*glyph);
	  if (metrics->descent < x)
	    metrics->descent = x;
	}
    }
  return width;
}


static Lisp_Object gstring_work;
static Lisp_Object gstring_work_headers;

static Lisp_Object
fill_gstring_header (header, start, end, font_object, string)
     Lisp_Object header, start, end, font_object, string;
{
  EMACS_INT from, to, from_byte;
  EMACS_INT len, i;

  if (NILP (string))
    {
      if (NILP (current_buffer->enable_multibyte_characters))
	error ("Attempt to shape unibyte text");
      validate_region (&start, &end);
      from = XFASTINT (start);
      to = XFASTINT (end);
      from_byte = CHAR_TO_BYTE (from);
    }
  else
    {
      CHECK_STRING (string);
      if (! STRING_MULTIBYTE (current_buffer->enable_multibyte_characters))
	error ("Attempt to shape unibyte text");
      CHECK_NATNUM (start);
      from = XINT (start);
      CHECK_NATNUM (end);
      to = XINT (end);
      if (from < 0 || from > to || to > SCHARS (string))
	args_out_of_range_3 (string, start, end);
      from_byte = string_char_to_byte (string, from);
    }

  len = to - from;
  if (len == 0)
    error ("Attempt to shape zero-length text");
  if (VECTORP (header))
    {
      if (ASIZE (header) != len + 1)
	args_out_of_range (header, make_number (len + 1));
    }
  else
    {
      if (len <= 8)
	header = AREF (gstring_work_headers, len - 1);
      else
	header = Fmake_vector (make_number (len + 1), Qnil);
    }

  ASET (header, 0, font_object);
  for (i = 0; i < len; i++)
    {
      int c;

      if (NILP (string))
	FETCH_CHAR_ADVANCE_NO_CHECK (c, from, from_byte);
      else
	FETCH_STRING_CHAR_ADVANCE_NO_CHECK (c, string, from, from_byte);
      ASET (header, i + 1, make_number (c));
    }
  return header;
}

extern void font_fill_lglyph_metrics P_ ((Lisp_Object, Lisp_Object));

static void
fill_gstring_body (gstring)
     Lisp_Object gstring;
{
  Lisp_Object font_object = LGSTRING_FONT (gstring);
  Lisp_Object header = AREF (gstring, 0);
  EMACS_INT len = LGSTRING_CHAR_LEN (gstring);
  EMACS_INT i;

  for (i = 0; i < len; i++)
    {
      Lisp_Object g = LGSTRING_GLYPH (gstring, i);
      EMACS_INT c = XINT (AREF (header, i + 1));

      if (NILP (g))
	{
	  g = LGLYPH_NEW ();
	  LGSTRING_SET_GLYPH (gstring, i, g);
	}
      LGLYPH_SET_FROM (g, i);
      LGLYPH_SET_TO (g, i);
      LGLYPH_SET_CHAR (g, c);
      if (! NILP (font_object))
	{
	  font_fill_lglyph_metrics (g, font_object);
	}
      else
	{
	  int width = XFASTINT (CHAR_TABLE_REF (Vchar_width_table, c));

	  LGLYPH_SET_CODE (g, c);
	  LGLYPH_SET_LBEARING (g, 0);
	  LGLYPH_SET_RBEARING (g, width);
	  LGLYPH_SET_WIDTH (g, width);
	  LGLYPH_SET_ASCENT (g, 1);
	  LGLYPH_SET_DESCENT (g, 0);
	}
      LGLYPH_SET_ADJUSTMENT (g, Qnil);
    }
  if (i < LGSTRING_GLYPH_LEN (gstring))
    LGSTRING_SET_GLYPH (gstring, i, Qnil);
}

EXFUN (Fre_search_forward, 4);

/* Try to compose the characters at CHARPOS according to CFT_ELEMENT
   which is an element of composition-fucntion-table (which see).
   LIMIT limits the characters to compose.  STRING, if not nil, is a
   target string.  WIN is a window where the characters are being
   displayed.  */

static Lisp_Object
autocmp_chars (cft_element, charpos, bytepos, limit, win, face, string)
     Lisp_Object cft_element;
     EMACS_INT charpos, bytepos, limit;
     struct window *win;
     struct face *face;
     Lisp_Object string;
{
  int count = SPECPDL_INDEX ();
  FRAME_PTR f = XFRAME (win->frame);
  Lisp_Object pos = make_number (charpos);
  EMACS_INT pt = PT, pt_byte = PT_BYTE;
  int lookback;
  
  record_unwind_save_match_data ();
  for (lookback = -1; CONSP (cft_element); cft_element = XCDR (cft_element))
    {
      Lisp_Object elt = XCAR (cft_element);
      Lisp_Object re;
      Lisp_Object font_object = Qnil, gstring;
      EMACS_INT to;

      if (! VECTORP (elt) || ASIZE (elt) != 3)
	continue;
      if (lookback < 0)
	lookback = XFASTINT (AREF (elt, 1));
      else if (lookback != XFASTINT (AREF (elt, 1)))
	break;
      re = AREF (elt, 0);
      if (NILP (string))
	TEMP_SET_PT_BOTH (charpos, bytepos);
      if (NILP (re)
	  || (STRINGP (re)
	      && (STRINGP (string)
		  ? EQ (Fstring_match (re, string, pos), pos)
		  : (! NILP (Fre_search_forward (re, make_number (limit), Qt, Qnil))
		     && EQ (Fmatch_beginning (make_number (0)), pos)))))
	{
	  to = (NILP (re) ? charpos + 1 : XINT (Fmatch_end (make_number (0))));
#ifdef HAVE_WINDOW_SYSTEM
	  if (FRAME_WINDOW_P (f))
	    {
	      font_object = font_range (charpos, &to, win, face, string);
	      if (! FONT_OBJECT_P (font_object))
		{
		  if (NILP (string))
		    TEMP_SET_PT_BOTH (pt, pt_byte);
		  return unbind_to (count, Qnil);
		}
	    }
#endif	/* not HAVE_WINDOW_SYSTEM */
	  gstring = Fcomposition_get_gstring (pos, make_number (to),
					      font_object, string);
	  if (NILP (LGSTRING_ID (gstring)))
	    {
	      Lisp_Object args[6];

	      args[0] = Vauto_composition_function;
	      args[1] = AREF (elt, 2);
	      args[2] = pos;
	      args[3] = make_number (to);
	      args[4] = font_object;
	      args[5] = string;
	      gstring = safe_call (6, args);
	    }
	  if (NILP (string))
	    TEMP_SET_PT_BOTH (pt, pt_byte);
	  return unbind_to (count, gstring);
	}
    }
  if (NILP (string))
    TEMP_SET_PT_BOTH (pt, pt_byte);
  return unbind_to (count, Qnil);
}


/* Update cmp_it->stop_pos to the next position after CHARPOS (and
   BYTEPOS) where character composition may happen.  If BYTEPOS is
   negative, compoute it.  If it is a static composition, set
   cmp_it->ch to -1.  Otherwise, set cmp_it->ch to the character that
   triggers a automatic composition.  */

void
composition_compute_stop_pos (cmp_it, charpos, bytepos, endpos, string)
     struct composition_it *cmp_it;
     EMACS_INT charpos, bytepos, endpos;
     Lisp_Object string;
{
  EMACS_INT start, end, c;
  Lisp_Object prop, val;
  /* This is from forward_to_next_line_start in xdisp.c.  */
  const int MAX_NEWLINE_DISTANCE = 500;

  if (endpos > charpos + MAX_NEWLINE_DISTANCE)
    endpos = charpos + MAX_NEWLINE_DISTANCE;
  cmp_it->stop_pos = endpos;
  cmp_it->id = -1;
  cmp_it->ch = -2;
  if (find_composition (charpos, endpos, &start, &end, &prop, string)
      && COMPOSITION_VALID_P (start, end, prop))
    {
      cmp_it->stop_pos = endpos = start;
      cmp_it->ch = -1;
    }
  if (NILP (current_buffer->enable_multibyte_characters)
      || ! FUNCTIONP (Vauto_composition_function))
    return;
  if (bytepos < 0)
    {
      if (STRINGP (string))
	bytepos = string_char_to_byte (string, charpos);
      else
	bytepos = CHAR_TO_BYTE (charpos);
    }

  start = charpos;
  while (charpos < endpos)
    {
      if (STRINGP (string))
	FETCH_STRING_CHAR_ADVANCE (c, string, charpos, bytepos);
      else
	FETCH_CHAR_ADVANCE (c, charpos, bytepos);
      if (c == '\n')
	break;
      val = CHAR_TABLE_REF (Vcomposition_function_table, c);
      if (! NILP (val))
	{
	  Lisp_Object elt;

	  for (; CONSP (val); val = XCDR (val))
	    {
	      elt = XCAR (val);
	      if (VECTORP (elt) && ASIZE (elt) == 3 && NATNUMP (AREF (elt, 1))
		  && charpos - 1 - XFASTINT (AREF (elt, 1)) >= start)
		break;
	    }
	  if (CONSP (val))
	    {
	      cmp_it->lookback = XFASTINT (AREF (elt, 1));
	      cmp_it->stop_pos = charpos - 1 - cmp_it->lookback;
	      cmp_it->ch = c;
	      return;
	    }
	}
    }
  cmp_it->stop_pos = charpos;
  cmp_it->ch = -2;
}

/* Check if the character at CHARPOS (and BYTEPOS) is composed
   (possibly with the following charaters) on window W.  ENDPOS limits
   characters to be composed.  FACE, in non-NULL, is a base face of
   the character.  If STRING is not nil, it is a string containing the
   character to check, and CHARPOS and BYTEPOS are indices in the
   string.  In that case, FACE must not be NULL.

   If the character is composed, setup members of CMP_IT (id, nglyphs,
   and from), and return 1.  Otherwise, update CMP_IT->stop_pos, and
   return 0.  */

int
composition_reseat_it (cmp_it, charpos, bytepos, endpos, w, face, string)
     struct composition_it *cmp_it;
     EMACS_INT charpos, bytepos, endpos;
     struct window *w;
     struct face *face;
     Lisp_Object string;
{
  if (cmp_it->ch == -2)
    {
      composition_compute_stop_pos (cmp_it, charpos, bytepos, endpos, string);
      if (cmp_it->ch == -2)
	return 0;
    }

  if (cmp_it->ch < 0)
    {
      /* We are looking at a static composition.  */
      EMACS_INT start, end;
      Lisp_Object prop;

      find_composition (charpos, -1, &start, &end, &prop, string);
      cmp_it->id = get_composition_id (charpos, bytepos, end - start,
				       prop, string);
      if (cmp_it->id < 0)
	goto no_composition;
      cmp_it->nchars = end - start;
      cmp_it->nglyphs = composition_table[cmp_it->id]->glyph_len;
    }
  else if (w)
    {
      Lisp_Object val, elt;
      int i;

      val = CHAR_TABLE_REF (Vcomposition_function_table, cmp_it->ch);
      for (; CONSP (val); val = XCDR (val))
	{
	  elt = XCAR (val);
	  if (cmp_it->lookback == XFASTINT (AREF (elt, 1)))
	    break;
	}
      if (NILP (val))
	goto no_composition;

      val = autocmp_chars (val, charpos, bytepos, endpos, w, face, string);
      if (! composition_gstring_p (val))
	goto no_composition;
      if (NILP (LGSTRING_ID (val)))
	val = composition_gstring_put_cache (val, -1);
      cmp_it->id = XINT (LGSTRING_ID (val));
      for (i = 0; i < LGSTRING_GLYPH_LEN (val); i++)
	if (NILP (LGSTRING_GLYPH (val, i)))
	  break;
      cmp_it->nglyphs = i;
    }
  else
    goto no_composition;
  cmp_it->from = 0;
  return 1;

 no_composition:
  charpos++;
  if (STRINGP (string))
    bytepos += MULTIBYTE_LENGTH_NO_CHECK (SDATA (string) + bytepos);
  else
    INC_POS (bytepos);
  composition_compute_stop_pos (cmp_it, charpos, bytepos, endpos, string);
  return 0;
}

int
composition_update_it (cmp_it, charpos, bytepos, string)
     struct composition_it *cmp_it;
     EMACS_INT charpos, bytepos;
     Lisp_Object string;
{
  int i, c;

  if (cmp_it->ch < 0)
    {
      struct composition *cmp = composition_table[cmp_it->id];

      cmp_it->to = cmp_it->nglyphs;
      if (cmp_it->nglyphs == 0)
	c = -1;
      else
	{
	  for (i = 0; i < cmp->glyph_len; i++)
	    if ((c = COMPOSITION_GLYPH (cmp, i)) != '\t')
	      break;
	  if (c == '\t')
	    c = ' ';
	}
      cmp_it->width = cmp->width;
    }
  else
    {
      Lisp_Object gstring = composition_gstring_from_id (cmp_it->id);

      if (cmp_it->nglyphs == 0)
	{
	  c = -1;
	  cmp_it->nchars = LGSTRING_CHAR_LEN (gstring);
	  cmp_it->width = 0;
	}
      else
	{
	  Lisp_Object glyph = LGSTRING_GLYPH (gstring, cmp_it->from);
	  int from = LGLYPH_FROM (glyph);

	  c = XINT (LGSTRING_CHAR (gstring, from));
	  cmp_it->nchars = LGLYPH_TO (glyph) - from + 1;
	  cmp_it->width = (LGLYPH_WIDTH (glyph) > 0
			   ? CHAR_WIDTH (LGLYPH_CHAR (glyph)) : 0);
	  for (cmp_it->to = cmp_it->from + 1; cmp_it->to < cmp_it->nglyphs;
	       cmp_it->to++)
	    {
	      glyph = LGSTRING_GLYPH (gstring, cmp_it->to);
	      if (LGLYPH_FROM (glyph) != from)
		break;
	      if (LGLYPH_WIDTH (glyph) > 0)
		cmp_it->width += CHAR_WIDTH (LGLYPH_CHAR (glyph));
	    }
	}
    }

  charpos += cmp_it->nchars;
  if (STRINGP (string))
    cmp_it->nbytes = string_char_to_byte (string, charpos) - bytepos;
  else
    cmp_it->nbytes = CHAR_TO_BYTE (charpos) - bytepos;
  return c;
}


struct position_record
{
  EMACS_INT pos, pos_byte;
  unsigned char *p;
};

/* Update the members of POSTION to the next character boundary.  */
#define FORWARD_CHAR(POSITION, STOP)					\
  do {									\
    (POSITION).pos++;							\
    if ((POSITION).pos == (STOP))					\
      {									\
	(POSITION).p = GAP_END_ADDR;					\
	(POSITION).pos_byte = GPT_BYTE;					\
      }									\
    else								\
      {									\
	(POSITION).pos_byte += BYTES_BY_CHAR_HEAD (*((POSITION).p));	\
	(POSITION).p += BYTES_BY_CHAR_HEAD (*((POSITION).p));		\
      }									\
  } while (0)

/* Update the members of POSTION to the previous character boundary.  */
#define BACKWARD_CHAR(POSITION, STOP)		\
  do {						\
    if ((POSITION).pos == STOP)			\
      (POSITION).p = GPT_ADDR;			\
    do {					\
      (POSITION).pos_byte--;			\
      (POSITION).p--;				\
    } while (! CHAR_HEAD_P (*((POSITION).p)));	\
    (POSITION).pos--;				\
  } while (0)

static Lisp_Object _work_val;
static int _work_char;

/* 1 iff the character C is composable.  */
#define CHAR_COMPOSABLE_P(C)					\
  (_work_val = CHAR_TABLE_REF (Vunicode_category_table, (C)),	\
   (SYMBOLP (_work_val)						\
    && (_work_char = SDATA (SYMBOL_NAME (_work_val))[0]) != 'C'	\
    && _work_char != 'Z'))

/* This is like find_composition, but find an automatic composition
   instead.  If found, set *GSTRING to the glyph-string representing
   the composition, and return 1.  Otherwise, return 0.  */

static int
find_automatic_composition (pos, limit, start, end, gstring, string)
     EMACS_INT pos, limit, *start, *end;
     Lisp_Object *gstring, string;
{
  EMACS_INT head, tail, stop;
  struct position_record orig, cur, check, prev;
  Lisp_Object check_val, val, elt;
  int check_lookback;
  int c;
  Lisp_Object window;
  struct window *w;

  window = Fget_buffer_window (Fcurrent_buffer (), Qnil);
  if (NILP (window))
    return 0;
  w = XWINDOW (window);

  orig.pos = pos;
  if (NILP (string))
    {
      head = BEGV, tail = ZV, stop = GPT;
      orig.pos_byte = CHAR_TO_BYTE (orig.pos);
      orig.p = BYTE_POS_ADDR (orig.pos_byte);
    }
  else
    {
      head = 0, tail = SCHARS (string), stop = -1;
      orig.pos_byte = string_char_to_byte (string, orig.pos);
      orig.p = SDATA (string) + orig.pos_byte;
    }
  if (limit < pos)
    {
      head = max (head, limit);
      tail = min (tail, pos + 3);
    }
  else
    {
      tail = min (tail, limit + 3);
    }
  cur = orig;

 retry:
  check_val = Qnil;
  /* At first, check if POS is compoable.  */
  c = STRING_CHAR (cur.p, 0);
  if (! CHAR_COMPOSABLE_P (c))
    {
      if (limit < 0)
	return 0;
      if (limit >= cur.pos)
	goto search_forward;
    }
  else
    {
      val = CHAR_TABLE_REF (Vcomposition_function_table, c);
      if (! NILP (val))
	check_val = val, check = cur;
      else
	while (cur.pos + 1 < tail)
	  {
	    FORWARD_CHAR (cur, stop);
	    c = STRING_CHAR (cur.p, 0);
	    if (! CHAR_COMPOSABLE_P (c))
	      break;
	    val = CHAR_TABLE_REF (Vcomposition_function_table, c);
	    if (NILP (val))
	      continue;
	    check_val = val, check = cur;
	    break;
	  }
      cur = orig;
    }
  /* Rewind back to the position where we can safely search forward
     for compositions.  */
  while (cur.pos > head)
    {
      BACKWARD_CHAR (cur, stop);
      c = STRING_CHAR (cur.p, 0);
      if (! CHAR_COMPOSABLE_P (c))
	break;
      val = CHAR_TABLE_REF (Vcomposition_function_table, c);
      if (! NILP (val))
	check_val = val, check = cur;
    }
  prev = cur;
  /* Now search forward.  */
 search_forward:  
  *gstring = Qnil;
  if (! NILP (check_val) || limit >= orig.pos)
    {
      if (NILP (check_val))
	cur = orig;
      else
	cur = check;
      while (cur.pos < tail)
	{
	  int need_adjustment = 0;

	  if (NILP (check_val))
	    {
	      c = STRING_CHAR (cur.p, 0);
	      check_val = CHAR_TABLE_REF (Vcomposition_function_table, c);
	    }
	  for (; CONSP (check_val); check_val = XCDR (check_val))
	    {
	      elt = XCAR (check_val);
	      if (VECTORP (elt) && ASIZE (elt) == 3 && NATNUMP (AREF (elt, 1))
		  && cur.pos - XFASTINT (AREF (elt, 1)) >= head)
		{
		  check.pos = cur.pos - XFASTINT (AREF (elt, 1));
		  if (check.pos == cur.pos)
		    check.pos_byte = cur.pos_byte;
		  else
		    check.pos_byte = CHAR_TO_BYTE (check.pos);
		  val = autocmp_chars (check_val, check.pos, check.pos_byte,
				       tail, w, NULL, string);
		  need_adjustment = 1;
		  if (! NILP (val))
		    {
		      *gstring = val;
		      *start = check.pos;
		      *end = check.pos + LGSTRING_CHAR_LEN (*gstring);
		      if (*start <= orig.pos ? *end > orig.pos
			  : limit >= orig.pos)
			return 1;
		      cur.pos = *end;
		      cur.pos_byte = CHAR_TO_BYTE (cur.pos);
		      break;
		    }
		}
	    }
	  if (need_adjustment)
	    {
	      /* As we have called Lisp, there's a possibilily that
		 buffer/string is relocated.  */
	      if (NILP (string))
		cur.p  = BYTE_POS_ADDR (cur.pos_byte);
	      else
		cur.p = SDATA (string) + cur.pos_byte;
	    }
	  if (! CONSP (check_val))
	    FORWARD_CHAR (cur, stop);
	  check_val = Qnil;
	}
    }
  if (! NILP (*gstring))
    return (limit >= 0 || (*start <= orig.pos && *end > orig.pos));
  if (limit >= 0 && limit < orig.pos && prev.pos > head)
    {
      cur = prev;
      BACKWARD_CHAR (cur, stop);
      orig = cur;
      tail = orig.pos;
      goto retry;
    }
  return 0;
}

int
composition_adjust_point (last_pt)
     EMACS_INT last_pt;
{
  EMACS_INT charpos, bytepos, startpos, beg, end, pos;
  Lisp_Object val;
  int i;

  if (PT == BEGV || PT == ZV)
    return PT;

  /* At first check the static composition. */
  if (get_property_and_range (PT, Qcomposition, &val, &beg, &end, Qnil)
      && COMPOSITION_VALID_P (beg, end, val)
      && beg < PT /* && end > PT   <- It's always the case.  */
      && (last_pt <= beg || last_pt >= end))
    return (PT < last_pt ? beg : end);

  if (NILP (current_buffer->enable_multibyte_characters)
      || ! FUNCTIONP (Vauto_composition_function))
    return PT;

  /* Next check the automatic composition.  */
  if (! find_automatic_composition (PT, -1, &beg, &end, &val, Qnil)
      || beg == PT)
    return PT;
  for (i = 0; i < LGSTRING_GLYPH_LEN (val); i++)
    {
      Lisp_Object glyph = LGSTRING_GLYPH (val, i);

      if (NILP (glyph))
	break;
      if (beg + LGLYPH_FROM (glyph) == PT)
	return PT;
      if (beg + LGLYPH_TO (glyph) >= PT)
	return (PT < last_pt
		? beg + LGLYPH_FROM (glyph)
		: beg + LGLYPH_TO (glyph) + 1);
    }
  return PT;
}

DEFUN ("composition-get-gstring", Fcomposition_get_gstring,
       Scomposition_get_gstring, 4, 4, 0,
       doc: /* Return a glyph-string for characters between FROM and TO.
If the glhph string is for graphic display, FONT-OBJECT must be
a font-object to use for those characters.
Otherwise (for terminal display), FONT-OBJECT must be nil.

If the optional 4th argument STRING is not nil, it is a string
containing the target characters between indices FROM and TO.

A glhph-string is a vector containing information about how to display
specific character sequence.  The format is:
   [HEADER ID GLYPH ...]

HEADER is a vector of this form:
    [FONT-OBJECT CHAR ...]
where
    FONT-OBJECT is a font-object for all glyphs in the glyph-string,
    or nil if not yet decided.
    CHARs are characters to be composed by GLYPHs.

ID is an identification number of the glyph-string.  It may be nil if
not yet shaped.

GLYPH is a vector whose elements has this form:
    [ FROM-IDX TO-IDX C CODE WIDTH LBEARING RBEARING ASCENT DESCENT
      [ [X-OFF Y-OFF WADJUST] | nil] ]
where
    FROM-IDX and TO-IDX are used internally and should not be touched.
    C is the character of the glyph.
    CODE is the glyph-code of C in FONT-OBJECT.
    WIDTH thru DESCENT are the metrics (in pixels) of the glyph.
    X-OFF and Y-OFF are offests to the base position for the glyph.
    WADJUST is the adjustment to the normal width of the glyph.

If GLYPH is nil, the remaining elements of the glhph-string vector
must be ignore.  */)
     (from, to, font_object, string)
     Lisp_Object font_object, from, to, string;
{
  Lisp_Object gstring, header;

  if (! NILP (font_object))
    CHECK_FONT_OBJECT (font_object);
  header = fill_gstring_header (Qnil, from, to, font_object, string);
  gstring = gstring_lookup_cache (header);
  if (! NILP (gstring))
    return gstring;
  LGSTRING_SET_HEADER (gstring_work, header);
  LGSTRING_SET_ID (gstring_work, Qnil);
  fill_gstring_body (gstring_work);
  return gstring_work;
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
  Lisp_Object prop, tail, gstring;
  EMACS_INT start, end, from, to;
  int id;

  CHECK_NUMBER_COERCE_MARKER (pos);
  from = XINT (pos);
  if (!NILP (limit))
    {
      CHECK_NUMBER_COERCE_MARKER (limit);
      to = XINT (limit);
    }
  else
    to = -1;

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

  if (!find_composition (from, to, &start, &end, &prop, string))
    {
      if (!NILP (current_buffer->enable_multibyte_characters)
	  && FUNCTIONP (Vauto_composition_function)
	  && find_automatic_composition (from, to, &start, &end, &gstring,
					 string))
	return list3 (make_number (start), make_number (end), gstring);
      return Qnil;
    }
  if ((end <= XINT (pos) || start > XINT (pos)))
    {
      EMACS_INT s, e;

      if (find_automatic_composition (from, to, &s, &e, &gstring, string)
	  && (e <= XINT (pos) ? e > end : s < start))
	return list3 (make_number (start), make_number (end), gstring);
    }
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
  int i;

  Qcomposition = intern ("composition");
  staticpro (&Qcomposition);

  /* Make a hash table for static composition.  */
  {
    Lisp_Object args[6];
    extern Lisp_Object QCsize;

    args[0] = QCtest;
    args[1] = Qequal;
    args[2] = QCweakness;
    /* We used to make the hash table weak so that unreferenced
       compositions can be garbage-collected.  But, usually once
       created compositions are repeatedly used in an Emacs session,
       and thus it's not worth to save memory in such a way.  So, we
       make the table not weak.  */
    args[3] = Qnil;
    args[4] = QCsize;
    args[5] = make_number (311);
    composition_hash_table = Fmake_hash_table (6, args);
    staticpro (&composition_hash_table);
  }

  /* Make a hash table for glyph-string.  */
  {
    Lisp_Object args[6];
    extern Lisp_Object QCsize;

    args[0] = QCtest;
    args[1] = Qequal;
    args[2] = QCweakness;
    args[3] = Qnil;
    args[4] = QCsize;
    args[5] = make_number (311);
    gstring_hash_table = Fmake_hash_table (6, args);
    staticpro (&gstring_hash_table);
  }

  staticpro (&gstring_work_headers);
  gstring_work_headers = Fmake_vector (make_number (8), Qnil);
  for (i = 0; i < 8; i++)
    ASET (gstring_work_headers, i, Fmake_vector (make_number (i + 2), Qnil));
  staticpro (&gstring_work);
  gstring_work = Fmake_vector (make_number (10), Qnil);

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

  Qauto_composed = intern ("auto-composed");
  staticpro (&Qauto_composed);

  Qauto_composition_function = intern ("auto-composition-function");
  staticpro (&Qauto_composition_function);

  DEFVAR_LISP ("auto-composition-function", &Vauto_composition_function,
	       doc: /* Function to call to compose characters automatically.
The function is called from the display routine with four arguments,
FROM, TO, WINDOW, and STRING.

If STRING is nil, the function must compose characters in the region
between FROM and TO in the current buffer.

Otherwise, STRING is a string, and FROM and TO are indices into the
string.  In this case, the function must compose characters in the
string.  */);
  Vauto_composition_function = Qnil;

  DEFVAR_LISP ("composition-function-table", &Vcomposition_function_table,
	       doc: /* Char-able of functions for automatic character composition.
For each character that has to be composed automatically with
preceding and/or following characters, this char-table contains
a function to call to compose that character.

The element at index C in the table, if non-nil, is a list of
this form: ([PATTERN PREV-CHARS FUNC] ...)

PATTERN is a regular expression with which C and the surrounding
characters must match.

PREV-CHARS is a number of characters before C to check the
matching with PATTERN.  If it is 0, PATTERN must match with C and
the following characters.  If it is 1, PATTERN must match with a
character before C and the following characters.

If PREV-CHARS is 0, PATTERN can be nil, which means that the
single character C should be composed.

FUNC is a function to return a glyph-string representing a
composition of the characters matching with PATTERN.  It is
called with one argument GSTRING.

GSTRING is a template of a glyph-string to return.  It is already
filled with a proper header for the characters to compose, and
glyphs corresponding to those characters one by one.  The
function must return a new glyph-string of the same header as
GSTRING, or modify GSTRING itself and return it.

See also the documentation of `auto-composition-mode'.  */);
  Vcomposition_function_table = Fmake_char_table (Qnil, Qnil);

  defsubr (&Scompose_region_internal);
  defsubr (&Scompose_string_internal);
  defsubr (&Sfind_composition_internal);
  defsubr (&Scomposition_get_gstring);
}

/* arch-tag: 79cefaf8-ca48-4eed-97e5-d5afb290d272
   (do not change this comment) */
