/* -*- coding: utf-8 -*- */
/* GNU Emacs case conversion functions.

Copyright (C) 1985, 1994, 1997-1999, 2001-2018 Free Software Foundation,
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

#include "lisp.h"
#include "character.h"
#include "buffer.h"
#include "commands.h"
#include "syntax.h"
#include "composite.h"
#include "keymap.h"

enum case_action {CASE_UP, CASE_DOWN, CASE_CAPITALIZE, CASE_CAPITALIZE_UP};

/* State for casing individual characters.  */
struct casing_context
{
  /* A char-table with title-case character mappings or nil.  Non-nil implies
     flag is CASE_CAPITALIZE or CASE_CAPITALIZE_UP.  */
  Lisp_Object titlecase_char_table;

  /* The unconditional special-casing Unicode property char tables for upper
     casing, lower casing and title casing respectively.  */
  Lisp_Object specialcase_char_tables[3];

  /* User-requested action.  */
  enum case_action flag;

  /* If true, the function operates on a buffer as opposed to a string
     or character.  When run on a buffer, syntax_prefix_flag_p is
     taken into account when determining whether the context is within
     a word.  */
  bool inbuffer;

  /* Whether the context is within a word.  */
  bool inword;
};

/* Initialize CTX structure for casing characters.  */
static void
prepare_casing_context (struct casing_context *ctx,
			enum case_action flag, bool inbuffer)
{
  ctx->flag = flag;
  ctx->inbuffer = inbuffer;
  ctx->inword = false;
  ctx->titlecase_char_table
    = (flag < CASE_CAPITALIZE ? Qnil
       : uniprop_table (Qtitlecase));
  ctx->specialcase_char_tables[CASE_UP]
    = (flag == CASE_DOWN ? Qnil
       : uniprop_table (Qspecial_uppercase));
  ctx->specialcase_char_tables[CASE_DOWN]
    = (flag == CASE_UP ? Qnil
       : uniprop_table (Qspecial_lowercase));
  ctx->specialcase_char_tables[CASE_CAPITALIZE]
    = (flag < CASE_CAPITALIZE ? Qnil
       : uniprop_table (Qspecial_titlecase));

  /* If the case table is flagged as modified, rescan it.  */
  if (NILP (XCHAR_TABLE (BVAR (current_buffer, downcase_table))->extras[1]))
    Fset_case_table (BVAR (current_buffer, downcase_table));

  if (inbuffer && flag >= CASE_CAPITALIZE)
    SETUP_BUFFER_SYNTAX_TABLE ();	/* For syntax_prefix_flag_p.  */
}

struct casing_str_buf
{
  unsigned char data[max (6, MAX_MULTIBYTE_LENGTH)];
  unsigned char len_chars;
  unsigned char len_bytes;
};

/* Based on CTX, case character CH.  If BUF is NULL, return cased character.
   Otherwise, if BUF is non-NULL, save result in it and return whether the
   character has been changed.

   Since meaning of return value depends on arguments, it’s more convenient to
   use case_single_character or case_character instead.  */
static int
case_character_impl (struct casing_str_buf *buf,
		     struct casing_context *ctx, int ch)
{
  enum case_action flag;
  Lisp_Object prop;
  int cased;

  /* Update inword state */
  bool was_inword = ctx->inword;
  ctx->inword = SYNTAX (ch) == Sword &&
    (!ctx->inbuffer || was_inword || !syntax_prefix_flag_p (ch));

  /* Normalize flag so its one of CASE_UP, CASE_DOWN or CASE_CAPITALIZE.  */
  if (ctx->flag == CASE_CAPITALIZE)
    flag = ctx->flag - was_inword;
  else if (ctx->flag != CASE_CAPITALIZE_UP)
    flag = ctx->flag;
  else if (!was_inword)
    flag = CASE_CAPITALIZE;
  else
    {
      cased = ch;
      goto done;
    }

  /* Look through the special casing entries.  */
  if (buf && !NILP (ctx->specialcase_char_tables[flag]))
    {
      prop = CHAR_TABLE_REF (ctx->specialcase_char_tables[flag], ch);
      if (STRINGP (prop))
        {
          struct Lisp_String *str = XSTRING (prop);
          if (STRING_BYTES (str) <= sizeof buf->data)
	    {
	      buf->len_chars = str->u.s.size;
	      buf->len_bytes = STRING_BYTES (str);
	      memcpy (buf->data, str->u.s.data, buf->len_bytes);
	      return 1;
	    }
        }
    }

  /* Handle simple, one-to-one case.  */
  if (flag == CASE_DOWN)
    cased = downcase (ch);
  else
    {
      bool cased_is_set = false;
      if (!NILP (ctx->titlecase_char_table))
	{
	  prop = CHAR_TABLE_REF (ctx->titlecase_char_table, ch);
	  if (CHARACTERP (prop))
	    {
	      cased = XFASTINT (prop);
	      cased_is_set = true;
	    }
	}
      if (!cased_is_set)
	cased = upcase (ch);
    }

  /* And we’re done.  */
 done:
  if (!buf)
    return cased;
  buf->len_chars = 1;
  buf->len_bytes = CHAR_STRING (cased, buf->data);
  return cased != ch;
}

/* In Greek, lower case sigma has two forms: one when used in the middle and one
   when used at the end of a word.  Below is to help handle those cases when
   casing.

   The rule does not conflict with any other casing rules so while it is
   a conditional one, it is independent of language.  */

enum { GREEK_CAPITAL_LETTER_SIGMA = 0x03A3 }; /* Σ */
enum { GREEK_SMALL_LETTER_FINAL_SIGMA = 0x03C2 }; /* ς */

/* Based on CTX, case character CH accordingly.  Update CTX as necessary.
   Return cased character.

   Special casing rules (such as upcase(ﬁ) = FI) are not handled.  For
   characters whose casing results in multiple code points, the character is
   returned unchanged.  */
static inline int
case_single_character (struct casing_context *ctx, int ch)
{
  return case_character_impl (NULL, ctx, ch);
}

/* Save in BUF result of casing character CH.  Return whether casing changed the
   character.

   If not-NULL, NEXT points to the next character in the cased string.  If NULL,
   it is assumed current character is the last one being cased.  This is used to
   apply some rules which depend on proceeding state.

   This is like case_single_character but also handles one-to-many casing
   rules.  */
static bool
case_character (struct casing_str_buf *buf, struct casing_context *ctx,
		int ch, const unsigned char *next)
{
  bool was_inword = ctx->inword;
  bool changed = case_character_impl (buf, ctx, ch);

  /* If we have just down-cased a capital sigma and the next character no longer
     has a word syntax (i.e. current character is end of word), use final
     sigma.  */
  if (was_inword && ch == GREEK_CAPITAL_LETTER_SIGMA && changed
      && (!next || SYNTAX (STRING_CHAR (next)) != Sword))
    {
      buf->len_bytes = CHAR_STRING (GREEK_SMALL_LETTER_FINAL_SIGMA, buf->data);
      buf->len_chars = 1;
    }

  return changed;
}

static Lisp_Object
do_casify_natnum (struct casing_context *ctx, Lisp_Object obj)
{
  int flagbits = (CHAR_ALT | CHAR_SUPER | CHAR_HYPER
		  | CHAR_SHIFT | CHAR_CTL | CHAR_META);
  int ch = XFASTINT (obj);

  /* If the character has higher bits set above the flags, return it unchanged.
     It is not a real character.  */
  if (UNSIGNED_CMP (ch, >, flagbits))
    return obj;

  int flags = ch & flagbits;
  ch = ch & ~flagbits;

  /* FIXME: Even if enable-multibyte-characters is nil, we may manipulate
     multibyte chars.  This means we have a bug for latin-1 chars since when we
     receive an int 128-255 we can't tell whether it's an eight-bit byte or
     a latin-1 char.  */
  bool multibyte = (ch >= 256
		    || !NILP (BVAR (current_buffer,
				    enable_multibyte_characters)));
  if (! multibyte)
    MAKE_CHAR_MULTIBYTE (ch);
  int cased = case_single_character (ctx, ch);
  if (cased == ch)
    return obj;

  if (! multibyte)
    MAKE_CHAR_UNIBYTE (cased);
  return make_natnum (cased | flags);
}

static Lisp_Object
do_casify_multibyte_string (struct casing_context *ctx, Lisp_Object obj)
{
  /* Verify that ‘data’ is the first member of struct casing_str_buf
     so that when casting char * to struct casing_str_buf *, the
     representation of the character is at the beginning of the
     buffer.  This is why we don’t need a separate struct
     casing_str_buf object, and can write directly to the destination.  */
  verify (offsetof (struct casing_str_buf, data) == 0);

  ptrdiff_t size = SCHARS (obj), n;
  USE_SAFE_ALLOCA;
  if (INT_MULTIPLY_WRAPV (size, MAX_MULTIBYTE_LENGTH, &n)
      || INT_ADD_WRAPV (n, sizeof (struct casing_str_buf), &n))
    n = PTRDIFF_MAX;
  unsigned char *dst = SAFE_ALLOCA (n);
  unsigned char *dst_end = dst + n;
  unsigned char *o = dst;

  const unsigned char *src = SDATA (obj);

  for (n = 0; size; --size)
    {
      if (dst_end - o < sizeof (struct casing_str_buf))
	string_overflow ();
      int ch = STRING_CHAR_ADVANCE (src);
      case_character ((struct casing_str_buf *) o, ctx, ch,
		      size > 1 ? src : NULL);
      n += ((struct casing_str_buf *) o)->len_chars;
      o += ((struct casing_str_buf *) o)->len_bytes;
    }
  eassert (o <= dst_end);
  obj = make_multibyte_string ((char *) dst, n, o - dst);
  SAFE_FREE ();
  return obj;
}

static Lisp_Object
do_casify_unibyte_string (struct casing_context *ctx, Lisp_Object obj)
{
  ptrdiff_t i, size = SCHARS (obj);
  int ch, cased;

  obj = Fcopy_sequence (obj);
  for (i = 0; i < size; i++)
    {
      ch = SREF (obj, i);
      MAKE_CHAR_MULTIBYTE (ch);
      cased = case_single_character (ctx, ch);
      if (ch == cased)
	continue;
      MAKE_CHAR_UNIBYTE (cased);
      /* If the char can't be converted to a valid byte, just don't
	 change it.  */
      if (cased >= 0 && cased < 256)
	SSET (obj, i, cased);
    }
  return obj;
}

static Lisp_Object
casify_object (enum case_action flag, Lisp_Object obj)
{
  struct casing_context ctx;
  prepare_casing_context (&ctx, flag, false);

  if (NATNUMP (obj))
    return do_casify_natnum (&ctx, obj);
  else if (!STRINGP (obj))
    wrong_type_argument (Qchar_or_string_p, obj);
  else if (!SCHARS (obj))
    return obj;
  else if (STRING_MULTIBYTE (obj))
    return do_casify_multibyte_string (&ctx, obj);
  else
    return do_casify_unibyte_string (&ctx, obj);
}

DEFUN ("upcase", Fupcase, Supcase, 1, 1, 0,
       doc: /* Convert argument to upper case and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.  If argument
is a character, characters which map to multiple code points when
cased, e.g. ﬁ, are returned unchanged.
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
This means that each word's first character is converted to either
title case or upper case, and the rest to lower case.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.  If argument
is a character, characters which map to multiple code points when
cased, e.g. ﬁ, are returned unchanged.  */)
  (Lisp_Object obj)
{
  return casify_object (CASE_CAPITALIZE, obj);
}

/* Like Fcapitalize but change only the initials.  */

DEFUN ("upcase-initials", Fupcase_initials, Supcase_initials, 1, 1, 0,
       doc: /* Convert the initial of each word in the argument to upper case.
This means that each word's first character is converted to either
title case or upper case, and the rest are left unchanged.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.  If argument
is a character, characters which map to multiple code points when
cased, e.g. ﬁ, are returned unchanged.  */)
  (Lisp_Object obj)
{
  return casify_object (CASE_CAPITALIZE_UP, obj);
}

/* Based on CTX, case region in a unibyte buffer from *STARTP to *ENDP.

   Save first and last positions that has changed in *STARTP and *ENDP
   respectively.  If no characters were changed, save -1 to *STARTP and leave
   *ENDP unspecified.

   Always return 0.  This is so that interface of this function is the same as
   do_casify_multibyte_region.  */
static ptrdiff_t
do_casify_unibyte_region (struct casing_context *ctx,
			  ptrdiff_t *startp, ptrdiff_t *endp)
{
  ptrdiff_t first = -1, last = -1;  /* Position of first and last changes.  */
  ptrdiff_t end = *endp;

  for (ptrdiff_t pos = *startp; pos < end; ++pos)
    {
      int ch = FETCH_BYTE (pos);
      MAKE_CHAR_MULTIBYTE (ch);

      int cased = case_single_character (ctx, ch);
      if (cased == ch)
	continue;

      last = pos + 1;
      if (first < 0)
	first = pos;

      MAKE_CHAR_UNIBYTE (cased);
      FETCH_BYTE (pos) = cased;
    }

  *startp = first;
  *endp = last;
  return 0;
}

/* Based on CTX, case region in a multibyte buffer from *STARTP to *ENDP.

   Return number of added characters (may be negative if more characters were
   deleted then inserted), save first and last positions that has changed in
   *STARTP and *ENDP respectively.  If no characters were changed, return 0,
   save -1 to *STARTP and leave *ENDP unspecified.  */
static ptrdiff_t
do_casify_multibyte_region (struct casing_context *ctx,
			    ptrdiff_t *startp, ptrdiff_t *endp)
{
  ptrdiff_t first = -1, last = -1;  /* Position of first and last changes.  */
  ptrdiff_t pos = *startp, pos_byte = CHAR_TO_BYTE (pos), size = *endp - pos;
  ptrdiff_t opoint = PT, added = 0;

  for (; size; --size)
    {
      int len;
      int ch = STRING_CHAR_AND_LENGTH (BYTE_POS_ADDR (pos_byte), len);
      struct casing_str_buf buf;
      if (!case_character (&buf, ctx, ch,
			   size > 1 ? BYTE_POS_ADDR (pos_byte + len) : NULL))
	{
	  pos_byte += len;
	  ++pos;
	  continue;
	}

      last = pos + buf.len_chars;
      if (first < 0)
	first = pos;

      if (buf.len_chars == 1 && buf.len_bytes == len)
	memcpy (BYTE_POS_ADDR (pos_byte), buf.data, len);
      else
	{
	  /* Replace one character with the other(s), keeping text
	     properties the same.  */
	  replace_range_2 (pos, pos_byte, pos + 1, pos_byte + len,
			   (const char *) buf.data, buf.len_chars,
			   buf.len_bytes,
			   0);
	  added += (ptrdiff_t) buf.len_chars - 1;
	  if (opoint > pos)
	    opoint += (ptrdiff_t) buf.len_chars - 1;
	}

      pos_byte += buf.len_bytes;
      pos += buf.len_chars;
    }

  if (PT != opoint)
    TEMP_SET_PT_BOTH (opoint, CHAR_TO_BYTE (opoint));

  *startp = first;
  *endp = last;
  return added;
}

/* flag is CASE_UP, CASE_DOWN or CASE_CAPITALIZE or CASE_CAPITALIZE_UP.  b and
   e specify range of buffer to operate on.  Return character position of the
   end of the region after changes.  */
static ptrdiff_t
casify_region (enum case_action flag, Lisp_Object b, Lisp_Object e)
{
  ptrdiff_t added;
  struct casing_context ctx;

  validate_region (&b, &e);
  ptrdiff_t start = XFASTINT (b);
  ptrdiff_t end = XFASTINT (e);
  if (start == end)
    /* Not modifying because nothing marked.  */
    return end;
  modify_text (start, end);
  prepare_casing_context (&ctx, flag, true);

  ptrdiff_t orig_end = end;
  record_delete (start, make_buffer_string (start, end, true), false);
  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    {
      record_insert (start, end - start);
      added = do_casify_unibyte_region (&ctx, &start, &end);
    }
  else
    {
      ptrdiff_t len = end - start, ostart = start;
      added = do_casify_multibyte_region (&ctx, &start, &end);
      record_insert (ostart, len + added);
    }

  if (start >= 0)
    {
      signal_after_change (start, end - start - added, end - start);
      update_compositions (start, end, CHECK_ALL);
    }

  return orig_end + added;
}

DEFUN ("upcase-region", Fupcase_region, Supcase_region, 2, 3,
       "(list (region-beginning) (region-end) (region-noncontiguous-p))",
       doc: /* Convert the region to upper case.  In programs, wants two arguments.
These arguments specify the starting and ending character numbers of
the region to operate on.  When used as a command, the text between
point and the mark is operated on.
See also `capitalize-region'.  */)
  (Lisp_Object beg, Lisp_Object end, Lisp_Object region_noncontiguous_p)
{
  Lisp_Object bounds = Qnil;

  if (!NILP (region_noncontiguous_p))
    {
      bounds = call1 (Fsymbol_value (intern ("region-extract-function")),
		      intern ("bounds"));

      while (CONSP (bounds))
	{
	  casify_region (CASE_UP, XCAR (XCAR (bounds)), XCDR (XCAR (bounds)));
	  bounds = XCDR (bounds);
	}
    }
  else
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
This means that each word's first character is converted to either
title case or upper case, and the rest to lower case.
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
This means that each word's first character is converted to either
title case or upper case, and the rest are left unchanged.
In programs, give two arguments, the starting and ending
character positions to operate on.  */)
  (Lisp_Object beg, Lisp_Object end)
{
  casify_region (CASE_CAPITALIZE_UP, beg, end);
  return Qnil;
}

static Lisp_Object
casify_word (enum case_action flag, Lisp_Object arg)
{
  CHECK_NUMBER (arg);
  ptrdiff_t farend = scan_words (PT, XINT (arg));
  if (!farend)
    farend = XINT (arg) <= 0 ? BEGV : ZV;
  SET_PT (casify_region (flag, make_number (PT), make_number (farend)));
  return Qnil;
}

DEFUN ("upcase-word", Fupcase_word, Supcase_word, 1, 1, "p",
       doc: /* Convert to upper case from point to end of word, moving over.

If point is in the middle of a word, the part of that word before point
is ignored when moving forward.

With negative argument, convert previous words but do not move.
See also `capitalize-word'.  */)
  (Lisp_Object arg)
{
  return casify_word (CASE_UP, arg);
}

DEFUN ("downcase-word", Fdowncase_word, Sdowncase_word, 1, 1, "p",
       doc: /* Convert to lower case from point to end of word, moving over.

If point is in the middle of a word, the part of that word before point
is ignored when moving forward.

With negative argument, convert previous words but do not move.  */)
  (Lisp_Object arg)
{
  return casify_word (CASE_DOWN, arg);
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
  return casify_word (CASE_CAPITALIZE, arg);
}

void
syms_of_casefiddle (void)
{
  DEFSYM (Qidentity, "identity");
  DEFSYM (Qtitlecase, "titlecase");
  DEFSYM (Qspecial_uppercase, "special-uppercase");
  DEFSYM (Qspecial_lowercase, "special-lowercase");
  DEFSYM (Qspecial_titlecase, "special-titlecase");

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
