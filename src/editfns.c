/* Lisp functions pertaining to editing.
   Copyright (C) 1985,86,87,89,93,94,95,96,97,98, 1999, 2000, 2001, 2002
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
#include <sys/types.h>

#ifdef VMS
#include "vms-pwd.h"
#else
#include <pwd.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* Without this, sprintf on Mac OS Classic will produce wrong
   result.  */
#ifdef MAC_OS8
#include <stdio.h>
#endif

#include <ctype.h>

#include "lisp.h"
#include "intervals.h"
#include "buffer.h"
#include "charset.h"
#include "coding.h"
#include "frame.h"
#include "window.h"

#include "systime.h"

#ifdef STDC_HEADERS
#include <float.h>
#define MAX_10_EXP	DBL_MAX_10_EXP
#else
#define MAX_10_EXP	310
#endif

#ifndef NULL
#define NULL 0
#endif

#ifndef USE_CRT_DLL
extern char **environ;
#endif

extern Lisp_Object make_time P_ ((time_t));
extern size_t emacs_strftimeu P_ ((char *, size_t, const char *,
				   const struct tm *, int));
static int tm_diff P_ ((struct tm *, struct tm *));
static void find_field P_ ((Lisp_Object, Lisp_Object, Lisp_Object, int *, Lisp_Object, int *));
static void update_buffer_properties P_ ((int, int));
static Lisp_Object region_limit P_ ((int));
static int lisp_time_argument P_ ((Lisp_Object, time_t *, int *));
static size_t emacs_memftimeu P_ ((char *, size_t, const char *,
				   size_t, const struct tm *, int));
static void general_insert_function P_ ((void (*) (unsigned char *, int),
					 void (*) (Lisp_Object, int, int, int,
						   int, int),
					 int, int, Lisp_Object *));
static Lisp_Object subst_char_in_region_unwind P_ ((Lisp_Object));
static Lisp_Object subst_char_in_region_unwind_1 P_ ((Lisp_Object));
static void transpose_markers P_ ((int, int, int, int, int, int, int, int));

#ifdef HAVE_INDEX
extern char *index P_ ((const char *, int));
#endif

Lisp_Object Vbuffer_access_fontify_functions;
Lisp_Object Qbuffer_access_fontify_functions;
Lisp_Object Vbuffer_access_fontified_property;

Lisp_Object Fuser_full_name P_ ((Lisp_Object));

/* Non-nil means don't stop at field boundary in text motion commands.  */

Lisp_Object Vinhibit_field_text_motion;

/* Some static data, and a function to initialize it for each run */

Lisp_Object Vsystem_name;
Lisp_Object Vuser_real_login_name;	/* login name of current user ID */
Lisp_Object Vuser_full_name;		/* full name of current user */
Lisp_Object Vuser_login_name;		/* user name from LOGNAME or USER */

/* Symbol for the text property used to mark fields.  */

Lisp_Object Qfield;

/* A special value for Qfield properties.  */

Lisp_Object Qboundary;


void
init_editfns ()
{
  char *user_name;
  register unsigned char *p;
  struct passwd *pw;	/* password entry for the current user */
  Lisp_Object tem;

  /* Set up system_name even when dumping.  */
  init_system_name ();

#ifndef CANNOT_DUMP
  /* Don't bother with this on initial start when just dumping out */
  if (!initialized)
    return;
#endif /* not CANNOT_DUMP */

  pw = (struct passwd *) getpwuid (getuid ());
#ifdef MSDOS
  /* We let the real user name default to "root" because that's quite
     accurate on MSDOG and because it lets Emacs find the init file.
     (The DVX libraries override the Djgpp libraries here.)  */
  Vuser_real_login_name = build_string (pw ? pw->pw_name : "root");
#else
  Vuser_real_login_name = build_string (pw ? pw->pw_name : "unknown");
#endif

  /* Get the effective user name, by consulting environment variables,
     or the effective uid if those are unset.  */
  user_name = (char *) getenv ("LOGNAME");
  if (!user_name)
#ifdef WINDOWSNT
    user_name = (char *) getenv ("USERNAME");	/* it's USERNAME on NT */
#else  /* WINDOWSNT */
    user_name = (char *) getenv ("USER");
#endif /* WINDOWSNT */
  if (!user_name)
    {
      pw = (struct passwd *) getpwuid (geteuid ());
      user_name = (char *) (pw ? pw->pw_name : "unknown");
    }
  Vuser_login_name = build_string (user_name);

  /* If the user name claimed in the environment vars differs from
     the real uid, use the claimed name to find the full name.  */
  tem = Fstring_equal (Vuser_login_name, Vuser_real_login_name);
  Vuser_full_name = Fuser_full_name (NILP (tem)? make_number (geteuid())
				     : Vuser_login_name);

  p = (unsigned char *) getenv ("NAME");
  if (p)
    Vuser_full_name = build_string (p);
  else if (NILP (Vuser_full_name))
    Vuser_full_name = build_string ("unknown");
}

DEFUN ("char-to-string", Fchar_to_string, Schar_to_string, 1, 1, 0,
       doc: /* Convert arg CHAR to a string containing that character.
usage: (char-to-string CHAR)  */)
     (character)
     Lisp_Object character;
{
  int len;
  unsigned char str[MAX_MULTIBYTE_LENGTH];

  CHECK_NUMBER (character);

  len = (SINGLE_BYTE_CHAR_P (XFASTINT (character))
	 ? (*str = (unsigned char)(XFASTINT (character)), 1)
	 : char_to_string (XFASTINT (character), str));
  return make_string_from_bytes (str, 1, len);
}

DEFUN ("string-to-char", Fstring_to_char, Sstring_to_char, 1, 1, 0,
       doc: /* Convert arg STRING to a character, the first character of that string.
A multibyte character is handled correctly.  */)
     (string)
     register Lisp_Object string;
{
  register Lisp_Object val;
  register struct Lisp_String *p;
  CHECK_STRING (string);
  p = XSTRING (string);
  if (p->size)
    {
      if (STRING_MULTIBYTE (string))
	XSETFASTINT (val, STRING_CHAR (p->data, STRING_BYTES (p)));
      else
	XSETFASTINT (val, p->data[0]);
    }
  else
    XSETFASTINT (val, 0);
  return val;
}

static Lisp_Object
buildmark (charpos, bytepos)
     int charpos, bytepos;
{
  register Lisp_Object mark;
  mark = Fmake_marker ();
  set_marker_both (mark, Qnil, charpos, bytepos);
  return mark;
}

DEFUN ("point", Fpoint, Spoint, 0, 0, 0,
       doc: /* Return value of point, as an integer.
Beginning of buffer is position (point-min).  */)
     ()
{
  Lisp_Object temp;
  XSETFASTINT (temp, PT);
  return temp;
}

DEFUN ("point-marker", Fpoint_marker, Spoint_marker, 0, 0, 0,
       doc: /* Return value of point, as a marker object.  */)
     ()
{
  return buildmark (PT, PT_BYTE);
}

int
clip_to_bounds (lower, num, upper)
     int lower, num, upper;
{
  if (num < lower)
    return lower;
  else if (num > upper)
    return upper;
  else
    return num;
}

DEFUN ("goto-char", Fgoto_char, Sgoto_char, 1, 1, "NGoto char: ",
       doc: /* Set point to POSITION, a number or marker.
Beginning of buffer is position (point-min), end is (point-max).
If the position is in the middle of a multibyte form,
the actual point is set at the head of the multibyte form
except in the case that `enable-multibyte-characters' is nil.  */)
     (position)
     register Lisp_Object position;
{
  int pos;

  if (MARKERP (position)
      && current_buffer == XMARKER (position)->buffer)
    {
      pos = marker_position (position);
      if (pos < BEGV)
	SET_PT_BOTH (BEGV, BEGV_BYTE);
      else if (pos > ZV)
	SET_PT_BOTH (ZV, ZV_BYTE);
      else
	SET_PT_BOTH (pos, marker_byte_position (position));

      return position;
    }

  CHECK_NUMBER_COERCE_MARKER (position);

  pos = clip_to_bounds (BEGV, XINT (position), ZV);
  SET_PT (pos);
  return position;
}


/* Return the start or end position of the region.
   BEGINNINGP non-zero means return the start.
   If there is no region active, signal an error. */

static Lisp_Object
region_limit (beginningp)
     int beginningp;
{
  extern Lisp_Object Vmark_even_if_inactive; /* Defined in callint.c. */
  Lisp_Object m;
  
  if (!NILP (Vtransient_mark_mode)
      && NILP (Vmark_even_if_inactive)
      && NILP (current_buffer->mark_active))
    Fsignal (Qmark_inactive, Qnil);
  
  m = Fmarker_position (current_buffer->mark);
  if (NILP (m))
    error ("The mark is not set now, so there is no region");
  
  if ((PT < XFASTINT (m)) == beginningp)
    m = make_number (PT);
  return m;
}

DEFUN ("region-beginning", Fregion_beginning, Sregion_beginning, 0, 0, 0,
       doc: /* Return position of beginning of region, as an integer.  */)
     ()
{
  return region_limit (1);
}

DEFUN ("region-end", Fregion_end, Sregion_end, 0, 0, 0,
       doc: /* Return position of end of region, as an integer.  */)
     ()
{
  return region_limit (0);
}

DEFUN ("mark-marker", Fmark_marker, Smark_marker, 0, 0, 0,
       doc: /* Return this buffer's mark, as a marker object.
Watch out!  Moving this marker changes the mark position.
If you set the marker not to point anywhere, the buffer will have no mark.  */)
     ()
{
  return current_buffer->mark;
}


/* Find the field surrounding POS in *BEG and *END.  If POS is nil,
   the value of point is used instead.  If BEG or END null,
   means don't store the beginning or end of the field.

   BEG_LIMIT and END_LIMIT serve to limit the ranged of the returned
   results; they do not effect boundary behavior.

   If MERGE_AT_BOUNDARY is nonzero, then if POS is at the very first
   position of a field, then the beginning of the previous field is
   returned instead of the beginning of POS's field (since the end of a
   field is actually also the beginning of the next input field, this
   behavior is sometimes useful).  Additionally in the MERGE_AT_BOUNDARY
   true case, if two fields are separated by a field with the special
   value `boundary', and POS lies within it, then the two separated
   fields are considered to be adjacent, and POS between them, when
   finding the beginning and ending of the "merged" field.

   Either BEG or END may be 0, in which case the corresponding value
   is not stored.  */

static void
find_field (pos, merge_at_boundary, beg_limit, beg, end_limit, end)
     Lisp_Object pos;
     Lisp_Object merge_at_boundary;
     Lisp_Object beg_limit, end_limit;
     int *beg, *end;
{
  /* Fields right before and after the point.  */
  Lisp_Object before_field, after_field;
  /* If the fields came from overlays, the associated overlays.
     Qnil means they came from text-properties.  */
  Lisp_Object before_overlay = Qnil, after_overlay = Qnil;
  /* 1 if POS counts as the start of a field.  */
  int at_field_start = 0;
  /* 1 if POS counts as the end of a field.  */
  int at_field_end = 0;

  if (NILP (pos))
    XSETFASTINT (pos, PT);
  else
    CHECK_NUMBER_COERCE_MARKER (pos);

  after_field
    = get_char_property_and_overlay (pos, Qfield, Qnil, &after_overlay);
  before_field
    = (XFASTINT (pos) > BEGV
       ? get_char_property_and_overlay (make_number (XINT (pos) - 1),
					Qfield, Qnil,
					&before_overlay)
       : Qnil);

  /* See if we need to handle the case where MERGE_AT_BOUNDARY is nil
     and POS is at beginning of a field, which can also be interpreted
     as the end of the previous field.  Note that the case where if
     MERGE_AT_BOUNDARY is non-nil (see function comment) is actually the
     more natural one; then we avoid treating the beginning of a field
     specially.  */
  if (NILP (merge_at_boundary) && !EQ (after_field, before_field))
    /* We are at a boundary, see which direction is inclusive.  We
       decide by seeing which field the `field' property sticks to.  */
    {
      /* -1 means insertions go into before_field, 1 means they go
	 into after_field, 0 means neither.  */
      int stickiness;
      /* Whether the before/after_field come from overlays.  */
      int bop = !NILP (before_overlay);
      int aop = !NILP (after_overlay);

      if (bop && XMARKER (OVERLAY_END (before_overlay))->insertion_type == 1)
	/* before_field is from an overlay, which expands upon
	   end-insertions.  Note that it's possible for after_overlay to
	   also eat insertions here, but then they will overlap, and
	   there's not much we can do.  */
	stickiness = -1;
      else if (aop
	       && XMARKER (OVERLAY_START (after_overlay))->insertion_type == 0)
	/* after_field is from an overlay, which expand to contain
	   start-insertions.  */
	stickiness = 1;
      else if (bop && aop)
	/* Both fields come from overlays, but neither will contain any
	   insertion here.  */
	stickiness = 0;
      else if (bop)
	/* before_field is an overlay that won't eat any insertion, but
	   after_field is from a text-property.  Assume that the
	   text-property continues underneath the overlay, and so will
	   be inherited by any insertion, regardless of any stickiness
	   settings.  */
	stickiness = 1;
      else if (aop)
	/* Similarly, when after_field is the overlay.  */
	stickiness = -1;
      else
	/* Both fields come from text-properties.  Look for explicit
	   stickiness properties.  */
	stickiness = text_property_stickiness (Qfield, pos);

      if (stickiness > 0)
	at_field_start = 1;
      else if (stickiness < 0)
	at_field_end = 1;
      else
	/* STICKINESS == 0 means that any inserted text will get a
	   `field' char-property of nil, so check to see if that
	   matches either of the adjacent characters (this being a
	   kind of "stickiness by default").  */
	{
	  if (NILP (before_field))
	    at_field_end = 1; /* Sticks to the left.  */
	  else if (NILP (after_field))
	    at_field_start = 1; /* Sticks to the right.  */
	}
    }

  /* Note about special `boundary' fields:

     Consider the case where the point (`.') is between the fields `x' and `y':

	xxxx.yyyy

     In this situation, if merge_at_boundary is true, we consider the
     `x' and `y' fields as forming one big merged field, and so the end
     of the field is the end of `y'.

     However, if `x' and `y' are separated by a special `boundary' field
     (a field with a `field' char-property of 'boundary), then we ignore
     this special field when merging adjacent fields.  Here's the same
     situation, but with a `boundary' field between the `x' and `y' fields:

	xxx.BBBByyyy

     Here, if point is at the end of `x', the beginning of `y', or
     anywhere in-between (within the `boundary' field), we merge all
     three fields and consider the beginning as being the beginning of
     the `x' field, and the end as being the end of the `y' field.  */

  if (beg)
    {
      if (at_field_start)
	/* POS is at the edge of a field, and we should consider it as
	   the beginning of the following field.  */
	*beg = XFASTINT (pos);
      else
	/* Find the previous field boundary.  */
	{
	  if (!NILP (merge_at_boundary) && EQ (before_field, Qboundary))
	    /* Skip a `boundary' field.  */
	    pos = Fprevious_single_char_property_change (pos, Qfield, Qnil,
							 beg_limit);

	  pos = Fprevious_single_char_property_change (pos, Qfield, Qnil,
						       beg_limit);
	  *beg = NILP (pos) ? BEGV : XFASTINT (pos);
	}
    }

  if (end)
    {
      if (at_field_end)
	/* POS is at the edge of a field, and we should consider it as
	   the end of the previous field.  */
	*end = XFASTINT (pos);
      else
	/* Find the next field boundary.  */
	{
	  if (!NILP (merge_at_boundary) && EQ (after_field, Qboundary))
	    /* Skip a `boundary' field.  */
	    pos = Fnext_single_char_property_change (pos, Qfield, Qnil,
						     end_limit);

	  pos = Fnext_single_char_property_change (pos, Qfield, Qnil,
						   end_limit);
	  *end = NILP (pos) ? ZV : XFASTINT (pos);
	}
    }
}


DEFUN ("delete-field", Fdelete_field, Sdelete_field, 0, 1, 0,
       doc: /* Delete the field surrounding POS.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS.  */)
     (pos)
     Lisp_Object pos;
{
  int beg, end;
  find_field (pos, Qnil, Qnil, &beg, Qnil, &end);
  if (beg != end)
    del_range (beg, end);
  return Qnil;
}

DEFUN ("field-string", Ffield_string, Sfield_string, 0, 1, 0,
       doc: /* Return the contents of the field surrounding POS as a string.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS.  */)
     (pos)
     Lisp_Object pos;
{
  int beg, end;
  find_field (pos, Qnil, Qnil, &beg, Qnil, &end);
  return make_buffer_string (beg, end, 1);
}

DEFUN ("field-string-no-properties", Ffield_string_no_properties, Sfield_string_no_properties, 0, 1, 0,
       doc: /* Return the contents of the field around POS, without text-properties.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS.  */)
     (pos)
     Lisp_Object pos;
{
  int beg, end;
  find_field (pos, Qnil, Qnil, &beg, Qnil, &end);
  return make_buffer_string (beg, end, 0);
}

DEFUN ("field-beginning", Ffield_beginning, Sfield_beginning, 0, 3, 0,
       doc: /* Return the beginning of the field surrounding POS.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS.
If ESCAPE-FROM-EDGE is non-nil and POS is at the beginning of its
field, then the beginning of the *previous* field is returned.
If LIMIT is non-nil, it is a buffer position; if the beginning of the field
is before LIMIT, then LIMIT will be returned instead.  */)
     (pos, escape_from_edge, limit)
     Lisp_Object pos, escape_from_edge, limit;
{
  int beg;
  find_field (pos, escape_from_edge, limit, &beg, Qnil, 0);
  return make_number (beg);
}

DEFUN ("field-end", Ffield_end, Sfield_end, 0, 3, 0,
       doc: /* Return the end of the field surrounding POS.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS.
If ESCAPE-FROM-EDGE is non-nil and POS is at the end of its field,
then the end of the *following* field is returned.
If LIMIT is non-nil, it is a buffer position; if the end of the field
is after LIMIT, then LIMIT will be returned instead.  */)
     (pos, escape_from_edge, limit)
     Lisp_Object pos, escape_from_edge, limit;
{
  int end;
  find_field (pos, escape_from_edge, Qnil, 0, limit, &end);
  return make_number (end);
}

DEFUN ("constrain-to-field", Fconstrain_to_field, Sconstrain_to_field, 2, 5, 0,
       doc: /* Return the position closest to NEW-POS that is in the same field as OLD-POS.

A field is a region of text with the same `field' property.
If NEW-POS is nil, then the current point is used instead, and set to the
constrained position if that is different.

If OLD-POS is at the boundary of two fields, then the allowable
positions for NEW-POS depends on the value of the optional argument
ESCAPE-FROM-EDGE: If ESCAPE-FROM-EDGE is nil, then NEW-POS is
constrained to the field that has the same `field' char-property
as any new characters inserted at OLD-POS, whereas if ESCAPE-FROM-EDGE
is non-nil, NEW-POS is constrained to the union of the two adjacent
fields.  Additionally, if two fields are separated by another field with
the special value `boundary', then any point within this special field is
also considered to be `on the boundary'.

If the optional argument ONLY-IN-LINE is non-nil and constraining
NEW-POS would move it to a different line, NEW-POS is returned
unconstrained.  This useful for commands that move by line, like
\\[next-line] or \\[beginning-of-line], which should generally respect field boundaries
only in the case where they can still move to the right line.

If the optional argument INHIBIT-CAPTURE-PROPERTY is non-nil, and OLD-POS has
a non-nil property of that name, then any field boundaries are ignored.

Field boundaries are not noticed if `inhibit-field-text-motion' is non-nil.  */)
     (new_pos, old_pos, escape_from_edge, only_in_line, inhibit_capture_property)
     Lisp_Object new_pos, old_pos;
     Lisp_Object escape_from_edge, only_in_line, inhibit_capture_property;
{
  /* If non-zero, then the original point, before re-positioning.  */
  int orig_point = 0;

  if (NILP (new_pos))
    /* Use the current point, and afterwards, set it.  */
    {
      orig_point = PT;
      XSETFASTINT (new_pos, PT);
    }

  if (NILP (Vinhibit_field_text_motion)
      && !EQ (new_pos, old_pos)
      && (!NILP (Fget_char_property (new_pos, Qfield, Qnil))
	  || !NILP (Fget_char_property (old_pos, Qfield, Qnil)))
      && (NILP (inhibit_capture_property)
	  || NILP (Fget_char_property(old_pos, inhibit_capture_property, Qnil))))
    /* NEW_POS is not within the same field as OLD_POS; try to
       move NEW_POS so that it is.  */
    {
      int fwd, shortage;
      Lisp_Object field_bound;

      CHECK_NUMBER_COERCE_MARKER (new_pos);
      CHECK_NUMBER_COERCE_MARKER (old_pos);

      fwd = (XFASTINT (new_pos) > XFASTINT (old_pos));

      if (fwd)
	field_bound = Ffield_end (old_pos, escape_from_edge, new_pos);
      else
	field_bound = Ffield_beginning (old_pos, escape_from_edge, new_pos);

      if (/* See if ESCAPE_FROM_EDGE caused FIELD_BOUND to jump to the
             other side of NEW_POS, which would mean that NEW_POS is
             already acceptable, and it's not necessary to constrain it
             to FIELD_BOUND.  */
	  ((XFASTINT (field_bound) < XFASTINT (new_pos)) ? fwd : !fwd)
	  /* NEW_POS should be constrained, but only if either
	     ONLY_IN_LINE is nil (in which case any constraint is OK),
	     or NEW_POS and FIELD_BOUND are on the same line (in which
	     case the constraint is OK even if ONLY_IN_LINE is non-nil). */
	  && (NILP (only_in_line)
	      /* This is the ONLY_IN_LINE case, check that NEW_POS and
		 FIELD_BOUND are on the same line by seeing whether
		 there's an intervening newline or not.  */
	      || (scan_buffer ('\n',
			       XFASTINT (new_pos), XFASTINT (field_bound),
			       fwd ? -1 : 1, &shortage, 1),
		  shortage != 0)))
	/* Constrain NEW_POS to FIELD_BOUND.  */
	new_pos = field_bound;

      if (orig_point && XFASTINT (new_pos) != orig_point)
	/* The NEW_POS argument was originally nil, so automatically set PT. */
	SET_PT (XFASTINT (new_pos));
    }

  return new_pos;
}


DEFUN ("line-beginning-position",
       Fline_beginning_position, Sline_beginning_position, 0, 1, 0,
       doc: /* Return the character position of the first character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.

The scan does not cross a field boundary unless doing so would move
beyond there to a different line; if N is nil or 1, and scan starts at a
field boundary, the scan stops as soon as it starts.  To ignore field
boundaries bind `inhibit-field-text-motion' to t.

This function does not move point.  */)
     (n)
     Lisp_Object n;
{
  int orig, orig_byte, end;

  if (NILP (n))
    XSETFASTINT (n, 1);
  else
    CHECK_NUMBER (n);

  orig = PT;
  orig_byte = PT_BYTE;
  Fforward_line (make_number (XINT (n) - 1));
  end = PT;

  SET_PT_BOTH (orig, orig_byte);

  /* Return END constrained to the current input field.  */
  return Fconstrain_to_field (make_number (end), make_number (orig),
			      XINT (n) != 1 ? Qt : Qnil,
			      Qt, Qnil);
}

DEFUN ("line-end-position", Fline_end_position, Sline_end_position, 0, 1, 0,
       doc: /* Return the character position of the last character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.

The scan does not cross a field boundary unless doing so would move
beyond there to a different line; if N is nil or 1, and scan starts at a
field boundary, the scan stops as soon as it starts.  To ignore field
boundaries bind `inhibit-field-text-motion' to t.

This function does not move point.  */)
     (n)
     Lisp_Object n;
{
  int end_pos;
  int orig = PT;

  if (NILP (n))
    XSETFASTINT (n, 1);
  else
    CHECK_NUMBER (n);

  end_pos = find_before_next_newline (orig, 0, XINT (n) - (XINT (n) <= 0));

  /* Return END_POS constrained to the current input field.  */
  return Fconstrain_to_field (make_number (end_pos), make_number (orig),
			      Qnil, Qt, Qnil);
}


Lisp_Object
save_excursion_save ()
{
  int visible = (XBUFFER (XWINDOW (selected_window)->buffer)
		 == current_buffer);

  return Fcons (Fpoint_marker (),
		Fcons (Fcopy_marker (current_buffer->mark, Qnil),
		       Fcons (visible ? Qt : Qnil,
			      Fcons (current_buffer->mark_active,
				     selected_window))));
}

Lisp_Object
save_excursion_restore (info)
     Lisp_Object info;
{
  Lisp_Object tem, tem1, omark, nmark;
  struct gcpro gcpro1, gcpro2, gcpro3;
  int visible_p;

  tem = Fmarker_buffer (XCAR (info));
  /* If buffer being returned to is now deleted, avoid error */
  /* Otherwise could get error here while unwinding to top level
     and crash */
  /* In that case, Fmarker_buffer returns nil now.  */
  if (NILP (tem))
    return Qnil;

  omark = nmark = Qnil;
  GCPRO3 (info, omark, nmark);

  Fset_buffer (tem);

  /* Point marker.  */
  tem = XCAR (info);
  Fgoto_char (tem);
  unchain_marker (tem);

  /* Mark marker.  */
  info = XCDR (info);
  tem = XCAR (info);
  omark = Fmarker_position (current_buffer->mark);
  Fset_marker (current_buffer->mark, tem, Fcurrent_buffer ());
  nmark = Fmarker_position (tem);
  unchain_marker (tem);

  /* visible */
  info = XCDR (info);
  visible_p = !NILP (XCAR (info));
  
#if 0 /* We used to make the current buffer visible in the selected window
	 if that was true previously.  That avoids some anomalies.
	 But it creates others, and it wasn't documented, and it is simpler
	 and cleaner never to alter the window/buffer connections.  */
  tem1 = Fcar (tem);
  if (!NILP (tem1)
      && current_buffer != XBUFFER (XWINDOW (selected_window)->buffer))
    Fswitch_to_buffer (Fcurrent_buffer (), Qnil);
#endif /* 0 */

  /* Mark active */
  info = XCDR (info);
  tem = XCAR (info);
  tem1 = current_buffer->mark_active;
  current_buffer->mark_active = tem;

  if (!NILP (Vrun_hooks))
    {
      /* If mark is active now, and either was not active
	 or was at a different place, run the activate hook.  */
      if (! NILP (current_buffer->mark_active))
	{
	  if (! EQ (omark, nmark))
	    call1 (Vrun_hooks, intern ("activate-mark-hook"));
	}
      /* If mark has ceased to be active, run deactivate hook.  */
      else if (! NILP (tem1))
	call1 (Vrun_hooks, intern ("deactivate-mark-hook"));
    }

  /* If buffer was visible in a window, and a different window was
     selected, and the old selected window is still showing this
     buffer, restore point in that window.  */
  tem = XCDR (info);
  if (visible_p
      && !EQ (tem, selected_window)
      && (tem1 = XWINDOW (tem)->buffer,
	  (/* Window is live...  */
	   BUFFERP (tem1)
	   /* ...and it shows the current buffer.  */
	   && XBUFFER (tem1) == current_buffer)))
    Fset_window_point (tem, make_number (PT));

  UNGCPRO;
  return Qnil;
}

DEFUN ("save-excursion", Fsave_excursion, Ssave_excursion, 0, UNEVALLED, 0,
       doc: /* Save point, mark, and current buffer; execute BODY; restore those things.
Executes BODY just like `progn'.
The values of point, mark and the current buffer are restored
even in case of abnormal exit (throw or error).
The state of activation of the mark is also restored.

This construct does not save `deactivate-mark', and therefore
functions that change the buffer will still cause deactivation
of the mark at the end of the command.  To prevent that, bind
`deactivate-mark' with `let'.

usage: (save-excursion &rest BODY)  */)
     (args)
     Lisp_Object args;
{
  register Lisp_Object val;
  int count = specpdl_ptr - specpdl;

  record_unwind_protect (save_excursion_restore, save_excursion_save ());

  val = Fprogn (args);
  return unbind_to (count, val);
}

DEFUN ("save-current-buffer", Fsave_current_buffer, Ssave_current_buffer, 0, UNEVALLED, 0,
       doc: /* Save the current buffer; execute BODY; restore the current buffer.
Executes BODY just like `progn'.
usage: (save-current-buffer &rest BODY)  */)
     (args)
     Lisp_Object args;
{
  Lisp_Object val;
  int count = specpdl_ptr - specpdl;

  record_unwind_protect (set_buffer_if_live, Fcurrent_buffer ());

  val = Fprogn (args);
  return unbind_to (count, val);
}

DEFUN ("buffer-size", Fbufsize, Sbufsize, 0, 1, 0,
       doc: /* Return the number of characters in the current buffer.
If BUFFER, return the number of characters in that buffer instead.  */)
     (buffer)
     Lisp_Object buffer;
{
  if (NILP (buffer))
    return make_number (Z - BEG);
  else
    {
      CHECK_BUFFER (buffer);
      return make_number (BUF_Z (XBUFFER (buffer))
			  - BUF_BEG (XBUFFER (buffer)));
    }
}

DEFUN ("point-min", Fpoint_min, Spoint_min, 0, 0, 0,
       doc: /* Return the minimum permissible value of point in the current buffer.
This is 1, unless narrowing (a buffer restriction) is in effect.  */)
     ()
{
  Lisp_Object temp;
  XSETFASTINT (temp, BEGV);
  return temp;
}

DEFUN ("point-min-marker", Fpoint_min_marker, Spoint_min_marker, 0, 0, 0,
       doc: /* Return a marker to the minimum permissible value of point in this buffer.
This is the beginning, unless narrowing (a buffer restriction) is in effect.  */)
     ()
{
  return buildmark (BEGV, BEGV_BYTE);
}

DEFUN ("point-max", Fpoint_max, Spoint_max, 0, 0, 0,
       doc: /* Return the maximum permissible value of point in the current buffer.
This is (1+ (buffer-size)), unless narrowing (a buffer restriction)
is in effect, in which case it is less.  */)
     ()
{
  Lisp_Object temp;
  XSETFASTINT (temp, ZV);
  return temp;
}

DEFUN ("point-max-marker", Fpoint_max_marker, Spoint_max_marker, 0, 0, 0,
       doc: /* Return a marker to the maximum permissible value of point in this buffer.
This is (1+ (buffer-size)), unless narrowing (a buffer restriction)
is in effect, in which case it is less.  */)
     ()
{
  return buildmark (ZV, ZV_BYTE);
}

DEFUN ("gap-position", Fgap_position, Sgap_position, 0, 0, 0,
       doc: /* Return the position of the gap, in the current buffer.
See also `gap-size'.  */)
     ()
{
  Lisp_Object temp;
  XSETFASTINT (temp, GPT);
  return temp;
}

DEFUN ("gap-size", Fgap_size, Sgap_size, 0, 0, 0,
       doc: /* Return the size of the current buffer's gap.
See also `gap-position'.  */)
     ()
{
  Lisp_Object temp;
  XSETFASTINT (temp, GAP_SIZE);
  return temp;
}

DEFUN ("position-bytes", Fposition_bytes, Sposition_bytes, 1, 1, 0,
       doc: /* Return the byte position for character position POSITION.
If POSITION is out of range, the value is nil.  */)
     (position)
     Lisp_Object position;
{
  CHECK_NUMBER_COERCE_MARKER (position);
  if (XINT (position) < BEG || XINT (position) > Z)
    return Qnil;
  return make_number (CHAR_TO_BYTE (XINT (position)));
}

DEFUN ("byte-to-position", Fbyte_to_position, Sbyte_to_position, 1, 1, 0,
       doc: /* Return the character position for byte position BYTEPOS.
If BYTEPOS is out of range, the value is nil.  */)
     (bytepos)
     Lisp_Object bytepos;
{
  CHECK_NUMBER (bytepos);
  if (XINT (bytepos) < BEG_BYTE || XINT (bytepos) > Z_BYTE)
    return Qnil;
  return make_number (BYTE_TO_CHAR (XINT (bytepos)));
}

DEFUN ("following-char", Ffollowing_char, Sfollowing_char, 0, 0, 0,
       doc: /* Return the character following point, as a number.
At the end of the buffer or accessible region, return 0.  */)
     ()
{
  Lisp_Object temp;
  if (PT >= ZV)
    XSETFASTINT (temp, 0);
  else
    XSETFASTINT (temp, FETCH_CHAR (PT_BYTE));
  return temp;
}

DEFUN ("preceding-char", Fprevious_char, Sprevious_char, 0, 0, 0,
       doc: /* Return the character preceding point, as a number.
At the beginning of the buffer or accessible region, return 0.  */)
     ()
{
  Lisp_Object temp;
  if (PT <= BEGV)
    XSETFASTINT (temp, 0);
  else if (!NILP (current_buffer->enable_multibyte_characters))
    {
      int pos = PT_BYTE;
      DEC_POS (pos);
      XSETFASTINT (temp, FETCH_CHAR (pos));
    }
  else
    XSETFASTINT (temp, FETCH_BYTE (PT_BYTE - 1));
  return temp;
}

DEFUN ("bobp", Fbobp, Sbobp, 0, 0, 0,
       doc: /* Return t if point is at the beginning of the buffer.
If the buffer is narrowed, this means the beginning of the narrowed part.  */)
     ()
{
  if (PT == BEGV)
    return Qt;
  return Qnil;
}

DEFUN ("eobp", Feobp, Seobp, 0, 0, 0,
       doc: /* Return t if point is at the end of the buffer.
If the buffer is narrowed, this means the end of the narrowed part.  */)
     ()
{
  if (PT == ZV)
    return Qt;
  return Qnil;
}

DEFUN ("bolp", Fbolp, Sbolp, 0, 0, 0,
       doc: /* Return t if point is at the beginning of a line.  */)
     ()
{
  if (PT == BEGV || FETCH_BYTE (PT_BYTE - 1) == '\n')
    return Qt;
  return Qnil;
}

DEFUN ("eolp", Feolp, Seolp, 0, 0, 0,
       doc: /* Return t if point is at the end of a line.
`End of a line' includes point being at the end of the buffer.  */)
     ()
{
  if (PT == ZV || FETCH_BYTE (PT_BYTE) == '\n')
    return Qt;
  return Qnil;
}

DEFUN ("char-after", Fchar_after, Schar_after, 0, 1, 0,
       doc: /* Return character in current buffer at position POS.
POS is an integer or a marker.
If POS is out of range, the value is nil.  */)
     (pos)
     Lisp_Object pos;
{
  register int pos_byte;

  if (NILP (pos))
    {
      pos_byte = PT_BYTE;
      XSETFASTINT (pos, PT);
    }

  if (MARKERP (pos))
    {
      pos_byte = marker_byte_position (pos);
      if (pos_byte < BEGV_BYTE || pos_byte >= ZV_BYTE)
	return Qnil;
    }
  else
    {
      CHECK_NUMBER_COERCE_MARKER (pos);
      if (XINT (pos) < BEGV || XINT (pos) >= ZV)
	return Qnil;

      pos_byte = CHAR_TO_BYTE (XINT (pos));
    }

  return make_number (FETCH_CHAR (pos_byte));
}

DEFUN ("char-before", Fchar_before, Schar_before, 0, 1, 0,
       doc: /* Return character in current buffer preceding position POS.
POS is an integer or a marker.
If POS is out of range, the value is nil.  */)
     (pos)
     Lisp_Object pos;
{
  register Lisp_Object val;
  register int pos_byte;

  if (NILP (pos))
    {
      pos_byte = PT_BYTE;
      XSETFASTINT (pos, PT);
    }

  if (MARKERP (pos))
    {
      pos_byte = marker_byte_position (pos);

      if (pos_byte <= BEGV_BYTE || pos_byte > ZV_BYTE)
	return Qnil;
    }
  else
    {
      CHECK_NUMBER_COERCE_MARKER (pos);

      if (XINT (pos) <= BEGV || XINT (pos) > ZV)
	return Qnil;

      pos_byte = CHAR_TO_BYTE (XINT (pos));
    }

  if (!NILP (current_buffer->enable_multibyte_characters))
    {
      DEC_POS (pos_byte);
      XSETFASTINT (val, FETCH_CHAR (pos_byte));
    }
  else
    {
      pos_byte--;
      XSETFASTINT (val, FETCH_BYTE (pos_byte));
    }
   return val;
}

DEFUN ("user-login-name", Fuser_login_name, Suser_login_name, 0, 1, 0,
       doc: /* Return the name under which the user logged in, as a string.
This is based on the effective uid, not the real uid.
Also, if the environment variable LOGNAME or USER is set,
that determines the value of this function.

If optional argument UID is an integer, return the login name of the user
with that uid, or nil if there is no such user.  */)
     (uid)
     Lisp_Object uid;
{
  struct passwd *pw;

  /* Set up the user name info if we didn't do it before.
     (That can happen if Emacs is dumpable
     but you decide to run `temacs -l loadup' and not dump.  */
  if (INTEGERP (Vuser_login_name))
    init_editfns ();

  if (NILP (uid))
    return Vuser_login_name;

  CHECK_NUMBER (uid);
  pw = (struct passwd *) getpwuid (XINT (uid));
  return (pw ? build_string (pw->pw_name) : Qnil);
}

DEFUN ("user-real-login-name", Fuser_real_login_name, Suser_real_login_name,
       0, 0, 0,
       doc: /* Return the name of the user's real uid, as a string.
This ignores the environment variables LOGNAME and USER, so it differs from
`user-login-name' when running under `su'.  */)
     ()
{
  /* Set up the user name info if we didn't do it before.
     (That can happen if Emacs is dumpable
     but you decide to run `temacs -l loadup' and not dump.  */
  if (INTEGERP (Vuser_login_name))
    init_editfns ();
  return Vuser_real_login_name;
}

DEFUN ("user-uid", Fuser_uid, Suser_uid, 0, 0, 0,
       doc: /* Return the effective uid of Emacs.
Value is an integer or float, depending on the value.  */)
     ()
{
  return make_fixnum_or_float (geteuid ());
}

DEFUN ("user-real-uid", Fuser_real_uid, Suser_real_uid, 0, 0, 0,
       doc: /* Return the real uid of Emacs.
Value is an integer or float, depending on the value.  */)
     ()
{
  return make_fixnum_or_float (getuid ());
}

DEFUN ("user-full-name", Fuser_full_name, Suser_full_name, 0, 1, 0,
       doc: /* Return the full name of the user logged in, as a string.
If the full name corresponding to Emacs's userid is not known,
return "unknown".

If optional argument UID is an integer or float, return the full name
of the user with that uid, or nil if there is no such user.
If UID is a string, return the full name of the user with that login
name, or nil if there is no such user.  */)
     (uid)
     Lisp_Object uid;
{
  struct passwd *pw;
  register unsigned char *p, *q;
  Lisp_Object full;

  if (NILP (uid))
    return Vuser_full_name;
  else if (NUMBERP (uid))
    pw = (struct passwd *) getpwuid ((uid_t) XFLOATINT (uid));
  else if (STRINGP (uid))
    pw = (struct passwd *) getpwnam (XSTRING (uid)->data);
  else
    error ("Invalid UID specification");

  if (!pw)
    return Qnil;

  p = (unsigned char *) USER_FULL_NAME;
  /* Chop off everything after the first comma. */
  q = (unsigned char *) index (p, ',');
  full = make_string (p, q ? q - p : strlen (p));

#ifdef AMPERSAND_FULL_NAME
  p = XSTRING (full)->data;
  q = (unsigned char *) index (p, '&');
  /* Substitute the login name for the &, upcasing the first character.  */
  if (q)
    {
      register unsigned char *r;
      Lisp_Object login;

      login = Fuser_login_name (make_number (pw->pw_uid));
      r = (unsigned char *) alloca (strlen (p) + XSTRING (login)->size + 1);
      bcopy (p, r, q - p);
      r[q - p] = 0;
      strcat (r, XSTRING (login)->data);
      r[q - p] = UPCASE (r[q - p]);
      strcat (r, q + 1);
      full = build_string (r);
    }
#endif /* AMPERSAND_FULL_NAME */

  return full;
}

DEFUN ("system-name", Fsystem_name, Ssystem_name, 0, 0, 0,
       doc: /* Return the name of the machine you are running on, as a string.  */)
     ()
{
  return Vsystem_name;
}

/* For the benefit of callers who don't want to include lisp.h */

char *
get_system_name ()
{
  if (STRINGP (Vsystem_name))
    return (char *) XSTRING (Vsystem_name)->data;
  else
    return "";
}

DEFUN ("emacs-pid", Femacs_pid, Semacs_pid, 0, 0, 0,
       doc: /* Return the process ID of Emacs, as an integer.  */)
     ()
{
  return make_number (getpid ());
}

DEFUN ("current-time", Fcurrent_time, Scurrent_time, 0, 0, 0,
       doc: /* Return the current time, as the number of seconds since 1970-01-01 00:00:00.
The time is returned as a list of three integers.  The first has the
most significant 16 bits of the seconds, while the second has the
least significant 16 bits.  The third integer gives the microsecond
count.

The microsecond count is zero on systems that do not provide
resolution finer than a second.  */)
     ()
{
  EMACS_TIME t;
  Lisp_Object result[3];

  EMACS_GET_TIME (t);
  XSETINT (result[0], (EMACS_SECS (t) >> 16) & 0xffff);
  XSETINT (result[1], (EMACS_SECS (t) >> 0)  & 0xffff);
  XSETINT (result[2], EMACS_USECS (t));

  return Flist (3, result);
}


static int
lisp_time_argument (specified_time, result, usec)
     Lisp_Object specified_time;
     time_t *result;
     int *usec;
{
  if (NILP (specified_time))
    {
      if (usec)
        {
          EMACS_TIME t;

          EMACS_GET_TIME (t);
          *usec = EMACS_USECS (t);
          *result = EMACS_SECS (t);
          return 1;
        }
      else
        return time (result) != -1;
    }
  else
    {
      Lisp_Object high, low;
      high = Fcar (specified_time);
      CHECK_NUMBER (high);
      low = Fcdr (specified_time);
      if (CONSP (low))
        {
          if (usec)
            {
              Lisp_Object usec_l = Fcdr (low);
              if (CONSP (usec_l))
                usec_l = Fcar (usec_l);
              if (NILP (usec_l))
                *usec = 0;
              else
                {
                  CHECK_NUMBER (usec_l);
                  *usec = XINT (usec_l);
                }
            }
          low = Fcar (low);
        }
      else if (usec)
        *usec = 0;
      CHECK_NUMBER (low);
      *result = (XINT (high) << 16) + (XINT (low) & 0xffff);
      return *result >> 16 == XINT (high);
    }
}

DEFUN ("float-time", Ffloat_time, Sfloat_time, 0, 1, 0,
       doc: /* Return the current time, as a float number of seconds since the epoch.
If an argument is given, it specifies a time to convert to float
instead of the current time.  The argument should have the forms:
 (HIGH . LOW) or (HIGH LOW USEC) or (HIGH LOW . USEC).
Thus, you can use times obtained from `current-time'
and from `file-attributes'.

WARNING: Since the result is floating point, it may not be exact.
Do not use this function if precise time stamps are required.  */)
     (specified_time)
     Lisp_Object specified_time;
{
  time_t sec;
  int usec;

  if (! lisp_time_argument (specified_time, &sec, &usec))
    error ("Invalid time specification");

  return make_float ((sec * 1e6 + usec) / 1e6);
}

/* Write information into buffer S of size MAXSIZE, according to the
   FORMAT of length FORMAT_LEN, using time information taken from *TP.
   Default to Universal Time if UT is nonzero, local time otherwise.
   Return the number of bytes written, not including the terminating
   '\0'.  If S is NULL, nothing will be written anywhere; so to
   determine how many bytes would be written, use NULL for S and
   ((size_t) -1) for MAXSIZE.

   This function behaves like emacs_strftimeu, except it allows null
   bytes in FORMAT.  */
static size_t
emacs_memftimeu (s, maxsize, format, format_len, tp, ut)
      char *s;
      size_t maxsize;
      const char *format;
      size_t format_len;
      const struct tm *tp;
      int ut;
{
  size_t total = 0;

  /* Loop through all the null-terminated strings in the format
     argument.  Normally there's just one null-terminated string, but
     there can be arbitrarily many, concatenated together, if the
     format contains '\0' bytes.  emacs_strftimeu stops at the first
     '\0' byte so we must invoke it separately for each such string.  */
  for (;;)
    {
      size_t len;
      size_t result;

      if (s)
	s[0] = '\1';

      result = emacs_strftimeu (s, maxsize, format, tp, ut);

      if (s)
	{
	  if (result == 0 && s[0] != '\0')
	    return 0;
	  s += result + 1;
	}

      maxsize -= result + 1;
      total += result;
      len = strlen (format);
      if (len == format_len)
	return total;
      total++;
      format += len + 1;
      format_len -= len + 1;
    }
}

DEFUN ("format-time-string", Fformat_time_string, Sformat_time_string, 1, 3, 0,
       doc: /* Use FORMAT-STRING to format the time TIME, or now if omitted.
TIME is specified as (HIGH LOW . IGNORED) or (HIGH . LOW), as returned by
`current-time' or `file-attributes'.
The third, optional, argument UNIVERSAL, if non-nil, means describe TIME
as Universal Time; nil means describe TIME in the local time zone.
The value is a copy of FORMAT-STRING, but with certain constructs replaced
by text that describes the specified date and time in TIME:

%Y is the year, %y within the century, %C the century.
%G is the year corresponding to the ISO week, %g within the century.
%m is the numeric month.
%b and %h are the locale's abbreviated month name, %B the full name.
%d is the day of the month, zero-padded, %e is blank-padded.
%u is the numeric day of week from 1 (Monday) to 7, %w from 0 (Sunday) to 6.
%a is the locale's abbreviated name of the day of week, %A the full name.
%U is the week number starting on Sunday, %W starting on Monday,
 %V according to ISO 8601.
%j is the day of the year.

%H is the hour on a 24-hour clock, %I is on a 12-hour clock, %k is like %H
 only blank-padded, %l is like %I blank-padded.
%p is the locale's equivalent of either AM or PM.
%M is the minute.
%S is the second.
%Z is the time zone name, %z is the numeric form.
%s is the number of seconds since 1970-01-01 00:00:00 +0000.

%c is the locale's date and time format.
%x is the locale's "preferred" date format.
%D is like "%m/%d/%y".

%R is like "%H:%M", %T is like "%H:%M:%S", %r is like "%I:%M:%S %p".
%X is the locale's "preferred" time format.

Finally, %n is a newline, %t is a tab, %% is a literal %.

Certain flags and modifiers are available with some format controls.
The flags are `_', `-', `^' and `#'.  For certain characters X,
%_X is like %X, but padded with blanks; %-X is like %X,
ut without padding.  %^X is like %X but with all textual
characters up-cased; %#X is like %X but with letter-case of
all textual characters reversed.
%NX (where N stands for an integer) is like %X,
but takes up at least N (a number) positions.
The modifiers are `E' and `O'.  For certain characters X,
%EX is a locale's alternative version of %X;
%OX is like %X, but uses the locale's number symbols.

For example, to produce full ISO 8601 format, use "%Y-%m-%dT%T%z".  */)
     (format_string, time, universal)
     Lisp_Object format_string, time, universal;
{
  time_t value;
  int size;
  struct tm *tm;
  int ut = ! NILP (universal);

  CHECK_STRING (format_string);

  if (! lisp_time_argument (time, &value, NULL))
    error ("Invalid time specification");

  format_string = code_convert_string_norecord (format_string,
						Vlocale_coding_system, 1);

  /* This is probably enough.  */
  size = STRING_BYTES (XSTRING (format_string)) * 6 + 50;

  tm = ut ? gmtime (&value) : localtime (&value);
  if (! tm)
    error ("Specified time is not representable");

  synchronize_system_time_locale ();

  while (1)
    {
      char *buf = (char *) alloca (size + 1);
      int result;

      buf[0] = '\1';
      result = emacs_memftimeu (buf, size, XSTRING (format_string)->data,
				STRING_BYTES (XSTRING (format_string)),
				tm, ut);
      if ((result > 0 && result < size) || (result == 0 && buf[0] == '\0'))
	return code_convert_string_norecord (make_string (buf, result),
					     Vlocale_coding_system, 0);

      /* If buffer was too small, make it bigger and try again.  */
      result = emacs_memftimeu (NULL, (size_t) -1,
				XSTRING (format_string)->data,
				STRING_BYTES (XSTRING (format_string)),
				tm, ut);
      size = result + 1;
    }
}

DEFUN ("decode-time", Fdecode_time, Sdecode_time, 0, 1, 0,
       doc: /* Decode a time value as (SEC MINUTE HOUR DAY MONTH YEAR DOW DST ZONE).
The optional SPECIFIED-TIME should be a list of (HIGH LOW . IGNORED)
or (HIGH . LOW), as from `current-time' and `file-attributes', or `nil'
to use the current time.  The list has the following nine members:
SEC is an integer between 0 and 60; SEC is 60 for a leap second, which
only some operating systems support.  MINUTE is an integer between 0 and 59.
HOUR is an integer between 0 and 23.  DAY is an integer between 1 and 31.
MONTH is an integer between 1 and 12.  YEAR is an integer indicating the
four-digit year.  DOW is the day of week, an integer between 0 and 6, where
0 is Sunday.  DST is t if daylight savings time is effect, otherwise nil.
ZONE is an integer indicating the number of seconds east of Greenwich.
(Note that Common Lisp has different meanings for DOW and ZONE.)  */)
     (specified_time)
     Lisp_Object specified_time;
{
  time_t time_spec;
  struct tm save_tm;
  struct tm *decoded_time;
  Lisp_Object list_args[9];

  if (! lisp_time_argument (specified_time, &time_spec, NULL))
    error ("Invalid time specification");

  decoded_time = localtime (&time_spec);
  if (! decoded_time)
    error ("Specified time is not representable");
  XSETFASTINT (list_args[0], decoded_time->tm_sec);
  XSETFASTINT (list_args[1], decoded_time->tm_min);
  XSETFASTINT (list_args[2], decoded_time->tm_hour);
  XSETFASTINT (list_args[3], decoded_time->tm_mday);
  XSETFASTINT (list_args[4], decoded_time->tm_mon + 1);
  XSETINT (list_args[5], decoded_time->tm_year + 1900);
  XSETFASTINT (list_args[6], decoded_time->tm_wday);
  list_args[7] = (decoded_time->tm_isdst)? Qt : Qnil;

  /* Make a copy, in case gmtime modifies the struct.  */
  save_tm = *decoded_time;
  decoded_time = gmtime (&time_spec);
  if (decoded_time == 0)
    list_args[8] = Qnil;
  else
    XSETINT (list_args[8], tm_diff (&save_tm, decoded_time));
  return Flist (9, list_args);
}

DEFUN ("encode-time", Fencode_time, Sencode_time, 6, MANY, 0,
       doc: /* Convert SECOND, MINUTE, HOUR, DAY, MONTH, YEAR and ZONE to internal time.
This is the reverse operation of `decode-time', which see.
ZONE defaults to the current time zone rule.  This can
be a string or t (as from `set-time-zone-rule'), or it can be a list
\(as from `current-time-zone') or an integer (as from `decode-time')
applied without consideration for daylight savings time.

You can pass more than 7 arguments; then the first six arguments
are used as SECOND through YEAR, and the *last* argument is used as ZONE.
The intervening arguments are ignored.
This feature lets (apply 'encode-time (decode-time ...)) work.

Out-of-range values for SEC, MINUTE, HOUR, DAY, or MONTH are allowed;
for example, a DAY of 0 means the day preceding the given month.
Year numbers less than 100 are treated just like other year numbers.
If you want them to stand for years in this century, you must do that yourself.

usage: (encode-time SECOND MINUTE HOUR DAY MONTH YEAR &optional ZONE)  */)
     (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  time_t time;
  struct tm tm;
  Lisp_Object zone = (nargs > 6 ? args[nargs - 1] : Qnil);

  CHECK_NUMBER (args[0]);	/* second */
  CHECK_NUMBER (args[1]);	/* minute */
  CHECK_NUMBER (args[2]);	/* hour */
  CHECK_NUMBER (args[3]);	/* day */
  CHECK_NUMBER (args[4]);	/* month */
  CHECK_NUMBER (args[5]);	/* year */

  tm.tm_sec = XINT (args[0]);
  tm.tm_min = XINT (args[1]);
  tm.tm_hour = XINT (args[2]);
  tm.tm_mday = XINT (args[3]);
  tm.tm_mon = XINT (args[4]) - 1;
  tm.tm_year = XINT (args[5]) - 1900;
  tm.tm_isdst = -1;

  if (CONSP (zone))
    zone = Fcar (zone);
  if (NILP (zone))
    time = mktime (&tm);
  else
    {
      char tzbuf[100];
      char *tzstring;
      char **oldenv = environ, **newenv;

      if (EQ (zone, Qt))
	tzstring = "UTC0";
      else if (STRINGP (zone))
	tzstring = (char *) XSTRING (zone)->data;
      else if (INTEGERP (zone))
	{
	  int abszone = abs (XINT (zone));
	  sprintf (tzbuf, "XXX%s%d:%02d:%02d", "-" + (XINT (zone) < 0),
		   abszone / (60*60), (abszone/60) % 60, abszone % 60);
	  tzstring = tzbuf;
	}
      else
	error ("Invalid time zone specification");

      /* Set TZ before calling mktime; merely adjusting mktime's returned
	 value doesn't suffice, since that would mishandle leap seconds.  */
      set_time_zone_rule (tzstring);

      time = mktime (&tm);

      /* Restore TZ to previous value.  */
      newenv = environ;
      environ = oldenv;
      xfree (newenv);
#ifdef LOCALTIME_CACHE
      tzset ();
#endif
    }

  if (time == (time_t) -1)
    error ("Specified time is not representable");

  return make_time (time);
}

DEFUN ("current-time-string", Fcurrent_time_string, Scurrent_time_string, 0, 1, 0,
       doc: /* Return the current time, as a human-readable string.
Programs can use this function to decode a time,
since the number of columns in each field is fixed.
The format is `Sun Sep 16 01:03:52 1973'.
However, see also the functions `decode-time' and `format-time-string'
which provide a much more powerful and general facility.

If an argument is given, it specifies a time to format
instead of the current time.  The argument should have the form:
  (HIGH . LOW)
or the form:
  (HIGH LOW . IGNORED).
Thus, you can use times obtained from `current-time'
and from `file-attributes'.  */)
     (specified_time)
     Lisp_Object specified_time;
{
  time_t value;
  char buf[30];
  register char *tem;

  if (! lisp_time_argument (specified_time, &value, NULL))
    value = -1;
  tem = (char *) ctime (&value);

  strncpy (buf, tem, 24);
  buf[24] = 0;

  return build_string (buf);
}

#define TM_YEAR_BASE 1900

/* Yield A - B, measured in seconds.
   This function is copied from the GNU C Library.  */
static int
tm_diff (a, b)
     struct tm *a, *b;
{
  /* Compute intervening leap days correctly even if year is negative.
     Take care to avoid int overflow in leap day calculations,
     but it's OK to assume that A and B are close to each other.  */
  int a4 = (a->tm_year >> 2) + (TM_YEAR_BASE >> 2) - ! (a->tm_year & 3);
  int b4 = (b->tm_year >> 2) + (TM_YEAR_BASE >> 2) - ! (b->tm_year & 3);
  int a100 = a4 / 25 - (a4 % 25 < 0);
  int b100 = b4 / 25 - (b4 % 25 < 0);
  int a400 = a100 >> 2;
  int b400 = b100 >> 2;
  int intervening_leap_days = (a4 - b4) - (a100 - b100) + (a400 - b400);
  int years = a->tm_year - b->tm_year;
  int days = (365 * years + intervening_leap_days
	      + (a->tm_yday - b->tm_yday));
  return (60 * (60 * (24 * days + (a->tm_hour - b->tm_hour))
		+ (a->tm_min - b->tm_min))
	  + (a->tm_sec - b->tm_sec));
}

DEFUN ("current-time-zone", Fcurrent_time_zone, Scurrent_time_zone, 0, 1, 0,
       doc: /* Return the offset and name for the local time zone.
This returns a list of the form (OFFSET NAME).
OFFSET is an integer number of seconds ahead of UTC (east of Greenwich).
    A negative value means west of Greenwich.
NAME is a string giving the name of the time zone.
If an argument is given, it specifies when the time zone offset is determined
instead of using the current time.  The argument should have the form:
  (HIGH . LOW)
or the form:
  (HIGH LOW . IGNORED).
Thus, you can use times obtained from `current-time'
and from `file-attributes'.

Some operating systems cannot provide all this information to Emacs;
in this case, `current-time-zone' returns a list containing nil for
the data it can't find.  */)
     (specified_time)
     Lisp_Object specified_time;
{
  time_t value;
  struct tm *t;
  struct tm gmt;

  if (lisp_time_argument (specified_time, &value, NULL)
      && (t = gmtime (&value)) != 0
      && (gmt = *t, t = localtime (&value)) != 0)
    {
      int offset = tm_diff (t, &gmt);
      char *s = 0;
      char buf[6];
#ifdef HAVE_TM_ZONE
      if (t->tm_zone)
	s = (char *)t->tm_zone;
#else /* not HAVE_TM_ZONE */
#ifdef HAVE_TZNAME
      if (t->tm_isdst == 0 || t->tm_isdst == 1)
	s = tzname[t->tm_isdst];
#endif
#endif /* not HAVE_TM_ZONE */

#if defined HAVE_TM_ZONE || defined HAVE_TZNAME
      if (s)
	{
	  /* On Japanese w32, we can get a Japanese string as time
	     zone name.  Don't accept that.  */
	  char *p;
	  for (p = s; *p && (isalnum ((unsigned char)*p) || *p == ' '); ++p)
	    ;
	  if (p == s || *p)
	    s = NULL;
	}
#endif

      if (!s)
	{
	  /* No local time zone name is available; use "+-NNNN" instead.  */
	  int am = (offset < 0 ? -offset : offset) / 60;
	  sprintf (buf, "%c%02d%02d", (offset < 0 ? '-' : '+'), am/60, am%60);
	  s = buf;
	}
      return Fcons (make_number (offset), Fcons (build_string (s), Qnil));
    }
  else
    return Fmake_list (make_number (2), Qnil);
}

/* This holds the value of `environ' produced by the previous
   call to Fset_time_zone_rule, or 0 if Fset_time_zone_rule
   has never been called.  */
static char **environbuf;

DEFUN ("set-time-zone-rule", Fset_time_zone_rule, Sset_time_zone_rule, 1, 1, 0,
       doc: /* Set the local time zone using TZ, a string specifying a time zone rule.
If TZ is nil, use implementation-defined default time zone information.
If TZ is t, use Universal Time.  */)
     (tz)
     Lisp_Object tz;
{
  char *tzstring;

  if (NILP (tz))
    tzstring = 0;
  else if (EQ (tz, Qt))
    tzstring = "UTC0";
  else
    {
      CHECK_STRING (tz);
      tzstring = (char *) XSTRING (tz)->data;
    }

  set_time_zone_rule (tzstring);
  if (environbuf)
    free (environbuf);
  environbuf = environ;

  return Qnil;
}

#ifdef LOCALTIME_CACHE

/* These two values are known to load tz files in buggy implementations,
   i.e. Solaris 1 executables running under either Solaris 1 or Solaris 2.
   Their values shouldn't matter in non-buggy implementations.
   We don't use string literals for these strings,
   since if a string in the environment is in readonly
   storage, it runs afoul of bugs in SVR4 and Solaris 2.3.
   See Sun bugs 1113095 and 1114114, ``Timezone routines
   improperly modify environment''.  */

static char set_time_zone_rule_tz1[] = "TZ=GMT+0";
static char set_time_zone_rule_tz2[] = "TZ=GMT+1";

#endif

/* Set the local time zone rule to TZSTRING.
   This allocates memory into `environ', which it is the caller's
   responsibility to free.  */

void
set_time_zone_rule (tzstring)
     char *tzstring;
{
  int envptrs;
  char **from, **to, **newenv;

  /* Make the ENVIRON vector longer with room for TZSTRING.  */
  for (from = environ; *from; from++)
    continue;
  envptrs = from - environ + 2;
  newenv = to = (char **) xmalloc (envptrs * sizeof (char *)
				   + (tzstring ? strlen (tzstring) + 4 : 0));

  /* Add TZSTRING to the end of environ, as a value for TZ.  */
  if (tzstring)
    {
      char *t = (char *) (to + envptrs);
      strcpy (t, "TZ=");
      strcat (t, tzstring);
      *to++ = t;
    }

  /* Copy the old environ vector elements into NEWENV,
     but don't copy the TZ variable.
     So we have only one definition of TZ, which came from TZSTRING.  */
  for (from = environ; *from; from++)
    if (strncmp (*from, "TZ=", 3) != 0)
      *to++ = *from;
  *to = 0;

  environ = newenv;

  /* If we do have a TZSTRING, NEWENV points to the vector slot where
     the TZ variable is stored.  If we do not have a TZSTRING,
     TO points to the vector slot which has the terminating null.  */

#ifdef LOCALTIME_CACHE
  {
    /* In SunOS 4.1.3_U1 and 4.1.4, if TZ has a value like
       "US/Pacific" that loads a tz file, then changes to a value like
       "XXX0" that does not load a tz file, and then changes back to
       its original value, the last change is (incorrectly) ignored.
       Also, if TZ changes twice in succession to values that do
       not load a tz file, tzset can dump core (see Sun bug#1225179).
       The following code works around these bugs.  */

    if (tzstring)
      {
	/* Temporarily set TZ to a value that loads a tz file
	   and that differs from tzstring.  */
	char *tz = *newenv;
	*newenv = (strcmp (tzstring, set_time_zone_rule_tz1 + 3) == 0
		   ? set_time_zone_rule_tz2 : set_time_zone_rule_tz1);
	tzset ();
	*newenv = tz;
      }
    else
      {
	/* The implied tzstring is unknown, so temporarily set TZ to
	   two different values that each load a tz file.  */
	*to = set_time_zone_rule_tz1;
	to[1] = 0;
	tzset ();
	*to = set_time_zone_rule_tz2;
	tzset ();
	*to = 0;
      }

    /* Now TZ has the desired value, and tzset can be invoked safely.  */
  }

  tzset ();
#endif
}

/* Insert NARGS Lisp objects in the array ARGS by calling INSERT_FUNC
   (if a type of object is Lisp_Int) or INSERT_FROM_STRING_FUNC (if a
   type of object is Lisp_String).  INHERIT is passed to
   INSERT_FROM_STRING_FUNC as the last argument.  */

static void
general_insert_function (insert_func, insert_from_string_func,
			 inherit, nargs, args)
     void (*insert_func) P_ ((unsigned char *, int));
     void (*insert_from_string_func) P_ ((Lisp_Object, int, int, int, int, int));
     int inherit, nargs;
     register Lisp_Object *args;
{
  register int argnum;
  register Lisp_Object val;

  for (argnum = 0; argnum < nargs; argnum++)
    {
      val = args[argnum];
    retry:
      if (INTEGERP (val))
	{
	  unsigned char str[MAX_MULTIBYTE_LENGTH];
	  int len;

	  if (!NILP (current_buffer->enable_multibyte_characters))
	    len = CHAR_STRING (XFASTINT (val), str);
	  else
	    {
	      str[0] = (SINGLE_BYTE_CHAR_P (XINT (val))
			? XINT (val)
			: multibyte_char_to_unibyte (XINT (val), Qnil));
	      len = 1;
	    }
	  (*insert_func) (str, len);
	}
      else if (STRINGP (val))
	{
	  (*insert_from_string_func) (val, 0, 0,
				      XSTRING (val)->size,
				      STRING_BYTES (XSTRING (val)),
				      inherit);
	}
      else
	{
	  val = wrong_type_argument (Qchar_or_string_p, val);
	  goto retry;
	}
    }
}

void
insert1 (arg)
     Lisp_Object arg;
{
  Finsert (1, &arg);
}


/* Callers passing one argument to Finsert need not gcpro the
   argument "array", since the only element of the array will
   not be used after calling insert or insert_from_string, so
   we don't care if it gets trashed.  */

DEFUN ("insert", Finsert, Sinsert, 0, MANY, 0,
       doc: /* Insert the arguments, either strings or characters, at point.
Point and before-insertion markers move forward to end up
 after the inserted text.
Any other markers at the point of insertion remain before the text.

If the current buffer is multibyte, unibyte strings are converted
to multibyte for insertion (see `unibyte-char-to-multibyte').
If the current buffer is unibyte, multibyte strings are converted
to unibyte for insertion.

usage: (insert &rest ARGS)  */)
     (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  general_insert_function (insert, insert_from_string, 0, nargs, args);
  return Qnil;
}

DEFUN ("insert-and-inherit", Finsert_and_inherit, Sinsert_and_inherit,
   0, MANY, 0,
       doc: /* Insert the arguments at point, inheriting properties from adjoining text.
Point and before-insertion markers move forward to end up
 after the inserted text.
Any other markers at the point of insertion remain before the text.

If the current buffer is multibyte, unibyte strings are converted
to multibyte for insertion (see `unibyte-char-to-multibyte').
If the current buffer is unibyte, multibyte strings are converted
to unibyte for insertion.

usage: (insert-and-inherit &rest ARGS)  */)
     (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  general_insert_function (insert_and_inherit, insert_from_string, 1,
			   nargs, args);
  return Qnil;
}

DEFUN ("insert-before-markers", Finsert_before_markers, Sinsert_before_markers, 0, MANY, 0,
       doc: /* Insert strings or characters at point, relocating markers after the text.
Point and markers move forward to end up after the inserted text.

If the current buffer is multibyte, unibyte strings are converted
to multibyte for insertion (see `unibyte-char-to-multibyte').
If the current buffer is unibyte, multibyte strings are converted
to unibyte for insertion.

usage: (insert-before-markers &rest ARGS)  */)
     (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  general_insert_function (insert_before_markers,
			   insert_from_string_before_markers, 0,
			   nargs, args);
  return Qnil;
}

DEFUN ("insert-before-markers-and-inherit", Finsert_and_inherit_before_markers,
  Sinsert_and_inherit_before_markers, 0, MANY, 0,
       doc: /* Insert text at point, relocating markers and inheriting properties.
Point and markers move forward to end up after the inserted text.

If the current buffer is multibyte, unibyte strings are converted
to multibyte for insertion (see `unibyte-char-to-multibyte').
If the current buffer is unibyte, multibyte strings are converted
to unibyte for insertion.

usage: (insert-before-markers-and-inherit &rest ARGS)  */)
     (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  general_insert_function (insert_before_markers_and_inherit,
			   insert_from_string_before_markers, 1,
			   nargs, args);
  return Qnil;
}

DEFUN ("insert-char", Finsert_char, Sinsert_char, 2, 3, 0,
       doc: /* Insert COUNT (second arg) copies of CHARACTER (first arg).
Both arguments are required.
Point, and before-insertion markers, are relocated as in the function `insert'.
The optional third arg INHERIT, if non-nil, says to inherit text properties
from adjoining text, if those properties are sticky.  */)
     (character, count, inherit)
       Lisp_Object character, count, inherit;
{
  register unsigned char *string;
  register int strlen;
  register int i, n;
  int len;
  unsigned char str[MAX_MULTIBYTE_LENGTH];

  CHECK_NUMBER (character);
  CHECK_NUMBER (count);

  if (!NILP (current_buffer->enable_multibyte_characters))
    len = CHAR_STRING (XFASTINT (character), str);
  else
    str[0] = XFASTINT (character), len = 1;
  n = XINT (count) * len;
  if (n <= 0)
    return Qnil;
  strlen = min (n, 256 * len);
  string = (unsigned char *) alloca (strlen);
  for (i = 0; i < strlen; i++)
    string[i] = str[i % len];
  while (n >= strlen)
    {
      QUIT;
      if (!NILP (inherit))
	insert_and_inherit (string, strlen);
      else
	insert (string, strlen);
      n -= strlen;
    }
  if (n > 0)
    {
      if (!NILP (inherit))
	insert_and_inherit (string, n);
      else
	insert (string, n);
    }
  return Qnil;
}


/* Making strings from buffer contents.  */

/* Return a Lisp_String containing the text of the current buffer from
   START to END.  If text properties are in use and the current buffer
   has properties in the range specified, the resulting string will also
   have them, if PROPS is nonzero.

   We don't want to use plain old make_string here, because it calls
   make_uninit_string, which can cause the buffer arena to be
   compacted.  make_string has no way of knowing that the data has
   been moved, and thus copies the wrong data into the string.  This
   doesn't effect most of the other users of make_string, so it should
   be left as is.  But we should use this function when conjuring
   buffer substrings.  */

Lisp_Object
make_buffer_string (start, end, props)
     int start, end;
     int props;
{
  int start_byte = CHAR_TO_BYTE (start);
  int end_byte = CHAR_TO_BYTE (end);

  return make_buffer_string_both (start, start_byte, end, end_byte, props);
}

/* Return a Lisp_String containing the text of the current buffer from
   START / START_BYTE to END / END_BYTE.

   If text properties are in use and the current buffer
   has properties in the range specified, the resulting string will also
   have them, if PROPS is nonzero.

   We don't want to use plain old make_string here, because it calls
   make_uninit_string, which can cause the buffer arena to be
   compacted.  make_string has no way of knowing that the data has
   been moved, and thus copies the wrong data into the string.  This
   doesn't effect most of the other users of make_string, so it should
   be left as is.  But we should use this function when conjuring
   buffer substrings.  */

Lisp_Object
make_buffer_string_both (start, start_byte, end, end_byte, props)
     int start, start_byte, end, end_byte;
     int props;
{
  Lisp_Object result, tem, tem1;

  if (start < GPT && GPT < end)
    move_gap (start);

  if (! NILP (current_buffer->enable_multibyte_characters))
    result = make_uninit_multibyte_string (end - start, end_byte - start_byte);
  else
    result = make_uninit_string (end - start);
  bcopy (BYTE_POS_ADDR (start_byte), XSTRING (result)->data,
	 end_byte - start_byte);

  /* If desired, update and copy the text properties.  */
  if (props)
    {
      update_buffer_properties (start, end);

      tem = Fnext_property_change (make_number (start), Qnil, make_number (end));
      tem1 = Ftext_properties_at (make_number (start), Qnil);

      if (XINT (tem) != end || !NILP (tem1))
	copy_intervals_to_string (result, current_buffer, start,
				  end - start);
    }

  return result;
}

/* Call Vbuffer_access_fontify_functions for the range START ... END
   in the current buffer, if necessary.  */

static void
update_buffer_properties (start, end)
     int start, end;
{
  /* If this buffer has some access functions,
     call them, specifying the range of the buffer being accessed.  */
  if (!NILP (Vbuffer_access_fontify_functions))
    {
      Lisp_Object args[3];
      Lisp_Object tem;

      args[0] = Qbuffer_access_fontify_functions;
      XSETINT (args[1], start);
      XSETINT (args[2], end);

      /* But don't call them if we can tell that the work
	 has already been done.  */
      if (!NILP (Vbuffer_access_fontified_property))
	{
	  tem = Ftext_property_any (args[1], args[2],
				    Vbuffer_access_fontified_property,
				    Qnil, Qnil);
	  if (! NILP (tem))
	    Frun_hook_with_args (3, args);
	}
      else
	Frun_hook_with_args (3, args);
    }
}

DEFUN ("buffer-substring", Fbuffer_substring, Sbuffer_substring, 2, 2, 0,
       doc: /* Return the contents of part of the current buffer as a string.
The two arguments START and END are character positions;
they can be in either order.
The string returned is multibyte if the buffer is multibyte.

This function copies the text properties of that part of the buffer
into the result string; if you don't want the text properties,
use `buffer-substring-no-properties' instead.  */)
     (start, end)
     Lisp_Object start, end;
{
  register int b, e;

  validate_region (&start, &end);
  b = XINT (start);
  e = XINT (end);

  return make_buffer_string (b, e, 1);
}

DEFUN ("buffer-substring-no-properties", Fbuffer_substring_no_properties,
       Sbuffer_substring_no_properties, 2, 2, 0,
       doc: /* Return the characters of part of the buffer, without the text properties.
The two arguments START and END are character positions;
they can be in either order.  */)
     (start, end)
     Lisp_Object start, end;
{
  register int b, e;

  validate_region (&start, &end);
  b = XINT (start);
  e = XINT (end);

  return make_buffer_string (b, e, 0);
}

DEFUN ("buffer-string", Fbuffer_string, Sbuffer_string, 0, 0, 0,
       doc: /* Return the contents of the current buffer as a string.
If narrowing is in effect, this function returns only the visible part
of the buffer.  */)
     ()
{
  return make_buffer_string (BEGV, ZV, 1);
}

DEFUN ("insert-buffer-substring", Finsert_buffer_substring, Sinsert_buffer_substring,
       1, 3, 0,
       doc: /* Insert before point a substring of the contents of buffer BUFFER.
BUFFER may be a buffer or a buffer name.
Arguments START and END are character numbers specifying the substring.
They default to the beginning and the end of BUFFER.  */)
     (buf, start, end)
     Lisp_Object buf, start, end;
{
  register int b, e, temp;
  register struct buffer *bp, *obuf;
  Lisp_Object buffer;

  buffer = Fget_buffer (buf);
  if (NILP (buffer))
    nsberror (buf);
  bp = XBUFFER (buffer);
  if (NILP (bp->name))
    error ("Selecting deleted buffer");

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

  obuf = current_buffer;
  set_buffer_internal_1 (bp);
  update_buffer_properties (b, e);
  set_buffer_internal_1 (obuf);

  insert_from_buffer (bp, b, e - b, 0);
  return Qnil;
}

DEFUN ("compare-buffer-substrings", Fcompare_buffer_substrings, Scompare_buffer_substrings,
       6, 6, 0,
       doc: /* Compare two substrings of two buffers; return result as number.
the value is -N if first string is less after N-1 chars,
+N if first string is greater after N-1 chars, or 0 if strings match.
Each substring is represented as three arguments: BUFFER, START and END.
That makes six args in all, three for each substring.

The value of `case-fold-search' in the current buffer
determines whether case is significant or ignored.  */)
     (buffer1, start1, end1, buffer2, start2, end2)
     Lisp_Object buffer1, start1, end1, buffer2, start2, end2;
{
  register int begp1, endp1, begp2, endp2, temp;
  register struct buffer *bp1, *bp2;
  register Lisp_Object *trt
    = (!NILP (current_buffer->case_fold_search)
       ? XCHAR_TABLE (current_buffer->case_canon_table)->contents : 0);
  int chars = 0;
  int i1, i2, i1_byte, i2_byte;

  /* Find the first buffer and its substring.  */

  if (NILP (buffer1))
    bp1 = current_buffer;
  else
    {
      Lisp_Object buf1;
      buf1 = Fget_buffer (buffer1);
      if (NILP (buf1))
	nsberror (buffer1);
      bp1 = XBUFFER (buf1);
      if (NILP (bp1->name))
	error ("Selecting deleted buffer");
    }

  if (NILP (start1))
    begp1 = BUF_BEGV (bp1);
  else
    {
      CHECK_NUMBER_COERCE_MARKER (start1);
      begp1 = XINT (start1);
    }
  if (NILP (end1))
    endp1 = BUF_ZV (bp1);
  else
    {
      CHECK_NUMBER_COERCE_MARKER (end1);
      endp1 = XINT (end1);
    }

  if (begp1 > endp1)
    temp = begp1, begp1 = endp1, endp1 = temp;

  if (!(BUF_BEGV (bp1) <= begp1
	&& begp1 <= endp1
        && endp1 <= BUF_ZV (bp1)))
    args_out_of_range (start1, end1);

  /* Likewise for second substring.  */

  if (NILP (buffer2))
    bp2 = current_buffer;
  else
    {
      Lisp_Object buf2;
      buf2 = Fget_buffer (buffer2);
      if (NILP (buf2))
	nsberror (buffer2);
      bp2 = XBUFFER (buf2);
      if (NILP (bp2->name))
	error ("Selecting deleted buffer");
    }

  if (NILP (start2))
    begp2 = BUF_BEGV (bp2);
  else
    {
      CHECK_NUMBER_COERCE_MARKER (start2);
      begp2 = XINT (start2);
    }
  if (NILP (end2))
    endp2 = BUF_ZV (bp2);
  else
    {
      CHECK_NUMBER_COERCE_MARKER (end2);
      endp2 = XINT (end2);
    }

  if (begp2 > endp2)
    temp = begp2, begp2 = endp2, endp2 = temp;

  if (!(BUF_BEGV (bp2) <= begp2
	&& begp2 <= endp2
        && endp2 <= BUF_ZV (bp2)))
    args_out_of_range (start2, end2);

  i1 = begp1;
  i2 = begp2;
  i1_byte = buf_charpos_to_bytepos (bp1, i1);
  i2_byte = buf_charpos_to_bytepos (bp2, i2);

  while (i1 < endp1 && i2 < endp2)
    {
      /* When we find a mismatch, we must compare the
	 characters, not just the bytes.  */
      int c1, c2;

      QUIT;

      if (! NILP (bp1->enable_multibyte_characters))
	{
	  c1 = BUF_FETCH_MULTIBYTE_CHAR (bp1, i1_byte);
	  BUF_INC_POS (bp1, i1_byte);
	  i1++;
	}
      else
	{
	  c1 = BUF_FETCH_BYTE (bp1, i1);
	  c1 = unibyte_char_to_multibyte (c1);
	  i1++;
	}

      if (! NILP (bp2->enable_multibyte_characters))
	{
	  c2 = BUF_FETCH_MULTIBYTE_CHAR (bp2, i2_byte);
	  BUF_INC_POS (bp2, i2_byte);
	  i2++;
	}
      else
	{
	  c2 = BUF_FETCH_BYTE (bp2, i2);
	  c2 = unibyte_char_to_multibyte (c2);
	  i2++;
	}

      if (trt)
	{
	  c1 = XINT (trt[c1]);
	  c2 = XINT (trt[c2]);
	}
      if (c1 < c2)
	return make_number (- 1 - chars);
      if (c1 > c2)
	return make_number (chars + 1);

      chars++;
    }

  /* The strings match as far as they go.
     If one is shorter, that one is less.  */
  if (chars < endp1 - begp1)
    return make_number (chars + 1);
  else if (chars < endp2 - begp2)
    return make_number (- chars - 1);

  /* Same length too => they are equal.  */
  return make_number (0);
}

static Lisp_Object
subst_char_in_region_unwind (arg)
     Lisp_Object arg;
{
  return current_buffer->undo_list = arg;
}

static Lisp_Object
subst_char_in_region_unwind_1 (arg)
     Lisp_Object arg;
{
  return current_buffer->filename = arg;
}

DEFUN ("subst-char-in-region", Fsubst_char_in_region,
       Ssubst_char_in_region, 4, 5, 0,
       doc: /* From START to END, replace FROMCHAR with TOCHAR each time it occurs.
If optional arg NOUNDO is non-nil, don't record this change for undo
and don't mark the buffer as really changed.
Both characters must have the same length of multi-byte form.  */)
     (start, end, fromchar, tochar, noundo)
     Lisp_Object start, end, fromchar, tochar, noundo;
{
  register int pos, pos_byte, stop, i, len, end_byte;
  int changed = 0;
  unsigned char fromstr[MAX_MULTIBYTE_LENGTH], tostr[MAX_MULTIBYTE_LENGTH];
  unsigned char *p;
  int count = specpdl_ptr - specpdl;
#define COMBINING_NO	 0
#define COMBINING_BEFORE 1
#define COMBINING_AFTER  2
#define COMBINING_BOTH (COMBINING_BEFORE | COMBINING_AFTER)
  int maybe_byte_combining = COMBINING_NO;
  int last_changed = 0;
  int multibyte_p = !NILP (current_buffer->enable_multibyte_characters);

  validate_region (&start, &end);
  CHECK_NUMBER (fromchar);
  CHECK_NUMBER (tochar);

  if (multibyte_p)
    {
      len = CHAR_STRING (XFASTINT (fromchar), fromstr);
      if (CHAR_STRING (XFASTINT (tochar), tostr) != len)
	error ("Characters in subst-char-in-region have different byte-lengths");
      if (!ASCII_BYTE_P (*tostr))
	{
	  /* If *TOSTR is in the range 0x80..0x9F and TOCHAR is not a
	     complete multibyte character, it may be combined with the
	     after bytes.  If it is in the range 0xA0..0xFF, it may be
	     combined with the before and after bytes.  */
	  if (!CHAR_HEAD_P (*tostr))
	    maybe_byte_combining = COMBINING_BOTH;
	  else if (BYTES_BY_CHAR_HEAD (*tostr) > len)
	    maybe_byte_combining = COMBINING_AFTER;
	}
    }
  else
    {
      len = 1;
      fromstr[0] = XFASTINT (fromchar);
      tostr[0] = XFASTINT (tochar);
    }

  pos = XINT (start);
  pos_byte = CHAR_TO_BYTE (pos);
  stop = CHAR_TO_BYTE (XINT (end));
  end_byte = stop;

  /* If we don't want undo, turn off putting stuff on the list.
     That's faster than getting rid of things,
     and it prevents even the entry for a first change.
     Also inhibit locking the file.  */
  if (!NILP (noundo))
    {
      record_unwind_protect (subst_char_in_region_unwind,
			     current_buffer->undo_list);
      current_buffer->undo_list = Qt;
      /* Don't do file-locking.  */
      record_unwind_protect (subst_char_in_region_unwind_1,
			     current_buffer->filename);
      current_buffer->filename = Qnil;
    }

  if (pos_byte < GPT_BYTE)
    stop = min (stop, GPT_BYTE);
  while (1)
    {
      int pos_byte_next = pos_byte;

      if (pos_byte >= stop)
	{
	  if (pos_byte >= end_byte) break;
	  stop = end_byte;
	}
      p = BYTE_POS_ADDR (pos_byte);
      if (multibyte_p)
	INC_POS (pos_byte_next);
      else
	++pos_byte_next;
      if (pos_byte_next - pos_byte == len
	  && p[0] == fromstr[0]
	  && (len == 1
	      || (p[1] == fromstr[1]
		  && (len == 2 || (p[2] == fromstr[2]
				 && (len == 3 || p[3] == fromstr[3]))))))
	{
	  if (! changed)
	    {
	      changed = pos;
	      modify_region (current_buffer, changed, XINT (end));

	      if (! NILP (noundo))
		{
		  if (MODIFF - 1 == SAVE_MODIFF)
		    SAVE_MODIFF++;
		  if (MODIFF - 1 == current_buffer->auto_save_modified)
		    current_buffer->auto_save_modified++;
		}
	    }

	  /* Take care of the case where the new character
	     combines with neighboring bytes.  */
	  if (maybe_byte_combining
	      && (maybe_byte_combining == COMBINING_AFTER
		  ? (pos_byte_next < Z_BYTE
		     && ! CHAR_HEAD_P (FETCH_BYTE (pos_byte_next)))
		  : ((pos_byte_next < Z_BYTE
		      && ! CHAR_HEAD_P (FETCH_BYTE (pos_byte_next)))
		     || (pos_byte > BEG_BYTE
			 && ! ASCII_BYTE_P (FETCH_BYTE (pos_byte - 1))))))
	    {
	      Lisp_Object tem, string;

	      struct gcpro gcpro1;

	      tem = current_buffer->undo_list;
	      GCPRO1 (tem);

	      /* Make a multibyte string containing this single character.  */
	      string = make_multibyte_string (tostr, 1, len);
	      /* replace_range is less efficient, because it moves the gap,
		 but it handles combining correctly.  */
	      replace_range (pos, pos + 1, string,
			     0, 0, 1);
	      pos_byte_next = CHAR_TO_BYTE (pos);
	      if (pos_byte_next > pos_byte)
		/* Before combining happened.  We should not increment
		   POS.  So, to cancel the later increment of POS,
		   decrease it now.  */
		pos--;
	      else
		INC_POS (pos_byte_next);

	      if (! NILP (noundo))
		current_buffer->undo_list = tem;

	      UNGCPRO;
	    }
	  else
	    {
	      if (NILP (noundo))
		record_change (pos, 1);
	      for (i = 0; i < len; i++) *p++ = tostr[i];
	    }
	  last_changed =  pos + 1;
	}
      pos_byte = pos_byte_next;
      pos++;
    }

  if (changed)
    {
      signal_after_change (changed,
			   last_changed - changed, last_changed - changed);
      update_compositions (changed, last_changed, CHECK_ALL);
    }

  unbind_to (count, Qnil);
  return Qnil;
}

DEFUN ("translate-region", Ftranslate_region, Stranslate_region, 3, 3, 0,
       doc: /* From START to END, translate characters according to TABLE.
TABLE is a string; the Nth character in it is the mapping
for the character with code N.
This function does not alter multibyte characters.
It returns the number of characters changed.  */)
     (start, end, table)
     Lisp_Object start;
     Lisp_Object end;
     register Lisp_Object table;
{
  register int pos_byte, stop;	/* Limits of the region. */
  register unsigned char *tt;	/* Trans table. */
  register int nc;		/* New character. */
  int cnt;			/* Number of changes made. */
  int size;			/* Size of translate table. */
  int pos;
  int multibyte = !NILP (current_buffer->enable_multibyte_characters);

  validate_region (&start, &end);
  CHECK_STRING (table);

  size = STRING_BYTES (XSTRING (table));
  tt = XSTRING (table)->data;

  pos_byte = CHAR_TO_BYTE (XINT (start));
  stop = CHAR_TO_BYTE (XINT (end));
  modify_region (current_buffer, XINT (start), XINT (end));
  pos = XINT (start);

  cnt = 0;
  for (; pos_byte < stop; )
    {
      register unsigned char *p = BYTE_POS_ADDR (pos_byte);
      int len;
      int oc;
      int pos_byte_next;

      if (multibyte)
	oc = STRING_CHAR_AND_LENGTH (p, stop - pos_byte, len);
      else
	oc = *p, len = 1;
      pos_byte_next = pos_byte + len;
      if (oc < size && len == 1)
	{
	  nc = tt[oc];
	  if (nc != oc)
	    {
	      /* Take care of the case where the new character
		 combines with neighboring bytes.  */
	      if (!ASCII_BYTE_P (nc)
		  && (CHAR_HEAD_P (nc)
		      ? ! CHAR_HEAD_P (FETCH_BYTE (pos_byte + 1))
		      : (pos_byte > BEG_BYTE
			 && ! ASCII_BYTE_P (FETCH_BYTE (pos_byte - 1)))))
		{
		  Lisp_Object string;

		  string = make_multibyte_string (tt + oc, 1, 1);
		  /* This is less efficient, because it moves the gap,
		     but it handles combining correctly.  */
		  replace_range (pos, pos + 1, string,
				 1, 0, 1);
		  pos_byte_next = CHAR_TO_BYTE (pos);
		  if (pos_byte_next > pos_byte)
		    /* Before combining happened.  We should not
		       increment POS.  So, to cancel the later
		       increment of POS, we decrease it now.  */
		    pos--;
		  else
		    INC_POS (pos_byte_next);
		}
	      else
		{
		  record_change (pos, 1);
		  *p = nc;
		  signal_after_change (pos, 1, 1);
		  update_compositions (pos, pos + 1, CHECK_BORDER);
		}
	      ++cnt;
	    }
	}
      pos_byte = pos_byte_next;
      pos++;
    }

  return make_number (cnt);
}

DEFUN ("delete-region", Fdelete_region, Sdelete_region, 2, 2, "r",
       doc: /* Delete the text between point and mark.
When called from a program, expects two arguments,
positions (integers or markers) specifying the stretch to be deleted.  */)
     (start, end)
     Lisp_Object start, end;
{
  validate_region (&start, &end);
  del_range (XINT (start), XINT (end));
  return Qnil;
}

DEFUN ("delete-and-extract-region", Fdelete_and_extract_region,
       Sdelete_and_extract_region, 2, 2, 0,
       doc: /* Delete the text between START and END and return it.  */)
     (start, end)
     Lisp_Object start, end;
{
  validate_region (&start, &end);
  return del_range_1 (XINT (start), XINT (end), 1, 1);
}

DEFUN ("widen", Fwiden, Swiden, 0, 0, "",
       doc: /* Remove restrictions (narrowing) from current buffer.
This allows the buffer's full text to be seen and edited.  */)
     ()
{
  if (BEG != BEGV || Z != ZV)
    current_buffer->clip_changed = 1;
  BEGV = BEG;
  BEGV_BYTE = BEG_BYTE;
  SET_BUF_ZV_BOTH (current_buffer, Z, Z_BYTE);
  /* Changing the buffer bounds invalidates any recorded current column.  */
  invalidate_current_column ();
  return Qnil;
}

DEFUN ("narrow-to-region", Fnarrow_to_region, Snarrow_to_region, 2, 2, "r",
       doc: /* Restrict editing in this buffer to the current region.
The rest of the text becomes temporarily invisible and untouchable
but is not deleted; if you save the buffer in a file, the invisible
text is included in the file.  \\[widen] makes all visible again.
See also `save-restriction'.

When calling from a program, pass two arguments; positions (integers
or markers) bounding the text that should remain visible.  */)
     (start, end)
     register Lisp_Object start, end;
{
  CHECK_NUMBER_COERCE_MARKER (start);
  CHECK_NUMBER_COERCE_MARKER (end);

  if (XINT (start) > XINT (end))
    {
      Lisp_Object tem;
      tem = start; start = end; end = tem;
    }

  if (!(BEG <= XINT (start) && XINT (start) <= XINT (end) && XINT (end) <= Z))
    args_out_of_range (start, end);

  if (BEGV != XFASTINT (start) || ZV != XFASTINT (end))
    current_buffer->clip_changed = 1;

  SET_BUF_BEGV (current_buffer, XFASTINT (start));
  SET_BUF_ZV (current_buffer, XFASTINT (end));
  if (PT < XFASTINT (start))
    SET_PT (XFASTINT (start));
  if (PT > XFASTINT (end))
    SET_PT (XFASTINT (end));
  /* Changing the buffer bounds invalidates any recorded current column.  */
  invalidate_current_column ();
  return Qnil;
}

Lisp_Object
save_restriction_save ()
{
  if (BEGV == BEG && ZV == Z)
    /* The common case that the buffer isn't narrowed.
       We return just the buffer object, which save_restriction_restore
       recognizes as meaning `no restriction'.  */
    return Fcurrent_buffer ();
  else
    /* We have to save a restriction, so return a pair of markers, one
       for the beginning and one for the end.  */
    {
      Lisp_Object beg, end;

      beg = buildmark (BEGV, BEGV_BYTE);
      end = buildmark (ZV, ZV_BYTE);

      /* END must move forward if text is inserted at its exact location.  */
      XMARKER(end)->insertion_type = 1;

      return Fcons (beg, end);
    }
}

Lisp_Object
save_restriction_restore (data)
     Lisp_Object data;
{
  if (CONSP (data))
    /* A pair of marks bounding a saved restriction.  */
    {
      struct Lisp_Marker *beg = XMARKER (XCAR (data));
      struct Lisp_Marker *end = XMARKER (XCDR (data));
      struct buffer *buf = beg->buffer; /* END should have the same buffer. */

      if (beg->charpos != BUF_BEGV(buf) || end->charpos != BUF_ZV(buf))
	/* The restriction has changed from the saved one, so restore
	   the saved restriction.  */
	{
	  int pt = BUF_PT (buf);

	  SET_BUF_BEGV_BOTH (buf, beg->charpos, beg->bytepos);
	  SET_BUF_ZV_BOTH (buf, end->charpos, end->bytepos);

	  if (pt < beg->charpos || pt > end->charpos)
	    /* The point is outside the new visible range, move it inside. */
	    SET_BUF_PT_BOTH (buf,
			     clip_to_bounds (beg->charpos, pt, end->charpos),
			     clip_to_bounds (beg->bytepos, BUF_PT_BYTE(buf),
					     end->bytepos));
	  
	  buf->clip_changed = 1; /* Remember that the narrowing changed. */
	}
    }
  else
    /* A buffer, which means that there was no old restriction.  */
    {
      struct buffer *buf = XBUFFER (data);

      if (BUF_BEGV(buf) != BUF_BEG(buf) || BUF_ZV(buf) != BUF_Z(buf))
	/* The buffer has been narrowed, get rid of the narrowing.  */
	{
	  SET_BUF_BEGV_BOTH (buf, BUF_BEG(buf), BUF_BEG_BYTE(buf));
	  SET_BUF_ZV_BOTH (buf, BUF_Z(buf), BUF_Z_BYTE(buf));

	  buf->clip_changed = 1; /* Remember that the narrowing changed. */
	}
    }

  return Qnil;
}

DEFUN ("save-restriction", Fsave_restriction, Ssave_restriction, 0, UNEVALLED, 0,
       doc: /* Execute BODY, saving and restoring current buffer's restrictions.
The buffer's restrictions make parts of the beginning and end invisible.
(They are set up with `narrow-to-region' and eliminated with `widen'.)
This special form, `save-restriction', saves the current buffer's restrictions
when it is entered, and restores them when it is exited.
So any `narrow-to-region' within BODY lasts only until the end of the form.
The old restrictions settings are restored
even in case of abnormal exit (throw or error).

The value returned is the value of the last form in BODY.

Note: if you are using both `save-excursion' and `save-restriction',
use `save-excursion' outermost:
    (save-excursion (save-restriction ...))

usage: (save-restriction &rest BODY)  */)
     (body)
     Lisp_Object body;
{
  register Lisp_Object val;
  int count = specpdl_ptr - specpdl;

  record_unwind_protect (save_restriction_restore, save_restriction_save ());
  val = Fprogn (body);
  return unbind_to (count, val);
}

/* Buffer for the most recent text displayed by Fmessage_box.  */
static char *message_text;

/* Allocated length of that buffer.  */
static int message_length;

DEFUN ("message", Fmessage, Smessage, 1, MANY, 0,
       doc: /* Print a one-line message at the bottom of the screen.
The first argument is a format control string, and the rest are data
to be formatted under control of the string.  See `format' for details.

If the first argument is nil, clear any existing message; let the
minibuffer contents show.

usage: (message STRING &rest ARGS)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  if (NILP (args[0]))
    {
      message (0);
      return Qnil;
    }
  else
    {
      register Lisp_Object val;
      val = Fformat (nargs, args);
      message3 (val, STRING_BYTES (XSTRING (val)), STRING_MULTIBYTE (val));
      return val;
    }
}

DEFUN ("message-box", Fmessage_box, Smessage_box, 1, MANY, 0,
       doc: /* Display a message, in a dialog box if possible.
If a dialog box is not available, use the echo area.
The first argument is a format control string, and the rest are data
to be formatted under control of the string.  See `format' for details.

If the first argument is nil, clear any existing message; let the
minibuffer contents show.

usage: (message-box STRING &rest ARGS)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  if (NILP (args[0]))
    {
      message (0);
      return Qnil;
    }
  else
    {
      register Lisp_Object val;
      val = Fformat (nargs, args);
#ifdef HAVE_MENUS
      /* The MS-DOS frames support popup menus even though they are
	 not FRAME_WINDOW_P.  */
      if (FRAME_WINDOW_P (XFRAME (selected_frame))
	  || FRAME_MSDOS_P (XFRAME (selected_frame)))
      {
	Lisp_Object pane, menu, obj;
	struct gcpro gcpro1;
	pane = Fcons (Fcons (build_string ("OK"), Qt), Qnil);
	GCPRO1 (pane);
	menu = Fcons (val, pane);
	obj = Fx_popup_dialog (Qt, menu);
	UNGCPRO;
	return val;
      }
#endif /* HAVE_MENUS */
      /* Copy the data so that it won't move when we GC.  */
      if (! message_text)
	{
	  message_text = (char *)xmalloc (80);
	  message_length = 80;
	}
      if (STRING_BYTES (XSTRING (val)) > message_length)
	{
	  message_length = STRING_BYTES (XSTRING (val));
	  message_text = (char *)xrealloc (message_text, message_length);
	}
      bcopy (XSTRING (val)->data, message_text, STRING_BYTES (XSTRING (val)));
      message2 (message_text, STRING_BYTES (XSTRING (val)),
		STRING_MULTIBYTE (val));
      return val;
    }
}
#ifdef HAVE_MENUS
extern Lisp_Object last_nonmenu_event;
#endif

DEFUN ("message-or-box", Fmessage_or_box, Smessage_or_box, 1, MANY, 0,
       doc: /* Display a message in a dialog box or in the echo area.
If this command was invoked with the mouse, use a dialog box if
`use-dialog-box' is non-nil.
Otherwise, use the echo area.
The first argument is a format control string, and the rest are data
to be formatted under control of the string.  See `format' for details.

If the first argument is nil, clear any existing message; let the
minibuffer contents show.

usage: (message-or-box STRING &rest ARGS)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
#ifdef HAVE_MENUS
  if ((NILP (last_nonmenu_event) || CONSP (last_nonmenu_event))
      && use_dialog_box)
    return Fmessage_box (nargs, args);
#endif
  return Fmessage (nargs, args);
}

DEFUN ("current-message", Fcurrent_message, Scurrent_message, 0, 0, 0,
       doc: /* Return the string currently displayed in the echo area, or nil if none.  */)
     ()
{
  return current_message ();
}


DEFUN ("propertize", Fpropertize, Spropertize, 1, MANY, 0,
       doc: /* Return a copy of STRING with text properties added.
First argument is the string to copy.
Remaining arguments form a sequence of PROPERTY VALUE pairs for text
properties to add to the result.
usage: (propertize STRING &rest PROPERTIES)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  Lisp_Object properties, string;
  struct gcpro gcpro1, gcpro2;
  int i;

  /* Number of args must be odd.  */
  if ((nargs & 1) == 0 || nargs < 1)
    error ("Wrong number of arguments");

  properties = string = Qnil;
  GCPRO2 (properties, string);

  /* First argument must be a string.  */
  CHECK_STRING (args[0]);
  string = Fcopy_sequence (args[0]);

  for (i = 1; i < nargs; i += 2)
    {
      CHECK_SYMBOL (args[i]);
      properties = Fcons (args[i], Fcons (args[i + 1], properties));
    }

  Fadd_text_properties (make_number (0),
			make_number (XSTRING (string)->size),
			properties, string);
  RETURN_UNGCPRO (string);
}


/* Number of bytes that STRING will occupy when put into the result.
   MULTIBYTE is nonzero if the result should be multibyte.  */

#define CONVERTED_BYTE_SIZE(MULTIBYTE, STRING)				\
  (((MULTIBYTE) && ! STRING_MULTIBYTE (STRING))				\
   ? count_size_as_multibyte (XSTRING (STRING)->data,			\
			      STRING_BYTES (XSTRING (STRING)))		\
   : STRING_BYTES (XSTRING (STRING)))

DEFUN ("format", Fformat, Sformat, 1, MANY, 0,
       doc: /* Format a string out of a control-string and arguments.
The first argument is a control string.
The other arguments are substituted into it to make the result, a string.
It may contain %-sequences meaning to substitute the next argument.
%s means print a string argument.  Actually, prints any object, with `princ'.
%d means print as number in decimal (%o octal, %x hex).
%X is like %x, but uses upper case.
%e means print a number in exponential notation.
%f means print a number in decimal-point notation.
%g means print a number in exponential notation
  or decimal-point notation, whichever uses fewer characters.
%c means print a number as a single character.
%S means print any object as an s-expression (using `prin1').
  The argument used for %d, %o, %x, %e, %f, %g or %c must be a number.
Use %% to put a single % into the output.

usage: (format STRING &rest OBJECTS)  */)
     (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  register int n;		/* The number of the next arg to substitute */
  register int total;		/* An estimate of the final length */
  char *buf, *p;
  register unsigned char *format, *end;
  int nchars;
  /* Nonzero if the output should be a multibyte string,
     which is true if any of the inputs is one.  */
  int multibyte = 0;
  /* When we make a multibyte string, we must pay attention to the
     byte combining problem, i.e., a byte may be combined with a
     multibyte charcter of the previous string.  This flag tells if we
     must consider such a situation or not.  */
  int maybe_combine_byte;
  unsigned char *this_format;
  int longest_format;
  Lisp_Object val;
  struct info
  {
    int start, end;
  } *info = 0;

  /* It should not be necessary to GCPRO ARGS, because
     the caller in the interpreter should take care of that.  */

  /* Try to determine whether the result should be multibyte.
     This is not always right; sometimes the result needs to be multibyte
     because of an object that we will pass through prin1,
     and in that case, we won't know it here.  */
  for (n = 0; n < nargs; n++)
    if (STRINGP (args[n]) && STRING_MULTIBYTE (args[n]))
      multibyte = 1;

  CHECK_STRING (args[0]);

  /* If we start out planning a unibyte result,
     and later find it has to be multibyte, we jump back to retry.  */
 retry:

  format = XSTRING (args[0])->data;
  end = format + STRING_BYTES (XSTRING (args[0]));
  longest_format = 0;

  /* Make room in result for all the non-%-codes in the control string.  */
  total = 5 + CONVERTED_BYTE_SIZE (multibyte, args[0]);

  /* Add to TOTAL enough space to hold the converted arguments.  */

  n = 0;
  while (format != end)
    if (*format++ == '%')
      {
	int thissize = 0;
	int actual_width = 0;
	unsigned char *this_format_start = format - 1;
	int field_width, precision;

	/* General format specifications look like

	   '%' [flags] [field-width] [precision] format

	   where

	   flags	::= [#-* 0]+
	   field-width	::= [0-9]+
	   precision	::= '.' [0-9]*

	   If a field-width is specified, it specifies to which width
	   the output should be padded with blanks, iff the output
	   string is shorter than field-width.

	   if precision is specified, it specifies the number of
	   digits to print after the '.' for floats, or the max.
	   number of chars to print from a string.  */

	precision = field_width = 0;
	
	while (index ("-*# 0", *format))
	  ++format;

	if (*format >= '0' && *format <= '9')
	  {
	    for (field_width = 0; *format >= '0' && *format <= '9'; ++format)
	      field_width = 10 * field_width + *format - '0';
	  }

	if (*format == '.')
	  {
	    ++format;
	    for (precision = 0; *format >= '0' && *format <= '9'; ++format)
	      precision = 10 * precision + *format - '0';
	  }

	if (format - this_format_start + 1 > longest_format)
	  longest_format = format - this_format_start + 1;

	if (format == end)
	  error ("Format string ends in middle of format specifier");
	if (*format == '%')
	  format++;
	else if (++n >= nargs)
	  error ("Not enough arguments for format string");
	else if (*format == 'S')
	  {
	    /* For `S', prin1 the argument and then treat like a string.  */
	    register Lisp_Object tem;
	    tem = Fprin1_to_string (args[n], Qnil);
	    if (STRING_MULTIBYTE (tem) && ! multibyte)
	      {
		multibyte = 1;
		goto retry;
	      }
	    args[n] = tem;
	    goto string;
	  }
	else if (SYMBOLP (args[n]))
	  {
	    /* Use a temp var to avoid problems when ENABLE_CHECKING
	       is turned on.  */
	    struct Lisp_String *t = XSTRING (SYMBOL_NAME (args[n]));
	    XSETSTRING (args[n], t);
	    if (STRING_MULTIBYTE (args[n]) && ! multibyte)
	      {
		multibyte = 1;
		goto retry;
	      }
	    goto string;
	  }
	else if (STRINGP (args[n]))
	  {
	  string:
	    if (*format != 's' && *format != 'S')
	      error ("Format specifier doesn't match argument type");
	    thissize = CONVERTED_BYTE_SIZE (multibyte, args[n]);
	    actual_width = lisp_string_width (args[n], -1, NULL, NULL);
	  }
	/* Would get MPV otherwise, since Lisp_Int's `point' to low memory.  */
	else if (INTEGERP (args[n]) && *format != 's')
	  {
	    /* The following loop assumes the Lisp type indicates
	       the proper way to pass the argument.
	       So make sure we have a flonum if the argument should
	       be a double.  */
	    if (*format == 'e' || *format == 'f' || *format == 'g')
	      args[n] = Ffloat (args[n]);
	    else
	      if (*format != 'd' && *format != 'o' && *format != 'x'
		  && *format != 'i' && *format != 'X' && *format != 'c')
		error ("Invalid format operation %%%c", *format);

	    thissize = 30;
	    if (*format == 'c'
		&& (! SINGLE_BYTE_CHAR_P (XINT (args[n]))
		    || XINT (args[n]) == 0))
	      {
		if (! multibyte)
		  {
		    multibyte = 1;
		    goto retry;
		  }
		args[n] = Fchar_to_string (args[n]);
		thissize = STRING_BYTES (XSTRING (args[n]));
	      }
	  }
	else if (FLOATP (args[n]) && *format != 's')
	  {
	    if (! (*format == 'e' || *format == 'f' || *format == 'g'))
	      args[n] = Ftruncate (args[n], Qnil);

	    /* Note that we're using sprintf to print floats,
	       so we have to take into account what that function
	       prints.  */
	    thissize = MAX_10_EXP + 100 + precision;
	  }
	else
	  {
	    /* Anything but a string, convert to a string using princ.  */
	    register Lisp_Object tem;
	    tem = Fprin1_to_string (args[n], Qt);
	    if (STRING_MULTIBYTE (tem) & ! multibyte)
	      {
		multibyte = 1;
		goto retry;
	      }
	    args[n] = tem;
	    goto string;
	  }

	thissize += max (0, field_width - actual_width);
	total += thissize + 4;
      }

  /* Now we can no longer jump to retry.
     TOTAL and LONGEST_FORMAT are known for certain.  */

  this_format = (unsigned char *) alloca (longest_format + 1);

  /* Allocate the space for the result.
     Note that TOTAL is an overestimate.  */
  if (total < 1000)
    buf = (char *) alloca (total + 1);
  else
    buf = (char *) xmalloc (total + 1);

  p = buf;
  nchars = 0;
  n = 0;

  /* Scan the format and store result in BUF.  */
  format = XSTRING (args[0])->data;
  maybe_combine_byte = 0;
  while (format != end)
    {
      if (*format == '%')
	{
	  int minlen;
	  int negative = 0;
	  unsigned char *this_format_start = format;

	  format++;

	  /* Process a numeric arg and skip it.  */
	  minlen = atoi (format);
	  if (minlen < 0)
	    minlen = - minlen, negative = 1;

	  while ((*format >= '0' && *format <= '9')
		 || *format == '-' || *format == ' ' || *format == '.')
	    format++;

	  if (*format++ == '%')
	    {
	      *p++ = '%';
	      nchars++;
	      continue;
	    }

	  ++n;

	  if (STRINGP (args[n]))
	    {
	      int padding, nbytes, start, end;
	      int width = lisp_string_width (args[n], -1, NULL, NULL);

	      /* If spec requires it, pad on right with spaces.  */
	      padding = minlen - width;
	      if (! negative)
		while (padding-- > 0)
		  {
		    *p++ = ' ';
		    ++nchars;
		  }

	      start = nchars;
	      
	      if (p > buf
		  && multibyte
		  && !ASCII_BYTE_P (*((unsigned char *) p - 1))
		  && STRING_MULTIBYTE (args[n])
		  && !CHAR_HEAD_P (XSTRING (args[n])->data[0]))
		maybe_combine_byte = 1;
	      nbytes = copy_text (XSTRING (args[n])->data, p,
				  STRING_BYTES (XSTRING (args[n])),
				  STRING_MULTIBYTE (args[n]), multibyte);
	      p += nbytes;
	      nchars += XSTRING (args[n])->size;
	      end = nchars;

	      if (negative)
		while (padding-- > 0)
		  {
		    *p++ = ' ';
		    nchars++;
		  }

	      /* If this argument has text properties, record where
		 in the result string it appears.  */
	      if (XSTRING (args[n])->intervals)
		{
		  if (!info)
		    {
		      int nbytes = nargs * sizeof *info;
		      info = (struct info *) alloca (nbytes);
		      bzero (info, nbytes);
		    }

		  info[n].start = start;
		  info[n].end = end;
		}
	    }
	  else if (INTEGERP (args[n]) || FLOATP (args[n]))
	    {
	      int this_nchars;

	      bcopy (this_format_start, this_format,
		     format - this_format_start);
	      this_format[format - this_format_start] = 0;

	      if (INTEGERP (args[n]))
		sprintf (p, this_format, XINT (args[n]));
	      else
		sprintf (p, this_format, XFLOAT_DATA (args[n]));

	      if (p > buf
		  && multibyte
		  && !ASCII_BYTE_P (*((unsigned char *) p - 1))
		  && !CHAR_HEAD_P (*((unsigned char *) p)))
		maybe_combine_byte = 1;
	      this_nchars = strlen (p);
	      if (multibyte)
		p += str_to_multibyte (p, buf + total - p, this_nchars);
	      else
		p += this_nchars;
	      nchars += this_nchars;
	    }
	}
      else if (STRING_MULTIBYTE (args[0]))
	{
	  /* Copy a whole multibyte character.  */
	  if (p > buf
	      && multibyte
	      && !ASCII_BYTE_P (*((unsigned char *) p - 1))
	      && !CHAR_HEAD_P (*format))
	    maybe_combine_byte = 1;
	  *p++ = *format++;
	  while (! CHAR_HEAD_P (*format)) *p++ = *format++;
	  nchars++;
	}
      else if (multibyte)
	{
	  /* Convert a single-byte character to multibyte.  */
	  int len = copy_text (format, p, 1, 0, 1);

	  p += len;
	  format++;
	  nchars++;
	}
      else
	*p++ = *format++, nchars++;
    }

  if (p > buf + total + 1)
    abort ();

  if (maybe_combine_byte)
    nchars = multibyte_chars_in_text (buf, p - buf);
  val = make_specified_string (buf, nchars, p - buf, multibyte);

  /* If we allocated BUF with malloc, free it too.  */
  if (total >= 1000)
    xfree (buf);

  /* If the format string has text properties, or any of the string
     arguments has text properties, set up text properties of the
     result string.  */

  if (XSTRING (args[0])->intervals || info)
    {
      Lisp_Object len, new_len, props;
      struct gcpro gcpro1;

      /* Add text properties from the format string.  */
      len = make_number (XSTRING (args[0])->size);
      props = text_property_list (args[0], make_number (0), len, Qnil);
      GCPRO1 (props);

      if (CONSP (props))
	{
	  new_len = make_number (XSTRING (val)->size);
	  extend_property_ranges (props, len, new_len);
	  add_text_properties_from_list (val, props, make_number (0));
	}

      /* Add text properties from arguments.  */
      if (info)
	for (n = 1; n < nargs; ++n)
	  if (info[n].end)
	    {
	      len = make_number (XSTRING (args[n])->size);
	      new_len = make_number (info[n].end - info[n].start);
	      props = text_property_list (args[n], make_number (0), len, Qnil);
	      extend_property_ranges (props, len, new_len);
	      /* If successive arguments have properites, be sure that
		 the value of `composition' property be the copy.  */
	      if (n > 1 && info[n - 1].end)
		make_composition_value_copy (props);
	      add_text_properties_from_list (val, props,
					     make_number (info[n].start));
	    }

      UNGCPRO;
    }

  return val;
}


/* VARARGS 1 */
Lisp_Object
#ifdef NO_ARG_ARRAY
format1 (string1, arg0, arg1, arg2, arg3, arg4)
     EMACS_INT arg0, arg1, arg2, arg3, arg4;
#else
format1 (string1)
#endif
     char *string1;
{
  char buf[100];
#ifdef NO_ARG_ARRAY
  EMACS_INT args[5];
  args[0] = arg0;
  args[1] = arg1;
  args[2] = arg2;
  args[3] = arg3;
  args[4] = arg4;
  doprnt (buf, sizeof buf, string1, (char *)0, 5, (char **) args);
#else
  doprnt (buf, sizeof buf, string1, (char *)0, 5, &string1 + 1);
#endif
  return build_string (buf);
}

DEFUN ("char-equal", Fchar_equal, Schar_equal, 2, 2, 0,
       doc: /* Return t if two characters match, optionally ignoring case.
Both arguments must be characters (i.e. integers).
Case is ignored if `case-fold-search' is non-nil in the current buffer.  */)
     (c1, c2)
     register Lisp_Object c1, c2;
{
  int i1, i2;
  CHECK_NUMBER (c1);
  CHECK_NUMBER (c2);

  if (XINT (c1) == XINT (c2))
    return Qt;
  if (NILP (current_buffer->case_fold_search))
    return Qnil;

  /* Do these in separate statements,
     then compare the variables.
     because of the way DOWNCASE uses temp variables.  */
  i1 = DOWNCASE (XFASTINT (c1));
  i2 = DOWNCASE (XFASTINT (c2));
  return (i1 == i2 ? Qt :  Qnil);
}

/* Transpose the markers in two regions of the current buffer, and
   adjust the ones between them if necessary (i.e.: if the regions
   differ in size).

   START1, END1 are the character positions of the first region.
   START1_BYTE, END1_BYTE are the byte positions.
   START2, END2 are the character positions of the second region.
   START2_BYTE, END2_BYTE are the byte positions.

   Traverses the entire marker list of the buffer to do so, adding an
   appropriate amount to some, subtracting from some, and leaving the
   rest untouched.  Most of this is copied from adjust_markers in insdel.c.

   It's the caller's job to ensure that START1 <= END1 <= START2 <= END2.  */

static void
transpose_markers (start1, end1, start2, end2,
		   start1_byte, end1_byte, start2_byte, end2_byte)
     register int start1, end1, start2, end2;
     register int start1_byte, end1_byte, start2_byte, end2_byte;
{
  register int amt1, amt1_byte, amt2, amt2_byte, diff, diff_byte, mpos;
  register Lisp_Object marker;

  /* Update point as if it were a marker.  */
  if (PT < start1)
    ;
  else if (PT < end1)
    TEMP_SET_PT_BOTH (PT + (end2 - end1),
		      PT_BYTE + (end2_byte - end1_byte));
  else if (PT < start2)
    TEMP_SET_PT_BOTH (PT + (end2 - start2) - (end1 - start1),
		      (PT_BYTE + (end2_byte - start2_byte)
		       - (end1_byte - start1_byte)));
  else if (PT < end2)
    TEMP_SET_PT_BOTH (PT - (start2 - start1),
		      PT_BYTE - (start2_byte - start1_byte));

  /* We used to adjust the endpoints here to account for the gap, but that
     isn't good enough.  Even if we assume the caller has tried to move the
     gap out of our way, it might still be at start1 exactly, for example;
     and that places it `inside' the interval, for our purposes.  The amount
     of adjustment is nontrivial if there's a `denormalized' marker whose
     position is between GPT and GPT + GAP_SIZE, so it's simpler to leave
     the dirty work to Fmarker_position, below.  */

  /* The difference between the region's lengths */
  diff = (end2 - start2) - (end1 - start1);
  diff_byte = (end2_byte - start2_byte) - (end1_byte - start1_byte);

  /* For shifting each marker in a region by the length of the other
     region plus the distance between the regions.  */
  amt1 = (end2 - start2) + (start2 - end1);
  amt2 = (end1 - start1) + (start2 - end1);
  amt1_byte = (end2_byte - start2_byte) + (start2_byte - end1_byte);
  amt2_byte = (end1_byte - start1_byte) + (start2_byte - end1_byte);

  for (marker = BUF_MARKERS (current_buffer); !NILP (marker);
       marker = XMARKER (marker)->chain)
    {
      mpos = marker_byte_position (marker);
      if (mpos >= start1_byte && mpos < end2_byte)
	{
	  if (mpos < end1_byte)
	    mpos += amt1_byte;
	  else if (mpos < start2_byte)
	    mpos += diff_byte;
	  else
	    mpos -= amt2_byte;
	  XMARKER (marker)->bytepos = mpos;
	}
      mpos = XMARKER (marker)->charpos;
      if (mpos >= start1 && mpos < end2)
	{
	  if (mpos < end1)
	    mpos += amt1;
	  else if (mpos < start2)
	    mpos += diff;
	  else
	    mpos -= amt2;
	}
      XMARKER (marker)->charpos = mpos;
    }
}

DEFUN ("transpose-regions", Ftranspose_regions, Stranspose_regions, 4, 5, 0,
       doc: /* Transpose region START1 to END1 with START2 to END2.
The regions may not be overlapping, because the size of the buffer is
never changed in a transposition.

Optional fifth arg LEAVE_MARKERS, if non-nil, means don't update
any markers that happen to be located in the regions.

Transposing beyond buffer boundaries is an error.  */)
     (startr1, endr1, startr2, endr2, leave_markers)
     Lisp_Object startr1, endr1, startr2, endr2, leave_markers;
{
  register int start1, end1, start2, end2;
  int start1_byte, start2_byte, len1_byte, len2_byte;
  int gap, len1, len_mid, len2;
  unsigned char *start1_addr, *start2_addr, *temp;

  INTERVAL cur_intv, tmp_interval1, tmp_interval_mid, tmp_interval2;
  cur_intv = BUF_INTERVALS (current_buffer);

  validate_region (&startr1, &endr1);
  validate_region (&startr2, &endr2);

  start1 = XFASTINT (startr1);
  end1 = XFASTINT (endr1);
  start2 = XFASTINT (startr2);
  end2 = XFASTINT (endr2);
  gap = GPT;

  /* Swap the regions if they're reversed.  */
  if (start2 < end1)
    {
      register int glumph = start1;
      start1 = start2;
      start2 = glumph;
      glumph = end1;
      end1 = end2;
      end2 = glumph;
    }

  len1 = end1 - start1;
  len2 = end2 - start2;

  if (start2 < end1)
    error ("Transposed regions overlap");
  else if (start1 == end1 || start2 == end2)
    error ("Transposed region has length 0");

  /* The possibilities are:
     1. Adjacent (contiguous) regions, or separate but equal regions
     (no, really equal, in this case!), or
     2. Separate regions of unequal size.

     The worst case is usually No. 2.  It means that (aside from
     potential need for getting the gap out of the way), there also
     needs to be a shifting of the text between the two regions.  So
     if they are spread far apart, we are that much slower... sigh.  */

  /* It must be pointed out that the really studly thing to do would
     be not to move the gap at all, but to leave it in place and work
     around it if necessary.  This would be extremely efficient,
     especially considering that people are likely to do
     transpositions near where they are working interactively, which
     is exactly where the gap would be found.  However, such code
     would be much harder to write and to read.  So, if you are
     reading this comment and are feeling squirrely, by all means have
     a go!  I just didn't feel like doing it, so I will simply move
     the gap the minimum distance to get it out of the way, and then
     deal with an unbroken array.  */

  /* Make sure the gap won't interfere, by moving it out of the text
     we will operate on.  */
  if (start1 < gap && gap < end2)
    {
      if (gap - start1 < end2 - gap)
	move_gap (start1);
      else
	move_gap (end2);
    }

  start1_byte = CHAR_TO_BYTE (start1);
  start2_byte = CHAR_TO_BYTE (start2);
  len1_byte = CHAR_TO_BYTE (end1) - start1_byte;
  len2_byte = CHAR_TO_BYTE (end2) - start2_byte;

#ifdef BYTE_COMBINING_DEBUG
  if (end1 == start2)
    {
      if (count_combining_before (BYTE_POS_ADDR (start2_byte),
				  len2_byte, start1, start1_byte)
	  || count_combining_before (BYTE_POS_ADDR (start1_byte),
				     len1_byte, end2, start2_byte + len2_byte)
	  || count_combining_after (BYTE_POS_ADDR (start1_byte),
				    len1_byte, end2, start2_byte + len2_byte))
	abort ();
    }
  else
    {
      if (count_combining_before (BYTE_POS_ADDR (start2_byte),
				  len2_byte, start1, start1_byte)
	  || count_combining_before (BYTE_POS_ADDR (start1_byte),
				     len1_byte, start2, start2_byte)
	  || count_combining_after (BYTE_POS_ADDR (start2_byte),
				    len2_byte, end1, start1_byte + len1_byte)
	  || count_combining_after (BYTE_POS_ADDR (start1_byte),
				    len1_byte, end2, start2_byte + len2_byte))
	abort ();
    }
#endif

  /* Hmmm... how about checking to see if the gap is large
     enough to use as the temporary storage?  That would avoid an
     allocation... interesting.  Later, don't fool with it now.  */

  /* Working without memmove, for portability (sigh), so must be
     careful of overlapping subsections of the array...  */

  if (end1 == start2)		/* adjacent regions */
    {
      modify_region (current_buffer, start1, end2);
      record_change (start1, len1 + len2);

      tmp_interval1 = copy_intervals (cur_intv, start1, len1);
      tmp_interval2 = copy_intervals (cur_intv, start2, len2);
      Fset_text_properties (make_number (start1), make_number (end2),
			    Qnil, Qnil);

      /* First region smaller than second.  */
      if (len1_byte < len2_byte)
        {
	  /* We use alloca only if it is small,
	     because we want to avoid stack overflow.  */
	  if (len2_byte > 20000)
	    temp = (unsigned char *) xmalloc (len2_byte);
	  else
	    temp = (unsigned char *) alloca (len2_byte);

	  /* Don't precompute these addresses.  We have to compute them
	     at the last minute, because the relocating allocator might
	     have moved the buffer around during the xmalloc.  */
	  start1_addr = BYTE_POS_ADDR (start1_byte);
	  start2_addr = BYTE_POS_ADDR (start2_byte);

          bcopy (start2_addr, temp, len2_byte);
          bcopy (start1_addr, start1_addr + len2_byte, len1_byte);
          bcopy (temp, start1_addr, len2_byte);
	  if (len2_byte > 20000)
	    xfree (temp);
        }
      else
	/* First region not smaller than second.  */
        {
	  if (len1_byte > 20000)
	    temp = (unsigned char *) xmalloc (len1_byte);
	  else
	    temp = (unsigned char *) alloca (len1_byte);
	  start1_addr = BYTE_POS_ADDR (start1_byte);
	  start2_addr = BYTE_POS_ADDR (start2_byte);
          bcopy (start1_addr, temp, len1_byte);
          bcopy (start2_addr, start1_addr, len2_byte);
          bcopy (temp, start1_addr + len2_byte, len1_byte);
	  if (len1_byte > 20000)
	    xfree (temp);
        }
      graft_intervals_into_buffer (tmp_interval1, start1 + len2,
                                   len1, current_buffer, 0);
      graft_intervals_into_buffer (tmp_interval2, start1,
                                   len2, current_buffer, 0);
      update_compositions (start1, start1 + len2, CHECK_BORDER);
      update_compositions (start1 + len2, end2, CHECK_TAIL);
    }
  /* Non-adjacent regions, because end1 != start2, bleagh...  */
  else
    {
      len_mid = start2_byte - (start1_byte + len1_byte);

      if (len1_byte == len2_byte)
	/* Regions are same size, though, how nice.  */
        {
          modify_region (current_buffer, start1, end1);
          modify_region (current_buffer, start2, end2);
          record_change (start1, len1);
          record_change (start2, len2);
          tmp_interval1 = copy_intervals (cur_intv, start1, len1);
          tmp_interval2 = copy_intervals (cur_intv, start2, len2);
          Fset_text_properties (make_number (start1), make_number (end1),
				Qnil, Qnil);
          Fset_text_properties (make_number (start2), make_number (end2),
				Qnil, Qnil);

	  if (len1_byte > 20000)
	    temp = (unsigned char *) xmalloc (len1_byte);
	  else
	    temp = (unsigned char *) alloca (len1_byte);
	  start1_addr = BYTE_POS_ADDR (start1_byte);
	  start2_addr = BYTE_POS_ADDR (start2_byte);
          bcopy (start1_addr, temp, len1_byte);
          bcopy (start2_addr, start1_addr, len2_byte);
          bcopy (temp, start2_addr, len1_byte);
	  if (len1_byte > 20000)
	    xfree (temp);
          graft_intervals_into_buffer (tmp_interval1, start2,
                                       len1, current_buffer, 0);
          graft_intervals_into_buffer (tmp_interval2, start1,
                                       len2, current_buffer, 0);
        }

      else if (len1_byte < len2_byte)	/* Second region larger than first */
        /* Non-adjacent & unequal size, area between must also be shifted.  */
        {
          modify_region (current_buffer, start1, end2);
          record_change (start1, (end2 - start1));
          tmp_interval1 = copy_intervals (cur_intv, start1, len1);
          tmp_interval_mid = copy_intervals (cur_intv, end1, len_mid);
          tmp_interval2 = copy_intervals (cur_intv, start2, len2);
          Fset_text_properties (make_number (start1), make_number (end2),
				Qnil, Qnil);

	  /* holds region 2 */
	  if (len2_byte > 20000)
	    temp = (unsigned char *) xmalloc (len2_byte);
	  else
	    temp = (unsigned char *) alloca (len2_byte);
	  start1_addr = BYTE_POS_ADDR (start1_byte);
	  start2_addr = BYTE_POS_ADDR (start2_byte);
          bcopy (start2_addr, temp, len2_byte);
          bcopy (start1_addr, start1_addr + len_mid + len2_byte, len1_byte);
          safe_bcopy (start1_addr + len1_byte, start1_addr + len2_byte, len_mid);
          bcopy (temp, start1_addr, len2_byte);
	  if (len2_byte > 20000)
	    xfree (temp);
          graft_intervals_into_buffer (tmp_interval1, end2 - len1,
                                       len1, current_buffer, 0);
          graft_intervals_into_buffer (tmp_interval_mid, start1 + len2,
                                       len_mid, current_buffer, 0);
          graft_intervals_into_buffer (tmp_interval2, start1,
                                       len2, current_buffer, 0);
        }
      else
	/* Second region smaller than first.  */
        {
          record_change (start1, (end2 - start1));
          modify_region (current_buffer, start1, end2);

          tmp_interval1 = copy_intervals (cur_intv, start1, len1);
          tmp_interval_mid = copy_intervals (cur_intv, end1, len_mid);
          tmp_interval2 = copy_intervals (cur_intv, start2, len2);
          Fset_text_properties (make_number (start1), make_number (end2),
				Qnil, Qnil);

	  /* holds region 1 */
	  if (len1_byte > 20000)
	    temp = (unsigned char *) xmalloc (len1_byte);
	  else
	    temp = (unsigned char *) alloca (len1_byte);
	  start1_addr = BYTE_POS_ADDR (start1_byte);
	  start2_addr = BYTE_POS_ADDR (start2_byte);
          bcopy (start1_addr, temp, len1_byte);
          bcopy (start2_addr, start1_addr, len2_byte);
          bcopy (start1_addr + len1_byte, start1_addr + len2_byte, len_mid);
          bcopy (temp, start1_addr + len2_byte + len_mid, len1_byte);
	  if (len1_byte > 20000)
	    xfree (temp);
          graft_intervals_into_buffer (tmp_interval1, end2 - len1,
                                       len1, current_buffer, 0);
          graft_intervals_into_buffer (tmp_interval_mid, start1 + len2,
                                       len_mid, current_buffer, 0);
          graft_intervals_into_buffer (tmp_interval2, start1,
                                       len2, current_buffer, 0);
        }

      update_compositions (start1, start1 + len2, CHECK_BORDER);
      update_compositions (end2 - len1, end2, CHECK_BORDER);
    }

  /* When doing multiple transpositions, it might be nice
     to optimize this.  Perhaps the markers in any one buffer
     should be organized in some sorted data tree.  */
  if (NILP (leave_markers))
    {
      transpose_markers (start1, end1, start2, end2,
			 start1_byte, start1_byte + len1_byte,
			 start2_byte, start2_byte + len2_byte);
      fix_overlays_in_range (start1, end2);
    }

  return Qnil;
}


void
syms_of_editfns ()
{
  environbuf = 0;

  Qbuffer_access_fontify_functions
    = intern ("buffer-access-fontify-functions");
  staticpro (&Qbuffer_access_fontify_functions);

  DEFVAR_LISP ("inhibit-field-text-motion", &Vinhibit_field_text_motion,
	       doc: /* Non-nil means text motion commands don't notice fields.  */);
  Vinhibit_field_text_motion = Qnil;

  DEFVAR_LISP ("buffer-access-fontify-functions",
	       &Vbuffer_access_fontify_functions,
	       doc: /* List of functions called by `buffer-substring' to fontify if necessary.
Each function is called with two arguments which specify the range
of the buffer being accessed.  */);
  Vbuffer_access_fontify_functions = Qnil;

  {
    Lisp_Object obuf;
    extern Lisp_Object Vprin1_to_string_buffer;
    obuf = Fcurrent_buffer ();
    /* Do this here, because init_buffer_once is too early--it won't work.  */
    Fset_buffer (Vprin1_to_string_buffer);
    /* Make sure buffer-access-fontify-functions is nil in this buffer.  */
    Fset (Fmake_local_variable (intern ("buffer-access-fontify-functions")),
	  Qnil);
    Fset_buffer (obuf);
  }

  DEFVAR_LISP ("buffer-access-fontified-property",
	       &Vbuffer_access_fontified_property,
	       doc: /* Property which (if non-nil) indicates text has been fontified.
`buffer-substring' need not call the `buffer-access-fontify-functions'
functions if all the text being accessed has this property.  */);
  Vbuffer_access_fontified_property = Qnil;

  DEFVAR_LISP ("system-name", &Vsystem_name,
	       doc: /* The name of the machine Emacs is running on.  */);

  DEFVAR_LISP ("user-full-name", &Vuser_full_name,
	       doc: /* The full name of the user logged in.  */);

  DEFVAR_LISP ("user-login-name", &Vuser_login_name,
	       doc: /* The user's name, taken from environment variables if possible.  */);

  DEFVAR_LISP ("user-real-login-name", &Vuser_real_login_name,
	       doc: /* The user's name, based upon the real uid only.  */);

  defsubr (&Spropertize);
  defsubr (&Schar_equal);
  defsubr (&Sgoto_char);
  defsubr (&Sstring_to_char);
  defsubr (&Schar_to_string);
  defsubr (&Sbuffer_substring);
  defsubr (&Sbuffer_substring_no_properties);
  defsubr (&Sbuffer_string);

  defsubr (&Spoint_marker);
  defsubr (&Smark_marker);
  defsubr (&Spoint);
  defsubr (&Sregion_beginning);
  defsubr (&Sregion_end);

  staticpro (&Qfield);
  Qfield = intern ("field");
  staticpro (&Qboundary);
  Qboundary = intern ("boundary");
  defsubr (&Sfield_beginning);
  defsubr (&Sfield_end);
  defsubr (&Sfield_string);
  defsubr (&Sfield_string_no_properties);
  defsubr (&Sdelete_field);
  defsubr (&Sconstrain_to_field);

  defsubr (&Sline_beginning_position);
  defsubr (&Sline_end_position);

/*  defsubr (&Smark); */
/*  defsubr (&Sset_mark); */
  defsubr (&Ssave_excursion);
  defsubr (&Ssave_current_buffer);

  defsubr (&Sbufsize);
  defsubr (&Spoint_max);
  defsubr (&Spoint_min);
  defsubr (&Spoint_min_marker);
  defsubr (&Spoint_max_marker);
  defsubr (&Sgap_position);
  defsubr (&Sgap_size);
  defsubr (&Sposition_bytes);
  defsubr (&Sbyte_to_position);

  defsubr (&Sbobp);
  defsubr (&Seobp);
  defsubr (&Sbolp);
  defsubr (&Seolp);
  defsubr (&Sfollowing_char);
  defsubr (&Sprevious_char);
  defsubr (&Schar_after);
  defsubr (&Schar_before);
  defsubr (&Sinsert);
  defsubr (&Sinsert_before_markers);
  defsubr (&Sinsert_and_inherit);
  defsubr (&Sinsert_and_inherit_before_markers);
  defsubr (&Sinsert_char);

  defsubr (&Suser_login_name);
  defsubr (&Suser_real_login_name);
  defsubr (&Suser_uid);
  defsubr (&Suser_real_uid);
  defsubr (&Suser_full_name);
  defsubr (&Semacs_pid);
  defsubr (&Scurrent_time);
  defsubr (&Sformat_time_string);
  defsubr (&Sfloat_time);
  defsubr (&Sdecode_time);
  defsubr (&Sencode_time);
  defsubr (&Scurrent_time_string);
  defsubr (&Scurrent_time_zone);
  defsubr (&Sset_time_zone_rule);
  defsubr (&Ssystem_name);
  defsubr (&Smessage);
  defsubr (&Smessage_box);
  defsubr (&Smessage_or_box);
  defsubr (&Scurrent_message);
  defsubr (&Sformat);

  defsubr (&Sinsert_buffer_substring);
  defsubr (&Scompare_buffer_substrings);
  defsubr (&Ssubst_char_in_region);
  defsubr (&Stranslate_region);
  defsubr (&Sdelete_region);
  defsubr (&Sdelete_and_extract_region);
  defsubr (&Swiden);
  defsubr (&Snarrow_to_region);
  defsubr (&Ssave_restriction);
  defsubr (&Stranspose_regions);
}
