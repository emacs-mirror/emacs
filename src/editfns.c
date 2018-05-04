/* Lisp functions pertaining to editing.                 -*- coding: utf-8 -*-

Copyright (C) 1985-1987, 1989, 1993-2018 Free Software Foundation, Inc.

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
#include <sys/types.h>
#include <stdio.h>

#ifdef HAVE_PWD_H
#include <pwd.h>
#include <grp.h>
#endif

#include <unistd.h>

#ifdef HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
#endif

#include "lisp.h"

/* systime.h includes <sys/time.h> which, on some systems, is required
   for <sys/resource.h>; thus systime.h must be included before
   <sys/resource.h> */
#include "systime.h"

#if defined HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#include <errno.h>
#include <float.h>
#include <limits.h>

#ifdef HAVE_TIMEZONE_T
# include <sys/param.h>
# if defined __NetBSD_Version__ && __NetBSD_Version__ < 700000000
#  define HAVE_TZALLOC_BUG true
# endif
#endif
#ifndef HAVE_TZALLOC_BUG
# define HAVE_TZALLOC_BUG false
#endif

#include <c-ctype.h>
#include <intprops.h>
#include <stdlib.h>
#include <strftime.h>
#include <verify.h>

#include "composite.h"
#include "intervals.h"
#include "ptr-bounds.h"
#include "character.h"
#include "buffer.h"
#include "coding.h"
#include "window.h"
#include "blockinput.h"

#define TM_YEAR_BASE 1900

#ifdef WINDOWSNT
extern Lisp_Object w32_get_internal_run_time (void);
#endif

static struct lisp_time lisp_time_struct (Lisp_Object, int *);
static Lisp_Object format_time_string (char const *, ptrdiff_t, struct timespec,
				       Lisp_Object, struct tm *);
static long int tm_gmtoff (struct tm *);
static int tm_diff (struct tm *, struct tm *);
static void update_buffer_properties (ptrdiff_t, ptrdiff_t);
static Lisp_Object styled_format (ptrdiff_t, Lisp_Object *, bool);

#ifndef HAVE_TM_GMTOFF
# define HAVE_TM_GMTOFF false
#endif

enum { tzeqlen = sizeof "TZ=" - 1 };

/* Time zones equivalent to current local time and to UTC, respectively.  */
static timezone_t local_tz;
static timezone_t const utc_tz = 0;

/* The cached value of Vsystem_name.  This is used only to compare it
   to Vsystem_name, so it need not be visible to the GC.  */
static Lisp_Object cached_system_name;

static void
init_and_cache_system_name (void)
{
  init_system_name ();
  cached_system_name = Vsystem_name;
}

static struct tm *
emacs_localtime_rz (timezone_t tz, time_t const *t, struct tm *tm)
{
  tm = localtime_rz (tz, t, tm);
  if (!tm && errno == ENOMEM)
    memory_full (SIZE_MAX);
  return tm;
}

static time_t
emacs_mktime_z (timezone_t tz, struct tm *tm)
{
  errno = 0;
  time_t t = mktime_z (tz, tm);
  if (t == (time_t) -1 && errno == ENOMEM)
    memory_full (SIZE_MAX);
  return t;
}

static _Noreturn void
invalid_time_zone_specification (Lisp_Object zone)
{
  xsignal2 (Qerror, build_string ("Invalid time zone specification"), zone);
}

/* Free a timezone, except do not free the time zone for local time.
   Freeing utc_tz is also a no-op.  */
static void
xtzfree (timezone_t tz)
{
  if (tz != local_tz)
    tzfree (tz);
}

/* Convert the Lisp time zone rule ZONE to a timezone_t object.
   The returned value either is 0, or is LOCAL_TZ, or is newly allocated.
   If SETTZ, set Emacs local time to the time zone rule; otherwise,
   the caller should eventually pass the returned value to xtzfree.  */
static timezone_t
tzlookup (Lisp_Object zone, bool settz)
{
  static char const tzbuf_format[] = "<%+.*"pI"d>%s%"pI"d:%02d:%02d";
  char const *trailing_tzbuf_format = tzbuf_format + sizeof "<%+.*"pI"d" - 1;
  char tzbuf[sizeof tzbuf_format + 2 * INT_STRLEN_BOUND (EMACS_INT)];
  char const *zone_string;
  timezone_t new_tz;

  if (NILP (zone))
    return local_tz;
  else if (EQ (zone, Qt) || EQ (zone, make_number (0)))
    {
      zone_string = "UTC0";
      new_tz = utc_tz;
    }
  else
    {
      bool plain_integer = INTEGERP (zone);

      if (EQ (zone, Qwall))
	zone_string = 0;
      else if (STRINGP (zone))
	zone_string = SSDATA (ENCODE_SYSTEM (zone));
      else if (plain_integer || (CONSP (zone) && INTEGERP (XCAR (zone))
				 && CONSP (XCDR (zone))))
	{
	  Lisp_Object abbr;
	  if (!plain_integer)
	    {
	      abbr = XCAR (XCDR (zone));
	      zone = XCAR (zone);
	    }

	  EMACS_INT abszone = eabs (XINT (zone)), hour = abszone / (60 * 60);
	  int hour_remainder = abszone % (60 * 60);
	  int min = hour_remainder / 60, sec = hour_remainder % 60;

	  if (plain_integer)
	    {
	      int prec = 2;
	      EMACS_INT numzone = hour;
	      if (hour_remainder != 0)
		{
		  prec += 2, numzone = 100 * numzone + min;
		  if (sec != 0)
		    prec += 2, numzone = 100 * numzone + sec;
		}
	      sprintf (tzbuf, tzbuf_format, prec,
		       XINT (zone) < 0 ? -numzone : numzone,
		       &"-"[XINT (zone) < 0], hour, min, sec);
	      zone_string = tzbuf;
	    }
	  else
	    {
	      AUTO_STRING (leading, "<");
	      AUTO_STRING_WITH_LEN (trailing, tzbuf,
				    sprintf (tzbuf, trailing_tzbuf_format,
					     &"-"[XINT (zone) < 0],
					     hour, min, sec));
	      zone_string = SSDATA (concat3 (leading, ENCODE_SYSTEM (abbr),
					     trailing));
	    }
	}
      else
	invalid_time_zone_specification (zone);

      new_tz = tzalloc (zone_string);

      if (HAVE_TZALLOC_BUG && !new_tz && errno != ENOMEM && plain_integer
	  && XINT (zone) % (60 * 60) == 0)
	{
	  /* tzalloc mishandles POSIX strings; fall back on tzdb if
	     possible (Bug#30738).  */
	  sprintf (tzbuf, "Etc/GMT%+"pI"d", - (XINT (zone) / (60 * 60)));
	  new_tz = tzalloc (zone_string);
	}

      if (!new_tz)
	{
	  if (errno == ENOMEM)
	    memory_full (SIZE_MAX);
	  invalid_time_zone_specification (zone);
	}
    }

  if (settz)
    {
      block_input ();
      emacs_setenv_TZ (zone_string);
      tzset ();
      timezone_t old_tz = local_tz;
      local_tz = new_tz;
      tzfree (old_tz);
      unblock_input ();
    }

  return new_tz;
}

void
init_editfns (bool dumping)
{
#if !defined CANNOT_DUMP
  /* A valid but unlikely setting for the TZ environment variable.
     It is OK (though a bit slower) if the user chooses this value.  */
  static char dump_tz_string[] = "TZ=UtC0";
#endif

  const char *user_name;
  register char *p;
  struct passwd *pw;	/* password entry for the current user */
  Lisp_Object tem;

  /* Set up system_name even when dumping.  */
  init_and_cache_system_name ();

#ifndef CANNOT_DUMP
  /* When just dumping out, set the time zone to a known unlikely value
     and skip the rest of this function.  */
  if (dumping)
    {
      xputenv (dump_tz_string);
      tzset ();
      return;
    }
#endif

  char *tz = getenv ("TZ");

#if !defined CANNOT_DUMP
  /* If the execution TZ happens to be the same as the dump TZ,
     change it to some other value and then change it back,
     to force the underlying implementation to reload the TZ info.
     This is needed on implementations that load TZ info from files,
     since the TZ file contents may differ between dump and execution.  */
  if (tz && strcmp (tz, &dump_tz_string[tzeqlen]) == 0)
    {
      ++*tz;
      tzset ();
      --*tz;
    }
#endif

  /* Set the time zone rule now, so that the call to putenv is done
     before multiple threads are active.  */
  tzlookup (tz ? build_string (tz) : Qwall, true);

  pw = getpwuid (getuid ());
#ifdef MSDOS
  /* We let the real user name default to "root" because that's quite
     accurate on MS-DOS and because it lets Emacs find the init file.
     (The DVX libraries override the Djgpp libraries here.)  */
  Vuser_real_login_name = build_string (pw ? pw->pw_name : "root");
#else
  Vuser_real_login_name = build_string (pw ? pw->pw_name : "unknown");
#endif

  /* Get the effective user name, by consulting environment variables,
     or the effective uid if those are unset.  */
  user_name = getenv ("LOGNAME");
  if (!user_name)
#ifdef WINDOWSNT
    user_name = getenv ("USERNAME");	/* it's USERNAME on NT */
#else  /* WINDOWSNT */
    user_name = getenv ("USER");
#endif /* WINDOWSNT */
  if (!user_name)
    {
      pw = getpwuid (geteuid ());
      user_name = pw ? pw->pw_name : "unknown";
    }
  Vuser_login_name = build_string (user_name);

  /* If the user name claimed in the environment vars differs from
     the real uid, use the claimed name to find the full name.  */
  tem = Fstring_equal (Vuser_login_name, Vuser_real_login_name);
  if (! NILP (tem))
    tem = Vuser_login_name;
  else
    {
      uid_t euid = geteuid ();
      tem = make_fixnum_or_float (euid);
    }
  Vuser_full_name = Fuser_full_name (tem);

  p = getenv ("NAME");
  if (p)
    Vuser_full_name = build_string (p);
  else if (NILP (Vuser_full_name))
    Vuser_full_name = build_string ("unknown");

#ifdef HAVE_SYS_UTSNAME_H
  {
    struct utsname uts;
    uname (&uts);
    Voperating_system_release = build_string (uts.release);
  }
#else
  Voperating_system_release = Qnil;
#endif
}

DEFUN ("char-to-string", Fchar_to_string, Schar_to_string, 1, 1, 0,
       doc: /* Convert arg CHAR to a string containing that character.
usage: (char-to-string CHAR)  */)
  (Lisp_Object character)
{
  int c, len;
  unsigned char str[MAX_MULTIBYTE_LENGTH];

  CHECK_CHARACTER (character);
  c = XFASTINT (character);

  len = CHAR_STRING (c, str);
  return make_string_from_bytes ((char *) str, 1, len);
}

DEFUN ("byte-to-string", Fbyte_to_string, Sbyte_to_string, 1, 1, 0,
       doc: /* Convert arg BYTE to a unibyte string containing that byte.  */)
  (Lisp_Object byte)
{
  unsigned char b;
  CHECK_NUMBER (byte);
  if (XINT (byte) < 0 || XINT (byte) > 255)
    error ("Invalid byte");
  b = XINT (byte);
  return make_string_from_bytes ((char *) &b, 1, 1);
}

DEFUN ("string-to-char", Fstring_to_char, Sstring_to_char, 1, 1, 0,
       doc: /* Return the first character in STRING.  */)
  (register Lisp_Object string)
{
  register Lisp_Object val;
  CHECK_STRING (string);
  if (SCHARS (string))
    {
      if (STRING_MULTIBYTE (string))
	XSETFASTINT (val, STRING_CHAR (SDATA (string)));
      else
	XSETFASTINT (val, SREF (string, 0));
    }
  else
    XSETFASTINT (val, 0);
  return val;
}

DEFUN ("point", Fpoint, Spoint, 0, 0, 0,
       doc: /* Return value of point, as an integer.
Beginning of buffer is position (point-min).  */)
  (void)
{
  Lisp_Object temp;
  XSETFASTINT (temp, PT);
  return temp;
}

DEFUN ("point-marker", Fpoint_marker, Spoint_marker, 0, 0, 0,
       doc: /* Return value of point, as a marker object.  */)
  (void)
{
  return build_marker (current_buffer, PT, PT_BYTE);
}

DEFUN ("goto-char", Fgoto_char, Sgoto_char, 1, 1, "NGoto char: ",
       doc: /* Set point to POSITION, a number or marker.
Beginning of buffer is position (point-min), end is (point-max).

The return value is POSITION.  */)
  (register Lisp_Object position)
{
  if (MARKERP (position))
    set_point_from_marker (position);
  else if (INTEGERP (position))
    SET_PT (clip_to_bounds (BEGV, XINT (position), ZV));
  else
    wrong_type_argument (Qinteger_or_marker_p, position);
  return position;
}


/* Return the start or end position of the region.
   BEGINNINGP means return the start.
   If there is no region active, signal an error. */

static Lisp_Object
region_limit (bool beginningp)
{
  Lisp_Object m;

  if (!NILP (Vtransient_mark_mode)
      && NILP (Vmark_even_if_inactive)
      && NILP (BVAR (current_buffer, mark_active)))
    xsignal0 (Qmark_inactive);

  m = Fmarker_position (BVAR (current_buffer, mark));
  if (NILP (m))
    error ("The mark is not set now, so there is no region");

  /* Clip to the current narrowing (bug#11770).  */
  return make_number ((PT < XFASTINT (m)) == beginningp
		      ? PT
		      : clip_to_bounds (BEGV, XFASTINT (m), ZV));
}

DEFUN ("region-beginning", Fregion_beginning, Sregion_beginning, 0, 0, 0,
       doc: /* Return the integer value of point or mark, whichever is smaller.  */)
  (void)
{
  return region_limit (1);
}

DEFUN ("region-end", Fregion_end, Sregion_end, 0, 0, 0,
       doc: /* Return the integer value of point or mark, whichever is larger.  */)
  (void)
{
  return region_limit (0);
}

DEFUN ("mark-marker", Fmark_marker, Smark_marker, 0, 0, 0,
       doc: /* Return this buffer's mark, as a marker object.
Watch out!  Moving this marker changes the mark position.
If you set the marker not to point anywhere, the buffer will have no mark.  */)
  (void)
{
  return BVAR (current_buffer, mark);
}


/* Find all the overlays in the current buffer that touch position POS.
   Return the number found, and store them in a vector in VEC
   of length LEN.  */

static ptrdiff_t
overlays_around (EMACS_INT pos, Lisp_Object *vec, ptrdiff_t len)
{
  Lisp_Object overlay, start, end;
  struct Lisp_Overlay *tail;
  ptrdiff_t startpos, endpos;
  ptrdiff_t idx = 0;

  for (tail = current_buffer->overlays_before; tail; tail = tail->next)
    {
      XSETMISC (overlay, tail);

      end = OVERLAY_END (overlay);
      endpos = OVERLAY_POSITION (end);
      if (endpos < pos)
	  break;
      start = OVERLAY_START (overlay);
      startpos = OVERLAY_POSITION (start);
      if (startpos <= pos)
	{
	  if (idx < len)
	    vec[idx] = overlay;
	  /* Keep counting overlays even if we can't return them all.  */
	  idx++;
	}
    }

  for (tail = current_buffer->overlays_after; tail; tail = tail->next)
    {
      XSETMISC (overlay, tail);

      start = OVERLAY_START (overlay);
      startpos = OVERLAY_POSITION (start);
      if (pos < startpos)
	break;
      end = OVERLAY_END (overlay);
      endpos = OVERLAY_POSITION (end);
      if (pos <= endpos)
	{
	  if (idx < len)
	    vec[idx] = overlay;
	  idx++;
	}
    }

  return idx;
}

DEFUN ("get-pos-property", Fget_pos_property, Sget_pos_property, 2, 3, 0,
       doc: /* Return the value of POSITION's property PROP, in OBJECT.
Almost identical to `get-char-property' except for the following difference:
Whereas `get-char-property' returns the property of the char at (i.e. right
after) POSITION, this pays attention to properties's stickiness and overlays's
advancement settings, in order to find the property of POSITION itself,
i.e. the property that a char would inherit if it were inserted
at POSITION.  */)
  (Lisp_Object position, register Lisp_Object prop, Lisp_Object object)
{
  CHECK_NUMBER_COERCE_MARKER (position);

  if (NILP (object))
    XSETBUFFER (object, current_buffer);
  else if (WINDOWP (object))
    object = XWINDOW (object)->contents;

  if (!BUFFERP (object))
    /* pos-property only makes sense in buffers right now, since strings
       have no overlays and no notion of insertion for which stickiness
       could be obeyed.  */
    return Fget_text_property (position, prop, object);
  else
    {
      EMACS_INT posn = XINT (position);
      ptrdiff_t noverlays;
      Lisp_Object *overlay_vec, tem;
      struct buffer *obuf = current_buffer;
      USE_SAFE_ALLOCA;

      set_buffer_temp (XBUFFER (object));

      /* First try with room for 40 overlays.  */
      Lisp_Object overlay_vecbuf[40];
      noverlays = ARRAYELTS (overlay_vecbuf);
      overlay_vec = overlay_vecbuf;
      noverlays = overlays_around (posn, overlay_vec, noverlays);

      /* If there are more than 40,
	 make enough space for all, and try again.  */
      if (ARRAYELTS (overlay_vecbuf) < noverlays)
	{
	  SAFE_ALLOCA_LISP (overlay_vec, noverlays);
	  noverlays = overlays_around (posn, overlay_vec, noverlays);
	}
      noverlays = sort_overlays (overlay_vec, noverlays, NULL);

      set_buffer_temp (obuf);

      /* Now check the overlays in order of decreasing priority.  */
      while (--noverlays >= 0)
	{
	  Lisp_Object ol = overlay_vec[noverlays];
	  tem = Foverlay_get (ol, prop);
	  if (!NILP (tem))
	    {
	      /* Check the overlay is indeed active at point.  */
	      Lisp_Object start = OVERLAY_START (ol), finish = OVERLAY_END (ol);
	      if ((OVERLAY_POSITION (start) == posn
		   && XMARKER (start)->insertion_type == 1)
		  || (OVERLAY_POSITION (finish) == posn
		      && XMARKER (finish)->insertion_type == 0))
		; /* The overlay will not cover a char inserted at point.  */
	      else
		{
		  SAFE_FREE ();
		  return tem;
		}
	    }
	}
      SAFE_FREE ();

      { /* Now check the text properties.  */
	int stickiness = text_property_stickiness (prop, position, object);
	if (stickiness > 0)
	  return Fget_text_property (position, prop, object);
	else if (stickiness < 0
		 && XINT (position) > BUF_BEGV (XBUFFER (object)))
	  return Fget_text_property (make_number (XINT (position) - 1),
				     prop, object);
	else
	  return Qnil;
      }
    }
}

/* Find the field surrounding POS in *BEG and *END.  If POS is nil,
   the value of point is used instead.  If BEG or END is null,
   means don't store the beginning or end of the field.

   BEG_LIMIT and END_LIMIT serve to limit the ranged of the returned
   results; they do not effect boundary behavior.

   If MERGE_AT_BOUNDARY is non-nil, then if POS is at the very first
   position of a field, then the beginning of the previous field is
   returned instead of the beginning of POS's field (since the end of a
   field is actually also the beginning of the next input field, this
   behavior is sometimes useful).  Additionally in the MERGE_AT_BOUNDARY
   non-nil case, if two fields are separated by a field with the special
   value `boundary', and POS lies within it, then the two separated
   fields are considered to be adjacent, and POS between them, when
   finding the beginning and ending of the "merged" field.

   Either BEG or END may be 0, in which case the corresponding value
   is not stored.  */

static void
find_field (Lisp_Object pos, Lisp_Object merge_at_boundary,
	    Lisp_Object beg_limit,
	    ptrdiff_t *beg, Lisp_Object end_limit, ptrdiff_t *end)
{
  /* Fields right before and after the point.  */
  Lisp_Object before_field, after_field;
  /* True if POS counts as the start of a field.  */
  bool at_field_start = 0;
  /* True if POS counts as the end of a field.  */
  bool at_field_end = 0;

  if (NILP (pos))
    XSETFASTINT (pos, PT);
  else
    CHECK_NUMBER_COERCE_MARKER (pos);

  after_field
    = get_char_property_and_overlay (pos, Qfield, Qnil, NULL);
  before_field
    = (XFASTINT (pos) > BEGV
       ? get_char_property_and_overlay (make_number (XINT (pos) - 1),
					Qfield, Qnil, NULL)
       /* Using nil here would be a more obvious choice, but it would
          fail when the buffer starts with a non-sticky field.  */
       : after_field);

  /* See if we need to handle the case where MERGE_AT_BOUNDARY is nil
     and POS is at beginning of a field, which can also be interpreted
     as the end of the previous field.  Note that the case where if
     MERGE_AT_BOUNDARY is non-nil (see function comment) is actually the
     more natural one; then we avoid treating the beginning of a field
     specially.  */
  if (NILP (merge_at_boundary))
    {
      Lisp_Object field = Fget_pos_property (pos, Qfield, Qnil);
      if (!EQ (field, after_field))
	at_field_end = 1;
      if (!EQ (field, before_field))
	at_field_start = 1;
      if (NILP (field) && at_field_start && at_field_end)
	/* If an inserted char would have a nil field while the surrounding
	   text is non-nil, we're probably not looking at a
	   zero-length field, but instead at a non-nil field that's
	   not intended for editing (such as comint's prompts).  */
	at_field_end = at_field_start = 0;
    }

  /* Note about special `boundary' fields:

     Consider the case where the point (`.') is between the fields `x' and `y':

	xxxx.yyyy

     In this situation, if merge_at_boundary is non-nil, consider the
     `x' and `y' fields as forming one big merged field, and so the end
     of the field is the end of `y'.

     However, if `x' and `y' are separated by a special `boundary' field
     (a field with a `field' char-property of 'boundary), then ignore
     this special field when merging adjacent fields.  Here's the same
     situation, but with a `boundary' field between the `x' and `y' fields:

	xxx.BBBByyyy

     Here, if point is at the end of `x', the beginning of `y', or
     anywhere in-between (within the `boundary' field), merge all
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
	  Lisp_Object p = pos;
	  if (!NILP (merge_at_boundary) && EQ (before_field, Qboundary))
	    /* Skip a `boundary' field.  */
	    p = Fprevious_single_char_property_change (p, Qfield, Qnil,
						       beg_limit);

	  p = Fprevious_single_char_property_change (p, Qfield, Qnil,
						     beg_limit);
	  *beg = NILP (p) ? BEGV : XFASTINT (p);
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
  (Lisp_Object pos)
{
  ptrdiff_t beg, end;
  find_field (pos, Qnil, Qnil, &beg, Qnil, &end);
  if (beg != end)
    del_range (beg, end);
  return Qnil;
}

DEFUN ("field-string", Ffield_string, Sfield_string, 0, 1, 0,
       doc: /* Return the contents of the field surrounding POS as a string.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS.  */)
  (Lisp_Object pos)
{
  ptrdiff_t beg, end;
  find_field (pos, Qnil, Qnil, &beg, Qnil, &end);
  return make_buffer_string (beg, end, 1);
}

DEFUN ("field-string-no-properties", Ffield_string_no_properties, Sfield_string_no_properties, 0, 1, 0,
       doc: /* Return the contents of the field around POS, without text properties.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS.  */)
  (Lisp_Object pos)
{
  ptrdiff_t beg, end;
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
  (Lisp_Object pos, Lisp_Object escape_from_edge, Lisp_Object limit)
{
  ptrdiff_t beg;
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
  (Lisp_Object pos, Lisp_Object escape_from_edge, Lisp_Object limit)
{
  ptrdiff_t end;
  find_field (pos, escape_from_edge, Qnil, 0, limit, &end);
  return make_number (end);
}

DEFUN ("constrain-to-field", Fconstrain_to_field, Sconstrain_to_field, 2, 5, 0,
       doc: /* Return the position closest to NEW-POS that is in the same field as OLD-POS.
A field is a region of text with the same `field' property.

If NEW-POS is nil, then use the current point instead, and move point
to the resulting constrained position, in addition to returning that
position.

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
unconstrained.  This is useful for commands that move by line, like
\\[next-line] or \\[beginning-of-line], which should generally respect field boundaries
only in the case where they can still move to the right line.

If the optional argument INHIBIT-CAPTURE-PROPERTY is non-nil, and OLD-POS has
a non-nil property of that name, then any field boundaries are ignored.

Field boundaries are not noticed if `inhibit-field-text-motion' is non-nil.  */)
  (Lisp_Object new_pos, Lisp_Object old_pos, Lisp_Object escape_from_edge,
   Lisp_Object only_in_line, Lisp_Object inhibit_capture_property)
{
  /* If non-zero, then the original point, before re-positioning.  */
  ptrdiff_t orig_point = 0;
  bool fwd;
  Lisp_Object prev_old, prev_new;

  if (NILP (new_pos))
    /* Use the current point, and afterwards, set it.  */
    {
      orig_point = PT;
      XSETFASTINT (new_pos, PT);
    }

  CHECK_NUMBER_COERCE_MARKER (new_pos);
  CHECK_NUMBER_COERCE_MARKER (old_pos);

  fwd = (XINT (new_pos) > XINT (old_pos));

  prev_old = make_number (XINT (old_pos) - 1);
  prev_new = make_number (XINT (new_pos) - 1);

  if (NILP (Vinhibit_field_text_motion)
      && !EQ (new_pos, old_pos)
      && (!NILP (Fget_char_property (new_pos, Qfield, Qnil))
          || !NILP (Fget_char_property (old_pos, Qfield, Qnil))
          /* To recognize field boundaries, we must also look at the
             previous positions; we could use `Fget_pos_property'
             instead, but in itself that would fail inside non-sticky
             fields (like comint prompts).  */
          || (XFASTINT (new_pos) > BEGV
              && !NILP (Fget_char_property (prev_new, Qfield, Qnil)))
          || (XFASTINT (old_pos) > BEGV
              && !NILP (Fget_char_property (prev_old, Qfield, Qnil))))
      && (NILP (inhibit_capture_property)
          /* Field boundaries are again a problem; but now we must
             decide the case exactly, so we need to call
             `get_pos_property' as well.  */
          || (NILP (Fget_pos_property (old_pos, inhibit_capture_property, Qnil))
              && (XFASTINT (old_pos) <= BEGV
                  || NILP (Fget_char_property
			   (old_pos, inhibit_capture_property, Qnil))
                  || NILP (Fget_char_property
			   (prev_old, inhibit_capture_property, Qnil))))))
    /* It is possible that NEW_POS is not within the same field as
       OLD_POS; try to move NEW_POS so that it is.  */
    {
      ptrdiff_t shortage;
      Lisp_Object field_bound;

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
	     case the constraint is OK even if ONLY_IN_LINE is non-nil).  */
	  && (NILP (only_in_line)
	      /* This is the ONLY_IN_LINE case, check that NEW_POS and
		 FIELD_BOUND are on the same line by seeing whether
		 there's an intervening newline or not.  */
	      || (find_newline (XFASTINT (new_pos), -1,
				XFASTINT (field_bound), -1,
				fwd ? -1 : 1, &shortage, NULL, 1),
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
With optional argument N, scan forward N - 1 lines first.
If the scan reaches the end of the buffer, return that position.

This function ignores text display directionality; it returns the
position of the first character in logical order, i.e. the smallest
character position on the line.

This function constrains the returned position to the current field
unless that position would be on a different line than the original,
unconstrained result.  If N is nil or 1, and a front-sticky field
starts at point, the scan stops as soon as it starts.  To ignore field
boundaries, bind `inhibit-field-text-motion' to t.

This function does not move point.  */)
  (Lisp_Object n)
{
  ptrdiff_t charpos, bytepos;

  if (NILP (n))
    XSETFASTINT (n, 1);
  else
    CHECK_NUMBER (n);

  scan_newline_from_point (XINT (n) - 1, &charpos, &bytepos);

  /* Return END constrained to the current input field.  */
  return Fconstrain_to_field (make_number (charpos), make_number (PT),
			      XINT (n) != 1 ? Qt : Qnil,
			      Qt, Qnil);
}

DEFUN ("line-end-position", Fline_end_position, Sline_end_position, 0, 1, 0,
       doc: /* Return the character position of the last character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.

This function ignores text display directionality; it returns the
position of the last character in logical order, i.e. the largest
character position on the line.

This function constrains the returned position to the current field
unless that would be on a different line than the original,
unconstrained result.  If N is nil or 1, and a rear-sticky field ends
at point, the scan stops as soon as it starts.  To ignore field
boundaries bind `inhibit-field-text-motion' to t.

This function does not move point.  */)
  (Lisp_Object n)
{
  ptrdiff_t clipped_n;
  ptrdiff_t end_pos;
  ptrdiff_t orig = PT;

  if (NILP (n))
    XSETFASTINT (n, 1);
  else
    CHECK_NUMBER (n);

  clipped_n = clip_to_bounds (PTRDIFF_MIN + 1, XINT (n), PTRDIFF_MAX);
  end_pos = find_before_next_newline (orig, 0, clipped_n - (clipped_n <= 0),
				      NULL);

  /* Return END_POS constrained to the current input field.  */
  return Fconstrain_to_field (make_number (end_pos), make_number (orig),
			      Qnil, Qt, Qnil);
}

/* Save current buffer state for `save-excursion' special form.
   We (ab)use Lisp_Misc_Save_Value to allow explicit free and so
   offload some work from GC.  */

Lisp_Object
save_excursion_save (void)
{
  return make_save_obj_obj_obj_obj
    (Fpoint_marker (),
     Qnil,
     /* Selected window if current buffer is shown in it, nil otherwise.  */
     (EQ (XWINDOW (selected_window)->contents, Fcurrent_buffer ())
      ? selected_window : Qnil),
     Qnil);
}

/* Restore saved buffer before leaving `save-excursion' special form.  */

void
save_excursion_restore (Lisp_Object info)
{
  Lisp_Object tem, tem1;

  tem = Fmarker_buffer (XSAVE_OBJECT (info, 0));
  /* If we're unwinding to top level, saved buffer may be deleted.  This
     means that all of its markers are unchained and so tem is nil.  */
  if (NILP (tem))
    goto out;

  Fset_buffer (tem);

  /* Point marker.  */
  tem = XSAVE_OBJECT (info, 0);
  Fgoto_char (tem);
  unchain_marker (XMARKER (tem));

  /* If buffer was visible in a window, and a different window was
     selected, and the old selected window is still showing this
     buffer, restore point in that window.  */
  tem = XSAVE_OBJECT (info, 2);
  if (WINDOWP (tem)
      && !EQ (tem, selected_window)
      && (tem1 = XWINDOW (tem)->contents,
	  (/* Window is live...  */
	   BUFFERP (tem1)
	   /* ...and it shows the current buffer.  */
	   && XBUFFER (tem1) == current_buffer)))
    Fset_window_point (tem, make_number (PT));

 out:

  free_misc (info);
}

DEFUN ("save-excursion", Fsave_excursion, Ssave_excursion, 0, UNEVALLED, 0,
       doc: /* Save point, and current buffer; execute BODY; restore those things.
Executes BODY just like `progn'.
The values of point and the current buffer are restored
even in case of abnormal exit (throw or error).

If you only want to save the current buffer but not point,
then just use `save-current-buffer', or even `with-current-buffer'.

Before Emacs 25.1, `save-excursion' used to save the mark state.
To save the mark state as well as point and the current buffer, use
`save-mark-and-excursion'.

usage: (save-excursion &rest BODY)  */)
  (Lisp_Object args)
{
  register Lisp_Object val;
  ptrdiff_t count = SPECPDL_INDEX ();

  record_unwind_protect (save_excursion_restore, save_excursion_save ());

  val = Fprogn (args);
  return unbind_to (count, val);
}

DEFUN ("save-current-buffer", Fsave_current_buffer, Ssave_current_buffer, 0, UNEVALLED, 0,
       doc: /* Record which buffer is current; execute BODY; make that buffer current.
BODY is executed just like `progn'.
usage: (save-current-buffer &rest BODY)  */)
  (Lisp_Object args)
{
  ptrdiff_t count = SPECPDL_INDEX ();

  record_unwind_current_buffer ();
  return unbind_to (count, Fprogn (args));
}

DEFUN ("buffer-size", Fbuffer_size, Sbuffer_size, 0, 1, 0,
       doc: /* Return the number of characters in the current buffer.
If BUFFER is not nil, return the number of characters in that buffer
instead.

This does not take narrowing into account; to count the number of
characters in the accessible portion of the current buffer, use
`(- (point-max) (point-min))', and to count the number of characters
in some other BUFFER, use
`(with-current-buffer BUFFER (- (point-max) (point-min)))'.  */)
  (Lisp_Object buffer)
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
  (void)
{
  Lisp_Object temp;
  XSETFASTINT (temp, BEGV);
  return temp;
}

DEFUN ("point-min-marker", Fpoint_min_marker, Spoint_min_marker, 0, 0, 0,
       doc: /* Return a marker to the minimum permissible value of point in this buffer.
This is the beginning, unless narrowing (a buffer restriction) is in effect.  */)
  (void)
{
  return build_marker (current_buffer, BEGV, BEGV_BYTE);
}

DEFUN ("point-max", Fpoint_max, Spoint_max, 0, 0, 0,
       doc: /* Return the maximum permissible value of point in the current buffer.
This is (1+ (buffer-size)), unless narrowing (a buffer restriction)
is in effect, in which case it is less.  */)
  (void)
{
  Lisp_Object temp;
  XSETFASTINT (temp, ZV);
  return temp;
}

DEFUN ("point-max-marker", Fpoint_max_marker, Spoint_max_marker, 0, 0, 0,
       doc: /* Return a marker to the maximum permissible value of point in this buffer.
This is (1+ (buffer-size)), unless narrowing (a buffer restriction)
is in effect, in which case it is less.  */)
  (void)
{
  return build_marker (current_buffer, ZV, ZV_BYTE);
}

DEFUN ("gap-position", Fgap_position, Sgap_position, 0, 0, 0,
       doc: /* Return the position of the gap, in the current buffer.
See also `gap-size'.  */)
  (void)
{
  Lisp_Object temp;
  XSETFASTINT (temp, GPT);
  return temp;
}

DEFUN ("gap-size", Fgap_size, Sgap_size, 0, 0, 0,
       doc: /* Return the size of the current buffer's gap.
See also `gap-position'.  */)
  (void)
{
  Lisp_Object temp;
  XSETFASTINT (temp, GAP_SIZE);
  return temp;
}

DEFUN ("position-bytes", Fposition_bytes, Sposition_bytes, 1, 1, 0,
       doc: /* Return the byte position for character position POSITION.
If POSITION is out of range, the value is nil.  */)
  (Lisp_Object position)
{
  CHECK_NUMBER_COERCE_MARKER (position);
  if (XINT (position) < BEG || XINT (position) > Z)
    return Qnil;
  return make_number (CHAR_TO_BYTE (XINT (position)));
}

DEFUN ("byte-to-position", Fbyte_to_position, Sbyte_to_position, 1, 1, 0,
       doc: /* Return the character position for byte position BYTEPOS.
If BYTEPOS is out of range, the value is nil.  */)
  (Lisp_Object bytepos)
{
  ptrdiff_t pos_byte;

  CHECK_NUMBER (bytepos);
  pos_byte = XINT (bytepos);
  if (pos_byte < BEG_BYTE || pos_byte > Z_BYTE)
    return Qnil;
  if (Z != Z_BYTE)
    /* There are multibyte characters in the buffer.
       The argument of BYTE_TO_CHAR must be a byte position at
       a character boundary, so search for the start of the current
       character.  */
    while (!CHAR_HEAD_P (FETCH_BYTE (pos_byte)))
      pos_byte--;
  return make_number (BYTE_TO_CHAR (pos_byte));
}

DEFUN ("following-char", Ffollowing_char, Sfollowing_char, 0, 0, 0,
       doc: /* Return the character following point, as a number.
At the end of the buffer or accessible region, return 0.  */)
  (void)
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
  (void)
{
  Lisp_Object temp;
  if (PT <= BEGV)
    XSETFASTINT (temp, 0);
  else if (!NILP (BVAR (current_buffer, enable_multibyte_characters)))
    {
      ptrdiff_t pos = PT_BYTE;
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
  (void)
{
  if (PT == BEGV)
    return Qt;
  return Qnil;
}

DEFUN ("eobp", Feobp, Seobp, 0, 0, 0,
       doc: /* Return t if point is at the end of the buffer.
If the buffer is narrowed, this means the end of the narrowed part.  */)
  (void)
{
  if (PT == ZV)
    return Qt;
  return Qnil;
}

DEFUN ("bolp", Fbolp, Sbolp, 0, 0, 0,
       doc: /* Return t if point is at the beginning of a line.  */)
  (void)
{
  if (PT == BEGV || FETCH_BYTE (PT_BYTE - 1) == '\n')
    return Qt;
  return Qnil;
}

DEFUN ("eolp", Feolp, Seolp, 0, 0, 0,
       doc: /* Return t if point is at the end of a line.
`End of a line' includes point being at the end of the buffer.  */)
  (void)
{
  if (PT == ZV || FETCH_BYTE (PT_BYTE) == '\n')
    return Qt;
  return Qnil;
}

DEFUN ("char-after", Fchar_after, Schar_after, 0, 1, 0,
       doc: /* Return character in current buffer at position POS.
POS is an integer or a marker and defaults to point.
If POS is out of range, the value is nil.  */)
  (Lisp_Object pos)
{
  register ptrdiff_t pos_byte;

  if (NILP (pos))
    {
      pos_byte = PT_BYTE;
      if (pos_byte < BEGV_BYTE || pos_byte >= ZV_BYTE)
        return Qnil;
    }
  else if (MARKERP (pos))
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
POS is an integer or a marker and defaults to point.
If POS is out of range, the value is nil.  */)
  (Lisp_Object pos)
{
  register Lisp_Object val;
  register ptrdiff_t pos_byte;

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

  if (!NILP (BVAR (current_buffer, enable_multibyte_characters)))
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
Also, if the environment variables LOGNAME or USER are set,
that determines the value of this function.

If optional argument UID is an integer or a float, return the login name
of the user with that uid, or nil if there is no such user.  */)
  (Lisp_Object uid)
{
  struct passwd *pw;
  uid_t id;

  /* Set up the user name info if we didn't do it before.
     (That can happen if Emacs is dumpable
     but you decide to run `temacs -l loadup' and not dump.  */
  if (NILP (Vuser_login_name))
    init_editfns (false);

  if (NILP (uid))
    return Vuser_login_name;

  CONS_TO_INTEGER (uid, uid_t, id);
  block_input ();
  pw = getpwuid (id);
  unblock_input ();
  return (pw ? build_string (pw->pw_name) : Qnil);
}

DEFUN ("user-real-login-name", Fuser_real_login_name, Suser_real_login_name,
       0, 0, 0,
       doc: /* Return the name of the user's real uid, as a string.
This ignores the environment variables LOGNAME and USER, so it differs from
`user-login-name' when running under `su'.  */)
  (void)
{
  /* Set up the user name info if we didn't do it before.
     (That can happen if Emacs is dumpable
     but you decide to run `temacs -l loadup' and not dump.  */
  if (NILP (Vuser_login_name))
    init_editfns (false);
  return Vuser_real_login_name;
}

DEFUN ("user-uid", Fuser_uid, Suser_uid, 0, 0, 0,
       doc: /* Return the effective uid of Emacs.
Value is an integer or a float, depending on the value.  */)
  (void)
{
  uid_t euid = geteuid ();
  return make_fixnum_or_float (euid);
}

DEFUN ("user-real-uid", Fuser_real_uid, Suser_real_uid, 0, 0, 0,
       doc: /* Return the real uid of Emacs.
Value is an integer or a float, depending on the value.  */)
  (void)
{
  uid_t uid = getuid ();
  return make_fixnum_or_float (uid);
}

DEFUN ("group-gid", Fgroup_gid, Sgroup_gid, 0, 0, 0,
       doc: /* Return the effective gid of Emacs.
Value is an integer or a float, depending on the value.  */)
  (void)
{
  gid_t egid = getegid ();
  return make_fixnum_or_float (egid);
}

DEFUN ("group-real-gid", Fgroup_real_gid, Sgroup_real_gid, 0, 0, 0,
       doc: /* Return the real gid of Emacs.
Value is an integer or a float, depending on the value.  */)
  (void)
{
  gid_t gid = getgid ();
  return make_fixnum_or_float (gid);
}

DEFUN ("user-full-name", Fuser_full_name, Suser_full_name, 0, 1, 0,
       doc: /* Return the full name of the user logged in, as a string.
If the full name corresponding to Emacs's userid is not known,
return "unknown".

If optional argument UID is an integer or float, return the full name
of the user with that uid, or nil if there is no such user.
If UID is a string, return the full name of the user with that login
name, or nil if there is no such user.  */)
  (Lisp_Object uid)
{
  struct passwd *pw;
  register char *p, *q;
  Lisp_Object full;

  if (NILP (uid))
    return Vuser_full_name;
  else if (NUMBERP (uid))
    {
      uid_t u;
      CONS_TO_INTEGER (uid, uid_t, u);
      block_input ();
      pw = getpwuid (u);
      unblock_input ();
    }
  else if (STRINGP (uid))
    {
      block_input ();
      pw = getpwnam (SSDATA (uid));
      unblock_input ();
    }
  else
    error ("Invalid UID specification");

  if (!pw)
    return Qnil;

  p = USER_FULL_NAME;
  /* Chop off everything after the first comma. */
  q = strchr (p, ',');
  full = make_string (p, q ? q - p : strlen (p));

#ifdef AMPERSAND_FULL_NAME
  p = SSDATA (full);
  q = strchr (p, '&');
  /* Substitute the login name for the &, upcasing the first character.  */
  if (q)
    {
      Lisp_Object login = Fuser_login_name (make_number (pw->pw_uid));
      USE_SAFE_ALLOCA;
      char *r = SAFE_ALLOCA (strlen (p) + SBYTES (login) + 1);
      memcpy (r, p, q - p);
      char *s = lispstpcpy (&r[q - p], login);
      r[q - p] = upcase ((unsigned char) r[q - p]);
      strcpy (s, q + 1);
      full = build_string (r);
      SAFE_FREE ();
    }
#endif /* AMPERSAND_FULL_NAME */

  return full;
}

DEFUN ("system-name", Fsystem_name, Ssystem_name, 0, 0, 0,
       doc: /* Return the host name of the machine you are running on, as a string.  */)
  (void)
{
  if (EQ (Vsystem_name, cached_system_name))
    init_and_cache_system_name ();
  return Vsystem_name;
}

DEFUN ("emacs-pid", Femacs_pid, Semacs_pid, 0, 0, 0,
       doc: /* Return the process ID of Emacs, as a number.  */)
  (void)
{
  pid_t pid = getpid ();
  return make_fixnum_or_float (pid);
}



#ifndef TIME_T_MIN
# define TIME_T_MIN TYPE_MINIMUM (time_t)
#endif
#ifndef TIME_T_MAX
# define TIME_T_MAX TYPE_MAXIMUM (time_t)
#endif

/* Report that a time value is out of range for Emacs.  */
void
time_overflow (void)
{
  error ("Specified time is not representable");
}

static _Noreturn void
invalid_time (void)
{
  error ("Invalid time specification");
}

/* Check a return value compatible with that of decode_time_components.  */
static void
check_time_validity (int validity)
{
  if (validity <= 0)
    {
      if (validity < 0)
	time_overflow ();
      else
	invalid_time ();
    }
}

/* Return the upper part of the time T (everything but the bottom 16 bits).  */
static EMACS_INT
hi_time (time_t t)
{
  time_t hi = t >> LO_TIME_BITS;
  if (FIXNUM_OVERFLOW_P (hi))
    time_overflow ();
  return hi;
}

/* Return the bottom bits of the time T.  */
static int
lo_time (time_t t)
{
  return t & ((1 << LO_TIME_BITS) - 1);
}

DEFUN ("current-time", Fcurrent_time, Scurrent_time, 0, 0, 0,
       doc: /* Return the current time, as the number of seconds since 1970-01-01 00:00:00.
The time is returned as a list of integers (HIGH LOW USEC PSEC).
HIGH has the most significant bits of the seconds, while LOW has the
least significant 16 bits.  USEC and PSEC are the microsecond and
picosecond counts.  */)
  (void)
{
  return make_lisp_time (current_timespec ());
}

static struct lisp_time
time_add (struct lisp_time ta, struct lisp_time tb)
{
  EMACS_INT hi = ta.hi + tb.hi;
  int lo = ta.lo + tb.lo;
  int us = ta.us + tb.us;
  int ps = ta.ps + tb.ps;
  us += (1000000 <= ps);
  ps -= (1000000 <= ps) * 1000000;
  lo += (1000000 <= us);
  us -= (1000000 <= us) * 1000000;
  hi += (1 << LO_TIME_BITS <= lo);
  lo -= (1 << LO_TIME_BITS <= lo) << LO_TIME_BITS;
  return (struct lisp_time) { hi, lo, us, ps };
}

static struct lisp_time
time_subtract (struct lisp_time ta, struct lisp_time tb)
{
  EMACS_INT hi = ta.hi - tb.hi;
  int lo = ta.lo - tb.lo;
  int us = ta.us - tb.us;
  int ps = ta.ps - tb.ps;
  us -= (ps < 0);
  ps += (ps < 0) * 1000000;
  lo -= (us < 0);
  us += (us < 0) * 1000000;
  hi -= (lo < 0);
  lo += (lo < 0) << LO_TIME_BITS;
  return (struct lisp_time) { hi, lo, us, ps };
}

static Lisp_Object
time_arith (Lisp_Object a, Lisp_Object b,
	    struct lisp_time (*op) (struct lisp_time, struct lisp_time))
{
  int alen, blen;
  struct lisp_time ta = lisp_time_struct (a, &alen);
  struct lisp_time tb = lisp_time_struct (b, &blen);
  struct lisp_time t = op (ta, tb);
  if (FIXNUM_OVERFLOW_P (t.hi))
    time_overflow ();
  Lisp_Object val = Qnil;

  switch (max (alen, blen))
    {
    default:
      val = Fcons (make_number (t.ps), val);
      FALLTHROUGH;
    case 3:
      val = Fcons (make_number (t.us), val);
      FALLTHROUGH;
    case 2:
      val = Fcons (make_number (t.lo), val);
      val = Fcons (make_number (t.hi), val);
      break;
    }

  return val;
}

DEFUN ("time-add", Ftime_add, Stime_add, 2, 2, 0,
       doc: /* Return the sum of two time values A and B, as a time value.
A nil value for either argument stands for the current time.
See `current-time-string' for the various forms of a time value.  */)
  (Lisp_Object a, Lisp_Object b)
{
  return time_arith (a, b, time_add);
}

DEFUN ("time-subtract", Ftime_subtract, Stime_subtract, 2, 2, 0,
       doc: /* Return the difference between two time values A and B, as a time value.
Use `float-time' to convert the difference into elapsed seconds.
A nil value for either argument stands for the current time.
See `current-time-string' for the various forms of a time value.  */)
  (Lisp_Object a, Lisp_Object b)
{
  return time_arith (a, b, time_subtract);
}

DEFUN ("time-less-p", Ftime_less_p, Stime_less_p, 2, 2, 0,
       doc: /* Return non-nil if time value T1 is earlier than time value T2.
A nil value for either argument stands for the current time.
See `current-time-string' for the various forms of a time value.  */)
  (Lisp_Object t1, Lisp_Object t2)
{
  int t1len, t2len;
  struct lisp_time a = lisp_time_struct (t1, &t1len);
  struct lisp_time b = lisp_time_struct (t2, &t2len);
  return ((a.hi != b.hi ? a.hi < b.hi
	   : a.lo != b.lo ? a.lo < b.lo
	   : a.us != b.us ? a.us < b.us
	   : a.ps < b.ps)
	  ? Qt : Qnil);
}


DEFUN ("get-internal-run-time", Fget_internal_run_time, Sget_internal_run_time,
       0, 0, 0,
       doc: /* Return the current run time used by Emacs.
The time is returned as a list (HIGH LOW USEC PSEC), using the same
style as (current-time).

On systems that can't determine the run time, `get-internal-run-time'
does the same thing as `current-time'.  */)
  (void)
{
#ifdef HAVE_GETRUSAGE
  struct rusage usage;
  time_t secs;
  int usecs;

  if (getrusage (RUSAGE_SELF, &usage) < 0)
    /* This shouldn't happen.  What action is appropriate?  */
    xsignal0 (Qerror);

  /* Sum up user time and system time.  */
  secs = usage.ru_utime.tv_sec + usage.ru_stime.tv_sec;
  usecs = usage.ru_utime.tv_usec + usage.ru_stime.tv_usec;
  if (usecs >= 1000000)
    {
      usecs -= 1000000;
      secs++;
    }
  return make_lisp_time (make_timespec (secs, usecs * 1000));
#else /* ! HAVE_GETRUSAGE  */
#ifdef WINDOWSNT
  return w32_get_internal_run_time ();
#else /* ! WINDOWSNT  */
  return Fcurrent_time ();
#endif /* WINDOWSNT  */
#endif /* HAVE_GETRUSAGE  */
}


/* Make a Lisp list that represents the Emacs time T.  T may be an
   invalid time, with a slightly negative tv_nsec value such as
   UNKNOWN_MODTIME_NSECS; in that case, the Lisp list contains a
   correspondingly negative picosecond count.  */
Lisp_Object
make_lisp_time (struct timespec t)
{
  time_t s = t.tv_sec;
  int ns = t.tv_nsec;
  return list4i (hi_time (s), lo_time (s), ns / 1000, ns % 1000 * 1000);
}

/* Decode a Lisp list SPECIFIED_TIME that represents a time.
   Set *PHIGH, *PLOW, *PUSEC, *PPSEC to its parts; do not check their values.
   Return 2, 3, or 4 to indicate the effective length of SPECIFIED_TIME
   if successful, 0 if unsuccessful.  */
static int
disassemble_lisp_time (Lisp_Object specified_time, Lisp_Object *phigh,
		       Lisp_Object *plow, Lisp_Object *pusec,
		       Lisp_Object *ppsec)
{
  Lisp_Object high = make_number (0);
  Lisp_Object low = specified_time;
  Lisp_Object usec = make_number (0);
  Lisp_Object psec = make_number (0);
  int len = 4;

  if (CONSP (specified_time))
    {
      high = XCAR (specified_time);
      low = XCDR (specified_time);
      if (CONSP (low))
	{
	  Lisp_Object low_tail = XCDR (low);
	  low = XCAR (low);
	  if (CONSP (low_tail))
	    {
	      usec = XCAR (low_tail);
	      low_tail = XCDR (low_tail);
	      if (CONSP (low_tail))
		psec = XCAR (low_tail);
	      else
		len = 3;
	    }
	  else if (!NILP (low_tail))
	    {
	      usec = low_tail;
	      len = 3;
	    }
	  else
	    len = 2;
	}
      else
	len = 2;

      /* When combining components, require LOW to be an integer,
	 as otherwise it would be a pain to add up times.  */
      if (! INTEGERP (low))
	return 0;
    }
  else if (INTEGERP (specified_time))
    len = 2;

  *phigh = high;
  *plow = low;
  *pusec = usec;
  *ppsec = psec;
  return len;
}

/* Convert T into an Emacs time *RESULT, truncating toward minus infinity.
   Return true if T is in range, false otherwise.  */
static bool
decode_float_time (double t, struct lisp_time *result)
{
  double lo_multiplier = 1 << LO_TIME_BITS;
  double emacs_time_min = MOST_NEGATIVE_FIXNUM * lo_multiplier;
  if (! (emacs_time_min <= t && t < -emacs_time_min))
    return false;

  double small_t = t / lo_multiplier;
  EMACS_INT hi = small_t;
  double t_sans_hi = t - hi * lo_multiplier;
  int lo = t_sans_hi;
  long double fracps = (t_sans_hi - lo) * 1e12L;
#ifdef INT_FAST64_MAX
  int_fast64_t ifracps = fracps;
  int us = ifracps / 1000000;
  int ps = ifracps % 1000000;
#else
  int us = fracps / 1e6L;
  int ps = fracps - us * 1e6L;
#endif
  us -= (ps < 0);
  ps += (ps < 0) * 1000000;
  lo -= (us < 0);
  us += (us < 0) * 1000000;
  hi -= (lo < 0);
  lo += (lo < 0) << LO_TIME_BITS;
  result->hi = hi;
  result->lo = lo;
  result->us = us;
  result->ps = ps;
  return true;
}

/* From the time components HIGH, LOW, USEC and PSEC taken from a Lisp
   list, generate the corresponding time value.
   If LOW is floating point, the other components should be zero.

   If RESULT is not null, store into *RESULT the converted time.
   If *DRESULT is not null, store into *DRESULT the number of
   seconds since the start of the POSIX Epoch.

   Return 1 if successful, 0 if the components are of the
   wrong type, and -1 if the time is out of range.  */
int
decode_time_components (Lisp_Object high, Lisp_Object low, Lisp_Object usec,
			Lisp_Object psec,
			struct lisp_time *result, double *dresult)
{
  EMACS_INT hi, lo, us, ps;
  if (! (INTEGERP (high)
	 && INTEGERP (usec) && INTEGERP (psec)))
    return 0;
  if (! INTEGERP (low))
    {
      if (FLOATP (low))
	{
	  double t = XFLOAT_DATA (low);
	  if (result && ! decode_float_time (t, result))
	    return -1;
	  if (dresult)
	    *dresult = t;
	  return 1;
	}
      else if (NILP (low))
	{
	  struct timespec now = current_timespec ();
	  if (result)
	    {
	      result->hi = hi_time (now.tv_sec);
	      result->lo = lo_time (now.tv_sec);
	      result->us = now.tv_nsec / 1000;
	      result->ps = now.tv_nsec % 1000 * 1000;
	    }
	  if (dresult)
	    *dresult = now.tv_sec + now.tv_nsec / 1e9;
	  return 1;
	}
      else
	return 0;
    }

  hi = XINT (high);
  lo = XINT (low);
  us = XINT (usec);
  ps = XINT (psec);

  /* Normalize out-of-range lower-order components by carrying
     each overflow into the next higher-order component.  */
  us += ps / 1000000 - (ps % 1000000 < 0);
  lo += us / 1000000 - (us % 1000000 < 0);
  hi += lo >> LO_TIME_BITS;
  ps = ps % 1000000 + 1000000 * (ps % 1000000 < 0);
  us = us % 1000000 + 1000000 * (us % 1000000 < 0);
  lo &= (1 << LO_TIME_BITS) - 1;

  if (result)
    {
      if (FIXNUM_OVERFLOW_P (hi))
	return -1;
      result->hi = hi;
      result->lo = lo;
      result->us = us;
      result->ps = ps;
    }

  if (dresult)
    {
      double dhi = hi;
      *dresult = (us * 1e6 + ps) / 1e12 + lo + dhi * (1 << LO_TIME_BITS);
    }

  return 1;
}

struct timespec
lisp_to_timespec (struct lisp_time t)
{
  if (! ((TYPE_SIGNED (time_t) ? TIME_T_MIN >> LO_TIME_BITS <= t.hi : 0 <= t.hi)
	 && t.hi <= TIME_T_MAX >> LO_TIME_BITS))
    return invalid_timespec ();
  time_t s = (t.hi << LO_TIME_BITS) + t.lo;
  int ns = t.us * 1000 + t.ps / 1000;
  return make_timespec (s, ns);
}

/* Decode a Lisp list SPECIFIED_TIME that represents a time.
   Store its effective length into *PLEN.
   If SPECIFIED_TIME is nil, use the current time.
   Signal an error if SPECIFIED_TIME does not represent a time.  */
static struct lisp_time
lisp_time_struct (Lisp_Object specified_time, int *plen)
{
  Lisp_Object high, low, usec, psec;
  struct lisp_time t;
  int len = disassemble_lisp_time (specified_time, &high, &low, &usec, &psec);
  if (!len)
    invalid_time ();
  int val = decode_time_components (high, low, usec, psec, &t, 0);
  check_time_validity (val);
  *plen = len;
  return t;
}

/* Like lisp_time_struct, except return a struct timespec.
   Discard any low-order digits.  */
struct timespec
lisp_time_argument (Lisp_Object specified_time)
{
  int len;
  struct lisp_time lt = lisp_time_struct (specified_time, &len);
  struct timespec t = lisp_to_timespec (lt);
  if (! timespec_valid_p (t))
    time_overflow ();
  return t;
}

/* Like lisp_time_argument, except decode only the seconds part,
   and do not check the subseconds part.  */
static time_t
lisp_seconds_argument (Lisp_Object specified_time)
{
  Lisp_Object high, low, usec, psec;
  struct lisp_time t;

  int val = disassemble_lisp_time (specified_time, &high, &low, &usec, &psec);
  if (val != 0)
    {
      val = decode_time_components (high, low, make_number (0),
				    make_number (0), &t, 0);
      if (0 < val
	  && ! ((TYPE_SIGNED (time_t)
		 ? TIME_T_MIN >> LO_TIME_BITS <= t.hi
		 : 0 <= t.hi)
		&& t.hi <= TIME_T_MAX >> LO_TIME_BITS))
	val = -1;
    }
  check_time_validity (val);
  return (t.hi << LO_TIME_BITS) + t.lo;
}

DEFUN ("float-time", Ffloat_time, Sfloat_time, 0, 1, 0,
       doc: /* Return the current time, as a float number of seconds since the epoch.
If SPECIFIED-TIME is given, it is the time to convert to float
instead of the current time.  The argument should have the form
\(HIGH LOW) or (HIGH LOW USEC) or (HIGH LOW USEC PSEC).  Thus,
you can use times from `current-time' and from `file-attributes'.
SPECIFIED-TIME can also have the form (HIGH . LOW), but this is
considered obsolete.

WARNING: Since the result is floating point, it may not be exact.
If precise time stamps are required, use either `current-time',
or (if you need time as a string) `format-time-string'.  */)
  (Lisp_Object specified_time)
{
  double t;
  Lisp_Object high, low, usec, psec;
  if (! (disassemble_lisp_time (specified_time, &high, &low, &usec, &psec)
	 && decode_time_components (high, low, usec, psec, 0, &t)))
    invalid_time ();
  return make_float (t);
}

/* Write information into buffer S of size MAXSIZE, according to the
   FORMAT of length FORMAT_LEN, using time information taken from *TP.
   Use the time zone specified by TZ.
   Use NS as the number of nanoseconds in the %N directive.
   Return the number of bytes written, not including the terminating
   '\0'.  If S is NULL, nothing will be written anywhere; so to
   determine how many bytes would be written, use NULL for S and
   ((size_t) -1) for MAXSIZE.

   This function behaves like nstrftime, except it allows null
   bytes in FORMAT and it does not support nanoseconds.  */
static size_t
emacs_nmemftime (char *s, size_t maxsize, const char *format,
		 size_t format_len, const struct tm *tp, timezone_t tz, int ns)
{
  size_t total = 0;

  /* Loop through all the null-terminated strings in the format
     argument.  Normally there's just one null-terminated string, but
     there can be arbitrarily many, concatenated together, if the
     format contains '\0' bytes.  nstrftime stops at the first
     '\0' byte so we must invoke it separately for each such string.  */
  for (;;)
    {
      size_t len;
      size_t result;

      if (s)
	s[0] = '\1';

      result = nstrftime (s, maxsize, format, tp, tz, ns);

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
       doc: /* Use FORMAT-STRING to format the time TIME, or now if omitted or nil.
TIME is specified as (HIGH LOW USEC PSEC), as returned by
`current-time' or `file-attributes'.  It can also be a single integer
number of seconds since the epoch.  The obsolete form (HIGH . LOW) is
also still accepted.

The optional ZONE is omitted or nil for Emacs local time, t for
Universal Time, `wall' for system wall clock time, or a string as in
the TZ environment variable.  It can also be a list (as from
`current-time-zone') or an integer (as from `decode-time') applied
without consideration for daylight saving time.

The value is a copy of FORMAT-STRING, but with certain constructs replaced
by text that describes the specified date and time in TIME:

%Y is the year, %y within the century, %C the century.
%G is the year corresponding to the ISO week, %g within the century.
%m is the numeric month.
%b and %h are the locale's abbreviated month name, %B the full name.
 (%h is not supported on MS-Windows.)
%d is the day of the month, zero-padded, %e is blank-padded.
%u is the numeric day of week from 1 (Monday) to 7, %w from 0 (Sunday) to 6.
%a is the locale's abbreviated name of the day of week, %A the full name.
%U is the week number starting on Sunday, %W starting on Monday,
 %V according to ISO 8601.
%j is the day of the year.

%H is the hour on a 24-hour clock, %I is on a 12-hour clock, %k is like %H
 only blank-padded, %l is like %I blank-padded.
%p is the locale's equivalent of either AM or PM.
%q is the calendar quarter (1–4).
%M is the minute (00-59).
%S is the second (00-59; 00-60 on platforms with leap seconds)
%s is the number of seconds since 1970-01-01 00:00:00 +0000.
%N is the nanosecond, %6N the microsecond, %3N the millisecond, etc.
%Z is the time zone abbreviation, %z is the numeric form.

%c is the locale's date and time format.
%x is the locale's "preferred" date format.
%D is like "%m/%d/%y".
%F is the ISO 8601 date format (like "%Y-%m-%d").

%R is like "%H:%M", %T is like "%H:%M:%S", %r is like "%I:%M:%S %p".
%X is the locale's "preferred" time format.

Finally, %n is a newline, %t is a tab, %% is a literal %, and
unrecognized %-sequences stand for themselves.

Certain flags and modifiers are available with some format controls.
The flags are `_', `-', `^' and `#'.  For certain characters X,
%_X is like %X, but padded with blanks; %-X is like %X,
but without padding.  %^X is like %X, but with all textual
characters up-cased; %#X is like %X, but with letter-case of
all textual characters reversed.
%NX (where N stands for an integer) is like %X,
but takes up at least N (a number) positions.
The modifiers are `E' and `O'.  For certain characters X,
%EX is a locale's alternative version of %X;
%OX is like %X, but uses the locale's number symbols.

For example, to produce full ISO 8601 format, use "%FT%T%z".

usage: (format-time-string FORMAT-STRING &optional TIME ZONE)  */)
  (Lisp_Object format_string, Lisp_Object timeval, Lisp_Object zone)
{
  struct timespec t = lisp_time_argument (timeval);
  struct tm tm;

  CHECK_STRING (format_string);
  format_string = code_convert_string_norecord (format_string,
						Vlocale_coding_system, 1);
  return format_time_string (SSDATA (format_string), SBYTES (format_string),
			     t, zone, &tm);
}

static Lisp_Object
format_time_string (char const *format, ptrdiff_t formatlen,
		    struct timespec t, Lisp_Object zone, struct tm *tmp)
{
  char buffer[4000];
  char *buf = buffer;
  ptrdiff_t size = sizeof buffer;
  size_t len;
  int ns = t.tv_nsec;
  USE_SAFE_ALLOCA;

  timezone_t tz = tzlookup (zone, false);
  /* On some systems, like 32-bit MinGW, tv_sec of struct timespec is
     a 64-bit type, but time_t is a 32-bit type.  emacs_localtime_rz
     expects a pointer to time_t value.  */
  time_t tsec = t.tv_sec;
  tmp = emacs_localtime_rz (tz, &tsec, tmp);
  if (! tmp)
    {
      xtzfree (tz);
      time_overflow ();
    }
  synchronize_system_time_locale ();

  while (true)
    {
      buf[0] = '\1';
      len = emacs_nmemftime (buf, size, format, formatlen, tmp, tz, ns);
      if ((0 < len && len < size) || (len == 0 && buf[0] == '\0'))
	break;

      /* Buffer was too small, so make it bigger and try again.  */
      len = emacs_nmemftime (NULL, SIZE_MAX, format, formatlen, tmp, tz, ns);
      if (STRING_BYTES_BOUND <= len)
	{
	  xtzfree (tz);
	  string_overflow ();
	}
      size = len + 1;
      buf = SAFE_ALLOCA (size);
    }

  xtzfree (tz);
  AUTO_STRING_WITH_LEN (bufstring, buf, len);
  Lisp_Object result = code_convert_string_norecord (bufstring,
						     Vlocale_coding_system, 0);
  SAFE_FREE ();
  return result;
}

DEFUN ("decode-time", Fdecode_time, Sdecode_time, 0, 2, 0,
       doc: /* Decode a time value as (SEC MINUTE HOUR DAY MONTH YEAR DOW DST UTCOFF).
The optional TIME should be a list of (HIGH LOW . IGNORED),
as from `current-time' and `file-attributes', or nil to use the
current time.  It can also be a single integer number of seconds since
the epoch.  The obsolete form (HIGH . LOW) is also still accepted.

The optional ZONE is omitted or nil for Emacs local time, t for
Universal Time, `wall' for system wall clock time, or a string as in
the TZ environment variable.  It can also be a list (as from
`current-time-zone') or an integer (the UTC offset in seconds) applied
without consideration for daylight saving time.

The list has the following nine members: SEC is an integer between 0
and 60; SEC is 60 for a leap second, which only some operating systems
support.  MINUTE is an integer between 0 and 59.  HOUR is an integer
between 0 and 23.  DAY is an integer between 1 and 31.  MONTH is an
integer between 1 and 12.  YEAR is an integer indicating the
four-digit year.  DOW is the day of week, an integer between 0 and 6,
where 0 is Sunday.  DST is t if daylight saving time is in effect,
otherwise nil.  UTCOFF is an integer indicating the UTC offset in
seconds, i.e., the number of seconds east of Greenwich.  (Note that
Common Lisp has different meanings for DOW and UTCOFF.)

usage: (decode-time &optional TIME ZONE)  */)
  (Lisp_Object specified_time, Lisp_Object zone)
{
  time_t time_spec = lisp_seconds_argument (specified_time);
  struct tm local_tm, gmt_tm;
  timezone_t tz = tzlookup (zone, false);
  struct tm *tm = emacs_localtime_rz (tz, &time_spec, &local_tm);
  xtzfree (tz);

  if (! (tm
	 && MOST_NEGATIVE_FIXNUM - TM_YEAR_BASE <= local_tm.tm_year
	 && local_tm.tm_year <= MOST_POSITIVE_FIXNUM - TM_YEAR_BASE))
    time_overflow ();

  /* Avoid overflow when INT_MAX < EMACS_INT_MAX.  */
  EMACS_INT tm_year_base = TM_YEAR_BASE;

  return CALLN (Flist,
		make_number (local_tm.tm_sec),
		make_number (local_tm.tm_min),
		make_number (local_tm.tm_hour),
		make_number (local_tm.tm_mday),
		make_number (local_tm.tm_mon + 1),
		make_number (local_tm.tm_year + tm_year_base),
		make_number (local_tm.tm_wday),
		local_tm.tm_isdst ? Qt : Qnil,
		(HAVE_TM_GMTOFF
		 ? make_number (tm_gmtoff (&local_tm))
		 : gmtime_r (&time_spec, &gmt_tm)
		 ? make_number (tm_diff (&local_tm, &gmt_tm))
		 : Qnil));
}

/* Return OBJ - OFFSET, checking that OBJ is a valid fixnum and that
   the result is representable as an int.  */
static int
check_tm_member (Lisp_Object obj, int offset)
{
  CHECK_NUMBER (obj);
  EMACS_INT n = XINT (obj);
  int result;
  if (INT_SUBTRACT_WRAPV (n, offset, &result))
    time_overflow ();
  return result;
}

DEFUN ("encode-time", Fencode_time, Sencode_time, 6, MANY, 0,
       doc: /* Convert SECOND, MINUTE, HOUR, DAY, MONTH, YEAR and ZONE to internal time.
This is the reverse operation of `decode-time', which see.

The optional ZONE is omitted or nil for Emacs local time, t for
Universal Time, `wall' for system wall clock time, or a string as in
the TZ environment variable.  It can also be a list (as from
`current-time-zone') or an integer (as from `decode-time') applied
without consideration for daylight saving time.

You can pass more than 7 arguments; then the first six arguments
are used as SECOND through YEAR, and the *last* argument is used as ZONE.
The intervening arguments are ignored.
This feature lets (apply \\='encode-time (decode-time ...)) work.

Out-of-range values for SECOND, MINUTE, HOUR, DAY, or MONTH are allowed;
for example, a DAY of 0 means the day preceding the given month.
Year numbers less than 100 are treated just like other year numbers.
If you want them to stand for years in this century, you must do that yourself.

Years before 1970 are not guaranteed to work.  On some systems,
year values as low as 1901 do work.

usage: (encode-time SECOND MINUTE HOUR DAY MONTH YEAR &optional ZONE)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  time_t value;
  struct tm tm;
  Lisp_Object zone = (nargs > 6 ? args[nargs - 1] : Qnil);

  tm.tm_sec  = check_tm_member (args[0], 0);
  tm.tm_min  = check_tm_member (args[1], 0);
  tm.tm_hour = check_tm_member (args[2], 0);
  tm.tm_mday = check_tm_member (args[3], 0);
  tm.tm_mon  = check_tm_member (args[4], 1);
  tm.tm_year = check_tm_member (args[5], TM_YEAR_BASE);
  tm.tm_isdst = -1;

  timezone_t tz = tzlookup (zone, false);
  value = emacs_mktime_z (tz, &tm);
  xtzfree (tz);

  if (value == (time_t) -1)
    time_overflow ();

  return list2i (hi_time (value), lo_time (value));
}

DEFUN ("current-time-string", Fcurrent_time_string, Scurrent_time_string,
       0, 2, 0,
       doc: /* Return the current local time, as a human-readable string.
Programs can use this function to decode a time,
since the number of columns in each field is fixed
if the year is in the range 1000-9999.
The format is `Sun Sep 16 01:03:52 1973'.
However, see also the functions `decode-time' and `format-time-string'
which provide a much more powerful and general facility.

If SPECIFIED-TIME is given, it is a time to format instead of the
current time.  The argument should have the form (HIGH LOW . IGNORED).
Thus, you can use times obtained from `current-time' and from
`file-attributes'.  SPECIFIED-TIME can also be a single integer number
of seconds since the epoch.  The obsolete form (HIGH . LOW) is also
still accepted.

The optional ZONE is omitted or nil for Emacs local time, t for
Universal Time, `wall' for system wall clock time, or a string as in
the TZ environment variable.  It can also be a list (as from
`current-time-zone') or an integer (as from `decode-time') applied
without consideration for daylight saving time.  */)
  (Lisp_Object specified_time, Lisp_Object zone)
{
  time_t value = lisp_seconds_argument (specified_time);
  timezone_t tz = tzlookup (zone, false);

  /* Convert to a string in ctime format, except without the trailing
     newline, and without the 4-digit year limit.  Don't use asctime
     or ctime, as they might dump core if the year is outside the
     range -999 .. 9999.  */
  struct tm tm;
  struct tm *tmp = emacs_localtime_rz (tz, &value, &tm);
  xtzfree (tz);
  if (! tmp)
    time_overflow ();

  static char const wday_name[][4] =
    { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
  static char const mon_name[][4] =
    { "Jan", "Feb", "Mar", "Apr", "May", "Jun",
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };
  printmax_t year_base = TM_YEAR_BASE;
  char buf[sizeof "Mon Apr 30 12:49:17 " + INT_STRLEN_BOUND (int) + 1];
  int len = sprintf (buf, "%s %s%3d %02d:%02d:%02d %"pMd,
		     wday_name[tm.tm_wday], mon_name[tm.tm_mon], tm.tm_mday,
		     tm.tm_hour, tm.tm_min, tm.tm_sec,
		     tm.tm_year + year_base);

  return make_unibyte_string (buf, len);
}

/* Yield A - B, measured in seconds.
   This function is copied from the GNU C Library.  */
static int
tm_diff (struct tm *a, struct tm *b)
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

/* Yield A's UTC offset, or an unspecified value if unknown.  */
static long int
tm_gmtoff (struct tm *a)
{
#if HAVE_TM_GMTOFF
  return a->tm_gmtoff;
#else
  return 0;
#endif
}

DEFUN ("current-time-zone", Fcurrent_time_zone, Scurrent_time_zone, 0, 2, 0,
       doc: /* Return the offset and name for the local time zone.
This returns a list of the form (OFFSET NAME).
OFFSET is an integer number of seconds ahead of UTC (east of Greenwich).
    A negative value means west of Greenwich.
NAME is a string giving the name of the time zone.
If SPECIFIED-TIME is given, the time zone offset is determined from it
instead of using the current time.  The argument should have the form
\(HIGH LOW . IGNORED).  Thus, you can use times obtained from
`current-time' and from `file-attributes'.  SPECIFIED-TIME can also be
a single integer number of seconds since the epoch.  The obsolete form
(HIGH . LOW) is also still accepted.

The optional ZONE is omitted or nil for Emacs local time, t for
Universal Time, `wall' for system wall clock time, or a string as in
the TZ environment variable.  It can also be a list (as from
`current-time-zone') or an integer (as from `decode-time') applied
without consideration for daylight saving time.

Some operating systems cannot provide all this information to Emacs;
in this case, `current-time-zone' returns a list containing nil for
the data it can't find.  */)
  (Lisp_Object specified_time, Lisp_Object zone)
{
  struct timespec value;
  struct tm local_tm, gmt_tm;
  Lisp_Object zone_offset, zone_name;

  zone_offset = Qnil;
  value = make_timespec (lisp_seconds_argument (specified_time), 0);
  zone_name = format_time_string ("%Z", sizeof "%Z" - 1, value,
				  zone, &local_tm);

  /* gmtime_r expects a pointer to time_t, but tv_sec of struct
     timespec on some systems (MinGW) is a 64-bit field.  */
  time_t tsec = value.tv_sec;
  if (HAVE_TM_GMTOFF || gmtime_r (&tsec, &gmt_tm))
    {
      long int offset = (HAVE_TM_GMTOFF
			 ? tm_gmtoff (&local_tm)
			 : tm_diff (&local_tm, &gmt_tm));
      zone_offset = make_number (offset);
      if (SCHARS (zone_name) == 0)
	{
	  /* No local time zone name is available; use numeric zone instead.  */
	  long int hour = offset / 3600;
	  int min_sec = offset % 3600;
	  int amin_sec = min_sec < 0 ? - min_sec : min_sec;
	  int min = amin_sec / 60;
	  int sec = amin_sec % 60;
	  int min_prec = min_sec ? 2 : 0;
	  int sec_prec = sec ? 2 : 0;
	  char buf[sizeof "+0000" + INT_STRLEN_BOUND (long int)];
	  zone_name = make_formatted_string (buf, "%c%.2ld%.*d%.*d",
					     (offset < 0 ? '-' : '+'),
					     hour, min_prec, min, sec_prec, sec);
	}
    }

  return list2 (zone_offset, zone_name);
}

DEFUN ("set-time-zone-rule", Fset_time_zone_rule, Sset_time_zone_rule, 1, 1, 0,
       doc: /* Set the Emacs local time zone using TZ, a string specifying a time zone rule.
If TZ is nil or `wall', use system wall clock time; this differs from
the usual Emacs convention where nil means current local time.  If TZ
is t, use Universal Time.  If TZ is a list (as from
`current-time-zone') or an integer (as from `decode-time'), use the
specified time zone without consideration for daylight saving time.

Instead of calling this function, you typically want something else.
To temporarily use a different time zone rule for just one invocation
of `decode-time', `encode-time', or `format-time-string', pass the
function a ZONE argument.  To change local time consistently
throughout Emacs, call (setenv "TZ" TZ): this changes both the
environment of the Emacs process and the variable
`process-environment', whereas `set-time-zone-rule' affects only the
former.  */)
  (Lisp_Object tz)
{
  tzlookup (NILP (tz) ? Qwall : tz, true);
  return Qnil;
}

/* A buffer holding a string of the form "TZ=value", intended
   to be part of the environment.  If TZ is supposed to be unset,
   the buffer string is "tZ=".  */
 static char *tzvalbuf;

/* Get the local time zone rule.  */
char *
emacs_getenv_TZ (void)
{
  return tzvalbuf[0] == 'T' ? tzvalbuf + tzeqlen : 0;
}

/* Set the local time zone rule to TZSTRING, which can be null to
   denote wall clock time.  Do not record the setting in LOCAL_TZ.

   This function is not thread-safe, in theory because putenv is not,
   but mostly because of the static storage it updates.  Other threads
   that invoke localtime etc. may be adversely affected while this
   function is executing.  */

int
emacs_setenv_TZ (const char *tzstring)
{
  static ptrdiff_t tzvalbufsize;
  ptrdiff_t tzstringlen = tzstring ? strlen (tzstring) : 0;
  char *tzval = tzvalbuf;
  bool new_tzvalbuf = tzvalbufsize <= tzeqlen + tzstringlen;

  if (new_tzvalbuf)
    {
      /* Do not attempt to free the old tzvalbuf, since another thread
	 may be using it.  In practice, the first allocation is large
	 enough and memory does not leak.  */
      tzval = xpalloc (NULL, &tzvalbufsize,
		       tzeqlen + tzstringlen - tzvalbufsize + 1, -1, 1);
      tzvalbuf = tzval;
      tzval[1] = 'Z';
      tzval[2] = '=';
    }

  if (tzstring)
    {
      /* Modify TZVAL in place.  Although this is dicey in a
	 multithreaded environment, we know of no portable alternative.
	 Calling putenv or setenv could crash some other thread.  */
      tzval[0] = 'T';
      strcpy (tzval + tzeqlen, tzstring);
    }
  else
    {
      /* Turn 'TZ=whatever' into an empty environment variable 'tZ='.
	 Although this is also dicey, calling unsetenv here can crash Emacs.
	 See Bug#8705.  */
      tzval[0] = 't';
      tzval[tzeqlen] = 0;
    }


#ifndef WINDOWSNT
  /* Modifying *TZVAL merely requires calling tzset (which is the
     caller's responsibility).  However, modifying TZVAL requires
     calling putenv; although this is not thread-safe, in practice this
     runs only on startup when there is only one thread.  */
  bool need_putenv = new_tzvalbuf;
#else
  /* MS-Windows 'putenv' copies the argument string into a block it
     allocates, so modifying *TZVAL will not change the environment.
     However, the other threads run by Emacs on MS-Windows never call
     'xputenv' or 'putenv' or 'unsetenv', so the original cause for the
     dicey in-place modification technique doesn't exist there in the
     first place.  */
  bool need_putenv = true;
#endif
  if (need_putenv)
    xputenv (tzval);

  return 0;
}

/* Insert NARGS Lisp objects in the array ARGS by calling INSERT_FUNC
   (if a type of object is Lisp_Int) or INSERT_FROM_STRING_FUNC (if a
   type of object is Lisp_String).  INHERIT is passed to
   INSERT_FROM_STRING_FUNC as the last argument.  */

static void
general_insert_function (void (*insert_func)
			      (const char *, ptrdiff_t),
			 void (*insert_from_string_func)
			      (Lisp_Object, ptrdiff_t, ptrdiff_t,
			       ptrdiff_t, ptrdiff_t, bool),
			 bool inherit, ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t argnum;
  Lisp_Object val;

  for (argnum = 0; argnum < nargs; argnum++)
    {
      val = args[argnum];
      if (CHARACTERP (val))
	{
	  int c = XFASTINT (val);
	  unsigned char str[MAX_MULTIBYTE_LENGTH];
	  int len;

	  if (!NILP (BVAR (current_buffer, enable_multibyte_characters)))
	    len = CHAR_STRING (c, str);
	  else
	    {
	      str[0] = CHAR_TO_BYTE8 (c);
	      len = 1;
	    }
	  (*insert_func) ((char *) str, len);
	}
      else if (STRINGP (val))
	{
	  (*insert_from_string_func) (val, 0, 0,
				      SCHARS (val),
				      SBYTES (val),
				      inherit);
	}
      else
	wrong_type_argument (Qchar_or_string_p, val);
    }
}

void
insert1 (Lisp_Object arg)
{
  Finsert (1, &arg);
}


DEFUN ("insert", Finsert, Sinsert, 0, MANY, 0,
       doc: /* Insert the arguments, either strings or characters, at point.
Point and after-insertion markers move forward to end up
 after the inserted text.
Any other markers at the point of insertion remain before the text.

If the current buffer is multibyte, unibyte strings are converted
to multibyte for insertion (see `string-make-multibyte').
If the current buffer is unibyte, multibyte strings are converted
to unibyte for insertion (see `string-make-unibyte').

When operating on binary data, it may be necessary to preserve the
original bytes of a unibyte string when inserting it into a multibyte
buffer; to accomplish this, apply `string-as-multibyte' to the string
and insert the result.

usage: (insert &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  general_insert_function (insert, insert_from_string, 0, nargs, args);
  return Qnil;
}

DEFUN ("insert-and-inherit", Finsert_and_inherit, Sinsert_and_inherit,
   0, MANY, 0,
       doc: /* Insert the arguments at point, inheriting properties from adjoining text.
Point and after-insertion markers move forward to end up
 after the inserted text.
Any other markers at the point of insertion remain before the text.

If the current buffer is multibyte, unibyte strings are converted
to multibyte for insertion (see `unibyte-char-to-multibyte').
If the current buffer is unibyte, multibyte strings are converted
to unibyte for insertion.

usage: (insert-and-inherit &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
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

If an overlay begins at the insertion point, the inserted text falls
outside the overlay; if a nonempty overlay ends at the insertion
point, the inserted text falls inside that overlay.

usage: (insert-before-markers &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
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
  (ptrdiff_t nargs, Lisp_Object *args)
{
  general_insert_function (insert_before_markers_and_inherit,
			   insert_from_string_before_markers, 1,
			   nargs, args);
  return Qnil;
}

DEFUN ("insert-char", Finsert_char, Sinsert_char, 1, 3,
       "(list (read-char-by-name \"Insert character (Unicode name or hex): \")\
              (prefix-numeric-value current-prefix-arg)\
              t))",
       doc: /* Insert COUNT copies of CHARACTER.
Interactively, prompt for CHARACTER.  You can specify CHARACTER in one
of these ways:

 - As its Unicode character name, e.g. \"LATIN SMALL LETTER A\".
   Completion is available; if you type a substring of the name
   preceded by an asterisk `*', Emacs shows all names which include
   that substring, not necessarily at the beginning of the name.

 - As a hexadecimal code point, e.g. 263A.  Note that code points in
   Emacs are equivalent to Unicode up to 10FFFF (which is the limit of
   the Unicode code space).

 - As a code point with a radix specified with #, e.g. #o21430
   (octal), #x2318 (hex), or #10r8984 (decimal).

If called interactively, COUNT is given by the prefix argument.  If
omitted or nil, it defaults to 1.

Inserting the character(s) relocates point and before-insertion
markers in the same ways as the function `insert'.

The optional third argument INHERIT, if non-nil, says to inherit text
properties from adjoining text, if those properties are sticky.  If
called interactively, INHERIT is t.  */)
  (Lisp_Object character, Lisp_Object count, Lisp_Object inherit)
{
  int i, stringlen;
  register ptrdiff_t n;
  int c, len;
  unsigned char str[MAX_MULTIBYTE_LENGTH];
  char string[4000];

  CHECK_CHARACTER (character);
  if (NILP (count))
    XSETFASTINT (count, 1);
  CHECK_NUMBER (count);
  c = XFASTINT (character);

  if (!NILP (BVAR (current_buffer, enable_multibyte_characters)))
    len = CHAR_STRING (c, str);
  else
    str[0] = c, len = 1;
  if (XINT (count) <= 0)
    return Qnil;
  if (BUF_BYTES_MAX / len < XINT (count))
    buffer_overflow ();
  n = XINT (count) * len;
  stringlen = min (n, sizeof string - sizeof string % len);
  for (i = 0; i < stringlen; i++)
    string[i] = str[i % len];
  while (n > stringlen)
    {
      maybe_quit ();
      if (!NILP (inherit))
	insert_and_inherit (string, stringlen);
      else
	insert (string, stringlen);
      n -= stringlen;
    }
  if (!NILP (inherit))
    insert_and_inherit (string, n);
  else
    insert (string, n);
  return Qnil;
}

DEFUN ("insert-byte", Finsert_byte, Sinsert_byte, 2, 3, 0,
       doc: /* Insert COUNT (second arg) copies of BYTE (first arg).
Both arguments are required.
BYTE is a number of the range 0..255.

If BYTE is 128..255 and the current buffer is multibyte, the
corresponding eight-bit character is inserted.

Point, and before-insertion markers, are relocated as in the function `insert'.
The optional third arg INHERIT, if non-nil, says to inherit text properties
from adjoining text, if those properties are sticky.  */)
  (Lisp_Object byte, Lisp_Object count, Lisp_Object inherit)
{
  CHECK_NUMBER (byte);
  if (XINT (byte) < 0 || XINT (byte) > 255)
    args_out_of_range_3 (byte, make_number (0), make_number (255));
  if (XINT (byte) >= 128
      && ! NILP (BVAR (current_buffer, enable_multibyte_characters)))
    XSETFASTINT (byte, BYTE8_TO_CHAR (XINT (byte)));
  return Finsert_char (byte, count, inherit);
}


/* Making strings from buffer contents.  */

/* Return a Lisp_String containing the text of the current buffer from
   START to END.  If text properties are in use and the current buffer
   has properties in the range specified, the resulting string will also
   have them, if PROPS is true.

   We don't want to use plain old make_string here, because it calls
   make_uninit_string, which can cause the buffer arena to be
   compacted.  make_string has no way of knowing that the data has
   been moved, and thus copies the wrong data into the string.  This
   doesn't effect most of the other users of make_string, so it should
   be left as is.  But we should use this function when conjuring
   buffer substrings.  */

Lisp_Object
make_buffer_string (ptrdiff_t start, ptrdiff_t end, bool props)
{
  ptrdiff_t start_byte = CHAR_TO_BYTE (start);
  ptrdiff_t end_byte = CHAR_TO_BYTE (end);

  return make_buffer_string_both (start, start_byte, end, end_byte, props);
}

/* Return a Lisp_String containing the text of the current buffer from
   START / START_BYTE to END / END_BYTE.

   If text properties are in use and the current buffer
   has properties in the range specified, the resulting string will also
   have them, if PROPS is true.

   We don't want to use plain old make_string here, because it calls
   make_uninit_string, which can cause the buffer arena to be
   compacted.  make_string has no way of knowing that the data has
   been moved, and thus copies the wrong data into the string.  This
   doesn't effect most of the other users of make_string, so it should
   be left as is.  But we should use this function when conjuring
   buffer substrings.  */

Lisp_Object
make_buffer_string_both (ptrdiff_t start, ptrdiff_t start_byte,
			 ptrdiff_t end, ptrdiff_t end_byte, bool props)
{
  Lisp_Object result, tem, tem1;
  ptrdiff_t beg0, end0, beg1, end1, size;

  if (start_byte < GPT_BYTE && GPT_BYTE < end_byte)
    {
      /* Two regions, before and after the gap.  */
      beg0 = start_byte;
      end0 = GPT_BYTE;
      beg1 = GPT_BYTE + GAP_SIZE - BEG_BYTE;
      end1 = end_byte + GAP_SIZE - BEG_BYTE;
    }
  else
    {
      /* The only region.  */
      beg0 = start_byte;
      end0 = end_byte;
      beg1 = -1;
      end1 = -1;
    }

  if (! NILP (BVAR (current_buffer, enable_multibyte_characters)))
    result = make_uninit_multibyte_string (end - start, end_byte - start_byte);
  else
    result = make_uninit_string (end - start);

  size = end0 - beg0;
  memcpy (SDATA (result), BYTE_POS_ADDR (beg0), size);
  if (beg1 != -1)
    memcpy (SDATA (result) + size, BEG_ADDR + beg1, end1 - beg1);

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
update_buffer_properties (ptrdiff_t start, ptrdiff_t end)
{
  /* If this buffer has some access functions,
     call them, specifying the range of the buffer being accessed.  */
  if (!NILP (Vbuffer_access_fontify_functions))
    {
      /* But don't call them if we can tell that the work
	 has already been done.  */
      if (!NILP (Vbuffer_access_fontified_property))
	{
	  Lisp_Object tem
	    = Ftext_property_any (make_number (start), make_number (end),
				  Vbuffer_access_fontified_property,
				  Qnil, Qnil);
	  if (NILP (tem))
	    return;
	}

      CALLN (Frun_hook_with_args, Qbuffer_access_fontify_functions,
	     make_number (start), make_number (end));
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
  (Lisp_Object start, Lisp_Object end)
{
  register ptrdiff_t b, e;

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
  (Lisp_Object start, Lisp_Object end)
{
  register ptrdiff_t b, e;

  validate_region (&start, &end);
  b = XINT (start);
  e = XINT (end);

  return make_buffer_string (b, e, 0);
}

DEFUN ("buffer-string", Fbuffer_string, Sbuffer_string, 0, 0, 0,
       doc: /* Return the contents of the current buffer as a string.
If narrowing is in effect, this function returns only the visible part
of the buffer.  */)
  (void)
{
  return make_buffer_string_both (BEGV, BEGV_BYTE, ZV, ZV_BYTE, 1);
}

DEFUN ("insert-buffer-substring", Finsert_buffer_substring, Sinsert_buffer_substring,
       1, 3, 0,
       doc: /* Insert before point a substring of the contents of BUFFER.
BUFFER may be a buffer or a buffer name.
Arguments START and END are character positions specifying the substring.
They default to the values of (point-min) and (point-max) in BUFFER.

Point and before-insertion markers move forward to end up after the
inserted text.
Any other markers at the point of insertion remain before the text.

If the current buffer is multibyte and BUFFER is unibyte, or vice
versa, strings are converted from unibyte to multibyte or vice versa
using `string-make-multibyte' or `string-make-unibyte', which see.  */)
  (Lisp_Object buffer, Lisp_Object start, Lisp_Object end)
{
  register EMACS_INT b, e, temp;
  register struct buffer *bp, *obuf;
  Lisp_Object buf;

  buf = Fget_buffer (buffer);
  if (NILP (buf))
    nsberror (buffer);
  bp = XBUFFER (buf);
  if (!BUFFER_LIVE_P (bp))
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
Return -N if first string is less after N-1 chars, +N if first string is
greater after N-1 chars, or 0 if strings match.
The first substring is in BUFFER1 from START1 to END1 and the second
is in BUFFER2 from START2 to END2.
All arguments may be nil.  If BUFFER1 or BUFFER2 is nil, the current
buffer is used.  If START1 or START2 is nil, the value of `point-min'
in the respective buffers is used.  If END1 or END2 is nil, the value
of `point-max' in the respective buffers is used.
The value of `case-fold-search' in the current buffer
determines whether case is significant or ignored.  */)
  (Lisp_Object buffer1, Lisp_Object start1, Lisp_Object end1, Lisp_Object buffer2, Lisp_Object start2, Lisp_Object end2)
{
  register EMACS_INT begp1, endp1, begp2, endp2, temp;
  register struct buffer *bp1, *bp2;
  register Lisp_Object trt
    = (!NILP (BVAR (current_buffer, case_fold_search))
       ? BVAR (current_buffer, case_canon_table) : Qnil);
  ptrdiff_t chars = 0;
  ptrdiff_t i1, i2, i1_byte, i2_byte;

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
      if (!BUFFER_LIVE_P (bp1))
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
      if (!BUFFER_LIVE_P (bp2))
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

      if (! NILP (BVAR (bp1, enable_multibyte_characters)))
	{
	  c1 = BUF_FETCH_MULTIBYTE_CHAR (bp1, i1_byte);
	  BUF_INC_POS (bp1, i1_byte);
	  i1++;
	}
      else
	{
	  c1 = BUF_FETCH_BYTE (bp1, i1);
	  MAKE_CHAR_MULTIBYTE (c1);
	  i1++;
	}

      if (! NILP (BVAR (bp2, enable_multibyte_characters)))
	{
	  c2 = BUF_FETCH_MULTIBYTE_CHAR (bp2, i2_byte);
	  BUF_INC_POS (bp2, i2_byte);
	  i2++;
	}
      else
	{
	  c2 = BUF_FETCH_BYTE (bp2, i2);
	  MAKE_CHAR_MULTIBYTE (c2);
	  i2++;
	}

      if (!NILP (trt))
	{
	  c1 = char_table_translate (trt, c1);
	  c2 = char_table_translate (trt, c2);
	}

      if (c1 != c2)
	return make_number (c1 < c2 ? -1 - chars : chars + 1);

      chars++;
      rarely_quit (chars);
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


/* Set up necessary definitions for diffseq.h; see comments in
   diffseq.h for explanation.  */

#undef ELEMENT
#undef EQUAL

#define XVECREF_YVECREF_EQUAL(ctx, xoff, yoff)  \
  buffer_chars_equal ((ctx), (xoff), (yoff))

#define OFFSET ptrdiff_t

#define EXTRA_CONTEXT_FIELDS                    \
  /* Buffers to compare.  */                    \
  struct buffer *buffer_a;                      \
  struct buffer *buffer_b;                      \
  /* Bit vectors recording for each character whether it was deleted
     or inserted.  */                           \
  unsigned char *deletions;                     \
  unsigned char *insertions;

#define NOTE_DELETE(ctx, xoff) set_bit ((ctx)->deletions, (xoff))
#define NOTE_INSERT(ctx, yoff) set_bit ((ctx)->insertions, (yoff))

struct context;
static void set_bit (unsigned char *, OFFSET);
static bool bit_is_set (const unsigned char *, OFFSET);
static bool buffer_chars_equal (struct context *, OFFSET, OFFSET);

#include "minmax.h"
#include "diffseq.h"

DEFUN ("replace-buffer-contents", Freplace_buffer_contents,
       Sreplace_buffer_contents, 1, 1, "bSource buffer: ",
       doc: /* Replace accessible portion of current buffer with that of SOURCE.
SOURCE can be a buffer or a string that names a buffer.
Interactively, prompt for SOURCE.
As far as possible the replacement is non-destructive, i.e. existing
buffer contents, markers, properties, and overlays in the current
buffer stay intact.  */)
  (Lisp_Object source)
{
  struct buffer *a = current_buffer;
  Lisp_Object source_buffer = Fget_buffer (source);
  if (NILP (source_buffer))
    nsberror (source);
  struct buffer *b = XBUFFER (source_buffer);
  if (! BUFFER_LIVE_P (b))
    error ("Selecting deleted buffer");
  if (a == b)
    error ("Cannot replace a buffer with itself");

  ptrdiff_t min_a = BEGV;
  ptrdiff_t min_b = BUF_BEGV (b);
  ptrdiff_t size_a = ZV - min_a;
  ptrdiff_t size_b = BUF_ZV (b) - min_b;
  eassume (size_a >= 0);
  eassume (size_b >= 0);
  bool a_empty = size_a == 0;
  bool b_empty = size_b == 0;

  /* Handle trivial cases where at least one accessible portion is
     empty.  */

  if (a_empty && b_empty)
    return Qnil;

  if (a_empty)
    return Finsert_buffer_substring (source, Qnil, Qnil);

  if (b_empty)
    {
      del_range_both (BEGV, BEGV_BYTE, ZV, ZV_BYTE, true);
      return Qnil;
    }

  /* FIXME: It is not documented how to initialize the contents of the
     context structure.  This code cargo-cults from the existing
     caller in src/analyze.c of GNU Diffutils, which appears to
     work.  */

  ptrdiff_t diags = size_a + size_b + 3;
  ptrdiff_t *buffer;
  USE_SAFE_ALLOCA;
  SAFE_NALLOCA (buffer, 2, diags);
  /* Micro-optimization: Casting to size_t generates much better
     code.  */
  ptrdiff_t del_bytes = (size_t) size_a / CHAR_BIT + 1;
  ptrdiff_t ins_bytes = (size_t) size_b / CHAR_BIT + 1;
  struct context ctx = {
    .buffer_a = a,
    .buffer_b = b,
    .deletions = SAFE_ALLOCA (del_bytes),
    .insertions = SAFE_ALLOCA (ins_bytes),
    .fdiag = buffer + size_b + 1,
    .bdiag = buffer + diags + size_b + 1,
    /* FIXME: Find a good number for .too_expensive.  */
    .too_expensive = 1000000,
  };
  memclear (ctx.deletions, del_bytes);
  memclear (ctx.insertions, ins_bytes);
  /* compareseq requires indices to be zero-based.  We add BEGV back
     later.  */
  bool early_abort = compareseq (0, size_a, 0, size_b, false, &ctx);
  /* Since we didn’t define EARLY_ABORT, we should never abort
     early.  */
  eassert (! early_abort);
  SAFE_FREE ();

  Fundo_boundary ();
  ptrdiff_t count = SPECPDL_INDEX ();
  record_unwind_protect (save_excursion_restore, save_excursion_save ());

  ptrdiff_t i = size_a;
  ptrdiff_t j = size_b;
  /* Walk backwards through the lists of changes.  This was also
     cargo-culted from src/analyze.c in GNU Diffutils.  Because we
     walk backwards, we don’t have to keep the positions in sync.  */
  while (i >= 0 || j >= 0)
    {
      /* Check whether there is a change (insertion or deletion)
         before the current position.  */
      if ((i > 0 && bit_is_set (ctx.deletions, i - 1)) ||
          (j > 0 && bit_is_set (ctx.insertions, j - 1)))
	{
          ptrdiff_t end_a = min_a + i;
          ptrdiff_t end_b = min_b + j;
          /* Find the beginning of the current change run.  */
	  while (i > 0 && bit_is_set (ctx.deletions, i - 1))
            --i;
	  while (j > 0 && bit_is_set (ctx.insertions, j - 1))
            --j;
          ptrdiff_t beg_a = min_a + i;
          ptrdiff_t beg_b = min_b + j;
          eassert (beg_a >= BEGV);
          eassert (beg_b >= BUF_BEGV (b));
          eassert (beg_a <= end_a);
          eassert (beg_b <= end_b);
          eassert (end_a <= ZV);
          eassert (end_b <= BUF_ZV (b));
          eassert (beg_a < end_a || beg_b < end_b);
          if (beg_a < end_a)
            del_range (beg_a, end_a);
          if (beg_b < end_b)
            {
              SET_PT (beg_a);
              Finsert_buffer_substring (source, make_natnum (beg_b),
                                        make_natnum (end_b));
            }
	}
      --i;
      --j;
    }

  return unbind_to (count, Qnil);
}

static void
set_bit (unsigned char *a, ptrdiff_t i)
{
  eassert (i >= 0);
  /* Micro-optimization: Casting to size_t generates much better
     code.  */
  size_t j = i;
  a[j / CHAR_BIT] |= (1 << (j % CHAR_BIT));
}

static bool
bit_is_set (const unsigned char *a, ptrdiff_t i)
{
  eassert (i >= 0);
  /* Micro-optimization: Casting to size_t generates much better
     code.  */
  size_t j = i;
  return a[j / CHAR_BIT] & (1 << (j % CHAR_BIT));
}

/* Return true if the characters at position POS_A of buffer
   CTX->buffer_a and at position POS_B of buffer CTX->buffer_b are
   equal.  POS_A and POS_B are zero-based.  Text properties are
   ignored.  */

static bool
buffer_chars_equal (struct context *ctx,
                    ptrdiff_t pos_a, ptrdiff_t pos_b)
{
  eassert (pos_a >= 0);
  pos_a += BUF_BEGV (ctx->buffer_a);
  eassert (pos_a >= BUF_BEGV (ctx->buffer_a));
  eassert (pos_a < BUF_ZV (ctx->buffer_a));

  eassert (pos_b >= 0);
  pos_b += BUF_BEGV (ctx->buffer_b);
  eassert (pos_b >= BUF_BEGV (ctx->buffer_b));
  eassert (pos_b < BUF_ZV (ctx->buffer_b));

  return BUF_FETCH_CHAR_AS_MULTIBYTE (ctx->buffer_a, pos_a)
    == BUF_FETCH_CHAR_AS_MULTIBYTE (ctx->buffer_b, pos_b);
}


static void
subst_char_in_region_unwind (Lisp_Object arg)
{
  bset_undo_list (current_buffer, arg);
}

static void
subst_char_in_region_unwind_1 (Lisp_Object arg)
{
  bset_filename (current_buffer, arg);
}

DEFUN ("subst-char-in-region", Fsubst_char_in_region,
       Ssubst_char_in_region, 4, 5, 0,
       doc: /* From START to END, replace FROMCHAR with TOCHAR each time it occurs.
If optional arg NOUNDO is non-nil, don't record this change for undo
and don't mark the buffer as really changed.
Both characters must have the same length of multi-byte form.  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object fromchar, Lisp_Object tochar, Lisp_Object noundo)
{
  register ptrdiff_t pos, pos_byte, stop, i, len, end_byte;
  /* Keep track of the first change in the buffer:
     if 0 we haven't found it yet.
     if < 0 we've found it and we've run the before-change-function.
     if > 0 we've actually performed it and the value is its position.  */
  ptrdiff_t changed = 0;
  unsigned char fromstr[MAX_MULTIBYTE_LENGTH], tostr[MAX_MULTIBYTE_LENGTH];
  unsigned char *p;
  ptrdiff_t count = SPECPDL_INDEX ();
#define COMBINING_NO	 0
#define COMBINING_BEFORE 1
#define COMBINING_AFTER  2
#define COMBINING_BOTH (COMBINING_BEFORE | COMBINING_AFTER)
  int maybe_byte_combining = COMBINING_NO;
  ptrdiff_t last_changed = 0;
  bool multibyte_p
    = !NILP (BVAR (current_buffer, enable_multibyte_characters));
  int fromc, toc;

 restart:

  validate_region (&start, &end);
  CHECK_CHARACTER (fromchar);
  CHECK_CHARACTER (tochar);
  fromc = XFASTINT (fromchar);
  toc = XFASTINT (tochar);

  if (multibyte_p)
    {
      len = CHAR_STRING (fromc, fromstr);
      if (CHAR_STRING (toc, tostr) != len)
	error ("Characters in `subst-char-in-region' have different byte-lengths");
      if (!ASCII_CHAR_P (*tostr))
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
      fromstr[0] = fromc;
      tostr[0] = toc;
    }

  pos = XINT (start);
  pos_byte = CHAR_TO_BYTE (pos);
  stop = CHAR_TO_BYTE (XINT (end));
  end_byte = stop;

  /* If we don't want undo, turn off putting stuff on the list.
     That's faster than getting rid of things,
     and it prevents even the entry for a first change.
     Also inhibit locking the file.  */
  if (!changed && !NILP (noundo))
    {
      record_unwind_protect (subst_char_in_region_unwind,
			     BVAR (current_buffer, undo_list));
      bset_undo_list (current_buffer, Qt);
      /* Don't do file-locking.  */
      record_unwind_protect (subst_char_in_region_unwind_1,
			     BVAR (current_buffer, filename));
      bset_filename (current_buffer, Qnil);
    }

  if (pos_byte < GPT_BYTE)
    stop = min (stop, GPT_BYTE);
  while (1)
    {
      ptrdiff_t pos_byte_next = pos_byte;

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
	  if (changed < 0)
	    /* We've already seen this and run the before-change-function;
	       this time we only need to record the actual position. */
	    changed = pos;
	  else if (!changed)
	    {
	      changed = -1;
	      modify_text (pos, XINT (end));

	      if (! NILP (noundo))
		{
		  if (MODIFF - 1 == SAVE_MODIFF)
		    SAVE_MODIFF++;
		  if (MODIFF - 1 == BUF_AUTOSAVE_MODIFF (current_buffer))
		    BUF_AUTOSAVE_MODIFF (current_buffer)++;
		}

	      /* The before-change-function may have moved the gap
		 or even modified the buffer so we should start over. */
	      goto restart;
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
			 && ! ASCII_CHAR_P (FETCH_BYTE (pos_byte - 1))))))
	    {
	      Lisp_Object tem, string;

	      tem = BVAR (current_buffer, undo_list);

	      /* Make a multibyte string containing this single character.  */
	      string = make_multibyte_string ((char *) tostr, 1, len);
	      /* replace_range is less efficient, because it moves the gap,
		 but it handles combining correctly.  */
	      replace_range (pos, pos + 1, string,
			     0, 0, 1, 0);
	      pos_byte_next = CHAR_TO_BYTE (pos);
	      if (pos_byte_next > pos_byte)
		/* Before combining happened.  We should not increment
		   POS.  So, to cancel the later increment of POS,
		   decrease it now.  */
		pos--;
	      else
		INC_POS (pos_byte_next);

	      if (! NILP (noundo))
		bset_undo_list (current_buffer, tem);
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

  if (changed > 0)
    {
      signal_after_change (changed,
			   last_changed - changed, last_changed - changed);
      update_compositions (changed, last_changed, CHECK_ALL);
    }

  unbind_to (count, Qnil);
  return Qnil;
}


static Lisp_Object check_translation (ptrdiff_t, ptrdiff_t, ptrdiff_t,
				      Lisp_Object);

/* Helper function for Ftranslate_region_internal.

   Check if a character sequence at POS (POS_BYTE) matches an element
   of VAL.  VAL is a list (([FROM-CHAR ...] . TO) ...).  If a matching
   element is found, return it.  Otherwise return Qnil.  */

static Lisp_Object
check_translation (ptrdiff_t pos, ptrdiff_t pos_byte, ptrdiff_t end,
		   Lisp_Object val)
{
  int initial_buf[16];
  int *buf = initial_buf;
  ptrdiff_t buf_size = ARRAYELTS (initial_buf);
  int *bufalloc = 0;
  ptrdiff_t buf_used = 0;
  Lisp_Object result = Qnil;

  for (; CONSP (val); val = XCDR (val))
    {
      Lisp_Object elt;
      ptrdiff_t len, i;

      elt = XCAR (val);
      if (! CONSP (elt))
	continue;
      elt = XCAR (elt);
      if (! VECTORP (elt))
	continue;
      len = ASIZE (elt);
      if (len <= end - pos)
	{
	  for (i = 0; i < len; i++)
	    {
	      if (buf_used <= i)
		{
		  unsigned char *p = BYTE_POS_ADDR (pos_byte);
		  int len1;

		  if (buf_used == buf_size)
		    {
		      bufalloc = xpalloc (bufalloc, &buf_size, 1, -1,
					  sizeof *bufalloc);
		      if (buf == initial_buf)
			memcpy (bufalloc, buf, sizeof initial_buf);
		      buf = bufalloc;
		    }
		  buf[buf_used++] = STRING_CHAR_AND_LENGTH (p, len1);
		  pos_byte += len1;
		}
	      if (XINT (AREF (elt, i)) != buf[i])
		break;
	    }
	  if (i == len)
	    {
	      result = XCAR (val);
	      break;
	    }
	}
    }

  xfree (bufalloc);
  return result;
}


DEFUN ("translate-region-internal", Ftranslate_region_internal,
       Stranslate_region_internal, 3, 3, 0,
       doc: /* Internal use only.
From START to END, translate characters according to TABLE.
TABLE is a string or a char-table; the Nth character in it is the
mapping for the character with code N.
It returns the number of characters changed.  */)
  (Lisp_Object start, Lisp_Object end, register Lisp_Object table)
{
  register unsigned char *tt;	/* Trans table. */
  register int nc;		/* New character. */
  int cnt;			/* Number of changes made. */
  ptrdiff_t size;		/* Size of translate table. */
  ptrdiff_t pos, pos_byte, end_pos;
  bool multibyte = !NILP (BVAR (current_buffer, enable_multibyte_characters));
  bool string_multibyte UNINIT;

  validate_region (&start, &end);
  if (CHAR_TABLE_P (table))
    {
      if (! EQ (XCHAR_TABLE (table)->purpose, Qtranslation_table))
	error ("Not a translation table");
      size = MAX_CHAR;
      tt = NULL;
    }
  else
    {
      CHECK_STRING (table);

      if (! multibyte && (SCHARS (table) < SBYTES (table)))
	table = string_make_unibyte (table);
      string_multibyte = SCHARS (table) < SBYTES (table);
      size = SBYTES (table);
      tt = SDATA (table);
    }

  pos = XINT (start);
  pos_byte = CHAR_TO_BYTE (pos);
  end_pos = XINT (end);
  modify_text (pos, end_pos);

  cnt = 0;
  for (; pos < end_pos; )
    {
      unsigned char *p = BYTE_POS_ADDR (pos_byte);
      unsigned char *str UNINIT;
      unsigned char buf[MAX_MULTIBYTE_LENGTH];
      int len, str_len;
      int oc;
      Lisp_Object val;

      if (multibyte)
	oc = STRING_CHAR_AND_LENGTH (p, len);
      else
	oc = *p, len = 1;
      if (oc < size)
	{
	  if (tt)
	    {
	      /* Reload as signal_after_change in last iteration may GC.  */
	      tt = SDATA (table);
	      if (string_multibyte)
		{
		  str = tt + string_char_to_byte (table, oc);
		  nc = STRING_CHAR_AND_LENGTH (str, str_len);
		}
	      else
		{
		  nc = tt[oc];
		  if (! ASCII_CHAR_P (nc) && multibyte)
		    {
		      str_len = BYTE8_STRING (nc, buf);
		      str = buf;
		    }
		  else
		    {
		      str_len = 1;
		      str = tt + oc;
		    }
		}
	    }
	  else
	    {
	      nc = oc;
	      val = CHAR_TABLE_REF (table, oc);
	      if (CHARACTERP (val))
		{
		  nc = XFASTINT (val);
		  str_len = CHAR_STRING (nc, buf);
		  str = buf;
		}
	      else if (VECTORP (val) || (CONSP (val)))
		{
		  /* VAL is [TO_CHAR ...] or (([FROM-CHAR ...] .  TO) ...)
		     where TO is TO-CHAR or [TO-CHAR ...].  */
		  nc = -1;
		}
	    }

	  if (nc != oc && nc >= 0)
	    {
	      /* Simple one char to one char translation.  */
	      if (len != str_len)
		{
		  Lisp_Object string;

		  /* This is less efficient, because it moves the gap,
		     but it should handle multibyte characters correctly.  */
		  string = make_multibyte_string ((char *) str, 1, str_len);
		  replace_range (pos, pos + 1, string, 1, 0, 1, 0);
		  len = str_len;
		}
	      else
		{
		  record_change (pos, 1);
		  while (str_len-- > 0)
		    *p++ = *str++;
		  signal_after_change (pos, 1, 1);
		  update_compositions (pos, pos + 1, CHECK_BORDER);
		}
	      ++cnt;
	    }
	  else if (nc < 0)
	    {
	      Lisp_Object string;

	      if (CONSP (val))
		{
		  val = check_translation (pos, pos_byte, end_pos, val);
		  if (NILP (val))
		    {
		      pos_byte += len;
		      pos++;
		      continue;
		    }
		  /* VAL is ([FROM-CHAR ...] . TO).  */
		  len = ASIZE (XCAR (val));
		  val = XCDR (val);
		}
	      else
		len = 1;

	      if (VECTORP (val))
		{
		  string = Fconcat (1, &val);
		}
	      else
		{
		  string = Fmake_string (make_number (1), val, Qnil);
		}
	      replace_range (pos, pos + len, string, 1, 0, 1, 0);
	      pos_byte += SBYTES (string);
	      pos += SCHARS (string);
	      cnt += SCHARS (string);
	      end_pos += SCHARS (string) - len;
	      continue;
	    }
	}
      pos_byte += len;
      pos++;
    }

  return make_number (cnt);
}

DEFUN ("delete-region", Fdelete_region, Sdelete_region, 2, 2, "r",
       doc: /* Delete the text between START and END.
If called interactively, delete the region between point and mark.
This command deletes buffer text without modifying the kill ring.  */)
  (Lisp_Object start, Lisp_Object end)
{
  validate_region (&start, &end);
  del_range (XINT (start), XINT (end));
  return Qnil;
}

DEFUN ("delete-and-extract-region", Fdelete_and_extract_region,
       Sdelete_and_extract_region, 2, 2, 0,
       doc: /* Delete the text between START and END and return it.  */)
  (Lisp_Object start, Lisp_Object end)
{
  validate_region (&start, &end);
  if (XINT (start) == XINT (end))
    return empty_unibyte_string;
  return del_range_1 (XINT (start), XINT (end), 1, 1);
}

DEFUN ("widen", Fwiden, Swiden, 0, 0, "",
       doc: /* Remove restrictions (narrowing) from current buffer.
This allows the buffer's full text to be seen and edited.  */)
  (void)
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
  (register Lisp_Object start, Lisp_Object end)
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
save_restriction_save (void)
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

      beg = build_marker (current_buffer, BEGV, BEGV_BYTE);
      end = build_marker (current_buffer, ZV, ZV_BYTE);

      /* END must move forward if text is inserted at its exact location.  */
      XMARKER (end)->insertion_type = 1;

      return Fcons (beg, end);
    }
}

void
save_restriction_restore (Lisp_Object data)
{
  struct buffer *cur = NULL;
  struct buffer *buf = (CONSP (data)
			? XMARKER (XCAR (data))->buffer
			: XBUFFER (data));

  if (buf && buf != current_buffer && !NILP (BVAR (buf, pt_marker)))
    { /* If `buf' uses markers to keep track of PT, BEGV, and ZV (as
	 is the case if it is or has an indirect buffer), then make
	 sure it is current before we update BEGV, so
	 set_buffer_internal takes care of managing those markers.  */
      cur = current_buffer;
      set_buffer_internal (buf);
    }

  if (CONSP (data))
    /* A pair of marks bounding a saved restriction.  */
    {
      struct Lisp_Marker *beg = XMARKER (XCAR (data));
      struct Lisp_Marker *end = XMARKER (XCDR (data));
      eassert (buf == end->buffer);

      if (buf /* Verify marker still points to a buffer.  */
	  && (beg->charpos != BUF_BEGV (buf) || end->charpos != BUF_ZV (buf)))
	/* The restriction has changed from the saved one, so restore
	   the saved restriction.  */
	{
	  ptrdiff_t pt = BUF_PT (buf);

	  SET_BUF_BEGV_BOTH (buf, beg->charpos, beg->bytepos);
	  SET_BUF_ZV_BOTH (buf, end->charpos, end->bytepos);

	  if (pt < beg->charpos || pt > end->charpos)
	    /* The point is outside the new visible range, move it inside. */
	    SET_BUF_PT_BOTH (buf,
			     clip_to_bounds (beg->charpos, pt, end->charpos),
			     clip_to_bounds (beg->bytepos, BUF_PT_BYTE (buf),
					     end->bytepos));

	  buf->clip_changed = 1; /* Remember that the narrowing changed. */
	}
      /* Detach the markers, and free the cons instead of waiting for GC.  */
      detach_marker (XCAR (data));
      detach_marker (XCDR (data));
      free_cons (XCONS (data));
    }
  else
    /* A buffer, which means that there was no old restriction.  */
    {
      if (buf /* Verify marker still points to a buffer.  */
	  && (BUF_BEGV (buf) != BUF_BEG (buf) || BUF_ZV (buf) != BUF_Z (buf)))
	/* The buffer has been narrowed, get rid of the narrowing.  */
	{
	  SET_BUF_BEGV_BOTH (buf, BUF_BEG (buf), BUF_BEG_BYTE (buf));
	  SET_BUF_ZV_BOTH (buf, BUF_Z (buf), BUF_Z_BYTE (buf));

	  buf->clip_changed = 1; /* Remember that the narrowing changed. */
	}
    }

  /* Changing the buffer bounds invalidates any recorded current column.  */
  invalidate_current_column ();

  if (cur)
    set_buffer_internal (cur);
}

DEFUN ("save-restriction", Fsave_restriction, Ssave_restriction, 0, UNEVALLED, 0,
       doc: /* Execute BODY, saving and restoring current buffer's restrictions.
The buffer's restrictions make parts of the beginning and end invisible.
\(They are set up with `narrow-to-region' and eliminated with `widen'.)
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
  (Lisp_Object body)
{
  register Lisp_Object val;
  ptrdiff_t count = SPECPDL_INDEX ();

  record_unwind_protect (save_restriction_restore, save_restriction_save ());
  val = Fprogn (body);
  return unbind_to (count, val);
}

DEFUN ("message", Fmessage, Smessage, 1, MANY, 0,
       doc: /* Display a message at the bottom of the screen.
The message also goes into the `*Messages*' buffer, if `message-log-max'
is non-nil.  (In keyboard macros, that's all it does.)
Return the message.

In batch mode, the message is printed to the standard error stream,
followed by a newline.

The first argument is a format control string, and the rest are data
to be formatted under control of the string.  Percent sign (%), grave
accent (\\=`) and apostrophe (\\=') are special in the format; see
`format-message' for details.  To display STRING without special
treatment, use (message "%s" STRING).

If the first argument is nil or the empty string, the function clears
any existing message; this lets the minibuffer contents show.  See
also `current-message'.

usage: (message FORMAT-STRING &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  if (NILP (args[0])
      || (STRINGP (args[0])
	  && SBYTES (args[0]) == 0))
    {
      message1 (0);
      return args[0];
    }
  else
    {
      Lisp_Object val = Fformat_message (nargs, args);
      message3 (val);
      return val;
    }
}

DEFUN ("message-box", Fmessage_box, Smessage_box, 1, MANY, 0,
       doc: /* Display a message, in a dialog box if possible.
If a dialog box is not available, use the echo area.
The first argument is a format control string, and the rest are data
to be formatted under control of the string.  See `format-message' for
details.

If the first argument is nil or the empty string, clear any existing
message; let the minibuffer contents show.

usage: (message-box FORMAT-STRING &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  if (NILP (args[0]))
    {
      message1 (0);
      return Qnil;
    }
  else
    {
      Lisp_Object val = Fformat_message (nargs, args);
      Lisp_Object pane, menu;

      pane = list1 (Fcons (build_string ("OK"), Qt));
      menu = Fcons (val, pane);
      Fx_popup_dialog (Qt, menu, Qt);
      return val;
    }
}

DEFUN ("message-or-box", Fmessage_or_box, Smessage_or_box, 1, MANY, 0,
       doc: /* Display a message in a dialog box or in the echo area.
If this command was invoked with the mouse, use a dialog box if
`use-dialog-box' is non-nil.
Otherwise, use the echo area.
The first argument is a format control string, and the rest are data
to be formatted under control of the string.  See `format-message' for
details.

If the first argument is nil or the empty string, clear any existing
message; let the minibuffer contents show.

usage: (message-or-box FORMAT-STRING &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  if ((NILP (last_nonmenu_event) || CONSP (last_nonmenu_event))
      && use_dialog_box)
    return Fmessage_box (nargs, args);
  return Fmessage (nargs, args);
}

DEFUN ("current-message", Fcurrent_message, Scurrent_message, 0, 0, 0,
       doc: /* Return the string currently displayed in the echo area, or nil if none.  */)
  (void)
{
  return current_message ();
}


DEFUN ("propertize", Fpropertize, Spropertize, 1, MANY, 0,
       doc: /* Return a copy of STRING with text properties added.
First argument is the string to copy.
Remaining arguments form a sequence of PROPERTY VALUE pairs for text
properties to add to the result.
usage: (propertize STRING &rest PROPERTIES)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object properties, string;
  ptrdiff_t i;

  /* Number of args must be odd.  */
  if ((nargs & 1) == 0)
    error ("Wrong number of arguments");

  properties = string = Qnil;

  /* First argument must be a string.  */
  CHECK_STRING (args[0]);
  string = Fcopy_sequence (args[0]);

  for (i = 1; i < nargs; i += 2)
    properties = Fcons (args[i], Fcons (args[i + 1], properties));

  Fadd_text_properties (make_number (0),
			make_number (SCHARS (string)),
			properties, string);
  return string;
}

/* Convert the prefix of STR from ASCII decimal digits to a number.
   Set *STR_END to the address of the first non-digit.  Return the
   number, or PTRDIFF_MAX on overflow.  Return 0 if there is no number.
   This is like strtol for ptrdiff_t and base 10 and C locale,
   except without negative numbers or errno.  */

static ptrdiff_t
str2num (char *str, char **str_end)
{
  ptrdiff_t n = 0;
  for (; c_isdigit (*str); str++)
    if (INT_MULTIPLY_WRAPV (n, 10, &n) || INT_ADD_WRAPV (n, *str - '0', &n))
      n = PTRDIFF_MAX;
  *str_end = str;
  return n;
}

DEFUN ("format", Fformat, Sformat, 1, MANY, 0,
       doc: /* Format a string out of a format-string and arguments.
The first argument is a format control string.
The other arguments are substituted into it to make the result, a string.

The format control string may contain %-sequences meaning to substitute
the next available argument, or the argument explicitly specified:

%s means print a string argument.  Actually, prints any object, with `princ'.
%d means print as signed number in decimal.
%o means print as unsigned number in octal, %x as unsigned number in hex.
%X is like %x, but uses upper case.
%e means print a number in exponential notation.
%f means print a number in decimal-point notation.
%g means print a number in exponential notation if the exponent would be
   less than -4 or greater than or equal to the precision (default: 6);
   otherwise it prints in decimal-point notation.
%c means print a number as a single character.
%S means print any object as an s-expression (using `prin1').

The argument used for %d, %o, %x, %e, %f, %g or %c must be a number.
Use %% to put a single % into the output.

A %-sequence other than %% may contain optional field number, flag,
width, and precision specifiers, as follows:

  %<field><flags><width><precision>character

where field is [0-9]+ followed by a literal dollar "$", flags is
[+ #-0]+, width is [0-9]+, and precision is a literal period "."
followed by [0-9]+.

If a %-sequence is numbered with a field with positive value N, the
Nth argument is substituted instead of the next one.  A format can
contain either numbered or unnumbered %-sequences but not both, except
that %% can be mixed with numbered %-sequences.

The + flag character inserts a + before any positive number, while a
space inserts a space before any positive number; these flags only
affect %d, %e, %f, and %g sequences, and the + flag takes precedence.
The - and 0 flags affect the width specifier, as described below.

The # flag means to use an alternate display form for %o, %x, %X, %e,
%f, and %g sequences: for %o, it ensures that the result begins with
\"0\"; for %x and %X, it prefixes the result with \"0x\" or \"0X\";
for %e and %f, it causes a decimal point to be included even if the
precision is zero; for %g, it causes a decimal point to be
included even if the precision is zero, and also forces trailing
zeros after the decimal point to be left in place.

The width specifier supplies a lower limit for the length of the
printed representation.  The padding, if any, normally goes on the
left, but it goes on the right if the - flag is present.  The padding
character is normally a space, but it is 0 if the 0 flag is present.
The 0 flag is ignored if the - flag is present, or the format sequence
is something other than %d, %e, %f, and %g.

For %e and %f sequences, the number after the "." in the precision
specifier says how many decimal places to show; if zero, the decimal
point itself is omitted.  For %g, the precision specifies how many
significant digits to print; zero or omitted are treated as 1.
For %s and %S, the precision specifier truncates the string to the
given width.

Text properties, if any, are copied from the format-string to the
produced text.

usage: (format STRING &rest OBJECTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return styled_format (nargs, args, false);
}

DEFUN ("format-message", Fformat_message, Sformat_message, 1, MANY, 0,
       doc: /* Format a string out of a format-string and arguments.
The first argument is a format control string.
The other arguments are substituted into it to make the result, a string.

This acts like `format', except it also replaces each grave accent (\\=`)
by a left quote, and each apostrophe (\\=') by a right quote.  The left
and right quote replacement characters are specified by
`text-quoting-style'.

usage: (format-message STRING &rest OBJECTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return styled_format (nargs, args, true);
}

/* Implement ‘format-message’ if MESSAGE is true, ‘format’ otherwise.  */

static Lisp_Object
styled_format (ptrdiff_t nargs, Lisp_Object *args, bool message)
{
  ptrdiff_t n;		/* The number of the next arg to substitute.  */
  char initial_buffer[4000];
  char *buf = initial_buffer;
  ptrdiff_t bufsize = sizeof initial_buffer;
  ptrdiff_t max_bufsize = STRING_BYTES_BOUND + 1;
  char *p;
  ptrdiff_t buf_save_value_index UNINIT;
  char *format, *end;
  ptrdiff_t nchars;
  /* When we make a multibyte string, we must pay attention to the
     byte combining problem, i.e., a byte may be combined with a
     multibyte character of the previous string.  This flag tells if we
     must consider such a situation or not.  */
  bool maybe_combine_byte;
  Lisp_Object val;
  bool arg_intervals = false;
  USE_SAFE_ALLOCA;
  sa_avail -= sizeof initial_buffer;

  /* Information recorded for each format spec.  */
  struct info
  {
    /* The corresponding argument, converted to string if conversion
       was needed.  */
    Lisp_Object argument;

    /* The start and end bytepos in the output string.  */
    ptrdiff_t start, end;

    /* Whether the argument is a string with intervals.  */
    bool_bf intervals : 1;
  } *info;

  CHECK_STRING (args[0]);
  char *format_start = SSDATA (args[0]);
  bool multibyte_format = STRING_MULTIBYTE (args[0]);
  ptrdiff_t formatlen = SBYTES (args[0]);

  /* Upper bound on number of format specs.  Each uses at least 2 chars.  */
  ptrdiff_t nspec_bound = SCHARS (args[0]) >> 1;

  /* Allocate the info and discarded tables.  */
  ptrdiff_t info_size, alloca_size;
  if (INT_MULTIPLY_WRAPV (nspec_bound, sizeof *info, &info_size)
      || INT_ADD_WRAPV (formatlen, info_size, &alloca_size)
      || SIZE_MAX < alloca_size)
    memory_full (SIZE_MAX);
  info = SAFE_ALLOCA (alloca_size);
  /* discarded[I] is 1 if byte I of the format
     string was not copied into the output.
     It is 2 if byte I was not the first byte of its character.  */
  char *discarded = (char *) &info[nspec_bound];
  info = ptr_bounds_clip (info, info_size);
  discarded = ptr_bounds_clip (discarded, formatlen);
  memset (discarded, 0, formatlen);

  /* Try to determine whether the result should be multibyte.
     This is not always right; sometimes the result needs to be multibyte
     because of an object that we will pass through prin1.
     or because a grave accent or apostrophe is requoted,
     and in that case, we won't know it here.  */

  /* True if the output should be a multibyte string,
     which is true if any of the inputs is one.  */
  bool multibyte = multibyte_format;
  for (ptrdiff_t i = 1; !multibyte && i < nargs; i++)
    if (STRINGP (args[i]) && STRING_MULTIBYTE (args[i]))
      multibyte = true;

  int quoting_style = message ? text_quoting_style () : -1;

  ptrdiff_t ispec;
  ptrdiff_t nspec = 0;

  /* True if a string needs to be allocated to hold the result.  */
  bool new_result = false;

  /* If we start out planning a unibyte result,
     then discover it has to be multibyte, we jump back to retry.  */
 retry:

  p = buf;
  nchars = 0;

  /* N is the argument index, ISPEC is the specification index.  */
  n = 0;
  ispec = 0;

  /* Scan the format and store result in BUF.  */
  format = format_start;
  end = format + formatlen;
  maybe_combine_byte = false;

  while (format != end)
    {
      /* The values of N, ISPEC, and FORMAT when the loop body is
         entered.  */
      ptrdiff_t n0 = n;
      ptrdiff_t ispec0 = ispec;
      char *format0 = format;
      char const *convsrc = format;
      unsigned char format_char = *format++;

      /* Bytes needed to represent the output of this conversion.  */
      ptrdiff_t convbytes = 1;

      if (format_char == '%')
	{
	  /* General format specifications look like

	     '%' [field-number] [flags] [field-width] [precision] format

	     where

             field-number ::= [0-9]+ '$'
	     flags ::= [-+0# ]+
	     field-width ::= [0-9]+
	     precision ::= '.' [0-9]*

	     If present, a field-number specifies the argument number
	     to substitute.  Otherwise, the next argument is taken.

	     If a field-width is specified, it specifies to which width
	     the output should be padded with blanks, if the output
	     string is shorter than field-width.

	     If precision is specified, it specifies the number of
	     digits to print after the '.' for floats, or the max.
	     number of chars to print from a string.  */

	  ptrdiff_t num;
	  char *num_end;
	  if (c_isdigit (*format))
	    {
	      num = str2num (format, &num_end);
	      if (*num_end == '$')
		{
		  n = num - 1;
		  format = num_end + 1;
		}
	    }

	  bool minus_flag = false;
	  bool  plus_flag = false;
	  bool space_flag = false;
	  bool sharp_flag = false;
	  bool  zero_flag = false;

	  for (; ; format++)
	    {
	      switch (*format)
		{
		case '-': minus_flag = true; continue;
		case '+':  plus_flag = true; continue;
		case ' ': space_flag = true; continue;
		case '#': sharp_flag = true; continue;
		case '0':  zero_flag = true; continue;
		}
	      break;
	    }

	  /* Ignore flags when sprintf ignores them.  */
	  space_flag &= ! plus_flag;
	  zero_flag &= ! minus_flag;

	  num = str2num (format, &num_end);
	  if (max_bufsize <= num)
	    string_overflow ();
	  ptrdiff_t field_width = num;

	  bool precision_given = *num_end == '.';
	  ptrdiff_t precision = (precision_given
				 ? str2num (num_end + 1, &num_end)
				 : PTRDIFF_MAX);
	  format = num_end;

	  if (format == end)
	    error ("Format string ends in middle of format specifier");

	  char conversion = *format++;
	  memset (&discarded[format0 - format_start], 1,
		  format - format0 - (conversion == '%'));
	  if (conversion == '%')
	    {
	      new_result = true;
	      goto copy_char;
	    }

	  ++n;
	  if (! (n < nargs))
	    error ("Not enough arguments for format string");

	  struct info *spec = &info[ispec++];
	  if (nspec < ispec)
	    {
	      spec->argument = args[n];
	      spec->intervals = false;
	      nspec = ispec;
	    }
	  Lisp_Object arg = spec->argument;

	  /* For 'S', prin1 the argument, and then treat like 's'.
	     For 's', princ any argument that is not a string or
	     symbol.  But don't do this conversion twice, which might
	     happen after retrying.  */
	  if ((conversion == 'S'
	       || (conversion == 's'
		   && ! STRINGP (arg) && ! SYMBOLP (arg))))
	    {
	      if (EQ (arg, args[n]))
		{
		  Lisp_Object noescape = conversion == 'S' ? Qnil : Qt;
		  spec->argument = arg = Fprin1_to_string (arg, noescape);
		  if (STRING_MULTIBYTE (arg) && ! multibyte)
		    {
		      multibyte = true;
		      goto retry;
		    }
		}
	      conversion = 's';
	    }
	  else if (conversion == 'c')
	    {
	      if (INTEGERP (arg) && ! ASCII_CHAR_P (XINT (arg)))
		{
		  if (!multibyte)
		    {
		      multibyte = true;
		      goto retry;
		    }
		  spec->argument = arg = Fchar_to_string (arg);
		}

	      if (!EQ (arg, args[n]))
		conversion = 's';
	      zero_flag = false;
	    }

	  if (SYMBOLP (arg))
	    {
	      spec->argument = arg = SYMBOL_NAME (arg);
	      if (STRING_MULTIBYTE (arg) && ! multibyte)
		{
		  multibyte = true;
		  goto retry;
		}
	    }

	  bool float_conversion
	    = conversion == 'e' || conversion == 'f' || conversion == 'g';

	  if (conversion == 's')
	    {
	      if (format == end && format - format_start == 2
		  && ! string_intervals (args[0]))
		{
		  val = arg;
		  goto return_val;
		}

	      /* handle case (precision[n] >= 0) */

	      ptrdiff_t prec = -1;
	      if (precision_given)
		prec = precision;

	      /* lisp_string_width ignores a precision of 0, but GNU
		 libc functions print 0 characters when the precision
		 is 0.  Imitate libc behavior here.  Changing
		 lisp_string_width is the right thing, and will be
		 done, but meanwhile we work with it. */

	      ptrdiff_t width, nbytes;
	      ptrdiff_t nchars_string;
	      if (prec == 0)
		width = nchars_string = nbytes = 0;
	      else
		{
		  ptrdiff_t nch, nby;
		  width = lisp_string_width (arg, prec, &nch, &nby);
		  if (prec < 0)
		    {
		      nchars_string = SCHARS (arg);
		      nbytes = SBYTES (arg);
		    }
		  else
		    {
		      nchars_string = nch;
		      nbytes = nby;
		    }
		}

	      convbytes = nbytes;
	      if (convbytes && multibyte && ! STRING_MULTIBYTE (arg))
		convbytes = count_size_as_multibyte (SDATA (arg), nbytes);

	      ptrdiff_t padding
		= width < field_width ? field_width - width : 0;

	      if (max_bufsize - padding <= convbytes)
		string_overflow ();
	      convbytes += padding;
	      if (convbytes <= buf + bufsize - p)
		{
		  if (! minus_flag)
		    {
		      memset (p, ' ', padding);
		      p += padding;
		      nchars += padding;
		    }
		  spec->start = nchars;

		  if (p > buf
		      && multibyte
		      && !ASCII_CHAR_P (*((unsigned char *) p - 1))
		      && STRING_MULTIBYTE (arg)
		      && !CHAR_HEAD_P (SREF (arg, 0)))
		    maybe_combine_byte = true;

		  p += copy_text (SDATA (arg), (unsigned char *) p,
				  nbytes,
				  STRING_MULTIBYTE (arg), multibyte);

		  nchars += nchars_string;

		  if (minus_flag)
		    {
		      memset (p, ' ', padding);
		      p += padding;
		      nchars += padding;
		    }
		  spec->end = nchars;

		  /* If this argument has text properties, record where
		     in the result string it appears.  */
		  if (string_intervals (arg))
		    spec->intervals = arg_intervals = true;

		  new_result = true;
		  continue;
		}
	    }
	  else if (! (conversion == 'c' || conversion == 'd'
		      || float_conversion || conversion == 'i'
		      || conversion == 'o' || conversion == 'x'
		      || conversion == 'X'))
	    error ("Invalid format operation %%%c",
		   STRING_CHAR ((unsigned char *) format - 1));
	  else if (! (INTEGERP (arg) || (FLOATP (arg) && conversion != 'c')))
	    error ("Format specifier doesn't match argument type");
	  else
	    {
	      enum
	      {
		/* Lower bound on the number of bits per
		   base-FLT_RADIX digit.  */
		DIG_BITS_LBOUND = FLT_RADIX < 16 ? 1 : 4,

		/* 1 if integers should be formatted as long doubles,
		   because they may be so large that there is a rounding
		   error when converting them to double, and long doubles
		   are wider than doubles.  */
		INT_AS_LDBL = (DIG_BITS_LBOUND * DBL_MANT_DIG < FIXNUM_BITS - 1
			       && DBL_MANT_DIG < LDBL_MANT_DIG),

		/* Maximum precision for a %f conversion such that the
		   trailing output digit might be nonzero.  Any precision
		   larger than this will not yield useful information.  */
		USEFUL_PRECISION_MAX =
		  ((1 - LDBL_MIN_EXP)
		   * (FLT_RADIX == 2 || FLT_RADIX == 10 ? 1
		      : FLT_RADIX == 16 ? 4
		      : -1)),

		/* Maximum number of bytes generated by any format, if
		   precision is no more than USEFUL_PRECISION_MAX.
		   On all practical hosts, %f is the worst case.  */
		SPRINTF_BUFSIZE =
		  sizeof "-." + (LDBL_MAX_10_EXP + 1) + USEFUL_PRECISION_MAX,

		/* Length of pM (that is, of pMd without the
		   trailing "d").  */
		pMlen = sizeof pMd - 2
	      };
	      verify (USEFUL_PRECISION_MAX > 0);

	      /* Avoid undefined behavior in underlying sprintf.  */
	      if (conversion == 'd' || conversion == 'i')
		sharp_flag = false;

	      /* Create the copy of the conversion specification, with
		 any width and precision removed, with ".*" inserted,
		 with "L" possibly inserted for floating-point formats,
		 and with pM inserted for integer formats.
		 At most two flags F can be specified at once.  */
	      char convspec[sizeof "%FF.*d" + max (INT_AS_LDBL, pMlen)];
	      char *f = convspec;
	      *f++ = '%';
	      /* MINUS_FLAG and ZERO_FLAG are dealt with later.  */
	      *f = '+'; f +=  plus_flag;
	      *f = ' '; f += space_flag;
	      *f = '#'; f += sharp_flag;
	      *f++ = '.';
	      *f++ = '*';
	      if (float_conversion)
		{
		  if (INT_AS_LDBL)
		    {
		      *f = 'L';
		      f += INTEGERP (arg);
		    }
		}
	      else if (conversion != 'c')
		{
		  memcpy (f, pMd, pMlen);
		  f += pMlen;
		  zero_flag &= ! precision_given;
		}
	      *f++ = conversion;
	      *f = '\0';

	      int prec = -1;
	      if (precision_given)
		prec = min (precision, USEFUL_PRECISION_MAX);

	      /* Use sprintf to format this number into sprintf_buf.  Omit
		 padding and excess precision, though, because sprintf limits
		 output length to INT_MAX.

		 There are four types of conversion: double, unsigned
		 char (passed as int), wide signed int, and wide
		 unsigned int.  Treat them separately because the
		 sprintf ABI is sensitive to which type is passed.  Be
		 careful about integer overflow, NaNs, infinities, and
		 conversions; for example, the min and max macros are
		 not suitable here.  */
	      char sprintf_buf[SPRINTF_BUFSIZE];
	      ptrdiff_t sprintf_bytes;
	      if (float_conversion)
		{
		  if (INT_AS_LDBL && INTEGERP (arg))
		    {
		      /* Although long double may have a rounding error if
			 DIG_BITS_LBOUND * LDBL_MANT_DIG < FIXNUM_BITS - 1,
			 it is more accurate than plain 'double'.  */
		      long double x = XINT (arg);
		      sprintf_bytes = sprintf (sprintf_buf, convspec, prec, x);
		    }
		  else
		    sprintf_bytes = sprintf (sprintf_buf, convspec, prec,
					     XFLOATINT (arg));
		}
	      else if (conversion == 'c')
		{
		  /* Don't use sprintf here, as it might mishandle prec.  */
		  sprintf_buf[0] = XINT (arg);
		  sprintf_bytes = prec != 0;
		  sprintf_buf[sprintf_bytes] = '\0';
		}
	      else if (conversion == 'd' || conversion == 'i')
		{
		  if (INTEGERP (arg))
		    {
		      printmax_t x = XINT (arg);
		      sprintf_bytes = sprintf (sprintf_buf, convspec, prec, x);
		    }
		  else
		    {
		      strcpy (f - pMlen - 1, "f");
		      double x = XFLOAT_DATA (arg);
		      sprintf_bytes = sprintf (sprintf_buf, convspec, 0, x);
		      char c0 = sprintf_buf[0];
		      bool signedp = ! ('0' <= c0 && c0 <= '9');
		      prec = min (precision, sprintf_bytes - signedp);
		    }
		}
	      else
		{
		  /* Don't sign-extend for octal or hex printing.  */
		  uprintmax_t x;
		  if (INTEGERP (arg))
		    x = XUINT (arg);
		  else
		    {
		      double d = XFLOAT_DATA (arg);
		      double uprintmax = TYPE_MAXIMUM (uprintmax_t);
		      if (! (0 <= d && d < uprintmax + 1))
			xsignal1 (Qoverflow_error, arg);
		      x = d;
		    }
		  sprintf_bytes = sprintf (sprintf_buf, convspec, prec, x);
		}

	      /* Now the length of the formatted item is known, except it omits
		 padding and excess precision.  Deal with excess precision
		 first.  This happens when the format specifies ridiculously
		 large precision, or when %d or %i formats a float that would
		 ordinarily need fewer digits than a specified precision.  */
	      ptrdiff_t excess_precision
		= precision_given ? precision - prec : 0;
	      ptrdiff_t leading_zeros = 0, trailing_zeros = 0;
	      if (excess_precision)
		{
		  if (float_conversion)
		    {
		      if ((conversion == 'g' && ! sharp_flag)
			  || ! ('0' <= sprintf_buf[sprintf_bytes - 1]
				&& sprintf_buf[sprintf_bytes - 1] <= '9'))
			excess_precision = 0;
		      else
			{
			  if (conversion == 'g')
			    {
			      char *dot = strchr (sprintf_buf, '.');
			      if (!dot)
				excess_precision = 0;
			    }
			}
		      trailing_zeros = excess_precision;
		    }
		  else
		    leading_zeros = excess_precision;
		}

	      /* Compute the total bytes needed for this item, including
		 excess precision and padding.  */
	      ptrdiff_t numwidth;
	      if (INT_ADD_WRAPV (sprintf_bytes, excess_precision, &numwidth))
		numwidth = PTRDIFF_MAX;
	      ptrdiff_t padding
		= numwidth < field_width ? field_width - numwidth : 0;
	      if (max_bufsize - sprintf_bytes <= excess_precision
		  || max_bufsize - padding <= numwidth)
		string_overflow ();
	      convbytes = numwidth + padding;

	      if (convbytes <= buf + bufsize - p)
		{
		  /* Copy the formatted item from sprintf_buf into buf,
		     inserting padding and excess-precision zeros.  */

                  char *src = sprintf_buf;
		  char src0 = src[0];
		  int exponent_bytes = 0;
		  bool signedp = src0 == '-' || src0 == '+' || src0 == ' ';
		  int prefix_bytes = (signedp
				      + ((src[signedp] == '0'
					  && (src[signedp + 1] == 'x'
					      || src[signedp + 1] == 'X'))
					 ? 2 : 0));
		  if (zero_flag)
		    {
		      unsigned char after_prefix = src[prefix_bytes];
		      if (0 <= char_hexdigit (after_prefix))
			{
			  leading_zeros += padding;
			  padding = 0;
			}
		    }

		  if (excess_precision
		      && (conversion == 'e' || conversion == 'g'))
		    {
		      char *e = strchr (src, 'e');
		      if (e)
			exponent_bytes = src + sprintf_bytes - e;
		    }

		  spec->start = nchars;
		  if (! minus_flag)
		    {
		      memset (p, ' ', padding);
		      p += padding;
		      nchars += padding;
		    }

		  memcpy (p, src, prefix_bytes);
		  p += prefix_bytes;
		  src += prefix_bytes;
		  memset (p, '0', leading_zeros);
		  p += leading_zeros;
		  int significand_bytes
		    = sprintf_bytes - prefix_bytes - exponent_bytes;
		  memcpy (p, src, significand_bytes);
                  p += significand_bytes;
		  src += significand_bytes;
		  memset (p, '0', trailing_zeros);
		  p += trailing_zeros;
		  memcpy (p, src, exponent_bytes);
		  p += exponent_bytes;

		  nchars += leading_zeros + sprintf_bytes + trailing_zeros;

		  if (minus_flag)
		    {
		      memset (p, ' ', padding);
		      p += padding;
		      nchars += padding;
		    }
		  spec->end = nchars;

		  new_result = true;
		  continue;
		}
	    }
	}
      else
	{
	  unsigned char str[MAX_MULTIBYTE_LENGTH];

	  if ((format_char == '`' || format_char == '\'')
	      && quoting_style == CURVE_QUOTING_STYLE)
	    {
	      if (! multibyte)
		{
		  multibyte = true;
		  goto retry;
		}
	      convsrc = format_char == '`' ? uLSQM : uRSQM;
	      convbytes = 3;
	      new_result = true;
	    }
	  else if (format_char == '`' && quoting_style == STRAIGHT_QUOTING_STYLE)
	    {
	      convsrc = "'";
	      new_result = true;
	    }
	  else
	    {
	      /* Copy a single character from format to buf.  */
	      if (multibyte_format)
		{
		  /* Copy a whole multibyte character.  */
		  if (p > buf
		      && !ASCII_CHAR_P (*((unsigned char *) p - 1))
		      && !CHAR_HEAD_P (format_char))
		    maybe_combine_byte = true;

		  while (! CHAR_HEAD_P (*format))
		    format++;

		  convbytes = format - format0;
		  memset (&discarded[format0 + 1 - format_start], 2,
			  convbytes - 1);
		}
	      else if (multibyte && !ASCII_CHAR_P (format_char))
		{
		  int c = BYTE8_TO_CHAR (format_char);
		  convbytes = CHAR_STRING (c, str);
		  convsrc = (char *) str;
		  new_result = true;
		}
	    }

	copy_char:
	  if (convbytes <= buf + bufsize - p)
	    {
	      memcpy (p, convsrc, convbytes);
	      p += convbytes;
	      nchars++;
	      continue;
	    }
	}

      /* There wasn't enough room to store this conversion or single
	 character.  CONVBYTES says how much room is needed.  Allocate
	 enough room (and then some) and do it again.  */

      ptrdiff_t used = p - buf;
      if (max_bufsize - used < convbytes)
	string_overflow ();
      bufsize = used + convbytes;
      bufsize = bufsize < max_bufsize / 2 ? bufsize * 2 : max_bufsize;

      if (buf == initial_buffer)
	{
	  buf = xmalloc (bufsize);
	  sa_must_free = true;
	  buf_save_value_index = SPECPDL_INDEX ();
	  record_unwind_protect_ptr (xfree, buf);
	  memcpy (buf, initial_buffer, used);
	}
      else
	{
	  buf = xrealloc (buf, bufsize);
	  set_unwind_protect_ptr (buf_save_value_index, xfree, buf);
	}

      p = buf + used;
      format = format0;
      n = n0;
      ispec = ispec0;
    }

  if (bufsize < p - buf)
    emacs_abort ();

  if (! new_result)
    {
      val = args[0];
      goto return_val;
    }

  if (maybe_combine_byte)
    nchars = multibyte_chars_in_text ((unsigned char *) buf, p - buf);
  val = make_specified_string (buf, nchars, p - buf, multibyte);

  /* If the format string has text properties, or any of the string
     arguments has text properties, set up text properties of the
     result string.  */

  if (string_intervals (args[0]) || arg_intervals)
    {
      /* Add text properties from the format string.  */
      Lisp_Object len = make_number (SCHARS (args[0]));
      Lisp_Object props = text_property_list (args[0], make_number (0),
					      len, Qnil);
      if (CONSP (props))
	{
	  ptrdiff_t bytepos = 0, position = 0, translated = 0;
	  ptrdiff_t fieldn = 0;

	  /* Adjust the bounds of each text property
	     to the proper start and end in the output string.  */

	  /* Put the positions in PROPS in increasing order, so that
	     we can do (effectively) one scan through the position
	     space of the format string.  */
	  props = Fnreverse (props);

	  /* BYTEPOS is the byte position in the format string,
	     POSITION is the untranslated char position in it,
	     TRANSLATED is the translated char position in BUF,
	     and ARGN is the number of the next arg we will come to.  */
	  for (Lisp_Object list = props; CONSP (list); list = XCDR (list))
	    {
	      Lisp_Object item = XCAR (list);

	      /* First adjust the property start position.  */
	      ptrdiff_t pos = XINT (XCAR (item));

	      /* Advance BYTEPOS, POSITION, TRANSLATED and ARGN
		 up to this position.  */
	      for (; position < pos; bytepos++)
		{
		  if (! discarded[bytepos])
		    position++, translated++;
		  else if (discarded[bytepos] == 1)
		    {
		      position++;
		      if (fieldn < nspec && translated == info[fieldn].start)
			{
			  translated += info[fieldn].end - info[fieldn].start;
			  fieldn++;
			}
		    }
		}

	      XSETCAR (item, make_number (translated));

	      /* Likewise adjust the property end position.  */
	      pos = XINT (XCAR (XCDR (item)));

	      for (; position < pos; bytepos++)
		{
		  if (! discarded[bytepos])
		    position++, translated++;
		  else if (discarded[bytepos] == 1)
		    {
		      position++;
		      if (fieldn < nspec && translated == info[fieldn].start)
			{
			  translated += info[fieldn].end - info[fieldn].start;
			  fieldn++;
			}
		    }
		}

	      XSETCAR (XCDR (item), make_number (translated));
	    }

	  add_text_properties_from_list (val, props, make_number (0));
	}

      /* Add text properties from arguments.  */
      if (arg_intervals)
	for (ptrdiff_t i = 0; i < nspec; i++)
	  if (info[i].intervals)
	    {
	      len = make_number (SCHARS (info[i].argument));
	      Lisp_Object new_len = make_number (info[i].end - info[i].start);
	      props = text_property_list (info[i].argument,
                                          make_number (0), len, Qnil);
	      props = extend_property_ranges (props, len, new_len);
	      /* If successive arguments have properties, be sure that
		 the value of `composition' property be the copy.  */
	      if (1 < i && info[i - 1].end)
		make_composition_value_copy (props);
	      add_text_properties_from_list (val, props,
					     make_number (info[i].start));
	    }
    }

 return_val:
  /* If we allocated BUF or INFO with malloc, free it too.  */
  SAFE_FREE ();

  return val;
}

DEFUN ("char-equal", Fchar_equal, Schar_equal, 2, 2, 0,
       doc: /* Return t if two characters match, optionally ignoring case.
Both arguments must be characters (i.e. integers).
Case is ignored if `case-fold-search' is non-nil in the current buffer.  */)
  (register Lisp_Object c1, Lisp_Object c2)
{
  int i1, i2;
  /* Check they're chars, not just integers, otherwise we could get array
     bounds violations in downcase.  */
  CHECK_CHARACTER (c1);
  CHECK_CHARACTER (c2);

  if (XINT (c1) == XINT (c2))
    return Qt;
  if (NILP (BVAR (current_buffer, case_fold_search)))
    return Qnil;

  i1 = XFASTINT (c1);
  i2 = XFASTINT (c2);

  /* FIXME: It is possible to compare multibyte characters even when
     the current buffer is unibyte.  Unfortunately this is ambiguous
     for characters between 128 and 255, as they could be either
     eight-bit raw bytes or Latin-1 characters.  Assume the former for
     now.  See Bug#17011, and also see casefiddle.c's casify_object,
     which has a similar problem.  */
  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    {
      if (SINGLE_BYTE_CHAR_P (i1))
	i1 = UNIBYTE_TO_CHAR (i1);
      if (SINGLE_BYTE_CHAR_P (i2))
	i2 = UNIBYTE_TO_CHAR (i2);
    }

  return (downcase (i1) == downcase (i2) ? Qt :  Qnil);
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
transpose_markers (ptrdiff_t start1, ptrdiff_t end1,
		   ptrdiff_t start2, ptrdiff_t end2,
		   ptrdiff_t start1_byte, ptrdiff_t end1_byte,
		   ptrdiff_t start2_byte, ptrdiff_t end2_byte)
{
  register ptrdiff_t amt1, amt1_byte, amt2, amt2_byte, diff, diff_byte, mpos;
  register struct Lisp_Marker *marker;

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

  for (marker = BUF_MARKERS (current_buffer); marker; marker = marker->next)
    {
      mpos = marker->bytepos;
      if (mpos >= start1_byte && mpos < end2_byte)
	{
	  if (mpos < end1_byte)
	    mpos += amt1_byte;
	  else if (mpos < start2_byte)
	    mpos += diff_byte;
	  else
	    mpos -= amt2_byte;
	  marker->bytepos = mpos;
	}
      mpos = marker->charpos;
      if (mpos >= start1 && mpos < end2)
	{
	  if (mpos < end1)
	    mpos += amt1;
	  else if (mpos < start2)
	    mpos += diff;
	  else
	    mpos -= amt2;
	}
      marker->charpos = mpos;
    }
}

DEFUN ("transpose-regions", Ftranspose_regions, Stranspose_regions, 4, 5,
       "(if (< (length mark-ring) 2)\
	    (error \"Other region must be marked before transposing two regions\")\
	  (let* ((num (if current-prefix-arg\
			 (prefix-numeric-value current-prefix-arg)\
			0))\
		 (ring-length (length mark-ring))\
		 (eltnum (mod num ring-length))\
		 (eltnum2 (mod (1+ num) ring-length)))\
	    (list (point) (mark) (elt mark-ring eltnum) (elt mark-ring eltnum2))))",
       doc: /* Transpose region STARTR1 to ENDR1 with STARTR2 to ENDR2.
The regions should not be overlapping, because the size of the buffer is
never changed in a transposition.

Optional fifth arg LEAVE-MARKERS, if non-nil, means don't update
any markers that happen to be located in the regions.

Transposing beyond buffer boundaries is an error.

Interactively, STARTR1 and ENDR1 are point and mark; STARTR2 and ENDR2
are the last two marks pushed to the mark ring; LEAVE-MARKERS is nil.
If a prefix argument N is given, STARTR2 and ENDR2 are the two
successive marks N entries back in the mark ring.  A negative prefix
argument instead counts forward from the oldest mark in the mark
ring.  */)
  (Lisp_Object startr1, Lisp_Object endr1, Lisp_Object startr2, Lisp_Object endr2, Lisp_Object leave_markers)
{
  register ptrdiff_t start1, end1, start2, end2;
  ptrdiff_t start1_byte, start2_byte, len1_byte, len2_byte, end2_byte;
  ptrdiff_t gap, len1, len_mid, len2;
  unsigned char *start1_addr, *start2_addr, *temp;

  INTERVAL cur_intv, tmp_interval1, tmp_interval_mid, tmp_interval2, tmp_interval3;
  Lisp_Object buf;

  XSETBUFFER (buf, current_buffer);
  cur_intv = buffer_intervals (current_buffer);

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
      register ptrdiff_t glumph = start1;
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
  /* Nothing to change for adjacent regions with one being empty */
  else if ((start1 == end1 || start2 == end2) && end1 == start2)
    return Qnil;

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

  start1_byte = CHAR_TO_BYTE (start1);
  end2_byte = CHAR_TO_BYTE (end2);

  /* Make sure the gap won't interfere, by moving it out of the text
     we will operate on.  */
  if (start1 < gap && gap < end2)
    {
      if (gap - start1 < end2 - gap)
	move_gap_both (start1, start1_byte);
      else
	move_gap_both (end2, end2_byte);
    }

  start2_byte = CHAR_TO_BYTE (start2);
  len1_byte = CHAR_TO_BYTE (end1) - start1_byte;
  len2_byte = end2_byte - start2_byte;

#ifdef BYTE_COMBINING_DEBUG
  if (end1 == start2)
    {
      if (count_combining_before (BYTE_POS_ADDR (start2_byte),
				  len2_byte, start1, start1_byte)
	  || count_combining_before (BYTE_POS_ADDR (start1_byte),
				     len1_byte, end2, start2_byte + len2_byte)
	  || count_combining_after (BYTE_POS_ADDR (start1_byte),
				    len1_byte, end2, start2_byte + len2_byte))
	emacs_abort ();
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
	emacs_abort ();
    }
#endif

  /* Hmmm... how about checking to see if the gap is large
     enough to use as the temporary storage?  That would avoid an
     allocation... interesting.  Later, don't fool with it now.  */

  /* Working without memmove, for portability (sigh), so must be
     careful of overlapping subsections of the array...  */

  if (end1 == start2)		/* adjacent regions */
    {
      modify_text (start1, end2);
      record_change (start1, len1 + len2);

      tmp_interval1 = copy_intervals (cur_intv, start1, len1);
      tmp_interval2 = copy_intervals (cur_intv, start2, len2);
      /* Don't use Fset_text_properties: that can cause GC, which can
	 clobber objects stored in the tmp_intervals.  */
      tmp_interval3 = validate_interval_range (buf, &startr1, &endr2, 0);
      if (tmp_interval3)
	set_text_properties_1 (startr1, endr2, Qnil, buf, tmp_interval3);

      USE_SAFE_ALLOCA;

      /* First region smaller than second.  */
      if (len1_byte < len2_byte)
        {
	  temp = SAFE_ALLOCA (len2_byte);

	  /* Don't precompute these addresses.  We have to compute them
	     at the last minute, because the relocating allocator might
	     have moved the buffer around during the xmalloc.  */
	  start1_addr = BYTE_POS_ADDR (start1_byte);
	  start2_addr = BYTE_POS_ADDR (start2_byte);

          memcpy (temp, start2_addr, len2_byte);
          memcpy (start1_addr + len2_byte, start1_addr, len1_byte);
          memcpy (start1_addr, temp, len2_byte);
        }
      else
	/* First region not smaller than second.  */
        {
	  temp = SAFE_ALLOCA (len1_byte);
	  start1_addr = BYTE_POS_ADDR (start1_byte);
	  start2_addr = BYTE_POS_ADDR (start2_byte);
          memcpy (temp, start1_addr, len1_byte);
          memcpy (start1_addr, start2_addr, len2_byte);
          memcpy (start1_addr + len2_byte, temp, len1_byte);
        }

      SAFE_FREE ();
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
	  USE_SAFE_ALLOCA;

          modify_text (start1, end2);
          record_change (start1, len1);
          record_change (start2, len2);
          tmp_interval1 = copy_intervals (cur_intv, start1, len1);
          tmp_interval2 = copy_intervals (cur_intv, start2, len2);

	  tmp_interval3 = validate_interval_range (buf, &startr1, &endr1, 0);
	  if (tmp_interval3)
	    set_text_properties_1 (startr1, endr1, Qnil, buf, tmp_interval3);

	  tmp_interval3 = validate_interval_range (buf, &startr2, &endr2, 0);
	  if (tmp_interval3)
	    set_text_properties_1 (startr2, endr2, Qnil, buf, tmp_interval3);

	  temp = SAFE_ALLOCA (len1_byte);
	  start1_addr = BYTE_POS_ADDR (start1_byte);
	  start2_addr = BYTE_POS_ADDR (start2_byte);
          memcpy (temp, start1_addr, len1_byte);
          memcpy (start1_addr, start2_addr, len2_byte);
          memcpy (start2_addr, temp, len1_byte);
	  SAFE_FREE ();

          graft_intervals_into_buffer (tmp_interval1, start2,
                                       len1, current_buffer, 0);
          graft_intervals_into_buffer (tmp_interval2, start1,
                                       len2, current_buffer, 0);
        }

      else if (len1_byte < len2_byte)	/* Second region larger than first */
        /* Non-adjacent & unequal size, area between must also be shifted.  */
        {
	  USE_SAFE_ALLOCA;

          modify_text (start1, end2);
          record_change (start1, (end2 - start1));
          tmp_interval1 = copy_intervals (cur_intv, start1, len1);
          tmp_interval_mid = copy_intervals (cur_intv, end1, len_mid);
          tmp_interval2 = copy_intervals (cur_intv, start2, len2);

	  tmp_interval3 = validate_interval_range (buf, &startr1, &endr2, 0);
	  if (tmp_interval3)
	    set_text_properties_1 (startr1, endr2, Qnil, buf, tmp_interval3);

	  /* holds region 2 */
	  temp = SAFE_ALLOCA (len2_byte);
	  start1_addr = BYTE_POS_ADDR (start1_byte);
	  start2_addr = BYTE_POS_ADDR (start2_byte);
          memcpy (temp, start2_addr, len2_byte);
          memcpy (start1_addr + len_mid + len2_byte, start1_addr, len1_byte);
          memmove (start1_addr + len2_byte, start1_addr + len1_byte, len_mid);
          memcpy (start1_addr, temp, len2_byte);
	  SAFE_FREE ();

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
	  USE_SAFE_ALLOCA;

          record_change (start1, (end2 - start1));
          modify_text (start1, end2);

          tmp_interval1 = copy_intervals (cur_intv, start1, len1);
          tmp_interval_mid = copy_intervals (cur_intv, end1, len_mid);
          tmp_interval2 = copy_intervals (cur_intv, start2, len2);

	  tmp_interval3 = validate_interval_range (buf, &startr1, &endr2, 0);
	  if (tmp_interval3)
	    set_text_properties_1 (startr1, endr2, Qnil, buf, tmp_interval3);

	  /* holds region 1 */
	  temp = SAFE_ALLOCA (len1_byte);
	  start1_addr = BYTE_POS_ADDR (start1_byte);
	  start2_addr = BYTE_POS_ADDR (start2_byte);
          memcpy (temp, start1_addr, len1_byte);
          memcpy (start1_addr, start2_addr, len2_byte);
          memmove (start1_addr + len2_byte, start1_addr + len1_byte, len_mid);
          memcpy (start1_addr + len2_byte + len_mid, temp, len1_byte);
	  SAFE_FREE ();

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
      fix_start_end_in_overlays (start1, end2);
    }
  else
    {
      /* The character positions of the markers remain intact, but we
	 still need to update their byte positions, because the
	 transposed regions might include multibyte sequences which
	 make some original byte positions of the markers invalid.  */
      adjust_markers_bytepos (start1, start1_byte, end2, end2_byte, 0);
    }

  signal_after_change (start1, end2 - start1, end2 - start1);
  return Qnil;
}


void
syms_of_editfns (void)
{
  DEFSYM (Qbuffer_access_fontify_functions, "buffer-access-fontify-functions");
  DEFSYM (Qwall, "wall");

  DEFVAR_LISP ("inhibit-field-text-motion", Vinhibit_field_text_motion,
	       doc: /* Non-nil means text motion commands don't notice fields.  */);
  Vinhibit_field_text_motion = Qnil;

  DEFVAR_LISP ("buffer-access-fontify-functions",
	       Vbuffer_access_fontify_functions,
	       doc: /* List of functions called by `buffer-substring' to fontify if necessary.
Each function is called with two arguments which specify the range
of the buffer being accessed.  */);
  Vbuffer_access_fontify_functions = Qnil;

  {
    Lisp_Object obuf;
    obuf = Fcurrent_buffer ();
    /* Do this here, because init_buffer_once is too early--it won't work.  */
    Fset_buffer (Vprin1_to_string_buffer);
    /* Make sure buffer-access-fontify-functions is nil in this buffer.  */
    Fset (Fmake_local_variable (Qbuffer_access_fontify_functions), Qnil);
    Fset_buffer (obuf);
  }

  DEFVAR_LISP ("buffer-access-fontified-property",
	       Vbuffer_access_fontified_property,
	       doc: /* Property which (if non-nil) indicates text has been fontified.
`buffer-substring' need not call the `buffer-access-fontify-functions'
functions if all the text being accessed has this property.  */);
  Vbuffer_access_fontified_property = Qnil;

  DEFVAR_LISP ("system-name", Vsystem_name,
	       doc: /* The host name of the machine Emacs is running on.  */);
  Vsystem_name = cached_system_name = Qnil;

  DEFVAR_LISP ("user-full-name", Vuser_full_name,
	       doc: /* The full name of the user logged in.  */);

  DEFVAR_LISP ("user-login-name", Vuser_login_name,
	       doc: /* The user's name, taken from environment variables if possible.  */);
  Vuser_login_name = Qnil;

  DEFVAR_LISP ("user-real-login-name", Vuser_real_login_name,
	       doc: /* The user's name, based upon the real uid only.  */);

  DEFVAR_LISP ("operating-system-release", Voperating_system_release,
	       doc: /* The release of the operating system Emacs is running on.  */);

  defsubr (&Spropertize);
  defsubr (&Schar_equal);
  defsubr (&Sgoto_char);
  defsubr (&Sstring_to_char);
  defsubr (&Schar_to_string);
  defsubr (&Sbyte_to_string);
  defsubr (&Sbuffer_substring);
  defsubr (&Sbuffer_substring_no_properties);
  defsubr (&Sbuffer_string);
  defsubr (&Sget_pos_property);

  defsubr (&Spoint_marker);
  defsubr (&Smark_marker);
  defsubr (&Spoint);
  defsubr (&Sregion_beginning);
  defsubr (&Sregion_end);

  /* Symbol for the text property used to mark fields.  */
  DEFSYM (Qfield, "field");

  /* A special value for Qfield properties.  */
  DEFSYM (Qboundary, "boundary");

  defsubr (&Sfield_beginning);
  defsubr (&Sfield_end);
  defsubr (&Sfield_string);
  defsubr (&Sfield_string_no_properties);
  defsubr (&Sdelete_field);
  defsubr (&Sconstrain_to_field);

  defsubr (&Sline_beginning_position);
  defsubr (&Sline_end_position);

  defsubr (&Ssave_excursion);
  defsubr (&Ssave_current_buffer);

  defsubr (&Sbuffer_size);
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
  defsubr (&Sinsert_byte);

  defsubr (&Suser_login_name);
  defsubr (&Suser_real_login_name);
  defsubr (&Suser_uid);
  defsubr (&Suser_real_uid);
  defsubr (&Sgroup_gid);
  defsubr (&Sgroup_real_gid);
  defsubr (&Suser_full_name);
  defsubr (&Semacs_pid);
  defsubr (&Scurrent_time);
  defsubr (&Stime_add);
  defsubr (&Stime_subtract);
  defsubr (&Stime_less_p);
  defsubr (&Sget_internal_run_time);
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
  defsubr (&Sformat_message);

  defsubr (&Sinsert_buffer_substring);
  defsubr (&Scompare_buffer_substrings);
  defsubr (&Sreplace_buffer_contents);
  defsubr (&Ssubst_char_in_region);
  defsubr (&Stranslate_region_internal);
  defsubr (&Sdelete_region);
  defsubr (&Sdelete_and_extract_region);
  defsubr (&Swiden);
  defsubr (&Snarrow_to_region);
  defsubr (&Ssave_restriction);
  defsubr (&Stranspose_regions);
}
