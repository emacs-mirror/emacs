/* Lisp functions pertaining to editing.
   Copyright (C) 1985,86,87,89,93,94,95 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <sys/types.h>

#include <config.h>

#ifdef VMS
#include "vms-pwd.h"
#else
#include <pwd.h>
#endif

#include "lisp.h"
#include "intervals.h"
#include "buffer.h"
#include "window.h"

#include "systime.h"

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

extern void insert_from_buffer ();
static long difftm ();

/* Some static data, and a function to initialize it for each run */

Lisp_Object Vsystem_name;
Lisp_Object Vuser_real_name;	/* login name of current user ID */
Lisp_Object Vuser_full_name;	/* full name of current user */
Lisp_Object Vuser_name;		/* user name from LOGNAME or USER */

void
init_editfns ()
{
  char *user_name;
  register unsigned char *p, *q, *r;
  struct passwd *pw;	/* password entry for the current user */
  extern char *index ();
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
  Vuser_real_name = build_string (pw ? pw->pw_name : "root");
#else
  Vuser_real_name = build_string (pw ? pw->pw_name : "unknown");
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
  Vuser_name = build_string (user_name);

  /* If the user name claimed in the environment vars differs from
     the real uid, use the claimed name to find the full name.  */
  tem = Fstring_equal (Vuser_name, Vuser_real_name);
  if (NILP (tem))
    pw = (struct passwd *) getpwnam (XSTRING (Vuser_name)->data);
  
  p = (unsigned char *) (pw ? USER_FULL_NAME : "unknown");
  q = (unsigned char *) index (p, ',');
  Vuser_full_name = make_string (p, q ? q - p : strlen (p));
  
#ifdef AMPERSAND_FULL_NAME
  p = XSTRING (Vuser_full_name)->data;
  q = (unsigned char *) index (p, '&');
  /* Substitute the login name for the &, upcasing the first character.  */
  if (q)
    {
      r = (unsigned char *) alloca (strlen (p) + XSTRING (Vuser_name)->size + 1);
      bcopy (p, r, q - p);
      r[q - p] = 0;
      strcat (r, XSTRING (Vuser_name)->data);
      r[q - p] = UPCASE (r[q - p]);
      strcat (r, q + 1);
      Vuser_full_name = build_string (r);
    }
#endif /* AMPERSAND_FULL_NAME */

  p = (unsigned char *) getenv ("NAME");
  if (p)
    Vuser_full_name = build_string (p);
}

DEFUN ("char-to-string", Fchar_to_string, Schar_to_string, 1, 1, 0,
  "Convert arg CHAR to a one-character string containing that character.")
  (n)
     Lisp_Object n;
{
  char c;
  CHECK_NUMBER (n, 0);

  c = XINT (n);
  return make_string (&c, 1);
}

DEFUN ("string-to-char", Fstring_to_char, Sstring_to_char, 1, 1, 0,
  "Convert arg STRING to a character, the first character of that string.")
  (str)
     register Lisp_Object str;
{
  register Lisp_Object val;
  register struct Lisp_String *p;
  CHECK_STRING (str, 0);

  p = XSTRING (str);
  if (p->size)
    XSETFASTINT (val, ((unsigned char *) p->data)[0]);
  else
    XSETFASTINT (val, 0);
  return val;
}

static Lisp_Object
buildmark (val)
     int val;
{
  register Lisp_Object mark;
  mark = Fmake_marker ();
  Fset_marker (mark, make_number (val), Qnil);
  return mark;
}

DEFUN ("point", Fpoint, Spoint, 0, 0, 0,
  "Return value of point, as an integer.\n\
Beginning of buffer is position (point-min)")
  ()
{
  Lisp_Object temp;
  XSETFASTINT (temp, point);
  return temp;
}

DEFUN ("point-marker", Fpoint_marker, Spoint_marker, 0, 0, 0,
   "Return value of point, as a marker object.")
  ()
{
  return buildmark (point);
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
  "Set point to POSITION, a number or marker.\n\
Beginning of buffer is position (point-min), end is (point-max).")
  (n)
     register Lisp_Object n;
{
  CHECK_NUMBER_COERCE_MARKER (n, 0);

  SET_PT (clip_to_bounds (BEGV, XINT (n), ZV));
  return n;
}

static Lisp_Object
region_limit (beginningp)
     int beginningp;
{
  extern Lisp_Object Vmark_even_if_inactive; /* Defined in callint.c. */
  register Lisp_Object m;
  if (!NILP (Vtransient_mark_mode) && NILP (Vmark_even_if_inactive)
      && NILP (current_buffer->mark_active))
    Fsignal (Qmark_inactive, Qnil);
  m = Fmarker_position (current_buffer->mark);
  if (NILP (m)) error ("There is no region now");
  if ((point < XFASTINT (m)) == beginningp)
    return (make_number (point));
  else
    return (m);
}

DEFUN ("region-beginning", Fregion_beginning, Sregion_beginning, 0, 0, 0,
  "Return position of beginning of region, as an integer.")
  ()
{
  return (region_limit (1));
}

DEFUN ("region-end", Fregion_end, Sregion_end, 0, 0, 0,
  "Return position of end of region, as an integer.")
  ()
{
  return (region_limit (0));
}

#if 0 /* now in lisp code */
DEFUN ("mark", Fmark, Smark, 0, 0, 0,
  "Return this buffer's mark value as integer, or nil if no mark.\n\
If you are using this in an editing command, you are most likely making\n\
a mistake; see the documentation of `set-mark'.")
  ()
{
  return Fmarker_position (current_buffer->mark);
}
#endif /* commented out code */

DEFUN ("mark-marker", Fmark_marker, Smark_marker, 0, 0, 0,
  "Return this buffer's mark, as a marker object.\n\
Watch out!  Moving this marker changes the mark position.\n\
If you set the marker not to point anywhere, the buffer will have no mark.")
  ()
{
  return current_buffer->mark;
}

#if 0 /* this is now in lisp code */
DEFUN ("set-mark", Fset_mark, Sset_mark, 1, 1, 0,
  "Set this buffer's mark to POS.  Don't use this function!\n\
That is to say, don't use this function unless you want\n\
the user to see that the mark has moved, and you want the previous\n\
mark position to be lost.\n\
\n\
Normally, when a new mark is set, the old one should go on the stack.\n\
This is why most applications should use push-mark, not set-mark.\n\
\n\
Novice programmers often try to use the mark for the wrong purposes.\n\
The mark saves a location for the user's convenience.\n\
Most editing commands should not alter the mark.\n\
To remember a location for internal use in the Lisp program,\n\
store it in a Lisp variable.  Example:\n\
\n\
   (let ((beg (point))) (forward-line 1) (delete-region beg (point))).")
  (pos)
     Lisp_Object pos;
{
  if (NILP (pos))
    {
      current_buffer->mark = Qnil;
      return Qnil;
    }
  CHECK_NUMBER_COERCE_MARKER (pos, 0);

  if (NILP (current_buffer->mark))
    current_buffer->mark = Fmake_marker ();

  Fset_marker (current_buffer->mark, pos, Qnil);
  return pos;
}
#endif /* commented-out code */

Lisp_Object
save_excursion_save ()
{
  register int visible = (XBUFFER (XWINDOW (selected_window)->buffer)
			  == current_buffer);

  return Fcons (Fpoint_marker (),
		Fcons (Fcopy_marker (current_buffer->mark),
		       Fcons (visible ? Qt : Qnil,
			      current_buffer->mark_active)));		       
}

Lisp_Object
save_excursion_restore (info)
     register Lisp_Object info;
{
  register Lisp_Object tem, tem1, omark, nmark;

  tem = Fmarker_buffer (Fcar (info));
  /* If buffer being returned to is now deleted, avoid error */
  /* Otherwise could get error here while unwinding to top level
     and crash */
  /* In that case, Fmarker_buffer returns nil now.  */
  if (NILP (tem))
    return Qnil;
  Fset_buffer (tem);
  tem = Fcar (info);
  Fgoto_char (tem);
  unchain_marker (tem);
  tem = Fcar (Fcdr (info));
  omark = Fmarker_position (current_buffer->mark);
  Fset_marker (current_buffer->mark, tem, Fcurrent_buffer ());
  nmark = Fmarker_position (tem);
  unchain_marker (tem);
  tem = Fcdr (Fcdr (info));
#if 0 /* We used to make the current buffer visible in the selected window
	 if that was true previously.  That avoids some anomalies.
	 But it creates others, and it wasn't documented, and it is simpler
	 and cleaner never to alter the window/buffer connections.  */
  tem1 = Fcar (tem);
  if (!NILP (tem1)
      && current_buffer != XBUFFER (XWINDOW (selected_window)->buffer))
    Fswitch_to_buffer (Fcurrent_buffer (), Qnil);
#endif /* 0 */

  tem1 = current_buffer->mark_active;
  current_buffer->mark_active = Fcdr (tem);
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
  return Qnil;
}

DEFUN ("save-excursion", Fsave_excursion, Ssave_excursion, 0, UNEVALLED, 0,
  "Save point, mark, and current buffer; execute BODY; restore those things.\n\
Executes BODY just like `progn'.\n\
The values of point, mark and the current buffer are restored\n\
even in case of abnormal exit (throw or error).\n\
The state of activation of the mark is also restored.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object val;
  int count = specpdl_ptr - specpdl;

  record_unwind_protect (save_excursion_restore, save_excursion_save ());
			 
  val = Fprogn (args);
  return unbind_to (count, val);
}

DEFUN ("buffer-size", Fbufsize, Sbufsize, 0, 0, 0,
  "Return the number of characters in the current buffer.")
  ()
{
  Lisp_Object temp;
  XSETFASTINT (temp, Z - BEG);
  return temp;
}

DEFUN ("point-min", Fpoint_min, Spoint_min, 0, 0, 0,
  "Return the minimum permissible value of point in the current buffer.\n\
This is 1, unless narrowing (a buffer restriction) is in effect.")
  ()
{
  Lisp_Object temp;
  XSETFASTINT (temp, BEGV);
  return temp;
}

DEFUN ("point-min-marker", Fpoint_min_marker, Spoint_min_marker, 0, 0, 0,
  "Return a marker to the minimum permissible value of point in this buffer.\n\
This is the beginning, unless narrowing (a buffer restriction) is in effect.")
  ()
{
  return buildmark (BEGV);
}

DEFUN ("point-max", Fpoint_max, Spoint_max, 0, 0, 0,
  "Return the maximum permissible value of point in the current buffer.\n\
This is (1+ (buffer-size)), unless narrowing (a buffer restriction)\n\
is in effect, in which case it is less.")
  ()
{
  Lisp_Object temp;
  XSETFASTINT (temp, ZV);
  return temp;
}

DEFUN ("point-max-marker", Fpoint_max_marker, Spoint_max_marker, 0, 0, 0,
  "Return a marker to the maximum permissible value of point in this buffer.\n\
This is (1+ (buffer-size)), unless narrowing (a buffer restriction)\n\
is in effect, in which case it is less.")
  ()
{
  return buildmark (ZV);
}

DEFUN ("following-char", Ffollowing_char, Sfollowing_char, 0, 0, 0,
  "Return the character following point, as a number.\n\
At the end of the buffer or accessible region, return 0.")
  ()
{
  Lisp_Object temp;
  if (point >= ZV)
    XSETFASTINT (temp, 0);
  else
    XSETFASTINT (temp, FETCH_CHAR (point));
  return temp;
}

DEFUN ("preceding-char", Fprevious_char, Sprevious_char, 0, 0, 0,
  "Return the character preceding point, as a number.\n\
At the beginning of the buffer or accessible region, return 0.")
  ()
{
  Lisp_Object temp;
  if (point <= BEGV)
    XSETFASTINT (temp, 0);
  else
    XSETFASTINT (temp, FETCH_CHAR (point - 1));
  return temp;
}

DEFUN ("bobp", Fbobp, Sbobp, 0, 0, 0,
  "Return T if point is at the beginning of the buffer.\n\
If the buffer is narrowed, this means the beginning of the narrowed part.")
  ()
{
  if (point == BEGV)
    return Qt;
  return Qnil;
}

DEFUN ("eobp", Feobp, Seobp, 0, 0, 0,
  "Return T if point is at the end of the buffer.\n\
If the buffer is narrowed, this means the end of the narrowed part.")
  ()
{
  if (point == ZV)
    return Qt;
  return Qnil;
}

DEFUN ("bolp", Fbolp, Sbolp, 0, 0, 0,
  "Return T if point is at the beginning of a line.")
  ()
{
  if (point == BEGV || FETCH_CHAR (point - 1) == '\n')
    return Qt;
  return Qnil;
}

DEFUN ("eolp", Feolp, Seolp, 0, 0, 0,
  "Return T if point is at the end of a line.\n\
`End of a line' includes point being at the end of the buffer.")
  ()
{
  if (point == ZV || FETCH_CHAR (point) == '\n')
    return Qt;
  return Qnil;
}

DEFUN ("char-after", Fchar_after, Schar_after, 1, 1, 0,
  "Return character in current buffer at position POS.\n\
POS is an integer or a buffer pointer.\n\
If POS is out of range, the value is nil.")
  (pos)
     Lisp_Object pos;
{
  register Lisp_Object val;
  register int n;

  CHECK_NUMBER_COERCE_MARKER (pos, 0);

  n = XINT (pos);
  if (n < BEGV || n >= ZV) return Qnil;

  XSETFASTINT (val, FETCH_CHAR (n));
  return val;
}

DEFUN ("user-login-name", Fuser_login_name, Suser_login_name, 0, 1, 0,
  "Return the name under which the user logged in, as a string.\n\
This is based on the effective uid, not the real uid.\n\
Also, if the environment variable LOGNAME or USER is set,\n\
that determines the value of this function.\n\n\
If optional argument UID is an integer, return the login name of the user\n\
with that uid, or nil if there is no such user.")
  (uid)
     Lisp_Object uid;
{
  struct passwd *pw;

  /* Set up the user name info if we didn't do it before.
     (That can happen if Emacs is dumpable
     but you decide to run `temacs -l loadup' and not dump.  */
  if (INTEGERP (Vuser_name))
    init_editfns ();

  if (NILP (uid))
    return Vuser_name;

  CHECK_NUMBER (uid, 0);
  pw = (struct passwd *) getpwuid (XINT (uid));
  return (pw ? build_string (pw->pw_name) : Qnil);
}

DEFUN ("user-real-login-name", Fuser_real_login_name, Suser_real_login_name,
  0, 0, 0,
  "Return the name of the user's real uid, as a string.\n\
This ignores the environment variables LOGNAME and USER, so it differs from\n\
`user-login-name' when running under `su'.")
  ()
{
  /* Set up the user name info if we didn't do it before.
     (That can happen if Emacs is dumpable
     but you decide to run `temacs -l loadup' and not dump.  */
  if (INTEGERP (Vuser_name))
    init_editfns ();
  return Vuser_real_name;
}

DEFUN ("user-uid", Fuser_uid, Suser_uid, 0, 0, 0,
  "Return the effective uid of Emacs, as an integer.")
  ()
{
  return make_number (geteuid ());
}

DEFUN ("user-real-uid", Fuser_real_uid, Suser_real_uid, 0, 0, 0,
  "Return the real uid of Emacs, as an integer.")
  ()
{
  return make_number (getuid ());
}

DEFUN ("user-full-name", Fuser_full_name, Suser_full_name, 0, 0, 0,
  "Return the full name of the user logged in, as a string.")
  ()
{
  return Vuser_full_name;
}

DEFUN ("system-name", Fsystem_name, Ssystem_name, 0, 0, 0,
  "Return the name of the machine you are running on, as a string.")
  ()
{
  return Vsystem_name;
}

/* For the benefit of callers who don't want to include lisp.h */
char *
get_system_name ()
{
  return (char *) XSTRING (Vsystem_name)->data;
}

DEFUN ("emacs-pid", Femacs_pid, Semacs_pid, 0, 0, 0,
  "Return the process ID of Emacs, as an integer.")
  ()
{
  return make_number (getpid ());
}

DEFUN ("current-time", Fcurrent_time, Scurrent_time, 0, 0, 0,
  "Return the current time, as the number of seconds since 12:00 AM January 1970.\n\
The time is returned as a list of three integers.  The first has the\n\
most significant 16 bits of the seconds, while the second has the\n\
least significant 16 bits.  The third integer gives the microsecond\n\
count.\n\
\n\
The microsecond count is zero on systems that do not provide\n\
resolution finer than a second.")
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
lisp_time_argument (specified_time, result)
     Lisp_Object specified_time;
     time_t *result;
{
  if (NILP (specified_time))
    return time (result) != -1;
  else
    {
      Lisp_Object high, low;
      high = Fcar (specified_time);
      CHECK_NUMBER (high, 0);
      low = Fcdr (specified_time);
      if (CONSP (low))
	low = Fcar (low);
      CHECK_NUMBER (low, 0);
      *result = (XINT (high) << 16) + (XINT (low) & 0xffff);
      return *result >> 16 == XINT (high);
    }
}

DEFUN ("format-time-string", Fformat_time_string, Sformat_time_string, 2, 2, 0,
  "Use FORMAT-STRING to format the time TIME.\n\
TIME is specified as (HIGH LOW . IGNORED) or (HIGH . LOW), as from\n\
`current-time' and `file-attributes'.\n\
FORMAT-STRING may contain %-sequences to substitute parts of the time.\n\
%a is replaced by the abbreviated name of the day of week.\n\
%A is replaced by the full name of the day of week.\n\
%b is replaced by the abbreviated name of the month.\n\
%B is replaced by the full name of the month.\n\
%c is a synonym for \"%x %X\".\n\
%C is a locale-specific synonym, which defaults to \"%A, %B %e, %Y\" in the C locale.\n\
%d is replaced by the day of month, zero-padded.\n\
%D is a synonym for \"%m/%d/%y\".\n\
%e is replaced by the day of month, blank-padded.\n\
%h is a synonym for \"%b\".\n\
%H is replaced by the hour (00-23).\n\
%I is replaced by the hour (00-12).\n\
%j is replaced by the day of the year (001-366).\n\
%k is replaced by the hour (0-23), blank padded.\n\
%l is replaced by the hour (1-12), blank padded.\n\
%m is replaced by the month (01-12).\n\
%M is replaced by the minut (00-59).\n\
%n is a synonym for \"\\n\".\n\
%p is replaced by AM or PM, as appropriate.\n\
%r is a synonym for \"%I:%M:%S %p\".\n\
%R is a synonym for \"%H:%M\".\n\
%S is replaced by the seconds (00-60).\n\
%t is a synonym for \"\\t\".\n\
%T is a synonym for \"%H:%M:%S\".\n\
%U is replaced by the week of the year (01-52), first day of week is Sunday.\n\
%w is replaced by the day of week (0-6), Sunday is day 0.\n\
%W is replaced by the week of the year (01-52), first day of week is Monday.\n\
%x is a locale-specific synonym, which defaults to \"%D\" in the C locale.\n\
%X is a locale-specific synonym, which defaults to \"%T\" in the C locale.\n\
%y is replaced by the year without century (00-99).\n\
%Y is replaced by the year with century.\n\
%Z is replaced by the time zone abbreviation.\n\
\n\
The number of options reflects the `strftime' function.")
  (format_string, time)
     Lisp_Object format_string, time;
{
  time_t value;
  int size;

  CHECK_STRING (format_string, 1);

  if (! lisp_time_argument (time, &value))
    error ("Invalid time specification");

  /* This is probably enough.  */
  size = XSTRING (format_string)->size * 6 + 50;

  while (1)
    {
      char *buf = (char *) alloca (size);
      if (emacs_strftime (buf, size, XSTRING (format_string)->data,
			  localtime (&value)))
	return build_string (buf);
      /* If buffer was too small, make it bigger.  */
      size *= 2;
    }
}

DEFUN ("decode-time", Fdecode_time, Sdecode_time, 0, 1, 0,
  "Decode a time value as (SEC MINUTE HOUR DAY MONTH YEAR DOW DST ZONE).\n\
The optional SPECIFIED-TIME should be a list of (HIGH LOW . IGNORED)\n\
or (HIGH . LOW), as from `current-time' and `file-attributes', or `nil'\n\
to use the current time.  The list has the following nine members:\n\
SEC is an integer between 0 and 59.  MINUTE is an integer between 0 and 59.\n\
HOUR is an integer between 0 and 23.  DAY is an integer between 1 and 31.\n\
MONTH is an integer between 1 and 12.  YEAR is an integer indicating the\n\
four-digit year.  DOW is the day of week, an integer between 0 and 6, where\n\
0 is Sunday.  DST is t if daylight savings time is effect, otherwise nil.\n\
ZONE is an integer indicating the number of seconds east of Greenwich.\n\
(Note that Common Lisp has different meanings for DOW and ZONE.)")
  (specified_time)
     Lisp_Object specified_time;
{
  time_t time_spec;
  struct tm save_tm;
  struct tm *decoded_time;
  Lisp_Object list_args[9];
  
  if (! lisp_time_argument (specified_time, &time_spec))
    error ("Invalid time specification");

  decoded_time = localtime (&time_spec);
  XSETFASTINT (list_args[0], decoded_time->tm_sec);
  XSETFASTINT (list_args[1], decoded_time->tm_min);
  XSETFASTINT (list_args[2], decoded_time->tm_hour);
  XSETFASTINT (list_args[3], decoded_time->tm_mday);
  XSETFASTINT (list_args[4], decoded_time->tm_mon + 1);
  XSETFASTINT (list_args[5], decoded_time->tm_year + 1900);
  XSETFASTINT (list_args[6], decoded_time->tm_wday);
  list_args[7] = (decoded_time->tm_isdst)? Qt : Qnil;

  /* Make a copy, in case gmtime modifies the struct.  */
  save_tm = *decoded_time;
  decoded_time = gmtime (&time_spec);
  if (decoded_time == 0)
    list_args[8] = Qnil;
  else
    XSETINT (list_args[8], difftm (&save_tm, decoded_time));
  return Flist (9, list_args);
}

static char days_per_month[11]
  = { 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31 };

DEFUN ("encode-time", Fencode_time, Sencode_time, 6, 7, 0,
  "Convert SEC, MINUTE, HOUR, DAY, MONTH, YEAR and ZONE to internal time.\n\
This is the reverse operation of `decode-time', which see.  ZONE defaults\n\
to the current time zone and daylight savings time if not specified; if\n\
specified, it can be either a list (as from `current-time-zone') or an\n\
integer (as from `decode-time'), and is applied without consideration for\n\
daylight savings time.\n\
Year numbers less than 100 are treated just like other year numbers.\n\
If you them to stand for years above 1900, you must do that yourself.")
  (sec, minute, hour, day, month, year, zone)
     Lisp_Object sec, minute, hour, day, month, year, zone;
{
  time_t time;
  int fullyear, mon, days, seconds, tz = 0;

  CHECK_NATNUM (sec, 0);
  CHECK_NATNUM (minute, 1);
  CHECK_NATNUM (hour, 2);
  CHECK_NATNUM (day, 3);
  CHECK_NATNUM (month, 4);
  CHECK_NATNUM (year, 5);

  fullyear = XINT (year);

  /* Adjust incoming datespec to epoch = March 1, year 0.
     The "date" March 1, year 0, is an abstraction used purely for its
     computational convenience; year 0 never existed.  */
  mon = XINT (month) - 1 + 10;
  fullyear += mon/12 - 1;
  mon %= 12;

  days = XINT (day) - 1;		/* day of month */
  while (mon-- > 0)			/* day of year */
    days += days_per_month[mon];
  days += 146097 * (fullyear/400);	/* 400 years = 146097 days */
  fullyear %= 400;
  days += 36524 * (fullyear/100);	/* 100 years = 36524 days */
  fullyear %= 100;
  days += 1461 * (fullyear/4);		/* 4 years = 1461 days */
  fullyear %= 4;
  days += 365 * fullyear;		/* 1 year = 365 days */

  /* Adjust computed datespec to epoch = January 1, 1970.  */
  days += 59;				/* March 1 is 59th day.  */
  days -= 719527;			/* 1970 years = 719527 days */

  seconds = XINT (sec) + 60 * XINT (minute) + 3600 * XINT (hour);

  if (sizeof (time_t) == 4
      && ((days+(seconds/86400) > 24854) || (days+(seconds/86400) < -24854)))
    error ("the specified time is outside the representable range");

  time = days * 86400 + seconds;

  /* We have the correct value for UTC.  Adjust for timezones.  */
  if (NILP (zone))
    {
      struct tm gmt, *t;
      time_t adjusted_time;
      int adjusted_tz;
      /* If the system does not use timezones, gmtime returns 0, and we
	 already have the correct value, by definition.  */
      if ((t = gmtime (&time)) != 0)
	{
	  gmt = *t;
	  t = localtime (&time);
	  tz = difftm (t, &gmt);
	  /* The timezone returned is that at the specified Universal Time,
	     not the local time, which is what we want.  Adjust, repeat.  */
	  adjusted_time = time - tz;
	  gmt = *gmtime (&adjusted_time); /* this is safe now */
	  t = localtime (&adjusted_time);
	  adjusted_tz = difftm (t, &gmt);
	  /* In case of discrepancy, adjust again for extra accuracy.  */
	  if (adjusted_tz != tz)
	    {
	      adjusted_time = time - adjusted_tz;
	      gmt = *gmtime (&adjusted_time);
	      t = localtime (&adjusted_time);
	      adjusted_tz = difftm (t, &gmt);
	    }
	  tz = adjusted_tz;
	}
    }
  else 
    {
      if (CONSP (zone))
	zone = Fcar (zone);
      CHECK_NUMBER (zone, 6);
      tz = XINT (zone);
    }

  return make_time (time - tz);
}

DEFUN ("current-time-string", Fcurrent_time_string, Scurrent_time_string, 0, 1, 0,
  "Return the current time, as a human-readable string.\n\
Programs can use this function to decode a time,\n\
since the number of columns in each field is fixed.\n\
The format is `Sun Sep 16 01:03:52 1973'.\n\
If an argument is given, it specifies a time to format\n\
instead of the current time.  The argument should have the form:\n\
  (HIGH . LOW)\n\
or the form:\n\
  (HIGH LOW . IGNORED).\n\
Thus, you can use times obtained from `current-time'\n\
and from `file-attributes'.")
  (specified_time)
     Lisp_Object specified_time;
{
  time_t value;
  char buf[30];
  register char *tem;

  if (! lisp_time_argument (specified_time, &value))
    value = -1;
  tem = (char *) ctime (&value);

  strncpy (buf, tem, 24);
  buf[24] = 0;

  return build_string (buf);
}

#define TM_YEAR_ORIGIN 1900

/* Yield A - B, measured in seconds.  */
static long
difftm (a, b)
     struct tm *a, *b;
{
  int ay = a->tm_year + (TM_YEAR_ORIGIN - 1);
  int by = b->tm_year + (TM_YEAR_ORIGIN - 1);
  /* Some compilers can't handle this as a single return statement.  */
  long days = (
	      /* difference in day of year */
	      a->tm_yday - b->tm_yday
	      /* + intervening leap days */
	      +  ((ay >> 2) - (by >> 2))
	      -  (ay/100 - by/100)
	      +  ((ay/100 >> 2) - (by/100 >> 2))
	      /* + difference in years * 365 */
	      +  (long)(ay-by) * 365
	      );
  return (60*(60*(24*days + (a->tm_hour - b->tm_hour))
	      + (a->tm_min - b->tm_min))
	  + (a->tm_sec - b->tm_sec));
}

DEFUN ("current-time-zone", Fcurrent_time_zone, Scurrent_time_zone, 0, 1, 0,
  "Return the offset and name for the local time zone.\n\
This returns a list of the form (OFFSET NAME).\n\
OFFSET is an integer number of seconds ahead of UTC (east of Greenwich).\n\
    A negative value means west of Greenwich.\n\
NAME is a string giving the name of the time zone.\n\
If an argument is given, it specifies when the time zone offset is determined\n\
instead of using the current time.  The argument should have the form:\n\
  (HIGH . LOW)\n\
or the form:\n\
  (HIGH LOW . IGNORED).\n\
Thus, you can use times obtained from `current-time'\n\
and from `file-attributes'.\n\
\n\
Some operating systems cannot provide all this information to Emacs;\n\
in this case, `current-time-zone' returns a list containing nil for\n\
the data it can't find.")
  (specified_time)
     Lisp_Object specified_time;
{
  time_t value;
  struct tm *t;

  if (lisp_time_argument (specified_time, &value)
      && (t = gmtime (&value)) != 0)
    {
      struct tm gmt;
      long offset;
      char *s, buf[6];

      gmt = *t;		/* Make a copy, in case localtime modifies *t.  */
      t = localtime (&value);
      offset = difftm (t, &gmt);
      s = 0;
#ifdef HAVE_TM_ZONE
      if (t->tm_zone)
	s = (char *)t->tm_zone;
#else /* not HAVE_TM_ZONE */
#ifdef HAVE_TZNAME
      if (t->tm_isdst == 0 || t->tm_isdst == 1)
	s = tzname[t->tm_isdst];
#endif
#endif /* not HAVE_TM_ZONE */
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
    return Fmake_list (2, Qnil);
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
  "Insert the arguments, either strings or characters, at point.\n\
Point moves forward so that it ends up after the inserted text.\n\
Any other markers at the point of insertion remain before the text.")
  (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  register int argnum;
  register Lisp_Object tem;
  char str[1];

  for (argnum = 0; argnum < nargs; argnum++)
    {
      tem = args[argnum];
    retry:
      if (INTEGERP (tem))
	{
	  str[0] = XINT (tem);
	  insert (str, 1);
	}
      else if (STRINGP (tem))
	{
	  insert_from_string (tem, 0, XSTRING (tem)->size, 0);
	}
      else
	{
	  tem = wrong_type_argument (Qchar_or_string_p, tem);
	  goto retry;
	}
    }

  return Qnil;
}

DEFUN ("insert-and-inherit", Finsert_and_inherit, Sinsert_and_inherit,
   0, MANY, 0,
  "Insert the arguments at point, inheriting properties from adjoining text.\n\
Point moves forward so that it ends up after the inserted text.\n\
Any other markers at the point of insertion remain before the text.")
  (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  register int argnum;
  register Lisp_Object tem;
  char str[1];

  for (argnum = 0; argnum < nargs; argnum++)
    {
      tem = args[argnum];
    retry:
      if (INTEGERP (tem))
	{
	  str[0] = XINT (tem);
	  insert_and_inherit (str, 1);
	}
      else if (STRINGP (tem))
	{
	  insert_from_string (tem, 0, XSTRING (tem)->size, 1);
	}
      else
	{
	  tem = wrong_type_argument (Qchar_or_string_p, tem);
	  goto retry;
	}
    }

  return Qnil;
}

DEFUN ("insert-before-markers", Finsert_before_markers, Sinsert_before_markers, 0, MANY, 0,
  "Insert strings or characters at point, relocating markers after the text.\n\
Point moves forward so that it ends up after the inserted text.\n\
Any other markers at the point of insertion also end up after the text.")
  (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  register int argnum;
  register Lisp_Object tem;
  char str[1];

  for (argnum = 0; argnum < nargs; argnum++)
    {
      tem = args[argnum];
    retry:
      if (INTEGERP (tem))
	{
	  str[0] = XINT (tem);
	  insert_before_markers (str, 1);
	}
      else if (STRINGP (tem))
	{
	  insert_from_string_before_markers (tem, 0, XSTRING (tem)->size, 0);
	}
      else
	{
	  tem = wrong_type_argument (Qchar_or_string_p, tem);
	  goto retry;
	}
    }

  return Qnil;
}

DEFUN ("insert-before-markers-and-inherit",
  Finsert_and_inherit_before_markers, Sinsert_and_inherit_before_markers,
  0, MANY, 0,
  "Insert text at point, relocating markers and inheriting properties.\n\
Point moves forward so that it ends up after the inserted text.\n\
Any other markers at the point of insertion also end up after the text.")
  (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  register int argnum;
  register Lisp_Object tem;
  char str[1];

  for (argnum = 0; argnum < nargs; argnum++)
    {
      tem = args[argnum];
    retry:
      if (INTEGERP (tem))
	{
	  str[0] = XINT (tem);
	  insert_before_markers_and_inherit (str, 1);
	}
      else if (STRINGP (tem))
	{
	  insert_from_string_before_markers (tem, 0, XSTRING (tem)->size, 1);
	}
      else
	{
	  tem = wrong_type_argument (Qchar_or_string_p, tem);
	  goto retry;
	}
    }

  return Qnil;
}

DEFUN ("insert-char", Finsert_char, Sinsert_char, 2, 3, 0,
  "Insert COUNT (second arg) copies of CHAR (first arg).\n\
Point and all markers are affected as in the function `insert'.\n\
Both arguments are required.\n\
The optional third arg INHERIT, if non-nil, says to inherit text properties\n\
from adjoining text, if those properties are sticky.")
  (chr, count, inherit)
       Lisp_Object chr, count, inherit;
{
  register unsigned char *string;
  register int strlen;
  register int i, n;

  CHECK_NUMBER (chr, 0);
  CHECK_NUMBER (count, 1);

  n = XINT (count);
  if (n <= 0)
    return Qnil;
  strlen = min (n, 256);
  string = (unsigned char *) alloca (strlen);
  for (i = 0; i < strlen; i++)
    string[i] = XFASTINT (chr);
  while (n >= strlen)
    {
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
   have them.

   We don't want to use plain old make_string here, because it calls
   make_uninit_string, which can cause the buffer arena to be
   compacted.  make_string has no way of knowing that the data has
   been moved, and thus copies the wrong data into the string.  This
   doesn't effect most of the other users of make_string, so it should
   be left as is.  But we should use this function when conjuring
   buffer substrings.  */

Lisp_Object
make_buffer_string (start, end)
     int start, end;
{
  Lisp_Object result, tem, tem1;

  if (start < GPT && GPT < end)
    move_gap (start);

  result = make_uninit_string (end - start);
  bcopy (&FETCH_CHAR (start), XSTRING (result)->data, end - start);

  tem = Fnext_property_change (make_number (start), Qnil, make_number (end));
  tem1 = Ftext_properties_at (make_number (start), Qnil);

#ifdef USE_TEXT_PROPERTIES
  if (XINT (tem) != end || !NILP (tem1))
    copy_intervals_to_string (result, current_buffer, start, end - start);
#endif

  return result;
}

DEFUN ("buffer-substring", Fbuffer_substring, Sbuffer_substring, 2, 2, 0,
  "Return the contents of part of the current buffer as a string.\n\
The two arguments START and END are character positions;\n\
they can be in either order.")
  (b, e)
     Lisp_Object b, e;
{
  register int beg, end;

  validate_region (&b, &e);
  beg = XINT (b);
  end = XINT (e);

  return make_buffer_string (beg, end);
}

DEFUN ("buffer-string", Fbuffer_string, Sbuffer_string, 0, 0, 0,
  "Return the contents of the current buffer as a string.\n\
If narrowing is in effect, this function returns only the visible part\n\
of the buffer.")
  ()
{
  return make_buffer_string (BEGV, ZV);
}

DEFUN ("insert-buffer-substring", Finsert_buffer_substring, Sinsert_buffer_substring,
  1, 3, 0,
  "Insert before point a substring of the contents of buffer BUFFER.\n\
BUFFER may be a buffer or a buffer name.\n\
Arguments START and END are character numbers specifying the substring.\n\
They default to the beginning and the end of BUFFER.")
  (buf, b, e)
     Lisp_Object buf, b, e;
{
  register int beg, end, temp;
  register struct buffer *bp;
  Lisp_Object buffer;

  buffer = Fget_buffer (buf);
  if (NILP (buffer))
    nsberror (buf);
  bp = XBUFFER (buffer);

  if (NILP (b))
    beg = BUF_BEGV (bp);
  else
    {
      CHECK_NUMBER_COERCE_MARKER (b, 0);
      beg = XINT (b);
    }
  if (NILP (e))
    end = BUF_ZV (bp);
  else
    {
      CHECK_NUMBER_COERCE_MARKER (e, 1);
      end = XINT (e);
    }

  if (beg > end)
    temp = beg, beg = end, end = temp;

  if (!(BUF_BEGV (bp) <= beg && end <= BUF_ZV (bp)))
    args_out_of_range (b, e);

  insert_from_buffer (bp, beg, end - beg, 0);
  return Qnil;
}

DEFUN ("compare-buffer-substrings", Fcompare_buffer_substrings, Scompare_buffer_substrings,
  6, 6, 0,
  "Compare two substrings of two buffers; return result as number.\n\
the value is -N if first string is less after N-1 chars,\n\
+N if first string is greater after N-1 chars, or 0 if strings match.\n\
Each substring is represented as three arguments: BUFFER, START and END.\n\
That makes six args in all, three for each substring.\n\n\
The value of `case-fold-search' in the current buffer\n\
determines whether case is significant or ignored.")
  (buffer1, start1, end1, buffer2, start2, end2)
     Lisp_Object buffer1, start1, end1, buffer2, start2, end2;
{
  register int begp1, endp1, begp2, endp2, temp, len1, len2, length, i;
  register struct buffer *bp1, *bp2;
  register unsigned char *trt
    = (!NILP (current_buffer->case_fold_search)
       ? XSTRING (current_buffer->case_canon_table)->data : 0);

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
    }

  if (NILP (start1))
    begp1 = BUF_BEGV (bp1);
  else
    {
      CHECK_NUMBER_COERCE_MARKER (start1, 1);
      begp1 = XINT (start1);
    }
  if (NILP (end1))
    endp1 = BUF_ZV (bp1);
  else
    {
      CHECK_NUMBER_COERCE_MARKER (end1, 2);
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
      bp2 = XBUFFER (buffer2);
    }

  if (NILP (start2))
    begp2 = BUF_BEGV (bp2);
  else
    {
      CHECK_NUMBER_COERCE_MARKER (start2, 4);
      begp2 = XINT (start2);
    }
  if (NILP (end2))
    endp2 = BUF_ZV (bp2);
  else
    {
      CHECK_NUMBER_COERCE_MARKER (end2, 5);
      endp2 = XINT (end2);
    }

  if (begp2 > endp2)
    temp = begp2, begp2 = endp2, endp2 = temp;

  if (!(BUF_BEGV (bp2) <= begp2
	&& begp2 <= endp2
        && endp2 <= BUF_ZV (bp2)))
    args_out_of_range (start2, end2);

  len1 = endp1 - begp1;
  len2 = endp2 - begp2;
  length = len1;
  if (len2 < length)
    length = len2;

  for (i = 0; i < length; i++)
    {
      int c1 = *BUF_CHAR_ADDRESS (bp1, begp1 + i);
      int c2 = *BUF_CHAR_ADDRESS (bp2, begp2 + i);
      if (trt)
	{
	  c1 = trt[c1];
	  c2 = trt[c2];
	}
      if (c1 < c2)
	return make_number (- 1 - i);
      if (c1 > c2)
	return make_number (i + 1);
    }

  /* The strings match as far as they go.
     If one is shorter, that one is less.  */
  if (length < len1)
    return make_number (length + 1);
  else if (length < len2)
    return make_number (- length - 1);

  /* Same length too => they are equal.  */
  return make_number (0);
}

static Lisp_Object
subst_char_in_region_unwind (arg)
     Lisp_Object arg;
{
  return current_buffer->undo_list = arg;
}

DEFUN ("subst-char-in-region", Fsubst_char_in_region,
  Ssubst_char_in_region, 4, 5, 0,
  "From START to END, replace FROMCHAR with TOCHAR each time it occurs.\n\
If optional arg NOUNDO is non-nil, don't record this change for undo\n\
and don't mark the buffer as really changed.")
  (start, end, fromchar, tochar, noundo)
     Lisp_Object start, end, fromchar, tochar, noundo;
{
  register int pos, stop, look;
  int changed = 0;
  int count = specpdl_ptr - specpdl;

  validate_region (&start, &end);
  CHECK_NUMBER (fromchar, 2);
  CHECK_NUMBER (tochar, 3);

  pos = XINT (start);
  stop = XINT (end);
  look = XINT (fromchar);

  /* If we don't want undo, turn off putting stuff on the list.
     That's faster than getting rid of things,
     and it prevents even the entry for a first change.  */
  if (!NILP (noundo))
    {
      record_unwind_protect (subst_char_in_region_unwind,
			     current_buffer->undo_list);
      current_buffer->undo_list = Qt;
    }

  while (pos < stop)
    {
      if (FETCH_CHAR (pos) == look)
	{
	  if (! changed)
	    {
	      modify_region (current_buffer, XINT (start), stop);

	      if (! NILP (noundo))
		{
		  if (MODIFF - 1 == SAVE_MODIFF)
		    SAVE_MODIFF++;
		  if (MODIFF - 1 == current_buffer->auto_save_modified)
		    current_buffer->auto_save_modified++;
		}

 	      changed = 1;
	    }

	  if (NILP (noundo))
	    record_change (pos, 1);
	  FETCH_CHAR (pos) = XINT (tochar);
	}
      pos++;
    }

  if (changed)
    signal_after_change (XINT (start),
			 stop - XINT (start), stop - XINT (start));

  unbind_to (count, Qnil);
  return Qnil;
}

DEFUN ("translate-region", Ftranslate_region, Stranslate_region, 3, 3, 0,
  "From START to END, translate characters according to TABLE.\n\
TABLE is a string; the Nth character in it is the mapping\n\
for the character with code N.  Returns the number of characters changed.")
  (start, end, table)
     Lisp_Object start;
     Lisp_Object end;
     register Lisp_Object table;
{
  register int pos, stop;	/* Limits of the region. */
  register unsigned char *tt;	/* Trans table. */
  register int oc;		/* Old character. */
  register int nc;		/* New character. */
  int cnt;			/* Number of changes made. */
  Lisp_Object z;		/* Return. */
  int size;			/* Size of translate table. */

  validate_region (&start, &end);
  CHECK_STRING (table, 2);

  size = XSTRING (table)->size;
  tt = XSTRING (table)->data;

  pos = XINT (start);
  stop = XINT (end);
  modify_region (current_buffer, pos, stop);

  cnt = 0;
  for (; pos < stop; ++pos)
    {
      oc = FETCH_CHAR (pos);
      if (oc < size)
	{
	  nc = tt[oc];
	  if (nc != oc)
	    {
	      record_change (pos, 1);
	      FETCH_CHAR (pos) = nc;
	      signal_after_change (pos, 1, 1);
	      ++cnt;
	    }
	}
    }

  XSETFASTINT (z, cnt);
  return (z);
}

DEFUN ("delete-region", Fdelete_region, Sdelete_region, 2, 2, "r",
  "Delete the text between point and mark.\n\
When called from a program, expects two arguments,\n\
positions (integers or markers) specifying the stretch to be deleted.")
  (b, e)
     Lisp_Object b, e;
{
  validate_region (&b, &e);
  del_range (XINT (b), XINT (e));
  return Qnil;
}

DEFUN ("widen", Fwiden, Swiden, 0, 0, "",
  "Remove restrictions (narrowing) from current buffer.\n\
This allows the buffer's full text to be seen and edited.")
  ()
{
  BEGV = BEG;
  SET_BUF_ZV (current_buffer, Z);
  clip_changed = 1;
  /* Changing the buffer bounds invalidates any recorded current column.  */
  invalidate_current_column ();
  return Qnil;
}

DEFUN ("narrow-to-region", Fnarrow_to_region, Snarrow_to_region, 2, 2, "r",
  "Restrict editing in this buffer to the current region.\n\
The rest of the text becomes temporarily invisible and untouchable\n\
but is not deleted; if you save the buffer in a file, the invisible\n\
text is included in the file.  \\[widen] makes all visible again.\n\
See also `save-restriction'.\n\
\n\
When calling from a program, pass two arguments; positions (integers\n\
or markers) bounding the text that should remain visible.")
  (b, e)
     register Lisp_Object b, e;
{
  CHECK_NUMBER_COERCE_MARKER (b, 0);
  CHECK_NUMBER_COERCE_MARKER (e, 1);

  if (XINT (b) > XINT (e))
    {
      Lisp_Object tem;
      tem = b;  b = e;  e = tem;
    }

  if (!(BEG <= XINT (b) && XINT (b) <= XINT (e) && XINT (e) <= Z))
    args_out_of_range (b, e);

  BEGV = XFASTINT (b);
  SET_BUF_ZV (current_buffer, XFASTINT (e));
  if (point < XFASTINT (b))
    SET_PT (XFASTINT (b));
  if (point > XFASTINT (e))
    SET_PT (XFASTINT (e));
  clip_changed = 1;
  /* Changing the buffer bounds invalidates any recorded current column.  */
  invalidate_current_column ();
  return Qnil;
}

Lisp_Object
save_restriction_save ()
{
  register Lisp_Object bottom, top;
  /* Note: I tried using markers here, but it does not win
     because insertion at the end of the saved region
     does not advance mh and is considered "outside" the saved region. */
  XSETFASTINT (bottom, BEGV - BEG);
  XSETFASTINT (top, Z - ZV);

  return Fcons (Fcurrent_buffer (), Fcons (bottom, top));
}

Lisp_Object
save_restriction_restore (data)
     Lisp_Object data;
{
  register struct buffer *buf;
  register int newhead, newtail;
  register Lisp_Object tem;

  buf = XBUFFER (XCONS (data)->car);

  data = XCONS (data)->cdr;

  tem = XCONS (data)->car;
  newhead = XINT (tem);
  tem = XCONS (data)->cdr;
  newtail = XINT (tem);
  if (newhead + newtail > BUF_Z (buf) - BUF_BEG (buf))
    {
      newhead = 0;
      newtail = 0;
    }
  BUF_BEGV (buf) = BUF_BEG (buf) + newhead;
  SET_BUF_ZV (buf, BUF_Z (buf) - newtail);
  clip_changed = 1;

  /* If point is outside the new visible range, move it inside. */
  SET_BUF_PT (buf,
	      clip_to_bounds (BUF_BEGV (buf), BUF_PT (buf), BUF_ZV (buf)));

  return Qnil;
}

DEFUN ("save-restriction", Fsave_restriction, Ssave_restriction, 0, UNEVALLED, 0,
  "Execute BODY, saving and restoring current buffer's restrictions.\n\
The buffer's restrictions make parts of the beginning and end invisible.\n\
\(They are set up with `narrow-to-region' and eliminated with `widen'.)\n\
This special form, `save-restriction', saves the current buffer's restrictions\n\
when it is entered, and restores them when it is exited.\n\
So any `narrow-to-region' within BODY lasts only until the end of the form.\n\
The old restrictions settings are restored\n\
even in case of abnormal exit (throw or error).\n\
\n\
The value returned is the value of the last form in BODY.\n\
\n\
`save-restriction' can get confused if, within the BODY, you widen\n\
and then make changes outside the area within the saved restrictions.\n\
\n\
Note: if you are using both `save-excursion' and `save-restriction',\n\
use `save-excursion' outermost:\n\
    (save-excursion (save-restriction ...))")
  (body)
     Lisp_Object body;
{
  register Lisp_Object val;
  int count = specpdl_ptr - specpdl;

  record_unwind_protect (save_restriction_restore, save_restriction_save ());
  val = Fprogn (body);
  return unbind_to (count, val);
}

/* Buffer for the most recent text displayed by Fmessage.  */
static char *message_text;

/* Allocated length of that buffer.  */
static int message_length;

DEFUN ("message", Fmessage, Smessage, 1, MANY, 0,
  "Print a one-line message at the bottom of the screen.\n\
The first argument is a control string.\n\
It may contain %s or %d or %c to print successive following arguments.\n\
%s means print an argument as a string, %d means print as number in decimal,\n\
%c means print a number as a single character.\n\
The argument used by %s must be a string or a symbol;\n\
the argument used by %d or %c must be a number.\n\
If the first argument is nil, clear any existing message; let the\n\
minibuffer contents show.")
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
      /* Copy the data so that it won't move when we GC.  */
      if (! message_text)
	{
	  message_text = (char *)xmalloc (80);
	  message_length = 80;
	}
      if (XSTRING (val)->size > message_length)
	{
	  message_length = XSTRING (val)->size;
	  message_text = (char *)xrealloc (message_text, message_length);
	}
      bcopy (XSTRING (val)->data, message_text, XSTRING (val)->size);
      message2 (message_text, XSTRING (val)->size);
      return val;
    }
}

DEFUN ("message-box", Fmessage_box, Smessage_box, 1, MANY, 0,
  "Display a message, in a dialog box if possible.\n\
If a dialog box is not available, use the echo area.\n\
The first argument is a control string.\n\
It may contain %s or %d or %c to print successive following arguments.\n\
%s means print an argument as a string, %d means print as number in decimal,\n\
%c means print a number as a single character.\n\
The argument used by %s must be a string or a symbol;\n\
the argument used by %d or %c must be a number.\n\
If the first argument is nil, clear any existing message; let the\n\
minibuffer contents show.")
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
#ifdef HAVE_X_MENU
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
#else
      /* Copy the data so that it won't move when we GC.  */
      if (! message_text)
	{
	  message_text = (char *)xmalloc (80);
	  message_length = 80;
	}
      if (XSTRING (val)->size > message_length)
	{
	  message_length = XSTRING (val)->size;
	  message_text = (char *)xrealloc (message_text, message_length);
	}
      bcopy (XSTRING (val)->data, message_text, XSTRING (val)->size);
      message2 (message_text, XSTRING (val)->size);
      return val;
#endif
    }
}
#ifdef HAVE_X_MENU
extern Lisp_Object last_nonmenu_event;
#endif
DEFUN ("message-or-box", Fmessage_or_box, Smessage_or_box, 1, MANY, 0,
  "Display a message in a dialog box or in the echo area.\n\
If this command was invoked with the mouse, use a dialog box.\n\
Otherwise, use the echo area.\n\
\n\
The first argument is a control string.\n\
It may contain %s or %d or %c to print successive following arguments.\n\
%s means print an argument as a string, %d means print as number in decimal,\n\
%c means print a number as a single character.\n\
The argument used by %s must be a string or a symbol;\n\
the argument used by %d or %c must be a number.\n\
If the first argument is nil, clear any existing message; let the\n\
minibuffer contents show.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
#ifdef HAVE_X_MENU
  if (NILP (last_nonmenu_event) || CONSP (last_nonmenu_event))
    return Fmessage_box (nargs, args);
#endif
  return Fmessage (nargs, args);
}

DEFUN ("format", Fformat, Sformat, 1, MANY, 0,
  "Format a string out of a control-string and arguments.\n\
The first argument is a control string.\n\
The other arguments are substituted into it to make the result, a string.\n\
It may contain %-sequences meaning to substitute the next argument.\n\
%s means print a string argument.  Actually, prints any object, with `princ'.\n\
%d means print as number in decimal (%o octal, %x hex).\n\
%c means print a number as a single character.\n\
%S means print any object as an s-expression (using prin1).\n\
  The argument used for %d, %o, %x or %c must be a number.\n\
Use %% to put a single % into the output.")
  (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  register int n;		/* The number of the next arg to substitute */
  register int total = 5;	/* An estimate of the final length */
  char *buf;
  register unsigned char *format, *end;
  int length;
  extern char *index ();
  /* It should not be necessary to GCPRO ARGS, because
     the caller in the interpreter should take care of that.  */

  CHECK_STRING (args[0], 0);
  format = XSTRING (args[0])->data;
  end = format + XSTRING (args[0])->size;

  n = 0;
  while (format != end)
    if (*format++ == '%')
      {
	int minlen;

	/* Process a numeric arg and skip it.  */
	minlen = atoi (format);
	if (minlen > 0)
	  total += minlen;
	else
	  total -= minlen;
	while ((*format >= '0' && *format <= '9')
	       || *format == '-' || *format == ' ' || *format == '.')
	  format++;

	if (*format == '%')
	  format++;
	else if (++n >= nargs)
	  error ("not enough arguments for format string");
	else if (*format == 'S')
	  {
	    /* For `S', prin1 the argument and then treat like a string.  */
	    register Lisp_Object tem;
	    tem = Fprin1_to_string (args[n], Qnil);
	    args[n] = tem;
	    goto string;
	  }
	else if (SYMBOLP (args[n]))
	  {
	    XSETSTRING (args[n], XSYMBOL (args[n])->name);
	    goto string;
	  }
	else if (STRINGP (args[n]))
	  {
	  string:
	    if (*format != 's' && *format != 'S')
	      error ("format specifier doesn't match argument type");
	    total += XSTRING (args[n])->size;
	  }
	/* Would get MPV otherwise, since Lisp_Int's `point' to low memory.  */
	else if (INTEGERP (args[n]) && *format != 's')
	  {
#ifdef LISP_FLOAT_TYPE
	    /* The following loop assumes the Lisp type indicates
	       the proper way to pass the argument.
	       So make sure we have a flonum if the argument should
	       be a double.  */
	    if (*format == 'e' || *format == 'f' || *format == 'g')
	      args[n] = Ffloat (args[n]);
#endif
	    total += 30;
	  }
#ifdef LISP_FLOAT_TYPE
	else if (FLOATP (args[n]) && *format != 's')
	  {
	    if (! (*format == 'e' || *format == 'f' || *format == 'g'))
	      args[n] = Ftruncate (args[n]);
	    total += 30;
	  }
#endif
	else
	  {
	    /* Anything but a string, convert to a string using princ.  */
	    register Lisp_Object tem;
	    tem = Fprin1_to_string (args[n], Qt);
	    args[n] = tem;
	    goto string;
	  }
      }

  {
    register int nstrings = n + 1;

    /* Allocate twice as many strings as we have %-escapes; floats occupy
       two slots, and we're not sure how many of those we have.  */
    register unsigned char **strings
      = (unsigned char **) alloca (2 * nstrings * sizeof (unsigned char *));
    int i;

    i = 0;
    for (n = 0; n < nstrings; n++)
      {
	if (n >= nargs)
	  strings[i++] = (unsigned char *) "";
	else if (INTEGERP (args[n]))
	  /* We checked above that the corresponding format effector
	     isn't %s, which would cause MPV.  */
	  strings[i++] = (unsigned char *) XINT (args[n]);
#ifdef LISP_FLOAT_TYPE
	else if (FLOATP (args[n]))
	  {
	    union { double d; int half[2]; } u;

	    u.d = XFLOAT (args[n])->data;
	    strings[i++] = (unsigned char *) (EMACS_INT) u.half[0];
	    strings[i++] = (unsigned char *) (EMACS_INT) u.half[1];
	  }
#endif
	else
	  strings[i++] = XSTRING (args[n])->data;
      }

    /* Format it in bigger and bigger buf's until it all fits. */
    while (1)
      {
	buf = (char *) alloca (total + 1);
	buf[total - 1] = 0;

	length = doprnt (buf, total + 1, strings[0], end, i-1, strings + 1);
	if (buf[total - 1] == 0)
	  break;

	total *= 2;
      }
  }

  /*   UNGCPRO;  */
  return make_string (buf, length);
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
  doprnt (buf, sizeof buf, string1, 0, 5, args);
#else
  doprnt (buf, sizeof buf, string1, 0, 5, &string1 + 1);
#endif
  return build_string (buf);
}

DEFUN ("char-equal", Fchar_equal, Schar_equal, 2, 2, 0,
  "Return t if two characters match, optionally ignoring case.\n\
Both arguments must be characters (i.e. integers).\n\
Case is ignored if `case-fold-search' is non-nil in the current buffer.")
  (c1, c2)
     register Lisp_Object c1, c2;
{
  unsigned char *downcase = DOWNCASE_TABLE;
  CHECK_NUMBER (c1, 0);
  CHECK_NUMBER (c2, 1);

  if (!NILP (current_buffer->case_fold_search)
      ? (downcase[0xff & XFASTINT (c1)] == downcase[0xff & XFASTINT (c2)]
	 && (XFASTINT (c1) & ~0xff) == (XFASTINT (c2) & ~0xff))
      : XINT (c1) == XINT (c2))
    return Qt;
  return Qnil;
}

/* Transpose the markers in two regions of the current buffer, and
   adjust the ones between them if necessary (i.e.: if the regions
   differ in size).

   Traverses the entire marker list of the buffer to do so, adding an
   appropriate amount to some, subtracting from some, and leaving the
   rest untouched.  Most of this is copied from adjust_markers in insdel.c.
  
   It's the caller's job to see that (start1 <= end1 <= start2 <= end2).  */

void
transpose_markers (start1, end1, start2, end2)
     register int start1, end1, start2, end2;
{
  register int amt1, amt2, diff, mpos;
  register Lisp_Object marker;

  /* Update point as if it were a marker.  */
  if (PT < start1)
    ;
  else if (PT < end1)
    TEMP_SET_PT (PT + (end2 - end1));
  else if (PT < start2)
    TEMP_SET_PT (PT + (end2 - start2) - (end1 - start1));
  else if (PT < end2)
    TEMP_SET_PT (PT - (start2 - start1));

  /* We used to adjust the endpoints here to account for the gap, but that
     isn't good enough.  Even if we assume the caller has tried to move the
     gap out of our way, it might still be at start1 exactly, for example;
     and that places it `inside' the interval, for our purposes.  The amount
     of adjustment is nontrivial if there's a `denormalized' marker whose
     position is between GPT and GPT + GAP_SIZE, so it's simpler to leave
     the dirty work to Fmarker_position, below.  */

  /* The difference between the region's lengths */
  diff = (end2 - start2) - (end1 - start1);
  
  /* For shifting each marker in a region by the length of the other
   * region plus the distance between the regions.
   */
  amt1 = (end2 - start2) + (start2 - end1);
  amt2 = (end1 - start1) + (start2 - end1);

  for (marker = BUF_MARKERS (current_buffer); !NILP (marker);
       marker = XMARKER (marker)->chain)
    {
      mpos = Fmarker_position (marker);
      if (mpos >= start1 && mpos < end2)
	{
	  if (mpos < end1)
	    mpos += amt1;
	  else if (mpos < start2)
	    mpos += diff;
	  else
	    mpos -= amt2;
	  if (mpos > GPT) mpos += GAP_SIZE;
	  XMARKER (marker)->bufpos = mpos;
	}
    }
}

DEFUN ("transpose-regions", Ftranspose_regions, Stranspose_regions, 4, 5, 0,
       "Transpose region START1 to END1 with START2 to END2.\n\
The regions may not be overlapping, because the size of the buffer is\n\
never changed in a transposition.\n\
\n\
Optional fifth arg LEAVE_MARKERS, if non-nil, means don't transpose\n\
any markers that happen to be located in the regions.\n\
\n\
Transposing beyond buffer boundaries is an error.")
  (startr1, endr1, startr2, endr2, leave_markers)
     Lisp_Object startr1, endr1, startr2, endr2, leave_markers;
{
  register int start1, end1, start2, end2,
  gap, len1, len_mid, len2;
  unsigned char *start1_addr, *start2_addr, *temp;

#ifdef USE_TEXT_PROPERTIES
  INTERVAL cur_intv, tmp_interval1, tmp_interval_mid, tmp_interval2;
  cur_intv = BUF_INTERVALS (current_buffer);
#endif /* USE_TEXT_PROPERTIES */

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
    error ("transposed regions not properly ordered");
  else if (start1 == end1 || start2 == end2)
    error ("transposed region may not be of length 0");

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
      
  /* Hmmm... how about checking to see if the gap is large
     enough to use as the temporary storage?  That would avoid an
     allocation... interesting.  Later, don't fool with it now.  */

  /* Working without memmove, for portability (sigh), so must be
     careful of overlapping subsections of the array...  */

  if (end1 == start2)		/* adjacent regions */
    {
      modify_region (current_buffer, start1, end2);
      record_change (start1, len1 + len2);

#ifdef USE_TEXT_PROPERTIES
      tmp_interval1 = copy_intervals (cur_intv, start1, len1);
      tmp_interval2 = copy_intervals (cur_intv, start2, len2);
      Fset_text_properties (start1, end2, Qnil, Qnil);
#endif /* USE_TEXT_PROPERTIES */

      /* First region smaller than second.  */
      if (len1 < len2)
        {
	  /* We use alloca only if it is small,
	     because we want to avoid stack overflow.  */
	  if (len2 > 20000)
	    temp = (unsigned char *) xmalloc (len2);
	  else
	    temp = (unsigned char *) alloca (len2);

	  /* Don't precompute these addresses.  We have to compute them
	     at the last minute, because the relocating allocator might
	     have moved the buffer around during the xmalloc.  */
	  start1_addr = BUF_CHAR_ADDRESS (current_buffer, start1);
	  start2_addr = BUF_CHAR_ADDRESS (current_buffer, start2);

          bcopy (start2_addr, temp, len2);
          bcopy (start1_addr, start1_addr + len2, len1);
          bcopy (temp, start1_addr, len2);
	  if (len2 > 20000)
	    free (temp);
        }
      else
	/* First region not smaller than second.  */
        {
	  if (len1 > 20000)
	    temp = (unsigned char *) xmalloc (len1);
	  else
	    temp = (unsigned char *) alloca (len1);
	  start1_addr = BUF_CHAR_ADDRESS (current_buffer, start1);
	  start2_addr = BUF_CHAR_ADDRESS (current_buffer, start2);
          bcopy (start1_addr, temp, len1);
          bcopy (start2_addr, start1_addr, len2);
          bcopy (temp, start1_addr + len2, len1);
	  if (len1 > 20000)
	    free (temp);
        }
#ifdef USE_TEXT_PROPERTIES
      graft_intervals_into_buffer (tmp_interval1, start1 + len2,
                                   len1, current_buffer, 0);
      graft_intervals_into_buffer (tmp_interval2, start1,
                                   len2, current_buffer, 0);
#endif /* USE_TEXT_PROPERTIES */
    }
  /* Non-adjacent regions, because end1 != start2, bleagh...  */
  else
    {
      if (len1 == len2)
	/* Regions are same size, though, how nice.  */
        {
          modify_region (current_buffer, start1, end1);
          modify_region (current_buffer, start2, end2);
          record_change (start1, len1);
          record_change (start2, len2);
#ifdef USE_TEXT_PROPERTIES
          tmp_interval1 = copy_intervals (cur_intv, start1, len1);
          tmp_interval2 = copy_intervals (cur_intv, start2, len2);
          Fset_text_properties (start1, end1, Qnil, Qnil);
          Fset_text_properties (start2, end2, Qnil, Qnil);
#endif /* USE_TEXT_PROPERTIES */

	  if (len1 > 20000)
	    temp = (unsigned char *) xmalloc (len1);
	  else
	    temp = (unsigned char *) alloca (len1);
	  start1_addr = BUF_CHAR_ADDRESS (current_buffer, start1);
	  start2_addr = BUF_CHAR_ADDRESS (current_buffer, start2);
          bcopy (start1_addr, temp, len1);
          bcopy (start2_addr, start1_addr, len2);
          bcopy (temp, start2_addr, len1);
	  if (len1 > 20000)
	    free (temp);
#ifdef USE_TEXT_PROPERTIES
          graft_intervals_into_buffer (tmp_interval1, start2,
                                       len1, current_buffer, 0);
          graft_intervals_into_buffer (tmp_interval2, start1,
                                       len2, current_buffer, 0);
#endif /* USE_TEXT_PROPERTIES */
        }

      else if (len1 < len2)	/* Second region larger than first */
        /* Non-adjacent & unequal size, area between must also be shifted.  */
        {
          len_mid = start2 - end1;
          modify_region (current_buffer, start1, end2);
          record_change (start1, (end2 - start1));
#ifdef USE_TEXT_PROPERTIES
          tmp_interval1 = copy_intervals (cur_intv, start1, len1);
          tmp_interval_mid = copy_intervals (cur_intv, end1, len_mid);
          tmp_interval2 = copy_intervals (cur_intv, start2, len2);
          Fset_text_properties (start1, end2, Qnil, Qnil);
#endif /* USE_TEXT_PROPERTIES */

	  /* holds region 2 */
	  if (len2 > 20000)
	    temp = (unsigned char *) xmalloc (len2);
	  else
	    temp = (unsigned char *) alloca (len2);
	  start1_addr = BUF_CHAR_ADDRESS (current_buffer, start1);
	  start2_addr = BUF_CHAR_ADDRESS (current_buffer, start2);
          bcopy (start2_addr, temp, len2);
          bcopy (start1_addr, start1_addr + len_mid + len2, len1);
          safe_bcopy (start1_addr + len1, start1_addr + len2, len_mid);
          bcopy (temp, start1_addr, len2);
	  if (len2 > 20000)
	    free (temp);
#ifdef USE_TEXT_PROPERTIES
          graft_intervals_into_buffer (tmp_interval1, end2 - len1,
                                       len1, current_buffer, 0);
          graft_intervals_into_buffer (tmp_interval_mid, start1 + len2,
                                       len_mid, current_buffer, 0);
          graft_intervals_into_buffer (tmp_interval2, start1,
                                       len2, current_buffer, 0);
#endif /* USE_TEXT_PROPERTIES */
        }
      else
	/* Second region smaller than first.  */
        {
          len_mid = start2 - end1;
          record_change (start1, (end2 - start1));
          modify_region (current_buffer, start1, end2);

#ifdef USE_TEXT_PROPERTIES
          tmp_interval1 = copy_intervals (cur_intv, start1, len1);
          tmp_interval_mid = copy_intervals (cur_intv, end1, len_mid);
          tmp_interval2 = copy_intervals (cur_intv, start2, len2);
          Fset_text_properties (start1, end2, Qnil, Qnil);
#endif /* USE_TEXT_PROPERTIES */

	  /* holds region 1 */
	  if (len1 > 20000)
	    temp = (unsigned char *) xmalloc (len1);
	  else
	    temp = (unsigned char *) alloca (len1);
	  start1_addr = BUF_CHAR_ADDRESS (current_buffer, start1);
	  start2_addr = BUF_CHAR_ADDRESS (current_buffer, start2);
          bcopy (start1_addr, temp, len1);
          bcopy (start2_addr, start1_addr, len2);
          bcopy (start1_addr + len1, start1_addr + len2, len_mid);
          bcopy (temp, start1_addr + len2 + len_mid, len1);
	  if (len1 > 20000)
	    free (temp);
#ifdef USE_TEXT_PROPERTIES
          graft_intervals_into_buffer (tmp_interval1, end2 - len1,
                                       len1, current_buffer, 0);
          graft_intervals_into_buffer (tmp_interval_mid, start1 + len2,
                                       len_mid, current_buffer, 0);
          graft_intervals_into_buffer (tmp_interval2, start1,
                                       len2, current_buffer, 0);
#endif /* USE_TEXT_PROPERTIES */
        }
    }

  /* todo: this will be slow, because for every transposition, we
     traverse the whole friggin marker list.  Possible solutions:
     somehow get a list of *all* the markers across multiple
     transpositions and do it all in one swell phoop.  Or maybe modify
     Emacs' marker code to keep an ordered list or tree.  This might
     be nicer, and more beneficial in the long run, but would be a
     bunch of work.  Plus the way they're arranged now is nice.  */
  if (NILP (leave_markers))
    {
      transpose_markers (start1, end1, start2, end2);
      fix_overlays_in_range (start1, end2);
    }

  return Qnil;
}


void
syms_of_editfns ()
{
  DEFVAR_LISP ("system-name", &Vsystem_name,
	       "The name of the machine Emacs is running on.");
  
  DEFVAR_LISP ("user-full-name", &Vuser_full_name,
	       "The full name of the user logged in.");

  DEFVAR_LISP ("user-name", &Vuser_name,
	       "The user's name, taken from environment variables if possible.");

  DEFVAR_LISP ("user-real-name", &Vuser_real_name,
	       "The user's name, based upon the real uid only.");

  defsubr (&Schar_equal);
  defsubr (&Sgoto_char);
  defsubr (&Sstring_to_char);
  defsubr (&Schar_to_string);
  defsubr (&Sbuffer_substring);
  defsubr (&Sbuffer_string);

  defsubr (&Spoint_marker);
  defsubr (&Smark_marker);
  defsubr (&Spoint);
  defsubr (&Sregion_beginning);
  defsubr (&Sregion_end);
/*  defsubr (&Smark); */
/*  defsubr (&Sset_mark); */
  defsubr (&Ssave_excursion);

  defsubr (&Sbufsize);
  defsubr (&Spoint_max);
  defsubr (&Spoint_min);
  defsubr (&Spoint_min_marker);
  defsubr (&Spoint_max_marker);

  defsubr (&Sbobp);
  defsubr (&Seobp);
  defsubr (&Sbolp);
  defsubr (&Seolp);
  defsubr (&Sfollowing_char);
  defsubr (&Sprevious_char);
  defsubr (&Schar_after);
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
  defsubr (&Sdecode_time);
  defsubr (&Sencode_time);
  defsubr (&Scurrent_time_string);
  defsubr (&Scurrent_time_zone);
  defsubr (&Ssystem_name);
  defsubr (&Smessage);
  defsubr (&Smessage_box);
  defsubr (&Smessage_or_box);
  defsubr (&Sformat);

  defsubr (&Sinsert_buffer_substring);
  defsubr (&Scompare_buffer_substrings);
  defsubr (&Ssubst_char_in_region);
  defsubr (&Stranslate_region);
  defsubr (&Sdelete_region);
  defsubr (&Swiden);
  defsubr (&Snarrow_to_region);
  defsubr (&Ssave_restriction);
  defsubr (&Stranspose_regions);
}
