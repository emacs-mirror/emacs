/* Copyright (C) 1991-2025 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   This file is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This file is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* If FPRINTFTIME is set to 1, this file defines a function with a
   'FILE *fp' parameter instead of two 'char *s, size_t max' parameters.  */
#ifndef FPRINTFTIME
# define FPRINTFTIME 0
#endif

/* If USE_C_LOCALE is set to 1, this file defines a function that uses the
   "C" locale, regardless of the current locale.  */
#ifndef USE_C_LOCALE
# define USE_C_LOCALE 0
#endif

#ifdef _LIBC
# define USE_IN_EXTENDED_LOCALE_MODEL 1
# define HAVE_STRUCT_ERA_ENTRY 1
# define HAVE_STRUCT_TM_TM_GMTOFF 1
# define HAVE_STRUCT_TM_TM_ZONE 1
# include "../locale/localeinfo.h"
#else
# include <libc-config.h>
# if FPRINTFTIME
#  include "fprintftime.h"
# else
#  include "strftime.h"
# endif
# include "time-internal.h"
#endif

/* Whether the system supports no localized output at all, that is, whether
   strftime's output does not depend on the current locale.  */
#if defined __ANDROID__
# define HAVE_ONLY_C_LOCALE 1
#else
# define HAVE_ONLY_C_LOCALE 0
#endif

/* Whether to require GNU behavior for AM and PM indicators, even on
   other platforms.  This matters only in non-C locales.
   The default is to require it; you can override this via
   AC_DEFINE([REQUIRE_GNUISH_STRFTIME_AM_PM], [false]) and if you do that
   you may be able to omit Gnulib's localename module and its dependencies.  */
#ifndef REQUIRE_GNUISH_STRFTIME_AM_PM
# define REQUIRE_GNUISH_STRFTIME_AM_PM true
#endif
#if HAVE_ONLY_C_LOCALE || USE_C_LOCALE
# undef REQUIRE_GNUISH_STRFTIME_AM_PM
# define REQUIRE_GNUISH_STRFTIME_AM_PM false
#endif

/* Whether to include support for non-Gregorian calendars (outside of the scope
   of ISO C, POSIX, and glibc).  This matters only in non-C locales.
   The default is to include it, except on platforms where retrieving the locale
   name drags in too many dependencies
   (LOCALENAME_ENHANCE_LOCALE_FUNCS || !SETLOCALE_NULL_ONE_MTSAFE).
   You can override this via
   AC_DEFINE([SUPPORT_NON_GREG_CALENDARS_IN_STRFTIME], [false])
   and if you do that you may be able to omit Gnulib's localename module and its
   dependencies.  */
#ifndef SUPPORT_NON_GREG_CALENDARS_IN_STRFTIME
# define SUPPORT_NON_GREG_CALENDARS_IN_STRFTIME true
#endif
#if defined _LIBC || (HAVE_ONLY_C_LOCALE || USE_C_LOCALE) \
    || ((defined __OpenBSD__ || defined _AIX || defined __ANDROID__) \
        && !GNULIB_NSTRFTIME)
# undef SUPPORT_NON_GREG_CALENDARS_IN_STRFTIME
# define SUPPORT_NON_GREG_CALENDARS_IN_STRFTIME false
#endif

#if HAVE_ONLY_C_LOCALE || USE_C_LOCALE
# include "c-ctype.h"
#else
# include <ctype.h>
#endif
#include <errno.h>
#include <time.h>

/* Do multibyte processing if multibyte encodings are supported, unless
   multibyte sequences are safe in formats.  Multibyte sequences are
   safe if they cannot contain byte sequences that look like format
   conversion specifications.  The multibyte encodings used by the
   C library on the various platforms (UTF-8, GB2312, GBK, CP936,
   GB18030, EUC-TW, BIG5, BIG5-HKSCS, CP950, EUC-JP, EUC-KR, CP949,
   SHIFT_JIS, CP932, JOHAB) are safe for formats, because the byte '%'
   cannot occur in a multibyte character except in the first byte.  */
#define MULTIBYTE_IS_FORMAT_SAFE 1
#define DO_MULTIBYTE (! MULTIBYTE_IS_FORMAT_SAFE)

#if DO_MULTIBYTE
# include <wchar.h>
  static const mbstate_t mbstate_zero;
#endif

#include <limits.h>
#include <locale.h>
#include <stdckdint.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#if (defined __NetBSD__ || defined __sun) && REQUIRE_GNUISH_STRFTIME_AM_PM
# include "localename.h"
#elif defined _WIN32 && !defined __CYGWIN__
# include <wchar.h>
#endif

#include "attribute.h"
#include <intprops.h>

#ifdef COMPILE_WIDE
# include <endian.h>
# define CHAR_T wchar_t
# define UCHAR_T unsigned int
# define L_(Str) L##Str
# define NLW(Sym) _NL_W##Sym

# define MEMCPY(d, s, n) __wmemcpy (d, s, n)
# define STRLEN(s) __wcslen (s)

#else
# define CHAR_T char
# define UCHAR_T unsigned char
# define L_(Str) Str
# define NLW(Sym) Sym
# define ABALTMON_1 _NL_ABALTMON_1

# define MEMCPY(d, s, n) memcpy (d, s, n)
# define STRLEN(s) strlen (s)

#endif

/* Shift A right by B bits portably, by dividing A by 2**B and
   truncating towards minus infinity.  A and B should be free of side
   effects, and B should be in the range 0 <= B <= INT_BITS - 2, where
   INT_BITS is the number of useful bits in an int.  GNU code can
   assume that INT_BITS is at least 32.

   ISO C99 says that A >> B is implementation-defined if A < 0.  Some
   implementations (e.g., UNICOS 9.0 on a Cray Y-MP EL) don't shift
   right in the usual way when A < 0, so SHR falls back on division if
   ordinary A >> B doesn't seem to be the usual signed shift.  */
#define SHR(a, b)       \
  (-1 >> 1 == -1        \
   ? (a) >> (b)         \
   : ((a) + ((a) < 0)) / (1 << (b)) - ((a) < 0))

enum pad_style
{
  ZERO_PAD,         /* (default) Pad with 0 unless format says otherwise.  */
  ALWAYS_ZERO_PAD,  /* '0'       Always pad with 0.  */
  SIGN_PAD,         /* '+'       Always output a sign.  */
  SPACE_PAD,        /* '_'       Pad with space.  */
  NO_PAD            /* '-'       Do not pad.  */
};

#define TM_YEAR_BASE 1900

#ifndef __isleap
/* Nonzero if YEAR is a leap year (every 4 years,
   except every 100th isn't, and every 400th is).  */
# define __isleap(year) \
  ((year) % 4 == 0 && ((year) % 100 != 0 || (year) % 400 == 0))
#endif

#if SUPPORT_NON_GREG_CALENDARS_IN_STRFTIME
/* Support for non-Gregorian calendars.  */
# include "localcharset.h"
# include "localename.h"
# include "calendars.h"
# define CAL_ARGS(x,y) x, y,
#else
# define CAL_ARGS(x,y) /* empty */
#endif


#ifdef _LIBC
# define mktime_z(tz, tm) mktime (tm)
# define tzname __tzname
# define tzset __tzset

# define time_t __time64_t
# define __gmtime_r(t, tp) __gmtime64_r (t, tp)
# define mktime(tp) __mktime64 (tp)
#endif

/* For functions that fill an in-memory string, the number of bytes fits in a
   size_t.  For functions that write to a stream, the number of bytes fits in
   an off64_t (a type that is always at least 64 bits large).  */
#if FPRINTFTIME
# define STREAM_OR_CHAR_T FILE
# define STRFTIME_ARG(x) /* empty */
typedef off64_t byte_count_t;
typedef off64_t sbyte_count_t;
#else
# define STREAM_OR_CHAR_T CHAR_T
# define STRFTIME_ARG(x) x,
typedef size_t byte_count_t;
typedef ptrdiff_t sbyte_count_t;
#endif

/* The functions strftime[_l], wcsftime[_l] defined by glibc have a return type
   'size_t', for compatibility with POSIX, and return 0 upon failure.
   The functions defined by Gnulib have a signed return type, and return -1
   upon failure.  */
#ifdef _LIBC
typedef size_t retval_t;
# define FAILURE 0
#else
typedef sbyte_count_t retval_t;
# define FAILURE (-1)
#endif

#if FPRINTFTIME
# define FPUTC(Byte, P) \
   do \
     { \
       int _r = fputc (Byte, P); \
       if (_r < 0) \
         return FAILURE; \
     } \
   while (false)

# define memset_byte(P, Len, Byte) \
   do \
     for (byte_count_t _i = Len; 0 < _i; _i--) \
       FPUTC (Byte, P); \
   while (false)
# define memset_space(P, Len) memset_byte (P, Len, ' ')
# define memset_zero(P, Len) memset_byte (P, Len, '0')
#elif defined COMPILE_WIDE
# define memset_space(P, Len) (wmemset (P, L' ', Len), (P) += (Len))
# define memset_zero(P, Len) (wmemset (P, L'0', Len), (P) += (Len))
#else
# define memset_space(P, Len) (memset (P, ' ', Len), (P) += (Len))
# define memset_zero(P, Len) (memset (P, '0', Len), (P) += (Len))
#endif

#if FPRINTFTIME
# define advance(P, N)
#else
# define advance(P, N) ((P) += (N))
#endif

#define add(n, f) width_add (width, n, f)

/* Add INCR, returning true if I would become too large.
   INCR should not have side effects.  */
#if FPRINTFTIME
# define incr_overflow(incr) ckd_add (&i, i, incr)
#else
/* Use <= not <, to leave room for trailing NUL.  */
# define incr_overflow(incr) (maxsize - i <= (incr) || (i += (incr), false))
#endif

#define width_add(width, n, f)                                                \
  do                                                                          \
    {                                                                         \
      byte_count_t _n = n;                                                    \
      byte_count_t _w = pad == NO_PAD || width < 0 ? 0 : width;               \
      byte_count_t _incr = _n < _w ? _w : _n;                                 \
      if (incr_overflow (_incr))                                              \
        {                                                                     \
          errno = ERANGE;                                                     \
          return FAILURE;                                                     \
        }                                                                     \
      if (p)                                                                  \
        {                                                                     \
          if (_n < _w)                                                        \
            {                                                                 \
              byte_count_t _delta = _w - _n;                                  \
              if (pad == ALWAYS_ZERO_PAD || pad == SIGN_PAD)                  \
                memset_zero (p, _delta);                                      \
              else                                                            \
                memset_space (p, _delta);                                     \
            }                                                                 \
          f;                                                                  \
          advance (p, _n);                                                    \
        }                                                                     \
    } while (0)

#define add1(c) width_add1 (width, c)
#if FPRINTFTIME
# define width_add1(width, c) width_add (width, 1, FPUTC (c, p))
#else
# define width_add1(width, c) width_add (width, 1, *p = c)
#endif

#define cpy(n, s) width_cpy (width, n, s)
#if FPRINTFTIME
# define width_cpy(width, n, s)                                               \
    width_add (width, n,                                                      \
     do                                                                       \
       {                                                                      \
         CHAR_T const *_s = s;                                                \
         if (to_lowcase)                                                      \
           for (byte_count_t _i = 0; _i < _n; _i++)                           \
             FPUTC (TOLOWER ((UCHAR_T) _s[_i], loc), p);                      \
         else if (to_uppcase)                                                 \
           for (byte_count_t _i = 0; _i < _n; _i++)                           \
             FPUTC (TOUPPER ((UCHAR_T) _s[_i], loc), p);                      \
         else if (fwrite (_s, _n, 1, p) == 0)                                 \
           return FAILURE;                                                    \
       }                                                                      \
     while (0)                                                                \
    )
#else
# define width_cpy(width, n, s)                                               \
    width_add (width, n,                                                      \
         if (to_lowcase)                                                      \
           memcpy_lowcase (p, (s), _n LOCALE_ARG);                            \
         else if (to_uppcase)                                                 \
           memcpy_uppcase (p, (s), _n LOCALE_ARG);                            \
         else                                                                 \
           MEMCPY ((void *) p, (void const *) (s), _n))
#endif

#ifdef COMPILE_WIDE
# ifndef USE_IN_EXTENDED_LOCALE_MODEL
#  undef __mbsrtowcs_l
#  define __mbsrtowcs_l(d, s, l, st, loc) __mbsrtowcs (d, s, l, st)
# endif
#endif


#if defined _LIBC && defined USE_IN_EXTENDED_LOCALE_MODEL
/* We use this code also for the extended locale handling where the
   function gets as an additional argument the locale which has to be
   used.  To access the values we have to redefine the _NL_CURRENT
   macro.  */
# define strftime               __strftime_l
# define wcsftime               __wcsftime_l
# undef _NL_CURRENT
# define _NL_CURRENT(category, item) \
  (current->values[_NL_ITEM_INDEX (item)].string)
# define LOCALE_PARAM , locale_t loc
# define LOCALE_ARG , loc
# define HELPER_LOCALE_ARG  , current
#else
# define LOCALE_PARAM
# define LOCALE_ARG
# ifdef _LIBC
#  define HELPER_LOCALE_ARG , _NL_CURRENT_DATA (LC_TIME)
# else
#  define HELPER_LOCALE_ARG
# endif
#endif

#ifdef COMPILE_WIDE
# ifdef USE_IN_EXTENDED_LOCALE_MODEL
#  define TOUPPER(Ch, L) __towupper_l (Ch, L)
#  define TOLOWER(Ch, L) __towlower_l (Ch, L)
# else
#  define TOUPPER(Ch, L) towupper (Ch)
#  define TOLOWER(Ch, L) towlower (Ch)
# endif
#else
# ifdef USE_IN_EXTENDED_LOCALE_MODEL
#  define TOUPPER(Ch, L) __toupper_l (Ch, L)
#  define TOLOWER(Ch, L) __tolower_l (Ch, L)
# else
#  if HAVE_ONLY_C_LOCALE || USE_C_LOCALE
#   define TOUPPER(Ch, L) c_toupper (Ch)
#   define TOLOWER(Ch, L) c_tolower (Ch)
#  else
#   define TOUPPER(Ch, L) toupper (Ch)
#   define TOLOWER(Ch, L) tolower (Ch)
#  endif
# endif
#endif
/* We don't use 'isdigit' here since the locale dependent
   interpretation is not what we want here.  We only need to accept
   the arabic digits in the ASCII range.  One day there is perhaps a
   more reliable way to accept other sets of digits.  */
#define ISDIGIT(Ch) ((unsigned int) (Ch) - L_('0') <= 9)

/* Avoid false GCC warning "'memset' specified size 18446744073709551615 exceeds
   maximum object size 9223372036854775807", caused by insufficient data flow
   analysis and value propagation of the 'width_add' expansion when GCC is not
   optimizing.  Cf. <https://gcc.gnu.org/PR88443>.  */
#if _GL_GNUC_PREREQ (7, 0) && !__OPTIMIZE__
# pragma GCC diagnostic ignored "-Wstringop-overflow"
#endif

#if !FPRINTFTIME
static CHAR_T *memcpy_lowcase (CHAR_T *dest, const CHAR_T *src,
                               size_t len LOCALE_PARAM);

static CHAR_T *
memcpy_lowcase (CHAR_T *dest, const CHAR_T *src, size_t len LOCALE_PARAM)
{
  while (len-- > 0)
    dest[len] = TOLOWER ((UCHAR_T) src[len], loc);
  return dest;
}

static CHAR_T *memcpy_uppcase (CHAR_T *dest, const CHAR_T *src,
                               size_t len LOCALE_PARAM);

static CHAR_T *
memcpy_uppcase (CHAR_T *dest, const CHAR_T *src, size_t len LOCALE_PARAM)
{
  while (len-- > 0)
    dest[len] = TOUPPER ((UCHAR_T) src[len], loc);
  return dest;
}
#endif


/* Note: We assume that HAVE_STRFTIME_LZ implies HAVE_STRFTIME_L.
   Otherwise, we would have to write (HAVE_STRFTIME_L || HAVE_STRFTIME_LZ)
   instead of HAVE_STRFTIME_L everywhere.  */

/* Define to 1 if we can use the system's native functions that takes a
   timezone_t argument.  As of 2024, this is only true on NetBSD.  */
#define HAVE_NATIVE_TIME_Z \
  (USE_C_LOCALE && HAVE_STRFTIME_L ? HAVE_STRFTIME_LZ : HAVE_STRFTIME_Z)

#if (!HAVE_ONLY_C_LOCALE || !HAVE_STRUCT_TM_TM_ZONE) \
    && USE_C_LOCALE && HAVE_STRFTIME_L

/* Cache for the C locale object.
   Marked volatile so that different threads see the same value
   (avoids locking).  */
static volatile locale_t c_locale_cache;

/* Return the C locale object, or (locale_t) 0 with errno set
   if it cannot be created.  */
static locale_t
c_locale (void)
{
  if (!c_locale_cache)
    c_locale_cache = newlocale (LC_ALL_MASK, "C", (locale_t) 0);
  return c_locale_cache;
}

#endif

#if !defined _LIBC \
    && (!(HAVE_ONLY_C_LOCALE || (USE_C_LOCALE && !HAVE_STRFTIME_L)) \
        || !HAVE_STRUCT_TM_TM_ZONE) \
    && HAVE_NATIVE_TIME_Z

/* On NetBSD a null tz has undefined behavior, so use a non-null tz.
   Cache the UTC time zone object in a volatile variable for improved
   thread safety.  This is good enough in practice, although in theory
   stdatomic.h should be used.  */
static volatile timezone_t utc_timezone_cache;

/* Return the UTC time zone object, or (timezone_t) 0 with errno set
   if it cannot be created.  */
static timezone_t
utc_timezone (void)
{
  timezone_t tz = utc_timezone_cache;
  if (!tz)
    utc_timezone_cache = tz = tzalloc ("UTC0");
  return tz;
}

#endif


#if (defined __NetBSD__ || defined __sun) && REQUIRE_GNUISH_STRFTIME_AM_PM

/* Return true if an AM/PM indicator should be removed.  */
static bool
should_remove_ampm (void)
{
  /* According to glibc's 'am_pm' attribute in the locale database, an AM/PM
     indicator should be absent in the locales for the following languages:
     ab an ast az be ber bg br bs ce cs csb cv da de dsb eo et eu fa fi fo fr
     fur fy ga gl gv hr hsb ht hu hy it ka kk kl ku kv kw ky lb lg li lij ln
     lt lv mg mhr mi mk mn ms mt nb nds nhn nl nn nr nso oc os pap pl pt ro
     ru rw sah sc se sgs sk sl sm sr ss st su sv szl tg tk tn ts tt ug uk unm
     uz ve wae wo xh zu  */
  const char *loc = gl_locale_name_unsafe (LC_TIME, "LC_TIME");
  bool remove_ampm = false;
  switch (loc[0])
    {
    case 'a':
      switch (loc[1])
        {
        case 'b': case 'n': case 'z':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        case 's':
          if (loc[2] == 't' && (loc[3] == '\0' || loc[3] == '_'))
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    case 'b':
      switch (loc[1])
        {
        case 'e':
          if (loc[2] == '\0' || loc[2] == '_'
              || (loc[2] == 'r' && (loc[3] == '\0' || loc[3] == '_')))
            remove_ampm = true;
          break;
        case 'g': case 'r': case 's':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    case 'c':
      switch (loc[1])
        {
        case 'e': case 'v':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        case 's':
          if (loc[2] == '\0' || loc[2] == '_'
              || (loc[2] == 'b' && (loc[3] == '\0' || loc[3] == '_')))
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    case 'd':
      switch (loc[1])
        {
        case 'a': case 'e':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        case 's':
          if (loc[2] == 'b' && (loc[3] == '\0' || loc[3] == '_'))
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    case 'e':
      switch (loc[1])
        {
        case 'o': case 't': case 'u':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    case 'f':
      switch (loc[1])
        {
        case 'a': case 'i': case 'o': case 'r': case 'y':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        case 'u':
          if (loc[2] == 'r' && (loc[3] == '\0' || loc[3] == '_'))
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    case 'g':
      switch (loc[1])
        {
        case 'a': case 'l': case 'v':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    case 'h':
      switch (loc[1])
        {
        case 'r': case 't': case 'u': case 'y':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        case 's':
          if (loc[2] == 'b' && (loc[3] == '\0' || loc[3] == '_'))
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    case 'i':
      switch (loc[1])
        {
        case 't':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    case 'k':
      switch (loc[1])
        {
        case 'a': case 'k': case 'l': case 'u': case 'v': case 'w': case 'y':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    case 'l':
      switch (loc[1])
        {
        case 'b': case 'g': case 'n': case 't': case 'v':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        case 'i':
          if (loc[2] == 'j' && (loc[3] == '\0' || loc[3] == '_'))
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    case 'm':
      switch (loc[1])
        {
        case 'g': case 'i': case 'k': case 'n': case 's': case 't':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        case 'h':
          if (loc[2] == 'r' && (loc[3] == '\0' || loc[3] == '_'))
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    case 'n':
      switch (loc[1])
        {
        case 'b': case 'l': case 'n': case 'r':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        case 'd':
          if (loc[2] == 's' && (loc[3] == '\0' || loc[3] == '_'))
            remove_ampm = true;
          break;
        case 'h':
          if (loc[2] == 'n' && (loc[3] == '\0' || loc[3] == '_'))
            remove_ampm = true;
          break;
        case 's':
          if (loc[2] == 'o' && (loc[3] == '\0' || loc[3] == '_'))
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    case 'o':
      switch (loc[1])
        {
        case 'c': case 's':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    case 'p':
      switch (loc[1])
        {
        case 'l': case 't':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        case 'a':
          if (loc[2] == 'p' && (loc[3] == '\0' || loc[3] == '_'))
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    case 'r':
      switch (loc[1])
        {
        case 'o': case 'u': case 'w':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    case 's':
      switch (loc[1])
        {
        case 'c': case 'e': case 'k': case 'l': case 'm': case 'r': case 's':
        case 't': case 'u': case 'v':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        case 'a':
          if (loc[2] == 'h' && (loc[3] == '\0' || loc[3] == '_'))
            remove_ampm = true;
          break;
        case 'g':
          if (loc[2] == 's' && (loc[3] == '\0' || loc[3] == '_'))
            remove_ampm = true;
          break;
        case 'z':
          if (loc[2] == 'l' && (loc[3] == '\0' || loc[3] == '_'))
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    case 't':
      switch (loc[1])
        {
        case 'g': case 'k': case 'n': case 's': case 't':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    case 'u':
      switch (loc[1])
        {
        case 'g': case 'k': case 'z':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        case 'n':
          if (loc[2] == 'm'&& (loc[3] == '\0' || loc[3] == '_'))
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    case 'v':
      switch (loc[1])
        {
        case 'e':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    case 'w':
      switch (loc[1])
        {
        case 'a':
          if (loc[2] == 'e' && (loc[3] == '\0' || loc[3] == '_'))
            remove_ampm = true;
          break;
        case 'o':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    case 'x':
      switch (loc[1])
        {
        case 'h':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    case 'z':
      switch (loc[1])
        {
        case 'u':
          if (loc[2] == '\0' || loc[2] == '_')
            remove_ampm = true;
          break;
        default:
          break;
        }
      break;
    default:
      break;
    }
  return remove_ampm;
}

#endif


#if ! HAVE_STRUCT_TM_TM_GMTOFF
/* Yield the difference between *A and *B,
   measured in seconds, ignoring leap seconds.  */
# define tm_diff ftime_tm_diff
static int tm_diff (const struct tm *, const struct tm *);
static int
tm_diff (const struct tm *a, const struct tm *b)
{
  /* Compute intervening leap days correctly even if year is negative.
     Take care to avoid int overflow in leap day calculations,
     but it's OK to assume that A and B are close to each other.  */
  int a4 = SHR (a->tm_year, 2) + SHR (TM_YEAR_BASE, 2) - ! (a->tm_year & 3);
  int b4 = SHR (b->tm_year, 2) + SHR (TM_YEAR_BASE, 2) - ! (b->tm_year & 3);
  int a100 = (a4 + (a4 < 0)) / 25 - (a4 < 0);
  int b100 = (b4 + (b4 < 0)) / 25 - (b4 < 0);
  int a400 = SHR (a100, 2);
  int b400 = SHR (b100, 2);
  int intervening_leap_days = (a4 - b4) - (a100 - b100) + (a400 - b400);
  int years = a->tm_year - b->tm_year;
  int days = (365 * years + intervening_leap_days
              + (a->tm_yday - b->tm_yday));
  return (60 * (60 * (24 * days + (a->tm_hour - b->tm_hour))
                + (a->tm_min - b->tm_min))
          + (a->tm_sec - b->tm_sec));
}
#endif



/* The number of days from the first day of the first ISO week of this
   year to the year day YDAY with week day WDAY.  ISO weeks start on
   Monday; the first ISO week has the year's first Thursday.  YDAY may
   be as small as YDAY_MINIMUM.  */
#define ISO_WEEK_START_WDAY 1 /* Monday */
#define ISO_WEEK1_WDAY 4 /* Thursday */
#define YDAY_MINIMUM (-366)
static int iso_week_days (int, int);
static __inline int
iso_week_days (int yday, int wday)
{
  /* Add enough to the first operand of % to make it nonnegative.  */
  int big_enough_multiple_of_7 = (-YDAY_MINIMUM / 7 + 2) * 7;
  return (yday
          - (yday - wday + ISO_WEEK1_WDAY + big_enough_multiple_of_7) % 7
          + ISO_WEEK1_WDAY - ISO_WEEK_START_WDAY);
}


#if !defined _NL_CURRENT && (HAVE_ONLY_C_LOCALE || (USE_C_LOCALE && !HAVE_STRFTIME_L))
static CHAR_T const c_weekday_names[][sizeof "Wednesday"] =
  {
    L_("Sunday"), L_("Monday"), L_("Tuesday"), L_("Wednesday"),
    L_("Thursday"), L_("Friday"), L_("Saturday")
  };
static CHAR_T const c_month_names[][sizeof "September"] =
  {
    L_("January"), L_("February"), L_("March"), L_("April"), L_("May"),
    L_("June"), L_("July"), L_("August"), L_("September"), L_("October"),
    L_("November"), L_("December")
  };
#endif


/* When compiling this file, Gnulib-using applications should #define
   my_strftime to a symbol (typically nstrftime) to name their
   extended strftime with extra arguments TZ and NS.  */

#ifdef my_strftime
# define extra_args , tz, ns
# define extra_args_spec , timezone_t tz, int ns
#else
# if defined COMPILE_WIDE
#  define my_strftime wcsftime
#  define nl_get_alt_digit _nl_get_walt_digit
# else
#  define my_strftime strftime
#  define nl_get_alt_digit _nl_get_alt_digit
# endif
# define extra_args
# define extra_args_spec
/* We don't have this information in general.  */
# define tz 1
# define ns 0
#endif

static retval_t __strftime_internal (STREAM_OR_CHAR_T *,
                                     STRFTIME_ARG (size_t)
                                     const CHAR_T *, const struct tm *,
                                     CAL_ARGS (const struct calendar *,
                                               struct calendar_date *)
                                     bool, enum pad_style,
                                     sbyte_count_t, bool *
                                     extra_args_spec LOCALE_PARAM);

#if !defined _LIBC \
    && (!(HAVE_ONLY_C_LOCALE || (USE_C_LOCALE && !HAVE_STRFTIME_L)) \
        || !HAVE_STRUCT_TM_TM_ZONE)

/* Make sure we're calling the actual underlying strftime.
   In some cases, time.h contains something like
   "#define strftime rpl_strftime".  */
# ifdef strftime
#  undef strftime
# endif

/* Assuming the time zone is TZ, store into UBUF, of size UBUFSIZE, a
   ' ' followed by the result of calling strftime with the format
   "%MF" where M is MODIFIER (or is omitted if !MODIFIER) and F is
   FORMAT_CHAR, along with the time information specified by *TP.
   Return the number of bytes stored if successful, zero otherwise.  */
static size_t
underlying_strftime (timezone_t tz, char *ubuf, size_t ubufsize,
                     char modifier, char format_char, struct tm const *tp)
{
  /* The relevant information is available only via the
     underlying strftime implementation, so use that.  */
  char ufmt[5];
  char *u = ufmt;

  /* The space helps distinguish strftime failure from empty
     output.  */
  *u++ = ' ';
  *u++ = '%';
  *u = modifier;
  u += !!modifier;
  *u++ = format_char;
  *u = '\0';

# if HAVE_NATIVE_TIME_Z
  if (!tz)
    {
      tz = utc_timezone ();
      if (!tz)
        return 0; /* errno is set here */
    }
# endif

# if !HAVE_NATIVE_TIME_Z
  if (tz && tz != local_tz)
    {
      tz = set_tz (tz);
      if (!tz)
        return 0;
    }
# endif

  size_t len;
# if USE_C_LOCALE && HAVE_STRFTIME_L
  locale_t locale = c_locale ();
  if (!locale)
    return 0; /* errno is set here */
#  if HAVE_STRFTIME_LZ
  len = strftime_lz (tz, ubuf, ubufsize, ufmt, tp, locale);
#  else
  len = strftime_l (ubuf, ubufsize, ufmt, tp, locale);
#  endif
# else
#  if HAVE_STRFTIME_Z
  len = strftime_z (tz, ubuf, ubufsize, ufmt, tp);
#  else
  len = strftime (ubuf, ubufsize, ufmt, tp);
#  endif
# endif

# if !HAVE_NATIVE_TIME_Z
  if (tz && !revert_tz (tz))
    return 0;
# endif

  if (len != 0)
    {
# if ((__GLIBC__ == 2 && __GLIBC_MINOR__ < 31) \
      || defined __NetBSD__ || defined __sun)
      /* glibc < 2.31, NetBSD, Solaris */
      if (format_char == 'c')
        {
          /* The output of the strftime %c directive consists of the
             date, the time, and the time zone.  But the time zone is
             wrong, since neither TZ nor ZONE was passed as argument.
             Therefore, remove the the last space-delimited word.
             In order not to accidentally remove a date or a year
             (that contains no letter) or an AM/PM indicator (that has
             length 2), remove that last word only if it contains a
             letter and has length >= 3.  */
          char *space;
          for (space = ubuf + len - 1; *space != ' '; space--)
            continue;
          if (space > ubuf)
            {
              /* Found a space.  */
              if (strlen (space + 1) >= 3)
                {
                  /* The last word has length >= 3.  */
                  bool found_letter = false;
                  const char *p;
                  for (p = space + 1; *p != '\0'; p++)
                    if ((*p >= 'A' && *p <= 'Z')
                        || (*p >= 'a' && *p <= 'z'))
                      {
                        found_letter = true;
                        break;
                      }
                  if (found_letter)
                    {
                      /* The last word contains a letter.  */
                      *space = '\0';
                      len = space - ubuf;
                    }
                }
            }
        }
#  if (defined __NetBSD__ || defined __sun) && REQUIRE_GNUISH_STRFTIME_AM_PM
      /* The output of the strftime %p and %r directives contains
         an AM/PM indicator even for locales where it is not
         suitable, such as French.  Remove this indicator.  */
      if (format_char == 'p')
        {
          bool found_ampm = (len > 1);
          if (found_ampm && should_remove_ampm ())
            {
              ubuf[1] = '\0';
              len = 1;
            }
        }
      else if (format_char == 'r')
        {
          char last_char = ubuf[len - 1];
          bool found_ampm = !(last_char >= '0' && last_char <= '9');
          if (found_ampm && should_remove_ampm ())
            {
              char *space;
              for (space = ubuf + len - 1; *space != ' '; space--)
                continue;
              if (space > ubuf)
                {
                  *space = '\0';
                  len = space - ubuf;
                }
            }
        }
#  endif
# endif
    }
  return len;
}
#endif

/* Return a time zone abbreviation for TZ.  Use BUF, of size BUFSIZE,
   to store it if needed.  If MODIFIER use the strftime format
   "%mZ" to format it, where m is the MODIFIER; otherwise
   use plain "%Z".  Format an abbreviation appropriate for
   TP and EXTRA_ARGS_SPEC.  Return the empty string on failure.  */
static char const *
get_tm_zone (timezone_t tz, char *ubuf, int ubufsize, int modifier,
             struct tm const *tp)
{
#if HAVE_STRUCT_TM_TM_ZONE
  /* The POSIX test suite assumes that setting
     the environment variable TZ to a new value before calling strftime()
     will influence the result (the %Z format) even if the information in
     *TP is computed with a totally different time zone.
     This is bogus: though POSIX allows bad behavior like this,
     POSIX does not require it.  Do the right thing instead.  */
  const char *ret = tp->tm_zone;
# if defined __ANDROID__
  if (!ret)
    ret = "";
# endif
  return ret;
#else
  if (!tz)
    return "UTC";

# if !HAVE_NATIVE_TIME_Z
  timezone_t old_tz = tz;
  if (tz != local_tz)
    {
      old_tz = set_tz (tz);
      if (!old_tz)
        return "";
    }
# endif

  int zsize = underlying_strftime (tz, ubuf, ubufsize, 0, 'Z', tp);

# if !HAVE_NATIVE_TIME_Z
  if (!revert_tz (old_tz))
    return "";
# endif

  return zsize ? ubuf + 1 : "";
#endif
}

/* Write information from TP into S according to the format
   string FORMAT.  Return the number of bytes written.
   Upon failure:
     - return 0 for the functions defined by glibc,
     - return -1 for the functions defined by Gnulib.

   If !FPRINTFTIME, write no more than MAXSIZE bytes (including the
   terminating '\0'), and if S is NULL do not write into S.
   To determine how many characters would be written, use NULL for S
   and (size_t) -1 for MAXSIZE.  */
retval_t
my_strftime (STREAM_OR_CHAR_T *s, STRFTIME_ARG (size_t maxsize)
             const CHAR_T *format,
             const struct tm *tp extra_args_spec LOCALE_PARAM)
{
#if SUPPORT_NON_GREG_CALENDARS_IN_STRFTIME
  /* Recognize whether to use a non-Gregorian calendar.  */
  const struct calendar *cal = NULL;
  struct calendar_date caldate;
  if (strcmp (locale_charset (), "UTF-8") == 0)
    {
      const char *loc = gl_locale_name_unsafe (LC_TIME, "LC_TIME");
      if (strlen (loc) >= 5 && !(loc[5] >= 'A' && loc[5] <= 'Z'))
        {
          if (memcmp (loc, "th_TH", 5) == 0)
            cal = &thai_calendar;
          else if (memcmp (loc, "fa_IR", 5) == 0)
            cal = &persian_calendar;
          else if (memcmp (loc, "am_ET", 5) == 0)
            cal = &ethiopian_calendar;
          if (cal != NULL)
            {
              if (cal->from_gregorian (&caldate,
                                       tp->tm_year + 1900,
                                       tp->tm_mon,
                                       tp->tm_mday) < 0)
                cal = NULL;
            }
        }
    }
#endif
  bool tzset_called = false;
  return __strftime_internal (s, STRFTIME_ARG (maxsize) format, tp,
                              CAL_ARGS (cal, &caldate)
                              false, ZERO_PAD, -1,
                              &tzset_called extra_args LOCALE_ARG);
}
libc_hidden_def (my_strftime)

/* Just like my_strftime, above, but with more parameters.
   UPCASE indicates that the result should be converted to upper case.
   YR_SPEC and WIDTH specify the padding and width for the year.
   *TZSET_CALLED indicates whether tzset has been called here.  */
static retval_t
__strftime_internal (STREAM_OR_CHAR_T *s, STRFTIME_ARG (size_t maxsize)
                     const CHAR_T *format,
                     const struct tm *tp,
                     CAL_ARGS (const struct calendar *cal,
                               struct calendar_date *caldate)
                     bool upcase,
                     enum pad_style yr_spec, sbyte_count_t width,
                     bool *tzset_called
                     extra_args_spec LOCALE_PARAM)
{
#if defined _LIBC && defined USE_IN_EXTENDED_LOCALE_MODEL
  struct __locale_data *const current = loc->__locales[LC_TIME];
#endif
#if FAILURE == 0
  int saved_errno = errno;
#elif !FPRINTFTIME
  if (PTRDIFF_MAX < maxsize)
    maxsize = PTRDIFF_MAX;
#endif

#ifdef _NL_CURRENT
  /* We cannot make the following values variables since we must delay
     the evaluation of these values until really needed since some
     expressions might not be valid in every situation.  The 'struct tm'
     might be generated by a strptime() call that initialized
     only a few elements.  Dereference the pointers only if the format
     requires this.  Then it is ok to fail if the pointers are invalid.  */
# define a_wkday \
  ((const CHAR_T *) (tp->tm_wday < 0 || tp->tm_wday > 6                      \
                     ? "?" : _NL_CURRENT (LC_TIME, NLW(ABDAY_1) + tp->tm_wday)))
# define f_wkday \
  ((const CHAR_T *) (tp->tm_wday < 0 || tp->tm_wday > 6                      \
                     ? "?" : _NL_CURRENT (LC_TIME, NLW(DAY_1) + tp->tm_wday)))
# define a_month \
  ((const CHAR_T *) (tp->tm_mon < 0 || tp->tm_mon > 11                       \
                     ? "?" : _NL_CURRENT (LC_TIME, NLW(ABMON_1) + tp->tm_mon)))
# define f_month \
  ((const CHAR_T *) (tp->tm_mon < 0 || tp->tm_mon > 11                       \
                     ? "?" : _NL_CURRENT (LC_TIME, NLW(MON_1) + tp->tm_mon)))
# define a_altmonth \
  ((const CHAR_T *) (tp->tm_mon < 0 || tp->tm_mon > 11                       \
                     ? "?" : _NL_CURRENT (LC_TIME, NLW(ABALTMON_1) + tp->tm_mon)))
# define f_altmonth \
  ((const CHAR_T *) (tp->tm_mon < 0 || tp->tm_mon > 11                       \
                     ? "?" : _NL_CURRENT (LC_TIME, NLW(ALTMON_1) + tp->tm_mon)))
# define ampm \
  ((const CHAR_T *) _NL_CURRENT (LC_TIME, tp->tm_hour > 11                    \
                                 ? NLW(PM_STR) : NLW(AM_STR)))

# define aw_len STRLEN (a_wkday)
# define am_len STRLEN (a_month)
# define aam_len STRLEN (a_altmonth)
# define ap_len STRLEN (ampm)
#elif HAVE_ONLY_C_LOCALE || (USE_C_LOCALE && !HAVE_STRFTIME_L)
/* The English abbreviated weekday names are just the first 3 characters of the
   English full weekday names.  */
# define a_wkday \
  (tp->tm_wday < 0 || tp->tm_wday > 6 ? L_("?") : c_weekday_names[tp->tm_wday])
# define aw_len 3
# define f_wkday \
  (tp->tm_wday < 0 || tp->tm_wday > 6 ? L_("?") : c_weekday_names[tp->tm_wday])
/* The English abbreviated month names are just the first 3 characters of the
   English full month names.  */
# define a_month \
  (tp->tm_mon < 0 || tp->tm_mon > 11 ? L_("?") : c_month_names[tp->tm_mon])
# define am_len 3
# define f_month \
  (tp->tm_mon < 0 || tp->tm_mon > 11 ? L_("?") : c_month_names[tp->tm_mon])
/* The English AM/PM strings happen to have the same length, namely 2.  */
# define ampm (L_("AMPM") + 2 * (tp->tm_hour > 11))
# define ap_len 2
#endif
  retval_t i = 0;
  STREAM_OR_CHAR_T *p = s;
  const CHAR_T *f;
#if DO_MULTIBYTE && !defined COMPILE_WIDE
  const char *format_end = NULL;
#endif

  int hour12 = tp->tm_hour;
  if (hour12 > 12)
    hour12 -= 12;
  else
    if (hour12 == 0)
      hour12 = 12;

  for (f = format; *f != '\0'; width = -1, f++)
    {
      enum pad_style pad = ZERO_PAD;
      int modifier;             /* Field modifier ('E', 'O', or 0).  */
      int digits = 0;           /* Max digits for numeric format.  */
      int number_value;         /* Numeric value to be printed.  */
      unsigned int u_number_value; /* (unsigned int) number_value.  */
      bool negative_number;     /* The number is negative.  */
      bool always_output_a_sign; /* +/- should always be output.  */
      int tz_colon_mask;        /* Bitmask of where ':' should appear.  */
#if SUPPORT_NON_GREG_CALENDARS_IN_STRFTIME
      unsigned int digits_base = '0'; /* '0' or some UCS-2 value.  */
#endif
      const CHAR_T *subfmt;
      CHAR_T *bufp;
      CHAR_T buf[1
                 + 2 /* for the two colons in a %::z or %:::z time zone */
                 + (sizeof (int) < sizeof (time_t)
                    ? INT_STRLEN_BOUND (time_t)
                    : INT_STRLEN_BOUND (int))];
      bool to_lowcase = false;
      bool to_uppcase = upcase;
      size_t colons;
      bool change_case = false;
      int format_char;
      sbyte_count_t subwidth;

#if DO_MULTIBYTE && !defined COMPILE_WIDE
      switch (*f)
        {
        case L_('%'):
          break;

        case L_('\b'): case L_('\t'): case L_('\n'):
        case L_('\v'): case L_('\f'): case L_('\r'):
        case L_(' '): case L_('!'): case L_('"'): case L_('#'): case L_('&'):
        case L_('\''): case L_('('): case L_(')'): case L_('*'): case L_('+'):
        case L_(','): case L_('-'): case L_('.'): case L_('/'): case L_('0'):
        case L_('1'): case L_('2'): case L_('3'): case L_('4'): case L_('5'):
        case L_('6'): case L_('7'): case L_('8'): case L_('9'): case L_(':'):
        case L_(';'): case L_('<'): case L_('='): case L_('>'): case L_('?'):
        case L_('A'): case L_('B'): case L_('C'): case L_('D'): case L_('E'):
        case L_('F'): case L_('G'): case L_('H'): case L_('I'): case L_('J'):
        case L_('K'): case L_('L'): case L_('M'): case L_('N'): case L_('O'):
        case L_('P'): case L_('Q'): case L_('R'): case L_('S'): case L_('T'):
        case L_('U'): case L_('V'): case L_('W'): case L_('X'): case L_('Y'):
        case L_('Z'): case L_('['): case L_('\\'): case L_(']'): case L_('^'):
        case L_('_'): case L_('a'): case L_('b'): case L_('c'): case L_('d'):
        case L_('e'): case L_('f'): case L_('g'): case L_('h'): case L_('i'):
        case L_('j'): case L_('k'): case L_('l'): case L_('m'): case L_('n'):
        case L_('o'): case L_('p'): case L_('q'): case L_('r'): case L_('s'):
        case L_('t'): case L_('u'): case L_('v'): case L_('w'): case L_('x'):
        case L_('y'): case L_('z'): case L_('{'): case L_('|'): case L_('}'):
        case L_('~'):
          /* The C Standard requires these 98 characters (plus '%') to
             be in the basic execution character set.  None of these
             characters can start a multibyte sequence, so they need
             not be analyzed further.  */
          add1 (*f);
          continue;

        default:
          /* Copy this multibyte sequence until we reach its end, find
             an error, or come back to the initial shift state.  */
          {
            mbstate_t mbstate = mbstate_zero;
            size_t len = 0;
            size_t fsize;

            if (! format_end)
              format_end = f + strlen (f) + 1;
            fsize = format_end - f;

            do
              {
                size_t bytes = mbrlen (f + len, fsize - len, &mbstate);

                if (bytes == 0)
                  break;

                if (bytes == (size_t) -2)
                  {
                    len += strlen (f + len);
                    break;
                  }

                if (bytes == (size_t) -1)
                  {
                    len++;
                    break;
                  }

                len += bytes;
              }
            while (! mbsinit (&mbstate));

            cpy (len, f);
            f += len - 1;
            continue;
          }
        }

#else /* ! DO_MULTIBYTE */

      /* Either multibyte encodings are not supported, they are
         safe for formats, so any non-'%' byte can be copied through,
         or this is the wide character version.  */
      if (*f != L_('%'))
        {
          add1 (*f);
          continue;
        }

#endif /* ! DO_MULTIBYTE */

      char const *percent = f;

      /* Check for flags that can modify a format.  */
      while (1)
        {
          switch (*++f)
            {
              /* This influences the number formats.  */
            case L_('_'): pad = SPACE_PAD; continue;
            case L_('-'): pad = NO_PAD; continue;
            case L_('+'): pad = SIGN_PAD; continue;
            case L_('0'): pad = ALWAYS_ZERO_PAD; continue;

              /* This changes textual output.  */
            case L_('^'):
              to_uppcase = true;
              continue;
            case L_('#'):
              change_case = true;
              continue;

            default:
              break;
            }
          break;
        }

      if (ISDIGIT (*f))
        {
          width = 0;
          do
            {
              if (ckd_mul (&width, width, 10)
                  || ckd_add (&width, width, *f - L_('0')))
                return FAILURE;
              ++f;
            }
          while (ISDIGIT (*f));
        }

      /* Check for modifiers.  */
      switch (*f)
        {
        case L_('E'):
        case L_('O'):
          modifier = *f++;
          break;

        default:
          modifier = 0;
          break;
        }

      /* Now do the specified format.  */
      format_char = *f;
      switch (format_char)
        {
#define DO_NUMBER(d, v) \
          do                                                                  \
            {                                                                 \
              digits = d;                                                     \
              number_value = v;                                               \
              goto do_number;                                                 \
            }                                                                 \
          while (0)
#define DO_SIGNED_NUMBER(d, negative, v) \
          DO_MAYBE_SIGNED_NUMBER (d, negative, v, do_signed_number)
#define DO_YEARISH(d, negative, v) \
          DO_MAYBE_SIGNED_NUMBER (d, negative, v, do_yearish)
#define DO_MAYBE_SIGNED_NUMBER(d, negative, v, label) \
          do                                                                  \
            {                                                                 \
              digits = d;                                                     \
              negative_number = negative;                                     \
              u_number_value = v;                                             \
              goto label;                                                     \
            }                                                                 \
          while (0)

          /* The mask is not what you might think.
             When the ordinal i'th bit is set, insert a colon
             before the i'th digit of the time zone representation.  */
#define DO_TZ_OFFSET(d, mask, v) \
          do                                                                  \
            {                                                                 \
              digits = d;                                                     \
              tz_colon_mask = mask;                                           \
              u_number_value = v;                                             \
              goto do_tz_offset;                                              \
            }                                                                 \
          while (0)
#define DO_NUMBER_SPACEPAD(d, v) \
          do                                                                  \
            {                                                                 \
              digits = d;                                                     \
              number_value = v;                                               \
              goto do_number_spacepad;                                        \
            }                                                                 \
          while (0)

        case L_('%'):
          if (f - 1 != percent)
            goto bad_percent;
          add1 (*f);
          break;

        case L_('a'):
          if (modifier != 0)
            goto bad_format;
          if (change_case)
            {
              to_uppcase = true;
              to_lowcase = false;
            }
#if defined _NL_CURRENT || HAVE_ONLY_C_LOCALE || (USE_C_LOCALE && !HAVE_STRFTIME_L)
          cpy (aw_len, a_wkday);
          break;
#else
          goto underlying_strftime;
#endif

        case 'A':
          if (modifier != 0)
            goto bad_format;
          if (change_case)
            {
              to_uppcase = true;
              to_lowcase = false;
            }
#if defined _NL_CURRENT || HAVE_ONLY_C_LOCALE || (USE_C_LOCALE && !HAVE_STRFTIME_L)
          cpy (STRLEN (f_wkday), f_wkday);
          break;
#else
          goto underlying_strftime;
#endif

        case L_('b'):
        case L_('h'):
          if (change_case)
            {
              to_uppcase = true;
              to_lowcase = false;
            }
          if (modifier == L_('E'))
            goto bad_format;
#if SUPPORT_NON_GREG_CALENDARS_IN_STRFTIME
          if (cal != NULL)
            {
              cpy (STRLEN (caldate->month_names[caldate->month].abbrev),
                   caldate->month_names[caldate->month].abbrev);
              break;
            }
#endif
#ifdef _NL_CURRENT
          if (modifier == L_('O'))
            cpy (aam_len, a_altmonth);
          else
            cpy (am_len, a_month);
          break;
#elif HAVE_ONLY_C_LOCALE || (USE_C_LOCALE && !HAVE_STRFTIME_L)
          cpy (am_len, a_month);
          break;
#else
# if defined _WIN32 && !defined __CYGWIN__
          format_char = L_('b');
# endif
          goto underlying_strftime;
#endif

        case L_('B'):
          if (modifier == L_('E'))
            goto bad_format;
          if (change_case)
            {
              to_uppcase = true;
              to_lowcase = false;
            }
#if SUPPORT_NON_GREG_CALENDARS_IN_STRFTIME
          if (cal != NULL)
            {
              cpy (STRLEN (caldate->month_names[caldate->month].full),
                   caldate->month_names[caldate->month].full);
              break;
            }
#endif
#ifdef _NL_CURRENT
          if (modifier == L_('O'))
            cpy (STRLEN (f_altmonth), f_altmonth);
          else
            cpy (STRLEN (f_month), f_month);
          break;
#elif HAVE_ONLY_C_LOCALE || (USE_C_LOCALE && !HAVE_STRFTIME_L)
          cpy (STRLEN (f_month), f_month);
          break;
#else
          goto underlying_strftime;
#endif

        case L_('c'):
          if (modifier == L_('O'))
            goto bad_format;
#ifdef _NL_CURRENT
          if (! (modifier == L_('E')
                 && (*(subfmt =
                       (const CHAR_T *) _NL_CURRENT (LC_TIME,
                                                     NLW(ERA_D_T_FMT)))
                     != '\0')))
            subfmt = (const CHAR_T *) _NL_CURRENT (LC_TIME, NLW(D_T_FMT));
#elif HAVE_ONLY_C_LOCALE || (USE_C_LOCALE && !HAVE_STRFTIME_L)
          subfmt = L_("%a %b %e %H:%M:%S %Y");
#elif defined _WIN32 && !defined __CYGWIN__
          /* On native Windows, "%c" is "%d/%m/%Y %H:%M:%S" by default.  */
          bool is_c_locale;
          /* This code is equivalent to is_c_locale = !hard_locale (LC_TIME). */
# if defined _MSC_VER
          const wchar_t *locale = _wsetlocale (LC_TIME, NULL);
          is_c_locale =
            (wcscmp (locale, L"C") == 0 || wcscmp (locale, L"POSIX") == 0);
# else
          const char *locale = setlocale (LC_TIME, NULL);
          is_c_locale =
            (strcmp (locale, "C") == 0 || strcmp (locale, "POSIX") == 0);
# endif
          if (is_c_locale)
            subfmt = L_("%a %b %e %H:%M:%S %Y");
          else
            subfmt = L_("%a %e %b %Y %H:%M:%S");
#else
          goto underlying_strftime;
#endif

        subformat:
          subwidth = -1;
        subformat_width:
          {
            retval_t len =
              __strftime_internal (NULL, STRFTIME_ARG ((size_t) -1)
                                   subfmt, tp,
                                   CAL_ARGS (cal, caldate)
                                   to_uppcase, pad, subwidth,
                                   tzset_called
                                   extra_args LOCALE_ARG);
            if (FAILURE < 0 && len < 0)
              return FAILURE; /* errno is set here */
            add (len, __strftime_internal (p,
                                           STRFTIME_ARG (maxsize - i)
                                           subfmt, tp,
                                           CAL_ARGS (cal, caldate)
                                           to_uppcase, pad, subwidth,
                                           tzset_called
                                           extra_args LOCALE_ARG));
          }
          break;

#if !defined _LIBC && !(HAVE_ONLY_C_LOCALE || (USE_C_LOCALE && !HAVE_STRFTIME_L))
        underlying_strftime:
          {
            char ubuf[1024]; /* enough for any single format in practice */
            size_t len;
            len = underlying_strftime (tz, ubuf, sizeof ubuf,
                                       modifier, format_char, tp);
            if (len != 0)
              {
# if (__GLIBC__ == 2 && __GLIBC_MINOR__ < 31) || defined __NetBSD__ || defined __sun /* glibc < 2.31, NetBSD, Solaris */
                if (format_char == L_('c'))
                  {
                    /* The output of the strftime %c directive consists of the
                       date, the time, and the time zone.  But the time zone is
                       wrong, since neither TZ nor ZONE was passed as argument.
                       Therefore, remove the the last space-delimited word.
                       In order not to accidentally remove a date or a year
                       (that contains no letter) or an AM/PM indicator (that has
                       length 2), remove that last word only if it contains a
                       letter and has length >= 3.  */
                    char *space;
                    for (space = ubuf + len - 1; *space != ' '; space--)
                      ;
                    if (space > ubuf)
                      {
                        /* Found a space.  */
                        if (strlen (space + 1) >= 3)
                          {
                            /* The last word has length >= 3.  */
                            bool found_letter = false;
                            const char *p;
                            for (p = space + 1; *p != '\0'; p++)
                              if ((*p >= 'A' && *p <= 'Z')
                                  || (*p >= 'a' && *p <= 'z'))
                                {
                                  found_letter = true;
                                  break;
                                }
                            if (found_letter)
                              {
                                /* The last word contains a letter.  */
                                *space = '\0';
                                len = space - ubuf;
                              }
                          }
                      }
                  }
#  if (defined __NetBSD__ || defined __sun) && REQUIRE_GNUISH_STRFTIME_AM_PM
                /* The output of the strftime %p and %r directives contains
                   an AM/PM indicator even for locales where it is not
                   suitable, such as French.  Remove this indicator.  */
                else if (format_char == L_('p'))
                  {
                    bool found_ampm = (len > 1);
                    if (found_ampm && should_remove_ampm ())
                      {
                        ubuf[1] = '\0';
                        len = 1;
                      }
                  }
                else if (format_char == L_('r'))
                  {
                    char last_char = ubuf[len - 1];
                    bool found_ampm = !(last_char >= '0' && last_char <= '9');
                    if (found_ampm && should_remove_ampm ())
                      {
                        char *space;
                        for (space = ubuf + len - 1; *space != ' '; space--)
                          ;
                        if (space > ubuf)
                          {
                            *space = '\0';
                            len = space - ubuf;
                          }
                      }
                  }
#  endif
# endif
                cpy (len - 1, ubuf + 1);
              }
          }
          break;
#endif

        case L_('C'):
          if (modifier == L_('E'))
            {
#if HAVE_STRUCT_ERA_ENTRY
              struct era_entry *era = _nl_get_era_entry (tp HELPER_LOCALE_ARG);
              if (era)
                {
# ifdef COMPILE_WIDE
                  size_t len = __wcslen (era->era_wname);
                  cpy (len, era->era_wname);
# else
                  size_t len = strlen (era->era_name);
                  cpy (len, era->era_name);
# endif
                  break;
                }
#elif HAVE_ONLY_C_LOCALE || (USE_C_LOCALE && !HAVE_STRFTIME_L)
#else
              goto underlying_strftime;
#endif
            }

          {
            bool negative_year = tp->tm_year < - TM_YEAR_BASE;
            bool zero_thru_1899 = !negative_year & (tp->tm_year < 0);
            int century = ((tp->tm_year - 99 * zero_thru_1899) / 100
                           + TM_YEAR_BASE / 100);
            DO_YEARISH (2, negative_year, century);
          }

        case L_('x'):
          if (modifier == L_('O'))
            goto bad_format;
#if SUPPORT_NON_GREG_CALENDARS_IN_STRFTIME
          if (cal != NULL)
            {
              subfmt = cal->d_fmt;
              goto subformat;
            }
#endif
#ifdef _NL_CURRENT
          if (! (modifier == L_('E')
                 && (*(subfmt =
                       (const CHAR_T *) _NL_CURRENT (LC_TIME, NLW(ERA_D_FMT)))
                     != L_('\0'))))
            subfmt = (const CHAR_T *) _NL_CURRENT (LC_TIME, NLW(D_FMT));
          goto subformat;
#elif HAVE_ONLY_C_LOCALE || (USE_C_LOCALE && !HAVE_STRFTIME_L)
          subfmt = L_("%m/%d/%y");
          goto subformat;
#else
          goto underlying_strftime;
#endif

        case L_('D'):
          if (modifier != 0)
            goto bad_format;
          subfmt = L_("%m/%d/%y");
          goto subformat;

        case L_('d'):
          if (modifier == L_('E'))
            goto bad_format;

#if SUPPORT_NON_GREG_CALENDARS_IN_STRFTIME
          if (cal != NULL)
            DO_NUMBER (2, caldate->day);
#endif
          DO_NUMBER (2, tp->tm_mday);

        case L_('e'):
          if (modifier == L_('E'))
            goto bad_format;

#if SUPPORT_NON_GREG_CALENDARS_IN_STRFTIME
          if (cal != NULL)
            DO_NUMBER_SPACEPAD (2, caldate->day);
#endif
          DO_NUMBER_SPACEPAD (2, tp->tm_mday);

          /* All numeric formats set DIGITS and NUMBER_VALUE (or U_NUMBER_VALUE)
             and then jump to one of these labels.  */

        do_tz_offset:
          always_output_a_sign = true;
          goto do_number_body;

        do_yearish:
          if (pad == ZERO_PAD)
            pad = yr_spec;
          always_output_a_sign
            = (pad == SIGN_PAD
               && ((digits == 2 ? 99 : 9999) < u_number_value
                   || digits < width));
          goto do_maybe_signed_number;

        do_number_spacepad:
          if (pad == ZERO_PAD)
            pad = SPACE_PAD;

        do_number:
          /* Format NUMBER_VALUE according to the MODIFIER flag.  */
          negative_number = number_value < 0;
          u_number_value = number_value;

        do_signed_number:
          always_output_a_sign = false;

        do_maybe_signed_number:
          tz_colon_mask = 0;

        do_number_body:
          /* Format U_NUMBER_VALUE according to the MODIFIER flag.
             NEGATIVE_NUMBER is nonzero if the original number was
             negative; in this case it was converted directly to
             unsigned int (i.e., modulo (UINT_MAX + 1)) without
             negating it.  */
          if (modifier == L_('O') && !negative_number)
            {
#if SUPPORT_NON_GREG_CALENDARS_IN_STRFTIME
              if (cal != NULL)
                digits_base = cal->alt_digits_base;
#elif defined _NL_CURRENT
              /* Get the locale specific alternate representation of
                 the number.  If none exist NULL is returned.  */
              const CHAR_T *cp = nl_get_alt_digit (u_number_value
                                                   HELPER_LOCALE_ARG);

              if (cp != NULL)
                {
                  size_t digitlen = STRLEN (cp);
                  if (digitlen != 0)
                    {
                      cpy (digitlen, cp);
                      break;
                    }
                }
#endif
            }

          bufp = buf + sizeof (buf) / sizeof (buf[0]);

          if (negative_number)
            u_number_value = - u_number_value;

          do
            {
              if (tz_colon_mask & 1)
                *--bufp = ':';
              tz_colon_mask >>= 1;
#if SUPPORT_NON_GREG_CALENDARS_IN_STRFTIME
              *--bufp = u_number_value % 10 + (digits_base & 0xFF);
              if (digits_base >= 0x100)
                *--bufp = digits_base >> 8;
#else
              *--bufp = u_number_value % 10 + '0';
#endif
              u_number_value /= 10;
            }
          while (u_number_value != 0 || tz_colon_mask != 0);

        do_number_sign_and_padding:
          if (pad == ZERO_PAD)
            pad = ALWAYS_ZERO_PAD;
          if (width < 0)
            width = digits;

          {
            CHAR_T sign_char = (negative_number ? L_('-')
                                : always_output_a_sign ? L_('+')
                                : 0);
            int number_bytes = buf + sizeof buf / sizeof buf[0] - bufp;
            int number_digits = number_bytes;
#if SUPPORT_NON_GREG_CALENDARS_IN_STRFTIME
            if (digits_base >= 0x100)
              number_digits = number_bytes / 2;
#endif
            byte_count_t shortage = width - !!sign_char - number_digits;
            byte_count_t padding = (pad == NO_PAD || shortage <= 0
                                    ? 0 : shortage);

            if (sign_char)
              {
                if (pad == SPACE_PAD)
                  {
                    if (p)
                      memset_space (p, padding);
                    if (ckd_add (&i, i, padding) && FPRINTFTIME)
                      {
                        errno = ERANGE;
                        return FAILURE;
                      }
                    width -= padding;
                  }
                width_add1 (0, sign_char);
                width--;
              }

            cpy (number_bytes, bufp);
          }
          break;

        case L_('F'):
          if (modifier != 0)
            goto bad_format;
          if (pad == ZERO_PAD && width < 0)
            {
              pad = SIGN_PAD;
              subwidth = 4;
            }
          else
            {
              subwidth = width - 6;
              if (subwidth < 0)
                subwidth = 0;
            }
          subfmt = L_("%Y-%m-%d");
          goto subformat_width;

        case L_('H'):
          if (modifier == L_('E'))
            goto bad_format;

          DO_NUMBER (2, tp->tm_hour);

        case L_('I'):
          if (modifier == L_('E'))
            goto bad_format;

          DO_NUMBER (2, hour12);

        case L_('k'):           /* GNU extension.  */
          if (modifier == L_('E'))
            goto bad_format;

          DO_NUMBER_SPACEPAD (2, tp->tm_hour);

        case L_('l'):           /* GNU extension.  */
          if (modifier == L_('E'))
            goto bad_format;

          DO_NUMBER_SPACEPAD (2, hour12);

        case L_('j'):
          if (modifier == L_('E'))
            goto bad_format;

          DO_SIGNED_NUMBER (3, tp->tm_yday < -1, tp->tm_yday + 1U);

        case L_('M'):
          if (modifier == L_('E'))
            goto bad_format;

          DO_NUMBER (2, tp->tm_min);

        case L_('m'):
          if (modifier == L_('E'))
            goto bad_format;

#if SUPPORT_NON_GREG_CALENDARS_IN_STRFTIME
          if (cal != NULL)
            DO_SIGNED_NUMBER (2, false, caldate->month + 1U);
#endif
          DO_SIGNED_NUMBER (2, tp->tm_mon < -1, tp->tm_mon + 1U);

#ifndef _LIBC
        case L_('N'):           /* GNU extension.  */
          if (modifier == L_('E'))
            goto bad_format;
          {
            int n = ns, ns_digits = 9;
            if (width <= 0)
              width = ns_digits;
            int ndigs = ns_digits;
            while (width < ndigs || (1 < ndigs && n % 10 == 0))
              ndigs--, n /= 10;
            for (int j = ndigs; 0 < j; j--)
              buf[j - 1] = n % 10 + L_('0'), n /= 10;
            if (pad == ZERO_PAD)
              pad = ALWAYS_ZERO_PAD;
            width_cpy (0, ndigs, buf);
            width_add (width - ndigs, 0, (void) 0);
          }
          break;
#endif

        case L_('n'):
          add1 (L_('\n'));
          break;

        case L_('P'):
          to_lowcase = true;
#ifndef _NL_CURRENT
          format_char = L_('p');
#endif
          FALLTHROUGH;
        case L_('p'):
          if (change_case)
            {
              to_uppcase = false;
              to_lowcase = true;
            }
#if defined _NL_CURRENT || HAVE_ONLY_C_LOCALE || (USE_C_LOCALE && !HAVE_STRFTIME_L)
          cpy (ap_len, ampm);
          break;
#else
          goto underlying_strftime;
#endif

        case L_('q'):           /* GNU extension.  */
          DO_SIGNED_NUMBER (1, false, ((tp->tm_mon * 11) >> 5) + 1);

        case L_('R'):
          subfmt = L_("%H:%M");
          goto subformat;

        case L_('r'):
#ifdef _NL_CURRENT
          if (*(subfmt = (const CHAR_T *) _NL_CURRENT (LC_TIME,
                                                       NLW(T_FMT_AMPM)))
              == L_('\0'))
            subfmt = L_("%I:%M:%S %p");
          goto subformat;
#elif HAVE_ONLY_C_LOCALE || (USE_C_LOCALE && !HAVE_STRFTIME_L)
          subfmt = L_("%I:%M:%S %p");
          goto subformat;
#elif ((defined __APPLE__ && defined __MACH__) || defined __FreeBSD__ \
       || (defined _WIN32 && !defined __CYGWIN__))
          /* macOS, FreeBSD, native Windows strftime() may produce empty output
             for "%r".  */
          subfmt = L_("%I:%M:%S %p");
          goto subformat;
#else
          goto underlying_strftime;
#endif

        case L_('S'):
          if (modifier == L_('E'))
            goto bad_format;

          DO_NUMBER (2, tp->tm_sec);

        case L_('s'):           /* GNU extension.  */
          {
            struct tm ltm;
            time_t t;

            ltm = *tp;
            ltm.tm_yday = -1;
            t = mktime_z (tz, &ltm);
            if (ltm.tm_yday < 0)
              {
                errno = EOVERFLOW;
                return FAILURE;
              }

            /* Generate string value for T using time_t arithmetic;
               this works even if sizeof (long) < sizeof (time_t).  */

            bufp = buf + sizeof (buf) / sizeof (buf[0]);
            negative_number = t < 0;

            do
              {
                int d = t % 10;
                t /= 10;
                *--bufp = (negative_number ? -d : d) + L_('0');
              }
            while (t != 0);

            digits = 1;
            always_output_a_sign = false;
            goto do_number_sign_and_padding;
          }

        case L_('X'):
          if (modifier == L_('O'))
            goto bad_format;
#ifdef _NL_CURRENT
          if (! (modifier == L_('E')
                 && (*(subfmt =
                       (const CHAR_T *) _NL_CURRENT (LC_TIME, NLW(ERA_T_FMT)))
                     != L_('\0'))))
            subfmt = (const CHAR_T *) _NL_CURRENT (LC_TIME, NLW(T_FMT));
          goto subformat;
#elif HAVE_ONLY_C_LOCALE || (USE_C_LOCALE && !HAVE_STRFTIME_L)
          subfmt = L_("%H:%M:%S");
          goto subformat;
#else
          goto underlying_strftime;
#endif
        case L_('T'):
          subfmt = L_("%H:%M:%S");
          goto subformat;

        case L_('t'):
          add1 (L_('\t'));
          break;

        case L_('u'):
          DO_NUMBER (1, (tp->tm_wday - 1 + 7) % 7 + 1);

        case L_('U'):
          if (modifier == L_('E'))
            goto bad_format;

          DO_NUMBER (2, (tp->tm_yday - tp->tm_wday + 7) / 7);

        case L_('V'):
        case L_('g'):
        case L_('G'):
          if (modifier == L_('E'))
            goto bad_format;
          {
            /* YEAR is a leap year if and only if (tp->tm_year + TM_YEAR_BASE)
               is a leap year, except that YEAR and YEAR - 1 both work
               correctly even when (tp->tm_year + TM_YEAR_BASE) would
               overflow.  */
            int year = (tp->tm_year
                        + (tp->tm_year < 0
                           ? TM_YEAR_BASE % 400
                           : TM_YEAR_BASE % 400 - 400));
            int year_adjust = 0;
            int days = iso_week_days (tp->tm_yday, tp->tm_wday);

            if (days < 0)
              {
                /* This ISO week belongs to the previous year.  */
                year_adjust = -1;
                days = iso_week_days (tp->tm_yday + (365 + __isleap (year - 1)),
                                      tp->tm_wday);
              }
            else
              {
                int d = iso_week_days (tp->tm_yday - (365 + __isleap (year)),
                                       tp->tm_wday);
                if (0 <= d)
                  {
                    /* This ISO week belongs to the next year.  */
                    year_adjust = 1;
                    days = d;
                  }
              }

            switch (*f)
              {
              case L_('g'):
                {
                  int yy = (tp->tm_year % 100 + year_adjust) % 100;
                  DO_YEARISH (2, false,
                              (0 <= yy
                               ? yy
                               : tp->tm_year < -TM_YEAR_BASE - year_adjust
                               ? -yy
                               : yy + 100));
                }

              case L_('G'):
                DO_YEARISH (4, tp->tm_year < -TM_YEAR_BASE - year_adjust,
                            (tp->tm_year + (unsigned int) TM_YEAR_BASE
                             + year_adjust));

              default:
                DO_NUMBER (2, days / 7 + 1);
              }
          }

        case L_('W'):
          if (modifier == L_('E'))
            goto bad_format;

          DO_NUMBER (2, (tp->tm_yday - (tp->tm_wday - 1 + 7) % 7 + 7) / 7);

        case L_('w'):
          if (modifier == L_('E'))
            goto bad_format;

          DO_NUMBER (1, tp->tm_wday);

        case L_('Y'):
          if (modifier == L_('E'))
            {
#if HAVE_STRUCT_ERA_ENTRY
              struct era_entry *era = _nl_get_era_entry (tp HELPER_LOCALE_ARG);
              if (era)
                {
# ifdef COMPILE_WIDE
                  subfmt = era->era_wformat;
# else
                  subfmt = era->era_format;
# endif
                  if (pad == ZERO_PAD)
                    pad = yr_spec;
                  goto subformat;
                }
#elif HAVE_ONLY_C_LOCALE || (USE_C_LOCALE && !HAVE_STRFTIME_L)
#else
              goto underlying_strftime;
#endif
            }

#if SUPPORT_NON_GREG_CALENDARS_IN_STRFTIME
          if (cal != NULL)
            DO_YEARISH (4, false, caldate->year);
#endif
          DO_YEARISH (4, tp->tm_year < -TM_YEAR_BASE,
                      tp->tm_year + (unsigned int) TM_YEAR_BASE);

        case L_('y'):
          if (modifier == L_('E'))
            {
#if HAVE_STRUCT_ERA_ENTRY
              struct era_entry *era = _nl_get_era_entry (tp HELPER_LOCALE_ARG);
              if (era)
                {
                  int delta = tp->tm_year - era->start_date[0];
                  if (pad == ZERO_PAD)
                    pad = yr_spec;
                  DO_NUMBER (2, (era->offset
                                 + delta * era->absolute_direction));
                }
#elif HAVE_ONLY_C_LOCALE || (USE_C_LOCALE && !HAVE_STRFTIME_L)
#else
              goto underlying_strftime;
#endif
            }

          {
            int yy = tp->tm_year % 100;
            if (yy < 0)
              yy = tp->tm_year < - TM_YEAR_BASE ? -yy : yy + 100;
            DO_YEARISH (2, false, yy);
          }

        case L_('Z'):
          if (change_case)
            {
              to_uppcase = false;
              to_lowcase = true;
            }

         {
            char const *zone;
#ifdef _LIBC
            zone = tp->tm_zone;
            /* The tzset() call might have changed the value.  */
            if (!(zone && *zone) && tp->tm_isdst >= 0)
              {
                /* POSIX.1 requires that local time zone information be used as
                   though strftime called tzset.  */
                if (!*tzset_called)
                  {
                    tzset ();
                    *tzset_called = true;
                  }
                zone = tp->tm_isdst <= 1 ? tzname[tp->tm_isdst] : "?";
              }
            if (! zone)
              zone = "";
#else
            char zonebuf[128]; /* Enough for any time zone abbreviation.  */
            zone = get_tm_zone (tz, zonebuf, sizeof zonebuf, modifier, tp);
#endif

#ifdef COMPILE_WIDE
            /* The zone string is always given in multibyte form.  We have
               to convert it to wide character.  */
            size_t w = pad == NO_PAD || width < 0 ? 0 : width;
            char const *z = zone;
            mbstate_t st = {0};
            size_t len = __mbsrtowcs_l (p, &z, maxsize - i, &st, loc);
            if (len == (size_t) -1)
              return FAILURE;
            size_t incr = len < w ? w : len;
            if (incr >= maxsize - i)
              {
                errno = ERANGE;
                return FAILURE;
              }
            if (p)
              {
                if (len < w)
                  {
                    size_t delta = w - len;
                    __wmemmove (p + delta, p, len);
                    wchar_t wc = (pad == ALWAYS_ZERO_PAD || pad == SIGN_PAD
                                  ? L'0' : L' ');
                    wmemset (p, wc, delta);
                  }
                p += incr;
              }
            i += incr;
#else
            cpy (strlen (zone), zone);
#endif
          }
          break;

        case L_(':'):
          /* :, ::, and ::: are valid only just before 'z'.
             :::: etc. are rejected later.  */
          for (colons = 1; f[colons] == L_(':'); colons++)
            continue;
          if (f[colons] != L_('z'))
            goto bad_format;
          f += colons;
          goto do_z_conversion;

        case L_('z'):
          colons = 0;

        do_z_conversion:
          if (tp->tm_isdst < 0)
            break;

          {
            int diff;
            int hour_diff;
            int min_diff;
            int sec_diff;
#if HAVE_STRUCT_TM_TM_GMTOFF
            diff = tp->tm_gmtoff;
#else
            if (!tz)
              diff = 0;
            else
              {
                struct tm gtm;
                struct tm ltm;
                time_t lt;

                ltm = *tp;
                ltm.tm_wday = -1;
                lt = mktime_z (tz, &ltm);
                if (ltm.tm_wday < 0 || ! localtime_rz (0, &lt, &gtm))
                  break;
                diff = tm_diff (&ltm, &gtm);
              }
#endif

            negative_number = diff < 0;
            if (diff == 0)
              {
                char zonebuf[128]; /* Enough for any time zone abbreviation.  */
                negative_number = (*get_tm_zone (tz, zonebuf, sizeof zonebuf,
                                                 0, tp)
                                   == '-');
              }
            hour_diff = diff / 60 / 60;
            min_diff = diff / 60 % 60;
            sec_diff = diff % 60;

            switch (colons)
              {
              case 0: /* +hhmm */
                DO_TZ_OFFSET (5, 0, hour_diff * 100 + min_diff);

              case 1: tz_hh_mm: /* +hh:mm */
                DO_TZ_OFFSET (6, 04, hour_diff * 100 + min_diff);

              case 2: tz_hh_mm_ss: /* +hh:mm:ss */
                DO_TZ_OFFSET (9, 024,
                              hour_diff * 10000 + min_diff * 100 + sec_diff);

              case 3: /* +hh if possible, else +hh:mm, else +hh:mm:ss */
                if (sec_diff != 0)
                  goto tz_hh_mm_ss;
                if (min_diff != 0)
                  goto tz_hh_mm;
                DO_TZ_OFFSET (3, 0, hour_diff);

              default:
                goto bad_format;
              }
          }

        case L_('\0'):          /* GNU extension: % at end of format.  */
        bad_percent:
            --f;
            FALLTHROUGH;
        default:
          /* Unknown format; output the format, including the '%',
             since this is most likely the right thing to do if a
             multibyte string has been misparsed.  */
        bad_format:
          cpy (f - percent + 1, percent);
          break;
        }
    }

#if ! FPRINTFTIME
  if (p && maxsize != 0)
    *p = L_('\0');
#endif

#if FAILURE == 0
  errno = saved_errno;
#endif

  return i;
}
