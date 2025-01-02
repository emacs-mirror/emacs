/* Compare file names containing version numbers.

   Copyright (C) 1995 Ian Jackson <iwj10@cus.cam.ac.uk>
   Copyright (C) 2001 Anthony Towns <aj@azure.humbug.org.au>
   Copyright (C) 2008-2025 Free Software Foundation, Inc.

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

#include <config.h>
#include "filevercmp.h"

#include <c-ctype.h>
#include <limits.h>
#include <idx.h>

/* Return the length of a prefix of S that corresponds to the suffix
   defined by this extended regular expression in the C locale:
     (\.[A-Za-z~][A-Za-z0-9~]*)*$
   Use the longest suffix matching this regular expression,
   except do not use all of S as a suffix if S is nonempty.
   If *LEN is -1, S is a string; set *LEN to S's length.
   Otherwise, *LEN should be nonnegative, S is a char array,
   and *LEN does not change.  */
static idx_t
file_prefixlen (char const *s, ptrdiff_t *len)
{
  size_t n = *len;  /* SIZE_MAX if N == -1.  */
  idx_t prefixlen = 0;

  for (idx_t i = 0; ; )
    {
      if (*len < 0 ? !s[i] : i == n)
        {
          *len = i;
          return prefixlen;
        }

      i++;
      prefixlen = i;
      while (i + 1 < n && s[i] == '.' && (c_isalpha (s[i + 1])
                                          || s[i + 1] == '~'))
        for (i += 2; i < n && (c_isalnum (s[i]) || s[i] == '~'); i++)
          continue;
    }
}

/* Return a version sort comparison value for S's byte at position POS.
   S has length LEN.  If POS == LEN, sort before all non-'~' bytes.  */

static int
order (char const *s, idx_t pos, idx_t len)
{
  if (pos == len)
    return -1;

  unsigned char c = s[pos];
  if (c_isdigit (c))
    return 0;
  else if (c_isalpha (c))
    return c;
  else if (c == '~')
    return -2;
  else
    {
      static_assert (UCHAR_MAX <= (INT_MAX - 1 - 2) / 2);
      return c + UCHAR_MAX + 1;
    }
}

/* slightly modified verrevcmp function from dpkg
   S1, S2 - compared char array
   S1_LEN, S2_LEN - length of arrays to be scanned

   This implements the algorithm for comparison of version strings
   specified by Debian and now widely adopted.  The detailed
   specification can be found in the Debian Policy Manual in the
   section on the 'Version' control field.  This version of the code
   implements that from s5.6.12 of Debian Policy v3.8.0.1
   https://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Version */
static int _GL_ATTRIBUTE_PURE
verrevcmp (const char *s1, idx_t s1_len, const char *s2, idx_t s2_len)
{
  idx_t s1_pos = 0;
  idx_t s2_pos = 0;
  while (s1_pos < s1_len || s2_pos < s2_len)
    {
      int first_diff = 0;
      while ((s1_pos < s1_len && !c_isdigit (s1[s1_pos]))
             || (s2_pos < s2_len && !c_isdigit (s2[s2_pos])))
        {
          int s1_c = order (s1, s1_pos, s1_len);
          int s2_c = order (s2, s2_pos, s2_len);
          if (s1_c != s2_c)
            return s1_c - s2_c;
          s1_pos++;
          s2_pos++;
        }
      while (s1_pos < s1_len && s1[s1_pos] == '0')
        s1_pos++;
      while (s2_pos < s2_len && s2[s2_pos] == '0')
        s2_pos++;
      while (s1_pos < s1_len && s2_pos < s2_len
             && c_isdigit (s1[s1_pos]) && c_isdigit (s2[s2_pos]))
        {
          if (!first_diff)
            first_diff = s1[s1_pos] - s2[s2_pos];
          s1_pos++;
          s2_pos++;
        }
      if (s1_pos < s1_len && c_isdigit (s1[s1_pos]))
        return 1;
      if (s2_pos < s2_len && c_isdigit (s2[s2_pos]))
        return -1;
      if (first_diff)
        return first_diff;
    }
  return 0;
}

/* Compare version strings S1 and S2.
   See filevercmp.h for function description.  */
int
filevercmp (const char *s1, const char *s2)
{
  return filenvercmp (s1, -1, s2, -1);
}

/* Compare versions A (of length ALEN) and B (of length BLEN).
   See filevercmp.h for function description.  */
int
filenvercmp (char const *a, ptrdiff_t alen, char const *b, ptrdiff_t blen)
{
  /* Special case for empty versions.  */
  bool aempty = alen < 0 ? !a[0] : !alen;
  bool bempty = blen < 0 ? !b[0] : !blen;
  if (aempty)
    return -!bempty;
  if (bempty)
    return 1;

  /* Special cases for leading ".": "." sorts first, then "..", then
     other names with leading ".", then other names.  */
  if (a[0] == '.')
    {
      if (b[0] != '.')
        return -1;

      bool adot = alen < 0 ? !a[1] : alen == 1;
      bool bdot = blen < 0 ? !b[1] : blen == 1;
      if (adot)
        return -!bdot;
      if (bdot)
        return 1;

      bool adotdot = a[1] == '.' && (alen < 0 ? !a[2] : alen == 2);
      bool bdotdot = b[1] == '.' && (blen < 0 ? !b[2] : blen == 2);
      if (adotdot)
        return -!bdotdot;
      if (bdotdot)
        return 1;
    }
  else if (b[0] == '.')
    return 1;

  /* Cut file suffixes.  */
  idx_t aprefixlen = file_prefixlen (a, &alen);
  idx_t bprefixlen = file_prefixlen (b, &blen);

  /* If both suffixes are empty, a second pass would return the same thing.  */
  bool one_pass_only = aprefixlen == alen && bprefixlen == blen;

  int result = verrevcmp (a, aprefixlen, b, bprefixlen);

  /* Return the initial result if nonzero, or if no second pass is needed.
     Otherwise, restore the suffixes and try again.  */
  return result || one_pass_only ? result : verrevcmp (a, alen, b, blen);
}
