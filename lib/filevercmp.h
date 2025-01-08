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

#ifndef FILEVERCMP_H
#define FILEVERCMP_H

/* This file uses _GL_ATTRIBUTE_PURE.  */
#if !_GL_CONFIG_H_INCLUDED
 #error "Please include config.h first."
#endif

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif


/* Compare strings A and B as file names containing version numbers,
   and return an integer that is negative, zero, or positive depending
   on whether A compares less than, equal to, or greater than B.

   Use the following version sort algorithm:

     1. Compare the strings' maximal-length non-digit prefixes lexically.
        If there is a difference return that difference.
        Otherwise discard the prefixes and continue with the next step.

     2. Compare the strings' maximal-length digit prefixes, using
        numeric comparison of the numbers represented by each prefix.
        (Treat an empty prefix as zero; this can happen only at string end.)
        If there is a difference, return that difference.
        Otherwise discard the prefixes and continue with the next step.

     3. If both strings are empty, return 0.  Otherwise continue with step 1.

   In version sort, lexical comparison is left to right, byte by byte,
   using the byte's numeric value (0-255), except that:

     1. ASCII letters sort before other bytes.
     2. A tilde sorts before anything, even an empty string.

   In addition to the version sort rules, the following strings have
   special priority and sort before all other strings (listed in order):

     1. The empty string.
     2. ".".
     3. "..".
     4. Strings starting with "." sort before other strings.

   Before comparing two strings where both begin with non-".",
   or where both begin with "." but neither is "." or "..",
   suffixes matching the C-locale extended regular expression
   (\.[A-Za-z~][A-Za-z0-9~]*)*$ are removed and the strings compared
   without them, using version sort without special priority;
   if they do not compare equal, this comparison result is used and
   the suffixes are effectively ignored.  Otherwise, the entire
   strings are compared using version sort.  When removing a suffix
   from a nonempty string, remove the maximal-length suffix such that
   the remaining string is nonempty.

   This function is intended to be a replacement for strverscmp.  */
int filevercmp (char const *a, char const *b) _GL_ATTRIBUTE_PURE;

/* Like filevercmp, except compare the byte arrays A (of length ALEN)
   and B (of length BLEN) so that A and B can contain '\0', which
   sorts just before '\1'.  But if ALEN is -1 treat A as a string
   terminated by '\0', and similarly for BLEN.  */
int filenvercmp (char const *a, ptrdiff_t alen, char const *b, ptrdiff_t blen)
  _GL_ATTRIBUTE_PURE;


#ifdef __cplusplus
}
#endif

#endif /* FILEVERCMP_H */
