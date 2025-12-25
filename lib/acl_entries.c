/* Return the number of entries in an ACL.

   Copyright (C) 2002-2003, 2005-2025 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.

   Written by Paul Eggert and Andreas Gruenbacher.  */

#include <config.h>

#include "acl-internal.h"

/* This file assumes POSIX-draft like ACLs
   (Linux, FreeBSD, NetBSD >= 10, Mac OS X, Cygwin >= 2.5).

   It is compiled only on systems that do not have the acl_entries() function
   (in libc or libacl).  */

/* Return the number of entries in ACL.
   Return -1 and set errno upon failure to determine it.  */

int
acl_entries (acl_t acl)
{
  int count = 0;

  if (acl != NULL)
    {
#if HAVE_ACL_FIRST_ENTRY /* Linux, FreeBSD, NetBSD >= 10, Mac OS X, Cygwin >= 2.5 */
# if HAVE_ACL_TYPE_EXTENDED /* Mac OS X */
      /* acl_get_entry returns 0 when it successfully fetches an entry,
         and -1/EINVAL at the end.  */
      acl_entry_t ace;
      int got_one;

      for (got_one = acl_get_entry (acl, ACL_FIRST_ENTRY, &ace);
           got_one >= 0;
           got_one = acl_get_entry (acl, ACL_NEXT_ENTRY, &ace))
        count++;
# else /* Linux, FreeBSD, NetBSD >= 10, Cygwin >= 2.5 */
      /* acl_get_entry returns 1 when it successfully fetches an entry,
         and 0 at the end.  */
      acl_entry_t ace;
      int got_one;

      for (got_one = acl_get_entry (acl, ACL_FIRST_ENTRY, &ace);
           got_one > 0;
           got_one = acl_get_entry (acl, ACL_NEXT_ENTRY, &ace))
        count++;
      if (got_one < 0)
        return -1;
# endif
#endif
    }

  return count;
}
