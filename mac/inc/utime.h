/* Replacement utime.h file for building GNU Emacs on the Macintosh.
   Copyright (C) 2000 Free Software Foundation, Inc.

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

/* Contributed by Andrew Choi (akochoi@mac.com).  */

#ifndef _UTIME_H_
#define _UTIME_H_

#include <time.h>

#define _mac_unix_epoch_offset_  (365L * 4L) * 24L * 60L * 60L

struct utimbuf {
	time_t actime;					/* access time (ignored on the Mac) */
	time_t modtime;					/* modification time */
};

int utime(const char *path, const struct utimbuf *buf);

#endif
