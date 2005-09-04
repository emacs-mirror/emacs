/* Replacement pwd.h file for building GNU Emacs on the Macintosh.
   Copyright (C) 1999, 2000, 2002, 2003, 2004,
      2005 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* Contributed by Andrew Choi (akochoi@mac.com).  */

#ifndef _PWD_H
#define _PWD_H

#include <sys/types.h>

/* Emacs uses only pw_name and pw_dir: let's just simulate these */
struct passwd {
  char *pw_name;		/* user name */
  char *pw_dir;			/* home directory */
};

struct passwd *getpwuid(uid_t);
struct passwd *getpwnam(const char *);

#endif /* _PWD_H */

/* arch-tag: e169cad7-12ca-4660-a35e-36f80d5d345f
   (do not change this comment) */
