/* save-cwd.c -- Save and restore current working directory.

   Copyright (C) 1995, 1997-1998, 2003-2006, 2009-2020 Free Software
   Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* Gnulib needs to save and restore the current working directory to
   fully emulate functions like fstatat.  But Emacs doesn't care what
   the current working directory is; it always uses absolute file
   names.  This module replaces the Gnulib module by omitting the code
   that Emacs does not need.  */

#include <config.h>

#include "save-cwd.h"

#include <fcntl.h>
#include <unistd.h>

/* Record the location of the current working directory in CWD so that
   the program may change to other directories and later use restore_cwd
   to return to the recorded location.  This function may allocate
   space using malloc (via getcwd) or leave a file descriptor open;
   use free_cwd to perform the necessary free or close.  Upon failure,
   no memory is allocated, any locally opened file descriptors are
   closed;  return non-zero -- in that case, free_cwd need not be
   called, but doing so is ok.  Otherwise, return zero.

   The _raison d'etre_ for this interface is that the working directory
   is sometimes inaccessible, and getcwd is not robust or as efficient.
   So, we prefer to use the open/fchdir approach, but fall back on
   getcwd if necessary.  This module works for most cases with just
   the getcwd-lgpl module, but to be truly robust, use the getcwd module.

   Some systems lack fchdir altogether: e.g., OS/2, pre-2001 Cygwin,
   SCO Xenix.  Also, SunOS 4 and Irix 5.3 provide the function, yet it
   doesn't work for partitions on which auditing is enabled.  If
   you're still using an obsolete system with these problems, please
   send email to the maintainer of this code.  */

#if !defined HAVE_FCHDIR && !defined fchdir
# define fchdir(fd) (-1)
#endif

int
save_cwd (struct saved_cwd *cwd)
{
  cwd->desc = open (".", O_SEARCH | O_CLOEXEC);
  /* The 'name' member is present only to minimize differences from
     gnulib.  Initialize it to zero, if only to simplify debugging.  */
  cwd->name = 0;
  return 0;
}

/* Change to recorded location, CWD, in directory hierarchy.
   Upon failure, return -1 (errno is set by chdir or fchdir).
   Upon success, return zero.  */

int
restore_cwd (const struct saved_cwd *cwd)
{
  /* Restore the previous directory if possible, to avoid tying down
     the file system of the new directory (Bug#18232).
     Don't worry if fchdir fails, as Emacs doesn't care what the
     working directory is.  The fchdir call is inside an 'if' merely to
     pacify compilers that complain if fchdir's return value is ignored.  */
  if (fchdir (cwd->desc) == 0)
    return 0;

  return 0;
}

void
free_cwd (struct saved_cwd *cwd)
{
  close (cwd->desc);
}
