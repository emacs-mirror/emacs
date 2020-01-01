/* Check the access rights of a file relative to an open directory.
   Copyright (C) 2009-2020 Free Software Foundation, Inc.

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

/* written by Eric Blake */

/* If the user's config.h happens to include <unistd.h>, let it include only
   the system's <unistd.h> here, so that orig_faccessat doesn't recurse to
   rpl_faccessat.  */
#define _GL_INCLUDING_UNISTD_H
#include <config.h>

/* Specification.  */
#include <unistd.h>

#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#undef _GL_INCLUDING_UNISTD_H

#if HAVE_FACCESSAT
static int
orig_faccessat (int fd, char const *name, int mode, int flag)
{
  return faccessat (fd, name, mode, flag);
}
#endif

/* Write "unistd.h" here, not <unistd.h>, otherwise OSF/1 5.1 DTK cc
   eliminates this include because of the preliminary #include <unistd.h>
   above.  */
#include "unistd.h"

#ifndef HAVE_ACCESS
/* Mingw lacks access, but it also lacks real vs. effective ids, so
   the gnulib euidaccess module is good enough.  */
# undef access
# define access euidaccess
#endif

#if HAVE_FACCESSAT

int
rpl_faccessat (int fd, char const *file, int mode, int flag)
{
  int result = orig_faccessat (fd, file, mode, flag);

  if (result == 0 && file[strlen (file) - 1] == '/')
    {
      struct stat st;
      result = fstatat (fd, file, &st, 0);
      if (result == 0 && !S_ISDIR (st.st_mode))
        {
          errno = ENOTDIR;
          return -1;
        }
    }

  return result;
}

#else /* !HAVE_FACCESSAT */

/* Invoke access or euidaccess on file, FILE, using mode MODE, in the directory
   open on descriptor FD.  If possible, do it without changing the
   working directory.  Otherwise, resort to using save_cwd/fchdir, then
   (access|euidaccess)/restore_cwd.  If either the save_cwd or the
   restore_cwd fails, then give a diagnostic and exit nonzero.
   Note that this implementation only supports AT_EACCESS, although some
   native versions also support AT_SYMLINK_NOFOLLOW.  */

# define AT_FUNC_NAME faccessat
# define AT_FUNC_F1 euidaccess
# define AT_FUNC_F2 access
# define AT_FUNC_USE_F1_COND AT_EACCESS
# define AT_FUNC_POST_FILE_PARAM_DECLS , int mode, int flag
# define AT_FUNC_POST_FILE_ARGS        , mode
# include "at-func.c"

#endif
