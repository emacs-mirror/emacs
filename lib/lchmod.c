/* Implement lchmod on platforms where it does not work correctly.

   Copyright 2020-2022 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* written by Paul Eggert */

#include <config.h>

/* Specification.  */
#include <sys/stat.h>

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <intprops.h>

/* Work like chmod, except when FILE is a symbolic link.
   In that case, on systems where permissions on symbolic links are unsupported
   (such as Linux), set errno to EOPNOTSUPP and return -1.  */

int
lchmod (char const *file, mode_t mode)
{
  char readlink_buf[1];

#ifdef O_PATH
  /* Open a file descriptor with O_NOFOLLOW, to make sure we don't
     follow symbolic links, if /proc is mounted.  O_PATH is used to
     avoid a failure if the file is not readable.
     Cf. <https://sourceware.org/bugzilla/show_bug.cgi?id=14578>  */
  int fd = open (file, O_PATH | O_NOFOLLOW | O_CLOEXEC);
  if (fd < 0)
    return fd;

  int err;
  if (0 <= readlinkat (fd, "", readlink_buf, sizeof readlink_buf))
    err = EOPNOTSUPP;
  else if (errno == EINVAL)
    {
      static char const fmt[] = "/proc/self/fd/%d";
      char buf[sizeof fmt - sizeof "%d" + INT_BUFSIZE_BOUND (int)];
      sprintf (buf, fmt, fd);
      err = chmod (buf, mode) == 0 ? 0 : errno == ENOENT ? -1 : errno;
    }
  else
    err = errno == ENOENT ? -1 : errno;

  close (fd);

  errno = err;
  if (0 <= err)
    return err == 0 ? 0 : -1;
#endif

  size_t len = strlen (file);
  if (len && file[len - 1] == '/')
    {
      struct stat st;
      if (lstat (file, &st) < 0)
        return -1;
      if (!S_ISDIR (st.st_mode))
        {
          errno = ENOTDIR;
          return -1;
        }
    }

  /* O_PATH + /proc is not supported.  */

  if (0 <= readlink (file, readlink_buf, sizeof readlink_buf))
    {
      errno = EOPNOTSUPP;
      return -1;
    }

  /* Fall back on chmod, despite a possible race.  */
  return chmod (file, mode);
}
