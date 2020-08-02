/* Implement lchmod on platforms where it does not work correctly.

   Copyright 2020 Free Software Foundation, Inc.

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

/* written by Paul Eggert */

#include <config.h>

/* Specification.  */
#include <sys/stat.h>

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

#ifdef __osf__
/* Write "sys/stat.h" here, not <sys/stat.h>, otherwise OSF/1 5.1 DTK cc
   eliminates this include because of the preliminary #include <sys/stat.h>
   above.  */
# include "sys/stat.h"
#else
# include <sys/stat.h>
#endif

#include <intprops.h>

/* Work like chmod, except when FILE is a symbolic link.
   In that case, on systems where permissions on symbolic links are unsupported
   (such as Linux), set errno to EOPNOTSUPP and return -1.  */

int
lchmod (char const *file, mode_t mode)
{
#if defined O_PATH && defined AT_EMPTY_PATH
  /* Open a file descriptor with O_NOFOLLOW, to make sure we don't
     follow symbolic links, if /proc is mounted.  O_PATH is used to
     avoid a failure if the file is not readable.
     Cf. <https://sourceware.org/bugzilla/show_bug.cgi?id=14578>  */
  int fd = open (file, O_PATH | O_NOFOLLOW | O_CLOEXEC);
  if (fd < 0)
    return fd;

  /* Up to Linux 5.3 at least, when FILE refers to a symbolic link, the
     chmod call below will change the permissions of the symbolic link
     - which is undesired - and on many file systems (ext4, btrfs, jfs,
     xfs, ..., but not reiserfs) fail with error EOPNOTSUPP - which is
     misleading.  Therefore test for a symbolic link explicitly.
     Use fstatat because fstat does not work on O_PATH descriptors
     before Linux 3.6.  */
  struct stat st;
  if (fstatat (fd, "", &st, AT_EMPTY_PATH) != 0)
    {
      int stat_errno = errno;
      close (fd);
      errno = stat_errno;
      return -1;
    }
  if (S_ISLNK (st.st_mode))
    {
      close (fd);
      errno = EOPNOTSUPP;
      return -1;
    }

# if defined __linux__ || defined __ANDROID__ || defined __CYGWIN__
  static char const fmt[] = "/proc/self/fd/%d";
  char buf[sizeof fmt - sizeof "%d" + INT_BUFSIZE_BOUND (int)];
  sprintf (buf, fmt, fd);
  int chmod_result = chmod (buf, mode);
  int chmod_errno = errno;
  close (fd);
  if (chmod_result == 0)
    return chmod_result;
  if (chmod_errno != ENOENT)
    {
      errno = chmod_errno;
      return chmod_result;
    }
# endif
  /* /proc is not mounted or would not work as in GNU/Linux.  */

#elif HAVE_LSTAT
  struct stat st;
  int lstat_result = lstat (file, &st);
  if (lstat_result != 0)
    return lstat_result;
  if (S_ISLNK (st.st_mode))
    {
      errno = EOPNOTSUPP;
      return -1;
    }
#endif

  /* Fall back on chmod, despite a possible race.  */
  return chmod (file, mode);
}
