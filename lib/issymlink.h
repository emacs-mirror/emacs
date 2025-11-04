/* Test whether a file is a symbolic link.
   Copyright (C) 2025 Free Software Foundation, Inc.

   This file is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation; either version 2.1 of the
   License, or (at your option) any later version.

   This file is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

#ifndef _ISSYMLINK_H
#define _ISSYMLINK_H

/* This file uses _GL_ARG_NONNULL, _GL_INLINE.  */
#if !_GL_CONFIG_H_INCLUDED
 #error "Please include config.h first."
#endif

#include <errno.h>
#include <unistd.h> /* for readlink, readlinkat */


_GL_INLINE_HEADER_BEGIN

#ifndef _GL_ISSYMLINK_INLINE
# define _GL_ISSYMLINK_INLINE _GL_INLINE
#endif
#ifndef _GL_ISSYMLINKAT_INLINE
# define _GL_ISSYMLINKAT_INLINE _GL_INLINE
#endif

#if GNULIB_ISSYMLINK
/* Tests whether FILENAME represents a symbolic link.
   This function is more reliable than lstat() / fstatat() followed by S_ISLNK,
   because it avoids possible EOVERFLOW errors.
   Returns
     1                      if FILENAME is a symbolic link,
     0                      if FILENAME exists and is not a symbolic link,
    -1 with errno set       if determination failed, in particular
    -1 with errno = ENOENT or ENOTDIR  if FILENAME does not exist.  */
# ifdef __cplusplus
extern "C" {
# endif
_GL_ISSYMLINK_INLINE int issymlink (const char *filename)
     _GL_ARG_NONNULL ((1));
_GL_ISSYMLINK_INLINE int
issymlink (const char *filename)
{
  char linkbuf[1];
  if (readlink (filename, linkbuf, sizeof (linkbuf)) >= 0)
    return 1;
  if (errno == EINVAL)
    return 0;
  else
    return -1;
}
# ifdef __cplusplus
}
# endif
#endif

#if GNULIB_ISSYMLINKAT
/* Tests whether FILENAME represents a symbolic link.
   This function is more reliable than lstat() / fstatat() followed by S_ISLNK,
   because it avoids possible EOVERFLOW errors.
   If FILENAME is a relative file name, it is interpreted as relative to the
   directory referred to by FD (where FD = AT_FDCWD denotes the current
   directory).
   Returns
     1                      if FILENAME is a symbolic link,
     0                      if FILENAME exists and is not a symbolic link,
    -1 with errno set       if determination failed, in particular
    -1 with errno = ENOENT or ENOTDIR  if FILENAME does not exist.  */
# ifdef __cplusplus
extern "C" {
# endif
_GL_ISSYMLINKAT_INLINE int issymlinkat (int fd, const char *filename)
     _GL_ARG_NONNULL ((2));
_GL_ISSYMLINKAT_INLINE int
issymlinkat (int fd, const char *filename)
{
  char linkbuf[1];
  if (readlinkat (fd, filename, linkbuf, sizeof (linkbuf)) >= 0)
    return 1;
  if (errno == EINVAL)
    return 0;
  else
    return -1;
}
# ifdef __cplusplus
}
# endif
#endif

_GL_INLINE_HEADER_END

#endif /* _ISSYMLINK_H */
