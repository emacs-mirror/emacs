/* Information about executables.

   Copyright (C) 2012-2018 Free Software Foundation, Inc.

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

/* Written by Paul Eggert.  */

#ifndef _GL_EXECINFO_H
#define _GL_EXECINFO_H

#ifndef _GL_INLINE_HEADER_BEGIN
 #error "Please include config.h first."
#endif
_GL_INLINE_HEADER_BEGIN
#ifndef _GL_EXECINFO_INLINE
# define _GL_EXECINFO_INLINE _GL_INLINE
#endif

_GL_EXECINFO_INLINE int
backtrace (void **buffer, int size)
{
  (void) buffer;
  (void) size;
  return 0;
}

_GL_EXECINFO_INLINE char **
backtrace_symbols (void *const *buffer, int size)
{
  (void) buffer;
  (void) size;
  return 0;
}

_GL_EXECINFO_INLINE void
backtrace_symbols_fd (void *const *buffer, int size, int fd)
{
  (void) buffer;
  (void) size;
  (void) fd;
}

_GL_INLINE_HEADER_END

#endif
