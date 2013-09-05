/* Do not save and restore the current working directory.

   Copyright 2013 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* Gnulib needs to save and restore the current working directory to
   fully emulate functions like fstatat.  But Emacs doesn't care what
   the current working directory is; it always uses absolute file
   names.  This module replaces the Gnulib module by omitting the code
   that Emacs does not need.  */

#ifndef SAVE_CWD_H
#define SAVE_CWD_H 1

_GL_INLINE_HEADER_BEGIN
#ifndef SAVE_CWD_INLINE
# define SAVE_CWD_INLINE _GL_INLINE
#endif

struct saved_cwd { int desc; };

SAVE_CWD_INLINE int
save_cwd (struct saved_cwd *cwd)
{
  cwd->desc = -1;
  return 0;
}

SAVE_CWD_INLINE int restore_cwd (struct saved_cwd const *cwd) { return 0; }
SAVE_CWD_INLINE void free_cwd (struct saved_cwd *cwd) { }

_GL_INLINE_HEADER_END

#endif
