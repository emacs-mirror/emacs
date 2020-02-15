/* Stub for copy_file_range
   Copyright 2019-2020 Free Software Foundation, Inc.

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

#include <config.h>

#include <unistd.h>

#include <errno.h>

ssize_t
copy_file_range (int infd, off_t *pinoff,
                 int outfd, off_t *poutoff,
                 size_t length, unsigned int flags)
{
  /* There is little need to emulate copy_file_range with read+write,
     since programs that use copy_file_range must fall back on
     read+write anyway.  */
  errno = ENOSYS;
  return -1;
}
