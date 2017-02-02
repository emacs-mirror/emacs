/* Standard I/O for Emacs.

Copyright 2013-2017 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#ifndef EMACS_SYSSTDIO_H
#define EMACS_SYSSTDIO_H

#include <fcntl.h>
#include <stdio.h>

extern FILE *emacs_fopen (char const *, char const *);

#if O_BINARY
# define FOPEN_BINARY "b"
# define FOPEN_TEXT "t"
#else
# define FOPEN_BINARY ""
# define FOPEN_TEXT ""
#endif

#endif /* EMACS_SYSSTDIO_H */
