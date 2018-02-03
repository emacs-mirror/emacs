/* Selection processing for Emacs on the Microsoft W32 API.

Copyright (C) 1993-1994, 2001-2018 Free Software Foundation, Inc.

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
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#ifndef W32SELECT_H
#define W32SELECT_H
#include <windows.h>

extern void syms_of_w32select (void);
extern void globals_of_w32select (void);
extern void term_w32select (void);

#endif
