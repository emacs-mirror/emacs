/* Definitions needed by most editing commands.
   Copyright (C) 1985, 1994, 2001-2026 Free Software Foundation, Inc.

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

#ifndef EMACS_COMMANDS_H
#define EMACS_COMMANDS_H

#include "lisp.h"

#define Ctl(c) ((c)&037)

/* Nonzero if input is coming from the keyboard.  */

#define INTERACTIVE (NILP (Vexecuting_kbd_macro) && !noninteractive)

#endif /* EMACS_COMMANDS_H */
