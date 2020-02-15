/* Header file for the Emacs build fingerprint.

Copyright (C) 2016, 2018-2020 Free Software Foundation, Inc.

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

#ifndef EMACS_FINGERPRINT_H
#define EMACS_FINGERPRINT_H

/* We generate fingerprint.c and fingerprint.o from all the sources in
   Emacs.  This way, we have a unique value that we can use to pair
   data files (like a dump file) with a specific build of Emacs.  */
extern volatile unsigned char fingerprint[32];

#endif
