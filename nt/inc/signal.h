/* Addition to system header signal.h file for building GNU Emacs on
   Windows.  It is needed because Posix mandates sig2str should appear
   in signal.h, and Gnulib insists on assuming that, and the MinGW build
   cannot use Gnulib's signal.h.

Copyright (C) 2024-2026 Free Software Foundation, Inc.

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

#ifndef _NT_INC_SIGNAL_H_
# define _NT_INC_SIGNAL_H_

/* Maximum size of a signal name returned by sig2str(), including the
   terminating NUL byte.  Shamelessly stolen from Gnulib.  */
# ifndef SIG2STR_MAX
/* The longest one: "RTMAX", then "+" or "-", then up to 10 digits, then NUL.
   Add + 2 as a reserve for the future.  */
#  define SIG2STR_MAX (5 + 1 + 10 + 1 + 2)
# endif

int sig2str (int, char *);
int str2sig (char const *, int *);

# include_next <signal.h>

#endif
