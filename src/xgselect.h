/* Header for xg_select.

Copyright (C) 2009-2020 Free Software Foundation, Inc.

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

#ifndef XGSELECT_H
#define XGSELECT_H

#include "lisp.h"
#include "sysselect.h"

struct timespec;

extern int xg_select (int max_fds,
		      fd_set *rfds, fd_set *wfds, fd_set *efds,
		      struct timespec *timeout, sigset_t *sigmask);

#endif /* XGSELECT_H */
