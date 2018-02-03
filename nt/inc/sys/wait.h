/* A limited emulation of sys/wait.h on Posix systems.

Copyright (C) 2012-2018 Free Software Foundation, Inc.

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

#ifndef INC_SYS_WAIT_H_
#define INC_SYS_WAIT_H_

#define WNOHANG    1
#define WUNTRACED  2
#define WSTOPPED   2	/* same as WUNTRACED */
#define WEXITED    4
#define WCONTINUED 8

/* The various WIF* macros are defined in src/syswait.h.  */

extern pid_t waitpid (pid_t, int *, int);

#endif	/* INC_SYS_WAIT_H_ */
