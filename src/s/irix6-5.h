/* Definitions file for GNU Emacs running on Silicon Graphics Irix system 6.5.

Copyright (C) 1999-2012  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */


#define IRIX6_5			/* used in m/iris4d */
#include "usg5-4-common.h"

#undef _longjmp /* use system versions, not conservative aliases */
#undef _setjmp

#undef SETUP_SLAVE_PTY

/* No need to use sprintf to get the tty name--we get that from _getpty.  */
#define PTY_TTY_NAME_SPRINTF
/* No need to get the pty name at all.  */
#undef PTY_NAME_SPRINTF
#define PTY_NAME_SPRINTF
#ifdef emacs
char *_getpty();
#endif
/* Here is how to do it.  */
#define PTY_OPEN					    \
{							    \
  struct sigaction ocstat, cstat;			    \
  struct stat stb;					    \
  char * name;						    \
  sigemptyset(&cstat.sa_mask);				    \
  cstat.sa_handler = SIG_DFL;				    \
  cstat.sa_flags = 0;					    \
  sigaction(SIGCLD, &cstat, &ocstat);			    \
  name = _getpty (&fd, O_RDWR | O_NDELAY, 0600, 0);	    \
  sigaction(SIGCLD, &ocstat, (struct sigaction *)0);	    \
  if (name == 0)					    \
    return -1;						    \
  if (fd < 0)						    \
    return -1;						    \
  if (fstat (fd, &stb) < 0)				    \
    return -1;						    \
  strcpy (pty_name, name);				    \
}

/* Ulimit(UL_GMEMLIM) is busted...  */
#define ULIMIT_BREAK_VALUE 0x14000000

#undef SA_RESTART     /* not the same as defining BROKEN_SA_RESTART */

#undef TIOCSIGSEND		/* defined in usg5-4-common.h */

/* Tested on Irix 6.5.  SCM worked on earlier versions.  */
#define GC_SETJMP_WORKS 1
