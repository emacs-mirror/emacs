/* Interface definitions for termcap entries.

Copyright (C) 2011-2018 Free Software Foundation, Inc.

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

#ifndef EMACS_TPARAM_H
#define EMACS_TPARAM_H

/* Don't try to include termcap.h.  On some systems, configure finds a
   non-standard termcap.h that the main build won't find.  */

void tputs (const char *, int, int (*) (int));
int tgetent (char *, const char *);
int tgetflag (const char *);
int tgetnum (const char *);
char *tgetstr (const char *, char **);
char *tgoto (const char *, int, int);

char *tparam (const char *, char *, int, int, int, int, int) ATTRIBUTE_MALLOC;

extern char PC;
extern char *BC;
extern char *UP;

#ifdef TERMINFO
int tigetflag (const char *);
char *tigetstr (const char *);
#endif

#endif /* EMACS_TPARAM_H */
