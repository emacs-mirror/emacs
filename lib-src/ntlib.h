/* Utility and Unix shadow routines for GNU Emacs support programs on NT.
   Copyright (C) 1994 Free Software Foundation, Inc.

   This file is part of GNU Emacs.

   GNU Emacs is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any later
   version.

   GNU Emacs is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License
   along with GNU Emacs; see the file COPYING.  If not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

*/

#include <pwd.h>
#include <malloc.h>

void sleep(int seconds);
char *getwd (char *dir);
int getppid(void);
char * getlogin ();
char * cuserid (char * s);
int getuid ();
int setuid (int uid);
struct passwd * getpwuid (int uid);
char * getpass (const char * prompt);
int fchown (int fd, int uid, int gid);

#ifndef BSTRING
#define bzero(b, l) memset(b, 0, l)
#define bcopy(s, d, l) memcpy(d, s, l)
#define bcmp(a, b, l) memcmp(a, b, l)

#define index   strchr
#define rindex  strrchr
#endif

/* end of ntlib.h */
