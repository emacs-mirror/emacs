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
   Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.

*/

#include <pwd.h>
#include <malloc.h>

/* Include these headers now so we don't have to worry about include
   order dependencies in common source files.  */
#include <direct.h>
#include <io.h>
#include <stdio.h>

#ifdef sleep
#undef sleep
#endif
void sleep(unsigned long seconds);
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
#endif

/* redirect or undo interceptions created by config.h */
#undef access
#define access  _access
#undef chdir
#define chdir   _chdir
#undef chmod
#define chmod   _chmod
#undef close
#define close   _close
#undef creat
#define creat   _creat
#undef ctime
#undef dup
#define dup     _dup
#undef dup2
#define dup2    _dup2
#undef fopen
#undef mkdir
#define mkdir   _mkdir
#undef mktemp
#define mktemp  _mktemp
#undef open
#define open    _open
#undef pipe
#define pipe    _pipe
#undef read
#define read    _read
#undef rename
#undef rmdir
#define rmdir   _rmdir
#undef unlink
#define unlink  _unlink
#undef write
#define write   _write

/* map to MSVC names */
#define execlp    _execlp
#define execvp    _execvp
#define fcloseall _fcloseall
#define fdopen	  _fdopen
#define fgetchar  _fgetchar
#ifndef fileno
#define fileno	  _fileno
#endif
#define flushall  _flushall
#define fputchar  _fputchar
#define getcwd	  _getcwd
#define getw	  _getw
#define getpid    _getpid
#define isatty    _isatty
#define locking   _locking
#define logb      _logb
#define _longjmp  longjmp
#define lseek     _lseek
#define popen     _popen
#define pclose    _pclose
#define putw	  _putw
#define umask	  _umask
#define utime	  _utime
#define index     strchr
#define rindex    strrchr

/* Make standard winsock definitions available if needed.  */
#undef _WINSOCKAPI_
#undef _WINSOCK_H

/* end of ntlib.h */

/* arch-tag: 93444f66-7b98-4aa5-a5cd-01444094af28
   (do not change this comment) */
