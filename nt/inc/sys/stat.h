/* sys/stat.h supplied with MSVCRT uses too narrow data types for
   inode and user/group id, so we replace them with our own.

   Copyright (C) 2008  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#ifndef INC_SYS_STAT_H_
#define INC_SYS_STAT_H_

#ifdef __MINGW32__
# include <_mingw.h>
#endif

/* Only MinGW 3.13 and later has __MINGW_NOTHROW.  */
#ifndef __MINGW_NOTHROW
# define __MINGW_NOTHROW
#endif

#include <sys/types.h>
#include <time.h>

#define	S_IFMT	0xF000

#define	S_IFREG	0x8000
#define	S_IFDIR	0x4000
#define	S_IFBLK	0x3000
#define	S_IFCHR	0x2000
#define	S_IFIFO	0x1000

#define	S_IREAD	 0x0100
#define	S_IWRITE 0x0080
#define	S_IEXEC	 0x0040

#define	S_IRUSR	S_IREAD
#define	S_IWUSR	S_IWRITE
#define	S_IXUSR	S_IEXEC
#define	S_IRWXU	(S_IREAD | S_IWRITE | S_IEXEC)

#define	S_ISREG(m)	(((m) & S_IFMT) == S_IFREG)
#define	S_ISDIR(m)	(((m) & S_IFMT) == S_IFDIR)
#define	S_ISBLK(m)	(((m) & S_IFMT) == S_IFBLK)
#define	S_ISCHR(m)	(((m) & S_IFMT) == S_IFCHR)
#define	S_ISFIFO(m)	(((m) & S_IFMT) == S_IFIFO)

struct stat {
  dev_t st_dev;
  unsigned __int64 st_ino;	/* ino_t in sys/types.h is too narrow */
  unsigned short   st_mode;
  short		   st_nlink;
  int		   st_uid;
  int		   st_gid;
  dev_t		   st_rdev;
  off_t		   st_size;
  time_t	   st_atime;
  time_t	   st_mtime;
  time_t	   st_ctime;
};

_CRTIMP int __cdecl __MINGW_NOTHROW	fstat (int, struct stat*);
_CRTIMP int __cdecl __MINGW_NOTHROW	chmod (const char*, int);
_CRTIMP int __cdecl __MINGW_NOTHROW	stat (const char*, struct stat*);

#endif	/* INC_SYS_STAT_H_ */

/* arch-tag: 17d8fc06-f2e5-4d10-a01e-af819918fe42
   (do not change this comment) */
