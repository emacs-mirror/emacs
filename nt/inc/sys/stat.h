/* sys/stat.h supplied with MSVCRT uses too narrow data types for
   inode and user/group id, so we replace them with our own.

Copyright (C) 2008-2018 Free Software Foundation, Inc.

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

#ifndef INC_SYS_STAT_H_
#define INC_SYS_STAT_H_

#ifdef __MINGW32__
# include <_mingw.h>
#endif

/* Only MinGW 3.13 and later has __MINGW_NOTHROW.  */
#ifndef __MINGW_NOTHROW
# define __MINGW_NOTHROW
#endif

/* Prevent the MinGW stat.h header from being included, ever.  */
#ifndef _SYS_STAT_H
# define _SYS_STAT_H
#endif
#ifndef _INC_STAT_H
# define _INC_STAT_H
#endif

#include <sys/types.h>
#include <time.h>

#define	S_IFMT	0xF800

#define	S_IFREG	0x8000
#define	S_IFDIR	0x4000
#define	S_IFBLK	0x3000
#define	S_IFCHR	0x2000
#define	S_IFIFO	0x1000
#define	S_IFLNK 0x0800

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
#define	S_ISLNK(m)	(((m) & S_IFMT) == S_IFLNK)

/* These don't exist on Windows, but lib/filemode.c wants them.  */
#define S_ISUID 0
#define S_ISGID 0
#define S_ISVTX 0
#define S_IRGRP (S_IRUSR >> 3)
#define S_IROTH (S_IRUSR >> 6)
#define S_IWGRP (S_IWUSR >> 3)
#define S_IWOTH (S_IWUSR >> 6)
#define S_IXGRP (S_IXUSR >> 3)
#define S_IXOTH (S_IXUSR >> 6)

#define S_ISSOCK(m)    0
#define S_ISCTG(p)     0
#define S_ISDOOR(m)    0
#define S_ISMPB(m)     0
#define S_ISMPC(m)     0
#define S_ISMPX(m)     0
#define S_ISNWK(m)     0
#define S_ISPORT(m)    0
#define S_ISWHT(m)     0
#define S_TYPEISMQ(p)  0
#define S_TYPEISSEM(p) 0
#define S_TYPEISSHM(p) 0
#define S_TYPEISTMO(p) 0

#define UTIME_NOW  (-1)
#define UTIME_OMIT (-2)

struct stat {
  unsigned __int64 st_ino;	/* ino_t in sys/types.h is too narrow */
  dev_t st_dev;
  unsigned short   st_mode;
  short		   st_nlink;
  unsigned	   st_uid; /* Vista's TrustedInstaller has a very large RID */
  unsigned	   st_gid;
  unsigned __int64 st_size;
  dev_t		   st_rdev;
  time_t	   st_atime;
  time_t	   st_mtime;
  time_t	   st_ctime;
  char		   st_uname[260];
  char		   st_gname[260];
};

/* These are here to avoid compiler warnings when using wchar.h.  */
struct _stat
{
	_dev_t	st_dev;		/* Equivalent to drive number 0=A 1=B ... */
	_ino_t	st_ino;		/* Always zero ? */
	_mode_t	st_mode;	/* See above constants */
	short	st_nlink;	/* Number of links. */
	short	st_uid;		/* User: Maybe significant on NT ? */
	short	st_gid;		/* Group: Ditto */
	_dev_t	st_rdev;	/* Seems useless (not even filled in) */
	_off_t	st_size;	/* File size in bytes */
	time_t	st_atime;	/* Accessed date (always 00:00 hrs local
				 * on FAT) */
	time_t	st_mtime;	/* Modified time */
	time_t	st_ctime;	/* Creation time */
};

#if defined (__MSVCRT__)
struct _stati64 {
    _dev_t st_dev;
    _ino_t st_ino;
    _mode_t st_mode;
    short st_nlink;
    short st_uid;
    short st_gid;
    _dev_t st_rdev;
    __int64 st_size;
    time_t st_atime;
    time_t st_mtime;
    time_t st_ctime;
};
#endif

/* Internal variable for asking 'stat'/'lstat' to produce accurate
   info about owner and group of files. */
extern int w32_stat_get_owner_group;

/* Prevent redefinition by other headers, e.g. wchar.h.  */
#define _STAT_DEFINED
/* This prevents definition in MinGW's wchar.h of inline functions
   that use struct _stat64i32 etc., which we don't define and don't
   support in our implementation of 'stat' and 'fstat'.  If we don't
   prevent definition of those inline functions, any program (e.g.,
   test programs run by configure) that includes both wchar.h and
   sys/stat.h will fail to compile.  */
#define _WSTAT_DEFINED

int __cdecl __MINGW_NOTHROW	fstat (int, struct stat*);
int __cdecl __MINGW_NOTHROW	stat (const char*, struct stat*);
int __cdecl __MINGW_NOTHROW	lstat (const char*, struct stat*);
int __cdecl __MINGW_NOTHROW	fstatat (int, char const *,
						 struct stat *, int);
int __cdecl __MINGW_NOTHROW	chmod (const char*, int);

#endif	/* INC_SYS_STAT_H_ */
