/* Replacement sys/stat.h file for building GNU Emacs on the Macintosh.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Contributed by Andrew Choi (akochoi@mac.com).  */

#ifndef _SYS_STAT_H
#define _SYS_STAT_H

#ifdef __MWERKS__
#include <stat.mac.h>

#if __MSL__ >= 0x6000
#define fstat _fstat
#endif

#undef S_IFMT
#undef S_IFBLK
#undef S_IFCHR
#undef S_IFIFO
#undef S_IFREG
#undef S_IFDIR
#undef S_IFLNK

#undef S_IRUSR
#undef S_IWUSR
#undef S_IXUSR

#endif  /* __MWERKS__ */

/* Need to redefine these for CW, filemode.c assumes Unix definitions which are
   inconsistent with CW definitions because CW uses bits 8-12 for S_IFMT info.
   Bit 8 is used by S_IRUSR on Unix! */
#define	S_IFMT	0170000		/* type of file */
#define S_IFBLK	0060000		/* block special */
#define S_IFCHR	0020000		/* character special */
#define S_IFIFO	0010000		/* FIFO special */
#define S_IFREG	0100000		/* regular */
#define S_IFDIR	0040000		/* directory */
#define S_IFLNK	0030000		/* symbolic link */

#define S_IREAD  00400
#define S_IWRITE 00200
#define S_IEXEC  00100

/* Need to redefine these for because mode_string in filemode.c assumes Unix
   values in the lower 9 bits which are different from CW values.  */
#define S_IRUSR S_IREAD
#define S_IWUSR S_IWRITE
#define S_IXUSR S_IEXEC

#ifdef __MRC__
typedef unsigned long dev_t;

struct stat {
  dev_t st_dev;			/* ID of device containing file */
  int st_ino;			/* file serial number */
  unsigned short st_mode;	/* mode of file */
  int st_nlink;			/* number of links to the file */
  int st_uid;			/* user ID of file */
  int st_gid;			/* group ID of file */
  int st_rdev;			/* device ID (if file is character or block special) */
  int st_size;			/* file size in bytes (if file is a regular file) */
  int st_atime;			/* time of last access */
  int st_mtime;			/* time of last data modification */
  int st_ctime;			/* time of last status change */
};
#endif  /* __MRC__ */

#endif /* _SYS_STAT_H */
