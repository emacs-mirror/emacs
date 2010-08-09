/* Machine description file for intel 386.

Copyright (C) 1987, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
  2009, 2010  Free Software Foundation, Inc.

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


#ifdef WINDOWSNT
#define VIRT_ADDR_VARIES
#define DATA_START 	get_data_start ()
#endif

#ifdef GNU_LINUX
/* libc-linux/sysdeps/linux/i386/ulimit.c says that due to shared library, */
/* we cannot get the maximum address for brk */
#define ULIMIT_BREAK_VALUE (32*1024*1024)
#endif

/* arch-tag: 746338f0-cb7b-4f49-a98c-cb50817cf2ec
   (do not change this comment) */
