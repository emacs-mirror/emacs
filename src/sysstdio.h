/* Standard I/O for Emacs.

Copyright 2013-2017 Free Software Foundation, Inc.

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

#ifndef EMACS_SYSSTDIO_H
#define EMACS_SYSSTDIO_H

#include <fcntl.h>
#include <stdio.h>

extern FILE *emacs_fopen (char const *, char const *);

#if O_BINARY
# define FOPEN_BINARY "b"
# define FOPEN_TEXT "t"
#else
# define FOPEN_BINARY ""
# define FOPEN_TEXT ""
#endif

/* These are compatible with unlocked-io.h, if both files are included.  */
#if !HAVE_DECL_CLEARERR_UNLOCKED
# define clearerr_unlocked(x) clearerr (x)
#endif
#if !HAVE_DECL_FEOF_UNLOCKED
# define feof_unlocked(x) feof (x)
#endif
#if !HAVE_DECL_FERROR_UNLOCKED
# define ferror_unlocked(x) ferror (x)
#endif
#if !HAVE_DECL_FFLUSH_UNLOCKED
# define fflush_unlocked(x) fflush (x)
#endif
#if !HAVE_DECL_FGETS_UNLOCKED
# define fgets_unlocked(x,y,z) fgets (x,y,z)
#endif
#if !HAVE_DECL_FPUTC_UNLOCKED
# define fputc_unlocked(x,y) fputc (x,y)
#endif
#if !HAVE_DECL_FPUTS_UNLOCKED
# define fputs_unlocked(x,y) fputs (x,y)
#endif
#if !HAVE_DECL_FREAD_UNLOCKED
# define fread_unlocked(w,x,y,z) fread (w,x,y,z)
#endif
#if !HAVE_DECL_FWRITE_UNLOCKED
# define fwrite_unlocked(w,x,y,z) fwrite (w,x,y,z)
#endif
#if !HAVE_DECL_GETC_UNLOCKED
# define getc_unlocked(x) getc (x)
#endif
#if !HAVE_DECL_GETCHAR_UNLOCKED
# define getchar_unlocked() getchar ()
#endif
#if !HAVE_DECL_PUTC_UNLOCKED
# define putc_unlocked(x,y) putc (x,y)
#endif
#if !HAVE_DECL_PUTCHAR_UNLOCKED
# define putchar_unlocked(x) putchar (x)
#endif

#endif /* EMACS_SYSSTDIO_H */
