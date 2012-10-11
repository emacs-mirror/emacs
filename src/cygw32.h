/* Header for Cygwin support routines.
   Copyright (C) 2011-2012  Free Software Foundation, Inc.

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

#ifndef CYGW32_H
#define CYGW32_H
#include <config.h>
#include <windef.h>
#include <sys/cygwin.h>
#include <wchar.h>

#include <signal.h>
#include <stdio.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#include <setjmp.h>

#include "lisp.h"
#include "coding.h"

/* *** Character conversion *** */

/* Access the wide-character string stored in a Lisp string object.  */
#define WCSDATA(x) ((wchar_t *) SDATA (x))

/* Convert the multi-byte string in STR to UTF-16LE encoded unibyte
   string, and store it in *BUF.  BUF may safely point to STR on entry.  */
extern wchar_t *to_unicode (Lisp_Object str, Lisp_Object *buf);

/* Convert STR, a UTF-16LE encoded string embedded in a unibyte string
   object, to a multi-byte Emacs string, and return it.  */
extern Lisp_Object from_unicode (Lisp_Object str);

/* *** Misc *** */
extern void syms_of_cygw32 (void);
extern char * w32_strerror (int error_no);

#endif /* CYGW32_H */
