/* Header for Cygwin support routines.
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

extern void syms_of_cygw32 (void);
extern char * w32_strerror (int error_no);

#endif /* CYGW32_H */
