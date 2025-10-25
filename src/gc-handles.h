/* API for GC handles
   Copyright (C) 2025 Free Software Foundation, Inc.

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
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>. */

#ifndef EMACS_GC_HANDLES_H
#define EMACS_GC_HANDLES_H

#include "config.h"
#include "lisp.h"

struct gc_handle_struct;
typedef struct gc_handle_struct *gc_handle;

gc_handle gc_handle_for (Lisp_Object);
gc_handle gc_handle_for_pvec (struct vectorlike_header *);
void free_gc_handle (gc_handle);
Lisp_Object gc_handle_value (gc_handle);

void syms_of_gc_handles (void);

#endif
