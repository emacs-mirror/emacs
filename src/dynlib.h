/* Portable API for dynamic loading.

Copyright 2015-2020 Free Software Foundation, Inc.

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

#ifndef DYNLIB_H
#define DYNLIB_H

#include <stdbool.h>

typedef void *dynlib_handle_ptr;
dynlib_handle_ptr dynlib_open (const char *path);
int dynlib_close (dynlib_handle_ptr h);
const char *dynlib_error (void);

ATTRIBUTE_MAY_ALIAS void *dynlib_sym (dynlib_handle_ptr h, const char *sym);

typedef void (ATTRIBUTE_MAY_ALIAS *dynlib_function_ptr) (void);
dynlib_function_ptr dynlib_func (dynlib_handle_ptr h, const char *sym);

/* Sets *FILE to the file name from which PTR was loaded, and *SYM to
   its symbol name.  If the file or symbol name could not be
   determined, set the corresponding argument to NULL.  */
void dynlib_addr (void (*ptr) (void), const char **file, const char **sym);

#endif /* DYNLIB_H */
