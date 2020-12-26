/* read-file.h -- read file contents into a string
   Copyright (C) 2006, 2009-2020 Free Software Foundation, Inc.
   Written by Simon Josefsson.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <https://www.gnu.org/licenses/>.  */

#ifndef READ_FILE_H
#define READ_FILE_H

/* Get size_t.  */
#include <stddef.h>

/* Get FILE.  */
#include <stdio.h>

/* Indicate that the file is treated as binary.  */
#define RF_BINARY 0x1

/* Indicate that the file content contains sensitive information.  */
#define RF_SENSITIVE 0x2

extern char *fread_file (FILE * stream, int flags, size_t * length);

extern char *read_file (const char *filename, int flags, size_t * length);

#endif /* READ_FILE_H */
