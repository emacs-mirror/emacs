/* Memory allocators such as malloc+free.

   Copyright (C) 2011 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* Written by Paul Eggert.  */

#ifndef _GL_ALLOCATOR_H

#include <stddef.h>

struct allocator
{
  /* Call MALLOC to allocate memory, like 'malloc'.  On failure MALLOC
     should return NULL, though not necessarily set errno.  When given
     a zero size it may return NULL even if successful.  */
  void *(*malloc) (size_t);

  /* If nonnull, call REALLOC to reallocate memory, like 'realloc'.
     On failure REALLOC should return NULL, though not necessarily set
     errno.  When given a zero size it may return NULL even if
     successful.  */
  void *(*realloc) (void *, size_t);

  /* Call FREE to free memory, like 'free'.  */
  void (*free) (void *);

  /* If nonnull, call DIE if MALLOC or REALLOC fails.  DIE should
     not return.  */
  void (*die) (void);
};

#endif
