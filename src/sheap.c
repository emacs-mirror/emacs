/* simulate `sbrk' with an array in .bss, for `unexec' support for Cygwin;
   complete rewrite of xemacs Cygwin `unexec' code

   Copyright (C) 2004-2018 Free Software Foundation, Inc.

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

#include <config.h>

#include "sheap.h"

#include <stdio.h>
#include "lisp.h"
#include <unistd.h>
#include <stdlib.h>		/* for exit */

static int debug_sheap;

char bss_sbrk_buffer[STATIC_HEAP_SIZE];
char *max_bss_sbrk_ptr;
bool bss_sbrk_did_unexec;

void *
bss_sbrk (ptrdiff_t request_size)
{
  static char *bss_sbrk_ptr;

  if (!bss_sbrk_ptr)
    {
      max_bss_sbrk_ptr = bss_sbrk_ptr = bss_sbrk_buffer;
#ifdef CYGWIN
      /* Force space for fork to work.  */
      sbrk (4096);
#endif
    }

  int used = bss_sbrk_ptr - bss_sbrk_buffer;

  if (request_size < -used)
    {
      printf (("attempt to free too much: "
	       "avail %d used %d failed request %"pD"d\n"),
	      STATIC_HEAP_SIZE, used, request_size);
      exit (-1);
      return 0;
    }
  else if (STATIC_HEAP_SIZE - used < request_size)
    {
      printf ("static heap exhausted: avail %d used %d failed request %"pD"d\n",
	      STATIC_HEAP_SIZE, used, request_size);
      exit (-1);
      return 0;
    }

  void *ret = bss_sbrk_ptr;
  bss_sbrk_ptr += request_size;
  if (max_bss_sbrk_ptr < bss_sbrk_ptr)
    max_bss_sbrk_ptr = bss_sbrk_ptr;
  if (debug_sheap)
    {
      if (request_size < 0)
	printf ("freed size %"pD"d\n", request_size);
      else
	printf ("allocated %p size %"pD"d\n", ret, request_size);
    }
  return ret;
}
