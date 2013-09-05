/* Heap management routines for GNU Emacs on the Microsoft Windows API.
   Copyright (C) 1994, 2001-2013 Free Software Foundation, Inc.

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

/*
   Geoff Voelker (voelker@cs.washington.edu)			     7-29-94
*/

#include <config.h>
#include <stdio.h>

#include "w32common.h"
#include "w32heap.h"
#include "lisp.h"  /* for VALMASK */

#define RVA_TO_PTR(rva) ((unsigned char *)((DWORD_PTR)(rva) + (DWORD_PTR)GetModuleHandle (NULL)))

/* Emulate getpagesize.  */
int
getpagesize (void)
{
  return sysinfo_cache.dwPageSize;
}

/* Info for managing our preload heap, which is essentially a fixed size
   data area in the executable.  */
PIMAGE_SECTION_HEADER preload_heap_section;

/* Info for keeping track of our heap.  */
unsigned char *data_region_base = NULL;
unsigned char *data_region_end = NULL;
unsigned char *real_data_region_end = NULL;
size_t  reserved_heap_size = 0;

/* The start of the data segment.  */
unsigned char *
get_data_start (void)
{
  return data_region_base;
}

/* The end of the data segment.  */
unsigned char *
get_data_end (void)
{
  return data_region_end;
}

#if !USE_LSB_TAG
static char *
allocate_heap (void)
{
  /* Try to get as much as possible of the address range from the end of
     the preload heap section up to the usable address limit.  Since GNU
     malloc can handle gaps in the memory it gets from sbrk, we can
     simply set the sbrk pointer to the base of the new heap region.  */
  DWORD_PTR base =
    ROUND_UP ((RVA_TO_PTR (preload_heap_section->VirtualAddress)
	       + preload_heap_section->Misc.VirtualSize),
	      get_allocation_unit ());
  DWORD_PTR end  = ((unsigned __int64)1) << VALBITS; /* 256MB */
  void *ptr = NULL;

  while (!ptr && (base < end))
    {
#ifdef _WIN64
      reserved_heap_size = min(end - base, 0x4000000000i64); /* Limit to 256Gb */
#else
      reserved_heap_size = end - base;
#endif
      ptr = VirtualAlloc ((void *) base,
			  get_reserved_heap_size (),
			  MEM_RESERVE,
			  PAGE_NOACCESS);
      base += 0x00100000;  /* 1MB increment */
    }

  return ptr;
}
#else  /* USE_LSB_TAG */
static char *
allocate_heap (void)
{
#ifdef _WIN64
  size_t size = 0x4000000000i64; /* start by asking for 32GB */
#else
  /* We used to start with 2GB here, but on Windows 7 that would leave
     too little room in the address space for threads started by
     Windows on our behalf, e.g. when we pop up the file selection
     dialog.  */
  size_t size = 0x68000000; /* start by asking for 1.7GB */
#endif
  void *ptr = NULL;

  while (!ptr && size > 0x00100000)
    {
      reserved_heap_size = size;
      ptr = VirtualAlloc (NULL,
			  get_reserved_heap_size (),
			  MEM_RESERVE,
			  PAGE_NOACCESS);
      size -= 0x00800000; /* if failed, decrease request by 8MB */
    }

  return ptr;
}
#endif /* USE_LSB_TAG */


/* Emulate Unix sbrk.  Note that ralloc.c expects the return value to
   be the address of the _start_ (not end) of the new block in case of
   success, and zero (not -1) in case of failure.  */
void *
sbrk (ptrdiff_t increment)
{
  void *result;
  ptrdiff_t size = increment;

  result = data_region_end;

  /* If size is negative, shrink the heap by decommitting pages.  */
  if (size < 0)
    {
      ptrdiff_t new_size;
      unsigned char *new_data_region_end;

      size = -size;

      /* Sanity checks.  */
      if ((data_region_end - size) < data_region_base)
	return NULL;

      /* We can only decommit full pages, so allow for
	 partial deallocation [cga].  */
      new_data_region_end = (data_region_end - size);
      new_data_region_end = (unsigned char *)
	((DWORD_PTR) (new_data_region_end + syspage_mask) & ~syspage_mask);
      new_size = real_data_region_end - new_data_region_end;
      real_data_region_end = new_data_region_end;
      if (new_size > 0)
	{
	  /* Decommit size bytes from the end of the heap.  */
	  if (using_dynamic_heap
	      && !VirtualFree (real_data_region_end, new_size, MEM_DECOMMIT))
	    return NULL;
 	}

      data_region_end -= size;
    }
  /* If size is positive, grow the heap by committing reserved pages.  */
  else if (size > 0)
    {
      /* Sanity checks.  */
      if ((data_region_end + size) >
	  (data_region_base + get_reserved_heap_size ()))
	return NULL;

      /* Commit more of our heap. */
      if (using_dynamic_heap
	  && VirtualAlloc (data_region_end, size, MEM_COMMIT,
			   PAGE_READWRITE) == NULL)
	return NULL;
      data_region_end += size;

      /* We really only commit full pages, so record where
	 the real end of committed memory is [cga].  */
      real_data_region_end = (unsigned char *)
	  ((DWORD_PTR) (data_region_end + syspage_mask) & ~syspage_mask);
    }

  return result;
}

/* Initialize the internal heap variables used by sbrk.  When running in
   preload phase (ie. in the undumped executable), we rely entirely on a
   fixed size heap section included in the .exe itself; this is
   preserved during dumping, and truncated to the size actually used.

   When running in the dumped executable, we reserve as much as possible
   of the address range that is addressable by Lisp object pointers, to
   supplement what is left of the preload heap.  Although we cannot rely
   on the dynamically allocated arena being contiguous with the static
   heap area, it is not a problem because sbrk can pretend that the gap
   was allocated by something else; GNU malloc detects when there is a
   jump in the sbrk values, and starts a new heap block.  */
void
init_heap (void)
{
  PIMAGE_DOS_HEADER dos_header;
  PIMAGE_NT_HEADERS nt_header;

  dos_header = (PIMAGE_DOS_HEADER) RVA_TO_PTR (0);
  nt_header = (PIMAGE_NT_HEADERS) (((DWORD_PTR) dos_header) +
				   dos_header->e_lfanew);
  preload_heap_section = find_section ("EMHEAP", nt_header);

  if (using_dynamic_heap)
    {
      data_region_base = allocate_heap ();
      if (!data_region_base)
	{
	  printf ("Error: Could not reserve dynamic heap area.\n");
	  exit (1);
	}

#if !USE_LSB_TAG
      /* Ensure that the addresses don't use the upper tag bits since
	 the Lisp type goes there.  */
      if (((DWORD_PTR) data_region_base & ~VALMASK) != 0)
	{
	  printf ("Error: The heap was allocated in upper memory.\n");
	  exit (1);
	}
#endif
      data_region_end = data_region_base;
      real_data_region_end = data_region_end;
    }
  else
    {
      data_region_base = RVA_TO_PTR (preload_heap_section->VirtualAddress);
      data_region_end = data_region_base;
      real_data_region_end = data_region_end;
      reserved_heap_size = preload_heap_section->Misc.VirtualSize;
    }

  /* Update system version information to match current system.  */
  cache_system_info ();
}

/* Round the heap up to the given alignment.  */
void
round_heap (size_t align)
{
  DWORD_PTR needs_to_be;
  DWORD_PTR need_to_alloc;

  needs_to_be = (DWORD_PTR) ROUND_UP (get_heap_end (), align);
  need_to_alloc = needs_to_be - (DWORD_PTR) get_heap_end ();

  if (need_to_alloc)
    sbrk (need_to_alloc);
}
