/* Heap management routines for GNU Emacs on Windows NT.
   Copyright (C) 1994 Free Software Foundation, Inc.

   This file is part of GNU Emacs.

   GNU Emacs is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any later
   version.

   GNU Emacs is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along
   with GNU Emacs; see the file COPYING.  If not, write to the Free Software
   Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Geoff Voelker (voelker@cs.washington.edu)			     7-29-94
*/

#include <stdlib.h>
#include <stdio.h>

#include "ntheap.h"

/* This gives us the page size and the size of the allocation unit on NT.  */
SYSTEM_INFO sysinfo_cache;

/* These are defined to get Emacs to compile, but are not used.  */
int edata;
int etext;

/* The major and minor versions of NT.  */
int nt_major_version;
int nt_minor_version;

/* Cache information describing the NT system for later use.  */
void
cache_system_info (void)
{
  union 
    {
      struct info 
	{
	  char  major;
	  char  minor;
	  short platform;
	} info;
      DWORD data;
    } version;

  /* Cache the version of the operating system.  */
  version.data = GetVersion ();
  nt_major_version = version.info.major;
  nt_minor_version = version.info.minor;

  /* Cache page size, allocation unit, processor type, etc.  */
  GetSystemInfo (&sysinfo_cache);
}

/* Round ADDRESS up to be aligned with ALIGN.  */
unsigned char *
round_to_next (unsigned char *address, unsigned long align)
{
  unsigned long tmp;

  tmp = (unsigned long) address;
  tmp = (tmp + align - 1) / align;

  return (unsigned char *) (tmp * align);
}

/* Info for keeping track of our heap.  */
unsigned char *data_region_base = NULL;
unsigned char *data_region_end = NULL;
unsigned long  data_region_size = 0;

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

/* Emulate Unix sbrk.  */
void *
sbrk (unsigned long increment)
{
  void *result;
  long size = (long) increment;
  
  /* Allocate our heap if we haven't done so already.  */
  if (!data_region_base) 
    {
      data_region_base = VirtualAlloc ((void *) get_data_region_base (),
				       get_reserved_heap_size (),
				       MEM_RESERVE,
				       PAGE_NOACCESS);
      if (!data_region_base)
	return NULL;

      /* Ensure that the addresses don't use the upper 8 bits since
	 the Lisp type goes there (yucko).  */
      if (((unsigned long) data_region_base & 0xFF000000) != 0) 
	{
	  printf ("Error: The heap was allocated in upper memory.\n");
	  exit (1);
	}

      data_region_end = data_region_base;
      data_region_size = get_reserved_heap_size ();
    }
  
  result = data_region_end;
  
  /* If size is negative, shrink the heap by decommitting pages.  */
  if (size < 0) 
    {
      size = -size;

      /* Sanity checks.  */
      if ((data_region_end - size) < data_region_base)
	return NULL;

      /* Decommit size bytes from the end of the heap.  */
      if (!VirtualFree (data_region_end - size, size, MEM_DECOMMIT))
	return NULL;

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
      if (VirtualAlloc (data_region_end, size, MEM_COMMIT,
			PAGE_READWRITE) == NULL)
	return NULL;
      data_region_end += size;
    }
  
  return result;
}

/* Recreate the heap from the data that was dumped to the executable.
   EXECUTABLE_PATH tells us where to find the executable.  */
void
recreate_heap (char *executable_path)
{
  unsigned char *tmp;

  /* First reserve the upper part of our heap.  (We reserve first
     because there have been problems in the past where doing the
     mapping first has loaded DLLs into the VA space of our heap.)  */
  tmp = VirtualAlloc ((void *) get_heap_end (),
		      get_reserved_heap_size () - get_committed_heap_size (),
		      MEM_RESERVE,
		      PAGE_NOACCESS);
  if (!tmp)
    exit (1);

  /* We read in the data for the .bss section from the executable
     first and map in the heap from the executable second to prevent
     any funny interactions between file I/O and file mapping.  */
  read_in_bss (executable_path);
  map_in_heap (executable_path);
}

/* Round the heap up to the given alignment.  */
void
round_heap (unsigned long align)
{
  unsigned long needs_to_be;
  unsigned long need_to_alloc;
  
  needs_to_be = (unsigned long) round_to_next (get_heap_end (), align);
  need_to_alloc = needs_to_be - (unsigned long) get_heap_end ();
  
  if (need_to_alloc) 
    sbrk (need_to_alloc);
}
