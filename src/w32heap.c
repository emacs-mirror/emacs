/* Heap management routines for GNU Emacs on the Microsoft Windows API.
   Copyright (C) 1994, 2001-2026 Free Software Foundation, Inc.

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
   along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>. */

/*
  Geoff Voelker (voelker@cs.washington.edu)                          7-29-94
*/

/*
  Heavily modified by Fabrice Popineau (fabrice.popineau@gmail.com) 28-02-2014
*/

/*
  Memory allocation scheme for w32/w64:

  - Buffers are mmap'ed using a very simple emulation of mmap/munmap
  - During the emacs phase, or always if pdumper is used:
    * we create a private heap for new memory blocks
    * we make sure that we never free a block that has been dumped.
      Freeing a dumped block could work in principle, but may prove
      unreliable if we distribute binaries of emacs.exe: MS does not
      guarantee that the heap data structures are the same across all
      versions of their OS, even though the API is available since XP.  */

#include <config.h>
#include <stdio.h>
#include <errno.h>

#include <sys/mman.h>
#include <sys/resource.h>
#include "w32common.h"
#include "w32heap.h"
#include "lisp.h"
#include "w32.h"	/* for FD_SETSIZE */

/* We chose to leave those declarations here.  They are used only in
   this file.  The RtlCreateHeap is available since XP.  It is located
   in ntdll.dll and is available with the DDK.  People often
   complained that HeapCreate doesn't offer the ability to create a
   heap at a given place, which we need here, and which RtlCreateHeap
   provides.  We reproduce here the definitions available with the
   DDK.  */

typedef PVOID (WINAPI * RtlCreateHeap_Proc) (
                                             /* _In_ */      ULONG Flags,
                                             /* _In_opt_ */  PVOID HeapBase,
                                             /* _In_opt_ */  SIZE_T ReserveSize,
                                             /* _In_opt_ */  SIZE_T CommitSize,
                                             /* _In_opt_ */  PVOID Lock,
                                             /* _In_opt_ */  PVOID Parameters
                                             );

typedef LONG NTSTATUS;

typedef NTSTATUS (NTAPI *PRTL_HEAP_COMMIT_ROUTINE) (
						    IN PVOID Base,
						    IN OUT PVOID *CommitAddress,
						    IN OUT PSIZE_T CommitSize
						    );

typedef struct _RTL_HEAP_PARAMETERS {
  ULONG Length;
  SIZE_T SegmentReserve;
  SIZE_T SegmentCommit;
  SIZE_T DeCommitFreeBlockThreshold;
  SIZE_T DeCommitTotalFreeThreshold;
  SIZE_T MaximumAllocationSize;
  SIZE_T VirtualMemoryThreshold;
  SIZE_T InitialCommit;
  SIZE_T InitialReserve;
  PRTL_HEAP_COMMIT_ROUTINE CommitRoutine;
  SIZE_T Reserved[ 2 ];
} RTL_HEAP_PARAMETERS, *PRTL_HEAP_PARAMETERS;

/* Info for keeping track of our dynamic heap used after dumping. */
static unsigned char *data_region_base = NULL;
static unsigned char *data_region_end = NULL;

/* Handle for the private heap:
    - inside the dumped_data[] array before dump with unexec,
    - outside of it after dump, or always if pdumper is used.
*/
static HANDLE heap = NULL;

/* We redirect the standard allocation functions.  */
malloc_fn the_malloc_fn;
realloc_fn the_realloc_fn;
free_fn the_free_fn;

static void *
heap_alloc (size_t size)
{
  void *p = size <= PTRDIFF_MAX ? HeapAlloc (heap, 0, size | !size) : NULL;
  if (!p)
    errno = ENOMEM;
  return p;
}

static void *
heap_realloc (void *ptr, size_t size)
{
  void *p = (size <= PTRDIFF_MAX
	     ? HeapReAlloc (heap, 0, ptr, size | !size)
	     : NULL);
  if (!p)
    errno = ENOMEM;
  return p;
}

/* It doesn't seem to be useful to allocate from a file mapping.
   It would be if the memory was shared.
     https://stackoverflow.com/questions/307060/what-is-the-purpose-of-allocating-pages-in-the-pagefile-with-createfilemapping  */

/* Heap creation.  */

/* We want to turn on Low Fragmentation Heap for XP and older systems.
   MinGW32 lacks those definitions.  */
#ifndef MINGW_W64
typedef enum _HEAP_INFORMATION_CLASS {
  HeapCompatibilityInformation
} HEAP_INFORMATION_CLASS;

typedef WINBASEAPI BOOL (WINAPI * HeapSetInformation_Proc)(HANDLE,HEAP_INFORMATION_CLASS,PVOID,SIZE_T);
#endif

void
init_heap (void)
{
  /* After dumping, use a new private heap.  We explicitly enable
     the low fragmentation heap (LFH) here, for the sake of pre
     Vista versions.  Note: this will harmlessly fail on Vista and
     later, where the low-fragmentation heap is enabled by
     default.  It will also fail on pre-Vista versions when Emacs
     is run under a debugger; set _NO_DEBUG_HEAP=1 in the
     environment before starting GDB to get low fragmentation heap
     on XP and older systems, for the price of losing "certain
     heap debug options"; for the details see
     https://msdn.microsoft.com/en-us/library/windows/desktop/aa366705%28v=vs.85%29.aspx.  */
  data_region_end = data_region_base;

  /* Create the private heap.  */
  heap = HeapCreate (0, 0, 0);

#ifndef MINGW_W64
  unsigned long enable_lfh = 2;
  /* Set the low-fragmentation heap for OS before Vista.  */
  HMODULE hm_kernel32dll = LoadLibrary ("kernel32.dll");
  HeapSetInformation_Proc s_pfn_Heap_Set_Information =
    (HeapSetInformation_Proc) get_proc_addr (hm_kernel32dll,
					     "HeapSetInformation");
  if (s_pfn_Heap_Set_Information != NULL)
    {
      if (s_pfn_Heap_Set_Information ((PVOID) heap,
				      HeapCompatibilityInformation,
				      &enable_lfh, sizeof(enable_lfh)) == 0)
	DebPrint (("Enabling Low Fragmentation Heap failed: error %ld\n",
		   GetLastError ()));
    }
#endif

  if (os_subtype == OS_SUBTYPE_9X)
    {
      the_malloc_fn = malloc_after_dump_9x;
      the_realloc_fn = realloc_after_dump_9x;
      the_free_fn = free_after_dump_9x;
    }
  else
    {
      the_malloc_fn = malloc_after_dump;
      the_realloc_fn = realloc_after_dump;
      the_free_fn = free_after_dump;
    }

  /* Update system version information to match current system.  */
  cache_system_info ();
}


/* malloc, realloc, free.  */

#undef malloc
#undef realloc
#undef free

/* FREEABLE_P checks if the block can be safely freed.  */
#define FREEABLE_P(addr)						\
  ((DWORD_PTR)(unsigned char *)(addr) > 0)

void *
malloc_after_dump (size_t size)
{
  /* Use the new private heap.  */
  void *p = heap_alloc (size);

  /* After dump, keep track of the "brk value" for sbrk(0).  */
  if (p)
    {
      unsigned char *new_brk = (unsigned char *)p + size;

      if (new_brk > data_region_end)
	data_region_end = new_brk;
    }
  return p;
}

/* Re-allocate the previously allocated block in ptr, making the new
   block SIZE bytes long.  */
void *
realloc_after_dump (void *ptr, size_t size)
{
  void *p;

  /* After dumping.  */
  if (FREEABLE_P (ptr))
    {
      /* Reallocate the block since it lies in the new heap.  */
      p = heap_realloc (ptr, size);
    }
  else
    {
      /* If the block lies in the dumped data, do not free it.  Only
         allocate a new one.  */
      p = heap_alloc (size);
      if (p && ptr)
	CopyMemory (p, ptr, size);
    }
  /* After dump, keep track of the "brk value" for sbrk(0).  */
  if (p)
    {
      unsigned char *new_brk = (unsigned char *)p + size;

      if (new_brk > data_region_end)
	data_region_end = new_brk;
    }
  return p;
}

/* Free a block allocated by `malloc', `realloc' or `calloc'.  */
void
free_after_dump (void *ptr)
{
  /* After dumping.  */
  if (FREEABLE_P (ptr))
    {
      /* Free the block if it is in the new private heap.  */
      HeapFree (heap, 0, ptr);
    }
}

/* On Windows 9X, HeapAlloc may return pointers that are not aligned
   on 8-byte boundary, alignment which is required by the Lisp memory
   management.  To circumvent this problem, manually enforce alignment
   on Windows 9X.  */

void *
malloc_after_dump_9x (size_t size)
{
  void *p = malloc_after_dump (size + 8);
  void *pa;
  if (p == NULL)
    return p;
  pa = (void*)(((intptr_t)p + 8) & ~7);
  *((void**)pa-1) = p;
  return pa;
}

void *
realloc_after_dump_9x (void *ptr, size_t size)
{
  if (FREEABLE_P (ptr))
    {
      void *po = *((void**)ptr-1);
      void *p;
      void *pa;
      p = realloc_after_dump (po, size + 8);
      if (p == NULL)
        return p;
      pa = (void*)(((intptr_t)p + 8) & ~7);
      if (ptr != NULL &&
          (char*)pa - (char*)p != (char*)ptr - (char*)po)
        {
          /* Handle the case where alignment in pre-realloc and
             post-realloc blocks does not match.  */
          MoveMemory (pa, (void*)((char*)p + ((char*)ptr - (char*)po)), size);
        }
      *((void**)pa-1) = p;
      return pa;
    }
  else
    {
      /* Non-freeable pointers have no alignment-enforcing header
         (since dumping is not allowed on Windows 9X).  */
      void* p = malloc_after_dump_9x (size);
      if (p != NULL)
	CopyMemory (p, ptr, size);
      return p;
    }
}

void
free_after_dump_9x (void *ptr)
{
  if (FREEABLE_P (ptr))
    {
      free_after_dump (*((void**)ptr-1));
    }
}

void *
sys_calloc (size_t number, size_t size)
{
  size_t nbytes = number * size;
  void *ptr = (*the_malloc_fn) (nbytes);
  if (ptr)
    memset (ptr, 0, nbytes);
  return ptr;
}

/* Emulate getpagesize. */
int
getpagesize (void)
{
  return sysinfo_cache.dwPageSize;
}

void *
sbrk (ptrdiff_t increment)
{
  /* data_region_end is the address beyond the last allocated byte.
     The sbrk() function is not emulated at all, except for a 0 value
     of its parameter.  This is needed by the Emacs Lisp function
     `memory-limit'.  */
  eassert (increment == 0);
  return data_region_end;
}



/* MMAP allocation for buffers.  */

#define MAX_BUFFER_SIZE (512 * 1024 * 1024)

void *
mmap_alloc (void **var, size_t nbytes)
{
  void *p = NULL;

  /* We implement amortized allocation.  We start by reserving twice
     the size requested and commit only the size requested.  Then
     realloc could proceed and use the reserved pages, reallocating
     only if needed.  Buffer shrink would happen only so that we stay
     in the 2x range.  This is a big win when visiting compressed
     files, where the final size of the buffer is not known in
     advance, and the buffer is enlarged several times as the data is
     decompressed on the fly.  */
  if (nbytes < MAX_BUFFER_SIZE)
    p = VirtualAlloc (NULL, ROUND_UP (nbytes * 2, get_allocation_unit ()),
		      MEM_RESERVE, PAGE_READWRITE);

  /* If it fails, or if the request is above 512MB, try with the
     requested size.  */
  if (p == NULL)
    p = VirtualAlloc (NULL, ROUND_UP (nbytes, get_allocation_unit ()),
		      MEM_RESERVE, PAGE_READWRITE);

  if (p != NULL)
    {
      /* Now, commit pages for NBYTES.  */
      *var = VirtualAlloc (p, nbytes, MEM_COMMIT, PAGE_READWRITE);
      if (*var == NULL)
	p = *var;
    }

  if (!p)
    {
      DWORD e = GetLastError ();

      if (e == ERROR_NOT_ENOUGH_MEMORY)
	errno = ENOMEM;
      else
	{
	  DebPrint (("mmap_alloc: error %ld\n", e));
	  errno = EINVAL;
	}
    }

  return *var = p;
}

void
mmap_free (void **var)
{
  if (*var)
    {
      if (VirtualFree (*var, 0, MEM_RELEASE) == 0)
        DebPrint (("mmap_free: error %ld\n", GetLastError ()));
      *var = NULL;
    }
}

void *
mmap_realloc (void **var, size_t nbytes)
{
  MEMORY_BASIC_INFORMATION memInfo, m2;
  void *old_ptr;

  if (*var == NULL)
    return mmap_alloc (var, nbytes);

  /* This case happens in init_buffer().  */
  if (nbytes == 0)
    {
      mmap_free (var);
      return mmap_alloc (var, nbytes);
    }

  memset (&memInfo, 0, sizeof (memInfo));
  if (VirtualQuery (*var, &memInfo, sizeof (memInfo)) == 0)
    DebPrint (("mmap_realloc: VirtualQuery error = %ld\n", GetLastError ()));

  /* We need to enlarge the block.  */
  if (memInfo.RegionSize < nbytes)
    {
      memset (&m2, 0, sizeof (m2));
      if (VirtualQuery ((char *)*var + memInfo.RegionSize, &m2, sizeof(m2)) == 0)
        DebPrint (("mmap_realloc: VirtualQuery error = %ld\n",
		   GetLastError ()));
      /* If there is enough room in the current reserved area, then
	 commit more pages as needed.  */
      if (m2.State == MEM_RESERVE
	  && m2.AllocationBase == memInfo.AllocationBase
	  && nbytes <= memInfo.RegionSize + m2.RegionSize)
	{
	  void *p;

	  p = VirtualAlloc (*var, nbytes, MEM_COMMIT, PAGE_READWRITE);
	  if (!p /* && GetLastError() != ERROR_NOT_ENOUGH_MEMORY */)
	    {
	      DebPrint (("realloc enlarge: VirtualAlloc (%p + %I64x, %I64x) error %ld\n",
			 *var, (uint64_t)memInfo.RegionSize,
			 (uint64_t)(nbytes - memInfo.RegionSize),
			 GetLastError ()));
	      DebPrint (("next region: %p %p %I64x %x\n", m2.BaseAddress,
			 m2.AllocationBase, (uint64_t)m2.RegionSize,
			 m2.AllocationProtect));
	    }
	  else
	    return *var;
	}
      /* Else we must actually enlarge the block by allocating a new
	 one and copying previous contents from the old to the new one.  */
      old_ptr = *var;

      if (mmap_alloc (var, nbytes))
	{
	  CopyMemory (*var, old_ptr, memInfo.RegionSize);
	  mmap_free (&old_ptr);
	  return *var;
	}
      else
	{
	  /* We failed to reallocate the buffer.  */
	  *var = old_ptr;
	  return NULL;
	}
    }

  /* If we are shrinking by more than one page...  */
  if (memInfo.RegionSize  > nbytes + getpagesize())
    {
      /* If we are shrinking a lot...  */
      if ((memInfo.RegionSize / 2) > nbytes)
        {
          /* Let's give some memory back to the system and release
	     some pages.  */
          old_ptr = *var;

	  if (mmap_alloc (var, nbytes))
            {
              CopyMemory (*var, old_ptr, nbytes);
              mmap_free (&old_ptr);
              return *var;
            }
          else
	    {
	      /* In case we fail to shrink, try to go on with the old block.
		 But that means there is a lot of memory pressure.
		 We could also decommit pages.  */
	      *var = old_ptr;
	      return *var;
	    }
        }

      /* We still can decommit pages.  */
      if (VirtualFree ((char *)*var + nbytes + get_page_size(),
		       memInfo.RegionSize - nbytes - get_page_size(),
		       MEM_DECOMMIT) == 0)
        DebPrint (("mmap_realloc: VirtualFree error %ld\n", GetLastError ()));
      return *var;
    }

  /* Not enlarging, not shrinking by more than one page.  */
  return *var;
}


/* Emulation of getrlimit and setrlimit.  */

int
getrlimit (rlimit_resource_t rltype, struct rlimit *rlp)
{
  int retval = -1;

  switch (rltype)
    {
    case RLIMIT_STACK:
      {
	MEMORY_BASIC_INFORMATION m;
	/* Implementation note: Posix says that RLIMIT_STACK returns
	   information about the stack size for the main thread.  The
	   implementation below returns the stack size for the calling
	   thread, so it's more like pthread_attr_getstacksize.  But
	   Emacs clearly wants the latter, given how it uses the
	   results, so the implementation below is more future-proof,
	   if what's now the main thread will become some other thread
	   at some future point.  */
	if (!VirtualQuery ((LPCVOID) &m, &m, sizeof m))
	  errno = EPERM;
	else
	  {
	    rlp->rlim_cur = (DWORD_PTR) &m - (DWORD_PTR) m.AllocationBase;
	    rlp->rlim_max =
	      (DWORD_PTR) m.BaseAddress + m.RegionSize
	      - (DWORD_PTR) m.AllocationBase;

	    /* The last page is the guard page, so subtract that.  */
	    rlp->rlim_cur -= getpagesize ();
	    rlp->rlim_max -= getpagesize ();
	    retval = 0;
	  }
	}
      break;
    case RLIMIT_NOFILE:
      /* Implementation note: The real value is returned by
	 _getmaxstdio.  But our FD_SETSIZE is smaller, to cater to
	 Windows 9X, and process.c includes some logic that's based on
	 the assumption that the handle resource is inherited to child
	 processes.  We want to avoid that logic, so we tell process.c
	 our current limit is already equal to FD_SETSIZE.  */
      rlp->rlim_cur = FD_SETSIZE;
      rlp->rlim_max = 2048;	/* see _setmaxstdio documentation */
      retval = 0;
      break;
    default:
      /* Note: we could return meaningful results for other RLIMIT_*
	 requests, but Emacs doesn't currently need that, so we just
	 punt for them.  */
      errno = ENOSYS;
      break;
    }
  return retval;
}

int
setrlimit (rlimit_resource_t rltype, const struct rlimit *rlp)
{
  switch (rltype)
    {
    case RLIMIT_STACK:
    case RLIMIT_NOFILE:
      /* We cannot modify these limits, so we always fail.  */
      errno = EPERM;
      break;
    default:
      errno = ENOSYS;
      break;
    }
  return -1;
}
