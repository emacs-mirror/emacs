/*  ==== VIRTUAL MEMORY MAPPING FOR WIN32S ====
 *
 *  $HopeName$
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This is the implementation of the virtual memory mapping interface (vm.h)
 *  for Win32s.
 *
 *  VirtualAlloc is used to reserve address space and to "commit" (map)
 *  address ranges onto storage.  VirtualFree is used to release and
 *  "decommit" (unmap) pages.
 *
 *  Notes
 *   1. GetSystemInfo returns a thing called szAllocationGranularity
 *      the purpose of which is unclear but which might affect the
 *      reservation of address space.  Experimentally, it does not.
 *      Microsoft's documentation is extremely unclear on this point.
 *      richard 1995-02-15
 */

#include "std.h"
#include "vm.h"

#ifndef OS_NT
#error "vmnt.c is NT specific, but OS_NT is not set"
#endif

#include <stddef.h>
#include <windows.h>


Addr VMGrain(void)
{
  Addr grain;
  SYSTEM_INFO si;

  AVER(sizeof(DWORD) == sizeof(Addr));

  GetSystemInfo(&si);
  grain = (Addr)si.dwPageSize;

  AVER(IsPoT(grain));

  return(grain);
}


Error VMReserve(Addr *baseReturn, Addr *limitReturn, Addr size)
{
  LPVOID base;
#ifdef DEBUG_ASSERT
  Addr grain = VMGrain();
#endif

  AVER(size > 0);
  AVER(IsAligned(grain, size));
  AVER(sizeof(LPVOID) == sizeof(Addr));

  base = VirtualAlloc(NULL, size, MEM_RESERVE, PAGE_NOACCESS);
  if(base == NULL)
    return(ErrRESOURCE);

  AVER(IsAligned(grain, (Addr)base));

  *baseReturn = (Addr)base;
  *limitReturn = (Addr)base + size;
  return(ErrSUCCESS);
}


void VMRelease(Addr base, Addr limit)
{
  BOOL b;
#ifdef DEBUG_ASSERT
  Addr grain = VMGrain();
#endif

  AVER(IsAligned(grain, base));
  AVER(IsAligned(grain, limit));
  AVER(base < limit);
  AVER(base != 0);

  b = VirtualFree((LPVOID)base, (DWORD)(limit - base), MEM_RELEASE);
  AVER(b == TRUE);
}


Error VMMap(Addr base, Addr limit)
{
  LPVOID b;
#ifdef DEBUG_ASSERT
  Addr grain = VMGrain();
#endif

  AVER(IsAligned(grain, base));
  AVER(IsAligned(grain, limit));
  AVER(base < limit);
  AVER(base != 0);

  b = VirtualAlloc((LPVOID)base, (DWORD)(limit - base),
		   MEM_COMMIT, PAGE_READWRITE);
  if(b == NULL)
    return(ErrRESMEM);

  AVER((Addr)b == base);	/* base should've been aligned */

  return(ErrSUCCESS);
}


void VMUnmap(Addr base, Addr limit)
{
#ifdef DEBUG_ASSERT
  Addr grain = VMGrain();
#endif
  BOOL b;

  AVER(IsAligned(grain, base));
  AVER(IsAligned(grain, limit));
  AVER(base < limit);
  AVER(base != 0);

  b = VirtualFree((LPVOID)base, (DWORD)(limit - base), MEM_DECOMMIT);
  AVER(b == TRUE);
}
