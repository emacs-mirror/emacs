/*  impl.c.vmnt
 *
 *                 VIRTUAL MEMORY MAPPING FOR WIN32
 *
 *  $HopeName: MMsrc/!vmnt.c(trunk.1)$
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This is the implementation of the virtual memory mapping interface (vm.h)
 *  for Win32s.
 *
 *  VirtualAlloc is used to reserve address space and to "commit" (map)
 *  address ranges onto storage.  VirtualFree is used to release and
 *  "decommit" (unmap) pages.  These functions are documented in the
 *  Win32 SDK help, under System Services/Memory Management.
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
#error "vmnt.c is Win32 specific, but OS_NT is not set"
#endif

#include <stddef.h>
#include <windows.h>


#ifdef DEBUG_SIGN
static SigStruct VMSigStruct;
#endif


typedef struct VMStruct
{
#ifdef DEBUG_SIGN
  Sig sig;
#endif
  Addr base, limit;     /* boundaries of reserved space */
} VMStruct;


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


#ifdef DEBUG_ASSERT

Bool VMIsValid(VM vm, ValidationType validParam)
{
  AVER(vm != NULL);
#ifdef DEBUG_SIGN
  AVER(ISVALIDNESTED(Sig, vm->sig));
  AVER(vm->sig == &VMSigStruct);
#endif
  AVER(vm->base != 0);
  AVER(vm->limit != 0);
  AVER(vm->base < vm->limit);
  AVER(IsAligned(VMGrain(), vm->base));
  AVER(IsAligned(VMGrain(), vm->limit));
  return(TRUE);
}

#endif /* DEBUG_ASSERT */


Error VMCreate(VM *vmReturn, Addr size)
{
  LPVOID base;
  Addr grain = VMGrain();
  VM vm;

  AVER(vmReturn != NULL);
  AVER(IsAligned(grain, size));
  AVER(sizeof(LPVOID) == sizeof(Addr));

  /* Allocate in a page to store the descriptor on. */
  base = VirtualAlloc(NULL, AlignUp(grain, sizeof(VMStruct)),
          MEM_COMMIT, PAGE_READWRITE);
  if(base == NULL)
    return(ErrRESOURCE);
  vm = (VM)base;

  /* Allocate the address space. */
  base = VirtualAlloc(NULL, size, MEM_RESERVE, PAGE_NOACCESS);
  if(base == NULL)
    return(ErrRESOURCE);

  AVER(IsAligned(grain, (Addr)base));

  vm->base = (Addr)base;
  vm->limit = (Addr)base + size;
  
#ifdef DEBUG_SIGN
  SigInit(&VMSigStruct, "VM");
  vm->sig = &VMSigStruct;
#endif

  AVER(ISVALID(VM, vm));

  *vmReturn = vm;
  return(ErrSUCCESS);
}


void VMDestroy(VM vm)
{
  BOOL b;
  Addr grain = VMGrain();

  AVER(ISVALID(VM, vm));

  /* Note: There is no need to invalidate vm->sig since the page it's */
  /* on is about to disappear from memory. */

  b = VirtualFree((LPVOID)vm->base, (DWORD)0, MEM_RELEASE);
  AVER(b == TRUE);
  
  b = VirtualFree((LPVOID)vm, (DWORD)0, MEM_RELEASE);
  AVER(b == TRUE);
}


Addr VMBase(VM vm)
{
  AVER(ISVALID(VM, vm));
  return(vm->base);
}

Addr VMLimit(VM vm)
{
  AVER(ISVALID(VM, vm));
  return(vm->limit);
}


Error VMMap(VM vm, Addr base, Addr limit)
{
  LPVOID b;
#ifdef DEBUG_ASSERT
  Addr grain = VMGrain();
#endif

  AVER(ISVALID(VM, vm));
  AVER(IsAligned(grain, base));
  AVER(IsAligned(grain, limit));
  AVER(base < limit);
  AVER(base >= vm->base);
  AVER(limit <= vm->limit);

  b = VirtualAlloc((LPVOID)base, (DWORD)(limit - base),
       MEM_COMMIT, PAGE_READWRITE);
  if(b == NULL)
    return(ErrRESMEM);

  AVER((Addr)b == base);        /* base should've been aligned */

  return(ErrSUCCESS);
}


void VMUnmap(VM vm, Addr base, Addr limit)
{
#ifdef DEBUG_ASSERT
  Addr grain = VMGrain();
#endif
  BOOL b;

  AVER(ISVALID(VM, vm));
  AVER(IsAligned(grain, base));
  AVER(IsAligned(grain, limit));
  AVER(base < limit);
  AVER(base >= vm->base);
  AVER(limit <= vm->limit);

  b = VirtualFree((LPVOID)base, (DWORD)(limit - base), MEM_DECOMMIT);
  AVER(b == TRUE);
}
