/*  impl.c.vmnt
 *
 *                 VIRTUAL MEMORY MAPPING FOR WIN32
 *
 *  $HopeName: MMsrc!vmnt.c(trunk.7) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  Design: design.mps.vm
 *
 *  This is the implementation of the virtual memory mapping interface (vm.h)
 *  for Win32s.
 *
 *  The documentation for Win32 used is the "Win32 Programmer's Reference"
 *  provided with Microsoft Visual C++ 2.0.
 *
 *  VirtualAlloc is used to reserve address space and to "commit" (map)
 *  address ranges onto storage.  VirtualFree is used to release and
 *  "decommit" (unmap) pages.  These functions are documented in the
 *  Win32 SDK help, under System Services/Memory Management.
 *
 *  .assume.free.success:  We assume that VirtualFree will never return
 *    an error; this is because we always pass in legal parameters
 *    (hopefully).
 *
 *  .assume.not-last:  We assume that VirtualAlloc will never return
 *    a block of memory that occupies the last page in memory, so
 *    that limit is representable and bigger than base.
 *
 *  .assume.dword-addr:  We assume that the windows type DWORD and
 *    the MM type Addr are the same size.
 *
 *  .assume.lpvoid-addr:  We assume that the windows type LPVOID and
 *    the MM type Addr are the same size.
 *
 *  .assume.sysgrain:  The assume that the page size on the system
 *    is a power of two.
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

#ifndef MPS_OS_W3
#error "vmnt.c is Win32 specific, but MPS_OS_W3 is not set"
#endif

#include <stddef.h>
#include <windows.h>

SRCID("$HopeName: MMsrc!vmnt.c(trunk.7) $");

#define VMSig	((Sig)0x519FEE33)

typedef struct VMStruct
{
  Sig sig;
  Addr grain;		/* page size */
  Addr base, limit;     /* boundaries of reserved space */
} VMStruct;


Addr VMGrain(void)
{
  Addr grain;
  SYSTEM_INFO si;

  /* See .assume.dword-addr */
  AVER(sizeof(DWORD) == sizeof(Addr));

  GetSystemInfo(&si);
  grain = (Addr)si.dwPageSize;
  AVER(IsPoT(grain));    /* see .assume.sysgrain */

  return(grain);
}


#ifdef DEBUG

Bool VMIsValid(VM vm, ValidationType validParam)
{
  AVER(vm != NULL);
  AVER(vm->sig == VMSig);
  AVER(vm->base != 0);
  AVER(vm->limit != 0);
  AVER(vm->base < vm->limit);
  AVER(IsAligned(vm->grain, vm->base));
  AVER(IsAligned(vm->grain, vm->limit));
  return(TRUE);
}

#endif /* DEBUG */


Error VMCreate(VM *vmReturn, Addr size)
{
  LPVOID base;
  Addr grain;
  VM vm;

  AVER(vmReturn != NULL);
  AVER(sizeof(LPVOID) == sizeof(Addr));  /* .assume.lpvoid-addr */

  /* See .assume.dword-addr */
  AVER(sizeof(DWORD) == sizeof(Addr));

  grain = VMGrain();
  AVER(IsPoT(grain));    /* see .assume.sysgrain */

  AVER(IsAligned(grain, size));

  /* Allocate some store for the descriptor.
   * This is likely to be wasteful see issue.vmnt.waste */
  base = VirtualAlloc(NULL, AlignUp(grain, sizeof(VMStruct)),
          MEM_COMMIT, PAGE_READWRITE);
  if(base == NULL)
    return(ErrRESMEM);
  vm = (VM)base;

  /* Allocate the address space. */
  base = VirtualAlloc(NULL, size, MEM_RESERVE, PAGE_NOACCESS);
  if(base == NULL)
    return(ErrRESOURCE);

  AVER(IsAligned(grain, (Addr)base));

  vm->grain = grain;
  vm->base = (Addr)base;
  vm->limit = (Addr)base + size;
  AVER(vm->base < vm->limit);  /* .assume.not-last */
  
  vm->sig = VMSig;

  AVER(ISVALID(VM, vm));

  *vmReturn = vm;
  return(ErrSUCCESS);
}


void VMDestroy(VM vm)
{
  BOOL b;
  Addr grain;

  AVER(ISVALID(VM, vm));

  grain = vm->grain;

  /* This appears to be pretty pointless, since the vm descriptor page
   * is about to vanish completely.  However, the VirtaulFree might
   * fail and it would be nice to have a dead sig there. */
  vm->sig = SigInvalid;

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
  Addr grain = vm->grain;

  AVER(ISVALID(VM, vm));
  AVER(IsAligned(grain, base));
  AVER(IsAligned(grain, limit));
  AVER(vm->base <= base);
  AVER(base < limit);
  AVER(limit <= vm->limit);
  /* We could check that the pages we are about to map are unmapped
   * using VirtualQuery.  We could, but we don't. */

  b = VirtualAlloc((LPVOID)base, (DWORD)(limit - base),
       MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if(b == NULL)
    return(ErrRESMEM);

  AVER((Addr)b == base);        /* base should've been aligned */

  return(ErrSUCCESS);
}


void VMUnmap(VM vm, Addr base, Addr limit)
{
  Addr grain = vm->grain;
  BOOL b;

  AVER(ISVALID(VM, vm));
  AVER(IsAligned(grain, base));
  AVER(IsAligned(grain, limit));
  AVER(vm->base <= base);
  AVER(base < limit);
  AVER(limit <= vm->limit);
  /* Could check that the pages we are about to unmap are mapped
   * using VirtualQuery.  We could but we don't. */

  b = VirtualFree((LPVOID)base, (DWORD)(limit - base), MEM_DECOMMIT);
  AVER(b == TRUE);  /* .assume.free.success */
}
