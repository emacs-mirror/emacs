/*  impl.c.vmw3: VIRTUAL MEMORY MAPPING FOR WIN32
 *
 *  $HopeName: MMsrc!vmw3.c(trunk.25) $
 *  Copyright (C) 1997, 1998 Harlequin Group, all rights reserved
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
 *  .assume.dword-align:  We assume that the windows type DWORD and
 *    the MM type Align are assignment-compatible.
 *
 *  .assume.lpvoid-addr:  We assume that the windows type LPVOID and
 *    the MM type Addr are assignment-compatible.
 *
 *  .assume.sysalign:  The assume that the page size on the system
 *    is a power of two.
 *
 *  Notes
 *   1. GetSystemInfo returns a thing called szAllocationGranularity
 *      the purpose of which is unclear but which might affect the
 *      reservation of address space.  Experimentally, it does not.
 *      Microsoft's documentation is extremely unclear on this point.
 *      richard 1995-02-15
 */

#include "mpm.h"

#ifndef MPS_OS_W3
#error "vmw3.c is Win32 specific, but MPS_OS_W3 is not set"
#endif
#ifdef VM_RM
#error "vmw3.c compiled with VM_RM set"
#endif

#include "mpswin.h"

SRCID(vmw3, "$HopeName: MMsrc!vmw3.c(trunk.25) $");


/* VMStruct -- virtual memory structure */

#define VMSig           ((Sig)0x519B3999) /* SIGnature VM */

typedef struct VMStruct {
  Sig sig;                      /* design.mps.sig */
  Align align;                  /* page size */
  Addr base, limit;             /* boundaries of reserved space */
  Size reserved;                /* total reserved address space */
  Size mapped;                  /* total mapped memory */
} VMStruct;


Align VMAlign(VM vm)
{
  return vm->align;
}


Bool VMCheck(VM vm)
{
  CHECKS(VM, vm);
  CHECKL(vm->base != 0);
  CHECKL(vm->limit != 0);
  CHECKL(vm->base < vm->limit);
  CHECKL(vm->mapped <= vm->reserved);
  CHECKL(AddrIsAligned(vm->base, vm->align));
  CHECKL(AddrIsAligned(vm->limit, vm->align));
  return TRUE;
}




/* VMRAMSize -- determine the RAM size for the platform */
/* This is not a protocol function - but it could be in future */

static Res VMRAMSize(VM vm, Size *vmRAMSizeReturn)
{
  MEMORYSTATUS memstat;

  AVERT(VM, vm);
 
  memstat.dwLength = sizeof(MEMORYSTATUS);
  GlobalMemoryStatus(&memstat);
  *vmRAMSizeReturn = (Size)memstat.dwTotalPhys;
  return ResOK;
}


/* VMSetCollectionStrategy -- initialize strategy for platform */
/* This is not a protocol function - but it could be in future */

static Res VMSetCollectionStrategy(VM vm)
{
  Res res;
  Size vmRAMSize;

  AVERT(VM, vm);

  res = VMRAMSize(vm, &vmRAMSize);

  /* Adjust the collection frequencies according to the RAM size */
  /* The magic numbers are the result of (some) benchmark testing */
  /* Note that the RAM size returned is actually slightly less */
  /* than the total RAM size (for some reason) - so there needs to */
  /* be a comparison against a lower value than the optimal size */
  if (ResOK == res) {
    if (vmRAMSize >= 110*1024*1024) {
      /* Parameters optimized for 128 MB machines */
      /* Source: drj's test data, 1998-03-30 */
      AMCGen0Frequency = 16;
      AMCGen1Frequency = 160;
      AMCGen2Frequency = 1000;
    } else if (vmRAMSize >= 60*1024*1024) {
      /* Parameters optimized for 64 MB machines */
      /* Source: RIT's test data, 1998-03-26 */
      AMCGen0Frequency = 6;
      AMCGen1Frequency = 48;
      AMCGen2Frequency = 480;
    } else {
      /* Parameters optimized for 32 MB machines */
      /* Source: Tony's test data, 1998-03-26 */
      AMCGen0Frequency = 4;
      AMCGen1Frequency = 20;
      AMCGen2Frequency = 300;
    }
  }
  return res;
}


/* VMCreate -- reserve some virtual address space, and create a VM structure */

Res VMCreate(VM *vmReturn, Size size)
{
  LPVOID vbase;
  SYSTEM_INFO si;
  Align align;
  VM vm;
  Res sres;

  AVER(vmReturn != NULL);

  AVER(CHECKTYPE(LPVOID, Addr));  /* .assume.lpvoid-addr */
  AVER(sizeof(DWORD) == sizeof(Addr));  /* See .assume.dword-addr */
  AVER(CHECKTYPE(DWORD, Align));  /* See .assume.dword-align */

  GetSystemInfo(&si);
  align = (Align)si.dwPageSize;
  AVER(SizeIsP2(align));    /* see .assume.sysalign */
  size = SizeAlignUp(size, align);
  if((size == 0) || (size > (Size)(DWORD)-1))
    return ResRESOURCE;

  /* Allocate the vm descriptor.  This is likely to be wasteful. */
  vbase = VirtualAlloc(NULL, SizeAlignUp(sizeof(VMStruct), align),
		       MEM_COMMIT, PAGE_READWRITE);
  if(vbase == NULL)
    return ResMEMORY;
  vm = (VM)vbase;

  /* Allocate the address space. */
  vbase = VirtualAlloc(NULL, size, MEM_RESERVE, PAGE_NOACCESS);
  if(vbase == NULL)
    return ResRESOURCE;

  AVER(AddrIsAligned(vbase, align));

  vm->align = align;
  vm->base = (Addr)vbase;
  vm->limit = AddrAdd(vbase, size);
  vm->reserved = size;
  vm->mapped = 0;
  AVER(vm->base < vm->limit);  /* .assume.not-last */

  vm->sig = VMSig;

  AVERT(VM, vm);

  sres = VMSetCollectionStrategy(vm);
  AVER(ResOK == sres);

  EVENT_PAA(VMCreate, vm, vm->base, vm->limit);

  *vmReturn = vm;
  return ResOK;
}


void VMDestroy(VM vm)
{
  BOOL b;

  AVERT(VM, vm);
  AVER(vm->mapped == 0);

  /* This appears to be pretty pointless, since the vm descriptor page
   * is about to vanish completely.  However, the VirtualFree might
   * fail and it would be nice to have a dead sig there. */
  vm->sig = SigInvalid;

  b = VirtualFree((LPVOID)vm->base, (DWORD)0, MEM_RELEASE);
  AVER(b != 0);

  b = VirtualFree((LPVOID)vm, (DWORD)0, MEM_RELEASE);
  AVER(b != 0);
  EVENT_P(VMDestroy, vm);
}


/* VMBase -- return the base address of the memory reserved */

Addr VMBase(VM vm)
{
  AVERT(VM, vm);

  return vm->base;
}


Addr VMLimit(VM vm)
{
  AVERT(VM, vm);

  return vm->limit;
}


Size VMReserved(VM vm)
{
  AVERT(VM, vm);

  return vm->reserved;
}


Size VMMapped(VM vm)
{
  AVERT(VM, vm);

  return vm->mapped;
}


Res VMMap(VM vm, Addr base, Addr limit)
{
  LPVOID b;
  Align align;

  AVERT(VM, vm);
  align = vm->align;
  AVER(AddrIsAligned(base, align));
  AVER(AddrIsAligned(limit, align));
  AVER(vm->base <= base);
  AVER(base < limit);
  AVER(limit <= vm->limit);

  /* .improve.query-map: We could check that the pages we are about to
   * map are unmapped using VirtualQuery. */

  b = VirtualAlloc((LPVOID)base, (DWORD)AddrOffset(base, limit),
       MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if(b == NULL)
    return ResMEMORY;

  AVER((Addr)b == base);        /* base should've been aligned */

  vm->mapped += AddrOffset(base, limit);

  EVENT_PAA(VMMap, vm, base, limit);

  return ResOK;
}


void VMUnmap(VM vm, Addr base, Addr limit)
{
  Align align;
  BOOL b;

  AVERT(VM, vm);
  align = vm->align;
  AVER(AddrIsAligned(base, align));
  AVER(AddrIsAligned(limit, align));
  AVER(vm->base <= base);
  AVER(base < limit);
  AVER(limit <= vm->limit);

  /* .improve.query-unmap: Could check that the pages we are about
   * to unmap are mapped using VirtualQuery. */

  b = VirtualFree((LPVOID)base, (DWORD)AddrOffset(base, limit), MEM_DECOMMIT);
  AVER(b != 0);  /* .assume.free.success */
  vm->mapped -= AddrOffset(base, limit);

  EVENT_PAA(VMUnmap, vm, base, limit);
}
