/*  impl.c.vmnt: VIRTUAL MEMORY MAPPING FOR WIN32
 *
 *  $HopeName: MMsrc!vmw3.c(trunk.17) $
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
 *  .assume.dword-align:  We assume that the windows type DWORD and
 *    the MM type Align are the same size.
 *
 *  .assume.lpvoid-addr:  We assume that the windows type LPVOID and
 *    the MM type Addr are the same size.
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
#error "vmnt.c is Win32 specific, but MPS_OS_W3 is not set"
#endif
#ifdef VM_RM
#error "vmnt.c compiled with VM_RM set"
#endif

#include <windows.h>

SRCID(vmnt, "$HopeName: MMsrc!vmw3.c(trunk.17) $");


#define SpaceVM(space)  (&(space)->arenaStruct.vmStruct)

Align VMAlign(void)
{
  Align align;
  SYSTEM_INFO si;

  /* See .assume.dword-align */
  AVER(sizeof(DWORD) == sizeof(Align));

  GetSystemInfo(&si);
  align = (Align)si.dwPageSize;
  AVER(SizeIsP2(align));    /* see .assume.sysalign */

  return align;
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


Res VMCreate(Space *spaceReturn, Size size, Addr base)
{
  LPVOID vbase;
  Align align;
  VM vm;
  Space space;

  AVER(spaceReturn != NULL);
  AVER(sizeof(LPVOID) == sizeof(Addr));  /* .assume.lpvoid-addr */

  /* See .assume.dword-addr */
  AVER(sizeof(DWORD) == sizeof(Addr));

  align = VMAlign();
  AVER(SizeIsP2(align));    /* see .assume.sysalign */

  AVER(SizeIsAligned(size, align));
  AVER(base == NULL);

  /* Allocate some store for the space descriptor.
   * This is likely to be wasteful see issue.vmnt.waste */
  vbase = VirtualAlloc(NULL, SizeAlignUp(sizeof(SpaceStruct), align),
          MEM_COMMIT, PAGE_READWRITE);
  if(vbase == NULL)
    return ResMEMORY;
  space = (Space)vbase;
  vm = SpaceVM(space);

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

  EVENT4(VMCreate, vm, space, vm->base, vm->limit);

  *spaceReturn = space;
  return ResOK;
}


void VMDestroy(Space space)
{
  BOOL b;
  VM vm;

  vm = SpaceVM(space);
  AVERT(VM, vm);
  AVER(vm->mapped == 0);

  /* This appears to be pretty pointless, since the vm descriptor page
   * is about to vanish completely.  However, the VirtualFree might
   * fail and it would be nice to have a dead sig there. */
  vm->sig = SigInvalid;

  b = VirtualFree((LPVOID)vm->base, (DWORD)0, MEM_RELEASE);
  AVER(b != 0);

  b = VirtualFree((LPVOID)space, (DWORD)0, MEM_RELEASE);
  AVER(b != 0);
  EVENT1(VMDestroy, vm);
}


Addr VMBase(Space space)
{
  VM vm = SpaceVM(space);

  AVERT(VM, vm);
  return vm->base;
}

Addr VMLimit(Space space)
{
  VM vm = SpaceVM(space);

  AVERT(VM, vm);
  return vm->limit;
}


Size VMReserved(Space space)
{
  VM vm = SpaceVM(space);
  AVERT(VM, vm);
  return vm->reserved;
}

Size VMMapped(Space space)
{
  VM vm = SpaceVM(space);
  AVERT(VM, vm);
  return vm->mapped;
}


Res VMMap(Space space, Addr base, Addr limit)
{
  VM vm = SpaceVM(space);
  LPVOID b;
  Align align = vm->align;

  AVERT(VM, vm);
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

  EVENT3(VMMap, vm, base, limit);

  return ResOK;
}


void VMUnmap(Space space, Addr base, Addr limit)
{
  VM vm = SpaceVM(space);
  Align align = vm->align;
  BOOL b;

  AVERT(VM, vm);
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

  EVENT3(VMUnmap, vm, base, limit);
}
