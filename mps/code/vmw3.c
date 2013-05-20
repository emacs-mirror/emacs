/* vmw3.c: VIRTUAL MEMORY MAPPING FOR WIN32
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .design: See <design/vm/>.
 *
 * .purpose: This is the implementation of the virtual memory mapping
 * interface (vm.h) for Win32s.
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
 *  .assume.lpvoid-addr:  We assume that the windows type LPVOID and
 *    the MM type Addr are assignment-compatible.
 *
 *  .assume.sysalign: We assume that the page size on the system
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

SRCID(vmw3, "$Id$");


/* VMStruct -- virtual memory structure */

#define VMSig           ((Sig)0x519B3999) /* SIGnature VM */

typedef struct VMStruct {
  Sig sig;                      /* <design/sig/> */
  Align align;                  /* page size */
  Addr base, limit;             /* boundaries of reserved space */
  Size reserved;                /* total reserved address space */
  Size mapped;                  /* total mapped memory */
} VMStruct;


/* VMAlign -- return the page size */

Align VMAlign(VM vm)
{
  AVERT(VM, vm);

  return vm->align;
}


/* VMCheck -- check a VM structure */

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


typedef struct VMParamsStruct {
  Bool topDown;
} VMParamsStruct, *VMParams;

static const VMParamsStruct vmParamsDefaults = {
  /* .topDown = */ FALSE,
};

Res VMParamFromArgs(void *params, size_t paramSize, ArgList args)
{
  VMParams vmParams;
  ArgStruct arg;
  AVER(params != NULL);
  AVERT(ArgList, args);
  AVER(paramSize >= sizeof(VMParamsStruct));
  UNUSED(paramSize);
  vmParams = (VMParams)params;
  memcpy(vmParams, &vmParamsDefaults, sizeof(VMParamsStruct));
  if (ArgPick(&arg, args, MPS_KEY_VMW3_TOP_DOWN))
    vmParams->topDown = arg.val.b;
  return ResOK;
}


/* VMCreate -- reserve some virtual address space, and create a VM structure */

Res VMCreate(VM *vmReturn, Size size, void *params)
{
  LPVOID vbase;
  SYSTEM_INFO si;
  Align align;
  VM vm;
  Res res;
  BOOL b;
  VMParams vmParams = params;

  AVER(vmReturn != NULL);
  AVER(params != NULL); /* FIXME: Should have full AVERT? */

  AVER(COMPATTYPE(LPVOID, Addr));  /* .assume.lpvoid-addr */
  AVER(COMPATTYPE(SIZE_T, Size));

  GetSystemInfo(&si);
  align = (Align)si.dwPageSize;
  AVER((DWORD)align == si.dwPageSize); /* check it didn't truncate */
  AVER(SizeIsP2(align));    /* see .assume.sysalign */
  size = SizeAlignUp(size, align);
  if ((size == 0) || (size > (Size)(SIZE_T)-1))
    return ResRESOURCE;

  /* Allocate the vm descriptor.  This is likely to be wasteful. */
  vbase = VirtualAlloc(NULL, SizeAlignUp(sizeof(VMStruct), align),
                       MEM_COMMIT, PAGE_READWRITE);
  if (vbase == NULL)
    return ResMEMORY;
  vm = (VM)vbase;

  /* Allocate the address space. */
  vbase = VirtualAlloc(NULL,
                       size,
                       vmParams->topDown ?
                         MEM_RESERVE | MEM_TOP_DOWN :
                         MEM_RESERVE,
                       PAGE_NOACCESS);
  if (vbase == NULL) {
    res = ResRESOURCE;
    goto failReserve;
  }

  AVER(AddrIsAligned(vbase, align));

  vm->align = align;
  vm->base = (Addr)vbase;
  vm->limit = AddrAdd(vbase, size);
  vm->reserved = size;
  vm->mapped = 0;
  AVER(vm->base < vm->limit);  /* .assume.not-last */

  vm->sig = VMSig;
  AVERT(VM, vm);

  EVENT3(VMCreate, vm, vm->base, vm->limit);
  *vmReturn = vm;
  return ResOK;

failReserve:
  b = VirtualFree((LPVOID)vm, (SIZE_T)0, MEM_RELEASE);
  AVER(b != 0);
  return res;
}


/* VMDestroy -- destroy the VM structure */

void VMDestroy(VM vm)
{
  BOOL b;

  AVERT(VM, vm);
  AVER(vm->mapped == 0);

  /* This appears to be pretty pointless, since the vm descriptor page
   * is about to vanish completely.  However, the VirtualFree might
   * fail and it would be nice to have a dead sig there. */
  vm->sig = SigInvalid;

  b = VirtualFree((LPVOID)vm->base, (SIZE_T)0, MEM_RELEASE);
  AVER(b != 0);

  b = VirtualFree((LPVOID)vm, (SIZE_T)0, MEM_RELEASE);
  AVER(b != 0);
  EVENT1(VMDestroy, vm);
}


/* VMBase -- return the base address of the memory reserved */

Addr VMBase(VM vm)
{
  AVERT(VM, vm);

  return vm->base;
}


/* VMLimit -- return the limit address of the memory reserved */

Addr VMLimit(VM vm)
{
  AVERT(VM, vm);

  return vm->limit;
}


/* VMReserved -- return the amount of address space reserved */

Size VMReserved(VM vm)
{
  AVERT(VM, vm);

  return vm->reserved;
}


/* VMMapped -- return the amount of memory actually mapped */

Size VMMapped(VM vm)
{
  AVERT(VM, vm);

  return vm->mapped;
}


/* VMMap -- map the given range of memory */

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

  b = VirtualAlloc((LPVOID)base, (SIZE_T)AddrOffset(base, limit),
                   MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if (b == NULL)
    return ResMEMORY;
  AVER((Addr)b == base);        /* base should've been aligned */

  vm->mapped += AddrOffset(base, limit);

  EVENT3(VMMap, vm, base, limit);
  return ResOK;
}


/* VMUnmap -- unmap the given range of memory */

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

  /* .improve.query-unmap: Could check that the pages we are about */
  /* to unmap are mapped, using VirtualQuery. */
  b = VirtualFree((LPVOID)base, (SIZE_T)AddrOffset(base, limit), MEM_DECOMMIT);
  AVER(b != 0);  /* .assume.free.success */
  vm->mapped -= AddrOffset(base, limit);

  EVENT3(VMUnmap, vm, base, limit);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
