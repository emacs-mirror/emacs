/* vmw3.c: VIRTUAL MEMORY MAPPING FOR WIN32
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
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
#include "vm.h"

#ifndef MPS_OS_W3
#error "vmw3.c is Win32 specific, but MPS_OS_W3 is not set"
#endif

#include "mpswin.h"

SRCID(vmw3, "$Id$");


/* PageSize -- return the operating system page size */

Size PageSize(void)
{
  SYSTEM_INFO si;

  /* Find out the page size from the OS */
  GetSystemInfo(&si);

  /* Check the page size will fit in a Size. */
  AVER(si.dwPageSize <= (Size)(SIZE_T)-1);

  return (Size)si.dwPageSize;
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


/* VMInit -- reserve some virtual address space, and create a VM structure */

Res VMInit(VM vm, Size size, Size grainSize, void *params)
{
  LPVOID vbase;
  Size pageSize, reserved;
  VMParams vmParams = params;

  AVER(vm != NULL);
  AVERT(ArenaGrainSize, grainSize);
  AVER(size > 0);
  AVER(params != NULL); /* FIXME: Should have full AVERT? */

  AVER(COMPATTYPE(LPVOID, Addr));  /* .assume.lpvoid-addr */
  AVER(COMPATTYPE(SIZE_T, Size));

  pageSize = PageSize();

  /* Grains must consist of whole pages. */
  AVER(grainSize % pageSize == 0);

  /* Check that the rounded-up sizes will fit in a Size. */
  size = SizeRoundUp(size, grainSize);
  if (size < grainSize || size > (Size)(SIZE_T)-1)
    return ResRESOURCE;
  reserved = size + grainSize - pageSize;
  if (reserved < grainSize || reserved > (Size)(SIZE_T)-1)
    return ResRESOURCE;

  /* Allocate the address space. */
  vbase = VirtualAlloc(NULL,
                       reserved,
                       vmParams->topDown ?
                         MEM_RESERVE | MEM_TOP_DOWN :
                         MEM_RESERVE,
                       PAGE_NOACCESS);
  if (vbase == NULL)
    return ResRESOURCE;

  AVER(AddrIsAligned(vbase, pageSize));

  vm->pageSize = pageSize;
  vm->block = vbase;
  vm->base = AddrAlignUp(vbase, grainSize);
  vm->limit = AddrAdd(vm->base, size);
  AVER(vm->base < vm->limit);  /* .assume.not-last */
  AVER(vm->limit <= AddrAdd((Addr)vm->block, reserved));
  vm->reserved = reserved;
  vm->mapped = 0;

  vm->sig = VMSig;
  AVERT(VM, vm);

  EVENT3(VMInit, vm, VMBase(vm), VMLimit(vm));
  return ResOK;
}


/* VMFinish -- release address space and finish the VM structure */

void VMFinish(VM vm)
{
  BOOL b;

  AVERT(VM, vm);
  /* Descriptor must not be stored inside its own VM at this point. */
  AVER(PointerAdd(vm, sizeof *vm) <= vm->block
       || PointerAdd(vm->block, VMReserved(vm)) <= (Pointer)vm);
  /* All address space must have been unmapped. */
  AVER(VMMapped(vm) == (Size)0);

  EVENT1(VMFinish, vm);

  vm->sig = SigInvalid;

  b = VirtualFree((LPVOID)vm->block, (SIZE_T)0, MEM_RELEASE);
  AVER(b != 0);
}


/* VMMap -- map the given range of memory */

Res VMMap(VM vm, Addr base, Addr limit)
{
  LPVOID b;

  AVERT(VM, vm);
  AVER(AddrIsAligned(base, vm->pageSize));
  AVER(AddrIsAligned(limit, vm->pageSize));
  AVER(VMBase(vm) <= base);
  AVER(base < limit);
  AVER(limit <= VMLimit(vm));

  /* .improve.query-map: We could check that the pages we are about to
   * map are unmapped using VirtualQuery. */

  b = VirtualAlloc((LPVOID)base, (SIZE_T)AddrOffset(base, limit),
                   MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if (b == NULL)
    return ResMEMORY;
  AVER((Addr)b == base);        /* base should've been aligned */

  vm->mapped += AddrOffset(base, limit);
  AVER(VMMapped(vm) <= VMReserved(vm));

  EVENT3(VMMap, vm, base, limit);
  return ResOK;
}


/* VMUnmap -- unmap the given range of memory */

void VMUnmap(VM vm, Addr base, Addr limit)
{
  BOOL b;
  Size size;

  AVERT(VM, vm);
  AVER(AddrIsAligned(base, vm->pageSize));
  AVER(AddrIsAligned(limit, vm->pageSize));
  AVER(VMBase(vm) <= base);
  AVER(base < limit);
  AVER(limit <= VMLimit(vm));

  size = AddrOffset(base, limit);
  AVER(size <= VMMapped(vm));

  /* .improve.query-unmap: Could check that the pages we are about */
  /* to unmap are mapped, using VirtualQuery. */
  b = VirtualFree((LPVOID)base, (SIZE_T)size, MEM_DECOMMIT);
  AVER(b != 0);  /* .assume.free.success */
  vm->mapped -= size;

  EVENT3(VMUnmap, vm, base, limit);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
