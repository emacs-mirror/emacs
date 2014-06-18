/* vman.c: ANSI VM: MALLOC-BASED PSEUDO MEMORY MAPPING
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 */

#include "mpm.h"
#include "vm.h"

#include <stdlib.h>     /* for malloc and free */

SRCID(vman, "$Id$");


/* PageSize -- return the page size */

Size PageSize(void)
{
  return VMAN_PAGE_SIZE;
}


Res VMParamFromArgs(void *params, size_t paramSize, ArgList args)
{
  AVER(params != NULL);
  AVERT(ArgList, args);
  UNUSED(paramSize);
  return ResOK;
}


/* VMInit -- reserve some virtual address space, and create a VM structure */

Res VMInit(VM vm, Size size, Size grainSize, void *params)
{
  void *vbase;
  Size pageSize, reserved;

  AVER(vm != NULL);
  AVERT(ArenaGrainSize, grainSize);
  AVER(size > 0);
  AVER(params != NULL);

  pageSize = PageSize();

  /* Grains must consist of whole pages. */
  AVER(grainSize % pageSize == 0);

  /* Check that the rounded-up sizes will fit in a Size. */
  size = SizeRoundUp(size, grainSize);
  if (size < grainSize || size > (Size)(size_t)-1)
    return ResRESOURCE;
  /* Note that because we add a whole grainSize here (not grainSize -
   * pageSize), we are not in danger of overflowing vm->limit even if
   * malloc were perverse enough to give us a block at the end of
   * memory. Compare vmix.c#.assume.not-last. */
  reserved = size + grainSize;
  if (reserved < grainSize || reserved > (Size)(size_t)-1)
    return ResRESOURCE;

  vbase = malloc((size_t)reserved);
  if (vbase == NULL)
    return ResMEMORY;
  (void)mps_lib_memset(vbase, VMJunkBYTE, reserved);

  vm->pageSize = pageSize;
  vm->block = vbase;
  vm->base  = AddrAlignUp(vbase, grainSize);
  vm->limit = AddrAdd(vm->base, size);
  AVER(vm->base < vm->limit); /* can't overflow, as discussed above */
  AVER(vm->limit < AddrAdd((Addr)vm->block, reserved));
  vm->reserved = reserved;
  vm->mapped = (Size)0;
 
  vm->sig = VMSig;
  AVERT(VM, vm);
 
  EVENT3(VMInit, vm, VMBase(vm), VMLimit(vm));
  return ResOK;
}


/* VMFinish -- release all address space and finish VM structure */

void VMFinish(VM vm)
{
  AVERT(VM, vm);
  /* Descriptor must not be stored inside its own VM at this point. */
  AVER(PointerAdd(vm, sizeof *vm) <= vm->block
       || PointerAdd(vm->block, VMReserved(vm)) <= (Pointer)vm);
  /* All address space must have been unmapped. */
  AVER(VMMapped(vm) == (Size)0);

  EVENT1(VMFinish, vm);

  vm->sig = SigInvalid;

  (void)mps_lib_memset(vm->block, VMJunkBYTE, vm->reserved);
  free(vm->block);
}


/* VMMap -- map the given range of memory */

Res VMMap(VM vm, Addr base, Addr limit)
{
  Size size;

  AVER(base != (Addr)0);
  AVER(VMBase(vm) <= base);
  AVER(base < limit);
  AVER(limit <= VMLimit(vm));
  AVER(AddrIsAligned(base, vm->pageSize));
  AVER(AddrIsAligned(limit, vm->pageSize));

  size = AddrOffset(base, limit);
  (void)mps_lib_memset((void *)base, VMJunkByte, size);

  vm->mapped += size;
  AVER(VMMapped(vm) <= VMReserved(vm));

  EVENT3(VMMap, vm, base, limit);
  return ResOK;
}


/* VMUnmap -- unmap the given range of memory */

void VMUnmap(VM vm, Addr base, Addr limit)
{
  Size size;

  AVER(base != (Addr)0);
  AVER(VMBase(vm) <= base);
  AVER(base < limit);
  AVER(limit <= VMLimit(vm));
  AVER(AddrIsAligned(base, vm->pageSize));
  AVER(AddrIsAligned(limit, vm->pageSize));
 
  size = AddrOffset(base, limit);
  AVER(VMMapped(vm) >= size);

  (void)mps_lib_memset((void *)base, VMJunkBYTE, size);
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
