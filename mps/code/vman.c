/* vman.c: ANSI VM: MALLOC-BASED PSEUDO MEMORY MAPPING
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 */

#include "mpm.h"

#include <stdlib.h>     /* for malloc and free */
#include <string.h>     /* for memset */

SRCID(vman, "$Id$");


/* VMStruct -- virtual memory structure */

#define VMSig           ((Sig)0x519B3999) /* SIGnature VM */

/* ANSI fake VM structure, see <design/vman/> */
typedef struct VMStruct {
  Sig sig;                      /* <design/sig/> */
  Addr base, limit;             /* boundaries of malloc'd memory */
  void *block;                  /* pointer to malloc'd block, for free() */
  Size reserved;                /* total reserved address space */
  Size mapped;                  /* total mapped memory */
} VMStruct;


/* VMCheck -- check a VM structure */

Bool VMCheck(VM vm)
{
  CHECKS(VM, vm);
  CHECKL(vm->base != (Addr)0);
  CHECKL(vm->limit != (Addr)0);
  CHECKL(vm->base < vm->limit);
  CHECKL(AddrIsAligned(vm->base, VMANPageALIGNMENT));
  CHECKL(AddrIsAligned(vm->limit, VMANPageALIGNMENT));
  CHECKL(vm->block != NULL);
  CHECKL((Addr)vm->block <= vm->base);
  CHECKL(vm->mapped <= vm->reserved);
  return TRUE;
}


/* VMAlign -- return the page size */

Align VMAlign(VM vm)
{
  UNUSED(vm);
  return VMANPageALIGNMENT;
}


Res VMParamFromArgs(void *params, size_t paramSize, ArgList args)
{
  AVER(params != NULL);
  AVERT(ArgList, args);
  UNUSED(paramSize);
  return ResOK;
}


/* VMCreate -- reserve some virtual address space, and create a VM structure */

Res VMCreate(VM *vmReturn, Size size)
{
  VM vm;

  AVER(vmReturn != NULL);

  /* Note that because we add VMANPageALIGNMENT rather than */
  /* VMANPageALIGNMENT-1 we are not in danger of overflowing */
  /* vm->limit even if malloc were perverse enough to give us */
  /* a block at the end of memory. */
  size = SizeAlignUp(size, VMANPageALIGNMENT) + VMANPageALIGNMENT;
  if ((size < VMANPageALIGNMENT) || (size > (Size)(size_t)-1))
    return ResRESOURCE;

  vm = (VM)malloc(sizeof(VMStruct));
  if (vm == NULL)
    return ResMEMORY;

  vm->block = malloc((size_t)size);
  if (vm->block == NULL) {
    free(vm);
    return ResMEMORY;
  }

  vm->base  = AddrAlignUp((Addr)vm->block, VMANPageALIGNMENT);
  vm->limit = AddrAdd(vm->base, size - VMANPageALIGNMENT);
  AVER(vm->limit < AddrAdd((Addr)vm->block, size));

  memset((void *)vm->block, VMJunkBYTE, size);
 
  /* Lie about the reserved address space, to simulate real */
  /* virtual memory. */
  vm->reserved = size - VMANPageALIGNMENT;
  vm->mapped = (Size)0;
 
  vm->sig = VMSig;

  AVERT(VM, vm);
 
  EVENT3(VMCreate, vm, vm->base, vm->limit);
  *vmReturn = vm;
  return ResOK;
}


/* VMDestroy -- destroy the VM structure */

void VMDestroy(VM vm)
{
  /* All vm areas should have been unmapped. */
  AVERT(VM, vm);
  AVER(vm->mapped == (Size)0);
  AVER(vm->reserved == AddrOffset(vm->base, vm->limit));

  memset((void *)vm->base, VMJunkBYTE, AddrOffset(vm->base, vm->limit));
  free(vm->block);
 
  vm->sig = SigInvalid;
  free(vm);
 
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
  Size size;

  AVER(base != (Addr)0);
  AVER(vm->base <= base);
  AVER(base < limit);
  AVER(limit <= vm->limit);
  AVER(AddrIsAligned(base, VMANPageALIGNMENT));
  AVER(AddrIsAligned(limit, VMANPageALIGNMENT));

  size = AddrOffset(base, limit);
  memset((void *)base, (int)0, size);

  vm->mapped += size;

  EVENT3(VMMap, vm, base, limit);
  return ResOK;
}


/* VMUnmap -- unmap the given range of memory */

void VMUnmap(VM vm, Addr base, Addr limit)
{
  Size size;

  AVER(base != (Addr)0);
  AVER(vm->base <= base);
  AVER(base < limit);
  AVER(limit <= vm->limit);
  AVER(AddrIsAligned(base, VMANPageALIGNMENT));
  AVER(AddrIsAligned(limit, VMANPageALIGNMENT));
 
  size = AddrOffset(base, limit);
  memset((void *)base, 0xCD, size);

  AVER(vm->mapped >= size);
  vm->mapped -= size;

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
