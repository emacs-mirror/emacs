/* impl.c.vman: ANSI VM: MALLOC-BASED PSEUDO MEMORY MAPPING
 *
 * $HopeName: MMsrc!vman.c(trunk.21) $
 * Copyright (C) 1998 Harlequin Limited.  All rights reserved.
 */

#include "mpm.h"

#include <stdlib.h>     /* for malloc and free */
#include <string.h>     /* for memset */

SRCID(vman, "$HopeName: MMsrc!vman.c(trunk.21) $");


/* VMStruct -- virtual memory structure */

#define VMSig           ((Sig)0x519B3999) /* SIGnature VM */

/* ANSI fake VM structure, see design.mps.vman */
typedef struct VMStruct {
  Sig sig;                      /* design.mps.sig */
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
  
  EVENT_PAA(VMCreate, vm, vm->base, vm->limit);
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
  
  EVENT_P(VMDestroy, vm);
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

  EVENT_PAA(VMMap, vm, base, limit);
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
  memset((void *)base, VM_JUNKBYTE, size);

  AVER(vm->mapped >= size);
  vm->mapped -= size;

  EVENT_PAA(VMUnmap, vm, base, limit);
}
