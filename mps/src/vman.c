/* impl.c.vman: ANSI VM: MALLOC-BASED PSUEDO MEMORY MAPPING
 *
 * $HopeName: MMsrc!vman.c(MMdevel_restr.4) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 */

#include "mpm.h"
#include <stdlib.h>     /* for malloc and free */
#include <string.h>     /* for memset */

SRCID(vman, "$HopeName: MMsrc!vman.c(MMdevel_restr.4) $");

#define SpaceVM(_space) (&(_space)->arenaStruct.vmStruct)

Bool VMCheck(VM vm)
{
  CHECKS(VM, vm);
  CHECKL(vm->base != (Addr)0);
  CHECKL(vm->limit != (Addr)0);
  CHECKL(vm->base < vm->limit);
  CHECKL(AddrIsAligned(vm->base, VMAN_ALIGN));
  CHECKL(AddrIsAligned(vm->limit, VMAN_ALIGN));
  CHECKL(vm->block != NULL);
  CHECKL((Addr)vm->block <= vm->base);
  CHECKL(vm->mapped <= vm->reserved);
  return TRUE;
}

Align VMAlign()
{
  return VMAN_ALIGN;
}

Res VMCreate(Space *spaceReturn, Size size)
{
  Space space;
  VM vm;

  AVER(size > 0);
  AVER(SizeIsAligned(size, VMAN_ALIGN));
  
  space = (Space)malloc(sizeof(SpaceStruct));
  if(space == NULL)
    return ResMEMORY;
  vm = SpaceVM(space);

  /* Note that because we add VMAN_ALIGN rather than */
  /* VMAN_ALIGN-1 we are not in danger of overflowing */
  /* vm->limit even if malloc were peverse enough to give us */
  /* a block at the end of memory. */

  vm->block = malloc((Size)(size + VMAN_ALIGN));
  if(vm->block == NULL) {
    free(vm);
    return ResMEMORY;
  }

  vm->base  = AddrAlignUp((Addr)vm->block, VMAN_ALIGN);
  vm->limit = AddrAdd(vm->base, size);
  AVER(vm->limit < AddrAdd((Addr)vm->block, size + VMAN_ALIGN));

  memset((void *)vm->base, VM_JUNKBYTE, size);
  
  /* Lie about the reserved address space, to simulate real */
  /* virtual memory. */
  vm->reserved = size;
  vm->mapped = (Size)0;
  
  vm->sig = VMSig;

  AVERT(VM, vm);

  *spaceReturn = space;  
  return ResOK;
}

void VMDestroy(Space space)
{
  VM vm = SpaceVM(space);
  
  /* All vm areas should have been unmapped. */
  AVER(vm->mapped == (Size)0);
  AVER(vm->reserved == AddrOffset(vm->base, vm->limit));

  memset((void *)vm->base, VM_JUNKBYTE, AddrOffset(vm->base, vm->limit));
  free(vm->block);
  
  vm->sig = SigInvalid;
  free(space);
}

Addr (VMBase)(Space space)
{
  VM vm = SpaceVM(space);
  return vm->base;
}

Addr (VMLimit)(Space space)
{
  VM vm = SpaceVM(space);
  return vm->limit;
}

Res VMMap(Space space, Addr base, Addr limit)
{
  VM vm = SpaceVM(space);
  Size size;

  AVER(base != (Addr)0);
  AVER(vm->base <= base);
  AVER(base < limit);
  AVER(limit <= vm->limit);
  AVER(AddrIsAligned(base, VMAN_ALIGN));
  AVER(AddrIsAligned(limit, VMAN_ALIGN));
  
  size = AddrOffset(base, limit);
  memset((void *)base, (int)0, size);
  
  vm->mapped += size;

  return ResOK;
}

void VMUnmap(Space space, Addr base, Addr limit)
{
  VM vm = SpaceVM(space);
  Size size;

  AVER(base != (Addr)0);
  AVER(vm->base <= base);
  AVER(base < limit);
  AVER(limit <= vm->limit);
  AVER(AddrIsAligned(base, VMAN_ALIGN));
  AVER(AddrIsAligned(limit, VMAN_ALIGN));
  
  size = AddrOffset(base, limit);
  memset((void *)base, VM_JUNKBYTE, size);

  AVER(vm->mapped >= size);
  vm->mapped -= size;
}
