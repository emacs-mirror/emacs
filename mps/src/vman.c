/* impl.c.vman: ANSI VM: MALLOC-BASED PSEUDO MEMORY MAPPING
 *
 * $HopeName: !vman.c(trunk.21) $
 * Copyright (C) 1997, 1998 The Harlequin Group Limited.  All rights reserved.
 */

#include "mpm.h"

#include <stdlib.h>     /* for malloc and free */
#include <string.h>     /* for memset */

SRCID(vman, "$HopeName: !vman.c(trunk.21) $");


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


Align VMAlign(VM vm)
{
  UNUSED(vm);
  return VMAN_ALIGN;
}


Res VMCreate(VM *vmReturn, Size size)
{
  VM vm;

  AVER(vmReturn != NULL);

  /* Note that because we add VMAN_ALIGN rather than */
  /* VMAN_ALIGN-1 we are not in danger of overflowing */
  /* vm->limit even if malloc were perverse enough to give us */
  /* a block at the end of memory. */
  size = SizeAlignUp(size, VMAN_ALIGN) + VMAN_ALIGN;
  if((size < VMAN_ALIGN) || (size > (Size)(size_t)-1))
    return ResRESOURCE;

  vm = (VM)malloc(sizeof(VMStruct));
  if(vm == NULL)
    return ResMEMORY;

  vm->block = malloc((size_t)size);
  if(vm->block == NULL) {
    free(vm);
    return ResMEMORY;
  }

  vm->base  = AddrAlignUp((Addr)vm->block, VMAN_ALIGN);
  vm->limit = AddrAdd(vm->base, size - VMAN_ALIGN);
  AVER(vm->limit < AddrAdd((Addr)vm->block, size));

  memset((void *)vm->block, VM_JUNKBYTE, size);
  
  /* Lie about the reserved address space, to simulate real */
  /* virtual memory. */
  vm->reserved = size - VMAN_ALIGN;
  vm->mapped = (Size)0;
  
  vm->sig = VMSig;

  AVERT(VM, vm);
  
  EVENT_PAA(VMCreate, vm, vm->base, vm->limit);

  *vmReturn = vm;
  return ResOK;
}


void VMDestroy(VM vm)
{
  /* All vm areas should have been unmapped. */
  AVER(vm->mapped == (Size)0);
  AVER(vm->reserved == AddrOffset(vm->base, vm->limit));

  memset((void *)vm->base, VM_JUNKBYTE, AddrOffset(vm->base, vm->limit));
  free(vm->block);
  
  vm->sig = SigInvalid;
  free(vm);
  
  EVENT_P(VMDestroy, vm);
}


Addr (VMBase)(VM vm)
{
  return vm->base;
}

Addr (VMLimit)(VM vm)
{
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

  EVENT_PAA(VMMap, vm, base, limit);

  return ResOK;
}


void VMUnmap(VM vm, Addr base, Addr limit)
{
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

  EVENT_PAA(VMUnmap, vm, base, limit);
}
