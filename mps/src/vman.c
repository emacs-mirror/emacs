/*  impl.c.vman  draft impl
 *
 *           MALLOC-BASED PSUEDO-VIRTUAL MEMORY MAPPING
 *
 *  $HopeName: MMsrc/!vman.c(trunk.1)$
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This is a pretend implementation of the virtual memory mapping
 *  interface (vm.h) based on ANSI malloc.
 *
 *  VMCreate simply mallocs a large block of memory to simulate the
 *  address space.  Map and Unmap splat the memory as an aid to 
 *  debugging.
 */

#include "std.h"
#include "mpmconf.h"
#include "vm.h"

#include <stdlib.h>
#include <string.h>


#ifdef DEBUG_SIGN
static SigStruct VMSigStruct;
#endif


typedef struct VMStruct
{
#ifdef DEBUG_SIGN
  Sig sig;
#endif
  Addr base, limit;	/* boundaries of malloc'd memory */
  void *block;		/* pointer to actual malloc'd block */
} VMStruct;


Addr VMGrain(void)
{
  return(VMAN_GRAIN);	/* see mpmconf.h */
}


#ifdef DEBUG_ASSERT

Bool VMIsValid(VM vm, ValidationType validParam)
{
  AVER(vm != NULL);
#ifdef DEBUG_SIGN
  AVER(ISVALIDNESTED(Sig, vm->sig));
  AVER(vm->sig == &VMSigStruct);
#endif
  AVER(vm->base != 0);
  AVER(vm->limit != 0);
  AVER(vm->base < vm->limit);
  AVER(IsAligned(VMAN_GRAIN, vm->base));
  AVER(IsAligned(VMAN_GRAIN, vm->limit));
  AVER(vm->block != NULL);
  return(TRUE);
}

#endif /* DEBUG_ASSERT */


Error VMCreate(VM *vmReturn, Addr size)
{
  VM vm;

  AVER(IsAligned(VMAN_GRAIN, size));
  AVER(size != 0);

  vm = (VM)malloc(sizeof(VMStruct));
  if(vm == NULL) return(ErrRESMEM);
    
  vm->block = malloc(size + VMAN_GRAIN);
  if(vm->block == NULL) {
    free(vm);
    return(ErrRESMEM);
  }

  vm->base  = AlignUp(VMAN_GRAIN, (Addr)vm->block);
  vm->limit = vm->base + size;

#ifdef DEBUG_SIGN
  SigInit(&VMSigStruct, "VM");
  vm->sig = &VMSigStruct;
#endif

  AVER(ISVALID(VM, vm));
  
  memset((void *)vm->base, VMAN_JUNKBYTE, (size_t)size);

  *vmReturn = vm;
  return(ErrSUCCESS);
}


void VMDestroy(VM vm)
{
  AVER(ISVALID(VM, vm));
  
  memset((void *)vm->base, VMAN_JUNKBYTE, (size_t)(vm->limit - vm->base));

#ifdef DEBUG_SIGN
  vm->sig = SigInvalid;
#endif

  free(vm->block);
  free(vm);
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
  AVER(ISVALID(VM, vm));
  AVER(base >= vm->base);
  AVER(limit <= vm->limit);
  AVER(base < limit);
  AVER(base != 0);
  AVER(IsAligned(VMAN_GRAIN, base));
  AVER(IsAligned(VMAN_GRAIN, limit));
  
  memset((void *)base, 0, (size_t)(limit - base));

  return(ErrSUCCESS);
}


void VMUnmap(VM vm, Addr base, Addr limit)
{
  AVER(ISVALID(VM, vm));
  AVER(base >= vm->base);
  AVER(limit <= vm->limit);
  AVER(base < limit);
  AVER(base != 0);
  AVER(IsAligned(VMAN_GRAIN, base));
  AVER(IsAligned(VMAN_GRAIN, limit));
  
  memset((void *)base, VMAN_JUNKBYTE, (size_t)(limit - base));
}
