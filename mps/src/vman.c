/*  impl.c.vman
 *
 *           MALLOC-BASED PSUEDO-VIRTUAL MEMORY MAPPING
 *
 *  $HopeName: MMsrc/!vman.c(trunk.3)$
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  Design: design.mps.vm
 *
 *  This is an implementation of the virtual memory mapping
 *  interface (vm.h) based on ANSI malloc.  It doesn't actually
 *  provide the mapping services described in the interface, but
 *  rather simulates then using malloc.  VMCreate simply mallocs
 *  a large block of memory to simulate the reserved address space.
 *  Map and Unmap overwrite the memory as an aid to debugging.
 *
 *  .grain: Since the memory manager needs a maximum granularity
 *  of about a page, this module restricts Map and Unmap artificially
 *  to the granularity defined as VMAN_GRAIN in mpmconf.h.
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
  void *block;		/* pointer to malloc'd block, for free() */
} VMStruct;


Addr VMGrain(void)
{
  return(VMAN_GRAIN);	/* see .grain */
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
  AVER(vm->block <= (void *)vm->base);
  return(TRUE);
}

#endif /* DEBUG_ASSERT */


Error VMCreate(VM *vmReturn, Addr size)
{
  VM vm;

  AVER(IsAligned(VMAN_GRAIN, size));
  AVER(size != 0);
  AVER(sizeof(Addr) == sizeof(size_t));	/* must conform */

  vm = (VM)malloc(sizeof(VMStruct));
  if(vm == NULL) return(ErrRESMEM);
    
  /* Note that because we add VMAN_GRAIN rather than VMAN_GRAIN-1 */
  /* we are not in danger of overflowing vm->limit even if malloc */
  /* were peverse enough to give us a block at the end of memory. */

  vm->block = malloc((size_t)(size + VMAN_GRAIN));
  if(vm->block == NULL) {
    free(vm);
    return(ErrRESMEM);
  }

  vm->base  = AlignUp(VMAN_GRAIN, (Addr)vm->block);
  vm->limit = vm->base + size;
  AVER(vm->limit < (Addr)vm->block + size + VMAN_GRAIN);

  memset((void *)vm->base, VMAN_JUNKBYTE, (size_t)size);

#ifdef DEBUG_SIGN
  SigInit(&VMSigStruct, "VM");
  vm->sig = &VMSigStruct;
#endif

  AVER(ISVALID(VM, vm));
  
  *vmReturn = vm;
  return(ErrSUCCESS);
}


void VMDestroy(VM vm)
{
  AVER(ISVALID(VM, vm));
  
#ifdef DEBUG_SIGN
  vm->sig = SigInvalid;
#endif

  memset((void *)vm->base, VMAN_JUNKBYTE, (size_t)(vm->limit - vm->base));

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
  AVER(vm->base <= base);
  AVER(base < limit);
  AVER(limit <= vm->limit);
  AVER(base != 0);
  AVER(IsAligned(VMAN_GRAIN, base));
  AVER(IsAligned(VMAN_GRAIN, limit));
  
  memset((void *)base, (int)0, (size_t)(limit - base));

  return(ErrSUCCESS);
}


void VMUnmap(VM vm, Addr base, Addr limit)
{
  AVER(ISVALID(VM, vm));
  AVER(vm->base <= base);
  AVER(base < limit);
  AVER(limit <= vm->limit);
  AVER(base != 0);
  AVER(IsAligned(VMAN_GRAIN, base));
  AVER(IsAligned(VMAN_GRAIN, limit));
  
  memset((void *)base, VMAN_JUNKBYTE, (size_t)(limit - base));
}
