/* impl.h.boot: BOOTSTRAP ALLOCATOR INTERFACE
 *
 * $HopeName: MMsrc!boot.h(MMdevel_pekka_locus.2) $
 * Copyright (C) 2000 Harlequin Limited.  All rights reserved.
 *
 * .overview:  A protocol for allocating memory from a given block.
 */

#ifndef boot_h
#define boot_h

#include "mpmtypes.h"


/* BootBlockStruct -- descriptor of the block to allocate from */

typedef struct BootBlockStruct
{
  Sig sig;
  void *base;
  void *alloc;
  void *limit;
} BootBlockStruct;


extern Res BootBlockInit(BootBlockStruct *boot, void *base, void *limit);
extern void BootBlockFinish(BootBlock boot);
extern Res BootAlloc(void **pReturn, BootBlock boot, size_t size,
                     size_t align);
extern size_t BootAllocated(BootBlock boot);
extern Bool BootBlockCheck(BootBlock boot);


#endif /* boot_h */
