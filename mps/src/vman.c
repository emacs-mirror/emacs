/*  ==== PSUEDO-VIRTUAL MEMORY MAPPING FOR ANSI ====
 *
 *  $HopeName$
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This is a pretend implementation of the virtual memory mapping
 *  interface (vm.h) based on ANSI malloc.  Reserving memory simply
 *  malloc it, and mapping and unmapping splat the memory as an
 *  aid to debugging.
 */

#include "std.h"
#include "mpmconf.h"
#include "vm.h"

#include <stdlib.h>
#include <string.h>


static Bool reserved = FALSE;
static Addr resBase, resLimit;
static void *block;


Addr VMGrain(void)
{
  return(VMAN_GRAIN);
}


Error VMReserve(Addr *baseReturn, Addr *limitReturn, Addr size)
{
  AVER(IsAligned(VMAN_GRAIN, size));
  AVER(size != 0);
  AVER(!reserved);
  
  block = malloc(size + VMAN_GRAIN);
  if(block == NULL) return(ErrRESMEM);

  resBase  = AlignUp(VMAN_GRAIN, (Addr)block);
  resLimit = resBase + size;
  reserved = TRUE;
  
  memset((void *)resBase, VMAN_JUNKBYTE, (size_t)size);

  *baseReturn = resBase;
  *limitReturn = resLimit;
  return(ErrSUCCESS);
}


void VMRelease(Addr base, Addr limit)
{
  AVER(reserved);
  reserved = FALSE;
  free(block);
  return;
  (void)base;
  (void)limit;
}


Error VMMap(Addr base, Addr limit)
{
  AVER(reserved);
  AVER(base >= resBase);
  AVER(limit <= resLimit);
  AVER(base < limit);
  AVER(base != 0);
  AVER(IsAligned(VMAN_GRAIN, base));
  AVER(IsAligned(VMAN_GRAIN, limit));
  
  memset((void *)base, 0, (size_t)(limit - base));

  return(ErrSUCCESS);
}


void VMUnmap(Addr base, Addr limit)
{
  AVER(reserved);
  AVER(base >= resBase);
  AVER(limit <= resLimit);
  AVER(base < limit);
  AVER(base != 0);
  AVER(IsAligned(VMAN_GRAIN, base));
  AVER(IsAligned(VMAN_GRAIN, limit));
  
  memset((void *)base, VMAN_JUNKBYTE, (size_t)(limit - base));
}
