/* impl.c.protan: ANSI MEMORY PROTECTION
 *
 * $HopeName: MMsrc!protan.c(trunk.2) $
 * Copyright (C) 1996,1997 Harlequin Group, all rights reserved.
 *
 * READERSHIP
 *
 * .readership: Any MPS developer
 *
 * DESIGN
 *
 * design.mps.protan
 *
 */

#include "mpm.h"

SRCID(protan, "$HopeName: MMsrc!protan.c(trunk.2) $");

void ProtSetup(void)
{
  NOOP;
}

void ProtSet(Addr base, Addr limit, AccessSet pm)
{
  AVER(base < limit);
  /* .improve.protset.check: There is nor AccessSetCheck, so we */
  /* don't check it. */
  UNUSED(pm);
  NOOP;
}

/* design.mps.protan.fun.sync */
void ProtSync(Space space)
{
  Bool synced;

  AVERT(Space, space);

  do {
    Seg seg;

    synced = TRUE;
    seg = SegFirst(space);
    while(seg != NULL) {
      if(seg->pm != AccessSetEMPTY) {   /* design.mps.protan.fun.sync.seg */
        ShieldEnter(space);
        PoolAccess(seg->pool, seg, seg->pm);
        ShieldLeave(space);
        synced = FALSE;
      }
      seg = SegNext(space, seg);
    }
  } while(!synced);
}

void ProtTramp(void **rReturn,
                void *(*f)(void *, size_t),
                void *p, size_t s)
{
  AVER(rReturn != NULL);
  AVER(FUNCHECK(f));
  /* Can't check p and s as they are interpreted by the client */

  *(rReturn) = (*(f))(p, s);
}
