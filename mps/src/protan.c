/* impl.c.protan: ANSI MEMORY PROTECTION
 *
 * $HopeName: !protan.c(trunk.8) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
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

SRCID(protan, "$HopeName: !protan.c(trunk.8) $");

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
    if(SegFirst(&seg, space)) {
      Addr base;
      do {
	base = SegBase(seg);
	if(SegPM(seg) != AccessSetEMPTY) {   /* design.mps.protan.fun.sync.seg */
	  ShieldEnter(space);
	  TraceSegAccess(space, seg, SegPM(seg));
	  ShieldLeave(space);
	  synced = FALSE;
	}
      } while(SegNext(&seg, space, base));
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
