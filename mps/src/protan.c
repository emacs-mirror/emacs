/* impl.c.protan: ANSI MEMORY PROTECTION
 *
 * $HopeName: MMsrc!protan.c(trunk.8) $
 * Copyright (C) 1997 Harlequin Limited.  All rights reserved.
 *
 *
 * DESIGN
 *
 * design.mps.protan
 */

#include "mpm.h"

SRCID(protan, "$HopeName: MMsrc!protan.c(trunk.8) $");


/* ProtSetup -- global protection setup */

void ProtSetup(void)
{
  NOOP;
}


/* ProtSet -- set the protection for a page */

void ProtSet(Addr base, Addr limit, AccessSet pm)
{
  AVER(base < limit);
  /* .improve.protset.check: There is nor AccessSetCheck, so we */
  /* don't check it. */
  UNUSED(pm);
  NOOP;
}


/* ProtSync -- synchronize protection settings with hardware
 *
 * See design.mps.protan.fun.sync.
 */

void ProtSync(Arena arena)
{
  Bool synced;

  AVERT(Arena, arena);

  do {
    Seg seg;

    synced = TRUE;
    if (SegFirst(&seg, arena)) {
      Addr base;
      do {
	base = SegBase(seg);
	if (SegPM(seg) != AccessSetEMPTY) { /* design.mps.protan.fun.sync.seg */
	  ShieldEnter(arena);
	  TraceSegAccess(arena, seg, SegPM(seg));
	  ShieldLeave(arena);
	  synced = FALSE;
	}
      } while(SegNext(&seg, arena, base));
    }
  } while(!synced);
}


/* ProtTramp -- protection trampoline */

void ProtTramp(void **rReturn, void *(*f)(void *, size_t),
               void *p, size_t s)
{
  AVER(rReturn != NULL);
  AVER(FUNCHECK(f));
  /* Can't check p and s as they are interpreted by the client */

  *(rReturn) = (*(f))(p, s);
}
