/* impl.c.protan: ANSI MEMORY PROTECTION
 *
 * $HopeName: MMsrc!protan.c(MMdevel_restr.2) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 */

#include "mpm.h"

SRCID(protan, "$HopeName: MMsrc!protan.c(MMdevel_restr.2) $");

void ProtSetup(void)
{
  NOOP;
}

void ProtSet(Addr base, Addr limit, AccessSet pm)
{
  NOOP;
}

void ProtSync(Space space)
{
  Seg seg;
  Bool synced;

  AVERT(Space, space);

  do {
    synced = FALSE;
    seg = SegFirst(space);
    while(seg != NULL) {
      if(seg->pm != AccessSetEMPTY) {
        ShieldEnter(space);
        PoolAccess(seg->pool, seg, seg->pm);
        ShieldLeave(space);
        synced = TRUE;
      }
      seg = SegNext(space, seg);
    }
  } while(synced);
}

void ProtTramp(void **r_o,
                void *(*f)(void *p, size_t s),
                void *p, size_t s)
{
  *(r_o) = (*(f))(p, s);
}
