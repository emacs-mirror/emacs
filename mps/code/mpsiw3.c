/* impl.c.mpsint:
 *
 *       WIN32 MEMORY POOL SYSTEM INTERFACE LAYER EXTRAS
 *
 *  $HopeName: MMsrc!mpsiw3.c(trunk.3) $
 *
 *  Copyright (C) 1996, 1997 Harlequin Group, all rights reserved
 */

#include "mpm.h"
#include "mps.h"

#include "mpswin.h"

SRCID(mpsint, "$HopeName: MMsrc!mpsiw3.c(trunk.3) $");

/* This is defined in protnt.c */
extern LONG ProtSEHfilter(LPEXCEPTION_POINTERS info);

LONG mps_SEH_filter(LPEXCEPTION_POINTERS info,
                    void **hp_o, size_t *hs_o)
{
  UNUSED(hp_o);
  UNUSED(hs_o);
  return ProtSEHfilter(info);
}

void mps_SEH_handler(void *p, size_t s)
{
  UNUSED(p); UNUSED(s);
  NOTREACHED;
}
