/* impl.c.arenavmx: STUB FOR ARENAVM
 *
 * $HopeName$
 * Copyright (C) 1999 Harlequin Group plc.  All rights reserved.
 *
 * .purpose: This file provides stub functions to take the place of VM
 * arena functions on platforms where it doesn't exist.  Calling a stub
 * function causes a run-time assertion.  This file will be included in
 * SW libraries to allow linking against these symbols (don't ask).
 */


#include "mpm.h"
#include "mpsavm.h"

SRCID(arenavmx, "$HopeName$");


mps_arena_class_t mps_arena_class_vm(void)
{
  NOTREACHED;
  return (mps_arena_class_t)NULL;
}

mps_arena_class_t mps_arena_class_vmnz(void)
{
  NOTREACHED;
  return (mps_arena_class_t)NULL;
}
