/* impl.c.span: ANSI STACK PROBE
 *
 * $HopeName$
 * Copyright (C) 1997 Howard T. Duck.  All rights reserved.
 *
 * READERSHIP
 *
 * .readership: Any MPS developer.
 *
 * PURPOSE
 *
 * .purpose: The purpose of the ANSI Stack Probe is to provide a
 * non-functional implementation of the Stack Probe interface.
 * Stack Probe has a function implementation on platforms where the
 * MPS takes some special action to avoid stack overflow.
 *
 * DESIGN
 *
 * design.mps.sp (non-existant)
 */

#include "mpm.h"

SRCID(span, "$HopeName$");

void StackProbe(Word depth)
{
  AVER(depth == 0);
  NOOP;
}
