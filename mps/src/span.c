/* impl.c.span: ANSI STACK PROBE
 *
 * $HopeName: MMsrc!span.c(trunk.2) $
 * Copyright (C) 1997 Harlequin Limited.  All rights reserved.
 *
 * PURPOSE
 *
 * .purpose: The purpose of the ANSI Stack Probe is to provide a
 * non-functional implementation of the StackProbe interface.
 * StackProbe has a function implementation on platforms where the
 * MPS takes some special action to avoid stack overflow.
 */

#include "mpm.h"

SRCID(span, "$HopeName: MMsrc!span.c(trunk.2) $");


/* StackProbe -- probe above the stack to provoke early stack overflow */

void StackProbe(Size depth)
{
  AVER(depth == 0);
  NOOP;
}
