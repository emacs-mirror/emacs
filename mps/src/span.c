/* impl.c.span: ANSI STACK PROBE
 *
 * $HopeName: !span.c(trunk.2) $
 * Copyright (C) 1997 Harlequin Limited.  All rights reserved.
 *
 * PURPOSE
 *
 * .purpose: The purpose of the ANSI Stack Probe is to provide a
 * non-functional implementation of the Stack Probe interface.
 * Stack Probe has a function implementation on platforms where the
 * MPS takes some special action to avoid stack overflow.
 */

#include "mpm.h"

SRCID(span, "$HopeName: !span.c(trunk.2) $");


/* StackProbe -- probe above the stack to provoke early stack overflow */

void StackProbe(Word depth)
{
  AVER(depth == 0);
  NOOP;
}
