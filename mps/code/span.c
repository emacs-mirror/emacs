/* impl.c.span: ANSI STACK PROBE
 *
 * $Id$
 * $HopeName: MMsrc!span.c(trunk.3) $
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 * PURPOSE
 *
 * .purpose: The purpose of the ANSI Stack Probe is to provide a
 * non-functional implementation of the StackProbe interface.
 * StackProbe has a function implementation on platforms where the
 * MPS takes some special action to avoid stack overflow.
 */

#include "mpm.h"

SRCID(span, "$Id$");


/* StackProbe -- probe above the stack to provoke early stack overflow */

void StackProbe(Size depth)
{
  AVER(depth == 0);
  NOOP;
}
