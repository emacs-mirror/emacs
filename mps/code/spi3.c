/*  impl.c.spi3: STACK PROBE
 *
 * $Id: spi3.c,v 1.2 2002/02/01 13:56:52 pekka Exp $
 * $HopeName: MMsrc!spi3.c(trunk.2) $
 * Copyright (c) 2001 Ravenbrook Limited.
 * Copyright (C) 2001 Global Graphics Software.
 *
 *  This function reads a location that is probeDepth words beyond
 *  the current stack pointer.  On intel platforms the stack grows
 *  downwards so this means reading from a location with a lesser address.
 */


#include "mpm.h"


void StackProbe(Size depth)
{
  __asm {
    mov  eax, depth
    neg  eax
    mov  eax, [esp+eax*4] /* do the actual probe */
  }
}
