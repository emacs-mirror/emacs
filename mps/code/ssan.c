/* impl.c.ssan: ANSI STACK SCANNER
 *
 * $Id$
 * $HopeName: MMsrc!ssan.c(trunk.3) $
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 * This module provides zero functionality.  It exists to feed the
 * linker (prevent linker errors).
 */

#include "mpmtypes.h"
#include "misc.h"
#include "ss.h"


SRCID(ssan, "$Id$");


Res StackScan(ScanState ss, Addr *stackBot)
{
  UNUSED(ss); UNUSED(stackBot);
  return ResUNIMPL;
}
