/* impl.c.ssan: ANSI STACK SCANNER
 *
 * $HopeName$
 * Copyright (C) 1996 Harlequin Limited.  All rights reserved.
 *
 * This module provides zero functionality.  It exists to feed the
 * linker (prevent linker errors).
 */

#include "mpmtypes.h"
#include "misc.h"
#include "ss.h"


SRCID(ssan, "$HopeName$");


Res StackScan(ScanState ss, Addr *stackBot)
{
  UNUSED(ss); UNUSED(stackBot);
  return ResUNIMPL;
}
