/* impl.c.ssan: ANSI STACK SCANNER
 *
 * $HopeName: !ssan.c(trunk.2) $
 *
 * This module provides zero functionality.  It exists to feed the
 * linker (prevent linker errors).
 */

#include "mpm.h"
#include "ss.h"


Res StackScan(ScanState ss, Addr *stackBot)
{
  UNUSED(ss); UNUSED(stackBot);
  return ResUNIMPL;
}
