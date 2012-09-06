/* ssw3mv.c: STACK SCANNING FOR WIN32 WITH MICROSOFT C
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * This scans the stack and fixes the registers which may contain roots.
 * See <design/thread-manager/>.
 */

#include "mpm.h"
#include <setjmp.h>

SRCID(ssw3mv, "$Id$");


Res StackScan(ScanState ss, Addr *stackBot)
{
  jmp_buf jb;

  /* We rely on the fact that Microsoft C's setjmp stores the callee-save
     registers in the jmp_buf. */
  (void)setjmp(jb);

  /* These checks will just serve to warn us at compile-time if the
     setjmp.h header changes to indicate that the registers we want aren't
     saved any more. */
  AVER(sizeof(((_JUMP_BUFFER *)jb)->Edi) == sizeof(Addr));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->Esi) == sizeof(Addr));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->Ebx) == sizeof(Addr));

  AVER(offsetof(_JUMP_BUFFER, Edi) == offsetof(_JUMP_BUFFER, Ebx) + 4);
  AVER(offsetof(_JUMP_BUFFER, Esi) == offsetof(_JUMP_BUFFER, Ebx) + 8);

  return StackScanInner(ss, stackBot, (Addr *)&((_JUMP_BUFFER *)jb)->Ebx, 3);
}
