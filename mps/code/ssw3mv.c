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
  Addr *stackTop;

  /* We rely on the fact that Microsoft C's setjmp stores the callee-save
     registers in the jmp_buf. */
  (void)setjmp(jb);

  /* These checks will just serve to warn us at compile-time if the
     setjmp.h header changes to indicate that the registers we want aren't
     saved any more. */
#if defined(MPS_ARCH_I3)
  AVER(sizeof(((_JUMP_BUFFER *)jb)->Edi) == sizeof(Addr));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->Esi) == sizeof(Addr));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->Ebx) == sizeof(Addr));
#elif defined(MPS_ARCH_I6)
  AVER(sizeof(((_JUMP_BUFFER *)jb)->Rdi) == sizeof(Addr));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->Rsi) == sizeof(Addr));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->Rbp) == sizeof(Addr));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->R12) == sizeof(Addr));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->R13) == sizeof(Addr));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->R14) == sizeof(Addr));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->R15) == sizeof(Addr));
#else
#error "StackScan not verified for the target architecture"
#endif

  stackTop = (Addr *)jb;
  AVER(AddrIsAligned((Addr)stackTop, sizeof(Addr)));  /* .align */

  return TraceScanAreaTagged(ss, stackTop, stackBot);
}
