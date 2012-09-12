/* ssw3mv.c: STACK SCANNING FOR WIN32 WITH MICROSOFT C
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * This scans the stack and fixes the registers which may contain roots.
 * See <design/thread-manager/>.  It's unlikely that the callee-save
 * registers contain mutator roots by the time this function is called, but
 * we can't be certain, so we must scan them anyway.
 *
 * REFERENCES
 *
 * "Overview of x64 Calling Conventions"; MSDN; Microsoft Corporation;
 * <http://msdn.microsoft.com/en-us/library/ms235286%28v=vs.100%29.aspx>.
 *
 * "Caller/Callee Saved Registers"; MSDN; Microsoft Corporation;
 * <http://msdn.microsoft.com/en-us/library/6t169e9c%28v=vs.100%29.aspx>.
 *
 * "Register Usage"; MSDN; Microsoft Corporation;
 * <http://msdn.microsoft.com/en-us/library/9z1stfyw%28v=vs.100%29.aspx>.
 *
 * "Calling conventions for different C++ compilers and operating systems";
 * Agner Fog; Copenhagen University College of Engineering; 2012-02-29;
 * <http://agner.org./optimize/calling_conventions.pdf>.
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
  AVER(sizeof(((_JUMP_BUFFER *)jb)->Rdi) == sizeof(Addr));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->Rsi) == sizeof(Addr));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->Rbp) == sizeof(Addr));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->R12) == sizeof(Addr));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->R13) == sizeof(Addr));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->R14) == sizeof(Addr));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->R15) == sizeof(Addr));

  /* The layout of the jmp_buf forces us to harmlessly scan Rsp as well. */
  AVER(offsetof(_JUMP_BUFFER, Rsp) == offsetof(_JUMP_BUFFER, Rbx) + 8);
  AVER(offsetof(_JUMP_BUFFER, Rbp) == offsetof(_JUMP_BUFFER, Rbx) + 16);
  AVER(offsetof(_JUMP_BUFFER, Rsi) == offsetof(_JUMP_BUFFER, Rbx) + 24);
  AVER(offsetof(_JUMP_BUFFER, Rdi) == offsetof(_JUMP_BUFFER, Rbx) + 32);
  AVER(offsetof(_JUMP_BUFFER, R12) == offsetof(_JUMP_BUFFER, Rbx) + 40);
  AVER(offsetof(_JUMP_BUFFER, R13) == offsetof(_JUMP_BUFFER, Rbx) + 48);
  AVER(offsetof(_JUMP_BUFFER, R14) == offsetof(_JUMP_BUFFER, Rbx) + 56);
  AVER(offsetof(_JUMP_BUFFER, R15) == offsetof(_JUMP_BUFFER, Rbx) + 64);

  return StackScanInner(ss, stackBot, (Addr *)&((_JUMP_BUFFER *)jb)->Rbx, 9);
}
