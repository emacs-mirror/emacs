/* ssw3i6pc.c: STACK SCANNING FOR WIN64 WITH PELLES C
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * This scans the stack and fixes the registers which may contain roots.
 * See <design/thread-manager/>.
 *
 * .assume.ms-compat: We rely on the fact that Pelles C's setjmp stores
 * the callee-save registers in the jmp_buf and is compatible with Microsoft
 * C.  The Pelles C 7.00 setjmp.h header has a comment "MS compatible".  See
 * also "Is Pelles C's jmp_buf compatible with Microsoft C's?"
 * <http://forum.pellesc.de/index.php?topic=5464>
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

SRCID(ssw3i6pc, "$Id$");


/* This definition isn't in the Pelles C headers, so we reproduce it here.
 * See .assume.ms-compat. */

typedef /* _CRT_ALIGN(16) */ struct _SETJMP_FLOAT128 {
    unsigned __int64 Part[2];
} SETJMP_FLOAT128;

typedef struct _JUMP_BUFFER {
    unsigned __int64 Frame;
    unsigned __int64 Rbx;
    unsigned __int64 Rsp;
    unsigned __int64 Rbp;
    unsigned __int64 Rsi;
    unsigned __int64 Rdi;
    unsigned __int64 R12;
    unsigned __int64 R13;
    unsigned __int64 R14;
    unsigned __int64 R15;
    unsigned __int64 Rip;
    unsigned __int64 Spare;
    
    SETJMP_FLOAT128 Xmm6;
    SETJMP_FLOAT128 Xmm7;
    SETJMP_FLOAT128 Xmm8;
    SETJMP_FLOAT128 Xmm9;
    SETJMP_FLOAT128 Xmm10;
    SETJMP_FLOAT128 Xmm11;
    SETJMP_FLOAT128 Xmm12;
    SETJMP_FLOAT128 Xmm13;
    SETJMP_FLOAT128 Xmm14;
    SETJMP_FLOAT128 Xmm15;
} _JUMP_BUFFER;


Res StackScan(ScanState ss, Word *stackBot, Word mask, Word pattern)
{
  jmp_buf jb;

  /* We rely on the fact that Pelles C's setjmp stores the callee-save
     registers in the jmp_buf. */
  (void)setjmp(jb);

  /* These checks, on the _JUMP_BUFFER defined above, are mainly here
   * to maintain similarity to the matching code on the MPS_BUILD_MV
   * version of this code. */
  AVER(sizeof(((_JUMP_BUFFER *)jb)->Rbx) == sizeof(Word));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->Rsp) == sizeof(Word));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->Rbp) == sizeof(Word));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->Rsi) == sizeof(Word));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->Rdi) == sizeof(Word));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->R12) == sizeof(Word));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->R13) == sizeof(Word));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->R14) == sizeof(Word));
  AVER(sizeof(((_JUMP_BUFFER *)jb)->R15) == sizeof(Word));

  /* The layout of the jmp_buf forces us to harmlessly scan Rsp as well. */
  AVER(offsetof(_JUMP_BUFFER, Rsp) == offsetof(_JUMP_BUFFER, Rbx) + 8);
  AVER(offsetof(_JUMP_BUFFER, Rbp) == offsetof(_JUMP_BUFFER, Rbx) + 16);
  AVER(offsetof(_JUMP_BUFFER, Rsi) == offsetof(_JUMP_BUFFER, Rbx) + 24);
  AVER(offsetof(_JUMP_BUFFER, Rdi) == offsetof(_JUMP_BUFFER, Rbx) + 32);
  AVER(offsetof(_JUMP_BUFFER, R12) == offsetof(_JUMP_BUFFER, Rbx) + 40);
  AVER(offsetof(_JUMP_BUFFER, R13) == offsetof(_JUMP_BUFFER, Rbx) + 48);
  AVER(offsetof(_JUMP_BUFFER, R14) == offsetof(_JUMP_BUFFER, Rbx) + 56);
  AVER(offsetof(_JUMP_BUFFER, R15) == offsetof(_JUMP_BUFFER, Rbx) + 64);

  return StackScanInner(ss, stackBot, (Word *)&((_JUMP_BUFFER *)jb)->Rbx, 9,
                        mask, pattern);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
