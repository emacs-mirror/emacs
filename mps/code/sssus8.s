!  impl.s.sssus8
!
!                      STACK SCANNING
!
!  $Id$
!
!  Copyright (c) 2001 Ravenbrook Limited.
!
!  This scans the stack and the preserved integer registers.
!  See design.mps.thread-manager
!
!  This file is identical to impl.s.sssos8, except for the
!  leading underscore convention on C names.  They should be
!  changed in parallel.
!
!  The non-global registers are preserved into the stackframe
!  by the "ta 3" instruction.  This leaves the global registers.
!  According to the Sparc Architecture Manual:
!  %g1 is assumed to be volatile across procedure calls
!  %g2...%g4 are "reserved for use by application programmer"
!  %g5...%g7 are "nonvolatile and reserved for (as-yet-undefined)
!     use by the execution environment"
!  To be safe %g2 to %g7 are pushed onto the stack before scanning
!  it just in case.

.text
  .align 4
  .global _StackScan
_StackScan:               !(ss, stackBot)
  save %sp,-120,%sp       !23 required + 6 globals = 29 words, 8-aligned

  std %g6,[%fp-8]         !double stores
  std %g4,[%fp-16]
  std %g2,[%fp-24]
  ta 3                    !flushes register windows onto stack

  mov %i0,%o0             !ss
  sub %fp,24,%o1          !stackTop (base)
  call _TraceScanArea     !(stackTop,stackBot,trace,rank) returns e
  mov %i1,%o2          !ds!stackBot (limit)

  ret
  restore %g0,%o0,%o0  !ds!return e

! C. COPYRIGHT AND LICENSE
!
! Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
! All rights reserved.  This is an open source license.  Contact
! Ravenbrook for commercial licensing options.
! 
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are
! met:
! 
! 1. Redistributions of source code must retain the above copyright
! notice, this list of conditions and the following disclaimer.
! 
! 2. Redistributions in binary form must reproduce the above copyright
! notice, this list of conditions and the following disclaimer in the
! documentation and/or other materials provided with the distribution.
! 
! 3. Redistributions in any form must be accompanied by information on how
! to obtain complete source code for this software and any accompanying
! software that uses this software.  The source code must either be
! included in the distribution or be available for no more than the cost
! of distribution plus a nominal fee, and must be freely redistributable
! under reasonable conditions.  For an executable file, complete source
! code means the source code for all modules it contains. It does not
! include source code for modules or files that typically accompany the
! major components of the operating system on which the executable file
! runs.
! 
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
! IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
! TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
! PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
! COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
! NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
! USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
! ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
! THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
