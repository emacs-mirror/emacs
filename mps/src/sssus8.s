!  impl.s.sssusp
!
!                      STACK SCANNING
!
!  $HopeName: MMsrc/!thsusp.s(MMdevel_dsm_0.2)$
!
!  Copyright (C) 1995 Harlequin Group, all rights reserved
!
!  This scans the stack and the preserved integer registers.
!  See design.mps.thread-manager
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
_StackScan:               !(stackBot, trace, rank)
  save %sp,-120,%sp       !23 required + 6 globals = 29 words, 8-aligned

  std %g6,[%fp-8]         !double stores
  std %g4,[%fp-16]
  std %g2,[%fp-24]
  ta 3                    !flushes register windows onto stack

  sub %fp,24,%o0          !stackTop (base)
  mov %i0,%o1             !stackBot (limit)
  mov %i1,%o2             !trace
  call _TraceScanArea     !(stackTop,stackBot,trace,rank) returns e
  mov %i2,%o3  !delay-slot!rank

  ret
  restore %g0,%o0,%o0  !ds!return e
