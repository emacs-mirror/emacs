!  impl.s.sssos8
!
!                      STACK SCANNING
!
!  $HopeName: MMsrc!sssos9sc.S(trunk.1) $
!
!  Copyright (C) 1995 Harlequin Group, all rights reserved
!
!  This scans the stack and the preserved integer registers.
!  See design.mps.thread-manager
!
!  This file is identical to impl.s.sssus8, except for the
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
  .global StackScan
StackScan:                !(ss, stackBot)
  save %sp,-120,%sp       !23 required + 6 globals = 29 words, 8-aligned

  std %g6,[%fp-8]         !double stores
  std %g4,[%fp-16]
  std %g2,[%fp-24]
  ta 3                    !flushes register windows onto stack

  mov %i0,%o0             !ss
  sub %fp,24,%o1          !stackTop (base)
  call TraceScanArea      !(stackTop,stackBot,trace,rank) returns e
  mov %i1,%o2          !ds!stackBot (limit)

  ret
  restore %g0,%o0,%o0  !ds!return e
