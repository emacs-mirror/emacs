/* impl.c.sslii3: LINUX/INTEL STACK SCANNING
 *
 * $HopeName: $
 * Copyright (C) 1999.  Harlequin Group plc.  All rights reserved.
 *
 *  This scans the stack and fixes the registers which may contain 
 *  roots.  See design.mps.thread-manager
 *
 *  The registers edi, esi, ebx are the registers defined to be preserved
 *  across function calls and therefore may contain roots.
 *  These are pushed on the stack for scanning.
 *
 * ASSUMPTIONS
 *
 * .align: The stack pointer is assumed to be aligned on a word
 * boundary.
 */


#include "mpm.h"

SRCID(sslii3, "$HopeName: $");

#define ASMV(x) __asm__ volatile (x)


Res StackScan(ScanState ss, Addr *stackBot)
{
  Addr *stackTop;
  Res res;

  ASMV("push %ebx");    /* these registers are the save registers  */
  ASMV("push %esi");    /* and so may contain roots.  They are pushed */
  ASMV("push %edi");    /* for scanning */
  ASMV("mov %%esp, %0" : "=r" (stackTop) :);    /* stack pointer */

  AVER(AddrIsAligned((Addr)stackTop, sizeof(Addr)));  /* .align */
  res = TraceScanArea(ss, stackTop, stackBot);

  ASMV("add $12, %esp");  /* pop 3 registers to restore the stack pointer */

  return res;
}
