/* impl.c.prmci3li: PROTECTION MUTATOR CONTEXT INTEL 386 (Linux)
 *
 * $HopeName: $
 * Copyright (C) 1998, 1999. Harlequin Group plc. All rights reserved.
 *
 * READERSHIP
 *
 * .readership: Any MPS developer.
 *
 * PURPOSE
 *
 * .purpose: This module implements the part of the protection module
 * that decodes the MutatorFaultContext.  
 *
 * SOURCES
 *
 * .source.i486: Intel486 Microprocessor Family Programmer's 
 * Reference Manual
 *
 * ASSUMPTIONS
 *
 * .assume.regref: The resisters in the context can be modified by
 * storing into an MRef pointer.
 *
 */

/* prmcli.h will include mpm.h after defining open sesame magic */
#include "prmcli.h"
#include "prmci3.h"


/* Return an address for a machine register given a context and a 
 * register number
 */
MRef Prmci3AddressHoldingReg(MutatorFaultContext context, 
                                    unsigned int regnum)
{
  struct sigcontext *scp;

  AVER(regnum <= 7);
  AVER(regnum >= 0);

  scp = context->scp;

  /* .source.i486 */
  /* .assume.regref */
  switch (regnum) {
  case 0: return (MRef)&scp->eax;
  case 1: return (MRef)&scp->ecx;
  case 2: return (MRef)&scp->edx;
  case 3: return (MRef)&scp->ebx;
  case 4: return (MRef)&scp->esp;
  case 5: return (MRef)&scp->ebp;
  case 6: return (MRef)&scp->esi;
  case 7: return (MRef)&scp->edi;
  }
  NOTREACHED;
  return (MRef)NULL;  /* Keep compiler happy. */
}


void Prmci3DecodeFaultContext(MRef *faultmemReturn, 
                              Byte **insvecReturn, 
                              MutatorFaultContext context)
{
  struct sigcontext *scp;

  scp = context->scp;

  /* Assert that this is an access violation. The computation of */
  /* faultmem depends on this. */
  AVER(scp->trapno == 14);

  *faultmemReturn = (MRef)scp->cr2;
  *insvecReturn = (Byte*)scp->eip;
}

void Prmci3StepOverIns(MutatorFaultContext context, Size inslen)
{
  context->scp->eip += (unsigned long)inslen;
}

