/* impl.c.prmci3w3: PROTECTION MUTATOR CONTEXT INTEL 386 (Win32)
 *
 * $HopeName: !prmci3w3.c(trunk.1) $
 * Copyright (C) 1999. Harlequin Group plc. All rights reserved.
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

#include "mpm.h"
#include "prmcw3.h"
#include "prmci3.h"


/* Return an address for a machine register given a context and a 
 * register number
 */
MRef Prmci3AddressHoldingReg(MutatorFaultContext context, 
                                    unsigned int regnum)
{
  PCONTEXT wincont;

  AVER(regnum <= 7);
  AVER(regnum >= 0);

  wincont = context->ep->ContextRecord;

  switch (regnum) {
  case 0: return (MRef)&wincont->Eax;
  case 1: return (MRef)&wincont->Ecx;
  case 2: return (MRef)&wincont->Edx;
  case 3: return (MRef)&wincont->Ebx;
  case 4: return (MRef)&wincont->Esp;
  case 5: return (MRef)&wincont->Ebp;
  case 6: return (MRef)&wincont->Esi;
  case 7: return (MRef)&wincont->Edi;
  }
  NOTREACHED;
}


void Prmci3DecodeFaultContext(MRef *faultmemReturn, 
                              Byte **insvecReturn, 
                              MutatorFaultContext context)
{
  LPEXCEPTION_RECORD er;

  er = context->ep->ExceptionRecord;

  /* Assert that this is an access violation. The computation of */
  /* faultmem depends on this. */
  AVER(er->ExceptionCode == EXCEPTION_ACCESS_VIOLATION);

  *faultmemReturn = (MRef)er->ExceptionInformation[1];
  *insvecReturn = (Byte*)context->ep->ContextRecord->Eip;
}

void Prmci3StepOverIns(MutatorFaultContext context, Size inslen)
{
  context->ep->ContextRecord->Eip += (DWORD)inslen;
}

