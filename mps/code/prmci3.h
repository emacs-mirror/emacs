/* impl.h.prmci3: PROTECTION MUTATOR CONTEXT (Intel 386)
 *
 * $HopeName: $
 * Copyright (C) 1999 The Harlequin Group Limited.  All rights reserved.
 *
 * .readership: MPS developers.
 */

#ifndef prmci3_h
#define prmci3_h


#include "mpm.h"

typedef Word *MRef;                  /* pointer to a machine word */

MRef Prmci3AddressHoldingReg(MutatorFaultContext, unsigned int);

void Prmci3DecodeFaultContext(MRef *, Byte **, MutatorFaultContext);

void Prmci3StepOverIns(MutatorFaultContext, Size);

#endif /* prmci3_h */
