/* impl.h.prmci3: PROTECTION MUTATOR CONTEXT (Intel 386)
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
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
