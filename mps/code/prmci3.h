/* impl.h.prmci3: PROTECTION MUTATOR CONTEXT (Intel 386)
 *
 * $Id$
 * $HopeName: MMsrc!prmci3.h(trunk.1) $
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
