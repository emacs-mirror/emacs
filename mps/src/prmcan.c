/* impl.c.prmcan: PROTECTION MUTATOR CONTEXT (ANSI)
 *
 * $HopeName: !prmcan.c(trunk.1) $
 * Copyright (C) 1998. Harlequin Group plc. All rights reserved.
 *
 * READERSHIP
 *
 * .readership: Any MPS developer.
 *
 * DESIGN
 *
 * .design: See design.mps.prot for the generic design of the interface
 * which is implemented in this module including the contracts for the
 * functions.
 *
 * PURPOSE
 *
 * .purpose: This module implements the part of the protection module
 * that implements the MutatorFaultContext type.  In this ANSI version
 * none of the functions have a useful implementation.
 */

#include "mpm.h"

Bool ProtCanStepInstruction(MutatorFaultContext context)
{
  UNUSED(context);

  return FALSE;
}

Res ProtStepInstruction(MutatorFaultContext context)
{
  UNUSED(context);

  return ResUNIMPL;
}
