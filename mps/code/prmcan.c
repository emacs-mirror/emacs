/* impl.c.prmcan: PROTECTION MUTATOR CONTEXT (ANSI)
 *
 * $HopeName: MMsrc!prmcan.c(trunk.1) $
 * Copyright (C) 1998 Harlequin Limited.  All rights reserved.
 *
 * .design: See design.mps.prot for the generic design of the interface
 * which is implemented in this module including the contracts for the
 * functions.
 *
 * .purpose: This module implements the part of the protection module
 * that implements the MutatorFaultContext type.  In this ANSI version
 * none of the functions have a useful implementation.
 */

#include "mpm.h"

SRCID(prmcan, "$HopeName$");


/* ProtCanStepInstruction -- can the current instruction be single-stepped */

Bool ProtCanStepInstruction(MutatorFaultContext context)
{
  UNUSED(context);

  return FALSE;
}


/* ProtStepInstruction -- step over instruction by modifying context */

Res ProtStepInstruction(MutatorFaultContext context)
{
  UNUSED(context);

  return ResUNIMPL;
}
