/* impl.c.pool: PROTOCOL IMPLEMENTATION
 *
 * $HopeName: !protocol.c(trunk.2) $
 * Copyright (C) 1998. Harlequin Group plc. All rights reserved.
 *
 * READERSHIP
 *
 * .readership: any MPS developer
 *
 * DESIGN
 *
 * .design: See design.mps.protocol
 *
 */

#include "mpm.h"


SRCID(protocol, "$HopeName: !protocol.c(trunk.2) $");


Bool ProtocolClassCheck(ProtocolClass class)
{
  CHECKS(ProtocolClass, class);
  CHECKS(ProtocolClass, class->superclass);
  CHECKL(FUNCHECK(class->coerceInst));
  CHECKL(FUNCHECK(class->coerceClass));
  return TRUE;
}

Bool ProtocolInstCheck(ProtocolInst inst)
{
  CHECKS(ProtocolInst, inst);
  CHECKL(ProtocolClassCheck(inst->class));
  return TRUE;
}


/* ProtocolIsSubclass
 *
 * A predicate for testing subclass relationships.
 * A protocol class is always a subclass of itself.
 * This is implemented via the coerceClass method 
 * proivided by each class.
 */

Bool ProtocolIsSubclass(ProtocolClass sub, ProtocolClass super)
{
  ProtocolClass coerced;

  AVERT(ProtocolClass, sub);
  AVERT(ProtocolClass, super);

  if (sub->coerceClass(&coerced, sub, super)) {
    AVERT(ProtocolClass, coerced);
    return TRUE;
  } else {
    return FALSE;
  }
}


/* ProtocolCoerceClassMethod
 *
 * This default method must be inherited by any subclass
 * which does not perform a multiple inheritance.
 */


static Bool ProtocolCoerceClass(ProtocolClass *coerceResult, 
                                ProtocolClass proClass,
                                ProtocolClass super)
{
  ProtocolClass p = proClass;
  ProtocolClass root = (ProtocolClass)EnsureProtocolClass();

  AVERT(ProtocolClass, proClass);
  AVERT(ProtocolClass, super);
  AVERT(ProtocolClass, root);

  while (p != super) {
    AVERT(ProtocolClass, p);
    if (p == root)
      return FALSE;
    p = p->superclass;
  }
  *coerceResult = proClass;
  return TRUE;
}


/* ProtocolCoerceInstMethod
 *
 * This default method must be inherited by any subclass
 * which does not perform a multiple inheritance.
 */

static Bool ProtocolCoerceInst(ProtocolInst *coerceResult, 
                               ProtocolInst proInst,
                               ProtocolClass super)
{
  ProtocolClass p = proInst->class;
  ProtocolClass root = (ProtocolClass)EnsureProtocolClass();

  AVERT(ProtocolInst, proInst);
  AVERT(ProtocolClass, super);
  AVERT(ProtocolClass, root);

  while (p != super) {
    AVERT(ProtocolClass, p);
    if (p == root)
      return FALSE;
    p = p->superclass;
  }
  *coerceResult = proInst;
  return TRUE;
}


/* The class definition for the root of the hierarchy */

DEFINE_CLASS(ProtocolClass, theClass) 
{
  theClass->sig = ProtocolClassSig;
  theClass->superclass = theClass;
  theClass->coerceInst = ProtocolCoerceInst;
  theClass->coerceClass = ProtocolCoerceClass;
}



