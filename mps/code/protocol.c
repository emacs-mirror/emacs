/* impl.c.pool: PROTOCOL IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 * DESIGN
 *
 * .design: See design.mps.protocol
 */

#include "mpm.h"


SRCID(protocol, "$Id$");


/* ProtocolClassCheck -- check a protocol class */

Bool ProtocolClassCheck(ProtocolClass class)
{
  CHECKS(ProtocolClass, class);
  CHECKS(ProtocolClass, class->superclass);
  CHECKL(FUNCHECK(class->coerceInst));
  CHECKL(FUNCHECK(class->coerceClass));
  return TRUE;
}


/* ProtocolInstCheck -- check a protocol instance */

Bool ProtocolInstCheck(ProtocolInst inst)
{
  CHECKS(ProtocolInst, inst);
  CHECKL(ProtocolClassCheck(inst->class));
  return TRUE;
}


/* ProtocolIsSubclass -- a predicate for testing subclass relationships
 *
 * A protocol class is always a subclass of itself.  This is implemented
 * via the coerceClass method provided by each class.
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


/* ProtocolCoerceClass -- the default method for coerceClass
 *
 * This default method must be inherited by any subclass
 * which does not perform a multiple inheritance.
 */
static Bool ProtocolCoerceClass(ProtocolClass *coerceResult,
                                ProtocolClass proClass,
                                ProtocolClass super)
{
  ProtocolClass p = proClass;
  ProtocolClass root = ProtocolClassGet();

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


/* ProtocolCoerceInst -- the default method for coerceInst
 *
 * This default method must be inherited by any subclass
 * which does not perform a multiple inheritance.
 */
static Bool ProtocolCoerceInst(ProtocolInst *coerceResult,
                               ProtocolInst proInst,
                               ProtocolClass super)
{
  ProtocolClass p = proInst->class;
  ProtocolClass root = ProtocolClassGet();

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





/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
