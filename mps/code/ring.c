/* impl.c.ring: RING IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 * .intro: This is a portable implementation of Rings.
 *
 * .purpose: Rings are used to manage potentially unbounded collections
 * of things.
 *
 * .sources: design.mps.ring,
 *   item 6 of mail.richard_brooksby.1996-03-25.16-02
 */

#include "ring.h"
#include "check.h"
#include "misc.h"


SRCID(ring, "$Id$");


/* RingCheck, RingCheckSingle -- check the validity of a ring node
 *
 * RingCheck performs a consistency check on the ring node.
 * RingCheckSingle performs the same check, but also checks that
 * the ring node is a singleton (design.mps.ring.def.singleton).
 */

Bool RingCheck(Ring ring)
{
  CHECKL(ring != NULL);
  CHECKL(ring->next != NULL);
  CHECKL(ring->next->prev == ring);
  CHECKL(ring->prev != NULL);
  CHECKL(ring->prev->next == ring);
  UNUSED(ring); /* impl.c.mpm.check.unused */
  return TRUE;
}

Bool RingCheckSingle(Ring ring)
{
  CHECKL(RingCheck(ring));
  CHECKL(ring->next == ring);
  CHECKL(ring->prev == ring);
  UNUSED(ring); /* impl.c.mpm.check.unused */
  return TRUE;
}

Bool RingIsSingle(Ring ring)
{
  AVERT(Ring, ring);
  return (ring->next == ring);
}


/* RingInit -- initialize a ring node
 */

void (RingInit)(Ring ring)
{
  RingInit(ring);                       /* impl.h.mpm.ring.init */
}


/* RingFinish -- finish a ring node
 */

void (RingFinish)(Ring ring)
{
  RingFinish(ring);                     /* impl.h.mpm.ring.finish */
}


/* RingAppend -- add a ring node to the end of a ring
 */

void (RingAppend)(Ring ring, Ring new)
{
  RingAppend(ring, new);                /* impl.h.mpm.ring.append */
}


/* RingInsert -- add a ring node to the start of a ring
 */

void (RingInsert)(Ring ring, Ring new)
{
  RingInsert(ring, new);                /* impl.h.mpm.ring.insert */
}


/* RingRemove -- remove a node from a ring
 */

void (RingRemove)(Ring old)
{
  RingRemove(old);                      /* impl.h.mpm.ring.remove */
}


/* RingNext -- get the next element of a ring
 */

Ring (RingNext)(Ring ring)
{
  return RingNext(ring);                /* impl.h.mpm.ring.next */
}


/* RING_ELT -- get the ring element structure
 *
 * RING_ELT has no function (as it does not have function-like
 * behaviour), and is defined in impl.h.mpm.ring.elt.
 */


/* RING_FOR -- ring iterator construct
 *
 * RING_FOR has no function (as it does not have function-like
 * behaviour), and is defined in impl.h.mpm.ring.for.
 */


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
