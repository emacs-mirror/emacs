/* boot.c: BOOTSTRAP ALLOCATOR
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 *
 * .overview: A structure and protocols for allocating memory from a
 * given block.  Very simple, it basically just increments a pointer.
 *
 * .boot.c: The Bootstrap Allocator is used to allocate C structures
 * for use in the implementation, not client objects.  Therefore,
 * we use "C types" (void *, size_t) not "client types" (Addr, Size).
 */

#include "boot.h"
#include "mpm.h"

SRCID(boot, "$Id$");


#define BootBlockSig ((Sig)0x519B002B) /* SIGnature BOOT Block */


/* BootBlockCheck -- check a BootBlock structure */

Bool BootBlockCheck(BootBlock boot)
{
  CHECKS(BootBlock, boot);
  CHECKL(boot->base != NULL);
  CHECKL(boot->alloc != NULL);
  CHECKL(boot->limit != NULL);
  CHECKL(boot->base <= boot->alloc);
  CHECKL(boot->alloc <= boot->limit);
  CHECKL(boot->base < boot->limit);

  return TRUE;
}


/* BootBlockInit -- initialize a BootBlock
 *
 * boot: a pointer to the structure to be initialized
 *   (must have been allocated by the caller, probably on the stack).
 * base: a pointer to the base of the memory to be allocated from
 *   from (the memory need not be committed)
 * limit: a pointer to the limit of the memory to be allocated from
 */

Res BootBlockInit(BootBlockStruct *boot, void *base, void *limit)
{
  /* Can't check boot as we are supposed to be initializing it */
  AVER(boot != NULL);
  AVER(base != NULL);
  AVER(limit != NULL);
  AVER(base < limit);

  boot->base = base;
  boot->alloc = base;
  boot->limit = limit;
  boot->sig = BootBlockSig;

  AVERT(BootBlock, boot);
  return ResOK;
}


/* BootBlockFinish -- finish a BootBlock structure */

void BootBlockFinish(BootBlock boot)
{
  AVERT(BootBlock, boot);

  boot->base = boot->alloc = boot->limit = NULL;
  boot->sig = SigInvalid;
}


/* BootAllocated
 *
 * Returns the total amount allocated using this descriptor
 */
size_t BootAllocated(BootBlock boot)
{
  AVERT(BootBlock, boot);

  return PointerOffset(boot->base, boot->alloc);
}


/* BootAlloc -- allocate from BootBlock structure
 *
 * preturn: The returned pointer, see .boot.c.
 * boot: must have been initialized with BootBlockInit().
 * size: size of requested object, see .boot.c.
 * align: required alignment of object, see .boot.c.
 */

Res BootAlloc(void **pReturn, BootBlock boot, size_t size, size_t align)
{
  void *blockBase, *blockLimit;  /* base, limit of candidate block */

  AVER(pReturn != NULL);
  AVERT(BootBlock, boot);
  AVER(size > 0);
  AVERT(Align, (Align)align);

  /* Align alloc pointer up and bounds check. */
  blockBase = PointerAlignUp(boot->alloc, align);
  if(boot->limit <= blockBase || blockBase < boot->alloc) {
    return ResMEMORY;
  }
  blockLimit = PointerAdd(blockBase, size);
  /* Following checks that the ordering constraint holds: */
  /* boot->alloc <= blockBase < blockLimit <= boot->limit */
  /* (if it doesn't hold then something overallocated/wrapped round) */
  if(blockBase < boot->alloc ||
     blockLimit <= blockBase ||
     boot->limit < blockLimit) {
    return ResMEMORY;
  }

  /* Fits!  So allocate it */
  boot->alloc = blockLimit;
  *pReturn = blockBase;
  return ResOK;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
