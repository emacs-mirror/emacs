/* vm.c: VIRTUAL MEMORY IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2014 Ravenbrook Limited.  See end of file for license.
 *
 * This is the portable part of the virtual memory implementation.
 */

#include "mpm.h"
#include "vm.h"

SRCID(vm, "$Id$");


/* VMCheck -- check a VM structure */

Bool VMCheck(VM vm)
{
  CHECKS(VM, vm);
  CHECKL(vm->base != (Addr)0);
  CHECKL(vm->limit != (Addr)0);
  CHECKL(vm->base < vm->limit);
  CHECKL(ArenaGrainSizeCheck(vm->pageSize));
  CHECKL(AddrIsAligned(vm->base, vm->pageSize));
  CHECKL(AddrIsAligned(vm->limit, vm->pageSize));
  CHECKL(vm->block != NULL);
  CHECKL((Addr)vm->block <= vm->base);
  CHECKL(vm->mapped <= vm->reserved);
  return TRUE;
}


/* VMPageSize -- return the page size cached in the VM */

Size (VMPageSize)(VM vm)
{
  AVERT(VM, vm);

  return VMPageSize(vm);
}


/* VMBase -- return the base address of the memory reserved */

Addr (VMBase)(VM vm)
{
  AVERT(VM, vm);

  return VMBase(vm);
}


/* VMLimit -- return the limit address of the memory reserved */

Addr (VMLimit)(VM vm)
{
  AVERT(VM, vm);

  return VMLimit(vm);
}


/* VMReserved -- return the amount of address space reserved */

Size (VMReserved)(VM vm)
{
  AVERT(VM, vm);

  return VMReserved(vm);
}


/* VMMapped -- return the amount of memory actually mapped */

Size (VMMapped)(VM vm)
{
  AVERT(VM, vm);

  return VMMapped(vm);
}


/* VMCopy -- copy VM descriptor */

void VMCopy(VM dest, VM src)
{
  AVER(dest != NULL);
  AVERT(VM, src);

  (void)mps_lib_memcpy(dest, src, sizeof(VMStruct));
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
