/* impl.c.vmxc: VIRTUAL MEMORY MAPPING FOR MacOS X
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .design: <design/vm/>
 *
 * .details: mmap(2) is used to reserve address space by creating a
 * mapping to the swap with page access none.  mmap(2) is used to map
 * pages onto store by creating a copy-on-write mapping to swap.
 *
 * .assume.not-last: The implementation of VMCreate assumes that mmap()
 * will not choose a region which contains the last page in the address
 * space, so that the limit of the mapped area is representable.
 *
 * .assume.mmap.err: ENOMEM is the only error we really expect to get
 * from mmap.  The others are either caused by invalid params or
 * features we don't use.  See mmap(2) for details.
 *
 * .overcommit: Apparently, MacOS X will overcommit, instead of
 * returning ENOMEM from mmap.  There appears to be no way to tell
 * whether the process is running out of swap and no way to reserve the
 * swap, apart from actually touching every page.
 */

#include "mpm.h"

#ifndef MPS_OS_XC
#error "vmxc.c is MacOS X specific, but MPS_OS_XC is not set"
#endif
#ifdef VM_RM
#error "vmxc.c compiled with VM_RM set"
#endif /* VM_RM */

#include <sys/types.h>
#include <sys/mman.h>
#include <errno.h>
#include <unistd.h>
#include <limits.h> /* for INT_MAX */

SRCID(vmxc, "$Id$");


/* VMStruct -- virtual memory structure */

#define VMSig           ((Sig)0x519B3999) /* SIGnature VM */

typedef struct VMStruct {
  Sig sig;                      /* <design/sig/> */
  Align align;                  /* page size */
  Addr base, limit;             /* boundaries of reserved space */
  Size reserved;                /* total reserved address space */
  Size mapped;                  /* total mapped memory */
} VMStruct;


/* VMAlign -- return the page size */

Align VMAlign(VM vm)
{
  AVERT(VM, vm);
  return vm->align;
}


/* VMCheck -- check a VM structure */

Bool VMCheck(VM vm)
{
  CHECKS(VM, vm);
  CHECKL(vm->base != 0);
  CHECKL(vm->limit != 0);
  CHECKL(vm->base < vm->limit);
  CHECKL(vm->mapped <= vm->reserved);
  CHECKL(SizeIsP2(vm->align));
  CHECKL(AddrIsAligned(vm->base, vm->align));
  CHECKL(AddrIsAligned(vm->limit, vm->align));
  return TRUE;
}


/* VMCreate -- reserve some virtual address space, and create a VM structure */

Res VMCreate(VM *vmReturn, Size size)
{
  caddr_t addr;
  Align align;
  VM vm;

  AVER(vmReturn != NULL);

  align = (Align)getpagesize();
  AVER(SizeIsP2(align));
  size = SizeAlignUp(size, align);
  if(size == 0)
    return ResRESOURCE;

  /* Map in a page to store the descriptor on. */
  AVER(sizeof(caddr_t) == sizeof(Addr)); /* verify address spaces match */
  addr = mmap((caddr_t)0, SizeAlignUp(sizeof(VMStruct), align),
              PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, (off_t)0);
  if(addr == (caddr_t)-1) {
    int e = errno;
    AVER(e == ENOMEM); /* .assume.mmap.err */
    if(e == ENOMEM)
      return ResMEMORY;
    else
      return ResFAIL;
  }
  vm = (VM)addr;

  vm->align = align;

  addr = mmap((caddr_t)0, size, PROT_NONE, MAP_SHARED | MAP_ANON, -1, (off_t)0);
  if(addr == (caddr_t)-1) {
    int e = errno;
    AVER(e == ENOMEM); /* .assume.mmap.err */
    if(e == ENOMEM)
      return ResRESOURCE;
    else
      return ResFAIL;
  }

  vm->base = (Addr)addr;
  vm->limit = AddrAdd(vm->base, size); /* .assume.not-last. */
  vm->reserved = size;
  vm->mapped = (Size)0;

  vm->sig = VMSig;

  AVERT(VM, vm);

  EVENT_PAA(VMCreate, vm, vm->base, vm->limit);

  *vmReturn = vm;
  return ResOK;
}


/* VMDestroy -- destroy the VM structure and release the address space */

void VMDestroy(VM vm)
{
  int r;

  AVERT(VM, vm);
  AVER(vm->mapped == (Size)0);

  /* This appears to be pretty pointless, since the space descriptor */
  /* page is  about to vanish completely.  However, munmap might fail */
  /* for some reason, and this would ensure that it was still */
  /* discovered if sigs were being checked. */
  vm->sig = SigInvalid;

  r = munmap((caddr_t)vm->base, (int)AddrOffset(vm->base, vm->limit));
  AVER(r == 0);
  r = munmap((caddr_t)vm,
             (int)SizeAlignUp(sizeof(VMStruct), vm->align));
  AVER(r == 0);
}


/* VMBase, VMLimit -- return the base & limit of the memory reserved */

Addr VMBase(VM vm)
{
  AVERT(VM, vm);
  return vm->base;
}

Addr VMLimit(VM vm)
{
  AVERT(VM, vm);
  return vm->limit;
}


/* VMReserved -- return the amount of the memory reserved */

Size VMReserved(VM vm)
{
  AVERT(VM, vm);
  return vm->reserved;
}


/* VMMapped -- return the amount of the memory committed */

Size VMMapped(VM vm)
{
  AVERT(VM, vm);
  return vm->mapped;
}


/* VMMap -- commit memory between base & limit */

Res VMMap(VM vm, Addr base, Addr limit)
{
  Size size;

  AVERT(VM, vm);
  AVER(base < limit);
  AVER(base >= vm->base);
  AVER(limit <= vm->limit);
  AVER(AddrOffset(base, limit) <= INT_MAX);
  AVER(AddrIsAligned(base, vm->align));
  AVER(AddrIsAligned(limit, vm->align));

  size = AddrOffset(base, limit);

  if(mmap((caddr_t)base, (int)size,
	  PROT_READ | PROT_WRITE | PROT_EXEC,
	  MAP_PRIVATE | MAP_FIXED | MAP_ANON,
	  -1, (off_t)0)
     == (caddr_t)-1) {
    AVER(errno == ENOMEM); /* .assume.mmap.err */
    return ResMEMORY;
  }

  vm->mapped += size;

  EVENT_PAA(VMMap, vm, base, limit);
  return ResOK;
}


/* VMUnmap -- decommit memory between base & limit */

void VMUnmap(VM vm, Addr base, Addr limit)
{
  Size size;
  caddr_t addr;

  AVERT(VM, vm);
  AVER(sizeof(int) == sizeof(Addr));
  AVER(base < limit);
  AVER(base >= vm->base);
  AVER(limit <= vm->limit);
  AVER(AddrIsAligned(base, vm->align));
  AVER(AddrIsAligned(limit, vm->align));

  /* .unmap: Map with MAP_ANON, allowing no access.  This */
  /* effectively depopulates the area from memory, but keeps */
  /* it "busy" as far as the OS is concerned, so that it will not */
  /* be re-used by other calls to mmap which do not specify */
  /* MAP_FIXED.  The offset is specified to mmap so that */
  /* the OS can merge this mapping with .map.reserve. */
  size = AddrOffset(base, limit);
  addr = mmap((caddr_t)base, (int)size,
              PROT_NONE, MAP_SHARED | MAP_FIXED | MAP_ANON,
              -1, (off_t)AddrOffset(vm->base, base));
  AVER(addr == (caddr_t)base);

  vm->mapped -= size;

  EVENT_PAA(VMUnmap, vm, base, limit);
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
