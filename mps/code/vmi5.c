/* impl.c.vmi5: VIRTUAL MEMORY MAPPING FOR IRIX 5 (AND 6)
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 * Design: design.mps.vm
 *
 * This is the implementation of the virtual memory mapping interface
 * (vm.h) for IRIX 5.x.
 *
 * mmap(2) is used to reserve address space by creating a mapping to
 * /dev/zero with page access none.  mmap(2) is used to map pages
 * onto store by creating a copy-on-write mapping to /dev/zero.
 *
 * .assume.not-last: The implementation of VMCreate assumes that
 * mmap() will not choose a region which contains the last page
 * in the address space, so that the limit of the mapped area
 * is representable.
 *
 * .assume.mmap.err: EAGAIN is the only error we really expect to get
 * from mmap when committing and ENOMEM when reserving or committing (we
 * have actually observed ENOMEM when committing).  The others are
 * either caused by invalid params or features we don't use.  See
 * mmap(2) for details.
 *
 * TRANSGRESSIONS
 *
 * .fildes.name: VMStruct has one fields whose name violates our naming
 * conventions.  It's called zero_fd to emphasize that it's a file
 * descriptor and this fact is not reflected in the type.
 */

#include "mpm.h"

#if !defined(MPS_OS_I5) && !defined(MPS_OS_IA)
#error "vmi5.c is IRIX-specific, but MPS_OS_I5 or MPS_OS_IA is not set"
#endif

#define _POSIX_SOURCE
#define _POSIX_C_SOURCE 199309L

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>

/* No constant for the mmap error return on IRIX 5, so define one. */
#if !defined(MAP_FAILED) && defined(MPS_OS_I5)
#define MAP_FAILED ((void *)-1)
#endif

SRCID(vmi5, "$Id$");


/* VMStruct -- virtual memory structure */

#define VMSig           ((Sig)0x519B3999) /* SIGnature VM */

/* The name zero_fd is a transgression, see .fildes.name. */
typedef struct VMStruct {
  Sig sig;                      /* design.mps.sig */
  int zero_fd;                  /* fildes for mmap */
  Align align;                  /* page size */
  Addr base, limit;             /* boundaries of reserved space */
  Size reserved;                /* total reserved address space */
  Size mapped;                  /* total mapped memory */
} VMStruct;


Align VMAlign(VM vm)
{
  AVERT(VM, vm);
  return vm->align;
}


Bool VMCheck(VM vm)
{
  CHECKS(VM, vm);
  CHECKL(vm->zero_fd >= 0);
  CHECKL(vm->base != 0);
  CHECKL(vm->limit != 0);
  CHECKL(vm->base < vm->limit);
  CHECKL(vm->mapped <= vm->reserved);
  CHECKL(SizeIsP2(vm->align));
  CHECKL(AddrIsAligned(vm->base, vm->align));
  CHECKL(AddrIsAligned(vm->limit, vm->align));
  return TRUE;
}


Res VMCreate(VM *vmReturn, Size size)
{
  void *addr;
  Align align;
  int zero_fd;
  VM vm;
  Res res;

  AVER(vmReturn != NULL);

  align = (Align)sysconf(_SC_PAGESIZE);
  AVER(SizeIsP2(align));
  size = SizeAlignUp(size, align);
  if((size == 0) || (size > (Size)(size_t)-1))
    return ResRESOURCE;

  zero_fd = open("/dev/zero", O_RDONLY);
  if(zero_fd == -1)
    return ResFAIL;

  /* Map in a page to store the descriptor on. */
  addr = mmap((void *)0, (size_t)SizeAlignUp(sizeof(VMStruct), align),
              PROT_READ | PROT_WRITE, MAP_PRIVATE,
              zero_fd, (off_t)0);
  if(addr == MAP_FAILED) {
    AVER(errno == ENOMEM || errno == EAGAIN); /* .assume.mmap.err */
    res = (errno == ENOMEM || errno == EAGAIN) ? ResMEMORY : ResFAIL;
    goto failVMMap;
  }
  vm = (VM)addr;

  vm->zero_fd = zero_fd;
  vm->align = align;

  /* .map.reserve: MAP_AUTORESRV is necessary to avoid reserving swap. */
  addr = mmap((void *)0, (size_t)size, PROT_NONE, MAP_SHARED | MAP_AUTORESRV,
	      zero_fd, (off_t)0);
  if(addr == MAP_FAILED) {
    AVER(errno == ENOMEM); /* .assume.mmap.err */
    res = (errno == ENOMEM) ? ResRESOURCE : ResFAIL;
    goto failReserve;
  }

  vm->base = (Addr)addr;
  vm->limit = AddrAdd(vm->base, size);
  vm->reserved = size;
  vm->mapped = (Size)0;

  vm->sig = VMSig;

  AVERT(VM, vm);

  EVENT_PAA(VMCreate, vm, vm->base, vm->limit);

  *vmReturn = vm;
  return ResOK;

failReserve:
  (void)munmap((void *)vm, (size_t)SizeAlignUp(sizeof(VMStruct), align));
failVMMap:
  (void)close(zero_fd);
  return res;
}


void VMDestroy(VM vm)
{
  int r;
  int zero_fd;

  AVERT(VM, vm);
  AVER(vm->mapped == (Size)0);

  /* This appears to be pretty pointless, since the descriptor */
  /* page is about to vanish completely.  However, munmap might fail */
  /* for some reason, and this would ensure that it was still */
  /* discovered if sigs were being checked. */
  vm->sig = SigInvalid;

  zero_fd = vm->zero_fd;
  r = munmap((void *)vm->base, (size_t)AddrOffset(vm->base, vm->limit));
  AVER(r == 0);
  r = munmap((void *)vm, (size_t)SizeAlignUp(sizeof(VMStruct), vm->align));
  AVER(r == 0);
  r = close(zero_fd);
  AVER(r == 0);

  EVENT_P(VMDestroy, vm);
}


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


Size VMReserved(VM vm)
{
  AVERT(VM, vm);
  return vm->reserved;
}

Size VMMapped(VM vm)
{
  AVERT(VM, vm);
  return vm->mapped;
}


Res VMMap(VM vm, Addr base, Addr limit)
{
  Size size;
  void *addr;

  AVERT(VM, vm);
  AVER(base < limit);
  AVER(base >= vm->base);

  AVER(limit <= vm->limit);
  AVER(AddrIsAligned(base, vm->align));
  AVER(AddrIsAligned(limit, vm->align));

  /* Map /dev/zero onto the area with a copy-on-write policy.  This */
  /* effectively populates the area with zeroed memory. */
  size = AddrOffset(base, limit);
  /* Check it won't lose any bits. */
  AVER(size <= (Size)(size_t)-1);
  addr = mmap((void *)base, (size_t)size,
	      PROT_READ | PROT_WRITE | PROT_EXEC,
	      MAP_PRIVATE | MAP_FIXED,
	      vm->zero_fd, (off_t)0);
  if(addr == MAP_FAILED) {
    AVER(errno == ENOMEM || errno == EAGAIN); /* .assume.mmap.err */
    return ResMEMORY;
  }
  AVER(addr == (void *)base);

  vm->mapped += size;

  EVENT_PAA(VMMap, vm, base, limit);
  return ResOK;
}


void VMUnmap(VM vm, Addr base, Addr limit)
{
  Size size;
  void *addr;

  AVERT(VM, vm);
  AVER(base < limit);
  AVER(base >= vm->base);
  AVER(limit <= vm->limit);
  AVER(AddrIsAligned(base, vm->align));
  AVER(AddrIsAligned(limit, vm->align));

  /* .unmap.reserve: Map /dev/zero onto the area, allowing no access. */
  /* This effectively depopulates the area from memory, but keeps */
  /* it "busy" as far as the OS is concerned, so that it will not */
  /* be re-used by other calls to mmap which do not specify */
  /* MAP_FIXED.  See also .map.reserve. */
  /* The OS doesn't merge this mapping with any neighbours, but it */
  /* can keep track of at least 16K mappings, so it's good enough. */
  size = AddrOffset(base, limit);
  /* Check it won't lose any bits. */
  AVER(size <= (Size)(size_t)-1);
  addr = mmap((void *)base, (size_t)size,
              PROT_NONE, MAP_SHARED | MAP_FIXED | MAP_AUTORESRV,
              vm->zero_fd, (off_t)AddrOffset(vm->base, base));
  AVER(addr == (void *)base);

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
