/* impl.c.vmso: VIRTUAL MEMORY MAPPING FOR SOLARIS 2.x
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * DESIGN
 *
 * .design: design.mps.vmso
 *
 * PURPOSE
 *
 * .purpose: This is the implementation of the virtual memory mapping
 * interface (vm.h) for Solaris 2.x.  It allows arenas (typically
 * arenavm is the only client of the interface) to reserve virtual
 * address space and to map ranges with RAM and unmap memory.
 *
 * ASSUMPTIONS
 *
 * .assume.not-last: The implementation of VMCreate assumes that mmap()
 * will not choose a region which contains the last page in the address
 * space, so that the limit of the mapped area is representable.
 * (VMCheck checks limit != 0 which is a roundabout way of checking
 * this.)
 *
 * .assume.mmap.err: EAGAIN is the only error we really expect to get
 * from mmap when committing and ENOMEM when reserving.  The others are
 * either caused by invalid params or features we don't use.  See
 * mmap(2) for details.
 *
 * TRANSGRESSIONS
 *
 * .fildes.name: VMStruct has two fields whose names violate our naming
 * conventions.  They are called none_fd and zero_fd to emphasize that
 * they are file descriptors and this fact is not reflected in their
 * type (we can't change their type as that is restricted by the
 * interface provided by Solaris).
 */

#include "mpm.h"

#ifndef MPS_OS_SO
#error "vmso.c is Solaris 2.x specific, but MPS_OS_SO is not set"
#endif

/* Open sesame magic */
#define _POSIX_SOURCE

#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/errno.h>
/* unistd for _SC_PAGESIZE */
#include <unistd.h>

SRCID(vmso, "$Id$");


/* Fix up unprototyped system calls.  */

extern int close(int fd);
extern int munmap(caddr_t addr, size_t len);


/* VMStruct -- virtual memory structure */

#define VMSig           ((Sig)0x519B3999) /* SIGnature VM */

/* The names of zero_fd and none_fd are transgressions, see .fildes.name */
typedef struct VMStruct {
  Sig sig;                      /* design.mps.sig */
  int zero_fd;                  /* fildes for mmap */
  int none_fd;                  /* fildes for mmap */
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
  CHECKL(vm->none_fd >= 0);
  CHECKL(vm->zero_fd != vm->none_fd);
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
  caddr_t addr;
  Align align;
  int zero_fd;
  int none_fd;
  VM vm;
  long pagesize;
  Res res;

  AVER(vmReturn != NULL);

  /* Find out the page size from the OS */
  pagesize = sysconf(_SC_PAGESIZE);
  /* check the actual returned pagesize will fit in an object of */
  /* type Align. */
  AVER(pagesize > 0);
  AVER((unsigned long)pagesize <= (unsigned long)(Align)-1);
  /* Note implicit conversion from "long" to "Align". */
  align = pagesize;
  AVER(SizeIsP2(align));
  size = SizeAlignUp(size, align);
  if((size == 0) || (size > (Size)(size_t)-1))
    return ResRESOURCE;

  zero_fd = open("/dev/zero", O_RDONLY);
  if(zero_fd == -1)
    return ResFAIL;
  none_fd = open("/etc/passwd", O_RDONLY);
  if(none_fd == -1) {
    res = ResFAIL;
    goto failNoneOpen;
  }

  /* Map in a page to store the descriptor on. */
  addr = mmap((caddr_t)0, (size_t)SizeAlignUp(sizeof(VMStruct), align),
              PROT_READ | PROT_WRITE, MAP_PRIVATE,
              zero_fd, (off_t)0);
  if(addr == MAP_FAILED) {
    AVER(errno == EAGAIN); /* .assume.mmap.err */
    res = ResMEMORY;
    goto failVMMap;
  }
  vm = (VM)addr;

  vm->zero_fd = zero_fd;
  vm->none_fd = none_fd;
  vm->align = align;

  /* .map.reserve: See .assume.not-last. */
  addr = mmap((caddr_t)0, (size_t)size, PROT_NONE, MAP_SHARED,
              none_fd, (off_t)0);
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
  (void)munmap((caddr_t)vm, (size_t)SizeAlignUp(sizeof(VMStruct), align));
failVMMap:
  (void)close(none_fd); /* see .close.fail */
failNoneOpen:
  (void)close(zero_fd);
  return res;
}


void VMDestroy(VM vm)
{
  int r;
  int zero_fd, none_fd;

  AVERT(VM, vm);
  AVER(vm->mapped == (Size)0);

  /* This appears to be pretty pointless, since the descriptor */
  /* page is about to vanish completely.  However, munmap might fail */
  /* for some reason, and this would ensure that it was still */
  /* discovered if sigs were being checked. */
  vm->sig = SigInvalid;

  zero_fd = vm->zero_fd; none_fd = vm->none_fd;
  r = munmap((caddr_t)vm->base, (size_t)AddrOffset(vm->base, vm->limit));
  AVER(r == 0);
  r = munmap((caddr_t)vm,
             (size_t)SizeAlignUp(sizeof(VMStruct), vm->align));
  AVER(r == 0);
  /* .close.fail: We ignore failure from close() as there's very */
  /* little we can do anyway. */
  (void)close(zero_fd);
  (void)close(none_fd);

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
  caddr_t addr;

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

  addr = mmap((caddr_t)base, (size_t)size,
              PROT_READ | PROT_WRITE | PROT_EXEC,
              MAP_PRIVATE | MAP_FIXED,
              vm->zero_fd, (off_t)0);
  if(addr == MAP_FAILED) {
    AVER(errno == EAGAIN); /* .assume.mmap.err */
    return ResMEMORY;
  }
  AVER(addr == (caddr_t)base);

  vm->mapped += size;

  EVENT_PAA(VMMap, vm, base, limit);
  return ResOK;
}


void VMUnmap(VM vm, Addr base, Addr limit)
{
  Size size;
  caddr_t addr;

  AVERT(VM, vm);
  AVER(base < limit);
  AVER(base >= vm->base);
  AVER(limit <= vm->limit);
  AVER(AddrIsAligned(base, vm->align));
  AVER(AddrIsAligned(limit, vm->align));

  /* Map /etc/passwd onto the area, allowing no access.  This */
  /* effectively depopulates the area from memory, but keeps */
  /* it "busy" as far as the OS is concerned, so that it will not */
  /* be re-used by other calls to mmap which do not specify */
  /* MAP_FIXED.  The offset is specified to mmap so that */
  /* the OS merges this mapping with .map.reserve. */
  size = AddrOffset(base, limit);
  /* Check it won't lose any bits. */
  AVER(size <= (Size)(size_t)-1);
  addr = mmap((caddr_t)base, (size_t)size,
              PROT_NONE, MAP_SHARED | MAP_FIXED,
              vm->none_fd, (off_t)AddrOffset(vm->base, base));
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
