/* impl.c.vmso: VIRTUAL MEMORY MAPPING FOR SOLARIS 2.x
 *
 * $HopeName: MMsrc!vmso.c(trunk.3) $
 * Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 * Design: design.mps.vm
 *
 * This is the implementation of the virtual memory mapping interface
 * (vm.h) for Solaris 2.x
 *
 * mmap(2) is used to reserve address space by creating a mapping to
 * /etc/passwd with page access none.  mmap(2) is used to map pages
 * onto store by creating a copy-on-write mapping to /dev/zero.
 *
 * Experiments have shown that attempting to reserve address space
 * by mapping /dev/zero results in swap being reserved.  This
 * appears to be a bug, so we work round it by using /etc/passwd,
 * the only file we can think of which is pretty much guaranteed
 * to be around.
 *
 * .assume.not-last: The implementation of VMCreate assumes that
 *   mmap() will not choose a region which contains the last page
 *   in the address space, so that the limit of the mapped area
 *   is representable.
 *
 * .assume.size: The maximum size of the reserved address space
 *   is limited by the range of "int".  This will probably be half
 *   of the address space.
 *
 * .assume.mmap.err: ENOMEM is the only error we really expect to
 *   get from mmap.  The others are either caused by invalid params
 *   or features we don't use.  See mmap(2) for details.
 */

#include "mpm.h"

#ifndef MPS_OS_SO
#error "vmso.c is Solaris 2.x specific, but MPS_OS_SO is not set"
#endif

#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/errno.h>

SRCID(vmso, "$HopeName: MMsrc!vmso.c(trunk.3) $");


/* Fix up unprototyped system calls.  */
/* Note that these are not fixed up by std.h because that only fixes */
/* up discrepancies with ANSI. */

extern int close(int fd);
extern int munmap(caddr_t addr, size_t len);
extern int getpagesize(void);

#define SpaceVM(space)  (&(space)->arenaStruct.vmStruct)

Align VMAlign(void)
{
  Align align;

  align = (Align)getpagesize();
  AVER(SizeIsP2(align));

  return align;
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


Res VMCreate(Space *spaceReturn, Size size, Addr base)
{
  caddr_t addr;
  Align align;
  int zero_fd;
  int none_fd;
  Space space;
  VM vm;

  align = VMAlign();

  AVER(spaceReturn != NULL);
  AVER(SizeIsAligned(size, align));
  AVER(size != 0);
  AVER(size <= INT_MAX); /* see .assume.size */
  AVER(base == NULL);

  zero_fd = open("/dev/zero", O_RDONLY);
  if(zero_fd == -1)
    return ResFAIL;
  none_fd = open("/etc/passwd", O_RDONLY);
  if(none_fd == -1) {
    close(zero_fd);
    return ResFAIL;
  }

  /* Map in a page to store the descriptor on. */
  addr = mmap((caddr_t)0, SizeAlignUp(sizeof(SpaceStruct), align),
              PROT_READ | PROT_WRITE, MAP_PRIVATE,
              zero_fd, (off_t)0);
  if((int)addr == -1) {
    int e = errno;
    AVER(e == ENOMEM); /* .assume.mmap.err */
    close(none_fd);
    close(zero_fd);
    if(e == ENOMEM)
      return ResMEMORY;
    else
      return ResFAIL;
  }
  space = (Space)addr;
  vm = SpaceVM(space);

  vm->zero_fd = zero_fd;
  vm->none_fd = none_fd;
  vm->align = align;

  /* See .assume.not-last. */
  addr = mmap((caddr_t)0, size, PROT_NONE, MAP_SHARED, none_fd, (off_t)0);
  if((int)addr == -1) {
    int e = errno;
    AVER(e == ENOMEM); /* .assume.mmap.err */
    close(none_fd);
    close(zero_fd);
    if(e == ENOMEM)
      return ResRESOURCE;
    else
      return ResFAIL;
  }

  vm->base = (Addr)addr;
  vm->limit = AddrAdd(vm->base, size);
  vm->reserved = size;
  vm->mapped = (Size)0;

  vm->sig = VMSig;

  AVERT(VM, vm);

  *spaceReturn = space;
  return ResOK;
}


void VMDestroy(Space space)
{
  int r;
  VM vm = SpaceVM(space);

  AVERT(VM, vm);
  AVER(vm->mapped == (Size)0);

  /* This appears to be pretty pointless, since the space descriptor */
  /* page is  about to vanish completely.  However, munmap might fail */
  /* for some reason, and this would ensure that it was still */
  /* discovered if sigs were being checked. */
  vm->sig = SigInvalid;

  close(vm->zero_fd);
  close(vm->none_fd);
  r = munmap((caddr_t)vm->base, (int)AddrOffset(vm->base, vm->limit));
  AVER(r == 0);
  r = munmap((caddr_t)space,
             (int)SizeAlignUp(sizeof(SpaceStruct), vm->align));
  AVER(r == 0);
}


Addr VMBase(Space space)
{
  VM vm = SpaceVM(space);
  AVERT(VM, vm);
  return vm->base;
}

Addr VMLimit(Space space)
{
  VM vm = SpaceVM(space);
  AVERT(VM, vm);
  return vm->limit;
}


Size VMReserved(Space space)
{
  VM vm = SpaceVM(space);
  AVERT(VM, vm);
  return vm->reserved;
}

Size VMMapped(Space space)
{
  VM vm = SpaceVM(space);
  AVERT(VM, vm);
  return vm->mapped;
}


Res VMMap(Space space, Addr base, Addr limit)
{
  VM vm = SpaceVM(space);
  Size size;

  AVERT(VM, vm);
  AVER(sizeof(int) == sizeof(Addr));
  AVER(base < limit);
  AVER(base >= vm->base);
  AVER(limit <= vm->limit);
  AVER(AddrOffset(base, limit) <= INT_MAX);
  AVER(AddrIsAligned(base, vm->align));
  AVER(AddrIsAligned(limit, vm->align));

  /* Map /dev/zero onto the area with a copy-on-write policy.  This */
  /* effectively populates the area with zeroed memory. */

  size = AddrOffset(base, limit);

  if((int)mmap((caddr_t)base, (int)size,
               PROT_READ | PROT_WRITE | PROT_EXEC,
               MAP_PRIVATE | MAP_FIXED,
               vm->zero_fd, (off_t)0) == -1) {
    AVER(errno == ENOMEM); /* .assume.mmap.err */
    return ResMEMORY;
  }

  vm->mapped += size;

  return ResOK;
}


void VMUnmap(Space space, Addr base, Addr limit)
{
  VM vm = SpaceVM(space);
  Size size;
  caddr_t addr;

  AVERT(VM, vm);
  AVER(sizeof(int) == sizeof(Addr));
  AVER(base < limit);
  AVER(base >= vm->base);
  AVER(limit <= vm->limit);
  AVER(AddrIsAligned(base, vm->align));
  AVER(AddrIsAligned(limit, vm->align));

  /* Map /etc/passwd onto the area, allowing no access.  This */
  /* effectively depopulates the area from memory, but keeps */
  /* it "busy" as far as the OS is concerned, so that it will not */
  /* be re-used by other calls to mmap which do not specify MAP_FIXED. */

  size = AddrOffset(base, limit);

  addr = mmap((caddr_t)base, (int)size,
              PROT_NONE, MAP_SHARED, vm->none_fd, (off_t)0);
  AVER((int)addr != -1);

  vm->mapped -= size;
}

