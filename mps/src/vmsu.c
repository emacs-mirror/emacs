/* impl.c.vmsu: VIRTUAL MEMORY MAPPING FOR SUNOS 4
 *
 * $HopeName: MMsrc!vmsu.c(trunk.19) $
 * Copyright (C) 1998 Harlequin Limited.  All rights reserved.
 *
 * Design: design.mps.vm
 *
 * This is the implementation of the virtual memory mapping interface
 * (vm.h) for SunOS 4.
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
 *
 * TRANSGRESSIONS
 *
 * .fildes.name: VMStruct has two fields whose names violate our
 * naming conventions.  They are called none_fd and zero_fd to
 * emphasize that they are file descriptors and this fact is not
 * reflected in their type.
 */

#include "mpm.h"

#ifndef MPS_OS_SU
#error "vmsu.c is SunOS 4 specific, but MPS_OS_SU is not set"
#endif
#ifdef VM_RM
#error "vmsu.c compiled with VM_RM set"
#endif /* VM_RM */

#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/errno.h>

SRCID(vmsu, "$HopeName: MMsrc!vmsu.c(trunk.19) $");


/* Fix up unprototyped system calls.  */
/* Note that these are not fixed up by std.h because that only fixes */
/* up discrepancies with ANSI. */

extern int close(int fd);
extern int munmap(caddr_t addr, int len);
extern int getpagesize(void);


/* VMStruct -- virtual memory structure */

#define VMSig           ((Sig)0x519B3999) /* SIGnature VM */

/* The names of zero_fd and none_fd are transgressions, see .fildes.name */
typedef struct VMStruct {
  Sig sig;                      /* design.mps.sig */
  int zero_fd;                  /* fildes for mmap, see impl.c.vms{o,u} */
  int none_fd;                  /* fildes for mmap, see impl.c.vms{o,u} */
  Align align;                  /* page size */
  Addr base, limit;             /* boundaries of reserved space */
  Size reserved;                /* total reserved address space */
  Size mapped;                  /* total mapped memory */
} VMStruct;


/* VMAlign -- return the page size */

Align VMAlign(VM vm)
{
  return vm->align;
}


/* VMCheck -- check a VM structure */

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


/* VMCreate -- reserve some virtual address space, and create a VM structure */

Res VMCreate(VM *vmReturn, Size size)
{
  caddr_t addr;
  Align align;
  int zero_fd;
  int none_fd;
  VM vm;
  Res res;

  AVER(vmReturn != NULL);

  align = (Align)getpagesize();
  AVER(SizeIsP2(align));
  size = SizeAlignUp(size, align);
  if ((size == 0) || (size > (Size)INT_MAX)) /* see .assume.size */
    return ResRESOURCE;

  zero_fd = open("/dev/zero", O_RDONLY);
  if (zero_fd == -1)
    return ResFAIL;
  none_fd = open("/etc/passwd", O_RDONLY);
  if (none_fd == -1) {
    res = ResFAIL;
    goto failNoneOpen;
  }

  /* Map in a page to store the descriptor on. */
  addr = mmap((caddr_t)0, SizeAlignUp(sizeof(VMStruct), align),
              PROT_READ | PROT_WRITE, MAP_PRIVATE,
              zero_fd, (off_t)0);
  if (addr == (caddr_t)-1) {
    int e = errno;
    AVER(e == ENOMEM); /* .assume.mmap.err */
    res = (e == ENOMEM) ? ResMEMORY : ResFAIL;
    goto failVMMap;
  }
  vm = (VM)addr;

  vm->zero_fd = zero_fd;
  vm->none_fd = none_fd;
  vm->align = align;

  /* .map.reserve: See .assume.not-last. */
  addr = mmap((caddr_t)0, size, PROT_NONE, MAP_SHARED, none_fd,
              (off_t)0);
  if (addr == (caddr_t)-1) {
    int e = errno;
    AVER(e == ENOMEM); /* .assume.mmap.err */
    res = (e == ENOMEM) ? ResRESOURCE : ResFAIL;
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


/* VMDestroy -- destroy the VM structure and release the address space */

void VMDestroy(VM vm)
{
  int r;
  int zero_fd, none_fd;

  AVERT(VM, vm);
  AVER(vm->mapped == (Size)0);

  /* This appears to be pretty pointless, since the space descriptor */
  /* page is  about to vanish completely.  However, munmap might fail */
  /* for some reason, and this would ensure that it was still */
  /* discovered if sigs were being checked. */
  vm->sig = SigInvalid;

  zero_fd = vm->zero_fd; none_fd = vm->none_fd;
  r = munmap((caddr_t)vm->base, (int)AddrOffset(vm->base, vm->limit));
  AVER(r == 0);
  r = munmap((caddr_t)vm,
             (int)SizeAlignUp(sizeof(VMStruct), vm->align));
  AVER(r == 0);
  /* .close.fail: We ignore failure from close() as there's very */
  /* little we can do anyway. */
  (void)close(zero_fd);
  (void)close(none_fd);

  EVENT_P(VMDestroy, vm);
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

  if (mmap((caddr_t)base, (int)size,
           PROT_READ | PROT_WRITE | PROT_EXEC,
           MAP_PRIVATE | MAP_FIXED,
           vm->zero_fd, (off_t)0)
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

  /* Map /etc/passwd onto the area, allowing no access.  This */
  /* effectively depopulates the area from memory, but keeps */
  /* it "busy" as far as the OS is concerned, so that it will not */
  /* be re-used by other calls to mmap which do not specify */
  /* MAP_FIXED.  The offset is specified to mmap so that */
  /* the OS merges this mapping with .map.reserve. */
  size = AddrOffset(base, limit);
  addr = mmap((caddr_t)base, (int)size,
              PROT_NONE, MAP_SHARED | MAP_FIXED,
              vm->none_fd, (off_t)AddrOffset(vm->base, base));
  AVER(addr == (caddr_t)base);

  vm->mapped -= size;

  EVENT_PAA(VMUnmap, vm, base, limit);
}
