/* impl.c.vmli: VIRTUAL MEMORY MAPPING FOR LINUX
 *
 * $HopeName: MMsrc!vmli.c(trunk.3) $
 * Copyright (C) 1995, 1997, 1998 Harlequin Group, all rights reserved
 *
 * Readership: Any MPS developer
 *
 * Design: design.mps.vm, design.mps.vmli
 *
 * Status: a bit hacky, but probably working.
 *
 * This is the implementation of the virtual memory mapping interface
 * (vm.h) for LINUX.  It was created by copying vmo1.c (the DIGITAL
 * UNIX implementation) as that seemed to be closest.
 *
 * mmap(2) is used to reserve address space by creating a mapping to
 * /etc/passwd with page access none.  mmap(2) is used to map pages
 * onto store by creating a copy-on-write (MAP_PRIVATE) mapping with
 * the flag MAP_ANONYMOUS.
 *
 * Experiments have shown that attempting to reserve address space
 * by mapping /dev/zero results in swap being reserved.  This
 * appears to be a bug, so we work round it by using /etc/passwd,
 * the only file we can think of which is pretty much guaranteed
 * to be around. [these experiments have not been tried on LINUX
 * so might be okay to /dev/zero].
 *
 * .assume.not-last: The implementation of VMCreate assumes that
 *   mmap() will not choose a region which contains the last page
 *   in the address space, so that the limit of the mapped area
 *   is representable.
 *
 * .assume.mmap.err: ENOMEM is the only error we really expect to
 *   get from mmap.  The others are either caused by invalid params
 *   or features we don't use.  See mmap(2) for details.
 *
 * .assume.off_t: We assume that the Size type (defined by the MM) fits
 * in the off_t type (define by the system (POSIX?)).  In fact we test
 * the more stringent requirement that they are the same size.  This
 * assumption is made in VMUnmap.
 */

#include "mpm.h"

#ifndef MPS_OS_LI
#error "vmli.c is LINUX specific, but MPS_OS_LI is not set"
#endif

/* open sesame magic, see standards(5) */
#define _POSIX_C_SOURCE 199309L
#define _XOPEN_SOURCE_EXTENDED 1

/* for open(2) */
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>

/* for mmap(2),munmap(2) */
#include <sys/mman.h>

/* for errno(2) */
#include <errno.h>

/* for sysconf(2),close(2) */
#include <unistd.h>

SRCID(vmli, "$HopeName: MMsrc!vmli.c(trunk.3) $");


/* VMStruct -- virtual memory structure */

#define VMSig           ((Sig)0x519B3999) /* SIGnature VM */

typedef struct VMStruct {
  Sig sig;                      /* design.mps.sig */
  Align align;                  /* page size */
  Addr base, limit;             /* boundaries of reserved space */
  Size reserved;                /* total reserved address space */
  Size mapped;                  /* total mapped memory */
  int none_fd;                  /* fildes for reserved memory */
} VMStruct;


Align VMAlign(VM vm)
{
  return vm->align;
}


Bool VMCheck(VM vm)
{
  CHECKS(VM, vm);
  CHECKL(vm->none_fd >= 0);
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
  Align align;
  VM vm;
  int none_fd;
  long pagesize;
  void *addr;

  AVER(vmReturn != NULL);

  /* sysconf code copied wholesale from vmso.c */
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

  none_fd = open("/etc/passwd", O_RDONLY);
  if(none_fd == -1) {
    return ResFAIL;
  }

  /* Map in a page to store the descriptor on. */
  addr = mmap(0, (size_t)SizeAlignUp(sizeof(VMStruct), align),
              PROT_READ | PROT_WRITE,
              MAP_ANONYMOUS | MAP_PRIVATE,
              -1, 0);
  if(addr == (void *)-1) {
    int e = errno;
    AVER(e == ENOMEM); /* .assume.mmap.err */
    close(none_fd);
    if(e == ENOMEM)
      return ResMEMORY;
    else
      return ResFAIL;
  }
  vm = (VM)addr;

  vm->none_fd = none_fd;
  vm->align = align;

  /* See .assume.not-last. */
  addr = mmap(0, (size_t)size,
              PROT_NONE, MAP_FILE | MAP_SHARED,
              none_fd, 0);
  if(addr == (void *)-1) {
    int e = errno;
    AVER(e == ENOMEM); /* .assume.mmap.err */
    close(none_fd);
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

  EVENT_PAA(VMCreate, vm, vm->base, vm->limit);

  *vmReturn = vm;
  return ResOK;
}


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

  close(vm->none_fd);
  r = munmap((void *)vm->base, (size_t)AddrOffset(vm->base, vm->limit));
  AVER(r == 0);
  r = munmap((void *)vm,
             (size_t)SizeAlignUp(sizeof(VMStruct), vm->align));
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

  AVERT(VM, vm);
  AVER(sizeof(void *) == sizeof(Addr));
  AVER(base < limit);
  AVER(base >= vm->base);
  AVER(limit <= vm->limit);
  AVER(AddrOffset(base, limit) <= INT_MAX);
  AVER(AddrIsAligned(base, vm->align));
  AVER(AddrIsAligned(limit, vm->align));

  size = AddrOffset(base, limit);

  if(mmap((void *)base, (size_t)size,
          PROT_READ | PROT_WRITE | PROT_EXEC,
          MAP_ANONYMOUS | MAP_PRIVATE | MAP_FIXED,
          -1, 0)
     == (void *)-1) {
    AVER(errno == ENOMEM); /* .assume.mmap.err */
    return ResMEMORY;
  }

  vm->mapped += size;

  EVENT_PAA(VMUnmap, vm, base, limit);
  return ResOK;
}


/* see design.mps.vmo1.fun.unmap */
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
  AVER(sizeof(off_t) == sizeof(Size));  /* .assume.off_t */

  size = AddrOffset(base, limit);

  /* see design.mps.vmo1.fun.unmap.offset */
  addr = mmap((void *)base, (size_t)size,
              PROT_NONE, MAP_FILE | MAP_SHARED | MAP_FIXED,
              vm->none_fd, (off_t)AddrOffset(vm->base, base));
  AVER(addr == (void *)base);

  vm->mapped -= size;

  EVENT_PAA(VMUnmap, vm, base, limit);
}
