/* impl.c.vmo1: VIRTUAL MEMORY MAPPING FOR DIGITAL UNIX
 *
 * $HopeName: MMsrc!vmo1.c(trunk.9) $
 * Copyright (C) 1998 Harlequin Limited.  All rights reserved.
 *
 * .design: design.mps.vm, design.mps.vmo1
 *
 * .status: A bit hacky, but probably working.
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

#ifndef MPS_OS_O1
#error "vmo1.c is DEC UNIX / OSF1 specific, but MPS_OS_O1 is not set"
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

/* for getpagesize(2),close(2) */
#include <unistd.h>

SRCID(vmo1, "$HopeName: MMsrc!vmo1.c(trunk.9) $");


/* Fix unprototyped system calls
 *
 * For some bizarre reason DEC go out of their way to not prototype
 * these calls when using gcc.  See /usr/include/standards.h and
 * /usr/include/unistd.h for the very gory details.
 */
#ifdef MPS_BUILD_GC
extern int close(int);
extern int getpagesize(void);
#endif


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


/* VMAlign -- return the page size */

Align VMAlign(VM vm)
{
  return vm->align;
}


/* VMCheck -- check a VM structure */

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
  void *addr;
  Align align;
  int none_fd;
  VM vm;
  Res res;

  AVER(vmReturn != NULL);

  align = (Align)getpagesize();
  AVER(SizeIsP2(align));
  size = SizeAlignUp(size, align);
  if ((size == 0) || (size > (Size)(size_t)-1))
    return ResRESOURCE;

  none_fd = open("/etc/passwd", O_RDONLY);
  if (none_fd == -1) {
    return ResFAIL;
  }

  /* Map in a page to store the descriptor on. */
  addr = mmap(0, (size_t)SizeAlignUp(sizeof(VMStruct), align),
              PROT_READ | PROT_WRITE,
              MAP_ANONYMOUS | MAP_PRIVATE | MAP_VARIABLE,
              -1, 0);
  if (addr == (void *)-1) {
    int e = errno;
    AVER(e == ENOMEM); /* .assume.mmap.err */
    res = (e == ENOMEM) ? ResMEMORY : ResFAIL;
    goto failVMMap;
  }
  vm = (VM)addr;

  vm->none_fd = none_fd;
  vm->align = align;

  /* See .assume.not-last. */
  addr = mmap(0, (size_t)size,
              PROT_NONE, MAP_FILE | MAP_SHARED | MAP_VARIABLE,
              none_fd, 0);
  if (addr == (void *)-1) {
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
  (void)munmap((void *)vm, (size_t)SizeAlignUp(sizeof(VMStruct), align));
failVMMap:
  (void)close(none_fd); /* see .close.fail */
  return res;
}


/* VMDestroy -- destroy the VM structure */

void VMDestroy(VM vm)
{
  int r;
  int none_fd;

  AVERT(VM, vm);
  AVER(vm->mapped == (Size)0);

  /* This appears to be pretty pointless, since the space descriptor */
  /* page is  about to vanish completely.  However, munmap might fail */
  /* for some reason, and this would ensure that it was still */
  /* discovered if sigs were being checked. */
  vm->sig = SigInvalid;

  none_fd = vm->none_fd;
  r = munmap((void *)vm->base, (size_t)AddrOffset(vm->base, vm->limit));
  AVER(r == 0);
  r = munmap((void *)vm, (size_t)SizeAlignUp(sizeof(VMStruct), vm->align));
  AVER(r == 0);
  /* .close.fail: We ignore failure from close() as there's very */
  /* little we can do anyway. */
  (void)close(none_fd);

  EVENT_P(VMDestroy, vm);
}


/* VMBase -- return the base address of the memory reserved */

Addr VMBase(VM vm)
{
  AVERT(VM, vm);

  return vm->base;
}


/* VMLimit -- return the limit address of the memory reserved */

Addr VMLimit(VM vm)
{
  AVERT(VM, vm);

  return vm->limit;
}


/* VMReserved -- return the amount of address space reserved */

Size VMReserved(VM vm)
{
  AVERT(VM, vm);

  return vm->reserved;
}


/* VMMapped -- return the amount of memory actually mapped */

Size VMMapped(VM vm)
{
  AVERT(VM, vm);

  return vm->mapped;
}


/* VMMap -- map the given range of memory */

Res VMMap(VM vm, Addr base, Addr limit)
{
  Size size;

  AVERT(VM, vm);
  AVER(sizeof(void *) == sizeof(Addr));
  AVER(base < limit);
  AVER(base >= vm->base);
  AVER(limit <= vm->limit);
  AVER(AddrIsAligned(base, vm->align));
  AVER(AddrIsAligned(limit, vm->align));

  size = AddrOffset(base, limit);

  if (mmap((void *)base, (size_t)size,
          PROT_READ | PROT_WRITE | PROT_EXEC,
          MAP_ANONYMOUS | MAP_PRIVATE | MAP_FIXED,
          -1, 0)
      == (void *)-1) {
    AVER(errno == ENOMEM); /* .assume.mmap.err */
    return ResMEMORY;
  }

  vm->mapped += size;

  EVENT_PAA(VMMap, vm, base, limit);
  return ResOK;
}


/* VMUnmap -- unmap the given range of memory
 *
 * See design.mps.vmo1.fun.unmap.
 */

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
