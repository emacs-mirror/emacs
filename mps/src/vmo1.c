/* impl.c.vmso: VIRTUAL MEMORY MAPPING FOR SOLARIS 2.x
 *
 * $HopeName$
 * Copyright (C) 1995,1997 Harlequin Group, all rights reserved
 *
 * Design: design.mps.vm
 *
 * Status: er, a bit hacky.
 *
 * This is the implementation of the virtual memory mapping interface
 * (vm.h) for DEC UNIX / OSF1 (os.o1).
 *
 * mmap(2) is used to reserve address space by creating a mapping to
 * /etc/passwd with page access none.  mmap(2) is used to map pages
 * onto store by creating a copy-on-write mapping with the flag 
 * MAP_ANONYMOUS.
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
 * .assume.mmap.err: ENOMEM is the only error we really expect to
 *   get from mmap.  The others are either caused by invalid params
 *   or features we don't use.  See mmap(2) for details.
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

SRCID(vmo1, "$HopeName$");


/* Fix unprototyped system calls
 *
 * For some bizarre reason DEC go out of there way to not prototype
 * these calls when using gcc.  See /usr/include/standards.h and
 * /usr/include/unistd.h for the very gory details. */
#ifdef MPS_BUILD_GC
extern int close(int);
extern int getpagesize(void);
#endif


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


Res VMCreate(Space *spaceReturn, Size size, Addr base)
{
  void *addr;
  Align align;
  int none_fd;
  Space space;
  VM vm;

  align = VMAlign();

  AVER(spaceReturn != NULL);
  AVER(SizeIsAligned(size, align));
  AVER(size != 0);
  AVER(base == NULL);

  none_fd = open("/etc/passwd", O_RDONLY);
  if(none_fd == -1) {
    return ResFAIL;
  }

  /* Map in a page to store the descriptor on. */
  addr = mmap(0, (size_t)SizeAlignUp(sizeof(SpaceStruct), align),
              PROT_READ | PROT_WRITE,
	      MAP_ANONYMOUS | MAP_PRIVATE | MAP_VARIABLE,
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
  space = (Space)addr;
  vm = SpaceVM(space);

  vm->none_fd = none_fd;
  vm->align = align;

  /* See .assume.not-last. */
  addr = mmap(0, (size_t)size,
	      PROT_NONE, MAP_FILE | MAP_SHARED | MAP_VARIABLE,
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

  close(vm->none_fd);
  r = munmap((void *)vm->base, (size_t)AddrOffset(vm->base, vm->limit));
  AVER(r == 0);
  r = munmap((void *)space,
             (size_t)SizeAlignUp(sizeof(SpaceStruct), vm->align));
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
	  -1, 0) == (void *)-1) {
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
  void *addr;

  AVERT(VM, vm);
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

  addr = mmap((void *)base, (size_t)size,
              PROT_NONE, MAP_FILE | MAP_SHARED | MAP_FIXED,
	      vm->none_fd, 0);
  AVER(addr != (void *)-1);

  vm->mapped -= size;
}

