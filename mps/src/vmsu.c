/*  impl.c.vmsu
 *
 *                     VIRTUAL MEMORY MAPPING FOR SUNOS 4
 *
 *  $HopeName: MMsrc/!vmsu.c(trunk.4)$
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  Design: design.mps.vm
 *
 *  This is the implementation of the virtual memory mapping interface
 *  (vm.h) for SunOS 4.
 *
 *  mmap(2) is used to reserve address space by creating a mapping to
 *  /etc/passwd with page access none.  mmap(2) is used to map pages
 *  onto store by creating a copy-on-write mapping to /dev/zero.
 *
 *  Experiments have shown that attempting to reserve address space
 *  by mapping /dev/zero results in swap being reserved.  This
 *  appears to be a bug, so we work round it by using /etc/passwd,
 *  the only file we can think of which is pretty much guaranteed
 *  to be around.
 *
 *  .assume.not-last: The implementation of VMCreate assumes that
 *    mmap() will not choose a region which contains the last page
 *    in the address space, so that the limit of the mapped area
 *    is representable.
 *
 *  .assume.size: The maximum size of the reserved address space
 *    is limited by the range of "int".  This will probably be half
 *    of the address space.
 *
 *  .assume.mmap.err: ENOMEM is the only error we really expect to
 *    get from mmap.  The others are either caused by invalid params
 *    or features we don't use.  See mmap(2) for details.
 */

#include "std.h"
#include "vm.h"

#ifndef OS_SUNOS
#error "vmsu.c is SunOS 4 specific, but OS_SUNOS is not set"
#endif

#include <limits.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/errno.h>


/* Fix up unprototyped system calls.  */
/* Note that these are not fixed up by std.h because that only fixes */
/* up discrepancies with ANSI. */

extern int close(int fd);
extern int munmap(caddr_t addr, int len);
extern int getpagesize(void);


#ifdef DEBUG_SIGN
static SigStruct VMSigStruct;
#endif


typedef struct VMStruct
{
#ifdef DEBUG_SIGN
  Sig sig;
#endif
  int zero_fd;		/* file descriptor for /dev/zero */
  int none_fd;          /* fildes used for PROT_NONE (/etc/passwd) */
  Addr base, limit;	/* boundaries of reserved space */
} VMStruct;


Addr VMGrain(void)
{
  Addr grain;

  grain = (Addr)getpagesize();
  AVER(IsPoT(grain));

  return(grain);
}


#ifdef DEBUG_ASSERT

Bool VMIsValid(VM vm, ValidationType validParam)
{
  AVER(vm != NULL);
#ifdef DEBUG_SIGN
  AVER(ISVALIDNESTED(Sig, vm->sig));
  AVER(vm->sig == &VMSigStruct);
#endif
  AVER(vm->zero_fd >= 0);
  AVER(vm->none_fd >= 0);
  AVER(vm->zero_fd != vm->none_fd);
  AVER(vm->base != 0);
  AVER(vm->limit != 0);
  AVER(vm->base < vm->limit);
  AVER(IsAligned(VMGrain(), vm->base));
  AVER(IsAligned(VMGrain(), vm->limit));
  return(TRUE);
}

#endif /* DEBUG_ASSERT */


Error VMCreate(VM *vmReturn, Addr size)
{
  caddr_t addr;
  Addr grain;
  int zero_fd;
  int none_fd;
  VM vm;

  grain = VMGrain();

  AVER(vmReturn != NULL);
  AVER(IsAligned(grain, size));
  AVER(size != 0);
  AVER(size <= INT_MAX); /* see .assume.size */

  zero_fd = open("/dev/zero", O_RDONLY);
  if(zero_fd == -1)
    return(ErrFAILURE);
  none_fd = open("/etc/passwd", O_RDONLY);
  if(none_fd == -1) {
    close(zero_fd);
    return(ErrFAILURE);
  }

  /* Map in a page to store the descriptor on. */
  addr = mmap((caddr_t)0, AlignUp(grain, sizeof(VMStruct)),
	      PROT_READ | PROT_WRITE, MAP_PRIVATE,
	      zero_fd, (off_t)0);
  if((int)addr == -1) {
    int e = errno;
    AVER(e == ENOMEM); /* .assume.mmap.err */
    close(none_fd);
    close(zero_fd);
    if(e == ENOMEM)
      return ErrRESMEM;
    else
      return ErrFAILURE;
  }
  vm = (VM)addr;

  vm->zero_fd = zero_fd;
  vm->none_fd = none_fd;

  /* See .assume.not-last. */
  addr = mmap((caddr_t)0, size, PROT_NONE, MAP_SHARED, none_fd, (off_t)0);
  if((int)addr == -1) {
    int e = errno;
    AVER(e == ENOMEM); /* .assume.mmap.err */
    close(none_fd);
    close(zero_fd);
    if(e == ENOMEM)
      return ErrRESOURCE;
    else
      return ErrFAILURE;
  }

  vm->base = (Addr)addr;
  vm->limit = vm->base + size;

#ifdef DEBUG_SIGN
  SigInit(&VMSigStruct, "VM");
  vm->sig = &VMSigStruct;
#endif

  AVER(ISVALID(VM, vm));

  *vmReturn = vm;
  return(ErrSUCCESS);
}


void VMDestroy(VM vm)
{
  int r;
  Addr grain;

  AVER(ISVALID(VM, vm));

  /* This appears to be pretty pointless, since the vm descriptor page is */
  /* about to vanish completely.  However, munmap might fail for some */
  /* reason, and this would ensure that it was still discovered if sigs */
  /* were being checked. */
#ifdef DEBUG_SIGN
  vm->sig = SigInvalid;
#endif

  close(vm->zero_fd);
  close(vm->none_fd);
  grain = VMGrain();
  r = munmap((caddr_t)vm->base, (int)(vm->limit - vm->base));
  AVER(r == 0);
  r = munmap((caddr_t)vm, (int)AlignUp(grain, sizeof(VMStruct)));
  AVER(r == 0);
}


Addr VMBase(VM vm)
{
  AVER(ISVALID(VM, vm));
  return(vm->base);
}

Addr VMLimit(VM vm)
{
  AVER(ISVALID(VM, vm));
  return(vm->limit);
}


Error VMMap(VM vm, Addr base, Addr limit)
{
#ifdef DEBUG_ASSERT
  Addr grain = VMGrain();
#endif

  AVER(ISVALID(VM, vm));
  AVER(sizeof(int) == sizeof(Addr));
  AVER(base < limit);
  AVER(base >= vm->base);
  AVER(limit <= vm->limit);
  AVER((limit - base) <= INT_MAX); /* This should be redundant. */
  AVER(IsAligned(grain, base));
  AVER(IsAligned(grain, limit));

  /* Map /dev/zero onto the area with a copy-on-write policy.  This */
  /* effectively populates the area with zeroed memory. */

  if((int)mmap((caddr_t)base, (int)(limit - base),
	       PROT_READ | PROT_WRITE | PROT_EXEC,
	       MAP_PRIVATE | MAP_FIXED,
	       vm->zero_fd, (off_t)0) == -1) {
    AVER(errno == ENOMEM); /* .assume.mmap.err */
    return(ErrRESMEM);
  }

  return(ErrSUCCESS);
}


void VMUnmap(VM vm, Addr base, Addr limit)
{
  caddr_t addr;
#ifdef DEBUG_ASSERT
  Addr grain = VMGrain();
#endif

  AVER(ISVALID(VM, vm));
  AVER(sizeof(int) == sizeof(Addr));
  AVER(base < limit);
  AVER(base >= vm->base);
  AVER(limit <= vm->limit);
  AVER(IsAligned(grain, base));
  AVER(IsAligned(grain, limit));

  /* Map /etc/passwd onto the area, allowing no access.  This */
  /* effectively depopulates the area from memory, but keeps */
  /* it "busy" as far as the OS is concerned, so that it will not */
  /* be re-used by other calls to mmap which do not specify MAP_FIXED. */

  addr = mmap((caddr_t)base, (int)(limit - base),
	      PROT_NONE, MAP_SHARED, vm->none_fd, (off_t)0);
  AVER((int)addr != -1);
}

