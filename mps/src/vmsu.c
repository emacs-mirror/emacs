/*  impl.c.vmsu  draft impl
 *
 *                     VIRTUAL MEMORY MAPPING FOR SUNOS 4
 *
 *  $HopeName: MMsrc/!vmsu.c(trunk.3)$
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
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
 *  appears to be a bug, so we work round it by using /etc/passwd.
 */

#include "std.h"
#include "vm.h"

#ifndef OS_SUNOS
#error "vmsu.c is SunOS 4 specific, but OS_SUNOS is not set"
#endif

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
  Addr grain = VMGrain();
  int zero_fd;
  int none_fd;
  VM vm;

  AVER(vmReturn != NULL);
  AVER(IsAligned(grain, size));
  AVER(size != 0);

  zero_fd = open("/dev/zero", O_RDONLY);
  if(zero_fd == -1)
    return(ErrIO);
  none_fd = open("/etc/passwd", O_RDONLY);
  if(none_fd == -1) {
    close(zero_fd);
    return(ErrIO);
  }

  /* Map in a page to store the descriptor on. */
  addr = mmap(0, AlignUp(grain, sizeof(VMStruct)),
	      PROT_READ | PROT_WRITE, MAP_PRIVATE,
	      zero_fd, 0);
  if((int)addr == -1)
    return(errno == ENOMEM ? ErrRESOURCE : ErrFAILURE);
  vm = (VM)addr;

  vm->zero_fd = zero_fd;
  vm->none_fd = none_fd;

  addr = mmap(0, size, PROT_NONE, MAP_SHARED, none_fd, 0);
  if((int)addr == -1)
    return(errno == ENOMEM ? ErrRESOURCE : ErrFAILURE);

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
  Addr grain = VMGrain();

  AVER(ISVALID(VM, vm));

  /* This is pretty pointless, since the vm descriptor page is about */
  /* to vanish completely. */
#ifdef DEBUG_SIGN
  vm->sig = SigInvalid;
#endif

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
  AVER(IsAligned(grain, base));
  AVER(IsAligned(grain, limit));

  if((int)mmap((caddr_t)base, (int)(limit - base),
	       PROT_READ | PROT_WRITE | PROT_EXEC,
	       MAP_PRIVATE | MAP_FIXED,
	       vm->zero_fd, 0) == -1)
    return(ErrRESMEM);

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

  addr = mmap((caddr_t)base, (int)(limit - base),
	      PROT_NONE, MAP_SHARED, vm->none_fd, 0);
  AVER((int)addr != -1);
}

