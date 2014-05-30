/* vmix.c: VIRTUAL MEMORY MAPPING FOR UNIX (ISH)
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: This is the implementation of the virtual memory mapping
 * interface (vm.h) for Unix-like operating systems.  It was created
 * by copying vmfr.c (the FreeBSD) implementation (as that seemed to
 * use the most standards conforming interfaces).  vmfr.c was itself
 * copied from vli.c (Linux) which was itself copied from vmo1.c (OSF/1
 * / DIGITAL UNIX / Tru64).
 *
 * .deployed: Currently used on Darwin (OS X) and FreeBSD.
 *
 * .design: See <design/vm/>.  .design.mmap: mmap(2) is used to
 * reserve address space by creating a mapping with page access none.
 * mmap(2) is used to map pages onto store by creating a copy-on-write
 * (MAP_PRIVATE) mapping with the flag MAP_ANON.
 *
 * .non-standard: Note that the MAP_ANON flag is non-standard; it is
 * available on Darwin and FreeBSD. .non-standard.linux: Linux seems
 * to use MAP_ANONYMOUS instead. Some Linux systems make MAP_ANON
 * available and deprecate it. .non-standard.sesame: On Linux getting
 * a definition of MAP_ANON requires a _BSD_SOURCE to be defined prior
 * to <sys/mman.h>; see config.h.
 *
 * .assume.not-last: The implementation of VMCreate assumes that
 * mmap() will not choose a region which contains the last page
 * in the address space, so that the limit of the mapped area
 * is representable.
 *
 * .assume.mmap.err: ENOMEM is the only error we really expect to
 * get from mmap.  The others are either caused by invalid params
 * or features we don't use.  See mmap(2) for details.
 *
 * .remap: Possibly this should use mremap to reduce the number of
 * distinct mappings.  According to our current testing, it doesn't
 * seem to be a problem.
 */

#include "mpm.h"

/* for mmap(2), munmap(2) */
#include <sys/types.h>
#include <sys/mman.h> /* see .feature.li in config.h */

/* for errno(2) */
#include <errno.h>

/* for getpagesize(3) */
#include <unistd.h>


#if !defined(MPS_OS_FR) && !defined(MPS_OS_XC) && !defined(MPS_OS_LI)
#error "vmix.c is Unix-like specific, currently MPS_OS_FR XC LI"
#endif

SRCID(vmix, "$Id$");


/* VMStruct -- virtual memory structure */

#define VMSig           ((Sig)0x519B3999) /* SIGnature VM */

typedef struct VMStruct {
  Sig sig;                      /* <design/sig/> */
  Align align;                  /* page size */
  void *reserved_base;          /* unaligned base of mmap'd memory */
  Addr base, limit;             /* aligned boundaries of reserved space */
  Size reserved;                /* total reserved address space */
  Size mapped;                  /* total mapped memory */
} VMStruct;


/* VMAlign -- return page size */

Align VMAlign(VM vm)
{
  return vm->align;
}


/* VMCheck -- check a VM */

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


Res VMParamFromArgs(void *params, size_t paramSize, ArgList args)
{
  AVER(params != NULL);
  AVERT(ArgList, args);
  UNUSED(paramSize);
  return ResOK;
}


/* VMCreate -- reserve some virtual address space, and create a VM structure */

Res VMCreate(VM *vmReturn, Size size, Align align, void *params)
{
  VM vm;
  int pagesize;
  size_t reserved;
  Align pagealign;
  void *addr;
  Res res;

  AVER(vmReturn != NULL);
  AVER(params != NULL);

  /* Find out the page size from the OS */
  pagesize = getpagesize();

  /* Check the actual returned pagesize will fit in an object of type
   * Align, and that it is a valid alignment. */
  AVER(pagesize > 0);
  AVER((unsigned long)pagesize <= (unsigned long)(Align)-1);
  pagealign = (Align)pagesize;
  AVERT(Align, pagealign);

  align = AlignAlignUp(align, pagealign);
  size = SizeAlignUp(size, align);
  if((size == 0) || (size > (Size)(size_t)-1))
    return ResRESOURCE;

  /* Map in some pages to store the descriptor on. */
  addr = mmap(0, (size_t)SizeAlignUp(sizeof(VMStruct), pagealign),
              PROT_READ | PROT_WRITE,
              MAP_ANON | MAP_PRIVATE,
              -1, 0);
  /* On Darwin the MAP_FAILED return value is not documented, but does
   * work.  MAP_FAILED _is_ documented by POSIX.
   */
  if(addr == MAP_FAILED) {
    int e = errno;
    AVER(e == ENOMEM); /* .assume.mmap.err */
    return ResMEMORY;
  }
  vm = (VM)addr;

  vm->align = align;

  /* See .assume.not-last. */
  reserved = size + align - pagealign;
  addr = mmap(0, reserved, PROT_NONE, MAP_ANON | MAP_PRIVATE, -1, 0);
  if(addr == MAP_FAILED) {
    int e = errno;
    AVER(e == ENOMEM); /* .assume.mmap.err */
    res = ResRESOURCE;
    goto failReserve;
  }

  vm->reserved_base = addr;
  vm->base = AddrAlignUp(addr, align);
  vm->limit = AddrAdd(vm->base, size);
  vm->reserved = reserved;
  vm->mapped = (Size)0;

  vm->sig = VMSig;

  AVERT(VM, vm);

  EVENT4(VMCreate, vm, vm->align, vm->base, vm->limit);

  *vmReturn = vm;
  return ResOK;

failReserve:
  (void)munmap((void *)vm, (size_t)SizeAlignUp(sizeof(VMStruct), pagealign));
  return res;
}


/* VMDestroy -- release all address space and destroy VM structure */

void VMDestroy(VM vm)
{
  int r;
  Align pagealign = (Align)getpagesize();

  AVERT(VM, vm);
  AVER(vm->mapped == (Size)0);

  EVENT1(VMDestroy, vm);

  /* This appears to be pretty pointless, since the descriptor */
  /* page is about to vanish completely.  However, munmap might fail */
  /* for some reason, and this would ensure that it was still */
  /* discovered if sigs were being checked. */
  vm->sig = SigInvalid;

  r = munmap(vm->reserved_base, vm->reserved);
  AVER(r == 0);
  r = munmap((void *)vm, (size_t)SizeAlignUp(sizeof(VMStruct), pagealign));
  AVER(r == 0);
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


/* VMReserved -- return the amount of memory reserved */

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

  if(mmap((void *)base, (size_t)size,
          PROT_READ | PROT_WRITE | PROT_EXEC,
          MAP_ANON | MAP_PRIVATE | MAP_FIXED,
          -1, 0)
     == MAP_FAILED) {
    AVER(errno == ENOMEM); /* .assume.mmap.err */
    return ResMEMORY;
  }

  vm->mapped += size;

  EVENT3(VMMap, vm, base, limit);
  return ResOK;
}


/* VMUnmap -- unmap the given range of memory */

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

  size = AddrOffset(base, limit);

  /* see <design/vmo1/#fun.unmap.offset> */
  addr = mmap((void *)base, (size_t)size,
              PROT_NONE, MAP_ANON | MAP_PRIVATE | MAP_FIXED,
              -1, 0);
  AVER(addr == (void *)base);

  vm->mapped -= size;

  EVENT3(VMUnmap, vm, base, limit);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
