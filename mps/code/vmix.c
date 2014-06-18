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
 * .assume.not-last: The implementation of VMInit assumes that
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
#include "vm.h"

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


/* PageSize -- return operating system page size */

Size PageSize(void)
{
  int pageSize;

  /* Find out the operating system page size */
  pageSize = getpagesize();

  /* Check the page size will fit in a Size. */
  AVER((unsigned long)pageSize <= (unsigned long)(Size)-1);

  return (Size)pageSize;
}


Res VMParamFromArgs(void *params, size_t paramSize, ArgList args)
{
  AVER(params != NULL);
  AVERT(ArgList, args);
  UNUSED(paramSize);
  return ResOK;
}


/* VMInit -- reserve some virtual address space, and create a VM structure */

Res VMInit(VM vm, Size size, Size grainSize, void *params)
{
  Size pageSize, reserved;
  void *vbase;

  AVER(vm != NULL);
  AVERT(ArenaGrainSize, grainSize);
  AVER(size > 0);
  AVER(params != NULL);

  pageSize = PageSize();

  /* Grains must consist of whole pages. */
  AVER(grainSize % pageSize == 0);

  /* Check that the rounded-up sizes will fit in a Size. */
  size = SizeRoundUp(size, grainSize);
  if (size < grainSize || size > (Size)(size_t)-1)
    return ResRESOURCE;
  reserved = size + grainSize - pageSize;
  if (reserved < grainSize || reserved > (Size)(size_t)-1)
    return ResRESOURCE;

  /* See .assume.not-last. */
  vbase = mmap(0, reserved,
               PROT_NONE, MAP_ANON | MAP_PRIVATE,
               -1, 0);
  /* On Darwin the MAP_FAILED return value is not documented, but does
   * work.  MAP_FAILED _is_ documented by POSIX.
   */
  if (vbase == MAP_FAILED) {
    int e = errno;
    AVER(e == ENOMEM); /* .assume.mmap.err */
    return ResRESOURCE;
  }

  vm->pageSize = pageSize;
  vm->block = vbase;
  vm->base = AddrAlignUp(vbase, grainSize);
  vm->limit = AddrAdd(vm->base, size);
  AVER(vm->base < vm->limit);  /* .assume.not-last */
  AVER(vm->limit <= AddrAdd((Addr)vm->block, reserved));
  vm->reserved = reserved;
  vm->mapped = 0;

  vm->sig = VMSig;
  AVERT(VM, vm);

  EVENT3(VMInit, vm, VMBase(vm), VMLimit(vm));
  return ResOK;
}


/* VMFinish -- release all address space and finish VM structure */

void VMFinish(VM vm)
{
  int r;

  AVERT(VM, vm);
  /* Descriptor must not be stored inside its own VM at this point. */
  AVER(PointerAdd(vm, sizeof *vm) <= vm->block
       || PointerAdd(vm->block, VMReserved(vm)) <= (Pointer)vm);
  /* All address space must have been unmapped. */
  AVER(VMMapped(vm) == (Size)0);

  EVENT1(VMFinish, vm);

  vm->sig = SigInvalid;

  r = munmap(vm->block, vm->reserved);
  AVER(r == 0);
}


/* VMMap -- map the given range of memory */

Res VMMap(VM vm, Addr base, Addr limit)
{
  Size size;

  AVERT(VM, vm);
  AVER(sizeof(void *) == sizeof(Addr));
  AVER(base < limit);
  AVER(base >= VMBase(vm));
  AVER(limit <= VMLimit(vm));
  AVER(AddrIsAligned(base, vm->pageSize));
  AVER(AddrIsAligned(limit, vm->pageSize));

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
  AVER(VMMapped(vm) <= VMReserved(vm));

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
  AVER(base >= VMBase(vm));
  AVER(limit <= VMLimit(vm));
  AVER(AddrIsAligned(base, vm->pageSize));
  AVER(AddrIsAligned(limit, vm->pageSize));

  size = AddrOffset(base, limit);
  AVER(size <= VMMapped(vm));

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
