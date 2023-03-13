/* vm.h: VIRTUAL MEMORY INTERFACE
 *
 * $Id$
 * Copyright (c) 2014-2020 Ravenbrook Limited.  See end of file for license.
 */

#ifndef vm_h
#define vm_h

#include "mpmtypes.h"


/* VMStruct -- virtual memory structure
 *
 * Unlike most other datatypes we permit this structure to be moved
 * around in memory, and in particular, allocated temporarily on the
 * stack, to help with bootstrapping. Look for uses of VMCopy.
 */

#define VMSig           ((Sig)0x519B3999) /* SIGnature VM */

typedef struct VMStruct {
  Sig sig;                      /* design.mps.sig.field */
  Size pageSize;                /* operating system page size */
  void *block;                  /* unaligned base of mmap'd memory */
  Addr base, limit;             /* aligned boundaries of reserved space */
  Size reserved;                /* total reserved address space */
  Size mapped;                  /* total mapped memory */
} VMStruct;


#define VMPageSize(vm) RVALUE((vm)->pageSize)
#define VMBase(vm) RVALUE((vm)->base)
#define VMLimit(vm) RVALUE((vm)->limit)
#define VMReserved(vm) RVALUE((vm)->reserved)
#define VMMapped(vm) RVALUE((vm)->mapped)

extern Size PageSize(void);
extern Size (VMPageSize)(VM vm);
extern Bool VMCheck(VM vm);
extern Res VMParamFromArgs(void *params, size_t paramSize, ArgList args);
extern Res VMInit(VM vmReturn, Size size, Size grainSize, void *params);
extern void VMFinish(VM vm);
extern Addr (VMBase)(VM vm);
extern Addr (VMLimit)(VM vm);
extern Res VMMap(VM vm, Addr base, Addr limit);
extern void VMUnmap(VM vm, Addr base, Addr limit);
extern Size (VMReserved)(VM vm);
extern Size (VMMapped)(VM vm);
extern void VMCopy(VM dest, VM src);


#endif /* vm_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2014-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
