/* sa.h: SPARSE ARRAY INTERFACE
 *
 * $Id$
 * Copyright (c) 2014 Ravenbrook Limited.  See end of file for license.
 *
 * A sparse array is an array whose storage is partially mapped from a VM.
 * Each element in the array is its own "mapped" status, and may only
 * be used if it is mapped.
 *
 * The main use of sparse arrays is partially mapped page tables in the
 * VM arena, where they provide a fast lookup from an address within
 * a chunk to a page descriptor, while avoiding mapping memory for
 * page descriptors for unused areas of address space, such as unused
 * zone stripes or gaps between those stripes.
 */

#ifndef sa_h
#define sa_h

#include "mpmtypes.h"

typedef struct SparseArrayStruct *SparseArray;

#define SparseArraySig  ((Sig)0x5195BA66) /* SIGnature SParse ARRay */

typedef struct SparseArrayStruct {
  Sig sig;
  void *base;           /* base of array, page aligned */
  Size elementSize;     /* size of array elements, <= page size */
  Index length;         /* number of elements in the array */
  BT mapped;            /* whether elements exist in the array */
  BT pages;             /* whether underlying pages are mapped */
  VM vm;                /* where pages are mapped from */
  Shift shift;          /* SizeLog2(VMAlign(vm)) TODO: VMShift(vm) */
} SparseArrayStruct;

extern void SparseArrayInit(SparseArray sa,
                            void *base, Size elementSize, Index length,
                            BT defined, BT mapped, VM vm);
extern void SparseArrayFinish(SparseArray sa);
extern Bool SparseArrayCheck(SparseArray sa);

#define SparseArrayIsMapped(sa, i) BTGet((sa)->mapped, i)

extern Res SparseArrayMap(SparseArray sa, Index baseEI, Index limitEI);
extern void SparseArrayUnmap(SparseArray sa, Index baseEI, Index limitEI);

#endif /* sa_h */

/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
