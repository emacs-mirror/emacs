/* sa.c: SPARSE ARRAY IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2014-2020 Ravenbrook Limited.  See end of file for license.
 */

#include "sa.h"
#include "mpm.h"
#include "bt.h"
#include "vm.h"

static Index pagesLength(SparseArray sa)
{
  return (sa->length * sa->elementSize + VMPageSize(sa->vm) - 1) >> sa->shift;
}

void SparseArrayInit(SparseArray sa,
                     void *base, Size elementSize, Index length,
                     BT mapped, BT pages, VM vm)
{
  AVER(sa != NULL);

  sa->base = base;
  sa->elementSize = elementSize;
  sa->length = length;
  sa->mapped = mapped;
  sa->pages = pages;
  sa->vm = vm;
  AVER(SizeIsP2(VMPageSize(vm)));
  sa->shift = SizeLog2(VMPageSize(vm));
  BTResRange(mapped, 0, length);
  BTResRange(pages, 0, pagesLength(sa));

  sa->sig = SparseArraySig;
  AVERT(SparseArray, sa);
}

void SparseArrayFinish(SparseArray sa)
{
  AVERT(SparseArray, sa);
  AVER(BTIsResRange(sa->mapped, 0, sa->length));
  AVER(BTIsResRange(sa->pages, 0, pagesLength(sa)));
  sa->sig = SigInvalid;
}

Bool SparseArrayCheck(SparseArray sa)
{
  CHECKL(sa != NULL);
  CHECKS(SparseArray, sa);
  CHECKL(sa->base != NULL);
  CHECKL(sa->elementSize >= 1);
  CHECKD_NOSIG(VM, sa->vm); /* <design/check#.hidden-type> */
  CHECKL(sa->elementSize <= VMPageSize(sa->vm));
  CHECKL(sa->length > 0);
  CHECKD_NOSIG(BT, sa->mapped);
  CHECKD_NOSIG(BT, sa->pages);
  CHECKL(sa->shift == SizeLog2(VMPageSize(sa->vm)));
  return TRUE;
}


/* SparseArrayMap -- map memory for a range of elements in the array
 *
 * Ensures that the array elements in the unmapped range [baseEI, limitEI)
 * have memory.  The array elements may then be accessed, but their contents
 * will be undefined.
 *
 * In the MPS we expect this to be called frequently when allocating in
 * the arena, and so it's worth having the pages bit table to make this
 * fast.  Compare with SparseArrayUnmap.
 */

Res SparseArrayMap(SparseArray sa, Index baseEI, Index limitEI)
{
  Index baseMI, limitMI;

  AVERT(SparseArray, sa);
  AVER(NONNEGATIVE(baseEI));
  AVER(baseEI < limitEI);
  AVER(limitEI <= sa->length);
  AVER(BTIsResRange(sa->mapped, baseEI, limitEI));

  /* Calculate the index of the page on which the base element resides.
     If that's already mapped (because some other element below baseEI
     is defined) bump up to the next page. */
  baseMI = (baseEI * sa->elementSize) >> sa->shift;
  if (BTGet(sa->pages, baseMI))
    ++baseMI;

  /* Calculate the index of the page on which the last element resides.
     If that's already mapped (because some other element not below
     limitEI is defined) bump down to the previous page. */
  limitMI = ((limitEI * sa->elementSize - 1) >> sa->shift) + 1;
  if (BTGet(sa->pages, limitMI - 1))
    --limitMI;

  if (baseMI < limitMI) {
    Addr base, limit;
    Res res;
    AVER(BTIsResRange(sa->pages, baseMI, limitMI));
    base = AddrAdd(sa->base, baseMI << sa->shift);
    limit = AddrAdd(sa->base, limitMI << sa->shift);
    res = VMMap(sa->vm, base, limit);
    if (res != ResOK)
      return res;
    BTSetRange(sa->pages, baseMI, limitMI);
  }

  BTSetRange(sa->mapped, baseEI, limitEI);

  return ResOK;
}


/* SparseArrayUnmap -- unmap memory for a range of elements in the array
 *
 * Declare that the array elements in the range [baseEI, limitEI) can be
 * unmapped.  After this call they may not be accessed.
 *
 * In the MPS we expect this to be called infrequently when purging large
 * numbers of spare pages at once, so scanning a range of bits to determine
 * whether we can unmap isn't too bad.
 *
 * TODO: Consider keeping a count of the number of array elements defined
 * on each page, rather than a bit table, then we can unmap pages with
 * zero counts rather than scanning.
 */

void SparseArrayUnmap(SparseArray sa, Index baseEI, Index limitEI)
{
  Index baseMI, limitMI, i;

  AVERT(SparseArray, sa);
  AVER(NONNEGATIVE(baseEI));
  AVER(baseEI < limitEI);
  AVER(limitEI <= sa->length);
  AVER(BTIsSetRange(sa->mapped, baseEI, limitEI));

  /* Calculate the index of the lowest element that might be occupying
     the page on which the base element resides.  If any elements between
     there and baseMI are defined, we can't unmap that page, so bump up. */
  baseMI = (baseEI * sa->elementSize) >> sa->shift;
  i = SizeAlignDown(baseEI * sa->elementSize, VMPageSize(sa->vm)) / sa->elementSize;
  if (i < baseEI && !BTIsResRange(sa->mapped, i, baseEI))
    ++baseMI;

  /* Calculate the index of the highest element that might be occupying
     the page on which the last element resides.  If any elements between
     limitMI and there are defined, we can't unmap that page, so bump down. */
  limitMI = ((limitEI * sa->elementSize - 1) >> sa->shift) + 1;
  i = (SizeAlignUp(limitEI * sa->elementSize, VMPageSize(sa->vm)) +
       sa->elementSize - 1) / sa->elementSize;
  if (i > sa->length)
    i = sa->length;
  if (i > limitEI && !BTIsResRange(sa->mapped, limitEI, i))
    --limitMI;

  if (baseMI < limitMI) {
    Addr base, limit;
    AVER(BTIsSetRange(sa->pages, baseMI, limitMI));
    base = AddrAdd(sa->base, baseMI << sa->shift);
    limit = AddrAdd(sa->base, limitMI << sa->shift);
    VMUnmap(sa->vm, base, limit);
    BTResRange(sa->pages, baseMI, limitMI);
  }

  BTResRange(sa->mapped, baseEI, limitEI);
}


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
