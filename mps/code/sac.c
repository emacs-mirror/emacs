/* sac.c: SEGREGATED ALLOCATION CACHES
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 */

#include "mpm.h"
#include "sac.h"

SRCID(sac, "$Id$");


/* SACCheck -- check function for SACs */

static Bool sacFreeListBlockCheck(SACFreeListBlock fb)
{
  Count j;
  Addr cb;

  /* nothing to check about size */
  CHECKL(fb->count <= fb->countMax);
  /* check the freelist has the right number of blocks */
  for (j = 0, cb = fb->blocks; j < fb->count; ++j) {
    CHECKL(cb != NULL);
    /* @@@@ ignoring shields for now */
    cb = *ADDR_PTR(Addr, cb);
  }
  CHECKL(cb == NULL);
  return TRUE;
}

static Bool SACCheck(SAC sac)
{
  Index i, j;
  Bool b;
  Size prevSize;

  CHECKS(SAC, sac);
  CHECKU(Pool, sac->pool);
  CHECKL(sac->classesCount > 0);
  CHECKL(sac->classesCount > sac->middleIndex);
  CHECKL(BoolCheck(sac->esacStruct.trapped));
  CHECKL(sac->esacStruct.middle > 0);
  /* check classes above middle */
  prevSize = sac->esacStruct.middle;
  for (j = sac->middleIndex + 1, i = 0;
       j <= sac->classesCount; ++j, i += 2) {
    CHECKL(prevSize < sac->esacStruct.freelists[i].size);
    b = sacFreeListBlockCheck(&(sac->esacStruct.freelists[i]));
    if (!b) return b;
    prevSize = sac->esacStruct.freelists[i].size;
  }
  /* check overlarge class */
  CHECKL(sac->esacStruct.freelists[i-2].size == SizeMAX);
  CHECKL(sac->esacStruct.freelists[i-2].count == 0);
  CHECKL(sac->esacStruct.freelists[i-2].countMax == 0);
  CHECKL(sac->esacStruct.freelists[i-2].blocks == NULL);
  /* check classes below middle */
  prevSize = sac->esacStruct.middle;
  for (j = sac->middleIndex, i = 1; j > 0; --j, i += 2) {
    CHECKL(prevSize > sac->esacStruct.freelists[i].size);
    b = sacFreeListBlockCheck(&(sac->esacStruct.freelists[i]));
    if (!b) return b;
    prevSize = sac->esacStruct.freelists[i].size;
  }
  /* check smallest class */
  CHECKL(sac->esacStruct.freelists[i].size == 0);
  b = sacFreeListBlockCheck(&(sac->esacStruct.freelists[i]));
  return b;
}


/* sacSize -- calculate size of a SAC structure */

static Size sacSize(Index middleIndex, Count classesCount)
{
  Index indexMax; /* max index for the freelist */
  SACStruct dummy;

  if (middleIndex + 1 < classesCount - middleIndex)
    indexMax = 2 * (classesCount - middleIndex - 1);
  else
    indexMax = 1 + 2 * middleIndex;
  return PointerOffset(&dummy, &dummy.esacStruct.freelists[indexMax+1]);
}


/* SACCreate -- create an SAC object */

Res SACCreate(SAC *sacReturn, Pool pool, Count classesCount,
              SACClasses classes)
{
  void *p;
  SAC sac;
  Res res;
  Index i, j;
  Index middleIndex;  /* index of the size in the middle */
  Size prevSize;
  unsigned totalFreq = 0;

  AVER(sacReturn != NULL);
  AVERT(Pool, pool);
  AVER(classesCount > 0);
  /* In this cache type, there is no upper limit on classesCount. */
  prevSize = sizeof(Addr) - 1; /* must large enough for freelist link */
  /* @@@@ It would be better to dynamically adjust the smallest class */
  /* to be large enough, but that gets complicated, if you have to */
  /* merge classes because of the adjustment. */
  for (i = 0; i < classesCount; ++i) {
    AVER(classes[i].blockSize > 0);
    AVER(SizeIsAligned(classes[i].blockSize, PoolAlignment(pool)));
    AVER(prevSize < classes[i].blockSize);
    prevSize = classes[i].blockSize;
    /* no restrictions on count */
    /* no restrictions on frequency */
  }

  /* Calculate frequency scale */
  for (i = 0; i < classesCount; ++i) {
    unsigned oldFreq = totalFreq;
    totalFreq += classes[i].frequency;
    AVER(oldFreq <= totalFreq); /* check for overflow */
    UNUSED(oldFreq); /* <code/mpm.c#check.unused> */
  }

  /* Find middle one */
  totalFreq /= 2;
  for (i = 0; i < classesCount; ++i) {
    if (totalFreq < classes[i].frequency) break;
    totalFreq -= classes[i].frequency;
  }
  if (totalFreq <= classes[i].frequency / 2)
    middleIndex = i;
  else
    middleIndex = i + 1; /* there must exist another class at i+1 */

  /* Allocate SAC */
  res = ControlAlloc(&p, PoolArena(pool), sacSize(middleIndex, classesCount),
                     FALSE);
  if(res != ResOK)
    goto failSACAlloc;
  sac = p;

  /* Move classes in place */
  /* It's important this matches SACFind. */
  for (j = middleIndex + 1, i = 0; j < classesCount; ++j, i += 2) {
    sac->esacStruct.freelists[i].size = classes[j].blockSize;
    sac->esacStruct.freelists[i].count = 0;
    sac->esacStruct.freelists[i].countMax = classes[j].cachedCount;
    sac->esacStruct.freelists[i].blocks = NULL;
  }
  sac->esacStruct.freelists[i].size = SizeMAX;
  sac->esacStruct.freelists[i].count = 0;
  sac->esacStruct.freelists[i].countMax = 0;
  sac->esacStruct.freelists[i].blocks = NULL;
  for (j = middleIndex, i = 1; j > 0; --j, i += 2) {
    sac->esacStruct.freelists[i].size = classes[j-1].blockSize;
    sac->esacStruct.freelists[i].count = 0;
    sac->esacStruct.freelists[i].countMax = classes[j].cachedCount;
    sac->esacStruct.freelists[i].blocks = NULL;
  }
  sac->esacStruct.freelists[i].size = 0;
  sac->esacStruct.freelists[i].count = 0;
  sac->esacStruct.freelists[i].countMax = classes[j].cachedCount;
  sac->esacStruct.freelists[i].blocks = NULL;

  /* finish init */
  sac->esacStruct.trapped = FALSE;
  sac->esacStruct.middle = classes[middleIndex].blockSize;
  sac->pool = pool;
  sac->classesCount = classesCount;
  sac->middleIndex = middleIndex;
  sac->sig = SACSig;
  AVERT(SAC, sac);
  *sacReturn = sac;
  return ResOK;

failSACAlloc:
  return res;
}


/* SACDestroy -- destroy an SAC object */

void SACDestroy(SAC sac)
{
  AVERT(SAC, sac);
  SACFlush(sac);
  sac->sig = SigInvalid;
  ControlFree(PoolArena(sac->pool), sac,
              sacSize(sac->middleIndex, sac->classesCount));
}


/* sacFind -- find the index corresponding to size
 *
 * This function replicates the loop in MPS_SAC_ALLOC_FAST, only with
 * added checks.
 */

static void sacFind(Index *iReturn, Size *blockSizeReturn,
                    SAC sac, Size size)
{
  Index i, j;

  if (size > sac->esacStruct.middle) {
    i = 0; j = sac->middleIndex + 1;
    AVER(j <= sac->classesCount);
    while (size > sac->esacStruct.freelists[i].size) {
      AVER(j < sac->classesCount);
      i += 2; ++j;
    }
    *blockSizeReturn = sac->esacStruct.freelists[i].size;
  } else {
    Size prevSize = sac->esacStruct.middle;

    i = 1; j = sac->middleIndex;
    while (size <= sac->esacStruct.freelists[i].size) {
      AVER(j > 0);
      prevSize = sac->esacStruct.freelists[i].size;
      i += 2; --j;
    }
    *blockSizeReturn = prevSize;
  }
  *iReturn = i;
}


/* SACFill -- alloc an object, and perhaps fill the cache */

Res SACFill(Addr *p_o, SAC sac, Size size, Bool hasReservoirPermit)
{
  Index i;
  Count blockCount, j;
  Size blockSize;
  Addr p, fl;
  Res res = ResOK; /* stop compiler complaining */

  AVER(p_o != NULL);
  AVERT(SAC, sac);
  AVER(size != 0);
  AVER(BoolCheck(hasReservoirPermit));

  sacFind(&i, &blockSize, sac, size);
  /* Check it's empty (in the future, there will be other cases). */
  AVER(sac->esacStruct.freelists[i].count == 0);

  /* Fill 1/3 of the cache for this class. */
  blockCount = sac->esacStruct.freelists[i].countMax / 3;
  /* Adjust size for the overlarge class. */
  if (blockSize == SizeMAX)
    /* .align: align 'cause some classes don't accept unaligned. */
    blockSize = SizeAlignUp(size, PoolAlignment(sac->pool));
  for (j = 0, fl = sac->esacStruct.freelists[i].blocks;
       j <= blockCount; ++j) {
    res = PoolAlloc(&p, sac->pool, blockSize, hasReservoirPermit);
    if (res != ResOK)
      break;
    /* @@@@ ignoring shields for now */
    *ADDR_PTR(Addr, p) = fl; fl = p;
  }
  /* If didn't get any, just return. */
  if (j == 0) {
    AVER(res != ResOK);
    return res;
  }

  /* Take the last one off, and return it. */
  sac->esacStruct.freelists[i].count = j - 1;
  *p_o = fl;
  /* @@@@ ignoring shields for now */
  sac->esacStruct.freelists[i].blocks = *ADDR_PTR(Addr, fl);
  return ResOK;
}


/* sacClassFlush -- discard elements from the cache for a given class
 *
 * blockCount says how many elements to discard.
 */

static void sacClassFlush(SAC sac, Index i, Size blockSize,
                          Count blockCount)
{
  Addr cb, fl;
  Count j;

  for (j = 0, fl = sac->esacStruct.freelists[i].blocks;
       j < blockCount; ++j) {
    /* @@@@ ignoring shields for now */
    cb = fl; fl = *ADDR_PTR(Addr, cb);
    PoolFree(sac->pool, cb, blockSize);
  }
  sac->esacStruct.freelists[i].count -= blockCount;
  sac->esacStruct.freelists[i].blocks = fl;
}


/* SACEmpty -- free an object, and perhaps empty the cache */

void SACEmpty(SAC sac, Addr p, Size size)
{
  Index i;
  Size blockSize;

  AVERT(SAC, sac);
  AVER(p != NULL);
  AVER(PoolHasAddr(sac->pool, p));
  AVER(size > 0);

  sacFind(&i, &blockSize, sac, size);
  /* Check it's full (in the future, there will be other cases). */
  AVER(sac->esacStruct.freelists[i].count
       == sac->esacStruct.freelists[i].countMax);

  /* Adjust size for the overlarge class. */
  if (blockSize == SizeMAX)
    /* see .align */
    blockSize = SizeAlignUp(size, PoolAlignment(sac->pool));
  if (sac->esacStruct.freelists[i].countMax > 0) {
    Count blockCount;

    /* Flush 2/3 of the cache for this class. */
    /* Computed as count - count/3, so that the rounding works out right. */
    blockCount = sac->esacStruct.freelists[i].count;
    blockCount -= sac->esacStruct.freelists[i].count / 3;
    sacClassFlush(sac, i, blockSize, (blockCount > 0) ? blockCount : 1);
    /* Leave the current one in the cache. */
    sac->esacStruct.freelists[i].count += 1;
    /* @@@@ ignoring shields for now */
    *ADDR_PTR(Addr, p) = sac->esacStruct.freelists[i].blocks;
    sac->esacStruct.freelists[i].blocks = p;
  } else {
    /* Free even the current one. */
    PoolFree(sac->pool, p, blockSize);
  }
}


/* SACFlush -- flush the cache, releasing all memory held in it */

void SACFlush(SAC sac)
{
  Index i, j;
  Size prevSize;

  AVERT(SAC, sac);

  for (j = sac->middleIndex + 1, i = 0;
       j < sac->classesCount; ++j, i += 2) {
    sacClassFlush(sac, i, sac->esacStruct.freelists[i].size,
                  sac->esacStruct.freelists[i].count);
    AVER(sac->esacStruct.freelists[i].blocks == NULL);
  }
  /* no need to flush overlarge, there's nothing there */
  prevSize = sac->esacStruct.middle;
  for (j = sac->middleIndex, i = 1; j > 0; --j, i += 2) {
    sacClassFlush(sac, i, prevSize, sac->esacStruct.freelists[i].count);
    AVER(sac->esacStruct.freelists[i].blocks == NULL);
    prevSize = sac->esacStruct.freelists[i].size;
  }
  /* flush smallest class */
  sacClassFlush(sac, i, prevSize, sac->esacStruct.freelists[i].count);
  AVER(sac->esacStruct.freelists[i].blocks == NULL);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
