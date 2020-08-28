/* sac.c: SEGREGATED ALLOCATION CACHES
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 */

#include "mpm.h"
#include "sac.h"

SRCID(sac, "$Id$");


typedef _mps_sac_freelist_block_s *SACFreeListBlock;


/* SACCheck -- check function for SACs */

static Bool sacFreeListBlockCheck(SACFreeListBlock fb)
{
  Count j;
  Addr cb;

  /* nothing to check about size */
  CHECKL(fb->_count <= fb->_count_max);
  /* check the freelist has the right number of blocks */
  for (j = 0, cb = fb->_blocks; j < fb->_count; ++j) {
    CHECKL(cb != NULL);
    /* @@@@ ignoring shields for now */
    cb = *ADDR_PTR(Addr, cb);
  }
  CHECKL(cb == NULL);
  return TRUE;
}

ATTRIBUTE_UNUSED
static Bool SACCheck(SAC sac)
{
  Index i, j;
  Bool b;
  Size prevSize;
  mps_sac_t esac;

  CHECKS(SAC, sac);
  esac = ExternalSACOfSAC(sac);
  CHECKU(Pool, sac->pool);
  CHECKL(sac->classesCount > 0);
  CHECKL(sac->classesCount > sac->middleIndex);
  CHECKL(BoolCheck(esac->_trapped));
  CHECKL(esac->_middle > 0);
  /* check classes above middle */
  prevSize = esac->_middle;
  for (j = sac->middleIndex + 1, i = 0; j < sac->classesCount; ++j, i += 2) {
    CHECKL(prevSize < esac->_freelists[i]._size);
    b = sacFreeListBlockCheck(&(esac->_freelists[i]));
    if (!b)
      return b;
    prevSize = esac->_freelists[i]._size;
  }
  /* check overlarge class */
  CHECKL(prevSize < esac->_freelists[i]._size);
  b = sacFreeListBlockCheck(&(esac->_freelists[i]));
  if (!b)
    return b;
  CHECKL(esac->_freelists[i]._size == SizeMAX);
  CHECKL(esac->_freelists[i]._count == 0);
  CHECKL(esac->_freelists[i]._count_max == 0);
  CHECKL(esac->_freelists[i]._blocks == NULL);
  /* check classes below middle */
  prevSize = esac->_middle;
  for (j = sac->middleIndex, i = 1; j > 0; --j, i += 2) {
    CHECKL(prevSize > esac->_freelists[i]._size);
    b = sacFreeListBlockCheck(&(esac->_freelists[i]));
    if (!b)
      return b;
    prevSize = esac->_freelists[i]._size;
  }
  /* check smallest class */
  CHECKL(prevSize > esac->_freelists[i]._size);
  CHECKL(esac->_freelists[i]._size == 0);
  b = sacFreeListBlockCheck(&(esac->_freelists[i]));
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
  return PointerOffset(&dummy, &dummy.esac_s._freelists[indexMax+1]);
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
  mps_sac_t esac;

  AVER(sacReturn != NULL);
  AVERT(Pool, pool);
  AVER(classesCount > 0);
  /* In this cache type, there is no upper limit on classesCount. */
  prevSize = sizeof(Addr) - 1; /* must large enough for freelist link */
  /* @@@@ It would be better to dynamically adjust the smallest class */
  /* to be large enough, but that gets complicated, if you have to */
  /* merge classes because of the adjustment. */
  for (i = 0; i < classesCount; ++i) {
    AVER(classes[i].mps_block_size > 0);
    AVER(SizeIsAligned(classes[i].mps_block_size, PoolAlignment(pool)));
    AVER(prevSize < classes[i].mps_block_size);
    prevSize = classes[i].mps_block_size;
    /* no restrictions on count */
    /* no restrictions on frequency */
  }

  /* Calculate frequency scale */
  for (i = 0; i < classesCount; ++i) {
    unsigned oldFreq = totalFreq;
    totalFreq += classes[i].mps_frequency;
    AVER(oldFreq <= totalFreq); /* check for overflow */
    UNUSED(oldFreq); /* <code/mpm.c#check.unused> */
  }

  /* Find middle one */
  totalFreq /= 2;
  for (i = 0; i < classesCount; ++i) {
    if (totalFreq < classes[i].mps_frequency)
      break;
    totalFreq -= classes[i].mps_frequency;
  }
  if (totalFreq <= classes[i].mps_frequency / 2)
    middleIndex = i;
  else
    middleIndex = i + 1; /* there must exist another class at i+1 */

  /* Allocate SAC */
  res = ControlAlloc(&p, PoolArena(pool), sacSize(middleIndex, classesCount));
  if(res != ResOK)
    goto failSACAlloc;
  sac = p;

  /* Move classes in place */
  /* It's important this matches SACFind. */
  esac = ExternalSACOfSAC(sac);
  for (j = middleIndex + 1, i = 0; j < classesCount; ++j, i += 2) {
    esac->_freelists[i]._size = classes[j].mps_block_size;
    esac->_freelists[i]._count = 0;
    esac->_freelists[i]._count_max = classes[j].mps_cached_count;
    esac->_freelists[i]._blocks = NULL;
  }
  esac->_freelists[i]._size = SizeMAX;
  esac->_freelists[i]._count = 0;
  esac->_freelists[i]._count_max = 0;
  esac->_freelists[i]._blocks = NULL;
  for (j = middleIndex, i = 1; j > 0; --j, i += 2) {
    esac->_freelists[i]._size = classes[j-1].mps_block_size;
    esac->_freelists[i]._count = 0;
    esac->_freelists[i]._count_max = classes[j].mps_cached_count;
    esac->_freelists[i]._blocks = NULL;
  }
  esac->_freelists[i]._size = 0;
  esac->_freelists[i]._count = 0;
  esac->_freelists[i]._count_max = classes[j].mps_cached_count;
  esac->_freelists[i]._blocks = NULL;

  /* finish init */
  esac->_trapped = FALSE;
  esac->_middle = classes[middleIndex].mps_block_size;
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
  mps_sac_t esac;

  esac = ExternalSACOfSAC(sac);
  if (size > esac->_middle) {
    i = 0; j = sac->middleIndex + 1;
    AVER(j <= sac->classesCount);
    while (size > esac->_freelists[i]._size) {
      AVER(j < sac->classesCount);
      i += 2; ++j;
    }
    *blockSizeReturn = esac->_freelists[i]._size;
  } else {
    Size prevSize = esac->_middle;

    i = 1; j = sac->middleIndex;
    while (size <= esac->_freelists[i]._size) {
      AVER(j > 0);
      prevSize = esac->_freelists[i]._size;
      i += 2; --j;
    }
    *blockSizeReturn = prevSize;
  }
  *iReturn = i;
}


/* SACFill -- alloc an object, and perhaps fill the cache */

Res SACFill(Addr *p_o, SAC sac, Size size)
{
  Index i;
  Count blockCount, j;
  Size blockSize;
  Addr p, fl;
  Res res = ResOK; /* stop compiler complaining */
  mps_sac_t esac;

  AVER(p_o != NULL);
  AVERT(SAC, sac);
  AVER(size != 0);
  esac = ExternalSACOfSAC(sac);

  sacFind(&i, &blockSize, sac, size);
  /* Check it's empty (in the future, there will be other cases). */
  AVER(esac->_freelists[i]._count == 0);

  /* Fill 1/3 of the cache for this class. */
  blockCount = esac->_freelists[i]._count_max / 3;
  /* Adjust size for the overlarge class. */
  if (blockSize == SizeMAX)
    /* .align: align 'cause some classes don't accept unaligned. */
    blockSize = SizeAlignUp(size, PoolAlignment(sac->pool));
  for (j = 0, fl = esac->_freelists[i]._blocks;
       j <= blockCount; ++j) {
    res = PoolAlloc(&p, sac->pool, blockSize);
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
  esac->_freelists[i]._count = j - 1;
  *p_o = fl;
  /* @@@@ ignoring shields for now */
  esac->_freelists[i]._blocks = *ADDR_PTR(Addr, fl);
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
  mps_sac_t esac;

  esac = ExternalSACOfSAC(sac);
  for (j = 0, fl = esac->_freelists[i]._blocks;
       j < blockCount; ++j) {
    /* @@@@ ignoring shields for now */
    cb = fl; fl = *ADDR_PTR(Addr, cb);
    PoolFree(sac->pool, cb, blockSize);
  }
  esac->_freelists[i]._count -= blockCount;
  esac->_freelists[i]._blocks = fl;
}


/* SACEmpty -- free an object, and perhaps empty the cache */

void SACEmpty(SAC sac, Addr p, Size size)
{
  Index i;
  Size blockSize;
  mps_sac_t esac;

  AVERT(SAC, sac);
  AVER(p != NULL);
  AVER(PoolHasAddr(sac->pool, p));
  AVER(size > 0);
  esac = ExternalSACOfSAC(sac);

  sacFind(&i, &blockSize, sac, size);
  /* Check it's full (in the future, there will be other cases). */
  AVER(esac->_freelists[i]._count
       == esac->_freelists[i]._count_max);

  /* Adjust size for the overlarge class. */
  if (blockSize == SizeMAX)
    /* see .align */
    blockSize = SizeAlignUp(size, PoolAlignment(sac->pool));
  if (esac->_freelists[i]._count_max > 0) {
    Count blockCount;

    /* Flush 2/3 of the cache for this class. */
    /* Computed as count - count/3, so that the rounding works out right. */
    blockCount = esac->_freelists[i]._count;
    blockCount -= esac->_freelists[i]._count / 3;
    sacClassFlush(sac, i, blockSize, (blockCount > 0) ? blockCount : 1);
    /* Leave the current one in the cache. */
    esac->_freelists[i]._count += 1;
    /* @@@@ ignoring shields for now */
    *ADDR_PTR(Addr, p) = esac->_freelists[i]._blocks;
    esac->_freelists[i]._blocks = p;
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
  mps_sac_t esac;

  AVERT(SAC, sac);

  esac = ExternalSACOfSAC(sac);
  for (j = sac->middleIndex + 1, i = 0;
       j < sac->classesCount; ++j, i += 2) {
    sacClassFlush(sac, i, esac->_freelists[i]._size,
                  esac->_freelists[i]._count);
    AVER(esac->_freelists[i]._blocks == NULL);
  }
  /* no need to flush overlarge, there's nothing there */
  prevSize = esac->_middle;
  for (j = sac->middleIndex, i = 1; j > 0; --j, i += 2) {
    sacClassFlush(sac, i, prevSize, esac->_freelists[i]._count);
    AVER(esac->_freelists[i]._blocks == NULL);
    prevSize = esac->_freelists[i]._size;
  }
  /* flush smallest class */
  sacClassFlush(sac, i, prevSize, esac->_freelists[i]._count);
  AVER(esac->_freelists[i]._blocks == NULL);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
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
