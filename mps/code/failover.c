/* failover.c: FAILOVER IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2014 Ravenbrook Limited.  See end of file for license.
 *
 * .design: <design/failover/>
 */

#include "failover.h"
#include "mpm.h"
#include "range.h"

SRCID(failover, "$Id$");


#define failoverOfLand(land) PARENT(FailoverStruct, landStruct, land)


ARG_DEFINE_KEY(failover_primary, Pointer);
ARG_DEFINE_KEY(failover_secondary, Pointer);


Bool FailoverCheck(Failover fo)
{
  CHECKS(Failover, fo);
  CHECKD(Land, &fo->landStruct);
  CHECKD(Land, fo->primary);
  CHECKD(Land, fo->secondary);
  return TRUE;
}


static Res failoverInit(Land land, ArgList args)
{
  Failover fo;
  LandClass super;
  Land primary, secondary;
  ArgStruct arg;
  Res res;

  AVERT(Land, land);
  super = LAND_SUPERCLASS(FailoverLandClass);
  res = (*super->init)(land, args);
  if (res != ResOK)
    return res;

  ArgRequire(&arg, args, FailoverPrimary);
  primary = arg.val.p;
  ArgRequire(&arg, args, FailoverSecondary);
  secondary = arg.val.p;

  fo = failoverOfLand(land);
  fo->primary = primary;
  fo->secondary = secondary;
  fo->sig = FailoverSig;
  AVERT(Failover, fo);
  return ResOK;
}


static void failoverFinish(Land land)
{
  Failover fo;

  AVERT(Land, land);
  fo = failoverOfLand(land);
  AVERT(Failover, fo);

  fo->sig = SigInvalid;
}


static Size failoverSize(Land land)
{
  Failover fo;

  AVERT(Land, land);
  fo = failoverOfLand(land);
  AVERT(Failover, fo);

  return LandSize(fo->primary) + LandSize(fo->secondary);
}


static Res failoverInsert(Range rangeReturn, Land land, Range range)
{
  Failover fo;
  Res res;

  AVER(rangeReturn != NULL);
  AVERT(Land, land);
  fo = failoverOfLand(land);
  AVERT(Failover, fo);
  AVERT(Range, range);

  /* Provide more opportunities for coalescence. See
   * <design/failover/#impl.assume.flush>.
   */
  (void)LandFlush(fo->primary, fo->secondary);

  res = LandInsert(rangeReturn, fo->primary, range);
  if (res != ResOK && res != ResFAIL)
    res = LandInsert(rangeReturn, fo->secondary, range);

  return res;
}


static Res failoverDelete(Range rangeReturn, Land land, Range range)
{
  Failover fo;
  Res res;
  RangeStruct oldRange, dummyRange, left, right;

  AVER(rangeReturn != NULL);
  AVERT(Land, land);
  fo = failoverOfLand(land);
  AVERT(Failover, fo);
  AVERT(Range, range);

  /* Prefer efficient search in the primary. See
   * <design/failover/#impl.assume.flush>.
   */
  (void)LandFlush(fo->primary, fo->secondary);

  res = LandDelete(&oldRange, fo->primary, range);

  if (res == ResFAIL) {
    /* Range not found in primary: try secondary. */
    return LandDelete(rangeReturn, fo->secondary, range);
  } else if (res != ResOK) {
    /* Range was found in primary, but couldn't be deleted. The only
     * case we expect to encounter here is the case where the primary
     * is out of memory. (In particular, we don't handle the case of a
     * CBS returning ResLIMIT because its block pool has been
     * configured not to automatically extend itself.)
     */
    AVER(ResIsAllocFailure(res));

    /* Delete the whole of oldRange, and re-insert the fragments
     * (which might end up in the secondary). See
     * <design/failover/#impl.assume.delete>.
     */
    res = LandDelete(&dummyRange, fo->primary, &oldRange);
    if (res != ResOK)
      return res;

    AVER(RangesEqual(&oldRange, &dummyRange));
    RangeInit(&left, RangeBase(&oldRange), RangeBase(range));
    if (!RangeIsEmpty(&left)) {
      /* Don't call LandInsert(..., land, ...) here: that would be
       * re-entrant and fail the landEnter check. */
      res = LandInsert(&dummyRange, fo->primary, &left);
      if (res != ResOK) {
        /* The range was successful deleted from the primary above. */
        AVER(res != ResFAIL);
        res = LandInsert(&dummyRange, fo->secondary, &left);
        AVER(res == ResOK);
      }
    }
    RangeInit(&right, RangeLimit(range), RangeLimit(&oldRange));
    if (!RangeIsEmpty(&right)) {
      res = LandInsert(&dummyRange, fo->primary, &right);
      if (res != ResOK) {
        /* The range was successful deleted from the primary above. */
        AVER(res != ResFAIL);
        res = LandInsert(&dummyRange, fo->secondary, &right);
        AVER(res == ResOK);
      }
    }
  }
  if (res == ResOK) {
    AVER(RangesNest(&oldRange, range));
    RangeCopy(rangeReturn, &oldRange);
  }
  return res;
}


static Bool failoverIterate(Land land, LandVisitor visitor, void *closureP, Size closureS)
{
  Failover fo;

  AVERT(Land, land);
  fo = failoverOfLand(land);
  AVERT(Failover, fo);
  AVER(visitor != NULL);

  return LandIterate(fo->primary, visitor, closureP, closureS)
    && LandIterate(fo->secondary, visitor, closureP, closureS);
}


static Bool failoverFindFirst(Range rangeReturn, Range oldRangeReturn, Land land, Size size, FindDelete findDelete)
{
  Failover fo;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Land, land);
  fo = failoverOfLand(land);
  AVERT(Failover, fo);
  AVERT(FindDelete, findDelete);

  /* See <design/failover/#impl.assume.flush>. */
  (void)LandFlush(fo->primary, fo->secondary);

  return LandFindFirst(rangeReturn, oldRangeReturn, fo->primary, size, findDelete)
    || LandFindFirst(rangeReturn, oldRangeReturn, fo->secondary, size, findDelete);
}


static Bool failoverFindLast(Range rangeReturn, Range oldRangeReturn, Land land, Size size, FindDelete findDelete)
{
  Failover fo;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Land, land);
  fo = failoverOfLand(land);
  AVERT(Failover, fo);
  AVERT(FindDelete, findDelete);

  /* See <design/failover/#impl.assume.flush>. */
  (void)LandFlush(fo->primary, fo->secondary);

  return LandFindLast(rangeReturn, oldRangeReturn, fo->primary, size, findDelete)
    || LandFindLast(rangeReturn, oldRangeReturn, fo->secondary, size, findDelete);
}


static Bool failoverFindLargest(Range rangeReturn, Range oldRangeReturn, Land land, Size size, FindDelete findDelete)
{
  Failover fo;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Land, land);
  fo = failoverOfLand(land);
  AVERT(Failover, fo);
  AVERT(FindDelete, findDelete);

  /* See <design/failover/#impl.assume.flush>. */
  (void)LandFlush(fo->primary, fo->secondary);

  return LandFindLargest(rangeReturn, oldRangeReturn, fo->primary, size, findDelete)
    || LandFindLargest(rangeReturn, oldRangeReturn, fo->secondary, size, findDelete);
}


static Bool failoverFindInZones(Bool *foundReturn, Range rangeReturn, Range oldRangeReturn, Land land, Size size, ZoneSet zoneSet, Bool high)
{
  Failover fo;
  Bool found = FALSE;
  Res res;

  AVER(FALSE); /* TODO: this code is completely untested! */
  AVER(foundReturn != NULL);
  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Land, land);
  fo = failoverOfLand(land);
  AVERT(Failover, fo);
  /* AVERT(ZoneSet, zoneSet); */
  AVERT(Bool, high);

  /* See <design/failover/#impl.assume.flush>. */
  (void)LandFlush(fo->primary, fo->secondary);

  res = LandFindInZones(&found, rangeReturn, oldRangeReturn, fo->primary, size, zoneSet, high);
  if (res != ResOK || !found)
    res = LandFindInZones(&found, rangeReturn, oldRangeReturn, fo->secondary, size, zoneSet, high);

  *foundReturn = found;
  return res;
}


static Res failoverDescribe(Land land, mps_lib_FILE *stream)
{
  Failover fo;
  Res res;

  if (!TESTT(Land, land)) return ResFAIL;
  fo = failoverOfLand(land);
  if (!TESTT(Failover, fo)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  res = WriteF(stream,
               "Failover $P {\n", (WriteFP)fo,
               "  primary = $P ($S)\n", (WriteFP)fo->primary,
               fo->primary->class->name,
               "  secondary = $P ($S)\n", (WriteFP)fo->secondary,
               fo->secondary->class->name,
               "}\n", NULL);

  return res;
}


DEFINE_LAND_CLASS(FailoverLandClass, class)
{
  INHERIT_CLASS(class, LandClass);
  class->name = "FAILOVER";
  class->size = sizeof(FailoverStruct);
  class->init = failoverInit;
  class->finish = failoverFinish;
  class->sizeMethod = failoverSize;
  class->insert = failoverInsert;
  class->delete = failoverDelete;
  class->iterate = failoverIterate;
  class->findFirst = failoverFindFirst;
  class->findLast = failoverFindLast;
  class->findLargest = failoverFindLargest;
  class->findInZones = failoverFindInZones;
  class->describe = failoverDescribe;
  AVERT(LandClass, class);
}


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
