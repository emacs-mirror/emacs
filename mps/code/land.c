/* land.c: LAND (COLLECTION OF ADDRESS RANGES) IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2014-2020 Ravenbrook Limited.  See end of file for license.
 *
 * .design: <design/land>
 *
 * .critical.macros: In manual-allocation-bound programs using MVFF,
 * the Land generic functions are on the critical path via mps_free.
 * In non-checking varieties we provide macro alternatives (in mpm.h)
 * to these functions that call the underlying methods directly,
 * giving a few percent improvement in performance but skipping the
 * re-entrancy checking provided by landEnter and landLeave.
 */

#include "mpm.h"
#include "range.h"

SRCID(land, "$Id$");


/* Forward declarations */

static Res landNoInsert(Range rangeReturn, Land land, Range range);
static Res landNoDelete(Range rangeReturn, Land land, Range range);


/* FindDeleteCheck -- check method for a FindDelete value */

Bool FindDeleteCheck(FindDelete findDelete)
{
  CHECKL(findDelete == FindDeleteNONE
         || findDelete == FindDeleteLOW
         || findDelete == FindDeleteHIGH
         || findDelete == FindDeleteENTIRE);
  UNUSED(findDelete); /* <code/mpm.c#check.unused> */

  return TRUE;
}


/* landEnter, landLeave -- Avoid re-entrance
 *
 * .enter-leave: The visitor functions passed to LandIterate and
 * LandIterateAndDelete are not allowed to call methods of that land.
 * These functions enforce this.
 *
 * .enter-leave.simple: Some simple queries are fine to call from
 * visitor functions. These are marked with the tag of this comment.
 */

static void landEnter(Land land)
{
  /* Don't need to check as always called from interface function. */
  AVER(!land->inLand);
  land->inLand = TRUE;
}

static void landLeave(Land land)
{
  /* Don't need to check as always called from interface function. */
  AVER(land->inLand);
  land->inLand = FALSE;
}


/* LandCheck -- check land */

Bool LandCheck(Land land)
{
  LandClass klass;
  /* .enter-leave.simple */
  CHECKS(Land, land);
  CHECKC(Land, land);
  klass = ClassOfPoly(Land, land);
  CHECKD(LandClass, klass);
  CHECKU(Arena, land->arena);
  CHECKL(AlignCheck(land->alignment));
  CHECKL(BoolCheck(land->inLand));
  return TRUE;
}

static Res LandAbsInit(Land land, Arena arena, Align alignment, ArgList args)
{
  AVER(land != NULL);
  AVERT(Arena, arena);
  AVERT(Align, alignment);
  UNUSED(args);

  /* Superclass init */
  InstInit(CouldBeA(Inst, land));

  land->inLand = TRUE;
  land->alignment = alignment;
  land->arena = arena;

  SetClassOfPoly(land, CLASS(Land));
  land->sig = LandSig;
  AVERC(Land, land);

  return ResOK;
}

static void LandAbsFinish(Inst inst)
{
  Land land = MustBeA(Land, inst);
  AVERC(Land, land);
  land->sig = SigInvalid;
  NextMethod(Inst, Land, finish)(inst);
}


/* LandInit -- initialize land
 *
 * <design/land#.function.init>
 */

Res LandInit(Land land, LandClass klass, Arena arena, Align alignment, void *owner, ArgList args)
{
  Res res;

  AVER(land != NULL);
  AVERT(LandClass, klass);
  AVERT(Align, alignment);

  res = klass->init(land, arena, alignment, args);
  if (res != ResOK)
    return res;

  EVENT2(LandInit, land, owner);
  landLeave(land);
  return ResOK;
}


/* LandFinish -- finish land
 *
 * <design/land#.function.finish>
 */

void LandFinish(Land land)
{
  AVERC(Land, land);
  landEnter(land);

  Method(Inst, land, finish)(MustBeA(Inst, land));
}


/* LandSize -- return the total size of ranges in land
 *
 * <design/land#.function.size>
 */

Size (LandSize)(Land land)
{
  /* .enter-leave.simple */
  AVERC(Land, land);

  return LandSizeMacro(land);
}


/* LandInsert -- insert range of addresses into land
 *
 * <design/land#.function.insert>
 */

Res (LandInsert)(Range rangeReturn, Land land, Range range)
{
  Res res;

  AVER(rangeReturn != NULL);
  AVERC(Land, land);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, land->alignment));
  AVER(!RangeIsEmpty(range));
  landEnter(land);

  res = LandInsertMacro(rangeReturn, land, range);

  landLeave(land);
  return res;
}


/* LandInsertSteal -- insert range of addresses into land, possibly
 * stealing some of the inserted memory to allocate internal data
 * structures.
 *
 * <design/land#.function.insert-steal>
 */

Res LandInsertSteal(Range rangeReturn, Land land, Range rangeIO)
{
  Res res;

  AVER(rangeReturn != NULL);
  AVERC(Land, land);
  AVER(rangeReturn != rangeIO);
  AVERT(Range, rangeIO);
  AVER(RangeIsAligned(rangeIO, land->alignment));
  AVER(!RangeIsEmpty(rangeIO));

  landEnter(land);

  res = Method(Land, land, insertSteal)(rangeReturn, land, rangeIO);

  landLeave(land);
  return res;
}


/* LandDelete -- delete range of addresses from land
 *
 * <design/land#.function.delete>
 */

Res (LandDelete)(Range rangeReturn, Land land, Range range)
{
  Res res;

  AVER(rangeReturn != NULL);
  AVERC(Land, land);
  AVERT(Range, range);
  AVER(!RangeIsEmpty(range));
  AVER(RangeIsAligned(range, land->alignment));
  landEnter(land);

  res = LandDeleteMacro(rangeReturn, land, range);

  landLeave(land);
  return res;
}


/* LandDeleteSteal -- delete range of addresses from land, possibly
 * stealing some memory from the land to allocate internal data
 * structures.
 *
 * <design/land#.function.delete-steal>
 */

Res LandDeleteSteal(Range rangeReturn, Land land, Range range)
{
  Res res;

  AVER(rangeReturn != NULL);
  AVERC(Land, land);
  AVERT(Range, range);
  AVER(!RangeIsEmpty(range));
  AVER(RangeIsAligned(range, land->alignment));
  landEnter(land);

  res = Method(Land, land, deleteSteal)(rangeReturn, land, range);

  landLeave(land);
  return res;
}


/* LandIterate -- iterate over isolated ranges of addresses in land
 *
 * <design/land#.function.iterate>
 */

Bool (LandIterate)(Land land, LandVisitor visitor, void *closure)
{
  Bool b;
  AVERC(Land, land);
  AVER(FUNCHECK(visitor));
  landEnter(land);

  b = LandIterateMacro(land, visitor, closure);

  landLeave(land);
  return b;
}


/* LandIterateAndDelete -- iterate over isolated ranges of addresses
 * in land, deleting some of them
 *
 * <design/land#.function.iterate.and.delete>
 */

Bool (LandIterateAndDelete)(Land land, LandDeleteVisitor visitor, void *closure)
{
  Bool b;
  AVERC(Land, land);
  AVER(FUNCHECK(visitor));
  landEnter(land);

  b = LandIterateAndDeleteMacro(land, visitor, closure);

  landLeave(land);
  return b;
}


/* LandFindFirst -- find first range of given size
 *
 * <design/land#.function.find.first>
 */

Bool (LandFindFirst)(Range rangeReturn, Range oldRangeReturn, Land land, Size size, FindDelete findDelete)
{
  Bool b;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERC(Land, land);
  AVER(SizeIsAligned(size, land->alignment));
  AVERT(FindDelete, findDelete);
  landEnter(land);

  b = LandFindFirstMacro(rangeReturn, oldRangeReturn, land, size, findDelete);

  landLeave(land);
  return b;
}


/* LandFindLast -- find last range of given size
 *
 * <design/land#.function.find.last>
 */

Bool (LandFindLast)(Range rangeReturn, Range oldRangeReturn, Land land, Size size, FindDelete findDelete)
{
  Bool b;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERC(Land, land);
  AVER(SizeIsAligned(size, land->alignment));
  AVERT(FindDelete, findDelete);
  landEnter(land);

  b = LandFindLastMacro(rangeReturn, oldRangeReturn, land, size, findDelete);

  landLeave(land);
  return b;
}


/* LandFindLargest -- find largest range of at least given size
 *
 * <design/land#.function.find.largest>
 */

Bool (LandFindLargest)(Range rangeReturn, Range oldRangeReturn, Land land, Size size, FindDelete findDelete)
{
  Bool b;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERC(Land, land);
  AVER(SizeIsAligned(size, land->alignment));
  AVERT(FindDelete, findDelete);
  landEnter(land);

  b = LandFindLargestMacro(rangeReturn, oldRangeReturn, land, size, findDelete);

  landLeave(land);
  return b;
}


/* LandFindInSize -- find range of given size in set of zones
 *
 * <design/land#.function.find.zones>
 */

Res (LandFindInZones)(Bool *foundReturn, Range rangeReturn, Range oldRangeReturn, Land land, Size size, ZoneSet zoneSet, Bool high)
{
  Res res;

  AVER(foundReturn != NULL);
  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERC(Land, land);
  AVER(SizeIsAligned(size, land->alignment));
  /* AVER(ZoneSet, zoneSet); */
  AVERT(Bool, high);
  landEnter(land);

  res = LandFindInZonesMacro(foundReturn, rangeReturn, oldRangeReturn,
                             land, size, zoneSet, high);

  landLeave(land);
  return res;
}


/* LandDescribe -- describe land for debugging
 *
 * <design/land#.function.describe>
 */

Res LandDescribe(Land land, mps_lib_FILE *stream, Count depth)
{
  return Method(Inst, land, describe)(MustBeA(Inst, land), stream, depth);
}


/* landFlushVisitor -- visitor for LandFlush.
 *
 * closure argument is the destination Land. Attempt to insert the
 * range into the destination.
 *
 * .flush.critical: In manual-allocation-bound programs using MVFF
 * this is on the critical paths via mps_alloc (and then PoolAlloc,
 * MVFFAlloc, failoverFind*, LandFlush) and mps_free (and then
 * MVFFFree, failoverInsert, LandFlush).
 */
Bool LandFlushVisitor(Bool *deleteReturn, Land land, Range range,
                      void *closure)
{
  Res res;
  RangeStruct newRange;
  Land dest;

  AVER_CRITICAL(deleteReturn != NULL);
  AVERC_CRITICAL(Land, land);
  AVERT_CRITICAL(Range, range);
  AVER_CRITICAL(closure != NULL);

  dest = MustBeA_CRITICAL(Land, closure);
  res = LandInsert(&newRange, dest, range);
  if (res == ResOK) {
    *deleteReturn = TRUE;
    return TRUE;
  } else {
    *deleteReturn = FALSE;
    return FALSE;
  }
}


/* LandFlush -- move ranges from src to dest
 *
 * <design/land#.function.flush>
 */

Bool (LandFlush)(Land dest, Land src)
{
  AVERC(Land, dest);
  AVERC(Land, src);

  return LandFlushMacro(dest, src);
}


/* LandClassCheck -- check land class */

Bool LandClassCheck(LandClass klass)
{
  CHECKL(InstClassCheck(&klass->instClassStruct));
  CHECKL(klass->size >= sizeof(LandStruct));
  CHECKL(FUNCHECK(klass->init));
  CHECKL(FUNCHECK(klass->insert));
  CHECKL(FUNCHECK(klass->delete));
  CHECKL(FUNCHECK(klass->findFirst));
  CHECKL(FUNCHECK(klass->findLast));
  CHECKL(FUNCHECK(klass->findLargest));
  CHECKL(FUNCHECK(klass->findInZones));

  /* Check that land classes override sets of related methods. */
  CHECKL((klass->init == LandAbsInit)
         == (klass->instClassStruct.finish == LandAbsFinish));
  CHECKL((klass->insert == landNoInsert) == (klass->delete == landNoDelete));

  CHECKS(LandClass, klass);
  return TRUE;
}


static Size landNoSize(Land land)
{
  UNUSED(land);
  NOTREACHED;
  return 0;
}

/* LandSlowSize -- generic size method but slow */

static Bool landSizeVisitor(Land land, Range range,
                            void *closure)
{
  Size *size;

  AVERC(Land, land);
  AVERT(Range, range);
  AVER(closure != NULL);

  size = closure;
  *size += RangeSize(range);

  return TRUE;
}

Size LandSlowSize(Land land)
{
  Size size = 0;
  Bool b = LandIterate(land, landSizeVisitor, &size);
  AVER(b);
  return size;
}

static Res landNoInsert(Range rangeReturn, Land land, Range range)
{
  AVER(rangeReturn != NULL);
  AVERC(Land, land);
  AVERT(Range, range);
  return ResUNIMPL;
}

static Res landNoDelete(Range rangeReturn, Land land, Range range)
{
  AVER(rangeReturn != NULL);
  AVERC(Land, land);
  AVERT(Range, range);
  return ResUNIMPL;
}

static Bool landNoIterate(Land land, LandVisitor visitor, void *closure)
{
  AVERC(Land, land);
  AVER(visitor != NULL);
  UNUSED(closure);
  return FALSE;
}

static Bool landNoIterateAndDelete(Land land, LandDeleteVisitor visitor, void *closure)
{
  AVERC(Land, land);
  AVER(visitor != NULL);
  UNUSED(closure);
  return FALSE;
}

static Bool landNoFind(Range rangeReturn, Range oldRangeReturn, Land land, Size size, FindDelete findDelete)
{
  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERC(Land, land);
  UNUSED(size);
  AVERT(FindDelete, findDelete);
  return ResUNIMPL;
}

static Res landNoFindInZones(Bool *foundReturn, Range rangeReturn, Range oldRangeReturn, Land land, Size size, ZoneSet zoneSet, Bool high)
{
  AVER(foundReturn != NULL);
  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERC(Land, land);
  UNUSED(size);
  UNUSED(zoneSet);
  AVERT(Bool, high);
  return ResUNIMPL;
}

static Res LandAbsDescribe(Inst inst, mps_lib_FILE *stream, Count depth)
{
  Land land = CouldBeA(Land, inst);
  LandClass klass;
  Res res;

  if (!TESTC(Land, land))
    return ResPARAM;
  if (stream == NULL)
    return ResPARAM;

  res = NextMethod(Inst, Land, describe)(inst, stream, depth);
  if (res != ResOK)
    return res;

  klass = ClassOfPoly(Land, land);
  return WriteF(stream, depth + 2,
                "class $P (\"$S\")\n",
                (WriteFP)klass, (WriteFS)ClassName(klass),
                "arena  $P\n", (WriteFP)land->arena,
                "align  $U\n", (WriteFU)land->alignment,
                "inLand $S\n", WriteFYesNo(land->inLand),
                NULL);
}

DEFINE_CLASS(Inst, LandClass, klass)
{
  INHERIT_CLASS(klass, LandClass, InstClass);
  AVERT(InstClass, klass);
}

DEFINE_CLASS(Land, Land, klass)
{
  INHERIT_CLASS(&klass->instClassStruct, Land, Inst);
  klass->instClassStruct.describe = LandAbsDescribe;
  klass->instClassStruct.finish = LandAbsFinish;
  klass->size = sizeof(LandStruct);
  klass->init = LandAbsInit;
  klass->sizeMethod = landNoSize;
  klass->insert = landNoInsert;
  klass->insertSteal = landNoInsert;
  klass->delete = landNoDelete;
  klass->deleteSteal = landNoDelete;
  klass->iterate = landNoIterate;
  klass->iterateAndDelete = landNoIterateAndDelete;
  klass->findFirst = landNoFind;
  klass->findLast = landNoFind;
  klass->findLargest = landNoFind;
  klass->findInZones = landNoFindInZones;
  klass->sig = LandClassSig;
  AVERT(LandClass, klass);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2014-2020 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
