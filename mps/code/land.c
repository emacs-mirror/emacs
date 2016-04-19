/* land.c: LAND (COLLECTION OF ADDRESS RANGES) IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2014-2015 Ravenbrook Limited.  See end of file for license.
 *
 * .design: <design/land/>
 */

#include "mpm.h"
#include "range.h"

SRCID(land, "$Id$");


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
  return;
}

static void landLeave(Land land)
{
  /* Don't need to check as always called from interface function. */
  AVER(land->inLand);
  land->inLand = FALSE;
  return;
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

static void LandAbsFinish(Land land)
{
  AVERC(Land, land);
  land->sig = SigInvalid;
  InstFinish(CouldBeA(Inst, land));
}


/* LandInit -- initialize land
 *
 * See <design/land/#function.init>
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


/* LandCreate -- allocate and initialize land
 *
 * See <design/land/#function.create>
 */

Res LandCreate(Land *landReturn, Arena arena, LandClass klass, Align alignment, void *owner, ArgList args)
{
  Res res;
  Land land;
  void *p;

  AVER(landReturn != NULL);
  AVERT(Arena, arena);
  AVERT(LandClass, klass);

  res = ControlAlloc(&p, arena, klass->size);
  if (res != ResOK)
    goto failAlloc;
  land = p;

  res = LandInit(land, klass, arena, alignment, owner, args);
  if (res != ResOK)
    goto failInit;

  *landReturn = land;
  return ResOK;

failInit:
  ControlFree(arena, land, klass->size);
failAlloc:
  return res;
}


/* LandDestroy -- finish and deallocate land
 *
 * See <design/land/#function.destroy>
 */

void LandDestroy(Land land)
{
  Arena arena;
  Size size;

  AVERC(Land, land);
  arena = land->arena;
  size = ClassOfPoly(Land, land)->size;
  LandFinish(land);
  ControlFree(arena, land, size);
}


/* LandFinish -- finish land
 *
 * See <design/land/#function.finish>
 */

void LandFinish(Land land)
{
  AVERC(Land, land);
  landEnter(land);

  Method(Land, land, finish)(land);
}


/* LandSize -- return the total size of ranges in land
 *
 * See <design/land/#function.size>
 */

Size LandSize(Land land)
{
  /* .enter-leave.simple */
  AVERC(Land, land);

  return Method(Land, land, sizeMethod)(land);
}


/* LandInsert -- insert range of addresses into land
 *
 * See <design/land/#function.insert>
 */

Res LandInsert(Range rangeReturn, Land land, Range range)
{
  Res res;

  AVER(rangeReturn != NULL);
  AVERC(Land, land);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, land->alignment));
  landEnter(land);

  res = Method(Land, land, insert)(rangeReturn, land, range);

  landLeave(land);
  return res;
}


/* LandDelete -- delete range of addresses from land
 *
 * See <design/land/#function.delete>
 */

Res LandDelete(Range rangeReturn, Land land, Range range)
{
  Res res;

  AVER(rangeReturn != NULL);
  AVERC(Land, land);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, land->alignment));
  landEnter(land);

  res = Method(Land, land, delete)(rangeReturn, land, range);

  landLeave(land);
  return res;
}


/* LandIterate -- iterate over isolated ranges of addresses in land
 *
 * See <design/land/#function.iterate>
 */

Bool LandIterate(Land land, LandVisitor visitor, void *closure)
{
  Bool b;
  AVERC(Land, land);
  AVER(FUNCHECK(visitor));
  landEnter(land);

  b = Method(Land, land, iterate)(land, visitor, closure);

  landLeave(land);
  return b;
}


/* LandIterateAndDelete -- iterate over isolated ranges of addresses
 * in land, deleting some of them
 *
 * See <design/land/#function.iterate.and.delete>
 */

Bool LandIterateAndDelete(Land land, LandDeleteVisitor visitor, void *closure)
{
  Bool b;
  AVERC(Land, land);
  AVER(FUNCHECK(visitor));
  landEnter(land);

  b = Method(Land, land, iterateAndDelete)(land, visitor, closure);

  landLeave(land);
  return b;
}


/* LandFindFirst -- find first range of given size
 *
 * See <design/land/#function.find.first>
 */

Bool LandFindFirst(Range rangeReturn, Range oldRangeReturn, Land land, Size size, FindDelete findDelete)
{
  Bool b;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERC(Land, land);
  AVER(SizeIsAligned(size, land->alignment));
  AVERT(FindDelete, findDelete);
  landEnter(land);

  b = Method(Land, land, findFirst)(rangeReturn, oldRangeReturn, land, size,
                                findDelete);

  landLeave(land);
  return b;
}


/* LandFindLast -- find last range of given size
 *
 * See <design/land/#function.find.last>
 */

Bool LandFindLast(Range rangeReturn, Range oldRangeReturn, Land land, Size size, FindDelete findDelete)
{
  Bool b;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERC(Land, land);
  AVER(SizeIsAligned(size, land->alignment));
  AVERT(FindDelete, findDelete);
  landEnter(land);

  b = Method(Land, land, findLast)(rangeReturn, oldRangeReturn, land, size,
                               findDelete);

  landLeave(land);
  return b;
}


/* LandFindLargest -- find largest range of at least given size
 *
 * See <design/land/#function.find.largest>
 */

Bool LandFindLargest(Range rangeReturn, Range oldRangeReturn, Land land, Size size, FindDelete findDelete)
{
  Bool b;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERC(Land, land);
  AVER(SizeIsAligned(size, land->alignment));
  AVERT(FindDelete, findDelete);
  landEnter(land);

  b = Method(Land, land, findLargest)(rangeReturn, oldRangeReturn, land, size,
                                  findDelete);

  landLeave(land);
  return b;
}


/* LandFindInSize -- find range of given size in set of zones
 *
 * See <design/land/#function.find.zones>
 */

Res LandFindInZones(Bool *foundReturn, Range rangeReturn, Range oldRangeReturn, Land land, Size size, ZoneSet zoneSet, Bool high)
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

  res = Method(Land, land, findInZones)(foundReturn, rangeReturn, oldRangeReturn,
                                    land, size, zoneSet, high);

  landLeave(land);
  return res;
}


/* LandDescribe -- describe land for debugging
 *
 * See <design/land/#function.describe>
 */

Res LandDescribe(Land land, mps_lib_FILE *stream, Count depth)
{
  return Method(Land, land, describe)(land, stream, depth);
}


/* landFlushVisitor -- visitor for LandFlush.
 *
 * closure argument is the destination Land. Attempt to insert the
 * range into the destination.
 */
static Bool landFlushVisitor(Bool *deleteReturn, Land land, Range range,
                             void *closure)
{
  Res res;
  RangeStruct newRange;
  Land dest;

  AVER(deleteReturn != NULL);
  AVERC(Land, land);
  AVERT(Range, range);
  AVER(closure != NULL);

  dest = closure;
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
 * See <design/land/#function.flush>
 */

Bool LandFlush(Land dest, Land src)
{
  AVERC(Land, dest);
  AVERC(Land, src);

  return LandIterateAndDelete(src, landFlushVisitor, dest);
}


/* LandClassCheck -- check land class */

Bool LandClassCheck(LandClass klass)
{
  CHECKL(InstClassCheck(&klass->protocol));
  CHECKL(klass->size >= sizeof(LandStruct));
  CHECKL(FUNCHECK(klass->init));
  CHECKL(FUNCHECK(klass->finish));
  CHECKL(FUNCHECK(klass->insert));
  CHECKL(FUNCHECK(klass->delete));
  CHECKL(FUNCHECK(klass->findFirst));
  CHECKL(FUNCHECK(klass->findLast));
  CHECKL(FUNCHECK(klass->findLargest));
  CHECKL(FUNCHECK(klass->findInZones));
  CHECKL(FUNCHECK(klass->describe));
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

static Res LandAbsDescribe(Land land, mps_lib_FILE *stream, Count depth)
{
  LandClass klass;
  Res res;
  
  if (!TESTC(Land, land))
    return ResPARAM;
  if (stream == NULL)
    return ResPARAM;

  res = InstDescribe(CouldBeA(Inst, land), stream, depth);
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
}

DEFINE_CLASS(Land, Land, klass)
{
  INHERIT_CLASS(&klass->protocol, Land, Inst);
  klass->size = sizeof(LandStruct);
  klass->init = LandAbsInit;
  klass->sizeMethod = landNoSize;
  klass->finish = LandAbsFinish;
  klass->insert = landNoInsert;
  klass->delete = landNoDelete;
  klass->iterate = landNoIterate;
  klass->iterateAndDelete = landNoIterateAndDelete;
  klass->findFirst = landNoFind;
  klass->findLast = landNoFind;
  klass->findLargest = landNoFind;
  klass->findInZones = landNoFindInZones;
  klass->describe = LandAbsDescribe;
  klass->sig = LandClassSig;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2014-2015 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
