/* land.c: LAND (COLLECTION OF ADDRESS RANGES) IMPLEMENTATION
 *
 * $Id: //info.ravenbrook.com/project/mps/branch/2014-03-30/land/code/land.c#1 $
 * Copyright (c) 2014 Ravenbrook Limited.  See end of file for license.
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
 * .enter-leave: The visitor function passed to LandIterate is not
 * allowed to call methods of that land. These functions enforce this.
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
  /* .enter-leave.simple */
  CHECKS(Land, land);
  CHECKD(LandClass, land->class);
  CHECKU(Arena, land->arena);
  CHECKL(AlignCheck(land->alignment));
  return TRUE;
}


/* LandInit -- initialize land
 *
 * See <design/land/#function.init>
 */

Res LandInit(Land land, LandClass class, Arena arena, Align alignment, void *owner, ArgList args)
{
  Res res;

  AVER(land != NULL);
  AVERT(LandClass, class);
  AVERT(Align, alignment);

  land->inLand = TRUE;
  land->alignment = alignment;
  land->arena = arena;
  land->class = class;
  land->sig = LandSig;

  AVERT(Land, land);

  res = (*class->init)(land, args);
  if (res != ResOK)
    goto failInit;

  EVENT2(LandInit, land, owner);
  landLeave(land);
  return ResOK;

 failInit:
  land->sig = SigInvalid;
  return res;
}


/* LandCreate -- allocate and initialize land
 *
 * See <design/land/#function.create>
 */

Res LandCreate(Land *landReturn, Arena arena, LandClass class, Align alignment, void *owner, ArgList args)
{
  Res res;
  Land land;
  void *p;

  AVER(landReturn != NULL);
  AVERT(Arena, arena);
  AVERT(LandClass, class);

  res = ControlAlloc(&p, arena, class->size,
                     /* withReservoirPermit */ FALSE);
  if (res != ResOK)
    goto failAlloc;
  land = p;

  res = LandInit(land, class, arena, alignment, owner, args);
  if (res != ResOK)
    goto failInit;

  *landReturn = land;
  return ResOK;

failInit:
  ControlFree(arena, land, class->size);
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
  LandClass class;

  AVERT(Land, land);
  arena = land->arena;
  class = land->class;
  AVERT(LandClass, class);
  LandFinish(land);
  ControlFree(arena, land, class->size);
}


/* LandFinish -- finish land
 *
 * See <design/land/#function.finish>
 */

void LandFinish(Land land)
{
  AVERT(Land, land);
  landEnter(land);

  (*land->class->finish)(land);

  land->sig = SigInvalid;
}


/* LandSize -- return the total size of ranges in land
 *
 * See <design/land/#function.size>
 */

Size LandSize(Land land)
{
  /* .enter-leave.simple */
  AVERT(Land, land);

  return (*land->class->sizeMethod)(land);
}


/* LandInsert -- insert range of addresses into land
 *
 * See <design/land/#function.insert>
 */

Res LandInsert(Range rangeReturn, Land land, Range range)
{
  Res res;

  AVER(rangeReturn != NULL);
  AVERT(Land, land);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, land->alignment));
  landEnter(land);

  res = (*land->class->insert)(rangeReturn, land, range);

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
  AVERT(Land, land);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, land->alignment));
  landEnter(land);

  res = (*land->class->delete)(rangeReturn, land, range);

  landLeave(land);
  return res;
}


/* LandIterate -- iterate over isolated ranges of addresses in land
 *
 * See <design/land/#function.iterate>
 */

void LandIterate(Land land, LandVisitor visitor, void *closureP, Size closureS)
{
  AVERT(Land, land);
  AVER(FUNCHECK(visitor));
  landEnter(land);

  (*land->class->iterate)(land, visitor, closureP, closureS);

  landLeave(land);
}


/* LandFindFirst -- find first range of given size
 *
 * See <design/land/#function.find.first>
 */

Bool LandFindFirst(Range rangeReturn, Range oldRangeReturn, Land land, Size size, FindDelete findDelete)
{
  Bool res;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Land, land);
  AVER(SizeIsAligned(size, land->alignment));
  AVER(FindDeleteCheck(findDelete));
  landEnter(land);

  res = (*land->class->findFirst)(rangeReturn, oldRangeReturn, land, size,
                                  findDelete);

  landLeave(land);
  return res;
}


/* LandFindLast -- find last range of given size
 *
 * See <design/land/#function.find.last>
 */

Bool LandFindLast(Range rangeReturn, Range oldRangeReturn, Land land, Size size, FindDelete findDelete)
{
  Bool res;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Land, land);
  AVER(SizeIsAligned(size, land->alignment));
  AVER(FindDeleteCheck(findDelete));
  landEnter(land);

  res = (*land->class->findLast)(rangeReturn, oldRangeReturn, land, size,
                                 findDelete);

  landLeave(land);
  return res;
}


/* LandFindLargest -- find largest range of at least given size
 *
 * See <design/land/#function.find.largest>
 */

Bool LandFindLargest(Range rangeReturn, Range oldRangeReturn, Land land, Size size, FindDelete findDelete)
{
  Bool res;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Land, land);
  AVER(SizeIsAligned(size, land->alignment));
  AVER(FindDeleteCheck(findDelete));
  landEnter(land);

  res = (*land->class->findLargest)(rangeReturn, oldRangeReturn, land, size,
                                    findDelete);

  landLeave(land);
  return res;
}


/* LandFindInSize -- find range of given size in set of zones
 *
 * See <design/land/#function.find.zones>
 */

Res LandFindInZones(Range rangeReturn, Range oldRangeReturn, Land land, Size size, ZoneSet zoneSet, Bool high)
{
  Res res;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Land, land);
  AVER(SizeIsAligned(size, land->alignment));
  /* AVER(ZoneSet, zoneSet); */
  AVERT(Bool, high);
  landEnter(land);

  res = (*land->class->findInZones)(rangeReturn, oldRangeReturn, land, size,
                                    zoneSet, high);

  landLeave(land);
  return res;
}


/* LandDescribe -- describe land for debugging
 *
 * See <design/land/#function.describe>
 */

Res LandDescribe(Land land, mps_lib_FILE *stream)
{
  Res res;

  if (!TESTT(Land, land)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  res = WriteF(stream,
               "Land $P {\n", (WriteFP)land,
               "  class $P", (WriteFP)land->class,
               " (\"$S\")\n", land->class->name,
               "  arena $P\n", (WriteFP)land->arena,
               "  align $U\n", (WriteFU)land->alignment,
               "  inLand: $U\n", (WriteFU)land->inLand,
               NULL);
  if (res != ResOK)
    return res;

  res = (*land->class->describe)(land, stream);
  if (res != ResOK)
    return res;

  res = WriteF(stream, "} Land $P\n", (WriteFP)land, NULL);
  return ResOK;
}


/* landFlushVisitor -- visitor for LandFlush.
 *
 * closureP argument is the destination Land. Attempt to insert the
 * range into the destination.
 */
static Bool landFlushVisitor(Bool *deleteReturn, Land land, Range range,
                             void *closureP, Size closureS)
{
  Res res;
  RangeStruct newRange;
  Land dest;

  AVER(deleteReturn != NULL);
  AVERT(Land, land);
  AVERT(Range, range);
  AVER(closureP != NULL);
  UNUSED(closureS);

  dest = closureP;
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

void LandFlush(Land dest, Land src)
{
  AVERT(Land, dest);
  AVERT(Land, src);

  LandIterate(src, landFlushVisitor, dest, 0);
}


/* LandClassCheck -- check land class */

Bool LandClassCheck(LandClass class)
{
  CHECKL(ProtocolClassCheck(&class->protocol));
  CHECKL(class->name != NULL); /* Should be <=6 char C identifier */
  CHECKL(class->size >= sizeof(LandStruct));
  CHECKL(FUNCHECK(class->init));
  CHECKL(FUNCHECK(class->finish));
  CHECKL(FUNCHECK(class->insert));
  CHECKL(FUNCHECK(class->delete));
  CHECKL(FUNCHECK(class->findFirst));
  CHECKL(FUNCHECK(class->findLast));
  CHECKL(FUNCHECK(class->findLargest));
  CHECKL(FUNCHECK(class->findInZones));
  CHECKL(FUNCHECK(class->describe));
  CHECKS(LandClass, class);
  return TRUE;
}


static Res landTrivInit(Land land, ArgList args)
{
  AVERT(Land, land);
  AVER(ArgListCheck(args));
  UNUSED(args);
  return ResOK;
}

static void landTrivFinish(Land land)
{
  AVERT(Land, land);
  NOOP;
}

static Size landNoSize(Land land)
{
  UNUSED(land);
  NOTREACHED;
  return 0;
}

/* LandSlowSize -- generic size method but slow */

static Bool landSizeVisitor(Bool *deleteReturn, Land land, Range range,
                            void *closureP, Size closureS)
{
  Size *size;

  AVER(deleteReturn != NULL);
  AVERT(Land, land);
  AVERT(Range, range);
  AVER(closureP != NULL);
  UNUSED(closureS);

  size = closureP;
  *size += RangeSize(range);
  *deleteReturn = FALSE;

  return TRUE;
}

Size LandSlowSize(Land land)
{
  Size size = 0;
  LandIterate(land, landSizeVisitor, &size, 0);
  return size;
}

static Res landNoInsert(Range rangeReturn, Land land, Range range)
{
  AVER(rangeReturn != NULL);
  AVERT(Land, land);
  AVERT(Range, range);
  return ResUNIMPL;
}

static Res landNoDelete(Range rangeReturn, Land land, Range range)
{
  AVER(rangeReturn != NULL);
  AVERT(Land, land);
  AVERT(Range, range);
  return ResUNIMPL;
}

static void landNoIterate(Land land, LandVisitor visitor, void *closureP, Size closureS)
{
  AVERT(Land, land);
  AVER(visitor != NULL);
  UNUSED(closureP);
  UNUSED(closureS);
  NOOP;
}

static Bool landNoFind(Range rangeReturn, Range oldRangeReturn, Land land, Size size, FindDelete findDelete)
{
  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Land, land);
  UNUSED(size);
  AVER(FindDeleteCheck(findDelete));
  return ResUNIMPL;
}

static Res landNoFindInZones(Range rangeReturn, Range oldRangeReturn, Land land, Size size, ZoneSet zoneSet, Bool high)
{
  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Land, land);
  UNUSED(size);
  UNUSED(zoneSet);
  AVER(BoolCheck(high));
  return ResUNIMPL;
}

static Res landTrivDescribe(Land land, mps_lib_FILE *stream)
{
  if (!TESTT(Land, land))
    return ResFAIL;
  if (stream == NULL)
    return ResFAIL;
  /* dispatching function does it all */
  return ResOK;
}

DEFINE_CLASS(LandClass, class)
{
  INHERIT_CLASS(&class->protocol, ProtocolClass);
  class->name = "LAND";
  class->size = sizeof(LandStruct);
  class->init = landTrivInit;
  class->sizeMethod = landNoSize;
  class->finish = landTrivFinish;
  class->insert = landNoInsert;
  class->delete = landNoDelete;
  class->iterate = landNoIterate;
  class->findFirst = landNoFind;
  class->findLast = landNoFind;
  class->findLargest = landNoFind;
  class->findInZones = landNoFindInZones;
  class->describe = landTrivDescribe;
  class->sig = LandClassSig;
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
