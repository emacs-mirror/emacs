/* cbs.c: COALESCING BLOCK STRUCTURE IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2001-2013 Ravenbrook Limited.  See end of file for license.
 *
 * .intro: This is a portable implementation of coalescing block
 * structures.
 *
 * .purpose: CBSs are used to manage potentially unbounded
 * collections of memory blocks.
 *
 * .sources: <design/cbs/>.
 */

#include "cbs.h"
#include "splay.h"
#include "meter.h"
#include "poolmfs.h"
#include "mpm.h"

SRCID(cbs, "$Id$");


typedef struct CBSBlockStruct *CBSBlock;
typedef struct CBSBlockStruct {
  SplayNodeStruct splayNode;
  Addr base;
  Addr limit;
  Size maxSize; /* accurate maximum block size of sub-tree */
  ZoneSet zones; /* union zone set of all ranges in sub-tree */
} CBSBlockStruct;

#define CBSBlockBase(block) ((block)->base)
#define CBSBlockLimit(block) ((block)->limit)
#define CBSBlockSize(block) AddrOffset((block)->base, (block)->limit)


#define cbsOfSplayTree(tree) PARENT(CBSStruct, splayTree, (tree))
#define cbsBlockOfSplayNode(node) PARENT(CBSBlockStruct, splayNode, (node))
#define splayTreeOfCBS(tree) (&((cbs)->splayTree))
#define splayNodeOfCBSBlock(block) (&((block)->splayNode))
#define keyOfCBSBlock(block) ((void *)&((block)->base))

#define cbsBlockPool(cbs) MFSPool(&(cbs)->blockPoolStruct)


/* cbsEnter, cbsLeave -- Avoid re-entrance
 *
 * .enter-leave: The callbacks are restricted in what they may call.
 * These functions enforce this.
 *
 * .enter-leave.simple: Simple queries may be called from callbacks.
 */

static void cbsEnter(CBS cbs)
{
  /* Don't need to check as always called from interface function. */
  AVER(!cbs->inCBS);
  cbs->inCBS = TRUE;
  return;
}

static void cbsLeave(CBS cbs)
{
  /* Don't need to check as always called from interface function. */
  AVER(cbs->inCBS);
  cbs->inCBS = FALSE;
  return;
}


/* CBSCheck -- Check CBS */

Bool CBSCheck(CBS cbs)
{
  /* See .enter-leave.simple. */
  CHECKS(CBS, cbs);
  CHECKL(cbs != NULL);
  CHECKL(SplayTreeCheck(splayTreeOfCBS(cbs)));
  /* nothing to check about splayTreeSize */
  CHECKD(MFS, &cbs->blockPoolStruct);
  CHECKU(Arena, cbs->arena);
  CHECKL(BoolCheck(cbs->fastFind));
  CHECKL(BoolCheck(cbs->inCBS));
  /* No MeterCheck */

  return TRUE;
}


static Bool CBSBlockCheck(CBSBlock block)
{
  /* See .enter-leave.simple. */
  UNUSED(block); /* Required because there is no signature */
  CHECKL(block != NULL);
  CHECKL(SplayNodeCheck(splayNodeOfCBSBlock(block)));

  /* If the block is in the middle of being deleted, */
  /* the pointers will be equal. */
  CHECKL(CBSBlockBase(block) <= CBSBlockLimit(block));
  /* Can't check maxSize because it may be invalid at the time */
  return TRUE;
}


/* cbsSplayCompare -- Compare key to [base,limit)
 *
 * See <design/splay/#type.splay.compare.method>
 */

static Compare cbsSplayCompare(void *key, SplayNode node)
{
  Addr base1, base2, limit2;
  CBSBlock cbsBlock;

  /* NULL key compares less than everything. */
  if (key == NULL)
    return CompareLESS;

  AVER(node != NULL);

  base1 = *(Addr *)key;
  cbsBlock = cbsBlockOfSplayNode(node);
  base2 = cbsBlock->base;
  limit2 = cbsBlock->limit;

  if (base1 < base2)
    return CompareLESS;
  else if (base1 >= limit2)
    return CompareGREATER;
  else
    return CompareEQUAL;
}


/* cbsTestNode, cbsTestTree -- test for nodes larger than the S parameter */

static Bool cbsTestNode(SplayTree tree, SplayNode node,
                        void *closureP, Size size)
{
  CBSBlock block;

  AVERT(SplayTree, tree);
  AVERT(SplayNode, node);
  AVER(closureP == NULL);
  AVER(size > 0);
  AVER(cbsOfSplayTree(tree)->fastFind);

  block = cbsBlockOfSplayNode(node);

  return CBSBlockSize(block) >= size;
}

static Bool cbsTestTree(SplayTree tree, SplayNode node,
                        void *closureP, Size size)
{
  CBSBlock block;

  AVERT(SplayTree, tree);
  AVERT(SplayNode, node);
#if 0
  AVER(closureP == NULL);
  AVER(size > 0);
#endif
  UNUSED(closureP);
  UNUSED(size);
  AVER(cbsOfSplayTree(tree)->fastFind);

  block = cbsBlockOfSplayNode(node);

  return block->maxSize >= size;
}


/* cbsUpdateNode -- update size info after restructuring */

static void cbsUpdateNode(SplayTree tree, SplayNode node,
                          SplayNode leftChild, SplayNode rightChild)
{
  Size maxSize;
  ZoneSet zones;
  CBSBlock block;
  Arena arena;

  AVERT(SplayTree, tree);
  AVERT(SplayNode, node);
  if (leftChild != NULL)
    AVERT(SplayNode, leftChild);
  if (rightChild != NULL)
    AVERT(SplayNode, rightChild);
  AVER(cbsOfSplayTree(tree)->fastFind);

  block = cbsBlockOfSplayNode(node);
  maxSize = CBSBlockSize(block);
  arena = cbsOfSplayTree(tree)->arena;
  zones = ZoneSetOfRange(arena, CBSBlockBase(block), CBSBlockLimit(block));

  if (leftChild != NULL) {
    Size size = cbsBlockOfSplayNode(leftChild)->maxSize;
    if (size > maxSize)
      maxSize = size;
    zones = ZoneSetUnion(zones, cbsBlockOfSplayNode(leftChild)->zones);
  }

  if (rightChild != NULL) {
    Size size = cbsBlockOfSplayNode(rightChild)->maxSize;
    if (size > maxSize)
      maxSize = size;
    zones = ZoneSetUnion(zones, cbsBlockOfSplayNode(rightChild)->zones);
  }

  block->maxSize = maxSize;
  block->zones = zones;
}


/* CBSInit -- Initialise a CBS structure
 *
 * See <design/cbs/#function.cbs.init>.
 */

ARG_DEFINE_KEY(cbs_extend_by, Size);

Res CBSInit(Arena arena, CBS cbs, void *owner, Align alignment,
            Bool fastFind, ArgList args)
{
  Size extendBy = CBS_EXTEND_BY_DEFAULT;
  Bool extendSelf = TRUE;
  ArgStruct arg;
  Res res;

  AVERT(Arena, arena);

  if (ArgPick(&arg, args, MPS_KEY_CBS_EXTEND_BY))
    extendBy = arg.val.size;
  if (ArgPick(&arg, args, MFSExtendSelf))
    extendSelf = arg.val.b;

  cbs->arena = arena;
  SplayTreeInit(splayTreeOfCBS(cbs), &cbsSplayCompare,
                fastFind ? &cbsUpdateNode : NULL);
  MPS_ARGS_BEGIN(piArgs) {
    MPS_ARGS_ADD(piArgs, MPS_KEY_MFS_UNIT_SIZE, sizeof(CBSBlockStruct));
    MPS_ARGS_ADD(piArgs, MPS_KEY_EXTEND_BY, extendBy);
    MPS_ARGS_ADD(piArgs, MFSExtendSelf, extendSelf);
    MPS_ARGS_DONE(piArgs);
    res = PoolInit(&cbs->blockPoolStruct.poolStruct, arena, PoolClassMFS(), piArgs);
  } MPS_ARGS_END(piArgs);
  if (res != ResOK)
    return res;
  cbs->splayTreeSize = 0;

  cbs->fastFind = fastFind;
  cbs->alignment = alignment;
  cbs->inCBS = TRUE;

  METER_INIT(cbs->splaySearch, "size of splay tree", (void *)cbs);

  cbs->sig = CBSSig;

  AVERT(CBS, cbs);
  EVENT2(CBSInit, cbs, owner);
  cbsLeave(cbs);
  return ResOK;
}


/* CBSFinish -- Finish a CBS structure
 *
 * See <design/cbs/#function.cbs.finish>.
 */

void CBSFinish(CBS cbs)
{
  AVERT(CBS, cbs);
  cbsEnter(cbs);

  METER_EMIT(&cbs->splaySearch);

  cbs->sig = SigInvalid;

  SplayTreeFinish(splayTreeOfCBS(cbs));
  PoolFinish(cbsBlockPool(cbs));
}


/* Node change operators
 *
 * These four functions are called whenever blocks are created,
 * destroyed, grow, or shrink.  They maintain the maxSize if fastFind is
 * enabled.
 */

static void cbsBlockDelete(CBS cbs, CBSBlock block)
{
  Res res;

  AVERT(CBS, cbs);
  AVERT(CBSBlock, block);

  METER_ACC(cbs->splaySearch, cbs->splayTreeSize);
  res = SplayTreeDelete(splayTreeOfCBS(cbs), splayNodeOfCBSBlock(block),
                        keyOfCBSBlock(block));
  AVER(res == ResOK); /* Must be possible to delete node */
  STATISTIC(--cbs->splayTreeSize);

  /* make invalid */
  block->limit = block->base;

  PoolFree(cbsBlockPool(cbs), (Addr)block, sizeof(CBSBlockStruct));

  return;
}

static void cbsBlockShrunk(CBS cbs, CBSBlock block, Size oldSize)
{
  Size newSize;

  AVERT(CBS, cbs);
  AVERT(CBSBlock, block);

  newSize = CBSBlockSize(block);
  AVER(oldSize > newSize);

  if (cbs->fastFind) {
    SplayNodeRefresh(splayTreeOfCBS(cbs), splayNodeOfCBSBlock(block),
                     keyOfCBSBlock(block));
    AVER(CBSBlockSize(block) <= block->maxSize);
  }
}

static void cbsBlockGrew(CBS cbs, CBSBlock block, Size oldSize)
{
  Size newSize;

  AVERT(CBS, cbs);
  AVERT(CBSBlock, block);

  newSize = CBSBlockSize(block);
  AVER(oldSize < newSize);

  if (cbs->fastFind) {
    SplayNodeRefresh(splayTreeOfCBS(cbs), splayNodeOfCBSBlock(block),
                     keyOfCBSBlock(block));
    AVER(CBSBlockSize(block) <= block->maxSize);
  }
}

/* cbsBlockAlloc -- allocate a new block and set its base and limit,
   but do not insert it into the splay tree yet */

static Res cbsBlockAlloc(CBSBlock *blockReturn, CBS cbs, Range range)
{
  Res res;
  CBSBlock block;
  Addr p;

  AVER(blockReturn != NULL);
  AVERT(CBS, cbs);
  AVERT(Range, range);

  res = PoolAlloc(&p, cbsBlockPool(cbs), sizeof(CBSBlockStruct),
                  /* withReservoirPermit */ FALSE);
  if (res != ResOK)
    goto failPoolAlloc;
  block = (CBSBlock)p;

  SplayNodeInit(splayNodeOfCBSBlock(block));
  block->base = RangeBase(range);
  block->limit = RangeLimit(range);
  block->maxSize = CBSBlockSize(block);

  AVERT(CBSBlock, block);
  *blockReturn = block;
  return ResOK;

failPoolAlloc:
  AVER(res != ResOK);
  return res;
}

/* cbsBlockInsert -- insert a block into the splay tree */

static void cbsBlockInsert(CBS cbs, CBSBlock block)
{
  Res res;

  AVERT(CBS, cbs);
  AVERT(CBSBlock, block);

  METER_ACC(cbs->splaySearch, cbs->splayTreeSize);
  res = SplayTreeInsert(splayTreeOfCBS(cbs), splayNodeOfCBSBlock(block),
                        keyOfCBSBlock(block));
  AVER(res == ResOK);
  STATISTIC(++cbs->splayTreeSize);
}


/* cbsInsertIntoTree -- Insert a range into the splay tree */

static Res cbsInsertIntoTree(Range rangeReturn, CBS cbs, Range range)
{
  Res res;
  Addr base, limit, newBase, newLimit;
  SplayNode leftSplay, rightSplay;
  CBSBlock leftCBS, rightCBS;
  Bool leftMerge, rightMerge;
  Size oldSize;

  AVER(rangeReturn != NULL);
  AVERT(CBS, cbs);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, cbs->alignment));

  base = RangeBase(range);
  limit = RangeLimit(range);

  METER_ACC(cbs->splaySearch, cbs->splayTreeSize);
  res = SplayTreeNeighbours(&leftSplay, &rightSplay,
                            splayTreeOfCBS(cbs), (void *)&base);
  if (res != ResOK)
    goto fail;

  /* The two cases below are not quite symmetrical, because base was
   * passed into the call to SplayTreeNeighbours(), but limit was not.
   * So we know that if there is a left neighbour, then leftCBS->limit
   * <= base (this is ensured by cbsSplayCompare, which is the
   * comparison method on the splay tree). But if there is a right
   * neighbour, all we know is that base < rightCBS->base. But for the
   * range to fit, we need limit <= rightCBS->base too. Hence the extra
   * check and the possibility of failure in the second case.
   */
  if (leftSplay == NULL) {
    leftCBS = NULL;
    leftMerge = FALSE;
  } else {
    leftCBS = cbsBlockOfSplayNode(leftSplay);
    AVER(leftCBS->limit <= base);
    leftMerge = leftCBS->limit == base;
  }

  if (rightSplay == NULL) {
    rightCBS = NULL;
    rightMerge = FALSE;
  } else {
    rightCBS = cbsBlockOfSplayNode(rightSplay);
    if (rightCBS != NULL && limit > CBSBlockLimit(rightCBS)) {
      res = ResFAIL;
      goto fail;
    }
    rightMerge = rightCBS->base == limit;
  }

  newBase = leftMerge ? CBSBlockBase(leftCBS) : base;
  newLimit = rightMerge ? CBSBlockLimit(rightCBS) : limit;

  if (leftMerge && rightMerge) {
    Size oldLeftSize = CBSBlockSize(leftCBS);
    Addr rightLimit = CBSBlockLimit(rightCBS);
    cbsBlockDelete(cbs, rightCBS);
    leftCBS->limit = rightLimit;
    cbsBlockGrew(cbs, leftCBS, oldLeftSize);

  } else if (leftMerge) {
    oldSize = CBSBlockSize(leftCBS);
    leftCBS->limit = limit;
    cbsBlockGrew(cbs, leftCBS, oldSize);

  } else if (rightMerge) {
    oldSize = CBSBlockSize(rightCBS);
    rightCBS->base = base;
    cbsBlockGrew(cbs, rightCBS, oldSize);

  } else {
    CBSBlock block;
    res = cbsBlockAlloc(&block, cbs, range);
    if (res != ResOK)
      goto fail;
    cbsBlockInsert(cbs, block);
  }

  AVER(newBase <= base);
  AVER(newLimit >= limit);
  RangeInit(rangeReturn, newBase, newLimit);

  return ResOK;

fail:
  AVER(res != ResOK);
  return res;
}


/* CBSInsert -- Insert a range into the CBS
 *
 * See <design/cbs/#functions.cbs.insert>.
 */

Res CBSInsert(Range rangeReturn, CBS cbs, Range range)
{
  Res res;

  AVERT(CBS, cbs);
  cbsEnter(cbs);

  AVER(rangeReturn != NULL);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, cbs->alignment));

  res = cbsInsertIntoTree(rangeReturn, cbs, range);

  cbsLeave(cbs);
  return res;
}


/* cbsDeleteFromTree -- delete blocks from the splay tree */

static Res cbsDeleteFromTree(Range rangeReturn, CBS cbs, Range range)
{
  Res res;
  CBSBlock cbsBlock;
  SplayNode splayNode;
  Addr base, limit, oldBase, oldLimit;
  Size oldSize;

  AVER(rangeReturn != NULL);
  AVERT(CBS, cbs);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, cbs->alignment));

  base = RangeBase(range);
  limit = RangeLimit(range);

  METER_ACC(cbs->splaySearch, cbs->splayTreeSize);
  res = SplayTreeSearch(&splayNode, splayTreeOfCBS(cbs), (void *)&base);
  if (res != ResOK)
    goto failSplayTreeSearch;
  cbsBlock = cbsBlockOfSplayNode(splayNode);

  if (limit > cbsBlock->limit) {
    res = ResFAIL;
    goto failLimitCheck;
  }

  oldBase = cbsBlock->base;
  oldLimit = cbsBlock->limit;
  oldSize = CBSBlockSize(cbsBlock);
  RangeInit(rangeReturn, oldBase, oldLimit);

  if (base == oldBase && limit == oldLimit) {
    /* entire block */
    cbsBlockDelete(cbs, cbsBlock);

  } else if (base == oldBase) {
    /* remaining fragment at right */
    AVER(limit < oldLimit);
    cbsBlock->base = limit;
    cbsBlockShrunk(cbs, cbsBlock, oldSize);

  } else if (limit == oldLimit) {
    /* remaining fragment at left */
    AVER(base > oldBase);
    cbsBlock->limit = base;
    cbsBlockShrunk(cbs, cbsBlock, oldSize);

  } else {
    /* two remaining fragments. shrink block to represent fragment at
       left, and create new block for fragment at right. */
    RangeStruct newRange;
    CBSBlock newBlock;
    AVER(base > oldBase);
    AVER(limit < oldLimit);
    RangeInit(&newRange, limit, oldLimit);
    res = cbsBlockAlloc(&newBlock, cbs, &newRange);
    if (res != ResOK) {
      goto failAlloc;
    }
    cbsBlock->limit = base;
    cbsBlockShrunk(cbs, cbsBlock, oldSize);
    cbsBlockInsert(cbs, newBlock);
  }

  return ResOK;

failAlloc:
failLimitCheck:
failSplayTreeSearch:
  AVER(res != ResOK);
  return res;
}


/* CBSDelete -- Remove a range from a CBS
 *
 * See <design/cbs/#function.cbs.delete>.
 */

Res CBSDelete(Range rangeReturn, CBS cbs, Range range)
{
  Res res;

  AVERT(CBS, cbs);
  cbsEnter(cbs);

  AVER(rangeReturn != NULL);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, cbs->alignment));

  res = cbsDeleteFromTree(rangeReturn, cbs, range);

  cbsLeave(cbs);
  return res;
}


static Res cbsBlockDescribe(CBSBlock block, mps_lib_FILE *stream)
{
  Res res;

  if (stream == NULL) return ResFAIL;

  res = WriteF(stream,
               "[$P,$P) {$U}",
               (WriteFP)block->base,
               (WriteFP)block->limit,
               (WriteFU)block->maxSize,
               NULL);
  return res;
}

static Res cbsSplayNodeDescribe(SplayNode splayNode, mps_lib_FILE *stream)
{
  Res res;

  if (splayNode == NULL) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  res = cbsBlockDescribe(cbsBlockOfSplayNode(splayNode), stream);
  return res;
}


/* CBSIterate -- Iterate all blocks in CBS
 *
 * This is not necessarily efficient.
 * See <design/cbs/#function.cbs.iterate>.
 */

void CBSIterate(CBS cbs, CBSIterateMethod iterate,
                void *closureP, Size closureS)
{
  SplayNode splayNode;
  SplayTree splayTree;
  CBSBlock cbsBlock;

  AVERT(CBS, cbs);
  cbsEnter(cbs);
  AVER(FUNCHECK(iterate));

  splayTree = splayTreeOfCBS(cbs);
  /* .splay-iterate.slow: We assume that splay tree iteration does */
  /* searches and meter it. */
  METER_ACC(cbs->splaySearch, cbs->splayTreeSize);
  splayNode = SplayTreeFirst(splayTree, NULL);
  while(splayNode != NULL) {
    RangeStruct range;
    cbsBlock = cbsBlockOfSplayNode(splayNode);
    RangeInit(&range, CBSBlockBase(cbsBlock), CBSBlockLimit(cbsBlock));
    if (!(*iterate)(cbs, &range, closureP, closureS))
      break;
    METER_ACC(cbs->splaySearch, cbs->splayTreeSize);
    splayNode = SplayTreeNext(splayTree, splayNode, keyOfCBSBlock(cbsBlock));
  }

  cbsLeave(cbs);
  return;
}


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


/* cbsFindDeleteRange -- delete appropriate range of block found */

static void cbsFindDeleteRange(Range rangeReturn, Range oldRangeReturn,
                               CBS cbs, Range range, Size size,
                               FindDelete findDelete)
{
  Bool callDelete = TRUE;
  Addr base, limit;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(CBS, cbs);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, cbs->alignment));
  AVER(size > 0);
  AVER(SizeIsAligned(size, cbs->alignment));
  AVER(RangeSize(range) >= size);
  AVERT(FindDelete, findDelete);

  base = RangeBase(range);
  limit = RangeLimit(range);

  switch(findDelete) {

  case FindDeleteNONE:
    callDelete = FALSE;
    break;

  case FindDeleteLOW:
    limit = AddrAdd(base, size);
    break;

  case FindDeleteHIGH:
    base = AddrSub(limit, size);
    break;

  case FindDeleteENTIRE:
    /* do nothing */
    break;

  default:
    NOTREACHED;
    break;
  }

  RangeInit(rangeReturn, base, limit);

  if (callDelete) {
    Res res;
    res = cbsDeleteFromTree(oldRangeReturn, cbs, rangeReturn);
    /* Can't have run out of memory, because all our callers pass in
       blocks that were just found in the splay tree, and we only
       deleted from one end of the block, so cbsDeleteFromTree did not
       need to allocate a new block. */
    AVER(res == ResOK);
  }
}


/* CBSFindFirst -- find the first block of at least the given size */

Bool CBSFindFirst(Range rangeReturn, Range oldRangeReturn,
                  CBS cbs, Size size, FindDelete findDelete)
{
  Bool found;
  SplayNode node;

  AVERT(CBS, cbs);
  cbsEnter(cbs);

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVER(size > 0);
  AVER(SizeIsAligned(size, cbs->alignment));
  AVER(cbs->fastFind);
  AVERT(FindDelete, findDelete);

  METER_ACC(cbs->splaySearch, cbs->splayTreeSize);
  found = SplayFindFirst(&node, splayTreeOfCBS(cbs), &cbsTestNode,
                         &cbsTestTree, NULL, size);
  if (found) {
    CBSBlock block;
    RangeStruct range;
    block = cbsBlockOfSplayNode(node);
    AVER(CBSBlockSize(block) >= size);
    RangeInit(&range, CBSBlockBase(block), CBSBlockLimit(block));
    AVER(RangeSize(&range) >= size);
    cbsFindDeleteRange(rangeReturn, oldRangeReturn, cbs, &range,
                       size, findDelete);
  }

  cbsLeave(cbs);
  return found;
}

/* CBSFindFirstInZones -- find the first block of at least the given size
   that lies entirely within a zone set */

typedef struct cbsTestNodeInZonesClosureStruct {
  Size size;
  Arena arena;
  ZoneSet zoneSet;
  Addr base;
  Addr limit;
} cbsTestNodeInZonesClosureStruct, *cbsTestNodeInZonesClosure;

static Bool cbsTestNodeInZones(SplayTree tree, SplayNode node,
                               void *closureP, Size closureSize)
{
  CBSBlock block = cbsBlockOfSplayNode(node);
  cbsTestNodeInZonesClosure closure = closureP;
  
  UNUSED(tree);
  AVER(closureSize == sizeof(cbsTestNodeInZonesClosureStruct));
  UNUSED(closureSize);
  
  return CBSBlockSize(block) >= closure->size &&
         RangeInZoneSet(&closure->base, &closure->limit,
                        CBSBlockBase(block), CBSBlockLimit(block),
                        closure->arena, closure->zoneSet, closure->size);
}

static Bool cbsTestTreeInZones(SplayTree tree, SplayNode node,
                               void *closureP, Size closureSize)
{
  CBSBlock block = cbsBlockOfSplayNode(node);
  cbsTestNodeInZonesClosure closure = closureP;
  
  UNUSED(tree);
  AVER(closureSize == sizeof(cbsTestNodeInZonesClosureStruct));
  UNUSED(closureSize);
  
  return block->maxSize >= closure->size &&
         ZoneSetInter(block->zones, closure->zoneSet) != ZoneSetEMPTY;
}

Bool CBSFindFirstInZones(Range rangeReturn, Range oldRangeReturn,
                         CBS cbs, Size size,
                         Arena arena, ZoneSet zoneSet)
{
  SplayNode node;
  cbsTestNodeInZonesClosureStruct closure;
  Bool found;
  
  /* Check whether the size will fit in the zoneSet at all. */
  /* FIXME: Perhaps this should be a function in ref.c */
  if (zoneSet == ZoneSetEMPTY)
    return FALSE;
  if (zoneSet == ZoneSetUNIV)
    return CBSFindFirst(rangeReturn, oldRangeReturn, cbs, size, FindDeleteLOW);
  if (ZoneSetIsSingle(zoneSet)) {
    if (size > ArenaStripeSize(arena))
      return FALSE;
  } else {
    /* Check whether any run of bits in zoneSet can accommodate the size. */
#if 0
    ZoneSet mask = ((ZoneSet)1 << SizeAlignUp(size, ArenaStripeSize(arena))) - 1;
    /* mask == ZoneSetUNIV case very unlikely, so don't bother testing for it */
    for (i = 0; i < ZONE_SET_WIDTH; ++i) {
      if (ZoneSetSub(BS_ROTATE_LEFT(ZoneSet, mask, i), zoneSet))
        goto found;
    }
    return FALSE;
  found:;
#endif
  }
  
  /* It would be nice if there were a neat way to eliminate all runs of
     zones in zoneSet too small for size.*/

  cbsEnter(cbs);

  closure.arena = arena;
  closure.zoneSet = zoneSet;
  closure.size = size;
  found = SplayFindFirst(&node, splayTreeOfCBS(cbs),
                         &cbsTestNodeInZones,
                         &cbsTestTreeInZones,
                         &closure, sizeof(closure));
  if (found) {
    CBSBlock block = cbsBlockOfSplayNode(node);
    RangeStruct rangeStruct, oldRangeStruct;
    Res res;

    AVER(CBSBlockBase(block) <= closure.base);
    AVER(AddrOffset(closure.base, closure.limit) >= size);
    AVER(ZoneSetSub(ZoneSetOfRange(arena, closure.base, closure.limit), zoneSet));
    AVER(closure.limit <= CBSBlockLimit(block));

    RangeInit(&rangeStruct, closure.base, AddrAdd(closure.base, size));
    res = cbsDeleteFromTree(&oldRangeStruct, cbs, &rangeStruct);
    if (res != ResOK)  /* not enough memory to split block FIXME: Think about this! */
      found = FALSE;
    else {
      RangeCopy(rangeReturn, &rangeStruct);
      RangeCopy(oldRangeReturn, &oldRangeStruct);
    }
  }

  cbsLeave(cbs);
  return found;
}


/* CBSFindLast -- find the last block of at least the given size */

Bool CBSFindLast(Range rangeReturn, Range oldRangeReturn,
                 CBS cbs, Size size, FindDelete findDelete)
{
  Bool found;
  SplayNode node;

  AVERT(CBS, cbs);
  cbsEnter(cbs);

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVER(size > 0);
  AVER(SizeIsAligned(size, cbs->alignment));
  AVER(cbs->fastFind);
  AVERT(FindDelete, findDelete);

  METER_ACC(cbs->splaySearch, cbs->splayTreeSize);
  found = SplayFindLast(&node, splayTreeOfCBS(cbs), &cbsTestNode,
                        &cbsTestTree, NULL, size);
  if (found) {
    CBSBlock block;
    RangeStruct range;
    block = cbsBlockOfSplayNode(node);
    AVER(CBSBlockSize(block) >= size);
    RangeInit(&range, CBSBlockBase(block), CBSBlockLimit(block));
    AVER(RangeSize(&range) >= size);
    cbsFindDeleteRange(rangeReturn, oldRangeReturn, cbs, &range,
                       size, findDelete);
  }

  cbsLeave(cbs);
  return found;
}


/* CBSFindLargest -- find the largest block in the CBS */

Bool CBSFindLargest(Range rangeReturn, Range oldRangeReturn,
                    CBS cbs, Size size, FindDelete findDelete)
{
  Bool found = FALSE;
  SplayNode root;
  Bool notEmpty;

  AVERT(CBS, cbs);
  cbsEnter(cbs);

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVER(cbs->fastFind);
  AVERT(FindDelete, findDelete);

  notEmpty = SplayRoot(&root, splayTreeOfCBS(cbs));
  if (notEmpty) {
    RangeStruct range;
    CBSBlock block;
    SplayNode node = NULL;    /* suppress "may be used uninitialized" */
    Size maxSize;

    maxSize = cbsBlockOfSplayNode(root)->maxSize;
    if (maxSize >= size) {
      METER_ACC(cbs->splaySearch, cbs->splayTreeSize);
      found = SplayFindFirst(&node, splayTreeOfCBS(cbs), &cbsTestNode,
                             &cbsTestTree, NULL, maxSize);
      AVER(found); /* maxSize is exact, so we will find it. */
      block = cbsBlockOfSplayNode(node);
      AVER(CBSBlockSize(block) >= maxSize);
      RangeInit(&range, CBSBlockBase(block), CBSBlockLimit(block));
      AVER(RangeSize(&range) >= maxSize);
      cbsFindDeleteRange(rangeReturn, oldRangeReturn, cbs, &range,
                         maxSize, findDelete);
    }
  }

  cbsLeave(cbs);
  return found;
}


/* CBSDescribe -- describe a CBS
 *
 * See <design/cbs/#function.cbs.describe>.
 */

Res CBSDescribe(CBS cbs, mps_lib_FILE *stream)
{
  Res res;

  if (!TESTT(CBS, cbs)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  res = WriteF(stream,
               "CBS $P {\n", (WriteFP)cbs,
               "  alignment: $U\n", (WriteFU)cbs->alignment,
               "  blockPool: $P\n", (WriteFP)cbsBlockPool(cbs),
               "  fastFind: $U\n", (WriteFU)cbs->fastFind,
               "  inCBS: $U\n", (WriteFU)cbs->inCBS,
               "  splayTreeSize: $U\n", (WriteFU)cbs->splayTreeSize,
               NULL);
  if (res != ResOK) return res;

  res = SplayTreeDescribe(splayTreeOfCBS(cbs), stream, &cbsSplayNodeDescribe);
  if (res != ResOK) return res;

  res = METER_WRITE(cbs->splaySearch, stream);
  if (res != ResOK) return res;

  res = WriteF(stream, "}\n", NULL);
  return res;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
