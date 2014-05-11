/* cbs.c: COALESCING BLOCK STRUCTURE IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
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


#define CBSBlockBase(block) ((block)->base)
#define CBSBlockLimit(block) ((block)->limit)
#define CBSBlockSize(block) AddrOffset((block)->base, (block)->limit)


#define cbsSplay(cbs) (&((cbs)->splayTreeStruct))
#define cbsOfSplay(_splay) PARENT(CBSStruct, splayTreeStruct, _splay)
#define cbsBlockTree(block) (&((block)->treeStruct))
#define cbsBlockOfTree(_tree) TREE_ELT(CBSBlock, treeStruct, _tree)
#define cbsBlockKey(block) (&((block)->base))
#define cbsBlockPool(cbs) RVALUE((cbs)->blockPool)


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
  CHECKD(SplayTree, cbsSplay(cbs));
  /* nothing to check about treeSize */
  CHECKD(Pool, cbs->blockPool);
  CHECKU(Arena, cbs->arena);
  CHECKL(BoolCheck(cbs->fastFind));
  CHECKL(BoolCheck(cbs->inCBS));
  CHECKL(BoolCheck(cbs->ownPool));
  CHECKL(BoolCheck(cbs->zoned));
  /* No MeterCheck */

  return TRUE;
}


ATTRIBUTE_UNUSED
static Bool CBSBlockCheck(CBSBlock block)
{
  /* See .enter-leave.simple. */
  UNUSED(block); /* Required because there is no signature */
  CHECKL(block != NULL);
  /* Can't use CHECKD_NOSIG because TreeEMPTY is NULL. */
  CHECKL(TreeCheck(cbsBlockTree(block)));

  /* If the block is in the middle of being deleted, */
  /* the pointers will be equal. */
  CHECKL(CBSBlockBase(block) <= CBSBlockLimit(block));
  /* Can't check maxSize because it may be invalid at the time */
  return TRUE;
}


/* cbsCompare -- Compare key to [base,limit)
 *
 * See <design/splay/#type.splay.compare.method>
 */

static Compare cbsCompare(Tree tree, TreeKey key)
{
  Addr base1, base2, limit2;
  CBSBlock cbsBlock;

  AVER(tree != NULL);
  AVER(tree != TreeEMPTY);

  base1 = *(Addr *)key;
  cbsBlock = cbsBlockOfTree(tree);
  base2 = cbsBlock->base;
  limit2 = cbsBlock->limit;

  if (base1 < base2)
    return CompareLESS;
  else if (base1 >= limit2)
    return CompareGREATER;
  else
    return CompareEQUAL;
}

static TreeKey cbsKey(Tree tree)
{
  return cbsBlockKey(cbsBlockOfTree(tree));
}


/* cbsTestNode, cbsTestTree -- test for nodes larger than the S parameter */

static Bool cbsTestNode(SplayTree splay, Tree tree,
                        void *closureP, Size size)
{
  CBSBlock block;

  AVERT(SplayTree, splay);
  AVERT(Tree, tree);
  AVER(closureP == NULL);
  AVER(size > 0);
  AVER(cbsOfSplay(splay)->fastFind);

  block = cbsBlockOfTree(tree);

  return CBSBlockSize(block) >= size;
}

static Bool cbsTestTree(SplayTree splay, Tree tree,
                        void *closureP, Size size)
{
  CBSBlock block;

  AVERT(SplayTree, splay);
  AVERT(Tree, tree);
#if 0
  AVER(closureP == NULL);
  AVER(size > 0);
#endif
  UNUSED(closureP);
  UNUSED(size);
  AVER(cbsOfSplay(splay)->fastFind);

  block = cbsBlockOfTree(tree);

  return block->maxSize >= size;
}


/* cbsUpdateNode -- update size info after restructuring */

static void cbsUpdateNode(SplayTree splay, Tree tree)
{
  Size maxSize;
  CBSBlock block;

  AVERT_CRITICAL(SplayTree, splay);
  AVERT_CRITICAL(Tree, tree);
  AVER_CRITICAL(cbsOfSplay(splay)->fastFind);

  block = cbsBlockOfTree(tree);
  maxSize = CBSBlockSize(block);

  if (TreeHasLeft(tree)) {
    Size size = cbsBlockOfTree(TreeLeft(tree))->maxSize;
    if (size > maxSize)
      maxSize = size;
  }

  if (TreeHasRight(tree)) {
    Size size = cbsBlockOfTree(TreeRight(tree))->maxSize;
    if (size > maxSize)
      maxSize = size;
  }

  block->maxSize = maxSize;
}


/* cbsUpdateZonedNode -- update size and zone info after restructuring */

static void cbsUpdateZonedNode(SplayTree splay, Tree tree)
{
  ZoneSet zones;
  CBSBlock block;
  Arena arena;

  AVERT_CRITICAL(SplayTree, splay);
  AVERT_CRITICAL(Tree, tree);
  AVER_CRITICAL(cbsOfSplay(splay)->fastFind);
  AVER_CRITICAL(cbsOfSplay(splay)->zoned);

  cbsUpdateNode(splay, tree);

  block = cbsBlockOfTree(tree);
  arena = cbsOfSplay(splay)->arena;
  zones = ZoneSetOfRange(arena, CBSBlockBase(block), CBSBlockLimit(block));

  if (TreeHasLeft(tree))
    zones = ZoneSetUnion(zones, cbsBlockOfTree(TreeLeft(tree))->zones);

  if (TreeHasRight(tree))
    zones = ZoneSetUnion(zones, cbsBlockOfTree(TreeRight(tree))->zones);

  block->zones = zones;
}


/* CBSInit -- Initialise a CBS structure
 *
 * See <design/cbs/#function.cbs.init>.
 */

ARG_DEFINE_KEY(cbs_block_pool, Pool);

Res CBSInit(CBS cbs, Arena arena, void *owner, Align alignment,
            Bool fastFind, Bool zoned, ArgList args)
{
  ArgStruct arg;
  Res res;
  Pool blockPool = NULL;
  SplayUpdateNodeMethod update;

  AVERT(Arena, arena);
  AVER(cbs != NULL);
  AVERT(Align, alignment);
  AVERT(Bool, fastFind);
  AVERT(Bool, zoned);

  if (ArgPick(&arg, args, CBSBlockPool))
    blockPool = arg.val.pool;

  update = SplayTrivUpdate;
  if (fastFind)
    update = cbsUpdateNode;
  if (zoned) {
    AVER(fastFind);
    update = cbsUpdateZonedNode;
  }

  SplayTreeInit(cbsSplay(cbs), cbsCompare, cbsKey, update);

  if (blockPool != NULL) {
    cbs->blockPool = blockPool;
    cbs->ownPool = FALSE;
  } else {
    MPS_ARGS_BEGIN(pcArgs) {
      MPS_ARGS_ADD(pcArgs, MPS_KEY_MFS_UNIT_SIZE, sizeof(CBSBlockStruct));
      res = PoolCreate(&cbs->blockPool, arena, PoolClassMFS(), pcArgs);
    } MPS_ARGS_END(pcArgs);
    if (res != ResOK)
      return res;
    cbs->ownPool = TRUE;
  }
  cbs->treeSize = 0;

  cbs->arena = arena;
  cbs->fastFind = fastFind;
  cbs->zoned = zoned;
  cbs->alignment = alignment;
  cbs->inCBS = TRUE;

  METER_INIT(cbs->treeSearch, "size of tree", (void *)cbs);

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

  METER_EMIT(&cbs->treeSearch);

  cbs->sig = SigInvalid;

  SplayTreeFinish(cbsSplay(cbs));
  if (cbs->ownPool)
    PoolDestroy(cbsBlockPool(cbs));
}


/* Node change operators
 *
 * These four functions are called whenever blocks are created,
 * destroyed, grow, or shrink.  They maintain the maxSize if fastFind is
 * enabled.
 */

static void cbsBlockDelete(CBS cbs, CBSBlock block)
{
  Bool b;

  AVERT(CBS, cbs);
  AVERT(CBSBlock, block);

  METER_ACC(cbs->treeSearch, cbs->treeSize);
  b = SplayTreeDelete(cbsSplay(cbs), cbsBlockTree(block));
  AVER(b); /* expect block to be in the tree */
  STATISTIC(--cbs->treeSize);

  /* make invalid */
  block->limit = block->base;

  PoolFree(cbsBlockPool(cbs), (Addr)block, sizeof(CBSBlockStruct));
}

static void cbsBlockShrunk(CBS cbs, CBSBlock block, Size oldSize)
{
  Size newSize;

  AVERT(CBS, cbs);
  AVERT(CBSBlock, block);

  newSize = CBSBlockSize(block);
  AVER(oldSize > newSize);

  if (cbs->fastFind) {
    SplayNodeRefresh(cbsSplay(cbs), cbsBlockTree(block));
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
    SplayNodeRefresh(cbsSplay(cbs), cbsBlockTree(block));
    AVER(CBSBlockSize(block) <= block->maxSize);
  }
}

/* cbsBlockAlloc -- allocate a new block and set its base and limit,
   but do not insert it into the tree yet */

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

  TreeInit(cbsBlockTree(block));
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

/* cbsBlockInsert -- insert a block into the tree */

static void cbsBlockInsert(CBS cbs, CBSBlock block)
{
  Bool b;

  AVERT(CBS, cbs);
  AVERT(CBSBlock, block);

  METER_ACC(cbs->treeSearch, cbs->treeSize);
  b = SplayTreeInsert(cbsSplay(cbs), cbsBlockTree(block));
  AVER(b);
  STATISTIC(++cbs->treeSize);
}


/* cbsInsertIntoTree -- Insert a range into the tree */

static Res cbsInsertIntoTree(Range rangeReturn, CBS cbs, Range range)
{
  Bool b;
  Res res;
  Addr base, limit, newBase, newLimit;
  Tree leftSplay, rightSplay;
  CBSBlock leftCBS, rightCBS;
  Bool leftMerge, rightMerge;
  Size oldSize;

  AVER(rangeReturn != NULL);
  AVERT(CBS, cbs);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, cbs->alignment));

  base = RangeBase(range);
  limit = RangeLimit(range);

  METER_ACC(cbs->treeSearch, cbs->treeSize);
  b = SplayTreeNeighbours(&leftSplay, &rightSplay, cbsSplay(cbs), &base);
  if (!b) {
    res = ResFAIL;
    goto fail;
  }

  /* The two cases below are not quite symmetrical, because base was
   * passed into the call to SplayTreeNeighbours(), but limit was not.
   * So we know that if there is a left neighbour, then leftCBS->limit
   * <= base (this is ensured by cbsCompare, which is the
   * comparison method on the tree). But if there is a right
   * neighbour, all we know is that base < rightCBS->base. But for the
   * range to fit, we need limit <= rightCBS->base too. Hence the extra
   * check and the possibility of failure in the second case.
   */
  if (leftSplay == TreeEMPTY) {
    leftCBS = NULL;
    leftMerge = FALSE;
  } else {
    leftCBS = cbsBlockOfTree(leftSplay);
    AVER(leftCBS->limit <= base);
    leftMerge = leftCBS->limit == base;
  }

  if (rightSplay == TreeEMPTY) {
    rightCBS = NULL;
    rightMerge = FALSE;
  } else {
    rightCBS = cbsBlockOfTree(rightSplay);
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
 *
 * .insert.alloc: Will only allocate a block if the range does not
 * abut an existing range.
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


/* cbsDeleteFromTree -- delete blocks from the tree */

static Res cbsDeleteFromTree(Range rangeReturn, CBS cbs, Range range)
{
  Res res;
  CBSBlock cbsBlock;
  Tree tree;
  Addr base, limit, oldBase, oldLimit;
  Size oldSize;

  AVER(rangeReturn != NULL);
  AVERT(CBS, cbs);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, cbs->alignment));

  base = RangeBase(range);
  limit = RangeLimit(range);

  METER_ACC(cbs->treeSearch, cbs->treeSize);
  if (!SplayTreeFind(&tree, cbsSplay(cbs), (void *)&base)) {
    res = ResFAIL;
    goto failSplayTreeSearch;
  }
  cbsBlock = cbsBlockOfTree(tree);

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
 *
 * .delete.alloc: Will only allocate a block if the range splits
 * an existing range.
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

  if (stream == NULL)
    return ResFAIL;

  res = WriteF(stream,
               "[$P,$P) {$U, $B}",
               (WriteFP)block->base,
               (WriteFP)block->limit,
               (WriteFU)block->maxSize,
               (WriteFB)block->zones,
               NULL);
  return res;
}

static Res cbsSplayNodeDescribe(Tree tree, mps_lib_FILE *stream)
{
  Res res;

  if (tree == TreeEMPTY)
    return ResFAIL;
  if (stream == NULL)
    return ResFAIL;

  res = cbsBlockDescribe(cbsBlockOfTree(tree), stream);
  return res;
}


/* CBSIterate -- iterate over all blocks in CBS
 *
 * Applies a visitor to all isolated contiguous ranges in a CBS.
 * It receives a pointer, ``Size`` closure pair to pass on to the
 * visitor function, and an visitor function to invoke on every range
 * in address order. If the visitor returns ``FALSE``, then the iteration
 * is terminated.
 *
 * The visitor function may not modify the CBS during the iteration.
 * This is because CBSIterate uses TreeTraverse, which does not permit
 * modification, for speed and to avoid perturbing the splay tree balance.
 *
 * See <design/cbs/#function.cbs.iterate>.
 */

typedef struct CBSIterateClosure {
  CBS cbs;
  CBSVisitor iterate;
  void *closureP;
  Size closureS;
} CBSIterateClosure;

static Bool cbsIterateVisit(Tree tree, void *closureP, Size closureS)
{
  CBSIterateClosure *closure = closureP;
  RangeStruct range;
  CBSBlock cbsBlock;
  CBS cbs = closure->cbs;

  UNUSED(closureS);

  cbsBlock = cbsBlockOfTree(tree);
  RangeInit(&range, CBSBlockBase(cbsBlock), CBSBlockLimit(cbsBlock));
  if (!closure->iterate(cbs, &range, closure->closureP, closure->closureS))
    return FALSE;
  METER_ACC(cbs->treeSearch, cbs->treeSize);
  return TRUE;
}

void CBSIterate(CBS cbs, CBSVisitor visitor,
                void *closureP, Size closureS)
{
  SplayTree splay;
  CBSIterateClosure closure;

  AVERT(CBS, cbs);
  cbsEnter(cbs);
  AVER(FUNCHECK(visitor));

  splay = cbsSplay(cbs);
  /* .splay-iterate.slow: We assume that splay tree iteration does */
  /* searches and meter it. */
  METER_ACC(cbs->treeSearch, cbs->treeSize);

  closure.cbs = cbs;
  closure.iterate = visitor;
  closure.closureP = closureP;
  closure.closureS = closureS;
  (void)TreeTraverse(SplayTreeRoot(splay), splay->compare, splay->nodeKey,
                     cbsIterateVisit, &closure, 0);

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
       blocks that were just found in the tree, and we only
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
  Tree tree;

  AVERT(CBS, cbs);
  cbsEnter(cbs);

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVER(size > 0);
  AVER(SizeIsAligned(size, cbs->alignment));
  AVER(cbs->fastFind);
  AVERT(FindDelete, findDelete);

  METER_ACC(cbs->treeSearch, cbs->treeSize);
  found = SplayFindFirst(&tree, cbsSplay(cbs), &cbsTestNode,
                         &cbsTestTree, NULL, size);
  if (found) {
    CBSBlock block;
    RangeStruct range;
    block = cbsBlockOfTree(tree);
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
  Bool high;
} cbsTestNodeInZonesClosureStruct, *cbsTestNodeInZonesClosure;

static Bool cbsTestNodeInZones(SplayTree splay, Tree tree,
                               void *closureP, Size closureSize)
{
  CBSBlock block = cbsBlockOfTree(tree);
  cbsTestNodeInZonesClosure closure = closureP;
  RangeInZoneSet search;
  
  UNUSED(splay);
  AVER(closureSize == sizeof(cbsTestNodeInZonesClosureStruct));
  UNUSED(closureSize);

  search = closure->high ? RangeInZoneSetLast : RangeInZoneSetFirst;

  return search(&closure->base, &closure->limit,
                CBSBlockBase(block), CBSBlockLimit(block),
                closure->arena, closure->zoneSet, closure->size);
}

static Bool cbsTestTreeInZones(SplayTree splay, Tree tree,
                               void *closureP, Size closureSize)
{
  CBSBlock block = cbsBlockOfTree(tree);
  cbsTestNodeInZonesClosure closure = closureP;
  
  UNUSED(splay);
  AVER(closureSize == sizeof(cbsTestNodeInZonesClosureStruct));
  UNUSED(closureSize);
  
  return block->maxSize >= closure->size &&
         ZoneSetInter(block->zones, closure->zoneSet) != ZoneSetEMPTY;
}

Res CBSFindInZones(Range rangeReturn, Range oldRangeReturn,
                   CBS cbs, Size size,
                   ZoneSet zoneSet, Bool high)
{
  Tree tree;
  cbsTestNodeInZonesClosureStruct closure;
  Res res;
  CBSFindMethod cbsFind;
  SplayFindMethod splayFind;
  
  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(CBS, cbs);
  /* AVERT(ZoneSet, zoneSet); */
  AVERT(Bool, high);

  cbsFind = high ? CBSFindLast : CBSFindFirst;
  splayFind = high ? SplayFindLast : SplayFindFirst;
  
  if (zoneSet == ZoneSetEMPTY)
    return ResFAIL;
  if (zoneSet == ZoneSetUNIV) {
    FindDelete fd = high ? FindDeleteHIGH : FindDeleteLOW;
    if (cbsFind(rangeReturn, oldRangeReturn, cbs, size, fd))
      return ResOK;
    else
      return ResFAIL;
  }
  if (ZoneSetIsSingle(zoneSet) && size > ArenaStripeSize(cbs->arena))
    return ResFAIL;

  /* It would be nice if there were a neat way to eliminate all runs of
     zones in zoneSet too small for size.*/

  cbsEnter(cbs);

  closure.arena = cbs->arena;
  closure.zoneSet = zoneSet;
  closure.size = size;
  closure.high = high;
  if (splayFind(&tree, cbsSplay(cbs),
                cbsTestNodeInZones,
                cbsTestTreeInZones,
                &closure, sizeof(closure))) {
    CBSBlock block = cbsBlockOfTree(tree);
    RangeStruct rangeStruct, oldRangeStruct;

    AVER(CBSBlockBase(block) <= closure.base);
    AVER(AddrOffset(closure.base, closure.limit) >= size);
    AVER(ZoneSetSub(ZoneSetOfRange(cbs->arena, closure.base, closure.limit), zoneSet));
    AVER(closure.limit <= CBSBlockLimit(block));

    if (!high)
      RangeInit(&rangeStruct, closure.base, AddrAdd(closure.base, size));
    else
      RangeInit(&rangeStruct, AddrSub(closure.limit, size), closure.limit);
    res = cbsDeleteFromTree(&oldRangeStruct, cbs, &rangeStruct);
    if (res == ResOK) {  /* enough memory to split block */
      RangeCopy(rangeReturn, &rangeStruct);
      RangeCopy(oldRangeReturn, &oldRangeStruct);
    }
  } else
    res = ResFAIL;

  cbsLeave(cbs);
  return res;
}


/* CBSFindLast -- find the last block of at least the given size */

Bool CBSFindLast(Range rangeReturn, Range oldRangeReturn,
                 CBS cbs, Size size, FindDelete findDelete)
{
  Bool found;
  Tree tree;

  AVERT(CBS, cbs);
  cbsEnter(cbs);

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVER(size > 0);
  AVER(SizeIsAligned(size, cbs->alignment));
  AVER(cbs->fastFind);
  AVERT(FindDelete, findDelete);

  METER_ACC(cbs->treeSearch, cbs->treeSize);
  found = SplayFindLast(&tree, cbsSplay(cbs), &cbsTestNode,
                        &cbsTestTree, NULL, size);
  if (found) {
    CBSBlock block;
    RangeStruct range;
    block = cbsBlockOfTree(tree);
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

  AVERT(CBS, cbs);
  cbsEnter(cbs);

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVER(cbs->fastFind);
  AVERT(FindDelete, findDelete);

  if (!SplayTreeIsEmpty(cbsSplay(cbs))) {
    RangeStruct range;
    CBSBlock block;
    Tree tree = TreeEMPTY;    /* suppress "may be used uninitialized" */
    Size maxSize;

    maxSize = cbsBlockOfTree(SplayTreeRoot(cbsSplay(cbs)))->maxSize;
    if (maxSize >= size) {
      METER_ACC(cbs->treeSearch, cbs->treeSize);
      found = SplayFindFirst(&tree, cbsSplay(cbs), &cbsTestNode,
                             &cbsTestTree, NULL, maxSize);
      AVER(found); /* maxSize is exact, so we will find it. */
      block = cbsBlockOfTree(tree);
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

  if (!TESTT(CBS, cbs))
    return ResFAIL;
  if (stream == NULL)
    return ResFAIL;

  res = WriteF(stream,
               "CBS $P {\n", (WriteFP)cbs,
               "  alignment: $U\n", (WriteFU)cbs->alignment,
               "  blockPool: $P\n", (WriteFP)cbsBlockPool(cbs),
               "  fastFind: $U\n", (WriteFU)cbs->fastFind,
               "  inCBS: $U\n", (WriteFU)cbs->inCBS,
               "  treeSize: $U\n", (WriteFU)cbs->treeSize,
               NULL);
  if (res != ResOK) return res;

  res = SplayTreeDescribe(cbsSplay(cbs), stream, &cbsSplayNodeDescribe);
  if (res != ResOK) return res;

  METER_WRITE(cbs->treeSearch, stream);

  res = WriteF(stream, "}\n", NULL);
  return res;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
