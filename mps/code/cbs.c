/* cbs.c: COALESCING BLOCK STRUCTURE IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 *
 * .intro: This is a portable implementation of coalescing block
 * structures.
 *
 * .purpose: CBSs are used to manage potentially unbounded collections
 * of memory blocks.
 *
 * .sources: <design/cbs>.
 *
 * .critical: In manual-allocation-bound programs using MVFF, many of
 * these functions are on the critical paths via mps_alloc (and then
 * PoolAlloc, MVFFAlloc, failoverFind*, cbsFind*) and mps_free (and
 * then MVFFFree, failoverInsert, cbsInsert).
 */

#include "cbs.h"
#include "rangetree.h"
#include "range.h"
#include "splay.h"
#include "meter.h"
#include "poolmfs.h"
#include "mpm.h"

SRCID(cbs, "$Id$");


#define cbsSplay(cbs) (&((cbs)->splayTreeStruct))
#define cbsOfSplay(splay) PARENT(CBSStruct, splayTreeStruct, splay)
#define cbsFastBlockOfTree(tree) \
  PARENT(CBSFastBlockStruct, rangeTreeStruct, RangeTreeOfTree(tree))
#define cbsFastBlockNode(block) (&(block)->rangeTreeStruct)
#define cbsZonedBlockOfTree(tree) \
  PARENT(CBSZonedBlockStruct, cbsFastBlockStruct, cbsFastBlockOfTree(tree))
#define cbsZonedBlockNode(block) cbsFastBlockNode(&(block)->cbsFastBlockStruct)
#define cbsBlockPool(cbs) RVALUE((cbs)->blockPool)


/* CBSCheck -- Check CBS */

Bool CBSCheck(CBS cbs)
{
  /* See .enter-leave.simple. */
  Land land;
  CHECKS(CBS, cbs);
  land = CBSLand(cbs);
  CHECKD(Land, land);
  CHECKD(SplayTree, cbsSplay(cbs));
  CHECKD(Pool, cbs->blockPool);
  CHECKL(cbs->blockStructSize > 0);
  CHECKL(BoolCheck(cbs->ownPool));
  CHECKL(SizeIsAligned(cbs->size, LandAlignment(land)));
  STATISTIC(CHECKL((cbs->size == 0) == (cbs->treeSize == 0)));

  return TRUE;
}


/* cbsTestNode, cbsTestTree -- test for nodes larger than the S parameter */

static Bool cbsTestNode(SplayTree splay, Tree tree, void *closure)
{
  RangeTree block;
  Size *sizeP = closure;

  AVERT_CRITICAL(SplayTree, splay);
  AVERT_CRITICAL(Tree, tree);
  AVER_CRITICAL(sizeP != NULL);
  AVER_CRITICAL(*sizeP > 0);
  AVER_CRITICAL(IsA(CBSFast, cbsOfSplay(splay)));

  block = RangeTreeOfTree(tree);

  return RangeTreeSize(block) >= *sizeP;
}

static Bool cbsTestTree(SplayTree splay, Tree tree,
                        void *closure)
{
  CBSFastBlock block;
  Size *sizeP = closure;

  AVERT_CRITICAL(SplayTree, splay);
  AVERT_CRITICAL(Tree, tree);
  AVER_CRITICAL(sizeP != NULL);
  AVER_CRITICAL(*sizeP > 0);
  AVER_CRITICAL(IsA(CBSFast, cbsOfSplay(splay)));

  block = cbsFastBlockOfTree(tree);

  return block->maxSize >= *sizeP;
}


/* cbsUpdateFastNode -- update size info after restructuring */

static void cbsUpdateFastNode(SplayTree splay, Tree tree)
{
  Size maxSize;

  AVERT_CRITICAL(SplayTree, splay);
  AVERT_CRITICAL(Tree, tree);
  AVER_CRITICAL(IsA(CBSFast, cbsOfSplay(splay)));

  maxSize = RangeTreeSize(RangeTreeOfTree(tree));

  if (TreeHasLeft(tree)) {
    Size size = cbsFastBlockOfTree(TreeLeft(tree))->maxSize;
    if (size > maxSize)
      maxSize = size;
  }

  if (TreeHasRight(tree)) {
    Size size = cbsFastBlockOfTree(TreeRight(tree))->maxSize;
    if (size > maxSize)
      maxSize = size;
  }

  cbsFastBlockOfTree(tree)->maxSize = maxSize;
}


/* cbsUpdateZonedNode -- update size and zone info after restructuring */

static void cbsUpdateZonedNode(SplayTree splay, Tree tree)
{
  ZoneSet zones;
  CBSZonedBlock zonedBlock;
  RangeTree block;
  Arena arena;

  AVERT_CRITICAL(SplayTree, splay);
  AVERT_CRITICAL(Tree, tree);
  AVER_CRITICAL(IsA(CBSZoned, cbsOfSplay(splay)));

  cbsUpdateFastNode(splay, tree);

  zonedBlock = cbsZonedBlockOfTree(tree);
  block = cbsZonedBlockNode(zonedBlock);
  arena = LandArena(CBSLand(cbsOfSplay(splay)));
  zones = ZoneSetOfRange(arena, RangeTreeBase(block), RangeTreeLimit(block));

  if (TreeHasLeft(tree))
    zones = ZoneSetUnion(zones, cbsZonedBlockOfTree(TreeLeft(tree))->zones);

  if (TreeHasRight(tree))
    zones = ZoneSetUnion(zones, cbsZonedBlockOfTree(TreeRight(tree))->zones);

  zonedBlock->zones = zones;
}


/* cbsInit -- Initialise a CBS structure
 *
 * <design/land#.function.init>.
 */

ARG_DEFINE_KEY(cbs_block_pool, Pool);

static Res cbsInitComm(Land land, LandClass klass,
                       Arena arena, Align alignment,
                       ArgList args, SplayUpdateNodeFunction update,
                       Size blockStructSize)
{
  CBS cbs;
  ArgStruct arg;
  Res res;
  Pool blockPool = NULL;

  AVER(land != NULL);
  res = NextMethod(Land, CBS, init)(land, arena, alignment, args);
  if (res != ResOK)
    return res;
  cbs = CouldBeA(CBS, land);

  if (ArgPick(&arg, args, CBSBlockPool))
    blockPool = arg.val.pool;

  SplayTreeInit(cbsSplay(cbs), RangeTreeCompare, RangeTreeKey, update);

  if (blockPool != NULL) {
    cbs->blockPool = blockPool;
    cbs->ownPool = FALSE;
  } else {
    MPS_ARGS_BEGIN(pcArgs) {
      MPS_ARGS_ADD(pcArgs, MPS_KEY_MFS_UNIT_SIZE, blockStructSize);
      res = PoolCreate(&cbs->blockPool, LandArena(land), PoolClassMFS(), pcArgs);
    } MPS_ARGS_END(pcArgs);
    if (res != ResOK)
      return res;
    cbs->ownPool = TRUE;
  }
  STATISTIC(cbs->treeSize = 0);
  cbs->size = 0;

  cbs->blockStructSize = blockStructSize;

  METER_INIT(cbs->treeSearch, "size of tree", (void *)cbs);

  SetClassOfPoly(land, klass);
  cbs->sig = CBSSig;
  AVERC(CBS, cbs);

  return ResOK;
}

static Res cbsInit(Land land, Arena arena, Align alignment, ArgList args)
{
  return cbsInitComm(land, CLASS(CBS), arena, alignment,
                     args, SplayTrivUpdate,
                     sizeof(RangeTreeStruct));
}

static Res cbsInitFast(Land land, Arena arena, Align alignment, ArgList args)
{
  return cbsInitComm(land, CLASS(CBSFast), arena, alignment,
                     args, cbsUpdateFastNode,
                     sizeof(CBSFastBlockStruct));
}

static Res cbsInitZoned(Land land, Arena arena, Align alignment, ArgList args)
{
  return cbsInitComm(land, CLASS(CBSZoned), arena, alignment,
                     args, cbsUpdateZonedNode,
                     sizeof(CBSZonedBlockStruct));
}


/* cbsFinish -- Finish a CBS structure
 *
 * <design/land#.function.finish>.
 */

static void cbsFinish(Inst inst)
{
  Land land = MustBeA(Land, inst);
  CBS cbs = MustBeA(CBS, land);

  METER_EMIT(&cbs->treeSearch);

  cbs->sig = SigInvalid;

  SplayTreeFinish(cbsSplay(cbs));
  if (cbs->ownPool)
    PoolDestroy(cbsBlockPool(cbs));

  NextMethod(Inst, CBS, finish)(inst);
}


/* cbsSize -- total size of ranges in CBS
 *
 * <design/land#.function.size>.
 */

static Size cbsSize(Land land)
{
  CBS cbs = MustBeA_CRITICAL(CBS, land);
  return cbs->size;
}

/* cbsBlockDestroy -- destroy a block */

static void cbsBlockDestroy(CBS cbs, RangeTree block)
{
  Size size;

  AVERT(CBS, cbs);
  AVERT(RangeTree, block);
  size = RangeTreeSize(block);

  STATISTIC(--cbs->treeSize);
  AVER(cbs->size >= size);
  cbs->size -= size;

  RangeTreeFinish(block);
  PoolFree(cbsBlockPool(cbs), (Addr)block, cbs->blockStructSize);
}


/* RangeTree change operators
 *
 * These four functions are called whenever blocks are created,
 * destroyed, grow, or shrink.  They maintain the maxSize if fastFind is
 * enabled.
 */

static void cbsBlockDelete(CBS cbs, RangeTree block)
{
  Bool b;

  AVERT(CBS, cbs);
  AVERT(RangeTree, block);

  METER_ACC(cbs->treeSearch, cbs->treeSize);
  b = SplayTreeDelete(cbsSplay(cbs), RangeTreeTree(block));
  AVER(b); /* expect block to be in the tree */
  cbsBlockDestroy(cbs, block);
}

static void cbsBlockShrunk(CBS cbs, RangeTree block, Size oldSize)
{
  Size newSize;

  AVERT(CBS, cbs);
  AVERT(RangeTree, block);

  newSize = RangeTreeSize(block);
  AVER(oldSize > newSize);
  AVER(cbs->size >= oldSize - newSize);

  SplayNodeRefresh(cbsSplay(cbs), RangeTreeTree(block));
  cbs->size -= oldSize - newSize;
}

static void cbsBlockGrew(CBS cbs, RangeTree block, Size oldSize)
{
  Size newSize;

  AVERT(CBS, cbs);
  AVERT(RangeTree, block);

  newSize = RangeTreeSize(block);
  AVER(oldSize < newSize);

  SplayNodeRefresh(cbsSplay(cbs), RangeTreeTree(block));
  cbs->size += newSize - oldSize;
}

/* cbsBlockAlloc -- allocate a new block and set its base and limit,
   but do not insert it into the tree yet */

static Res cbsBlockAlloc(RangeTree *blockReturn, CBS cbs, Range range)
{
  Res res;
  RangeTree block;
  Addr p;

  AVER(blockReturn != NULL);
  AVERT(CBS, cbs);
  AVERT(Range, range);

  res = PoolAlloc(&p, cbsBlockPool(cbs), cbs->blockStructSize);
  if (res != ResOK)
    goto failPoolAlloc;
  block = (RangeTree)p;

  RangeTreeInit(block, range);

  SplayNodeInit(cbsSplay(cbs), RangeTreeTree(block));

  AVERT(RangeTree, block);
  *blockReturn = block;
  return ResOK;

failPoolAlloc:
  AVER(res != ResOK);
  return res;
}

/* cbsBlockInsert -- insert a block into the tree */

static void cbsBlockInsert(CBS cbs, RangeTree block)
{
  Bool b;

  AVERT_CRITICAL(CBS, cbs);
  AVERT_CRITICAL(RangeTree, block);

  METER_ACC(cbs->treeSearch, cbs->treeSize);
  b = SplayTreeInsert(cbsSplay(cbs), RangeTreeTree(block));
  AVER_CRITICAL(b);
  STATISTIC(++cbs->treeSize);
  cbs->size += RangeTreeSize(block);
}


/* cbsInsert -- Insert a range into the CBS
 *
 * <design/cbs#.functions.cbs.insert>.
 *
 * .insert.alloc: Will only allocate a block if the range does not
 * abut an existing range.
 */

static Res cbsInsert(Range rangeReturn, Land land, Range range)
{
  CBS cbs = MustBeA_CRITICAL(CBS, land);
  Bool b;
  Res res;
  Addr base, limit, newBase, newLimit;
  Tree leftSplay, rightSplay;
  RangeTree leftBlock, rightBlock;
  Bool leftMerge, rightMerge;
  Size oldSize;

  AVER_CRITICAL(rangeReturn != NULL);
  AVERT_CRITICAL(Range, range);
  AVER_CRITICAL(!RangeIsEmpty(range));
  AVER_CRITICAL(RangeIsAligned(range, LandAlignment(land)));

  base = RangeBase(range);
  limit = RangeLimit(range);

  METER_ACC(cbs->treeSearch, cbs->treeSize);
  b = SplayTreeNeighbours(&leftSplay, &rightSplay, cbsSplay(cbs),
                          RangeTreeKeyOfBaseVar(base));
  if (!b) {
    res = ResFAIL;
    goto fail;
  }

  /* .insert.overlap: The two cases below are not quite symmetrical,
     because base was passed into the call to SplayTreeNeighbours, but
     limit was not. So we know that if there is a left neighbour, then
     leftBlock's limit <= base (this is ensured by RangeTreeCompare,
     which is the comparison method on the tree). But if there is a
     right neighbour, all we know is that base < rightBlock's base. But
     for the range to fit, we need limit <= rightBlock's base too. Hence
     the extra check and the possibility of failure in the second
     case. */

  if (leftSplay == TreeEMPTY) {
    leftBlock = NULL;
    leftMerge = FALSE;
  } else {
    leftBlock = RangeTreeOfTree(leftSplay);
    AVER_CRITICAL(RangeTreeLimit(leftBlock) <= base);
    leftMerge = RangeTreeLimit(leftBlock) == base;
  }

  if (rightSplay == TreeEMPTY) {
    rightBlock = NULL;
    rightMerge = FALSE;
  } else {
    rightBlock = RangeTreeOfTree(rightSplay);
    if (rightBlock != NULL && limit > RangeTreeBase(rightBlock)) {
      /* .insert.overlap */
      res = ResFAIL;
      goto fail;
    }
    rightMerge = RangeTreeBase(rightBlock) == limit;
  }

  newBase = leftMerge ? RangeTreeBase(leftBlock) : base;
  newLimit = rightMerge ? RangeTreeLimit(rightBlock) : limit;

  if (leftMerge && rightMerge) {
    Size oldLeftSize = RangeTreeSize(leftBlock);
    Addr rightLimit = RangeTreeLimit(rightBlock);
    cbsBlockDelete(cbs, rightBlock);
    RangeTreeSetLimit(leftBlock, rightLimit);
    cbsBlockGrew(cbs, leftBlock, oldLeftSize);

  } else if (leftMerge) {
    oldSize = RangeTreeSize(leftBlock);
    RangeTreeSetLimit(leftBlock, limit);
    cbsBlockGrew(cbs, leftBlock, oldSize);

  } else if (rightMerge) {
    oldSize = RangeTreeSize(rightBlock);
    RangeTreeSetBase(rightBlock, base);
    cbsBlockGrew(cbs, rightBlock, oldSize);

  } else {
    RangeTree block;
    res = cbsBlockAlloc(&block, cbs, range);
    if (res != ResOK)
      goto fail;
    cbsBlockInsert(cbs, block);
  }

  AVER_CRITICAL(newBase <= base);
  AVER_CRITICAL(newLimit >= limit);
  RangeInit(rangeReturn, newBase, newLimit);

  return ResOK;

fail:
  AVER_CRITICAL(res != ResOK);
  return res;
}


/* cbsExtendBlockPool -- extend block pool with memory */

static void cbsExtendBlockPool(CBS cbs, Addr base, Addr limit)
{
  Tract tract;
  Addr addr;

  AVERC(CBS, cbs);
  AVER(base < limit);

  /* Steal tracts from their owning pool */
  TRACT_FOR(tract, addr, CBSLand(cbs)->arena, base, limit) {
    TractFinish(tract);
    TractInit(tract, cbs->blockPool, addr);
  }

  /* Extend the block pool with the stolen memory. */
  MFSExtend(cbs->blockPool, base, limit);
}


/* cbsInsertSteal -- Insert a range into the CBS, possibly stealing
 * memory for the block pool
 */

static Res cbsInsertSteal(Range rangeReturn, Land land, Range rangeIO)
{
  CBS cbs = MustBeA(CBS, land);
  Arena arena = land->arena;
  Size grainSize = ArenaGrainSize(arena);
  Res res;

  AVER(rangeReturn != NULL);
  AVER(rangeReturn != rangeIO);
  AVERT(Range, rangeIO);
  AVER(!RangeIsEmpty(rangeIO));
  AVER(RangeIsAligned(rangeIO, LandAlignment(land)));
  AVER(AlignIsAligned(LandAlignment(land), grainSize));

  res = cbsInsert(rangeReturn, land, rangeIO);
  if (res != ResOK && res != ResFAIL) {
    /* Steal an arena grain and use it to extend the block pool. */
    Addr stolenBase = RangeBase(rangeIO);
    Addr stolenLimit = AddrAdd(stolenBase, grainSize);
    cbsExtendBlockPool(cbs, stolenBase, stolenLimit);

    /* Update the inserted range and try again. */
    RangeSetBase(rangeIO, stolenLimit);
    AVERT(Range, rangeIO);
    if (RangeIsEmpty(rangeIO)) {
      RangeCopy(rangeReturn, rangeIO);
      res = ResOK;
    } else {
      res = cbsInsert(rangeReturn, land, rangeIO);
      AVER(res == ResOK);  /* since we just extended the block pool */
    }
  }
  return res;
}


/* cbsDelete -- Remove a range from a CBS
 *
 * <design/land#.function.delete>.
 *
 * .delete.alloc: Will only allocate a block if the range splits
 * an existing range.
 */

static Res cbsDelete(Range rangeReturn, Land land, Range range)
{
  CBS cbs = MustBeA(CBS, land);
  Res res;
  RangeTree block;
  Tree tree;
  Addr base, limit, oldBase, oldLimit;
  Size oldSize;

  AVER(rangeReturn != NULL);
  AVERT(Range, range);
  AVER(!RangeIsEmpty(range));
  AVER(RangeIsAligned(range, LandAlignment(land)));

  base = RangeBase(range);
  limit = RangeLimit(range);

  METER_ACC(cbs->treeSearch, cbs->treeSize);
  if (!SplayTreeFind(&tree, cbsSplay(cbs), RangeTreeKeyOfBaseVar(base))) {
    res = ResFAIL;
    goto failSplayTreeSearch;
  }
  block = RangeTreeOfTree(tree);

  if (limit > RangeTreeLimit(block)) {
    res = ResFAIL;
    goto failLimitCheck;
  }

  oldBase = RangeTreeBase(block);
  oldLimit = RangeTreeLimit(block);
  oldSize = RangeTreeSize(block);
  RangeInit(rangeReturn, oldBase, oldLimit);

  if (base == oldBase && limit == oldLimit) {
    /* entire block */
    cbsBlockDelete(cbs, block);

  } else if (base == oldBase) {
    /* remaining fragment at right */
    AVER(limit < oldLimit);
    RangeTreeSetBase(block, limit);
    cbsBlockShrunk(cbs, block, oldSize);

  } else if (limit == oldLimit) {
    /* remaining fragment at left */
    AVER(base > oldBase);
    RangeTreeSetLimit(block, base);
    cbsBlockShrunk(cbs, block, oldSize);

  } else {
    /* two remaining fragments. shrink block to represent fragment at
       left, and create new block for fragment at right. */
    RangeStruct newRange;
    RangeTree newBlock;
    AVER(base > oldBase);
    AVER(limit < oldLimit);
    RangeInit(&newRange, limit, oldLimit);
    res = cbsBlockAlloc(&newBlock, cbs, &newRange);
    if (res != ResOK) {
      goto failAlloc;
    }
    RangeTreeSetLimit(block, base);
    cbsBlockShrunk(cbs, block, oldSize);
    cbsBlockInsert(cbs, newBlock);
  }

  return ResOK;

failAlloc:
failLimitCheck:
failSplayTreeSearch:
  AVER(res != ResOK);
  return res;
}


static Res cbsDeleteSteal(Range rangeReturn, Land land, Range range)
{
  CBS cbs = MustBeA(CBS, land);
  Arena arena = land->arena;
  Size grainSize = ArenaGrainSize(arena);
  RangeStruct containingRange;
  Res res;

  AVER(rangeReturn != NULL);
  AVERT(Range, range);
  AVER(!RangeIsEmpty(range));
  AVER(RangeIsAligned(range, LandAlignment(land)));
  AVER(AlignIsAligned(LandAlignment(land), grainSize));

  res = cbsDelete(&containingRange, land, range);
  if (res == ResOK) {
    RangeCopy(rangeReturn, &containingRange);
  } else if (res != ResFAIL) {
    /* Steal an arena grain from the base of the containing range and
       use it to extend the block pool. */
    Addr stolenBase = RangeBase(&containingRange);
    Addr stolenLimit = AddrAdd(stolenBase, grainSize);
    RangeStruct stolenRange;
    AVER(stolenLimit <= RangeBase(range));
    RangeInit(&stolenRange, stolenBase, stolenLimit);
    res = cbsDelete(&containingRange, land, &stolenRange);
    AVER(res == ResOK);  /* since this does not split any range */
    cbsExtendBlockPool(cbs, stolenBase, stolenLimit);

    /* Try again with original range. */
    res = cbsDelete(rangeReturn, land, range);
    AVER(res == ResOK);  /* since we just extended the block pool */
  }
  return res;
}


static Res cbsBlockDescribe(RangeTree block, mps_lib_FILE *stream)
{
  Res res;

  if (stream == NULL)
    return ResFAIL;

  res = WriteF(stream, 0,
               "[$P,$P)",
               (WriteFP)RangeTreeBase(block),
               (WriteFP)RangeTreeLimit(block),
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

  res = cbsBlockDescribe(RangeTreeOfTree(tree), stream);
  return res;
}

static Res cbsFastBlockDescribe(CBSFastBlock block, mps_lib_FILE *stream)
{
  Res res;

  if (stream == NULL)
    return ResFAIL;

  res = WriteF(stream, 0,
               "[$P,$P) {$U}",
               (WriteFP)RangeTreeBase(cbsFastBlockNode(block)),
               (WriteFP)RangeTreeLimit(cbsFastBlockNode(block)),
               (WriteFU)block->maxSize,
               NULL);
  return res;
}

static Res cbsFastSplayNodeDescribe(Tree tree, mps_lib_FILE *stream)
{
  Res res;

  if (tree == TreeEMPTY)
    return ResFAIL;
  if (stream == NULL)
    return ResFAIL;

  res = cbsFastBlockDescribe(cbsFastBlockOfTree(tree), stream);
  return res;
}

static Res cbsZonedBlockDescribe(CBSZonedBlock block, mps_lib_FILE *stream)
{
  Res res;

  if (stream == NULL)
    return ResFAIL;

  res = WriteF(stream, 0,
               "[$P,$P) {$U, $B}",
               (WriteFP)RangeTreeBase(cbsZonedBlockNode(block)),
               (WriteFP)RangeTreeLimit(cbsZonedBlockNode(block)),
               (WriteFU)block->cbsFastBlockStruct.maxSize,
               (WriteFB)block->zones,
               NULL);
  return res;
}

static Res cbsZonedSplayNodeDescribe(Tree tree, mps_lib_FILE *stream)
{
  Res res;

  if (tree == TreeEMPTY)
    return ResFAIL;
  if (stream == NULL)
    return ResFAIL;

  res = cbsZonedBlockDescribe(cbsZonedBlockOfTree(tree), stream);
  return res;
}


/* cbsIterate -- iterate over all blocks in CBS
 *
 * <design/land#.function.iterate>.
 */

typedef struct CBSIterateClosure {
  Land land;
  LandVisitor visitor;
  void *visitorClosure;
} CBSIterateClosure;

static Bool cbsIterateVisit(Tree tree, void *closure)
{
  CBSIterateClosure *my = closure;
  Land land = my->land;
  RangeTree block = RangeTreeOfTree(tree);
  RangeStruct range;
  RangeInit(&range, RangeTreeBase(block), RangeTreeLimit(block));
  return my->visitor(land, &range, my->visitorClosure);
}

static Bool cbsIterate(Land land, LandVisitor visitor, void *visitorClosure)
{
  CBS cbs = MustBeA(CBS, land);
  SplayTree splay;
  CBSIterateClosure iterateClosure;

  AVER(FUNCHECK(visitor));

  splay = cbsSplay(cbs);
  /* .splay-iterate.slow: We assume that splay tree iteration does */
  /* searches and meter it. */
  METER_ACC(cbs->treeSearch, cbs->treeSize);

  iterateClosure.land = land;
  iterateClosure.visitor = visitor;
  iterateClosure.visitorClosure = visitorClosure;
  return TreeTraverse(SplayTreeRoot(splay), splay->compare, splay->nodeKey,
                      cbsIterateVisit, &iterateClosure);
}


/* cbsIterateAndDelete -- iterate over all blocks in CBS
 *
 * <design/land#.function.iterate.and.delete>.
 */

typedef struct CBSIterateAndDeleteClosure {
  Land land;
  LandDeleteVisitor visitor;
  Bool cont;
  void *visitorClosure;
} CBSIterateAndDeleteClosure;

static Bool cbsIterateAndDeleteVisit(Tree tree, void *closure)
{
  CBSIterateAndDeleteClosure *my = closure;
  Land land = my->land;
  CBS cbs = MustBeA(CBS, land);
  RangeTree block = RangeTreeOfTree(tree);
  Bool deleteNode = FALSE;
  RangeStruct range;

  RangeInit(&range, RangeTreeBase(block), RangeTreeLimit(block));
  if (my->cont)
    my->cont = my->visitor(&deleteNode, land, &range,
                           my->visitorClosure);
  if (deleteNode)
    cbsBlockDestroy(cbs, block);
  return deleteNode;
}

static Bool cbsIterateAndDelete(Land land, LandDeleteVisitor visitor,
                                void *visitorClosure)
{
  CBS cbs = MustBeA(CBS, land);
  SplayTree splay;
  CBSIterateAndDeleteClosure iterateClosure;

  AVER(FUNCHECK(visitor));

  splay = cbsSplay(cbs);
  /* .splay-iterate.slow: We assume that splay tree iteration does */
  /* searches and meter it. */
  METER_ACC(cbs->treeSearch, cbs->treeSize);

  iterateClosure.land = land;
  iterateClosure.visitor = visitor;
  iterateClosure.visitorClosure = visitorClosure;
  iterateClosure.cont = TRUE;
  TreeTraverseAndDelete(&splay->root, cbsIterateAndDeleteVisit,
                        &iterateClosure);
  return iterateClosure.cont;
}


/* cbsFindDeleteRange -- delete appropriate range of block found */

static void cbsFindDeleteRange(Range rangeReturn, Range oldRangeReturn,
                               Land land, Range range, Size size,
                               FindDelete findDelete)
{
  Bool callDelete = TRUE;
  Addr base, limit;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Land, land);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, LandAlignment(land)));
  AVER(size > 0);
  AVER(SizeIsAligned(size, LandAlignment(land)));
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
    res = cbsDelete(oldRangeReturn, land, rangeReturn);
    /* Can't have run out of memory, because all our callers pass in
       blocks that were just found in the tree, and we only
       deleted from one end of the block, so cbsDelete did not
       need to allocate a new block. */
    AVER(res == ResOK);
  } else {
    RangeCopy(oldRangeReturn, rangeReturn);
  }
}


/* CBSFindFirst -- find the first block of at least the given size */

static Bool cbsFindFirst(Range rangeReturn, Range oldRangeReturn,
                         Land land, Size size, FindDelete findDelete)
{
  CBS cbs = MustBeA_CRITICAL(CBS, land);
  Bool found;
  Tree tree;

  AVER_CRITICAL(rangeReturn != NULL);
  AVER_CRITICAL(oldRangeReturn != NULL);
  AVER_CRITICAL(size > 0);
  AVER_CRITICAL(SizeIsAligned(size, LandAlignment(land)));
  AVERT_CRITICAL(FindDelete, findDelete);

  METER_ACC(cbs->treeSearch, cbs->treeSize);
  found = SplayFindFirst(&tree, cbsSplay(cbs), &cbsTestNode,
                         &cbsTestTree, &size);
  if (found) {
    RangeTree block;
    RangeStruct range;
    block = RangeTreeOfTree(tree);
    AVER_CRITICAL(RangeTreeSize(block) >= size);
    RangeInit(&range, RangeTreeBase(block), RangeTreeLimit(block));
    AVER_CRITICAL(RangeSize(&range) >= size);
    cbsFindDeleteRange(rangeReturn, oldRangeReturn, land, &range,
                       size, findDelete);
  }

  return found;
}


/* cbsFindInZones -- find a block within a zone set
 *
 * Finds a block of at least the given size that lies entirely within a
 * zone set. (The first such block, if high is FALSE, or the last, if
 * high is TRUE.)
 */

typedef struct cbsTestNodeInZonesClosureStruct {
  Size size;
  Arena arena;
  ZoneSet zoneSet;
  Addr base;
  Addr limit;
  Bool high;
} cbsTestNodeInZonesClosureStruct, *cbsTestNodeInZonesClosure;

static Bool cbsTestNodeInZones(SplayTree splay, Tree tree,
                               void *closure)
{
  RangeTree block = RangeTreeOfTree(tree);
  cbsTestNodeInZonesClosure my = closure;
  RangeInZoneSet search;

  AVER_CRITICAL(closure != NULL);
  UNUSED(splay);

  search = my->high ? RangeInZoneSetLast : RangeInZoneSetFirst;

  return search(&my->base, &my->limit,
                RangeTreeBase(block), RangeTreeLimit(block),
                my->arena, my->zoneSet, my->size);
}

static Bool cbsTestTreeInZones(SplayTree splay, Tree tree,
                               void *closure)
{
  CBSFastBlock fastBlock = cbsFastBlockOfTree(tree);
  CBSZonedBlock zonedBlock = cbsZonedBlockOfTree(tree);
  cbsTestNodeInZonesClosure my = closure;

  AVER_CRITICAL(closure != NULL);
  UNUSED(splay);

  return fastBlock->maxSize >= my->size
    && ZoneSetInter(zonedBlock->zones, my->zoneSet) != ZoneSetEMPTY;
}


/* cbsFindLast -- find the last block of at least the given size */

static Bool cbsFindLast(Range rangeReturn, Range oldRangeReturn,
                        Land land, Size size, FindDelete findDelete)
{
  CBS cbs = MustBeA_CRITICAL(CBSFast, land);
  Bool found;
  Tree tree;

  AVER_CRITICAL(rangeReturn != NULL);
  AVER_CRITICAL(oldRangeReturn != NULL);
  AVER_CRITICAL(size > 0);
  AVER_CRITICAL(SizeIsAligned(size, LandAlignment(land)));
  AVERT_CRITICAL(FindDelete, findDelete);

  METER_ACC(cbs->treeSearch, cbs->treeSize);
  found = SplayFindLast(&tree, cbsSplay(cbs), &cbsTestNode,
                        &cbsTestTree, &size);
  if (found) {
    RangeTree block;
    RangeStruct range;
    block = RangeTreeOfTree(tree);
    AVER_CRITICAL(RangeTreeSize(block) >= size);
    RangeInit(&range, RangeTreeBase(block), RangeTreeLimit(block));
    AVER_CRITICAL(RangeSize(&range) >= size);
    cbsFindDeleteRange(rangeReturn, oldRangeReturn, land, &range,
                       size, findDelete);
  }

  return found;
}


/* cbsFindLargest -- find the largest block in the CBS */

static Bool cbsFindLargest(Range rangeReturn, Range oldRangeReturn,
                           Land land, Size size, FindDelete findDelete)
{
  CBS cbs = MustBeA_CRITICAL(CBSFast, land);
  Bool found = FALSE;

  AVER_CRITICAL(rangeReturn != NULL);
  AVER_CRITICAL(oldRangeReturn != NULL);
  AVER_CRITICAL(size > 0);
  AVERT_CRITICAL(FindDelete, findDelete);

  if (!SplayTreeIsEmpty(cbsSplay(cbs))) {
    RangeStruct range;
    Tree tree = TreeEMPTY;    /* suppress "may be used uninitialized" */
    Size maxSize;

    maxSize = cbsFastBlockOfTree(SplayTreeRoot(cbsSplay(cbs)))->maxSize;
    if (maxSize >= size) {
      RangeTree block;
      METER_ACC(cbs->treeSearch, cbs->treeSize);
      found = SplayFindFirst(&tree, cbsSplay(cbs), &cbsTestNode,
                             &cbsTestTree, &maxSize);
      AVER_CRITICAL(found); /* maxSize is exact, so we will find it. */
      block = RangeTreeOfTree(tree);
      AVER_CRITICAL(RangeTreeSize(block) >= maxSize);
      RangeInit(&range, RangeTreeBase(block), RangeTreeLimit(block));
      AVER_CRITICAL(RangeSize(&range) >= maxSize);
      cbsFindDeleteRange(rangeReturn, oldRangeReturn, land, &range,
                         size, findDelete);
    }
  }

  return found;
}


static Res cbsFindInZones(Bool *foundReturn, Range rangeReturn,
                          Range oldRangeReturn, Land land, Size size,
                          ZoneSet zoneSet, Bool high)
{
  CBS cbs = MustBeA_CRITICAL(CBSZoned, land);
  RangeTree block;
  Tree tree;
  cbsTestNodeInZonesClosureStruct closure;
  Res res;
  LandFindMethod landFind;
  SplayFindFunction splayFind;
  RangeStruct rangeStruct, oldRangeStruct;

  AVER_CRITICAL(foundReturn != NULL);
  AVER_CRITICAL(rangeReturn != NULL);
  AVER_CRITICAL(oldRangeReturn != NULL);
  /* AVERT_CRITICAL(ZoneSet, zoneSet); */
  AVERT_CRITICAL(Bool, high);

  landFind = high ? cbsFindLast : cbsFindFirst;
  splayFind = high ? SplayFindLast : SplayFindFirst;

  if (zoneSet == ZoneSetEMPTY)
    goto fail;
  if (zoneSet == ZoneSetUNIV) {
    FindDelete fd = high ? FindDeleteHIGH : FindDeleteLOW;
    *foundReturn = (*landFind)(rangeReturn, oldRangeReturn, land, size, fd);
    return ResOK;
  }
  if (ZoneSetIsSingle(zoneSet) && size > ArenaStripeSize(LandArena(land)))
    goto fail;

  /* It would be nice if there were a neat way to eliminate all runs of
     zones in zoneSet too small for size.*/

  closure.arena = LandArena(land);
  closure.zoneSet = zoneSet;
  closure.size = size;
  closure.high = high;
  if (!(*splayFind)(&tree, cbsSplay(cbs),
                    cbsTestNodeInZones, cbsTestTreeInZones,
                    &closure))
    goto fail;

  block = RangeTreeOfTree(tree);

  AVER_CRITICAL(RangeTreeBase(block) <= closure.base);
  AVER_CRITICAL(AddrOffset(closure.base, closure.limit) >= size);
  AVER_CRITICAL(ZoneSetSub(ZoneSetOfRange(LandArena(land), closure.base, closure.limit), zoneSet));
  AVER_CRITICAL(closure.limit <= RangeTreeLimit(block));

  if (!high)
    RangeInit(&rangeStruct, closure.base, AddrAdd(closure.base, size));
  else
    RangeInit(&rangeStruct, AddrSub(closure.limit, size), closure.limit);
  res = cbsDelete(&oldRangeStruct, land, &rangeStruct);
  if (res != ResOK)
    /* not enough memory to split block */
    return res;
  RangeCopy(rangeReturn, &rangeStruct);
  RangeCopy(oldRangeReturn, &oldRangeStruct);
  *foundReturn = TRUE;
  return ResOK;

fail:
  *foundReturn = FALSE;
  return ResOK;
}


/* cbsDescribe -- describe a CBS
 *
 * <design/land#.function.describe>.
 */

static Res cbsDescribe(Inst inst, mps_lib_FILE *stream, Count depth)
{
  Land land = CouldBeA(Land, inst);
  CBS cbs = CouldBeA(CBS, land);
  Res res;
  Res (*describe)(Tree, mps_lib_FILE *);

  if (!TESTC(CBS, cbs))
    return ResPARAM;
  if (stream == NULL)
    return ResPARAM;

  res = NextMethod(Inst, CBS, describe)(inst, stream, depth);
  if (res != ResOK)
    return res;

  res = WriteF(stream, depth + 2,
               "blockPool $P\n", (WriteFP)cbsBlockPool(cbs),
               "ownPool   $U\n", (WriteFU)cbs->ownPool,
               STATISTIC_WRITE("  treeSize: $U\n", (WriteFU)cbs->treeSize)
               NULL);
  if (res != ResOK)
    return res;

  METER_WRITE(cbs->treeSearch, stream, depth + 2);

  /* This could be done by specialised methods in subclasses, but it
     doesn't really come out any neater. */
  if (IsA(CBSZoned, land))
    describe = cbsZonedSplayNodeDescribe;
  else if (IsA(CBSFast, land))
    describe = cbsFastSplayNodeDescribe;
  else
    describe = cbsSplayNodeDescribe;

  res = SplayTreeDescribe(cbsSplay(cbs), stream, depth + 2, describe);
  if (res != ResOK)
    return res;

  return res;
}

DEFINE_CLASS(Land, CBS, klass)
{
  INHERIT_CLASS(klass, CBS, Land);
  klass->instClassStruct.describe = cbsDescribe;
  klass->instClassStruct.finish = cbsFinish;
  klass->size = sizeof(CBSStruct);
  klass->init = cbsInit;
  klass->sizeMethod = cbsSize;
  klass->insert = cbsInsert;
  klass->insertSteal = cbsInsertSteal;
  klass->delete = cbsDelete;
  klass->deleteSteal = cbsDeleteSteal;
  klass->iterate = cbsIterate;
  klass->iterateAndDelete = cbsIterateAndDelete;
  klass->findFirst = cbsFindFirst;
  klass->findLast = cbsFindLast;
  klass->findLargest = cbsFindLargest;
  klass->findInZones = cbsFindInZones;
  AVERT(LandClass, klass);
}

DEFINE_CLASS(Land, CBSFast, klass)
{
  INHERIT_CLASS(klass, CBSFast, CBS);
  klass->init = cbsInitFast;
  AVERT(LandClass, klass);
}

DEFINE_CLASS(Land, CBSZoned, klass)
{
  INHERIT_CLASS(klass, CBSZoned, CBSFast);
  klass->init = cbsInitZoned;
  AVERT(LandClass, klass);
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
