/* cbs.c: COALESCING BLOCK STRUCTURE IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2001-2015 Ravenbrook Limited.  See end of file for license.
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
#define cbsFastBlockOfTree(_tree) \
  PARENT(CBSFastBlockStruct, cbsBlockStruct, cbsBlockOfTree(_tree))
#define cbsZonedBlockOfTree(_tree) \
  PARENT(CBSZonedBlockStruct, cbsFastBlockStruct, cbsFastBlockOfTree(_tree))
#define cbsBlockPool(cbs) RVALUE((cbs)->blockPool)

/* We pass the block base directly as a TreeKey (void *) assuming that
   Addr can be encoded, and possibly breaking <design/type/#addr.use>.
   On an exotic platform where this isn't true, pass the address of base.
   i.e. add an & */
#define cbsBlockKey(block)  ((TreeKey)(block)->base)
#define keyOfBaseVar(baseVar) ((TreeKey)(baseVar))
#define baseOfKey(key)        ((Addr)(key))


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
  STATISTIC_STAT({CHECKL((cbs->size == 0) == (cbs->treeSize == 0));});

  return TRUE;
}


ATTRIBUTE_UNUSED
static Bool CBSBlockCheck(CBSBlock block)
{
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

  AVERT_CRITICAL(Tree, tree);
  AVER_CRITICAL(tree != TreeEMPTY);
  AVER_CRITICAL(key != NULL);

  base1 = baseOfKey(key);
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

static Bool cbsTestNode(SplayTree splay, Tree tree, void *closure)
{
  CBSBlock block;
  Size *sizeP = closure;

  AVERT(SplayTree, splay);
  AVERT(Tree, tree);
  AVER(sizeP != NULL);
  AVER(*sizeP > 0);
  AVER(IsA(CBSFast, CBSLand(cbsOfSplay(splay))));

  block = cbsBlockOfTree(tree);

  return CBSBlockSize(block) >= *sizeP;
}

static Bool cbsTestTree(SplayTree splay, Tree tree,
                        void *closure)
{
  CBSFastBlock block;
  Size *sizeP = closure;

  AVERT(SplayTree, splay);
  AVERT(Tree, tree);
  AVER(sizeP != NULL);
  AVER(*sizeP > 0);
  AVER(IsA(CBSFast, CBSLand(cbsOfSplay(splay))));

  block = cbsFastBlockOfTree(tree);

  return block->maxSize >= *sizeP;
}


/* cbsUpdateFastNode -- update size info after restructuring */

static void cbsUpdateFastNode(SplayTree splay, Tree tree)
{
  Size maxSize;

  AVERT_CRITICAL(SplayTree, splay);
  AVERT_CRITICAL(Tree, tree);
  AVER_CRITICAL(IsA(CBSFast, CBSLand(cbsOfSplay(splay))));

  maxSize = CBSBlockSize(cbsBlockOfTree(tree));

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
  CBSBlock block;
  Arena arena;

  AVERT_CRITICAL(SplayTree, splay);
  AVERT_CRITICAL(Tree, tree);
  AVER_CRITICAL(IsA(CBSZoned, CBSLand(cbsOfSplay(splay))));

  cbsUpdateFastNode(splay, tree);

  zonedBlock = cbsZonedBlockOfTree(tree);
  block = &zonedBlock->cbsFastBlockStruct.cbsBlockStruct;
  arena = LandArena(CBSLand(cbsOfSplay(splay)));
  zones = ZoneSetOfRange(arena, CBSBlockBase(block), CBSBlockLimit(block));

  if (TreeHasLeft(tree))
    zones = ZoneSetUnion(zones, cbsZonedBlockOfTree(TreeLeft(tree))->zones);

  if (TreeHasRight(tree))
    zones = ZoneSetUnion(zones, cbsZonedBlockOfTree(TreeRight(tree))->zones);

  zonedBlock->zones = zones;
}


/* cbsInit -- Initialise a CBS structure
 *
 * See <design/land/#function.init>.
 */

ARG_DEFINE_KEY(cbs_block_pool, Pool);

static Res cbsInitComm(Land land, LandClass class,
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

  SplayTreeInit(cbsSplay(cbs), cbsCompare, cbsKey, update);

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
  cbs->treeSize = 0;
  cbs->size = 0;

  cbs->blockStructSize = blockStructSize;

  METER_INIT(cbs->treeSearch, "size of tree", (void *)cbs);

  SetClassOfPoly(land, class);
  cbs->sig = CBSSig;
  AVERC(CBS, cbs);

  return ResOK;
}

static Res cbsInit(Land land, Arena arena, Align alignment, ArgList args)
{
  return cbsInitComm(land, CLASS(CBS), arena, alignment,
                     args, SplayTrivUpdate,
                     sizeof(CBSBlockStruct));
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
 * See <design/land/#function.finish>.
 */

static void cbsFinish(Land land)
{
  CBS cbs = MustBeA(CBS, land);

  METER_EMIT(&cbs->treeSearch);

  cbs->sig = SigInvalid;

  SplayTreeFinish(cbsSplay(cbs));
  if (cbs->ownPool)
    PoolDestroy(cbsBlockPool(cbs));

  NextMethod(Land, CBS, finish)(land);
}


/* cbsSize -- total size of ranges in CBS
 *
 * See <design/land/#function.size>.
 */

static Size cbsSize(Land land)
{
  CBS cbs = MustBeA(CBS, land);
  return cbs->size;
}

/* cbsBlockDestroy -- destroy a block */

static void cbsBlockDestroy(CBS cbs, CBSBlock block)
{
  Size size;

  AVERT(CBS, cbs);
  AVERT(CBSBlock, block);
  size = CBSBlockSize(block);

  STATISTIC(--cbs->treeSize);
  AVER(cbs->size >= size);
  cbs->size -= size;

  /* make invalid */
  block->limit = block->base;
  PoolFree(cbsBlockPool(cbs), (Addr)block, cbs->blockStructSize);
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
  cbsBlockDestroy(cbs, block);
}

static void cbsBlockShrunk(CBS cbs, CBSBlock block, Size oldSize)
{
  Size newSize;

  AVERT(CBS, cbs);
  AVERT(CBSBlock, block);

  newSize = CBSBlockSize(block);
  AVER(oldSize > newSize);
  AVER(cbs->size >= oldSize - newSize);

  SplayNodeRefresh(cbsSplay(cbs), cbsBlockTree(block));
  cbs->size -= oldSize - newSize;
}

static void cbsBlockGrew(CBS cbs, CBSBlock block, Size oldSize)
{
  Size newSize;

  AVERT(CBS, cbs);
  AVERT(CBSBlock, block);

  newSize = CBSBlockSize(block);
  AVER(oldSize < newSize);

  SplayNodeRefresh(cbsSplay(cbs), cbsBlockTree(block));
  cbs->size += newSize - oldSize;
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

  res = PoolAlloc(&p, cbsBlockPool(cbs), cbs->blockStructSize);
  if (res != ResOK)
    goto failPoolAlloc;
  block = (CBSBlock)p;

  TreeInit(cbsBlockTree(block));
  block->base = RangeBase(range);
  block->limit = RangeLimit(range);

  SplayNodeInit(cbsSplay(cbs), cbsBlockTree(block));

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
  cbs->size += CBSBlockSize(block);
}


/* cbsInsert -- Insert a range into the CBS
 *
 * See <design/cbs/#functions.cbs.insert>.
 *
 * .insert.alloc: Will only allocate a block if the range does not
 * abut an existing range.
 */

static Res cbsInsert(Range rangeReturn, Land land, Range range)
{
  CBS cbs = MustBeA(CBS, land);
  Bool b;
  Res res;
  Addr base, limit, newBase, newLimit;
  Tree leftSplay, rightSplay;
  CBSBlock leftCBS, rightCBS;
  Bool leftMerge, rightMerge;
  Size oldSize;

  AVER(rangeReturn != NULL);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, LandAlignment(land)));

  base = RangeBase(range);
  limit = RangeLimit(range);

  METER_ACC(cbs->treeSearch, cbs->treeSize);
  b = SplayTreeNeighbours(&leftSplay, &rightSplay, cbsSplay(cbs), keyOfBaseVar(base));
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


/* cbsDelete -- Remove a range from a CBS
 *
 * See <design/land/#function.delete>.
 *
 * .delete.alloc: Will only allocate a block if the range splits
 * an existing range.
 */

static Res cbsDelete(Range rangeReturn, Land land, Range range)
{
  CBS cbs = MustBeA(CBS, land);
  Res res;
  CBSBlock cbsBlock;
  Tree tree;
  Addr base, limit, oldBase, oldLimit;
  Size oldSize;

  AVER(rangeReturn != NULL);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, LandAlignment(land)));

  base = RangeBase(range);
  limit = RangeLimit(range);

  METER_ACC(cbs->treeSearch, cbs->treeSize);
  if (!SplayTreeFind(&tree, cbsSplay(cbs), keyOfBaseVar(base))) {
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


static Res cbsBlockDescribe(CBSBlock block, mps_lib_FILE *stream)
{
  Res res;

  if (stream == NULL)
    return ResFAIL;

  res = WriteF(stream, 0,
               "[$P,$P)",
               (WriteFP)block->base,
               (WriteFP)block->limit,
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

static Res cbsFastBlockDescribe(CBSFastBlock block, mps_lib_FILE *stream)
{
  Res res;

  if (stream == NULL)
    return ResFAIL;

  res = WriteF(stream, 0,
               "[$P,$P) {$U}",
               (WriteFP)block->cbsBlockStruct.base,
               (WriteFP)block->cbsBlockStruct.limit,
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
               (WriteFP)block->cbsFastBlockStruct.cbsBlockStruct.base,
               (WriteFP)block->cbsFastBlockStruct.cbsBlockStruct.limit,
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
 * See <design/land/#function.iterate>.
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
  CBSBlock cbsBlock = cbsBlockOfTree(tree);
  RangeStruct range;
  RangeInit(&range, CBSBlockBase(cbsBlock), CBSBlockLimit(cbsBlock));
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
 * See <design/land/#function.iterate.and.delete>.
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
  CBSBlock cbsBlock = cbsBlockOfTree(tree);
  Bool deleteNode = FALSE;
  RangeStruct range;

  RangeInit(&range, CBSBlockBase(cbsBlock), CBSBlockLimit(cbsBlock));
  if (my->cont)
    my->cont = my->visitor(&deleteNode, land, &range,
                           my->visitorClosure);
  if (deleteNode)
    cbsBlockDestroy(cbs, cbsBlock);
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
  CBS cbs = MustBeA(CBS, land);
  Bool found;
  Tree tree;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVER(size > 0);
  AVER(SizeIsAligned(size, LandAlignment(land)));
  AVERT(FindDelete, findDelete);

  METER_ACC(cbs->treeSearch, cbs->treeSize);
  found = SplayFindFirst(&tree, cbsSplay(cbs), &cbsTestNode,
                         &cbsTestTree, &size);
  if (found) {
    CBSBlock block;
    RangeStruct range;
    block = cbsBlockOfTree(tree);
    AVER(CBSBlockSize(block) >= size);
    RangeInit(&range, CBSBlockBase(block), CBSBlockLimit(block));
    AVER(RangeSize(&range) >= size);
    cbsFindDeleteRange(rangeReturn, oldRangeReturn, land, &range,
                       size, findDelete);
  }

  return found;
}

/* cbsFindInZones -- find a block of at least the given size that lies
 * entirely within a zone set. (The first such block, if high is
 * FALSE, or the last, if high is TRUE.)
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
  CBSBlock block = cbsBlockOfTree(tree);
  cbsTestNodeInZonesClosure my = closure;
  RangeInZoneSet search;
  
  UNUSED(splay);

  search = my->high ? RangeInZoneSetLast : RangeInZoneSetFirst;

  return search(&my->base, &my->limit,
                CBSBlockBase(block), CBSBlockLimit(block),
                my->arena, my->zoneSet, my->size);
}

static Bool cbsTestTreeInZones(SplayTree splay, Tree tree,
                               void *closure)
{
  CBSFastBlock fastBlock = cbsFastBlockOfTree(tree);
  CBSZonedBlock zonedBlock = cbsZonedBlockOfTree(tree);
  cbsTestNodeInZonesClosure my = closure;
  
  UNUSED(splay);
  
  return fastBlock->maxSize >= my->size
    && ZoneSetInter(zonedBlock->zones, my->zoneSet) != ZoneSetEMPTY;
}


/* cbsFindLast -- find the last block of at least the given size */

static Bool cbsFindLast(Range rangeReturn, Range oldRangeReturn,
                        Land land, Size size, FindDelete findDelete)
{
  CBS cbs = MustBeA(CBSFast, land);
  Bool found;
  Tree tree;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVER(size > 0);
  AVER(SizeIsAligned(size, LandAlignment(land)));
  AVERT(FindDelete, findDelete);

  METER_ACC(cbs->treeSearch, cbs->treeSize);
  found = SplayFindLast(&tree, cbsSplay(cbs), &cbsTestNode,
                        &cbsTestTree, &size);
  if (found) {
    CBSBlock block;
    RangeStruct range;
    block = cbsBlockOfTree(tree);
    AVER(CBSBlockSize(block) >= size);
    RangeInit(&range, CBSBlockBase(block), CBSBlockLimit(block));
    AVER(RangeSize(&range) >= size);
    cbsFindDeleteRange(rangeReturn, oldRangeReturn, land, &range,
                       size, findDelete);
  }

  return found;
}


/* cbsFindLargest -- find the largest block in the CBS */

static Bool cbsFindLargest(Range rangeReturn, Range oldRangeReturn,
                           Land land, Size size, FindDelete findDelete)
{
  CBS cbs = MustBeA(CBSFast, land);
  Bool found = FALSE;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVER(size > 0);
  AVERT(FindDelete, findDelete);

  if (!SplayTreeIsEmpty(cbsSplay(cbs))) {
    RangeStruct range;
    Tree tree = TreeEMPTY;    /* suppress "may be used uninitialized" */
    Size maxSize;

    maxSize = cbsFastBlockOfTree(SplayTreeRoot(cbsSplay(cbs)))->maxSize;
    if (maxSize >= size) {
      CBSBlock block;
      METER_ACC(cbs->treeSearch, cbs->treeSize);
      found = SplayFindFirst(&tree, cbsSplay(cbs), &cbsTestNode,
                             &cbsTestTree, &maxSize);
      AVER(found); /* maxSize is exact, so we will find it. */
      block = cbsBlockOfTree(tree);
      AVER(CBSBlockSize(block) >= maxSize);
      RangeInit(&range, CBSBlockBase(block), CBSBlockLimit(block));
      AVER(RangeSize(&range) >= maxSize);
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
  CBS cbs = MustBeA(CBSZoned, land);
  CBSBlock block;
  Tree tree;
  cbsTestNodeInZonesClosureStruct closure;
  Res res;
  LandFindMethod landFind;
  SplayFindFunction splayFind;
  RangeStruct rangeStruct, oldRangeStruct;
  
  AVER(foundReturn != NULL);
  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  /* AVERT(ZoneSet, zoneSet); */
  AVERT(Bool, high);

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

  block = cbsBlockOfTree(tree);

  AVER(CBSBlockBase(block) <= closure.base);
  AVER(AddrOffset(closure.base, closure.limit) >= size);
  AVER(ZoneSetSub(ZoneSetOfRange(LandArena(land), closure.base, closure.limit), zoneSet));
  AVER(closure.limit <= CBSBlockLimit(block));

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
 * See <design/land/#function.describe>.
 */

static Res cbsDescribe(Land land, mps_lib_FILE *stream, Count depth)
{
  CBS cbs = CouldBeA(CBS, land);
  Res res;
  Res (*describe)(Tree, mps_lib_FILE *);

  if (!TESTC(CBS, cbs))
    return ResPARAM;
  if (stream == NULL)
    return ResPARAM;

  res = NextMethod(Land, CBS, describe)(land, stream, depth);
  if (res != ResOK)
    return res;

  res = WriteF(stream, depth + 2,
               "blockPool $P\n", (WriteFP)cbsBlockPool(cbs),
               "ownPool   $U\n", (WriteFU)cbs->ownPool,
               "treeSize  $U\n", (WriteFU)cbs->treeSize,
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

DEFINE_CLASS(Land, CBS, class)
{
  INHERIT_CLASS(class, CBS, Land);
  class->size = sizeof(CBSStruct);
  class->init = cbsInit;
  class->finish = cbsFinish;
  class->sizeMethod = cbsSize;
  class->insert = cbsInsert;
  class->delete = cbsDelete;
  class->iterate = cbsIterate;
  class->iterateAndDelete = cbsIterateAndDelete;
  class->findFirst = cbsFindFirst;
  class->findLast = cbsFindLast;
  class->findLargest = cbsFindLargest;
  class->findInZones = cbsFindInZones;
  class->describe = cbsDescribe;
}

DEFINE_CLASS(Land, CBSFast, class)
{
  INHERIT_CLASS(class, CBSFast, CBS);
  class->init = cbsInitFast;
}

DEFINE_CLASS(Land, CBSZoned, class)
{
  INHERIT_CLASS(class, CBSZoned, CBSFast);
  class->init = cbsInitZoned;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2015 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
