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
} CBSBlockStruct;


extern Bool CBSBlockCheck(CBSBlock block);
#define CBSBlockBase(block) ((block)->base)
#define CBSBlockLimit(block) ((block)->limit)
#define CBSBlockSize(block) AddrOffset((block)->base, (block)->limit)
extern Size (CBSBlockSize)(CBSBlock block);


#define cbsOfSplayTree(tree) PARENT(CBSStruct, splayTree, (tree))
#define cbsBlockOfSplayNode(node) PARENT(CBSBlockStruct, splayNode, (node))
#define splayTreeOfCBS(tree) (&((cbs)->splayTree))
#define splayNodeOfCBSBlock(block) (&((block)->splayNode))
#define keyOfCBSBlock(block) ((void *)&((block)->base))


/* CBSEnter, CBSLeave -- Avoid re-entrance
 *
 * .enter-leave: The callbacks are restricted in what they may call.
 * These functions enforce this.
 *
 * .enter-leave.simple: Simple queries may be called from callbacks.
 */

static void CBSEnter(CBS cbs)
{
  /* Don't need to check as always called from interface function. */
  AVER(!cbs->inCBS);
  cbs->inCBS = TRUE;
  return;
}

static void CBSLeave(CBS cbs)
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
  CHECKD(Pool, cbs->blockPool);
  CHECKL(BoolCheck(cbs->fastFind));
  CHECKL(BoolCheck(cbs->inCBS));
  /* No MeterCheck */

  return TRUE;
}


Bool CBSBlockCheck(CBSBlock block)
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


Size (CBSBlockSize)(CBSBlock block)
{
  /* See .enter-leave.simple. */
  return CBSBlockSize(block);
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
  AVER(closureP == NULL);
  AVER(size > 0);
  AVER(cbsOfSplayTree(tree)->fastFind);

  block = cbsBlockOfSplayNode(node);

  return block->maxSize >= size;
}


/* cbsUpdateNode -- update size info after restructuring */

static void cbsUpdateNode(SplayTree tree, SplayNode node,
                          SplayNode leftChild, SplayNode rightChild)
{
  Size maxSize;
  CBSBlock block;

  AVERT(SplayTree, tree);
  AVERT(SplayNode, node);
  if (leftChild != NULL)
    AVERT(SplayNode, leftChild);
  if (rightChild != NULL)
    AVERT(SplayNode, rightChild);
  AVER(cbsOfSplayTree(tree)->fastFind);

  block = cbsBlockOfSplayNode(node);
  maxSize = CBSBlockSize(block);

  if (leftChild != NULL) {
    Size size = cbsBlockOfSplayNode(leftChild)->maxSize;
    if (size > maxSize)
      maxSize = size;
  }

  if (rightChild != NULL) {
    Size size = cbsBlockOfSplayNode(rightChild)->maxSize;
    if (size > maxSize)
      maxSize = size;
  }

  block->maxSize = maxSize;
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
  ArgStruct arg;
  Res res;

  AVERT(Arena, arena);

  if (ArgPick(&arg, args, MPS_KEY_CBS_EXTEND_BY))
    extendBy = arg.val.size;

  SplayTreeInit(splayTreeOfCBS(cbs), &cbsSplayCompare,
                fastFind ? &cbsUpdateNode : NULL);
  MPS_ARGS_BEGIN(pcArgs) {
    MPS_ARGS_ADD(pcArgs, MPS_KEY_MFS_UNIT_SIZE, sizeof(CBSBlockStruct));
    MPS_ARGS_ADD(pcArgs, MPS_KEY_EXTEND_BY, extendBy);
    MPS_ARGS_DONE(pcArgs);
    res = PoolCreate(&(cbs->blockPool), arena, PoolClassMFS(), pcArgs);
  } MPS_ARGS_END(pcArgs);
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
  CBSLeave(cbs);
  return ResOK;
}


/* CBSFinish -- Finish a CBS structure
 *
 * See <design/cbs/#function.cbs.finish>.
 */

void CBSFinish(CBS cbs)
{
  AVERT(CBS, cbs);
  CBSEnter(cbs);

  METER_EMIT(&cbs->splaySearch);

  cbs->sig = SigInvalid;

  SplayTreeFinish(splayTreeOfCBS(cbs));
  PoolDestroy(cbs->blockPool);
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

  PoolFree(cbs->blockPool, (Addr)block, sizeof(CBSBlockStruct));

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

static Res cbsBlockAlloc(CBSBlock *blockReturn, CBS cbs, Addr base, Addr limit)
{
  Res res;
  CBSBlock block;
  Addr p;

  AVER(blockReturn != NULL);
  AVERT(CBS, cbs);

  res = PoolAlloc(&p, cbs->blockPool, sizeof(CBSBlockStruct),
                  /* withReservoirPermit */ FALSE);
  if (res != ResOK)
    goto failPoolAlloc;
  block = (CBSBlock)p;

  SplayNodeInit(splayNodeOfCBSBlock(block));
  block->base = base;
  block->limit = limit;
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

static Res cbsInsertIntoTree(Addr *baseReturn, Addr *limitReturn,
                             CBS cbs, Addr base, Addr limit)
{
  Res res;
  Addr newBase, newLimit;
  SplayNode leftSplay, rightSplay;
  CBSBlock leftCBS, rightCBS;
  Bool leftMerge, rightMerge;
  Size oldSize;

  AVERT(CBS, cbs);
  AVER(base != (Addr)0);
  AVER(base < limit);
  AVER(AddrIsAligned(base, cbs->alignment));
  AVER(AddrIsAligned(limit, cbs->alignment));

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
    if (rightCBS != NULL && limit > rightCBS->base) {
      res = ResFAIL;
      goto fail;
    }
    rightMerge = rightCBS->base == limit;
  }

  newBase = leftMerge ? CBSBlockBase(leftCBS) : base;
  newLimit = rightMerge ? CBSBlockLimit(rightCBS) : limit;

  if (leftMerge) {
    if (rightMerge) {
      Size oldLeftSize = CBSBlockSize(leftCBS);
      Size oldRightSize = CBSBlockSize(rightCBS);

      /* must block larger neighbour and destroy smaller neighbour */
      if (oldLeftSize >= oldRightSize) {
        Addr rightLimit = rightCBS->limit;
        cbsBlockDelete(cbs, rightCBS);
        leftCBS->limit = rightLimit;
        cbsBlockGrew(cbs, leftCBS, oldLeftSize);
      } else { /* left block is smaller */
        Addr leftBase = leftCBS->base;
        cbsBlockDelete(cbs, leftCBS);
        rightCBS->base = leftBase;
        cbsBlockGrew(cbs, rightCBS, oldRightSize);
      }
    } else { /* leftMerge, !rightMerge */
      oldSize = CBSBlockSize(leftCBS);
      leftCBS->limit = limit;
      cbsBlockGrew(cbs, leftCBS, oldSize);
    }
  } else { /* !leftMerge */
    if (rightMerge) {
      oldSize = CBSBlockSize(rightCBS);
      rightCBS->base = base;
      cbsBlockGrew(cbs, rightCBS, oldSize);
    } else { /* !leftMerge, !rightMerge */
      CBSBlock block;
      res = cbsBlockAlloc(&block, cbs, base, limit);
      if (res != ResOK)
        goto fail;
      cbsBlockInsert(cbs, block);
    }
  }

  AVER(newBase <= base);
  AVER(newLimit >= limit);
  *baseReturn = newBase;
  *limitReturn = newLimit;

  return ResOK;

fail:
  AVER(res != ResOK);
  return res;
}


/* CBSInsert -- Insert a range into the CBS
 *
 * See <design/cbs/#functions.cbs.insert>.
 */

Res CBSInsert(Addr *baseReturn, Addr *limitReturn,
              CBS cbs, Addr base, Addr limit)
{
  Addr newBase, newLimit;
  Res res;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(CBS, cbs);
  CBSEnter(cbs);

  AVER(base != (Addr)0);
  AVER(base < limit);
  AVER(AddrIsAligned(base, cbs->alignment));
  AVER(AddrIsAligned(limit, cbs->alignment));

  res = cbsInsertIntoTree(&newBase, &newLimit, cbs, base, limit);
  if (res == ResOK) {
    AVER(newBase <= base);
    AVER(limit <= newLimit);
    *baseReturn = newBase;
    *limitReturn = newLimit;
  }

  CBSLeave(cbs);
  return res;
}


/* cbsDeleteFromTree -- delete blocks from the splay tree */

static Res cbsDeleteFromTree(Addr *baseReturn, Addr *limitReturn,
                             CBS cbs, Addr base, Addr limit)
{
  Res res;
  CBSBlock cbsBlock;
  SplayNode splayNode;
  Addr oldBase, oldLimit;
  Size oldSize;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(CBS, cbs);
  AVER(base != NULL);
  AVER(limit > base);
  AVER(AddrIsAligned(base, cbs->alignment));
  AVER(AddrIsAligned(limit, cbs->alignment));

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
  *baseReturn = cbsBlock->base;
  oldLimit = cbsBlock->limit;
  *limitReturn = cbsBlock->limit;
  oldSize = CBSBlockSize(cbsBlock);

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
    CBSBlock newBlock;
    AVER(base > oldBase);
    AVER(limit < oldLimit);
    res = cbsBlockAlloc(&newBlock, cbs, limit, oldLimit);
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

Res CBSDelete(Addr *baseReturn, Addr *limitReturn,
              CBS cbs, Addr base, Addr limit)
{
  Res res;

  AVERT(CBS, cbs);
  CBSEnter(cbs);

  AVER(base != NULL);
  AVER(limit > base);
  AVER(AddrIsAligned(base, cbs->alignment));
  AVER(AddrIsAligned(limit, cbs->alignment));

  res = cbsDeleteFromTree(baseReturn, limitReturn, cbs, base, limit);

  CBSLeave(cbs);
  return res;
}


static Res CBSBlockDescribe(CBSBlock block, mps_lib_FILE *stream)
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

static Res CBSSplayNodeDescribe(SplayNode splayNode, mps_lib_FILE *stream)
{
  Res res;

  if (splayNode == NULL) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  res = CBSBlockDescribe(cbsBlockOfSplayNode(splayNode), stream);
  return res;
}


/* CBSIterate -- Iterate all blocks in CBS
 *
 * This is not necessarily efficient.
 * See <design/cbs/#function.cbs.iterate>.
 */

/* Internal version without enter/leave checking. */
static void cbsIterateInternal(CBS cbs, CBSIterateMethod iterate,
                               void *closureP, Size closureS)
{
  SplayNode splayNode;
  SplayTree splayTree;
  CBSBlock cbsBlock;

  AVERT(CBS, cbs);
  AVER(FUNCHECK(iterate));

  splayTree = splayTreeOfCBS(cbs);
  /* .splay-iterate.slow: We assume that splay tree iteration does */
  /* searches and meter it. */
  METER_ACC(cbs->splaySearch, cbs->splayTreeSize);
  splayNode = SplayTreeFirst(splayTree, NULL);
  while(splayNode != NULL) {
    cbsBlock = cbsBlockOfSplayNode(splayNode);
    if (!(*iterate)(cbs, CBSBlockBase(cbsBlock), CBSBlockLimit(cbsBlock), closureP, closureS)) {
      break;
    }
    METER_ACC(cbs->splaySearch, cbs->splayTreeSize);
    splayNode = SplayTreeNext(splayTree, splayNode, keyOfCBSBlock(cbsBlock));
  }
  return;
}

void CBSIterate(CBS cbs, CBSIterateMethod iterate,
                void *closureP, Size closureS)
{
  AVERT(CBS, cbs);
  AVER(FUNCHECK(iterate));
  CBSEnter(cbs);

  cbsIterateInternal(cbs, iterate, closureP, closureS);

  CBSLeave(cbs);
  return;
}


/* CBSFindDeleteCheck -- check method for a CBSFindDelete value */

static Bool CBSFindDeleteCheck(CBSFindDelete findDelete)
{
  CHECKL(findDelete == CBSFindDeleteNONE || findDelete == CBSFindDeleteLOW
         || findDelete == CBSFindDeleteHIGH
         || findDelete == CBSFindDeleteENTIRE);
  UNUSED(findDelete); /* <code/mpm.c#check.unused> */

  return TRUE;
}


/* cbsFindDeleteRange -- delete appropriate range of block found */

static void cbsFindDeleteRange(Addr *baseReturn, Addr *limitReturn,
                               Addr *oldBaseReturn, Addr *oldLimitReturn,
                               CBS cbs, Addr base, Addr limit, Size size,
                               CBSFindDelete findDelete)
{
  Bool callDelete = TRUE;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(CBS, cbs);
  AVER(base < limit);
  AVER(size > 0);
  AVER(AddrOffset(base, limit) >= size);
  AVERT(CBSFindDelete, findDelete);

  switch(findDelete) {

  case CBSFindDeleteNONE:
    callDelete = FALSE;
    break;

  case CBSFindDeleteLOW:
    limit = AddrAdd(base, size);
    break;

  case CBSFindDeleteHIGH:
    base = AddrSub(limit, size);
    break;

  case CBSFindDeleteENTIRE:
    /* do nothing */
    break;

  default:
    NOTREACHED;
    break;
  }

  if (callDelete) {
    Res res;
    res = cbsDeleteFromTree(oldBaseReturn, oldLimitReturn, cbs, base, limit);
    /* Can't have run out of memory, because all our callers pass in
       blocks that were just found in the splay tree, and we only
       deleted from one end of the block, so cbsDeleteFromTree did not
       need to allocate a new block. */
    AVER(res == ResOK);
  }

  *baseReturn = base;
  *limitReturn = limit;
}


/* CBSFindFirst -- find the first block of at least the given size */

Bool CBSFindFirst(Addr *baseReturn, Addr *limitReturn,
                  Addr *oldBaseReturn, Addr *oldLimitReturn,
                  CBS cbs, Size size, CBSFindDelete findDelete)
{
  Bool found;
  SplayNode node;

  AVERT(CBS, cbs);
  CBSEnter(cbs);

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVER(size > 0);
  AVER(SizeIsAligned(size, cbs->alignment));
  AVER(cbs->fastFind);
  AVERT(CBSFindDelete, findDelete);

  METER_ACC(cbs->splaySearch, cbs->splayTreeSize);
  found = SplayFindFirst(&node, splayTreeOfCBS(cbs), &cbsTestNode,
                         &cbsTestTree, NULL, size);
  if (found) {
    CBSBlock block;
    Addr base, limit;
    block = cbsBlockOfSplayNode(node);
    AVER(CBSBlockSize(block) >= size);
    base = CBSBlockBase(block);
    limit = CBSBlockLimit(block);
    AVER(AddrOffset(base, limit) >= size);
    cbsFindDeleteRange(baseReturn, limitReturn, oldBaseReturn, oldLimitReturn,
                       cbs, base, limit, size, findDelete);
  }

  CBSLeave(cbs);
  return found;
}


/* CBSFindLast -- find the last block of at least the given size */

Bool CBSFindLast(Addr *baseReturn, Addr *limitReturn,
                 Addr *oldBaseReturn, Addr *oldLimitReturn,
                 CBS cbs, Size size, CBSFindDelete findDelete)
{
  Bool found;
  SplayNode node;

  AVERT(CBS, cbs);
  CBSEnter(cbs);

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVER(size > 0);
  AVER(SizeIsAligned(size, cbs->alignment));
  AVER(cbs->fastFind);
  AVERT(CBSFindDelete, findDelete);

  METER_ACC(cbs->splaySearch, cbs->splayTreeSize);
  found = SplayFindLast(&node, splayTreeOfCBS(cbs), &cbsTestNode,
                        &cbsTestTree, NULL, size);
  if (found) {
    CBSBlock block;
    Addr base, limit;
    block = cbsBlockOfSplayNode(node);
    AVER(CBSBlockSize(block) >= size);
    base = CBSBlockBase(block);
    limit = CBSBlockLimit(block);
    AVER(AddrOffset(base, limit) >= size);
    cbsFindDeleteRange(baseReturn, limitReturn, oldBaseReturn, oldLimitReturn,
                       cbs, base, limit, size, findDelete);
  }

  CBSLeave(cbs);
  return found;
}


/* CBSFindLargest -- find the largest block in the CBS */

Bool CBSFindLargest(Addr *baseReturn, Addr *limitReturn,
                    Addr *oldBaseReturn, Addr *oldLimitReturn,
                    CBS cbs, CBSFindDelete findDelete)
{
  Bool found = FALSE;
  SplayNode root;
  Bool notEmpty;

  AVERT(CBS, cbs);
  CBSEnter(cbs);

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVER(cbs->fastFind);
  AVERT(CBSFindDelete, findDelete);

  notEmpty = SplayRoot(&root, splayTreeOfCBS(cbs));
  if (notEmpty) {
    CBSBlock block;
    SplayNode node = NULL;    /* suppress "may be used uninitialized" */
    Addr base, limit;
    Size size;

    size = cbsBlockOfSplayNode(root)->maxSize;
    METER_ACC(cbs->splaySearch, cbs->splayTreeSize);
    found = SplayFindFirst(&node, splayTreeOfCBS(cbs), &cbsTestNode,
                           &cbsTestTree, NULL, size);
    AVER(found); /* maxSize is exact, so we will find it. */
    block = cbsBlockOfSplayNode(node);
    AVER(CBSBlockSize(block) >= size);
    base = CBSBlockBase(block);
    limit = CBSBlockLimit(block);
    cbsFindDeleteRange(baseReturn, limitReturn, oldBaseReturn, oldLimitReturn,
                       cbs, base, limit, size, findDelete);
  }

  CBSLeave(cbs);
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
               "  blockPool: $P\n", (WriteFP)cbs->blockPool,
               "  fastFind: $B\n", (WriteFB)cbs->fastFind,
               "  inCBS: $B\n", (WriteFB)cbs->inCBS,
               "  splayTreeSize: $U\n", (WriteFU)cbs->splayTreeSize,
               NULL);
  if (res != ResOK) return res;

  res = SplayTreeDescribe(splayTreeOfCBS(cbs), stream, &CBSSplayNodeDescribe);
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
