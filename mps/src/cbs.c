/* impl.c.cbs: COALESCING BLOCK STRUCTURE IMPLEMENTATION
 *
 * $HopeName: MMsrc!cbs.c(trunk.10) $
 * Copyright (C) 1998 Harlequin Group plc, all rights reserved.
 *
 * .readership: Any MPS developer.
 *
 * .intro: This is a portable implementation of coalescing block
 * structures.
 *
 * .purpose: CBSs are used to manage potentially unbounded 
 * collections of memory blocks.
 *
 * .sources: design.mps.cbs.
 */


#include "mpm.h"


SRCID(cbs, "$HopeName: MMsrc!cbs.c(trunk.10) $");

typedef void **CBSEmergencyBlock; /* next, limit */
typedef void **CBSEmergencyGrain; /* next */

/* See design.mps.cbs.align */
#define CBSMinimumAlignment ((Align)sizeof(void *))

#define CBSOfSplayTree(tree) PARENT(CBSStruct, splayTree, (tree))
#define CBSBlockOfSplayNode(node) PARENT(CBSBlockStruct, splayNode, (node))
#define SplayTreeOfCBS(tree) (&((cbs)->splayTree))
#define SplayNodeOfCBSBlock(block) (&((block)->splayNode))
#define KeyOfCBSBlock(block) ((void *)&((block)->base))


/* CBSEmergencyBlock* -- Getters and setters for emergency blocks
 *
 * See design.mps.cbs.impl.low-mem.inline.block. */

#define CBSEmergencyBlockBase(block) ((Addr)(block))
#define CBSEmergencyBlockLimit(block) ((Addr)((block)[1]))
#define CBSEmergencyBlockSize(block) \
  (AddrOffset(CBSEmergencyBlockBase((block)), \
              CBSEmergencyBlockLimit((block))))
#define CBSEmergencyBlockNext(block) ((CBSEmergencyBlock)((block)[0]))

#define CBSEmergencyBlockSetNext(block, next) \
  BEGIN (block)[0] = (void *)(next); END
#define CBSEmergencyBlockSetLimit(block, limit) \
  BEGIN (block)[1] = (void *)(limit); END


/* CBSEmergencyGrain* -- Getters and setters for emergency grains
 *
 * See design.mps.cbs.impl.low-mem.inline.grain. */

#define CBSEmergencyGrainBase(grain) ((Addr)(grain))
#define CBSEmergencyGrainLimit(cbs, grain) \
  AddrAdd(CBSEmergencyGrainBase((grain)), CBSEmergencyGrainSize((cbs)))
#define CBSEmergencyGrainSize(cbs) ((cbs)->alignment)
#define CBSEmergencyGrainNext(grain) ((CBSEmergencyGrain)((grain)[0]))

#define CBSEmergencyGrainSetNext(grain, next) \
  BEGIN (grain)[0] = (void *)(next); END


static CBSEmergencyBlock CBSEmergencyBlockInit(Addr base, Addr limit)
{
  CBSEmergencyBlock block = (CBSEmergencyBlock)base;
  CBSEmergencyBlockSetNext(block, NULL);
  CBSEmergencyBlockSetLimit(block, limit);
  return block;
}

static CBSEmergencyGrain CBSEmergencyGrainInit(CBS cbs, Addr base, Addr limit)
{
  CBSEmergencyGrain grain = (CBSEmergencyGrain)base;
  AVER(AddrOffset(base, limit) == CBSEmergencyGrainSize(cbs));
  CBSEmergencyGrainSetNext(grain, NULL);
  return grain;
}


/* CBSEnter, CBSLeave -- Avoid re-entrance 
 *
 * .enter-leave: The callbacks are restricted in what they may call.
 * These functions enforce this.
 *
 * .enter-leave.simple: Simple queries may be called from callbacks.
 */

static void CBSEnter(CBS cbs) {
  /* Don't need to check as always called from interface function. */
  AVER(!cbs->inCBS);
  cbs->inCBS = TRUE;
  return;
}

static void CBSLeave(CBS cbs) {
  /* Don't need to check as always called from interface function. */
  AVER(cbs->inCBS);
  cbs->inCBS = FALSE;
  return;
}

Bool CBSCheck(CBS cbs) {
  /* See .enter-leave.simple. */
  CHECKS(CBS, cbs);
  CHECKL(cbs != NULL);
  CHECKL(SplayTreeCheck(SplayTreeOfCBS(cbs)));
  /* can't check emergencyBlockList or emergencyGrainList */
  CHECKD(Pool, cbs->blockPool);
  CHECKL(BoolCheck(cbs->mayUseInline));
  CHECKL(BoolCheck(cbs->fastFind));
  CHECKL(BoolCheck(cbs->inCBS));
  CHECKL(cbs->new == NULL || FUNCHECK(cbs->new));
  CHECKL(cbs->delete == NULL || FUNCHECK(cbs->delete));
  CHECKL(cbs->grow == NULL || FUNCHECK(cbs->grow));
  CHECKL(cbs->shrink == NULL || FUNCHECK(cbs->shrink));
  CHECKL(cbs->mayUseInline || cbs->emergencyBlockList == NULL);
  CHECKL(cbs->mayUseInline || cbs->emergencyGrainList == NULL);
  /* See design.mps.cbs.align */
  CHECKL(!cbs->mayUseInline || 
         AlignIsAligned(cbs->alignment, CBSMinimumAlignment));

  return TRUE;
}

/* CBSBlockCheck -- See design.mps.cbs.function.cbs.block.check */
 
Bool CBSBlockCheck(CBSBlock block) {
  /* See .enter-leave.simple. */
  UNUSED(block); /* Required because there is no signature */
  CHECKL(block != NULL);
  CHECKL(SplayNodeCheck(SplayNodeOfCBSBlock(block)));
 
  /* If the block is in the middle of being deleted, */
  /* the pointers will be equal. */
  CHECKL(CBSBlockBase(block) <= CBSBlockLimit(block));
  /* Can't check maxSize because it may be invalid at the time */
  return TRUE;
}
 
 
/* CBSBlockBase -- See design.mps.cbs.function.cbs.block.base */
 
Addr (CBSBlockBase)(CBSBlock block) {
  /* See .enter-leave.simple. */
  return CBSBlockBase(block);
}
 
 
/* CBSBlockLimit -- See design.mps.cbs.function.cbs.block.limit */
 
Addr (CBSBlockLimit)(CBSBlock block) {
  /* See .enter-leave.simple. */
  return CBSBlockLimit(block);
}
 
 
/* CBSBlockSize -- see design.mps.cbs.function.cbs.block.size */
 
Size (CBSBlockSize)(CBSBlock block) {
  /* See .enter-leave.simple. */
  return CBSBlockSize(block);
}


/* CBSSplayCompare -- Compare base to [base,limit) 
 *
 * See design.mps.splay.type.splay.compare.method
 */

static Compare CBSSplayCompare(void *key, SplayNode node) {
  Addr base1, base2, limit2;
  CBSBlock cbsBlock;

  /* NULL key compares less than everything. */
  if(key == NULL)
    return CompareLESS;

  AVER(node != NULL);

  base1 = *(Addr *)key;
  cbsBlock = CBSBlockOfSplayNode(node);
  base2 = cbsBlock->base;
  limit2 = cbsBlock->limit;

  if(base1 < base2) 
    return CompareLESS;
  else if(base1 >= limit2)
    return CompareGREATER;
  else
    return CompareEQUAL;
}

static Bool CBSTestNode(SplayTree tree, SplayNode node, 
                        void *closureP, unsigned long closureS)
{
  Size size;
  CBSBlock block;

  AVERT(SplayTree, tree);
  AVERT(SplayNode, node);
  AVER(closureP == NULL);
  AVER(closureS > 0);
  AVER(CBSOfSplayTree(tree)->fastFind);

  size = (Size)closureS;
  block = CBSBlockOfSplayNode(node);

  return CBSBlockSize(block) >= size;
}

static Bool CBSTestTree(SplayTree tree, SplayNode node,
                        void *closureP, unsigned long closureS) 
{
  Size size;
  CBSBlock block;

  AVERT(SplayTree, tree);
  AVERT(SplayNode, node);
  AVER(closureP == NULL);
  AVER(closureS > 0);
  AVER(CBSOfSplayTree(tree)->fastFind);

  size = (Size)closureS;
  block = CBSBlockOfSplayNode(node);

  return block->maxSize >= size;
}

static void CBSUpdateNode(SplayTree tree, SplayNode node,
                          SplayNode leftChild, SplayNode rightChild)
{
  Size maxSize;
  CBSBlock block;

  AVERT(SplayTree, tree);
  AVERT(SplayNode, node);
  if(leftChild != NULL)
    AVERT(SplayNode, leftChild);
  if(rightChild != NULL)
    AVERT(SplayNode, rightChild);
  AVER(CBSOfSplayTree(tree)->fastFind);

  block = CBSBlockOfSplayNode(node);
  maxSize = CBSBlockSize(block);

  if(leftChild != NULL) {
    Size size = CBSBlockOfSplayNode(leftChild)->maxSize;
    if(size > maxSize)
      maxSize = size;
  }

  if(rightChild != NULL) {
    Size size = CBSBlockOfSplayNode(rightChild)->maxSize;
    if(size > maxSize)
      maxSize = size;
  }

  block->maxSize = maxSize;
}

/* CBSInit -- Initialise a CBS structure
 *
 * See design.mps.cbs.function.cbs.init.
 */

Res CBSInit(Arena arena, CBS cbs, 
            CBSChangeSizeMethod new, 
            CBSChangeSizeMethod delete,
            CBSChangeSizeMethod grow,
            CBSChangeSizeMethod shrink,
            Size minSize,
            Align alignment,
            Bool mayUseInline,
            Bool fastFind) {
  Res res;

  AVERT(Arena, arena);
  AVER(new == NULL || FUNCHECK(new));
  AVER(delete == NULL || FUNCHECK(delete));
  AVER(BoolCheck(mayUseInline));
  if(mayUseInline) {
    /* See design.mps.cbs.align */
    if(!AlignIsAligned(alignment, CBSMinimumAlignment))
      return ResPARAM;
  }

  SplayTreeInit(SplayTreeOfCBS(cbs), &CBSSplayCompare, 
                fastFind ? &CBSUpdateNode : NULL);
  res = PoolCreate(&(cbs->blockPool), arena, PoolClassMFS(),
                   sizeof(CBSBlockStruct) * 64, sizeof(CBSBlockStruct));
  if(res != ResOK)
    return res;

  cbs->new = new;
  cbs->delete = delete;
  cbs->grow = grow;
  cbs->shrink = shrink;
  cbs->minSize = minSize;
  cbs->mayUseInline = mayUseInline;
  cbs->fastFind = fastFind;
  cbs->alignment = alignment;
  cbs->inCBS = TRUE;
  cbs->emergencyBlockList = NULL;
  cbs->emergencyGrainList = NULL;

  cbs->sig = CBSSig;

  AVERT(CBS, cbs);

  CBSLeave(cbs);
  return ResOK;
}


/* CBSFinish -- Finish a CBS structure
 *
 * See design.mps.cbs.function.cbs.finish.
 */

void CBSFinish(CBS cbs) {
  AVERT(CBS, cbs);
  CBSEnter(cbs);

  cbs->sig = SigInvalid;

  SplayTreeFinish(SplayTreeOfCBS(cbs));
  PoolDestroy(cbs->blockPool);
  cbs->emergencyBlockList = NULL;
  cbs->emergencyGrainList = NULL;
}


/* Node change operators
 *
 * These four functions are called whenever blocks are created,
 * destroyed, grow, or shrink.  They report to the client, and
 * perform the necessary memory management.  They are responsible
 * for the client interaction logic.
 */

static void CBSBlockDelete(CBS cbs, CBSBlock block) {
  Res res;
  Size oldSize;

  AVERT(CBS, cbs);
  AVERT(CBSBlock, block);

  oldSize = CBSBlockSize(block);

  res = SplayTreeDelete(SplayTreeOfCBS(cbs), SplayNodeOfCBSBlock(block), 
                        KeyOfCBSBlock(block));
  AVER(res == ResOK); /* Must be possible to delete node */

  /* make invalid */
  block->limit = block->base;

  if(cbs->delete != NULL && oldSize >= cbs->minSize)
    (*(cbs->delete))(cbs, block, oldSize, (Size)0);

  PoolFree(cbs->blockPool, (Addr)block, sizeof(CBSBlockStruct));

  return;
}

static void CBSBlockShrink(CBS cbs, CBSBlock block, Size oldSize) {
  Size newSize;

  AVERT(CBS, cbs);
  AVERT(CBSBlock, block);

  newSize = CBSBlockSize(block);
  AVER(oldSize > newSize);

  if(cbs->fastFind) {
    SplayNodeRefresh(SplayTreeOfCBS(cbs), SplayNodeOfCBSBlock(block),
                     KeyOfCBSBlock(block));
    AVER(CBSBlockSize(block) <= block->maxSize);
  }

  if(cbs->delete != NULL && oldSize >= cbs->minSize && 
     newSize < cbs->minSize)
    (*(cbs->delete))(cbs, block, oldSize, newSize);
  else if(cbs->shrink != NULL && newSize >= cbs->minSize)
    (*(cbs->shrink))(cbs, block, oldSize, newSize);
}

static void CBSBlockGrow(CBS cbs, CBSBlock block, Size oldSize) {
  Size newSize;

  AVERT(CBS, cbs);
  AVERT(CBSBlock, block);

  newSize = CBSBlockSize(block);
  AVER(oldSize < newSize);

  if(cbs->fastFind) {
    SplayNodeRefresh(SplayTreeOfCBS(cbs), SplayNodeOfCBSBlock(block),
                     KeyOfCBSBlock(block));
    AVER(CBSBlockSize(block) <= block->maxSize);
  }

  if(cbs->new != NULL && oldSize < cbs->minSize &&
     newSize >= cbs->minSize)
    (*(cbs->new))(cbs, block, oldSize, newSize);
  else if(cbs->grow != NULL && oldSize >= cbs->minSize)
    (*(cbs->grow))(cbs, block, oldSize, newSize);
}

static Res CBSBlockNew(CBS cbs, Addr base, Addr limit) {
  CBSBlock block;
  Res res;
  Addr p;
  Size newSize;

  AVERT(CBS, cbs);

  res = PoolAlloc(&p, cbs->blockPool, sizeof(CBSBlockStruct),
                  /* withReservoirPermit */ FALSE);
  if(res != ResOK)
    goto failPoolAlloc;
  block = (CBSBlock)p;

  SplayNodeInit(SplayNodeOfCBSBlock(block));
  block->base = base;
  block->limit = limit;
  newSize = CBSBlockSize(block);
  block->maxSize = newSize;

  AVERT(CBSBlock, block);

  res = SplayTreeInsert(SplayTreeOfCBS(cbs), SplayNodeOfCBSBlock(block),
                        KeyOfCBSBlock(block));
  AVER(res == ResOK);

  if(cbs->new != NULL && newSize >= cbs->minSize)
    (*(cbs->new))(cbs, block, (Size)0, newSize);

  return ResOK;

failPoolAlloc:
  AVER(res != ResOK);
  return res;
}


/* CBSInsertIntoTree -- Insert a range into the CBS proper
 *
 * ... as opposed to the emergency lists.
 */

static Res CBSInsertIntoTree(Addr *baseReturn, Addr *limitReturn,
                             CBS cbs, Addr base, Addr limit) {
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

  res = SplayTreeNeighbours(&leftSplay, &rightSplay,
                            SplayTreeOfCBS(cbs), (void *)&base);
  if(res != ResOK)
    goto fail;

  if(leftSplay == NULL) {
    leftCBS = NULL;
    leftMerge = FALSE;
  } else {
    leftCBS = CBSBlockOfSplayNode(leftSplay);
    AVER(leftCBS->limit <= base); /* by CBSSplayCompare */
    leftMerge = leftCBS->limit == base;
  }

  if(rightSplay == NULL) {
    rightCBS = NULL;
    rightMerge = FALSE;
  } else {
    rightCBS = CBSBlockOfSplayNode(rightSplay);
    if(rightCBS != NULL && limit > rightCBS->base) {
      res = ResFAIL;
      goto fail;
    }
    rightMerge = rightCBS->base == limit;
  }

  newBase = leftMerge ? CBSBlockBase(leftCBS) : base;
  newLimit = rightMerge ? CBSBlockLimit(rightCBS) : limit;

  if(leftMerge) {
    if(rightMerge) {
      Size oldLeftSize = CBSBlockSize(leftCBS);
      Size oldRightSize = CBSBlockSize(rightCBS);

      /* must block larger neighbour and destroy smaller neighbour; */
      /* see design.mps.cbs.function.cbs.insert.callback */
      if(oldLeftSize >= oldRightSize) {
        Addr rightLimit = rightCBS->limit;
        CBSBlockDelete(cbs, rightCBS);
        leftCBS->limit = rightLimit;
        CBSBlockGrow(cbs, leftCBS, oldLeftSize);
      } else { /* left block is smaller */
        Addr leftBase = leftCBS->base;
        CBSBlockDelete(cbs, leftCBS);
        rightCBS->base = leftBase;
        CBSBlockGrow(cbs, rightCBS, oldRightSize);
      }
    } else { /* leftMerge, !rightMerge */
      oldSize = CBSBlockSize(leftCBS);
      leftCBS->limit = limit;
      CBSBlockGrow(cbs, leftCBS, oldSize);
    }
  } else { /* !leftMerge */
    if(rightMerge) {
      oldSize = CBSBlockSize(rightCBS);
      rightCBS->base = base;
      CBSBlockGrow(cbs, rightCBS, oldSize);
    } else { /* !leftMerge, !rightMerge */
      res = CBSBlockNew(cbs, base, limit);
      if(res != ResOK) 
        goto fail;
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


/* CBSCoalesceWithEmergencyLists -- coalesce received range with EBL and EGL
 *
 * Attempts to extend the range about to be freed by adding ranges from
 * the emergency lists.  May remove blocks from the emergency list.
 */

static Res CBSCoalesceWithEmergencyLists(Addr *baseIO, Addr *limitIO, CBS cbs)
{
  Addr base, limit;
  Count nCoalescences = 0;

  AVER(baseIO != NULL);
  AVER(limitIO != NULL);
  AVERT(CBS, cbs);
  AVER(cbs->mayUseInline);

  base = *baseIO;
  limit = *limitIO;
  AVER(base < limit);

  if(cbs->emergencyBlockList != NULL) {
    CBSEmergencyBlock prev, block, next;
    Addr blockBase, blockLimit;

    for(block = cbs->emergencyBlockList, prev = NULL;
        block != NULL && CBSEmergencyBlockBase(block) <= limit;
        block = CBSEmergencyBlockNext(block)) {

      blockBase = CBSEmergencyBlockBase(block);
      blockLimit = CBSEmergencyBlockLimit(block);
      AVER(blockBase < blockLimit);

      if(prev != NULL)
        AVER(CBSEmergencyBlockLimit(prev) < blockBase);

      if(blockLimit == base) {
        base = blockBase;
        next = CBSEmergencyBlockNext(block);
        if(prev == NULL)
          cbs->emergencyBlockList = next;
        else
          CBSEmergencyBlockSetNext(prev, next);
        ++nCoalescences;
      } else if(blockBase == limit) {
        limit = blockLimit;
        next = CBSEmergencyBlockNext(block);
        if(prev == NULL)
          cbs->emergencyBlockList = next;
        else
          CBSEmergencyBlockSetNext(prev, next);
        ++nCoalescences;
        /* For loop will stop at next test */
      } else if(blockLimit > base) {
        return ResFAIL; /* range intersects block */
      } else {
        prev = block; /* Only move prev if we didn't delete */
      }
    }
    /* block's next is still valid, even if it's been coalesced */
  }
  
  if(cbs->emergencyGrainList != NULL) {
    CBSEmergencyGrain prev, grain, next;
    Addr grainBase, grainLimit;

    for(grain = cbs->emergencyGrainList, prev = NULL;
        grain != NULL && CBSEmergencyGrainBase(grain) <= limit &&
          nCoalescences < 2;
        grain = CBSEmergencyGrainNext(grain)) {
      grainBase = CBSEmergencyGrainBase(grain);
      grainLimit = CBSEmergencyGrainLimit(cbs, grain);
      AVER(grainBase < grainLimit);

      if(prev != NULL)
        AVER(CBSEmergencyGrainLimit(cbs, prev) < grainBase);

      if(grainLimit == base) {
        base = grainBase;
        next = CBSEmergencyGrainNext(grain);
        if(prev == NULL)
          cbs->emergencyGrainList = next;
        else
          CBSEmergencyGrainSetNext(prev, next);
        ++nCoalescences;
      } else if(grainBase == limit) {
        limit = grainLimit;
        next = CBSEmergencyGrainNext(grain);
        if(prev == NULL)
          cbs->emergencyGrainList = next;
        else
          CBSEmergencyGrainSetNext(prev, next);
        ++nCoalescences;
        break;
      } else if(grainLimit > base) {
        return ResFAIL; /* range intersects grain */
      } else {
        prev = grain;
      }
    }
    /* grain's next is still valid, even if it's been coalesced */
  }

  /* Because the lists are known to have isolated ranges, there can */
  /* be no more than 2 coalescences. */
  AVER(nCoalescences <= 2); 

  *baseIO = base;
  *limitIO = limit;
  return ResOK;
}


/* CBSAddToEmergencyLists -- Adds range to emergency lists
 *
 * The range must be unadjacent to any items on the emergency lists.
 */

static Res CBSAddToEmergencyLists(CBS cbs, Addr base, Addr limit) 
{
  Res res = ResOK;
  Size size;

  AVERT(CBS, cbs);
  AVER(base < limit);
  AVER(cbs->mayUseInline);

  size = AddrOffset(base, limit);
  /* Use the block list if possible.  See design.mps.cbs.align. */
  if(size > CBSMinimumAlignment) {
    CBSEmergencyBlock prev, block, new;
    new = CBSEmergencyBlockInit(base, limit);
    for(prev = NULL, block = cbs->emergencyBlockList;
        block != NULL && CBSEmergencyBlockBase(block) < base;
        prev = block, block = CBSEmergencyBlockNext(block)) {
      if(prev != NULL)
        AVER(CBSEmergencyBlockLimit(prev) < CBSEmergencyBlockBase(block));
      AVER(CBSEmergencyBlockBase(block) < CBSEmergencyBlockLimit(block));
    }

    if(prev != NULL && block != NULL)
      AVER(CBSEmergencyBlockLimit(prev) < CBSEmergencyBlockBase(block));
      
    /* check ordering: prev ... new ... block */
    if(prev != NULL && CBSEmergencyBlockLimit(prev) >= base)
      return ResFAIL; /* range intersects with existing block */

    if(block != NULL && limit >= CBSEmergencyBlockBase(block))
      return ResFAIL; /* range intersects with existing block */

    if(prev == NULL) 
      cbs->emergencyBlockList = new;
    else
      CBSEmergencyBlockSetNext(prev, new);
    CBSEmergencyBlockSetNext(new, block); /* may be NULL */
  } else if(size == CBSEmergencyGrainSize(cbs)) {
    CBSEmergencyGrain prev, grain, new;
    new = CBSEmergencyGrainInit(cbs, base, limit);
    for(prev = NULL, grain = cbs->emergencyGrainList;
        grain != NULL && CBSEmergencyGrainBase(grain) < base;
        prev = grain, grain = CBSEmergencyGrainNext(grain)) {
      if(prev != NULL)
        AVER(CBSEmergencyGrainLimit(cbs, prev) < 
             CBSEmergencyGrainBase(grain));
    }

    if(prev != NULL && grain != NULL)
      AVER(CBSEmergencyGrainLimit(cbs, prev) < CBSEmergencyGrainBase(grain));
      
    /* check ordering: prev ... new ... grain */
    if(prev != NULL && CBSEmergencyGrainLimit(cbs, prev) >= base)
      return ResFAIL; /* range intersects with existing grain */

    if(grain != NULL && limit >= CBSEmergencyGrainBase(grain))
      return ResFAIL; /* range intersects with existing grain */

    if(prev == NULL) 
      cbs->emergencyGrainList = new;
    else
      CBSEmergencyGrainSetNext(prev, new);
    CBSEmergencyGrainSetNext(new, grain); /* may be NULL */
  } else {
    NOTREACHED;
    res = ResFAIL; /* in case AVERs are compiled out */
  }

  return res;
}


/* CBSFlushEmergencyLists -- Attempt to move ranges to CBS proper */

static void CBSFlushEmergencyLists(CBS cbs) 
{
  Res res = ResOK;
  Addr base, limit;

  AVERT(CBS, cbs);
  AVER(cbs->mayUseInline);

  if(cbs->emergencyBlockList != NULL) {
    CBSEmergencyBlock block;
    for(block = cbs->emergencyBlockList;
        block != NULL;
        block = CBSEmergencyBlockNext(block)) {
      AVER(CBSEmergencyBlockBase(block) < CBSEmergencyBlockLimit(block));
      res = CBSInsertIntoTree(&base, &limit,
                              cbs, CBSEmergencyBlockBase(block),
                              CBSEmergencyBlockLimit(block));
      if(res == ResOK) {
        AVER(cbs->emergencyBlockList == block);
        /* Emergency block is isolated in CBS */
        AVER(base == CBSEmergencyBlockBase(block));
        AVER(limit == CBSEmergencyBlockLimit(block));

        cbs->emergencyBlockList = CBSEmergencyBlockNext(block);
      } else {
        AVER(ResIsAllocFailure(res));
        goto done;
      }
    }
  }

  if(cbs->emergencyGrainList != NULL) {
    CBSEmergencyGrain grain;
    for(grain = cbs->emergencyGrainList;
        grain != NULL;
        grain = CBSEmergencyGrainNext(grain)) {
      res = CBSInsertIntoTree(&base, &limit,
                              cbs, CBSEmergencyGrainBase(grain),
                              CBSEmergencyGrainLimit(cbs, grain));
      if(res == ResOK) {
        AVER(cbs->emergencyGrainList == grain);
        /* Emergency grain is isolated in CBS */
        AVER(base == CBSEmergencyGrainBase(grain));
        AVER(limit == CBSEmergencyGrainLimit(cbs, grain));

        cbs->emergencyGrainList = CBSEmergencyGrainNext(grain);
      } else {
        AVER(ResIsAllocFailure(res));
        goto done;
      }
    }
  }

  done:
  return;
}

/* CBSInsert -- Insert a range into the CBS
 *
 * See design.mps.cbs.functions.cbs.insert.
 */

Res CBSInsertReturningRange(Addr *baseReturn, Addr *limitReturn,
                            CBS cbs, Addr base, Addr limit) {
  Addr newBase, newLimit;
  Res res;

  AVERT(CBS, cbs);
  CBSEnter(cbs);

  AVER(base != (Addr)0);
  AVER(base < limit);
  AVER(AddrIsAligned(base, cbs->alignment));
  AVER(AddrIsAligned(limit, cbs->alignment));

  if(cbs->mayUseInline) {
    newBase = base;
    newLimit = limit;

    res = CBSCoalesceWithEmergencyLists(&newBase, &newLimit, cbs);
    if(res != ResOK) {
      AVER(res == ResFAIL);
      goto done;
    }

    res = CBSInsertIntoTree(&newBase, &newLimit, cbs, newBase, newLimit);
    /* newBase and newLimit only changed if res == ResOK */

    if(ResIsAllocFailure(res)) {
      res = CBSAddToEmergencyLists(cbs, newBase, newLimit);
      if(res != ResOK) {
        AVER(res == ResFAIL);
        goto done;
      }
    } else {
      /* Attempt to clear emergency lists */
      CBSFlushEmergencyLists(cbs);
    }
  } else {
    res = CBSInsertIntoTree(&newBase, &newLimit, cbs, base, limit);
  }

  done:
  if(res == ResOK) {
    AVER(newBase <= base);
    AVER(limit <= newLimit);
    *baseReturn = newBase;
    *limitReturn = newLimit;
  }

  CBSLeave(cbs);
  return res;
}

Res CBSInsert(CBS cbs, Addr base, Addr limit)
{
  Res res;
  Addr newBase, newLimit;

  /* all parameters checked by CBSInsertReturningRange */
  /* CBSEnter/Leave done by CBSInsertReturningRange */

  res = CBSInsertReturningRange(&newBase, &newLimit,
                                cbs, base, limit);

  return res;
}

static Res CBSDeleteFromTree(CBS cbs, Addr base, Addr limit) {
  Res res;
  CBSBlock cbsBlock;
  SplayNode splayNode;
  Size oldSize;

  /* parameters already checked */

  res = SplayTreeSearch(&splayNode, SplayTreeOfCBS(cbs), (void *)&base);
  if(res != ResOK)
    goto failSplayTreeSearch;
  cbsBlock = CBSBlockOfSplayNode(splayNode);

  if(limit > cbsBlock->limit) {
    res = ResFAIL;
    goto failLimitCheck;
  }

  if(base == cbsBlock->base) {
    if(limit == cbsBlock->limit) { /* entire block */
      CBSBlockDelete(cbs, cbsBlock);
    } else { /* remaining fragment at right */
      AVER(limit < cbsBlock->limit);
      oldSize = CBSBlockSize(cbsBlock);
      cbsBlock->base = limit;
      CBSBlockShrink(cbs, cbsBlock, oldSize);
    }
  } else {
    AVER(base > cbsBlock->base);
    if(limit == cbsBlock->limit) { /* remaining fragment at left */
      oldSize = CBSBlockSize(cbsBlock);
      cbsBlock->limit = base;
      CBSBlockShrink(cbs, cbsBlock, oldSize);
    } else { /* two remaining fragments */
      Size leftNewSize = AddrOffset(cbsBlock->base, base);
      Size rightNewSize = AddrOffset(limit, cbsBlock->limit);
      /* must shrink larger fragment and create smaller; */
      /* see design.mps.cbs.function.cbs.delete.callback */
      if(leftNewSize >= rightNewSize) {
        Addr oldLimit = cbsBlock->limit;
        AVER(limit < cbsBlock->limit);
        oldSize = CBSBlockSize(cbsBlock);
        cbsBlock->limit = base;
        CBSBlockShrink(cbs, cbsBlock, oldSize);
        res = CBSBlockNew(cbs, limit, oldLimit);
        if(res != ResOK) {
          AVER(ResIsAllocFailure(res));
          if(cbs->mayUseInline) {
            res = CBSAddToEmergencyLists(cbs, limit, oldLimit);
            AVER(res == ResOK);
          } else {
            goto failNew;
          }
        }
      } else { /* right fragment is larger */
        Addr oldBase = cbsBlock->base;
        AVER(base > cbsBlock->base);
        oldSize = CBSBlockSize(cbsBlock);
        cbsBlock->base = limit;
        CBSBlockShrink(cbs, cbsBlock, oldSize);
        res = CBSBlockNew(cbs, oldBase, base);
        if(res != ResOK) {
          AVER(ResIsAllocFailure(res));
          if(cbs->mayUseInline) {
            res = CBSAddToEmergencyLists(cbs, oldBase, base);
            AVER(res == ResOK);
          } else {
            goto failNew;
          }
        }
      }
    }
  }

  return ResOK;

failNew:
failLimitCheck:
failSplayTreeSearch:
  AVER(res != ResOK);
  return res;
}

static Res CBSDeleteFromEmergencyBlockList(CBS cbs, Addr base, Addr limit)
{
  Res res;
  Addr blockBase, blockLimit;
  CBSEmergencyBlock prev, block;

  /* parameters already checked in caller */
  AVER(cbs->mayUseInline);

  for(prev = NULL, block = cbs->emergencyBlockList;
      block != NULL && CBSEmergencyBlockLimit(block) < limit;
      prev = block, block = CBSEmergencyBlockNext(block)) {
    AVER(CBSEmergencyBlockBase(block) < CBSEmergencyBlockLimit(block));
    if(CBSEmergencyBlockBase(block) >= base)
      return ResFAIL;
    if(prev != NULL)
      AVER(CBSEmergencyBlockLimit(prev) < CBSEmergencyBlockBase(block));
  }

  if(block != NULL) {
    blockBase = CBSEmergencyBlockBase(block);
    blockLimit = CBSEmergencyBlockLimit(block);
    AVER(blockBase < blockLimit);
    AVER(blockLimit >= limit);

    if(blockBase <= base && limit <= blockLimit) {
      /* remove from list */
      if(prev == NULL) 
        cbs->emergencyBlockList = CBSEmergencyBlockNext(block);
      else
        CBSEmergencyBlockSetNext(prev, CBSEmergencyBlockNext(block));
      if(blockBase < base) {
        res = CBSAddToEmergencyLists(cbs, blockBase, base);
        if(res != ResOK)
          return res;
      }
      if(limit < blockLimit) {
        res = CBSAddToEmergencyLists(cbs, limit, blockLimit);
        if(res != ResOK)
          return res;
      }
      return ResOK;
    } else {
      return ResFAIL; /* partly in list */
    }
  } 
  return ResFAIL; /* not in list at all */
}


static Res CBSDeleteFromEmergencyGrainList(CBS cbs, Addr base, Addr limit)
{
  Addr grainBase, grainLimit;
  CBSEmergencyGrain prev, grain;

  /* parameters already checked in caller */
  AVER(cbs->mayUseInline);
  if(AddrOffset(base, limit) != CBSEmergencyGrainSize(cbs))
    return ResFAIL;

  for(prev = NULL, grain = cbs->emergencyGrainList;
      grain != NULL && CBSEmergencyGrainLimit(cbs, grain) < limit;
      prev = grain, grain = CBSEmergencyGrainNext(grain)) {
    if(prev != NULL)
      AVER(CBSEmergencyGrainLimit(cbs, prev) < CBSEmergencyGrainBase(grain));
  }   

  if(grain != NULL) {
    grainBase = CBSEmergencyGrainBase(grain);
    grainLimit = CBSEmergencyGrainLimit(cbs, grain);
    AVER(grainLimit >= limit);

    if(grainBase <= base && limit <= grainLimit) {
      AVER(grainBase == base);
      AVER(grainLimit == limit);
      /* remove from list */
      if(prev == NULL)
        cbs->emergencyGrainList = CBSEmergencyGrainNext(grain);
      else
        CBSEmergencyGrainSetNext(prev, CBSEmergencyGrainNext(grain));
      return ResOK;
    } else {
      return ResFAIL; /* range is partly in list */
    }
  } 
  return ResFAIL; /* range is not in list at all */
}


/* CBSDelete -- Remove a range from a CBS
 *
 * See design.mps.cbs.function.cbs.delete.
 */

Res CBSDelete(CBS cbs, Addr base, Addr limit) {
  Res res;

  AVERT(CBS, cbs);
  CBSEnter(cbs);

  AVER(base != NULL);
  AVER(limit > base);
  AVER(AddrIsAligned(base, cbs->alignment));
  AVER(AddrIsAligned(limit, cbs->alignment));

  res = CBSDeleteFromTree(cbs, base, limit);

  /* We rely on the consistency of the three free structures. */
  /* These checks don't distinguish "partially in" from */
  /* "not in". */
  if(cbs->mayUseInline) {
    AVER(res == ResOK || res == ResFAIL);
    if(res == ResFAIL) { /* wasn't in tree */
      res = CBSDeleteFromEmergencyBlockList(cbs, base, limit);
      if(res == ResFAIL) { /* wasn't in block list */
        res = CBSDeleteFromEmergencyGrainList(cbs, base, limit);
      }
    }
    /* always worth trying, wherever we found the deleted block */
    if(res == ResOK)
      CBSFlushEmergencyLists(cbs);
  }

  CBSLeave(cbs);
  return res;
}

Res CBSBlockDescribe(CBSBlock block, mps_lib_FILE *stream) {
  Res res;

  /* See .enter-leave.simple. */

  AVER(stream != NULL);

  res = WriteF(stream,
               "[$P,$P) {$U}", 
               (WriteFP)block->base, 
               (WriteFP)block->limit,
               (WriteFU)block->maxSize,
               NULL);
  if(res != ResOK)
    return res;

  return ResOK;
}

static Res CBSSplayNodeDescribe(SplayNode splayNode, 
                                mps_lib_FILE *stream) {
  Res res;
  CBSBlock cbsBlock;

  AVER(splayNode != NULL);
  AVER(stream != NULL);

  cbsBlock = CBSBlockOfSplayNode(splayNode);

  res = CBSBlockDescribe(cbsBlock, stream);

  if(res != ResOK)
    return res;

  return ResOK;
}


/* CBSIterate -- Iterate all blocks in CBS
 *
 * This is not necessarily efficient.
 *
 * See design.mps.cbs.function.cbs.iterate.
 */

/* Internal version for enter/leave checking. */
static void CBSIterateInternal(CBS cbs, CBSIterateMethod iterate,
                               void *closureP, unsigned long closureS) {
  SplayNode splayNode;
  SplayTree splayTree;
  CBSBlock cbsBlock;

  AVERT(CBS, cbs);
  AVER(FUNCHECK(iterate));

  splayTree = SplayTreeOfCBS(cbs);
  splayNode = SplayTreeFirst(splayTree, NULL);
  while(splayNode != NULL) {
    cbsBlock = CBSBlockOfSplayNode(splayNode);
    if(!(*iterate)(cbs, cbsBlock, closureP, closureS)) {
      break;
    }
    splayNode = SplayTreeNext(splayTree, splayNode, 
                              KeyOfCBSBlock(cbsBlock));
  }
  return;
}

void CBSIterate(CBS cbs, CBSIterateMethod iterate,
                void *closureP, unsigned long closureS) {
  AVERT(CBS, cbs);
  AVER(FUNCHECK(iterate));

  CBSEnter(cbs);

  CBSIterateInternal(cbs, iterate, closureP, closureS);

  CBSLeave(cbs);

  return;
}


/* CBSIterateLarge -- Iterate only large blocks
 *
 * This function iterates only blocks that are larger than or equal
 * to the minimum size.
 */

typedef struct CBSIterateLargeClosureStruct {
  void *p;
  unsigned long s;
  CBSIterateMethod f;
} CBSIterateLargeClosureStruct, *CBSIterateLargeClosure;

static Bool CBSIterateLargeAction(CBS cbs, CBSBlock block, 
                                  void *p, unsigned long s) {
  Bool b = TRUE;
  CBSIterateLargeClosure closure;

  closure = (CBSIterateLargeClosure)p;
  AVER(closure != NULL);
  AVER(s == (unsigned long)0);

  if (CBSBlockSize(block) >= cbs->minSize)
    b = (closure->f)(cbs, block, closure->p, closure->s);

  return b;
}


void CBSIterateLarge(CBS cbs, CBSIterateMethod iterate,
                     void *closureP, unsigned long closureS) {
  CBSIterateLargeClosureStruct closure;

  AVERT(CBS, cbs);
  CBSEnter(cbs);

  AVER(FUNCHECK(iterate));

  closure.p = closureP;
  closure.s = closureS;
  closure.f = iterate;

  CBSIterateInternal(cbs, &CBSIterateLargeAction, 
                     (void *)&closure, (unsigned long)0);

  CBSLeave(cbs);
  return;
}


/* CBSSetMinSize -- Set minimum interesting size for cbs
 *
 * This function may invoke the shrink and grow methods as
 * appropriate.  See design.mps.cbs.function.cbs.set.min-size.
 */

typedef struct {
  Size old;
  Size new;
} CBSSetMinSizeClosureStruct, *CBSSetMinSizeClosure;

static Bool CBSSetMinSizeGrow(CBS cbs, CBSBlock block,
                              void *p, unsigned long s) {
  CBSSetMinSizeClosure closure;
  Size size;
  
  UNUSED(s);
  closure = (CBSSetMinSizeClosure)p;
  AVER(closure->old > closure->new);
  size = CBSBlockSize(block);
  if(size < closure->old && size >= closure->new)
    (*cbs->new)(cbs, block, size, size);

  return TRUE;
}

static Bool CBSSetMinSizeShrink(CBS cbs, CBSBlock block,
                                void *p, unsigned long s) {
  CBSSetMinSizeClosure closure;
  Size size;
  
  UNUSED(s);
  closure = (CBSSetMinSizeClosure)p;
  AVER(closure->old < closure->new);
  size = CBSBlockSize(block);
  if(size >= closure->old && size < closure->new)
    (*cbs->delete)(cbs, block, size, size);

  return TRUE;
}

void CBSSetMinSize(CBS cbs, Size minSize) {
  CBSSetMinSizeClosureStruct closure;

  AVERT(CBS, cbs);
  CBSEnter(cbs);

  closure.old = cbs->minSize;
  closure.new = minSize;

  if(minSize < cbs->minSize)
    CBSIterateInternal(cbs, &CBSSetMinSizeGrow, 
                       (void *)&closure, (unsigned long)0);
  else if(minSize > cbs->minSize)
    CBSIterateInternal(cbs, &CBSSetMinSizeShrink, 
                       (void *)&closure, (unsigned long)0);

  cbs->minSize = minSize;

  CBSLeave(cbs);
}


static Bool CBSFindDeleteCheck(CBSFindDelete findDelete) 
{
  CHECKL(findDelete == CBSFindDeleteNONE ||
         findDelete == CBSFindDeleteLOW ||
         findDelete == CBSFindDeleteHIGH ||
         findDelete == CBSFindDeleteENTIRE);

  return TRUE;
}

typedef (*CBSDeleteMethod)(CBS cbs, Addr base, Addr limit);

static void CBSFindDeleteRange(Addr *baseReturn, Addr *limitReturn, 
                               CBS cbs, Addr base, Addr limit, Size size, 
                               CBSDeleteMethod delete, 
                               CBSFindDelete findDelete)
{
  Bool callDelete = TRUE;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(CBS, cbs);
  AVER(base < limit);
  AVER(size > 0);
  AVER(AddrOffset(base, limit) >= size);
  AVER(FUNCHECK(delete));
  AVERT(CBSFindDelete, findDelete);

  switch(findDelete) {

  case CBSFindDeleteNONE: {
    callDelete = FALSE;
  } break;

  case CBSFindDeleteLOW: {
    limit = AddrAdd(base, size);
  } break;

  case CBSFindDeleteHIGH: {
    base = AddrSub(limit, size);
  } break;

  case CBSFindDeleteENTIRE: {
    /* do nothing */
  } break;

  default: {
    NOTREACHED;
  } break;
  }

  if(callDelete) {
    Res res;
    res = (*delete)(cbs, base, limit);
    AVER(res == ResOK);
  }

  *baseReturn = base;
  *limitReturn = limit;
}


Bool CBSFindFirst(Addr *baseReturn, Addr *limitReturn,
                  CBS cbs, Size size, CBSFindDelete findDelete) 
{
  Bool found;
  Addr base = (Addr)0, limit = (Addr)0; /* only defined when found is TRUE */
  CBSDeleteMethod deleteMethod = NULL;

  AVERT(CBS, cbs);
  CBSEnter(cbs);

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVER(size > 0);
  AVER(sizeof(unsigned long) >= sizeof(Size));
  AVER(cbs->fastFind);
  AVERT(CBSFindDelete, findDelete);

  /* might do some good. */
  CBSFlushEmergencyLists(cbs);

  {
    SplayNode node;

    found = SplayFindFirst(&node, SplayTreeOfCBS(cbs), &CBSTestNode,
                           &CBSTestTree, NULL, (unsigned long)size);

    if(found) {
      CBSBlock block;

      block = CBSBlockOfSplayNode(node);
      AVER(CBSBlockSize(block) >= size);
      base = CBSBlockBase(block);
      limit = CBSBlockLimit(block);
      deleteMethod = &CBSDeleteFromTree;
    }
  }

  if(cbs->emergencyBlockList != NULL) {
    CBSEmergencyBlock block;

    for(block = cbs->emergencyBlockList;
        block != NULL &&
        (!found || CBSEmergencyBlockBase(block) < base);
        block = CBSEmergencyBlockNext(block)) {
      if(CBSEmergencyBlockSize(block) >= size) {
        found = TRUE;
        base = CBSEmergencyBlockBase(block);
        limit = CBSEmergencyBlockLimit(block);
        deleteMethod = &CBSDeleteFromEmergencyBlockList;
        break;
      }      
    }
  }

  if(cbs->emergencyGrainList != NULL && 
     size <= CBSEmergencyGrainSize(cbs)) {
    /* Take first grain */
    CBSEmergencyGrain grain = cbs->emergencyGrainList;

    if(!found || CBSEmergencyGrainBase(grain) < base) {
      found = TRUE;
      base = CBSEmergencyGrainBase(grain);
      limit = CBSEmergencyGrainLimit(cbs, grain);
      deleteMethod = &CBSDeleteFromEmergencyGrainList;
    }
  }

  if(found) {
    AVER(AddrOffset(base, limit) >= size);
    CBSFindDeleteRange(baseReturn, limitReturn, cbs, base, limit, size,
                       deleteMethod, findDelete);
  }

  CBSLeave(cbs);
  return found;
}


Bool CBSFindLast(Addr *baseReturn, Addr *limitReturn,
                 CBS cbs, Size size, CBSFindDelete findDelete) 
{
  Bool found;
  Addr base = (Addr)0, limit = (Addr)0; /* only defined in found is TRUE */

  AVERT(CBS, cbs);
  CBSEnter(cbs);

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVER(size > 0);
  AVER(sizeof(unsigned long) >= sizeof(Size));
  AVER(cbs->fastFind);
  AVERT(CBSFindDelete, findDelete);

  /* might do some good. */
  CBSFlushEmergencyLists(cbs);

  {
    SplayNode node;

    found = SplayFindLast(&node, SplayTreeOfCBS(cbs), &CBSTestNode,
                          &CBSTestTree, NULL, (unsigned long)size);
    if(found) {
      CBSBlock block;

      block = CBSBlockOfSplayNode(node);
      AVER(CBSBlockSize(block) >= size);
      CBSFindDeleteRange(&base, &limit, cbs, 
                         CBSBlockBase(block), CBSBlockLimit(block), 
                         size, &CBSDeleteFromTree, findDelete);
    }
  }

  if(cbs->emergencyBlockList != NULL) {
    CBSEmergencyBlock block;

    for(block = cbs->emergencyBlockList;
        block != NULL;
        block = CBSEmergencyBlockNext(block)) {
      if(CBSEmergencyBlockSize(block) >= size &&
         (!found || CBSEmergencyBlockBase(block) > base)) {
        found = TRUE;
        /* @@@ could be done more efficiently */
        CBSFindDeleteRange(&base, &limit, cbs,
                           CBSEmergencyBlockBase(block),
                           CBSEmergencyBlockLimit(block),
                           size, &CBSDeleteFromEmergencyBlockList,
                           findDelete);
      }      
    }
  }

  if(cbs->emergencyGrainList != NULL && 
     size <= CBSEmergencyGrainSize(cbs)) {
    CBSEmergencyGrain grain;

    /* Find last grain */
    for(grain = cbs->emergencyGrainList;
        CBSEmergencyGrainNext(grain) != NULL;
        grain = CBSEmergencyGrainNext(grain))
      NOOP;

    if(!found || CBSEmergencyGrainBase(grain) > base) {
      found = TRUE;
      /* @@@ could be done more efficiently */
      CBSFindDeleteRange(&base, &limit, cbs,
                         CBSEmergencyGrainBase(grain),
                         CBSEmergencyGrainLimit(cbs, grain),
                         size, &CBSDeleteFromEmergencyGrainList,
                         findDelete);
    }
  }

  if(found) {
    AVER(AddrOffset(base, limit) >= size);
    *baseReturn = base;
    *limitReturn = limit;
  }

  CBSLeave(cbs);
  return found;
}


/* CBSDescribe -- Describe a CBS
 *
 * See design.mps.cbs.function.cbs.describe.
 */

Res CBSDescribe(CBS cbs, mps_lib_FILE *stream) {
  Res res;

  AVERT(CBS, cbs);
  AVER(stream != NULL);

  res = WriteF(stream,
               "CBS $P {\n", (WriteFP)cbs,
               "  blockPool: $P\n", (WriteFP)cbs->blockPool,
               "  new: $F ", (WriteFF)cbs->new,
               "  delete: $F \n", (WriteFF)cbs->delete,
               NULL);
  if(res != ResOK)
    return res;

  res = SplayTreeDescribe(SplayTreeOfCBS(cbs), stream,
                          &CBSSplayNodeDescribe);
  if(res != ResOK)
    return res;

  res = WriteF(stream, "}\n", NULL);
  if(res != ResOK)
    return res;

  return ResOK;
}
