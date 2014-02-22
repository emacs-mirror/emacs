/* splay.c: SPLAY TREE IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: Splay trees are used to manage potentially unbounded
 * collections of ordered things.
 *
 * .source: <design/splay/>
 *
 * .note.stack: It's important that the MPS have a bounded stack
 * size, and this is a problem for tree algorithms.  Basically,
 * we have to avoid recursion.
 */


#include "splay.h"
#include "mpm.h"

SRCID(splay, "$Id$");


/* #define SPLAY_DEBUG */


/* Basic getter and setter methods */

#define SplayTreeRoot(t) RVALUE((t)->root)
#define SplayTreeSetRoot(t, r) BEGIN ((t)->root = (r)); END

#define SplayCompare(tree, key, node) (((tree)->compare)(node, key))


Bool SplayTreeCheck(SplayTree tree)
{
  UNUSED(tree);
  CHECKL(tree != NULL);
  CHECKL(FUNCHECK(tree->compare));
  CHECKL(FUNCHECK(tree->nodeKey));
  CHECKL(FUNCHECK(tree->updateNode));
  return TRUE;
}


void SplayTreeInit(SplayTree tree,
                   TreeCompare compare,
                   TreeKeyMethod nodeKey,
                   SplayUpdateNodeMethod updateNode)
{
  AVER(tree != NULL);
  AVER(FUNCHECK(compare));
  AVER(FUNCHECK(nodeKey));
  AVER(FUNCHECK(updateNode));

  tree->compare = compare;
  tree->nodeKey = nodeKey;
  tree->updateNode = updateNode;
  SplayTreeSetRoot(tree, TreeEMPTY);

  AVERT(SplayTree, tree);
}


void SplayTreeFinish(SplayTree tree)
{
  AVERT(SplayTree, tree);
  SplayTreeSetRoot(tree, TreeEMPTY);
  tree->compare = NULL;
  tree->updateNode = NULL;
}


void SplayTrivUpdate(SplayTree tree, Tree node)
{
  AVERT(SplayTree, tree);
  AVERT(Tree, node);
}


#ifdef SPLAY_DEBUG
static void SplayDebugUpdate(SplayTree tree, Tree node)
{
  if (node == TreeEMPTY)
    return;
  SplayDebugUpdate(tree, TreeLeft(node));
  SplayDebugUpdate(tree, TreeRight(node));
  tree->updateNode(tree, node);
}
#endif


/* SplayLinkRight -- Move top to left child of top
 *
 * Link the current top node into the left child of the right tree,
 * leaving the top node as the left child of the old top node.
 *
 * See <design/splay/#impl.link.right>.
 */

static void SplayLinkRight(Tree *topIO, Tree *rightIO)
{
  AVERT(Tree, *topIO);
  AVERT(Tree, *rightIO);

  /* Don't fix client properties yet. */

  /* .link.right.first: *rightIO is always the first node in the */
  /* right tree, so its left child must be null. */
  AVER(TreeLeft(*rightIO) == TreeEMPTY);

  TreeSetLeft(*rightIO, *topIO);
  *rightIO = *topIO;
  *topIO = TreeLeft(*topIO);

  /* The following line is only required for .link.right.first. */
  TreeSetLeft(*rightIO, TreeEMPTY);
}


/* SplayLinkLeft -- Move top to right child of top
 *
 * Link the current top node into the right child of the left tree,
 * leaving the top node as the right child of the old top node.
 *
 * See <design/splay/#impl.link.left>.
 */

static void SplayLinkLeft(Tree *topIO, Tree *leftIO) {
  AVERT(Tree, *topIO);
  AVERT(Tree, *leftIO);

  /* Don't fix client properties yet. */

  /* .link.left.first: *leftIO is always the last node in the */
  /* left tree, so its right child must be null. */
  AVER(TreeRight(*leftIO) == TreeEMPTY);

  TreeSetRight(*leftIO, *topIO);
  *leftIO = *topIO;
  *topIO = TreeRight(*topIO);

  /* The following line is only required for .link.left.first. */
  TreeSetRight(*leftIO, TreeEMPTY);
}


/* SplayUpdateLeftSpine -- undo pointer reversal, updating client property */

static Tree SplayUpdateLeftSpine(SplayTree tree, Tree node, Tree top)
{
  Tree child = TreeRight(top);
  while(node != TreeEMPTY) {
    Tree parent = TreeLeft(node);
    TreeSetLeft(node, child); /* un-reverse pointer */
    tree->updateNode(tree, node);
    child = node;
    node = parent;
  }
  return child;
}


/* SplayUpdateRightSpine -- undo pointer reversal, updating client property */

static Tree SplayUpdateRightSpine(SplayTree tree, Tree node, Tree top)
{
  Tree child = TreeLeft(top);
  while (node != TreeEMPTY) {
    Tree parent = TreeRight(node);
    TreeSetRight(node, child); /* un-reverse pointer */
    tree->updateNode(tree, node);
    child = node;
    node = parent;
  }
  return child;
}


/* SplayAssemble -- Assemble left right and top trees into one
 *
 * We do this by moving the children of the top tree to the last and
 * first nodes in the left and right trees, and then moving the tops
 * of the left and right trees to the children of the top tree.
 *
 * When we reach this function, the nodes between the roots of the
 * left and right trees and their last and first nodes respectively
 * will have out of date client properties.
 *
 * See <design/splay/#impl.assemble>.
 */

static void SplayAssemble(SplayTree tree, Tree top,
                          Tree leftTop, Tree leftLast,
                          Tree rightTop, Tree rightFirst)
{
  AVERT(SplayTree, tree);
  AVERT(Tree, top);
  AVERT(Tree, leftTop);
  AVERT(Tree, rightTop);

  if (leftTop != TreeEMPTY) {
    if (tree->updateNode != SplayTrivUpdate) {
      /* Update client property using pointer reversal (Ugh!). */
      Tree left, newLeft;
      left = TreeReverseRightSpine(leftTop);
      AVER(left == leftLast);
      newLeft = SplayUpdateRightSpine(tree, left, top);
      AVER(newLeft == leftTop);
      TreeSetLeft(top, newLeft);
    } else {
      AVER(leftLast != TreeEMPTY);
      TreeSetRight(leftLast, TreeLeft(top));
      TreeSetLeft(top, leftTop);
    }
  }

  if (rightTop != TreeEMPTY) {
    if (tree->updateNode != SplayTrivUpdate) {
      /* Update client property using pointer reversal (Ugh!). */
      Tree right, newRight;
      right = TreeReverseLeftSpine(rightTop);
      AVER(right == rightFirst);
      newRight = SplayUpdateLeftSpine(tree, rightFirst, top);
      AVER(newRight == rightTop);
      TreeSetRight(top, newRight);
    } else {
      AVER(rightFirst != TreeEMPTY);
      TreeSetLeft(rightFirst, TreeRight(top));
      TreeSetRight(top, rightTop);
    }
  }

  tree->updateNode(tree, top);
}


/* SplaySplay -- Splay the tree (top-down) around a given key
 *
 * If the key is not found, splays around an arbitrary neighbour.
 * Returns the relationship of the new tree root to the key.
 * This is the real logic behind splay trees.
 *
 * See <design/splay/#impl.splay>.
 */

static Compare SplaySplit(Tree *nodeReturn,
                          Tree *leftTopReturn, Tree *leftLastReturn,
                          Tree *rightTopReturn, Tree *rightFirstReturn,
                          SplayTree tree, TreeKey key, TreeCompare compare)
{
  /* The sides structure avoids a boundary case in SplayLink* */
  TreeStruct sides; /* rightTop and leftTop */
  Tree node, leftLast, rightFirst, leftPrev, rightNext;
  Compare cmp;

  AVERT(SplayTree, tree);
  AVER(FUNCHECK(compare));
  
  node = SplayTreeRoot(tree); /* will be copied back at end */

  TreeInit(&sides); /* left and right trees now TreeEMPTY */
  leftLast = &sides;
  rightFirst = &sides;

  if (node == TreeEMPTY) {
    cmp = CompareEQUAL;
    goto assemble;
  }

  for (;;) {
    Tree child;

    /* cmp is already initialised above. */
    cmp = compare(node, key);
    switch(cmp) {

    case CompareLESS:
      child = TreeLeft(node);
      if (child == TreeEMPTY)
        goto assemble;

      rightNext = rightFirst;
      SplayLinkRight(&node, &rightFirst);   /* zig */

      cmp = compare(child, key);
      switch(cmp) {
      default:
        NOTREACHED;
        /* defensive fall-through */

      case CompareEQUAL:
        goto assemble;

      case CompareLESS:                    /* zig-zig */
        if (TreeLeft(child) == TreeEMPTY)
          goto assemble;
        TreeSetLeft(rightFirst, TreeRight(node));
        TreeSetRight(node, rightFirst);
        TreeSetLeft(rightNext, node);
        rightFirst = node;
        node = TreeLeft(node);
        TreeClearLeft(rightFirst); /* for AVER */
        tree->updateNode(tree, TreeRight(rightFirst));
        break;

      case CompareGREATER:                 /* zig-zag */
        if (TreeRight(child) == TreeEMPTY)
          goto assemble;
        leftPrev = leftLast;
        SplayLinkLeft(&node, &leftLast);
        break;
      }
      break;

    case CompareGREATER:
      child = TreeRight(node);
      if (child == TreeEMPTY)
        goto assemble;

      leftPrev = leftLast;
      SplayLinkLeft(&node, &leftLast);      /* zag */

      cmp = compare(child, key);
      switch(cmp) {
      default:
        NOTREACHED;
        /* defensive fall-through */

      case CompareEQUAL:
        goto assemble;

      case CompareGREATER:                 /* zag-zag */
        if (TreeRight(child) == TreeEMPTY)
          goto assemble;
        TreeSetRight(leftLast, TreeLeft(node));
        TreeSetLeft(node, leftLast);
        TreeSetRight(leftPrev, node);
        leftLast = node;
        node = TreeRight(node);
        TreeClearRight(leftLast); /* for AVER */
        tree->updateNode(tree, TreeLeft(leftLast));
        break;

      case CompareLESS:                    /* zag-zig */
        if (TreeLeft(child) == TreeEMPTY)
          goto assemble;
        SplayLinkRight(&node, &rightFirst);
        break;
      }
      break;

    default:
      NOTREACHED;
      /* defensive fall-through */

    case CompareEQUAL:
      goto assemble;
    }
  }

assemble:
  *nodeReturn = node;
  *leftTopReturn = TreeRight(&sides);
  *leftLastReturn = leftLast == &sides ? TreeEMPTY : leftLast;
  *rightTopReturn = TreeLeft(&sides);
  *rightFirstReturn = rightFirst == &sides ? TreeEMPTY : rightFirst;
  return cmp;
}


static Compare SplaySplay(SplayTree tree, TreeKey key, TreeCompare compare)
{
  Compare cmp;
  Tree node, leftTop, leftLast, rightTop, rightFirst;
#ifdef SPLAY_DEBUG
  Count count = TreeDebugCount(SplayTreeRoot(tree), tree->compare, tree->nodeKey);
#endif

  cmp = SplaySplit(&node, &leftTop, &leftLast, &rightTop, &rightFirst,
                   tree, key, compare);

  SplayAssemble(tree, node,
                leftTop, leftLast,
                rightTop, rightFirst);

  SplayTreeSetRoot(tree, node);

#ifdef SPLAY_DEBUG
  AVER(count == TreeDebugCount(SplayTreeRoot(tree), tree->compare, tree->nodeKey));
#endif

  return cmp;
}


static void SplayLeft(SplayTree tree)
{
  /* The sides structure avoids a boundary case in SplayLink* */
  TreeStruct sides; /* rightTop and leftTop */
  Tree node, rightFirst;
#ifdef SPLAY_DEBUG
  Count count = TreeDebugCount(SplayTreeRoot(tree), tree->compare, tree->nodeKey);
#endif

  AVERT(SplayTree, tree);

  node = SplayTreeRoot(tree); /* will be copied back at end */

  if (node == TreeEMPTY)
    return;

  TreeInit(&sides); /* left and right trees now TreeEMPTY */
  rightFirst = &sides;

  for (;;) {
    Tree child = TreeLeft(node);
    if (child == TreeEMPTY)
      break;
    if (TreeLeft(child) == TreeEMPTY) {
      SplayLinkRight(&node, &rightFirst);
      break;
    }
    TreeRotateRight(&node);
    tree->updateNode(tree, TreeRight(node));
    SplayLinkRight(&node, &rightFirst);
  }

  SplayAssemble(tree, node,
                TreeEMPTY, TreeEMPTY,
                TreeLeft(&sides), rightFirst);

  SplayTreeSetRoot(tree, node);

#ifdef SPLAY_DEBUG
  AVER(count == TreeDebugCount(SplayTreeRoot(tree), tree->compare, tree->nodeKey));
#endif
}


static void SplayRight(SplayTree tree)
{
  /* The sides structure avoids a boundary case in SplayLink* */
  TreeStruct sides; /* rightTop and leftTop */
  Tree node, leftLast;
#ifdef SPLAY_DEBUG
  Count count = TreeDebugCount(SplayTreeRoot(tree), tree->compare, tree->nodeKey);
#endif

  AVERT(SplayTree, tree);

  node = SplayTreeRoot(tree); /* will be copied back at end */

  if (node == TreeEMPTY)
    return;

  TreeInit(&sides); /* left and right trees now TreeEMPTY */
  leftLast = &sides;

  for (;;) {
    Tree child = TreeRight(node);
    if (child == TreeEMPTY)
      break;
    if (TreeRight(child) == TreeEMPTY) {
      SplayLinkLeft(&node, &leftLast);
      break;
    }
    TreeRotateLeft(&node);
    tree->updateNode(tree, TreeLeft(node));
    SplayLinkLeft(&node, &leftLast);
  }

  SplayAssemble(tree, node,
                TreeRight(&sides), leftLast,
                TreeEMPTY, TreeEMPTY);

  SplayTreeSetRoot(tree, node);

#ifdef SPLAY_DEBUG
  AVER(count == TreeDebugCount(SplayTreeRoot(tree), tree->compare, tree->nodeKey));
#endif
}


/* SplayTreeInsert -- Insert a node into a splay tree
 *
 * See <design/splay/#function.splay.tree.insert> and
 * <design/splay/#impl.insert>.
 */

Bool SplayTreeInsert(SplayTree tree, Tree node) {
  AVERT(SplayTree, tree);
  AVERT(Tree, node);
  AVER(TreeLeft(node) == TreeEMPTY);
  AVER(TreeRight(node) == TreeEMPTY);

  if (SplayTreeRoot(tree) == TreeEMPTY) {
    SplayTreeSetRoot(tree, node);
  } else {
    Tree neighbour;
    switch (SplaySplay(tree, tree->nodeKey(node), tree->compare)) {
    default:
      NOTREACHED;
      /* defensive fall-through */
    case CompareEQUAL:
      return FALSE;
      
    case CompareGREATER: /* left neighbour */
      neighbour = SplayTreeRoot(tree);
      SplayTreeSetRoot(tree, node);
      TreeSetRight(node, TreeRight(neighbour));
      TreeSetLeft(node, neighbour);
      TreeSetRight(neighbour, TreeEMPTY);
      break;

    case CompareLESS: /* right neighbour */
      neighbour = SplayTreeRoot(tree);
      SplayTreeSetRoot(tree, node);
      TreeSetLeft(node, TreeLeft(neighbour));
      TreeSetRight(node, neighbour);
      TreeSetLeft(neighbour, TreeEMPTY);
      break;
    }

    tree->updateNode(tree, neighbour);
    tree->updateNode(tree, node);
  }

  return TRUE;
}


/* SplayTreeDelete -- Delete a node from a splay tree
 *
 * See <design/splay/#function.splay.tree.delete> and
 * <design/splay/#impl.delete>.
 */

Bool SplayTreeDelete(SplayTree tree, Tree node) {
  Tree leftLast;
  Compare cmp;

  AVERT(SplayTree, tree);
  AVERT(Tree, node);

  cmp = SplaySplay(tree, tree->nodeKey(node), tree->compare);
  AVER(cmp != CompareEQUAL || SplayTreeRoot(tree) == node);

  if (cmp != CompareEQUAL) {
    return FALSE;
  } else if (TreeLeft(node) == TreeEMPTY) {
    SplayTreeSetRoot(tree, TreeRight(node));
    TreeClearRight(node);
  } else if (TreeRight(node) == TreeEMPTY) {
    SplayTreeSetRoot(tree, TreeLeft(node));
    TreeClearLeft(node);
  } else {
    Tree rightHalf = TreeRight(node);
    TreeClearRight(node);
    SplayTreeSetRoot(tree, TreeLeft(node));
    TreeClearLeft(node);
    SplayRight(tree);
    leftLast = SplayTreeRoot(tree);
    AVER(leftLast != TreeEMPTY);
    AVER(TreeRight(leftLast) == TreeEMPTY);
    TreeSetRight(leftLast, rightHalf);
    tree->updateNode(tree, leftLast);
  }

  TreeFinish(node);

  return TRUE;
}


/* SplayTreeFind -- search for a node in a splay tree matching a key
 *
 * See <design/splay/#function.splay.tree.search> and
 * <design/splay/#impl.search>.
 */

Bool SplayTreeFind(Tree *nodeReturn, SplayTree tree, TreeKey key) {
  AVERT(SplayTree, tree);
  AVER(nodeReturn != NULL);

  if (SplayTreeRoot(tree) == TreeEMPTY)
    return FALSE;

  if (SplaySplay(tree, key, tree->compare) != CompareEQUAL)
    return FALSE;

  *nodeReturn = SplayTreeRoot(tree);
  return TRUE;
}


/* SplayTreeSuccessor -- Splays a tree at the root's successor
 *
 * Must not be called on en empty tree.  Successor need not exist,
 * in which case TreeEMPTY is returned, and the tree is unchanged.
 */

static Tree SplayTreeSuccessor(SplayTree tree) {
  Tree oldRoot, newRoot;

  AVERT(SplayTree, tree);

  oldRoot = SplayTreeRoot(tree);
  AVERT(Tree, oldRoot);

  if (TreeRight(oldRoot) == TreeEMPTY) {
    newRoot = TreeEMPTY; /* No successor */
  } else {
    /* temporarily chop off the left half-tree, inclusive of root */
    SplayTreeSetRoot(tree, TreeRight(oldRoot));
    TreeSetRight(oldRoot, TreeEMPTY);
    SplayLeft(tree);
    newRoot = SplayTreeRoot(tree);
    AVER(newRoot != TreeEMPTY);
    AVER(TreeLeft(newRoot) == TreeEMPTY);
    TreeSetLeft(newRoot, oldRoot);
    tree->updateNode(tree, oldRoot);
    tree->updateNode(tree, newRoot);
  }

  return newRoot;
}


/* SplayTreeNeighbours
 *
 * Search for the two nodes in a splay tree neighbouring a key.
 *
 * See <design/splay/#function.splay.tree.neighbours> and
 * <design/splay/#impl.neighbours>.
 */

Bool SplayTreeNeighbours(Tree *leftReturn, Tree *rightReturn,
                         SplayTree tree, TreeKey key)
{
  Tree node, leftTop, leftLast, rightTop, rightFirst;
  Bool found;
#ifdef SPLAY_DEBUG
  Count count = TreeDebugCount(SplayTreeRoot(tree), tree->compare, tree->nodeKey);
#endif


  AVERT(SplayTree, tree);
  AVER(leftReturn != NULL);
  AVER(rightReturn != NULL);

  if (SplayTreeRoot(tree) == TreeEMPTY) {
    *leftReturn = *rightReturn = TreeEMPTY;
    return TRUE;
  }

  switch (SplaySplit(&node, &leftTop, &leftLast, &rightTop, &rightFirst,
                     tree, key, tree->compare)) {
  default:
    NOTREACHED;
    /* defensive fall-through */
  case CompareEQUAL:
    found = FALSE;
    break;

  case CompareLESS:
    AVER(TreeLeft(node) == TreeEMPTY);
    *rightReturn = node;
    *leftReturn = leftLast;
    found = TRUE;
    break;

  case CompareGREATER:
    AVER(TreeRight(node) == TreeEMPTY);
    *leftReturn = node;
    *rightReturn = rightFirst;
    found = TRUE;
    break;
  }

  SplayAssemble(tree, node,
                leftTop, leftLast,
                rightTop, rightFirst);
  SplayTreeSetRoot(tree, node);

#ifdef SPLAY_DEBUG
  AVER(count == TreeDebugCount(SplayTreeRoot(tree), tree->compare, tree->nodeKey));
#endif

  return found;
}


/* SplayTreeFirst, SplayTreeNext -- Iterators
 *
 * SplayTreeFirst receives a key that must precede all
 * nodes in the tree.  It returns TreeEMPTY if the tree is empty.
 * Otherwise, it splays the tree to the first node, and returns the
 * new root.  See <design/splay/#function.splay.tree.first>.
 *
 * SplayTreeNext takes a tree and splays it to the successor of the
 * old root, and returns the new root.  Returns TreeEMPTY is there are
 * no successors.  It takes a key for the old root.  See
 * <design/splay/#function.splay.tree.next>.
 */

Tree SplayTreeFirst(SplayTree tree) {
  Tree node;

  AVERT(SplayTree, tree);

  if (SplayTreeRoot(tree) == TreeEMPTY)
    return TreeEMPTY;
  
  SplayLeft(tree);
  node = SplayTreeRoot(tree);
  AVER(node != TreeEMPTY);
  AVER(TreeLeft(node) == TreeEMPTY);

  return node;
}

Tree SplayTreeNext(SplayTree tree, TreeKey oldKey) {
  AVERT(SplayTree, tree);

  if (SplayTreeRoot(tree) == TreeEMPTY)
    return TreeEMPTY;
  
  /* Make old node the root.  Probably already is.  We don't mind if the
     node has been deleted, or replaced by a node with the same key. */
  switch (SplaySplay(tree, oldKey, tree->compare)) {
  default:
    NOTREACHED;
    /* defensive fall-through */
  case CompareGREATER:
    return SplayTreeRoot(tree);

  case CompareLESS:
  case CompareEQUAL:
    return SplayTreeSuccessor(tree);
  }
}


/* SplayNodeDescribe -- Describe a node in the splay tree
 *
 * Note that this breaks the restriction of .note.stack.
 * This is alright as the function is debug only.
 */

static Res SplayNodeDescribe(Tree node, mps_lib_FILE *stream,
                             SplayNodeDescribeMethod nodeDescribe) {
  Res res;

#if defined(AVER_AND_CHECK)
  if (!TreeCheck(node)) return ResFAIL;
  /* stream and nodeDescribe checked by SplayTreeDescribe */
#endif

  res = WriteF(stream, "( ", NULL);
  if (res != ResOK) return res;

  if (TreeLeft(node) != TreeEMPTY) {
    res = SplayNodeDescribe(TreeLeft(node), stream, nodeDescribe);
    if (res != ResOK) return res;

    res = WriteF(stream, " / ", NULL);
    if (res != ResOK) return res;
  }

  res = (*nodeDescribe)(node, stream);
  if (res != ResOK) return res;

  if (TreeRight(node) != TreeEMPTY) {
    res = WriteF(stream, " \\ ", NULL);
    if (res != ResOK) return res;

    res = SplayNodeDescribe(TreeRight(node), stream, nodeDescribe);
    if (res != ResOK) return res;
  }

  res = WriteF(stream, " )", NULL);
  if (res != ResOK) return res;

  return ResOK;
}


typedef struct {
  SplayTestNodeMethod testNode;
  SplayTestTreeMethod testTree;
  void *p;
  Size s;
  SplayTree tree;
} SplayFindClosureStruct, *SplayFindClosure;

static Compare SplayFindFirstCompare(Tree node, TreeKey key)
{
  SplayFindClosure closure;
  void *closureP;
  Size closureS;
  SplayTestNodeMethod testNode;
  SplayTestTreeMethod testTree;
  SplayTree tree;

  AVERT(Tree, node);
  AVER(key != NULL);

  closure = (SplayFindClosure)key;
  closureP = closure->p;
  closureS = closure->s;
  testNode = closure->testNode;
  testTree = closure->testTree;
  tree = closure->tree;

  if (TreeLeft(node) != TreeEMPTY &&
     (*testTree)(tree, TreeLeft(node), closureP, closureS)) {
    return CompareLESS;
  } else if ((*testNode)(tree, node, closureP, closureS)) {
    return CompareEQUAL;
  } else {
    AVER(TreeRight(node) != TreeEMPTY);
    AVER((*testTree)(tree, TreeRight(node), closureP, closureS));
    return CompareGREATER;
  }
}

static Compare SplayFindLastCompare(Tree node, TreeKey key)
{
  SplayFindClosure closure;
  void *closureP;
  Size closureS;
  SplayTestNodeMethod testNode;
  SplayTestTreeMethod testTree;
  SplayTree tree;

  AVERT(Tree, node);
  AVER(key != NULL);

  closure = (SplayFindClosure)key;
  closureP = closure->p;
  closureS = closure->s;
  testNode = closure->testNode;
  testTree = closure->testTree;
  tree = closure->tree;

  if (TreeRight(node) != TreeEMPTY &&
     (*testTree)(tree, TreeRight(node), closureP, closureS)) {
     return CompareGREATER;
  } else if ((*testNode)(tree, node, closureP, closureS)) {
    return CompareEQUAL;
  } else {
    AVER(TreeLeft(node) != TreeEMPTY);
    AVER((*testTree)(tree, TreeLeft(node), closureP, closureS));
    return CompareLESS;
  }
}


/* SplayFindFirst -- Find first node that satisfies client property
 *
 * This function finds the first node (in address order) in the given
 * tree that satisfies some property defined by the client.  The
 * property is such that the client can detect, given a sub-tree,
 * whether that sub-tree contains any nodes satisfying the property.
 *
 * The given callbacks testNode and testTree detect this property in
 * a single node or a sub-tree rooted at a node, and both receive the
 * arbitrary closures closureP and closureS.
 */

Bool SplayFindFirst(Tree *nodeReturn, SplayTree tree,
                           SplayTestNodeMethod testNode,
                           SplayTestTreeMethod testTree,
                           void *closureP, Size closureS)
{
  Tree node;
  SplayFindClosureStruct closureStruct;

  AVER(nodeReturn != NULL);
  AVERT(SplayTree, tree);
  AVER(FUNCHECK(testNode));
  AVER(FUNCHECK(testTree));

  node = SplayTreeRoot(tree);

  if (node == TreeEMPTY || !(*testTree)(tree, node, closureP, closureS))
    return FALSE; /* no suitable nodes in tree */

  closureStruct.p = closureP;
  closureStruct.s = closureS;
  closureStruct.testNode = testNode;
  closureStruct.testTree = testTree;
  closureStruct.tree = tree;

  if (SplaySplay(tree, &closureStruct, SplayFindFirstCompare) != CompareEQUAL)
    return FALSE;

  *nodeReturn = SplayTreeRoot(tree);
  return TRUE;
}


/* SplayFindLast -- As SplayFindFirst but in reverse address order */

Bool SplayFindLast(Tree *nodeReturn, SplayTree tree,
                          SplayTestNodeMethod testNode,
                          SplayTestTreeMethod testTree,
                          void *closureP, Size closureS)
{
  Tree node;
  SplayFindClosureStruct closureStruct;

  AVER(nodeReturn != NULL);
  AVERT(SplayTree, tree);
  AVER(FUNCHECK(testNode));
  AVER(FUNCHECK(testTree));

  node = SplayTreeRoot(tree);

  if (node == TreeEMPTY || !(*testTree)(tree, node, closureP, closureS))
    return FALSE; /* no suitable nodes in tree */

  closureStruct.p = closureP;
  closureStruct.s = closureS;
  closureStruct.testNode = testNode;
  closureStruct.testTree = testTree;
  closureStruct.tree = tree;

  if (SplaySplay(tree, &closureStruct, SplayFindLastCompare) != CompareEQUAL)
    return FALSE;

  *nodeReturn = SplayTreeRoot(tree);
  return TRUE;
}


/* SplayRoot -- return the root node of the tree */

Bool SplayRoot(Tree *nodeReturn, SplayTree tree)
{
  Tree node;

  AVER(nodeReturn != NULL);
  AVERT(SplayTree, tree);

  node = SplayTreeRoot(tree);
  if (node == TreeEMPTY)
    return FALSE;
  else {
    *nodeReturn = node;
    return TRUE;
  }
}


/* SplayNodeRefresh -- Updates the client property that has changed at a node
 *
 * This function undertakes to call the client updateNode callback for each
 * node affected by the change in properties at the given node (which has
 * the given key) in an appropriate order.
 *
 * The function fullfils its job by first splaying at the given node, and
 * updating the single node.  This may change.
 */

void SplayNodeRefresh(SplayTree tree, Tree node)
{
  Compare cmp;

  AVERT(SplayTree, tree);
  AVERT(Tree, node);

  cmp = SplaySplay(tree, tree->nodeKey(node), tree->compare);
  AVER(cmp == CompareEQUAL);
  AVER(SplayTreeRoot(tree) == node);

  tree->updateNode(tree, node);
}


/* SplayTreeDescribe -- Describe a splay tree
 *
 * See <design/splay/#function.splay.tree.describe>.
 */

Res SplayTreeDescribe(SplayTree tree, mps_lib_FILE *stream,
                      SplayNodeDescribeMethod nodeDescribe) {
  Res res;

#if defined(AVER_AND_CHECK)
  if (!SplayTreeCheck(tree)) return ResFAIL;
  if (stream == NULL) return ResFAIL;
  if (!FUNCHECK(nodeDescribe)) return ResFAIL;
#endif

  res = WriteF(stream,
               "Splay $P {\n", (WriteFP)tree,
               "  compare $F\n", (WriteFF)tree->compare,
               NULL);
  if (res != ResOK) return res;

  if (SplayTreeRoot(tree) != TreeEMPTY) {
    res = SplayNodeDescribe(SplayTreeRoot(tree), stream, nodeDescribe);
    if (res != ResOK) return res;
  }

  res = WriteF(stream, "\n}\n", NULL);
  return res;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
