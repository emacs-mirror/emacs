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


/* Basic getter and setter methods */

#define SplayTreeRoot(t) RVALUE((t)->root)
#define SplayTreeSetRoot(t, r) BEGIN ((t)->root = (r)); END

#define SplayCompare(tree, key, node) (((tree)->compare)(node, key))


Bool SplayTreeCheck(SplayTree tree)
{
  UNUSED(tree);
  CHECKL(tree != NULL);
  CHECKL(FUNCHECK(tree->compare));
  CHECKL(tree->updateNode == NULL || FUNCHECK(tree->updateNode));
  return TRUE;
}


void SplayTreeInit(SplayTree tree,
                   TreeCompare compare,
                   SplayUpdateNodeMethod updateNode)
{
  AVER(tree != NULL);
  AVER(FUNCHECK(compare));
  AVER(FUNCHECK(updateNode));

  tree->compare = compare;
  tree->updateNode = updateNode;
  SplayTreeSetRoot(tree, NULL);

  AVERT(SplayTree, tree);
}


void SplayTreeFinish(SplayTree tree)
{
  AVERT(SplayTree, tree);
  SplayTreeSetRoot(tree, NULL);
  tree->compare = NULL;
}


void SplayTrivUpdate(SplayTree tree, Tree node)
{
  AVERT(SplayTree, tree);
  AVERT(Tree, node);
}


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
  TreeSetLeft(*rightIO, NULL);
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
  TreeSetRight(*leftIO, NULL);
}


/* SplayRotateLeft -- Rotate right child edge of node
 *
 * Rotates node, right child of node, and left child of right
 * child of node, leftwards in the order stated.
 *
 * See <design/splay/#impl.rotate.left>.
 */

static void SplayRotateLeft(Tree *nodeIO, SplayTree tree) {
  Tree nodeRight;

  AVER(nodeIO != NULL);
  AVERT(Tree, *nodeIO);
  AVERT(Tree, TreeRight(*nodeIO));
  AVERT(SplayTree, tree);

  nodeRight = TreeRight(*nodeIO);
  TreeSetRight(*nodeIO, TreeLeft(nodeRight));
  TreeSetLeft(nodeRight, *nodeIO);
  *nodeIO = nodeRight;

  tree->updateNode(tree, TreeLeft(nodeRight));
  /* Don't need to update new root because we know that we will */
  /* do either a link or an assemble next, and that will sort it */
  /* out. */
}


/* SplayRotateRight -- Rotate left child edge of node
 *
 * Rotates node, left child of node, and right child of left
 * child of node, leftwards in the order stated.
 *
 * See <design/splay/#impl.rotate.right>.
 */

static void SplayRotateRight(Tree *nodeIO, SplayTree tree) {
  Tree nodeLeft;

  AVER(nodeIO != NULL);
  AVERT(Tree, *nodeIO);
  AVERT(Tree, TreeLeft(*nodeIO));
  AVERT(SplayTree, tree);

  nodeLeft = TreeLeft(*nodeIO);
  TreeSetLeft(*nodeIO, TreeRight(nodeLeft));
  TreeSetRight(nodeLeft, *nodeIO);
  *nodeIO = nodeLeft;

  tree->updateNode(tree, TreeRight(nodeLeft));
  /* Don't need to update new root because we know that we will */
  /* do either a link or an assemble next, and that will sort it */
  /* out. */
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
                          Tree rightTop, Tree rightFirst) {
  AVERT(SplayTree, tree);
  AVERT(Tree, top);
  AVER(leftTop == TreeEMPTY ||
       (TreeCheck(leftTop) && TreeCheck(leftLast)));
  AVER(rightTop == TreeEMPTY ||
       (TreeCheck(rightTop) && TreeCheck(rightFirst)));

  if (leftTop != TreeEMPTY) {
    TreeSetRight(leftLast, TreeLeft(top));
    TreeSetLeft(top, leftTop);

    if (tree->updateNode != SplayTrivUpdate) {
      /* Update client property using pointer reversal (Ugh!). */
      Tree node, parent, rightChild;

      /* Reverse the pointers between leftTop and leftLast */
      /* leftLast is not reversed. */
      node = leftTop;
      parent = NULL;
      while(node != leftLast) {
        rightChild = TreeRight(node);
        TreeSetRight(node, parent); /* pointer reversal */
        parent = node;
        node = rightChild;
       }

      /* Now restore the pointers, updating the client property. */
      /* node is leftLast, parent is the last parent (or NULL). */
      tree->updateNode(tree, node);
      while(node != leftTop) {
        rightChild = node;
        node = parent;
        parent = TreeRight(node);
        TreeSetRight(node, rightChild); /* un-reverse pointer */
        tree->updateNode(tree, node);
      }
    }
  }
  /* otherwise leave top->left alone */

  if (rightTop != TreeEMPTY) {
    TreeSetLeft(rightFirst, TreeRight(top));
    TreeSetRight(top, rightTop);

    if (tree->updateNode != SplayTrivUpdate) {
      /* Update client property using pointer reversal (Ugh!). */
      Tree node, parent, leftChild;

      /* Reverse the pointers between rightTop and rightFirst */
      /* ightFirst is not reversed. */
      node = rightTop;
      parent = NULL;
      while(node != rightFirst) {
        leftChild = TreeLeft(node);
        TreeSetLeft(node, parent); /* pointer reversal */
        parent = node;
        node = leftChild;
       }

      /* Now restore the pointers, updating the client property. */
      /* node is rightFirst, parent is the last parent (or NULL). */
      tree->updateNode(tree, node);
      while(node != rightTop) {
        leftChild = node;
        node = parent;
        parent = TreeLeft(node);
        TreeSetLeft(node, leftChild); /* un-reverse pointer */
        tree->updateNode(tree, node);
      }
    }
  }
  /* otherwise leave top->right alone */

  tree->updateNode(tree, top);
}


/* SplaySplay -- Splay the tree (top-down) around a given key
 *
 * If the key is not found, splays around an arbitrary neighbour.
 * Returns whether key was found.  This is the real logic behind
 * splay trees.
 *
 * See <design/splay/#impl.splay>.
 */

static Bool SplaySplay(Tree *nodeReturn, SplayTree tree,
                       TreeKey key, TreeCompare compare) {
  /* The sides structure avoids a boundary case in SplayLink* */
  TreeStruct sides; /* rightTop and leftTop */
  Tree top, leftLast, rightFirst;
  Bool found;
  Compare compareTop;

  AVERT(SplayTree, tree);
  AVER(nodeReturn != NULL);
  AVER(FUNCHECK(compare));

  top = SplayTreeRoot(tree); /* will be copied back at end */

  if (top == TreeEMPTY) {
    *nodeReturn = NULL;
    return FALSE;
  }

  /* short-circuit case where node is already top */
  compareTop = compare(top, key);
  if (compareTop == CompareEQUAL) {
    *nodeReturn = top;
    return TRUE;
  }

  TreeInit(&sides); /* left and right trees now TreeEMPTY */
  leftLast = &sides;
  rightFirst = &sides;

  while(TRUE) {
    /* compareTop is already initialised above. */
    switch(compareTop) {

    case CompareLESS: {
      Tree topLeft = TreeLeft(top);
      if (topLeft == TreeEMPTY) {
        found = FALSE;
        goto assemble;
      } else {
        Compare compareTopLeft = compare(topLeft, key);

        switch(compareTopLeft) {

        case CompareEQUAL: {                 /* zig */
          SplayLinkRight(&top, &rightFirst);
          found = TRUE;
          goto assemble;
        } /* break; */

        case CompareLESS: {                  /* zig-zig */
          if (TreeLeft(topLeft) == TreeEMPTY)
            goto terminalZig;
          SplayRotateRight(&top, tree);
          SplayLinkRight(&top, &rightFirst);
        } break;

        case CompareGREATER: {               /* zig-zag */
          if (TreeRight(topLeft) == TreeEMPTY)
            goto terminalZig;
          SplayLinkRight(&top, &rightFirst);
          SplayLinkLeft(&top, &leftLast);
        } break;

        default: {
          NOTREACHED;
        } break;
        }
      }
    } break;

    case CompareGREATER: {
      Tree topRight = TreeRight(top);
      if (topRight == TreeEMPTY) {
        found = FALSE;
        goto assemble;
      } else {
        Compare compareTopRight = compare(topRight, key);

        switch(compareTopRight) {

        case CompareEQUAL: {                 /* zag */
            SplayLinkLeft(&top, &leftLast);
            found = TRUE;
            goto assemble;
        } /* break; */

        case CompareGREATER: {               /* zag-zag */
          if (TreeRight(topRight) == TreeEMPTY)
            goto terminalZag;
          SplayRotateLeft(&top, tree);
          SplayLinkLeft(&top, &leftLast);
        } break;

        case CompareLESS: {                  /* zag-zig */
          if (TreeLeft(topRight) == TreeEMPTY)
            goto terminalZag;
          SplayLinkLeft(&top, &leftLast);
          SplayLinkRight(&top, &rightFirst);
        } break;

        default: {
          NOTREACHED;
        } break;
        }
      }
    } break;

    case CompareEQUAL: {
      found = TRUE;
      goto assemble;
    } /* break; */

    default: {
      NOTREACHED;
    } break;
    }
    compareTop = compare(top, key);
  } /* end while(TRUE) */

terminalZig:
  SplayLinkRight(&top, &rightFirst);
  found = FALSE;
  goto assemble;

terminalZag:
  SplayLinkLeft(&top, &leftLast);
  found = FALSE;
  goto assemble;

assemble:
  SplayAssemble(tree, top,
                TreeRight(&sides), leftLast,
                TreeLeft(&sides), rightFirst);

  SplayTreeSetRoot(tree, top);
  *nodeReturn = top;

  return found;
}


/* SplayTreeInsert -- Insert a node into a splay tree
 *
 * See <design/splay/#function.splay.tree.insert> and
 * <design/splay/#impl.insert>.
 */

Res SplayTreeInsert(SplayTree tree, Tree node, TreeKey key) {
  Tree neighbour;

  AVERT(SplayTree, tree);
  AVERT(Tree, node);
  AVER(TreeLeft(node) == TreeEMPTY);
  AVER(TreeRight(node) == TreeEMPTY);

  if (SplayTreeRoot(tree) == TreeEMPTY) {
    SplayTreeSetRoot(tree, node);
  } else if (SplaySplay(&neighbour, tree, key, tree->compare)) {
    return ResFAIL;
  } else {
    AVER(SplayTreeRoot(tree) == neighbour);
    switch(SplayCompare(tree, key, neighbour)) {

    case CompareGREATER: { /* left neighbour */
      SplayTreeSetRoot(tree, node);
      TreeSetRight(node, TreeRight(neighbour));
      TreeSetLeft(node, neighbour);
      TreeSetRight(neighbour, NULL);
    } break;

    case CompareLESS: { /* right neighbour */
      SplayTreeSetRoot(tree, node);
      TreeSetLeft(node, TreeLeft(neighbour));
      TreeSetRight(node, neighbour);
      TreeSetLeft(neighbour, NULL);
    } break;

    case CompareEQUAL:
    default: {
      NOTREACHED;
    } break;
    }

    tree->updateNode(tree, neighbour);
    tree->updateNode(tree, node);
  }

  return ResOK;
}


/* SplayTreeDelete -- Delete a node from a splay tree
 *
 * See <design/splay/#function.splay.tree.delete> and
 * <design/splay/#impl.delete>.
 */

Res SplayTreeDelete(SplayTree tree, Tree node, TreeKey key) {
  Tree rightHalf, del, leftLast;
  Bool found;

  AVERT(SplayTree, tree);
  AVERT(Tree, node);

  found = SplaySplay(&del, tree, key, tree->compare);
  AVER(!found || del == node);

  if (!found) {
    return ResFAIL;
  } else if (TreeLeft(node) == TreeEMPTY) {
    SplayTreeSetRoot(tree, TreeRight(node));
    TreeClearRight(node);
  } else if (TreeRight(node) == TreeEMPTY) {
    SplayTreeSetRoot(tree, TreeLeft(node));
    TreeClearLeft(node);
  } else {
    rightHalf = TreeRight(node);
    TreeClearRight(node);
    SplayTreeSetRoot(tree, TreeLeft(node));
    TreeClearLeft(node);
    if (SplaySplay(&leftLast, tree, key, tree->compare)) {
      return ResFAIL;
    } else {
      AVER(TreeRight(leftLast) == TreeEMPTY);
      TreeSetRight(leftLast, rightHalf);
      tree->updateNode(tree, leftLast);
    }
  }

  TreeFinish(node);

  return ResOK;
}


/* SplayTreeSearch -- Search for a node in a splay tree matching a key
 *
 * See <design/splay/#function.splay.tree.search> and
 * <design/splay/#impl.search>.
 */

Res SplayTreeSearch(Tree *nodeReturn, SplayTree tree, TreeKey key) {
  Tree node;

  AVERT(SplayTree, tree);
  AVER(nodeReturn != NULL);

  if (SplaySplay(&node, tree, key, tree->compare)) {
    *nodeReturn = node;
  } else {
    return ResFAIL;
  }

  return ResOK;
}


/* SplayTreePredecessor -- Splays a tree at the root's predecessor
 *
 * Must not be called on en empty tree.  Predecessor need not exist,
 * in which case NULL is returned, and the tree is unchanged.
 */

static Tree SplayTreePredecessor(SplayTree tree, TreeKey key) {
  Tree oldRoot, newRoot;

  AVERT(SplayTree, tree);

  oldRoot = SplayTreeRoot(tree);
  AVERT(Tree, oldRoot);

  if (TreeLeft(oldRoot) == TreeEMPTY) {
    newRoot = NULL; /* No predecessor */
  } else {
    /* temporarily chop off the right half-tree, inclusive of root */
    SplayTreeSetRoot(tree, TreeLeft(oldRoot));
    TreeSetLeft(oldRoot, NULL);
    if (SplaySplay(&newRoot, tree, key, tree->compare)) {
      NOTREACHED; /* Another matching node found */
    } else {
      AVER(TreeRight(newRoot) == TreeEMPTY);
      TreeSetRight(newRoot, oldRoot);
    }

    tree->updateNode(tree, oldRoot);
    tree->updateNode(tree, newRoot);
  }

  return newRoot;
}


/* SplayTreeSuccessor -- Splays a tree at the root's successor
 *
 * Must not be called on en empty tree.  Successor need not exist,
 * in which case NULL is returned, and the tree is unchanged.
 */

static Tree SplayTreeSuccessor(SplayTree tree, TreeKey key) {
  Tree oldRoot, newRoot;

  AVERT(SplayTree, tree);

  oldRoot = SplayTreeRoot(tree);
  AVERT(Tree, oldRoot);

  if (TreeRight(oldRoot) == TreeEMPTY) {
    newRoot = NULL; /* No successor */
  } else {
    /* temporarily chop off the left half-tree, inclusive of root */
    SplayTreeSetRoot(tree, TreeRight(oldRoot));
    TreeSetRight(oldRoot, NULL);
    if (SplaySplay(&newRoot, tree, key, tree->compare)) {
      NOTREACHED; /* Another matching node found */
    } else {
      AVER(TreeLeft(newRoot) == TreeEMPTY);
      TreeSetLeft(newRoot, oldRoot);
    }

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


Res SplayTreeNeighbours(Tree *leftReturn, Tree *rightReturn,
                        SplayTree tree, TreeKey key) {
  Tree neighbour;

  AVERT(SplayTree, tree);
  AVER(leftReturn != NULL);
  AVER(rightReturn != NULL);

  if (SplaySplay(&neighbour, tree, key, tree->compare)) {
    return ResFAIL;
  } else if (neighbour == TreeEMPTY) {
    *leftReturn = *rightReturn = NULL;
  } else {
    switch(SplayCompare(tree, key, neighbour)) {

    case CompareLESS: {
      *rightReturn = neighbour;
      *leftReturn = SplayTreePredecessor(tree, key);
    } break;

    case CompareGREATER: {
      *leftReturn = neighbour;
      *rightReturn = SplayTreeSuccessor(tree, key);
    } break;

    case CompareEQUAL:
    default: {
      NOTREACHED;
    } break;
    }
  }
  return ResOK;
}


/* SplayTreeFirst, SplayTreeNext -- Iterators
 *
 * SplayTreeFirst receives a key that must precede all
 * nodes in the tree.  It returns NULL if the tree is empty.
 * Otherwise, it splays the tree to the first node, and returns the
 * new root.  See <design/splay/#function.splay.tree.first>.
 *
 * SplayTreeNext takes a tree and splays it to the successor of the
 * old root, and returns the new root.  Returns NULL is there are
 * no successors.  It takes a key for the old root.  See
 * <design/splay/#function.splay.tree.next>.
 */

Tree SplayTreeFirst(SplayTree tree, TreeKey zeroKey) {
  Tree node;
  AVERT(SplayTree, tree);

  if (SplayTreeRoot(tree) == TreeEMPTY) {
    node = NULL;
  } else if (SplaySplay(&node, tree, zeroKey, tree->compare)) {
    NOTREACHED;
  } else {
    AVER(TreeLeft(node) == TreeEMPTY);
  }

  return node;
}

Tree SplayTreeNext(SplayTree tree, Tree oldNode, TreeKey oldKey) {
  Bool b;
  Tree node;

  AVERT(SplayTree, tree);
  AVERT(Tree, oldNode);

  /* Make old node the root.  Probably already is. */
  b = SplaySplay(&node, tree, oldKey, tree->compare);
  AVER(b);
  AVER(node == oldNode);

  return SplayTreeSuccessor(tree, oldKey);
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

  if (SplaySplay(&node, tree, &closureStruct, SplayFindFirstCompare)) {
    *nodeReturn = node;
    return TRUE;
  } else {
    return FALSE;
  }
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

  if (SplaySplay(&node, tree, &closureStruct, SplayFindLastCompare)) {
    *nodeReturn = node;
    return TRUE;
  } else {
    return FALSE;
  }
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

void SplayNodeRefresh(SplayTree tree, Tree node, TreeKey key)
{
  Bool b;
  Tree node2;

  AVERT(SplayTree, tree);
  AVERT(Tree, node);

  b = SplaySplay(&node2, tree, key, tree->compare);
  AVER(b);
  AVER(node == node2);

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
