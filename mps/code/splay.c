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
#define SplayNodeLeftChild(n) RVALUE((n)->left)
#define SplayNodeSetLeftChild(n, child) \
  BEGIN ((n)->left = (child)); END
#define SplayNodeRightChild(n) RVALUE((n)->right)
#define SplayNodeSetRightChild(n, child) \
  BEGIN ((n)->right = (child)); END


#define SplayCompare(tree, key, node) \
  (((tree)->compare)((key), (node)))


Bool SplayTreeCheck(SplayTree tree)
{
  UNUSED(tree);
  CHECKL(tree != NULL);
  CHECKL(FUNCHECK(tree->compare));
  CHECKL(tree->updateNode == NULL || FUNCHECK(tree->updateNode));
  return TRUE;
}


Bool SplayNodeCheck(SplayNode node)
{
  UNUSED(node);
  CHECKL(node != NULL);
  return TRUE;
}


void SplayTreeInit(SplayTree tree, SplayCompareMethod compare,
                   SplayUpdateNodeMethod updateNode)
{
  AVER(tree != NULL);
  AVER(FUNCHECK(compare));
  AVER(updateNode == NULL || FUNCHECK(updateNode));

  tree->compare = compare;
  tree->updateNode = updateNode;
  SplayTreeSetRoot(tree, NULL);

  AVERT(SplayTree, tree);
}


void SplayNodeInit(SplayNode node)
{
  AVER(node != NULL);

  /* We don't try to finish the attached nodes.  See .note.stack.  */
  SplayNodeSetLeftChild(node, NULL);
  SplayNodeSetRightChild(node, NULL);

  AVERT(SplayNode, node);
}


void SplayNodeFinish(SplayNode node)
{
  AVERT(SplayNode, node);

  /* we don't try to do a recursive finish.  See .note.stack. */
  SplayNodeSetLeftChild(node, NULL);
  SplayNodeSetRightChild(node, NULL);
}


void SplayTreeFinish(SplayTree tree)
{
  AVERT(SplayTree, tree);
  SplayTreeSetRoot(tree, NULL);
  tree->compare = NULL;
}


static void SplayNodeUpdate(SplayTree tree, SplayNode node)
{
  AVERT(SplayTree, tree);
  AVERT(SplayNode, node);
  AVER(tree->updateNode != NULL);

  (*tree->updateNode)(tree, node, SplayNodeLeftChild(node),
                      SplayNodeRightChild(node));
  return;
}


/* SplayLinkRight -- Move top to left child of top
 *
 * Link the current top node into the left child of the right tree,
 * leaving the top node as the left child of the old top node.
 *
 * See <design/splay/#impl.link.right>.
 */

static void SplayLinkRight(SplayNode *topIO, SplayNode *rightIO)
{
  AVERT(SplayNode, *topIO);
  AVERT(SplayNode, *rightIO);

  /* Don't fix client properties yet. */

  /* .link.right.first: *rightIO is always the first node in the */
  /* right tree, so its left child must be null. */
  AVER(SplayNodeLeftChild(*rightIO) == NULL);

  SplayNodeSetLeftChild(*rightIO, *topIO);
  *rightIO = *topIO;
  *topIO = SplayNodeLeftChild(*topIO);

  /* The following line is only required for .link.right.first. */
  SplayNodeSetLeftChild(*rightIO, NULL);
}


/* SplayLinkLeft -- Move top to right child of top
 *
 * Link the current top node into the right child of the left tree,
 * leaving the top node as the right child of the old top node.
 *
 * See <design/splay/#impl.link.left>.
 */

static void SplayLinkLeft(SplayNode *topIO, SplayNode *leftIO) {
  AVERT(SplayNode, *topIO);
  AVERT(SplayNode, *leftIO);

  /* Don't fix client properties yet. */

  /* .link.left.first: *leftIO is always the last node in the */
  /* left tree, so its right child must be null. */
  AVER(SplayNodeRightChild(*leftIO) == NULL);

  SplayNodeSetRightChild(*leftIO, *topIO);
  *leftIO = *topIO;
  *topIO = SplayNodeRightChild(*topIO);

  /* The following line is only required for .link.left.first. */
  SplayNodeSetRightChild(*leftIO, NULL);
}


/* SplayRotateLeft -- Rotate right child edge of node
 *
 * Rotates node, right child of node, and left child of right
 * child of node, leftwards in the order stated.
 *
 * See <design/splay/#impl.rotate.left>.
 */

static void SplayRotateLeft(SplayNode *nodeIO, SplayTree tree) {
  SplayNode nodeRight;

  AVER(nodeIO != NULL);
  AVERT(SplayNode, *nodeIO);
  AVERT(SplayNode, SplayNodeRightChild(*nodeIO));
  AVERT(SplayTree, tree);

  nodeRight = SplayNodeRightChild(*nodeIO);
  SplayNodeSetRightChild(*nodeIO, SplayNodeLeftChild(nodeRight));
  SplayNodeSetLeftChild(nodeRight, *nodeIO);
  *nodeIO = nodeRight;

  if (tree->updateNode != NULL) {
    SplayNodeUpdate(tree, SplayNodeLeftChild(nodeRight));
    /* Don't need to update new root because we know that we will */
    /* do either a link or an assemble next, and that will sort it */
    /* out. */
  }

  return;
}


/* SplayRotateRight -- Rotate left child edge of node
 *
 * Rotates node, left child of node, and right child of left
 * child of node, leftwards in the order stated.
 *
 * See <design/splay/#impl.rotate.right>.
 */

static void SplayRotateRight(SplayNode *nodeIO, SplayTree tree) {
  SplayNode nodeLeft;

  AVER(nodeIO != NULL);
  AVERT(SplayNode, *nodeIO);
  AVERT(SplayNode, SplayNodeLeftChild(*nodeIO));
  AVERT(SplayTree, tree);

  nodeLeft = SplayNodeLeftChild(*nodeIO);
  SplayNodeSetLeftChild(*nodeIO, SplayNodeRightChild(nodeLeft));
  SplayNodeSetRightChild(nodeLeft, *nodeIO);
  *nodeIO = nodeLeft;

  if (tree->updateNode != NULL) {
    SplayNodeUpdate(tree, SplayNodeRightChild(nodeLeft));
    /* Don't need to update new root because we know that we will */
    /* do either a link or an assemble next, and that will sort it */
    /* out. */
  }

  return;
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

static void SplayAssemble(SplayTree tree, SplayNode top,
                          SplayNode leftTop, SplayNode leftLast,
                          SplayNode rightTop, SplayNode rightFirst) {
  AVERT(SplayTree, tree);
  AVERT(SplayNode, top);
  AVER(leftTop == NULL ||
       (SplayNodeCheck(leftTop) && SplayNodeCheck(leftLast)));
  AVER(rightTop == NULL ||
       (SplayNodeCheck(rightTop) && SplayNodeCheck(rightFirst)));

  if (leftTop != NULL) {
    SplayNodeSetRightChild(leftLast, SplayNodeLeftChild(top));
    SplayNodeSetLeftChild(top, leftTop);

    if (tree->updateNode != NULL) {
      /* Update client property using pointer reversal (Ugh!). */
      SplayNode node, parent, rightChild;

      /* Reverse the pointers between leftTop and leftLast */
      /* leftLast is not reversed. */
      node = leftTop;
      parent = NULL;
      while(node != leftLast) {
        rightChild = SplayNodeRightChild(node);
        SplayNodeSetRightChild(node, parent); /* pointer reversal */
        parent = node;
        node = rightChild;
       }

      /* Now restore the pointers, updating the client property. */
      /* node is leftLast, parent is the last parent (or NULL). */
      SplayNodeUpdate(tree, node);
      while(node != leftTop) {
        rightChild = node;
        node = parent;
        parent = SplayNodeRightChild(node);
        SplayNodeSetRightChild(node, rightChild); /* un-reverse pointer */
        SplayNodeUpdate(tree, node);
      }
    }
  }
  /* otherwise leave top->left alone */

  if (rightTop != NULL) {
    SplayNodeSetLeftChild(rightFirst, SplayNodeRightChild(top));
    SplayNodeSetRightChild(top, rightTop);

    if (tree->updateNode != NULL) {
      /* Update client property using pointer reversal (Ugh!). */
      SplayNode node, parent, leftChild;

      /* Reverse the pointers between rightTop and rightFirst */
      /* ightFirst is not reversed. */
      node = rightTop;
      parent = NULL;
      while(node != rightFirst) {
        leftChild = SplayNodeLeftChild(node);
        SplayNodeSetLeftChild(node, parent); /* pointer reversal */
        parent = node;
        node = leftChild;
       }

      /* Now restore the pointers, updating the client property. */
      /* node is rightFirst, parent is the last parent (or NULL). */
      SplayNodeUpdate(tree, node);
      while(node != rightTop) {
        leftChild = node;
        node = parent;
        parent = SplayNodeLeftChild(node);
        SplayNodeSetLeftChild(node, leftChild); /* un-reverse pointer */
        SplayNodeUpdate(tree, node);
      }
    }
  }
  /* otherwise leave top->right alone */

  if (tree->updateNode != NULL)
    SplayNodeUpdate(tree, top);
}


/* SplaySplay -- Splay the tree (top-down) around a given key
 *
 * If the key is not found, splays around an arbitrary neighbour.
 * Returns whether key was found.  This is the real logic behind
 * splay trees.
 *
 * See <design/splay/#impl.splay>.
 */

static Bool SplaySplay(SplayNode *nodeReturn, SplayTree tree,
                       void *key, SplayCompareMethod compareMethod) {
  /* The sides structure avoids a boundary case in SplayLink* */
  SplayNodeStruct sides; /* rightTop and leftTop */
  SplayNode top, leftLast, rightFirst;
  Bool found;
  Compare compareTop;

  AVERT(SplayTree, tree);
  AVER(nodeReturn != NULL);
  AVER(FUNCHECK(compareMethod));

  top = SplayTreeRoot(tree); /* will be copied back at end */

  if (top == NULL) {
    *nodeReturn = NULL;
    return FALSE;
  }

  /* short-circuit case where node is already top */
  compareTop = (*compareMethod)(key, top);
  if (compareTop == CompareEQUAL) {
    *nodeReturn = top;
    return TRUE;
  }

  SplayNodeInit(&sides); /* left and right trees now NULL */
  leftLast = &sides;
  rightFirst = &sides;

  while(TRUE) {
    /* compareTop is already initialised above. */
    switch(compareTop) {

    case CompareLESS: {
      SplayNode topLeft = SplayNodeLeftChild(top);
      if (topLeft == NULL) {
        found = FALSE;
        goto assemble;
      } else {
        Compare compareTopLeft = (*compareMethod)(key, topLeft);

        switch(compareTopLeft) {

        case CompareEQUAL: {                 /* zig */
          SplayLinkRight(&top, &rightFirst);
          found = TRUE;
          goto assemble;
        } /* break; */

        case CompareLESS: {                  /* zig-zig */
          if (SplayNodeLeftChild(topLeft) == NULL)
            goto terminalZig;
          SplayRotateRight(&top, tree);
          SplayLinkRight(&top, &rightFirst);
        } break;

        case CompareGREATER: {               /* zig-zag */
          if (SplayNodeRightChild(topLeft) == NULL)
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
      SplayNode topRight = SplayNodeRightChild(top);
      if (topRight == NULL) {
        found = FALSE;
        goto assemble;
      } else {
        Compare compareTopRight = (*compareMethod)(key, topRight);

        switch(compareTopRight) {

        case CompareEQUAL: {                 /* zag */
            SplayLinkLeft(&top, &leftLast);
            found = TRUE;
            goto assemble;
        } /* break; */

        case CompareGREATER: {               /* zag-zag */
          if (SplayNodeRightChild(topRight) == NULL)
            goto terminalZag;
          SplayRotateLeft(&top, tree);
          SplayLinkLeft(&top, &leftLast);
        } break;

        case CompareLESS: {                  /* zag-zig */
          if (SplayNodeLeftChild(topRight) == NULL)
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
    compareTop = (*compareMethod)(key, top);
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
                SplayNodeRightChild(&sides), leftLast,
                SplayNodeLeftChild(&sides), rightFirst);

  SplayTreeSetRoot(tree, top);
  *nodeReturn = top;

  return found;
}


/* SplayTreeInsert -- Insert a node into a splay tree
 *
 * See <design/splay/#function.splay.tree.insert> and
 * <design/splay/#impl.insert>.
 */

Res SplayTreeInsert(SplayTree tree, SplayNode node, void *key) {
  SplayNode neighbour;

  AVERT(SplayTree, tree);
  AVERT(SplayNode, node);
  AVER(SplayNodeLeftChild(node) == NULL);
  AVER(SplayNodeRightChild(node) == NULL);

  if (SplayTreeRoot(tree) == NULL) {
    SplayTreeSetRoot(tree, node);
  } else if (SplaySplay(&neighbour, tree, key, tree->compare)) {
    return ResFAIL;
  } else {
    AVER(SplayTreeRoot(tree) == neighbour);
    switch(SplayCompare(tree, key, neighbour)) {

    case CompareGREATER: { /* left neighbour */
      SplayTreeSetRoot(tree, node);
      SplayNodeSetRightChild(node, SplayNodeRightChild(neighbour));
      SplayNodeSetLeftChild(node, neighbour);
      SplayNodeSetRightChild(neighbour, NULL);
    } break;

    case CompareLESS: { /* right neighbour */
      SplayTreeSetRoot(tree, node);
      SplayNodeSetLeftChild(node, SplayNodeLeftChild(neighbour));
      SplayNodeSetRightChild(node, neighbour);
      SplayNodeSetLeftChild(neighbour, NULL);
    } break;

    case CompareEQUAL:
    default: {
      NOTREACHED;
    } break;
    }

    if (tree->updateNode != NULL) {
      SplayNodeUpdate(tree, neighbour);
      SplayNodeUpdate(tree, node);
    }
  }

  return ResOK;
}


/* SplayTreeDelete -- Delete a node from a splay tree
 *
 * See <design/splay/#function.splay.tree.delete> and
 * <design/splay/#impl.delete>.
 */

Res SplayTreeDelete(SplayTree tree, SplayNode node, void *key) {
  SplayNode rightHalf, del, leftLast;
  Bool found;

  AVERT(SplayTree, tree);
  AVERT(SplayNode, node);

  found = SplaySplay(&del, tree, key, tree->compare);
  AVER(!found || del == node);

  if (!found) {
    return ResFAIL;
  } else if (SplayNodeLeftChild(node) == NULL) {
    SplayTreeSetRoot(tree, SplayNodeRightChild(node));
  } else if (SplayNodeRightChild(node) == NULL) {
    SplayTreeSetRoot(tree, SplayNodeLeftChild(node));
  } else {
    rightHalf = SplayNodeRightChild(node);
    SplayTreeSetRoot(tree, SplayNodeLeftChild(node));
    if (SplaySplay(&leftLast, tree, key, tree->compare)) {
      return ResFAIL;
    } else {
      AVER(SplayNodeRightChild(leftLast) == NULL);
      SplayNodeSetRightChild(leftLast, rightHalf);
      if (tree->updateNode != NULL) {
        SplayNodeUpdate(tree, leftLast);
      }
    }
  }

  SplayNodeFinish(node);

  return ResOK;
}


/* SplayTreeSearch -- Search for a node in a splay tree matching a key
 *
 * See <design/splay/#function.splay.tree.search> and
 * <design/splay/#impl.search>.
 */

Res SplayTreeSearch(SplayNode *nodeReturn, SplayTree tree, void *key) {
  SplayNode node;

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

static SplayNode SplayTreePredecessor(SplayTree tree, void *key) {
  SplayNode oldRoot, newRoot;

  AVERT(SplayTree, tree);

  oldRoot = SplayTreeRoot(tree);
  AVERT(SplayNode, oldRoot);

  if (SplayNodeLeftChild(oldRoot) == NULL) {
    newRoot = NULL; /* No predecessor */
  } else {
    /* temporarily chop off the right half-tree, inclusive of root */
    SplayTreeSetRoot(tree, SplayNodeLeftChild(oldRoot));
    SplayNodeSetLeftChild(oldRoot, NULL);
    if (SplaySplay(&newRoot, tree, key, tree->compare)) {
      NOTREACHED; /* Another matching node found */
    } else {
      AVER(SplayNodeRightChild(newRoot) == NULL);
      SplayNodeSetRightChild(newRoot, oldRoot);
    }

    if (tree->updateNode != NULL) {
      SplayNodeUpdate(tree, oldRoot);
      SplayNodeUpdate(tree, newRoot);
    }
  }

  return newRoot;
}


/* SplayTreeSuccessor -- Splays a tree at the root's successor
 *
 * Must not be called on en empty tree.  Successor need not exist,
 * in which case NULL is returned, and the tree is unchanged.
 */

static SplayNode SplayTreeSuccessor(SplayTree tree, void *key) {
  SplayNode oldRoot, newRoot;

  AVERT(SplayTree, tree);

  oldRoot = SplayTreeRoot(tree);
  AVERT(SplayNode, oldRoot);

  if (SplayNodeRightChild(oldRoot) == NULL) {
    newRoot = NULL; /* No successor */
  } else {
    /* temporarily chop off the left half-tree, inclusive of root */
    SplayTreeSetRoot(tree, SplayNodeRightChild(oldRoot));
    SplayNodeSetRightChild(oldRoot, NULL);
    if (SplaySplay(&newRoot, tree, key, tree->compare)) {
      NOTREACHED; /* Another matching node found */
    } else {
      AVER(SplayNodeLeftChild(newRoot) == NULL);
      SplayNodeSetLeftChild(newRoot, oldRoot);
    }

    if (tree->updateNode != NULL) {
      SplayNodeUpdate(tree, oldRoot);
      SplayNodeUpdate(tree, newRoot);
    }
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


Res SplayTreeNeighbours(SplayNode *leftReturn, SplayNode *rightReturn,
                        SplayTree tree, void *key) {
  SplayNode neighbour;

  AVERT(SplayTree, tree);
  AVER(leftReturn != NULL);
  AVER(rightReturn != NULL);

  if (SplaySplay(&neighbour, tree, key, tree->compare)) {
    return ResFAIL;
  } else if (neighbour == NULL) {
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

SplayNode SplayTreeFirst(SplayTree tree, void *zeroKey) {
  SplayNode node;
  AVERT(SplayTree, tree);

  if (SplayTreeRoot(tree) == NULL) {
    node = NULL;
  } else if (SplaySplay(&node, tree, zeroKey, tree->compare)) {
    NOTREACHED;
  } else {
    AVER(SplayNodeLeftChild(node) == NULL);
  }

  return node;
}

SplayNode SplayTreeNext(SplayTree tree, SplayNode oldNode, void *oldKey) {
  Bool b;
  SplayNode node;

  AVERT(SplayTree, tree);
  AVERT(SplayNode, oldNode);

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

static Res SplayNodeDescribe(SplayNode node, mps_lib_FILE *stream,
                             SplayNodeDescribeMethod nodeDescribe) {
  Res res;

#if defined(AVER_AND_CHECK)
  if (!SplayNodeCheck(node)) return ResFAIL;
  /* stream and nodeDescribe checked by SplayTreeDescribe */
#endif

  res = WriteF(stream, "( ", NULL);
  if (res != ResOK) return res;

  if (SplayNodeLeftChild(node) != NULL) {
    res = SplayNodeDescribe(SplayNodeLeftChild(node), stream, nodeDescribe);
    if (res != ResOK) return res;

    res = WriteF(stream, " / ", NULL);
    if (res != ResOK) return res;
  }

  res = (*nodeDescribe)(node, stream);
  if (res != ResOK) return res;

  if (SplayNodeRightChild(node) != NULL) {
    res = WriteF(stream, " \\ ", NULL);
    if (res != ResOK) return res;

    res = SplayNodeDescribe(SplayNodeRightChild(node), stream, nodeDescribe);
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
  Bool found;
} SplayFindClosureStruct, *SplayFindClosure;

static Compare SplayFindFirstCompare(void *key, SplayNode node)
{
  SplayFindClosure closure;
  void *closureP;
  Size closureS;
  SplayTestNodeMethod testNode;
  SplayTestTreeMethod testTree;
  SplayTree tree;

  AVERT(SplayNode, node);
  AVER(key != NULL);

  closure = (SplayFindClosure)key;
  closureP = closure->p;
  closureS = closure->s;
  testNode = closure->testNode;
  testTree = closure->testTree;
  tree = closure->tree;
  
  if (SplayNodeLeftChild(node) != NULL &&
     (*testTree)(tree, SplayNodeLeftChild(node), closureP, closureS)) {
    return CompareLESS;
  } else if ((*testNode)(tree, node, closureP, closureS)) {
    closure->found = TRUE;
    return CompareEQUAL;
  } else {
    /* If there's a right subtree but it doesn't satisfy the tree test
       then we want to terminate the splay right now.  SplaySplay will
       return TRUE, so the caller must check closure->found to find out
       whether the result node actually satisfies testNode. */
    if (SplayNodeRightChild(node) != NULL &&
        !(*testTree)(tree, SplayNodeRightChild(node), closureP, closureS)) {
      closure->found = FALSE;
      return CompareEQUAL;
    }
    return CompareGREATER;
  }
}

static Compare SplayFindLastCompare(void *key, SplayNode node)
{
  SplayFindClosure closure;
  void *closureP;
  Size closureS;
  SplayTestNodeMethod testNode;
  SplayTestTreeMethod testTree;
  SplayTree tree;

  AVERT(SplayNode, node);
  AVER(key != NULL);

  closure = (SplayFindClosure)key;
  closureP = closure->p;
  closureS = closure->s;
  testNode = closure->testNode;
  testTree = closure->testTree;
  tree = closure->tree;

  if (SplayNodeRightChild(node) != NULL &&
     (*testTree)(tree, SplayNodeRightChild(node), closureP, closureS)) {
     return CompareGREATER;
  } else if ((*testNode)(tree, node, closureP, closureS)) {
    closure->found = TRUE;
    return CompareEQUAL;
  } else {
    /* See SplayFindFirstCompare. */
    if (SplayNodeLeftChild(node) != NULL &&
        !(*testTree)(tree, SplayNodeLeftChild(node), closureP, closureS)) {
      closure->found = FALSE;
      return CompareEQUAL;
    }
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
 *
 * TODO: This repeatedly splays failed matches to the root and rotates
 * them, so it could have quite an unbalancing effect if size is small.
 * Think about a better search, perhaps using TreeTraverse?
 */

Bool SplayFindFirst(SplayNode *nodeReturn, SplayTree tree,
                    SplayTestNodeMethod testNode,
                    SplayTestTreeMethod testTree,
                    void *closureP, Size closureS)
{
  SplayNode node;
  SplayFindClosureStruct closureStruct;
  Bool found;

  AVER(nodeReturn != NULL);
  AVERT(SplayTree, tree);
  AVER(FUNCHECK(testNode));
  AVER(FUNCHECK(testTree));

  node = SplayTreeRoot(tree);

  if (node == NULL || !(*testTree)(tree, node, closureP, closureS))
    return FALSE; /* no suitable nodes in tree */

  closureStruct.p = closureP;
  closureStruct.s = closureS;
  closureStruct.testNode = testNode;
  closureStruct.testTree = testTree;
  closureStruct.tree = tree;
  closureStruct.found = FALSE;

  found = SplaySplay(&node, tree, (void *)&closureStruct,
                     &SplayFindFirstCompare) && closureStruct.found;

  while (!found) {
    SplayNode oldRoot, newRoot;
    
    /* FIXME: Rename to "seen" and "not yet seen" or something. */
    oldRoot = SplayTreeRoot(tree);
    newRoot = SplayNodeRightChild(oldRoot);

    if (newRoot == NULL || !(*testTree)(tree, newRoot, closureP, closureS))
      return FALSE; /* no suitable nodes in the rest of the tree */
  
    /* Temporarily chop off the left half-tree, inclusive of root,
       so that the search excludes any nodes we've seen already. */
    SplayTreeSetRoot(tree, newRoot);
    SplayNodeSetRightChild(oldRoot, NULL);

    found = SplaySplay(&node, tree, (void *)&closureStruct,
                       &SplayFindFirstCompare) && closureStruct.found;

    /* Restore the left tree, then rotate left so that the node we
       just splayed is at the root.  Update both. */
    newRoot = SplayTreeRoot(tree);
    SplayNodeSetRightChild(oldRoot, newRoot);
    SplayTreeSetRoot(tree, oldRoot);
    SplayRotateLeft(&tree->root, tree);
    if (tree->updateNode != NULL) {
      SplayNodeUpdate(tree, oldRoot);
      SplayNodeUpdate(tree, newRoot);
    }
  }

  *nodeReturn = node;
  return TRUE;
}


/* SplayFindLast -- As SplayFindFirst but in reverse address order */

Bool SplayFindLast(SplayNode *nodeReturn, SplayTree tree,
                          SplayTestNodeMethod testNode,
                          SplayTestTreeMethod testTree,
                          void *closureP, Size closureS)
{
  SplayNode node;
  SplayFindClosureStruct closureStruct;
  Bool found;

  AVER(nodeReturn != NULL);
  AVERT(SplayTree, tree);
  AVER(FUNCHECK(testNode));
  AVER(FUNCHECK(testTree));

  node = SplayTreeRoot(tree);

  if (node == NULL || !(*testTree)(tree, node, closureP, closureS))
    return FALSE; /* no suitable nodes in tree */

  closureStruct.p = closureP;
  closureStruct.s = closureS;
  closureStruct.testNode = testNode;
  closureStruct.testTree = testTree;
  closureStruct.tree = tree;

  found = SplaySplay(&node, tree, (void *)&closureStruct,
                     &SplayFindLastCompare) && closureStruct.found;

  while (!found) {
    SplayNode oldRoot, newRoot;
    
    oldRoot = SplayTreeRoot(tree);
    newRoot = SplayNodeLeftChild(oldRoot);

    if (newRoot == NULL || !(*testTree)(tree, newRoot, closureP, closureS))
      return FALSE; /* no suitable nodes in the rest of the tree */
  
    /* Temporarily chop off the right half-tree, inclusive of root,
       so that the search excludes any nodes we've seen already. */
    SplayTreeSetRoot(tree, newRoot);
    SplayNodeSetLeftChild(oldRoot, NULL);

    found = SplaySplay(&node, tree, (void *)&closureStruct,
                       &SplayFindLastCompare) && closureStruct.found;

    /* Restore the right tree, then rotate right so that the node we
       just splayed is at the root.  Update both. */
    newRoot = SplayTreeRoot(tree);
    SplayNodeSetLeftChild(oldRoot, newRoot);
    SplayTreeSetRoot(tree, oldRoot);
    SplayRotateRight(&tree->root, tree);
    if (tree->updateNode != NULL) {
      SplayNodeUpdate(tree, oldRoot);
      SplayNodeUpdate(tree, newRoot);
    }
  }

  *nodeReturn = node;
  return TRUE;
}


/* SplayRoot -- return the root node of the tree */

Bool SplayRoot(SplayNode *nodeReturn, SplayTree tree)
{
  SplayNode node;

  AVER(nodeReturn != NULL);
  AVERT(SplayTree, tree);

  node = SplayTreeRoot(tree);
  if (node == NULL)
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

void SplayNodeRefresh(SplayTree tree, SplayNode node, void *key)
{
  Bool b;
  SplayNode node2;

  AVERT(SplayTree, tree);
  AVERT(SplayNode, node);

  b = SplaySplay(&node2, tree, key, tree->compare);
  AVER(b);
  AVER(node == node2);

  (*tree->updateNode)(tree, node, SplayNodeLeftChild(node),
                      SplayNodeRightChild(node));
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

  if (SplayTreeRoot(tree) != NULL) {
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
