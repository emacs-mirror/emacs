/* impl.c.splay: SPLAY TREE IMPLEMENTATION
 *
 * $HopeName: MMsrc!splay.c(trunk.2) $
 * Copyright (C) 1998 Harlequin Group plc, all rights reserved.
 *
 * .readership: Any MPS developer.
 *
 * .intro: This is a portable implementation of splay trees.
 *
 * .purpose: Splay trees are used to manage potentially unbounded 
 * collections of ordered things.
 *
 * .sources: design.mps.splay,
 *
 * .note.stack: It's important that the MPS have a bounded stack 
 * size, and this is a problem for tree algoriths.  Basically,
 * we have to avoid recursion.
 */


#include "mpm.h"


SRCID(splay, "$HopeName: MMsrc!splay.c(trunk.2) $");

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

Bool SplayTreeCheck(SplayTree tree) {
  UNUSED(tree);
  CHECKL(tree != NULL);
  CHECKL(FUNCHECK(tree->compare));
  return TRUE;
}

Bool SplayNodeCheck(SplayNode node) {
  UNUSED(node);
  CHECKL(node != NULL);
  return TRUE;
}

void SplayTreeInit(SplayTree tree, SplayCompareMethod compare) {
  AVER(tree != NULL);
  AVER(FUNCHECK(compare));

  tree->compare = compare;
  SplayTreeSetRoot(tree, NULL);

  AVERT(SplayTree, tree);
}

void SplayNodeInit(SplayNode node) {
  AVER(node != NULL);

  /* We don't try to finish the attached nodes.  See .note.stack.  */
  SplayNodeSetLeftChild(node, NULL);
  SplayNodeSetRightChild(node, NULL);

  AVERT(SplayNode, node);
}

void SplayNodeFinish(SplayNode node) {
  AVERT(SplayNode, node);

  /* we don't try to do a recursive finish.  See .note.stack. */
  SplayNodeSetLeftChild(node, NULL);
  SplayNodeSetRightChild(node, NULL);
}

void SplayTreeFinish(SplayTree tree) {
  AVERT(SplayTree, tree);
  SplayTreeSetRoot(tree, NULL);
  tree->compare = NULL;
}

/* SplayLinkRight -- Move top to left child of top
 *
 * Link the current top node into the left child of the right tree,
 * leaving the top node as the left child of the old top node.
 *
 * See design.mps.splay.impl.link.right.
 */

static void SplayLinkRight(SplayNode *topIO, SplayNode *rightIO) {
  AVERT(SplayNode, *topIO);
  AVERT(SplayNode, *rightIO);

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
 * See design.mps.splay.impl.link.left.
 */

static void SplayLinkLeft(SplayNode *topIO, SplayNode *leftIO) {
  AVERT(SplayNode, *topIO);
  AVERT(SplayNode, *leftIO);

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
 * See design.mps.splay.impl.rotate.left.
 */

static void SplayRotateLeft(SplayNode *nodeIO) {
  SplayNode nodeRight;

  AVERT(SplayNode, *nodeIO);
  AVERT(SplayNode, SplayNodeRightChild(*nodeIO));

  nodeRight = SplayNodeRightChild(*nodeIO);
  SplayNodeSetRightChild(*nodeIO, SplayNodeLeftChild(nodeRight));
  SplayNodeSetLeftChild(nodeRight, *nodeIO);
  *nodeIO = nodeRight;
}

/* SplayRotateRight -- Rotate left child edge of node
 *
 * Rotates node, left child of node, and right child of left
 * child of node, leftwards in the order stated.
 *
 * See design.mps.splay.impl.rotate.right.
 */

static void SplayRotateRight(SplayNode *nodeIO) {
  SplayNode nodeLeft;

  AVERT(SplayNode, *nodeIO);
  AVERT(SplayNode, SplayNodeLeftChild(*nodeIO));

  nodeLeft = SplayNodeLeftChild(*nodeIO);
  SplayNodeSetLeftChild(*nodeIO, SplayNodeRightChild(nodeLeft));
  SplayNodeSetRightChild(nodeLeft, *nodeIO);
  *nodeIO = nodeLeft;
}

/* SplayAssemble -- Assemble left right and top trees into one
 *
 * We do this by moving the children of the top tree to the last and 
 * first nodes in the left and right trees, and then moving the tops
 * of the left and right trees to the children of the top tree.
 * 
 * See design.mps.splay.impl.assemble.
 */

static void SplayAssemble(SplayNode top, 
		          SplayNode leftTop, SplayNode leftLast,
			  SplayNode rightTop, SplayNode rightFirst) {
  AVERT(SplayNode, top);
  AVER(leftTop == NULL || 
       (SplayNodeCheck(leftTop) && SplayNodeCheck(leftLast)));
  AVER(rightTop == NULL || 
       (SplayNodeCheck(rightTop) && SplayNodeCheck(rightFirst)));
 
  if(leftTop != NULL) {
    SplayNodeSetRightChild(leftLast, SplayNodeLeftChild(top));
    SplayNodeSetLeftChild(top, leftTop);
  }
  /* otherwise leave top->left alone */

  if(rightTop != NULL) {
    SplayNodeSetLeftChild(rightFirst, SplayNodeRightChild(top));
    SplayNodeSetRightChild(top, rightTop);
  }
  /* otherwise leave top->right alone */
}

/* SplaySplay -- Splay the tree (top-down) around a given key
 *
 * If the key is not found, splays around an arbitrary neighbour.
 * Returns whether key was found.  This is the real logic behind
 * splay trees.
 *
 * See design.mps.splay.impl.splay.
 */

static Bool SplaySplay(SplayNode *nodeReturn, SplayTree tree, void *key) {
  /* The sides structure avoids a boundary case in SplayLink* */
  SplayNodeStruct sides; /* rightTop and leftTop */
  SplayNode top, leftLast, rightFirst;  
  Bool found;
  Compare compareTop;

  AVERT(SplayTree, tree);
  AVER(nodeReturn != NULL);

  if(SplayTreeRoot(tree) == NULL) {
    *nodeReturn = NULL;
    return FALSE;
  }

  SplayNodeInit(&sides); /* left and right trees now NULL */
  top = SplayTreeRoot(tree); /* will be copied back at end */
  leftLast = &sides;
  rightFirst = &sides;

  while(TRUE) {
    compareTop = SplayCompare(tree, key, top);
    switch(compareTop) {

    case CompareLESS: {
      SplayNode topLeft = SplayNodeLeftChild(top);
      if(topLeft == NULL) {
	found = FALSE;
	goto assemble;
      } else {
	Compare compareTopLeft = SplayCompare(tree, key, topLeft);

	switch(compareTopLeft) {

	case CompareEQUAL: {                 /* zig */
	  SplayLinkRight(&top, &rightFirst);
	  found = TRUE;
	  goto assemble;
        } /* break; */

	case CompareLESS: {                  /* zig-zig */
	  if(SplayNodeLeftChild(topLeft) == NULL)
	    goto terminalZig;
          SplayRotateRight(&top);
	  SplayLinkRight(&top, &rightFirst);
        } break;

	case CompareGREATER: {               /* zig-zag */
	  if(SplayNodeRightChild(topLeft) == NULL)
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
      if(topRight == NULL) {
	found = FALSE;
	goto assemble;
      } else {
	Compare compareTopRight = SplayCompare(tree, key, topRight);

	switch(compareTopRight) {

	case CompareEQUAL: {                 /* zag */
	  SplayLinkLeft(&top, &leftLast);
	  found = TRUE;
	  goto assemble;
        } /* break; */

	case CompareGREATER: {               /* zag-zag */
	  if(SplayNodeRightChild(topRight) == NULL)
	    goto terminalZag;
          SplayRotateLeft(&top);
	  SplayLinkLeft(&top, &leftLast);
        } break;

	case CompareLESS: {                  /* zag-zig */
	  if(SplayNodeLeftChild(topRight) == NULL)
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
  }; /* end while(TRUE) */
      
terminalZig:
  SplayLinkRight(&top, &rightFirst);
  found = FALSE;
  goto assemble;

terminalZag:
  SplayLinkLeft(&top, &leftLast);
  found = FALSE;
  goto assemble;

assemble:
  SplayAssemble(top, 
		SplayNodeRightChild(&sides), leftLast,
		SplayNodeLeftChild(&sides), rightFirst);

  SplayTreeSetRoot(tree, top);
  *nodeReturn = top;

  return found;
}


/* SplayTreeInsert -- Insert a node into a splay tree
 *
 * See design.mps.splay.function.splay.tree.insert and
 * design.mps.splay.impl.insert.
 */

Res SplayTreeInsert(SplayTree tree, SplayNode node, void *key) {
  SplayNode neighbour;

  AVERT(SplayTree, tree);
  AVERT(SplayNode, node);
  AVER(SplayNodeLeftChild(node) == NULL);
  AVER(SplayNodeRightChild(node) == NULL);

  if(SplayTreeRoot(tree) == NULL) {
    SplayTreeSetRoot(tree, node);
  } else if(SplaySplay(&neighbour, tree, key)) {
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
  }

  return ResOK;
}


/* SplayTreeDelete -- Delete a node from a splay tree
 *
 * See design.mps.splay.function.splay.tree.delete and
 * design.mps.splay.impl.delete.
 */

Res SplayTreeDelete(SplayTree tree, SplayNode node, void *key) {
  SplayNode rightHalf, del, leftLast;
  Bool found;

  AVERT(SplayTree, tree);
  AVERT(SplayNode, node);

  found = SplaySplay(&del, tree, key);
  AVER(!found || del == node);

  if(!found) {
    return ResFAIL;
  } else if(SplayNodeLeftChild(node) == NULL) {
    SplayTreeSetRoot(tree, SplayNodeRightChild(node));
  } else if(SplayNodeRightChild(node) == NULL) {
    SplayTreeSetRoot(tree, SplayNodeLeftChild(node));
  } else {
    rightHalf = SplayNodeRightChild(node);
    SplayTreeSetRoot(tree, SplayNodeLeftChild(node));
    if(SplaySplay(&leftLast, tree, key)) {
      return ResFAIL;
    } else {
      AVER(SplayNodeRightChild(leftLast) == NULL);
      SplayNodeSetRightChild(leftLast, rightHalf);
    }
  }

  SplayNodeFinish(node);

  return ResOK;
}


/* SplayTreeSearch -- Search for a node in a splay tree matching a key
 *
 * See design.mps.splay.function.splay.tree.search and
 * design.mps.splay.impl.search.
 */


Res SplayTreeSearch(SplayNode *nodeReturn, SplayTree tree, void *key) {
  SplayNode node;

  AVERT(SplayTree, tree);
  AVER(nodeReturn != NULL);

  if(SplaySplay(&node, tree, key)) {
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

  if(SplayNodeLeftChild(oldRoot) == NULL) {
    newRoot = NULL; /* No predecessor */
  } else {
    /* temporarily chop off the right half-tree, inclusive of root */
    SplayTreeSetRoot(tree, SplayNodeLeftChild(oldRoot));
    SplayNodeSetLeftChild(oldRoot, NULL);
    if(SplaySplay(&newRoot, tree, key)) {
      NOTREACHED; /* Another matching node found */
    } else {
      AVER(SplayNodeRightChild(newRoot) == NULL);
      SplayNodeSetRightChild(newRoot, oldRoot);
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

  if(SplayNodeRightChild(oldRoot) == NULL) {
    newRoot = NULL; /* No successor */
  } else {
    /* temporarily chop off the left half-tree, inclusive of root */
    SplayTreeSetRoot(tree, SplayNodeRightChild(oldRoot));
    SplayNodeSetRightChild(oldRoot, NULL);
    if(SplaySplay(&newRoot, tree, key)) {
      NOTREACHED; /* Another matching node found */
    } else {
      AVER(SplayNodeLeftChild(newRoot) == NULL);
      SplayNodeSetLeftChild(newRoot, oldRoot);
    }
  }

  return newRoot;
}


/* SplayTreeNeighbours
 *
 * Search for the two nodes in a splay tree neighbouring a key.
 *
 * See design.mps.splay.function.splay.tree.neighbours and
 * design.mps.splay.impl.neighbours.
 */


Res SplayTreeNeighbours(SplayNode *leftReturn, SplayNode *rightReturn,
                        SplayTree tree, void *key) {
  SplayNode neighbour;

  AVERT(SplayTree, tree);
  AVER(leftReturn != NULL);
  AVER(rightReturn != NULL);

  if(SplaySplay(&neighbour, tree, key)) {
    return ResFAIL;
  } else if(neighbour == NULL) {
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
 * new root.  See design.mps.splay.function.splay.tree.first.
 *
 * SplayTreeNext takes a tree and splays it to the successor of the 
 * old root, and returns the new root.  Returns NULL is there are 
 * no successors.  It takes a key for the old root.  See 
 * design.mps.splay.function.splay.tree.next.
 */

SplayNode SplayTreeFirst(SplayTree tree, void *zeroKey) {
  SplayNode node;
  AVERT(SplayTree, tree);

  if(SplayTreeRoot(tree) == NULL) {
    node = NULL;
  } else if(SplaySplay(&node, tree, zeroKey)) {
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
  b = SplaySplay(&node, tree, oldKey);
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

  AVERT(SplayNode, node);
  /* stream and nodeDescribe checked by SplayTreeDescribe */

  res = WriteF(stream, "( ", NULL);
  if(res != ResOK)
    return res;

  if(SplayNodeLeftChild(node) != NULL) {
    res = SplayNodeDescribe(SplayNodeLeftChild(node), stream, nodeDescribe);
    if(res != ResOK)
      return res;

    res = WriteF(stream, " / ", NULL);
    if(res != ResOK)
      return res;
  }

  res = (*nodeDescribe)(node, stream);
  if(res != ResOK)
    return res;

  if(SplayNodeRightChild(node) != NULL) {
    res = WriteF(stream, " \\ ", NULL);
    if(res != ResOK)
      return res;

    res = SplayNodeDescribe(SplayNodeRightChild(node), stream, nodeDescribe);
    if(res != ResOK)
      return res;
  }

  res = WriteF(stream, " )", NULL);
  if(res != ResOK)
    return res;

  return ResOK;
}


/* SplayTreeDescribe -- Describe a splay tree
 *
 * See design.mps.splay.function.splay.tree.describe.
 */

Res SplayTreeDescribe(SplayTree tree, mps_lib_FILE *stream, 
		      SplayNodeDescribeMethod nodeDescribe) {
  Res res;

  AVERT(SplayTree, tree);
  AVER(stream != NULL);
  AVER(FUNCHECK(nodeDescribe));

  res = WriteF(stream,
	       "Splay $P {\n", (WriteFP)tree,
	       "  compare $F\n", (WriteFF)tree->compare,
	       NULL);
  if(res != ResOK)
    return res;

  if(SplayTreeRoot(tree) != NULL) {
    res = SplayNodeDescribe(SplayTreeRoot(tree), stream, nodeDescribe);
    if(res != ResOK)
      return res;
  }

  res = WriteF(stream, 
	      "\n}\n", 
	      NULL);
  if(res != ResOK)
    return res;

  return ResOK;
}
