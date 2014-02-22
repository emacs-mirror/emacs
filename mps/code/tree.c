/* tree.c: BINARY TREE IMPLEMENTATION
 *
 * $Id$
 * Copyright (C) 2014 Ravenbrook Limited.  See end of file for license.
 */

#include "tree.h"

SRCID(tree, "$Id$");


Bool TreeCheck(Tree tree)
{
  if (tree != TreeEMPTY) {
    CHECKL(tree != NULL);
    CHECKL(tree->left == TreeEMPTY || tree->left != NULL);
    CHECKL(tree->right == TreeEMPTY || tree->right != NULL);
  }
  return TRUE;
}

Bool TreeCheckLeaf(Tree tree)
{
  CHECKL(TreeCheck(tree));
  CHECKL(tree != TreeEMPTY);
  CHECKL(tree->left == TreeEMPTY);
  CHECKL(tree->right == TreeEMPTY);
  return TRUE;
}


/* TreeFind -- search for a node matching the key
 *
 * If a matching node is found, sets *treeReturn to that node and returns
 * CompareEQUAL.  Otherwise returns values useful for inserting a node with
 * the key.  If the tree is empty, returns CompareEQUAL and sets *treeReturn
 * to NULL.  Otherwise, sets *treeReturn to a potential parent for the new
 * node and returns CompareLESS if the new node should be its left child,
 * or CompareGREATER for its right.
 */

Compare TreeFind(Tree *treeReturn, Tree root, TreeKey key, TreeCompare compare)
{
  Tree node, parent;
  Compare cmp = CompareEQUAL;
  
  AVER(TreeCheck(root));
  AVER(treeReturn != NULL);
  AVER(FUNCHECK(compare));
  /* key is arbitrary */

  parent = NULL;
  node = root;
  while (node != TreeEMPTY) {
    parent = node;
    cmp = compare(node, key);
    switch (cmp) {
    case CompareLESS:
      node = node->left;
      break;
    case CompareEQUAL:
      *treeReturn = node;
      return cmp;
    case CompareGREATER:
      node = node->right;
    default:
      NOTREACHED;
      *treeReturn = NULL;
      return cmp;
    }
  }
  
  *treeReturn = parent;
  return cmp;
}


/* TreeInsert -- insert a node into a tree
 *
 * If the key doesn't exist in the tree, inserts a node as a leaf of the
 * tree, returning the resulting tree in *treeReturn, and returns TRUE.
 * Otherwise, *treeReturn points to the existing matching node, the tree
 * is not modified, and returns FALSE.
 */

Bool TreeInsert(Tree *treeReturn, Tree root, Tree node,
                TreeKey key, TreeCompare compare)
{
  Tree parent;
  Compare cmp;
  
  AVER(treeReturn != NULL);
  AVER(TreeCheck(root));
  AVER(TreeCheckLeaf(node));
  AVER(FUNCHECK(compare));
  /* key is arbitrary */

  cmp = TreeFind(&parent, root, key, compare);
  switch (cmp) {
  case CompareLESS:
    parent->left = node;
    break;
  case CompareEQUAL:
    if (parent != NULL) {
      *treeReturn = parent;
      return FALSE;
    }
    AVER(root == TreeEMPTY);
    root = node;
    break;
  case CompareGREATER:
    parent->right = node;
    break;
  default:
    NOTREACHED;
    *treeReturn = NULL;
    return cmp;
  }
  
  *treeReturn = root;
  return TRUE;
}


/* TreeTraverseMorris -- traverse tree in constant space, n log n time
 *
 * <http://en.wikipedia.org/wiki/Tree_traversal#Morris_in-order_traversal_using_threading>
 *
 * The tree may not be modified during the traversal, and the traversal
 * must complete.
 */

void TreeTraverseMorris(Tree tree, TreeVisitor visit,
                        void *closureP, Size closureS)
{
  Tree node;
  
  AVER(TreeCheck(tree));
  AVER(FUNCHECK(visit));
  /* closureP, closureS arbitrary */
  
  node = tree;
  while (node != TreeEMPTY) {
    if (node->left == TreeEMPTY) {
      visit(node, closureP, closureS);
      node = node->right;
    } else {
      Tree pre = node->left;
      for (;;) {
        if (pre->right == TreeEMPTY) {
          pre->right = node;
          node = node->left;
          break;
        }
        if (pre->right == node) {
          pre->right = TreeEMPTY;
          visit(node, closureP, closureS);
          node = node->right;
          break;
        }
        pre = pre->right;
      }
    }
  }
}


/* TreeRotateLeft -- Rotate right child edge of node
 *
 * Rotates node, right child of node, and left child of right
 * child of node, leftwards in the order stated.
 */

void TreeRotateLeft(Tree *treeIO)
{
  Tree tree, right;

  AVER(treeIO != NULL);
  tree = *treeIO;
  AVERT(Tree, tree);
  right = TreeRight(tree);
  AVERT(Tree, right);

  TreeSetRight(tree, TreeLeft(right));
  TreeSetLeft(right, tree);

  *treeIO = right;
}


/* TreeRotateRight -- Rotate left child edge of node
 *
 * Rotates node, left child of node, and right child of left
 * child of node, leftwards in the order stated.
 */

void TreeRotateRight(Tree *treeIO) {
  Tree tree, left;

  AVER(treeIO != NULL);
  tree = *treeIO;
  AVERT(Tree, tree);
  left = TreeLeft(tree);
  AVERT(Tree, left);

  TreeSetLeft(*treeIO, TreeRight(left));
  TreeSetRight(left, *treeIO);

  *treeIO = left;
}


/* TreeReverseLeftSpine -- reverse the pointers on the right spine
 *
 * Descends the left spine of a tree, updating each node's left child
 * to point to its parent instead.  The root's left child is set to
 * TreeEMPTY.  Returns the leftmost child in *leftReturn, or TreeEMPTY
 * if the tree was empty.
 */

Tree TreeReverseLeftSpine(Tree tree)
{
  Tree node, parent;

  AVERT(Tree, tree);
  
  parent = TreeEMPTY;
  node = tree;
  while (node != TreeEMPTY) {
    Tree child = TreeLeft(node);
    TreeSetLeft(node, parent);
    parent = node;
    node = child;
  }
  
  return parent;
}


/* TreeReverseRightSpine -- reverse the pointers on the right spine
 *
 * Descends the right spine of a tree, updating each node's right child
 * to point to its parent instead.  The root's right child is set to
 * TreeEMPTY.  Returns the rightmost child in *rightReturn, or TreeEMPTY
 * if the tree was empty.
 */

Tree TreeReverseRightSpine(Tree tree)
{
  Tree node, parent;

  AVERT(Tree, tree);
  
  parent = TreeEMPTY;
  node = tree;
  while (node != TreeEMPTY) {
    Tree child = TreeRight(node);
    TreeSetRight(node, parent);
    parent = node;
    node = child;
  }
  
  return parent;
}




/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
